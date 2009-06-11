char rcsid[] = "$Header$";

/*

 * Proposed SPEC 95 version of IJG JPEG.
 
 * The trivial provided routines [cd]jpeg
 * required too much I/O to be acceptable to SPEC95;
 * this version remedies that by reading an image
 * into a memory buffer, and processing it repeatedly
 * with different compression settings.

 * An original bitmap image (usually GIF, although potentially any
 * format supported by [cd]jpeg) is both compressed and decompressed
 * at multiple settings.  The difference between the original and
 * decompressed image is evaluated, and simple statistics are taken.

 * Conceptually, this could be part of a naive "search for the optimal
 * compression parameters" program, although no attempt is made to
 * determine an optimal quality/size tradeoff.

 */

#ifndef __HEAPIFY
  #define __HEAPIFY
#endif


#include <stdio.h>
#include <stdlib.h>
#ifdef WIN32
#  include <string.h>
#endif
#ifdef __VMS
#  include <errno.h>
#endif
#include <sys/errno.h>
#include "jpeglib.h"
#include "spec_jpeg.h"


/*
 * Image buffer,
 * original, and result of compress/decompress,
 * TBD defined in spec_jpeg.h
 */


/*
 * Compressed data storage
 * used by the IJG JPEG "destination manager"
 */

#define JPEG_BUFFER_SIZE (16*1024*1024)	/* TBD: reduce to what is actually needed */
#define jpeg_compressed_data_actual_size jpgcmpdatactsiz
int jpeg_compressed_data_max_size = JPEG_BUFFER_SIZE;
int jpeg_compressed_data_actual_size = 0;
JOCTET jpeg_compressed_data[ JPEG_BUFFER_SIZE ]; /* TBD: dynamically size */
int loop_comp_quality;
int loop_comp_smooth;
int loop_comp_opt;


/*
 * subimage coordinates specified in unit floating point,
 * where (0,0) is the upper leftr corner of the original image,
 * and (1,1) is the lower right corner of the original image.
 */
struct coord_s {
    float x, y;
};
struct subimage_s {
    struct coord_s upper_left;
    struct coord_s lower_right;
};
#define FULL_IMAGE  { { 0.0, 0.0}, { 1.0, 1.0 } }

struct compression_parameter_s {
    int quality;			/* jpeg_set_quality(0..100) lo..hi  */
    int optimize_coding;		/* 0/1 - do/do not compute optimum Huffman coding tables */
    int smoothing_factor;		/* 0..100 minimal..maximal */
    struct subimage_s subimage;
};

struct compression_parameter_s compression_parameter = {
    100, 1, 0, FULL_IMAGE
};


struct decompression_parameter_s {
    int quantize_colors;		/* 0/1 do/do not */
    int do_fancy_upsampling;		/* 0/1 do/do not */
    int desired_number_of_colors;	/* number of colors to quantize to */
    int two_pass_quantize;		/* 0/1 take extra pass */
    int dither_mode;			/* JDITHER_NONE, ORDERED, FS */
};

struct decompression_parameter_s decompression_parameter = {
    0, 0, 0,  0, JDITHER_NONE
};



LOCAL
void
compress( struct compression_parameter_s * cp )
{
    int nlines;

    /*
     * IJG JPEG libjpeg context used for the compression pass.
     * Note that this context persists only for the life of this compression pass.
     */
    struct jpeg_compress_struct cinfo __HEAPIFY ;
    struct jpeg_error_mgr jerr __HEAPIFY ;

    /*
     * libjpeg context management
     */
    cinfo.err = jpeg_std_error(&jerr);
    jpeg_create_compress(&cinfo);

    /*
     * Copy information from the persistent images
     * into the current short-lived compression context.
     */
    cinfo.image_width = sliced_original_image.image_width; 
    cinfo.image_height = sliced_original_image.image_height; 
    cinfo.input_components = sliced_original_image.num_color_components; /* # of color components per pixel */
    cinfo.in_color_space = sliced_original_image.color_space; /* colorspace of input image */

    /*
     * libjpeg context management
     */
    jpeg_set_defaults(&cinfo);
    /* Make optional parameter settings here */
    jpeg_set_quality( &cinfo, cp->quality, 1 );
    cinfo.optimize_coding = cp->optimize_coding;
    cinfo.smoothing_factor = cp->smoothing_factor;


    /*
     * Set up "destination manager"
     * to write compressed data into memory
     */
    spec_jpeg_mem_dest( &cinfo,
		       jpeg_compressed_data,
		       jpeg_compressed_data_max_size,
		       &jpeg_compressed_data_actual_size
		       );

    /*
     * Actually do the compression
     */
    jpeg_start_compress(&cinfo, TRUE);

    nlines = jpeg_write_scanlines(&cinfo,
				  sliced_original_image.buffer,
				  sliced_original_image.image_height);
    if ( nlines != sliced_original_image.image_height ) {
	fprintf(stderr,"Error: jpeg_write_scanlines returned %d not %d\n",
		nlines,
		sliced_original_image.image_height
		);
	exit(1);
    }

    jpeg_finish_compress(&cinfo);

    /*
     * libjpeg context management:
     * destroy the context allocated for the compression pass,
     * deallocating all associated data structures.
     */
    jpeg_destroy_compress(&cinfo);
}

LOCAL
void
decompress( struct decompression_parameter_s * dp )
{
    int nlines;

    /*
     * IJG JPEG libjpeg context used for the decompression pass.
     * Note that this context persists only for the life of this decompression pass.
     * Note that there are some other IJG JPEG libjpeg contexts allocated
     * previously, for allocation of the original and decompressed image buffers.
     * TBD: verify that multiple contexts do not interfere.
     */
    struct jpeg_decompress_struct dinfo __HEAPIFY;
    struct jpeg_error_mgr jerr __HEAPIFY;                  


    /*
     * libjpeg context management
     */
    dinfo.err = jpeg_std_error(&jerr);
    jpeg_create_decompress(&dinfo);

    /*
     * Set up "source manager"
     * to read compressed data from memory
     */
    spec_jpeg_mem_src(&dinfo, jpeg_compressed_data, jpeg_compressed_data_actual_size);

    /*
     * Actually do the decompression
     */
    (void) jpeg_read_header(&dinfo, TRUE); /* TBD: ignoring return code */

    /* Optional parameters */
    dinfo.quantize_colors = dp->quantize_colors;
    dinfo.desired_number_of_colors = dp->desired_number_of_colors;
    dinfo.two_pass_quantize = dp->two_pass_quantize;
    dinfo.dither_mode = dp->dither_mode;
    dinfo.do_fancy_upsampling = dp->do_fancy_upsampling;

    /* do it! */
    jpeg_start_decompress(&dinfo);

    /* jpeg_read_scanlines() refuses to process all of the data in one shot */
    while (dinfo.output_scanline < dinfo.output_height) {
	nlines = jpeg_read_scanlines(&dinfo,
				     decompressed_image.buffer+dinfo.output_scanline,
				     dinfo.image_height-dinfo.output_scanline
				     );
	/* TBD: check nlines? */
    }

    (void) jpeg_finish_decompress(&dinfo);

    /*
     * Remember decompressed image parameters
     * with the persistent image data
     */
    decompressed_image.image_height=dinfo.image_height;
    decompressed_image.image_width=dinfo.image_width;

    /*
     * libjpeg context management:
     * destroy the context allocated for the compression pass,
     * deallocating all associated data structures.
     */
    jpeg_destroy_decompress(&dinfo);
}


LOCAL
void
print_parameters(
		 char* image_filename,
		 struct compression_parameter_s* cp,
		 struct decompression_parameter_s* dp
		 )
{
    int uncompressed_data_size =
	sliced_original_image.image_width
	* sliced_original_image.image_height
	* sliced_original_image.num_color_components;

    printf("\n");
    
    printf("image filename: \"%s\"\n", image_filename);
    printf("compression statistics:\n");
    printf("	uncompressed data size:   %d\n", uncompressed_data_size );
    printf("	compressed data size:   %d\n",jpeg_compressed_data_actual_size);
/*  printf("  compressed ratio    :   %.5f\n",
 (float)jpeg_compressed_data_actual_size/(float)uncompressed_data_size); Removed to allow integer comparisons, JWR, 3/21/95 */
    printf("compression parameters:\n");
    printf("	quality	        : %d\n",cp->quality);
    printf("	optimize_coding : %d\n",cp->optimize_coding);
    printf("	smoothing_factor: %d\n",cp->smoothing_factor);
    printf("	subimage    	: ul(%g,%g) lr(%g,%g)\n",
	   cp->subimage.upper_left.x,
	   cp->subimage.upper_left.y,
	   cp->subimage.lower_right.x,
	   cp->subimage.lower_right.y
	   );
    printf("decompression parameters:\n");
    printf("	quantize_colors         : %d\n",dp->quantize_colors          );
    printf("	do_fancy_upsampling     : %d\n",dp->do_fancy_upsampling      );
    printf("	desired_number_of_colors: %d\n",dp->desired_number_of_colors );
    printf("	two_pass_quantize       : %d\n",dp->two_pass_quantize        );
    printf("	dither_mode             : %d\n",dp->dither_mode              );
}

LOCAL
void
go_execute_compression()
{
    if( image_filename == 0 ) {
	fprintf(stderr,"Error: have not defined -image_file\n");
	Usage();
	exit(1);
    }

    spec_free_subimage(&sliced_original_image);
    spec_define_subimage_fp(
			    &original_image,
			    &sliced_original_image,
			    compression_parameter.subimage.upper_left.x,
			    compression_parameter.subimage.upper_left.y,
			    compression_parameter.subimage.lower_right.x,
			    compression_parameter.subimage.lower_right.y
			    );

    compress(&compression_parameter);
    if( verbose_flag ) {
	print_parameters(image_filename,
			 &compression_parameter,
			 &decompression_parameter
			 );
    }
}


LOCAL
void
go_execute_decompression()
{

    decompress(&decompression_parameter);
    spec_difference_images( &sliced_original_image,
			   &decompressed_image,
			   &difference_image,
			   y_stride,
			   x_stride
			   );
    if( checksum_flag ) {
	spec_checksum_image( &decompressed_image,
			    y_stride,
			    x_stride
			    );
    }
}

LOCAL
void
go_execute_compression_and_decompression()
{
    go_execute_compression();
    go_execute_decompression();
    
#ifdef DEBUG
    if( debug_flag ) {
	spec_view_image("new_original_image.ppm","",&original_image);
	spec_view_image("new_sliced_original_image.ppm","",&sliced_original_image);
	spec_view_image("new_decompressed_image.ppm","",&decompressed_image);
	spec_view_image("new_difference_image.ppm","",&difference_image);
    }
#endif /* DEBUG */
}




GLOBAL
void
Usage()
{
    fprintf(stderr,"spec_jpeg - prototype JPEG benchmark for SPEC95\n\
arguments:\n\
    -GO\n\
    	actually perform a JPEG compression and decompression cycle,\n\
    	evaluating results, using parameters specified to the left\n\
    -GO.compress\n\
    -GO.decompress\n\
    	execute compression and decompression independently,\n\
    	e.g. when benchmarking, to balance weightings\n\
\n\
    -image_file	<filename>\n\
    -subimage ulx uly rlx rly\n\
    	4 FP values specifying coordinates of upper left (UL)\n\
    	and lower right (LR) corners of subimage,\n\
    	where (0,0) is UL and (1,1) are UL and LR corners\n\
    	of original image.\n\
\n\
    -compression.quality <0-100>\n\
    	lowest..highest quality\n\
    -compression.optimize_coding <0/1>\n\
    	do not/do compute optimum Huffman coding tables\n\
    -compression.smoothing_factor <0-100>\n\
    	minimal..maximal smoothing\n\
\n\
    -decompression.quantize_colors <0/1>\n\
    -decompression.do_fancy_upsampling <0/1>\n\
    -decompression.desired_number_of_colors <n>\n\
    	number of colours to quantize to\n\
    -decompression.two_pass_quantize <0/1>\n\
    -decompression.dither_mode <mode>\n\
    	NONE, ORDERED, or FS\n\
\n\
    -difference.image <0/1>\n\
    	calculate the difference of the original and decompressed images\n\
    -difference.histogram <0/1>\n\
    	print a histogram of RGB differences\n\
    -difference.L1_norm <0/1>\n\
    	calculate an L1 norm (sum of absolute values of differences)\n\
    -difference.L2_norm <0/1>\n\
    	calculate an L2 norm (sum of squares of differences)\n\
    -difference.row_stride <n>\n\
    -difference.row_stride <n>\n\
    	only look at ever Nth row or column\n\
    	as a quick and dirty check\n\
\n\
    -checksum <0/1>\n\
    	calculate simple XOR  checksum of the image\n\
    	using row and column strides defined above\n\
\n\
    Miscellaneous options:\n\
	-help\n\
	-usage\n\
	    print this message\n\
	-verbose <0/1>\n\
    	-comment <comment-token>\n\
    	-comment.begin ... -comment.end\n\
    	-end\n\
    	    stop command line parsing\n\
    	-debug <0/1>\n\
    	    enables debugging (if program was compiled with it)\n\
\n\
");
}




/*
 * Parse command line (TBD: file) arguments,
 * incrementally modifying parameters and executing commands.
 * Returns index of last argument parsed.
 */
LOCAL
int
parse_args( char** argv, int argstart )
{
    int i = argstart;
    int go;				/* 0 if have not processed data yet since last setting
					 * 1 if have
    

                			         				 */

    #ifdef DANNY
      for(; i < 4; i++) {
    #else
      for(; argv[i]; i++) {
    #endif

	go = 0;
	
	if( 0 ) {
	    /* skip */
	}
	else if( !strcmp( argv[i], "-image_file" ) ) {

	    image_filename = argv[++i];

	    spec_free_image(&original_image);
	    spec_free_image(&decompressed_image);
	    spec_free_image(&difference_image);
		    
	    spec_read_original_image(image_filename); /* TBD: get from arg list */
    
	    spec_allocate_similar_image(&original_image, &decompressed_image);
	    spec_allocate_similar_image(&original_image, &difference_image); 

	}
	/*
	 * Subimage 
	 */
	else if( !strcmp( argv[i], "-subimage" ) ) {
#ifndef __VMS
	    extern int errno;		/* Standard UNIX library error code */
#endif
	    float ulx,uly, lrx, lry;

	    if( argv[i+1] == 0 || argv[i+2] == 0 || argv[i+3] == 0 || argv[i+4] == 0 ) {
		fprintf(stderr, "Expected 4 floating point parameters after -subimage\n");
		Usage();
		exit(1);
	    }
	    errno = 0;
	    ulx = atof( argv[i+1] );	/* TBD: use Dgetnumber() */
	    uly = atof( argv[i+2] );
	    lrx = atof( argv[i+3] );
	    lry = atof( argv[i+4] );
	    i += 4;

	    if( errno != 0 ) {
		fprintf(stderr, "Expected 4 floating point parameters after -subimage\n");
		Usage();
		exit(1);
	    }
	    
	    compression_parameter.subimage.upper_left.x = ulx;
	    compression_parameter.subimage.upper_left.y = uly;
	    compression_parameter.subimage.lower_right.x = lrx;
	    compression_parameter.subimage.lower_right.y = lry;

	}
	/*
	 * Compression parameters
	 */
	else if( !strcmp( argv[i], "-compression.quality" ) ) {
	    compression_parameter.quality = atoi( argv[++i] );
	}
	else if( !strcmp( argv[i], "-compression.optimize_coding" ) ) {
	    compression_parameter.optimize_coding = atoi( argv[++i] );
	}
	else if( !strcmp( argv[i], "-compression.smoothing_factor" ) ) {
	    compression_parameter.smoothing_factor = atoi( argv[++i] );
	}
	else if( !strcmp( argv[i], "-compression.smoothing_factor" ) ) {
	    compression_parameter.smoothing_factor = atoi( argv[++i] );
	}
	/*
	 * Decompression parameters
	 */
	else if( !strcmp( argv[i], "-decompression.quantize_colors" ) ) {
	    decompression_parameter.quantize_colors  = atoi( argv[++i] );
	}
	else if( !strcmp( argv[i], "-decompression.do_fancy_upsampling" ) ) {
	    decompression_parameter.do_fancy_upsampling  = atoi( argv[++i] );
	}
	else if( !strcmp( argv[i], "-decompression.desired_number_of_colors" ) ) {
	    decompression_parameter.desired_number_of_colors  = atoi( argv[++i] );
	}
	else if( !strcmp( argv[i], "-decompression.two_pass_quantize" ) ) {
	    decompression_parameter.two_pass_quantize  = atoi( argv[++i] );
	}
	else if( !strcmp( argv[i], "-decompression.dither_mode" ) ) {
	    ++i;
	    if( !strcmp(argv[i],"NONE") ) {
		decompression_parameter.dither_mode  = JDITHER_NONE;
	    }
	    else if( !strcmp(argv[i],"ORDERED") ) {
		decompression_parameter.dither_mode  = JDITHER_ORDERED;
	    }
	    else if( !strcmp(argv[i],"FS") ) {
		decompression_parameter.dither_mode  = JDITHER_FS;
	    }
	    else {
		fprintf(stderr,"Unrecognized -decompression.dither_mode <%s>\n",argv[i]);
		fprintf(stderr,"Values supported are:\n");
		fprintf(stderr,"    -decompression.dither_mode NONE\n");
		fprintf(stderr,"    -decompression.dither_mode ORDERED\n");
		fprintf(stderr,"    -decompression.dither_mode FS\n");
		exit(1);
	    }
	    
	}
	/*
	 * Reporting Modes
	 */
	else if( !strcmp( argv[i], "-difference.image" ) ) {
	    difference_flag = atoi( argv[++i] );
	}
	else if( !strcmp( argv[i], "-difference.x_stride" ) ) {
	    x_stride = atoi( argv[++i] );
	}
	else if( !strcmp( argv[i], "-difference.y_stride" ) ) {
	    y_stride = atoi( argv[++i] );
	}
	else if( !strcmp( argv[i], "-difference.histogram" ) ) {
	    histogram_flag = atoi( argv[++i] );
	}
	else if( !strcmp( argv[i], "-difference.L1_norm" ) ) {
	    L1_norm_flag = atoi( argv[++i] );
	}
	else if( !strcmp( argv[i], "-difference.L2_norm" ) ) {
	    L2_norm_flag = atoi( argv[++i] );
	}
	/*
	 * Checksum
	 */
	else if( !strcmp( argv[i], "-checksum") ) {
	    checksum_flag = atoi( argv[++i] );
	}
	/*
	 * GO: Actually run something
	 */
	else if( !strcmp( argv[i], "-GO") ) {
	    go = 1;
	    go_execute_compression_and_decompression();
	}
	else if( !strcmp( argv[i], "-GO.compress") ) {
	    go = 1;
	    go_execute_compression();
	}
	else if( !strcmp( argv[i], "-GO.decompress") ) {
	    go = 1;
	    go_execute_decompression();
	}
	else if( !strcmp( argv[i], "-GO.findoptcomp") ) {
	    loop_comp_quality = compression_parameter.quality;
	    loop_comp_smooth = compression_parameter.smoothing_factor;
	    for (compression_parameter.quality = loop_comp_quality; compression_parameter.quality > 10; compression_parameter.quality = compression_parameter.quality - 10 ) {
		for (compression_parameter.smoothing_factor = loop_comp_smooth;compression_parameter.smoothing_factor > 10; compression_parameter.smoothing_factor = compression_parameter.smoothing_factor - 10) {

			compression_parameter.optimize_coding=0;
			go = 1;
			go_execute_compression_and_decompression();
			compression_parameter.optimize_coding=1;
			go = 1;
			go_execute_compression_and_decompression();
		}
	    }
	}
	/*
	 * Standard commands
	 */
	else if( !strcmp( argv[i], "-usage" ) ) {
	    Usage();
	}
	else if( !strcmp( argv[i], "-help" ) ) {
	    Usage();
	}
	else if( !strcmp( argv[i], "-debug" ) ) {
	    debug_flag = atoi( argv[++i] );
	}
	else if( !strcmp( argv[i], "-verbose" ) ) {
	    verbose_flag = atoi( argv[++i] );
	}
	else if( !strcmp( argv[i], "-comment" ) ) {
	    /* skip over next argument */
	    ++i;
	}
	else if( !strcmp( argv[i], "-comment.begin" ) ) {
	    /* skip over multiple arguments */
	    for(;;) {
		++i;
		if( argv[i] == 0  ) {
		    fprintf(stderr,"Error: -comment.begin without a -comment.end\n");
		    Usage();
		    exit(1);
		}
		if( !strcmp( argv[i], "-comment.end" ) ) {
		    break;
		}
	    }
	}
	else if( !strcmp( argv[i], "-end" ) ) {
	    exit(0);
	}
	/*
	 * Unrecognized command
	 */
	else {
	    fprintf(stderr,"Unrecognized argument <%s>\n", argv[i]);
	    Usage();
	    exit(1);
	}
    }

    if( go == 0 ) {
	fprintf(stderr,"Warning: no -GO argument\n");
	go_execute_compression_and_decompression();
    }
    
    return i;
}



main(int argc, char **argv)
{

    verbose_flag = 0;
    histogram_flag = 0;
    image_filename = 0;
    L1_norm_flag = 1;
    L2_norm_flag = 1;
    difference_flag = 1;
    checksum_flag = 1;
    x_stride = 1;
    y_stride = 1;

    (void)parse_args(argv, 1);

    exit(0);
}



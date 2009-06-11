/* $Header$ */

/*
 * spec_image.c
 *
 * routines to manipulate a variety of image formats
 * into a canonical in-memory buffer format.
 *
 * (used by spec_jpeg)
 */

#include <math.h>
#include <signal.h>
#include <stdio.h>
#ifdef WIN32
#	include <stdlib.h>
#endif

#include "spec_jpeg.h"
#include "ppm.h"

/* Define some conversion functions between the two different 
 * representations used for the image in the PPM and the JPEG libraries */
/* This works with EFAT as well, but FAT is faster !!!!! */
pixel * FAT *  jpeg_to_ppm_image(JSAMPLE * FAT * buffer) {
  /* Because we use trusted_cast CCured will not see the connection for the 
   * inner pointers. They have to have the same kind, so for now we just 
   * manually specify their types. */
  return CCURED_TRUSTEDCAST(pixel **, buffer);
}

JSAMPLE * FAT * ppm_to_jpeg_image(pixel * FAT * buffer) {
  return CCURED_TRUSTEDCAST(JSAMPLE **, buffer);
}


#ifdef DEBUG
/*
 * View an image
 * - mainly for debugging purposes
 */
GLOBAL
void
spec_view_image(char* filename, char* comment, spec_image* image)
{
/* Code commented for portability to WIN.NT - JWR, 3/15/95
    int child;
    
    fprintf(stderr,"Viewing image");
    fprintf(stderr,"	filename <%s>\n",filename);
    fprintf(stderr,"	comment\n",comment);
    fprintf(stderr," 	%d x %d\n", image->image_width, image->image_height);
    fprintf(stderr," 	num color %d\n", image->num_color_components);

    spec_write_image(filename, image);
#ifndef WIN32

    if( (child = fork()) == 0 ) {
	execl( "/usr/local/bin/X11R5/xv", "xv", filename, 0);
	fprintf(stderr,"error execing xv\n");
	exit(1);
    }
    getchar();

#if 0
    if( kill(child,SIGKILL) ) {
	fprintf(stderr,"error killing xv\n");
    }
#endif
#endif
   End of commented out code - JWR, SPEC, 3/15/95 */
}
#endif /* DEBUG */

/*
 * Read the original image.
 *
 * TBD: really should make the original image into a parameter or return value.
 */

GLOBAL
void 
spec_read_original_image(char *filename)
{
    FILE *infile;
    pixval  max_color_component;

    
    if( !strcmp(filename,"stdin") ) {
	/* kluge for systems that lack /dev/stdin, /dev/stdout, /dev/fd<n> */
	infile = stdin;
    } else {
	infile = fopen(filename,"r");
	if( infile == 0 ) {
	    fprintf(stderr,"error opening input file <%s>\n", filename);
	    exit(1);
	}
    }

    /* TBD: should malloc original image */

    original_image.buffer =
	ppm_to_jpeg_image(ppm_readppm(
                                      infile,
                                      (int *)&original_image.image_width,
                                      (int *)&original_image.image_height,
                                      &max_color_component
                                      ));
    if( original_image.buffer == 0 ) {
	fprintf(stderr,"error allocating decompressed_image.buffer\n");
	exit(1);
    }
    original_image.max_color_component = max_color_component; /* type conversion */

    original_image.num_color_components = 3;

    /* TBD: is it portable to cast a libppm pixel**
     * into a libjpeg JSAMPARRAY?
     */

    /*
     * Parameters ppm_readppm() doesn't know about
     */
    original_image.color_space = JCS_RGB;
				 
}

GLOBAL
void
spec_allocate_similar_image( spec_image* pattern, spec_image* new )
{
    /* TBD: parametrize and return, or the like */
    
    new->image_height =	    pattern->image_height;
    new->image_width =	    pattern->image_width;
    new->max_color_component =  pattern->max_color_component;
    new->num_color_components =  pattern->num_color_components;
    new->color_space =	    pattern->color_space;

    new->buffer =
	ppm_to_jpeg_image(ppm_allocarray(
                                         pattern->image_width,
                                         pattern->image_height
                                         ));
    if( new->buffer == 0 ) {
	fprintf(stderr,"error allocating new->buffer\n");
	exit(1);
    }

    /* TBD: is it portable to cast a libppm pixel**
     * into a libjpeg JSAMPARRAY?
     */

}


/*
 * Free an image's buffer storage;
 * assumes that the image struct itself is static,
 * and doesn't free it.
 */
GLOBAL
void
spec_free_image( spec_image* image )
{
    if( image-> buffer ) {
	ppm_freearray(jpeg_to_ppm_image(image->buffer), image->image_height);
    }
    image->buffer = 0;
    image->image_height = -1;
    image->image_width = -1;
    image->max_color_component = -1;
    image->num_color_components = -1;
    image->color_space = -1;
}

/*
 * Define pointers to a subimage.
 * Note: knows the internal structure of spec_image, JSAMPARRAY, etc.
 */
GLOBAL
void
spec_define_subimage_fp(
		 spec_image* image,
		 spec_image* subimage,
		 float ulx, float uly,
		 float lrx, float lry
		 )
{
    int in_range= ( 1
		   && 0.0 <= ulx && ulx <= 1.0
		   && 0.0 <= lrx && lrx <= 1.0
		   && ulx < lrx
		   && 0.0 <= uly && uly <= 1.0
		   && 0.0 <= lry && lry <= 1.0
		   && uly < lry
		   );
    if ( !in_range ) {
	fprintf(stderr,"%s:%d: badly defined fp subimage\n", __FILE__, __LINE__);
	fprintf(stderr,"    ul:(%g,%g) lr:(%g,%g)\n",ulx,uly, lrx,lry);
	exit(1);
    }

    spec_define_subimage_int( image, subimage,
			 (int) (ulx * (image->image_width-1)),
			 (int) (uly * (image->image_height-1)),
			 (int) (lrx * (image->image_width-1)),
			 (int) (lry * (image->image_height-1))
			 );
}

GLOBAL
void
spec_define_subimage_int(
		 spec_image* image,
		 spec_image* subimage,
		 int ulx, int uly,
		 int lrx, int lry
		 )
{
    int j, jj;
    int in_range = ( 1
		    && 0 <= ulx && ulx < image->image_width
		    && 0 <= lrx && lrx < image->image_width
		    && ulx < lrx
		    && 0 <= uly && uly < image->image_height
		    && 0 <= lry && lry < image->image_height
		    && uly < lry
		    );
    
    if ( !in_range ) {
	fprintf(stderr,"%s:%d: badly defined int subimage\n", __FILE__, __LINE__);
	fprintf(stderr,"    ul:(%d,%d) lr:(%d,%d) image:(%d,%d)\n",
		ulx,uly, lrx,lry,
		image->image_width, image->image_height
		);
	exit(1);
    }

    subimage->image_height =	    lry-uly+1;
    subimage->image_width =	    lrx-ulx+1;
    subimage->max_color_component =  image->max_color_component;
    subimage->num_color_components =  image->num_color_components;
    subimage->color_space =	    image->color_space;

    subimage->buffer =
	// sm: (JSAMPARRAY) malloc( subimage->image_height * sizeof(JSAMPROW) );
	(JSAMPARRAY) malloc( subimage->image_height * sizeof(*(subimage->buffer)) );

    if( subimage->buffer == 0 ) {
	fprintf(stderr,"error allocating subimage->buffer\n");
	exit(1);
    }

    for(j=uly, jj=0;
	j<=lry;
	j++, jj++
	)
    {
	subimage->buffer[jj] = image->buffer[j] + image->num_color_components*ulx;
    }
}

GLOBAL
void
spec_free_subimage( spec_image* subimage )
{
    subimage->image_height =	    0;
    subimage->image_width =	    0;
    subimage->max_color_component =  0;
    subimage->num_color_components =  0;
    subimage->color_space =	    0;

    /*
     * Free the first level of the buffer,
     * i.e. pointers to rows,
     * but do not free the actual buffer itself,
     * which is shared with the original unsliced image.
     */
    if( subimage->buffer ) {
	free(subimage->buffer);
    }
    subimage->buffer = 0;
}


GLOBAL
void
spec_write_image( char *filename, spec_image* image )
{
    FILE* outfile;
    
    if( !strcmp(filename,"stdout") ) {
	/* kluge for systems that lack /dev/stdin, /dev/stdout, /dev/fd<n> */
	outfile = stdout;
    } else {
	outfile = fopen(filename,"w+");
	if( outfile == 0 ) {
	    fprintf(stderr,"error opening output file <%s>\n", filename);
	    exit(1);
	}
    }

    ppm_writeppm( outfile,
		 jpeg_to_ppm_image(image->buffer),
		 image->image_width,
		 image->image_height,
		 image->max_color_component,
		 1			/* force plain format output */
		 );

    if ( fclose(outfile) ) {
	fprintf(stderr,"error closing output file <%s>\n",filename);
	exit(0);
    }
    
}


/*
 * Histogram
 */
#define HISTO_MIN -128
#define HISTO_MAX 128
int histogram[ HISTO_MAX - HISTO_MIN+1 ];
int histo_underflow = 0;
int histo_overflow = 0;



LOCAL
zero_histogram()
{
    int i;

    histo_underflow = 0;
    histo_overflow = 0;
    for(i=HISTO_MIN;i<=HISTO_MAX;i++) {
	histogram[ i - HISTO_MIN ] = 0;
    }
}

LOCAL
print_histogram()
{
    int i;

    printf("underflow	%d\n",histo_underflow);
    printf("overflow	%d\n",histo_overflow);
    for(i=HISTO_MIN;i<=HISTO_MAX;i++) {
	printf("%d  - %d\n", i, histogram[ i - HISTO_MIN ] );
    }
    printf("\n");
}



/*
 * Difference images,
 * calculating
 * (0) difference image
 * (1) histogram of differences
 * (2) L1 norm
 * (3) L2 norm
 *  	both norms subject to odd saturation.
 * To avoid spending too much time here, the user
 * can specify both a row and column stride so that not all pixels
 * need be sampled. A really aggressive compiler might be able to
 * calculate only those pixels desired.
 */

/* Machine dependent saturation constants for calculating L1 and L2 norms
 * (values chosen to be acceptable for a machine with 32 bit integers,
 * so therefore probably acceptable to SPEC)
 */
#define DIF_THRESHOLD	64		
#define SUM_THRESHOLD	(1024*1024*1024)       

GLOBAL
void
spec_difference_images( spec_image* a,
		       spec_image*  b,
		       spec_image*  d,
		       int row_stride,
		       int col_stride
		       )
{
    int row, col, channel;
    int dif;
    int bias = a->max_color_component >> 1;
    unsigned L1_norm = 0;
    unsigned L2_norm = 0;

    /*
     * Ensure that arguments to be differenced are compatible
     */
    if( a->image_width != b->image_width
       || a->image_height != b->image_height
       || a->num_color_components != b->num_color_components
       || a->max_color_component != b->max_color_component
       || a->color_space != b->color_space
       || a->gamma != b->gamma
       ) {
	fprintf(stderr,"error: mismatched images differencing\n");
	exit(1);
    }

    /*
     * Copy image parameters,
     * blindly overwriting parameters originally defined.
     * ASSUMPTION: buffer size already allocated is acceptable.
     */
    d->image_width	=   a->image_width;
    d->image_height	=   a->image_height;
    d->num_color_components	=   a->num_color_components;
    d->max_color_component	=   a->max_color_component;
    d->color_space	=   a->color_space;
    d->gamma	=   a->gamma;

    zero_histogram();

    for( row=0; row < d->image_height; row += row_stride ) {
	for( col=0; col < d->image_width; col += col_stride ) {
	    for( channel=0; channel < d->num_color_components; channel++ ) {

		/*
		 * Calculate difference,
		 */
		dif = a->buffer[row][col*d->num_color_components+channel] - b->buffer[row][col*d->num_color_components+channel];

		if( histogram_flag ) {
		    /*
		     * Histogram difference
		     */
		    if( dif < HISTO_MIN ) {
			histo_underflow ++;
		    } else if( dif > HISTO_MAX ) {
			histo_overflow ++;
		    } else {
			histogram[ dif - HISTO_MIN ]++;
		    }
		}

		if( difference_flag ) {
		    /* Record difference
		     * biasing to center in range of original image color space,
		     * without worrying too much about rounding
		     * (since we only intend to display the difference as a check).
		     *
		     * Note: in a real application we would more likely want to like at difference
		     * image in YUV space instead of RGB space.
		     */
		    /* TBD: make the recording of the difference optional? */
		    if( dif + bias < 0 ) {
			d->buffer[row][col*d->num_color_components+channel] = 0;
		    } else if( dif + bias > d->max_color_component ) {
			d->buffer[row][col*d->num_color_components+channel] = d->max_color_component-1;
		    } else {
			d->buffer[row][col*d->num_color_components+channel] = dif + bias;
		    }
		}

		if( L1_norm_flag ) {
		    /*
		     * L1 norm
		     */
		    /* TBD: a good compiler should be able to do this with a table lookup
		     * because of the range restriction above */
		    dif = abs(dif);
		    if( dif > DIF_THRESHOLD ) dif = DIF_THRESHOLD;

		    if( L1_norm + dif > SUM_THRESHOLD ) {
			L1_norm = SUM_THRESHOLD;
		    } else {
			L1_norm += dif;
		    }
		}

		if( L2_norm_flag ) {
		    /*
		     * L2 norm
		     */
		    /* TBD: a good compiler should be able to do this with a table lookup
		     * because of the range restriction above */
		    dif = dif * dif;

		    if( L2_norm + dif > SUM_THRESHOLD ) {
			L2_norm = SUM_THRESHOLD;
		    } else {
			L2_norm += dif;
		    }
		}
	    }
	}
    }

    if( L1_norm_flag ) {
	printf("L1 norm = %d\n", L1_norm);
    }
    if( L2_norm_flag ) {
	printf("L2 norm = %d\n", L2_norm);
    }
    if( histogram_flag ) {
	print_histogram();
    }

}



/*
 * Checksum image
 *
 * This function is provided mainly for purposes of benchmarking,
 * to prevent a *really*, *really*, good compiler from eliminating
 * a whole slew of calculations, e.g. by evaluating only those pixels
 * that are necessary to evaluate the sampled histogram and difference
 * arrays above.
 *
 * I had thought to create a data dependent checksum, that sampled
 * only a few representative pixels. Unfortunately, it is theoretically possible
 * for a compiler using a compute on deman style to compute only those pixels
 * that are sampled.
 *
 * I therefore revert to using the simplest possible checksum that evaluates
 * the entire array - a simple bytewise XOR - with optional strides.
 * I note that any good compiler
 * should be able to do this 32 or 64 bits at a time,
 * if the column stride is 1 - that is left as a grace note.
 */

GLOBAL
void
spec_checksum_image( spec_image* a,
		       int row_stride,
		       int col_stride
		       )
{
    int row, col, channel;
    int checksum = 0;

    for( row=0; row < a->image_height; row += row_stride ) {
	for( col=0; col < a->image_width; col += col_stride ) {
	    for( channel=0; channel < a->num_color_components; channel++ ) {
		checksum ^= a->buffer[row][col*a->num_color_components+channel];
	    }
	}
    }

    printf("checksum: %d\n",checksum);
}


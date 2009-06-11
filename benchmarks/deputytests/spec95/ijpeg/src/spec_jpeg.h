#ifndef SPEC_JPEG_H
#define SPEC_JPEG_H
/* $Header$ */

/*
 * spec_jpeg.h
 *
 * header for the proposed SPEC95 JPEG application.
 */

#ifndef JPEG_LIB_VERSION
#include "jpeglib.h"
#endif

#define DEBUG 1

struct spec_image_s {
    
    /*
     * Description of image
     */

  JDIMENSION image_width;		/*  image width */
  JDIMENSION image_height;		/*  image height */
  int num_color_components;		/* # of color components in  image */
  int max_color_component;		/* maximum colour component value
					 * (used by libppm)
					 */
  J_COLOR_SPACE color_space;		/* colorspace of  image */

  double gamma;		/* image gamma of  image */

  /* The actual image data is stored as a 3D array,
   * height X width X color components
   * with each dimension containing a vector of pointers
   * to sub-arrays (to avoid index calculations, as defined
   * by the original [cd]jpeg applications).
   *
   * This is the IJG JPEG libjpeg JSAMPARRAY data type,
   * manipulated by routines such as alloc_sarray.
   *
   * GN: CCured says this is not equivalent to pixmap **.
   
   * This is assumed to be equivalent to the libppm data type pixmap**.
   * (TBD: may need to verify portability of this, or add conversion functions.)
   */

  JSAMPARRAY buffer;
};

typedef struct spec_image_s spec_image;


/*
 * Prototypes
 */

GLOBAL
void
spec_read_original_image(char *filename);

GLOBAL
void
spec_allocate_similar_image( spec_image* pattern, spec_image* new );

GLOBAL
void
spec_define_subimage_fp(
			spec_image* image,
			spec_image* subimage,
			float ulx, float uly,
			float lrx, float lry
			);

GLOBAL
void
spec_define_subimage_int(
			 spec_image* image,
			 spec_image* subimage,
			 int ulx, int uly,
			 int lrx, int lry
			 );

GLOBAL
void
spec_free_subimage( spec_image* subimage );

GLOBAL
void
spec_write_image( char *filename, spec_image* image );

GLOBAL
void
spec_difference_images( spec_image* a,
		       spec_image*  b,
		       spec_image*  d,
		       int row_stride,
		       int col_stride
		       );

GLOBAL
void
spec_checksum_image( spec_image* a,
		       int row_stride,
		       int col_stride
		       );

GLOBAL
void
Usage();





/*
 * Declarations of the actual images
 * TBD: move to a header file separate from the type declarations.
 */
spec_image original_image;
spec_image decompressed_image;
spec_image difference_image;
spec_image sliced_original_image;	/* special considerations: this could have been malloc'ed
					 * in every pass, but we don't want to  benchmark malloc.
					 * we therefore allocate it once, and then modify it in place
					 */


int debug_flag;
int verbose_flag;
int histogram_flag;
char* image_filename;
int L1_norm_flag;
int L2_norm_flag;
int difference_flag;
int checksum_flag;
int x_stride;
int y_stride;


/*
 * Declarations of crud that seems to be required
 * to keep the JSAMPARRAYs allocated by alloc_sarry() around
 */
struct jpeg_compress_struct TBD_image_cinfo;
struct jpeg_error_mgr TBD_image_jerr;

#endif 

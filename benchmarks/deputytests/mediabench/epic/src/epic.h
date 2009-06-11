/*
--------------------------------------------------------------------- 
---		 EPIC (Efficient Pyramid Image Coder)             ---
---	 Designed by Eero P. Simoncelli and Edward H. Adelson     ---
---		    Written by Eero P. Simoncelli                 ---
---  Developed at the Vision Science Group, The Media Laboratory  ---
---	Copyright 1989, Massachusetts Institute of Technology     ---
---			 All rights reserved.                     ---
---------------------------------------------------------------------

Permission to use, copy, or modify this software and its documentation
for educational and research purposes only and without fee is hereby
granted, provided that this copyright notice appear on all copies and
supporting documentation.  For any other uses of this software, in
original or modified form, including but not limited to distribution
in whole or in part, specific prior permission must be obtained from
M.I.T. and the authors.  These programs shall not be used, rewritten,
or adapted as the basis of a commercial software or hardware product
without first obtaining appropriate licenses from M.I.T.  M.I.T. makes
no representations about the suitability of this software for any
purpose.  It is provided "as is" without express or implied warranty.

---------------------------------------------------------------------
*/

#define EPIC_VERSION 1.1

#ifndef DEPUTY
  #define NTS
  #define NT
  #define NONNULL
  #define COUNT(x)
  #define DALLOC(x)
  #define SAFE
  #define EFAT
  #define POLY
  #define TC(c) (c)
#endif

/* ============= FUNDAMENTAL LIMITATIONS ============= */

/* Maximum x- or y-size of image */
#define MAX_IMAGE_DIM 65535  

/* Maximum number of pyramid levels (value 3*levs+1 stored in 5 bits).
   This doesn't need to be larger than log2(MAX_IMAGE_DIM/FILTER_SIZE). */
#define MAX_LEVELS 10

/* Maximum number of quantization bins.  This essentially determines
   the maximum depth image to be represented. */ 
#define MAX_BINS 511

/* ============= SECONDARY (derived) LIMITATIONS ============= */


/* This number determines the precision of the stored binsizes:
   stored coefficients are accurate to +/- (1/SCALE_FACTOR).
   On the other hand, this number also will limit the maximum amount 
   of compression.  It should not be more than [2^(8*sizeof(BinValueType))]/256. */
#define SCALE_FACTOR 128

/* This number must be consistent with the filters that are 
   hardwired into epic.c */
#define FILTER_SIZE 15

/* Log (base 2) of MAX_IMAGE_DIM^2: (bits required to store the dimensions) */
#define LOG_MAX_IMAGE_SIZE 32

/* The type of the quantized images. Must be SIGNED, and capable of holding 
   values  in the range [-MAX_BINS, MAX_BINS] */
typedef short BinIndexType;  

/* The type used to represent the binsizes. Should be UNSIGNED. If this is
   changed, be sure to change the places in epic.c and unepic.c where 
   binsizes are written or read from files.  */
typedef unsigned short BinValueType;

/* Number of possible values for a symbol.  This must be at least
   (MAX_BINS * 4)  (one sign bit, one tag bit)... */
#define NUM_SYMBOL_VALUES 65536

/* The symbols encoded by the Huffman coder.  Should be unsigned, 
   and capable of holding NUM_SYMBOL_VALUES.
   If this is changed, change the places in huffman.c, and unepic.c 
   where symbols are written or read from files. */
typedef unsigned short SymbolType;

typedef unsigned char Byte;

/* the data type of the encoded stream */
typedef Byte CodeType;

/* ============= FILE BYTE TAGS =============== */
#define EPIC_ID_TAG   '\377'   /* eight bits on */
#define BIN_INFO_TAG  '\200'    /* high bit is 1 */

/* coded data block tags: high bit must be 0! */
#define RAW_DATA_TAG        '\000'
#define HUFFMAN_DATA_TAG    '\001'

/* ============== FILE IO MACROS ============== */
/*  We write everything as bytes for portability. For the            */
/*  files to be machine independent, we write the bytes of           */
/*  longer quantities (like shorts and ints) in a fixed order.       */
/*  WE ASSUME THAT:                                                  */
/*     - short quantities are at least 2 bytes.                      */
/*     - int quantitites are at least 4 bytes.                       */
/*  **** The arrays are written incorrectly and need to be fixed!    */
/*  Temporary variables are defined in utilities.c                   */

/* If non-zero, unepic will write a PGM file.  Otherwise, writes a   */
/* raw byte file.  */
#define WRITE_PGM_FILE 1

#define write_byte(val, stream) temp_byte = (Byte) val; \
  fwrite(&temp_byte,1,1,stream)
#define write_short(val,stream) temp_short = (unsigned short) val; \
  temp_byte = (temp_short & 0xFF00)>>8; fwrite(&temp_byte,1,1,stream); \
  temp_byte = (temp_short & 0x00FF); fwrite(&temp_byte,1,1,stream)
#define write_int(val,stream) temp_int = (unsigned int) val; \
  temp_byte = (temp_int & 0xFF000000)>>24; fwrite(&temp_byte,1,1,stream); \
  temp_byte = (temp_int & 0x00FF0000)>>16; fwrite(&temp_byte,1,1,stream); \
  temp_byte = (temp_int & 0x0000FF00)>>8;  fwrite(&temp_byte,1,1,stream); \
  temp_byte = (temp_int & 0x000000FF);     fwrite(&temp_byte,1,1,stream)
#define write_array(ptr, size, stream) fwrite( ptr, 1, sizeof(*ptr)*size, stream)

#define read_byte(sym, stream) fread(&temp_byte,1,1,stream); sym=temp_byte
#define read_short(sym, stream) \
  fread(&temp_byte,1,1,stream); temp_short = temp_byte; temp_short <<=8; \
  fread(&temp_byte,1,1,stream); temp_short |= temp_byte; sym=temp_short
#define read_int(sym, stream) \
  fread(&temp_byte,1,1,stream); temp_int  = temp_byte; temp_int <<= 8; \
  fread(&temp_byte,1,1,stream); temp_int |= temp_byte; temp_int <<= 8; \
  fread(&temp_byte,1,1,stream); temp_int |= temp_byte; temp_int <<= 8; \
  fread(&temp_byte,1,1,stream); temp_int |= temp_byte; sym = temp_int;
#define read_array(ptr, size, stream)  fread(ptr, 1, sizeof(*ptr) * size, stream)

extern Byte temp_byte;
extern short temp_short;
extern int temp_int;

extern float *ReadMatrixFromPGMStream();

/* ============= FUNCTION DECLARATIONS ============= */
/*    These functions are defined in utilities.c     */
#include <stdio.h>


/* ============= STRUCTURES ============== */

struct code_node   /* A node of a huffman tree used for decoding  */
  {
  struct code_node *zero_child, *one_child;
  SymbolType symbol;
  };

extern struct code_node *read_huffman_tree();

// in parse_args.c
// The image is returned with its size, because we cannot say that
// the return value is *x_size * *y_size
extern void parse_epic_args (int argc, char * NTS * COUNT(argc) NONNULL argv,
                            float * EFAT * SAFE image,
                            int * x_size, int * y_size, 
                            int * num_levels, double * compression_factor,
                            FILE * SAFE * outfile);
extern void parse_unepic_args(int argc, char * NTS * COUNT(argc) NONNULL argv,
                             FILE * SAFE * epicfile,
                             int * num_levels, int * x_size, int * y_size, 
                             double * scale_factor,
                             FILE * SAFE * outfile);

// in build_pyr.c
int build_pyr(float * COUNT(x_size*y_size) NONNULL image,
              int x_size, int y_size,
              int num_levels,
              float * COUNT(filter_size) NONNULL lo_filter,
              float * COUNT(filter_size) NONNULL hi_filter,
              int filter_size);

// collapse_pyr.c
void collapse_pyr(int * COUNT(x_size*y_size) NONNULL pyr,
                 int * COUNT(x_size*y_size) NONNULL result,
                 int x_size, int y_size, int num_levels);

// In huffman.c
int huffman_encode (SymbolType * COUNT(symbol_stream_length) NONNULL symbol_stream,
                    int symbol_stream_length, 
		    Byte * EFAT * packed_huffman_tree,
                    int * huffman_tree_size,
                    CodeType * COUNT(symbol_stream_length) NONNULL encoded_stream);
void read_and_huffman_decode(FILE * file, struct code_node * tree,
                             SymbolType * COUNT(count) NONNULL symbol_stream,
                             int count);

struct code_node *read_huffman_tree(FILE *file);


// run_length.c
int run_length_encode_zeros (BinIndexType * COUNT(block_size) NONNULL block,
                             int block_size,
                             SymbolType * COUNT(block_size) NONNULL encoded_stream);
void run_length_decode_zeros(SymbolType * EFAT NONNULL symbol_stream,
                             int block_size,
                             BinIndexType * COUNT(block_size) NONNULL block);

  
// utilities.c
void * NONNULL (DALLOC(size) check_malloc) (int size);
void check_free (void * NONNULL ptr);

FILE * NONNULL check_fopen (char * NTS NONNULL ptr,
                            char * NTS NONNULL read_write_flag);

char * NTS NONNULL concatenate(char * NTS NONNULL string1,
                               char * NTS NONNULL string2);


// fileio.c
int PGMStream(FILE * infile);
float * EFAT ReadMatrixFromPGMStream(FILE * Infile,
                                     int * xsize, int * ysize);

void read_byte_image(FILE * stream,
                    float * COUNT(x_size*y_size) NONNULL image,
                    int x_size, int y_size);
void write_byte_image(FILE * stream, int * COUNT(x_size*y_size) NONNULL image,
                     int x_size, int y_size);
void write_pgm_image(FILE * stream, int * COUNT(x_size*y_size) NONNULL image,
                    int x_size, int y_size);


// quantize.c
void quantize_pyr (float * COUNT(im_size) NONNULL float_pyr,
                  BinIndexType * COUNT(im_size) NONNULL q_pyr,
                  int im_size, int num_levels,
                  double compression_factor,
                  BinValueType * COUNT(3*num_levels + 1) NONNULL bin_size);


void unquantize_pyr(BinIndexType * COUNT(im_size) NONNULL q_pyr,
                   int * COUNT(im_size) NONNULL pyr, 
                   int im_size, int num_levels,
                   BinValueType * COUNT(3*num_levels + 1) NONNULL bin_size);

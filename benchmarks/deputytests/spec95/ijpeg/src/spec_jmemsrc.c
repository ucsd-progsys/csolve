/* $Header$ */

/*
 * spec_jmemsrc.c
 * 
 * This file contains decompression source routines for the case
 * of extracting JPEG data from a memory buffer.
 *
 * The prefix "jms_" is used, standing for "jpeg memory source".
 */

/* this is not a core library module, so it doesn't define JPEG_INTERNALS */
#include "jinclude.h"
#include "jpeglib.h"
#include "jerror.h"


/* Expanded data source object for stdio input */

typedef struct jms_my_source_mgr {
  struct jpeg_source_mgr pub;	/* public fields */
  /* Currently have no private data */
} jms_my_source_mgr;

typedef jms_my_source_mgr * jms_my_src_ptr;

#pragma ccured_extends("Sjms_my_source_mgr", "Sjpeg_source_mgr")

/*
 * Initialize source --- called by jpeg_read_header
 * before any data is actually read.
 *
 * Is a NO-OP for an in-memory buffer
 * (NB. this means that we must "reopen" the buffer by calling
 * spec_jpeg_mem_src() every time we need to reprocess.)
 */

METHODDEF void
jms_init_source (j_decompress_ptr dinfo)
{
    /* nothing needed */
}


/*
 * Fill the input buffer --- called whenever buffer is emptied.
 *
 * This is an error for the memory source (jms_) routines.
 *
 * TBD: use standard libjpeg error handling?
 */

METHODDEF boolean
jms_fill_input_buffer (j_decompress_ptr dinfo)
{
    fprintf(stderr,"Error: jms_fill_input_buffer_called\n");
    exit(1);
    return 1;
}


/*
 * Skip data --- used to skip over a potentially large amount of
 * uninteresting data (such as an APPn marker).
 *
 * Simply moves pointers for the in-memory routines.
 */

METHODDEF void
jms_skip_input_data (j_decompress_ptr dinfo, long num_bytes)
{
  jms_my_src_ptr src = (jms_my_src_ptr) dinfo->src;

  src->pub.next_input_byte += (size_t) num_bytes;
  src->pub.bytes_in_buffer -= (size_t) num_bytes;

  if( src->pub.bytes_in_buffer < 0 ) {
      fprintf(stderr,"jms_skip_input_data skips past end\n");
      exit(1);
  }
}


/*
 * An additional method that can be provided by data source modules is the
 * resync_to_restart method for error recovery in the presence of RST markers.
 * For the moment, this source module just uses the default resync method
 * provided by the JPEG library.  That method assumes that no backtracking
 * is possible.
 */


/*
 * Terminate source --- called by jpeg_finish_decompress
 * after all data has been read.
 *
 * Is a NO-OP for memory input.
 *
 * NB: *not* called by jpeg_abort or jpeg_destroy; surrounding
 * application must deal with any cleanup that should happen even
 * for error exit.
 */

METHODDEF void
jms_term_source (j_decompress_ptr dinfo)
{
  /* no work necessary here */
}


/*
 * Prepare for input from a memory buffer.
 */

GLOBAL void
spec_jpeg_mem_src (j_decompress_ptr dinfo, JOCTET* jms_buffer, int jms_bufsz)
{
  jms_my_src_ptr src;

  /* The source object and input buffer are made permanent so that a series
   * of JPEG images can be read from the same file by calling jpeg_stdio_src
   * only before the first one.  (If we discarded the buffer at the end of
   * one image, we'd likely lose the start of the next one.)
   * This makes it unsafe to use this manager and a different source
   * manager serially with the same JPEG object.  Caveat programmer.
   */
  if (dinfo->src == NULL) {	/* first time for this JPEG object? */
    dinfo->src =
      (struct jms_my_source_mgr *)
      alloc_small_wrapper ((j_common_ptr) dinfo, JPOOL_PERMANENT,
                           SIZEOF(jms_my_source_mgr));
  }

  src = (jms_my_src_ptr) dinfo->src;

  /*
   * Methods
   */
  src->pub.init_source = jms_init_source;
  src->pub.fill_input_buffer = jms_fill_input_buffer;
  src->pub.skip_input_data = jms_skip_input_data;
  src->pub.resync_to_restart = jpeg_resync_to_restart; /* use default method */
  src->pub.term_source = jms_term_source;
  
  /*
   * Data structures
   */
  src->pub.bytes_in_buffer = jms_bufsz; 
  src->pub.next_input_byte = jms_buffer;
}


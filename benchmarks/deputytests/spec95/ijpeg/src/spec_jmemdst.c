/* $Header$ */

/*
 * spec_jmemdst.c
 *
 * This file contains compression data destination routines for the case of
 * emitting JPEG data to a memory buffer.
 *
 * The prefix "jmd_" is used, standing for "jpeg memory destination".
 */

/* this is not a core library module, so it doesn't define JPEG_INTERNALS */
#include "jinclude.h"
#include "jpeglib.h"
#include "jerror.h"


/* Expanded data destination object for stdio output */

typedef struct jmd_my_destination_mgr {
  struct jpeg_destination_mgr pub;	/* public fields */
  JOCTET* jmd_buffer;			/* pointer to memory buffer */
  int 	  jmd_max_bufsz;		/* maximum size of memory buffer */
  int	  *jmd_actual_bufszptr;		/* pointer to where we record the actual size on completion */
} jmd_my_destination_mgr;

typedef jmd_my_destination_mgr* jmd_my_dest_ptr;
#pragma ccured_extends("Sjmd_my_destination_mgr", "Sjpeg_destination_mgr")
/*
 * Initialize destination --- called by jpeg_start_compress
 * before any data is actually written.
 */

METHODDEF void
jmd_init_destination (j_compress_ptr cinfo)
{
  jmd_my_dest_ptr dest = (jmd_my_dest_ptr) cinfo->dest;

  dest->pub.next_output_byte = dest->jmd_buffer;
  dest->pub.free_in_buffer = dest->jmd_max_bufsz;
  *(dest->jmd_actual_bufszptr) = 0;
}


/*
 * Empty the output buffer --- called whenever buffer fills up.
 *
 * Should never happen for the jpeg memory destination (jmd) routines.
 */

METHODDEF boolean
jmd_empty_output_buffer (j_compress_ptr cinfo)
{
    fprintf(stderr,"jmd_empty_output_buffer called\n");
    exit(1);
    return 1;
}


/*
 * Terminate destination --- called by jpeg_finish_compress
 * after all data has been written.  Usually needs to flush buffer.
 *
 * A no-op for the jpeg memory destination (jmd) routines,
 * except that it records the actual buffer size.
 */

METHODDEF void
jmd_term_destination (j_compress_ptr cinfo)
{
  jmd_my_dest_ptr dest = (jmd_my_dest_ptr) cinfo->dest;

  *(dest->jmd_actual_bufszptr) = dest->jmd_max_bufsz - dest->pub.free_in_buffer;
}


/*
 * Prepare for output to a memory buffer
 * The caller must have already opened the stream, and is responsible
 * for closing it after finishing compression.
 */

GLOBAL void
spec_jpeg_mem_dest (j_compress_ptr cinfo, JOCTET* jmd_buffer, int jmd_max_bufsz, int* jmd_actual_bufszptr)
{
  jmd_my_dest_ptr dest;

  /*
   * The destination object is made permanent.
   * Conceivably this could be used to output multiple
   * images to the same memory buffer; it was actually just copied from jpeg_stdio_dest().
   * This makes it dangerous to use this manager and a different destination
   * manager serially with the same JPEG object, because their private object
   * sizes may be different.  Caveat programmer.
   */
  
  if (cinfo->dest == NULL) {	/* first time for this JPEG object? */
      cinfo->dest =
        (struct jmd_my_destination_mgr *)
        alloc_small_wrapper ((j_common_ptr) cinfo, JPOOL_PERMANENT,
                             SIZEOF(jmd_my_destination_mgr));
      if( cinfo->dest == NULL ) {
	  fprintf(stderr,"error allocating jpeg memory destination buffer\n");
	  exit(1);
	  /* TBD: should use libjpeg internal error reporting. */
      }
  }

  dest = (jmd_my_dest_ptr) cinfo->dest;

  /* Methods */
  dest->pub.init_destination 	= jmd_init_destination;
  dest->pub.empty_output_buffer = jmd_empty_output_buffer;
  dest->pub.term_destination 	= jmd_term_destination;

  /* Data structures */
  dest->jmd_buffer = jmd_buffer;
  dest->jmd_max_bufsz = jmd_max_bufsz;
  dest->jmd_actual_bufszptr = jmd_actual_bufszptr;
}



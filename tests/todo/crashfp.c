
/**************************************************************************
*                                                                         *
*         Java Grande Forum Benchmark Suite - Thread Version 1.0          *
*                                                                         *
*                            produced by                                  *
*                                                                         *
*                  Java Grande Benchmarking Project                       *
*                                                                         *
*                                at                                       *
*                                                                         *
*                Edinburgh Parallel Computing Centre                      *
*                                                                         * 
*                email: epcc-javagrande@epcc.ed.ac.uk                     *
*                                                                         *
*                  Original version of this code by                       *
*                 Gabriel Zachmann (zach@igd.fhg.de)                      *
*                                                                         *
*      This version copyright (c) The University of Edinburgh, 2001.      *
*                         All rights reserved.                            *
*                                                                         *
**************************************************************************/


/**
*
* This test performs IDEA encryption then decryption. IDEA stands
* for International Data Encryption Algorithm. The test is based
* on code presented in Applied Cryptography by Bruce Schnier,
* which was based on code developed by Xuejia Lai and James L.
* Massey.

**/

#include <stdlib.h>
#include <stdio.h>
#include <cpj.h>

#define KEYSZ (USERKEYSZ*6 + 4)
#define USERKEYSZ 8

void Do(int REF(V > nthreads) array_rows,
        int REF(V > 0) nthreads,
        char* START ARRAY VALIDPTR SIZE(4*array_rows) plain1) CHECK_TYPE
{
  int slice;
  int ub;
  int lbi;

  /* yes this is silly, but right now it crashes fp */
  // slice = 0;
 
  /* Switching the next line for the previous enables loop forever
     mode */
  slice = nondetpos();

  ub = array_rows/slice;

  foreach(i, 0, ub)
    lbi = i*slice;
    run1(lbi, lbi+slice, array_rows, plain1);
  endfor
}

void
run1(int REF(V >= 0) ilow,
     int REF(V > ilow) iupper,
     int REF(V > 0) array_rows,
     char * VALIDPTR ARRAY START SIZE_GE(array_rows) text1)
{
  int i1 = ilow; // Index into first text array.
  int x1; // Four "16-bit" blocks, two temps.

  if (iupper > array_rows) {
    iupper = array_rows;
  }

  /* ABAKST added i+8 */
  for (int i =ilow ; i+8 < iupper ; i +=8)
  {
    x1 = text1[i1++] & 0xff;          // Build 16-bit x1 from 2 bytes,
  }   // End for loop.
}




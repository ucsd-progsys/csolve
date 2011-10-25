
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
	char* START ARRAY VALIDPTR SIZE(4*array_rows) plain1,
	char* START ARRAY VALIDPTR SIZE(4*array_rows) crypt1,
	char* START ARRAY VALIDPTR SIZE(4*array_rows) plain2,
	short* START ARRAY VALIDPTR ROOM_FOR(short[USERKEYSZ]) userkey,
	int* START ARRAY VALIDPTR ROOM_FOR(int[KEYSZ]) z,
	int* START ARRAY VALIDPTR ROOM_FOR(int[KEYSZ]) dk)
{
  int slice, tslice, ttslice;
  int start_time, stop_time;
  int ub;

  // Start the stopwatch.
  start_time = nondetpos();
  printf("Length...%d, nthreads = %d", array_rows, nthreads);

  // Encrypt plain1.
  tslice = array_rows / 8;
  ttslice = (tslice + nthreads-1) / nthreads;
  slice = ttslice*8;
  LCC_ASSUME(slice > 0);
  ub = array_rows/slice;
  /* foreach(i, 0, ub) */
  /*   int lbi = i*slice; */
  /*   int ubi = (i+1)*slice; */
  /*   LCC_ASSUME(lbi < ubi); */
  /*   run1(lbi, ubi, array_rows, plain1, crypt1, z); */
  /* endfor */
  for (int i = 0; i < ub; i++)
    {
      int lbi = i*slice;
      int ubi = (i+1)*slice;
      run1(lbi, ubi, array_rows, plain1, crypt1, z);
    }
    
  /* // Decrypt crypt1 */
  /* foreach(i, 0, (array_rows/slice)) */
  /*   ub = (i+1)*slice; */
  /*   ub = ub > array_rows ? array_rows : ub; */
  /*   LCC_ASSUME(ub > i*slice); */
  /*   run1(i*slice, ub, crypt1, plain2, dk); */
  /* endfor */
  for (int i = 0; i < ub; i++)
    {
      int lbi = i*slice;
      int ubi = (i+1)*slice;
      run1(lbi, ubi, array_rows, crypt1, plain2, z);
    }

  // Stop the stopwatch.
  stop_time = nondetpos();
}

/*
* buildTestData
*
* Builds the data used for the test -- each time the test is run.
*/

void buildTestData(int array_rows,
                   char  *ARRAY plain1,
                   short *ARRAY userkey,
		   int   *ARRAY z,
		   int   *ARRAY dk)
{
  // Generate user key randomly; eight 16-bit values in an array.

  for (int i = 0; i < 8; i++)
    {
        // Again, the random number function returns int. Converting
        // to a short type preserves the bit pattern in the lower 16
        // bits of the int and discards the rest.

      userkey[i] = (short) nondetpos();
    }

    // Compute encryption and decryption subkeys.

  calcEncryptKey(userkey, z);
  calcDecryptKey(z, dk);

    // Fill plain1 with "text."
    for (int i = 0; i < array_rows; i++)
    {
      plain1[i] = (char)i;
    }
}

/*
* calcEncryptKey
*
* Builds the 52 16-bit encryption subkeys Z[] from the user key and
* stores in 32-bit int array. The routing corrects an error in the
* source code in the Schnier book. Basically, the sense of the 7-
* and 9-bit shifts are reversed. It still works reversed, but would
* encrypted code would not decrypt with someone else's IDEA code.
*/

void
calcEncryptKey(short * START ARRAY VALIDPTR ROOM_FOR(short[USERKEYSZ]) userkey,
	       int * START ARRAY VALIDPTR ROOM_FOR(int[KEYSZ]) z)
{
    int j;                       // Utility variable.

    for (int i = 0; i < KEYSZ; i++) // Zero out the 52-int Z array.
        z[i] = 0;

    for (int i = 0; i < USERKEYSZ; i++)  // First 8 subkeys are userkey itself.
    {
        z[i] = userkey[i] & 0xffff;     // Convert "unsigned"
                                        // short to int.
    }

    // Each set of 8 subkeys thereafter is derived from left rotating
    // the whole 128-bit key 25 bits to left (once between each set of
    // eight keys and then before the last four). Instead of actually
    // rotating the whole key, this routine just grabs the 16 bits
    // that are 25 bits to the right of the corresponding subkey
    // eight positions below the current subkey. That 16-bit extent
    // straddles two array members, so bits are shifted left in one
    // member and right (with zero fill) in the other. For the last
    // two subkeys in any group of eight, those 16 bits start to
    // wrap around to the first two members of the previous eight.

    for (int i = 8; i < KEYSZ; i++)
    {
        j = i % 8;
        LCC_ASSUME(i >= 8 + j);
        if (j < 6)
        {
	  z[i] = ((z[i -7]>>9) | (z[i-6]<<7)) // Shift and combine.
                    & 0xFFFF;                    // Just 16 bits.
            continue;                            // Next iteration.
        }

        if (j == 6)    // Wrap to beginning for second chunk.
        {
	  z[i] = ((z[i -7]>>9) | (z[i-14]<<7))
                    & 0xFFFF;
            continue;
        }

         // j == 7 so wrap to beginning for both chunks.
        z[i] = ((z[i -15]>>9) | (z[i-14]<<7))
                    & 0xFFFF;
    }
}

/*
* calcDecryptKey
*
* Builds the 52 16-bit encryption subkeys DK[] from the encryption-
* subkeys Z[]. DK[] is a 32-bit int array holding 16-bit values as
* unsigned.
*/

void
calcDecryptKey(int * START VALIDPTR ARRAY ROOM_FOR(int[KEYSZ]) z,
	       int * START VALIDPTR ARRAY ROOM_FOR(int[KEYSZ]) dk)
{
    int j, k;                 // Index counters.
    int t1, t2, t3;           // Temps to hold decrypt subkeys.

    t1 = inv(z[0]);           // Multiplicative inverse (mod x10001).
    t2 = - z[1] & 0xffff;     // Additive inverse, 2nd encrypt subkey.
    t3 = - z[2] & 0xffff;     // Additive inverse, 3rd encrypt subkey.

    dk[51] = inv(z[3]);       // Multiplicative inverse (mod x10001).
    dk[50] = t3;
    dk[49] = t2;
    dk[48] = t1;

    j = 47;                   // Indices into temp and encrypt arrays.
    k = 4;
    for (int i = 0; i < 7; i++)
    {
      LCC_ASSUME(k = 4 + (i*5));
      LCC_ASSUME(j = 47 - (i*6));
        t1 = z[k++];
        dk[j--] = z[k++];
        dk[j--] = t1;
        t1 = inv(z[k++]);
        t2 = -z[k++] & 0xffff;
        t3 = -z[k++] & 0xffff;
        dk[j--] = inv(z[k++]);
        dk[j--] = t2;
        dk[j--] = t3;
        dk[j--] = t1;
    }

    t1 = z[k++];
    dk[j--] = z[k++];
    dk[j--] = t1;
    t1 = inv(z[k++]);
    t2 = -z[k++] & 0xffff;
    t3 = -z[k++] & 0xffff;
    dk[j--] = inv(z[k++]);
    dk[j--] = t3;
    dk[j--] = t2;
    dk[j--] = t1;
}





/*
* mul
*
* Performs multiplication, modulo (2**16)+1. This code is structured
* on the assumption that untaken branches are cheaper than taken
* branches, and that the compiler doesn't schedule branches.
* Java: Must work with 32-bit int and one 64-bit long to keep
* 16-bit values and their products "unsigned." The routine assumes
* that both a and b could fit in 16 bits even though they come in
* as 32-bit ints. Lots of "& 0xFFFF" masks here to keep things 16-bit.
* Also, because the routine stores mod (2**16)+1 results in a 2**16
* space, the result is truncated to zero whenever the result would
* zero, be 2**16. And if one of the multiplicands is 0, the result
* is not zero, but (2**16) + 1 minus the other multiplicand (sort
* of an additive inverse mod 0x10001).

* NOTE: The java conversion of this routine works correctly, but
* is half the speed of using Java's modulus division function (%)
* on the multiplication with a 16-bit masking of the result--running
* in the Symantec Caje IDE. So it's not called for now; the test
* uses Java % instead.
*/

int mul(int a, int b)
{
    long p;             // Large enough to catch 16-bit multiply
                        // without hitting sign bit.
    if (a != 0)
    {
        if(b != 0)
        {
            p = (long) a * b;
            b = (int) p & 0xFFFF;       // Lower 16 bits.
            a = (unsigned int) p >> 16; // Upper 16 bits.

            return (b - a + (b < a ? 1 : 0) & 0xFFFF);
        }
        else
            return ((1 - a) & 0xFFFF);  // If b = 0, then same as
                                        // 0x10001 - a.
    }
    else                                // If a = 0, then return
        return((1 - b) & 0xFFFF);       // same as 0x10001 - b.
}

/*
* inv
*
* Compute multiplicative inverse of x, modulo (2**16)+1 using
* extended Euclid's GCD (greatest common divisor) algorithm.
* It is unrolled twice to avoid swapping the meaning of
* the registers. And some subtracts are changed to adds.
* Java: Though it uses signed 32-bit ints, the interpretation
* of the bits within is strictly unsigned 16-bit.
*/

int inv(int x)
{
    int t0, t1;
    int q, y;

    if (x <= 1)             // Assumes positive x.
        return(x);          // 0 and 1 are self-inverse.

    t1 = 0x10001 / x;       // (2**16+1)/x; x is >= 2, so fits 16 bits.
    y = 0x10001 % x;
    LCC_ASSUME( y > 0 );
    if (y == 1)
        return((1 - t1) & 0xFFFF);

    t0 = 1;
    do {
      LCC_ASSUME( y > 0);
        q = x / y;
        x = x % y;
        t0 += q * t1;
        if (x == 1) return(t0);
        LCC_ASSUME( x > 0);
        q = y / x;
        y = y % x;
        t1 += q * t0;
    } while (y != 1);

    return((1 - t1) & 0xFFFF);
}

/* /\* freeTestData */
/*    Nulls arrays and forces garbage collection to free up memory. *\/ */

/* void freeTestData(char *plain1, */
/* 		  char *crypt1, */
/* 		  char *plain2, */
/* 		  short*userkey, */
/* 		  int  *z, */
/* 		  int  *dk) /\* CHECK_TYPE *\/ */
/* { */
/*   free(plain1); */
/*   free(crypt1); */
/*   free(plain2); */
/*   free(userkey); */
/*   free(z); */
/*   free(dk); */
/* } */


void
run1(int REF(V >= 0) ilow,
     int REF(V >= ilow) iupper,
     int REF(V > 0) array_rows,
     char * VALIDPTR ARRAY START SIZE_GE(array_rows) text1,
     char * VALIDPTR ARRAY START SIZE_GE(array_rows) text2,
     int  * VALIDPTR ARRAY START ROOM_FOR(int[KEYSZ]) key)
{
  /* if(iupper > text1.length) iupper = text1.length; */

  int i1 = ilow;                 // Index into first text array.
  int i2 = ilow;                 // Index into second text array.
  int ik;                     // Index into key array.
  int x1, x2, x3, x4, t1, t2; // Four "16-bit" blocks, two temps.
  int r;                      // Eight rounds of processing.
  
  if (iupper > array_rows) {
    iupper = array_rows;
  }

  /* ABAKST added i+8 */
  for (int i =ilow ; i+8 < iupper ; i +=8)
  {
    ik = 0;                 // Restart key index.
    r = 8;                  // Eight rounds of processing.
    // Load eight plain1 bytes as four 16-bit "unsigned" integers.
    // Masking with 0xff prevents sign extension with cast to int.
    x1 = text1[i1++] & 0xff;          // Build 16-bit x1 from 2 bytes,
    x1 |= (text1[i1++] & 0xff) << 8;  // assuming low-order byte first.
    x2 = text1[i1++] & 0xff;
    x2 |= (text1[i1++] & 0xff) << 8;
    x3 = text1[i1++] & 0xff;
    x3 |= (text1[i1++] & 0xff) << 8;
    x4 = text1[i1++] & 0xff;
    x4 |= (text1[i1++] & 0xff) << 8;

    do {
      LCC_ASSUME(ik == 6*(r - 8));
        // 1) Multiply (modulo 0x10001), 1st text sub-block
        // with 1st key sub-block.
        x1 = (int) ((long) x1 * key[ik++] % 0x10001L & 0xffff);
        // 2) Add (modulo 0x10000), 2nd text sub-block
        // with 2nd key sub-block.

        x2 = x2 + key[ik++] & 0xffff;

        // 3) Add (modulo 0x10000), 3rd text sub-block
        // with 3rd key sub-block.

        x3 = x3 + key[ik++] & 0xffff;

        // 4) Multiply (modulo 0x10001), 4th text sub-block
        // with 4th key sub-block.

        x4 = (int) ((long) x4 * key[ik++] % 0x10001L & 0xffff);

        // 5) XOR results from steps 1 and 3.

        t2 = x1 ^ x3;

        // 6) XOR results from steps 2 and 4.
        // Included in step 8.

        // 7) Multiply (modulo 0x10001), result of step 5
        // with 5th key sub-block.

        t2 = (int) ((long) t2 * key[ik++] % 0x10001L & 0xffff);

        // 8) Add (modulo 0x10000), results of steps 6 and 7.

        t1 = t2 + (x2 ^ x4) & 0xffff;

        // 9) Multiply (modulo 0x10001), result of step 8
        // with 6th key sub-block.

        t1 = (int) ((long) t1 * key[ik++] % 0x10001L & 0xffff);

        // 10) Add (modulo 0x10000), results of steps 7 and 9.

        t2 = t1 + t2 & 0xffff;

        // 11) XOR results from steps 1 and 9.

        x1 ^= t1;

        // 14) XOR results from steps 4 and 10. (Out of order).

        x4 ^= t2;

        // 13) XOR results from steps 2 and 10. (Out of order).

        t2 ^= x2;

        // 12) XOR results from steps 3 and 9. (Out of order).

        x2 = x3 ^ t1;

        x3 = t2;        // Results of x2 and x3 now swapped.
    } while(--r > 0);  // Repeats seven more rounds.

    // Final output transform (4 steps).

    // 1) Multiply (modulo 0x10001), 1st text-block
    // with 1st key sub-block.

    x1 = (int) ((long) x1 * key[ik++] % 0x10001L & 0xffff);

    // 2) Add (modulo 0x10000), 2nd text sub-block
    // with 2nd key sub-block. It says x3, but that is to undo swap
    // of subblocks 2 and 3 in 8th processing round.

    x3 = x3 + key[ik++] & 0xffff;

    // 3) Add (modulo 0x10000), 3rd text sub-block
    // with 3rd key sub-block. It says x2, but that is to undo swap
    // of subblocks 2 and 3 in 8th processing round.

    x2 = x2 + key[ik++] & 0xffff;

    // 4) Multiply (modulo 0x10001), 4th text-block
    // with 4th key sub-block.

    x4 = (int) ((long) x4 * key[ik++] % 0x10001L & 0xffff);

    // Repackage from 16-bit sub-blocks to 8-bit byte array text2.

    text2[i2++] = (char) x1;
    text2[i2++] = (char) (x1 >> 8);
    text2[i2++] = (char) x3;                // x3 and x2 are switched
    text2[i2++] = (char) (x3 >> 8);        // only in name.
    text2[i2++] = (char) x2;
    text2[i2++] = (char) (x2 >> 8);
    text2[i2++] = (char) x4;
    text2[i2++] = (char) (x4 >> 8);

  }   // End for loop.
}

void main() // char ** argv, int argc)
{
  int array_rows = 3000000;
  char * ARRAY plain1, * ARRAY crypt1, * ARRAY plain2;
  short * ARRAY userkey;
  int * ARRAY z, * ARRAY dk;

  // Create three byte arrays that will be used (and reused) for
  // encryption/decryption operations.
  plain1 = malloc(array_rows);
  crypt1 = malloc(array_rows);
  plain2 = malloc(array_rows);

  // Allocate three arrays to hold keys: userkey is the 128-bit key.
  // Z is the set of 16-bit encryption subkeys derived from userkey,
  // while DK is the set of 16-bit decryption subkeys also derived
  // from userkey. NOTE: The 16-bit values are stored here in
  // 32-bit int arrays so that the values may be used in calculations
  // as if they are unsigned. Each 64-bit block of plaintext goes
  // through eight processing rounds involving six of the subkeys
  // then a final output transform with four of the keys; (8 * 6)
  // + 4 = 52 subkeys.
  userkey = malloc(USERKEYSZ*sizeof(short));
  z       = malloc(KEYSZ*sizeof(int));
  dk      = malloc(KEYSZ*sizeof(int));

  if (plain1 && crypt1 && plain2 && userkey && z && dk)
    {
      /* build the keys */
      buildTestData(array_rows, plain1, userkey, z, dk);
      
      Do(array_rows, 4, plain1, crypt1, plain2, userkey, z, dk);
      
      free(plain1);
      free(crypt1);
      free(plain2);
      free(userkey);
      free(z);
      free(dk);
    }
}



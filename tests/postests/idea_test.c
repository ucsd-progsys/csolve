
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

/*
 * MODIFICATIONS/CHEATS:
 *
 * - All key sizes/data sizes are (static) macros. These values were
 *   calculated (or supplied as input) in the original Java
 *   program. We were not able to handle division properly.
 *
 * - CSOLVE_ASSUME in inv() due to arithmetic issues, csolve_mod.
 *
 * - Not a cheat per se, but >>> was replaced with >>, since C has
 *   unsigned types.
 *
 * - the for loop in run1() had its conditional changed to "i+8 <
 *   iupper" from "i < iupper". I believe this was a bug in the
 *   original program, unless the original program was assuming that
 *   iupper would be divisible by 8. 
 *
 * - changed do-while loop in run1() to while loop. We did not
 *   seem to be getting the correct guard information in the do-while
 *   version.
 */
 
  

#include <stdlib.h>
#include <stdio.h>
#include <cpj.h>

#define KEYSZ (USERKEYSZ*6 + 4)
#define USERKEYSZ 8

#define SLICE ((((ARRAY_ROWS/8) + NTHREADS - 1)/NTHREADS)*8)
#define NTHREADS 8
#define ARRAY_ROWS 3000000

int inv (int x) {
    int t0, t1;
    int q, y;

    if (x <= 1)             // Assumes positive x.
        return(x);          // 0 and 1 are self-inverse.

    t1 = 0x10001 / x;       // (2**16+1)/x; x is >= 2, so fits 16 bits.
    y = csolve_mod (0x10001, x);
    CSOLVE_ASSUME (y > 0);
    if (y == 1)
        return((1 - t1) & 0xFFFF);

    t0 = 1;
    do {
        q = x / y;
        x = csolve_mod (x, y);
        CSOLVE_ASSUME (x > 0);
        t0 += q * t1;
        if (x == 1) return(t0);
        q = y / x;
        y = csolve_mod (y, x);
        CSOLVE_ASSUME (y > 0);
        t1 += q * t0;
    } while (y != 1);

    return((1 - t1) & 0xFFFF);
}

void run_cycle(char* STRINGPTR plain1,
               char* STRINGPTR crypt1,
               char* STRINGPTR plain2,
               short* START ARRAY userkey,
               int* START ARRAY z,
               int* START ARRAY dk)
{
  int slice, tslice, ttslice;
  int start_time, stop_time;
  int ub;

  // Start the stopwatch.
  start_time = nondetpos();
  printf("Length...%d, nthreads = %d", ARRAY_ROWS, NTHREADS);

  // Encrypt plain1.
  foreach(i, 0, ARRAY_ROWS/SLICE)
    int lbi = i*SLICE;
    int ubi = (i+1)*SLICE;
    run1(lbi, ubi, plain1, crypt1, z);
  endfor

  // Decrypt crypt1
  foreach(i, 0, ARRAY_ROWS/SLICE)
    int lbi = i*SLICE;
    int ubi = (i+1)*SLICE;
    run1(lbi, ubi, crypt1, plain2, dk);
  endfor

  // Stop the stopwatch.
  stop_time = nondetpos();
  printf("...Completed in %d ticks\n", stop_time-start_time);
}

/*
* buildTestData
*
* Builds the data used for the test -- each time the test is run.
*/

void buildTestData(int array_rows,
                   char  *ARRAY plain1,
                   short *ARRAY START userkey,
                   int   *ARRAY START z,
                   int   *ARRAY START dk)
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
calcEncryptKey(short * START ARRAY userkey,
               int * START ARRAY z)
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
calcDecryptKey(int * ARRAY START z,
               int * ARRAY START dk)
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

void
run1(int ilow,
     int REF(((V - ilow) mod 8) = 0) iupper,
     char * STRINGPTR text1,
     char * STRINGPTR text2,
     int  * ARRAY START key)
{
  int i1 = ilow;                 // Index into first text array.
  int i2 = ilow;                 // Index into second text array.
  int ik;                     // Index into key array.
  int x1, x2, x3, x4, t1, t2; // Four "16-bit" blocks, two temps.
  int r;                      // Eight rounds of processing.

  for (int i = ilow ; i < iupper ; i +=8)
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

    while (r-- > 0)
    {
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
    }

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

    /* Repackage from 16-bit sub-blocks to 8-bit byte array text2. */
    /* CSOLVE_ASSUME(i2 == 8*i); */
    csolve_assert (i2 == i);
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
  /* int array_rows = 3000000; */
  char * plain1, * crypt1, * plain2;
  short * userkey;
  int * z, * dk;

  // Create three byte arrays that will be used (and reused) for
  // encryption/decryption operations.
  plain1 = malloc(ARRAY_ROWS);
  crypt1 = malloc(ARRAY_ROWS);
  plain2 = malloc(ARRAY_ROWS);

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
      /* build the keys and set up some dummy text to encrypt*/
      buildTestData(ARRAY_ROWS, plain1, userkey, z, dk);

      /* perform an encryption/decryption */
      run_cycle(plain1, crypt1, plain2, userkey, z, dk);

      free(plain1);
      free(crypt1);
      free(plain2);
      free(userkey);
      free(z);
      free(dk);
    }
}



/* Convert string to double, using the C locale.

   Copyright (C) 2003-2004, 2009-2011 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* Parse the initial portion of the string pointed to by NPTR as a floating-
   point number (in decimal or hexadecimal notation), like in the C locale:
   accepting only the ASCII digits '0'..'9', and only '.' as decimal point
   character.
   If ENDPTR is not NULL, set *ENDPTR to point to the first byte beyond the
   parsed number or to NPTR if the string does not start with a parseable
   number.
   Return value:
   - If successful, return the value as a double or 'long double',
     respectively, and don't modify errno.
   - In case of overflow, return ±HUGE_VAL or ±HUGE_VALL, respectively, and
     set errno to ERANGE.
   - In case of underflow, return a value very near to 0 and set errno to
     ERANGE.
   - If the string does not start with a number at all, return 0 (and recall
     that if ENDPTR != NULL, *ENDPTR is set to NPTR), and maybe set errno to
     EINVAL.
   - In case of other error, return 0 and set errno, for example to ENOMEM.  */

//typedef double conversion_t (char const * STRINGPTR LOC(L_CONVERT) nptr, char * STRINGPTR LOC(L_CONVERT) * NNSTART NNROOM_FOR(char *) endptr);
typedef double conversion_t (char const * START STRINGPTR LOC(L_CONVERT) nptr, char * NNSTART STRINGPTR LOC(L_CONVERT) * NNSTART NNROOM_FOR(char *) endptr);

extern conversion_t c_strtod OKEXTERN;

extern long double c_strtold (char const *nptr, char ** endptr);

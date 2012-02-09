/* Error-checking interface to strtod-like functions.

   Copyright (C) 1996, 1998, 2003-2004, 2006, 2009-2011 Free Software
   Foundation, Inc.

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

/* Written by Jim Meyering.  */

#ifndef XSTRTOD_H
# define XSTRTOD_H 1

# include <stdbool.h>

//bool xstrtod (const char * STRINGPTR str, const char * NNSTRINGPTR *ptr, double *result,
//              double (*convert) (char const *, char **));


bool xstrtod ( const char NULLTERMSTR * STRINGPTR LOC(L_XSTRTOD) str
             , const char NULLTERMSTR * LOC(L_XSTRTOD) NNSTRINGPTR REF(PNN(PINTO(str,0,1))) * fptr
             , double *result
             , conversion_t *convert) OKEXTERN; 

bool xstrtold (const char *str, const char **ptr, long double *result,
               long double (*convert) (char const *, char **));

#endif /* not XSTRTOD_H */

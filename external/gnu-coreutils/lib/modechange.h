/* modechange.h -- definitions for file mode manipulation

   Copyright (C) 1989-1990, 1997, 2003-2006, 2009-2011 Free Software
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

#if ! defined MODECHANGE_H_
# define MODECHANGE_H_

# include <stdbool.h>
# include <sys/types.h>

struct mode_change * ARRAYSTART VALIDPTR mode_compile (const char NULLTERMSTR * STRINGPTR) OKEXTERN;
struct mode_change *mode_create_from_ref (const char *);
mode_t mode_adjust (mode_t, bool, mode_t, struct mode_change const NULLTERMSTR * ARRAYSTART,
                    mode_t * NNVALIDPTR) OKEXTERN;

//JHALA: copied from lib/modechange.h -- to avoid CIL grumbling about "sizeof(abstract type)"
/* Description of a mode change.  */
struct mode_change
{
  char op;                      /* One of "=+-".  */
  char flag;                    /* Special operations flag.  */
  mode_t affected;              /* Set for u, g, o, or a.  */
  mode_t value;                 /* Bits to add/remove.  */
  mode_t mentioned;             /* Bits explicitly mentioned.  */
};


#endif

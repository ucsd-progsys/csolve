/* Exit with a status code indicating success.
   Copyright (C) 1999-2003, 2005, 2007-2011 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>
#include <stdio.h>
#include <sys/types.h>
#include "system.h"

/* Act like "true" by default; false.c overrides this.  */
#ifndef EXIT_STATUS
# define EXIT_STATUS EXIT_SUCCESS
#endif

#if EXIT_STATUS == EXIT_SUCCESS
# define PROGRAM_NAME "true"
#else
# define PROGRAM_NAME "false"
#endif

#define AUTHORS proper_name ("Jim Meyering")

int
main (int argc, char * ARRAY VALIDPTR LOC(PROGRAM_NAME_LOC) * START NONNULL ARRAY SIZE(argc * 4) argv)
    CHECK_TYPE GLOBAL(PROGRAM_NAME_LOC)
{
  exit (EXIT_STATUS);

  // pmr: csolve needs returns before the ends of function bodies to make the heap
  // constraints work ok
  return 0;
}

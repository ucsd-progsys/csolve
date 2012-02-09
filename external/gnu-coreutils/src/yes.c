// pmr: Needs contextual types to really work

/* yes - output a string repeatedly until killed
   Copyright (C) 1991-1997, 1999-2004, 2007-2011 Free Software Foundation, Inc.

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

/* David MacKenzie <djm@gnu.ai.mit.edu> */

#include <config.h>
#include <stdio.h>
#include <sys/types.h>
#include <getopt.h>

#include "system.h"

#include "error.h"
#include "long-options.h"

/* The official name of this program (e.g., no `g' prefix).  */
#define PROGRAM_NAME "yes"

#define AUTHORS proper_name ("David MacKenzie")

void
usage (int status)
{
  if (status != EXIT_SUCCESS)
    fprintf (stderr, _("Try `%s --help' for more information.\n"),
             program_name);
  else
    {
      printf (_("\
Usage: %s [STRING]...\n\
  or:  %s OPTION\n\
"),
              program_name, program_name);

      fputs (_("\
Repeatedly output a line with all specified STRING(s), or `y'.\n\
\n\
"), stdout);
      fputs (HELP_OPTION_DESCRIPTION, stdout);
      fputs (VERSION_OPTION_DESCRIPTION, stdout);
      emit_ancillary_info ();
    }
  exit (status);

  return;
}

int
main (int REF(V > 0) argc, char NULLTERMSTR * STRINGPTR LOC(PROGRAM_NAME_LOC) * START NONNULL ARRAY SIZE(argc * 4) argv)
  CHECK_TYPE GLOBAL(PROGRAM_NAME_LOC)
{
  initialize_main (&argc, &argv);
  set_program_name (argv[0]);
  setlocale (LC_ALL, "");
  bindtextdomain (PACKAGE, LOCALEDIR);
  textdomain (PACKAGE);

  atexit (close_stdout);

  parse_long_options (argc, argv, PROGRAM_NAME, PACKAGE_NAME, Version,
                      usage, AUTHORS, (char const *) NULL);
  if (getopt_long (argc, argv, "+", NULL, NULL) != -1)
    usage (EXIT_FAILURE);

  // pmr: Very tricky! - here we need contextual types to bail us out
  if (argc <= optind)
    {
      optind = argc;
      // pmr: TODO
      // should be argv[argc++]; for some reason, &argv[argc++] doesn't work
      CSOLVE_UNSAFE_WRITE(argv + argc++, bad_cast ("y"));
    } 

  while (true)
    {
      int i;
      for (i = optind; i < argc; i++)
        // pmr: TODO
        // should be argv[i]; for some reason, &argv[i] doesn't work
        if (fputs (CSOLVE_UNSAFE_READ(argv + i), stdout) == EOF
            || putchar (i == argc - 1 ? '\n' : ' ') == EOF)
          {
            error (0, errno, _("standard output"));
            exit (EXIT_FAILURE);

            return 0;
          }
    }
}

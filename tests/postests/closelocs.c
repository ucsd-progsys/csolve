//! run with --manual

#include <stdlib.h>

void main (char * ARRAY argv[]) {
  char * ARRAY * ARRAY fileptr;

  fileptr = (char **) malloc (sizeof (char *));

  if (**argv) {
      *fileptr = *argv;
  }
}

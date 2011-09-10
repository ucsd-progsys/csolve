//! run with --manual

#include <stdlib.h>

void main (char * NONNULL START VALIDPTR * ARRAY NONNULL VALIDPTR START argv) CHECK_TYPE {
  char * ARRAY * ARRAY fileptr;

  fileptr = (char **) malloc (sizeof (char *));

  if (**argv) {
      *fileptr = *argv;
  }
}

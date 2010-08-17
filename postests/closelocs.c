//! run with -manual

extern void *malloc(int);

void main (char * __attribute__((array)) argv[]) {
  char * __attribute__ ((array)) * __attribute__ ((array)) fileptr;

  fileptr = (char **) malloc (sizeof (char *));

  if (**argv) {
      *fileptr = *argv;
  }
}

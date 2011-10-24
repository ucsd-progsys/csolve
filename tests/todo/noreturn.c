#include <stdlib.h>
#include <liquidc.h>

#define lcc_main(argc, argv)\
  int \
  main( int REF(V > 0) argc \
      , char * STRINGPTR LOC(PROGRAM_NAME_LOC) * START NONNULL ARRAY SIZE(argc * 4) argv \
      ) GLOBAL(PROGRAM_NAME_LOC) CHECK_TYPE

// The following is marked unsafe without the return
lcc_main (argc, argv) {
    exit (0);
    /* return 0; */
}

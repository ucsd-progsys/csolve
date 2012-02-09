#ifndef __HAVE_COREUTILS_CSOLVE
#define __HAVE_COREUTILS_CSOLVE

#define csolve_main(argc, argv)\
  int \
  main( int REF(V > 0) argc \
      , char NULLTERMSTR * STRINGPTR LOC(PROGRAM_NAME_LOC) * START NONNULL ARRAY SIZE(argc * 4) argv \
      ) GLOBAL(PROGRAM_NAME_LOC) CHECK_TYPE

bool csolve_test (const char * STRINGPTR LOC(L) str, const char * LOC(L) NNSTRINGPTR REF(PINTO(str,0,1)) * fptr) OKEXTERN;



#endif

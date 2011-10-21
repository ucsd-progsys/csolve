#include <liquidc.h>

typedef int lcc_dummy_t (char * LOC(L) nptr);
extern lcc_dummy_t lcc_c_strtod OKEXTERN;
extern int lcc_xstrtod (char * LOC(L) str, lcc_dummy_t *convert) OKEXTERN; 

int
main (char **argv)
{
  lcc_xstrtod (argv[0], lcc_c_strtod);
}

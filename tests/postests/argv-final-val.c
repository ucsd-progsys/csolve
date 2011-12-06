#include <liquidc.h>

void main (int REF(V > 0) argc,
           char * STRINGPTR SIZE_GE(1) * START NONNULL ARRAY SIZE(argc * 4) argv)
  CHECK_TYPE
{
    if (argv[0][0] == 'a') {
        lcc_assert (argv[0][0] == 'a');
    }
}

#include <csolve.h>

void main (int REF(V > 0) argc,
           char * STRINGPTR SIZE_GE(1) * START NONNULL ARRAY SIZE(argc * 4) argv)
  CHECK_TYPE
{
    if (argv[0][0] == 'a') {
        csolve_assert (argv[0][0] == 'a');
    }
}

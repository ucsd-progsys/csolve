#include <csolve.h>

void main() {
  char *str = "ABCDEFGH";

  for (int i = 0; i < 9; i++) {
    str[i] = 'a';
  }
  
  /* Should be unsafe because the 0 was overwritten */
  for (int j = 0; str[j]; j++)
    ;
}

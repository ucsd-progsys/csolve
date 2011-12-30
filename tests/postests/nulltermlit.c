#include <csolve.h>

void main() {
  char *str = "ABCDEFGH";

  for (int i = 0; i < 8; i++) {
    str[i] = 'a';
  }

  for (int j = 0; str[j]; j++)
    ;
}

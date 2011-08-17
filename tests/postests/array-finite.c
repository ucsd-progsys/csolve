#include <stdlib.h>

void main(){
  int i;
  int char_to_index[256];
  int index_to_char[257];

  for (i = 0; i<256; i++){ 
    char_to_index[i] = i+1;
    index_to_char[i+1] = i;
  }
 
}

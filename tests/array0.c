extern char* malloc(int);

//int y[10];
//void foo(int x[]/*__attribute__((array))*/){
//  return;
//}
  int char_to_index[256];		
  unsigned char index_to_char[257]; 


start_model()
{ int i;
  for (i = 0; i<256; i++) {		
        char_to_index[i] = i+1;		
        index_to_char[i+1] = i;		
    }
}					

void main(){
//  foo(y);
  return 0;
}

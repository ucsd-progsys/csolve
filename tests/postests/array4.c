extern char *malloc(int);


int *__attribute__((array)) main(){
  int size;
  int *retval;
  
  size = nondetpos();
  retval = (int *) malloc(size * sizeof(int));
  
  for (int i=0; i < size; i++){
    retval[i] = 0;
  }
    
  return retval;
}

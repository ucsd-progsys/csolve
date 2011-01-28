//CRASH 
//int x[10];

void main(){
  int i;
  int y = 0;

  //OK
  int x[10];
  
  for (i = 0; i<10; i++){ 
    x[i] = 1;
    y += i * 4;
  }
}

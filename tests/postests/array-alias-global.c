int x[10];
int y[10];

void main(){
  int i;
  for (i = 0; i<10; i++){ 
    x[i] = 1;
  }
  
  int j = x[5];
  int n = y[j];
  y[j] = 200; 
  //COMMENT THIS OUT AND ITs SAFE!
}

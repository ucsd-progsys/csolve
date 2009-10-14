int hashfunc(unsigned int HashRange, unsigned int key ) 
{ 
  return ((int )((key >> 4) % (unsigned int )HashRange));
}

void main(){
  unsigned int size;
  unsigned int k;
  int h;

  size = nondetpos();
  k    = nondetpos();
  h    = hashfunc(size, k);

  assert(h >= 0);
  assert(h < size);
}




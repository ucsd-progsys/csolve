//testing contextual types for object uniqueness

extern const int sz;

typedef struct _obj obj;

struct _obj {
  int x;
  int FINAL id;
}

int main(int argc, char ** argv)
{
  list ** arr;  //do we need the array?
  arr = malloc(sizeof(obj*) * sz);

  foreach(i in 0 to sz)
    arr[i] = alloc_obj(i);

  foreach(i in 0 to sz)
    arr[i] -> x = i;

  foreach(i in 0 to sz)
    arr[i] -> x = -i;
}


obj* alloc_obj(int i)
{
 obj* o = malloc(sizeof(obj)); 
 o.id = i;
}

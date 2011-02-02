struct pair {
   int fst;
   int snd;
};

struct pair *p;

void set_one (int *x)
{
    *x = 3;
}

int main(void) 
{
    set_one (p);
}

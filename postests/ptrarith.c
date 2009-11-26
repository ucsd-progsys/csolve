extern char *malloc(int);

void main () {
    int  *p;
    int  off;
    char * __attribute__ ((unchecked)) c;

    p           = (int *)malloc(4 * 10);
    off         = nondet() * 4;
    c           = ((char *)p) + off;
    *((int *)c) = 0;
}

    

#include <liquidc.h>

extern char * REF(V >= 0) getPtrInString () OKEXTERN;

void main () {
    char *(*f) () = &getPtrInString;
    char *p = f ();
}

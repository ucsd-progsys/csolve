#include <csolve.h>

extern char * SINGLE REF(V >= 0) getPtrInString () OKEXTERN;

void main () {
    char *(*f) () = &getPtrInString;
    char *p = f ();
}

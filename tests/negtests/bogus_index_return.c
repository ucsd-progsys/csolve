extern char *getPtrInString ();

void main () {
    char *(*f) () = &getPtrInString;
    char *p = f ();
}

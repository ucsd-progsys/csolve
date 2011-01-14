struct FILE {
  unsigned short _cur_column;
  signed char _vtable_offset;
  char _shortbuf[1];
  char *_lock;
};

struct FILE *main () {
    return (struct FILE *) 0;
}

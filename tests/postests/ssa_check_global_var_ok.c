int buffer;

void output_bit (int bit) {
    buffer >>= 1;
    buffer |= 0x80;
}

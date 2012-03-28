#include <csolve.h>
#include <cpj.h>

int global;

void work() {
 global++;
}

void main() {
	foreach(i, 0, 10)
          // global++; UNSAFE
          work();
        endfor
}

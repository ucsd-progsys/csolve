// The following demo illustrates a set of functions for dividing
// a string into its comma-separated fields, then lowercasing each
// field.
//
// CSolve verifies that all memory accesses within the example
// are within bounds.

#include <stdio.h>
#include <csolve.h>
#include <stdlib.h>
#include <ctype.h>

void strntolower (char * s, int n) {
  for (; n-- && *s != '\0'; s++)
    *s = tolower (*s);
}

extern char * LOC(L) NNREF(&& [s <= V; V < s + n; PEQBLOCK(s)])
  strnchr (char * LOC(L) SIZE_GE(n) s, int NONNEG n, char c) OKEXTERN;

typedef struct _csval {
  int            len;
  char *  LOC(L) str;
  struct _csval *next;
} csval;

csval INST(L, S) * revstrncsvals (char * LOC(S) s, int n) {
  csval *last = NULL;
  while (n > 0) {
    csval *v    = (csval *) malloc (sizeof (csval));
    v->next     = last;
    v->str      = s;
    char *comma = strnchr (s, n, ',');

    if (!comma) {
      comma = s + n - 1;
    }

    *comma     = '\0';
    v->len     = comma - s;
    n         -= v->len + 1;
    s          = comma + 1;
    last       = v;
  }

  return last;
}

void lowercase_csvals (csval *v) {
  while (v) {
    strntolower (v->str, v->len);
    v = v->next;
  }
}

int main () {
  int len    = nondetpos ();
  char *line = (char *) malloc (len);
  for (int i = 0; i < len - 1; i++)
    line[i] = nondetpos ();

  csval *vs = revstrncsvals (line, len);
  lowercase_csvals (vs);

  return 0;
}

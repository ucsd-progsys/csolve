// Another strategy: check moduli by index subtyping, check bounds by predicate typing
// Different strategies for aliasing per-function?
// Also, if a type is taken as input, we might say it's aliased by
//   default with the same type in the output; this handles cases like returning what
//   you were passed and inserting an element into a linked list
// Slow as hell - a lot of TRACKSTR going on, maybe it's to blame?

#include <stdio.h>

// BEGIN CAV PORTION

#include <csolve.h>
#include <stdlib.h>
#include <ctype.h>

void strntolower (char * SIZE_GE(n) s, int NONNEG n) CHECK_TYPE {
  for (; n-- && *s != '\0'; s++)
    *s = tolower (*s);
}

// Show unannotated version first, with code, then show extern decl - in its own module
// Also show only the quals we need to verify this first
char * LOC(L) NNREF(&& [s <= V; V < s + n; PEQBLOCK(s)])
  strnchr (char * LOC(L) SIZE_GE(n) s, int NONNEG n, char c) CHECK_TYPE
{
  for (; n-- && *s != '\0'; s++)
    if (*s == c) return s;

  return NULL;
}

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

// END CAV PORTION

/* void printfields (field *f) { */
/*     while (f) { */
/*         printf ("Field: \"%s\", len %d\n", f->str, f->len); */
/*         f = f->next; */
/*     } */
/* } */

/* int main () { */
/*   char *line = NULL; */
/*   ssize_t n  = 0; */

/*   while ((n = getline (&line, &n, stdin)) > 0) { */
/*       field *fs = revstrnfields (line, n); */
/*       printfields (fs); */
/*   } */

/*   return 0; */
/* } */

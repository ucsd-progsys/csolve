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

// Show unannotated version first, with code, then show extern decl - in its own module
// Also show only the quals we need to verify this first
char * ARRAY LOC(L) NNREF(&& [V >= s; V < s + n; PEQBLOCK(s)])
  strnchr (char * STRINGPTR LOC(L) SIZE_GE(n) s, int NONNEG n, char c) CHECK_TYPE
{
  for (; n-- && *s != '\0'; s++)
    if (*s == c) return s;

  return NULL;
}

typedef struct _field {
  int                 len;
  char * ARRAY LOC(L) str;
  struct _field *     next;
} field;

field INST(L, S) * revstrnfields (char * ARRAY LOC(S) s, int n) {
  field *last = NULL;
  while (n > 0) {
    field *f    = (field *) malloc (sizeof (field));
    f->next     = last;
    f->str      = s;
    char *comma = strnchr (s, n, ',');

    if (!comma) {
      f->len = n;
      return f;
    }
    // would prefer to just do comma = s + n - 1,
    // but breaks shape inference because we can't prove the ref offset is positive any more
    // would probably let us reorganize the code to do linked list stuff last

    *comma     = '\0';
    f->len     = comma - s;
    n         -= f->len + 1;
    s          = comma + 1;
    last       = f;
  }

  return last;
}

void strntolower (char * ARRAY s, int n) {
  for (int i = 0; i < n && *s != '\0'; i++)
    s[i] = tolower (s[i]);
}

void lowercase_fields (field *f) {
  while (f) {
    strntolower (f->str, f->len);
    f = f->next;
  }
}

void driver () {
  int len    = nondetpos ();
  char *line = (char *) malloc (len);
  for (int i = 0; i < len - 1; i++)
    line[i] = nondetpos ();

  field *fs = revstrnfields (line, len);
  lowercase_fields (fs);
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

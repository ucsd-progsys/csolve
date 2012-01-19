// Add --bare flag if it doesn't exist so we can check with limited quals
// Make IGNORE_INDEX the default on integers?
// Another strategy: check moduli by index subtyping, check bounds by predicate typing
// Different strategies for aliasing per-function?
// Slow as hell - a lot of TRACKSTR going on, maybe it's to blame?
// Try the whole program with driver and no annots

#include <csolve.h>
#include <stdlib.h>
#include <ctype.h>

// Typedef params inst automatically? (i.e., avoid having to reabstract typedef like this?)
typedef struct _field {
  int                                       len;
  char * ARRAY SIZE_GE(len) LOC(STRLOC)     str;
  struct _field * NNROOM_FOR(struct _field) next;
} INST(STRLOC, STRLOC) field;

// Show unannotated version first, with code, then show extern decl - in its own module
// Also show only the quals we need to verify this first
char * ARRAY LOC(L) NNREF(V >= s) NNREF(V < s + n) NNREF(PEQBLOCK(s))
  strnchr (char * STRINGPTR LOC(L) SIZE_GE(n) s, int NONNEG IGNORE_INDEX n, char c)
  CHECK_TYPE {
  for (; n-- && *s != '\0'; s++)
    if (*s == 'c') return s;

  return NULL;
}

void strntolower (char * STRINGPTR SIZE_GE(n) s, int NONNEG IGNORE_INDEX n)
  CHECK_TYPE {
  for (int i = 0; i < n && *s != '\0'; i++)
    s[i] = tolower (s[i]);
}

field INST(STRLOC, L) * NNROOM_FOR(field) revstrnfields (char * STRINGPTR SIZE_GE(n) LOC(L) s, int NONNEG n)
  CHECK_TYPE
{
  field *last = NULL;
  while (n > 0) {
    field *f    = (field *) malloc (sizeof (field));
    f->next     = last;
    f->str      = s;
    char *comma = strnchr (s, n, ',');
    validptr (s);

    if (!comma) {
      f->len = n;
      break;
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

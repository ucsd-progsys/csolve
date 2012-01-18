// Add --bare flag if it doesn't exist so we can check with limited quals
// Make IGNORE_INDEX the default on integers?
// Another strategy: check moduli by index subtyping, check bounds by predicate typing
// Different strategies for aliasing per-function?
// Slow as hell - a lot of TRACKSTR going on, maybe it's to blame?
// Try the whole program with driver and no annots

#include <csolve.h>
#include <stdlib.h>

// Typedef params inst automatically?
typedef struct {
  int                                   len;
  char * ARRAY SIZE_GE(len) LOC(STRLOC) str;
} INST(STRLOC, STRLOC) field;

typedef struct _field_list {
  field INST(STRLOC, STRLOC) * fld;
  struct _field_list *         next;
} INST(STRLOC, STRLOC) field_list;

// Show unannotated version first, with code, then show extern decl - in its own module
// Also show only the quals we need to verify this first
char * ARRAY LOC(L) NNREF(V >= s) NNREF(V < s + n) NNREF(PEQBLOCK(s))
  strnchr (char * STRINGPTR LOC(L) SIZE_GE(n) s, int NONNEG IGNORE_INDEX n, char c)
  CHECK_TYPE {
  for (; n-- && *s != '\0'; s++)
    if (*s == 'c') return s;

  return NULL;
}

// Also do something with the fields, like lowercase their contents
field_list INST(STRLOC, L) *strnfields (char * STRINGPTR SIZE_GE(n) LOC(L) s, int NONNEG n)
  CHECK_TYPE
{
  field_list head;
  head.fld  = NULL;
  head.next = NULL;

  field_list *last = &head;
  while (n > 0) {
    char *comma = strnchr (s, n, ',');
    CSOLVE_ASSUME (comma != 0);

    field *f = (field *) malloc (sizeof (field));
    f->str = s;

    if (comma) {
      *comma = '\0';
      f->len = comma - s;
    } else {
      f->len = n;
    }

    /* field_list *fl = (field_list *) malloc (sizeof (field_list)); */
    /* fl->fld    = f; */
    /* fl->next   = NULL; */
    /* last->next = fl; */
    /* last       = fl; */

    n -= f->len + 1;
    s = comma + 1; // But note comma may be NULL
  }

  return head.next;
}

// Add --bare flag if it doesn't exist so we can check with limited quals
// Make IGNORE_INDEX the default on integers?
// Another strategy: check moduli by index subtyping, check bounds by predicate typing
// Different strategies for aliasing per-function?

#include <csolve.h>
#include <string.h>
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

// Show unannotated version first, with code, then show extern decl
char * LOC(L) NNREF(V >= s) NNREF(V <= s + n)
  strnchr (char * STRINGPTR LOC(L) SIZE_GE(n) s, int NONNEG IGNORE_INDEX n, char c)
  CHECK_TYPE {
  for (; n-- && *s != '\0'; s++)
    if (*s == 'c') return s;

  return NULL;
}

// Handle null case below
/* field_list INST(STRLOC, L) *strnfields (char * STRINGPTR SIZE_GE(n) LOC(L) s, int NONNEG n) */
/*   CHECK_TYPE */
/* { */
/*   field_list head; */
/*   head.fld  = NULL; */
/*   head.next = NULL; */

/*   field_list *last = &head; */
/*   while (n > 0) { */
/*     char *spc = strnchr (s, n, ' '); */

/*     field *f = (field *) malloc (sizeof (field)); */
/*     f->str = s; */
/*     f->len = spc - s; */

/*     n -= f->len; */

/*     field_list *fl = (field_list *) malloc (sizeof (field_list)); */
/*     fl->fld    = f; */
/*     fl->next   = NULL; */
/*     last->next = fl; */
/*     last       = fl; */
/*   } */

/*   return head.next; */
/* } */

// also uppercase tokens

/* typedef struct _line { */
/*     int                       len; */
/*     char * ARRAY SIZE_GE(len) text; */
/*     struct _line *            next; */
/* } line; */

/* line *filter (line *lines, char * ARRAY SIZE_GE(n) s, int POS n) */
/*   CHECK_TYPE */
/* { */
/*     line first; */
/*     first.len  = 0; */
/*     first.text = 0; */
/*     first.next = lines; */

/*     // Idiomatic version, but not typable without final fields (is it */
/*     // typable *with* them?) */
/*     while (lines) { */
/*         if (lines->next->len >= n && strncmp (lines->next->text, s, n) == 0) {  */
/*             lines = lines->next; */
/*         } else { */
/*             lines->next = lines->next->next; */
/*         } */
/*     } */

/*     /\* line *prev = &first; *\/ */
/*     /\* line *curr = prev->next; *\/ */
/*     /\* while (curr) { *\/ */
/*     /\*     if (curr->len >= n && strncmp (curr->text, s, n) == 0) {  *\/ */
/*     /\*         prev->next = curr; *\/ */
/*     /\*         prev       = curr; *\/ */
/*     /\*         curr       = curr->next; *\/ */
/*     /\*     } else { *\/ */
/*     /\*         curr = curr->next; *\/ */
/*     /\*     } *\/ */
/*     /\* } *\/ */

/*     /\* prev->next = curr; *\/ */

/*     return first.next; */
/* } */

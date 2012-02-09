// TODO (* is a challenge)
// - Fix issue that makes dp necessary in detect_loop
// - Fix "annoying aliasing" below
// * Undo manual peeling of detect_loop loop
// * Undo struct rearrangement
//
// FINAL TODO
// Compare with original tsort, check for divergence

/* tsort - topological sort.
   Copyright (C) 1998-2005, 2007-2011 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* Written by Mark Kettenis <kettenis@phys.uva.nl>.  */

/* The topological sort is done according to Algorithm T (Topological
   sort) in Donald E. Knuth, The Art of Computer Programming, Volume
   1/Fundamental Algorithms, page 262.  */

#include <config.h>

#include <assert.h>
#include <getopt.h>
#include <sys/types.h>

#include <csolve.h>

#include "system.h"
#include "long-options.h"
#include "error.h"
#include "fadvise.h"
#include "quote.h"
#include "readtokens.h"
#include "stdio--.h"

/* The official name of this program (e.g., no `g' prefix).  */
#define PROGRAM_NAME "tsort"

#define AUTHORS proper_name ("Mark Kettenis")

/* Token delimiters when reading from a file.  */
#define DELIM " \t\n"

typedef char * ARRAY char_array; //RJ

/* Members of the list of successors.  */
struct successor
{
  struct item *suc;
  struct successor *next;
};

/* Each string is held in core as the head of a list of successors.  */
struct item
{
  //const char *str;
  const char_array LOC(ITEM_STR_LOC) str;
  struct item *left, *right;
  int balance; /* -1, 0, or +1 */
  size_t count;
  struct item *qlink;
  struct successor *top;
};

/* The head of the sorted list.  */
static struct item * LOC(TS_GLOC) head = NULL;

/* The tail of the list of `zeros', strings that have no predecessors.  */
static struct item * LOC(TS_GLOC) zeros = NULL;

/* Used for loop detection.  */
static struct item * LOC(TS_GLOC) loop = NULL;

/* The number of strings to sort.  */
static size_t n_strings = 0;

void
usage (int status)
{
  if (status != EXIT_SUCCESS)
    fprintf (stderr, _("Try `%s --help' for more information.\n"),
             program_name);
  else
    {
      printf (_("\
Usage: %s [OPTION] [FILE]\n\
Write totally ordered list consistent with the partial ordering in FILE.\n\
With no FILE, or when FILE is -, read standard input.\n\
\n\
"), program_name);
      fputs (HELP_OPTION_DESCRIPTION, stdout);
      fputs (VERSION_OPTION_DESCRIPTION, stdout);
      emit_ancillary_info ();
    }

  exit (status);
}

/* Create a new item/node for STR.  */
static struct item INST(ITEM_STR_LOC, L) *
new_item (const char_array LOC(L) str)
{
  struct item *k = xmalloc (sizeof *k);

  k->str = (str ? xstrdup (str): NULL);
  k->left = k->right = NULL;
  k->balance = 0;

  /* T1. Initialize (COUNT[k] <- 0 and TOP[k] <- ^).  */
  k->count = 0;
  k->qlink = NULL;
  k->top = NULL;

  return k;
}

// pmr: This comment is not very good; bring it up with maintainer?
/* Search binary tree rooted at *ROOT for STR.  Allocate a new tree if
   *ROOT is NULL.  Insert a node/item for STR if not found.  Return
   the node/item found/created for STR.

   This is done according to Algorithm A (Balanced tree search and
   insertion) in Donald E. Knuth, The Art of Computer Programming,
   Volume 3/Searching and Sorting, pages 455--457.  */

// pmr: It seems that root can have a NULL string, but the other
//      nodes cannot. Should we pull root out?
//      Note that in the function t can be both root and non-root
//      nodes, so we'll probably have to make an assumption about
//      NULL-ness.
// pmr: Is this function assuming that the string str must be
//      found in the tree?
static struct item * LOC(L)
search_item (struct item INST(ITEM_STR_LOC, K) * LOC(L) root,
             const char_array LOC(K) str)
{
  struct item *p, *q, *r, *s, *t;
  int a;

  assert (root);

  /* Make sure the tree is not empty, since that is what the algorithm
     below expects.  */
  if (root->right == NULL)
    return (root->right = new_item (str));

  /* A1. Initialize.  */
  t = root;
  s = p = root->right;

  while (true)
    {
      /* A2. Compare.  */

      // pmr: Assumption, folding to be worked away
      char * __csolve_p_str = p->str;
      /* CSOLVE_ASSUME (__csolve_p_str != NULL); */

      a = strcmp (str, /* p->str */ __csolve_p_str);
      if (a == 0)
        return p;

      /* A3 & A4.  Move left & right.  */
      if (a < 0)
        q = p->left;
      else
        q = p->right;

      // pmr: Analysis gets *drastically* slower as more of this loop is uncommented!
      // HERE [[[
      if (q == NULL)
        {
          /* A5. Insert.  */
          q = new_item (str);

          /* A3 & A4.  (continued).  */
          if (a < 0)
            p->left = q;
          else
            p->right = q;

          // pmr: Assumption, folding to be worked away
          char * __csolve_s_str = s->str;
          /* CSOLVE_ASSUME (__csolve_s_str != NULL); */

          /* A6. Adjust balance factors.  */
          assert (!STREQ (str, /* s->str */ __csolve_s_str));
          if (strcmp (str, /* s->str */ __csolve_s_str) < 0)
            {
              r = p = s->left;
              a = -1;
            }
          else
            {
              r = p = s->right;
              a = 1;
            }

          // pmr: What justifies this assumption?
          /* CSOLVE_ASSUME (r != NULL); */

          while (p != q)
            {
              // pmr: What justifies this assumption?
              /* CSOLVE_ASSUME (p != NULL); */

              // pmr: Assumption, folding to be worked away
              char * __csolve_p_str2 = p->str;
              /* CSOLVE_ASSUME (__csolve_p_str2 != NULL); */

              /* assert (!STREQ (str, p->str)); */
              if (strcmp (str, p->str) < 0)
                {
                  p->balance = -1;
                  p = p->left;
                }
              else
                {
                  p->balance = 1;
                  p = p->right;
                }
            }

          /* A7. Balancing act.  */
          if (s->balance == 0 || s->balance == -a)
            {
              s->balance += a;
              return q;
            }

          if (r->balance == a)
            {
              /* A8. Single Rotation.  */
              p = r;
              if (a < 0)
                {
                  s->left = r->right;
                  r->right = s;
                }
              else
                {
                  s->right = r->left;
                  r->left = s;
                }
              s->balance = r->balance = 0;
            }
          else
            {
              /* A9. Double rotation.  */
              if (a < 0)
                {
                  p = r->right;

                  // pmr: What justifies this assumption? Probably balance factor.
                  /* CSOLVE_ASSUME (p != NULL); */

                  r->right = p->left;
                  p->left = r;
                  s->left = p->right;
                  p->right = s;
                }
              else
                {
                  p = r->left;

                  // pmr: What justifies this assumption? Probably balance factor.
                  /* CSOLVE_ASSUME (p != NULL); */

                  r->left = p->right;
                  p->right = r;
                  s->right = p->left;
                  p->left = s;
                }

              s->balance = 0;
              r->balance = 0;
              if (p->balance == a)
                s->balance = -a;
              else if (p->balance == -a)
                r->balance = a;
              p->balance = 0;
            }

          /* A10. Finishing touch.  */
          if (s == t->right)
            t->right = p;
          else
            t->left = p;

          return q;
        }

      // pmr: HERE ]]]

      /* A3 & A4.  (continued).  */
      if (q->balance)
        {
          t = p;
          s = q;
        }

      p = q;
    }

  /* NOTREACHED */
}

/* Record the fact that J precedes K.  */

static void
record_relation (struct item * LOC(L) j, struct item * LOC(L) k)
{
  struct successor *p;

  // Could this be fixed with the better final fields implementation?
  // At the very least, assumptions can be made about fields and they'll
  // hold as long as there's no intervening write, no matter what unfolds
  // happen.
  char *jstr = j->str;       // pmr
  char *kstr = k->str;       // pmr
  /* CSOLVE_ASSUME (jstr != NULL); // pmr */
  /* CSOLVE_ASSUME (kstr != NULL); // pmr */

  if (!STREQ (jstr, kstr))
    {
      k->count++;
      p = xmalloc (sizeof *p);
      p->suc = k;
      p->next = j->top;
      j->top = p;
    }
}

// pmr: Initially had the wrong signature: head and zeros were struct item *s, not **s.
static bool
count_items (struct item * LOC(TS_GLOC) unused ATTRIBUTE_UNUSED) GLOBAL(TS_GLOC)
{
  n_strings++;
  return false;
}

static bool
scan_zeros (struct item * LOC(TS_GLOC) k)
  GLOBAL(TS_GLOC)
{
  /* Ignore strings that have already been printed.  */
  if (k->count == 0 && k->str)
    {
      if (head == NULL)
        head = k;
      else
        zeros->qlink = k;

      zeros = k;
    }

  return false;
}

/* Try to detect the loop.  If we have detected that K is part of a
   loop, print the loop on standard error, remove a relation to break
   the loop, and return true.

   The loop detection strategy is as follows: Realise that what we're
   dealing with is essentially a directed graph.  If we find an item
   that is part of a graph that contains a cycle we traverse the graph
   in backwards direction.  In general there is no unique way to do
   this, but that is no problem.  If we encounter an item that we have
   encountered before, we know that we've found a cycle.  All we have
   to do now is retrace our steps, printing out the items until we
   encounter that item again.  (This is not necessarily the item that
   we started from originally.)  Since the order in which the items
   are stored in the tree is not related to the specified partial
   ordering, we may need to walk the tree several times before the
   loop has completely been constructed.  If the loop was found, the
   global variable LOOP will be NULL.  */


static bool
detect_loop (struct item * LOC(TS_GLOC) k)
  GLOBAL(TS_GLOC)
{
  // pmr: Contents of k start out with item
  if (k->count > 0)
    {
      /* K does not have to be part of a cycle.  It is however part of
         a graph that contains a cycle.  */

      if (loop == NULL)
        /* Start traversing the graph at K.  */
        loop = k;
      else
        {
          struct successor **p = &k->top;
          struct successor *dp = *p;    // RJ: deref in while (*p)
                                        // not getting 3-addressed by CIL

          if (dp)                    // RJ: deref 3-addr issue
            {
              if ((*p)->suc == loop)
                {
                  if (k->qlink)
                    {
                      /* We have found a loop.  Retrace our steps.  */
                      while (loop)
                        {
                          struct item *tmp = loop->qlink;

                          fprintf (stderr, "%s: %s\n", program_name,
                                   loop->str);

                          /* Until we encounter K again.  */
                          if (loop == k)
                            {
                              /* Remove relation.  */
                              (*p)->suc->count--;
                              *p = (*p)->next;
                              break;
                            }

                          /* Tidy things up since we might have to
                             detect another loop.  */
                          loop->qlink = NULL;
                          loop = tmp;
                        }

                      while (loop)
                        {
                          struct item *tmp = loop->qlink;

                          loop->qlink = NULL;
                          loop = tmp;
                        }

                      /* Since we have found the loop, stop walking
                         the tree.  */
                      return true;
                    }
                  else
                    {
                      k->qlink = loop;
                      loop = k;
                      goto csolve_loop_break;
                    }
                }

                // pmr: Loop peeling, including copying the variables
                struct successor **p2 = &(*p)->next;
                struct successor *dp2 = *p2;
                while (dp2) {
                    // pmr: REPEATED FROM ABOVE
                    if ((*p2)->suc == loop)
                      {
                        if (k->qlink)
                          {
                            /* We have found a loop.  Retrace our steps.  */
                            while (loop)
                              {
                                struct item *tmp = loop->qlink;
      
                                fprintf (stderr, "%s: %s\n", program_name,
                                         loop->str);
      
                                /* Until we encounter K again.  */
                                if (loop == k)
                                  {
                                    /* Remove relation.  */
                                    (*p2)->suc->count--;
                                    *p2 = (*p2)->next;
                                    break;
                                  }
      
                                /* Tidy things up since we might have to
                                   detect another loop.  */
                                loop->qlink = NULL;
                                loop = tmp;
                              }
      
                            while (loop)
                              {
                                struct item *tmp = loop->qlink;
      
                                loop->qlink = NULL;
                                loop = tmp;
                              }
      
                            /* Since we have found the loop, stop walking
                               the tree.  */
                            return true;
                          }
                        else
                          {
                            k->qlink = loop;
                            loop = k;
                            goto csolve_loop_break;
                          }
                      }

                    p2  = &(*p2)->next;
                    dp2 = *p2;                  // RJ: deref 3-addr issue
                }
            }
        csolve_loop_break: // pmr: from loop peeling
        }
    }

  return false;
}

/* Recurse (sub)tree rooted at ROOT, calling ACTION for each node.
   Stop when ACTION returns true.  */

static bool
recurse_tree (struct item * LOC(TS_GLOC) root,
              bool (GLOBAL(TS_GLOC) *action) (struct item * LOC(TS_GLOC)))
  GLOBAL(TS_GLOC)
{
  if (root->left == NULL && root->right == NULL)
    return (*action) (root);
  else
    {
      if (root->left != NULL)
        if (recurse_tree (root->left, action))
          return true;
      if ((*action) (root))
        return true;
      if (root->right != NULL)
        if (recurse_tree (root->right, action))
          return true;
    }

  return false;
}

/* Walk the tree specified by the head ROOT, calling ACTION for
   each node.  */

static void
walk_tree (struct item * LOC(TS_GLOC) root,
           bool (GLOBAL(TS_GLOC) *action) (struct item * LOC(TS_GLOC)))
  GLOBAL(TS_GLOC)
{
  if (root->right)
    recurse_tree (root->right, action);
}

/* Do a topological sort on FILE.   Return true if successful.  */

static bool
tsort (const char_array file)
{
  bool ok = true;
  struct item *root;
  struct item *j = NULL;
  struct item *k = NULL;
  token_buffer tokenbuffer;
  bool is_stdin = STREQ (file, "-");

  /* Intialize the head of the tree will hold the strings we're sorting.  */
  root = new_item (NULL);

  if (!is_stdin && ! freopen (file, "r", stdin))
    error (EXIT_FAILURE, errno, "%s", file);

  fadvise (stdin, FADVISE_SEQUENTIAL);

  init_tokenbuffer (&tokenbuffer);

  while (1)
    {
      /* T2. Next Relation.  */
      size_t len = readtoken (stdin, DELIM, sizeof (DELIM) - 1, &tokenbuffer);
      if (len == (size_t) -1)
        break;

      assert (len != 0);

      // pmr: Find a way to remove assumption/constant fold
      char * __csolve_buffer = tokenbuffer.buffer;
      /* CSOLVE_ASSUME (__csolve_buffer != NULL); */

      k = search_item (root, /* tokenbuffer.buffer */ __csolve_buffer);
      if (j)
        {
          /* T3. Record the relation.  */
          record_relation (j, k);
          k = NULL;
        }

      j = k;
    }

  if (k != NULL)
    error (EXIT_FAILURE, 0, _("%s: input contains an odd number of tokens"),
           file);

  /* T1. Initialize (N <- n).  */
  walk_tree (root, count_items);

  while (n_strings > 0)
    {
      /* T4. Scan for zeros.  */
      walk_tree (root, scan_zeros);

      while (head)
        {
          struct successor *p = head->top;

          /* T5. Output front of queue.  */
          puts (head->str);
#ifdef lint
          /* suppress valgrind "definitely lost" warnings.  */
          void *head_str = (void *) head->str;
          free (head_str);
#endif
          head->str = NULL;	/* Avoid printing the same string twice.  */
          n_strings--;

          /* T6. Erase relations.  */
          while (p)
            {
              p->suc->count--;
              if (p->suc->count == 0)
                {
                  zeros->qlink = p->suc;
                  zeros = p->suc;
                }

              p = p->next;
            }

          /* T7. Remove from queue.  */
          head = head->qlink;
        }

      /* T8.  End of process.  */
      if (n_strings > 0)
        {
          /* The input contains a loop.  */
          error (0, 0, _("%s: input contains a loop:"), file);
          ok = false;

          /* Print the loop and remove a relation to break it.  */
          do
            walk_tree (root, detect_loop);
          while (loop);
        }
    }

  // pmr: if you use quote(file) in the else branch, then the global location QUOTELOC
  // gets unified with the global location GETTEXT_MSG_LOC. Of course, here again,
  // the unification is read-only, so this is not so bad...
  // We should incorporate a more general idea of *read-only* unified locations
  // and *read-write* unified locations. Generally, we may read from a *set* of
  // abstract locations and take the join of those, but we may only write to a
  // single abstract location.
  if (fclose (stdin) != 0)
    // pmr: Avoid annoying aliasing
    error (EXIT_FAILURE, errno, "%s",
           is_stdin ? _("standard input") : _("PMR! PMR! PMR!")/* quote (file) */);

  return ok;
}

int
main (int REF(V > 0) argc, char * ARRAY VALIDPTR LOC(PROGRAM_NAME_LOC) * START NONNULL ARRAY SIZE(argc * 4) argv)
  CHECK_TYPE GLOBAL(PROGRAM_NAME_LOC)
{
  bool ok;

  initialize_main (&argc, &argv);
  set_program_name (argv[0]);
  setlocale (LC_ALL, "");
  bindtextdomain (PACKAGE, LOCALEDIR);
  textdomain (PACKAGE);

  atexit (close_stdout);

  parse_long_options (argc, argv, PROGRAM_NAME, PACKAGE, Version,
                      usage, AUTHORS, (const char_array) NULL);
  if (getopt_long (argc, argv, "", NULL, NULL) != -1)
    usage (EXIT_FAILURE);

  // pmr: Annotation
  int __csolve_optind = optind;
  if (1 < argc - __csolve_optind)
    {
      error (0, 0, _("extra operand %s"), quote (argv[__csolve_optind + 1]));
      usage (EXIT_FAILURE);
    }

  // pmr: Annotation
  int __csolve_optind2 = optind;
  /* CSOLVE_ASSUME(__csolve_optind2 <= argc); */
  ok = tsort (__csolve_optind2 == argc ? "-" : argv[__csolve_optind2]);

  exit (ok ? EXIT_SUCCESS : EXIT_FAILURE);
  return 0;
}

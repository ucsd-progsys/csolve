#include <stdlib.h>
#include <csolve.h>

typedef struct _SortedList SortedList;

typedef SortedList * NNSTART NNREF(PSIZE(12)) NNREF(V > 0) sortedlistptr;

struct _SortedList {
    int FINAL d;
    sortedlistptr NNSTART NNREF((DEREF([V]): int) >= d) n;
    sortedlistptr NNSTART NNREF((DEREF([V]): int) <= d) p;
};

extern void assert_sorted (SortedList *) OKEXTERN;

#define _g_list_alloc0()       malloc(sizeof (GList))
#define _g_list_alloc()        malloc(sizeof (GList))

typedef struct _GList GList;

struct _GList
{
  int    data;
  GList *next;
  GList *prev;
};

GList* LOC(L) g_list_remove (GList * LOC(L) list, int data) {
  GList *tmp;
  
  tmp = list;
  while (tmp)
    {
      if (tmp->data != data)
	tmp = tmp->next;
      else
	{
	  if (tmp->prev)
	    tmp->prev->next = tmp->next;
	  if (tmp->next)
	    tmp->next->prev = tmp->prev;
	  
	  if (list == tmp)
	    list = list->next;

          // pmr: wretched polymorphism?
/* 	  _g_list_free1 (tmp); */
	  
	  break;
	}
    }
  return list;
}

GList * LOC(L) g_list_remove_all (GList	* LOC(L) list, int data) {
  GList *tmp = list;

  while (tmp)
    {
      if (tmp->data != data)
	tmp = tmp->next;
      else
	{
	  GList *next = tmp->next;

	  if (tmp->prev)
	    tmp->prev->next = next;
	  else
	    list = next;
	  if (next)
	    next->prev = tmp->prev;

          // pmr: ditto polymorphism
/* 	  _g_list_free1 (tmp); */
	  tmp = next;
	}
    }
  return list;
}

GList * LOC(L) g_list_remove_link (GList * LOC(L) list, GList * LOC(L) link) {
  if (link)
    {
      if (link->prev)
	link->prev->next = link->next;
      if (link->next)
	link->next->prev = link->prev;
      
      if (link == list)
	list = list->next;
      
      link->next = NULL;
      link->prev = NULL;
    }
  
  return list;
}

GList * LOC(L) g_list_nth (GList * LOC(L) list, int n) {
  while ((n-- > 0) && list)
    list = list->next;

  return list;
}

GList * LOC(L) g_list_nth_prev (GList * LOC(L) list, int n) {
  while ((n-- > 0) && list)
    list = list->prev;
  
  return list;
}

GList * LOC(L) g_list_insert_sorted (GList * LOC(L) list, int data)
{
  GList *tmp_list = list;
  GList *new_list;
  int    cmp;

  if (!list) {
      new_list = _g_list_alloc0 ();
      new_list->data = data;
      return new_list;
    }

  cmp = data > tmp_list->data;
  while ((tmp_list->next) && (cmp > 0)) {
      tmp_list = tmp_list->next;

      cmp = data > tmp_list->data;
    }

  new_list = _g_list_alloc0 ();
  new_list->data = data;

  if ((!tmp_list->next) && (cmp > 0)) {
      tmp_list->next = new_list;
      new_list->prev = tmp_list;
      return list;
    }

  if (tmp_list->prev) {
      // pmr: This whole block is annotation, for which I introduct the notation:
      // !{
      // pmr: Fold workaround (can't assume about tmp_list->prev directly)
      GList *p       = tmp_list->prev;
      int    a       = csolve_assume (p->data <= data);
      p->next        = new_list;
      new_list->prev = p;
      // }!
    }

  new_list->next = tmp_list;
  tmp_list->prev = new_list;

  if (tmp_list == list)
    return new_list;
  else
    return list;
}

void main () {
    GList *head = NULL;

    while (1) {
        switch (nondet ()) {
        case 0:
            head = g_list_insert_sorted (head, nondet ());
            break;
        case 1:
            head = g_list_nth (head, nondet ());
            break;
        case 2:
            head = g_list_nth_prev (head, nondet ());
            break;
        case 3:
            head = g_list_remove (head, nondet ());
            break;
        case 4:
            head = g_list_remove_all (head, nondet ());
            break;
        case 5:
            head = g_list_remove_link (head, g_list_nth (head, nondet ()));
            break;
        default:
        }
        if (head) {
            assert_sorted (head);
        }
    }
}

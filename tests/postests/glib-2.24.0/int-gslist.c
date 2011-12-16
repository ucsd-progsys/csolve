// Instantiated with int:

#include <stdlib.h>
#include <csolve.h>

#define _g_slist_alloc0()       malloc(sizeof (GSList))
#define _g_slist_alloc()        malloc(sizeof (GSList))
#define _g_slist_free1(slist)   free(slist)
#define _g_slist_free_1(slist)  free(slist)

typedef struct _GSList GSList;

struct _GSList
{
  int    data;
  GSList *next;
};

typedef int gint;

static GSList * LOC(L)
g_slist_insert_sorted_real (GSList * LOC(L) list,
			    int             data)
{
  GSList *tmp_list = list;
  GSList *prev_list = NULL;
  GSList *new_list;
  gint cmp;

  if (!list)
    {
      new_list = _g_slist_alloc ();
      new_list->data = data;
      new_list->next = NULL;
      return new_list;
    }

  cmp = data > tmp_list->data;

  while ((tmp_list->next) && (cmp > 0))
    {
      prev_list = tmp_list;
      tmp_list = tmp_list->next;

      cmp = data > tmp_list->data;
    }

  // pmr: sanity check
  csolve_assert (tmp_list != NULL);

  // We need both tmp_list and new_list unfolded here.
  new_list = _g_slist_alloc ();
  new_list->data = data;

  if ((!tmp_list->next) && (cmp > 0))
    {
      // pmr: sanity check
      csolve_assert (new_list->data > tmp_list->data);

      tmp_list->next = new_list;
      new_list->next = NULL;
      return list;
    }

  if (prev_list)
    {
      // pmr: sanity checks
      csolve_assert (prev_list->data <= data);
      csolve_assert (new_list->data <= tmp_list->data);

      prev_list->next = new_list;
      new_list->next = tmp_list;
      return list;
    }
  else
    {
      // pmr: sanity checks
      csolve_assert (list == tmp_list);
      csolve_assert (new_list->data <= list->data);

      new_list->next = list;
      return new_list;
    }
}

GSList * LOC(L)
g_slist_insert_sorted (GSList * LOC(L) list,
                       int             data)
{
  return g_slist_insert_sorted_real (list, data);
}

GSList * LOC(L)
g_slist_remove (GSList * LOC(L) list,
		int             data)
{
  GSList *tmp, *prev = NULL;

  tmp = list;
  while (tmp)
    {
      if (tmp->data == data)
	{
          if (prev)
	    prev->next = tmp->next;
	  else
	    list = tmp->next;

          // pmr: Seems to screw up finality? (Polymorphism hack in finalfields?)
/* 	  _g_slist_free_1 (tmp); */
	  break;
	}

      prev = tmp;
      tmp = prev->next;
      if (tmp) {
          csolve_assert (tmp->data >= prev->data);
      }
    }

  return list;
}

GSList * LOC(L)
g_slist_remove_all (GSList * LOC(L) list,
		    int             data)
{
  GSList *tmp, *prev = NULL;

  tmp = list;
  while (tmp)
    {
      if (tmp->data == data)
	{
	  GSList *next = tmp->next;

	  if (prev)
	    prev->next = next;
	  else
	    list = next;
	  
/* 	  g_slist_free_1 (tmp); */
	  tmp = next;
	}
      else
	{
	  prev = tmp;
	  tmp = prev->next;
	}
    }

  return list;
}

GSList * LOC(L)
g_slist_remove_link (GSList * LOC(L) list,
	             GSList * LOC(L) link)
{
  GSList *tmp;
  GSList *prev;

  prev = NULL;
  tmp = list;

  while (tmp)
    {
      if (tmp == link)
	{
	  if (prev)
	    prev->next = tmp->next;
	  if (list == tmp)
	    list = list->next;

	  tmp->next = NULL;
	  break;
	}

      prev = tmp;
      tmp = tmp->next;
    }

  return list;
}

GSList * LOC(L)
g_slist_nth (GSList * LOC(L) list,
	     int             n)
{
  while (n-- > 0 && list)
    list = list->next;

  return list;
}

void test_sorted (GSList *hd) {
    GSList *cur = hd;

    while (cur != (GSList *) NULL && cur->next != (GSList *) NULL) {
        csolve_assert (cur->data <= cur->next->data);
        cur = cur->next;
    }

    return;
}

int main () {
    GSList *head = NULL;

    while (1) {
        switch (nondet ()) {
        case 0:
            head = g_slist_insert_sorted (head, nondet ());
            break;
        case 1:
            head = g_slist_remove (head, nondet ());
            break;
        case 2:
            head = g_slist_remove_all (head, nondet ());
            break;
        case 3:
            head = g_slist_remove_link (head, g_slist_nth (head, nondet ()));
            break;
        case 4:
            head = g_slist_nth (head, nondet ());
            break;
        default:
        }
        test_sorted (head);
    }

    return 0;
}

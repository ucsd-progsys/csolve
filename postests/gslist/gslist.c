/*
Original:

static GSList*
g_slist_insert_sorted_real (GSList   *list,
			    gpointer  data,
			    GFunc     func,
			    gpointer  user_data)
{
  GSList *tmp_list = list;
  GSList *prev_list = NULL;
  GSList *new_list;
  gint cmp;
 
  g_return_val_if_fail (func != NULL, list);

  if (!list)
    {
      new_list = _g_slist_alloc ();
      new_list->data = data;
      new_list->next = NULL;
      return new_list;
    }
 
  cmp = ((GCompareDataFunc) func) (data, tmp_list->data, user_data);
 
  while ((tmp_list->next) && (cmp > 0))
    {
      prev_list = tmp_list;
      tmp_list = tmp_list->next;

      cmp = ((GCompareDataFunc) func) (data, tmp_list->data, user_data);
    }

  new_list = _g_slist_alloc ();
  new_list->data = data;

  if ((!tmp_list->next) && (cmp > 0))
    {
      tmp_list->next = new_list;
      new_list->next = NULL;
      return list;
    }
  
  if (prev_list)
    {
      prev_list->next = new_list;
      new_list->next = tmp_list;
      return list;
    }
  else
    {
      new_list->next = list;
      return new_list;
    }
}

GSList*
g_slist_insert_sorted (GSList       *list,
                       gpointer      data,
                       GCompareFunc  func)
{
  return g_slist_insert_sorted_real (list, data, (GFunc) func, NULL);
}
*/

// Instantiated with int:

extern void *malloc(int);

#define NULL                    0
#define _g_slist_alloc0()       malloc(sizeof (GSList))
#define _g_slist_alloc()        malloc(sizeof (GSList))
#define _g_slist_free1(slist)   free(slist)

typedef struct _GSList GSList;

struct _GSList
{
  int    data;
  GSList *next;
};

typedef int gint;

static GSList*
g_slist_insert_sorted_real (GSList   *list,
			    int       data)
{
  GSList *tmp_list = list;
  GSList *prev_list = NULL;
  GSList *new_list;
  gint cmp;

  // g_return_val_if_fail (func != NULL, list);

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

  // The following needs to pass:
  assert (tmp_list->next == 0 || cmp <= 0);

  // We need both tmp_list and new_list unfolded here.
  new_list = _g_slist_alloc ();
  new_list->data = data;

  if ((!tmp_list->next) && (cmp > 0))
    {
      tmp_list->next = new_list;
      new_list->next = NULL;
      return list;
    }
  
  if (prev_list)
    {
      prev_list->next = new_list;
      new_list->next = tmp_list;
      return list;
    }
  else
    {
      new_list->next = list;
      return new_list;
    }
}

GSList*
g_slist_insert_sorted (GSList       *list,
                       int          data)
{
  return g_slist_insert_sorted_real (list, data);
}

int main () {
    GSList *head = NULL;

    while (nondet ()) {
        head = g_slist_insert_sorted (head, nondet ());
    }

    return 0;
}

/**
 * g_slist_insert_sorted:
 * @list: a #GSList
 * @data: the data for the new element
 * @func: the function to compare elements in the list. 
 *     It should return a number > 0 if the first parameter 
 *     comes after the second parameter in the sort order.
 *
 * Inserts a new element into the list, using the given 
 * comparison function to determine its position.
 *
 * Returns: the new start of the #GSList
 */
/* GSList* */
/* g_slist_insert_sorted (GSList       *list, */
/*                        gpointer      data, */
/*                        GCompareFunc  func) */
/* { */
/*   return g_slist_insert_sorted_real (list, data, (GFunc) func, NULL); */
/* } */

/* Copyright 1993 David Fotland.
 * Permission granted to Spec to use and distribute as a computer benchamrk
 * permission is not granted to use or modify this code for any
 * purpose other than benchmarking or computer system performance tuning.
 *
 * This program is available (with a user interface) for:
 * IBM-PC/DOS from Ishi Press Inc San Jose CA (408)944-9900
 * Penpoint from PenGames San Jose Ca (408)985-1236
 */


  
# include "g2hd.h"
  
/* return first element in list and return it to free list */

int gtflist(int* head) {
   int tmp;
   if(*head == EOL)return(ERROR);
   tmp = *head;
   *head = links[*head];
   links[tmp] = freelist;
   freelist = tmp;
   return(list[freelist]);
   }
  
/* count number of common elements in l1 and l2 (sorted lists) */

int comlist(int l1,int l2){
	int count= 0;
	while(TRUE){
		if(l1 == EOL)return(count);
		if(l2 == EOL)return(count);
		if(list[l1] < list[l2])
			l1 = links[l1];
		else if(list[l1] > list[l2])
			l2 = links[l2];
		else if(list[l1] == list[l2]){
			++count;
			l1 = links[l1];
			l2 = links[l2];
			}
		}
	}

void cpylist(int list1,int *list2){  /* copy list1 to list2. list2 must be empty */
   register int ptr,ptr2;

   if(list1 == EOL)return;
   *list2 = freelist; 
   ptr2 = freelist; 
   ptr = list1; 
   while(1){
      list[ptr2] = list[ptr]; 
      ptr = links[ptr];
      if(ptr == EOL)break;
      ptr2 = links[ptr2];
      } 
   freelist = links[ptr2]; 
   links[ptr2] = EOL;
   }
  
  
/* merge list1 into list2, leaving list1 unchanged.  return
 * number of elements added to list2
 */


int mrglist(int list1, int* list2){
   register int ptr1,ptr2, count,temp,temp2;
   count = 0;
   if(list1 == EOL)return(0);
   if(*list2 == EOL){
      cpylist(list1,list2);
      ptr1 = *list2;
      while(ptr1 != EOL){
        ++count;
        ptr1 = links[ptr1];
        }
      return(count);
      }

   if (list[list1] < list[*list2]) {
      temp = *list2;
      *list2 = freelist; 
      freelist = links[freelist];
      links[*list2] = temp;
      list[*list2] = list[list1];
      ptr1 = links[list1];
      ++count;
      }
   else if(list[list1] == list[*list2])
      ptr1 = links[list1];
   else ptr1 = list1;

   ptr2 = *list2;

   while(ptr1 != EOL){

/* guaranteed that list[ptr1] > list[ptr2] */

      if(links[ptr2] == EOL){         /* end case */
         links[ptr2] = freelist;
         while(ptr1 != EOL){
            list[freelist] = list[ptr1];
            ptr2 = freelist;
            freelist = links[freelist];
            ptr1 = links[ptr1];
            ++count;
            }
         links[ptr2] = EOL;
         return(count);
         }

      temp2 = list[links[ptr2]];
      if(list[ptr1] < temp2){
         temp = links[ptr2];
         links[ptr2] = freelist;
         ptr2 = freelist;
         freelist = links[freelist];
         links[ptr2] = temp;
         list[ptr2] = list[ptr1];
         ++count;
         ptr1 = links[ptr1];
         }
      else if(list[ptr1] == temp2)
         ptr1 = links[ptr1];
      else ptr2 = links[ptr2];
      }

   return(count);
   } 

  
int andlist(int list1,int list2,int* head){
				/* find elements in both list1 and list2 and
				 * add them to list at *head returns number 
                                 * added to head */
   register int count = 0;
   while(list1 != EOL && list2 != EOL){
      if(list[list1] == list[list2]){
         addlist(list[list1],head);
         list1 = links[list1];
         list2 = links[list2];
         ++count;
         }
      else if(list[list1] < list[list2])
         list1 = links[list1];
      else
         list2 = links[list2];
      }
   return(count);
   }


/* add value to sorted list at head */
/* returns TRUE if new value added */
/* returns FALSE if value is duplicate */

int addlist(int value,int* head){
   register int ptr,optr;

   if(list[*head] > value){ 		/* add to front of list */
      ptr = *head;
      *head = freelist; 
      freelist = links[freelist];
      links[*head] = ptr;
      list[*head] = value;
      return(TRUE); 
      } 
   if(list[*head] == value)return(FALSE);
   optr = *head; 
   while(1){
      ptr = links[optr];
      if(list[ptr] >= value){
   	if(list[ptr] == value)return(FALSE);
   	links[optr] = freelist;
   	list[freelist] = value;
   	freelist = links[freelist];
   	links[links[optr]] = ptr;
   	return(TRUE);
	}
      optr = links[ptr];
      if(list[optr] >= value){
   	if(list[optr] == value)return(FALSE);
   	links[ptr] = freelist;
   	list[freelist] = value;
   	freelist = links[freelist];
   	links[links[ptr]] = optr;
   	return(TRUE);
   	}
      ptr = links[optr];
      if(list[ptr] >= value){
   	if(list[ptr] == value)return(FALSE);
   	links[optr] = freelist;
   	list[freelist] = value;
   	freelist = links[freelist];
   	links[links[optr]] = ptr;
   	return(TRUE);
	}
      optr = links[ptr];
      if(list[optr] >= value){
   	if(list[optr] == value)return(FALSE);
   	links[ptr] = freelist;
   	list[freelist] = value;
   	freelist = links[freelist];
   	links[links[ptr]] = optr;
   	return(TRUE);
   	}
      }
   }
  



/* add value to front of unsorted list at head */
/* always returns TRUE */

int adflist(int value,int* head){
   register int ptr;

   if(*head == EOL){
      *head = freelist;
      freelist = links[freelist];
      links[*head] = EOL;
      list[*head] = value;
      return(TRUE);
      }
   ptr = *head;
   *head = freelist;
   freelist = links[freelist];
   links[*head] = ptr;
   list[*head] = value;
   return(TRUE);
   }


  
/* delete value from sorted list at *head.  return true if successful */

int dellist(int value,int* head) {
   register int ptr,optr;
   if(list[*head] >= value){  /* found on head of list */
      if(list[*head] != value)return(FALSE);
      ptr = *head;
      *head = links[*head];
      links[ptr] = freelist; 
      freelist = ptr; 
      return(TRUE); 
      } 
   optr = *head; 
   while(1){ 
      ptr = links[optr];
      if(list[ptr] >= value){
   	if(list[ptr] != value)return(FALSE); 
   	links[optr] = links[ptr];
   	links[ptr] = freelist;
   	freelist = ptr;
   	return(TRUE);
	}
      optr = links[ptr];
      if(list[optr] >= value){
   	if(list[optr] != value)return(FALSE); 
   	links[ptr] = links[optr];
   	links[optr] = freelist;
   	freelist = optr;
   	return(TRUE);
	}
      ptr = links[optr];
      if(list[ptr] >= value){
   	if(list[ptr] != value)return(FALSE); 
   	links[optr] = links[ptr];
   	links[ptr] = freelist;
   	freelist = ptr;
   	return(TRUE);
	}
      optr = links[ptr];
      if(list[optr] >= value){
   	if(list[optr] != value)return(FALSE); 
   	links[ptr] = links[optr];
   	links[optr] = freelist;
   	freelist = optr;
   	return(TRUE);
	}
      } 
   }
  



/* return entire list at head to the free list */

void killist(int* head){
   register int ptr;
   if(*head == EOL)return;
   ptr = *head;
   for(ptr = *head; links[ptr] != EOL; ptr = links[ptr]); /* find last element*/
   links[ptr] = freelist;
   freelist = *head;
   *head = EOL;
   }


/* returns the number of elements in list at head
 */

int cntlist(int* head){
	register int ptr,cnt;
	cnt = 0;
	for(ptr = *head; ptr != EOL; ptr = links[ptr])++cnt;
	return(cnt);
	}


/* newlist returns the number of values in list l2 which are not in
 * list l1.  The number of new values.
 */



int newlist(int l1,int l2){
	int sum;
	sum = 0;
	while(l2 != EOL){
		while(list[l1] < list[l2])l1 = links[l1];
		if(list[l1] != list[l2])++sum;
		l2 = links[l2];
		}
	return(sum);
	}


/* return TRUE if val is in sorted list at *head
 */


int inlist(int val,int* head){
	int l;
	l = *head;
	while(l != EOL){
		if(list[l] == val)return(TRUE);
		if(list[l] > val)return(FALSE);
		l = links[l];
		}
	return(FALSE);
	}

#include <cthreads.h>
#include <stdio.h>

#define ME (*cthread_data(cthread_self()))
#define MAX_WORKERS 16

/* Most of this code comes from the book 
**	"The C programming language"
**		by Brian W. Kernighan and Dennis M. Ritchie
**
** I only added the small part to make it efficient in parallel.
*/

typedef int ALIGN; /* forces alignment */

union header { /* free block header */
	struct{
		union header *ptr; /* next free block */
		unsigned size; /* size of this free block */
	} s;
	ALIGN x; /* force alignment of blocks */
};

typedef union header HEADER;

int name_at_beginning=0;


static HEADER base[MAX_WORKERS]; /*empty lists to get started */
static HEADER *allocp[MAX_WORKERS]; /*last allocated blocks */


static mutex_t sbrk_lock;

init_alloc_par()
{
	int i;
	
	sbrk_lock = mutex_alloc();
	for (i=0;i<MAX_WORKERS;i++){
		base[i].s.ptr = allocp[i] = &base[i];
		base[i].s.size=0;
	}

};


char *malloc_par(nbytes) /* general-purpose storage allocator */
unsigned nbytes;
{
	HEADER *morecore();
	register HEADER *p,*q;
	register int nunits;

	nunits = 1 + (nbytes+sizeof(HEADER)-1)/sizeof(HEADER);
	q=allocp[ME];
	for (p=q->s.ptr; ; q=p, p = p->s.ptr){
		if (p->s.size >= nunits){ /*big enough */
			if (p->s.size == nunits) /* exactly */
				q->s.ptr = p->s.ptr;
			else {	/* allocate tail end */
				p->s.size -= nunits;
				p += p->s.size;
				p->s.size = nunits;
			}
			allocp[ME] = q;
			return((char *) (p+1));
		}
		if (p==allocp[ME]) /* wrapped around free list */
			if ((p = morecore(nunits)) == NULL)
				return(NULL); /* none left */
	}
}



#define NALLOC 2048 /* units to allocate at once */

static HEADER *morecore(nu) /* ask system for memory */
unsigned nu;

{
	char *sbrk();
	register char *cp;
	register HEADER *up;
	register int rnu;

	
	rnu = NALLOC * ((nu+NALLOC-1) / NALLOC);
	mutex_lock(sbrk_lock);
	cp = sbrk(rnu*sizeof(HEADER));
	mutex_unlock(sbrk_lock);
	if ((int)cp== -1) /* no space at all */
		return(NULL);
	up = (HEADER *)cp;
	up->s.size = rnu;
	free_par((char *) (up+1));
	return(allocp[ME]);
}




free_par(ap) /* put block ap in free list */
char *ap;
{
	register HEADER *p, *q;

	p = (HEADER *)ap - 1; /* point to header */
	for (q=allocp[ME]; !(p>q && p < q->s.ptr);q=q->s.ptr)
		if (q>=q->s.ptr && (p>q || p < q->s.ptr))
			break;	/* at one end or other */

	if (p+p->s.size == q->s.ptr) { /* join to upper nbr */
		p->s.size += q->s.ptr->s.size;
		p->s.ptr = q->s.ptr->s.ptr;
	} else
		p->s.ptr = q->s.ptr;
	if (q+q->s.size == p) { /* join to lower nbr */
		q->s.size += p->s.size;
		q->s.ptr = p->s.ptr;
	} else
		q->s.ptr = p;
	allocp[ME] = q;
}

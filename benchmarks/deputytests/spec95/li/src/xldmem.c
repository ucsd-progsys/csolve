/* xldmem - xlisp dynamic memory management routines */
/*	Copyright (c) 1985, by David Michael Betz
	All Rights Reserved
	Permission is granted for unrestricted non-commercial use	*/
/* modified for SPEC by Michael Paton 1994 and Alexander Carlton 1995 */

#include <math.h>
#include <stdarg.h>
#include "xlisp.h"
#include "proto.h"
#include "xtrnl.h"

/* useful definitions */
// sm: changed the size here to correspond to [0] change to struct segment
//#define ALLOCSIZE (sizeof(struct segment) + (anodes-1) * sizeof(NODE))
#define ALLOCSIZE (sizeof(struct segment) + (anodes) * sizeof(NODE))

/* cons - construct a new cons node */
NODE *cons( NODE *x,NODE *y)
{
    NODE *val;
    val = newnode(LIST);
    rplaca(val,x);
    rplacd(val,y);
    return (val);
}

/* consa - (cons x nil) */
NODE *consa( NODE *x)
{
    NODE *val;
    val = newnode(LIST);
    rplaca(val,x);
    return (val);
}

/* consd - (cons nil x) */
NODE *consd( NODE *x)
{
    NODE *val;
    val = newnode(LIST);
    rplacd(val,x);
    return (val);
}

/* cvstring - convert a string to a string node */
NODE *cvstring( char *str)
{
    NODE ***oldstk,*val __HEAPIFY;
    oldstk = xlsave1(&val);
    val = newnode(STR);
    val->n_str = strsave(str);
    val->n_strtype = DYNAMIC;
    xlstack = oldstk;
    return (val);
}

/* cvcstring - convert a constant string to a string node */
NODE *cvcstring( char *str)
{
    NODE *val;
    val = newnode(STR);
    val->n_str = str;
    val->n_strtype = STATIC;
    return (val);
}

/* cvsymbol - convert a string to a symbol */
NODE *cvsymbol( char *pname)
{
    NODE ***oldstk,*val __HEAPIFY;
    oldstk = xlsave1(&val);
    val = newnode(SYM);
    val->n_symplist = newnode(LIST);
    rplaca(val->n_symplist,cvstring(pname));
    xlstack = oldstk;
    return (val);
}

/* cvcsymbol - convert a constant string to a symbol */
NODE *cvcsymbol( char *pname)
{
    NODE ***oldstk,*val __HEAPIFY;
    oldstk = xlsave1(&val);
    val = newnode(SYM);
    val->n_symplist = newnode(LIST);
    rplaca(val->n_symplist,cvcstring(pname));
    xlstack = oldstk;
    return (val);
}

/* cvsubr - convert a function to a subr or fsubr */
//sm: NODE *cvsubr( NODE *(*fcn)(), int type)
NODE *cvsubr( NODE *(*fcn)(NODE*), int type)
{
    NODE *val;
    val = newnode(type);
    val->n_subr = fcn;
    return (val);
}

/* cvfile - convert a file pointer to a file */
NODE *cvfile( FILE *fp)
{
    NODE *val;
    val = newnode(FPTR);
    setfile(val,fp);
    setsavech(val,0);
    return (val);
}

/* cvfixnum - convert an integer to a fixnum node */
NODE *cvfixnum(FIXNUM n)
{
    NODE *val;
    val = newnode(INT);
    val->n_int = n;
    return (val);
}

/* cvflonum - convert a floating point number to a flonum node */
NODE *cvflonum(FLONUM n)
{
    NODE *val;
    val = newnode(FLOAT);
    val->n_float = n;
    return (val);
}

/* newstring - allocate and initialize a new string */
NODE *newstring(int size)
{
    NODE ***oldstk,*val __HEAPIFY;
    oldstk = xlsave1(&val);
    val = newnode(STR);
    val->n_str = stralloc(size);
    *getstring(val) = 0;
    val->n_strtype = DYNAMIC;
    xlstack = oldstk;
    return (val);
}

/* newobject - allocate and initialize a new object */
NODE *newobject(NODE *cls, int size)
{
    NODE *val;
    val = newvector(size+1);
    setelement(val,0,cls);
    val->n_type = OBJ;
    return (val);
}

/* newvector - allocate and initialize a new vector node */
NODE *newvector(int size)
{
    NODE ***oldstk,*vect __HEAPIFY;
    int bsize;

    /* establish a new stack frame */
    oldstk = xlsave1(&vect);

    /* allocate a vector node and set the size to zero (in case of gc) */
    vect = newnode(VECT);
    vect->n_vsize = 0;

    /* allocate memory for the vector */
    bsize = size * sizeof(NODE *);
    vect->n_vsize = size;
    if ((vect->n_vdata = (NODE **) calloc(1,bsize)) == NULL) {
	findmem();
	if ((vect->n_vdata = (NODE **) calloc(1,bsize)) == NULL)
	    xlfail("insufficient vector space");
    }
    total += (long) bsize;
 
    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the new vector */
    return (vect);
}

/* newnode - allocate a new node */
LOCAL NODE *newnode(int type)
{
    NODE *nnode;

    /* get a free node */
    if ((nnode = fnodes) == NIL) {
	findmem();
	if ((nnode = fnodes) == NIL)
	    xlabort("insufficient node space");
    }

    /* unlink the node from the free list */
    fnodes = cdr(nnode);
    nfree -= 1;

    /* initialize the new node */
    memset(&nnode->n_info, 0, sizeof(nnode->n_info)); //matth
    nnode->n_type = type;
    //rplacd(nnode,NIL); matth

    /* return the new node */
    return (nnode);
}

/* stralloc - allocate memory for a string adding a byte for the terminator */
LOCAL char *stralloc(int size)
{
    char *sptr;

    /* allocate memory for the string copy */
    if ((sptr = malloc(size+1)) == NULL) {
	findmem();  
	if ((sptr = malloc(size+1)) == NULL)
	    xlfail("insufficient string space");
    }
    total += (long) (size+1);

    /* return the new string memory */
    return (sptr);
}

/* strsave - generate a dynamic copy of a string */
LOCAL char *strsave(char *str)
{
    char *sptr;
    int len = strlen(str);

    /* create a new string */
    sptr = stralloc(len);
    //strlcpy(sptr, str, len+1);
    strncpy(sptr,str,len); sptr[len] = 0;

    /* return the new string */
    return (sptr);
}


/* findmem - find more memory by collecting then expanding */
void findmem(void)
{
    gc();
    if (nfree < anodes)
	addseg();
}

/* WES void mark(); George: pulled out of the body of gc() */
void mark(NODE *ptr);

/* gc - garbage collect */
void gc(void)
{
    NODE ***p;

    /* mark the obarray and the current environment */
    mark(obarray);
    mark(xlenv);

    /* mark the evaluation stack */
    for (p = xlstack; p < xlstktop; )
	mark(**p++);

    /* sweep memory collecting all unmarked nodes */
    sweep();

    /* count the gc call */
    gccalls++;
}

/* mark - mark all accessible nodes */
void mark(NODE *ptr)
{
    NODE *this,*prev,*tmp;

    /* just return on nil */
    if (ptr == NIL)
	return;

    /* initialize */
    prev = NIL;
    this = ptr;

    /* mark this list */
    while (TRUE) {

	/* descend as far as we can */
	while (TRUE) {

	    /* check for this node being marked */
	    if (this->n_flags & MARK)
		break;

	    /* mark it and its descendants */
	    else {

		/* mark the node */
		this->n_flags |= MARK;

		/* follow the left sublist if there is one */
		if (livecar(this)) {
		    this->n_flags |= LEFT;
		    tmp = prev;
		    prev = this;
		    this = car(prev);
		    rplaca(prev,tmp);
		}

		/* otherwise, follow the right sublist if there is one */
		else if (livecdr(this)) {
		    this->n_flags &= ~LEFT;
		    tmp = prev;
		    prev = this;
		    this = cdr(prev);
		    rplacd(prev,tmp);
		}
		else
		    break;
	    }
	}

	/* backup to a point where we can continue descending */
	while (TRUE) {

	    /* check for termination condition */
	    if (prev == NIL)
		return;

	    /* check for coming from the left side */
	    if (prev->n_flags & LEFT)
		if (livecdr(prev)) {
		    prev->n_flags &= ~LEFT;
		    tmp = car(prev);
		    rplaca(prev,this);
		    this = cdr(prev);
		    rplacd(prev,tmp);
		    break;
		}
		else {
		    tmp = prev;
		    prev = car(tmp);
		    rplaca(tmp,this);
		    this = tmp;
		}

	    /* otherwise, came from the right side */
	    else {
		tmp = prev;
		prev = cdr(tmp);
		rplacd(tmp,this);
		this = tmp;
	    }
	}
    }
}

/* vmark - mark a vector */
void vmark(NODE *n)
{
    int i;
    for (i = 0; i < getsize(n); ++i)
	mark(getelement(n,i));
}

/* sweep - sweep all unmarked nodes and add them to the free list */
LOCAL void sweep(void)
{
    struct segment *seg;
    //NODE *p;
    int n;

    /* empty the free list */
    fnodes = NIL;
    nfree = 0;

    /* add all unmarked nodes */
    for (seg = segs; seg != NULL; seg = seg->sg_next) {
	//p = &seg->sg_nodes[0];
        NODE *SNT end = 0;
        NODE * BND(__this, end) p = 0;
        end = &seg->sg_nodes[0] + seg->sg_size;
        p = &seg->sg_nodes[0];
	for (n = seg->sg_size; n--; p++) {
	    if (!(p->n_flags & MARK)) {
		switch (ntype(p)) {
		case STR:
			if (p->n_strtype == DYNAMIC && p->n_str != NULL) {
			    total -= (long) (strlen(p->n_str)+1);
			    free(p->n_str);
			}
			break;
		case FPTR:
			if (p->n_fp)
			    fclose(p->n_fp);
			break;
		case VECT:
			if (p->n_vsize) {
			    //sm: total -= (long) (p->n_vsize * sizeof(NODE **));
			    total -= (long) (p->n_vsize * sizeof(*(p->n_vdata)));  // see xlisp.h defn of NODE
			    free(p->n_vdata);
			}
			break;
		}
#ifdef DEPUTY
                memset(&p->n_info, 0, sizeof(p->n_info)); //matth
#else
		rplaca(p,NIL);
#endif
		p->n_type = FREE;
		p->n_flags = 0;
		rplacd(p,fnodes);
		fnodes = p;
		nfree++;
	    }
	    else
		p->n_flags &= ~(MARK | LEFT);
        }
        p = 0;
    }
}

/* addseg - add a segment to the available memory */
int addseg(void)
{
    struct segment *newseg;
    NODE *p;
    int n;

    /* check for zero allocation */
    if (anodes == 0)
	return (FALSE);

    /* allocate a new segment */
    if ((newseg = (struct segment *) calloc(1,ALLOCSIZE)) != NULL) {

	/* initialize the new segment */
	newseg->sg_size = anodes;
	newseg->sg_next = segs;
	segs = newseg;

	/* add each new node to the free list */
	p = &newseg->sg_nodes[0];
	for (n = anodes; n--; ) {
	    rplacd(p,fnodes);
	    fnodes = p++;
	}

	/* update the statistics */
	total += (long) ALLOCSIZE;
	nnodes += anodes;
	nfree += anodes;
	nsegs++;

	/* return successfully */
	return (TRUE);
    }
    else
	return (FALSE);
}
 
/* livecar - do we need to follow the car? */
LOCAL int livecar(NODE *n)
{
    switch (ntype(n)) {
    case OBJ:
    case VECT:
	    vmark(n);
    case SUBR:
    case FSUBR:
    case INT:
    case FLOAT:
    case STR:
    case FPTR:
	    return (FALSE);
    case SYM:
    case LIST:
	    return (car(n) != NIL);
    default:
	    printf("bad node type (%d) found during left scan\n",ntype(n));
	    osfinish ();
	    exit(1);
    }
    /*NOTREACHED*/
}

/* livecdr - do we need to follow the cdr? */
LOCAL int livecdr(NODE *n)
{
    switch (ntype(n)) {
    case SUBR:
    case FSUBR:
    case INT:
    case FLOAT:
    case STR:
    case FPTR:
    case OBJ:
    case VECT:
	    return (FALSE);
    case SYM:
    case LIST:
	    return (cdr(n) != NIL);
    default:
	    printf("bad node type (%d) found during right scan\n",ntype(n));
	    osfinish ();
	    exit(1);
    }
    /*NOTREACHED*/
}

/* stats - print memory statistics */
void stats(void)
{
    sprintf(buf,"Nodes:       %d\n",nnodes);  stdputstr(buf);
    sprintf(buf,"Free nodes:  %d\n",nfree);   stdputstr(buf);
    sprintf(buf,"Segments:    %d\n",nsegs);   stdputstr(buf);
    sprintf(buf,"Allocate:    %d\n",anodes);  stdputstr(buf);
    sprintf(buf,"Total:       %ld\n",total);  stdputstr(buf);
    sprintf(buf,"Collections: %d\n",gccalls); stdputstr(buf);
}

/* xlminit - initialize the dynamic memory module */
void xlminit(void )
{
    /* initialize our internal variables */
    anodes = NNODES;
    total = 0L;
    nnodes = nsegs = nfree = gccalls = 0;
    fnodes = NIL;
    segs = NULL;

    /* initialize structures that are marked by the collector */
    xlenv = obarray = NIL;

    /* allocate the evaluation stack */
    if ((xlstkbase = (NODE ***)malloc(EDEPTH * sizeof(NODE **))) == NULL) {
	printf("insufficient memory");
	osfinish ();
	exit(1);
    }
    total += (long)(EDEPTH * sizeof(NODE **));
    xlstack = xlstktop = xlstkbase + EDEPTH;
}


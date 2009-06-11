/* xlsym - symbol handling routines */
/*	Copyright (c) 1985, by David Michael Betz
	All Rights Reserved
	Permission is granted for unrestricted non-commercial use	*/
/* modified for SPEC by Michael Paton 1994 and Alexander Carlton 1995 */

#include <math.h>
#include <stdarg.h>
#include "xlisp.h"
#include "proto.h"
#include "xtrnl.h"

/* xlenter - enter a symbol into the obarray */
NODE *xlenter(char *name,int type)
{
    NODE ***oldstk,*sym __HEAPIFY,*array;
    int i;

    /* check for nil */
    if (strcmp(name,"NIL") == 0)
	return (NIL);

    /* check for symbol already in table */
    array = getvalue(obarray);
    i = hash(name,HSIZE);
    for (sym = getelement(array,i); sym; sym = cdr(sym))
	if (strcmp(name,getstring(getpname(car(sym)))) == 0)
	    return (car(sym));

    /* make a new symbol node and link it into the list */
    oldstk = xlsave1(&sym);
    sym = consd(getelement(array,i));
    rplaca(sym,xlmakesym(name,type));
    setelement(array,i,sym);
    xlstack = oldstk;

    /* return the new symbol */
    return (car(sym));
}

/* xlsenter - enter a symbol with a static print name */
NODE *xlsenter(char *name)
{
    return (xlenter(name,STATIC));
}

/* xlmakesym - make a new symbol node */
NODE *xlmakesym(char *name,int type)
{
    NODE *sym;
    sym = (type == DYNAMIC ? cvsymbol(name) : cvcsymbol(name));
    setvalue(sym,*name == ':' ? sym : s_unbound);
    return (sym);
}

/* xlframe - create a new environment frame */
NODE *xlframe(NODE *env)
{
    return (consd(env));
}

/* xlbind - bind a value to a symbol */
void xlbind(NODE *sym,NODE *val,NODE *env)
{
    NODE *ptr;

    /* create a new environment list entry */
    ptr = consd(car(env));
    rplaca(env,ptr);

    /* create a new variable binding */
    rplaca(ptr,cons(sym,val));
}

/* xlgetvalue - get the value of a symbol (checked) */
NODE *xlgetvalue(NODE *sym)
{
    register NODE *val;
    while ((val = xlxgetvalue(sym)) == s_unbound)
	xlunbound(sym);
    return (val);
}

/* xlxgetvalue - get the value of a symbol */
NODE *xlxgetvalue(NODE *sym)
{
    register NODE *fp,*ep;
    NODE *val;

    /* check for this being an instance variable */
    if (getvalue(self) && xlobgetvalue(sym,&val))
	return (val);

    /* check the environment list */
    for (fp = xlenv; fp; fp = cdr(fp))
	for (ep = car(fp); ep; ep = cdr(ep))
	    if (sym == car(car(ep)))
		return (cdr(car(ep)));

    /* return the global value */
    return (getvalue(sym));
}

/* xlygetvalue - get the value of a symbol (no instance variables) */
NODE *xlygetvalue(NODE *sym)
{
    register NODE *fp,*ep;

    /* check the environment list */
    for (fp = xlenv; fp; fp = cdr(fp))
	for (ep = car(fp); ep; ep = cdr(ep))
	    if (sym == car(car(ep)))
		return (cdr(car(ep)));

    /* return the global value */
    return (getvalue(sym));
}

/* xlsetvalue - set the value of a symbol */
void xlsetvalue(NODE *sym,NODE *val)
{
    NODE *fp,*ep;

    /* check for this being an instance variable */
    if (getvalue(self) && xlobsetvalue(sym,val))
	return;

    /* look for the symbol in the environment list */
    for (fp = xlenv; fp; fp = cdr(fp))
	for (ep = car(fp); ep; ep = cdr(ep))
	    if (sym == car(car(ep))) {
		rplacd(car(ep),val);
		return;
	    }

    /* store the global value */
    setvalue(sym,val);
}

/* xlgetprop - get the value of a property */
NODE *xlgetprop(NODE *sym,NODE *prp)
{
    NODE *p;
    return ((p = findprop(sym,prp)) ? car(p) : NIL);
}

/* xlputprop - put a property value onto the property list */
void xlputprop(NODE *sym,NODE *val,NODE *prp)
{
    NODE ***oldstk,*p __HEAPIFY,*pair;
    if ((pair = findprop(sym,prp)) == NIL) {
	oldstk = xlsave1(&p);
	p = consa(prp);
	rplacd(p,pair = cons(val,getplist(sym)));
	setplist(sym,p);
	xlstack = oldstk;
    }
    rplaca(pair,val);
}

/* xlremprop - remove a property from a property list */
void xlremprop(NODE *sym,NODE *prp)
{
    NODE *last,*p;
    last = NIL;
    for (p = getplist(sym); consp(p) && consp(cdr(p)); p = cdr(last)) {
	if (car(p) == prp)
	    if (last)
		rplacd(last,cdr(cdr(p)));
	    else
		setplist(sym,cdr(cdr(p)));
	last = cdr(p);
    }
}

/* findprop - find a property pair */
LOCAL NODE *findprop(NODE *sym,NODE *prp)
{
    NODE *p;
    for (p = getplist(sym); consp(p) && consp(cdr(p)); p = cdr(cdr(p)))
	if (car(p) == prp)
	    return (cdr(p));
    return (NIL);
}

/* hash - hash a symbol name string */
int hash(char *str,int len)
{
    int i;
    for (i = 0; *str; )
	i = (i << 2) ^ *str++;
    i %= len;
    return (abs(i));
}

/* xlsinit - symbol initialization routine */
void xlsinit(void)
{
    NODE *array,*p;

    /* initialize the obarray */
    obarray = xlmakesym("*OBARRAY*",STATIC);
    array = newvector(HSIZE);
    setvalue(obarray,array);

    /* add the symbol *OBARRAY* to the obarray */
    p = consa(obarray);
    setelement(array,hash("*OBARRAY*",HSIZE),p);

    /* enter the unbound symbol indicator */
    s_unbound = xlsenter("*UNBOUND*");
    setvalue(s_unbound,s_unbound);
}


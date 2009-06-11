/* xllist - xlisp built-in list functions */
/*	Copyright (c) 1985, by David Michael Betz
	All Rights Reserved
	Permission is granted for unrestricted non-commercial use	*/
/* modified for SPEC by Michael Paton 1994 and Alexander Carlton 1995 */

#include <math.h>
#include <stdarg.h>
#include "xlisp.h"
#include "proto.h"
#include "xtrnl.h"

/* cxr functions */
NODE *xcar(NODE *args) { return (cxr(args,"a")); }
NODE *xcdr(NODE *args) { return (cxr(args,"d")); }

/* cxxr functions */
NODE *xcaar(NODE *args) { return (cxr(args,"aa")); }
NODE *xcadr(NODE *args) { return (cxr(args,"da")); }
NODE *xcdar(NODE *args) { return (cxr(args,"ad")); }
NODE *xcddr(NODE *args) { return (cxr(args,"dd")); }

/* cxxxr functions */
NODE *xcaaar(NODE *args) { return (cxr(args,"aaa")); }
NODE *xcaadr(NODE *args) { return (cxr(args,"daa")); }
NODE *xcadar(NODE *args) { return (cxr(args,"ada")); }
NODE *xcaddr(NODE *args) { return (cxr(args,"dda")); }
NODE *xcdaar(NODE *args) { return (cxr(args,"aad")); }
NODE *xcdadr(NODE *args) { return (cxr(args,"dad")); }
NODE *xcddar(NODE *args) { return (cxr(args,"add")); }
NODE *xcdddr(NODE *args) { return (cxr(args,"ddd")); }

/* cxxxxr functions */
NODE *xcaaaar(NODE *args) { return (cxr(args,"aaaa")); }
NODE *xcaaadr(NODE *args) { return (cxr(args,"daaa")); }
NODE *xcaadar(NODE *args) { return (cxr(args,"adaa")); }
NODE *xcaaddr(NODE *args) { return (cxr(args,"ddaa")); }
NODE *xcadaar(NODE *args) { return (cxr(args,"aada")); }
NODE *xcadadr(NODE *args) { return (cxr(args,"dada")); }
NODE *xcaddar(NODE *args) { return (cxr(args,"adda")); }
NODE *xcadddr(NODE *args) { return (cxr(args,"ddda")); }
NODE *xcdaaar(NODE *args) { return (cxr(args,"aaad")); }
NODE *xcdaadr(NODE *args) { return (cxr(args,"daad")); }
NODE *xcdadar(NODE *args) { return (cxr(args,"adad")); }
NODE *xcdaddr(NODE *args) { return (cxr(args,"ddad")); }
NODE *xcddaar(NODE *args) { return (cxr(args,"aadd")); }
NODE *xcddadr(NODE *args) { return (cxr(args,"dadd")); }
NODE *xcdddar(NODE *args) { return (cxr(args,"addd")); }
NODE *xcddddr(NODE *args) { return (cxr(args,"dddd")); }

/* cxr - common car/cdr routine */
LOCAL NODE *cxr(NODE *args,char *adstr)
{
    NODE *list;

    /* get the list */
    list = xlmatch(LIST,&args);
    xllastarg(args);

    /* perform the car/cdr operations */
    while (*adstr && consp(list))
	list = (*adstr++ == 'a' ? car(list) : cdr(list));

    /* make sure the operation succeeded */
    if (*adstr && list)
	xlfail("bad argument");

    /* return the result */
    return (list);
}

/* xcons - construct a new list cell */
NODE *xcons(NODE *args)
{
    NODE *arg1,*arg2;

    /* get the two arguments */
    arg1 = xlarg(&args);
    arg2 = xlarg(&args);
    xllastarg(args);

    /* construct a new list element */
    return (cons(arg1,arg2));
}

/* xlist - built a list of the arguments */
NODE *xlist(NODE *args)
{
    NODE ***oldstk,*arg __HEAPIFY,*list __HEAPIFY,*val __HEAPIFY,*last;
    NODE *lptr = NIL;

    /* create a new stack frame */
    oldstk = xlsave3(&arg,&list,&val);

    /* initialize */
    arg = args;

    /* evaluate and append each argument */
    for (last = NIL; arg; last = lptr) {

	/* evaluate the next argument */
	val = xlarg(&arg);

	/* append this argument to the end of the list */
	lptr = consa(val);
	if (last == NIL)
	    list = lptr;
	else
	    rplacd(last,lptr);
    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the list */
    return (list);
}

/* xappend - built-in function append */
NODE *xappend(NODE *args)
{
    NODE ***oldstk,*arg __HEAPIFY,*list __HEAPIFY,*last __HEAPIFY,*val __HEAPIFY,*lptr;

    /* create a new stack frame */
    oldstk = xlsave4(&arg,&list,&last,&val);

    /* initialize */
    arg = args;

    /* evaluate and append each argument */
    while (arg) {

	/* evaluate the next argument */
	list = xlmatch(LIST,&arg);

	/* append each element of this list to the result list */
	while (consp(list)) {

	    /* append this element */
	    lptr = consa(car(list));
	    if (last == NIL)
		val = lptr;
	    else
		rplacd(last,lptr);

	    /* save the new last element */
	    last = lptr;

	    /* move to the next element */
	    list = cdr(list);
	}
    }

    /* restore previous stack frame */
    xlstack = oldstk;

    /* return the list */
    return (val);
}

/* xreverse - built-in function reverse */
NODE *xreverse(NODE *args)
{
    NODE ***oldstk,*list __HEAPIFY,*val __HEAPIFY;

    /* create a new stack frame */
    oldstk = xlsave2(&list,&val);

    /* get the list to reverse */
    list = xlmatch(LIST,&args);
    xllastarg(args);

    /* append each element of this list to the result list */
    while (consp(list)) {

	/* append this element */
	val = cons(car(list),val);

	/* move to the next element */
	list = cdr(list);
    }

    /* restore previous stack frame */
    xlstack = oldstk;

    /* return the list */
    return (val);
}

/* xlast - return the last cons of a list */
NODE *xlast(NODE *args)
{
    NODE *list;

    /* get the list */
    list = xlmatch(LIST,&args);
    xllastarg(args);

    /* find the last cons */
    while (consp(list) && cdr(list))
	list = cdr(list);

    /* return the last element */
    return (list);
}

/* xmember - built-in function 'member' */
NODE *xmember(NODE *args)
{
    NODE ***oldstk,*x __HEAPIFY,*list __HEAPIFY,*fcn __HEAPIFY,*val;
    int tresult;

    /* create a new stack frame */
    oldstk = xlsave3(&x,&list,&fcn);

    /* get the expression to look for and the list */
    x = xlarg(&args);
    list = xlmatch(LIST,&args);
    xltest(&fcn,&tresult,&args);
    xllastarg(args);

    /* look for the expression */
    for (val = NIL; consp(list); list = cdr(list))
	if (dotest(x,car(list),fcn) == tresult) {
	    val = list;
	    break;
	}

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (val);
}

/* xassoc - built-in function 'assoc' */
NODE *xassoc(NODE *args)
{
    NODE ***oldstk,*x __HEAPIFY,*alist __HEAPIFY,*fcn __HEAPIFY,*pair,*val;
    int tresult;

    /* create a new stack frame */
    oldstk = xlsave3(&x,&alist,&fcn);

    /* get the expression to look for and the association list */
    x = xlarg(&args);
    alist = xlmatch(LIST,&args);
    xltest(&fcn,&tresult,&args);
    xllastarg(args);

    /* look for the expression */
    for (val = NIL; consp(alist); alist = cdr(alist))
	if ((pair = car(alist)) && consp(pair))
	    if (dotest(x,car(pair),fcn) == tresult) {
		val = pair;
		break;
	    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (val);
}

/* xsubst - substitute one expression for another */
NODE *xsubst(NODE *args)
{
    NODE ***oldstk,*to __HEAPIFY,*from __HEAPIFY,*expr __HEAPIFY,*fcn __HEAPIFY,*val;
    int tresult;

    /* create a new stack frame */
    oldstk = xlsave4(&to,&from,&expr,&fcn);

    /* get the to value, the from value and the expression */
    to = xlarg(&args);
    from = xlarg(&args);
    expr = xlarg(&args);
    xltest(&fcn,&tresult,&args);
    xllastarg(args);

    /* do the substitution */
    val = subst(to,from,expr,fcn,tresult);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (val);
}

/* subst - substitute one expression for another */
LOCAL NODE *subst(NODE *to,NODE *from,NODE *expr,NODE *fcn,int tresult)
{
    NODE ***oldstk,*carval __HEAPIFY,*cdrval __HEAPIFY,*val;

    if (dotest(expr,from,fcn) == tresult)
	val = to;
    else if (consp(expr)) {
	oldstk = xlsave2(&carval,&cdrval);
	carval = subst(to,from,car(expr),fcn,tresult);
	cdrval = subst(to,from,cdr(expr),fcn,tresult);
	val = cons(carval,cdrval);
	xlstack = oldstk;
    }
    else
	val = expr;
    return (val);
}

/* xsublis - substitute using an association list */
NODE *xsublis(NODE *args)
{
    NODE ***oldstk,*alist __HEAPIFY,*expr __HEAPIFY,*fcn __HEAPIFY,*val;
    int tresult;

    /* create a new stack frame */
    oldstk = xlsave3(&alist,&expr,&fcn);

    /* get the assocation list and the expression */
    alist = xlmatch(LIST,&args);
    expr = xlarg(&args);
    xltest(&fcn,&tresult,&args);
    xllastarg(args);

    /* do the substitution */
    val = sublis(alist,expr,fcn,tresult);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (val);
}

/* sublis - substitute using an association list */
LOCAL NODE *sublis(NODE *alist,NODE *expr,NODE *fcn,int tresult)
{
    NODE ***oldstk,*carval __HEAPIFY,*cdrval __HEAPIFY,*val;

    if (val = assoc(expr,alist,fcn,tresult))
	val = cdr(val);
    else if (consp(expr)) {
	oldstk = xlsave2(&carval,&cdrval);
	carval = sublis(alist,car(expr),fcn,tresult);
	cdrval = sublis(alist,cdr(expr),fcn,tresult);
	val = cons(carval,cdrval);
	xlstack = oldstk;
    }
    else
	val = expr;
    return (val);
}

/* assoc - find a pair in an association list */
LOCAL NODE *assoc(NODE *expr,NODE *alist,NODE *fcn,int tresult)
{
    NODE *pair;

    for (; consp(alist); alist = cdr(alist))
	if ((pair = car(alist)) && consp(pair))
	    if (dotest(expr,car(pair),fcn) == tresult)
		return (pair);
    return (NIL);
}

/* xremove - built-in function 'remove' */
NODE *xremove(NODE *args)
{
    NODE ***oldstk,*x __HEAPIFY,*list __HEAPIFY,*fcn __HEAPIFY,*val __HEAPIFY,*p;
    NODE *last = NIL;
    int tresult;

    /* create a new stack frame */
    oldstk = xlsave4(&x,&list,&fcn,&val);

    /* get the expression to remove and the list */
    x = xlarg(&args);
    list = xlmatch(LIST,&args);
    xltest(&fcn,&tresult,&args);
    xllastarg(args);

    /* remove matches */
    while (consp(list)) {

	/* check to see if this element should be deleted */
	if (dotest(x,car(list),fcn) != tresult) {
	    p = consa(car(list));
	    if (val) rplacd(last,p);
	    else val = p;
	    last = p;
	}

	/* move to the next element */
	list = cdr(list);
    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the updated list */
    return (val);
}

/* dotest - call a test function */
int dotest(NODE *arg1,NODE *arg2,NODE *fcn)
{
    NODE ***oldstk,*args __HEAPIFY,*val;

    /* create a new stack frame */
    oldstk = xlsave1(&args);

    /* build an argument list */
    args = consa(arg1);
    rplacd(args,consa(arg2));

    /* apply the test function */
    val = xlapply(fcn,args);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result of the test */
    return (val != NIL);
}

/* xnth - return the nth element of a list */
NODE *xnth(NODE *args)
{
    return (nth(args,TRUE));
}

/* xnthcdr - return the nth cdr of a list */
NODE *xnthcdr(NODE *args)
{
    return (nth(args,FALSE));
}

/* nth - internal nth function */
LOCAL NODE *nth(NODE *args,int carflag)
{
    NODE *list;
    int n;

    /* get n and the list */
    if ((n = getfixnum(xlmatch(INT,&args))) < 0)
	xlfail("bad argument");
    if ((list = xlmatch(LIST,&args)) == NIL)
	xlfail("bad argument");
    xllastarg(args);

    /* find the nth element */
    while (consp(list) && n--)
	list = cdr(list);

    /* return the list beginning at the nth element */
    return (carflag && consp(list) ? car(list) : list);
}

/* xlength - return the length of a list or string */
NODE *xlength(NODE *args)
{
    NODE *arg;
    int n;

    /* get the list or string */
    arg = xlarg(&args);
    xllastarg(args);

    /* find the length of a list */
    if (listp(arg))
	for (n = 0; consp(arg); n++)
	    arg = cdr(arg);

    /* find the length of a string */
    else if (stringp(arg))
	n = strlen(getstring(arg));

    /* find the length of a vector */
    else if (vectorp(arg))
	n = getsize(arg);

    /* otherwise, bad argument type */
    else
	xlerror("bad argument type",arg);

    /* return the length */
    return (cvfixnum((FIXNUM)n));
}

/* xmapc - built-in function 'mapc' */
NODE *xmapc(NODE *args)
{
    return (map(args,TRUE,FALSE));
}

/* xmapcar - built-in function 'mapcar' */
NODE *xmapcar(NODE *args)
{
    return (map(args,TRUE,TRUE));
}

/* xmapl - built-in function 'mapl' */
NODE *xmapl(NODE *args)
{
    return (map(args,FALSE,FALSE));
}

/* xmaplist - built-in function 'maplist' */
NODE *xmaplist(NODE *args)
{
    return (map(args,FALSE,TRUE));
}

/* map - internal mapping function */
LOCAL NODE *map(NODE *args,int carflag,int valflag)
{
    NODE ***oldstk,*fcn __HEAPIFY,*lists __HEAPIFY,*arglist __HEAPIFY,*val __HEAPIFY,*p,*x,*y;
    NODE *last = NIL;

    /* create a new stack frame */
    oldstk = xlsave4(&fcn,&lists,&arglist,&val);

    /* get the function to apply and the first list */
    fcn = xlarg(&args);
    lists = xlmatch(LIST,&args);

    /* save the first list if not saving function values */
    if (!valflag)
	val = lists;

    /* set up the list of argument lists */
    lists = consa(lists);

    /* get the remaining argument lists */
    while (args) {
	lists = consd(lists);
	rplaca(lists,xlmatch(LIST,&args));
    }

    /* if the function is a symbol, get its value */
    if (symbolp(fcn))
	fcn = xleval(fcn);

    /* loop through each of the argument lists */
    for (;;) {

	/* build an argument list from the sublists */
	arglist = NIL;
	for (x = lists; x && (y = car(x)) && consp(y); x = cdr(x)) {
	    arglist = consd(arglist);
	    rplaca(arglist,carflag ? car(y) : y);
	    rplaca(x,cdr(y));
	}

	/* quit if any of the lists were empty */
	if (x) break;

	/* apply the function to the arguments */
	if (valflag) {
	    p = consa(NIL);
	    if (val) rplacd(last,p);
	    else val = p;
	    rplaca(p,xlapply(fcn,arglist));
	    last = p;
	}
	else
	    xlapply(fcn,arglist);
    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the last test expression value */
    return (val);
}

/* xrplca - replace the car of a list node */
NODE *xrplca(NODE *args)
{
    NODE *list,*newcar;

    /* get the list and the new car */
    if ((list = xlmatch(LIST,&args)) == NIL)
	xlfail("bad argument");
    newcar = xlarg(&args);
    xllastarg(args);

    /* replace the car */
    rplaca(list,newcar);

    /* return the list node that was modified */
    return (list);
}

/* xrplcd - replace the cdr of a list node */
NODE *xrplcd(NODE *args)
{
    NODE *list,*newcdr;

    /* get the list and the new cdr */
    if ((list = xlmatch(LIST,&args)) == NIL)
	xlfail("bad argument");
    newcdr = xlarg(&args);
    xllastarg(args);

    /* replace the cdr */
    rplacd(list,newcdr);

    /* return the list node that was modified */
    return (list);
}

/* xnconc - destructively append lists */
NODE *xnconc(NODE *args)
{
    NODE *list,*val;
    NODE *last = NIL;

    /* concatenate each argument */
    for (val = NIL; args; ) {

	/* concatenate this list */
	if (list = xlmatch(LIST,&args)) {

	    /* check for this being the first non-empty list */
	    if (val)
		rplacd(last,list);
	    else
		val = list;

	    /* find the end of the list */
	    while (consp(cdr(list)))
		list = cdr(list);

	    /* save the new last element */
	    last = list;
	}
    }

    /* return the list */
    return (val);
}

/* xdelete - built-in function 'delete' */
NODE *xdelete(NODE *args)
{
    NODE ***oldstk,*x __HEAPIFY,*list __HEAPIFY,*fcn __HEAPIFY,*last,*val;
    int tresult;

    /* create a new stack frame */
    oldstk = xlsave3(&x,&list,&fcn);

    /* get the expression to delete and the list */
    x = xlarg(&args);
    list = xlmatch(LIST,&args);
    xltest(&fcn,&tresult,&args);
    xllastarg(args);

    /* delete leading matches */
    while (consp(list)) {
	if (dotest(x,car(list),fcn) != tresult)
	    break;
	list = cdr(list);
    }
    val = last = list;

    /* delete embedded matches */
    if (consp(list)) {

	/* skip the first non-matching element */
	list = cdr(list);

	/* look for embedded matches */
	while (consp(list)) {

	    /* check to see if this element should be deleted */
	    if (dotest(x,car(list),fcn) == tresult)
		rplacd(last,cdr(list));
	    else
		last = list;

	    /* move to the next element */
	    list = cdr(list);
 	}
    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the updated list */
    return (val);
}

/* xatom - is this an atom? */
NODE *xatom(NODE *args)
{
    NODE *arg;
    arg = xlarg(&args);
    xllastarg(args);
    return (atom(arg) ? true : NIL);
}

/* xsymbolp - is this an symbol? */
NODE *xsymbolp(NODE *args)
{
    NODE *arg;
    arg = xlarg(&args);
    xllastarg(args);
    return (arg == NIL || symbolp(arg) ? true : NIL);
}

/* xnumberp - is this a number? */
NODE *xnumberp(NODE *args)
{
    NODE *arg;
    arg = xlarg(&args);
    xllastarg(args);
    return (fixp(arg) || floatp(arg) ? true : NIL);
}

/* xboundp - is this a value bound to this symbol? */
NODE *xboundp(NODE *args)
{
    NODE *sym;
    sym = xlmatch(SYM,&args);
    xllastarg(args);
    return (getvalue(sym) == s_unbound ? NIL : true);
}

/* xnull - is this null? */
NODE *xnull(NODE *args)
{
    NODE *arg;
    arg = xlarg(&args);
    xllastarg(args);
    return (null(arg) ? true : NIL);
}

/* xlistp - is this a list? */
NODE *xlistp(NODE *args)
{
    NODE *arg;
    arg = xlarg(&args);
    xllastarg(args);
    return (listp(arg) ? true : NIL);
}

/* xconsp - is this a cons? */
NODE *xconsp(NODE *args)
{
    NODE *arg;
    arg = xlarg(&args);
    xllastarg(args);
    return (consp(arg) ? true : NIL);
}

/* xeq - are these equal? */
NODE *xeq(NODE *args)
{
    return (cequal(args,eq));
}

/* xeql - are these equal? */
NODE *xeql(NODE *args)
{
    return (cequal(args,eql));
}

/* xequal - are these equal? */
NODE *xequal(NODE *args)
{
    return (cequal(args,equal));
}

/* cequal - common eq/eql/equal function */
/* weimer: added args to fcn decl */
LOCAL NODE *cequal(NODE *args, int (*fcn)(NODE *, NODE *))
{
    NODE *arg1,*arg2;

    /* get the two arguments */
    arg1 = xlarg(&args);
    arg2 = xlarg(&args);
    xllastarg(args);

    /* compare the arguments */
    return ((*fcn)(arg1,arg2) ? true : NIL);
}


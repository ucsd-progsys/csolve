/* xlbfun.c - xlisp basic built-in functions */
/*	Copyright (c) 1985, by David Michael Betz
	All Rights Reserved
	Permission is granted for unrestricted non-commercial use	*/
/* modified for SPEC by Michael Paton 1994 and Alexander Carlton 1995 */

#include <math.h>
#include <stdarg.h>
#include "xlisp.h"
#include "proto.h"
#include "xtrnl.h"

/* xeval - the built-in function 'eval' */
NODE *xeval(NODE *args)
{
    NODE ***oldstk,*expr __HEAPIFY,*val;

    /* create a new stack frame */
    oldstk = xlsave1(&expr);

    /* get the expression to evaluate */
    expr = xlarg(&args);
    xllastarg(args);

    /* evaluate the expression */
    val = xleval(expr);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the expression evaluated */
    return (val);
}

/* xapply - the built-in function 'apply' */
NODE *xapply(NODE *args)
{
    NODE ***oldstk,*fun __HEAPIFY,*arglist __HEAPIFY,*val;

    /* create a new stack frame */
    oldstk = xlsave2(&fun,&arglist);

    /* get the function and argument list */
    fun = xlarg(&args);
    arglist = xlmatch(LIST,&args);
    xllastarg(args);

    /* if the function is a symbol, get its value */
    if (symbolp(fun))
	fun = xleval(fun);

    /* apply the function to the arguments */
    val = xlapply(fun,arglist);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the expression evaluated */
    return (val);
}

/* xfuncall - the built-in function 'funcall' */
NODE *xfuncall(NODE *args)
{
    NODE ***oldstk,*fun __HEAPIFY,*arglist __HEAPIFY,*val;

    /* create a new stack frame */
    oldstk = xlsave2(&fun,&arglist);

    /* get the function and argument list */
    fun = xlarg(&args);
    arglist = args;

    /* if the function is a symbol, get its value */
    if (symbolp(fun))
	fun = xleval(fun);

    /* apply the function to the arguments */
    val = xlapply(fun,arglist);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the expression evaluated */
    return (val);
}

/* xquote - built-in function to quote an expression */
NODE *xquote(NODE *args)
{
    NODE *val;

    /* get the argument */
    val = xlarg(&args);
    xllastarg(args);

    /* return the quoted expression */
    return (val);
}

/* xfunction - built-in function to quote a function */
NODE *xfunction(NODE *args)
{
    NODE *val;

    /* get the argument */
    val = xlarg(&args);
    xllastarg(args);

    /* create a closure for lambda expressions */
    if (consp(val) && car(val) == s_lambda)
	val = cons(val,xlenv);

    /* otherwise, get the value of a symbol */
    else if (symbolp(val))
	val = xlgetvalue(val);

    /* otherwise, its an error */
    else
	xlerror("not a function",val);

    /* return the function */
    return (val);
}

/* xlambda - lambda function */
NODE *xlambda(NODE *args)
{
    NODE ***oldstk,*fargs __HEAPIFY,*closure __HEAPIFY;

    /* create a new stack frame */
    oldstk = xlsave2(&fargs,&closure);

    /* get the formal argument list */
    fargs = xlmatch(LIST,&args);

    /* create a new function definition */
    closure = cons(fargs,args);
    closure = cons(s_lambda,closure);
    closure = cons(closure,xlenv);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the closure */
    return (closure);
}

/* xbquote - back quote function */
NODE *xbquote(NODE *args)
{
    NODE ***oldstk,*expr __HEAPIFY,*val;

    /* create a new stack frame */
    oldstk = xlsave1(&expr);

    /* get the expression */
    expr = xlarg(&args);
    xllastarg(args);

    /* fill in the template */
    val = bquote1(expr);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (val);
}

/* bquote1 - back quote helper function */
LOCAL NODE *bquote1(NODE *expr)
{
    NODE ***oldstk,*val __HEAPIFY,*list __HEAPIFY,*last,*new;

    /* handle atoms */
    if (atom(expr))
	val = expr;

    /* handle (comma <expr>) */
    else if (car(expr) == s_comma) {
	if (atom(cdr(expr)))
	    xlfail("bad comma expression");
	val = xleval(car(cdr(expr)));
    }

    /* handle ((comma-at <expr>) ... ) */
    else if (consp(car(expr)) && car(car(expr)) == s_comat) {
	oldstk = xlsave2(&list,&val);
	if (atom(cdr(car(expr))))
	    xlfail("bad comma-at expression");
	list = xleval(car(cdr(car(expr))));
	for (last = NIL; consp(list); list = cdr(list)) {
	    new = consa(car(list));
	    if (last)
		rplacd(last,new);
	    else
		val = new;
	    last = new;
	}
	if (last)
	    rplacd(last,bquote1(cdr(expr)));
	else
	    val = bquote1(cdr(expr));
	xlstack = oldstk;
    }

    /* handle any other list */
    else {
	oldstk = xlsave1(&val);
	val = consa(NIL);
	rplaca(val,bquote1(car(expr)));
	rplacd(val,bquote1(cdr(expr)));
	xlstack = oldstk;
    }

    /* return the result */
    return (val);
}

/* xset - built-in function set */
NODE *xset(NODE *args)
{
    NODE *sym,*val;

    /* get the symbol and new value */
    sym = xlmatch(SYM,&args);
    val = xlarg(&args);
    xllastarg(args);

    /* assign the symbol the value of argument 2 and the return value */
    setvalue(sym,val);

    /* return the result value */
    return (val);
}

/* xsetq - built-in function setq */
NODE *xsetq(NODE *args)
{
    NODE ***oldstk,*arg __HEAPIFY,*sym __HEAPIFY,*val __HEAPIFY;

    /* create a new stack frame */
    oldstk = xlsave3(&arg,&sym,&val);

    /* initialize */
    arg = args;

    /* handle each pair of arguments */
    while (arg) {
	sym = xlmatch(SYM,&arg);
	val = xlevarg(&arg);
	xlsetvalue(sym,val);
    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result value */
    return (val);
}

/* xsetf - built-in function 'setf' */
NODE *xsetf(NODE *args)
{
    NODE ***oldstk,*arg __HEAPIFY,*place __HEAPIFY,*value __HEAPIFY;

    /* create a new stack frame */
    oldstk = xlsave3(&arg,&place,&value);

    /* initialize */
    arg = args;

    /* handle each pair of arguments */
    while (arg) {

	/* get place and value */
	place = xlarg(&arg);
	value = xlevarg(&arg);

	/* check the place form */
	if (symbolp(place))
	    xlsetvalue(place,value);
	else if (consp(place))
	    placeform(place,value);
	else
	    xlfail("bad place form");
    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the value */
    return (value);
}

/* placeform - handle a place form other than a symbol */
LOCAL void /*matth: added return type */
placeform(NODE *place,NODE *value)
{
    NODE ***oldstk,*fun,*arg1 __HEAPIFY,*arg2 __HEAPIFY;
    int i;

    /* check the function name */
    if ((fun = xlmatch(SYM,&place)) == s_get) {
	oldstk = xlsave2(&arg1,&arg2);
	arg1 = xlevmatch(SYM,&place);
	arg2 = xlevmatch(SYM,&place);
	xllastarg(place);
	xlputprop(arg1,value,arg2);
	xlstack = oldstk;
    }
    else if (fun == s_svalue || fun == s_splist) {
	oldstk = xlsave1(&arg1);
	arg1 = xlevmatch(SYM,&place);
	xllastarg(place);
	if (fun == s_svalue)
	    setvalue(arg1,value);
	else
	    setplist(arg1,value);
	xlstack = oldstk;
    }
    else if (fun == s_car || fun == s_cdr) {
	oldstk = xlsave1(&arg1);
	arg1 = xlevmatch(LIST,&place);
	xllastarg(place);
	if (consp(arg1))
	    if (fun == s_car)
		rplaca(arg1,value);
	    else
		rplacd(arg1,value);
	xlstack = oldstk;
    }
    else if (fun == s_nth) {
	oldstk = xlsave2(&arg1,&arg2);
	arg1 = xlevmatch(INT,&place);
	arg2 = xlevmatch(LIST,&place);
	xllastarg(place);
	for (i = (int)getfixnum(arg1); i > 0 && consp(arg2); --i)
	    arg2 = cdr(arg2);
	if (consp(arg2))
	    rplaca(arg2,value);
	xlstack = oldstk;
    }

    else if (fun == s_aref) {
	oldstk = xlsave2(&arg1,&arg2);
	arg1 = xlevmatch(VECT,&place);
	arg2 = xlevmatch(INT,&place); i = (int)getfixnum(arg2);
	xllastarg(place);
	if (i < 0 || i >= getsize(arg1))
	    xlerror("index out of range",arg2);
	setelement(arg1,i,value);
	xlstack = oldstk;
    }
    else
	xlfail("bad place form");
}
		       
/* xdefun - built-in function 'defun' */
NODE *xdefun(NODE *args)
{
    return (defun(args,s_lambda));
}

/* xdefmacro - built-in function 'defmacro' */
NODE *xdefmacro(NODE *args)
{
    return (defun(args,s_macro));
}

/* defun - internal function definition routine */
LOCAL NODE *defun(NODE *args,NODE *type)
{
    NODE ***oldstk,*sym __HEAPIFY,*fargs __HEAPIFY,*closure __HEAPIFY;

    /* create a new stack frame */
    oldstk = xlsave3(&sym,&fargs,&closure);

    /* get the function symbol and formal argument list */
    sym = xlmatch(SYM,&args);
    fargs = xlmatch(LIST,&args);

    /* create a new function definition */
    closure = cons(fargs,args);
    closure = cons(type,closure);
    closure = cons(closure,xlenv);

    /* make the symbol point to a new function definition */
    xlsetvalue(sym,closure);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the function symbol */
    return (sym);
}

/* xgensym - generate a symbol */
NODE *xgensym(NODE *args)
{
    char sym[STRMAX+1];
    NODE *x;

    /* get the prefix or number */
    if (args) {
	x = xlarg(&args);
	switch (ntype(x)) {
	case STR:
          //matth: fix for Deputy
          //	strcpy(gsprefix,getstring(x));
                strncpy(gsprefix,getstring(x), STRMAX); gsprefix[STRMAX] = 0;
		break;
	case INT:
		gsnumber = getfixnum(x);
		break;
	default:
		xlerror("bad argument type",x);
	}
    }
    xllastarg(args);

    /* create the pname of the new symbol */
    sprintf(sym,"%s%d",gsprefix,gsnumber++);

    /* make a symbol with this print name */
    return (xlmakesym(sym,DYNAMIC));
}

/* xmakesymbol - make a new uninterned symbol */
NODE *xmakesymbol(NODE *args)
{
    return (makesymbol(args,FALSE));
}

/* xintern - make a new interned symbol */
NODE *xintern(NODE *args)
{
    return (makesymbol(args,TRUE));
}

/* makesymbol - make a new symbol */
LOCAL NODE *makesymbol(NODE *args, int iflag)
{
    NODE ***oldstk,*pname __HEAPIFY,*val;
    char *str;

    /* create a new stack frame */
    oldstk = xlsave1(&pname);

    /* get the print name of the symbol to intern */
    pname = xlmatch(STR,&args);
    xllastarg(args);

    /* make the symbol */
    str = getstring(pname);
    val = (iflag ? xlenter(str,DYNAMIC) : xlmakesym(str,DYNAMIC));

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the symbol */
    return (val);
}

/* xsymname - get the print name of a symbol */
NODE *xsymname(NODE *args)
{
    NODE *sym;

    /* get the symbol */
    sym = xlmatch(SYM,&args);
    xllastarg(args);

    /* return the print name */
    return (getpname(sym));
}

/* xsymvalue - get the value of a symbol */
NODE *xsymvalue(NODE *args)
{
    NODE *sym,*val;

    /* get the symbol */
    sym = xlmatch(SYM,&args);
    xllastarg(args);

    /* get the global value */
    while ((val = getvalue(sym)) == s_unbound)
	xlcerror("try evaluating symbol again","unbound variable",sym);

    /* return its value */
    return (val);
}

/* xsymplist - get the property list of a symbol */
NODE *xsymplist(NODE *args)
{
    NODE *sym;

    /* get the symbol */
    sym = xlmatch(SYM,&args);
    xllastarg(args);

    /* return the property list */
    return (getplist(sym));
}

/* xget - get the value of a property */
NODE *xget(NODE *args)
{
    NODE *sym,*prp;

    /* get the symbol and property */
    sym = xlmatch(SYM,&args);
    prp = xlmatch(SYM,&args);
    xllastarg(args);

    /* retrieve the property value */
    return (xlgetprop(sym,prp));
}

/* xputprop - set the value of a property */
NODE *xputprop(NODE *args)
{
    NODE *sym,*val,*prp;

    /* get the symbol and property */
    sym = xlmatch(SYM,&args);
    val = xlarg(&args);
    prp = xlmatch(SYM,&args);
    xllastarg(args);

    /* set the property value */
    xlputprop(sym,val,prp);

    /* return the value */
    return (val);
}

/* xremprop - remove a property value from a property list */
NODE *xremprop(NODE *args)
{
    NODE *sym,*prp;

    /* get the symbol and property */
    sym = xlmatch(SYM,&args);
    prp = xlmatch(SYM,&args);
    xllastarg(args);

    /* remove the property */
    xlremprop(sym,prp);

    /* return nil */
    return (NIL);
}

/* xhash - compute the hash value of a string or symbol */
NODE *xhash(NODE *args)
{
    char *str = 0; //BUG: uninitialized variable is used if xlerror returns
    NODE *val;
    int len;

    /* get the string and the table length */
    val = xlarg(&args);
    len = (int)getfixnum(xlmatch(INT,&args));
    xllastarg(args);

    /* get the string */
    if (symbolp(val))
	str = getstring(getpname(val));
    else if (stringp(val))
	str = getstring(val);
    else
	xlerror("bad argument type",val);

    /* return the hash index */
    return (cvfixnum((FIXNUM)hash(str,len)));
}

/* xaref - array reference function */
NODE *xaref(NODE *args)
{
    NODE *array,*index;
    int i;

    /* get the array and the index */
    array = xlmatch(VECT,&args);
    index = xlmatch(INT,&args); i = (int)getfixnum(index);
    xllastarg(args);

    /* range check the index */
    if (i < 0 || i >= getsize(array))
	xlerror("array index out of bounds",index);

    /* return the array element */
    return (getelement(array,i));
}

/* xmkarray - make a new array */
NODE *xmkarray(NODE *args)
{
    int size;

    /* get the size of the array */
    size = (int)getfixnum(xlmatch(INT,&args));
    xllastarg(args);

    /* create the array */
    return (newvector(size));
}


/* xleval - xlisp evaluator */
/*	Copyright (c) 1985, by David Michael Betz
	All Rights Reserved
	Permission is granted for unrestricted non-commercial use	*/
/* modified for SPEC by Michael Paton 1994 and Alexander Carlton 1995 */

#include <math.h>
#include <stdarg.h>
#include "xlisp.h"
#include "proto.h"
#include "xtrnl.h"

/* xleval - evaluate an xlisp expression (checking for *evalhook*) */
NODE *xleval(NODE *expr)
{
    /* check for control codes */
    if (--xlsample <= 0) {
	xlsample = SAMPLE;
	oscheck();
    }

    /* check for *evalhook* */
    if (getvalue(s_evalhook))
	return (evalhook(expr));

    /* add trace entry */
    if (++xltrace < TDEPTH)
	trace_stack[xltrace] = expr;

    /* check type of value */
    if (consp(expr))
	expr = evform(expr);
    else if (symbolp(expr))
	expr = xlgetvalue(expr);

    /* remove trace entry */
    --xltrace;

    /* return the value */
    return (expr);
}

/* xlxeval - evaluate an xlisp expression (bypassing *evalhook*) */
NODE *xlxeval(NODE *expr)
{
    /* check type of value */
    if (consp(expr))
	expr = evform(expr);
    else if (symbolp(expr))
	expr = xlgetvalue(expr);

    /* return the value */
    return (expr);
}

/* xlapply - apply a function to a list of arguments */
NODE *xlapply(NODE *fun,NODE *args)
{
    NODE *env,*val;
    val = 0; //BUG: uninitialized variable is used if xlfail returns

    /* check for a null function */
    if (fun == NIL)
	xlfail("bad function");

    /* evaluate the function */
    if (subrp(fun))
	val = (*getsubr(fun))(args);
    else if (consp(fun)) {
	if (consp(car(fun))) {
	    env = cdr(fun);
	    fun = car(fun);
	}
	else
	    env = xlenv;
	if (car(fun) != s_lambda)
	    xlfail("bad function type");
	val = evfun(fun,args,env);
    }
    else
	xlfail("bad function");

    /* return the result value */
    return (val);
}

/* evform - evaluate a form */
LOCAL NODE *evform(NODE *expr)
{
    NODE ***oldstk,*fun __HEAPIFY,*args __HEAPIFY,*env,*val,*type;
    val = 0; //BUG: uninitialized variable is used if xlfail returns

    /* create a stack frame */
    oldstk = xlsave2(&fun,&args);

    /* get the function and the argument list */
    fun = car(expr);
    args = cdr(expr);

    /* evaluate the first expression */
    if ((fun = xleval(fun)) == NIL)
	xlfail("bad function");

    /* evaluate the function */
    if (subrp(fun) || fsubrp(fun)) {
	if (subrp(fun))
	    args = xlevlist(args);
	val = (*getsubr(fun))(args);
    }
    else if (consp(fun)) {
	if (consp(car(fun))) {
	    env = cdr(fun);
	    fun = car(fun);
	}
	else
	    env = xlenv;
	if ((type = car(fun)) == s_lambda) {
	    args = xlevlist(args);
	    val = evfun(fun,args,env);
	}
	else if (type == s_macro) {
	    args = evfun(fun,args,env);
	    val = xleval(args);
	}
	else
	    xlfail("bad function type");
    }
    else if (objectp(fun))
	val = xlsend(fun,args);
    else
	xlfail("bad function");

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result value */
    return (val);
}

/* evalhook - call the evalhook function */
LOCAL NODE *evalhook(NODE *expr)
{
    NODE ***oldstk,*ehook __HEAPIFY,*ahook __HEAPIFY,*args __HEAPIFY,*val;

    /* create a new stack frame */
    oldstk = xlsave3(&ehook,&ahook,&args);

    /* make an argument list */
    args = consa(expr);
    rplacd(args,consa(xlenv));

    /* rebind the hook functions to nil */
    ehook = getvalue(s_evalhook);
    setvalue(s_evalhook,NIL);
    ahook = getvalue(s_applyhook);
    setvalue(s_applyhook,NIL);

    /* call the hook function */
    val = xlapply(ehook,args);

    /* unbind the symbols */
    setvalue(s_evalhook,ehook);
    setvalue(s_applyhook,ahook);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the value */
    return (val);
}

/* xlevlist - evaluate a list of arguments */
NODE *xlevlist(NODE *args)
{
    NODE ***oldstk,*src __HEAPIFY,*dst __HEAPIFY,*new,*val;
    NODE *last = NIL;

    /* create a stack frame */
    oldstk = xlsave2(&src,&dst);

    /* initialize */
    src = args;

    /* evaluate each argument */
    for (val = NIL; src; src = cdr(src)) {

	/* check this entry */
	if (!consp(src))
	    xlfail("bad argument list");

	/* allocate a new list entry */
	new = consa(NIL);
	if (val)
	    rplacd(last,new);
	else
	    val = dst = new;
	rplaca(new,xleval(car(src)));
	last = new;
    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the new list */
    return (val);
}

/* xlunbound - signal an unbound variable error */
void xlunbound(NODE *sym)
{
    xlcerror("try evaluating symbol again","unbound variable",sym);
}

/* evfun - evaluate a function */
LOCAL NODE *evfun(NODE *fun,NODE *args,NODE *env)
{
    NODE ***oldstk,*oldenv __HEAPIFY,*newenv __HEAPIFY,*cptr __HEAPIFY,*fargs,*val;
    val = 0; //BUG: uninitialized variable is used if xlfail returns

    /* create a stack frame */
    oldstk = xlsave3(&oldenv,&newenv,&cptr);

    /* skip the function type */
    if ((fun = cdr(fun)) == NIL || !consp(fun))
	xlfail("bad function definition");

    /* get the formal argument list */
    if ((fargs = car(fun)) && !consp(fargs))
	xlfail("bad formal argument list");

    /* create a new environment frame */
    newenv = xlframe(env);
    oldenv = xlenv;

    /* bind the formal parameters */
    xlabind(fargs,args,newenv);
    xlenv = newenv;

    /* execute the code */
    for (cptr = cdr(fun); cptr; )
	val = xlevarg(&cptr);

    /* restore the environment */
    xlenv = oldenv;

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result value */
    return (val);
}

/* xlabind - bind the arguments for a function */
void xlabind(NODE *fargs,NODE *aargs,NODE *env)
{
    NODE *arg;

    /* evaluate and bind each required argument */
    while (consp(fargs) && !iskeyword(arg = car(fargs)) && consp(aargs)) {

	/* bind the formal variable to the argument value */
	xlbind(arg,car(aargs),env);

	/* move the argument list pointers ahead */
	fargs = cdr(fargs);
	aargs = cdr(aargs);
    }

    /* check for the '&optional' keyword */
    if (consp(fargs) && car(fargs) == k_optional) {
	fargs = cdr(fargs);

	/* bind the arguments that were supplied */
	while (consp(fargs) && !iskeyword(arg = car(fargs)) && consp(aargs)) {

	    /* bind the formal variable to the argument value */
	    xlbind(arg,car(aargs),env);

	    /* move the argument list pointers ahead */
	    fargs = cdr(fargs);
	    aargs = cdr(aargs);
	}

	/* bind the rest to nil */
	while (consp(fargs) && !iskeyword(arg = car(fargs))) {

	    /* bind the formal variable to nil */
	    xlbind(arg,NIL,env);

	    /* move the argument list pointer ahead */
	    fargs = cdr(fargs);
	}
    }

    /* check for the '&rest' keyword */
    if (consp(fargs) && car(fargs) == k_rest) {
	fargs = cdr(fargs);
	if (consp(fargs) && (arg = car(fargs)) && !iskeyword(arg))
	    xlbind(arg,aargs,env);
	else
	    xlfail("symbol missing after &rest");
	fargs = cdr(fargs);
	aargs = NIL;
    }

    /* check for the '&aux' keyword */
    if (consp(fargs) && car(fargs) == k_aux)
	while ((fargs = cdr(fargs)) != NIL && consp(fargs))
	    xlbind(car(fargs),NIL,env);

    /* make sure the correct number of arguments were supplied */
    if (fargs != aargs)
	xlfail(fargs ? "too few arguments" : "too many arguments");
}

/* iskeyword - check to see if a symbol is a keyword */
LOCAL int iskeyword(NODE *sym)
{
    return (sym == k_optional || sym == k_rest || sym == k_aux);
}


#if 0    // old; was trying to make varargs work
  #ifdef _GNUCC
    // sm: this is the definition that appears in my stdarg.h, at
    //     /usr/lib/gcc-lib/i386-slackware-linux/egcs-2.91.66/include/stdarg.h
    // by #defining it here again, I assure myself I've got the right definition
    // (because otherwise the preprocessor would complain)
    #define va_arg(AP, TYPE)						\
     (AP = (__gnuc_va_list) ((char *) (AP) + __va_rounded_size (TYPE)),	\
      *((TYPE *) (void *) ((char *) (AP) - __va_rounded_size (TYPE))))

    // now redefine it, adding a parameter whose size will be taken
    #define scott_va_arg(AP, TYPE, EXPR_FOR_SIZE)                                   \
     (AP = (__gnuc_va_list) ((char *) (AP) + __va_rounded_size (EXPR_FOR_SIZE)),    \
      *((TYPE *) (void *) ((char *) (AP) - __va_rounded_size (EXPR_FOR_SIZE))))

  #else
    // throw away the 3rd arg
    #define scott_va_arg(AP, TYPE, EXPR_FOR_SIZE) va_arg(AP, TYPE)
  #endif

  /* xlsave - save nodes on the stack */
  NODE ***xlsave(NODE **nptr,...)
  {        
      va_list	pvar;
      NODE ***oldstk;
      oldstk = xlstack;
      va_start(pvar,nptr);
      //sm: for (; nptr != (NODE **) NULL; nptr = va_arg(pvar, NODE **)) {
      for (; nptr != (NODE **) NULL; nptr = scott_va_arg(pvar, NODE **, nptr)) {
          if (xlstack <= xlstkbase)
              xlabort("evaluation stack overflow");
          *--xlstack = nptr;
          *nptr = NIL;
      }
      va_end(pvar);
      return (oldstk);
  }
#endif // 0

#define PUSHPTR(nptr)                          \
    if ((int)xlstack <= (int)xlstkbase)        \
        xlabort("evaluation stack overflow");  \
    *--xlstack = nptr;                         \
    *nptr = NIL;

NODE ***xlsave1(NODE **nptr1)
{
    NODE ***oldstk = xlstack;
    PUSHPTR(nptr1);
    return (oldstk);
}

NODE ***xlsave2(NODE **nptr1, NODE **nptr2)
{
    NODE ***oldstk = xlstack;
    PUSHPTR(nptr1);
    PUSHPTR(nptr2);
    return (oldstk);
}

NODE ***xlsave3(NODE **nptr1, NODE **nptr2, NODE **nptr3)
{
    NODE ***oldstk = xlstack;
    PUSHPTR(nptr1);
    PUSHPTR(nptr2);
    PUSHPTR(nptr3);
    return (oldstk);
}

NODE ***xlsave4(NODE **nptr1, NODE **nptr2, NODE **nptr3,
                NODE **nptr4)
{
    NODE ***oldstk = xlstack;
    PUSHPTR(nptr1);
    PUSHPTR(nptr2);
    PUSHPTR(nptr3);
    PUSHPTR(nptr4);
    return (oldstk);
}

NODE ***xlsave5(NODE **nptr1, NODE **nptr2, NODE **nptr3,
                NODE **nptr4, NODE **nptr5)
{
    NODE ***oldstk = xlstack;
    PUSHPTR(nptr1);
    PUSHPTR(nptr2);
    PUSHPTR(nptr3);
    PUSHPTR(nptr4);
    PUSHPTR(nptr5);
    return (oldstk);
}

NODE ***xlsave6(NODE **nptr1, NODE **nptr2, NODE **nptr3,
                NODE **nptr4, NODE **nptr5, NODE **nptr6)
{
    NODE ***oldstk = xlstack;
    PUSHPTR(nptr1);
    PUSHPTR(nptr2);
    PUSHPTR(nptr3);
    PUSHPTR(nptr4);
    PUSHPTR(nptr5);
    PUSHPTR(nptr6);
    return (oldstk);
}

NODE ***xlsave7(NODE **nptr1, NODE **nptr2, NODE **nptr3,
                NODE **nptr4, NODE **nptr5, NODE **nptr6,
                NODE **nptr7)
{
    NODE ***oldstk = xlstack;
    PUSHPTR(nptr1);
    PUSHPTR(nptr2);
    PUSHPTR(nptr3);
    PUSHPTR(nptr4);
    PUSHPTR(nptr5);
    PUSHPTR(nptr6);
    PUSHPTR(nptr7);
    return (oldstk);
}

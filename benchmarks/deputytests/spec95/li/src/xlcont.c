/* xlcont - xlisp control built-in functions */
/*	Copyright (c) 1985, by David Michael Betz
	All Rights Reserved
	Permission is granted for unrestricted non-commercial use	*/
/* modified for SPEC by Michael Paton 1994 and Alexander Carlton 1995 */

#include <math.h>
#include <stdarg.h>
#include "xlisp.h"
#include "proto.h"
#include "xtrnl.h"

/* xcond - built-in function 'cond' */
NODE *xcond(NODE *args)
{
    NODE ***oldstk,*arg __HEAPIFY,*list __HEAPIFY,*val;

    /* create a new stack frame */
    oldstk = xlsave2(&arg,&list);

    /* initialize */
    arg = args;

    /* initialize the return value */
    val = NIL;

    /* find a predicate that is true */
    while (arg) {

	/* get the next conditional */
	list = xlmatch(LIST,&arg);

	/* evaluate the predicate part */
	if (val = xlevarg(&list)) {

	    /* evaluate each expression */
	    while (list)
		val = xlevarg(&list);

	    /* exit the loop */
	    break;
	}
    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the value */
    return (val);
}

/* xcase - built-in function 'case' */
NODE *xcase(NODE *args)
{
    NODE ***oldstk,*key __HEAPIFY,*arg __HEAPIFY,*clause __HEAPIFY,*list,*val;

    /* create a new stack frame */
    oldstk = xlsave3(&key,&arg,&clause);

    /* initialize */
    arg = args;

    /* get the key expression */
    key = xlevarg(&arg);

    /* initialize the return value */
    val = NIL;

    /* find a case that matches */
    while (arg) {

	/* get the next case clause */
	clause = xlmatch(LIST,&arg);

	/* compare the key list against the key */
	if ((list = xlarg(&clause)) == true ||
            (listp(list) && keypresent(key,list)) ||
            eql(key,list)) {

	    /* evaluate each expression */
	    while (clause)
		val = xlevarg(&clause);

	    /* exit the loop */
	    break;
	}
    }

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the value */
    return (val);
}

/* keypresent - check for the presence of a key in a list */
LOCAL int keypresent(NODE *key,NODE *list)
{
    for (; consp(list); list = cdr(list))
	if (eql(car(list),key))
	    return (TRUE);
    return (FALSE);
}

/* xand - built-in function 'and' */
NODE *xand(NODE *args)
{
    NODE ***oldstk,*arg __HEAPIFY,*val;

    /* create a new stack frame */
    oldstk = xlsave1(&arg);

    /* initialize */
    arg = args;
    val = true;

    /* evaluate each argument */
    while (arg)

	/* get the next argument */
	if ((val = xlevarg(&arg)) == NIL)
	    break;

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result value */
    return (val);
}

/* xor - built-in function 'or' */
NODE *xor(NODE *args)
{
    NODE ***oldstk,*arg __HEAPIFY,*val;

    /* create a new stack frame */
    oldstk = xlsave1(&arg);

    /* initialize */
    arg = args;
    val = NIL;

    /* evaluate each argument */
    while (arg)
	if ((val = xlevarg(&arg)))
	    break;

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result value */
    return (val);
}

/* xif - built-in function 'if' */
NODE *xif(NODE *args)
{
    NODE ***oldstk,*testexpr __HEAPIFY,*thenexpr __HEAPIFY,*elseexpr __HEAPIFY,*val;

    /* create a new stack frame */
    oldstk = xlsave3(&testexpr,&thenexpr,&elseexpr);

    /* get the test expression, then clause and else clause */
    testexpr = xlarg(&args);
    thenexpr = xlarg(&args);
    elseexpr = (args ? xlarg(&args) : NIL);
    xllastarg(args);

    /* evaluate the appropriate clause */
    val = xleval(xleval(testexpr) ? thenexpr : elseexpr);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the last value */
    return (val);
}

/* xlet - built-in function 'let' */
NODE *xlet(NODE *args)
{
    return (let(args,TRUE));
}

/* xletstar - built-in function 'let*' */
NODE *xletstar(NODE *args)
{
    return (let(args,FALSE));
}

/* let - common let routine */
LOCAL NODE *let(NODE *args, int pflag)
{
    NODE ***oldstk,*newenv __HEAPIFY,*arg __HEAPIFY,*val;

    /* create a new stack frame */
    oldstk = xlsave2(&newenv,&arg);

    /* initialize */
    arg = args;

    /* create a new environment frame */
    newenv = xlframe(xlenv);

    /* get the list of bindings and bind the symbols */
    if (!pflag) xlenv = newenv;
    dobindings(xlmatch(LIST,&arg),newenv);
    if (pflag) xlenv = newenv;

    /* execute the code */
    for (val = NIL; arg; )
	val = xlevarg(&arg);

    /* unbind the arguments */
    xlenv = cdr(xlenv);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (val);
}

/* xprog - built-in function 'prog' */
NODE *xprog(NODE *args)
{
    return (prog(args,TRUE));
}

/* xprogstar - built-in function 'prog*' */
NODE *xprogstar(NODE *args)
{
    return (prog(args,FALSE));
}

/* prog - common prog routine */
LOCAL NODE *prog(NODE *args, int pflag)
{
    NODE ***oldstk,*newenv __HEAPIFY,*arg __HEAPIFY,*val;

    /* create a new stack frame */
    oldstk = xlsave2(&newenv,&arg);

    /* initialize */
    arg = args;

    /* create a new environment frame */
    newenv = xlframe(xlenv);

    /* get the list of bindings and bind the symbols */
    if (!pflag) xlenv = newenv;
    dobindings(xlmatch(LIST,&arg),newenv);
    if (pflag) xlenv = newenv;

    /* execute the code */
    tagblock(arg,&val);

    /* unbind the arguments */
    xlenv = cdr(xlenv);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (val);
}

/* xgo - built-in function 'go' */
NODE *xgo(NODE *args)
{
    NODE *label;

    /* get the target label */
    label = xlarg(&args);
    xllastarg(args);

    /* transfer to the label */
    xlgo(label);

    /* return nil */
    return (NIL);
}

/* xreturn - built-in function 'return' */
NODE *xreturn(NODE *args)
{
    NODE *val;

    /* get the return value */
    val = (args ? xlarg(&args) : NIL);
    xllastarg(args);

    /* return from the inner most block */
    xlreturn(val);

    /* return nil */
    return (NIL);
}

/* xprog1 - built-in function 'prog1' */
NODE *xprog1(NODE *args)
{
    return (progx(args,1));
}

/* xprog2 - built-in function 'prog2' */
NODE *xprog2(NODE *args)
{
    return (progx(args,2));
}

/* progx - common progx code */
LOCAL NODE *progx(NODE *args, int n)
{
    NODE ***oldstk,*arg __HEAPIFY,*val __HEAPIFY;

    /* create a new stack frame */
    oldstk = xlsave2(&arg,&val);

    /* initialize */
    arg = args;

    /* evaluate the first n expressions */
    while (n--)
	val = xlevarg(&arg);

    /* evaluate each remaining argument */
    while (arg)
	xlevarg(&arg);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the last test expression value */
    return (val);
}

/* xprogn - built-in function 'progn' */
NODE *xprogn(NODE *args)
{
    NODE ***oldstk,*arg __HEAPIFY,*val;

    /* create a new stack frame */
    oldstk = xlsave1(&arg);

    /* initialize */
    arg = args;

    /* evaluate each remaining argument */
    for (val = NIL; arg; )
	val = xlevarg(&arg);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the last test expression value */
    return (val);
}

/* xdo - built-in function 'do' */
NODE *xdo(NODE *args)
{
    return (doloop(args,TRUE));
}

/* xdostar - built-in function 'do*' */
NODE *xdostar(NODE *args)
{
    return (doloop(args,FALSE));
}

/* doloop - common do routine */
LOCAL NODE *doloop(NODE *args, int pflag)
{
    NODE ***oldstk,*newenv __HEAPIFY,*arg __HEAPIFY,*blist __HEAPIFY,*clist __HEAPIFY,*test __HEAPIFY,*rval;
    int rbreak;

    /* create a new stack frame */
    oldstk = xlsave5(&newenv,&arg,&blist,&clist,&test);

    /* initialize */
    arg = args;

    /* get the list of bindings */
    blist = xlmatch(LIST,&arg);

    /* create a new environment frame */
    newenv = xlframe(xlenv);

    /* bind the symbols */
    if (!pflag) xlenv = newenv;
    dobindings(blist,newenv);
    if (pflag) xlenv = newenv;

    /* get the exit test and result forms */
    clist = xlmatch(LIST,&arg);
    test = xlarg(&clist);

    /* execute the loop as long as the test is false */
    rbreak = FALSE;
    while (xleval(test) == NIL) {

	/* execute the body of the loop */
	if (tagblock(arg,&rval)) {
	    rbreak = TRUE;
	    break;
	}

	/* update the looping variables */
	doupdates(blist,pflag);
    }

    /* evaluate the result expression */
    if (!rbreak)
	for (rval = NIL; consp(clist); )
	    rval = xlevarg(&clist);

    /* unbind the arguments */
    xlenv = cdr(xlenv);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (rval);
}

/* xdolist - built-in function 'dolist' */
NODE *xdolist(NODE *args)
{
    NODE ***oldstk,*arg __HEAPIFY,*clist __HEAPIFY,*sym __HEAPIFY,*list __HEAPIFY,*val __HEAPIFY,*rval;
    int rbreak;

    /* create a new stack frame */
    oldstk = xlsave5(&arg,&clist,&sym,&list,&val);

    /* initialize */
    arg = args;

    /* get the control list (sym list result-expr) */
    clist = xlmatch(LIST,&arg);
    sym = xlmatch(SYM,&clist);
    list = xlevmatch(LIST,&clist);
    val = (clist ? xlarg(&clist) : NIL);

    /* initialize the local environment */
    xlenv = xlframe(xlenv);
    xlbind(sym,NIL,xlenv);

    /* loop through the list */
    rbreak = FALSE;
    for (; consp(list); list = cdr(list)) {

	/* bind the symbol to the next list element */
	xlsetvalue(sym,car(list));

	/* execute the loop body */
	if (tagblock(arg,&rval)) {
	    rbreak = TRUE;
	    break;
	}
    }

    /* evaluate the result expression */
    if (!rbreak) {
	xlsetvalue(sym,NIL);
	rval = xleval(val);
    }

    /* unbind the arguments */
    xlenv = cdr(xlenv);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (rval);
}

/* xdotimes - built-in function 'dotimes' */
NODE *xdotimes(NODE *args)
{
    NODE ***oldstk,*arg __HEAPIFY,*clist __HEAPIFY,*sym __HEAPIFY,*val __HEAPIFY,*rval;
    int rbreak,cnt,i;

    /* create a new stack frame */
    oldstk = xlsave4(&arg,&clist,&sym,&val);

    /* initialize */
    arg = args;

    /* get the control list (sym list result-expr) */
    clist = xlmatch(LIST,&arg);
    sym = xlmatch(SYM,&clist);
    cnt = getfixnum(xlevmatch(INT,&clist));
    val = (clist ? xlarg(&clist) : NIL);

    /* initialize the local environment */
    xlenv = xlframe(xlenv);
    xlbind(sym,NIL,xlenv);

    /* loop through for each value from zero to cnt-1 */
    rbreak = FALSE;
    for (i = 0; i < cnt; i++) {

	/* bind the symbol to the next list element */
	xlsetvalue(sym,cvfixnum((FIXNUM)i));

	/* execute the loop body */
	if (tagblock(arg,&rval)) {
	    rbreak = TRUE;
	    break;
	}
    }

    /* evaluate the result expression */
    if (!rbreak) {
	xlsetvalue(sym,cvfixnum((FIXNUM)cnt));
	rval = xleval(val);
    }

    /* unbind the arguments */
    xlenv = cdr(xlenv);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (rval);
}

/* xcatch - built-in function 'catch' */
NODE *xcatch(NODE *args)
{
    NODE ***oldstk,*tag __HEAPIFY,*arg __HEAPIFY,*val;
    CONTEXT cntxt __HEAPIFY;

    /* create a new stack frame */
    oldstk = xlsave2(&tag,&arg);

    /* initialize */
    tag = xlevarg(&args);
    arg = args;
    val = NIL;

    /* establish an execution context */
    xlbegin(&cntxt,CF_THROW,tag);

    /* check for 'throw' */
    if (setjmp(cntxt.c_jmpbuf))
	val = xlvalue;

    /* otherwise, evaluate the remainder of the arguments */
    else {
	while (arg)
	    val = xlevarg(&arg);
    }
    xlend(&cntxt);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (val);
}

/* xthrow - built-in function 'throw' */
NODE *xthrow(NODE *args)
{
    NODE *tag,*val;

    /* get the tag and value */
    tag = xlarg(&args);
    val = (args ? xlarg(&args) : NIL);
    xllastarg(args);

    /* throw the tag */
    xlthrow(tag,val);

    /* return nil */
    return (NIL);
}

/* xerror - built-in function 'error' */
NODE *xerror(NODE *args)
{
    char *emsg; NODE *arg;

    /* get the error message and the argument */
    emsg = getstring(xlmatch(STR,&args));
    arg = (args ? xlarg(&args) : s_unbound);
    xllastarg(args);

    /* signal the error */
    xlerror(emsg,arg);

    /* return nil */
    return (NIL);
}

/* xcerror - built-in function 'cerror' */
NODE *xcerror(NODE *args)
{
    char *cmsg,*emsg; NODE *arg;

    /* get the correction message, the error message, and the argument */
    cmsg = getstring(xlmatch(STR,&args));
    emsg = getstring(xlmatch(STR,&args));
    arg = (args ? xlarg(&args) : s_unbound);
    xllastarg(args);

    /* signal the error */
    xlcerror(cmsg,emsg,arg);

    /* return nil */
    return (NIL);
}

/* xbreak - built-in function 'break' */
NODE *xbreak(NODE *args)
{
    char *emsg; NODE *arg;

    /* get the error message */
    emsg = (args ? getstring(xlmatch(STR,&args)) : "**BREAK**");
    arg = (args ? xlarg(&args) : s_unbound);
    xllastarg(args);

    /* enter the break loop */
    xlbreak(emsg,arg);

    /* return nil */
    return (NIL);
}

/* xcleanup - built-in function 'clean-up' */
NODE *xcleanup(NODE *args)
{
    xllastarg(args);
    xlcleanup();

    /* return nil */
    return (NIL);
}

/* xcontinue - built-in function 'continue' */
NODE *xcontinue(NODE *args)
{
    xllastarg(args);
    xlcontinue();

    /* return nil */
    return (NIL);
}

/* xerrset - built-in function 'errset' */
NODE *xerrset(NODE *args)
{
    NODE ***oldstk,*expr __HEAPIFY,*flag __HEAPIFY,*val;
    CONTEXT cntxt __HEAPIFY;

    /* create a new stack frame */
    oldstk = xlsave2(&expr,&flag);

    /* get the expression and the print flag */
    expr = xlarg(&args);
    flag = (args ? xlarg(&args) : true);
    xllastarg(args);

    /* establish an execution context */
    xlbegin(&cntxt,CF_ERROR,flag);

    /* check for error */
    if (setjmp(cntxt.c_jmpbuf))
	val = NIL;

    /* otherwise, evaluate the expression */
    else {
	expr = xleval(expr);
	val = consa(expr);
    }
    xlend(&cntxt);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (val);
}

/* xevalhook - eval hook function */
NODE *xevalhook(NODE *args)
{
    NODE ***oldstk,*expr __HEAPIFY,*ehook __HEAPIFY,*ahook __HEAPIFY,*env __HEAPIFY,*newehook __HEAPIFY,*newahook __HEAPIFY,*newenv __HEAPIFY,*val;

    /* create a new stack frame */
    oldstk = xlsave7(&expr,&ehook,&ahook,&env,&newehook,&newahook,&newenv);

    /* get the expression, the new hook functions and the environment */
    expr = xlarg(&args);
    newehook = xlarg(&args);
    newahook = xlarg(&args);
    newenv = (args ? xlarg(&args) : xlenv);
    xllastarg(args);

    /* bind *evalhook* and *applyhook* to the hook functions */
    ehook = getvalue(s_evalhook);
    setvalue(s_evalhook,newehook);
    ahook = getvalue(s_applyhook);
    setvalue(s_applyhook,newahook);
    env = xlenv;
    xlenv = newenv;

    /* evaluate the expression (bypassing *evalhook*) */
    val = xlxeval(expr);

    /* unbind the hook variables */
    setvalue(s_evalhook,ehook);
    setvalue(s_applyhook,ahook);
    xlenv = env;

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return the result */
    return (val);
}

/* dobindings - handle bindings for let/let*, prog/prog*, do/do* */
LOCAL void /*matth: added return type */
dobindings(NODE *blist,NODE *env)
{
    NODE ***oldstk,*list __HEAPIFY,*bnd __HEAPIFY,*sym __HEAPIFY,*val __HEAPIFY;

    /* create a new stack frame */
    oldstk = xlsave4(&list,&bnd,&sym,&val);

    /* bind each symbol in the list of bindings */
    for (list = blist; consp(list); list = cdr(list)) {

	/* get the next binding */
	bnd = car(list);

	/* handle a symbol */
	if (symbolp(bnd)) {
	    sym = bnd;
	    val = NIL;
	}

	/* handle a list of the form (symbol expr) */
	else if (consp(bnd)) {
	    sym = xlmatch(SYM,&bnd);
	    val = xlevarg(&bnd);
	}
	else
	    xlfail("bad binding");

	/* bind the value to the symbol */
	xlbind(sym,val,env);
    }

    /* restore the previous stack frame */
    xlstack = oldstk;
}

/* doupdates - handle updates for do/do* */
void doupdates(NODE *blist, int pflag)
{
    NODE ***oldstk,*plist __HEAPIFY,*list __HEAPIFY,*bnd __HEAPIFY,*sym __HEAPIFY,*val __HEAPIFY;

    /* create a new stack frame */
    oldstk = xlsave5(&plist,&list,&bnd,&sym,&val);

    /* bind each symbol in the list of bindings */
    for (list = blist; consp(list); list = cdr(list)) {

	/* get the next binding */
	bnd = car(list);

	/* handle a list of the form (symbol expr) */
	if (consp(bnd)) {
	    sym = xlmatch(SYM,&bnd);
	    bnd = cdr(bnd);
	    if (bnd) {
		val = xlevarg(&bnd);
		if (pflag) {
		    plist = consd(plist);
		    rplaca(plist,cons(sym,val));
		}
		else
		    xlsetvalue(sym,val);
	    }
	}
    }

    /* set the values for parallel updates */
    for (; plist; plist = cdr(plist))
	xlsetvalue(car(car(plist)),cdr(car(plist)));

    /* restore the previous stack frame */
    xlstack = oldstk;
}

/* tagblock - execute code within a block and tagbody */
int tagblock(NODE *code,NODE **pval)
{
    NODE ***oldstk,*arg __HEAPIFY;
    CONTEXT cntxt __HEAPIFY;
    int type,sts;

    /* create a new stack frame */
    oldstk = xlsave1(&arg);

    /* initialize */
    arg = code;

    /* establish an execution context */
    xlbegin(&cntxt,CF_GO|CF_RETURN,arg);

    /* check for a 'return' */
    if ((type = setjmp(cntxt.c_jmpbuf)) == CF_RETURN) {
	*pval = xlvalue;
	sts = TRUE;
    }

    /* otherwise, enter the body */
    else {
	/* check for a 'go' */
	if (type == CF_GO)
	    arg = xlvalue;

	/* evaluate each expression in the body */
	while (consp(arg))
	    if (consp(car(arg)))
		xlevarg(&arg);
	    else
		arg = cdr(arg);

	/* fell out the bottom of the loop */
	*pval = NIL;
	sts = FALSE;
    }
    xlend(&cntxt);

    /* restore the previous stack frame */
    xlstack = oldstk;

    /* return status */
    return (sts);
}


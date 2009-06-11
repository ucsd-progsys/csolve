/* xlmath - xlisp builtin arithmetic functions */
/*	Copyright (c) 1985, by David Michael Betz
	All Rights Reserved
	Permission is granted for unrestricted non-commercial use	*/
/* modified for SPEC by Michael Paton 1994 and Alexander Carlton 1995 */

#include <math.h>
#include <stdarg.h>
#include "xlisp.h"
#include "proto.h"
#include "xtrnl.h"

/* xadd - builtin function for addition */
NODE *xadd(NODE *args)
{
    return (binary(args,'+'));
}

/* xsub - builtin function for subtraction */
NODE *xsub(NODE *args)
{
    return (binary(args,'-'));
}

/* xmul - builtin function for multiplication */
NODE *xmul(NODE *args)
{
    return (binary(args,'*'));
}

/* xdiv - builtin function for division */
NODE *xdiv(NODE *args)
{
    return (binary(args,'/'));
}

/* xrem - builtin function for remainder */
NODE *xrem(NODE *args)
{
    return (binary(args,'%'));
}

/* xmin - builtin function for minimum */
NODE *xmin(NODE *args)
{
    return (binary(args,'m'));
}

/* xmax - builtin function for maximum */
NODE *xmax(NODE *args)
{
    return (binary(args,'M'));
}

/* xexpt - built-in function 'expt' */
NODE *xexpt(NODE *args)
{
    return (binary(args,'E'));
}

/* xbitand - builtin function for bitwise and */
NODE *xbitand(NODE *args)
{
    return (binary(args,'&'));
}

/* xbitior - builtin function for bitwise inclusive or */
NODE *xbitior(NODE *args)
{
    return (binary(args,'|'));
}

/* xbitxor - builtin function for bitwise exclusive or */
NODE *xbitxor(NODE *args)
{
    return (binary(args,'^'));
}

/* binary - handle binary operations */
LOCAL NODE *binary(NODE *args,int fcn)
{
    FIXNUM ival,iarg;
    FLONUM fval,farg;
    NODE *arg;
    int imode;

    /* get the first argument */
    arg = xlarg(&args);

    /* set the type of the first argument */
    if (fixp(arg)) {
	ival = getfixnum(arg);
	imode = TRUE;
    }
    else if (floatp(arg)) {
	fval = getflonum(arg);
	imode = FALSE;
    }
    else
	xlerror("bad argument type",arg);

    /* treat '-' with a single argument as a special case */
    if (fcn == '-' && args == NIL)
	if (imode)
	    ival = -ival;
	else
	    fval = -fval;

    /* handle each remaining argument */
    while (args) {

	/* get the next argument */
	arg = xlarg(&args);

	/* check its type */
	if (fixp(arg))
	    if (imode) iarg = getfixnum(arg);
	    else farg = (FLONUM)getfixnum(arg);
	else if (floatp(arg))
	    if (imode) { fval = (FLONUM)ival; farg = getflonum(arg); imode = FALSE; }
	    else farg = getflonum(arg);
	else
	    xlerror("bad argument type",arg);

	/* accumulate the result value */
	if (imode)
	    switch (fcn) {
	    case '+':	ival += iarg; break;
	    case '-':	ival -= iarg; break;
	    case '*':	ival *= iarg; break;
	    case '/':	checkizero(iarg); ival /= iarg; break;
	    case '%':	checkizero(iarg); ival %= iarg; break;
	    case 'M':	if (iarg > ival) ival = iarg; break;
	    case 'm':	if (iarg < ival) ival = iarg; break;
	    case '&':	ival &= iarg; break;
	    case '|':	ival |= iarg; break;
	    case '^':	ival ^= iarg; break;
	    default:	badiop();
	    }
	else
	    switch (fcn) {
	    case '+':	fval += farg; break;
	    case '-':	fval -= farg; break;
	    case '*':	fval *= farg; break;
	    case '/':	checkfzero(farg); fval /= farg; break;
	    case 'M':	if (farg > fval) fval = farg; break;
	    case 'm':	if (farg < fval) fval = farg; break;
	    case 'E':	fval = pow(fval,farg); break;
	    default:	badfop();
	    }
    }

    /* return the result */
    return (imode ? cvfixnum(ival) : cvflonum(fval));
}

/* checkizero - check for integer division by zero */
void checkizero(FIXNUM iarg)
{
    if (iarg == 0)
	xlfail("division by zero");
}

/* checkfzero - check for floating point division by zero */
void checkfzero(FLONUM farg)
{
    if (farg == 0.0)
	xlfail("division by zero");
}

/* checkfneg - check for square root of a negative number */
void checkfneg(FLONUM farg)
{
    if (farg < 0.0)
	xlfail("square root of a negative number");
}

/* xbitnot - bitwise not */
NODE *xbitnot(NODE *args)
{
    return (unary(args,'~'));
}

/* xabs - builtin function for absolute value */
NODE *xabs(NODE *args)
{
    return (unary(args,'A'));
}

/* xadd1 - builtin function for adding one */
NODE *xadd1(NODE *args)
{
    return (unary(args,'+'));
}

/* xsub1 - builtin function for subtracting one */
NODE *xsub1(NODE *args)
{
    return (unary(args,'-'));
}

/* xsin - built-in function 'sin' */
NODE *xsin(NODE *args)
{
    return (unary(args,'S'));
}

/* xcos - built-in function 'cos' */
NODE *xcos(NODE *args)
{
    return (unary(args,'C'));
}

/* xtan - built-in function 'tan' */
NODE *xtan(NODE *args)
{
    return (unary(args,'T'));
}

/* xexp - built-in function 'exp' */
NODE *xexp(NODE *args)
{
    return (unary(args,'E'));
}

/* xsqrt - built-in function 'sqrt' */
NODE *xsqrt(NODE *args)
{
    return (unary(args,'R'));
}

/* xfix - built-in function 'fix' */
NODE *xfix(NODE *args)
{
    return (unary(args,'I'));
}

/* xfloat - built-in function 'float' */
NODE *xfloat(NODE *args)
{
    return (unary(args,'F'));
}

/* xrand - built-in function 'random' */
NODE *xrand(NODE *args)
{
    return (unary(args,'R'));
}

/* unary - handle unary operations */
LOCAL NODE *unary(NODE *args,int fcn)
{
    FLONUM fval;
    FIXNUM ival;
    NODE *arg;

    /* get the argument */
    arg = xlarg(&args);
    xllastarg(args);

    /* check its type */
    if (fixp(arg)) {
	ival = getfixnum(arg);
	switch (fcn) {
	case '~':	ival = ~ival; break;
	case 'A':	ival = abs(ival); break;
	case '+':	ival++; break;
	case '-':	ival--; break;
	case 'I':	break;
	case 'F':	return (cvflonum((FLONUM)ival));
	case 'R':	ival = (FIXNUM)osrand((int)ival); break;
	default:	badiop();
	}
	return (cvfixnum(ival));
    }
    else if (floatp(arg)) {
	fval = getflonum(arg);
	switch (fcn) {
	case 'A':	fval = fabs(fval); break;
	case '+':	fval += 1.0; break;
	case '-':	fval -= 1.0; break;
	case 'S':	fval = sin(fval); break;
	case 'C':	fval = cos(fval); break;
	case 'T':	fval = tan(fval); break;
	case 'E':	fval = exp(fval); break;
	case 'R':	checkfneg(fval); fval = sqrt(fval); break;
	case 'I':	return (cvfixnum((FIXNUM)fval));
	case 'F':	break;
	default:	badfop();
	}
	return (cvflonum(fval));
    }
    else
	xlerror("bad argument type",arg);
    /*NOTREACHED*/
}

/* xminusp - is this number negative? */
NODE *xminusp(NODE *args)
{
    return (predicate(args,'-'));
}

/* xzerop - is this number zero? */
NODE *xzerop(NODE *args)
{
    return (predicate(args,'Z'));
}

/* xplusp - is this number positive? */
NODE *xplusp(NODE *args)
{
    return (predicate(args,'+'));
}

/* xevenp - is this number even? */
NODE *xevenp(NODE *args)
{
    return (predicate(args,'E'));
}

/* xoddp - is this number odd? */
NODE *xoddp(NODE *args)
{
    return (predicate(args,'O'));
}

/* predicate - handle a predicate function */
LOCAL NODE *predicate(NODE *args,int fcn)
{
    FLONUM fval;
    FIXNUM ival;
    NODE *arg;

    /* get the argument */
    arg = xlarg(&args);
    xllastarg(args);

    /* check the argument type */
    if (fixp(arg)) {
	ival = getfixnum(arg);
	switch (fcn) {
	case '-':	ival = (ival < 0); break;
	case 'Z':	ival = (ival == 0); break;
	case '+':	ival = (ival > 0); break;
	case 'E':	ival = ((ival & 1) == 0); break;
	case 'O':	ival = ((ival & 1) != 0); break;
	default:	badiop();
	}
    }
    else if (floatp(arg)) {
	fval = getflonum(arg);
	switch (fcn) {
	case '-':	ival = (fval < 0); break;
	case 'Z':	ival = (fval == 0); break;
	case '+':	ival = (fval > 0); break;
	default:	badfop();
	}
    }
    else
	xlerror("bad argument type",arg);

    /* return the result value */
    return (ival ? true : NIL);
}

/* xlss - builtin function for < */
NODE *xlss(NODE *args)
{
    return (compare(args,'<'));
}

/* xleq - builtin function for <= */
NODE *xleq(NODE *args)
{
    return (compare(args,'L'));
}

/* equ - builtin function for = */
NODE *xequ(NODE *args)
{
    return (compare(args,'='));
}

/* xneq - builtin function for /= */
NODE *xneq(NODE *args)
{
    return (compare(args,'#'));
}

/* xgeq - builtin function for >= */
NODE *xgeq(NODE *args)
{
    return (compare(args,'G'));
}

/* xgtr - builtin function for > */
NODE *xgtr(NODE *args)
{
    return (compare(args,'>'));
}

/* compare - common compare function */
LOCAL NODE *compare(NODE *args,int fcn)
{
    NODE *arg1,*arg2;
    FIXNUM icmp;
    FLONUM fcmp;
    int imode;

    /* get the two arguments */
    arg1 = xlarg(&args);
    arg2 = xlarg(&args);
    xllastarg(args);

    /* do the compare */
    if (stringp(arg1) && stringp(arg2)) {
	icmp = strcmp(getstring(arg1),getstring(arg2));
	imode = TRUE;
    }
    else if (fixp(arg1) && fixp(arg2)) {
	icmp = getfixnum(arg1) - getfixnum(arg2);
	imode = TRUE;
    }
    else if (floatp(arg1) && floatp(arg2)) {
	fcmp = getflonum(arg1) - getflonum(arg2);
	imode = FALSE;
    }
    else if (fixp(arg1) && floatp(arg2)) {
	fcmp = (FLONUM)getfixnum(arg1) - getflonum(arg2);
	imode = FALSE;
    }
    else if (floatp(arg1) && fixp(arg2)) {
	fcmp = getflonum(arg1) - (FLONUM)getfixnum(arg2);
	imode = FALSE;
    }
    else
	xlfail("expecting strings, integers or floats");

    /* compute result of the compare */
    if (imode)
	switch (fcn) {
	case '<':	icmp = (icmp < 0); break;
	case 'L':	icmp = (icmp <= 0); break;
	case '=':	icmp = (icmp == 0); break;
	case '#':	icmp = (icmp != 0); break;
	case 'G':	icmp = (icmp >= 0); break;
	case '>':	icmp = (icmp > 0); break;
	}
    else
	switch (fcn) {
	case '<':	icmp = (fcmp < 0.0); break;
	case 'L':	icmp = (fcmp <= 0.0); break;
	case '=':	icmp = (fcmp == 0.0); break;
	case '#':	icmp = (fcmp != 0.0); break;
	case 'G':	icmp = (fcmp >= 0.0); break;
	case '>':	icmp = (fcmp > 0.0); break;
	}

    /* return the result */
    return (icmp ? true : NIL);
}

/* badiop - bad integer operation */
LOCAL void badiop(void)
{
    xlfail("bad integer operation");
}

/* badfop - bad floating point operation */
LOCAL void badfop(void)
{
    xlfail("bad floating point operation");
}


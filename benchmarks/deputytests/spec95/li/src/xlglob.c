/* xlglobals - xlisp global variables */
/*	Copyright (c) 1985, by David Michael Betz
	All Rights Reserved
	Permission is granted for unrestricted non-commercial use	*/
/* modified for SPEC by Michael Paton 1994 and Alexander Carlton 1995 */

#include <math.h>
#include <stdarg.h>
#include "xlisp.h"
#include "proto.h"
#include "xtrnl.h"

/* symbols */
NODE *true = NIL, *s_dot = NIL;
NODE *s_quote = NIL, *s_function = NIL;
NODE *s_bquote = NIL, *s_comma = NIL, *s_comat = NIL;
NODE *s_evalhook = NIL, *s_applyhook = NIL;
NODE *s_lambda = NIL, *s_macro = NIL;
NODE *s_stdin = NIL, *s_stdout = NIL, *s_rtable = NIL;
NODE *s_tracenable = NIL, *s_tlimit = NIL, *s_breakenable = NIL;
NODE *s_car = NIL, *s_cdr = NIL, *s_nth = NIL;
NODE *s_get = NIL, *s_svalue = NIL, *s_splist = NIL, *s_aref = NIL;
NODE *s_eql = NIL, *k_test = NIL, *k_tnot = NIL;
NODE *k_wspace = NIL, *k_const = NIL, *k_nmacro = NIL, *k_tmacro = NIL;
NODE *k_optional = NIL, *k_rest = NIL, *k_aux = NIL;
NODE *a_subr = NIL, *a_fsubr = NIL;
NODE *a_list = NIL, *a_sym = NIL, *a_int = NIL, *a_float = NIL;
NODE *a_str = NIL, *a_obj = NIL, *a_fptr = NIL, *a_vect;
NODE *obarray = NIL, *s_unbound = NIL;

/* evaluation variables */
NODE ***xlstack = NULL, ***xlstkbase = NULL, ***xlstktop = NULL;
NODE *xlenv = NIL;

/* exception handling variables */
CONTEXT *xlcontext = NULL;	/* current exception handler */
NODE *xlvalue = NIL;		/* exception value */

/* debugging variables */
int xldebug = 0;		/* debug level */
int xltrace = -1;		/* trace stack pointer */
NODE **trace_stack = NULL;	/* trace stack */
int xlsample = 0;		/* control character sample rate */

/* gensym variables */
char gsprefix[STRMAX+1] = { 'G',0 };	/* gensym prefix string */
int gsnumber = 1;		/* gensym number */

/* i/o variables */
int prompt = TRUE; 		/* prompt flag */
int xlplevel = 0;		/* paren nesting level */
int xlfsize = 0;		/* flat size of current print call */

/* dynamic memory variables */
long total = 0L;		/* total memory in use */
int anodes = 0;			/* number of nodes to allocate */
int nnodes = 0;			/* number of nodes allocated */
int nsegs = 0;			/* number of segments allocated */
int nfree = 0;			/* number of nodes free */
int gccalls = 0;		/* number of gc calls */
struct segment *segs = NULL;	/* list of allocated segments */
NODE *fnodes = NIL;		/* list of free nodes */

/* object programming variables */
NODE *self = NIL, *Class = NIL, *object = NIL;
NODE *new = NIL, *isnew = NIL, *msgcls = NIL, *msgclass = NIL;

/* general purpose string buffer */
char buf[STRMAX+1] = { 0 };


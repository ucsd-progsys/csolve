/* xtrnl - xlisp external global definitions */
/*	Copyright (c) 1985, by David Michael Betz
	All Rights Reserved
	Permission is granted for unrestricted non-commercial use	*/
/* modified for SPEC by Michael Paton 1994 and Alexander Carlton 1995 */

/* symbols */
extern NODE *true, *s_dot;
extern NODE *s_quote, *s_function;
extern NODE *s_bquote, *s_comma, *s_comat;
extern NODE *s_evalhook, *s_applyhook;
extern NODE *s_lambda, *s_macro;
extern NODE *s_stdin, *s_stdout, *s_rtable;
extern NODE *s_tracenable, *s_tlimit, *s_breakenable;
extern NODE *s_car, *s_cdr, *s_nth;
extern NODE *s_get, *s_svalue, *s_splist, *s_aref;
extern NODE *s_eql, *k_test, *k_tnot;
extern NODE *k_wspace, *k_const, *k_nmacro, *k_tmacro;
extern NODE *k_optional, *k_rest, *k_aux;
extern NODE *a_subr, *a_fsubr;
extern NODE *a_list, *a_sym, *a_int, *a_float;
extern NODE *a_str, *a_obj, *a_fptr, *a_vect;
extern NODE *obarray, *s_unbound;

/* evaluation variables */
extern NODE *** COUNT(EDEPTH) ASSUMECONST xlstkbase;
// xlstktop == xlstkbase+EDEPTH.
extern NODE *** BND(xlstkbase, xlstktop) ASSUMECONST xlstktop;   // aka COUNT(-EDEPTH)
extern NODE *** BND(xlstkbase, xlstktop) xlstack;
#pragma cilnoremove("xlstkbase", "xlstktop");
extern NODE *xlenv;

/* exception handling variables */
extern CONTEXT *xlcontext;		/* current exception handler */
extern NODE *xlvalue;			/* exception value */

/* debugging variables */
extern int xldebug;			/* debug level */
extern int xltrace;			/* trace stack pointer */
extern NODE ** COUNT(TDEPTH) trace_stack;		/* trace stack */
extern int xlsample;			/* control character sample rate */

/* gensym variables */
extern char (NT gsprefix)[STRMAX+1];		/* gensym prefix string */
extern int gsnumber;			/* gensym number */

/* i/o variables */
extern int prompt; 			/* prompt flag */
extern int xlplevel;			/* paren nesting level */
extern int xlfsize;			/* flat size of current print call */

/* dynamic memory variables */
extern long total;			/* total memory in use */
extern int anodes;			/* number of nodes to allocate */
extern int nnodes;			/* number of nodes allocated */
extern int nsegs;			/* number of segments allocated */
extern int nfree;			/* number of nodes free */
extern int gccalls;			/* number of gc calls */
extern struct segment *segs;		/* list of allocated segments */
extern NODE *fnodes;			/* list of free nodes */

/* object programming variables */
extern NODE *self, *Class, *object;
extern NODE *new, *isnew, *msgcls, *msgclass;

/* general purpose string buffer */
extern char (NT buf)[STRMAX+1];

/* xlisp function table */
//matth: I'll hardcode the length here, but it could also be handled 
//as an NT array of structs.
extern struct fdef ftab[174];

#define d294 0

#include "cmump.h"
#include "multpol.h"
#include "gbas.h"
#include <stdio.h>

#define MSIZE(x) ((x)->len>0 ? (x)->len : - ((x)->len))
extern short nvars;
extern int (*cmp_exp)();
extern long arith_cost[MAX_WORKERS];
extern int nbumps[MAX_WORKERS];
extern int spy, redgcd;
extern int zero_poly_count;

#if	eager_kills
extern	LMPOL *current_pair[MAX_WORKERS][2];
extern  int kill_pair[MAX_WORKERS];
extern  int valid_pair[MAX_WORKERS];
extern  int nworkers,co_workers;
#endif	eager_kills

#if	TIME_REDUCTION

/*
 * time_ stuff is used to record intervals for reductions delays using 
 * the microsecond timer.
 */

#define	TIME_SIZE	32768

extern unsigned	*microtime;
extern int		time_log_start[TIME_SIZE];
extern int		time_log_end[TIME_SIZE];
extern int		time_who[TIME_SIZE];
extern int		time_what[TIME_SIZE];
extern int		time_index;

#define		TIME_WHAT_READY 5
#define		TIME_WHAT_START 6
#define		TIME_WHAT_SEARCH 7

#endif	TIME_REDUCTION

extern int primitive_reduction_count;
extern int dead_time[MAX_WORKERS];

#if LOCKCOUNT
extern int lockcount;
#endif

extern int state[MAX_WORKERS];
extern int deque();

FN int redfast(p,nextp,co_work_record)  MPOL *p; 
LMPOL *nextp;
struct co_record *co_work_record;
{

	MPOL *temp;
	LMPOL *pmpol;
	short *scratch;

#if	TIME_REDUCTION_2
	int temp_time,time_index_t;
#endif	TIME_REDUCTION_2

#if d294
	if (spy>=4) {
		mutex_lock(write_stdout);
		printf("Worker %d : redfast (p=0x%x,co_work_record=0x%x)\n",
		    *(int *)cthread_data(cthread_self()),p,co_work_record);
		mutex_unlock(write_stdout);
		fflush(stdout);
	}
#endif d294

#if	TIME_REDUCTION_2
	time_index_t = time_index++;
	time_log_start[ time_index_t ] = *microtime;
	time_log_end[ time_index_t ] = -1;
	time_who[ time_index_t ] = *((int *)cthread_data(cthread_self()));
	time_what[ time_index_t ] = TIME_WHAT_READY;
#endif	TIME_REDUCTION_2

	while ( (p->nterms == 0) && (p->done == 0));
	if (p->nterms == 0) {
		mpolfree(p);
		/* deteced a zero polynomial */
		zero_poly_count++; /* unprotected shared variable */
		return ( 1 );
	}

#if	TIME_REDUCTION_2
	temp_time = *microtime;
#endif	TIME_REDUCTION_2

	pmpol=nextp;
	while ((pmpol!=NULL)&&
	    (!expodiv(MEXPO(pmpol->pol,0),MEXPO(p,0))||
	    (pmpol->pol->done==0)) ) {
		pmpol=pmpol->next;
	}

#if	TIME_REDUCTION_2
	time_index_t = time_index++;
	time_log_start[ time_index_t ] = temp_time;
	time_log_end[ time_index_t ] = *microtime;
	time_who[ time_index_t ] = *((int *)cthread_data(cthread_self()));
	time_what[ time_index_t ] = TIME_WHAT_SEARCH;
#endif	TIME_REDUCTION_2

#if	eager_kills
	if (kill_pair[ ME % nworkers ] == 1) {
		mpolfree(p);
		return (1);
	}
#endif	eager_kills

	if (pmpol==NULL) {
#if 0
		while ( p->done == 0 ) {
#if dead_time
			dead_time[ *(int *)cthread_data(cthread_self()) ] ++;
#endif
		}
#endif
		/* produced a new polynomial, clear count */
		zero_poly_count = 0;
#if d294
		finish_and_add( p );
#endif d294
		return ( 1 );
	}

	if ((scratch = (short *)galloc(4*(nvars+1)))==0) valerr();

	temp = (MPOL *) galloc( sizeof(MPOL) );
	temp->nterms = 0;
	temp->done = 0;

#if	TIME_REDUCTION_2
	time_index_t = time_index++;
	time_log_start[ time_index_t ] = *microtime;
	time_log_end[ time_index_t ] = -1;
	time_who[ time_index_t ] = *((int *)cthread_data(cthread_self()));
	time_what[ time_index_t ] = TIME_WHAT_START;
#endif	TIME_REDUCTION_2

#if d294
	enque( temp, nextp, co_work_record );
#endif d294
	redonefast(p,pmpol->pol,temp,scratch,nextp);

#if d294
	if (spy>=4) {
		mutex_lock(write_stdout);
		printf("Worker %d : Reduction by polynomial %d (temp = 0x%x)\n",
		    *(int *)cthread_data(cthread_self()),
		    pmpol->label, temp);
		mpolout(temp);
		printf("\n\n");
		mutex_unlock(write_stdout);
	}
#endif d294

	temp->done = 1;
	temp->maxterms = temp->nterms;
	mpolfree(p);

	free((char *)scratch);
	return ( 0 );
}

FN MPOL *redfast2(p,nextp)  MPOL *p; 
LMPOL *nextp;
{
	MPOL *temp;
	LMPOL *pmpol;
	short *scratch;

	for (;;) {
		/* soumen: patch up */
		if(p->nterms==0)
			return(p);
		pmpol=nextp;
		while(
		    (pmpol!=NULL)&&
		    (!expodiv(MEXPO(pmpol->pol,0),MEXPO(p,0))||
		    (pmpol->pol->done==0))
		    )
			pmpol=pmpol->next;

		if (pmpol==NULL)
			return( p );

		if ((scratch = (short *)galloc(4*(nvars+1)))==0) valerr();

		temp = (MPOL *) galloc( sizeof(MPOL) );
		temp->nterms = 0;
		temp->done = 0;

		/* printf("reduced by: "); mpolout(pmpol->pol); printf("\n"); */
		redonefast(p,pmpol->pol,temp,scratch,nextp);

		mpolfree(p);
		free((char *)scratch);
		temp->done = 1;
		p = temp;
	}
}

#if	INTERACTIVE
#endif	INTERACTIVE

FN redonefast(p,q,r,scratch,nextp) MPOL *p,*q,*r; 
short *scratch;
LMPOL *nextp;
{
	MINT coef1,coef2,rem;
	register int ip,iq,ires;
	short *pow,*lpp;
	int flag;


	pow = scratch;
	lpp = scratch + nvars + 1;
	MINIT(&coef1); 
	MINIT(&coef2);
	MINIT(&rem);

	/* This code should never execute. */
	while ( p->nterms == 0 ) { /* What if p has 0 terms? */

	}

	exposub(MEXPO(p,0),MEXPO(q,0),scratch);
	mgcd(&(p->coefs[0]),&(q->coefs[0]),&coef1);
	mdiv(&(p->coefs[0]),&coef1,&coef2,&rem);
	mdiv(&(q->coefs[0]),&coef1,&coef1,&rem);
	if (mtest(&coef1)<0){
		mnegate(&coef1); 
		mnegate(&coef2);
	}

	ip=1;
	iq=1;
	ires=0;

#if 0
	POL_ALLOC(r,p->nterms+q->nterms-2);
#endif
	/* Make sure that there is enough space */
	while (q->done == 0) {
	}
	POL_ALLOC(r,p->maxterms+q->nterms-2);

	if (iq<q->nterms) {
		expoadd(pow,MEXPO(q,1),lpp);
	}
	flag = 0;
	while ( flag == 0 ) {
		if ((ip<p->nterms)&&(iq<q->nterms)) {
			switch((*cmp_exp)(MEXPO(p,ip),lpp)){
			case 1 : 
				expocopy(MEXPO(p,ip),MEXPO(r,ires));
				MINIT(&(r->coefs[ires]));
				mmult(&(p->coefs[ip]),&coef1,&(r->coefs[ires]));
				ip++;
				ires++;
				r->nterms++;
				break;
			case -1 : 
				expocopy(lpp,MEXPO(r,ires));
				MINIT(&(r->coefs[ires]));
				mmult(&(q->coefs[iq]), &coef2, &(r->coefs[ires]));
				mnegate(&(r->coefs[ires]));
				iq++;
				ires++;
				r->nterms++;
				if (iq<q->nterms) expoadd(pow,MEXPO(q,iq),lpp);
				break;
			case 0 : 
				MINIT(&(r->coefs[ires]));
				mmult(&(p->coefs[ip]),&coef1,&(r->coefs[ires]));
				mmult(&(q->coefs[iq]),&coef2,&rem);
				msub(&(r->coefs[ires]),&rem,&(r->coefs[ires]));
				if (mtest(&(r->coefs[ires]))){
					expocopy(lpp,MEXPO(r,ires));
					ires++;
					r->nterms++;
				};
				ip++;
				iq++;
				if (iq<q->nterms) expoadd(pow,MEXPO(q,iq),lpp);
			}
		}
		else if ( iq == q->nterms ) { 
			flag = 1; 
		}
		else if ( (p->done == 1) && (ip == p->nterms) ) { 
			flag = 1; 
		}
		else {
			while ( (ip >= p->nterms) && (p->done == 0) ) {
			}
			if ( (ip >= p->nterms) && (p->done == 1) ) { 
				flag = 1; 
			}
		}
	}
	flag = 0;
	while ( flag == 0 ) {
		if (ip<p->nterms) {
			expocopy(MEXPO(p,ip),MEXPO(r,ires));
			MINIT(&(r->coefs[ires]));
			mmult(&(p->coefs[ip]),&coef1,&(r->coefs[ires]));
			ip++;
			ires++;
			r->nterms++;
		}
		else if ( p->done == 1) {
			if (ip >= p->nterms) { 
				flag = 1; 
			}
		}
		else {
			while ( (ip >= p->nterms) && (p->done == 0) ) {
			}
			if ( (ip >= p->nterms) && (p->done == 1) ) { 
				flag = 1; 
			}
		}
	}
	while (iq<q->nterms){
		expoadd(pow,MEXPO(q,iq),MEXPO(r,ires));
		MINIT(&(r->coefs[ires]));
		mmult(&(q->coefs[iq]), &coef2, &(r->coefs[ires]));
		mnegate(&(r->coefs[ires]));
		iq++;
		ires++;
		r->nterms++;
	}

	if (ires==0){
		free((char *) r->coefs);
		free((char *) r->expos);
	};

	MFREE(&rem);
	MFREE(&coef1);
	MFREE(&coef2);
}




FN spolfast(p,q,r) MPOL *p,*q,*r;
{
	MINT coef1,coef2,rem;
	register ip,iq,ires;
	short *expofactp,*expofactq,*lpp_p,*lpp_q;
	int flag,flag2;

	if ((expofactp = (short *)galloc(8*(unsigned)(nvars+1)))==0)
		valerr();
	expofactq = expofactp + nvars + 1;
	lpp_p = expofactq + nvars + 1;
	lpp_q = lpp_p + nvars + 1;

	MPOLINIT(r);
	MINIT(&coef1); 
	MINIT(&coef2);
	MINIT(&rem);

	expofactor(MEXPO(p,0),MEXPO(q,0),expofactp);
	expofactor(MEXPO(q,0),MEXPO(p,0),expofactq);

	mgcd(&(p->coefs[0]),&(q->coefs[0]),&coef1);
	mdiv(&(p->coefs[0]),&coef1,&coef2,&rem);
	mdiv(&(q->coefs[0]),&coef1,&coef1,&rem);

	ip=1;
	iq=1;
	ires=0;
#if 0
	while ( p->done == 0 ) {
#if dead_time
		dead_time[*(int *)(cthread_data(cthread_self()))] ++;
#endif 

	}
	while ( q->done == 0 ) {
#if dead_time
		dead_time[*(int *)(cthread_data(cthread_self()))] ++;
#endif 
	}
	POL_ALLOC(r,p->nterms+q->nterms-2);
#endif 0
	POL_ALLOC(r,p->maxterms+q->maxterms-2);

	flag = 0;
	while (flag == 0) {
		if ((ip<p->nterms)&&(iq<q->nterms)){
			expoadd(expofactp,MEXPO(p,1),lpp_p);
			expoadd(expofactq,MEXPO(q,1),lpp_q);
			flag = 1;
		} else if (/* (p->done == 1) &&*/ (ip>=p->nterms)) {
			flag = 1;
		} else if (/* (q->done == 1) &&*/ (iq>=q->nterms)) {
			flag = 1;
		}
	}
	flag = 0;
	while ( flag == 0 ) {
		if ((ip<p->nterms)&&(iq<q->nterms)) {
			switch((*cmp_exp)(lpp_p,lpp_q)){
			case 1 : 
				expocopy(lpp_p,MEXPO(r,ires));
				MINIT(&(r->coefs[ires]));
				mmult(&(p->coefs[ip]),&coef1,&(r->coefs[ires]));
#if arith_cost
				arith_cost[*cthread_data(cthread_self())] 
				    += 2*MSIZE(&(p->coefs[ip]))*
				    MSIZE(&coef1);
#endif
				ip++;
				ires++;
				r->nterms++;
				flag2 = 0;
				while ( flag2 == 0) {
					if (ip<p->nterms) {
						expoadd(expofactp,MEXPO(p,ip),lpp_p);
						flag2 = 1;
					} else if (/* (p->done == 1) &&*/ (ip >= p->nterms)) {
						flag2 = 1;
					}
				}
				break;
			case -1 : 
				expocopy(lpp_q,MEXPO(r,ires));
				MINIT(&(r->coefs[ires]));
				mmult(&(q->coefs[iq]),
				    &coef2,
				    &(r->coefs[ires]));
#if arith_cost
				arith_cost[*cthread_data(cthread_self())] 
				    += 2*MSIZE(&(q->coefs[iq]))*
				    MSIZE(&coef2);
#endif
				mnegate(&(r->coefs[ires]));
				iq++;
				ires++;
				r->nterms++;
				flag2 = 0;
				while ( flag2 == 0) {
					if (iq<q->nterms) {
						expoadd(expofactq,MEXPO(q,iq),lpp_q);
						flag2 = 1;
					} else if (/*(q->done == 1) &&*/ (iq >= q->nterms)) {
						flag2 = 1;
					}
				}
				break;
			case 0 : 
				MINIT(&(r->coefs[ires]));
				mmult(&(p->coefs[ip]),&coef1,&(r->coefs[ires]));
				mmult(&(q->coefs[iq]),&coef2,&rem);
				msub(&(r->coefs[ires]),&rem,&(r->coefs[ires]));
#if arith_cost
				arith_cost[*cthread_data(cthread_self())] +=
				    2*MSIZE(&(p->coefs[ip]))*MSIZE(&coef1) +
				    2*MSIZE(&(q->coefs[iq]))*MSIZE(&coef2) +
				    (MSIZE(&(p->coefs[ip]))+MSIZE(&coef1)>
				    MSIZE(&(q->coefs[iq]))+MSIZE(&coef2))?
				    MSIZE(&(p->coefs[ip]))+ MSIZE(&coef1) + 1:
				    MSIZE(&(q->coefs[iq]))+ MSIZE(&coef2) + 1;
#endif
				if (mtest(&(r->coefs[ires]))){
					expocopy(lpp_p,MEXPO(r,ires));
					ires++;
					r->nterms++;
				};
				ip++;
				iq++;
				flag2 = 0;
				while ( flag2 == 0) {
					if (ip<p->nterms) {
						expoadd(expofactp,MEXPO(p,ip),lpp_p);
						flag2 = 1;
					} else if (/*(p->done == 1) && */(ip >= p->nterms)) {
						flag2 = 1;
					}
				}
				flag2 = 0;
				while ( flag2 == 0) {
					if (iq<q->nterms) {
						expoadd(expofactq,MEXPO(q,iq),lpp_q);
						flag2 = 1;
					} else if (/*(q->done == 1) && */(iq >= q->nterms)) {
						flag2 = 1;
					}
				}
			}
		}
		else if ( /*(p->done == 1) &&*/ (ip >= p->nterms) ) { 
			flag = 1; 
		}
		else if (/* (q->done == 1) &&*/ (iq >= q->nterms) ) { 
			flag = 1; 
		}
		else {
			while ( (ip >= p->nterms) && (iq >= q->nterms)/* && (p->done == 0) &&
			    (q->done == 0)*/) {
#if dead_time
				dead_time[*(int *)(cthread_data(cthread_self()))] ++;
#endif 
			}
			if ( (ip >= p->nterms)/* && (p->done == 1)*/ ) {
				flag = 1; 
			}
			if ( (iq >= q->nterms)/* && (q->done == 1)*/ ) {
				flag = 1; 
			}
		}
	}
	flag = 0;
	while ( flag == 0 ) {
		if (ip<p->nterms) {
			expoadd(expofactp,MEXPO(p,ip),MEXPO(r,ires));
			MINIT(&(r->coefs[ires]));
			mmult(&(p->coefs[ip]),&coef1,&(r->coefs[ires]));
#if arith_cost
			arith_cost[*cthread_data(cthread_self())] 
			    += 2*MSIZE(&(p->coefs[ip]))*MSIZE(&coef1);
#endif
			ip++;
			ires++;
			r->nterms++;
		}
		else if (/* p->done ==*/ 1 ) {
			if (ip >= p->nterms) { 
				flag = 1; 
			}
		}
		else {
			while ( 0 /*(ip >= p->nterms) && (p->done == 0)*/ ) {
#if dead_time
				dead_time[*(int *)(cthread_data(cthread_self()))] ++;
#endif 
			}
			if ( (ip >= p->nterms)/* && (p->done == 1)*/ ) { 
				flag = 1; 
			}
		}
	}
	flag = 0;
	while ( flag == 0) {
		if (iq<q->nterms){
			expoadd(expofactq,MEXPO(q,iq),MEXPO(r,ires));
			MINIT(&(r->coefs[ires]));
			mmult(&(q->coefs[iq]),
			    &coef2,
			    &(r->coefs[ires]));
#if arith_cost
			arith_cost[*cthread_data(cthread_self())] 
			    += 2*MSIZE(&(q->coefs[iq]))*MSIZE(&coef2);
#endif
			mnegate(&(r->coefs[ires]));
			iq++;
			ires++;
			r->nterms++;
		}
		else if (/* q->done ==*/ 1 ) {
			if (iq >= q->nterms) { 
				flag = 1; 
			}
		}
		else {
			while ( ( iq >= q->nterms)/* && (q->done == 0)*/) {
#if dead_time
				dead_time[*(int *)(cthread_data(cthread_self()))] ++;
#endif 
			}
			if ( (iq >= q->nterms)/* && (q->done == 1) */) { 
				flag = 1; 
			}
		}
	}
	r->nterms = ires;
	r->done = 1;
	r->maxterms = r->nterms;
#if 0
	/* can't do this anymore */
	if (redgcd) mpolunit(r,&rem,r);
#endif 0

#if d294
	if (spy>=4) {
		mutex_lock(write_stdout);
		printf("Worker %d : s-polynomial of current pair :\n",
		    *cthread_data(cthread_self()));
		mpolout(r);
		printf("\n\n");
		mutex_unlock(write_stdout);
	}
#endif d294

	MFREE(&rem);
	MFREE(&coef1);
	MFREE(&coef2);
	free(expofactp);
}




FN redfasttotal(p,s,q,c) MPOL *p,*q; 
MPOLSET *s; 
MINT *c;

/* used at the end to obtain the unique completely reduced basis. 
   The leading monomial is supposed irreducible. */
{
	LMPOL *pt;
	int j;
	MPOL temp;
	short *scratch;

	if (p->nterms==0){
		mpolfree(q);
		return;
	};

	if ((scratch = (short *)galloc(4*(nvars+1)))==0) valerr();

	MPOLINIT(&temp);
	mpolcopy(p,&temp);
	MSET(1,c);
	j=1;
	while (j<temp.nterms){
		pt = s->pols;
		while ((temp.nterms>0)&&(pt!=NULL)){
			if (expodiv(MEXPO(pt->pol,0),MEXPO(&temp,j))){
				redonetotal(&temp,j,pt->pol,&temp,c,scratch);
				if (spy>=4){
					printf("Reduction by polynomial %d\n",
					    pt->label);
					if ((temp.nterms!=0)&&(!meqshort(c,1))){
						printf("1/");
						mout(c);
						printf(" (");
						mpolout(&temp);
						printf(")\n\n");
					}
					else {
						mpolout(&temp);
						printf("\n\n");
					};
				};
				pt=s->pols;
			}
			else pt=pt->next;
		};
		j++;
	};

	mpolfree(q);
	MPOLMOVEFREE(&temp,q);
	free((char *)scratch);
};






FN redonetotal(p,i,q,r,c,scratch) MPOL *p,*q,*r; 
MINT *c; 
short *scratch;

/* used at the end to obtain a total reduced basis. 
** The polynomial p is reduced at monomial i by q 
** For now, this is a sequential version. */
{
	MPOL temp;
	MINT coef1,coef2,rem;
	register ip,iq,ires;
	short *pow,*lpp;


	pow = scratch;
	lpp = scratch + nvars + 1;
	MPOLINIT(&temp);
	MINIT(&coef1); 
	MINIT(&coef2);
	MINIT(&rem);

	exposub(MEXPO(p,i),MEXPO(q,0),pow);
	mgcd(&(p->coefs[i]),&(q->coefs[0]),&coef1);
	mdiv(&(p->coefs[i]),&coef1,&coef2,&rem);
	mdiv(&(q->coefs[0]),&coef1,&coef1,&rem);
	mmult(c,&coef1,c);
	iq=1;
	ires=0;
	POL_ALLOC(&temp,p->nterms+q->nterms-2);

	for (ip=0;ip<i;ip++){
		expocopy(MEXPO(p,ip),MEXPO(&temp,ires));
		MINIT(&(temp.coefs[ires]));
		mmult(&(p->coefs[ip]),&coef1,&(temp.coefs[ires]));
#if arith_cost
		arith_cost[*cthread_data(cthread_self())] += 
		    2*MSIZE(&(p->coefs[ip]))*MSIZE(&coef1);
#endif
		ires++;
	};
	ip++;
	if (iq<q->nterms)
		expoadd(pow,MEXPO(q,1),lpp);
	while ((ip<p->nterms)&&(iq<q->nterms))
		switch((*cmp_exp)(MEXPO(p,ip),lpp)){
		case 1 : 
			expocopy(MEXPO(p,ip),MEXPO(&temp,ires));
			MINIT(&(temp.coefs[ires]));
			mmult(&(p->coefs[ip]),&coef1,&(temp.coefs[ires]));
#if arith_cost
			arith_cost[*cthread_data(cthread_self())] += 
			    2*MSIZE(&(p->coefs[ip]))*MSIZE(&coef1);
#endif
			ip++;
			ires++;
			break;
		case -1 : 
			expocopy(lpp,MEXPO(&temp,ires));
			MINIT(&(temp.coefs[ires]));
			mmult(&(q->coefs[iq]),
			    &coef2,
			    &(temp.coefs[ires]));
#if arith_cost
			arith_cost[*cthread_data(cthread_self())] += 
			    2*MSIZE(&(q->coefs[iq]))*MSIZE(&coef2);
#endif
			mnegate(&(temp.coefs[ires]));
			iq++;
			ires++;
			if (iq<q->nterms) expoadd(pow,MEXPO(q,iq),lpp);
			break;
		case 0 : 
			MINIT(&(temp.coefs[ires]));
			mmult(&(p->coefs[ip]),&coef1,&(temp.coefs[ires]));
			mmult(&(q->coefs[iq]),&coef2,&rem);
			msub(&(temp.coefs[ires]),&rem,&(temp.coefs[ires]));
#if arith_cost
			arith_cost[*cthread_data(cthread_self())] +=
			    2*MSIZE(&(p->coefs[ip]))*MSIZE(&coef1) +
			    2*MSIZE(&(q->coefs[iq]))*MSIZE(&coef2) +
			    (MSIZE(&(p->coefs[ip]))+MSIZE(&coef1)>
			    MSIZE(&(q->coefs[iq]))+MSIZE(&coef2))?
			    MSIZE(&(p->coefs[ip]))+ MSIZE(&coef1) + 1:
			    MSIZE(&(q->coefs[iq]))+ MSIZE(&coef2) + 1;
#endif
			if (mtest(&(temp.coefs[ires]))){
				expocopy(lpp,MEXPO(&temp,ires));
				ires++;
			};
			ip++;
			iq++;
			if (iq<q->nterms) expoadd(pow,MEXPO(q,iq),lpp);
		};
	while (ip<p->nterms) {
		expocopy(MEXPO(p,ip),MEXPO(&temp,ires));
		MINIT(&(temp.coefs[ires]));
		mmult(&(p->coefs[ip]),&coef1,&(temp.coefs[ires]));
#if arith_cost
		arith_cost[*cthread_data(cthread_self())] += 
		    2*MSIZE(&(p->coefs[ip]))*MSIZE(&coef1);
#endif
		ip++;
		ires++;
	};
	while (iq<q->nterms){
		expoadd(pow,MEXPO(q,iq),MEXPO(&temp,ires));
		MINIT(&(temp.coefs[ires]));
		mmult(&(q->coefs[iq]),
		    &coef2,
		    &(temp.coefs[ires]));
#if arith_cost
		arith_cost[*cthread_data(cthread_self())] += 
		    2*MSIZE(&(q->coefs[iq]))*MSIZE(&coef2);
#endif
		mnegate(&(temp.coefs[ires]));
		iq++;
		ires++;
	};
	temp.nterms = ires;

	if (ires==0){
		free((char *) temp.coefs);
		free((char *) temp.expos);
	};

	mpolfree(r);
	MPOLMOVEFREE(&temp,r);
	if (redgcd) mpolunit(r,&rem,r);
	MFREE(&rem);
	MFREE(&coef1);
	MFREE(&coef2);
}


#define d294 0

#include "cmump.h"
#include "multpol.h"
#include <stdio.h>

#if	TIME_REDUCTION
#include <mach.h>
#endif	TIME_REDUCTION

#include "gbas.h"
#include <sys/time.h>

#if LOCKCOUNT
	extern int lockcount;
#endif

extern char etext;
extern short nvars;
extern char *varnames[30];
extern short next_label;

extern int nworkers, co_workers;
extern int spy;
extern int deletion, redgcd, allow_same_lpp;
extern int polynomial_rlimit;
extern int pair_input_flag, pair_input_size, *pair1, *pair2;
extern int zero_poly_count, zero_poly_threshold;

extern struct timeval start_time;
struct timeval t1,t2;
int total_block_time = 0;
int sleep_count = 0;
int primitive_reduction_count = 0;
int dead_time[MAX_WORKERS];

#if	eager_kills
/*
 * Keep track of the pair each worker-pipeline is reducing. 
 * Only use elements [0..nworkers].  Co-workers refer to the workers'
 * entry.
 */
LMPOL	*current_pair[MAX_WORKERS][2];
int	kill_pair[MAX_WORKERS];
int	valid_pair[MAX_WORKERS];
#endif	eager_kills

/* all these data structures are shared by the different threads */

static MPOLSET pols;
static PAIRSET pairs;

#if d294
mutex_t block_pairs;
mutex_t write_pols;
mutex_t write_stdout;
static condition_t wake_up, end;
#endif d294

static int n_idles;

/* wake_up is broadcast when new-pairs appear 
   n_idles is the numbers of workers waiting for work.*/



int workername[MAX_WORKERS];
long arith_cost[MAX_WORKERS]; 
/* workername give a way to associate an index with the process, without 
** to carry it as a parameter to every function (we use cthread_set_data 
** to achieve this).
** arith_cost measures the approximate number of arithmetical operations on
** short integers performed by this process.
*/
extern long total_arith_cost;


int nbumps[MAX_WORKERS];
/* 
** We count the number of times we need to substract two unequal numbers.
** This is to measure how an implementation of numbers as their prime
** factorizarion will be useful.
*/


int nred=0;
/* counts the number of pairs which have been really reduced. */

int log_p1[128],log_p2[128],log_who[128];
int log_index = 0;

#if	TIME_REDUCTION

/*
 * time_ stuff is used to record intervals for reductions delays using 
 * the microsecond timer.
 */

#define	TIME_SIZE	32768

unsigned	*microtime;
vm_address_t	timer_address;
int		time_log_start[TIME_SIZE];
int		time_log_end[TIME_SIZE];
int		time_who[TIME_SIZE];
int		time_what[TIME_SIZE];
int		time_index = 0;

#define		TIME_WHAT_PAIR 1
#define		TIME_WHAT_SPOL 2
#define		TIME_WHAT_RED  3
#define		TIME_WHAT_FINI 4
#endif	TIME_REDUCTION

struct co_queue co_work_queues[MAX_WORKERS];
static int workers_ready = 1; /* Initial thread is always ready */

/* -------------------------------------------------------------------------------------- */

#if	SINGLE_QUEUE
co_queue_init( q )
struct co_queue *q;
{
  q->count = 0;
  q->enque_index = 0;
  q->deque_index = 0;
  q->lock = mutex_alloc();
}
#endif	SINGLE_QUEUE

/* -------------------------------------------------------------------------------------- */

pre_allocate_memory()
/* This is done by worker threads != 0.  This is a hack to zero-fill and
   put on free-lists some of the pre allocated memory.
*/
{
  int i;
  char *j;
  
  for (i=4; i<128; i = i * 2 ) {
    j = (char *) malloc(i);
    free(j);
  }
}

/* -------------------------------------------------------------------------------------- */

enque( work, next, qr )
MPOL *work;
LMPOL *next;
struct co_record *qr;
{
#if	SINGLE_QUEUE
	
#endif	SINGLE_QUEUE
	qr->work_pol = work;
	qr->next_pol = next;
	qr->signal = 1;
	if ( spy >= 4 ) {
#if d294
		mutex_lock(write_stdout);
		printf("Co-worker %d : op=enque\n",
			*(int *)cthread_data(cthread_self()));
		mutex_unlock(write_stdout);
#endif d294
	}
}

/* -------------------------------------------------------------------------------------- */

deque( )
{
	if ( spy >= 4 ) {
#if d294
		mutex_lock(write_stdout);
		printf("Co-worker %d : op=deque\n",
			*(int *)cthread_data(cthread_self()));
		mutex_unlock(write_stdout);
#endif
	}
}

/* -------------------------------------------------------------------------------------- */

co_worker( i )
int i;
{
	int flag,j;
#if d294
	mutex_t block;
#endif d294
	struct co_record *qr,*co_work_record;
	struct co_queue *co_work_queue;

#if	TIME_REDUCTION_1
	int time_temp,time_index_t;
#endif	TIME_REDUCTION_1
#if	TIME_REDUCTION_3
	int co_worker_ready[256];
#endif	TIME_REDUCTION_3

	flag = (i>=1024) ? 1 : 0;
	i = (i>=1024) ? i-1024 : i;
	cthread_set_data(cthread_self(),(char *)&i );
#if	TIME_REDUCTION_3
	co_worker_ready[i] = 1;
	if (i == 0) {
	  for (j = 1; j < nworkers + co_workers; j++) {
	    while (co_worker_ready[i] != 1);
	  }
	}
#endif	TIME_REDUCTION_3

	co_work_queue = &( co_work_queues[ i % nworkers ] );
	qr = &(co_work_queue->entries[ i / nworkers ]);
	if ( (i + nworkers) >= (nworkers + co_workers) ) {
	  co_work_record = &(co_work_queue->entries[ 0 ]);
        } else {
	  co_work_record = &(co_work_queue->entries[ (i / nworkers + 1) ] );
	}
#if d294
	block = mutex_alloc();
#endif d294
	while (1) {

#if	INTERACTIVE
	        interactive();
#endif	INTERACTIVE

		switch ( flag ) {
		case 1:
			flag = gbasis_one_worker( co_work_record );
			break;

		case 0:
		deque();
		while ( qr->signal == 0 ) {
#if dead_time
			dead_time[*(int *)cthread_data(cthread_self())]++;
#endif 
		}

#if	TIME_REDUCTION_1
		time_temp = *microtime;		       
#endif	TIME_REDUCTION_1

		qr->signal = 0;
		if (spy >= 4) {
#if d294
			mutex_lock ( write_stdout );
			printf("Co-worker %d : awake\n",
				*(int *)cthread_data(cthread_self()));
			mutex_unlock ( write_stdout );
#endif d294
		}
		if (qr->work_pol != NULL) {
			if (spy >= 4) {    
#if d294
				mutex_lock( write_stdout );
				printf("Co-worker %d : calling redfast \n",
					*(int *)cthread_data(cthread_self()));
				mutex_unlock( write_stdout );
#endif d294
			}
			flag = redfast( qr->work_pol, qr->next_pol,
				       co_work_record );
		}
#if	TIME_REDUCTION_1
	time_index_t = time_index++;
	time_log_start[ time_index_t ] = time_temp;
	time_log_end[ time_index_t ] = *microtime;
	time_who[ time_index_t ] = *((int *)cthread_data(cthread_self()));
	time_what[ time_index_t ] = TIME_WHAT_RED;
#endif	TIME_REDUCTION_1
		}
	}
}

int gbas_done = 0;

#if	TIME_REDUCTION_3
	unsigned time_get,time_work,time_end_work,time_get_old,time_new_poly;
#endif	TIME_REDUCTION_3

/* -------------------------------------------------------------------------------------- */

int gbasis_one_worker( co_work_record )
struct co_record *co_work_record;
{
	LMPOL *p1,*p2,*premp;
	MPOL *remp;
	int flag;

#if	TIME_REDUCTION_1
	unsigned time_temp,time_index_t;
#endif	TIME_REDUCTION_1

#if block_time
        struct timeval block_start,block_stop;
        gettimeofday( &block_start, 0 );
#endif 

#if	TIME_REDUCTION_1
	time_temp = *microtime;
#endif	TIME_REDUCTION_1
#if	TIME_REDUCTION_3
	/* The first time this is executed, time_end gets a valid begin
	   time.  The second time around, this is pushed to time_start, and
	   time_end gets set.  Used in conjuction with -x1, this should
	   provide a more accurate time from selection of first pair to
	   completion of the reduction and updating the pairset */
	time_get_old = time_get;
	time_get = *microtime;
#endif	TIME_REDUCTION_3
       
#if d294
	mutex_lock( block_pairs );
#endif d294
	n_idles++;
	if ( (pairs.npairs==0) && 
		(n_idles == nworkers) ) {
#if d294
		mutex_unlock( block_pairs );
#endif d294
		gbas_done = 1;
		gbasis_conclude();
	};
	while (pairs.npairs==0) {
#if sleep_count
		sleep_count++;
#endif 
#if d294
		condition_wait(wake_up,block_pairs);
#endif d294
		if (gbas_done == 1) {
#if d294
		  mutex_unlock( block_pairs);
		  cthread_exit(0);
#endif d294
		}
	}

	/* Check polynomial_rlimit and quit if we hit it */
	if (polynomial_rlimit == nred) {
	  if (n_idles == nworkers) {
	    gbas_done = 1;
	    gbasis_terminate();
	  }
#if d294
	  mutex_unlock( block_pairs );
          cthread_exit(0);
#endif d294
	}

	/* Check zero_poly_count, and quit if we exceeded threshold */
	if (zero_poly_count > zero_poly_threshold) {
	  printf("zero_poly_count = %d, nred = %d\n",zero_poly_count,nred);
	}

	choose_pair(&pairs,&p1,&p2);
	nred++;
	n_idles--;

#if block_time
        gettimeofday( &block_stop, 0 );
        total_block_time += elapsed_time( &block_start, &block_stop );
#endif 

#if show_log
	/* log code */
	log_p1[log_index] = p1->label;
	log_p2[log_index] = p2->label;
	log_who[log_index] = *((int *)cthread_data(cthread_self()));
	log_index++;
#endif show_log


#if	TIME_REDUCTION_1
	time_index_t = time_index++;
	time_log_start[ time_index_t ] = time_temp;
	time_log_end[ time_index_t ] = *microtime;
	time_who[ time_index_t ] = *((int *)cthread_data(cthread_self()));
	time_what[ time_index_t ] = TIME_WHAT_PAIR;
#endif	TIME_REDUCTION_1

#if d294
	mutex_unlock(block_pairs);
#endif d294


	if (spy>=2){
#if d294
		mutex_lock(write_stdout);
		printf("Reduction of the pair (%d,%d) by worker %d\n\n",
				p1->label,p2->label,
				*(int *)cthread_data(cthread_self()));
		mutex_unlock(write_stdout);
#endif d294
	};

#if	TIME_REDUCTION_1
	time_temp = *microtime;
#endif	TIME_REDUCTION_1
#if	TIME_REDUCTION_3
	time_work = *microtime;
	time_new_poly = 0; /* We have not yet produced a new polynomial
			in this reduction cycle */
#endif	TIME_REDUCTION_3

	remp = (MPOL *) malloc( sizeof(MPOL) );
	remp->nterms = 0;
	remp->done = 0;

/* A bit more parallelism can be squeezed out here by
   beginning the first reduction as soon as the first term
   of the s-polynomial is available.  For Plus, fences will
   be needed in spolfast ala redonefast, and a co-worker
   will have to be selected here just like in redfast. */

#define pipelined_spol 1
#if	pipelined_spol
	enque( remp, pols.pols, co_work_record );
#endif
	spolfast(p1->pol,p2->pol,remp);
	remp->done = 1;

#if	TIME_REDUCTION_1
	time_index_t = time_index++;
	time_log_start[ time_index_t ] = time_temp;
	time_log_end[ time_index_t ] = *microtime;
	time_who[ time_index_t ] = *((int *)cthread_data(cthread_self()));
	time_what[ time_index_t ] = TIME_WHAT_SPOL;
#endif	TIME_REDUCTION_1

#if	pipelined_spol
	return ( 0 );
#else	pipelined_spol

#if primitive_reduction_count
	printf("prim red count = %d\n",primitive_reduction_count);
#endif

	if ( remp->nterms != 0 ) {
		flag = redfast( remp, pols.pols, co_work_record );
	} else {
		flag = 1;
	}
	return ( flag );
#endif	pipeline_spol 
}

/* -------------------------------------------------------------------------------------- */

finish_and_add(remp)
MPOL *remp;
{
        /*
         * We should really put back in some checks to prevent more than
         * one polynomial with the same leading power product from being
         * added to the polset.   
         */

        LMPOL *premp;
#if block_time
        struct timeval block_start,block_stop;
#endif 

#if	TIME_REDUCTION_1
	int time_temp,time_index_t;

	time_temp = *microtime;
#endif	TIME_REDUCTION_1


        if ((premp=(LMPOL *)malloc(sizeof(LMPOL)))==0)
       	        valerr();
	premp->pol = remp;
        premp->next = NULL;
        premp->old=0; premp->np=0;

#if block_time
        gettimeofday( &block_start, 0);
#endif 

#if	TIME_REDUCTION_3
	time_end_work = *microtime;
	time_new_poly = 1; /* Record that a new polynomial was produced */
#endif	TIME_REDUCTION_3

#if d294
	mutex_lock(block_pairs);
#endif d294
        premp->label=next_label;next_label++;
        mpolsetadd(&pols,premp);
        update_pairs(&pairs,&pols,premp);

	if (pairs.npairs>0) {
#if d294
		condition_broadcast(wake_up);
#endif d294
	}

        if (spy>=2){
#if d294
                mutex_lock(write_stdout);
                printf("Polynomial %d created by worker %d : ",
                            premp->label,
                            *(int *)cthread_data(cthread_self()));
		mpolout(remp);
		printf("\n");

		print_pairset(&pairs);
		mutex_unlock(write_stdout);
#endif d294
        }
#if block_time
        gettimeofday( &block_stop, 0 );
        total_block_time += elapsed_time( &block_start,&block_stop);
#endif 

#if d294
	mutex_unlock(block_pairs);
#endif d294

#if	TIME_REDUCTION_1
	time_index_t = time_index++;
	time_log_start[ time_index_t ] = time_temp;
	time_log_end[ time_index_t ] = *microtime;
	time_who[ time_index_t ] = *((int *)cthread_data(cthread_self()));
	time_what[ time_index_t ] = TIME_WHAT_FINI;
#endif	TIME_REDUCTION_1

}

/* -------------------------------------------------------------------------------------- */


LMPOL *label_to_lmpol(s,i)
LMPOL *s;
int i;
{
  while (s != NULL) {
    if (s->label == i) return s;
    s=s->next;
  }
  printf("label_to_lmpol: no LMPOL\n");
  exit(-1);
}

/* -------------------------------------------------------------------------------------- */

gbasis(spol) MPOLSET *spol;

{
	int i;
 	LMPOL *pt,*pt2, *p1, *p2;

#if	TIME_REDUCTION
	if ( mapfrcounter( &timer_address, TRUE ) == -1 ) {
	  printf("gbas: mapfrcounter failed\n");
	  exit(-1);
	}
	microtime = (unsigned *) timer_address;
#endif	TIME_REDUCTION

#if d294
	block_pairs = mutex_alloc();
	write_pols = mutex_alloc();
	write_stdout = mutex_alloc();
	wake_up = condition_alloc();
#endif d294
	
	MPOLSETINIT(&pols);
	PAIRSETINIT(&pairs);

	pt = spol->pols;
	while (pt!=NULL){
		if ((pt2 = (LMPOL *) malloc(sizeof(LMPOL)))==0)
			printf("No space available.\n");
		*pt2 = (*pt);
		/*pt2->label = pt->label;*/ /* MMax compiler is braindead */
		if ((pt2->pol = (MPOL *) malloc(sizeof(MPOL)))==0)
			printf("No space available.\n");
		MPOLINIT(pt2->pol);
		mpolcopy(pt->pol,pt2->pol);
		pt2->next = NULL;
		pt2->pol->done = 1;
		mpolsetadd(&pols,pt2);
		if (pols.npols > 1) {
		    update_pairs(&pairs,&pols,pt2,pols.npols-1);
	        }
		if (spy>=3) print_pairset(&pairs);
		pt = pt->next;
	};

	/* Accept pairs using -y option */
	if (pair_input_flag) {
	  pairs.npairs = pair_input_size;
	  for (i=pair_input_size-1; i>=0; i--) {
	    pairs.pairs[i].p1 = label_to_lmpol(pols.pols,pair1[i]);
	    pairs.pairs[i].p2 = label_to_lmpol(pols.pols,pair2[i]);
	  }
	  free(pair1); free(pair2);
        }

	if (spy>=2){
		printf("Initially :\n");
		printf("Set of polynomials :\n");
		pt=spol->pols;
		while (pt!=NULL){
			printf("Polynomial %d : ",pt->label);
			mpolout(pt->pol);	
			printf("\n");
			pt=pt->next;
		};
		printf("\n");
		print_pairset(&pairs);
	};
	
        /* Create workers and co_workers */
        gettimeofday(&t1,0);
        for  (i=0; i<co_workers;i++ ) {
          cthread_detach(cthread_fork(co_worker,(i+nworkers)));
        }  
#if 0
        while (co_work_queue.count + co_workers != 0); /* XXX */
        if ( spy >= 2) {
	  mutex_lock (write_stdout);
          printf("Gbasis: All co-workers enque'd.\n");
          fflush(stdout);
	  mutex_unlock (write_stdout);
        }
#endif

	n_idles = 0;
	for (i=0;i<nworkers;i++){
		workername[i]=i;
		arith_cost[i]=0;
		nbumps[i]=0;
	};
	for (i=1;i<nworkers;i++)
		 /* Hack: add 1024 to indicate gbasis_one_worker */
		cthread_detach(cthread_fork(co_worker,(i+1024)));

        gettimeofday(&t2,0);
	
#if dead_time
	/* clear out the dead_time counters */
	{
		int i;
		for (i=nworkers; i<nworkers+co_workers; i++) {
			dead_time[i]  = 0;
		}
	}
#endif
	co_worker(1024);
};

/* -------------------------------------------------------------------------------------- */

gbasis_terminate()
/* One thread executes this if the x option is used and tripped */
{
  LMPOL *pt;
  int i;
  struct timeval stop_time;

#if d294
  condition_broadcast( wake_up );
#endif d294
  gettimeofday(&stop_time,0);
  printf("%d\n",pairs.npairs);
  for (i=pairs.npairs-1; i>=0; i--) {
    printf("%d %d\n",pairs.pairs[i].p1->label,pairs.pairs[i].p2->label);
  }
  printf("%d\n",nvars);
  for (i=0; i < nvars; i++ ) {
    printf("%s ",varnames[i]);
  }
  printf("\n");
  printf("/* terminated at polynomial_rlimit = %d */\n\n",polynomial_rlimit);
  pt = pols.pols;
  while (pt != NULL ) {
	printf("/* Polynomial %d */ ",pt->label);
	mpolout(pt->pol);
	printf(";\n\n");
	pt=pt->next;
  }
  printf("/*\n");
  printf("Elapsed time for partial phase 1 : %d milliseconds.\n\n",
			elapsed_time(&t2,&stop_time));
/*  printf("Time adjustment : %d milliseconds.\n", elapsed_time(&t1,&t2));*/
#if	TIME_REDUCTION_3
	if (time_new_poly) {
	  printf("Time for single reduction : %d %d %d microseconds.\n", 
	       time_work - time_get_old, time_end_work - time_work,
		time_get - time_end_work );
        } else {
	  printf("Time for single reduction : %d %d %d microseconds.\n", 
	       time_work - time_get_old, time_get - time_work,
		0 );
	}
#endif	TIME_REDUCTION_3
  printf("*/;\n");
  exit(0);
}

/* -------------------------------------------------------------------------------------- */

gbasis_conclude()	

{	
	int i;
	struct timeval stop_time;
	int total_ncolls=0;
	MINT gcd;
	MPOL mon;
	LMPOL *pt;

#if d294
	condition_broadcast( wake_up );
#endif d294
	gettimeofday(&stop_time,0);
	total_arith_cost = 0;
	for (i=0;i<nworkers;i++){
		total_arith_cost += arith_cost[i];
		arith_cost[i]=0;
		total_ncolls += nbumps[i];
	};
	printf("Total number of polynomials which entered the basis \n");
	printf("      (including the initial ones) : %d\n",next_label);
	printf("Number of S-polynomials which have been reduced : %d\n",nred);
	printf("Approximate number of arithmetical operations on short integers\n     during phase 1 : %ld\n",total_arith_cost);
	printf("Number of collisions : %d\n",total_ncolls);
	printf("Elapsed time for phase 1 : %d milliseconds.\n\n",
			elapsed_time(&start_time,&stop_time));
        printf("Time adjustment : %d milliseconds.\n",
                        elapsed_time(&t1,&t2));
#if	TIME_REDUCTION_3
	if (time_new_poly) {
	  printf("Time for single reduction : %d %d %d microseconds.\n", 
	       time_work - time_get_old, time_end_work - time_work,
		time_get - time_end_work );
        } else {
	  printf("Time for single reduction : %d %d %d microseconds.\n", 
	       time_work - time_get_old, time_get - time_work,
		0 );
	}
#endif	TIME_REDUCTION_3
#if block_time
        printf("Block time :  %d milliseconds.\n",total_block_time);
#endif
#if sleep_count
	printf("Sleep count: %d\n",sleep_count);
#endif 
#if primitive_reduction_count
	printf("Primitive reduction count: %d\n",primitive_reduction_count);
#endif
#if show_log
	/* show log */
	{
	  int i;
	  for (i=0; i< log_index; i++) {
	    printf("w%d : (%d,%d)\n",log_who[i],log_p1[i],log_p2[i]);
	  }
	}
#endif

#if	TIME_REDUCTION
	{
	  int i;

	  printf("microsecond timer data:\n");
	  printf("who  start    end      what\n");
	  for (i=0; i<time_index; i++) {
	    printf("%3d %12d %12d %2d\n",time_who[i],time_log_start[i],
		   time_log_end[i],time_what[i]);
	  }
	  printf("\n");
	}
#endif	TIME_REDUCTION

#if dead_time
	/* show dead times */
	{
	  int i;
	  for (i=0; i < nworkers+co_workers; i++ ) {
		printf("%d ",dead_time[i]);
	  }
	  printf("dead counts.\n");
	}
#endif

	start_time = stop_time;
	
	/* and finally "interreduces the result". */	
	
	if (spy>=2) printf("Final interreduction\n");

	eliminate_redundant_end(&pols);
	complete_reduce_end(&pols);
	if (!redgcd)
		MINIT(&gcd);
		pt = pols.pols;
		while (pt!=NULL){
			mpolunit(pt->pol,&gcd,pt->pol);
			pt = pt->next;
		};

	gettimeofday(&stop_time,0);
	if (spy>=1){
		pt = pols.pols;
		while (pt!=NULL){
			printf("Polynomial %d : ",pt->label);
			mpolout(pt->pol);
			printf("\n\n");
			pt=pt->next;
  		}
	}
	else{
		printf("Leading monomials : ");
		pt = pols.pols;
		while (pt!=NULL){
			MPOLMONSET(&(pt->pol->coefs[0]),&(pt->pol->expos[0]),&mon);
			mpolout(&mon);
			if (i!=(pols.npols-1)) printf(", ");
			pt=pt->next;
		};
		printf("\n\n");
	};
	total_arith_cost = 0;
	for (i=0;i<nworkers;i++) total_arith_cost += arith_cost[i];
	
  	printf("\n\nApproximate number of arithmetical operations on short integers \nduring final reduction : %ld\n",total_arith_cost);
	printf("Elapsed time during final interreduction : %d milliseconds.\n",
			elapsed_time(&start_time,&stop_time));
#if LOCKCOUNT
	printf("Waiting for a lock : %d\n",lockcount);
#endif
	exit(0); /* Hack, should kill off co-worker threads so exit occurs */
	cthread_exit(0);
};

/* -------------------------------------------------------------------------------------- */

#if	INTERACTIVE

int go = 0;
int interactive_init = 0;

interactive()
{
  int i,j,ans,x,y;
  LMPOL *p,*p1,*p2;
  MPOL *remp,*reg[10];

  if (go == 1) return;
  if (interactive_init == 0) {
    interactive_init = 1;
    remp = NULL;
    for (j=0; j<10; j++) { reg[j] = NULL; }
  }
  do {
    printf("command: "); fflush(stdout); fflush(stdin);
    scanf("%d",&i);
    if (i == 1) { /* Enter next polynomial */
      p = (LMPOL *) malloc(sizeof(LMPOL));
      p->pol = (MPOL *) malloc(sizeof(MPOL));
      MPOLINIT(p->pol);
      while ((printf("Polynomial %d : ",(int)next_label),ans=mpolin(p->pol))!=1)
	if (ans==0){
		p->label=next_label; next_label++;
		p->old=0;
		p->np=0;
		p->next = NULL;
		p->pol->done = 1;
		p->pol->maxterms = p->pol->nterms;
		mpolsetadd(&pols,p);
		update_pairs(&pairs,&pols,p,next_label-2);
		p = (LMPOL *)malloc(sizeof(LMPOL));
		p->pol = (MPOL *)malloc(sizeof(MPOL));
		MPOLINIT(p->pol);
	};
    } else if (i == 2) { /* Print pairs */
 		print_pairset(&pairs);
    } else if (i == 3) { /* Print pols */
		p = pols.pols;
		while (p!=NULL){
			printf("Polynomial %d : ",p->label);
			mpolout(p->pol);
			printf("\n");
			p=p->next;
  		}
    } else if (i == 4) { /* go do it */
      go = 1;
      return;
    } else if (i == 5) { /* remove pair */
      choose_pair(&pairs,&p1,&p2);
    } else if (i == 6) { /* spol */
      printf("Pair: {Int,Int} "); fflush(stdout);
      scanf("%d,%d",&x,&y);
      if ((x>=0) && (y>=0) && (x!=y)) {
	p = pols.pols; p1 = NULL; p2 = NULL;
	while ( p != NULL ) {
	  if (p->label == x) p1 = p;
	  if (p->label == y) p2 = p;
	  p=p->next;
	}
	remp = (MPOL *) malloc(sizeof(MPOL));
	remp->nterms = 0;
	spolfast(p1->pol,p2->pol,remp);
	remp->done = 1;
	mpolout(remp);
	printf("\n");
      }
    } else if (i == 7) { /* reduce last spol thing */
      if ((remp != NULL) && (remp->nterms != 0)) {
	remp = (MPOL *) redfast2( remp, pols.pols);
	mpolout(remp);
	printf("\n");
      }
    } else if (i == 8) { /* add last reduction thing */
        remp->done = 1;
	p = (LMPOL *) malloc(sizeof(LMPOL));
	p->pol = remp;
	p->label = next_label++;
	p->old = 0; p->np = 0;
	p->next = NULL;
	p->pol->maxterms = p->pol->nterms;
	mpolsetadd(&pols,p);
	update_pairs(&pairs,&pols,p,next_label-2);
    } else if (i == 9) { 
        printf("to reg: "); fflush(stdout);
	scanf("%d",&j);
	if ((j>=0) && (j<=9)) reg[j] = remp;
    } else if (i == 10) {
        printf("from reg: "); fflush(stdout);
	scanf("%d",&j);
	if ((j>=0) && (j<=9)) remp = reg[j];
    } else if (i == 11) {
        if (remp != NULL) {
	  printf("remp: ");	  
	  mpolout(remp);
	  printf("\n");
	}
        for (i=0; i<10; i++) {
	  if (reg[i] != NULL) {
	    printf("%d: ",i);
	    mpolout(reg[i]);
	    printf("\n");
	  }
	}
    }
  } 
  while ( i != 0 );
  exit(0);
}
#endif	INTERACTIVE
/* -------------------------------------------------------------------------------------- */


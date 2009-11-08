#if defined(RP3)
#include <machine/FEATURES.h>
#endif
#include <stdio.h>
#include <cthreads.h>
#if defined(RP3)
#include <kern/rp3_types.h>
#include <machine/vmparam.h>
#endif

#include "gbas.h"
#include <sys/time.h>

#if defined(RP3)
extern family_t our_family;
#endif
extern char etext;

short nvars;
char *varnames[30];
char temp[10];
short next_label=0;

#if LOCKCOUNT
	int lockcount=0;
#endif

extern cthread_debug;


int nworkers=2; /* Number of workers. By default, its value is 2 */

int co_workers=0; /* Number of co_workers for LinearCombinations */

int spy=2; /* Determines the trace : 0, 1 or 2 */

int order_exp=0, first_group; /* Determine the order on the power products. 
		            ** See multpol.h for more details.  */

int order_on_pols=0;/* determines how the polynomials are sorted in the 
		   ** structure : chronologically, upward (downward)
		   ** according to the order on monomials or by length (number
		   ** of terms).
		   */

int order_on_pairs=0; /* determine how the pairs are ordered in the set and,
		      ** therefore, which pair is selected by choose_pair.
		      */

int reduction_first=0; /* determines if the pairs (p1,p2) such that
		       ** lpp(p1) divides lpp(p2) (or vice-versa) have
		       ** priority over the other (priority to
		       ** interreduction.
		       */

int deletion=0;	/* determines if unuseful polynomials are deleted from the
		** basis.
		*/

long total_arith_cost=0;/* Approximation of the total number of
			** arithmetical operations on short integers.
			** Used to compare the efficiencies of several
			** policies. 
			** See the sequential program for more details.
			*/


extern int alloc_policy;/* Determines if we use the standard malloc and free
			  ** (alloc_policy=0) or our own (alloc_policy=1),
			  ** or a powers-of-two policy with parallel data.
			  ** standard is powers-of-two,sequential.
			  ** 1 is first-fit,parallel (slower)
			  ** 2 is powers-of-two, parallel.
			  */

int redgcd=0;	/* Determines how often apolynomial is divided by the 
		** gcd of its coeeficient.
		** If redgcd = 0, then the division is only done at the end.
		** If redgcd = 1, then the division is performed after each
		** reduction step.
		*/

int allow_same_lpp=0; /* Determines if we allow (1) or not (0) the introduction
		      ** of polynomials with same leading power product in
		      ** the basis. Trade-off : number of times the set of
		      ** pairs is recomputed / amount of concurrency.
		      */

int polynomial_rlimit= -1; /* Immediately terminate a partial computation which
			  ** produces this number of polynomials.
			  */

int pair_input_flag=0; /* Indicates that a list of pairs will be supplied */
int pair_input_size=0;
int *pair1,*pair2;

int zero_poly_count=0; /* Used to track how many reductions have produced the
			  zero polynomial.  When more than threshold (=2) have
			  been seen in a row, pull everything up to the coarse
			  level */

int zero_poly_threshold=100000; /* The limit for reporting zero_poly_count */

struct timeval start_time;	/* For elapsed time computation. */
					
main(argc,argv)

  int argc;
  char *argv[];

{

  MPOLSET s;
  LMPOL *p;
  int i,ans;

  int name=0;

  char *sarg;

  int console_input=1; /* flag set to 1 if the standard input is the console */
  
  cthread_init();
  
  cthread_set_data(cthread_self(),(char *)&name);


/* Search the arguments and set the correct flags. */

  while ((--argc > 0) && ((*++argv)[0] == '-'))
	for (sarg = argv[0]+1; *sarg != '\0'; sarg++)
		switch (*sarg) {
		case 'a': 		/* dynamic allocation of memory */
		   sarg++;
		   sscanf(sarg,"%d",&alloc_policy);
		   while ((*sarg >=48) && (*sarg <=57)) sarg++;
		   sarg--;
		   break;
 		case 'c':
 			sarg++;
 			sscanf(sarg,"%d",&co_workers);
 			if (co_workers<0) co_workers=0;
 			while ((*sarg >= 48) && (*sarg <= 57)) sarg++;
 			sarg--;
			break;
		case 'd':
		   sarg++;
		   sscanf(sarg,"%d",&deletion);
		   while ((*sarg >=48) && (*sarg <=57)) sarg++;
		   sarg--;
		   break;
		case 'g':
		   sarg++;
		   sscanf(sarg,"%d",&redgcd);
		   while ((*sarg >=48) && (*sarg <=57)) sarg++;
		   sarg--;
		   break;
		case 'h': print_options(); exit(0);
		case 'l':
		   sarg++;
		   sscanf(sarg,"%d",&allow_same_lpp);
		   while ((*sarg >=48) && (*sarg <=57)) sarg++;
		   sarg--;
		   break;
		case 'o':
		   sarg++;
		   switch(*sarg) {
			case 'l' : break;
		 	case 't' : 
				sarg++;
				switch (*sarg) {
					case 'l' : order_exp = 1; break;
					case 'r' : order_exp = 2; break;
					default  :
					   fprintf(stderr,
						"Order not available.\n");
					   exit(0);
					   break;
				};
				break;
			case 'd' :
				sarg++;
				sscanf(sarg,"%d",&first_group);
				while ((*sarg >=48) && (*sarg <=57)) sarg++;
				switch(*sarg) {
					case 'r' : order_exp = 3; break;
					default  :
					   fprintf(stderr,
						"Order not available.\n");
					   exit(0);
					   break;
				};
				break;
			default :
				fprintf(stderr,"Order not available.\n");
				exit(0);
				break;
		   };
		   break;
		case 'p':
		   sarg++;
		   switch(*sarg) {
		 	case 'd' : 
				sarg++;
				switch (*sarg) {
					case 'o' : 
					    sarg++;
					    switch (*sarg) {
						  case 'a':
							order_on_pairs=0;
							break;
						  case 'd':
							order_on_pairs=1;
							break;
						  default:
							fprintf(stderr,
					    "Order on pairs not available.\n");
							exit(0);
							break;
					    };
					    break;
					default : 
						fprintf(stderr,
					    "Order on pairs not available.\n");
						exit(0);
						break;
				};
				break;
			case 'o' :
				sarg++;
				switch (*sarg) {
					  case 'a':
						order_on_pairs=2;
						break;
					  case 'd':
						order_on_pairs=3;
						break;
					  default:
						fprintf(stderr,
					    "Order on pairs not available.\n");
						exit(0);
						break;
				};
				break;
			default : 
				fprintf(stderr,
					    "Order on pairs not available.\n");
				exit(0);
				break;
		   };
		   break;
		case 'r':	/* determines if the interreductions are
				** performed first.
				*/
		   sarg++;
		   sscanf(sarg,"%d",&reduction_first);
		   while ((*sarg >=48) && (*sarg <=57)) sarg++;
		   sarg--;
		   break;
	
		case 's':	/* determines how the polynomials */
		   sarg++;	/* are listed in the structure.   */
		   switch(*sarg) {
			case 'c' : break;
			case 'd' : order_on_pols=1; break;
			case 'u' : order_on_pols=2; break;
			case 'l' : order_on_pols=3; break;
			default  : 
			   fprintf(stderr,"Warning : unknown option to sort polynomials : %c\n",sarg);
			   break;
		   };
		 case 't':
		   sarg++;
		   sscanf(sarg,"%d",&zero_poly_threshold);
		   while ((*sarg >= 48) && (*sarg <= 57)) sarg++;
		   sarg--;
		   break;
		case 'v':
		   sarg++;
		   sscanf(sarg,"%d",&spy);
		   while ((*sarg >=48) && (*sarg <=57)) sarg++;
		   sarg--;
		   break;
		case 'w':
			sarg++;
			sscanf(sarg,"%d",&nworkers);
			if (nworkers<1) nworkers=1;
			if (nworkers + co_workers > MAX_WORKERS)
				{
				fprintf(stderr,
				     "Warning: too many workers specified. ");
				nworkers = MAX_WORKERS - co_workers;
				fprintf(stderr,"Truncated to %d.\n",nworkers);
				};
			while ((*sarg >= 48) && (*sarg <= 57)) sarg++;
			sarg--;
			break;
		 case 'x':
		   sarg++;
		   sscanf(sarg,"%d",&polynomial_rlimit);
		   while ((*sarg >= 48) && (*sarg <= 57)) sarg++;
		   sarg--;
		   break;
		 case 'y':		   
		   sarg++;
		   sscanf(sarg,"%d",&pair_input_flag);
		   while ((*sarg >= 48) && (*sarg <= 57)) sarg++;
		   sarg--;
		   break;
		default:
			fprintf(stderr, 
				"fast grobner: illegal option %c\n",*sarg);
			argc = 0;
			break;
		};

#if defined(RP3)
        if (family_create(thread_self(),&our_family)) {
                 fprintf(stderr,"Can't make the family.\n");
                 exit(1);
	}
        if (processor_allocate(our_family,nworkers+co_workers,
                               nworkers + co_workers,1) !=
                nworkers + co_workers) {
             fprintf(stderr,"Can't allocate processors.\n");
             exit(1);
	}
        if (thread_bind(thread_self())) {
            fprintf(stderr,"Something's wrong binding initial thread\n");
            exit(1);
	}
#if 1
        if (vm_set_attributes(task_self(), USRTEXT, &etext-USRTEXT,
                VM_ATTR_REPLICATED | VM_ATTR_CACHEABLE,
                PORT_NULL) != KERN_SUCCESS) 
	{
                fprintf(stderr,"Can't set code attributes\n");
                exit(1);
	}
#endif
        {
	  char *i,j; /* touch all text pages */

	  for ( i = USRTEXT; i < &etext; i += 16384 ) j += *i;
	  printf("Checksum %d\n",(int)j);
	}
        if (vm_set_attributes(task_self(), USRDATA, sbrk(0)-USRDATA, 
                VM_ATTR_NONE, PORT_NULL) != KERN_SUCCESS)
/*
	  VM_ATTR_LOCAL | VM_ATTR_CACHEABLE,thread_self()) != KERN_SUCCESS)
*/
	{
                fprintf(stderr,"Can't set data attributes\n");
                exit(1);
	}
#if 0
        if (vm_set_attributes(task_self(), &pols, sizeof(MPOLSET)+
                              sizeof(PAIRSET), VM_ATTR_NONE, PORT_NULL)
            != KERN_SUCCESS)
	{
            fprintf(stderr,"Can't set polset and pairset\n");
            exit(1);
	}
#endif 0
#endif defined(RP3)
  init_order_exp(); 
  init_alloc();

  if (polynomial_rlimit > -1) console_input = 0;

  {
    int i;

    if (pair_input_flag) {
      if (console_input) printf("Pairset size ? : ");
      scanf("%hd",&pair_input_size);
      if (console_input) printf("Pairs ? : ");
      pair1 = (int *) malloc(pair_input_size * sizeof(int));
      pair2 = (int *) malloc(pair_input_size * sizeof(int));
      for (i=pair_input_size-1; i>=0; i--) {
        scanf("%hd %hd",&pair1[i],&pair2[i]);
      }
    }
  }
  if (console_input) printf("Number of variables ? : ");
  scanf("%hd",&nvars);
  if (console_input) printf("Variables ? : ");
  for (i=0;i<nvars;i++){
	scanf("%s",temp);
	varnames[i]=malloc(sizeof(char)*(unsigned)strlen(temp));
	strcpy(varnames[i],temp);
  };

  p = (LMPOL *)malloc(sizeof(LMPOL));
  p->pol = (MPOL *)malloc(sizeof(MPOL));
  MPOLINIT(p->pol);
  MPOLSETINIT(&s);

  if (console_input) printf("Enter the basis of polynomials :\n");
  if (console_input) 
      printf("Polynomial %d : ",(int)s.npols+1);
  while ( (ans = mpolin(p->pol) ) !=1 )
	if (ans==0){
		p->label=s.npols+1;
		p->old=0;
		p->np=0;
		p->next = NULL;
		mpolsetadd(&s,p);
		p = (LMPOL *)malloc(sizeof(LMPOL));
		p->pol = (MPOL *)malloc(sizeof(MPOL));
		MPOLINIT(p->pol);
		if (console_input)
                     printf("Polynomial %d : ",(int)s.npols+1);
	};
  if (console_input) printf("\n\n");

  if (console_input) {
  printf("Admissible order : ");
  switch (order_exp){
	case 0 : printf("lexicographic.\n"); break;
	case 1 : printf("total degree refined by lexicographic.\n"); break;
	case 2 : printf("total degree refined by reverse lexicographic.\n");
			break;
	case 3 : printf("non primitive order\n"); break;
  };
  printf("Number of processors : %d\n",nworkers);
  printf("Number of co_workers : %d\n",co_workers);
  printf("Dynamic allocation of memory : ");
  switch (alloc_policy){
	case 0 : printf(" standard.\n");break;
	case 1 : printf(" mine (first-fit).\n");break;
	case 2 : printf(" power-of-2 (parallel).\n");break;
  };
  printf("\n\n");
  }

  next_label=s.npols+1;
  gettimeofday(&start_time,NULL);
  gbasis(&s);
}; 



print_options(){
	printf("Different options : \n\n");

	printf("Option a : selects an algorithm for dynamic allocation of memory.\n");
	printf("           -a0 uses the standard allocation given in the cthreads package\n");
	printf("             (by Cooper and Draves, Carnegie-Mellon University)\n");
	printf("             This is the default\n");
	printf("           -a1 uses a different algorithm where each processor maintains\n");
	printf("             its own stack of free blocks. For a number of processors\n");
	printf("             greater than 3, this is generally more efficient.\n");
	printf("           -a2 uses the standard powers-of-two allocator, with multiple\n");
	printf("             free pools for each processor\n\n");

	printf("Option c : co-workers.  Specifies the number of processors to assign to\n");
	printf("           fine-grain parallelism.  Co-workers are split evenly among\n");
	printf("           the workers.\n\n");

	printf("Option d : d1 for immediate deletion of polynomials form the basis\n");
	printf("           d0 deletion delayed until final reduction (default)\n\n");
	printf("Option g : g0 for no cancellation of common factors in the coefficients\n");
	printf("            of the polynomials (except at the final reduction),\n");
	printf("	   g1 for immediate cancellation (Not supported for fine-grain).\n\n");

	printf("Option h : prints this message\n\n");

	printf("Option l : allows (l1) or not (l0) the introduction of several\n");
	printf("            polynomials with the same leading power product in\n");
	printf("            the basis.\n\n");

	printf("Option o : defines the order on the monomials :\n");
	printf("		- ol : pure lexicographic.\n");
	printf("		- otl : total degree ordering refined by\n");
	printf("			the pure lexicographic ordering\n");
	printf("		- otr : total degree ordering refined by\n");
	printf("			the reverse lexicographic ordering\n");
	printf("		- od : the variables are split in two groups.\n");
	printf("			The variables in the first group are\n");
	printf("			the x first variables, where x is the\n");
	printf("			number appearing after the d. Two monomials\n");
	printf("			are first compared according to the variables\n");
	printf("			of the first group (the chosen order on \n");
	printf("			these variables is specified by a last\n");
	printf("			letter : r for reverse lexicographic order\n");
	printf("			Ties are broken by considering the order\n");
	printf("			specified by the last letter on the second\n");
	printf("			set of variables.\n");
	printf("			Example : -od4r uses the reverse lexicographic\n");
	printf("			on the monomials obtained by substituting\n");
	printf("			1 for each variable but the 4 first ones.\n");
	printf("			If there is a tie, then the two monomials\n");
	printf("			obtained by substituting 1 for the first 4\n");
	printf("			variables are compared according to the\n");
	printf("			reverse lexicographic order.\n\n");
	
	printf("Option p : specifies in which order the pairs are considered. All\n");
	printf("            the proposed order so far are based on the least common\n");
	printf("            multiple of the leading power products of the two members\n");
	printf("            of the pair. The different choice are :\n");
	printf("	- pdoa : the one with smallest degree is chosen. If several\n");
	printf("                have same degree, the smallest one w.r.t. the\n");
	printf("                admissible order is chosen.\n");
	printf("        - pdod : highest degree, ties broken in the favor of the\n");
	printf("                highest w.r.t. the admissible order.\n");
	printf("        - poa : smallest w.r.t. the admissible order.\n");
	printf("        - pod : highest w.r.t. the admissible order.\n\n");

	printf("Option r : specifies if the pairs (p1,p2) such that the leading\n");
	printf("            power product of p1 divides the leading power product\n");
	printf("            of p2 have (r1) or do not have (r0) priority on the other\n");
	printf("            pairs.\n\n");

	printf("Option s : specifies in which order the polynomials of the basis are tried\n");
	printf("            to reduce another polynomial.\n");
	printf("      - sc : chronological order(Default). \n");
	printf("      - sd : descending order. The polynomials whose leading power\n");
	printf("             product is the first fot the underlying order is tried first\n");
	printf("             and so on.\n");
	printf("      - su : ascending order. \n");
	printf("      - sl : order by length. The polynomial with the smaller number\n");
	printf("             of monomials is tried first. And so on.\n\n");

	printf("Option t : sets the zero polynomial threshold.  An informational\n");
        printf("           message will be produced.\n\n");
	printf("Option v : specifies the level of trace.\n");
	printf("           (0 : minimum trace, 4 : maximum trace)\n\n");

	printf("Option w : specifies the number of processors (default 2)\n\n");
	
	printf("Option x : sets early termination after a fixed number of reductions\n\n");
	
	printf("Option y : Accepts an additional input parameter, which limits the\n");
	printf("           size of the set of pairs\n\n");
};

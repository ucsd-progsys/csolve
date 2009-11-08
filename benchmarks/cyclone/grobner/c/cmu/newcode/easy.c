#define	out

#include <stdio.h>
#include <sys/time.h>
#include "cmump.h"
#include "multpol.h"
#include "gbas.h"

short nvars;
char *varnames[30];
int order_exp=1, first_group=0;
int zero_poly_count = 0;
int redgcd = 0;
int order_on_pols=0;
int order_on_pairs=0;
int reduction_first=0;
int deletion=1;
int allow_same_lpp=0;

int spy = 0;
extern float timerdiff();

char *galloc(n)
int n;
{
	char *t;
	if((t=(char*)malloc(n))==NULL){
		fprintf(stderr,"malloc failed\n");
		exit(-1);
	}
	return t;
}

void main(argc,argv)
int argc; char **argv;
{
	double maxpairsetsize = 0.0;
	double addcnt = 0.0;
	int pflag=0, oflag=0, aflag=0;
	/* print action, old basis, answer output */
	struct itimerval oldt, midt, newt;
	MPOLSET s, pols;
	PAIRSET pairs;
	LMPOL *p, *v, *w;
	int vn, npolyin, cp;

	for(;argc>1;argc--){
		switch(argv[argc-1][1]){
		case 'p':
			pflag = 1;
			break;
		case 'o':
			oflag = 1;
			break;
		case 'a':
			aflag = 1;
			break;
		}
	}
	printf("p%d o%d a%d\n", pflag, oflag, aflag);

	init_order_exp(); /* total degree refined by lex */

	/* get variables */
#ifdef out
	printf("# vars: ");
#endif
	scanf("%hd",&nvars);
#ifdef out
	printf("variables: ");
#endif
	for(vn=0;vn<nvars;vn++){
		char tbuf[32];
		scanf("%s",tbuf);
		varnames[vn] = (char*)galloc(sizeof(char)*(strlen(tbuf)+1));
		strcpy(varnames[vn],tbuf);
	}

	MPOLSETINIT(&s);

	/* get input poly set */
#ifdef out
	printf("# input poly: ");
#endif
	scanf("%d", &npolyin);
	for(cp=0;cp<npolyin;cp++){
		p = (LMPOL *)galloc(sizeof(LMPOL));
		p->pol = (MPOL *)galloc(sizeof(MPOL));
		MPOLINIT(p->pol);
		do{
#ifdef out
			printf("\n[%d] ",(int)s.npols+1);
#endif
		}while(mpolin(p->pol)!=0);
		p->label=s.npols+1;
		p->old=0;
		p->np=0;
		p->next = NULL;
		mpolsetadd(&s,p);
	}
#ifdef out
	printf("\n");
#endif

	/* form poly set pols and pair set pairs */
	MPOLSETINIT(&pols);
	PAIRSETINIT(&pairs);
	v = s.pols;
	while(v!=NULL){
		w = (LMPOL*) galloc(sizeof(LMPOL));
		*w = *v;
		w->pol = (MPOL*) galloc(sizeof(MPOL));
		MPOLINIT(w->pol);
		mpolcopy(v->pol,w->pol);
		w->next = NULL;
		w->pol->done = 1;
		mpolsetadd(&pols,w);
		if(pols.npols>1)
			update_pairs(&pairs,&pols,w);
		v = v->next;
	}

	/* start timer */
	timerinit(ITIMER_VIRTUAL);
	getitimer(ITIMER_VIRTUAL,&oldt);

	/* start cranking */
	while(pairs.npairs>0){
		LMPOL *new;
		new = (LMPOL*)galloc(sizeof(LMPOL));
		new->pol = (MPOL*)galloc(sizeof(MPOL));

		choose_pair(&pairs, &v, &w);
		if(pflag){
			printf("chosen pair: ");
			mpolout(v->pol); printf(" and   ");
			mpolout(w->pol); printf("\n");
		}

		/* compute s poly */
		spolfast(v->pol,w->pol,new->pol);
		if(pflag){
			printf("s-poly: "); mpolout(new->pol); printf("\n");
		}

		/* reduce the s-poly */
		if(oflag)
			new->pol = (MPOL*) redfast2(new->pol, s.pols);
		else
			new->pol = (MPOL*) redfast2(new->pol, pols.pols);

		if(pflag){
			printf("reduced poly: "); mpolout(new->pol); printf("\n\n");
		}

		/* add to basis */
		if(new->pol->nterms!=0){
			new->pol->done = 1;
			new->old = 0;
			new->np = 0;
			new->next = NULL;
			new->pol->maxterms = new->pol->nterms;
			mpolsetadd(&pols,new);
			update_pairs(&pairs,&pols,new);
			if(pairs.npairs>maxpairsetsize)
				maxpairsetsize = (double)pairs.npairs;
			addcnt = addcnt  +  1.0;

		}
	}

	getitimer(ITIMER_VIRTUAL,&midt);

	eliminate_redundant_end(&pols);

	/* read timers */
	getitimer(ITIMER_VIRTUAL,&newt);


	/* output the basis */
	if(aflag)
		for(v=pols.pols;v!=NULL;v=v->next){
			mpolout(v->pol);
			printf("\n");
		}

	/* output statistics */
	fprintf(stderr,"\n\n%.5f %.5f\n",(float)timerdiff(oldt,newt),(float)timerdiff(midt,newt));
	printf("p%.0f b%.0f\n", maxpairsetsize, addcnt);

	/* khel khotom poisha hojom */
	exit(0);
}


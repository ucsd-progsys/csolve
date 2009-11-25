/* --------------------- includes --------------------------------- */
#include <cm/cmmd.h>
#include <cm/timers.h>
#include <cmam/cmam.h>

#include "cmump.h"
#include "multpol.h"
#include "gbas.h"
#include "boot-2.h"

/* -------------------------- globals ----------------------------------- */

int reduction_first=0;
int order_on_pairs=0;
int redgcd = 0;

#if 0
int order_exp=1;	/* deg */

short nvars;
char *varnames[100];
int first_group=0;
int zero_poly_count = 0;
int redgcd = 0;
int order_on_pols=0;
int deletion=1;
int allow_same_lpp=0;
int spy = 0;

extern float itimerread();

polsettype s, pols;
#endif

extern polsettype s, pols;
extern int zerocnt, addcnt;

double
	maxRedTime=0.0,
	minRedTime=1e30,
	totRedTime=0;

polpairsettype pairs;

static double TimeInSec(int t)
{ CM_TIME ct = {0, t}; return CM_ni_time_in_sec(ct); }


/* ------------------------------- main ---------------------------------- */


void splice()
{
	MPOL *p, *v, *w;
	int vn, npolyin, cp;
	float oldt, newt;
	int pidx=0;

	init_order_exp();

	{
		MPOL *y;
		pols.npols = 0;
		pairs.npairs = 0;
		y = s.polp;
		while ( y ) {
			MPOL *z;
			z = y;
			y = z->next;
			z->next = 0;
			new_mpolsetadd ( &pols, z );
			if ( pols.npols > 1 )
				new_update_pairs ( &pairs, &pols, z );
		}
	}

	if (0){
		MPOL *s; char ps[65536];
		sprintf ( ps, "Start Basis:\n" );
		for ( s = pols.polp; s; s = s->next ) {
			Smpolout ( ps, s );
			sprintf ( ps+strlen(ps), "\n" );
		}
		hprintf ( ps );
	}

	/* start timer */
	CMMD_node_timer_clear ( _LOST_TIMER_ );
	CMMD_node_timer_clear ( _LOST_TIMER_ );
	CMMD_node_timer_start ( _LOST_TIMER_ );

	/* start cranking */

	while(pairs.npairs>0){
		MPOL *new;

		new_choose_pair(&pairs, &v, &w);

		/* compute s poly */
		new = (MPOL*)galloc(sizeof(MPOL));
		MPOLINIT ( new );
		spolfast(v,w,new);

		/* reduce the s-poly */
		new = (MPOL*) new_redfast2(new, pols.polp);
		/* reduce all possible terms : Wed Dec  2 16:11:31 PST 1992
		total ( new, &pols, new);
		*/

		/* add to basis */
		if(new->nterms!=0){

			if(0){
				char npa[1024];
				sprintf(npa,"add ");
				Smpolout(npa+strlen(npa),new);
				sprintf(npa+strlen(npa),"\n");
				hprintf(npa);
			}
			new->owner = 0;
			new->ppid = pidx++;
			new->next = NULL;
			new->compact = 0;

			new_mpolsetadd(&pols,new);
			new_update_pairs(&pairs,&pols,new);

			addcnt = addcnt  +  1;
		}
		else {
			gfree ( new );
			zerocnt = zerocnt + 1;
		}
	}

	/* stop timer */
	CMMD_node_timer_stop ( _LOST_TIMER_ );
	/* output statistics */
	{
		char timestr[255];
		sprintf(timestr,"t%f a%d z%d totR%d maxR%d minR%d P%d\n",
			CMMD_node_timer_busy(_LOST_TIMER_), addcnt, zerocnt,
			(int)(1e6*totRedTime),
			(int)(1e6*maxRedTime),
			(int)(1e6*minRedTime),
			(int)(totRedTime/maxRedTime));
		hprintf(timestr);
	}
	if(1){
		char redstr[1024]; MPOL *pv; int cnt;
		int maxStageTime=0, totStageTime=0;

		redstr[0] = 0;
		for ( pv = pols.polp, cnt=0; pv; pv = pv->next,cnt++ ){
			if ( maxStageTime < pv->owner )
				maxStageTime = pv->owner;
			totStageTime += pv->owner;
/*
			sprintf ( redstr+strlen(redstr), "%d\n", pv->owner );
*/
		}
		sprintf( redstr, "Pipeline Stage%.0f MaxPar%f Eff%f\n",
			(float)maxStageTime,
			(float)totStageTime/(float)maxStageTime,
			(float)totStageTime/(float)maxStageTime/(float)pols.npols );
		hprintf(redstr);
	}

	if(0) {
		int w;
		char out[100000];

		out[0] = 0;
		sprintf ( out+strlen(out), "\n%d\n", (int)nvars);
		for ( w=0; w<(int)nvars; w++ )
			sprintf ( out+strlen(out), "%s ", varnames[w] );
		sprintf ( out+strlen(out), "\n%d\n", pols.npols );
		for(v=pols.polp;v!=NULL;v=v->next){
			Smpolout(out, v); sprintf(out+strlen(out)," ;\n");
		}
		hprintf(out);
	}

	hexit();
}

/* ---------------------- size of polys ----------- */

#define abs(x) ((x>0)?(x):(-(x)))


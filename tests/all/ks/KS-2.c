/*
 *	program:	Graph partition via Kernighan-Lin, modified
 *			Kernighan-Lin, or Kernighan-Schweikert
 *
 *	author:		Todd M. Austin
 *			ECE 756
 *
 *	date:		Thursday, February 25, 1993
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "KS.h"

/* handle special cases where both nodes are switched */
float
CAiBj(ModuleRecPtr mrA, ModuleRecPtr mrB, GFORMALS)
{
    NetPtr netNode;
    ModulePtr modNode;
    float gain = 0.0;
    float netCost;
    unsigned long module = (*mrB).module;
    validptr(&mrB->module);

    /* is mrA connected to mrB? */
    /* mrA and mrB are both un-Swapped */
    validptr(&mrA->module);
    validptr(&modules[(*mrA).module]);
    for (netNode = modules[(*mrA).module];
	 netNode != NULL;
	 netNode = (*netNode).next) {
        validptr(&netNode->next);
        validptr(&netNode->net);
        validptr(&cost[(*netNode).net]);
	netCost = cost[(*netNode).net];
	for (modNode = nets[(*netNode).net];
	     modNode != NULL;
	     modNode = (*modNode).next) {
            validptr(&modNode->next);
            validptr(&modNode->module);
	    if ((*modNode).module == module) {
		gain = gain + netCost;
	    }
	}
    }
    return gain;
}

/* swap a node out of the current group, and into a target group */
void
SwapNode(ModuleRecPtr maxPrev, ModuleRecPtr max,
	 ModuleListPtr group, ModuleListPtr swapTo, GFORMALS)
{
    ModuleRecPtr swapToTail;

    if (maxPrev == NULL) {	/* found at head of list */
        validptr(&group->head);
        validptr(&group->tail);
        validptr(&max->next);
	if ((*group).head == (*group).tail)	{ /* only one in the list */
	    (*group).head = NULL;
	    (*group).tail = NULL;
	    (*max).next = NULL;
	}
	else {
	    (*group).head = (*max).next;
	    (*max).next = NULL;
	}
    }
    else {	/* middle or end of list */
        validptr(&group->tail);
	if ((*group).tail == max)		/* end of list */
	    (*group).tail = maxPrev;
        validptr(&maxPrev->next);
        validptr(&max->next);
	(*maxPrev).next = (*max).next;
	(*max).next = NULL;
    }

    /* put max on the tail of swapTo */
    validptr(&swapTo->tail);
    // pmr: Need a witness value not on the heap
    swapToTail = (*swapTo).tail;
    if (swapToTail == NULL) {	/* empty */
#if 0
        validptr(&swapTo->head);
	(*swapTo).head = (*swapTo).tail = max;
#endif
	(*swapTo).tail = max;
	(*swapTo).head = max;
    }
    else { /* end of list */
        validptr(&(swapToTail->next));
	(*swapToTail).next = max;
	(*swapTo).tail = max;
    }
    validptr(&max->next);
    (*max).next = NULL;
}

/* incrementally update the D values in Kernighan-Lin algorithm */
void
UpdateDs(ModuleRecPtr max, Groups group, GFORMALS)
{
    NetPtr net;
    ModulePtr mod;

    /* for all nets this is connected to */
    validptr(&max->module);
    validptr(&modules[(*max).module]);
    for (net = modules[(*max).module]; net != NULL; net = (*net).next) {
        validptr(&net->next);

	/* for a modules this net is connected to */
        validptr(&net->net);
        validptr(&nets[(*net).net]);
	for (mod = nets[(*net).net]; mod != NULL; mod = (*mod).next) {
            validptr(&mod->next);

            validptr(&mod->module);
            validptr(&moduleToGroup[(*mod).module]);
	    if (moduleToGroup[(*mod).module] < SwappedToA) {
                validptr(&D[(*mod).module]);
                validptr(&cost[(*net).net]);
		if (moduleToGroup[(*mod).module] == group)
		    D[(*mod).module] = D[(*mod).module] + cost[(*net).net];
		else
		    D[(*mod).module] = D[(*mod).module] - cost[(*net).net];
	    }
	}
    }
}

/* find the best swap available and do it */
float
FindMaxGpAndSwap(GFORMALS)
{
    ModuleRecPtr mrA, mrPrevA, mrB, mrPrevB;
    ModuleRecPtr maxA, maxPrevA, maxB, maxPrevB;
    float gp, gpMax;

    gpMax = -9999999;
    maxA = maxPrevA = maxB = maxPrevB = NULL;
    for (mrA = groupA->head, mrPrevA = NULL;
	 mrA != NULL;
	 mrPrevA = mrA, mrA = (*mrA).next) {
        validptr(&mrA->next);

	for (mrB = groupB->head, mrPrevB = NULL;
	     mrB != NULL;
	     mrPrevB = mrB, mrB = (*mrB).next) {
            validptr(&mrB->next);

            validptr(&mrA->module);
            validptr(&mrB->module);
            validptr(&D[(*mrA).module]);
            validptr(&D[(*mrB).module]);
#ifdef KS_MODE
	    gp = D[(*mrA).module] + D[(*mrB).module] - CAiBj(mrA, mrB, GACTUALS);
#else /* !KS_MODE */
	    gp = D[(*mrA).module] + D[(*mrB).module] - 2*CAiBj(mrA, mrB, GACTUALS);
#endif /* !KS_MODE */
	    if (gp > gpMax) {
		gpMax = gp;
		maxA = mrA; maxPrevA = mrPrevA;
		maxB = mrB; maxPrevB = mrPrevB;
	    }
	}
    }

    /* swap the nodes out, into the swap lists */
    // assert(maxA != NULL);
    // pmr: Assumption based on in-code assert
    if (maxA == NULL) { HELL: goto HELL; }
    SwapNode(maxPrevA, maxA, groupA, swapToB, GACTUALS);
    // assert(maxB != NULL);
    // pmr: Assumption based on in-code assert
    if (maxB == NULL) { LIMBO: goto LIMBO; }
    SwapNode(maxPrevB, maxB, groupB, swapToA, GACTUALS);


    /* update the inverse mapping, these two node are now gone */
    // assert(moduleToGroup[(*maxA).module] == GroupA);
    validptr(&maxA->module);
    validptr(&moduleToGroup[(*maxA).module]);
    moduleToGroup[(*maxA).module] = SwappedToB;

    // assert(moduleToGroup[(*maxB).module] == GroupB);
    validptr(&maxB->module);
    validptr(&moduleToGroup[(*maxB).module]);
    moduleToGroup[(*maxB).module] = SwappedToA;

#ifndef KS_MODE
    /* update the Ds */
    UpdateDs(maxA, GroupA, GACTUALS);
    UpdateDs(maxB, GroupB, GACTUALS);
#endif /* !KS_MODE */

    return gpMax;
}

/* find the best point, during the last numModules/2 swaps */
float
FindGMax(unsigned long * iMax, GFORMALS)
{
    int i;
    float gMax;

    gMax = -9999999;
    *iMax = 0xffffffff;
    for (i=0; i<*numModules/2; i++) {
        validptr(&GP[i]);
	if (GP[i] > gMax) {
	    gMax = GP[i];
	    *iMax = i;
	}
    }
    return gMax;
}

/* swap groupA and groupB from [0..iMax] */
void
SwapSubsetAndReset(unsigned long iMax, GFORMALS)
{
    unsigned long i;
    ModuleRecPtr mrPrevA, mrA, mrPrevB, mrB;

    /* re-splice the lists @ iMax pointers into the lists */
    for (mrPrevA = NULL, mrA = swapToA->head,
	 mrPrevB = NULL, mrB = swapToB->head, i=0;
	 i <= iMax;
	 mrPrevA = mrA, mrA = (*mrA).next,
	 mrPrevB = mrB, mrB = (*mrB).next,
         i++) {
        // pmr: unlikely to work because it's indexed by i
        // pmr: ASSUME validptr(&mrA->next);
        // pmr: ASSUME validptr(&mrB->next);
    }

    /* must at least select one to swap, case where gMax is first */
    // assert(mrPrevA != NULL && mrPrevB != NULL);
    // assumes based on above assert
    if (mrPrevA == NULL) { DIS: goto DIS; }
    if (mrPrevB == NULL) { BED: goto BED; }

    if (mrA == NULL) {	
	/* swap entire list */
	// *groupA = *swapToA;
        groupA->head = swapToA->head;
        groupA->tail = swapToA->tail;
	// *groupB = *swapToB;
        groupB->head = swapToB->head;
        groupB->tail = swapToB->tail;
    }
    else {
	/* splice the lists */
        validptr(&mrPrevA->next);
	(*mrPrevA).next = mrB;
	groupA->head = swapToA->head;
	groupA->tail = swapToB->tail;

        validptr(&mrPrevB->next);
	(*mrPrevB).next = mrA;
	groupB->head = swapToB->head;
	groupB->tail = swapToA->tail;
    }

    /* reset the inverse mappings */
    for (mrA = groupA->head; mrA != NULL; mrA = (*mrA).next) {
        validptr(&mrA->next);
        validptr(&mrA->module);
        validptr(&moduleToGroup[(*mrA).module]);
	moduleToGroup[(*mrA).module] = GroupA;
    }
    for (mrB = groupB->head; mrB != NULL; mrB = (*mrB).next) {
        validptr(&mrB->next);
        validptr(&mrB->module);
        validptr(&moduleToGroup[(*mrB).module]);
	moduleToGroup[(*mrB).module] = GroupB;
    }

    /* clear the swap lists */
    swapToA->head = swapToA->tail = NULL;
    swapToB->head = swapToB->tail = NULL;
}

/* print the current groups, and their edge and net cut counts */
void
PrintResults(int verbose, GFORMALS)
{
    ModuleRecPtr mr;
    NetPtr nn;
    ModulePtr mn;
    unsigned long cuts;
    Groups grp;
    int i, netSz;
    long maxStat;
    struct {
        unsigned long total;
        unsigned long edgesCut;
        unsigned long netsCut;
    } netStats[256];


    //    fprintf(stdout, "----------------------------------------------\n");

    maxStat = -1;
    for (i=0; i<256; i++) {
        validptr(&netStats[i].total);
        validptr(&netStats[i].edgesCut);
        validptr(&netStats[i].netsCut);
	netStats[i].total = netStats[i].edgesCut = netStats[i].netsCut = 0;
    }

    /* partitions */
    if (verbose) {
        //	fprintf(stdout, "Group A:  \n");
	for (mr = groupA->head; mr != NULL; mr = (*mr).next) {
	    validptr(&mr->next);//fprintf(stdout, "%3lu ", (*mr).module+1);
            validptr(&mr->module);
        }
        //	fprintf(stdout, "\n");

        //	fprintf(stdout, "Group B:  \n");
	for (mr = groupB->head; mr != NULL; mr = (*mr).next) {
	    validptr(&mr->next); // fprintf(stdout, "%3lu ", (*mr).module+1);
            validptr(&mr->module);
        }
        //	fprintf(stdout, "\n");
    }

    /* total edge cuts */
    cuts = 0;
    for (mr = groupA->head; mr != NULL; mr = (*mr).next) {
        validptr(&mr->next);

	// assert(moduleToGroup[(*mr).module] == GroupA);

	/* for all nets on this module */
        validptr(&mr->module);
        validptr(&modules[(*mr).module]);
	for (nn = modules[(*mr).module]; nn != NULL; nn = (*nn).next) {
            validptr(&nn->next);
	    
	    netSz = 0;
            validptr(&nn->net);
            validptr(&nets[(*nn).net]);
	    for (mn = nets[(*nn).net]; mn != NULL; mn = (*mn).next) {
                validptr(&mn->next);
		netSz++;
            }
	    // assert(netSz >= 2);
            // pmr: ASSUME
            if (netSz > 255) { LALALAND: goto LALALAND; }

	    /* for all modules on this net */
	    for (mn = nets[(*nn).net]; mn != NULL; mn = (*mn).next) {
                validptr(&mn->next);

		/* only check nodes other than self, and not swapped */
                validptr(&moduleToGroup[(*mr).module]);
                validptr(&moduleToGroup[(*mn).module]);
		if (moduleToGroup[(*mr).module] != moduleToGroup[(*mn).module]) {
                    validptr(&mr->module);
		    if (verbose)
			; //fprintf(stdout, "Conn %3lu - %3lu cut.\n",
                    // (*mr).module+1, (*mn).module+1);
                    validptr(&(netStats[netSz].edgesCut));
		    netStats[netSz].edgesCut++;
		    cuts++;
		}
	    }
	}
    }
    //    fprintf(stdout, "Total edge cuts = %lu\n", cuts);

    /* total net cuts */
    cuts = 0;
    for (i=0; i<*numNets; i++) {

	netSz = 0;
        validptr(&nets[i]);
	for (mn = nets[i]; mn != NULL; mn = (*mn).next) {
            validptr(&mn->next);
	    netSz++;
        }
	// assert(netSz >= 2);
        // pmr: ASSUME
        if (netSz > 255) { HOLLYWOOD: goto HOLLYWOOD; }

        // pmr: TODO how to check that netsz is in the bounds??
        validptr(&netStats[netSz]);
        validptr(&(netStats[netSz].total));
	netStats[netSz].total++;
	if (netSz > maxStat)
	    maxStat = netSz;

        // pmr: ASSUME
        ModulePtr m = nets[i];
        if (m == NULL) { OUTERSPACE: goto OUTERSPACE; }

        validptr(&m->module);
        validptr(&(moduleToGroup[(*(nets[i])).module]));
        validptr(&m->next);
	for (grp=moduleToGroup[(*(nets[i])).module],mn = (*(nets[i])).next;
	     mn != NULL;
	     mn = (*mn).next) {
            validptr(&mn->next);
	    
	    /* only check nodes other than self, and not swapped */
            validptr(&mn->module);
            validptr(&moduleToGroup[(*mn).module]);
	    if (grp != moduleToGroup[(*mn).module]) {
		if (verbose)
		    ; // fprintf(stdout, "Net %3lu cut.\n", i+1);
		cuts++;
                validptr(&(netStats[netSz].netsCut));
		netStats[netSz].netsCut++;
		break;
	    }
	}
    }
    //    fprintf(stdout, "Total net cuts  = %lu\n", cuts);

    for (i=2; i<=maxStat; i++) {
        validptr(&netStats[i]);
        validptr(&(netStats[i].total));
        validptr(&(netStats[i].edgesCut));
        validptr(&(netStats[i].netsCut));
    }
	/* fprintf(stdout,
		"sz:%5lu     total:%5lu     edgesCut:%5lu     netsCuts:%5lu\n",
		i, netStats[i].total,
		netStats[i].edgesCut, netStats[i].netsCut); */ ;
}

int     // sm: silence warning
main(int argc, char * __attribute__ ((array)) * __attribute__ ((array)) argv)
{
    unsigned long p, iMax;
    float gMax, lastGMax;
    ModuleRecPtr mr;
    unsigned long *numNets;
    unsigned long *numModules;
    float *GP;
    float *D;
    float *cost;
    Groups *moduleToGroup;
    ModuleList *groupA;
    ModuleList *groupB;
    ModuleList *swapToA;
    ModuleList *swapToB;
    NetPtr     *modules;
    ModulePtr  *nets;
    ;

    // pmr: global inits
    numNets     = (unsigned long *)malloc(sizeof(unsigned long));
    *numNets    = 0;
    numModules  = (unsigned long *)malloc(sizeof(unsigned long));
    *numModules = 0;
    GP          = (float *)malloc(sizeof(float) * G_SZ);
    D           = (float *)malloc(sizeof(float) * G_SZ);
    cost        = (float *)malloc(sizeof(float) * G_SZ);
    moduleToGroup = (Groups *)malloc(sizeof(Groups) * G_SZ);
    groupA      = (ModuleList *)malloc(sizeof(ModuleList));
    groupB      = (ModuleList *)malloc(sizeof(ModuleList));
    swapToA     = (ModuleList *)malloc(sizeof(ModuleList));
    swapToB     = (ModuleList *)malloc(sizeof(ModuleList));
    modules     = (NetPtr *)malloc(sizeof(ModulePtr) * G_SZ);
    nets        = (ModulePtr *)malloc(sizeof(ModulePtr) * G_SZ);
    // pmr: end global inits

    /* parse argument */
    if (argc != 2) {
	; // fprintf(stderr, "Usage: KL <input_file>\n");
        ;
    PURGATORY: goto PURGATORY; // exit(1);
    }

    /* prepare the data structures */
    validptr(&argv[1]);
    ReadNetList(argv[1], GACTUALS);
    NetsToModules(GACTUALS);
    ComputeNetCosts(GACTUALS);

    // assert((*numModules % 2) == 0);

    /* initial partition */
    InitLists(GACTUALS);
    lastGMax = 0;

    /* do until we don't make any progress */
    do {

#ifndef KS_MODE
	/* compute the swap costs */
	ComputeDs(groupA, GroupA, SwappedToA, GACTUALS);
	ComputeDs(groupB, GroupB, SwappedToB, GACTUALS);
#endif /* !KS_MODE */

	/* for all pairs of nodes in A,B */
	for (p = 0; p<*numModules/2; p++) {

#ifdef KS_MODE
	    /* compute the swap costs */
	    ComputeDs(groupA, GroupA, SwappedToA, GACTUALS);
	    ComputeDs(groupB, GroupB, SwappedToB, GACTUALS);
#endif /* KS_MODE */

	    /* find the max swap opportunity, and swap */
            validptr(&GP[p]);
	    GP[p] = FindMaxGpAndSwap(GACTUALS);

	}
	/* lists should both be empty now */
	// assert(groupA->head == NULL && groupA->tail == NULL);
	// assert(groupB->head == NULL && groupB->tail == NULL);

	gMax = FindGMax(&iMax, GACTUALS);

	/* debug/statistics */
	if (lastGMax == gMax)
	    ; // fprintf(stdout, "No progress: gMax = %f\n", gMax);
	lastGMax = gMax;
	; // fprintf(stdout, "gMax = %f, iMax = %lu\n", gMax, iMax);

	if (gMax > 0.0)
	    SwapSubsetAndReset(iMax, GACTUALS);
	PrintResults(0, GACTUALS);
    } while (gMax > 0.0);	/* progress made? */

    /* all swaps rejected */
    // *groupA = *swapToB;
    groupA->head = swapToB->head;
    groupA->tail = swapToB->tail;
    for (mr = groupA->head; mr != NULL; mr = (*mr).next) {
        validptr(&mr->next);
        validptr(&mr->module);
        validptr(&moduleToGroup[(*mr).module]);
	moduleToGroup[(*mr).module] = GroupA;
    }
    // *groupB = *swapToA;
    groupB->head = swapToA->head;
    groupB->tail = swapToA->tail;
    for (mr = groupB->head; mr != NULL; mr = (*mr).next) {
        validptr(&mr->next);
        validptr(&mr->module);
        validptr(&moduleToGroup[(*mr).module]);
	moduleToGroup[(*mr).module] = GroupB;
    }

    ;

    /* all done, show results */
    PrintResults(1, GACTUALS);
#ifdef PLUS_STATS
    PrintDerefStats(stderr);
    PrintHeapSize(stderr);
#endif /* PLUS_STATS */
    exit(0);
    return 0;     // sm: silence warning
}

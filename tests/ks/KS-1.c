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
#include <stdarg.h>
#include <assert.h>

#include "KS.h"

/* read the netlist into the nets[] structure */
void
ReadNetList(char * __attribute__ ((array)) fname, GFORMALS)
{
    FILE *inFile;
    char line[BUF_LEN];
    char *tok;
    unsigned long net, dest;
    ModulePtr node, prev, head;
    unsigned long nNets;
    unsigned long nModules;
    unsigned long m;

    TRY(inFile = fopen(fname, "r"),
	inFile != NULL, "ReadData",
	"unable to open input file [%s]", /* gn: BUG!! inFile*/ fname, 0, 0,
	exit(1));

    // pmr: checking bounds for i/o here?
    TRY(fgets(line, BUF_LEN, inFile),
        /* sscanf(NTDROP(line), "%lu %lu", &nNets, &nModules) == 2 */ nondetpos(),
        "ReadData",
	"unable to parse header in file [%s]", /* sm: also BUG!! inFile*/ fname, 0, 0,
	exit(1));

    // pmr: added
    if (nNets < 0 || nModules < 0 || nNets > G_SZ || nModules > G_SZ) {
        goto HELL; // exit(1) doesn't alter the CFG
    }

    *numModules = nModules;
    *numNets    = nNets;
    // pmr: end added

    for (net = 0; net < *numNets; net++) {
	fgets(line, BUF_LEN, inFile);
	
	/* net connections for "dest" */
	dest = atol(strtok(line, " \t\n"))-1;
        // pmr: begin added
        if (dest < 0 || dest >= G_SZ) {
            goto HELL; // exit(1) doesn't alter the CFG
        }
        // pmr: end added

	/* parse out all the net module connections */
	TRY(head = prev = (Module *)malloc(sizeof(Module)),
	    prev != NULL, "ReadData",
	    "unable to allocate a module list node", 0, 0, 0,
	    exit(1));
        m = atol(strtok(NULL, " \t\n"))-1;
        // pmr: begin added
        if (m < 0 || m >= G_SZ) {
            goto HELL; // exit(1) doesn't alter the CFG
        }
        // pmr: end added
	(*prev).module = m;
        validptr(&prev->module);
	(*prev).next = NULL;
        validptr(&prev->next);
	while ((tok = strtok(NULL, " \t\n")) != NULL) {
	    TRY(node = (Module *)malloc(sizeof(Module)),
		node != NULL, "ReadData",
		"unable to allocate a module list node", 0, 0, 0,
		exit(1));
            m = atol(tok)-1;
            // pmr: begin added
            if (m < 0 || m >= G_SZ) {
                goto HELL; // exit(1) doesn't alter the CFG
            }
            // pmr: end added
            validptr(&node->module);
	    (*node).module = m;
            validptr(&node->next);
	    (*node).next = NULL;
            validptr(&prev->next);
	    (*prev).next = node;
	    prev = node;
	}
        validptr(&nets[dest]);
	nets[dest] = head;
    }

    return;

 HELL: goto HELL;
}

/* invert the previously read nets, into a module to net structure */
void
NetsToModules(GFORMALS)
{
    unsigned long net, mod;
    ModulePtr modNode;
    NetPtr netNode;

    for (mod = 0; mod<*numModules; mod++) {
        validptr(&modules[mod]);
	modules[mod] = NULL;
    }

    for (net=0; net<*numNets; net++) {
	for (modNode = nets[net]; modNode != NULL; modNode = (*modNode).next) {
	    TRY(netNode = (Net *)malloc(sizeof(Net)),
		netNode != NULL, "NetsToModules",
		"unable to allocate net list node", 0, 0, 0,
		exit(1));
            validptr(&netNode->net);
	    (*netNode).net = net;
            validptr(&netNode->next);
            validptr(&modNode->module);
            validptr(&modules[modNode->module]);
	    (*netNode).next = modules[(*modNode).module];
	    modules[(*modNode).module] = netNode;
	}
    }
}


/* compute the net edge costs, based on the weighting strategy */
void
ComputeNetCosts(GFORMALS)
{
#ifdef WEIGHTED
    ModulePtr nn;
    unsigned long count;
#endif /* WEIGHTED */
    unsigned long i;

    for (i=0; i<*numNets; i++) {
#ifndef WEIGHTED
        validptr(&cost[i]);
	cost[i] = 1.0;
#else
	count = 0;
        validptr(&nets[i]);
	for (nn = nets[i]; nn != NULL; nn = (*nn).next) {
	    count++;
            validptr(&nn->next);
        }

	cost[i] = 1.0/((float)count - 1.0);
#endif /* WEIGHTED */
    }
}

/* set up the initial groups, just split down the middle */
void
InitLists(GFORMALS)
{
    unsigned long p;
    ModuleRecPtr mr;
    unsigned long numMods;

    groupA->head = groupA->tail = NULL;
    groupB->head = groupB->tail = NULL;

    /* for all modules */
    numMods = *numModules;
    for (p = 0; p<(numMods)/2; p++) {

	/* build the group A module list */
	TRY(mr = (ModuleRec *)malloc(sizeof(ModuleRec)),
	    mr != NULL, "main",
	    "unable to allocate ModuleRec", 0, 0, 0,
	    exit(1));
        validptr(&mr->module);
	(*mr).module = p;
	if (groupA->head == NULL) {
	    /* first item */
	    groupA->head = groupA->tail = mr;
            validptr(&mr->next);
	    (*mr).next = NULL;
	}
	else {
	    /* add to tail */
            validptr(&mr->next);
	    (*mr).next = NULL;
            // pmr: ASSUME
            // validptr(&groupA->tail->next);
	    (*groupA->tail).next = mr;
	    groupA->tail = mr;
	}
        validptr(&moduleToGroup[p]);
	moduleToGroup[p] = GroupA;

	/* build the group B module list */
	TRY(mr = (ModuleRec *)malloc(sizeof(ModuleRec)),
	    mr != NULL, "main",
	    "unable to allocate ModuleRec", 0, 0, 0,
	    exit(1));
        validptr(&mr->module);
	(*mr).module = ((numMods)/2) + p;
	if (groupB->head == NULL) {
	    /* first item */
	    groupB->head = groupB->tail = mr;
            validptr(&mr->next);
	    (*mr).next = NULL;
	}
	else {
	    /* add to tail */
            validptr(&mr->next);
	    (*mr).next = NULL;
            // pmr: ASSUME
            // validptr(&groupB->tail->next);
	    (*groupB->tail).next = mr;
	    groupB->tail = mr;
	}
        validptr(&moduleToGroup[((numMods)/2) + p]);
	moduleToGroup[((numMods)/2) + p] = GroupB;
    }

    /* initially clear the swap chains */
    swapToA->head = swapToA->tail = NULL;
    swapToB->head = swapToB->tail = NULL;
}


/* compute the cost of switching every node in group to the other group */
void
ComputeDs(ModuleListPtr group, Groups myGroup, Groups mySwap, GFORMALS)
{
#ifdef KS_MODE
    // pmr: The following code is dead

    NetPtr netNode;
    ModulePtr modNode;
    ModuleRecPtr groupNode;
    unsigned long numInG, numInNet;
    ModulePtr oneInG;

    /* for all modules in group */
    validptr(&group->head);
    for (groupNode = (*group).head;
	 groupNode != NULL;
	 groupNode = (*groupNode).next) {
        validptr(&groupNode->next);

	// assert(moduleToGroup[(*groupNode).module] == myGroup);

	/* for all nets on this module, check if groupNode move unifies net */
        validptr(&groupNode->module);
        validptr(&modules[(*groupNode).module]);
	for (netNode = modules[(*groupNode).module];
	     netNode != NULL;
	     netNode = (*netNode).next) {
            validptr(&netNode->next);

	    /* look for single node nets, or single partition nets */
	    numInG = numInNet = 0;
	    oneInG = NULL;
	    /* for all modules on this net */
            validptr(&netNode->net);
            validptr(&nets[(*netNode).net]);
	    for (modNode = nets[(*netNode).net];
		 modNode != NULL;
		 modNode = (*modNode).next) {
                validptr(&modNode->next);
                validptr(&modNode->module);
                validptr(&moduleToGroup[(*modNode).module]);
		if ((moduleToGroup[(*modNode).module] == myGroup) ||
		    (moduleToGroup[(*modNode).module] == mySwap)) {
		    numInG++;
		    oneInG = modNode;
		}
		numInNet++;
	    }
	    /* single node net? */
            // pmr: Separate if because of short-circuiting in next block
            if (numInG == 1) {
                validptr(&oneInG->module);
                validptr(&groupNode->module);
            }
	    if ((numInG == 1) && ((*oneInG).module == (*groupNode).module)) {
                validptr(&D[(*groupNode).module]);
		D[(*groupNode).module] = D[(*groupNode).module] + 1;
            }
	    /* single partition net? */
	    if (numInG == numInNet) {
                validptr(&groupNode->module);
                validptr(&D[(*groupNode).module]);
		D[(*groupNode).module] = D[(*groupNode).module] - 1;
            }
	}
    }

#else /* !KS_MODE */

    float I, E;

    NetPtr netNode;
    ModulePtr modNode;
    ModuleRecPtr groupNode;

    /* for all modules in group */
    validptr(&group->head);
    for (groupNode = (*group).head;
	 groupNode != NULL;
	 groupNode = (*groupNode).next) {
        validptr(&groupNode->next);

	// assert(moduleToGroup[(*groupNode).module] == myGroup);

	/* initial conditions */
	I = E = 0.0;

	/* for all nets on this module */
        validptr(&groupNode->module);
        validptr(&modules[(*groupNode).module]);
	for (netNode = modules[(*groupNode).module];
	     netNode != NULL;
	     netNode = (*netNode).next) {
            validptr(&netNode->next);
	    
	    /* for all modules on this net */
            validptr(&netNode->net);
            validptr(&nets[(*netNode).net]);
	    for (modNode = nets[(*netNode).net];
		 modNode != NULL;
		 modNode = (*modNode).next) {
                validptr(&modNode->next);

		/* only check nodes other than self, and not swapped */
                validptr(&modNode->module);
                validptr(&groupNode->module);
                validptr(&moduleToGroup[(*modNode).module]);
		if (((*modNode).module != (*groupNode).module) &&
		    (moduleToGroup[(*modNode).module] < SwappedToA)) {
                    validptr(&netNode->net);
                    validptr(&cost[(*netNode).net]);
		    if (moduleToGroup[(*modNode).module] == myGroup)
			I = I + cost[(*netNode).net]; /* internal */
		    else
			E = E + cost[(*netNode).net]; /* external */
		}
	    }
	}
        validptr(&groupNode->module);
        validptr(&D[(*groupNode).module]);
	D[(*groupNode).module] = E - I;
    }

#endif /* !KS_MODE */

}

//! run with --manual

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <liquidc.h>

/* Generated by CIL v. 1.3.7 */
/* print_CIL_Input is true */

typedef unsigned int size_t;
struct _Net {
   struct _Net *next ;
   unsigned long net ;
};
typedef struct _Net Net;
typedef Net *NetPtr;
struct _Module {
   struct _Module *next ;
   unsigned long module ;
};
typedef struct _Module Module;
typedef Module *ModulePtr;
struct _ModuleRec {
   struct _ModuleRec *next ;
   unsigned long module ;
};
typedef struct _ModuleRec ModuleRec;
typedef ModuleRec *ModuleRecPtr;
struct _ModuleList {
   ModuleRecPtr LOC(L) head ;
   ModuleRecPtr LOC(L) tail ;
};
typedef struct _ModuleList ModuleList;
typedef ModuleList *ModuleListPtr;
enum __anonenum_Groups_1 {
    GroupA = 0,
    GroupB = 1,
    SwappedToA = 2,
    SwappedToB = 3
} ;
typedef enum __anonenum_Groups_1 Groups;
struct __anonstruct_netStats_2 {
   unsigned long total ;
   unsigned long edgesCut ;
   unsigned long netsCut ;
};
#pragma merger(0,"/tmp/cil-Njq7l947.i","")
void ReadNetList(char * ARRAY fname , unsigned long * LOC(L) numModules ,
                 unsigned long * LOC(L) numNets , float * ARRAY LOC(F) GP ,
                 float * ARRAY LOC(F) D , float * ARRAY LOC(F) cost ,
                 Groups * ARRAY moduleToGroup ,
                 ModuleList INST(L, P) * LOC(M) groupA ,
                 ModuleList INST(L, P) * LOC(M) groupB ,
                 ModuleList INST(L, P) * LOC(M) swapToA ,
                 ModuleList INST(L, P) * LOC(M) swapToB ,
                 NetPtr * ARRAY modules , ModulePtr LOC(P) * ARRAY nets ) 
{ int *inFile ;
  char line[1024] ;
  char *tok ;
  unsigned long net ;
  unsigned long dest ;
  ModulePtr node ;
  ModulePtr prev ;
  ModulePtr head ;
  unsigned long nNets ;
  unsigned long nModules ;
  unsigned long m ;
  int tmp ;
  char *tmp___0 ;
  long tmp___1 ;
  void *tmp___2 ;
  char *tmp___3 ;
  long tmp___4 ;
  void *tmp___5 ;
  long tmp___6 ;

  {
  inFile = fopen(fname, (char * __attribute__((__array__)) )"r");
  if (! ((unsigned int )inFile != (unsigned int )((int *)0))) {
    exit(1);
  }
  fgets((char * __attribute__((__array__)) )(line), 1024, inFile);
  tmp = nondetpos();
  if (! tmp) {
    exit(1);
  }
  if (nNets < 0UL) {
    goto HELL;
  } else {
    if (nModules < 0UL) {
      goto HELL;
    } else {
      if (nNets > 1024UL) {
        goto HELL;
      } else {
        if (nModules > 1024UL) {
          goto HELL;
        }
      }
    }
  }
  *numModules = nModules;
  *numNets = nNets;
  net = 0UL;
  while (net < *numNets) {
    fgets((char * __attribute__((__array__)) )(line), 1024, inFile);
    tmp___0 = strtok((char * __attribute__((__array__)) )(line), (char * __attribute__((__array__)) )" \t\n");
    int pmr_assm1 = assume (tmp___0 != (char *) 0);
    tmp___1 = atol((char * __attribute__((__array__)) )tmp___0);
    dest = (unsigned long )(tmp___1 - 1L);
    if (dest < 0UL) {
      goto HELL;
    } else {
      if (dest >= 1024UL) {
        goto HELL;
      }
    }
    tmp___2 = malloc(sizeof(Module ));
    prev = (Module *)tmp___2;
    head = prev;
    if (! ((unsigned int )prev != (unsigned int )((ModulePtr )0))) {
      exit(1);
    }
    tmp___3 = strtok((char * __attribute__((__array__)) )0, (char * __attribute__((__array__)) )" \t\n");
    int pmr_assm2 = assume (tmp___3 != (char *) 0);
    tmp___4 = atol((char * __attribute__((__array__)) )tmp___3);
    m = (unsigned long )(tmp___4 - 1L);
    if (m < 0UL) {
      goto HELL;
    } else {
      if (m >= 1024UL) {
        goto HELL;
      }
    }
    prev->module = m;
    validptr((void *)(& prev->module));
    prev->next = (struct _Module *)0;
    validptr((void *)(& prev->next));
    while (1) {
      tok = strtok((char * __attribute__((__array__)) )0, (char * __attribute__((__array__)) )" \t\n");
      if (! ((unsigned int )tok != (unsigned int )((char *)0))) {
        break;
      }
      tmp___5 = malloc(sizeof(Module ));
      node = (Module *)tmp___5;
      if (! ((unsigned int )node != (unsigned int )((ModulePtr )0))) {
        exit(1);
      }
      tmp___6 = atol((char * __attribute__((__array__)) )tok);
      m = (unsigned long )(tmp___6 - 1L);
      if (m < 0UL) {
        goto HELL;
      } else {
        if (m >= 1024UL) {
          goto HELL;
        }
      }
      validptr((void *)(& node->module));
      node->module = m;
      validptr((void *)(& node->next));
      node->next = (struct _Module *)0;
      validptr((void *)(& prev->next));
      prev->next = node;
      prev = node;
    }
    validptr((void *)(nets + dest));
    *(nets + dest) = head;
    net ++;
  }
  return;
  HELL: 
  goto HELL;
}
}
void NetsToModules(unsigned long * LOC(L) numModules , unsigned long * LOC(L) numNets , float * ARRAY LOC(F) GP ,
                   float * ARRAY LOC(F) D , float * ARRAY LOC(F) cost ,
                   Groups * ARRAY moduleToGroup ,
                   ModuleList INST(L, P) * LOC(M) groupA ,
                   ModuleList INST(L, P) * LOC(M) groupB ,
                   ModuleList INST(L, P) * LOC(M) swapToA ,
                   ModuleList INST(L, P) * LOC(M) swapToB ,
                   NetPtr * ARRAY modules , ModulePtr LOC(P) * ARRAY nets )
{ unsigned long net ;
  unsigned long mod ;
  ModulePtr modNode ;
  NetPtr netNode ;
  void *tmp ;

  {
  mod = 0UL;
  while (mod < *numModules) {
    validptr((void *)(modules + mod));
    *(modules + mod) = (Net *)0;
    mod ++;
  }
  net = 0UL;
  while (net < *numNets) {
    modNode = *(nets + net);
    while ((unsigned int )modNode != (unsigned int )((ModulePtr )0)) {
      tmp = malloc(sizeof(Net ));
      netNode = (Net *)tmp;
      if (! ((unsigned int )netNode != (unsigned int )((NetPtr )0))) {
        exit(1);
      }
      validptr((void *)(& netNode->net));
      netNode->net = net;
      validptr((void *)(& netNode->next));
      validptr((void *)(& modNode->module));
      validptr((void *)(modules + modNode->module));
      netNode->next = *(modules + modNode->module);
      *(modules + modNode->module) = netNode;
      modNode = modNode->next;
    }
    net ++;
  }
  return;
}
}
void ComputeNetCosts(unsigned long * LOC(L) numModules , unsigned long * LOC(L) numNets , float * ARRAY LOC(F) GP ,
                     float * ARRAY LOC(F) D , float * ARRAY LOC(F) cost ,
                     Groups * ARRAY moduleToGroup ,
                     ModuleList INST(L, P) * LOC(M) groupA ,
                     ModuleList INST(L, P) * LOC(M) groupB ,
                     ModuleList INST(L, P) * LOC(M) swapToA ,
                     ModuleList INST(L, P) * LOC(M) swapToB ,
                     NetPtr * ARRAY modules , ModulePtr LOC(P) * ARRAY nets )
{ unsigned long i ;

  {
  i = 0UL;
  while (i < *numNets) {
    validptr((void *)(cost + i));
    *(cost + i) = (float )1.0;
    i ++;
  }
  return;
}
}
void InitLists(unsigned long * LOC(L) numModules , unsigned long * LOC(L) numNets , float * ARRAY LOC(F) GP ,
               float * ARRAY LOC(F) D , float * ARRAY LOC(F) cost ,
               Groups * ARRAY moduleToGroup ,
               ModuleList INST(L, P) * LOC(M)groupA ,
               ModuleList INST(L, P) * LOC(M) groupB ,
               ModuleList INST(L, P) * LOC(M) swapToA ,
               ModuleList INST(L, P) * LOC(M) swapToB ,
               NetPtr * ARRAY modules ,
               ModulePtr LOC(P) * ARRAY nets )
{ unsigned long p ;
  ModuleRecPtr mr ;
  unsigned long numMods ;
  ModuleRecPtr tmp ;
  ModuleRecPtr tmp___0 ;
  void *tmp___1 ;
  ModuleRecPtr tmp___2 ;
  void *tmp___3 ;
  ModuleRecPtr tmp___4 ;
  ModuleRecPtr tmp___5 ;
  ModuleRecPtr tmp___6 ;

  {
  tmp = (ModuleRec *)0;
  groupA->tail = tmp;
  groupA->head = tmp;
  tmp___0 = (ModuleRec *)0;
  groupB->tail = tmp___0;
  groupB->head = tmp___0;
  numMods = *numModules;
  p = 0UL;
  while (p < numMods / 2UL) {
    tmp___1 = malloc(sizeof(ModuleRec ));
    mr = (ModuleRec *)tmp___1;
    if (! ((unsigned int )mr != (unsigned int )((ModuleRecPtr )0))) {
      exit(1);
    }
    validptr((void *)(& mr->module));
    mr->module = p;
    if ((unsigned int )groupA->head == (unsigned int )((ModuleRecPtr )0)) {
      tmp___2 = mr;
      groupA->tail = tmp___2;
      groupA->head = tmp___2;
      validptr((void *)(& mr->next));
      mr->next = (struct _ModuleRec *)0;
    } else {
      validptr((void *)(& mr->next));
      mr->next = (struct _ModuleRec *)0;
      (groupA->tail)->next = mr;
      groupA->tail = mr;
    }
    validptr((void *)(moduleToGroup + p));
    *(moduleToGroup + p) = (enum __anonenum_Groups_1 )0;
    tmp___3 = malloc(sizeof(ModuleRec ));
    mr = (ModuleRec *)tmp___3;
    if (! ((unsigned int )mr != (unsigned int )((ModuleRecPtr )0))) {
      exit(1);
    }
    validptr((void *)(& mr->module));
    mr->module = numMods / 2UL + p;
    if ((unsigned int )groupB->head == (unsigned int )((ModuleRecPtr )0)) {
      tmp___4 = mr;
      groupB->tail = tmp___4;
      groupB->head = tmp___4;
      validptr((void *)(& mr->next));
      mr->next = (struct _ModuleRec *)0;
    } else {
      validptr((void *)(& mr->next));
      mr->next = (struct _ModuleRec *)0;
      (groupB->tail)->next = mr;
      groupB->tail = mr;
    }
    validptr((void *)(moduleToGroup + (numMods / 2UL + p)));
    *(moduleToGroup + (numMods / 2UL + p)) = (enum __anonenum_Groups_1 )1;
    p ++;
  }
  tmp___5 = (ModuleRec *)0;
  swapToA->tail = tmp___5;
  swapToA->head = tmp___5;
  tmp___6 = (ModuleRec *)0;
  swapToB->tail = tmp___6;
  swapToB->head = tmp___6;
  return;
}
}
void ComputeDs(ModuleList INST(L, P) * LOC(M) group ,
               Groups myGroup , Groups mySwap , unsigned long * LOC(L) numModules ,
               unsigned long * LOC(L) numNets , float * ARRAY LOC(F) GP , float * ARRAY LOC(F) D ,
               float * ARRAY LOC(F) cost , Groups * ARRAY moduleToGroup ,
               ModuleList INST(L, P) * LOC(M) groupA ,
               ModuleList INST(L, P) * LOC(M) groupB ,
               ModuleList INST(L, P) * LOC(M) swapToA ,
               ModuleList INST(L, P) * LOC(M) swapToB ,
               NetPtr * ARRAY modules , ModulePtr LOC(P) * ARRAY nets )
{ float I ;
  float E ;
  NetPtr netNode ;
  ModulePtr modNode ;
  ModuleRecPtr groupNode ;

  {
  validptr((void *)(& group->head));
  groupNode = group->head;
  while ((unsigned int )groupNode != (unsigned int )((ModuleRecPtr )0)) {
    validptr((void *)(& groupNode->next));
    E = (float )0.0;
    I = E;
    validptr((void *)(& groupNode->module));
    validptr((void *)(modules + groupNode->module));
    netNode = *(modules + groupNode->module);
    while ((unsigned int )netNode != (unsigned int )((NetPtr )0)) {
      validptr((void *)(& netNode->next));
      validptr((void *)(& netNode->net));
      validptr((void *)(nets + netNode->net));
      modNode = *(nets + netNode->net);
      while ((unsigned int )modNode != (unsigned int )((ModulePtr )0)) {
        validptr((void *)(& modNode->next));
        validptr((void *)(& modNode->module));
        validptr((void *)(& groupNode->module));
        validptr((void *)(moduleToGroup + modNode->module));
        if (modNode->module != groupNode->module) {
          if ((unsigned int )*(moduleToGroup + modNode->module) < 2U) {
            validptr((void *)(& netNode->net));
            validptr((void *)(cost + netNode->net));
            if ((unsigned int )*(moduleToGroup + modNode->module) == (unsigned int )myGroup) {
              I += *(cost + netNode->net);
            } else {
              E += *(cost + netNode->net);
            }
          }
        }
        modNode = modNode->next;
      }
      netNode = netNode->next;
    }
    validptr((void *)(& groupNode->module));
    validptr((void *)(D + groupNode->module));
    *(D + groupNode->module) = E - I;
    groupNode = groupNode->next;
  }
  return;
}
}
#pragma merger(0,"/tmp/cil-CBuNWwqL.i","")
float CAiBj(ModuleRecPtr LOC(P) mrA , ModuleRecPtr LOC(P) mrB ,
            unsigned long * LOC(L) numModules , unsigned long * LOC(L) numNets ,
            float * ARRAY LOC(F) GP , float * ARRAY LOC(F) D ,
            float * ARRAY LOC(F) cost , Groups * ARRAY moduleToGroup ,
            ModuleList INST(L, P) * LOC(M) groupA ,
            ModuleList INST(L, P) * LOC(M) groupB ,
            ModuleList INST(L, P) * LOC(M) swapToA ,
            ModuleList INST(L, P) * LOC(M) swapToB ,
            NetPtr * ARRAY modules , ModulePtr LOC(P) * ARRAY nets )
{ NetPtr netNode ;
  ModulePtr modNode ;
  float gain ;
  float netCost ;
  unsigned long module ;

  {
  gain = (float )0.0;
  module = mrB->module;
  validptr((void *)(& mrB->module));
  validptr((void *)(& mrA->module));
  validptr((void *)(modules + mrA->module));
  netNode = *(modules + mrA->module);
  while ((unsigned int )netNode != (unsigned int )((NetPtr )0)) {
    validptr((void *)(& netNode->next));
    validptr((void *)(& netNode->net));
    validptr((void *)(cost + netNode->net));
    netCost = *(cost + netNode->net);
    modNode = *(nets + netNode->net);
    while ((unsigned int )modNode != (unsigned int )((ModulePtr )0)) {
      validptr((void *)(& modNode->next));
      validptr((void *)(& modNode->module));
      if (modNode->module == module) {
        gain += netCost;
      }
      modNode = modNode->next;
    }
    netNode = netNode->next;
  }
  return (gain);
}
}
void SwapNode(ModuleRecPtr LOC(P) maxPrev ,
              ModuleRecPtr LOC(P) max ,
              ModuleList INST(L, P) * LOC(M) group ,
              ModuleList INST(L, P) * LOC(M) swapTo ,
              unsigned long * LOC(L) numModules , unsigned long * LOC(L) numNets , float * ARRAY LOC(F) GP ,
              float * ARRAY LOC(F) D , float * ARRAY LOC(F) cost ,
              Groups * ARRAY moduleToGroup ,
              ModuleList INST(L, P) * LOC(M) groupA ,
              ModuleList INST(L, P) * LOC(M) groupB ,
              ModuleList INST(L, P) * LOC(M) swapToA ,
              ModuleList INST(L, P) * LOC(M) swapToB ,
              NetPtr * ARRAY modules ,
              ModulePtr LOC(P) * ARRAY nets )
{ ModuleRecPtr swapToTail ;

  {
  if ((unsigned int )maxPrev == (unsigned int )((ModuleRecPtr )0)) {
    validptr((void *)(& group->head));
    validptr((void *)(& group->tail));
    validptr((void *)(& max->next));
    if ((unsigned int )group->head == (unsigned int )group->tail) {
      group->head = (ModuleRec *)0;
      group->tail = (ModuleRec *)0;
      max->next = (struct _ModuleRec *)0;
    } else {
      group->head = max->next;
      max->next = (struct _ModuleRec *)0;
    }
  } else {
    validptr((void *)(& group->tail));
    if ((unsigned int )group->tail == (unsigned int )max) {
      group->tail = maxPrev;
    }
    validptr((void *)(& maxPrev->next));
    validptr((void *)(& max->next));
    maxPrev->next = max->next;
    max->next = (struct _ModuleRec *)0;
  }
  validptr((void *)(& swapTo->tail));
  swapToTail = swapTo->tail;
  if ((unsigned int )swapToTail == (unsigned int )((ModuleRecPtr )0)) {
    swapTo->tail = max;
    swapTo->head = max;
  } else {
    validptr((void *)(& swapToTail->next));
    swapToTail->next = max;
    swapTo->tail = max;
  }
  validptr((void *)(& max->next));
  max->next = (struct _ModuleRec *)0;
  return;
}
}
void UpdateDs(ModuleRecPtr LOC(P) max , Groups group ,
              unsigned long * LOC(L) numModules , unsigned long * LOC(L) numNets ,
              float * ARRAY LOC(F) GP , float * ARRAY LOC(F) D ,
              float * ARRAY LOC(F) cost , Groups * ARRAY moduleToGroup ,
              ModuleList INST(L, P) * LOC(M) groupA ,
              ModuleList INST(L, P) * LOC(M) groupB ,
              ModuleList INST(L, P) * LOC(M) swapToA ,
              ModuleList INST(L, P) * LOC(M) swapToB ,
              NetPtr * ARRAY modules , ModulePtr LOC(P) * ARRAY nets )
{ NetPtr net ;
  ModulePtr mod ;

  {
  validptr((void *)(& max->module));
  validptr((void *)(modules + max->module));
  net = *(modules + max->module);
  while ((unsigned int )net != (unsigned int )((NetPtr )0)) {
    validptr((void *)(& net->next));
    validptr((void *)(& net->net));
    validptr((void *)(nets + net->net));
    mod = *(nets + net->net);
    while ((unsigned int )mod != (unsigned int )((ModulePtr )0)) {
      validptr((void *)(& mod->next));
      validptr((void *)(& mod->module));
      validptr((void *)(moduleToGroup + mod->module));
      if ((unsigned int )*(moduleToGroup + mod->module) < 2U) {
        validptr((void *)(D + mod->module));
        validptr((void *)(cost + net->net));
        if ((unsigned int )*(moduleToGroup + mod->module) == (unsigned int )group) {
          *(D + mod->module) += *(cost + net->net);
        } else {
          *(D + mod->module) -= *(cost + net->net);
        }
      }
      mod = mod->next;
    }
    net = net->next;
  }
  return;
}
}
float FindMaxGpAndSwap(unsigned long * LOC(L) numModules , unsigned long * LOC(L) numNets ,
                       float * ARRAY LOC(F) GP ,
                       float * ARRAY LOC(F) D , float * ARRAY LOC(F) cost ,
                       Groups * ARRAY moduleToGroup ,
                       ModuleList INST(L, P) * LOC(M) groupA ,
                       ModuleList INST(L, P) * LOC(M) groupB ,
                       ModuleList INST(L, P) * LOC(M) swapToA ,
                       ModuleList INST(L, P) * LOC(M) swapToB ,
                       NetPtr * ARRAY modules , ModulePtr LOC(P) * ARRAY nets )
{ ModuleRecPtr mrA ;
  ModuleRecPtr mrPrevA ;
  ModuleRecPtr mrB ;
  ModuleRecPtr mrPrevB ;
  ModuleRecPtr maxA ;
  ModuleRecPtr maxPrevA ;
  ModuleRecPtr maxB ;
  ModuleRecPtr maxPrevB ;
  float gp ;
  float gpMax ;
  float tmp ;

  {
  gpMax = (float )-9999999;
  maxPrevB = (ModuleRec *)0;
  maxB = maxPrevB;
  maxPrevA = maxB;
  maxA = maxPrevA;
  mrA = groupA->head;
  mrPrevA = (ModuleRec *)0;
  while ((unsigned int )mrA != (unsigned int )((ModuleRecPtr )0)) {
    validptr((void *)(& mrA->next));
    mrB = groupB->head;
    mrPrevB = (ModuleRec *)0;
    while ((unsigned int )mrB != (unsigned int )((ModuleRecPtr )0)) {
      validptr((void *)(& mrB->next));
      validptr((void *)(& mrA->module));
      validptr((void *)(& mrB->module));
      validptr((void *)(D + mrA->module));
      validptr((void *)(D + mrB->module));
      tmp = CAiBj(mrA, mrB, numModules, numNets, GP, D, cost, moduleToGroup, groupA,
                  groupB, swapToA, swapToB, modules, nets);
      gp = (*(D + mrA->module) + *(D + mrB->module)) - (float )2 * tmp;
      if (gp > gpMax) {
        gpMax = gp;
        maxA = mrA;
        maxPrevA = mrPrevA;
        maxB = mrB;
        maxPrevB = mrPrevB;
      }
      mrPrevB = mrB;
      mrB = mrB->next;
    }
    mrPrevA = mrA;
    mrA = mrA->next;
  }
  if ((unsigned int )maxA == (unsigned int )((ModuleRecPtr )0)) {
    HELL:
    goto HELL;
  }
  SwapNode(maxPrevA, maxA, groupA, swapToB, numModules, numNets, GP, D, cost, moduleToGroup,
           groupA, groupB, swapToA, swapToB, modules, nets);
  if ((unsigned int )maxB == (unsigned int )((ModuleRecPtr )0)) {
    LIMBO:
    goto LIMBO;
  }
  SwapNode(maxPrevB, maxB, groupB, swapToA, numModules, numNets, GP, D, cost, moduleToGroup,
           groupA, groupB, swapToA, swapToB, modules, nets);
  validptr((void *)(& maxA->module));
  validptr((void *)(moduleToGroup + maxA->module));
  *(moduleToGroup + maxA->module) = (enum __anonenum_Groups_1 )3;
  validptr((void *)(& maxB->module));
  validptr((void *)(moduleToGroup + maxB->module));
  *(moduleToGroup + maxB->module) = (enum __anonenum_Groups_1 )2;
  UpdateDs(maxA, (enum __anonenum_Groups_1 )0, numModules, numNets, GP, D, cost, moduleToGroup,
           groupA, groupB, swapToA, swapToB, modules, nets);
  UpdateDs(maxB, (enum __anonenum_Groups_1 )1, numModules, numNets, GP, D, cost, moduleToGroup,
           groupA, groupB, swapToA, swapToB, modules, nets);
  return (gpMax);
}
}
float FindGMax(unsigned long * LOC(L) iMax , unsigned long * LOC(L) numModules , unsigned long * LOC(L) numNets ,
               float * ARRAY LOC(F) GP , float * ARRAY LOC(F) D ,
               float * ARRAY LOC(F) cost , Groups * ARRAY moduleToGroup ,
               ModuleList INST(L, P) * LOC(M) groupA ,
               ModuleList INST(L, P) * LOC(M) groupB ,
               ModuleList INST(L, P) * LOC(M) swapToA ,
               ModuleList INST(L, P) * LOC(M) swapToB ,
               NetPtr * ARRAY modules , ModulePtr LOC(P) * ARRAY nets )
{ int i ;
  float gMax ;

  {
  gMax = (float )-9999999;
  *iMax = 4294967295UL;
  i = 0;
  while ((unsigned long )i < *numModules / 2UL) {
    validptr((void *)(GP + i));
    if (*(GP + i) > gMax) {
      gMax = *(GP + i);
      *iMax = (unsigned long )i;
    }
    i ++;
  }
  return (gMax);
}
}
void SwapSubsetAndReset(unsigned long iMax , unsigned long * LOC(L) numModules , unsigned long * LOC(L) numNets ,
                        float * ARRAY LOC(F) GP , float * ARRAY LOC(F) D ,
                        float * ARRAY LOC(F) cost , Groups * ARRAY moduleToGroup ,
                        ModuleList INST(L, P) * LOC(M) groupA ,
                        ModuleList INST(L, P) * LOC(M) groupB ,
                        ModuleList INST(L, P) * LOC(M) swapToA ,
                        ModuleList INST(L, P) * LOC(M) swapToB ,
                        NetPtr * ARRAY modules ,
                        ModulePtr LOC(P) * ARRAY nets )
{ unsigned long i ;
  ModuleRecPtr mrPrevA ;
  ModuleRecPtr mrA ;
  ModuleRecPtr mrPrevB ;
  ModuleRecPtr mrB ;
  ModuleRecPtr tmp ;
  ModuleRecPtr tmp___0 ;

  {
  mrPrevA = (ModuleRec *)0;
  mrA = swapToA->head;
  mrPrevB = (ModuleRec *)0;
  mrB = swapToB->head;
  i = 0UL;
  while (i <= iMax) {
    mrPrevA = mrA;
    mrA = mrA->next;
    mrPrevB = mrB;
    mrB = mrB->next;
    i ++;
  }
  if ((unsigned int )mrPrevA == (unsigned int )((ModuleRecPtr )0)) {
    DIS:
    goto DIS;
  }
  if ((unsigned int )mrPrevB == (unsigned int )((ModuleRecPtr )0)) {
    BED:
    goto BED;
  }
  if ((unsigned int )mrA == (unsigned int )((ModuleRecPtr )0)) {
    groupA->head = swapToA->head;
    groupA->tail = swapToA->tail;
    groupB->head = swapToB->head;
    groupB->tail = swapToB->tail;
  } else {
    validptr((void *)(& mrPrevA->next));
    mrPrevA->next = mrB;
    groupA->head = swapToA->head;
    groupA->tail = swapToB->tail;
    validptr((void *)(& mrPrevB->next));
    mrPrevB->next = mrA;
    groupB->head = swapToB->head;
    groupB->tail = swapToA->tail;
  }
  mrA = groupA->head;
  while ((unsigned int )mrA != (unsigned int )((ModuleRecPtr )0)) {
    validptr((void *)(& mrA->next));
    validptr((void *)(& mrA->module));
    validptr((void *)(moduleToGroup + mrA->module));
    *(moduleToGroup + mrA->module) = (enum __anonenum_Groups_1 )0;
    mrA = mrA->next;
  }
  mrB = groupB->head;
  while ((unsigned int )mrB != (unsigned int )((ModuleRecPtr )0)) {
    validptr((void *)(& mrB->next));
    validptr((void *)(& mrB->module));
    validptr((void *)(moduleToGroup + mrB->module));
    *(moduleToGroup + mrB->module) = (enum __anonenum_Groups_1 )1;
    mrB = mrB->next;
  }
  tmp = (ModuleRec *)0;
  swapToA->tail = tmp;
  swapToA->head = tmp;
  tmp___0 = (ModuleRec *)0;
  swapToB->tail = tmp___0;
  swapToB->head = tmp___0;
  return;
}
}
void PrintResults(int verbose , unsigned long * LOC(L) numModules , unsigned long * LOC(L) numNets ,
                  float * ARRAY LOC(F) GP , float * ARRAY LOC(F) D ,
                  float * ARRAY LOC(F) cost , Groups * ARRAY moduleToGroup ,
                  ModuleList INST(L, P) * LOC(M) groupA ,
                  ModuleList INST(L, P) * LOC(M) groupB ,
                  ModuleList INST(L, P) * LOC(M) swapToA ,
                  ModuleList INST(L, P) * LOC(M) swapToB ,
                  NetPtr * ARRAY modules ,
                  ModulePtr LOC(P) * ARRAY nets )
{ ModuleRecPtr mr ;
  NetPtr nn ;
  ModulePtr mn ;
  unsigned long cuts ;
  Groups grp ;
  int i ;
  int netSz ;
  long maxStat ;
  struct __anonstruct_netStats_2 netStats[256] ;
  unsigned long tmp ;
  unsigned long tmp___0 ;
  ModulePtr m ;

  {
  maxStat = -1L;
  i = 0;
  while (i < 256) {
    validptr((void *)(& netStats[i].total));
    validptr((void *)(& netStats[i].edgesCut));
    validptr((void *)(& netStats[i].netsCut));
    tmp___0 = 0UL;
    netStats[i].netsCut = tmp___0;
    tmp = tmp___0;
    netStats[i].edgesCut = tmp;
    netStats[i].total = tmp;
    i ++;
  }
  if (verbose) {
    mr = groupA->head;
    while ((unsigned int )mr != (unsigned int )((ModuleRecPtr )0)) {
      validptr((void *)(& mr->next));
      validptr((void *)(& mr->module));
      mr = mr->next;
    }
    mr = groupB->head;
    while ((unsigned int )mr != (unsigned int )((ModuleRecPtr )0)) {
      validptr((void *)(& mr->next));
      validptr((void *)(& mr->module));
      mr = mr->next;
    }
  }
  cuts = 0UL;
  mr = groupA->head;
  while ((unsigned int )mr != (unsigned int )((ModuleRecPtr )0)) {
    validptr((void *)(& mr->next));
    validptr((void *)(& mr->module));
    validptr((void *)(modules + mr->module));
    nn = *(modules + mr->module);
    while ((unsigned int )nn != (unsigned int )((NetPtr )0)) {
      validptr((void *)(& nn->next));
      netSz = 0;
      validptr((void *)(& nn->net));
      validptr((void *)(nets + nn->net));
      mn = *(nets + nn->net);
      while ((unsigned int )mn != (unsigned int )((ModulePtr )0)) {
        validptr((void *)(& mn->next));
        netSz ++;
        mn = mn->next;
      }
      if (netSz > 255) {
        LALALAND:
        goto LALALAND;
      }
      mn = *(nets + nn->net);
      while ((unsigned int )mn != (unsigned int )((ModulePtr )0)) {
        validptr((void *)(& mn->next));
        validptr((void *)(moduleToGroup + mr->module));
        validptr((void *)(moduleToGroup + mn->module));
        if ((unsigned int )*(moduleToGroup + mr->module) != (unsigned int )*(moduleToGroup + mn->module)) {
          validptr((void *)(& mr->module));
          if (verbose) {

          }
          validptr((void *)(& netStats[netSz].edgesCut));
          (netStats[netSz].edgesCut) ++;
          cuts ++;
        }
        mn = mn->next;
      }
      nn = nn->next;
    }
    mr = mr->next;
  }
  cuts = 0UL;
  i = 0;
  while ((unsigned long )i < *numNets) {
    netSz = 0;
    validptr((void *)(nets + i));
    mn = *(nets + i);
    while ((unsigned int )mn != (unsigned int )((ModulePtr )0)) {
      validptr((void *)(& mn->next));
      netSz ++;
      mn = mn->next;
    }
    if (netSz > 255) {
      HOLLYWOOD:
      goto HOLLYWOOD;
    }
    validptr((void *)(& netStats[netSz]));
    validptr((void *)(& netStats[netSz].total));
    (netStats[netSz].total) ++;
    if ((long )netSz > maxStat) {
      maxStat = (long )netSz;
    }
    m = *(nets + i);
    if ((unsigned int )m == (unsigned int )((ModulePtr )0)) {
      OUTERSPACE:
      goto OUTERSPACE;
    }
    validptr((void *)(& m->module));
    validptr((void *)(moduleToGroup + (*(nets + i))->module));
    validptr((void *)(& m->next));
    grp = *(moduleToGroup + (*(nets + i))->module);
    mn = (*(nets + i))->next;
    while ((unsigned int )mn != (unsigned int )((ModulePtr )0)) {
      validptr((void *)(& mn->next));
      validptr((void *)(& mn->module));
      validptr((void *)(moduleToGroup + mn->module));
      if ((unsigned int )grp != (unsigned int )*(moduleToGroup + mn->module)) {
        if (verbose) {

        }
        cuts ++;
        validptr((void *)(& netStats[netSz].netsCut));
        (netStats[netSz].netsCut) ++;
        break;
      }
      mn = mn->next;
    }
    i ++;
  }
  i = 2;
  while ((long )i <= maxStat) {
    validptr((void *)(& netStats[i]));
    validptr((void *)(& netStats[i].total));
    validptr((void *)(& netStats[i].edgesCut));
    validptr((void *)(& netStats[i].netsCut));
    i ++;
  }
  return;
}
}

// pmr: We assume all entries in argv are valid pointers; we can't express the exact contract yet
int main(int argc, char * ARRAY VALIDPTR * START NONNULL ARRAY SIZE(argc * 4) argv) CHECK_TYPE
{ unsigned long p ;
  unsigned long iMax ;
  float gMax ;
  float lastGMax ;
  ModuleRecPtr mr ;
  unsigned long *numNets ;
  unsigned long *numModules ;
  float *GP ;
  float *D ;
  float *cost ;
  Groups *moduleToGroup ;
  ModuleList *groupA ;
  ModuleList *groupB ;
  ModuleList *swapToA ;
  ModuleList *swapToB ;
  NetPtr *modules ;
  ModulePtr *nets ;
  void *tmp ;
  void *tmp___0 ;
  void *tmp___1 ;
  void *tmp___2 ;
  void *tmp___3 ;
  void *tmp___4 ;
  void *tmp___5 ;
  void *tmp___6 ;
  void *tmp___7 ;
  void *tmp___8 ;
  void *tmp___9 ;
  void *tmp___10 ;

  {
  tmp = malloc(sizeof(unsigned long ));
  numNets = (unsigned long *)tmp;
  *numNets = 0UL;
  tmp___0 = malloc(sizeof(unsigned long ));
  numModules = (unsigned long *)tmp___0;
  *numModules = 0UL;
  tmp___1 = malloc(sizeof(float ) * 1024U);
  GP = (float *)tmp___1;
  tmp___2 = malloc(sizeof(float ) * 1024U);
  D = (float *)tmp___2;
  tmp___3 = malloc(sizeof(float ) * 1024U);
  cost = (float *)tmp___3;
  tmp___4 = malloc(sizeof(Groups ) * 1024U);
  moduleToGroup = (Groups *)tmp___4;
  tmp___5 = malloc(sizeof(ModuleList ));
  groupA = (ModuleList *)tmp___5;
  tmp___6 = malloc(sizeof(ModuleList ));
  groupB = (ModuleList *)tmp___6;
  tmp___7 = malloc(sizeof(ModuleList ));
  swapToA = (ModuleList *)tmp___7;
  tmp___8 = malloc(sizeof(ModuleList ));
  swapToB = (ModuleList *)tmp___8;
  tmp___9 = malloc(sizeof(ModulePtr ) * 1024U);
  modules = (NetPtr *)tmp___9;
  tmp___10 = malloc(sizeof(ModulePtr ) * 1024U);
  nets = (ModulePtr *)tmp___10;
  if (argc != 2) {
    PURGATORY: 
    goto PURGATORY;
  }
  validptr((void *)(argv + 1));
  ReadNetList(*(argv + 1), numModules, numNets, (float * ARRAY )GP,
              (float * ARRAY )D, (float * ARRAY )cost,
              (Groups * ARRAY )moduleToGroup, groupA, groupB,
              swapToA, swapToB, (NetPtr * ARRAY )modules, (ModulePtr * ARRAY )nets);
  NetsToModules(numModules, numNets, (float * ARRAY )GP, (float * ARRAY )D,
                (float * ARRAY )cost, (Groups * ARRAY )moduleToGroup,
                groupA, groupB, swapToA, swapToB, (NetPtr * ARRAY )modules,
                (ModulePtr * ARRAY )nets);
  ComputeNetCosts(numModules, numNets, (float * ARRAY )GP, (float * ARRAY )D,
                  (float * ARRAY )cost, (Groups * ARRAY )moduleToGroup,
                  groupA, groupB, swapToA, swapToB, (NetPtr * ARRAY )modules,
                  (ModulePtr * ARRAY )nets);
  InitLists(numModules, numNets, (float * ARRAY )GP, (float * ARRAY )D,
            (float * ARRAY )cost, (Groups * ARRAY )moduleToGroup,
            groupA, groupB, swapToA, swapToB, (NetPtr * ARRAY )modules,
            (ModulePtr * ARRAY )nets);
  lastGMax = (float )0;
  while (1) {
    ComputeDs(groupA, (enum __anonenum_Groups_1 )0, (enum __anonenum_Groups_1 )2,
              numModules, numNets, (float * ARRAY )GP, (float * ARRAY )D,
              (float * ARRAY )cost, (Groups * ARRAY )moduleToGroup,
              groupA, groupB, swapToA, swapToB, (NetPtr * ARRAY )modules,
              (ModulePtr * ARRAY )nets);
    ComputeDs(groupB, (enum __anonenum_Groups_1 )1, (enum __anonenum_Groups_1 )3,
              numModules, numNets, (float * ARRAY )GP, (float * ARRAY )D,
              (float * ARRAY )cost, (Groups * ARRAY )moduleToGroup,
              groupA, groupB, swapToA, swapToB, (NetPtr * ARRAY )modules,
              (ModulePtr * ARRAY )nets);
    p = 0UL;
    while (p < *numModules / 2UL) {
      validptr((void *)(GP + p));
      *(GP + p) = FindMaxGpAndSwap(numModules, numNets, (float * ARRAY )GP,
                                   (float * ARRAY )D, (float * ARRAY )cost,
                                   (Groups * ARRAY )moduleToGroup,
                                   groupA, groupB, swapToA, swapToB, (NetPtr * ARRAY )modules,
                                   (ModulePtr * ARRAY )nets);
      p ++;
    }
    gMax = FindGMax(& iMax, numModules, numNets, (float * ARRAY )GP,
                    (float * ARRAY )D, (float * ARRAY )cost,
                    (Groups * ARRAY )moduleToGroup, groupA, groupB,
                    swapToA, swapToB, (NetPtr * ARRAY )modules,
                    (ModulePtr * ARRAY )nets);
    if (lastGMax == gMax) {

    }
    lastGMax = gMax;
    if ((double )gMax > 0.0) {
      SwapSubsetAndReset(iMax, numModules, numNets, (float * ARRAY )GP,
                         (float * ARRAY )D, (float * ARRAY )cost,
                         (Groups * ARRAY )moduleToGroup, groupA,
                         groupB, swapToA, swapToB, (NetPtr * ARRAY )modules,
                         (ModulePtr * ARRAY )nets);
    }
    PrintResults(0, numModules, numNets, (float * ARRAY )GP,
                 (float * ARRAY )D, (float * ARRAY )cost,
                 (Groups * ARRAY )moduleToGroup, groupA, groupB,
                 swapToA, swapToB, (NetPtr * ARRAY )modules,
                 (ModulePtr * ARRAY )nets);
    if (! ((double )gMax > 0.0)) {
      break;
    }
  }
  groupA->head = swapToB->head;
  groupA->tail = swapToB->tail;
  mr = groupA->head;
  while ((unsigned int )mr != (unsigned int )((ModuleRecPtr )0)) {
    validptr((void *)(& mr->next));
    validptr((void *)(& mr->module));
    validptr((void *)(moduleToGroup + mr->module));
    *(moduleToGroup + mr->module) = (enum __anonenum_Groups_1 )0;
    mr = mr->next;
  }
  groupB->head = swapToA->head;
  groupB->tail = swapToA->tail;
  mr = groupB->head;
  while ((unsigned int )mr != (unsigned int )((ModuleRecPtr )0)) {
    validptr((void *)(& mr->next));
    validptr((void *)(& mr->module));
    validptr((void *)(moduleToGroup + mr->module));
    *(moduleToGroup + mr->module) = (enum __anonenum_Groups_1 )1;
    mr = mr->next;
  }
  PrintResults(1, numModules, numNets, (float * ARRAY )GP, (float * ARRAY )D,
               (float * ARRAY )cost, (Groups * ARRAY )moduleToGroup,
               groupA, groupB, swapToA, swapToB, (NetPtr * ARRAY )modules,
               (ModulePtr * ARRAY )nets);
  exit(0);
  return (0);
}
}

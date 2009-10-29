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
   ModuleRecPtr head ;
   ModuleRecPtr tail ;
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
#pragma merger(0,"/tmp/cil-sOn55UwB.i","")
extern void *malloc(size_t  ) ;
extern void exit(int  ) ;
extern long atol(char * __attribute__((__array__))  ) ;
extern char *strtok(char * __attribute__((__array__))  , char * __attribute__((__array__))  ) ;
extern int *fopen(char * __attribute__((__array__))  , char * __attribute__((__array__))  ) ;
extern char *fgets(char * __attribute__((__array__))  , int  , int * ) ;
extern void validptr(void * ) ;
void ReadNetList(char * __attribute__((__array__)) fname , unsigned long *numModules ,
                 unsigned long *numNets , float * __attribute__((__array__)) GP ,
                 float * __attribute__((__array__)) D , float * __attribute__((__array__)) cost ,
                 Groups * __attribute__((__array__)) moduleToGroup , ModuleList *groupA ,
                 ModuleList *groupB , ModuleList *swapToA , ModuleList *swapToB ,
                 NetPtr * __attribute__((__array__)) modules , ModulePtr * __attribute__((__array__)) nets ) ;
void NetsToModules(unsigned long *numModules , unsigned long *numNets , float * __attribute__((__array__)) GP ,
                   float * __attribute__((__array__)) D , float * __attribute__((__array__)) cost ,
                   Groups * __attribute__((__array__)) moduleToGroup , ModuleList *groupA ,
                   ModuleList *groupB , ModuleList *swapToA , ModuleList *swapToB ,
                   NetPtr * __attribute__((__array__)) modules , ModulePtr * __attribute__((__array__)) nets ) ;
void ComputeNetCosts(unsigned long *numModules , unsigned long *numNets , float * __attribute__((__array__)) GP ,
                     float * __attribute__((__array__)) D , float * __attribute__((__array__)) cost ,
                     Groups * __attribute__((__array__)) moduleToGroup , ModuleList *groupA ,
                     ModuleList *groupB , ModuleList *swapToA , ModuleList *swapToB ,
                     NetPtr * __attribute__((__array__)) modules , ModulePtr * __attribute__((__array__)) nets ) ;
void InitLists(unsigned long *numModules , unsigned long *numNets , float * __attribute__((__array__)) GP ,
               float * __attribute__((__array__)) D , float * __attribute__((__array__)) cost ,
               Groups * __attribute__((__array__)) moduleToGroup , ModuleList *groupA ,
               ModuleList *groupB , ModuleList *swapToA , ModuleList *swapToB , NetPtr * __attribute__((__array__)) modules ,
               ModulePtr * __attribute__((__array__)) nets ) ;
void ComputeDs(ModuleListPtr group , Groups myGroup , Groups mySwap , unsigned long *numModules ,
               unsigned long *numNets , float * __attribute__((__array__)) GP , float * __attribute__((__array__)) D ,
               float * __attribute__((__array__)) cost , Groups * __attribute__((__array__)) moduleToGroup ,
               ModuleList *groupA , ModuleList *groupB , ModuleList *swapToA , ModuleList *swapToB ,
               NetPtr * __attribute__((__array__)) modules , ModulePtr * __attribute__((__array__)) nets ) ;
extern int ( /* missing proto */  nondetpos)() ;
void ReadNetList(char * __attribute__((__array__)) fname , unsigned long *numModules ,
                 unsigned long *numNets , float * __attribute__((__array__)) GP ,
                 float * __attribute__((__array__)) D , float * __attribute__((__array__)) cost ,
                 Groups * __attribute__((__array__)) moduleToGroup , ModuleList *groupA ,
                 ModuleList *groupB , ModuleList *swapToA , ModuleList *swapToB ,
                 NetPtr * __attribute__((__array__)) modules , ModulePtr * __attribute__((__array__)) nets ) 
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
void NetsToModules(unsigned long *numModules , unsigned long *numNets , float * __attribute__((__array__)) GP ,
                   float * __attribute__((__array__)) D , float * __attribute__((__array__)) cost ,
                   Groups * __attribute__((__array__)) moduleToGroup , ModuleList *groupA ,
                   ModuleList *groupB , ModuleList *swapToA , ModuleList *swapToB ,
                   NetPtr * __attribute__((__array__)) modules , ModulePtr * __attribute__((__array__)) nets ) 
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
void ComputeNetCosts(unsigned long *numModules , unsigned long *numNets , float * __attribute__((__array__)) GP ,
                     float * __attribute__((__array__)) D , float * __attribute__((__array__)) cost ,
                     Groups * __attribute__((__array__)) moduleToGroup , ModuleList *groupA ,
                     ModuleList *groupB , ModuleList *swapToA , ModuleList *swapToB ,
                     NetPtr * __attribute__((__array__)) modules , ModulePtr * __attribute__((__array__)) nets ) 
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
void InitLists(unsigned long *numModules , unsigned long *numNets , float * __attribute__((__array__)) GP ,
               float * __attribute__((__array__)) D , float * __attribute__((__array__)) cost ,
               Groups * __attribute__((__array__)) moduleToGroup , ModuleList *groupA ,
               ModuleList *groupB , ModuleList *swapToA , ModuleList *swapToB , NetPtr * __attribute__((__array__)) modules ,
               ModulePtr * __attribute__((__array__)) nets ) 
{ unsigned long p ;
  ModuleRecPtr mr ;
  unsigned int numMods ;
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
  numMods = (unsigned int )*numModules;
  p = 0UL;
  while (p < (unsigned long )(numMods / 2U)) {
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
    mr->module = (unsigned long )(numMods / 2U) + p;
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
    validptr((void *)(moduleToGroup + ((unsigned long )(numMods / 2U) + p)));
    *(moduleToGroup + ((unsigned long )(numMods / 2U) + p)) = (enum __anonenum_Groups_1 )1;
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
void ComputeDs(ModuleListPtr group , Groups myGroup , Groups mySwap , unsigned long *numModules ,
               unsigned long *numNets , float * __attribute__((__array__)) GP , float * __attribute__((__array__)) D ,
               float * __attribute__((__array__)) cost , Groups * __attribute__((__array__)) moduleToGroup ,
               ModuleList *groupA , ModuleList *groupB , ModuleList *swapToA , ModuleList *swapToB ,
               NetPtr * __attribute__((__array__)) modules , ModulePtr * __attribute__((__array__)) nets ) 
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
#pragma merger(0,"/tmp/cil-vmmpClOE.i","")
float CAiBj(ModuleRecPtr mrA , ModuleRecPtr mrB , unsigned long *numModules , unsigned long *numNets ,
            float * __attribute__((__array__)) GP , float * __attribute__((__array__)) D ,
            float * __attribute__((__array__)) cost , Groups * __attribute__((__array__)) moduleToGroup ,
            ModuleList *groupA , ModuleList *groupB , ModuleList *swapToA , ModuleList *swapToB ,
            NetPtr * __attribute__((__array__)) modules , ModulePtr * __attribute__((__array__)) nets ) ;
void SwapNode(ModuleRecPtr maxPrev , ModuleRecPtr max , ModuleListPtr group , ModuleListPtr swapTo ,
              unsigned long *numModules , unsigned long *numNets , float * __attribute__((__array__)) GP ,
              float * __attribute__((__array__)) D , float * __attribute__((__array__)) cost ,
              Groups * __attribute__((__array__)) moduleToGroup , ModuleList *groupA ,
              ModuleList *groupB , ModuleList *swapToA , ModuleList *swapToB , NetPtr * __attribute__((__array__)) modules ,
              ModulePtr * __attribute__((__array__)) nets ) ;
void UpdateDs(ModuleRecPtr max , Groups group , unsigned long *numModules , unsigned long *numNets ,
              float * __attribute__((__array__)) GP , float * __attribute__((__array__)) D ,
              float * __attribute__((__array__)) cost , Groups * __attribute__((__array__)) moduleToGroup ,
              ModuleList *groupA , ModuleList *groupB , ModuleList *swapToA , ModuleList *swapToB ,
              NetPtr * __attribute__((__array__)) modules , ModulePtr * __attribute__((__array__)) nets ) ;
float FindMaxGpAndSwap(unsigned long *numModules , unsigned long *numNets , float * __attribute__((__array__)) GP ,
                       float * __attribute__((__array__)) D , float * __attribute__((__array__)) cost ,
                       Groups * __attribute__((__array__)) moduleToGroup , ModuleList *groupA ,
                       ModuleList *groupB , ModuleList *swapToA , ModuleList *swapToB ,
                       NetPtr * __attribute__((__array__)) modules , ModulePtr * __attribute__((__array__)) nets ) ;
void SwapSubsetAndReset(unsigned long iMax , unsigned long *numModules , unsigned long *numNets ,
                        float * __attribute__((__array__)) GP , float * __attribute__((__array__)) D ,
                        float * __attribute__((__array__)) cost , Groups * __attribute__((__array__)) moduleToGroup ,
                        ModuleList *groupA , ModuleList *groupB , ModuleList *swapToA ,
                        ModuleList *swapToB , NetPtr * __attribute__((__array__)) modules ,
                        ModulePtr * __attribute__((__array__)) nets ) ;
void PrintResults(int verbose , unsigned long *numModules , unsigned long *numNets ,
                  float * __attribute__((__array__)) GP , float * __attribute__((__array__)) D ,
                  float * __attribute__((__array__)) cost , Groups * __attribute__((__array__)) moduleToGroup ,
                  ModuleList *groupA , ModuleList *groupB , ModuleList *swapToA ,
                  ModuleList *swapToB , NetPtr * __attribute__((__array__)) modules ,
                  ModulePtr * __attribute__((__array__)) nets ) ;
int main(int argc , char * __attribute__((__array__)) * __attribute__((__array__)) argv ) ;
float CAiBj(ModuleRecPtr mrA , ModuleRecPtr mrB , unsigned long *numModules , unsigned long *numNets ,
            float * __attribute__((__array__)) GP , float * __attribute__((__array__)) D ,
            float * __attribute__((__array__)) cost , Groups * __attribute__((__array__)) moduleToGroup ,
            ModuleList *groupA , ModuleList *groupB , ModuleList *swapToA , ModuleList *swapToB ,
            NetPtr * __attribute__((__array__)) modules , ModulePtr * __attribute__((__array__)) nets ) 
{ NetPtr netNode ;
  ModulePtr modNode ;
  float gain ;
  float netCost ;
  unsigned long module ;

  {
  gain = (float )0.0;
  module = mrB->module;
  validptr((void *)(modules + mrA->module));
  netNode = *(modules + mrA->module);
  while ((unsigned int )netNode != (unsigned int )((NetPtr )0)) {
    validptr((void *)(cost + netNode->net));
    netCost = *(cost + netNode->net);
    modNode = *(nets + netNode->net);
    while ((unsigned int )modNode != (unsigned int )((ModulePtr )0)) {
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
void SwapNode(ModuleRecPtr maxPrev , ModuleRecPtr max , ModuleListPtr group , ModuleListPtr swapTo ,
              unsigned long *numModules , unsigned long *numNets , float * __attribute__((__array__)) GP ,
              float * __attribute__((__array__)) D , float * __attribute__((__array__)) cost ,
              Groups * __attribute__((__array__)) moduleToGroup , ModuleList *groupA ,
              ModuleList *groupB , ModuleList *swapToA , ModuleList *swapToB , NetPtr * __attribute__((__array__)) modules ,
              ModulePtr * __attribute__((__array__)) nets ) 
{ 

  {
  if ((unsigned int )maxPrev == (unsigned int )((ModuleRecPtr )0)) {
    if ((unsigned int )group->head == (unsigned int )group->tail) {
      group->head = (ModuleRec *)0;
      group->tail = (ModuleRec *)0;
      max->next = (struct _ModuleRec *)0;
    } else {
      group->head = max->next;
      max->next = (struct _ModuleRec *)0;
    }
  } else {
    if ((unsigned int )group->tail == (unsigned int )max) {
      group->tail = maxPrev;
    }
    maxPrev->next = max->next;
    max->next = (struct _ModuleRec *)0;
  }
  if ((unsigned int )swapTo->tail == (unsigned int )((ModuleRecPtr )0)) {
    swapTo->tail = max;
    swapTo->head = max;
  } else {
    (swapTo->tail)->next = max;
    swapTo->tail = max;
  }
  max->next = (struct _ModuleRec *)0;
  return;
}
}
void UpdateDs(ModuleRecPtr max , Groups group , unsigned long *numModules , unsigned long *numNets ,
              float * __attribute__((__array__)) GP , float * __attribute__((__array__)) D ,
              float * __attribute__((__array__)) cost , Groups * __attribute__((__array__)) moduleToGroup ,
              ModuleList *groupA , ModuleList *groupB , ModuleList *swapToA , ModuleList *swapToB ,
              NetPtr * __attribute__((__array__)) modules , ModulePtr * __attribute__((__array__)) nets ) 
{ NetPtr net ;
  ModulePtr mod ;

  {
  validptr((void *)(modules + max->module));
  net = *(modules + max->module);
  while ((unsigned int )net != (unsigned int )((NetPtr )0)) {
    validptr((void *)(nets + net->net));
    mod = *(nets + net->net);
    while ((unsigned int )mod != (unsigned int )((ModulePtr )0)) {
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
float FindMaxGpAndSwap(unsigned long *numModules , unsigned long *numNets , float * __attribute__((__array__)) GP ,
                       float * __attribute__((__array__)) D , float * __attribute__((__array__)) cost ,
                       Groups * __attribute__((__array__)) moduleToGroup , ModuleList *groupA ,
                       ModuleList *groupB , ModuleList *swapToA , ModuleList *swapToB ,
                       NetPtr * __attribute__((__array__)) modules , ModulePtr * __attribute__((__array__)) nets ) 
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
    mrB = groupB->head;
    mrPrevB = (ModuleRec *)0;
    while ((unsigned int )mrB != (unsigned int )((ModuleRecPtr )0)) {
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
  SwapNode(maxPrevA, maxA, groupA, swapToB, numModules, numNets, GP, D, cost, moduleToGroup,
           groupA, groupB, swapToA, swapToB, modules, nets);
  SwapNode(maxPrevB, maxB, groupB, swapToA, numModules, numNets, GP, D, cost, moduleToGroup,
           groupA, groupB, swapToA, swapToB, modules, nets);
  validptr((void *)(moduleToGroup + maxA->module));
  *(moduleToGroup + maxA->module) = (enum __anonenum_Groups_1 )3;
  validptr((void *)(moduleToGroup + maxB->module));
  *(moduleToGroup + maxB->module) = (enum __anonenum_Groups_1 )2;
  UpdateDs(maxA, (enum __anonenum_Groups_1 )0, numModules, numNets, GP, D, cost, moduleToGroup,
           groupA, groupB, swapToA, swapToB, modules, nets);
  UpdateDs(maxB, (enum __anonenum_Groups_1 )1, numModules, numNets, GP, D, cost, moduleToGroup,
           groupA, groupB, swapToA, swapToB, modules, nets);
  return (gpMax);
}
}
float FindGMax(unsigned long *iMax , unsigned long *numModules , unsigned long *numNets ,
               float * __attribute__((__array__)) GP , float * __attribute__((__array__)) D ,
               float * __attribute__((__array__)) cost , Groups * __attribute__((__array__)) moduleToGroup ,
               ModuleList *groupA , ModuleList *groupB , ModuleList *swapToA , ModuleList *swapToB ,
               NetPtr * __attribute__((__array__)) modules , ModulePtr * __attribute__((__array__)) nets ) 
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
void SwapSubsetAndReset(unsigned long iMax , unsigned long *numModules , unsigned long *numNets ,
                        float * __attribute__((__array__)) GP , float * __attribute__((__array__)) D ,
                        float * __attribute__((__array__)) cost , Groups * __attribute__((__array__)) moduleToGroup ,
                        ModuleList *groupA , ModuleList *groupB , ModuleList *swapToA ,
                        ModuleList *swapToB , NetPtr * __attribute__((__array__)) modules ,
                        ModulePtr * __attribute__((__array__)) nets ) 
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
  if ((unsigned int )mrA == (unsigned int )((ModuleRecPtr )0)) {
    groupA->head = swapToA->head;
    groupA->tail = swapToA->tail;
    groupB->head = swapToB->head;
    groupB->tail = swapToB->tail;
  } else {
    mrPrevA->next = mrB;
    groupA->head = swapToA->head;
    groupA->tail = swapToB->tail;
    mrPrevB->next = mrA;
    groupB->head = swapToB->head;
    groupB->tail = swapToA->tail;
  }
  mrA = groupA->head;
  while ((unsigned int )mrA != (unsigned int )((ModuleRecPtr )0)) {
    validptr((void *)(moduleToGroup + mrA->module));
    *(moduleToGroup + mrA->module) = (enum __anonenum_Groups_1 )0;
    mrA = mrA->next;
  }
  mrB = groupB->head;
  while ((unsigned int )mrB != (unsigned int )((ModuleRecPtr )0)) {
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
void PrintResults(int verbose , unsigned long *numModules , unsigned long *numNets ,
                  float * __attribute__((__array__)) GP , float * __attribute__((__array__)) D ,
                  float * __attribute__((__array__)) cost , Groups * __attribute__((__array__)) moduleToGroup ,
                  ModuleList *groupA , ModuleList *groupB , ModuleList *swapToA ,
                  ModuleList *swapToB , NetPtr * __attribute__((__array__)) modules ,
                  ModulePtr * __attribute__((__array__)) nets ) 
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
      mr = mr->next;
    }
    mr = groupB->head;
    while ((unsigned int )mr != (unsigned int )((ModuleRecPtr )0)) {
      mr = mr->next;
    }
  }
  cuts = 0UL;
  mr = groupA->head;
  while ((unsigned int )mr != (unsigned int )((ModuleRecPtr )0)) {
    validptr((void *)(modules + mr->module));
    nn = *(modules + mr->module);
    while ((unsigned int )nn != (unsigned int )((NetPtr )0)) {
      netSz = 0;
      validptr((void *)(nets + nn->net));
      mn = *(nets + nn->net);
      while ((unsigned int )mn != (unsigned int )((ModulePtr )0)) {
        validptr((void *)(& mn->next));
        netSz ++;
        mn = mn->next;
      }
      mn = *(nets + nn->net);
      while ((unsigned int )mn != (unsigned int )((ModulePtr )0)) {
        validptr((void *)(moduleToGroup + mr->module));
        validptr((void *)(moduleToGroup + mn->module));
        if ((unsigned int )*(moduleToGroup + mr->module) != (unsigned int )*(moduleToGroup + mn->module)) {
          if (verbose) {

          }
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
      netSz ++;
      mn = mn->next;
    }
    (netStats[netSz].total) ++;
    if ((long )netSz > maxStat) {
      maxStat = (long )netSz;
    }
    validptr((void *)(moduleToGroup + (*(nets + i))->module));
    grp = *(moduleToGroup + (*(nets + i))->module);
    mn = (*(nets + i))->next;
    while ((unsigned int )mn != (unsigned int )((ModulePtr )0)) {
      validptr((void *)(moduleToGroup + mn->module));
      if ((unsigned int )grp != (unsigned int )*(moduleToGroup + mn->module)) {
        if (verbose) {

        }
        cuts ++;
        (netStats[netSz].netsCut) ++;
        break;
      }
      mn = mn->next;
    }
    i ++;
  }
  i = 2;
  while ((long )i <= maxStat) {
    i ++;
  }
  return;
}
}
int main(int argc , char * __attribute__((__array__)) * __attribute__((__array__)) argv ) 
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
    exit(1);
  }
  ReadNetList(*(argv + 1), numModules, numNets, (float * __attribute__((__array__)) )GP,
              (float * __attribute__((__array__)) )D, (float * __attribute__((__array__)) )cost,
              (Groups * __attribute__((__array__)) )moduleToGroup, groupA, groupB,
              swapToA, swapToB, (NetPtr * __attribute__((__array__)) )modules, (ModulePtr * __attribute__((__array__)) )nets);
  NetsToModules(numModules, numNets, (float * __attribute__((__array__)) )GP, (float * __attribute__((__array__)) )D,
                (float * __attribute__((__array__)) )cost, (Groups * __attribute__((__array__)) )moduleToGroup,
                groupA, groupB, swapToA, swapToB, (NetPtr * __attribute__((__array__)) )modules,
                (ModulePtr * __attribute__((__array__)) )nets);
  ComputeNetCosts(numModules, numNets, (float * __attribute__((__array__)) )GP, (float * __attribute__((__array__)) )D,
                  (float * __attribute__((__array__)) )cost, (Groups * __attribute__((__array__)) )moduleToGroup,
                  groupA, groupB, swapToA, swapToB, (NetPtr * __attribute__((__array__)) )modules,
                  (ModulePtr * __attribute__((__array__)) )nets);
  InitLists(numModules, numNets, (float * __attribute__((__array__)) )GP, (float * __attribute__((__array__)) )D,
            (float * __attribute__((__array__)) )cost, (Groups * __attribute__((__array__)) )moduleToGroup,
            groupA, groupB, swapToA, swapToB, (NetPtr * __attribute__((__array__)) )modules,
            (ModulePtr * __attribute__((__array__)) )nets);
  lastGMax = (float )0;
  while (1) {
    ComputeDs(groupA, (enum __anonenum_Groups_1 )0, (enum __anonenum_Groups_1 )2,
              numModules, numNets, (float * __attribute__((__array__)) )GP, (float * __attribute__((__array__)) )D,
              (float * __attribute__((__array__)) )cost, (Groups * __attribute__((__array__)) )moduleToGroup,
              groupA, groupB, swapToA, swapToB, (NetPtr * __attribute__((__array__)) )modules,
              (ModulePtr * __attribute__((__array__)) )nets);
    ComputeDs(groupB, (enum __anonenum_Groups_1 )1, (enum __anonenum_Groups_1 )3,
              numModules, numNets, (float * __attribute__((__array__)) )GP, (float * __attribute__((__array__)) )D,
              (float * __attribute__((__array__)) )cost, (Groups * __attribute__((__array__)) )moduleToGroup,
              groupA, groupB, swapToA, swapToB, (NetPtr * __attribute__((__array__)) )modules,
              (ModulePtr * __attribute__((__array__)) )nets);
    p = 0UL;
    while (p < *numModules / 2UL) {
      validptr((void *)(GP + p));
      *(GP + p) = FindMaxGpAndSwap(numModules, numNets, (float * __attribute__((__array__)) )GP,
                                   (float * __attribute__((__array__)) )D, (float * __attribute__((__array__)) )cost,
                                   (Groups * __attribute__((__array__)) )moduleToGroup,
                                   groupA, groupB, swapToA, swapToB, (NetPtr * __attribute__((__array__)) )modules,
                                   (ModulePtr * __attribute__((__array__)) )nets);
      p ++;
    }
    gMax = FindGMax(& iMax, numModules, numNets, (float * __attribute__((__array__)) )GP,
                    (float * __attribute__((__array__)) )D, (float * __attribute__((__array__)) )cost,
                    (Groups * __attribute__((__array__)) )moduleToGroup, groupA, groupB,
                    swapToA, swapToB, (NetPtr * __attribute__((__array__)) )modules,
                    (ModulePtr * __attribute__((__array__)) )nets);
    if (lastGMax == gMax) {

    }
    lastGMax = gMax;
    if ((double )gMax > 0.0) {
      SwapSubsetAndReset(iMax, numModules, numNets, (float * __attribute__((__array__)) )GP,
                         (float * __attribute__((__array__)) )D, (float * __attribute__((__array__)) )cost,
                         (Groups * __attribute__((__array__)) )moduleToGroup, groupA,
                         groupB, swapToA, swapToB, (NetPtr * __attribute__((__array__)) )modules,
                         (ModulePtr * __attribute__((__array__)) )nets);
    }
    PrintResults(0, numModules, numNets, (float * __attribute__((__array__)) )GP,
                 (float * __attribute__((__array__)) )D, (float * __attribute__((__array__)) )cost,
                 (Groups * __attribute__((__array__)) )moduleToGroup, groupA, groupB,
                 swapToA, swapToB, (NetPtr * __attribute__((__array__)) )modules,
                 (ModulePtr * __attribute__((__array__)) )nets);
    if (! ((double )gMax > 0.0)) {
      break;
    }
  }
  groupA->head = swapToB->head;
  groupA->tail = swapToB->tail;
  mr = groupA->head;
  while ((unsigned int )mr != (unsigned int )((ModuleRecPtr )0)) {
    validptr((void *)(moduleToGroup + mr->module));
    *(moduleToGroup + mr->module) = (enum __anonenum_Groups_1 )0;
    mr = mr->next;
  }
  groupB->head = swapToA->head;
  groupB->tail = swapToB->tail;
  mr = groupB->head;
  while ((unsigned int )mr != (unsigned int )((ModuleRecPtr )0)) {
    validptr((void *)(moduleToGroup + mr->module));
    *(moduleToGroup + mr->module) = (enum __anonenum_Groups_1 )1;
    mr = mr->next;
  }
  PrintResults(1, numModules, numNets, (float * __attribute__((__array__)) )GP, (float * __attribute__((__array__)) )D,
               (float * __attribute__((__array__)) )cost, (Groups * __attribute__((__array__)) )moduleToGroup,
               groupA, groupB, swapToA, swapToB, (NetPtr * __attribute__((__array__)) )modules,
               (ModulePtr * __attribute__((__array__)) )nets);
  exit(0);
  return (0);
}
}

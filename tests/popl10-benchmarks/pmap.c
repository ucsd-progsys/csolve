#include <stdlib.h>
#include <csolve.h>

extern void dummyassert (int) OKEXTERN;

typedef struct env {
    int env_mypp;
    struct env *env_prev;
    struct env *env_next;
    int (SHAPE_IGNORE_BOUND env_pgdir)[2000];
} env_t;

typedef int * ARRAY int_array;

/* Globals, now parameters passed around...
int pages[1000];
int page_protected[1000];
env_t *envs = 0;
*/

int page_getfree(int pages[])
{
    int i;
    int rv;
    int rv2;

    for (i = 0; i < 1000; i++){
        validptr(pages + i);
        if (pages[i] == 0){
            return i;
	}
    }
    return -1;
}

int page_free(int ppno, int pages[], int page_protected[])
{
  csolve_assert(0 <= ppno);
  csolve_assert(ppno < 1000);
  dummyassert(pages[ppno] > 0 || page_protected[ppno] == 0);
  validptr(pages + ppno);
  return pages[ppno] == 0;
}

int is_page_protected(int ppno, int pages[], int page_protected[])
{
    csolve_assert(0 <= ppno);
    csolve_assert(ppno < 1000);
    dummyassert(page_protected[ppno] == 0 || pages[ppno] > 0);
    validptr(page_protected + ppno);
    return page_protected[ppno] != 0;
}

void page_decref(int ppno, int pages[], int page_protected[])
{
    dummyassert(!page_free(ppno, pages, page_protected));
    validptr(pages + ppno);
    // pmr: TODO assert(pages[ppno] >= 0);
    pages[ppno]--;
}

void env_check(env_t * LOC(L) env, env_t * LOC(L) *envs, int pages[], int page_protected[])
{
    int i, found;
    env_t *walk;
    int ppi = 0;
    int a = csolve_assume(env != (env_t*) 0); // pmr: precondition

    dummyassert(is_page_protected(env->env_mypp, pages, page_protected));

    for (i = 0; i < 2000; i++){
        csolve_assert(0 <= i);
	csolve_assert(i < 2000);
	ppi = env->env_pgdir[i]; //RECHECK ISSUE -- SAVE result in ppi
        if (ppi >= 0) {
            dummyassert(!page_free(ppi, pages, page_protected));
            dummyassert(!is_page_protected(ppi, pages, page_protected));
        }
    }

    for (walk = *envs, found = 0; walk; walk = walk->env_next){
        if (walk == env){
            found = 1;
	}
    }
    dummyassert(found);
}

void mem_check(env_t **envs, int pages[], int page_protected[])
{
    int i;
    int *lpages;
    env_t *walk;

    int ppi = 0;

    lpages = (int *)malloc(1000 * sizeof(int));

    for (i = 0; i < 1000; i++){
        validptr(lpages + i);
        lpages[i] = 0;
    }

    for (walk = *envs; walk; walk = walk->env_next) {
        dummyassert(is_page_protected(walk->env_mypp, pages, page_protected));

	//csolve_assert(0); SANITY
        validptr(lpages + walk->env_mypp);
        lpages[walk->env_mypp]++;
        for (i = 0; i < 2000; i++){
            csolve_assert(0 <= i); csolve_assert(i < 2000);
            ppi = walk->env_pgdir[i]; // RECHECK ISSUE
	    if (ppi >= 0) {
                dummyassert(!is_page_protected(ppi, pages, page_protected));
                validptr(lpages + ppi);
                lpages[ppi]++;
            }
	}
    }
    for (i = 0; i < 1000; i++) {
        validptr(lpages + i);
        validptr(pages + i);
        dummyassert(lpages[i] == pages[i]);
        dummyassert(lpages[i] > 0 || page_protected[i] == 0);
    }
}

env_t * LOC(L) env_alloc(env_t * LOC(L) *envs, int pages[], int page_protected[])
{
    env_t *env;
    env_t *tmp;
    
    int i, env_pp = page_getfree(pages);

    int a = csolve_assume(envs != (env_t **) 0); // pmr: precondition
    
    if (env_pp < 0)
        return 0;

    csolve_assert(0 <= env_pp);
    csolve_assert(env_pp < 1000);
    env = (env_t *) malloc(sizeof(env_t));

    env->env_mypp = env_pp;
    for (i = 0; i < 2000; i++){
        csolve_assert(0 <= i); csolve_assert(i < 2000);
        env->env_pgdir[i] = -1;
    }
    tmp = *envs; // PURIFIER ISSUE
    env->env_next = tmp;
    env->env_prev = 0;
    // pmr: Used to be tmp == 0!
    if (tmp != 0){
        tmp->env_prev = env;
    }
    *envs = env;

    validptr(pages + env_pp);
    pages[env_pp]++;
    
    validptr(page_protected + env_pp);
    page_protected[env_pp] = 1;
    env_check(env, envs, pages, page_protected);
    mem_check(envs, pages, page_protected);
    // csolve_assert(0); SANITY
    return env;
}

void env_free(env_t * LOC(L) env, env_t * LOC(L) *envs, int pages[], int page_protected[])
{
    int i;
    int ppi = 0;

    int a = csolve_assume(env != (env_t*) 0); // pmr: precondition
    int a = csolve_assume(envs != (env_t**) 0); // pmr: precondition
    
    env_check(env, envs, pages, page_protected);

    for (i = 0; i < 2000; i++){
        csolve_assert(0 <= i); csolve_assert(i < 2000);

	ppi = env->env_pgdir[i];
	if (ppi >= 0){
            page_decref(ppi, pages, page_protected);
	}
    }
    validptr(page_protected + env->env_mypp);
    page_protected[env->env_mypp] = 0;
    page_decref(env->env_mypp, pages, page_protected);
    dummyassert(page_free(env->env_mypp, pages, page_protected));

    env_t *t;
    t = env->env_next;
    if (t)
        t->env_prev = env->env_prev;
    t = env->env_prev;
    if (t)
        t->env_next = env->env_next;
    else
        *envs = env->env_next;

    //free(env); can't deal with concrete input -- fix Refanno
    mem_check(envs, pages, page_protected);
}

int page_alloc(env_t * LOC(L) env, int NONNEG USE_INDEX vp, env_t * LOC(L) *envs, int pages[], int page_protected[])
{
    int pp;
    int tmp; // RECHECK ISSUE

    pp = page_getfree(pages);
    if (pp < 0)
        return -1;

    csolve_assert(0 <= vp); csolve_assert(vp < 2000);
    tmp = env->env_pgdir[vp];
    if (tmp >= 0){
        page_decref(tmp, pages, page_protected);
    }
    
    csolve_assert(0 <= vp); csolve_assert(vp < 2000);
    env->env_pgdir[vp] = pp;
    validptr(pages + pp);
    pages[pp]++;

    env_check(env, envs, pages, page_protected);
    mem_check(envs, pages, page_protected);
    
    return 0;
}

void page_unmap(env_t * LOC(L) env, int NONNEG USE_INDEX vp, env_t * LOC(L) *envs, int pages[], int page_protected[])
{
    int tmp; // RECHECK ISSUE

    csolve_assert(0 <= vp); csolve_assert(vp < 2000);
    tmp = env->env_pgdir[vp];
    if (tmp >= 0){
        page_decref(tmp, pages, page_protected);
    }
    env_check(env, envs, pages, page_protected);
    mem_check(envs, pages, page_protected);
}

int page_map(env_t * LOC(L) srcenv, int NONNEG USE_INDEX srcvp, env_t * LOC(L) dstenv, int NONNEG USE_INDEX dstvp,
             env_t * LOC(L) *envs, int pages[], int page_protected[])
{
    int tmp, tmp2; // RECHECK ISSUE

    int a = csolve_assume(srcenv != (env_t*) 0); // pmr: precondition
    int a = csolve_assume(dstenv != (env_t*) 0); // pmr: precondition

    csolve_assert(srcvp >= 0); csolve_assert(srcvp < 2000);
    csolve_assert(dstvp >= 0); csolve_assert(dstvp < 2000);

    env_check(srcenv, envs, pages, page_protected);
    env_check(dstenv, envs, pages, page_protected);

    tmp2 = srcenv->env_pgdir[srcvp];
    if (tmp2 < 0)
        return -1;

    tmp = dstenv->env_pgdir[dstvp];
    if (tmp >= 0){
        page_decref(tmp, pages, page_protected);
    }

    dstenv->env_pgdir[dstvp] = tmp2;
    validptr(pages + tmp2);
    pages[tmp2]++;

    env_check(dstenv, envs, pages, page_protected);
    mem_check(envs, pages, page_protected);

    return 0;
}

void main(/* env_t *envs, int pages[], int page_protected[] */)
{
    env_t **envs;
    int *pages;
    int *page_protected;
    int i;
    int jhalatemp;
    int p;
    int vp, vp2;

    pages = (int *) malloc(1000 * sizeof(int));
    page_protected = (int *) malloc(1000 * sizeof(int));
    for (i = 0; i < 1000; i++){
      validptr(pages + i);
      pages[i] = 0;
      validptr(page_protected + i);
      page_protected[i] = 0;
    }

    jhalatemp = page_getfree(pages);

    envs = (env_t *) malloc(sizeof(env_t *));
    
    //*envs = (env_t *) 0;
    env_t *zz = (env_t *) 0;
    *envs = zz;

    env_t *e = env_alloc(envs, pages, page_protected);

    if (e == 0)
        return;

    env_check(e, envs, pages, page_protected);
    env_free(e, envs, pages, page_protected);

    env_t *e2 = env_alloc(envs, pages, page_protected);

    p = 0;
    vp = 0;
    // Not having this line causes an unknown name error on some vp2 phi node!!!
    vp2 = 0;
    while (nondet()) {
	vp = nondetpos();
        if (0 <= vp && vp < 2000) {
          p = page_alloc(e, vp, envs, pages, page_protected);

          vp2 = nondetpos();
          if (0 <= vp2 && vp2 < 2000) {
              page_map(e, vp, e2, vp2, envs, pages, page_protected);
              page_unmap(e, vp, envs, pages, page_protected);
          }
        }
    }
}

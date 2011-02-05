extern char *malloc(int);
extern void free(char *);

typedef struct env {
    int env_mypp;
    struct env *env_prev;
    struct env *env_next;
    int env_pgdir[2000];
} env_t;

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
  assert(0 <= ppno);
  assert(ppno < 1000);
  dummyassert(pages[ppno] > 0 || page_protected[ppno] == 0);
  validptr(pages + ppno);
  return pages[ppno] == 0;
}

int is_page_protected(int ppno, int pages[], int page_protected[])
{
    assert(0 <= ppno);
    assert(ppno < 1000);
    dummyassert(page_protected[ppno] == 0 || pages[ppno] > 0);
    validptr(page_protected + ppno);
    return page_protected[ppno] != 0;
}

void page_decref(int ppno, int pages[], int page_protected[])
{
    dummyassert(!page_free(ppno, pages, page_protected));
    validptr(pages + ppno);
    pages[ppno]--;
}


void env_check(env_t *env, env_t *envs, int pages[], int page_protected[])
{
    int i, found;
    env_t *walk;
    int ppi = 0;

    env = env;	//THETA ISSUE
    envs = envs;//THETA ISSUE

    dummyassert(is_page_protected(env->env_mypp, pages, page_protected));

    for (i = 0; i < 2000; i++){
        assert(0 <= i);
	assert(i < 2000);
	ppi = env->env_pgdir[i]; //RECHECK ISSUE -- SAVE result in ppi
        if (ppi >= 0) {
            dummyassert(!page_free(ppi, pages, page_protected));
            dummyassert(!is_page_protected(ppi, pages, page_protected));
        }
    }

    for (walk = envs, found = 0; walk; walk = walk->env_next){
        if (walk == env){
            found = 1;
	}
    }
    dummyassert(found);
}

void mem_check(env_t *envs, int pages[], int page_protected[])
{
    int i;
    int *lpages;
    env_t *walk;

    int ppi = 0;

    lpages = (int *)malloc(1000);

    for (i = 0; i < 1000; i++){
        validptr(lpages + i);
        lpages[i] = 0;
    }

    for (walk = envs; walk; walk = walk->env_next) {
        dummyassert(is_page_protected(walk->env_mypp, pages, page_protected));

	//assert(0); SANITY
        validptr(lpages + walk->env_mypp);
        lpages[walk->env_mypp]++;
        for (i = 0; i < 2000; i++){
            assert(0 <= i); assert(i < 2000);
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

env_t *env_alloc(env_t *envs, int pages[], int page_protected[])
{
    env_t *env;
    
    int i, env_pp = page_getfree(pages);
    
    if (env_pp < 0)
        return 0;

    assert(0 <= env_pp);
    assert(env_pp < 1000);
    env = (env_t *) malloc(sizeof(env_t));

    env->env_mypp = env_pp;
    for (i = 0; i < 2000; i++){
        assert(0 <= i); assert(i < 2000);
        env->env_pgdir[i] = -1;
    }
    env->env_next = envs;
    env->env_prev = 0;
    if (envs){
        envs->env_prev = env;
    }
    envs = env;

    validptr(pages + env_pp);
    pages[env_pp]++;
    
    validptr(page_protected + env_pp);
    page_protected[env_pp] = 1;
    env_check(env, envs, pages, page_protected);
    mem_check(envs, pages, page_protected);
    // assert(0); SANITY
    return env;
}

void env_free(env_t *env, env_t *envs, int pages[], int page_protected[])
{
    int i;
    int ppi = 0;
    env = env;	//THETA ISSUE
    envs = envs; //THETA ISSUE
    env_check(env, envs, pages, page_protected);

    for (i = 0; i < 2000; i++){
        assert(0 <= i); assert(i < 2000);

	ppi = env->env_pgdir[i];
	if (ppi >= 0){
            page_decref(ppi, pages, page_protected);
	}
    }
    validptr(page_protected + env->env_mypp);
    page_protected[env->env_mypp] = 0;
    page_decref(env->env_mypp, pages, page_protected);
    dummyassert(page_free(env->env_mypp, pages, page_protected));

    if (env->env_next)
        env->env_next->env_prev = env->env_prev;
    if (env->env_prev)
        env->env_prev->env_next = env->env_next;
    else
        envs = env->env_next;

    //free(env); can't deal with concrete input -- fix Refanno
    mem_check(envs, pages, page_protected);
}

int page_alloc(env_t *env, int vp, env_t *envs, int pages[], int page_protected[])
{
    int pp;
    int tmp; // RECHECK ISSUE

    pp = page_getfree(pages);
    if (pp < 0)
        return -1;

    assert(0 <= vp); assert(vp < 2000);
    tmp = env->env_pgdir[vp];
    if (tmp >= 0){
        page_decref(tmp, pages, page_protected);
    }
    
    assert(0 <= vp); assert(vp < 2000);
    env->env_pgdir[vp] = pp;
    validptr(pages + pp);
    pages[pp]++;

    env_check(env, envs, pages, page_protected);
    mem_check(envs, pages, page_protected);
    
    return 0;
}

void page_unmap(env_t *env, int vp, env_t *envs, int pages[], int page_protected[])
{
    int tmp; // RECHECK ISSUE

    assert(0 <= vp); assert(vp < 2000);
    tmp = env->env_pgdir[vp];
    if (tmp >= 0){
        page_decref(tmp, pages, page_protected);
    }
    env_check(env, envs, pages, page_protected);
    mem_check(envs, pages, page_protected);
}

int page_map(env_t *srcenv, int srcvp, env_t *dstenv, int dstvp, env_t *envs, int pages[], int page_protected[])
{
    int tmp, tmp2; // RECHECK ISSUE

    assert(srcvp >= 0); assert(srcvp < 2000);
    assert(dstvp >= 0); assert(dstvp < 2000);

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
    pages[dstenv->env_pgdir[dstvp]]++;

    env_check(dstenv, envs, pages, page_protected);
    mem_check(envs, pages, page_protected);

    return 0;
}

void main(/* env_t *envs, int pages[], int page_protected[] */)
{
    env_t *envs;
    int *pages;
    int *page_protected;
    int i;
    int jhalatemp;
    int p;
    int vp, vp2;

    pages = (int *) malloc(1000);
    page_protected = (int *) malloc(1000);
    for (i = 0; i < 1000; i++){
      validptr(pages + i);
      pages[i] = 0;
      validptr(page_protected + i);
      page_protected[i] = 0;
    }

    jhalatemp = page_getfree(pages);

    envs = (env_t *) 0;
    env_t *e = env_alloc(envs, pages, page_protected);
   
    if (e!=0) {
        env_check(e, envs, pages, page_protected);
        env_free(e, envs, pages, page_protected);
    }

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

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
    for (i = 0; i < 1000; i++)
        if (pages[i] == 0)
            return i;
    return -1;
}

int page_free(int ppno, int pages[], int page_protected[])
{
    assert(ppno >= 0 && ppno < 1000);
    assert(pages[ppno] > 0 || page_protected[ppno] == 0);
    return pages[ppno] == 0;
}

int is_page_protected(int ppno, int pages[], int page_protected[])
{
    assert(ppno >= 0 && ppno < 1000);
    assert(page_protected[ppno] == 0 || pages[ppno] > 0);
    return page_protected[ppno] != 0;
}

void page_decref(int ppno, int pages[], int page_protected[])
{
    assert(!page_free(ppno, pages, page_protected));
    pages[ppno]--;
}

void env_check(env_t *env, env_t *envs, int pages[], int page_protected[])
{
    int i, found;
    env_t *walk;

    assert(is_page_protected(env->env_mypp, pages, page_protected));

    for (i = 0; i < 2000; i++)
        if (env->env_pgdir[i] >= 0) {
            assert(!page_free(env->env_pgdir[i], pages, page_protected));
            assert(!is_page_protected(env->env_pgdir[i], pages, page_protected));
        }

    for (walk = envs, found = 0; walk; walk = walk->env_next)
        if (walk == env)
            found = 1;
    assert(found);
}

void mem_check(env_t *envs, int pages[], int page_protected[])
{
    int i, lpages[1000];
    env_t *walk;

    for (i = 0; i < 1000; i++)
        lpages[i] = 0;

    for (walk = envs; walk; walk = walk->env_next) {
        assert(is_page_protected(walk->env_mypp, pages, page_protected));

        lpages[walk->env_mypp]++;
        for (i = 0; i < 2000; i++)
            if (walk->env_pgdir[i] >= 0) {
                assert(!is_page_protected(walk->env_pgdir[i], pages, page_protected));
                lpages[walk->env_pgdir[i]]++;
            }
    }
    for (i = 0; i < 1000; i++) {
        assert(lpages[i] == pages[i]);
        assert(lpages[i] > 0 || page_protected[i] == 0);
    }
}

env_t *env_alloc(env_t *envs, int pages[], int page_protected[])
{
    env_t *env;
    int i, env_pp = page_getfree(pages);

    if (env_pp < 0)
        return 0;

    env = (env_t *) malloc(sizeof(env_t));

    env->env_mypp = env_pp;
    for (i = 0; i < 2000; i++)
        env->env_pgdir[i] = -1;

    env->env_next = envs;
    env->env_prev = 0;
    if (envs)
        envs->env_prev = env;
    envs = env;

    pages[env_pp]++;
    page_protected[env_pp] = 1;
    env_check(env, envs, pages, page_protected);
    mem_check(envs, pages, page_protected);
    return env;
}

void env_free(env_t *env, env_t *envs, int pages[], int page_protected[])
{
    int i;
    env_check(env, envs, pages, page_protected);

    for (i = 0; i < 2000; i++)
        if (env->env_pgdir[i] >= 0)
            page_decref(env->env_pgdir[i], pages, page_protected);
    page_protected[env->env_mypp] = 0;
    page_decref(env->env_mypp, pages, page_protected);
    assert(page_free(env->env_mypp, pages, page_protected));

    if (env->env_next)
        env->env_next->env_prev = env->env_prev;
    if (env->env_prev)
        env->env_prev->env_next = env->env_next;
    else
        envs = env->env_next;

    free(env);
    mem_check(envs, pages, page_protected);
}

int page_alloc(env_t *env, int vp, env_t *envs, int pages[], int page_protected[])
{
    int pp;
    assert(vp >= 0 && vp < 2000);

    pp = page_getfree(pages);
    if (pp < 0)
        return -1;

    if (env->env_pgdir[vp] >= 0)
        page_decref(env->env_pgdir[vp], pages, page_protected);

    env->env_pgdir[vp] = pp;
    pages[pp]++;

    env_check(env, envs, pages, page_protected);
    mem_check(envs, pages, page_protected);
    return 0;
}

void page_unmap(env_t *env, int vp, env_t *envs, int pages[], int page_protected[])
{
    assert(vp >= 0 && vp < 2000);
    if (env->env_pgdir[vp] >= 0)
        page_decref(env->env_pgdir[vp], pages, page_protected);
    env_check(env, envs, pages, page_protected);
    mem_check(envs, pages, page_protected);
}

int page_map(env_t *srcenv, int srcvp, env_t *dstenv, int dstvp, env_t *envs, int pages[], int page_protected[])
{
    assert(srcvp >= 0 && srcvp < 2000);
    assert(dstvp >= 0 && dstvp < 2000);

    env_check(srcenv, envs, pages, page_protected);
    env_check(dstenv, envs, pages, page_protected);

    if (srcenv->env_pgdir[srcvp] < 0)
        return -1;

    if (dstenv->env_pgdir[dstvp] >= 0)
        page_decref(dstenv->env_pgdir[dstvp], pages, page_protected);
    dstenv->env_pgdir[dstvp] = srcenv->env_pgdir[srcvp];
    pages[dstenv->env_pgdir[dstvp]]++;

    env_check(dstenv, envs, pages, page_protected);
    mem_check(envs, pages, page_protected);

    return 0;
}

void main(env_t *envs, int pages[], int page_protected[])
{
    env_t *e = env_alloc(envs, pages, page_protected);

    if (e!=0) {
        env_check(e, envs, pages, page_protected);
        env_free(e, envs, pages, page_protected);
    }
}

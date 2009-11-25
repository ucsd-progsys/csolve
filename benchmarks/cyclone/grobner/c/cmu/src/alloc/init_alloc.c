#include <cthreads.h>

char *malloc_par(),*malloc_par2(),*malloc();
free_par();
free_par2();
free();

int alloc_policy=0;

char *(*malloc_f)();
(*free_f)();

int name = 0;

init_alloc()
{
	if (alloc_policy == 2) {
	        malloc_f = malloc_par2;
		free_f = free_par2;
		if (cthread_data(cthread_self())==0)
		        cthread_set_data(cthread_self(),(any_t)&name);
		init_alloc_par2();
	}
	else if (alloc_policy == 1) {
		malloc_f = malloc_par;
		free_f = free_par;
		if (cthread_data(cthread_self())==0)
			cthread_set_data(cthread_self(),(any_t)&name);
		init_alloc_par();
	}
	else {
		malloc_f = malloc;
		free_f = free;
	}
}

#include <stdlib.h>

extern struct group * NNOK getgrnam (char * STRINGPTR __name) OKEXTERN;

//extern struct group * NNSTART NNVALIDPTR NNROOM_FOR(struct group) getgrnam (char * STRINGPTR __name) OKEXTERN;

struct group
  {
    char *gr_name;		/* Group name.	*/
    char *gr_passwd;    /* Password.	*/
    int  gr_gid;		/* Group ID.	*/
    char **gr_mem;		/* Member list.	*/
  };


int main(char * STRINGPTR name) CHECK_TYPE
{
  int gid = 0;

  struct group *grp = getgrnam(name);
  if (grp) 
    gid = grp->gr_gid;
  
  return 0;
}

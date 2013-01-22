
#include <stdlib.h>
#include <csolve.h>

/* The group structure.	 */
struct group
  {
    char *gr_name;		/* Group name.	*/
    char *gr_passwd;	/* Password.	*/
    int  gr_gid;		/* Group ID.	*/
    char **gr_mem;		/* Member list.	*/
  };

extern struct group * NNOK getgrnam (__const char * STRINGPTR __name) OKEXTERN;

int parse_group (const char * name) CHECK_TYPE
{
  int gid = -1;

  if (name)
    {
      struct group *grp = getgrnam (name);
      if (grp) gid = grp->gr_gid;
    }

  return gid;
}

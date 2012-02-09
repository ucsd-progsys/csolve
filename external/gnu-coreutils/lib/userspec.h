#ifndef USERSPEC_H
# define USERSPEC_H 1

# include <sys/types.h>

const char * NNSTRINGPTR
parse_user_spec (const char * STRINGPTR spec_arg, uid_t *uid, gid_t *gid,
                 char * NNSTART NNSTRINGPTR *username_arg, char * NNSTART NNSTRINGPTR *groupname_arg) OKEXTERN;

#endif

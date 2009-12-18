#ifndef __UNISTD_H__
#define __UNISTD_H__

#include <stdlib.h>
#include <inttypes.h>
#include <liquidc.h>
#include <types.h>

# ifndef __gid_t_defined
typedef __gid_t gid_t;
#  define __gid_t_defined
# endif

# ifndef __uid_t_defined
typedef __uid_t uid_t;
#  define __uid_t_defined
# endif

# ifndef __off_t_defined
#  ifndef __USE_FILE_OFFSET64
typedef __off_t off_t;
#  else
typedef __off64_t off_t;
#  endif
#  define __off_t_defined
# endif
# if defined __USE_LARGEFILE64 && !defined __off64_t_defined
typedef __off64_t off64_t;
#  define __off64_t_defined
# endif

# ifndef __useconds_t_defined
typedef __useconds_t useconds_t;
#  define __useconds_t_defined
# endif

# ifndef __pid_t_defined
typedef __pid_t pid_t;
#  define __pid_t_defined
# endif
#endif	/* X/Open */

#if defined __USE_MISC || defined __USE_XOPEN_EXTENDED
# ifndef __intptr_t_defined
typedef __intptr_t intptr_t;
#  define __intptr_t_defined
# endif
#endif

#if defined __USE_BSD || defined __USE_XOPEN
# ifndef __socklen_t_defined
typedef __socklen_t socklen_t;
#  define __socklen_t_defined
# endif

extern int          access(const char * ARRAY, int);
extern unsigned int alarm(unsigned int);
extern int          brk(void *);
extern int          chdir(const char * ARRAY);
extern int          chown(const char * ARRAY, uid_t, gid_t);
extern int          close(int);
extern size_t       confstr(int, char * ARRAY, size_t);
extern char        *crypt(const char * ARRAY, const char * ARRAY);
extern char        *ctermid(char * ARRAY);
extern int          dup(int);
extern int          dup2(int, int);
extern void         encrypt(char[64], int);
extern int          execl(const char * ARRAY, const char * ARRAY, ...);
extern int          execle(const char * ARRAY, const char * ARRAY, ...);
extern int          execlp(const char * ARRAY, const char * ARRAY, ...);
extern int          execv(const char * ARRAY, char * ARRAYconst []);
extern int          execve(const char * ARRAY, char * ARRAYconst [], char * ARRAYconst []);
extern int          execvp(const char * ARRAY, char * ARRAYconst []);
extern void        _exit(int);
extern int          fchown(int, uid_t, gid_t);
extern int          fchdir(int);
extern int          fdatasync(int);
extern pid_t        fork(void);
extern long int     fpathconf(int, int);
extern int          fsync(int);
extern int          ftruncate(int, off_t);
extern char        *getcwd(char * ARRAY, size_t);
extern gid_t        getegid(void);
extern uid_t        geteuid(void);
extern gid_t        getgid(void);
extern int          getgroups(int, gid_t []);
extern long         gethostid(void);
extern char        *getlogin(void);
extern int          getlogin_r(char * ARRAY, size_t);
extern int          getopt(int, char * ARRAY const [], const char * ARRAY);
extern pid_t        getpgid(pid_t);
extern pid_t        getpgrp(void);
extern pid_t        getpid(void);
extern pid_t        getppid(void);
extern pid_t        getsid(pid_t);
extern uid_t        getuid(void);
extern char        *getwd(char * ARRAY);
extern int          isatty(int);
extern int          lchown(const char * ARRAY, uid_t, gid_t);
extern int          link(const char * ARRAY, const char * ARRAY);
extern int          lockf(int, int, off_t);
extern off_t        lseek(int, off_t, int);
extern int          nice(int);
extern long int     pathconf(const char * ARRAY, int);
extern int          pause(void);
extern int          pipe(int [2]);
extern ssize_t      pread(int, void *, size_t, off_t);
extern int          pthread_atfork(void (*)(void), void (*)(void),
extern                  void(*)(void));
extern ssize_t      pwrite(int, const void *, size_t, off_t);
extern ssize_t      read(int, void *, size_t);
extern int          readlink(const char * ARRAY, char * ARRAY, size_t);
extern int          rmdir(const char * ARRAY);
extern void        *sbrk(intptr_t);
extern int          setgid(gid_t);
extern int          setpgid(pid_t, pid_t);
extern pid_t        setpgrp(void);
extern int          setregid(gid_t, gid_t);
extern int          setreuid(uid_t, uid_t);
extern pid_t        setsid(void);
extern int          setuid(uid_t);
extern unsigned int sleep(unsigned int);
extern void         swab(const void *, void *, ssize_t);
extern int          symlink(const char * ARRAY, const char * ARRAY);
extern void         sync(void);
extern long int     sysconf(int);
extern pid_t        tcgetpgrp(int);
extern int          tcsetpgrp(int, pid_t);
extern int          truncate(const char * ARRAY, off_t);
extern char        *ttyname(int);
extern int          ttyname_r(int, char * ARRAY, size_t);
extern useconds_t   ualarm(useconds_t, useconds_t);
extern int          unlink(const char * ARRAY);
extern int          usleep(useconds_t);
extern pid_t        vfork(void);
extern ssize_t      write(int, const void *, size_t);

#endif

#ifndef __DIRENT_H__
#define __DIRENT_H__

struct __dirstream {
    int dummy;
};

typedef struct __dirstream DIR;

struct dirent {
    ino_t d_ino;
    char  d_name[];
};

int            closedir(DIR *);
DIR           *opendir(const char * ARRAY);
struct dirent *readdir(DIR *);
int            readdir_r(DIR *, struct dirent *, struct dirent * ARRAY *);
void           rewinddir(DIR *);
void           seekdir(DIR *, long int);
long int       telldir(DIR *);

#endif

//! run with -manual -nop

struct dirent {
    int  d_ino;
    char d_name[];
};

extern void *malloc(int);
extern void isEsperanto(const char * __attribute__((array)));

void main () {
    struct dirent *dep;

    dep = (struct dirent *)malloc(sizeof(struct dirent));
    dep->d_ino = 0;
    isEsperanto(dep->d_name);
}

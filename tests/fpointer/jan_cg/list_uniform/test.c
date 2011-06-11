/** Test with uniform lists */

#include <stdio.h>
#include <stdlib.h>


typedef struct file {
  int (*write) (const char *);
  struct file *siblings;
} file_t;

typedef struct dir {
  file_t * (*newFile) (struct dir *);
  int (*writeToAll) (struct file *, const char *);
  struct file *files;
} dir_t;

/* hmm... linux also has dentries for every file, and dentry->d_parent
   is the directory. They also have mount information, so maybe
   it's based on that instead */

/*** Generic funcs ***/

file_t * allocFile() {
  return (file_t *)malloc(sizeof(file_t));
}

int writeToAll(file_t *head, const char *msg) {
  int written = 0;
  while (head != NULL) {
    written += head->write(msg);
    head = head->siblings;
  }
  return written;
}

/*** FooFS ***/

int fooFS_write (const char *data) {
  printf("foo: %s\n", data);
  return (strlen(data));
}

file_t * fooFS_newFile(struct dir *dir) {
  file_t *newFile = allocFile();
  newFile->write = &fooFS_write;
  newFile->siblings = dir->files;
  dir->files = newFile;
  return newFile;
}

dir_t * fooFS_newDir() {
  dir_t *newDir = (dir_t *)malloc(sizeof(dir_t));
  newDir->newFile = &fooFS_newFile;
  newDir->writeToAll = &writeToAll;
  newDir->files = NULL;
  return newDir;
}

/*** BarFS ***/

int barFS_write (const char *data) {
  printf("bar: %s\n", data);
  return (strlen(data));
}

file_t * barFS_newFile(struct dir *dir) {
  file_t *newFile = allocFile();
  newFile->write = &barFS_write;
  newFile->siblings = dir->files;
  dir->files = newFile;
  return newFile;
}

dir_t * barFS_newDir() {
  dir_t *newDir = (dir_t *)malloc(sizeof(dir_t));
  newDir->newFile = &barFS_newFile;
  newDir->writeToAll = &writeToAll;
  newDir->files = NULL;
  return newDir;
}

void testDir (struct dir *d) {
  file_t *aFile;
  int ign;
  aFile = d->newFile(d); //one of the two...
  aFile->write("swan");             
  aFile = d->newFile(d);
  ign = d->writeToAll(d->files, "quack");
  return;
}

/*** Main ***/

int main(int argc, char *argv[]) {

  if (argc == 2) {
    dir_t *aDir;

    if (strcmp(argv[1], "foo") == 0) {
      aDir = fooFS_newDir ();
      testDir(aDir);
    } else if(strcmp(argv[1], "bar") == 0) {
      aDir = barFS_newDir ();
      testDir(aDir);
    }

    testDir(aDir); // should try to focus here too, or allow a set of inputs ?
    
  } else {
    printf ("Pick a damn file system!\n");
  }

  return 0;
}

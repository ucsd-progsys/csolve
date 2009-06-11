//Include this file in your program if you also want to compile without Deputy.
//This file has no effect when you use Deputy to compile.

#ifndef NODEPUTY_H
#define NODEPUTY_H

#ifndef DEPUTY

#define BND(lo, hi)
#define COUNT(n)
#define SAFE
#define SNT

#define SIZE(n)

#define NULLTERM
#define NT
#define NTS

#define NTDROPATTR
#define NTEXPANDATTR

#define NULLABLE
#define OPT
#define NONNULL

#define ASSUMECONST

#define POLY 

#define TRUSTED

#define WHEN(e)

#define DMEMCPY(x, y, z)
#define DMEMSET(x, y, z)
#define DMEMCMP(x, y, z)

#define DALLOC(x)
#define DVARARG(x)
#define DPRINTF(x)

#define NTDROP(x)       (x)
#define NTEXPAND(x)     (x)

#define TC(x)           (x)

#define DEPUTY_NUM_CHECKS_ADDED 0

#endif // DEPUTY not defined

#endif // NODEPUTY_H

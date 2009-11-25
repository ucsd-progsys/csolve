extern char *varnames[];
extern short nvars;
/* ----------------------new definitions to avoid LMPOLs--------------------------------- */
typedef struct{
	int npols;
	MPOL *polp;
} polsettype;

#if oldpair_
	typedef struct{
		MPOL *p1, *p2;
	} polpairtype;
	typedef struct{
		int npairs;
		polpairtype *polpair;
	} polpairsettype;
#endif

#define lpp_lbd_pair(p1,p2,exp) expomax(MEXPO((p1),0), MEXPO((p2),0),(exp))
#define NULL 0

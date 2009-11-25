/*
	central pair queue --- to reduce pair overhead in interredcution
*/

#define CP_INIT 512 /* start size: grow to double, shrink to half */
#define CP_DQOK 1
#define CP_NONE 2   /* for dequeue status */

#define CP_PARENT(x)       (((x) - 1) / 2)
#define CP_LEFT(x)         (2 * (x) + 1)
#define CP_RIGHT(x)        (2 * (x) + 2)

typedef struct {
	int empty;
	int ppe, pid, qpe, qid;
	short merit[NVARS+1];
} CP_type;

extern int CentralInit ( void );
	/*
		return 1 if all ok, 0 else
	*/

extern void CentralEnqueue ( CP_type *newPtr, int *balancePtr );
	/*
		a copy of *newPtr is made, so it can be junked after the call
	*/

extern int CentralDequeue ( CP_type *oldPtr, int *balancePtr );
	/*
		returns 1 for successful dequeue, 0 if none available
	*/

extern void InitialPairsDone ( void );
extern int PairsReady ( void );


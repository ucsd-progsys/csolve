typedef struct {
	int where;
	int bytes;
	MPOL *polPtr;
} RED_type;

void ReductPoll ( void );
void ReductInit ( void );
int ReductPut ( MPOL *red, int *balancePtr );
MPOL *ReductGet ( int *balancePtr );


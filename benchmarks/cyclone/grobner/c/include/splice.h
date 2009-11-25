#include "tq-1.h"

typedef struct {
	int pe[2], id[2];
	int merit;
} PairType;

void PairInit ( int nworkers );
int PairEnqueue ( PairType *pairPtr );
int	PairDequeue ( PairType *pairPtr );
inline void PairPoll ( void );


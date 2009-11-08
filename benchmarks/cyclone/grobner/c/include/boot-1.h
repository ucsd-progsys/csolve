#ifndef	BOOT

#define	BOOT
#define	MAXNPE	256
#define	NPE		(CMNA_partition_size)
#define	self	(CMNA_self_address)
#define	host	NPE

#define	NVARS	50		/* max # variables */
#define	LVAR	4		/* max length of identifier */

extern	short	nvars;

extern	int nworkers;

extern	int npoly;

extern	int	spy,		order_exp,	first_group,	zero_poly_count,
		order_on_pols,	order_on_pairs,	redgcd,		reduction_first,
		deletion,		allow_same_lpp;

extern	char	namebuf[NVARS*LVAR];
extern	char	*varnames[NVARS];

typedef struct{

	short	nvars;

	int nworkers;

	int	npoly;

	int	spy,			order_exp,	first_group,	zero_poly_count,
		order_on_pols,	order_on_pairs,	redgcd,		reduction_first,
		deletion,		allow_same_lpp;

	void	(*hlock_handler)(),		(*hunlock_handler)(),
			(*hprintf_handler)(),	(*hexit_handler)();

	char	namebuf[NVARS*LVAR];
	char	*varnames[NVARS];
} boottype;

#define	YES	100
#define	NO	200

#define	MAXINT		~ ( 1 << ( 8 * sizeof(int) - 1 ) )

#define RUNTIME     0
#define TERMTIME    1
#define BCASTTIME   2
#define REDTIME     3
#define DEQTIME     4
#define LOCKTIME    5
#define IDLETIME    6
#define CSTIME		7
#define	VALID		8
#define	SPOL		9
#define	GENPAIR		10
#define	MALLOC		11
#define	PACK		12

#define	PAIR_DEQ_SERVER	63
#define	TICKET_SERVER	62
#define	ASKTERM			61
#define	BUSY			60

#define A_EVENT 0
#define R_EVENT 1
#define S_EVENT 2

#endif	BOOT


/* ---------------------------------------------------------------------- */
#ifndef	BOOT

#define	BOOT

#define MAXNPE  256
#ifndef NPE
#	define	NPE		(CMNA_partition_size)
#endif
#define	self	(CMNA_self_address)
#define	host	NPE

#define	NVARS	50		/* max # variables */
#define	LVAR	4		/* max length of identifier */

/* ---------------------------------------------------------------------- */

extern	int		first_group,	order_exp,		order_on_pols,
				nworkers,		startpol;
extern	short	nvars;
extern	char	*varnames[NVARS],
				namebuf[NVARS*LVAR];

typedef struct{
	int		first_group,	order_exp,	order_on_pols,	nworkers, startpol;
	short	nvars;
	char	*varnames[NVARS],
			namebuf[NVARS*LVAR];
} boottype;

/* ---------------------------------------------------------------------- */

#define	YES	100
#define	NO	200

#define	MAXPEND	4000	/* max pending invalidation entries */
#define	ADD		100
#define	DELETE	101
#define	MODIFY	102

#define	A_EVENT	0
#define	R_EVENT	1
#define	S_EVENT	2
#define	I_EVENT	3

#define	MAXINT		~ ( 1 << ( 8 * sizeof(int) - 1 ) )

/* ---------------------------------------------------------------------- */
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
#define	INTER		12

#define	PAIR_DEQ_SERVER	63
#define	TICKET_SERVER	62
#define	ASKTERM			61
#define	BUSY			60

/* prototypes */

void hprintf ( char *s );
void habort ( void );
void hexit ( void );

MPOL *getpolfromhost ( int px );
void new_mpolsetadd( polsettype *s, MPOL *p );
int new_mpolsetremove ( polsettype *s, int who, int which );

int getppid ( void );

void shm_invalidate ( int type, MPOL *p );
int shm_validp ( void );
void shm_validate ( void );

int MPOL_sizeof(MPOL *poly);
char *pack(MPOL *p);
inline MPOL *MPOLptr(char *b);
inline char *packbase(MPOL *p);
MPOL *unpack(char *q);
char *copypack(char *s);

void setterm ( void );
void resetterm ( void );
inline int somebusy ( void );
int termasked ( void );
void reply ( int myvote, int mycnt );
int commit ( void );
int askterm ( void );

int get_ticket ( void );
void ret_ticket ( void );

void new_spol ( MPOL *p, MPOL *q, MPOL *r );
MPOL *reduce ( MPOL *p, polsettype *psets[], int *flagp );
MPOL *inter ( MPOL *p, polsettype *psets[], int *flagp );

/* ----------------- shm stuff ----------------- */

typedef struct {
    int type, who, which;
} shmtype;



#endif	BOOT
/* ---------------------------------------------------------------------- */


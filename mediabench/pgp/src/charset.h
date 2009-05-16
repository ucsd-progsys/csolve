
/* Internal/External representation conversion macros for conversion of
 * user ID's (will be implemented in a future release of PGP, now they
 * are just dummies)
 */
#define	CONVERT_TO_CANONICAL_CHARSET(s)
            /* String to internal string (at same place) */
#define	LOCAL_CHARSET(s)	s	/* String to external string */

extern char INT_C(char c);      /* Char to internal char */
extern char EXT_C(char c);      /* Char to external char */

/* Plaintext files import/export conversion modes */

#define NO_CONV  0      /* No conversion needed */
#define INT_CONV 1      /* Convert text to internal representation */
#define EXT_CONV 2      /* Convert text to external representation */

extern int CONVERSION;  /* Global var to rule copyfiles */


extern void init_charset(void);
extern int to_upper(int c);
extern int to_lower(int c);

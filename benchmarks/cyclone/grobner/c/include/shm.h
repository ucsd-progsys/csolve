/*	shm_invalidate returns only when all other pe's have acked
*/
void shm_invalidate ( int type, MPOL *p );
int shm_validp ( void );
int shm_no_adds ( void );
int star ( MPOL *a );
void shm_validate ();
void add_del ( int who, int which );
int is_del ( int who, int which );


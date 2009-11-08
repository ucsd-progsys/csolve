/* system constants */
#define QUEUE_TERMINATE -1
#define MESH 1
#define DONT_MOVE 0x7fffffff
#define ME CMMD_self_address()

/* predefined types */
typedef struct {
        int qid;
        int home_proc;                  /* home proc for storing sync bit    */
        int task_id;                    /* handle of the task                */
        int comp_cost;                  /* estimate of the computation cost  */
        int comm_cost;                  /* estimate of the communication cost*/
        int priority;                   /* the scheduling priority           */
} task_info;

/* initialization - called once on each processor before using any taskq */
int dq_enable();

/* create a taskq on processor start_proc ... end_proc and return the queue id. */
/*    called by one processor only. the user must pass the queue id explicitly  */
/*    after creation.      							*/
int dq_create(int start_proc, int end_proc, int task_size, int topology);

/* dequeue a task and its info. return 1 if successful, 0 if unsuccessful, -1 if*/
/*    the taskq has terminate.						        */
int dq_dequeue(int qid, task_info *info, void *data);

/* enqueue a task locally. return 1 if successful, 0 if unsuccessful (no space) */
/*    user specifies computation cost of task and migration penalty (DONT_MOVE  */
/*    for task pinning)								*/
int dq_enqueue(int qid, int priority, void *data, int comp, int comm);

/* enqueue a task on a remote processor. user must ensure availability of space */
/*    on the remote processor.							*/
int dq_enqueueX(int qid, int proc, int priority, void *data, int comp, int comm);

/* query if taskq has terminated. return 1 if so and 0 otherwise 		*/
int dq_terminated(int qid);

/* signal to the taskq that the processor is not holding any task. not usable   */
/* in the current version which assumes that processors do not buffer tasks     */
int clear(int qid);

/* check on the taskq and take proper actions. must be called periodically.     */
int dq_poll(int qid);

/* check current processor load */
int dq_load(int qid, int *tasks, int *comp);


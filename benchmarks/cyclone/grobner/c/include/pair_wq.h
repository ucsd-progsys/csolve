/*----------------------------------------------------------------------*
 * pair_wq.h  --  a header file for pair_wq.c                           *
 *                                                                      *
 * by John Tse (1992)                                                   *
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
 * create_pair_wq  --  create a work queue for pairs of polynomial      *
 *                     pointers. A queue id is returned.                *
 *----------------------------------------------------------------------*/

int	create_pair_wq(int ncrankers);

/*----------------------------------------------------------------------*
 * pair_enq  --  put <addr1, pe1> and <addr2, pe2> as a pair of         *
 *               polynomial pointers with a priority 0 into the work    *
 *               queue specified by qid.                                *
 *----------------------------------------------------------------------*/

void	pair_enq(int prior, int addr1, int pe1, int addr2, int pe2);

/*----------------------------------------------------------------------*
 * pair_deq  --  get a pair of polynomial pointers from the work queue  *
 *               specified by Pair_qid.                                 *
 *               Return 1 if a pair is successfully dequeued;           *
 *               return 0 otherwise.                                    *
 *----------------------------------------------------------------------*/

int	pair_deq(int *addr1, int *pe1, int *addr2, int *pe2);

/*----------------------------------------------------------------------*
 * no_more_pair  --  return 1 if no more pair is found in local pair    *
 *                   work queue; return 0 otherwise.                    *
 *----------------------------------------------------------------------*/

int	no_more_pair(int qid);

/*----------------------------------------------------------------------*
 * test_if_terminated  --  return 1 if pair work queue has terminated;  *
 *                         return 0 otherwise;                          *
 *                         return -1 on error                           *
 *                         MUST BE CALLED BY PAIR QUEUE CREATOR ONLY.   *
 *----------------------------------------------------------------------*/

int	test_if_terminated(int qid);

/*----------------------------------------------------------------------*
 * restart_pair_wq  --  restart the pair work queue AFTER it has been   *
 *                      terminated.                                     *
 *                      MUST BE CALLED BY PAIR QUEUE CREATOR ONLY.      *
 *----------------------------------------------------------------------*/

void	restart_pair_wq(int qid);


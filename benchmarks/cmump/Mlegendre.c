/*
 * HISTORY
 * 07-Dec-87  Bennet Yee (bsy) at Carnegie-Mellon University
 *	Created.  Original code, not based on mp(3X).
 */
#include <cmump.h>

#define	meqone(p)	((p)->len == 1 && (p)->val[0] == 1)

FN mlegendre(pi,qi)	/* precondition:  both p and q are positive */
MINT *pi,*qi;
{
	MINT		p, q,
#ifdef	Q_MINUS_1
			one, q_minus_1,
#endif
			scratch, *mtmp;
	register MINT	*pptr, *qptr;
	register int	retval = 1, lowzeros;

	pptr = &p;
	MCOPY(pi,pptr);
	qptr = &q;
	MCOPY(qi,qptr);

#ifdef	Q_MINUS_1
	MINIT(&q_minus_1);
	MSET(1,&one);
#endif
	MINIT(&scratch);

tail_recurse2:
#ifdef	DEBUG
	printf("tail_recurse2: p="); mout(pptr); printf("\nq="); mout(qptr);
	putchar('\n');
#endif
	lowzeros = mlowzeros(qptr);
	if (lowzeros) mshiftr(qptr,lowzeros);	/* J(a,2) = 1 */
	if (meqone(qptr)) return retval;	/* J(a,1) = 1 */
	mmod(pptr,qptr,pptr);	/* J(a,q) = J(b,q) when a == b mod q */
#ifdef	Q_MINUS_1
	msub(qptr,&one,&q_minus_1);
#endif
tail_recurse:	/* we use tail_recurse only if q has not changed */
#ifdef	DEBUG
	printf("tail_recurse1: p="); mout(pptr); printf("\nq="); mout(qptr);
	putchar('\n');
#endif
	/*
	 * J(0,q) = 0
	 * this occurs only if gcd(p,q) != 1 which is never true here
	if (mtest(pptr) == 0) return 0;
	 */
	if (meqone(pptr)) {
		/* J(1,q) = 1 */
		/* retval *= 1; */
		goto done;
	}
#ifdef	Q_MINUS_1
	if (!mcmp(pptr,&q_minus_1)) {
		/* J(-1,q) = (-1)^((q-1)/2) */
		if (!modd(qptr))	retval = -retval;
		/* else		retval *= 1; */
		goto done;
	}
#endif
	/*
	 * we don.t handle J(ab,q) except for a==2
	 * since we don.t want to factor
	 */
	if (lowzeros = mlowzeros(pptr)) {
		/*
		 * J(2,q) = (-1)^((q^2-1)/8)
		 *
		 * Note that q odd guarantees that q^2-1 is divisible by 8
		 * q=2a+1, q^2 = 4a^2+4a+1, (q^2-1)/8 = a(a+1)/2, qed
		 *
		 * Now, note that this means that the low two bits of _a_
		 * (or the low bits of q shifted over by 1 determines
		 * the factor).
		 */
		mshiftr(pptr,lowzeros);
		if (lowzeros & 1) {
#ifndef	FAST_MACHINE_MULT
			/*
			 * low bits of _a_
			 */
			lowzeros = mlowbits(qptr);
			if (lowzeros & 2) {
				/* a odd, so a+1 even */
				if (!(lowzeros & 4))
					/* (a+1)/2 is odd */
					retval = -retval;
			} else {
				/* a even, a+1 odd */
				if (lowzeros & 4)
					/* a/2 odd */
					retval = -retval;
			}
#else	/* some CISC machines */
			lowzeros = mlowbits(qptr) >> 1;
			lowzeros = lowzeros * (lowzeros + 1);
			if (lowzeros & 2)
				retval = -retval;
#endif
		}
		/* else {
			retval *= 1;
		} */
		goto tail_recurse;
	}
	/*
	 * we know p is odd since we.ve cast out 2.s
	 * precondition that q is odd guarantees both odd.
	 *
	 * quadratic reciprocity
	 * J(p,q) = (-1)^((p-1)(q-1)/4) * J(q,p)
	 */
#ifdef	OLD_EQUIV_CODE
	if ((mlowzeros2(pptr) + mlowzeros2(qptr)) <= 2) {
		/* (p-1)(q-1)/4 is odd */
		retval = -retval;
	}
#else
	if ((lowzeros = mlowzeros2(pptr)) <= 2 && (lowzeros + mlowzeros2(qptr)) <= 2) {
		/* we may eliminate an extra call to mlowzeros2 here */
		retval = -retval;
	}
#endif
	mtmp = pptr; pptr = qptr; qptr = mtmp;
	goto tail_recurse2;
done:
	MFREE(&p); MFREE(&q);
	MFREE(&scratch);
#ifdef	Q_MINUS_1
	MFREE(&q_minus_1);
	MFREE(&one);
#endif
	return retval;
}

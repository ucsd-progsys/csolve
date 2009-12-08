#ifndef	__TYPESIZES_H__
#define	__TYPESIZES_H__

/* See <bits/types.h> for the meaning of these macros.  This file exists so
   that <bits/types.h> need not vary across different GNU platforms.  */

#define __DEV_T_TYPE		__UQUAD_TYPE
#define __UID_T_TYPE		__U32_TYPE
#define __GID_T_TYPE		__U32_TYPE
#define __INO_T_TYPE		__ULONGWORD_TYPE
#define __INO64_T_TYPE		__UQUAD_TYPE
#define __MODE_T_TYPE		__U32_TYPE
#define __NLINK_T_TYPE		__UWORD_TYPE
#define __OFF_T_TYPE		__SLONGWORD_TYPE
#define __OFF64_T_TYPE		__SQUAD_TYPE
#define __PID_T_TYPE		__S32_TYPE
#define __RLIM_T_TYPE		__ULONGWORD_TYPE
#define __RLIM64_T_TYPE		__UQUAD_TYPE
#define	__BLKCNT_T_TYPE		__SLONGWORD_TYPE
#define	__BLKCNT64_T_TYPE	__SQUAD_TYPE
#define	__FSBLKCNT_T_TYPE	__ULONGWORD_TYPE
#define	__FSBLKCNT64_T_TYPE	__UQUAD_TYPE
#define	__FSFILCNT_T_TYPE	__ULONGWORD_TYPE
#define	__FSFILCNT64_T_TYPE	__UQUAD_TYPE
#define	__ID_T_TYPE		__U32_TYPE
#define __CLOCK_T_TYPE		__SLONGWORD_TYPE
#define __TIME_T_TYPE		__SLONGWORD_TYPE
#define __USECONDS_T_TYPE	__U32_TYPE
#define __SUSECONDS_T_TYPE	__SLONGWORD_TYPE
#define __DADDR_T_TYPE		__S32_TYPE
#define __SWBLK_T_TYPE		__SLONGWORD_TYPE
#define __KEY_T_TYPE		__S32_TYPE
#define __CLOCKID_T_TYPE	__S32_TYPE
#define __TIMER_T_TYPE		void *
#define __BLKSIZE_T_TYPE	__SLONGWORD_TYPE
#define __FSID_T_TYPE		struct { int __val[2]; }
#define __SSIZE_T_TYPE		__SWORD_TYPE

/* Number of descriptors that can fit in an `fd_set'.  */
#define	__FD_SETSIZE		1024

#endif

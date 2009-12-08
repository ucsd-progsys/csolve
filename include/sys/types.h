#ifndef __TYPES_H__
#define __TYPES_H__

#define __WORDSIZE 32

/* Convenience types.  */
typedef unsigned char __u_char;
typedef unsigned short int __u_short;
typedef unsigned int __u_int;
typedef unsigned long int __u_long;

/* Fixed-size types, underlying types depend on word size and compiler.  */
typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;
#if __WORDSIZE == 64
typedef signed long int __int64_t;
typedef unsigned long int __uint64_t;
#elif defined __GLIBC_HAVE_LONG_LONG
__extension__ typedef signed long long int __int64_t;
__extension__ typedef unsigned long long int __uint64_t;
#endif

/* quad_t is also 64 bits.  */
#if __WORDSIZE == 64
typedef long int __quad_t;
typedef unsigned long int __u_quad_t;
#elif defined __GLIBC_HAVE_LONG_LONG
__extension__ typedef long long int __quad_t;
__extension__ typedef unsigned long long int __u_quad_t;
#else
typedef struct
{
  long __val[2];
} __quad_t;
typedef struct
{
  __u_long __val[2];
} __u_quad_t;
#endif


/* The machine-dependent file <bits/typesizes.h> defines __*_T_TYPE
   macros for each of the OS types we define below.  The definitions
   of those macros must use the following macros for underlying types.
   We define __S<SIZE>_TYPE and __U<SIZE>_TYPE for the signed and unsigned
   variants of each of the following integer types on this machine.

	16		-- "natural" 16-bit type (always short)
	32		-- "natural" 32-bit type (always int)
	64		-- "natural" 64-bit type (long or long long)
	LONG32		-- 32-bit type, traditionally long
	QUAD		-- 64-bit type, always long long
	WORD		-- natural type of __WORDSIZE bits (int or long)
	LONGWORD	-- type of __WORDSIZE bits, traditionally long

   We distinguish WORD/LONGWORD, 32/LONG32, and 64/QUAD so that the
   conventional uses of `long' or `long long' type modifiers match the
   types we define, even when a less-adorned type would be the same size.
   This matters for (somewhat) portably writing printf/scanf formats for
   these types, where using the appropriate l or ll format modifiers can
   make the typedefs and the formats match up across all GNU platforms.  If
   we used `long' when it's 64 bits where `long long' is expected, then the
   compiler would warn about the formats not matching the argument types,
   and the programmer changing them to shut up the compiler would break the
   program's portability.

   Here we assume what is presently the case in all the GCC configurations
   we support: long long is always 64 bits, long is always word/address size,
   and int is always 32 bits.  */

#define	__S16_TYPE		short int
#define __U16_TYPE		unsigned short int
#define	__S32_TYPE		int
#define __U32_TYPE		unsigned int
#define __SLONGWORD_TYPE	long int
#define __ULONGWORD_TYPE	unsigned long int
#if __WORDSIZE == 32
# define __SQUAD_TYPE		__quad_t
# define __UQUAD_TYPE		__u_quad_t
# define __SWORD_TYPE		int
# define __UWORD_TYPE		unsigned int
# define __SLONG32_TYPE		long int
# define __ULONG32_TYPE		unsigned long int
# define __S64_TYPE		__quad_t
# define __U64_TYPE		__u_quad_t
/* We want __extension__ before typedef's that use nonstandard base types
   such as `long long' in C89 mode.  */
# define __STD_TYPE		__extension__ typedef
#elif __WORDSIZE == 64
# define __SQUAD_TYPE		long int
# define __UQUAD_TYPE		unsigned long int
# define __SWORD_TYPE		long int
# define __UWORD_TYPE		unsigned long int
# define __SLONG32_TYPE		int
# define __ULONG32_TYPE		unsigned int
# define __S64_TYPE		long int
# define __U64_TYPE		unsigned long int
/* No need to mark the typedef with __extension__.   */
# define __STD_TYPE		typedef
#else
# error
#endif
#include <sys/typesizes.h>	/* Defines __*_T_TYPE macros.  */


__STD_TYPE __DEV_T_TYPE __dev_t;	/* Type of device numbers.  */
__STD_TYPE __UID_T_TYPE __uid_t;	/* Type of user identifications.  */
__STD_TYPE __GID_T_TYPE __gid_t;	/* Type of group identifications.  */
__STD_TYPE __INO_T_TYPE __ino_t;	/* Type of file serial numbers.  */
__STD_TYPE __INO64_T_TYPE __ino64_t;	/* Type of file serial numbers (LFS).*/
__STD_TYPE __MODE_T_TYPE __mode_t;	/* Type of file attribute bitmasks.  */
__STD_TYPE __NLINK_T_TYPE __nlink_t;	/* Type of file link counts.  */
__STD_TYPE __OFF_T_TYPE __off_t;	/* Type of file sizes and offsets.  */
__STD_TYPE __OFF64_T_TYPE __off64_t;	/* Type of file sizes and offsets (LFS).  */
__STD_TYPE __PID_T_TYPE __pid_t;	/* Type of process identifications.  */
__STD_TYPE __FSID_T_TYPE __fsid_t;	/* Type of file system IDs.  */
__STD_TYPE __CLOCK_T_TYPE __clock_t;	/* Type of CPU usage counts.  */
__STD_TYPE __RLIM_T_TYPE __rlim_t;	/* Type for resource measurement.  */
__STD_TYPE __RLIM64_T_TYPE __rlim64_t;	/* Type for resource measurement (LFS).  */
__STD_TYPE __ID_T_TYPE __id_t;		/* General type for IDs.  */
__STD_TYPE __TIME_T_TYPE __time_t;	/* Seconds since the Epoch.  */
__STD_TYPE __USECONDS_T_TYPE __useconds_t; /* Count of microseconds.  */
__STD_TYPE __SUSECONDS_T_TYPE __suseconds_t; /* Signed count of microseconds.  */

__STD_TYPE __DADDR_T_TYPE __daddr_t;	/* The type of a disk address.  */
__STD_TYPE __SWBLK_T_TYPE __swblk_t;	/* Type of a swap block maybe?  */
__STD_TYPE __KEY_T_TYPE __key_t;	/* Type of an IPC key.  */

/* Clock ID used in clock and timer functions.  */
__STD_TYPE __CLOCKID_T_TYPE __clockid_t;

/* Timer ID returned by `timer_create'.  */
__STD_TYPE __TIMER_T_TYPE __timer_t;

/* Type to represent block size.  */
__STD_TYPE __BLKSIZE_T_TYPE __blksize_t;

/* Types from the Large File Support interface.  */

/* Type to count number of disk blocks.  */
__STD_TYPE __BLKCNT_T_TYPE __blkcnt_t;
__STD_TYPE __BLKCNT64_T_TYPE __blkcnt64_t;

/* Type to count file system blocks.  */
__STD_TYPE __FSBLKCNT_T_TYPE __fsblkcnt_t;
__STD_TYPE __FSBLKCNT64_T_TYPE __fsblkcnt64_t;

/* Type to count file system nodes.  */
__STD_TYPE __FSFILCNT_T_TYPE __fsfilcnt_t;
__STD_TYPE __FSFILCNT64_T_TYPE __fsfilcnt64_t;

__STD_TYPE __SSIZE_T_TYPE __ssize_t; /* Type of a byte count, or error.  */

/* These few don't really vary by system, they always correspond
   to one of the other defined types.  */
typedef __off64_t __loff_t;	/* Type of file sizes and offsets (LFS).  */
typedef __quad_t *__qaddr_t;
typedef char *__caddr_t;

/* Duplicates info from stdint.h but this is used in unistd.h.  */
__STD_TYPE __SWORD_TYPE __intptr_t;

/* Duplicate info from sys/socket.h.  */
__STD_TYPE __U32_TYPE __socklen_t;

#ifdef	__USE_BSD
# ifndef __u_char_defined
typedef __u_char u_char;
typedef __u_short u_short;
typedef __u_int u_int;
typedef __u_long u_long;
typedef __quad_t quad_t;
typedef __u_quad_t u_quad_t;
typedef __fsid_t fsid_t;
#  define __u_char_defined
# endif
#endif

typedef __loff_t loff_t;

#ifndef __ino_t_defined
# ifndef __USE_FILE_OFFSET64
typedef __ino_t ino_t;
# else
typedef __ino64_t ino_t;
# endif
# define __ino_t_defined
#endif
#if defined __USE_LARGEFILE64 && !defined __ino64_t_defined
typedef __ino64_t ino64_t;
# define __ino64_t_defined
#endif

#ifndef __dev_t_defined
typedef __dev_t dev_t;
# define __dev_t_defined
#endif

#ifndef __gid_t_defined
typedef __gid_t gid_t;
# define __gid_t_defined
#endif

#ifndef __mode_t_defined
typedef __mode_t mode_t;
# define __mode_t_defined
#endif

#ifndef __nlink_t_defined
typedef __nlink_t nlink_t;
# define __nlink_t_defined
#endif

#ifndef __uid_t_defined
typedef __uid_t uid_t;
# define __uid_t_defined
#endif

#ifndef __off_t_defined
# ifndef __USE_FILE_OFFSET64
typedef __off_t off_t;
# else
typedef __off64_t off_t;
# endif
# define __off_t_defined
#endif
#if defined __USE_LARGEFILE64 && !defined __off64_t_defined
typedef __off64_t off64_t;
# define __off64_t_defined
#endif

#ifndef __pid_t_defined
typedef __pid_t pid_t;
# define __pid_t_defined
#endif

#if (defined __USE_SVID || defined __USE_XOPEN) && !defined __id_t_defined
typedef __id_t id_t;
# define __id_t_defined
#endif

#ifndef __ssize_t_defined
typedef __ssize_t ssize_t;
# define __ssize_t_defined
#endif

#ifdef	__USE_BSD
# ifndef __daddr_t_defined
typedef __daddr_t daddr_t;
typedef __caddr_t caddr_t;
#  define __daddr_t_defined
# endif
#endif

#if (defined __USE_SVID || defined __USE_XOPEN) && !defined __key_t_defined
typedef __key_t key_t;
# define __key_t_defined
#endif

#ifdef __USE_XOPEN
# define __need_clock_t
#endif
#define	__need_time_t
#define __need_timer_t
#define __need_clockid_t
#include <time.h>

#ifdef __USE_XOPEN
# ifndef __useconds_t_defined
typedef __useconds_t useconds_t;
#  define __useconds_t_defined
# endif
# ifndef __suseconds_t_defined
typedef __suseconds_t suseconds_t;
#  define __suseconds_t_defined
# endif
#endif

#define	__need_size_t
#include <stddef.h>

#ifdef __USE_MISC
/* Old compatibility names for C types.  */
typedef unsigned long int ulong;
typedef unsigned short int ushort;
typedef unsigned int uint;
#endif

/* These size-specific names are used by some of the inet code.  */

#if !__GNUC_PREREQ (2, 7)

/* These types are defined by the ISO C99 header <inttypes.h>. */
# ifndef __int8_t_defined
#  define __int8_t_defined
typedef	char int8_t;
typedef	short int int16_t;
typedef	int int32_t;
#  if __WORDSIZE == 64
typedef long int int64_t;
#  elif __GLIBC_HAVE_LONG_LONG
__extension__ typedef long long int int64_t;
#  endif
# endif

/* But these were defined by ISO C without the first `_'.  */
typedef	unsigned char u_int8_t;
typedef	unsigned short int u_int16_t;
typedef	unsigned int u_int32_t;
# if __WORDSIZE == 64
typedef unsigned long int u_int64_t;
# elif __GLIBC_HAVE_LONG_LONG
__extension__ typedef unsigned long long int u_int64_t;
# endif

typedef int register_t;

#else

/* For GCC 2.7 and later, we can use specific type-size attributes.  */
# define __intN_t(N, MODE) \
  typedef int int##N##_t __attribute__ ((__mode__ (MODE)))
# define __u_intN_t(N, MODE) \
  typedef unsigned int u_int##N##_t __attribute__ ((__mode__ (MODE)))

# ifndef __int8_t_defined
#  define __int8_t_defined
__intN_t (8, __QI__);
__intN_t (16, __HI__);
__intN_t (32, __SI__);
__intN_t (64, __DI__);
# endif

__u_intN_t (8, __QI__);
__u_intN_t (16, __HI__);
__u_intN_t (32, __SI__);
__u_intN_t (64, __DI__);

typedef int register_t __attribute__ ((__mode__ (__word__)));


/* Some code from BIND tests this macro to see if the types above are
   defined.  */
#endif
#define __BIT_TYPES_DEFINED__	1

typedef __blksize_t blksize_t;

/* Types from the Large File Support interface.  */
#ifndef __USE_FILE_OFFSET64
# ifndef __blkcnt_t_defined
typedef __blkcnt_t blkcnt_t;	 /* Type to count number of disk blocks.  */
#  define __blkcnt_t_defined
# endif
# ifndef __fsblkcnt_t_defined
typedef __fsblkcnt_t fsblkcnt_t; /* Type to count file system blocks.  */
#  define __fsblkcnt_t_defined
# endif
# ifndef __fsfilcnt_t_defined
typedef __fsfilcnt_t fsfilcnt_t; /* Type to count file system inodes.  */
#  define __fsfilcnt_t_defined
# endif
#else
# ifndef __blkcnt_t_defined
typedef __blkcnt64_t blkcnt_t;	   /* Type to count number of disk blocks.  */
#  define __blkcnt_t_defined
# endif
# ifndef __fsblkcnt_t_defined
typedef __fsblkcnt64_t fsblkcnt_t; /* Type to count file system blocks.  */
#  define __fsblkcnt_t_defined
# endif
# ifndef __fsfilcnt_t_defined
typedef __fsfilcnt64_t fsfilcnt_t; /* Type to count file system inodes.  */
#  define __fsfilcnt_t_defined
# endif
#endif

#ifdef __USE_LARGEFILE64
typedef __blkcnt64_t blkcnt64_t;     /* Type to count number of disk blocks. */
typedef __fsblkcnt64_t fsblkcnt64_t; /* Type to count file system blocks.  */
typedef __fsfilcnt64_t fsfilcnt64_t; /* Type to count file system inodes.  */
#endif

#endif __TYPE_H__

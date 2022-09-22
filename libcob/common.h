/*
   Copyright (C) 2002-2012, 2014-2022 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Simon Sobisch, Ron Norman,
   Edward Hart

   This file is part of GnuCOBOL.

   The GnuCOBOL runtime library is free software: you can redistribute it
   and/or modify it under the terms of the GNU Lesser General Public License
   as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   GnuCOBOL is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with GnuCOBOL.  If not, see <https://www.gnu.org/licenses/>.
*/

#ifndef COB_COMMON_H
#define COB_COMMON_H

/* Only define cob_decimal if we have the necessary mpz_t from gmp.h/mpir.h
   (or can self-define it from mp.h) */
#if !defined (__GMP_H__)
#ifndef __GNU_MP__
#define COB_WITHOUT_DECIMAL
#else
typedef __mpz_struct mpz_t[1];
#endif
#endif


/* General type defines */
#define	cob_c8_t		char
#define	cob_s8_t		signed char
#define	cob_u8_t		unsigned char
#define	cob_s16_t		short
#define	cob_u16_t		unsigned short
#define	cob_s32_t		int
#define	cob_u32_t		unsigned int
#define	cob_sli_t		long int
#define	cob_uli_t		unsigned long int

#if	defined(_WIN32) && !defined(__MINGW32__)

#define	cob_s64_t		__int64
#define	cob_u64_t		unsigned __int64

#define	COB_S64_C(x)		x ## I64
#define	COB_U64_C(x)		x ## UI64

#else

#define	cob_s64_t		long long
#define	cob_u64_t		unsigned long long

#define	COB_S64_C(x)		x ## LL
#define	COB_U64_C(x)		x ## ULL

#endif

#if	defined(_WIN32)

#define	CB_FMT_LLD		"%I64d"
#define	CB_FMT_LLU		"%I64u"
#define	CB_FMT_LLX		"%I64x"
#define	CB_FMT_PLLD		"%+*.*I64d"
#define	CB_FMT_PLLU		"%*.*I64u"

#if defined (__MINGW32__)
#define	CB_FMT_LLD_F		"%I64dLL"
#define	CB_FMT_LLU_F		"%I64uULL"
#else
#define	CB_FMT_LLD_F		"%I64dI64"
#define	CB_FMT_LLU_F		"%I64uUI64"
#endif

#else

#define	CB_FMT_LLD		"%lld"
#define	CB_FMT_LLU		"%llu"
#define	CB_FMT_LLX		"%llx"
#define	CB_FMT_PLLD		"%+*.*lld"
#define	CB_FMT_PLLU		"%*.*llu"
#define	CB_FMT_LLD_F		"%lldLL"
#define	CB_FMT_LLU_F		"%lluULL"

#endif

#define	cob_c8_ptr		cob_c8_t *
#define	cob_u8_ptr		cob_u8_t *
#define	cob_s8_ptr		cob_s8_t *
#define	cob_u16_ptr		cob_u16_t *
#define	cob_s16_ptr		cob_s16_t *
#define	cob_u32_ptr		cob_u32_t *
#define	cob_s32_ptr		cob_s32_t *
#define	cob_u64_ptr		cob_u64_t *
#define	cob_s64_ptr		cob_s64_t *

#define	cob_void_ptr		void *
#define	cob_field_ptr		cob_field *
#define	cob_file_ptr		cob_file *
#define	cob_module_ptr		cob_module *
#define	cob_screen_ptr		cob_screen *
#define	cob_file_key_ptr	cob_file_key *

/* Byte swap functions */

/*
   The original idea for the byteswap routines was taken from GLib.
   (Specifically glib/gtypes.h)
   GLib is licensed under the GNU Lesser General Public License.
*/

/* Generic swapping functions */

#undef	COB_BSWAP_16_CONSTANT
#undef	COB_BSWAP_32_CONSTANT
#undef	COB_BSWAP_64_CONSTANT
#undef	COB_BSWAP_16
#undef	COB_BSWAP_32
#undef	COB_BSWAP_64

#define COB_BSWAP_16_CONSTANT(val)	((cob_u16_t) (		\
    (((cob_u16_t)(val) & (cob_u16_t) 0x00FFU) << 8) |		\
    (((cob_u16_t)(val) & (cob_u16_t) 0xFF00U) >> 8)))

#define COB_BSWAP_32_CONSTANT(val)	((cob_u32_t) (		\
    (((cob_u32_t) (val) & (cob_u32_t) 0x000000FFU) << 24) |	\
    (((cob_u32_t) (val) & (cob_u32_t) 0x0000FF00U) <<  8) |	\
    (((cob_u32_t) (val) & (cob_u32_t) 0x00FF0000U) >>  8) |	\
    (((cob_u32_t) (val) & (cob_u32_t) 0xFF000000U) >> 24)))

#define COB_BSWAP_64_CONSTANT(val)	((cob_u64_t) (		\
    (((cob_u64_t) (val) &					\
      (cob_u64_t) COB_U64_C(0x00000000000000FF)) << 56) |	\
    (((cob_u64_t) (val) &					\
      (cob_u64_t) COB_U64_C(0x000000000000FF00)) << 40) |	\
    (((cob_u64_t) (val) &					\
      (cob_u64_t) COB_U64_C(0x0000000000FF0000)) << 24) |	\
    (((cob_u64_t) (val) &					\
      (cob_u64_t) COB_U64_C(0x00000000FF000000)) <<  8) |	\
    (((cob_u64_t) (val) &					\
      (cob_u64_t) COB_U64_C(0x000000FF00000000)) >>  8) |	\
    (((cob_u64_t) (val) &					\
      (cob_u64_t) COB_U64_C(0x0000FF0000000000)) >> 24) |	\
    (((cob_u64_t) (val) &					\
      (cob_u64_t) COB_U64_C(0x00FF000000000000)) >> 40) |	\
    (((cob_u64_t) (val) &					\
      (cob_u64_t) COB_U64_C(0xFF00000000000000)) >> 56)))

/* Machine/OS specific overrides */

#ifdef	__GNUC__

#if	__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 3)

#define COB_BSWAP_16(val) (COB_BSWAP_16_CONSTANT (val))
#define COB_BSWAP_32(val) (__builtin_bswap32 (val))
#define COB_BSWAP_64(val) (__builtin_bswap64 (val))

#elif	defined(__i386__)

#define COB_BSWAP_16(val) (COB_BSWAP_16_CONSTANT (val))
#define COB_BSWAP_32(val)					\
       (__extension__						\
	({ register cob_u32_t __v,				\
	     __x = ((cob_u32_t) (val));				\
	   if (__builtin_constant_p (__x))			\
	     __v = COB_BSWAP_32_CONSTANT (__x);			\
	   else							\
	     __asm__ ("bswap %0"				\
		      : "=r" (__v)				\
		      : "0" (__x));				\
	    __v; }))
#define COB_BSWAP_64(val)					\
       (__extension__						\
	({ union { cob_u64_t __ll;				\
		   cob_u32_t __l[2]; } __w, __r;		\
	   __w.__ll = ((cob_u64_t) (val));			\
	   if (__builtin_constant_p (__w.__ll))			\
	     __r.__ll = COB_BSWAP_64_CONSTANT (__w.__ll);	\
	   else							\
	     {							\
	       __r.__l[0] = COB_BSWAP_32 (__w.__l[1]);		\
	       __r.__l[1] = COB_BSWAP_32 (__w.__l[0]);		\
	     }							\
	   __r.__ll; }))

#elif defined (__ia64__)

#define COB_BSWAP_16(val) (COB_BSWAP_16_CONSTANT (val))
#define COB_BSWAP_32(val)					\
       (__extension__						\
	 ({ register cob_u32_t __v,				\
	      __x = ((cob_u32_t) (val));			\
	    if (__builtin_constant_p (__x))			\
	      __v = COB_BSWAP_32_CONSTANT (__x);		\
	    else						\
	     __asm__ __volatile__ ("shl %0 = %1, 32 ;;"		\
				   "mux1 %0 = %0, @rev ;;"	\
				    : "=r" (__v)		\
				    : "r" (__x));		\
	    __v; }))
#define COB_BSWAP_64(val)					\
       (__extension__						\
	({ register cob_u64_t __v,				\
	     __x = ((cob_u64_t) (val));				\
	   if (__builtin_constant_p (__x))			\
	     __v = COB_BSWAP_64_CONSTANT (__x);			\
	   else							\
	     __asm__ __volatile__ ("mux1 %0 = %1, @rev ;;"	\
				   : "=r" (__v)			\
				   : "r" (__x));		\
	   __v; }))

#elif defined (__x86_64__)

#define COB_BSWAP_16(val) (COB_BSWAP_16_CONSTANT (val))
#define COB_BSWAP_32(val)					\
      (__extension__						\
	({ register cob_u32_t __v,				\
	     __x = ((cob_u32_t) (val));				\
	   if (__builtin_constant_p (__x))			\
	     __v = COB_BSWAP_32_CONSTANT (__x);			\
	   else							\
	    __asm__ ("bswapl %0"				\
		     : "=r" (__v)				\
		     : "0" (__x));				\
	   __v; }))
#define COB_BSWAP_64(val)					\
       (__extension__						\
	({ register cob_u64_t __v,				\
	     __x = ((cob_u64_t) (val));				\
	   if (__builtin_constant_p (__x))			\
	     __v = COB_BSWAP_64_CONSTANT (__x);			\
	   else							\
	     __asm__ ("bswapq %0"				\
		      : "=r" (__v)				\
		      : "0" (__x));				\
	   __v; }))

#else /* Generic gcc */

#define COB_BSWAP_16(val) (COB_BSWAP_16_CONSTANT (val))
#define COB_BSWAP_32(val) (COB_BSWAP_32_CONSTANT (val))
#define COB_BSWAP_64(val) (COB_BSWAP_64_CONSTANT (val))

#endif

#elif defined(_MSC_VER)

#define COB_BSWAP_16(val) (_byteswap_ushort (val))
#define COB_BSWAP_32(val) (_byteswap_ulong (val))
#define COB_BSWAP_64(val) (_byteswap_uint64 (val))

#elif defined(__ORANGEC__)

#define COB_BSWAP_16(val) (COB_BSWAP_16_CONSTANT (val))
#define COB_BSWAP_32(val) (__builtin_bswap32 (val))
#define COB_BSWAP_64(val) (__builtin_bswap64 (val))

#else /* Generic */

#define COB_BSWAP_16(val) (COB_BSWAP_16_CONSTANT (val))
#define COB_BSWAP_32(val) (COB_BSWAP_32_CONSTANT (val))
#define COB_BSWAP_64(val) (COB_BSWAP_64_CONSTANT (val))

#endif

/* End byte swap functions */

/* Compiler characteristics */

#ifdef	_MSC_VER

#ifndef	_CRT_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_DEPRECATE	1
#endif

/* Disable certain warnings */
/* Deprecated functions */
#pragma warning(disable: 4996)
/* Function declarations without parameter list */
#pragma warning(disable: 4255)

#define strncasecmp		_strnicmp
#define strcasecmp		_stricmp
#define snprintf		_snprintf
#define getpid			_getpid
#define access			_access
#define popen			_popen
#define pclose			_pclose
/* MSDN says these are available since VC2005 #if COB_USE_VC2013_OR_GREATER
only usable with COB_USE_VC2013_OR_GREATER */
#define timezone		_timezone
#define tzname			_tzname
#define daylight		_daylight
/* only usable with COB_USE_VC2013_OR_GREATER - End
#endif */

#if !COB_USE_VC2013_OR_GREATER
#define atoll			_atoi64
#endif

#define __attribute__(x)

#ifndef	_M_IA64
#ifdef	_WIN64
#define	__x86_64__
#else
#define	__i386__
#endif
#else
#define __ia64__
#endif

#endif /* _MSC_VER */

#ifdef	__MINGW32__	/* needed by older versions */
#define strncasecmp		_strnicmp
#define strcasecmp		_stricmp
#endif /* __MINGW32__ */

#ifdef __BORLANDC__
#define strncasecmp	strnicmp
#define strcasecmp	stricmp
#define _setmode	setmode
#define _chdir		chdir
#define timezone	_timezone
#define tzname		_tzname
#define daylight	_daylight
#endif /* __BORLANDC__ */

#ifdef __ORANGEC__
#define timezone		_timezone
#define tzname			_tzname
#define daylight		_daylight
#endif /* _ORANGEC__ */

#if	__SUNPRO_C
/* Disable certain warnings */
#pragma error_messages (off, E_STATEMENT_NOT_REACHED)
#endif

#ifndef COB_WITHOUT_JMP
#include <setjmp.h>
#endif

#ifndef	COB_EXT_EXPORT
#if	((defined(_WIN32) || defined(__CYGWIN__)) && !defined(__clang__))
#define COB_EXT_EXPORT	__declspec(dllexport) extern
#else
#define COB_EXT_EXPORT	extern
#endif
#endif
#ifndef COB_EXT_IMPORT
#if	((defined(_WIN32) || defined(__CYGWIN__)) && !defined(__clang__))
#define COB_EXT_IMPORT	__declspec(dllimport) extern
#else
#define COB_EXT_IMPORT	extern
#endif
#endif

#ifndef COB_EXPIMP
#ifdef	COB_LIB_EXPIMP
	#define COB_EXPIMP	COB_EXT_EXPORT
#else
	#define COB_EXPIMP	COB_EXT_IMPORT
#endif
#endif

#if	defined(COB_KEYWORD_INLINE)
	#define COB_INLINE	COB_KEYWORD_INLINE
#else
	#define COB_INLINE
#endif

/* Also OK for icc which defines __GNUC__ */

#if	 defined(__GNUC__) || \
	(defined(__xlc__) && __IBMC__ >= 700  ) || \
	(defined(__HP_cc) && __HP_cc  >= 61000)
#define	COB_A_NORETURN	__attribute__((noreturn))
#define	COB_A_FORMAT12	__attribute__((format(printf, 1, 2)))
#define	COB_A_FORMAT23	__attribute__((format(printf, 2, 3)))
#define	COB_A_FORMAT34	__attribute__((format(printf, 3, 4)))
#define	COB_A_FORMAT45	__attribute__((format(printf, 4, 5)))
#define	DECLNORET
#else
#define	COB_A_NORETURN
#define	COB_A_FORMAT12
#define	COB_A_FORMAT23
#define	COB_A_FORMAT34
#define	COB_A_FORMAT45

#if defined	(_MSC_VER) || defined (__ORANGEC__) || \
   (defined (__BORLANDC__) && defined (_WIN32))
#define	DECLNORET	__declspec(noreturn)
#else
#define	DECLNORET
#endif
#endif

#if	defined(__GNUC__)
#define	optim_memcpy(x,y,z)	__builtin_memcpy (x, y, z)
#else
#define	optim_memcpy(x,y,z)	memcpy (x, y, z)
#endif

#if	defined(__GNUC__) && (__GNUC__ >= 3)
#define likely(x)	__builtin_expect((long int)!!(x), 1L)
#define unlikely(x)	__builtin_expect((long int)!!(x), 0L)
#define	COB_A_MALLOC	__attribute__((malloc))
#define	COB_HAVE_STEXPR	1

#if	__GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 1)
#define	COB_NOINLINE	__attribute__((noinline))
#define	COB_A_INLINE	__attribute__((always_inline))
#else
#define	COB_NOINLINE
#define	COB_A_INLINE
#endif

#if	__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 3)
#define	COB_A_COLD	__attribute__((cold))
#else
#define	COB_A_COLD
#endif

#elif	defined(__xlc__) && __IBMC__ >= 700

#if	__IBMC__ >= 900
#define likely(x)	__builtin_expect((long int)!!(x), 1L)
#define unlikely(x)	__builtin_expect((long int)!!(x), 0L)
#else
#define likely(x)	(x)
#define unlikely(x)	(x)
#endif
#define	COB_NOINLINE	__attribute__((noinline))
#define	COB_A_INLINE	__attribute__((always_inline))
#define	COB_A_MALLOC
#define	COB_A_COLD
#if	__IBMC__ >= 800
#define	COB_HAVE_STEXPR	1
#else
#undef	COB_HAVE_STEXPR
#endif

#elif	defined(__SUNPRO_C) && __SUNPRO_C >= 0x590

#define likely(x)	(x)
#define unlikely(x)	(x)
#define	COB_A_MALLOC	__attribute__((malloc))
#define	COB_NOINLINE	__attribute__((noinline))
#define	COB_A_INLINE	__attribute__((always_inline))
#define	COB_A_COLD
#define	COB_HAVE_STEXPR	1

#elif	defined(_MSC_VER)

#define likely(x)	(x)
#define unlikely(x)	(x)
#define	COB_A_MALLOC
#define	COB_NOINLINE	__declspec(noinline)
#define	COB_A_INLINE	__forceinline
#define	COB_A_COLD
/* #undef	COB_HAVE_STEXPR */

#else

#define likely(x)	(x)
#define unlikely(x)	(x)
#define	COB_A_MALLOC
#define	COB_NOINLINE
#define	COB_A_INLINE
#define	COB_A_COLD
#undef	COB_HAVE_STEXPR

#endif

/* Posix macros, in case they are not defined */
#ifndef	S_ISDIR
#define S_ISDIR(x)		(((x) & _S_IFMT) == _S_IFDIR)
#endif

#ifndef	S_ISREG
#define S_ISREG(x)		(((x) & _S_IFMT) == _S_IFREG)
#endif

/* Prevent unwanted verbosity when using icc */
#ifdef	__INTEL_COMPILER

/* Unreachable code */
#pragma warning ( disable : 111 )
/* Declared but never referenced */
#pragma warning ( disable : 177 )
/* Format conversion */
#pragma warning ( disable : 181 )
/* Enumerated type mixed with other type */
#pragma warning ( disable : 188 )
/* #undefine tested for zero */
#pragma warning ( disable : 193 )
/* Set but not used */
#pragma warning ( disable : 593 )
/* Parameter not referenced */
#pragma warning ( disable : 869 )
/* Operands are evaluated in unspecified order */
#pragma warning ( disable : 981 )
/* Missing return at end of non-void function */
/* Note - occurs because we have a non-returning abort call in cobc */
#pragma warning ( disable : 1011 )
/* Declaration in same source as definition */
#pragma warning ( disable : 1419 )
/* Shadowed variable - 1599 and 1944 are essentially the same */
#pragma warning ( disable : 1599 )
#pragma warning ( disable : 1944 )
/* Possible loss of precision */
#pragma warning ( disable : 2259 )

#endif

#if defined(_MSC_VER) || defined(__ORANGEC__) || defined(__WATCOMC__) || \
    defined(__BORLANDC__) || defined(__MINGW32__) || defined (__DJGPP__)
#define PATHSEP_CHAR (char) ';'
#define PATHSEP_STR (char *) ";"
#else
#define PATHSEP_CHAR (char) ':'
#define PATHSEP_STR (char *) ":"
#endif
#if !defined(_WIN32) && !defined(__DJGPP__) /* note: needs to be \ for MinGW, needed for cobc -j */
#define SLASH_CHAR	(char) '/'
#define SLASH_STR	(char *) "/"
#else
#define SLASH_CHAR	(char) '\\'
#define SLASH_STR	(char *) "\\"
#endif

#ifdef __DJGPP__
#define HAVE_8DOT3_FILENAMES
#endif

/* End compiler stuff */


/* EBCDIC determination */

#if ' ' == 0x40
#define	COB_EBCDIC_MACHINE
#else
#undef	COB_EBCDIC_MACHINE
#endif

/* Macro to prevent compiler warning "conditional expression is constant" */
#if defined (_MSC_VER) && COB_USE_VC2008_OR_GREATER
#define ONCE_COB \
	__pragma( warning(push) )		\
	__pragma( warning(disable:4127) )	\
	while (0) \
	__pragma( warning(pop) )
#else
#define ONCE_COB while (0)
#endif

/* Macro to prevent unused parameter warning */

#define	COB_UNUSED(z)	do { (void)(z); } ONCE_COB

/* Buffer size definitions */

#define	COB_MINI_BUFF		256
#define	COB_SMALL_BUFF		1024
#define	COB_NORMAL_BUFF		2048
#define	COB_FILE_BUFF		4096
#define	COB_MEDIUM_BUFF		8192
#define	COB_LARGE_BUFF		16384
#define	COB_MINI_MAX		(COB_MINI_BUFF - 1)
#define	COB_SMALL_MAX		(COB_SMALL_BUFF - 1)
#define	COB_NORMAL_MAX		(COB_NORMAL_BUFF - 1)
#define	COB_FILE_MAX		(COB_FILE_BUFF - 1)
#define	COB_MEDIUM_MAX		(COB_MEDIUM_BUFF - 1)
#define	COB_LARGE_MAX		(COB_LARGE_BUFF - 1)

/* Perform stack size */
#define	COB_STACK_SIZE		255

/* Maximum size of file records */
/* TODO: add compiler configuration for limiting this */
#define	MAX_FD_RECORD		64 * 1024 * 1024

/* Maximum size of file records (IDX) */
/* TODO: define depending on used ISAM */
/* TODO: add compiler configuration for limiting this */
#define	MAX_FD_RECORD_IDX	65535

/* Maximum amount of keys per file */
/* TODO: define depending on used ISAM */
/* TODO: add compiler configuration for limiting this */
#define	MAX_FILE_KEYS	255

/* Maximum number of field digits */
#define	COB_MAX_DIGITS		38

/* Maximum digits in binary field */
#define	COB_MAX_BINARY		39

/* Maximum exponent digits (both in literals and floating-point numeric-edited item */
#define COB_FLOAT_DIGITS_MAX         36

/* note: more (internal-only) limits in coblocal.h */

/* Maximum number of cob_decimal structures */
#define	COB_MAX_DEC_STRUCT	32

/* Maximum length of COBOL words */
#define	COB_MAX_WORDLEN		63

/* Maximum length of literals */
#define	COB_MAX_LITERAL_LEN		256 * 1024

/* Maximum length of COBOL program names */
#define	COB_MAX_NAMELEN		31

/* Maximum number of subscripts */
#define COB_MAX_SUBSCRIPTS	16

/* Memory size for sorting */
#define	COB_SORT_MEMORY		128 * 1024 * 1024
#define	COB_SORT_CHUNK		256 * 1024

/* Program return types */
#define	COB_RET_TYPE_INT	0
#define	COB_RET_TYPE_PTR	1
#define	COB_RET_TYPE_VOID	2

/* Fold case types */
#define	COB_FOLD_NONE		0
#define	COB_FOLD_UPPER		1
#define	COB_FOLD_LOWER		2

/* Locale types */
#define	COB_LC_COLLATE		0
#define	COB_LC_CTYPE		1
#define	COB_LC_MESSAGES		2
#define	COB_LC_MONETARY		3
#define	COB_LC_NUMERIC		4
#define	COB_LC_TIME		5
#define	COB_LC_ALL		6
#define	COB_LC_USER		7
#define	COB_LC_CLASS		8

/* Field types */

#define COB_TYPE_UNKNOWN		0x00
#define COB_TYPE_GROUP			0x01U
#define COB_TYPE_BOOLEAN		0x02U

#define COB_TYPE_NUMERIC		0x10U
#define COB_TYPE_NUMERIC_DISPLAY	0x10U
#define COB_TYPE_NUMERIC_BINARY		0x11U
#define COB_TYPE_NUMERIC_PACKED		0x12U
#define COB_TYPE_NUMERIC_FLOAT		0x13U
#define COB_TYPE_NUMERIC_DOUBLE		0x14U
#define COB_TYPE_NUMERIC_L_DOUBLE	0x15U
#define COB_TYPE_NUMERIC_FP_DEC64	0x16U
#define COB_TYPE_NUMERIC_FP_DEC128	0x17U
#define COB_TYPE_NUMERIC_FP_BIN32	0x18U
#define COB_TYPE_NUMERIC_FP_BIN64	0x19U
#define COB_TYPE_NUMERIC_FP_BIN128	0x1AU
#define COB_TYPE_NUMERIC_COMP5		0x1BU

#define COB_TYPE_NUMERIC_EDITED		0x24U

#define COB_TYPE_ALNUM			0x20U
#define COB_TYPE_ALPHANUMERIC		0x21U
#define COB_TYPE_ALPHANUMERIC_ALL	0x22U
#define COB_TYPE_ALPHANUMERIC_EDITED	0x23U

#define COB_TYPE_NATIONAL		0x40U
#define COB_TYPE_NATIONAL_EDITED	0x41U

/* Field flags */

#define COB_FLAG_HAVE_SIGN		(1U << 0)	/* 0x0001 */
#define COB_FLAG_SIGN_SEPARATE		(1U << 1)	/* 0x0002 */
#define COB_FLAG_SIGN_LEADING		(1U << 2)	/* 0x0004 */
#define COB_FLAG_BLANK_ZERO		(1U << 3)	/* 0x0008 */
#define COB_FLAG_JUSTIFIED		(1U << 4)	/* 0x0010 */
#define COB_FLAG_BINARY_SWAP		(1U << 5)	/* 0x0020 */
#define COB_FLAG_REAL_BINARY		(1U << 6)	/* 0x0040 */
#define COB_FLAG_IS_POINTER		(1U << 7)	/* 0x0080 */
#define COB_FLAG_NO_SIGN_NIBBLE		(1U << 8)	/* 0x0100 */
#define COB_FLAG_IS_FP			(1U << 9)	/* 0x0200 */
#define COB_FLAG_REAL_SIGN		(1U << 10)	/* 0x0400 */
#define COB_FLAG_BINARY_TRUNC		(1U << 11)	/* 0x0800 */
#define COB_FLAG_CONSTANT		(1U << 12)	/* 0x1000 */

#define COB_FIELD_HAVE_SIGN(f)		((f)->attr->flags & COB_FLAG_HAVE_SIGN)
#define COB_FIELD_SIGN_SEPARATE(f)	((f)->attr->flags & COB_FLAG_SIGN_SEPARATE)
#define COB_FIELD_SIGN_LEADING(f)	((f)->attr->flags & COB_FLAG_SIGN_LEADING)
#define COB_FIELD_BLANK_ZERO(f)		((f)->attr->flags & COB_FLAG_BLANK_ZERO)
#define COB_FIELD_JUSTIFIED(f)		((f)->attr->flags & COB_FLAG_JUSTIFIED)
#define COB_FIELD_BINARY_SWAP(f)	((f)->attr->flags & COB_FLAG_BINARY_SWAP)
#define COB_FIELD_REAL_BINARY(f)	((f)->attr->flags & COB_FLAG_REAL_BINARY)
#define COB_FIELD_IS_POINTER(f)		((f)->attr->flags & COB_FLAG_IS_POINTER)
#define COB_FIELD_NO_SIGN_NIBBLE(f)	((f)->attr->flags & COB_FLAG_NO_SIGN_NIBBLE)
#define COB_FIELD_IS_FP(f)		((f)->attr->flags & COB_FLAG_IS_FP)
#define COB_FIELD_REAL_SIGN(f)		((f)->attr->flags & COB_FLAG_REAL_SIGN)
#define COB_FIELD_BINARY_TRUNC(f)	((f)->attr->flags & COB_FLAG_BINARY_TRUNC)
#define COB_FIELD_CONSTANT(f)		((f)->attr->flags & COB_FLAG_CONSTANT)

#define	COB_FLAG_LEADSEP		\
	(COB_FLAG_SIGN_SEPARATE | COB_FLAG_SIGN_LEADING)

#define COB_FIELD_SIGN_LEADSEP(f)	\
	(((f)->attr->flags & COB_FLAG_LEADSEP) == COB_FLAG_LEADSEP)

#define COB_FIELD_TYPE(f)	((f)->attr->type)
#define COB_FIELD_DIGITS(f)	((f)->attr->digits)
#define COB_FIELD_SCALE(f)	((f)->attr->scale)
#define COB_FIELD_FLAGS(f)	((f)->attr->flags)
#define COB_FIELD_PIC(f)	((f)->attr->pic)

#define COB_FIELD_DATA(f)	\
	((f)->data + (COB_FIELD_SIGN_LEADSEP (f) ? 1 : 0))

#define COB_FIELD_SIZE(f)	\
	(COB_FIELD_SIGN_SEPARATE (f) ? f->size - 1 : f->size)

#define COB_FIELD_IS_NUMERIC(f)	(COB_FIELD_TYPE (f) & COB_TYPE_NUMERIC)
#define COB_FIELD_IS_NUMDISP(f)	(COB_FIELD_TYPE (f) == COB_TYPE_NUMERIC_DISPLAY)
#define COB_FIELD_IS_ALNUM(f)	(COB_FIELD_TYPE (f) == COB_TYPE_ALPHANUMERIC)
#define COB_FIELD_IS_ANY_ALNUM(f)	(COB_FIELD_TYPE (f) & COB_TYPE_ALNUM)
#define COB_FIELD_IS_NATIONAL(f)	(COB_FIELD_TYPE (f) & COB_TYPE_NATIONAL)


#define	COB_DISPLAY_SIGN_ASCII	0
#define	COB_DISPLAY_SIGN_EBCDIC	1

#define	COB_NATIONAL_SIZE	2

#define	COB_SET_FLD(v,x,y,z)	(v.size = x, v.data = y, v.attr = z, &v)
#define	COB_SET_DATA(x,z)	(x.data = z, &x)

/* Fatal error definitions */

enum cob_fatal_error {
	COB_FERROR_NONE = 0,
	COB_FERROR_CANCEL,
	COB_FERROR_INITIALIZED,
	COB_FERROR_CODEGEN,
	COB_FERROR_CHAINING,
	COB_FERROR_STACK,
	COB_FERROR_GLOBAL,
	COB_FERROR_MEMORY,
	COB_FERROR_MODULE,
	COB_FERROR_RECURSIVE,
	COB_FERROR_SCR_INP,
	COB_FERROR_FILE,
	COB_FERROR_FUNCTION,
	COB_FERROR_FREE,
	COB_FERROR_XML,
	COB_FERROR_JSON
};

/* Exception identifier enumeration */

#undef	COB_EXCEPTION
#ifndef COB_WITHOUT_EXCEPTIONS
#define	COB_EXCEPTION(code,tag,name,critical)	tag,

enum cob_exception_id {
	COB_EC_ZERO = 0,
#include "exception.def"	/* located and installed next to common.h */
	COB_EC_MAX
};

#undef	COB_EXCEPTION
#endif

/* File attributes */

/* Start conditions */
/* Note that COB_NE is disallowed */
#define COB_EQ			1	/* x == y */
#define COB_LT			2	/* x <  y */
#define COB_LE			3	/* x <= y */
#define COB_GT			4	/* x >  y */
#define COB_GE			5	/* x >= y */
#define COB_NE			6	/* x != y */
#define COB_FI			7	/* First */
#define COB_LA			8	/* Last */

#define COB_ASCENDING		0
#define COB_DESCENDING		1

#define COB_FILE_MODE		0666

/* Organization, FIXME: change to enum */

#define COB_ORG_SEQUENTIAL	0
#define COB_ORG_LINE_SEQUENTIAL	1
#define COB_ORG_RELATIVE	2
#define COB_ORG_INDEXED		3
#define COB_ORG_SORT		4
#define COB_ORG_MAX		5
#define COB_ORG_MESSAGE	6 /* only for syntax checks */

/* Access mode, FIXME: change to enum */

#define COB_ACCESS_SEQUENTIAL	1
#define COB_ACCESS_DYNAMIC	2
#define COB_ACCESS_RANDOM	3

/* SELECT features */

#define	COB_SELECT_FILE_STATUS	(1U << 0)
#define	COB_SELECT_EXTERNAL	(1U << 1)
#define	COB_SELECT_LINAGE	(1U << 2)
#define	COB_SELECT_SPLITKEY	(1U << 3)
#define	COB_SELECT_STDIN	(1U << 4)
#define	COB_SELECT_STDOUT	(1U << 5)
#define	COB_SELECT_TEMPORARY	(1U << 6)

#define COB_FILE_SPECIAL(x)	\
	((x)->flag_select_features & (COB_SELECT_STDIN | COB_SELECT_STDOUT))
#define COB_FILE_STDIN(x)	((x)->flag_select_features & COB_SELECT_STDIN)
#define COB_FILE_STDOUT(x)	((x)->flag_select_features & COB_SELECT_STDOUT)
#define COB_FILE_TEMPORARY(x)	((x)->flag_select_features & COB_SELECT_TEMPORARY)

/* Lock mode */

#define COB_LOCK_EXCLUSIVE	(1U << 0)
#define COB_LOCK_MANUAL		(1U << 1)
#define COB_LOCK_AUTOMATIC	(1U << 2)
#define COB_LOCK_MULTIPLE	(1U << 3)
#define COB_LOCK_OPEN_EXCLUSIVE	(1U << 4)

#define COB_FILE_EXCLUSIVE	(COB_LOCK_EXCLUSIVE | COB_LOCK_OPEN_EXCLUSIVE)

/* Open mode */

#define COB_OPEN_CLOSED		0
#define COB_OPEN_INPUT		1
#define COB_OPEN_OUTPUT		2
#define COB_OPEN_I_O		3
#define COB_OPEN_EXTEND		4
#define COB_OPEN_LOCKED		5

/* Close options */

#define COB_CLOSE_NORMAL	0
#define COB_CLOSE_LOCK		1
#define COB_CLOSE_NO_REWIND	2
#define COB_CLOSE_UNIT		3
#define COB_CLOSE_UNIT_REMOVAL	4

/* Write options */

#define COB_WRITE_MASK		0x0000FFFF

#define COB_WRITE_LINES		0x00010000
#define COB_WRITE_PAGE		0x00020000
#define COB_WRITE_CHANNEL	0x00040000
#define COB_WRITE_AFTER		0x00100000
#define COB_WRITE_BEFORE	0x00200000
#define COB_WRITE_EOP		0x00400000
#define COB_WRITE_LOCK		0x00800000
#define COB_WRITE_NO_LOCK	0x01000000

/* Read options */

#define COB_READ_NEXT		(1 << 0)
#define COB_READ_PREVIOUS	(1 << 1)
#define COB_READ_FIRST		(1 << 2)
#define COB_READ_LAST		(1 << 3)
#define COB_READ_LOCK		(1 << 4)
#define COB_READ_NO_LOCK	(1 << 5)
#define COB_READ_KEPT_LOCK	(1 << 6)
#define COB_READ_WAIT_LOCK	(1 << 7)
#define COB_READ_IGNORE_LOCK	(1 << 8)

#define COB_READ_MASK		\
	(COB_READ_NEXT | COB_READ_PREVIOUS | COB_READ_FIRST | COB_READ_LAST)

/* I-O status */

#define COB_STATUS_00_SUCCESS			00
#define COB_STATUS_02_SUCCESS_DUPLICATE		02
#define COB_STATUS_04_SUCCESS_INCOMPLETE	04
#define COB_STATUS_05_SUCCESS_OPTIONAL		05
#define COB_STATUS_07_SUCCESS_NO_UNIT		07
#define COB_STATUS_09_READ_DATA_BAD			 9
#define COB_STATUS_10_END_OF_FILE		10
#define COB_STATUS_14_OUT_OF_KEY_RANGE		14
#define COB_STATUS_21_KEY_INVALID		21
#define COB_STATUS_22_KEY_EXISTS		22
#define COB_STATUS_23_KEY_NOT_EXISTS		23
#define COB_STATUS_24_KEY_BOUNDARY		24
#define COB_STATUS_30_PERMANENT_ERROR		30
#define COB_STATUS_31_INCONSISTENT_FILENAME	31
#define COB_STATUS_34_BOUNDARY_VIOLATION	34
#define COB_STATUS_35_NOT_EXISTS		35
#define COB_STATUS_37_PERMISSION_DENIED		37
#define COB_STATUS_38_CLOSED_WITH_LOCK		38
#define COB_STATUS_39_CONFLICT_ATTRIBUTE	39
#define COB_STATUS_41_ALREADY_OPEN		41
#define COB_STATUS_42_NOT_OPEN			42
#define COB_STATUS_43_READ_NOT_DONE		43
#define COB_STATUS_44_RECORD_OVERFLOW		44
#define COB_STATUS_45_IDENTIFICATION_FAILURE	45	/* currently not implemented */
#define COB_STATUS_46_READ_ERROR		46
#define COB_STATUS_47_INPUT_DENIED		47
#define COB_STATUS_48_OUTPUT_DENIED		48
#define COB_STATUS_49_I_O_DENIED		49
#define COB_STATUS_51_RECORD_LOCKED		51
#define COB_STATUS_52_DEAD_LOCK			52	/* currently not implemented (patch available) */
#define COB_STATUS_53_MAX_LOCKS			53	/* currently not implemented */
#define COB_STATUS_54_MAX_LOCKS_FD		54	/* currently not implemented */
#define COB_STATUS_57_I_O_LINAGE		57
#define COB_STATUS_61_FILE_SHARING		61
#define COB_STATUS_71_BAD_CHAR				71
#define COB_STATUS_91_NOT_AVAILABLE		91

/* Special status */
/* Used by extfh handler */
#define	COB_NOT_CONFIGURED			32768

/* End File attributes */

/* Number store defines */

#define COB_STORE_ROUND			(1 << 0)
#define COB_STORE_KEEP_ON_OVERFLOW	(1 << 1)
#define COB_STORE_TRUNC_ON_OVERFLOW	(1 << 2)

#define COB_STORE_AWAY_FROM_ZERO	(1 << 4)
#define COB_STORE_NEAR_AWAY_FROM_ZERO	(1 << 5)
#define COB_STORE_NEAR_EVEN		(1 << 6)
#define COB_STORE_NEAR_TOWARD_ZERO	(1 << 7)
#define COB_STORE_PROHIBITED		(1 << 8)
#define COB_STORE_TOWARD_GREATER	(1 << 9)
#define COB_STORE_TOWARD_LESSER		(1 << 10)
#define COB_STORE_TRUNCATION		(1 << 11)

#define COB_STORE_MASK					\
	(COB_STORE_ROUND | COB_STORE_KEEP_ON_OVERFLOW |	\
	 COB_STORE_TRUNC_ON_OVERFLOW)

/* Screen attribute defines */

#define COB_SCREEN_BLACK		0
#define COB_SCREEN_BLUE			1
#define COB_SCREEN_GREEN		2
#define COB_SCREEN_CYAN			3
#define COB_SCREEN_RED			4
#define COB_SCREEN_MAGENTA		5
#define COB_SCREEN_YELLOW		6
#define COB_SCREEN_WHITE		7

typedef cob_s64_t cob_flags_t;

#define COB_SCREEN_LINE_PLUS		((cob_flags_t)1 << 0)
#define COB_SCREEN_LINE_MINUS		((cob_flags_t)1 << 1)
#define COB_SCREEN_COLUMN_PLUS		((cob_flags_t)1 << 2)
#define COB_SCREEN_COLUMN_MINUS		((cob_flags_t)1 << 3)
#define COB_SCREEN_AUTO			((cob_flags_t)1 << 4)
#define COB_SCREEN_BELL			((cob_flags_t)1 << 5)
#define COB_SCREEN_BLANK_LINE		((cob_flags_t)1 << 6)
#define COB_SCREEN_BLANK_SCREEN		((cob_flags_t)1 << 7)
#define COB_SCREEN_BLINK		((cob_flags_t)1 << 8)
#define COB_SCREEN_ERASE_EOL		((cob_flags_t)1 << 9)
#define COB_SCREEN_ERASE_EOS		((cob_flags_t)1 << 10)
#define COB_SCREEN_FULL			((cob_flags_t)1 << 11)
#define COB_SCREEN_HIGHLIGHT		((cob_flags_t)1 << 12)
#define COB_SCREEN_LOWLIGHT		((cob_flags_t)1 << 13)
#define COB_SCREEN_REQUIRED		((cob_flags_t)1 << 14)
#define COB_SCREEN_REVERSE		((cob_flags_t)1 << 15)
#define COB_SCREEN_SECURE		((cob_flags_t)1 << 16)
#define COB_SCREEN_UNDERLINE		((cob_flags_t)1 << 17)
#define COB_SCREEN_OVERLINE		((cob_flags_t)1 << 18)
#define COB_SCREEN_PROMPT		((cob_flags_t)1 << 19)
#define COB_SCREEN_UPDATE		((cob_flags_t)1 << 20)
#define COB_SCREEN_INPUT		((cob_flags_t)1 << 21)
#define COB_SCREEN_SCROLL_DOWN		((cob_flags_t)1 << 22)
#define COB_SCREEN_INITIAL		((cob_flags_t)1 << 23)
#define COB_SCREEN_NO_ECHO		((cob_flags_t)1 << 24)
#define COB_SCREEN_LEFTLINE		((cob_flags_t)1 << 25)
#define COB_SCREEN_NO_DISP		((cob_flags_t)1 << 26)
#define COB_SCREEN_EMULATE_NL		((cob_flags_t)1 << 27)
#define COB_SCREEN_UPPER		((cob_flags_t)1 << 28)
#define COB_SCREEN_LOWER		((cob_flags_t)1 << 29)
#define COB_SCREEN_GRID			((cob_flags_t)1 << 30)
/*#define COB_SCREEN_reserved		((cob_flags_t)1 << 31) /+ reserved for next flag used in screenio */
#define COB_SCREEN_TAB			((cob_flags_t)1 << 32) /* used for syntax checking */
#define COB_SCREEN_NO_UPDATE		((cob_flags_t)1 << 33) /* used for syntax checking */
#define COB_SCREEN_SCROLL_UP		((cob_flags_t)1 << 34) /* used for syntax checking */

#define COB_SCREEN_TYPE_GROUP		0
#define COB_SCREEN_TYPE_FIELD		1
#define COB_SCREEN_TYPE_VALUE		2
#define COB_SCREEN_TYPE_ATTRIBUTE	3

/* End Screen attribute defines */

/* Report attribute defines */

#define COB_REPORT_LINE			(1U << 0)
#define COB_REPORT_LINE_PLUS		(1U << 1)
#define COB_REPORT_COLUMN_PLUS		(1U << 2)
#define COB_REPORT_RESET_FINAL		(1U << 3)
#define COB_REPORT_HEADING		(1U << 4)
#define COB_REPORT_FOOTING		(1U << 5)
#define COB_REPORT_PAGE_HEADING		(1U << 6)
#define COB_REPORT_PAGE_FOOTING		(1U << 7)
#define COB_REPORT_CONTROL_HEADING	(1U << 8)
#define COB_REPORT_CONTROL_HEADING_FINAL (1U << 9)
#define COB_REPORT_CONTROL_FOOTING	(1U << 10)
#define COB_REPORT_CONTROL_FOOTING_FINAL (1U << 11)
#define COB_REPORT_DETAIL		(1U << 12)
#define COB_REPORT_NEXT_GROUP_LINE	(1U << 13)
#define COB_REPORT_NEXT_GROUP_PLUS	(1U << 14)
#define COB_REPORT_NEXT_GROUP_PAGE	(1U << 15)
#define COB_REPORT_LINE_NEXT_PAGE	(1U << 16)
#define COB_REPORT_NEXT_PAGE		(1U << 17)
#define COB_REPORT_GROUP_INDICATE	(1U << 18)
#define COB_REPORT_GROUP_ITEM		(1U << 19)
#define COB_REPORT_HAD_WHEN		(1U << 20)
#define COB_REPORT_COLUMN_LEFT		(1U << 21)
#define COB_REPORT_COLUMN_CENTER	(1U << 22)
#define COB_REPORT_COLUMN_RIGHT		(1U << 23)
#define COB_REPORT_PRESENT		(1U << 24)
#define COB_REPORT_BEFORE		(1U << 25)
#define COB_REPORT_PAGE			(1U << 26)
#define COB_REPORT_ALL			(1U << 27)

#define COB_REPORT_NEGATE		(1U << 28)	/* Negative: so ABSENT == PRESENT & NEGATE */

#define COB_REPORT_SUM_EMITTED		(1U << 29)
#define COB_REPORT_LINE_EMITTED		(1U << 30)
#define COB_REPORT_REF_EMITTED		(1U << 31)
#define COB_REPORT_EMITTED		(COB_REPORT_REF_EMITTED | COB_REPORT_LINE_EMITTED | COB_REPORT_SUM_EMITTED)

/* End Report attribute defines */

#define COB_JSON_CJSON			1
#define COB_JSON_JSON_C			2

/* Structure/union declarations */


/* Picture symbol structure */

typedef struct __cob_pic_symbol {
	char	symbol;
	int 	times_repeated;
} cob_pic_symbol;

/* Field attribute structure */

typedef struct __cob_field_attr {
	unsigned short		type;		/* Field type */
	unsigned short		digits;		/* Digit count */
	signed short		scale;		/* Field scale */
	unsigned short		flags;		/* Field flags */
	const cob_pic_symbol	*pic;		/* Pointer to picture string */
} cob_field_attr;

/* Field structure */

typedef struct __cob_field {
	size_t			size;		/* Field size */
	unsigned char		*data;		/* Pointer to field data */
	const cob_field_attr	*attr;		/* Pointer to attribute */
} cob_field;

#if	0	/* RXWRXW - Constant field */
/* Field structure for constants */

typedef struct __cob_const_field {
	const size_t		size;		/* Field size */
	const unsigned char	*data;		/* Pointer to field data */
	const cob_field_attr	*attr;		/* Pointer to attribute */
} cob_const_field;


/* Union for field constants */

typedef union __cob_fld_union {
	const cob_const_field	cf;
	cob_field		vf;
} cob_fld_union;
#endif

/* Representation of 128 bit FP */

typedef struct __cob_fp_128 {
	cob_u64_t	fpval[2];
} cob_fp_128;

#ifndef COB_WITHOUT_DECIMAL
/* Internal representation of decimal numbers */
/* n = value / 10 ^ scale */
/* Decimal structure */

typedef struct __cob_decimal {
	mpz_t		value;			/* GMP value definition */
	int 		scale;			/* Decimal scale */
} cob_decimal;
#endif

/* Perform stack structure */
struct cob_frame {
	void		*return_address_ptr;	/* Return address pointer */
	unsigned int	perform_through;	/* Perform number */
	unsigned int	return_address_num;	/* Return address number */
};

/* Call union structures */

typedef union __cob_content {
	unsigned char data[8];
	cob_s64_t     datall;
	cob_u64_t     dataull;
	int           dataint;
} cob_content;

typedef union __cob_call_union {
	void		*(*funcptr)();	/* Function returning "void *" */
	void		(*funcnull)();	/* Function returning nothing */
	cob_field	*(*funcfld)();	/* Function returning "cob_field *" */
	int		(*funcint)();	/* Function returning "int" */
	void		*funcvoid;	/* Redefine to "void *" */
#ifdef	_WIN32				/* stdcall variants */
	void		*(__stdcall *funcptr_std)();
	void		(__stdcall *funcnull_std)();
	cob_field	*(__stdcall *funcfld_std)();
	int		(__stdcall *funcint_std)();
#endif
} cob_call_union;

struct cob_call_struct {
	const char		*cob_cstr_name;		/* Call name */
	cob_call_union		cob_cstr_call;		/* Call entry */
	cob_call_union		cob_cstr_cancel;	/* Cancel entry */
};

/* Screen structure */
typedef struct __cob_screen {
	struct __cob_screen	*next;		/* Pointer to next */
	struct __cob_screen	*prev;		/* Pointer to previous */
	struct __cob_screen	*child;		/* For COB_SCREEN_TYPE_GROUP */
	struct __cob_screen	*parent;	/* Pointer to parent */
	cob_field		*field;		/* For COB_SCREEN_TYPE_FIELD */
	cob_field		*value;		/* For COB_SCREEN_TYPE_VALUE */
	cob_field		*line;		/* LINE */
	cob_field		*column;	/* COLUMN */
	cob_field		*foreg;		/* FOREGROUND */
	cob_field		*backg;		/* BACKGROUND */
	cob_field		*prompt;	/* PROMPT */
	int			type;		/* Structure type */
	int			occurs;		/* OCCURS */
	int			attr;		/* COB_SCREEN_TYPE_ATTRIBUTE */
} cob_screen;

/* Module structure */
#define COB_MODULE_TYPE_PROGRAM		0
#define COB_MODULE_TYPE_FUNCTION	1

/*
  For backwards compatibility of the libcob ABI, the size of existing members
  and their positions must not change! Add new members at the end.
 */
typedef struct __cob_module {
	struct __cob_module	*next;			/* Next pointer */
	cob_field		**cob_procedure_params;	/* Arguments */
	const char		*module_name;		/* Module name */
	const char		*module_formatted_date;	/* Module full date */
	const char		*module_source;		/* Module source */
	cob_call_union		module_entry;		/* Module entry */
	cob_call_union		module_cancel;		/* Module cancel */
	const unsigned char	*collating_sequence;	/* COLLATING */
	cob_field		*crt_status;		/* CRT STATUS */
	cob_field		*cursor_pos;		/* CURSOR */
	unsigned int		*module_ref_count;	/* Module ref count */
	const char		**module_path;		/* Module path */

	unsigned int		module_active;		/* Module is active */
	unsigned int		module_date;		/* Module num date */
	unsigned int		module_time;		/* Module num time */
	unsigned int		module_type;		/* Module type (program = 0, function = 1) */
	unsigned int		module_param_cnt;	/* Module param count */
	unsigned int		module_returning;	/* Module return type, currently unset+unused */
	int			module_num_params;	/* Module arg count */

	unsigned char		ebcdic_sign;		/* DISPLAY SIGN */
	unsigned char		decimal_point;		/* DECIMAL POINT */
	unsigned char		currency_symbol;	/* CURRENCY */
	unsigned char		numeric_separator;	/* Separator */

	unsigned char		flag_filename_mapping;	/* Mapping */
	unsigned char		flag_binary_truncate;	/* Truncation */
	unsigned char		flag_pretty_display;	/* Pretty display */
	unsigned char		flag_host_sign;		/* Host sign */

	unsigned char		flag_no_phys_canc;	/* No physical cancel */
	unsigned char		flag_main;		/* Main module */
	unsigned char		flag_fold_call;		/* Fold case */
	unsigned char		flag_exit_program;	/* Exit after CALL */

	unsigned char		flag_did_cancel;	/* Module has been canceled */
	unsigned char		flag_dump_ready;	/* Module was compiled with -fdump */
	unsigned char		flag_debug_trace;	/* Module debug/trace compile option */
#if 0 /* currently unused */
#define COB_MODULE_DEBUG	1
#endif
#define COB_MODULE_TRACE	2
#define COB_MODULE_TRACEALL	4

	unsigned char		unused[1];		/* Use these flags up later, added for alignment */

	unsigned int		module_stmt;		/* Last statement executed as modulated source line
											   and index to module_sources for source file */
	const char		**module_sources;	/* Source module names compiled */

	cob_field		*xml_code;		/* XML-CODE */
	cob_field		*xml_event;		/* XML-EVENT */
	cob_field		*xml_information;	/* XML-INFORMATION */
	cob_field		*xml_namespace;		/* XML-NAMESPACE */
	cob_field		*xml_nnamespace;	/* XML-NNAMESPACE */
	cob_field		*xml_namespace_prefix;	/* XML-NAMESPACE-PREFIX */
	cob_field		*xml_nnamespace_prefix;	/* XML-NNAMESPACE-PREFIX */
	cob_field		*xml_ntext;		/* XML-NTEXT */
	cob_field		*xml_text;		/* XML-TEXT */

	cob_field		*json_code;		/* JSON-CODE */
	cob_field		*json_status;		/* JSON-STATUS */

	const char	*gc_version;	/* module version, until 3.1.2: set by cob_check_version */

	unsigned char		xml_mode;		/* Mode to handle XML PARSE (may be extended) */
#define COB_XML_XMLNSS		1			/* similar to XMLPARSE(XMLNSS) Micro Focus,
											   IBM may be different (_very_ likely for error codes);
											   but the main difference is to "COMPAT" */

} cob_module;


/* User function structure */

struct cob_func_loc {
	cob_field		*ret_fld;
	cob_field		**save_proc_parms;
	cob_field		**func_params;
	unsigned char		**data;
	cob_module		*save_module;
	int			save_call_params;
	int			save_num_params;
};

/* File connector */

/* Key structure */

#define COB_MAX_KEYCOMP 8	/* max number of parts in a compound key (disam.h :: NPARTS ) */

typedef struct __cob_file_key {
	cob_field	*field;	/* Key field */
	int		flag;				/* ASCENDING/DESCENDING (for SORT) [3.x only] */
	int		tf_duplicates;			/* WITH DUPLICATES (for RELATIVE/INDEXED) */
	int		tf_ascending;			/* ASCENDING/DESCENDING (for SORT) [4.x only, for now: unused] */
	int		tf_suppress;			/* supress keys where all chars = char_suppress */
	int		char_suppress;			/* key supression character  */
	unsigned int	offset;			/* Offset of field */
	int		count_components;		/* 0..1::simple-key  2..n::split-key   */
	cob_field	*component[COB_MAX_KEYCOMP];	/* key-components iff split-key   */
} cob_file_key;


/* File version (likely can be removed from cob_file in the future) */
#define	COB_FILE_VERSION	1

/* File structure */

/*NOTE: *** Add new fields to end  ***
 *       cob_file is now allocated by cob_file_malloc in common.c
 *       so as long as you add new fields to the end there should be no
 *       need to change COB_FILE_VERSION
 */
typedef struct __cob_file {
	const char		*select_name;		/* Name in SELECT */
	unsigned char		*file_status;		/* FILE STATUS */
	cob_field		*assign;		/* ASSIGN TO */
	cob_field		*record;		/* Record area */
	cob_field		*variable_record;	/* Record size variable */
	cob_file_key		*keys;			/* ISAM/RANDOM/SORT keys */
	void			*file;			/* File specific pointer */
	void			*linorkeyptr;		/* LINAGE or SPLIT KEY */
	const unsigned char	*sort_collating;	/* SORT collating */
	void			*extfh_ptr;		/* For EXTFH usage */
	size_t			record_min;		/* Record min size */
	size_t			record_max;		/* Record max size */
	size_t			nkeys;			/* Number of keys */
	int			fd;			/* File descriptor */

	unsigned char		organization;		/* ORGANIZATION */
	unsigned char		access_mode;		/* ACCESS MODE */
	unsigned char		lock_mode;		/* LOCK MODE */
	unsigned char		open_mode;		/* OPEN MODE */
	unsigned char		flag_optional;		/* OPTIONAL */
	unsigned char		last_open_mode;		/* Mode given by OPEN */
	unsigned char		flag_operation;		/* File type specific */
	unsigned char		flag_nonexistent;	/* Nonexistent file */

	unsigned char		flag_end_of_file;	/* Reached end of file */
	unsigned char		flag_begin_of_file;	/* Reached start of file */
	unsigned char		flag_first_read;	/* OPEN/START read flag */
	unsigned char		flag_read_done;		/* READ successful */
	unsigned char		flag_select_features;	/* SELECT features */
	unsigned char		flag_needs_nl;		/* Needs NL at close */
	unsigned char		flag_needs_top;		/* Linage needs top */
	unsigned char		file_version;		/* File I/O version */

	unsigned char		flag_line_adv;		/* LINE ADVANCING */
	short				curkey;			/* Current file index read sequentially */
	short 				mapkey;			/* Remapped index number, when FD does not match file */

	struct __fcd3		*fcd;			/* FCD created via SET ... TO ADDRESS OF FH--FCD */

	cob_s64_t			record_off;		/* Starting position of last record read/written
	         			           		   (in 3.x: [LINE] SEQUENTIAL ONLY!) */
	char				*org_filename;	/* Full concatenated file name */
	char				*nxt_filename;	/* Next position in org_filename */
	unsigned int		flag_is_concat:1;	/* SEQUENTIAL concatenated file names */
} cob_file;


/* Linage structure */

typedef struct __cob_linage {
	cob_field		*linage;		/* LINAGE */
	cob_field		*linage_ctr;		/* LINAGE-COUNTER */
	cob_field		*latfoot;		/* LINAGE FOOTING */
	cob_field		*lattop;		/* LINAGE AT TOP */
	cob_field		*latbot;		/* LINAGE AT BOTTOM */
	int			lin_lines;		/* Current Linage */
	int			lin_foot;		/* Current Footage */
	int			lin_top;		/* Current Top */
	int			lin_bot;		/* Current Bottom */
} cob_linage;


/********************/
/* Report structure */
/********************/

/* for each SUM field of each line in the report */
typedef struct __cob_report_sum {
	struct __cob_report_sum	*next;			/* Next field */
	cob_field		*f;			/* Field to be summed */
} cob_report_sum;

/* for each field of each line in the report */
typedef struct __cob_report_field {
	struct __cob_report_field *next;			/* Next field */
	cob_field		*f;			/* Field definition */
	cob_field		*source;		/* Field SOURCE */
	cob_field		*control;		/* CONTROL Field */
	char			*litval;		/* Literal value */
	int			litlen;			/* Length of literal string */
	unsigned int		flags;
	int			line;
	int			column;
	int			step_count;
	int			next_group_line;	/* NEXT GROUP line or PLUS line; see flags */
	unsigned int		level:8;		/* Data item level number */
	unsigned int		group_indicate:1;	/* field had GROUP INDICATE */
	unsigned int		suppress:1;		/* SUPPRESS display of this field */
	unsigned int		present_now:1;		/* PRESENT BEFORE|AFTER to be processed */
} cob_report_field;

/* for each line of a report */
typedef struct __cob_report_line {
	struct __cob_report_line	*sister;		/* Next line */
	struct __cob_report_line	*child;			/* Child line */
	cob_report_field	*fields;		/* List of fields on this line */
	cob_field		*control;		/* CONTROL Field */
	int			use_decl;		/* Label# of Declaratives code */
	unsigned int		flags;			/* flags defined with line */
	int			line;			/* 'LINE' value */
	int			step_count;
	int			next_group_line;
	unsigned int		report_flags;		/* flags ORed with upper level flags */
	unsigned int		suppress:1;		/* SUPPRESS printing this line */
} cob_report_line;

/* for each 'line referencing a control field' of the report */
typedef struct __cob_report_control_ref {
	struct __cob_report_control_ref *next;		/* Next control_ref */
	cob_report_line		*ref_line;		/* Report Line with this control field */
} cob_report_control_ref;

/* for each 'control field' of the report */
typedef struct __cob_report_control {
	struct __cob_report_control *next;		/* Next control */
	const char		*name;			/* Control field name */
	cob_field		*f;			/* Field definition */
	cob_field		*val;			/* previous field value */
	cob_field		*sf;			/* save field value */
	cob_report_control_ref	*control_ref;		/* References to this control field */
	int			sequence;		/* Order of Control Break */
	unsigned int		data_change:1;		/* Control field data did change */
	unsigned int		has_heading:1;		/* CONTROL HEADING */
	unsigned int		has_footing:1;		/* CONTROL FOOTING */
	unsigned int		suppress:1;		/* SUPPRESS printing this break */
} cob_report_control;

/* for each SUM counter in the report */
typedef struct __cob_report_sumctr {
	struct __cob_report_sumctr *next;		/* Next sum counter */
	const char		*name;			/* Name of this SUM counter */
	cob_report_sum		*sum;			/* list of fields to be summed */
	cob_field		*counter;		/* Field to hold the SUM counter */
	cob_field		*f;			/* Data Field for SUM counter */
	cob_report_control	*control;		/* RESET when this control field changes */
	unsigned int		reset_final:1;		/* RESET on FINAL */
	unsigned int		control_final:1;	/* CONTROL FOOTING FINAL */
	unsigned int		subtotal:1;		/* This is a 'subtotal' counter */
	unsigned int		crossfoot:1;		/* This is a 'crossfoot' counter */
} cob_report_sum_ctr;

/* main report table for each RD */
typedef struct __cob_report {
	const char		*report_name;		/* Report name */
	struct __cob_report	*next;			/* Next report */
	cob_file		*report_file;		/* Report file */
	cob_field		*page_counter;		/* PAGE-COUNTER */
	cob_field		*line_counter;		/* LINE-COUNTER */
	cob_report_line		*first_line;		/* First defined LINE of report */
	cob_report_control	*controls;		/* control fields of report */
	cob_report_sum_ctr	*sum_counters;		/* List of SUM counters in report */
	int			def_lines;		/* Default lines */
	int			def_cols;		/* Default columns */
	int			def_heading;		/* Default heading */
	int			def_first_detail;	/* Default first detail */
	int			def_last_control;	/* Default last control */
	int			def_last_detail;	/* Default last detail */
	int			def_footing;		/* Default footing */
	int			curr_page;		/* Current page */
	int			curr_line;		/* Current line on page */
	int			curr_cols;		/* Current column on line */
	int			curr_status;		/* Current status */
	int			next_value;		/* NEXT GROUP Line/Page/Plus value */
	unsigned int		control_final:1;	/* CONTROL FINAL declared */
	unsigned int		global:1;		/* IS GLOBAL declared */
	unsigned int		first_detail:1;		/* First Detail on page */
	unsigned int		in_page_footing:1;	/* doing page footing now */
	unsigned int		in_page_heading:1;	/* doing page heading now */
	unsigned int		first_generate:1;	/* Ready for first GENERATE */
	unsigned int		initiate_done:1;	/* INITIATE has been done */
	unsigned int		next_line:1;		/* Advance to line on next DETAIL */

	unsigned int		next_line_plus:1;	/* Advance to plus line on next DETAIL */
	unsigned int		next_page:1;		/* Advance to next page on next DETAIL */
	unsigned int		next_just_set:1;	/* NEXT xxx was just set so ignore */
	unsigned int		in_report_footing:1;	/* doing report footing now */
	unsigned int		incr_line:1;		/* 'curr_lines' should be incremented */
	unsigned int		foot_next_page:1;	/* Advance to next page after all CONTROL footings */
	unsigned int		code_is_present:1;	/* CODE IS present */
	unsigned int		unused:17;		/* Use these bits up next */

	int			code_len;		/* Length to use for holding 'CODE IS' value */
	char			*code_is;		/* Value of CODE IS for this report */
} cob_report;

/* ML tree structure */

typedef struct __cob_ml_attr {
	cob_field		*name;
	cob_field		*value;
	unsigned int		is_suppressed;
	struct __cob_ml_attr	*sibling;
} cob_ml_attr;

typedef struct __cob_ml_tree {
        cob_field		*name;
	cob_ml_attr		*attrs;
	cob_field		*content;
	unsigned int		is_suppressed;
	struct __cob_ml_tree	*children;
	struct __cob_ml_tree	*sibling;
} cob_ml_tree;

/* Global variable structure */

typedef struct __cob_global {
	cob_file		*cob_error_file;	/* Last error file */
	cob_module		*cob_current_module;	/* Current module */
	const char		*last_exception_statement;	/* SLast exception: tatement */
	const char		*last_exception_id;	/* Last exception: PROGRAMM-ID / FUNCTION-ID*/
	const char		*last_exception_section;	/* Last exception: Section */
	const char		*last_exception_paragraph;	/* Last exception: Paragraph */
	const char		*cob_main_argv0;	/* Main program */
	char			*cob_locale;		/* Program locale */
	char			*cob_locale_orig;	/* Initial locale */
	char			*cob_locale_ctype;	/* Initial locale */
	char			*cob_locale_collate;	/* Initial locale */
	char			*cob_locale_messages;	/* Initial locale */
	char			*cob_locale_monetary;	/* Initial locale */
	char			*cob_locale_numeric;	/* Initial locale */
	char			*cob_locale_time;	/* Initial locale */

	int			cob_exception_code;	/* current exception code, in contrast to last_exception_code heavily changed */
	int			cob_call_params;	/* Number of current arguments
									   This is set to the actual number before a CALL
									   and is stored directly on module entry to its
									   cob_module structure within cob_module_enter().
									*/
	int			cob_initial_external;	/* First external ref */
	unsigned int		last_exception_line;		/* Last exception: Program source line */
	unsigned int		cob_got_exception;	/* Exception active (see last_exception) */
	unsigned int		cob_screen_initialized;	/* Screen initialized */
	unsigned int		cob_physical_cancel;	/* Unloading of modules */
												/* screenio / termio */
	unsigned char		*cob_term_buff;		/* Screen I/O buffer */
	int			cob_accept_status;	/* ACCEPT STATUS */

	int			cob_max_y;		/* Screen max y */
	int			cob_max_x;		/* Screen max x */

	unsigned int		cob_stmt_exception;	/* Statement has 'On Exception' */

	unsigned int		cob_debugging_mode;	/* activation of USE ON DEBUGGING code */

} cob_global;

/* File I/O function pointer structure */
struct cob_fileio_funcs {
	int	(*open)		(cob_file *, char *, const int, const int);
	int	(*close)	(cob_file *, const int);
	int	(*start)	(cob_file *, const int, cob_field *);
	int	(*read)		(cob_file *, cob_field *, const int);
	int	(*read_next)	(cob_file *, const int);
	int	(*write)	(cob_file *, const int);
	int	(*rewrite)	(cob_file *, const int);
	int	(*fdelete)	(cob_file *);
};

#ifndef COB_WITHOUT_JMP
/* Low level jump structure */
struct cobjmp_buf {
	int	cbj_int[4];
	void	*cbj_ptr[4];
	jmp_buf	cbj_jmp_buf;
	void	*cbj_ptr_rest[2];
};
#endif

#define __LIBCOB_VERSION	3
#define __LIBCOB_VERSION_MINOR		2
#define __LIBCOB_VERSION_PATCHLEVEL	0	/* Note: possibly differs from patchelvel shown with cobc --version! */

#define __LIBCOB_RELEASE (__LIBCOB_VERSION * 10000 + __LIBCOB_VERSION_MINOR * 100 + __LIBCOB_VERSION_PATCHLEVEL)


/*******************************/

/* Function declarations */

/*******************************/
/* Functions in common.c */
COB_EXPIMP const char*	cob_get_sig_name (int);
COB_EXPIMP const char*	cob_get_sig_description (int);
COB_EXPIMP const char*	libcob_version (void);
COB_EXPIMP int		set_libcob_version (int *, int *, int *);
COB_EXPIMP void		print_info	(void);
COB_EXPIMP void		print_info_detailed	(const int);
COB_EXPIMP void		print_version	(void);
COB_EXPIMP void		print_version_summary (void);
COB_EXPIMP int		cob_load_config	(void);
COB_EXPIMP void		print_runtime_conf	(void);

COB_EXPIMP void		cob_set_exception	(const int);
COB_EXPIMP int		cob_last_exception_is	(const int);

COB_EXPIMP int		cob_last_exit_code	(void);
COB_EXPIMP const char*	cob_last_runtime_error	(void);

COB_EXPIMP void		cob_runtime_hint	(const char *, ...) COB_A_FORMAT12;
COB_EXPIMP void		cob_runtime_error	(const char *, ...) COB_A_FORMAT12;
COB_EXPIMP void		cob_runtime_warning	(const char *, ...) COB_A_FORMAT12;

/* General functions */

COB_EXPIMP int		cob_is_initialized	(void);
COB_EXPIMP cob_global		*cob_get_global_ptr	(void);

COB_EXPIMP void	cob_init			(const int, char **);
COB_EXPIMP void	cob_init_nomain		(const int, char **);
COB_EXPIMP void	cob_common_init		(void *);

COB_EXPIMP int	cob_module_global_enter	(cob_module **, cob_global **,
						 const int, const int, const unsigned int *);
COB_EXPIMP void	cob_module_enter		(cob_module **, cob_global **,
						 const int);
COB_EXPIMP void	cob_module_leave		(cob_module *);

COB_EXPIMP void	cob_module_free	(cob_module **);

DECLNORET COB_EXPIMP void	cob_stop_run	(const int) COB_A_NORETURN;
DECLNORET COB_EXPIMP void	cob_stop_error	(void) COB_A_NORETURN;
DECLNORET COB_EXPIMP void	cob_fatal_error	(const enum cob_fatal_error) COB_A_NORETURN;

COB_EXPIMP void	*cob_malloc			(const size_t) COB_A_MALLOC;
COB_EXPIMP void	*cob_realloc			(void *, const size_t, const size_t) COB_A_MALLOC;
COB_EXPIMP char	*cob_strdup				(const char *);
COB_EXPIMP void	cob_free			(void *);
COB_EXPIMP void	*cob_fast_malloc		(const size_t) COB_A_MALLOC;
COB_EXPIMP void	*cob_cache_malloc		(const size_t) COB_A_MALLOC;
COB_EXPIMP void	*cob_cache_realloc		(void *, const size_t);
COB_EXPIMP void	cob_cache_free			(void *);

COB_EXPIMP void	cob_set_locale			(cob_field *, const int);

COB_EXPIMP int 	cob_setenv		(const char *, const char *, int);
COB_EXPIMP int 	cob_unsetenv		(const char *);
COB_EXPIMP char	*cob_getenv_direct		(const char *);
COB_EXPIMP char* cob_expand_env_string	(char*);
COB_EXPIMP char	*cob_getenv			(const char *);
COB_EXPIMP int	cob_putenv			(char *);

COB_EXPIMP void	cob_check_version		(const char *, const char *,
						 const int);

COB_EXPIMP struct cob_func_loc *cob_save_func	(cob_field **,
									const int, const int, ...);
COB_EXPIMP void	cob_restore_func		(struct cob_func_loc *);

COB_EXPIMP void	cob_accept_arg_number		(cob_field *);
COB_EXPIMP void	cob_accept_arg_value		(cob_field *);
COB_EXPIMP void	cob_accept_command_line		(cob_field *);
COB_EXPIMP void	cob_accept_date			(cob_field *);
COB_EXPIMP void	cob_accept_date_yyyymmdd	(cob_field *);
COB_EXPIMP void	cob_accept_day			(cob_field *);
COB_EXPIMP void	cob_accept_day_yyyyddd		(cob_field *);
COB_EXPIMP void	cob_accept_day_of_week		(cob_field *);
COB_EXPIMP void	cob_accept_environment		(cob_field *);
COB_EXPIMP void	cob_accept_exception_status	(cob_field *);
COB_EXPIMP void	cob_accept_time			(cob_field *);
COB_EXPIMP void	cob_accept_user_name		(cob_field *);
COB_EXPIMP void	cob_display_command_line	(cob_field *);
COB_EXPIMP void	cob_display_environment		(const cob_field *);
COB_EXPIMP void	cob_display_env_value		(const cob_field *);
COB_EXPIMP void	cob_display_arg_number		(cob_field *);
COB_EXPIMP void	cob_get_environment		(const cob_field *, cob_field *);
COB_EXPIMP void	cob_set_environment		(const cob_field *,
						 const cob_field *);
COB_EXPIMP void	cob_chain_setup			(void *, const size_t,
						 const size_t);
COB_EXPIMP void	cob_allocate			(unsigned char **, cob_field *,
						 cob_field *, cob_field *);
COB_EXPIMP void	cob_free_alloc			(unsigned char **, unsigned char *);
COB_EXPIMP void	cob_continue_after		(cob_field *);
COB_EXPIMP int	cob_extern_init			(void);
COB_EXPIMP int	cob_tidy			(void);
COB_EXPIMP char	*cob_command_line		(int, int *, char ***,
						 char ***, char **);

COB_EXPIMP void	cob_incr_temp_iteration 	(void);
COB_EXPIMP void	cob_temp_name			(char *, const char *);

/* System routines */
COB_EXPIMP int	cob_sys_exit_proc	(const void *, const void *);
COB_EXPIMP int	cob_sys_error_proc	(const void *, const void *);
COB_EXPIMP int	cob_sys_system		(const void *);
COB_EXPIMP int	cob_sys_hosted		(void *, const void *);
COB_EXPIMP int	cob_sys_and		(const void *, void *, const int);
COB_EXPIMP int	cob_sys_or		(const void *, void *, const int);
COB_EXPIMP int	cob_sys_nor		(const void *, void *, const int);
COB_EXPIMP int	cob_sys_xor		(const void *, void *, const int);
COB_EXPIMP int	cob_sys_imp		(const void *, void *, const int);
COB_EXPIMP int	cob_sys_nimp		(const void *, void *, const int);
COB_EXPIMP int	cob_sys_eq		(const void *, void *, const int);
COB_EXPIMP int	cob_sys_not		(void *, const int);
COB_EXPIMP int	cob_sys_xf4		(void *, const void *);
COB_EXPIMP int	cob_sys_xf5		(const void *, void *);
COB_EXPIMP int	cob_sys_x91		(void *, const void *, void *);
COB_EXPIMP int	cob_sys_toupper		(void *, const int);
COB_EXPIMP int	cob_sys_tolower		(void *, const int);
COB_EXPIMP int	cob_sys_oc_nanosleep	(const void *);
COB_EXPIMP int	cob_sys_getpid		(void);
COB_EXPIMP int	cob_sys_return_args	(void *);
COB_EXPIMP int	cob_sys_parameter_size	(void *);
COB_EXPIMP int	cob_sys_fork	(void);
COB_EXPIMP int	cob_sys_waitpid	(const void *);

/*
 * cob_sys_getopt_long_long
 */
COB_EXPIMP int	cob_sys_getopt_long_long	(void*, void*, void*, const int, void*, void*);
typedef struct __longoption_def {
	char name[25];
	char has_option;
	char return_value_pointer[sizeof(char*)];
	char return_value[4];
} longoption_def;


COB_EXPIMP int	cob_sys_sleep		(const void *);
COB_EXPIMP int	cob_sys_calledby	(void *);
COB_EXPIMP int	cob_sys_justify		(void *, ...);
COB_EXPIMP int	cob_sys_printable	(void *, ...);

COB_EXPIMP int	cob_sys_extfh		(const void *, void *);

/* Utilities */

/* compatibility functions up to GnuCOBOL 2.2 */
COB_EXPIMP void	cob_set_location	(const char *, const unsigned int,
					 const char *, const char *,
					 const char *);
COB_EXPIMP void	cob_trace_section	(const char *, const char *, const int);
/* new functions from 3.0-dev on: */
COB_EXPIMP void	cob_trace_sect		(const char *name);
COB_EXPIMP void	cob_trace_para		(const char *name);
COB_EXPIMP void	cob_trace_entry		(const char *name);
COB_EXPIMP void	cob_trace_exit		(const char *name);
COB_EXPIMP void	cob_trace_stmt		(const char *stmt);

COB_EXPIMP void			*cob_external_addr	(const char *, const int);
COB_EXPIMP unsigned char	*cob_get_pointer	(const void *);
COB_EXPIMP void			cob_ready_trace		(void);
COB_EXPIMP void			cob_reset_trace		(void);
COB_EXPIMP void			cob_nop (void);

/* Call from outside to set/read/re-evaluate libcob options */
enum cob_runtime_option_switch {
	COB_SET_RUNTIME_TRACE_FILE = 0,				/* 'p' is  FILE *  */
	COB_SET_RUNTIME_DISPLAY_PRINTER_FILE = 1,	/* 'p' is  FILE *  */
	COB_SET_RUNTIME_RESCAN_ENV = 2,		/* rescan environment variables */
	COB_SET_RUNTIME_DISPLAY_PUNCH_FILE = 3,	/* 'p' is  FILE *  */
	COB_SET_RUNTIME_DUMP_FILE = 4	/* 'p' is  FILE *  */
};
COB_EXPIMP void			cob_set_runtime_option		(enum cob_runtime_option_switch opt, void *p);
COB_EXPIMP void			*cob_get_runtime_option		(enum cob_runtime_option_switch opt);

COB_EXPIMP void			cob_stack_trace (void *target);		/* 'target' is FILE *  */
COB_EXPIMP void			cob_backtrace	(void *target, int count);		/* 'target' is FILE *  */

#define COB_GET_LINE_NUM(n) ( n & 0xFFFFF )
#define COB_GET_FILE_NUM(n) ( (n >> 20) & 0xFFF)
#define COB_SET_LINE_FILE(l,f) ( (unsigned int)((unsigned int)f<<20) | l)

/* Datetime structure */
struct cob_time
{
	int	year;			/* Year         [1900-9999] */
	int	month;			/* Month        [1-12] 1 = Jan ... 12 = Dec */
	int	day_of_month;	/* Day          [1-31] */
	int	day_of_week;	/* Day of week  [1-7] 1 = Monday ... 7 = Sunday */
	int day_of_year;	/* Days in year [1-366] -1 on _WIN32! */
	int	hour;			/* Hours        [0-23] */
	int	minute;			/* Minutes      [0-59] */
	int	second;			/* Seconds      [0-60] (1 leap second) */
	int	nanosecond;		/* Nanoseconds */
	int	offset_known;
	int	utc_offset;		/* Minutes east of UTC */
	int is_daylight_saving_time;	/* DST [-1/0/1] */
};

COB_EXPIMP struct cob_time cob_get_current_date_and_time	(void);

/* Registration of external handlers */
COB_EXPIMP void	cob_reg_sighnd	(void (*sighnd) (int));

/* Raise signal (run both internal and external handlers) */
COB_EXPIMP void	cob_raise		(int);

/* Switch */

COB_EXPIMP int	cob_get_switch		(const int);
COB_EXPIMP void	cob_set_switch		(const int, const int);

/* Comparison */

COB_EXPIMP int	cob_cmp			(cob_field *, cob_field *);

/* Class check */

COB_EXPIMP int	cob_is_omitted		(const cob_field *);
COB_EXPIMP int	cob_is_numeric		(const cob_field *);
COB_EXPIMP int	cob_is_alpha		(const cob_field *);
COB_EXPIMP int	cob_is_upper		(const cob_field *);
COB_EXPIMP int	cob_is_lower		(const cob_field *);

/* Table sort */

COB_EXPIMP void	cob_table_sort_init	(const size_t, const unsigned char *);
COB_EXPIMP void	cob_table_sort_init_key	(cob_field *, const int,
					 const unsigned int);
COB_EXPIMP void	cob_table_sort		(cob_field *, const int);

/* Run-time error checking */

COB_EXPIMP void	cob_check_numeric	(const cob_field *, const char *);
COB_EXPIMP void	cob_correct_numeric	(cob_field *);
COB_EXPIMP void	cob_check_based		(const unsigned char *,
					 const char *);
COB_EXPIMP void	cob_check_linkage	(const unsigned char *,
					 const char *, const int);
COB_EXPIMP void	cob_check_odo		(const int, const int, const int,
					 const char *, const char *);
COB_EXPIMP void	cob_check_subscript	(const int, const int,
					 const char *, const int);
COB_EXPIMP void	cob_check_ref_mod_detailed	(const char *, const int, const int,
					 const int, const int, const int);
COB_EXPIMP void	cob_check_ref_mod_minimal	(const char *,
					 const int, const int);
COB_EXPIMP void	cob_check_ref_mod	(const int, const int,
					 const int, const char *);

/* Comparison functions */
COB_EXPIMP int	cob_numeric_cmp		(cob_field *, cob_field *);

/*******************************/
/* Functions in strings.c */

COB_EXPIMP void cob_inspect_init	(cob_field *, const cob_u32_t);
COB_EXPIMP void cob_inspect_init_converting	(cob_field *);
COB_EXPIMP void cob_inspect_start	(void);
COB_EXPIMP void cob_inspect_before	(const cob_field *);
COB_EXPIMP void cob_inspect_after	(const cob_field *);
COB_EXPIMP void cob_inspect_characters	(cob_field *);
COB_EXPIMP void cob_inspect_all		(cob_field *, cob_field *);
COB_EXPIMP void cob_inspect_leading	(cob_field *, cob_field *);
COB_EXPIMP void cob_inspect_first	(cob_field *, cob_field *);
COB_EXPIMP void cob_inspect_trailing	(cob_field *, cob_field *);
COB_EXPIMP void cob_inspect_converting	(const cob_field *, const cob_field *);
COB_EXPIMP void cob_inspect_finish	(void);

COB_EXPIMP void cob_string_init		(cob_field *, cob_field *);
COB_EXPIMP void cob_string_delimited	(cob_field *);
COB_EXPIMP void cob_string_append	(cob_field *);
COB_EXPIMP void cob_string_finish	(void);

COB_EXPIMP void cob_unstring_init	(cob_field *, cob_field *, const size_t);
COB_EXPIMP void cob_unstring_delimited	(cob_field *, const cob_u32_t);
COB_EXPIMP void cob_unstring_into	(cob_field *, cob_field *, cob_field *);
COB_EXPIMP void cob_unstring_tallying	(cob_field *);
COB_EXPIMP void cob_unstring_finish	(void);

/*******************************/
/*   Functions in move.c       */
/*******************************/

COB_EXPIMP void		cob_move	(cob_field *, cob_field *);
COB_EXPIMP void		cob_move_ibm	(void *, void *, const int);
COB_EXPIMP void		cob_init_table	(void *, const size_t, const size_t);
COB_EXPIMP void		cob_set_int	(cob_field *, const int);
COB_EXPIMP int		cob_get_int	(cob_field *);
COB_EXPIMP cob_s64_t	cob_get_llint	(cob_field *);
/**************************************************/
/* Functions in move.c for C access to COBOL data */
/**************************************************/
COB_EXPIMP char *	cob_get_picx( void *cbldata, size_t len, void *charfld, size_t charlen);
COB_EXPIMP cob_s64_t	cob_get_s64_comp3(void *cbldata, int len);
COB_EXPIMP cob_s64_t	cob_get_s64_comp5(void *cbldata, int len);
COB_EXPIMP cob_s64_t	cob_get_s64_compx(void *cbldata, int len);
COB_EXPIMP cob_s64_t	cob_get_s64_pic9 (void *cbldata, int len);
COB_EXPIMP cob_u64_t	cob_get_u64_comp3(void *cbldata, int len);
COB_EXPIMP cob_u64_t	cob_get_u64_comp5(void *cbldata, int len);
COB_EXPIMP cob_u64_t	cob_get_u64_comp6(void *cbldata, int len);
COB_EXPIMP cob_u64_t	cob_get_u64_compx(void *cbldata, int len);
COB_EXPIMP cob_u64_t	cob_get_u64_pic9 (void *cbldata, int len);
COB_EXPIMP float 	cob_get_comp1(void *cbldata);
COB_EXPIMP double	cob_get_comp2(void *cbldata);
COB_EXPIMP void		cob_put_comp1(float val, void *cbldata);
COB_EXPIMP void		cob_put_comp2(double val, void *cbldata);
COB_EXPIMP void 	cob_put_picx( void *cbldata, size_t len, void *string);
COB_EXPIMP void		cob_put_s64_comp3(cob_s64_t val, void *cbldata, int len);
COB_EXPIMP void		cob_put_s64_comp5(cob_s64_t val, void *cbldata, int len);
COB_EXPIMP void		cob_put_s64_compx(cob_s64_t val, void *cbldata, int len);
COB_EXPIMP void		cob_put_s64_pic9 (cob_s64_t val, void *cbldata, int len);
COB_EXPIMP void		cob_put_u64_comp3(cob_u64_t val, void *cbldata, int len);
COB_EXPIMP void		cob_put_u64_comp5(cob_u64_t val, void *cbldata, int len);
COB_EXPIMP void		cob_put_u64_comp6(cob_u64_t val, void *cbldata, int len);
COB_EXPIMP void		cob_put_u64_compx(cob_u64_t val, void *cbldata, int len);
COB_EXPIMP void		cob_put_u64_pic9 (cob_u64_t val, void *cbldata, int len);
COB_EXPIMP void		cob_put_pointer(void *val, void *cbldata);


/**************************/
/* Functions in numeric.c */

#ifndef COB_WITHOUT_DECIMAL
COB_EXPIMP void	cob_decimal_init	(cob_decimal *);
COB_EXPIMP void	cob_decimal_clear	(cob_decimal *);
COB_EXPIMP void cob_decimal_set_llint	(cob_decimal *, const cob_s64_t);
COB_EXPIMP void cob_decimal_set_ullint	(cob_decimal *, const cob_u64_t);
COB_EXPIMP void	cob_decimal_set_field	(cob_decimal *, cob_field *);
COB_EXPIMP int	cob_decimal_get_field	(cob_decimal *, cob_field *, const int);
COB_EXPIMP void	cob_decimal_copy	(cob_decimal *, cob_decimal *);
COB_EXPIMP void	cob_decimal_add		(cob_decimal *, cob_decimal *);
COB_EXPIMP void	cob_decimal_sub		(cob_decimal *, cob_decimal *);
COB_EXPIMP void	cob_decimal_mul		(cob_decimal *, cob_decimal *);
COB_EXPIMP void	cob_decimal_div		(cob_decimal *, cob_decimal *);
COB_EXPIMP void	cob_decimal_pow		(cob_decimal *, cob_decimal *);
COB_EXPIMP int	cob_decimal_cmp		(cob_decimal *, cob_decimal *);
COB_EXPIMP void	cob_decimal_align(cob_decimal *, const int);
COB_EXPIMP void cob_logical_not (cob_decimal *d0, cob_decimal *d1);
COB_EXPIMP void cob_logical_and (cob_decimal *d0, cob_decimal *d1);
COB_EXPIMP void cob_logical_xor (cob_decimal *d0, cob_decimal *d1);
COB_EXPIMP void cob_logical_or  (cob_decimal *d0, cob_decimal *d1);
COB_EXPIMP void cob_logical_left  (cob_decimal *d0, cob_decimal *d1);
COB_EXPIMP void cob_logical_right (cob_decimal *d0, cob_decimal *d1);
COB_EXPIMP void cob_logical_left_c  (cob_decimal *d0, cob_decimal *d1, int sz);
COB_EXPIMP void cob_logical_right_c (cob_decimal *d0, cob_decimal *d1, int sz);
#endif

COB_EXPIMP void	cob_add			(cob_field *, cob_field *, const int);
COB_EXPIMP void	cob_sub			(cob_field *, cob_field *, const int);
COB_EXPIMP void	cob_mul			(cob_field *, cob_field *, const int);
COB_EXPIMP void	cob_div			(cob_field *, cob_field *, const int);
COB_EXPIMP int	cob_add_int		(cob_field *, const int, const int);
COB_EXPIMP int	cob_sub_int		(cob_field *, const int, const int);
COB_EXPIMP void	cob_div_quotient	(cob_field *, cob_field *,
					 cob_field *, const int);
COB_EXPIMP void	cob_div_remainder	(cob_field *, const int);

COB_EXPIMP int	cob_cmp_int		(cob_field *, const int);
COB_EXPIMP int	cob_cmp_uint		(cob_field *, const unsigned int);
COB_EXPIMP int	cob_cmp_llint		(cob_field *, const cob_s64_t);
COB_EXPIMP int	cob_cmp_packed		(cob_field *, const cob_s64_t);
COB_EXPIMP int	cob_cmp_numdisp		(const unsigned char *,
					 const size_t, const cob_s64_t,
					 const cob_u32_t);
COB_EXPIMP int	cob_cmp_float		(cob_field *, cob_field *);
COB_EXPIMP void	cob_set_packed_zero	(cob_field *);
COB_EXPIMP void	cob_set_packed_int	(cob_field *, const int);

COB_EXPIMP void	cob_decimal_alloc	(const cob_u32_t, ...);
COB_EXPIMP void	cob_decimal_push	(const cob_u32_t, ...);
COB_EXPIMP void	cob_decimal_pop		(const cob_u32_t, ...);

COB_EXPIMP void	cob_gmp_free		(void *);

COB_EXPIMP cob_s32_t	cob_s32_pow	(cob_s32_t, cob_s32_t);
COB_EXPIMP cob_s64_t	cob_s64_pow (cob_s64_t, cob_s64_t);


/*******************************/
/* Functions in call.c */

DECLNORET COB_EXPIMP void	cob_call_error		(void) COB_A_NORETURN;
COB_EXPIMP void		cob_field_constant (cob_field *f, cob_field *t, cob_field_attr *a, void *d);

COB_EXPIMP void		cob_set_cancel		(cob_module *);
COB_EXPIMP int		cob_encode_program_id (const unsigned char * const, unsigned char * const,
						 const int, const int);
COB_EXPIMP void		*cob_resolve		(const char *);
COB_EXPIMP void		*cob_resolve_cobol	(const char *, const int,
						 const int);
COB_EXPIMP void		*cob_resolve_func	(const char *);
COB_EXPIMP const char	*cob_resolve_error	(void);
COB_EXPIMP void		*cob_call_field		(const cob_field *,
						 const struct cob_call_struct *,
						 const unsigned int,
						 const int);
COB_EXPIMP void		cob_cancel_field	(const cob_field *,
						 const struct cob_call_struct *);
COB_EXPIMP void		cob_cancel		(const char *);
COB_EXPIMP int		cob_call_with_exception_check (const char*, const int, void **);
COB_EXPIMP int		cob_call		(const char *, const int, void **);
COB_EXPIMP int		cob_func		(const char *, const int, void **);

#ifndef COB_WITHOUT_JMP
COB_EXPIMP void		*cob_savenv		(struct cobjmp_buf *);
COB_EXPIMP void		*cob_savenv2		(struct cobjmp_buf *, const int);
COB_EXPIMP void		cob_longjmp		(struct cobjmp_buf *);
#endif

COB_EXPIMP int		cob_get_num_params ( void );
COB_EXPIMP int		cob_get_param_constant ( int num_param );
COB_EXPIMP int		cob_get_param_digits( int num_param );
COB_EXPIMP int		cob_get_param_scale( int num_param );
COB_EXPIMP int		cob_get_param_sign ( int num_param );
COB_EXPIMP int		cob_get_param_size ( int num_param );
COB_EXPIMP int		cob_get_param_type ( int num_param );
COB_EXPIMP void *	cob_get_param_data ( int num_param );
COB_EXPIMP cob_s64_t	cob_get_s64_param  ( int num_param );
COB_EXPIMP cob_u64_t	cob_get_u64_param  ( int num_param );
COB_EXPIMP double	cob_get_dbl_param  ( int num_param );
COB_EXPIMP char *	cob_get_picx_param ( int num_param, void *charfld, size_t charlen );
COB_EXPIMP void *	cob_get_grp_param  ( int num_param, void *charfld, size_t charlen );
COB_EXPIMP void		cob_put_dbl_param  ( int num_param, double value );
COB_EXPIMP void		cob_put_s64_param  ( int num_param, cob_s64_t value );
COB_EXPIMP void		cob_put_u64_param  ( int num_param, cob_u64_t value );
COB_EXPIMP void 	cob_put_picx_param ( int num_param, void *charfld );
COB_EXPIMP void  	cob_put_grp_param  ( int num_param, void *charfld, size_t charlen );

COB_EXPIMP const char	*cob_get_param_str ( int num_param, char *buff, size_t size);
COB_EXPIMP const char	*cob_get_param_str_buffered ( int num_param );
COB_EXPIMP int		cob_put_param_str ( int num_param, const char *src );


COB_EXPIMP void		cob_runtime_warning_external	(const char *, const int,
						const char *, ...) COB_A_FORMAT34;

/* get access to one of the fields (to only operate with libcob functions on it!) */
COB_EXPIMP cob_field	*cob_get_param_field (int n, const char *caller_name);
COB_EXPIMP int		cob_get_field_size (const cob_field *);
COB_EXPIMP int		cob_get_field_type (const cob_field *);
COB_EXPIMP int		cob_get_field_digits	(const cob_field *);
COB_EXPIMP int		cob_get_field_scale	(const cob_field *);
COB_EXPIMP int		cob_get_field_sign	(const cob_field *);
COB_EXPIMP int		cob_get_field_constant (const cob_field *);
COB_EXPIMP const char	*explain_field_type (const cob_field *);

/* get the field's pretty-display value */
COB_EXPIMP const char	*cob_get_field_str (const cob_field *, char *buff, size_t size);
/* get the field's pretty-display value with an internal buffer for one-time access */
COB_EXPIMP const char	*cob_get_field_str_buffered (const cob_field *);
/* set the field's data using the appropriate internal type, returns EINVAL if data is invalid */
COB_EXPIMP int		cob_put_field_str (const cob_field *, const char *);

/*******************************/
/* Functions in screenio.c */

COB_EXPIMP void		cob_screen_line_col	(cob_field *, const int);
COB_EXPIMP void		cob_screen_display	(cob_screen *, cob_field *,
					 cob_field *, const int);
COB_EXPIMP void		cob_screen_accept	(cob_screen *, cob_field *,
					 cob_field *, cob_field *,
					 const int);
COB_EXPIMP void		cob_field_display	(cob_field *, cob_field *, cob_field *,
					 cob_field *, cob_field *, cob_field *,
					 cob_field *, const cob_flags_t);
COB_EXPIMP void		cob_field_accept	(cob_field *, cob_field *, cob_field *,
					 cob_field *, cob_field *, cob_field *,
					 cob_field *, cob_field *, cob_field *,
					 const cob_flags_t);
COB_EXPIMP int		cob_display_text (const char *);
COB_EXPIMP int		cob_display_formatted_text (const char *, ...);
COB_EXPIMP int		cob_get_char	(void);
COB_EXPIMP void		cob_set_cursor_pos	(int, int);
COB_EXPIMP void		cob_accept_escape_key	(cob_field *);
COB_EXPIMP int		cob_sys_clear_screen	(void);
COB_EXPIMP int		cob_sys_sound_bell	(void);
COB_EXPIMP int		cob_sys_get_scr_size	(unsigned char *, unsigned char *);
COB_EXPIMP int		cob_sys_get_char	(unsigned char *);
COB_EXPIMP int		cob_get_text 		(char *, int);
COB_EXPIMP int		cob_get_scr_cols	(void);
COB_EXPIMP int		cob_get_scr_lines	(void);
COB_EXPIMP int		cob_sys_get_csr_pos	(unsigned char *);
COB_EXPIMP int		cob_sys_set_csr_pos	(unsigned char *);

/******************************************************************************
*                                                                             *
*  Data structure definitions and function prototypes for the External File   *
*  Handler (ExtFH) as defined by Micro Focus COBOL for use with GnuCOBOL      *
*                                                                             *
******************************************************************************/
/*
 *  COBOL status code values
*/
#define S1_SUCCESS		'0'
#define S1_AT_END		'1'
#define S1_INVALID_KEY		'2'
#define S1_PERMANENT_ERROR	'3'
#define S1_LOGIC_ERROR		'4'
#define S1_RUN_TIME_ERROR	'9'

#define S2_NO_INFO		'0'			/* S1_SUCCESS */
#define S2_DUPLICATE		'2'
#define S2_REC_LENGTH		'4'
#define S2_FILE_MISSING		'5'
#define S2_REEL_UNIT		'7'
#define S2_AT_END		'0'			/* S1_AT_END */
#define S2_KEY_LENGTH		'4'
#define S2_SEQ_ERROR		'1'			/* S1_INVALID_KEY */
#define S2_DUPLICATE_ERROR	'2'
#define S2_NO_FIND		'3'
#define S2_BOUNDARY_ERROR	'4'
#define S2_OPEN_ERROR		'7'			/* S1_PERMANENT_ERROR */
#define S2_OPEN_LOCK		'8'
#define S2_ATTR_CONFLICT	'9'
#define S2_ALREADY_OPEN		'1'			/* S1_LOGIC_ERROR */
#define S2_ALREADY_CLOSED	'2'
#define S2_NO_READ		'3'
#define S2_NO_NEXT		'6'
#define S2_NOT_INPUT		'7'
#define S2_NOT_OUTPUT		'8'
#define S2_NOT_OUTPUT2		'9'

/********************************************
	INDEXED FILE Key definition block
********************************************/
#define MF_MAXKEYS	64
typedef struct {
	unsigned char	count[2];		/* Component count */
	unsigned char	offset[2];		/* Offset to components */
	unsigned char	keyFlags;
#define	KEY_SPARSE		0x02
#define	KEY_PRIMARY		0x10
#define	KEY_DUPS		0x40
	unsigned char	compFlags;
#define KEY_COMP_DUPS		0x01
#define KEY_COMP_LEADING	0x02
#define KEY_COMP_TRAILING	0x04
	unsigned char	sparse;			/* Character which defines SPARSE key */
	unsigned char	reserved[9];
} KDB_KEY;

typedef struct {
	unsigned char	kdbLen[2];
	char		filler[4];
	unsigned char	nkeys[2];
	char		filler2[6];
	KDB_KEY	 key[MF_MAXKEYS];
} KDB;

typedef struct {
	unsigned char	kdbLen[2];
	char		filler[4];
	unsigned char	nkeys[2];
	char		filler2[6];
} KDB_GLOBAL;

typedef struct {
	unsigned char	desc;
	unsigned char	type;
	unsigned char	pos[4];				/* Position in record */
	unsigned char	len[4];				/* length of key component */
} EXTKEY;

#define MF_MAXKEYAREA (sizeof(KDB)+(sizeof(EXTKEY)*MF_MAXKEYS))
/****************************
 *  File Control Description (FCD).  The format of this structure is
 *  defined by the MicroFocus COBOL compiler.  Do not change this
 *  structure unless required by changes to MF COBOL.
****************************/

#define pointer_8byte(type, name)	\
	union {					\
		type	*ptr_name;			\
		char	filler[8];			\
	} name

/**********************************************************/
/* Following is the 64-bit FCD (or also known as FCD3) */
/* This format is used at least for:                      */
/* - MF Visual COBOL         (both 32 and 64 bit)         */
/* - MF Developer Enterprise (both 32 and 64 bit)         */
/* - MF Server Express       (64 bit)                     */
/* - MF Studio Enterprise    (64 bit)                     */
/*                                                        */
/* The FCD2 format is currently not supported, it was     */
/* used at least for                                      */
/* - MF Server Express, Net Express  (32 bit)             */
/* - MF Studio Enterprise            (32 bit)             */
/*                                                        */
/* MF says: FCD 1 is obsolete and should never be used    */
/**********************************************************/
typedef struct __fcd3 {
	unsigned char	fileStatus[2];		/* I/O completion status */
	unsigned char	fcdLen[2];			/* contains length of FCD */
	unsigned char	fcdVer;				/* FCD format version */
#define FCD_VER_64Bit	1
	unsigned char	fileOrg;			/* file organization */
#define ORG_LINE_SEQ		0
#define ORG_SEQ			1
#define ORG_INDEXED		2
#define ORG_RELATIVE		3
#define ORG_DETERMINE		255			/* not really implemented yet */
	unsigned char	accessFlags;		/* status byte (bit 7) & file access flags (bits 0-6)*/
#define ACCESS_SEQ			0
#define ACCESS_DUP_PRIME	1			/* not implemented yet */
#define ACCESS_RANDOM		4
#define ACCESS_DYNAMIC		8
#define ACCESS_USER_STAT	0x80
	unsigned char	openMode;			/* open mode INPUT, I-O, etc. */
#define OPEN_INPUT		0		
#define OPEN_OUTPUT		1
#define OPEN_IO			2
#define OPEN_EXTEND		3
#define OPEN_NOT_OPEN	128
	unsigned char	recordMode;			/* recording mode */
#define REC_MODE_FIXED		0	
#define REC_MODE_VARIABLE	1
	unsigned char	fileFormat;			/* File format */
#define MF_FF_DEFAULT		0		/* Default format */
#define MF_FF_CISAM			1		/* C-ISAM format */
#define MF_FF_LEVELII		2		/* LEVEL II COBOL format */
#define MF_FF_COBOL			3		/* IDXFORMAT"3" format (COBOL2) */
#define MF_FF_IDX4			4		/* IDXFORMAT"4" format */
#define MF_FF_IDX8			8		/* IDXFORMAT"8" format (BIG) */
	unsigned char	deviceFlag;		
	unsigned char	lockAction;		
	unsigned char	compType;			/* data compression type */
	unsigned char	blocking;				
	unsigned char	idxCacheSz;			/* index cache size */
	unsigned char	percent;
	unsigned char	blockSize;			/* block-size, fcd-convert only */
	unsigned char	flags1;
	unsigned char	flags2;
	unsigned char	mvsFlags;
	unsigned char	fstatusType;
#define MF_FST_COBOL85		0x80
#define MF_FST_NoSpaceFill	0x40
#define MF_FST_NoStripSpaces	0x20
#define MF_FST_NoExpandtabs	0x10
#define MF_FST_LsRecLF		0x08
#define MF_FST_InsertTabs	0x04
#define MF_FST_InsertNulls	0x02
#define MF_FST_CRdelim		0x01
	unsigned char	otherFlags;			/* miscellaneous flags */
#define OTH_OPTIONAL		0x80		
#define OTH_NOT_OPTIONAL	0x20
#define OTH_EXTERNAL		0x10
#define OTH_DOLSREAD		0x08
#define OTH_NODETECTLOCK	0x04
#define OTH_MULTI_REEL		0x02
#define OTH_LINE_ADVANCE	0x01
	unsigned char	transLog;
	unsigned char	lockTypes;
	unsigned char	fsFlags;
	unsigned char	confFlags;			/* configuration flags */
#define MF_CF_WRTHRU	0x80			/* Write through to disk */
#define MF_CF_RELADRS	0x40			/* Use relative byte address */
#define MF_CF_UPPTR		0x20			/* Update current record pointer */
#define MF_CF_REC64		0x10			/* Use 64-bit record address */
	unsigned char	miscFlags;			/* misc flags */
	unsigned char	confFlags2;			/* configuration flags */
#define MF_CF2_EBCDIC		0x80		/* EBCDIC collation*/
#define MF_CF2_AFTER_ADV	0x40		/* has WRITE AFTER ADVANCING */
#define MF_CF2_BEFORE_ADV	0x20		/* has WRITE BEFORE ADVANCING */
#define MF_CF2_ADV		0x10			/* ADV Byte */
#define MF_CF2_IGN_MIN_LEN	0x08		/* ignore minimal lenght check (auto-fill?) */
	unsigned char	lockMode;			/* locking flags */
#define FCD_LOCK_MULTI		0x80
#define FCD_LOCK_WRITE		0x40
#define FCD_LOCK_RETRY_OPEN	0x20
#define FCD_LOCK_SKIP		0x10
#define FCD_LOCK_RETRY_LOCK	0x08
#define FCD_LOCK_MANU_LOCK	0x04
#define FCD_LOCK_AUTO_LOCK	0x02
#define FCD_LOCK_EXCL_LOCK	0x01
	unsigned char	fsv2Flags;			/* Fileshare V2 flags */
	unsigned char	idxCacheArea;		/* index cache buffers */
	unsigned char	fcdInternal1;
	unsigned char	fcdInternal2;
	char		res3[14];	
	unsigned char	gcFlags; 			/* was "res3"; Local GnuCOBOL feature only */
#define MF_CALLFH_GNUCOBOL	0x80			/* GnuCOBOL is being used */
#define MF_CALLFH_BYPASS	0x40			/* Stop passing this file to 'callfh' */
#define MF_CALLFH_TRACE		0x20			/* Trace I/O for this file */
#define MF_CALLFH_STATS		0x10			/* Record Stats for this file */
	unsigned char	nlsId[2];
	char		fsv2FileId[2];			/* Fileshare V2 file id */
	char		retryOpenCount[2];
	unsigned char	fnameLen[2];		/* file name length */
	unsigned char	idxNameLen[2];		/* index name length */
	char		retryCount[2];
	unsigned char	refKey[2];			/* key of reference */
	unsigned char	lineCount[2];	
	unsigned char	useFiles;	
	unsigned char	giveFiles;	
	unsigned char	effKeyLen[2];		/* effective key length */
	char		res5[14];		
	unsigned char	eop[2];				/* was "res5"; Use for cob_write eop value */
	char		opt[4];					/* was "res5"; Use for cob_write opts value */
	unsigned char	curRecLen[4];		/* current record length in bytes */
	unsigned char	minRecLen[4];		/* min. record length in bytes */
	unsigned char	maxRecLen[4];		/* max. record length in bytes */
	char		fsv2SessionId[4];		/* Fileshare V2 session id */
	char		res6[24];
	unsigned char	relByteAdrs[8];		/* 64-bit, relative byte address */
	unsigned char	maxRelKey[8];		/* 64-bit, max relative key/Record num */
	unsigned char	relKey[8];			/* 64-bit, (cur) relative key/Record num */
	pointer_8byte(void,	_fileHandle);		/* file handle */
	pointer_8byte(unsigned char, _recPtr);	/* pointer to record area */
	pointer_8byte(char,	_fnamePtr);			/* pointer to file name area */
	pointer_8byte(char,	_idxNamePtr);		/* pointer to index name area */
	pointer_8byte(KDB,	_kdbPtr);			/* pointer to key definition block */
	pointer_8byte(void,	_colPtr);			/* pointer to collating sequence block */
	pointer_8byte(void,	_fileDef);			/* pointer to filedef */
	pointer_8byte(void,	_dfSortPtr);		/* pointer to DFSORT */
} FCD3;

/*******************************************************/
/* Following is the 32-bit FCD (or also known as FCD2) */
/**** Newer versions of MF COBOL do not support this ***/
/*******************************************************/
typedef struct __fcd2 {
	unsigned char	fileStatus[2];		/* I/O completion status */
	unsigned char	fcdLen[2];			/* reserved */
	unsigned char	fcdVer;				/* reserved */
#define FCD2_VER	0
	unsigned char	fileOrg;			/* file organization */
	unsigned char	accessFlags;		/* status byte (bit 7) & file access flags (bits 0-6)*/
	unsigned char	openMode;			/* open mode INPUT, I-O, etc. */
	char			res2[2];			/* reserved */
	unsigned char	blockSize;			/* block-size, fcd-convert only */
	unsigned char	fnameLen[2];		/* file name length */
	unsigned char	relByteAdrs64[8];	/* 64-bit, relative byte address */
	char			res3[3];			/* reserved */
	unsigned char	lockMode;			/* lock mode flags for shareable files*/
	unsigned char	otherFlags;			/* miscellaneous flags */
	char			res4[2];			/* reserved */
	void			*fileHandle2;		/* file handle */
	unsigned char   gcFlags;			/* Flags used in 4.x by extfh.c */
	unsigned char	fstatusType;		/* file status type */
	unsigned char	fileFormat;			/* file format */
	char			res6[3];			/* reserved */
	unsigned char	maxRecLen[2];		/* max. record length in bytes */
	char			res7[3];			/* rserved */
	unsigned char	relRecNum[4];			/* relative record number */
	unsigned char	recordMode;			/* recording mode */
	unsigned char	curRecLen[2];		/* current record length in bytes */
	unsigned char	minRecLen[2];		/* min. record length in bytes */
	unsigned char	refKey[2];			/* key of reference */
	unsigned char 	effKeyLen[2];		/* effective key length */
	unsigned char	*recPtr2;			/* Pointer to record area */
	char			*fnamePtr2;			/* pointer to file name area */
	KDB				*kdbPtr2;			/* pointer to key definition block */
	char			res8[4];			/* reserved */
	unsigned char	relByteAddr[4];		/* relative byte address */
	char			res9[2];			/* reserved */
	unsigned char	compType;			/* data compression type */
	char			fsv2SessionId[4];	/* Fileshare V2 session id */
	char			fsv2FileId[2];		/* Fileshare V2 file id */
	unsigned char	maxRelKeySize[4];	/* Maximum relative Key size */
	char			res10[2];			/* reserved */
	unsigned char	lockFlags;			/* locking flags */
	unsigned char	fsv2Flags;			/* Fileshare V2 flags */
	unsigned char	confFlags;			/* configuration flags */
	char			res11;				/* reserved */
	unsigned char	confFlags2;			/* yet another bunch of flags */
	unsigned char	idxCacheSz;			/* index cache size */
	unsigned char	idxCacheArea;		/* index cache area */
	char			res12[2];			/* reserved */
} FCD2;

#define fileHandle	_fileHandle.ptr_name	/* EXTFH: file handle */
#define recPtr		_recPtr.ptr_name		/* EXTFH: pointer to record area */
#define fnamePtr	_fnamePtr.ptr_name		/* EXTFH: pointer to file name area */
#define idxNamePtr	_idxNamePtr.ptr_name	/* EXTFH: pointer to index name area */
#define kdbPtr		_kdbPtr.ptr_name 		/* EXTFH: pointer to key definition block */
#define colPtr		_colPtr.ptr_name		/* EXTFH: pointer to collating sequence block */
#define fileDef		_fileDef.ptr_name		/* EXTFH: pointer to filedef */
#define dfSortPtr	_dfSortPtr.ptr_name		/* EXTFH: pointer to DFSORT */

#define LSUCHAR(f)	((unsigned char*)(f))
/* xxCOMPXn : Big Endian Binary data */
#define LDCOMPX2(f)	((((f)[0] << 8 ) & 0xFF00) | ((f)[1] & 0xFF))
#define LDCOMPX4(f)	((((f)[0] << 24 ) & 0xFF000000) | (((f)[1] << 16 ) & 0xFF0000) | (((f)[2] << 8 ) & 0xFF00) | ((f)[3] & 0xFF))
#define STCOMPX2(v,f)	((f)[1] = (v) & 0xFF, (f)[0] = ((v) >> 8) & 0xFF)
#define STCOMPX4(v,f)	((f)[3] = (v) & 0xFF, (f)[2] = ((v) >> 8) & 0xFF, (f)[1] = ((v) >> 16) & 0xFF, (f)[0] = ((v) >> 24) & 0xFF)

/* xxBINLEn : Little Endian Binary data */
#define LDBINLE2(f)	((((f)[1] << 8 ) & 0xFF00) | ((f)[0] & 0xFF))
#define LDBINLE4(f)	((((f)[3] << 24 ) & 0xFF000000) | (((f)[2] << 16 ) & 0xFF0000) | (((f)[1] << 8 ) & 0xFF00) | ((f)[0] & 0xFF))
#define STBINLE2(v,f)	((f)[0] = (v) & 0xFF, (f)[1] = ((v) >> 8) & 0xFF)
#define STBINLE4(v,f)	((f)[0] = (v) & 0xFF, (f)[1] = ((v) >> 8) & 0xFF, (f)[2] = ((v) >> 16) & 0xFF, (f)[3] = ((v) >> 24) & 0xFF)

/*************************/
/* EXTFH operation codes */
/*************************/
#define OP_GETINFO			0x0006	
#define OP_CRE8_INDEX			0x0007	
#define OP_FLUSH			0x000C	
#define OP_UNLOCK_REC			0x000F	

#define OP_CLOSE			0xFA80		/* OP CODES */
#define OP_CLOSE_LOCK			0xFA81
#define OP_CLOSE_NO_REWIND		0xFA82
#define OP_CLOSE_REEL			0xFA84
#define OP_CLOSE_REMOVE			0xFA85
#define OP_CLOSE_NOREWIND		0xFA86

#define OP_OPEN_INPUT			0xFA00
#define OP_OPEN_OUTPUT			0xFA01
#define OP_OPEN_IO			0xFA02
#define OP_OPEN_EXTEND			0xFA03
#define OP_OPEN_INPUT_NOREWIND		0xFA04
#define OP_OPEN_OUTPUT_NOREWIND		0xFA05
#define OP_OPEN_INPUT_REVERSED		0xFA08

#define OP_READ_SEQ_NO_LOCK		0xFA8D
#define OP_READ_SEQ_LOCK		0xFAD8
#define OP_READ_SEQ_KEPT_LOCK		0xFAD9
#define OP_READ_SEQ			0xFAF5
#define OP_READ_PREV_NO_LOCK		0xFA8C
#define OP_READ_PREV_LOCK		0xFADE
#define OP_READ_PREV_KEPT_LOCK		0xFADF
#define OP_READ_PREV			0xFAF9
#define OP_READ_RAN_NO_LOCK		0xFA8E
#define OP_READ_RAN_LOCK		0xFADA
#define OP_READ_RAN_KEPT_LOCK		0xFADB
#define OP_READ_RAN			0xFAF6
#define OP_READ_DIR_NO_LOCK		0xFA8F
#define OP_READ_DIR_LOCK		0xFAD6
#define OP_READ_DIR_KEPT_LOCK		0xFAD7
#define OP_READ_DIR			0xFAC9
#define OP_READ_POSITION		0xFAF1

#define OP_WRITE_BEFORE			0xFAE1
#define OP_WRITE_BEFORE_TAB		0xFAE3
#define OP_WRITE_BEFORE_PAGE		0xFAE5
#define OP_WRITE_AFTER			0xFAE2
#define OP_WRITE_AFTER_TAB		0xFAE4
#define OP_WRITE_AFTER_PAGE		0xFAE6

#define OP_WRITE			0xFAF3
#define OP_REWRITE			0xFAF4

#define OP_START_EQ			0xFAE8
#define OP_START_EQ_ANY			0xFAE9
#define OP_START_GT			0xFAEA
#define OP_START_GE			0xFAEB
#define OP_START_LT			0xFAFE
#define OP_START_LE			0xFAFF
#define OP_START_LA			0xFAEC	/* LAST: Not in MF standard */
#define OP_START_FI			0xFAED	/* FIRST: Not in MF standard */

#define OP_STEP_NEXT_NO_LOCK		0xFA90
#define OP_STEP_NEXT_LOCK		0xFAD4
#define OP_STEP_NEXT_KEPT_LOCK		0xFAD5
#define OP_STEP_NEXT			0xFACA
#define OP_STEP_FIRST_NO_LOCK		0xFA92
#define OP_STEP_FIRST_LOCK		0xFAD0
#define OP_STEP_FIRST_KEPT_LOCK		0xFAD1
#define OP_STEP_FIRST			0xFACC

#define OP_DELETE			0xFAF7
#define OP_DELETE_FILE			0xFAF8
#define OP_UNLOCK			0xFA0E
#define OP_COMMIT			0xFADC
#define OP_ROLLBACK			0xFADD

/*******************************/
/* Functions in termio.c */

COB_EXPIMP void cob_display	(const int, const int, const int, ...);
COB_EXPIMP void cob_dump_output (const char *);
COB_EXPIMP void cob_dump_file (const char *, cob_file *);
COB_EXPIMP void cob_dump_field	(const int, const char *, cob_field *, const int, const int, ...);
COB_EXPIMP void cob_dump_field_ext	(const int, const char *, cob_field *, const cob_uli_t, const cob_u32_t, ...);
COB_EXPIMP void cob_accept	(cob_field *);

/*******************************/
/* Functions in fileio.c */

COB_EXPIMP void	cob_file_external_addr (const char *,
				 cob_file **, cob_file_key **,
				 const int nkeys, const int linage);
COB_EXPIMP void	cob_file_malloc (cob_file **, cob_file_key **,
				 const int nkeys, const int linage);
COB_EXPIMP void	cob_file_free   (cob_file **, cob_file_key **);

COB_EXPIMP int	cob_findkey (cob_file *, cob_field *, int *, int *);

COB_EXPIMP void cob_open	(cob_file *, const int, const int, cob_field *);
COB_EXPIMP void cob_close	(cob_file *, cob_field *, const int, const int);
COB_EXPIMP void cob_read	(cob_file *, cob_field *, cob_field *, const int);
COB_EXPIMP void cob_read_next	(cob_file *, cob_field *, const int);
COB_EXPIMP void cob_rewrite	(cob_file *, cob_field *, const int, cob_field *);
COB_EXPIMP void cob_delete	(cob_file *, cob_field *);
COB_EXPIMP void cob_start	(cob_file *, const int, cob_field *,
				 cob_field *, cob_field *);
COB_EXPIMP void cob_write	(cob_file *, cob_field *, const int,
				 cob_field *, const unsigned int);

COB_EXPIMP void cob_delete_file	(cob_file *, cob_field *);
COB_EXPIMP void cob_unlock_file	(cob_file *, cob_field *);
COB_EXPIMP void cob_commit	(void);
COB_EXPIMP void cob_rollback	(void);


/* functions in fileio.c for the MF style EXTFH interface */
COB_EXPIMP int	EXTFH		(unsigned char *opcode, FCD3 *fcd);

COB_EXPIMP void	cob_extfh_open		(int (*callfh)(unsigned char *opcode, FCD3 *fcd),
					cob_file *, const int, const int, cob_field *);
COB_EXPIMP void cob_extfh_close		(int (*callfh)(unsigned char *opcode, FCD3 *fcd),
					cob_file *, cob_field *, const int, const int);
COB_EXPIMP void cob_extfh_read		(int (*callfh)(unsigned char *opcode, FCD3 *fcd),
					cob_file *, cob_field *, cob_field *, const int);
COB_EXPIMP void cob_extfh_read_next	(int (*callfh)(unsigned char *opcode, FCD3 *fcd),
					cob_file *, cob_field *, const int);
COB_EXPIMP void cob_extfh_rewrite	(int (*callfh)(unsigned char *opcode, FCD3 *fcd),
					cob_file *, cob_field *, const int, cob_field *);
COB_EXPIMP void cob_extfh_delete	(int (*callfh)(unsigned char *opcode, FCD3 *fcd),
					cob_file *, cob_field *);
COB_EXPIMP void cob_extfh_start		(int (*callfh)(unsigned char *opcode, FCD3 *fcd),
					cob_file *, const int, cob_field *,
					cob_field *, cob_field *);
COB_EXPIMP void cob_extfh_write		(int (*callfh)(unsigned char *opcode, FCD3 *fcd),
					cob_file *, cob_field *, const int,
				 	cob_field *, const unsigned int);
COB_EXPIMP void cob_file_fcd_adrs		(cob_file *, void *);
COB_EXPIMP void cob_file_fcdkey_adrs	(cob_file *, void *);

/* File system routines */
COB_EXPIMP int cob_sys_open_file	(unsigned char *, unsigned char *,
					 unsigned char *, unsigned char *,
					 unsigned char *);
COB_EXPIMP int cob_sys_create_file	(unsigned char *, unsigned char *,
					 unsigned char *, unsigned char *,
					 unsigned char *);
COB_EXPIMP int cob_sys_read_file	(unsigned char *, unsigned char *,
					 unsigned char *, unsigned char *,
					 unsigned char *);
COB_EXPIMP int cob_sys_write_file	(unsigned char *, unsigned char *,
					 unsigned char *, unsigned char *,
					 unsigned char *);
COB_EXPIMP int cob_sys_close_file	(unsigned char *);
COB_EXPIMP int cob_sys_flush_file	(unsigned char *);
COB_EXPIMP int cob_sys_delete_file	(unsigned char *);
COB_EXPIMP int cob_sys_copy_file	(unsigned char *, unsigned char *);
COB_EXPIMP int cob_sys_check_file_exist	(unsigned char *, unsigned char *);
COB_EXPIMP int cob_sys_rename_file	(unsigned char *, unsigned char *);
COB_EXPIMP int cob_sys_get_current_dir	(const int, const int, unsigned char *);
COB_EXPIMP int cob_sys_change_dir	(unsigned char *);
COB_EXPIMP int cob_sys_create_dir	(unsigned char *);
COB_EXPIMP int cob_sys_delete_dir	(unsigned char *);
COB_EXPIMP int cob_sys_chdir		(unsigned char *, unsigned char *);
COB_EXPIMP int cob_sys_mkdir		(unsigned char *);
COB_EXPIMP int cob_sys_copyfile		(unsigned char *, unsigned char *,
					 unsigned char *);
COB_EXPIMP int cob_sys_file_info	(unsigned char *, unsigned char *);
COB_EXPIMP int cob_sys_file_delete	(unsigned char *, unsigned char *);

/* SORT routines */
COB_EXPIMP void	cob_file_sort_init	(cob_file *, const unsigned int,
					 const unsigned char *,
					 void *, cob_field *);
COB_EXPIMP void	cob_file_sort_init_key	(cob_file *, cob_field *,
					 const int, const unsigned int);
COB_EXPIMP void	cob_file_sort_close	(cob_file *);
COB_EXPIMP void	cob_file_sort_using	(cob_file *, cob_file *);
COB_EXPIMP void	cob_file_sort_giving	(cob_file *, const size_t, ...);
COB_EXPIMP void	cob_file_release	(cob_file *);
COB_EXPIMP void	cob_file_return		(cob_file *);

/***************************/
/* Functions in reportio.c */
/***************************/
COB_EXPIMP void cob_report_initiate	(cob_report *);
COB_EXPIMP int  cob_report_terminate	(cob_report *, int);
COB_EXPIMP int  cob_report_generate	(cob_report *, cob_report_line *, int);
COB_EXPIMP void cob_report_suppress	(cob_report *r, cob_report_line *l);

/**********************/
/* Functions in mlio.c */
/**********************/

COB_EXPIMP int	cob_is_valid_uri	(const char *);
COB_EXPIMP int	cob_is_xml_namestartchar	(const int);
COB_EXPIMP int	cob_is_xml_namechar	(const int);
COB_EXPIMP void	cob_xml_generate	(cob_field *, cob_ml_tree *,
					 cob_field *, const int, cob_field *,
					 cob_field *);
COB_EXPIMP void cob_json_generate	(cob_field *, cob_ml_tree *,
					 cob_field *);
COB_EXPIMP void	cob_xml_generate_new	(cob_field *, cob_ml_tree *,
					 cob_field *, const int, cob_field *,
					 cob_field *, const char);
COB_EXPIMP void cob_json_generate_new	(cob_field *, cob_ml_tree *,
					 cob_field *, const char);
COB_EXPIMP int	cob_xml_parse	(cob_field *, cob_field *,
					 cob_field *, const int, void **);


/****************************/
/* Functions in intrinsic.c */
/****************************/
COB_EXPIMP void		cob_put_indirect_field		(cob_field *);
COB_EXPIMP void		cob_get_indirect_field		(cob_field *);
COB_EXPIMP cob_field *cob_switch_value			(const int);
COB_EXPIMP cob_field *cob_intr_binop			(cob_field *, const int,
							 cob_field *);

COB_EXPIMP int cob_check_numval				(const cob_field *,
							 const cob_field *,
							 const int, const int);

COB_EXPIMP int cob_valid_date_format			(const char *);
COB_EXPIMP int cob_valid_datetime_format		(const char *, const char);
COB_EXPIMP int cob_valid_time_format			(const char *, const char);

COB_EXPIMP cob_field *cob_intr_current_date		(const int, const int);
COB_EXPIMP cob_field *cob_intr_when_compiled		(const int, const int,
							 cob_field *);
COB_EXPIMP cob_field *cob_intr_module_date		(void);
COB_EXPIMP cob_field *cob_intr_module_time		(void);
COB_EXPIMP cob_field *cob_intr_module_id		(void);
COB_EXPIMP cob_field *cob_intr_module_caller_id		(void);
COB_EXPIMP cob_field *cob_intr_module_source		(void);
COB_EXPIMP cob_field *cob_intr_module_formatted_date	(void);
COB_EXPIMP cob_field *cob_intr_module_path		(void);
COB_EXPIMP cob_field *cob_intr_exception_file		(void);
COB_EXPIMP cob_field *cob_intr_exception_location	(void);
COB_EXPIMP cob_field *cob_intr_exception_status		(void);
COB_EXPIMP cob_field *cob_intr_exception_statement	(void);
COB_EXPIMP cob_field *cob_intr_mon_decimal_point	(void);
COB_EXPIMP cob_field *cob_intr_num_decimal_point	(void);
COB_EXPIMP cob_field *cob_intr_mon_thousands_sep	(void);
COB_EXPIMP cob_field *cob_intr_num_thousands_sep	(void);
COB_EXPIMP cob_field *cob_intr_currency_symbol		(void);
COB_EXPIMP cob_field *cob_intr_char			(cob_field *);
COB_EXPIMP cob_field *cob_intr_ord			(cob_field *);
COB_EXPIMP cob_field *cob_intr_stored_char_length	(cob_field *);
COB_EXPIMP cob_field *cob_intr_combined_datetime	(cob_field *, cob_field *);
COB_EXPIMP cob_field *cob_intr_date_of_integer		(cob_field *);
COB_EXPIMP cob_field *cob_intr_day_of_integer		(cob_field *);
COB_EXPIMP cob_field *cob_intr_integer_of_date		(cob_field *);
COB_EXPIMP cob_field *cob_intr_integer_of_day		(cob_field *);
COB_EXPIMP cob_field *cob_intr_test_date_yyyymmdd	(cob_field *);
COB_EXPIMP cob_field *cob_intr_test_day_yyyyddd		(cob_field *);
COB_EXPIMP cob_field *cob_intr_test_numval		(cob_field *);
COB_EXPIMP cob_field *cob_intr_test_numval_c		(cob_field *, cob_field *);
COB_EXPIMP cob_field *cob_intr_test_numval_f		(cob_field *);
COB_EXPIMP cob_field *cob_intr_factorial		(cob_field *);

COB_EXPIMP cob_field *cob_intr_pi			(void);
COB_EXPIMP cob_field *cob_intr_e			(void);
COB_EXPIMP cob_field *cob_intr_exp			(cob_field *);
COB_EXPIMP cob_field *cob_intr_exp10			(cob_field *);
COB_EXPIMP cob_field *cob_intr_abs			(cob_field *);
COB_EXPIMP cob_field *cob_intr_acos			(cob_field *);
COB_EXPIMP cob_field *cob_intr_asin			(cob_field *);
COB_EXPIMP cob_field *cob_intr_atan			(cob_field *);
COB_EXPIMP cob_field *cob_intr_cos			(cob_field *);
COB_EXPIMP cob_field *cob_intr_log			(cob_field *);
COB_EXPIMP cob_field *cob_intr_log10			(cob_field *);
COB_EXPIMP cob_field *cob_intr_sin			(cob_field *);
COB_EXPIMP cob_field *cob_intr_sqrt			(cob_field *);
COB_EXPIMP cob_field *cob_intr_tan			(cob_field *);

COB_EXPIMP cob_field *cob_intr_upper_case		(const int, const int,
							 cob_field *);
COB_EXPIMP cob_field *cob_intr_lower_case		(const int, const int,
							 cob_field *);
COB_EXPIMP cob_field *cob_intr_reverse			(const int, const int,
							 cob_field *);
COB_EXPIMP cob_field *cob_intr_concatenate		(const int, const int,
							 const int, ...);
COB_EXPIMP cob_field *cob_intr_substitute		(const int, const int,
							 const int, ...);
COB_EXPIMP cob_field *cob_intr_substitute_case		(const int, const int,
							 const int, ...);
COB_EXPIMP cob_field *cob_intr_trim			(const int, const int,
							 cob_field *, const int);
COB_EXPIMP cob_field *cob_intr_length			(cob_field *);
COB_EXPIMP cob_field *cob_intr_byte_length		(cob_field *);
COB_EXPIMP cob_field *cob_intr_integer			(cob_field *);
COB_EXPIMP cob_field *cob_intr_integer_part		(cob_field *);
COB_EXPIMP cob_field *cob_intr_fraction_part		(cob_field *);
COB_EXPIMP cob_field *cob_intr_sign			(cob_field *);
COB_EXPIMP cob_field *cob_intr_lowest_algebraic		(cob_field *);
COB_EXPIMP cob_field *cob_intr_highest_algebraic	(cob_field *);
COB_EXPIMP cob_field *cob_intr_numval			(cob_field *);
COB_EXPIMP cob_field *cob_intr_numval_c			(cob_field *, cob_field *);
COB_EXPIMP cob_field *cob_intr_numval_f			(cob_field *);
COB_EXPIMP cob_field *cob_intr_annuity			(cob_field *, cob_field *);
COB_EXPIMP cob_field *cob_intr_mod			(cob_field *, cob_field *);
COB_EXPIMP cob_field *cob_intr_rem			(cob_field *, cob_field *);
COB_EXPIMP cob_field *cob_intr_sum			(const int, ...);
COB_EXPIMP cob_field *cob_intr_ord_min			(const int, ...);
COB_EXPIMP cob_field *cob_intr_ord_max			(const int, ...);
COB_EXPIMP cob_field *cob_intr_min			(const int, ...);
COB_EXPIMP cob_field *cob_intr_max			(const int, ...);
COB_EXPIMP cob_field *cob_intr_midrange			(const int, ...);
COB_EXPIMP cob_field *cob_intr_median			(const int, ...);
COB_EXPIMP cob_field *cob_intr_mean			(const int, ...);
COB_EXPIMP cob_field *cob_intr_range			(const int, ...);
COB_EXPIMP cob_field *cob_intr_random			(const int, ...);
COB_EXPIMP cob_field *cob_intr_variance			(const int, ...);
COB_EXPIMP cob_field *cob_intr_standard_deviation	(const int, ...);
COB_EXPIMP cob_field *cob_intr_present_value		(const int, ...);
COB_EXPIMP cob_field *cob_intr_year_to_yyyy		(const int, ...);
COB_EXPIMP cob_field *cob_intr_date_to_yyyymmdd		(const int, ...);
COB_EXPIMP cob_field *cob_intr_day_to_yyyyddd		(const int, ...);
COB_EXPIMP cob_field *cob_intr_locale_compare		(const int, ...);
COB_EXPIMP cob_field *cob_intr_locale_date		(const int, const int,
							 cob_field *, cob_field *);
COB_EXPIMP cob_field *cob_intr_locale_time		(const int, const int,
							 cob_field *, cob_field *);

COB_EXPIMP cob_field *cob_intr_seconds_past_midnight	(void);
COB_EXPIMP cob_field *cob_intr_lcl_time_from_secs	(const int, const int,
							 cob_field *, cob_field *);

COB_EXPIMP cob_field *cob_intr_seconds_from_formatted_time	(cob_field *,
								 cob_field *);

COB_EXPIMP cob_field *cob_intr_boolean_of_integer	(cob_field *, cob_field *);
COB_EXPIMP cob_field *cob_intr_char_national		(cob_field *);
COB_EXPIMP cob_field *cob_intr_display_of		(const int, const int,
							 const int, ...);
COB_EXPIMP cob_field *cob_intr_exception_file_n		(void);
COB_EXPIMP cob_field *cob_intr_exception_location_n	(void);
COB_EXPIMP cob_field *cob_intr_formatted_current_date	(const int, const int,
							 cob_field *);
COB_EXPIMP cob_field *cob_intr_formatted_date		(const int, const int,
							 cob_field *, cob_field *);
COB_EXPIMP cob_field *cob_intr_formatted_datetime	(const int, const int,
							 const int, ...);
COB_EXPIMP cob_field *cob_intr_formatted_time		(const int, const int,
							 const int, ...);
COB_EXPIMP cob_field *cob_intr_integer_of_boolean	(cob_field *);
COB_EXPIMP cob_field *cob_intr_national_of		(const int, const int,
							 const int, ...);
COB_EXPIMP cob_field *cob_intr_standard_compare		(const int, ...);
COB_EXPIMP cob_field *cob_intr_test_formatted_datetime	(cob_field *, cob_field *);

COB_EXPIMP cob_field *cob_intr_integer_of_formatted_date	(cob_field *,
								 cob_field *);
COB_EXPIMP cob_field *cob_intr_content_length		(cob_field *);
COB_EXPIMP cob_field *cob_intr_content_of		(const int, const int,
							 const int, ...);
COB_EXPIMP cob_field *cob_intr_bit_of		(cob_field *);
COB_EXPIMP cob_field *cob_intr_bit_to_char		(cob_field *);
COB_EXPIMP cob_field* cob_intr_hex_of (cob_field*);
COB_EXPIMP cob_field* cob_intr_hex_to_char (cob_field*);

/*******************************/

/*******************************/
/* defines for MicroFocus C -> COBOL API */
typedef	char *		cobchar_t;
#define	cobs8_t		cob_s8_t
#define	cobuns8_t	cob_u8_t
#define	cobs16_t	cob_s16_t
#define	cobuns16_t	cob_u16_t
#define	cobs32_t	cob_s32_t
#define	cobuns32_t	cob_u32_t
#define	cobs64_t	cob_s64_t
#define	cobuns64_t	cob_u64_t

#define	cobsetjmp(x)	setjmp (cob_savenv (x))
#define	coblongjmp(x)	cob_longjmp (x)
#define	cobsavenv(x)	cob_savenv (x)
#define	cobsavenv2(x,z)	cob_savenv2 (x, z)

#define	cobfunc(x,y,z)	cob_func (x, y, (void **)z)
#define	cobcall(x,y,z)	cob_call (x, y, (void **)z)
#define	cobcancel(x)	cob_cancel (x)

#define	cobgetenv(x)	cob_getenv (x)
#define	cobputenv(x)	cob_putenv (x)
#define cobrescanenv()	0 	/* not necessary as GnuCOBOL always reads the process environment */
#define	cobtidy()	cob_tidy ()
#define	cobinit()	cob_extern_init ()
#define	cobexit(x)	cob_stop_run (x)
#define	cobcommandline(v,w,x,y,z)	cob_command_line (v,w,x,y,z)

#define cobclear()	(void) cob_sys_clear_screen ()
#define cobmove(y,x)	cob_set_cursor_pos (y, x)
#define	cobcols()	cob_get_scr_cols ()
#define	coblines()	cob_get_scr_lines ()
#define cobaddstrc(x)	cob_display_text (x) 		/* no limit [MF=255] */
#define cobprintf	cob_display_formatted_text	/* limit of 2047 [MF=255] */
#define cobgetch()	cob_get_char ()

#define cobget_x1_compx(d)	(cobuns8_t) 	cob_get_u64_compx(d, 1)
#define cobget_x2_compx(d)	(cobuns16_t)	cob_get_u64_compx(d, 2)
#define cobget_x4_compx(d)	(cobuns32_t)	cob_get_u64_compx(d, 4)
#define cobget_x8_compx(d)	(cobuns64_t)	cob_get_u64_compx(d, 8)
#define cobget_sx1_compx(d)	(cobs8_t) 	cob_get_s64_compx(d, 1)
#define cobget_sx2_compx(d)	(cobs16_t)	cob_get_s64_compx(d, 2)
#define cobget_sx4_compx(d)	(cobs32_t)	cob_get_s64_compx(d, 4)
#define cobget_sx8_compx(d)	(cobs64_t)	cob_get_s64_compx(d, 8)
#define cobget_x1_comp5(d)	(cobuns8_t) 	cob_get_u64_comp5(d, 1)
#define cobget_x2_comp5(d)	(cobuns16_t)	cob_get_u64_comp5(d, 2)
#define cobget_x4_comp5(d)	(cobuns32_t)	cob_get_u64_comp5(d, 4)
#define cobget_x8_comp5(d)	(cobuns64_t)	cob_get_u64_comp5(d, 8)
#define cobget_sx1_comp5(d)	(cobs8_t) 	cob_get_s64_comp5(d, 1)
#define cobget_sx2_comp5(d)	(cobs16_t)	cob_get_s64_comp5(d, 2)
#define cobget_sx4_comp5(d)	(cobs32_t)	cob_get_s64_comp5(d, 4)
#define cobget_sx8_comp5(d)	(cobs64_t)	cob_get_s64_comp5(d, 8)
#define cobget_xn_comp5(d,n)	(cobuns64_t)	cob_get_u64_comp5(d, n)
#define cobget_xn_compx(d,n)	(cobuns64_t)	cob_get_u64_compx(d, n)
#define cobget_sxn_comp5(d,n)	(cobs64_t)	cob_get_s64_comp5(d, n)
#define cobget_sxn_compx(d,n)	(cobs64_t)	cob_get_s64_compx(d, n)

#define cobput_x1_compx(d,v)	(void)	cob_put_u64_compx((cob_u64_t)v,d,1)
#define cobput_x2_compx(d,v)	(void)	cob_put_u64_compx((cob_u64_t)v,d,2)
#define cobput_x4_compx(d,v)	(void)	cob_put_u64_compx((cob_u64_t)v,d,4)
#define cobput_x8_compx(d,v)	(void)	cob_put_u64_compx((cob_u64_t)v,d,8)
#define cobput_x1_comp5(d,v)	(void)	cob_put_u64_comp5((cob_u64_t)v,d,1)
#define cobput_x2_comp5(d,v)	(void)	cob_put_u64_comp5((cob_u64_t)v,d,2)
#define cobput_x4_comp5(d,v)	(void)	cob_put_u64_comp5((cob_u64_t)v,d,4)
#define cobput_x8_comp5(d,v)	(void)	cob_put_u64_comp5((cob_u64_t)v,d,8)
#define cobput_sx1_comp5(d,v)	(void)	cob_put_s64_comp5((cob_s64_t)v,d,1)
#define cobput_sx2_comp5(d,v)	(void)	cob_put_s64_comp5((cob_s64_t)v,d,2)
#define cobput_sx4_comp5(d,v)	(void)	cob_put_s64_comp5((cob_s64_t)v,d,4)
#define cobput_sx8_comp5(d,v)	(void)	cob_put_s64_comp5((cob_s64_t)v,d,8)
#define cobput_xn_comp5(d,n,v)	(void)	cob_put_u64_comp5(v, d, n)
#define cobput_xn_compx(d,n,v)	(void)	cob_put_u64_compx(v, d, n)
#define cobput_sxn_comp5(d,n,v)	(void)	cob_put_s64_comp5(v, d, n)
#define cobput_sxn_compx(d,n,v)	(void)	cob_put_s64_compx(v, d, n)

/*******************************/

#endif	/* COB_COMMON_H */

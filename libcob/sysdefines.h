/*
   Copyright (C) 2002-2012, 2014-2020 Free Software Foundation, Inc.
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

#ifndef COB_SYSDEFINE_H
#define COB_SYSDEFINE_H

/*
* This header is the place to test for the platform
* and/or C compiler then set required #define values
*/

/* Stringify macros */
#define CB_STRINGIFY(s)			#s
#define CB_XSTRINGIFY(s)		CB_STRINGIFY(s)
#define CB_XRANGE(min,max)              CB_XSTRINGIFY(min) ".." CB_XSTRINGIFY(max)  

/* C version info */
#ifdef	__VERSION__
#if	! defined (_MSC_VER)
#if	defined (__MINGW32__)
#define GC_C_VERSION_PRF	"(MinGW) "
#elif	defined (__DJGPP__)
#define GC_C_VERSION_PRF	"(DJGPP) "
#elif	defined (__ORANGEC__)
#define GC_C_VERSION_PRF	"(OrangeC) "
#else
#define GC_C_VERSION_PRF	""
#endif
#elif	defined (__c2__)
#define GC_C_VERSION_PRF	"(Microsoft C2) "
#elif	defined (__llvm__)
#define GC_C_VERSION_PRF	"(LLVM / MSC) "
#else
#define GC_C_VERSION_PRF	"(Microsoft) "
#endif
#define GC_C_VERSION	CB_XSTRINGIFY(__VERSION__)

#elif	defined(__xlc__)
#define GC_C_VERSION_PRF	"(IBM XL C/C++) "
#define GC_C_VERSION	CB_XSTRINGIFY(__xlc__)

#elif	defined(__SUNPRO_C)
#define GC_C_VERSION_PRF	"(Sun C) "
#define GC_C_VERSION	CB_XSTRINGIFY(__SUNPRO_C)

#elif	defined(_MSC_VER)
#define GC_C_VERSION_PRF	"(Microsoft) "
#define GC_C_VERSION	CB_XSTRINGIFY(_MSC_VER)

#elif	defined(__BORLANDC__)
#define GC_C_VERSION_PRF	"(Borland) "
#define GC_C_VERSION	CB_XSTRINGIFY(__BORLANDC__)

#elif	defined(__WATCOMC__)
#define GC_C_VERSION_PRF	"(Watcom) "
#define GC_C_VERSION	CB_XSTRINGIFY(__WATCOMC__)

#elif	defined(__INTEL_COMPILER)
#define GC_C_VERSION_PRF	"(Intel) "
#define GC_C_VERSION	CB_XSTRINGIFY(__INTEL_COMPILER)

#elif	defined(__TINYC__)
#define GC_C_VERSION_PRF	"(Tiny C) "
#define GC_C_VERSION	CB_XSTRINGIFY(__TINYC__)

#elif  defined(__HP_cc)
#define GC_C_VERSION_PRF       "(HP aC++/ANSI C) "
#define GC_C_VERSION   CB_XSTRINGIFY(__HP_cc) 

#elif  defined(__hpux) || defined(_HPUX_SOURCE)
#if  defined(__ia64)
#define GC_C_VERSION_PRF       "(HPUX IA64) "
#else
#define GC_C_VERSION_PRF       "(HPUX PA-RISC) "
#endif
#define GC_C_VERSION   " C"  

#else
#define GC_C_VERSION_PRF	""
#define GC_C_VERSION	        "unknown"
#endif


/* C compiler optimization flags */
#ifdef	_MSC_VER
#define	CB_COPT_0	" /Od"
#define	CB_COPT_1	" /O1"
#define	CB_COPT_2	" /O2"
#define	CB_COPT_3	" /Ox"
#define	CB_COPT_S	" /Os"

#elif   defined(__BORLANDC__)
#define	CB_COPT_0	" -O"
#define	CB_COPT_1	" -O"	/* optimize jumps only*/
#define	CB_COPT_2	" -O2"	/* optimize for speed */
#define	CB_COPT_3	" -O2"	/* CHECKME: is -O03 available? */
#define	CB_COPT_S	" -O1"	/* optimize for size */

#elif defined(__hpux) && !defined(__GNUC__)
#define	CB_COPT_0	" +O0"
#define	CB_COPT_1	" +O1"
#define	CB_COPT_2	" +O2"
#define	CB_COPT_3	" +O3"
#define	CB_COPT_S	" +Osize" /* CHECKME: may not available on old versions */

#elif   defined(__WATCOMC__)
#define	CB_COPT_0	" -od"
#define	CB_COPT_1	" -ot"
#define	CB_COPT_2	" -ox"
#define	CB_COPT_3	" -ox -oh"
#define	CB_COPT_S	" -os"

#elif   defined(__ORANGEC__)
#define	CB_COPT_0	" -O-"
#define	CB_COPT_1	""
#define	CB_COPT_2	""
#define	CB_COPT_3	""
#define	CB_COPT_S	""

#elif   defined(__SUNPRO_C)
#define	CB_COPT_0	" -xO1"	/* CHECKME: is -xO0 available? */
#define	CB_COPT_1	" -xO1"
#define	CB_COPT_2	" -xO2"
#define	CB_COPT_3	" -xO2"	/* CHECKME: Oracle docs are confusing, is -xO3 working? */
#define	CB_COPT_S	" -xO1 -xspace"

#elif	defined(__xlc__)
#define	CB_COPT_0	" -O0"
#define	CB_COPT_1	" -O"
#define	CB_COPT_2	" -O2"
#define	CB_COPT_3	" -O3"
#define	CB_COPT_S	" -O"

#else
#define	CB_COPT_0	" -O0"
#define	CB_COPT_1	" -O"
#define	CB_COPT_2	" -O2"
#define	CB_COPT_3	" -O3"
#define	CB_COPT_S	" -Os"
#endif

/* Define how C compiler aligns data */
#ifdef	HAVE_ATTRIBUTE_ALIGNED
#define COB_ALIGN " __attribute__((aligned))"
#define COB_ALIGN_KNOWN

#else

#if defined(_WIN32)
#define COB_ALIGN_ATTR_8 ""
#define COB_ALIGN_DECL_8 "__declspec(align(8)) "
#define COB_ALIGN_KNOWN

#elif defined(__arm__)
#define COB_ALIGN_ATTR_8 " __align(8)"
#define COB_ALIGN_DECL_8 ""
#define COB_ALIGN_KNOWN

#elif defined(__SUNPRO_C)
/* Insert #pragma align 8 (varname) */
#define COB_ALIGN_PRAGMA_8
#define COB_ALIGN_ATTR_8 ""
#define COB_ALIGN_DECL_8 ""
#define COB_ALIGN_KNOWN

#else
#define COB_ALIGN_ATTR_8 ""
#define COB_ALIGN_DECL_8 ""
#endif

#endif

#if defined (COB_NON_ALIGNED)
	/* allow explicit check of generated code 
	 * and to skip this part in checks of undefined behavior */
	/* Some DEC Alphas can only load shorts at 4-byte aligned addresses */
	#ifdef	__alpha
		#define COB_SHORT_BORK
	#endif
	#define COB_NO_UNALIGNED_ATTRIBUTE

#elif !defined(__i386__) && !defined(__x86_64__) && !defined(__powerpc__) && !defined(__powerpc64__) && !defined(__ppc__) && !defined(__amd64__) && !defined(__s390__)
	#define	COB_NON_ALIGNED
	/* Some DEC Alphas can only load shorts at 4-byte aligned addresses */
	#ifdef	__alpha
		#define COB_SHORT_BORK
	#endif
	#if defined(_MSC_VER)
		#define COB_ALLOW_UNALIGNED
		#define COB_NO_UNALIGNED_ATTRIBUTE
	#endif
#else
	#if defined(__i386__) || defined(__x86_64__) || defined(__powerpc__)
		#define COB_ALLOW_UNALIGNED
	#elif !defined(__hpux) && !defined(_HPUX_SOURCE) && !defined(__SUNPRO_C) && !defined(__s390__)
		#define COB_ALLOW_UNALIGNED
	#endif
	#if !defined(__powerpc__)
	#define COB_NO_UNALIGNED_ATTRIBUTE
	#endif
#endif

/* Max size of a single 'static char' allowed by C compiler */
#if defined(__GNUC__)
#define COB_MAX_CHAR_SIZE 2000000000
#else
#define COB_MAX_CHAR_SIZE 2000000
#endif
/* Define filename & path charcteristics */
#if	defined(_MSC_VER) || defined(__ORANGEC__) || defined(__WATCOMC__) || \
    defined(__BORLANDC__) || defined(__MINGW32__) || defined (__DJGPP__)
#define PATHSEP_CHAR (char) ';'
#define PATHSEP_STR (char *) ";"
#else
#define PATHSEP_CHAR (char) ':'
#define PATHSEP_STR (char *) ":"
#endif

#ifndef	_WIN32 /* note: needs to be \ for MinGW, needed for cobc -j */
#define SLASH_CHAR	(char) '/'
#define SLASH_STR	(char *) "/"
#else
#define SLASH_CHAR	(char) '\\'
#define SLASH_STR	(char *) "\\"
#endif

#ifdef __DJGPP__
#define HAVE_8DOT3_FILENAMES
#endif

/*
 * Mapping of COBOL Numeric to an SQL DATE
 *   (used in cobc/sqlxfdgen.c and libcob/fsqlxfd.c)
 */
struct sql_date {
	unsigned char	digits;		/* Total number of digits */
	unsigned char	hasDate;	/* Some part of YYMMDD is present */
	unsigned char	hasTime;	/* Some part of HHMISS is present */
	char			yyRule;		/* Rule code for adjusting Year */
								/* '+': real year := yy + yyAdj */
								/* '%': real year := yy pivot value yyAdj */
	short 			yyAdj;		/* Value to adjust year by */
	unsigned char	ccLen;		/* length of Century */
	unsigned char	ccPos;		/* position of Century */
	unsigned char	yyLen;		/* length of Year */
	unsigned char	yyPos;		/* position of year */
	unsigned char	mmLen;		/* length of Month */
	unsigned char	mmPos;		/* position of Month */
	unsigned char	ddLen;		/* length of Day */
	unsigned char	ddPos;		/* position of Day */
	unsigned char	hhLen;		/* length of Hour */
	unsigned char	hhPos;		/* position of Hour */
	unsigned char	miLen;		/* length of Minute */
	unsigned char	miPos;		/* position of Minute */
	unsigned char	ssLen;		/* length of Seconds */
	unsigned char	ssPos;		/* position of Seconds */
	unsigned char	huLen;		/* length of Hundredths of Second */
	unsigned char	huPos;		/* position of Hundredths of Second */
	char			format[32];	/* Date format string; Used for date conversion */
};

/* End compiler stuff */

/* EBCDIC determination */

#if	' ' == 0x40
#define	COB_EBCDIC_MACHINE
#elif defined(COB_EBCDIC_MACHINE)
#undef	COB_EBCDIC_MACHINE
#endif

#if defined (HAVE_NCURSESW_NCURSES_H) \
	|| defined (HAVE_NCURSESW_CURSES_H) \
	|| defined (HAVE_NCURSES_H) \
	|| defined (HAVE_NCURSES_NCURSES_H) \
	|| defined (HAVE_PDCURSES_H) \
	|| defined (HAVE_CURSES_H)
#define WITH_EXTENDED_SCREENIO
#endif

/* End of  COB_SYSDEFINE_H */
#endif

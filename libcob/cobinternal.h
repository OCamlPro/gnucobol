/*
   Copyright (C) 2007-2012, 2014-2022 Free Software Foundation, Inc.
   Written by Roger While, Simon Sobisch, Ron Norman

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


/* 
 * This header is for functions which exist in 'libcob', may be used by 'cobc'
 * and which are not exposed outside of the compiler
 */

#ifndef COB_INTERNAL_H
#define COB_INTERNAL_H

#ifndef HAVE_ATOLL
#ifdef  HAVE_STRTOLL
#ifndef atoll
#define atoll(x) strtoll(x, NULL, 10)
#endif
#endif
#endif

#ifndef HAVE_ATOL
#ifdef  HAVE_STRTOL
#ifndef atol
#define atol(x) strtol(x, NULL, 10)
#endif
#endif
#endif        

#if	defined(_WIN32) || defined(__CYGWIN__) || defined(COB_NO_VISIBILITY_ATTRIBUTE)
#define COB_HIDDEN	extern
#elif	defined(__GNUC__) && \
	(__GNUC__ > 4 ||  /* note: this check should be moved to configure... */ \
	 (__GNUC__ == 4 && __GNUC_MINOR__ > 2))
/* Also OK for icc which defines __GNUC__ */
#define COB_HIDDEN	extern __attribute__ ((visibility("hidden")))
#elif	defined(__SUNPRO_C) && (__SUNPRO_C >= 0x550)
/* Note - >= 0x590 supports gcc syntax */
#define COB_HIDDEN	extern __hidden
#else
#define COB_HIDDEN	extern
#endif/* Readable compiler version defines */

#if defined(_MSC_VER)

/*
_MSC_VER == 1400 (Visual Studio 2005, VS8 , MSVC 8) since OS-Version 2000
_MSC_VER == 1500 (Visual Studio 2008, VS9 , MSVC 9) since OS-Version XP / 2003
_MSC_VER == 1600 (Visual Studio 2010, VS10, MSVC10) since OS-Version XP / 2003
_MSC_VER == 1700 (Visual Studio 2012, VS11, MSVC11) since OS-Version 7(XP) / 2008 R2(2003)
_MSC_VER == 1800 (Visual Studio 2013, VS12, MSVC12) since OS-Version 7(XP) / 2008 R2(2003)
_MSC_VER == 1900 (Visual Studio 2015, VS14, MSVC14) since OS-Version 7(XP) / 2008 R2(2003)
_MSC_VER == 1910 (Visual Studio 2017, VS15, MSVC14.1) since OS-Version 7 / 2012 R2
_MSC_VER == 1920 (Visual Studio 2019, VS16, MSVC14.2) since OS-Version 7 / 2012 R2

Note: also defined together with __clang__ in both frontends:
   __llvm__ Clang LLVM frontend for Visual Studio by LLVM Project (via clang-cl.exe [cl build options])
   __c2__   Clang C2 frontend with MS CodeGen (via clang.exe [original clang build options])
*/

#if _MSC_VER >= 1500
#define COB_USE_VC2008_OR_GREATER 1
#else
#define COB_USE_VC2008_OR_GREATER 0
#if _MSC_VER < 1500
#error Support for Visual Studio 2005 and older Visual C++ compilers dropped with GnuCOBOL 4.0
#endif
#endif

#if _MSC_VER >= 1700
#define COB_USE_VC2012_OR_GREATER 1
#else
#define COB_USE_VC2012_OR_GREATER 0
#endif

#if _MSC_VER >= 1800
#define COB_USE_VC2013_OR_GREATER 1
#else
#define COB_USE_VC2013_OR_GREATER 0
#endif

#if _MSC_VER >= 1900
#define COB_USE_VC2015_OR_GREATER 1
#else
#define COB_USE_VC2015_OR_GREATER 0
#endif

#endif /* _MSC_VER */

/* COB_DEBUG_LOG Macros and routines found in common.c */
COB_EXPIMP int	cob_debug_logit		(int level, char *module);
COB_EXPIMP int	cob_debug_logger	(const char *fmt, ... );
COB_EXPIMP int	cob_debug_dump		(void *mem, int len);
#ifdef COB_DEBUG_LOG
#define DEBUG_TRACE(module, arglist)		cob_debug_logit(3, (char*)module) ? 0 : cob_debug_logger arglist
#define DEBUG_WARN(module, arglist)			cob_debug_logit(2, (char*)module) ? 0 : cob_debug_logger arglist
#define DEBUG_LOG(module, arglist)			cob_debug_logit(0, (char*)module) ? 0 : cob_debug_logger arglist
#define DEBUG_DUMP_TRACE(module, mem, len)	cob_debug_logit(3, (char*)module) ? 0 : cob_debug_dump(mem, len)
#define DEBUG_DUMP_WARN(module, mem, len)	cob_debug_logit(2, (char*)module) ? 0 : cob_debug_dump(mem, len)
#define DEBUG_DUMP(module, mem, len)		cob_debug_logit(0, (char*)module) ? 0 : cob_debug_dump(mem, len)
#define DEBUG_ISON_TRACE(module)			!cob_debug_logit(3, (char*)module)
#define DEBUG_ISON_WARN(module)				!cob_debug_logit(2, (char*)module)
#define DEBUG_ISON(module)					!cob_debug_logit(0, (char*)module)
#else
#define DEBUG_TRACE(module, arglist)
#define DEBUG_WARN(module, arglist)
#define DEBUG_LOG(module, arglist)
#define DEBUG_DUMP_TRACE(module, mem, len)
#define DEBUG_DUMP_WARN(module, mem, len)
#define DEBUG_DUMP(module, mem, len)
/* Note: no definition for DEBUG_ISON_TRACE, DEBUG_ISON_WARN, DEBUG_ISON
         as these parts should be surrounded by #ifdef COB_DEBUG_LOG */
#endif

#endif	/* COB_LOCAL_H */

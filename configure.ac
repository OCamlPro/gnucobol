#
# Configure template for GnuCOBOL C++
# Process this file with autoconf to produce a configure script.
#
# Copyright (C) 2001-2012, 2014-2017 Free Software Foundation, Inc.
# Written by Keisuke Nishida, Roger While, Ron Norman, Simon Sobisch, Sergey Kashyrin
#
# This file is part of GnuCOBOL.
#
# The GnuCOBOL C++ compiler is free software: you can redistribute it
# and/or modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# GnuCOBOL C++ is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GnuCOBOL C++.  If not, see <http://www.gnu.org/licenses/>.

AC_PREREQ([2.64])

AC_INIT([GnuCOBOL C++],
	[2.2],
	[bug-gnucobol@gnu.org],
	[gnucobolcpp],
	[https://www.gnu.org/software/gnucobol/])
AC_CONFIG_SRCDIR([libcob.h.in])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_TESTDIR([tests])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([build_aux])
AC_LANG([C++])

AC_PROG_CXX

# Note for SUN Solaris (gcc)
# export/setenv CXXFLAGS "-m64 -mptr64"
# export/setenv LDFLAGS "-m64 -mptr64 -L/usr/local/lib/sparcv9"
# ./configure --libdir=/usr/local/lib/sparcv9
#
# Hack for AIX 64 bit (gcc)
# Required -
# export/setenv CXXFLAGS=-maix64
# export/setenv LDFLAGS=-maix64

if echo $CXXFLAGS | grep 'aix64' 1>/dev/null 2>&1
then
	if test -f /usr/ccs/bin/ar
	then
		AR="/usr/ccs/bin/ar -X64"
	else
		AR="ar -X64"
	fi
	NM="/usr/ccs/bin/nm -X64 -B"
fi

AM_INIT_AUTOMAKE([gnu no-texinfo.tex])
AM_MAINTAINER_MODE

# Autoheader templates
AH_TEMPLATE([COB_EXPERIMENTAL], [Enable experimental code (Developers only!)])
AH_TEMPLATE([COB_PARAM_CHECK], [Enable minimum parameter check for system libraries])
AH_TEMPLATE([PATCH_LEVEL], [Define a patch level (numeric, max. 8 digits)])
AH_TEMPLATE([MAX_CALL_FIELD_PARAMS], [Define maximum parameters for CALL])
AH_TEMPLATE([WITH_INDEX_EXTFH], [Compile with an external ISAM handler])
AH_TEMPLATE([WITH_SEQRA_EXTFH], [Compile with an external SEQ/RAN handler])
AH_TEMPLATE([WITH_DB], [Use Berkeley DB library as emulation of ISAM handler])
AH_TEMPLATE([WITH_CISAM], [Use CISAM as ISAM handler])
AH_TEMPLATE([WITH_DISAM], [Use DISAM as ISAM handler])
AH_TEMPLATE([WITH_VBISAM], [Use VBISAM as ISAM handler])
AH_TEMPLATE([WITH_MPIR], [Use MPIR instead of GMP])
AH_TEMPLATE([COB_EXPORT_DYN], [Compile/link option for exporting symbols])
AH_TEMPLATE([COB_PIC_FLAGS], [Compile/link option for PIC code])
AH_TEMPLATE([COB_SHARED_OPT], [Compile/link option for shared code])
AH_TEMPLATE([COB_STRIP_CMD], [Strip command])
AH_TEMPLATE([USE_LIBDL], [Use system dynamic loader])
AH_TEMPLATE([HAVE_DLADDR], [Has dladdr function])
AH_TEMPLATE([WITH_VARSEQ], [Define variable sequential file format])
AH_TEMPLATE([HAVE_ATTRIBUTE_ALIGNED], [Has __attribute__((aligned))])
AH_TEMPLATE([HAVE_TIMEZONE], [Has timezone variable])
AH_TEMPLATE([COB_OBJECT_EXT], [Object extension])
AH_TEMPLATE([COB_MODULE_EXT], [Module extension])
AH_TEMPLATE([COB_EXE_EXT], [Executable extension])
AH_TEMPLATE([COB_KEYWORD_INLINE], [Keyword for inline])
AH_TEMPLATE([COB_NO_SELFOPEN], [Can not dlopen self])
AH_TEMPLATE([COB_STRFTIME], [Can use strftime for timezone])
AH_TEMPLATE([COB_LI_IS_LL], [long int is long long])
AH_TEMPLATE([COB_32_BIT_LONG], [long int is 32 bits])
AH_TEMPLATE([COB_64_BIT_POINTER], [Pointers are longer than 32 bits])
AH_TEMPLATE([WITH_CURSES], [curses library for extended SCREEN I/O])
AH_TEMPLATE([HAVE_COLOR_SET], [curses has color_set function])
AH_TEMPLATE([HAVE_CURSES_FREEALL], [ncurses has _nc_freeall function])
AH_TEMPLATE([HAVE_USE_LEGACY_CODING], [ncurses has use_legacy_coding function])
AH_TEMPLATE([HAVE_NANO_SLEEP], [Has nanosleep function])
AH_TEMPLATE([HAVE_CLOCK_GETTIME], [Has clock_gettime function and CLOCK_REALTIME])
AH_TEMPLATE([HAVE_ISFINITE], [Has isfinite function])
#AH_TEMPLATE([HAVE_RAISE], [Has raise function])
AH_TEMPLATE([HAVE_FINITE_IEEEFP_H],
	[Declaration of finite function in ieeefp.h instead of math.h])

save_libs="$LIBS"

enable_cflags_setting=no
AC_MSG_CHECKING([whether CFLAGS can be modified])
# Enable setting if the user has not specified the optimisation in CFLAGS.
echo "$CFLAGS" | grep "\-O.*\( \|$\)" 1>/dev/null 2>/dev/null
if test $? != 0; then
	enable_cflags_setting=yes
fi
AC_MSG_RESULT([$enable_cflags_setting])

# Configure options part I (not needing any compilation)

AC_ARG_ENABLE([debug],
  [AS_HELP_STRING([--enable-debug],
    [(GnuCOBOL C++) Enable -g C++ compiler debug option])],
  [],
  [enable_debug=no])

AC_ARG_ENABLE([experimental],
  [AS_HELP_STRING([--enable-experimental],
    [(GnuCOBOL C++) Enable experimental code (Developers only!)])],
  [if test "$enable_experimental" = "yes"; then
	AC_DEFINE([COB_EXPERIMENTAL], [1])
   fi],
  [])

AC_ARG_ENABLE([param-check],
  [AS_HELP_STRING([--enable-param-check],
    [(GnuCOBOL C++) Enable minimum parameter check for system libraries (default no)])],
  [if test "$enable_param_check" = "yes"; then
	AC_DEFINE([COB_PARAM_CHECK], [1])
   fi],
  [])

AC_ARG_WITH([patch-level],
  [AS_HELP_STRING([--with-patch-level],
    [(GnuCOBOL C++) Define a patch level (default TAR date), numeric, max. 8 digits])],
  [case $with_patch_level in  
    yes)	AC_MSG_ERROR([[You must give --with-patch-level an argument.]])
		;;
    no)		AC_MSG_ERROR([[--without-patch-level not supported.]])
		;;
    [[0-9]]*)
		if test $with_patch_level -gt 99999999; then
    		AC_MSG_ERROR([[Patch level must not contain more than 8 digits]])
		fi
		;;
    *)		AC_MSG_ERROR([[You must use a numeric patch level]])
		;;
  esac],
  [with_patch_level=COB_NUM_TAR_DATE])

AC_DEFINE_UNQUOTED([PATCH_LEVEL], [$with_patch_level])
  
AC_ARG_WITH([max-call-params],
  [AS_HELP_STRING([--with-max-call-params],
    [(GnuCOBOL C++) Define maximum number of parameters for CALL (default 192)])],
  [case $with_max_call_params in
    yes)	AC_MSG_ERROR([[You must give --with-max-call-params an argument.]])
		;;
    no)		AC_MSG_ERROR([[--without-max-call-params not supported.]])
		;;
    16 | 36 | 56 | 76 | 96 | 192 | 252)
		;;
    *)		AC_MSG_ERROR([[Maximum number of parameters for CALL must be one of 16/36/56/76/96/192/252]])
		;;
  esac],
  [with_max_call_params=192])

AC_DEFINE_UNQUOTED([MAX_CALL_FIELD_PARAMS], [$with_max_call_params])

# Basic capability tests

case $host_os in
  cygwin*)
	AC_PROG_CXX([xlc++_r xlc++ c++ g++ CC aCC])
	;;
  *)
	AC_PROG_CXX([xlc++_r xlc++ CC aCC c++ g++])
	;;
esac

AC_PROG_CC([xlc gcc cc])
AC_PROG_CC_STDC
AC_PROG_CPP

AC_USE_SYSTEM_EXTENSIONS

LT_INIT([dlopen win32-dll])

AC_PROG_EGREP
AM_PROG_CC_C_O
# Stop tests for Fortran
AC_DEFUN([AC_PROG_F77], [])
AC_PROG_LN_S
AC_PROG_INSTALL

# AC_LIBTOOL_DLOPEN
# AC_LIBTOOL_WIN32_DLL
# AC_PROG_LIBTOOL

AC_PROG_MAKE_SET
AC_LIB_RPATH

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([stdint.h sys/types.h signal.h fcntl.h malloc.h locale.h \
	stddef.h wchar.h dlfcn.h sys/time.h sys/wait.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_BIGENDIAN
AC_TYPE_SIZE_T
AC_STRUCT_TM
AC_CHECK_TYPES([sig_atomic_t], [], [], [[#include <signal.h>]])

AC_DEFINE([COB_KEYWORD_INLINE], [inline])

# Checks for library functions.
AC_FUNC_ALLOCA
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([memmove memset setlocale fcntl strerror strcasecmp])
AC_CHECK_FUNCS([strchr strrchr strdup strstr strtol gettimeofday localeconv])
AC_CHECK_FUNCS([readlink getexecname canonicalize_file_name realpath])
AC_CHECK_FUNCS([strcoll strxfrm setenv raise])

# Checks for UTC offset (init)
# FIXME: set to yes if C compiler defines _BSD_SOURCE
COB_HAS_UTC_OFFSET="no"

# Check for timezone
AC_MSG_CHECKING([for timezone variable access])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <time.h>]],
	[[timezone = 3600;]])],
	[AC_DEFINE([HAVE_TIMEZONE], [1])
		COB_HAS_UTC_OFFSET="yes"
		AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])])

# Check gcc/icc
COB_USES_GCC_ICC=no
COB_USES_GCC_ONLY=no
COB_USES_ICC_ONLY=no

AC_RUN_IFELSE([AC_LANG_PROGRAM([[]], [[
	#if	defined(__GNUC__) && !defined(__HP_aCC)
	return 0;
	#else
	return 1;
	#endif]])],
	[COB_USES_GCC_ICC=yes],
	[],
	[])

AC_RUN_IFELSE([AC_LANG_PROGRAM([[]], [[
	#if	defined(__GNUC__) && !defined(__INTEL_COMPILER) && !defined(__HP_aCC)
	return 0;
	#else
	return 1;
	#endif]])],
	[COB_USES_GCC_ONLY=yes],
	[],
	[])

AC_RUN_IFELSE([AC_LANG_PROGRAM([[]], [[
	#ifdef __INTEL_COMPILER
	return 0;
	#else
	return 1;
	#endif]])],
	[COB_USES_ICC_ONLY=yes],
	[],
	[])

COB_CC="$CXX"
if test "$COB_USES_ICC_ONLY" = "yes"
then
	LIBCOB_LIBS="-limf -lm"
else
	LIBCOB_LIBS="-lm"
fi

# Add --enable-code-coverage and test for code-coverage tools if enabled
AX_CODE_COVERAGE

if test "$COB_USES_GCC_ONLY" != yes -a "$enable_code_coverage" = "yes"; then
	AC_MSG_ERROR([Code coverage checks are only usable with GCC!])
fi


# Configure options part II (needing compilation)

AC_ARG_WITH([curses],
  [AS_HELP_STRING([--with-curses],
    [(GnuCOBOL) Used curses library for extended SCREEN I/O (default=check)])],
  [case "$with_curses" in
    ncursesw | ncurses | pdcurses | curses | check | no)
		USE_NCURSES="$with_curses"
		;;
    *)
		AC_MSG_ERROR([Wrong value for --with-curses, must be one of the following
  ncursesw, ncurses, pdcurses, curses  (use only the specified library)
  check  (use whatever curses library is usable, disable if no one usable)
  no  (disable curses usage)])
		;;
  esac],
  [USE_NCURSES="check"])

AC_ARG_WITH([seqra-extfh],
  [AS_HELP_STRING([--with-seqra-extfh],
    [(GnuCOBOL C++) Use external SEQ/RAN file handler])],
  [ if test "$with_seqra_extfh" = "yes"; then
	AC_DEFINE([WITH_SEQRA_EXTFH], [1])
    fi ],
  [])

AC_ARG_WITH([cisam],
  [AS_HELP_STRING([--with-cisam],
    [(GnuCOBOL C++) Use CISAM for ISAM I/O])],
  [ if test "$with_cisam" = "yes"; then
	AC_CHECK_HEADERS([isam.h], [],
		AC_MSG_ERROR([isam.h is required for CISAM]))
	AC_CHECK_LIB([ifisam], [isopen],
		[AC_DEFINE([WITH_CISAM], [1])
		LIBCOB_LIBS="$LIBCOB_LIBS -lifisam -lifisamx"],
		AC_MSG_ERROR([libifisam is required for CISAM]), [-lifisamx])
    fi ],
  [])

if test "$with_cisam" != "yes"; then
  AC_ARG_WITH([disam],
  [AS_HELP_STRING([--with-disam],
    [(GnuCOBOL C++) Use DISAM for ISAM I/O])],
  [ if test "$with_disam" = "yes"; then
	AC_CHECK_HEADERS([disam.h], [],
		AC_MSG_ERROR([disam.h is required for DISAM]))

	for cobdisam in disam disam8 disam72 disam71 disam7
	do
		AC_CHECK_LIB([$cobdisam], [isopen],
			[AC_DEFINE([WITH_DISAM], [1])
			LIBCOB_LIBS="$LIBCOB_LIBS -l$cobdisam"]
			cob_got_disam=yes
			break,
			[], [])
	done
	if test "$cob_got_disam" = "yes"; then
		AC_MSG_NOTICE([DISAM library found as -l$cobdisam])
	else
		AC_MSG_ERROR([DISAM library not found])
	fi
	unset cobdisam
	unset cob_got_disam
    fi ],
  [])
fi

if test "$with_cisam" != "yes" -a "$with_disam" != "yes"; then
  AC_ARG_WITH([vbisam],
  [AS_HELP_STRING([--with-vbisam],
    [(GnuCOBOL C++) Use VBISAM for ISAM I/O])],
  [ if test "$with_vbisam" = "yes"; then
	AC_CHECK_HEADERS([vbisam.h], [],
		AC_MSG_ERROR([vbisam.h is required for VBISAM]))
	AC_CHECK_LIB([vbisam], [isopen],
		[AC_DEFINE([WITH_VBISAM], [1])
		LIBCOB_LIBS="$LIBCOB_LIBS -lvbisam"],
		AC_MSG_ERROR([libvbisam is required for VBISAM]), [])
    fi ],
  [])
fi

if test "$with_cisam" != "yes" -a "$with_disam" != "yes" -a "$with_vbisam" != "yes"; then
  AC_ARG_WITH([index-extfh],
  [AS_HELP_STRING([--with-index-extfh],
    [(GnuCOBOL C++) Use external ISAM file handler])],
  [ if test "$with_index_extfh" = "yes"; then
	AC_DEFINE([WITH_INDEX_EXTFH], [1])
    fi ],
  [])
fi

if test "$with_cisam" != "yes" -a "$with_disam" != "yes" -a "$with_vbisam" != "yes" -a "$with_index_extfh" != "yes"; then
  AC_ARG_WITH([db],
  [AS_HELP_STRING([--with-db],
    [(GnuCOBOL C++) Use Berkeley DB >= 4.1 for ISAM I/O (default)])],
  [],
  [with_db=yes])
fi

AC_ARG_WITH([dl],
  [AS_HELP_STRING([--with-dl],
    [(GnuCOBOL C++) Use system dynamic loader (default)])],
  [case $with_dl in  
    yes)
		;;
    no)
		;;
    *)		AC_MSG_ERROR([[--with/without-dl can not have a value]])
		;;
   esac],
  [with_dl=yes])

AC_ARG_WITH([varseq],
  [AS_HELP_STRING([--with-varseq],
    [(GnuCOBOL C++) Define variable sequential format (default 1)])],
  [case $with_varseq in  
    yes)	AC_MSG_ERROR([[You must give --with-varseq an argument.]])
		;;
    no)		AC_MSG_ERROR([[--without-varseq not supported.]])
		;;
    [[0-3]])
		;;
    *)		AC_MSG_ERROR([[Invalid --with-varseq argument]])
		;;
  esac],
  [with_varseq=1])

AC_DEFINE_UNQUOTED([WITH_VARSEQ], [$with_varseq])

# Checks for mpir.
AC_ARG_WITH([mpir],
  [AS_HELP_STRING([--with-mpir],
    [(GnuCOBOL C++) Use MPIR instead of GMP])],
  [ if test "$with_mpir" = "yes"
    then
	AC_MSG_NOTICE([Checks for MPIR ...])
	AC_CHECK_HEADERS([mpirxx.h], [],
		AC_MSG_ERROR([mpirxx.h is required for MPIR]))
	AC_CHECK_LIB([mpir], [__gmpz_init],
		[AC_DEFINE([WITH_MPIR], [1])
		LIBCOB_LIBS="$LIBCOB_LIBS -lmpir"],
		AC_MSG_ERROR([libmpir is required for MPIR]), [-lmpir])
    fi ],
  [])

if test "$with_mpir" = "yes"
then

# Check just major/minor levels between header and library
MYOLDLIBS=$LIBS
LIBS="$MYOLDLIBS -lmpir"
AC_RUN_IFELSE([AC_LANG_PROGRAM([[#include <stdio.h>
	#include <string.h>
	#include <mpirxx.h>]], [[
	char	dbver[16];
	printf("checking matching MPIR version ... ");
	(void)snprintf (dbver, sizeof(dbver), "%d.%d",
			__GNU_MP_VERSION, __GNU_MP_VERSION_MINOR);
	if (!memcmp (dbver, gmp_version, strlen (dbver))) {
		printf("yes\n");
		return 0;
	}
	printf("no\n");
	printf("\tHeader  has version %d.%d.%d\n",
		__GNU_MP_VERSION, __GNU_MP_VERSION_MINOR,
		__GNU_MP_VERSION_PATCHLEVEL);
	printf("\tLibrary has version %s\n", gmp_version);
	return 1;
	]])],
	[],
	[AC_MSG_ERROR([Unable to use MPIR - Please check config.log])],
	[])
LIBS=$MYOLDLIBS

else

# Checks for gmp.
AC_MSG_NOTICE([Checks for GMP ...])
AC_CHECK_HEADERS([gmpxx.h], [], AC_MSG_ERROR([gmpxx.h (GMP) is required]), [])

AC_CHECK_LIB([gmp], [__gmpz_init],
	[LIBCOB_LIBS="$LIBCOB_LIBS -lgmp"],
	AC_MSG_ERROR([GMP library is required]), [])

# Check just major/minor levels between header and library
MYOLDLIBS=$LIBS
LIBS="$MYOLDLIBS -lgmp"
AC_RUN_IFELSE([AC_LANG_PROGRAM([[#include <stdio.h>
	#include <string.h>
	#include <gmpxx.h>]], [[
	char	dbver[16];
	printf("checking matching GMP version ... ");
	(void)snprintf (dbver, sizeof(dbver), "%d.%d",
			__GNU_MP_VERSION, __GNU_MP_VERSION_MINOR);
	if (!memcmp (dbver, gmp_version, strlen (dbver))) {
		printf("yes\n");
		return 0;
	}
	printf("no\n");
	printf("\tHeader  has version %d.%d.%d\n",
		__GNU_MP_VERSION, __GNU_MP_VERSION_MINOR,
		__GNU_MP_VERSION_PATCHLEVEL);
	printf("\tLibrary has version %s\n", gmp_version);
	return 1;
	]])],
	[],
	[AC_MSG_ERROR([Unable to use GMP - Please check config.log])],
	[])
LIBS=$MYOLDLIBS
fi

# Solaris has nanosleep in other libraries
AC_MSG_CHECKING([for nanosleep])
AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <time.h>]],
  [[nanosleep (NULL, NULL);]])],
  [AC_DEFINE([HAVE_NANO_SLEEP], [1]) AC_MSG_RESULT([yes])],
  [AC_CHECK_LIB([rt], [nanosleep], [], [], [])
   if test "x$ac_cv_lib_rt_nanosleep" = "xyes"; then
     AC_DEFINE([HAVE_NANO_SLEEP], [1])
     AC_MSG_RESULT([yes])
     LIBCOB_LIBS="$LIBCOB_LIBS -lrt"
   else
     AC_CHECK_LIB([posix4], [nanosleep], [], [], [])
     if test "x$ac_cv_lib_posix4_nanosleep" = "xyes"; then
       AC_DEFINE([HAVE_NANO_SLEEP], [1])
       AC_MSG_RESULT([yes])
       LIBCOB_LIBS="$LIBCOB_LIBS -lposix4"
     else
       AC_MSG_RESULT([no])
     fi
   fi])

AC_MSG_CHECKING([for clock_gettime and CLOCK_REALTIME])
AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <time.h>]],
  [[clock_gettime (CLOCK_REALTIME, NULL);]])],
  [AC_DEFINE([HAVE_CLOCK_GETTIME], [1]) AC_MSG_RESULT([yes])],
  [AC_MSG_RESULT([no])])

AC_MSG_CHECKING([for isfinite ])
AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <math.h>]],
  [[isfinite ( 1.0 );]])],
  [AC_DEFINE([HAVE_ISFINITE], [1]) AC_MSG_RESULT([yes])],
  [AC_MSG_RESULT([no])])

# Check for raise (optional) --> done via AC_CHECK_FUNCS
#AC_MSG_CHECKING([for raise])
#AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <signal.h>]],
#  [[raise (SIGINT);]])],
#  [AC_DEFINE([HAVE_RAISE], [1]) AC_MSG_RESULT([yes])],
#  [AC_MSG_RESULT([no])],
#  [])

# Check prototype for finite in math.h (alternative ieeefp.h)
AC_MSG_CHECKING([prototype for finite in <math.h>])
AC_EGREP_HEADER(finite, math.h, AC_MSG_RESULT([yes]),
	[AC_MSG_RESULT([no])
	AC_MSG_CHECKING([prototype for finite in <ieeefp.h>])
	AC_EGREP_HEADER(finite, ieeefp.h,
	[AC_DEFINE([HAVE_FINITE_IEEEFP_H], [1]) AC_MSG_RESULT([yes])],
	AC_MSG_ERROR([
		Declaration for finite function neither in math.h nor in ieeefp.h]))
	])

AC_CHECK_FUNCS([fdatasync sigaction])

# Checks for gettext.

case $host_os in
  darwin* | rhapsody*)
	;;
  *)
	gt_cv_func_CFPreferencesCopyAppValue=no
	gt_cv_func_CFLocaleCopyCurrent=no
	;;
esac

# Simon: removed, use --disable-nls instead
## Disable for Cygwin
#AC_RUN_IFELSE([AC_LANG_PROGRAM([[]], [[
#	#ifdef __CYGWIN__
#	return 0;
#	#else
#	return 1;
#	#endif]])],
#	[enable_nls=no],
#	[],
#	[])

AM_GNU_GETTEXT([external])
AM_GNU_GETTEXT_VERSION([0.19.8])
if test "x$LTLIBINTL" != "x"; then
  COBC_LIBS="$COBC_LIBS $LTLIBINTL"
  LIBCOB_LIBS="$LIBCOB_LIBS $LTLIBINTL"
fi

# Checks for internationalization stuff
# AM_ICONV
AM_LANGINFO_CODESET

# Checks for ncurses/pdcurses/curses.

AC_MSG_NOTICE([Checks for curses ...])

if test "$USE_NCURSES" = "ncursesw" -o "$USE_NCURSES" = "check"; then
  AC_CHECK_LIB([ncursesw], [initscr], [], [], [])
  if test "x$ac_cv_lib_ncursesw_initscr" = "xyes"; then
    AC_CHECK_HEADERS([ncursesw/ncurses.h], [USE_NCURSES="ncursesw"],
      [AC_CHECK_HEADERS([ncursesw/curses.h], [USE_NCURSES="ncursesw"],
        [if test "$USE_NCURSES" = "ncursesw"; then
           USE_NCURSES="missing_header"
         fi], [])], [])
    if test "$USE_NCURSES" = "ncursesw"; then
      LIBCOB_LIBS="$LIBCOB_LIBS -lncursesw"
    fi
  else
    if test "$USE_NCURSES" = "ncursesw"; then
      USE_NCURSES="missing_lib"
    fi
  fi
fi

if test "$USE_NCURSES" = "ncurses" -o "$USE_NCURSES" = "check"; then
  AC_CHECK_LIB([ncurses], [initscr], [], [], [])
  if test "x$ac_cv_lib_ncurses_initscr" = "xyes"; then
    AC_CHECK_HEADERS([ncurses.h], [USE_NCURSES="ncurses"],
      [AC_CHECK_HEADERS([ncurses/ncurses.h], [USE_NCURSES="ncurses"],
         [AC_CHECK_HEADERS([curses.h], [USE_NCURSES="ncurses"],
           [if test "$USE_NCURSES" = "ncurses"; then
              USE_NCURSES="missing_header"
            fi], [])], [])], [])
    if test "$USE_NCURSES" = "ncurses"; then
      LIBCOB_LIBS="$LIBCOB_LIBS -lncurses"
    fi
  else
    if test "$USE_NCURSES" = "ncurses"; then
      USE_NCURSES="missing_lib"
    fi
  fi
fi


if test "$USE_NCURSES" = "pdcurses" -o "$USE_NCURSES" = "check"; then
  AC_CHECK_LIB([pdcurses], [initscr], [], [], [])
  if test "x$ac_cv_lib_pdcurses_initscr" = "xyes"; then
    AC_CHECK_HEADERS([pdcurses.h], [USE_NCURSES="pdcurses"],
      [AC_CHECK_HEADERS([curses.h], [USE_NCURSES="pdcurses"],
         [if test "$USE_NCURSES" = "pdcurses"; then
            USE_NCURSES="missing_header"
          fi], [])], [])
    if test "$USE_NCURSES" = "pdcurses"; then
      LIBCOB_LIBS="$LIBCOB_LIBS -lpdcurses"
    fi
  else
    if test "$USE_NCURSES" = "pdcurses"; then
      USE_NCURSES="missing_lib"
    fi
  fi
fi

if test "$USE_NCURSES" = "curses" -o "$USE_NCURSES" = "check"; then
  AC_CHECK_LIB([curses], [initscr], [], [], [])
  if test "x$ac_cv_lib_curses_initscr" = "xyes"; then
    AC_CHECK_HEADERS([curses.h], [USE_NCURSES="curses"],
      [if test "$USE_NCURSES" = "curses"; then
         USE_NCURSES="missing_header"
       fi], [])
    if test "$USE_NCURSES" != "no"; then
      LIBCOB_LIBS="$LIBCOB_LIBS -lcurses"
    fi
  fi
else
  if test "$USE_NCURSES" = "curses"; then
    USE_NCURSES="missing_lib"
  fi
fi

case "$USE_NCURSES" in
  check)
	USE_NCURSES="not_found"
	AC_DEFINE([WITH_CURSES], ["no curses found"])
	;;
  no)
	AC_DEFINE([WITH_CURSES], ["disabled"])
	;;
  missing_lib)
	AC_MSG_ERROR([[Not able to link configured library $with_curses]])
	;;
  missing_header)
	AC_MSG_ERROR([[No header found for configured library $with_curses]])
	;;
  ncursesw)
	AC_DEFINE([WITH_CURSES], ["ncursesw"])
	;;
  ncurses)
	AC_DEFINE([WITH_CURSES], ["ncurses"])
	;;
  pdcurses)
	AC_DEFINE([WITH_CURSES], ["pdcurses"])
	;;
  curses)
	AC_DEFINE([WITH_CURSES], ["curses"])
	;;
esac


if test "$USE_NCURSES" = "ncurses" -o "$USE_NCURSES" = "ncursesw"; then
    AC_MSG_CHECKING([for ncurses _nc_freeall function])
    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
    extern void _nc_freeall (void);]],
    [[_nc_freeall ();]])],
    [AC_DEFINE([HAVE_CURSES_FREEALL], [1]) AC_MSG_RESULT([yes])],
    [AC_MSG_RESULT([no])],
    [])
    AC_MSG_CHECKING([for ncurses use_legacy_coding function])
    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
    extern void use_legacy_coding (void);]],
    [[use_legacy_coding ();]])],
    [AC_DEFINE([HAVE_USE_LEGACY_CODING], [1]) AC_MSG_RESULT([yes])],
    [AC_MSG_RESULT([no])],
    [])
fi

if test "$USE_NCURSES" != "no" -a "$USE_NCURSES" != "not_found"; then
	AC_MSG_CHECKING([for curses color_set function])
	AC_LINK_IFELSE([AC_LANG_PROGRAM([[
	#ifdef HAVE_NCURSESW_NCURSES_H
	#include <ncursesw/ncurses.h>
	#elif defined (HAVE_NCURSESW_CURSES_H)
	#include <ncursesw/curses.h>
	#elif defined (HAVE_NCURSES_H)
	#include <ncurses.h>
	#elif defined (HAVE_NCURSES_NCURSES_H)
	#include <ncurses/ncurses.h>
	#elif defined (HAVE_PDCURSES_H)
	#include <pdcurses.h>
	#elif defined (HAVE_CURSES_H)
	#include <curses.h>
	#endif]], [[
	color_set (0, NULL);]])],
	[AC_DEFINE([HAVE_COLOR_SET], [1]) AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])],
	[])
fi

# Checks for db.
if test "$with_db" = "yes"; then
	AC_MSG_NOTICE([Checks for Berkeley DB ...])

	AC_CHECK_HEADERS([db.h], [], AC_MSG_ERROR([Berkeley DB db.h is missing]), [])

	# BDB header exists. Extract major/minor number pair
	COB_BDB_HEADER=''
	AC_RUN_IFELSE([AC_LANG_SOURCE([[
		#include <stdio.h>
		#include <db.h>
		int main (int argc, char **argv)
		{
			(void)argv;
			if (argc > 1)
				printf ("%d.%d\n", DB_VERSION_MAJOR, DB_VERSION_MINOR);
			return 0;
		}
		]])],
		[COB_BDB_HEADER=`./conftest$ac_exeext x`],
		[AC_MSG_ERROR([Unable to extract Berkeley DB version information from db.h])],
		[])
	if test "x$COB_BDB_HEADER" = "x"; then
		AC_MSG_ERROR([Unable to extract Berkeley DB version information])
	fi
	AC_MSG_CHECKING([for Berkeley DB db.h version >= 4.1])
	COB_BDB_HEADER_MAJOR=$(echo "$COB_BDB_HEADER" | cut -d. -f1)
	if test $COB_BDB_HEADER_MAJOR -gt 4; then
		AC_MSG_RESULT([yes ($COB_BDB_HEADER)])
	else
		COB_BDB_HEADER_MINOR=$(echo "$COB_BDB_HEADER" | cut -d. -f2)
		if test $COB_BDB_HEADER_MAJOR -eq 4 -a $COB_BDB_HEADER_MINOR -ge 1; then
			AC_MSG_RESULT([yes ($COB_BDB_HEADER)])
		else
			AC_MSG_RESULT([no ($COB_BDB_HEADER)])
			AC_MSG_ERROR([Berkeley DB db.h has incompatible version])
		fi
	fi
	MYOLDLIBS="$LIBS"
	cob_got_db=no
	AC_MSG_CHECKING([for Berkeley DB library with version $COB_BDB_HEADER])
	for cobdb in db db-$COB_BDB_HEADER
	do
		LIBS="$MYOLDLIBS -l$cobdb"
		AC_RUN_IFELSE([AC_LANG_PROGRAM([[#include <stdio.h>
		#include <db.h>]], [[
		int major, minor, patch;
		db_version (&major, &minor, &patch);
		if (major == DB_VERSION_MAJOR && minor == DB_VERSION_MINOR) {
			return 0;
		}
		return 1;]])],
		[AC_DEFINE([WITH_DB], [1])
		cob_got_db=yes
		LIBCOB_LIBS="$LIBCOB_LIBS -l$cobdb"
		break],
		[],
		[])
	done
	LIBS="$MYOLDLIBS"
	if test "$cob_got_db" = "yes"; then
		AC_MSG_RESULT([yes])
		AC_MSG_NOTICE([BDB library version $COB_BDB_HEADER found as -l$cobdb])
	else
		AC_MSG_RESULT([no])
		AC_MSG_ERROR([BDB library version $COB_BDB_HEADER not found])
	fi
	unset cobdb
	unset cob_got_db
	unset COB_BDB_HEADER
fi

# Checks for dl/ltdl.
DEFINE_DL="no"

AC_RUN_IFELSE([AC_LANG_PROGRAM([[]], [[
	#ifdef _WIN32
	return 0;
	#else
	return 1;
	#endif]])],
	[DEFINE_DL="yes"],
	[],
	[])

if test "$DEFINE_DL" = "no" -a "$with_dl" = "yes"; then
  if test "x$ac_cv_header_dlfcn_h" = "xyes"; then
    AC_CHECK_LIB([c], [dlopen], [DEFINE_DL="yes"], [], [])
    if test "$DEFINE_DL" = "no"; then
      AC_CHECK_LIB([dl], [dlopen], [DEFINE_DL="yes"], [], [])
      if test "$DEFINE_DL" = "yes"; then
        AC_DEFINE([USE_LIBDL], [1])
        LIBCOB_LIBS="$LIBCOB_LIBS -ldl"
        AC_CHECK_LIB([dl], [dladdr], [AC_DEFINE([HAVE_DLADDR], [1])], [], [])
      else
        AC_CHECK_LIB([dld], [dlopen], [DEFINE_DL="yes"], [], [])
        if test "$DEFINE_DL" = "yes"; then
          AC_DEFINE([USE_LIBDL], [1])
          LIBCOB_LIBS="$LIBCOB_LIBS -ldld"
          AC_CHECK_LIB([dld], [dladdr], [AC_DEFINE([HAVE_DLADDR], [1])], [], [])
        fi
      fi
    else
      AC_DEFINE([USE_LIBDL], [1])
      AC_CHECK_LIB([c], [dladdr], [AC_DEFINE([HAVE_DLADDR], [1])], [], [])
    fi
  fi
fi

if test "$DEFINE_DL" = "no"; then
  AC_CHECK_HEADERS([ltdl.h], [], AC_MSG_ERROR([ltdl.h is required]), [])
  AC_CHECK_LIB([ltdl], [lt_dlopen],
	[LIBCOB_LIBS="$LIBCOB_LIBS -lltdl"],
	AC_MSG_ERROR([libltdl is required]), [])
fi

# Checks for workable strftime
AC_MSG_CHECKING([for working strftime])
AC_RUN_IFELSE([AC_LANG_PROGRAM([[#include <stdio.h>
	#include <time.h>]], [[
	time_t	ticks;
	int	ret;
	char	buff[16];
	ticks = time(NULL);
	ret = strftime(buff, 15, "%z", localtime(&ticks));
	if (ret != 5) {
		return 1;
	}
	if (buff[0] != '+' && buff[0] != '-') {
		return 1;
	}
	return 0;]])],
	[AC_DEFINE([COB_STRFTIME], [1])
		COB_HAS_UTC_OFFSET="yes"
		AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])],
	[])

# Checks for size of long
AC_MSG_CHECKING([if size of long int = size of long long])
AC_RUN_IFELSE([AC_LANG_PROGRAM([[]], [[
	if (sizeof(long int) == sizeof(long long))
		return 0;
	return 1;
	]])],
	[AC_DEFINE([COB_LI_IS_LL], [1]) AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])],
	[])

AC_MSG_CHECKING([if long is 32 bits])
AC_RUN_IFELSE([AC_LANG_PROGRAM([[]], [[
	if (sizeof (long) == 4)
		return 0;
	return 1;
	]])],
	[AC_DEFINE([COB_32_BIT_LONG], [1]) AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])],
	[])


COB_HAS_64_BIT_POINTER="no"
AC_MSG_CHECKING([if pointer is longer than 32 bits])
AC_RUN_IFELSE([AC_LANG_PROGRAM([[]], [[
	if (sizeof (void *) > 4U)
		return 0;
	return 1;
	]])],
	[COB_HAS_64_BIT_POINTER="yes"
	 AC_DEFINE([COB_64_BIT_POINTER], [1])
	 AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])],
	[])

#if test "$enable_debug" != "yes" -a "$COB_USES_GCC_ONLY" = "yes"; then
#	MYOLDCFLAGS="$CFLAGS"
#	CFLAGS="$CFLAGS -fno-asynchronous-unwind-tables"
#	AC_MSG_CHECKING([for gcc unwind tables option])
#	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[int testint;]])],
#		[AC_DEFINE([HAVE_UNWIND_OPT], [1]) AC_MSG_RESULT([yes])],
#		[AC_MSG_RESULT([no])])
#	CFLAGS="$MYOLDCFLAGS"
#fi

# Check gcc wrapv option
# We MAY need this eventually for >= 4.4. 4.3 appears to be OK.
#if test "$COB_USES_GCC_ONLY" = "yes"; then
#	MYOLDCFLAGS="$CFLAGS"
#	CFLAGS="$CFLAGS -fwrapv"
#	AC_MSG_CHECKING([for gcc -fwrapv option])
#	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[int testint;]])],
#		[AC_DEFINE([HAVE_FWRAPV_OPT], [1]) AC_MSG_RESULT([yes])],
#		[AC_MSG_RESULT([no])])
#		[],
#		[CFLAGS="$MYOLDCFLAGS"])
#	CFLAGS="$MYOLDCFLAGS"
#fi

# Check if aligned attribute works
AC_MSG_CHECKING([for aligned attribute])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]],
	[[char testchar[4] __attribute__((aligned));]])],
	[AC_DEFINE([HAVE_ATTRIBUTE_ALIGNED], [1]) AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])])

# GnuCOBOL C++ Configuration


if test "$enable_cflags_setting" = "yes"; then

	# Remove -O2 option added by AC_PROG_CC and add -O0
	if test "$enable_debug" = "yes" -o "$enable_code_coverage" = "yes"; then
		CFLAGS=`echo "$CFLAGS" | sed -e 's/ *-O@<:@0-9a-zA-Z@:>@* */ /g' -e 's/ $//' -e 's/^ //'`
		if test "$COB_USES_GCC" = "yes"; then
			CFLAGS="$CFLAGS -O0"
		fi
	fi

	# For debugging: add -g3 if using GCC.
	if test "$enable_debug" = "yes" -a "$COB_USES_GCC" = "yes"; then
		MYOLDCFLAGS="$CFLAGS"
		CFLAGS="$CFLAGS -g3"
		AC_MSG_CHECKING([for gcc -g3 option])
		AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[int testint;]])],
			[AC_MSG_RESULT([yes])],
			[CFLAGS="$MYOLDCFLAGS"; AC_MSG_RESULT([no])])
		unset MYOLDCFLAGS
	fi
fi

unset enable_cflags_setting

# include directory (install dir)

if test "x$prefix" = "xNONE"; then
  if test "x$includedir" = "x\${prefix}/include"; then
    COB_CFLAGS="-I${ac_default_prefix}/include"
  else
    COB_CFLAGS="-I${includedir}"
  fi
else
  if test "x$includedir" = "x\${prefix}/include"; then
    if test "x$prefix" = "x/usr"; then
      COB_CFLAGS=""
    else
      COB_CFLAGS="-I${prefix}/include"
    fi
  else
    COB_CFLAGS="-I${includedir}"
  fi
fi

if test "x$CPPFLAGS" != "x"; then
	COB_CFLAGS="$COB_CFLAGS $CPPFLAGS"
fi

COB_LIBS="-L$libdir -lcob ${LIBCOB_LIBS}"
COB_LDFLAGS=""
if test "x$LDFLAGS" != "x"; then
	COB_LIBS="$LDFLAGS $COB_LIBS"
	COB_LDFLAGS="$LDFLAGS"
fi

COB_CONFIG_DIR="$datadir/$PACKAGE_TARNAME/config"
COB_COPY_DIR="$datadir/$PACKAGE_TARNAME/copy"
COB_LIBRARY_PATH="$libdir/$PACKAGE_TARNAME"
COB_OBJECT_EXT="$OBJEXT"
COB_MODULE_EXT=`echo "$acl_cv_shlibext" | sed -e 's/dll\.a/dll/'`
COB_EXE_EXT="$EXEEXT"
LIBS="$save_libs"
COB_EXPORT_DYN="`eval echo $export_dynamic_flag_spec`"
COB_PIC_FLAGS=`echo "$lt_prog_compiler_pic" | sed -e 's/^ //'`
COB_SHARED_OPT="-shared"

if test "$enable_debug" != "yes" -a "x$CXXFLAGS" != "x"
then
	CXXFLAGS=`echo "$CXXFLAGS" | sed -e 's/-g//' -e 's/^ //' -e 's/ $//'`
fi

# Include CXXFLAGS in COB_CFLAGS without optimization/debug options

cob_temp_flags=""
if test "x$CXXFLAGS" != "x"
then
	cob_temp_flags=`echo "$CXXFLAGS" | sed -e 's/-g//' -e 's/ $//' -e 's/^ //'`
	cob_temp_flags=`echo "$cob_temp_flags" | sed -e 's/@<:@+-@:>@O@<:@0-9s@:>@//' -e 's/ $//' -e 's/^ //'`
	cob_temp_flags=`echo "$cob_temp_flags" | sed -e 's/-O//' -e 's/ $//' -e 's/^ //'`
	cob_temp_flags=`echo "$cob_temp_flags" | sed -e 's/-fmessage-length=0//'`
	cob_temp_flags=`echo "$cob_temp_flags" | sed -e 's/-D_FORTIFY_SOURCE=.//'`
	cob_temp_flags=`echo "$cob_temp_flags" | sed -e 's/-fstack-protector//'`
	cob_temp_flags=`echo "$cob_temp_flags" | sed -e 's/-funwind-tables//'`
	cob_temp_flags=`echo "$cob_temp_flags" | sed -e 's/-fasynchronous-unwind-tables//'`
	cob_temp_flags=`echo "$cob_temp_flags" | sed -e 's/  */ /g' -e 's/ $//' -e 's/^ //'`
fi

if test "x$cob_temp_flags" != "x"
then
	COB_CFLAGS="$COB_CFLAGS $cob_temp_flags"
fi
unset cob_temp_flags

if test "$COB_USES_GCC_ICC" = "yes"
then
	if test "x$CXXFLAGS" != "x"
	then
		CXXFLAGS="$CXXFLAGS -pipe"
	else
		CXXFLAGS="-pipe"
	fi
	if test "x$COB_CFLAGS" != "x"
	then
		COB_CFLAGS="$COB_CFLAGS -pipe"
	else
		COB_CFLAGS="-pipe"
	fi
fi

# Special stuff

AH_VERBATIM([_XOPEN_SOURCE_EXTENDED],
[/* Define to 1 if on HPUX.  */
#ifndef _XOPEN_SOURCE_EXTENDED
# undef _XOPEN_SOURCE_EXTENDED
#endif])dnl

COB_FIX_LIB="$COB_EXPORT_DYN"
COB_FIX_LIBTOOL=""
case $host_os in
  mingw*)
	if test "$prefix" = "NONE"
	then
	COB_CONFIG_DIR="`cd /usr && pwd -W`/local/share/$PACKAGE_TARNAME/config"
	COB_COPY_DIR="`cd /usr && pwd -W`/local/share/$PACKAGE_TARNAME/copy"
	fi
	COB_EXPORT_DYN="-Wl,--export-all-symbols -Wl,--enable-auto-import -Wl,--enable-auto-image-base"
	COB_FIX_LIB=""
	COB_FIX_LIBTOOL="-Wl,--enable-auto-import"
	;;
  cygwin*)
	COB_EXPORT_DYN="-Wl,--export-all-symbols -Wl,--enable-auto-import -Wl,--enable-auto-image-base"
	COB_FIX_LIB=""
	COB_FIX_LIBTOOL="-Wl,--enable-auto-import"
	;;
  darwin* | rhapsody*)
	COB_SHARED_OPT="-bundle -flat_namespace -undefined suppress"
	;;
  hpux*)
	LIBCOB_LIBS="$LIBCOB_LIBS -lunalign"
	if test "$COB_USES_GCC_ICC" != "yes"
	then
		CXXFLAGS="$CXXFLAGS -Aa -Ag++ -D_HPUX_SOURCE"
		COB_CFLAGS="$COB_CFLAGS -Aa -Ag++ -D_HPUX_SOURCE"
		COB_SHARED_OPT="-b"
		AC_DEFINE([_XOPEN_SOURCE_EXTENDED], [1])
		rm -f hptest*
		echo 'int main(int ac, char **av) { return 0; }' > hptest.cpp
		${CXX} ${CXXFLAGS} +Olit=all -o hptest hptest.cpp > hptest.out 2>&1
		if test $? -ne 0 -o -s hptest.out
		then
			CXXFLAGS="$CXXFLAGS +ESlit +O2"
			COB_CFLAGS="$COB_CFLAGS +ESlit -w"
		else
			CXXFLAGS="$CXXFLAGS +Olit=all +O2"
			COB_CFLAGS="$COB_CFLAGS +Olit=all -w"
		fi
		rm -f hptest*
	fi
	;;
  aix*)
	COB_EXPORT_DYN="-Wl,-bexpfull -Wl,-brtl"
	if test "$COB_USES_GCC_ICC" != "yes"
	then
		CXXFLAGS="$CXXFLAGS -O2 -Q -qro -qroconst"
		COB_SHARED_OPT="-G"
		COB_FIX_LIB=""
		# COB_FIX_LIB="-Wc,-G $COB_EXPORT_DYN"
		# COB_CFLAGS="$COB_CFLAGS -qchars=signed"
	else
		COB_FIX_LIB="$COB_EXPORT_DYN"
	fi
	;;
  solaris*)
	if test "$COB_USES_GCC_ICC" != "yes"
	then
		CXXFLAGS="$CXXFLAGS -xO2"
		# Need to workaround possible names collision
		# Without __XOPEN_OR_POSIX there are too many stupid defines, like SEC, MILLISEC
		COB_CFLAGS="$COB_CFLAGS -D__XOPEN_OR_POSIX -erroff=wunreachable"
		COB_LDFLAGS="$COB_LDFLAGS -xmemalign=1i"
		COB_LIBS="-xstrconst -xmemalign=1i $COB_LIBS"
		LDFLAGS="$LDFLAGS -xmemalign=1i"
		COB_SHARED_OPT="-G"
		LIBCOB_LIBS="$LIBCOB_LIBS -lCstd -lCrun"
	fi
	;;
esac

if test "$COB_USES_GCC_ONLY" = "yes" && test "$with_gnu_ld" = "yes"
then
	MYOLDLDFLAGS="$LDFLAGS"
	LDFLAGS="$LDFLAGS -Wl,-z,relro,-z,now,-O1"
	AC_MSG_CHECKING([for ld bind now option])
	AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[int testint;]])],
		[AC_MSG_RESULT([yes])],
		[LDFLAGS=$MYOLDLDFLAGS
		 AC_MSG_RESULT([no])])
	unset MYOLDLDFLAGS
	CXXFLAGS="$CXXFLAGS -finline-functions -fsigned-char -Wall -Wwrite-strings -Wno-format-y2k"
	if test "$enable_debug" != "yes"
	then
		CXXFLAGS="$CXXFLAGS -U_FORTIFY_SOURCE"
	fi
elif test "$COB_USES_ICC_ONLY" = "yes"
then
	CXXFLAGS="$CXXFLAGS -finline-functions -Wall -wd1419 -vec-report0 -opt-report 0"
fi

if test "x$lt_cv_dlopen_self" != "xyes"
then
	AC_DEFINE([COB_NO_SELFOPEN], [1])
fi

if test "$with_cisam" != "yes" -a "$with_disam" != "yes" -a "$with_vbisam" != "yes" -a "$with_index_extfh" != "yes" -a "$with_db" != "yes"
then
	COB_HAS_ISAM=no
else
	COB_HAS_ISAM=yes
fi

if test "$USE_NCURSES" = "not_found" -o "$USE_NCURSES" = "no"; then
	COB_HAS_CURSES=no
else
	COB_HAS_CURSES=yes
fi

unset COB_USES_GCC_ICC
unset COB_USES_GCC_ONLY
unset COB_USES_ICC_ONLY
unset MYOLDCFLAGS
unset MYOLDLIBS
unset MYOCLIBS

# Generate the output

AM_CONDITIONAL([COB_MAKE_IX], [test $COB_HAS_ISAM = yes])

AC_DEFINE_UNQUOTED([COB_EXPORT_DYN], ["$COB_EXPORT_DYN"])
AC_DEFINE_UNQUOTED([COB_PIC_FLAGS], ["$COB_PIC_FLAGS"])
AC_DEFINE_UNQUOTED([COB_SHARED_OPT], ["$COB_SHARED_OPT"])
AC_DEFINE_UNQUOTED([COB_OBJECT_EXT], ["$COB_OBJECT_EXT"])
AC_DEFINE_UNQUOTED([COB_MODULE_EXT], ["$COB_MODULE_EXT"])
AC_DEFINE_UNQUOTED([COB_EXE_EXT], ["$COB_EXE_EXT"])
if test "x$striplib" != "x"; then
	AC_DEFINE_UNQUOTED([COB_STRIP_CMD], ["$striplib"])
fi
AC_SUBST([COB_CC])
AC_SUBST([COB_CFLAGS])
AC_SUBST([COB_LDFLAGS])
AC_SUBST([COB_LIBS])
AC_SUBST([COB_CONFIG_DIR])
AC_SUBST([COB_COPY_DIR])
AC_SUBST([COB_LIBRARY_PATH])
AC_SUBST([COB_OBJECT_EXT])
AC_SUBST([COB_MODULE_EXT])
AC_SUBST([COB_EXE_EXT])
AC_SUBST([COBC_LIBS])
AC_SUBST([LIBCOB_LIBS])
AC_SUBST([COB_EXPORT_DYN])
AC_SUBST([COB_PIC_FLAGS])
AC_SUBST([COB_SHARED_OPT])
COB_BIGENDIAN="$ac_cv_c_bigendian"
AC_SUBST([COB_BIGENDIAN])
AC_SUBST([COB_FIX_LIB])
AC_SUBST([COB_FIX_LIBTOOL])
AC_SUBST([COB_KEYWORD_INLINE])
AC_SUBST([COB_HAS_ISAM])
AC_SUBST([COB_HAS_CURSES])
AC_SUBST([COB_HAS_UTC_OFFSET])
AC_SUBST([COB_HAS_64_BIT_POINTER])
AC_SUBST([COB_PATCH_LEVEL], [$with_patch_level]) # needed for bin/cob-config

AC_CONFIG_FILES([bin/cob-config Makefile libcob/Makefile lib/Makefile
	cobc/Makefile bin/Makefile po/Makefile.in doc/Makefile
	config/Makefile copy/Makefile
	tests/atlocal tests/Makefile tests/cobol85/Makefile
	extras/Makefile])

AC_CONFIG_COMMANDS([chmod1], [chmod +x bin/cob-config])
AC_CONFIG_COMMANDS([chmod2], [chmod +x tests/atconfig])
AC_CONFIG_COMMANDS([chmod3], [chmod +x tests/atlocal])

AC_OUTPUT

echo "Creating libcob.h..."
if test "$with_mpir" = "yes"
then
  sed -e "s/gmp/mpir/" $srcdir/libcob.h.in > "$ac_top_build_prefix"libcob.h
else
  cp -p $srcdir/libcob.h.in "$ac_top_build_prefix"libcob.h
fi

AC_MSG_NOTICE([GnuCOBOL C++ Configuration:])
AC_MSG_NOTICE([ CC                ${CC}])
AC_MSG_NOTICE([ CFLAGS            ${CFLAGS}])
AC_MSG_NOTICE([ CXXFLAGS          ${CXXFLAGS}])
AC_MSG_NOTICE([ COB_CFLAGS        ${COB_CFLAGS}])
AC_MSG_NOTICE([ LDFLAGS           ${LDFLAGS}])
AC_MSG_NOTICE([ COB_LDFLAGS       ${COB_LDFLAGS}])
if test "x$COBC_LIBS" != "x"; then
  AC_MSG_NOTICE([ COBC_LIBS         ${COBC_LIBS}])
fi
AC_MSG_NOTICE([ COB_LIBS          ${COB_LIBS}])
AC_MSG_NOTICE([ COB_CONFIG_DIR    ${COB_CONFIG_DIR}])
AC_MSG_NOTICE([ COB_COPY_DIR      ${COB_COPY_DIR}])
AC_MSG_NOTICE([ COB_LIBRARY_PATH  ${COB_LIBRARY_PATH}])
AC_MSG_NOTICE([ COB_OBJECT_EXT    ${COB_OBJECT_EXT}])
AC_MSG_NOTICE([ COB_MODULE_EXT    ${COB_MODULE_EXT}])
AC_MSG_NOTICE([ COB_EXE_EXT       ${COB_EXE_EXT}])
AC_MSG_NOTICE([ COB_SHARED_OPT    ${COB_SHARED_OPT}])
AC_MSG_NOTICE([ COB_PIC_FLAGS     ${COB_PIC_FLAGS}])
AC_MSG_NOTICE([ COB_EXPORT_DYN    ${COB_EXPORT_DYN}])
if test "x$striplib" != "x"; then
  AC_MSG_NOTICE([ COB_STRIP_CMD     ${striplib}])
fi
if test "${DEFINE_DL}" = "yes" ; then
  AC_MSG_NOTICE([ Dynamic loading:                             System])
else
  AC_MSG_NOTICE([ Dynamic loading:                             Libtool])
fi
AC_MSG_NOTICE([ Use gettext for international messages:      ${USE_NLS}])
AC_MSG_NOTICE([ Use fcntl for file locking:                  ${ac_cv_func_fcntl}])
case "$USE_NCURSES" in
  not_found)
    AC_MSG_NOTICE([ screen I/O (no curses found):                NO])
	;;
  no)
    AC_MSG_NOTICE([ screen I/O (disabled):                       NO])
	;;
  *)
    AC_MSG_NOTICE([ Use ncurses/pdcurses/curses for screen I/O:  ${USE_NCURSES}])
	;;
esac
if test "$with_mpir" = "yes"; then
  AC_MSG_NOTICE([ Use MPIR Library                             yes])
else
  AC_MSG_NOTICE([ Use GMP  Library                             yes])
fi
if test "$with_seqra_extfh" = "yes"; then
  AC_MSG_NOTICE([ Use external SEQ/RAN file handler            yes])
fi
if test "$with_index_extfh" = "yes"; then
  AC_MSG_NOTICE([ Use external ISAM file handler               yes])
elif test "$with_cisam" = "yes"; then
  AC_MSG_NOTICE([ Use CISAM for ISAM I/O                       yes])
elif test "$with_disam" = "yes"; then
  AC_MSG_NOTICE([ Use DISAM for ISAM I/O                       yes])
elif test "$with_vbisam" = "yes"; then
  AC_MSG_NOTICE([ Use VBISAM for ISAM I/O                      yes])
elif test "$with_db" = "yes"; then
  AC_MSG_NOTICE([ Use Berkeley DB for ISAM I/O:                yes])
else
  AC_MSG_NOTICE([ ISAM I/O (no handler configured):            NO])
fi
unset DEFINE_DL
if test "$COB_HAS_UTC_OFFSET" = "no"; then
  AC_MSG_WARN([ Could not find strftime or timezone. FORMATTED-CURRENT-DATE, FORMATTED-DATE])
  AC_MSG_WARN([and FORMATTED-DATETIME may not work.])
fi

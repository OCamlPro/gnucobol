#
# Configure template for GnuCOBOL
# Process this file with autoconf to produce a configure script.
#
# Copyright (C) 2001-2012, 2014-2021 Free Software Foundation, Inc.
# Written by Keisuke Nishida, Roger While, Ron Norman, Simon Sobisch,
# Edward Hart
#
# This file is part of GnuCOBOL.
#
# The GnuCOBOL compiler is free software: you can redistribute it
# and/or modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# GnuCOBOL is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GnuCOBOL.  If not, see <https://www.gnu.org/licenses/>.

AC_PREREQ([2.64])

AC_INIT([GnuCOBOL],
	[4.0],
	[bug-gnucobol@gnu.org],
	[gnucobol],
	[https://www.gnu.org/software/gnucobol/])
AC_REVISION([GnuCOBOL snapshot $Revision: 4131 $])
AC_CONFIG_SRCDIR([libcob.h])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_TESTDIR([tests])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([build_aux])

AC_CONFIG_FILES([Makefile libcob/Makefile lib/Makefile cobc/Makefile
	bin/Makefile po/Makefile.in doc/Makefile config/Makefile
	copy/Makefile tests/Makefile tests/cobol85/Makefile
	extras/Makefile])

AC_CONFIG_FILES([bin/cob-config], [chmod +x bin/cob-config])
AC_CONFIG_FILES([pre-inst-env:build_aux/pre-inst-env.in],
  [chmod +x pre-inst-env])
AC_CONFIG_FILES([tests/atlocal], [chmod +x tests/atlocal])
AC_CONFIG_FILES([tests/run_prog_manual.sh],
  [chmod +x tests/run_prog_manual.sh])

# Note for SUN Solaris (gcc)
# export/setenv CFLAGS "-m64 -mptr64"
# export/setenv LDFLAGS "-m64 -mptr64 -L/usr/local/lib/sparcv9"
# ./configure --libdir=/usr/local/lib/sparcv9
#
# Hack for AIX 64 bit (gcc)
# Required -
# export/setenv CFLAGS=-maix64
# export/setenv LDFLAGS=-maix64

if echo $CFLAGS | grep 'aix64' 1>/dev/null 2>&1; then
	if test -f /usr/ccs/bin/ar; then
		AR="/usr/ccs/bin/ar -X64"
	else
		AR="ar -X64"
	fi
	NM="/usr/ccs/bin/nm -X64 -B"
fi

AM_INIT_AUTOMAKE([gnu no-texinfo.tex])

# Autoheader templates
AH_TEMPLATE([COB_DEBUG_LOG], [Enable internal logging (Developers only!)])
AH_TEMPLATE([COB_EXPERIMENTAL], [Enable experimental code (Developers only!)])
AH_TEMPLATE([COB_TREE_DEBUG], [Enable extra checks within the compiler (Developers only!)])
AH_TEMPLATE([COB_PARAM_CHECK], [Enable minimum parameter check for system libraries])
AH_TEMPLATE([PATCH_LEVEL], [Define a patch level (numeric, max. 8 digits)])
AH_TEMPLATE([MAX_CALL_FIELD_PARAMS], [Define maximum parameters for CALL])
AH_TEMPLATE([WITH_MATH], [Math multiple precision library])
AH_TEMPLATE([WITH_INDEX_EXTFH], [Compile with obsolete external INDEXED handler])
AH_TEMPLATE([WITH_INDEXED], [Default INDEXED file handler])
AH_TEMPLATE([WITH_IXDFLT], [Default INDEXED file handler])
AH_TEMPLATE([WITH_SEQRA_EXTFH], [Compile with obsolete external SEQ/RAN handler])
AH_TEMPLATE([WITH_VISAM], [Use V-ISAM as INDEXED handler])
AH_TEMPLATE([WITH_DISAM], [Use D-ISAM as INDEXED handler])
AH_TEMPLATE([WITH_CISAM], [Use C-ISAM as INDEXED handler])
AH_TEMPLATE([WITH_VBISAM], [Use VB-ISAM as INDEXED handler])
AH_TEMPLATE([WITH_DB], [Use Berkeley DB library as INDEXED handler])
AH_TEMPLATE([WITH_LMDB], [Use Lightning Memory-Mapped Database as INDEXED handler])
AH_TEMPLATE([WITH_ODBC], [Use ODBC for INDEXED file handler])
AH_TEMPLATE([WITH_OCI], [Use OCI for INDEXED file handler])
AH_TEMPLATE([WITH_STATIC_ISAM], [Build ISAM into libcob])

AH_TEMPLATE([WITH_XML2], [Use libxml2 as XML handler])
AH_TEMPLATE([WITH_JSON], [JSON handler])
AH_TEMPLATE([WITH_CJSON], [Use cJSON library/source as JSON handler])
AH_TEMPLATE([WITH_JSON_C], [Use JSON-C library as JSON handler])

AH_TEMPLATE([COB_EXPORT_DYN], [Compile/link option for exporting symbols])
AH_TEMPLATE([COB_PIC_FLAGS], [Compile/link option for PIC code])
AH_TEMPLATE([COB_DEBUG_FLAGS], [Compile/link option for debugging])
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
AH_TEMPLATE([COB_COMPUTED_GOTO], [Compilation of computed gotos works])
AH_TEMPLATE([COB_LI_IS_LL], [long int is long long])
AH_TEMPLATE([COB_32_BIT_LONG], [long int is 32 bits])
AH_TEMPLATE([COB_64_BIT_POINTER], [Pointers are longer than 32 bits])
AH_TEMPLATE([WITH_CURSES], [curses library for extended SCREEN I/O])
AH_TEMPLATE([HAVE_COLOR_SET], [curses has color_set function])
AH_TEMPLATE([HAVE_DEFINE_KEY], [curses has define_key function])
AH_TEMPLATE([HAVE_MOUSEINTERVAL], [curses has mouseinterval function])
AH_TEMPLATE([HAVE_HAS_MOUSE], [curses has has_mouse function])
AH_TEMPLATE([HAVE_CURSES_FREEALL], [ncurses has _nc_freeall function])
AH_TEMPLATE([HAVE_USE_LEGACY_CODING], [ncurses has use_legacy_coding function])
AH_TEMPLATE([HAVE_DESIGNATED_INITS], [Has designated initializers])
AH_TEMPLATE([HAVE_NANO_SLEEP], [Has nanosleep function])
AH_TEMPLATE([HAVE_CLOCK_GETTIME], [Has clock_gettime function and CLOCK_REALTIME])
AH_TEMPLATE([HAVE_ISFINITE], [Has isfinite function])
AH_TEMPLATE([HAVE_MP_GET_MEMORY_FUNCTIONS], [Do we have mp_get_memory_functions in GMP/MPIR])
#AH_TEMPLATE([HAVE_RAISE], [Has raise function])
AH_TEMPLATE([HAVE_FINITE_IEEEFP_H],
	[Declaration of finite function in ieeefp.h instead of math.h])

# preparation for cross-compilation
AC_ARG_PROGRAM

# Configure options part I (not needing any compilation)

AC_ARG_ENABLE([debug],
  [AS_HELP_STRING([--enable-debug],
    [Enable -g C compiler debug option for cobc + libcob])],
  [],
  [enable_debug=no])

AC_ARG_ENABLE([hardening],
  [AS_HELP_STRING([--enable-hardening, --disable-hardening],
    [Enable GNU C "hardening" options: define _FORTIFY_SOURCE and use -fstack-protector.
	If disabled, these are explicit removed from CFLAGS for building GnuCOBOL.
	Defaults to "defined by CFLAGS".])],,
  [enable_hardening="unset"])

AC_ARG_ENABLE([experimental],
  [AS_HELP_STRING([--enable-experimental],
    [Enable experimental code (Developers only!)])],
  [if test "$enable_experimental" = "yes"; then
	AC_DEFINE([COB_EXPERIMENTAL], [1])
   fi],
  [])

AC_ARG_ENABLE([cobc-internal-checks],
  [AS_HELP_STRING([--enable-cobc-internal-checks],
    [Enable extra checks within the compiler (Developers only!)])],
  [if test "$enable_cobc_internal_checks" = "yes"; then
	AC_DEFINE([COB_TREE_DEBUG], [1])
   fi],
  [])

AC_ARG_ENABLE([debuglog],
  [AS_HELP_STRING([--enable-debuglog],
    [Enable internal logging code (Developers only!)])],
  [if test "$enable_debuglog" = "yes"; then
	AC_DEFINE([COB_DEBUG_LOG], [1])
   fi],
  [])

AC_ARG_ENABLE([param-check],
  [AS_HELP_STRING([--enable-param-check],
    [Enable minimum parameter check for system libraries (default no)])],
  [if test "$enable_param_check" = "yes"; then
	AC_DEFINE([COB_PARAM_CHECK], [1])
   fi],
  [])

AC_ARG_WITH([patch-level],
  [AS_HELP_STRING([--with-patch-level],
    [Define a patch level (default 0), numeric, max. 8 digits])],
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
  [with_patch_level=0])

AC_DEFINE_UNQUOTED([PATCH_LEVEL], [$with_patch_level])

AC_ARG_WITH([max-call-params],
  [AS_HELP_STRING([--with-max-call-params],
    [Define maximum number of parameters for CALL (default 192)])],
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

configured_make=""
if test -z "$MAKE"; then
  AC_CHECK_PROGS([configured_make], [make gmake gnumake])
  if test -z "$configured_make"; then
	AC_MSG_ERROR([make not found])
  else
	MAKE=$configured_make
	AC_SUBST(MAKE)
  fi
else
  AC_CHECK_PROG([configured_make], [$MAKE], [$MAKE])
  if test -z "$configured_make"; then
	AC_MSG_ERROR([make, configured as "$MAKE", was not found])
  fi
fi

AC_MSG_CHECKING([whether ${MAKE} supports order-only prerequisites])
rm -f confinc.* confprereq
cat > confmf.mk << 'END'
PREREQ := confprereq
am__doit:  am__prereq | $(PREREQ)
	@echo target am__doit. >>confinc.out
am__prereq:
	@echo target am__prereq. >>confinc.out
$(PREREQ):
	@touch $(PREREQ)
	@echo target $(PREREQ). >>confinc.out
.PHONY: am__doit am__prereq
END
_am_result=no
  AM_RUN_LOG([${MAKE} -f confmf.mk && ${MAKE-make} -f confmf.mk && cat confinc.out])
  ## redirecting via echo to remove special chars
  __am_checkme=$(echo `cat confinc.out 2>/dev/null`)
  AS_CASE([$?:${__am_checkme}],
      ['0:target am__prereq. target confprereq. target am__doit. target am__prereq. target am__doit.'],
      [_am_result="yes"])
rm -f confinc.* confmf.mk confprereq
AC_MSG_RESULT([${_am_result}])
AM_CONDITIONAL([MAKE_HAS_PREREQ_ONLY], [test "${_am_result}" = "yes"])

curr_libs="$LIBS"

enable_cflags_setting=no
AC_MSG_CHECKING([whether CFLAGS -O can be modified])
# Enable setting if the user has not specified the optimisation in CFLAGS.
# Checked before AC_PROG_CC and default flags are applied.
echo "$CFLAGS" | grep "\-O.*\( \|$\)" 1>/dev/null 2>/dev/null
if test $? != 0; then
	enable_cflags_setting=yes
fi
AC_MSG_RESULT([$enable_cflags_setting])

# Default CFLAGS (removed -g set in AC_INIT for compatibility)
: ${CFLAGS="-O2"}

AC_PROG_CC([gcc xlc cc])
AC_PROG_CC_STDC

AC_PROG_CPP

AC_USE_SYSTEM_EXTENSIONS

# general flags for compiler and linker and tests for those
AC_ARG_VAR([LIBCOB_CPPFLAGS], [see CPPFLAGS, but only applied during generation of libcob])
AS_IF([test "x$LIBCOB_CPPFLAGS" != "x"], [
	AC_MSG_CHECKING([if compilation with LIBCOB_CPPFLAGS works])
	curr_cppflags="$CPPFLAGS"
	CPPFLAGS="$CPPFLAGS $LIBCOB_CPPFLAGS"
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],
		[[static int i = 1;]])],
		[AC_MSG_RESULT([yes])],
		[AC_MSG_RESULT([no])
		AC_MSG_ERROR([not possible to compile with LIBCOB_CPPFLAGS="$LIBCOB_CPPFLAGS"])])
	CPPFLAGS="$curr_cppflags"
])

curr_libs="$LIBS"
AC_ARG_VAR([LIBCOB_LIBS], [see LIBS, but only applied during generation of libcob])
AS_IF([test "x$LIBCOB_LIBS" != "x"], [
	AC_MSG_CHECKING([if linking with LIBCOB_LIBS works])
	LIBS="$LIBS $LIBCOB_LIBS"
	AC_LINK_IFELSE([AC_LANG_PROGRAM([],
		  [[return 0;]])],
		  [AC_MSG_RESULT([yes])],
		  [AC_MSG_RESULT([no])
		   AC_MSG_ERROR([not possible to link with LIBCOB_LIBS="$LIBCOB_LIBS"])])
])

AC_ARG_VAR([PROGRAMS_LIBS], [see LIBS, but only applied during generation of programs: cobc, cobcrun])
AS_IF([test "x$PROGRAMS_LIBS" != "x"], [
	AC_MSG_CHECKING([if linking with PROGRAMS_LIBS works])
	LIBS="$LIBS $PROGRAMS_LIBS"
	AC_LINK_IFELSE([AC_LANG_PROGRAM([],
		  [[return 0;]])],
		  [AC_MSG_RESULT([yes])],
		  [AC_MSG_RESULT([no])
		   AC_MSG_ERROR([not possible to link with PROGRAMS_LIBS="$PROGRAMS_LIBS"])])
])
LIBS="$curr_libs"


LT_INIT([dlopen win32-dll])

AC_PROG_EGREP
AM_PROG_CC_C_O

AM_PROG_LEX
AC_PROG_YACC

# only GNU Bison 3+ is supported, so drop yacc-compatibility warnings
# TODO: check for Bison 3+ here
AC_MSG_CHECKING([how to disable yacc compatibility warnings])
case "$YACC $YFLAGS $AM_YFLAGS" in
  *-Wno-yacc*)
   AC_MSG_RESULT([already specified])
  ;;
  *-Wyacc*)
   AC_MSG_RESULT([explicit enabled])
  ;;
  *)
   $YACC $AM_YFLAGS $YFLAGS -Wno-yacc -V 1>/dev/null  2>&1
   if test $? -eq 0; then
      AC_MSG_RESULT([-Wno-yacc])
      if test -z "$YFLAGS"; then
         YFLAGS="-Wno-yacc"
      else
         YFLAGS="$YFLAGS -Wno-yacc"
      fi
   else
      AC_MSG_RESULT([not supported])
   fi
  ;;
esac

AC_ARG_VAR(DIFF_FLAGS, arguments passed to diff)
AC_MSG_CHECKING([for diff arguments])
case "$DIFF_FLAGS" in
  *--strip-trailing-cr*)
   AC_MSG_RESULT([specified as $DIFF_FLAGS])
  ;;
  *)
   diff $DIFF_FLAGS --strip-trailing-cr --version 1>/dev/null  2>&1
   if test $? -eq 0; then
      if test -z "$DIFF_FLAGS"; then
         DIFF_FLAGS="--strip-trailing-cr"
      else
         DIFF_FLAGS="$DIFF_FLAGS --strip-trailing-cr"
      fi
      AC_MSG_RESULT([$DIFF_FLAGS])
   else
      AC_MSG_RESULT([not supported])
   fi
  ;;
esac
AC_SUBST(DIFF_FLAGS)

# Stop tests for C++ and Fortran
AC_DEFUN([AC_PROG_F77], [])
AC_DEFUN([AC_PROG_CXX], [])
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
	stddef.h wchar.h dlfcn.h sys/time.h sys/wait.h sys/sysmacros.h])


# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_BIGENDIAN
AC_TYPE_SIZE_T
AC_STRUCT_TM
AC_CHECK_TYPES([sig_atomic_t], [], [], [[#include <signal.h>]])

# Don't use AC_C_INLINE here. We need the value

AC_MSG_CHECKING([for inline keyword])
for cob_keyw in __inline __inline__ inline
do
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
		#ifndef __cplusplus
		typedef int foo_t;
		static $cob_keyw foo_t foo () { return 0; }
		#endif]], [[
		#ifndef __cplusplus
		return foo ();
		#else
		choke me
		#endif]])],
		[AC_DEFINE_UNQUOTED([COB_KEYWORD_INLINE], [$cob_keyw]) break],
		[],
		[])
done
AC_MSG_RESULT([$cob_keyw])
unset cob_keyw


# Checks for library functions.
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([memmove memset setlocale fcntl strerror strcasecmp \
	strchr strrchr strdup strstr atol strtol atoll strtoll gettimeofday localeconv \
	getexecname canonicalize_file_name popen raise readlink realpath \
	setenv strcoll flockfile])

# Check for timezone
AC_MSG_CHECKING([for timezone variable access])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <time.h>]],
	[[timezone = 3600;]])],
	[AC_DEFINE([HAVE_TIMEZONE], [1])
	 AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])])

# Check for designated initializers
AC_MSG_CHECKING([for designated initializers])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <stdio.h>]],
	[[static const unsigned char valid_char[256] = {
		@<:@'0'@:>@ = 1,
		@<:@'1'@:>@ = 1 };
	]])],
	[AC_DEFINE([HAVE_DESIGNATED_INITS], [1]) AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])])

# Check gcc/icc/clang
COB_USES_GCC="no"
COB_USES_GCC_NO_ICC="no"
COB_USES_ICC_ONLY="no"
COB_USES_CLANG_ONLY="no"
COB_USES_XLC_ONLY="no"
COB_USES_WATCOMC_ONLY="no"

AC_MSG_CHECKING([for __GNUC__])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
	#ifndef __GNUC__
	# error macro not defined
	#endif]])],
	[COB_USES_GCC="yes"
	 AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])])

AC_MSG_CHECKING([for __INTEL_COMPILER])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
	#ifndef __INTEL_COMPILER
	# error macro not defined
	#endif]])],
	[COB_USES_ICC_ONLY=yes
	 AC_MSG_RESULT([yes])],
	[if test "$COB_USES_GCC" = "yes"; then
		COB_USES_GCC_NO_ICC="yes"
	 fi
	 AC_MSG_RESULT([no])]
	)

AC_MSG_CHECKING([for __clang__])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
	#ifndef __clang__
	# error macro not defined
	#endif]])],
	[COB_USES_CLANG_ONLY="yes"
	 AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])])

AC_MSG_CHECKING([for __xlc__])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
	#ifndef __xlc__
	# error macro not defined
	#endif]])],
	[COB_USES_XLC_ONLY="yes"
	 AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])])

AC_MSG_CHECKING([for __WATCOMC__])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
	#ifndef __WATCOMC__
	# error macro not defined
	#endif]])],
	[COB_USES_WATCOMC_ONLY="yes"
	 AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])])

if test "x$COB_CC" = "x"; then
	COB_CC="$CC"
fi

# FIXME: -lm should only be used if needed (which it often is not), see Bug #559
if test "$COB_USES_ICC_ONLY" = "yes"; then
	COMMON_LIBS="-limf -lm"
else
	COMMON_LIBS="-lm"
fi
if test "x$LIBCOB_LIBS" != "x"; then
	LIBCOB_LIBS="$LIBCOB_LIBS $COMMON_LIBS"
else
	LIBCOB_LIBS="$COMMON_LIBS"
fi

COB_LIBS="-L$libdir -lcob"
if test "x$COMMON_LIBS" != "x"; then
	if test "x$COB_LIBS" != "x"; then
		COB_LIBS="$COB_LIBS $COMMON_LIBS"
	else
		COB_LIBS="$COMMON_LIBS"
	fi
fi
if test "x$LDFLAGS" != "x"; then
	if test "x$COB_LDFLAGS" != "x"; then
		COB_LDFLAGS="$COB_LDFLAGS $LDFLAGS"
	else
		COB_LDFLAGS="$LDFLAGS"
	fi
fi

# Add --enable-code-coverage and test for code-coverage tools if enabled
AX_CODE_COVERAGE

if test "$COB_USES_GCC_NO_ICC" != yes -a "$enable_code_coverage" = "yes"; then
	AC_MSG_ERROR([Code coverage checks are only usable with GCC!])
fi


# set PKG_CONFIG to use (cross-compile aware)
PKG_PROG_PKG_CONFIG


# Checks for gmp.
AC_MSG_NOTICE([Checks for GMP/MPIR ...])

AC_ARG_WITH([math],
  [AS_HELP_STRING([[--with-math[=ARG]]],
    [Use math multiple precision library, where ARG may be:
     check (default), gmp, mpir])],
  [case "$with_math" in
    gmp | mpir | check)
		USE_MATH="$with_math"
		;;
	yes)
		USE_MATH="check"
		;;
	no)
		AC_MSG_ERROR([Cannot build GnuCOBOL without math multiple precision library)])
		;;
    *)
		AC_MSG_ERROR([invalid value "$with_math" for --with-math,
  must be one of the following:
  gmp, mpir (use only the specified library)
  check  (use whatever library is usable)])
		;;
  esac],
  [USE_MATH="check"])

curr_libs=$LIBS
curr_cppflags=$CPPFLAGS

AS_IF([test "$USE_MATH" = "gmp" -o "$USE_MATH" = "check"], [
	curr_libs="$LIBS"; curr_cppflags="$CPPFLAGS"
	PKG_CHECK_MODULES([GMP], [gmp], [], [
		PKG_CHECK_MODULES([GMP], [libgmp], [], [#
		])
	])
	if test -n "$GMP_CFLAGS"; then
	  CPPFLAGS="$CPPFLAGS $GMP_CFLAGS"
	fi
	if test -z "${GMP_LIBS+x}"; then
	  GMP_LIBS="-lgmp"
	fi
	LIBS="$LIBS $GMP_LIBS"
	AC_CHECK_HEADERS([gmp.h], [
		AC_MSG_CHECKING([if linking against libgmp with "$GMP_LIBS" works])
		AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <gmp.h>]],
		  [[mpz_t integ; mpz_init (integ);]])],
		  [AC_MSG_RESULT([yes])
		   curr_cppflags=$CPPFLAGS   # for the include in cobc generated modules
		   LIBCOB_LIBS="$LIBCOB_LIBS $GMP_LIBS"
		   USE_MATH=gmp],
		  [AC_MSG_RESULT([no])
		   if test "$USE_MATH" = "gmp"; then
		     AC_MSG_ERROR([[GMP is required for --with-math=gmp, you may adjust GMP_LIBS]])
		   fi
		   LIBS="$curr_libs"; CPPFLAGS="$curr_cppflags"]
		)
	], [if test "$USE_MATH" = "gmp"; then
		    AC_MSG_ERROR([gmp.h is required for --with-math=gmp, you may adjust GMP_CFLAGS])
		 fi
		 LIBS="$curr_libs"; CPPFLAGS="$curr_cppflags"
	])
])

AS_IF([test "$USE_MATH" = "mpir" -o "$USE_MATH" = "check"], [
	# note: currently MPIR does not ship a .pc file, but is expected to follow GMP
	curr_libs="$LIBS"; curr_cppflags="$CPPFLAGS"
	PKG_CHECK_MODULES([MPIR], [mpir], [], [#
	])
	if test -n "$MPIR_CFLAGS"; then
	  CPPFLAGS="$CPPFLAGS $MPIR_CFLAGS"
	fi
	if test -z "${MPIR_LIBS+x}"; then
	  GMP_LIBS="-lmpir"
	fi
	LIBS="$LIBS $MPIR_LIBS"
	AC_CHECK_HEADERS([mpir.h], [
		AC_MSG_CHECKING([if linking against libmpir with "$MPIR_LIBS" works])
		AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <mpir.h>]],
		  [[mpz_t integ; mpz_init (integ);]])],
		  [AC_MSG_RESULT([yes])
		   curr_cppflags=$CPPFLAGS   # for the include in cobc generated modules
		   LIBCOB_LIBS="$LIBCOB_LIBS $MPIR_LIBS"
		   USE_MATH=mpir],
		  [AC_MSG_RESULT([no])
		   if test "$USE_MATH" = "mpir"; then
		     AC_MSG_ERROR([[MPIR is required for --with-math=mpir, you may adjust GMP_LIBS]])
		   fi
		   LIBS="$curr_libs"; CPPFLAGS="$curr_cppflags"]
		)
	], [if test "$USE_MATH" = "mpir"; then
		    AC_MSG_ERROR([mpir.h is required for --with-math=mpir, you may adjust GMP_CFLAGS])
		 fi
		 LIBS="$curr_libs"; CPPFLAGS="$curr_cppflags"
	])
])

if test "$USE_MATH" = "check"; then
	AC_MSG_ERROR([neither GMP nor MPIR library are found, but one of those is mandatory])
fi

AC_MSG_CHECKING([matching GMP version])
# Check just major/minor levels between header and library
# get GMP version from lib
AC_RUN_IFELSE([AC_LANG_SOURCE([[
	#include <stdio.h>
	#ifdef _WIN32
	#ifndef __GMP_LIBGMP_DLL
	#define __GMP_LIBGMP_DLL 1
	#endif
	#endif
	#if defined HAVE_GMP_H
	#include <gmp.h>
	#elif defined HAVE_MPIR_H
	#include <mpir.h>
	#endif
	int main (int argc, char **argv)
	{
		(void)argv;
		if (argc > 1)
			printf ("%s", gmp_version);
		return 0;
	}
	]])],
	[COB_GMP_LIB=$(./conftest$ac_exeext x)],
	[AC_MSG_ERROR([Unable to extract GMP version information from gmp_version])],
	[AC_MSG_WARN([matching GMP version assumed])]
	 COB_GMP_LIB="cross")
if test "x$COB_GMP_LIB" = "x"; then
	AC_MSG_ERROR([Unable to extract GMP version information (library)])
fi

# get GMP version from header
AC_RUN_IFELSE([AC_LANG_SOURCE([[
	#include <stdio.h>
	#if defined HAVE_GMP_H
	#include <gmp.h>
	#elif defined HAVE_MPIR_H
	#include <mpir.h>
	#endif
	int main (int argc, char **argv)
	{
		(void)argv;
		if (argc > 1)
			printf ("%d.%d", __GNU_MP_VERSION, __GNU_MP_VERSION_MINOR);
		return 0;
	}
	]])],
	[COB_GMP_HEADER=$(./conftest$ac_exeext x)],
	[AC_MSG_ERROR([Unable to extract GMP version information from header])],
	[AC_MSG_WARN([cannot run test program while cross-compiling])
	 COB_GMP_HEADER="cross-compilation - assumed"])
if test "x$COB_GMP_HEADER" = "x"; then
	AC_MSG_ERROR([Unable to extract GMP version information (header)])
fi

COB_GMP_LIB_MAJOR=$(echo "$COB_GMP_LIB" | cut -d. -f1)
COB_GMP_LIB_MINOR=$(echo "$COB_GMP_LIB" | cut -d. -f2)

if test "$COB_GMP_HEADER" = "$COB_GMP_LIB_MAJOR.$COB_GMP_LIB_MINOR" -o "$COB_GMP_LIB" = "cross"; then
	AC_MSG_RESULT([yes ($COB_GMP_HEADER)])
else
	AC_MSG_RESULT([no (header: $COB_GMP_HEADER / library: $COB_GMP_LIB)])
	AC_MSG_ERROR([Unable to use GMP - Please check config.log])
fi

LIBS="$curr_libs"
CPPFLAGS="$curr_cppflags"

AC_CHECK_LIB([$USE_MATH], [__gmp_get_memory_functions],
 [AC_DEFINE([HAVE_MP_GET_MEMORY_FUNCTIONS], [1])], [], [])


# Configure options part II (needing compilation)
AC_ARG_WITH([xml2],
  [AS_HELP_STRING([--with-xml2],
    [Use libxml2 as XML handler (default)])],
  [],
  [with_xml2=check])

AS_IF([test "$with_xml2" = "yes" -o "$with_xml2" = "check"], [
	PKG_CHECK_MODULES([XML2], [libxml-2.0], [], [
	if test -z "${XML2_CFLAGS+x}" -o -z "${XML2_LIBS+x}"; then
	  AC_CHECK_PROG(xml2_config_found, xml2-config, "yes")
	  if test "$xml2_config_found" = "yes"; then
	    if test -z "${XML2_CFLAGS+x}"; then
	      XML2_CFLAGS="$(xml2-config --cflags)"
	    fi
	    if test -z "${XML2_LIBS+x}"; then
	      XML2_LIBS="$(xml2-config --libs)"
	    fi
	  fi
	fi])
	curr_libs="$LIBS"; curr_cppflags="$CPPFLAGS"
	if test -n "$XML2_CFLAGS"; then
	  CPPFLAGS="$CPPFLAGS $XML2_CFLAGS"
	fi
	if test -z "${XML2_LIBS+x}"; then
	  XML2_LIBS="-lxml2"
	fi
	LIBS="$LIBS $XML2_LIBS"
	for header in xmlversion uri xmlwriter
	do
		AC_CHECK_HEADER([libxml/$header.h], [],
			[if test "$with_xml2" = "yes"; then
			   AC_MSG_ERROR([Headers for libxml2 are required for --with-xml2, you may adjust XML2_CFLAGS])
			else
			   with_xml2=not_found
			   break
			fi]
		)
	done
	if test "$with_xml2" != "not_found"; then
		AX_CHECK_DEFINE([libxml/xmlwriter.h], [LIBXML_WRITER_ENABLED], [],
		   [if test "$with_xml2" = "yes"; then
		      AC_MSG_ERROR([libxml2 is required to be configured with xmlWriter])
		    else
		      AC_MSG_WARN([libxml2 is required to be configured with xmlWriter])
		      with_xml2=not_found
		    fi]
		)
	fi
	if test "$with_xml2" != "not_found"; then
		AC_MSG_CHECKING([if linking against libxml2 with "$XML2_LIBS" works])
		AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <libxml/xmlwriter.h>]],
		  [[xmlNewTextWriterFilename (NULL, 0);]])],
		  [AC_DEFINE([WITH_XML2], [1]) AC_MSG_RESULT([yes])
		   with_xml2=yes
		   LIBCOB_CPPFLAGS="$LIBCOB_CPPFLAGS $XML2_CFLAGS"
		   LIBCOB_LIBS="$LIBCOB_LIBS $XML2_LIBS"],
		  [AC_MSG_RESULT([no])
		   if test "$with_xml2" = "yes"; then
		     AC_MSG_ERROR([[xml library is required for --with-xml2, you may adjust XML2_LIBS]])
		   else
		     AC_MSG_WARN([xml library not found, you may adjust XML2_LIBS])
		     with_xml2=not_found
		   fi])
	else
		AC_MSG_WARN([Headers for libxml2 missing, you may adjust XML2_CFLAGS])
		with_xml2=not_found
	fi
	LIBS="$curr_libs"; CPPFLAGS="$curr_cppflags"
])



# Checks for cjson/json-c.
AC_MSG_NOTICE([Checks for JSON handler])

AC_ARG_WITH([json],
  [AS_HELP_STRING([[--with-json[=ARG]]],
    [Use json library/source as JSON handler, where ARG may be:
     check (default), cjson, local, json-c, no])],
  [case "$with_json" in
	cjson | local | "json-c" | check | no )
		USE_JSON="$with_json"
		;;
	yes)
		USE_JSON="check"
		;;
    *)
		AC_MSG_ERROR([invalid value "$with_json" for --with-json,
  must be one of the following:
  cjson, json-c  (use only the specified library)
  local  (use cjson source included in lib)
  check  (use whatever json library is usable, disable if no one usable)
  no  (disable json usage)])
		;;
  esac],
  [USE_JSON="check"])

curr_libs="$LIBS"
AS_IF([test "$USE_JSON" = "cjson" -o "$USE_JSON" = "local" -o "$USE_JSON" = "check"], [
	AC_MSG_NOTICE([Checks for local cJSON ...])
	curr_libs="$LIBS"; curr_cppflags="$CPPFLAGS"
	with_cjson_local=no
	AC_CHECK_FILE([./lib/cJSON.c],
	  [AC_MSG_CHECKING([if linking of ./lib/cJSON.c works])
	   CPPFLAGS="$curr_cppflags -I./lib"
	   LIBS="$LIBS $COMMON_LIBS"
	   AC_LINK_IFELSE([
		AC_LANG_PROGRAM([[#include "cJSON.c"]],
	                   [[#if (CJSON_VERSION_MAJOR * 100 + CJSON_VERSION_MINOR) < 103
                        	#error Needs at least cJSON 1.3.0
					     #endif
						 cJSON_CreateNull ();]])],
	   [AC_MSG_RESULT([yes])
	    AC_DEFINE([WITH_CJSON], [1])
	    with_cjson_local="yes (in ./lib)"],
	   [AC_MSG_RESULT([no])]
	   )
	   LIBS="$curr_libs"]
	)
	if test "$with_cjson_local" = "no"; then
	  AC_CHECK_FILE([$srcdir/lib/cJSON.c],
	    [AC_MSG_CHECKING([if linking of $srcdir/lib/cJSON.c works])
	     CPPFLAGS="$curr_cppflags -I$srcdir/lib"
	     LIBS="$LIBS $COMMON_LIBS"
	     AC_LINK_IFELSE([
		  AC_LANG_PROGRAM([[#include "cJSON.c"]],
	                     [[#if (CJSON_VERSION_MAJOR * 100 + CJSON_VERSION_MINOR) < 103
                         	  #error Needs at least cJSON 1.3.0
					       #endif
						   cJSON_CreateNull ();]])],
	     [AC_MSG_RESULT([yes])
	      AC_DEFINE([WITH_CJSON], [1])
	      with_cjson_local="yes (in $srcdir/lib)"],
	     [AC_MSG_RESULT([no])]
	     )
	     LIBS="$curr_libs"]
	  )
	fi
	CPPFLAGS="$curr_cppflags"
	AS_IF([test "$with_cjson_local" = "no"], [
		AC_MSG_NOTICE([using local cJSON: no])
		if test "$with_json" = "local"; then
		  AC_MSG_ERROR([[cJSON source is required in directory "lib" for --with-json=local]])
		fi
		PKG_CHECK_MODULES([CJSON], [libcjson], [], [#
		])
		if test -n "$CJSON_CFLAGS"; then
		  CPPFLAGS="$CPPFLAGS $CJSON_CFLAGS"
		fi
		if test -z "${CJSON_LIBS+x}"; then
		  CJSON_LIBS="-lcjson"
		fi
		LIBS="$LIBS $CJSON_LIBS"
		AC_CHECK_HEADERS([cJSON.h], [],
			[AC_CHECK_HEADERS([cjson/cJSON.h], [],
			[if test "$with_json" = "cjson"; then
			    AC_MSG_ERROR([Headers for libcjson are required for --with-json, you may adjust CJSON_CFLAGS])
			 else
				AC_MSG_WARN([Headers for libcjson missing, you may adjust CJSON_CFLAGS or put cJSON sources in "lib"])
			 fi]
		)])
		if test "x$ac_cv_header_cJSON_h" = "xyes" -o "x$ac_cv_header_cjson_cJSON_h" = "xyes"; then
			AC_MSG_CHECKING([if linking against libcjson with "$CJSON_LIBS" works])
			AC_LINK_IFELSE([AC_LANG_PROGRAM([
			 [#if defined HAVE_CJSON_CJSON_H
			  #include <cjson/cJSON.h>
			  #elif defined HAVE_CJSON_H
			  #include <cJSON.h>
			  #endif
			  #if (CJSON_VERSION_MAJOR * 100 + CJSON_VERSION_MINOR) < 103
               	#error Needs at least cJSON 1.3.0
			  #endif]],
			  [[cJSON_CreateNull ();]])],
			  [AC_MSG_RESULT([yes])
			   AC_DEFINE([WITH_CJSON], [1])
			   USE_JSON=cjson
			   LIBCOB_CPPFLAGS="$LIBCOB_CPPFLAGS $CJSON_CFLAGS"
			   LIBCOB_LIBS="$LIBCOB_LIBS $CJSON_LIBS"],
			  [AC_MSG_RESULT([no])
			   if test "$with_json" = "cjson"; then
			     AC_MSG_ERROR([[cJSON library is required for --with-json=cjson, you may adjust CJSON_LIBS]])
			   else
			     AC_MSG_WARN([cJSON library not found, you may adjust CJSON_LIBS])
			   fi])
		fi
	], [
		AC_MSG_NOTICE([using local cJSON: $with_cjson_local])
		USE_JSON="local"
		AC_DEFINE([HAVE_CJSON_H], [1])
	])
	LIBS="$curr_libs"; CPPFLAGS="$curr_cppflags"
])

AS_IF([test "$USE_JSON" = "json-c" -o "$USE_JSON" = "check"],  [
	CPPFLAGS="$curr_cppflags"
	PKG_CHECK_MODULES([JSON_C], [json-c], [], [#
	])
	# possibly set JSON_C_CFLAGS with $includedir/json-c;
	# (has to consider user-setprefix --includir, ... -> left out for now)
	if test -n "$JSON_C_CFLAGS"; then
	  CPPFLAGS="$CPPFLAGS $JSON_C_CFLAGS"
	fi
	if test -z "${JSON_C_LIBS+x}"; then
	  JSON_C_LIBS="-ljson-c"
	fi
	LIBS="$LIBS $JSON_C_LIBS"
	AC_CHECK_HEADER([json.h], [
	 AC_MSG_CHECKING([if linking against libjson-c with "$JSON_C_LIBS" works])
	 AC_LINK_IFELSE([AC_LANG_PROGRAM([
	 [#include <json.h>]],
	  [[#if (JSON_C_MAJOR_VERSION * 100 + JSON_C_MINOR_VERSION) < 12
           	#error Needs at least json-c 0.12
		#endif
		json_c_version ();]])],
	  [AC_MSG_RESULT([yes])
	   AC_DEFINE([WITH_JSON_C], [1])
	   USE_JSON=json-c
	   LIBCOB_CPPFLAGS="$LIBCOB_CPPFLAGS $JSON_C_CFLAGS"
	   LIBCOB_LIBS="$LIBCOB_LIBS $JSON_C_LIBS"],
	  [AC_MSG_RESULT([no])
	   if test "$USE_JSON" = "json-c"; then
	     AC_MSG_ERROR([[JSON-C library is required for --with-json-c, you may adjust JSON_C_LIBS]])
	   else
	     AC_MSG_WARN([JSON-C not found, you may adjust JSON_C_LIBS])
	   fi])
	],
	[if test "$USE_JSON" = "json-c"; then
		AC_MSG_ERROR([Headers for libjson-c are required for --with-json-c, you may adjust JSON_C_CFLAGS])
	 else
		AC_MSG_WARN([Headers for libjson-c missing, you may adjust JSON_C_CFLAGS"])
	 fi])
	LIBS="$curr_libs"; CPPFLAGS="$curr_cppflags"
])

case "$USE_JSON" in
  check)
	if test "$with_json" = yes; then
		AC_MSG_ERROR([[No applicable handler for JSON found]])
	fi
	USE_JSON="not_found"
	AC_DEFINE([WITH_JSON], ["not found"])
	;;
  no)
	AC_DEFINE([WITH_JSON], ["disabled"])
	;;
  cjson | local)
	AC_DEFINE([WITH_JSON], ["cjson"])
	;;
  "json-c")
	AC_DEFINE([WITH_JSON], ["json-c"])
	;;
esac


AC_ARG_WITH([dl],
  [AS_HELP_STRING([--with-dl],
    [Use system dynamic loader (default)])],
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
    [Define variable sequential format (default 0)])],
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
  [with_varseq=0])

AC_DEFINE_UNQUOTED([WITH_VARSEQ], [$with_varseq])


# Solaris has nanosleep in other libraries
AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <time.h>]],
  [[nanosleep (NULL, NULL);]])],
  [AC_DEFINE([HAVE_NANO_SLEEP], [1])],
  [AC_CHECK_LIB([rt], [nanosleep], [], [], [])
   if test "x$ac_cv_lib_rt_nanosleep" = "xyes"; then
     AC_DEFINE([HAVE_NANO_SLEEP], [1])
     LIBCOB_LIBS="$LIBCOB_LIBS -lrt"
   else
     AC_CHECK_LIB([posix4], [nanosleep], [], [], [])
     if test "x$ac_cv_lib_posix4_nanosleep" = "xyes"; then
       AC_DEFINE([HAVE_NANO_SLEEP], [1])
       LIBCOB_LIBS="$LIBCOB_LIBS -lposix4"
     fi
   fi])

AC_MSG_CHECKING([for clock_gettime and CLOCK_REALTIME])
AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <time.h>]],
  [[clock_gettime (CLOCK_REALTIME, NULL);]])],
  [AC_DEFINE([HAVE_CLOCK_GETTIME], [1]) AC_MSG_RESULT([yes])],
  [AC_MSG_RESULT([no])])

AC_MSG_CHECKING([for isfinite])
AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <math.h>]],
  [[isfinite ( 1.0 );]])],
  [AC_DEFINE([HAVE_ISFINITE], [1]) AC_MSG_RESULT([yes])],
  [AC_MSG_RESULT([no])]
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
)

# Check for raise (optional) --> done via AC_CHECK_FUNCS
#AC_MSG_CHECKING([for raise])
#AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <signal.h>]],
#  [[raise (SIGINT);]])],
#  [AC_DEFINE([HAVE_RAISE], [1]) AC_MSG_RESULT([yes])],
#  [AC_MSG_RESULT([no])],
#  [])

AC_CHECK_FUNCS([fdatasync sigaction fmemopen])
AC_CHECK_DECLS([fdatasync])	# also check for declaration, missing on MacOS...
AC_CHECK_DECLS([fmemopen])	# also check for declaration, missing on AIX...

# Checks for gettext.

case $host_os in
  darwin* | rhapsody*)
	;;
  *)
	gt_cv_func_CFPreferencesCopyAppValue=no
	gt_cv_func_CFLocaleCopyCurrent=no
	;;
esac

# Checks for ncurses/pdcurses/curses.
AC_MSG_NOTICE([Checks for curses ...])

AC_ARG_WITH([curses],
  [AS_HELP_STRING([[--with-curses[=ARG]]],
    [Use curses library for extended SCREEN I/O, where ARG may be:
     check (default), ncursesw, ncurses, pdcurses, curses, no])],
  [case "$with_curses" in
    ncursesw | ncurses | pdcurses | curses | check | no)
		USE_CURSES="$with_curses"
		;;
	yes)
		USE_CURSES="check"
		;;
    *)
		AC_MSG_ERROR([invalid value "$with_curses" for --with-curses,
  must be one of the following:
  ncursesw, ncurses, pdcurses, curses  (use only the specified library)
  check  (use whatever curses library is usable, disable if no one usable)
  no  (disable curses usage)])
		;;
  esac],
  [USE_CURSES="check"])

curr_libs="$LIBS"
if test "$USE_CURSES" = "ncursesw" -o "$USE_CURSES" = "check"; then
  AC_CHECK_LIB([ncursesw], [initscr], [], [], [])
  if test "x$ac_cv_lib_ncursesw_initscr" = "xyes"; then
    AC_CHECK_HEADERS([ncursesw/ncurses.h], [USE_CURSES="ncursesw"],
      [AC_CHECK_HEADERS([ncursesw/curses.h], [USE_CURSES="ncursesw"],
        [if test "$USE_CURSES" = "ncursesw"; then
           USE_CURSES="missing_header"
         fi], [])], [])
    if test "$USE_CURSES" = "ncursesw"; then
      LIBCOB_LIBS="$LIBCOB_LIBS -lncursesw"
    fi
  else
    if test "$USE_CURSES" = "ncursesw"; then
      USE_CURSES="missing_lib"
    fi
  fi
fi

if test "$USE_CURSES" = "ncurses" -o "$USE_CURSES" = "check"; then
  AC_CHECK_LIB([ncurses], [initscr], [], [], [])
  if test "x$ac_cv_lib_ncurses_initscr" = "xyes"; then
    AC_CHECK_HEADERS([ncurses.h], [USE_CURSES="ncurses"],
      [AC_CHECK_HEADERS([ncurses/ncurses.h], [USE_CURSES="ncurses"],
         [AC_CHECK_HEADERS([curses.h], [USE_CURSES="ncurses"],
           [if test "$USE_CURSES" = "ncurses"; then
              USE_CURSES="missing_header"
            fi], [])], [])], [])
    if test "$USE_CURSES" = "ncurses"; then
      LIBCOB_LIBS="$LIBCOB_LIBS -lncurses"
    fi
  else
    if test "$USE_CURSES" = "ncurses"; then
      USE_CURSES="missing_lib"
    fi
  fi
fi


if test "$USE_CURSES" = "pdcurses" -o "$USE_CURSES" = "check"; then
  AC_CHECK_LIB([pdcurses], [initscr], [], [], [])
  if test "x$ac_cv_lib_pdcurses_initscr" = "xyes"; then
    AC_CHECK_HEADERS([pdcurses.h], [USE_CURSES="pdcurses"],
      [AC_CHECK_HEADERS([curses.h], [USE_CURSES="pdcurses"],
         [if test "$USE_CURSES" = "pdcurses"; then
            USE_CURSES="missing_header"
          fi], [])], [])
    if test "$USE_CURSES" = "pdcurses"; then
      LIBCOB_LIBS="$LIBCOB_LIBS -lpdcurses"
    fi
  else
    if test "$USE_CURSES" = "pdcurses"; then
      USE_CURSES="missing_lib"
    fi
  fi
fi

if test "$USE_CURSES" = "curses" -o "$USE_CURSES" = "check"; then
  AC_CHECK_LIB([curses], [initscr], [], [], [])
  if test "x$ac_cv_lib_curses_initscr" = "xyes"; then
    AC_CHECK_HEADERS([curses.h], [USE_CURSES="curses"],
      [if test "$USE_CURSES" = "curses"; then
         USE_CURSES="missing_header"
       fi], [])
    if test "$USE_CURSES" != "no"; then
      LIBCOB_LIBS="$LIBCOB_LIBS -lcurses"
    fi
  fi
else
  if test "$USE_CURSES" = "curses"; then
    USE_CURSES="missing_lib"
  fi
fi

case "$USE_CURSES" in
  check)
	USE_CURSES="not_found"
	AC_DEFINE([WITH_CURSES], ["not found"])
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


if test "$USE_CURSES" = "ncurses" -o "$USE_CURSES" = "ncursesw"; then
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

if test "$USE_CURSES" != "no" -a "$USE_CURSES" != "not_found"; then
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
	color_set (0, NULL);
	]])],
	[AC_DEFINE([HAVE_COLOR_SET], [1]) AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])],
	[])

	AC_MSG_CHECKING([for curses define_key function])
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
	define_key ("\E-3;3~", (KEY_MAX + 1));
	]])],
	[AC_DEFINE([HAVE_DEFINE_KEY], [1]) AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])],
	[])

	AC_MSG_CHECKING([for curses mouseinterval function])
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
	mouseinterval (-1);
	]])],
	[AC_DEFINE([HAVE_MOUSEINTERVAL], [1]) AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])],
	[])

	AC_MSG_CHECKING([for curses has_mouse function])
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
	has_mouse ();
	]])],
	[AC_DEFINE([HAVE_HAS_MOUSE], [1]) AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])],
	[])
fi
LIBS="$curr_libs" # removing curses

#
## Checks for indexed handlers
#

AC_ARG_WITH([indexed],
  [AS_HELP_STRING([--with-indexed],
    [Define default INDEXED file handler (visam,vbisam,disam,cisam,db)])],
  [ case "$with_indexed" in
    vbisam)
	    with_vbisam=yes
	    AC_DEFINE([WITH_INDEXED], [COB_IO_VBISAM])
	    AC_DEFINE([WITH_IXDFLT], ["VB-ISAM"]);;
    visam)
	    with_visam=yes
	    AC_DEFINE([WITH_INDEXED], [COB_IO_VISAM])
	    AC_DEFINE([WITH_IXDFLT], ["V-ISAM"]);;
    disam)
	    with_disam=yes
	    AC_DEFINE([WITH_INDEXED], [COB_IO_DISAM])
	    AC_DEFINE([WITH_IXDFLT], ["D-ISAM"]);;
    cisam)
	    with_cisam=yes
	    AC_DEFINE([WITH_INDEXED], [COB_IO_CISAM])
	    AC_DEFINE([WITH_IXDFLT], ["C-ISAM"]);;
    db)
	    with_db=yes
	    AC_DEFINE([WITH_INDEXED], [COB_IO_BDB])
	    AC_DEFINE([WITH_IXDFLT], ["BDB"]);;
    lmdb)
	    with_lmdb=yes
	    AC_DEFINE([WITH_INDEXED], [COB_IO_LMDB])
	    AC_DEFINE([WITH_IXDFLT], ["LMDB"]);;
	*)
  	  if test "$with_visam" = "yes"; then
	    AC_DEFINE([WITH_INDEXED], [COB_IO_VISAM])
	    AC_DEFINE([WITH_IXDFLT], ["V-ISAM"])
  	  elif test "$with_cisam" = "yes"; then
	    AC_DEFINE([WITH_INDEXED], [COB_IO_CISAM])
	    AC_DEFINE([WITH_IXDFLT], ["C-ISAM"])
  	  elif test "$with_disam" = "yes"; then
	    AC_DEFINE([WITH_INDEXED], [COB_IO_DISAM])
	    AC_DEFINE([WITH_IXDFLT], ["D-ISAM"])
  	  elif test "$with_vbisam" = "yes"; then
	    AC_DEFINE([WITH_INDEXED], [COB_IO_VBISAM])
	    AC_DEFINE([WITH_IXDFLT], ["VB-ISAM"])
  	  elif test "$with_db" = "yes"; then
	    AC_DEFINE([WITH_INDEXED], [COB_IO_BDB])
	    AC_DEFINE([WITH_IXDFLT], ["BDB"])
  	  elif test "$with_lmdb" = "yes"; then
	    AC_DEFINE([WITH_INDEXED], [COB_IO_LMDB])
	    AC_DEFINE([WITH_IXDFLT], ["LMDB"])
	  else
		AC_MSG_ERROR([--with-indexed=<HANDLER>, must be one of visam|vbisam|disam|cisam|db|lmdb])
	  fi ;;
    esac
  ], [
  	  if test "$with_visam" = "yes"; then
	    AC_DEFINE([WITH_INDEXED], [COB_IO_VISAM])
	    AC_DEFINE([WITH_IXDFLT], ["V-ISAM"])
  	  elif test "$with_cisam" = "yes"; then
	    AC_DEFINE([WITH_INDEXED], [COB_IO_CISAM])
	    AC_DEFINE([WITH_IXDFLT], ["C-ISAM"])
  	  elif test "$with_disam" = "yes"; then
	    AC_DEFINE([WITH_INDEXED], [COB_IO_DISAM])
	    AC_DEFINE([WITH_IXDFLT], ["D-ISAM"])
  	  elif test "$with_vbisam" = "yes"; then
	    AC_DEFINE([WITH_INDEXED], [COB_IO_VBISAM])
	    AC_DEFINE([WITH_IXDFLT], ["VB-ISAM"])
  	  elif test "$with_db" = "yes"; then
	    AC_DEFINE([WITH_INDEXED], [COB_IO_BDB])
	    AC_DEFINE([WITH_IXDFLT], ["BDB"])
  	  elif test "$with_lmdb" = "yes"; then
	    AC_DEFINE([WITH_INDEXED], [COB_IO_LMDB])
	    AC_DEFINE([WITH_IXDFLT], ["LMDB"])
	  fi
  ])

cob_gen_vbisam=no
cob_gen_visam=no
cob_gen_disam=no
cob_gen_cisam=no
cob_gen_odbc=no
cob_gen_oci=no
cob_gen_bdb=no
cob_gen_lmdb=no
cob_has_isam=no

curr_cppflags="$CPPFLAGS"
curr_libs="$LIBS"

AC_ARG_WITH([visam],
  [AS_HELP_STRING([--with-visam],
    [Use V-ISAM for INDEXED I/O])],
  [cob_has_isam=yes],
  [with_visam=check])

AC_ARG_WITH([cisam],
  [AS_HELP_STRING([--with-cisam],
    [Use C-ISAM for INDEXED I/O])],
  [cob_has_isam=yes],
  [with_cisam=check])

AC_ARG_WITH([disam],
  [AS_HELP_STRING([--with-disam],
    [Use D-ISAM for INDEXED I/O])],
  [cob_has_isam=yes],
  [with_disam=check])

AC_ARG_WITH([vbisam],
  [AS_HELP_STRING([--with-vbisam],
    [Use VB-ISAM for INDEXED I/O])],
  [cob_has_isam=yes],
  [with_vbisam=check])

AC_ARG_WITH([db],
[AS_HELP_STRING([--with-db],
  [Use Berkeley DB >= 4.1 for INDEXED I/O])],
[cob_has_isam=yes],
[with_db=check])

reset_isam_check() {
	if test "$with_visam" = "check"; then
		with_visam=no
	fi
	if test "$with_vbisam" = "check"; then
		with_vbisam=no
	fi
	if test "$with_disam" = "check"; then
		with_disam=no
	fi
	if test "$with_cisam" = "check"; then
		with_cisam=no
	fi
	if test "$with_db" = "check"; then
		with_db=no
	fi
}

if test "$cob_has_isam" = "yes"; then
# Some INDEXED handler was selected so do not check for others
	reset_isam_check
fi

AS_IF([test "$with_visam" = "yes" -o "$with_visam" = "check"], [
	PKG_CHECK_MODULES([VISAM], [visam], [], [#])
	if test -n "$VISAM_CFLAGS"; then
		CPPFLAGS="$CPPFLAGS $VISAM_CFLAGS"
	fi
	AC_CHECK_HEADERS([visam.h], [],
			[if test "$with_visam" = "yes"; then
				AC_MSG_ERROR([visam.h is required for V-ISAM])
			else
			   with_visam=not_found
			   break
			fi])
	unset ac_cv_search_isopen
	if test "$with_visam" = "not_found"; then
		 with_visam=no
	else
		AC_SEARCH_LIBS([isopen], [visam],
			[AC_DEFINE([WITH_VISAM], [1])
			 if test "$with_visam" = "check"; then
				 AC_DEFINE([WITH_INDEXED], [COB_IO_VISAM])
				 AC_DEFINE([WITH_IXDFLT], ["V-ISAM"])
			 fi
			 cob_gen_visam=yes
		 	 with_visam=yes
			 reset_isam_check
			], [], [$VISAM_LIBS])
		if test "$ac_cv_search_isopen" = "no"; then
			AC_MSG_ERROR([VISAM library not found])
		elif test "$ac_cv_search_isopen" != "none required"; then
			AC_MSG_NOTICE([VISAM library found as $ac_cv_search_isopen])
			VISAM_LIBS="$VISAM_LIBS $ac_cv_search_isopen"
		elif test "x$VISAM_LIBS" != "x"; then
			AC_MSG_NOTICE([VISAM library found in $VISAM_LIBS])
		else
			AC_MSG_NOTICE([VISAM library found])
		fi
		unset ac_cv_search_isopen
		CPPFLAGS="$curr_cppflags"
		LIBS="$curr_libs"
	fi
   ],
  [VISAM_LIBS=""])

AS_IF([test "$with_vbisam" = "yes" -o "$with_vbisam" = "check"], [
	PKG_CHECK_MODULES([VBISAM], [vbisam], [], [#])
	if test -n "$VBISAM_CFLAGS"; then
		CPPFLAGS="$CPPFLAGS $VBISAM_CFLAGS"
	fi
	AC_CHECK_HEADERS([vbisam.h], [],
			[if test "$with_vbisam" = "yes"; then
				AC_MSG_ERROR([vbisam.h is required for VB-ISAM])
			else
			   with_vbisam=not_found
			   break
			fi])
	unset ac_cv_search_isopen
	unset ac_cv_search_isfullclose
	if test "$with_vbisam" = "not_found"; then
		 with_vbisam=no
	else
		# note: isfullclose is available since 2.0, isopen since 1.0
		AC_SEARCH_LIBS([isfullclose], [vbisam],
			[AC_DEFINE([WITH_VBISAM], [1])
			 if test "$with_vbisam" = "check"; then
				 AC_DEFINE([WITH_INDEXED], [COB_IO_VBISAM])
				 AC_DEFINE([WITH_IXDFLT], ["VB-ISAM"])
			 fi
			 cob_gen_vbisam=yes
		 	 with_vbisam=yes
			 reset_isam_check
			], [], [$VBISAM_LIBS])
		if test "$ac_cv_search_isopen" = "no"; then
			AC_MSG_ERROR([libvbisam >= 2.0 is required for VBISAM])
		elif test "$ac_cv_search_isopen" != "none required"; then
			AC_MSG_NOTICE([VBISAM library found as $ac_cv_search_isfullclose])
			VBISAM_LIBS="$VBISAM_LIBS $ac_cv_search_isfullclose"
		elif test "x$VBISAM_LIBS" != "x"; then
			AC_MSG_NOTICE([VBISAM library found in $VBISAM_LIBS])
		else
			AC_MSG_NOTICE([VBISAM library found])
		fi
		unset ac_cv_search_isfullclose
		CPPFLAGS="$curr_cppflags"
		LIBS="$curr_libs"
   fi ],
  [VBISAM_LIBS=""])

AS_IF([test "$with_disam" = "yes" -o "$with_disam" = "check"], [
	PKG_CHECK_MODULES([DISAM], [disam], [], [#])
	if test -n "$DISAM_CFLAGS"; then
		CPPFLAGS="$CPPFLAGS $DISAM_CFLAGS"
	fi
	unset ac_cv_search_isopen
	AC_SEARCH_LIBS([isopen], [disam8 disam72 disam71 disam7 disam],
			[AC_DEFINE([WITH_DISAM], [1])
			 if test "$with_disam" = "check"; then
				 AC_DEFINE([WITH_INDEXED], [COB_IO_DISAM])
				 AC_DEFINE([WITH_IXDFLT], ["D-ISAM"])
			 fi
		 	cob_gen_disam=yes
		 	with_disam=yes
			reset_isam_check
		], [], [$DISAM_LIBS])
	if test "$ac_cv_search_isopen" = "no"; then
		 if test "$with_disam" != "check"; then
			AC_MSG_ERROR([DISAM library not found])
		fi
	elif test "$ac_cv_search_isopen" != "none required"; then
		AC_MSG_NOTICE([DISAM library found as $ac_cv_search_isopen])
		DISAM_LIBS="$DISAM_LIBS $ac_cv_search_isopen"
	elif test "x$DISAM_LIBS" != "x"; then
		AC_MSG_NOTICE([DISAM library found in $DISAM_LIBS])
	else
		AC_MSG_NOTICE([DISAM library found])
	fi
	# note: old DISAM versions only ship disam.h, newer _may_ provide it as convenience
	#       to not break an environment which has both old and new we explicit only check
	#       what we want to use, matching to the library found (and assume that "none needed"
	#       means we're using one of the newer ones)
	if test "$ac_cv_search_isopen" = "disam"; then
	 AC_CHECK_HEADERS([disam.h], [],
	   AC_MSG_ERROR([disam.h is required for DISAM]))
	elif test "$ac_cv_search_isopen" != "no"; then
	 AC_CHECK_HEADERS([iswrap.h isconfig.h isintstd.h], [],
	   AC_MSG_ERROR([iswrap.h isconfig.h isintstd.h are required for $ac_cv_search_isopen]))
	fi
	unset ac_cv_search_isopen
	CPPFLAGS="$curr_cppflags"
	LIBS="$curr_libs"
  ],
  [DISAM_LIBS=""])

AS_IF([test "$with_cisam" = "yes" -o "$with_cisam" = "check"], [
	PKG_CHECK_MODULES([CISAM], [cisam], [], [#])
	if test -n "$CISAM_CFLAGS"; then
		CPPFLAGS="$CPPFLAGS $CISAM_CFLAGS"
	fi
	AC_CHECK_HEADERS([isam.h], [],
			[if test "$with_cisam" = "yes"; then
				AC_MSG_ERROR([isam.h is required for C-ISAM])
			else
			   with_cisam=not_found
			   break
			fi])
	if test "$with_cisam" = "not_found"; then
		 with_cisam=no
	else
	  AC_CHECK_LIB([ifisam], [isopen],
		[AC_DEFINE([WITH_CISAM], [1])
			 if test "$with_cisam" = "check"; then
				 AC_DEFINE([WITH_INDEXED], [COB_IO_CISAM])
				 AC_DEFINE([WITH_IXDFLT], ["C-ISAM"])
			 fi
	    	cob_gen_cisam=yes
		 	with_cisam=yes
			reset_isam_check
		CISAM_LIBS="-lifisam -lifisamx"],
		AC_MSG_ERROR([libifisam is required for C-ISAM]), [-lifisamx])
	  CPPFLAGS="$curr_cppflags"
	  LIBS="$curr_libs"
   fi ],
  [CISAM_LIBS=""])

static_isam=no
num_isam=0
if  test "$cob_gen_visam" = "yes"; then
	num_isam=`expr $num_isam + 1`
fi
if  test "$cob_gen_vbisam" = "yes"; then
	num_isam=`expr $num_isam + 1`
fi
if  test "$cob_gen_disam" = "yes"; then
	num_isam=`expr $num_isam + 1`
fi
if  test "$cob_gen_cisam" = "yes"; then
	num_isam=`expr $num_isam + 1`
fi
#
# If just one ISAM handler, then build as part of libcob otherwise
# each ISAM handler needs to be a separate shared code library
#
if test $num_isam = 1; then
	AC_DEFINE([WITH_STATIC_ISAM], [1])
	LIBCOB_LIBS="$LIBCOB_LIBS $CISAM_LIBS$DISAM_LIBS$VBISAM_LIBS$VISAM_LIBS"
	static_isam=yes
fi

AC_ARG_WITH([odbc],
[AS_HELP_STRING([--with-odbc],
  [Optionally use ODBC for INDEXED/RELATIVE I/O])],
[ if test "$with_odbc" = "yes"; then
	PKG_CHECK_MODULES([ODBC], [odbc], [], [#])
	if test -n "$ODBC_CFLAGS"; then
		CPPFLAGS="$CPPFLAGS $ODBC_CFLAGS"
	fi
	AC_CHECK_HEADERS([sql.h sqlext.h], [],
		AC_MSG_ERROR([sql.h and sqlext.h are required for ODBC]))
# DB2 should work fine using the ODBC interface (RJN Dec 2020) but should be tested
#	AC_CHECK_HEADERS([sqlcli1.h sqlca.h sqludf.h], [],
#		AC_MSG_ERROR([sqlcli1.h, sqlca.h, and sqludf.h are required for ODBC (DB2)]))

	if test "x$ODBC_LIBS" = "x"; then
		ODBC_LIBS="-lodbc"
	fi
	LIBS="$LIBS $ODBC_LIBS"
	AC_CHECK_LIB([odbc], [SQLConnect],
		[AC_DEFINE([WITH_ODBC], [1])],
		AC_MSG_ERROR([libodbc is required for ODBC]), [])
	cob_gen_odbc=yes 
	CPPFLAGS="$curr_cppflags"
	LIBS="$curr_libs"
  fi ],
[])

AC_ARG_WITH([oci],
[AS_HELP_STRING([--with-oci],
  [Optionally use Oracle OCI for INDEXED/RELATIVE I/O])],
[ if test "$with_oci" = "yes"; then
	if test -n "$OCI_CFLAGS"; then
		CPPFLAGS="$CPPFLAGS $OCI_CFLAGS"
	elif test -n "$ORACLE_HOME"; then
		CPPFLAGS="$CPPFLAGS -I${ORACLE_HOME}/rdbms/public"
	fi
	AC_CHECK_HEADERS([oci.h], [],
		AC_MSG_ERROR([oci.h is required for Oracle OCI]))

	if test -n "$OCI_LIBS"; then
		LIBS="$LIBS $OCI_LIBS"
	elif test -n "$ORACLE_HOME"; then
		OCI_LIBS="-L${ORACLE_HOME}/lib"
		LIBS="$LIBS $OCI_LIBS"
	fi
	AC_CHECK_LIB([clntsh], [OCIEnvCreate],
		[AC_DEFINE([WITH_OCI], [1])],
		AC_MSG_ERROR([libclnt is required for Oracle OCI]), [])
	cob_gen_oci=yes 
	OCI_LIBS="-L${ORACLE_HOME}/lib -lclntsh"
	CPPFLAGS="$curr_cppflags"
	LIBS="$curr_libs"
  fi ],
[])

if test "$with_db" = "yes" -o "$with_db" = "check"; then
	AC_MSG_NOTICE([Checks for Berkeley DB ...])

	AC_CHECK_HEADERS([db.h], [], AC_MSG_ERROR([Berkeley DB db.h is missing]), [])

	# BDB header exists. Extract major/minor number pair
	COB_BDB_HEADER=''
	COB_BDB_HEADER_STR=''
	AC_RUN_IFELSE([AC_LANG_SOURCE([[
		#include <stdio.h>
		#include <db.h>
		int main (int argc, char **argv)
		{
			(void)argv;
			if (argc == 2)
				printf ("%d.%d", DB_VERSION_MAJOR, DB_VERSION_MINOR);
			if (argc == 3)
				printf ("-%s-", DB_VERSION_STRING);
			return 0;
		}
		]])],
		[COB_BDB_HEADER=$(./conftest$ac_exeext x)]
		[COB_BDB_HEADER_STR=$(./conftest$ac_exeext x y)],
		[AC_MSG_ERROR([Unable to extract Berkeley DB version information from db.h])],
		[AC_MSG_WARN([cannot run test program while cross-compiling])
		 COB_BDB_HEADER="cross"
		 COB_BDB_HEADER_STR="cross"])
	if test "x$COB_BDB_HEADER" = "x"; then
		AC_MSG_ERROR([Unable to extract Berkeley DB version information])
	fi
	if test "x$COB_BDB_HEADER_STR" != "x"; then
		AC_MSG_NOTICE([db.h reports version "$COB_BDB_HEADER_STR"])
	fi
	if test "$COB_BDB_HEADER_STR" != "cross"; then
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
		test_libs="$LIBS"
		cob_got_db=no
		AC_MSG_CHECKING([for Berkeley DB library with version $COB_BDB_HEADER])
		# prefer library with version number included as some systems link against wrong version
		# of the library and to not break when a newer BDB version is installed, see bug #100
		for cobdb in db-$COB_BDB_HEADER db
		do
			LIBS="$test_libs -l$cobdb"
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
			cob_gen_bdb=yes 
			BDB_LIBS="-l$cobdb"
			break],
			[],
			[])
		done
		LIBS="$test_libs"
		unset test_libs
		if test "$cob_got_db" = "yes"; then
			AC_MSG_RESULT([yes])
			AC_MSG_NOTICE([BDB library version $COB_BDB_HEADER found as -l$cobdb])
			with_db=yes
		elif test "$with_db" = "check"; then
			AC_MSG_RESULT([no])
			with_db=no
		else
			AC_MSG_RESULT([no])
			AC_MSG_ERROR([BDB library version $COB_BDB_HEADER not found])
		fi
		unset cobdb
		unset cob_got_db
	else
		AC_MSG_WARN([Matching BDB version (>=4.1) assumed])
		AC_CHECK_LIB([db], [db_version],
		[LIBCOB_LIBS="$LIBCOB_LIBS"],
		AC_MSG_ERROR([BDB library is required as -ldb]), [])
	fi
	unset COB_BDB_HEADER
	LIBS="$curr_libs"
fi

if test "$with_lmdb" = "yes"; then
	AC_MSG_NOTICE([Checks for Lightning Memory-Mapped Database (LMDB) ...])
	AC_CHECK_HEADER([lmdb.h], [],	AC_MSG_ERROR([LMDB lmdb.h is missing]),[])

	# MDB header exists. Extract major/minor/patch numbers
	COB_MDB_HEADER=''
	COB_MDB_HEADER_STR=''
	AC_RUN_IFELSE([AC_LANG_SOURCE([[
		#include <stdio.h>
		#include <lmdb.h>
		int main (int argc, char **argv)
		{
			(void)argv;
			if (argc == 2)
				printf ("%d.%d.%d", MDB_VERSION_MAJOR, MDB_VERSION_MINOR, MDB_VERSION_PATCH);
			if (argc == 3)
				printf ("-%s-", MDB_VERSION_STRING);
			return 0;
		}
		]])],
		[COB_MDB_HEADER=`./conftest$ac_exeext x`]
		[COB_MDB_HEADER_STR=`./conftest$ac_exeext x y`],
		[AC_MSG_ERROR([Unable to extract LMDB version information from lmdb.h])],
		[AC_MSG_WARN([cannot run test program while cross-compiling])
		 COB_MDB_HEADER="cross"
		 COB_MDB_HEADER_STR="cross"])
	if test "x$COB_MDB_HEADER" = "x"; then
		AC_MSG_ERROR([Unable to extract LMDB version information])
	fi
	if test "x$COB_MDB_HEADER_STR" != "x"; then
		AC_MSG_NOTICE([lmdb.h reports version "$COB_MDB_HEADER_STR"])
	fi
	AC_CHECK_LIB([lmdb], [mdb_version],
		[AC_DEFINE([WITH_LMDB], [1])],
		AC_MSG_ERROR([liblmdb is required for LMDB]), [])
	LMDB_LIBS="-llmdb"

	AC_MSG_CHECKING([for Symas LMDB version >= 0.9.19])
	COB_MDB_HEADER_MAJOR=$(echo "$COB_MDB_HEADER" | cut -d. -f1)
	COB_MDB_HEADER_PATCH=$(echo "$COB_MDB_HEADER" | cut -d. -f3)
	if test "$COB_MDB_HEADER_STR" != "cross"; then
		if test $COB_MDB_HEADER_MAJOR -gt 0; then
			AC_MSG_RESULT([yes ($COB_MDB_HEADER)])
		else
			if test $COB_MDB_HEADER_PATCH -gt 18; then
				AC_MSG_RESULT([yes ($COB_MDB_HEADER)])
			else
				AC_MSG_RESULT([no ($COB_MDB_HEADER)])
				AC_MSG_ERROR([LMDB version < 0.9.19])
			fi
		fi
	fi
	cob_gen_lmdb=yes
	LIBS="$curr_libs"
fi

AC_ARG_WITH([lmdb],
[AS_HELP_STRING([--with-lmdb],
  [Use Lightning Memory-Mapped Database (LMDB) for INDEXED I/O (experimental, no locking or shared storage, write is extremely slow)])],
[ if test "$with_lmdb" = "yes"; then
  AC_DEFINE([WITH_LMDB], [1])
  fi ],
[])

AC_ARG_WITH([index-extfh],
[AS_HELP_STRING([--with-index-extfh],
  [Use external INDEXED handler (obsolete)])],
[ if test "$with_index_extfh" = "yes"; then
  AC_DEFINE([WITH_INDEX_EXTFH], [1])
  fi ],
[])

AC_ARG_WITH([seqra-extfh],
  [AS_HELP_STRING([--with-seqra-extfh],
    [Use external SEQ/RAN handler (obsolete)])],
  [ if test "$with_seqra_extfh" = "yes"; then
	AC_DEFINE([WITH_SEQRA_EXTFH], [1])
    fi ],
  [])

# Checks for dl/ltdl.
DEFINE_DL="no"

AC_MSG_CHECKING([for _WIN32])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
	#ifndef _WIN32
	# error macro not defined
	#endif]])],
	[DEFINE_DL="yes"
	 AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])])

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

# Checks for compiling computed gotos
AC_MSG_CHECKING([for support of computed gotos])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
	int test ()
	{
	void	*test_ptr;
	test_ptr = &&lab;

	goto *test_ptr;
	return 1;

	lab:
	return 0;
	}]])],
	[AC_DEFINE([COB_COMPUTED_GOTO], [1])
	 AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])])

# Checks for size of long
AC_MSG_CHECKING([if size of long int = size of long long])
AC_RUN_IFELSE([AC_LANG_PROGRAM([[]], [[
	if (sizeof(long int) == sizeof(long long))
		return 0;
	return 1;
	]])],
	[AC_DEFINE([COB_LI_IS_LL], [1]) AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])],
	[if test "$COB_LI_IS_LL" = "0"; then
		AC_MSG_RESULT([specified "no" on configure line])
	 else
		if test "$COB_LI_IS_LL" = "1"; then
			AC_MSG_RESULT([specified "yes" on configure line])
		else
			COB_LI_IS_LL=1
			AC_MSG_RESULT([assumed - cross-compilation])
		fi
	 fi
	 AC_DEFINE([COB_LI_IS_LL], [$COB_LI_IS_LL])
	 AC_MSG_WARN([cannot run test program while cross-compiling])])

AC_MSG_CHECKING([if long is 32 bits])
AC_RUN_IFELSE([AC_LANG_PROGRAM([[]], [[
	if (sizeof (long) == 4)
		return 0;
	return 1;
	]])],
	[AC_DEFINE([COB_32_BIT_LONG], [1]) AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])],
	[if test "$COB_32_BIT_LONG" = "0"; then
		AC_MSG_RESULT([specified "no" on configure line])
	 else
		if test "$COB_32_BIT_LONG" = "1"; then
			AC_MSG_RESULT([specified "yes" on configure line])
		else
			COB_32_BIT_LONG=1
			AC_MSG_RESULT([assumed - cross-compilation])
		fi
	 fi
	 AC_DEFINE([COB_32_BIT_LONG], [$COB_32_BIT_LONG])
	 AC_MSG_WARN([cannot run test program while cross-compiling])])


if test "cross_compiling" != "yes"; then
	COB_HAS_64_BIT_POINTER="no"
fi
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
	[if test "$COB_HAS_64_BIT_POINTER" = "0"; then
		COB_HAS_64_BIT_POINTER="no"
		AC_DEFINE([COB_64_BIT_POINTER], [0])
		AC_MSG_RESULT([specified "no" on configure line])
	 else
		if test "$COB_HAS_64_BIT_POINTER" = "1"; then
			COB_HAS_64_BIT_POINTER="yes"
			AC_DEFINE([COB_64_BIT_POINTER], [1])
			AC_MSG_RESULT([specified "yes" on configure line])
		else
			COB_HAS_64_BIT_POINTER="no"
			AC_DEFINE([COB_64_BIT_POINTER], [0])
			AC_MSG_RESULT([assumed "no" - cross-compilation])
		fi
	 fi
	 AC_MSG_WARN([cannot run test program while cross-compiling])])

# Check gcc wrapv option
# We likely don't need this and remove it before 3.0 final release
#if test "$COB_USES_GCC_NO_ICC" = "yes"; then
#	curr_cflags="$CFLAGS"
#	CFLAGS="$CFLAGS -fwrapv"
#	AC_MSG_CHECKING([for gcc -fwrapv option])
#	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[int testint;]])],
#		[AC_DEFINE([HAVE_FWRAPV_OPT], [1]) AC_MSG_RESULT([yes])],
#		[AC_MSG_RESULT([no])])
#		[],
#		[CFLAGS="$curr_cflags"])
#	CFLAGS="$curr_cflags"
#fi

# Check if aligned attribute seems to work
# done:   does not raise an error
# *TODO*: has same output as omitting it)
AC_MSG_CHECKING([for aligned attribute])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]],
	[[char testchar[4] __attribute__((aligned));]])],
	[AC_DEFINE([HAVE_ATTRIBUTE_ALIGNED], [1]) AC_MSG_RESULT([yes])
	],
	[AC_MSG_RESULT([no])])

# Simon: removed, use --disable-nls instead
## Disable for Cygwin
#AC_MSG_CHECKING([for __CYGWIN__])
#AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
#	#ifndef __CYGWIN__
#	# error macro not defined
#	#endif]])],
#	[enable_nls=no
#	 AC_MSG_RESULT([yes])],
#	[AC_MSG_RESULT([no])])

AM_GNU_GETTEXT([external])
AM_GNU_GETTEXT_VERSION([0.19.8])
if test "x$LTLIBINTL" != "x"; then
  if test "x$PROGRAMS_LIBS" != "x"; then
    PROGRAMS_LIBS="$PROGRAMS_LIBS $LTLIBINTL"
  else
    PROGRAMS_LIBS="$LTLIBINTL"
  fi
  LIBCOB_LIBS="$LIBCOB_LIBS $LTLIBINTL"
fi

# Checks for internationalization stuff
# AM_ICONV
AM_LANGINFO_CODESET

# GnuCOBOL Configuration

COB_CONFIG_DIR="$datadir/$PACKAGE_TARNAME/config"
COB_SCHEMA_DIR="$datadir/$PACKAGE_TARNAME/schema"
COB_COPY_DIR="$datadir/$PACKAGE_TARNAME/copy"
COB_LIBRARY_PATH="$libdir/$PACKAGE_TARNAME"
COB_EXE_EXT="$EXEEXT"
if test "$COB_EXE_EXT" = ".exe"; then
	COB_MODULE_EXT="dll"
else
	if test "$COB_EXE_EXT" = ".exe" -o "$COB_EXE_EXT" = ".EXE"; then
		COB_MODULE_EXT="dll"
	else
		# normal case...
		COB_MODULE_EXT=$(echo "$acl_cv_shlibext" | sed -e 's/dll\.a/dll/')
	fi
fi
COB_OBJECT_EXT="$OBJEXT"
LIBS="$curr_libs"
COB_EXPORT_DYN="$(eval echo $export_dynamic_flag_spec)"
# FIXME: lt_prog_compiler_pic is not always correct, for example with occ
COB_PIC_FLAGS=$(echo "$lt_prog_compiler_pic" | sed -e 's/^ //')

if test "$enable_cflags_setting" = "yes"; then
	if test "$enable_hardening" != "yes"; then
		# Remove -O2 option added by AC_PROG_CC and add -O0
		if test "$enable_debug" = "yes" -o "$enable_code_coverage" = "yes"; then
			CFLAGS=$(echo "$CFLAGS" | sed -e 's/ *-O@<:@0-9a-zA-Z@:>@* */ /g' -e 's/ $//' -e 's/^ //')
			if test "$COB_USES_GCC" = "yes"; then
				CFLAGS="$CFLAGS -O0"
			fi
		fi
	fi
fi

unset enable_cflags_setting

# cobc.c assumed -g for all but _MSC_VER
if test "${COB_DEBUG_FLAGS}x" = "x"; then
COB_DEBUG_FLAGS="-g"

# For debugging: add most expressive debug level if using GCC (compatible)
curr_cflags="$CFLAGS"
cob_temp_flags="-ggdb3"
CFLAGS="$curr_cflags $cob_temp_flags"
AC_MSG_CHECKING([for debug option $cob_temp_flags])
AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[int testint;]])],
	[AC_MSG_RESULT([yes])
	 COB_DEBUG_FLAGS="$cob_temp_flags"],
	[AC_MSG_RESULT([no])
	 cob_temp_flags="-g3"
	 CFLAGS="$curr_cflags $cob_temp_flags"
	 AC_MSG_CHECKING([for debug option $cob_temp_flags])
	 AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[int testint;]])],
		[AC_MSG_RESULT([yes])
		 COB_DEBUG_FLAGS="$cob_temp_flags"],
		[AC_MSG_RESULT([no])])
	]
)

cob_temp_flags="-fasynchronous-unwind-tables"
CFLAGS="$curr_cflags $cob_temp_flags"
AC_MSG_CHECKING([for debug option $cob_temp_flags])
AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[int testint;]])],
	[AC_MSG_RESULT([yes])
	 COB_DEBUG_FLAGS="$COB_DEBUG_FLAGS $cob_temp_flags"],
	[AC_MSG_RESULT([no])
	 cob_temp_flags="-funwind-tables"
	 CFLAGS="$curr_cflags $cob_temp_flags"
	 AC_MSG_CHECKING([for debug option $cob_temp_flags])
	 AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[int testint;]])],
		[AC_MSG_RESULT([yes])
		 COB_DEBUG_FLAGS="$COB_DEBUG_FLAGS $cob_temp_flags"],
		[AC_MSG_RESULT([no])])
	]
)

if test "$enable_debug" = "yes"; then
	CFLAGS="$curr_cflags $COB_DEBUG_FLAGS"
else
	CFLAGS="$curr_cflags"
fi
fi

if test "$enable_hardening" = "yes"; then
	AC_MSG_NOTICE([Compile with hardening options])
	# needs optimization
	#if test "$enable_debug" = "yes"; then
	#   CFLAGS="$CFLAGS -O"
	#fi
	CPPFLAGS="$CPPFLAGS -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=2"

	curr_cflags="$CFLAGS"
	cob_temp_flags="-fstack-protector-strong"
	CFLAGS="$curr_cflags $cob_temp_flags"
	AC_MSG_CHECKING([for $cob_temp_flags option])
	AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[int testint;]])],
		[AC_MSG_RESULT([yes])],
		[AC_MSG_RESULT([no])
		cob_temp_flags="-fstack-protector"
		CFLAGS="$curr_cflags $cob_temp_flags"
		AC_MSG_CHECKING([for $cob_temp_flags option])
		AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[int testint;]])],
			[AC_MSG_RESULT([yes])],
			[CFLAGS="$curr_cflags"; cob_temp_flags=""; AC_MSG_RESULT([no])])
		 ])
	if test "x$cob_temp_flags" != "x"; then
	   if test "x$COB_LDFLAGS" != "x"; then
	      COB_LDFLAGS="$COB_LDFLAGS $cob_temp_flags"
	   else
	      COB_LDFLAGS="$cob_temp_flags"
	   fi
	fi
	unset cob_temp_flags
fi

if test "$COB_USES_GCC" = "yes"; then
	if test "x$CFLAGS" != "x"; then
		CFLAGS="$CFLAGS -pipe"
	else
		CFLAGS="-pipe"
	fi
fi

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


# compiler specific general options for COB_CFLAGS, originating from cobc.c (main)

if test "$COB_USES_ICC_ONLY" = "yes"; then
	# these are deprecated...
	COB_CFLAGS="$COB_CFLAGS -vec-report0 -opt-report 0"
elif test "$COB_USES_WATCOMC_ONLY" = "yes"; then
	# -s = no overflow checks, otherwise need to code/link a CHK routine
	COB_CFLAGS="$COB_CFLAGS -s -wcd=118"
elif test "$COB_USES_XLC_ONLY" = "yes"; then
	# use read-only memory for string literals and constants
	COB_CFLAGS="$COB_CFLAGS -qro -qroconst"
	# allow nonstandard usage - CHECKME where do we need this ???
	# do we need the additional check for __IBMC__ >= 700 we had in cobc?
	COB_CFLAGS="$COB_CFLAGS -qlanglvl=extended"
	# Suppress compiler warning about MAXMEM optimization
	COB_CFLAGS="$COB_CFLAGS -qsuppress=1500-030"
fi

if test "$COB_USES_GCC_NO_ICC" = "yes"; then
	# comment from cobc.c:	--param max-goto-duplication-insns=100000
	# /* RXWRXW - gcse */
	# COB_CFLAGS="$COB_CFLAGS -Wno-unused -fsigned-char -fno-gcse"
	COB_CFLAGS="$COB_CFLAGS -Wno-unused -fsigned-char"
fi

# Check gcc 4 pointer sign option (at least available with "recent" clang, too)
#if test "$COB_USES_GCC_NO_ICC" = "yes"; then
if test "$COB_USES_XLC_ONLY" != "yes"; then
	curr_cflags="$CFLAGS"
	CFLAGS="$CFLAGS -Wno-pointer-sign"
	AC_MSG_CHECKING([for gcc pointer sign option])
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[int testint;]])],
		[COB_CFLAGS="$COB_CFLAGS -Wno-pointer-sign"; AC_MSG_RESULT([yes])],
		[AC_MSG_RESULT([no])])
	CFLAGS="$curr_cflags"
fi
#fi

if test "$COB_USES_CLANG_ONLY" = "yes"; then
	# don't warn if cobc uses arguments which aren't picked up (likely because of the translation phase)
	COB_CFLAGS="$COB_CFLAGS -Qunused-arguments"
fi


#
# Add CFLAGS and CPPFLAGS to COB_CFLAGS, minus optimization/debug options.

cob_temp_flags="$CFLAGS $CPPFLAGS"

if test "$enable_hardening" = no; then
	cob_temp_flags=$(echo x$cob_temp_flags |		\
	 sed -e 's/^x//'				\
 	     -e 's/-Wp,-D_FORTIFY_SOURCE=.//g'		\
 	     -e 's/-D_FORTIFY_SOURCE=.//g'		\
 	     -e 's/-fmessage-length=0//g'		\
 	     -e 's/-fstack-protector-strong//g'		\
 	     -e 's/-fstack-protector-all//g'		\
 	     -e 's/-fstack-protector//g'		\
	     )
fi

cob_temp_flags=$(echo x$cob_temp_flags |		\
	 sed -e 's/^x//'				\
	     -e 's/-g\(db\)\{0,1\}[[0-9 ]]//g'				\
	     -e 's/-g\(db\)\{0,1\}$//'				\
	     -e 's/[[+-]]O[[0-9s]]*//g'			\
 	     -e 's/-O//g'				\
 	     -e 's/-funwind-tables//g'			\
 	     -e 's/-fasynchronous-unwind-tables//g'	\
 	     -e 's/  / /g'	\
 	     -e 's/ $//g'	\
	     )

if test "x$cob_temp_flags" != "x"; then
	COB_CFLAGS="$cob_temp_flags $COB_CFLAGS"
fi
if test "$enable_hardening" = "yes"; then
	# needs optimization
	COB_CFLAGS="$COB_CFLAGS -O"
fi
unset cob_temp_flags

# Special stuff

AH_VERBATIM([_XOPEN_SOURCE_EXTENDED],
[/* Define to 1 if on HPUX.  */
#ifndef _XOPEN_SOURCE_EXTENDED
# undef _XOPEN_SOURCE_EXTENDED
#endif])dnl

# FIXME: COB_SHARED_OPT should at least be checked for "compiles";
#        for example breaks with occ and other non-GCC compilers,
#        *at least* check that compilation still works when using this option...
COB_SHARED_OPT="-shared"

COB_FIX_LIB="$COB_EXPORT_DYN"
COB_FIX_LIBTOOL=""
case $host_os in
  mingw*)
	if test "$prefix" = "NONE"; then
		COB_CONFIG_DIR="$(cd /usr && pwd -W)/local/share/$PACKAGE_TARNAME/config"
		COB_COPY_DIR="$(cd /usr && pwd -W)/local/share/$PACKAGE_TARNAME/copy"
		COB_SCHEMA_DIR="$(cd /usr && pwd -W)/local/share/$PACKAGE_TARNAME/schema"
	fi
	if test "$COB_USES_GCC" = "yes"; then
		COB_EXPORT_DYN="-Wl,--export-all-symbols -Wl,--enable-auto-import -Wl,--enable-auto-image-base"
		COB_FIX_LIBTOOL="-Wl,--enable-auto-import"
	fi
	#COB_FIX_LIB=""
	;;
  cygwin*)
	if test "$COB_USES_GCC" = "yes"; then
		COB_EXPORT_DYN="-Wl,--export-all-symbols -Wl,--enable-auto-import -Wl,--enable-auto-image-base"
		COB_FIX_LIBTOOL="-Wl,--enable-auto-import"
	fi
	#COB_FIX_LIB=""
	;;
  darwin* | rhapsody*)
	if test "$COB_USES_GCC" = "yes"; then
		COB_SHARED_OPT="-bundle -flat_namespace -undefined suppress"
	fi
	;;
  hpux*)
	if test "$COB_USES_GCC" != "yes"; then
		COB_SHARED_OPT="-b"
		AC_DEFINE([_XOPEN_SOURCE_EXTENDED], [1])
		rm -f hptest*
		echo 'int main() { return 0; }' > hptest.c
		${CC} ${CFLAGS} +Olit=all -o hptest hptest.c > hptest.out 2>&1
		if test $? -ne 0 -o -s hptest.out; then
			CFLAGS="$CFLAGS +ESlit"
			COB_CFLAGS="$COB_CFLAGS +ESlit -w"
		else
			CFLAGS="$CFLAGS +Olit=all"
			COB_CFLAGS="$COB_CFLAGS +Olit=all -w"
		fi

		if test "$enable_debug" = "yes"; then
			CFLAGS="$CFLAGS +O0"
		else
			CFLAGS="$CFLAGS +O2"
		fi

		rm -f hptest*
	fi
	;;
  aix*)
	COB_EXPORT_DYN="-Wl,-bexpfull -Wl,-brtl"
	if test "$COB_USES_GCC" != "yes"; then
		CFLAGS="$CFLAGS -Q -qro -qroconst"
		if test "$enable_debug" = "yes"; then
			CFLAGS="$CFLAGS -qnoopt"
		else
			CFLAGS="$CFLAGS -O2"
		fi

		COB_SHARED_OPT="-G"
		COB_FIX_LIB=""
		# COB_FIX_LIB="-Wc,-G $COB_EXPORT_DYN"
		# COB_CFLAGS="$COB_CFLAGS -qchars=signed"
	else
		COB_FIX_LIB="$COB_EXPORT_DYN"
	fi
	;;
  solaris*)
	if test "$COB_USES_GCC" != "yes"; then
		CFLAGS="$CFLAGS -xstrconst"
		if test "$enable_debug" != "yes"; then
			CFLAGS="$CFLAGS -xO2"
		fi
		COB_CFLAGS="$COB_CFLAGS -xstrconst"
		COB_SHARED_OPT="-G"
	fi
	;;
esac

if test "$COB_USES_ICC_ONLY" = "yes"; then
	if test "$enable_debug" != "yes"; then
		CFLAGS="$CFLAGS -finline-functions"
	fi
	CFLAGS="$CFLAGS -Wall -wd1419 -vec-report0 -opt-report 0"
elif test "$COB_USES_GCC" = "yes" && test "$with_gnu_ld" = "yes"; then
	curr_ldflags="$LDFLAGS"
	LDFLAGS="$LDFLAGS -Wl,-z,relro,-z,now,-O1"
	AC_MSG_CHECKING([for ld bind now option])
	AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[int testint;]])],
		[AC_MSG_RESULT([yes])],
		[LDFLAGS=$curr_ldflags
		 AC_MSG_RESULT([no])])
	unset curr_ldflags
	if test "$enable_debug" != "yes"; then
		CFLAGS="$CFLAGS -finline-functions"
	fi
	CFLAGS="$CFLAGS -fsigned-char -Wall -Wwrite-strings -Wmissing-prototypes -Wno-format-y2k"
fi

if test "x$lt_cv_dlopen_self" != "xyes"; then
	AC_DEFINE([COB_NO_SELFOPEN], [1])
fi

if test "$with_cisam" = "yes"; then
	COB_HAS_ISAM=cisam
elif test "$with_visam" = "yes"; then
	COB_HAS_ISAM=visam
elif test "$with_disam" = "yes"; then
	COB_HAS_ISAM=disam
elif test "$with_vbisam" = "yes"; then
	COB_HAS_ISAM=vbisam
elif test "$with_db" = "yes"; then
	COB_HAS_ISAM=db
elif test "$with_lmdb" = "yes"; then
	COB_HAS_ISAM=lmdb
elif test "$with_index_extfh" = "yes"; then
	COB_HAS_ISAM=index_extfh
else
	COB_HAS_ISAM=no
fi
COB_HAS_CISAM=$with_cisam
COB_HAS_DISAM=$with_disam
COB_HAS_VBISAM=$with_vbisam
COB_HAS_VISAM=$with_visam
COB_HAS_BDB=$with_db
COB_HAS_LMDB=$with_lmdb
COB_HAS_ODBC=$with_odbc
COB_HAS_OCI=$with_oci
COB_HAS_OCEXTFH=$with_index_extfh

if test "$USE_CURSES" = "not_found" -o "$USE_CURSES" = "no"; then
	COB_HAS_CURSES=no
else
	COB_HAS_CURSES=yes
fi

if test "$with_xml2" = "yes"; then
	COB_HAS_XML2=yes
	with_xml2=libxml2
else
	COB_HAS_XML2=no
fi

if test "$USE_JSON" = "local"; then
	COB_HAS_JSON=cjson
elif test "$USE_JSON" = "not_found"; then
	COB_HAS_JSON=no
else
	COB_HAS_JSON=$USE_JSON
fi

AM_CONDITIONAL([COB_MAKE_CISAM_LIB], [test "$cob_gen_cisam" = "yes" -a "$static_isam" = "no"])
AM_CONDITIONAL([COB_MAKE_DISAM_LIB], [test "$cob_gen_disam" = "yes" -a "$static_isam" = "no"])
AM_CONDITIONAL([COB_MAKE_VBISAM_LIB], [test "$cob_gen_vbisam" = "yes" -a "$static_isam" = "no"])
AM_CONDITIONAL([COB_MAKE_VISAM_LIB], [test "$cob_gen_visam" = "yes" -a "$static_isam" = "no"])
AM_CONDITIONAL([COB_MAKE_BDB_LIB], [test "$cob_gen_bdb" = "yes"])
AM_CONDITIONAL([COB_MAKE_ODBC_LIB], [test "$cob_gen_odbc" = "yes"])
AM_CONDITIONAL([COB_MAKE_OCI_LIB], [test "$cob_gen_oci" = "yes"])
AM_CONDITIONAL([COB_MAKE_LMDB_LIB], [test "$cob_gen_lmdb" = "yes"])
AM_CONDITIONAL([COB_MAKE_STATIC_ISAM], [test "$static_isam" = "yes"])

AM_CONDITIONAL([LOCAL_CJSON],[test "$USE_JSON" = "local"])

unset COB_USES_GCC
unset COB_USES_GCC_NO_ICC
unset COB_USES_ICC_ONLY
unset COB_USES_CLANG_ONLY
unset COB_USES_XLC_ONLY
unset COB_USES_WATCOM_ONLY
unset curr_cflags
unset curr_cppflags
unset curr_libs

# Generate the output

AM_CONDITIONAL([COB_MAKE_IX], [test "$COB_HAS_ISAM" != no])
AM_CONDITIONAL([COB_MAKE_RUN_BINARIES], [test "$cross_compiling" != yes])
# FIXME: Should be tested as the system may can actually run these (Bash on Windows?)

AC_DEFINE_UNQUOTED([COB_CC],			["$COB_CC"],		[compiler used by cobc])
AX_AC_DEFINE_RESOLVED([COB_CFLAGS],		[$COB_CFLAGS],		[compiler flags passed to compiler by cobc])
AX_AC_DEFINE_RESOLVED([COB_LDFLAGS],	[$COB_LDFLAGS],		[linker flags passed to linker by cobc])
AX_AC_DEFINE_RESOLVED([COB_LIBS],		[$COB_LIBS],		[libraries passed to linker by cobc])
AC_DEFINE_UNQUOTED([COB_PIC_FLAGS],		["$COB_PIC_FLAGS"])
AC_DEFINE_UNQUOTED([COB_DEBUG_FLAGS],	["$COB_DEBUG_FLAGS"])
AC_DEFINE_UNQUOTED([COB_EXPORT_DYN],	["$COB_EXPORT_DYN"])
AC_DEFINE_UNQUOTED([COB_SHARED_OPT],	["$COB_SHARED_OPT"])
AC_DEFINE_UNQUOTED([COB_MODULE_EXT],	["$COB_MODULE_EXT"])
AC_DEFINE_UNQUOTED([COB_OBJECT_EXT],	["$OBJEXT"])
AC_DEFINE_UNQUOTED([COB_EXE_EXT],		["$COB_EXE_EXT"])
AX_AC_DEFINE_RESOLVED([COB_LIBRARY_PATH],	[$COB_LIBRARY_PATH], 	[default search path for extra modules])
if test "x$striplib" != "x"; then
	AC_DEFINE_UNQUOTED([COB_STRIP_CMD], ["$striplib"])
fi
AX_AC_DEFINE_RESOLVED([COB_CONFIG_DIR],	[$COB_CONFIG_DIR],	[default search path for copybooks])
AX_AC_DEFINE_RESOLVED([COB_COPY_DIR],	[$COB_COPY_DIR],	[default search path for configuration files])
AX_AC_DEFINE_RESOLVED([COB_SCHEMA_DIR],	[$COB_SCHEMA_DIR],	[default search path for indexed schema definitions])

#AX_AC_DEFINE_RESOLVED([DATADIR],		[$datarootdir], 	[path for configuration files])
#AX_AC_DEFINE_RESOLVED([LOCALEDIR],		[$localedir], 		[path for locale-dependent data])

#AC_DEFINE_UNQUOTED([COBC_NAME],	["$(echo cobc    | $SED "$program_transform_name")"], [executable name for compiler])
AC_DEFINE_UNQUOTED([COBCRUN_NAME],	["$(echo cobcrun | $SED "$program_transform_name")"], [executable name for module runner])

# informational only
AC_DEFINE_UNQUOTED([COB_BLD_CC],		["$CC"],		[informational: compiler during compilation])
AC_DEFINE_UNQUOTED([COB_BLD_CFLAGS],	["$CFLAGS"],	[informational: compiler flags during compilation])
AC_DEFINE_UNQUOTED([COB_BLD_CPPFLAGS],	["$CPPFLAGS"],	[informational: preparser flags during compilation])
AC_DEFINE_UNQUOTED([COB_BLD_LD],		["$LD"],		[informational: linker during compilation])
AC_DEFINE_UNQUOTED([COB_BLD_LDFLAGS],	["$LDFLAGS"],	[informational: linker flags during compilation])
AC_DEFINE_UNQUOTED([COB_BLD_BUILD],		["$build"],		[informational: build environment during compilation])

AC_SUBST([COB_CC])
AC_SUBST([COB_CFLAGS])
AC_SUBST([COB_LDFLAGS]) # needed for pre-inst-env
AC_SUBST([COB_LIBS])
AC_SUBST([COB_CONFIG_DIR])
AC_SUBST([COB_SCHEMA_DIR])
AC_SUBST([COB_COPY_DIR])
AC_SUBST([COB_LIBRARY_PATH])
AC_SUBST([COB_OBJECT_EXT])
AC_SUBST([COB_MODULE_EXT])
AC_SUBST([COB_EXE_EXT])
AC_SUBST([PROGRAMS_LIBS])
AC_SUBST([LIBCOB_LIBS])
AC_SUBST([LIBCOB_CPPFLAGS])
AC_SUBST([CISAM_LIBS])
AC_SUBST([DISAM_LIBS])
AC_SUBST([VBISAM_LIBS])
AC_SUBST([VISAM_LIBS])
AC_SUBST([ODBC_LIBS])
AC_SUBST([OCI_LIBS])
AC_SUBST([BDB_LIBS])
AC_SUBST([LMDB_LIBS])
AC_SUBST([CISAM_CFLAGS])
AC_SUBST([DISAM_CFLAGS])
AC_SUBST([VBISAM_CFLAGS])
AC_SUBST([VISAM_CFLAGS])
AC_SUBST([ODBC_CFLAGS])
AC_SUBST([OCI_CFLAGS])
AC_SUBST([BDB_CFLAGS])
AC_SUBST([LMDB_CFLAGS])

# was used in bin/Makefile.am - seems not to be needed
#AC_SUBST([COB_EXPORT_DYN])

# used in the testsuite
COB_BIGENDIAN="$ac_cv_c_bigendian"
AC_SUBST([COB_BIGENDIAN])

#AC_SUBST([COB_FIX_LIB])
AC_SUBST([COB_FIX_LIBTOOL])

AC_SUBST([COB_HAS_ISAM])
AC_SUBST([COB_HAS_CISAM])
AC_SUBST([COB_HAS_DISAM])
AC_SUBST([COB_HAS_VBISAM])
AC_SUBST([COB_HAS_VISAM])
AC_SUBST([COB_HAS_BDB])
AC_SUBST([COB_HAS_LMDB])
AC_SUBST([COB_HAS_ODBC])
AC_SUBST([COB_HAS_OCI])
AC_SUBST([COB_HAS_OCEXTFH])
AC_SUBST([COB_HAS_CURSES])
AC_SUBST([COB_HAS_XML2])
AC_SUBST([COB_HAS_JSON])
AC_SUBST([COB_HAS_64_BIT_POINTER])
AC_SUBST([COB_PATCH_LEVEL], [$with_patch_level]) # needed for bin/cob-config

AC_CONFIG_COMMANDS([chmod],
[chmod +x bin/cob-config;
chmod +x tests/atconfig;
chmod +x tests/atlocal])

AM_MISSING_PROG([HELP2MAN], [help2man])

AC_OUTPUT

AC_MSG_NOTICE([GnuCOBOL Configuration:])
AC_MSG_NOTICE([ CC                ${CC}])
AC_MSG_NOTICE([ CFLAGS            ${CFLAGS}])
AC_MSG_NOTICE([ LDFLAGS           ${LDFLAGS}])
if test "x$PROGRAMS_LIBS" != "x"; then
  AC_MSG_NOTICE([ PROGRAMS_LIBS         ${PROGRAMS_LIBS}])
fi
AC_MSG_NOTICE([ COB_CC            ${COB_CC}])
AC_MSG_NOTICE([ COB_CFLAGS        ${COB_CFLAGS}])
AC_MSG_NOTICE([ COB_LDFLAGS       ${COB_LDFLAGS}])
AC_MSG_NOTICE([ COB_DEBUG_FLAGS   ${COB_DEBUG_FLAGS}])
AC_MSG_NOTICE([ COB_LIBS          ${COB_LIBS}])
AC_MSG_NOTICE([ COB_CONFIG_DIR    ${COB_CONFIG_DIR}])
AC_MSG_NOTICE([ COB_COPY_DIR      ${COB_COPY_DIR}])
AC_MSG_NOTICE([ COB_SCHEMA_DIR    ${COB_SCHEMA_DIR}])
AC_MSG_NOTICE([ COB_LIBRARY_PATH  ${COB_LIBRARY_PATH}])
AC_MSG_NOTICE([ COB_OBJECT_EXT    ${OBJEXT}])
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
AC_MSG_NOTICE([ Use math multiple precision library:         ${USE_MATH}])
case "$USE_CURSES" in
  not_found)
    AC_MSG_NOTICE([ screen I/O (no curses found):                NO])
	;;
  no)
    AC_MSG_NOTICE([ screen I/O (disabled):                       NO])
	;;
  *)
    AC_MSG_NOTICE([ Use curses library for screen I/O:           ${USE_CURSES}])
	;;
esac
if test "$with_debug_log" = "yes"; then
  AC_MSG_NOTICE([ Enable GnuCOBOL developer logging            yes])
fi
if test "$with_seqra_extfh" = "yes"; then
  AC_MSG_NOTICE([ Use external SEQ/RAN file handler:           yes])
  AC_MSG_WARN([ Obsolete feature "OpenCOBOL-only-EXTFH" used!])
fi
if test "$with_index_extfh" = "yes"; then
  AC_MSG_NOTICE([ Use external INDEXED file handler            yes])
  AC_MSG_WARN([ Obsolete feature "OpenCOBOL-only-EXTFH" used!])
fi
if test "$with_visam" = "yes"; then
  AC_MSG_NOTICE([ Use V-ISAM for INDEXED I/O                   yes])
fi
if test "$with_disam" = "yes"; then
  AC_MSG_NOTICE([ Use D-ISAM for INDEXED I/O                   yes])
fi
if test "$with_cisam" = "yes"; then
  AC_MSG_NOTICE([ Use C-ISAM for INDEXED I/O                   yes])
fi
if test "$with_vbisam" = "yes"; then
  AC_MSG_NOTICE([ Use VB-ISAM for INDEXED I/O                  yes])
fi
if test "$with_db" = "yes"; then
  AC_MSG_NOTICE([ Use Berkeley DB for INDEXED I/O:             yes])
fi
if test "$with_lmdb" = "yes"; then
  AC_MSG_NOTICE([ Use LMDB for INDEXED I/O:                    yes])
fi
if test "$with_odbc" = "yes"; then
  AC_MSG_NOTICE([ Use ODBC for INDEXED I/O:                    yes])
fi
if test "$with_oci" = "yes"; then
  AC_MSG_NOTICE([ Use Oracle DB (OCI) for INDEXED I/O:         yes])
fi

if test "$COB_HAS_ISAM" = no; then
  AC_MSG_NOTICE([ INDEXED I/O (no handler configured):         NO])
elif test "$static_indexed" = yes; then
  AC_MSG_NOTICE([ INDEXED handler linked with libcob             ])
fi

case "$with_xml2" in
  not_found)
    AC_MSG_NOTICE([ XML I/O (no handler found):                  NO])
	;;
  no)
    AC_MSG_NOTICE([ XML I/O (disabled):                          NO])
	;;
  *)
    AC_MSG_NOTICE([ Used for XML I/O:                            ${with_xml2}])
	;;
esac
case "$USE_JSON" in
  not_found)
    AC_MSG_NOTICE([ JSON I/O (no handler found):                 NO])
	;;
  no)
    AC_MSG_NOTICE([ JSON I/O (disabled):                         NO])
	;;
  *)
    AC_MSG_NOTICE([ Used for JSON I/O:                           ${USE_JSON}])
	;;
esac

unset DEFINE_DL

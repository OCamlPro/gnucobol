dnl
dnl Configure template for GnuCOBOL
dnl Process this file with autoconf to produce a configure script.
dnl
dnl Copyright (C) 2001-2012, 2014-2023 Free Software Foundation, Inc.
dnl Written by Keisuke Nishida, Roger While, Ron Norman, Simon Sobisch,
dnl Edward Hart
dnl
dnl This file is part of GnuCOBOL.
dnl
dnl The GnuCOBOL compiler is free software: you can redistribute it
dnl and/or modify it under the terms of the GNU General Public License
dnl as published by the Free Software Foundation, either version 3 of the
dnl License, or (at your option) any later version.
dnl
dnl GnuCOBOL is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with GnuCOBOL.  If not, see <https://www.gnu.org/licenses/>.
dnl

AC_PREREQ([2.67])  # note: 2.67 is the one found in old msys, 2.69 is commonly available

AC_INIT([GnuCOBOL],
	[3.2-dev],
	[bug-gnucobol@gnu.org],
	[gnucobol],
	[https://www.gnu.org/software/gnucobol/])
AC_REVISION([GnuCOBOL snapshot $Revision$])
AC_COPYRIGHT([This file is part of GnuCOBOL.

Copyright (C) 2001-2012, 2014-2023 Free Software Foundation, Inc.
Written by Keisuke Nishida, Roger While, Ron Norman, Simon Sobisch,
Edward Hart
])
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

# In general: don't export/setenv but pass as option to configure
#             this has the benefit that re-runs will take the same and "sudo"
#             or later "make" (possibly as different user) will use the same
#             set of tools
# Note for SUN Solaris (gcc)
#  options to configure: CC="gcc -m64" --libdir=/usr/local/lib/sparcv9
# or:
#  options to configure: CFLAGS=-m64 LDFLAGS="-m64 -L/usr/local/lib/sparcv9"
#
# Hack for AIX 64 bit (gcc)
# Required -
#  options to configure: CC="gcc -maix64" / CC="xlc -q64"
# or:
#  options to configure: CFLAGS=-maix64 and LDFLAGS=-maix64
# Note: AIX commonly uses -Lpath like GNU/Linux would use -Lpath -Rpath

if test "x$OBJECT_MODE" = x; then
	echo "$CC $CFLAGS" | grep ' -maix32' 1>/dev/null 2>&1
	check1=$?
	echo "$CC $CFLAGS" | grep ' -q32'    1>/dev/null 2>&1
	check2=$?
	if test $check1 -eq 0 -o $check2 -eq 0; then
		OBJECT_MODE=32 # for libtool
	fi
	echo "$CC $CFLAGS" | grep ' -maix64' 1>/dev/null 2>&1
	check1=$?
	echo "$CC $CFLAGS" | grep ' -q64'    1>/dev/null 2>&1
	check2=$?
	if test $check1 -eq 0 -o $check2 -eq 0; then
		OBJECT_MODE=64 # for libtool
	fi
	unset check1
	unset check2
fi

if test "$OBJECT_MODE" = "64"; then
	if test "x$AR" = x; then
		if test -f /usr/ccs/bin/ar; then
			AR="/usr/ccs/bin/ar -X64"
		else
			AR="ar -X64"
		fi
	fi
	if test "x$NM" = x; then
		if test -f /usr/ccs/bin/nm; then
			NM="/usr/ccs/bin/nm -X64 -B"
		else
			NM="nm -X64 -B"
		fi
	fi
elif test "$OBJECT_MODE" = "32"; then
	if test "x$AR" = x; then
		if test -f /usr/ccs/bin/ar; then
			AR="/usr/ccs/bin/ar -X32"
		else
			AR="ar -X32"
		fi
	fi
	if test "x$NM" = x; then
		if test -f /usr/ccs/bin/nm; then
			NM="/usr/ccs/bin/nm -X32 -B"
		else
			NM="nm -X32 -B"
		fi
	fi
fi

dnl We don't want to have the full list of automatic defines from automake,
dnl and take care of the one we want explicit AC_DEFINE_UNQUOTED.
dnl While we do care about portability we get a long list of false-positives
dnl because of code-coverage parts that are only part of the final Makefile if we
dnl actually do use GNU make, so skip those warnings here and manually check with
dnl "automake -Wall" before creating a release quality tarball.
AM_INIT_AUTOMAKE([gnu no-texinfo.tex no-define -Wno-portability])
AC_DEFINE_UNQUOTED([PACKAGE], ["$PACKAGE"], [Name of package])  # used for bindtextdomain and in .rc-files


dnl Autoheader templates
AH_TEMPLATE([COB_DEBUG_LOG], [Enable internal logging (Developers only!)])
AH_TEMPLATE([COB_EXPERIMENTAL], [Enable experimental code (Developers only!)])
AH_TEMPLATE([COB_TREE_DEBUG], [Enable extra checks within the compiler (Developers only!)])
AH_TEMPLATE([COB_PARAM_CHECK], [Enable minimum parameter check for system libraries])
AH_TEMPLATE([PATCH_LEVEL], [Define a patch level (numeric, max. 8 digits)])
AH_TEMPLATE([MAX_CALL_FIELD_PARAMS], [Define maximum parameters for CALL])
AH_TEMPLATE([WITH_MATH], [Math multiple precision library])
AH_TEMPLATE([WITH_INDEX_EXTFH], [Compile with obsolete external INDEXED handler])
AH_TEMPLATE([WITH_SEQRA_EXTFH], [Compile with obsolete external SEQ/RAN handler])
AH_TEMPLATE([WITH_DB], [Use Berkeley DB library as INDEXED handler])
dnl FIXME: may be specified with different file version
dnl AH_TEMPLATE([COB_BDB_BAD_DUPNO], [Retain incorrect duplicate sequence for compatibility on little endian!])
AH_TEMPLATE([WITH_CISAM], [Use CISAM as INDEXED handler])
AH_TEMPLATE([WITH_DISAM], [Use DISAM as INDEXED handler])
AH_TEMPLATE([WITH_VBISAM], [Use VBISAM as INDEXED handler])

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
AH_TEMPLATE([WITH_VARSEQ], [Define variable sequential file format])
AH_TEMPLATE([HAVE_ATTRIBUTE_ALIGNED], [Has __attribute__((aligned))])
AH_TEMPLATE([HAVE_ATTRIBUTE_PURE], [Has __attribute__((pure))])
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
AH_TEMPLATE([HAVE_CURSES_FREEALL], [curses provides function to free all memory])
AH_TEMPLATE([HAVE_USE_LEGACY_CODING], [ncurses has use_legacy_coding function])
AH_TEMPLATE([HAVE_DESIGNATED_INITS], [Has designated initializers])
AH_TEMPLATE([HAVE_NANO_SLEEP], [Has nanosleep function])
AH_TEMPLATE([HAVE_CLOCK_GETTIME], [Has clock_gettime function and CLOCK_REALTIME])
AH_TEMPLATE([HAVE_ISFINITE], [Has isfinite function])
AH_TEMPLATE([HAVE_MP_GET_MEMORY_FUNCTIONS], [Do we have mp_get_memory_functions in GMP/MPIR])
dnl done via AC_CHECK_FUNCS: AH_TEMPLATE([HAVE_RAISE], [Has raise function])
AH_TEMPLATE([HAVE_FINITE_IEEEFP_H],
	[Declaration of finite function in ieeefp.h instead of math.h])

dnl preparation for cross-compilation
AC_ARG_PROGRAM

dnl
dnl Configure options part I (not needing any compilation)
dnl

AC_ARG_ENABLE([debug],
  [AS_HELP_STRING([--enable-debug],
    [(GnuCOBOL) Enable -g C compiler debug option for cobc + libcob])],
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
    [(GnuCOBOL) Enable experimental code (Developers only!)])],
  [if test "$enable_experimental" = yes; then
	AC_DEFINE([COB_EXPERIMENTAL], [1])
   fi],
  [])

AC_ARG_ENABLE([cobc-internal-checks],
  [AS_HELP_STRING([--enable-cobc-internal-checks],
    [(GnuCOBOL) Enable extra checks within the compiler (Developers only!)])],
  [if test "$enable_cobc_internal_checks" = yes; then
	AC_DEFINE([COB_TREE_DEBUG], [1])
   fi],
  [])

AC_ARG_ENABLE([debuglog],
  [AS_HELP_STRING([--enable-debuglog],
    [(GnuCOBOL) Enable internal logging code (Developers only!)])],
  [if test "$enable_debuglog" = yes; then
	AC_DEFINE([COB_DEBUG_LOG], [1])
   fi],
  [])

AC_ARG_ENABLE([param-check],
  [AS_HELP_STRING([--enable-param-check],
    [(GnuCOBOL) Enable minimum parameter check for system libraries (default no)])],
  [if test "$enable_param_check" = yes; then
	AC_DEFINE([COB_PARAM_CHECK], [1])
   fi],
  [])

dnl FIXME: may be specified with different file version
dnl AC_ARG_ENABLE([bdbdupno],
dnl  [AS_HELP_STRING([--enable-bdbdupno],
dnl    [(GnuCOBOL) Retain old BDB duplicate sequence (Incorrect on little-endian!)])],
dnl  [if test "$enable_bdbdupno" = yes; then
dnl	AC_DEFINE([COB_BDB_BAD_DUPNO], [1])
dnl   fi],
dnl  [])

AC_ARG_WITH([patch-level],
  [AS_HELP_STRING([--with-patch-level],
    [(GnuCOBOL) Define a patch level (default 0), numeric, max. 8 digits])],
  [case $with_patch_level in
    yes)	AC_MSG_ERROR([[--with-patch-level needs an argument]])
		;;
    no)		AC_MSG_ERROR([[--without-patch-level not supported]])
		;;
    [[0-9]]*)
		if test $with_patch_level -gt 99999999; then
    		AC_MSG_ERROR([[patch level must not contain more than 8 digits]])
		fi
		;;
    *)		AC_MSG_ERROR([[patch level must be numeric]])
		;;
  esac],
  [with_patch_level=0])

AC_DEFINE_UNQUOTED([PATCH_LEVEL], [$with_patch_level])

AC_ARG_WITH([max-call-params],
  [AS_HELP_STRING([--with-max-call-params],
    [(GnuCOBOL) Define maximum number of parameters for CALL (default 192)])],
  [case $with_max_call_params in
    yes)	AC_MSG_ERROR([[--with-max-call-params needs an argument]])
		;;
    no)		AC_MSG_ERROR([[--without-max-call-params not supported]])
		;;
    16 | 36 | 56 | 76 | 96 | 192 | 252)
		;;
    *)		AC_MSG_ERROR([[maximum number of parameters for CALL must be one of 16/36/56/76/96/192/252]])
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

AC_MSG_CHECKING([whether ${MAKE} can handle common constructs])
rm -f confmake.out
cat > confmf.mk << 'END'
NOPROG = ${SHELL} './missing' no-prog
DEFAULTVAR = $${VAR-default}
command = \
  somestrip=`echo "$(DEFAULTVAR)" | sed 's/[].[^$$\\*|]/\\\\&/g'`
test_target:
	@echo target executed > confmake.out
END
_am_result=no
  AM_RUN_LOG([${MAKE} -f confmf.mk])
  ## redirecting via echo to remove special chars
  __am_checkme=$(echo `cat confmake.out 2>/dev/null`)
  AS_CASE([$?:${__am_checkme}],
  	['0:target executed'],
  		[AC_MSG_RESULT([yes])],
      [AC_MSG_RESULT([no])
       AC_MSG_FAILURE([make, configured as "$MAKE", does not work])])
rm -f confmake.out confmf.mk

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
  		[_am_result=yes])
rm -f confinc.* confmf.mk confprereq
AC_MSG_RESULT([${_am_result}])
AM_CONDITIONAL([MAKE_HAS_PREREQ_ONLY], [test "${_am_result}" = yes])

curr_libs="$LIBS"

dnl Checked before AC_PROG_CC and default flags are applied.
enable_cflags_setting=no
AC_MSG_CHECKING([whether CFLAGS -O can be modified])
# Enable setting if the user has not specified the optimisation in CFLAGS.
echo "$CFLAGS" | grep "\-O.*\( \|$\)" 1>/dev/null 2>/dev/null
if test $? != 0; then
	enable_cflags_setting=yes
fi
AC_MSG_RESULT([$enable_cflags_setting])

dnl Default CFLAGS (removed -g set in AC_INIT for compatibility)
: ${CFLAGS="-O2"}

AC_PROG_CC([gcc icc xlc cc])

dnl no explict usage of CPP var
dnl AC_PROG_CPP

# save what we have so far as it is quite "constant"
AC_CACHE_SAVE

AC_USE_SYSTEM_EXTENSIONS
#
## general flags for compiler and linker and tests for those
#
# Note:  this and the following checks don't check for stderr and don't need to produce
#        a working compiler - but need to produce something that passes  make check
# FIXME: add checking with stderr here, add an option to skip this part with a warning message

AC_ARG_VAR([COBC_CPPFLAGS], [see CPPFLAGS, but only applied during generation of cobc])
AS_IF([test "x$COBC_CPPFLAGS" != x], [
	AC_MSG_CHECKING([whether compilation with COBC_CPPFLAGS works])
	curr_cppflags="$CPPFLAGS"
	CPPFLAGS="$CPPFLAGS $COBC_CPPFLAGS"
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],
		[[static int i = 1;]])],
		[AC_MSG_RESULT([yes])],
		[AC_MSG_RESULT([no])
		AC_MSG_FAILURE([cannot compile with COBC_CPPFLAGS="$COBC_CPPFLAGS"])])
	CPPFLAGS="$curr_cppflags"
])
AC_ARG_VAR([LIBCOB_CPPFLAGS], [see CPPFLAGS, but only applied during generation of libcob])
AS_IF([test "x$LIBCOB_CPPFLAGS" != x], [
	AC_MSG_CHECKING([whether compilation with LIBCOB_CPPFLAGS works])
	curr_cppflags="$CPPFLAGS"
	CPPFLAGS="$CPPFLAGS $LIBCOB_CPPFLAGS"
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],
		[[static int i = 1;]])],
		[AC_MSG_RESULT([yes])],
		[AC_MSG_RESULT([no])
		AC_MSG_FAILURE([cannot compile with LIBCOB_CPPFLAGS="$LIBCOB_CPPFLAGS"])])
	CPPFLAGS="$curr_cppflags"
])

AS_IF([test "x$LDFLAGS" != "x" -o "x$LIBS" != "x"], [
	AC_MSG_CHECKING([whether linking with LDFLAGS / LIBS works])
	AC_LINK_IFELSE([AC_LANG_PROGRAM([],
		  [[return 0;]])],
		  [AC_MSG_RESULT([yes])],
		  [AC_MSG_RESULT([no])
		   AC_MSG_FAILURE([cannot link with LDFLAGS="$LDFLAGS", / LIBS="$LIBS"])])
])

curr_libs="$LIBS"
AC_ARG_VAR([LIBCOB_LIBS], [see LIBS, but only applied during generation of libcob])
AS_IF([test "x$LIBCOB_LIBS" != x], [
	AC_MSG_CHECKING([whether linking with LIBCOB_LIBS works])
	LIBS="$LIBS $LIBCOB_LIBS"
	AC_LINK_IFELSE([AC_LANG_PROGRAM([],
		  [[return 0;]])],
		  [AC_MSG_RESULT([yes])],
		  [AC_MSG_RESULT([no])
		   AC_MSG_FAILURE([cannot link with LIBCOB_LIBS="$LIBCOB_LIBS"])])
])

AC_ARG_VAR([PROGRAMS_LIBS], [see LIBS, but only applied during generation of programs: cobc, cobcrun])
AS_IF([test "x$PROGRAMS_LIBS" != x], [
	AC_MSG_CHECKING([whether linking with PROGRAMS_LIBS works])
	LIBS="$LIBS $PROGRAMS_LIBS"
	AC_LINK_IFELSE([AC_LANG_PROGRAM([],
		  [[return 0;]])],
		  [AC_MSG_RESULT([yes])],
		  [AC_MSG_RESULT([no])
		   AC_MSG_FAILURE([cannot link with PROGRAMS_LIBS="$PROGRAMS_LIBS"])])
])
LIBS="$curr_libs"

dnl only needed until automake 1.14 - but we don't have a check for the
dnl                                   minimal version, so leaving in
AM_PROG_CC_C_O

LT_PREREQ([2.2])
LT_PROG_RC

AM_PROG_AR
dnl CHECKME: do we want disable-static (mind libsupport) 
dnl          if yes then drop AM_PROG_AR + build_aux/ar-lib
LT_INIT([dlopen win32-dll])

AX_PROG_FLEX
AX_PROG_BISON

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
AC_DEFUN([AC_PROG_CXX], [])
AC_DEFUN([AC_PROG_F77], [])

AC_PROG_LN_S
AC_PROG_INSTALL

AC_PROG_MAKE_SET
AC_LIB_RPATH

# more things to save...
AC_CACHE_SAVE

# Checks for header files.
dnl entries only used from external includes -> likely no need to check here
dnl AC_CHECK_HEADERS([stdint.h whcar.h malloc.h])
# mandatory:
AC_CHECK_HEADERS([sys/types.h signal.h stddef.h], [],
	[AC_MSG_ERROR([mandatory header could not be found or included])])
# optional:
AC_CHECK_HEADERS([sys/time.h locale.h fcntl.h dlfcn.h])


# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_BIGENDIAN
AC_TYPE_SIZE_T
AC_STRUCT_TM
AC_CHECK_TYPES([sig_atomic_t], [], [], [[#include <signal.h>]])

dnl Note: Don't use AC_C_INLINE here. We need the value.

AC_CACHE_CHECK([for inline keyword], gc_cv_keyword_inline, [
  for gc_cv_keyword_inline in __inline __inline__ inline ""
  do
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
		#ifndef __cplusplus
		typedef int foo_t;
		static $gc_cv_keyword_inline foo_t foo () { return 0; }
		#endif]], [[
		#ifndef __cplusplus
		return foo ();
		#else
		choke me
		#endif]])],
		[break],
		[],
		[])
  done]
)
AC_DEFINE_UNQUOTED([COB_KEYWORD_INLINE], [$gc_cv_keyword_inline])


# Checks for library functions.
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([memmove memset setlocale fcntl strerror strcasecmp \
	strchr strrchr strdup strstr strtol gettimeofday localeconv \
	getexecname canonicalize_file_name popen raise readlink realpath \
	setenv strcoll flockfile])

# Check for timezone
AC_CACHE_CHECK([for timezone variable access], gc_cv_time_timezone, [
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <time.h>]],
	[[timezone = 3600;]])],
	[gc_cv_time_timezone=yes],
	[gc_cv_time_timezone=no]
  )]
)
if test "$gc_cv_time_timezone" = yes; then
	AC_DEFINE([HAVE_TIMEZONE], [1])
fi

# Check for designated initializers
AC_CACHE_CHECK([for designated initializers], gc_cv_designated_inits, [
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <stdio.h>]],
	[[static const unsigned char valid_char[256] = {
		@<:@'0'@:>@ = 1,
		@<:@'1'@:>@ = 1 };
	]])],
	[gc_cv_designated_inits=yes],
	[gc_cv_designated_inits=no]
  )]
)
if test "$gc_cv_designated_inits" = yes; then
	AC_DEFINE([HAVE_DESIGNATED_INITS], [1])
fi

# more things to save...
AC_CACHE_SAVE

# Check for the compiler used
dnl COB_USES_GCC=no
COB_USES_GCC_NO_ICC=no
COB_USES_ICC_ONLY=no
COB_USES_CLANG_ONLY=no
COB_USES_XLC_ONLY=no
COB_USES_WATCOMC_ONLY=no

dnl AC_MSG_CHECKING([for __GNUC__])
dnl AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
dnl 	#ifndef __GNUC__
dnl 	# error macro not defined
dnl 	#endif]])],
dnl 	[COB_USES_GCC=yes
dnl 	 AC_MSG_RESULT([yes])],
dnl 	[AC_MSG_RESULT([no])])
COB_USES_GCC=$GCC

AC_MSG_CHECKING([for __INTEL_COMPILER])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
	#ifndef __INTEL_COMPILER
	# error macro not defined
	#endif]])],
	[COB_USES_ICC_ONLY=yes
	 AC_MSG_RESULT([yes])],
	[if test "$COB_USES_GCC" = yes; then
		COB_USES_GCC_NO_ICC=yes
	 fi
	 AC_MSG_RESULT([no])]
	)

AC_MSG_CHECKING([for __clang__])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
	#ifndef __clang__
	# error macro not defined
	#endif]])],
	[COB_USES_CLANG_ONLY=yes
	 AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])])

AC_MSG_CHECKING([for __xlc__])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
	#ifndef __xlc__
	# error macro not defined
	#endif]])],
	[COB_USES_XLC_ONLY=yes
	 AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])])

AC_MSG_CHECKING([for __WATCOMC__])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
	#ifndef __WATCOMC__
	# error macro not defined
	#endif]])],
	[COB_USES_WATCOMC_ONLY=yes
	 AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])])

if test "x$COB_CC" = x; then
	COB_CC="$CC"
fi

if test "x$COMMON_LIBS" != x; then
	if test "x$LIBCOB_LIBS" != x; then
		LIBCOB_LIBS="$LIBCOB_LIBS $COMMON_LIBS"
	else
		LIBCOB_LIBS="$COMMON_LIBS"
	fi
	case " $COMMON_LIBS " in
	  *" -L$libdir "*) COB_LIBS="$COMMON_LIBS -lcob" ;;
	  *) COB_LIBS="-L$libdir $COMMON_LIBS -lcob" ;;
	esac
else
	COB_LIBS="-L$libdir -lcob"
fi

if test "x$LDFLAGS" != x; then
	if test "x$COB_LDFLAGS" != x; then
		COB_LDFLAGS="$COB_LDFLAGS $LDFLAGS"
	else
		COB_LDFLAGS="$LDFLAGS"
	fi
fi

# some math functions (fabs, isnan, isinf) are used in numeric.c
# (all other computations are used from GMP)
#
AC_MSG_NOTICE([Checks for math library ...])
# FIXME - Check for necessary math lib - in most cases they are part of the C library
# for now: explicit pass them to configure via MATH_LIBS
dnl if test "x$MATH_LIBS" = "x"; then
dnl 	if test "$COB_USES_ICC_ONLY" = yes; then
dnl 		MATH_LIBS="-limf -lm"
dnl 	else
dnl 		curr_libs=$LIBS
dnl 		AC_SEARCH_LIBS([cos], [m], [], [
dnl 		  AC_MSG_ERROR([unable to find the cos() function])
dnl 		])
dnl 		if test "x$curr_libs" = x; then
dnl 			MATH_LIBS=$LIBS
dnl 		else
dnl 			MATH_LIBS=$(echo "$LIBS" | sed 's#'"$curr_libs"'##')
dnl 		fi
dnl 		LIBS=$curr_libs
dnl 	fi
dnl fi
if test "x$MATH_LIBS" = x; then
	AC_MSG_NOTICE([none needed])
else
	AC_MSG_NOTICE([using $MATH_LIBS])
	case " $LIBCOB_LIBS " in
	  *" $MATH_LIBS "*) ;;
	  *) LIBCOB_LIBS="$LIBCOB_LIBS $MATH_LIBS" ;;
	esac
fi


dnl Add --enable-code-coverage and test for code-coverage tools if enabled
AX_CODE_COVERAGE

if test "$COB_USES_GCC_NO_ICC" != yes -a "$enable_code_coverage" = yes; then
	AC_MSG_ERROR([Code coverage checks are only usable with GCC!])
fi


dnl set PKG_CONFIG to use (cross-compile aware)
PKG_PROG_PKG_CONFIG


# Checks for gmp.
AC_MSG_NOTICE([Checks for GMP/MPIR ...])

AC_ARG_WITH([math],
  [AS_HELP_STRING([[--with-math[=ARG]]],
    [(GnuCOBOL) Use math multiple precision library, where ARG may be:
     check (default), gmp, mpir])],
  [case "$with_math" in
    gmp | mpir | check)
		;;
	yes)
		with_math=check
		;;
	no)
		AC_MSG_ERROR([cannot build GnuCOBOL without math multiple precision library)])
		;;
    *)
		AC_MSG_ERROR([invalid value "$with_math" for --with-math,
  must be one of the following:
  gmp, mpir (use only the specified library)
  check  (use whatever library is usable)])
		;;
  esac],
  [with_math=check])

curr_libs=$LIBS
curr_cppflags=$CPPFLAGS

AS_IF([test "$with_math" = gmp -o "$with_math" = check], [
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
		AC_MSG_CHECKING([whether linking against libgmp with "$GMP_LIBS" works])
		AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <gmp.h>]],
		  [[mpz_t integ; mpz_init (integ);]])],
		  [AC_MSG_RESULT([yes])
		   curr_cppflags=$CPPFLAGS   # for the include in cobc generated modules
		   LIBCOB_LIBS="$LIBCOB_LIBS $GMP_LIBS"
		   with_math=gmp],
		  [AC_MSG_RESULT([no])
		   if test "$with_math" = gmp; then
		     AC_MSG_ERROR([[GMP is required for --with-math=gmp, you may adjust GMP_LIBS]])
		   fi
		   LIBS="$curr_libs"; CPPFLAGS="$curr_cppflags"]
		)
	], [if test "$with_math" = gmp; then
		    AC_MSG_ERROR([gmp.h is required for --with-math=gmp, you may adjust GMP_CFLAGS])
		 fi
		 LIBS="$curr_libs"; CPPFLAGS="$curr_cppflags"
	])
])

AS_IF([test "$with_math" = mpir -o "$with_math" = check], [
	# note: currently MPIR does not ship a .pc file, but is expected to follow GMP
	curr_libs="$LIBS"; curr_cppflags="$CPPFLAGS"
	PKG_CHECK_MODULES([MPIR], [mpir], [], [#
	])
	if test -n "$MPIR_CFLAGS"; then
	  CPPFLAGS="$CPPFLAGS $MPIR_CFLAGS"
	fi
	if test -z "${MPIR_LIBS+x}"; then
	  MPIR_LIBS="-lmpir"
	fi
	LIBS="$LIBS $MPIR_LIBS"
	AC_CHECK_HEADERS([mpir.h], [
		AC_MSG_CHECKING([whether linking against libmpir with "$MPIR_LIBS" works])
		AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <mpir.h>]],
		  [[mpz_t integ; mpz_init (integ);]])],
		  [AC_MSG_RESULT([yes])
		   curr_cppflags=$CPPFLAGS   # for the include in cobc generated modules
		   LIBCOB_LIBS="$LIBCOB_LIBS $MPIR_LIBS"
		   with_math=mpir],
		  [AC_MSG_RESULT([no])
		   if test "$with_math" = mpir; then
		     AC_MSG_ERROR([[MPIR is required for --with-math=mpir, you may adjust MPIR_LIBS]])
		   fi
		   LIBS="$curr_libs"; CPPFLAGS="$curr_cppflags"]
		)
	], [if test "$with_math" = mpir; then
		    AC_MSG_ERROR([mpir.h is required for --with-math=mpir, you may adjust MPIR_CFLAGS])
		 fi
		 LIBS="$curr_libs"; CPPFLAGS="$curr_cppflags"
	])
])

if test "$with_math" = check; then
	AC_MSG_FAILURE([neither GMP nor MPIR library are found, but one of those is mandatory])
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
	[AC_MSG_ERROR([unable to extract GMP version information from gmp_version])],
	[AC_MSG_WARN([matching GMP version assumed])]
	 COB_GMP_LIB="cross")
if test "x$COB_GMP_LIB" = x; then
	AC_MSG_ERROR([unable to extract GMP version information (library)])
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
	[AC_MSG_ERROR([unable to extract GMP version information from header])],
	[AC_MSG_WARN([cannot run test program while cross-compiling])
	 COB_GMP_HEADER="cross-compilation - assumed"])
if test "x$COB_GMP_HEADER" = x; then
	AC_MSG_ERROR([unable to extract GMP version information (header)])
fi

COB_GMP_LIB_MAJOR=$(echo "$COB_GMP_LIB" | cut -d. -f1)
COB_GMP_LIB_MINOR=$(echo "$COB_GMP_LIB" | cut -d. -f2)

if test "$COB_GMP_HEADER" = "$COB_GMP_LIB_MAJOR.$COB_GMP_LIB_MINOR" -o "$COB_GMP_LIB" = "cross"; then
	AC_MSG_RESULT([yes ($COB_GMP_HEADER)])
else
	AC_MSG_RESULT([no (header: $COB_GMP_HEADER / library: $COB_GMP_LIB)])
	AC_MSG_FAILURE([unable to use GMP])
fi

LIBS="$curr_libs"
CPPFLAGS="$curr_cppflags"

AC_CHECK_LIB([$with_math], [__gmp_get_memory_functions],
 [AC_DEFINE([HAVE_MP_GET_MEMORY_FUNCTIONS], [1])], [], [])


dnl
dnl Configure options part II (needing compilation)
dnl

AC_ARG_WITH([xml2],
  [AS_HELP_STRING([--with-xml2],
    [(GnuCOBOL) Use libxml2 as XML handler (default)])],
  [],
  [with_xml2=check])

AS_IF([test "$with_xml2" = yes -o "$with_xml2" = check], [
	PKG_CHECK_MODULES([XML2], [libxml-2.0], [], [
	if test -z "${XML2_CFLAGS+x}" -o -z "${XML2_LIBS+x}"; then
	  AC_PATH_PROG(XML2_CONFIG, xml2-config)
	  if test -n "$XML2_CONFIG"; then
	    if test -z "$XML2_CFLAGS"; then
	      XML2_CFLAGS="$($XML2_CONFIG --cflags)"
	    fi
	    if test -z "$XML2_LIBS"; then
	      XML2_LIBS="$($XML2_CONFIG --libs)"
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
	# note: PKG_CONFIG and xml2-config set -I /path/to/libxml2 which contains a "libxml" folder where
	#       all the files we look for are included
	for header in xmlwriter xmlversion uri parser tree; do
		AC_CHECK_HEADER([libxml/$header.h], [],
			[if test "$with_xml2" = yes; then
			   AC_MSG_ERROR([headers for libxml2 are required for --with-xml2, you may adjust XML2_CFLAGS])
			 else
			   AC_MSG_WARN([headers for libxml2 are missing, you may adjust XML2_CFLAGS])
			   with_xml2=not_found
			   break
			 fi]
		)
	done
	if test "$with_xml2" != "not_found"; then
		AX_CHECK_DEFINE([libxml/xmlwriter.h], [LIBXML_WRITER_ENABLED], [],
		   [if test "$with_xml2" = yes; then
		      AC_MSG_ERROR([libxml2 is required to be configured with xmlWriter])
		    else
		      AC_MSG_WARN([libxml2 is required to be configured with xmlWriter])
		      with_xml2=not_found
		    fi]
		)
	fi
	if test "$with_xml2" != "not_found"; then
		AC_MSG_CHECKING([whether linking against libxml2 with "$XML2_LIBS" works])
		AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <libxml/xmlwriter.h>]],
		  [[xmlNewTextWriterFilename (NULL, 0);]])],
		  [AC_DEFINE([WITH_XML2], [1]) AC_MSG_RESULT([yes])
		   with_xml2=yes
		   LIBCOB_CPPFLAGS="$LIBCOB_CPPFLAGS $XML2_CFLAGS"
		   LIBCOB_LIBS="$LIBCOB_LIBS $XML2_LIBS"],
		  [AC_MSG_RESULT([no])
		   if test "$with_xml2" = yes; then
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
    [(GnuCOBOL) Use json library/source as JSON handler, where ARG may be:
     check (default), cjson, local, json-c, no])],
  [case "$with_json" in
	cjson | local | "json-c" | check | no )
		USE_JSON="$with_json"
		;;
	yes)
		USE_JSON=check
		;;
    *)
		AC_MSG_ERROR([invalid value "$with_json" for --with-json,
  must be one of the following:
  cjson, json-c  (use only the specified library)
  local  (use cjson source included in libcob)
  check  (use whatever json library is usable, disable if no one usable)
  no  (disable json usage)])
		;;
  esac],
  [USE_JSON=check])

curr_libs="$LIBS"
AS_IF([test "$USE_JSON" = "cjson" -o "$USE_JSON" = "local" -o "$USE_JSON" = check], [
	AC_MSG_NOTICE([Checks for local cJSON ...])
	curr_libs="$LIBS"; curr_cppflags="$CPPFLAGS"
	with_cjson_local=no
	AS_IF([test -e ./libcob/cJSON.c],
	  [AC_MSG_CHECKING([whether linking of ./libcob/cJSON.c works])
	   CPPFLAGS="$curr_cppflags -I./libcob"
	   LIBS="$LIBS $COMMON_LIBS"
	   AC_LINK_IFELSE([
		AC_LANG_PROGRAM([[#include "cJSON.c"]],
		               [[#if (CJSON_VERSION_MAJOR * 100 + CJSON_VERSION_MINOR) < 103
		                    #error Needs at least cJSON 1.3.0
		                 #endif
		                 cJSON_CreateNull ();]])],
	   [AC_MSG_RESULT([yes])
	    AC_DEFINE([WITH_CJSON], [1])
	    with_cjson_local="yes (in ./libcob)"],
	   [AC_MSG_RESULT([no])]
	   )
	   LIBS="$curr_libs"]
	)
	if test "$with_cjson_local" = no; then
	  AS_IF([test -e "$srcdir/libcob/cJSON.c"],
	    [AC_MSG_CHECKING([whether linking of "$srcdir/libcob/cJSON.c" works])
	     CPPFLAGS="$curr_cppflags -I\"$srcdir/libcob\""
	     LIBS="$LIBS $COMMON_LIBS"
	     AC_LINK_IFELSE([
		  AC_LANG_PROGRAM([[#include "cJSON.c"]],
		                 [[#if (CJSON_VERSION_MAJOR * 100 + CJSON_VERSION_MINOR) < 103
		                       #error Needs at least cJSON 1.3.0
		                   #endif
		                   cJSON_CreateNull ();]])],
	     [AC_MSG_RESULT([yes])
	      AC_DEFINE([WITH_CJSON], [1])
	      with_cjson_local="yes (in \"$srcdir/libcob\")"],
	     [AC_MSG_RESULT([no])]
	     )
	     LIBS="$curr_libs"]
	  )
	fi
	CPPFLAGS="$curr_cppflags"
	AS_IF([test "$with_cjson_local" = no], [
		AC_MSG_NOTICE([using local cJSON: no])
		if test "$with_json" = "local"; then
		  AC_MSG_ERROR([[cJSON source is required in directory "libcob" for --with-json=local]])
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
		AC_CHECK_HEADERS([cJSON.h cjson/cJSON.h], [break],
			[if test "$with_json" = "cjson"; then
			    AC_MSG_ERROR([header for libcjson is required for --with-json=cjson, you may adjust CJSON_CFLAGS])
			 else
				AC_MSG_WARN([header for libcjson missing, you may adjust CJSON_CFLAGS or put cJSON sources in "libcob"])
			 fi]
		)
		AS_IF([test "x$ac_cv_header_cJSON_h" = xyes -o "x$ac_cv_header_cjson_cJSON_h" = xyes], [
			AC_MSG_CHECKING([whether linking against libcjson with "$CJSON_LIBS" works])
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
		])
	], [
		AC_MSG_NOTICE([using local cJSON: $with_cjson_local])
		USE_JSON="local"
		AC_DEFINE([HAVE_CJSON_H], [1])
	])
	LIBS="$curr_libs"; CPPFLAGS="$curr_cppflags"
])

AS_IF([test "$USE_JSON" = "json-c" -o "$USE_JSON" = check], [
	CPPFLAGS="$curr_cppflags"
	PKG_CHECK_MODULES([JSON_C], [json-c], [], [#
	])
	if test -n "$JSON_C_CFLAGS"; then
	  CPPFLAGS="$CPPFLAGS $JSON_C_CFLAGS"
	fi
	if test -z "${JSON_C_LIBS+x}"; then
	  JSON_C_LIBS="-ljson-c"
	fi
	LIBS="$LIBS $JSON_C_LIBS"
	AC_CHECK_HEADERS([json.h json-c/json.h], [break],
		[if test "$with_json" = "json-c"; then
		    AC_MSG_ERROR([header for libjson-c is required for --with-json=json-c, you may adjust JSON_C_CFLAGS])
		 else
			AC_MSG_WARN([header for libjson-c missing, you may adjust JSON_C_CFLAGS])
		 fi]
	)
	AS_IF([test "x$ac_cv_header_json_h" = xyes -o "x$ac_cv_header_json_c_json_h" = xyes], [
	 AC_MSG_CHECKING([whether linking against libjson-c with "$JSON_C_LIBS" works])
	 AC_LINK_IFELSE([AC_LANG_PROGRAM([
	  [#if defined (HAVE_JSON_C_JSON_H)
	   #include <json-c/json.h>
	   #elif defined (HAVE_JSON_H)
	   #include <json.h>
	   #endif
	   #if (JSON_C_MAJOR_VERSION * 100 + JSON_C_MINOR_VERSION) < 12
	   	#error Needs at least json-c 0.12
	   #endif]],
	   [[json_c_version ();]])],
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
	])
	LIBS="$curr_libs"; CPPFLAGS="$curr_cppflags"
])

case "$USE_JSON" in
  check)
	if test "$with_json" = yes; then
		AC_MSG_FAILURE([[no applicable handler for JSON found]])
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
    [(GnuCOBOL) Use system dynamic loader (default)])],
  [case $with_dl in
    yes)
		;;
    no)
		;;
    *)		AC_MSG_ERROR([[--with/without-dl can not have an argument]])
		;;
   esac],
  [with_dl=yes])

AC_ARG_WITH([varseq],
  [AS_HELP_STRING([--with-varseq],
    [(GnuCOBOL) Define variable sequential format (default 0)])],
  [case $with_varseq in
    yes)	AC_MSG_ERROR([[--with-varseq needs an argument]])
		;;
    no)		AC_MSG_ERROR([[--without-varseq not supported]])
		;;
    [[0-3]])
		;;
    *)		AC_MSG_ERROR([[invalid --with-varseq argument]])
		;;
  esac],
  [with_varseq=0])

AC_DEFINE_UNQUOTED([WITH_VARSEQ], [$with_varseq])


# Solaris has nanosleep in other libraries
AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <time.h>]],
  [[nanosleep (NULL, NULL);]])],
  [AC_DEFINE([HAVE_NANO_SLEEP], [1])],
  [AC_CHECK_LIB([rt], [nanosleep], [], [], [])
   if test "x$ac_cv_lib_rt_nanosleep" = xyes; then
     AC_DEFINE([HAVE_NANO_SLEEP], [1])
     LIBCOB_LIBS="$LIBCOB_LIBS -lrt"
   else
     AC_CHECK_LIB([posix4], [nanosleep], [], [], [])
     if test "x$ac_cv_lib_posix4_nanosleep" = xyes; then
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

dnl  Check for raise (optional) --> done via AC_CHECK_FUNCS
dnl AC_MSG_CHECKING([for raise])
dnl AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <signal.h>]],
dnl   [[raise (SIGINT);]])],
dnl   [AC_DEFINE([HAVE_RAISE], [1]) AC_MSG_RESULT([yes])],
dnl   [AC_MSG_RESULT([no])],
dnl   [])

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
AC_ARG_VAR([CURSES_LIBS], [linker flags for curses lookup, overriding automatic lookup, to be used with explicit selection via --with-curses=arg])

AC_ARG_WITH([curses],
  [AS_HELP_STRING([[--with-curses[=ARG]]],
    [(GnuCOBOL) Use curses library for extended SCREEN I/O, where ARG may be:
     check (default), ncursesw, ncurses, pdcurses, xcurses, curses, no])],
  [case "$with_curses" in
    ncursesw | ncurses | pdcurses | xcurses | curses | check | no)
		USE_CURSES="$with_curses"
		;;
	yes)
		USE_CURSES=check
		;;
    *)
		AC_MSG_ERROR([invalid value "$with_curses" for --with-curses,
  must be one of the following:
  ncursesw, ncurses, pdcurses, xcurses, curses  (use only the specified library)
  check  (use whatever curses library is usable, disable if no one usable)
  no  (disable curses usage)])
		;;
  esac],
  [USE_CURSES=check])

curr_libs="$LIBS" # backup current LIBS for later

if test "x$CURSES_LIBS" != x; then
	LIBS="$CURSES_LIBS $LIBS"
	AC_MSG_CHECKING([whether linking initscr works with CURSES_LIBS, given as "$CURSES_LIBS"])
	AC_LINK_IFELSE([AC_LANG_PROGRAM([[
	extern void *initscr (void);]],
	[[initscr ();]])],
	 [AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])
	  AC_MSG_FAILURE([[not able to link initscr with CURSES_LIBS]])])
	AS_IF([test "$USE_CURSES" = check -o "$USE_CURSES" = no], [
	  AC_MSG_ERROR([[explicit selection with --with-curses=ARG needed, as CURSES_LIBS is set]])
	])
	LIBS=$"curr_libs"
fi

dnl note: when switching to $ncurses all checks are done correctly but no generation in config.h is done
if test "x$CURSES_LIBS" = x; then
  if test "$USE_CURSES" = "ncursesw" -o "$USE_CURSES" = check; then
    AC_CHECK_LIB([ncursesw], [initscr], [true],
      [AS_IF([test "$USE_CURSES" != check], [USE_CURSES="missing_lib"])])
  fi
fi
if test "$USE_CURSES" = "ncursesw" -o "x$ac_cv_lib_ncursesw_initscr" = xyes; then
  dnl note: at least OpenBSD has ncursesw only with only ncurses.h in default header directory
  AC_CHECK_HEADERS([ncursesw/ncurses.h ncursesw/curses.h ncurses.h],
    [USE_CURSES="ncursesw"
     break],
    [AS_IF([test "$USE_CURSES" != check],
       [USE_CURSES="missing_header"]) ])
  AS_IF([test $USE_CURSES="ncursesw" -a "x$CURSES_LIBS" = x],
    [CURSES_LIBS="-l$USE_CURSES"])
fi

if test "x$CURSES_LIBS" = x; then
  if test "$USE_CURSES" = "ncurses" -o "$USE_CURSES" = check; then
    AC_CHECK_LIB([ncurses], [initscr], [true],
      [AS_IF([test "$USE_CURSES" != check], [USE_CURSES="missing_lib"])])
  fi
fi
if test "$USE_CURSES" = "ncurses" -o "x$ac_cv_lib_ncurses_initscr" = xyes; then
  AC_CHECK_HEADERS([ncurses/ncurses.h ncurses/curses.h ncurses.h],
    [USE_CURSES="ncurses"
     break],
    [AS_IF([test "$USE_CURSES" != check],
       [USE_CURSES="missing_header"]) ])
  AS_IF([test $USE_CURSES="ncurses" -a "x$CURSES_LIBS" = x],
    [CURSES_LIBS="-l$USE_CURSES"])
fi

if test "x$CURSES_LIBS" = x; then
  if test "$USE_CURSES" = "pdcurses" -o "$USE_CURSES" = check; then
    AC_CHECK_LIB([pdcurses], [initscr], [true],
      [AS_IF([test "$USE_CURSES" != check], [USE_CURSES="missing_lib"])])
  fi
fi
if test "$USE_CURSES" = "pdcurses" -o "x$ac_cv_lib_pdcurses_initscr" = xyes; then
  AC_CHECK_HEADERS([pdcurses.h curses.h],
    [USE_CURSES="pdcurses"
     break],
    [AS_IF([test "$USE_CURSES" != check],
       [USE_CURSES="missing_header"]) ])
  AS_IF([test $USE_CURSES="pdcurses" -a "x$CURSES_LIBS" = x],
    [CURSES_LIBS="-l$USE_CURSES"])
fi

if test "x$CURSES_LIBS" = x; then
  if test "$USE_CURSES" = "xcurses" -o "$USE_CURSES" = check; then
    # consider using xcurses-config
    # note: the library name is actually with an upper-case XC:
    AC_CHECK_LIB([XCurses], [initscr], [true],
      [AS_IF([test "$USE_CURSES" != check], [USE_CURSES="missing_lib"])])
  fi
fi
if test "$USE_CURSES" = "xcurses" -o "x$ac_cv_lib_XCurses_initscr" = xyes; then
  AC_CHECK_HEADERS([xcurses/curses.h xcurses.h],
    [USE_CURSES="xcurses"
     break],
    [AS_IF([test "$USE_CURSES" != check],
       [USE_CURSES="missing_header"]) ])
  AS_IF([test $USE_CURSES="xcurses" -a "x$CURSES_LIBS" = x],
    # note: the library name is actually with an upper-case XC:
    [CURSES_LIBS="-lXCurses"])
fi

if test "x$CURSES_LIBS" = x; then
  if test "$USE_CURSES" = "curses" -o "$USE_CURSES" = check; then
    AC_CHECK_LIB([curses], [initscr], [true],
      [AS_IF([test "$USE_CURSES" != check], [USE_CURSES="missing_lib"])])
  fi
fi
if test "$USE_CURSES" = "curses" -o "x$ac_cv_lib_curses_initscr" = xyes; then
  AC_CHECK_HEADERS([curses.h], [USE_CURSES="curses"],
    [AS_IF([test "$USE_CURSES" != check], [USE_CURSES="missing_header"]) ])
  AS_IF([test $USE_CURSES="curses" -a "x$CURSES_LIBS" = x],
    [CURSES_LIBS="-l$USE_CURSES"])
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
	AC_MSG_FAILURE([[not able to link configured library $with_curses]])
	;;
  missing_header)
	AC_MSG_ERROR([[no header found for configured library $with_curses]])
	;;
  *curses*)
	AC_DEFINE_UNQUOTED([WITH_CURSES], ["$USE_CURSES"])
	LIBCOB_LIBS="$LIBCOB_LIBS $CURSES_LIBS"
	;;
esac

LIBS="$LIBS $CURSES_LIBS"  # adding found curses libraries to look for functions
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

if test "$USE_CURSES" != no -a "$USE_CURSES" != "not_found"; then
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
LIBS="$curr_libs"  # clean for next checks

AC_ARG_WITH([seqra-extfh],
  [AS_HELP_STRING([--with-seqra-extfh],
    [(GnuCOBOL) Use external SEQ/RAN handler (obsolete)])],
  [ if test "$with_seqra_extfh" = yes; then
	AC_DEFINE([WITH_SEQRA_EXTFH], [1])
    fi ],
  [])

AC_ARG_WITH([cisam],
  [AS_HELP_STRING([--with-cisam],
    [(GnuCOBOL) Use CISAM for INDEXED I/O])],
  [ if test "$with_cisam" = yes; then
	AC_CHECK_HEADERS([isam.h], [],
		AC_MSG_ERROR([isam.h is required for CISAM]))
	AC_CHECK_LIB([ifisam], [isopen],
		[AC_DEFINE([WITH_CISAM], [1])
		LIBCOB_LIBS="$LIBCOB_LIBS -lifisam -lifisamx"],
		AC_MSG_ERROR([libifisam is required for CISAM]), [-lifisamx])
    fi ],
  [])

AS_IF([test "$with_cisam" != yes], [
  AC_ARG_WITH([disam],
  [AS_HELP_STRING([--with-disam],
    [(GnuCOBOL) Use DISAM for INDEXED I/O])],
  [ if test "$with_disam" = yes; then
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
	if test "$cob_got_disam" = yes; then
		AC_MSG_NOTICE([DISAM library found as -l$cobdisam])
	else
		AC_MSG_ERROR([DISAM library not found])
	fi
	unset cobdisam
	unset cob_got_disam
    fi ],
  [])
])

AS_IF([test "$with_cisam" != yes -a "$with_disam" != yes], [
  AC_ARG_WITH([vbisam],
  [AS_HELP_STRING([--with-vbisam],
    [(GnuCOBOL) Use VBISAM for ISAM I/O])],
  [ if test "$with_vbisam" = yes; then
	AC_CHECK_HEADERS([vbisam.h], [],
		AC_MSG_ERROR([vbisam.h is required for VBISAM]))
	# note: isfullclose is available since 2.0, isopen since 1.0
	AC_CHECK_LIB([vbisam], [isfullclose],
		[AC_DEFINE([WITH_VBISAM], [1])
		LIBCOB_LIBS="$LIBCOB_LIBS -lvbisam"],
		AC_MSG_ERROR([libvbisam >= 2.0 is required for VBISAM]), [])
    fi ],
  [])
])

AS_IF([test "$with_cisam" != yes -a "$with_disam" != yes -a "$with_vbisam" != yes], [
  AC_ARG_WITH([index-extfh],
  [AS_HELP_STRING([--with-index-extfh],
    [(GnuCOBOL) Use external ISAM file handler (obsolete)])],
  [ if test "$with_index_extfh" = yes; then
	AC_DEFINE([WITH_INDEX_EXTFH], [1])
    fi ],
  [])
])

AS_IF([test "$with_cisam" != yes -a "$with_disam" != yes -a "$with_vbisam" != yes -a "$with_index_extfh" != yes], [
  AC_ARG_WITH([db],
  [AS_HELP_STRING([--with-db],
    [(GnuCOBOL) Use Berkeley DB >= 4.1 for ISAM I/O (default)])],
  [],
  [with_db=yes])
])

# Checks for db.
AC_ARG_VAR([BDB_CFLAGS], [compiler flags necessary to preprocess/compile for Berkeley DB])
AC_ARG_VAR([BDB_LIBS], [linker flags for Berkeley DB, overriding automatic lookup])
if test "$with_db" = yes; then
	AC_MSG_NOTICE([Checks for Berkeley DB ...])

	curr_cppflags="$CPPFLAGS"
	if test -n "$BDB_CFLAGS"; then
	   CPPFLAGS="$CPPFLAGS $BDB_CFLAGS"
	fi
	AC_CHECK_HEADERS([db.h], [], AC_MSG_ERROR([[Berkeley DB db.h is missing, you may adjust BDB_CFLAGS]]), [])

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
		[AC_MSG_ERROR([unable to extract Berkeley DB version information from db.h])],
		[AC_MSG_WARN([cannot run test program while cross-compiling])
		 COB_BDB_HEADER="cross"
		 COB_BDB_HEADER_STR="cross"])
	if test "x$COB_BDB_HEADER" = x; then
		AC_MSG_ERROR([Unable to extract Berkeley DB version information])
	fi
	if test "x$COB_BDB_HEADER_STR" != x; then
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
		if test "x$BDB_LIBS" != x; then
			AC_MSG_CHECKING([for Berkeley DB library with BDB_LIBS])
			LIBS="$test_libs $BDB_LIBS"
			AC_RUN_IFELSE([AC_LANG_PROGRAM([[#include <stdio.h>
			#include <db.h>]], [[
			int major = 0, minor = 0, patch = 0;
			db_version (&major, &minor, &patch);
			if (major == DB_VERSION_MAJOR && minor == DB_VERSION_MINOR) {
				return 0;
			}
			return 1;]])],
			[AC_DEFINE([WITH_DB], [1])
			cob_got_db=yes
			LIBCOB_LIBS="$LIBCOB_LIBS $BDB_LIBS"],
			[],
			[])
		else
			AC_MSG_CHECKING([for Berkeley DB library with version $COB_BDB_HEADER])
			# prefer library with version number included as some systems link against wrong version
			# of the library and to not break when a newer BDB version is installed, see bug #100
			COB_BDB_HEADER_CLEAN=$(echo "$COB_BDB_HEADER" | sed -e 's/\.//' -e 's/\-//')
			for cobdb in db-$COB_BDB_HEADER db-$COB_BDB_HEADER_CLEAN db$COB_BDB_HEADER_CLEAN \
				db-$COB_BDB_HEADER_MAJOR db$COB_BDB_HEADER_MAJOR db \
				libdb$COB_BDB_HEADER libdb$COB_BDB_HEADER_CLEAN  # for common Win32 versions...
			do
				LIBS="$test_libs -l$cobdb"
				AC_RUN_IFELSE([AC_LANG_PROGRAM([[#include <stdio.h>
				#include <db.h>]], [[
				int major = 0, minor = 0, patch = 0;
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
		fi
		LIBS="$test_libs"
		unset test_libs
		if test "$cob_got_db" = yes; then
			AC_MSG_RESULT([yes])
			if test "x$BDB_LIBS" != x; then
				AC_MSG_NOTICE([BDB library version $COB_BDB_HEADER linked with $BDB_LIBS])
			else
				AC_MSG_NOTICE([BDB library version $COB_BDB_HEADER found as -l$cobdb])
			fi
		else
			AC_MSG_RESULT([no])
			AC_MSG_ERROR([BDB library version $COB_BDB_HEADER is required for --with-db, you may adjust BDB_LIBS])
		fi
	else
		AC_MSG_WARN([Matching BDB version (>=4.1) assumed])
		AC_CHECK_LIB([db], [db_version],
		[LIBCOB_LIBS="$LIBCOB_LIBS -ldb"],
		AC_MSG_ERROR([BDB library is required as -ldb]), [])
	fi
	if test -n "$BDB_CFLAGS"; then
	   LIBCOB_CPPFLAGS="$LIBCOB_CPPFLAGS $BDB_CFLAGS"
	fi
	CPPFLAGS="$curr_cppflags"
	unset cobdb
	unset cob_got_db
	unset COB_BDB_HEADER
	unset COB_BDB_HEADER_MAJOR
	unset COB_BDB_HEADER_MINOR
fi

# Checks for dl/ltdl.
DEFINE_DL=no

AC_MSG_CHECKING([for _WIN32])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
	#ifndef _WIN32
	# error macro not defined
	#endif]])],
	[DEFINE_DL=yes
	 AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])])

if test "$DEFINE_DL" = no -a "$with_dl" = yes; then
  if test "x$ac_cv_header_dlfcn_h" = xyes; then
    AC_CHECK_LIB([c], [dlopen], [DEFINE_DL=yes], [], [])
    if test "$DEFINE_DL" = no; then
      AC_CHECK_LIB([dl], [dlopen], [DEFINE_DL=yes], [], [])
      if test "$DEFINE_DL" = yes; then
        AC_DEFINE([USE_LIBDL], [1])
        LIBCOB_LIBS="$LIBCOB_LIBS -ldl"
        dnl AC_CHECK_LIB([dl], [dladdr], [AC_DEFINE([HAVE_DLADDR], [1])], [], [])
      else
        AC_CHECK_LIB([dld], [dlopen], [DEFINE_DL=yes], [], [])
        if test "$DEFINE_DL" = yes; then
          AC_DEFINE([USE_LIBDL], [1])
          LIBCOB_LIBS="$LIBCOB_LIBS -ldld"
          dnl AC_CHECK_LIB([dld], [dladdr], [AC_DEFINE([HAVE_DLADDR], [1])], [], [])
        fi
      fi
    else
      AC_DEFINE([USE_LIBDL], [1])
      dnl AC_CHECK_LIB([c], [dladdr], [AC_DEFINE([HAVE_DLADDR], [1])], [], [])
    fi
  fi
fi

if test "$DEFINE_DL" = no; then
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


AC_MSG_NOTICE([Checks for different sizes ...])

AC_CHECK_SIZEOF(long int)
AC_CHECK_SIZEOF(long long)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(void *)

AC_MSG_CHECKING([whether size of long int = size of long long])
AS_IF([test "$ac_cv_sizeof_long_int" = "$ac_cv_sizeof_long_long"],
	[AC_DEFINE([COB_LI_IS_LL], [1]) AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])])

AC_MSG_CHECKING([whether long is 32 bits])
AS_IF([test "$ac_cv_sizeof_long" = 4],
	[AC_DEFINE([COB_32_BIT_LONG], [1]) AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])])

AC_MSG_CHECKING([whether pointer is longer than 32 bits])
AS_IF([test "$ac_cv_sizeof_void_p" -gt 4],
	[COB_HAS_64_BIT_POINTER=yes
	 AC_DEFINE([COB_64_BIT_POINTER], [1])
	 AC_MSG_RESULT([yes])],
	[AC_MSG_RESULT([no])])

dnl We likely don't need this and remove it before 4.0 final release
dnl # Check gcc wrapv option
dnl if test "$COB_USES_GCC_NO_ICC" = yes; then
dnl 	curr_cflags="$CFLAGS"
dnl 	CFLAGS="$CFLAGS -fwrapv"
dnl 	AC_MSG_CHECKING([for gcc -fwrapv option])
dnl 	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[int testint;]])],
dnl 		[AC_DEFINE([HAVE_FWRAPV_OPT], [1]) AC_MSG_RESULT([yes])],
dnl 		[AC_MSG_RESULT([no])])
dnl 		[],
dnl 		[CFLAGS="$curr_cflags"])
dnl 	CFLAGS="$curr_cflags"
dnl fi

# Check if aligned attribute seems to work
# done:   does not raise an error
# *TODO*: has same output as omitting it

AC_CACHE_CHECK([for aligned attribute], gc_cv_attribute_alligned, [
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]],
	[[char testchar[4] __attribute__((aligned));]])],
	[gc_cv_attribute_alligned=yes],
	[gc_cv_attribute_alligned=no]
  ])
)
  if test "$gc_cv_attribute_alligned" = yes; then
	AC_DEFINE([HAVE_ATTRIBUTE_ALIGNED], [1])
  fi

AC_CACHE_CHECK([for pure attribute], gc_cv_attribute_pure, [
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
	  __attribute__((pure)) int f (void) { return 0; }
	]],
	[[]])],
	[gc_cv_attribute_pure=yes],
	[gc_cv_attribute_pure=no]
  ])
)
  if test "$gc_cv_attribute_pure" = yes; then
	AC_DEFINE([HAVE_ATTRIBUTE_PURE], [1])
  fi

dnl  Simon: removed, use --disable-nls instead
dnl # Disable for Cygwin
dnl AC_MSG_CHECKING([for __CYGWIN__])
dnl AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
dnl 	#ifndef __CYGWIN__
dnl 	# error macro not defined
dnl 	#endif]])],
dnl 	[enable_nls=no
dnl 	 AC_MSG_RESULT([yes])],
dnl 	[AC_MSG_RESULT([no])])

AM_GNU_GETTEXT([external])
AM_GNU_GETTEXT_VERSION([0.19.8])
if test "x$LTLIBINTL" != x; then
  if test "x$PROGRAMS_LIBS" != x; then
    PROGRAMS_LIBS="$PROGRAMS_LIBS $LTLIBINTL"
  else
    PROGRAMS_LIBS="$LTLIBINTL"
  fi
  LIBCOB_LIBS="$LIBCOB_LIBS $LTLIBINTL"
fi

# Checks for internationalization stuff
dnl currently not directly used: AM_ICONV
AM_LANGINFO_CODESET

# Check for perl, used for "make test"
#AM_MISSING_PROG(PERL, perl) --> only use for tools that generate something
AC_CHECK_PROG(PERL, perl, perl)

# GnuCOBOL Configuration

COB_CONFIG_DIR="$datadir/$PACKAGE_TARNAME/config"
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

if test "$enable_cflags_setting" = yes; then
	if test "$enable_hardening" != yes; then
		# Remove -O2 option added by AC_PROG_CC and add -O0
		if test "$enable_debug" = yes -o "$enable_code_coverage" = yes; then
			CFLAGS=$(echo "$CFLAGS" | sed -e 's/ *-O@<:@0-9a-zA-Z@:>@* */ /g' -e 's/ $//' -e 's/^ //')
			if test "$COB_USES_GCC" = yes; then
				CFLAGS="$CFLAGS -O0"
			fi
		fi
	fi
fi

unset enable_cflags_setting

# Note: cobc.c assumed -g for all but _MSC_VER
if test "$ac_cv_prog_cc_g" = yes; then
	COB_DEBUG_FLAGS="-g"
else
	COB_DEBUG_FLAGS=""
	AC_MSG_WARN([No clue how to add debug info with this compiler!])
fi

# For debugging: add most expressive debug level if using GCC (compatible)
curr_cflags="$CFLAGS"
AS_IF([test "$COB_USES_GCC" = yes], [
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
])

if test "$enable_debug" = yes; then
	if test "x$curr_cflags" != "x"; then
		CFLAGS="$curr_cflags $COB_DEBUG_FLAGS"
	else
		CFLAGS="$COB_DEBUG_FLAGS"
	fi
else
	CFLAGS="$curr_cflags"
fi

AS_IF([test "$enable_hardening" = yes], [
	AC_MSG_NOTICE([Compile with hardening options])
	# needs optimization
	#if test "$enable_debug" = yes; then
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
	if test "x$cob_temp_flags" != x; then
	   if test "x$COB_LDFLAGS" != x; then
	      COB_LDFLAGS="$COB_LDFLAGS $cob_temp_flags"
	   else
	      COB_LDFLAGS="$cob_temp_flags"
	   fi
	fi
	unset cob_temp_flags
])

AS_IF([test "$COB_USES_GCC" = yes], [
	curr_cflags="$CFLAGS"
	AC_CACHE_CHECK([for -pipe option], gc_cv_gcc_pipe, [
	CFLAGS="$CFLAGS -pipe"
	AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[int testint;]])],
		[gc_cv_gcc_pipe=yes],
		[gc_cv_gcc_pipe=no])
	])
	  if test "$gc_cv_gcc_pipe" = yes; then
		CFLAGS="$curr_cflags -pipe"
	  else
		CFLAGS="$curr_cflags"
	  fi
])

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

if test "$COB_USES_ICC_ONLY" = yes; then
	# these are deprecated...
	COB_CFLAGS="$COB_CFLAGS -vec-report0 -opt-report 0"
elif test "$COB_USES_WATCOMC_ONLY" = yes; then
	# -s = no overflow checks, otherwise need to code/link a CHK routine
	COB_CFLAGS="$COB_CFLAGS -s -wcd=118"
elif test "$COB_USES_XLC_ONLY" = yes; then
	# use read-only memory for string literals and constants
	COB_CFLAGS="$COB_CFLAGS -qro -qroconst"
	# allow nonstandard usage - CHECKME where do we need this ???
	# do we need the additional check for __IBMC__ >= 700 we had in cobc?
	COB_CFLAGS="$COB_CFLAGS -qlanglvl=extended"
	# Suppress compiler warning about MAXMEM optimization
	COB_CFLAGS="$COB_CFLAGS -qsuppress=1500-030"
fi

if test "$COB_USES_GCC_NO_ICC" = yes; then
	# comment from cobc.c:	--param max-goto-duplication-insns=100000
	# /* RXWRXW - gcse */
	# COB_CFLAGS="$COB_CFLAGS -Wno-unused -fsigned-char -fno-gcse"
	COB_CFLAGS="$COB_CFLAGS -Wno-unused -fsigned-char"
fi

# Check gcc 4 pointer sign option (at least available with "recent" clang, too)
#if test "$COB_USES_GCC_NO_ICC" = yes; then
if test "$COB_USES_XLC_ONLY" != yes; then
	curr_cflags="$CFLAGS"
	CFLAGS="$CFLAGS -Wno-pointer-sign"
	AC_MSG_CHECKING([for gcc pointer sign option])
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[int testint;]])],
		[COB_CFLAGS="$COB_CFLAGS -Wno-pointer-sign"; AC_MSG_RESULT([yes])],
		[AC_MSG_RESULT([no])])
	CFLAGS="$curr_cflags"
fi
#fi

if test "$COB_USES_CLANG_ONLY" = yes; then
	# don't warn if cobc uses arguments which aren't picked up (likely because of the translation phase)
	COB_CFLAGS="$COB_CFLAGS -Qunused-arguments"
fi


# Include CFLAGS / CPPFLAGS in COB_CFLAGS without optimization/debug options.

if test "x$CFLAGS" != x; then
	cob_temp_flags="$CFLAGS"
else
	cob_temp_flags=""
fi
if test "x$CPPFLAGS" != x; then
	if test "x$cob_temp_flags" != x; then
		cob_temp_flags="$CPPFLAGS $cob_temp_flags"
	else
		cob_temp_flags="$CPPFLAGS"
	fi
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
	     -e 's/-g\(gdb\)\{0,1\}[[0-9 ]]//g'				\
	     -e 's/-g\(gdb\)\{0,1\}$//'				\
	     -e 's/-Ofast//g'			\
	     -e 's/[[+-]]O[[0-9sz]]* //g'		\
	     -e 's/[[+-]]O[[0-9sz]]*$//g'		\
	     -e 's/-funwind-tables//g'			\
	     -e 's/-fasynchronous-unwind-tables//g'	\
	     -e 's/  / /g'	\
	     -e 's/ $//g'	\
	     )

if test "x$cob_temp_flags" != x; then
	COB_CFLAGS="$cob_temp_flags $COB_CFLAGS"
fi
if test "$enable_hardening" = yes; then
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
	fi
	if test "$COB_USES_GCC" = yes; then
		COB_EXPORT_DYN="-Wl,--export-all-symbols -Wl,--enable-auto-import -Wl,--enable-auto-image-base"
		COB_FIX_LIBTOOL="-Wl,--enable-auto-import"
	fi
	#COB_FIX_LIB=""
	;;
  cygwin*)
	if test "$COB_USES_GCC" = yes; then
		COB_EXPORT_DYN="-Wl,--export-all-symbols -Wl,--enable-auto-import -Wl,--enable-auto-image-base"
		COB_FIX_LIBTOOL="-Wl,--enable-auto-import"
	fi
	#COB_FIX_LIB=""
	;;
  darwin* | rhapsody*)
	if test "$COB_USES_GCC" = yes; then
		COB_SHARED_OPT="-bundle -flat_namespace -undefined suppress"
	fi
	;;
  hpux*)
	if test "$COB_USES_GCC" != yes; then
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

		if test "$enable_debug" = yes; then
			CFLAGS="$CFLAGS +O0"
		else
			CFLAGS="$CFLAGS +O2"
		fi

		rm -f hptest*
	fi
	;;
  aix*)
	COB_EXPORT_DYN="-Wl,-bexpfull -Wl,-brtl"
	if test "$COB_USES_GCC" != yes; then
		CFLAGS="$CFLAGS -Q -qro -qroconst"
		if test "$enable_debug" = yes; then
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
	if test "$COB_USES_GCC" != yes; then
		CFLAGS="$CFLAGS -xstrconst"
		if test "$enable_debug" != yes; then
			CFLAGS="$CFLAGS -xO2"
		fi
		COB_CFLAGS="$COB_CFLAGS -xstrconst"
		COB_SHARED_OPT="-G"
	fi
	;;
esac

if test "$COB_USES_ICC_ONLY" = yes; then
	if test "$enable_debug" != yes; then
		CFLAGS="$CFLAGS -finline-functions"
	fi
	CFLAGS="$CFLAGS -Wall -wd1419 -vec-report0 -opt-report 0"
elif test "$COB_USES_GCC" = yes && test "$with_gnu_ld" = yes; then
	curr_ldflags="$LDFLAGS"
	LDFLAGS="$LDFLAGS -Wl,-z,relro,-z,now,-O1"
	AC_MSG_CHECKING([for ld bind now option])
	AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[int testint;]])],
		[AC_MSG_RESULT([yes])],
		[LDFLAGS=$curr_ldflags
		 AC_MSG_RESULT([no])])
	unset curr_ldflags
	if test "$enable_debug" != yes; then
		CFLAGS="$CFLAGS -finline-functions"
	fi
	CFLAGS="$CFLAGS -fsigned-char -Wall -Wwrite-strings -Wmissing-prototypes -Wno-format-y2k"
fi

if test "x$lt_cv_dlopen_self" != xyes; then
	AC_DEFINE([COB_NO_SELFOPEN], [1])
fi

if test "$with_cisam" = yes; then
	COB_HAS_ISAM=cisam
elif test "$with_disam" = yes; then
	COB_HAS_ISAM=disam
elif test "$with_vbisam" = yes; then
	COB_HAS_ISAM=vbisam
elif test "$with_db" = yes; then
	COB_HAS_ISAM=db
elif test "$with_index_extfh" = yes; then
	COB_HAS_ISAM=index_extfh
else
	COB_HAS_ISAM=no
fi

if test "$USE_CURSES" = "not_found" -o "$USE_CURSES" = no; then
	COB_HAS_CURSES=no
else
	COB_HAS_CURSES=yes
fi

if test "$with_xml2" = yes; then
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
dnl FIXME: Should be tested as the system may can actually run these
dnl        for example x86/x64 cross, GNU/Linux with Wine, Bash on Windows, ...

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
if test "x$striplib" != x; then
	AC_DEFINE_UNQUOTED([COB_STRIP_CMD], ["$striplib"])
fi
AX_AC_DEFINE_RESOLVED([COB_CONFIG_DIR],	[$COB_CONFIG_DIR],	[default search path for copybooks])
AX_AC_DEFINE_RESOLVED([COB_COPY_DIR],	[$COB_COPY_DIR],	[default search path for configuration files])

dnl unused: AX_AC_DEFINE_RESOLVED([DATADIR],		[$datarootdir], 	[path for configuration files])
dnl unused: AX_AC_DEFINE_RESOLVED([LOCALEDIR],		[$localedir], 		[path for locale-dependent data])

dnl unused: AC_DEFINE_UNQUOTED([COBC_NAME],	["$(echo cobc    | $SED "$program_transform_name")"], [executable name for compiler])
AC_DEFINE_UNQUOTED([COBCRUN_NAME],	["$(echo cobcrun | $SED "$program_transform_name")"], [executable name for module runner])

dnl informational only
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
AC_SUBST([COB_COPY_DIR])
AC_SUBST([COB_LIBRARY_PATH])
AC_SUBST([COB_OBJECT_EXT])
AC_SUBST([COB_MODULE_EXT])
AC_SUBST([COB_EXE_EXT])
AC_SUBST([QUOTE_INCLUDE_FLAG])
AC_SUBST([COBC_CPPFLAGS])
AC_SUBST([PROGRAMS_LIBS])
AC_SUBST([LIBCOB_LIBS])
AC_SUBST([LIBCOB_CPPFLAGS])

dnl was used in bin/Makefile.am - seems not to be needed
dnl AC_SUBST([COB_EXPORT_DYN])

dnl used in the testsuite
COB_BIGENDIAN="$ac_cv_c_bigendian"
AC_SUBST([COB_BIGENDIAN])

dnl unused: AC_SUBST([COB_FIX_LIB])
AC_SUBST([COB_FIX_LIBTOOL])

AC_SUBST([COB_HAS_ISAM])
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
AC_MSG_NOTICE([ LIBCOB_LIBS       ${LIBCOB_LIBS}])
if test "x$PROGRAMS_LIBS" != x; then
  AC_MSG_NOTICE([ PROGRAMS_LIBS         ${PROGRAMS_LIBS}])
fi
AC_MSG_NOTICE([ COB_CC            ${COB_CC}])
AC_MSG_NOTICE([ COB_CFLAGS        ${COB_CFLAGS}])
AC_MSG_NOTICE([ COB_LDFLAGS       ${COB_LDFLAGS}])
AC_MSG_NOTICE([ COB_DEBUG_FLAGS   ${COB_DEBUG_FLAGS}])
AC_MSG_NOTICE([ COB_LIBS          ${COB_LIBS}])
AC_MSG_NOTICE([ COB_CONFIG_DIR    ${COB_CONFIG_DIR}])
AC_MSG_NOTICE([ COB_COPY_DIR      ${COB_COPY_DIR}])
AC_MSG_NOTICE([ COB_LIBRARY_PATH  ${COB_LIBRARY_PATH}])
AC_MSG_NOTICE([ COB_OBJECT_EXT    ${OBJEXT}])
AC_MSG_NOTICE([ COB_MODULE_EXT    ${COB_MODULE_EXT}])
AC_MSG_NOTICE([ COB_EXE_EXT       ${COB_EXE_EXT}])
AC_MSG_NOTICE([ COB_SHARED_OPT    ${COB_SHARED_OPT}])
AC_MSG_NOTICE([ COB_PIC_FLAGS     ${COB_PIC_FLAGS}])
AC_MSG_NOTICE([ COB_EXPORT_DYN    ${COB_EXPORT_DYN}])
if test "x$striplib" != x; then
  AC_MSG_NOTICE([ COB_STRIP_CMD     ${striplib}])
fi
if test "${DEFINE_DL}" = yes ; then
  AC_MSG_NOTICE([ Dynamic loading:                             System])
else
  AC_MSG_NOTICE([ Dynamic loading:                             Libtool])
fi
AC_MSG_NOTICE([ Use gettext for international messages:      ${USE_NLS}])
AC_MSG_NOTICE([ Use fcntl for file locking:                  ${ac_cv_func_fcntl}])
AC_MSG_NOTICE([ Use math multiple precision library:         ${with_math}])
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
if test "$with_debug_log" = yes; then
  AC_MSG_NOTICE([ Enable GnuCOBOL developer logging            yes])
fi
if test "$with_seqra_extfh" = yes; then
  AC_MSG_NOTICE([ Use external SEQ/RAN file handler:           yes])
  AC_MSG_WARN([ Obsolete feature "OpenCOBOL-only-EXTFH" used!])
fi
if test "$with_index_extfh" = yes; then
  AC_MSG_NOTICE([ Use external INDEXED file handler:           yes])
  AC_MSG_WARN([ Obsolete feature "OpenCOBOL-only-EXTFH" used!])
elif test "$with_cisam" = yes; then
  AC_MSG_NOTICE([ Use CISAM for INDEXED I/O:                   yes])
elif test "$with_disam" = yes; then
  AC_MSG_NOTICE([ Use DISAM for INDEXED I/O:                   yes])
elif test "$with_vbisam" = yes; then
  AC_MSG_NOTICE([ Use VBISAM for INDEXED I/O:                  yes])
elif test "$with_db" = yes; then
  AC_MSG_NOTICE([ Use Berkeley DB for INDEXED I/O:             yes])
else
  AC_MSG_NOTICE([ INDEXED I/O (no handler configured):         NO])
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

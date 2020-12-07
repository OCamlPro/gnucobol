/* Define if building universal (internal helper macro) */
#cmakedefine AC_APPLE_UNIVERSAL_BUILD

/* executable name for module runner */
#define COBCRUN_NAME "@COBCRUN_NAME@"

/* long int is 32 bits */
#cmakedefine COB_32_BIT_LONG

/* Pointers are longer than 32 bits */
#cmakedefine COB_64_BIT_POINTER

/* informational: build environment during compilation */
#cmakedefine COB_BLD_BUILD "@COB_BLD_BUILD@"

/* informational: compiler during compilation */
#define COB_BLD_CC "@COB_BLD_CC@"

/* informational: compiler flags during compilation */
#define COB_BLD_CFLAGS "@COB_BLD_CFLAGS@"

/* informational: preparser flags during compilation */
#define COB_BLD_CPPFLAGS "@COB_BLD_CPPFLAGS@"

/* informational: linker during compilation */
#define COB_BLD_LD "@COB_BLD_LD@"

/* informational: linker flags during compilation */
#define COB_BLD_LDFLAGS "@COB_BLD_LDFLAGS@"

/* compiler used by cobc */
#define COB_CC "@COB_BLD_CC@"

/* compiler flags passed to compiler by cobc */
#define COB_CFLAGS "@COB_BLD_CFLAGS@"

/* Compilation of computed gotos works */
#cmakedefine COB_COMPUTED_GOTO

/* default search path for copybooks */
#cmakedefine COB_CONFIG_DIR "@COB_CONFIG_DIR@"

/* default search path for configuration files */
#cmakedefine COB_COPY_DIR "@COB_COPY_DIR@"

/* Compile/link option for debugging */
#cmakedefine COB_DEBUG_FLAGS "@COB_DEBUG_FLAGS@"

/* Enable internal logging (Developers only!) */
#cmakedefine COB_DEBUG_LOG

/* Executable extension */
#define COB_EXE_EXT "@CMAKE_EXECUTABLE_SUFFIX@"

/* Enable experimental code (Developers only!) */
#cmakedefine COB_EXPERIMENTAL

/* Compile/link option for exporting symbols */
#cmakedefine COB_EXPORT_DYN "@COB_EXPORT_DYN@"

/* Keyword for inline */
#cmakedefine COB_KEYWORD_INLINE @COB_KEYWORD_INLINE@

/* linker flags passed to linker by cobc */
#define COB_LDFLAGS "@COB_LDFLAGS@"

/* default search path for extra modules */
#cmakedefine COB_LIBRARY_PATH "@COB_LIBRARY_PATH@"

/* libraries passed to linker by cobc */
#cmakedefine COB_LIBS "@COB_LIBS@"

/* long int is long long */
#cmakedefine COB_LI_IS_LL

/* Module extension */
#cmakedefine COB_MODULE_EXT "@CMAKE_SHARED_LIBRARY_SUFFIX@"

/* Can not dlopen self */
#cmakedefine COB_NO_SELFOPEN

/* Object extension */
#define COB_OBJECT_EXT "@CMAKE_OBJECT_SUFFIX@"

/* Enable minimum parameter check for system libraries */
#cmakedefine COB_PARAM_CHECK

/* Compile/link option for PIC code */
#define COB_PIC_FLAGS ""

/* default search path for indexed schema definitions */
#cmakedefine COB_SCHEMA_DIR  "@COB_SCHEMA_DIR@"

/* Compile/link option for shared code */
#define COB_SHARED_OPT ""

/* Strip command */
#cmakedefine COB_STRIP_CMD

/* Enable extra checks within the compiler (Developers only!) */
#cmakedefine COB_TREE_DEBUG

/* Define to 1 if translation of program messages to the user's native
   language is requested. */
#cmakedefine ENABLE_NLS

/* Define to 1 if you have the `atol' function. */
#cmakedefine HAVE_ATOL

/* Define to 1 if you have the `atoll' function. */
#cmakedefine HAVE_ATOLL

/* Has __attribute__((aligned)) */
#cmakedefine HAVE_ATTRIBUTE_ALIGNED

/* Define to 1 if you have the `canonicalize_file_name' function. */
#cmakedefine HAVE_CANONICALIZE_FILE_NAME

/* Define to 1 if you have the Mac OS X function
   CFLocaleCopyPreferredLanguages in the CoreFoundation framework. */
#cmakedefine HAVE_CFLOCALECOPYPREFERREDLANGUAGES

/* Define to 1 if you have the Mac OS X function CFPreferencesCopyAppValue in
   the CoreFoundation framework. */
#cmakedefine HAVE_CFPREFERENCESCOPYAPPVALUE

/* Define to 1 if you have the <cjson/cJSON.h> header file. */
#cmakedefine HAVE_CJSON_CJSON_H

/* Define to 1 if you have the <cJSON.h> header file. */
#cmakedefine HAVE_CJSON_H

/* Has clock_gettime function and CLOCK_REALTIME */
#cmakedefine HAVE_CLOCK_GETTIME

/* curses has color_set function */
#cmakedefine HAVE_COLOR_SET

/* ncurses has _nc_freeall function */
#cmakedefine HAVE_CURSES_FREEALL

/* Define to 1 if you have the <curses.h> header file. */
#cmakedefine HAVE_CURSES_H

/* Define to 1 if you have the <db.h> header file. */
#cmakedefine HAVE_DB_H

/* Define if the GNU dcgettext() function is already present or preinstalled.
   */
#cmakedefine HAVE_DCGETTEXT

/* Define to 1 if you have the declaration of `fdatasync', and to 0 if you
   don't. */
#cmakedefine HAVE_DECL_FDATASYNC

/* Define to 1 if you have the declaration of `fmemopen', and to 0 if you
   don't. */
#cmakedefine HAVE_DECL_FMEMOPEN

/* curses has define_key function */
#cmakedefine HAVE_DEFINE_KEY

/* Has designated initializers */
#cmakedefine HAVE_DESIGNATED_INITS

/* Define to 1 if you have the <disam.h> header file. */
#cmakedefine HAVE_DISAM_H

/* Has dladdr function */
#cmakedefine HAVE_DLADDR

/* Define to 1 if you have the <dlfcn.h> header file. */
#cmakedefine HAVE_DLFCN_H

/* Define to 1 if you don't have `vprintf' but do have `_doprnt.' */
#cmakedefine HAVE_DOPRNT

/* Define to 1 if you have the `fcntl' function. */
#cmakedefine HAVE_FCNTL

/* Define to 1 if you have the <fcntl.h> header file. */
#cmakedefine HAVE_FCNTL_H

/* Define to 1 if you have the `fdatasync' function. */
#cmakedefine HAVE_FDATASYNC

/* Declaration of finite function in ieeefp.h instead of math.h */
#cmakedefine HAVE_FINITE_IEEEFP_H

/* Define to 1 if you have the `flockfile' function. */
#cmakedefine HAVE_FLOCKFILE

/* Define to 1 if you have the `fmemopen' function. */
#cmakedefine HAVE_FMEMOPEN

/* Define to 1 if you have the `getexecname' function. */
#cmakedefine HAVE_GETEXECNAME

/* Define if the GNU gettext() function is already present or preinstalled. */
#cmakedefine HAVE_GETTEXT

/* Define to 1 if you have the `gettimeofday' function. */
#cmakedefine HAVE_GETTIMEOFDAY

/* Define to 1 if you have the <gmp.h> header file. */
#cmakedefine HAVE_GMP_H

/* curses has has_mouse function */
#cmakedefine HAVE_HAS_MOUSE

/* Define if you have the iconv() function and it works. */
#cmakedefine HAVE_ICONV

/* Define to 1 if you have the <inttypes.h> header file. */
#cmakedefine HAVE_INTTYPES_H

/* Define to 1 if you have the <isam.h> header file. */
#cmakedefine HAVE_ISAM_H

/* Has isfinite function */
#cmakedefine HAVE_ISFINITE @HAVE_ISFINITE@

/* Define if you have <langinfo.h> and nl_langinfo(CODESET). */
#cmakedefine HAVE_LANGINFO_CODESET

/* Define to 1 if you have the `curses' library (-lcurses). */
#cmakedefine HAVE_LIBCURSES

/* Define to 1 if you have the `ncurses' library (-lncurses). */
#cmakedefine HAVE_LIBNCURSES

/* Define to 1 if you have the `ncursesw' library (-lncursesw). */
#cmakedefine HAVE_LIBNCURSESW

/* Define to 1 if you have the `pdcurses' library (-lpdcurses). */
#cmakedefine HAVE_LIBPDCURSES

/* Define to 1 if you have the `posix4' library (-lposix4). */
#cmakedefine HAVE_LIBPOSIX4

/* Define to 1 if you have the `rt' library (-lrt). */
#cmakedefine HAVE_LIBRT

/* Define to 1 if you have the `localeconv' function. */
#cmakedefine HAVE_LOCALECONV

/* Define to 1 if you have the <locale.h> header file. */
#cmakedefine HAVE_LOCALE_H

/* Define to 1 if you have the <ltdl.h> header file. */
#cmakedefine HAVE_LTDL_H

/* Define to 1 if you have the <malloc.h> header file. */
#cmakedefine HAVE_MALLOC_H

/* Define to 1 if you have the `memmove' function. */
#cmakedefine HAVE_MEMMOVE

/* Define to 1 if you have the <memory.h> header file. */
#cmakedefine HAVE_MEMORY_H

/* Define to 1 if you have the `memset' function. */
#cmakedefine HAVE_MEMSET

/* curses has mouseinterval function */
#cmakedefine HAVE_MOUSEINTERVAL

/* Define to 1 if you have the <mpir.h> header file. */
#cmakedefine HAVE_MPIR_H

/* Do we have mp_get_memory_functions in GMP/MPIR */
#cmakedefine HAVE_MP_GET_MEMORY_FUNCTIONS

/* Has nanosleep function */
#cmakedefine HAVE_NANO_SLEEP

/* Define to 1 if you have the <ncursesw/curses.h> header file. */
#cmakedefine HAVE_NCURSESW_CURSES_H

/* Define to 1 if you have the <ncursesw/ncurses.h> header file. */
#cmakedefine HAVE_NCURSESW_NCURSES_H

/* Define to 1 if you have the <ncurses.h> header file. */
#cmakedefine HAVE_NCURSES_H

/* Define to 1 if you have the <ncurses/ncurses.h> header file. */
#cmakedefine HAVE_NCURSES_NCURSES_H

/* Define to 1 if you have the <oci.h> header file. */
#cmakedefine HAVE_OCI_H

/* Define to 1 if you have the <pdcurses.h> header file. */
#cmakedefine HAVE_PDCURSES_H

/* Define to 1 if you have the `popen' function. */
#cmakedefine HAVE_POPEN

/* Define to 1 if you have the `raise' function. */
#cmakedefine HAVE_RAISE

/* Define to 1 if you have the `readlink' function. */
#cmakedefine HAVE_READLINK

/* Define to 1 if you have the `realpath' function. */
#cmakedefine HAVE_REALPATH

/* Define to 1 if you have the `setenv' function. */
#cmakedefine HAVE_SETENV

/* Define to 1 if you have the `setlocale' function. */
#cmakedefine HAVE_SETLOCALE

/* Define to 1 if you have the `sigaction' function. */
#cmakedefine HAVE_SIGACTION

/* Define to 1 if you have the <signal.h> header file. */
#cmakedefine HAVE_SIGNAL_H

/* Define to 1 if the system has the type `sig_atomic_t'. */
#cmakedefine HAVE_SIG_ATOMIC_T

/* Define to 1 if you have the <sqlext.h> header file. */
#cmakedefine HAVE_SQLEXT_H

/* Define to 1 if you have the <sql.h> header file. */
#cmakedefine HAVE_SQL_H

/* Define to 1 if you have the <stddef.h> header file. */
#cmakedefine HAVE_STDDEF_H

/* Define to 1 if you have the <stdint.h> header file. */
#cmakedefine HAVE_STDINT_H

/* Define to 1 if you have the <stdlib.h> header file. */
#cmakedefine HAVE_STDLIB_H

/* Define to 1 if you have the `strcasecmp' function. */
#cmakedefine HAVE_STRCASECMP

/* Define to 1 if you have the `strchr' function. */
#cmakedefine HAVE_STRCHR

/* Define to 1 if you have the `strcoll' function. */
#cmakedefine HAVE_STRCOLL

/* Define to 1 if you have the `strdup' function. */
#cmakedefine HAVE_STRDUP

/* Define to 1 if you have the `strerror' function. */
#cmakedefine HAVE_STRERROR

/* Define to 1 if you have the <strings.h> header file. */
#cmakedefine HAVE_STRINGS_H

/* Define to 1 if you have the <string.h> header file. */
#cmakedefine HAVE_STRING_H

/* Define to 1 if you have the `strrchr' function. */
#cmakedefine HAVE_STRRCHR

/* Define to 1 if you have the `strstr' function. */
#cmakedefine HAVE_STRSTR

/* Define to 1 if you have the `strtol' function. */
#cmakedefine HAVE_STRTOL

/* Define to 1 if you have the `strtoll' function. */
#cmakedefine HAVE_STRTOLL

/* Define to 1 if you have the <sys/stat.h> header file. */
#cmakedefine HAVE_SYS_STAT_H

/* Define to 1 if you have the <sys/sysmacros.h> header file. */
#cmakedefine HAVE_SYS_SYSMACROS_H

/* Define to 1 if you have the <sys/time.h> header file. */
#cmakedefine HAVE_SYS_TIME_H

/* Define to 1 if you have the <sys/types.h> header file. */
#cmakedefine HAVE_SYS_TYPES_H

/* Define to 1 if you have the <sys/wait.h> header file. */
#cmakedefine HAVE_SYS_WAIT_H

/* Has timezone variable */
#cmakedefine HAVE_TIMEZONE

/* Define to 1 if you have the <unistd.h> header file. */
#cmakedefine HAVE_UNISTD_H

/* ncurses has use_legacy_coding function */
#cmakedefine HAVE_USE_LEGACY_CODING

/* Define to 1 if you have the <vbisam.h> header file. */
#cmakedefine HAVE_VBISAM_H

/* Define to 1 if you have the `vprintf' function. */
#cmakedefine HAVE_VPRINTF

/* Define to 1 if you have the <wchar.h> header file. */
#cmakedefine HAVE_WCHAR_H

/* Define to the sub-directory where libtool stores uninstalled libraries. */
#cmakedefine LT_OBJDIR

/* Define maximum parameters for CALL */
#cmakedefine MAX_CALL_FIELD_PARAMS @MAX_CALL_FIELD_PARAMS@

/* Name of package */
#cmakedefine PACKAGE "@PACKAGE@"

/* Define to the address where bug reports for this package should be sent. */
#cmakedefine PACKAGE_BUGREPORT 

/* Define to the full name of this package. */
#cmakedefine PACKAGE_NAME "@PROJECT_NAME@"

/* Define to the full name and version of this package. */
#cmakedefine PACKAGE_STRING "@PROJECT_DESCRIPTION@"

/* Define to the one symbol short name of this package. */
#cmakedefine PACKAGE_TARNAME "gnucobol"

/* Define to the home page for this package. */
#cmakedefine PACKAGE_URL "@PROJECT_HOMEPAGE_URL@"

/* Define to the version of this package. */
#cmakedefine PACKAGE_VERSION "@PROJECT_VERSION@"

/* Define a patch level (numeric, max. 8 digits) */
#define PATCH_LEVEL @PATCH_LEVEL@

/* Define to 1 if you have the ANSI C header files. */
#cmakedefine STDC_HEADERS

/* Define to 1 if your <sys/time.h> declares `struct tm'. */
#cmakedefine TM_IN_SYS_TIME

/* Use system dynamic loader */
#cmakedefine USE_LIBDL @USE_LIBDL@

/* Enable extensions on AIX 3, Interix.  */
#ifndef _ALL_SOURCE
# undef _ALL_SOURCE
#endif
/* Enable GNU extensions on systems that have them.  */
#ifndef _GNU_SOURCE
# undef _GNU_SOURCE
#endif
/* Enable threading extensions on Solaris.  */
#ifndef _POSIX_PTHREAD_SEMANTICS
# undef _POSIX_PTHREAD_SEMANTICS
#endif
/* Enable extensions on HP NonStop.  */
#ifndef _TANDEM_SOURCE
# undef _TANDEM_SOURCE
#endif
/* Enable general extensions on Solaris.  */
#ifndef __EXTENSIONS__
# undef __EXTENSIONS__
#endif


/* Version number of package */
#cmakedefine VERSION

/* Use CISAM as INDEXED handler */
#cmakedefine WITH_CISAM

/* Use cJSON library/source as JSON handler */
#cmakedefine WITH_CJSON

/* curses library for extended SCREEN I/O */
#define WITH_CURSES "@WITH_CURSES@"

/* Use Berkeley DB library as INDEXED handler */
#cmakedefine WITH_DB

/* Use DISAM as INDEXED handler */
#cmakedefine WITH_DISAM

/* Default INDEXED file handler */
#cmakedefine WITH_INDEXED

/* Compile with obsolete external INDEXED handler */
#cmakedefine WITH_INDEX_EXTFH

/* Default INDEXED file handler */
#cmakedefine WITH_IXDFLT

/* JSON handler */
#cmakedefine WITH_JSON "@WITH_JSON@"

/* Use JSON-C library as JSON handler */
#cmakedefine WITH_JSON_C

/* Use Lightning Memory-Mapped Database as INDEXED handler */
#cmakedefine WITH_LMDB

/* Math multiple precision library */
#cmakedefine WITH_MATH

/* Using more than 1 of C/D/VB-ISAM */
#cmakedefine WITH_MULTI_ISAM

/* Use OCI for INDEXED file handler */
#cmakedefine WITH_OCI

/* Use ODBC for INDEXED file handler */
#cmakedefine WITH_ODBC

/* Compile with obsolete external SEQ/RAN handler */
#cmakedefine WITH_SEQRA_EXTFH

/* Define variable sequential file format */
#define WITH_VARSEQ @VARSEQ@

/* Use VBISAM as INDEXED handler */
#cmakedefine WITH_VBISAM

/* Use libxml2 as XML handler */
#cmakedefine WITH_XML2

/* Define WORDS_BIGENDIAN to 1 if your processor stores words with the most
   significant byte first (like Motorola and SPARC, unlike Intel). */
#if defined AC_APPLE_UNIVERSAL_BUILD
# if defined __BIG_ENDIAN__
#  define WORDS_BIGENDIAN 1
# endif
#else
# ifndef WORDS_BIGENDIAN
#  undef WORDS_BIGENDIAN
# endif
#endif

/* Define to 1 if `lex' declares `yytext' as a `char *' by default, not a
   `char[]'. */
#cmakedefine YYTEXT_POINTER

/* Define to 1 if on MINIX. */
#cmakedefine _MINIX

/* Define to 2 if the system does not provide POSIX.1 features except with
   this defined. */
#cmakedefine _POSIX_1_SOURCE

/* Define to 1 if you need to in order for `stat' and other things to work. */
#cmakedefine _POSIX_SOURCE

/* Define to 1 if on HPUX.  */
#ifndef _XOPEN_SOURCE_EXTENDED
# undef _XOPEN_SOURCE_EXTENDED
#endif

/* Define to empty if `const' does not conform to ANSI C. */
#cmakedefine const

/* Define to `unsigned int' if <sys/types.h> does not define. */
#cmakedefine size_t

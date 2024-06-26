/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.ac by autoheader.  */

/* Define if building universal (internal helper macro) */
/* #undef AC_APPLE_UNIVERSAL_BUILD */

/* executable name for module runner */
#define COBCRUN_NAME "cobcrun"

/* long int is 32 bits */
/* #undef COB_32_BIT_LONG */

/* Pointers are longer than 32 bits */
#define COB_64_BIT_POINTER 1

/* informational: build environment during compilation */
#define COB_BLD_BUILD "x86_64-pc-linux-gnu"

/* informational: compiler during compilation */
#define COB_BLD_CC "gcc"

/* informational: compiler flags during compilation */
#define COB_BLD_CFLAGS "-O2 -pipe -finline-functions -fsigned-char -Wall -Wwrite-strings -Wmissing-prototypes -Wno-format-y2k"

/* informational: preparser flags during compilation */
#define COB_BLD_CPPFLAGS ""

/* informational: linker during compilation */
#define COB_BLD_LD "/usr/bin/ld -m elf_x86_64"

/* informational: linker flags during compilation */
#define COB_BLD_LDFLAGS " -Wl,-z,relro,-z,now,-O1"

/* compiler used by cobc */
#define COB_CC "gcc"

/* compiler flags passed to compiler by cobc */
#define COB_CFLAGS "-pipe -I/home/a7medmaher/Github/gnucobol/.local/include -Wno-unused -fsigned-char -Wno-pointer-sign"

/* Compilation of computed gotos works */
#define COB_COMPUTED_GOTO 1

/* default search path for copybooks */
#define COB_CONFIG_DIR "/home/a7medmaher/Github/gnucobol/.local/share/gnucobol/config"

/* default search path for configuration files */
#define COB_COPY_DIR "/home/a7medmaher/Github/gnucobol/.local/share/gnucobol/copy"

/* Compile/link option for debugging */
#define COB_DEBUG_FLAGS "-ggdb3 -fasynchronous-unwind-tables"

/* Enable internal logging (Developers only!) */
/* #undef COB_DEBUG_LOG */

/* Executable extension */
#define COB_EXE_EXT ""

/* Enable experimental code (Developers only!) */
/* #undef COB_EXPERIMENTAL */

/* Compile/link option for exporting symbols */
#define COB_EXPORT_DYN "-Wl,--export-dynamic"

/* Keyword for inline */
#define COB_KEYWORD_INLINE __inline

/* linker flags passed to linker by cobc */
#define COB_LDFLAGS ""

/* default search path for extra modules */
#define COB_LIBRARY_PATH "/home/a7medmaher/Github/gnucobol/.local/lib/gnucobol"

/* libraries passed to linker by cobc */
#define COB_LIBS "-L/home/a7medmaher/Github/gnucobol/.local/lib -lcob"

/* long int is long long */
#define COB_LI_IS_LL 1

/* Module extension */
#define COB_MODULE_EXT "so"

/* Can not dlopen self */
/* #undef COB_NO_SELFOPEN */

/* Object extension */
#define COB_OBJECT_EXT "o"

/* Enable minimum parameter check for system libraries */
/* #undef COB_PARAM_CHECK */

/* Compile/link option for PIC code */
#define COB_PIC_FLAGS "-fPIC -DPIC"

/* Compile/link option for shared code */
#define COB_SHARED_OPT "-shared"

/* Strip command */
#define COB_STRIP_CMD "strip --strip-unneeded"

/* Enable extra checks within the compiler (Developers only!) */
/* #undef COB_TREE_DEBUG */

/* Define to 1 if translation of program messages to the user's native
   language is requested. */
#define ENABLE_NLS 1

/* Has __attribute__((aligned)) */
#define HAVE_ATTRIBUTE_ALIGNED 1

/* Has __attribute__((constructor)) */
#define HAVE_ATTRIBUTE_CONSTRUCTOR 1

/* Has __attribute__((pure)) */
#define HAVE_ATTRIBUTE_PURE 1

/* Define to 1 if you have the `canonicalize_file_name' function. */
#define HAVE_CANONICALIZE_FILE_NAME 1

/* Define to 1 if you have the Mac OS X function
   CFLocaleCopyPreferredLanguages in the CoreFoundation framework. */
/* #undef HAVE_CFLOCALECOPYPREFERREDLANGUAGES */

/* Define to 1 if you have the Mac OS X function CFPreferencesCopyAppValue in
   the CoreFoundation framework. */
/* #undef HAVE_CFPREFERENCESCOPYAPPVALUE */

/* Define to 1 if you have the <cjson/cJSON.h> header file. */
/* #undef HAVE_CJSON_CJSON_H */

/* Define to 1 if you have the <cJSON.h> header file. */
/* #undef HAVE_CJSON_H */

/* Has clock_gettime function and CLOCK_REALTIME */
#define HAVE_CLOCK_GETTIME 1

/* curses has color_set function */
#define HAVE_COLOR_SET 1

/* curses provides function to free all memory */
#define HAVE_CURSES_FREEALL 1

/* Define to 1 if you have the <curses.h> header file. */
/* #undef HAVE_CURSES_H */

/* Define to 1 if you have the <db.h> header file. */
/* #undef HAVE_DB_H */

/* Define if the GNU dcgettext() function is already present or preinstalled.
   */
#define HAVE_DCGETTEXT 1

/* Define to 1 if you have the declaration of `fdatasync', and to 0 if you
   don't. */
#define HAVE_DECL_FDATASYNC 1

/* Define to 1 if you have the declaration of `fmemopen', and to 0 if you
   don't. */
#define HAVE_DECL_FMEMOPEN 1

/* curses has define_key function */
#define HAVE_DEFINE_KEY 1

/* Has designated initializers */
#define HAVE_DESIGNATED_INITS 1

/* Define to 1 if you have the <disam.h> header file. */
/* #undef HAVE_DISAM_H */

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Define to 1 if you don't have `vprintf' but do have `_doprnt.' */
/* #undef HAVE_DOPRNT */

/* Define to 1 if you have the `fcntl' function. */
#define HAVE_FCNTL 1

/* Define to 1 if you have the <fcntl.h> header file. */
#define HAVE_FCNTL_H 1

/* Define to 1 if you have the `fdatasync' function. */
#define HAVE_FDATASYNC 1

/* Declaration of finite function in ieeefp.h instead of math.h */
/* #undef HAVE_FINITE_IEEEFP_H */

/* Define to 1 if you have the `flockfile' function. */
#define HAVE_FLOCKFILE 1

/* Define to 1 if you have the `fmemopen' function. */
#define HAVE_FMEMOPEN 1

/* Define to 1 if you have the `getexecname' function. */
/* #undef HAVE_GETEXECNAME */

/* Define if the GNU gettext() function is already present or preinstalled. */
#define HAVE_GETTEXT 1

/* Define to 1 if you have the `gettimeofday' function. */
#define HAVE_GETTIMEOFDAY 1

/* Define to 1 if you have the <gmp.h> header file. */
#define HAVE_GMP_H 1

/* curses has has_mouse function */
#define HAVE_HAS_MOUSE 1

/* Define if you have the iconv() function and it works. */
/* #undef HAVE_ICONV */

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the <isam.h> header file. */
/* #undef HAVE_ISAM_H */

/* Has isfinite function */
#define HAVE_ISFINITE 1

/* Define to 1 if you have the <json-c/json.h> header file. */
/* #undef HAVE_JSON_C_JSON_H */

/* Define to 1 if you have the <json.h> header file. */
/* #undef HAVE_JSON_H */

/* Define if you have <langinfo.h> and nl_langinfo(CODESET). */
#define HAVE_LANGINFO_CODESET 1

/* Define to 1 if you have the `posix4' library (-lposix4). */
/* #undef HAVE_LIBPOSIX4 */

/* Define to 1 if you have the `rt' library (-lrt). */
/* #undef HAVE_LIBRT */

/* Define to 1 if you have the `localeconv' function. */
#define HAVE_LOCALECONV 1

/* Define to 1 if you have the <locale.h> header file. */
#define HAVE_LOCALE_H 1

/* Define to 1 if you have the <ltdl.h> header file. */
/* #undef HAVE_LTDL_H */

/* Define to 1 if you have the `memmove' function. */
#define HAVE_MEMMOVE 1

/* Define to 1 if you have the `memset' function. */
#define HAVE_MEMSET 1

/* Define to 1 if you have the <minix/config.h> header file. */
/* #undef HAVE_MINIX_CONFIG_H */

/* curses has mouseinterval function */
#define HAVE_MOUSEINTERVAL 1

/* curses has mousemask function and mmask_t definition */
#define HAVE_MOUSEMASK 1

/* Define to 1 if you have the <mpir.h> header file. */
/* #undef HAVE_MPIR_H */

/* Do we have mp_get_memory_functions in GMP/MPIR */
#define HAVE_MP_GET_MEMORY_FUNCTIONS 1

/* Has nanosleep function */
#define HAVE_NANO_SLEEP 1

/* Define to 1 if you have the <ncursesw/curses.h> header file. */
/* #undef HAVE_NCURSESW_CURSES_H */

/* Define to 1 if you have the <ncursesw/ncurses.h> header file. */
#define HAVE_NCURSESW_NCURSES_H 1

/* Define to 1 if you have the <ncurses/curses.h> header file. */
/* #undef HAVE_NCURSES_CURSES_H */

/* Define to 1 if you have the <ncurses.h> header file. */
/* #undef HAVE_NCURSES_H */

/* Define to 1 if you have the <ncurses/ncurses.h> header file. */
/* #undef HAVE_NCURSES_NCURSES_H */

/* Define to 1 if you have the <pdcurses.h> header file. */
/* #undef HAVE_PDCURSES_H */

/* Define to 1 if you have the `popen' function. */
#define HAVE_POPEN 1

/* Define to 1 if you have the `raise' function. */
#define HAVE_RAISE 1

/* Define to 1 if you have the `readlink' function. */
#define HAVE_READLINK 1

/* Define to 1 if you have the `realpath' function. */
#define HAVE_REALPATH 1

/* curses has resize_term function */
#define HAVE_RESIZE_TERM 1

/* Define to 1 if you have the `setenv' function. */
#define HAVE_SETENV 1

/* Define to 1 if you have the `setlocale' function. */
#define HAVE_SETLOCALE 1

/* Define to 1 if you have the `sigaction' function. */
#define HAVE_SIGACTION 1

/* Define to 1 if you have the <signal.h> header file. */
#define HAVE_SIGNAL_H 1

/* Define to 1 if the system has the type `sig_atomic_t'. */
#define HAVE_SIG_ATOMIC_T 1

/* Define to 1 if you have the <stddef.h> header file. */
#define HAVE_STDDEF_H 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdio.h> header file. */
#define HAVE_STDIO_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the `strcasecmp' function. */
#define HAVE_STRCASECMP 1

/* Define to 1 if you have the `strchr' function. */
#define HAVE_STRCHR 1

/* Define to 1 if you have the `strcoll' function. */
#define HAVE_STRCOLL 1

/* Define to 1 if you have the `strdup' function. */
#define HAVE_STRDUP 1

/* Define to 1 if you have the `strerror' function. */
#define HAVE_STRERROR 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the `strrchr' function. */
#define HAVE_STRRCHR 1

/* Define to 1 if you have the `strstr' function. */
#define HAVE_STRSTR 1

/* Define to 1 if you have the `strtol' function. */
#define HAVE_STRTOL 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
#define HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Has timezone variable */
#define HAVE_TIMEZONE 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* ncurses has use_legacy_coding function */
#define HAVE_USE_LEGACY_CODING 1

/* Define to 1 if you have the <vbisam.h> header file. */
/* #undef HAVE_VBISAM_H */

/* Define to 1 if you have the `vprintf' function. */
#define HAVE_VPRINTF 1

/* Define to 1 if you have the <wchar.h> header file. */
#define HAVE_WCHAR_H 1

/* Define to 1 if you have the <xcurses/curses.h> header file. */
/* #undef HAVE_XCURSES_CURSES_H */

/* Define to 1 if you have the <xcurses.h> header file. */
/* #undef HAVE_XCURSES_H */

/* Define to the sub-directory where libtool stores uninstalled libraries. */
#define LT_OBJDIR ".libs/"

/* Define maximum parameters for CALL */
#define MAX_CALL_FIELD_PARAMS 192

/* Name of package */
#define PACKAGE "gnucobol"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "bug-gnucobol@gnu.org"

/* Define to the full name of this package. */
#define PACKAGE_NAME "GnuCOBOL"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "GnuCOBOL 3.3-dev"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "gnucobol"

/* Define to the home page for this package. */
#define PACKAGE_URL "https://www.gnu.org/software/gnucobol/"

/* Define to the version of this package. */
#define PACKAGE_VERSION "3.3-dev"

/* Define a patch level (numeric, max. 8 digits) */
#define PATCH_LEVEL 0

/* The size of `long', as computed by sizeof. */
#define SIZEOF_LONG 8

/* The size of `long int', as computed by sizeof. */
#define SIZEOF_LONG_INT 8

/* The size of `long long', as computed by sizeof. */
#define SIZEOF_LONG_LONG 8

/* The size of `void *', as computed by sizeof. */
#define SIZEOF_VOID_P 8

/* Define to 1 if all of the C90 standard headers exist (not just the ones
   required in a freestanding environment). This macro is provided for
   backward compatibility; new code need not use it. */
#define STDC_HEADERS 1

/* Define to 1 if your <sys/time.h> declares `struct tm'. */
/* #undef TM_IN_SYS_TIME */

/* Use system dynamic loader */
#define USE_LIBDL 1

/* Enable extensions on AIX 3, Interix.  */
#ifndef _ALL_SOURCE
# define _ALL_SOURCE 1
#endif
/* Enable general extensions on macOS.  */
#ifndef _DARWIN_C_SOURCE
# define _DARWIN_C_SOURCE 1
#endif
/* Enable general extensions on Solaris.  */
#ifndef __EXTENSIONS__
# define __EXTENSIONS__ 1
#endif
/* Enable GNU extensions on systems that have them.  */
#ifndef _GNU_SOURCE
# define _GNU_SOURCE 1
#endif
/* Enable X/Open compliant socket functions that do not require linking
   with -lxnet on HP-UX 11.11.  */
#ifndef _HPUX_ALT_XOPEN_SOCKET_API
# define _HPUX_ALT_XOPEN_SOCKET_API 1
#endif
/* Identify the host operating system as Minix.
   This macro does not affect the system headers' behavior.
   A future release of Autoconf may stop defining this macro.  */
#ifndef _MINIX
/* # undef _MINIX */
#endif
/* Enable general extensions on NetBSD.
   Enable NetBSD compatibility extensions on Minix.  */
#ifndef _NETBSD_SOURCE
# define _NETBSD_SOURCE 1
#endif
/* Enable OpenBSD compatibility extensions on NetBSD.
   Oddly enough, this does nothing on OpenBSD.  */
#ifndef _OPENBSD_SOURCE
# define _OPENBSD_SOURCE 1
#endif
/* Define to 1 if needed for POSIX-compatible behavior.  */
#ifndef _POSIX_SOURCE
/* # undef _POSIX_SOURCE */
#endif
/* Define to 2 if needed for POSIX-compatible behavior.  */
#ifndef _POSIX_1_SOURCE
/* # undef _POSIX_1_SOURCE */
#endif
/* Enable POSIX-compatible threading on Solaris.  */
#ifndef _POSIX_PTHREAD_SEMANTICS
# define _POSIX_PTHREAD_SEMANTICS 1
#endif
/* Enable extensions specified by ISO/IEC TS 18661-5:2014.  */
#ifndef __STDC_WANT_IEC_60559_ATTRIBS_EXT__
# define __STDC_WANT_IEC_60559_ATTRIBS_EXT__ 1
#endif
/* Enable extensions specified by ISO/IEC TS 18661-1:2014.  */
#ifndef __STDC_WANT_IEC_60559_BFP_EXT__
# define __STDC_WANT_IEC_60559_BFP_EXT__ 1
#endif
/* Enable extensions specified by ISO/IEC TS 18661-2:2015.  */
#ifndef __STDC_WANT_IEC_60559_DFP_EXT__
# define __STDC_WANT_IEC_60559_DFP_EXT__ 1
#endif
/* Enable extensions specified by ISO/IEC TS 18661-4:2015.  */
#ifndef __STDC_WANT_IEC_60559_FUNCS_EXT__
# define __STDC_WANT_IEC_60559_FUNCS_EXT__ 1
#endif
/* Enable extensions specified by ISO/IEC TS 18661-3:2015.  */
#ifndef __STDC_WANT_IEC_60559_TYPES_EXT__
# define __STDC_WANT_IEC_60559_TYPES_EXT__ 1
#endif
/* Enable extensions specified by ISO/IEC TR 24731-2:2010.  */
#ifndef __STDC_WANT_LIB_EXT2__
# define __STDC_WANT_LIB_EXT2__ 1
#endif
/* Enable extensions specified by ISO/IEC 24747:2009.  */
#ifndef __STDC_WANT_MATH_SPEC_FUNCS__
# define __STDC_WANT_MATH_SPEC_FUNCS__ 1
#endif
/* Enable extensions on HP NonStop.  */
#ifndef _TANDEM_SOURCE
# define _TANDEM_SOURCE 1
#endif
/* Enable X/Open extensions.  Define to 500 only if necessary
   to make mbstate_t available.  */
#ifndef _XOPEN_SOURCE
/* # undef _XOPEN_SOURCE */
#endif


/* Use CISAM as INDEXED handler */
/* #undef WITH_CISAM */

/* Use cJSON library/source as JSON handler */
/* #undef WITH_CJSON */

/* curses library for extended SCREEN I/O */
#define WITH_CURSES "ncursesw"

/* Use Berkeley DB library as INDEXED handler */
/* #undef WITH_DB */

/* Use DISAM as INDEXED handler */
/* #undef WITH_DISAM */

/* Compile with obsolete external INDEXED handler */
/* #undef WITH_INDEX_EXTFH */

/* JSON handler */
#define WITH_JSON "disabled"

/* Use JSON-C library as JSON handler */
/* #undef WITH_JSON_C */

/* Math multiple precision library */
/* #undef WITH_MATH */

/* Compile with obsolete external SEQ/RAN handler */
/* #undef WITH_SEQRA_EXTFH */

/* Define variable sequential file format */
#define WITH_VARSEQ 0

/* Use VBISAM as INDEXED handler */
/* #undef WITH_VBISAM */

/* Use libxml2 as XML handler */
#define WITH_XML2 1

/* Define WORDS_BIGENDIAN to 1 if your processor stores words with the most
   significant byte first (like Motorola and SPARC, unlike Intel). */
#if defined AC_APPLE_UNIVERSAL_BUILD
# if defined __BIG_ENDIAN__
#  define WORDS_BIGENDIAN 1
# endif
#else
# ifndef WORDS_BIGENDIAN
/* #  undef WORDS_BIGENDIAN */
# endif
#endif

/* Define to 1 if `lex' declares `yytext' as a `char *' by default, not a
   `char[]'. */
#define YYTEXT_POINTER 1

/* Define to 1 if on HPUX.  */
#ifndef _XOPEN_SOURCE_EXTENDED
/* # undef _XOPEN_SOURCE_EXTENDED */
#endif

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef const */

/* Define to `unsigned int' if <sys/types.h> does not define. */
/* #undef size_t */

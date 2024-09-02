/*
   Copyright (C) 2001-2012, 2014-2024 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Simon Sobisch, Ron Norman

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

#include "tarstamp.h"
#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <string.h>
#ifdef	HAVE_STRINGS_H
#include <strings.h>
#endif
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#include <math.h>
#ifdef HAVE_FINITE_IEEEFP_H
#include <ieeefp.h>
#endif

#include <time.h>

#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#else
#ifndef STDIN_FILENO
#define STDIN_FILENO 0
#endif
#ifndef STDOUT_FILENO
#define STDOUT_FILENO 1
#endif
#ifndef STDERR_FILENO
#define STDERR_FILENO 2
#endif
#endif
#ifdef	HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef	HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#ifdef	_WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef MOUSE_MOVED
#include <process.h>
#include <direct.h>
#include <io.h>
#include <fcntl.h>	/* for _O_BINARY only */
#if !defined(__BORLANDC__) && !defined(__WATCOMC__) && !defined(__ORANGEC__)
#define	getcwd		_getcwd
#endif
#endif


#ifdef	HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifndef SIGFPE
#ifndef NSIG
#define NSIG 240
#endif
#define SIGFPE NSIG + 1
#endif

#ifdef	HAVE_LOCALE_H
#include <locale.h>
#endif

/* library headers for version output */
#ifdef _WIN32
#ifndef __GMP_LIBGMP_DLL
#define __GMP_LIBGMP_DLL 1
#endif
#endif
#ifdef	HAVE_GMP_H
#include <gmp.h>
#elif defined HAVE_MPIR_H
#include <mpir.h>
#else
#error either HAVE_GMP_H or HAVE_MPIR_H needs to be defined
#endif

#if defined (HAVE_NCURSESW_NCURSES_H)
#include <ncursesw/ncurses.h>
#define COB_GEN_SCREENIO
#elif defined (HAVE_NCURSESW_CURSES_H)
#include <ncursesw/curses.h>
#define COB_GEN_SCREENIO
#elif defined (HAVE_NCURSES_H)
#include <ncurses.h>
#define COB_GEN_SCREENIO
#elif defined (HAVE_NCURSES_NCURSES_H)
#include <ncurses/ncurses.h>
#define COB_GEN_SCREENIO
#elif defined (HAVE_PDCURSES_H)
/* will internally define NCURSES_MOUSE_VERSION with
   a recent version (for older version define manually): */
#define PDC_NCMOUSE		/* use ncurses compatible mouse API */
#include <pdcurses.h>
#define COB_GEN_SCREENIO
#elif defined (HAVE_CURSES_H)
#define PDC_NCMOUSE	/* see comment above */
#include <curses.h>
#define COB_GEN_SCREENIO
#ifndef PDC_MOUSE_MOVED
#undef PDC_NCMOUSE
#endif
#endif

#if defined (WITH_XML2)
#include <libxml/parser.h>
#include <libxml/xmlversion.h>
#include <libxml/xmlwriter.h>
#endif

#if defined (WITH_CJSON)
#if defined (HAVE_CJSON_CJSON_H)
#include <cjson/cJSON.h>
#elif defined (HAVE_CJSON_H)
#include <cJSON.h>
#else
#error CJSON without necessary header
#endif
#elif defined (WITH_JSON_C)
#include <json_c_version.h>
#endif

/* end of library headers */

/* Force symbol exports */
#define	COB_LIB_EXPIMP
#include "common.h"
#include "cobcapi.h"	/* for helper functions */

#include "coblocal.h"

#include "cobgetopt.h"
#include "sysdefines.h"

/* sanity checks */
#if COB_MAX_WORDLEN > 255
#error COB_MAX_WORDLEN is too big, must be less than 256
#endif
#if COB_MAX_NAMELEN > COB_MAX_WORDLEN
#error COB_MAX_NAMELEN is too big, must be less than COB_MAX_WORDLEN
#endif

#define	CB_IMSG_SIZE	24
#define	CB_IVAL_SIZE	(80 - CB_IMSG_SIZE - 4)

#if COB_MAX_UNBOUNDED_SIZE > COB_MAX_FIELD_SIZE
#define COB_MAX_ALLOC_SIZE COB_MAX_UNBOUNDED_SIZE
#else
#define COB_MAX_ALLOC_SIZE COB_MAX_FIELD_SIZE
#endif

struct cob_alloc_cache {
	struct cob_alloc_cache	*next;		/* Pointer to next */
	void			*cob_pointer;	/* Pointer to malloced space */
	size_t			size;		/* Item size */
};
const int	MAX_MODULE_ITERS = 10240;

struct cob_alloc_module {
	struct cob_alloc_module	*next;		/* Pointer to next */
	void			*cob_pointer;	/* Pointer to malloced space */
};

/* EXTERNAL structure */

struct cob_external {
	struct cob_external	*next;		/* Pointer to next */
	void			*ext_alloc;	/* Pointer to malloced space */
	char			*ename;		/* External name */
	int			esize;		/* Item size */
};

#define COB_ERRBUF_SIZE		1024

/* Local variables */

static int			cob_initialized = 0;
static int			check_mainhandle = 1;
static int			cob_argc = 0;
static char			**cob_argv = NULL;
static struct cob_alloc_cache	*cob_alloc_base = NULL;
static struct cob_alloc_module	*cob_module_list = NULL;
static cob_module		*cob_module_err = NULL;
static const char		*cob_last_sfile = NULL;
static const char		*cob_last_progid = NULL;

static cob_global		*cobglobptr = NULL;
static cob_settings		*cobsetptr = NULL;

static int			last_exception_code;	/* Last exception: code */
static int			active_error_handler = 0;
static int			in_stop_run = 0;

static const cob_field_attr	const_alpha_attr =
				{COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
static const cob_field_attr	const_bin_nano_attr =
				{COB_TYPE_NUMERIC_BINARY, 20, 9,
				 COB_FLAG_HAVE_SIGN, NULL};

static char			*cob_local_env = NULL;
static int			current_arg = 0;
static unsigned char		*commlnptr = NULL;
static size_t			commlncnt = 0;
static size_t			cob_local_env_size = 0;

static struct cob_external	*basext = NULL;

static size_t			sort_nkeys = 0;
static cob_file_key		*sort_keys = NULL;
static const unsigned char	*sort_collate = NULL;

static const char		*cob_source_file = NULL;
static unsigned int		cob_source_line = 0;

#ifdef	HAVE_DESIGNATED_INITS
const char	*cob_statement_name[STMT_MAX_ENTRY] = {
	[STMT_UNKNOWN] = "UNKNOWN"
	/* note: STMT_UNKNOWN left out here */
#define COB_STATEMENT(ename,str)	, [ename] = str
#include "statement.def"	/* located and installed next to common.h */
#undef COB_STATEMENT
};
#else
const char	*cob_statement_name[STMT_MAX_ENTRY];
static void init_statement_list (void);
#endif

#ifdef COB_DEBUG_LOG
static int			cob_debug_check_open = 1;
static int			cob_debug_log_time = 0;
static FILE			*cob_debug_file = NULL;
static int			cob_debug_level = 9;
static char			*cob_debug_mod = NULL;
#define DEBUG_MOD_LEN 6
#define DEBUG_MOD_MAX 12
static char			cob_debug_modules[DEBUG_MOD_MAX][DEBUG_MOD_LEN+1] = 
					{" ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " "};
static char			*cob_debug_file_name = NULL;
#endif

static char			*strbuff = NULL;

static int		cob_process_id = 0;
static int		cob_temp_iteration = 0;

static unsigned int	conf_runtime_error_displayed = 0;
static unsigned int	last_runtime_error_line = 0;
static const char	*last_runtime_error_file = NULL;

/* List of dynamically allocated field attributes */
static struct dyn_attr {
	struct dyn_attr	*next;
	cob_field_attr	attr;
} *dyn_attr_list = NULL;

static char			runtime_err_str[COB_ERRBUF_SIZE] = {0};	/* emergency buffer */
static int			exit_code = 0;	/* exit code when leaving libcob */

#ifndef COB_WITHOUT_JMP
/* longjmp buffer used instead of exit() if cob_init_with_return is used */
static jmp_buf	return_jmp_buf;
static int		return_jmp_buffer_set = 0;
#endif

#if	defined (HAVE_SIGNAL_H) && defined (HAVE_SIG_ATOMIC_T)
volatile sig_atomic_t	sig_is_handled = 0;
#endif

/* Function Pointer for external signal handling */
static void		(*cob_ext_sighdl) (int) = NULL;

#if defined (_MSC_VER)
static VOID		(WINAPI *time_as_filetime_func) (LPFILETIME) = NULL;
#endif

#undef	COB_EXCEPTION
#define COB_EXCEPTION(code, tag, name, critical)	name,
static const char		* const cob_exception_tab_name[] = {
	"None",		/* COB_EC_ZERO */
#include "exception.def"
	"Invalid"	/* COB_EC_MAX */
};

#undef	COB_EXCEPTION
#define COB_EXCEPTION(code, tag, name, critical)	0x##code,
static const int		cob_exception_tab_code[] = {
	0,		/* COB_EC_ZERO */
#include "exception.def"
	0		/* COB_EC_MAX */
};

#undef	COB_EXCEPTION

#define EXCEPTION_TAB_SIZE	sizeof (cob_exception_tab_code) / sizeof (int)

/* Switches */
#define	COB_SWITCH_MAX	36  /* (must match cobc/tree.h)*/

static int		cob_switch[COB_SWITCH_MAX + 1];

/* Runtime exit handling */
static struct exit_handlerlist {
	struct exit_handlerlist	*next;
	int			(*proc)(void);
	unsigned char priority;
} *exit_hdlrs;

/* Runtime error handling */
static struct handlerlist {
	struct handlerlist	*next;
	int			(*proc)(char *s);
} *hdlrs;

/* note: set again (translated) in print_runtime_conf */
static const char *setting_group[] = {" hidden setting ", "CALL configuration",
					"File I/O configuration", "Screen I/O configuration", "Miscellaneous",
					"System configuration"};

static struct config_enum lwrupr[] = {{"LOWER", "1"}, {"UPPER", "2"}, {"not set", "0"}, {NULL, NULL}};
static struct config_enum notset[] = {{"not set", "!"}, {NULL, NULL}};
static struct config_enum never[] = {{"never", "!"}, {NULL, NULL}};
static struct config_enum beepopts[] = {{"FLASH", "1"}, {"SPEAKER", "2"}, {"FALSE", "9"}, {"BEEP", "0"}, {NULL, NULL}};
static struct config_enum timeopts[] = {{"0", "1000"}, {"1", "100"}, {"2", "10"}, {"3", "1"}, {NULL, NULL}};
static struct config_enum coeopts[] = {{"0", "0"}, {"1", "1"}, {"2", "2"}, {"3", "3"}, {NULL, NULL}};
static struct config_enum syncopts[] = {{"P", "1"}, {NULL, NULL}};
static struct config_enum varseqopts[] = {{"0", "0"}, {"1", "1"}, {"2", "2"}, {"3", "3"},
					  {"mf","11"},{"gc","10"},{"gc3","10"},
					  {"b4","4"},{"b32","4"},
					  {"l4","6"},{"l32","6"},
					  {NULL, NULL}};
/* Make sure the values here match up with those defined in common.h */
static struct config_enum relopts[]	= {
						{"0","0"},{"gc","10"},{"mf","11"},{"gc3","10"},
						{"b4","4"},{"b32","4"},{"b8","5"},{"b64","5"},
						{"l4","6"},{"l32","6"},{"l8","7"},{"l64","7"},
						{NULL,NULL}};
static struct config_enum format_opts[]	= {{"0","0"},
						{"gc","10"},{"gc3","10"},{"mf","11"},
						{NULL,NULL}};
#if defined(WITH_FILE_FORMAT) && (WITH_FILE_FORMAT == COB_FILE_IS_MF)
static char	file_format_dflt[8] = "mf";	/* Default file format */
static char	varrel_dflt[8] = "mf";	/* Default Variable length Relative file format */
static char	fixrel_dflt[8] = "mf";	/* Default Fixed length Relative file format */
static char	varseq_dflt[8] = "mf";	/* Default Variable length Sequential MF format */
#elif defined(WITH_FILE_FORMAT) && (WITH_FILE_FORMAT == COB_FILE_IS_GC)
static char	file_format_dflt[8] = "gc";	/* Default file format */
static char	varrel_dflt[8] = "gc";	/* Default Variable length Relative file format */
static char	fixrel_dflt[8] = "gc";	/* Default Fixed length Relative file format */
static char	varseq_dflt[8] = "gc";	/* varseq0: Default Variable length Sequential file format */
#else
static char	file_format_dflt[8] = "0";	/* Default file format */
static char	varrel_dflt[8] = "0";	/* Default Variable length Relative file format */
static char	fixrel_dflt[8] = "0";	/* Default Fixed length Relative file format */
static char	varseq_dflt[8] = "0";	/* varseq0: Default Variable length Sequential file format */
#endif
static struct config_enum shareopts[]	= {{"none","0"},{"read","1"},{"all","2"},{"no","4"},{NULL,NULL}};
static struct config_enum retryopts[]	= {{"none","0"},{"never","64"},{"forever","8"},{NULL,NULL}};
static struct config_enum dict_opts[]	= {{"false","0"},{"true","1"},{"always","2"},
											{"no","0"},{"min","1"},{"max","2"},{NULL,NULL}};
static struct config_enum dups_opts[]	= {{"default","0"},{"never","1"},{"always","2"}};
static struct config_enum bdborder[]	= {
						{"native","0"},
						{"big-endian","1"},{"little-endian","2"},
						{"big_endian","1"},{"little_endian","2"},
						{NULL,NULL}};
static unsigned char min_conf_length = 0;
static const char *not_set;

/*
 * Table of possible environment variables and/or runtime.cfg parameters:
   Env Var name, Name used in run-time config file, Default value (NULL for aliases), Table of Alternate values,
   Grouping for display of run-time options, Data type, Location within structure (adds computed length of referenced field),
   Set by which runtime.cfg file, value set by a different keyword,
   optional: Minimum accepted value, Maximum accepted value
 */
static struct config_tbl gc_conf[] = {
	{"COB_LOAD_CASE", "load_case", 		"0", 	lwrupr, GRP_CALL, ENV_UINT | ENV_ENUMVAL, SETPOS (name_convert)},
	{"COB_PHYSICAL_CANCEL", "physical_cancel", 	"0", 	never, GRP_CALL, ENV_BOOL | ENV_ENUMVAL, SETPOS (cob_physical_cancel)},
	{"default_cancel_mode", "default_cancel_mode", 	NULL, NULL, GRP_HIDE, ENV_BOOL | ENV_NOT, SETPOS (cob_physical_cancel)},
	{"LOGICAL_CANCELS", "logical_cancels", 	NULL, NULL, GRP_HIDE, ENV_BOOL | ENV_NOT, SETPOS (cob_physical_cancel)},
	{"COB_LIBRARY_PATH", "library_path", 	NULL, 	NULL, GRP_CALL, ENV_PATH, SETPOS (cob_library_path)}, /* default value set in cob_init_call() */
	{"COB_PRE_LOAD", "pre_load", 		NULL, 	NULL, GRP_CALL, ENV_STR, SETPOS (cob_preload_str)},
	{"COB_BELL", "bell", 			"0", 	beepopts, GRP_SCREEN, ENV_UINT | ENV_ENUMVAL, SETPOS (cob_beep_value)},
	{"COB_DEBUG_LOG", "debug_log", 		NULL, 	NULL, GRP_HIDE, ENV_FILE, SETPOS (cob_debug_log)},
	{"COB_DISABLE_WARNINGS", "disable_warnings", "0", 	NULL, GRP_MISC, ENV_BOOL | ENV_NOT, SETPOS (cob_display_warn)},
	{"COB_ENV_MANGLE", "env_mangle", 		"0", 	NULL, GRP_MISC, ENV_BOOL, SETPOS (cob_env_mangle)},
	{"COB_REDIRECT_DISPLAY", "redirect_display", "0", 	NULL, GRP_SCREEN, ENV_BOOL, SETPOS (cob_disp_to_stderr)},
	{"COB_SCREEN_ESC", "screen_esc", 		"0", 	NULL, GRP_SCREEN, ENV_BOOL, SETPOS (cob_use_esc)},
	{"COB_SCREEN_EXCEPTIONS", "screen_exceptions", "0", NULL, GRP_SCREEN, ENV_BOOL, SETPOS (cob_extended_status)},
	{"COB_TIMEOUT_SCALE", "timeout_scale", 	"0", 	timeopts, GRP_SCREEN, ENV_UINT, SETPOS (cob_timeout_scale)},
	{"COB_INSERT_MODE", "insert_mode", "0", NULL, GRP_SCREEN, ENV_BOOL, SETPOS (cob_insert_mode)},
	{"COB_MOUSE_FLAGS", "mouse_flags", "1", NULL, GRP_SCREEN, ENV_UINT, SETPOS (cob_mouse_flags)},
	{"MOUSE_FLAGS", "mouse_flags", NULL, NULL, GRP_HIDE, ENV_UINT, SETPOS (cob_mouse_flags)},
#ifdef HAVE_MOUSEINTERVAL	/* possibly add an internal option for mouse support, too */
	{"COB_MOUSE_INTERVAL", "mouse_interval", "100", NULL, GRP_SCREEN, ENV_UINT, SETPOS (cob_mouse_interval), 0, 166},
#endif
	{"COB_SET_DEBUG", "debugging_mode", 		"0", 	NULL, GRP_MISC, ENV_BOOL | ENV_RESETS, SETPOS (cob_debugging_mode)},
	{"COB_SET_TRACE", "set_trace", 		"0", 	NULL, GRP_MISC, ENV_BOOL, SETPOS (cob_line_trace)},
	{"COB_TRACE_FILE", "trace_file", 		NULL, 	NULL, GRP_MISC, ENV_FILE, SETPOS (cob_trace_filename)},
	{"COB_TRACE_FORMAT", "trace_format",	"%P %S Line: %L", NULL, GRP_MISC, ENV_STR, SETPOS (cob_trace_format)},
	{"COB_STACKTRACE", "stacktrace", 	"1", 	NULL, GRP_MISC, ENV_BOOL, SETPOS (cob_stacktrace)},
	{"COB_CORE_ON_ERROR", "core_on_error", 	"0", 	coeopts, GRP_MISC, ENV_UINT | ENV_ENUMVAL, SETPOS (cob_core_on_error)},
	{"COB_CORE_FILENAME", "core_filename", 	"./core.libcob", 	NULL, GRP_MISC, ENV_STR, SETPOS (cob_core_filename)},
	{"COB_DUMP_FILE", "dump_file",		NULL,	NULL, GRP_MISC, ENV_FILE, SETPOS (cob_dump_filename)},
	{"COB_DUMP_WIDTH", "dump_width",		"100",	NULL, GRP_MISC, ENV_UINT, SETPOS (cob_dump_width)},
	{"COB_TRACE_IO","trace_io",		NULL,	NULL,GRP_FILE,ENV_BOOL,SETPOS(cob_trace_io)},
	{"COB_STATS_RECORD","stats_record",	NULL,	NULL,GRP_MISC,ENV_BOOL,SETPOS(cob_stats_record)},
	{"COB_STATS_FILE","stats_file",		NULL,	NULL,GRP_MISC,ENV_FILE,SETPOS(cob_stats_filename)},
#ifdef  _WIN32
	/* checked before configuration load if set from environment in cob_common_init() */
	{"COB_UNIX_LF", "unix_lf", 		"0", 	NULL, GRP_FILE, ENV_BOOL, SETPOS (cob_unix_lf)},
#endif
	{"USERNAME", "username", 			NULL, 	NULL, GRP_SYSENV, ENV_STR, SETPOS (cob_user_name)},	/* default set in cob_init() */
	{"LOGNAME", "logname", 			NULL, 	NULL, GRP_HIDE, ENV_STR, SETPOS (cob_user_name)},
#if !defined (_WIN32) || defined (__MINGW32__) /* cygwin does not define _WIN32 */
	{"LANG", "lang", 				NULL, 	NULL, GRP_SYSENV, ENV_STR, SETPOS (cob_sys_lang)},
#if defined (__linux__) || defined (__CYGWIN__) || defined (__MINGW32__)
	{"OSTYPE", "ostype", 			NULL, 	NULL, GRP_SYSENV, ENV_STR, SETPOS (cob_sys_type)},
#endif
	{"TERM", "term", 				NULL, 	NULL, GRP_SYSENV, ENV_STR, SETPOS (cob_sys_term)},
#endif
#if defined (_WIN32) && !defined (__MINGW32__)
	{"OS", "ostype", 			NULL, 	NULL, GRP_SYSENV, ENV_STR, SETPOS (cob_sys_type)},
#endif
	{"COB_FILE_PATH","file_path",		NULL,	NULL,GRP_FILE,ENV_PATH,SETPOS(cob_file_path)},
	{"COB_FILE_FORMAT","file_format",file_format_dflt,format_opts,GRP_FILE,ENV_UINT|ENV_ENUM,SETPOS(cob_file_format)},
	{"COB_FIXREL_FORMAT","fixrel_format",	fixrel_dflt,relopts,GRP_FILE,ENV_UINT|ENV_ENUM,SETPOS(cob_fixrel_type)},
	{"COB_VARREL_FORMAT","varrel_format",	varrel_dflt,relopts,GRP_FILE,ENV_UINT|ENV_ENUM,SETPOS(cob_varrel_type)},
	{"COB_VARSEQ_FORMAT","varseq_format",	varseq_dflt,varseqopts,GRP_FILE,ENV_UINT|ENV_ENUM,SETPOS(cob_varseq_type)},
	{"COB_BDB_BYTEORDER","bdb_byteorder",	"native",bdborder,GRP_FILE,ENV_UINT|ENV_ENUM,SETPOS(cob_bdb_byteorder)},
	{"COB_LS_FIXED","ls_fixed",				"0",NULL,GRP_FILE,ENV_BOOL,SETPOS(cob_ls_fixed)},
	{"STRIP_TRAILING_SPACES","strip_trailing_spaces",NULL,NULL,GRP_HIDE,ENV_BOOL|ENV_NOT,SETPOS(cob_ls_fixed)},
	{"COB_LS_SPLIT","ls_split",				"1",NULL,GRP_FILE,ENV_BOOL,SETPOS(cob_ls_split)},
	{"COB_LS_INSTAB","ls_instab",			"false",NULL,GRP_FILE,ENV_BOOL,SETPOS(cob_ls_instab)},
	{"COB_LS_NULLS","ls_nulls",				"not set",notset,GRP_FILE,ENV_BOOL,SETPOS(cob_ls_nulls)},
	{"COB_LS_VALIDATE","ls_validate",		"1",NULL,GRP_FILE,ENV_BOOL,SETPOS(cob_ls_validate)},
	{"COB_SHARE_MODE","share_mode",			"none",shareopts,GRP_FILE,ENV_UINT|ENV_ENUM,SETPOS(cob_share_mode)},
	{"COB_RETRY_MODE","retry_mode",			"none",retryopts,GRP_FILE,ENV_UINT|ENV_ENUM,SETPOS(cob_retry_mode)},
	{"COB_RETRY_TIMES","retry_times",		"0",NULL,GRP_FILE,ENV_UINT,SETPOS(cob_retry_times)},
	{"COB_RETRY_SECONDS","retry_seconds",	"0",NULL,GRP_FILE,ENV_UINT,SETPOS(cob_retry_seconds)},
	{"COB_SORT_CHUNK","sort_chunk",		"256K",	NULL,GRP_FILE,ENV_SIZE,SETPOS(cob_sort_chunk),(128 * 1024),(16 * 1024 * 1024)},
	{"COB_SORT_MEMORY","sort_memory",	"128M",	NULL,GRP_FILE,ENV_SIZE,SETPOS(cob_sort_memory),(1024*1024),4294967294UL /* max. guaranteed - 1 */},
	{"COB_SYNC","sync",			"false",syncopts,GRP_FILE,ENV_BOOL,SETPOS(cob_do_sync)},
    {"COB_KEYCHECK","keycheck",     "on",NULL,GRP_FILE,ENV_BOOL,SETPOS(cob_keycheck)},
    {"COB_FILE_DICTIONARY","file_dictionary",     "min",dict_opts,GRP_FILE,ENV_UINT|ENV_ENUMVAL,SETPOS(cob_file_dict),0,3},
	{"COB_FILE_DICTIONARY_PATH","file_dictionary_path",		NULL,	NULL,GRP_FILE,ENV_FILE,SETPOS(cob_dictionary_path)},
	{"COB_FILE_ROLLBACK", "rollback", 	"0", 	NULL, GRP_FILE, ENV_BOOL, SETPOS (cob_file_rollback)},
	{"COB_FILE_VBISAM", "file_vbisam", 	"0", 	NULL, GRP_FILE, ENV_BOOL, SETPOS (cob_file_vbisam)},
	{"COB_FILE_ISNODAT", "file_isnodat","0", 	NULL, GRP_FILE, ENV_BOOL, SETPOS (cob_file_isnodat)},
	{"COB_STOP_RUN_COMMIT", "stop_run_commit", 	"0", 	NULL, GRP_FILE, ENV_BOOL, SETPOS (cob_stop_run_commit)},
    {"COB_DUPS_AHEAD","dups_ahead",     "default",dups_opts,GRP_FILE,ENV_UINT|ENV_ENUMVAL,SETPOS(cob_file_dups),0,3},
    {"COB_SEQ_CONCAT_NAME","seq_concat_name","0",NULL,GRP_FILE,ENV_BOOL,SETPOS(cob_concat_name)},
    {"COB_SEQ_CONCAT_SEP","seq_concat_sep","+",NULL,GRP_FILE,ENV_CHAR,SETPOS(cob_concat_sep),1},
#ifdef  WITH_DB
	{"DB_HOME", "db_home", 			NULL, 	NULL, GRP_FILE, ENV_FILE, SETPOS (bdb_home)},
#endif
	{"COB_COL_JUST_LRC", "col_just_lrc", "true", 	NULL, GRP_FILE, ENV_BOOL, SETPOS (cob_col_just_lrc)},
	{"COB_DISPLAY_PRINT_PIPE", "display_print_pipe",		NULL,	NULL, GRP_SCREEN, ENV_STR, SETPOS (cob_display_print_pipe)},
	{"COBPRINTER", "printer",		NULL,	NULL, GRP_HIDE, ENV_STR, SETPOS (cob_display_print_pipe)},
	{"COB_DISPLAY_PRINT_FILE", "display_print_file",		NULL,	NULL, GRP_SCREEN, ENV_STR,SETPOS (cob_display_print_filename)},
	{"COB_DISPLAY_PUNCH_FILE", "display_punch_file",		NULL,	NULL, GRP_SCREEN, ENV_STR,SETPOS (cob_display_punch_filename)},
	{"COB_LEGACY", "legacy", 			NULL, 	NULL, GRP_SCREEN, ENV_BOOL, SETPOS (cob_legacy)},
	{"COB_EXIT_WAIT", "exit_wait", 		"1", 	NULL, GRP_SCREEN, ENV_BOOL, SETPOS (cob_exit_wait)},
	{"COB_EXIT_MSG", "exit_msg", 		NULL, NULL, GRP_SCREEN, ENV_STR, SETPOS (cob_exit_msg)},	/* default set in cob_init_screenio() */
	{"COB_CURRENT_DATE" ,"current_date",	NULL,	NULL, GRP_MISC, ENV_STR, SETPOS (cob_date)},
	{"COB_DATE", "date",			NULL,	NULL, GRP_HIDE, ENV_STR, SETPOS (cob_date)},
	{NULL, NULL, 0, 0}
};
#define NUM_CONFIG (sizeof (gc_conf) /sizeof (struct config_tbl) - 1)
#define FUNC_NAME_IN_DEFAULT NUM_CONFIG + 1

/* 
 * Table of 'signal' supported by this system 
 */
static struct signal_table {
	short		sig;			/* Signal number */
	short		for_set;		/* Set via 'cob_set_signal' 1=set if not SIG_IGN */
	short		for_dump;		/* set via 'cob_set_dump_signal' */
	short		unused;
	const char	*shortname;		/* Short signal name */
	const char	*description;	/* Longer desciption message */
} signals[] = {
#ifdef	SIGINT
	{SIGINT,1,0,0,"SIGINT"},
#endif
#ifdef	SIGHUP
	{SIGHUP,1,0,0,"SIGHUP"},
#endif
#ifdef	SIGQUIT
	{SIGQUIT,1,0,0,"SIGQUIT"},
#endif
#ifdef	SIGTERM
	{SIGTERM,1,0,0,"SIGTERM"},
#endif
#ifdef	SIGEMT
	{SIGEMT,1,0,0,"SIGEMT"},
#endif
#ifdef	SIGPIPE
	{SIGPIPE,1,0,0,"SIGPIPE"},
#endif
#ifdef	SIGIO
	{SIGIO,1,0,0,"SIGIO"},
#endif
#ifdef	SIGSEGV
	{SIGSEGV,2,1,0,"SIGSEGV"},
#endif
#ifdef	SIGBUS
	{SIGBUS,2,1,0,"SIGBUS"},
#endif
	{SIGFPE,1,1,0,"SIGFPE"},	/* always defined, if missing */
#ifdef	SIGILL
	{SIGILL,0,0,0,"SIGILL"},
#endif
#ifdef	SIGABRT
	{SIGABRT,0,0,0,"SIGABRT"},
#endif
#ifdef	SIGKILL
	{SIGKILL,0,0,0,"SIGKILL"},
#endif
#ifdef	SIGALRM
	{SIGALRM,0,0,0,"SIGALRM"},
#endif
#ifdef	SIGSTOP
	{SIGSTOP,0,0,0,"SIGSTOP"},
#endif
#ifdef	SIGCHLD
	{SIGCHLD,0,0,0,"SIGCHLD"},
#endif
#ifdef	SIGCLD
	{SIGCLD,0,0,0,"SIGCLD"},
#endif
	{-1,0,0,0,"unknown"}
};
#define NUM_SIGNALS (int)((sizeof (signals) / sizeof (struct signal_table)) - 1)
const char *signal_msgid = "signal";
const char *warning_msgid = "warning: ";
const char *more_stack_frames_msgid = "(more COBOL runtime elements follow...)";

/* Local functions */
static int		translate_boolean_to_int	(const char *ptr);
static cob_s64_t	get_sleep_nanoseconds	(cob_field *nano_seconds);
static cob_s64_t	get_sleep_nanoseconds_from_seconds	(cob_field *decimal_seconds);
static void		internal_nanosleep	(cob_s64_t nsecs, int round_to_minmal);

static int		set_config_val	(char *value, int pos);
static char		*get_config_val	(char *value, int pos, char *orgvalue);

static void		cob_dump_module (char *reason);
static char		abort_reason[COB_MINI_BUFF] = "";
static unsigned int 	dump_trace_started;	/* ensures that we dump/stacktrace only once */
#define 		DUMP_TRACE_DONE_DUMP 		(1U << 0)
#define 		DUMP_TRACE_DONE_TRACE		(1U << 1)
#define 		DUMP_TRACE_ACTIVE_TRACE		(1U << 2)
static void		cob_stack_trace_internal (FILE *target, int verbose, int count);

#ifdef COB_DEBUG_LOG
static void		cob_debug_open	(void);
#endif
void		conf_runtime_error_value	(const char *value, const int conf_pos);
void		conf_runtime_error	(const int finish_error, const char *fmt, ...);

static void
cob_exit_common (void)
{
	struct cob_external	*p;
	struct cob_external	*q;
	struct cob_alloc_cache	*x;
	struct cob_alloc_cache	*y;

#ifdef	HAVE_SETLOCALE
	if (cobglobptr->cob_locale_orig) {
		(void) setlocale (LC_ALL, cobglobptr->cob_locale_orig);
		cob_free (cobglobptr->cob_locale_orig);
	}
	if (cobglobptr->cob_locale) {
		cob_free (cobglobptr->cob_locale);
	}
	if (cobglobptr->cob_locale_ctype) {
		cob_free (cobglobptr->cob_locale_ctype);
	}
	if (cobglobptr->cob_locale_collate) {
		cob_free (cobglobptr->cob_locale_collate);
	}
	if (cobglobptr->cob_locale_messages) {
		cob_free (cobglobptr->cob_locale_messages);
	}
	if (cobglobptr->cob_locale_monetary) {
		cob_free (cobglobptr->cob_locale_monetary);
	}
	if (cobglobptr->cob_locale_numeric) {
		cob_free (cobglobptr->cob_locale_numeric);
	}
	if (cobglobptr->cob_locale_time) {
		cob_free (cobglobptr->cob_locale_time);
	}
#endif

	if (commlnptr) {
		cob_free (commlnptr);
	}
	if (cob_local_env) {
		cob_free (cob_local_env);
	}

	/* Free library routine stuff */

	if (cobglobptr->cob_term_buff) {
		cob_free (cobglobptr->cob_term_buff);
	}

	/* Free cached externals */
	for (p = basext; p;) {
		q = p;
		p = p->next;
		if (q->ename) {
			cob_free (q->ename);
		}
		if (q->ext_alloc) {
			cob_free (q->ext_alloc);
		}
		cob_free (q);
	}

	/* Free cached mallocs */
	for (x = cob_alloc_base; x;) {
		y = x;
		x = x->next;
		cob_free (y->cob_pointer);
		cob_free (y);
	}
	dyn_attr_list = NULL;

	/* Free last stuff */
	if (cob_last_sfile) {
		cob_free ((void *)cob_last_sfile);
	}
	if (cobglobptr) {
		if (cobglobptr->cob_main_argv0) {
			cob_free ((void *)(cobglobptr->cob_main_argv0));
		}
		cob_free (cobglobptr);
		cobglobptr = NULL;
	}
	if (cobsetptr) {
		void 	*data;
		char	*str;
		unsigned int	i;
		if (cobsetptr->cob_config_file) {
			for (i = 0; i < cobsetptr->cob_config_num; i++) {
				if (cobsetptr->cob_config_file[i]) {
					cob_free ((void *)cobsetptr->cob_config_file[i]);
				}
			}
			cob_free ((void *)cobsetptr->cob_config_file);
		}
		/* Free all strings pointed to by cobsetptr */
		for (i = 0; i < NUM_CONFIG; i++) {
			if ((gc_conf[i].data_type & ENV_STR)
			||  (gc_conf[i].data_type & ENV_FILE)
			||  (gc_conf[i].data_type & ENV_PATH)) {	/* String/Path to be stored as a string */
				data = (void *)((char *)cobsetptr + gc_conf[i].data_loc);
				memcpy (&str, data, sizeof (char *));
				if (str != NULL) {
					cob_free ((void *)str);
					str = NULL;
					memcpy (data, &str, sizeof (char *));	/* Reset pointer to NULL */
				}
			}
		}
		if (cobsetptr->cob_preload_str_set) {
			cob_free((void*)(cobsetptr->cob_preload_str_set));
		}
		cob_free (cobsetptr);
		cobsetptr = NULL;
	}
	cob_initialized = 0;
}

static void
cob_exit_common_modules (void)
{
	cob_module	*mod;
	struct cob_alloc_module	*ptr, *nxt;
	int		(*cancel_func)(const int);

	/* Call each module to release local memory
	   - currently used for: decimals -
	   and remove it from the internal module list */
	for (ptr = cob_module_list; ptr; ptr = nxt) {
		mod = ptr->cob_pointer;
		nxt = ptr->next;
		if (mod && mod->module_cancel.funcint) {
			mod->module_active = 0;
			cancel_func = mod->module_cancel.funcint;
			(void)cancel_func (-20);	/* Clear just decimals */
		}
		cob_free (ptr);
	}
	cob_module_list = NULL;
}

static void
ss_terminate_routines (void)
{
	if (!cob_initialized || !cobglobptr) {
		return;
	}
	cob_exit_fileio_msg_only ();
	cob_exit_screen_from_signal(1);

	if (COB_MODULE_PTR && abort_reason[0] != 0) {
		if (cobsetptr->cob_stacktrace) {
			if (!(dump_trace_started & (DUMP_TRACE_DONE_TRACE | DUMP_TRACE_ACTIVE_TRACE))) {
				dump_trace_started |= DUMP_TRACE_DONE_TRACE;
				dump_trace_started |= DUMP_TRACE_ACTIVE_TRACE;
				cob_stack_trace_internal (stderr, 1, 0);
				dump_trace_started ^= DUMP_TRACE_ACTIVE_TRACE;
			}
		}
		/* note: no dump code here as getting all the data out of the modules
		   isn't signal-safe at all; the code to write it out isn't either */
	}
}

static void
cob_terminate_routines (void)
{
	if (!cob_initialized || !cobglobptr) {
		return;
	}
	fflush (stderr);

	cob_exit_fileio_msg_only ();

	if (COB_MODULE_PTR && abort_reason[0] != 0) {
		if (cobsetptr->cob_stacktrace) {
			if (!(dump_trace_started & (DUMP_TRACE_DONE_TRACE | DUMP_TRACE_ACTIVE_TRACE))) {
				dump_trace_started |= DUMP_TRACE_DONE_TRACE;
				dump_trace_started |= DUMP_TRACE_ACTIVE_TRACE;
				cob_stack_trace_internal (stderr, 1, 0);
				dump_trace_started ^= DUMP_TRACE_ACTIVE_TRACE;
			}
		}
		if (!(dump_trace_started & DUMP_TRACE_DONE_DUMP)) {
			dump_trace_started |= DUMP_TRACE_DONE_DUMP;
			cob_dump_module (abort_reason);
		}
	}

	if (cobsetptr->cob_dump_file == cobsetptr->cob_trace_file
	 || cobsetptr->cob_dump_file == stderr) {
		cobsetptr->cob_dump_file = NULL;
	}

	if (cobsetptr->cob_dump_file) {
		fclose (cobsetptr->cob_dump_file);
		cobsetptr->cob_dump_file = NULL;
	}

	if (cobsetptr->cob_trace_file
	 && cobsetptr->cob_trace_file != stderr
	 && !cobsetptr->external_trace_file	/* note: may include stdout */) {
		fclose (cobsetptr->cob_trace_file);
	}
	cobsetptr->cob_trace_file = NULL;

	/* close punch file if self-opened */
	if (cobsetptr->cob_display_punch_file
	 && cobsetptr->cob_display_punch_filename) {
		fclose (cobsetptr->cob_display_punch_file);
		cobsetptr->cob_display_punch_file = NULL;
	}

	cob_exit_screen ();
	cob_exit_fileio ();
#ifdef COB_DEBUG_LOG
	/* close debug log (delete file if empty) */
	if (cob_debug_file
	 && cob_debug_file != stderr) {
		/* note: cob_debug_file can only be identical to cob_trace_file
		         if same file name was used, not with external_trace_file */
		if (cob_debug_file == cobsetptr->cob_trace_file) {
			cobsetptr->cob_trace_file = NULL;
		}
		if (cob_debug_file_name != NULL
		 && ftell (cob_debug_file) == 0) {
			fclose (cob_debug_file);
			unlink (cob_debug_file_name);
		} else {
			fclose (cob_debug_file);
		}
	}
	cob_debug_file = NULL;
	if (cob_debug_file_name) {
		cob_free (cob_debug_file_name);
		cob_debug_file_name = NULL;
	}
#endif

	cob_exit_reportio ();
	cob_exit_mlio ();

	cob_exit_intrinsic ();
	cob_exit_strings ();
	cob_exit_numeric ();

	cob_exit_common_modules ();
	cob_exit_call ();
	cob_exit_cobcapi ();
	cob_exit_common ();
}

static void
cob_get_source_line ()
{
	if (cobglobptr
	 && COB_MODULE_PTR) {
		cob_module	*mod = COB_MODULE_PTR;
		while (mod && mod->module_stmt == 0) {
			mod = mod->next;
		}
		if (mod
		 && mod->module_stmt != 0
		 && mod->module_sources) {
			cob_source_file =
				mod->module_sources[COB_GET_FILE_NUM (mod->module_stmt)];
			cob_source_line = COB_GET_LINE_NUM (mod->module_stmt);
		}
#if 0	/* note: those are also manually set
		         and therefore may not be reset here */
	} else {
		cob_source_file = NULL;
		cob_source_line = 0;
#endif
	}
}

/* reentrant version of strerror */
static char *
cob_get_strerror (void)
{
	size_t size;
	char *ret;
#ifdef HAVE_STRERROR
	char *msg = strerror (errno);
	size = strlen (msg);
#else
	char msg[COB_ERRBUF_SIZE];
	size = snprintf (msg, COB_ERRBUF_SIZE, _("system error %d"), errno);
	if (size >= COB_ERRBUF_SIZE) {
		/* very unlikely, but if that would be a bad msg catalog
		   we don't want to memcpy from invalid data */
		size = COB_ERRBUF_SIZE - 1;
	}
#endif
	ret = cob_cache_malloc (size + 1);
	memcpy (ret, msg, size + 1);
	return ret;
}


static char ss_itoa_buf[12];
/* signal safe integer to string conversion,;
   operates on ss_itoa_buff, returns length;
   uses ideas of Shaoquan's
   https://github.com/wsq003/itoa_for_linux/blob/master/itoa.c */
static size_t
ss_itoa_u10 (int value)
{
	const unsigned int radix = 10;
	char *p, *p2;
	unsigned int digit;
	unsigned int u;
	size_t len;

	p = ss_itoa_buf;

	if (value < 0) {
		*p++ = '-';
		value = 0 - value;
	}
	u = (unsigned int)value;

	p2 = p;

	do {
		digit = u % radix;
		u /= radix;
		*p++ = '0' + digit;
	} while (u > 0);

	len = p - ss_itoa_buf;
	*p-- = 0;

	/* swap around */
	do {
		char temp = *p;
		*p-- = *p2;
		*p2++ = temp;

	} while (p2 < p);

	return len;
}

static COB_INLINE void
set_source_location (const char **file, unsigned int *line)
{
	*file = cob_source_file; /* may be NULL */
	*line = cob_source_line; /* may be zero */
	if (cobglobptr) {
		const cob_module *mod = COB_MODULE_PTR;
		if (mod
		 && mod->module_stmt != 0
		 && mod->module_sources) {
			*file = mod->module_sources[COB_GET_FILE_NUM(mod->module_stmt)];
			*line = COB_GET_LINE_NUM(mod->module_stmt);
		}
	}
}

/* write integer to stderr using fixed buffer */
#define write_to_stderr_or_return_int(i) \
	if (write (STDERR_FILENO, ss_itoa_buf, ss_itoa_u10 (i)) == -1) return
/* write char array (constant C string) to stderr */
#define write_to_stderr_or_return_arr(ch_arr) \
	if (write (STDERR_FILENO, ch_arr, sizeof (ch_arr) - 1) == -1) return
/* write string to stderr, byte count computed with strlen,
   str is evaluated twice */
#define write_to_stderr_or_return_str(str) \
	if (write (STDERR_FILENO, str, strlen (str)) == -1) return

/* write integer to fileno using fixed buffer */
#define write_or_return_int(fileno,i) \
	if (write (fileno, ss_itoa_buf, ss_itoa_u10 (i)) == -1) return
/* write char array (constant C string) to fileno */
#define write_or_return_arr(fileno, ch_arr) \
	if (write (fileno, ch_arr, sizeof (ch_arr) - 1) == -1) return
/* write string to fileno, byte count computed with strlen,
   str is evaluated twice */
#define write_or_return_str(fileno,str) \
	if (write (fileno, str, strlen (str)) == -1) return

#if 0 /* unused */
/* write buffer with given byte count to stderr */
#define write_to_stderr_or_return_buf(buff,count) \
	if (write (STDERR_FILENO, buff, count) == -1) return
/* write buffer with given byte count to fileno */
#define write_or_return_buf(fileno,buff,count) \
	if (write (fileno, buff, count) == -1) return
#endif

static void
output_source_location (void)
{
	const char		*source_file;
	unsigned int	 source_line;
	set_source_location (&source_file, &source_line);

	if (source_file) {
		write_to_stderr_or_return_str (source_file);
		if (source_line) {
			write_to_stderr_or_return_arr (":");
			write_to_stderr_or_return_int ((int)source_line);
		}
		write_to_stderr_or_return_arr (": ");
	}
}

static int
create_dumpfile (void)
{
	/* Note: this function is not called from a signal handler so may use stdio */

	char cmd [COB_NORMAL_BUFF];
	const char *default_name = "./core.libcob";
	const char *envval;
	size_t content_size;
	int ret;

	if (cobsetptr) {
		envval = cobsetptr->cob_core_filename;
	} else {
		envval = cob_getenv_direct ("COB_CORE_FILENAME");
	}
	if (envval == NULL) {
		envval = default_name;
	}

	content_size = snprintf (cmd, COB_NORMAL_BUFF, "gcore -a -o %s %d", envval, cob_sys_getpid ());
	/* LCOV_EXCL_START */
	if (content_size >= COB_NORMAL_BUFF) {
		/* in unlikely case of "cob_core_filename is too long" use the simple one */
		sprintf (cmd, "gcore -a -o %s %d", default_name, cob_sys_getpid ());
	}
	/* LCOV_EXCL_STOP */

	ret = system (cmd);
	if (ret) {
		fprintf (stderr, "\nlibcob: requested coredump creation failed with status %d\n", ret);
	}
	return ret;
}


#ifdef	HAVE_SIGNAL_H
static void
cob_sig_handler (int sig)
{
	const char *signal_name;
	const char *msg;
	char signal_text[COB_MINI_BUFF] = {0};
	size_t str_len;

#if	defined (HAVE_SIGACTION) && !defined (SA_RESETHAND)
	struct sigaction	sa;
#endif

#ifdef	HAVE_SIG_ATOMIC_T
	 if (sig_is_handled) {
#ifdef	HAVE_RAISE
		raise (sig);
#else
		kill (getpid (), sig);
#endif
		exit (sig);
	}
	sig_is_handled = 1;
#endif

#if 0	/* We do not generally flush whatever we may have in our streams
     	   as stdio is not signal-safe;
		   we _may_ do this if not SIGSEGV/SIGBUS/SIGABRT */
	fflush (stdout);
	fflush (stderr);
#endif

	signal_name = cob_get_sig_name (sig);
	/* LCOV_EXCL_START */
	if (signal_name == signals[NUM_SIGNALS].shortname) {
		/* not translated as it is a very unlikely error case */
		signal_name = signals[NUM_SIGNALS].description;	/* translated unknown */
		write_to_stderr_or_return_arr ("\ncob_sig_handler caught not handled signal: ");
		write_to_stderr_or_return_int (sig);
		write_to_stderr_or_return_arr ("\n");
	}
	/* LCOV_EXCL_STOP */

	if (cobsetptr
	&& !in_stop_run)
		cob_rollback ();
	in_stop_run = 1;

	/* Skip dumping for SIGTERM, SIGINT and "other process" issue's SIGHUP, SIGPIPE */
	switch (sig) {
	case -1:
#ifdef	SIGTERM
	case SIGTERM:
#endif
#ifdef	SIGINT
	case SIGINT:
#endif
#ifdef	SIGHUP
	case SIGHUP:
#endif
#ifdef	SIGPIPE
	case SIGPIPE:
#endif
		dump_trace_started |= DUMP_TRACE_DONE_DUMP;
		/* Fall-through */
	default:
		break;
	}

#ifdef	HAVE_SIGACTION
#ifndef	SA_RESETHAND
	memset (&sa, 0, sizeof (sa));
	sa.sa_handler = SIG_DFL;
	(void)sigemptyset (&sa.sa_mask);
	(void)sigaction (sig, &sa, NULL);
#endif
#else
	(void)signal (sig, SIG_DFL);
#endif
	cob_exit_screen ();

	write_to_stderr_or_return_arr ("\n");
	cob_get_source_line ();
	output_source_location ();

	msg = cob_get_sig_description (sig);
	write_to_stderr_or_return_str (msg);

	/* setup "signal %s" */
	str_len = strlen (signal_msgid);
	memcpy (signal_text, signal_msgid, str_len);
	*(signal_text + str_len++) = ' ';
	memcpy (signal_text + str_len, signal_name, strlen (signal_name));

	write_to_stderr_or_return_arr (" (");
	write_to_stderr_or_return_str (signal_text);
	write_to_stderr_or_return_arr (")\n\n");

	if (cob_initialized) {
		if (abort_reason[0] == 0) {
			memcpy (abort_reason, signal_text, COB_MINI_BUFF);
#if 0	/* Is there a use in this message ?*/
			write_to_stderr_or_return_str (abnormal_termination_msgid);
			write_to_stderr_or_return_arr ("\n");
#endif
		}
	}

	/* early coredump if requested would be nice,
	   but that is not signal-safe so do SIGABRT later... */
	if (cobsetptr && cobsetptr->cob_core_on_error == 3)  {
		cobsetptr->cob_core_on_error = 4;
	}
	switch (sig) {
	case -1:
#ifdef	SIGSEGV
	case SIGSEGV:
#endif
#ifdef	SIGBUS
	case SIGBUS:
#endif
#ifdef	SIGABRT
	case SIGABRT:
#endif
		if (cobsetptr && cobsetptr->cob_core_on_error != 0)  {
			ss_terminate_routines ();
			break;
		}
		/* Fall-through */
	default:
		cob_terminate_routines ();
		break;
	}

	/* call external signal handler if registered */
	if (cob_ext_sighdl != NULL) {
		(*cob_ext_sighdl) (sig);
		cob_ext_sighdl = NULL;
	}
	exit_code = sig;
#ifndef COB_WITHOUT_JMP
	if (return_jmp_buffer_set) {
		longjmp (return_jmp_buf, -3);
	}
#endif
	/* if an explicit requested coredump could not
	   be created - raise SIGABRT here */
	if (cobsetptr && cobsetptr->cob_core_on_error == 4) {
		sig = SIGABRT;
	} else {
		exit (sig);
	}
	signal (sig, SIG_DFL);
#ifdef	HAVE_RAISE
	raise (sig);
#else
	kill (cob_sys_getpid (), sig);
#endif

#if 0 /* we don't necessarily want the OS to handle this,
         so exit in all other cases*/
	exit (sig);
#endif
}
#endif /* HAVE_SIGNAL_H */

/* Raise signal (run both internal and external handlers)
   may return, depending on the signal
*/
void
cob_raise (int sig)
{
#ifdef	HAVE_SIGNAL_H
	/* let the registered signal handlers do their work */
#ifdef	HAVE_RAISE
	raise (sig);
#else
	kill (cob_sys_getpid (), sig);
#endif
	/* else: at least call external signal handler if registered */
#else
	if (cob_ext_sighdl != NULL) {
		(*cob_ext_sighdl) (sig);
		cob_ext_sighdl = NULL;
	}
#endif
}

static COB_INLINE struct signal_table
get_signal_entry (int sig)
{
	unsigned int	k;
	for (k = 0; k < NUM_SIGNALS; k++) {
		if (signals[k].sig == sig)
			break;
	}
	/* if we didn't found a matching signal we end with the max: "unknown" */
	return signals[k];
}


const char *
cob_get_sig_name (int sig)
{
	struct signal_table	signal_entry = get_signal_entry (sig);
	return signal_entry.shortname;
}

const char *
cob_get_sig_description (int sig)
{
	struct signal_table	signal_entry = get_signal_entry (sig);
	/* LCOV_EXCL_START */
	if (!signal_entry.description) {
		/* in theory we could get here before pre-initialization was finished,
		   and would want to have _anything_ to return then */
		return "unknown";
	}
	/* LCOV_EXCL_STOP */
	return signal_entry.description;
}

static void
cob_init_sig_descriptions (void)
{
	int	k;
	for (k = 0; k <= NUM_SIGNALS; k++) {
		/* always defined, if missing */
		if (signals[k].sig == SIGFPE) {
			signals[k].description = _("fatal arithmetic error");
	#ifdef	SIGINT
		} else if (signals[k].sig == SIGINT) {
			signals[k].description = _("interrupt from keyboard");
	#endif
	#ifdef	SIGHUP
		} else if (signals[k].sig == SIGHUP) {
			signals[k].description = _("hangup");
	#endif
	#ifdef	SIGQUIT
		} else if (signals[k].sig == SIGQUIT) {
			signals[k].description = _("quit");
	#endif
	#ifdef	SIGTERM
		} else if (signals[k].sig == SIGTERM) {
			signals[k].description = _("termination");
	#endif
	#ifdef	SIGEMT
		} else if (signals[k].sig == SIGEMT) {
			signals[k].description = _("emt termination");
	#endif
	#ifdef	SIGPIPE
		} else if (signals[k].sig == SIGPIPE) {
			signals[k].description = _("broken pipe");
	#endif
	#ifdef	SIGIO
		} else if (signals[k].sig == SIGIO) {
			signals[k].description = _("I/O signal");
	#endif
	#ifdef	SIGSEGV
		} else if (signals[k].sig == SIGSEGV) {
			signals[k].description = _("attempt to reference invalid memory address");
	#endif
	#ifdef	SIGBUS
		} else if (signals[k].sig == SIGBUS) {
			signals[k].description = _("bus error");
	#endif
	#ifdef	SIGILL
		} else if (signals[k].sig == SIGILL) {
			signals[k].description = _("illegal instruction");
	#endif
	#ifdef	SIGABRT
		} else if (signals[k].sig == SIGABRT) {
			signals[k].description = _("abort");
	#endif
	#ifdef	SIGKILL
		} else if (signals[k].sig == SIGKILL) {
			signals[k].description = _("process killed");
	#endif
	#ifdef	SIGALRM
		} else if (signals[k].sig == SIGALRM) {
			signals[k].description = _("alarm signal");
	#endif
	#ifdef	SIGSTOP
		} else if (signals[k].sig == SIGSTOP) {
			signals[k].description = _("stop process");
	#endif
	#ifdef	SIGCHLD
		} else if (signals[k].sig == SIGCHLD) {
			signals[k].description = _("child process stopped");
	#endif
	#ifdef	SIGCLD
		} else if (signals[k].sig == SIGCLD) {
			signals[k].description = _("child process stopped");
	#endif
		} else {
			signals[k].description = _("unknown");
		}
	}
	/* TRANSLATORS: This msgid is used for an OS signal like SIGABRT. */
	signal_msgid = _("signal");
	/* TRANSLATORS: This msgid is shown for a requested but not complete stack trace. */
	more_stack_frames_msgid = _("(more COBOL runtime elements follow...)");
#if 0	/* Is there a use in this message ?*/
	abnormal_termination_msgid = (_("abnormal termination - file contents may be incorrect");
#endif
	warning_msgid = _("warning: ");
}

static void
cob_set_signal (void)
{
#if	defined (HAVE_SIGNAL_H)
	int k;
#ifdef	HAVE_SIGACTION
	struct sigaction	sa;
	struct sigaction	osa;

	memset (&sa, 0, sizeof (sa));
	memset (&osa, 0, sizeof (osa));
	sa.sa_handler = cob_sig_handler;
#ifdef	SA_RESETHAND
	sa.sa_flags = SA_RESETHAND;
#endif
#ifdef	SA_NOCLDSTOP
	sa.sa_flags |= SA_NOCLDSTOP;
#endif

	for (k = 0; k < NUM_SIGNALS; k++) {
		if (signals[k].for_set) {
			/* Take direct control of some hard errors */
			if (signals[k].for_set == 2) {
				(void)sigemptyset (&sa.sa_mask);
				(void)sigaction (signals[k].sig, &sa, NULL);
			} else {
				/* for the others: only register if not configured
				   from the OS side to be ignored */
				(void)sigaction (signals[k].sig, NULL, &osa);
				if (osa.sa_handler != SIG_IGN) {
					(void)sigemptyset (&sa.sa_mask);
					(void)sigaction (signals[k].sig, &sa, NULL);
				}
				/* CHECKME: how should we handle externally registered
				            error handlers (handler != SIG_DFL)? */
			}
		}
	}
#else	/* still defined (HAVE_SIGNAL_H) */
	for (k = 0; k < NUM_SIGNALS; k++) {
		if (signals[k].for_set) {
			/* Take direct control of some hard errors */
			if (signals[k].for_set == 2) {
				(void)signal (signals[k].sig, cob_sig_handler);
			} else {
				/* for the others: only register if not configured
				   from the OS side to be ignored */
				if (signal (signals[k].sig, SIG_IGN) != SIG_IGN) {
					(void)signal (signals[k].sig, cob_sig_handler);
				}
				/* CHECKME: how should we handle externally registered
				            error handlers (handler != SIG_DFL)? */
			}
		}
	}
#endif
#endif
}

/* Used by cob_set_dump_signal (triggered with symbols generated with -fdump) to catch abort while dumping */
void
cob_set_dump_signal (void *hndlr)
{
#if defined(HAVE_SIGACTION) && !defined(_WIN32)
	int		k;
	sigset_t sigs;
	struct sigaction	sa;
	struct sigaction	osa;

	if (hndlr == NULL)
		hndlr = (void*)SIG_DFL;

	(void)sigemptyset(&sigs);
	/* Unblock signals to allow catch of another abort during dump */
	for (k = 0; k < NUM_SIGNALS; k++) {
		if (signals[k].for_dump) {
			(void)sigaddset(&sigs, signals[k].sig);
		}
	}
	(void)sigprocmask(SIG_UNBLOCK, &sigs, NULL);

	memset (&sa,  0, sizeof (sa));
	memset (&osa, 0, sizeof (osa));
	sa.sa_handler = (void(*)(int))hndlr;

	/* Establish signals to catch and continue */
	for (k = 0; k < NUM_SIGNALS; k++) {
		if (signals[k].for_dump) {
			(void)sigemptyset (&sa.sa_mask);
			(void)sigaction (signals[k].sig, &sa, NULL);
		}
	}
#else
	COB_UNUSED (hndlr);
#endif
}

/* ASCII Sign
 * positive: 0123456789
 * negative: pqrstuvwxy
 */

static int
cob_get_sign_ascii (unsigned char *p)
{
#ifdef	COB_EBCDIC_MACHINE
	switch (*p) {
	case 'p':
		*p = (unsigned char)'0';
		return -1;
	case 'q':
		*p = (unsigned char)'1';
		return -1;
	case 'r':
		*p = (unsigned char)'2';
		return -1;
	case 's':
		*p = (unsigned char)'3';
		return -1;
	case 't':
		*p = (unsigned char)'4';
		return -1;
	case 'u':
		*p = (unsigned char)'5';
		return -1;
	case 'v':
		*p = (unsigned char)'6';
		return -1;
	case 'w':
		*p = (unsigned char)'7';
		return -1;
	case 'x':
		*p = (unsigned char)'8';
		return -1;
	case 'y':
		*p = (unsigned char)'9';
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
		/* already without sign */
		return -1;
	}
	*p = (unsigned char)'0';
	return 1;
#else
	if (*p >= (unsigned char)'p' && *p <= (unsigned char)'y') {
		*p &= ~64U;
		return -1;
	}
	if (*p >= (unsigned char)'0' && *p <= (unsigned char)'9') {
		/* already without sign */
		return -1;
	}
	*p = (unsigned char)'0';
	return 1;
#endif
}

static void
cob_put_sign_ascii (unsigned char *p)
{
#ifdef	COB_EBCDIC_MACHINE
	switch (*p) {
	case '0':
		*p = (unsigned char)'p';
		return;
	case '1':
		*p = (unsigned char)'q';
		return;
	case '2':
		*p = (unsigned char)'r';
		return;
	case '3':
		*p = (unsigned char)'s';
		return;
	case '4':
		*p = (unsigned char)'t';
		return;
	case '5':
		*p = (unsigned char)'u';
		return;
	case '6':
		*p = (unsigned char)'v';
		return;
	case '7':
		*p = (unsigned char)'w';
		return;
	case '8':
		*p = (unsigned char)'x';
		return;
	case '9':
		*p = (unsigned char)'y';
		return;
	case 'p':
	case 'q':
	case 'r':
	case 's':
	case 't':
	case 'u':
	case 'v':
	case 'w':
	case 'x':
	case 'y':
		/* already signed */
		return;
	default:
		*p = (unsigned char)'0';
	}
#else
	*p |= 64U;
#endif
}

/* EBCDIC Sign
 * positive: {ABCDEFGHI
 * negative: }JKLMNOPQR
 */

static int
cob_get_sign_ebcdic (unsigned char *p)
{
	switch (*p) {
	case '{':
		*p = (unsigned char)'0';
		return 1;
	case 'A':
		*p = (unsigned char)'1';
		return 1;
	case 'B':
		*p = (unsigned char)'2';
		return 1;
	case 'C':
		*p = (unsigned char)'3';
		return 1;
	case 'D':
		*p = (unsigned char)'4';
		return 1;
	case 'E':
		*p = (unsigned char)'5';
		return 1;
	case 'F':
		*p = (unsigned char)'6';
		return 1;
	case 'G':
		*p = (unsigned char)'7';
		return 1;
	case 'H':
		*p = (unsigned char)'8';
		return 1;
	case 'I':
		*p = (unsigned char)'9';
		return 1;
	case '}':
		*p = (unsigned char)'0';
		return -1;
	case 'J':
		*p = (unsigned char)'1';
		return -1;
	case 'K':
		*p = (unsigned char)'2';
		return -1;
	case 'L':
		*p = (unsigned char)'3';
		return -1;
	case 'M':
		*p = (unsigned char)'4';
		return -1;
	case 'N':
		*p = (unsigned char)'5';
		return -1;
	case 'O':
		*p = (unsigned char)'6';
		return -1;
	case 'P':
		*p = (unsigned char)'7';
		return -1;
	case 'Q':
		*p = (unsigned char)'8';
		return -1;
	case 'R':
		*p = (unsigned char)'9';
		return -1;
	default:
		/* What to do here */
		*p = (unsigned char)('0' + (*p & 0x0F));
		if (*p > (unsigned char)'9') {
			*p = (unsigned char)'0';
		}
		return 1;
	}
}

static void
cob_put_sign_ebcdic (unsigned char *p, const int sign)
{
	if (sign < 0) {
		switch (*p) {
		case '0':
			*p = (unsigned char)'}';
			return;
		case '1':
			*p = (unsigned char)'J';
			return;
		case '2':
			*p = (unsigned char)'K';
			return;
		case '3':
			*p = (unsigned char)'L';
			return;
		case '4':
			*p = (unsigned char)'M';
			return;
		case '5':
			*p = (unsigned char)'N';
			return;
		case '6':
			*p = (unsigned char)'O';
			return;
		case '7':
			*p = (unsigned char)'P';
			return;
		case '8':
			*p = (unsigned char)'Q';
			return;
		case '9':
			*p = (unsigned char)'R';
			return;
		case '}':
		case 'J':
		case 'K':
		case 'L':
		case 'M':
		case 'N':
		case 'O':
		case 'P':
		case 'Q':
		case 'R':
			/* already signed */
			return;
		default:
			/* What to do here */
			*p = (unsigned char)'}';
			return;
		}
	}

	switch (*p) {
	case '0':
		*p = (unsigned char)'{';
		return;
	case '1':
		*p = (unsigned char)'A';
		return;
	case '2':
		*p = (unsigned char)'B';
		return;
	case '3':
		*p = (unsigned char)'C';
		return;
	case '4':
		*p = (unsigned char)'D';
		return;
	case '5':
		*p = (unsigned char)'E';
		return;
	case '6':
		*p = (unsigned char)'F';
		return;
	case '7':
		*p = (unsigned char)'G';
		return;
	case '8':
		*p = (unsigned char)'H';
		return;
	case '9':
		*p = (unsigned char)'I';
		return;
	case '{':
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'F':
	case 'G':
	case 'H':
	case 'I':
		/* already signed */
		return;
	default:
		/* What to do here */
		*p = (unsigned char)'{';
		return;
	}
}

/* compare up to 'size' characters from buffer 'p'
   against a single character 'c',
   optionally using collation 'col' */
static int
common_cmpc (const unsigned char *p, const unsigned int c,
	     const size_t size, const unsigned char *col)
{
	register const unsigned char *end = p + size;
	int			ret;

	if (col) {
		const unsigned char c_col = col[c];
		while (p < end) {
			if ((ret = col[*p] - c_col) != 0) {
				return ret;
			}
			p++;
		}
	} else {
		while (p < end) {
			if ((ret = *p - c) != 0) {
				return ret;
			}
			p++;
		}
	}
	return 0;
}

static int
common_cmps (const unsigned char *s1, const unsigned char *s2,
	     const size_t size, const unsigned char *col)
{
	register const unsigned char *end = s1 + size;
	int			ret;

	if (col) {
		while (s1 < end) {
			if ((ret = col[*s1] - col[*s2]) != 0) {
				return ret;
			}
			s1++, s2++;
		}
	} else {
		while (s1 < end) {
			if ((ret = *s1 - *s2) != 0) {
				return ret;
			}
			s1++, s2++;
		}
	}
	return 0;
}

static int
cob_cmp_all (cob_field *f1, cob_field *f2)
{
	const unsigned char	*s = COB_MODULE_PTR->collating_sequence;
	unsigned char		*data;
	unsigned char		buff[COB_MAX_DIGITS + 1];

	if (COB_FIELD_HAVE_SIGN (f1)) {
		/* drop sign for comparision, using a copy to not change
		   the field during comparision */
		/* CHECKME: What should be returned if f1 is negative? */
		unsigned char *real_data = f1->data;
		f1->data = data = buff;
		memcpy (buff, real_data, f1->size);
		(void)cob_real_get_sign (f1);
		f1->data = real_data;
	} else {
		data = f1->data;
	}

	/* check for IF VAR = ALL "9" */
	if (f2->size == 1) {
		return common_cmpc (data, f2->data[0], f1->size, s);
	}

	/* check for IF VAR = ALL "AB" ... */
	{
		size_t		size = f1->size;
		int			ret;

		while (size >= f2->size) {
			if ((ret = common_cmps (data, f2->data, f2->size, s)) != 0) {
				return ret;
			}
			size -= f2->size;
			data += f2->size;
		}
		if (size > 0) {
			return common_cmps (data, f2->data, size, s);
		}
	}

	return 0;
}

static int
cob_cmp_alnum (cob_field *f1, cob_field *f2)
{
	const unsigned char	*s = COB_MODULE_PTR->collating_sequence;
	const size_t	min = (f1->size < f2->size) ? f1->size : f2->size;
	int		ret;

	/* Compare common substring */
	if ((ret = common_cmps (f1->data, f2->data, min, s)) != 0) {
		return ret;
	}

	/* Compare the rest (if any) with spaces */
	if (f1->size > f2->size) {
		return common_cmpc (f1->data + min, ' ', f1->size - min, s);
	} else if (f1->size < f2->size) {
		return -common_cmpc (f2->data + min, ' ', f2->size - min, s);
	}

	return 0;
}

static int
sort_compare (const void *data1, const void *data2)
{
	size_t		i;
	int		res;
	cob_field	f1;
	cob_field	f2;

	for (i = 0; i < sort_nkeys; ++i) {
		f1 = f2 = *sort_keys[i].field;
		f1.data = (unsigned char *)data1 + sort_keys[i].offset;
		f2.data = (unsigned char *)data2 + sort_keys[i].offset;
		if (COB_FIELD_IS_NUMERIC (&f1)) {
			res = cob_numeric_cmp (&f1, &f2);
		} else {
			res = common_cmps (f1.data, f2.data, f1.size, sort_collate);
		}
		if (res != 0) {
			return (sort_keys[i].tf_ascending == COB_ASCENDING) ? res : -res;
		}
	}
	return 0;
}

static void
cob_move_intermediate (cob_field *dst, const void *src, const size_t size)
{
	cob_field	intermediate;
	intermediate.size = size;
	intermediate.data = (cob_u8_ptr)src;
	/* note: if the target is numeric then cob_move will convert
	         on the fly to numeric as necessary */
	intermediate.attr = &const_alpha_attr;
	cob_move (&intermediate, dst);
}

/* open file using mode according to cob_unix_lf and
   filename (append when starting with +) */
static FILE *
cob_open_logfile (const char *filename)
{
	const char *mode;

	if (!cobsetptr->cob_unix_lf) {
		if (*filename == '+') {
			filename++;
			mode = "a";
		} else {
			mode = "w";
		}
	} else {
		if (*filename == '+') {
			filename++;
			mode = "ab";
		} else {
			mode = "wb";
		}
	}
	return fopen (filename, mode);
}

/* ensure that cob_trace_file is available for writing */
void
cob_check_trace_file (void)
{
	if (cobsetptr->cob_trace_file) {
		return;
	}
	if (cobsetptr->cob_trace_filename) {
		cobsetptr->cob_trace_file = cob_open_logfile (cobsetptr->cob_trace_filename);
		if (!cobsetptr->cob_trace_file) {
			/* could not open the file
			   unset the filename for not referencing it later */
			cobsetptr->cob_trace_filename = NULL;
			cobsetptr->cob_trace_file = stderr;
		}
	} else {
		cobsetptr->cob_trace_file = stderr;
	}
}

/* close current trace file (if open) and open/attach a new one */
static void
cob_new_trace_file (void)
{
	FILE *old_trace_file = cobsetptr->cob_trace_file;

	if (!cobsetptr->cob_trace_file
	 || cobsetptr->external_trace_file
	 || cobsetptr->cob_trace_file == stderr) {
		cobsetptr->cob_trace_file = NULL;
		cob_check_trace_file ();
		return;
	}

	fclose (cobsetptr->cob_trace_file);
	cobsetptr->cob_trace_file = NULL;

	cob_check_trace_file ();
	if (cobsetptr->cob_display_print_file
	 && cobsetptr->cob_display_print_file == old_trace_file) {
		cobsetptr->cob_display_print_file = cobsetptr->cob_trace_file;
	}
	if (cobsetptr->cob_dump_file
	 && cobsetptr->cob_dump_file == old_trace_file) {
		cobsetptr->cob_dump_file = cobsetptr->cob_trace_file;
	}
#ifdef COB_DEBUG_LOG
	if (cob_debug_file
	 && cob_debug_file == old_trace_file) {
		cob_debug_file = cobsetptr->cob_trace_file;
	}
#endif
}

int
cob_check_env_true (char * s)
{
	if (s) {
		if (strlen (s) == 1 && (*s == 'Y' || *s == 'y' || *s == '1')) return 1;
		if (strcasecmp (s, "YES") == 0 || strcasecmp (s, "ON") == 0 ||
			strcasecmp (s, "TRUE") == 0) {
			return 1;
		}
	}
	return 0;
}

int
cob_check_env_false (char * s)
{
	return s && ((strlen (s) == 1 && (*s == 'N' || *s == 'n' || *s == '0'))
				|| (strcasecmp (s, "NO") == 0 || strcasecmp (s, "NONE") == 0
				|| strcasecmp (s, "OFF") == 0 || strcasecmp (s, "FALSE") == 0));
}

static char file_path_name [COB_FILE_BUFF] = "";
static int	exec_len = 0, build_len = 0, exec_chg = 0;
static char *exec_cfg_dir = NULL;
static char	exec64 [8] = "";
static char	exec_root [128] = COB_BLD_PREFIX;
static char	build_root [128] = COB_BLD_PREFIX;

#if defined	(HAVE_UNISTD_H) && !(defined (_WIN32)) 
#define dMaxGids 32

static int 
is_group_member (gid_t gid)
{
	int bRet = 0;

	if (getegid() == gid
	 || getgid() == gid) {
		bRet = 1;

	} else {
		gid_t grouplist[dMaxGids];
		int nGroups, k;

		for(k=0; k < dMaxGids; k++) grouplist[k] = 0;
		nGroups = getgroups(dMaxGids, grouplist);
		for(k=0; k < dMaxGids && k < nGroups; k++) {
			if (grouplist[k] == gid) {
				bRet = 1;
				break;
			}
		}
	}

	return bRet;
}

#ifdef S_IXUSR
#define PERM_UX S_IXUSR
#else
#define PERM_UX	00100
#endif
#ifdef S_IXGRP
#define PERM_GX S_IXGRP
#else
#define PERM_GX	00010
#endif
#ifdef S_IXOTH
#define PERM_OX S_IXOTH
#else
#define PERM_OX	00001
#endif

static int usr_exc = PERM_UX;
static int grp_exc = PERM_GX;
static int oth_exc = PERM_OX;

/* check if executable, returns 0 if it is */
static int 
is_executable_stat (struct stat *pStat)
{
	if (S_ISDIR(pStat->st_mode)) {
		return -1;
	}
	if (S_ISREG(pStat->st_mode)) {
		if ((pStat->st_mode & oth_exc) == oth_exc)
			return 0;
		if (geteuid() == (uid_t)0) {		/* Effectively 'root' */
			if ((pStat->st_mode & (PERM_UX|PERM_GX|PERM_OX)) != 0)
				return 0;
		}
		if (geteuid() == pStat->st_uid
		 || getuid()  == pStat->st_uid) {
			if ((pStat->st_mode & usr_exc) == usr_exc)
				return 0;
		} else if (is_group_member (pStat->st_gid)) {
			if ((pStat->st_mode & grp_exc) == grp_exc)
				return 0;
		}
	}
	return -1;
}

#else
/* check if executable, returns 0 if it is */
static COB_INLINE int 
is_executable_stat (struct stat *pStat)	/* Windows or NO unistd.h */
{
	return 0;	/* assume "yes" */
}
#endif

/* check if existing and executable, returns 0 if it is */
static COB_INLINE int
is_executable (const char *path)
{
	struct stat statbuf;
	int status = stat (path, &statbuf);

	if (status != -1) {
		status = is_executable_stat (&statbuf);
	}
	return status;
}

static char *
cob_find_path_internal (const char *progname, size_t prog_len)
{
	errno = 0;

	/* Complete path already given ? */
	if (progname[0] == SLASH_CHAR
#if defined (_WIN32) || defined (__DJGPP__)
	 || (progname[0]
	  && progname[1] == ':'
	  && progname[2] == SLASH_CHAR)
#endif
	) {
		if (is_executable (progname) == 0) {
			strcpy (file_path_name, progname);
			return file_path_name;
		}
		errno = ENOENT;
		return NULL;
	}

	/* relative invocation - prefix current path */
	if (progname[0] == '.') {
		size_t path_len;
		char *p = getcwd (file_path_name, COB_FILE_MAX - 1 - prog_len);
		if (p == NULL) return NULL;	/* unlikely overflow */
		path_len = strlen (p);
		if (progname[1] == SLASH_CHAR) {
			progname++;	/* start copying with slash */
		} else
		if (progname[1] == '.'
		 && progname[2] == SLASH_CHAR) {
			file_path_name[++path_len] = SLASH_CHAR;	/* add slash, start with copying with .. */
		} else {
			/* invalid path, we should never get here */
			errno = EINVAL;
			return NULL;
		}
		memcpy (file_path_name + path_len, progname, prog_len + 1);	/* add including trailing nul */
		if (is_executable (file_path_name) == 0) {
			return file_path_name;
		}
		errno = ENOENT;
	}

	return NULL;
}

/* Return full path name to progam (pointing to a static buffer) or NULL,
   checks for existing and, if possible, executable file */
static char *
cob_find_path ( 
	const char *progname, 			/* program/library to search for */
	const char *path)				/* PATH to search or NULL for env PATH */
{
	size_t prog_len;
	char *p;
	
	if (progname == NULL) {
		errno = EFAULT;
		return NULL;
	}

	/* check if  relative / full path */
	prog_len = strlen (progname);
	p = cob_find_path_internal (progname, prog_len);
	if (p || errno) {
		return p;
	}

#if 1  /* CHECKME: shouldn't this be only done in _WIN32 where PATH is
          explicit prefixed by "."? We'd find it below _if_ "." is in PATH */
	/* check for file in current working directory */
	{
		size_t path_len;
		p = getcwd (file_path_name, COB_FILE_MAX - 1 - prog_len);
		if (p == NULL) return NULL;	/* unlikely overflow */
		path_len = strlen (p);
		file_path_name[path_len++] = SLASH_CHAR;
		memcpy (file_path_name + path_len, progname, prog_len + 1);	/* add including trailing nul */
		if (is_executable (file_path_name) == 0) {
			return file_path_name;
		}
	}
#endif

	if (path == NULL) {
		path = getenv ("PATH");
		if (path == NULL) return NULL;	/* cater for broken PATH */
	}

	/* check with all entries of given (or computed) path entries prefixed */
	while ((p = strchr (path, PATHSEP_CHAR))) {
		if (p == path) {
			path++;	/* skip consecutive separators */
		} else {
			const size_t path_len = p - path;
			char  str_work [COB_FILE_BUFF];
			char  *s = str_work + path_len - 1;
			memcpy (str_work, path, path_len);
			if (*s != SLASH_CHAR) {
				s++; *s = SLASH_CHAR;
			}
			memcpy (s + 1, progname, prog_len + 1);	/* add including trailing nul */
			s = cob_find_path_internal (str_work, path_len + 1 + prog_len);
			if (s) {
				return s;	/* pointing to file_path_name */
			}
			path = p + 1;
		}
	}
	return NULL;
}

/* Determine if GnuCOBOL is installed in different location than built for */
void
cob_setup_env (const char *progname)
{
	char	*p;
	build_len = strlen (build_root);
	if (build_len > 0
	 && build_root[build_len-1] != SLASH_CHAR) {
		strcat (build_root, SLASH_STR);
		build_len++;
	}
	if ((p = getenv ("COB_DIR")) != NULL) {
		exec_len = sprintf (exec_root, "%s", p);
	} else {
		char *binpath = NULL;
		char	libcobnm[32];
#if !defined (_WIN32) && !defined(__CYGWIN__) && !defined(__DJGPP__)
		char *libpath = getenv("LD_LIBRARY_PATH");
		if (libpath == NULL)
			libpath = getenv("LIBPATH");
		if (libpath) {
			sprintf(libcobnm,"libcob." COB_MODULE_EXT ".%d",COB_LIBCOB_VER);
			binpath = cob_find_path (libcobnm, libpath);
			if (binpath == NULL) {	/* Try without the version */
				binpath = cob_find_path ("libcob." COB_MODULE_EXT, libpath);
			}
			if (binpath == NULL)	/* Try .so (some systems have .so and/or .sl) */
				binpath = cob_find_path ("libcob.so", libpath);
			if (binpath == NULL)	/* Try .sl */
				binpath = cob_find_path ("libcob.sl", libpath);
			if (binpath == NULL)	/* AIX may just have .a which could contain the .so */
				binpath = cob_find_path ("libcob.a", libpath);
		}
#else
		sprintf(libcobnm,"libcob-%d.dll",COB_LIBCOB_VER);
		binpath = cob_find_path (libcobnm, NULL);
		if (binpath == NULL) {
			binpath = cob_find_path ("libcob." COB_MODULE_EXT, NULL);
		}
#endif
		if (binpath == NULL) {	/* Search for argv[0] */
			binpath = cob_find_path (progname, NULL);
		}
		if (binpath == NULL) {
			exec_chg = 0;
			return;
		}
		/* /path/to/lib/libcob-5.so; positioning to /libcob-5.so */
		p = strrchr (binpath, SLASH_CHAR);
		if (p) {
			*p = 0;
			p = strrchr (binpath, SLASH_CHAR);
			if (memcmp (p+1, "bin", 3) == 0
			 || memcmp (p+1, "lib", 3) == 0) {
				if (memcmp (p+4, "64", 2) == 0)
					strcpy (exec64,"64");
				/* more checks, mostly for Win32 distributions */
				else if (memcmp (p+4, "_x64", 4) == 0)
					strcpy (exec64,"_x64");
				else if (memcmp (p + 4, "_x86", 4) == 0)
					strcpy (exec64, "_x86");
				else if (memcmp (p + 4, "_arm64", 6) == 0)
					strcpy (exec64, "_arm64");
				*p = 0;
			}
		}
		p = binpath;
	}

	exec_len = sprintf (exec_root, "%s", p);
	if (exec_len > 0 && exec_root[exec_len-1] != SLASH_CHAR) {
		strcat(exec_root, SLASH_STR);
		exec_len++;
	}
	if (strcmp (build_root, exec_root) == 0)
		exec_chg = 0;
	else
		exec_chg = 1;
}

/* If not in environment, relocate to installed location */
char *
cob_getenv_value (const char *ename)
{
	static char getenv_work [512];
	char	*penv;
	if ((penv = getenv (ename)) != NULL)
		return penv;

	if (strcmp (ename, "COB_CONFIG_DIR") == 0) {
		if (exec_cfg_dir)
			return exec_cfg_dir;
		penv = (char *)COB_CONFIG_DIR;
	} else if (strcmp (ename, "COB_COPY_DIR") == 0) {
		penv = (char *)COB_COPY_DIR;
	} else if (strcmp (ename, "COB_SCHEMA_DIR") == 0) {
		penv = (char *)COB_SCHEMA_DIR;
#ifdef LOCALEDIR
	} else if (strcmp (ename, "LOCALEDIR") == 0) {
		if (exec_chg) {
			sprintf (getenv_work, "%sshare/locale",exec_root);
			return getenv_work;
		}
		return (char *)LOCALEDIR;
#endif
	}
	if (!exec_chg
	 || penv == NULL)
		return penv;
	if (build_len > 0) {
		if (memcmp (penv,build_root,build_len) == 0) {
			sprintf (getenv_work, "%s%s", exec_root, penv + build_len);
			if (strcmp (ename, "COB_CONFIG_DIR") == 0)
				return exec_cfg_dir = cob_strdup (getenv_work);
			return cob_strdup (getenv_work);
		}
		return penv;
	}
	if (*penv == SLASH_CHAR)
		penv++;
	sprintf (getenv_work, "%s%s", exec_root, penv);
	if (strcmp (ename, "COB_CONFIG_DIR") == 0)
		return exec_cfg_dir = cob_strdup (getenv_work);
	return cob_strdup (getenv_work);
}

/* FIXME: this does not relocate the path but prefix
   necessary include / library directories;
   check for moving it to cobc and renaming the function
*/

/* Scan string inserting either -I or -L
   with relocated directory where used;
   may return the position to a static buffer */
const char *
cob_relocate_string (const char *str)
{
	static char str_work [COB_FILE_BUFF];

	const size_t len = strlen (str);
	const char *do_I, *do_L;

#if defined (_MSC_VER)
	#define IOPT	"/I"
	#define LOPT	"/LIBPATH:"
#elif defined (__WATCOMC__)
	#define IOPT	"-i"
	#define LOPT	"-L"
#else
	#define IOPT	"-I"
	#define LOPT	"-L"
#endif

	if (!exec_chg || len < 3)
		return str;

#ifndef _MSC_VER
	if (memcmp (str, IOPT, 2) == 0) {
		do_I = str;
	} else {
		do_I = strstr (str, " "IOPT);
	}
	if (memcmp (str, LOPT, 2) == 0) {
		do_L = str;
	} else {
		do_L = strstr (str, " "LOPT);
	}
#else
	if ((str[0] == '/'  || str[0] == '-')
	 && (str[1] == 'I'  || str[1] == 'i')
	 && (str[2] == ' ' || str[2] == '"')) {
		do_I = str;
	} else {
		do_I = cob_str_case_str (str, " /I ");
		do_L = cob_str_case_str (str, " /i ");
		if (do_L && (!do_I || do_L > do_I)) {
			do_I = do_L;
		}
		do_L = cob_str_case_str (str, " /I\"");
		if (do_L && (!do_I || do_L > do_I)) {
			do_I = do_L;
		}
		do_L = cob_str_case_str (str, " /i\"");
		if (do_L && (!do_I || do_L > do_I)) {
			do_I = do_L;
		}
	}
	do_L = cob_str_case_str(str, LOPT);
#endif
	/* nothing to change (or unexpectedly both) -> return original */
	if ((!do_I && !do_L)
	 || (do_I && do_L)) {
		return str;
	}

	if (do_I) {
		size_t i = do_I - str;
		if (i) {
			memcpy (str_work, str, i);
		}
		/* include dir should be enough here, too */
		snprintf (str_work + i, COB_FILE_MAX - i,
			" "IOPT"\"%s\" "IOPT"\"%sinclude\" %s",
			exec_root, exec_root, do_I);
	} else {
		size_t i = do_L - str;
		if (i) {
			memcpy (str_work, str, i);
		}
		snprintf (str_work + i, COB_FILE_MAX - i,
		         " "LOPT"\"%slib%s\" %s",
		         exec_root, exec64, do_L);
	}
	return (const char *)str_work;
#undef IOPT
#undef LOPT
}

static void
cob_rescan_env_vals (void)
{
	int	i;
	int	j;
	int	old_type;
	char	*env;
	char	*save_source_file = (char *) cob_source_file;

	cob_source_file = NULL;
	cob_source_line = 0;

	/* Check for possible environment variables */
	for (i = 0; i < NUM_CONFIG; i++) {
		if (gc_conf[i].env_name
		 && (env = getenv (gc_conf[i].env_name)) != NULL
		 && *env != 0) {
			old_type = gc_conf[i].data_type;
			gc_conf[i].data_type |= STS_ENVSET;

			if (*env != '\0' && set_config_val (env, i)) {
				gc_conf[i].data_type = old_type;

				/* Remove invalid setting */
				(void)cob_unsetenv (gc_conf[i].env_name);
			} else if (gc_conf[i].env_group == GRP_HIDE) {
				/* Any alias present? */
				for (j = 0; j < NUM_CONFIG; j++) {
					if (j != i
					 && gc_conf[i].data_loc == gc_conf[j].data_loc) {
						gc_conf[j].data_type |= STS_ENVSET;
						gc_conf[j].set_by = i;
					}
				}
			}
		}
	}
	cob_source_file = save_source_file;

	/* Extended ACCEPT status returns */
	if (cobsetptr->cob_extended_status == 0) {
		cobsetptr->cob_use_esc = 0;
	}
}

static COB_INLINE int
one_indexed_day_of_week_from_monday (int zero_indexed_from_sunday)
{
	return ((zero_indexed_from_sunday + 6) % 7) + 1;
}

#if defined (_MSC_VER)
static void
set_cob_time_ns_from_filetime (const FILETIME filetime, struct cob_time *cb_time)
{
	ULONGLONG	filetime_int;

	filetime_int = (((ULONGLONG) filetime.dwHighDateTime) << 32)
		+ filetime.dwLowDateTime;
	/* FILETIMEs are accurate to 100 nanosecond intervals */
	cb_time->nanosecond = (filetime_int % (ULONGLONG) 10000000) * 100;
}
#endif

/* Global functions */

int 
cob_ncase_cmp (char *str1, const char *str2, unsigned len)	/* Like strncasecmp */
{
	if (len == 0)
		return(0);

	while(*str1 && *str2 && --len) {
		if (toupper(*str1) != toupper(*str2))
			break;
		str1++;
		str2++;
	}
	return(toupper(*str1)-toupper(*str2));
}

char *
cob_str_case_str (char *str1, const char *str2)					/* Like  strcasestr  */
{
	unsigned len;
	int ch1 = toupper(*str2);

	if(ch1 == 0)
		return NULL;
	len = (unsigned)strlen(str2);

	while(*str1) {
		if(toupper(*str1) != ch1) {
			str1++;
			continue;
		}
		if (cob_ncase_cmp(str1, str2, len) == 0)
			return str1;
		str1++;
	}

	return NULL;
}

/* get last exception (or 0 if not active) */
int
cob_get_last_exception_code (void)
{
	return last_exception_code;
}

/* get exception name for last raised exception */
const char *
cob_get_last_exception_name (void)
{
	size_t	n;

	/* direct match */
	for (n = 1; n < EXCEPTION_TAB_SIZE; ++n) {
		if (last_exception_code == cob_exception_tab_code[n]) {
			return cob_exception_tab_name[n];
		}
	}
	/* any match (last to first to get most specific one,
	   checking missing/not supported first) */
	if (cob_last_exception_is (COB_EC_IMP_FEATURE_MISSING)) {
		return cob_exception_tab_name[COB_EC_IMP_FEATURE_MISSING];
	}
	if (cob_last_exception_is (COB_EC_IMP_FEATURE_DISABLED)) {
		return cob_exception_tab_name[COB_EC_IMP_FEATURE_DISABLED];
	}
	for (n = EXCEPTION_TAB_SIZE - 1; n != 0; --n) {
		if ((last_exception_code & cob_exception_tab_code[n])
			== cob_exception_tab_code[n]) {
			return cob_exception_tab_name[n];
		}
	}
	return NULL;
}

/* check if last exception is set and includes the given exception */
int
cob_last_exception_is (const int exception_to_check)
{
	if ((last_exception_code & cob_exception_tab_code[exception_to_check])
	 == cob_exception_tab_code[exception_to_check]) {
		return 1;
	} else {
		return 0;
	}
}

/* set last exception,
   used for EXCEPTION- functions and for cob_accept_exception_status,
   only reset on SET LAST EXCEPTION TO OFF */
void
cob_set_exception (const int id)
{
	cobglobptr->cob_exception_code = cob_exception_tab_code[id];
	last_exception_code = cobglobptr->cob_exception_code;

	cobglobptr->last_exception_statement = STMT_UNKNOWN;
	cobglobptr->last_exception_id = NULL;
	cobglobptr->last_exception_section = NULL;
	cobglobptr->last_exception_paragraph = NULL;

	if (id) {
		static char excp_mod[COB_MAX_WORDLEN + 1];
		static char excp_sec[COB_MAX_WORDLEN + 1];
		static char excp_para[COB_MAX_WORDLEN + 1];
		cob_module	*mod = COB_MODULE_PTR;
		cobglobptr->cob_got_exception = 1;
#if 0	/* consider addition for 4.x */
		cobglobptr->last_exception_line = cob_source_line;
#endif
		if (mod) {
#if 0	/* consider addition for 4.x */
			if (mod->module_sources
			 && mod->module_stmt != 0) {
				/* note: it is likely best to not copy name + line but store the name+line
				   together, maybe even comma-separated when we have copy->copy-prog */
				cobglobptr->last_exception_source = cob_strdup (mod->module_sources
					[COB_GET_FILE_NUM (mod->module_stmt)]);
				cobglobptr->last_exception_line = COB_GET_LINE_NUM (mod->module_stmt);
			} else {
				if (cob_source_file != NULL) {
					cobglobptr->last_exception_source = cob_strdup (cob_source_file);
				}
#else
			if (mod->module_stmt != 0) {
				cobglobptr->last_exception_line = COB_GET_LINE_NUM (mod->module_stmt);
#endif
			}
			cobglobptr->last_exception_statement = mod->statement;
			if (mod->module_name) {
				strcpy (excp_mod, mod->module_name);
				cobglobptr->last_exception_id = excp_mod;
			}
			if (mod->section_name) {
				strcpy (excp_sec, mod->section_name);
				cobglobptr->last_exception_section = excp_sec;
			}
			if (mod->paragraph_name) {
				strcpy (excp_para, mod->paragraph_name);
				cobglobptr->last_exception_paragraph = excp_para;
			}
			return;
#if 0	/* consider addition for 4.x */
		} else {
       			if (cob_source_file != NULL) {
				cobglobptr->last_exception_source = cob_strdup (cob_source_file);
			}
#endif
		}
		/* if no current module is available fall-through */
	} else {
		cobglobptr->cob_got_exception = 0;
#if 0	/* consider addition for 4.x */
		if (cobglobptr->last_exception_source) {
			cob_free ((void *)cobglobptr->last_exception_source);
			cobglobptr->last_exception_source = NULL;
		}
#endif
		cobglobptr->last_exception_line = 0;
	}
}

/* add to last exception, set if empty */
void
cob_add_exception (const int id)
{
	if (!cobglobptr->cob_exception_code) {
		cob_set_exception (id);
		return;
	}
	cobglobptr->cob_exception_code |= cob_exception_tab_code[id];
	last_exception_code |= cob_exception_tab_code[id];
}

/* return the last exception value */
void
cob_accept_exception_status (cob_field *f)
{
	int exception = last_exception_code;
	/* Note: MF set this to a 9(3) item, we do a translation here (only works for USAGE DISPLAY!);
	   MF: intended for CALL only, 0=ok, 1=ENOMEM, 2=module not found, 128 other CALL failure,
	   "unpredictable if the last statement was not a CALL, especially: adjusted by fileio" */
	if (exception
	 && f->size == 3	/* FIXME: current code works only for DISPLAY, adjust to work for other usages */
	 && COB_FIELD_TYPE (f) == COB_TYPE_NUMERIC_DISPLAY) {
		if (exception == cob_exception_tab_code[COB_EC_PROGRAM_RESOURCES]) {
			exception = 1;
		} else if (exception == cob_exception_tab_code[COB_EC_PROGRAM_NOT_FOUND]) {
			exception = 2;
		} else if (exception || cob_exception_tab_code[COB_EC_PROGRAM]) {
			exception = 128;
		}
	}
	cob_set_int (f, exception);
}

void
cob_accept_user_name (cob_field *f)
{
	if (cobsetptr->cob_user_name) {
		cob_move_intermediate (f, cobsetptr->cob_user_name,
			    strlen (cobsetptr->cob_user_name));
	} else {
		cob_move_intermediate (f, " ", (size_t)1);
	}
}

void *
cob_malloc (const size_t size)
{
	void	*mptr;

	mptr = calloc ((size_t)1, size);
	/* LCOV_EXCL_START */
	if (!mptr) {
		cob_fatal_error (COB_FERROR_MEMORY);
	}
	/* LCOV_EXCL_STOP */
	return mptr;
}

void *
cob_realloc (void * optr, const size_t osize, const size_t nsize)
{
	void	*mptr;

	/* LCOV_EXCL_START */
	if (!optr) {
		cob_fatal_error (COB_FERROR_FREE);
	}
	/* LCOV_EXCL_STOP */

	if (osize == nsize) {	/* No size change */
		return optr;
	} 
	if (osize > nsize) {		/* Reducing size */
		return realloc (optr, nsize);
	}

	mptr = calloc ((size_t)1, nsize);	/* New memory, past old is cleared */
	/* LCOV_EXCL_START */
	if (!mptr) {
		cob_fatal_error (COB_FERROR_MEMORY);
	}
	/* LCOV_EXCL_STOP */
	memcpy (mptr, optr, osize);
	cob_free (optr);
	return mptr;
}

void
cob_free (void * mptr)
{
#ifdef _DEBUG
	/* LCOV_EXCL_START */
	if (!mptr) {
		cob_fatal_error (COB_FERROR_FREE);
	}
	/* LCOV_EXCL_STOP */
#endif
	free (mptr);

}

void *
cob_fast_malloc (const size_t size)
{
	void	*mptr;

	mptr = malloc (size);
	/* LCOV_EXCL_START */
	if (!mptr) {
		cob_fatal_error (COB_FERROR_MEMORY);
	}
	/* LCOV_EXCL_STOP */
	return mptr;
}

char *
cob_strdup (const char *p)
{
	char	*mptr;
	size_t	len;

	len = strlen (p) + 1;
	mptr = (char *)cob_fast_malloc (len);
	memcpy (mptr, p, len);
	return mptr;
}

char *
cob_strndup (const char *p, const size_t max_len)
{
	char	*mptr;
	size_t	len;

	len = strlen (p);
	if (len > max_len) {
		len = max_len;
	}
	mptr = (char *)cob_fast_malloc (len + 1);
	memcpy (mptr, p, len);
	*(mptr + len) = 0;
	return mptr;
}

/* Caching versions of malloc/free */
void *
cob_cache_malloc (const size_t size)
{
	struct cob_alloc_cache	*cache_ptr;
	void			*mptr;

	cache_ptr = cob_malloc (sizeof (struct cob_alloc_cache));
	mptr = cob_malloc (size);
	cache_ptr->cob_pointer = mptr;
	cache_ptr->size = size;
	cache_ptr->next = cob_alloc_base;
	cob_alloc_base = cache_ptr;
	return mptr;
}

void *
cob_cache_realloc (void *ptr, const size_t size)
{
	struct cob_alloc_cache	*cache_ptr;
	void			*mptr;

	if (!ptr) {
		return cob_cache_malloc (size);
	}
	cache_ptr = cob_alloc_base;
	for (; cache_ptr; cache_ptr = cache_ptr->next) {
		if (ptr == cache_ptr->cob_pointer) {
			if (size <= cache_ptr->size) {
				return ptr;
			}
			mptr = cob_malloc (size);
			memcpy (mptr, cache_ptr->cob_pointer, cache_ptr->size);
			cob_free (cache_ptr->cob_pointer);
			cache_ptr->cob_pointer = mptr;
			cache_ptr->size = size;
			return mptr;
		}
	}
	return ptr;
}

void
cob_cache_free (void *ptr)
{
	struct cob_alloc_cache	*cache_ptr;
	struct cob_alloc_cache	*prev_ptr;

	if (!ptr) {
		return;
	}
	cache_ptr = cob_alloc_base;
	prev_ptr = cob_alloc_base;
	for (; cache_ptr; cache_ptr = cache_ptr->next) {
		if (ptr == cache_ptr->cob_pointer) {
			cob_free (cache_ptr->cob_pointer);
			if (cache_ptr == cob_alloc_base) {
				cob_alloc_base = cache_ptr->next;
			} else {
				prev_ptr->next = cache_ptr->next;
			}
			cob_free (cache_ptr);
			return;
		}
		prev_ptr = cache_ptr;
	}
}

/* routines for handling 'trace' follow */

/* Note: these functions are only called if the following vars are set:
         COB_MODULE_PTR + ->module_stmt + ->module_sources
*/
static int
cob_trace_prep (void)
{
	const char	*s;
	if (!cobsetptr->cob_trace_file) {
		cob_check_trace_file ();
		if (!cobsetptr->cob_trace_file)
			return 1; 	/* silence warnings */
	}
	cob_get_source_line ();
	if (cob_source_file
	 && (!cob_last_sfile || strcmp (cob_last_sfile, cob_source_file))) {
		if (cob_last_sfile) {
			cob_free ((void *)cob_last_sfile);
		}
		cob_last_sfile = cob_strdup (cob_source_file);
		fprintf (cobsetptr->cob_trace_file, "Source: '%s'\n", cob_source_file);
	}
	if (COB_MODULE_PTR->module_name) {
		s = COB_MODULE_PTR->module_name;
	} else {
		s = _("unknown");
	}
	if (!cob_last_progid
	 || strcmp (cob_last_progid, s)) {
		cob_last_progid = s;
		if (COB_MODULE_PTR->module_type == COB_MODULE_TYPE_FUNCTION) {
			fprintf (cobsetptr->cob_trace_file, "Function-Id: %s\n", cob_last_progid);
		} else {
			fprintf (cobsetptr->cob_trace_file, "Program-Id:  %s\n", cob_last_progid);
		}
	}
	return 0;
}

static void
cob_trace_print (char *val)
{
	int	i;
	int last_pos = (int)(strlen (cobsetptr->cob_trace_format) - 1);

	/* note: only executed after cob_trace_prep (), so
	         call to cob_get_source_line () already done */
	for (i=0; cobsetptr->cob_trace_format[i] != 0; i++) {
		if (cobsetptr->cob_trace_format[i] == '%') {
			i++;
			switch (cobsetptr->cob_trace_format[i]) {
			case 'P':
			case 'p':
				if (COB_MODULE_PTR && COB_MODULE_PTR->module_type == COB_MODULE_TYPE_FUNCTION) {
					if (i != last_pos) {
						fprintf (cobsetptr->cob_trace_file, "Function-Id: %-16s", cob_last_progid);
					} else {
						fprintf (cobsetptr->cob_trace_file, "Function-Id: %s", cob_last_progid);
					}
				} else {
					if (i != last_pos) {
						fprintf (cobsetptr->cob_trace_file, "Program-Id:  %-16s", cob_last_progid);
					} else {
						fprintf (cobsetptr->cob_trace_file, "Program-Id:  %s", cob_last_progid);
					}
				}
				break;
			case 'I':
			case 'i':
				fprintf (cobsetptr->cob_trace_file, "%s", cob_last_progid);
				break;
			case 'L':
			case 'l':
				fprintf (cobsetptr->cob_trace_file, "%6u", cob_source_line);
				break;
			case 'S':
			case 's':
				if (i != last_pos) {
					fprintf (cobsetptr->cob_trace_file, "%-42.42s", val);
				} else {
					fprintf (cobsetptr->cob_trace_file, "%s", val);
				}
				break;
			case 'F':
			case 'f':
				if (i != last_pos) {
					fprintf (cobsetptr->cob_trace_file, "Source: %-*.*s",
						-COB_MAX_NAMELEN, COB_MAX_NAMELEN, cob_last_sfile);
				} else {
					fprintf (cobsetptr->cob_trace_file, "Source: %s", cob_last_sfile);
				}
				break;
			default:
				fputc ('%', cobsetptr->cob_trace_file);
				fputc (cobsetptr->cob_trace_format[i], cobsetptr->cob_trace_file);
			}
		} else {
			fputc (cobsetptr->cob_trace_format[i], cobsetptr->cob_trace_file);
		}
	}
	fputc ('\n', cobsetptr->cob_trace_file);
	fflush (cobsetptr->cob_trace_file);
}

void
cob_trace_sect (const char *name)
{
	COB_MODULE_PTR->section_name = name;

	/* actual tracing, if activated */
	if (cobsetptr->cob_line_trace
	 && (COB_MODULE_PTR->flag_debug_trace & COB_MODULE_TRACE)) {
		char	val[60];
		if (cob_trace_prep ()
		 || name == NULL) {
			return;
		}
		snprintf (val, sizeof (val), "  Section: %s", name);
		cob_trace_print (val);
	}
}

void
cob_trace_para (const char *name)
{
	/* store for CHECKME */
	COB_MODULE_PTR->paragraph_name = name;

	/* actual tracing, if activated */
	if (cobsetptr->cob_line_trace
	 && (COB_MODULE_PTR->flag_debug_trace & COB_MODULE_TRACE)) {
		char	val[60];
		if (cob_trace_prep ()
		 || name == NULL) {
			return;
		}
		snprintf (val, sizeof (val), "Paragraph: %s", name);
		cob_trace_print (val);
	}
}

void
cob_trace_entry (const char *name)
{
	/* actual tracing, if activated */
	if (cobsetptr->cob_line_trace
	 && (COB_MODULE_PTR->flag_debug_trace & COB_MODULE_TRACE)) {
		char	val[60];
		if (cob_trace_prep ()
		 || name == NULL) {
			return;
		}
		snprintf (val, sizeof (val), "    Entry: %s", name);
		cob_trace_print (val);
	}
}

void
cob_trace_exit (const char *name)
{
	/* actual tracing, if activated */
	if (cobsetptr->cob_line_trace
	 && (COB_MODULE_PTR->flag_debug_trace & COB_MODULE_TRACE)) {
		char	val[60];
		if (cob_trace_prep ()
		 || name == NULL) {
			return;
		}
		snprintf (val, sizeof (val), "     Exit: %s", name);
		cob_trace_print (val);
	}
}

void
cob_trace_statement (const enum cob_statement stmt)
{
	/* actual tracing, if activated */
	if (cobsetptr->cob_line_trace
	 && (COB_MODULE_PTR->flag_debug_trace & COB_MODULE_TRACEALL)) {
		char	val[60];
		if (cob_trace_prep ()) {
			return;
		}
		snprintf (val, sizeof (val), "           %s",
			stmt != STMT_UNKNOWN ? cob_statement_name[stmt] : _("unknown"));
		cob_trace_print (val);
	}
}

void
cob_nop (void)
{
	/* this is only an empty function, a call to it may be inserted by cobc
	   to force some optimizations in the C compiler to not be triggered in
	   a quite portable way */
	;
}

void
cob_ready_trace (void)
{
	cob_module	*mod;
	int		k;
	const int	MAX_ITERS = 10240;

	cobsetptr->cob_line_trace = 1;
	if (!cobsetptr->cob_trace_file) {
		cob_check_trace_file ();
	}
	/* FIXME: this is overkill - it should only be set in the current program
	   and within the start code for each ENTRY be set */
	for (k = 0, mod = COB_MODULE_PTR; mod && k < MAX_ITERS; mod = mod->next, k++) {
		mod->flag_debug_trace |= COB_MODULE_READYTRACE;
	}
}

void
cob_reset_trace (void)
{
	cob_module	*mod;
	int		k;
	const int	MAX_ITERS = 10240;

	cobsetptr->cob_line_trace = 0;
	/* FIXME: with the change above only the current program and its calers need
	   to be reset here */
	for (k = 0, mod = COB_MODULE_PTR; mod && k < MAX_ITERS; mod = mod->next, k++) {
		mod->flag_debug_trace &= ~COB_MODULE_READYTRACE;
	}
}

unsigned char *
cob_get_pointer (const void *srcptr)
{
	void	*tmptr;

	memcpy (&tmptr, srcptr, sizeof (void *));
	return (cob_u8_ptr)tmptr;
}

/* stores the field's rtrimmed string content into the given buffer
   with maxlength */
void
cob_field_to_string (const cob_field *f, void *str, const size_t maxsize)
{
	register unsigned char	*end, *data, *s;

	if (f == NULL) {
		snprintf (str, maxsize, "%s", ("NULL field"));
		return;
	}

	if (f->size == 0) {
		return;
	}
	data = f->data;
	/* check if field has data assigned (may be a BASED / LINKAGE item) */
	if (data == NULL) {
		snprintf (str, maxsize, "%s", ("field with NULL address"));
		return;
	}
	end = data + f->size - 1;
	while (end > data) {
		if (*end != ' ' && *end) {
			break;
		}
		end--;
	}
	s = (unsigned char *)str;
	if (*end == ' ' || *end == 0) {
		*s = 0;
		return;
	}

	/* note: the specified max does not contain the low-value */
	if (end - data > maxsize) {
		end = data + maxsize;
	}
	while (data <= end) {
		*s++ = *data++;
	}
	*s = 0;
}

static void
call_exit_handlers_and_terminate (void)
{
	if (cobsetptr
	 && !in_stop_run) {
		if (cobsetptr->cob_stop_run_commit > 0) 
			cob_commit ();
		else
			cob_rollback ();
	}
	in_stop_run = 1;
	if (exit_hdlrs != NULL) {
		struct exit_handlerlist* h = exit_hdlrs;
		while (h != NULL) {
			h->proc ();
			h = h->next;
		}
	}
	cob_terminate_routines ();
}

void
cob_stop_run (const int status)
{
	if (!cob_initialized) {
		cob_hard_failure ();
	}
	call_exit_handlers_and_terminate ();
	exit_code = status;
#ifndef COB_WITHOUT_JMP
	if (return_jmp_buffer_set) {
		longjmp (return_jmp_buf, 1);
	}
#endif
	exit (status);
}

static int
handle_core_on_error ()
{
	unsigned int core_on_error = 0;
	if (cob_initialized) {
		core_on_error = cobsetptr->cob_core_on_error;
	} else {
		char *env_val = cob_getenv_direct ("COB_CORE_ON_ERROR");
		if (env_val && env_val[0] && env_val [1] == 0
		 && env_val[0] >= '0' && env_val[0] <= '3') {
			core_on_error = COB_D2I (env_val[0]);
		}
	}
	/* explicit create a coredump file */
	if (core_on_error == 3) {
		int ret = create_dumpfile ();
		if (ret) {
			/* creation did not work, set to "internally 4" */
			if (cob_initialized) {
				cobsetptr->cob_core_on_error = 4;
			}
			core_on_error = 4;
		}
	}
	return core_on_error;
}

void
cob_stop_error (void)
{
	cob_runtime_error ("STOP ERROR");
	cob_hard_failure ();
}

void
cob_hard_failure ()
{
	unsigned int core_on_error = handle_core_on_error ();
	if (core_on_error != 4) {
		if (core_on_error == 2 && cob_initialized) {
			/* prevent unloading modules */
			cobsetptr->cob_physical_cancel = -1;
		}
		call_exit_handlers_and_terminate ();
	}
	exit_code = -1;
#ifndef COB_WITHOUT_JMP
	if (return_jmp_buffer_set) {
		longjmp (return_jmp_buf, -1);
	}
#endif
	/* if explicit requested for errors or
	   an explicit manual coredump creation did
	   not work raise an abort here */
	if (core_on_error == 2
	 || core_on_error == 4) {
		cob_raise (SIGABRT);
	}
	exit (EXIT_FAILURE);
}

void
cob_hard_failure_internal (const char *prefix)
{
	unsigned int core_on_error;
	if (prefix) {
		fprintf (stderr, "\n%s: ", prefix);
	} else {
		fprintf (stderr, "\n");
	}
	fprintf (stderr, _("Please report this!"));
	fprintf (stderr, "\n");
	core_on_error = handle_core_on_error ();
	if (core_on_error != 4) {
		if (core_on_error == 2 && cob_initialized) {
			/* prevent unloading modules */
			cobsetptr->cob_physical_cancel = -1;
		}
		call_exit_handlers_and_terminate ();
	}
	exit_code = -2;
#ifndef COB_WITHOUT_JMP
	if (return_jmp_buffer_set) {
		longjmp (return_jmp_buf, -2);
	}
#endif
	/* if explicit requested for errors or
	   an explicit manual coredump creation did
	   not work raise an abort here */
	if (core_on_error == 2
	 || core_on_error == 4) {
		cob_raise (SIGABRT);
	}
	exit (EXIT_FAILURE);
}

/* get internal exit code, which is set on
  STOP RUN / last GOBACK / runtime error / cob_tidy */
int
cob_last_exit_code ()
{
	return exit_code;
}

/* get pointer to last runtime error (empty if no runtime error raised) */
const char *
cob_last_runtime_error ()
{
	return runtime_err_str;
}

int
cob_is_initialized (void)
{
	return (cobglobptr != NULL);
}

cob_global *
cob_get_global_ptr (void)
{
	/* LCOV_EXCL_START */
	if (!cob_initialized) {
		cob_fatal_error (COB_FERROR_INITIALIZED);
	}
	/* LCOV_EXCL_STOP */
	return cobglobptr;
}

int
cob_module_global_enter (cob_module **module, cob_global **mglobal,
		  const int auto_init, const int entry, const unsigned int *name_hash)
{
	/* Check initialized */
	if (!cob_initialized) {
		if (auto_init) {
			cob_init (0, NULL);
		} else {
			cob_fatal_error (COB_FERROR_INITIALIZED);
		}
	}

	/* Set global pointer */
	*mglobal = cobglobptr;

	/* Was caller a COBOL module */
	if (name_hash != NULL
	 && cobglobptr->cob_call_name_hash != 0) {
		cobglobptr->cob_call_from_c = 1;
		while (*name_hash != 0) {	/* Scan table of values */
			if (cobglobptr->cob_call_name_hash == *name_hash) {
				cobglobptr->cob_call_from_c = 0;
				break;
			}
			name_hash++;
		}
	}

	/* Check module pointer */
	if (!*module) {
		struct cob_alloc_module *mod_ptr;

		*module = cob_cache_malloc (sizeof (cob_module));
		/* Add to list of all modules activated */
		mod_ptr = cob_malloc (sizeof (struct cob_alloc_module));
		mod_ptr->cob_pointer = *module;
		mod_ptr->next = cob_module_list;
		cob_module_list = mod_ptr;
	} else
	if (entry == 0
	 && !cobglobptr->cob_call_from_c) {
		register int		k = 0;
		register cob_module	*mod;
		for (mod = COB_MODULE_PTR; mod; mod = mod->next) {
			if (*module == mod) {
				/* CHECKME: can we move this in 4.x to the generated program
				   to be done _before_ executing cob_module_global_enter using
				   a _static_ variable ? */
				if (cobglobptr->cob_stmt_exception) {
					/* CALL has ON EXCEPTION so return to caller */
					cob_set_exception (COB_EC_PROGRAM_RECURSIVE_CALL);
					cobglobptr->cob_stmt_exception = 0;
					return 1;
				}
				cob_module_err = mod;
				cob_fatal_error (COB_FERROR_RECURSIVE);
			}
			/* LCOV_EXCL_START */
			if (k++ == MAX_MODULE_ITERS) {	/* prevent endless loop in case of broken list */
				/* not translated as highly unexpected */
				cob_runtime_warning ("max module iterations exceeded, possible broken chain");
				break;
			}
			/* LCOV_EXCL_STOP */
		}
	}

	/* Save parameter count, get number from argc if main program */
	if (!COB_MODULE_PTR) {
		cobglobptr->cob_call_params = cob_argc - 1;
		if(cobglobptr->cob_call_params < 0)
			cobglobptr->cob_call_params = 0;
	}

	(*module)->module_num_params = cobglobptr->cob_call_params;

	/* Push module pointer */
	(*module)->next = COB_MODULE_PTR;
	COB_MODULE_PTR = *module;
	COB_MODULE_PTR->module_stmt = 0;
	COB_MODULE_PTR->statement = STMT_UNKNOWN;
	cobglobptr->cob_stmt_exception = 0;

	if (cobsetptr->cob_line_trace)
		COB_MODULE_PTR->flag_debug_trace |= COB_MODULE_READYTRACE;
	else
		COB_MODULE_PTR->flag_debug_trace &= ~COB_MODULE_READYTRACE;

	return 0;
}

void
cob_module_enter (cob_module **module, cob_global **mglobal,
		  const int auto_init)
{
	(void)cob_module_global_enter (module, mglobal, auto_init, 0, 0);
}

void
cob_module_leave (cob_module *module)
{
	COB_UNUSED (module);
	cob_get_source_line ();
	if(cobglobptr->cob_exception_code == -1)
		cobglobptr->cob_exception_code = 0;
	/* Pop module pointer */
	COB_MODULE_PTR = COB_MODULE_PTR->next;
	cobglobptr->cob_call_name_hash = 0;
	cobglobptr->cob_call_from_c = 1;
	cobglobptr->cob_call_params = 0;
}

void
cob_module_free (cob_module **module)
{
	struct cob_alloc_module	*ptr, *prv;
	if (*module == NULL) {
		return;
	}

	cob_module_clean (*module);

	/* TODO: consider storing the last entry and a prev pointer
	   to optimize for the likely case of "program added last is removed"
	   instead of checking _all_ previous entries */
	prv = NULL;
	/* Remove from list of all modules activated */
	for (ptr = cob_module_list; ptr; ptr = ptr->next) {
		if (ptr->cob_pointer == *module) {
			if (prv == NULL) {
				cob_module_list = ptr->next;
			} else {
				prv->next = ptr->next;
			}
			cob_free (ptr);
			break;
		}
		prv = ptr;
	}

	if (!cobglobptr->cob_call_from_c) {
		if ((*module)->param_buf != NULL) {
			cob_cache_free((*module)->param_buf);
		}
		if ((*module)->param_field != NULL) {
			cob_cache_free((*module)->param_field);
		}
	}
	cob_cache_free (*module);
	*module = NULL;
}

/* save module environment - returns an allocated cob_func_loc (free at cob_restore_func)
   and the intermediate return field (must be freed by caller) */
struct cob_func_loc *
cob_save_func (cob_field **savefld, const int params,
	       const int eparams, ...)
{
	struct cob_func_loc	*fl;
	int			numparams;

	if (params > eparams) {
		numparams = eparams;
	} else {
		numparams = params;
	}

	/* Allocate return field */
	*savefld = cob_malloc (sizeof (cob_field));

	/* Allocate save area */
	fl = cob_malloc (sizeof (struct cob_func_loc));
	fl->func_params = cob_malloc (sizeof (void *) * ((size_t)numparams + 1U));
	fl->data = cob_malloc (sizeof (void *) * ((size_t)numparams + 1U));

	/* Save values */
	fl->save_module = COB_MODULE_PTR->next;
	fl->save_call_params = cobglobptr->cob_call_params;
	fl->save_proc_parms = COB_MODULE_PTR->cob_procedure_params;
	fl->save_num_params = COB_MODULE_PTR->module_num_params;

	/* Set current values */
	COB_MODULE_PTR->cob_procedure_params = fl->func_params;
	cobglobptr->cob_call_params = numparams;
	if (numparams) {
		va_list			args;
		int			n;
		va_start (args, eparams);
		for (n = 0; n < numparams; ++n) {
			fl->func_params[n] = va_arg (args, cob_field *);
			if (fl->func_params[n]) {
				fl->data[n] = fl->func_params[n]->data;
			}
		}
		va_end (args);
	}
	return fl;
}

/* restores module environment - frees the passed cob_func_loc */
void
cob_restore_func (struct cob_func_loc *fl)
{
	/* Restore calling environment */
	cobglobptr->cob_call_params = fl->save_call_params;
#if	0	/* RXWRXW - MODNEXT */
	COB_MODULE_PTR->next = fl->save_module;
#endif
	COB_MODULE_PTR->cob_procedure_params = fl->save_proc_parms;
	COB_MODULE_PTR->module_num_params = fl->save_num_params;
	cob_free (fl->data);
	cob_free (fl->func_params);
	cob_free (fl);
}

/*
 * Copy the returning 'cob_field' and return address of the copy
 * This is done to avoid passing back a point to data on the C stack
 * for a function which has returned
*/
cob_field *
cob_function_return (cob_field *rtn)
{
	COB_MODULE_PTR->function_return = *rtn;
	return &COB_MODULE_PTR->function_return;
}

struct ver_t {
	int major, minor, point;
	unsigned int version;
};

/*
 * Convert version components to an integer value for comparison.
 */
static COB_INLINE unsigned int
version_bitstring( const struct ver_t module )
{
	unsigned int version =
		((unsigned int)module.major << 24) |
		((unsigned int)module.minor << 16) |
		((unsigned int)module.point <<  8);
	return version;
}

void
cob_check_version (const char *prog,
		   const char *packver_prog, const int patchlev_prog)
{
	int nparts;
	struct ver_t lib, app;

	app.major = 0;
	app.minor = 0;
	app.point = 0;
	lib.major = 9;
	lib.minor = 9;
	lib.point = 0;

	nparts = sscanf (PACKAGE_VERSION, "%d.%d.%d",
			 &lib.major, &lib.minor, &lib.point);
	lib.version = version_bitstring(lib);

	if (nparts >= 2) {
		(void)sscanf (packver_prog, "%d.%d.%d",
			 &app.major, &app.minor, &app.point);
		app.version = version_bitstring(app);

		if (app.major == 2 && app.minor < 2) {
			goto err;
		}
		/* COB_MODULE_PTR is expected to be set, because cob_module_global_enter
		   was called _directly_ before this function, also with 2.2,
		   still checking at least for the case of "misuse" of this function as
		   undocumented ABI call [as in our testsuite ...] */
		if (cobglobptr && COB_MODULE_PTR && !COB_MODULE_PTR->gc_version) {
			COB_MODULE_PTR->gc_version = packver_prog;
		}
		if (app.version == lib.version
		 && patchlev_prog <= PATCH_LEVEL) {
			return;
		}
		if (app.version < lib.version) {
			struct ver_t minimal = { 4, 0 };
			if (app.version >= version_bitstring (minimal)) {
				return;
			}
		}
	}

err:
	/* TODO: when CALLed - raise exception so program can go ON EXCEPTION */
	cob_runtime_error (_("version mismatch"));
	cob_runtime_hint (_("%s has version %s.%d"), prog,
			   packver_prog, patchlev_prog);
	cob_runtime_hint (_("%s has version %s.%d"), "libcob",
			   PACKAGE_VERSION, PATCH_LEVEL);
	cob_hard_failure ();
}

void
cob_parameter_check (const char *func_name, const int num_arguments)
{
	if (cobglobptr->cob_call_params < num_arguments) {
		/* TODO: raise exception */
		cob_runtime_error (_("CALL to %s requires %d arguments"),
				   func_name, num_arguments);
		cob_hard_failure ();
	}
}

void
cob_correct_numeric (cob_field *f)
{
	unsigned char	*p;
	unsigned char	*data;
	size_t		size;
	size_t		i;

	if (!COB_FIELD_IS_NUMDISP (f)) {
		return;
	}
	size = f->size;
	data = f->data;
	if (COB_FIELD_HAVE_SIGN (f)) {
		/* Adjust for sign byte */
		size--;
		if (COB_FIELD_SIGN_LEADING (f)) {
			p = f->data;
			data = p + 1;
		} else {
			p = f->data + f->size - 1;
		}
		if (COB_FIELD_SIGN_SEPARATE (f)) {
			if (*p != '+' && *p != '-') {
				*p = '+';
			}
		} else if (COB_MODULE_PTR->ebcdic_sign) {
			switch (*p) {
			case '{':
			case 'A':
			case 'B':
			case 'C':
			case 'D':
			case 'E':
			case 'F':
			case 'G':
			case 'H':
			case 'I':
			case '}':
			case 'J':
			case 'K':
			case 'L':
			case 'M':
			case 'N':
			case 'O':
			case 'P':
			case 'Q':
			case 'R':
				break;
			case '0':
				*p = '{';
				break;
			case '1':
				*p = 'A';
				break;
			case '2':
				*p = 'B';
				break;
			case '3':
				*p = 'C';
				break;
			case '4':
				*p = 'D';
				break;
			case '5':
				*p = 'E';
				break;
			case '6':
				*p = 'F';
				break;
			case '7':
				*p = 'G';
				break;
			case '8':
				*p = 'H';
				break;
			case '9':
				*p = 'I';
				break;
			case 0:
			case ' ':
				*p = '{';
				break;
			default:
				break;
			}
		} else {
			if (!*p || *p == ' ') {
				*p = '0';
			}
		}
	} else {
		p = f->data + f->size - 1;
		if (COB_MODULE_PTR->ebcdic_sign) {
			switch (*p) {
			case 0:
			case ' ':
			case '{':
			case '}':
				*p = '0';
				break;
			case 'A':
			case 'B':
			case 'C':
			case 'D':
			case 'E':
			case 'F':
			case 'G':
			case 'H':
			case 'I':
				*p = '1' + (*p - 'A');
				break;
			case 'J':
			case 'K':
			case 'L':
			case 'M':
			case 'N':
			case 'O':
			case 'P':
			case 'Q':
			case 'R':
				*p = '1' + (*p - 'J');
				break;
			default:
				break;
			}
		} else {
			switch (*p) {
			case 0:
			case ' ':
			case 'p':
				*p = '0';
				break;
			case 'q':
				*p = '1';
				break;
			case 'r':
				*p = '2';
				break;
			case 's':
				*p = '3';
				break;
			case 't':
				*p = '4';
				break;
			case 'u':
				*p = '5';
				break;
			case 'v':
				*p = '6';
				break;
			case 'w':
				*p = '7';
				break;
			case 'x':
				*p = '8';
				break;
			case 'y':
				*p = '9';
				break;
			default:
				break;
			}
		}
	}
	for (i = 0, p = data; i < size; ++i, ++p) {
		switch (*p) {
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			break;
		case 0:
		case ' ':
			*p = '0';
			break;
		default:
			if ((*p & 0x0F) <= 9) {
				*p = (*p & 0x0F) + '0';
			}
			break;
		}
	}
}

static int
cob_check_numdisp (const cob_field *f)
{
	unsigned char	*p;
	unsigned char	*data;
	size_t		size;
	size_t		i;

	size = f->size;
	data = f->data;
	if (COB_FIELD_HAVE_SIGN (f)) {
		/* Adjust for sign byte */
		size--;
		if (COB_FIELD_SIGN_LEADING (f)) {
			p = f->data;
			data = p + 1;
		} else {
			p = f->data + f->size - 1;
		}
		if (COB_FIELD_SIGN_SEPARATE (f)) {
			if (*p != '+' && *p != '-') {
				return 0;
			}
		} else if (COB_MODULE_PTR->ebcdic_sign) {
			switch (*p) {
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
			case '{':
			case 'A':
			case 'B':
			case 'C':
			case 'D':
			case 'E':
			case 'F':
			case 'G':
			case 'H':
			case 'I':
			case '}':
			case 'J':
			case 'K':
			case 'L':
			case 'M':
			case 'N':
			case 'O':
			case 'P':
			case 'Q':
			case 'R':
				break;
			default:
				return 0;
			}
		} else {
			switch (*p) {
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
			case 'p':
			case 'q':
			case 'r':
			case 's':
			case 't':
			case 'u':
			case 'v':
			case 'w':
			case 'x':
			case 'y':
				break;
			default:
				return 0;
			}
		}
	}
	for (i = 0; i < size; ++i) {
		if (!isdigit (data[i])) {
			return 0;
		}
	}
	return 1;
}

/* Sign */

static COB_INLINE COB_A_INLINE unsigned char *
locate_sign (cob_field *f)
{
	if (COB_FIELD_SIGN_LEADING (f)) {
		return f->data;
	}
	return f->data + f->size - 1;
}

int
cob_real_get_sign (cob_field *f)
{
	unsigned char	*p;

	switch (COB_FIELD_TYPE (f)) {
	case COB_TYPE_NUMERIC_DISPLAY:
		p = locate_sign (f);

		/* Get sign */
		if (COB_FIELD_SIGN_SEPARATE (f)) {
			return (*p == '-') ? -1 : 1;
		}
		if (*p >= (unsigned char)'0' && *p <= (unsigned char)'9') {
			return 1;
		}
		if (*p == ' ') {
#if	0	/* RXWRXW - Space sign */
			*p = (unsigned char)'0';
#endif
			return 1;
		}
		if (COB_MODULE_PTR->ebcdic_sign) {
			return cob_get_sign_ebcdic (p);
		}
		return cob_get_sign_ascii (p);
	case COB_TYPE_NUMERIC_PACKED:
		if (COB_FIELD_NO_SIGN_NIBBLE (f)) {
			return 1;
		}
		p = f->data + f->size - 1;
		return ((*p & 0x0F) == 0x0D) ? -1 : 1;
	}
	return 0;
}

void
cob_real_put_sign (cob_field *f, const int sign)
{
	unsigned char	*p;

	switch (COB_FIELD_TYPE (f)) {
	case COB_TYPE_NUMERIC_DISPLAY:
		/* Note: we only locate the sign if needed,
		   as the common case will be "nothing to do" */
		if (COB_FIELD_SIGN_SEPARATE (f)) {
			const unsigned char	c = (sign < 0) ? (cob_u8_t)'-' : (cob_u8_t)'+';
			p = locate_sign (f);
			if (*p != c) {
				*p = c;
			}
		} else if (COB_MODULE_PTR
				&& COB_MODULE_PTR->ebcdic_sign) {
			p = locate_sign (f);
			cob_put_sign_ebcdic (p, sign);
		} else if (sign < 0) {
			p = locate_sign (f);
			cob_put_sign_ascii (p);
		}
		return;
	case COB_TYPE_NUMERIC_PACKED:
		if (COB_FIELD_NO_SIGN_NIBBLE (f)) {
			return;
		}
		p = f->data + f->size - 1;
		if (sign < 0) {
			*p = (*p & 0xF0) | 0x0D;
		} else {
			*p = (*p & 0xF0) | 0x0C;
		}
		return;
	}
}

/* Registration of external handlers */
void
cob_reg_sighnd	(void (*sighnd) (int))
{
	if (!cob_initialized) {
		cob_set_signal ();
	}
	cob_ext_sighdl = sighnd;
}

/* Switch */

int
cob_get_switch (const int n)
{
	if (n < 0 || n > COB_SWITCH_MAX) {
		return 0;
	}
	return cob_switch[n];
}

void
cob_set_switch (const int n, const int flag)
{
	if (n < 0 || n > COB_SWITCH_MAX) {
		return;
	}
	if (flag == 0) {
		cob_switch[n] = 0;
	} else if (flag == 1) {
		cob_switch[n] = 1;
	}
}

int
cob_cmp (cob_field *f1, cob_field *f2)
{
	unsigned short		f1_type = COB_FIELD_TYPE (f1);
	unsigned short		f2_type = COB_FIELD_TYPE (f2);

	const int	f1_is_numeric = f1_type & COB_TYPE_NUMERIC;
	const int	f2_is_numeric = f2_type & COB_TYPE_NUMERIC;

	/* both numeric -> direct compare */
	if (f1_is_numeric && f2_is_numeric) {
		return cob_numeric_cmp (f1, f2);
	}

	/* one is an internal ALL field (ZERO,LOW-VALUE, ...) */
	if (f2_type == COB_TYPE_ALPHANUMERIC_ALL) {
		if (f2->size == 1 && f2->data[0] == '0'
		 && f1_is_numeric) {
			return cob_cmp_int (f1, 0);
		}
		return cob_cmp_all (f1, f2);
	}
	if (f1_type == COB_TYPE_ALPHANUMERIC_ALL) {
		if (f1->size == 1 && f1->data[0] == '0'
		 && f2_is_numeric) {
			return -cob_cmp_int (f2, 0);
		}
		return -cob_cmp_all (f2, f1);
	}

#if 0	/* FIXME later: must cater for national fields, too,
		   at least if that is numeric NATIONAL	*/
	if (COB_FIELD_IS_NATIONAL (f1)) {
		...
	}
#endif

	/* all else -> alphanumeric comparision */

	/* if one is numeric (cannot be "both", as checked above), then
	   convert that to alphanumeric for final test;
	   note: this is _very_ seldom the case, during "make checkall"
	   only in test "Alphanumeric and binary numeric" */

	if (f1_is_numeric || f2_is_numeric) {
		/* CHECKME: What should be returned if field is negative?
		   We suspicously change -12 to 12 here... */
		cob_field	temp;
		cob_field_attr	attr;
		unsigned char	buff[COB_MAX_DIGITS + 10];

		/* CHECKME: may need to abort if we ever get here with float data */

		/* FIXME: must be converted to COB_TYPE_NUMERIC_EDITED with an
		   internal PIC of COB_FIELD_DIGITS '9's and leading sign,
		   otherwise we'll fail as soon as we enable COB_MAX_BINARY */
		if (f1_is_numeric
		 && f1_type != COB_TYPE_NUMERIC_DISPLAY) {
			temp.size = COB_FIELD_DIGITS (f1);
			temp.data = buff;
			temp.attr = &attr;
			attr = *f1->attr;
			attr.type = COB_TYPE_NUMERIC_DISPLAY;
			attr.flags &= ~COB_FLAG_HAVE_SIGN;
			cob_move (f1, &temp);
			f1 = &temp;
		}
		if (f2_is_numeric
		 && f2_type != COB_TYPE_NUMERIC_DISPLAY) {
			temp.size = COB_FIELD_DIGITS (f2);
			temp.data = buff;
			temp.attr = &attr;
			attr = *f2->attr;
			attr.type = COB_TYPE_NUMERIC_DISPLAY;
			attr.flags &= ~COB_FLAG_HAVE_SIGN;
			cob_move (f2, &temp);
			f2 = &temp;
		}

		if (COB_FIELD_HAVE_SIGN (f1)) {
			/* Note: if field is numeric then it is always
			   USAGE DISPLAY here */

			if (f1 != &temp) {				
				/* drop sign for comparision, using a copy to not change
				   the field during comparision */
				unsigned char buff2[COB_MAX_DIGITS + 10];
				const size_t size = f1->size;
				int		ret;
				unsigned char	*real_data = f1->data;
				memcpy (buff2, real_data, size);
				f1->data = buff2;
				(void)cob_real_get_sign (f1);
				ret = cob_cmp_alnum (f1, f2);
				f1->data = real_data;
				return ret;
			} else {
				/* we operate on a buffer already, just go on */
				(void)cob_real_get_sign (f1);
				return cob_cmp_alnum (f1, f2);
			}
		}

		if (COB_FIELD_HAVE_SIGN (f2)) {
			/* Note: if field is numeric then it is always
			   USAGE DISPLAY here */

			if (f2 != &temp) {				
				/* drop sign for comparision, using a copy to not change
				   the field during comparision */
				unsigned char buff2[COB_MAX_DIGITS + 10];
				const size_t size = f2->size;
				int		ret;
				unsigned char	*real_data = f2->data;
				memcpy (buff2, real_data, size);
				f2->data = buff2;
				(void)cob_real_get_sign (f2);
				ret = cob_cmp_alnum (f1, f2);
				f2->data = real_data;
				return ret;
			} else {
				/* we operate on a buffer already, just go on */
				(void)cob_real_get_sign (f2);
				return cob_cmp_alnum (f1, f2);
			}
		}
	}
	return cob_cmp_alnum (f1, f2);
}

/* Class check */

int
cob_is_omitted (const cob_field *f)
{
	return f->data == NULL;
}

int
cob_is_numeric (const cob_field *f)
{
	size_t		i;
	union {
		float		fpf;
		double		fpd;
	} fval;
	int		sign;

	switch (COB_FIELD_TYPE (f)) {
	case COB_TYPE_NUMERIC_BINARY:
		return 1;
	case COB_TYPE_NUMERIC_FLOAT:
		memcpy (&fval.fpf, f->data, sizeof (float));
		return !ISFINITE ((double)fval.fpf);
	case COB_TYPE_NUMERIC_DOUBLE:
		memcpy (&fval.fpd, f->data, sizeof (double));
		return !ISFINITE (fval.fpd);
	case COB_TYPE_NUMERIC_PACKED:
		/* Check digits */
		for (i = 0; i < f->size - 1; ++i) {
			if ((f->data[i] & 0xF0) > 0x90 ||
			    (f->data[i] & 0x0F) > 0x09) {
				return 0;
			}
		}
		/* Check high nibble of last byte */
		if ((f->data[i] & 0xF0) > 0x90) {
			return 0;
		}

		if (COB_FIELD_NO_SIGN_NIBBLE (f)) {
			/* COMP-6 - Check last nibble */
			if ((f->data[i] & 0x0F) > 0x09) {
				return 0;
			}
			return 1;
		}

		/* Check sign */
		sign = f->data[i] & 0x0F;
		if (COB_FIELD_HAVE_SIGN (f)) {
			if (sign == 0x0C || sign == 0x0D) {
				return 1;
			}
			if (COB_MODULE_PTR->flag_host_sign &&
			    sign == 0x0F) {
				return 1;
			}
		} else if (sign == 0x0F) {
			return 1;
		}
		return 0;
	case COB_TYPE_NUMERIC_DISPLAY:
		return cob_check_numdisp (f);
	case COB_TYPE_NUMERIC_FP_DEC64:
#ifdef	WORDS_BIGENDIAN
		return (f->data[0] & 0x78U) != 0x78U;
#else
		return (f->data[7] & 0x78U) != 0x78U;
#endif
	case COB_TYPE_NUMERIC_FP_DEC128:
#ifdef	WORDS_BIGENDIAN
		return (f->data[0] & 0x78U) != 0x78U;
#else
		return (f->data[15] & 0x78U) != 0x78U;
#endif
	default:
		for (i = 0; i < f->size; ++i) {
			if (!isdigit (f->data[i])) {
				return 0;
			}
		}
		return 1;
	}
}

int
cob_is_alpha (const cob_field *f)
{
	size_t	i;

	for (i = 0; i < f->size; ++i) {
		if (!isalpha (f->data[i]) && f->data[i] != (unsigned char)' ') {
			return 0;
		}
	}
	return 1;
}

int
cob_is_upper (const cob_field *f)
{
	size_t	i;

	for (i = 0; i < f->size; ++i) {
		if (!isupper (f->data[i]) && f->data[i] != (unsigned char)' ') {
			return 0;
		}
	}
	return 1;
}

int
cob_is_lower (const cob_field *f)
{
	size_t	i;

	for (i = 0; i < f->size; ++i) {
		if (!islower (f->data[i]) && f->data[i] != (unsigned char)' ') {
			return 0;
		}
	}
	return 1;
}

/* Table sort */

void
cob_table_sort_init (const size_t nkeys, const unsigned char *collating_sequence)
{
	sort_nkeys = 0;
	sort_keys = cob_malloc (nkeys * sizeof (cob_file_key));
	if (collating_sequence) {
		sort_collate = collating_sequence;
	} else {
		sort_collate = COB_MODULE_PTR->collating_sequence;
	}
}

void
cob_table_sort_init_key (cob_field *field, const int flag,
			 const unsigned int offset)
{
	sort_keys[sort_nkeys].field = field;
	sort_keys[sort_nkeys].tf_ascending = flag;
	sort_keys[sort_nkeys].offset = offset;
	sort_nkeys++;
}

void
cob_table_sort (cob_field *f, const int n)
{
	qsort (f->data, (size_t) n, f->size, sort_compare);
	cob_free (sort_keys);
}

/* Run-time error checking */

void
cob_check_beyond_exit (const unsigned char *name)
{
	/* possibly allow to lower this to a runtime warning later */
	cob_runtime_error (_("code execution leaving %s"), name);
	cob_hard_failure ();
}

void
cob_check_based (const unsigned char *x, const char *name)
{
	if (!x) {
		/* name includes '' already and can be ... 'x' (addressed by 'y') */
		/* TODO: raise exception */
		cob_runtime_error (_("BASED/LINKAGE item %s has NULL address"), name);
		cob_hard_failure ();
	}
}

void
cob_check_linkage (const unsigned char *x, const char *name)
{
	if (!x) {
		const int check_type = 0;	/* not merged yet */
		/* name includes '' already and can be ... 'x' of 'y' */
		switch (check_type) {
		case 0: /* check for passed items and size on module entry */
			/* TODO: raise exception */
			cob_runtime_error (_("LINKAGE item %s not passed by caller"), name);
			break;
		case 1: /* check for passed OPTIONAL items on item use */
			/* TODO: raise exception */
			cob_runtime_error (_("LINKAGE item %s not passed by caller"), name);
			break;
		}
		cob_hard_failure ();
	}
}

const char *
explain_field_type (const cob_field *f)
{
	switch (COB_FIELD_TYPE (f)) {
	case COB_TYPE_GROUP:
		return "GROUP";
	case COB_TYPE_BOOLEAN:
		return "BOOLEAN";
	case COB_TYPE_NUMERIC_DISPLAY:
		return "NUMERIC DISPLAY";
	case COB_TYPE_NUMERIC_BINARY:
		return "BINARY";
	case COB_TYPE_NUMERIC_PACKED:
		return "PACKED-DECIMAL";
	case COB_TYPE_NUMERIC_FLOAT:
		return "FLOAT";
	case COB_TYPE_NUMERIC_DOUBLE:
		return "DOUBLE";
	case COB_TYPE_NUMERIC_L_DOUBLE:
		return "LONG DOUBLE";
	case COB_TYPE_NUMERIC_FP_DEC64:
		return "FP DECIMAL 64";
	case COB_TYPE_NUMERIC_FP_DEC128:
		return "FP DECIMAL 128";
	case COB_TYPE_NUMERIC_FP_BIN32:
		return "FP BINARY 32";
	case COB_TYPE_NUMERIC_FP_BIN64:
		return "FP BINARY 64";
	case COB_TYPE_NUMERIC_FP_BIN128:
		return "FP BINARY 128";
	/* note: may be not reached depending on endianness */
	case COB_TYPE_NUMERIC_COMP5:
		return "COMP-5";
	case COB_TYPE_NUMERIC_EDITED:
		return "NUMERIC EDITED";
	case COB_TYPE_ALPHANUMERIC:
		return "ALPHANUMERIC";
	case COB_TYPE_ALPHANUMERIC_ALL:
		return "ALPHANUMERIC ALL";
	case COB_TYPE_ALPHANUMERIC_EDITED:
		return "ALPHANUMERIC EDITED";
	case COB_TYPE_NATIONAL:
		return "NATIONAL";
	case COB_TYPE_NATIONAL_EDITED:
		return "NATIONAL EDITED";
	default:
		break;
	}
	return "UNKNOWN";
}

void
cob_check_numeric (const cob_field *f, const char *name)
{
	unsigned char	*data;
	char		*p;
	char		*buff;
	size_t		i;

	if (!cob_is_numeric (f)) {
		cob_set_exception (COB_EC_DATA_INCOMPATIBLE);
		buff = cob_fast_malloc ((size_t)COB_SMALL_BUFF);
		p = buff;
		data = f->data;
		if (COB_FIELD_IS_NUMDISP(f) || COB_FIELD_IS_ANY_ALNUM(f)) {
			for (i = 0; i < f->size; ++i) {
				if (isprint (data[i])) {
					*p++ = data[i];
				} else {
					p += sprintf (p, "\\%03o", data[i]);
				}
			}
		} else {
			p += sprintf (p, "0x");
			for (i = 0; i < f->size; ++i) {
				p += sprintf (p, "%02x", data[i]);
			}
		}
		*p = '\0';
		cob_runtime_error (_("'%s' (Type: %s) not numeric: '%s'"),
			name, explain_field_type(f), buff);
		cob_free (buff);
		cob_hard_failure ();
	}
}

void
cob_check_odo (const int i, const int min, const int max,
			const char *name, const char *dep_name)
{
	/* Check OCCURS DEPENDING ON item */
	if (i < min || i > max) {
		cob_set_exception (COB_EC_BOUND_ODO);

		cob_runtime_error (_("OCCURS DEPENDING ON '%s' out of bounds: %d"),
					dep_name, i);
		if (i > max) {
			cob_runtime_hint (_("maximum subscript for '%s': %d"), name, max);
		} else {
			cob_runtime_hint (_("minimum subscript for '%s': %d"), name, min);
		}
		cob_hard_failure ();
	}
}

void
cob_check_subscript (const int i, const int max,
			const char *name, const int odo_item)
{
	/* Check subscript */
	if (i < 1 || i > max) {
		cob_set_exception (COB_EC_BOUND_SUBSCRIPT);
		cob_runtime_error (_("subscript of '%s' out of bounds: %d"), name, i);
		if (i >= 1) {
			if (odo_item) {
				cob_runtime_hint (_("current maximum subscript for '%s': %d"),
							name, max);
			} else {
				cob_runtime_hint (_("maximum subscript for '%s': %d"),
							name, max);
			}
		}
		cob_hard_failure ();
	}
}

void
cob_check_ref_mod (const char *name, const int abend, const int zero_allowed,
	const int size, const int offset, const int length)
{
	const int minimal_length = zero_allowed ? 0 : 1;

	/* Check offset */
	if (offset < 1 || offset > size) {
		cob_set_exception (COB_EC_BOUND_REF_MOD);
		if (offset < 1) {
			cob_runtime_error (_("offset of '%s' out of bounds: %d"),
			   name, offset);
		} else {
			cob_runtime_error (_("offset of '%s' out of bounds: %d, maximum: %d"),
			   name, offset, size);
		}
		if (abend) {
			cob_hard_failure ();
		}
	}

	/* Check plain length */
	if (length < minimal_length || length > size) {
		cob_set_exception (COB_EC_BOUND_REF_MOD);
		if (length < minimal_length) {
			cob_runtime_error (_("length of '%s' out of bounds: %d"),
			   name, length);
		} else {
			cob_runtime_error (_("length of '%s' out of bounds: %d, maximum: %d"),
			   name, length, size);
		}
		if (abend) {
			cob_hard_failure ();
		}
	}

	/* Check length with offset */
	if (offset + length - 1 > size) {
		cob_set_exception (COB_EC_BOUND_REF_MOD);
		cob_runtime_error (_("length of '%s' out of bounds: %d, starting at: %d, maximum: %d"),
			name, length, offset, size);
		if (abend) {
			cob_hard_failure ();
		}
	}
}

void
cob_check_ref_mod_minimal (const char* name, const int offset, const int length)
{
	/* Check offset */
	if (offset < 1) {
		cob_set_exception (COB_EC_BOUND_REF_MOD);
		cob_runtime_error (_("offset of '%s' out of bounds: %d"),
			name, offset);
		cob_hard_failure ();
	}

	/* Check length */
	if (length < 1) {
		cob_set_exception (COB_EC_BOUND_REF_MOD);
		cob_runtime_error (_("length of '%s' out of bounds: %d"),
			name, length);
		cob_hard_failure ();
	}
}

/* check if already allocated, if yes returns its address and sets exlength */
static void *
cob_external_addr_lookup (const char *exname, int *exlength)
{
	struct cob_external *eptr;

	for (eptr = basext; eptr; eptr = eptr->next) {
		if (!strcmp (exname, eptr->ename)) {
			if (exlength) {
				*exlength = eptr->esize;
			}
			return eptr->ext_alloc;
		}
	}
	return NULL;
}

/* allocate new external entry;
   returns allocated pointer with requested size */
static void *
cob_external_addr_create (const char *exname, int exlength)
{
	struct cob_external *eptr;

	eptr = cob_malloc (sizeof (struct cob_external));
	eptr->next = basext;
	eptr->esize = exlength;
	eptr->ename = cob_strdup (exname);
	eptr->ext_alloc = cob_malloc ((size_t)exlength);
	basext = eptr;

	return eptr->ext_alloc;
}

/* lookup external item, if already created before check given length;
   returns allocated pointer with at least requested size */
void *
cob_external_addr (const char *exname, const int exlength)
{
	int		stored_length;
	void	*ret;

	/* special external "C" registers */
	if (exlength == sizeof (int)
	 && !strcmp (exname, "ERRNO")) {
		return &errno;
	}

	/* Locate or allocate EXTERNAL item */
	ret = cob_external_addr_lookup (exname, &stored_length);
	if (ret != NULL) {
		if (exlength > stored_length) {
			cob_runtime_error (_ ("EXTERNAL item '%s' previously allocated with size %d, requested size is %d"),
				exname, stored_length, exlength);
			cob_hard_failure ();
		}
		if (exlength < stored_length) {
			cob_runtime_warning (_ ("EXTERNAL item '%s' previously allocated with size %d, requested size is %d"),
				exname, stored_length, exlength);
		}
		cobglobptr->cob_initial_external = 0;
	} else {
		ret = cob_external_addr_create (exname, exlength);
		cobglobptr->cob_initial_external = 1;
	}
	return ret;
}

#if defined (_MSC_VER)

/* Get function pointer for most precise time function
   GetSystemTimePreciseAsFileTime is available since OS-version Windows 2000
   GetSystemTimeAsFileTime        is available since OS-version Windows 8 / Server 2012
*/
static void
get_function_ptr_for_precise_time (void)
{
	HMODULE		kernel32_handle;

	kernel32_handle = GetModuleHandle (TEXT ("kernel32.dll"));
	if (kernel32_handle != NULL) {
		time_as_filetime_func = (VOID (WINAPI *) (LPFILETIME))
			GetProcAddress (kernel32_handle, "GetSystemTimePreciseAsFileTime");
	}
	if (time_as_filetime_func == NULL) {
		time_as_filetime_func = GetSystemTimeAsFileTime;
	}
}
#endif

/* split the timep to cob_time and set the offset from UTC */
void
static set_cob_time_from_localtime (time_t curtime,
		struct cob_time *cb_time) {

	struct tm	*tmptr;
#if !defined (_BSD_SOURCE) && !defined (HAVE_TIMEZONE)
	time_t		utctime, lcltime, difftime;
#endif

#ifndef TIME_T_IS_NON_ARITHMETIC
	static time_t last_time = 0;
	static struct cob_time last_cobtime;
	
	/* FIXME: on reseting appropriate locale set last_time_no_sec = 0 */
	if (curtime == last_time) {
		memcpy (cb_time, &last_cobtime, sizeof (struct cob_time));
		return;
	}
	if (last_time != 0) {
		const time_t sec_passed = curtime - last_time;
		/* most likely we are in the same hour as before - in which case
		   there is no need to lookup the tz-database via localtime -
		   instead directly adjust the last value */
		if (sec_passed > 0 && sec_passed < 3600) {
			memcpy (cb_time, &last_cobtime, sizeof (struct cob_time));
			cb_time->second += (int)sec_passed;
			if (cb_time->second < 60) {
				memcpy (&last_cobtime, cb_time, sizeof (struct cob_time));
				last_time = curtime;
				return;
			} else {
				cb_time->minute += cb_time->second / 60;
				/* note: if the minute is >= 60 then we need to adjust the hour
				   and may end up with DST issues (or simple day switch),
				   we may also get into minute == 60 for leap second cases;
				   do a full recalculation in this case */
				if (cb_time->minute < 60) {
					cb_time->second = cb_time->second % 60;
					memcpy (&last_cobtime, cb_time, sizeof (struct cob_time));
					last_time = curtime;
					return;
				}
			}
		}
	}
	last_time = curtime;
#endif

	tmptr = localtime (&curtime);

	cb_time->year = tmptr->tm_year + 1900;
	cb_time->month = tmptr->tm_mon + 1;
	cb_time->day_of_month = tmptr->tm_mday;
	cb_time->day_of_week = one_indexed_day_of_week_from_monday (tmptr->tm_wday);
	cb_time->day_of_year = tmptr->tm_yday + 1;
	cb_time->hour = tmptr->tm_hour;
	cb_time->minute = tmptr->tm_min;
	/* LCOV_EXCL_START */
	/* Leap seconds ? */
	if (tmptr->tm_sec >= 60) {
		tmptr->tm_sec = 59;
	}
	/* LCOV_EXCL_STOP */
	cb_time->second = tmptr->tm_sec;
	cb_time->nanosecond = 0;
	cb_time->is_daylight_saving_time = tmptr->tm_isdst;

#if defined (_BSD_SOURCE)
	cb_time->offset_known = 1;
	cb_time->utc_offset = tmptr->tm_gmtoff / 60;
#elif defined (HAVE_TIMEZONE)
	cb_time->offset_known = 1;
	cb_time->utc_offset = timezone / -60;
	/* LCOV_EXCL_START */
	if (tmptr->tm_isdst) {
		cb_time->utc_offset += 60;
	}
	/* LCOV_EXCL_STOP */
#else
	lcltime = mktime (tmptr);

	tmptr = gmtime (&curtime);
	utctime = mktime (tmptr);

	if (utctime != -1 && lcltime != -1) { /* LCOV_EXCL_BR_LINE */
		difftime = utctime - lcltime;
		/* LCOV_EXCL_START */
		if (tmptr->tm_isdst) {
			difftime -= 3600;
		}
		/* LCOV_EXCL_STOP */
		cb_time->utc_offset = difftime / 60;
		cb_time->offset_known = 1;
	/* LCOV_EXCL_START */
	} else {
		cb_time->offset_known = 0;
		cb_time->utc_offset = 0;
	}
	/* LCOV_EXCL_STOP */
#endif

#ifndef TIME_T_IS_NON_ARITHMETIC
	/* keep backup for next call */
	memcpy (&last_cobtime, cb_time, sizeof (struct cob_time));
#endif
}

#if defined (_WIN32) /* cygwin does not define _WIN32 */
static struct cob_time
cob_get_current_date_and_time_from_os (const enum cob_datetime_res res)
{
	SYSTEMTIME	local_time;
#if defined (_MSC_VER)
	FILETIME	filetime;
	SYSTEMTIME	utc_time;
#endif

	time_t		curtime;
	struct cob_time	cb_time;

	curtime = time (NULL);
	set_cob_time_from_localtime (curtime, &cb_time);

	if (res <= DTR_TIME_NO_NANO) {
		cb_time.nanosecond = 0;
		return cb_time;
	}

	/* Get nanoseconds with highest precision possible */
#if defined (_MSC_VER)
	if (!time_as_filetime_func) {
		get_function_ptr_for_precise_time ();
	}
#pragma warning(suppress: 6011) /* the function pointer is always set by get_function_ptr_for_precise_time */
	(time_as_filetime_func) (&filetime);
	/* fallback to GetLocalTime if one of the following does not work */
	if (FileTimeToSystemTime (&filetime, &utc_time) &&
		SystemTimeToTzSpecificLocalTime (NULL, &utc_time, &local_time)) {
		set_cob_time_ns_from_filetime (filetime, &cb_time);
		return cb_time;
	}
#endif
	GetLocalTime (&local_time);
	cb_time.nanosecond = local_time.wMilliseconds * 1000000;
	return cb_time;
}
#else
static struct cob_time
cob_get_current_date_and_time_from_os (const enum cob_datetime_res res)
{
#if defined (HAVE_CLOCK_GETTIME)
	struct timespec	time_spec;
#elif defined (HAVE_SYS_TIME_H) && defined (HAVE_GETTIMEOFDAY)
	struct timeval	tmv;
#endif
	time_t		curtime;
	struct cob_time	cb_time;

	/* Get the current time */
#if defined (HAVE_CLOCK_GETTIME)
	clock_gettime (CLOCK_REALTIME, &time_spec);
	curtime = time_spec.tv_sec;
#elif defined (HAVE_SYS_TIME_H) && defined (HAVE_GETTIMEOFDAY)
	gettimeofday (&tmv, NULL);
	curtime = tmv.tv_sec;
#else
	curtime = time (NULL);
#endif

	set_cob_time_from_localtime (curtime, &cb_time);

	/* Get nanoseconds or microseconds, if possible */
#if defined (HAVE_CLOCK_GETTIME)
	cb_time.nanosecond = (int) time_spec.tv_nsec;
#elif defined (HAVE_SYS_TIME_H) && defined (HAVE_GETTIMEOFDAY)
	cb_time.nanosecond = tmv.tv_usec * 1000;
#else
	cb_time.nanosecond = 0;
#endif

	return cb_time;
}
#endif

/* obsolete function, only used in cobc before 3.2 */
struct cob_time
cob_get_current_date_and_time (void)
{
	return cob_get_current_datetime (DTR_TIME_NO_NANO);
}

struct cob_time
cob_get_current_datetime (const enum cob_datetime_res res)
{
	struct cob_time	cb_time = cob_get_current_date_and_time_from_os (res);

	/* Do we have a constant time? */
	if (cobsetptr != NULL
	 && cobsetptr->cob_time_constant.year != 0) {
		int		needs_calculation = 0;
		/* Note: constant time but X not part of constant --> -1 */
		if (cobsetptr->cob_time_constant.year != -1) {
			cb_time.year = cobsetptr->cob_time_constant.year;
			needs_calculation = 1;
		}
		if (cobsetptr->cob_time_constant.month != -1) {
			cb_time.month = cobsetptr->cob_time_constant.month;
			needs_calculation = 1;
		}
		if (cobsetptr->cob_time_constant.day_of_month != -1) {
			cb_time.day_of_month = cobsetptr->cob_time_constant.day_of_month;
			needs_calculation = 1;
		}
		if (cobsetptr->cob_time_constant.hour != -1) {
			cb_time.hour = cobsetptr->cob_time_constant.hour;
		}
		if (cobsetptr->cob_time_constant.minute != -1) {
			cb_time.minute = cobsetptr->cob_time_constant.minute;
		}
		if (cobsetptr->cob_time_constant.second != -1) {
			cb_time.second = cobsetptr->cob_time_constant.second;
		}
		if (cobsetptr->cob_time_constant.nanosecond != -1) {
			cb_time.nanosecond = cobsetptr->cob_time_constant.nanosecond;
		}
		if (cobsetptr->cob_time_constant.offset_known) {
			cb_time.offset_known = cobsetptr->cob_time_constant.offset_known;
			cb_time.utc_offset = cobsetptr->cob_time_constant.utc_offset;
		}

		/* set day_of_week, day_of_year, is_daylight_saving_time, if necessary */
		if (needs_calculation) {
			time_t		t;
			struct tm 	*tmptr;
			/* allocate tmptr (needs a correct time) */
			time (&t);
			tmptr = localtime (&t);
			tmptr->tm_isdst = -1;
			tmptr->tm_sec	= cb_time.second;
			tmptr->tm_min	= cb_time.minute;
			tmptr->tm_hour	= cb_time.hour;
			tmptr->tm_year	= cb_time.year - 1900;
			tmptr->tm_mon	= cb_time.month - 1;
			tmptr->tm_mday	= cb_time.day_of_month;
			tmptr->tm_wday	= -1;
			tmptr->tm_yday	= -1;
			(void)mktime(tmptr);
			cb_time.day_of_week = one_indexed_day_of_week_from_monday (tmptr->tm_wday);
			cb_time.day_of_year = tmptr->tm_yday + 1;
			cb_time.is_daylight_saving_time = tmptr->tm_isdst;
		}
	}

	/* Leap seconds ? */
	if (cb_time.second >= 60) {
		cb_time.second = 59;
	}

	return cb_time;
}

int
cob_set_date_from_epoch (struct cob_time *cb_time, const char *config)
{
	struct tm	*tmptr;
	time_t		t = 0;
	long long	seconds = 0;
	unsigned char *p = (unsigned char *)config;

	while (isdigit (*p)) {
		seconds = seconds * 10 + *p - '0';
		p++;
	}
	if (*p != 0 || seconds > 253402300799) {
		/* The value (as a unix timestamp) corresponds to date
		   "Dec 31 9999 23:59:59 UTC", which is the latest date that __DATE__
		   and __TIME__ can store.  */
		return 1;
	}

	/* allocate tmptr for epoch */
	tmptr = localtime (&t);
	/* set seconds, minutes, hours and big days */
	tmptr->tm_sec = seconds % 60;
	seconds /= 60;
	tmptr->tm_min = seconds % 60;
	seconds /= 60;
	tmptr->tm_hour = seconds % 24;
	seconds /= 24;
	tmptr->tm_mday = (int)seconds;
	tmptr->tm_isdst = -1;

	/* normalize if needed (definitely for epoch, but also for example 30 Feb
		to be changed to correct march date),
		set tm_wday, tm_yday and tm_isdst */
	if (mktime (tmptr) == -1) {
		return 1;
	}

	cb_time->year = tmptr->tm_year + 1900;
	cb_time->month = tmptr->tm_mon + 1;
	cb_time->day_of_month = tmptr->tm_mday;
	cb_time->hour = tmptr->tm_hour;
	cb_time->minute = tmptr->tm_min;
	cb_time->second = tmptr->tm_sec;
	cb_time->nanosecond = -1;

	cb_time->day_of_week = tmptr->tm_wday + 1;
	cb_time->day_of_year = tmptr->tm_yday + 1;
	cb_time->is_daylight_saving_time = tmptr->tm_isdst;
	return 0;
}

static void
check_current_date ()
{
	int		yr, mm, dd, hh, mi, ss, ns;
	int		offset = 9999;
	int		i, j, ret;
	time_t		t;
	struct tm	*tmptr;
	char	iso_timezone[7] = { 0 };
	char	nanoseconds[10];

	if (cobsetptr == NULL
	 || cobsetptr->cob_date == NULL) {
		return;
	}

	j = 0;

	/* skip quotes and space-characters */
	while (cobsetptr->cob_date[j] == '\''
		|| cobsetptr->cob_date[j] == '"'
	    || isspace((unsigned char)cobsetptr->cob_date[j])) {
		j++;
	}

	/* extract epoch, if specified */
	if (cobsetptr->cob_date[j] == '@') {
		/* @sssssssss   seconds since epoch */
		ret = cob_set_date_from_epoch (&cobsetptr->cob_time_constant, cobsetptr->cob_date + j + 1);
		if (ret) {
			cob_runtime_warning (_("COB_CURRENT_DATE '%s' is invalid"), cobsetptr->cob_date);
		}
		return;
	}

	yr = mm = dd = hh = mi = ss = ns = -1;
	ret = 0;

	/* extract date */
	if (cobsetptr->cob_date[j] != 0) {
		yr = 0;
		for (i = 0; cobsetptr->cob_date[j] != 0; j++) {
			if (isdigit ((unsigned char)cobsetptr->cob_date[j])) {
			 	yr = yr * 10 + COB_D2I (cobsetptr->cob_date[j]);
			} else {
				break;
			}
			if (++i == 4) {
				j++;
				break;
			}
		}
		if (i != 2 && i != 4) {
			if (cobsetptr->cob_date[j] == 'Y') {
				while (cobsetptr->cob_date[j] == 'Y') j++;
			} else {
				ret = 1;
			}
			yr = -1;
		} else if (yr < 100) {
			yr += 2000;
		}
		while (cobsetptr->cob_date[j] == '/'
		    || cobsetptr->cob_date[j] == '-') {
			j++;
		}
	}
	if (cobsetptr->cob_date[j] != 0) {
		mm = 0;
		for (i = 0; cobsetptr->cob_date[j] != 0; j++) {
			if (isdigit ((unsigned char)cobsetptr->cob_date[j])) {
				mm = mm * 10 + COB_D2I (cobsetptr->cob_date[j]);
			} else {
				break;
			}
			if (++i == 2) {
				j++;
				break;
			}
		}
		if (i != 2) {
			if (cobsetptr->cob_date[j] == 'M') {
				while (cobsetptr->cob_date[j] == 'M') j++;
			} else {
				ret = 1;
			}
			mm = -1;
		} else if (mm < 1 || mm > 12) {
			ret = 1;
		}
		while (cobsetptr->cob_date[j] == '/'
		    || cobsetptr->cob_date[j] == '-') {
			j++;
		}
	}
	if (cobsetptr->cob_date[j] != 0) {
		dd = 0;
		for (i = 0; cobsetptr->cob_date[j] != 0; j++) {
			if (isdigit ((unsigned char)cobsetptr->cob_date[j])) {
				dd = dd * 10 + COB_D2I (cobsetptr->cob_date[j]);
			} else {
				break;
			}
			if (++i == 2) {
				j++;
				break;
			}
		}
		if (i != 2) {
			if (cobsetptr->cob_date[j] == 'D') {
				while (cobsetptr->cob_date[j] == 'D') j++;
			} else {
				ret = 1;
			}
			dd = -1;
		} else if (dd < 1 || dd > 31) {
			ret = 1;
		}
	}

	/* extract time */
	if (cobsetptr->cob_date[j] != 0) {
		hh = 0;
		while (isspace ((unsigned char)cobsetptr->cob_date[j])) j++;
		for (i = 0; cobsetptr->cob_date[j] != 0; j++) {
			if (isdigit ((unsigned char)cobsetptr->cob_date[j])) {
				hh = hh * 10 + COB_D2I (cobsetptr->cob_date[j]);
			} else {
				break;
			}
			if (++i == 2) {
				j++;
				break;
			}
		}

		if (i != 2) {
			if (cobsetptr->cob_date[j] == 'H') {
				while (cobsetptr->cob_date[j] == 'H') j++;
			} else {
				ret = 1;
			}
			hh = -1;
		} else if (hh > 23) {
			ret = 1;
		}
		while (cobsetptr->cob_date[j] == ':'
		    || cobsetptr->cob_date[j] == '-')
			j++;
	}
	if (cobsetptr->cob_date[j] != 0) {
		mi = 0;
		for (i = 0; cobsetptr->cob_date[j] != 0; j++) {
			if (isdigit ((unsigned char)cobsetptr->cob_date[j])) {
				mi = mi * 10 + COB_D2I (cobsetptr->cob_date[j]);
			} else {
				break;
			}
			if (++i == 2) {
				j++;
				break;
			}
		}
		if (i != 2) {
			if (cobsetptr->cob_date[j] == 'M') {
				while (cobsetptr->cob_date[j] == 'M') j++;
			} else {
				ret = 1;
			}
			mi = -1;
		} else if (mi > 59) {
			ret = 1;
		}
		while (cobsetptr->cob_date[j] == ':'
		    || cobsetptr->cob_date[j] == '-') {
			j++;
		}
	}

	if (cobsetptr->cob_date[j] != 0
	 && cobsetptr->cob_date[j] != 'Z'
	 && cobsetptr->cob_date[j] != '+'
	 && cobsetptr->cob_date[j] != '-') {
		ss = 0;
		for (i = 0; cobsetptr->cob_date[j] != 0; j++) {
			if (isdigit ((unsigned char)cobsetptr->cob_date[j])) {
				ss = ss * 10 + COB_D2I (cobsetptr->cob_date[j]);
			} else {
				break;
			}
			if (++i == 2) {
				j++;
				break;
			}
		}
		if (i != 2) {
			if (cobsetptr->cob_date[j] == 'S') {
				while (cobsetptr->cob_date[j] == 'S') j++;
			} else {
				ret = 1;
			}
			ss = -1;
		/* leap second would be 60 */
		} else if (ss > 60) {
			ret = 1;
		}
	}

	/* extract nanoseconds */
	if (cobsetptr->cob_date[j] != 0
	 && cobsetptr->cob_date[j] != 'Z'
	 && cobsetptr->cob_date[j] != '+'
	 && cobsetptr->cob_date[j] != '-') {
		ns = 0;
		if (cobsetptr->cob_date[j] == '.'
		 || cobsetptr->cob_date[j] == ':') {
			j++;
		}
		strcpy (nanoseconds, "000000000");
		for (i=0; cobsetptr->cob_date[j] != 0; j++) {
			if (isdigit ((unsigned char)cobsetptr->cob_date[j])) {
				nanoseconds[i] = cobsetptr->cob_date[j];
			} else {
				break;
			}
			if (++i == 9) {
				j++;
				break;
			}
		}
		ns = atoi(nanoseconds);
	}

	/* extract UTC offset */
	if (cobsetptr->cob_date[j] == 'Z') {
		offset = 0;
		iso_timezone[0] = 'Z';
	} else if (cobsetptr->cob_date[j] == '+'
	        || cobsetptr->cob_date[j] == '-') {
		int len = snprintf (&iso_timezone[0], 7, "%s", cobsetptr->cob_date + j);
		if (len == 3) {
			memcpy (iso_timezone + 3, "00", 3);
		} else
		if (len >= 5 && iso_timezone[3] == ':') {
			snprintf (&iso_timezone[3], 3, "%s", cobsetptr->cob_date + j + 4);
			len--;
		}
		if (len > 5) {
			ret = 1;
		}
		for (i=1; i < 5 && iso_timezone[i] != 0; i++) {
			if (!isdigit ((unsigned char)iso_timezone[i])) {
				break;
			}
		}
		i--;
		if (i == 4) {
			offset = COB_D2I (iso_timezone[1]) * 60 * 10
				+ COB_D2I (iso_timezone[2]) * 60
				+ COB_D2I (iso_timezone[3]) * 10
				+ COB_D2I (iso_timezone[4]);
			if (iso_timezone[0] == '-') {
				offset *= -1;
			}
		} else {
			ret = 1;
			iso_timezone[0] = '\0';
		}
	}

	if (ret != 0) {
		cob_runtime_warning (_("COB_CURRENT_DATE '%s' is invalid"), cobsetptr->cob_date);
		return;
	}

	/* get local time, allocate tmptr */
	time (&t);
	tmptr = localtime (&t);
	/* override given parts in time */
	if (ss != -1) {
		tmptr->tm_sec	= ss;
	}
	if (mi != -1) {
		tmptr->tm_min	= mi;
	}
	if (hh != -1) {
		tmptr->tm_hour	= hh;
	}
	if (yr != -1) {
		tmptr->tm_year	= yr - 1900;
	}
	if (mm != -1) {
		tmptr->tm_mon	= mm - 1;
	}
	if (dd != -1) {
		tmptr->tm_mday	= dd;
	}

	tmptr->tm_isdst = -1;

	/* normalize if needed (for example 30 Feb to be changed to
	   correct march date), set tm_wday, tm_yday and tm_isdst */
	(void) mktime (tmptr);

	/* set datetime constant */
	if (yr != -1) {
		cobsetptr->cob_time_constant.year = tmptr->tm_year + 1900;
	} else {
		cobsetptr->cob_time_constant.year = -1;
	}
	if (mm != -1) {
		cobsetptr->cob_time_constant.month = tmptr->tm_mon + 1;
	} else {
		cobsetptr->cob_time_constant.month = -1;
	}
	if (dd != -1) {
		cobsetptr->cob_time_constant.day_of_month = tmptr->tm_mday;
	} else {
		cobsetptr->cob_time_constant.day_of_month = -1;
	}

	if (hh != -1) {
		cobsetptr->cob_time_constant.hour	= tmptr->tm_hour;
	} else {
		cobsetptr->cob_time_constant.hour	= -1;
	}
	if (mi != -1) {
		cobsetptr->cob_time_constant.minute	= tmptr->tm_min;
	} else {
		cobsetptr->cob_time_constant.minute	= -1;
	}
	if (ss != -1) {
		cobsetptr->cob_time_constant.second	= tmptr->tm_sec;
	} else {
		cobsetptr->cob_time_constant.second	= -1;
	}
	cobsetptr->cob_time_constant.nanosecond	= ns;

	/* the following are only set in "current" instances, not in the constant */
	cobsetptr->cob_time_constant.day_of_week = -1;
	cobsetptr->cob_time_constant.day_of_year = -1;
	cobsetptr->cob_time_constant.is_daylight_saving_time = -1;

	if (iso_timezone[0] != '\0') {
		cobsetptr->cob_time_constant.offset_known = 1;
		cobsetptr->cob_time_constant.utc_offset = offset;
	} else {
		cobsetptr->cob_time_constant.offset_known = 0;
		cobsetptr->cob_time_constant.utc_offset = 0;
	}
}

/* Extended ACCEPT/DISPLAY */

void
cob_accept_date (cob_field *field)
{
	struct cob_time	time;
	char		buff[16]; /* 16: make the compiler happy as "unsigned short" *could*
						         have more digits than we "assume" */

	time = cob_get_current_datetime (DTR_DATE);

	snprintf(buff, sizeof (buff), "%2.2d%2.2d%2.2d",
		(cob_u16_t) time.year % 100,
		(cob_u16_t) time.month,
		(cob_u16_t) time.day_of_month);
	cob_move_intermediate (field, buff, (size_t)6);
}

void
cob_accept_date_yyyymmdd (cob_field *field)
{
	struct cob_time	time;
	char		buff[16]; /* 16: make the compiler happy as "unsigned short" *could*
						         have more digits than we "assume" */

	time = cob_get_current_datetime (DTR_DATE);

	snprintf (buff, sizeof (buff), "%4.4d%2.2d%2.2d",
		(cob_u16_t) time.year,
		(cob_u16_t) time.month,
		(cob_u16_t) time.day_of_month);
	cob_move_intermediate (field, buff, (size_t)8);
}

void
cob_accept_day (cob_field *field)
{
	struct cob_time	time;
	char		buff[11]; /* 11: make the compiler happy as "unsigned short" *could*
						         have more digits than we "assume" */

	time = cob_get_current_datetime (DTR_DATE);
	snprintf (buff, sizeof (buff), "%2.2d%3.3d",
		(cob_u16_t) time.year % 100,
		(cob_u16_t) time.day_of_year);
	cob_move_intermediate (field, buff, (size_t)5);
}

void
cob_accept_day_yyyyddd (cob_field *field)
{
	struct cob_time	time;
	char		buff[11]; /* 11: make the compiler happy as "unsigned short" *could*
						         have more digits than we "assume" */

	time = cob_get_current_datetime (DTR_DATE);
	snprintf (buff, sizeof (buff), "%4.4d%3.3d",
		(cob_u16_t) time.year,
		(cob_u16_t) time.day_of_year);
	cob_move_intermediate (field, buff, (size_t)7);
}

void
cob_accept_day_of_week (cob_field *field)
{
	struct cob_time	time;
	unsigned char		day;

	time = cob_get_current_datetime (DTR_DATE);
	day = (unsigned char)(time.day_of_week + '0');
	cob_move_intermediate (field, &day, (size_t)1);
}

void
cob_accept_time (cob_field *field)
{
	struct cob_time	time;
	char		buff[21]; /* 11: make the compiler happy as "unsigned short" *could*
						         have more digits than we "assume" */

	if (field->size > 6) {
		time = cob_get_current_datetime (DTR_FULL);
	} else {
		time = cob_get_current_datetime (DTR_TIME_NO_NANO);
	}
	snprintf (buff, sizeof (buff), "%2.2d%2.2d%2.2d%2.2d",
		(cob_u16_t) time.hour,
		(cob_u16_t) time.minute,
		(cob_u16_t) time.second,
		(cob_u16_t) (time.nanosecond / 10000000));

	cob_move_intermediate (field, buff, (size_t)8);
}

void
cob_display_command_line (cob_field *f)
{
	if (commlnptr) {
		cob_free (commlnptr);
	}
	commlnptr = cob_malloc (f->size + 1U);
	commlncnt = f->size;
	memcpy (commlnptr, f->data, commlncnt);
}

void
cob_accept_command_line (cob_field *f)
{
	char	*buff;
	size_t	i;
	size_t	size;
	size_t	len;

	if (commlncnt) {
		cob_move_intermediate (f, commlnptr, commlncnt);
		return;
	}

	if (cob_argc <= 1) {
		cob_move_intermediate (f, " ", (size_t)1);
		return;
	}

	size = 0;
	for (i = 1; i < (size_t)cob_argc; ++i) {
		size += (strlen (cob_argv[i]) + 1);
		if (size > f->size) {
			break;
		}
	}
	buff = cob_malloc (size);
	buff[0] = ' ';
	size = 0;
	for (i = 1; i < (size_t)cob_argc; ++i) {
		len = strlen (cob_argv[i]);
		memcpy (buff + size, cob_argv[i], len);
		size += len;
		if (i != (size_t)cob_argc - 1U) {
			buff[size++] = ' ';
		}
		if (size > f->size) {
			break;
		}
	}
	cob_move_intermediate (f, buff, size);
	cob_free (buff);
}

/* Argument number */

void
cob_display_arg_number (cob_field *f)
{
	int		n;
	cob_field_attr	attr;
	cob_field	temp;

	temp.size = 4;
	temp.data = (unsigned char *)&n;
	temp.attr = &attr;
	COB_ATTR_INIT (COB_TYPE_NUMERIC_BINARY, 9, 0, 0, NULL);
	cob_move (f, &temp);
	if (n < 0 || n >= cob_argc) {
		cob_set_exception (COB_EC_IMP_DISPLAY);
		return;
	}
	current_arg = n;
}

void
cob_accept_arg_number (cob_field *f)
{
	int		n;
	cob_field_attr	attr;
	cob_field	temp;

	n = cob_argc - 1;
	temp.size = 4;
	temp.data = (unsigned char *)&n;
	temp.attr = &attr;
	COB_ATTR_INIT (COB_TYPE_NUMERIC_BINARY, 9, 0, 0, NULL);
	cob_move (&temp, f);
}

void
cob_accept_arg_value (cob_field *f)
{
	if (current_arg >= cob_argc) {
		cob_set_exception (COB_EC_IMP_ACCEPT);
		return;
	}
	cob_move_intermediate (f, cob_argv[current_arg],
		    strlen (cob_argv[current_arg]));
	current_arg++;
}

/* Environment variable handling */

#ifdef	_MSC_VER
/* _MSC does *NOT* have `setenv` (!)
   But as the handling of the fallback `putenv` is different in POSIX and _MSC
   (POSIX stores no duplicate of `putenv`, where _MSC does), we pretend to
   have support for `setenv` and define it here with the same behaviour: */

static COB_INLINE COB_A_INLINE int
setenv (const char *name, const char *value, int overwrite) {
	/* remark: _putenv_s does always overwrite, add a check for overwrite = 1 if necessary later */
	COB_UNUSED (overwrite);
	return _putenv_s (name,value);
}
static COB_INLINE COB_A_INLINE int
unsetenv (const char *name) {
	return _putenv_s (name,"");
}
#endif

/* set entry into environment, with/without overwriting existing values */
int
cob_setenv (const char *name, const char *value, int overwrite) {
#if defined (HAVE_SETENV) && HAVE_SETENV
	return setenv (name, value, overwrite);
#else
	char	*env;
	size_t	len;

	COB_UNUSED (overwrite);
	len = strlen (name) + strlen (value) + 2U;
	env = cob_fast_malloc (len);
	sprintf (env, "%s=%s", name, value);
	return putenv (env);
#endif
}

/* remove entry from environment */
int
cob_unsetenv (const char *name) {
#if defined(HAVE_SETENV) && HAVE_SETENV
	unsetenv (name);
	return 0;
#else
	char	*env;

	env = cob_fast_malloc (strlen (name) + 2U);
	sprintf (env, "%s=", name);
	return putenv (env);
#endif
}

/* resolve entry from environment */
char *
cob_getenv_direct (const char *name) {
	return getenv (name);
}

/* resolve entry from environment and return an allocated string copy
   --> call cob_free after use! */
char *
cob_getenv (const char *name)
{
	char	*p;

	if (name) {
		p = getenv (name);
		if (p) {
			return cob_strdup (p);
		}
	}
	return NULL;
}

int
cob_putenv (char *name)
{
	int	ret;

	if (name && strchr (name, '=')) {
		ret = putenv (cob_strdup (name));
		if (!ret) {
			cob_rescan_env_vals ();
		}
		return ret;
	}
	return -1;
}

void
cob_display_environment (const cob_field *f)
{
	size_t	i;

	if (cob_local_env_size < f->size) {
		cob_local_env_size = f->size;
		if (cob_local_env) {
			cob_free (cob_local_env);
		}
		cob_local_env = cob_malloc (cob_local_env_size + 1U);
	}
	cob_field_to_string (f, cob_local_env, cob_local_env_size);
	if (cobsetptr->cob_env_mangle) {
		const size_t len = strlen (cob_local_env);
		for (i = 0; i < len; ++i) {
			if (!isalnum ((int)cob_local_env[i])) {
				cob_local_env[i] = '_';
			}
		}
	}
}

void
cob_display_env_value (const cob_field *f)
{
	char	*env2;
	int		ret;

	if (!cob_local_env) {
		cob_set_exception (COB_EC_IMP_DISPLAY);
		return;
	}
	if (!*cob_local_env) {
		cob_set_exception (COB_EC_IMP_DISPLAY);
		return;
	}
	env2 = cob_malloc (f->size + 1U);
	cob_field_to_string (f, env2, f->size);
	ret = cob_setenv (cob_local_env, env2, 1);
	cob_free (env2);
	if (ret != 0) {
		cob_set_exception (COB_EC_IMP_DISPLAY);
		return;
	}
	/* Rescan term/screen variables */
	cob_rescan_env_vals ();
}

void
cob_set_environment (const cob_field *f1, const cob_field *f2)
{
	cob_display_environment (f1);
	cob_display_env_value (f2);
}

void
cob_get_environment (const cob_field *envname, cob_field *envval)
{
	const char	*p;
	char		*buff;
	size_t		size;

	if (envname->size == 0 || envval->size == 0) {
		cob_set_exception (COB_EC_IMP_ACCEPT);
		return;
	}

	buff = cob_malloc (envname->size + 1U);
	cob_field_to_string (envname, buff, envname->size);
	if (cobsetptr->cob_env_mangle) {
		const size_t len = strlen (buff);
		for (size = 0; size < len; ++size) {
			if (!isalnum ((int)buff[size])) {
				buff[size] = '_';
			}
		}
	}
	p = getenv (buff);
	if (!p) {
		cob_set_exception (COB_EC_IMP_ACCEPT);
		p = " ";
	}
	cob_move_intermediate (envval, p, strlen (p));
	cob_free (buff);
}

void
cob_accept_environment (cob_field *f)
{
	const char *p = NULL;

	if (cob_local_env) {
		p = getenv (cob_local_env);
	}
	if (!p) {
		cob_set_exception (COB_EC_IMP_ACCEPT);
		p = " ";
	}
	cob_move_intermediate (f, p, strlen (p));
}

void
cob_chain_setup (void *data, const size_t parm, const size_t size)
{
	size_t	len;

	/* only set if given on command-line, otherwise use normal
	   program internal initialization */
	if (parm <= (size_t)cob_argc - 1) {
		memset (data, ' ', size);
		len = strlen (cob_argv[parm]);
		if (len <= size) {
			memcpy (data, cob_argv[parm], len);
		} else {
			memcpy (data, cob_argv[parm], size);
		}
	}
}

void
cob_continue_after (cob_field *decimal_seconds)
{
	cob_s64_t	nanoseconds = get_sleep_nanoseconds_from_seconds (decimal_seconds);

	if (nanoseconds < 0) {
		/* TODO: current COBOL 20xx change proposal
		   specifies EC-CONTINUE-LESS-THAN-ZERO (NF) here... */
		return;
	}
	internal_nanosleep (nanoseconds, 0);
}

/* ALLOCATE statement
   dataptr    -> used for ALLOCATE identifier only, NULL otherwise
   retptr     -> RETURNING ret, may be NULL
   initialize -> used for ALLOCATE CHARACTERS only, may be NULL
*/
void
cob_allocate (unsigned char **dataptr, cob_field *retptr,
	      cob_field *sizefld, cob_field *initialize)
{
	const cob_s64_t		fsize = cob_get_llint (sizefld);
	void			*mptr = NULL;

	cobglobptr->cob_exception_code = 0;
	if (fsize > COB_MAX_ALLOC_SIZE) {
		cob_set_exception (COB_EC_STORAGE_IMP);
	} else if (fsize > 0) {
		const size_t memsize = (size_t)fsize;
		if (initialize
		 && initialize->data[0] == 0
		 && COB_FIELD_TYPE (initialize) == COB_TYPE_ALPHANUMERIC_ALL) {
			mptr = calloc (1, memsize);
		} else {
			mptr = malloc (memsize);
		}
		if (!mptr) {
			cob_set_exception (COB_EC_STORAGE_NOT_AVAIL);
		} else {
			struct cob_alloc_cache	*cache_ptr;
			if (initialize) {
				cob_field	temp;
				temp.size = memsize;
				temp.data = mptr;
				temp.attr = &const_alpha_attr;
				cob_move (initialize, &temp);
			}
#if 0
			else {
				memset (mptr, 0, memsize);
			}
#endif
			cache_ptr = cob_malloc (sizeof (struct cob_alloc_cache));
			cache_ptr->cob_pointer = mptr;
			cache_ptr->size = memsize;
			cache_ptr->next = cob_alloc_base;
			cob_alloc_base = cache_ptr;
		}
	}
	if (dataptr) {
		*dataptr = mptr;
	}
	if (retptr) {
		*(void **)(retptr->data) = mptr;
	}
}

/* FREE statement */
void
cob_free_alloc (unsigned char **ptr1, unsigned char *ptr2)
{
	struct cob_alloc_cache	*cache_ptr;
	struct cob_alloc_cache	*prev_ptr;

	cobglobptr->cob_exception_code = 0;
	cache_ptr = cob_alloc_base;
	prev_ptr = cob_alloc_base;
	if (ptr1 && *ptr1) {
		void	*vptr1;
		vptr1 = *ptr1;
		for (; cache_ptr; cache_ptr = cache_ptr->next) {
			if (vptr1 == cache_ptr->cob_pointer) {
				cob_free (cache_ptr->cob_pointer);
				if (cache_ptr == cob_alloc_base) {
					cob_alloc_base = cache_ptr->next;
				} else {
					prev_ptr->next = cache_ptr->next;
				}
				cob_free (cache_ptr);
				*ptr1 = NULL;
				return;
			}
			prev_ptr = cache_ptr;
		}
		cob_set_exception (COB_EC_STORAGE_NOT_ALLOC);
		return;
	}
	if (ptr2 && *(void **)ptr2) {
		for (; cache_ptr; cache_ptr = cache_ptr->next) {
			if (*(void **)ptr2 == cache_ptr->cob_pointer) {
				cob_free (cache_ptr->cob_pointer);
				if (cache_ptr == cob_alloc_base) {
					cob_alloc_base = cache_ptr->next;
				} else {
					prev_ptr->next = cache_ptr->next;
				}
				cob_free (cache_ptr);
				*(void **)ptr2 = NULL;
				return;
			}
			prev_ptr = cache_ptr;
		}
		cob_set_exception (COB_EC_STORAGE_NOT_ALLOC);
		return;
	}
}

#if 0 /* debug only */
void print_stat (const char *filename, struct stat sb)
{
	printf("File name:                ");
	if (filename) {
		printf ("%s\n", filename);
	} else {
		printf("- unknown -\n");
	}
	printf("File type:                ");

	switch (sb.st_mode & S_IFMT) {
#ifdef S_IFBLK
	case S_IFBLK:  printf("block device\n");            break;
#endif
#ifdef S_IFCHR
	case S_IFCHR:  printf("character device\n");        break;
#endif
	case S_IFDIR:  printf("directory\n");               break;
#ifdef S_IFIFO
	case S_IFIFO:  printf("FIFO/pipe\n");               break;
#endif
#ifdef S_IFLNK
	case S_IFLNK:  printf("symlink\n");                 break;
#endif
	case S_IFREG:  printf("regular file\n");            break;
#ifdef S_IFSOCK
	case S_IFSOCK: printf("socket\n");                  break;
#endif
	default:       printf("unknown?\n");                break;
	}

	printf("I-node number:            %ld\n", (long)sb.st_ino);

	printf("Mode:                     %lo (octal)\n",
		(unsigned long)sb.st_mode);

	printf("Link count:               %ld\n", (long)sb.st_nlink);
	printf("Ownership:                UID=%ld   GID=%ld\n",
		(long)sb.st_uid, (long)sb.st_gid);
	printf("File size:                %lld bytes\n",
		(long long)sb.st_size);
#if 0
	printf("Preferred I/O block size: %ld bytes\n",
		(long)sb.st_blksize);
	printf("Blocks allocated:         %lld\n",
		(long long)sb.st_blocks);
#endif

	printf("Last status change:       %s", ctime(&sb.st_ctime));
	printf("Last file access:         %s", ctime(&sb.st_atime));
	printf("Last file modification:   %s", ctime(&sb.st_mtime));
}
#endif

static COB_INLINE int
check_valid_dir (const char *dir)
{
	struct stat		sb;
	if (strlen (dir) > COB_NORMAL_MAX) return 1;
	if (stat (dir, &sb) || !(S_ISDIR (sb.st_mode))) return 1;

#if 0
	print_stat (dir, sb);
#endif
	
	return 0;
}

static const char *
check_valid_env_tmpdir (const char *envname)
{
	const char *dir;

	dir = getenv (envname);
	if (!dir || !dir[0]) {
		return NULL;
	}
	if (check_valid_dir (dir)) {
		cob_runtime_warning ("Temporary directory %s is invalid, adjust TMPDIR!", envname);
		(void)cob_unsetenv (envname);
		return NULL;
	}
	return dir;
}


/* return pointer to TMPDIR without trailing slash */
static const char *
cob_gettmpdir (void)
{
	static const char	*tmpdir = NULL;
	char	*tmp;

	if (tmpdir != NULL) {
		return tmpdir;
	}

	tmp = NULL;
	/* target directory, also used by most called external tools*/
	if ((tmpdir = check_valid_env_tmpdir ("TMPDIR")) == NULL
	/* OS specific temporary paths */
#ifdef	_WIN32
	 && (tmpdir = check_valid_env_tmpdir ("TEMP")) == NULL
	 && (tmpdir = check_valid_env_tmpdir ("TMP")) == NULL
	 && (tmpdir = check_valid_env_tmpdir ("USERPROFILE")) == NULL) {
#else
	 && (tmpdir = check_valid_env_tmpdir ("TMP")) == NULL
	 && (tmpdir = check_valid_env_tmpdir ("TEMP")) == NULL) {
		if (!check_valid_dir ("/tmp")) {
			tmp = cob_fast_malloc (5U);
			strcpy (tmp, "/tmp");
			tmpdir = tmp;
		}
#endif
	}
	/* fallback if still not valid: current directory */
	if (!tmpdir) {
		tmp = cob_fast_malloc (2U);
		tmp[0] = '.';
		tmp[1] = 0;
		tmpdir = tmp;
	} else {
		/* if we have a valid path: ensure there's no trailing slash */
		size_t size = strlen (tmpdir) - 1;
		if (tmpdir[size] == SLASH_CHAR) {
			tmp = (char*)cob_fast_malloc (size + 1);
			memcpy (tmp, tmpdir, size);
			tmp[size] = 0;
			tmpdir = tmp;
		}
	}
	/* ensure TMPDIR is set for called tools (which partially break hard otherwise) */
	(void)cob_setenv ("TMPDIR", tmpdir, 1);

	if (tmp) {
		cob_free ((void *)tmp);
	}

	/* get the pointer to the environment copy - as this may point to a different place -
	   store it for subsequent calls and finally return it to the caller */
	tmpdir = getenv ("TMPDIR");
	return tmpdir;
}

/* Set temporary file name */
void
cob_temp_name (char *filename, const char *ext)
{
	int pid = cob_sys_getpid ();
#ifndef HAVE_8DOT3_FILENAMES
#define TEMP_EXT_SCHEMA	"%s%ccob%d_%d%s"
#define TEMP_SORT_SCHEMA	"%s%ccobsort%d_%d"
#else
/* 8.3 allows only short names... */
#define TEMP_EXT_SCHEMA	"%s%cc%d_%d%s"
#define TEMP_SORT_SCHEMA	"%s%cs%d_%d"
	pid = pid % 9999;
#endif
	if (ext) {
		snprintf (filename, (size_t)COB_FILE_MAX, TEMP_EXT_SCHEMA,
			cob_gettmpdir (), SLASH_CHAR, pid, cob_temp_iteration, ext);
	} else {
		snprintf (filename, (size_t)COB_FILE_MAX, TEMP_SORT_SCHEMA,
			cob_gettmpdir (), SLASH_CHAR, pid, cob_temp_iteration);
	}
#undef TEMP_EXT_SCHEMA
#undef TEMP_SORT_SCHEMA
}

void
cob_incr_temp_iteration (void)
{
	cob_temp_iteration++;
}

int
cob_extern_init (void)
{
	/* can be called multiple times (MF docs say: should be done in all threads) */
	if (!cob_initialized) {
		cob_init (0, NULL);
	}
	return 0;
}

char *
cob_command_line (int flags, int *pargc, char ***pargv,
		  char ***penvp, char **pname)
{
#if	0	/* RXWRXW cob_command_line */
	char		**spenvp;
	char		*spname;
#else
	COB_UNUSED (penvp);
	COB_UNUSED (pname);
#endif

	COB_UNUSED (flags);

	if (!cob_initialized) {
		cob_fatal_error (COB_FERROR_INITIALIZED);
	}
	if (pargc && pargv) {
		cob_argc = *pargc;
		cob_argv = *pargv;
	}

#if	0	/* RXWRXW cob_command_line */
	if (penvp) {
		spenvp = *penvp;
	}
	if (pname) {
		spname = *pname;
	}
#endif

	/* What are we supposed to return here? */
	return NULL;
}

int
cob_tidy (void)
{
	if (!cob_initialized) {
		exit_code = -1;
		return 1;
	}
	exit_code = 0;
	call_exit_handlers_and_terminate ();
	return 0;
}

/* System routines */

int
cob_sys_exit_proc (const void *dispo, const void *pptr)
{
	struct exit_handlerlist *hp;
	struct exit_handlerlist *h;
	unsigned char	install_flag;
	/* only initialized to silence -Wmaybe-uninitialized */
	unsigned char	priority = 0;
	int			(**p)(void);

	COB_CHK_PARMS (CBL_EXIT_PROC, 2);

#if 0	/* TODO: take care of ACU variant:
	   pptr is not an already resolved entry point
	   but a name which is to be cob_resolve'd (at use-time);
	   maybe resolve here and return -1 if not possible;
	   furthermore the second parameter is a mixed priority + install_flag */
	if (something) {
		const char *name = (char *)pptr;
		pptr = cob_resolve_cobol (name, 0, 0);

		if (!p) {
			return -1;
		}

		priority = *(unsigned char *)dispo;
		if (priority == 254) {
			install_flag = 1;
		} else if (priority == 255) {
			install_flag = 2;
		} else {
			install_flag = 3;
		}

	} else {
#endif
		memcpy (&p, &pptr, sizeof (void *));

		if (!p || !*p) {
			return -1;
		}

		install_flag = *(unsigned char*)dispo;
		switch (install_flag) {
		case 0:
			priority = 64;
			break;
		case 1:
		case 2:
			/* remove / query - no need to check priority */
			break;
		case 3:
			/* set with explicit priority */
			priority = *((unsigned char*)pptr + sizeof (void*));
			if (priority > 127) {
				priority = 64;
			}
			break;
		default:
			return -1;
		}
#if 0
	}
#endif

	hp = NULL;
	h = exit_hdlrs;
	/* Search handler, remove if not function 2  */
	while (h != NULL) {
		if (h->proc == *p) {
			switch (install_flag) {
			case 2:
			/* Return priority of installed handler */
#if 0	/* TODO: take care of ACU variant: priority in return */
				if (something) {
					return priority;
				}
#endif
				*((unsigned char*)pptr + sizeof (void*)) = h->priority;
				return 0;
			case 0:
			case 3:
				if (priority == h->priority) {
					return -1;
				}
				break;
			default:
				break;
			}
			if (hp != NULL) {
				hp->next = h->next;
			} else {
				exit_hdlrs = h->next;
			}
			cob_free (h);
			/* Remove handler --> done */
			if (install_flag == 1) {
				return 0;
			}
			break;
		}
		hp = h;
		h = h->next;
	}
	if (install_flag == 1
	 || install_flag == 2) {
		/* deete or query priority with not available */
#if 0	/* TODO: take care of ACU variant: priority 255 = not available */
		if (something) {
			return 255;
		}
#endif
		return -1;
	}
	h = cob_malloc (sizeof (struct exit_handlerlist));
	h->next = exit_hdlrs;
	h->proc = *p;
	h->priority = priority;
	exit_hdlrs = h;
	return 0;
}

int
cob_sys_error_proc (const void *dispo, const void *pptr)
{
	struct handlerlist	*hp;
	struct handlerlist	*h;
	const unsigned char	*x;
	int			(**p) (char *s);

	COB_CHK_PARMS (CBL_ERROR_PROC, 2);

#if 0	/* TODO: take care of ACU variant:
	   pptr is not an already resolved entry point
	   but a name which is to be cob_resolve'd (at use-time);
	   maybe resolve here and return -1 if not possible */
	if (something) {
		const char *name = (char *)pptr;
		pptr = cob_resolve_cobol (name, 0, 0);
		if (!p) {
			return -1;
		}
	} else {
#endif
		memcpy (&p, &pptr, sizeof (void *));
		if (!p || !*p) {
			return -1;
		}
#if 0
}
#endif

	hp = NULL;
	h = hdlrs;
	/* Search for existing handler */
	while (h != NULL) {
		if (h->proc == *p) {
			break;
		}
		hp = h;
		h = h->next;
	}
	x = dispo;
	if (*x != 0) {
		/* Remove handler */
		if (h != NULL) {
			if (hp != NULL) {
				hp->next = h->next;
			} else {
				hdlrs = h->next;
			}
			cob_free (h);
		}
	} else {
		if (h == NULL) {
			/* insert handler */
			h = cob_malloc (sizeof (struct handlerlist));
			h->next = hdlrs;
			h->proc = *p;
			hdlrs = h;
		} else {
#if 0	/* TODO: take care of ACU variant: placing it first */
			if (something) {
				if (hp != NULL) {
					hp->next = h->next;
				}
				h->next = hdlrs;
				hdlrs = h;
			} else {
				/* MF-Variant: when already existing: do nothing */
				return 0;
			}
#else
			/* MF-Variant: when already existing: do nothing */
			return 0;
#endif
		}
	}
	return 0;
}

int
cob_sys_runtime_error_proc (const void *err_flags, const void *err_msg)
{
	const char *msg = (const char*)err_msg;

	COB_CHK_PARMS (CBL_RUNTIME_ERROR, 2);
	COB_UNUSED (err_flags);

	if (msg && msg[0]) {
		cob_runtime_error ("%s: %s",
			_("Program abandoned at user request"), msg);
	} else {
		cob_runtime_error ("%s",
			_("Program abandoned at user request"));
	}
	cob_hard_failure ();
}

int
cob_sys_system (const void *cmdline)
{
	COB_CHK_PARMS (SYSTEM, 1);

	if (COB_MODULE_PTR->cob_procedure_params[0]) {
		const char *cmd = cmdline;
		size_t		i = COB_MODULE_PTR->cob_procedure_params[0]->size;
		/* FIXME: if caller wasn't COBOL then size must be evaluated by strlen */
		i--;
		do {
			if (cmd[i] != ' ' && cmd[i] != 0) {
				break;
			}
		} while (--i != 0);
		if (i > 0) {
			char	*command;
			/* LCOV_EXCL_START */
			if (i > COB_MEDIUM_MAX) {
				cob_runtime_warning (_("parameter to SYSTEM call is larger than %d characters"),
					COB_MEDIUM_MAX);
				return 1;
			}
			/* LCOV_EXCL_STOP */
#ifdef _WIN32
			/* All known _WIN32 implementations use MSVCRT's system()
			   which passes the given commandline as parameter to "cmd /k".
			   Because "of compatibility" this checks if you have a
			   leading and trailing " and if yes simply removes them (!).
			   Check if this is the case and if it is handled already
			   by an *extra* pair of quotes, otherwise add these...
			   This fixes CALL 'SYSTEM' USING '"someprog" "opt"' being
			   executed as 'someprog" "opt'.
			*/
			if (i > 2 && cmd[0] == '"' && cmd[i] == '"'
			&& (cmd[1] != '"' || cmd[i - 1] != '"')) {
				command = cob_malloc (i + 4);
				command[0] = '"';
				memcpy (command + 1, cmd, i + 1);
				command[i + 1] = '"';
			} else {
#endif /* _WIN32 */
				command = cob_malloc (i + 2);
				memcpy (command, cmd, i + 1);
#ifdef _WIN32
			}
#endif
			{
				int status;
				if (cobglobptr->cob_screen_initialized) {
					cob_screen_set_mode (0);
				}
				/* note: if the command cannot be executed _WIN32 always returns 1
				   while GNU/Linux returns -1 */
				status = system (command);
				if (cobglobptr->cob_screen_initialized) {
					cob_screen_set_mode (1U);
				}
#ifdef	WIFSIGNALED
				if (WIFSIGNALED (status)) {
					int sig = WTERMSIG (status);
					const char *signal_name = cob_get_sig_name (sig);
					cob_runtime_warning (_("external process \"%s\" ended with signal %s (%d)"),
						command, signal_name, sig);
				}
#endif
				cob_free (command);
#if 0	/* possibly do this, but only if explicit asked for via a new runtime configuration
		   as at least MicroFocus always returns all bytes here;
		   from its docs it _looks_ like ACU only returns the lower bytes ... */
#ifdef WEXITSTATUS
				if (WIFEXITED (status)) {
					status = WEXITSTATUS (status);
				}
#endif
#endif
				return status;
			}
		}
	}
	return 1;
}

/**
* Return some hosted C variables, argc, argv, stdin, stdout, stderr.
*/
int
cob_sys_hosted (void *p, const void *var)
{
	const char		*name = var;
	cob_u8_ptr		data = p;
	size_t			i;

	COB_CHK_PARMS (CBL_GC_HOSTED, 2);

	if (!data) {
		return 1;
	}

	if (COB_MODULE_PTR->cob_procedure_params[1]) {
		i = (int)COB_MODULE_PTR->cob_procedure_params[1]->size;
		if ((i == 4) && !strncmp (name, "argc", 4)) {
			*((int *)data) = cob_argc;
			return 0;
		}
		if ((i == 4) && !strncmp (name, "argv", 4)) {
			*((char ***)data) = cob_argv;
			return 0;
		}
		if ((i == 5) && !strncmp (name, "stdin", 5)) {
			*((FILE **)data) = stdin;
			return 0;
		}
		if ((i == 6) && !strncmp (name, "stdout", 6)) {
			*((FILE **)data) = stdout;
			return 0;
		}
		if ((i == 6) && !strncmp (name, "stderr", 6)) {
			*((FILE **)data) = stderr;
			return 0;
		}
		if ((i == 5) && !strncmp (name, "errno", 5)) {
			*((int **)data) = &errno;
			return 0;
		}
#if defined (HAVE_TIMEZONE)
		if ((i == 6) && !strncmp (name, "tzname", 6)) {
			/* Recheck: bcc raises "suspicious pointer conversion */
			*((char ***)data) = tzname;
			return 0;
		}
		if ((i == 8) && !strncmp (name, "timezone", 8)) {
			*((long *)data) = timezone;
			return 0;
		}
		if ((i == 8) && !strncmp (name, "daylight", 8)) {
			*((int *)data) = daylight;
			return 0;
		}
#endif /* HAVE_TIMEZONE */
	}
	return 1;
}

int
cob_sys_and (const void *p1, void *p2, const int length)
{
	const cob_u8_ptr	data_1;
	cob_u8_ptr		data_2;
	size_t			n;

	COB_UNUSED (p1);
	COB_UNUSED (p2);
	COB_CHK_PARMS (CBL_AND, 3);
	data_1 = cob_get_param_data (1);
	data_2 = cob_get_param_data (2);

	if (length <= 0
	 || data_1 == NULL
	 || data_2 == NULL) {
		return 0;
	}
	for (n = 0; n < (size_t)length; ++n) {
		data_2[n] &= data_1[n];
	}
	return 0;
}

int
cob_sys_or (const void *p1, void *p2, const int length)
{
	const cob_u8_ptr	data_1;
	cob_u8_ptr		data_2;
	size_t			n;

	COB_UNUSED (p1);
	COB_UNUSED (p2);
	COB_CHK_PARMS (CBL_OR, 3);
	data_1 = cob_get_param_data (1);
	data_2 = cob_get_param_data (2);

	if (length <= 0
	 || data_1 == NULL
	 || data_2 == NULL) {
		return 0;
	}
	for (n = 0; n < (size_t)length; ++n) {
		data_2[n] |= data_1[n];
	}
	return 0;
}

int
cob_sys_nor (const void *p1, void *p2, const int length)
{
	const cob_u8_ptr	data_1;
	cob_u8_ptr		data_2;
	size_t			n;

	COB_UNUSED (p1);
	COB_UNUSED (p2);
	COB_CHK_PARMS (CBL_NOR, 3);

	data_1 = cob_get_param_data (1);
	data_2 = cob_get_param_data (2);

	if (length <= 0
	 || data_1 == NULL
	 || data_2 == NULL) {
		return 0;
	}
	for (n = 0; n < (size_t)length; ++n) {
		data_2[n] = ~(data_1[n] | data_2[n]);
	}
	return 0;
}

int
cob_sys_xor (const void *p1, void *p2, const int length)
{
	const cob_u8_ptr	data_1;
	cob_u8_ptr		data_2;
	size_t			n;

	COB_UNUSED (p1);
	COB_UNUSED (p2);
	COB_CHK_PARMS (CBL_XOR, 3);
	data_1 = cob_get_param_data (1);
	data_2 = cob_get_param_data (2);

	if (length <= 0
	 || data_1 == NULL
	 || data_2 == NULL) {
		return 0;
	}
	for (n = 0; n < (size_t)length; ++n) {
		data_2[n] ^= data_1[n];
	}
	return 0;
}

/* COBOL routine to perform for logical IMPLIES between the bits in two fields,
   storing the result in the second field */
int
cob_sys_imp (const void *p1, void *p2, const int length)
{
	const cob_u8_ptr	data_1;
	cob_u8_ptr		data_2;
	size_t			n;

	COB_UNUSED (p1);
	COB_UNUSED (p2);
	COB_CHK_PARMS (CBL_IMP, 3);

	data_1 = cob_get_param_data (1);
	data_2 = cob_get_param_data (2);

	if (length <= 0
	 || data_1 == NULL
	 || data_2 == NULL) {
		return 0;
	}
	for (n = 0; n < (size_t)length; ++n) {
		data_2[n] = (~data_1[n]) | data_2[n];
	}
	return 0;
}


/* COBOL routine to perform for logical NOT IMPLIES between the bits in two fields,
   storing the result in the second field */
int
cob_sys_nimp (const void *p1, void *p2, const int length)
{
	const cob_u8_ptr	data_1;
	cob_u8_ptr		data_2;
	size_t			n;

	COB_UNUSED (p1);
	COB_UNUSED (p2);
	COB_CHK_PARMS (CBL_NIMP, 3);

	data_1 = cob_get_param_data (1);
	data_2 = cob_get_param_data (2);

	if (length <= 0
	 || data_1 == NULL
	 || data_2 == NULL) {
		return 0;
	}
	for (n = 0; n < (size_t)length; ++n) {
		data_2[n] = data_1[n] & (~data_2[n]);
	}
	return 0;
}

/* COBOL routine to check for logical EQUIVALENCE between the bits in two fields,
   storing the result in the second field */
int
cob_sys_eq (const void *p1, void *p2, const int length)
{
	const cob_u8_ptr	data_1;
	cob_u8_ptr		data_2;
	size_t			n;

	COB_UNUSED (p1);
	COB_UNUSED (p2);
	COB_CHK_PARMS (CBL_EQ, 3);

	data_1 = cob_get_param_data (1);
	data_2 = cob_get_param_data (2);

	if (length <= 0
	 || data_1 == NULL
	 || data_2 == NULL) {
		return 0;
	}
	for (n = 0; n < (size_t)length; ++n) {
		data_2[n] = ~(data_1[n] ^ data_2[n]);
	}
	return 0;
}

/* COBOL routine to perform a logical NOT on the bits of a field */
int
cob_sys_not (void *p1, const int length)
{
	cob_u8_ptr	data_1;
	size_t		n;

	COB_UNUSED (p1);
	COB_CHK_PARMS (CBL_NOT, 2);

	data_1 = cob_get_param_data (1);

	if (length <= 0
	 || data_1 == NULL) {
		return 0;
	}
	for (n = 0; n < (size_t)length; ++n) {
		data_1[n] = ~data_1[n];
	}
	return 0;
}

/* COBOL routine to pack the least significant bits in eight bytes into a single byte */
int
cob_sys_xf4 (void *p1, const void *p2)
{
	cob_u8_ptr		data_1;
	const cob_u8_ptr	data_2;
	size_t			n;

	COB_UNUSED (p1);
	COB_UNUSED (p2);
	COB_CHK_PARMS (CBL_XF4, 2);

	data_1 = cob_get_param_data (1);
	data_2 = cob_get_param_data (2);

	if (data_1 == NULL
	 || data_2 == NULL) {
		return 0;
	}
	*data_1 = 0;
	for (n = 0; n < 8; ++n) {
		*data_1 |= (data_2[n] & 1) << (7 - n);
	}
	return 0;
}

/* COBOL routine to unpack the bits in a byte into eight bytes */
int
cob_sys_xf5 (const void *p1, void *p2)
{
	const cob_u8_ptr	data_1;
	cob_u8_ptr		data_2;
	size_t			n;

	COB_UNUSED (p1);
	COB_UNUSED (p2);
	COB_CHK_PARMS (CBL_XF5, 2);

	data_1 = cob_get_param_data (1);
	data_2 = cob_get_param_data (2);

	if (data_1 == NULL
	 || data_2 == NULL) {
		return 0;
	}
	for (n = 0; n < 8; ++n) {
		data_2[n] = (*data_1 & (1 << (7 - n))) ? 1 : 0;
	}
	return 0;
}

/* COBOL routine for different functions, including functions for
   the programmable COBOL SWITCHES:
   11: set  COBOL switches 0-7
   12: read COBOL switches 0-7
   16: return number of CALL USING parameters
*/
int
cob_sys_x91 (void *p1, const void *p2, void *p3)
{
	cob_u8_ptr		result = p1;
	const cob_u8_ptr	func = p2;
	cob_u8_ptr		parm = p3;
	unsigned char		*p;
	size_t			i;

	switch (*func) {

	/* Set switches (0-7) */
	case 11:
		p = parm;
		for (i = 0; i < 8; ++i, ++p) {
			if (*p == 0) {
				cob_switch[i] = 0;
			} else if (*p == 1) {
				cob_switch[i] = 1;
			}
		}
		/* INSPECT: MF additionally sets the ANSI DEBUG module switch */
		*result = 0;
		break;

	/* Get switches (0-7) */
	case 12:
		p = parm;
		for (i = 0; i < 8; ++i, ++p) {
			*p = (unsigned char)cob_switch[i];
		}
		/* INSPECT: MF additionally reads the ANSI DEBUG module switch */
		*result = 0;
		break;

	/* Return number of call parameters
		according to the docs this is only set for programs CALLed from COBOL
		NOT for main programs in contrast to C$NARG (cob_sys_return_args)
	*/
	case 16:
		*parm = (unsigned char)COB_MODULE_PTR->module_num_params;
		*result = 0;
		break;

	/* unimplemented function,
	   note: 46-49 may be implemented after fileio-specific merge of rw-branch
	         35 (EXEC) and 15 (program lookup) may be implemented as soon as some legacy code
			                                   shows its exact use and a test case */
	default:
		*result = 1;
		break;
	}
	return 0;
}

int
cob_sys_toupper (void *p1, const int length)
{
	cob_u8_ptr	data;
	size_t		n;

	COB_CHK_PARMS (CBL_TOUPPER, 2);
	COB_UNUSED (p1);
	data = cob_get_param_data (1);

	if (length > 0) {
		for (n = 0; n < (size_t)length; ++n) {
			data[n] = (cob_u8_t)toupper (data[n]);
		}
	}
	return 0;
}

int
cob_sys_tolower (void *p1, const int length)
{
	cob_u8_ptr	data;
	size_t		n;

	COB_CHK_PARMS (CBL_TOLOWER, 2);
	COB_UNUSED (p1);
	data = cob_get_param_data (1);

	if (length > 0) {
		for (n = 0; n < (size_t)length; ++n) {
			data[n] = (cob_u8_t)tolower (data[n]);
		}
	}
	return 0;
}

/* maximum sleep time in seconds, currently 7 days */
#define MAX_SLEEP_TIME 3600*24*7
#define NANOSECONDS_PER_MILISECOND 1000000

static cob_s64_t
get_sleep_nanoseconds (cob_field *nano_seconds) {

	cob_s64_t	nanoseconds = cob_get_llint (nano_seconds);

	if (nanoseconds < 0) {
		return -1;
	}
	if (nanoseconds >= ((cob_s64_t)MAX_SLEEP_TIME * 1000000000)) {
		return (cob_s64_t)MAX_SLEEP_TIME * 1000000000;
	} else {;
		return nanoseconds;
	}
}

static cob_s64_t
get_sleep_nanoseconds_from_seconds (cob_field *decimal_seconds) {

#define MAX_SLEEP_TIME 3600*24*7
	cob_s64_t	seconds = cob_get_llint (decimal_seconds);

	if (seconds < 0) {
		return -1;
	}
	if (seconds >= MAX_SLEEP_TIME) {
		return (cob_s64_t)MAX_SLEEP_TIME * 1000000000;
} else {
		cob_s64_t	nanoseconds;
		cob_field	temp;
		temp.size = 8;
		temp.data = (unsigned char *)&nanoseconds;
		temp.attr = &const_bin_nano_attr;
		cob_move (decimal_seconds, &temp);
		return nanoseconds;
	}
}

/* note: nsecs, while signed, is always >= 0 here */
static void
internal_nanosleep (cob_s64_t nsecs, int round_to_minmal)
{
#ifdef	HAVE_NANO_SLEEP
	struct timespec	tsec;

	tsec.tv_sec = nsecs / 1000000000;
	tsec.tv_nsec = nsecs % 1000000000;
	nanosleep (&tsec, NULL);

#elif	defined (_WIN32)
	const unsigned int	msecs = (unsigned int)(nsecs / 1000000);
	if (msecs > 0) {
		Sleep (msecs);
	}
#else
	unsigned int	seconds = (unsigned int)(nsecs * 1000 / NANOSECONDS_PER_MILISECOND);
	if (seconds == 0 && round_to_minmal) seconds = 1;
	sleep (seconds);
#endif
}

/* sleep for given number of milliseconds, rounded up if needed */
void
cob_sleep_msec (const unsigned int msecs)
{
	if (msecs == 0) return;
	internal_nanosleep (((cob_s64_t)msecs) * NANOSECONDS_PER_MILISECOND, 1);
}

/* CBL_GC_NANOSLEEP / CBL_OC_NANOSLEEP, origin: OpenCOBOL */
int
cob_sys_oc_nanosleep (const void *data)
{
	COB_UNUSED (data);
	COB_CHK_PARMS (CBL_GC_NANOSLEEP, 1);

	if (COB_MODULE_PTR->cob_procedure_params[0]) {
		cob_s64_t nsecs
			= get_sleep_nanoseconds (COB_MODULE_PTR->cob_procedure_params[0]);
		if (nsecs > 0) {
			internal_nanosleep (nsecs, 0);
		}
		return 0;
	}
	return -1;
}

/* C$SLEEP, origin: ACUCOBOL */
int
cob_sys_sleep (const void *data)
{
	COB_UNUSED (data);
	COB_CHK_PARMS (C$SLEEP, 1);

	if (COB_MODULE_PTR->cob_procedure_params[0]) {
		cob_s64_t	nanoseconds
			= get_sleep_nanoseconds_from_seconds (COB_MODULE_PTR->cob_procedure_params[0]);
		if (nanoseconds < 0) {
			/* ACUCOBOL specifies a runtime error here... */
			return -1;
		}
		internal_nanosleep (nanoseconds, 0);
		return 0;
	}
	return 0;	/* CHECKME */
}

int
cob_sys_getpid (void)
{
	if (!cob_process_id) {
		cob_process_id = (int)getpid ();
	}
	return cob_process_id;
}

int
cob_sys_fork (void)
{
 /* cygwin does not define _WIN32, but implements [slow] fork() and provides unistd.h
    MSYS defines _WIN32, provides unistd.h and not implements fork()
 */
#if defined	(HAVE_UNISTD_H) && !(defined (_WIN32))
	int	pid;
	if ((pid = fork ()) == 0 ) {
		cob_process_id = 0;	/* reset cached value */
		cob_fork_fileio(cobglobptr, cobsetptr);
		return 0;		/* child process just returns */
	}
	if (pid < 0) {			/* Some error happened */
		cob_runtime_warning (_("error '%s' during CBL_GC_FORK"), cob_get_strerror ());
		return -2;
	}
	return pid;			/* parent gets process id of child */
#else
	cob_runtime_warning (_("'%s' is not supported on this platform"), "CBL_GC_FORK");
	return -1;
#endif
}


/* wait for a pid to end and return its exit code
   error codes are returned as negative value
*/
int
cob_sys_waitpid (const void *p_id)
{
#ifdef	HAVE_SYS_WAIT_H
	int	pid, status, wait_sts;

	COB_UNUSED (p_id);

	if (COB_MODULE_PTR->cob_procedure_params[0]) {
		pid = cob_get_int (COB_MODULE_PTR->cob_procedure_params[0]);
		if (pid == cob_sys_getpid ()) {
			status = 0 - EINVAL;
			return status;
		}
		wait_sts = waitpid (pid, &status, 0);
		if (wait_sts < 0) {			/* Some error happened */
			status = 0 - errno;
			cob_runtime_warning (_("error '%s' for P%d during CBL_GC_WAITPID"),
				cob_get_strerror (), pid);
			return status;
		}
		status = WEXITSTATUS (status);
	} else {
		status = 0 - EINVAL;
	}
	return status;
#elif defined (_WIN32)
	int	pid, status;
	HANDLE process = NULL;
	DWORD ret;

	COB_UNUSED (p_id);

	status = 0;
	if (COB_MODULE_PTR->cob_procedure_params[0]) {
		pid = cob_get_int (COB_MODULE_PTR->cob_procedure_params[0]);
		if (pid == cob_sys_getpid ()) {
			status = 0 - ERROR_INVALID_DATA;
			return status;
		}
		/* get process handle with least necessary rights
		   PROCESS_QUERY_LIMITED_INFORMATION is available since OS-version Vista / Server 2008
		                                     and always leads to ERROR_ACCESS_DENIED on older systems
		   PROCESS_QUERY_INFORMATION         needs more rights
		   SYNCHRONIZE                       necessary for WaitForSingleObject
		*/
#if defined (PROCESS_QUERY_LIMITED_INFORMATION)
		process = OpenProcess (SYNCHRONIZE | PROCESS_QUERY_LIMITED_INFORMATION, FALSE, pid);
#if !defined (_MSC_VER) || COB_USE_VC2012_OR_GREATER /* only try a higher level if we possibly compile on XP/2003 */
		/* TODO: check what happens on WinXP / 2003 as PROCESS_QUERY_LIMITED_INFORMATION isn't available there */
		if (!process && GetLastError () == ERROR_ACCESS_DENIED) {
			process = OpenProcess (SYNCHRONIZE | PROCESS_QUERY_INFORMATION, FALSE, pid);
		}
#endif
#else
		process = OpenProcess (SYNCHRONIZE | PROCESS_QUERY_INFORMATION, FALSE, pid);
#endif
		/* if we don't get access to query the process' exit status try to get at least
			access to the process end (needed for WaitForSingleObject)
		*/
		if (!process && GetLastError () == ERROR_ACCESS_DENIED) {
			process = OpenProcess (SYNCHRONIZE, FALSE, pid);
			status = -2;
		}
		if (process) {
			/* wait until process exit */
			ret = WaitForSingleObject (process, INFINITE);
			if (ret == WAIT_FAILED) {
				status = 0 - GetLastError ();
			/* get exit code, if possible */
			} else if (status != -2) {
				if (!GetExitCodeProcess (process, &ret)) {
					status = 0 - GetLastError ();
				} else {
					status = (int) ret;
				}
			}
			CloseHandle (process);
		} else {
			status = 0 - GetLastError ();
		}
	} else {
		status = 0 - ERROR_INVALID_DATA;
	}
	return status;
#else
	COB_UNUSED (p_id);

	cob_runtime_warning (_("'%s' is not supported on this platform"), "CBL_GC_WAITPID");
	return -1;
#endif
}

/* set the number of arguments passed to the current program;
   works both for main programs and called sub programs
   Implemented according to ACUCOBOL-GT -> returns the number of arguments that were passed,
   not like in MF implementation the number of arguments that were received */
int
cob_sys_return_args (void *data)
{
	COB_UNUSED (data);

	COB_CHK_PARMS (C$NARG, 1);

	if (COB_MODULE_PTR->cob_procedure_params[0]) {
		cob_set_int (COB_MODULE_PTR->cob_procedure_params[0],
			COB_MODULE_PTR->module_num_params);
	}
	return 0;
}

int
cob_sys_calledby (void *data)
{
	size_t		size;
	size_t		msize;

	COB_CHK_PARMS (C$CALLEDBY, 1);

	if (!COB_MODULE_PTR->cob_procedure_params[0]) {
		/* TO-DO: check what ACU ccbl/runcbl returns,
		   the documentation doesn't say anything about this */
		return -1;
	}
	size = COB_MODULE_PTR->cob_procedure_params[0]->size;
	memset (data, ' ', size);
	if (!COB_MODULE_PTR->next) {
		return 0;
	}
	msize = strlen (COB_MODULE_PTR->next->module_name);
	if (msize > size) {
		msize = size;
	}
	memcpy (data, COB_MODULE_PTR->next->module_name, msize);
	return 1;
}

int
cob_sys_parameter_size (void *data)
{
	int	n;

	COB_UNUSED (data);

	COB_CHK_PARMS (C$PARAMSIZE, 1);

	if (COB_MODULE_PTR->cob_procedure_params[0]) {
		n = cob_get_int (COB_MODULE_PTR->cob_procedure_params[0]);
		if (n > 0 && n <= COB_MODULE_PTR->module_num_params) {
			n--;
			if (COB_MODULE_PTR->next
			 && COB_MODULE_PTR->next->cob_procedure_params[n]) {
				return (int)COB_MODULE_PTR->next->cob_procedure_params[n]->size;
			}
		}
	}
	return 0;
}

int
cob_sys_getopt_long_long (void *so, void *lo, void *idx, const int long_only, void *return_char, void *opt_val)
{
	/*
	 * cob_argc is a static int containing argc from runtime
	 * cob_argv is a static char ** containing argv from runtime
	 */

	size_t opt_val_size = 0;
	size_t so_size = 0;
	size_t lo_size = 0;

	unsigned int lo_amount;
	int exit_status;

	char *shortoptions;
	char *temp;

	struct option *longoptions, *longoptions_root;
	longoption_def *l = NULL;

	int longind = 0;
	unsigned int i;
	int j;

	int return_value;

	COB_UNUSED (idx);
	COB_UNUSED (lo);
	COB_UNUSED (so);

	COB_CHK_PARMS (CBL_GC_GETOPT, 6);

	/* Read in sizes of some parameters */
	if (COB_MODULE_PTR->cob_procedure_params[1]) {
		lo_size = COB_MODULE_PTR->cob_procedure_params[1]->size;
	}
	if (COB_MODULE_PTR->cob_procedure_params[0]) {
		so_size = COB_MODULE_PTR->cob_procedure_params[0]->size;
	}
	if (COB_MODULE_PTR->cob_procedure_params[5]) {
		opt_val_size = COB_MODULE_PTR->cob_procedure_params[5]->size;
	}

	/* buffering longoptions (COBOL), target format (struct option) */
	if (lo_size % sizeof (longoption_def) == 0) {
		lo_amount = (int)lo_size / sizeof (longoption_def);
		longoptions_root = (struct option*) cob_malloc (sizeof (struct option) * ((size_t)lo_amount + 1U));
	} else {
		cob_runtime_error (_("Call to CBL_GC_GETOPT with wrong longoption size."));
		cob_hard_failure ();
	}

	if (!COB_MODULE_PTR->cob_procedure_params[2]) {
		cob_runtime_error (_("Call to CBL_GC_GETOPT with missing longind."));
		cob_hard_failure ();
	}
	longind = cob_get_int (COB_MODULE_PTR->cob_procedure_params[2]);

	/* add 0-termination to strings */
	shortoptions = cob_malloc (so_size + 1U);
	if (COB_MODULE_PTR->cob_procedure_params[0]) {
		cob_field_to_string (COB_MODULE_PTR->cob_procedure_params[0], shortoptions, so_size);
	}

	if (COB_MODULE_PTR->cob_procedure_params[1]) {
		l = (struct __longoption_def*) (COB_MODULE_PTR->cob_procedure_params[1]->data);
	}

	longoptions = longoptions_root;
	for (i = 0; i < lo_amount; i++) {
		j = sizeof (l->name) - 1;
		while (j >= 0 && l->name[j] == ' ') {
			l->name[j] = 0;
			j--;
		}
		longoptions->name = l->name;
		longoptions->has_arg = COB_D2I (l->has_option);
		memcpy (&longoptions->flag, l->return_value_pointer, sizeof (l->return_value_pointer));
		memcpy (&longoptions->val, &l->return_value, 4);

		l = l + 1; /* +1 means pointer + 1*sizeof (longoption_def) */
		longoptions = longoptions + 1;
	}

	/* appending final record, so getopt can spot the end of longoptions */
	longoptions->name = NULL;
	longoptions->has_arg = 0;
	longoptions->flag = NULL;
	longoptions->val = 0;


	l -= lo_amount; /* Set pointer back to begin of longoptions */
	longoptions -= lo_amount;

	return_value = cob_getopt_long_long (cob_argc, cob_argv, shortoptions, longoptions, &longind, long_only);
	temp = (char *) &return_value;

	/* Write data back to COBOL */
#ifdef	WORDS_BIGENDIAN
	if (temp[3] == '?'
	 || temp[3] == ':'
	 || temp[3] == 'W'
	 || temp[3] == 0) {
		exit_status = temp[3] & 0xFF;
	} else if (return_value == -1) {
		exit_status = -1;
	} else {
		exit_status = 3;
	}
	 /* cob_getopt_long_long sometimes returns and 'int' value and sometimes a 'x   ' in the int */
	if (temp[0] == 0
	 && temp[1] == 0
	 && temp[2] == 0) {
		/* Move option value to 1st byte and SPACE fill the 'int' */
		temp[0] = temp[3];
		temp[1] = temp[2] = temp[3] = ' ';
	}
#else
	if (temp[0] == '?'
	 || temp[0] == ':'
	 || temp[0] == 'W'
	 || temp[0] == -1
	 || temp[0] == 0) {
		exit_status = return_value;
	} else {
		exit_status = 3;
	}

	for (i = 3; i > 0; i--) {
		if (temp[i] == 0) temp[i] = ' ';
		else break;
	}
#endif

	cob_set_int (COB_MODULE_PTR->cob_procedure_params[2], longind);
	memcpy (return_char, &return_value, 4);

	if (cob_optarg != NULL) {
		size_t optlen;
		memset (opt_val, 0, opt_val_size);

		optlen = strlen (cob_optarg);
		if (optlen > opt_val_size) {
			/* Return code 2 for "Option value too long => cut" */
			optlen = opt_val_size;
			exit_status = 2;
		}
		memcpy (opt_val, cob_optarg, optlen);
	}

	cob_free (shortoptions);
	cob_free (longoptions_root);

	return exit_status;
}

int
cob_sys_printable (void *p1, ...)
{
	size_t			datalen, n;
	unsigned char		*dotptr;
	unsigned char		dotrep;
	cob_u8_ptr		data;
	char		*previous_locale = NULL;

	COB_CHK_PARMS (CBL_GC_PRINTABLE, 1);
	COB_UNUSED (p1);

	if (!COB_MODULE_PTR->cob_procedure_params[0]) {
		return 0;
	}
	data = cob_get_param_data (1);
	datalen = cob_get_param_size (1);
	if (datalen <= 0)
		return 0;
	if (cob_get_num_params () > 1) {
		dotptr = cob_get_param_data (2);
		dotrep = *dotptr;
	} else {
		dotrep = (unsigned char)'.';
	}
#ifdef	HAVE_SETLOCALE
	if (cobglobptr->cob_locale_ctype) {
		previous_locale = setlocale (LC_CTYPE, NULL);
		setlocale (LC_CTYPE, cobglobptr->cob_locale_ctype);
	}
#endif
	data = p1;
	for (n = 0; n < datalen; ++n) {
		if (!isprint (data[n])) {
			data[n] = dotrep;
		}
	}
#ifdef	HAVE_SETLOCALE
	if (previous_locale) {
		setlocale (LC_CTYPE, previous_locale);
	}
#endif
	return 0;
}

int
cob_sys_justify (void *p1, ...)
{
	cob_u8_ptr	data;
	size_t		datalen;
	size_t		left;
	size_t		right;
	size_t		movelen;
	size_t		centrelen;
	size_t		n;
	size_t		shifting;

	COB_CHK_PARMS (C$JUSTIFY, 1);
	COB_UNUSED (p1);

	if (!COB_MODULE_PTR->cob_procedure_params[0]) {
		return 0;
	}
	data = cob_get_param_data (1);
	datalen = (size_t)cob_get_param_size (1);
	if ((int)datalen < 2) {
		return 0;
	}
	if (data[0] != ' ' && data[datalen - 1] != ' ') {
		return 0;
	}
	for (left = 0; left < datalen; ++left) {
		if (data[left] != ' ') {
			break;
		}
	}
	if (left == (size_t)datalen) {
		return 0;
	}
	right = 0;
	for (n = datalen - 1; ; --n, ++right) {
		if (data[n] != ' ') {
			break;
		}
		if (n == 0) {
			break;
		}
	}
	movelen = datalen - left - right;
	shifting = 0;
	if (cob_get_num_params () > 1) {
		unsigned char	*direction = cob_get_param_data (2);
		if (*direction == 'L') {
			shifting = 1;
		} else if (*direction == 'C') {
			shifting = 2;
		}
	}
	switch (shifting) {
	case 1:
		memmove (data, &data[left], movelen);
		memset (&data[movelen], ' ', datalen - movelen);
		break;
	case 2:
		centrelen = (left + right) / 2;
		memmove (&data[centrelen], &data[left], movelen);
		memset (data, ' ', centrelen);
		if ((left + right) % 2) {
			memset (&data[centrelen + movelen], ' ', centrelen + 1);
		} else {
			memset (&data[centrelen + movelen], ' ', centrelen);
		}
		break;
	default:
		memmove (&data[left + right], &data[left], movelen);
		memset (data, ' ', datalen - movelen);
		break;
	}
	return 0;
}

void
cob_set_locale (cob_field *locale, const int category)
{
#ifdef	HAVE_SETLOCALE
	char	*p;
	char	*buff;

	p = NULL;
	if (locale) {
		if (locale->size == 0) {
			return;
		}
		buff = cob_malloc (locale->size + 1U);
		cob_field_to_string (locale, buff, locale->size);
	} else {
		buff = NULL;
	}

	switch (category) {
	case COB_LC_COLLATE:
		p = setlocale (LC_COLLATE, buff);
		break;
	case COB_LC_CTYPE:
		p = setlocale (LC_CTYPE, buff);
		break;
#ifdef	LC_MESSAGES
	case COB_LC_MESSAGES:
		p = setlocale (LC_MESSAGES, buff);
		break;
#endif
	case COB_LC_MONETARY:
		p = setlocale (LC_MONETARY, buff);
		break;
	case COB_LC_NUMERIC:
		p = setlocale (LC_NUMERIC, buff);
		break;
	case COB_LC_TIME:
		p = setlocale (LC_TIME, buff);
		break;
	case COB_LC_ALL:
		p = setlocale (LC_ALL, buff);
		break;
	case COB_LC_USER:
		if (cobglobptr->cob_locale_orig) {
			p = setlocale (LC_ALL, cobglobptr->cob_locale_orig);
			(void)setlocale (LC_NUMERIC, "C");
		}
		break;
	case COB_LC_CLASS:
		if (cobglobptr->cob_locale_ctype) {
			p = setlocale (LC_CTYPE, cobglobptr->cob_locale_ctype);
		}
		break;
	}
	if (buff) {
		cob_free (buff);
	}
	if (!p) {
		cob_set_exception (COB_EC_LOCALE_MISSING);
		return;
	}
	p = setlocale (LC_ALL, NULL);
	if (p) {
		if (cobglobptr->cob_locale) {
			cob_free (cobglobptr->cob_locale);
		}
		cobglobptr->cob_locale = cob_strdup (p);
	}
#else
	cob_set_exception (COB_EC_LOCALE_MISSING);
#endif
}



/* concatenate two strings allocating a new one
   and optionally free one of the strings
   set str_to_free if the result is assigned to
   one of the two original strings
*/
char *
cob_strcat (char *str1, char *str2, int str_to_free)
{
	size_t		l;
	char		*temp1, *temp2;

	l = strlen (str1) + strlen (str2) + 1;

	/*
	 * If one of the parameter is the buffer itself,
	 * we copy the buffer before continuing.
	 */
	if (str1 == strbuff) {
		temp1 = cob_strdup (str1);
	} else {
		temp1 = str1;
	}
	if (str2 == strbuff) {
		temp2 = cob_strdup (str2);
	} else {
		temp2 = str2;
	}

	if (strbuff) {
		cob_free (strbuff);
	}
	strbuff = (char *) cob_fast_malloc (l);

	sprintf (strbuff, "%s%s", temp1, temp2);
	switch (str_to_free) {
		case 1: cob_free (temp1);
		        break;
		case 2: cob_free (temp2);
		        break;
		default: break;
	}
	return strbuff;
}

char *
cob_strjoin (char **strarray, int size, char *separator)
{
	char	*result;
	int	i;

	if (!strarray || size <= 0 || !separator) return NULL;

	result = cob_strdup (strarray[0]);
	for (i = 1; i < size; i++) {
		result = cob_strcat (result, separator, 1);
		result = cob_strcat (result, strarray[i], 1);
	}

	return result;
}

static void
var_print (const char *msg, const char *val, const char *default_val,
		const unsigned int format)
{
#if 0 /* currently only format 0/1 used */
	switch (format) {
	case 0:
		printf ("%-*.*s : ", CB_IMSG_SIZE, CB_IMSG_SIZE, msg);
		break;
	case 1: {
			int	lablen;
			printf ("  %s: ", _("env"));
			lablen = CB_IMSG_SIZE - 2 - (int)strlen (_("env")) - 2;
			printf ("%-*.*s : ", lablen, lablen, msg);
			break;
		}
	case 2:
		printf ("    %-*.*s     : ", CB_IMSG_SIZE, CB_IMSG_SIZE, msg);
		break;
	case 3:
		printf ("        %-*.*s : ", CB_IMSG_SIZE, CB_IMSG_SIZE, msg);
		break;
	default:
		printf ("%-*.*s : ", CB_IMSG_SIZE, CB_IMSG_SIZE, msg);
		break;
	}

	if (!val && (!default_val || default_val[0] == 0)) {
		putchar ('\n');
		return;
	} else if (format != 0 && val && default_val &&
		((format != 2 && val[0] == '0') || strcmp (val, default_val) == 0)) {
		char dflt[40];
		snprintf (dflt, 39, " %s", _("(default)"));
		val = cob_strcat ((char *) default_val, dflt, 0);
	} else if (!val && default_val) {
		val = default_val;
	}
#else
	if (format == 0) {
		printf ("%-*.*s : ", CB_IMSG_SIZE, CB_IMSG_SIZE, msg);
	} else {
		int	lablen;
		printf ("  %s: ", _("env"));
		lablen = CB_IMSG_SIZE - 2 - (int)strlen (_("env")) - 2;
		printf ("%-*.*s : ", lablen, lablen, msg);
	}

	if (!val && (!default_val || default_val[0] == 0)) {
		putchar ('\n');
		return;
	} else if (format == 1 && val && default_val 
			&& (val[0] == '0' || strcmp (val, default_val) == 0)) {
		char dflt[40];
		snprintf (dflt, 39, " %s", _("(default)"));
		val = cob_strcat ((char *) default_val, dflt, 0);
	} else if (!val && default_val) {
		val = default_val;
	}
#endif

	if (!val && (!default_val || default_val[0] == 0)) {
		putchar ('\n');
		return;
	} else if (format != 0 && val && default_val 
			&& ((format != 2 && val[0] == '0') || strcmp (val, default_val) == 0)) {
		char dflt[40];
		snprintf (dflt, 39, " %s", _("(default)"));
		val = cob_strcat ((char *) default_val, dflt, 0);
	} else if (!val && default_val) {
		val = default_val;
	}

	if (val && strlen (val) <= CB_IVAL_SIZE) {
		printf ("%s", val);
		putchar ('\n');

		return;
	}


	{
		char	*p;
		char	*token;
		size_t	n;

		p = cob_strdup (val);

		n = 0;
		token = strtok (p, " ");
		for (; token; token = strtok (NULL, " ")) {
			int toklen = (int)strlen (token) + 1;
			if ((n + toklen) > CB_IVAL_SIZE) {
				if (n) {
					if (format == 2 || format == 3)
						printf ("\n        %*.*s", CB_IMSG_SIZE + 3,
						CB_IMSG_SIZE + 3, " ");
					else
						printf ("\n%*.*s", CB_IMSG_SIZE + 3, CB_IMSG_SIZE + 3, " ");
				}
				n = 0;
			}
			printf ("%s%s", (n ? " " : ""), token);
			n += toklen;
		}
		putchar ('\n');
		cob_free (p);
	}

}

/*
 Expand a string with environment variable in it.
 Return malloced string.
*/
char *
cob_expand_env_string (char *strval)
{
	unsigned int	i;
	unsigned int	j = 0;
	unsigned int	k = 0;
	size_t	envlen = 1280;
	char		*env;
	char		*str;
	char		ename[128] = { '\0' };
	char		*penv;

	env = cob_malloc (envlen);
	for (k = 0; strval[k] != 0; k++) {
		/* String almost full?; Expand it */
		if (j >= envlen - 128) {
			env = cob_realloc (env, envlen, envlen + 256);
			envlen += 256;
		}

		/* ${envname:default} */
		if (strval[k] == '$' && strval[k + 1] == '{') {
			k += 2;
			for (i = 0; strval[k] != '}'
				     && strval[k] != 0
				     && strval[k] != ':'; k++) {
				ename[i++] = strval[k];
			}
			ename[i++] = 0;
			penv = getenv (ename);
			if (penv == NULL) {
				/* Copy 'default' value */
				if (strval[k] == ':') {
					k++;
					/* ${name:-default} */
					if (strval[k] == '-') {
						k++;
					}
					while (strval[k] != '}' && strval[k] != 0) {
						if (j >= envlen - 50) {
							env = cob_realloc (env, envlen, envlen + 128);
							envlen += 128;
						}
						env[j++] = strval[k++];
					}
					penv = cob_getenv_value (ename);
				}
			}
			if (penv != NULL) {
				if ((strlen (penv) + j) > (envlen - 128)) {
					env = cob_realloc (env, envlen, strlen (penv) + 256);
					envlen = strlen (penv) + 256;
				}
				j += sprintf (&env[j], "%s", penv);
				penv = NULL;
			}
			while (strval[k] != '}' && strval[k] != 0) {
				k++;
			}
			if (strval[k] == '}') {
				k++;
			}
			k--;
		} else if (strval[k] == '$'
		        && strval[k+1] == '$') {	/* Replace $$ with process-id */
			j += sprintf (&env[j], "%d", cob_sys_getpid());
			k++;
		/* CHECME: possibly add $f /$b as basename of executable [or, when passed to cob_init the first name] 
		           along with $d date as yyyymmdd and $t as hhmmss */
		} else if (!isspace ((unsigned char)strval[k])) {
			env[j++] = strval[k];
		} else {
			env[j++] = ' ';
		}
	}

	env[j] = '\0';
	str = cob_strdup (env);
	cob_free (env);

	return str;
}

/* Store 'integer' value in field for correct length (computed with sizeof (fieldtype)) */
static void
set_value (char *data, int len, cob_s64_t val)
{
	/* keep in order of occurrence in data types, last nanoseconds for startup... */
	if (len == sizeof (int)) {
		*(int *)data = (int)val;
	} else if (len == sizeof (short)) {
		*(short *)data = (short)val;
	} else if (len == sizeof (cob_s64_t)) {
		*(cob_s64_t *)data = val;
	} else {
		*data = (char)val;
	}
}

/* Get 'integer' value from field */
static cob_s64_t
get_value (char *data, int len)
{
	if (len == sizeof (int)) {
		return *(int *)data;
	} else if (len == sizeof (short)) {
		return *(short *)data;
	} else if (len == sizeof (cob_s64_t)) {
		return *(cob_s64_t *)data;
	} else {
		return *data;
	}
}

static int
translate_boolean_to_int (const char *ptr)
{
	if (ptr == NULL || *ptr == 0) {
		return 2;
	}
	if (*(ptr + 1) == 0 && isdigit ((unsigned char)*ptr)) {
		return atoi (ptr);		/* 0 or 1 */
	} else
	/* pre-translated boolean "never" - not set" */
	if (strcmp (ptr, "!") == 0) {
		return -1;
	} else
	if (strcasecmp (ptr, "true") == 0
	 || strcasecmp (ptr, "t") == 0
	 || strcasecmp (ptr, "on") == 0
	 || strcasecmp (ptr, "yes") == 0
	 || strcasecmp (ptr, "y") == 0) {
		return 1;			/* True value */
	} else
	if (strcasecmp (ptr, "false") == 0
	 || strcasecmp (ptr, "f") == 0
	 || strcasecmp (ptr, "off") == 0
	 || strcasecmp (ptr, "no") == 0
	 || strcasecmp (ptr, "n") == 0) {
		return 0;			/* False value */
	}
	return 2;
}

/* Set runtime setting with given value */
static int					/* returns 1 if any error, else 0 */
set_config_val (char *value, int pos)
{
	char 	*data;
	char	*ptr = value, *str;
	cob_s64_t	numval = 0;
	int 	i, data_type, data_len, slen;
	size_t	data_loc;

	data_type = gc_conf[pos].data_type;
	data_loc  = gc_conf[pos].data_loc;
	data_len  = gc_conf[pos].data_len;

	data = ((char *)cobsetptr) + data_loc;

	if (gc_conf[pos].enums) {		/* Translate 'word' into alternate 'value' */

		for (i = 0; gc_conf[pos].enums[i].match != NULL; i++) {
			if (strcasecmp (value, gc_conf[pos].enums[i].match) == 0) {
				ptr = value = (char *)gc_conf[pos].enums[i].value;
				break;
			}
			if ((data_type & ENV_ENUMVAL) && strcasecmp (value, gc_conf[pos].enums[i].value) == 0) {
				break;
			}
		}
		if ((data_type & ENV_ENUM || data_type & ENV_ENUMVAL)	/* Must be one of the 'enum' values */
		 && gc_conf[pos].enums[i].match == NULL
		 && (!(data_type & ENV_BOOL))) {
			conf_runtime_error_value (ptr, pos);
			fprintf (stderr, _("should be one of the following values: %s"), "");
			for (i = 0; gc_conf[pos].enums[i].match != NULL; i++) {
				if (i != 0) {
					putc (',', stderr);
					putc (' ', stderr);
				}
				fprintf (stderr, "%s", (char *)gc_conf[pos].enums[i].match);
				if (data_type & ENV_ENUMVAL) {
					fprintf (stderr, "(%s)", (char *)gc_conf[pos].enums[i].value);
				}
			}
			putc ('\n', stderr);
			fflush (stderr);
			return 1;
		}
	}

	if ((data_type & ENV_BOOL)) {	/* Boolean: Yes/No, True/False,... */
		numval = translate_boolean_to_int (ptr);

		if (numval != -1
		 && numval != 1
		 && numval != 0) {
			conf_runtime_error_value (ptr, pos);
			conf_runtime_error (1, _("should be one of the following values: %s"), "true, false");
			return 1;
		}
		if ((data_type & ENV_NOT)) {	/* Negate logic for actual setting */
			numval = !numval;
		}
		set_value (data, data_len, numval);
		if ((data_type & ENV_RESETS)) {	/* Additional setup needed */
			if (strcmp(gc_conf[pos].env_name, "COB_SET_DEBUG") == 0) {
				/* Copy variables from settings (internal) to global structure, each time */
				cobglobptr->cob_debugging_mode = cobsetptr->cob_debugging_mode;
			}
		}
		if (strcmp (gc_conf[pos].env_name, "COB_INSERT_MODE") == 0) {
			cob_settings_screenio ();
		}

	} else if ((data_type & ENV_UINT) 				/* Integer data, unsigned */
	 || (data_type & ENV_SINT) 				/* Integer data, signed */
	 || (data_type & ENV_SIZE) ) {				/* Size: integer with K, M, G */
		char sign = 0;
		for (; *ptr == ' '; ptr++);	/* skip leading space */
		if (*ptr == '-'
		 || *ptr == '+') {
			if ((data_type & ENV_SINT) == 0) {
				conf_runtime_error_value (ptr, pos);
				conf_runtime_error (1, _("should be unsigned")); /* cob_runtime_warning */
				return 1;
			}
			sign = *ptr;
			ptr++;
		}
		if (!isdigit ((unsigned char)*ptr)) {
			conf_runtime_error_value (ptr, pos);
			conf_runtime_error (1, _("should be numeric"));
			return 1;
		}
		for (; *ptr != 0 && (isdigit ((unsigned char)*ptr)); ptr++) {
			numval = (numval * 10) + COB_D2I (*ptr);
		}
		if (sign != 0
		 && ( *ptr == '-'
		   || *ptr == '+')) {
			if ((data_type & ENV_SINT) == 0) {
				conf_runtime_error_value (ptr, pos);
				conf_runtime_error (1, _("should be unsigned"));
				return 1;
			}
			sign = *ptr;
			ptr++;
		}
		if ((data_type & ENV_SIZE)			/* Size: any K, M, G */
		 && *ptr != 0) {
			switch (toupper ((unsigned char)*ptr)) {
			case 'K':
				numval = numval * 1024;
				ptr++;
				break;
			case 'M':
				if (numval < 4001) {
					numval = numval * 1024 * 1024;
				} else {
					/* use max. guaranteed value for unsigned long
					   to raise a warning as max value is limit to one less */
					numval = 4294967295UL;
				}
				ptr++;
				break;
			case 'G':
				if (numval < 4) {
					numval = numval * 1024 * 1024 * 1024;
				} else {
					/* use max. guaranteed value for unsigned long
					   to raise a warning as max value is limit to one less */
					numval = 4294967295UL;
				}
				ptr++;
				break;
			}
		}
		for (; *ptr == ' '; ptr++);	/* skip trailing space */
		if (*ptr != 0) {
			conf_runtime_error_value (ptr, pos);
			conf_runtime_error (1, _("should be numeric"));
			return 1;
		}
		if (sign == '-') {
			numval = -numval;
		}
		if (gc_conf[pos].min_value > 0
		 && numval < gc_conf[pos].min_value) {
			conf_runtime_error_value (value, pos);
			conf_runtime_error (1, _("minimum value: %lu"), gc_conf[pos].min_value);
			return 1;
		}
		if (gc_conf[pos].max_value > 0
		 && numval > gc_conf[pos].max_value) {
			conf_runtime_error_value (value, pos);
			conf_runtime_error (1, _("maximum value: %lu"), gc_conf[pos].max_value);
			return 1;
		}
		set_value (data, data_len, numval);
		if (strcmp (gc_conf[pos].env_name, "COB_MOUSE_FLAGS") == 0
#ifdef HAVE_MOUSEINTERVAL	/* possibly add an internal option for mouse support, too */
		 || strcmp (gc_conf[pos].env_name, "COB_MOUSE_INTERVAL") == 0
#endif
			) {
			cob_settings_screenio ();
		}

	} else if ((data_type & ENV_FILE)
	        || (data_type & ENV_PATH)) {	/* Path (environment expanded) to be stored as a string */
		memcpy (&str, data, sizeof (char *));
		if (str != NULL) {
			cob_free ((void *)str);
		}
		str = cob_expand_env_string (value);
		if ((data_type & ENV_FILE)
		 && strchr (str, PATHSEP_CHAR) != NULL) {
			conf_runtime_error_value (value, pos);
			conf_runtime_error (1, _("should not contain '%c'"), PATHSEP_CHAR);
			cob_free (str);
			return 1;
		}
		memcpy (data, &str, sizeof (char *));
		if (data_loc == offsetof (cob_settings, cob_preload_str)) {
			cobsetptr->cob_preload_str_set = cob_strdup(str);
		}

		/* call internal routines that do post-processing */
		if (strcmp (gc_conf[pos].env_name, "COB_TRACE_FILE") == 0
		 && cobsetptr->cob_trace_file != NULL) {
			cob_new_trace_file ();
		}

	} else if (data_type & ENV_STR) {	/* String (environment expanded) */
		memcpy (&str, data, sizeof (char *));
		if (str != NULL) {
			cob_free ((void *)str);
		}
		str = cob_expand_env_string (value);
		memcpy (data, &str, sizeof (char *));
		if (data_loc == offsetof (cob_settings, cob_preload_str)) {
			cobsetptr->cob_preload_str_set = cob_strdup(str);
		}

		/* call internal routines that do post-processing */
		if (strcmp (gc_conf[pos].env_name, "COB_CURRENT_DATE") == 0) {
			check_current_date ();
		}

	} else if ((data_type & ENV_CHAR)) {	/* 'char' field inline */
		memset (data, 0, data_len);
		slen = (int)strlen (value);
		if (slen > data_len) {
			slen = data_len;
		}
		memcpy (data, value, slen);
	}
	return 0;
}

/* Set runtime setting by name with given value */
static int					/* returns 1 if any error, else 0 */
set_config_val_by_name (char *value, const char *name, const char *func)
{
	int	i;
	int ret = 1;

	for (i = 0; i < NUM_CONFIG; i++) {
		if (!strcmp (gc_conf[i].conf_name, name)) {
			ret = set_config_val (value, i);
			if (func) {
				gc_conf[i].data_type |= STS_FNCSET;
				gc_conf[i].set_by = FUNC_NAME_IN_DEFAULT;
				gc_conf[i].default_val = func;
			}
			break;
		}
	}
	return ret;
}

/* Return setting value as a 'string' */
static char *
get_config_val (char *value, int pos, char *orgvalue)
{
	char 	*data;
	char	*str;
	double	dval;
	cob_s64_t	numval;
	int	i, data_type, data_len;
	size_t	data_loc;

	data_type	= gc_conf[pos].data_type;
	data_loc	= gc_conf[pos].data_loc;
	data_len	= gc_conf[pos].data_len;

	data = ((char *)cobsetptr) + data_loc;

	if (min_conf_length == 0) {
		not_set = _("not set");
		min_conf_length = (char) strlen (not_set) + 1;
		if (min_conf_length < 6) {
			min_conf_length = 6;
		} else if (min_conf_length > 15) {
			min_conf_length = 15;
		}
	}

	strcpy (value, _("unknown"));
	orgvalue[0] = 0;

	if ((data_type & ENV_BOOL)) {	/* Boolean: Yes/No, True/False,... */
		numval = get_value (data, data_len);
		if (numval == -1) {
			if (gc_conf[pos].enums == never) {
				strcpy (value, "never");
			} else {
				strcpy (value, _("not set"));
			}
		} else {
			if ((data_type & ENV_NOT)) {
				numval = !numval;
			}
			if (numval) {
				strcpy (value, _("yes"));
			} else {
				strcpy (value, _("no"));
			}
		}

	} else if (data_type & ENV_UINT) {				/* Integer data, unsigned */
		numval = get_value (data, data_len);
		sprintf (value, CB_FMT_LLU, numval);

	} else if (data_type & ENV_SINT) {				/* Integer data, signed */
		numval = get_value (data, data_len);
		sprintf (value, CB_FMT_LLD, numval);

	} else if ((data_type & ENV_SIZE)) {			/* Size: integer with K, M, G */
		numval = get_value (data, data_len);
		dval = (double) numval;
		if (numval > (1024 * 1024 * 1024)) {
			if ((numval % (1024 * 1024 * 1024)) == 0) {
				sprintf (value, CB_FMT_LLD" GB", numval / (1024 * 1024 * 1024));
			} else {
				sprintf (value, "%.2f GB", dval / (1024.0 * 1024.0 * 1024.0));
			}
		} else if (numval > (1024 * 1024)) {
			if ((numval % (1024 * 1024)) == 0) {
				sprintf (value, CB_FMT_LLD" MB", numval / (1024 * 1024));
			} else {
				sprintf (value, "%.2f MB", dval / (1024.0 * 1024.0));
			}
		} else if (numval > 1024) {
			if ((numval % 1024) == 0) {
				sprintf (value, CB_FMT_LLD" KB", numval / 1024);
			} else {
				sprintf (value, "%.2f KB", dval / 1024.0);
			}
		} else {
			sprintf (value, CB_FMT_LLD, numval);
		}

	/* TO-DO: Consolidate copy-and-pasted code! */
	} else if (data_type & ENV_STR) {	/* String stored as a string */
		memcpy (&str, data, sizeof (char *));
		if (data_loc == offsetof (cob_settings, cob_display_print_filename)
		 && cobsetptr->cob_display_print_file) {
			snprintf (value, COB_MEDIUM_MAX, _("set by %s"), "cob_set_runtime_option");
		} else if (data_loc == offsetof (cob_settings, cob_display_punch_filename)
		 && cobsetptr->cob_display_punch_file) {
			snprintf (value, COB_MEDIUM_MAX, _("set by %s"), "cob_set_runtime_option");
		} else if(data_loc == offsetof (cob_settings, cob_trace_filename)
		      && cobsetptr->external_trace_file) {
			snprintf (value, COB_MEDIUM_MAX, _("set by %s"), "cob_set_runtime_option");
		} else if (str == NULL) {
			snprintf (value, COB_MEDIUM_MAX, _("not set"));
		} else {
			snprintf (value, COB_MEDIUM_MAX, "'%s'", str);
		}

	} else if ((data_type & ENV_FILE)) {	/* File/path stored as a string */
		memcpy (&str, data, sizeof (char *));
		/* TODO: add special cases here on merging rw-branch */
		if (str == NULL)  {
			snprintf (value, COB_MEDIUM_MAX, _("not set"));
		} else {
			snprintf (value, COB_MEDIUM_MAX, "%s", str);
		}

	} else if ((data_type & ENV_PATH)) {	/* Path stored as a string */
		memcpy (&str, data, sizeof (char *));
		if (str == NULL)  {
			snprintf (value, COB_MEDIUM_MAX, _("not set"));
		} else {
			snprintf (value, COB_MEDIUM_MAX, "%s", str);
		}

	} else if ((data_type & ENV_CHAR)) {	/* 'char' field inline */
		if (*(char *)data == 0) {
			strcpy (value, "Nul");
		} else if (isprint (*(unsigned char *)data)) {
			sprintf (value, "'%s'", (char *)data);
		} else {
			sprintf (value, "0x%02X", *(char *)data);
		}
	}
	value[COB_MEDIUM_MAX] = 0;	/* fix warning */

	if (gc_conf[pos].enums) {		/* Translate 'word' into alternate 'value' */
		for (i = 0; gc_conf[pos].enums[i].match != NULL; i++) {
			if (strcasecmp (value, gc_conf[pos].enums[i].value) == 0) {
				if (strcmp(value,"0") != 0
				 && gc_conf[pos].default_val != NULL
				 && strcmp (value, gc_conf[pos].default_val) != 0) {
					strcpy (orgvalue, value);
				} 
				strcpy (value, gc_conf[pos].enums[i].match);
				break;
			}
		}
		if (gc_conf[pos].enums[i].match == NULL
		 && gc_conf[pos].default_val != NULL
		 && strcmp (value, gc_conf[pos].default_val) != 0) {
			strcpy (orgvalue, value);
		}
	} else
	if (!(gc_conf[pos].data_type & STS_ENVSET)
	 && !(gc_conf[pos].data_type & STS_CNFSET)
	 && !(gc_conf[pos].data_type & ENV_BOOL)
	 && gc_conf[pos].default_val != NULL) {
		strcpy (value, gc_conf[pos].default_val);
		orgvalue[0] = 0;
	}

	if (gc_conf[pos].default_val != NULL
	 && strcmp (orgvalue, gc_conf[pos].default_val) != 0) {
		orgvalue[0] = 0;
	} else if (strcmp (value, orgvalue) == 0) {
		orgvalue[0] = 0;
	}

	return value;
}

static int
cb_lookup_config (char *keyword)
{
	int	i;
	for (i = 0; i < NUM_CONFIG; i++) {		/* Set value from config file */
		if (gc_conf[i].conf_name
		&& strcasecmp (keyword, gc_conf[i].conf_name) == 0) {	/* Look for config file name */
			break;
		}
		if (gc_conf[i].env_name
		&& strcasecmp (keyword, gc_conf[i].env_name) == 0) {	/* Catch using env var name */
			break;
		}
	}
	return i;
}

static int
cb_config_entry (char *buf, int line)
{
	int	i, j, k, old_type;
	void	*data;
	char	*str, qt;
	char	keyword[COB_MINI_BUFF], value[COB_SMALL_BUFF];

	cob_source_line = line;

	for (j= (int)strlen (buf); buf[j-1] == '\r' || buf[j-1] == '\n'; )	/* Remove CR LF */
		buf[--j] = 0;

	for (i = 0; isspace ((unsigned char)buf[i]); i++);

	for (j = 0; buf[i] != 0 && buf[i] != ':' && !isspace ((unsigned char)buf[i]) && buf[i] != '=' && buf[i] != '#'; )
		keyword[j++] = buf[i++];
	keyword[j] = 0;

	while (buf[i] != 0 && (isspace ((unsigned char)buf[i]) || buf[i] == ':' || buf[i] == '=')) i++;
	if (buf[i] == '"'
	||  buf[i] == '\'') {
		qt = buf[i++];
		for (j = 0; buf[i] != qt && buf[i] != 0; )
			value[j++] = buf[i++];
	} else {
		for (j = 0; !isspace ((unsigned char)buf[i]) && buf[i] != '#' && buf[i] != 0; )
			value[j++] = buf[i++];
	}

	value[j] = 0;
	if (strcasecmp (keyword, "reset") != 0
	 && strcasecmp (keyword, "include") != 0
	 && strcasecmp (keyword, "includeif") != 0
	 && strcasecmp (keyword, "setenv") != 0
	 && strcasecmp (keyword, "unsetenv") != 0) {
		i = cb_lookup_config (keyword);

		if (i >= NUM_CONFIG) {
			conf_runtime_error (1, _("unknown configuration tag '%s'"), keyword);
			return -1;
		}
	}
	if (strcmp (value, "") == 0) {
		if (strcasecmp (keyword, "include") != 0
		&&  strcasecmp (keyword, "includeif")) {
			conf_runtime_error(1, _("WARNING - '%s' without a value - ignored!"), keyword);
			return 2;
		} else {
			conf_runtime_error (1, _("'%s' without a value!"), keyword);
			return -1;
		}
	}

	if (strcasecmp (keyword, "setenv") == 0 ) {
		char	value2[COB_SMALL_BUFF];
		/* collect additional value and push into environment */
		strcpy (value2, "");
		/* check for := in value 2 and split, if necessary */
		k = 0; while (value[k] != '=' && value[k] != ':' && value[k] != '"' && value[k] != '\'' && value[k] != 0) k++;
		if (value[k] == '=' || value[k] == ':') {
			i = i - (int)strlen (value + k);
			value[k] = 0;
		}
		while (isspace ((unsigned char)buf[i]) || buf[i] == ':' || buf[i] == '=') i++;
		if (buf[i] == '"'
		|| buf[i] == '\'') {
			qt = buf[i++];
			for (j = 0; buf[i] != qt && buf[i] != 0; )
				value2[j++] = buf[i++];
		} else {
			for (j = 0; !isspace ((unsigned char)buf[i]) && buf[i] != '#' && buf[i] != 0; )
				value2[j++] = buf[i++];
		}
		value2[j] = 0;
		if (strcmp (value2, "") == 0) {
			conf_runtime_error (1, _("WARNING - '%s %s' without a value - ignored!"), keyword, value);
			return 2;
		}
		/* check additional value for inline env vars ${varname:-default} */
		str = cob_expand_env_string (value2);

		(void)cob_setenv (value, str, 1);
		cob_free (str);
		for (i = 0; i < NUM_CONFIG; i++) {		/* Set value from config file */
			if (gc_conf[i].env_name
			&& strcasecmp (value, gc_conf[i].env_name) == 0) {/* no longer cleared by runtime.cfg */
				gc_conf[i].data_type &= ~STS_ENVCLR;
				break;
			}
		}
		return 0;
	}

	if (strcasecmp (keyword, "unsetenv") == 0) {
		if ((getenv (value)) != NULL ) {
			for (i = 0; i < NUM_CONFIG; i++) {		/* Set value from config file */
				if (gc_conf[i].env_name
				&& strcasecmp (value, gc_conf[i].env_name) == 0) {	/* Catch using env var name */
					gc_conf[i].data_type |= STS_ENVCLR;
					break;
				}
			}
			(void)cob_unsetenv (value);
		}
		return 0;
	}

	if (strcasecmp (keyword, "include") == 0 ||
		strcasecmp (keyword, "includeif") == 0) {
		str = cob_expand_env_string (value);
		strcpy (buf, str);
		cob_free (str);
		if (strcasecmp (keyword, "include") == 0) {
			return 1;
		} else {
			return 3;
		}
	}

	if (strcasecmp (keyword, "reset") == 0) {
		i = cb_lookup_config (value);
		if (i >= NUM_CONFIG) {
			conf_runtime_error (1, _("unknown configuration tag '%s'"), value);
			return -1;
		}
		gc_conf[i].data_type &= ~(STS_ENVSET | STS_CNFSET | STS_ENVCLR);	/* Clear status */
		gc_conf[i].data_type |= STS_RESET;
		gc_conf[i].set_by = 0;
		gc_conf[i].config_num = cobsetptr->cob_config_cur - 1;
		if (gc_conf[i].default_val) {
			set_config_val ((char *)gc_conf[i].default_val, i);
		} else if ((gc_conf[i].data_type & ENV_STR)
			|| (gc_conf[i].data_type & ENV_FILE)
			|| (gc_conf[i].data_type & ENV_PATH)) {	/* String/Path stored as a string */
			data = (void *) ((char *)cobsetptr + gc_conf[i].data_loc);
			memcpy (&str, data, sizeof (char *));
			if (str != NULL) {
				cob_free ((void *)str);
			}
			str = NULL;
			memcpy (data, &str, sizeof (char *));	/* Reset pointer to NULL */
		} else {
			set_config_val ((char *)"0", i);
		}
		return 0;
	}

	i = cb_lookup_config (keyword);

	if (i >= NUM_CONFIG) {
		conf_runtime_error (1, _("unknown configuration tag '%s'"), keyword);
		return -1;
	}

	old_type = gc_conf[i].data_type;
	gc_conf[i].data_type |= STS_CNFSET;
	if (!set_config_val (value, i)) {
		gc_conf[i].data_type &= ~STS_RESET;
		gc_conf[i].config_num = cobsetptr->cob_config_cur - 1;

		if (gc_conf[i].env_group == GRP_HIDE) {
			for (j = 0; j < NUM_CONFIG; j++) {		/* Any alias present? */
				if (j != i
				 && gc_conf[i].data_loc == gc_conf[j].data_loc) {
					gc_conf[j].data_type |= STS_CNFSET;
					gc_conf[j].data_type &= ~STS_RESET;
					gc_conf[j].config_num = gc_conf[i].config_num;
					gc_conf[j].set_by = i;
				}
			}
		}
	} else {
		gc_conf[i].data_type = old_type;
	}
	return 0;
}

static int
cob_load_config_file (const char *config_file, int isoptional)
{
	char			buff[COB_FILE_BUFF - 10], filename[COB_FILE_BUFF];
	char			*penv;
	int			sub_ret, ret;
	unsigned int	i;
	int			line;
	FILE			*conf_fd;

	for (i = 0; config_file[i] != 0 && config_file[i] != SLASH_CHAR; i++);
	if (config_file[i] == 0) {			/* Just a name, No directory */
		if (access (config_file, F_OK) != 0) {	/* and file does not exist */
			/* check for path of previous configuration file (for includes) */
			filename[0] = 0;
			if (cobsetptr->cob_config_cur != 0) {
				size_t size;
				strncpy (buff,
					cobsetptr->cob_config_file[cobsetptr->cob_config_cur - 1],
					(size_t)COB_FILE_MAX-10);
				size = strlen (buff);
				if (size != 0 && buff[size] == SLASH_CHAR) buff[--size] = 0;
				if (size != 0) {
					snprintf (filename, (size_t)COB_FILE_MAX, "%s%c%s", buff, SLASH_CHAR,
						config_file);
					if (access (filename, F_OK) == 0) {	/* and prefixed file exist */
						config_file = filename;		/* Prefix last directory */
					} else {
						filename[0] = 0;
					}
				}
			}
			if (filename[0] == 0) {
				/* check for COB_CONFIG_DIR (use default if not in environment) */
				penv = cob_getenv_value ("COB_CONFIG_DIR");
				if (penv != NULL) {
					snprintf (filename, (size_t)COB_FILE_MAX, "%s%c%s",
						penv, SLASH_CHAR, config_file);
				} else {
					snprintf (filename, (size_t)COB_FILE_MAX, "%s%c%s",
						COB_CONFIG_DIR, SLASH_CHAR, config_file);
				}
				if (access (filename, F_OK) == 0) {	/* and prefixed file exist */
					config_file = filename;		/* Prefix COB_CONFIG_DIR */
				}
			}
		}
	}

	cob_source_file = config_file;

	/* check for recursion */
	for (i = 0; i < cobsetptr->cob_config_num; i++) {
		if (strcmp (cobsetptr->cob_config_file[i], config_file) == 0) {
			cob_source_line = 0;
			conf_runtime_error (1, _("recursive inclusion"));
			return -2;
		}
	}

	/* Open the configuration file */
	conf_fd = fopen (config_file, "r");
	if (conf_fd == NULL && !isoptional) {
		cob_source_line = 0;
		conf_runtime_error (1, cob_get_strerror ());
		if (cobsetptr->cob_config_file) {
			cob_source_file = cobsetptr->cob_config_file[cobsetptr->cob_config_num-1];
		}
		return -1;
	}
	if (conf_fd != NULL) {
		if (cobsetptr->cob_config_file == NULL) {
			cobsetptr->cob_config_file = cob_malloc (sizeof (char *));
		} else {
			const size_t old_size = sizeof (char *) * cobsetptr->cob_config_num;
			const size_t new_size = sizeof (char *) * (cobsetptr->cob_config_num + 1);
			cobsetptr->cob_config_file = cob_realloc (cobsetptr->cob_config_file, old_size, new_size);
		}
		cobsetptr->cob_config_file[cobsetptr->cob_config_num++] = cob_strdup (config_file);	/* Save config file name */
		cobsetptr->cob_config_cur = cobsetptr->cob_config_num;
	}


	/* Read the configuration file */
	ret = 0;
	line = 0;
	while ((conf_fd != NULL)
	&& 	(fgets (buff, COB_SMALL_BUFF, conf_fd) != NULL) ) {
		line++;
		for (i = 0; isspace ((unsigned char)buff[i]); i++);
		if (buff[i] == 0
		||  buff[i] == '#'
		||  buff[i] == '\r'
		||  buff[i] == '\n')
			continue;	/* Skip comments and blank lines */

		/* Evaluate config line */
		sub_ret = cb_config_entry (buff, line);

		/* Include another configuration file */
		if (sub_ret == 1 || sub_ret == 3) {
			cob_source_line = line;
			sub_ret = cob_load_config_file (buff, sub_ret == 3);
			cob_source_file = config_file;
			if (sub_ret < 0) {
				ret = -1;
				cob_source_line = line;
				conf_runtime_error (1, _("configuration file was included here"));
				break;
			}
		}
		if (sub_ret < ret) ret = sub_ret;
	}
	if (conf_fd) {
		fclose (conf_fd);
		cobsetptr->cob_config_cur--;
	}
	cob_source_file = NULL;
	conf_fd = NULL;

	return ret;
}

/*
 * Load the GnuCOBOL runtime configuration information
 */
int
cob_load_config (void)
{
	char		*env;
	char		conf_file[COB_MEDIUM_BUFF];
	int		is_optional = 1, sts, i, j;


	/* Get the name for the configuration file */
	if ((env = getenv ("COB_RUNTIME_CONFIG")) != NULL && env[0]) {
		strncpy (conf_file, env, (size_t)COB_MEDIUM_MAX);
		conf_file[COB_MEDIUM_MAX] = 0;
		is_optional = 0;			/* If declared then it is NOT optional */
		if (strchr (conf_file, PATHSEP_CHAR) != NULL) {
			conf_runtime_error (0, _("invalid value '%s' for configuration tag '%s'"), conf_file, "COB_RUNTIME_CONFIG");
			conf_runtime_error (1, _("should not contain '%c'"), PATHSEP_CHAR);
			return -1;
		}
	} else {
		/* check for COB_CONFIG_DIR (use default if not in environment) */
		if ((env = cob_getenv_value ("COB_CONFIG_DIR")) != NULL && env[0]) {
			snprintf (conf_file, (size_t)COB_MEDIUM_MAX, "%s%c%s", env, SLASH_CHAR, "runtime.cfg");
		} else {
			snprintf (conf_file, (size_t)COB_MEDIUM_MAX, "%s%c%s", COB_CONFIG_DIR, SLASH_CHAR, "runtime.cfg");
		}
		conf_file[COB_MEDIUM_MAX] = 0; /* fixing code analyser warning */
		is_optional = 1;			/* If not present, then just use env vars */
		if (strchr (conf_file, PATHSEP_CHAR) != NULL) {
			conf_runtime_error (0, _("invalid value '%s' for configuration tag '%s'"), conf_file, "COB_CONFIG_DIR");
			conf_runtime_error (1, _("should not contain '%c'"), PATHSEP_CHAR);
			return -1;
		}
	}

#ifndef WITH_FILE_FORMAT
	sprintf (varseq_dflt, "%d", WITH_VARSEQ);		/* Default comes from config.h */
#endif
	for (i = 0; i < NUM_CONFIG; i++) {
		gc_conf[i].data_type &= ~(STS_ENVSET | STS_CNFSET | STS_ENVCLR);	/* Clear status */
	}

	sts = cob_load_config_file (conf_file, is_optional);
	if (sts < 0) {
		return sts;
	}
	cob_rescan_env_vals (); 			/* Check for possible environment variables */

	/* Set with default value if present and not set otherwise */
	for (i = 0; i < NUM_CONFIG; i++) {
		if (gc_conf[i].default_val
		&& !(gc_conf[i].data_type & STS_CNFSET)
		&& !(gc_conf[i].data_type & STS_ENVSET)) {
			for (j = 0; j < NUM_CONFIG; j++) {	/* Any alias present? */
				if (j != i
				&& gc_conf[i].data_loc == gc_conf[j].data_loc)
					break;
			}
			if (j < NUM_CONFIG) {
				if (!(gc_conf[j].data_type & STS_CNFSET)
				&& !(gc_conf[j].data_type & STS_ENVSET)) {	/* alias not defined? */
					set_config_val ((char *)gc_conf[i].default_val, i);
				}
			} else {
				set_config_val ((char *)gc_conf[i].default_val, i); /* Set default value */
			}
		}
	}
	check_current_date();

	return 0;
}

/* output runtime warning for issues produced by external API functions */
void
cob_runtime_warning_external (const char *caller_name, const int cob_reference, const char *fmt, ...)
{
	va_list args;

	if (!cobsetptr->cob_display_warn) {
		return;
	}
	if (!(caller_name && *caller_name)) caller_name = "unknown caller";

	/* Prefix */
	if (cob_reference) {
		fflush (stderr); /* necessary as we write afterwards */
		write_to_stderr_or_return_arr ("libcob: ");
		cob_get_source_line ();
		output_source_location ();
	} else {
		fprintf (stderr, "libcob: ");
	}
	fprintf (stderr, _("warning: "));
	fprintf (stderr, "%s: ", caller_name);

	/* Body */
	va_start (args, fmt);
	vfprintf (stderr, fmt, args);
	va_end (args);

	/* Postfix */
	putc ('\n', stderr);
	fflush (stderr);
}

void
cob_runtime_warning_ss (const char *msg, const char *addition)
{
	if (cobsetptr && !cobsetptr->cob_display_warn) {
		return;
	}

	/* Prefix */
	write_to_stderr_or_return_arr ("libcob: ");
	output_source_location ();
	write_to_stderr_or_return_str (warning_msgid);

	/* Body */
	write_to_stderr_or_return_str (msg);
	if (addition) {
		write_to_stderr_or_return_str (addition);
	}

	/* Postfix */
	write_to_stderr_or_return_arr ("\n");
}

void
cob_runtime_warning (const char *fmt, ...)
{
	va_list args;

	if (cobsetptr && !cobsetptr->cob_display_warn) {
		return;
	}

	fflush (stderr);	/* necessary as we write afterwards */

	/* Prefix */
	write_to_stderr_or_return_arr ("libcob: ");
	cob_get_source_line ();
	output_source_location ();

	fprintf (stderr, _("warning: "));	/* back to stdio */

	/* Body */
	va_start (args, fmt);
	vfprintf (stderr, fmt, args);
	va_end (args);

	/* Postfix */
	putc ('\n', stderr);
	fflush (stderr);
}

void
cob_runtime_hint (const char *fmt, ...)
{
	va_list args;

	/* Prefix */
	fprintf (stderr, "%s", _("note: "));

	/* Body */
	va_start (args, fmt);
	vfprintf (stderr, fmt, args);
	va_end (args);

	/* Postfix */
	putc ('\n', stderr);
	fflush (stderr);
}

/* extra function for direct interaction with the debugger
   when a new runtime error string is constructed */
static void COB_NOINLINE
cob_setup_runtime_error_str (const char *fmt, va_list ap)
{
	const char		*source_file;
	unsigned int	 source_line;
	char *p = runtime_err_str;

	set_source_location (&source_file, &source_line);
	if (source_file) {
		if (source_line) {
			sprintf (runtime_err_str, "%s:%u: ",
				source_file, source_line);
		} else {
			sprintf (runtime_err_str, "%s: ",
				source_file);
		}
		p = runtime_err_str + strlen (runtime_err_str);
	}
	vsprintf (p, fmt, ap);
}

void
cob_runtime_error (const char *fmt, ...)
{
	struct handlerlist	*h;
	va_list			ap;

	int	more_error_procedures = 1;
	cob_get_source_line ();

	va_start (ap, fmt);
	cob_setup_runtime_error_str (fmt, ap);
	va_end (ap);

#if	1	/* RXWRXW - Exit screen */
	/* Exit screen mode early */
	cob_exit_screen ();
#endif

	if (hdlrs != NULL && !active_error_handler && cobglobptr) {

		const char		*err_source_file;
		unsigned int	err_source_line, err_module_statement = 0;
		cob_module_ptr	err_module_pointer = NULL;
		int call_params = cobglobptr->cob_call_params;

		/* save error location */
		set_source_location (&err_source_file, &err_source_line);
		if (COB_MODULE_PTR) {
			err_module_pointer = COB_MODULE_PTR;
			err_module_statement = COB_MODULE_PTR->module_stmt;
		}

		/* run registered error handlers */
		active_error_handler = 1;
		h = hdlrs;
		while (h != NULL) {
			int			(*current_handler)(char *) = h->proc;
			struct handlerlist	*hp = h;

			h = h->next;
			cob_free (hp);

			if (more_error_procedures) {
				/* fresh error buffer with guaranteed size */
				char local_err_str[COB_ERRBUF_SIZE];
				memcpy (local_err_str, runtime_err_str, COB_ERRBUF_SIZE);

				/* ensure that error handlers set their own locations */
				cob_source_file = NULL;
				cob_source_line = 0;
				cobglobptr->cob_call_params = 1;

				more_error_procedures = current_handler (local_err_str);
			}
		}
		hdlrs = NULL;
		active_error_handler = 0;

		/* restore error location */
		cob_source_file = err_source_file;
		cob_source_line = err_source_line;
		COB_MODULE_PTR = err_module_pointer;
		if (COB_MODULE_PTR) {
			COB_MODULE_PTR->module_stmt = err_module_statement;
		}
		cobglobptr->cob_call_params = call_params;
	}

	/* Prefix */
	if (more_error_procedures) {
		const char		*source_file;
		unsigned int	 source_line;
		set_source_location (&source_file, &source_line);
		fputs ("libcob: ", stderr);
		if (source_file) {
			fprintf (stderr, "%s:", source_file);
			if (source_line) {
				fprintf (stderr, "%u:", source_line);
			}
			fputc (' ', stderr);
		}
		fprintf (stderr, "%s: ", _("error"));

		/* Body */
		va_start (ap, fmt);
		vfprintf (stderr, fmt, ap);
		va_end (ap);

		/* Postfix */
		fputc ('\n', stderr);
		fflush (stderr);
	}

	/* setup reason for optional module dump */
	if (cob_initialized && abort_reason[0] == 0) {
#if 0	/* Is there a use in this message ?*/
		fputc ('\n', stderr);
		fprintf (stderr, _("abnormal termination - file contents may be incorrect"));
#endif
		va_start (ap, fmt);
		vsnprintf (abort_reason, COB_MINI_BUFF, fmt, ap);
		va_end (ap);
	}
}

void
cob_fatal_error (const enum cob_fatal_error fatal_error)
{
	const char	*msg;
	unsigned char	*file_status;
	char		*err_cause;
	int		status;
#ifdef	_WIN32
	char		*p;
#endif

	switch (fatal_error) {
#if 0 /* Currently not in use, should enter unknown error */
	case COB_FERROR_NONE:
		break;
#endif
	/* Note: can be simply tested; therefore no exclusion */
	case COB_FERROR_CANCEL:
		cob_runtime_error (_("attempt to CANCEL active program"));
		break;
	/* Note: can be simply tested; therefore no exclusion */
	case COB_FERROR_INITIALIZED:
#ifdef	_WIN32
		/* cob_unix_lf needs to be set before any error message is thrown,
		as they would have wrong line endings otherwise */
		p = getenv ("COB_UNIX_LF");
		if (p && (*p == 'Y' || *p == 'y' ||
			*p == 'T' || *p == 't' ||
			*p == '1')) {
			(void)_setmode (_fileno (stdin), _O_BINARY);
			(void)_setmode (_fileno (stdout), _O_BINARY);
			(void)_setmode (_fileno (stderr), _O_BINARY);
		}
#endif
		/* note: same message in call.c */
		cob_runtime_error (_("cob_init() has not been called"));
		break;
	/* LCOV_EXCL_START */
	case COB_FERROR_CODEGEN:
		cob_runtime_error ("codegen error");	/* not translated by intent */
		cob_runtime_error (_("Please report this!"));
		break;
	/* LCOV_EXCL_STOP */
	/* Note: can be simply tested; therefore no exclusion */
	case COB_FERROR_CHAINING:
		cob_runtime_error (_("CALL of program with CHAINING clause"));
		break;
	/* LCOV_EXCL_START */
	case COB_FERROR_STACK:
		cob_runtime_error (_("stack overflow, possible PERFORM depth exceeded"));
		break;
	/* LCOV_EXCL_STOP */
	/* Note: can be simply tested (GO TO DECLARATIVES); therefore no exclusion */
	case COB_FERROR_GLOBAL:
		cob_runtime_error (_("invalid entry/exit in GLOBAL USE procedure"));
		break;
	/* LCOV_EXCL_START */
	case COB_FERROR_MEMORY:
		cob_runtime_error (_("unable to allocate memory"));
		break;
	/* LCOV_EXCL_STOP */
	/* LCOV_EXCL_START */
	case COB_FERROR_MODULE:
		cob_runtime_error (_("invalid entry into module"));
		break;
	/* LCOV_EXCL_STOP */
	/* Note: can be simply tested; therefore no exclusion */
	case COB_FERROR_RECURSIVE:
#if 0 /* not merged yet */
		/* LCOV_EXCL_LINE */
		if (cob_module_err) {
			cob_runtime_error (_("recursive CALL from '%s' to '%s' which is NOT RECURSIVE"),
					COB_MODULE_PTR->module_name, cob_module_err->module_name);
		/* LCOV_EXCL_START */
		/* Note: only in for old modules - not active with current generation */
		} else {
			cob_runtime_error (_("invalid recursive COBOL CALL to '%s'"),
					   COB_MODULE_PTR->module_name);
		}
		/* LCOV_EXCL_STOP */
#else
		cob_runtime_error (_("invalid recursive COBOL CALL to '%s'"),
				   COB_MODULE_PTR->module_name);
#endif
		break;
	/* LCOV_EXCL_START */
	case COB_FERROR_FREE:
		cob_runtime_error (_("call to %s with NULL pointer"), "cob_free");
		break;
	/* LCOV_EXCL_STOP */
	case COB_FERROR_DIV_ZERO:
		cob_runtime_error (_("divide by ZERO"));
		break;
	case COB_FERROR_FILE:
		file_status = cobglobptr->cob_error_file->file_status;
		status = COB_D2I (file_status[0]) * 10 + COB_D2I (file_status[1]);
		switch (status) {
		case COB_STATUS_10_END_OF_FILE:
			msg = _("end of file");
			break;
		case COB_STATUS_14_OUT_OF_KEY_RANGE:
			msg = _("key out of range");
			break;
		case COB_STATUS_21_KEY_INVALID:
			msg = _("key order not ascending");
			break;
		case COB_STATUS_22_KEY_EXISTS:
			msg = _("record key already exists");
			break;
		case COB_STATUS_23_KEY_NOT_EXISTS:
			msg = _("record key does not exist");
			break;
		case COB_STATUS_30_PERMANENT_ERROR:
			msg = _("permanent file error");
			break;
		case COB_STATUS_31_INCONSISTENT_FILENAME:
			msg = _("inconsistent file name");
			break;
		case COB_STATUS_35_NOT_EXISTS:
			msg = _("file does not exist");
			break;
		case COB_STATUS_37_PERMISSION_DENIED:
			msg = _("permission denied");
			break;
		case COB_STATUS_41_ALREADY_OPEN:
			msg = _("file already open");
			break;
		case COB_STATUS_42_NOT_OPEN:
			msg = _("file not open");
			break;
		case COB_STATUS_43_READ_NOT_DONE:
			msg = _("READ must be executed first");
			break;
		case COB_STATUS_44_RECORD_OVERFLOW:
			msg = _("record overflow");
			break;
		case COB_STATUS_46_READ_ERROR:
			msg = _("READ after unsuccessful READ/START");
			break;
		case COB_STATUS_47_INPUT_DENIED:
			msg = _("READ/START not allowed, file not open for input");
			break;
		case COB_STATUS_48_OUTPUT_DENIED:
			msg = _("WRITE not allowed, file not open for output");
			break;
		case COB_STATUS_49_I_O_DENIED:
			msg = _("DELETE/REWRITE not allowed, file not open for I-O");
			break;
		case COB_STATUS_51_RECORD_LOCKED:
			msg = _("record locked by another file connector");
			break;
		case COB_STATUS_57_I_O_LINAGE:
			msg = _("LINAGE values invalid");
			break;
		case COB_STATUS_61_FILE_SHARING:
			msg = _("file sharing conflict");
			break;
		case COB_STATUS_71_BAD_CHAR:
			msg = _("invalid data in LINE SEQUENTIAL file");
			break;
		/* LCOV_EXCL_START */
		case COB_STATUS_91_NOT_AVAILABLE:
			msg = _("runtime library is not configured for this operation");
			break;
		/* LCOV_EXCL_STOP */
		/* LCOV_EXCL_START */
		default:
			msg = _("unknown file error");
			break;
		/* LCOV_EXCL_STOP */
		}
		err_cause = cob_get_filename_print (cobglobptr->cob_error_file, 1);
		/* FIXME: additional check if referenced program has active code location */
		if (cobglobptr->last_exception_statement == STMT_UNKNOWN) {
			cob_runtime_error (_ ("%s (status = %02d) for file %s"),
				msg, status, err_cause);
		} else {
			cob_runtime_error (_("%s (status = %02d) for file %s on %s"),
				msg, status, err_cause,
				cob_statement_name[cobglobptr->last_exception_statement]);
		}
		break;
	/* LCOV_EXCL_START */
	case COB_FERROR_FUNCTION:
		cob_runtime_error (_("attempt to use non-implemented function"));
		break;
	case COB_FERROR_XML:
		cob_runtime_error (_("attempt to use non-implemented XML I/O"));
		break;
	case COB_FERROR_JSON:
		cob_runtime_error (_("attempt to use non-implemented JSON I/O"));
		break;		
	default:
		/* internal rare error, no need for translation */
		cob_runtime_error ("unknown failure: %d", fatal_error);
		break;
	/* LCOV_EXCL_STOP */
	}
	cob_hard_failure ();
}

void
conf_runtime_error_value (const char *value, const int pos)
{
	const char *name = NULL;

	if (gc_conf[pos].data_type & STS_CNFSET) {
		name = gc_conf[pos].conf_name;
	} else {
		name = gc_conf[pos].env_name;
	}
	conf_runtime_error (0, _("invalid value '%s' for configuration tag '%s'"), value, name);
}

void
conf_runtime_error (const int finish_error, const char *fmt, ...)
{
	va_list args;

	if (!conf_runtime_error_displayed) {
		conf_runtime_error_displayed = 1;
		fputs (_("configuration error:"), stderr);
		putc ('\n', stderr);
	}

	/* Prefix; note: no need to strcmp as we check against
	           the value passed last time */
	if (cob_source_file != last_runtime_error_file
	 || cob_source_line != last_runtime_error_line) {
		last_runtime_error_file = cob_source_file;
		last_runtime_error_line = cob_source_line;
		if (cob_source_file) {
			fprintf (stderr, "%s", cob_source_file);
			if (cob_source_line) {
				fprintf (stderr, ":%u", cob_source_line);
			}
		} else {
			fprintf (stderr, "%s", _("environment variables"));
		}
		fputc(':', stderr);
		fputc(' ', stderr);
	}

	/* Body */
	va_start (args, fmt);
	vfprintf (stderr, fmt, args);
	va_end (args);

	/* Postfix */
	if (!finish_error) {
		putc (';', stderr);
		putc ('\n', stderr);
		putc ('\t', stderr);
	} else {
		putc ('\n', stderr);
		fflush (stderr);
	}
}

#if defined (COB_GEN_SCREENIO)
/* resolve curses library related version information
   stores the information in the version_buffer parameter
   returns the mouse info */
static const char *
get_screenio_and_mouse_info (char *version_buffer, size_t size, const int verbose)
{
	const char	*mouse_support = _("unknown");
	int	major, minor, patch;
#if defined (__PDCURSES__)
	int	opt1, opt2, opt3;
#if defined (PDC_FORCE_UTF8)
	const int utf8 = 1;
#else
	const int utf8 = 0;
#endif
#endif
#if defined (__PDCURSES__) || defined (NCURSES_VERSION)
#if defined (PDC_WIDE) || defined (NCURSES_WIDECHAR)
	const int wide = 1;
#else
	const int wide = 0;
#endif
#endif
	char	buff[56] = {'\0'};

	memset (version_buffer, 0, size--);

	if (verbose) {
		initscr ();
	}
#ifdef HAVE_HAS_MOUSE
	if (verbose) {
		int mouse_available = 0;
		mousemask (ALL_MOUSE_EVENTS, NULL);
		if (has_mouse () == TRUE) mouse_available = 1;
		if (mouse_available) {
			mouse_support = _("yes");
		} else {
			mouse_support = _("no");
		}
}
#elif defined (NCURSES_MOUSE_VERSION)
#if defined (__PDCURSES__)
	mouse_support = _("yes");
#endif
#else
	mouse_support = _("disabled");
#endif

#if defined (__PDCURSES__) || defined (NCURSES_VERSION)
#if defined (__PDCURSES__)
#if defined (PDC_VER_MAJOR)
#define CURSES_CMP_MAJOR	PDC_VER_MAJOR
#define CURSES_CMP_MINOR	PDC_VER_MINOR
#if PDC_VER_MAJOR == 3 && PDC_BUILD >= 3703
#define RESOLVED_PDC_VER
	{
		PDC_VERSION ver;
		PDC_get_version (&ver);
		major = ver.major;
		minor = ver.minor;
		patch = 0;
		opt1 = ver.csize * 8;
		opt2 = ver.flags & PDC_VFLAG_WIDE;
		opt3 = ver.flags & PDC_VFLAG_UTF8;
	}
#elif defined (PDC_HAS_VERSION_INFO)
#define RESOLVED_PDC_VER
	{
		major = PDC_version.ver_major;
		minor = PDC_version.ver_minor;
		patch = PDC_version.ver_change;
		opt1 = PDC_version.chtype_size * 8;
		opt2 = PDC_version.is_wide;
		opt3 = PDC_version.is_forced_utf8;
	}
#endif
#else
#define CURSES_CMP_MAJOR	(PDC_BUILD / 1000)
#define CURSES_CMP_MINOR	(PDC_BUILD - CURSES_CMP_MAJOR * 1000) / 100
	COB_UNUSED (opt1);
	COB_UNUSED (opt2);
	COB_UNUSED (opt3);
#endif
#elif defined (NCURSES_VERSION)
#define CURSES_CMP_MAJOR	NCURSES_VERSION_MAJOR
#define CURSES_CMP_MINOR	NCURSES_VERSION_MINOR
#endif
#if !defined (RESOLVED_PDC_VER)
	snprintf (version_buffer, size, "%s", curses_version ());
	major = 0, minor = 0, patch = 0;
	if ((sscanf (version_buffer, "%55s %55s %d.%d.%d", (char *)&buff, (char *)&buff, &major, &minor, &patch) < 4)
	 && (sscanf (version_buffer, "%55s %d.%d.%d", (char *)&buff, &major, &minor, &patch) < 3)
	 && (sscanf (version_buffer, "%d.%d.%d", &major, &minor, &patch) < 2)) {
		major = 0, minor = 0;
	}
#endif
	if (major == CURSES_CMP_MAJOR && minor == CURSES_CMP_MINOR) {
		snprintf (buff, 55, _("%s, version %d.%d.%d"), WITH_CURSES, major, minor, patch);
	} else if (major != 0) {
		snprintf (buff, 55, _("%s, version %d.%d.%d (compiled with %d.%d)"),
			WITH_CURSES, major, minor, patch, CURSES_CMP_MAJOR, CURSES_CMP_MINOR);
	} else {
		snprintf (buff, 55, _("%s, version %s"), WITH_CURSES, version_buffer);
	}
#if defined (RESOLVED_PDC_VER) 
	{
		const int	chtype_val = (int)sizeof (chtype) * 8;
		char	chtype_def[10] = { '\0' };
		char	wide_def[6] = {'\0'};
		char	utf8_def[6] = {'\0'};
		const char	*match;
		if (chtype_val != opt1) {
			match = "!";
		} else {
			match = "";
		}
		snprintf (chtype_def, 9, "%d[%d%s]", chtype_val, opt1, match);
		if (wide != opt2) {
			match = "!";
		} else {
			match = "";
		}
		snprintf (wide_def, 5, "%d[%d%s]", wide, opt2, match);
		if (wide != opt2) {
			match = "!";
		} else {
			match = "";
		}
		snprintf (utf8_def, 5, "%d[%d%s]", utf8, opt3, match);
		snprintf (version_buffer, size, "%s (CHTYPE=%s, WIDE=%s, UTF8=%s)",
			buff, chtype_def, wide_def, utf8_def);
	}
#undef RESOLVED_PDC_VER
#elif defined (__PDCURSES__)
	snprintf (version_buffer, size, "%s (CHTYPE=%d, WIDE=%d, UTF8=%d)", buff,
		(int)sizeof (chtype) * 8, wide, utf8);
#else
	snprintf (version_buffer, size, "%s (CHTYPE=%d, WIDE=%d)", buff,
		(int)sizeof (chtype) * 8, wide);
#endif

#else /* defined (__PDCURSES__) || defined (NCURSES_VERSION) */
	snprintf (version_buffer, size, "%s (CHTYPE=%d)", WITH_CURSES,
		(int)sizeof (chtype) * 8);
#endif

	if (verbose) {
		size_t curr_size = strlen (version_buffer);
		snprintf (version_buffer + curr_size, size - curr_size, " %s",
			longname ());
		endwin ();
	}

	return mouse_support;
}
#endif

/* resolve math library related version information
   stores the information in the version_buffer parameter */
static void
get_math_info (char *version_buffer, size_t size, const int verbose)
{
	int	major, minor, patch;
	size_t	curr_size;
	COB_UNUSED (verbose);

	memset (version_buffer, 0, size--);
	major = 0, minor = 0, patch = 0;
	(void)sscanf (gmp_version, "%d.%d.%d", &major, &minor, &patch);
	if (major == __GNU_MP_VERSION && minor == __GNU_MP_VERSION_MINOR) {
		curr_size = snprintf (version_buffer, size, _("%s, version %d.%d.%d"), "GMP", major, minor, patch);
	} else {
		curr_size = snprintf (version_buffer, size, _("%s, version %d.%d.%d (compiled with %d.%d)"),
			"GMP", major, minor, patch, __GNU_MP_VERSION, __GNU_MP_VERSION_MINOR);
	}
#if defined (mpir_version)
	major = 0, minor = 0, patch = 0;
	(void)sscanf (mpir_version, "%d.%d.%d", &major, &minor, &patch);
	{
		const char *deli = " - ";
		curr_size += snprintf (version_buffer + curr_size, size - curr_size, "%s", deli);
	}

	if (major == __MPIR_VERSION && minor == __MPIR_VERSION_MINOR) {
		snprintf (version_buffer + curr_size, size - curr_size,
			_("%s, version %d.%d.%d"),
			"MPIR", major, minor, patch);
	} else {
		snprintf (version_buffer + curr_size, size - curr_size,
			_("%s, version %d.%d.%d (compiled with %d.%d)"),
			"MPIR", major, minor, patch, __MPIR_VERSION, __MPIR_VERSION_MINOR);
	}
#else
	COB_UNUSED (curr_size);
#endif
}

/* internal library version as string,
   note: the patchlevel may differ from the package one */
const char * 
libcob_version () 
{

/* FIXME: replace this define by a general one (COB_TREE_DEBUG) _was_ for debugging
          the parse tree only ... */
#if defined (COB_TREE_DEBUG) || defined (_DEBUG)
	{
		int	major, minor;
		major = 0, minor = 0;
		(void)sscanf (PACKAGE_VERSION, "%d.%d", &major, &minor);
		/* LCOV_EXCL_START */
		if (major != __LIBCOB_VERSION || minor != __LIBCOB_VERSION_MINOR) {
			const char *version = CB_XSTRINGIFY (__LIBCOB_VERSION) "."
				CB_XSTRINGIFY (__LIBCOB_VERSION_MINOR);
			cob_runtime_error (_("version mismatch"));
			cob_runtime_hint (_("%s has version %s.%d"), "libcob internally",
						version, __LIBCOB_VERSION_PATCHLEVEL);
			cob_runtime_hint (_("%s has version %s.%d"), "libcob package",
						PACKAGE_VERSION, PATCH_LEVEL);
			cob_hard_failure ();
		}
		/* LCOV_EXCL_STOP */
		{
			int check, patch;
			patch = 0;
			check = set_libcob_version (&major, &minor, &patch);
			/* LCOV_EXCL_START */
			if (check != 0 && check != 3) {
				cob_runtime_error (_("version mismatch"));
				/* untranslated as it is very unlikely to happen */
				cob_runtime_hint ("internal version check differs at %d\n", check);
				cob_hard_failure ();
			}
			/* LCOV_EXCL_STOP */
		}
	}
#endif
	return CB_XSTRINGIFY (__LIBCOB_VERSION) "."
		CB_XSTRINGIFY (__LIBCOB_VERSION_MINOR) "."
		CB_XSTRINGIFY (__LIBCOB_VERSION_PATCHLEVEL);
}

/* internal library version set/compare,
   if 'mayor' is not 0 on entry compares against the given
   values, returns the parameter that is not matching
   given parameters will be set to the internal values on exit
   note: the patchlevel may differ from the package one */
int set_libcob_version (int *mayor, int *minor, int *patch) {
	int ret = 0;
	if (*mayor != 0) {
		if (*mayor != __LIBCOB_VERSION) {
			ret = 1;
		} else if (*minor != __LIBCOB_VERSION_MINOR) {
			ret = 2;
		} else if (*patch != __LIBCOB_VERSION_PATCHLEVEL) {
			ret = 3;
		}
	}
	*mayor = __LIBCOB_VERSION;
	*minor = __LIBCOB_VERSION_MINOR;
	*patch = __LIBCOB_VERSION_PATCHLEVEL;
	return ret;
}

static void set_cob_build_stamp (char *cob_build_stamp)
{
	int		status, day, year;
	char	month[64];

	/* Set up build time stamp */
	memset (cob_build_stamp, 0, (size_t)COB_MINI_BUFF);
	memset (month, 0, sizeof (month));
	day = 0;
	year = 0;
	status = sscanf (__DATE__, "%63s %d %d", month, &day, &year);
	if (status == 3) {
		snprintf (cob_build_stamp, (size_t)COB_MINI_MAX,
			"%s %2.2d %4.4d %s", month, day, year, __TIME__);
	} else {
		snprintf (cob_build_stamp, (size_t)COB_MINI_MAX,
			"%s %s", __DATE__, __TIME__);
	}
}

/* provides a two line output for GnuCOBOL + C compiler and used libraries */
void
print_version_summary (void)
{
	char	cob_build_stamp[COB_MINI_BUFF];

	if(!cob_initialized)
		cob_init_nomain (0, NULL);
	set_cob_build_stamp (cob_build_stamp);
	
	printf ("%s %s (%s), ",
		PACKAGE_NAME, libcob_version(), cob_build_stamp);

	/* note: some compilers use a very long identifier */
	printf ("%s\n", GC_C_VERSION_PRF GC_C_VERSION);

	printf ("%s %d.%d.%d",
#ifdef __MPIR_VERSION
		"MPIR", __MPIR_VERSION, __MPIR_VERSION_MINOR, __MPIR_VERSION_PATCHLEVEL
#else
		"GMP", __GNU_MP_VERSION, __GNU_MP_VERSION_MINOR, __GNU_MP_VERSION_PATCHLEVEL
#endif
	);

#if defined (LIBXML_VERSION)
	printf (", libxml2 %d.%d.%d",
		LIBXML_VERSION / 10000,
		(LIBXML_VERSION - (int)(LIBXML_VERSION / 10000) * 10000) / 100,
		LIBXML_VERSION % 100);
#endif

#if defined (CJSON_VERSION_MAJOR)
	printf (", cJSON %d.%d.%d",
		CJSON_VERSION_MAJOR, CJSON_VERSION_MINOR, CJSON_VERSION_PATCH);
#endif
#if defined (JSON_C_MAJOR_VERSION)
	printf (", JSON-c %d.%d.%d",
		JSON_C_MAJOR_VERSION, JSON_C_MINOR_VERSION, JSON_C_MICRO_VERSION);
#endif
#if defined (__PDCURSES__)
	printf (", %s %d.%d",
#ifdef PDC_VER_YEAR	/* still the correct distinction in 2020 */
		"PDCursesMod",
#else
		"PDCurses",
#endif
		CURSES_CMP_MAJOR, CURSES_CMP_MINOR);
#ifdef PDC_VER_CHANGE
	printf (".%d", PDC_VER_CHANGE);
#elif defined (PDC_BUILD)
	printf (" (%d)", PDC_BUILD);
#endif
#endif
#if defined (NCURSES_VERSION_MAJOR)
	printf (", %s %d.%d.%d",
#ifdef NCURSES_WIDECHAR
		"ncursesw",
#else
		"ncurses",
#endif
		NCURSES_VERSION_MAJOR, NCURSES_VERSION_MINOR, NCURSES_VERSION_PATCH);
#endif

#if defined	(WITH_CISAM)
	printf (", %s", cob_io_version (COB_IO_CISAM, 0));
#endif
#if defined	(WITH_DISAM)
	printf (", %s", cob_io_version (COB_IO_DISAM, 0));
#endif
#if defined	(WITH_VISAM)
	printf (", %s", cob_io_version (COB_IO_VISAM, 0));
#endif
#if defined	(WITH_VBISAM)
	printf (", %s", cob_io_version (COB_IO_VBISAM, 0));
#endif
#if defined	(WITH_ODBC)
	printf (", %s", cob_io_version (COB_IO_ODBC, 0));
#endif
#if defined	(WITH_OCI)
	printf (", %s", cob_io_version (COB_IO_OCI, 0));
#endif
#if defined	(WITH_DB)
	printf (", %s", cob_io_version (COB_IO_BDB, 0));
#endif
#if defined	(WITH_LMDB)
	printf (", %s", cob_io_version (COB_IO_LMDB, 0));
#endif


	putchar ('\n');

}

void
print_version (void)
{
	char	cob_build_stamp[COB_MINI_BUFF];

	set_cob_build_stamp (cob_build_stamp);

	printf ("libcob (%s) %s.%d\n",
		PACKAGE_NAME, PACKAGE_VERSION, PATCH_LEVEL);
	puts ("Copyright (C) 2022 Free Software Foundation, Inc.");
	printf (_("License LGPLv3+: GNU LGPL version 3 or later <%s>"), "https://gnu.org/licenses/lgpl.html");
	putchar ('\n');
	puts (_("This is free software; see the source for copying conditions.  There is NO\n"
	        "warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE."));
	printf (_("Written by %s"), "Keisuke Nishida, Roger While, Ron Norman, Simon Sobisch, Edward Hart");
	putchar ('\n');

	/* TRANSLATORS: This msgid is intended as the "Packaged" msgid, %s expands to date and time */
	printf (_("Built     %s"), cob_build_stamp);
	putchar ('\n');
	/* TRANSLATORS: This msgid is intended as the "Built" msgid, %s expands to date and time */
	printf (_("Packaged  %s"), COB_TAR_DATE);
	putchar ('\n');
}

void
print_info (void)
{
	print_info_detailed (0);
}

void
print_info_detailed (const int verbose)
{
	char	screenio_info[150];
	const char *mouse_support;

	char	buff[56] = { '\0' };
	char	*s;
	int		num;

	if(!cob_initialized)
		cob_init_nomain (0, NULL);
	/* resolving screenio related information before anything else as this
	   function will possibly run initscr + endwin and therefore
	   may interfer with other output */
#if defined (COB_GEN_SCREENIO)
	mouse_support = get_screenio_and_mouse_info
	((char*)&screenio_info, sizeof (screenio_info), verbose);
#else
	snprintf ((char *)&screenio_info, sizeof(screenio_info) - 1,
		"%s", _("disabled"));
	mouse_support = _("disabled");
#endif

	print_version ();
	putchar ('\n');
	puts (_("build information"));
	var_print (_("build environment"), 	COB_BLD_BUILD, "", 0);
	var_print ("CC", COB_BLD_CC, "", 0);
	/* Note: newline because most compilers define a long version string (> 30 characters) */
	var_print (_("C version"), GC_C_VERSION_PRF GC_C_VERSION, "", 0);
	var_print ("CPPFLAGS", COB_BLD_CPPFLAGS, "", 0);
	var_print ("CFLAGS", COB_BLD_CFLAGS, "", 0);
	var_print ("LD", COB_BLD_LD, "", 0);
	var_print ("LDFLAGS", COB_BLD_LDFLAGS, "", 0);
	putchar ('\n');

	puts (_("GnuCOBOL information"));

	var_print ("COB_MODULE_EXT", COB_MODULE_EXT, "", 0);
#if 0 /* only relevant for cobc */
	var_print ("COB_OBJECT_EXT", COB_OBJECT_EXT, "", 0);
	var_print ("COB_EXE_EXT", COB_EXE_EXT, "", 0);
#endif

#if	defined (USE_LIBDL) || defined (_WIN32)
	var_print (_("dynamic loading"), 	"system", "", 0);
#else
	var_print (_("dynamic loading"), 	"libtool", "", 0);
#endif

	if (verbose) {
#ifdef	COB_PARAM_CHECK
		var_print ("\"CBL_\" param check", 	_("enabled"), "", 0);
#else
		var_print ("\"CBL_\" param check", 	_("disabled"), "", 0);
#endif
	}
#ifdef COB_64_BIT_POINTER
	var_print ("64bit-mode", 	_("yes"), "", 0);
#else
	var_print ("64bit-mode", 	_("no"), "", 0);
#endif

#ifdef	COB_LI_IS_LL
	var_print ("BINARY-C-LONG", 	_("8 bytes"), "", 0);
#else
	var_print ("BINARY-C-LONG", 	_("4 bytes"), "", 0);
#endif

#ifdef WORDS_BIGENDIAN
	var_print (_("endianness"),		_("big-endian"), "", 0);
#else
	var_print (_("endianness"),		_("little-endian"), "", 0);
#endif

#ifdef COB_EBCDIC_MACHINE
	var_print (_("native character set"),		"EBCDIC", "", 0);
#else
	var_print (_("native character set"),		"ASCII", "", 0);
#endif

	snprintf (buff, sizeof (buff), "%d", WITH_VARSEQ);
	var_print (_("variable file format"), buff, "", 0);
	if ((s = getenv ("COB_VARSEQ_FORMAT")) != NULL) {
		var_print ("COB_VARSEQ_FORMAT", s, "", 1);
	}

#ifdef	WITH_SEQRA_EXTFH
	var_print (_("sequential file handler"),	"EXTFH (obsolete)", "", 0);
#else
	var_print (_("sequential file handler"),	_("built-in"), "", 0);
#endif

#if defined	(WITH_INDEX_EXTFH)
	var_print (_("indexed file handler"), 		"EXTFH (obsolete)", "", 0);
#endif
	num = 0;
#if defined	(WITH_CISAM)
	var_print (_("indexed file handler"), 		cob_io_version (COB_IO_CISAM, verbose), "", 0);
	num++;
#endif
#if defined	(WITH_DISAM)
	var_print (_("indexed file handler"), 		cob_io_version (COB_IO_DISAM, verbose), "", 0);
	num++;
#endif
#if defined	(WITH_VISAM)
	var_print (_("indexed file handler"), 		cob_io_version (COB_IO_VISAM, verbose), "", 0);
	num++;
#endif
#if defined	(WITH_VBISAM)
	var_print (_("indexed file handler"), 		cob_io_version (COB_IO_VBISAM, verbose), "", 0);
	num++;
#endif
#if defined	(WITH_ODBC)
	var_print (_("indexed file handler"), 		cob_io_version (COB_IO_ODBC, verbose), "", 0);
	num++;
#endif
#if defined	(WITH_OCI)
	var_print (_("indexed file handler"), 		cob_io_version (COB_IO_OCI, verbose), "", 0);
	num++;
#endif
#if defined	(WITH_DB)
	var_print (_("indexed file handler"), 		cob_io_version (COB_IO_BDB, verbose), "", 0);
	num++;
#endif
#if defined	(WITH_LMDB)
	var_print (_("indexed file handler"), 		cob_io_version (COB_IO_LMDB, verbose), "", 0);
	num++;
#endif
#if defined(WITH_INDEXED)
	if (num > 1)
	var_print (_("default indexed handler"),	cob_io_version (WITH_INDEXED, -1), "", 0);
#endif

	if (num == 0)
	var_print (_("indexed file handler"), 		_("disabled"), "", 0);

#if defined(WITH_FILE_FORMAT)
	if (WITH_FILE_FORMAT == COB_FILE_IS_MF)
		var_print (_("default file format"),	"-ffile-format=mf", "", 0);
	else if (WITH_FILE_FORMAT == COB_FILE_IS_GC)
		var_print (_("default file format"),	"-ffile-format=gc", "", 0);
#endif

	{
		char	math_info[115];
		get_math_info ((char*)&math_info, sizeof (math_info), verbose);
		var_print (_("mathematical library"), 	(char *)&math_info, "", 0);
	}

#ifdef WITH_XML2
	{
		int	major, minor, patch;
		major = LIBXML_VERSION / 10000;
		minor = (LIBXML_VERSION - major * 10000) / 100 ;
		patch = LIBXML_VERSION - major * 10000 - minor * 100;
		snprintf (buff, 55, _("%s, version %d.%d.%d"),
			"libxml2", major, minor, patch);
		var_print (_("XML library"), 		buff, "", 0);
		LIBXML_TEST_VERSION
		xmlCleanupParser ();
	}
#else
	var_print (_("XML library"), 		_("disabled"), "", 0);
#endif


#if defined (WITH_CJSON)
	{
		int	major, minor, patch;
		major = 0, minor = 0, patch = 0;
		(void)sscanf (cJSON_Version(), "%d.%d.%d", &major, &minor, &patch);
		if (major == CJSON_VERSION_MAJOR && minor == CJSON_VERSION_MINOR) {
			snprintf (buff, 55, _("%s, version %d.%d.%d"),
				"cJSON", major, minor, patch);
		} else {
			snprintf (buff, 55, _("%s, version %d.%d.%d (compiled with %d.%d)"),
				"cJSON", major, minor, patch, CJSON_VERSION_MAJOR, CJSON_VERSION_MINOR);
		}
	}
	var_print (_("JSON library"), 		buff, "", 0);

#elif defined (WITH_JSON_C)
	{
		int	major, minor, patch;
		major = 0, minor = 0, patch = 0;
		(void)sscanf (json_c_version (), "%d.%d.%d", &major, &minor, &patch);
		if (major == JSON_C_MAJOR_VERSION && minor == JSON_C_MINOR_VERSION) {
			snprintf (buff, 55, _("%s, version %d.%d.%d"),
				"json-c", major, minor, patch);
		} else {
			snprintf (buff, 55, _("%s, version %d.%d.%d (compiled with %d.%d)"),
				"json-c", major, minor, patch, JSON_C_MAJOR_VERSION, JSON_C_MINOR_VERSION);
		}
	}
	var_print (_("JSON library"), 		buff, "", 0);
#else
	var_print (_("JSON library"), 		_("disabled"), "", 0);
#endif

	var_print (_("extended screen I/O"),	(char*)&screenio_info, "", 0);
	var_print (_("mouse support"),		mouse_support, "", 0);

#ifdef COB_DEBUG_LOG
	var_print ("DEBUG_LOG",		_("enabled"), "", 0);
#endif
}

void
print_runtime_conf ()
{
	unsigned int 	i, j, k, vl, dohdg, hdlen, plen, plen2;
	char	value[COB_MEDIUM_BUFF], orgvalue[COB_MINI_BUFF], *env;

#ifdef ENABLE_NLS	/* note: translated version of definition values */
#ifdef	HAVE_SETLOCALE
	const char	*s;
#endif
	setting_group[1] = _("CALL configuration");
	setting_group[2] = _("File I/O configuration");
	setting_group[3] = _("Screen I/O configuration");
	setting_group[4] = _("Miscellaneous");
	setting_group[5] = _("System configuration");
#endif

	printf ("%s %s.%d ", PACKAGE_NAME, PACKAGE_VERSION, PATCH_LEVEL);
	puts (_("runtime configuration"));
	if (cobsetptr->cob_config_file) {
		strncpy (value, _("via"), (size_t)COB_MEDIUM_MAX);
		value[COB_MEDIUM_MAX] = 0;
		hdlen = (unsigned int)strlen (value) + 3;

		/* output path of main configuration file */
		printf (" %s  ", value);
		plen = 80 - hdlen;
		strncpy (value, cobsetptr->cob_config_file[0], (size_t)COB_MEDIUM_MAX);
		value[COB_MEDIUM_MAX] = 0;
		vl = (unsigned int)strlen (value);
		for (k = 0; vl > plen; vl -= plen, k += plen) {
			printf ("%.*s\n%-*s", plen, &value[k], hdlen, "");
		}
		printf ("%s\n", &value[k]);

		/* output path of additional configuration files */
		for (i = 1; i < cobsetptr->cob_config_num; i++) {
			printf ("%*d  ", hdlen - 2, i);
			strncpy (value, cobsetptr->cob_config_file[i], (size_t)COB_MEDIUM_MAX);
			value[COB_MEDIUM_MAX] = 0;
			vl = (unsigned int)strlen (value);
			for (k = 0; vl > plen; vl -= plen, k += plen) {
				printf ("%.*s\n%-*s", plen, &value[k], hdlen, "");
			}
			printf ("%s\n", &value[k]);
		}

	}
	putchar ('\n');
	strcpy (value, "todo");
	hdlen = 15;
	for (i = 0; i < NUM_CONFIG; i++) {
		j = (unsigned int)strlen (gc_conf[i].env_name);
		if (j > hdlen)
			hdlen = j;
		j = (unsigned int)strlen (gc_conf[i].conf_name);
		if (j > hdlen)
			hdlen = j;
	}

	for (j = 1; j < GRP_MAX; j++) {
		dohdg = 1;
		for (i = 0; i < NUM_CONFIG; i++) {
			if (gc_conf[i].env_group == (int)j) {
				if (dohdg) {
					dohdg = 0;
					if (j > 1) {
						putchar ('\n');
					}
					printf (" %s\n", setting_group[j]);
				}
				/* Convert value back into string and display it */
				get_config_val (value, i, orgvalue);
				if ((gc_conf[i].data_type & STS_ENVSET)
				 || (gc_conf[i].data_type & STS_FNCSET)) {
					putchar (' ');
					if (gc_conf[i].data_type & STS_FNCSET) {
						printf ("   ");
					} else
					if (gc_conf[i].data_type & STS_CNFSET) {
						printf ("Ovr");
					} else {
						printf ("env");
						if (gc_conf[i].data_loc == (int)offsetof(cob_settings,cob_preload_str)
						 && cobsetptr->cob_preload_str_set != NULL) {
							printf (": %-*s : ", hdlen, gc_conf[i].env_name);
							printf ("%s\n", cobsetptr->cob_preload_str_set);
							printf ("eval");
						}
					}
					printf (": %-*s : ", hdlen, gc_conf[i].env_name);
				} else
				if (gc_conf[i].data_type & STS_CNFSET) {
					if ((gc_conf[i].data_type & STS_ENVCLR)) {
						printf ("    : %-*s : ", hdlen, gc_conf[i].env_name);
						puts (_("... removed from environment"));
					}
					if (gc_conf[i].config_num > 0) {
						printf ("  %d ", gc_conf[i].config_num);
					} else {
						printf ("    ");
					}
					if (gc_conf[i].data_loc == (int)offsetof(cob_settings,cob_preload_str)
					 && cobsetptr->cob_preload_str_set != NULL) {
						printf (": %-*s : ",hdlen,
							gc_conf[i].set_by > 0 ? gc_conf[i].env_name
							: gc_conf[i].conf_name);
						printf ("%s\n",cobsetptr->cob_preload_str_set);
						printf ("eval");
					}
					if (gc_conf[i].set_by > 0) {
						printf (": %-*s : ", hdlen, gc_conf[i].env_name);
					} else {
						printf (": %-*s : ", hdlen, gc_conf[i].conf_name);
					}
				} else if (gc_conf[i].env_name) {
					if (gc_conf[i].config_num > 0){
						printf ("  %d ", gc_conf[i].config_num);
					} else {
						printf ("    ");
					}
					printf (": %-*s : ", hdlen, gc_conf[i].env_name);
					if ((gc_conf[i].data_type & STS_ENVCLR)) {
						puts (_("... removed from environment"));
						continue;
					}
				} else {
					printf ("    : %-*s : ", hdlen, gc_conf[i].conf_name);
				}
				vl = (unsigned int)strlen (value);
				plen = 71 - hdlen;
				if (vl < (unsigned int)min_conf_length) {
					plen2 = min_conf_length - vl;
				} else if (vl == (unsigned int)min_conf_length) {
					plen2 = 1;
				} else {
					plen2 = 0;
				}
				for (k = 0; vl > plen; vl -= plen, k += plen) {
					printf ("%.*s\n      %-*s : ", plen, &value[k], hdlen, "");
				}
				printf ("%s", &value[k]);
				printf ("%.*s", plen2, "               ");
				if (orgvalue[0]) {
					printf (" (%s)", orgvalue);
				}
				if (gc_conf[i].set_by != 0) {
					putchar (' ');
					if (gc_conf[i].set_by != FUNC_NAME_IN_DEFAULT) {
						printf (_("(set by %s)"), gc_conf[gc_conf[i].set_by].env_name);
					} else {
						printf (_("(set by %s)"), gc_conf[i].default_val);
					}
				}
				if (!(gc_conf[i].data_type & STS_ENVSET)
				 && !(gc_conf[i].data_type & STS_CNFSET)
				 && !(gc_conf[i].data_type & STS_FNCSET)) {
					putchar (' ');
					if ((gc_conf[i].data_type & STS_RESET)) {
						printf (_("(reset)"));
					} else
					if (strcmp (value, not_set) != 0) {
						printf (_("(default)"));
					} else
					if (gc_conf[i].default_val
					 && strcmp (gc_conf[i].default_val, not_set) == 0) {
						printf (_("(default)"));
					}
				}
				putchar ('\n');
			}
		}
		if (j == GRP_FILE) {
			if ((env = cob_get_env ("SQL_HIGH_VALUES",orgvalue)) != NULL)
				printf ("    : %-*s : %s\n", hdlen, orgvalue, env);
			else if ((env = cob_get_env ("SQL_HIGH_VALUE",orgvalue)) != NULL)
				printf ("    : %-*s : %s\n", hdlen, orgvalue, env);
			if ((env = cob_get_env ("IO_OPTIONS",orgvalue)) != NULL)
				printf ("    : %-*s : %s\n", hdlen, orgvalue, env);
			if ((env = cob_get_env ("IX_OPTIONS",orgvalue)) != NULL)
				printf ("    : %-*s : %s\n", hdlen, orgvalue, env);
			if ((env = cob_get_env ("RL_OPTIONS",orgvalue)) != NULL)
				printf ("    : %-*s : %s\n", hdlen, orgvalue, env);
			if ((env = cob_get_env ("SQ_OPTIONS",orgvalue)) != NULL)
				printf ("    : %-*s : %s\n", hdlen, orgvalue, env);
			if ((env = cob_get_env ("LS_OPTIONS",orgvalue)) != NULL)
				printf ("    : %-*s : %s\n", hdlen, orgvalue, env);
		}
	}


#ifdef	HAVE_SETLOCALE
#ifdef	ENABLE_NLS
	s = cob_getenv_value ("LOCALEDIR");
	printf ("    : %-*s : %s\n", hdlen, "LOCALEDIR", s ? s : LOCALEDIR);
#endif
	printf ("    : %-*s : %s\n", hdlen, "LC_CTYPE", setlocale (LC_CTYPE, NULL));
	printf ("    : %-*s : %s\n", hdlen, "LC_NUMERIC", setlocale (LC_NUMERIC, NULL));
	printf ("    : %-*s : %s\n", hdlen, "LC_COLLATE", setlocale (LC_COLLATE, NULL));
#ifdef	LC_MESSAGES
	printf ("    : %-*s : %s\n", hdlen, "LC_MESSAGES", setlocale (LC_MESSAGES, NULL));
#endif
	printf ("    : %-*s : %s\n", hdlen, "LC_MONETARY", setlocale (LC_MONETARY, NULL));
	printf ("    : %-*s : %s\n", hdlen, "LC_TIME", setlocale (LC_TIME, NULL));
#endif
	if (cobsetptr->cob_config_num == 0) {
		printf (_(" No %s was used\n"),"runtime.cfg");
	}
}

cob_settings *
cob_get_settings_ptr ()
{
	return cobsetptr;
}

void
cob_init_nomain (const int argc, char **argv)
{
	check_mainhandle = 0;
	cob_init (argc, argv);
}

void
cob_common_init (void *setptr)
{
#ifdef	ENABLE_NLS
	{
		struct stat	localest;
		const char * localedir;

		localedir = cob_getenv_value ("LOCALEDIR");
		if (localedir != NULL
		 && !stat (localedir, &localest)
		 && (S_ISDIR (localest.st_mode))) {
			bindtextdomain (PACKAGE, localedir);
		} else {
			bindtextdomain (PACKAGE, LOCALEDIR);
		}
		textdomain (PACKAGE);
	}
#endif

#ifdef	_WIN32
	/* Allows running tests under Win */
	{
		int use_unix_lf = 0;
		char *s = getenv ("COB_UNIX_LF");

		if (s != NULL) {
			if (setptr) {
				set_config_val_by_name (s, "unix_lf", NULL);
				use_unix_lf = cobsetptr->cob_unix_lf;
			} else
			if (*s == 'Y' || *s == 'y' ||
			    *s == 'O' || *s == 'o' ||
			    *s == 'T' || *s == 't' ||
			    *s == '1') {
				use_unix_lf = 1;
			}
		}
		if (use_unix_lf) {
			(void)_setmode (_fileno (stdin), _O_BINARY);
			(void)_setmode (_fileno (stdout), _O_BINARY);
			(void)_setmode (_fileno (stderr), _O_BINARY);
		}
	}
#endif
}

/* normal call, but longjmp back on exit (1),
   errors (-1), hard errors (-2) or signals (-3) */
int
cob_call_with_exception_check (const char *name, const int argc, void **argv)
{	
#ifndef COB_WITHOUT_JMP
	int ret;
	return_jmp_buffer_set = 1;
	ret = setjmp (return_jmp_buf);
	if (ret) {
		return_jmp_buffer_set = 0;
		return ret;
	}
#endif
	exit_code = cob_call (name, argc, argv);
	return 0;
}

void
cob_init (const int argc, char **argv)
{
	char		*s;
#if	defined (HAVE_READLINK) || defined (HAVE_GETEXECNAME)
	const char	*path;
#endif
	int		i;

	/* Ensure initialization is only done once. Within generated modules and
	   libcob this is already ensured, but an external caller may call this
	   function again */
	if (cob_initialized) {
#if 0	/* Simon: We may raise a runtime warning/error in the future here */
		cob_runtime_warning ("%s called more than once", "cob_init");
#endif
		return;
	}

#ifdef __GLIBC__
	{
		/* 
		 * GNU libc may write a stack trace to /dev/tty when malloc
		 * detects corruption.  If LIBC_FATAL_STDERR_ is set to any
		 * nonempty string, it writes to stderr instead. See:
		 *https://code.woboq.org/userspace/glibc/sysdeps/posix/libc_fatal.c.html
		 */
		if (getenv ((const char*)"LIBC_FATAL_STDERR_") == NULL ) {
			(void)putenv ((char*)"LIBC_FATAL_STDERR_=keep_off_the_grass");
		}
	}
#endif

	cob_set_signal ();

	cob_alloc_base = NULL;
	cob_local_env = NULL;
	cob_last_sfile = NULL;
	commlnptr = NULL;
	basext = NULL;
	sort_keys = NULL;
	sort_collate = NULL;
	cob_source_file = NULL;
	exit_hdlrs = NULL;
	hdlrs = NULL;
	commlncnt = 0;
	sort_nkeys = 0;
	cob_source_line = 0;
	cob_local_env_size = 0;

	current_arg = 1;

	cob_argc = argc;
	cob_argv = argv;
	if (argc && argv)
		cob_setup_env (argv[0]);

	/* Get global structure */
	cobglobptr = cob_malloc (sizeof (cob_global));
	cobglobptr->cob_call_params = 0;

	/* Get settings structure */
	cobsetptr = cob_malloc (sizeof (cob_settings));

	cob_initialized = 1;

#ifdef	HAVE_SETLOCALE
	/* Prime the locale from user settings */
	s = setlocale (LC_ALL, "");
	if (s) {
		/* Save initial values */
		cobglobptr->cob_locale_orig = cob_strdup (s);
		s = setlocale (LC_CTYPE, NULL);
		if (s) {
			cobglobptr->cob_locale_ctype = cob_strdup (s);
		}
		s = setlocale (LC_COLLATE, NULL);
		if (s) {
			cobglobptr->cob_locale_collate = cob_strdup (s);
		}
#ifdef	LC_MESSAGES
		s = setlocale (LC_MESSAGES, NULL);
		if (s) {
			cobglobptr->cob_locale_messages = cob_strdup (s);
		}
#endif
		s = setlocale (LC_MONETARY, NULL);
		if (s) {
			cobglobptr->cob_locale_monetary = cob_strdup (s);
		}
		s = setlocale (LC_NUMERIC, NULL);
		if (s) {
			cobglobptr->cob_locale_numeric = cob_strdup (s);
		}
		s = setlocale (LC_TIME, NULL);
		if (s) {
			cobglobptr->cob_locale_time = cob_strdup (s);
		}
		/* Set to standard "C" locale for COBOL */
		setlocale (LC_NUMERIC, "C");
		setlocale (LC_CTYPE, "C");
		/* Save changed locale */
		s = setlocale (LC_ALL, NULL);
		if (s) {
			cobglobptr->cob_locale = cob_strdup (s);
		}
	}
#endif
	cob_init_sig_descriptions ();

	cob_common_init (cobsetptr);

	/* Load runtime configuration file */
	if (cob_load_config () < 0) {
		cob_hard_failure ();
	}

	/* Copy COB_PHYSICAL_CANCEL from settings (internal) to global structure */
	cobglobptr->cob_physical_cancel = cobsetptr->cob_physical_cancel;

	/* Internal Debug Log */
	if (cobsetptr->cob_debug_log) {
#ifdef COB_DEBUG_LOG
		cob_debug_open ();
#endif
	}

	/* Call inits with cobsetptr to get the addresses of all */
	/* Screen-IO might be needed for error outputs */
	cob_init_screenio (cobglobptr, cobsetptr);
	cob_init_numeric (cobglobptr);
	cob_init_strings (cobglobptr);
	cob_init_move (cobglobptr, cobsetptr);
	cob_init_intrinsic (cobglobptr);
	cob_init_fileio (cobglobptr, cobsetptr);
	cob_init_call (cobglobptr, cobsetptr, check_mainhandle);
	cob_init_cobcapi (cobglobptr, cobsetptr);
	cob_init_termio (cobglobptr, cobsetptr);
	cob_init_reportio (cobglobptr, cobsetptr);
	cob_init_mlio (cobglobptr);

	/* Set up library routine stuff */
	cobglobptr->cob_term_buff = cob_malloc ((size_t)COB_MEDIUM_BUFF);

	/* Set switches */
	for (i = 0; i <= COB_SWITCH_MAX; ++i) {
		char	switch_name[16];
		sprintf (switch_name, "COB_SWITCH_%d", i);
		s = getenv (switch_name);
		if (s && (*s == '1' || strcasecmp (s, "ON") == 0)) {
			cob_switch[i] = 1;
		} else {
			cob_switch[i] = 0;
		}
	}

#ifndef	HAVE_DESIGNATED_INITS
	init_statement_list ();
#endif

	/* Get user name if not set via environment already */
	if (cobsetptr->cob_user_name == NULL) {
#if defined (_WIN32)
	/* note: only defined manual (needs additional link to advapi32): */
#if defined (HAVE_GETUSERNAME)
		unsigned long bsiz = COB_MINI_MAX;
		char user_name[COB_MINI_BUFF];
		if (GetUserName (user_name, &bsiz)) {
			set_config_val_by_name (user_name, "username", "GetUserName()");
		}
#endif
#elif !defined(__OS400__)
		s = getlogin ();
		if (s) {
			set_config_val_by_name (s, "username", "getlogin()");
		}
#endif
#if 0	/* likely not needed, if unset then empty */
		if (cobsetptr->cob_user_name == NULL) {
			set_config_val_by_name (_("unknown"), "username", "cob_init()");
		}
#endif
	}

#if defined(_MSC_VER)
	get_function_ptr_for_precise_time ();
#endif

	/* This must be last in this function as we do early return */
	/* from certain ifdef's */

#ifdef	_WIN32
	s = cob_malloc ((size_t)COB_LARGE_BUFF);
	i = GetModuleFileNameA (NULL, s, COB_LARGE_MAX);
	if (i > 0 && i < COB_LARGE_BUFF) {
		cobglobptr->cob_main_argv0 = cob_strdup (s);
		cob_free (s);
		return;
	}
	cob_free (s);
#elif	defined (HAVE_READLINK)
	path = NULL;
	if (!access ("/proc/self/exe", R_OK)) {
		path = "/proc/self/exe";
	} else if (!access ("/proc/curproc/file", R_OK)) {
		path = "/proc/curproc/file";
	} else if (!access ("/proc/self/path/a.out", R_OK)) {
		path = "/proc/self/path/a.out";
	}
	if (path) {
		s = cob_malloc ((size_t)COB_LARGE_BUFF);
		i = (int)readlink (path, s, (size_t)COB_LARGE_MAX);
		if (i > 0 && i < COB_LARGE_BUFF) {
			s[i] = 0;
			cobglobptr->cob_main_argv0 = cob_strdup (s);
			cob_free (s);
			return;
		}
		cob_free (s);
	}
#endif

#ifdef	HAVE_GETEXECNAME
	path = getexecname ();
	if (path) {
#ifdef	HAVE_REALPATH
		s = cob_malloc ((size_t)COB_LARGE_BUFF);
		if (realpath (path, s) != NULL) {
			cobglobptr->cob_main_argv0 = cob_strdup (s);
		} else {
			cobglobptr->cob_main_argv0 = cob_strdup (path);
		}
		cob_free (s);
#else
		cobglobptr->cob_main_argv0 = cob_strdup (path);
#endif
		return;
	}
#endif

	if (argc && argv && argv[0]) {
#if	defined (HAVE_CANONICALIZE_FILE_NAME)
		/* Returns malloced path or NULL */
		cobglobptr->cob_main_argv0 = canonicalize_file_name (argv[0]);
#elif	defined (HAVE_REALPATH)
		s = cob_malloc ((size_t)COB_LARGE_BUFF);
		if (realpath (argv[0], s) != NULL) {
			cobglobptr->cob_main_argv0 = cob_strdup (s);
		}
		cob_free (s);
#elif	defined	(_WIN32)
		/* Returns malloced path or NULL */
		cobglobptr->cob_main_argv0 = _fullpath (NULL, argv[0], 1);
#endif
		if (!cobglobptr->cob_main_argv0) {
			cobglobptr->cob_main_argv0 = cob_strdup (argv[0]);
		}
	} else {
		cobglobptr->cob_main_argv0 = cob_strdup (_("unknown"));
	}
	/* The above must be last in this function as we do early return */
	/* from certain ifdef's */
	if (cobglobptr->cob_main_argv0)
		cob_setup_env (cobglobptr->cob_main_argv0);
}

/* Compute a hash value based on the string given */
unsigned int
cob_get_name_hash (const char *name)
{
	unsigned int hash;
	int	i, ch;
	hash = 0x074FADE1;		/* Seed value to agitate the bits */
	for (i=0; name[i] != 0; i++) {
		if(islower(name[i]))
			ch = toupper(name[i]);
		else
			ch = name[i];
		hash = (hash << 5) | (hash >> 27);
		hash = hash + ((ch & 0x7F) * (i + 3));
	}
	if (hash == 0)
		hash = 1;
	return hash;
}

/*
 * Set special runtime options:
 * Currently this is only FILE * for trace and printer output
 * or to reload the runtime configuration after changing environment
 */
void
cob_set_runtime_option (enum cob_runtime_option_switch opt, void *p)
{
	switch (opt) {
	case COB_SET_RUNTIME_TRACE_FILE:
		cobsetptr->cob_trace_file = (FILE *)p;
		if (p) {
			cobsetptr->external_trace_file = 1;
		} else {
			cobsetptr->external_trace_file = 0;
		}
		break;
	case COB_SET_RUNTIME_DISPLAY_PRINTER_FILE:
		/* note: if set cob_display_print_file is always external */
		cobsetptr->cob_display_print_file = (FILE *)p;
		break;
	case COB_SET_RUNTIME_DISPLAY_PUNCH_FILE:
		/* note: if set cob_display_punch_file is always external */
		if (cobsetptr->cob_display_punch_filename != NULL) {
			/* if previously opened by libcob: close and free pointer to filename */
			if (cobsetptr->cob_display_punch_file != NULL) {
				fclose (cobsetptr->cob_display_punch_file);
			}
			cob_free (cobsetptr->cob_display_punch_filename);
			cobsetptr->cob_display_punch_filename = NULL;
		}
		cobsetptr->cob_display_punch_file = (FILE *)p;
		break;
	case COB_SET_RUNTIME_DUMP_FILE:
		/* note: if set cob_dump_file is always external (libcob only opens it on abort)
		         therefore we don't need to close the old one */
		cobsetptr->cob_dump_file = (FILE *)p;
		if (!cobsetptr->cob_dump_file) {
			if (cobsetptr->cob_dump_filename) {
				cob_free (cobsetptr->cob_dump_filename);
			}
			cobsetptr->cob_dump_filename = cob_strdup ("NONE");
		}
		break;
	case COB_SET_RUNTIME_RESCAN_ENV:
		cob_rescan_env_vals ();
		break;
	default:
		cob_runtime_warning (_("%s called with unknown option: %d"),
			"cob_set_runtime_option", opt);
	}
	return;
}

/*
 * Return current value of special runtime options
 */
void *
cob_get_runtime_option (enum cob_runtime_option_switch opt)
{
	switch (opt) {
	case COB_SET_RUNTIME_TRACE_FILE:
		return (void*)cobsetptr->cob_trace_file;
	case COB_SET_RUNTIME_DISPLAY_PRINTER_FILE:
		return (void*)cobsetptr->cob_display_print_file;
	case COB_SET_RUNTIME_DISPLAY_PUNCH_FILE:
		/* only externalize if not aquired by libcob */
		if (cobsetptr->cob_display_punch_filename != NULL) {
			return NULL;
		}
		return (void*)cobsetptr->cob_display_punch_file;
	case COB_SET_RUNTIME_DUMP_FILE:
		return (void*)cobsetptr->cob_dump_file;
	default:
		cob_runtime_error (_("%s called with unknown option: %d"),
			"cob_get_runtime_option", opt);
	}
	return NULL;
}

/* output the COBOL-view of the stacktrace to the given target,
   does an early exit if 'target' is NULL, 
   'target' is FILE *  and should be flushed before */
void
cob_stack_trace (void *target)
{
	if (target == NULL || !cobglobptr || !COB_MODULE_PTR) {
		return;
	}
	dump_trace_started |= DUMP_TRACE_ACTIVE_TRACE;
	cob_stack_trace_internal ((FILE *)target, 1, 0);
	dump_trace_started ^= DUMP_TRACE_ACTIVE_TRACE;
}

static void
flush_target (FILE *target)
{
	/* exit early in the case of no module loaded at all,
	   possible to happen for example when aborted from cob_check_version of first module */
	if (!COB_MODULE_PTR
	 || (   COB_MODULE_PTR->module_stmt == 0
	     && COB_MODULE_PTR->next == NULL)) {
		return;
	}

	if (target == stderr
	 || target == stdout) {
		fflush (stdout);
		fflush (stderr);
	} else {
		fflush (target);
	}
}

/* output the COBOL-view of the stacktrace to the given target,
   does an early exit if 'target' is NULL,
   'target' is FILE *, output similar to GDBs backtrace command,
   "count" to limit to the first / last entries,
   REMARK: other than in GDB 0 means "full output" */
void
cob_backtrace (void *target, int count)
{
	if (target == NULL) {
		return;
	}
	if (!cobglobptr || !COB_MODULE_PTR) {
		flush_target (target);
		fputc (' ', target);
		/* TRANSLATORS: This msgid is shown for a requested but empty stack trace. */
		fputs (_("No COBOL runtime elements on stack."), target);
		fputc ('\n', target);
		return;
	}
	dump_trace_started |= DUMP_TRACE_ACTIVE_TRACE;
	cob_stack_trace_internal ((FILE *)target, 0, count);
	dump_trace_started ^= DUMP_TRACE_ACTIVE_TRACE;
}

/* internal output the procedure stack entry to the given target */
static void
output_procedure_stack_entry (const int file_no,
		const char *section, const char *paragraph,
		const char *source_file, const unsigned int source_line)
{
	if (!section && !paragraph) {
		return;
	}
	write_or_return_arr (file_no, "\n\t");
	if (section && paragraph) {
		write_or_return_str (file_no, paragraph);
		write_or_return_arr (file_no, " OF ");
		write_or_return_str (file_no, section);
	} else {
		if (section) {
			write_or_return_str (file_no, section);
		} else {
			write_or_return_str (file_no, paragraph);
		}
	}
	write_or_return_arr (file_no, " at ");
	write_or_return_str (file_no, source_file);
	write_or_return_arr (file_no, ":");
	write_or_return_int (file_no, (int)source_line);
}

/* internal output the COBOL-view of the stacktrace to the given target */
void
cob_stack_trace_internal (FILE *target, int verbose, int count)
{
	cob_module	*mod;
	int	first_entry = 0;
	int i, k;
	int file_no;

	/* exit early in the case of no module loaded at all,
	   possible to happen for example when aborted from cob_check_version of first module */
	if (!COB_MODULE_PTR
	 || (   COB_MODULE_PTR->module_stmt == 0
	     && COB_MODULE_PTR->next == NULL)) {
		return;
	}

	if (target == stderr) {
		file_no = STDERR_FILENO;
	} else {
		flush_target (target);
		file_no = fileno (target);
	}

	k = 0;
	if (count < 0) {
		for (mod = COB_MODULE_PTR, i = 0; mod; mod = mod->next, i++) {
			if (mod->next == mod
			 || k++ == MAX_MODULE_ITERS) {
				break;	/* messages in same checks below */
			}
		}
		first_entry = i + count;
	}

	if (verbose) {
		write_or_return_arr (file_no, "\n");
	}
	k = 0;
	for (mod = COB_MODULE_PTR, i = 0; mod; mod = mod->next, i++) {
		if (i < first_entry) {
			continue;
		}
		if (count > 0 && count == i) {
			break;
		}
		if (mod->module_stmt != 0
		 && mod->module_sources) {
			const unsigned int source_file_num = COB_GET_FILE_NUM (mod->module_stmt);
			const unsigned int source_line = COB_GET_LINE_NUM (mod->module_stmt);
			const char *source_file = mod->module_sources[source_file_num];
			write_or_return_arr (file_no, " ");
			if (!verbose) {
				write_or_return_str (file_no, mod->module_name);
				write_or_return_arr (file_no, " at ");
				write_or_return_str (file_no, source_file);
				write_or_return_arr (file_no, ":");
				write_or_return_int (file_no, (int)source_line);
			} else
			if (mod->statement == STMT_UNKNOWN
			 && !mod->section_name
			 && !mod->paragraph_name) {
				/* GC 3.1 output, now used for "no source location / no trace" case */
				write_or_return_arr (file_no, "Last statement of ");
				if (mod->module_type == COB_MODULE_TYPE_FUNCTION) {
					write_or_return_arr (file_no, "FUNCTION ");
				}
				write_or_return_arr (file_no, "\"");
				write_or_return_str (file_no, mod->module_name);
				write_or_return_arr (file_no, "\" was at line ");
				write_or_return_int (file_no, (int)source_line);
				write_or_return_arr (file_no, " of ");
				write_or_return_str (file_no, source_file);
			} else
			if (!mod->section_name && !mod->paragraph_name) {
				/* special case: there _would_ be data,
				   but there's no procedure defined in the program */
				write_or_return_arr (file_no, "Last statement of ");
				if (mod->module_type == COB_MODULE_TYPE_FUNCTION) {
					write_or_return_arr (file_no, "FUNCTION ");
				}
				write_or_return_arr (file_no, "\"");
				write_or_return_str (file_no, mod->module_name);
				write_or_return_arr (file_no, "\" was ");
				write_or_return_str (file_no, cob_statement_name[mod->statement]);
				write_or_return_arr (file_no, " at line ");
				write_or_return_int (file_no, (int)source_line);
				write_or_return_arr (file_no, " of ");
				write_or_return_str (file_no, source_file);
			} else {
				/* common case when compiled with runtime checks enabled: statement and
				   procedure known - the later is printed from the stack entry with the
				   source location by the following call */
				write_or_return_arr (file_no, "Last statement of ");
				if (mod->module_type == COB_MODULE_TYPE_FUNCTION) {
					write_or_return_arr (file_no, "FUNCTION ");
				}
				write_or_return_arr (file_no, "\"");
				write_or_return_str (file_no, mod->module_name);
				write_or_return_arr (file_no, "\" was ");
				write_or_return_str (file_no, cob_statement_name[mod->statement]);
			}
			output_procedure_stack_entry (file_no, mod->section_name, mod->paragraph_name,
					source_file, source_line);
			if (mod->frame_ptr) {
				struct cob_frame_ext *perform_ptr = mod->frame_ptr;
				int frame_max = 512; /* max from -fstack-size */
				while (frame_max--) {
					const unsigned int ffile_num = COB_GET_FILE_NUM (perform_ptr->module_stmt);
					const unsigned int fline = COB_GET_LINE_NUM (perform_ptr->module_stmt);
					const char *ffile = mod->module_sources[ffile_num];
					if (perform_ptr->section_name) {
						/* marker for "root frame" - at ENTRY */
						if (perform_ptr->section_name[0] == 0) {
							write_or_return_arr (file_no, "\n\tENTRY ");
							write_or_return_str (file_no, perform_ptr->paragraph_name);
							write_or_return_arr (file_no, " at ");
							write_or_return_str (file_no, ffile);
							write_or_return_arr (file_no, ":");
							write_or_return_int (file_no, (int)fline);
							break;
						}
					}
					output_procedure_stack_entry (file_no,
						perform_ptr->section_name, perform_ptr->paragraph_name,
						ffile, fline);
					perform_ptr--;
				}
			}
		} else {
			if (verbose) {
				write_or_return_arr (file_no, "Last statement of ");
				if (mod->module_type == COB_MODULE_TYPE_FUNCTION) {
					write_or_return_arr (file_no, "FUNCTION ");
				}
				write_or_return_arr (file_no, "\"");
				write_or_return_str (file_no, mod->module_name);
				write_or_return_arr (file_no, "\" unknown");
			} else {
				write_or_return_str (file_no, mod->module_name);
				write_or_return_arr (file_no, " at unknown");
			}
		}
		write_or_return_arr (file_no, "\n");
		if (mod->next == mod) {
			/* not translated as highly unexpected */
			write_or_return_arr (file_no, "FIXME: recursive mod (stack trace)\n");
			break;
		}
		if (k++ == MAX_MODULE_ITERS) {
			/* not translated as highly unexpected */
			write_or_return_arr (file_no,
				"max module iterations exceeded, possible broken chain\n");
			break;
		}
			
	}
	if (mod) {
		write_or_return_arr (file_no, " ");
		write_or_return_str (file_no, more_stack_frames_msgid);
		write_or_return_arr (file_no, "\n");
	}

	if (verbose && cob_argc != 0) {
		size_t ia;
		write_or_return_arr (file_no, " Started by ");
		write_or_return_str (file_no, cob_argv[0]);
		write_or_return_arr (file_no, "\n");
		for (ia = 1; ia < (size_t)cob_argc; ++ia) {
			write_or_return_arr (file_no, "\t");
			write_or_return_str (file_no, cob_argv[ia]);
			write_or_return_arr (file_no, "\n");
		}
	}
}

FILE *
cob_get_dump_file (void)
{
	if (cobsetptr->cob_dump_file != NULL) {	/* If DUMP active, use that */
		return cobsetptr->cob_dump_file;
	} else if (cobsetptr->cob_dump_filename != NULL) {	/* DUMP file defined */
		if (cob_check_env_false(cobsetptr->cob_dump_filename)) {
			return NULL;
		}
		cobsetptr->cob_dump_file = cob_open_logfile (cobsetptr->cob_dump_filename);
		if (cobsetptr->cob_dump_file != NULL) {
			return cobsetptr->cob_dump_file;
		}
		/* could not open the file
		   unset the filename for not referencing it later */
		cob_free (cobsetptr->cob_dump_filename);
		cobsetptr->cob_dump_filename = NULL;
		/* Fall-through */
	}
	if (cobsetptr->cob_trace_file != NULL) {	/* If TRACE active, use that */
		return cobsetptr->cob_trace_file;
	} else {
		return stderr;
	}
}

static const char *sectname[] = {
			"CONSTANT","FILE","WORKING-STORAGE",
			"LOCAL","LINKAGE","SCREEN",
			"REPORT","COMMUNICATION"};
static unsigned char sectdump[] = {
	0, COB_DUMP_FD, COB_DUMP_WS, COB_DUMP_LO, COB_DUMP_LS,
	COB_DUMP_SC, COB_DUMP_RD, COB_DUMP_RD};
#define SYM_MAX_IDX 8
static int	sym_idx = 0;
static int	sym_sub [SYM_MAX_IDX];
static int	sym_size[SYM_MAX_IDX];

static jmp_buf save_sig_env;
static void 
catch_sig_jmp (int sig)
{ 
	longjmp(save_sig_env, sig);
}

void
cob_sym_get_field (cob_field *f, cob_symbol *sym, int k)
{
	int		j;
	f->size = sym[k].size;
	f->attr = sym[k].attr;
	if (sym[k].is_indirect == SYM_ADRS_PTR) {
		memcpy (&f->data, sym[k].adrs, sizeof(void*));
		/* 
		 * If field has not yet been referenced, the address will be NULL
		 *   Scan up to parent and use that base address plus 'roffset'
		 * If the field had been referenced the field address will be set
		 *   and 'offset' will be ZERO
		 */
		for (j = k; f->data == NULL; j = sym[j].parent) {
			if (sym[j].parent == 0) {	/* Base of COBOL Record */
				memcpy (&f->data, sym[j].adrs, sizeof(void*));
				if (f->data != NULL)
					f->data += sym[k].roffset;
				return;
			}
		}
		if (f->data != NULL)
			f->data += sym[k].offset;
	} else if (sym[k].is_indirect == SYM_ADRS_FIELD) {
		memcpy (f, sym[k].adrs, sizeof(cob_field));
	} else {
		f->data = sym[k].adrs;
		if (f->data != NULL)
			f->data += sym[k].offset;
	}
}

int
cob_sym_get_occurs (cob_symbol *sym, int k)
{
	cob_field	d0;
	int			occmax;
	if (sym[k].has_depend) {
		cob_sym_get_field (&d0, sym, sym[k].depending);
		occmax = cob_get_int (&d0);
		if (occmax > sym[k].occurs)
			occmax = sym[k].occurs;
	} else {
		occmax = sym[k].occurs;
	}
	return occmax;
}

static void cob_dump_table ( cob_symbol *sym, int k);
static void
cob_dump_sub ( cob_symbol *sym, int k, int sub)
{
	cob_field	f0;
	int		j;

	sym_sub  [sym_idx-1] = sub;
	cob_sym_get_field (&f0, sym, k);
	cob_dump_field ( sym[k].level, sym[k].name?sym[k].name:"FILLER", 
					&f0, 0, sym_idx, 
					sym_sub [0], sym_size [0], 
					sym_sub [1], sym_size [1], 
					sym_sub [2], sym_size [2],
					sym_sub [3], sym_size [3], 
					sym_sub [4], sym_size [4], 
					sym_sub [5], sym_size [5],
					sym_sub [6], sym_size [6], 
					sym_sub [7], sym_size [7]);
	if (sym[k].is_group) {
		for (j = k+1; sym[j].parent == k; j++) {
			if (sym[j].occurs > 1) {
				cob_dump_table (sym, j);
				if ((j = sym[j].sister) == 0)
					break;
				j--;
			} else {
				cob_dump_sub (sym, j, sub);
			}
		}
	}
}

static void
cob_dump_table ( cob_symbol *sym, int k)
{
	int		j, occmax;

	occmax = cob_sym_get_occurs (sym, k);
	sym_size [sym_idx++] = sym[k].size;
	for (j=0; j < occmax; j++)
		cob_dump_sub (sym, k, j);
	sym_size [--sym_idx] = 0;
}

static void
cob_dump_symbols (cob_module *mod)
{
	static int skipgrp;
	static cob_symbol *skpsym; 
	int			j, k, sect;
	cob_symbol *sym;
	cob_field	f0;
	char		msg[80];
	FILE		*fp;
	cob_file	*fl;

	fp = cob_get_dump_file ();
	sect = 255;
	sym = mod->module_symbols;
	mod->flag_debug_trace |= COB_MODULE_DUMPED;

	fprintf (fp, _("Dump Program-Id %s from %s compiled %s"),
					mod->module_name, mod->module_source, mod->module_formatted_date);
	fputc ('\n', fp);
	for (k = 0; k < mod->num_symbols; k++) {
		if (sym[k].is_internal) {
			continue;
		}
		if (sym[k].is_redef) {
			j = k;
			while (j < mod->num_symbols 
				&& sym[j].is_redef
				&& sym[j].sister ) {
				k = j;
				j = sym[j].sister;
			}
			continue;
		}
		if (sym[k].section == 0
		|| !(mod->flag_dump_sect & sectdump[sym[k].section]))
			continue;
		if (sect != sym[k].section) {
			sect = sym[k].section;
			if (!sym[k].is_file)
				cob_dump_output (sectname[sect]);
		}
		if (sym[k].is_file) {
			memcpy (&fl, sym[k].adrs, sizeof(void*));
			cob_dump_file (sym[k].name, fl);
			continue;
		}
		skipgrp = 0;
		cob_sym_get_field (&f0, sym, k);
		cob_set_dump_signal ((void *)catch_sig_jmp);
		if (setjmp (save_sig_env) != 0) {
			skipgrp = 1;
			while (sym[k].parent > 0)
				k = sym[k].parent;
			if (skpsym == &sym[k])
				goto skipsym;
			skpsym = &sym[k];
			sprintf (msg," >>>> Dump of %s aborted! <<<< !!",
						sym[k].name?sym[k].name:"FILLER");
			cob_dump_output (msg);
		} else if (sym[k].occurs > 1) {
			for (sym_idx = 0; sym_idx < SYM_MAX_IDX; sym_idx++)
				sym_sub [sym_idx] = sym_size [sym_idx] = 0;
			sym_idx = 0;
			cob_dump_table ( sym, k);
			if (sym[k].is_group)
				skipgrp = 1;
		} else {
			cob_dump_field ( sym[k].level, sym[k].name?sym[k].name:"FILLER", &f0, 0, 0);
		}
		if (skipgrp) {
skipsym:
			if (sym[k].sister) {
				k = sym[k].sister;
			} else {
				while (++k < mod->num_symbols
					&& sym[k].level > 1
					&& sym[k].level != 77);
			}
			k--;
		} else if (f0.data == NULL) {
			if (sym[k].sister) {
				k = sym[k].sister - 1;
				continue;
			} else if (k+1 < mod->num_symbols
					&& sym[k].section != sym[k+1].section) {
				continue;
			} else if (sym[k].level == 1 
					|| sym[k].level == 77) {
				break;
			}
		}
	}
	sprintf (msg, "END OF DUMP - %s", mod->module_name);
	cob_dump_output (msg);
	fputc ('\n', fp);
	fflush (fp);
}

static void
cob_dump_module (char *reason)
{
	cob_module	*mod;
	int		wants_dump = 0;
	int k;

	/* Was any module compiled with -fdump? */
	k = 0;
	for (mod = COB_MODULE_PTR; mod; mod = mod->next) {
		if (mod->flag_dump_ready) {
			wants_dump = 1;
		}
		if (mod->next == mod) {
			/* not translated as highly unexpected */
			fputs ("FIXME: recursive mod (module dump)\n", stderr);
			break;
		}
		if (k++ == MAX_MODULE_ITERS) {
			/* not translated as highly unexpected */
			fputs ("max module iterations exceeded, possible broken chain\n", stderr);
			break;
		}
		if (mod->flag_dump_ready) {
			break;
		}
	}

	if (wants_dump) {
		FILE		*fp;
		char		*previous_locale = NULL;
		fp = cob_get_dump_file ();
		/* explicit disabled dump */
		if (fp == NULL) {
			return;
		}
		if (fp != stderr) {
			if (reason) {
				if (reason[0] == 0) {
					reason = (char *)_ ("unknown");
				}
				fputc ('\n', fp);
				fprintf (fp, _("Module dump due to %s"), reason);
				fputc ('\n', fp);
			}
			if (fp != stdout) {
				/* was already sent to stderr before this function was called,
				   so skip here for stdout/stderr ... */
				if (!(dump_trace_started & DUMP_TRACE_ACTIVE_TRACE)) {
					dump_trace_started |= DUMP_TRACE_ACTIVE_TRACE;
					cob_stack_trace_internal (fp, 1, 0);
					dump_trace_started ^= DUMP_TRACE_ACTIVE_TRACE;
				}
			}
			fflush (stdout);
		} else {
			fflush (stderr);
		}

		fputc ('\n', fp);
		if (cobglobptr->cob_locale_ctype) {
			previous_locale = setlocale (LC_CTYPE, NULL);
			setlocale (LC_CTYPE, cobglobptr->cob_locale_ctype);
		}
		k = 0;
		for (mod = COB_MODULE_PTR; mod; mod = mod->next) {
			if (mod->module_symbols
			 && mod->num_symbols > 0
			 && !(mod->flag_debug_trace & COB_MODULE_DUMPED)) {
					cob_dump_symbols (mod);
			}
			if (mod->next == mod
			 || k++ == MAX_MODULE_ITERS) {
				break;
			}
		}
		if (previous_locale) {
			setlocale (LC_CTYPE, previous_locale);
		}
		if (fp != stdout && fp != stderr) {
			char * fname = NULL;
			if (cobsetptr->cob_dump_filename) {
				fname = cobsetptr->cob_dump_filename;
			} else
			if (cobsetptr->cob_trace_file == fp
			 && cobsetptr->cob_trace_filename != NULL
			 && !cobsetptr->external_trace_file) {
				fname = cobsetptr->cob_trace_filename;
			}
			if (fname != NULL) {
				fputc ('\n', stderr);
				fprintf (stderr, _("dump written to %s"), fname);
				fputc ('\n', stderr);
				fflush (stderr);
			}
		}
	}
}

/*
 * Allocate field attribute; 
 * Used by subroutine entry when called by C code
 */
cob_field_attr *
cob_alloc_attr (int type, int digits, int scale, int flags)
{
	struct dyn_attr	*da;
	for (da = dyn_attr_list; da; da = da-> next) {
		if (da->attr.type == type
		&& da->attr.digits == digits
		&& da->attr.scale == scale
		&& da->attr.flags == flags)
			return &da->attr;
	}
	da = cob_cache_malloc (sizeof(struct dyn_attr));
	da->next = dyn_attr_list;
	dyn_attr_list = da;
	da->attr.type   = (unsigned short)type;
	da->attr.digits = (unsigned short)digits;
	da->attr.scale  = (short)scale;
	da->attr.flags  = (unsigned short)flags;
	return &da->attr;
}

/*
 * Check for "envname" and if not present try "COB_envname"
 */
char *
cob_get_env (const char *envname, char *envused)
{
	static char wrk[64];
	char	*env;
	unsigned char *uenvname;
	int		i,j;
	int		haslwr = 0;
	if (envused == NULL)
		envused = (void*)wrk;
	strcpy(envused, envname);
	if ((env = getenv (envused)) != NULL)
		return env;
	if (memcmp(envname,"COB_",4) == 0)
		strcpy(envused, envname + 4);
	else
		sprintf(envused, "COB_%s", envname);
	if ((env = getenv (envused)) != NULL)
		return env;
	uenvname = (unsigned char *)envname;
	for (i=0; uenvname[i] != 0; i++) {
		if (islower (uenvname[i])) {
			haslwr = 1;
			break;
		}
	}
	if (!haslwr) {	/* Try all lower case */
		for (i=j=0; uenvname[j] != 0; i++,j++) {
			if (isupper (uenvname[j]))
				envused[i] = tolower(uenvname[j]);
			else
				envused[i] = uenvname[j];
		}
		envused[i] = 0;
		if ((env = getenv (envused)) != NULL)
			return env;
		strcpy(envused,"cob_");
		for (i=4,j=0; uenvname[j] != 0; i++,j++) {
			if (isupper (uenvname[j]))
				envused[i] = tolower(uenvname[j]);
			else
				envused[i] = uenvname[j];
		}
		envused[i] = 0;
		if ((env = getenv (envused)) != NULL)
			return env;
	}
	if (haslwr) {	/* Try all upper case */
		for (i=j=0; uenvname[j] != 0; i++,j++) {
			if (islower (uenvname[j]))
				envused[i] = toupper(uenvname[j]);
			else
				envused[i] = uenvname[j];
		}
		envused[i] = 0;
		if ((env = getenv (envused)) != NULL)
			return env;
		strcpy(envused,"COB_");
		for (i=4,j=0; uenvname[j] != 0; i++,j++) {
			if (islower (uenvname[j]))
				envused[i] = toupper(uenvname[j]);
			else
				envused[i] = uenvname[j];
		}
		envused[i] = 0;
		if ((env = getenv (envused)) != NULL)
			return env;
	}
	return NULL;
}

#ifdef COB_DEBUG_LOG
/******************************/
/* Routines for COB_DEBUG_LOG */
/******************************/

/* Check env var value and open log file */
/*
 * Env var is  COB_DEBUG_LOG
 * Env Var string is a series of keyword=value parameters where keywords:
 * L=x  - options: T for trace level, W for warnings, N for normal, A for ALL
 * M=yy - module:  RW for report writer, the 2 char code is tabled and compared
 *        with the value coded on DEBUG_LOG("yy",("format",args));
 * O=path/file - file name to write log data to, default is: cob_debug_log.$$
 *        note:  replacements already done in common setting handling
 */
void
cob_debug_open (void)
{
	char	*debug_env = cobsetptr->cob_debug_log;
	int		i, j;
	char	module_name[4];
	char	log_opt;
	char	logfile[COB_SMALL_BUFF];

	cob_debug_check_open = 0;
	logfile[0] = 0;
	if (debug_env == NULL)
		return;

	for (i=0; debug_env[i] != 0; i++) {
		/* skip separator */
		if (debug_env[i] == ','
		 || debug_env[i] == ';')
			continue;

		/* debugging flags (not include in file name) */
		if (debug_env[i + 1] == '=') {
			log_opt = debug_env[i];
			i += 2;

			switch (log_opt) {

			case 'M':	/* module to debug */
			case 'm':
				for (j = 0; j < DEBUG_MOD_LEN; i++) {
					if (debug_env[i] == ','
					 || debug_env[i] == ';'
					 || debug_env[i] == 0) {
						break;
					}
					module_name[j++] = debug_env[i];
				}
				module_name[j] = 0;
				/* note: special module ALL is checked later */
				for (j = 0; j < DEBUG_MOD_MAX && cob_debug_modules[j][0] > ' '; j++) {
					if (strcasecmp (cob_debug_modules[j], module_name) == 0) {
						break;
					}
				}
				if (j < DEBUG_MOD_MAX && cob_debug_modules[j][0] <= ' ') {
					strcpy (cob_debug_modules[j], module_name);
				}
				if (debug_env[i] == 0) i--;
				break;

			case 'L':	/* logging options */
			case 'l':
				log_opt = debug_env[i];
				switch (log_opt) {
				case 'T':	/* trace */
				case 't':
					cob_debug_log_time = cob_debug_level = 3;
					break;
				case 'W':	/* warnings */
				case 'w':
					cob_debug_level = 2;
					break;
				case 'N':	/* normal */
				case 'n':
					cob_debug_level = 0;
					break;
				case 'A':	/* all */
				case 'a':
					cob_debug_level = 9;
					break;
				default:	/* Unknown log option, just ignored for now */
					i--;
					break;
				}
				break;

			case 'O':	/* output name for logfile */
			case 'o':
				for (j = 0; j < COB_SMALL_MAX; i++) {
					if (debug_env[i] == ','
					 || debug_env[i] == ';'
					 || debug_env[i] == 0) {
						break;
					}
					logfile[j++] = debug_env[i];
				}
				logfile[j] = 0;
				if (debug_env[i] == 0) i--;
				break;

			default:	/* Unknown x=, just ignored for now */
				break;
			}
		} else {
			/* invalid character, just ignored for now */
			/* note: this allows for L=WARNING (but also for L=WUMPUS) */
		}
	}

	/* set default logfile if not given */
	if (logfile[0] == 0) {
		sprintf (logfile, "cob_debug_log.%d", cob_sys_getpid());
	}
	/* store filename for possible unlink (empty log file) */
	cob_debug_file_name = cob_strdup (logfile);

	/* ensure trace file is open if we use this as debug log and exit */
	if (cobsetptr->cob_trace_filename 
	 && strcmp (cobsetptr->cob_trace_filename, cob_debug_file_name) == 0) {
		cob_check_trace_file ();
		cob_debug_file = cobsetptr->cob_trace_file;
		return;
	}

	/* open logfile */
	cob_debug_file = cob_open_logfile (cob_debug_file_name);
	if (cob_debug_file == NULL) {
		/* developer-only msg - not translated */
		cob_runtime_error ("error '%s' opening COB_DEBUG_LOG '%s', resolved from '%s'",
			cob_get_strerror (), cob_debug_file_name, cobsetptr->cob_debug_log);
		return;
	}
}

/* Determine if DEBUGLOG is to be allowed */
int
cob_debug_logit (int level, char *module)
{
	int	i;
	if (cob_debug_check_open) {
		if (cobsetptr == NULL)
			cob_init (0, NULL);
		cob_debug_open ();
	}
	if (cob_debug_file == NULL) {
		return 1;
	}
	if (level > cob_debug_level) {
		return 1;
	}
	for (i=0; i < DEBUG_MOD_MAX && cob_debug_modules[i][0] > ' '; i++) {
		if (strcasecmp ("ALL", cob_debug_modules[i]) == 0) {
			cob_debug_mod = (char*)module;
			return 0;						/* Logging is allowed */
		}
		if (strcasecmp (module,cob_debug_modules[i]) == 0) {
			cob_debug_mod = (char*)&cob_debug_modules[i];
			return 0;						/* Logging is allowed */
		}
	}
	return 1;
}

/* Write logging line */
static int cob_debug_hdr = 1;
static unsigned int cob_debug_prv_line = 0;
int
cob_debug_logger (const char *fmt, ...)
{
	va_list		ap;
	int		ln;
	struct cob_time time;

	if (cob_debug_file == NULL) {
		return 0;
	}
	if (*fmt == '~') {			/* Force line# out again to log file */
		fmt++;
		cob_debug_prv_line = -1;
		cob_debug_hdr = 1;
	}
	if (cob_debug_hdr) {
		cob_get_source_line ();
		if (cob_debug_log_time) {
			time = cob_get_current_datetime (DTR_FULL);
			fprintf (cob_debug_file, "%02d:%02d:%02d.%02d ", time.hour, time.minute,
							time.second, time.nanosecond / 10000000);
		}
		if (cob_debug_mod) {
			fprintf (cob_debug_file, "%-3s:", cob_debug_mod);
		}
		if (cob_source_file) {
			fprintf (cob_debug_file, " %s :", cob_source_file);
		}
		if (cob_source_line && cob_source_line != cob_debug_prv_line) {
			fprintf (cob_debug_file, "%5d : ", cob_source_line);
			cob_debug_prv_line = cob_source_line;
		} else {
			fprintf (cob_debug_file, "%5s : ", " ");
		}
		cob_debug_hdr = 0;
	}
	va_start (ap, fmt);
	vfprintf (cob_debug_file, fmt, ap);
	va_end (ap);
	ln = strlen(fmt);
	if (fmt[ln-1] == '\n') {
		cob_debug_hdr = 1;
		fflush (cob_debug_file);
	}
	return 0;
}

static int			/* Return TRUE if word is repeated 16 times */
repeatWord(
	char	*match,	/* 4 bytes to match */
	char	*mem)	/* Memory area to match repeated value */
{
	if(memcmp(match, &mem[0], 4) == 0
	&& memcmp(match, &mem[4], 4) == 0
	&& memcmp(match, &mem[8], 4) == 0
	&& memcmp(match, &mem[12], 4) == 0)
		return 1;
	return 0;
}

/* Hexdump of memory */
int
cob_debug_dump (void *pMem, int len)
{
#define dMaxPerLine	24
#define dMaxHex ((dMaxPerLine*2)+(dMaxPerLine/4-1))
	register int i, j, k;
	register char	c, *mem = pMem;
	char	lastWord[4];
	char	hex[dMaxHex+4],chr[dMaxPerLine+4];
	int		adrs = 0;

	if (cob_debug_file == NULL)
		return 0;
	memset (lastWord,0xFD, 4);
	for (i=0; i < len; ) {
		for (j=k=0; j < dMaxPerLine && (i+j) < len; j++) {
			k += sprintf(&hex[k],"%02X",mem[i+j]&0xFF);
			if ((j % 4) == 3 )
				hex[k++] = ' ';
		}
		if (k && hex[k-1] == ' ')
			hex[k-1] = 0;
		hex[k] = 0;

		k = 0;
		for (j=0; j<dMaxPerLine && (i+j)<len; j++) {
			c = mem[i+j];
			chr[k++] =  c >= ' ' && c < 0x7f ? c : '.';
		}
		chr[k++] = 0;

		fprintf (cob_debug_file," %6.6X : %-*s '%s'\n",adrs+i,dMaxHex,hex,chr);
		if ((i + dMaxPerLine) < len )
			memcpy( (char *)lastWord, (char *)&mem[i+dMaxPerLine-4], j<4?j:4);
		i += dMaxPerLine;
		if( (i + (16*2)) < len
		&& repeatWord (lastWord, &mem[i])
		&& repeatWord (lastWord, &mem[i+dMaxPerLine])) {
			fprintf (cob_debug_file," %6.6X : ",adrs+i);
			while (i < len - 16
			&& repeatWord(lastWord,&mem[i]))
				i += 16;
			fprintf (cob_debug_file," thru %6.6X same as last word\n",adrs+i-1);
		}
	}
	fflush (cob_debug_file);

	return 0;
}
#else
/* Dummy logging routines when --enable-debuglog not chosen */
/* This will avoid possible runtime issues of a missing symbol */
int cob_debug_logit (int level, char *module) { return 1; }
int cob_debug_dump (void *pMem, int len) { return 0; }
int cob_debug_logger (const char *fmt, ...) { return 0; }
#endif


#ifndef	HAVE_DESIGNATED_INITS
void
init_statement_list (void)
{
	cob_statement_name[STMT_UNKNOWN] = "UNKNOWN";
#define COB_STATEMENT(ename,str) \
	cob_statement_name[ename] = str;
#include "statement.def"	/* located and installed next to common.h */
#undef COB_STATEMENT
}
#endif

#ifdef _MSC_VER

#include <crtdbg.h>

BOOL WINAPI DllMain (HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpReserved)
{
	COB_UNUSED (hinstDLL);
	COB_UNUSED (lpReserved);

	if (fdwReason == DLL_PROCESS_ATTACH) {
	/* Programs compiled with MSVC will by default display a popup
	   window on some errors. In general, we do not want that,
	   so we disable them, unless explicitly requested. */
	if (!IsDebuggerPresent() && !getenv ("DEBUG_POPUPS_WANTED")) {
		_CrtSetReportMode(_CRT_WARN, _CRTDBG_MODE_FILE);
		_CrtSetReportFile(_CRT_WARN, _CRTDBG_FILE_STDERR);
		_CrtSetReportMode(_CRT_ERROR, _CRTDBG_MODE_FILE);
		_CrtSetReportFile(_CRT_ERROR, _CRTDBG_FILE_STDERR);
		_CrtSetReportMode(_CRT_ASSERT, _CRTDBG_MODE_FILE);
		_CrtSetReportFile(_CRT_ASSERT, _CRTDBG_FILE_STDERR);
	}
    }
    return TRUE;
}

#endif

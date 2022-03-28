/*
   Copyright (C) 2001-2022 Free Software Foundation, Inc.

   Authors:
   Keisuke Nishida, Roger While, Ron Norman, Simon Sobisch, Brian Tiffin,
   Edward Hart, Dave Pitts

   This file is part of GnuCOBOL.

   The GnuCOBOL compiler is free software: you can redistribute it
   and/or modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   GnuCOBOL is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GnuCOBOL.  If not, see <https://www.gnu.org/licenses/>.
*/

/* #define DEBUG_REPLACE */

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
#include <time.h>
#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef	HAVE_SIGNAL_H
#include <signal.h>
#endif

#ifdef	_WIN32
#define	WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef MOUSE_MOVED
#include <direct.h>
#include <io.h>
#endif

#ifdef	HAVE_LOCALE_H
#include <locale.h>
#endif

#include <limits.h>


#include "cobc.h"
#include "tree.h"

#include "../libcob/cobgetopt.h"

#ifdef COB_INTERNAL_XREF
enum xref_type {
	XREF_FIELD,
	XREF_FILE,
	XREF_LABEL,
	XREF_FUNCTION
};
#endif

struct strcache {
	struct strcache	*next;
	void		*val;
};

/* Compile level */
#define	CB_LEVEL_PREPROCESS	1
#define	CB_LEVEL_TRANSLATE	2
#define	CB_LEVEL_COMPILE	3
#define	CB_LEVEL_ASSEMBLE	4
#define	CB_LEVEL_MODULE		5
#define	CB_LEVEL_LIBRARY	6
#define	CB_LEVEL_EXECUTABLE	7

/* Info display limits */
#define	CB_IMSG_SIZE		24
#define	CB_IVAL_SIZE		(74 - CB_IMSG_SIZE - 4)

#define	COBC_ADD_STR(v,x,y,z)	cobc_add_str (&v, &v##_size, x, y, z);
#define	COBC_INV_PAR		_("invalid parameter: %s")

#define	CB_TEXT_LIST_ADD(y,z)	y = cb_text_list_add (y, z)
#define	CB_TEXT_LIST_CHK(y,z)	y = cb_text_list_chk (y, z)


/* Global variables */

const char		*cb_source_file = NULL;
const char		*cb_dialect = NULL;
const char		*cb_cobc_build_stamp = NULL;
const char		*demangle_name = NULL;
const char		*cb_storage_file_name = NULL;
const char		*cb_call_extfh = NULL;
const char		*cb_sqldb_schema = NULL;
struct cb_text_list	*cb_include_list = NULL;
struct cb_text_list	*cb_intrinsic_list = NULL;
struct cb_text_list	*cb_extension_list = NULL;
struct cb_text_list	*cb_static_call_list = NULL;
struct cb_text_list	*cb_early_exit_list = NULL;
char			**cb_saveargv = NULL;
const char		*cob_config_dir = NULL;
const char		*cob_schema_dir = NULL;
FILE			*cb_storage_file = NULL;
FILE			*cb_listing_file = NULL;

/* Listing structures and externals */

#define CB_LINE_LENGTH	1024 /* hint: we only read PPLEX_BUF_LEN bytes */
#define CB_READ_AHEAD	800 /* lines to read ahead */

/* TODO: add new compiler configuration flags for this*/
#define CB_MARGIN_A	cb_indicator_column
#define CB_MARGIN_B	11	/* careful, for COBOL 85 this would be 11,
						   for COBOL 2002 (removed it) would be 7 */
#define CB_INDICATOR	CB_MARGIN_A - 1
#define CB_SEQUENCE	cb_text_column /* the only configuration available...*/
#define CB_ENDLINE	(cb_text_column + cb_indicator_column + 1)

#define CB_MAX_LINES	55
#define CB_LIST_PICSIZE 80
#define CB_PRINT_LEN	132

char	print_data[CB_PRINT_LEN + 1];
size_t	pd_off;

#define IS_DEBUG_LINE(line) ((line)[CB_INDICATOR] == 'D')
#define IS_CONTINUE_LINE(line) ((line)[CB_INDICATOR] == '-')
#define IS_COMMENT_LINE(line) \
   ((line)[CB_INDICATOR] == '*' || (line)[CB_INDICATOR] == '/')

FILE			*cb_src_list_file = NULL;
int			cb_listing_page = 0;
int			cb_listing_wide = 0;
unsigned int		cb_lines_per_page = CB_MAX_LINES;
int			cb_listing_xref = 0;
#define			CB_LISTING_DATE_BUFF 26
#define			CB_LISTING_DATE_MAX (CB_LISTING_DATE_BUFF - 1)
char			cb_listing_date[CB_LISTING_DATE_BUFF]; /* Date/Time buffer for listing */
struct list_files	*cb_current_file = NULL;
#define			LCL_NAME_LEN 80
#define			LCL_NAME_MAX (LCL_NAME_LEN - 1)

/* compilation date/time of current source file */
struct cob_time		current_compile_time = { 0 };
struct tm		current_compile_tm = { 0 };

#if	0	/* RXWRXW - source format */
char			*source_name = NULL;
#endif

enum cb_format		cb_source_format = CB_FORMAT_FIXED;
#if 0 /* ancient OSVS registers that need special runtime handling - low priority */
enum cb_current_date	current_date = CB_DATE_MDY;
#endif
int			cb_text_column;
int			cb_indicator_column;
int			cb_id = 0;
int			cb_pic_id = 0;
int			cb_attr_id = 0;
int			cb_literal_id = 0;
int			cb_field_id = 0;
int			cb_ml_attr_id = 0;
int			cb_ml_tree_id = 0;
int			cobc_flag_main = 0;
int			cb_flag_main = 0;
int			cobc_wants_debug = 0;
int			cb_flag_functions_all = 0;
int			cb_flag_optimize_check = 0;
int			cb_flag_dump = 0;
int			cobc_seen_stdin = 0;
int			cb_unix_lf = 0;

int 		fatal_startup_error = 0;
int			errorcount = 0;
int			warningcount = 0;
int			fatal_errors_flag = 0;
int			no_physical_cancel = 0;
int			cb_source_line = 0;
int			cb_saveargc = 0;
unsigned int	cobc_gen_listing = 0;
unsigned int	cb_correct_program_order = 0;

cob_u32_t		optimize_defs[COB_OPTIM_MAX] = { 0 };

#define	COB_EXCEPTION(code,tag,name,critical) {name, 0x##code, 0},
struct cb_exception cb_exception_table[] = {
	{NULL, 0, 0},		/* CB_EC_ZERO */
#include "../libcob/exception.def"
	{NULL, 0, 0}		/* CB_EC_MAX */
};
#undef	COB_EXCEPTION

#define	CB_FLAG(var,print_help,name,doc)	int var = 0;
#define	CB_FLAG_ON(var,print_help,name,doc)	int var = 1;
#define	CB_FLAG_RQ(var,print_help,name,def,opt,doc)	int var = def;
#define	CB_FLAG_NQ(print_help,name,opt,doc)
#define	CB_FLAG_OP(print_help,name,opt,doc)
#include "flag.def"
#undef	CB_FLAG
#undef	CB_FLAG_ON
#undef	CB_FLAG_RQ
#undef	CB_FLAG_NQ
#undef	CB_FLAG_OP
int cb_mf_ibm_comp = -1;
int cb_cob_line_num = 0;
int cb_all_files_xfd = 0;

int cb_warn_opt_val[COB_WARNOPT_MAX];

/* Local variables */

static char	cob_std_conf[64] = "";
static int		num_std = 0;
static struct cb_define_struct	*cb_define_list = NULL;

static struct cobc_mem_struct	*cobc_mainmem_base = NULL;
static struct cobc_mem_struct	*cobc_parsemem_base = NULL;
static struct cobc_mem_struct	*cobc_plexmem_base = NULL;

static const char	*cobc_cc;		/* C compiler */
static char		*cobc_cflags;		/* C compiler flags */
#ifdef COB_DEBUG_FLAGS
static const char		*cobc_debug_flags;		/* C debgging flags */
#else
#ifndef	_MSC_VER
#error		missing definition of COB_DEBUG_FLAGS
#endif
#endif

static char		*cobc_libs;		/* -l... */
static char		*cobc_lib_paths;	/* -L... */
static char		*cobc_include;		/* -I... */
static char		*cobc_ldflags;		/* -Q / COB_LDFLAGS */

static size_t		cobc_cflags_size;
static size_t		cobc_libs_size;
static size_t		cobc_lib_paths_size;
static size_t		cobc_include_size;
static size_t		cobc_ldflags_size;

static size_t		cobc_cc_len;
static size_t		cobc_cflags_len;
static size_t		cobc_libs_len;
static size_t		cobc_lib_paths_len;
static size_t		cobc_include_len;
static size_t		cobc_ldflags_len;
#ifdef COB_EXPORT_DYN
static size_t		cobc_export_dyn_len;
#else
#define		cobc_export_dyn_len		0
#endif
#ifdef COB_SHARED_OPT
static size_t		cobc_shared_opt_len;
#else
#define		cobc_shared_opt_len		0
#endif
#ifdef COB_PIC_FLAGS
static size_t		cobc_pic_flags_len;
#else
#define		cobc_pic_flags_len		0
#endif

static char		*save_temps_dir = NULL;
static struct strcache	*base_string;

static char		*cobc_list_dir = NULL;
static char		*cobc_list_file = NULL;

static char		*output_name = NULL;
static char		*cobc_buffer;
static char		*cobc_objects_buffer;
static char		*output_name_buff;
static char		*basename_buffer;

static size_t		cobc_objects_len;
static size_t		basename_len;
static size_t		cobc_buffer_size;

static struct filename	*file_list;

static unsigned int	cb_compile_level = 0;

static int		iargs;

static size_t		cobc_flag_module = 0;
static size_t		cobc_flag_library = 0;
static size_t		cobc_flag_run = 0;
static char		*cobc_run_args = NULL;
static size_t		save_temps = 0;
static size_t		save_all_src = 0;
static size_t		save_c_src = 0;
static signed int	verbose_output = 0;

static unsigned int		cb_listing_linecount;
static int		cb_listing_eject = 0;
static char		cb_listing_filename[FILENAME_MAX];
static char		*cb_listing_outputfile = NULL;
static char		cb_listing_title[81];	/* Listing title (defaults to PACKAGE_NAME + Version */
static char		cb_listing_header[133];	/* Listing header */
static struct list_files	*cb_listing_file_struct = NULL;
static struct list_error	*cb_listing_error_head = NULL;
static struct list_error	*cb_listing_error_tail = NULL;

#ifdef	_MSC_VER
static const char	*manicmd;
static const char	*manilink;
static size_t		manilink_len;
#define PATTERN_DELIM '|'
#endif

static size_t		strip_output = 0;
static size_t		cb_source_debugging = 0;	/* note: was moved to global one later, so keep that name already*/

static const char	*const cob_csyns[] = {
#ifndef	COB_EBCDIC_MACHINE
	"NULL",
	"P_cancel",
	"P_initialize",
	"P_ret_initialize",
	"P_switch",
#endif
#ifdef	COB_EBCDIC_MACHINE
	"_float128",
#endif
	"_Bool",
	"_Complex",
	"_Imaginary",
#ifndef	COB_EBCDIC_MACHINE
	"_float128",
#endif
	"alignof",
	"asm",
	"auto",
	"bool",
	"break",
	"case",
	"catch",
	"char",
	"class",
	"const",
	"const_cast",
	"continue",
	"default",
	"delete",
	"do",
	"double",
	"dynamic_cast",
	"else",
	"enum",
	"exit_function",
	"exit_program",
	"explicit",
	"extern",
	"false",
	"float",
	"for",
	"frame_pointer",
	"frame_stack",
	"friend",
	"goto",
	"if",
	"inline",
	"int",
	"long",
	"mutable",
	"namespace",
	"new",
	"offsetof",
	"operator",
	"private",
	"protected",
	"public",
	"register",
	"reinterpret_cast",
	"restrict",
	"return",
	"short",
	"signed",
	"sizeof",
	"static",
	"static_cast",
	"struct",
	"switch",
	"template",
	"this",
	"throw",
	"true",
	"try",
	"typedef",
	"typeid",
	"typename",
	"typeof",
	"union",
	"unsigned",
	"using",
	"virtual",
	"void",
	"volatile",
#ifndef	COB_EBCDIC_MACHINE
	"wchar_t"
#else
	"wchar_t",
	"NULL",
	"P_cancel",
	"P_initialize",
	"P_ret_initialize",
	"P_switch"
#endif
};

#define COB_NUM_CSYNS	sizeof(cob_csyns) / sizeof(cob_csyns[0])

static const char short_options[] = "hVivqECScbmxjdFROPgGwo:t:T:I:L:l:D:K:k:";

#define	CB_NO_ARG	no_argument
#define	CB_RQ_ARG	required_argument
#define	CB_OP_ARG	optional_argument

static const struct option long_options[] = {
	{"help",		CB_NO_ARG, NULL, 'h'},
	{"version",		CB_NO_ARG, NULL, 'V'},
	{"verbose",		CB_OP_ARG, NULL, 'v'},
	{"brief",		CB_NO_ARG, NULL, 'q'},
	{"###",			CB_NO_ARG, NULL, '#'},
	{"info",		CB_NO_ARG, NULL, 'i'},
	{"list-reserved",	CB_NO_ARG, NULL, '5'},
	{"list-intrinsics",	CB_NO_ARG, NULL, '6'},
	{"list-mnemonics",	CB_NO_ARG, NULL, '7'},
	{"list-system",		CB_NO_ARG, NULL, '8'},
	{"list-registers",		CB_NO_ARG, NULL, '9'},
	{"O0",			CB_NO_ARG, NULL, '0'},
	{"O2",			CB_NO_ARG, NULL, '2'},
	{"O3",			CB_NO_ARG, NULL, '3'},
	{"Os",			CB_NO_ARG, NULL, 's'},
	{"save-temps",		CB_OP_ARG, NULL, '_'},
	{"std",			CB_RQ_ARG, NULL, '$'},
	{"conf",		CB_RQ_ARG, NULL, '&'},
	{"debug",		CB_NO_ARG, NULL, 'd'},
	{"ext",			CB_RQ_ARG, NULL, 'e'},
	{"free",		CB_NO_ARG, NULL, 'F'},	/* note: not assigned directly as this is only valid for */
	{"fixed",		CB_NO_ARG, NULL, 'f'},	/*       `int` and sizeof(enum) isn't always sizeof (int) */
	{"static",		CB_NO_ARG, &cb_flag_static_call, 1},
	{"dynamic",		CB_NO_ARG, &cb_flag_static_call, 0},
	{"job",			CB_OP_ARG, NULL, 'j'},
	{"j",			CB_OP_ARG, NULL, 'j'},
	{"Q",			CB_RQ_ARG, NULL, 'Q'},
	{"A",			CB_RQ_ARG, NULL, 'A'},
	{"P",			CB_OP_ARG, NULL, 'P'},
	{"Xref",		CB_NO_ARG, NULL, 'X'},
	{"use-extfh",		CB_RQ_ARG, NULL, 9},	/* this is used by COBOL-IT; Same is -fcallfh= */
	{"Wall",		CB_NO_ARG, NULL, 'W'},
	{"Wextra",		CB_NO_ARG, NULL, 'Y'},		/* this option used to be called -W */
#if 1
	{"W",			CB_NO_ARG, NULL, 'Y'},
#else /* TODO */
	{"W",			CB_OP_ARG, NULL, 'Y'},
	{"Wno",			CB_RQ_ARG, NULL, 'y'},		/* just a catch-all for unknown warnings */
#endif
	{"Werror",		CB_OP_ARG, NULL, 'Z'},
	{"Wno-error",		CB_OP_ARG, NULL, 'z'},
	{"tlines",		CB_RQ_ARG, NULL, '*'},
	{"tsymbols",		CB_NO_ARG, &cb_listing_symbols, 1},	/* kept for backwards-compatibility in 3.x */

#define	CB_FLAG(var,print_help,name,doc)			\
	{"f" name,		CB_NO_ARG, &var, 1},	\
	{"fno-" name,		CB_NO_ARG, &var, 0},
#define	CB_FLAG_ON(var,print_help,name,doc)		\
	{"f" name,		CB_NO_ARG, &var, 1},	\
	{"fno-" name,		CB_NO_ARG, &var, 0},
#define	CB_FLAG_RQ(var,print_help,name,def,opt,doc)		\
	{"f" name,		CB_RQ_ARG, NULL, opt},
#define	CB_FLAG_NQ(print_help,name,opt,doc)			\
	{"f" name,		CB_RQ_ARG, NULL, opt},
#define	CB_FLAG_OP(print_help,name,opt,doc)			\
	{"f" name,		CB_OP_ARG, NULL, opt},
#include "flag.def"
#undef	CB_FLAG
#undef	CB_FLAG_ON
#undef	CB_FLAG_RQ
#undef	CB_FLAG_NQ
#undef	CB_FLAG_OP
	{"fibmcomp",		CB_NO_ARG, &cb_mf_ibm_comp, 1},
	{"fno-ibmcomp",		CB_NO_ARG, &cb_mf_ibm_comp, 0},
	{"fdatamap",		CB_NO_ARG, &cb_list_datamap, 1},

	/* alias for backwards-compatibility, removed with 4.x: */
	{"fnotrunc",		CB_NO_ARG, &cb_flag_trunc, 0},
	{"fno-notrunc",		CB_NO_ARG, &cb_flag_trunc, 1},

#define	CB_CONFIG_ANY(type,var,name,doc)	\
	{"f" name,		CB_RQ_ARG, NULL, '%'},
#define	CB_CONFIG_INT(var,name,min,max,odoc,doc)	\
	{"f" name,		CB_RQ_ARG, NULL, '%'},
#define	CB_CONFIG_SIZE(var,name,min,max,odoc,doc)	\
	{"f" name,		CB_RQ_ARG, NULL, '%'},
#define	CB_CONFIG_STRING(var,name,doc)		\
	{"f" name,		CB_RQ_ARG, NULL, '%'},
#define	CB_CONFIG_BOOLEAN(var,name,doc)		\
	{"f" name,		CB_NO_ARG, &var, 1},	\
	{"fno-" name,		CB_NO_ARG, &var, 0},
#define	CB_CONFIG_SUPPORT(var,name,doc)		\
	{"f" name,		CB_RQ_ARG, NULL, '%'},
#include "config.def"
#undef	CB_CONFIG_ANY
#undef	CB_CONFIG_INT
#undef	CB_CONFIG_SIZE
#undef	CB_CONFIG_STRING
#undef	CB_CONFIG_BOOLEAN
#undef	CB_CONFIG_SUPPORT
	{"freserved",	CB_RQ_ARG, NULL, '%'},
	{"fnot-reserved",	CB_RQ_ARG, NULL, '%'},
	{"fintrinsic-function",	CB_RQ_ARG, NULL, '%'},
	{"fnot-intrinsic-function",	CB_RQ_ARG, NULL, '%'},
	{"fsystem-name",	CB_RQ_ARG, NULL, '%'},
	{"fnot-system-name",	CB_RQ_ARG, NULL, '%'},
	{"fregister",	CB_RQ_ARG, NULL, '%'},
	{"fnot-register",	CB_RQ_ARG, NULL, '%'},

#define	CB_WARNDEF(opt,name,doc)			\
	{"W" name,		CB_NO_ARG, &cb_warn_opt_val[opt], COBC_WARN_ENABLED_EXPL},	\
	{"Wno-" name,		CB_NO_ARG, &cb_warn_opt_val[opt], COBC_WARN_DISABLED_EXPL},
#define	CB_ONWARNDEF(opt,name,doc)			\
	{"W" name,		CB_NO_ARG, &cb_warn_opt_val[opt], COBC_WARN_ENABLED_EXPL},	\
	{"Wno-" name,		CB_NO_ARG, &cb_warn_opt_val[opt], COBC_WARN_DISABLED_EXPL},
#define	CB_NOWARNDEF(opt,name,doc)			\
	{"W" name,		CB_NO_ARG, &cb_warn_opt_val[opt], COBC_WARN_ENABLED_EXPL},	\
	{"Wno-" name,		CB_NO_ARG, &cb_warn_opt_val[opt], COBC_WARN_DISABLED_EXPL},
#define	CB_ERRWARNDEF(opt,name,doc)			\
	{"W" name,		CB_NO_ARG, &cb_warn_opt_val[opt], COBC_WARN_ENABLED_EXPL},	\
	{"Wno-" name,		CB_NO_ARG, &cb_warn_opt_val[opt], COBC_WARN_DISABLED_EXPL},
#include "warning.def"
#undef	CB_WARNDEF
#undef	CB_ONWARNDEF
#undef	CB_NOWARNDEF
#undef	CB_ERRWARNDEF
	{"Wfatal-errors",	CB_NO_ARG, &fatal_errors_flag, 1},
	{"Wno-fatal-errors",	CB_NO_ARG, &fatal_errors_flag, 0},

	{NULL,			0, NULL, 0}
};

#undef	CB_NO_ARG
#undef	CB_RQ_ARG
#undef	CB_OP_ARG

/* Prototype */
DECLNORET static void COB_A_NORETURN	cobc_early_exit (int);
DECLNORET static void COB_A_NORETURN	cobc_err_exit (const char *, ...) COB_A_FORMAT12;
static void	free_list_file		(struct list_files *);
static void	print_program	(struct list_files *, int);
static void	set_standard_title	(void);
static void	print_program_header	(void);
static void	print_program_data	(const char *);
static void	print_program_trailer	(void);
static void	print_program_listing	(void);
static int	process			(const char *);

/* cobc functions */

static void
cobc_free_mem (void)
{
	struct cobc_mem_struct	*reps;
	struct cobc_mem_struct	*repsl;

	if (save_temps_dir) {
		cobc_free (save_temps_dir);
		save_temps_dir = NULL;
	}
	if (cobc_list_dir) {
		cobc_free (cobc_list_dir);
		cobc_list_dir = NULL;
	}
	if (cobc_list_file) {
		cobc_free (cobc_list_file);
		cobc_list_file = NULL;
	}
	if (cb_listing_file_struct) {
		free_list_file (cb_listing_file_struct);
		cb_listing_file_struct = NULL;
	}
	if (cobc_run_args) {
		cobc_free (cobc_run_args);
		cobc_run_args = NULL;
	}
	for (reps = cobc_plexmem_base; reps; ) {
		repsl = reps;
		reps = reps->next;
		cobc_free (repsl);
	}
	cobc_plexmem_base = NULL;
	for (reps = cobc_parsemem_base; reps; ) {
		repsl = reps;
		reps = reps->next;
		cobc_free (repsl);
	}
	cobc_parsemem_base = NULL;
	for (reps = cobc_mainmem_base; reps; ) {
		repsl = reps;
		reps = reps->next;
		cobc_free (repsl);
	}
	cobc_mainmem_base = NULL;
	cb_init_codegen ();
	ppp_clear_lists ();
}

#ifdef COB_TREE_DEBUG
static const char *
cobc_enum_explain (const enum cb_tag tag)
{
	switch (tag) {
	case CB_TAG_CONST:
		return "CONSTANT";
	case CB_TAG_INTEGER:
		return "INTEGER";
	case CB_TAG_STRING:
		return "STRING";
	case CB_TAG_ALPHABET_NAME:
		return "ALPHABET";
	case CB_TAG_CLASS_NAME:
		return "CLASS";
	case CB_TAG_LOCALE_NAME:
		return "LOCALE";
	case CB_TAG_SYSTEM_NAME:
		return "SYSTEM";
	case CB_TAG_LITERAL:
		return "LITERAL";
	case CB_TAG_DECIMAL:
		return "DECIMAL";
	case CB_TAG_FIELD:
		return "FIELD";
	case CB_TAG_FILE:
		return "FILE";
	case CB_TAG_REPORT:
		return "REPORT";
	case CB_TAG_REFERENCE:
		return "REFERENCE";
	case CB_TAG_BINARY_OP:
		return "BINARY OP";
	case CB_TAG_FUNCALL:
		return "FUNCTION CALL";
	case CB_TAG_CAST:
		return "CAST";
	case CB_TAG_INTRINSIC:
		return "INTRINSIC";
	case CB_TAG_LABEL:
		return "LABEL";
	case CB_TAG_ASSIGN:
		return "ASSIGN";
	case CB_TAG_INITIALIZE:
		return "INITIALIZE";
	case CB_TAG_SEARCH:
		return "SEARCH";
	case CB_TAG_CALL:
		return "CALL";
	case CB_TAG_GOTO:
		return "GO TO";
	case CB_TAG_IF:
		return "IF";
	case CB_TAG_PERFORM:
		return "PERFORM";
	case CB_TAG_STATEMENT:
		return "STATEMENT";
	case CB_TAG_CONTINUE:
		return "CONTINUE";
	case CB_TAG_CANCEL:
		return "CANCEL";
	case CB_TAG_ALTER:
		return "ALTER";
	case CB_TAG_SET_ATTR:
		return "SET ATTRIBUTE";
	case CB_TAG_PERFORM_VARYING:
		return "PERFORM";
	case CB_TAG_PICTURE:
		return "PICTURE";
	case CB_TAG_LIST:
		return "LIST";
	case CB_TAG_DIRECT:
		return "DIRECT";
	case CB_TAG_DEBUG:
		return "DEBUG";
	case CB_TAG_DEBUG_CALL:
		return "DEBUG CALL";
	case CB_TAG_PROGRAM:
		return "PROGRAM";
	case CB_TAG_PROTOTYPE:
		return "PROTOTYPE";
	case CB_TAG_DECIMAL_LITERAL:
		return "DECIMAL LITERAL";
	case CB_TAG_REPORT_LINE:
		return "REPORT LINE";
	case CB_TAG_ML_SUPPRESS:
		return "ML SUPPRESS CLAUSE";
	case CB_TAG_ML_TREE:
		return "ML OUTPUT TREE";
	case CB_TAG_ML_SUPPRESS_CHECKS:
		return "ML SUPPRESS CHECKS";
	case CB_TAG_CD:
		return "COMMUNICATION DESCRIPTION";
	default:
		break;
	}
	return "UNKNOWN";
}
#endif

static void
free_error_list (struct list_error *err)
{
	struct list_error	*next;

	do {
		if (err->file) {
			cobc_free (err->file);
		}
		if (err->prefix) {
			cobc_free (err->prefix);
		}
		if (err->msg) {
			cobc_free (err->msg);
		}

		next = err->next;
		cobc_free (err);
		err = next;
	} while (err);
}

static void
free_replace_list (struct list_replace *replace)
{
	struct list_replace	*next;

	do {
		if (replace->from) {
			cobc_free (replace->from);
		}
		if (replace->to) {
			cobc_free (replace->to);
		}

		next = replace->next;
		cobc_free (replace);
		replace = next;
	} while (replace);
}

static void
free_list_skip (struct list_skip *skip)
{
	struct list_skip	*next;

	do {
		next = skip->next;
		cobc_free (skip);
		skip = next;
	} while (skip);

}

static void
free_list_file (struct list_files *list_files_struct)
{
	struct list_files	*next;

	do {
		/* Delete the resources held by the struct. */
		if (list_files_struct->copy_head) {
			free_list_file (list_files_struct->copy_head);
		}
		if (list_files_struct->err_head) {
			free_error_list (list_files_struct->err_head);
		}
		if (list_files_struct->replace_head) {
			free_replace_list (list_files_struct->replace_head);
		}
		if (list_files_struct->skip_head) {
			free_list_skip (list_files_struct->skip_head);
		}
		if (list_files_struct->name) {
			cobc_free ((char *) list_files_struct->name);
		}

		/* Delete the struct itself */
		next = list_files_struct->next;
		cobc_free (list_files_struct);
		list_files_struct = next;
	} while (list_files_struct);
}

/* Global functions */

/* Output a formatted message to stderr */
void
cobc_err_msg (const char *fmt, ...)
{
	va_list		ap;

	fprintf (stderr, "cobc: ");
	va_start (ap, fmt);
	vfprintf (stderr, fmt, ap);

	if (cb_src_list_file
		&& cb_listing_file_struct
		&& cb_listing_file_struct->name) {

		char			errmsg[BUFSIZ];
		vsprintf (errmsg, fmt, ap);

		cb_add_error_to_listing (NULL, 0,
			"cobc: ", errmsg);
	}
	va_end (ap);
	putc ('\n', stderr);
	fflush (stderr);
}

/* Output cobc source/line where an internal error occurs and exit */
/* LCOV_EXCL_START */
void
cobc_abort (const char * filename, const int line_num)
{
	++errorcount;

	cobc_err_msg ("%s: %d: %s", filename, line_num,
		_("internal compiler error"));
	cobc_abort_terminate (1);
}
/* LCOV_EXCL_STOP */

#ifdef COB_TREE_DEBUG
/* LCOV_EXCL_START */

DECLNORET static void	cobc_tree_cast_error (const cb_tree, const char *,
	const int, const enum cb_tag) COB_A_NORETURN;

static int cast_error_raised = 0;

/* Output cobc source/line where a tree cast error occurs and exit */
static void
cobc_tree_cast_error (const cb_tree x, const char * filename, const int line_num,
		      const enum cb_tag tagnum)
{
	const char *name, *type;

	cast_error_raised = 1;
	if (!x) {
		name = "NULL";
		type = "None";
	} else {
		name = cb_name (x);
		type = cobc_enum_explain (CB_TREE_TAG (x));
	}

	putc ('\n', stderr);
	/* not translated as this only occurs if developer-only setup is used: */
	cobc_err_msg ("%s: %d: invalid cast from '%s' type %s to type %s",
		filename, line_num, name, type,
		cobc_enum_explain (tagnum));

	if (cast_error_raised != 1) {
		cobc_err_msg ("additional cast error was raised during name lookup");
	}
	cobc_abort_terminate (1);
}

cb_tree
cobc_tree_cast_check (const cb_tree x, const char * file,
		      const int line, const enum cb_tag tag)
{
	if (!x || x == cb_error_node || CB_TREE_TAG (x) != tag) {
		/* if recursive don't raise a tree cast error */
		if (!cast_error_raised) {;
			cobc_tree_cast_error (x, file, line, tag);
		} else {
			cast_error_raised = 2;
		}
	}
	return x;
}
/* LCOV_EXCL_STOP */
#endif

void *
cobc_malloc (const size_t size)
{
	void	*mptr;

	mptr = calloc ((size_t)1, size);
	/* LCOV_EXCL_START */
	if (!mptr) {
		cobc_err_msg (_("cannot allocate %d bytes of memory"),
				(int)size);
		cobc_abort_terminate (0);
	}
	/* LCOV_EXCL_STOP */
	return mptr;
}

void
cobc_free (void * mptr)
{
	/* LCOV_EXCL_START */
	if (!mptr) {
		cobc_err_msg (_("call to %s with NULL pointer"), "cobc_free");
		cobc_abort_terminate (1);
	}
	/* LCOV_EXCL_STOP */
	free (mptr);
}

void *
cobc_strdup (const char *dupstr)
{
	void	*p;
	size_t	n;

#ifdef	COB_TREE_DEBUG
	/* LCOV_EXCL_START */
	if (!dupstr) {
		cobc_err_msg (_("call to %s with NULL pointer"), "cobc_strdup");
		cobc_abort_terminate (1);
	}
	/* LCOV_EXCL_STOP */
#endif
	n = strlen (dupstr);
	p = cobc_malloc (n + 1);
	memcpy (p, dupstr, n);
	return p;
}

#if	defined (_WIN32) || defined (__CYGWIN__)
static char *
cobc_stradd_dup (const char *str1, const char *str2)
{
	char	*p;
	size_t	m, n;

	/* LCOV_EXCL_START */
	if (!str1 || !str2) {
		cobc_err_msg (_("call to %s with NULL pointer"), "cobc_stradd_dup");
		cobc_abort_terminate (1);
	}
	/* LCOV_EXCL_STOP */
	m = strlen (str1);
	n = strlen (str2);
	p = cobc_malloc (m + n + 1);
	memcpy (p, str1, m);
	memcpy (p + m, str2, n);
	return p;
}
#endif

void *
cobc_realloc (void *prevptr, const size_t size)
{
	void	*mptr;

	mptr = realloc (prevptr, size);
	/* LCOV_EXCL_START */
	if (!mptr) {
		cobc_err_msg (_("cannot reallocate %d bytes of memory"),
				(int)size);
		cobc_abort_terminate (0);
	}
	/* LCOV_EXCL_STOP */
	return mptr;
}

/* Memory allocate/strdup/reallocate/free for complete execution */
void *
cobc_main_malloc (const size_t size)
{
	struct cobc_mem_struct	*m;

	m = calloc ((size_t)1, COBC_MEM_SIZE + size);
	/* LCOV_EXCL_START */
	if (!m) {
		cobc_err_msg (_("cannot allocate %d bytes of memory"),
				(int)size);
		cobc_abort_terminate (0);
	}
	/* LCOV_EXCL_STOP */
	m->next = cobc_mainmem_base;
	m->memptr = (char *)m + COBC_MEM_SIZE;
	m->memlen = size;
	cobc_mainmem_base = m;
	return m->memptr;
}

/* returns a fresh allocated copy of dupstr */
void *
cobc_main_strdup (const char *dupstr)
{
	void	*p;
	size_t	n;

	/* LCOV_EXCL_START */
	if (!dupstr) {
		cobc_err_msg (_("call to %s with NULL pointer"), "cobc_main_strdup");
		cobc_abort_terminate (1);
	}
	/* LCOV_EXCL_STOP */
	n = strlen (dupstr);
	p = cobc_main_malloc (n + 1);
	memcpy (p, dupstr, n);
	return p;
}

/* returns a fresh allocated copy of the concatenation from str1 + str2 */
static char *
cobc_main_stradd_dup (const char *str1, const char *str2)
{
	char	*p;
	size_t	m, n;

	/* LCOV_EXCL_START */
	if (!str1 || !str2) {
		cobc_err_msg (_("call to %s with NULL pointer"), "cobc_main_stradd_dup");
		cobc_abort_terminate (1);
	}
	/* LCOV_EXCL_STOP */
	m = strlen (str1);
	n = strlen (str2);
	p = cobc_main_malloc (m + n + 1);
	memcpy (p, str1, m);
	memcpy (p + m, str2, n);
	return p;
}

void *
cobc_main_realloc (void *prevptr, const size_t size)
{
	struct cobc_mem_struct	*m;
	struct cobc_mem_struct	*curr;
	struct cobc_mem_struct	*prev;

	m = calloc ((size_t)1, COBC_MEM_SIZE + size);
	/* LCOV_EXCL_START */
	if (!m) {
		cobc_err_msg (_("cannot allocate %d bytes of memory"),
				(int)size);
		cobc_abort_terminate (0);
	}
	/* LCOV_EXCL_STOP */
	m->memptr = (char *)m + COBC_MEM_SIZE;
	m->memlen = size;

	prev = NULL;
	for (curr = cobc_mainmem_base; curr; curr = curr->next) {
		if (curr->memptr == prevptr) {
			break;
		}
		prev = curr;
	}
	/* LCOV_EXCL_START */
	if (!curr) {
		cobc_err_msg (_("attempt to reallocate non-allocated memory"));
		cobc_abort_terminate (0);
	}
	/* LCOV_EXCL_STOP */
	m->next = curr->next;
	if (prev) {
		prev->next = m;
	} else {
		/* At mainmem_base */
		cobc_mainmem_base = m;
	}
	memcpy (m->memptr, curr->memptr, curr->memlen);
	cobc_free (curr);

	return m->memptr;
}

void
cobc_main_free (void *prevptr)
{
	struct cobc_mem_struct	*curr;
	struct cobc_mem_struct	*prev;

	prev = NULL;
	for (curr = cobc_mainmem_base; curr; curr = curr->next) {
		if (curr->memptr == prevptr) {
			break;
		}
		prev = curr;
	}
	/* LCOV_EXCL_START */
	if (!curr) {
#ifdef	COB_TREE_DEBUG
		cobc_err_msg (_("call to %s with invalid pointer, as it is missing in list"),
			"cobc_main_free");
		cobc_abort_terminate (1);
#else
		return;
#endif
	}
	/* LCOV_EXCL_STOP */
	if (prev) {
		prev->next = curr->next;
	} else {
		/* At mainmem_base */
		cobc_mainmem_base = curr->next;
	}
	cobc_free (curr);
}

/* Memory allocate/strdup/reallocate/free for parser */
void *
cobc_parse_malloc (const size_t size)
{
	struct cobc_mem_struct	*m;

	m = calloc ((size_t)1, COBC_MEM_SIZE + size);
	/* LCOV_EXCL_START */
	if (!m) {
		cobc_err_msg (_("cannot allocate %d bytes of memory"),
				(int)size);
		cobc_abort_terminate (0);
	}
	/* LCOV_EXCL_STOP */
	m->next = cobc_parsemem_base;
	m->memptr = (char *)m + COBC_MEM_SIZE;
	m->memlen = size;
	cobc_parsemem_base = m;
	return m->memptr;
}

void *
cobc_parse_strdup (const char *dupstr)
{
	void	*p;
	size_t	n;

	/* LCOV_EXCL_START */
	if (!dupstr) {
		cobc_err_msg (_("call to %s with NULL pointer"), "cobc_parse_strdup");
		cobc_abort_terminate (1);
	}
	/* LCOV_EXCL_STOP */
	n = strlen (dupstr);
	p = cobc_parse_malloc (n + 1);
	memcpy (p, dupstr, n);
	return p;
}

void *
cobc_parse_realloc (void *prevptr, const size_t size)
{
	struct cobc_mem_struct	*m;
	struct cobc_mem_struct	*curr;
	struct cobc_mem_struct	*prev;

	m = calloc ((size_t)1, COBC_MEM_SIZE + size);
	/* LCOV_EXCL_START */
	if (!m) {
		cobc_err_msg (_("cannot allocate %d bytes of memory"),
				(int)size);
		cobc_abort_terminate (0);
	}
	/* LCOV_EXCL_STOP */
	m->memptr = (char *)m + COBC_MEM_SIZE;
	m->memlen = size;

	prev = NULL;
	for (curr = cobc_parsemem_base; curr; curr = curr->next) {
		if (curr->memptr == prevptr) {
			break;
		}
		prev = curr;
	}
	/* LCOV_EXCL_START */
	if (!curr) {
		cobc_err_msg (_("attempt to reallocate non-allocated memory"));
		cobc_abort_terminate (0);
	}
	/* LCOV_EXCL_STOP */
	m->next = curr->next;
	if (prev) {
		prev->next = m;
	} else {
		/* At parsemem_base */
		cobc_parsemem_base = m;
	}
	memcpy (m->memptr, curr->memptr, curr->memlen);
	cobc_free (curr);

	return m->memptr;
}

void
cobc_parse_free (void *prevptr)
{
	struct cobc_mem_struct	*curr;
	struct cobc_mem_struct	*prev;

	prev = NULL;
	for (curr = cobc_parsemem_base; curr; curr = curr->next) {
		if (curr->memptr == prevptr) {
			break;
		}
		prev = curr;
	}
	/* LCOV_EXCL_START */
	if (!curr) {
#ifdef	COB_TREE_DEBUG
		cobc_err_msg (_("call to %s with invalid pointer, as it is missing in list"),
			"cobc_parse_free");
		cobc_abort_terminate (1);
#else
		return;
#endif
	}
	/* LCOV_EXCL_STOP */
	if (prev) {
		prev->next = curr->next;
	} else {
		/* At parsemem_base */
		cobc_parsemem_base = curr->next;
	}
	cobc_free (curr);
}

/* Memory allocate/strdup/reallocate/free for preprocessor */
void *
cobc_plex_malloc (const size_t size)
{
	struct cobc_mem_struct	*m;

	m = calloc ((size_t)1, COBC_MEM_SIZE + size);
	/* LCOV_EXCL_START */
	if (!m) {
		cobc_err_msg (_("cannot allocate %d bytes of memory"),
				(int)size);
		cobc_abort_terminate (0);
	}
	/* LCOV_EXCL_STOP */
	m->memptr = (char *)m + COBC_MEM_SIZE;
	m->next = cobc_plexmem_base;
	cobc_plexmem_base = m;
	return m->memptr;
}

void *
cobc_plex_strdup (const char *dupstr)
{
	void	*p;
	size_t	n;

	/* LCOV_EXCL_START */
	if (!dupstr) {
		cobc_err_msg (_("call to %s with NULL pointer"), "cobc_plex_strdup");
		cobc_abort_terminate (1);
	}
	/* LCOV_EXCL_STOP */
	n = strlen (dupstr);
	p = cobc_plex_malloc (n + 1);
	memcpy (p, dupstr, n);
	return p;
}

void *
cobc_check_string (const char *dupstr)
{
	struct strcache	*s;

	/* LCOV_EXCL_START */
	if (!dupstr) {
		cobc_err_msg (_("call to %s with NULL pointer"), "cobc_check_string");
		cobc_abort_terminate (1);
	}
	/* LCOV_EXCL_STOP */

	/* FIXME - optimize performance:
	   this loop is extensively used for comparision of picture strings,
	   it consumes ~6% of the compilation time with ~3% in strcmp */
	for (s = base_string; s; s = s->next) {
		if (!strcmp (dupstr, (const char *)s->val)) {
			return s->val;
		}
	}
	s = cobc_main_malloc (sizeof(struct strcache));
	s->next = base_string;
	s->val = cobc_main_strdup (dupstr);
	base_string = s;
	return s->val;
}

static struct cb_text_list *
cb_text_list_add (struct cb_text_list *list, const char *text)
{
	struct cb_text_list	*p;

	p = cobc_main_malloc (sizeof (struct cb_text_list));
	p->text = cobc_main_strdup (text);
	if (!list) {
		p->last = p;
		return p;
	}
	list->last->next = p;
	list->last = p;
	return list;
}

static struct cb_text_list *
cb_text_list_chk (struct cb_text_list *list, const char *text)
{
	struct cb_text_list	*p;

	for (p = list; p; p = p->next) {
		if (!strcmp (text, p->text)) {
			return list;
		}
	}
	return cb_text_list_add (list, text);
}

static unsigned int
cobc_set_value (struct cb_define_struct *p, const char *value)
{
	const char	*s;
	size_t		size;
	unsigned int	dot_seen;
	unsigned int	sign_seen;

	if (!value) {
		p->deftype = PLEX_DEF_NONE;
		p->value = NULL;
		p->sign = 0;
		p->int_part = 0;
		p->dec_part = 0;
		return 0;
	}

	/* Quoted value */
	if (*value == '"' || *value == '\'') {
		size = strlen (value) - 1U;
		if (value[0] != value[size]) {
			p->value = NULL;
			p->deftype = PLEX_DEF_NONE;
			return 1;
		}
		p->value = cobc_main_strdup (value);

		p->deftype = PLEX_DEF_LIT;
		p->sign = 0;
		p->int_part = 0;
		p->dec_part = 0;
		return 0;
	}

	/* Non-quoted value - Check if possible numeric */
	dot_seen = 0;
	sign_seen = 0;
	size = 0;
	s = value;
	if (*s == '+' || *s == '-') {
		sign_seen = 1;
		size++;
		s++;
	}
	for (; *s; ++s) {
		if (*s == '.') {
			if (dot_seen) {
				break;
			}
			dot_seen = 1;
			size++;
			continue;
		}
		if (*s > '9' || *s < '0') {
			break;
		}
		size++;
	}

	if (*s || size <= ((size_t)dot_seen + sign_seen)) {
		/* Not numeric */
#if	0	/* RXWRXW - Lit warn */
		cb_warning (COBC_WARN_FILLER, _("assuming literal for unquoted '%s'"),
				value);
#endif
		size = strlen (value);
		p->value = cobc_main_malloc (size + 4U);
		sprintf (p->value, "'%s'", value);
		p->deftype = PLEX_DEF_LIT;
		p->sign = 0;
		p->int_part = 0;
		p->dec_part = 0;
		return 0;
	}

	p->value = cobc_main_strdup (value);
	p->deftype = PLEX_DEF_NUM;
	p->sign = 0;
	p->int_part = 0;
	p->dec_part = 0;
	return 0;
}

static int
cobc_bcompare (const void *p1, const void *p2)
{
	const void	**tptr;

	tptr = (const void **)p2;
	return strcmp (p1, *tptr);
}

enum name_error_reason {
	INVALID_LENGTH = 1,
	EMPTY_NAME,
	SPACE_UNDERSCORE_FIRST_CHAR,
	GNUCOBOL_PREFIX,
	C_KEYWORD,
	CONTAINS_DIRECTORY_SEPARATOR
};

static void
cobc_error_name (const char *name, const enum cobc_name_type type,
		 const enum name_error_reason reason)
{
	const char *s, *dots = "";

	switch (reason) {
	case INVALID_LENGTH:	/* > COB_MAX_NAMELEN ("normal mode ") || > COB_MAX_WORDLEN */
		s = _(" - length exceeds maximum");
		dots = "...";
		break;
	case EMPTY_NAME:
		s = _(" - name cannot be empty");
		break;
	case SPACE_UNDERSCORE_FIRST_CHAR:
		s = _(" - name cannot begin with space or underscore");
		break;
	case GNUCOBOL_PREFIX:
		s = _(" - name cannot begin with 'cob_' or 'COB_'");
		break;
	case C_KEYWORD:
		s = _(" - name duplicates a 'C' keyword");
		break;
	case CONTAINS_DIRECTORY_SEPARATOR:
		s = _(" - name cannot contain a directory separator");
		break;
	default:
		s = "";
		break;
	}

	switch (type) {
	case FILE_BASE_NAME:
		cb_error (_("invalid file base name '%s'%s"),
			name, s);
		break;
	case ENTRY_NAME:
		cb_error (_("invalid ENTRY '%s'%s"), name, s);
		break;
	case PROGRAM_ID_NAME:
		cb_error (_("invalid PROGRAM-ID '%s'%s"), name, s);
		break;
	default:
		/* internal rare error (should be raised for 'a-[150 times]-b'),
		   no need for translation */
		cobc_err_msg ("unknown name error '%.32s%s'%s", name, dots, s);
		break;
	}
}

size_t
cobc_check_valid_name (const char *name, const enum cobc_name_type prechk)
{
	const char	*p;
	size_t		len;

	/* Check name doesn't contain path separator. */
	for (p = name, len = 0; *p; p++, len++) {
		if (*p == '/' || *p == '\\') {
			cobc_error_name (name, prechk,
					 CONTAINS_DIRECTORY_SEPARATOR);
			return 1;
		}
	}

	/* Check name is of valid length. */
	if (len < 1) {
		cobc_error_name (name, prechk, EMPTY_NAME);
		return 1;
	}
	if (cb_flag_main || !cb_relaxed_syntax_checks) {
		if (len > COB_MAX_NAMELEN) {
			cobc_error_name (name, prechk, INVALID_LENGTH);
			return 1;
		}
	} else {
		if (len > COB_MAX_WORDLEN) {
			cobc_error_name (name, prechk, INVALID_LENGTH);
			return 1;
		}
	}

	/* missing check (here): encoded length > internal buffer,
	   see cob_encode_program_id */

	if (*name == '_' || *name == ' ') {
		cobc_error_name (name, prechk, SPACE_UNDERSCORE_FIRST_CHAR);
		return 1;
	}

	/* Check name does not begin with the libcob prefixes cob_ and COB_. */
	if (prechk && len > 3 &&
	    (!memcmp (name, "cob_", (size_t)4) ||
	     !memcmp (name, "COB_", (size_t)4))) {
		cobc_error_name (name, prechk, GNUCOBOL_PREFIX);
		return 1;
	}

	/* Check name is not a C keyword. */
	if (bsearch (name, cob_csyns, COB_NUM_CSYNS,
		     sizeof (name), cobc_bcompare)) {
		cobc_error_name (name, prechk, C_KEYWORD);
		return 1;
	}

	return 0;
}

#if 0	/* to be merged */
/* Turn generation of runtime exceptions on/off */

static void
turn_ec_for_table (struct cb_exception *table, const size_t table_len,
		   struct cb_exception ec, const int to_on_off)
{
	size_t	i;

	if (ec.code & 0x00FF) {
		/* Set individual level-1 EC */
		for (i = 0; i < table_len; ++i) {
			if (table[i].code == ec.code) {
				table[i].enable = to_on_off;
				table[i].explicit_enable_val = 1;
				break;
			}
		}
	} else if (ec.code != 0) {
		/*
		  Simon: ToDo: Group activation; check occurences of
		  EC-generation
		*/
		/* Set all ECs subordinate to level-2 EC */
		for (i = 0; i < table_len; ++i) {
			if ((table[i].code & 0xFF00) == ec.code) {
				table[i].enable = to_on_off;
				table[i].explicit_enable_val = 1;
			}
		}
	} else {
		/* EC-ALL; set all ECs */
		for (i = 0; i < table_len; ++i) {
			table[i].enable = to_on_off;
			table[i].explicit_enable_val = 1;
		}
	}
}


static unsigned int
turn_ec_io (struct cb_exception ec_to_turn,
	    const cob_u32_t to_on_off,
	    cb_tree loc,
	    struct cb_text_list ** const ec_list)
{
	if (!(*ec_list)->next
	 || !strncmp ((*ec_list)->next->text, "EC-", 3)) {
		/* This >>TURN applies globally */
		turn_ec_for_table (cb_exception_table,
				   cb_exception_table_len,
				   ec_to_turn,
				   to_on_off);
		return 0;
	}

	/* The >>TURN applies to a list of files */
	do {
		struct cb_file	*f = NULL;
		cb_tree	l = NULL;
		*ec_list = (*ec_list)->next;

		/* Find file */
		for (l = current_program->file_list; l; l = CB_CHAIN (l)) {
			f = CB_FILE (CB_VALUE (l));
			if (!strcasecmp (f->name, (*ec_list)->text)) {
				break;
			}
			f = NULL;
		}
		/* Error if no file */
		if (!f) {
			cb_error_x (loc, _("file '%s' does not exist"), (*ec_list)->text);
			return 1;
		}
		
		/* Apply to file's exception list */
		turn_ec_for_table (f->exception_table, cb_io_exception_table_len,
				   ec_to_turn, to_on_off);
	} while ((*ec_list)->next && !strncmp ((*ec_list)->next->text, "EC-", 3));

	return 0;
}

static unsigned int
ec_duped (struct cb_text_list *ec_list, struct cb_text_list *ec,
	  const cob_u32_t ec_idx, cb_tree loc)
{
	struct cb_text_list	*ec_dupchk;

	/* TO-DO: Is duplication a problem? */
	/* TO-DO: Does this algo work? At least add a testcase... */
	for (ec_dupchk = ec_list; ec_dupchk; ec_dupchk = ec_dupchk->next) {
		if (ec_dupchk == ec) {
			return 0;
		}
		if (ec_dupchk->text
		 && !cb_strcasecmp(ec->text, ec_dupchk->text)) {
			cb_error_x (loc, _("duplicate exception '%s'"),
				    CB_EXCEPTION_NAME (ec_idx));
			ec_dupchk = NULL;
			return 1;
		}
	}

	return 0;
}

/*
  Simon: ToDo: Move save/restore of activated exceptions before
  preparse; after C generation A dynamic save (only if changed)
  and restore (only if set) would be nice
*/
unsigned int
cobc_turn_ec (struct cb_text_list *ec_list, const cob_u32_t to_on_off, cb_tree loc)
{
	cob_u32_t ec_idx, i;
	struct cb_text_list	*ec;

	if (to_on_off) {
		/* TO-DO: Only if >>TURN ... ON WITH LOCATION found? */
		cb_flag_source_location = 1;
	}

	for (ec = ec_list; ec; ec = ec->next) {
		/* upper-case exception name */
		size_t len = strlen (ec->text);
		unsigned char *upme = (unsigned char*)ec->text;
		for (i = 0; i < len; ++i) {
			upme[i] = (cob_u8_t)toupper (upme[i]);
		}
		/* extract exception code via text comparison */
		ec_idx = 0;
		for (i = (enum cob_exception_id)1; i < COB_EC_MAX; ++i) {
			if (!strcmp (ec->text, CB_EXCEPTION_NAME (i))) {
				ec_idx = i;
				break;
			}
		}

		/* Error if not a known exception name */
		/* TO-DO: What about EC-USER? */
		if (ec_idx == 0) {
			cb_error_x (loc, _("invalid exception-name: %s"),
				    ec->text);
			return 1;
		}

		if (ec_duped (ec_list, ec, ec_idx, loc)) {
			return 1;
		}

		if (!strncmp(CB_EXCEPTION_NAME(ec_idx), "EC-I-O", 6)) {
			if (turn_ec_io (cb_exception_table[ec_idx], to_on_off,
					loc, &ec)) {
				return 1;
			}
		} else {
			turn_ec_for_table (cb_exception_table,
					   cb_exception_table_len,
					   cb_exception_table[ec_idx], to_on_off);
		}
	}

	return 0;
}

void
cobc_apply_turn_directives (void)
{
	struct cb_tree_common	loc;

	loc.source_file = cb_source_file;
	loc.source_column = 0;
	
	/* Apply all >>TURN directives the scanner has passed */
	while (cb_turn_list
	       && cb_turn_list->line <= cb_source_line
	       && cb_turn_list->line != -1) {
		if (cb_turn_list->with_location) {
			cb_flag_source_location = 1;
		}
		loc.source_line = cb_turn_list->line;
		cobc_turn_ec (cb_turn_list->ec_names, cb_turn_list->enable, &loc);

		cb_turn_list = cb_turn_list->next;
		/* CHECKME: Should head of cb_turn_list be freed? Why doesn't
		   cobc_plex_free exist? */
	}
}

/* set the specified exception-name to the given value,
   note: exception-name may also specified without EC-prefix here */
static unsigned int
cobc_deciph_ec (const char *opt, const cob_u32_t to_on_off)
{
	struct cb_text_list	*cb_ec_list = NULL;
	char	*p;
	char	*q;
	char	wrk[COB_MAX_NAMELEN + 1];
	struct cb_tree_common	loc;

	p = cobc_strdup (opt);
	q = strtok (p, " ");
	while (q) {
		if (strncasecmp (q, "ec-", 3)) {
			snprintf (wrk, COB_MAX_NAMELEN, "EC-%s", q);
			CB_TEXT_LIST_ADD (cb_ec_list, wrk);
		} else {
			CB_TEXT_LIST_ADD (cb_ec_list, q);
		}
		q = strtok (NULL, " ");
	}
	cobc_free (p);

	loc.source_file = cb_source_file;
	loc.source_column = 0;
	loc.source_line = 0;
	return cobc_turn_ec (cb_ec_list, to_on_off, &loc);
}
#endif	/* to be merged */

/* Local functions */

static void
cobc_chk_buff_size (const size_t bufflen)
{
	if (bufflen >= cobc_buffer_size) {
		cobc_buffer_size = bufflen + 32;
		cobc_buffer = cobc_main_realloc (cobc_buffer, cobc_buffer_size);
	}
}

/* decipher a positive int from option argument,
   if allow_quote is set and quotes are used set int from char,
   returns -1 on error */
static int
cobc_deciph_optarg (const char *p, const int allow_quote)
{
	const unsigned char	*s;
	size_t			len;
	size_t			i;
	size_t			n;

	len = strlen (p);
	if (!len) {
		return -1;
	}
	s = (const unsigned char *)p;
	if (allow_quote) {
		if (*s == '"' || *s == '\'') {
			if (len != 3 || *(s + 2) != *s) {
				return -1;
			}
			return (int)(*(s + 1));
		}
		if (*s < '0' || *s > '9') {
			if (len != 1) {
				return -1;
			}
			return (int)*s;
		}
	}
	n = 0;
	for (i = 0; i < len; ++i) {
		if (s[i] < '0' || s[i] > '9') {
			return -1;
		}
		n *= 10;
		n += (s[i] & 0x0F);
		if (n > INT_MAX) return INT_MAX;
	}
	return (int)n;
}

/* exit to OS before processing a COBOL/C source file */
DECLNORET static void COB_A_NORETURN
cobc_early_exit (int ret_code)
{
	if (fatal_startup_error) {
		fatal_startup_error = 0;
		cobc_err_exit (_("please check environment variables as noted above"));
	}
	cobc_free_mem ();
	exit (ret_code);
}

DECLNORET static void COB_A_NORETURN
cobc_err_exit (const char *fmt, ...)
{
	va_list		ap;

	fputs ("cobc: ", stderr);
	fputs (_("error: "), stderr);
	va_start (ap, fmt);
	vfprintf (stderr, fmt, ap);
	va_end (ap);
	putc ('\n', stderr);
	fflush (stderr);
	cobc_early_exit (EXIT_FAILURE);
}

static struct cb_define_struct *
cb_define_list_add (struct cb_define_struct *list, const char *text)
{
	struct cb_define_struct	*p;
	struct cb_define_struct	*l;
	char			*s;
	char			*x;

	x = cobc_strdup (text);
	s = strtok (x, "=");

	/* Check duplicate */
	for (l = list; l; l = l->next) {
		if (!strcasecmp (s, l->name)) {
			cobc_err_msg (_("duplicate DEFINE '%s' - ignored"), s);
			cobc_free (x);
			return list;
		}
	}

	p = cobc_main_malloc (sizeof (struct cb_define_struct));
	p->next = NULL;
	p->name = cobc_check_string (s);
	p->deftype = PLEX_DEF_NONE;
	s = strtok (NULL, "");
	if (cobc_set_value (p, s)) {
		cobc_free (x);
		return NULL;
	}

	cobc_free (x);

	if (!list) {
		p->last = p;
		return p;
	}
	list->last->next = p;
	list->last = p;
	return list;
}

static char *
cobc_getenv (const char *env)
{
	char	*p;

	p = getenv (env);
	if (!p || *p == 0 || *p == ' ') {
		return NULL;
	}
	return cobc_main_strdup (p);
}

/*
 * Like cobc_getenv, except value is not allowed to hold any PATHSEP_CHAR
 */
static char *
cobc_getenv_path (const char *env)
{
	char	*p, *pos;

	p = getenv (env);
	if (!p || *p == 0) {
		return NULL;
	}
	pos = strchr (p, PATHSEP_CHAR);
	if (pos != NULL) {
		cobc_err_msg (_("environment variable '%s' is '%s'; should not contain '%c'"), env, p, PATHSEP_CHAR);
		fatal_startup_error = 1;
		*pos = 0;	/* strip PATHSEP_CHAR and following */
	}
	return cobc_main_strdup (p);
}

/* compiler startup phase: add string to internal flags which keep its own length,
   if target field is too small reallocate the memory with doubled size */
static void
cobc_add_str (char **var, size_t *cursize, const char *s1, const char *s2,
	      const char *s3)
{
	size_t	calcsize;

	if (!s1) {
		return;
	}

	calcsize = strlen (*var);
	calcsize += strlen (s1);
	if (s2) {
		calcsize += strlen (s2);
	}
	if (s3) {
		calcsize += strlen (s3);
	}
	/* LCOV_EXCL_START */
	if (calcsize >= 131072) {
		/* Arbitrary limit */
		cobc_err_exit (_("parameter buffer size exceeded"));
	}
	/* LCOV_EXCL_STOP */
	if (calcsize >= *cursize) {
		while (*cursize <= calcsize) {
			*cursize *= 2;
		}
		*var = cobc_main_realloc (*var, *cursize);
	}
	strcat (*var, s1);
	if (s2) {
		strcat (*var, s2);
	}
	if (s3) {
		strcat (*var, s3);
	}
}

static void
cobc_check_action (const char *name)
{
	int ret;
	if (!name || access (name, F_OK)) {
		return;
	}
	if (!save_temps) {
		(void)unlink (name);
		return;
	}
	if (save_temps_dir) {
		char	temp_buff[COB_MEDIUM_BUFF];

		snprintf (temp_buff, (size_t)COB_MEDIUM_MAX,
			  "%s%s%s", save_temps_dir, SLASH_STR, name);
		temp_buff[COB_MEDIUM_MAX] = 0;
		/* Remove possible target file - ignore return */
		(void)unlink (temp_buff);
		ret = rename (name, temp_buff);
		/* LCOV_EXCL_START */
		if (ret) {
			cobc_err_msg (_("warning: could not move temporary file to %s"),
					temp_buff);
		}
		/* LCOV_EXCL_STOP */
	}
}

static void
clean_up_intermediates (struct filename *fn, const int status)
{
	struct local_filename	*lf;
	cob_u32_t		i;
#ifdef HAVE_8DOT3_FILENAMES
	char	*buffer;
#endif
	for (lf = fn->localfile; lf; lf = lf->next) {
		if (lf->local_fp) {
			fclose (lf->local_fp);
			lf->local_fp = NULL;
		}
	}
	if (save_all_src) {
		return;
	}
	if (fn->need_preprocess &&
		(status || cb_compile_level > CB_LEVEL_PREPROCESS ||
		 (cb_compile_level == CB_LEVEL_PREPROCESS && save_temps))) {
		cobc_check_action (fn->preprocess);
	}
	if (save_c_src) {
		return;
	}
	if (fn->need_translate &&
		(status || cb_compile_level > CB_LEVEL_TRANSLATE ||
		 (cb_compile_level == CB_LEVEL_TRANSLATE && save_temps))) {
		cobc_check_action (fn->translate);
		cobc_check_action (fn->trstorage);
		if (fn->localfile) {
			for (lf = fn->localfile; lf; lf = lf->next) {
				cobc_check_action (lf->local_name);
			}
		} else if (fn->translate) {
			/* If we get syntax errors, we do not
			   know the number of local include files */
#ifndef HAVE_8DOT3_FILENAMES
			snprintf (cobc_buffer, cobc_buffer_size,
				 "%s.l.h", fn->translate);
#else
			/* for 8.3 filenames use no ".c" prefix and only one period */
			buffer = cobc_strdup (fn->translate);
			*(buffer + strlen(buffer) - 2) = 'l';
			*(buffer + strlen(buffer) - 1) = 0;
			snprintf (cobc_buffer, cobc_buffer_size,
				 "%s.h", buffer);
			cobc_free (buffer);
#endif
			cobc_buffer[cobc_buffer_size] = 0;
			for (i = 0; i < 30U; ++i) {
				if (i) {
#ifndef HAVE_8DOT3_FILENAMES
					snprintf (cobc_buffer, cobc_buffer_size,
						 "%s.l%u.h", fn->translate, i);
#else
					snprintf (cobc_buffer, cobc_buffer_size,
						"%s%u.h", buffer, i);
#endif
					cobc_buffer[cobc_buffer_size] = 0;
				}
				if (!access (cobc_buffer, F_OK)) {
					unlink (cobc_buffer);
				} else if (i) {
					break;
				}
			}
#ifdef HAVE_8DOT3_FILENAMES
			cobc_free (buffer);
#endif
		}
	}
}

static void
cobc_clean_up (const int status)
{
	struct filename		*fn;

	if (cb_src_list_file) {
		if (cb_src_list_file != stdout) {
			fclose (cb_src_list_file);
		}
		cb_src_list_file = NULL;
	}
	if (cb_listing_file) {
		fclose (cb_listing_file);
		cb_listing_file = NULL;
	}
	if (cb_storage_file) {
		fclose (cb_storage_file);
		cb_storage_file = NULL;
	}

	if (ppin) {
		fclose (ppin);
		ppin = NULL;
	}

	if (ppout) {
		fclose (ppout);
		ppout = NULL;
	}
	plex_call_destroy ();
	plex_clear_all ();

	if (yyin) {
		fclose (yyin);
		yyin = NULL;
	}
	if (yyout) {
		fclose (yyout);
		yyout = NULL;
	}
	ylex_call_destroy ();
	ylex_clear_all ();

	for (fn = file_list; fn; fn = fn->next) {
		if (fn->need_assemble &&
		    (status || cb_compile_level > CB_LEVEL_ASSEMBLE ||
		     (cb_compile_level == CB_LEVEL_ASSEMBLE && save_temps))) {
			cobc_check_action (fn->object);
		}
		clean_up_intermediates (fn, status);
	}
	cobc_free_mem ();
	file_list = NULL;
}

static void
set_listing_date (void)
{
	char	*time_buff;
	if (!current_compile_time.year) {
		current_compile_time = cob_get_current_date_and_time();
	}

	/* the following code is likely to get replaced by a self-written format */
	current_compile_tm.tm_sec = current_compile_time.second;
	current_compile_tm.tm_min = current_compile_time.minute;
	current_compile_tm.tm_hour = current_compile_time.hour;
	current_compile_tm.tm_mday = current_compile_time.day_of_month;
	current_compile_tm.tm_mon = current_compile_time.month - 1;
	current_compile_tm.tm_year = current_compile_time.year - 1900;
	if (current_compile_time.day_of_week == 7) {
		current_compile_tm.tm_wday = 0;
	} else {
		current_compile_tm.tm_wday = current_compile_time.day_of_week;
	}
	current_compile_tm.tm_yday = current_compile_time.day_of_year;
	current_compile_tm.tm_isdst = current_compile_time.is_daylight_saving_time;
	time_buff = asctime (&current_compile_tm);
	/* LCOV_EXCL_START */
	if (!time_buff) {
		strncpy (cb_listing_date, "DATE BUG, PLEASE REPORT", CB_LISTING_DATE_MAX);
		return;
	}
	/* LCOV_EXCL_STOP */
	*strchr (time_buff, '\n') = '\0';
	strncpy (cb_listing_date, time_buff, CB_LISTING_DATE_MAX);
}


DECLNORET static void COB_A_NORETURN
cobc_terminate (const char *str)
{
	if (cb_src_list_file) {
		set_listing_date ();
		set_standard_title ();
		cb_listing_linecount = cb_lines_per_page;
		strncpy (cb_listing_filename, str, FILENAME_MAX);
		cb_listing_filename[FILENAME_MAX - 1] = 0;
		print_program_header ();
	}
	cb_perror (0, "cobc: %s: %s", str, cb_get_strerror ());
	if (cb_src_list_file) {
		print_program_trailer ();
	}
	cobc_clean_up (1);
	exit (EXIT_FAILURE);
}

static void
cobc_abort_msg (void)
{
	char *prog_id;
	const char *prog_type;

	if (cb_source_file) {
		if (current_program) {
			if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION) {
				prog_type = "FUNCTION-ID";
			} else {
				prog_type = "PROGRAM-ID";
			}
			if (current_program->orig_program_id) {
				prog_id = (char *)current_program->orig_program_id;
			} else {
				prog_id = (char *)_("unknown");
			}
		} else {
			prog_type = prog_id = (char *)_("unknown");
		}
		if (!cb_source_line) {
			cobc_err_msg (_("aborting codegen for %s (%s: %s)"),
				cb_source_file, prog_type, prog_id);
		} else {
			cobc_err_msg (_("aborting compile of %s at line %d (%s: %s)"),
				cb_source_file, cb_source_line, prog_type, prog_id);
		}
	} else {
		cobc_err_msg (_("aborting"));
	}
}

/* return to OS in case of hard errors after trying to output the error to
   listing file if active */
void
cobc_abort_terminate (int should_be_reported)
{
	/* note we returned 99 for aborts earlier but autotest will
	   "recognize" status 99 as failure (you cannot "expect" the return 99 */
	const int ret_code = 97;

	if (!should_be_reported
	 &&	cb_src_list_file
	 && cb_listing_file_struct
	 && cb_listing_file_struct->name) {
		print_program_listing ();
	}
	putc ('\n', stderr);
	cobc_abort_msg ();

	if (should_be_reported) {
		cobc_err_msg (_("Please report this!"));
		if (cb_src_list_file
		 && cb_listing_file_struct
		 && cb_listing_file_struct->name) {
			print_program_listing ();
		}
	}
	cobc_clean_up (ret_code);
	exit (ret_code);
}

static void
cobc_sig_handler (int sig)
{
#if defined (SIGINT) || defined (SIGQUIT) || defined (SIGTERM) || defined (SIGPIPE)
	int ret = 0;
#endif

	cobc_abort_msg ();
#if defined (SIGINT) || defined (SIGQUIT) || defined (SIGTERM) || defined (SIGPIPE)
#ifdef SIGINT
	if (sig == SIGINT) ret = 1;
#endif
#ifdef SIGQUIT
	if (sig == SIGQUIT) ret = 1;
#endif
#ifdef SIGTERM
	if (sig == SIGTERM) ret = 1;
#endif
#ifdef SIGPIPE
	if (sig == SIGPIPE) ret = 1;
#endif

	/* LCOV_EXCL_START */
	if (!ret) {
		cobc_err_msg (_("Please report this!"));
	}
	/* LCOV_EXCL_STOP */
#else
	COB_UNUSED (sig);
#endif
	save_temps = 0;
	cobc_clean_up (1);
}

/* Command line */

static void
cobc_print_version (void)
{
	printf ("cobc (%s) %s.%d\n", PACKAGE_NAME, PACKAGE_VERSION, PATCH_LEVEL);
	puts ("Copyright (C) 2022 Free Software Foundation, Inc.");
	printf (_("License GPLv3+: GNU GPL version 3 or later <%s>"), "https://gnu.org/licenses/gpl.html");
	putchar ('\n');
	puts (_("This is free software; see the source for copying conditions.  There is NO\n"
	        "warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE."));
	printf (_("Written by %s"), "Keisuke Nishida, Roger While, Ron Norman, Simon Sobisch, Edward Hart");
	putchar ('\n');
	printf (_("Built     %s"), cb_cobc_build_stamp);
	putchar ('\n');
	printf (_("Packaged  %s"), COB_TAR_DATE);
	putchar ('\n');
	printf ("%s %s", _("C version"), GC_C_VERSION_PRF GC_C_VERSION);
	putchar ('\n');
}

static void
cobc_print_shortversion (void)
{
	printf ("cobc (%s) %s.%d\n",
		PACKAGE_NAME, PACKAGE_VERSION, PATCH_LEVEL);
	printf (_("Built     %s"), cb_cobc_build_stamp);
	putchar ('\t');
	printf (_("Packaged  %s"), COB_TAR_DATE);
	putchar ('\n');
	printf ("%s %s", _("C version"), GC_C_VERSION_PRF GC_C_VERSION);
	putchar ('\n');
}

static void
cobc_cmd_print (const char *cmd)
{
	char	*p;
	char	*token;
	size_t	n;
	size_t	toklen;

	if (verbose_output >= 0) {
		fputs (_("executing:"), stderr);
	} else {
		fputs (_("to be executed:"), stderr);
	}
	/* Check if it fits in 80 characters */
	if (strlen (cmd) < 64) {
		fprintf (stderr, "\t%s\n", (char *)cmd);
		fflush (stderr);
		return;
	}
	putc ('\t', stderr);
	p = cobc_strdup (cmd);
	n = 0;
	token = strtok (p, " ");
	for (; token; token = strtok (NULL, " ")) {
		toklen = strlen (token) + 1;
		if ((n + toklen) > 63) {
			fprintf (stderr, "\n\t\t");
			n = 0;
		}
		fprintf (stderr, "%s%s", (n ? " " : ""), token);
		n += toklen;
	}
	cobc_free (p);
	putc ('\n', stderr);
	fflush (stderr);
}

static void
cobc_var_print (const char *msg, const char *val, const unsigned int env)
{
	char	*p;
	char	*token;
	size_t	n;
	int 	lablen;
	size_t	toklen;

	if (!env) {
		printf ("%-*.*s : ", CB_IMSG_SIZE, CB_IMSG_SIZE, msg);
	} else {
		printf ("  %s: ", _("env"));
		lablen = CB_IMSG_SIZE - 2 - (int)strlen (_("env")) - 2;
		printf ("%-*.*s : ", lablen, lablen, msg);
	}
	if (strlen (val) <= CB_IVAL_SIZE) {
		printf ("%s\n", val);
		return;
	}
	p = cobc_strdup (val);
	n = 0;
	token = strtok (p, " ");
	for (; token; token = strtok (NULL, " ")) {
		toklen = strlen (token) + 1;
		if ((n + toklen) > CB_IVAL_SIZE) {
			if (n) {
				printf ("\n%*.*s", CB_IMSG_SIZE + 3,
					CB_IMSG_SIZE + 3, " ");
			}
			n = 0;
		}
		printf ("%s%s", (n ? " " : ""), token);
		n += toklen;
	}
	putchar ('\n');
	cobc_free (p);
}

static void
cobc_var_and_envvar_print (const char* name, const char* defined_val)
{
	char* s = getenv (name);

	cobc_var_print (name, defined_val, 0);

	if (s && *s) {
		cobc_var_print (name, s, 1);
	}

}

/* provides info about how cobc was built/configured, some details about
   how the compiler/linker will be invoked, it should not call into any
   other library than libcob and through libcob should only access "static"
   values */
static void
cobc_print_info (void)
{
	char	buff[64], iomods[80];
	char	*s;
	int		num;

	cobc_print_version ();
	putchar ('\n');
	puts (_("build information"));
	cobc_var_print (_("build environment"),	COB_BLD_BUILD, 0);
	cobc_var_print ("CC", COB_BLD_CC, 0);
	/* Note: newline because most compilers define a long version string (> 30 characters) */
	cobc_var_print (_("C version"), GC_C_VERSION_PRF GC_C_VERSION, 0);
	cobc_var_print ("CPPFLAGS",		COB_BLD_CPPFLAGS, 0);
	cobc_var_print ("CFLAGS",		COB_BLD_CFLAGS, 0);
	cobc_var_print ("LD",			COB_BLD_LD, 0);
	cobc_var_print ("LDFLAGS",		COB_BLD_LDFLAGS, 0);
	putchar ('\n');
	puts (_("GnuCOBOL information"));
	cobc_var_and_envvar_print ("COB_CC",		COB_CC);
	cobc_var_and_envvar_print ("COB_CFLAGS",	cob_relocate_string (COB_CFLAGS));
#ifndef	_MSC_VER
	if (verbose_output) {
#ifdef COB_STRIP_CMD
		char *strip_cmd = (char *)COB_STRIP_CMD;
#else
		char *strip_cmd = _("disabled");
#endif
		cobc_var_print ("COB_STRIP_CMD",	strip_cmd, 0);
	}
	cobc_var_and_envvar_print ("COB_DEBUG_FLAGS", COB_DEBUG_FLAGS);
#endif
	cobc_var_and_envvar_print ("COB_LDFLAGS",	cob_relocate_string (COB_LDFLAGS));
	cobc_var_and_envvar_print ("COB_LIBS",		cob_relocate_string (COB_LIBS));
	cobc_var_and_envvar_print ("COB_CONFIG_DIR",cob_getenv_value ("COB_CONFIG_DIR"));
	cobc_var_and_envvar_print ("COB_SCHEMA_DIR",cob_getenv_value ("COB_SCHEMA_DIR"));
	cobc_var_and_envvar_print ("COB_COPY_DIR",	cob_getenv_value ("COB_COPY_DIR"));
	if ((s = getenv ("COBCPY")) != NULL && *s) {
		cobc_var_print ("COBCPY",	s, 1);
	}
	{
		const char* val;
#if defined (_MSC_VER)
		val = "MSC";
#else
		val = "GCC";
#endif
		cobc_var_and_envvar_print ("COB_MSG_FORMAT", val);
	}
	cobc_var_print ("COB_OBJECT_EXT",	COB_OBJECT_EXT, 0);
	cobc_var_print ("COB_MODULE_EXT",	COB_MODULE_EXT, 0);
	cobc_var_print ("COB_EXE_EXT",		COB_EXE_EXT, 0);

#ifdef COB_64_BIT_POINTER
	cobc_var_print ("64bit-mode",	_("yes"), 0);
#else
	cobc_var_print ("64bit-mode",	_("no"), 0);
#endif

#ifdef	COB_LI_IS_LL
	cobc_var_print ("BINARY-C-LONG",	_("8 bytes"), 0);
#else
	cobc_var_print ("BINARY-C-LONG",	_("4 bytes"), 0);
#endif

#ifdef WORDS_BIGENDIAN
	cobc_var_print (_("endianness"),	_("big-endian"), 0);
#else
	cobc_var_print (_("endianness"),	_("little-endian"), 0);
#endif

#ifdef COB_EBCDIC_MACHINE
	cobc_var_print (_("native character set"),	_("EBCDIC"), 0);
#else
	cobc_var_print (_("native character set"),	_("ASCII"), 0);
#endif

	cobc_var_print (_("extended screen I/O"),	_(WITH_CURSES), 0);

	snprintf (buff, sizeof(buff), "%d", WITH_VARSEQ);
	cobc_var_print (_("variable file format"),	buff, 0);
	if ((s = getenv ("COB_VARSEQ_FORMAT")) != NULL) {
		cobc_var_print ("COB_VARSEQ_FORMAT", s, 1);
	}

#ifdef	WITH_SEQRA_EXTFH
	cobc_var_print (_("sequential file handler"),	"EXTFH (obsolete)", 0);
#else
	cobc_var_print (_("sequential file handler"),	_("built-in"), 0);
#endif


	num = 0;
	strcpy(iomods,"");
#if defined(WITH_CISAM) 	|| defined(WITH_DISAM) \
	|| defined(WITH_VBISAM) || defined(WITH_VISAM) \
	|| defined(WITH_ODBC)	|| defined(WITH_OCI) \
	|| defined(WITH_INDEX_EXTFH) || defined(WITH_DB) || defined(WITH_LMDB)
#if defined	(WITH_INDEX_EXTFH)
	cobc_var_print (_("indexed file handler"),		"EXTFH (obsolete)", 0);
#endif
#if defined	(WITH_CISAM)
	strcat(iomods,"C-ISAM");
	num++;
#endif
#if defined	(WITH_DISAM)
	if (num++ > 0) strcat(iomods,", ");
	strcat(iomods,"D-ISAM");
#endif
#if defined	(WITH_VISAM)
	if (num++ > 0) strcat(iomods,", ");
	strcat(iomods,"V-ISAM");
#endif
#if defined	(WITH_VBISAM)
	if (num++ > 0) strcat(iomods,", ");
	strcat(iomods,"VB-ISAM");
#endif
#if defined	(WITH_ODBC)
	if (num++ > 0) strcat(iomods,", ");
	strcat(iomods,"ODBC");
#endif
#if defined	(WITH_OCI)
	if (num++ > 0) strcat(iomods,", ");
	strcat(iomods,"OCI");
#endif
#if defined	(WITH_DB)
	if (num++ > 0) strcat(iomods,", ");
	strcat(iomods,"BDB");
#endif
#if defined	(WITH_LMDB)
	if (num++ > 0) strcat(iomods,", ");
	strcat(iomods,"LMDB");
#endif
	if (num > 1)
		cobc_var_print (_("indexed file handlers"),		iomods, 0);
	else
		cobc_var_print (_("indexed file handler"),		iomods, 0);
#if defined(WITH_IXDFLT) 
	if (num > 1)
	cobc_var_print (_("default indexed handler"),    WITH_IXDFLT, 0);
#endif
#else
	if (num == 0)
	cobc_var_print (_("indexed file handler"),		_("disabled"), 0);
#endif

#if defined(WITH_STD)
	cobc_var_print (_("default COBOL dialect"),	"-std=" WITH_STD, 0);
#endif

#if defined(WITH_FILE_FORMAT)
	if (WITH_FILE_FORMAT == COB_FILE_IS_MF)
		cobc_var_print (_("default file format"),	"-ffile-format=mf", 0);
	else if (WITH_FILE_FORMAT == COB_FILE_IS_GC)
		cobc_var_print (_("default file format"),	"-ffile-format=gc", 0);
#endif

#if defined(__MPIR_VERSION)
#if defined(HAVE_MPIR_H)
	cobc_var_print (_("mathematical library"),	"MPIR", 0);
#else
	cobc_var_print (_("mathematical library"),	"MPIR - GMP", 0);
#endif
#else
	cobc_var_print (_("mathematical library"),	"GMP", 0);
#endif

#ifdef WITH_XML2
	cobc_var_print (_("XML library"),		"libxml2", 0);
#else
	cobc_var_print (_("XML library"),		_("disabled"), 0);
#endif

	cobc_var_print (_("JSON library"),		_(WITH_JSON), 0);

#ifdef COB_DEBUG_LOG
	cobc_var_print ("DEBUG_LOG",		_("enabled"), 0);
#endif
}

static void
cobc_options_error_nonfinal (void)
{
	cobc_err_exit (_("only one of options 'E', 'S', 'C', 'c' may be specified"));
}

static void
cobc_options_error_build (void)
{
	cobc_err_exit (_("only one of options 'm', 'x', 'b' may be specified"));
}

/* decipher dump options given on command line */
static void
cobc_def_dump_opts (const char *opt, const int on)
{
	char	*p, *q;
	int 	dump_to_set;

	if (!cb_strcasecmp (opt, "ALL")) {
		if (on) {
			cb_flag_dump = COB_DUMP_ALL;
		} else {
			cb_flag_dump = COB_DUMP_NONE;
		}
		if (cb_flag_dump)
			cb_flag_symbols = 1;
		return;
	}

	p = cobc_strdup (opt);
	q = strtok (p, ",");
	if (!q) {
		q = (char *) "";
	}
	dump_to_set = 0;
	while (q) {
		if (!cb_strcasecmp (q, "FD")) {
			dump_to_set |= COB_DUMP_FD;
		} else if (!cb_strcasecmp (q, "WS")) {
			dump_to_set |= COB_DUMP_WS;
		} else if (!cb_strcasecmp (q, "LS")) {
			dump_to_set |= COB_DUMP_LS;
		} else if (!cb_strcasecmp (q, "RD")) {
			dump_to_set |= COB_DUMP_RD;
		} else if (!cb_strcasecmp (q, "SD")) {
			dump_to_set |= COB_DUMP_SD;
		} else if (!cb_strcasecmp (q, "SC")) {
			dump_to_set |= COB_DUMP_SC;
		} else if (!cb_strcasecmp (q, "LO")) {
			dump_to_set |= COB_DUMP_LO;
		} else {
			cobc_err_exit (_("option requires one of 'ALL', 'FD', 'WS', 'LS', "
			                 "'RD', 'FD', 'SC', 'LO' - not '%s'"), opt);
		}
		q = strtok (NULL, ",");
	}
	if (on) {
		cb_flag_dump |= dump_to_set;
	} else {
		cb_flag_dump ^= dump_to_set;
	}
	if (cb_flag_dump)
		cb_flag_symbols = 1;
	cobc_free (p);
}

/* decipher functions given on command line,
   checking that these are actually intrinsic functions */
static void
cobc_deciph_funcs (const char *opt)
{
	char	*p;
	char	*q;

	if (!cb_strcasecmp (opt, "ALL")) {
		cb_flag_functions_all = 1;
		return;
	}

	p = cobc_strdup (opt);
	q = strtok (p, ",");
	while (q) {
		if (!lookup_intrinsic (q, 1)) {
			cobc_err_exit (_("'%s' is not an intrinsic function"), q);
		}
		CB_TEXT_LIST_ADD (cb_intrinsic_list, q);
		q = strtok (NULL, ",");
	}
	cobc_free (p);
}

#if	defined (_WIN32)      || defined (__CYGWIN__) \
 || defined (__WATCOMC__) || defined (__BORLANDC__) \
 || defined (__OS400__)
static void
file_stripext (char *buff)
{
	char	*endp;

	endp = buff + strlen (buff) - 1U;
	while (endp > buff) {
		if (*endp == '/' || *endp == '\\') {
			break;
		}
		if (*endp == '.') {
			*endp = 0;
		}
		--endp;
	}
}
#endif

# define COB_BASENAME_KEEP_EXT ""
/* get basename from file, if optional parameter strip_ext is given then only
  strip the extension if it matches the parameter (must include the period),
  don't strip the extension if the parameter equals COB_BASENAME_KEEP_EXT;
  returns a pointer to the previous allocated basename_buffer */
static char *
file_basename (const char *filename, const char *strip_ext)
{
	const char	*p;
	const char	*startp;
	const char	*endp;
	size_t		len;

	/* LCOV_EXCL_START */
	if (!filename) {
		cobc_err_msg (_("call to '%s' with invalid parameter '%s'"),
			"file_basename", "filename");
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */

	/* Remove directory name */
	startp = NULL;
	for (p = filename; *p; p++) {
		if (*p == '/' || *p == '\\') {
			startp = p;
		}
	}
	if (startp) {
		startp++;
	} else {
		startp = filename;
	}

	/* Remove extension */
	if (!strip_ext || strcmp (strip_ext, COB_BASENAME_KEEP_EXT)) {
		endp = strrchr (filename, '.');
	} else {
		endp = startp;
	}
	if (endp > startp
		&& (!strip_ext || cb_strcasecmp (endp, strip_ext) == 0)) {
		len = endp - startp;
	} else {
		len = strlen (startp);
	}

	if (len >= basename_len) {
		basename_len = len + 16;
		basename_buffer = cobc_main_realloc (basename_buffer, basename_len);
	}
	/* Copy base name (possibly done before -> memmove) */
	memmove (basename_buffer, startp, len);
	basename_buffer[len] = 0;
	return basename_buffer;
}

/* get file extension from filename (without the leading period) */
static const char *
file_extension (const char *filename)
{
	const char *p;

	p = strrchr (filename, '.');
	if (p) {
		return p + 1;
	}
	return "";
}

/* set compile_level from output file if not set already */
static void
set_compile_level_from_file_extension (const char *filename)
{
	const char *ext;

	if (cb_flag_syntax_only) {
		return;
	}

	ext = file_extension (filename);

	if (cb_strcasecmp (ext, "i") == 0) {
		cb_compile_level = CB_LEVEL_PREPROCESS;
	} else if (cb_strcasecmp (ext, "c") == 0) {
		cb_compile_level = CB_LEVEL_TRANSLATE;
		save_c_src = 1;
	} else if (cb_strcasecmp (ext, "s") == 0 || cb_strcasecmp (ext, "asm") == 0) {
		cb_compile_level = CB_LEVEL_COMPILE;
	} else if (cb_strcasecmp (ext, COB_OBJECT_EXT) == 0) {
		cb_compile_level = CB_LEVEL_ASSEMBLE;
	} else if (cb_strcasecmp (ext, COB_MODULE_EXT) == 0 && !cobc_flag_main) {
		if (cobc_flag_library) {
			cb_compile_level = CB_LEVEL_LIBRARY;
		} else {
			cb_compile_level = CB_LEVEL_MODULE;
			cobc_flag_module = 1;
		}
	/* note: no setting of CB_LEVEL_EXECUTABLE as this should be explicit requested */
	}
}

#ifdef	_MSC_VER
/* MSC has issues with trailing slashes, which are auto-added in shell/cmd,
   we're removing them here to provide the same behaviour */
static void
remove_trailing_slash (char *data)
{
	char *path_end = data + strlen (data) - 1;
	if (*path_end == '/' || *path_end == '\\') {
		*path_end = 0;
	}
}
#endif

/* process command line options */
static int
process_command_line (const int argc, char **argv)
{
	struct cb_define_struct	*p;
	size_t			osize;
	int			c;
	int			idx;
	int			n;
	int			exit_option = 0;
	int			list_reserved = 0;
	int			list_registers = 0;
	int			list_intrinsics = 0;
	int			list_system_names = 0;
	int			list_system_routines = 0;
	enum cob_exception_id	i;
	char			ext[COB_MINI_BUFF];
	char			*conf_label;	/* we want a dynamic address for error.c, not a static one */
	char			*conf_entry;
	const char		*copt = NULL;	/* C optimization options */

	int			conf_ret = 0;
	int			error_all_warnings = 0;

	cb_dialect = "DEFAULT";
	cb_mf_ibm_comp = -1;
	cb_mf_files = 0;
	cb_warn_opt_val[(int)cb_warn_unsupported] = COBC_WARN_AS_ERROR;
#ifdef WITH_FILE_FORMAT
	if (WITH_FILE_FORMAT == COB_FILE_IS_MF)
		cb_mf_files = 1;
	else if (WITH_FILE_FORMAT == COB_FILE_IS_GC)
		cb_mf_files = 0;
#endif

	cob_config_dir = (const char *) cob_getenv_value ("COB_CONFIG_DIR");
	if (cob_config_dir == NULL) {
		cob_config_dir = COB_CONFIG_DIR;
	}

#if defined (_WIN32) || defined (__DJGPP__)
	if (!getenv ("POSIXLY_CORRECT")) {
		/* Translate command line arguments from DOS/WIN to UNIX style */
		int argnum = 0;
		while (++argnum < argc) {
			if (strrchr(argv[argnum], '/') == argv[argnum]) {
				if (argv[argnum][1] == '?' && !argv[argnum][2]) {
					argv[argnum] = "--help";
					continue;
				}
				argv[argnum][0] = '-';
			}
		}
	}
#endif

	/* First run of getopt: handle std/conf and all listing options
	   We need to postpone single configuration flags as we need
	   a full configuration to be loaded before */
	cob_optind = 1;
	while ((c = cob_getopt_long_long (argc, argv, short_options,
					  long_options, &idx, 1)) >= 0) {
		switch (c) {

		case '?':
			/* Unknown option or ambiguous */
			cobc_early_exit (EXIT_FAILURE);

		case 'h':
			/* --help */
			cobc_print_usage (argv[0]);
			if (verbose_output) {
				puts ("\n");
				fflush (stdout);
#ifdef _MSC_VER
				process ("cl.exe /help");
				puts ("\n");
				fflush (stdout);
				process ("link.exe");
#else
				cobc_buffer_size = strlen (cobc_cc) + 11;
				cobc_buffer = cobc_malloc (cobc_buffer_size);
				snprintf (cobc_buffer, cobc_buffer_size, "%s --help", cobc_cc);
#if (defined(__GNUC__) && !defined(__INTEL_COMPILER)) || defined(__TINYC__)
				if (verbose_output > 1) {
					snprintf (cobc_buffer, cobc_buffer_size, "%s -v --help", cobc_cc);
				}
#endif
				cobc_buffer[cobc_buffer_size] = 0;
				process (cobc_buffer);
				cobc_free (cobc_buffer);
				cobc_buffer = NULL;
#endif
			}
			cobc_early_exit (EXIT_SUCCESS);

		case 'V':
			/* --version */
			cobc_print_version ();
			if (verbose_output) {
				puts ("\n");
				fflush (stdout);
#ifdef _MSC_VER
				process ("cl.exe");
				puts ("\n");
#else
				cobc_buffer_size = strlen (cobc_cc) + 11;
				cobc_buffer = cobc_malloc (cobc_buffer_size);
#if defined(__TINYC__)
				snprintf (cobc_buffer, cobc_buffer_size, "%s -v", cobc_cc);
#else
				snprintf (cobc_buffer, cobc_buffer_size, "%s --version", cobc_cc);
#endif
#if (defined(__GNUC__) && !defined(__INTEL_COMPILER))
				if (verbose_output > 2) {
					snprintf (cobc_buffer, cobc_buffer_size, "%s -v", cobc_cc);
				}
#endif
				cobc_buffer[cobc_buffer_size] = 0;
				process (cobc_buffer);
				cobc_free (cobc_buffer);
				cobc_buffer = NULL;
#endif
			}
			cobc_early_exit (EXIT_SUCCESS);

		case 'i':
			/* --info */
			cobc_print_info ();
			cobc_early_exit (EXIT_SUCCESS);

		/*
			The following list options are postponed until
			until the configuration and exceptions are processed.
		*/
		case '5':
			/* --list-reserved */
			list_reserved = 1;
			exit_option = 1;
			break;

		case '6':
			/* --list-intrinsics */
			list_intrinsics = 1;
			exit_option = 1;
			break;

		case '7':
			/* --list-mnemonics */
			list_system_names = 1;
			exit_option = 1;
			break;

		case '8':
			/* --list-system */
			list_system_routines = 1;
			exit_option = 1;
			break;

		case '9':
			/* --list-registers */
			list_registers = 1;
			exit_option = 1;
			break;

		case 'q':
			/* --brief : reduced reporting */
			/* resets -verbose and removes the path to cobc in argv[0] */
			verbose_output = 0;
			strcpy (argv[0], "cobc");	/* set for simple compare in test suite
										   and other static output */
			break;

		case '#':
			/* --### : verbose output of commands, but don't execute them */
			if (!verbose_output) {
				cobc_print_shortversion ();
			}
			verbose_output = -1;
			break;

		case 'v':
			/* --verbose : Verbose reporting */
			/* VERY special case as we set different level by multiple calls */
			/* output version information when running very verbose -vv */
			/* pass verbose switch to invoked commands when running very very verbose -vvv */
			if (cob_optarg) {
				n = cobc_deciph_optarg (cob_optarg, 0);
				if (n == -1) {
					cobc_err_exit (COBC_INV_PAR, "-verbose");
				}
				verbose_output = n;
				if (verbose_output >= 1) {
					cobc_print_shortversion ();
				}
			} else {
				verbose_output++;
				if (verbose_output == 1) {
					cobc_print_shortversion ();
				}
			}
			break;

		case '$':
			/* -std=<xx> : Specify dialect */
			if (strlen (cob_optarg) > COB_MINI_MAX) {
				cobc_err_exit (COBC_INV_PAR, "-std");
			}
			for (n=0; cob_optarg[n] != 0 && cob_optarg[n] != '-'; n++)
				ext[n] = toupper(cob_optarg[n]);
			ext[n] = 0;
			cb_dialect = cobc_strdup (ext); 
			snprintf (cob_std_conf, sizeof(cob_std_conf), "%s.conf", cob_optarg);
			num_std++;
			break;

		case '&':
			/* -conf=<xx> : Specify dialect configuration file */
			if (strlen (cob_optarg) > COB_SMALL_MAX) {
				cobc_err_exit (COBC_INV_PAR, "-conf");
			}
			if (num_std > 1			/* -std was deferred so load now */
			 || memcmp(cob_std_conf,"default",7) != 0) {
				if (verbose_output) {
					fprintf(stderr, _("loading configuration file '%s'\n"),cob_std_conf);
				}
				conf_ret |= cb_load_std (cob_std_conf);
			}
			cob_std_conf[0] = 0;		/* Avoid reloading this one */
			conf_ret |= cb_load_conf (cob_optarg, 0);
			break;

		case 'd':
			/* --debug : Turn on all runtime checks */
			cb_flag_source_location = 1;
			cb_flag_stack_check = 1;
			cb_flag_symbols = 1;
			cobc_wants_debug = 1;
			break;

		default:
			/* as we postpone most options simply skip everything other here */
			break;
		}
	}

	/* Load default configuration file if necessary */
	if (cb_config_name == NULL
	 && cob_std_conf[0] > ' ') {
		if (verbose_output) {
			fprintf(stderr, _("loading configuration file '%s'\n"),cob_std_conf);
		}
		conf_ret |= cb_load_std (cob_std_conf);
	} else
	if (cb_config_name == NULL) {
#ifdef	WITH_STD
		strcpy(ext, WITH_STD);
		for (n=0; ext[n] != 0; n++)
			ext[n] = toupper(ext[n]);
		ext[n] = 0;
		cb_dialect = cobc_strdup (ext); 
		if (verbose_output) {
			fprintf(stderr, _("loading default configuration file '%s.conf'\n"),WITH_STD);
		}
		conf_ret |= cb_load_std (WITH_STD ".conf");
#else
		if (verbose_output) {
			fputs (_("loading standard configuration file 'default.conf'"), stderr);
			fputc ('\n', stderr);
		}
		conf_ret |= cb_load_std ("default.conf");
#endif
	}

	/* Exit for configuration errors resulting from -std/--conf/default.conf */
	if (conf_ret != 0) {
		cobc_early_exit (EXIT_FAILURE);
	}

	cob_optind = 1;
	while ((c = cob_getopt_long_long (argc, argv, short_options,
					  long_options, &idx, 1)) >= 0) {
		switch (c) {
		case 0:
			/* Defined flag */
			break;

		case 'h':
			/* --help */
		case 'V':
			/* --version */
		case 'i':
			/* --info */
		case '5':
			/* --list-reserved */
		case '6':
			/* --list-intrinsics */
		case '7':
			/* --list-mnemonics */
		case '8':
			/* --list-system */
		case '9':
			/* --list-registers */
			/* These options were all processed in the first getopt-run */
			break;

		case 'E':
			/* -E : Preprocess */
			if (cb_compile_level != 0) {
				cobc_options_error_nonfinal ();
			}
			cb_compile_level = CB_LEVEL_PREPROCESS;
			break;

		case 'C':
			/* -C : Generate C code */
			if (cb_compile_level != 0) {
				cobc_options_error_nonfinal ();
			}
			save_c_src = 1;
			cb_compile_level = CB_LEVEL_TRANSLATE;
			break;

		case 'S':
			/* -S : Generate assembler code */
			if (cb_compile_level != 0) {
				cobc_options_error_nonfinal ();
			}
#if defined(__TINYC__) || defined(__OS400__)
			/* check if we run the testsuite and skip the run,
			   otherwise exit with error  */
#define no_asm_msg _("the used C compiler is known to not be able to generate assembler code")
			if (getenv ("COB_IS_RUNNING_IN_TESTMODE")) {
				cobc_err_msg (no_asm_msg);
				cobc_early_exit (77);
			} else {
				cobc_err_exit (no_asm_msg);
			}
#endif
			cb_compile_level = CB_LEVEL_COMPILE;
			break;

		case 'c':
			/* -c : Generate C object code */
			if (cb_compile_level != 0) {
				cobc_options_error_nonfinal ();
			}
			cb_compile_level = CB_LEVEL_ASSEMBLE;
			break;

		case 'b':
			/* -b : Generate combined library module */
			if (cobc_flag_main || cobc_flag_module) {
				cobc_options_error_build ();
			}
			cobc_flag_library = 1;
			no_physical_cancel = 1;
			/* note: implied -fimplicit-init until GC 3.1 */
			break;

		case 'm':
			/* -m : Generate loadable module (default) */
			if (cobc_flag_main || cobc_flag_library) {
				cobc_options_error_build ();
			}
			cobc_flag_module = 1;
			break;

		case 'x':
			/* -x : Generate executable */
			if (cobc_flag_module || cobc_flag_library) {
				cobc_options_error_build ();
			}
			cobc_flag_main = 1;
			cb_flag_main = 1;
			no_physical_cancel = 1;
			break;

		case 'j':
			/* -j : Run job; compile, link and go, either by ./ or cobcrun */
			/* allows optional arguments, passed to program */
			cobc_flag_run = 1;
			if (cobc_run_args) {
				cobc_free (cobc_run_args);
			}
			if (cob_optarg) {
				cobc_run_args = cobc_strdup (cob_optarg);
			}
			break;

		case 'F':
			/* --free */
			cb_source_format = CB_FORMAT_FREE;
			break;

		case 'f':
			/* --fixed */
			cb_source_format = CB_FORMAT_FIXED;
			break;

		case 'q':
			/* --brief : reduced reporting */
		case '#':
			/* --### : verbose output of commands, but don't execute them */
		case 'v':
			/* --verbose : Verbose reporting */
			/* these options were processed in the first getopt-run */
			break;

		case 'o':
			/* -o : Output file */
			osize = strlen (cob_optarg);
			if (osize > COB_SMALL_MAX) {
				cobc_err_exit (_("invalid output file name"));
			}
			if (output_name) {
				cobc_main_free (output_name);
				cobc_main_free (output_name_buff);
			}
			output_name = cobc_main_strdup (cob_optarg);
			/* Allocate buffer plus extension reserve */
			output_name_buff = cobc_main_malloc (osize + 32U);
			break;

		case '0':
			/* -O0 : disable optimizations (or at least minimize them) */
			cb_flag_optimize_check = 0;
			strip_output = 0;
			cb_constant_folding = 0;
			copt = CB_COPT_0;
			break;

		case 'O':
			/* -O : Optimize */
			cb_flag_optimize_check = 1;
			copt = CB_COPT_1;
			break;

		case '2':
			/* -O2 : Optimize */
			cb_flag_optimize_check = 1;
			strip_output = 1;
			copt = CB_COPT_2;
			break;

		case '3':
			/* -O3 : Optimize */
			cb_flag_optimize_check = 1;
			strip_output = 1;
			copt = CB_COPT_3;
			break;

		case 's':
			/* -Os : Optimize */
			cb_flag_optimize_check = 1;
			strip_output = 1;
			copt = CB_COPT_S;
			break;

		case 'g':
			/* -g : Generate C debug code */
			save_all_src = 1;
			cb_source_debugging = 1;
			cb_flag_stack_check = 1;
			cb_flag_source_location = 1;
			cb_flag_symbols = 1;
			cb_flag_remove_unreachable = 0;
#ifdef COB_DEBUG_FLAGS
			COBC_ADD_STR (cobc_cflags, " ", cobc_debug_flags, NULL);
#endif
			break;

		case 'G':
			/* -G : Generate C debug code for use with gdb on COBOL source */
			cb_source_debugging = 1;
			cb_cob_line_num = 1;
			cb_flag_symbols = 1;
			cb_flag_remove_unreachable = 0;
			break;

		case '$':
			/* -std=<xx> : Specify dialect */
		case '&':
			/* -conf=<xx> : Specify dialect configuration file */
			/* These options were all processed in the first getopt-run */
			break;

		case '%':
			/* -f<tag>=<value> : Override configuration entry */
			/* hint: -f[no-]<tag> sets the var directly */
			/* including options -freserved=word / -fregister=word */
			conf_label = cobc_main_malloc (COB_MINI_BUFF);
			conf_entry = cobc_malloc (COB_MINI_BUFF - 2);
			snprintf (conf_label, COB_MINI_MAX, "-%s=%s",
				long_options[idx].name, cob_optarg);
			strncpy(conf_entry, conf_label + 2, COB_MINI_MAX - 2);
			conf_ret |= cb_config_entry (conf_entry, conf_label, 0);
			cobc_free (conf_entry);
			break;

		case 'd':
			/* --debug : Turn on all runtime checks */
			/* This options was processed in the first getopt-run */
			break;

		case '_':
			/* --save-temps : Save intermediary files */
			save_temps = 1;
			if (cob_optarg) {
				struct stat		st;
				if (save_temps_dir) {
					cobc_free (save_temps_dir);
					save_temps_dir = NULL;
				}
				if (stat (cob_optarg, &st) != 0 ||
				    !(S_ISDIR (st.st_mode))) {
					cobc_err_msg (_("warning: '%s' is not a directory, defaulting to current directory"),
						cob_optarg);
				} else {
					save_temps_dir = cobc_strdup (cob_optarg);
				}
			}
			break;

		case 'T':
			/* -T : Generate wide listing */
			cb_listing_wide = 1;
			/* fall through */
		case 't':
			/* -t : Generate listing */
			if (cb_listing_outputfile) {
				cobc_main_free (cb_listing_outputfile);
			}
			/* FIXME: add option to place each source in a single listing
			          by specifying a directory (similar to -P) */
			cb_listing_outputfile = cobc_main_strdup (cob_optarg);
			if (cb_list_datamap)
				cb_listing_symbols = 1;
			break;

		case '*':
			/* --tlines=nn : Lines per page */
			cb_lines_per_page = atoi (cob_optarg);
			if (cb_lines_per_page
			 && cb_lines_per_page < 20) {
				cobc_err_msg (_("warning: %d lines per listing page specified, using %d"),
						cb_lines_per_page, 20);
				cb_lines_per_page = 20;
			}
			break;

		case 'P':
			/* -P : Generate preproc listing */
			if (cob_optarg) {
				struct stat		st;
				if (cobc_list_dir) {
					cobc_free (cobc_list_dir);
					cobc_list_dir = NULL;
				}
				if (cobc_list_file) {
					cobc_free (cobc_list_file);
					cobc_list_file = NULL;
				}
				if (!stat (cob_optarg, &st) && S_ISDIR (st.st_mode)) {
					cobc_list_dir = cobc_strdup (cob_optarg);
				} else {
					cobc_list_file = cobc_strdup (cob_optarg);
				}
			}
			if (!cobc_gen_listing) {
				cobc_gen_listing = 1;
			}
			break;

		case 'X':
#ifndef COB_INTERNAL_XREF
			/* -Xref : Generate listing through 'cobxref' */
			cobc_gen_listing = 2;
			/* temporary: check if we run the testsuite and skip
			   the run if we don't have the internal xref */
			if (getenv ("COB_IS_RUNNING_IN_TESTMODE")) {
				cobc_early_exit (77);
			}
#else
			/* -Xref : Generate internal listing */
			cb_listing_xref = 1;
#endif
			break;

		case 'D':
			/* -D xx(=yy) : Define variables */
			if (strlen (cob_optarg) > 64U) {
				cobc_err_exit (COBC_INV_PAR, "-D");
			}
			if (!cb_strcasecmp (cob_optarg, "ebug")) {
				cobc_err_msg (_("warning: assuming '%s' is a DEFINE - did you intend to use -debug?"),
						cob_optarg);
			}
			p = cb_define_list_add (cb_define_list, cob_optarg);
			if (!p) {
				cobc_err_exit (COBC_INV_PAR, "-D");
			}
			cb_define_list = p;
			break;

		case 'I':
			/* -I <xx> : Include/copy directory */
			if (strlen (cob_optarg) > COB_SMALL_MAX) {
				cobc_err_exit (COBC_INV_PAR, "-I");
			}
			{
				struct stat		st;
				if (stat (cob_optarg, &st) != 0
				 || !(S_ISDIR (st.st_mode))) {
					break;
				}
			}
#ifdef	_MSC_VER
			remove_trailing_slash (cob_optarg);
			COBC_ADD_STR (cobc_include, " /I \"", cob_optarg, "\"");
#elif	defined (__WATCOMC__)
			COBC_ADD_STR (cobc_include, " -i\"", cob_optarg, "\"");
#else
			COBC_ADD_STR (cobc_include, " -I\"", cob_optarg, "\"");
#endif
			CB_TEXT_LIST_ADD (cb_include_list, cob_optarg);
			break;

		case 'L':
			/* -L <xx> : Directory for library search */
			if (strlen (cob_optarg) > COB_SMALL_MAX) {
				cobc_err_exit (COBC_INV_PAR, "-L");
			}
			{
				struct stat		st;
				if (stat (cob_optarg, &st) != 0
				 || !(S_ISDIR (st.st_mode))) {
					break;
				}
			}
#ifdef	_MSC_VER
			remove_trailing_slash (cob_optarg);
			COBC_ADD_STR (cobc_lib_paths, " /LIBPATH:\"", cob_optarg, "\"");
#else
			COBC_ADD_STR (cobc_lib_paths, " -L\"", cob_optarg, "\"");
#endif
			break;

		case 'l':
			/* -l <xx> : Add library to link phase */
			if (strlen (cob_optarg) > COB_SMALL_MAX) {
				cobc_err_exit (COBC_INV_PAR, "-l");
			}
#ifdef	_MSC_VER
			/* note: cb_strcasecmp because of likely compilation on FAT/NTFS */
			if (!cb_strcasecmp (file_extension (cob_optarg), "lib")) {
				COBC_ADD_STR (cobc_libs, " \"", cob_optarg, "\"");
			} else {
				COBC_ADD_STR (cobc_libs, " \"", cob_optarg, ".lib\"");
			}
#else
			COBC_ADD_STR (cobc_libs, " -l\"", cob_optarg, "\"");
#endif
			break;

		case 'e':
			/* -ext <xx> : Add an extension suffix */
			if (strlen (cob_optarg) > 15U) {
				cobc_err_exit (COBC_INV_PAR, "--ext");
			}
			snprintf (ext, (size_t)COB_MINI_MAX, ".%s", cob_optarg);
			CB_TEXT_LIST_ADD (cb_extension_list, ext);
			break;

		case 'K':
			/* -K <xx> : Define literal CALL to xx as static */
			if (strlen (cob_optarg) > 32U) {
				cobc_err_exit (COBC_INV_PAR, "-K");
			}
			CB_TEXT_LIST_ADD (cb_static_call_list, cob_optarg);
			break;

		case 'k':
			/* -k <xx> : Check for exit after CALL to xx  */
			/* This is to cater for legacy German DIN standard */
			/* Check after CALL if an exit program required */
			/* Not in --help as subject to change and highly specific */
			if (strlen (cob_optarg) > 32U) {
				cobc_err_exit (COBC_INV_PAR, "-k");
			}
			CB_TEXT_LIST_ADD (cb_early_exit_list, cob_optarg);
			break;

		case 1:
			/* -fstack-size=<xx> : Specify stack (perform) size */
			n = cobc_deciph_optarg (cob_optarg, 0);
			if (n < 16 || n > 512) {
				cobc_err_exit (COBC_INV_PAR, "-fstack-size");
			}
			cb_stack_size = n;
			break;

		case 3:
			/* -fsign=<ASCII/EBCDIC> : Specify display sign */
			if (!cb_strcasecmp (cob_optarg, "EBCDIC")) {
				cb_ebcdic_sign = 1;
			} else if (!cb_strcasecmp (cob_optarg, "ASCII")) {
				cb_ebcdic_sign = 0;
			} else {
				cobc_err_exit (COBC_INV_PAR, "-fsign");
			}
			break;

		case 4:
			/* -ffold-copy=<UPPER/LOWER> : COPY fold case */
			if (!cb_strcasecmp (cob_optarg, "UPPER")) {
				cb_fold_copy = COB_FOLD_UPPER;
			} else if (!cb_strcasecmp (cob_optarg, "LOWER")) {
				cb_fold_copy = COB_FOLD_LOWER;
			} else {
				cobc_err_exit (COBC_INV_PAR, "-ffold-copy");
			}
			break;

		case 5:
			/* -ffold-call=<UPPER/LOWER> : CALL/PROG-ID fold case */
			if (!cb_strcasecmp (cob_optarg, "UPPER")) {
				cb_fold_call = COB_FOLD_UPPER;
			} else if (!cb_strcasecmp (cob_optarg, "LOWER")) {
				cb_fold_call = COB_FOLD_LOWER;
			} else {
				cobc_err_exit (COBC_INV_PAR, "-ffold-call");
			}
			break;

		case 6:
			/* -fdefaultbyte=<xx> : Default initialization byte */
			n = cobc_deciph_optarg (cob_optarg, 1);
			if (n < 0 || n > 255) {
				cobc_err_exit (COBC_INV_PAR, "-fdefaultbyte");
			}
			cb_default_byte = n;
			break;

		case 7:
			/* -fmax-errors=<xx> : Maximum errors until abort */
			n = cobc_deciph_optarg (cob_optarg, 0);
			if (n < 0) {
				cobc_err_exit (COBC_INV_PAR, "-fmax-errors");
			}
			cb_max_errors = n;
			break;

		case 8:
			/* -fdump=<scope> : Add sections for dump code generation */
			cobc_def_dump_opts (cob_optarg, 1);
			break;

		case 13:
			/* -fno-dump=<scope> : Suppress sections in dump code generation */
			if (cob_optarg) {
				cobc_def_dump_opts (cob_optarg, 0);
			} else {
				cb_flag_dump = COB_DUMP_NONE;
			}
			break;

		case 9:
			/* -fcallfh=<func> : Function-name for EXTFH */
			cb_call_extfh = cobc_main_strdup (cob_optarg);
			break;

		case 10:
			/* -fintrinsics=<xx> : Intrinsic name or ALL */
			cobc_deciph_funcs (cob_optarg);
			break;

		case 11:
			/* -fsqlschema=<name> : Database schema name for XFD */
			cb_sqldb_schema = cobc_main_strdup (cob_optarg);
			cb_flag_sql_xfd = 1;
			if (cob_schema_dir != NULL) {
				char	temp_buff[COB_MEDIUM_BUFF];
				strcpy(temp_buff,cob_schema_dir);
				cobc_main_free ((void*)cob_schema_dir);
				cob_schema_dir = cobc_main_malloc (strlen(temp_buff) + strlen(cb_sqldb_schema) + 8);
				sprintf((void*)cob_schema_dir,"%s%s%s",temp_buff,SLASH_STR,cb_sqldb_schema);
			} else {
				cob_schema_dir = cobc_main_malloc (strlen(COB_SCHEMA_DIR) + strlen(cb_sqldb_schema) + 8);
				sprintf((void*)cob_schema_dir,"%s%s%s",COB_SCHEMA_DIR,SLASH_STR,cb_sqldb_schema);
			}
#ifdef _WIN32	/* simon: come back to this later... */
			_mkdir (cob_schema_dir);
			_chmod (cob_schema_dir, _S_IREAD | _S_IWRITE);
#else
			mkdir (cob_schema_dir, 0777);
			chmod (cob_schema_dir, 0777);
#endif
			break;

		case 12:
			/* -ffile-format=<name> : Default file format */
			if (cb_strcasecmp (cob_optarg, "mf") == 0) {
				cb_mf_files = 1;
			} else
			if (cb_strcasecmp (cob_optarg, "gc") == 0) {
				cb_mf_files = 0;
			} else {
				printf("Warning: -ffile-format = mf | gc\n");
			}
			break;

		case 'A':
			/* -A <xx> : Add options to C compile phase */
			COBC_ADD_STR (cobc_cflags, " ", cob_optarg, NULL);
			break;

		case 'Q':
			/* -Q <xx> : Add options to C link phase */
			COBC_ADD_STR (cobc_ldflags, " ", cob_optarg, NULL);
			break;

		case 'w':
			/* -w : Turn off all warnings (disables -Wall/-Wextra if passed later) */
#define	CB_WARNDEF(opt,name,doc)	cb_warn_opt_val[opt] = COBC_WARN_DISABLED;
#define	CB_ONWARNDEF(opt,name,doc)	cb_warn_opt_val[opt] = COBC_WARN_DISABLED;
#define	CB_NOWARNDEF(opt,name,doc)	cb_warn_opt_val[opt] = COBC_WARN_DISABLED;
#define	CB_ERRWARNDEF(opt,name,doc)	cb_warn_opt_val[opt] = COBC_WARN_ENABLED;
#include "warning.def"
#undef	CB_WARNDEF
#undef	CB_ONWARNDEF
#undef	CB_NOWARNDEF
#undef	CB_ERRWARNDEF
			break;

		case 'W':
			/* -Wall : Turn on most warnings */
#define	CB_WARNDEF(opt,name,doc)	cb_warn_opt_val[opt] = COBC_WARN_ENABLED;
#define	CB_ONWARNDEF(opt,name,doc)
#define	CB_NOWARNDEF(opt,name,doc)
#define	CB_ERRWARNDEF(opt,name,doc)	cb_warn_opt_val[opt] = COBC_WARN_AS_ERROR;
#include "warning.def"
#undef	CB_WARNDEF
#undef	CB_ONWARNDEF
#undef	CB_NOWARNDEF
#undef	CB_ERRWARNDEF
			break;

		case 'Y':
			/* -Wextra : Turn on every warning that is not dialect related */
#define	CB_WARNDEF(opt,name,doc)	cb_warn_opt_val[opt] = COBC_WARN_ENABLED;
#define	CB_ONWARNDEF(opt,name,doc)
#define	CB_NOWARNDEF(opt,name,doc)	cb_warn_opt_val[opt] = COBC_WARN_ENABLED;
#define	CB_ERRWARNDEF(opt,name,doc)	cb_warn_opt_val[opt] = COBC_WARN_AS_ERROR;
#include "warning.def"
#undef	CB_WARNDEF
#undef	CB_ONWARNDEF
#undef	CB_NOWARNDEF
#undef	CB_ERRWARNDEF
			break;

#if 0 /* TODO */
		case 'y':
			/* -Wunknown-option, -Wno-unknown-option: ignore with diagnostic */
			if (verbose_output) {
				cobc_err_msg (_("unknown warning option '%s'"),
					cob_optarg);
			}
			break;
#endif

		case 'Z':
			/* -Werror[=warning] : Treat all/single warnings as errors */
			if (cob_optarg) {
#define CB_CHECK_WARNING(opt,name)  \
				if (strcmp (cob_optarg, name) == 0) {	\
					cb_warn_opt_val[opt] = COBC_WARN_AS_ERROR;		\
				} else
#define	CB_WARNDEF(opt,name,doc)	CB_CHECK_WARNING(opt, name)
#define	CB_ONWARNDEF(opt,name,doc)	CB_CHECK_WARNING(opt, name)
#define	CB_NOWARNDEF(opt,name,doc)	CB_CHECK_WARNING(opt, name)
#define	CB_ERRWARNDEF(opt,name,doc)	CB_CHECK_WARNING(opt, name)
#include "warning.def"
#undef	CB_CHECK_WARNING
#undef	CB_WARNDEF
#undef	CB_ONWARNDEF
#undef	CB_NOWARNDEF
#undef	CB_ERRWARNDEF
				/* note: ends block from last CB_CHECK_WARNING */
				/* else */ if (verbose_output) {
					cobc_err_msg (_("unknown warning option '%s'"),
						cob_optarg);
				}
			} else {
				error_all_warnings = 1;
			}
			break;

		case 'z':
			/* -Wno-error[=warning] : Treat all/single warnings not as errors */
			if (cob_optarg) {
#define CB_CHECK_WARNING(opt,name)  \
				if (strcmp (cob_optarg, name) == 0	\
				 && cb_warn_opt_val[opt] == COBC_WARN_AS_ERROR) {	\
					cb_warn_opt_val[opt] = COBC_WARN_ENABLED;		\
				} else
#define	CB_WARNDEF(opt,name,doc)	CB_CHECK_WARNING(opt, name)
#define	CB_ONWARNDEF(opt,name,doc)	CB_CHECK_WARNING(opt, name)
#define	CB_NOWARNDEF(opt,name,doc)	CB_CHECK_WARNING(opt, name)
#define	CB_ERRWARNDEF(opt,name,doc)	CB_CHECK_WARNING(opt, name)
#include "warning.def"
#undef	CB_CHECK_WARNING
#undef	CB_WARNDEF
#undef	CB_ONWARNDEF
#undef	CB_NOWARNDEF
#undef	CB_ERRWARNDEF
				/* note: ends block from last CB_CHECK_WARNING */
				/* else */ if (verbose_output) {
					cobc_err_msg (_("unknown warning option '%s'"),
						cob_optarg);
				}
			} else {
				error_all_warnings = 0;
			}
			break;

		/* LCOV_EXCL_START */
		default:
			cobc_err_msg ("missing evaluation of command line option '%c'", c);	/* not translated as unlikely */
			COBC_ABORT ();
		/* LCOV_EXCL_STOP */

		}
	}

	/* Load reserved words from fixed word-list if specified */
	if (cb_reserved_words != NULL) {
		cb_load_words();
	}

	/* Exit for configuration errors resulting from -f<conf-tag>[=<value>] */
	if (conf_ret != 0) {
		cobc_early_exit (EXIT_FAILURE);
	}

	/* handling of list options */
	if (list_reserved) {
		/* includes register list */
		cb_list_reserved ();
	} else if (list_registers) {
		cb_list_registers ();
	}
	if (list_intrinsics) {
		cb_list_intrinsics ();
	}
	if (list_system_names) {
		cb_list_system_names ();
	}
	if (list_system_routines) {
		cb_list_system_routines ();
	}

	/* Exit if list options were specified */
	if (exit_option) {
		cobc_early_exit (EXIT_SUCCESS);
	}

	/* Exit on missing options */
#ifdef COB_INTERNAL_XREF
	if (cb_listing_xref && !cb_listing_outputfile) {
		cobc_err_exit (_("%s option requires a listing file"), "-Xref");
	}
#endif

	if (output_name && strcmp (output_name, COB_DASH) == 0) {
		cb_src_list_file = stdout;
		if (cb_compile_level != CB_LEVEL_PREPROCESS) {
			cobc_err_exit (_("output to stdout only valid for preprocess"));
		}
		cobc_main_free (output_name);
		cobc_main_free (output_name_buff);
	}

	/* Set relaxed syntax configuration options if requested */
	/* part 1: relaxed syntax compiler configuration option */
	if (cb_relaxed_syntax_checks) {
		if (cb_reference_out_of_declaratives > CB_WARNING) {
			cb_reference_out_of_declaratives = CB_WARNING;
		}
		if (cb_missing_statement > CB_WARNING) {
			cb_missing_statement = CB_WARNING;
		}
		/* FIXME - the warning was only raised if not relaxed */
		if (cb_warn_opt_val[(int)cb_warn_ignored_initial_val] != COBC_WARN_ENABLED_EXPL) {
			cb_warn_opt_val[(int)cb_warn_ignored_initial_val] = COBC_WARN_DISABLED;
		}
	}
#if 0 /* deactivated as -frelaxed-syntax-checks and other compiler configurations
		 are available at command line - maybe re-add with another name */
	/* 2: relaxed syntax group option from command line */
	if (cb_flag_relaxed_syntax_group) {
		cb_relaxed_syntax_checks = 1;
		cb_larger_redefines_ok = 1;
		cb_relax_level_hierarchy = 1;
		cb_top_level_occurs_clause = CB_OK;
	}
#endif

	{
		/* 3.x compat -Wconstant-expression also sets -Wconstant-numlit-expression */
		/* TODO: handle group warnings */
		const enum cb_warn_val detail_warn = cb_warn_opt_val[(int)cb_warn_constant_numlit_expr];
		if (detail_warn != COBC_WARN_DISABLED_EXPL
		 && detail_warn != COBC_WARN_ENABLED_EXPL) {
			const enum cb_warn_val group_warn = cb_warn_opt_val[(int)cb_warn_constant_expr];
			cb_warn_opt_val[(int)cb_warn_constant_numlit_expr] = group_warn;
		}
		/* set all explicit warning options to their later checked variants */
#define CB_CHECK_WARNING(opt)  \
		if (cb_warn_opt_val[opt] == COBC_WARN_ENABLED_EXPL) {	\
			cb_warn_opt_val[opt] = COBC_WARN_ENABLED;		\
		} else if (cb_warn_opt_val[opt] == COBC_WARN_DISABLED_EXPL) {	\
			cb_warn_opt_val[opt] = COBC_WARN_DISABLED;		\
		}
#define	CB_WARNDEF(opt,name,doc)	CB_CHECK_WARNING(opt)
#define	CB_ONWARNDEF(opt,name,doc)	CB_CHECK_WARNING(opt)
#define	CB_NOWARNDEF(opt,name,doc)	CB_CHECK_WARNING(opt)
#define	CB_ERRWARNDEF(opt,name,doc)	CB_CHECK_WARNING(opt)
#include "warning.def"
#undef	CB_CHECK_WARNING
#undef	CB_WARNDEF
#undef	CB_ONWARNDEF
#undef	CB_NOWARNDEF
#undef	CB_ERRWARNDEF
	}
	/* Set active warnings to errors, if requested */
	if (error_all_warnings) {
#define CB_CHECK_WARNING(opt)  \
		if (cb_warn_opt_val[opt] == COBC_WARN_ENABLED) {	\
			cb_warn_opt_val[opt] = COBC_WARN_AS_ERROR;		\
		}
#define	CB_WARNDEF(opt,name,doc)	CB_CHECK_WARNING(opt)
#define	CB_ONWARNDEF(opt,name,doc)	CB_CHECK_WARNING(opt)
#define	CB_NOWARNDEF(opt,name,doc)	CB_CHECK_WARNING(opt)
#define	CB_ERRWARNDEF(opt,name,doc)	CB_CHECK_WARNING(opt)
#include "warning.def"
#undef	CB_CHECK_WARNING
#undef	CB_WARNDEF
#undef	CB_ONWARNDEF
#undef	CB_NOWARNDEF
#undef	CB_ERRWARNDEF
	}

	if (fatal_errors_flag) {
		cb_max_errors = -1;
	}

	/* Set postponed options */
	if (copt != NULL) {
		COBC_ADD_STR (cobc_cflags, copt, NULL, NULL);
	}

	/* Set implied options */
	if (cb_title_statement <= CB_OBSOLETE
	&&  cb_listing_statements > CB_OBSOLETE) {
		cb_listing_statements = cb_title_statement;
	}
	if (!cb_flag_trunc) {
		cb_binary_truncate = 0;
		cb_pretty_display = 0;
	}
	if (cb_flag_traceall) {
		cb_flag_trace = 1;
		cb_flag_source_location = 1;
	}
	if (cb_flag_trace) {
		cb_flag_symbols = 1;
	}
	if (cb_flag_c_line_directives) {
		save_all_src = 1;
	}
#ifndef	_MSC_VER
	if (cb_source_debugging) {
		COBC_ADD_STR (cobc_cflags, " -g", NULL, NULL);
	}
#endif

	/* debug: Turn on all exception conditions */
	if (cobc_wants_debug) {
		for (i = (enum cob_exception_id)1; i < COB_EC_MAX; ++i) {
			CB_EXCEPTION_ENABLE (i) = 1;
		}
		if (verbose_output > 1) {
			fputs (_("all runtime checks are enabled"), stderr);
			fputc ('\n', stderr);
		}
	}

	/* If C debug, do not strip output */
	if (cb_source_debugging) {
		strip_output = 0;
	}

	/* set compile_level from output file if not set already */
	if (cb_compile_level == 0
	 && output_name != NULL) {
		set_compile_level_from_file_extension (output_name);
	}

	/* note: this is a "legacy" option, not a flag -
	   better use the two separate dialect flags */
	if (cb_mf_ibm_comp == 0) {		/* NO-IBMCOMP */
		cb_binary_size = CB_BINARY_SIZE_1__8;
		cb_synchronized_clause = CB_IGNORE;
	} else if (cb_mf_ibm_comp == 1) {	/* IBMCOMP */
		cb_binary_size = CB_BINARY_SIZE_2_4_8;
		cb_synchronized_clause = CB_OK;
		cb_align_record = 8;
		cb_align_opt = 1;
	}

	return cob_optind;
}

/* Restore the order in list of programs */
static void
restore_program_list_order (void)
{
	struct cb_program	*last;

	/* ensure that this function is only processed once
	   as we must call it from multiple places */
	if (cb_correct_program_order) {
		return;
	}
	cb_correct_program_order = 1;

	last = NULL;
	for (; current_program; current_program = current_program->next_program_ordered) {
		current_program->next_program_ordered = current_program->next_program;
		current_program->next_program = last;
		last = current_program;
	}
	current_program = last;
}

static void
process_env_copy_path (const char *p)
{
	char		*value;
	char		*token;
	struct stat	st;

	if (p == NULL || !*p || *p == ' ') {
		return;
	}

	/* Clone value for destructive strtok */
	value = cobc_strdup (p);

	/* Tokenize for path sep. */
	token = strtok (value, PATHSEP_STR);
	while (token) {
		const char* path = token;
		/* special case (MF-compat): empty evaluates to "." */
		if (*path == 0) {
			path = ".";
		}
		if (!stat (path, &st) && (S_ISDIR (st.st_mode))) {
			CB_TEXT_LIST_CHK (cb_include_list, path);
		}
		token = strtok (NULL, PATHSEP_STR);
	}

	cobc_free (value);
	return;
}

/* process setup for a single filename,
   returns a (struct filename *) if the file
   is to be processed, otherwise NULL */
static struct filename *
process_filename (const char *filename)
{
	const char	*extension;
	struct filename	*fn;
	struct filename	*ffn;
	char		*fbasename;
	char		*listptr;
	size_t		fsize;
	int		file_is_stdin;
#ifdef HAVE_8DOT3_FILENAMES
	char	*buffer;
#endif
#ifdef	__OS400__
	char	*full_path;
#endif

	if (strcmp (filename, COB_DASH) == 0) {
		if (cobc_seen_stdin == 0) {
			cobc_seen_stdin = 1;
			file_is_stdin = 1;
			filename = COB_DASH_NAME;
		} else {
			cobc_err_msg (_("only one stdin input allowed"));
			return NULL;
		}
	} else {
		file_is_stdin = 0;
	}

	fsize = strlen (filename);
	/* LCOV_EXCL_START */
	if (fsize > COB_NORMAL_MAX) {
		cobc_err_msg (_("invalid file name parameter (length > %d)"), COB_NORMAL_MAX);
		return NULL;
	}
	/* LCOV_EXCL_STOP */

#ifdef	__OS400__
	if (strchr (filename, '.') != NULL) {
#endif

	if (!file_is_stdin && access (filename, R_OK) != 0) {
		cobc_terminate (filename);
	}

#ifdef	__OS400__
	}
#endif

	fbasename = file_basename (filename, NULL);
	extension = file_extension (filename);
	 /* set source file for possible error message */
	cb_source_file = filename;
	/* note: cb_strcasecmp because of possible compilation on FAT/NTFS */
	if (cb_strcasecmp (extension, "lib")
	 && cb_strcasecmp (extension, "a")
	 && cb_strcasecmp (extension, COB_OBJECT_EXT)) {
		if (cobc_check_valid_name (fbasename, FILE_BASE_NAME)) {
			return NULL;
		}
	}
	fn = cobc_main_malloc (sizeof (struct filename));
	fn->need_preprocess = 1;
	fn->need_translate = 1;
	fn->need_assemble = 1;
	fn->file_is_stdin = file_is_stdin;
	fn->next = NULL;

	if (!file_list) {
		file_list = fn;
	} else {
		for (ffn = file_list; ffn->next; ffn = ffn->next)
			;
		ffn->next = fn;
	}

	fn->demangle_source = cb_encode_program_id (fbasename, 0, cb_fold_call);

	/* Check input file type */
	if (cb_strcasecmp (extension, "i") == 0) {
		/* Already preprocessed */
		fn->need_preprocess = 0;
	} else
	if (cb_strcasecmp (extension, "c") == 0
#if	defined(_WIN32)
	 || cb_strcasecmp (extension, "asm") == 0
#endif
	 || cb_strcasecmp (extension, "s") == 0) {
		/* Already compiled */
		fn->need_preprocess = 0;
		fn->need_translate = 0;
	} else
	if (
#if	defined(__OS400__)
	    extension[0] == 0
#else
		cb_strcasecmp (extension, COB_OBJECT_EXT) == 0
#if	defined(_WIN32)
	 || cb_strcasecmp (extension, "lib") == 0
#endif
#if	!defined(_WIN32) || defined(__MINGW32__) || defined(__MINGW64__)
	 || cb_strcasecmp (extension, "a") == 0
	 || cb_strcasecmp (extension, "so") == 0
	 || cb_strcasecmp (extension, "dylib") == 0
	 || cb_strcasecmp (extension, "sl") == 0
#endif
#endif
	) {
		/* Already assembled */
		fn->need_preprocess = 0;
		fn->need_translate = 0;
		fn->need_assemble = 0;
	}

	/* Set source filename */
	fn->source = cobc_main_strdup (filename);

	/* Set preprocess filename */
	if (!fn->need_preprocess) {
		fn->preprocess = cobc_main_strdup (fn->source);
	} else if (output_name && cb_compile_level == CB_LEVEL_PREPROCESS) {
		fn->preprocess = cobc_main_strdup (output_name);
	} else if (save_all_src || save_temps ||
		   cb_compile_level == CB_LEVEL_PREPROCESS) {
		fn->preprocess = cobc_main_stradd_dup (fbasename, ".i");
	} else {
		fn->preprocess = cobc_main_malloc (COB_FILE_MAX);
		cob_temp_name ((char *)fn->preprocess, ".cob");
	}

	/* Set translate filename */
	if (!fn->need_translate) {
		fn->translate = cobc_main_strdup (fn->source);
	} else if (output_name && cb_compile_level == CB_LEVEL_TRANSLATE) {
		fn->translate = cobc_main_strdup (output_name);
	} else if (save_all_src || save_temps || save_c_src ||
		   cb_compile_level == CB_LEVEL_TRANSLATE) {
		fn->translate = cobc_main_stradd_dup (fbasename, ".c");
	} else {
		fn->translate = cobc_main_malloc (COB_FILE_MAX);
		cob_temp_name ((char *)fn->translate, ".c");
	}
#ifdef	__OS400__
	/* adjustment of fn->translate, seems to need a full path
	   for later command line for cc; note - while it is unlikely that
	   cob_temp_name isn't starting with "/" it is still possible */
	if (fn->translate[0] != '/') {
		full_path = cobc_main_malloc (COB_LARGE_BUFF);
		getcwd (full_path, COB_LARGE_BUFF);
		strcat (full_path, "/");
		strcat (full_path, fn->translate);
		cobc_main_free (fn->translate);
		fn->translate = full_path;
	}
#endif
	fn->translate_len = strlen (fn->translate);

	/* Set storage filename */
	if (fn->need_translate) {
#ifndef HAVE_8DOT3_FILENAMES
		fn->trstorage = cobc_main_stradd_dup (fn->translate, ".h");
#else
		/* for 8.3 filenames use no ".c" prefix */
		buffer = cobc_strdup (fn->translate);
		*(buffer + strlen (buffer) - 1) = 'h';
		fn->trstorage = buffer;
#endif
	}

	/* Set object filename */
	if (!fn->need_assemble) {
		fn->object = cobc_main_strdup (fn->source);
	} else if (output_name && cb_compile_level == CB_LEVEL_ASSEMBLE) {
		fn->object = cobc_main_strdup (output_name);
	} else if (save_temps || cb_compile_level == CB_LEVEL_ASSEMBLE) {
		fn->object = cobc_main_stradd_dup (fbasename, "." COB_OBJECT_EXT);
	} else if (cb_compile_level != CB_LEVEL_MODULE) {
		/* note: CB_LEVEL_MODULE is compiled without an intermediate object file */
		fn->object = cobc_main_malloc (COB_FILE_MAX);
		cob_temp_name ((char *)fn->object, "." COB_OBJECT_EXT);
	}
	if (fn->object) {
		fn->object_len = strlen (fn->object);
		cobc_objects_len += fn->object_len + 8U;
	} else {
		fn->object_len = 0;
	}

	/* Set listing filename */
	if (cobc_gen_listing == 1) {
		if (cobc_list_file) {
			fn->listing_file = cobc_list_file;
		} else if (cobc_list_dir) {
			fsize = strlen (cobc_list_dir) + strlen (fbasename) + 8U;
			listptr = cobc_main_malloc (fsize);
			snprintf (listptr, fsize, "%s%c%s.lst",
				  cobc_list_dir, SLASH_CHAR, fbasename);
			fn->listing_file = listptr;
		} else {
			fn->listing_file = cobc_main_stradd_dup (fbasename, ".lst");
		}
#ifndef COB_INTERNAL_XREF
	/* LCOV_EXCL_START */
	} else if (cobc_gen_listing == 2) {
		fn->listing_file = cobc_main_stradd_dup (fbasename, ".xrf");
	/* LCOV_EXCL_STOP */
#endif
	}

	cob_incr_temp_iteration();
	return fn;
}

#ifdef _MSC_VER
/*
 * search_pattern can contain one or more search strings separated by '|'
 * search_patterns must have a final '|'
 */
static int
line_contains (char* line_start, char* line_end, char* search_patterns)
{
	int pattern_end, pattern_start, pattern_length, full_length;
	char* line_pos;

	if (search_patterns == NULL) return 0;

	pattern_start = 0;
	full_length = (int)strlen (search_patterns) - 1;
	for (pattern_end = 0; pattern_end < (int)strlen (search_patterns); pattern_end++) {
		if (search_patterns[pattern_end] == PATTERN_DELIM) {
			pattern_length = pattern_end - pattern_start;
			for (line_pos = line_start; line_pos + pattern_length <= line_end; line_pos++) {
				/* Find matching substring */
				if (memcmp (line_pos, search_patterns + pattern_start, pattern_length) == 0) {
					/* Exit if all patterns found, skip to next pattern otherwise */
					if (pattern_start + pattern_length == full_length) {
						return 1;
					} else {
						break;
					}
				}
			}
			pattern_start = pattern_end + 1;
		}
	}

	return 0;
}
#endif

static char *
resolve_name_from_cobc (const char *cobc_path)
{
	char *	cobcrun_path_malloced = NULL;

	const char	*cobc_real_name;
	size_t	cobc_name_length;
	size_t	cobc_path_length;
	int		i;

	if (cobc_path == NULL) {
		return NULL;
	}

	cobc_real_name = file_basename (cobc_path, NULL);
	cobc_name_length = strlen (cobc_real_name);
	cobc_path_length = (int)strlen (cobc_path);
	/* note, we cannot subtract strlen (COB_EXE_EXT)
	   as we may be called with/without it */
	for (i = cobc_path_length - cobc_name_length; i >= 0; i--) {
		if (!strncasecmp (cobc_real_name, cobc_path + i, cobc_name_length)) {
			const size_t cobcrun_name_length = strlen (COBCRUN_NAME);
			size_t length = cobc_path_length - cobc_name_length + cobcrun_name_length + 1;
			cobcrun_path_malloced = cobc_malloc (length);
			memcpy (cobcrun_path_malloced, cobc_path, i);
			memcpy (cobcrun_path_malloced + i, COBCRUN_NAME, cobcrun_name_length);
			length = cobc_path_length - i - cobc_name_length + 1;
			memcpy (cobcrun_path_malloced + i + cobcrun_name_length, cobc_path + i + cobc_name_length, length);
			break;
		}
	}
	return cobcrun_path_malloced;
}

static COB_INLINE COB_A_INLINE void
output_return (const int status)
{
	if (verbose_output) {
		fputs (_("return status:"), stderr);
		fprintf (stderr, "\t%d\n", status);
		fflush (stderr);
	}
}

/* do system call, with handling verbose options and return */
static int
call_system (const char *command)
{
	int status;

	if (verbose_output) {
		cobc_cmd_print (command);
	}
	if (verbose_output < 0) {
		return 0;
	}

	/* flush so that messages appear in order presented */
	fflush (stdout);
	fflush (stderr);

	status = system (command);

#ifdef	WIFSIGNALED
	if (WIFSIGNALED (status)) {
		int signal_value = WTERMSIG (status);
#if 0
		if (signal == SIGINT || signal == SIGQUIT) {
			save_temps = 0;
			cobc_clean_up (1);
			cob_raise (signal);
		}
#endif
		cobc_err_msg (_("external process \"%s\" ended with signal %s (%d)"),
			command, cob_get_sig_name (signal_value), signal_value);
	}
#endif
#ifdef WEXITSTATUS
	if (WIFEXITED (status)) {
		status = WEXITSTATUS (status);
	}
#endif

	output_return (status);
	return status;
}


/** -j run job after build */
static int
process_run (const char *name)
{
	size_t		curr_size;
	const char	*buffer;

	if (cb_compile_level < CB_LEVEL_MODULE) {
		fputs (_("nothing for -j to run"), stderr);
		fflush (stderr);
		return 0;
	}

	if (output_name) {
		name = output_name;
		/* ensure enough space (output name) */
		cobc_chk_buff_size (strlen (output_name) + 18);
	}

	if (cb_compile_level == CB_LEVEL_MODULE
	 || cb_compile_level == CB_LEVEL_LIBRARY) {
		const char *cobcrun_path = getenv ("COBCRUN");
		char* cobcrun_path_malloced = NULL;

		if (!cobcrun_path || !cobcrun_path[0]) {
			char *cobc_path = getenv ("COBC");
			char *cobc_path_malloced = NULL;

			if (!cobc_path || !cobc_path[0]) {
#if	defined(HAVE_CANONICALIZE_FILE_NAME)
				/* Malloced path or NULL */
				cobc_path_malloced = canonicalize_file_name (cb_saveargv[0]);
#elif	defined(HAVE_REALPATH)
				{
					char *s = cobc_malloc ((size_t)COB_NORMAL_BUFF);
					if (realpath (cb_saveargv[0], s) != NULL) {
						cobc_path_malloced = cob_strdup (s);
					}
					cobc_free (s);
				}
#elif defined (_WIN32)
				/* Malloced path or NULL */
				cobc_path_malloced = _fullpath (NULL, cb_saveargv[0], 1);
#endif
				if (cobc_path_malloced) {
					cobc_path = cobc_path_malloced;
				} else {
					cobc_path = NULL;
				}
			} else if (verbose_output > 1) {
				fprintf (stderr, _("%s is resolved by environment as: %s"),
					"COBC", cobc_path);
				fputc ('\n', stderr);
			}
			cobcrun_path_malloced = resolve_name_from_cobc (cobc_path);
			if (cobcrun_path_malloced) {
				cobcrun_path = cobcrun_path_malloced;
			} else {
				cobcrun_path = COBCRUN_NAME COB_EXE_EXT;
			}
			if (cobc_path_malloced) {
				cobc_free (cobc_path_malloced);
			}
		} else if (verbose_output > 1) {
			fprintf (stderr, _("%s is resolved by environment as: %s"),
				"COBCRUN", cobcrun_path);
			fputc ('\n', stderr);
		}
		curr_size = snprintf (cobc_buffer, cobc_buffer_size, "%s %s",
			cobcrun_path, name);
		if (cobcrun_path_malloced) {
			cobc_free (cobcrun_path_malloced);
		}
		/* strip period + COB_MODULE_EXT if specified */
		if (output_name && curr_size < cobc_buffer_size) {
			buffer = file_extension (output_name);
			if (!cb_strcasecmp (buffer, COB_MODULE_EXT)) {
				*(cobc_buffer + curr_size - strlen (buffer) - 1) = 0;
			}
		}
	} else {  /* executable */
		/* only add COB_EXE_EXT if it is not specified */
		const char *exe_ext = COB_EXE_EXT;
		exe_ext++; /* drop the "." */
		buffer = file_extension (name);
		/* only prefix with ./ if there is no directory portion in name */
		if (strchr (name, SLASH_CHAR) == NULL) {
			if (COB_EXE_EXT[0] && cb_strcasecmp (buffer, exe_ext)) {
				curr_size = snprintf (cobc_buffer, cobc_buffer_size, ".%c%s%s",
					SLASH_CHAR, name, COB_EXE_EXT);
			} else {
				curr_size = snprintf (cobc_buffer, cobc_buffer_size, ".%c%s",
					SLASH_CHAR, name);
			}
		} else {
			if (COB_EXE_EXT[0] && cb_strcasecmp (buffer, exe_ext)) {
				curr_size = snprintf (cobc_buffer, cobc_buffer_size, "%s%s",
					name, COB_EXE_EXT);
			} else {
				curr_size = snprintf (cobc_buffer, cobc_buffer_size, "%s",
					name);
			}
		}
	}
#ifdef	_WIN32 /* "fix" given output name */
	if (output_name) {
		char		*ptr;
		for (ptr = cobc_buffer; *ptr; ptr++) {
			if (*ptr == '/') *ptr = '\\';
		}
	}
#endif
	if (cobc_run_args) {
		cobc_chk_buff_size (curr_size + 1 + strlen (cobc_run_args));
		strncat (cobc_buffer, " ", cobc_buffer_size);
		strncat (cobc_buffer, cobc_run_args, cobc_buffer_size);
	}
	return call_system (cobc_buffer);
}

#ifdef	__OS400__
static int
process (char *cmd)
{
	char	*buffptr;
	char	*name = NULL;
	char	*objname = NULL;
	char	*cobjname = NULL;
	char	*token;
	char	*incl[100];
	char	*defs[100];
	char	*objs[100];
	char	*libs[100];
	char	*optc[100];
	char	*optl[100];
	int	nincl = 0;
	int	ndefs = 0;
	int	nobjs = 0;
	int	nlibs = 0;
	int	noptc = 0;
	int	noptl = 0;
	int	comp_only = 0;
	int	shared = 0;
	int	optimize = 0;
	int	i;
	int	len;
	int	ret;

	if (verbose_output) {
		cobc_cmd_print (cmd);
	}
	token = strtok (cmd, " ");
	if (token != NULL) {
		/* Skip C compiler */
		token = strtok (NULL, " ");
	}
	for (; token; token = strtok (NULL, " ")) {
		if (*token != '-') {
			len = strlen (token);
			if (*token == '"') {
				len -= 2;
				++token;
				token[len] = 0;
			}
			if (token[len-2] == '.' && token[len - 1] == 'c') {
				/* C source */
				name = token;
				continue;
			}
			/* Assuming module */
			objs[nobjs++] = token;
			continue;
		}
		++token;
		switch (*token) {
		case 'c':
			comp_only = 1;
			break;
		case 'I':
			++token;
			if (*token == 0) {
				token = strtok (NULL, " ");
			}
			if (*token == '"') {
				++token;
				token[strlen (token) - 1] = 0;
			}
			incl[nincl++] = token;
			break;
		case 'D':
			++token;
			if (*token == 0) {
				token = strtok (NULL, " ");
			}
			if (*token == '"') {
				++token;
				token[strlen (token) - 1] = 0;
			}
			defs[ndefs++] = token;
			break;
		case 'A':
			++token;
			optc[noptc++] = token;
			break;
		case 'Q':
			++token;
			optl[noptl++] = token;
			break;
		case 'o':
			++token;
			if (*token == 0) {
				token = strtok (NULL, " ");
			}
			if (*token == '"') {
				++token;
				token[strlen (token) - 1] = 0;
			}
			objname = token;
			break;
		case 'l':
			++token;
			if (*token == 0) {
				token = strtok (NULL, " ");
			}
			libs[nlibs++] = token;
			break;
		case 'G':
			shared = 1;
			break;
		case 'g':
			/* CHECKME: is this still reached? */
			break;
		case 'O':
			optimize = 1;
			break;
		default:
			/* rare issue only on OS400 where translation
			   may not even work - untranslated */
			cobc_err_msg ("unknown option ignored:\t%s",
				 token - 1);
		}
	}

	buffptr = cobc_malloc (COB_LARGE_BUFF);
	if (name != NULL) {
		/* Requires compilation */
		if (objname == NULL) {
			cobjname = file_basename (name, NULL);
		} else {
			cobjname = objname;
		}
		sprintf (buffptr, "CRTCMOD MODULE(%s) SRCSTMF('%s') ",
			cobjname, name);
		if (nincl > 0) {
			strcat (buffptr, "INCDIR(");
			for (i = 0; i < nincl; ++i) {
				if (i != 0) {
					strcat (buffptr, " ");
				}
				strcat (buffptr, "'");
				strcat (buffptr, incl[i]);
				strcat (buffptr, "' ");
			}
			strcat (buffptr, ") ");
		}
		if (ndefs > 0) {
			strcat (buffptr, "DEFINE(");
			for (i = 0; i < ndefs; ++i) {
				if (i != 0) {
					strcat (buffptr, " ");
				}
				strcat (buffptr, "'");
				strcat (buffptr, defs[i]);
				strcat (buffptr, "' ");
			}
			strcat (buffptr, ") ");
		}
		strcat (buffptr, "SYSIFCOPT(*IFSIO)");
		for (i = 0; i < noptc; ++i) {
			strcat (buffptr, " ");
			strcat (buffptr, optc[i]);
		}
		if (optimize) {
			strcat (buffptr, " OPTIMIZE(40)");
		}
		if (cb_source_debugging) {
			strcat (buffptr, " DBGVIEW(*ALL)");
		}
		if (cobc_gen_listing) {
			strcat (buffptr, " OUTPUT(*PRINT)");
		}
		ret = call_system (buffptr);
		if (comp_only || ret != 0) {
			cobc_free (buffptr);
			return ret;
		}
	}
	if (objname == NULL) {
		if (name != NULL) {
			objname = cobjname;
		} else if (nobjs > 0) {
			objname = objs[0];
		} else {
			objname = (char *)"AOUT";
		}
	}
	if (shared) {
		sprintf (buffptr, "CRTSRVPGM SRVPGM(%s) MODULE(", objname);
	} else {
		sprintf (buffptr, "CRTPGM PGM(%s) MODULE(", objname);
	}
	if (name != NULL) {
		strcat (buffptr, cobjname);
	}
	for (i = 0; i < nobjs; ++i) {
		if (i != 0 || name != NULL) {
			strcat (buffptr, " ");
		}
		strcat (buffptr, objs[i]);
	}
	strcat (buffptr, ")");
	if (nlibs > 0) {
		strcat (buffptr, " BNDSRVPGM(");
		for (i = 0; i < nlibs; ++i) {
			if (i != 0) {
				strcat (buffptr, " ");
			}
			strcat (buffptr, libs[i]);
		}
		strcat (buffptr, ")");
	}
	for (i = 0; i < noptl; ++i) {
		strcat (buffptr, " ");
		strcat (buffptr, optl[i]);
	}
	if (shared) {
		strcat (buffptr, " EXPORT(*ALL)");
	}
	ret = call_system (buffptr);
	cobc_free (buffptr);
	return ret;
}

#elif defined(_MSC_VER)
#ifndef HAVE_POPEN
#error HAVE_POPEN is missing in config.h
#endif
static int
process (const char *cmd)
{
	int ret = call_system (cmd);
	return !!ret;
}

static int
process_filtered (const char *cmd, struct filename *fn)
{
	FILE* pipe;
	char* read_buffer;
	char *line_start, *line_end;
	char* search_pattern, *search_pattern2 = NULL;
	char* output_name_temp;
	int i;
	int ret;

	if (verbose_output) {
		cobc_cmd_print (cmd);
	}
	if (verbose_output < 0) {
		return 0;
	}

	/* Open pipe to catch output of cl.exe */
	pipe = popen (cmd, "r");

	if (!pipe) {
		return 1; /* checkme */
	}

	/* building search_patterns */
	if (output_name) {
		if (cobc_flag_main) {
			output_name_temp = file_basename (output_name, COB_EXE_EXT);
		} else if (cb_compile_level == CB_LEVEL_ASSEMBLE) {
			output_name_temp = file_basename (output_name, "." COB_OBJECT_EXT);
		} else {
			output_name_temp = file_basename (output_name, "." COB_MODULE_EXT);
		}
	} else {
		/* demangle_source is encoded and cannot be used
		   -> set to file.something and strip at period */
		output_name_temp = file_basename (cobc_strdup (fn->source), NULL);
	}

	/* check for last path separator as we only need the file name */
	for (i = fn->translate_len; i > 0; i--) {
		if (fn->translate[i - 1] == '\\' || fn->translate[i - 1] == '/') break;
	}

	search_pattern = (char*)cobc_malloc ((fn->translate_len - i + 2) + 1);
	sprintf (search_pattern, "%s\n%c", fn->translate + i, PATTERN_DELIM);
	if (cb_compile_level > CB_LEVEL_ASSEMBLE) {
		search_pattern2 = (char*)cobc_malloc (2 * (strlen (output_name_temp) + 5) + 1);
		sprintf (search_pattern2, "%s.lib%c%s.exp%c", output_name_temp, PATTERN_DELIM,
			output_name_temp, PATTERN_DELIM);
	}

	/* prepare buffer and read from pipe */
	read_buffer = (char*) cobc_malloc (COB_FILE_BUFF);
	line_start = fgets (read_buffer, COB_FILE_BUFF - 1, pipe);

	while (line_start != NULL) {
		/* read one line from buffer, returning line end position */
		line_end = line_start + strlen (line_start);

		/* if non of the patterns was found, print line */
		if (line_start == line_end
			|| (!line_contains (line_start, line_end, search_pattern)
				&& !line_contains (line_start, line_end, search_pattern2)))
		{
			fprintf (stdout, "%*s", (int)(line_end - line_start + 2), line_start);
		}
		line_start = fgets (read_buffer, COB_FILE_BUFF - 1, pipe);
	}
	fflush (stdout);

	cobc_free (read_buffer);
	cobc_free (search_pattern);
	if (search_pattern2) {
		cobc_free (search_pattern2);
	}

	/* close pipe and get return code of cl.exe */
	ret = !!_pclose (pipe);


	output_return (ret);
	return ret;
}

#else
static int
process (const char *cmd)
{
	char	*p;
	char	*buffptr;
	size_t	clen;
	int	ret;

	if (strchr (cmd, '$') == NULL) {
		buffptr = (char *)cmd;
	} else {
		clen = strlen (cmd) + 64U;
		clen = clen + 6U;
		buffptr = (char *)cobc_malloc (clen);
		p = buffptr;
		/* Quote '$' */
		for (; *cmd; ++cmd) {
			if (*cmd == '$') {
				p += sprintf (p, "\\$");
			} else {
				*p++ = *cmd;
			}
		}
		*p = 0;
	}

	ret = call_system (buffptr);

	if (buffptr != cmd) {
		cobc_free (buffptr);
	}

	return !!ret;
}
#endif

static COB_INLINE COB_A_INLINE void
force_new_page_for_next_line (void)
{
	cb_listing_linecount = cb_lines_per_page;
}

/* Preprocess source */

static int
preprocess (struct filename *fn)
{
	const char		*sourcename;
	int			save_source_format, save_fold_copy, save_fold_call;
#ifndef COB_INTERNAL_XREF
#ifdef	_WIN32
	const char *envname = "%PATH%";
#else
	const char *envname = "$PATH";
#endif
	int			ret;
#endif

	if (output_name || cb_compile_level > CB_LEVEL_PREPROCESS) {
		if (cb_unix_lf) {
			ppout = fopen(fn->preprocess, "wb");
		} else {
			ppout = fopen(fn->preprocess, "w");
		}
		if (!ppout) {
			cobc_terminate (fn->preprocess);
		}
	} else {
		ppout = stdout;
	}

	if (fn->file_is_stdin) {
		sourcename = COB_DASH;
	} else {
		sourcename = fn->source;
	}
	if (ppopen (sourcename, NULL) != 0) {
		cobc_terminate (sourcename);
	}

	if (verbose_output) {
		fputs (_("preprocessing:"), stderr);
		fprintf (stderr, "\t%s -> %s\n",
			 sourcename, fn->preprocess);
		fflush (stderr);
	}

	if (cobc_gen_listing && !cobc_list_file) {
		if (cb_unix_lf) {
			cb_listing_file = fopen (fn->listing_file, "wb");
		} else {
			cb_listing_file = fopen (fn->listing_file, "w");
		}
		if (!cb_listing_file) {
			cobc_terminate (fn->listing_file);
		}
	}

	/* Reset pplex/ppparse variables */
	plex_clear_vars ();
	ppparse_clear_vars (cb_define_list);

	/* Save default flags in case program directives change them */
	save_source_format = cb_source_format;
	save_fold_copy = cb_fold_copy;
	save_fold_call = cb_fold_call;

	/* Preprocess */
	ppparse ();

	/* Restore default flags */
	cb_source_format = save_source_format;
	cb_fold_copy = save_fold_copy;
	cb_fold_call = save_fold_call;

	if (ppin) {
		fclose (ppin);
		ppin = NULL;
	}

	if (ppout) {
		if (fclose (ppout) != 0) {
			cobc_terminate (fn->preprocess);
		}
		ppout = NULL;
	}

	/* Release flex buffers - After file close */
	plex_call_destroy ();

	if (cobc_gen_listing && !cobc_list_file) {
		if (fclose (cb_listing_file) != 0) {
			cobc_terminate (fn->listing_file);
		}
#ifndef COB_INTERNAL_XREF
		/* LCOV_EXCL_START */
		/* external cross-reference with cobxref */
		if (cobc_gen_listing == 2) {
			if (cb_src_list_file) {
				fclose (cb_src_list_file);
			}

			snprintf (cobc_buffer, cobc_buffer_size,
				 "cobxref %s -R", fn->listing_file);
			cobc_buffer[cobc_buffer_size] = 0;
			ret = call_system (cobc_buffer);
			if (ret) {
				fputs (_("'cobxref' execution unsuccessful"),
					stderr);
				putc ('\n', stderr);
				fprintf (stderr, _("check that 'cobxref' is in %s"), envname);
				putc ('\n', stderr);
				fputs (_("no listing produced"),
					stderr);
				putc ('\n', stderr);
				fflush (stderr);
			}
			if (cb_listing_outputfile && verbose_output >= 0) {
				if (strcmp (cb_listing_outputfile, COB_DASH) == 0) {
					cb_src_list_file = stdout;
				} else {
					if (cb_unix_lf) {
						cb_src_list_file = fopen (cb_listing_outputfile, "ab");
					} else {
						cb_src_list_file = fopen (cb_listing_outputfile, "a");
					}
					if (!cb_src_list_file) {
						cobc_terminate (cb_listing_outputfile);
					}
				}
				cb_listing_eject = 1;
				force_new_page_for_next_line ();
			}
			unlink (fn->listing_file);
		}
		/* LCOV_EXCL_STOP */
#endif
		cb_listing_file = NULL;
	}

	output_return (errorcount);
	return !!errorcount;
}

/* Routines to generate program listing */


static void
set_listing_header_code (void)
{
	strcpy (cb_listing_header, "LINE    ");
	if (cb_listing_file_struct->source_format != CB_FORMAT_FREE) {
		strcat (cb_listing_header,
			"PG/LN  A...B..............................."
			".............................");
		if (cb_listing_wide) {
			if (cb_listing_file_struct->source_format == CB_FORMAT_FIXED
			    && cb_text_column == 72 && cb_indicator_column == 7) {
				strcat (cb_listing_header, "SEQUENCE");
			} else {
				strcat (cb_listing_header,
					"........................................");
			}
		}
	} else {
		if (cb_listing_wide) {
			strcat (cb_listing_header,
				"................................");
		}
		strcat (cb_listing_header,
			".....................SOURCE..................."
			"..........................");
		if (cb_listing_wide) {
			strcat (cb_listing_header, "........");
		}
	}
}

static void
set_listing_header_symbols (void)
{
	if (cb_list_datamap)
		strcpy (cb_listing_header,
			"OFFSET  SIZE  LVL  NAME                           PICTURE");
	else
		strcpy (cb_listing_header,
			"SIZE  TYPE           LVL  NAME                           PICTURE");
}

#ifdef COB_INTERNAL_XREF
/* listing header for internal xref */
static void
set_listing_header_xref (const enum xref_type type)
{
	if (!cb_listing_with_header) {
		return;
	}
	if (type == XREF_FUNCTION) {
		strcpy (cb_listing_header, "FUNCTION");
	} else if (type == XREF_LABEL) {
		strcpy (cb_listing_header, "LABEL   ");
	} else {
		strcpy (cb_listing_header, "NAME    ");
	}
	if (type == XREF_FUNCTION) {
		strcat (cb_listing_header,
			"                       TYPE                   ");
	} else {
		strcat (cb_listing_header,
			"                       DEFINED                ");
	}
	if (cb_listing_wide) {
		strcat (cb_listing_header, "                    ");
	}
	strcat (cb_listing_header, "REFERENCES");
}
#endif

/* listing header empty */
static void
set_listing_header_none (void)
{
	cb_listing_header[0] = 0;
}

/* standard title for listing
   (TODO: option to set by directive and/or command line option) */
static void
set_standard_title (void)
{
	char		version[30];
	snprintf (version, sizeof (version), "%s.%d", PACKAGE_VERSION, PATCH_LEVEL);
	snprintf (cb_listing_title, 80, "%s %s",
		PACKAGE_NAME,
		version);
}

/* print header */
static void
print_program_header (void)
{
	const char	*format_str;

	cb_listing_linecount = 1;

	/* header for print listing (with page breaks) */
	if (cb_lines_per_page != 0) {
		if (cb_listing_eject) {
			fputs ("\f", cb_src_list_file);
		} else {
			cb_listing_eject = 1;
		}
		if (!cb_listing_with_header) {
			fputc ('\n', cb_src_list_file);
			return;
		}
		if (cb_listing_wide) {
			format_str = "%-23.23s %-61.61s %s  Page %04d\n";
		} else {
			format_str = "%-23.23s %-20.20s %s  Page %04d\n";
		}
		fprintf (cb_src_list_file,
			 format_str,
			 cb_listing_title,
			 cb_listing_filename,
			 cb_listing_date,
			 ++cb_listing_page);

	/* header for listing without page breaks: --tlines=0 */
	} else {
		if (!cb_listing_with_header) {
			fputc ('\n', cb_src_list_file);
			return;
		}

		if (cb_listing_page == 0) {
			cb_listing_page = 1;
			if (cb_listing_wide) {
				format_str = "%-28.28s %-66.66s %s\n";
			} else {
				format_str = "%-28.28s %-26.26s %s\n";
			}
			fprintf (cb_src_list_file,
				 format_str,
				 cb_listing_title,
				 cb_listing_filename,
				 cb_listing_date);
		}
	}
	fputc ('\n', cb_src_list_file);

	/* print second header if set */
	if (cb_listing_header[0]) {
		print_program_data (cb_listing_header);
		print_program_data ("");
	}
}

static void
print_program_data (const char *data)
{
	/* no check for header if page break is disabled and not forced */
	if (cb_lines_per_page != 0 || cb_listing_linecount == 0) {
		/* increase listing line number and print header if necessary */
		if (++cb_listing_linecount >= cb_lines_per_page) {
			/* empty string - don't print anything */
			if (!data[0]) {
				return;
			}
			print_program_header ();
		}
	}

	/* print data + newline */
	fprintf (cb_src_list_file, "%s\n", data);
}

static char *
check_filler_name (char *name)
{
	if (strlen (name) >= 6 && memcmp (name, "FILLER", 6) == 0) {
		name = (char *)"FILLER";
	}
	return name;
}

static int
set_picture (struct cb_field *field, char *picture, size_t picture_len)
{
	size_t usage_len;
	char picture_usage[CB_LIST_PICSIZE];

	memset (picture, 0, CB_LIST_PICSIZE);

	/* check for external definition first */
	if (field->external_definition) {
		if (field->external_definition == cb_error_node) {
			strcpy (picture, "INVALID");
		} else {
			const char *name = CB_FIELD (field->external_definition)->name;
			strncpy (picture, name, picture_len);
			if (strlen (name) > picture_len - 1) {
				strcpy (picture + picture_len - 3, "...");
			}
		}
		return 1;
	}

	/* Check non-picture information next */
	switch (field->usage) {
	case CB_USAGE_INDEX:
	case CB_USAGE_LENGTH:
	case CB_USAGE_OBJECT:
	case CB_USAGE_LONG_DOUBLE:
	case CB_USAGE_FP_BIN32:
	case CB_USAGE_FP_BIN64:
	case CB_USAGE_FP_BIN128:
	case CB_USAGE_FP_DEC64:
	case CB_USAGE_FP_DEC128:
	case CB_USAGE_SIGNED_CHAR:
	case CB_USAGE_SIGNED_SHORT:
	case CB_USAGE_SIGNED_INT:
	case CB_USAGE_SIGNED_LONG:
	case CB_USAGE_UNSIGNED_CHAR:
	case CB_USAGE_UNSIGNED_SHORT:
	case CB_USAGE_UNSIGNED_INT:
	case CB_USAGE_UNSIGNED_LONG:
	case CB_USAGE_CONTROL:
		return 0;
	case CB_USAGE_POINTER:
	case CB_USAGE_PROGRAM_POINTER:
		if (cb_list_datamap) {
			strcpy (picture, "POINTER");
			return 1;
		}
		return 0;
	default:
		break;
	}

	/* check for invalid picture next */
	if (field->pic && !field->pic->orig) {
		strcpy (picture, "INVALID");
		return 1;
	}

	/* Get usage for this picture */
	strcpy (picture_usage, cb_get_usage_string (field->usage));
	usage_len = strlen (picture_usage);

	/* set picture for the rest */
	if (field->usage == CB_USAGE_BINARY
	 || field->usage == CB_USAGE_FLOAT
	 || field->usage == CB_USAGE_DOUBLE
	 || field->usage == CB_USAGE_PACKED
	 || field->usage == CB_USAGE_COMP_5
	 || field->usage == CB_USAGE_COMP_6
	 || field->usage == CB_USAGE_COMP_X
	 || field->usage == CB_USAGE_COMP_N) {
		if (field->pic) {
			strncpy (picture, field->pic->orig, picture_len - 1 - usage_len);
			picture[CB_LIST_PICSIZE - 1] = 0;
			strcat (picture, " ");
		}
		if (cb_list_datamap
		 && field->usage == CB_USAGE_COMP_X) {
			if (field->size == 1)
				strcpy(picture,"X    COMP-X");
			else
				sprintf(picture,"X(%d) COMP-X",field->size);
			return 1;
		}
	} else if (field->flag_any_numeric) {
		strncpy (picture, "9 ANY NUMERIC", 14);
		return 1;
	} else if (field->flag_any_length) {
		strncpy (picture, "X ANY LENGTH", 13);
		return 1;
	} else {
		if (!field->pic) {
			return 0;
		}
		strncpy (picture, field->pic->orig, picture_len - 1);
		return 1;
	}

	strcat (picture, picture_usage);
	return 1;
}

static void
set_category_from_usage (int usage, char *type)
{
	switch (usage) {
	case CB_USAGE_INDEX:
		strcpy (type, "INDEX");
		break;
	case CB_USAGE_POINTER:
	case CB_USAGE_PROGRAM_POINTER:
		strcpy (type, "POINTER");
		break;
	case CB_USAGE_DISPLAY:
		strcpy (type, "ALPHANUMERIC");
		break;
	case CB_USAGE_NATIONAL:
		strcpy (type, "NATIONAL");
		break;
	case CB_USAGE_BIT:
		strcpy (type, "BOOLEAN");
		break;
	default:
		strcpy (type, "NUMERIC");
		break;
	}
}

static void
set_category (int category, int usage, char *type)
{
	switch (category) {
	case CB_CATEGORY_UNKNOWN:
		set_category_from_usage (usage, type);
		break;
	case CB_CATEGORY_ALPHABETIC:
		strcpy (type, "ALPHABETIC");
		break;
	case CB_CATEGORY_ALPHANUMERIC:
	case CB_CATEGORY_ALPHANUMERIC_EDITED:
		strcpy (type, "ALPHANUMERIC");
		break;
	case CB_CATEGORY_BOOLEAN:
		strcpy (type, "BOOLEAN");
		break;
	case CB_CATEGORY_INDEX:
		strcpy (type, "INDEX");
		break;
	case CB_CATEGORY_NATIONAL:
	case CB_CATEGORY_NATIONAL_EDITED:
		strcpy (type, "NATIONAL");
		break;
	case CB_CATEGORY_NUMERIC:
	case CB_CATEGORY_NUMERIC_EDITED:
	case CB_CATEGORY_FLOATING_EDITED:
		strcpy (type, "NUMERIC");
		break;
	case CB_CATEGORY_OBJECT_REFERENCE:
		strcpy (type, "OBJECT REF");
		break;
	case CB_CATEGORY_DATA_POINTER:
	case CB_CATEGORY_PROGRAM_POINTER:
		strcpy (type, "POINTER");
		break;
	default:
		strcpy (type, "UNKNOWN");	/* LCOV_EXCL_LINE */
	}
}

/* terminate string at first trailing space ' ' and return its length */
static int
terminate_str_at_first_trailing_space (char * const str)
{
	int	i;

#if 0 /* Simon: We likely do not need to zero-out the complete memory... */
	for (i = strlen (str) - 1; i && isspace ((unsigned char)str[i]); i--) {
		str[i] = '\0';
	}
#else
	for (i = strlen (str) - 1; i && str[i] == ' '; i--);
	str[i + 1] = '\0';
#endif
	return i;
}

static void
print_88_values (struct cb_field *field)
{
	struct cb_field *f;
	char lcl_name[LCL_NAME_LEN] = { '\0' };

	for (f = field->validation; f; f = f->sister) {
		strncpy (lcl_name, (char *)f->name, LCL_NAME_MAX);
		snprintf (print_data, CB_PRINT_LEN,
			"      %-14.14s %02d   %s",
			"CONDITIONAL", f->level, lcl_name);
		print_program_data (print_data);
	}
}

/* print all fields including sister and child elements */
static void
print_fields (struct cb_field *top, int *found)
{
	int	first = 1;
	int	got_picture;
	int	old_level = 0;
	const size_t	picture_len = cb_listing_wide ? 64 : 24;
	char	type[20];
	char	picture[CB_LIST_PICSIZE];
	char	lcl_name[LCL_NAME_LEN];

	for (; top; top = top->sister) {
		if (!top->level) {
			continue;
		}
		if (*found == 0) {
			*found = 1;
			/* MAYBE use a second header line and a forced page break instead */
			snprintf (print_data, CB_PRINT_LEN,
				"      %s", enum_explain_storage(top->storage));
			print_program_data (print_data);
			print_program_data ("");
		}
		if ((top->level == 01
		  || (top->level == 77 && old_level != 77))
		 && !first) {
			print_program_data ("");
		}

		strncpy (lcl_name, check_filler_name ((char *)top->name),
			 LCL_NAME_MAX);

		if (top->children) {
			strcpy (type, "GROUP");
			if (!top->external_definition) {
				got_picture = 0;
			} else {
				got_picture = set_picture (top, picture, picture_len);
			}
		} else {
			set_category (top->common.category, top->usage, type);
			if (top->flag_any_length) {
				picture[0] = 0;
			}
			got_picture = set_picture (top, picture, picture_len);
		}

		if (top->flag_any_length || top->flag_unbounded) {
			pd_off = sprintf (print_data, "????? ");
		} else if (top->flag_occurs && !got_picture) {
			pd_off = sprintf (print_data, "%05d ", top->size * top->occurs_max);
		} else {
			pd_off = sprintf (print_data, "%05d ", top->size);
		}

		pd_off += sprintf (print_data + pd_off, "%-14.14s %02d   ", type, top->level);

		if (got_picture) {
			pd_off += sprintf (print_data + pd_off, "%-30.30s %s", lcl_name, picture);
		} else if (top->flag_occurs) {
			pd_off += sprintf (print_data + pd_off, "%-30.30s ", lcl_name);
		} else { /* Trailing spaces break testsuite AT_DATA */
			pd_off += sprintf (print_data + pd_off, "%s", lcl_name);
		}

		if (top->flag_occurs) {
			if (got_picture) {
				/* separator between picture from above and OCCURS */
				pd_off += sprintf (print_data + pd_off, ", ");
			}
			if (top->depending && top->flag_unbounded) {
				pd_off += sprintf (print_data + pd_off, "OCCURS %d TO UNBOUNDED", top->occurs_min);
			} else if (top->depending) {
				pd_off += sprintf (print_data + pd_off, "OCCURS %d TO %d", top->occurs_min, top->occurs_max);
			} else {
				pd_off += sprintf (print_data + pd_off, "OCCURS %d", top->occurs_max);
			}
			if (top->step_count
			 && top->step_count > top->size) {
				pd_off += sprintf (print_data + pd_off, ", STEP %d", top->step_count);
			}
		}

		pd_off += sprintf (print_data + pd_off, "%s%s%s",
			top->flag_external ? " EXTERNAL" : "",
			top->flag_is_global ? " GLOBAL" : "",
			top->flag_item_based ? " BASED" : "");

		if (top->redefines && !top->file) {
			pd_off += sprintf (print_data + pd_off, ", REDEFINES %s", top->redefines->name);
		}
		print_program_data (print_data);

		first = 0;
		old_level = top->level;

		/* skip printing of details for TYPEDEF / SAME-AS / LIKE */
		if (top->external_definition) {
			continue;
		}

		print_88_values (top);

		if (top->children) {
			print_fields (top->children, found);
		}
	}
}

static void
print_files_and_their_records (cb_tree file_list_p)
{
	cb_tree	l;
	int dummy = 1;

	for (l = file_list_p; l; l = CB_CHAIN (l)) {
		snprintf (print_data, CB_PRINT_LEN,
			"%s%05d %-14.14s      %s",
			cb_list_datamap ? "       " : "",
			 CB_FILE (CB_VALUE (l))->record_max,
			 "FILE",
			 CB_FILE (CB_VALUE (l))->name);
		print_program_data (print_data);
		if (CB_FILE (CB_VALUE (l))->record) {
			print_fields (CB_FILE (CB_VALUE (l))->record, &dummy);
			print_program_data ("");
		}
	}
}

static int
print_fields_in_section (struct cb_field *first_field_in_section)
{
	int found = 0;
	if (first_field_in_section != NULL) {
		print_fields (first_field_in_section, &found);
		if (found) {
			print_program_data ("");
		}
	}
	return found;
}

#ifdef COB_INTERNAL_XREF
/* create xref_elem with line number for existing xref entry */
void
cobc_xref_link (struct cb_xref *list, const int line, const int receiving)
{
	struct cb_xref_elem* elem = list->tail;
	struct cb_xref_elem* new_elem;

	/* only search if line is less then last entry ...*/
	if (elem && elem->line >= line) {
		for (; elem; elem = elem->prev) {
			if (elem->line == line) {
				if (receiving) {
					elem->receive = 1;
				}
				return;
			}
			if (elem->line < line) {
				break;
			}
		}
	}
	/* ... otherwise it is guaranteed to be new */

	list->amount++;

	new_elem = cobc_parse_malloc (sizeof (struct cb_xref_elem));
	new_elem->line = line;
	new_elem->receive = receiving;
	new_elem->prev = elem;

	/* add xref_elem to head/tail
	   remark: if head == NULL, then tail may contain reference to child's
	   head marking it as "referenced by child" - we don't want to preserve
	   this information but overwrite it with the actual reference */
	if (list->head == NULL) {
		list->head = new_elem;
	} else if (list->tail != NULL) {
		/* inserting in between, elem is last matched entry */
		if (list->tail->line > line) {
			if (!elem) {
				new_elem->next = list->head;
				list->head->prev = new_elem;
				list->head = new_elem;
			} else {
				new_elem->next = elem->next;
				elem->next = new_elem;
				if (list->tail == elem) {
					list->tail = new_elem;
				}
			}
			return;
		}
		list->tail->next = new_elem;
	}
	list->tail = new_elem;
}

/* set "referenced by child" (including lvl 88 validation) for field's parents */
void
cobc_xref_link_parent (const struct cb_field *field)
{
	struct cb_field *f;
	const struct cb_xref *f_xref = &field->xref;
	struct cb_xref *p_xref;

	for (f = field->parent; f; f = f->parent) {
		/* parent has own reference already -> exit */
		p_xref = &f->xref;
		if (p_xref->head != NULL) {
			return;
		}
		p_xref->tail = f_xref->tail;
	}
}

/* add a "receiving" entry for a given field reference */
void
cobc_xref_set_receiving (const cb_tree target_ext)
{
	cb_tree	target = target_ext;
	struct cb_field		*target_fld;
	int				xref_line;

	if (CB_CAST_P (target)) {
		target = CB_CAST (target)->val;
	}
	if (CB_REF_OR_FIELD_P (target)) {
		target_fld = CB_FIELD_PTR (target);
	} else {
		return;
	}
	if (CB_REFERENCE_P (target)) {
		xref_line = CB_REFERENCE (target)->common.source_line;
	} else if (current_statement) {
		xref_line = current_statement->common.source_line;
	} else {
		xref_line = cb_source_line;
	}
	cobc_xref_link (&target_fld->xref, xref_line, 1);
}

void
cobc_xref_call (const char *name, const int line, const int is_ident, const int is_sys)
{
	struct cb_call_elem	*elem;

	for (elem = current_program->call_xref.head; elem; elem = elem->next) {
		if (!strcmp (name, elem->name)) {
			cobc_xref_link (&elem->xref, line, 0);
			return;
		}
	}

	elem = cobc_parse_malloc (sizeof (struct cb_call_elem));
	elem->name = cobc_parse_strdup (name);
	elem->is_identifier = is_ident;
	elem->is_system = is_sys;
	cobc_xref_link (&elem->xref, line, 0);

	if (current_program->call_xref.head == NULL) {
		current_program->call_xref.head = elem;
	} else if (current_program->call_xref.tail != NULL) {
		current_program->call_xref.tail->next = elem;
	}
	current_program->call_xref.tail = elem;
}

static void
xref_print (struct cb_xref *xref, const enum xref_type type, struct cb_xref *xref_parent)
{
	struct cb_xref_elem	*elem;
	int     		cnt;
	int     		maxcnt = cb_listing_wide ? 10 : 5;

	if (xref->head == NULL) {
		sprintf (print_data + pd_off, "  ");
		if (type == XREF_FIELD) {
			/* check if parent has any reference and use it for the current field */
			if (xref_parent && xref_parent->head) {
				xref->head = xref_parent->head;
			}
			/* references by child only are stored in xref->tail if xref->head was NULL */
			if (xref->head && xref->tail) {
				sprintf (print_data + pd_off, "referenced by parent/child");
			} else if (xref->head) {
				sprintf (print_data + pd_off, "referenced by parent");
			} else if (xref->tail) {
				sprintf (print_data + pd_off, "referenced by child");
			} else {
				sprintf (print_data + pd_off, "not referenced");
			}
		} else {
			sprintf (print_data + pd_off, "not referenced");
		}
		print_program_data (print_data);
		return;
	}

	cnt = 0;
	for (elem = xref->head; elem; elem = elem->next) {
		pd_off += sprintf (print_data + pd_off, " %c%-6u",
			elem->receive ? '*' : ' ', elem->line);
		if (++cnt == maxcnt) {
			cnt = 0;
			(void)terminate_str_at_first_trailing_space (print_data);
			print_program_data (print_data);
			pd_off = sprintf (print_data, "%38.38s", " ");
		}
	}
	while (++cnt < maxcnt) {
		pd_off += sprintf (print_data + pd_off, "        ");
	}
	pd_off += sprintf (print_data + pd_off, " x%-6u",
		xref->amount);

	(void)terminate_str_at_first_trailing_space (print_data);
	print_program_data (print_data);
}

static void
xref_88_values (struct cb_field *field)
{
	struct cb_field *f;
	char lcl_name[LCL_NAME_LEN] = { '\0' };

	for (f = field->validation; f; f = f->sister) {
		strncpy (lcl_name, (char *)f->name, LCL_NAME_MAX);
		pd_off = sprintf (print_data,
			"%-30.30s %-6u ",
			lcl_name, f->common.source_line);
		xref_print (&f->xref, XREF_FIELD, NULL);
	}
}

static int
xref_fields (struct cb_field *top)
{
	char		lcl_name[LCL_NAME_LEN];
	int		found = 0;

	for (; top; top = top->sister) {
		/* no entry for internal generated fields
		   other than used special indexes */
		if (!top->level || (top->index_type != CB_NORMAL_INDEX
				    && !top->count)) {
			continue;
		}

		strncpy (lcl_name, check_filler_name ((char *)top->name), LCL_NAME_MAX);
		lcl_name[LCL_NAME_MAX] = 0;	/* make sure we always have the trailing NULL */
		if (!strcmp (lcl_name, "FILLER") && !top->validation) {
			if (top->children) {
				found += xref_fields (top->children);
			}
			continue;
		}
		found = 1;
		pd_off = sprintf (print_data, "%-30.30s %-6u ",
			 lcl_name, top->common.source_line);

		/* print xref for field */
		if (top->parent) {
			xref_print (&top->xref, XREF_FIELD, &top->parent->xref);
		} else {
			xref_print (&top->xref, XREF_FIELD, NULL);
		}

		/* print xref for all assigned 88 validation entries */
		if (top->validation) {
			xref_88_values (top);
		}

		/* print xref for all children */
		if (top->children) {
			(void)xref_fields (top->children);
		}
	}
	return found;
}

static void
xref_files_and_their_records (cb_tree file_list_p)
{
	cb_tree	l;

	for (l = file_list_p; l; l = CB_CHAIN (l)) {
		pd_off = sprintf (print_data, "%-30.30s %-6u ",
			 CB_FILE (CB_VALUE (l))->name,
			 CB_FILE (CB_VALUE (l))->common.source_line);
		xref_print (&CB_FILE (CB_VALUE (l))->xref, XREF_FILE, NULL);
		if (CB_FILE (CB_VALUE (l))->record) {
			(void)xref_fields (CB_FILE (CB_VALUE (l))->record);
		}
		print_program_data ("");
	}
}

static int
xref_fields_in_section (struct cb_field *first_field_in_section)
{
	int found = 0;

	if (first_field_in_section != NULL) {
		found = !!xref_fields (first_field_in_section);
		print_program_data ("");
	}
	return found;
}

static int
xref_labels (cb_tree label_list_p)
{
	cb_tree	l;
	char	label_type = ' ';
	struct cb_label *lab;

	for (l = label_list_p; l; l = CB_CHAIN (l)) {
		if (CB_LABEL_P(CB_VALUE(l))) {
			lab = CB_LABEL (CB_VALUE (l));
			if (lab->xref.skip) {
				continue;
			}
			if (lab->flag_entry) {
				label_type = 'E';
				sprintf (print_data, "E %-28.28s %d",
					lab->name, lab->common.source_line);
				print_program_data (print_data);
				continue;
			} else if (lab->flag_section) {
				label_type = 'S';
			} else {
				label_type = 'P';
			}
			pd_off = sprintf (print_data, "%c %-28.28s %-6u ",
				label_type, lab->name, lab->common.source_line);
			xref_print (&lab->xref, XREF_LABEL, NULL);
		}
	}
	if (label_type == ' ') {
		return 0;
	} else {
		return 1;
	}
}

static int
xref_calls (struct cb_call_xref *list)
{
	struct cb_call_elem *elem;
	int gotone = 0;

	if (list->head) {
		set_listing_header_xref (XREF_FUNCTION);
		force_new_page_for_next_line ();
		print_program_header ();
	}

	for (elem = list->head; elem; elem = elem->next) {
		gotone = 1;
		pd_off = sprintf (print_data, "%c %-28.28s %-6.6s ",
			elem->is_identifier ? 'I' : 'L',
			elem->name,
			elem->is_system ? "SYSTEM" : "EXTERN");
		xref_print (&elem->xref, XREF_FUNCTION, NULL);
	}
	return gotone;
}
#endif /* COB_INTERNAL_XREF */

static void
print_program_trailer (void)
{
	struct cb_program	*p;
	struct list_error	*err;
	int			print_names = 0;
	int			print_break = 1;
	int			found;
	char			err_msg[BUFSIZ];

	if (current_program != NULL) {

		/* ensure correct order in program list */
		restore_program_list_order ();

		/* Print program in symbol table / cross-reference if more than one program */
		/* MAYBE use a second header line and a forced page break instead */
		if (current_program->next_program) {
			print_names = 1;
		}

		/* Print file/symbol tables if requested */
		if (cb_listing_symbols) {
			if (cb_listing_with_header) {
				set_listing_header_symbols ();
			}
			force_new_page_for_next_line ();
			print_program_header ();

			for (p = current_program; p; p = p->next_program) {
				if (print_names) {
					sprintf (print_data,
						"      %-14s      %s",
			 	 		(p->prog_type == COB_MODULE_TYPE_FUNCTION ?
				 			"FUNCTION" : "PROGRAM"),
			 	 		p->program_name);
					print_program_data (print_data);
					print_program_data ("");
				}
				found = 0;
				if (p->file_list) {
					print_files_and_their_records (p->file_list);
					found++;
				}
				found += print_fields_in_section (p->working_storage);
				found += print_fields_in_section (p->local_storage);
				found += print_fields_in_section (p->linkage_storage);
				found += print_fields_in_section (p->screen_storage);
				found += print_fields_in_section (p->report_storage);
				if (!found) {
					snprintf (print_data, CB_PRINT_LEN, "      %s",
						_("No fields defined."));
					print_program_data (print_data);
					print_program_data ("");
				}
			}
			print_break = 0;
		}

#ifdef COB_INTERNAL_XREF
		/* Print internal cross reference if requested */
		if (cb_listing_xref) {

			for (p = current_program; p; p = p->next_program) {

				set_listing_header_xref (XREF_FIELD);
				force_new_page_for_next_line ();
				print_program_header ();

				if (print_names) {
					sprintf (print_data,
						 "%s %s",
			 	 		(p->prog_type == COB_MODULE_TYPE_FUNCTION ?
				 			"FUNCTION" : "PROGRAM"),
			 	 		p->program_name);
					print_program_data (print_data);
					print_program_data ("");
				}
				found = 0;
				if (p->file_list) {
					xref_files_and_their_records (p->file_list);
					found++;
				}
				found += xref_fields_in_section (p->working_storage);
				found += xref_fields_in_section (p->local_storage);
				found += xref_fields_in_section (p->linkage_storage);
				found += xref_fields_in_section (p->screen_storage);
				found += xref_fields_in_section (p->report_storage);
				if (!found) {
					snprintf (print_data, CB_PRINT_LEN, "      %s",
						_("No fields defined."));
					print_program_data (print_data);
					print_program_data ("");
				}

				set_listing_header_xref (XREF_LABEL);
				force_new_page_for_next_line ();
				print_program_header ();

				if (print_names) {
					sprintf (print_data,
						 "%s %s",
			 	 		(p->prog_type == COB_MODULE_TYPE_FUNCTION ?
				 			"FUNCTION" : "PROGRAM"),
			 	 		p->program_name);
					print_program_data (print_data);
					print_program_data ("");
				}
				if (!xref_labels (p->exec_list)) {
					snprintf (print_data, CB_PRINT_LEN, "      %s",
						_("No labels defined."));
					print_program_data (print_data);
					print_program_data ("");
				};

				xref_calls (&p->call_xref);
			}
			print_break = 0;
		}
#endif
	}

	set_listing_header_none();
	print_program_data ("");
	if (print_break) {
		print_program_data ("");
	}

	/* Print error/warning summary (this note may be always included later)
	   and/or be replaced to be the secondary title of the listing */
	if (cb_listing_error_head && cb_listing_with_messages) {
		force_new_page_for_next_line ();
		print_program_data (_("Error/Warning summary:"));
		print_program_data ("");
	}
	if (cb_listing_error_head) {
		if (cb_listing_with_messages) {
			err = cb_listing_error_head;
			do {
				const char *prefix = err->prefix ? err->prefix : "";
				if (err->file) {
					if (err->line > 0) {
						if (cb_msg_style == CB_MSG_STYLE_MSC) {
							snprintf (err_msg, BUFSIZ, "%s(%d): %s%s",
								err->file, err->line, prefix, err->msg);
						} else {
							snprintf (err_msg, BUFSIZ, "%s:%d: %s%s",
								err->file, err->line, prefix, err->msg);
						}
					} else {
						snprintf (err_msg, BUFSIZ, "%s: %s%s",
							err->file, prefix, err->msg);
					}
				} else {
					snprintf (err_msg, BUFSIZ, "%s%s",
						prefix, err->msg);
				}
				print_program_data (err_msg);
				err = err->next;
			} while (err);
			print_program_data ("");
		}

		free_error_list (cb_listing_error_head);
		cb_listing_error_head = NULL;
		cb_listing_error_tail = NULL;
	}

	if (!cb_listing_with_messages) {
		return;
	}

	/* Print error counts */

	switch (warningcount) {
	case 0:
		print_program_data (_("0 warnings in compilation group"));
		break;
	case 1:
		/* FIXME: Change to P_, needs changes to Makevars and tests */
		print_program_data (_("1 warning in compilation group"));
		break;
	default:
		snprintf (print_data, CB_PRINT_LEN,
			_("%d warnings in compilation group"), warningcount);
		print_program_data (print_data);
		break;
	}
	switch (errorcount) {
	case 0:
		print_program_data (_("0 errors in compilation group"));
		break;
	case 1:
		/* FIXME: Change to P_, needs changes to Makevars and tests */
		print_program_data (_("1 error in compilation group"));
		break;
	default:
		snprintf (print_data, CB_PRINT_LEN,
			_("%d errors in compilation group"), errorcount);
		print_program_data (print_data);
		break;
	}
	if (errorcount > cb_max_errors) {
		snprintf (print_data, CB_PRINT_LEN,
			_("Too many errors in compilation group: %d maximum errors"),
			cb_max_errors);
		print_program_data (print_data);
	}
	force_new_page_for_next_line ();
}

/*
  return pointer to next non-space character
*/
static COB_INLINE COB_A_INLINE char *
get_next_nonspace (char * pos)
{
	while (*pos != '\0' && isspace ((unsigned char)*pos)) {
		pos++;
	}
	return pos;
}

/*
  Find next token after bp, copy it to token and copy the token terminator to
  term. Return pointer to the character after the terminator.
*/
static char *
get_next_token (char *bp, char *token, char *term)
{
	char	*token_start = token;
	int	in_string = 0;

	/* Repeat until a token is found */
	do {
		bp = get_next_nonspace (bp);

		term[0] = '\0';
		term[1] = '\0';
		if (*bp == '\0') {
			return NULL;
		}

		/* Copy characters into token until a terminator is found. */
		while (*bp) {
			/* Return character strings as a single token */
			if (*bp == '"' || *bp == '\'') {
				in_string = !in_string;
				*token++ = *bp++;
				if (!in_string) {
					if (isspace ((unsigned char)*bp) || *bp == ',' || *bp == '.' || *bp == ';') {
						term[0] = *bp++;
					}
					break;
				}
				continue;
			}
			if (in_string) {
				*token++ = *bp++;
				continue;
			}
			if (*bp == '.' && isdigit((unsigned char)*(bp + 1))) {
				;
			} else if (isspace ((unsigned char)*bp) || *bp == ',' || *bp == '.' || *bp == ';') {
				term[0] = *bp++;
				break;
			}
			*token++ = *bp++;
		}
		*token = '\0';
	} while (*token_start == '\0' && *term != '\0');

	return bp;
}

static void
terminate_str_at_first_of_char (const char c, char * const str)
{
	char	*first_instance  = strchr (str, c);

	if (first_instance != NULL) {
		*first_instance = '\0';
	}
}

/*
  Copies the next CB_LINE_LENGTH chars from fd into out_line. If fixed is true,
  out_line is padded with spaces to column CB_ENDLINE. The return value is
  either the length of out_line, or -1 if the end of fd is reached.
*/
static int
get_next_listing_line (FILE *fd, char **pline, int fixed)
{
	char	*in_char, *out_line;
	unsigned int	i = 0;
	char	in_line[CB_LINE_LENGTH + 2];

	if (*pline == NULL) {
	   *pline = cobc_malloc (CB_LINE_LENGTH + 2);
	}
	out_line = *pline;

	if (!fgets (in_line, CB_LINE_LENGTH, fd)) {
		memset (out_line, 0, CB_LINE_LENGTH);
		return -1;
	}

	terminate_str_at_first_of_char ('\n', in_line);
	terminate_str_at_first_of_char ('\r', in_line);

	for (in_char = in_line; i != CB_LINE_LENGTH && *in_char; in_char++) {
		if (*in_char == '\t') {
			out_line[i++] = ' ';
			while (i % cb_tab_width != 0) {
				out_line[i++] = ' ';
				if (i == CB_LINE_LENGTH) {
					break;
				}
			}
		} else {
			out_line[i++] = *in_char;
		}
	}

	if (fixed) {
#if 1 /* Simon: that should be portable enough */
		int size = (unsigned int)CB_ENDLINE - i;
		if (size > 0) {
			memset (&out_line[i], ' ', (size_t)size);
			i = (unsigned int)CB_ENDLINE;
		}
#else
		while (i < (unsigned int)CB_ENDLINE) {
			out_line[i++] = ' ';
		}
#endif
	} else {
		out_line[i++] = ' ';
	}
	out_line[i] = 0;

	return i;
}

/*
  return pointer to first non-space character (ignoring sequence area)
*/
static COB_INLINE COB_A_INLINE char *
get_first_nonspace (char *line, const enum cb_format source_format)
{
	if (source_format != CB_FORMAT_FREE) {
		return get_next_nonspace (line + CB_INDICATOR + 1);
	} else {
		return get_next_nonspace (line);
	}
}

/*
  check for compiler directive indicator and return
  position of compiler instruction or NULL if not found
*/
static char *
get_directive_start (char *line, const enum cb_format source_format)
{
	char	*curr_pos;

	curr_pos = get_first_nonspace (line, source_format);
	if (*curr_pos == '>' && *++curr_pos == '>') {
		curr_pos = get_next_nonspace (++curr_pos);
		if (*curr_pos != 0) {
			return curr_pos;
		}
	}
	return NULL;
}

/*
  check for >> LISTING directive and set on_off value
*/
static int
line_has_listing_directive (char *line, const enum cb_format source_format, int *on_off)
{
	char	*token;

	token = get_directive_start (line, source_format);

	if (token != NULL &&
		!strncasecmp (token, "LISTING", 7)) {
		token += 7;
		*on_off = 1;
		token = get_next_nonspace (token);
		if (!strncasecmp (token, "OFF", 3))
			*on_off = 0;
		return 1;
	}
	return 0;
}

/*
  check for >> PAGE directive and page eject indicator
*/
static int
line_has_page_eject (char *line, const enum cb_format source_format)
{
	char	*directive_start;

	if (source_format != CB_FORMAT_FREE && line[CB_INDICATOR] == '/') {
		return 1;
	} else {
		directive_start = get_directive_start (line, source_format);
		return directive_start != NULL
			&& !strncasecmp (directive_start, "PAGE", 4);
	}
}
/*
  check for listing statements in current line and handle them
*/
static int
line_has_listing_statement (char *line, const enum cb_format source_format)
{
	char	*statement_start, *curr_pos;
	int		size;

	/* check if we actually want to process any listing statement */
	if (cb_listing_statements > CB_OBSOLETE) {
		return 0;
	}

	curr_pos = get_first_nonspace (line, source_format);

	if (curr_pos == NULL) {
		return 0;
	}

	statement_start = curr_pos++;

	/* extract first word with max. length of 6 */
	for (size = 1; size < 6 && curr_pos != 0; size++, curr_pos++) {
		if ((*curr_pos == ' ' )
			||  (*curr_pos == '.' )
			||  (*curr_pos == '*' && (*(curr_pos + 1) == '>' ) )) {
			break;
		}
	}

	/* compare word against listing statements */
	if (size != 5) {
		return 0;
	}
	if ((strncasecmp (statement_start, "EJECT", 5))
	&&  (strncasecmp (statement_start, "SKIP1", 5))
	&&  (strncasecmp (statement_start, "SKIP2", 5))
	&&  (strncasecmp (statement_start, "SKIP3", 5))
	&&  (strncasecmp (statement_start, "TITLE", 5))) {
		return 0;
	}

	/* handle statements */
	if (!strncasecmp (statement_start, "TITLE", 5)) {
		/* check if we actually want to process TITLE as a statement
		   note: the title statement is an extra listing-directive statement */
		if (cb_title_statement > CB_OBSOLETE) {
			return 0;
		}

		/* FIXME: the title should be handled correctly as literal */
		while (*curr_pos != 0) {
			if (*++curr_pos != ' ') {
				curr_pos++; /* skip start of literal */
				break;
			}
		}
		statement_start = curr_pos;
		for (size = 1; size < 80 && curr_pos != 0; size++, curr_pos++) {
			if ((*curr_pos == '.' )
				||  (*curr_pos == '*' && (*(curr_pos + 1) == '>' ) )) {
				break;
			}
		}
		snprintf (print_data, size, "%s", statement_start);
		size = terminate_str_at_first_trailing_space (print_data);
		print_data[size] = 0;
		print_data[sizeof (cb_listing_title)] = 0;
		strcpy (cb_listing_title, print_data);
		force_new_page_for_next_line ();
	} else {
		if (!strncasecmp (statement_start, "EJECT", 5)) {
			force_new_page_for_next_line ();
		} else if (!strncasecmp (statement_start, "SKIP1", 5))  {
			print_program_data ("\n");
		} else if (!strncasecmp (statement_start, "SKIP2", 5)) {
			print_program_data ("\n\n");
		} else if (!strncasecmp (statement_start, "SKIP3", 5)) {
			print_program_data ("\n\n\n");
		}
	}
	return 1;
}

static void
print_fixed_line (const int line_num, char pch, char *line)
{
	int		i;
	int		len = strlen (line);
	const int	max_chars_on_line = cb_listing_wide ? 112 : 72;
	const char	*format_str;

	if (line[CB_INDICATOR] == '&') {
		line[CB_INDICATOR] = '-';
		pch = '+';
	}

	for (i = 0; len > 0; i += max_chars_on_line, len -= max_chars_on_line) {
		if (cb_listing_wide) {
			format_str = "%06d%c %-112.112s";
		} else {
			format_str = "%06d%c %-72.72s";
		}
		sprintf (print_data, format_str, line_num, pch, line + i);
		(void)terminate_str_at_first_trailing_space (print_data);
		print_program_data (print_data);

		if (cb_text_column == 72) {
			break;
		}
		pch = '+';
	}
}

static void
print_free_line (const int line_num, char pch, char *line)
{
	int		i;
	int		len = strlen (line);
	const int	max_chars_on_line = cb_listing_wide ? 112 : 72;
	const char	*format_str;

	for (i = 0; len > 0; i += max_chars_on_line, len -= max_chars_on_line) {
		if (cb_listing_wide) {
			format_str = "%06d%c %-112.112s";
		} else {
			format_str = "%06d%c %-72.72s";
		}
		sprintf (print_data, format_str, line_num, pch, line + i);
		(void)terminate_str_at_first_trailing_space (print_data);
		print_program_data (print_data);
		pch = '+';
	}
}

static void
print_errors_for_line (const struct list_error * const first_error,
		       const int line_num)
{
	const struct list_error	*err;
	const unsigned int	max_chars_on_line = cb_listing_wide ? 120 : 80;
	size_t msg_off;

	for (err = first_error; err && err->line <= line_num; err = err->next) {
		if (err->line == line_num) {
			pd_off = snprintf (print_data, max_chars_on_line, "%s%s", err->prefix, err->msg);
			if (pd_off == -1) {	/* snprintf returns -1 in MS and on HPUX if max is reached */
				pd_off = max_chars_on_line;
				print_data[max_chars_on_line - 1] = 0;
			}
			if (pd_off >= max_chars_on_line) {
				/* trim on last space */
				pd_off = strlen (print_data) - 1;
				while (pd_off && !isspace ((unsigned char)print_data[pd_off])) {
					pd_off--;
				}
				print_data[pd_off] = '\0';
				print_program_data (print_data);
				msg_off = strlen (err->prefix);
				pd_off = strlen (print_data) - msg_off;
				if (msg_off < 2) msg_off = 2;
				memset (print_data, ' ', msg_off - 1);
				snprintf (print_data + msg_off - 2, max_chars_on_line, "%c%s", '+', err->msg + pd_off);
			}
			print_program_data (print_data);
		}
	}
}

static void
print_line (struct list_files *cfile, char *line, int line_num, int in_copy)
{
	struct list_skip	*skip;
	int	do_print;
	int	on_off;
	char	pch;

	do_print = cfile->listing_on;
	if (line_has_listing_directive (line, cfile->source_format, &on_off)) {
		cfile->listing_on = on_off;
		/* always print the directive itself */
		do_print = 1;
	} else if (line_has_page_eject (line, cfile->source_format)) {
		force_new_page_for_next_line ();
	} else if (line_has_listing_statement (line, cfile->source_format)) {
		do_print = 0;
	}

	if (do_print) {
		pch = in_copy ? 'C' : ' ';
		for (skip = cfile->skip_head; skip; skip = skip->next) {
			if (skip->skipline == line_num) {
				pch = 'X';
				break;
			}
		}

		(void)terminate_str_at_first_trailing_space (line);
		if (cfile->source_format == CB_FORMAT_FIXED) {
			print_fixed_line (line_num, pch, line);
		} else { /* CB_FORMAT_FREE */
			print_free_line (line_num, pch, line);
		}
	}

	/* Print errors regardless of LISTING setting */
	if (cfile->err_head) {
		print_errors_for_line (cfile->err_head, line_num);
	}
}

#define RET_IF_OVERFLOW(x)					\
	do {							\
		if (out_pos < CB_LINE_LENGTH) {			\
			(x);					\
		} else {					\
			cmp_line[CB_LINE_LENGTH] = '\0';	\
			return last_col;			\
		}						\
	} ONCE_COB
		
/*
  Copy each token in pline from the start of pline[first_idx] to the end of
  pline[last_idx] into cmp_line, separated by a space. Tokens are copied from
  the first_col of each line and up to the end of line or the sequence area (if
  fixed is true).
  Return the column to which pline[last_idx] was read up to.

  first_col is zero-indexed.
*/
static int
compare_prepare (char *cmp_line, char *pline[CB_READ_AHEAD],
		 int first_idx, int last_idx, int first_col, int fixed)
{
	int	i;
	int	out_pos = 0;
	int	line_idx;
	int	in_string = 0;
	int	last_col = CB_SEQUENCE;
	int	last_nonspace;

	cmp_line[0] = 0;

	/* Collapse pline into a string of tokens separated by spaces */
	for (line_idx = first_idx; line_idx < last_idx; line_idx++) {
		if (!fixed) {
			last_col = strlen (pline[line_idx]) - 1;
		}

		/* Go to the last non-space character */
		for (last_nonspace = last_col;
		     isspace ((unsigned char)pline[line_idx][last_nonspace]) && last_nonspace > first_col;
		     last_nonspace--);
		/* Go to first non-space character */
		for (i = first_col; (i <= last_nonspace) && isspace ((unsigned char)pline[line_idx][i]); i++);

		/* Copy chars between the first and last non-space characters */
		while (i <= last_nonspace) {
			if (isspace ((unsigned char)pline[line_idx][i])) {
				RET_IF_OVERFLOW (cmp_line[out_pos++] = ' ');
				for (i++; (i <= last_nonspace) && isspace ((unsigned char)pline[line_idx][i]); i++);
				if (i > last_nonspace) {
					break;
				}
			} else if (pline[line_idx][i] == '"') {
				/*
				  Merge multi-part strings into one string,
				  reading another line if necessary to find the
				  end.
				*/
				if (in_string) {
					i++;
				} else {
					RET_IF_OVERFLOW (cmp_line[out_pos++] = pline[line_idx][i++]);
					in_string = 1;
				}

				for (; (i <= last_nonspace) && (pline[line_idx][i] != '"'); ) {
					RET_IF_OVERFLOW (cmp_line[out_pos++] = pline[line_idx][i++]);
				}
				if (pline[line_idx][i] == '"') {
					RET_IF_OVERFLOW (cmp_line[out_pos++] = pline[line_idx][i++]);
					in_string = 0;
				}
				if (i > last_nonspace) {
					break;
				}
			} else {
				RET_IF_OVERFLOW (cmp_line[out_pos++] = pline[line_idx][i++]);
			}
		}
	}
	cmp_line[out_pos] = 0;
#ifdef DEBUG_REPLACE
	fprintf (stdout, "   last_col = %d\n   cmp_line: %s\n", last_col, cmp_line);
#endif
	return last_col;
}

#undef RET_IF_OVERFLOW

/*
  Add adjust to each line number less than line_num (if appropriate) in cfile's
  copy, replace and error lists.
*/
static void
adjust_line_numbers (struct list_files *cfile, int line_num, int adjust)
{
	struct list_files	*cur;
	struct list_replace	*rep;
	struct list_error	*err;

	for (cur = cfile->copy_head; cur; cur = cur->next) {
		cur->copy_line += adjust;
	}

	for (rep = cfile->replace_head; rep; rep = rep->next) {
		if (rep->firstline > line_num) {
			rep->firstline += adjust;
		}
	}

	for (err = cfile->err_head; err; err = err->next) {
		err->line += adjust;
	}
}

static COB_INLINE COB_A_INLINE int
is_debug_line (char *line, int fixed)
{
	if (line == NULL || line[0] == 0) {
		return 0;
	}
	return !cb_flag_debugging_line
		&& ((fixed && IS_DEBUG_LINE (line))
		    || (!fixed && !strncasecmp (line, "D ", 2)));
}

static COB_INLINE COB_A_INLINE int
is_comment_line (char *line, int fixed)
{
	if (line == NULL || line[0] == 0) {
		return 0;
	}
	return (fixed && IS_COMMENT_LINE (line))
		|| (!fixed && !strncmp (line, "*>", 2));
}

static int
is_continuation_line (char *line, int fixed)
{
	int i;

	if (line == NULL || line[0] == 0) {
		return 0;
	}
	if (fixed) {
		/* check for "-" in column 7 */
		if (IS_CONTINUE_LINE (line)) {
			return 1;
		}
	} else {
		/* check for "&" as last character */
		/* CHECKME: does this work with inline comments after "&"? */
		i = strlen (line) - 1;
		while (i && isspace ((unsigned char)line[i])) i--;
		if (line[i] == '&') {
			return 1;
		}
	}

	return 0;
}

static void
abort_if_too_many_continuation_lines (int pline_cnt, const char *filename, int line_num)
{
	if (pline_cnt >= CB_READ_AHEAD) {
		cobc_err_msg (_("%s: %d: Too many continuation lines"),
				filename, line_num);
		cobc_abort_terminate (0);
	}
}

static void
make_new_continuation_line (const char *cfile_name, char *pline[CB_READ_AHEAD],
			    int * const pline_cnt, int line_num)
{
	abort_if_too_many_continuation_lines (*pline_cnt + 1, cfile_name,
					      line_num);
	if (pline[*pline_cnt + 1] == NULL) {
		pline[*pline_cnt + 1] = cobc_malloc (CB_LINE_LENGTH + 2);
	}
	strcpy (pline[*pline_cnt + 1], pline[*pline_cnt]);
	strcpy (pline[*pline_cnt], pline[*pline_cnt - 1]);
	memset (&pline[*pline_cnt][CB_MARGIN_A], ' ',
		CB_SEQUENCE - CB_MARGIN_A);
	pline[*pline_cnt][CB_INDICATOR] = '&';

        (*pline_cnt)++;
}

static void
add_token_over_multiple_lines (const char *cfile_name,
			       char *pline[CB_READ_AHEAD],
			       int * const pline_cnt,
			       const int line_num,
			       const char *new_token,
			       const int first_col,
			       int new_token_len,
			       int * const out_line,
			       int * const out_col)
{
	int	tok_char = 0;

#ifdef DEBUG_REPLACE
	fprintf (stdout, "   new_token_len = %d\n", new_token_len);
#endif

	while (new_token_len) {
		/* Copy the token one character at a time. */
		pline[*out_line][(*out_col)++] = new_token[tok_char++];
		new_token_len--;

		/*
		  Move to the next line when reach the end of the current one.
		*/
		if (*out_col == CB_SEQUENCE) {
#ifdef DEBUG_REPLACE
			fprintf (stdout, "   NEW pline[%2d] = %s\n",
				 *out_line, pline[*out_line]);
#endif

			*out_col = first_col;
			(*out_line)++;

			/*
			  Allocate a new out_line if we are on the last
			  out_line.
			*/
			if (*out_line == *pline_cnt) {
				make_new_continuation_line (cfile_name, pline,
							    pline_cnt, line_num);
			}
		}
	}

	pline[*out_line][(*out_col)++] = ' ';
}

static void
reflow_replaced_fixed_format_text (const char *cfile_name, char *pline[CB_READ_AHEAD],
				   int * const pline_cnt, const int line_num,
				   char *newline, int first_col, const int last)
{
	int	first_nonspace;
	char	*new_line_ptr;
	char	*new_token;
	char	token_terminator[2];
	int	out_col;
	int	out_line;
	int	force_next_line;
	int	new_token_len;

	new_token = cobc_malloc (strlen(newline) + 2);
	new_line_ptr = get_next_token (newline, new_token, token_terminator);

	/*
	  Start adding tokens from margin B or the first non-space character.
	*/
	for (first_nonspace = first_col;
	     (first_nonspace < last)
	      && isspace ((unsigned char)(pline[0][first_nonspace]));
	     first_nonspace++);
	if (first_nonspace >= CB_MARGIN_B) {
		first_col = CB_MARGIN_B;
	}

	/* For each line,  */
	for (out_line = 0; out_line < *pline_cnt; out_line++) {
		force_next_line = 0;
		out_col = first_col;

		/* Add as many token as possible to the current line. */
		while (new_line_ptr && !force_next_line) {
			new_token_len = strlen (new_token);
			if (new_token_len >= (CB_SEQUENCE - first_col)) {
				/*
				  If the new token does not fit on this line,
				  reflow it onto the next line.
				*/
			        add_token_over_multiple_lines (cfile_name, pline, pline_cnt, line_num,
							       new_token, first_col, new_token_len,
							       &out_line, &out_col);
			} else if ((out_col + 2 + new_token_len) < last) {
				/*
				  If the new token *and* its terminator fits,
				  copy it all onto the current line.
				*/
				strcpy (&pline[out_line][out_col], new_token);
				out_col += strlen (new_token);

				if (token_terminator[0]) {
					pline[out_line][out_col++] = token_terminator[0];
				} else {
				        pline[out_line][out_col++] = ' ';
				}
				if (token_terminator[0] == '.') {
					pline[out_line][out_col++] = ' ';
				}
			} else {
				force_next_line = 1;
				make_new_continuation_line (cfile_name, pline,
							    pline_cnt, line_num);
				continue;
			}
			new_line_ptr = get_next_token (new_line_ptr, new_token, token_terminator);
		}

		if (out_col == first_col) {
			pline[out_line][CB_INDICATOR] = ' ';
		}
		while (out_col < last) {
			pline[out_line][out_col++] = ' ';
		}

#ifdef DEBUG_REPLACE
		fprintf (stdout, "   NEW pline[%2d] = %s\n", out_line, pline[out_line]);
#endif
	}
	cobc_free (new_token);
}

static void
reflow_replaced_free_format_text (char *pline[CB_READ_AHEAD],
				  const int pline_cnt, char *newline,
				  const int first_col)
{
	char	*new_line_ptr;
	char	*new_token;
	char	token_terminator[2];
	int	i;
	int	j;

	new_token = cobc_malloc (strlen(newline) + 2);
	new_line_ptr = get_next_token (newline, new_token, token_terminator);

	for (i = 0; i < pline_cnt; i++) {
		/*
		  Terminate the line at null or the first non-space character.
		*/
		for (j = first_col; pline[i][j] && pline[i][j] == ' '; j++);
		pline[i][j] = '\0';

		/*
		  If the text has not been copied yet, copy it to the start of
		  the line.
		*/
		while (new_line_ptr) {
			/* TO-DO: Replace with strncat? */
			strcat (pline[i], new_token);
			strcat (pline[i], token_terminator);
			j++;
			new_line_ptr = get_next_token (new_line_ptr, new_token,
						       token_terminator);
		}

		if (j == first_col) {
			strcat (pline[i], " ");
		}
	}
	cobc_free (new_token);
}

static int
reflow_replaced_text (const char *cfile_name, char *pline[CB_READ_AHEAD],
		      int pline_cnt, int line_num, char *newline, int first_col,
		      int last_col, int fixed)
{
	if (fixed) {
	        reflow_replaced_fixed_format_text (cfile_name, pline,
						   &pline_cnt, line_num,
						   newline, first_col,
						   last_col);
	} else {
		reflow_replaced_free_format_text (pline, pline_cnt, newline,
						  first_col);
	}

	return pline_cnt;
}

/* TODO: Modularise! */

static int
print_replace_text (struct list_files *cfile, FILE *fd,
		    struct list_replace *rep, char *pline[CB_READ_AHEAD],
		    int pline_cnt, int line_num)
{
	char	*rfp = rep->from;
	char	*from_ptr;
	char	*to_ptr;
	char	*newline;
	const int	fixed = (cfile->source_format == CB_FORMAT_FIXED);
	int	first_col = fixed ? CB_MARGIN_A : 0;
	int	last;
	int	multi_token;
	int	match = 0;
	int	eof = 0;
	int	submatch = 0;
	int	seccount = 0;
	int	overread = 0;
	int	tokmatch = 0;
	int	subword = 0;
	size_t	ttix, ttlen, from_token_len;
	size_t	newlinelen;
	char	lterm[2];
	char	fterm[2];
	char	ftoken[CB_LINE_LENGTH + 2];
	char	tterm[2];
	char	ttoken[CB_LINE_LENGTH + 2];
	char	cmp_line[CB_LINE_LENGTH + 2];
	char	from_line[CB_LINE_LENGTH + 2];

	if (is_comment_line (pline[0], fixed)) {
		return pline_cnt;
	}

	/* Trim the string to search and replace */
	(void)terminate_str_at_first_trailing_space (rfp);
	while (*rfp && isspace ((unsigned char)(*rfp))) {
		rfp++;
	}
	multi_token = (strchr (rfp, ' ') != NULL);

#ifdef DEBUG_REPLACE
	fprintf (stdout, "print_replace_text: line_num = %d", line_num);
	fprintf (stdout, ", multi_token = %s, fixed = %s\n",
		 multi_token ? "TRUE" : "FALSE", fixed ? "TRUE" : "FALSE");
	fprintf (stdout, "   pline_cnt = %d\n", pline_cnt);
	for (int i = 0; i < pline_cnt; i++) {
		fprintf (stdout, "   pline[%2d]: %s\n", i, pline[i]);
	}
	fprintf (stdout, "   rep: first = %d, last = %d, lead_trail = %d\n",
		 rep->firstline, rep->lastline, rep->lead_trail);
	fprintf (stdout, "   fromlen: %d\n", strlen(rfp));
	fprintf (stdout, "   from: '%80.80s'\n", rfp);
	fprintf (stdout, "   tolen: %d\n", strlen(rep->to));
	fprintf (stdout, "   to:   '%80.80s'\n", rep->to);
#endif

	newlinelen = CB_LINE_LENGTH+2;
	newline = cobc_malloc (newlinelen);

	last = compare_prepare (cmp_line, pline, 0, pline_cnt, first_col, fixed);

	newline[0] = 0;
	if (multi_token) {
		/*
		  Attempt to match the source text from the beginning of each
		  line (continuing the match to the next line if need be). If a
		  match is found, output the line to newline with the match
		  replaced.
		*/

		strcpy (from_line, rfp);
		from_ptr = get_next_token (from_line, ftoken, fterm);
	force_next_line:
		to_ptr = get_next_token (cmp_line, ttoken, tterm);

		/*
		  Read tokens until the match is complete or until a match
		  fails.
		*/
		while (to_ptr && from_ptr) {
			if (!strcasecmp (ttoken, ftoken)) {
				/*
				  Mark two tokens as matched, then read next
				  pair.
				*/
				submatch = 1;
				if (fterm[0] == tterm[0]) {
					lterm[0] = 0;
				} else {
					lterm[0] = tterm[0];
				}
				lterm[1] = tterm[1];
				to_ptr = get_next_token (to_ptr, ttoken, tterm);
				from_ptr = get_next_token (from_ptr, ftoken, fterm);
			} else {
				/* Discard partial match. */
				if (seccount == 0) {
					if ((strlen (newline) + strlen (ttoken) + strlen (tterm)) >= newlinelen) {
						newlinelen += strlen (ttoken) + CB_LINE_LENGTH;
						newline = cobc_realloc (newline, newlinelen);
					}
					strcat (newline, ttoken);
					strcat (newline, tterm);
				}
				submatch = 0;

				/* Start matching from beginning of from_line again. */
				strcpy (from_line, rfp);
				from_ptr = get_next_token (from_line, ftoken, fterm);
				to_ptr = get_next_token (to_ptr, ttoken, tterm);
				break;
			}
		}
		if (!from_ptr && submatch) {
			/*
			  If the match is complete, output the match's
			  replacement.
			*/
			match = 1;
			if ((strlen (newline) + strlen (rep->to) + strlen (lterm)) >= newlinelen) {
				newlinelen += strlen (rep->to) + CB_LINE_LENGTH;
				newline = cobc_realloc (newline, newlinelen);
			}
			strcat (newline, rep->to);
			strcat (newline, lterm);
			if (to_ptr) {
				if ((strlen (newline) + strlen (ttoken) + strlen (to_ptr)) >= newlinelen) {
					newlinelen += strlen (ttoken) + strlen (to_ptr) + CB_LINE_LENGTH;
					newline = cobc_realloc (newline, newlinelen);
				}
				strcat (newline, ttoken);
				strcat (newline, tterm);
				strcat (newline, to_ptr);
			}
		} else if (!to_ptr && submatch) {
			/*
			  If we run out of chars from the original source, get
			  more.
			*/

#ifdef DEBUG_REPLACE
			fprintf (stdout, "   submatch = TRUE\n");
#endif
			if (eof) {
				cobc_free (newline);
				return pline_cnt;
			}

			/*
			  Overwrite the current line if it is a comment or debug
			  line.
			*/
			if (is_comment_line (pline[pline_cnt], fixed)) {
				adjust_line_numbers (cfile, line_num,  -1);
				overread = 1;
			}
			if (is_debug_line (pline[pline_cnt], fixed)) {
				adjust_line_numbers (cfile, line_num,  -1);
				overread = 1;
			}

			/*
			  Read lines until we find a non-comment, non-debug
			  line.
			 */
		next_rec:
			if (!is_comment_line (pline[pline_cnt], fixed)) {
				pline_cnt++;
			}
			abort_if_too_many_continuation_lines (pline_cnt, cfile->name, line_num);
			if (get_next_listing_line (fd, &pline[pline_cnt], fixed) < 0) {
				pline[pline_cnt][0] = 0;
				eof = 1;
			}
			if (is_debug_line (pline[pline_cnt], fixed)
			    || is_comment_line (pline[pline_cnt], fixed)) {
				adjust_line_numbers (cfile, line_num,  -1);
				goto next_rec;
			}
#ifdef DEBUG_REPLACE
			fprintf (stdout, "   pline[%2d]: %s\n", pline_cnt - 1,
				 pline[pline_cnt - 1]);
#endif
			line_num++;
			seccount++;
			if (overread) {
				overread = 0;
				goto next_rec;
			}
			last = compare_prepare (cmp_line, pline, pline_cnt - 1, pline_cnt,
						first_col, fixed);
			strcat (newline, " ");
			goto force_next_line;
		}
	} else {
		strcpy (from_line, rfp);
		from_ptr = get_next_token (from_line, ftoken, fterm);
		if (ftoken[0] == ':' || ftoken[0] == '(') {
			subword = 1;
		}
		from_token_len = strlen (ftoken);

		/*
		  For each token in cmp_line, try to match it with the token in
		  from_line.
		 */
		for (to_ptr = get_next_token (cmp_line, ttoken, tterm); to_ptr;
		     to_ptr = get_next_token (to_ptr, ttoken, tterm)) {
#ifdef DEBUG_REPLACE
			fprintf (stdout, "   tterm = '%s', ttoken = '%s', ftoken = '%s'\n",
				 tterm, ttoken, ftoken);
#endif
			ttlen = strlen (ttoken);
			ttix = 0;
			if (rep->lead_trail == CB_REPLACE_LEADING) {
				subword = 1;
			} else if (rep->lead_trail == CB_REPLACE_TRAILING) {
				if (ttlen >= from_token_len) {
					subword = 1;
					ttix = ttlen - from_token_len;
					ttlen = ttix;
				}
			}
			if (subword) {
				tokmatch = !strncasecmp (&ttoken[ttix], ftoken, from_token_len);
			} else {
				tokmatch = !strcasecmp (ttoken, ftoken);
			}
			if (tokmatch) {
				if ((strlen (newline) + strlen (ttoken) + strlen (rep->to)) >= newlinelen) {
					newlinelen += strlen (ttoken) + strlen (rep->to) + CB_LINE_LENGTH;
					newline = cobc_realloc (newline, newlinelen);
				}
				if (subword) {
					if (rep->lead_trail == CB_REPLACE_LEADING) {
						strcat (newline, rep->to);
						strcat (newline, &ttoken[from_token_len]);
					} else if (rep->lead_trail == CB_REPLACE_TRAILING) {
						strncat (newline, ttoken, ttlen);
						strcat (newline, rep->to);
					} else {
						strcat (newline, rep->to);
					}
				} else {
					strcat (newline, rep->to);
				}
				match = 1;
			} else {
				if ((strlen (newline) + strlen (ttoken) + strlen (tterm)) >= newlinelen) {
					newlinelen += strlen (ttoken) + CB_LINE_LENGTH;
					newline = cobc_realloc (newline, newlinelen);
				}
				strcat (newline, ttoken);
			}
			strcat (newline, tterm);
		}
	}

	if (match) {
#ifdef DEBUG_REPLACE
		fprintf (stdout, "   match = TRUE\n   newline = %s\n", newline);
#endif
		pline_cnt = reflow_replaced_text (cfile->name, pline, pline_cnt,
						  line_num, newline, first_col,
						  last, fixed);
	}

	cobc_free (newline);
	return pline_cnt;
}

static void
remove_replace_entries_before_line (struct list_files *cfile, const int line_num)
{
	struct list_replace	*rep;

	while (cfile->replace_head
	       && cfile->replace_head->firstline < line_num) {
		rep = cfile->replace_head;
		cfile->replace_head = rep->next;

		if (rep->from) {
			cobc_free (rep->from);
		}
		if (rep->to) {
			cobc_free (rep->to);
		}
		cobc_free (rep);
	}
}

static void
deep_copy_list_replace (struct list_replace *src, struct list_files *dst_file)
{
	struct list_replace	*copy;

	copy = cobc_malloc (sizeof (struct list_replace));
	memcpy (copy, src, sizeof (struct list_replace));
	copy->next = NULL;
	if (src->to) {
		copy->to = cobc_strdup (src->to);
	}
	if (src->from) {
		copy->from = cobc_strdup (src->from);
	}

	if (dst_file->replace_tail) {
		dst_file->replace_tail->next = copy;
	}
	if (!dst_file->replace_head) {
		dst_file->replace_head = copy;
	}
	dst_file->replace_tail = copy;
}

static void
cleanup_copybook_reference (struct list_files *cur)
{
	if (cur->name) {
		cobc_free ((void *)cur->name);
	}
	cobc_free (cur);
}


/* TO-DO: Modularise! */
/*
  Applies active REPLACE statements to the source lines in pline. Returns the
  number of lines after the replacement has been performed.
*/
static int
print_replace_main (struct list_files *cfile, FILE *fd,
		    char *pline[CB_READ_AHEAD], int pline_cnt, int line_num)
{
	static int		active_replace_stmt = 0;
	char			*to_ptr;
	struct list_replace	*rep;
	struct list_files 	*cur;
	int    		i;
	const int	fixed = (cfile->source_format == CB_FORMAT_FIXED);
	const int	first_col = fixed ? CB_MARGIN_A : 0;
	int		is_copy_line;
	int		is_replace_line;
	int		is_replace_off = 0;
	char		tterm[2] = { '\0' };
	char		ttoken[CB_LINE_LENGTH + 2] = { '\0' };
	char		cmp_line[CB_LINE_LENGTH + 2] = { '\0' };

	if (is_comment_line (pline[0], cfile->source_format != CB_FORMAT_FREE)) {
		return pline_cnt;
	}

#ifdef DEBUG_REPLACE
	fprintf (stdout, "print_replace_main: line_num = %d\n", line_num);
	fprintf (stdout, "   pline_cnt = %d\n", pline_cnt);
	for (i = 0; i < pline_cnt; i++) {
		fprintf (stdout, "   pline[%2d]: %s\n", i, pline[i]);
	}
#endif

	compare_prepare (cmp_line, pline, 0, pline_cnt, first_col,
			 cfile->source_format != CB_FORMAT_FREE);

	/* Check whether we're given a COPY or REPLACE statement. */
	to_ptr = get_next_token (cmp_line, ttoken, tterm);
	is_copy_line = !cb_strcasecmp (ttoken, "COPY");
	is_replace_line = !cb_strcasecmp (ttoken, "REPLACE");
	if (is_replace_line && to_ptr) {
		to_ptr = get_next_token (to_ptr, ttoken, tterm);
		is_replace_off = !cb_strcasecmp (ttoken, "OFF");
	}

	/*
	  If no REPLACE is active, print nothing. If one is active, perform
	  replacements on the text.
	*/
	if (!active_replace_stmt && is_replace_line) {
		if (!is_replace_off) {
			active_replace_stmt = 1;
#ifdef DEBUG_REPLACE
			for (i = 0, rep = cfile->replace_head; rep; i++, rep = rep->next) {
				if (rep->firstline < (line_num + 10)) {
					if (i == 0)
						fprintf (stdout, "   replace_list: \n");
					fprintf (stdout, "      line[%d]: %d\n", i, rep->firstline);
					fprintf (stdout, "      from[%d]:%d: '%80.80s'\n", i, strlen(rep->from), rep->from);
					fprintf (stdout, "      to  [%d]:%d: '%80.80s'\n", i, strlen(rep->to), rep->to);
				}
			}
#endif
		}
	} else if (active_replace_stmt) {
		if (is_replace_line && is_replace_off) {
			active_replace_stmt = 0;
			remove_replace_entries_before_line (cfile, line_num);
		} else if (is_copy_line) {
			if (cfile->copy_head) {
				/* List all lines read so far and then discard them. */
				for (i = 0; i < pline_cnt; i++) {
					print_line (cfile, pline[i], line_num + i, 0);
					pline[i][0] = 0;
				}

				cur = cfile->copy_head;

				/* Print copybook, with REPLACE'd text. */
				if (!cur->replace_head) {
					for (rep = cfile->replace_head;
					     rep && rep->firstline <= line_num;
					     rep = rep->next) {
					        deep_copy_list_replace (rep, cur);
					}
				}
				print_program (cur, 1);

				/* Delete the copybook reference when done */
				cfile->copy_head = cur->next;
				cleanup_copybook_reference (cur);
			}
		} else {
			/* Print text with replacements */
			for (rep = cfile->replace_head;
			     rep && rep->firstline < line_num;
			     rep = rep->next) {
				pline_cnt = print_replace_text (cfile, fd, rep, pline,
								pline_cnt, line_num);
			}
		}
	}

	return pline_cnt;
}

/*
Print the listing for the file in cfile, with copybooks expanded and
after text has been REPLACE'd.

FIXME: this code doesn't check for huge replace values and will abort
       when these are used - see Bug #515
*/
static void
print_program_code (struct list_files *cfile, int in_copy)
{
	FILE			*fd = NULL;
	struct list_replace	*rep;
	struct list_files	*cur;
	struct list_error	*err;
	int	i;
	int	line_num = 1;
	const int	fixed = (cfile->source_format == CB_FORMAT_FIXED);
	int	eof = 0;
	int	pline_cnt = 0;
	char	*pline[CB_READ_AHEAD] = { NULL };
	int	lines_read;

	cfile->listing_on = 1;

#ifdef DEBUG_REPLACE
	struct list_skip *skip;

	fprintf (stdout, "print_program_code: in_copy = %s\n",
		in_copy ? "YES" : "NO");
	fprintf (stdout, "   name: %s\n", cfile->name);
	fprintf (stdout, "   copy_line: %d\n", cfile->copy_line);
	for (i = 0, cur = cfile->copy_head; cur; i++, cur = cur->next) {
		if (i == 0) {
			fprintf (stdout, "   copy_books: \n");
		}
		fprintf (stdout, "      name[%d]: %s\n", i, cur->name);
		fprintf (stdout, "      line[%d]: %d\n", i, cur->copy_line);
	}
	for (i = 0, rep = cfile->replace_head; rep; i++, rep = rep->next) {
		if (i == 0) {
			fprintf (stdout, "   replace_list: \n");
		}
		fprintf (stdout, "      line[%d]: %d\n", i, rep->firstline);
		fprintf (stdout, "      from[%d]:%d: '%80.80s'\n", i, strlen(rep->from), rep->from);
		fprintf (stdout, "      to  [%d]:%d: '%80.80s'\n", i, strlen(rep->to), rep->to);
	}
	for (i = 0, err = cfile->err_head; err; i++, err = err->next) {
		if (i == 0) {
			fprintf (stdout, "   error_list: \n");
		}
		fprintf (stdout, "      line[%d]: %d\n", i, err->line);
		fprintf (stdout, "      pref[%d]: '%s'\n", i, err->prefix);
		fprintf (stdout, "      msg [%d]: '%s'\n", i, err->msg);
	}
	for (i = 0, skip = cfile->skip_head; skip; i++, skip = skip->next) {
		if (i == 0) {
			fprintf (stdout, "   skip_list: \n");
		}
		fprintf (stdout, "      line[%d]: %d\n", i, skip->skipline);
	}
#endif

	if (cfile->name) {
		fd = fopen (cfile->name, "r");
	}
	if (fd != NULL) {
		abort_if_too_many_continuation_lines (pline_cnt, cfile->name, line_num);
		if (get_next_listing_line (fd, &pline[pline_cnt], fixed) >= 0) {
			do {
				abort_if_too_many_continuation_lines (pline_cnt, cfile->name, line_num);
				if (get_next_listing_line (fd, &pline[pline_cnt + 1], fixed) < 0) {
					eof = 1;
				}
				pline_cnt++;
				lines_read = 0;

				/* Collect all adjacent continuation lines */
				if (is_continuation_line (pline[fixed ? pline_cnt : pline_cnt - 1],
						  cfile->source_format != CB_FORMAT_FREE)) {
					continue;
				}
				/* handling for preprocessed directives */
				if (pline[0][0] == '#') {
					/* Set line number as specified by #line directive. */
					if (!strncmp (pline[0], "#line ", 6)) {
						line_num = atoi (&pline[0][6]);
						/* CHECKME: read the filename if given, too */
					}
					lines_read = -1;
				}

				/* Perform text replacement on the lines. */
				if (!in_copy) {
					pline_cnt = print_replace_main (cfile, fd, pline, pline_cnt,
						  line_num);
				} else if (cfile->replace_head) {
					rep = cfile->replace_head;
					while (rep) {
						pline_cnt = print_replace_text (cfile, fd, rep, pline,
							  pline_cnt, line_num);
						rep = rep->next;
					}
				}

				/* Print each line except the last. */
				for (i = 0; i < pline_cnt; i++) {
					if (pline[i][0]) {
						if (fixed && pline[i][CB_INDICATOR] == '&') {
							print_line (cfile, pline[i], line_num, in_copy);
						} else {
							print_line (cfile, pline[i], line_num + i, in_copy);
							lines_read++;
						}
					}
				}

				/* Output copybooks which are COPY'd at the current line */
				if (cfile->copy_head
				 && cfile->copy_head->copy_line == line_num) {

					cur = cfile->copy_head;

					/* Add the current text replacements to the copybook */
					for (rep = cfile->replace_head; rep && in_copy;
					     rep = rep->next) {
						deep_copy_list_replace (rep, cur);
					}
					print_program (cur, 1);

					/* Delete the copybook reference when done */
					cfile->copy_head = cur->next;
					cleanup_copybook_reference (cur);
				}

				/* Delete all but the last line. */
				strcpy (pline[0], pline[pline_cnt]);
				for (i = 1; i < pline_cnt + 1; i++) {
					memset (pline[i], 0, CB_LINE_LENGTH);
				}

				line_num += lines_read;
				pline_cnt = 0;
				if (pline[0][0] == 0) {
					eof = 1;
				}
			} while (!eof);
		}
		fclose (fd);

	/* Non-existent file, print errors to listing */
	} else {

		if (cfile->err_head) {
			for (err = cfile->err_head; err; err = err->next) {
				snprintf (print_data, CB_PRINT_LEN, "%s%s", err->prefix, err->msg);
				print_program_data (print_data);
			}
		}
	}

	for (i = 0; i < CB_READ_AHEAD; i++) {
		if (pline[i] == NULL) {
			break;
		}
		cobc_free (pline[i]);
	}
}

/*
  Print the listing for the file in cfile, with copybooks expanded and
  after text has been REPLACE'd.

  This function also frees contents of cfile's copy_head and replace_head
  members, then sets them to NULL.
*/
static void
print_program (struct list_files *cfile, int in_copy)
{
	struct list_error	*err;
	struct list_files	*cur;

	if (cb_listing_with_source) {
		/* actual printing of program code, copybooks included */
		print_program_code (cfile, in_copy);
	} else {
		/* Internal handling for copybooks (normally done within the source listing) */
		while (cfile->copy_head) {
			cur = cfile->copy_head;
			print_program (cur, 1);
			/* Delete the copybook reference when done */
			cfile->copy_head = cur->next;
			cleanup_copybook_reference (cur);
		}
	}
	/* Free replace data */
	if (cfile->replace_head) {
		free_replace_list (cfile->replace_head);
		cfile->replace_head = NULL;
	}

	/* Put errors on summary list */
	while (cfile->err_head) {
		err = cfile->err_head;
		cfile->err_head = err->next;
		if (cb_listing_error_tail) {
			cb_listing_error_tail->next = err;
		}
		if (!cb_listing_error_head) {
			cb_listing_error_head = err;
		}
		cb_listing_error_tail = err;
	}
}


/* Print the listing for the current file */
static void
print_program_listing (void)
{
	print_program (cb_listing_file_struct, 0);

	print_program_trailer ();

	/* TO-DO: Should this be here? */
	cobc_free ((void *)cb_listing_file_struct->name);
	cb_listing_file_struct->name = NULL;
}

/* Create single-element C source */

static int
process_translate (struct filename *fn)
{
	struct cb_program	*p;
	struct cb_program	*r;
	struct nested_list	*nlp;
	struct handler_struct	*hstr1;
	struct handler_struct	*hstr2;
	struct local_filename	*lf;
	int			ret;
	int			i;
	char	*buffer;

	/* Initialize */
	cb_source_file = NULL;
	cb_source_line = 0;

	/* Open the input file */
	yyin = fopen (fn->preprocess, "r");
	if (!yyin) {
		cobc_terminate (fn->preprocess);
	}

	if (verbose_output) {
		fputs (_("parsing:"), stderr);
		fprintf (stderr, "\t%s (%s)\n", fn->preprocess, fn->source);
		fflush (stderr);
	}

	current_program = NULL;
	cb_correct_program_order = 0;
	cb_source_file = fn->source;

	cb_init_constants ();

	/* Parse */
	ret = yyparse ();

	fclose (yyin);
	yyin = NULL;

	/* Release flex buffers - After file close */
	ylex_call_destroy ();

	output_return (ret);

	if (ret) {
		/* If processing raised errors set syntax-only flag to not
		   loose the information "no codegen occurred" */
		cb_flag_syntax_only = 1;
		return 1;
	}
	if (cb_flag_syntax_only) {
		return 0;
	}

	/* Set up USE GLOBAL handlers */
	for (p = current_program; p; p = p->next_program) {
		p->global_file_list = cb_list_reverse (p->global_file_list);
		if (p->nested_level) {
			for (r = p->next_program; r; r = r->next_program) {
				if (r->nested_level >= p->nested_level) {
					continue;
				}
				for (i = COB_OPEN_INPUT; i <= COB_OPEN_EXTEND; ++i) {
					hstr1 = &p->global_handler[i];
					hstr2 = &r->global_handler[i];
					if (!hstr1->handler_label &&
					    hstr2->handler_label &&
					    hstr2->handler_label->flag_global) {
						hstr1->handler_label = hstr2->handler_label;
						hstr1->handler_prog = r;
					}
				}
				if (!r->nested_level) {
					break;
				}
			}
		}
	}

	if (verbose_output) {
		fputs (_("translating:"), stderr);
		fprintf (stderr, "\t%s -> %s (%s)\n",
			 fn->preprocess, fn->translate, fn->source);
		fflush (stderr);
	}
	current_section = NULL;
	current_paragraph = NULL;
	current_statement = NULL;
	cb_source_line = 0;

	/* Open the output file */
	if (cb_unix_lf) {
		yyout = fopen (fn->translate, "wb");
	} else {
		yyout = fopen (fn->translate, "w");
	}
	if (!yyout) {
		cobc_terminate (fn->translate);
	}

	/* Open the common storage file */
	cb_storage_file_name = cobc_main_strdup (fn->trstorage);
	if (cb_unix_lf) {
		cb_storage_file = fopen (cb_storage_file_name, "wb");
	} else {
		cb_storage_file = fopen (cb_storage_file_name, "w");
	}
	if (!cb_storage_file) {
		cobc_terminate (cb_storage_file_name);
	}
	/* remove possible path from header name for later codegen */
	if (strrchr (cb_storage_file_name, '/')
	 || strrchr (cb_storage_file_name, '\\')) {
		buffer = file_basename (cb_storage_file_name, COB_BASENAME_KEEP_EXT);
		memcpy ((void *) cb_storage_file_name, (void *) buffer, strlen (buffer) + 1);
	}

	/* Process programs in original order */
	restore_program_list_order ();

	/* Set up local storage files */
	lf = NULL;
	ret = 1;
	for (p = current_program; p; p = p->next_program, ret++) {
		lf = cobc_main_malloc (sizeof(struct local_filename));
		lf->local_name = cobc_main_malloc (fn->translate_len + 12U);
#ifndef HAVE_8DOT3_FILENAMES
		if (p == current_program && !p->next_program) {
			sprintf (lf->local_name, "%s.l.h", fn->translate);
		} else {
			sprintf (lf->local_name, "%s.l%d.h", fn->translate, ret);
		}
#else
		/* for 8.3 filenames use no ".c" prefix and only one period */
		buffer = cobc_strdup (fn->translate);
		*(buffer + strlen(buffer) - 2) = 'l';
		*(buffer + strlen(buffer) - 1) = 0;
		if (p == current_program && !p->next_program) {
			sprintf (lf->local_name, "%s.h", buffer);
		} else {
			sprintf (lf->local_name, "%s%d.h", buffer, ret);
		}
		cobc_free (buffer);
#endif
		if (cb_unix_lf) {
			lf->local_fp = fopen (lf->local_name, "wb");
		} else {
			lf->local_fp = fopen (lf->local_name, "w");
		}
		if (!lf->local_fp) {
			cobc_terminate (lf->local_name);
		}
		/* remove possible path from header name for later codegen */
		lf->local_include_name = cobc_main_strdup (file_basename (lf->local_name, COB_BASENAME_KEEP_EXT));
		p->local_include = lf;
		lf->next = fn->localfile;
		fn->localfile = lf;
	}

	/* Entries for COMMON programs */
	for (p = current_program; p; p = p->next_program) {
		i = p->nested_level;
		for (nlp = p->common_prog_list; nlp; nlp = nlp->next) {
			for (r = p->next_program; r; r = r->next_program) {
				if (r->nested_level <= i) {
					break;
				}
				cb_insert_common_prog (r, nlp->nested_prog);
			}
		}
	}

	/* Translate to C */
	current_section = NULL;
	current_paragraph = NULL;
	current_statement = NULL;
	cb_source_line = 0;
	/* Temporarily disable cross-reference during C generation */
	if (cb_listing_xref) {
		cb_listing_xref = 0;
		codegen (current_program, fn->translate, 0);
		cb_listing_xref = 1;
	} else {
		codegen (current_program, fn->translate, 0);
	}

	/* Close files */
	if (fclose (cb_storage_file) != 0) {
		cobc_terminate (fn->trstorage);
	}
	cb_storage_file = NULL;
	if (fclose (yyout) != 0) {
		cobc_terminate (fn->translate);
	}
	yyout = NULL;
	for (p = current_program; p; p = p->next_program) {
		if (!p->local_include->local_fp) {
			continue;
		}
		if (fclose (p->local_include->local_fp) != 0) {
			cobc_terminate(lf->local_name);
		}
		p->local_include->local_fp = NULL;
	}
	return !!errorcount;
}

/* Create single-element assembly source */

static int
process_compile (struct filename *fn)
{
	char	*name;
	size_t	bufflen;
	size_t	size;

	if (output_name) {
		name = output_name;
	} else {
		name = file_basename (fn->source, NULL);
#ifndef	_MSC_VER
		strcat (name, ".s");
#endif
	}
	size = strlen (name);
#ifdef	_MSC_VER
	size *= 2U;
#endif

	bufflen = cobc_cc_len + cobc_cflags_len
			+ size + fn->translate_len
			+ cobc_include_len + 64U;

	cobc_chk_buff_size (bufflen);

#ifdef	_MSC_VER
	sprintf (cobc_buffer, cb_source_debugging ?
		"%s /c %s %s /Od /MDd /Zi /FR /c /Fa\"%s\" /Fo\"%s\" \"%s\"" :
		"%s /c %s %s     /MD          /c /Fa\"%s\" /Fo\"%s\" \"%s\"",
			cobc_cc, cobc_cflags, cobc_include, name,
			name, fn->translate);
	if (verbose_output > 1) {
		return process (cobc_buffer);
	} else {
		return process_filtered (cobc_buffer, fn);
	}
#elif defined(__WATCOMC__)
	sprintf (cobc_buffer, "%s -fe=\"%s\" -s %s %s %s", cobc_cc, name,
			cobc_cflags, cobc_include, fn->translate);
	return process (cobc_buffer);
#else
	/* TODO: check ORANGEC options */
	if (!cb_flag_main) {
		sprintf (cobc_buffer, "%s -S -o \"%s\" %s %s %s \"%s\"", cobc_cc, name,
			cobc_cflags, cobc_include, COB_PIC_FLAGS, fn->translate);
	} else {
		sprintf (cobc_buffer, "%s -S -o \"%s\" %s %s \"%s\"", cobc_cc, name,
			cobc_cflags, cobc_include, fn->translate);
	}
	return process(cobc_buffer);
#endif
}

/* Create single-element assembled object */

static int
process_assemble (struct filename *fn)
{
#ifndef _MSC_VER
	int		ret;
#endif
	size_t		bufflen;
#ifdef	__OS400__
	char	*name;
#endif

	bufflen = cobc_cc_len + cobc_cflags_len + fn->object_len
			+ fn->translate_len + cobc_include_len
#ifndef	__OS400__
			+ cobc_pic_flags_len
#endif
			+ 64U;

	cobc_chk_buff_size (bufflen);

#ifdef	_MSC_VER
	sprintf (cobc_buffer, cb_source_debugging ?
		"%s /c %s %s /Od /MDd /Zi /FR /Fo\"%s\" \"%s\"" :
		"%s /c %s %s     /MD          /Fo\"%s\" \"%s\"",
			cobc_cc, cobc_cflags, cobc_include,
			fn->object, fn->translate);
	if (verbose_output > 1) {
		return process (cobc_buffer);
	} else {
		return process_filtered (cobc_buffer, fn);
	}
#elif defined(__OS400__)
	file_stripext ((char *) fn->object);
	sprintf (cobc_buffer, "%s -c %s %s -o %s %s",
		 cobc_cc, cobc_cflags, cobc_include,
		 fn->object, fn->translate);
	ret = process (cobc_buffer);
	return ret;
#elif defined(__WATCOMC__)
	if (cb_compile_level == CB_LEVEL_MODULE ||
	    cb_compile_level == CB_LEVEL_LIBRARY) {
		sprintf (cobc_buffer, "%s -c %s %s %s -fe=\"%s\" \"%s\"",
			 cobc_cc, cobc_cflags, cobc_include,
			 COB_PIC_FLAGS, fn->object, fn->translate);
	} else {
		sprintf (cobc_buffer, "%s -c %s %s -fe=\"%s\" \"%s\"",
			 cobc_cc, cobc_cflags, cobc_include,
			 fn->object, fn->translate);
	}
	ret = process (cobc_buffer);
	return ret;
#else
	if (cb_compile_level == CB_LEVEL_MODULE ||
	    cb_compile_level == CB_LEVEL_LIBRARY ||
	    cb_compile_level == CB_LEVEL_ASSEMBLE) {
		sprintf (cobc_buffer, "%s -c %s %s %s -o \"%s\" \"%s\"",
			 cobc_cc, cobc_cflags, cobc_include,
			 COB_PIC_FLAGS, fn->object, fn->translate);
	} else {
		/* Only for CB_LEVEL_EXECUTABLE */
		sprintf (cobc_buffer, "%s -c %s %s -o \"%s\" \"%s\"",
			 cobc_cc, cobc_cflags, cobc_include,
			 fn->object, fn->translate);
	}
	ret = process (cobc_buffer);
	return ret;
#endif

}

/* Create single-element loadable object (as module)
   without intermediate stages */
static int
process_module_direct (struct filename *fn)
{
	char	*name;
#ifdef	_MSC_VER
	char	*exe_name;
#endif
	size_t	bufflen;
	size_t	size;
	int	ret;

	if (output_name) {
		name = output_name_buff;
		strcpy (name, output_name);
#if	defined (_MSC_VER) \
 || defined (__WATCOMC__) || defined (__BORLANDC__) \
 || defined (__OS400__)
		file_stripext (name);
#else
		if (strchr (output_name, '.') == NULL) {
			strcat (name, "." COB_MODULE_EXT);
		}
#endif
	} else {
		name = file_basename (fn->source, NULL);
#if	!defined (_MSC_VER) \
 && !defined (__WATCOMC__) && !defined (__BORLANDC__) \
 && !defined (__OS400__)
		strcat (name, "." COB_MODULE_EXT);
#endif
	}
#ifdef	_MSC_VER
	exe_name = cobc_stradd_dup (name, "." COB_MODULE_EXT);
#endif
#ifdef	__OS400__
	/* OS400: compilation needs full path so add it in front of transation name
	   CHECKME: is this true? Then that code should be moved out and also called in
	            the other process_xyz functions */
	if (fn->translate[0] != SLASH_CHAR) {
		char *p;

		p = cobc_main_malloc (COB_LARGE_BUFF);
		getcwd (p, COB_LARGE_BUFF);

		strcat (p, SLASH_STR);
		strcat (p, fn->translate);
		fn->translate = p;
		fn->translate_len = strlen (p);
	}
#endif

	size = strlen (name);
#ifdef	_MSC_VER
	size *= 2U;
#endif

	bufflen = cobc_cc_len + cobc_cflags_len
			+ cobc_include_len + cobc_shared_opt_len
			+ cobc_pic_flags_len + cobc_export_dyn_len
			+ size + fn->translate_len
#ifdef	_MSC_VER
			+ manilink_len
#endif
			+ cobc_ldflags_len + cobc_lib_paths_len + cobc_libs_len
			+ 128U;

	cobc_chk_buff_size (bufflen);

#ifndef	_MSC_VER
#ifdef	__WATCOMC__
	sprintf (cobc_buffer, "%s %s %s %s %s %s -fe=\"%s\" \"%s\" %s %s %s",
		 cobc_cc, cobc_cflags, cobc_include, COB_SHARED_OPT,
		 COB_PIC_FLAGS, COB_EXPORT_DYN, name,
		 fn->translate, cobc_ldflags, cobc_lib_paths, cobc_libs);
#else
	sprintf (cobc_buffer, "%s %s %s %s %s %s -o \"%s\" \"%s\" %s %s %s",
		 cobc_cc, cobc_cflags, cobc_include, COB_SHARED_OPT,
		 COB_PIC_FLAGS, COB_EXPORT_DYN, name,
		 fn->translate, cobc_ldflags, cobc_lib_paths, cobc_libs);
#endif
	ret = process (cobc_buffer);
#ifdef	COB_STRIP_CMD
	if (strip_output && ret == 0) {
		cobc_chk_buff_size (strlen (COB_STRIP_CMD) + 4 + strlen (name));
		sprintf (cobc_buffer, "%s \"%s\"", COB_STRIP_CMD, name);
		ret = process (cobc_buffer);
	}
#endif
#else	/* _MSC_VER */
	sprintf (cobc_buffer, cb_source_debugging ?
		"%s %s %s /Od /MDd /LDd /Zi /FR /Fe\"%s\" /Fo\"%s\" \"%s\" %s %s %s %s" :
		"%s %s %s     /MD  /LD          /Fe\"%s\" /Fo\"%s\" \"%s\" %s %s %s %s",
			cobc_cc, cobc_cflags, cobc_include, exe_name, name,
			fn->translate,
			manilink, cobc_ldflags, cobc_lib_paths, cobc_libs);
	if (verbose_output > 1) {
		ret = process (cobc_buffer);
	} else {
		ret = process_filtered (cobc_buffer, fn);
	}
	/* Embedding manifest */
	if (ret == 0) {
		sprintf (cobc_buffer,
			 "%s /manifest \"%s.manifest\" /outputresource:\"%s\";#2",
			 manicmd, exe_name, exe_name);
		ret = process (cobc_buffer);
		sprintf (cobc_buffer, "%s.manifest", exe_name);
		cobc_check_action (cobc_buffer);
	}
	cobc_free ((void *) exe_name);
	sprintf (cobc_buffer, "%s.exp", name);
	cobc_check_action (cobc_buffer);
	sprintf (cobc_buffer, "%s.lib", name);
	if (strstr (fn->source, cobc_buffer) == NULL)	cobc_check_action (cobc_buffer);
	sprintf (cobc_buffer, "%s.%s", name, COB_OBJECT_EXT);
	if (strstr (fn->source, cobc_buffer) == NULL)	cobc_check_action (cobc_buffer);
#endif
	return ret;
}

/* Create single-element loadable object */

static int
process_module (struct filename *fn)
{
	char	*name;
#ifdef	_MSC_VER
	char	*exe_name;
#endif
	size_t	bufflen;
	size_t	size;
	int	ret;

	if (output_name) {
		name = output_name_buff;
		strcpy (name, output_name);
#if	defined (_MSC_VER) \
 || defined (__WATCOMC__) || defined (__BORLANDC__) \
 || defined (__OS400__)
		file_stripext (name);
#else
		if (strchr (output_name, '.') == NULL) {
			strcat (name, "." COB_MODULE_EXT);
		}
#endif
	} else {
		name = file_basename (fn->source, NULL);
#if	!defined (_MSC_VER) \
 && !defined (__WATCOMC__) && !defined (__BORLANDC__) \
 && !defined (__OS400__)
		strcat (name, "." COB_MODULE_EXT);
#endif
	}
#ifdef	_MSC_VER
	exe_name = cobc_stradd_dup (name, "." COB_MODULE_EXT);
#endif

	size = strlen (name);
	bufflen = cobc_cc_len + cobc_shared_opt_len
			+ cobc_pic_flags_len + cobc_export_dyn_len
			+ size + fn->object_len
#ifdef	_MSC_VER
			+ manilink_len
#endif
			+ cobc_ldflags_len + cobc_lib_paths_len + cobc_libs_len
			+ 128U;

	cobc_chk_buff_size (bufflen);

#ifndef	_MSC_VER
#ifdef	__WATCOMC__
	sprintf (cobc_buffer, "%s %s %s %s -fe=\"%s\" \"%s\" %s %s %s",
		 cobc_cc, COB_SHARED_OPT, COB_PIC_FLAGS, COB_EXPORT_DYN,
		 name, fn->object, cobc_ldflags, cobc_lib_paths, cobc_libs);
#else
	sprintf (cobc_buffer, "%s %s %s %s -o \"%s\" \"%s\" %s %s %s",
		 cobc_cc, COB_SHARED_OPT, COB_PIC_FLAGS, COB_EXPORT_DYN,
		 name, fn->object, cobc_ldflags, cobc_lib_paths, cobc_libs);
#endif
	ret = process (cobc_buffer);
#ifdef	COB_STRIP_CMD
	if (strip_output && ret == 0) {
		cobc_chk_buff_size (strlen (COB_STRIP_CMD) + 4 + strlen (name));
		sprintf (cobc_buffer, "%s \"%s\"", COB_STRIP_CMD, name);
		ret = process (cobc_buffer);
	}
#endif
#else	/* _MSC_VER */
	sprintf (cobc_buffer, cb_source_debugging ?
		"%s /Od /MDd /LDd /Zi /FR /Fe\"%s\" \"%s\" %s %s %s %s" :
		"%s     /MD  /LD          /Fe\"%s\" \"%s\" %s %s %s %s",
		cobc_cc, exe_name, fn->object,
		manilink, cobc_ldflags, cobc_libs, cobc_lib_paths);
	if (verbose_output > 1) {
		ret = process (cobc_buffer);
	} else {
		ret = process_filtered (cobc_buffer, fn);
	}
	/* Embedding manifest */
	if (ret == 0) {
		sprintf (cobc_buffer,
			 "%s /manifest \"%s.manifest\" /outputresource:\"%s\";#2",
			 manicmd, exe_name, exe_name);
		ret = process (cobc_buffer);
		sprintf (cobc_buffer, "%s.manifest", exe_name);
		cobc_check_action (cobc_buffer);
	}
	cobc_free ((void *) exe_name);
	sprintf (cobc_buffer, "%s.exp", name);
	cobc_check_action (cobc_buffer);
	sprintf (cobc_buffer, "%s.lib", name);
	if (strstr (fn->source, cobc_buffer) == NULL)	cobc_check_action (cobc_buffer);
	sprintf (cobc_buffer, "%s.obj", name);
	if (strstr (fn->source, cobc_buffer) == NULL)	cobc_check_action (cobc_buffer);
#endif
	return ret;
}

/* Create multi-element loadable object */

static int
process_library (struct filename *l)
{
	struct filename	*f;
	char		*name;
#ifdef	_MSC_VER
	char	*exe_name;
#endif
	size_t		bufflen;
	size_t		size;
	int		ret;

	/* LCOV_EXCL_START */
	if (!l) {
		cobc_err_msg (_("call to '%s' with invalid parameter '%s'"),
			"process_library", "l");
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */

	for (f = l; f; f = f->next) {
		strcat (cobc_objects_buffer, "\"");
		strcat (cobc_objects_buffer, f->object);
		strcat (cobc_objects_buffer, "\" ");
	}

	if (output_name) {
		name = output_name_buff;
		strcpy (name, output_name);
#if	defined (_MSC_VER) \
 || defined (__WATCOMC__) || defined (__BORLANDC__) \
 || defined (__OS400__)
		file_stripext (name);
#else
		if (strchr (output_name, '.') == NULL) {
			strcat (name, "." COB_MODULE_EXT);
		}
#endif
	} else {
		name = file_basename (l->source, NULL);
#if	!defined (_MSC_VER) \
 && !defined (__WATCOMC__) && !defined (__BORLANDC__) \
 && !defined (__OS400__)
		strcat (name, "." COB_MODULE_EXT);
#endif
	}
#ifdef	_MSC_VER
	exe_name = cobc_stradd_dup (name, "." COB_MODULE_EXT);
#endif

	size = strlen (name);
	bufflen = cobc_cc_len + cobc_shared_opt_len
			+ cobc_pic_flags_len + cobc_export_dyn_len
			+ size + cobc_objects_len + cobc_libs_len
#ifdef	_MSC_VER
			+ manilink_len
#endif
			+ cobc_ldflags_len + cobc_lib_paths_len
			+ 64U;

	cobc_chk_buff_size (bufflen);

#ifndef	_MSC_VER
#ifdef	__WATCOMC__
	sprintf (cobc_buffer, "%s %s %s %s -fe=\"%s\" %s %s %s %s",
		 cobc_cc, COB_SHARED_OPT, COB_PIC_FLAGS,
		 COB_EXPORT_DYN, name, cobc_objects_buffer,
		 cobc_ldflags, cobc_lib_paths, cobc_libs);
#else
	sprintf (cobc_buffer, "%s %s %s %s -o \"%s\" %s %s %s %s",
		 cobc_cc, COB_SHARED_OPT, COB_PIC_FLAGS,
		 COB_EXPORT_DYN, name, cobc_objects_buffer,
		 cobc_ldflags, cobc_lib_paths, cobc_libs);
#endif
	ret = process (cobc_buffer);
#ifdef	COB_STRIP_CMD
	if (strip_output && ret == 0) {
		cobc_chk_buff_size (strlen (COB_STRIP_CMD) + 4 + strlen (name));
		sprintf (cobc_buffer, "%s \"%s\"", COB_STRIP_CMD, name);
		ret = process (cobc_buffer);
	}
#endif
#else	/* _MSC_VER */
	sprintf (cobc_buffer, cb_source_debugging ?
		"%s /Od /MDd /LDd /Zi /FR /Fe\"%s\" %s %s %s %s %s" :
		"%s     /MD  /LD          /Fe\"%s\" %s %s %s %s %s",
		cobc_cc, exe_name, cobc_objects_buffer,
		manilink, cobc_ldflags, cobc_lib_paths, cobc_libs);
	if (verbose_output > 1) {
		ret = process (cobc_buffer);
	} else {
		ret = process_filtered (cobc_buffer, l);
	}
	/* Embedding manifest */
	if (ret == 0) {
		sprintf (cobc_buffer,
			 "%s /manifest \"%s.manifest\" /outputresource:\"%s\";#2",
			 manicmd, exe_name, exe_name);
		ret = process (cobc_buffer);
		sprintf (cobc_buffer, "%s.manifest", exe_name);
		cobc_check_action (cobc_buffer);
	}
	cobc_free ((void *) exe_name);
	sprintf (cobc_buffer, "%s.exp", name);
	cobc_check_action (cobc_buffer);
	sprintf (cobc_buffer, "%s.lib", name);

	for (f = l; f; f = f->next) {
		if (strstr (f->source, cobc_buffer) != NULL) {
			break;
		}
	}
	if (!f)	cobc_check_action (cobc_buffer);
#endif
	return ret;
}

/* Create executable */

static int
process_link (struct filename *l)
{
	struct filename	*f;
	const char		*name;
#if defined(_WIN32) || defined(__CYGWIN__) || defined (COB_STRIP_CMD)
	const char		*exe_name;
#endif
	size_t		bufflen;
	size_t		size;
	int		ret;

	/* LCOV_EXCL_START */
	if (!l) {
		cobc_err_msg (_("call to '%s' with invalid parameter '%s'"),
			"process_link", "l");
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */

	for (f = l; f; f = f->next) {
#ifdef	__OS400__
		file_stripext ((char *) f->object);
		strcat (cobc_objects_buffer, f->object);
		strcat (cobc_objects_buffer, " ");
#else
		strcat (cobc_objects_buffer, "\"");
		strcat (cobc_objects_buffer, f->object);
		strcat (cobc_objects_buffer, "\" ");
#endif
	}

	if (output_name) {
#if	defined (_WIN32)      || defined (__CYGWIN__) \
 || defined (__WATCOMC__) || defined (__BORLANDC__) \
 || defined (__OS400__)
		name = cobc_main_strdup (output_name);
		file_stripext ((char *)name);
#else
		name = output_name;
#endif
	} else {
		if (l->file_is_stdin) {
			name = COB_DASH_OUT;
		} else {
			name = file_basename (l->source, NULL);
		}
	}
#if	defined (_WIN32) || defined (__CYGWIN__)
	exe_name = cobc_stradd_dup (name, COB_EXE_EXT);
#ifndef _MSC_VER
	name = exe_name;
#endif
#endif

	size = strlen (name);
	bufflen = cobc_cc_len + cobc_export_dyn_len
			+ size + cobc_objects_len
#ifdef	_MSC_VER
			+ manilink_len
#endif
			+ cobc_ldflags_len + cobc_libs_len + cobc_lib_paths_len
			+ 64U;

	cobc_chk_buff_size (bufflen);

#ifndef	_MSC_VER
#ifdef	__WATCOMC__
	sprintf (cobc_buffer, "%s %s -fe=\"%s\" %s %s %s %s",
		 cobc_cc, COB_EXPORT_DYN, name, cobc_objects_buffer,
		 cobc_ldflags, cobc_lib_paths, cobc_libs);
#else
	sprintf (cobc_buffer, "%s %s -o \"%s\" %s %s %s %s",
		 cobc_cc, COB_EXPORT_DYN, name, cobc_objects_buffer,
		 cobc_ldflags, cobc_lib_paths, cobc_libs);
#endif
	ret = process (cobc_buffer);

#ifdef	__hpux
	if (ret == 0) {
		sprintf (cobc_buffer, "chatr -s +s enable \"%s%s\" 1>/dev/null 2>&1",
			 name, COB_EXE_EXT);
		process (cobc_buffer);
	}
#endif

#ifdef	COB_STRIP_CMD
	if (strip_output && ret == 0) {
		const char *exe_ext = COB_EXE_EXT;
		if (*exe_ext) {
			exe_ext++; /* drop the "." */
		}
		cobc_chk_buff_size (strlen (COB_STRIP_CMD) + 4 + strlen (name) + strlen (COB_EXE_EXT));
		/* only add COB_EXE_EXT if it is not specified */
		exe_name = file_extension (name);
		if (cb_strcasecmp (exe_name, exe_ext)) {
			sprintf (cobc_buffer, "%s \"%s%s\"",
				 COB_STRIP_CMD, name, COB_EXE_EXT);
		} else {
			sprintf (cobc_buffer, "%s \"%s\"",
				 COB_STRIP_CMD, name);
		}
		ret = process (cobc_buffer);
	}
#endif
#else	/* _MSC_VER */
	sprintf (cobc_buffer, cb_source_debugging ?
		"%s /Od /MDd /Zi /FR /Fe\"%s\" %s %s %s %s %s" :
		"%s     /MD          /Fe\"%s\" %s %s %s %s %s",
		cobc_cc, exe_name, cobc_objects_buffer,
		manilink, cobc_ldflags, cobc_lib_paths, cobc_libs);
	if (verbose_output > 1) {
		ret = process (cobc_buffer);
	} else {
		ret = process_filtered (cobc_buffer, l);
	}
	/* Embedding manifest */
	if (ret == 0) {
		sprintf (cobc_buffer,
			 "%s /manifest \"%s.manifest\" /outputresource:\"%s\";#1",
			 manicmd, exe_name, exe_name);
		ret = process (cobc_buffer);
		sprintf (cobc_buffer, "%s.manifest", exe_name);
		cobc_check_action (cobc_buffer);
	}
	cobc_free ((void *) exe_name);
#endif
	return ret;
}

/* Set up build time stamp */
static void
set_const_cobc_build_stamp (void)
{
	int			year;
	int			day;
	char		month[32];

	memset (month, 0, sizeof(month));
	day = 0;
	year = 0;
	if (sscanf (__DATE__, "%s %d %d", month, &day, &year) == 3) {
		snprintf (cobc_buffer, (size_t)COB_MINI_MAX,
			"%s %2.2d %4.4d %s", month, day, year, __TIME__);
	} else {
		snprintf (cobc_buffer, (size_t)COB_MINI_MAX,
			"%s %s", __DATE__, __TIME__);
	}
	cb_cobc_build_stamp = (const char *)cobc_main_strdup (cobc_buffer);
}

/* Set up compiler defaults from environment/builtin */
static void
set_cobc_defaults (void)
{
	char			*p;

	cobc_cc = cobc_getenv_path ("COB_CC");
	if (cobc_cc == NULL ) {
		cobc_cc = COB_CC;
	}

	cob_config_dir = (const char *) cobc_getenv_path ("COB_CONFIG_DIR");
	if (cob_config_dir == NULL) {
		cob_config_dir = cob_getenv_value ("COB_CONFIG_DIR");
	}
	cob_schema_dir = (const char *) cobc_getenv_path ("COB_SCHEMA_DIR");
	if (cob_schema_dir == NULL) {
		cob_schema_dir = cob_getenv_value ("COB_SCHEMA_DIR");
	}

	p = cobc_getenv ("COB_CFLAGS");
	if (p) {
		COBC_ADD_STR (cobc_cflags, p, NULL, NULL);
	} else {
		COBC_ADD_STR (cobc_cflags, cob_relocate_string (COB_CFLAGS), NULL, NULL);
	}

	p = cobc_getenv ("COB_LDFLAGS");
	if (p) {
		COBC_ADD_STR (cobc_ldflags, p, NULL, NULL);
	} else {
		COBC_ADD_STR (cobc_ldflags, cob_relocate_string (COB_LDFLAGS), NULL, NULL);
	}

#ifdef COB_DEBUG_FLAGS
	p = cobc_getenv ("COB_DEBUG_FLAGS");
	if (p && *p) {
		cobc_debug_flags = (const char *)p;
	} else {
		cobc_debug_flags = COB_DEBUG_FLAGS;
	}
#endif

	p = cobc_getenv ("COB_LIBS");
	if (p) {
		COBC_ADD_STR (cobc_libs, p, NULL, NULL);
	} else {
		COBC_ADD_STR (cobc_libs, cob_relocate_string (COB_LIBS), NULL, NULL);
	}

	p = cobc_getenv ("COB_LDADD");
	if (p) {
		COBC_ADD_STR (cobc_libs, " ", p, NULL);
	}

	p = cobc_getenv ("COB_LIB_PATHS");
	if (p) {
		COBC_ADD_STR (cobc_lib_paths, p, NULL, NULL);
	} else {
		COBC_ADD_STR (cobc_lib_paths, " ", NULL, NULL);
	}

	/* Different styles for warning/error messages */
	p = cobc_getenv ("COB_MSG_FORMAT");
#if defined (_MSC_VER)
	if (p && cb_strcasecmp (p, "GCC") == 0) {
		cb_msg_style = CB_MSG_STYLE_GCC;
	} else {
		cb_msg_style = CB_MSG_STYLE_MSC;
	}
#else
	if (p && cb_strcasecmp (p, "MSC") == 0) {
		cb_msg_style = CB_MSG_STYLE_MSC;
	} else {
		cb_msg_style = CB_MSG_STYLE_GCC;
	}
#endif
	p = cobc_getenv ("COB_UNIX_LF");
	if (p
	 &&	(*p == 'Y' || *p == 'y' ||
		 *p == 'O' || *p == 'o' ||
		 *p == 'T' || *p == 't' ||
		 *p == '1')) {
		cb_unix_lf = 1;
	}
}

/* Setup for the C compiler/linker */
static void
begin_setup_compiler_env (void)
{
	cobc_libs = cobc_main_malloc ((size_t)COB_SMALL_BUFF);
	cobc_lib_paths = cobc_main_malloc ((size_t)COB_SMALL_BUFF);
	cobc_cflags = cobc_main_malloc ((size_t)COB_MINI_BUFF);
	cobc_ldflags = cobc_main_malloc ((size_t)COB_MINI_BUFF);
	cobc_include = cobc_main_malloc ((size_t)COB_MINI_BUFF);

	cobc_libs_size = COB_SMALL_MAX;
	cobc_lib_paths_size = COB_SMALL_MAX;
	cobc_cflags_size = COB_MINI_MAX;
	cobc_include_size = COB_MINI_MAX;
	cobc_ldflags_size = COB_MINI_MAX;

	cobc_objects_len = 0;
}

/* Setup for the C compiler/linker */
static void
finish_setup_compiler_env (void)
{
	/* compiler specific options for (non/very) verbose output */
#if defined(__GNUC__) || defined(__TINYC__)
	if (verbose_output > 1) {
		COBC_ADD_STR (cobc_cflags,  " -v", NULL, NULL);
#if	!defined (__INTEL_COMPILER) && !defined(__TINYC__)
		if (verbose_output > 2) {
			COBC_ADD_STR (cobc_ldflags, " -t", NULL, NULL);
		}
#endif
	}
#elif defined(_MSC_VER)
	/* MSC stuff reliant upon verbose option */
	switch (verbose_output) {
	case 0:
	/* -v */
	case 1:
		COBC_ADD_STR (cobc_cflags, " /nologo", NULL, NULL);
		manicmd = "mt /nologo";
		manilink = "/link /manifest /nologo";
		break;
	/* -vv */
	case 2:
		manicmd = "mt";
		manilink = "/link /manifest";
		break;
	/* -vvv */
	default:
		manicmd = "mt /verbose";
		manilink = "/link /manifest /verbose";
	}
	manilink_len = strlen (manilink);
#elif defined(__ORANGEC__)
	if (verbose_output <= 1) {
		COBC_ADD_STR (cobc_cflags,  " --nologo", NULL, NULL);
		COBC_ADD_STR (cobc_ldflags, " --nologo", NULL, NULL);
	} else {
		COBC_ADD_STR (cobc_cflags, " -yy", NULL, NULL);
		COBC_ADD_STR (cobc_ldflags, " -yy", NULL, NULL);
	}
#elif defined(__WATCOMC__)
	if (verbose_output < 2) {
		COBC_ADD_STR (cobc_cflags, " -q", NULL, NULL);
	}
#endif

	/* Set length of compiler strings */
	cobc_cc_len = strlen (cobc_cc);
	cobc_cflags_len = strlen (cobc_cflags);
	cobc_include_len = strlen (cobc_include);
#ifdef COB_SHARED_OPT
	cobc_shared_opt_len = strlen (COB_SHARED_OPT);
#endif
#ifdef COB_PIC_FLAGS
	cobc_pic_flags_len = strlen (COB_PIC_FLAGS);
#endif
#ifdef COB_EXPORT_DYN
	cobc_export_dyn_len = strlen (COB_EXPORT_DYN);
#endif
	cobc_ldflags_len = strlen (cobc_ldflags);
	cobc_lib_paths_len = strlen (cobc_lib_paths);
	cobc_libs_len = strlen (cobc_libs);
}


static void
begin_setup_internal_and_compiler_env (void)
{
	/* register signal handlers from cobc */
	cob_reg_sighnd (&cobc_sig_handler);

	file_list = NULL;
	cb_listing_file = NULL;
	cb_src_list_file = NULL;
	ppin = NULL;
	ppout = NULL;
	yyin = NULL;
	yyout = NULL;

	/* General buffers */
	cobc_buffer = cobc_main_malloc ((size_t)COB_LARGE_BUFF);
	cobc_buffer_size = COB_LARGE_MAX;
	basename_buffer = cobc_main_malloc ((size_t)COB_MINI_BUFF);
	basename_len = COB_MINI_MAX - 16;

	cb_source_file = NULL;
	save_temps_dir = NULL;
	base_string = NULL;
	cb_id = 1;
	cb_pic_id = 1;
	cb_attr_id = 1;
	cb_literal_id = 1;
	cb_field_id = 1;
#ifdef	COB_EBCDIC_MACHINE
	cb_ebcdic_sign = 1;
#else
	cb_ebcdic_sign = 0;
#endif

#ifdef	HAVE_SETLOCALE
	setlocale (LC_ALL, "");
	setlocale (LC_NUMERIC, "C");
#endif

	/* initial values for warning options */
#define	CB_WARNDEF(opt,name,doc)	cb_warn_opt_val[opt] = COBC_WARN_DISABLED;
#define	CB_ONWARNDEF(opt,name,doc)	cb_warn_opt_val[opt] = COBC_WARN_ENABLED;
#define	CB_NOWARNDEF(opt,name,doc)	cb_warn_opt_val[opt] = COBC_WARN_DISABLED;
#define	CB_ERRWARNDEF(opt,name,doc)	cb_warn_opt_val[opt] = COBC_WARN_ENABLED;
#include "warning.def"
#undef	CB_WARNDEF
#undef	CB_ONWARNDEF
#undef	CB_NOWARNDEF
#undef	CB_ERRWARNDEF

	/* minimal initialization of the environment like binding textdomain,
	   allowing test to be run under WIN32 (implied in cob_init(),
	   no need to call outside of GnuCOBOL) */
	cob_common_init (NULL);

	/* Initialize variables */
	begin_setup_compiler_env ();

	set_const_cobc_build_stamp();
	set_cobc_defaults();

	output_name = NULL;

	/* Set default computed goto usage if appropriate */
#if	defined(COB_COMPUTED_GOTO) && COB_COMPUTED_GOTO
	cb_flag_computed_goto = 1;
#endif

	/* Enable default I/O exceptions */
	CB_EXCEPTION_ENABLE (COB_EC_I_O) = 1;

#ifndef	HAVE_DESIGNATED_INITS
	cobc_init_reserved ();
#endif
}


static void
finish_setup_internal_env (void)
{
#ifndef	HAVE_DESIGNATED_INITS
	cobc_init_typeck ();
#endif

	/* Append default extensions */
	CB_TEXT_LIST_ADD (cb_extension_list, ".CPY");
	CB_TEXT_LIST_ADD (cb_extension_list, ".CBL");
	CB_TEXT_LIST_ADD (cb_extension_list, ".COB");
	CB_TEXT_LIST_ADD (cb_extension_list, ".cpy");
	CB_TEXT_LIST_ADD (cb_extension_list, ".cbl");
	CB_TEXT_LIST_ADD (cb_extension_list, ".cob");
	CB_TEXT_LIST_ADD (cb_extension_list, "");

	/* Process COB_COPY_DIR and COBCPY environment variables */
	process_env_copy_path (cob_getenv_value ("COB_COPY_DIR"));
	process_env_copy_path (getenv ("COBCPY"));

	/* Add default COB_COPY_DIR directory */
	CB_TEXT_LIST_CHK (cb_include_list, COB_COPY_DIR);
}

static int
process_file (struct filename *fn, int status)
{
	struct cobc_mem_struct	*mptr;
	struct cobc_mem_struct	*mptrt;

	current_compile_time = cob_get_current_date_and_time ();

	/* Initialize listing */
	if (cb_src_list_file) {
		set_listing_date ();
		set_standard_title ();

		cb_current_file = cb_listing_file_struct;
		cb_current_file->copy_tail = NULL;	/* may include an old reference */
		cb_current_file->name = cobc_strdup (fn->source);
		cb_current_file->source_format = cb_source_format;
		force_new_page_for_next_line ();
	}

	/* Initialize general vars */
	errorcount = 0;
	cb_source_file = NULL;
	cb_source_line = 0;
	current_section = NULL;
	current_paragraph = NULL;
	current_program = NULL;
	cb_id = 1;
	cb_pic_id = 1;
	cb_attr_id = 1;
	cb_literal_id = 1;
	cb_field_id = 1;
	cb_ml_attr_id = 1;
	cb_ml_tree_id = 1;
	demangle_name = fn->demangle_source;
	memset (optimize_defs, 0, sizeof (optimize_defs));

	if (cb_src_list_file) {
		cb_listing_page = 0;
		strncpy (cb_listing_filename, fn->source, FILENAME_MAX - 1);
		cb_listing_filename[FILENAME_MAX - 1] = 0;
		set_listing_header_code ();
	}

	if (cb_compile_level >= CB_LEVEL_PREPROCESS
	 && fn->need_preprocess) {
		/* Preprocess */
		fn->has_error = preprocess (fn);
		status |= fn->has_error;
		/* If preprocessing raised errors go on but only check syntax */
		if (fn->has_error) {
			cb_flag_syntax_only = 1;
		}
	}

	if (cobc_list_file) {
		putc ('\n', cb_listing_file);
	}

	if (cb_compile_level < CB_LEVEL_TRANSLATE) {
		if (cb_src_list_file) {
			print_program_listing ();
		}
		return status;
	}
	if (fn->need_translate) {
		/* Save default flags in case program directives change them */
		int			save_odoslide = cb_odoslide;

		/* Parse / Translate (to C code) */
		fn->has_error = process_translate (fn);
		status |= fn->has_error;
		if (cb_src_list_file) {
			print_program_listing ();
		}
		/* Free parse memory */
		for (mptr = cobc_parsemem_base; mptr; ) {
			mptrt = mptr;
			mptr = mptr->next;
			cobc_free (mptrt);
		}
		cobc_parsemem_base = NULL;
		cb_init_codegen ();
		/* Restore default flags */
		cb_odoslide = save_odoslide;
	} else {
		if (cb_src_list_file) {
			print_program_listing ();
		}
	}
	if (cb_compile_level < CB_LEVEL_COMPILE ||
	    cb_flag_syntax_only || fn->has_error) {
		return status;
	}
	if (cb_compile_level == CB_LEVEL_COMPILE) {
		/* Compile to assembler code */
		fn->has_error = process_compile (fn);
		status |= fn->has_error;
		return status;
	}

	if (cb_compile_level == CB_LEVEL_MODULE && fn->need_assemble) {
		/* Build module direct */
		fn->has_error = process_module_direct (fn);
		status |= fn->has_error;
	} else {
		/* Compile to object code */
		if (cb_compile_level >= CB_LEVEL_ASSEMBLE &&
		    fn->need_assemble) {
			fn->has_error = process_assemble (fn);
			status |= fn->has_error;
		}
		if (fn->has_error) {
			return status;
		}

		/* Build module */
		if (cb_compile_level == CB_LEVEL_MODULE) {
			fn->has_error = process_module (fn);
			status |= fn->has_error;
		}
	}
	return status;
}

/* Main function */
int
main (int argc, char **argv)
{
	struct filename		*fn;
	unsigned int		iparams;
	unsigned int		local_level;
	int			status;
	int			statuses = 0;
	int			i;
	const char		*run_name = NULL;

	/* Setup routines I */
	cob_setup_env ((const char*) argv[0]);
	begin_setup_internal_and_compiler_env ();

	cb_saveargc = argc;
	cb_saveargv = argv;

	/* Process command line arguments */
	iargs = process_command_line (argc, argv);

	if (fatal_startup_error) {
		cobc_err_msg (_("please check environment variables as noted above"));
		cobc_abort_terminate (0);
	}

	/* Check the filename */
	if (iargs == argc) {
		cobc_err_exit (_("no input files"));
	}

	/* Defaults are set here */
	if (!cb_flag_syntax_only) {
		if (cb_compile_level == 0) {
			if (cobc_flag_main) {
				cb_compile_level = CB_LEVEL_EXECUTABLE;
			} else if (cobc_flag_module) {
				cb_compile_level = CB_LEVEL_MODULE;
			} else if (cobc_flag_library) {
				cb_compile_level = CB_LEVEL_LIBRARY;
			} else {
				cb_compile_level = CB_LEVEL_MODULE;
				cobc_flag_module = 1;
			}
		} else if (cb_compile_level != CB_LEVEL_PREPROCESS &&
		    !cobc_flag_main && !cobc_flag_module && !cobc_flag_library) {
			cobc_flag_module = 1;
		}
	} else {
		cb_compile_level = CB_LEVEL_TRANSLATE;
		cobc_flag_main = 0;
		cobc_flag_module = 0;
		cobc_flag_library = 0;
	}

	if (output_name
	 && cb_compile_level < CB_LEVEL_LIBRARY
	 && (argc - iargs) > 1) {
		cobc_err_exit (_("%s option invalid in this combination"), "-o");
	}

	/* Setup routines II */
	finish_setup_compiler_env ();
	finish_setup_internal_env ();

	cb_text_column = cb_config_text_column;
	cb_indicator_column = 7;

	memset (cb_listing_header, 0, sizeof (cb_listing_header));
	/* If -P=file specified, all lists go to this file */
	if (cobc_list_file) {
		if (cb_unix_lf) {
			cb_listing_file = fopen (cobc_list_file, "wb");
		} else {
			cb_listing_file = fopen (cobc_list_file, "w");
		}
		if (!cb_listing_file) {
			cobc_terminate (cobc_list_file);
		}
	}

	/* internal complete source listing file */
	if (cb_listing_outputfile) {
		if (strcmp (cb_listing_outputfile, COB_DASH) == 0) {
			cb_src_list_file = stdout;
		} else {
			if (cb_unix_lf) {
				cb_src_list_file = fopen (cb_listing_outputfile, "wb");
			} else {
				cb_src_list_file = fopen (cb_listing_outputfile, "w");
			}
			if (!cb_src_list_file) {
				cobc_terminate (cb_listing_outputfile);
			}
		}
		cb_listing_file_struct = cobc_malloc (sizeof (struct list_files));
	}

	if (verbose_output) {
		fputs (_("command line:"), stderr);
		putc ('\t', stderr);
		for (i = 0; i < argc; ++i) {
			fprintf (stderr, "%s ", argv[i]);
		}
		putc ('\n', stderr);
		fflush (stderr);
	}

	/* Process input files */

	/* Set up file parameters, if any are missing: abort */
	while (iargs < argc) {
		fn = process_filename (argv[iargs++]);
		if (!fn) {
			cobc_clean_up (1);
			return 1;
		}
	}

	/* process all files */
	status = 0;
	iparams = 0;
	local_level = 0;

	for (fn = file_list; fn; fn = fn->next) {
		iparams++;
		if (iparams == 1 && cobc_flag_run) {
			if (fn->file_is_stdin
			 && cb_compile_level == CB_LEVEL_EXECUTABLE) {
				run_name = COB_DASH_OUT;
			} else {
				run_name = file_basename (fn->source, NULL);
			}
			run_name = cobc_strdup (run_name);
		}
		if (iparams > 1 && cb_compile_level == CB_LEVEL_EXECUTABLE) {
			/* only the first source has the compile_level and main flag set */
			local_level = cb_compile_level;
			cb_compile_level = CB_LEVEL_ASSEMBLE;
			cobc_flag_main = 0;
		}
		status = process_file (fn, status);
		statuses += status;

		/* take care for all intermediate files which aren't needed for linking */
		clean_up_intermediates (fn, status);
	}

	if (cobc_list_file) {
		fclose (cb_listing_file);
		cb_listing_file = NULL;
	}

	/* Clear rest of preprocess stuff */
	plex_clear_all ();

	/* Clear rest of parser stuff */
	ylex_clear_all ();

	if (local_level == CB_LEVEL_EXECUTABLE) {
		cb_compile_level = CB_LEVEL_EXECUTABLE;
	}

	if ((cb_compile_level < CB_LEVEL_LIBRARY) && cobc_flag_run && run_name) {
		/* Run job after module with cobcrun */
		if (status == 0) {
			status = process_run (run_name);
		}
		cobc_free ((void *)run_name);
	}
	
	if (cb_compile_level < CB_LEVEL_LIBRARY
	 || status || cb_flag_syntax_only) {
		/* Finished */
		cobc_clean_up (status);
		return status;
	}

	/* Allocate objects buffer */
	cobc_objects_buffer = cobc_main_malloc (cobc_objects_len);

	/* All processing must be ok before a job run will be attempted */
	statuses = 0;

	if (file_list) {
		/* Link */
		if (cb_compile_level == CB_LEVEL_LIBRARY) {
			/* Multi-program module */
			status = process_library (file_list);
		} else {
			/* Executable */
			status = process_link (file_list);
		}
		statuses += status;
	}

	/* Run job after compile? Use first (or only) filename */
	if (run_name) {
		if ((statuses == 0) && cobc_flag_run) {
			status = process_run (run_name);
		}
		cobc_free ((void *)run_name);
	}

	/* We have completed */
	cobc_clean_up (status);

	return status;
}

/*
   Copyright (C) 2007-2012, 2014-2020 Free Software Foundation, Inc.
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


#ifndef COB_LOCAL_H
#define COB_LOCAL_H

#ifdef	HAVE_STRINGS_H
#include <strings.h>
#endif

/* We use this file to define/prototype things that should not be
   exported to user space
*/

#ifdef HAVE_ISFINITE
#define ISFINITE isfinite
#else
#define ISFINITE finite
#endif

#if	defined(_MSC_VER) || defined(__BORLANDC__) || defined(__WATCOMC__)

#include <float.h>
#undef ISFINITE
#define	ISFINITE		_finite
#endif


#ifdef ENABLE_NLS
#include "gettext.h"	/* from lib/ */
#define _(s)		gettext(s)
#define N_(s)		gettext_noop(s)
#else
#define _(s)		s
#define N_(s)		s
#endif


#include <../libcob/cobinternal.h>

#ifndef	F_OK
#define	F_OK		0
#endif

#ifndef	X_OK
#define	X_OK		1
#endif

#ifndef	W_OK
#define	W_OK		2
#endif

#ifndef	R_OK
#define	R_OK		4
#endif

/* Stacked field depth */
#define COB_DEPTH_LEVEL		32U

/* Not-A-Number */
#define COB_DECIMAL_NAN		-32768

/* Infinity */
#define COB_DECIMAL_INF		-32767

/* GMP decimal default */
#define COB_MPZ_DEF		1024UL

/* GMP floating precision */
#define COB_MPF_PREC		2048UL

/* Complex calculation cutoff value */
/* This MUST be <= COB_MPF_PREC */
#define COB_MPF_CUTOFF		1024UL


/* Floating-decimal */
#ifdef	WORDS_BIGENDIAN
#define	COB_128_MSW(x)		x[0]
#define	COB_128_LSW(x)		x[1]
#define	COB_MPZ_ENDIAN		1
#else
#define	COB_128_MSW(x)		x[1]
#define	COB_128_LSW(x)		x[0]
#define	COB_MPZ_ENDIAN		-1
#endif

/* Mask for inf/nan */
#define	COB_DEC_SPECIAL		COB_U64_C(0x7800000000000000)
/* Mask for extended */
#define	COB_DEC_EXTEND		COB_U64_C(0x6000000000000000)
/* Mask for sign */
#define	COB_DEC_SIGN		COB_U64_C(0x8000000000000000)

#define	COB_64_IS_SPECIAL(x)	((x & COB_DEC_SPECIAL) == COB_DEC_SPECIAL)
#define	COB_128_IS_SPECIAL(x)	\
	((COB_128_MSW(x) & COB_DEC_SPECIAL) == COB_DEC_SPECIAL)
#define	COB_64_IS_EXTEND(x)	((x & COB_DEC_EXTEND) == COB_DEC_EXTEND)
#define	COB_128_IS_EXTEND(x)	\
	((COB_128_MSW(x) & COB_DEC_EXTEND) == COB_DEC_EXTEND)

/* Exponent 1 - 10 bits after sign bit */
#define	COB_64_EXPO_1		COB_U64_C(0x7FE0000000000000)
/* Significand 1 */
#define	COB_64_SIGF_1		COB_U64_C(0x001FFFFFFFFFFFFF)
/* Exponent 2 - 10 bits after (sign bit + 2) */
#define	COB_64_EXPO_2		COB_U64_C(0x1FF8000000000000)
/* Significand 2 */
#define	COB_64_SIGF_2		COB_U64_C(0x0007FFFFFFFFFFFF)
/* Extended or bit */
#define	COB_64_OR_EXTEND	COB_U64_C(0x0020000000000000)

/* Exponent 1 - 14 bits after sign bit */
#define	COB_128_EXPO_1		COB_U64_C(0x7FFE000000000000)
/* Significand 1 */
#define	COB_128_SIGF_1		COB_U64_C(0x0001FFFFFFFFFFFF)
/* Exponent 2 - 14 bits after (sign bit + 2) */
#define	COB_128_EXPO_2		COB_U64_C(0x1FFF800000000000)
/* Significand 2 */
#define	COB_128_SIGF_2		COB_U64_C(0x00007FFFFFFFFFFF)
/* Extended or bit */
#define	COB_128_OR_EXTEND	COB_U64_C(0x0002000000000000)

/* Field/attribute initializers */
#define COB_FIELD_INIT(x,y,z)	do { \
	field.size = x; \
	field.data = y; \
	field.attr = z; \
	} ONCE_COB

#define COB_ATTR_INIT(u,v,x,y,z)	do { \
	attr.type = u; \
	attr.digits = v; \
	attr.scale = x; \
	attr.flags = y; \
	attr.pic = z; \
	} ONCE_COB

#define COB_GET_SIGN(f)		\
	(COB_FIELD_HAVE_SIGN (f) ? cob_real_get_sign (f) : 0)
#define COB_PUT_SIGN(f,s)	\
	do { if (COB_FIELD_HAVE_SIGN (f)) cob_real_put_sign (f, s); } ONCE_COB

#ifdef	COB_PARAM_CHECK
#define	COB_CHK_PARMS(x,z)	\
	cob_parameter_check (#x, z)
#else
#define	COB_CHK_PARMS(x,z)
#endif

/* byte offset to structure member */
#if !defined(_OFFSET_OF_) && !defined(offsetof)
#define _OFFSET_OF_
#define offsetof(s_name,m_name) (int)(long)&(((s_name*)0))->m_name
#endif

/* Convert between a digit and an integer (e.g., '0' <-> 0) */
#define COB_D2I(x)		((x) - '0')
#define COB_I2D(x)		(char) ((x) + '0')

#define	COB_MODULE_PTR		cobglobptr->cob_current_module
#define	COB_TERM_BUFF		cobglobptr->cob_term_buff
#define	COB_ACCEPT_STATUS	cobglobptr->cob_accept_status
#define	COB_MAX_Y_COORD		cobglobptr->cob_max_y
#define	COB_MAX_X_COORD		cobglobptr->cob_max_x

#define	COB_DISP_TO_STDERR	cobsetptr->cob_disp_to_stderr
#define	COB_BEEP_VALUE		cobsetptr->cob_beep_value
#define	COB_TIMEOUT_SCALE	cobsetptr->cob_timeout_scale
#define	COB_INSERT_MODE		cobsetptr->cob_insert_mode
#define	COB_EXTENDED_STATUS	cobsetptr->cob_extended_status
#define	COB_MOUSE_FLAGS	cobsetptr->cob_mouse_flags
#define	COB_MOUSE_INTERVAL	cobsetptr->cob_mouse_interval
#define	COB_USE_ESC		cobsetptr->cob_use_esc

#ifdef __cplusplus
extern "C" {
#endif

/* Global settings structure */

typedef struct __cob_settings {
	unsigned int	cob_display_warn;	/* Display warnings */
	unsigned int	cob_env_mangle;		/* Mangle env names */
	unsigned int	cob_debugging_mode;		/* Activate USE ON DEBUGGING procedures */
	unsigned int	cob_line_trace;		/* Activate tracing for routines compiled with trace flag */
	unsigned int	cob_config_cur;		/* Current runtime.cfg file being processed */
	unsigned int	cob_config_num;		/* Number of different runtime.cfg files read */
	char		**cob_config_file;	/* Keep all file names for later reporting */

	char		*cob_trace_filename;	/* File to write TRACE[ALL] information to */
	char		*cob_trace_format;	/* Format of trace line */
	char		*cob_user_name;
	char		*cob_sys_lang;		/* LANG setting from env */
	char		*cob_sys_term;		/* TERM setting from env */
	char		*cob_sys_type;		/* OSTYPE setting from env */
	char		*cob_debug_log;
	char		*cob_date;		/* Date override for testing purposes / UTC hint */
	unsigned int		cob_stacktrace;		/* generate a stack trace on abort */
	struct cob_time	cob_time_constant;

	/* call.c */
	unsigned int	cob_physical_cancel;
	unsigned int	name_convert;
	char		*cob_preload_str;
	char		*cob_library_path;
	char		*cob_preload_str_set;

	size_t		*resolve_size;	/* Array size of resolve_path*/
	char		*cob_preload_resolved;
	char		*cob_preload_env;

	/* fileio.c */
	unsigned int	cob_unix_lf;		/* Use POSIX LF */
	unsigned int	cob_do_sync;
	unsigned int	cob_ls_uses_cr;		/* Line Sequential uses CR LF */
	unsigned int	cob_ls_fixed;		/* Line Sequential is fixed length */
	unsigned int	cob_ls_nulls;		/* NUL insert to Line Sequential */
	unsigned int	cob_ls_split;		/* Split 'too long' record into parts (Default is truncate) */
	unsigned int	cob_ls_validate;	/* Validate data in Line Sequential */
	unsigned int	cob_mf_ls_nulls;	/* MF file: NUL insert to Line Sequential (INSERTNULL)*/
	unsigned int	cob_mf_ls_instab;	/* MF file: TAB insert to Line Sequential (INSERTTAB)*/
	unsigned int	cob_mf_ls_split;	/* MF file: Split 'too long' record into parts */
	unsigned int	cob_mf_ls_validate;	/* MF file: Validate data in Line Sequential */
	unsigned int	cob_varseq_type;	/* Variable Sequential Default file format */
	unsigned int	cob_varrel_type;	/* Variable Relative default file format */
	unsigned int	cob_fixrel_type;	/* Fixed Relative default file format */
	unsigned int	cob_mf_files;		/* If TRUE, use Micro Focus file formats */
	unsigned int	cob_gc_files;		/* If TRUE, revert back to old GNU Cobol file formats */
	unsigned int	cob_retry_times;	/* Default: RETRY n TIMES value */
	unsigned int	cob_retry_seconds;	/* Default: RETRY n SECONDS value */
	unsigned int	cob_trace_io;		/* If TRACE READY, also dump File/Record/Status */
	unsigned int	cob_stats_record;	/* If record I/O statics */
	unsigned int	cob_share_mode;		/* Default file share mode */
	unsigned int	cob_retry_mode;		/* Default file retry mode */
	unsigned int	cob_keycheck;		/* Default KEYCHECK mode */
	unsigned int	cob_file_dict;		/* When to use filename.dd (File definition) */
	unsigned int	cob_bdb_byteorder;	/* Byte order to use for BDB files */
	unsigned int	cob_file_dups;		/* When to check for duplicate key in INDEXED file*/
	unsigned int	cob_file_rollback;	/* Should transactions be enabled */
	char		*cob_dictionary_path;	/* Place to write filename.dd stats */
	char		*cob_stats_filename;	/* Place to write I/O stats */
	char 		*cob_file_path;
	char		*bdb_home;
	char		*lmdb_home;
	size_t		cob_sort_memory;
	size_t		cob_sort_chunk;

	/* move.c */
	unsigned int	cob_local_edit;

	/* screenio.c */
	unsigned int 	cob_legacy;
	unsigned int	cob_disp_to_stderr;	/* Redirect to stderr */
	unsigned int	cob_beep_value;		/* Bell disposition */
	unsigned int	cob_extended_status;	/* Extended status */
	unsigned int	cob_mouse_flags;	/* Mouse flags to mask to COBOL, values according to ACUCOBOL */
	unsigned int	cob_mouse_interval;		/* time to recognize a click, 0 = click resolution disabled */
	unsigned int	cob_use_esc;		/* Check ESC key */
	unsigned int	cob_timeout_scale;	/* timeout scale */
	unsigned int	cob_insert_mode;	/* insert toggle, 0=off, 1=on */
	unsigned int	cob_exit_wait;		/* wait on program exit if no ACCEPT came after last DISPLAY */
	const char		*cob_exit_msg;		/* message for cob_exit_wait */


	/* reportio.c */
	unsigned int 	cob_col_just_lrc;	/* Justify data in column LEFT/RIGHT/CENTER */

	/* termio.c */
	char 		*cob_display_print_pipe;		/* DISPLAY UPON PRINTER destination */
	char		*cob_display_print_filename;	/* File name for DISPLAY UPON PRINTER */

	char		*cob_display_punch_filename;	/* File name for DISPLAY UPON SYSPUNCH/SYSPCH */
	FILE		*cob_display_punch_file;	/* possibly external FILE* to write DISPLAY UPON SYSPUNCH information to
											   cob_display_punch_filename is used to open the file
											   on first DISPLAY UPON SYSPCH statement and closed
											   on runtime exit */

	/* common.c */
	char		external_trace_file;	/* use external FILE * for TRACE[ALL] */
	FILE		*cob_trace_file;		/* FILE* to write TRACE[ALL] information to */
	FILE		*cob_display_print_file;	/* external FILE* to write DISPLAY UPON PRINTER information to 
											   if not external cob_display_print_filename is always opened
											   before each DISPLAY UPON PRINTER and closed afterwards */
	FILE		*cob_dump_file;		/* FILE* to write DUMP information to */

	char		*cob_dump_filename;	/* Place to write dump of variables */
	int		cob_dump_width;		/* Max line width for dump */
} cob_settings;


struct config_enum {
	const char	*match;			/* Alternate word that could be used */
	const char	*value;			/* Internal value for this 'word' */
};

/* Format of table for capturing run-time config information */
struct config_tbl {
	const char	*env_name;		/* Env Var name */
	const char	*conf_name;		/* Name used in run-time config file */
	const char	*default_val;		/* Default value */
	struct config_enum *enums;		/* Table of Alternate values */
	int		env_group;		/* Grouping for display of run-time options */
	int		data_type;		/* Data type */
	int		data_loc;		/* Location within structure */
	int		data_len;		/* Length of referenced field */
	int		config_num;		/* Set by which runtime.cfg file */
	int		set_by;			/* value set by a different keyword */
	long	min_value;		/* Minimum accepted value */
	unsigned long	max_value;		/* Maximum accepted value */
};

#define ENV_NOT		(1 << 1)		/* Negate True/False value setting */
#define ENV_UINT	(1 << 2)		/* an 'unsigned int' */
#define ENV_SINT	(1 << 3)		/* a 'signed int' */
#define ENV_SIZE	(1 << 4)		/* size; number with K - kb, M - mb, G - GB */
#define ENV_BOOL	(1 << 5)		/* int boolean; Yes, True, 1, No, False, 0, ... */
#define ENV_CHAR	(1 << 6)		/* inline 'char[]' field */
#define ENV_STR		(1 << 7)		/* a pointer to a string */
#define ENV_PATH	(1 << 8)		/* a pointer to one or more file system paths [fp1:fp2:fp3] */
#define ENV_ENUM	(1 << 9)		/* Value must in 'enum' list as match */
#define ENV_ENUMVAL	(1 << 10)		/* Value must in 'enum' list as match or value */
#define ENV_FILE 	(1 << 11)		/* a pointer to a directory/file [single path] */

#define ENV_RESETS 	(1 << 14)		/* Value setting needs additional code */

#define STS_ENVSET	(1 << 15)		/* value set via Env Var */
#define STS_CNFSET	(1 << 16)		/* value set via config file */
#define STS_ENVCLR	(1 << 17)		/* value removed from Env Var */
#define STS_RESET	(1 << 18)		/* value was reset back to default */
#define STS_FNCSET	(1 << 19)		/* value set via function call */

#define GRP_HIDE	0
#define GRP_CALL	1
#define GRP_FILE	2
#define GRP_SCREEN	3
#define GRP_MISC	4
#define GRP_SYSENV	5
#define GRP_MAX 	6

#define SETPOS(member)	offsetof(cob_settings,member),sizeof(cobsetptr->member),0,0


/* Local function prototypes */
COB_EXPIMP const char * cob_io_version	(const int, const int);
COB_EXPIMP const char * cob_io_name		(const int);
COB_HIDDEN void		cob_init_numeric	(cob_global *);
COB_HIDDEN void		cob_init_termio		(cob_global *, cob_settings *);
COB_HIDDEN void		cob_init_fileio		(cob_global *, cob_settings *);
COB_HIDDEN void		cob_init_reportio	(cob_global *, cob_settings *);
COB_HIDDEN void		cob_init_call		(cob_global *, cob_settings *, const int);
COB_HIDDEN void		cob_init_intrinsic	(cob_global *);
COB_HIDDEN void		cob_init_strings	(cob_global *);
COB_HIDDEN void		cob_init_move		(cob_global *, cob_settings *);
COB_HIDDEN void		cob_init_screenio	(cob_global *, cob_settings *);
COB_HIDDEN void		cob_init_mlio		(cob_global * const);

COB_HIDDEN void		cob_print_field		(FILE *, cob_field *, int, int);
COB_HIDDEN void		cob_module_clean	(cob_module *);

COB_HIDDEN char		*cob_get_filename_print	(cob_file *, const int);
COB_HIDDEN void		cob_fork_fileio		(cob_global *, cob_settings *);
COB_HIDDEN void		free_extfh_fcd		(void);

COB_HIDDEN void		cob_exit_screen		(void);
COB_HIDDEN void		cob_exit_numeric	(void);
COB_HIDDEN void		cob_exit_fileio_msg_only	(void);
COB_HIDDEN void		cob_exit_fileio		(void);
COB_HIDDEN void		cob_exit_reportio	(void);
COB_HIDDEN void		cob_exit_call		(void);
COB_HIDDEN void		cob_exit_intrinsic	(void);
COB_HIDDEN void		cob_exit_strings	(void);
COB_HIDDEN void		cob_exit_mlio		(void);

COB_HIDDEN FILE		*cob_create_tmpfile	(const char *);
COB_HIDDEN int		cob_check_numval_f	(const cob_field *);

COB_HIDDEN int		cob_real_get_sign	(cob_field *);
COB_HIDDEN void		cob_real_put_sign	(cob_field *, const int);

#ifndef COB_WITHOUT_DECIMAL
COB_HIDDEN void		cob_decimal_init2	(cob_decimal *, const cob_uli_t);
#endif
COB_HIDDEN void		cob_decimal_setget_fld	(cob_field *, cob_field *,
						 const int);
COB_HIDDEN void		cob_decimal_move_temp	(cob_field *, cob_field *);

COB_HIDDEN void		cob_display_common	(const cob_field *, FILE *);
COB_HIDDEN void		cob_print_ieeedec	(const cob_field *, FILE *);
COB_HIDDEN void		cob_print_realbin	(const cob_field *, FILE *,
						 const int);

COB_HIDDEN void		cob_screen_set_mode	(const cob_u32_t);
COB_HIDDEN void		cob_settings_screenio	(void);

COB_HIDDEN int		cob_get_last_exception_code	(void);
COB_HIDDEN int		cob_check_env_true	(char*);
COB_HIDDEN int		cob_check_env_false	(char*);
COB_HIDDEN const char	*cob_get_last_exception_name	(void);
COB_EXPIMP void		cob_field_to_string	(const cob_field *, void *,
						 const size_t);
COB_HIDDEN void		cob_parameter_check	(const char *, const int);

COB_HIDDEN cob_settings *cob_get_settings_ptr	(void);

COB_HIDDEN FILE			*cob_get_dump_file	(void);

COB_HIDDEN char		*cob_strcat		(char*, char*, int);
COB_HIDDEN char		*cob_strjoin		(char**, int, char*);

COB_HIDDEN void	cob_set_field_to_uint	(cob_field *, const cob_u32_t);
COB_EXPIMP int cob_ncase_cmp (char *, const char *, unsigned );
COB_EXPIMP char * cob_str_case_str (char *, const char *);

/* static inline of smaller helpers */

static COB_INLINE int
cob_min_int (const int x, const int y)
{
	if (x < y) return x;
	return y;
}

static COB_INLINE int
cob_max_int (const int x, const int y)
{
	if (x > y) return x;
	return y;
}


#ifdef __cplusplus
}
#endif

#endif	/* COB_LOCAL_H */

/*
   Copyright (C) 2001-2012, 2014-2023 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Simon Sobisch,
   Edward Hart, Ron Norman, Dave Pitts

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


#ifndef CB_COBC_H
#define CB_COBC_H

/* inclusion of common.h to get type definitions and some macros
   TODO: move those out of common.h to a second installed header
         and include that here */
#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#else
#include <stdio.h>
#endif
#include <stdio.h>	/* for FILE* */

#include "../libcob/common.h"

#ifdef	ENABLE_NLS
#include "../lib/gettext.h"
#define _(s)		gettext(s)
#define N_(s)		gettext_noop(s)
#else
#define _(s)		s
#define N_(s)		s
#endif

#include "../libcob/sysdefines.h"

/* Defines for access() */
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

#define COBC_ABORT()			cobc_abort(__FILE__, __LINE__)
#define YY_FATAL_ERROR(msg)		\
	flex_fatal_error (msg, __FILE__, __LINE__)

/* Source format enum */
enum cb_format {
	CB_FORMAT_FIXED = 0,
	CB_FORMAT_FREE
};

#if 0 /* ancient OSVS registers that need special runtime handling - low priority */
/* format in CURRENT-DATE register */
enum cb_current_date {
	CB_DATE_MDY = 0,
	CB_DATE_YMD,
	CB_DATE_DMY
};
#endif

/* COPY extended syntax defines */
#define CB_REPLACE_LEADING		1U
#define CB_REPLACE_TRAILING		2U

/* Context sensitive keyword defines (trigger words) */
#define	CB_CS_ACCEPT			(1U << 0)	/* within ACCEPT statement */
#define CB_CS_ALLOCATE			(1U << 1)	/* within ALLOCATE statement */
#define	CB_CS_ALPHABET			(1U << 2)
#define	CB_CS_ASSIGN			(1U << 3)
#define	CB_CS_CALL			(1U << 4)	/* within CALL statement */
#define	CB_CS_CONSTANT			(1U << 5)
#define	CB_CS_DATE			(1U << 6)
#define	CB_CS_DAY			(1U << 7)
#define	CB_CS_DISPLAY			(1U << 8)	/* within DISPLAY statement */
#define	CB_CS_ERASE			(1U << 9)
#define	CB_CS_EXIT			(1U << 10)	/* within EXIT statement */
#define	CB_CS_FROM			(1U << 11)
#define	CB_CS_OCCURS			(1U << 12)
#define CB_CS_OPTIONS			(1U << 13)
#define	CB_CS_PERFORM			(1U << 14)	/* within PERFORM statement */
#define	CB_CS_PROGRAM_ID		(1U << 15)	/* within PROGRAM-ID definition */
#define	CB_CS_READ			(1U << 16)	/* within READ statement */
#define	CB_CS_RECORDING			(1U << 17)
#define	CB_CS_RETRY			(1U << 18)
#define	CB_CS_ROUNDED			(1U << 19)
#define	CB_CS_SET			(1U << 20)	/* within SET statement */
#define	CB_CS_STOP			(1U << 21)
#define	CB_CS_OBJECT_COMPUTER		(1U << 22)
#define	CB_CS_DELIMITER			(1U << 23)
#define	CB_CS_SCREEN			(1U << 24)	/* within SCREEN section */
#define	CB_CS_INQUIRE_MODIFY		(1U << 25)	/* within INQUIRE or MODIFY statement */
#define	CB_CS_GRAPHICAL_CONTROL		(1U << 26)	/* within ACUCOBOL-GT graphical control */
#define	CB_CS_SELECT			(1U << 27)	/* within SELECT */
#define	CB_CS_XML_GENERATE		(1U << 28)
#define	CB_CS_XML_PARSE			(1U << 29)
#define	CB_CS_OPEN			(1U << 30)	/* within OPEN */
#define	CB_CS_JSON_GENERATE		(1U << 31)
/* HACK: no more space - using minor one until re-written */
#define	CB_CS_I_O_CONTROL		CB_CS_DAY
#define	CB_CS_TYPEDEF			CB_CS_DAY
#define	CB_CS_EXHIBIT			CB_CS_DAY
#define	CB_CS_MOVE_CONV			CB_CS_DAY
#define	CB_CS_INSPECT			CB_CS_DAY
#define	CB_CS_CONVERT			CB_CS_DAY
#define	CB_CS_MODULE_NAME		CB_CS_DAY
#define	CB_CS_DEFAULT			CB_CS_DAY
#define	CB_CS_VALIDATE_STATUS	CB_CS_DAY

/* Support for cobc from stdin */
#define COB_DASH			"-"
#define COB_DASH_NAME			"a.cob"
#define COB_DASH_OUT			"a.out"


/* Operand operation type */
enum cb_operation_type {
	CB_OPERATION_READ = 0,
	CB_OPERATION_WRITE,
	CB_OPERATION_ASSIGN
};

/* Config dialect support types */
enum cb_support {
	CB_OK = 0,
	CB_WARNING,
	CB_ARCHAIC,
	CB_OBSOLETE,
	CB_SKIP,
	CB_IGNORE,
	CB_ERROR,
	CB_UNCONFORMABLE
};

/* Config dialect support types */
enum cb_std_def {
	CB_STD_GC = 0,
	CB_STD_MF,
	CB_STD_IBM,
	CB_STD_MVS,
	CB_STD_BS2000,
	CB_STD_ACU,
	CB_STD_RM,
	/* the following must contain ANSI/ISO standards in order */
	CB_STD_85,
	CB_STD_2002,
	CB_STD_2014,
	/* the following must be the last and is invalid */
	CB_STD_MAX
};

/* Binary field sizes */
enum cb_binary_size_options {
	CB_BINARY_SIZE_1_2_4_8 = 0,	/* 1,2,4,8 bytes */
	CB_BINARY_SIZE_1__8,		/* 1,2,3,4,5,6,7,8 bytes */
	CB_BINARY_SIZE_2_4_8	/* 2,4,8 bytes */
};

/* COMP/BINARY byte order */
enum cb_binary_byteorder_options {
	CB_BYTEORDER_BIG_ENDIAN = 0,
	CB_BYTEORDER_NATIVE
};

/* Type of device specified in ASSIGN clause */
enum cb_assign_device {
	CB_ASSIGN_NO_DEVICE,
	CB_ASSIGN_GENERAL_DEVICE,
	CB_ASSIGN_LINE_SEQ_DEVICE,
	CB_ASSIGN_DISPLAY_DEVICE,
	CB_ASSIGN_KEYBOARD_DEVICE,
	CB_ASSIGN_PRINTER_DEVICE,
	CB_ASSIGN_PRINTER_1_DEVICE,
	CB_ASSIGN_PRINT_DEVICE
};

/* Clauses an elementary screen item is required to have */
enum cb_screen_clauses_rules {
	CB_ACU_SCREEN_RULES,
	CB_GC_SCREEN_RULES,
	CB_MF_SCREEN_RULES,
	CB_RM_SCREEN_RULES,
	CB_STD_SCREEN_RULES,
	CB_XOPEN_SCREEN_RULES
};

/* DECIMAL-POINT IS COMMA effect in XML/JSON GENERATE statements */
enum cb_dpc_in_data_options {
	CB_DPC_IN_NONE,
	CB_DPC_IN_XML,
	CB_DPC_IN_JSON,
	CB_DPC_IN_ALL
};

/* Generic text list structure */
struct cb_text_list {
	struct cb_text_list	*next;			/* next pointer */
	struct cb_text_list	*last;
	const char		*text;
};

/* Generic replace list structure */
struct cb_replace_list {
	int				line_num;
	struct cb_replace_list		*next;			/* next pointer */
	struct cb_replace_list		*last;
	struct cb_replace_list		*prev;
	const struct cb_text_list	*old_text;
	const struct cb_text_list	*new_text;
	unsigned int			lead_trail;
};

/* Structure for extended filenames */
struct local_filename {
	struct local_filename	*next;			/* next pointer */
	char			*local_name;			/* foo.c.l[n].h (full path) */
	char			*local_include_name;	/* foo.c.l[n].h (for #include)*/
	FILE			*local_fp;
};

/* Structure for filename */
struct filename {
	struct filename		*next;
	const char		*source;		/* foo.cob (path from command line) */
	const char		*preprocess;		/* foo.i / foo.cob (possibly full path) */
	const char		*translate;		/* foo.c (possibly full path) */
	const char		*trstorage;		/* foo.c.h (possibly full path) */
	const char		*object;		/* foo.o (possibly full path) */
	const char		*demangle_source;	/* foo */
	const char		*listing_file;		/* foo.lst */
	struct local_filename	*localfile;		/* foo.c.l[n].h */
	size_t			translate_len;		/* strlen translate */
	size_t			object_len;		/* strlen object */
	unsigned int		need_preprocess;	/* Needs preprocess */
	unsigned int		need_translate;		/* Needs parse */
	unsigned int		need_assemble;		/* Needs C compile */
	int			has_error;		/* Error detected */
	int			file_is_stdin;		/* dash used as filename */
};

/* Exception structure */
struct cb_exception {
	const char	*name;			/* Exception name */
	const int	code;			/* Exception code */
	int		enable;			/* If turned on */
};

/* Type of name to check in cobc_check_valid_name */
enum cobc_name_type {
	FILE_BASE_NAME = 0,
	ENTRY_NAME,
	PROGRAM_ID_NAME
};

/* Listing structures and externals */

/* List of error messages */
struct list_error {
	struct list_error	*next;
	struct list_error	*prev;
	int			line;		/* Line number for error */
	char			*file;		/* File name */
	char			*prefix;	/* Error prefix */
	char			*msg;		/* Error Message text */
};

/* List of REPLACE text blocks */
struct list_replace {
	struct list_replace	*next;
	int			firstline;	/* First line for replace */
	int			lastline;	/* Last line for replace */
	int			lead_trail;	/* LEADING/TRAILING flag */
	char			*from;		/* Old (from) text */
	char			*to;		/* New (to) text */
};

/* List of skipped lines (conditional compilation) */
struct list_skip {
	struct list_skip	*next;
	int			skipline;	/* line number of skipped line */
};

/* Listing file control structure */
struct list_files {
	struct list_files	*next;
	struct list_files	*copy_head;	/* COPY book list head */
	struct list_files	*copy_tail;	/* COPY book list tail */
	struct list_error	*err_head;	/* Error message list head */
	struct list_replace	*replace_head;	/* REPLACE list head */
	struct list_replace	*replace_tail;	/* REPLACE list tail */
	struct list_skip	*skip_head;	/* Skip list head */
	struct list_skip	*skip_tail;	/* Skip list tail */
	int 			copy_line;	/* Line start for copy book */
	int 			listing_on;	/* Listing flag for this file */
	enum cb_format		source_format;	/* source format for file */
	const char		*name;		/* Name of this file */
};

extern struct list_files	*cb_current_file;

extern enum cb_format		cb_source_format;
#if 0 /* ancient OSVS registers that need special runtime handling - low priority */
extern enum cb_current_date	current_date;
#endif
extern int			cb_text_column;	/* end of area B (in single-byte characters) */
extern int	cb_mf_ibm_comp;
extern int	cb_cob_line_num;
extern int	cb_all_files_xfd;
extern int	cb_keycompress_ready;
extern int	cb_keycompress_pend;
extern int	cb_max_binary;
extern int	cb_max_compx;

extern struct cb_exception	cb_exception_table[];

#define CB_EXCEPTION_NAME(id)	cb_exception_table[id].name
#define CB_EXCEPTION_CODE(id)	cb_exception_table[id].code
#define CB_EXCEPTION_ENABLE(id)	cb_exception_table[id].enable

/* undef macros that are only for internal use with def-files */

#undef	CB_FLAG
#undef	CB_FLAG_ON
#undef	CB_FLAG_RQ
#undef	CB_FLAG_NQ
#undef	CB_FLAG_OP
#undef	CB_FLAG_NO

#undef	CB_WARNDEF
#undef	CB_ONWARNDEF
#undef	CB_NOWARNDEF

#undef	CB_OPTIM_DEF

#undef	CB_CONFIG_ANY
#undef	CB_CONFIG_INT
#undef	CB_CONFIG_SIZE
#undef	CB_CONFIG_STRING
#undef	CB_CONFIG_BOOLEAN
#undef	CB_CONFIG_SUPPORT

#undef	COB_EXCEPTION


#define	CB_FLAG(var,print_help,name,doc)		extern int var;
#define	CB_FLAG_ON(var,print_help,name,doc)		extern int var;
#define CB_FLAG_RQ(var,print_help,name,def,opt,doc)	extern int var;
#define CB_FLAG_NQ(print_help,name,opt,doc)
#define CB_FLAG_OP(print_help,name,opt,doc)
#define CB_FLAG_NO(print_help,name,opt,doc)
#include "flag.def"
#undef	CB_FLAG
#undef	CB_FLAG_ON
#undef	CB_FLAG_RQ
#undef	CB_FLAG_NQ
#undef	CB_FLAG_OP
#undef	CB_FLAG_NO


#define	CB_WARNDEF(opt,name,doc)	opt,
#define	CB_ONWARNDEF(opt,name,doc)	opt,
#define	CB_NOWARNDEF(opt,name,doc)	opt,
#define	CB_ERRWARNDEF(opt,name,doc)	opt,
enum cb_warn_opt
{
	COB_WARNOPT_NONE = 0,
#include "warning.def"
	COB_WARNOPT_MAX
};
#undef	CB_WARNDEF
#undef	CB_ONWARNDEF
#undef	CB_NOWARNDEF
#undef	CB_ERRWARNDEF

#define COBC_WARN_FILLER  cb_warn_filler

enum cb_warn_val {
	COBC_WARN_DISABLED = 0,
	COBC_WARN_DISABLED_EXPL = 1,	/* only active during command line parsing */
	COBC_WARN_ENABLED_EXPL = 2,	/* only active during command line parsing */
	COBC_WARN_ENABLED  = 3,
	COBC_WARN_AS_ERROR = 4
};

extern int cb_warn_opt_val[COB_WARNOPT_MAX];	/* note: int as we feed that to getopt */


#define	CB_OPTIM_DEF(x)			x,
enum cb_optim {
	COB_OPTIM_MIN = 0,
#include "codeoptim.def"
	COB_OPTIM_MAX
};
#undef	CB_OPTIM_DEF

extern int			cb_id;
extern int			cb_pic_id;
extern int			cb_attr_id;
extern int			cb_literal_id;
extern int			cb_field_id;
extern int			cb_ml_attr_id;
extern int			cb_ml_tree_id;
extern int			cb_flag_optimize_check;
extern int			cb_flag_functions_all;

extern int			cb_flag_dump;

extern int			cb_unix_lf;

extern int			cb_flag_main;	/* set if "main" requested by -x */
extern int			cobc_flag_main;	/* set only until first program compiled, for general: use cb_flag_main*/
extern int			cobc_wants_debug;
extern int			cb_listing_xref;
extern int			cobc_seen_stdin;

extern int			errorcount;
extern int			warningcount;
extern int			no_physical_cancel;
extern cob_u32_t		optimize_defs[];

extern const char		*cb_cobc_build_stamp;
extern const char		*cb_source_file;
extern const char      *cb_dialect;
extern int			cb_source_line;
extern const char		*cb_call_extfh;

extern struct cob_time		current_compile_time;
extern struct tm			current_compile_tm;

extern const char		*cob_config_dir;
extern const char		*cob_schema_dir;
extern const char		*cb_sqldb_schema;

extern unsigned int		cobc_gen_listing;

extern const char		*demangle_name;
extern FILE			*cb_storage_file;
extern const char		*cb_storage_file_name;

extern char			**cb_saveargv;
extern int			cb_saveargc;

extern FILE			*cb_listing_file;
extern FILE			*cb_src_list_file;
extern struct cb_text_list	*cb_include_list;
extern struct cb_text_list	*cb_intrinsic_list;
extern struct cb_text_list	*cb_extension_list;
extern struct cb_text_list	*cb_static_call_list;
extern struct cb_text_list	*cb_early_exit_list;

extern struct cb_program	*current_program;
extern struct cb_statement	*current_statement;
extern struct cb_label		*current_section;
extern struct cb_label		*current_paragraph;
extern int			cb_exp_line;
extern int			functions_are_all;
extern struct cb_tree_common	*defined_prog_list;
extern int			current_call_convention;
extern struct cb_field		*external_defined_fields_ws;
extern struct cb_field		*external_defined_fields_global;

/* Functions */

/* cobc.c */

extern struct reserved_word_list	*cob_user_res_list;

extern void			*cobc_malloc (const size_t);
extern void			cobc_free (void *);
extern void			*cobc_strdup (const char *);
extern void			*cobc_realloc (void *, const size_t);

extern void			*cobc_main_malloc (const size_t);
extern void			*cobc_main_strdup (const char *);
extern void			*cobc_main_realloc (void *, const size_t);
extern void			cobc_main_free (void *);

extern void			*cobc_parse_malloc (const size_t);
extern void			*cobc_parse_strdup (const char *);
extern void			*cobc_parse_realloc (void *, const size_t);
extern void			cobc_parse_free (void *);

extern void			*cobc_plex_malloc (const size_t);
extern void			*cobc_plex_strdup (const char *);

extern void			*cobc_check_string (const char *);
extern void			cobc_err_msg (const char *, ...) COB_A_FORMAT12;

extern char			*cobc_elided_strcpy (char *, const char *, const size_t, const int);

DECLNORET extern void		cobc_abort (const char *,
					    const int) COB_A_NORETURN;
DECLNORET extern void		cobc_abort_terminate (const int) COB_A_NORETURN;


extern size_t			cobc_check_valid_name (const char *,
						       const enum cobc_name_type);

/* help.c (used only within cobc.c) */

extern void		cobc_print_usage (char *);
extern void		cobc_print_usage_common_options (void);
extern void		cobc_print_usage_dialect (void);
extern void		cobc_print_usage_warnings (void);
extern void		cobc_print_usage_flags (void);

/* config.c */

#define	CB_CONFIG_ANY(type,var,name,doc)	\
extern type			var;
#define	CB_CONFIG_INT(var,name,min,max,odoc,doc)	\
extern unsigned int		var;
#define	CB_CONFIG_SINT(var,name,min,max,odoc,doc)	\
extern int		var;
#define	CB_CONFIG_SIZE(var,name,min,max,odoc,doc)	\
extern unsigned long	var;
#define	CB_CONFIG_STRING(var,name,doc)	\
extern const char		*var;
#define	CB_CONFIG_BOOLEAN(var,name,doc)	\
extern int				var;
#define	CB_CONFIG_SUPPORT(var,name,doc)	\
extern enum				cb_support var;

#include "config.def"

#undef	CB_CONFIG_ANY
#undef	CB_CONFIG_INT
#undef	CB_CONFIG_SINT
#undef	CB_CONFIG_SIZE
#undef	CB_CONFIG_STRING
#undef	CB_CONFIG_BOOLEAN
#undef	CB_CONFIG_SUPPORT

extern int		cb_load_std (const char *);
extern int		cb_config_entry (char *, const char *, const int);
extern int		cb_load_conf (const char *, const int);
extern int		cb_load_words (void);

#ifndef	HAVE_DESIGNATED_INITS
/* "static" initialization routines in several files */
extern void		cobc_init_typeck (void);
extern void		cobc_init_reserved (void);
extern void		cobc_init_tree (void);
extern void		cobc_init_codegen (void);
#endif

/* preprocessor (in pplex.l, ppparse.y) */
#if	!defined (COB_IN_SCANNER ) && !defined (COB_IN_PPLEX)
extern FILE		*ppin;
extern FILE		*ppout;
extern int		pplex (void);
#endif

#ifndef	COB_IN_PPPARSE
extern int		ppparse (void);
#endif

extern int		ppopen (const char *, struct cb_replace_list *);
extern int		ppcopy (const char *, const char *,
				struct cb_replace_list *);
extern void		pp_set_replace_list (struct cb_replace_list *,
					     const cob_u32_t);
extern void		ppparse_error (const char *);


/* parser (in scanner.l, parser.y) */
#if	!defined (COB_IN_SCANNER ) && !defined (COB_IN_PPLEX) && \
	!defined (COB_IN_PPPARSE)
extern FILE		*yyin;
extern FILE		*yyout;
extern int		yylex (void);
#endif

#if	!defined (COB_IN_PPPARSE) && !defined (COB_IN_PARSER)
extern int		yyparse (void);
#endif

extern void		ylex_clear_all (void);
extern void		ylex_call_destroy (void);

/* typeck.c */
extern size_t		suppress_warn;	/* no warnings for internal generated stuff */

/* codeoptim.c */
extern void		cob_gen_optim (const enum cb_optim);

/* codegen.c */
extern void		cb_init_codegen (void);

/* error.c */
#define CB_MSG_STYLE_GCC	0
#define CB_MSG_STYLE_MSC	1U

#define CB_PENDING_X(x,s) \
	do { cb_warning_x (cb_warn_pending, x, _("%s is not implemented"), s); } ONCE_COB

#define CB_UNFINISHED_X(x,s) \
	do { cb_warning_x (cb_warn_unfinished, x, \
		_("handling of %s is unfinished; implementation is likely to be changed"), s); \
	} ONCE_COB
#define CB_PENDING(s)		CB_PENDING_X	(cb_error_node, s)
#define CB_UNFINISHED(s)	CB_UNFINISHED_X	(cb_error_node, s)
#define CB_UNSUPPORTED(x) \
	do { cb_error (_("%s is not supported"), x); } ONCE_COB
#define CB_UNSUPPORTED_X(x,y) \
	do { cb_error_x (x, _("%s is not supported"), y); } ONCE_COB

extern size_t		cb_msg_style;

extern enum cb_warn_val		cb_warning (const enum cb_warn_opt, const char *, ...) COB_A_FORMAT23;
extern enum cb_warn_val		cb_error (const char *, ...) COB_A_FORMAT12;
extern void		cb_error_always (const char *, ...) COB_A_FORMAT12;
extern void		cb_perror (const int, const char *, ...) COB_A_FORMAT23;
extern void		cb_plex_warning (const enum cb_warn_opt, const size_t,
					 const char *, ...) COB_A_FORMAT34;
extern void		cb_plex_error (const size_t,
					 const char *, ...) COB_A_FORMAT23;
extern unsigned int	cb_plex_verify (const size_t, const enum cb_support,
					const char *);
extern void		configuration_warning (const char *, const int,
					 const char *, ...) COB_A_FORMAT34;
extern void		configuration_error (const char *, const int,
					 const int, const char *, ...) COB_A_FORMAT45;
extern char *		cb_get_strerror (void);
extern void		cb_add_error_to_listing (const char *, int, const char *, char *);
DECLNORET extern void		flex_fatal_error (const char *, const char *,
					 const int) COB_A_NORETURN;

/* reserved.c */
extern struct reserved_word_list	*cobc_user_res_list;

extern void		remove_reserved_word (const char *, const char *, const int);
extern void		add_reserved_word (const char *, const char *, const int);
extern void		remove_reserved_word_now (const char * const);
extern void		add_reserved_word_now (char * const, char * const);

extern void		remove_register (const char *, const char *, const int);
extern void		add_register (const char *, const char *, const int);

extern void		deactivate_intrinsic (const char *, const char *, const int);
extern void		activate_intrinsic (const char *, const char *, const int);

extern void		deactivate_system_name (const char *, const char *, const int);
extern void		activate_system_name (const char *, const char *, const int);

extern int		cb_strcasecmp (const void *, const void *);
extern unsigned char	cb_toupper (const unsigned char);
extern unsigned char	cb_tolower (const unsigned char);

#endif /* CB_COBC_H */

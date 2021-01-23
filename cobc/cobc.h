/*
   Copyright (C) 2001-2012, 2014-2017 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Simon Sobisch,
   Edward Hart, Dave Pitts, Sergey Kashyrin

   This file is part of GnuCOBOL C++.

   The GnuCOBOL C++ compiler is free software: you can redistribute it
   and/or modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   GnuCOBOL C++ is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GnuCOBOL C++.  If not, see <http://www.gnu.org/licenses/>.
*/


#ifndef CB_COBC_H
#define CB_COBC_H

#include <stdio.h>
#ifdef	HAVE_UNISTD_H
	#include <unistd.h>
#endif
#ifdef	HAVE_STRINGS_H
	#include <strings.h>
#endif

#include "libcob.h"

#ifdef	ENABLE_NLS
	#include "lib/gettext.h"
	#define _(s)		gettext(s)
	#define N_(s)		gettext_noop(s)
#else
	#define _(s)		s
	#define N_(s)		s
#endif

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
#define YY_FATAL_ERROR(msg)		flex_fatal_error(msg, __FILE__, __LINE__)

/* Source format enum */
enum cb_format {
	CB_FORMAT_FIXED = 0,
	CB_FORMAT_FREE
};

/* COPY extended syntax defines */
#define CB_REPLACE_LEADING		1U
#define CB_REPLACE_TRAILING		2U

/* Stringify macros */
#define CB_STRINGIFY(s)			#s
#define CB_XSTRINGIFY(s)		CB_STRINGIFY(s)
#define CB_XRANGE(min,max)		CB_XSTRINGIFY(min) ".." CB_XSTRINGIFY(max)

/* ASSIGN clause interpretation */
#define CB_ASSIGN_MF			0	/* Micro Focus compatibility */
#define CB_ASSIGN_IBM			1U	/* IBM compatibility */
#define CB_ASSIGN_COBOL2002		2U	/* COBOL 2002 standard */

/* COMP/BINARY byte order */
#define CB_BYTEORDER_BIG_ENDIAN	0
#define CB_BYTEORDER_NATIVE		1U

/* Binary field sizes */
#define CB_BINARY_SIZE_1_2_4_8	0	/* 1,2,4,8 bytes */
#define CB_BINARY_SIZE_1__8		1U	/* 1,2,3,4,5,6,7,8 bytes */
#define CB_BINARY_SIZE_2_4_8	2U	/* 2,4,8 bytes */

/* Flex directive actions */
#define PLEX_ACT_IF			0
#define PLEX_ACT_ELSE			1U
#define PLEX_ACT_END			2U
#define PLEX_ACT_ELIF			3U

/* Flex value types */
#define PLEX_DEF_NONE			0
#define PLEX_DEF_LIT			1U
#define PLEX_DEF_NUM			2U
#define PLEX_DEF_DEL			3U

/* Context sensitive keyword defines(trigger words) */
#define	CB_CS_ACCEPT		(1U << 0)
#define CB_CS_ALLOCATE		(1U << 1)
#define	CB_CS_ALPHABET		(1U << 2)
#define	CB_CS_ASSIGN		(1U << 3)
#define	CB_CS_CALL			(1U << 4)
#define	CB_CS_CONSTANT		(1U << 5)
#define	CB_CS_DATE			(1U << 6)
#define	CB_CS_DAY			(1U << 7)
#define	CB_CS_DISPLAY		(1U << 8)
#define	CB_CS_ERASE			(1U << 9)
#define	CB_CS_EXIT			(1U << 10)
#define	CB_CS_FROM			(1U << 11)
#define	CB_CS_OCCURS		(1U << 12)
#define CB_CS_OPTIONS		(1U << 13)
#define	CB_CS_PERFORM		(1U << 14)
#define	CB_CS_PROGRAM_ID	(1U << 15)
#define	CB_CS_READ			(1U << 16)
#define	CB_CS_RECORDING		(1U << 17)
#define	CB_CS_RETRY			(1U << 18)
#define	CB_CS_ROUNDED		(1U << 19)
#define	CB_CS_SET			(1U << 20)
#define	CB_CS_STOP			(1U << 21)
#define	CB_CS_OBJECT_COMPUTER	(1U << 22)

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

#define COBC_WARN_FILLER  -1
#define COBC_WARN_DISABLED 0
#define COBC_WARN_ENABLED  1
#define COBC_WARN_AS_ERROR 2

/* Config dialect support types */
enum cb_std_def {
	CB_STD_OC = 0,
	CB_STD_MF,
	CB_STD_IBM,
	CB_STD_MVS,
	CB_STD_BS2000,
	CB_STD_ACU,
	CB_STD_RM,
	/* the following must contain ANSI/ISO standards in order */
	CB_STD_85,
	CB_STD_2002,
	CB_STD_2014
};

/* Generic text list structure */
struct cb_text_list {
	cb_text_list *	next;			/* next pointer */
	cb_text_list *	last;
	const char *	text;
};

/* Generic replace list structure */
struct cb_replace_list {
	cb_replace_list *	next;			/* next pointer */
	cb_replace_list *	last;
	cb_replace_list *	prev;
	const cb_text_list * old_text;
	const cb_text_list * new_text;
	unsigned int		lead_trail;
	int					line_num;
};

/* Generic define list structure */
struct cb_define_struct {
	cb_define_struct *	next;			/* next pointer */
	cb_define_struct *	last;
	char 		*		name;
	char 		*		value;
	unsigned int		deftype;
	int					sign;
	int					int_part;
	int					dec_part;
};

/* Structure for extended filenames */
struct local_filename {
	local_filename *	next;			/* next pointer */
	char 		*		local_name;
	FILE 		*		local_fp;
};

/* Structure for filename */
struct filename {
	filename 	*	next;
	const char *	source;				/* foo.cob (path from command line) */
	const char *	preprocess;			/* foo.i (full path) */
	const char *	translate;			/* foo.cpp (full path) */
	const char *	trstorage;			/* foo.cpp.h (full path) */
	const char *	object;				/* foo.o (full path) */
	const char *	demangle_source;	/* foo */
	const char *	listing_file;		/* foo.lst */
	local_filename * localfile;			/* foo.cpp.l[n].h */
	size_t			translate_len;		/* strlen translate */
	size_t			object_len;			/* strlen object */
	unsigned int	need_preprocess;	/* Needs preprocess */
	unsigned int	need_translate;		/* Needs parse */
	unsigned int	need_assemble;		/* Needs C compile */
	int				has_error;			/* Error detected */
	int				file_is_stdin;		/* dash used as filename */
};

/* Exception structure */
struct cb_exception {
	const char *	name;			/* Exception name */
	int				code;			/* Exception code */
	int				enable;			/* If turned on */
};

/* Basic memory structure */
struct cobc_mem_struct {
	cobc_mem_struct *	next;			/* next pointer */
	void 		*		memptr;
	size_t				memlen;
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
	list_error *	next;
	int				line;		/* Line number for error */
	char		*	file;		/* File name */
	char 	*		prefix;		/* Error prefix */
	char 	*		msg;		/* Error Message text */
	list_error()
	{
		memset(this, 0, sizeof(list_error));
	}
	~list_error()
	{
		if(file) {
			delete [] file;
		}
		if(prefix) {
			delete [] prefix;
		}
		if(msg) {
			delete [] msg;
		}
		if(next) {
			delete next;
		}
	}
};

/* List of REPLACE text blocks */
struct list_replace {
	list_replace *	next;
	int				firstline;	/* First line for replace */
	int				lastline;	/* Last line for replace */
	int				lead_trail;	/* LEADING/TRAILING flag */
	char 	*		from;		/* Old (from) text */
	char 	*		to;			/* New (to) text */
	list_replace()
	{
		memset(this, 0, sizeof(list_replace));
	}
	~list_replace()
	{
		if(from) {
			delete [] from;
		}
		if(to) {
			delete [] to;
		}
		if(next) {
			delete next;
		}
	}
};

/* List of skipped lines (conditional compilation) */
struct list_skip {
	list_skip *	next;
	int			skipline;	/* line number of skipped line */
	list_skip() : next(NULL), skipline(0) {}
	~list_skip()
	{
		if(next) {
			delete next;
		}
	}
};

/* Listing file control structure */
struct list_files {
	list_files *	next;
	list_files *	copy_head;		/* COPY book list head */
	list_files *	copy_tail;		/* COPY book list tail */
	list_error *	err_head;		/* Error message list head */
	list_replace *	replace_head;	/* REPLACE list head */
	list_replace *	replace_tail;	/* REPLACE list tail */
	list_skip 	*	skip_head;		/* Skip list head */
	list_skip 	*	skip_tail;		/* Skip list tail */
	int 			copy_line;		/* Line start for copy book */
	int 			listing_on;		/* Listing flag for this file */
	enum cb_format	source_format;	/* source format for file */
	const char *	name;			/* Name of this file */
	list_files()
	{
		memset(this, 0, sizeof(list_files));
	}
	~list_files()
	{
		if(copy_head) {
			delete copy_head;
		}
		if(err_head) {
			delete err_head;
		}
		if(replace_head) {
			delete replace_head;
		}
		if(skip_head) {
			delete skip_head;
		}
		if(name) {
			delete [] name;
		}
		if(next) {
			delete next;
		}
	}
};

extern list_files *	cb_listing_files;
extern list_files *	cb_current_file;

extern enum cb_format	cb_source_format;
extern int			cb_text_column;

extern cb_exception	cb_exception_table[];

#define CB_EXCEPTION_NAME(id)	cb_exception_table[id].name
#define CB_EXCEPTION_CODE(id)	cb_exception_table[id].code
#define CB_EXCEPTION_ENABLE(id)	cb_exception_table[id].enable

/* undef macros that are only for internal use with def-files */

#undef	CB_FLAG
#undef	CB_FLAG_ON
#undef	CB_FLAG_RQ
#undef	CB_FLAG_NQ

#undef	CB_WARNDEF
#undef	CB_ONWARNDEF
#undef	CB_NOWARNDEF

#undef	CB_OPTIM_DEF

#undef	CB_CONFIG_ANY
#undef	CB_CONFIG_INT
#undef	CB_CONFIG_STRING
#undef	CB_CONFIG_BOOLEAN
#undef	CB_CONFIG_SUPPORT

#undef	COB_EXCEPTION

#define	CB_FLAG(var,print_help,name,doc)		extern int var;
#define	CB_FLAG_ON(var,print_help,name,doc)		extern int var;
#define CB_FLAG_RQ(var,print_help,name,def,opt,doc)	extern int var;
#define CB_FLAG_NQ(print_help,name,opt,doc)
#include "flag.def"
#undef	CB_FLAG
#undef	CB_FLAG_ON
#undef	CB_FLAG_RQ
#undef	CB_FLAG_NQ

#define	CB_WARNDEF(var,name,doc)	extern int var;
#define	CB_ONWARNDEF(var,name,doc)	extern int var;
#define	CB_NOWARNDEF(var,name,doc)	extern int var;
#include "warning.def"
#undef	CB_WARNDEF
#undef	CB_ONWARNDEF
#undef	CB_NOWARNDEF

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
extern int			cobc_flag_main;
extern int			cb_flag_functions_all;
extern int			cb_flag_main;
extern int			cobc_wants_debug;
extern int			cb_listing_xref;
extern int			cobc_seen_stdin;

extern int			errorcount;
extern int			warningcount;
extern int			warningopt;
extern int			no_physical_cancel;
extern cob_u32_t	optimize_defs[];

extern const char *	cb_cobc_build_stamp;
extern const char *	cb_source_file;
extern int			cb_source_line;

extern cob_time		current_compile_time;
extern tm			current_compile_tm;

extern const char *	cob_config_dir;

extern unsigned int	cobc_gen_listing;

extern const char *	demangle_name;
extern FILE 	*	cb_storage_file;
extern const char *	cb_storage_file_name;

extern char 	**	cb_saveargv;
extern int			cb_saveargc;

extern FILE 	*	cb_listing_file;
extern FILE 	*	cb_src_list_file;
extern cb_text_list * cb_include_list;
extern cb_text_list * cb_intrinsic_list;
extern cb_text_list * cb_extension_list;
extern cb_text_list * cb_static_call_list;
extern cb_text_list * cb_early_exit_list;

extern struct cb_program 	* current_program;
extern struct cb_statement * current_statement;
extern struct cb_label *	current_section;
extern struct cb_label *	current_paragraph;
extern int			cb_exp_line;
extern int			functions_are_all;
extern struct cb_tree_common *	defined_prog_list;
extern int			current_call_convention;

/* Functions */

// cobc.cpp

extern struct reserved_word_list * cob_user_res_list;

char 	*		cobc_strdup(const char *);

void 	*		cobc_main_malloc(const size_t);
char 	*		cobc_main_strdup(const char *);
void 	*		cobc_main_realloc(void *, const size_t);
void			cobc_main_free(void *);

void 	*		cobc_parse_malloc(const size_t);
char 	*		cobc_parse_strdup(const char *);
void			cobc_parse_free(void *);

void 	*		cobc_plex_malloc(const size_t);
char 	*		cobc_plex_strdup(const char *);

void 	*		cobc_check_string(const char *);
void			cobc_err_msg(const char *, ...) COB_A_FORMAT12;

DECLNORET void	cobc_abort(const char *, const int) COB_A_NORETURN;
DECLNORET void	cobc_too_many_errors(void) COB_A_NORETURN;
void			cobc_dumb_abort(const char *, const int);

size_t	cobc_check_valid_name(const char *, const cobc_name_type);

/* config.c */

#undef	CB_CONFIG_ANY
#undef	CB_CONFIG_INT
#undef	CB_CONFIG_STRING
#undef	CB_CONFIG_BOOLEAN
#undef	CB_CONFIG_SUPPORT

#define	CB_CONFIG_ANY(type,var,name,doc)	extern type var;
#define	CB_CONFIG_INT(var,name,min,max,odoc,doc)	extern unsigned int var;
#define	CB_CONFIG_STRING(var,name,doc)	extern const char * var;
#define	CB_CONFIG_BOOLEAN(var,name,doc)	extern int var;
#define	CB_CONFIG_SUPPORT(var,name,doc)	extern enum cb_support var;

#include "config.def"

#undef	CB_CONFIG_ANY
#undef	CB_CONFIG_INT
#undef	CB_CONFIG_STRING
#undef	CB_CONFIG_BOOLEAN
#undef	CB_CONFIG_SUPPORT

int				cb_load_std(const char *);
int				cb_config_entry(char *, const char *, const int);
int				cb_load_conf(const char *, const int);
int				cb_load_words(void);

/* Initialization routines in typeck.c and reserved.c */
void			cobc_init_typeck(void);
void			cobc_init_reserved(void);

/* preprocessor(in pplex.l, ppparse.y) */
#if	!defined(COB_IN_SCANNER ) && !defined(COB_IN_PPLEX)
	extern FILE *	ppin;
	extern FILE *	ppout;
	int				pplex(void);
#endif

int				ppparse(void);
int				ppopen(const char *, cb_replace_list *);
int				ppcopy(const char *, const char *, cb_replace_list *);
void			pp_set_replace_list(cb_replace_list *, const cob_u32_t);
void			ppparse_error(const char *);
void			ppparse_clear_vars(const cb_define_struct *);
cb_define_struct * ppp_search_lists(const char * name);
void			ppp_clear_lists(void);
void			plex_clear_vars(void);
void			plex_clear_all(void);
void			plex_call_destroy(void);
void			plex_action_directive(const unsigned int, const unsigned int);

// parser(in scanner.l, parser.y)
#if	!defined(COB_IN_SCANNER ) && !defined(COB_IN_PPLEX) && !defined(COB_IN_PPPARSE)
	extern FILE *	yyin;
	extern FILE *	yyout;
#endif

int				yylex(void);
int				yyparse(void);
void			ylex_clear_all(void);
void			ylex_call_destroy(void);

// typeck.cpp
extern size_t	suppress_warn;

// codeoptim.cpp
void			cob_gen_optim(const enum cb_optim);

// codegen.cpp
unsigned int	chk_field_variable_address(struct cb_field *);

// naming.cpp
void			externalize_tree(struct cb_program *, struct cb_field **, bool bWS = false);
void			naming_init();

// error.cpp
#define CB_MSG_STYLE_GCC	0
#define CB_MSG_STYLE_MSC	1U

#define CB_PENDING(x) \
	cb_warning(cb_warn_pending, _("%s is not implemented"), x)
#define CB_PENDING_X(x,y) \
	cb_warning_x(cb_warn_pending, x, _("%s is not implemented"), y)
#define CB_UNFINISHED(x) \
	cb_warning(cb_warn_unfinished, _("handling of %s is unfinished; implementation is likely to be changed"), x)
#define CB_UNFINISHED_X(x,y) \
	cb_warning_x(cb_warn_unfinished, x, _("handling of %s is unfinished; implementation is likely to be changed"), y)

extern size_t		cb_msg_style;

void		cb_warning(int, const char *, ...) COB_A_FORMAT23;
void		cb_error(const char *, ...) COB_A_FORMAT12;
void		cb_perror(const int, const char *, ...) COB_A_FORMAT23;
void		cb_plex_warning(int, const size_t,
							const char *, ...) COB_A_FORMAT34;
void		cb_plex_error(const size_t,
						  const char *, ...) COB_A_FORMAT23;
unsigned int	cb_plex_verify(const size_t, const enum cb_support,
							   const char *);
void		configuration_warning(const char *, const int,
								  const char *, ...) COB_A_FORMAT34;
void		configuration_error(const char *, const int,
								const int, const char *, ...) COB_A_FORMAT45;
char 	*	cb_get_strerror(void);
DECLNORET void		flex_fatal_error(const char *, const char *,
									 const int) COB_A_NORETURN;
unsigned int	cb_verify(const enum cb_support, const char *);

// reserved.cpp
extern struct reserved_word_list	* cobc_user_res_list;

void		remove_reserved_word(const char *, const char *, const int);
void		add_reserved_word(const char *, const char *, const int);

void		remove_register(const char *, const char *, const int);
void		add_register(const char *, const char *, const int);

void		deactivate_intrinsic(const char *, const char *, const int);
void		activate_intrinsic(const char *, const char *, const int);

void		deactivate_system_name(const char *, const char *, const int);
void		activate_system_name(const char *, const char *, const int);

/////////////////////////// SKA DEBUG //////////////////////
void skadbg(const char * fmt, ...);
/////////////////////////// SKA DEBUG //////////////////////
#endif /* CB_COBC_H */

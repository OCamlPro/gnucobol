/*
   Copyright (C) 2003-2024 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Ron Norman, Simon Sobisch,
   Edward Hart

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
#include <time.h>
#include <limits.h>


#include "cobc.h"
#include "tree.h"

#if !defined(COB_ALIGN_KNOWN) && !defined(COB_ALLOW_UNALIGNED)
#error System requires data alignment which is unknown
#endif

#define COB_MAX_SUBSCRIPTS	16

#define COB_MALLOC_ALIGN	15

#define COB_INSIDE_SIZE		64

/* Type of initialization to be done */
enum cobc_init_type {
	INITIALIZE_NONE = 0,	/* no init (beause of FILLER, REDEFINES, ...) */
	INITIALIZE_ONE,		/* initialize a single variable */
	INITIALIZE_COMPOUND,	/* init structure */
	INITIALIZE_DEFAULT	/* init to default-byte value / PIC (USAGE) */
};

#define CB_NEED_HIGH		(1U << 0)
#define CB_NEED_LOW		(1U << 1)
#define CB_NEED_QUOTE		(1U << 2)
#define CB_NEED_SPACE		(1U << 3)
#define CB_NEED_ZERO		(1U << 4)

struct sort_list {
	struct sort_list	*next;
};

struct system_table {
	const char		*syst_name;
	const char		*syst_call;
	const unsigned int	syst_max_params;
};

struct label_list {
	struct label_list	*next;
	int			id;
	int			call_num;
};

struct string_list {
	struct string_list	*next;
	char			*text;
	int			id;
};

struct pic_list {
	struct pic_list		*next;
	const cob_pic_symbol	*str;
	int			length;
	int			id;
};

struct attr_list {
	struct attr_list	*next;
	int			pic_id;
	int			id;
	int			type;
	cob_u32_t		digits;
	int			scale;
	cob_u32_t		flags;
};

struct field_list {
	struct field_list	*next;
	struct cb_field		*f;
	cb_tree			x;
	const char		*curr_prog;
};

struct call_list {
	struct call_list	*next;
	const char		*call_name;
};

#define COB_RETURN_INT		0
#define COB_RETURN_ADDRESS_OF	1
#define COB_RETURN_NULL		2
struct static_call_list {
	struct static_call_list	*next;
	const char		*call_name;
	int			convention;
	int			return_type;
};

struct base_list {
	struct base_list	*next;
	struct cb_field		*f;
	const char		*curr_prog;
};

/* variable set in cobc.c during option parsing, see tree.h */
int		cb_flag_memory_check = 0;

/* Local variables */

static struct pic_list		*pic_cache = NULL;
static struct attr_list		*attr_cache = NULL;
static struct literal_list	*literal_cache = NULL;
static struct field_list	*field_cache = NULL;
static struct field_list	*local_field_cache = NULL;
static struct call_list		*call_cache = NULL;
static struct call_list		*func_call_cache = NULL;
static struct static_call_list	*static_call_cache = NULL;
static struct base_list		*base_cache = NULL;
static struct base_list		*globext_cache = NULL;
static struct base_list		*local_base_cache = NULL;
static struct string_list	*string_cache = NULL;
static struct string_list	*source_cache = NULL;
static struct label_list	*label_cache = NULL;
static struct ml_tree_list	*ml_tree_cache = NULL;


static FILE			*output_target = NULL;
static char			*output_name = NULL;
static unsigned int		output_line_number = 0;
static FILE			*cb_local_file = NULL;
static const char		*excp_current_program_id = NULL;
static const char		*excp_current_section = NULL;
static const char		*excp_current_paragraph = NULL;
static struct cb_program	*current_prog = NULL;    /* program in codegen (only) */
static struct cb_program	*recent_prog = NULL;

static const struct cb_label		*last_section = NULL;
#ifdef	GEN_SINGLE_MEMCPY
static unsigned char		*litbuff = NULL;
static int			litsize = 0;
#endif

static unsigned int		has_global_file = 0;
static unsigned int		needs_exit_prog = 0;
static unsigned int		needs_unifunc = 0;
static unsigned int		need_save_exception = 0;
static unsigned int		gen_nested_tab = 0;
static unsigned int		gen_ascii_ebcdic = 0;
static unsigned int		gen_ebcdic_ascii = 0;
static unsigned int		gen_native = 0;
static unsigned int		gen_custom = 0;
static unsigned int		gen_figurative = 0;
static unsigned int		gen_dynamic = 0;
static char			last_line_num[80] = "";
static int			skip_line_num = 0;
static int			report_field_id = 0;
static char			report_field_name[24] = "";

static int			param_id = 0;
static int			stack_id = 0;
static int			string_id = 1;
static int			source_id = 1;
static int			num_cob_fields = 0;
static int			non_nested_count = 0;
static int			loop_counter = 0;
static int			progid = 0;
static int			last_line = 0;
static int			last_stmt = 0;
static cob_u32_t		field_iteration = 0;
static int			screenptr = 0;
static int			local_mem = 0;
static int			working_mem = 0;
static int			local_working_mem = 0;
static int			output_indent_level = 0;
static int			last_segment = 0;
static int			gen_init_working = 0;	/* enable (0) / disable (1) use of DEPENDING ON fields */
static int			need_plus_sign = 0;
static int			odo_stop_now = 0;
static int			gen_num_lit_big_end = 1;
static int			r_ctl_id = 0;
static int			r_source_id = 0;
static unsigned int		nolitcast = 0;

static unsigned int		in_cond = 0;
static unsigned int		inside_check = 0;
static unsigned int		inside_stack[COB_INSIDE_SIZE];

static unsigned int		i_len_used = 0;
static unsigned int		i_counters[COB_MAX_SUBSCRIPTS];
static const cob_s64_t	cob_exp10_ll[19] = {
	COB_S64_C(1),
	COB_S64_C(10),
	COB_S64_C(100),
	COB_S64_C(1000),
	COB_S64_C(10000),
	COB_S64_C(100000),
	COB_S64_C(1000000),
	COB_S64_C(10000000),
	COB_S64_C(100000000),
	COB_S64_C(1000000000),
	COB_S64_C(10000000000),
	COB_S64_C(100000000000),
	COB_S64_C(1000000000000),
	COB_S64_C(10000000000000),
	COB_S64_C(100000000000000),
	COB_S64_C(1000000000000000),
	COB_S64_C(10000000000000000),
	COB_S64_C(100000000000000000),
	COB_S64_C(1000000000000000000)
};

#undef	COB_SYSTEM_GEN
#define	COB_SYSTEM_GEN(cob_name, pmin, pmax, c_name)	{ cob_name, #c_name, pmax },

static const struct system_table	system_tab[] = {
#include "../libcob/system.def"
	{ NULL, NULL, 0 }
};

#undef	COB_SYSTEM_GEN

#ifdef	HAVE_DESIGNATED_INITS
#define COB_STATEMENT(ename,str)	, [ename] = CB_STRINGIFY (ename)
const char	*cb_statement_enum_name[STMT_MAX_ENTRY] = {
	[STMT_UNKNOWN] = CB_STRINGIFY (STMT_UNKNOWN)
#include "../libcob/statement.def"
	/* note: STMT_MAX_ENTRY left out here */
};
#undef COB_STATEMENT
#else
const char	*cb_statement_enum_name[STMT_MAX_ENTRY];
#endif

/* Declarations */
static void output_occurs (struct cb_field *);
static void output (const char *, ...)		COB_A_FORMAT12;
static void output_line (const char *, ...)	COB_A_FORMAT12;
static void output_storage (const char *, ...)	COB_A_FORMAT12;
static void output_local (const char *, ...)	COB_A_FORMAT12;

static int	out_odoslide_grp_offset	(struct cb_field *p, struct cb_field *fld);
static void	out_odoslide_grp_size (struct cb_field *p, struct cb_field *fld);

static void output_stmt		(cb_tree);
static void output_integer	(cb_tree);
static void output_index	(cb_tree);
static void output_func_1	(const char *, cb_tree);
static void output_param	(cb_tree, int);
static void output_funcall	(cb_tree);
static void output_report_summed_field (struct cb_field *);
static int	any_source_moves (struct cb_report *r, struct cb_field *f, int first);
static struct cb_field * real_field_founder (const struct cb_field *f);
static void add_field_cache (struct cb_field *f01, struct cb_field *f);

static void output_line_and_trace_info (cb_tree x, const enum cob_statement stmnt);
static void output_source_reference (cb_tree, const enum cob_statement);

static void codegen_init (struct cb_program *, const char *);
static void codegen_internal (struct cb_program *, const int);
static void codegen_finalize (void);

static void output_perform_once (struct cb_perform *);

/* Local functions */

static void
count_all_fields (struct cb_field *p)
{
	struct cb_field	*f, *f01;
	cb_tree		l;
	if (p->flag_internal_register) {
		return;
	}
	if (p->storage == CB_STORAGE_REPORT) {
		f01 = real_field_founder (p);
		if (!f01->flag_base) {
			add_field_cache (f01, p);
		}
	}
	if (p->sister) {
		count_all_fields (p->sister);
	}
	if (p->children) {
		count_all_fields (p->children);
	}
	p->flag_sym_emitted = 0;

	if (p->storage == CB_STORAGE_REPORT
	 || p->level == 1 
	 || p->level == 77) {		/* Make sure cob_field is emitted */
		p->count++;
		for (l = p->index_list; l; l = CB_CHAIN (l)) {
			f = CB_FIELD_PTR (CB_VALUE (l));
			f->count++;
		}
	}
}

struct cb_field *
cb_code_field (cb_tree x)
{
	if (CB_REFERENCE_P (x)) {
		cb_tree f = CB_REFERENCE (x)->value;
		if (!f) {
			f = cb_ref (x);
		}
		return CB_FIELD (f);
	}
	if (CB_LIST_P (x)) {
		return cb_code_field (CB_VALUE (x));
	}
	return CB_FIELD (x);
}

static const char *
get_field_name (const struct cb_field *f)
{
	static char wrk[64];
	if (memcmp (f->name, "FILLER ",7) == 0
	 || f->flag_filler) {
		sprintf (wrk,"Line %d",f->common.source_line);
		return wrk;
	}
	return f->name;
}

static int
lookup_string (const char *p)
{
	struct string_list *stp;

	for (stp = string_cache; stp; stp = stp->next) {
		if (strcmp (p, stp->text) == 0) {
			return stp->id;
		}
	}
	stp = cobc_parse_malloc (sizeof (struct string_list));
	stp->text = cobc_parse_strdup (p);
	stp->id = string_id;
	stp->next = string_cache;
	string_cache = stp;
	return string_id++;
}

static int
lookup_source (const char *p)
{
	struct string_list *stp;

	for (stp = source_cache; stp; stp = stp->next) {
		if (strcmp (p, stp->text) == 0) {
			return stp->id;
		}
	}
	stp = cobc_parse_malloc (sizeof (struct string_list));
	stp->text = cobc_parse_strdup (p);
	stp->id = source_id;
	stp->next = source_cache;
	source_cache = stp;
	return source_id++;
}

static void
lookup_call (const char *p)
{
	struct call_list *clp;

	for (clp = call_cache; clp; clp = clp->next) {
		if (strcmp (p, clp->call_name) == 0) {
			return;
		}
	}
	clp = cobc_parse_malloc (sizeof (struct call_list));
	clp->call_name = p;
	clp->next = call_cache;
	call_cache = clp;
}

static void
lookup_func_call (const char *p)
{
	struct call_list *clp;

	for (clp = func_call_cache; clp; clp = clp->next) {
		if (strcmp (p, clp->call_name) == 0) {
			return;
		}
	}
	clp = cobc_parse_malloc (sizeof (struct call_list));
	clp->call_name = p;
	clp->next = func_call_cache;
	func_call_cache = clp;
}

static void
lookup_static_call (const char *p, int convention, int return_type)
{
	struct static_call_list *sclp;

	for (sclp = static_call_cache; sclp; sclp = sclp->next) {
		if (strcmp (p, sclp->call_name) == 0) {
			return;
		}
	}
	sclp = cobc_parse_malloc (sizeof (struct static_call_list));
	sclp->call_name = p;
	sclp->convention = convention;
	sclp->return_type = return_type;
	sclp->next = static_call_cache;
	static_call_cache = sclp;
}

#define LIST_REVERSE_FUNC(list_struct)		      \
	static struct list_struct *		      \
	list_struct##_reverse (struct list_struct *p) \
	{					      \
		struct list_struct	*next;	      \
		struct list_struct	*last;	      \
						      \
		last = NULL;			      \
		for (; p; p = next) {		      \
			next = p->next;		      \
			p->next = last;		      \
			last = p;		      \
		}				      \
		return last;			      \
	}

LIST_REVERSE_FUNC (call_list)
LIST_REVERSE_FUNC (static_call_list)
LIST_REVERSE_FUNC (pic_list)
LIST_REVERSE_FUNC (attr_list)
LIST_REVERSE_FUNC (string_list)
LIST_REVERSE_FUNC (literal_list)

static int field_cache_cmp (const void *mp1, const void *mp2)
{
	const struct field_list	*fl1;
	const struct field_list	*fl2;
	int			ret;

	fl1 = (const struct field_list *)mp1;
	fl2 = (const struct field_list *)mp2;
	ret = strcasecmp (fl1->curr_prog, fl2->curr_prog);
	if (ret) {
		return ret;
	}
	return fl1->f->id - fl2->f->id;
}

static int base_cache_cmp (const void *mp1, const void *mp2)
{
	const struct base_list	*fl1;
	const struct base_list	*fl2;

	fl1 = (const struct base_list *)mp1;
	fl2 = (const struct base_list *)mp2;
	return fl1->f->id - fl2->f->id;
}

/* Sort a structure linked list in place */
/* Assumed that pointer "next" is first item in structure */

static void *
list_cache_sort (void *inlist, int (*cmpfunc)(const void *mp1, const void *mp2))
{
	struct sort_list	*p;
	struct sort_list	*q;
	struct sort_list	*e;
	struct sort_list	*tail;
	struct sort_list	*list;
	size_t			insize;
	size_t			nmerges;
	size_t			psize;
	size_t			qsize;
	size_t			i;

	if (!inlist) {
		return NULL;
	}
	list = (struct sort_list *)inlist;
	insize = 1;
	for (;;) {
		p = list;
		list = NULL;
		tail = NULL;
		nmerges = 0;
		while (p) {
			nmerges++;
			q = p;
			psize = 0;
			for (i = 0; i < insize; i++) {
				psize++;
				q = q->next;
				if (!q) {
					break;
				}
			}
			qsize = insize;
			while (psize > 0 || (qsize > 0 && q)) {
				if (psize == 0) {
					e = q;
					q = q->next;
					if (qsize) {
						qsize--;
					}
				} else if (qsize == 0 || !q) {
					e = p;
					p = p->next;
					psize--;
				} else if ((*cmpfunc) (p, q) <= 0) {
					e = p;
					p = p->next;
					psize--;
				} else {
					e = q;
					q = q->next;
					qsize--;
				}
				if (tail) {
					tail->next = e;
				} else {
					list = e;
				}
				tail = e;
			}
			p = q;
		}
		if (tail) tail->next = NULL;
		if (nmerges <= 1) {
			return (void *)list;
		}
		insize *= 2;
	}
}

/* Clear local variables */
void
clear_local_codegen_vars (void)
{
	attr_cache = NULL;
	base_cache = NULL;
	call_cache = NULL;
	field_cache = NULL;
	func_call_cache = NULL;
	globext_cache = NULL;
	label_cache = NULL;
	literal_cache = NULL;
	local_base_cache = NULL;
	local_field_cache = NULL;
	static_call_cache = NULL;
	string_cache = NULL;
	ml_tree_cache = NULL;

	/* local referenced "program under codegen" */
	current_prog = NULL;

	/* variables defined in the parser */
	current_program = NULL;
	current_section = NULL;
	current_paragraph = NULL;
	current_statement = NULL;
}

/* Output routines */

static void
increase_output_line ()
{
	if (output_target == yyout) {
		output_line_number++;
		if (skip_line_num > 0)
			skip_line_num--;
		else
		if (last_line_num[0] > ' '
		 && cb_cob_line_num) 
			fprintf (output_target, "%s\n", last_line_num);
	}
}

/* output parts of a line to current target,
   should not contain any "\n" */
static void
output (const char *fmt, ...)
{
	if (output_target) {
		va_list		ap;
		int		ln = strlen(fmt);
		va_start (ap, fmt);
		vfprintf (output_target, fmt, ap);
		va_end (ap);
		if (fmt[ln-1] == '\n')
			increase_output_line ();
	}
}

/* output a new line to current target */
static void
output_newline (void)
{
	if (output_target) {
		fputc ('\n', output_target);
		increase_output_line ();
	}
}

/* output indentation prefix depending on current level
   to current target */
static void
output_prefix (void)
{
	int	i;

	if (output_target) {
		for (i = 0; i < output_indent_level; i++) {
			fputc (' ', output_target);
		}
	}
}

/* output a complete line with given data to current target,
   should not include additional "\n" */
static void
output_line (const char *fmt, ...)
{
	if (output_target) {
		va_list		ap;
		output_prefix ();
		va_start (ap, fmt);
		vfprintf (output_target, fmt, ap);
		va_end (ap);
		fputc ('\n', output_target);
		increase_output_line ();
	}
}

static const int indent_adjust_level = 2;

/* output a block opening to current target, adjusting the
   current indentation level */
static void
output_block_open (void)
{
	if (output_target) {
		output_prefix ();
		fputc ('{', output_target);
		fputc ('\n', output_target);
		skip_line_num++;
		increase_output_line ();
	}
	output_indent_level += indent_adjust_level;
}

/* output a block close to current target, adjusting the
   current indentation level */
static void
output_block_close (void)
{
	output_indent_level -= indent_adjust_level;
	if (output_target) {
		output_prefix ();
		fputc ('}', output_target);
		fputc ('\n', output_target);
		increase_output_line ();
	}
}

/* output string to current target, "*s" should not contain any "\n" */
static void
output_string (const unsigned char *s, const int size, const cob_u32_t llit)
{
	int	i;
	int	c;

	if (!s) {
		output ("NULL");
		return;
	}
	output ("\"");
	for (i = 0; i < size; i++) {
		c = s[i];
#ifndef	COB_EBCDIC_MACHINE
		if (c >= 0x7F) {
			output ("\\%03o", c);
		} else
#endif
		if (!isprint (c)) {
#if 1	/* octal */
			output ("\\%03o", c);
#else	/* hex (can be useful for a small amount of non-printable characters,
		   but gets really uggly if the string has a lot of those */
			output ("\" \"\\x%X\" \"", c);
#endif
		} else if (c == '\"') {
			output ("\\%c", c);
		} else if ((c == '\\' || c == '?') && !llit) {
			output ("\\%c", c);
		} else {
			output ("%c", c);
		}
	}
	output ("\"");
}

/* output data to current storage include file */
static void
output_storage (const char *fmt, ...)
{
	va_list		ap;

	if (cb_storage_file) {
		va_start (ap, fmt);
		vfprintf (cb_storage_file, fmt, ap);
		va_end (ap);
	}
}

/* output data to current local include file */
static void
output_local (const char *fmt, ...)
{
	va_list		ap;

	if (cb_local_file) {
		va_start (ap, fmt);
		vfprintf (cb_local_file, fmt, ap);
		va_end (ap);
	}
}

/* Field */

static struct cb_field *
real_field_founder (const struct cb_field *f)
{
	while (f->parent) {
		if (f->storage == CB_STORAGE_REPORT
		 && (f->report_flag & COB_REPORT_LINE)
		 && f->parent->children != f) {
			break;
		}
		f = f->parent;
	}
	if (f->redefines) {
		f = f->redefines;
	}
	return (struct cb_field *)f;
}

struct cb_field *
chk_field_variable_size (struct cb_field *f)
{
	if (!f->flag_vsize_done) {
		/* Note: will always return NULL for RENAMES items as those have no children,
		   which is fine because of the RENAMES syntax rule:
		   "None of the items within the range [...] shall be [...] a
		   variable-length data item, or an occurs-depending table. " */
		struct cb_field		*p;
		struct cb_field		*fc;
		f->vsize = NULL;
		for (fc = f->children; fc && !fc->redefines; fc = fc->sister) {
			if (fc->depending) {
				if (cb_odoslide || f->flag_picture_l) {
					f->vsize = fc;
					break;
				}
				if (cb_odo_last_varlen == CB_OK) {
					if (f->sister != NULL	/* Parent has sister so NOT vary size */
					 && f->level == f->sister->level
					 && f->level != 1
					 && f->level != 77
					 && !f->sister->redefines)
						break;
					if (fc->sister != NULL)
						continue;	 /* Group has sister so NOT vary size */
					f->vsize = fc;
					break;
				}
			} else if (fc->flag_picture_l) {
				continue;
			} else if ((p = chk_field_variable_size (fc)) != NULL) {
				f->vsize = p;
				break;
			}
		}
		f->flag_vsize_done = 1;
	}
	return f->vsize;
}

/* Check if previous field on current or higher level has variable size */
unsigned int
chk_field_variable_address (struct cb_field *fld)
{
	if (!cb_odoslide)
		return 0;
	if (!fld->flag_vaddr_done) {
		/* Note: this was called _very_ often and took 15-20% of parse + codegen time,
		   with about half the time in chk_field_variable_size; so try to not call
		   this function if not necessary (according to the testsuite: as long as
		   cb_odoslide is not set, but the caller's coverage is not that well...) */
		struct cb_field		*f = fld;
		struct cb_field		*p;
		for (p = f->parent; p; f = f->parent, p = f->parent) {
			for (p = p->children; p != f; p = p->sister) {
				if (p->depending	/* ODO leads to variable size */
				 || (!p->flag_picture_l && chk_field_variable_size (p)) /* skipping PIC L fields */
				   ) {
					fld->flag_vaddr_done = 1;
					fld->vaddr = 1;
					return 1;
				}
			}
		}
		fld->flag_vaddr_done = 1;
		fld->vaddr = 0;
	}
	return fld->vaddr;
}

/*
 * Output field offset from base, handle DEPENDING ON with 'odoslide'
 */
static int
out_odoslide_fld_offset (struct cb_field *p, struct cb_field *fld)
{

	if (p == fld) 	/* Single field */
		return 1;

	if (p->children && !p->flag_picture_l) {
		if (out_odoslide_grp_offset (p, fld))
			return 1;
	} else {
		if (need_plus_sign) {
			output ("+");
			need_plus_sign = 0;
		}
		if (p->depending) {
			if (p->size != 1) {
				output ("%d*", p->size);
			}
			output_integer (p->depending);
		} else if (p->occurs_max > 1) {
			output ("%d", p->size * p->occurs_max);
		} else {
			output ("%d", p->size);
		}
	}
	return 0;
}

static int
out_odoslide_grp_offset (struct cb_field *p, struct cb_field *fld)
{
	struct cb_field *f;
	int	found_it;
	int	add_size;

	if (p == fld
	 || odo_stop_now) {
		return 1;
	}
	if (p->children) {
		for (f = p->children; f; f = f->children) {
			if (f == fld) {
				need_plus_sign = 0;
				odo_stop_now = 1;
				return 1;
			}
		}
		if (need_plus_sign) {
			output ("+");
			need_plus_sign = 0;
		}
		found_it = 0;
		f = p->children;
		if (f->sister == NULL
	 	 && f->children == NULL
		 && f->depending == NULL) {
			found_it = out_odoslide_fld_offset (f, fld);
		} else {
			output ("(");
			add_size = 0;
			for (f = p->children; f; f = f->sister) {
				if (f == fld) {
					/* found_it = 1; */
					if (add_size > 0) {
						if (need_plus_sign) {
							output ("+");
							need_plus_sign = 0;
						}
						output ("%d", add_size);
						/* add_size = 0; */
					}
					output (")");
					return 1;
				}
				if (f != fld
				 && f->depending == NULL
				 && f->sister != NULL
				 && f->children == NULL) {
					if (f->occurs_max > 1) {
						add_size += (f->size * f->occurs_max);
					} else {
						add_size += f->size;
					}
					continue;
				}
				if (add_size > 0) {
					if (need_plus_sign) {
						output ("+");
						need_plus_sign = 0;
					}
					output ("%d",add_size);
					add_size = 0;
					need_plus_sign = 1;
				}
				found_it = out_odoslide_fld_offset (f, fld);
				if (found_it)
					break;
				need_plus_sign = 1;
			}
			need_plus_sign = 0;
			output (")");
			if (found_it)
				return 1;
		}
		if (odo_stop_now)
			return 1;
		if (p->depending) {
			output ("*");
			output_integer (p->depending);
		} else if (p->occurs_max > 1) {
			output ("*%d", p->occurs_max);
		}
		if (found_it)
			return 1;
	} else {
		if (out_odoslide_fld_offset (p, fld))
			return 1;
	}
	return 0;
}

static void
out_odoslide_offset (struct cb_field *f01, struct cb_field *fld)
{
	need_plus_sign = 1;
	odo_stop_now = 0;
	out_odoslide_fld_offset (f01, fld);
}

/*
 * Output field size, handle DEPENDING ON with 'odoslide'
 */
static void
out_odoslide_fld_size (struct cb_field *p, struct cb_field *fld)
{
	if (p->children) {
		out_odoslide_grp_size (p, fld);
	} else {
		if (p->depending) {
			if (p->size != 1) {
				output ("%d*", p->size);
			}
			output_integer (p->depending);
		} else if (p->occurs_max > 1) {
			output ("%d", p->size * p->occurs_max);
		} else {
			output ("%d", p->size);
		}
	}
}

static void
out_odoslide_grp_size (struct cb_field *p, struct cb_field *fld)
{
	struct cb_field	*f;
	int		add_size;

	if (p->children) {
		need_plus_sign = 0;
		f = p->children;
		if (f->sister == NULL
	 	 && f->children == NULL
		 && f->depending == NULL) {
			out_odoslide_fld_size (f, fld);
		} else {
			output ("(");
			add_size = 0;
			for (f = p->children; f; f = f->sister) {
				if (need_plus_sign) {
					output ("+");
					need_plus_sign = 0;
				}
				if (f != fld
				 && f->depending == NULL
				 && f->sister != NULL
				 && f->children == NULL) {
					if (f->occurs_max > 1) {
						add_size += (f->size * f->occurs_max);
					} else {
						add_size += f->size;
					}
					continue;
				}
				if (add_size > 0) {
					if (need_plus_sign) {
						output ("+");
					}
					output ("%d+", add_size);
					add_size = 0;
					need_plus_sign = 0;
				}
				out_odoslide_fld_size (f, fld);
				need_plus_sign = 1;
			}
			output (")");
		}
		need_plus_sign = 0;
		if (p == fld) {
			return;
		}
		if (p->depending) {
			output ("*");
			output_integer (p->depending);
		} else if (p->occurs_max > 1) {
			output ("*%d", p->occurs_max);
		}
	} else {
		out_odoslide_fld_size (p, fld);
	}
}

static void
out_odoslide_size (struct cb_field *fld)
{
	need_plus_sign = 0;
	out_odoslide_fld_size (fld, fld);
}

static void
add_field_cache (struct cb_field *f01, struct cb_field *f)
{
	struct base_list	*bl;
	if (!f01->flag_base) {
		if (f01->index_type == CB_INT_INDEX) {
			bl = cobc_parse_malloc (sizeof (struct base_list));
			bl->f = f01;
			bl->curr_prog = excp_current_program_id;
			bl->next = local_base_cache;
			local_base_cache = bl;
		} else if (!f01->flag_external && !f01->flag_local_storage) {
			if (!f01->flag_local || f01->flag_is_global) {
				bl = cobc_parse_malloc (sizeof (struct base_list));
				bl->f = f01;
				bl->curr_prog = excp_current_program_id;
				if (f01->flag_is_global || f->flag_is_global
				 || current_prog->flag_file_global) {
					bl->next = base_cache;
					base_cache = bl;
				} else {
					bl->next = local_base_cache;
					local_base_cache = bl;
				}
			} else {
				if (current_prog->flag_global_use) {
					output_local ("unsigned char\t\t*%s%d = NULL;",
							CB_PREFIX_BASE, f01->id);
					output_local ("\t/* %s */\n", f01->name);
					output_local ("static unsigned char\t*save_%s%d;\n",
							CB_PREFIX_BASE, f01->id);
				} else {
					output_local ("unsigned char\t*%s%d = NULL;",
							CB_PREFIX_BASE, f01->id);
					output_local ("\t/* %s */\n", f01->name);
				}
			}
		}
		f01->flag_base = 1;
	}
}

static void
output_base (struct cb_field *f, const cob_u32_t no_output)
{
	struct cb_field		*f01;

	/* LCOV_EXCL_START */
	if (f->flag_item_78) {
		cobc_err_msg (_("unexpected CONSTANT item"));
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */

	f01 = real_field_founder (f);

	/* Base storage */

	if (!f01->flag_base) {
		add_field_cache (f01, f);
	}

	if (no_output) {
		return;
	}

	if (f01->index_type != CB_NORMAL_INDEX) {
		if (f01->index_type == CB_STATIC_INT_VARYING) 
			output ("%s%d", CB_PREFIX_BASE, f01->id);
		else
			output ("(cob_u8_t *)&%s%d", CB_PREFIX_BASE, f01->id);
		return;
	} else if (f01->flag_local_storage) {
		if (f01->mem_offset) {
			output ("cob_local_ptr + %d", f01->mem_offset);
		} else {
			output ("cob_local_ptr");
		}
	} else if (f01->flag_data_set) {
		/* cob_field.data is set upon entry so use that here */
		output ("%s%d.data", CB_PREFIX_FIELD, f01->id);
	} else {
		output ("%s%d", CB_PREFIX_BASE, f01->id);
	}

	if (!gen_init_working
	 && chk_field_variable_address (f)) {
		if (f01->level == 0
		 && f01->sister
		 && strstr (f01->name, " Record")) {	/* Skip to First 01 within FD */
			f01 = f01->sister;
		}
		if (cb_odoslide) {
			out_odoslide_offset (f01, f);
		} else {
			struct cb_field		*v, *p;
			for (p = f->parent; p; f = f->parent, p = f->parent) {
				for (p = p->children; p != f; p = p->sister) {
					v = chk_field_variable_size (p);
					if (v) {
						output (" + %d + ", v->offset - p->offset);
						if (v->size != 1) {
							output ("%d * ", v->size);
						}
						output_integer (v->depending);
					} else {
						output (" + %d", p->size * p->occurs_max);
					}
				}
			}
		}
	} else if (f->offset > 0) {
		output (" + %d", f->offset);
	}
}

static int
is_index_1 (cb_tree x)
{
	switch (CB_TREE_TAG (x)) {
	case CB_TAG_INTEGER:
		if (CB_INTEGER (x)->val == 1) {
			return 1;
		}
		break;
	case CB_TAG_LITERAL:
		if (cb_get_int (x) == 1) {
			return 1;
		}
		break;
	default:
		return 0;
	}
	return 0;
}

static void
output_data (cb_tree x)
{
	switch (CB_TREE_TAG (x)) {
	case CB_TAG_LITERAL: {
		struct cb_literal	*l = CB_LITERAL (x);
		if (CB_TREE_CLASS (x) == CB_CLASS_NUMERIC) {
			output ("(cob_u8_ptr)\"%s%s\"",
					(l->sign < 0) ? "-" : (l->sign > 0) ? "+" : "",
					(char *)l->data);
		} else {
			output ("(cob_u8_ptr)");
			output_string (l->data, (int) l->size, l->llit);
		}
		break;
	}
	case CB_TAG_FIELD: {
		struct cb_field		*f = CB_FIELD (x);
		if (f->index_type != CB_STATIC_INT_INDEX)
			output("/* %s */",f->name);
		/* Base address */
		output_base (f, 0);
		break;
	}
	case CB_TAG_REFERENCE: {
		struct cb_reference	*r = CB_REFERENCE (x);
		struct cb_field		*f = CB_FIELD (r->value);
		int did_check = 0;

		if (r->check
		 && !gen_init_working
		 && in_cond
		 && inside_check == 0) {
			int n, sav_stack_id;
			cb_tree	l;

			inside_stack[inside_check++] = 0;
			did_check = 1;
			output_newline ();
			output_prefix ();
			output("(");		/* Parens starts list of debug functions */
			output("(");
			n = output_indent_level;
			output_indent_level = 0;
			for (l = r->check; l; l = CB_CHAIN (l)) {
				sav_stack_id = stack_id;
				output_stmt (CB_VALUE (l));
				stack_id = sav_stack_id;
				if (l == r->check) {
					output_indent_level = n;
				}
			}
			output ("), ");		/* End debug check and comma preceeds expression */
			output_newline ();
			output_prefix ();
		}

		/* Base address */
		output_base (f, 0);

		/* Subscripts */
		if (r->subs) {
			int		ncols, i, sub, pos;
			struct cb_field		*o_slide = NULL;
			struct cb_field		*o = f;
			cb_tree			l;
			cb_tree			lsub = r->subs;
			for (; f && lsub; f = f->parent) {
				/* add current field size for OCCURS */
				ncols = 0;
				if (f->report_column_list) {
					ncols = cb_list_length (f->report_column_list);
					if (ncols > 1) {
						if (CB_NUMERIC_LITERAL_P (CB_VALUE (lsub))
						 || CB_INTEGER(CB_VALUE (lsub))) {
							sub = cb_get_int (CB_VALUE (lsub));
						} else {
							cb_error_x (x, _("%s only numeric literal subscript supported"),f->name);
							ncols = sub = 0;
						}
					}
				}
				if (ncols > 1) {
					i = pos = 1;
					for (l = f->report_column_list; l; l = CB_CHAIN (l), i++) {
						if (sub > 0
						 && i == sub) {
							pos = cb_get_int (CB_VALUE (l));
							if (pos != 1)
								output (" + (%d - 1)",pos);
							break;
						}
					}
				} else
				if (f->flag_occurs) {
					/* 1 - 1 is 0 so skip it */
					if (is_index_1 (CB_VALUE (lsub)) ) {
						lsub = CB_CHAIN (lsub);
						continue;
					}

					if (cb_odoslide
					 && !gen_init_working
					 && f != o
					 && chk_field_variable_size(f)) {
						output (" + ");
						out_odoslide_size (f);
						output (" * ");
					} else {
						/* recalculate size for nested ODO ... */
						if (o_slide) {
							for (o = o_slide; o; o = o->children) {
								if (o->depending) {
									output (" + (%dLL * ", o->size);
									output_integer (o->depending);
									output (")");
								}
							}
							output (" * ");
						} else {
						/* ... use field size otherwise */
							output (" + ");
							if (f->step_count > f->size) {
								output ("%d * ", f->step_count);
							} else
							if (f->size != 1) {
								output ("%dLL * ", f->size);
							}
						}
						if (cb_odoslide
						 && !gen_init_working
						 && f->depending) {
							o_slide = f;
						}
					}

					output_index (CB_VALUE (lsub));
					lsub = CB_CHAIN (lsub);
				} else if (f->report_column_list) {
				}
			}
		}

		/* Offset */
		if (r->offset) {
			output (" + ");
			output_index (r->offset);
		}

		if (r->check
		 && did_check) {
			--inside_check;
			output (")");	/* End expression */
		}
		break;
	}
	case CB_TAG_CAST:
		output ("&");
		output_param (x, 0);
		break;
	case CB_TAG_INTRINSIC:
		output ("cob_procedure_params[%u]->data",
			field_iteration);
		break;
	case CB_TAG_CONST:
		if (x == cb_null) {
			output ("NULL");
		} else {
			output ("(");
			output_param (x, 0);
			output (")->data");
		}
		break;
	case CB_TAG_DIRECT:
		output ("%s", CB_DIRECT (x)->line);
		break;
	/* LCOV_EXCL_START */
	default:
		CB_TREE_TAG_UNEXPECTED_ABORT (x);
	/* LCOV_EXCL_STOP */
	}
}

static void
output_size (const cb_tree x)
{
	struct cb_literal	*l;
	struct cb_reference	*r;
	struct cb_field		*f;
	struct cb_field		*p = NULL;
	struct cb_field		*q;

	switch (CB_TREE_TAG (x)) {
	case CB_TAG_CONST:
		output ("1");
		break;
	case CB_TAG_LITERAL:
		l = CB_LITERAL (x);
		output ("%d", (int)(l->size + (l->sign != 0)));
		break;
	case CB_TAG_REFERENCE:
		r = CB_REFERENCE (x);
		f = CB_FIELD (r->value);
		if (f->flag_no_field) {
			output ("0");
			break;
		}
		if (r->length) {
			output_integer (r->length);
			break;
		}
		p = chk_field_variable_size (f);
		if (r->offset 
		 && p == NULL) {
			if (f->flag_any_length) {
				output ("%s%d.size - ", CB_PREFIX_FIELD, f->id);
			} else {
				output ("%d - ", f->size);
			}
			output_index (r->offset);
			break;
		}
		if (p != NULL
		 && (cb_odoslide
		  || f->flag_local
		  || f->flag_item_based
		  || f->storage == CB_STORAGE_LINKAGE)
		 && !gen_init_working) {
			out_odoslide_size (f);
		} else {
			q = f;
again:
			if ((!cb_odoslide || gen_init_working)
			 && p != NULL
			 && p->flag_odo_relative) {
				q = p;
				output ("%d", p->size * p->occurs_max);
			} else if (p 
					&& (!r->flag_receiving 
				   	 ||	!cb_field_subordinate (cb_code_field (p->depending), q))) {
				if ((p->offset - q->offset) > 0) {
					output ("%d + ", p->offset - q->offset);
				}
				if (p->size != 1) {
					output ("%d * ", p->size);
				}
				output_integer (p->depending);
				q = p;
			} else if (q->usage == CB_USAGE_COMP_X
					&& q->compx_size > 0) {
				output ("%d", q->compx_size);
			} else {
				output ("%d", q->size);
			}

			for (; q != f; q = q->parent) {
				if (q->sister 
				&& !q->sister->redefines) {
					q = q->sister;
					p = q->depending ? q : chk_field_variable_size (q);
					output (" + ");
					goto again;
				}
			}
		}
		if (r->offset) {		/* Size is reduced by initial offset (if any) */
			output (" - ");
			output_index (r->offset);
		}
		break;
	case CB_TAG_FIELD:
		output ("(int)%s%d.size", CB_PREFIX_FIELD, CB_FIELD (x)->id);
		break;
	/* LCOV_EXCL_START */
	default:
		CB_TREE_TAG_UNEXPECTED_ABORT (x);
	/* LCOV_EXCL_STOP */
	}
}


/* Generate goto label */

static void
perform_label (const char *to_prefix, int to_lbl, int call_num)
{
	int		sv_indent;
	if (!cb_flag_computed_goto) {
		struct label_list	*l;
		if (call_num > 0) {
			for (l = label_cache; l && l->call_num != call_num; l = l->next);
			/* LCOV_EXCL_START */
			if (l == NULL) {
				cobc_err_msg ("could not find label for %d", call_num);
				COBC_ABORT ();
			}
			/* LCOV_EXCL_STOP */
			output_line ("frame_ptr->return_address_num = %d; /* %s%d */",
				call_num, CB_PREFIX_LABEL, l->id);
			output_line ("goto %s%d;", to_prefix, to_lbl);
			return;
		}
		l = cobc_parse_malloc (sizeof (struct label_list));
		l->next = label_cache;
		l->id = cb_id;
		if (label_cache == NULL) {
			l->call_num = 0;
		} else {
			l->call_num = label_cache->call_num + 1;
		}
		label_cache = l;
		output_line ("frame_ptr->return_address_num = %d; /* %s%d */",
			l->call_num, CB_PREFIX_LABEL, cb_id);
	} else {
		if (call_num < 0) {
			call_num = cb_id;
		}
		output_line ("frame_ptr->return_address_ptr = &&%s%d;",
			CB_PREFIX_LABEL, call_num);
	}
	output_line ("goto %s%d;", to_prefix, to_lbl);
	sv_indent = output_indent_level;
	output_indent_level = 0;
	output_line ("%s%d:", CB_PREFIX_LABEL, cb_id);
	output_indent_level = sv_indent;
	cb_id++;
}

#if 0
/* This code is pending additional work on REPORT */
static int
add_new_label ()
{
	if (!cb_flag_computed_goto) {
		struct label_list *l;
		l = cobc_parse_malloc (sizeof (struct label_list));
		l->next = label_cache;
		l->id = cb_id;
		if (label_cache == NULL) {
			l->call_num = 0;
		} else {
			l->call_num = label_cache->call_num + 1;
		}
		label_cache = l;
		output_line ("%s%d:", CB_PREFIX_LABEL, cb_id++);
		return l->call_num;
	}
	output_line ("%s%d:", CB_PREFIX_LABEL, cb_id++);
	return cb_id - 1;
}

/*
 * Emit case for each control Declaratives
 * for GENERATE to execute
 */
static void
cb_emit_decl_case (struct cb_report *r, struct cb_field *f)
{
	struct cb_field		*p;

	for (p = f; p; p = p->sister) {
		if (p->report_decl_id) {
			output_line ("case %d:\t/* %s */",p->report_decl_id,p->name);
			output_indent_level += indent_adjust_level;
			output_line ("frame_ptr++;");
			output_line ("frame_ptr->perform_through = %d;", p->report_decl_id);
			output_indent_level -= indent_adjust_level;
			perform_label (CB_PREFIX_LABEL, p->report_decl_id, -1);
			output_indent_level += indent_adjust_level;
			output_line ("break;");
			output_indent_level -= indent_adjust_level;
		}
		if (p->children) {
			cb_emit_decl_case (r, p->children);
		}
	}
}
#endif

/* Picture strings */

static int
lookup_pic (const cob_pic_symbol *pic, const int length)
{
	struct pic_list *l;
	int		i;
	int		different_pic_str;

	/* Search picture string cache */
	for (l = pic_cache; l; l = l->next) {
		if (length != l->length) {
			continue;
		}

		different_pic_str = 0;
		for (i = 0; i < l->length; ++i) {
			if (pic[i].symbol != l->str[i].symbol
			    || pic[i].times_repeated != l->str[i].times_repeated) {
				different_pic_str = 1;
				break;
			}
		}

		if (different_pic_str) {
			continue;
		}

		return l->id;
	}

	/* Cache new picture string */

	l = cobc_parse_malloc (sizeof (struct pic_list));
	l->id = cb_pic_id;
	l->length = length;
	l->str = pic;
	l->next = pic_cache;
	pic_cache = l;

	return cb_pic_id++;
}

static void
output_pic_cache (void)
{
	struct pic_list	*pic;
	int		pos;

	if (!pic_cache) {
		return;
	}

	output_storage ("\n/* Picture strings */\n\n");
	pic_cache = pic_list_reverse (pic_cache);

	for (pic = pic_cache; pic; pic = pic->next) {
		output_storage ("static const cob_pic_symbol %s%d[] = {\n",
				CB_PREFIX_PIC, pic->id);

		for (pos = 0; pos < pic->length
			     && pic->str[pos].symbol != '\0'; ++pos) {
			output_storage ("\t{'%c', %u}",
					pic->str[pos].symbol,
					pic->str[pos].times_repeated);
			output_storage (",\n");
		}
		output_storage ("\t{'\\0', 1}");
		output_storage ("\n};\n");
	}
	output_storage ("\n");
}

/* Attributes */

static int
lookup_attr (const int type, const cob_u32_t digits, const int scale,
	     const cob_u32_t flags, cob_pic_symbol *pic, const int lenstr)
{
	const int		pic_id = pic ? lookup_pic (pic, lenstr) : -1;
	struct attr_list	*l;

	/* Search attribute cache */
	for (l = attr_cache; l; l = l->next) {
		if (type == l->type
		 && digits == l->digits
		 && scale == l->scale
		 && flags == l->flags
		 && pic_id == l->pic_id) {
			return l->id;
		}
	}

	/* Cache new attribute */

	l = cobc_parse_malloc (sizeof (struct attr_list));
	l->id = cb_attr_id;
	l->type = type;
	l->digits = digits;
	l->scale = scale;
	l->flags = flags;
	l->pic_id = pic_id;
	l->next = attr_cache;
	attr_cache = l;

	return cb_attr_id++;
}


static void
output_attr (const cb_tree x)
{
	int			id;
	cob_u32_t		flags;

	id = 0;
	switch (CB_TREE_TAG (x)) {
	case CB_TAG_LITERAL: {
		struct cb_literal	*l = CB_LITERAL (x);
		if (CB_TREE_CLASS (x) == CB_CLASS_NUMERIC) {
			flags = COB_FLAG_CONSTANT;
			if (l->sign != 0) {
				flags = COB_FLAG_HAVE_SIGN | COB_FLAG_SIGN_SEPARATE | COB_FLAG_SIGN_LEADING;
			}
			id = lookup_attr (COB_TYPE_NUMERIC_DISPLAY,
					  l->size, l->scale, flags, NULL, 0);
		} else {
			if (l->all) {
				id = lookup_attr (COB_TYPE_ALPHANUMERIC_ALL, 0, 0, COB_FLAG_CONSTANT, NULL, 0);
			} else {
				id = lookup_attr (COB_TYPE_ALPHANUMERIC, 0, 0, COB_FLAG_CONSTANT, NULL, 0);
			}
		}
		break;
	}
	case CB_TAG_REFERENCE: {
		struct cb_reference	*r = CB_REFERENCE (x);
		struct cb_field		*f = CB_FIELD (r->value);
		flags = 0;
		if (r->offset) {
			id = lookup_attr (COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL, 0);
		} else if (f->usage == CB_USAGE_CONTROL) {
			id = lookup_attr (COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL, 0);
		} else {
			int	type = cb_tree_type (x, f);
			switch (type) {
			case COB_TYPE_GROUP:
			case COB_TYPE_ALPHANUMERIC:
				if (f->flag_justified) {
					flags |= COB_FLAG_JUSTIFIED;
				}
				id = lookup_attr (type, 0, 0, flags, NULL, 0);
				break;
			default:
				if (f->pic->have_sign) {
					flags |= COB_FLAG_HAVE_SIGN;
					if (f->flag_sign_separate) {
						flags |= COB_FLAG_SIGN_SEPARATE;
					}
					if (f->flag_sign_leading) {
						flags |= COB_FLAG_SIGN_LEADING;
					}
				}
				if (f->flag_blank_zero) {
					flags |= COB_FLAG_BLANK_ZERO;
				}
				if (f->flag_justified) {
					flags |= COB_FLAG_JUSTIFIED;
				}
				if (f->flag_binary_swap) {
					flags |= COB_FLAG_BINARY_SWAP;
				}
				if (f->flag_real_binary
				 || f->usage == CB_USAGE_COMP_5) {
					flags |= COB_FLAG_REAL_BINARY;
				}
				if (f->flag_is_pointer) {
					flags |= COB_FLAG_IS_POINTER;
				}
				if (cb_binary_truncate 
				 && f->usage == CB_USAGE_BINARY 
				 && !f->flag_real_binary) {
					flags |= COB_FLAG_BINARY_TRUNC;
				}

				if (type == COB_TYPE_NUMERIC_BINARY
				 && f->usage == CB_USAGE_INDEX) {
					flags |= COB_FLAG_REAL_BINARY;
					type = COB_TYPE_NUMERIC_COMP5;
				} else
				if (type == COB_TYPE_NUMERIC_BINARY
				 && (f->flag_binary_swap || f->flag_real_binary)
				 && (f->flag_indexed_by || f->index_type || f->flag_internal_register)) {
					type = COB_TYPE_NUMERIC_COMP5;
				}
				switch (f->usage) {
				case CB_USAGE_COMP_6:
					flags |= COB_FLAG_NO_SIGN_NIBBLE;
					break;
				case CB_USAGE_FLOAT:
				case CB_USAGE_DOUBLE:
				case CB_USAGE_LONG_DOUBLE:
				case CB_USAGE_FP_BIN32:
				case CB_USAGE_FP_BIN64:
				case CB_USAGE_FP_BIN128:
				case CB_USAGE_FP_DEC64:
				case CB_USAGE_FP_DEC128:
					flags |= COB_FLAG_IS_FP;
					break;
				default:
					if (f->pic->category == CB_CATEGORY_FLOATING_EDITED) {
						flags |= COB_FLAG_IS_FP;
					}
					break;
				}

				id = lookup_attr (type, f->pic->digits,
						  f->pic->scale, flags,
						  f->pic->str, f->pic->lenstr);
				break;
			}
		}
		break;
	}
	case CB_TAG_ALPHABET_NAME:
		id = lookup_attr (COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL, 0);
		break;
	/* LCOV_EXCL_START */
	default:
		CB_TREE_TAG_UNEXPECTED_ABORT (x);
	/* LCOV_EXCL_STOP */
	}

	output ("&%s%d", CB_PREFIX_ATTR, id);
}

static void
output_attributes (void)
{
	struct attr_list	*attr;

	if (!(attr_cache || gen_figurative)) {
		return;
	}

	output_storage ("\n/* Attributes */\n\n");

	attr_cache = attr_list_reverse (attr_cache);
	for (attr = attr_cache; attr; attr = attr->next) {
		output_storage ("static const cob_field_attr %s%d =\t",
				CB_PREFIX_ATTR, attr->id);
		output_storage ("{0x%02x, %3u, %3d, 0x%04x, ",
				attr->type, attr->digits,
				attr->scale, attr->flags);
		if (attr->pic_id != -1) {
			output_storage ("%s%d", CB_PREFIX_PIC, attr->pic_id);
		} else {
			output_storage ("NULL");
		}
		output_storage ("};\n");
	}

	if (gen_figurative) {
		output_storage ("\nstatic const cob_field_attr cob_all_attr = ");
		output_storage ("{0x%02x, 0, 0, 0, NULL};\n",
				COB_TYPE_ALPHANUMERIC_ALL);
	}

	output_storage ("\n");
}

/* GLOBAL EXTERNAL pointers */

static void
output_globext_cache (void)
{
	struct base_list	*blp;

	if (!globext_cache) {
		return;
	}

	output_storage ("\n/* GLOBAL EXTERNAL pointers */\n");

	globext_cache = list_cache_sort (globext_cache, &base_cache_cmp);
	for (blp = globext_cache; blp; blp = blp->next) {
		output_storage ("static unsigned char\t\t*%s%d = NULL;",
				CB_PREFIX_BASE, blp->f->id);
		output_storage ("\t/* %s */\n", blp->f->name);
	}
}

/* Headers */

static void
output_standard_includes (struct cb_program *prog)
{
	struct cb_program *p;

#if defined (HAVE_GMP_H)
	#define MATH_INCLUDE "#include <gmp.h>"
#elif defined (HAVE_MPIR_H)
	#define MATH_INCLUDE "#include <mpir.h>"
#else
#error either HAVE_GMP_H or HAVE_MPIR_H needs to be defined
#endif

#if !defined (_GNU_SOURCE) && defined (_XOPEN_SOURCE_EXTENDED)
	output_line ("#ifndef\t_XOPEN_SOURCE_EXTENDED");
	output_line ("#define\t_XOPEN_SOURCE_EXTENDED 1");
	output_line ("#endif");
#endif
#if 0	/* Simon: why should we include that? */
	output_line ("#include <stdio.h>");
#endif
	output_line ("#include <string.h> /* for memcpy, memcmp and friends */");
#ifdef	WORDS_BIGENDIAN
	output_line ("#define  WORDS_BIGENDIAN 1");
#endif
#ifdef	COB_KEYWORD_INLINE
	output_line ("#define  COB_KEYWORD_INLINE %s",
		CB_XSTRINGIFY(COB_KEYWORD_INLINE));
#endif
	if (cb_flag_winmain) {
		output_line ("#include <windows.h>");
	}
	/* check if any of the processed programs has any decimal,
	   then include appropriate header */
	for (p = prog; p; p = p->next_program) {
		if (p->decimal_index_max || p->flag_decimal_comp) {
			output_line (MATH_INCLUDE);
			break;
		}
	}
	output_line ("#include <libcob.h>");
	output_newline ();
}

/* GnuCOBOL defines */

static void
output_gnucobol_defines (const char *formatted_date)
{
	int	i;

	if (!strrchr (cb_source_file, '\\')
	 && !strrchr (cb_source_file, '"')) {
		output_line ("#define  COB_SOURCE_FILE\t\t\"%s\"", cb_source_file);
	} else {
		char	cb_source_file_cleaned[FILENAME_MAX];
		int		pos = 0;
		const char *c;

		for (c = cb_source_file; *c; ++c) {
			if (*c == '\\' || *c == '"') {
				cb_source_file_cleaned[pos++] = '\\';
			}
			cb_source_file_cleaned[pos++] = *c;
		}
		cb_source_file_cleaned[pos] = 0;
		output_line ("#define  COB_SOURCE_FILE\t\t\"%s\"", cb_source_file_cleaned);
	}
	output_line ("#define  COB_PACKAGE_VERSION\t\t\"%s\"", PACKAGE_VERSION);
	output_line ("#define  COB_PATCH_LEVEL\t\t%d", PATCH_LEVEL);
	output_line ("#define  COB_MODULE_FORMATTED_DATE\t\"%s\"", formatted_date);

	i = ((current_compile_tm.tm_year + 1900) * 10000) +
		((current_compile_tm.tm_mon + 1) * 100) +
		current_compile_tm.tm_mday;
	output_line ("#define  COB_MODULE_DATE\t\t%d", i);
	i = (current_compile_tm.tm_hour * 10000) +
		(current_compile_tm.tm_min * 100) +
		current_compile_tm.tm_sec;
	output_line ("#define  COB_MODULE_TIME\t\t%d", i);

	{
		struct cb_text_list *l = cb_include_file_list ;
		for (; l; l = l->next){
			if (l->text[0] == '<') {
				output_line ("#include %s", l->text);
			} else {
				output_line ("#include \"%s\"", l->text);
			}
		}
	}

}

/* CALL cache */

static void
output_call_cache (void)
{
	struct call_list	*call;
	struct static_call_list	*static_call;

	if (needs_unifunc || call_cache || func_call_cache) {
		output_local ("\n/* Call pointers */\n");
	}
	if (needs_unifunc) {
		output_local ("cob_call_union\t\tcob_unifunc;\n");
	}
	if ((call_cache || func_call_cache)
	 && (cb_flag_memory_check & CB_MEMCHK_POINTER)) {
		optimize_defs[COB_CHK_MEMORYFENCE] = 1;
		/* note: we explicit do _not_ initialize it directly as that
		   will more likely lead to a non-consecutive memory layout,
		   which makes the whole purpose of the fence useless */
		output_local ("static char\tcall_fence_pre[8];\n");
	}
	call_cache = call_list_reverse (call_cache);
	for (call = call_cache; call; call = call->next) {
		output_local ("static cob_call_union\tcall_%s;\n",
			      call->call_name);
	}
	func_call_cache = call_list_reverse (func_call_cache);
	for (call = func_call_cache; call; call = call->next) {
		output_local ("static cob_call_union\tfunc_%s;\n",
			      call->call_name);
	}
	if ((call_cache || func_call_cache)
	 && (cb_flag_memory_check & CB_MEMCHK_POINTER)) {
		output_local ("static char\tcall_fence_post[8];\n");
	}
	if (static_call_cache) {
		const char			*convention_modifier;
		static_call_cache = static_call_list_reverse (static_call_cache);
		output_local ("/* Define external subroutines being called statically */\n");
		for (static_call = static_call_cache; static_call;
			 static_call = static_call->next) {
			if (static_call->convention & CB_CONV_STDCALL) {
				convention_modifier = "__stdcall ";
			} else {
				convention_modifier = "";
			}
			output_local ("#ifndef %s\n", static_call->call_name);
			if (static_call->return_type == COB_RETURN_NULL) {
				output_local ("extern void %s%s ();\n", convention_modifier,
					static_call->call_name);
			} else if (static_call->return_type == COB_RETURN_ADDRESS_OF) {
				output_local ("extern void * %s%s ();\n", convention_modifier,
					static_call->call_name);
			} else {
				output_local ("extern int %s%s ();\n", convention_modifier,
					static_call->call_name);
			}
			output_local ("#endif\n");
		}
	}
	needs_unifunc = 0;
}

/* Nested CALL table  */

static void
output_nested_call_table (struct cb_program *prog)
{
	struct nested_list	*nlp;

	if (!(prog->nested_prog_list && gen_nested_tab)) {
		return;
	}

	/* Generate contained program list */
	output_local ("\n/* Nested call table */\n");
	output_local ("static struct cob_call_struct\tcob_nest_tab[] = {\n");
	for (nlp = prog->nested_prog_list; nlp; nlp = nlp->next) {
		if (nlp->nested_prog == prog) {
			output_local ("\t{ \"%s\", { (void *(*)())%s_%d__ }, { NULL } },\n",
				      nlp->nested_prog->orig_program_id,
				      nlp->nested_prog->program_id,
				      nlp->nested_prog->toplev_count);
		} else {
			output_local ("\t{ \"%s\", { (void *(*)())%s_%d__ }, { (void *(*)())%s_%d_ } },\n",
				      nlp->nested_prog->orig_program_id,
				      nlp->nested_prog->program_id,
				      nlp->nested_prog->toplev_count,
				      nlp->nested_prog->program_id,
				      nlp->nested_prog->toplev_count);
		}
	}
	output_local ("\t{ NULL, { NULL }, { NULL } }\n");
	output_local ("};\n");
}

/* Local indexes */

static void
output_local_indexes (void)
{
	int	i;

	output_local ("\n/* Subscripts */\n");
	if (i_len_used)
		output_local ("\tint\t\ti_len = 0;\n");	/* used for ODO handling */
	for (i = 0; i < COB_MAX_SUBSCRIPTS; i++) {
		if (i_counters[i]) {
			output_local ("int\t\ti%d = 0, i%d_max = 0;\n", i, i);
		}
	}
}

/* PERFORM TIMES counters */
static void
output_perform_times_counters (void)
{
	int	i;

	if (loop_counter) {
		output_local ("\n/* Loop counters */\n");
		for (i = 0; i < loop_counter; i++) {
			output_local ("cob_s64_t\tn%d = 0;\n", i);
		}
		output_local ("\n");
	}
}

/* Local implicit fields */

static void
output_local_implicit_fields (void)
{
	int	i;

	if (num_cob_fields) {
		output_local ("\n/* Local cob_field items */\n");
		for (i = 0; i < num_cob_fields; i++) {
			output_local ("cob_field\t\tf%d;\n", i);
		}
		output_local ("\n");
	}
}

/* DEBUGGING fields */

static void
output_debugging_fields (struct cb_program *prog)
{
	COB_UNUSED (prog);
	if (need_save_exception) {
		output_local ("\n/* DEBUG exception code save */\n");
		output_local ("int\t\tsave_exception_code = 0;\n");
	}
}

static int	num_symtab = 0;
static int	sym_storage = 255;
static int	sym_comma = 0;
static int	sym_1st_file = 0;
static void
emit_comma ()
{
	if (sym_comma) {
		sym_comma = 0;
		output (",");
		output_newline ();
	}
}

static void
emit_one_sym (struct cb_field *f)
{
	struct cb_field *fp;
	int		is_indirect,idx;
	unsigned int offset;

	if (f->flag_is_returning)	/* Non static so cannot be in symbol table */
		return;
	if (!output_target)
		f->symtab = num_symtab++;
	emit_comma ();
	output ("/*%4d*/ {",f->symtab);
	if (f->flag_indexed_by
	|| (f->flag_external && f->level == 1)) {
		output ("   0,   0");
	} else {
		output ("%4d",f->parent?f->parent->symtab:0);
		output (",%4d",f->sister?f->sister->symtab:0);
	}
	if (f->flag_filler)
		output(",NULL");
	else
		output (",\"%s\"", f->name);
	fp = real_field_founder (f);
	is_indirect = SYM_ADRS_PTR;
	offset = f->offset;
	if (fp->flag_is_typedef) {
		output (",NULL");
		offset = 0;
	} else
	if (chk_field_variable_address (f)) {
		is_indirect = SYM_ADRS_VARY;
		output (",NULL");
		offset = 0;
	} else
	if (fp->flag_item_based) {
		output (",&%s%d", CB_PREFIX_BASE, fp->id);
	} else
	if (fp->storage == CB_STORAGE_LINKAGE) {
		if (f->flag_any_numeric
		 || f->flag_any_length) {
			output (",&%s%d", CB_PREFIX_FIELD, f->id);
			offset = 0;
			is_indirect = SYM_ADRS_FIELD;
		} else
		if (f->flag_cob_field) {
			output (",&%s%d.data", CB_PREFIX_FIELD, f->id);
			offset = 0;
		} else
		if (fp->flag_cob_field) {
			output (",&%s%d.data", CB_PREFIX_FIELD, fp->id);
		} else {
			output (",&%s%d", CB_PREFIX_BASE, fp->id);
		}
	} else 
	if (fp->storage == CB_STORAGE_LOCAL) {
		output (",&cob_local_save");
	} else 
	if (fp->flag_external) {
		output (",&%s%d", CB_PREFIX_BASE, fp->id);
	} else 
	if (f->flag_cob_field) {
		output (",&%s%d.data", CB_PREFIX_FIELD, f->id);
		offset = 0;
	} else 
	if (fp->flag_cob_field) {
		output (",&%s%d.data", CB_PREFIX_FIELD, fp->id);
	} else
	if (f->flag_indexed_by
	 || f->flag_local) {
		output (",&%s%d", CB_PREFIX_BASE, f->id);
		offset = 0;
		is_indirect = SYM_ADRS_DATA;
	} else
	if (f->children
	 && f->children->flag_cob_field
	 && f->children->offset == 0) {
		output (",&%s%d.data", CB_PREFIX_FIELD, f->children->id);
	} else {
		is_indirect = SYM_ADRS_DATA;
		output (",%s%d", CB_PREFIX_BASE, fp->id);
	}
	output (",");
	output_attr (cb_build_field_reference (f, NULL));
	output (",%d",fp->flag_is_typedef?1:0);
	output (",0");	/* NOT is_file */
	output (",%d",is_indirect);
	if (f->level < 8)
		output (",\t\t%02d",f->level);
	else
		output (",\t\t%2d",f->level);
	output (",%d",f->storage);
	output (",%d",f->children?1:0);
	if (sym_1st_file) {
		sym_1st_file = 0;
		output(",0");
	} else {
		output (",%d",f->redefines?1:0);
	}
	output (",%d",f->depending?1:0);
	for (idx=0, fp = f; fp; fp = fp->parent) {
		if (fp->occurs_max > 1)
			idx++;
	}
	output (",%d",idx);
	output (",00");			/* Unused bits */
	output (", %d",offset);
	output (",%d",f->size);
	if (f->depending) {
		fp = cb_code_field (f->depending);
		output (",%d", fp->symtab);
	} else {
		output(",0");
	}
	output (",%d",f->occurs_max>1?f->occurs_max:0);
	if (is_indirect == SYM_ADRS_VARY)
		output (",00");
	else
		output (", %d",f->offset);
	output ("}");
	sym_comma = 1;
}

static void
emit_field_indexes (struct cb_field *f)
{
	cb_tree l;
	struct cb_field *fp;

	for (l = f->index_list; l; l = CB_CHAIN (l)) {
		fp = CB_FIELD_PTR (CB_VALUE (l));
		if (fp->flag_sym_emitted)
			continue;
		fp->flag_sym_emitted = 1;
		emit_one_sym (fp);
	}
}

static void
emit_record_indexes (struct cb_field *f)
{
	while ( f != NULL ) {
		if (f->index_list != NULL)
			emit_field_indexes (f);
		if (f->children != NULL) {
			emit_record_indexes (f->children);
		}
		f = f->sister;
	}
	return;
}

static const char *sectname[] = {
			"CONSTANT","FILE","WORKING-STORAGE",
			"LOCAL","LINKAGE","SCREEN",
			"REPORT","COMMUNICATION"};
static void
emit_symtab (struct cb_field *f)
{
	if (!f->flag_sym_emitted
	 && !f->flag_internal_register
	 && f->level >= 1
	 && f->level != 66
	 && f->level != 78
	 && f->level != 88) {
		f->flag_sym_emitted = 1;
		if (f->storage != sym_storage) {
			emit_comma ();
			output_line ("/* %s */",sectname[f->storage]);
			sym_storage = f->storage;
		}
		if (f->level == 1 || f->level == 77) {
			emit_field_indexes (f);
			emit_record_indexes (f->children);
		}
		emit_one_sym (f);
	}
	if (f->children) {
		emit_symtab (f->children);
	}
	if (f->sister) {
		emit_symtab (f->sister);
	}
}

static void
emit_mod_symtab (struct cb_program *prog)
{
	struct cb_file	*fl;
	cb_tree			l;
	struct cb_field *f;
	char		wrk[64];

	num_symtab = 0;
	sym_comma = 0;
	sym_storage = 255;
	for (l = prog->file_list; l; l = CB_CHAIN (l)) {
		fl = CB_FILE(CB_VALUE (l));
		if (!fl->record) continue;
		sprintf(wrk,"%s %s",fl->organization != COB_ORG_SORT ? "FD" : "SD",fl->name);
		emit_comma ();
		if (sym_storage != CB_STORAGE_FILE) {
			sym_storage = CB_STORAGE_FILE;
			output_line ("/* FILE */");
		}
		output ("/*%4d*/ {",num_symtab++);
		output ("%4d",0);
		output (",%4d",fl->record->sister?fl->record->sister->symtab:0);
		output (",\"%s\"",wrk);
		output (",&%s%s",CB_PREFIX_FILE, fl->cname);
		output (",NULL,0,1,1,\t\t00,%d,0,0,0,0,0,0,0,0",CB_STORAGE_FILE);
		output ("}");
		sym_comma = 1;
		sym_1st_file = 1;
		for (f = fl->record->sister; f; f = f->sister) {
			emit_symtab (f);
		}
	}
	for (f = prog->working_storage; f; f = f->sister) {
		emit_symtab (f);
	}
	for (f = prog->screen_storage; f; f = f->sister) {
		emit_symtab (f);
	}
	for (f = prog->report_storage; f; f = f->sister) {
		emit_symtab (f);
	}
	for (f = prog->local_storage; f; f = f->sister) {
		emit_symtab (f);
	}
	for (f = prog->linkage_storage; f; f = f->sister) {
		emit_symtab (f);
	}
}

/*
 * Clear the symbol emited flag
 */
static void
clear_symtab (struct cb_field *f)
{
	struct cb_field *fp;
	cb_tree			l;
	f->flag_sym_emitted = 0;
	for (l = f->index_list; l; l = CB_CHAIN (l)) {
		fp = CB_FIELD_PTR (CB_VALUE (l));
		fp->flag_sym_emitted = 0;
	}
	if (f->children) {
		clear_symtab (f->children);
	}
	if (f->sister) {
		clear_symtab (f->sister);
	}
}

static void
clear_mod_symtab (struct cb_program *prog)
{
	struct cb_file	*fl;
	cb_tree			l;
	struct cb_field *f;

	num_symtab = 0;
	sym_comma = 0;
	sym_storage = 255;
	for (l = prog->file_list; l; l = CB_CHAIN (l)) {
		fl = CB_FILE(CB_VALUE (l));
		if (!fl->record) continue;
		for (f = fl->record->sister; f; f = f->sister) {
			clear_symtab (f);
		}
	}
	for (f = prog->working_storage; f; f = f->sister) {
		clear_symtab (f);
	}
	for (f = prog->local_storage; f; f = f->sister) {
		clear_symtab (f);
	}
	for (f = prog->linkage_storage; f; f = f->sister) {
		clear_symtab (f);
	}
	for (f = prog->screen_storage; f; f = f->sister) {
		clear_symtab (f);
	}
	for (f = prog->report_storage; f; f = f->sister) {
		clear_symtab (f);
	}
}
/* LOCAL-STORAGE pointer */

static void
output_local_storage_pointer (struct cb_program *prog)
{
	if (prog->local_storage && local_mem) {
		output_local ("\n/* LOCAL storage pointer */\n");
		output_local ("unsigned char\t\t*cob_local_ptr = NULL;\n");
		if (prog->flag_global_use
		 || cb_flag_symbols) {
			output_local ("static unsigned char\t*cob_local_save = NULL;\n");
		}
	}
}

/* CALL parameter stack */

static void
output_call_parameter_stack_pointers (struct cb_program *prog)
{
	output_local ("\n/* Call parameters */\n");
	if (cb_flag_stack_on_heap || prog->flag_recursive) {
		output_local ("cob_field\t\t**cob_procedure_params;\n");
	} else {
		output_local ("cob_field\t\t*cob_procedure_params[%u];\n",
			      prog->max_call_param ? prog->max_call_param : 1);
	}
}

/* Frame stack */

static void
output_frame_stack (struct cb_program *prog)
{
	const char *frame_type = (cb_flag_stack_extended) ? "cob_frame_ext" : "cob_frame";
	output_local ("\n/* Perform frame stack */\n");
	if (cb_perform_osvs && prog->prog_type == COB_MODULE_TYPE_PROGRAM) {
		output_local ("struct %s\t*temp_index;\n", frame_type);
	}
	if (cb_flag_stack_check) {
		output_local ("struct %s\t*frame_overflow;\n", frame_type);
	}
	output_local ("struct %s\t*frame_ptr;\n", frame_type);
	if (cb_flag_stack_on_heap || prog->flag_recursive) {
		output_local ("struct %s\t*frame_stack;\n\n", frame_type);
	} else {
		output_local ("struct %s\tframe_stack[%d];\n\n",
			      frame_type, cb_stack_size);
	}
}

/* Dynamic field FUNCTION-ID pointers */

static void
output_dynamic_field_function_id_pointers (void)
{
	cob_u32_t	i;

	if (gen_dynamic) {
		output_local ("\n/* Dynamic field FUNCTION-ID pointers */\n");
		for (i = 0; i < gen_dynamic; i++) {
			output_local ("cob_field\t*cob_dyn_%u = NULL;\n", i);
		}
	}
}


/* Based data */

static int ws_id = 0;
static size_t ws_used = 0;
/*
 * Compute memory size based on align-record and align-opt settings
 */
static size_t
compute_align_size (size_t fs, int dflt)
{
	int			align_on;
	align_on = cb_align_record;
	if (cb_align_opt) {
		if (fs >= 16) {
			align_on = 16;
		} else if (fs >= 8) {
			if (cb_align_record != 16)
				align_on = 8;
		} else if (fs >= 4) {
			if (cb_align_record != 8
			 && cb_align_record != 16)
				align_on = 4;
		} else if (fs >= 2) {
			if (cb_align_record != 4
			 && cb_align_record != 8
			 && cb_align_record != 16)
				align_on = 2;
		}
	}
	if (align_on == 0)
		align_on = dflt;
	fs = (fs + align_on - 1) / align_on;
	return fs * align_on;
}

/*
 * Emit storage collector variable
 */
static void
output_local_ws_group (void)
{
	if (ws_used > 0
	 && cb_align_record) {
#ifdef  HAVE_ATTRIBUTE_ALIGNED
		output_local ("static cob_u8_t	%s%d[%ld]%s;",
				  CB_PREFIX_WS_GROUP, ws_id, (long)ws_used, COB_ALIGN);
#else
#if defined(COB_ALIGN_PRAGMA_8)
		output_local ("#pragma align 8 (%s%d)\n", CB_PREFIX_WS_GROUP, ws_id);
#endif
		output_local ("static %scob_u8_t%s	%s%d[%ld];",
				  COB_ALIGN_DECL_8, COB_ALIGN_ATTR_8, CB_PREFIX_WS_GROUP, ws_id, (long)ws_used);
#endif
	}
}

/*
 * Emit all WORKING-STORAGE records
 */
static void
output_local_base_cache (void)
{
	struct base_list	*blp;
	size_t		fs;

	if (!local_base_cache) {
		return;
	}

	output_local ("\n/* WORKING-STORAGE Data */\n");

	local_base_cache = list_cache_sort (local_base_cache, &base_cache_cmp);
	ws_id++;
	ws_used = 0;
	for (blp = local_base_cache; blp; blp = blp->next) {
		const struct cb_field *fld = blp->f;
		if (fld->flag_used_in_call) {
			if (fld->index_type != CB_INT_INDEX) {
				output_local ("static ");
			}
#ifdef  HAVE_ATTRIBUTE_ALIGNED
			output_local ("cob_u8_t	%s%d_fence_pre[8]%s;\n",
				CB_PREFIX_BASE, fld->id, COB_ALIGN);
#else
			output_local ("%scob_u8_t%s	%s%d_fence_pre[8];\n",
				COB_ALIGN_DECL_8, COB_ALIGN_ATTR_8,
				CB_PREFIX_BASE, fld->id);
#endif
			optimize_defs[COB_CHK_MEMORYFENCE] = 1;
			/* note: we explicit do _not_ initialize it directly as that
			   will more likely lead to a non-consecutive memory layout,
			   which makes the whole purpose of the fence useless */
		}
		if (fld->index_type == CB_INT_INDEX) {
			output_local ("int		%s%d;",
				      CB_PREFIX_BASE, fld->id);
		} else if (fld->index_type == CB_STATIC_INT_INDEX) {
			output_local ("static int	%s%d;",
				      CB_PREFIX_BASE, fld->id);
		} else if( !(fld->report_flag & COB_REPORT_REF_EMITTED)) {
			if (!cb_align_record
			 || fld->memory_size >= COB_MAX_CHAR_SIZE) {
#ifdef  HAVE_ATTRIBUTE_ALIGNED
				output_local ("static cob_u8_t	%s%d[%d]%s;",
					      CB_PREFIX_BASE, fld->id,
					      fld->memory_size, COB_ALIGN);
#else
#if defined(COB_ALIGN_PRAGMA_8)
				output_local ("#pragma align 8 (%s%d)\n", CB_PREFIX_BASE, fld->id);
#endif
				output_local ("static %scob_u8_t%s	%s%d[%d];",
					      COB_ALIGN_DECL_8, COB_ALIGN_ATTR_8, CB_PREFIX_BASE,
					      fld->id, fld->memory_size);
#endif
			} else {
				fs = compute_align_size (fld->memory_size, 1);
				if (ws_used + fs > COB_MAX_CHAR_SIZE) {
					output_local_ws_group ();
					ws_id++;
					ws_used = 0;
				}

				output_local ("#define %s%d\t(%s%d + %ld)",
					      CB_PREFIX_BASE, fld->id, CB_PREFIX_WS_GROUP, ws_id, (long)ws_used);
				ws_used += fs;
			}
		}
		output_local ("\t/* %s */\n", get_field_name (fld));
		if (fld->flag_used_in_call) {
			if (fld->index_type != CB_INT_INDEX) {
				output_local ("static ");
			}
#ifdef  HAVE_ATTRIBUTE_ALIGNED
			output_local ("cob_u8_t	%s%d_fence_post[8]%s;\n",
				CB_PREFIX_BASE, fld->id, COB_ALIGN);
#else
			output_local ("%scob_u8_t%s	%s%d_fence_post[8];\n",
				COB_ALIGN_DECL_8, COB_ALIGN_ATTR_8,
				CB_PREFIX_BASE, fld->id);
#endif
		}
	}

	output_local_ws_group ();

	output_local ("\n/* End of WORKING-STORAGE data */\n\n");
}

static void
output_nonlocal_base_cache (void)
{
	struct base_list	*blp;
	const char		*prev_prog = NULL;

	if (!base_cache) {
		return;
	}

	output_storage ("\n/* Data storage */\n");
	base_cache = list_cache_sort (base_cache, &base_cache_cmp);

	for (blp = base_cache; blp; blp = blp->next) {
		const struct cb_field *fld = blp->f;
		if (blp->curr_prog != prev_prog) {
			prev_prog = blp->curr_prog;
			output_storage ("\n/* PROGRAM-ID : %s */\n",
					prev_prog);
		}

		if (fld->flag_used_in_call) {
#ifdef  HAVE_ATTRIBUTE_ALIGNED
			output_storage ("static cob_u8_t	%s%d_fence_pre[8]%s;\n",
				CB_PREFIX_BASE, fld->id, COB_ALIGN);
#else
#if defined(COB_ALIGN_PRAGMA_8)
			output_storage ("#pragma align 8 (%s%d_fence_pre)\n", CB_PREFIX_BASE, fld->id);
#endif
			output_storage ("static %scob_u8_t%s	%s%d_fence_pre[8];\n",
				COB_ALIGN_DECL_8, COB_ALIGN_ATTR_8,
				CB_PREFIX_BASE, fld->id);
#endif
			optimize_defs[COB_CHK_MEMORYFENCE] = 1;
			/* note: we explicit do _not_ initialize it directly as that
			   will more likely lead to a non-consecutive memory layout,
			   which makes the whole purpose of the fence useless */
		}
		if (fld->index_type != CB_NORMAL_INDEX) {
			output_storage ("static int	  %s%d;",
					CB_PREFIX_BASE, fld->id);
		} else {
#ifdef  HAVE_ATTRIBUTE_ALIGNED
			output_storage ("static cob_u8_t	%s%d[%d]%s;",
				      CB_PREFIX_BASE, fld->id,
				      fld->memory_size, COB_ALIGN);
#else
#if defined(COB_ALIGN_PRAGMA_8)
			output_storage ("#pragma align 8 (%s%d)\n", CB_PREFIX_BASE, fld->id);
#endif
			output_storage ("static %scob_u8_t%s	%s%d[%d];",
				      COB_ALIGN_DECL_8, COB_ALIGN_ATTR_8, CB_PREFIX_BASE,
				      fld->id, fld->memory_size);
#endif
		}
		output_storage ("\t/* %s */\n", fld->name);
		if (fld->flag_used_in_call) {
#ifdef  HAVE_ATTRIBUTE_ALIGNED
			output_storage ("static cob_u8_t	%s%d_fence_post[8]%s;\n",
				CB_PREFIX_BASE, fld->id, COB_ALIGN);
#else
#if defined(COB_ALIGN_PRAGMA_8)
			output_storage ("#pragma align 8 (%s%d_fence_post)\n", CB_PREFIX_BASE, fld->id);
#endif
			output_storage ("static %scob_u8_t%s	%s%d_fence_post[8];\n",
				COB_ALIGN_DECL_8, COB_ALIGN_ATTR_8,
				CB_PREFIX_BASE, fld->id);
#endif
		}
	}

	output_storage ("\n/* End of data storage */\n\n");
}

/* Fields */

static void
output_field (cb_tree x)
{
	output ("{");
	output_size (x);
	output (", ");
	output_data (x);
	output (", ");
	output_attr (x);
	output ("}");
}

static void
output_data_sub (cb_tree x, int subscript)
{
	switch (CB_TREE_TAG (x)) {
	case CB_TAG_LITERAL: {
		struct cb_literal	*l = CB_LITERAL (x);
		if (CB_TREE_CLASS (x) == CB_CLASS_NUMERIC) {
			output ("(cob_u8_ptr)\"%s%s\"", (char *)l->data,
				(l->sign < 0) ? "-" : (l->sign > 0) ? "+" : "");
		} else {
			output ("(cob_u8_ptr)");
			output_string (l->data, (int) l->size, l->llit);
		}
		break;
	}
	case CB_TAG_REFERENCE: {
		struct cb_reference	*r = CB_REFERENCE (x);
		struct cb_field		*f = CB_FIELD (r->value);

		/* Base address */
		if (f->flag_occurs
		 && subscript >= 1) {
			if (f->report_column_list
			 && r->subs == NULL) {
				cb_tree	value;
				int		offset = f->offset;
				value = cb_list_entry (f->report_column_list, subscript);
				if (value) {
					f->offset = cb_get_int (value) - 1;
				}
				output_base (f, 0);
				f->offset = offset;
			} else {
				output_base (f, 0);
				if (subscript > 1)
					output (" + %d", (subscript - 1) * f->size);
			}
		}

		/* Subscripts */
		if (r->subs) {
			cb_tree			lsub = r->subs;
			for (; f && lsub; f = f->parent) {
				if (f->flag_occurs) {
					output (" + ");
					if (f->step_count > f->size) {
						output ("%d * ", f->step_count);
					} else if (f->size != 1) {
						output ("%d * ", f->size);
					}
					output_index (CB_VALUE (lsub));
					lsub = CB_CHAIN (lsub);
				}
			}
		}

		/* Offset */
		if (r->offset) {
			output (" + ");
			output_index (r->offset);
		}
		break;
	}
	case CB_TAG_FIELD: {
		struct cb_field		*f = CB_FIELD (x);
			output("/* %s */", f->name);
			/* Base address */
			output_base (f, 0);
		}
		break;
	case CB_TAG_CAST:
		output ("&");
		output_param (x, 0);
		break;
	case CB_TAG_INTRINSIC:
		output ("cob_procedure_params[%u]->data",
			field_iteration);
		break;
	case CB_TAG_CONST:
		if (x == cb_null) {
			output ("NULL");
			return;
		}
		/* Fall through */
	/* LCOV_EXCL_START */
	default:
		CB_TREE_TAG_UNEXPECTED_ABORT (x);
	/* LCOV_EXCL_STOP */
	}
}

static void
output_field_sub (struct cb_field *f, cb_tree x, int subscript)
{
	output ("{");
	output_size (x);
	output (", ");
	if (f->index_type == CB_STATIC_INT_VARYING)
		output ("&");
	if (subscript > 1)
		output_data_sub (x,subscript);
	else
		output_data (x);
	output (", ");
	output_attr (x);
	output ("}");
}

static void
output_emit_one_field (struct cb_field *f, const char *cmt, int sub)
{
	if (f->flag_cob_field)
		return;
	if (f->storage == CB_STORAGE_REPORT
	 && (chk_field_variable_size (f)
	  || chk_field_variable_address (f))) {
		return;
	}
	if (sub == 0
	 && f->flag_occurs 
	 && f->occurs_max > 1) {
		int	i;
		for (i = 1; i <= f->occurs_max; i++) {
			f->flag_cob_field = 0;
			output_emit_one_field (f, cmt, i);
			f->flag_cob_field = 1;
		}
		return;
	}
	if (f->children) {
		output_emit_one_field (f->children, cmt, sub);
	}
	f->report_flag |= COB_REPORT_REF_EMITTED;
	f->flag_cob_field = 1;
	if (f->step_count < f->size) {
		f->step_count = f->size;
	}
	if (sub > 1)
		output ("static cob_field %s%d_%d\t= ", CB_PREFIX_FIELD, f->id, sub);
	else
		output ("static cob_field %s%d\t= ", CB_PREFIX_FIELD, f->id);
	output_field_sub (f, cb_build_field_reference (f, NULL), sub);
	output_local (";\t/* ");
	output_local ("%s ", get_field_name (f));
	if ((f->report_flag & COB_REPORT_COLUMN_RIGHT)) {
		output_local (", RIGHT %d",f->report_column);
	} else
	if ((f->report_flag & COB_REPORT_COLUMN_LEFT)) {
		output_local (", LEFT %d",f->report_column);
	} else
	if ((f->report_flag & COB_REPORT_COLUMN_CENTER)) {
		output_local (", CENTER %d",f->report_column);
	} else
	if (f->report_column > 0) {
		if (sub > 0 
		 && f->occurs_max > 1
		 && f->report_column_list) {
			cb_tree value = cb_list_entry (f->report_column_list, sub);
			if (value) {
				output_local (", Col%4d", cb_get_int (value));
			}
		} else {
			output_local (", col%4d", f->report_column);
		}
	}
	if ((f->report_flag & COB_REPORT_COLUMN_RIGHT)) {
		output_local (", GROUP INDICATE");
	}
	if (cmt) {
		output_local (" : %s ", cmt);
	}
	output_local ("*/\n");
}

/*
 * Emit cob_field (currently only for reportwriter) with comments
 */
static void
output_emit_field (cb_tree x, const char *cmt)
{
	struct cb_field *f = cb_code_field (x);

	if (!f)
		return;

	if (!(f->report_flag & COB_REPORT_REF_EMITTED)) {
		output_emit_one_field (f, cmt, 0);
		if (f->sister
		 && f->sister->count > 0
		 && f->sister->storage == f->storage) {
			output_emit_one_field (f->sister, cmt, 0);
		}
	}
}

static void
output_local_field_cache (struct cb_program *prog)
{
	struct field_list	*field;
	struct cb_field		*f;

	if (!local_field_cache) {
		return;
	}

	local_field_cache = list_cache_sort (local_field_cache,
					     &field_cache_cmp);
	for (field = local_field_cache; field; field = field->next) {
		int		needs_comment = 1;
		f = field->f;
		if (!f->flag_local
		 && !f->flag_external) {
			if (prog->flag_recursive
			 && !f->flag_filler) {
				output ("/* %s is not local */", f->name);
				output_newline ();
			}
			if (f->storage == CB_STORAGE_REPORT
			 && f->flag_occurs
			 && f->occurs_max > 1) {
				/* generate sub-fields and a comment each */
				output_emit_field (cb_build_field_reference (f, NULL), NULL);
				needs_comment = 0;
			} else {
				f->flag_cob_field = 1;
				output ("static cob_field %s%d\t= ", CB_PREFIX_FIELD, f->id);
				output_field (field->x);
			}
		} else {
			if (!prog->flag_recursive)
				f->flag_cob_field = 1;
			output ("%scob_field %s%d\t= ", prog->flag_recursive ? "\t" : "static ",
				CB_PREFIX_FIELD, f->id);
			output ("{");
			output_size (field->x);
			output (", NULL, ");
			output_attr (field->x);
			output ("}");
		}

		if (needs_comment) {
			output (";");
			if (!f->flag_filler) {
				output ("\t/* %s */", field->f->name);
			}
		}
		output_newline ();
		field->f->report_flag |= COB_REPORT_REF_EMITTED;
	}
}

static void
output_local_fields (struct cb_program *prog)
{
	cb_tree			l;
	struct cb_field		*f;

	/* Switch to local storage file */
	output_target = prog->local_include->local_fp;
	if (prog->flag_recursive) {
		output_local ("\n/* Fields for recursive routine */\n");
	} else {
		output_local ("\n/* Local fields */\n");
	}

	output_local_field_cache (prog);

	/* Output variable size/location parameters */
	for (l = prog->parameter_list; l; l = CB_CHAIN (l)) {
		f = cb_code_field (CB_VALUE (l));
		if (!f->flag_field
		 && (chk_field_variable_size (f)
		  || chk_field_variable_address (f))) {
			f->flag_cob_field = 1;
			output ("static cob_field %s%d\t= ", CB_PREFIX_FIELD, f->id);
			output ("{");
			output ("0, NULL, ");
			output_attr (cb_build_field_reference (f, NULL));
			output ("}; /* %s */",f->name);
			output_newline ();
		}

	}
	
	/* Output report writer special fields */
	if (prog->report_storage) {
		struct cb_report	*rep;
		cb_tree			l;
		for (l = prog->report_list; l; l = CB_CHAIN (l)) {
			rep = CB_REPORT(CB_VALUE(l));
			for (f = rep->records; f; f = f->sister) {
				if (f->storage == CB_STORAGE_WORKING
				&& !(f->report_flag & COB_REPORT_REF_EMITTED)) {
					output_emit_field (cb_build_field_reference (f, NULL), NULL);
				}
			}
		}
		for (l = prog->report_list; l; l = CB_CHAIN (l)) {
			rep = CB_REPORT(CB_VALUE(l));
			if(rep) {
				output_report_summed_field (rep->records);
			}
		}
	}

	output_local ("\n/* End of fields */\n\n");

	if (cb_flag_symbols
	 && prog->prog_type == 0
	 && !prog->flag_recursive) {	
		FILE			*svout = output_target;
		int		max;
		output_target = NULL;
		emit_mod_symtab (prog);
		max = num_symtab;
		output_target = svout;
		clear_mod_symtab (prog);
		output_line ("/* Symbol table for %s */",prog->orig_program_id);
		output_line ("static cob_symbol %s_sym_tab [] = {",prog->program_id);
		emit_mod_symtab (prog);
		emit_comma ();
		output_line ("         {%4d,%4d,NULL,NULL,NULL,0,0,0,0}",0,0);
		output_line ("};");
		output_line ("static unsigned int %s_num_sym = %d;",prog->program_id,max);
		output_newline ();
	}
	/* Switch to main storage file */
	output_target = cb_storage_file;
}

static void
output_nonlocal_field_cache (void)
{
	struct field_list	*field;
	const char		*prev_prog = NULL;

	if (!field_cache) {
		return;
	}

	output_storage ("\n/* Fields */\n");

	field_cache = list_cache_sort (field_cache, &field_cache_cmp);
	for (field = field_cache; field; field = field->next) {
		if (field->curr_prog != prev_prog) {
			prev_prog = field->curr_prog;
			output_storage ("\n/* PROGRAM-ID : %s */\n",
					prev_prog);
		}

		field->f->flag_cob_field = 1;
		output ("static cob_field %s%d\t= ", CB_PREFIX_FIELD, field->f->id);
		if (!field->f->flag_local) {
			output_field (field->x);
		} else {
			output ("{");
			output_size (field->x);
			output (", NULL, ");
			output_attr (field->x);
			output ("}");
		}
		if (field->f->flag_filler) {
			output (";\t/* Implicit FILLER */");
		} else {
			output (";\t/* %s */", field->f->name);
		}
		output_newline ();
	}

	output_storage ("\n/* End of fields */\n\n");
}

/* Literals, figurative constants and user-defined constants */

static void
output_low_value (void)
{
	if (gen_figurative & CB_NEED_LOW) {
		output ("static cob_field cob_all_low\t= ");
		output ("{1, ");
		output ("(cob_u8_ptr)\"\\0\", ");
		output ("&cob_all_attr};");
		output_newline ();
	}
}

static void
output_high_value (void)
{
	if (gen_figurative & CB_NEED_HIGH) {
		output ("static cob_field cob_all_high\t= ");
		output ("{1, ");
		output ("(cob_u8_ptr)\"\\xff\", ");
		output ("&cob_all_attr};");
		output_newline ();
	}
}

static void
output_quote (void)
{
	if (gen_figurative & CB_NEED_QUOTE) {
		output ("static cob_field cob_all_quote\t= ");
		output ("{1, ");
		if (cb_flag_apostrophe) {
			output ("(cob_u8_ptr)\"'\", ");
		} else {
			output ("(cob_u8_ptr)\"\\\"\", ");
		}
		output ("&cob_all_attr};");
		output_newline ();
	}
}

static void
output_space (void)
{
	if (gen_figurative & CB_NEED_SPACE) {
		output ("static cob_field cob_all_space\t= ");
		output ("{1, ");
		output ("(cob_u8_ptr)\" \", ");
		output ("&cob_all_attr};");
		output_newline ();
	}
}

static void
output_zero (void)
{
	if (gen_figurative & CB_NEED_ZERO) {
		output ("static cob_field cob_all_zero\t= ");
		output ("{1, ");
		output ("(cob_u8_ptr)\"0\", ");
		output ("&cob_all_attr};");
		output_newline ();
	}
}

static void
output_literals_figuratives_and_constants (void)
{
	struct literal_list	*lit;

	if (!(literal_cache || gen_figurative)) {
		return;
	}

	output_storage ("\n/* Constants */\n");

	literal_cache = literal_list_reverse (literal_cache);
	for (lit = literal_cache; lit; lit = lit->next) {
		output ("static const cob_field %s%d\t= ",
			CB_PREFIX_CONST, lit->id);
		output_field (CB_TREE(lit->literal));
		output (";");
		output_newline ();
	}

	if (gen_figurative) {
		output_newline ();
		output_low_value ();
		output_high_value ();
		output_quote ();
		output_space ();
		output_zero ();
	}

	output_newline ();
}

/* Collating tables */

/* Outputs conversion from given table, or a native conversion (identity) when
   omitted (if table == NULL). */
static void
output_colseq_table (const char * const table_name,
		     const cob_u8_t table[256])
{
	int i;
	output_storage ("static const unsigned char\t%s[256] = {", table_name);
	for (i = 0; i < 256; i++) {
		output_storage ("%s%#04x,",
				i % 8 == 0 ? "\n\t" : " ",
				table ? table[i] : i);
	}
	output_storage ("\n};\n");
}

static void
output_colseq_table_field (const char * field_name, const char * table_name)
{
	const int i = lookup_attr (COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL, 0);
	output_storage ("static cob_field %s = { 256, (cob_u8_ptr)%s, &%s%d };\n",
			field_name, table_name, CB_PREFIX_ATTR, i);
}

static void
output_collating_tables (void)
{
	cob_u8_t ebcdic_to_ascii[256];
	cob_u8_t ascii_to_ebcdic[256];

	/* Load the collating tables if needed */
	if (gen_ascii_ebcdic || gen_ebcdic_ascii) {
		if (cob_load_collation (cb_ebcdic_table,
					gen_ebcdic_ascii ? ebcdic_to_ascii : NULL,
					gen_ascii_ebcdic ? ascii_to_ebcdic : NULL) < 0) {
			cobc_err_exit (_("invalid parameter: %s"), "-febcdic-table");
		}
	}

	if (gen_native) {
		output_storage ("\n/* NATIVE table */\n");
		output_colseq_table ("cob_native", NULL);
		if (gen_native > 1) {
			output_colseq_table_field ("f_native", "cob_native");
		}
		output_storage ("\n");
	}

	if (gen_ascii_ebcdic) {
		output_storage ("\n/* ASCII to EBCDIC table */\n");
		output_colseq_table ("cob_ascii_ebcdic", ascii_to_ebcdic);
		if (gen_ascii_ebcdic > 1) {
			output_colseq_table_field ("f_ascii_ebcdic", "cob_ascii_ebcdic");
		}
		output_storage ("\n");
	}

	if (gen_ebcdic_ascii) {
		output_storage ("\n/* EBCDIC to ASCII table */\n");
		output_colseq_table ("cob_ebcdic_ascii", ebcdic_to_ascii);
		if (gen_ebcdic_ascii > 1) {
			output_colseq_table_field ("f_ebcdic_ascii", "cob_ebcdic_ascii");
		}
		output_storage ("\n");
	}
}

/* Strings */

static void
output_storage_clean (const char *text)
{
	char	text_cleaned[FILENAME_MAX];
	int	pos = 0;
	const char *c;

	for (c = text; *c; ++c) {
		if (*c == '\\' || *c == '"') {
			text_cleaned[pos++] = '\\';
		}
		text_cleaned[pos++] = *c;
	}
	text_cleaned[pos] = 0;

	output_storage ("%s", text_cleaned);
}

static void
output_string_cache (void)
{
	struct string_list	*stp;

	if (!string_cache) {
		return;
	}

	output_storage ("\n/* Strings */\n");

	string_cache = string_list_reverse (string_cache);
	for (stp = string_cache; stp; stp = stp->next) {
		output_storage ("static const char %s%d[]\t= \"",
				CB_PREFIX_STRING, stp->id);
		if (!strrchr (stp->text, '\\')
		 && !strrchr (stp->text, '"')) {
			output_storage ("%s", stp->text);
		} else {
			output_storage_clean (stp->text);
		}
		output_storage ("\";\n");
	}

	output_storage ("\n");
}

/* Source file names */

static void
output_source_cache (void)
{
	struct string_list	*stp;

	if (!source_cache) {
		return;
	}

	output_storage ("\n/* Source file names */\n");
	source_cache = string_list_reverse (source_cache);
	output_storage ("static const char *%ssource_files[]\t= { \"\" ", CB_PREFIX_STRING);
	if (source_cache) {
		for (stp = source_cache; stp; stp = stp->next) {
			output_storage ("\n\t\t,\"");
			
			if (!strrchr (stp->text, '\\')
			 && !strrchr (stp->text, '"')) {
				output_storage ("%s", stp->text);
			} else {
				output_storage_clean (stp->text);
			}
			
			output_storage ("\"");
		}
	}
	output_storage ("};\n");
}

/* Literal */

/* Add the given literal to the list of "seen" decimal
   constants in the given program "prog" */
static void
cb_cache_program_decimal_constant (struct cb_program *prog, struct literal_list *cached_literal)
{
	struct literal_list	*l;
	for (l = prog->decimal_constants; l; l = l->next) {
		if (cached_literal->id == l->id) {
			return;
		}
	}

	l = cobc_parse_malloc (sizeof (struct literal_list));
	l->id = cached_literal->id;
	l->literal = cached_literal->literal;
	l->make_decimal = cached_literal->make_decimal;
	l->next = prog->decimal_constants;
	prog->decimal_constants = l;
}

/* Resolve literal "x" from the literal cache and return its id.
   The literal is added to the literal cache if missing.
   Additionally, if the literal is a decimal constant, it is
   added to the list of "seen" decimal constant of program "prog". */
int
cb_lookup_literal (struct cb_program *prog, cb_tree x, int make_decimal)
{
	struct cb_literal	*literal;
	struct literal_list	*l;
	FILE			*savetarget;

	literal = CB_LITERAL (x);
	/* Search literal cache */
	for (l = literal_cache; l; l = l->next) {
		if (CB_TREE_CLASS (literal) == CB_TREE_CLASS (l->literal)
		 && literal->size == l->literal->size
		 && literal->all == l->literal->all
		 && literal->sign == l->literal->sign
		 && literal->scale == l->literal->scale
		 && memcmp (literal->data, l->literal->data,
			    (size_t)literal->size) == 0) {
			if (make_decimal) {
				l->make_decimal = 1;
				cb_cache_program_decimal_constant (prog, l);
			}
			return l->id;
		}
	}

	/* Output new literal */
	savetarget = output_target;
	output_target = NULL;
	output_field (x);

	output_target = savetarget;

	/* Cache it */
	l = cobc_parse_malloc (sizeof (struct literal_list));
	l->id = cb_literal_id;
	l->literal = literal;
	l->make_decimal = make_decimal;
	l->next = literal_cache;
	literal_cache = l;
	if (make_decimal) {
		cb_cache_program_decimal_constant (prog, l);
	}

	return cb_literal_id++;
}

/*
 * Should numeric literal for truncated into a PIC S9(9) BINARY field, ignoring scale?
 *  (This is the way that Micro Focus COBOL works; RJN Nov 2016)
 */
static int
cb_fit_to_int (const cb_tree x)
{
	struct cb_literal	*l;
	int			scale, sts;

#ifndef WORDS_BIGENDIAN
	if (cb_binary_byteorder == CB_BYTEORDER_BIG_ENDIAN) {
		gen_num_lit_big_end = 1;
	} else {
		gen_num_lit_big_end = 0;
	}
#else
	gen_num_lit_big_end = 1;
#endif

	if (CB_NUMERIC_LITERAL_P (x)) {
		if (gen_num_lit_big_end)
			return 1;
		l = CB_LITERAL (x);
		if (l->scale > 0) {
			scale = l->scale;
			l->scale = 0;
			sts = cb_fits_int ( x );
			l->scale = scale;
			return sts;
		}
	}

	return cb_fits_int ( x );
}

/* Integer */

static void
output_integer (cb_tree x)
{
	switch (CB_TREE_TAG (x)) {
	case CB_TAG_CONST:
		if (x == cb_zero) {
			output ("0");
		} else if (x == cb_null) {
			output ("(cob_u8_ptr)NULL");
		} else {
			output ("%s", CB_CONST (x)->val);
		}
		break;
	case CB_TAG_INTEGER:
#ifdef USE_INT_HEX /* Simon: using this increases the struct and we
		 *should* pass the flags as constants in any case... */
		if (CB_INTEGER (x)->hexval) {
			output ("0x%X", CB_INTEGER (x)->val);
		} else {
			output ("%d", CB_INTEGER (x)->val);
		}
#else
		output ("%d", CB_INTEGER (x)->val);
#endif
		break;
	case CB_TAG_LITERAL:
		output ("%d", cb_get_int (x));
		break;
	case CB_TAG_BINARY_OP: {
		const struct cb_binary_op *p = CB_BINARY_OP (x);
		if (p->flag == BOP_RESOLVE_AS_INTEGER) {
			if (!cb_fits_int (p->x) || !cb_fits_int (p->y)) {
				output ("cob_get_int (");
				output_param (x, -1);
				output (")");
				break;
			}
		}
		if (p->op == '^') {
			/* TODO: Optimize: base of constant 1/-1 -> (int)0 */
			output ("cob_s32_pow (");
			output_integer (p->x);
			output (", ");
			output_integer (p->y);
			output (")");
		} else if (p->op == 'n') {
			output (" ~ (");
			output_integer (p->y);
			output (")");
		} else {
			output ("(");
#ifdef	COB_NON_ALIGNED
			if (CB_TREE_TAG (p->x) == CB_TAG_REFERENCE
			 && p->x != cb_null) {
				const struct cb_field *f = cb_code_field (p->x);
				/* typecast is required on Sun because pointer
				 * arithmetic is not allowed on (void *)
				 */
				if (f->usage == CB_USAGE_POINTER
				 || f->usage == CB_USAGE_PROGRAM_POINTER) {
					output ("(cob_u8_ptr)");
				}
			}
#endif
			output_integer (p->x);
			if (p->op == 'a')
				output (" & ");
			else if (p->op == 'o')
				output (" | ");
			else if (p->op == 'e')
				output (" ^ ");
			else if (p->op == 'l')
				output (" << ");
			else if (p->op == 'r')
				output (" >> ");
			else
				output (" %c ", p->op);
#ifdef	COB_NON_ALIGNED
			if (CB_TREE_TAG (p->y) == CB_TAG_REFERENCE
			 && p->y != cb_null) {
				const struct cb_field *f = cb_code_field (p->y);
				/* typecast is required on Sun because pointer
				 * arithmetic is not allowed on (void *)
				 */
				if (f->usage == CB_USAGE_POINTER
				 || f->usage == CB_USAGE_PROGRAM_POINTER) {
					output ("(cob_u8_ptr)");
				}
			}
#endif
			output_integer (p->y);
			output (")");
		}
		break;
	}
	case CB_TAG_CAST: {
		const struct cb_cast *cp = CB_CAST (x);
		switch (cp->cast_type) {
		case CB_CAST_ADDRESS:
			output ("(");
			output_data (cp->val);
			output (")");
			break;
		case CB_CAST_PROGRAM_POINTER:
			output ("cob_call_field (");
			output_param (x, -1);
			if (current_prog->nested_prog_list) {
				gen_nested_tab = 1;
				output (", cob_nest_tab, 0, %d)", cb_fold_call);
			} else {
				output (", NULL, 0, %d)", cb_fold_call);
			}
			break;
		/* LCOV_EXCL_START */
		default:
			cobc_err_msg (_("unexpected cast type: %d"),
				 (int)cp->cast_type);
			COBC_ABORT ();
		/* LCOV_EXCL_STOP */
		}
		break;
	}
	case CB_TAG_REFERENCE: {
		struct cb_field *f = cb_code_field (x);
		switch (f->usage) {
		case CB_USAGE_INDEX:
			if (f->index_type != CB_NORMAL_INDEX) {
				output_base (f, 1U);
				output ("%s%d", CB_PREFIX_BASE, f->id);
				return;
			}
			/* Fall through */
		case CB_USAGE_HNDL:
		case CB_USAGE_HNDL_WINDOW:
		case CB_USAGE_HNDL_SUBWINDOW:
		case CB_USAGE_HNDL_FONT:
		case CB_USAGE_HNDL_THREAD:
		case CB_USAGE_HNDL_MENU:
		case CB_USAGE_HNDL_VARIANT:
		case CB_USAGE_HNDL_LM:
		case CB_USAGE_LENGTH:
			output ("(*(int *) (");
			output_data (x);
			output ("))");
			return;

		case CB_USAGE_POINTER:
		case CB_USAGE_PROGRAM_POINTER:
#ifdef	COB_NON_ALIGNED
			output ("(cob_get_pointer (");
			output_data (x);
			output ("))");
#else
			output ("(*(unsigned char **) (");
			output_data (x);
			output ("))");
#endif
			return;

		case CB_USAGE_DISPLAY:
			if (f->pic
			 && f->pic->scale == 0
			 && f->size > 8
			 && f->size < 16
			 && !f->flag_sign_clause
			 && !f->flag_any_numeric /* ANY NUMERIC format & usage could change */
			 && !cb_ebcdic_sign) {
				if (f->pic->have_sign) {
					optimize_defs[COB_GET_NUMDISPS64] = 1;
					output ("cob_get_numdisps64 (");
				} else {
					optimize_defs[COB_GET_NUMDISP64] = 1;
					output ("cob_get_numdisp64 (");
				}
				output_data (x);
				output (", %d)", f->size - f->pic->scale);
				return;
			}
			if (f->pic
			 && f->pic->scale >= 0
			 && f->size - f->pic->scale > 0
			 && f->size - f->pic->scale <= 9
			 && !f->flag_sign_clause
			 && !f->flag_any_numeric /* ANY NUMERIC format & usage could change */
			 && !cb_ebcdic_sign) {
				if (f->pic->have_sign) {
					optimize_defs[COB_GET_NUMDISPS] = 1;
					output ("cob_get_numdisps (");
				} else {
					optimize_defs[COB_GET_NUMDISP] = 1;
					output ("cob_get_numdisp (");
				}
				output_data (x);
				output (", %d)", f->size - f->pic->scale);
				return;
			}
			break;

		case CB_USAGE_BINARY:
		case CB_USAGE_COMP_5:
		case CB_USAGE_COMP_X:
		case CB_USAGE_COMP_N:
			if (f->size == 1) {
				output ("(*(");
				if (!f->pic->have_sign) {
					output ("cob_u8_ptr) (");
				} else {
					output ("cob_s8_ptr) (");
				}
				output_data (x);
				output ("))");
				return;
			}
#ifdef	COB_NON_ALIGNED
			if (f->storage != CB_STORAGE_LINKAGE && f->indexes == 0 && (
#ifdef	COB_SHORT_BORK
				(f->size == 2 && (f->offset % 4 == 0)) ||
#else
				(f->size == 2 && (f->offset % 2 == 0)) ||
#endif
				(f->size == 4 && (f->offset % 4 == 0)) ||
				(f->size == 8 && (f->offset % 8 == 0))))
#else
			if (f->size == 2 || f->size == 4 || f->size == 8)
#endif
				{
				if (f->flag_binary_swap) {
					output ("((");
					switch (f->size) {
					case 2:
						if (!f->pic->have_sign) {
							output ("unsigned short)COB_BSWAP_16(");
						} else {
							output ("short)COB_BSWAP_16(");
						}
						break;
					case 4:
						if (!f->pic->have_sign) {
							output ("unsigned int)COB_BSWAP_32(");
						} else {
							output ("int)COB_BSWAP_32(");
						}
						break;
					case 8:
						if (!f->pic->have_sign) {
							output ("cob_u64_t)COB_BSWAP_64(");
						} else {
							output ("cob_s64_t)COB_BSWAP_64(");
						}
						break;
					default:
						break;
					}
					output ("*(");
					switch (f->size) {
					case 2:
						output ("short *)(");
						break;
					case 4:
						output ("int *)(");
						break;
					case 8:
						output ("cob_s64_t *)(");
						break;
					default:
						break;
					}
					output_data (x);
					output (")))");
					return;
				} else {
					output ("(*(");
					switch (f->size) {
					case 2:
						if (!f->pic->have_sign) {
							output ("unsigned short *)(");
						} else {
							output ("short *)(");
						}
						break;
					case 4:
						if (!f->pic->have_sign) {
							output ("unsigned int *)(");
						} else {
							output ("int *)(");
						}
						break;
					case 8:
						if (!f->pic->have_sign) {
							output ("cob_u64_ptr)(");
						} else {
							output ("cob_s64_ptr)(");
						}
						break;
					default:
						break;
					}
					output_data (x);
					output ("))");
					return;
				}
			}
			if (f->pic->have_sign == 0) {
				output ("(unsigned int)");
			}
			break;

#if 0	/* libcob's optimized version is not slower, so drop that */
		case CB_USAGE_PACKED:
			if (f->pic->scale == 0 && f->pic->digits < 10) {
				optimize_defs[COB_GET_PACKED_INT] = 1;
				output_func_1 ("cob_get_packed_int", x);
				return;
			}
			break;
#endif

		default:
			break;
		}

		output_func_1 ("cob_get_int", x);
		break;
	}
	case CB_TAG_INTRINSIC:
		output ("cob_get_int (");
		output_param (x, -1);
		output (")");
		break;

	case CB_TAG_FUNCALL:
		output_funcall (x);
		break;

	/* LCOV_EXCL_START */
	default:
		CB_TREE_TAG_UNEXPECTED_ABORT (x);
	/* LCOV_EXCL_STOP */
	}
}

static void
output_long_integer (cb_tree x)
{
	switch (CB_TREE_TAG (x)) {
	case CB_TAG_CONST:
		if (x == cb_zero) {
			output (CB_FMT_LLD_F, COB_S64_C(0));
		} else if (x == cb_null) {
			output ("(cob_u8_ptr)NULL");
		} else {
			output ("%s", CB_CONST (x)->val);
		}
		break;
	case CB_TAG_INTEGER:
#ifdef USE_INT_HEX /* Simon: using this increases the struct and we
		 *should* pass the flags as constants in any case... */
		if (CB_INTEGER (x)->hexval) {
			output ("0x%X", CB_INTEGER (x)->val);
		} else {
			output ("%d", CB_INTEGER (x)->val);
		}
#else
		output (CB_FMT_LLD_F, (cob_s64_t)CB_INTEGER (x)->val);
#endif
		break;
	case CB_TAG_LITERAL:
		output (CB_FMT_LLD_F, cb_get_long_long (x));
		break;
	case CB_TAG_BINARY_OP: {
		const struct cb_binary_op *p = CB_BINARY_OP (x);
		if (p->flag == BOP_RESOLVE_AS_INTEGER) {
			if (!cb_fits_long_long (p->x) 
			 || !cb_fits_long_long (p->y)) {
				output ("cob_get_llint (");
				output_param (x, -1);
				output (")");
				break;
			}
		}
		if (p->op == '^') {
			/* TODO: Optimize: base of constant 1/-1 -> (int)0 */
			output ("cob_s64_pow (");
			output_long_integer (p->x);
			output (", ");
			output_long_integer (p->y);
			output (")");
		} else {
			output ("(");
			output_long_integer (p->x);
			output (" %c ", p->op);
			output_long_integer (p->y);
			output (")");
		}
		break;
	}
	case CB_TAG_CAST: {
		const struct cb_cast *cp = CB_CAST (x);
		switch (cp->cast_type) {
		case CB_CAST_ADDRESS:
			output ("(");
			output_data (cp->val);
			output (")");
			break;
		case CB_CAST_PROGRAM_POINTER:
			output ("cob_call_field (");
			output_param (x, -1);
			if (current_prog->nested_prog_list) {
				gen_nested_tab = 1;
				output (", cob_nest_tab, 0, %d)", cb_fold_call);
			} else {
				output (", NULL, 0, %d)", cb_fold_call);
			}
			break;
		/* LCOV_EXCL_START */
		default:
			cobc_err_msg (_("unexpected cast type: %d"),
				 (int)cp->cast_type);
			COBC_ABORT ();
		/* LCOV_EXCL_STOP */
		}
		break;
	}
	case CB_TAG_REFERENCE: {
		struct cb_field *f = cb_code_field (x);
		switch (f->usage) {
		case CB_USAGE_INDEX:
			if (f->index_type != CB_NORMAL_INDEX) {
				output_base (f, 1U);
				output ("(cob_s64_t)%s%d", CB_PREFIX_BASE, f->id);
				return;
			}
			/* Fall through */
		case CB_USAGE_HNDL:
		case CB_USAGE_HNDL_WINDOW:
		case CB_USAGE_HNDL_SUBWINDOW:
		case CB_USAGE_HNDL_FONT:
		case CB_USAGE_HNDL_THREAD:
		case CB_USAGE_HNDL_MENU:
		case CB_USAGE_HNDL_VARIANT:
		case CB_USAGE_HNDL_LM:
		case CB_USAGE_LENGTH:
			output ("(cob_s64_t)(*(int *) (");
			output_data (x);
			output ("))");
			return;

		case CB_USAGE_POINTER:
		case CB_USAGE_PROGRAM_POINTER:
#ifdef	COB_NON_ALIGNED
			output ("(cob_get_pointer (");
			output_data (x);
			output ("))");
#else
			output ("(*(unsigned char **) (");
			output_data (x);
			output ("))");
#endif
			return;

		case CB_USAGE_BINARY:
		case CB_USAGE_COMP_5:
		case CB_USAGE_COMP_X:
			if (f->size == 1) {
				output ("(*(");
				if (!f->pic->have_sign) {
					output ("cob_u8_ptr) (");
				} else {
					output ("cob_s8_ptr) (");
				}
				output_data (x);
				output ("))");
				return;
			}
#ifdef	COB_NON_ALIGNED
			if (f->storage != CB_STORAGE_LINKAGE && f->indexes == 0 && (
#ifdef	COB_SHORT_BORK
				(f->size == 2 && (f->offset % 4 == 0)) ||
#else
				(f->size == 2 && (f->offset % 2 == 0)) ||
#endif
				(f->size == 4 && (f->offset % 4 == 0)) ||
				(f->size == 8 && (f->offset % 8 == 0)))) {
#else
			if (f->size == 2 || f->size == 4 || f->size == 8) {
#endif
				if (f->flag_binary_swap) {
					output ("((");
					switch (f->size) {
					case 2:
						if (!f->pic->have_sign) {
							output ("unsigned short)COB_BSWAP_16(");
						} else {
							output ("short)COB_BSWAP_16(");
						}
						break;
					case 4:
						if (!f->pic->have_sign) {
							output ("unsigned int)COB_BSWAP_32(");
						} else {
							output ("int)COB_BSWAP_32(");
						}
						break;
					case 8:
						if (!f->pic->have_sign) {
							output ("cob_u64_t)COB_BSWAP_64(");
						} else {
							output ("cob_s64_t)COB_BSWAP_64(");
						}
						break;
					default:
						break;
					}
					output ("*(");
					switch (f->size) {
					case 2:
						output ("short *)(");
						break;
					case 4:
						output ("int *)(");
						break;
					case 8:
						output ("cob_s64_t *)(");
						break;
					default:
						break;
					}
					output_data (x);
					output (")))");
					return;
				} else {
					output ("(*(");
					switch (f->size) {
					case 2:
						if (!f->pic->have_sign) {
							output ("unsigned short *)(");
						} else {
							output ("short *)(");
						}
						break;
					case 4:
						if (!f->pic->have_sign) {
							output ("unsigned int *)(");
						} else {
							output ("int *)(");
						}
						break;
					case 8:
						if (!f->pic->have_sign) {
							output ("cob_u64_ptr)(");
						} else {
							output ("cob_s64_ptr)(");
						}
						break;
					default:
						break;
					}
					output_data (x);
					output ("))");
					return;
				}
			}
			break;

#if 0	/* libcob's optimized version is not slower, so drop that */
		case CB_USAGE_PACKED:
			if (f->pic->scale == 0 && f->pic->digits < 19) {
				optimize_defs[COB_GET_PACKED_INT64] = 1;
				output_func_1 ("cob_get_packed_int64", x);
				return;
			}
			break;
#endif

		default:
			break;
		}

		output_func_1 ("cob_get_llint", x);
		break;
	}
	case CB_TAG_INTRINSIC:
		output ("cob_get_llint (");
		output_param (x, -1);
		output (")");
		break;
	/* LCOV_EXCL_START */
	default:
		CB_TREE_TAG_UNEXPECTED_ABORT (x);
	/* LCOV_EXCL_STOP */
	}
}

static void
output_index (cb_tree x)
{
	/* note: integers and literals _must_ be positive */
	switch (CB_TREE_TAG (x)) {
	case CB_TAG_INTEGER:
		output ("%dLL", CB_INTEGER (x)->val - 1);
		break;
	case CB_TAG_LITERAL:
		output ("%dLL", cb_get_int (x) - 1);
		break;
	default:
		/* note: the index stored in a variable may be negative and of big
		   or small type; while we officially only support integer values
		   people use(d) UNBOUNDED or "OCCURS 1 / OCCURS 2" tables in LINKAGE
		   with bounds checks disabled, to map memmory with small records in
		   huge numbers; so cast to signed long integer to be able to safely
		   subtract 1 to get the C index (even when it is zero) while
		   preserving the value of huge integer values */
		output ("((cob_s64_t)(");
		output_integer (x);
		output (") - 1)");
		break;
	}
}

/* ML output trees */

static struct cb_ml_generate_tree *
get_last_attr (const struct cb_ml_generate_tree * const s)
{
	struct cb_ml_generate_tree	*attr;

	for (attr = s->attrs; attr->sibling; attr = attr->sibling);
	return attr;
}

static struct cb_ml_generate_tree *
get_last_child (const struct cb_ml_generate_tree * const s)
{
	struct cb_ml_generate_tree	*child;

	for (child = s->children; child->sibling; child = child->sibling);

	if (child->children) {
		return get_last_child (child);
	} else {
		return child;
	}
}

static struct cb_ml_generate_tree *
get_prev_ml_tree_entry (const struct cb_ml_generate_tree * const s)
{
	if (s->prev_sibling) {
		if (s->prev_sibling->children) {
			return get_last_child (s->prev_sibling);
		} else if (s->prev_sibling->attrs) {
			return get_last_attr (s->prev_sibling);
		} else {
			return s->prev_sibling;
		}
	} else if (s->attrs) {
		return get_last_attr (s);
	} else if (s->parent) {
		return s->parent;
	} else {
		return NULL;
	}
}

static void
output_ml_attrs_definitions (struct cb_ml_generate_tree *attr)
{
	/* TO-DO: Where does xa_7 come from?? (See test.c.l.h) */
	for (; attr; attr = attr->sibling) {
		output_local ("static cob_ml_attr\t%s%d;\n",
			      CB_PREFIX_ML_ATTR, attr->id);
	}
}

static void
output_ml_trees_definitions (struct cb_ml_generate_tree *tree)
{
	for (; tree; tree = tree->sibling) {
		output_ml_attrs_definitions (tree->attrs);
		output_ml_trees_definitions (tree->children);
		output_local ("static cob_ml_tree\t%s%d;\n", CB_PREFIX_ML_TREE, tree->id);
	}
}

/* Parameter */

static COB_INLINE COB_A_INLINE void
create_field (struct cb_field *f, cb_tree x)
{
	if (!f->flag_field) {
		struct field_list* fl;
		FILE* savetarget = output_target;
		output_target = NULL;
		output_field (x);

		fl = cobc_parse_malloc (sizeof (struct field_list));
		fl->x = x;
		fl->f = f;
		fl->curr_prog = excp_current_program_id;
		if (f->index_type != CB_INT_INDEX
		 && (f->flag_is_global
		  || current_prog->flag_file_global)) {
			fl->next = field_cache;
			field_cache = fl;
		} else {
			fl->next = local_field_cache;
			local_field_cache = fl;
		}

		f->flag_field = 1;
		output_target = savetarget;
	}
}

static void
output_param (cb_tree x, int id)
{
	struct cb_field		*f;
	cb_tree			l;
	int				add_comma = 0;

	if (x == NULL) {
		output ("NULL");
		return;
	}

	param_id = id;

	switch (CB_TREE_TAG (x)) {
	case CB_TAG_CONST:
		if (x == cb_quote) {
			gen_figurative |= CB_NEED_QUOTE;
		} else if (x == cb_norm_low) {
			gen_figurative |= CB_NEED_LOW;
		} else if (x == cb_norm_high) {
			gen_figurative |= CB_NEED_HIGH;
		} else if (x == cb_space) {
			gen_figurative |= CB_NEED_SPACE;
		} else if (x == cb_zero) {
			gen_figurative |= CB_NEED_ZERO;
		}
		output ("%s", CB_CONST (x)->val);
		break;
	case CB_TAG_INTEGER:
		output_integer (x);
		break;
	case CB_TAG_STRING: {
		const struct cb_string *str = CB_STRING (x);
		output_string (str->data, (int) str->size, 0);
		break;
	}
	case CB_TAG_LOCALE_NAME:
		output_param (CB_LOCALE_NAME(x)->list, id);
		break;
	case CB_TAG_ALPHABET_NAME: {
		const struct cb_alphabet_name	*abp = CB_ALPHABET_NAME (x);
		switch (abp->alphabet_type) {
		case CB_ALPHABET_ASCII:
#ifdef	COB_EBCDIC_MACHINE
			gen_ebcdic_ascii |= 1;
			output ("cob_ebcdic_ascii");
			break;
#endif
			/* Fall through for ASCII */
		case CB_ALPHABET_NATIVE:
			if (current_prog->collating_sequence) {
				gen_native |= 1;
				output ("cob_native");
			} else {
				output ("NULL");
			}
			break;
		case CB_ALPHABET_EBCDIC:
#ifdef	COB_EBCDIC_MACHINE
			if (current_prog->collating_sequence) {
				gen_native |= 1;
				output ("cob_native");
			} else {
				output ("NULL");
			}
#else
			output ("cob_ascii_ebcdic");
			gen_ascii_ebcdic |= 1;
#endif
			break;
		case CB_ALPHABET_CUSTOM:
			gen_custom = 1;
			output ("%s%s", CB_PREFIX_SEQUENCE, abp->cname);
			break;
		default:
			break;
		}
		break;
	}
	case CB_TAG_CAST: {
		const struct cb_cast	*cp = CB_CAST (x);
		switch (cp->cast_type) {
		case CB_CAST_INTEGER:
			output_integer (cp->val);
			break;
		case CB_CAST_LONG_INT:
			output_long_integer (cp->val);
			break;
		case CB_CAST_ADDRESS:
			output_data (cp->val);
			break;
		case CB_CAST_ADDR_OF_ADDR:
			output ("&");
			output_data (cp->val);
			break;
		case CB_CAST_LENGTH:
			output_size (cp->val);
			break;
		case CB_CAST_PROGRAM_POINTER:
			output_param (cp->val, id);
			break;
		case CB_CAST_NEGATIVE_INTEGER:
			output ("-(");
			output_integer (cp->val);
			output (")");
			break;
		case CB_CAST_NEGATIVE_LONG_INT:
			output ("-(");
			output_long_integer (cp->val);
			output (")");
			break;
		default:
			break;
		}
		break;
	}
	case CB_TAG_DECIMAL:
		output ("%s%d", CB_PREFIX_DECIMAL, CB_DECIMAL (x)->id);
		break;
	case CB_TAG_DECIMAL_LITERAL:
		output ("%s%d", CB_PREFIX_DEC_CONST, CB_DECIMAL_LITERAL (x)->id);
		break;
	case CB_TAG_FILE:
		output ("%s%s", CB_PREFIX_FILE, CB_FILE (x)->cname);
		break;
	case CB_TAG_REPORT:
		output ("&%s%s", CB_PREFIX_REPORT, CB_REPORT_PTR (x)->cname);
		break;
	case CB_TAG_REPORT_LINE: {
		const struct cb_reference	*r
		/* NOTE: do not use CB_REFERENCE_P because 'x' has a tag of CB_TAG_REPORT_LINE */
#if 1 /* FIXME: Should have expected type! */
			= (struct cb_reference *)x;
#else
			= CB_REFERENCE (x);
#endif
		f = CB_FIELD (r->value);
		output ("&%s%d", CB_PREFIX_REPORT_LINE, f->id);
		break;
	}
	case CB_TAG_LITERAL:
		if (nolitcast) {
			output ("&%s%d", CB_PREFIX_CONST, cb_lookup_literal (current_prog, x, 0));
		} else {
			output ("(cob_field *)&%s%d", CB_PREFIX_CONST,
				cb_lookup_literal (current_prog, x, 0));
		}
		break;
	case CB_TAG_FIELD:
		x = cb_build_field_reference (CB_FIELD (x), NULL);
		/* Fall through */
	case CB_TAG_REFERENCE: {
		const struct cb_reference	*r = CB_REFERENCE (x);
		if (CB_LOCALE_NAME_P (r->value)) {
			output_param (CB_LOCALE_NAME (r->value)->list, id);
			break;
		}
		if (CB_REPORT_P (r->value)) {
			output ("&%s%s", CB_PREFIX_REPORT, CB_REPORT_PTR (r->value)->cname);
			break;
		}
		if (CB_PROTOTYPE_P (r->value)) {
			const char *name = CB_PROTOTYPE (r->value)->ext_name;
			const size_t len = strlen (name);
			cb_tree lit = cb_build_alphanumeric_literal (name, len);
			output_param (lit, 0);
			break;
		}
		if (r->check) {
			int			n;
			int			sav_stack_id;
			inside_stack[inside_check++] = 0;
			/* LCOV_EXCL_START */
			if (inside_check >= COB_INSIDE_SIZE) {
				cobc_err_msg (_("internal statement stack depth exceeded: %d"),
						COB_INSIDE_SIZE);
				COBC_ABORT ();
			}
			/* LCOV_EXCL_STOP */
			output_newline ();
			output_prefix ();
			output ("(");
			n = output_indent_level;
			output_indent_level = 0;
			for (l = r->check; l; l = CB_CHAIN (l)) {
				sav_stack_id = stack_id;
				output_stmt (CB_VALUE (l));
				stack_id = sav_stack_id;
				if (l == r->check) {
					output_indent_level = n;
				}
				add_comma = 1;
			}
		}

		if (CB_FILE_P (r->value)) {
			output ("%s%s", CB_PREFIX_FILE, CB_FILE (r->value)->cname);
			if (r->check) {
				if (inside_check) {
					--inside_check;
				}
				output (" )");
			}
			break;
		}
		if (CB_ALPHABET_NAME_P (r->value)) {
			const struct cb_alphabet_name	*rbp = CB_ALPHABET_NAME (r->value);
			switch (rbp->alphabet_type) {
			case CB_ALPHABET_ASCII:
#ifdef	COB_EBCDIC_MACHINE
				gen_ebcdic_ascii |= 2;
				output ("&f_ebcdic_ascii");
				break;
#endif
			/* Fall through for ASCII */
			case CB_ALPHABET_NATIVE:
				gen_native |= 2;
				output ("&f_native");
				break;
			case CB_ALPHABET_EBCDIC:
#ifdef	COB_EBCDIC_MACHINE
				gen_native |= 2;
				output ("&f_native");
#else
				output ("&f_ascii_ebcdic");
				gen_ascii_ebcdic |= 2;
#endif
				break;
			case CB_ALPHABET_CUSTOM:
				gen_custom = 1;
				output ("&%s%s", CB_PREFIX_FIELD, rbp->cname);
				break;
			default:
				break;
			}
			if (r->check) {
				if (inside_check) {
					--inside_check;
				}
				output (" )");
			}
			break;
		}

		/* LCOV_EXCL_START */
		if (!CB_FIELD_P (r->value)) {
			cobc_err_msg (_("call to '%s' with invalid parameter '%s'"),
				"output_param", "x");
			cobc_err_msg (_("%s is not a field"), r->word->name);
			COBC_ABORT ();
		}
		/* LCOV_EXCL_STOP */

		f = CB_FIELD (r->value);

		{
			const struct cb_field	*ff = real_field_founder (f);
			if (ff->flag_external
			 || ff->flag_item_based) {
				f->flag_local = 1;
			}
			/* HACK, these are commonly only set at level 01/77
				but that _seems_ to only be necessary until codegen
				where we currently are */
			f->flag_external = ff->flag_external;
			f->flag_item_based = ff->flag_item_based;
		}
		if (!r->subs
		 && !r->offset
		 && f->count != 0
		 && !chk_field_variable_size (f)
		 && !chk_field_variable_address (f)) {
			create_field (f, x);
			if (add_comma) {
				add_comma = 0;
				output (", ");
			}
			if (f->flag_local
			 && !f->flag_data_set) {
					output ("COB_SET_DATA (%s%d, ",
						CB_PREFIX_FIELD, f->id);
					output_data (x);
					output (")");
			} else {
				if (screenptr && f->storage == CB_STORAGE_SCREEN) {
					output ("&%s%d", CB_PREFIX_SCR_FIELD, f->id);
				} else {
					output ("&%s%d", CB_PREFIX_FIELD, f->id);
				}
			}
		} else {
			if (stack_id >= num_cob_fields) {
				num_cob_fields = stack_id + 1;
			}
			if (inside_check != 0) {
				if (inside_stack[inside_check - 1] != 0) {
					inside_stack[inside_check - 1] = 0;
					output (",");
					output_newline ();
					output_prefix ();
				}
			}
			output ("COB_SET_FLD (f%d, ", stack_id++);
			output_size (x);
			output (", ");
			output_data (x);
			output (", ");
			output_attr (x);
			output (")");
		}

		if (r->check) {
			if (inside_check) {
				--inside_check;
			}
			output (" )");
		}
		break;
	}
	case CB_TAG_BINARY_OP: {
		const struct cb_binary_op	*bp = CB_BINARY_OP (x);
		output ("cob_intr_binop (");
		output_param (bp->x, id);
		output (", ");
		if (isprint(bp->op)
		 && bp->op != '"'
		 && bp->op != '\'') {
			output ("'%c'", bp->op);
		} else {
			output ("%d", bp->op);
		}
		output (", ");
		output_param (bp->y, id);
		output (")");
		break;
	}
	case CB_TAG_INTRINSIC: {
		const struct cb_intrinsic	*ip = CB_INTRINSIC (x);
		if (ip->isuser) {
			char			*func;
			l = cb_ref (ip->name);
			/* LCOV_EXCL_START */
			if (l == cb_error_node) {
				cobc_err_msg (_("call to '%s' with invalid parameter '%s'"),
					"output_param", "x");
				/* not translated as it is a highly unlikely interna abort */
				cobc_err_msg ("%s is no valid reference", cb_name (ip->name));
				COBC_ABORT ();
			}
			/* LCOV_EXCL_STOP */
			/* always convert function names to upper case */
			func = cb_encode_program_id (CB_PROTOTYPE (l)->ext_name, 0, COB_FOLD_UPPER);
			lookup_func_call (func);
			if ((call_cache || func_call_cache)
			 && (cb_flag_memory_check & CB_MEMCHK_POINTER)) {
				output ("(cob_check_fence (call_fence_pre, call_fence_post, %s, NULL), ",
					cb_statement_enum_name[STMT_BEFORE_UDF]);
			}
			output ("func_%s.funcfld (&cob_dyn_%u", func, gen_dynamic);
			gen_dynamic++;
			if (ip->intr_field || ip->args) {
				output (", ");
			}
#if	0	/* RXWRXW Func */
			if (ip->intr_tab->refmod) {
				if (ip->offset) {
					output_integer (ip->offset);
					output (", ");
				} else {
					output ("0, ");
				}
				if (ip->length) {
					output_integer (ip->length);
				} else {
					output ("0");
				}
				if (ip->intr_field || ip->args) {
					output (", ");
				}
			}
#endif
		} else {
			output ("%s (", ip->intr_tab->intr_routine);
			if (ip->intr_tab->refmod) {
				if (ip->offset) {
					output_integer (ip->offset);
					output (", ");
				} else {
					output ("0, ");
				}
				if (ip->length) {
					output_integer (ip->length);
				} else {
					output ("0");
				}
				if (ip->intr_field || ip->args) {
					output (", ");
				}
			}
		}
		if (ip->intr_field) {
			if (ip->intr_field == cb_int0) {
				output ("NULL");
			} else if (ip->intr_field == cb_int1) {
				output ("%u", cb_list_length (ip->args));
			} else {
				output_param (ip->intr_field, id);
			}
			if (ip->args) {
				output (", ");
			}
		}
		for (l = ip->args; l; l = CB_CHAIN (l)) {
			output_param (CB_VALUE (l), id);
			id++;
			param_id++;
			if (CB_CHAIN (l)) {
				output (", ");
			}
		}
		if (ip->isuser
		 && (call_cache || func_call_cache)
		 && (cb_flag_memory_check & CB_MEMCHK_POINTER)) {
			output (")");
		}
		output (")");
		break;
	}
	case CB_TAG_ML_TREE:
		output ("&%s%d", CB_PREFIX_ML_TREE, CB_ML_TREE (x)->id);
		break;

	case CB_TAG_FUNCALL:
		output_funcall (x);
		break;

	case CB_TAG_DIRECT: {
		const struct cb_direct *direct = CB_DIRECT (x);
		/* LCOV_EXCL_START */
		if (!direct->flag_is_direct || direct->flag_new_line) {
			cobc_err_msg (direct->flag_is_direct ?
				"unexpected \"direct comment\"" :
				"unexpected \"direct with newline\""
			);
			COBC_ABORT ();
		}
		/* LCOV_EXCL_STOP */
		output ("%s", direct->line);
		break;
	}

	/* LCOV_EXCL_START */
	default:
		CB_TREE_TAG_UNEXPECTED_ABORT (x);
	/* LCOV_EXCL_STOP */
	}
}

/* Function call */

static void
output_funcall_typed_report (struct cb_funcall *p, const char type)
{
	struct cb_report	*r;

	/* initialization for report */
	r = CB_REPORT_PTR (p->argv[0]);

	switch (type) {

	case 'R':	/* Generate REPORT line */
		output_newline ();
		output_line ("frame_ptr++; /*Execute any WHEN conditional code */");
		output_line ("frame_ptr->perform_through = 0;");
		perform_label ("rwfoot_",r->id,-1);
		output_line ("frame_ptr--;");
		output_newline ();
		output_prefix ();
		output("while ( cob_report_generate (");
		output_param (p->argv[0], 0);
		output(", ");
		output_param (p->argv[1], 1);
		output(") > 0 ) {");
		output_indent_level += 2;
		output_newline ();
		output_line ("frame_ptr++;");
		output_line ("frame_ptr->perform_through = 0;");
		perform_label ("rw_src_",r->id,-1);
		output_line ("frame_ptr--;");
		output_indent_level -= 2;
		output_line ("}");
		break;

	case 'T':	/* Terminate REPORT */
		output_newline ();
		output_prefix ();
		output("while ( cob_report_terminate (");
		output_param (p->argv[0], 0);
		output(") > 0 ) {");
		output_newline ();
		output_indent_level += 2;
		output_line ("frame_ptr++;");
		output_line ("frame_ptr->perform_through = 0;");
		perform_label ("rw_src_",r->id,-1);
		output_line ("frame_ptr--;");
		output_indent_level -= 2;
		output_line ("}");
		break;

	case 'M':	/* Move data for REPORT */
		output_line ("\tgoto rwexit_%d;", r->id);
		output("rwmove_%d: ",r->id);
		break;

	case 't':	/* Label for MOVE for just Footings */
		output("rwfoot_%d: ",r->id);
		break;

	case 'm':	/* End of Move data for REPORT */
		if (!cb_flag_computed_goto) {
			output_line ("\tgoto P_switch;");
		} else {
			output_line ("\tgoto *frame_ptr->return_address_ptr;");
		}
		output("rwexit_%d: ",r->id);
		break;

	case 'I':	/* Initiate REPORT */
		output_newline ();
		if(r->t_lines) {
			output_line ("/* Page Limit is %s */",cb_name (r->t_lines));
			output_prefix ();
			output ("%s%s.def_lines = ", CB_PREFIX_REPORT, r->cname);
			output_integer(r->t_lines);
			output (";");
			output_newline ();
		}
		if(r->t_columns) {
			output_line ("/* Page Limit is %s */",cb_name (r->t_columns));
			output_prefix ();
			output ("%s%s.def_cols = ", CB_PREFIX_REPORT, r->cname);
			output_integer(r->t_columns);
			output (";");
			output_newline ();
		}
		if(r->t_heading) {
			output_line ("/* Heading is %s */",cb_name (r->t_heading));
			output_prefix ();
			output ("%s%s.def_heading = ", CB_PREFIX_REPORT, r->cname);
			output_integer(r->t_heading);
			output (";");
			output_newline ();
		}
		if(r->t_footing) {
			output_line ("/* Footing is %s */",cb_name (r->t_footing));
			output_prefix ();
			output ("%s%s.def_footing = ", CB_PREFIX_REPORT, r->cname);
			output_integer(r->t_footing);
			output (";");
			output_newline ();
		}
		if(r->t_first_detail) {
			output_line ("/* First Detail is %s */",cb_name (r->t_first_detail));
			output_prefix ();
			output ("%s%s.def_first_detail = ", CB_PREFIX_REPORT, r->cname);
			output_integer(r->t_first_detail);
			output (";");
			output_newline ();
		}
		if(r->t_last_detail) {
			output_line ("/* Last Detail is %s */",cb_name (r->t_last_detail));
			output_prefix ();
			output ("%s%s.def_last_detail = ", CB_PREFIX_REPORT, r->cname);
			output_integer(r->t_last_detail);
			output (";");
			output_newline ();
		}
		if(r->t_last_control) {
			output_line ("/* Last Control is %s */",cb_name (r->t_last_control));
			output_prefix ();
			output ("%s%s.def_last_control = ", CB_PREFIX_REPORT, r->cname);
			output_integer(r->t_last_control);
			output (";");
			output_newline ();
		}
		if (r->code_clause) {
			output_prefix ();
			output ("%s%s.code_is = (char*)", CB_PREFIX_REPORT, r->cname);
			output_data (r->code_clause);
			output (";");
			output_newline ();
			output_prefix ();
			output ("%s%s.code_len = ", CB_PREFIX_REPORT, r->cname);
			output_size (r->code_clause);
			output (";");
			output_newline ();
			output_line ("%s%s.code_is_present = 1;", CB_PREFIX_REPORT, r->cname);
		} else {
			output_line ("%s%s.code_is_present = 0;", CB_PREFIX_REPORT, r->cname);
		}
		output_prefix ();
		output("while ( cob_report_initiate (");
		output_param (p->argv[0], 0);
		output(") > 0 ) {");
		output_indent_level += 2;
		output_newline ();
		output_line ("frame_ptr++;");
		output_line ("frame_ptr->perform_through = 0;");
		perform_label ("rw_src_",r->id,-1);
		output_line ("frame_ptr--;");
		output_indent_level -= 2;
		output_line ("}");
		break;

	/* LCOV_EXCL_START */
	default:
		cobc_err_msg (_("unexpected function: %s"), p->name);
		COBC_ABORT ();
	/* LCOV_EXCL_STOP */
	}
}


/* output a single char with optional (then empty)
   leading and trailing string */
static void COB_INLINE COB_A_INLINE
output_char (const char *lead, const unsigned char c, const char *trail)
{
	if (lead) {
		output ("%s", lead);
	}
#ifdef GEN_CHAR_AS_UINT		/* old "simple" version */
	output ("%u", c);
#else	/* "complex" one that we use everywhere else, like in output_string() */
	if (!isprint (c)) {
#if 0	/* octal */
		/* output ("(unsigned char)'\\%03o'", c); */
		output ("%03o", c);
#else	/* hex */
		/* output ("(unsigned char)'\\x%X'", c); */
		output ("0x%X", c);
#endif
	} else if (c == '\'' || c == '\\') {
		output ("(unsigned char)'\\%c'", c);
	} else {
		output ("(unsigned char)'%c'", c);
	}
#endif
	if (trail) {
		output ("%s", trail);
	}
}

static void
output_funcall_typed (struct cb_funcall *p, const char type)
{
	switch (type) {

	case 'E':	/* Set of one character */
		output ("*(");
		output_data (p->argv[0]);
		output (") = ");
		output_param (p->argv[1], 1);
		break;

	case 'F':	/* Move of one character */
		output ("*(");
		output_data (p->argv[0]);
		output (") = *(");
		output_data (p->argv[1]);
		output (")");
		break;

	case 'G':
		/* Test of one character */
		output ("(int)(*(");
		output_data (p->argv[0]);
		if (p->argv[1] == cb_space) {
			output (") - ' ')");
		} else if (p->argv[1] == cb_zero) {
			output (") - '0')");
		} else if (p->argv[1] == cb_low) {
			output ("))");
		} else if (p->argv[1] == cb_high) {
			output (") - 255)");
		} else if (CB_LITERAL_P (p->argv[1])) {
			output_char (") - ", CB_LITERAL (p->argv[1])->data[0], ")");
		} else {
			output (") - *(");
			output_data (p->argv[1]);
			output ("))");
		}
		break;

	case 'R':	/* Generate REPORT line */
	case 'T':	/* Terminate REPORT */
	case 'M':	/* Move data for REPORT */
	case 't':	/* Label for MOVE for just Footings */
	case 'm':	/* End of Move data for REPORT */
	case 'I':	/* Initiate REPORT */
		output_funcall_typed_report (p, type);
		break;

	case 'S':	/* Suppress flag on */
		output ("%s", CB_PREFIX_REPORT_LINE);
		output_param (p->argv[1], 0);
		output (".suppress = 1;");
		output_newline ();
		output("cob_report_suppress (");
		output_param (p->argv[0], 0);
		output(", ");
		output ("&%s",CB_PREFIX_REPORT_LINE);
		output_param (p->argv[1], 0);
		output(");");
		break;

    case ':':
		if (CB_TREE_TAG(p->argv[0]) == CB_TAG_REFERENCE
		 && CB_TREE_TAG(p->argv[2]) == CB_TAG_LITERAL) {
			struct cb_reference	*r = CB_REFERENCE (p->argv[0]);
			struct cb_field		*f = CB_FIELD (r->value);
			char	opcd = (char)(int)(long)(p->argv[1]);
			if ((opcd == '=' || opcd == '~' 
				|| opcd == '>' || opcd == '<'
				|| opcd == '[' || opcd == ']')
			 && r->subs == NULL
			 && f->pic
			 && f->pic->scale == 0
			 && f->pic->digits < 18
			 && f->pic->have_sign == 0) {
				int		i, j, len;
				char	digs[32], pack[80];
				const char	*opstr = "==";
				if (opcd == '~')
					opstr = "!=";
				else if (opcd == '>')
					opstr = ">";
				else if (opcd == '<')
					opstr = "<";
				else if (opcd == ']')
					opstr = ">=";
				else if (opcd == '[')
					opstr = "<=";
				if (f->usage == CB_USAGE_DISPLAY) {
					output ("(memcmp (");
					output_data (p->argv[0]);
					output (", \"%0*d\", %d) %s 0)",
									f->pic->digits,cb_get_int (p->argv[2]),
									f->pic->digits, opstr);
					break;
				}
				if (f->usage == CB_USAGE_PACKED) {
					len = (f->pic->digits + 2) / 2;
					len = len * 2;
					sprintf(digs,"%0*dF",len-1,cb_get_int (p->argv[2]));
					for (i=j=0; j < len; j += 2, i += 4) {
						sprintf(&pack[i],"\\x%c%c",digs[j],digs[j+1]);
					}

					output ("(memcmp (");
					output_data (p->argv[0]);
					output (", \"%s\", %d) %s 0)", pack, j / 2, opstr);
					break;
				}
				if (f->usage == CB_USAGE_COMP_6) {
					len = (f->pic->digits + 1) / 2;
					len = len * 2;
					sprintf(digs,"%0*d",len,cb_get_int (p->argv[2]));
					for (i=j=0; j < len; j += 2, i += 4) {
						sprintf(&pack[i],"\\x%c%c",digs[j],digs[j+1]);
					}

					output ("(memcmp (");
					output_data (p->argv[0]);
					output (", \"%s\", %d) %s 0)", pack, j / 2, opstr);
					break;
				}
			}
		}
		output (" (");
		output_integer (p->argv[0]);
		switch ((int)(long)(p->argv[1])) {
		case '=':	output(" == "); break;
		case '<':	output(" < "); break;
		case '[':	output(" <= "); break;
		case '>':	output(" > "); break;
		case ']':	output(" >= "); break;
		case '~':	output(" != "); break;
		case '*':	output(" * "); break;
		case '-':	output(" - "); break;
		case '+':	output(" + "); break;
		case '/':	output(" / "); break;
		default:
			cobc_err_msg (_("unexpected operator: %c"), (int)(long)p->argv[1]);
			COBC_ABORT ();
		}
		output_integer (p->argv[2]);
		output (") ");
		break;

	/* LCOV_EXCL_START */
	default:
		cobc_err_msg (_("unexpected function: %s"), p->name);
		COBC_ABORT ();
	/* LCOV_EXCL_STOP */
	}
}


static void COB_INLINE COB_A_INLINE
output_funcall_item (cb_tree x, const int i, unsigned int func_nolitcast)
{
	if (x && CB_LITERAL_P (x)) {
		nolitcast = func_nolitcast;
	} else {
		nolitcast = 0;
	}
	output_param (x, i);
}

static void
output_funcall (cb_tree x)
{
	struct cb_funcall	*p;
	cb_tree			l;
	int			i;
	const int	nolitcast_origin = nolitcast;
	const int	screenptr_origin = screenptr;

	p = CB_FUNCALL (x);
	if (p->name[0] == '$') {
		output_funcall_typed (p, p->name[1]);
		return;
	}

	if ( cb_flag_prof && p->name == cob_prof_function_call_str ) {

		int proc_idx ;

		switch ( CB_INTEGER (p->argv[0])->val ){

		case COB_PROF_EXIT_PARAGRAPH:
			proc_idx = CB_INTEGER(p->argv[1])->val;
			output ("cob_prof_exit_procedure (prof_info, %d)", proc_idx);
			break;
		case COB_PROF_ENTER_SECTION:
			proc_idx = CB_INTEGER(p->argv[1])->val;
			output ("cob_prof_enter_section (prof_info, %d)", proc_idx);
			break;
		case COB_PROF_EXIT_SECTION:
			proc_idx = CB_INTEGER(p->argv[1])->val;
			output ("cob_prof_exit_section (prof_info, %d)", proc_idx);
			break;
		case COB_PROF_ENTER_CALL:
			proc_idx = CB_INTEGER(p->argv[1])->val;
			output ("cob_prof_enter_procedure (prof_info, %d)", proc_idx);
			break;
		case COB_PROF_EXIT_CALL:
			proc_idx = CB_INTEGER(p->argv[1])->val;
			output ("cob_prof_exit_procedure (prof_info, %d)", proc_idx);
			break;
		case COB_PROF_ENTER_PARAGRAPH:
			proc_idx = CB_INTEGER(p->argv[1])->val;
			output ("cob_prof_enter_procedure (prof_info, %d);", proc_idx);
			output_newline ();
			output_prefix ();
			output ("fallthrough_label = 0");
			break;
		case COB_PROF_USE_PARAGRAPH_ENTRY: {
			int paragraph_idx = CB_INTEGER(p->argv[1])->val;
			int entry_idx = CB_INTEGER(p->argv[2])->val;
			output ("if (!fallthrough_label)");
			output_block_open ();
			output_line ("cob_prof_use_paragraph_entry (prof_info, %d, %d);",
				     paragraph_idx, entry_idx);
			output_block_close ();
			output_line ("else");
			output_block_open ();
			output_line ("fallthrough_label = 0;");
			output_block_close ();
			break;
		}
		case COB_PROF_STAYIN_PARAGRAPH:
			output ("fallthrough_label = 1");
			break;
		}
		return;
	}


	screenptr = p->screenptr;
	output ("%s (", p->name);
	for (i = 0; i < p->argc; i++) {
		if (i) {
			output (", ");
		}
		if (p->varcnt && i + 1 == p->argc) {
			output ("%d", p->varcnt);
			for (l = p->argv[i]; l; l = CB_CHAIN (l), i++) {
				output (", ");
				output_funcall_item (CB_VALUE (l), i, p->nolitcast);
			}
		} else {
			output_funcall_item (p->argv[i], i, p->nolitcast);
		}
	}
	output (")");
	nolitcast = nolitcast_origin;
	screenptr = screenptr_origin;
}

static void
output_func_1 (const char *name, cb_tree x)
{
	output ("%s (", name);
	output_param (x, param_id);
	output (")");
}

/* Condition */

/* output condition 'x' with optional storage in
   C field "ret" depending on 'save_flag' */
static void
output_cond (cb_tree x, const int save_flag)
{
	in_cond = 1;

	switch (CB_TREE_TAG (x)) {
	case CB_TAG_CONST:
		if (x == cb_true) {
			output ("1");
		} else if (x == cb_false) {
			output ("0");
		/* LCOV_EXCL_START */
		} else {
			cobc_err_msg ("invalid constant");
			COBC_ABORT ();
		}
		/* LCOV_EXCL_STOP */
		break;
	case CB_TAG_BINARY_OP: {
		const struct cb_binary_op	*p = CB_BINARY_OP (x);
		switch (p->op) {
		case '!':
			output ("!");
			output_cond (p->x, save_flag);
			break;

		case '&':
		case '|':
			output ("(");
			output_cond (p->x, save_flag);
			output (p->op == '&' ? " && " : " || ");
			output_newline ();
			output_prefix ();
			output ("    ");
			output_cond (p->y, save_flag);
			output (")");
			break;

		case '=':
		case '<':
		case '[':
		case '>':
		case ']':
		case '~':
			output ("((int)");
			if (save_flag
			 && p->flag == BOP_OPERANDS_SWAPPED) {
				output_cond (p->x, 2);
			} else {
				output_cond (p->x, save_flag);
			}
			switch (p->op) {
			case '=':
				output (" == 0");
				break;
			case '<':
				output (" < 0");
				break;
			case '[':
				output (" <= 0");
				break;
			case '>':
				output (" > 0");
				break;
			case ']':
				output (" >= 0");
				break;
			case '~':
				output (" != 0");
				break;
			default:
				/* FIXME - Check */
				break;
			}
			output (")");
			break;

		default:
			output_integer (x);
			break;
		}
		break;
	}
	case CB_TAG_FUNCALL:
		if (save_flag) {
			/* handle original swapped function */
			if (save_flag == 2) {
				output ("(ret = -");
			} else {
				output ("(ret = ");
			}
		}
		output_funcall (x);
		if (save_flag) {
			output (")");
		}
		break;
	case CB_TAG_LIST:
		if (save_flag) {
			/* handle original swapped function */
			if (save_flag == 2) {
				output ("(ret = -");
			} else {
				output ("(ret = ");
			}
		}
		inside_stack[inside_check++] = 0;
		/* LCOV_EXCL_START */
		if (inside_check >= COB_INSIDE_SIZE) {
			cobc_err_msg (_("internal statement stack depth exceeded: %d"),
					COB_INSIDE_SIZE);
			COBC_ABORT ();
		}
		/* LCOV_EXCL_STOP */
		output ("(");
		output_newline ();
		for (; x; x = CB_CHAIN (x)) {
			output_stmt (CB_VALUE (x));
		}
		if (inside_check) {
			--inside_check;
		}
		output (")");
		if (save_flag) {
			output (")");
		}
		break;
	/* LCOV_EXCL_START */
	default:
		CB_TREE_TAG_UNEXPECTED_ABORT (x);
	/* LCOV_EXCL_STOP */
	}

	in_cond = 0;
}

/* MOVE */

static void
output_move (cb_tree src, cb_tree dst)
{
	cb_tree		x;

	/* Suppress warnings */
	suppress_warn = 1;
	x = cb_build_move (src, dst);
	if (x != cb_error_node) {
		output_stmt (x);
	}
	suppress_warn = 0;
}

/* INITIALIZE */

static enum cobc_init_type
deduce_initialize_type (struct cb_initialize *p, struct cb_field *f,
			const int topfield)
{
	/* LCOV_EXCL_START */
	if (f->flag_item_78) {
		cobc_err_msg (_("unexpected CONSTANT item"));
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */

	if (f->flag_sign_separate	/* Need to use cob_move for this one */
	 && !f->children) {
		return INITIALIZE_ONE;
	}

	if (f->flag_external && p->statement == STMT_INIT_STORAGE) {
		return INITIALIZE_NONE;
	}

	if (f->redefines && (!topfield || p->statement != STMT_INITIALIZE)) {
		return INITIALIZE_NONE;
	}

	if (f->flag_filler && p->flag_no_filler_init && !f->children) {
		return INITIALIZE_NONE;
	}

	if (f->usage == CB_USAGE_CONTROL) {
		return INITIALIZE_NONE;
	}

	if (p->val && f->values) {
		return INITIALIZE_ONE;
	}

	if (p->var
	 && CB_REFERENCE_P (p->var)
	 && CB_REFERENCE (p->var)->offset) {
		/* Reference modified item */
		return INITIALIZE_ONE;
	}

	if (f->children) {
		const enum cobc_init_type		type
			= deduce_initialize_type (p, f->children, 0);
		if (type == INITIALIZE_ONE) {
			return INITIALIZE_COMPOUND;
		}
		for (f = f->children->sister; f; f = f->sister) {
			if (type != deduce_initialize_type (p, f, 0)) {
				return INITIALIZE_COMPOUND;
			}
		}
		return type;
	} else {
		cb_tree		l;
		for (l = p->rep; l; l = CB_CHAIN (l)) {
			if ((int)CB_PURPOSE_INT (l) == (int)CB_TREE_CATEGORY (f)) {
				return INITIALIZE_ONE;
			}
		}
	}

	if (p->flag_default) {
		if (p->statement == STMT_INIT_STORAGE && cb_default_byte >= 0) {
			return INITIALIZE_DEFAULT;
		}
		switch (f->usage) {
		case CB_USAGE_FLOAT:
		case CB_USAGE_DOUBLE:
		case CB_USAGE_LONG_DOUBLE:
		case CB_USAGE_FP_BIN32:
		case CB_USAGE_FP_BIN64:
		case CB_USAGE_FP_BIN128:
		case CB_USAGE_FP_DEC64:
		case CB_USAGE_FP_DEC128:
			return INITIALIZE_ONE;
		default:
			break;
		}
		switch (CB_TREE_CATEGORY (f)) {
		case CB_CATEGORY_NUMERIC_EDITED:
		case CB_CATEGORY_ALPHANUMERIC_EDITED:
		case CB_CATEGORY_NATIONAL_EDITED:
		case CB_CATEGORY_FLOATING_EDITED:
			return INITIALIZE_ONE;
		default:
			if (cb_tree_type (CB_TREE (f), f) == COB_TYPE_NUMERIC_PACKED) {
				return INITIALIZE_ONE;
			} else {
				return INITIALIZE_DEFAULT;
			}
		}
	}

	return INITIALIZE_NONE;
}

/*
 * Emit code to propagate table initialize
 */
static void
propagate_table (cb_tree x, int bgn_idx)
{
	struct cb_field *f = cb_code_field (x);
	const unsigned int occ = (unsigned int)f->occurs_max;
	cob_uli_t len = (cob_uli_t)f->size;
	cob_uli_t maxlen = len * occ;
	unsigned int j = 1;

	if (bgn_idx < 1) {
		bgn_idx = 1;
	}

	if (gen_init_working
	 || (!chk_field_variable_size (f)
	  && !f->flag_unbounded
	  && !f->depending)) {
		/* Table size is known at compile time */
		/* Generate inline 'memcpy' to propagate the array data */
		if (occ > 1) {
			output_block_open ();
			output_prefix ();
			output ("cob_u8_ptr b_ptr = ");
			output_data (x);
			if (bgn_idx > 1) {
				output (" + %ld",len * (bgn_idx - 1));
				maxlen -= len * (bgn_idx - 1);
			}
			output (";");
			output_newline ();

			/* double the chunks each time */
			do {
				output_prefix ();
				output ("memcpy (b_ptr + %6lu, b_ptr, %6lu);", len, len);
				output ("\t/* %s: %6u thru %u */",
						f->name, j + bgn_idx, j * 2 + bgn_idx - 1);
				output_newline ();
				j = j * 2;
				len = len * 2;
			} while ((j * 2) < occ);
			/* missing piece after last chunk */
			if (j < occ
			 && maxlen > len) {
				output_prefix ();
				output ("memcpy (b_ptr + %6lu, b_ptr, %6lu);",
					len, maxlen - len);
				output ("\t/* %s: %6u thru %u */",
					f->name, j + bgn_idx, occ);
				output_newline ();
			}
			output_block_close ();
		}
	} else {
		/* Table size is only known at run time */
		output_prefix ();
		output ("cob_init_table (");
		output_data (x);
		output (", ");
		output_size (x);
		output (", ");
		if (f->flag_vsize_done
		 && f->vsize == NULL
		 && !f->flag_unbounded
		 && f->depending) {
			output ("%d", f->occurs_max);
		} else {
			output_occurs (f);
		}
		output (");");
		output_newline ();
	}
}

static int
initialize_uniform_char (const struct cb_field *f,
			 const struct cb_initialize *p)
{
	if (p->statement == STMT_INIT_STORAGE && cb_default_byte >= 0) {
		return cb_default_byte;
	}

	if (f->children) {
		const int	c = initialize_uniform_char (f->children, p);
		for (f = f->children->sister; f; f = f->sister) {
			if (!f->redefines) {
				if (c != initialize_uniform_char (f, p)) {
					return -1;
				}
			}
		}
		return c;
	} else {
		switch (cb_tree_type (CB_TREE (f), f)) {
		case COB_TYPE_NUMERIC_BINARY:
			return 0;
		case COB_TYPE_NUMERIC_DISPLAY:
			return '0';
		case COB_TYPE_ALPHANUMERIC:
			return ' ';
#if 1 /* TODO: proper initialization of NATIONAL data */
		case COB_TYPE_NATIONAL:
			return ' ';
#endif
		default:
			return -1;
		}
	}
}

static void
output_figurative (cb_tree x, const struct cb_field *f, const int value,
		   const int init_occurs)
{
	/* REPORT lines are cleared to SPACES */
	if (f->storage == CB_STORAGE_REPORT
	 && value == ' ')
		return;
	output_prefix ();
	/* Check for non-standard 01 OCCURS */
	if (init_occurs) {
		output ("memset (");
		output_data (x);
		output (", %d, %d);", value, f->size * f->occurs_max);
	} else if (f->size == 1) {
		output ("*(cob_u8_ptr)(");
		output_data (x);
		output (") = %d;", value);
	} else {
		output ("memset (");
		output_data (x);
		if (CB_REFERENCE_P(x) && CB_REFERENCE(x)->length) {
			output (", %d, ", value);
			output_size (x);
			output (");");
		} else {
			output (", %d, %d);", value, f->size);
		}
	}
	output_newline ();
}

static void
output_initialize_literal (cb_tree x, struct cb_field *f,
			   struct cb_literal *l, const int init_occurs)
{
	int	n;
	int	size;
	int	lsize;

	/* Check for non-standard 01 OCCURS */
	if (init_occurs) {
		size = f->occurs_max;
		lsize = (int)l->size;
		/* Check truncated literal */
		if (lsize > f->size) {
			lsize = f->size;
		}
	} else {
		size = f->size;
		lsize = (int)l->size;
	}
	if (lsize == 1) {
		/* REPORT lines are cleared to SPACES */
		if (f->storage == CB_STORAGE_REPORT
		 && l->data[0] == ' ')
			return;
		output_prefix ();
		output ("memset (");
		output_data (x);
		if (CB_REFERENCE_P(x) && CB_REFERENCE(x)->length) {
			output (", %d, ", l->data[0]);
			output_size (x);
			output (");");
		} else {
			output (", %d, %d);", l->data[0], size);
		}
		output_newline ();
		return;
	}
	if (lsize >= size) {
		output_prefix ();
		output ("memcpy (");
		output_data (x);
		output (", ");
		output_string (l->data, size, l->llit);
		output (", %d);", size);
		output_newline ();
		return;
	}
	i_counters[0] = 1;
	if (CB_REFERENCE_P(x)) {
		cb_tree		rx;
		struct cb_reference	*r = CB_REFERENCE (x);
		for (rx = r->check; rx; rx = CB_CHAIN (rx)) {
			output_stmt (CB_VALUE (rx));
		}
	}
	if (!chk_field_variable_size(f)
	 && !f->depending
	 && !f->odo_level) {
		int off;
		output_prefix ();
		output ("memcpy (");
		output_data (x);
		output (", ");
		output_string (l->data, lsize, l->llit);
		output (", %d);", lsize);
		output_newline ();
		for (off = lsize; off+lsize < size; ) {
			output_prefix ();
			output ("memcpy (");
			output_data (x);
			output (" + %d, ", off);
			output_data (x);
			output (", %d);", lsize);
			output_newline ();
			off = off + lsize;
			lsize = lsize * 2;
		}
		if (off < size) {
			output_prefix ();
			output ("memcpy (");
			output_data (x);
			output (" + %d, ", off);
			output_data (x);
			output (", %d);", size-off);
			output_newline ();
		}
	} else {
		if (f->odo_level) {
			output_line("i0_max = i_len / %d;",lsize);
		}
		output_prefix ();
		output ("for (i0 = 0; i0 < ");
		if (f->odo_level) {
			output ("i0_max");
		} else if (f->flag_occurs) {
			output_occurs (f);
		} else {
			output ("%d",f->size/lsize);
		}
		output ("; i0++)");
		output_newline ();
		output_block_open ();
		output_prefix ();
		output ("memcpy (");
		output_data (x);
		output (" + (i0 * %d), ", lsize);
		output_string (l->data, lsize, l->llit);
		output (", %d);", lsize);
		output_newline ();
		output_block_close ();
		n = size % lsize;
		if (n) {
			output_prefix ();
			output ("memcpy (");
			output_data (x);
			output (" + (i0 * %d), ", lsize);
			output_string (l->data, n, l->llit);
			output (", %d);", n);
			output_newline ();
		}
	}
}

static void
output_initialize_fp_bindec (cb_tree x, struct cb_field *f)
{
	output_prefix ();
	output ("memset (");
	output_data (x);
	output (", 0, %d);", (int)f->size);
	output_newline ();
}

static void
output_initialize_fp (cb_tree x, struct cb_field *f)
{
	output_prefix ();
	if (f->usage == CB_USAGE_FLOAT) {
		output ("{float temp = 0.0;");
	} else {
		output ("{double temp = 0.0;");
	}
	output (" memcpy (");
	output_data (x);
	output (", (void *)&temp, sizeof(temp));}");
	output_newline ();
}

static void
output_initialize_uniform (cb_tree x, struct cb_field *f,
	const unsigned char cc, const int size)
{
	/* REPORT lines are cleared to SPACES */
	if (f->storage == CB_STORAGE_REPORT
	 && cc == ' ') {
		return;
	}

	output_prefix ();
	if (size == 1) {
		output ("*(cob_u8_ptr)(");
		output_data (x);
		output_char (") = ", cc, ";");
	} else {
		output ("memset (");
		output_data (x);
		output_char (", ", cc, ", ");
		if (size <= 0
		 || (CB_REFERENCE_P(x) && CB_REFERENCE(x)->length)) {
			output_size (x);
			output (");");
		} else if (!gen_init_working
			 && (f->flag_unbounded || !(cb_complex_odo || cb_odoslide))
			 && chk_field_variable_size (f) != NULL) {
			out_odoslide_size (f);
			output (");");
		} else {
			output ("%d);", size);
		}
	}
	output_newline ();
}

static void
output_initialize_chaining (struct cb_field *f, struct cb_initialize *p)
{
	/* only handle CHAINING for program initialization */
	if (p->statement == STMT_INITIALIZE) {
		return;
	}
	/* Note: CHAINING must be an extra initialization step as parameters not passed
	         must have standard initialization */
	if (f->flag_chained) {
		output_prefix ();
		output ("cob_chain_setup (");
		output_data (p->var);
		output (", %d, %d);", f->param_num, f->size);
		output_newline ();
	}
}

static void
output_initialize_to_value (struct cb_field *f, cb_tree x,
		const enum cob_statement statement)
{
	cb_tree			value;
	struct cb_literal	*l;
	int 		i;
	int 		size;
	int 		init_occurs;
#if defined (GEN_SINGLE_MEMCPY)
	int 		offset;
	int 		n;
#endif
	unsigned char		buffchar;

	if (!CB_LIST_P (f->values)) {
		/* common case: simple VALUE */
		value = f->values;
	} else {
		/* multiple VALUE, either from report-format
		   or from the complex table-format;
		   get the first one here */
		value = CB_VALUE (f->values);
		if (CB_TAB_VALS_P (value)) {
			/* get the first entry of many */
			value = CB_TAB_VALS (value)->values;
			value = CB_VALUE (value);
		}
	}
	/* Check for non-standard OCCURS */
	if ((f->level == 1 || f->level == 77)
	 && f->flag_occurs && statement == STMT_INIT_STORAGE) {
		init_occurs = 1;
	} else {
		init_occurs = 0;
	}
	/* figurative constants */
	if (CB_CONST_P (value)) {
		if (value == cb_space) {
			output_figurative (x, f, ' ', init_occurs);
			return;
		} else if (value == cb_low) {
			output_figurative (x, f, 0, init_occurs);
			return;
		} else if (value == cb_high) {
			output_figurative (x, f, 255, init_occurs);
			return;
		} else if (value == cb_quote) {
			if (cb_flag_apostrophe) {
				output_figurative (x, f, '\'', init_occurs);
			} else {
				output_figurative (x, f, '"', init_occurs);
			}
			return;
		} else if (value == cb_zero && f->usage == CB_USAGE_DISPLAY) {
			if (!f->flag_sign_separate && !f->flag_blank_zero) {
				output_figurative (x, f, '0', init_occurs);
			} else {
				output_move (cb_zero, x);	/* CHECKME: what about init_occurs ?*/
			}
			return;
		} else if (value == cb_null && f->usage == CB_USAGE_DISPLAY) {
			output_figurative (x, f, 0, init_occurs);
			return;
		}
	}
	/* Handle numeric literals + constants and
	   figurative constants ZERO + NULL for non-display USAGE */
	if (CB_CONST_P (value)
	 || CB_TREE_CLASS (value) == CB_CLASS_NUMERIC) {
		/* Check for non-standard 01 OCCURS */
		if (init_occurs) {
			i_counters[0] = 1;
			/* CHECKME *LATER* (we may want to use those again):
			   currently only cb_i[0] & i_counters[0] are used,
			   see also cb_const_subs */
			output_prefix ();
			output ("for (i0 = 1; i0 <= ");
			output_occurs (f);
			output ("; i0++)");
			output_newline ();
			output_block_open ();
			CB_REFERENCE (x)->subs =
				CB_BUILD_CHAIN (cb_i[0], CB_REFERENCE (x)->subs);
			output_move (value, x);
			CB_REFERENCE (x)->subs =
				CB_CHAIN (CB_REFERENCE (x)->subs);
			output_block_close ();
		} else {
			output_move (value, x);
		}
		return;
	}

	if (CB_LITERAL_P (value) && CB_LITERAL (value)->all) {
		/* ALL literal */
		output_initialize_literal (x, f,
					   CB_LITERAL (value), init_occurs);
		return;
	}

	/* Alphanumeric literal */
	/* We do not use output_move here because
	   we do not want to have the value be edited. */

	l = CB_LITERAL (value);

	/* Check for non-standard 01 OCCURS */
	if (init_occurs) {
		output_initialize_literal (x, f, l, 1);
		return;
	}

	/* zero-length literal -> generate MOVE SPACES and return */
	if (l->size == 0) {
		output_move (cb_space, x);
		return;
	}

	size = f->size;

	/* only single-byte to set, generate assignment and return */
	if (size == 1) {
		const unsigned char c = l->data[0];
		output_prefix ();
		output ("*(cob_u8_ptr)(");
		output_data (x);
		output_char (") = ", c, ";");
		output_newline ();
		return;
	}

	/* if the literal is at least as long as the target, we can use
	   either a single memset or a single memcpy; check if the former
	   is possible because it only consists of the same character */
	if (l->size >= size) {
		size_t first_pos = 0;
		size_t last_pos = size;
		/* we only care about the part that will be set in the target
		   so use the target's size; if right-justified then adjust */
		if (l->size > size
		 && f->flag_justified && cb_initial_justify) {
			first_pos = l->size - size;
			last_pos = l->size;
		}
		buffchar = l->data[first_pos];
		for (i = last_pos; i != first_pos; i--) {
			if (l->data[i] != buffchar) {
				break;
			}
		}

		if (i == first_pos) {
			/*... yes it does, so init by memset */
			output_prefix ();
			output ("memset (");
			output_data (x);
			output_char (", ", buffchar, ", ");
			output ("%u);", size);
			output_newline ();
		} else {
			/* no single value, so init by memcpy */
			output_prefix ();
			output ("memcpy (");
			output_data (x);
			output (", ");
			output_string (l->data + first_pos, size, l->llit);
			output (", %d);", size);
			output_newline ();
		}
		return;
	}

	/* literal is smaller than target, so space-padding is needed */
	{
		const unsigned int padlen = size - l->size;
		const unsigned int padstart =
			f->flag_justified && cb_initial_justify ? 0 : l->size;
		const unsigned int litstart =
			f->flag_justified && cb_initial_justify ? padlen : 0;
#if !defined (GEN_SINGLE_MEMCPY)
		if (size < 128) {
			/* for "common" small fields - generate a single memcpy
			   from a string - we use a local buffer to set that up */
			unsigned char litbuff[128];
			memcpy (litbuff + litstart, l->data, l->size);
			memset (litbuff + padstart, ' ', padlen);
			output_prefix ();
			output ("memcpy (");
			output_data (x);
			output (", ");
			output_string (litbuff, size, l->llit);
			output (", %d);", size);
			output_newline ();
		} else {
			/* otherwise: memcpy for the data, memset for padding */
			output_prefix ();
			output ("memcpy (");
			output_data (x);
			if (litstart) {
				output (" + %u", litstart);
			}
			output (", ");
			output_string (l->data, l->size, l->llit);
			output (", %d);", l->size);
			output_newline ();
			output_prefix ();
			output ("memset (");
			output_data (x);
			if (padstart) {
				output (" + %u", padstart);
			}
			output (", ' ', %u);", padlen);
			output_newline ();
		}
#else /* GEN_SINGLE_MEMCPY follows */
		/* construct buffer with full content including space-padding,
		   allowing us to generate a single memcpy */
		if (size > litsize) {
			litsize = size + 128;
			if (litbuff) {
				litbuff = cobc_main_realloc (litbuff, litsize);
			} else {
				litbuff = cobc_main_malloc (litsize);
			}
		}

		memcpy (litbuff + litstart, l->data, l->size);
		memset (litbuff + padstart, ' ', padlen);

		buffchar = *(litbuff + size - 1);
		n = 0;
		for (i = size - 1; i >= 0; i--, n++) {
			if (litbuff[i] != buffchar) {
				break;
			}
		}

		if (n > 8) {
			offset = size - n;
			size -= n;
		} else {
			offset = 0;
		}

		/* undocumented optimization (?) or taking care
		   for some old compiler's limits (?);
		   note: if this is about readability / max line length,
		   then splitting only the output of litbuff to multiple
		   lines would be enough */
		{
			cob_u32_t		inci = 0;
			for (; size > 509; size -= 509, inci += 509) {
				output_prefix ();
				output ("memcpy (");
				output_data (x);
				if (!inci) {
					output (", ");
				} else {
					output (" + %u, ", inci);
				}
				output_string (litbuff + inci, 509, l->llit);
				output (", 509);");
				output_newline ();
			}
			output_prefix ();
			output ("memcpy (");
			output_data (x);
			if (!inci) {
				output (", ");
			} else {
				output (" + %u, ", inci);
			}
			output_string (litbuff + inci, size, l->llit);
			output (", %d);", size);
			output_newline ();
		}

		if (offset
		 && n > 0) {
			if (f->storage == CB_STORAGE_REPORT	/* REPORT lines are cleared to SPACES */
			 && buffchar == ' ') {
				return;
			}
			output_prefix ();
			output ("memset (");
			output_data (x);
			output (" + %d, %u, %d);",
				offset, (unsigned int)buffchar, n);
			output_newline ();
		}
#endif	/* GEN_SINGLE_MEMCPY */
	}
}

static void
output_initialize_to_default (struct cb_field *f, cb_tree x)
{
	switch (f->usage) {
	case CB_USAGE_FLOAT:
	case CB_USAGE_DOUBLE:
	case CB_USAGE_LONG_DOUBLE:
		output_initialize_fp (x, f);
	return;
	case CB_USAGE_FP_BIN32:
	case CB_USAGE_FP_BIN64:
	case CB_USAGE_FP_BIN128:
	case CB_USAGE_FP_DEC64:
	case CB_USAGE_FP_DEC128:
		output_initialize_fp_bindec (x, f);
	return;
	default:
		break;
	}
	switch (CB_TREE_CATEGORY (x)) {
	case CB_CATEGORY_NUMERIC:
	case CB_CATEGORY_NUMERIC_EDITED:
	case CB_CATEGORY_FLOATING_EDITED:
		output_move (cb_zero, x);
		break;
	case CB_CATEGORY_ALPHANUMERIC:
	case CB_CATEGORY_ALPHANUMERIC_EDITED:
	case CB_CATEGORY_NATIONAL:
	case CB_CATEGORY_NATIONAL_EDITED:
		output_move (cb_space, x);
		break;
	/* LCOV_EXCL_START */
	default:
		cobc_err_msg (_("unexpected tree category: %d"),
			 (int)CB_TREE_CATEGORY (x));
		COBC_ABORT ();
	/* LCOV_EXCL_STOP */
	}
}

static void
output_c_info (void)
{
	if (cb_flag_c_line_directives) {
		/* note: output name is already escaped for C string */
		output ("#line %d \"%s\"", output_line_number + 1, output_name);
		output_newline ();
	}
}

static void
output_cobol_info (cb_tree x)
{
	const char *p = x->source_file;
	if (!cb_cob_line_num) {
		skip_line_num = 0;
		return;
	}
	if (!cb_flag_c_line_directives) {
		char *q = last_line_num + strlen(last_line_num);
		sprintf (last_line_num, "#line %d \"", x->source_line);
		while (*p) {
			if (*p == '\\' ) {
				*q++ = '\\';
			}
			*q++ = *p++;
		}
		sprintf (q, "\"");
	}
	output ("#line %d \"", x->source_line);
	/* escape COBOL file name for C string */
	while (*p) {
		if (*p == '\\') {
			output ("%c",'\\');
		}
		output ("%c",*p++);
	}
	output ("\"");
	skip_line_num++;
	output_newline ();
}

static void
output_init_comment_and_source_ref (struct cb_field *f)
{
	/* output comment and source location for each field */
	output_line ("/* initialize field %s */", f->name);
#ifndef	NO_INIT_SOURCE_LOC	/* allow user to not output these;
	note: this will lead to a COBOL step never land in the
	      DATA DIVISION; it will also lead to not be able to list
	      copybooks in there to be visible to the debugger */
	if (cb_flag_c_line_directives && f->common.source_line) {
		output_cobol_info (CB_TREE (f));
		output_line ("cob_nop ();");
		output_c_info ();
	}
#endif
}

static void
output_initialize_one (struct cb_initialize *p, cb_tree x)
{
	struct cb_field	*f = cb_code_field (x);

	/* Initialize TO VALUE */
	if (p->val && f->values) {
		if (p->statement == STMT_INIT_STORAGE) {
			output_init_comment_and_source_ref (f);
		}
		output_initialize_to_value (f, x, p->statement);
		return;
	}

	/* Initialize replacing */
	if (!f->children) {
		cb_tree			lrp;
		for (lrp = p->rep; lrp; lrp = CB_CHAIN (lrp)) {
			if ((int)CB_PURPOSE_INT (lrp) == (int)CB_TREE_CATEGORY (x)) {
				output_move (CB_VALUE (lrp), x);
				return;
			}
		}
	}

	/* Initialize TO DEFAULT */
	if (p->flag_default) {
		if (p->statement == STMT_INIT_STORAGE) {
			output_init_comment_and_source_ref (f);
		}
		output_initialize_to_default (f, x);
	}
}

static int	idx_incr = -1;
static int	idx_next = 0;
static int	idx_stop = 0;
static void
incr_table_index ( int idx, int *idxtbl, int *occtbl)
{
	int		k;
	idxtbl[idx_incr]++;		/* Increment subscript */
	while (idx_incr < idx
	 && !idx_stop
	 && idxtbl[idx_incr] >= occtbl[idx_incr]) {	/* At max, reset and advance next subscript */
		for (k = idx_incr; k >= 0; k--)			/* Clear subscripts below current */
			idxtbl[k] = 0;
		idx_incr++;
		if (idx_incr > idx_next) {
			idx_next++;
			if (idx_next > idx) {				/* Finished all so stop */
				idx_stop = 1;
				idx_next = 0;
				return;
			}
		}
		incr_table_index ( idx, idxtbl, occtbl);
		idx_incr = 0;
	}
	if (idx_next >= idx) {
		idx_stop = 1;
		idx_next = idx_incr = 0;
		for (k = idx - 1; k >= 0; k--)			/* Clear subscripts */
			idxtbl[k] = 0;
	}
}

/* Compute offset to occurence of the data field */
static int
get_table_offset ( int offset, int idx, int *idxtbl, int *occtbl, struct cb_field **pf)
{
	int	i;
	for (i = 0; i < idx; i++) {
		offset += idxtbl[i] * pf[i]->size;
	}
	if (idx_incr < 0) {
		idx_stop = 0;
		idx_next = idx_incr = 0;
	}

	incr_table_index (idx, idxtbl, occtbl);

	return offset;
}

/*
 * post-init for table-format VALUES ARE,
 *  initializing each occurence on its own
 */
static void
output_initialize_multi_values (struct cb_initialize *p, cb_tree x, struct cb_field *f)
{
	struct cb_field		*pf;
	struct cb_field		*pftbl[COB_MAX_SUBSCRIPTS+1] = { NULL };
	int			idxtbl[COB_MAX_SUBSCRIPTS+1] = { 0 };
	int			occtbl[COB_MAX_SUBSCRIPTS+1] = { 0 };
	int			idx, idx_clr;

#if 0 /* CHECKME: the init above should be fine */
	for (idx=0; idx <= COB_MAX_SUBSCRIPTS; idx++) {
		idxtbl[idx] = 0;
		pftbl[idx] = NULL;
	}
#endif
	idx_clr = 0;
	for (idx = 0, pf = f; pf; pf = pf->parent) {
		if (pf->flag_occurs
		 && pf->occurs_max > 1) {
			pftbl [idx] = pf;
			occtbl[idx] = pf->occurs_max;
			idx++;
		}
	}
	if (idx > 0
	 && !f->depending) {
		const int offset = f->offset;
		const cb_tree	values = f->values;
		cb_tree		l, lval;
		const int save_flag_occurs = f->flag_occurs;
		const int save_occurs_max = f->occurs_max;
		int repeated = 0;
		idx_incr = -1;
		idx_stop = 0;
		f->flag_occurs = 0;
		f->occurs_max = 0;
		lval = CB_VALUE (values);
		if (CB_TAB_VALS_P (lval)) {
			const struct cb_table_values* tvals = CB_TAB_VALS (lval);
			/* FIXME: handle FROM -> TO and
			                 FROM -> REPEATED times */
			lval = tvals->values;
			if (tvals->repeat_times == cb_null) {
				repeated = 1;
			}
		} else {
			lval = values;
		}
		while (!idx_stop) {
			for (l = lval, idx_clr = 0; l; l = CB_CHAIN (l), idx_clr++) {
				f->values = CB_VALUE (l);
				f->offset = get_table_offset ( offset, idx, idxtbl, occtbl, pftbl);
				output_initialize_one (p, x);	/* Init with each value */
				if (idx_stop) {
					/* get_table_offset may have found we're out, then exit */
					break;
				}
			}
			if (!repeated) {
				break;
			}
		}
		f->offset = offset;
		f->values = values;
		f->flag_occurs = save_flag_occurs;
		f->occurs_max = save_occurs_max;
	}
}

static int needs_table_format_value;

static void
output_initialize_record_one (struct cb_initialize *p, cb_tree c,
			      struct cb_field *f)
{
	/* check for table-format VALUES ARE, in this case
		init to default here and postpone setting of
		multi VALUES */
	if (p->val && f->values && CB_LIST_P (f->values)) {
		const cb_tree save_val = p->val;
		const unsigned char save_default = p->flag_default;
		p->val = NULL;
		p->flag_default = 1;
		output_initialize_one (p, c);
		p->flag_default = save_default;
		p->val = save_val;
		needs_table_format_value = 1;
	} else {
		output_initialize_one (p, c);
	}
}

static void
output_initialize_compound (struct cb_initialize *p, cb_tree x)
{
	const struct cb_field	*ff = cb_code_field (x);
	struct cb_field	*f, *last_field;

	for (f = ff->children; f; f = f->sister) {
		const enum cobc_init_type	type
			= deduce_initialize_type (p, f, 0);
		const cb_tree	c
			= cb_build_field_reference (f, x);

		switch (type) {
		case INITIALIZE_NONE:
			break;

		case INITIALIZE_DEFAULT: {
			int		last_char;
			last_field = f;
			last_char = initialize_uniform_char (f, p);

			if (last_char != -1) {
				if (f->flag_occurs) {
					CB_REFERENCE (c)->subs =
						CB_BUILD_CHAIN (cb_int1,
							CB_REFERENCE (c)->subs);
				}
				/* check for all next fields on the same level that have an identical
				   initialization ... */
				for (; f->sister; f = f->sister) {
					if (!f->sister->redefines) {
						if (deduce_initialize_type (p, f->sister, 0) != INITIALIZE_DEFAULT
						 || initialize_uniform_char (f->sister, p) != last_char) {
							break;
						}
					}
				}
				/* ... now initialize all of them at once */
				{
					int		size;
					if (f->sister) {
						size = f->sister->offset - last_field->offset;
					} else {
						size = ff->offset + ff->size - last_field->offset;
					}
					if (p->statement == STMT_INIT_STORAGE) {
						output_init_comment_and_source_ref (last_field);
					}
					output_initialize_uniform (c, last_field, (unsigned char)last_char, size);
				}
				break;
			}
		}
		/* Fall through */
		default:
			if (!f->flag_occurs) {
				if (type == INITIALIZE_ONE) {
					output_initialize_record_one (p, c, f);
				} else {
					output_initialize_compound (p, c);
				}
			} else {
				struct cb_reference *ref = CB_REFERENCE (c);
				const cb_tree		save_check = ref->check;
				const cb_tree		save_length = ref->length;
				cb_tree			r2;

				/* Output all 'check' first */
				for (r2 = ref->check; r2; r2 = CB_CHAIN (r2)) {
					output_stmt (CB_VALUE (r2));
				}

				/* check for table-format VALUES ARE, and OCCURS
				   without children, in this case init to default
				   here as setting of multi VALUES is postponed */
				if (type == INITIALIZE_ONE
				 && p->val
				 && f->pic
				 && f->values && CB_LIST_P (f->values)) {
					int init = -1;
					if (f->pic->category == CB_CATEGORY_NUMERIC) {
						switch (f->usage) {
						case CB_USAGE_DISPLAY:
							if (!cb_ebcdic_sign
							 && !f->pic->have_sign) {
								init = '0';
							}
							break;
						case CB_USAGE_COMP_6:
						case CB_USAGE_BINARY:
						case CB_USAGE_COMP_5:
						case CB_USAGE_COMP_X:
						case CB_USAGE_COMP_N:
							init = 0;
							break;
						default:
							break;
						};
					} else {
						init = ' ';
					}
					if (init != -1) {
						cb_tree stmt = CB_BUILD_FUNCALL_3 ("memset",
							CB_BUILD_CAST_ADDRESS (c),
							cb_int (init), cb_int (f->size * f->occurs_max));
						if (p->statement == STMT_INIT_STORAGE) {
							output_init_comment_and_source_ref (f);
						}
						output_stmt (stmt);
						continue;
						/* direct initialization possible
						   -> no need to init one-field record and propagate */
					}
				}

				/* Output initialization for the first record */
				if (f->occurs_max > 1) {
					output_line ("/* initialize %s: init first record */", f->name);
					output_block_open ();
				}
				/* all exceptions would have been raised above,
				   so temporarily detach from the reference */
				ref->check = NULL;

				ref->subs = CB_BUILD_CHAIN (cb_int1, ref->subs);
				if (type == INITIALIZE_ONE) {
					output_initialize_record_one (p, c, f);
				} else {
					ref->length = NULL;
					output_initialize_compound (p, c);
				}

				if (f->occurs_max > 1) {
					output_line ("/* initialize %s: copy initialized record to later occurrences */",
						f->name);
						propagate_table (c, 1);
					/* note: table-format VALUES are postponed until the end of the complete statement */
					output_block_close ();
				}

				/* restore previous exception-checks for the reference */
				ref->check = save_check;
				ref->length = save_length;
			}
		}
	}
}

static void
output_initialize_values_table_format_recursive (
		struct cb_initialize *p, cb_tree c,
		struct cb_field *f, const char *orig_name)
{
	if (f->children) {
		struct cb_field* fc;
		for (fc = f->children; fc; fc = fc->sister) {
			c = cb_build_field_reference (fc, NULL);
			CB_REFERENCE(c)->subs = CB_BUILD_CHAIN (cb_int1, CB_REFERENCE(c)->subs);
			if (fc->values && CB_LIST_P (fc->values)) {	/* Multiple VALUEs present */
				output_line ("/* initialize %s: handle multi-values for %s */", orig_name, fc->name);
				output_block_open ();
				output_initialize_multi_values (p, c, fc);
				output_block_close ();
			} else {
				output_initialize_values_table_format_recursive (p, c, fc, orig_name);
			}
		}
	} else {
		if (f->values && CB_LIST_P (f->values)) {	/* Multiple VALUEs present */
			output_initialize_multi_values (p, c, f);
		}
	}
}

static void
output_initialize_values_table_format (struct cb_initialize *p)
{
	if (needs_table_format_value
	 && (p->statement == STMT_INIT_STORAGE || p->val == cb_true)) {
		struct cb_field		*f = cb_code_field (p->var);
		const cb_tree		c = cb_build_field_reference (f, NULL);
		CB_REFERENCE(c)->subs = CB_BUILD_CHAIN (cb_int1, CB_REFERENCE(c)->subs);

		output_line ("/* initialize %s: handle multi-values */", f->name);
		output_block_open ();
		/* LCOV_EXCL_START */
		if (!f->children
		 && !(f->values && CB_LIST_P (f->values))) {
			cobc_err_msg (_("call to '%s' with invalid parameter '%s'"),
				"output_initialize_values_table_format", "field without multiple VALUEs");
			COBC_ABORT ();
		}
		/* LCOV_EXCL_STOP */
		output_initialize_values_table_format_recursive (p, c, f, f->name);
		output_block_close ();
	}
	needs_table_format_value = 0;
}

static void
output_initialize (struct cb_initialize *p)
{
	struct cb_field	*f = cb_code_field (p->var);
	int			c;

	const enum cobc_init_type	type
		= deduce_initialize_type (p, f, 1);

	if (type == INITIALIZE_NONE) {
		return;
	}

	/* output runtime checks */
	if (CB_REFERENCE_P (p->var)
	 && CB_REFERENCE (p->var)->check) {
		/* note: should only be when init_flag is set */
		struct cb_reference *ref = CB_REFERENCE (p->var);
		output_stmt (ref->check);
		ref->check = NULL;
	}

	/* TODO: if cb_default_byte >= 0 do a huge memset first, then only
	         emit setting for fields that need it (VALUE clause or
	         special category - in general: not matching cb_default_byte);
	         similar for cb_default_byte == CB_DEFAULT_BYTE_NONE (-2),
	         just without the initial huge memset */

	needs_table_format_value = 0;

	/* Check for non-standard OCCURS */
	if ((f->level == 1 || f->level == 77)
	 && f->flag_occurs
	 && p->statement == STMT_INIT_STORAGE) {
		cb_tree			x;
		switch (type) {
		case INITIALIZE_ONE:
			output_initialize_one (p, p->var);
			output_initialize_chaining (f, p);
			output_initialize_values_table_format (p);
			return;
		case INITIALIZE_DEFAULT:
			c = initialize_uniform_char (f, p);
			if (c != -1) {
				if (p->statement == STMT_INIT_STORAGE) {
					output_init_comment_and_source_ref (f);
				}
				output_initialize_uniform (p->var, f, (unsigned char)c, f->occurs_max);
				output_initialize_chaining (f, p);
				return;
			}
			/* Fall through */
		case INITIALIZE_COMPOUND:
			x = cb_build_field_reference (f, NULL);
			CB_REFERENCE (x)->subs = CB_BUILD_CHAIN (cb_int1, CB_REFERENCE (x)->subs);
			output_initialize_compound (p, x);
			CB_REFERENCE (x)->subs = CB_CHAIN (CB_REFERENCE (x)->subs);
			if (f->flag_occurs) {
				propagate_table (x, 1);
			}
			output_initialize_chaining (f, p);
			if (type != INITIALIZE_DEFAULT) {
				output_initialize_values_table_format (p);
			}
			return;
		default:
			break;
		}
	}

	if (f->odo_level
	 && ( CB_REFERENCE_P (p->var)
	   && CB_REFERENCE (p->var)->length)
	 && ( type != INITIALIZE_DEFAULT
	   || initialize_uniform_char (f, p) == -1)) {
		i_len_used = 1;
		output_prefix ();
		output ("i_len = ");
		output_integer (CB_REFERENCE (p->var)->length);
		output (";");
		output_newline ();
	}

	switch (type) {
	case INITIALIZE_ONE:
		output_initialize_one (p, p->var);
		output_initialize_chaining (f, p);
		output_initialize_values_table_format (p);
		return;
	case INITIALIZE_DEFAULT:
		c = initialize_uniform_char (f, p);
		if (c != -1) {
			if (p->statement == STMT_INIT_STORAGE) {
				output_init_comment_and_source_ref (f);
			}
			output_initialize_uniform (p->var, f, (unsigned char)c, f->size);
			output_initialize_chaining (f, p);
			return;
		}
		/* Fall through */
	case INITIALIZE_COMPOUND:
		output_initialize_compound (p, p->var);
		output_initialize_chaining (f, p);
		if (type != INITIALIZE_DEFAULT) {
			output_initialize_values_table_format (p);
		}
		return;
	default:
		break;
	}
}

static void
output_occurs (struct cb_field *p)
{
	if (p->depending) {
		output_integer (p->depending);
	} else {
		output ("%d", p->occurs_max);
	}
}

/* SEARCH */

static void
output_search_whens (cb_tree table, struct cb_field *p, cb_tree at_end,
						cb_tree var, cb_tree whens)
{
	cb_tree		l;
	cb_tree		idx = NULL;

	/* LCOV_EXCL_START */
	if (!p->index_list) {
		cobc_err_msg (_("call to '%s' with invalid parameter '%s'"),
			"output_search", "table");
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */

	/* Determine the index to use */
	if (var) {
		for (l = p->index_list; l; l = CB_CHAIN (l)) {
			if (cb_ref (CB_VALUE (l)) == cb_ref (var)) {
				idx = var;
			}
		}
	}
	if (!idx) {
		idx = CB_VALUE (p->index_list);
	}

	output_block_open ();
	output_prefix ();
	output ("const int max = ");
	output_occurs (p);
	output (";");
	output_newline ();

	/* Start loop */
	last_line = -1; /* force statement reference output at begin of loop */
	last_stmt = -1;
	skip_line_num++;
	output_prefix ();
	output ("if ( (");
	output_integer (idx);
	output (" < 1) || (");
	output_integer (idx);
	output (" > max) ) /* Is Table Index valid? */");
	output_newline ();
	output_block_open ();
	/* CHECKME: What should be done when the index is invalid */
	output_prefix ();
	output_integer (idx);
	output (" = 1;");
	output_newline ();
	/* -- */
	output_block_close ();
	output_line ("for (;;)");
	output_block_open ();

	/* End test */
	output_prefix ();
	output ("if ( (");
	output_integer (idx);
	output (" < 1) || (");
	output_integer (idx);
	output (" > max) )");
	output_newline ();
	output_block_open ();
	if (at_end) {
		output_source_reference (CB_PAIR_X (at_end), STMT_AT_END);
		output_stmt (CB_PAIR_Y (at_end));
	} else {
		/* position to table here, otherwise we likely land in the
		   first WHEN
		   (Note: if there's an explicit END-SEARCH there's always
		   and implicit AT END on its position (included by parser.y) */
		output_source_reference (table, STMT_AT_END);
		output_line ("break;");
	}
	output_block_close ();

	/* WHEN test */
	output_stmt (whens);
	output_newline ();

	/* Iteration */
	{
		/* Output source location as code,
		   especially for tracking adjustment of the index */
		if (var) {
			output_source_reference (var, STMT_SEARCH_VARYING);
		} else {
			int sav_fsl = cb_flag_source_location;
			cb_flag_source_location = 0;
			output_source_reference (table, STMT_SEARCH_VARYING);
			cb_flag_source_location = sav_fsl;
		}
	}
	output_prefix ();
	output_integer (idx);
	output ("++;");
	output_newline ();
	if (var && var != idx) {
		output_move (idx, var);
	}
	/* End loop */
	output_block_close ();

	output_block_close ();
}

/* generate code for SEARCH ALL,
   setup head (starting with 0) and tail (starting with max),
   using the mid as index, then compare,
   switching head/tail to the current index until match found */
static void
output_search_all (cb_tree table, struct cb_field *p, cb_tree at_end,
					cb_tree when_cond, cb_tree when_stmts)
{
	cb_tree		idx;

	idx = CB_VALUE (p->index_list);
	/* Header */
	output_block_open ();
	output_line ("int ret;");
	output_line ("int head = 0;");
	output_prefix ();
	output ("int tail = ");
	output_occurs (p);
	output (" + 1;");
	output_newline ();

	output_newline ();

	/* Start loop */
	last_line = -1; /* force statement reference output at begin of loop */
	last_stmt = -1;
	output_line ("for (;;)");
	output_block_open ();

	/* End test, note: if ODO is 0 then "if 0 >= 0+1 -1" -> direct exit */
	output_line ("if (head >= tail - 1)");
	output_block_open ();
	if (at_end) {
		output_source_reference (CB_PAIR_X (at_end), STMT_AT_END);
		output_stmt (CB_PAIR_Y (at_end));	/* this is a CB_LIST ending with "break" */
	} else {
		/* position to table here, otherwise we likely land in the
		   WHEN (Note: if there's an explicit END-SEARCH there's always
		   and implicit AT END on its position (included by parser.y) */
		output_source_reference (table, STMT_AT_END);
		output_line ("break;");
	}
	output_block_close ();
	output_newline ();

	/* Next index */
	{
		/* Output source location as code,
		   especially for tracking adjustment of the index */
		int sav_fsl = cb_flag_source_location;
		cb_flag_source_location = 0;
		output_source_reference (table, STMT_SEARCH_VARYING);
		cb_flag_source_location = sav_fsl;
	}
	output_prefix ();
	output_integer (idx);
	output (" = (head + tail) / 2;");
	output_newline ();
	output_newline ();

	/* (single) WHEN test */
	{
		/* output_source_reference would be ok here but
		   we don't want to trace this (already tracing
		   SEARCH VARYING), so temporarily disable traceall here */
		const int sav_trc_all = cb_flag_traceall;
		cb_flag_traceall = 0;
		output_source_reference (when_cond, STMT_WHEN);
		cb_flag_traceall = sav_trc_all;
	}
	output_prefix ();
	output ("if (");
	output_cond (when_cond, 1);
	output (")");
	output_newline ();
	output_block_open ();
	if (cb_flag_traceall) {
		/* Output trace info */
		/* note: the _position_ is set before the condition, but
		   for the trace code we add it here, as with traced VARYING
		   the _matched_ WHEN is enough */
		output_source_reference (when_cond, STMT_WHEN);
	}
	output_stmt (when_stmts);
	output_block_close ();
	output_newline ();

	output_line ("/* setup for next binary search position */");
	output_line ("if (ret < 0)");
	output_prefix ();
	output ("  head = ");
	output_integer (idx);
	output (";");
	output_newline ();
	output_line ("else");
	output_prefix ();
	output ("  tail = ");
	output_integer (idx);
	output (";");
	output_newline ();
	output_block_close ();
	output_block_close ();
}

static void
output_search (struct cb_search *p)
{
	struct cb_field *fp = cb_code_field (p->table);

	/* output ODO run-time check for the table */
	if (CB_EXCEPTION_ENABLE (COB_EC_BOUND_SUBSCRIPT) && fp->odo_level != 0) {
		struct cb_field *f;
		for (f = fp; f; f = f->children) {
			if (CB_VALID_TREE (f->depending)
			 && !f->flag_unbounded) {
				cb_tree check = CB_BUILD_FUNCALL_5 ("cob_check_odo",
					cb_build_cast_int (f->depending),
					cb_int (f->occurs_min),
					cb_int (f->occurs_max),
					CB_BUILD_STRING0 (f->name),
					CB_BUILD_STRING0 (CB_FIELD_PTR (f->depending)->name));
				optimize_defs[COB_CHK_ODO] = 1;
				output_stmt (check);
			}
		}
	}

	if (p->flag_all) {
		/* note: no runtime check for index, because set by this code */
		output_search_all (p->table, fp, p->at_end,
				   CB_IF (p->whens)->test, CB_IF (p->whens)->stmt1);
	} else {
		/* note: no runtime check for index, because if too big -> AT END */
		output_search_whens (p->table, fp, p->at_end, p->var, p->whens);
	}
}

/* CALL */

#if	 0	/* BWT, working through CALL BY VALUE */
static void
debug_call_by_value(cb_tree x, cb_tree l) {
	struct cb_field	*f;
	printf("CB_TREE_TAG(x) = %d\n", CB_TREE_TAG(x));
	printf("CB_TREE_CLASS(x) = %d\n", CB_TREE_CLASS(x));
	printf("CB_TREE_TAG(l) = %d\n", CB_TREE_TAG(l));
	printf("CB_TREE_CLASS(l) = %d\n", CB_TREE_CLASS(l));

	f = cb_code_field (x);
	printf("cb_code_field(x)->usage = %d\n", f->usage);
	return;
}
#endif

/**
 * cast function pointer call frame to avoid default argument promotion
 */

static const char *
get_size_parameter_type (const enum cb_param_size size, const int is_unsigned)
{
	switch (size) {
	case CB_SIZE_1:
		if (is_unsigned) {
			return "cob_u8_t";
		} else {
			return "cob_c8_t";
		}

	case CB_SIZE_2:
		if (is_unsigned) {
			return "cob_u16_t";
		} else {
			return "cob_s16_t";
		}

	case CB_SIZE_4:
		if (is_unsigned) {
			return "cob_u32_t";
		} else {
			return "cob_s32_t";
		}

	case CB_SIZE_8:
		if (is_unsigned) {
			return "cob_u64_t";
		} else {
			return "cob_s64_t";
		}

#if 0 /* reserved for future use */
	case CB_SIZE_16:
		if (is_unsigned) {
			return "cob_u128_t";
		} else {
			return "cob_s128_t";
		}

	case CB_SIZE_32:
		if (is_unsigned) {
			return "cob_u256_t";
		} else {
			return "cob_s256_t";
		}
#endif

	/* LCOV_EXCL_START */
	default:
		cobc_err_msg (_("unexpected size: %d"), size);
		COBC_ABORT ();
	/* LCOV_EXCL_STOP */
	}
}

static void
set_sign_and_size_from_parameter_field (cb_tree param, struct cb_field *f,
										int *sign, int *size)
{
	*size = CB_SIZES_INT (param);
	*sign = 0;
	if (*size == CB_SIZE_AUTO) {
		if (f->pic->have_sign) {
			*sign = 1;
		}
		if (f->usage == CB_USAGE_PACKED
		 || f->usage == CB_USAGE_DISPLAY
		 || f->usage == CB_USAGE_COMP_6) {
			*size = f->pic->digits - f->pic->scale;
		} else {
			*size = f->size;
		}
		switch (*size) {
		case 0:
#ifdef COB_64_BIT_POINTER
			*size = CB_SIZE_8;
#else
			*size = CB_SIZE_4;
#endif
			break;
		case 1:
			*size = CB_SIZE_1;
			break;
		case 2:
			*size = CB_SIZE_2;
			break;
		case 3:
		case 4:
			*size = CB_SIZE_4;
			break;
		case 5:
		case 6:
		case 7:
		default:
			*size = CB_SIZE_8;
			break;
		}
	} else {
		if (!CB_SIZES_INT_UNSIGNED(param)) {
			*sign = 1;
		}
	}
}

static void
output_call_protocast (cb_tree x, cb_tree l)
{
	switch (CB_TREE_TAG (x)) {
	case CB_TAG_CAST:
		output ("int");
		return;
	case CB_TAG_INTRINSIC:
		if (CB_INTRINSIC(x)->intr_tab->category == CB_CATEGORY_NUMERIC) {
			output ("int");
		} else {
			output ("void *");
		}
		return;
	case CB_TAG_LITERAL:
		if (CB_TREE_CLASS (x) != CB_CLASS_NUMERIC) {
			output ("int");
		} else {
			int	size = CB_SIZES_INT (l);
			if (CB_SIZES_INT_UNSIGNED(l)) {
				if (size == CB_SIZE_AUTO) {
					const cob_u64_t	uval = cb_get_u_long_long (x);
					if (uval > UINT_MAX) {
						size = CB_SIZE_8;
					} else {
						size = CB_SIZE_4;
					}
				}
				output ("%s", get_size_parameter_type (size, 0));
			} else {
				if (size == CB_SIZE_AUTO) {
					const cob_s64_t val = cb_get_long_long (x);
					if (val > INT_MAX) {
						size = CB_SIZE_8;
					} else {
						size = CB_SIZE_4;
					}
				}
				output ("%s", get_size_parameter_type (size, 1));
			}
		}
		return;
	default:
		{
			struct cb_field	*f = cb_code_field (x);
			int	size, sign;

			switch (f->usage) {
			case CB_USAGE_BINARY:
			case CB_USAGE_COMP_5:
			case CB_USAGE_COMP_X:
			case CB_USAGE_COMP_N:
			case CB_USAGE_PACKED:
			case CB_USAGE_DISPLAY:
			case CB_USAGE_COMP_6:
				set_sign_and_size_from_parameter_field (l, f, &sign, &size);
				output ("%s", get_size_parameter_type (size, sign));
				return;
			case CB_USAGE_INDEX:
				output ("cob_s32_t");
				return;
			case CB_USAGE_LENGTH:
				output ("cob_u32_t");
				return;
			case CB_USAGE_POINTER:
			case CB_USAGE_PROGRAM_POINTER:
				output ("void *");
				return;
			case CB_USAGE_FLOAT:
				output ("float");
				return;
			case CB_USAGE_DOUBLE:
				output ("double");
				return;
			case CB_USAGE_LONG_DOUBLE:
				output ("long double");
				return;
			case CB_USAGE_FP_BIN32:
				output ("cob_u32_t");
				return;
			case CB_USAGE_FP_BIN64:
			case CB_USAGE_FP_DEC64:
				output ("cob_u64_t");
				return;
			case CB_USAGE_FP_BIN128:
			case CB_USAGE_FP_DEC128:
				output ("cob_fp_128");
				return;
			default:
				output ("void *");
				return;
			}
		}
	}
}

/**
 * dereference BY VALUE arguments, sync with call_output_protocast
 */
static void
output_call_by_value_args (cb_tree x, cb_tree l)
{
	switch (CB_TREE_TAG (x)) {
	case CB_TAG_CAST:
		output_integer (x);
		return;
	case CB_TAG_INTRINSIC:
		if (CB_INTRINSIC(x)->intr_tab->category == CB_CATEGORY_NUMERIC) {
			output ("cob_get_int (");
			output_param (x, -1);
			output (")");
		} else {
			output_data (x);
		}
		return;
	case CB_TAG_LITERAL:
		if (CB_TREE_CLASS (x) != CB_CLASS_NUMERIC) {
			output ("%d", CB_LITERAL (x)->data[0]);
		} else {
			int size = CB_SIZES_INT (l);
			if (CB_SIZES_INT_UNSIGNED(l)) {
				const cob_u64_t	uval = cb_get_u_long_long (x);
				if (size == CB_SIZE_AUTO) {
					if (uval > UINT_MAX) {
						size = CB_SIZE_8;
					} else {
						size = CB_SIZE_4;
					}
				}
				output ("(%s)" CB_FMT_LLU_F,
					get_size_parameter_type (size, 0), uval);
			} else {
				const cob_s64_t	val = cb_get_long_long (x);
				if (size == CB_SIZE_AUTO) {
					if (val > INT_MAX) {
						size = CB_SIZE_8;
					} else {
						size = CB_SIZE_4;
					}
				}
				output ("(%s)" CB_FMT_LLD_F,
					get_size_parameter_type (size, 1), val);
			}
		}
		return;
	default:
		{
			struct cb_field	*f = cb_code_field (x);
			switch (f->usage) {
			case CB_USAGE_BINARY:
			case CB_USAGE_COMP_5:
			case CB_USAGE_COMP_X:
			case CB_USAGE_COMP_N:
			case CB_USAGE_PACKED:
			case CB_USAGE_DISPLAY:
			case CB_USAGE_COMP_6:
			{
				int	size, sign = 0;
				set_sign_and_size_from_parameter_field (l, f, &sign, &size);
				output ("(%s)(", get_size_parameter_type (size, sign));
				output_integer (x);
				output (")");
				return;
			}
			case CB_USAGE_INDEX:
			case CB_USAGE_HNDL:
			case CB_USAGE_HNDL_WINDOW:
			case CB_USAGE_HNDL_SUBWINDOW:
			case CB_USAGE_HNDL_FONT:
			case CB_USAGE_HNDL_THREAD:
			case CB_USAGE_HNDL_MENU:
			case CB_USAGE_HNDL_VARIANT:
			case CB_USAGE_HNDL_LM:
			case CB_USAGE_LENGTH:
			case CB_USAGE_POINTER:
			case CB_USAGE_PROGRAM_POINTER:
				output_integer (x);
				return;
			case CB_USAGE_FLOAT:
				output ("*(float *)(");
				output_data (x);
				output (")");
				return;
			case CB_USAGE_DOUBLE:
				output ("*(double *)(");
				output_data (x);
				output (")");
				return;
			case CB_USAGE_LONG_DOUBLE:
				output ("*(long double *)(");
				output_data (x);
				output (")");
				return;
			case CB_USAGE_FP_BIN32:
				output ("*(cob_u32_t *)(");
				output_data (x);
				output (")");
				return;
			case CB_USAGE_FP_BIN64:
			case CB_USAGE_FP_DEC64:
				output ("*(cob_u64_t *)(");
				output_data (x);
				output (")");
				return;
			case CB_USAGE_FP_BIN128:
			case CB_USAGE_FP_DEC128:
				output ("*(cob_fp_128 *)(");
				output_data (x);
				output (")");
				return;
			default:
				output ("*(");
				output_data (x);
				output (")");
				return;
			}
		}
	}
}

static void
output_bin_field (const cb_tree x, const cob_u32_t id)
{
	int		i;
	cob_u32_t	size;
	cob_u32_t	aflags;
	cob_u32_t	digits;

	if (!CB_NUMERIC_LITERAL_P (x)) {
		return;
	}
	aflags = COB_FLAG_REAL_BINARY;
	if (cb_fit_to_int (x)) {
		size = 4;
		digits = 9;
		aflags = COB_FLAG_HAVE_SIGN;	/* Drop: COB_FLAG_REAL_BINARY */
#ifndef WORDS_BIGENDIAN
		if (cb_binary_byteorder == CB_BYTEORDER_BIG_ENDIAN) {
			aflags |= COB_FLAG_BINARY_SWAP;
		}
#endif
	} else {
		size = 8;
		digits = 18;
		if (CB_LITERAL (x)->sign < 0) {
			aflags |= COB_FLAG_HAVE_SIGN;
		}
	}
	aflags |= COB_FLAG_CONSTANT;
	i = lookup_attr (COB_TYPE_NUMERIC_BINARY, digits, 0, aflags, NULL, 0);
	/* Note: some C compilers (SUN for one) will not accept inline initialization
	   with 'content_%u.data' because that is a local variable,
	   therefore generate this part of the assignment separately */
	output_line ("cob_field\tcontent_fb_%u = { %u, NULL, &%s%d };",
		     id, size, CB_PREFIX_ATTR, i);
}

static COB_INLINE COB_A_INLINE int
is_literal_or_prototype_ref (cb_tree x)
{
	return CB_LITERAL_P (x)
		|| (CB_REFERENCE_P (x) && CB_PROTOTYPE_P (cb_ref (x)));
}

static char *
get_program_id_str (cb_tree id_item)
{
	if (CB_LITERAL_P (id_item)) {
		return (char *)(CB_LITERAL (id_item)->data);
	} else { /* prototype */
		return (char *)CB_PROTOTYPE (cb_ref (id_item))->ext_name;
	}
}

static struct nested_list *
find_nested_prog_with_id (const char *encoded_id)
{
	struct nested_list	*nlp;

	for (nlp = current_prog->nested_prog_list; nlp; nlp = nlp->next) {
		if (!strcmp (encoded_id, nlp->nested_prog->program_id)) {
			break;
		}
	}

	return nlp;
}

static void
output_content (cb_tree x, int n, int addnul)
{
	output_line ("union {");
	output_prefix ();
	output_indent_level += indent_adjust_level;
	output ("unsigned char data[");
	if (CB_REF_OR_FIELD_P (x)) {
		output ("%u", (cob_u32_t)cb_code_field (x)->size);
	} else {
		output_size (x);
	}
	if (addnul) {
		output ("+%d];",addnul);
	} else {
		output ("];");
	}
	output_newline ();
	output_line ("cob_s64_t     datall;");
	output_line ("cob_u64_t     dataull;");
	output_line ("int           dataint;");
				output_indent_level -= indent_adjust_level;
	output_line ("} content_%u;", n);
}

static void
output_field_constant (cb_tree x, int n, const char *flagname)
{
	output_prefix ();
	if (flagname)
		output ("cob_field_%s (",flagname);
	else
		output ("cob_field_constant (");
	if (x != NULL)
		output_param (x, -1);
	else
		output ("&content_fb_%u", n);
	output (", &content_%s%u ", CB_PREFIX_FIELD, n);
	output (", &content_%s%u ", CB_PREFIX_ATTR, n);
	output (", &content_%u);", n);
	output_newline ();
}

/* output memory fence code for a given CALL 'p',
   'stmt' specifies the before/after part */
static void
output_memory_check_call (struct cb_call *p, const enum cob_statement stmt)
{
	/* fencing for used BY REFERENCE fields,
	   to prevent use of invalid data in the caller before the CALL and
	   to check for overwrite in the caller after the CALL */
	if (cb_flag_memory_check & CB_MEMCHK_USING) {
		cb_tree x, l;
		for (l = p->args; l; l = CB_CHAIN (l)) {
			if (CB_PURPOSE_INT (l) != CB_CALL_BY_REFERENCE) {
				continue;
			}
			x = CB_VALUE (l);
			if (CB_REFERENCE_P (x)) {
				x = cb_ref (x);
			}
			if (CB_FIELD_P (x)) {
				const struct cb_field *fchck = cb_field_founder (CB_FIELD (x));
				if (fchck->flag_used_in_call) {
					if (stmt == STMT_BEFORE_CALL) {
						output_line ("if (memcmp (%s%d_fence_pre, \"\\x00\\x00\\x00\\x00\\x00\\x00\\x00\", 8) == 0) {",
							CB_PREFIX_BASE, fchck->id);
						output_indent_level += indent_adjust_level;
						output_line ("memcpy (%s%d_fence_pre, \"\\xFF\\xFE\\xFD\\xFC\\xFB\\xFA\\xFF\", 8);",
							CB_PREFIX_BASE, fchck->id);
						output_line ("memcpy (%s%d_fence_post, \"\\xFA\\xFB\\xFC\\xFD\\xFE\\xFF\\xFA\", 8);",
							CB_PREFIX_BASE, fchck->id);
						output_indent_level -= indent_adjust_level;
						output_line ("} else {");
						output_indent_level += indent_adjust_level;
					}
					output_line ("cob_check_fence (%s%d_fence_pre, %s%d_fence_post, %s, \"%s\");",
						CB_PREFIX_BASE, fchck->id, CB_PREFIX_BASE, fchck->id,
						cb_statement_enum_name[stmt],
						fchck->name);
					if (stmt == STMT_BEFORE_CALL) {
						output_indent_level -= indent_adjust_level;
						output_line ("}");
					}
				}
			}
		}
	}
	/* fencing for internal pointer, to prevent SIGSEGV on CALL out-of-bound-data-in-ptr */
	if ((call_cache || func_call_cache)
	 && (cb_flag_memory_check & CB_MEMCHK_POINTER)) {
		output_line ("cob_check_fence (call_fence_pre, call_fence_post, %s, NULL);",
			cb_statement_enum_name[stmt]);
	}
}

static void
output_call (struct cb_call *p)
{
	cb_tree				x;
	cb_tree				l;
	const char			*name_str;
	struct nested_list		*nlp;
	const struct system_table	*psyst = NULL;
	const char			*convention;
	struct cb_text_list		*ctl;
	char				*s, *callname, *system_call;
	cob_u32_t			n;
	size_t				ret_ptr = 0;
	size_t				gen_exit_program = 0;
	size_t				dynamic_link = 1;
	size_t				need_brace;
	const int			name_is_literal_or_prototype
		= is_literal_or_prototype_ref (p->name);
	int				except_id;
	unsigned int 			pval;

	except_id = 0;
	if (p->call_returning
	 && p->call_returning != cb_null
	 && CB_TREE_CLASS(p->call_returning) == CB_CLASS_POINTER) {
		ret_ptr = 1;
	}
	system_call = NULL;

#ifdef	_WIN32
	if (p->convention & CB_CONV_STDCALL) {
		convention = "_std";
	} else {
		convention = "";
	}
#else
	convention = "";
#endif

	/* System routine entry points */
	if (p->is_system) {
		psyst = &system_tab[p->is_system - 1];
		dynamic_link = 0;
		system_call = (char *)psyst->syst_call;
	}

	if (dynamic_link && name_is_literal_or_prototype) {
		/* no static link for calls with exception statement */
		if ((cb_flag_static_call && !p->stmt1)
		 || (p->convention & CB_CONV_STATIC_LINK)) {
			dynamic_link = 0;
		}

		if (CB_LITERAL_P (p->name)) {
			name_str = (const char *) CB_LITERAL (p->name)->data;
		} else { /* prototype */
			name_str = CB_PROTOTYPE (cb_ref (p->name))->ext_name;
		}

		for (ctl = cb_static_call_list; ctl; ctl = ctl->next) {
			if (!strcmp (name_str, ctl->text)) {
				dynamic_link = 0;
				break;
			}
		}
		for (ctl = cb_early_exit_list; ctl; ctl = ctl->next) {
			if (!strcmp (name_str, ctl->text)) {
				gen_exit_program = 1;
				break;
			}
		}
	}
	need_brace = 0;

	if (p->call_returning && p->call_returning != cb_null) {
		if (!ret_ptr) {
			if (!need_brace) {
				need_brace = 1;
				output_block_open ();
			}
			output_line ("int ret;");
		}
#ifdef	COB_NON_ALIGNED
		else {
			if (!need_brace) {
				need_brace = 1;
				output_block_open ();
			}
			output_line ("void *temptr;");
		}
#endif
	}

	if (CB_REFERENCE_P (p->name)
	 && CB_FIELD_P (CB_REFERENCE (p->name)->value)
	 && CB_FIELD (CB_REFERENCE (p->name)->value)->usage == CB_USAGE_PROGRAM_POINTER) {
		dynamic_link = 0;
	}

	/*
	 * Check for LENGTH OF as BY REFERENCE and change to BY CONTENT
	 * as LENGTH OF is effectively a numeric literal
	 */
	for (l = p->args, n = 1; l; l = CB_CHAIN (l), n++) {
		x = CB_VALUE (l);
		if (CB_PURPOSE_INT (l) == CB_CALL_BY_REFERENCE
		 && CB_REF_OR_FIELD_P (x)
		 && CB_TREE_CATEGORY (x) == CB_CATEGORY_NUMERIC
		 && cb_code_field (x)->usage == CB_USAGE_LENGTH) {
			CB_PURPOSE (l) = cb_int (CB_CALL_BY_CONTENT);
		}
	}

	/* Set up arguments */
	for (l = p->args, n = 1; l; l = CB_CHAIN (l), n++) {
		x = CB_VALUE (l);
		switch (CB_PURPOSE_INT (l)) {
		case CB_CALL_BY_REFERENCE:
			if (CB_NUMERIC_LITERAL_P (x) || CB_BINARY_OP_P (x)) {
				if (!need_brace) {
					need_brace = 1;
					output_block_open ();
				}
				output_line ("cob_content\tcontent_%u;", n);
				output_bin_field (x, n);
			} else if (CB_CAST_P (x)) {
				if (!need_brace) {
					need_brace = 1;
					output_block_open ();
				}
				output_line ("void *ptr_%u;", n);
			}
			break;
		case CB_CALL_BY_CONTENT:
			if (CB_CAST_P (x)) {
				if (!need_brace) {
					need_brace = 1;
					output_block_open ();
				}
				output_line ("void *ptr_%u;", n);
			} else if (CB_TREE_TAG (x) != CB_TAG_INTRINSIC
				   && x != cb_null
				   && !(CB_CAST_P (x))) {
				if (!need_brace) {
					need_brace = 1;
					output_block_open ();
				}
				if (CB_NUMERIC_LITERAL_P (x)
				    || CB_BINARY_OP_P (x)
				    || CB_CAST_P(x)) {
					output_line ("cob_content\tcontent_%u;", n);
				} else {
					output_content (x, n, 1);
				}
				output_line ("cob_field      content_%s%u;", CB_PREFIX_FIELD, n);
				output_line ("cob_field_attr content_%s%u;", CB_PREFIX_ATTR, n);
				output_bin_field (x, n);
			}
			break;
		case CB_CALL_BY_VALUE:
			if (!need_brace) {
				need_brace = 1;
				output_block_open ();
			}
			if (CB_TREE_TAG (x) == CB_TAG_REFERENCE
			    && CB_TREE_TAG (CB_REFERENCE(x)->value) == CB_TAG_FIELD
			    && CB_TREE_CATEGORY (x) == CB_CATEGORY_NUMERIC
			    && cb_code_field (x)->usage == CB_USAGE_LENGTH) {
				if (CB_NUMERIC_LITERAL_P (x)
				    || CB_BINARY_OP_P (x)
				    || CB_CAST_P(x)) {
					output_line ("cob_content\tcontent_%u;", n);
				} else {
					output_content (x, n, 0);
					output_line ("cob_field      content_%s%u;", CB_PREFIX_FIELD, n);
				}
				output_line ("cob_field_attr content_%s%u;", CB_PREFIX_ATTR, n);
				output_bin_field (x, n);
			} else {
				if (CB_NUMERIC_LITERAL_P (x)
				    || CB_BINARY_OP_P (x)
				    || CB_CAST_P(x)) {
					output_line ("cob_content\tcontent_%u;", n);
				} else {
					output_content (x, n, 0);
				}
				output_line ("cob_field_attr content_%s%u;", CB_PREFIX_ATTR, n);
				output_line ("cob_field      content_%s%u;", CB_PREFIX_FIELD, n);
			}
			break;
		default:
			break;
		}
	}

	if (need_brace) {
		output_newline ();
	}

	for (l = p->args, n = 1; l; l = CB_CHAIN (l), n++) {
		x = CB_VALUE (l);
		switch (CB_PURPOSE_INT (l)) {
		case CB_CALL_BY_REFERENCE:
			if (CB_NUMERIC_LITERAL_P (x)) {
				/* Set 'data' after all variable declarations */
				output_line ("content_fb_%u.data = content_%u.data;", n, n);

				output_prefix ();
				if (cb_fit_to_int (x)) {
					pval = (unsigned int)cb_get_int (x);
					output ("cob_set_int(&content_fb_%d, %d)",n,pval);
				} else {
					if (CB_LITERAL (x)->sign >= 0) {
						output ("content_%u.dataull = ", n);
						output (CB_FMT_LLU_F,
							cb_get_u_long_long (x));
					} else {
						output ("content_%u.datall = ", n);
						output (CB_FMT_LLD_F,
							cb_get_long_long (x));
					}
				}
				output (";");
				output_newline ();
			} else if (CB_BINARY_OP_P (x)) {
				output_prefix ();
				output ("content_%u.dataint = ", n);
				output_integer (x);
				output (";");
				output_newline ();
			} else if (CB_CAST_P (x)) {
				output_prefix ();
				output ("ptr_%u = ", n);
				output_integer (x);
				output (";");
				output_newline ();
			}
			break;
		case CB_CALL_BY_CONTENT:
			if (CB_CAST_P (x)) {
				output_prefix ();
				output ("ptr_%u = ", n);
				output_integer (x);
				output (";");
				output_newline ();
			} else if (CB_TREE_TAG (x) != CB_TAG_INTRINSIC) {
				if (CB_NUMERIC_LITERAL_P (x)) {
					/* Set 'data' after all variable declarations */
					output_line ("content_fb_%u.data = content_%u.data;", n, n);

					output_prefix ();
					if (cb_fit_to_int (x)) {
						pval = (unsigned int)cb_get_int (x);
						output ("cob_set_int(&content_fb_%d, %d)",n,pval);
					} else if (CB_LITERAL (x)->sign >= 0) {
						output ("content_%u.dataull = ", n);
						output (CB_FMT_LLU_F, cb_get_u_long_long (x));
					} else {
						output ("content_%u.datall = ", n);
						output (CB_FMT_LLD_F, cb_get_long_long (x));
					}
					output (";");
					output_newline ();

					output_field_constant (NULL, n, NULL);
				} else if (CB_REF_OR_FIELD_P (x)
				        && CB_TREE_CATEGORY (x) == CB_CATEGORY_NUMERIC
				        && cb_code_field (x)->usage == CB_USAGE_LENGTH) {
					output_field_constant (x, n, NULL);
#ifndef WORDS_BIGENDIAN
					if (cb_binary_byteorder == CB_BYTEORDER_BIG_ENDIAN) {
						output_line ("content_%s%u.flags |= COB_FLAG_BINARY_SWAP;",
									CB_PREFIX_ATTR, n);
						output_prefix ();
						output ("content_%u.dataint = ", n);
						output ("(unsigned int)COB_BSWAP_32(");
						output_integer (x);
						output (");");
						output_newline ();
					}
#endif
				} else if (x != cb_null && !(CB_CAST_P (x))) {
					/*
					 * Create copy of cob_field&attr pointing to local copy of data
					 * and setting flag COB_FLAG_CONSTANT
					 */
					output_field_constant (x, n, "content");
					if (CB_LITERAL_P (x)
					 && !CB_NUMERIC_LITERAL_P (x)) {
						output_line ("content_%u.data[%d] = 0;",n,CB_LITERAL (x)->size);
					}
				}
			}
			break;
		case CB_CALL_BY_VALUE:
			output_field_constant (x, n, "value");
			break;
		default:
			break;
		}
	}

	/* Set up parameter types */
	n = 0;
	for (l = p->args; l; l = CB_CHAIN (l), n++) {
		x = CB_VALUE (l);
		field_iteration = n;
		output_prefix ();
		output ("cob_procedure_params[%u] = ", n);
		switch (CB_TREE_TAG (x)) {
		case CB_TAG_LITERAL:
			if (CB_NUMERIC_LITERAL_P (x)
			 && CB_PURPOSE_INT (l) != CB_CALL_BY_VALUE) {
				output ("&content_fb_%u", n + 1);
				break;
			}
			/* Fall through */
		case CB_TAG_FIELD:
			if (CB_PURPOSE_INT (l) == CB_CALL_BY_CONTENT
			 || CB_PURPOSE_INT (l) == CB_CALL_BY_VALUE) {
				output ("&content_%s%u", CB_PREFIX_FIELD, n + 1);
				break;
			}
			output_param (x, -1);
			break;
		case CB_TAG_INTRINSIC:
			output_param (x, -1);
			break;
		case CB_TAG_REFERENCE:
			switch (CB_TREE_TAG (CB_REFERENCE(x)->value)) {
			case CB_TAG_LITERAL:
			case CB_TAG_INTRINSIC:
				if (CB_PURPOSE_INT (l) == CB_CALL_BY_CONTENT
				 || CB_PURPOSE_INT (l) == CB_CALL_BY_VALUE) {
					output ("&content_%s%u", CB_PREFIX_FIELD, n + 1);
					break;
				}
				output_param (x, -1);
				break;
			case CB_TAG_FIELD:
				if (CB_PURPOSE_INT (l) == CB_CALL_BY_CONTENT
				 || CB_PURPOSE_INT (l) == CB_CALL_BY_VALUE
				 || (CB_TREE_CATEGORY (x) == CB_CATEGORY_NUMERIC
				  && cb_code_field (x)->usage == CB_USAGE_LENGTH)) {
					output ("&content_%s%u", CB_PREFIX_FIELD, n + 1);
					break;
				}
				output_param (x, -1);
				break;
			default:
				output ("NULL");
				break;
			}
			break;
		case CB_TAG_CONST:
			if (CB_PURPOSE_INT (l) == CB_CALL_BY_CONTENT
			 || CB_PURPOSE_INT (l) == CB_CALL_BY_VALUE) {
				output ("&content_%s%u", CB_PREFIX_FIELD, n + 1);
				break;
			}
			if (x == cb_null) {
				output ("NULL /*OMITTED*/");
			} else {
				output_param (x, 0);
			}
			break;
		default:
			output ("NULL");
			break;
		}
		output (";");
		output_newline ();
	}

	/* Set number of parameters */
	output_line ("cob_glob_ptr->cob_call_params = %u;", n);
	if (recent_prog != NULL
	 && recent_prog->max_call_param > n) {
		if ((recent_prog->max_call_param - n) > 1) {
			output_line ("memset(&cob_procedure_params[%d],0,sizeof(cob_procedure_params[0])*%d);",
								n, (recent_prog->max_call_param - n));
		} else {
			output_line ("cob_procedure_params[%u] = NULL;", n);
		}
	}
	callname = NULL;

	/* pass information about available ON EXCEPTION */
	if (p->stmt1) {
		output_line ("cob_glob_ptr->cob_stmt_exception = 1;");
		output_line ("COB_RESET_EXCEPTION (0);");
	} else {
		output_line ("cob_glob_ptr->cob_stmt_exception = 0;");
	}

	if (dynamic_link) {
		/* ensure that we don't have a program exception set already
		   as this will be checked directly when returning from CALL */
		output_line ("if ((cob_global_exception & 0x%04x) == 0x%04x) "
				"cob_global_exception = 0;",
			CB_EXCEPTION_CODE(COB_EC_PROGRAM), CB_EXCEPTION_CODE(COB_EC_PROGRAM));
	}

	/* fence check before the CALL to ensure it works and gets untrashed 01/77 */
	if (cb_flag_memory_check) {
		output_memory_check_call (p, STMT_BEFORE_CALL);
	}

	/* Function name */
	output_prefix ();
	/* Special for program pointers */
	if (CB_REFERENCE_P (p->name)
	 && CB_FIELD_P (CB_REFERENCE (p->name)->value)
	 && CB_FIELD (CB_REFERENCE (p->name)->value)->usage
	    == CB_USAGE_PROGRAM_POINTER) {
		needs_unifunc = 1;
		output ("cob_unifunc.funcvoid = ");
		output_integer (p->name);
		output (";");
		output_newline ();
		output_prefix ();
		if (p->call_returning == cb_null) {
			output ("cob_unifunc.funcnull");
		} else if (ret_ptr) {
#ifdef	COB_NON_ALIGNED
			output ("temptr");
#else
			output_integer (p->call_returning);
#endif
			output (" = cob_unifunc.funcptr");
		} else if (p->call_returning) {
			output ("ret = cob_unifunc.funcint");
		} else if (p->convention & CB_CONV_NO_RET_UPD
		       ||  !current_prog->cb_return_code) {
			output ("(void)cob_unifunc.funcint");
		} else {
			output_integer (current_prog->cb_return_code);
			output (" = cob_unifunc.funcint");
		}
	} else if (!dynamic_link) {
		/* Static link */
		if (system_call) {
			callname = system_call;
		} else {
			callname = (char *)(CB_LITERAL (p->name)->data);
		}
		output_line ("cob_glob_ptr->cob_call_name_hash = 0x%X;",
				cob_get_name_hash(callname));
		output_line ("COB_RESET_EXCEPTION (0);");
		output_prefix ();
		if (p->call_returning != cb_null) {
			if (ret_ptr) {
#ifdef	COB_NON_ALIGNED
				output ("temptr");
#else
				output_integer (p->call_returning);
#endif
				output (" = (void *)");
			} else if (p->call_returning) {
				output ("ret = ");
			} else if (!(p->convention & CB_CONV_NO_RET_UPD)
			        && current_prog->cb_return_code) {
				output_integer (current_prog->cb_return_code);
				output (" = ");
			} else {
				output ("(void)");
			}
		}
		if (psyst) {
			output ("%s", (char *)psyst->syst_call);
		} else {
			s = get_program_id_str (p->name);
			name_str = cb_encode_program_id (s, 1, cb_fold_call);

			/* Check contained programs */
			nlp = find_nested_prog_with_id (name_str);
			if (nlp) {
				output ("%s_%d__", name_str,
					nlp->nested_prog->toplev_count);
			} else {
				output ("%s", name_str);
				if (cb_flag_c_decl_for_static_call) {
					if (p->call_returning == cb_null) {
						lookup_static_call (name_str, p->convention, COB_RETURN_NULL);
					} else if (ret_ptr == 1) {
						lookup_static_call (name_str, p->convention, COB_RETURN_ADDRESS_OF);
					} else {
						lookup_static_call (name_str, p->convention, COB_RETURN_INT);
					}
				}
			}
		}
	} else {
		/* Dynamic link */
		if (name_is_literal_or_prototype) {
			s = get_program_id_str (p->name);
			name_str = cb_encode_program_id (s, 1, cb_fold_call);
			lookup_call (name_str);
			callname = s;

			output_line ("if (call_%s.funcvoid == NULL || cob_glob_ptr->cob_physical_cancel == 1)", name_str);
			output_block_open ();
			output_prefix ();

			nlp = find_nested_prog_with_id (name_str);
			if (nlp) {
				output ("call_%s.funcint = %s_%d__;",
					name_str, name_str,
					nlp->nested_prog->toplev_count);
			} else {
				output ("call_%s.funcvoid = ", name_str);
				output ("cob_resolve_cobol (");
				output_string ((const unsigned char *)s,
						(int)strlen (s), 0);
				output (", %d, %d);", cb_fold_call, !p->stmt1);
			}
			output_newline ();
			output_block_close ();
		} else {
			name_str = NULL;
			needs_unifunc = 1;
			output ("cob_unifunc.funcvoid = cob_call_field (");
			output_param (p->name, -1);
			if (current_prog->nested_prog_list) {
				gen_nested_tab = 1;
				output (", cob_nest_tab, %d, %d);",
					!p->stmt1, cb_fold_call);
			} else {
				output (", NULL, %d, %d);",
					!p->stmt1, cb_fold_call);
			}
			output_newline ();
		}
		if (p->stmt1) {
			if (name_str) {
				output_line ("if (call_%s.funcvoid == NULL)", name_str);
			} else {
				output_line ("if (cob_unifunc.funcvoid == NULL)");
			}
			output_block_open ();
			except_id = cb_id++;
			output_line ("%s%d:", CB_PREFIX_LABEL, except_id);
			output_stmt (p->stmt1);
			output_block_close ();
			output_line ("else");
			output_block_open ();
		}
		if (callname) {
			output_line ("cob_glob_ptr->cob_call_name_hash = 0x%X;",
					cob_get_name_hash(callname));
			output_line ("COB_RESET_EXCEPTION (0);");
		}
		output_prefix ();
		/* call frame cast prototype */
		if (ret_ptr) {
#ifdef	COB_NON_ALIGNED
			output ("temptr");
#else
			output_integer (p->call_returning);
#endif
			output (" = ((void *(*)");
		} else if (p->call_returning == cb_null) {
			output ("((void (*)");
		} else if (p->call_returning) {
			output ("ret = ((int (*)");
		} else if (p->convention & CB_CONV_NO_RET_UPD
		        || !current_prog->cb_return_code) {
			output ("((int (*)");
		} else {
			output_integer (current_prog->cb_return_code);
			output (" = ((int (*)");
		}
		if (p->args) {
			output ("(");
		} else {
			output ("(void)");
		}
		for (l = p->args, n = 1; l; l = CB_CHAIN (l), n++) {
			x = CB_VALUE (l);
			field_iteration = n - 1U;
			switch (CB_PURPOSE_INT (l)) {
			case CB_CALL_BY_REFERENCE:
			case CB_CALL_BY_CONTENT:
				output ("void *");
				break;
			case CB_CALL_BY_VALUE:
				output_call_protocast (x, l);
				break;
			default:
				break;
			}
			if (CB_CHAIN (l)) {
				output (", ");
			}
		}
		if (p->args) {
			output (")");
		}
		output(")");

		if (p->call_returning == cb_null) {
			if (name_str) {
				output ("call_%s.funcnull%s", name_str, convention);
			} else {
				output ("cob_unifunc.funcnull%s", convention);
			}
		} else if (ret_ptr) {
			if (name_str) {
				output ("call_%s.funcptr%s", name_str, convention);
			} else {
				output ("cob_unifunc.funcptr%s", convention);
			}
		} else {
			if (name_str) {
				output ("call_%s.funcint%s", name_str, convention);
			} else {
				output ("cob_unifunc.funcint%s", convention);
			}
		}
		output (")");
	}

	/* Arguments */
	output (" (");
	for (l = p->args, n = 1; l; l = CB_CHAIN (l), n++) {
		x = CB_VALUE (l);
		field_iteration = n - 1U;
		switch (CB_PURPOSE_INT (l)) {
		case CB_CALL_BY_REFERENCE:
			if (CB_NUMERIC_LITERAL_P (x) || CB_BINARY_OP_P (x)) {
				output ("content_%u.data", n);
			} else if (CB_REFERENCE_P (x) && CB_FILE_P (cb_ref (x))) {
				output_param (cb_ref (x), -1);
			} else if (CB_CAST_P (x)) {
				output ("&ptr_%u", n);
			} else {
				output_data (x);
			}
			break;
		case CB_CALL_BY_CONTENT:
			if (CB_TREE_TAG (x) != CB_TAG_INTRINSIC && x != cb_null) {
				if (CB_CAST_P (x)) {
					output ("&ptr_%u", n);
				} else {
					output ("content_%u.data", n);
				}
			} else {
				output_data (x);
			}
			break;
		case CB_CALL_BY_VALUE:
			output_call_by_value_args (x, l);
			break;
		default:
			break;
		}
		/* early exit if call parameters don't match, this is actually
		needed for all static calls, but only possible with an
		external program repository, so just do so for system calls
		*/
		if (psyst && psyst->syst_max_params == n) {
			break;
		}
		if (CB_CHAIN (l)) {
			output (", ");
		}
	}

	output (");");
	output_newline ();
	if (current_prog->local_storage
	 && cb_flag_symbols) {
		output_line ("cob_local_save = cob_local_ptr;");
	}

	if (except_id > 0) {
		output_line ("if ((cob_glob_ptr->cob_exception_code & 0x%04x) == 0x%04x)",
			CB_EXCEPTION_CODE(COB_EC_PROGRAM), CB_EXCEPTION_CODE(COB_EC_PROGRAM));
		output_line ("\tgoto %s%d;", CB_PREFIX_LABEL, except_id);
	}

	if (p->call_returning) {
		if (ret_ptr) {
#ifdef	COB_NON_ALIGNED
			output_prefix ();
			output ("memcpy (");
			output_data (p->call_returning);
			output (", &temptr, %u);", (cob_u32_t)sizeof (void *));
			output_newline ();
#endif
		} else if (p->call_returning != cb_null) {
			output_prefix ();
			output ("cob_set_int (");
			output_param (p->call_returning, -1);
			output (", ret);");
			output_newline ();
		}
	}
	if (gen_exit_program) {
		needs_exit_prog = 1;
		output_line ("if (module->flag_exit_program)");
		output_block_open ();
		output_line ("module->flag_exit_program = 0;");
		if (current_prog->prog_type == COB_MODULE_TYPE_FUNCTION) {
			output_line ("goto exit_function;");
		} else {
			output_line ("goto exit_program;");
		}
		output_block_close ();
	}
	/* fence check after the CALL (to hint at the callee trashing memory) */
	if (cb_flag_memory_check) {
		output_memory_check_call (p, STMT_CALL);
	}
	/* output of "NOT ON EXCEPTION" code */
	if (p->stmt2) {
		output_stmt (p->stmt2);
	}

	if (dynamic_link && p->stmt1) {
		output_block_close ();
	}

	if (need_brace) {
		output_block_close ();
	}
}

/* SET ATTRIBUTE */

static void
output_set_attribute (const struct cb_field *f, cob_flags_t val_on,
		      cob_flags_t val_off)
{
	/* Extension */
	/* Prevent specifying HIGHLIGHT and LOWLIGHT simultaneously. */
	if (val_on & COB_SCREEN_HIGHLIGHT) {
		val_off |= COB_SCREEN_LOWLIGHT;
	} else if (val_on & COB_SCREEN_LOWLIGHT) {
		val_off |= COB_SCREEN_HIGHLIGHT;
	}

	if (val_on) {
		output_line ("%s%d.attr |= 0x" CB_FMT_LLX ";", CB_PREFIX_SCR_FIELD, f->id, val_on);
	}
	if (val_off) {
		output_line ("%s%d.attr &= ~0x" CB_FMT_LLX ";", CB_PREFIX_SCR_FIELD, f->id, val_off);
	}
}

/* XML PARSE */


static void
output_xml_parse (struct cb_xml_parse *p)
{
	int flags = 0;
	if (cb_xml_parse_xmlss) {
		flags &= COB_XML_PARSE_XMLNSS;
	}
	if (p->returning_national && current_prog->xml_ntext) {
		flags &= COB_XML_PARSE_NATIONAL;
	}

	output_block_open ();
	output_line ("void *xml_state = NULL;");
	output_prefix ();
	output ("cob_set_int ("),
	output_param (CB_TREE (current_program->xml_code), 0);
	output (", 0);");
	output_newline ();

	output_line ("for (;;)");
	output_block_open ();

	/* actual XML parsing function and possible end */
	output_source_reference (CB_TREE (p), STMT_XML_PARSE);
	output_prefix ();
	output ("if (cob_xml_parse ("),
	output_param (p->data, 0);
	output (", ");
	output_param (p->encoding, 1);
	output (", ");
	output_param (p->validating, 2);
	output (", %d, &xml_state)) break;", flags);

	/* COBOL callback function -> PROCESSING PROCEDURE */
	/* note: automatic source reference */
	output_newline ();
	output_perform_once (CB_PERFORM (p->proc));

	output_block_close ();

	output_block_close ();
	output_newline ();
}

/* CANCEL */

static void
output_cancel (struct cb_cancel *p)
{
	struct nested_list	*nlp;
	char			*name_str;
	char			*s;
	unsigned int	i;

	if (is_literal_or_prototype_ref (p->target)) {
		s = get_program_id_str (p->target);
		name_str = cb_encode_program_id (s, 0, cb_fold_call);
		nlp = find_nested_prog_with_id (name_str);
		output_prefix ();
		if (nlp) {
			output ("(void)%s_%d_ (-1", name_str,
				nlp->nested_prog->toplev_count);
			for (i = 0; i < nlp->nested_prog->num_proc_params; ++i) {
				output (", NULL");
			}
			output (");");
		} else {
			output ("cob_cancel (");
			output_string ((const unsigned char *)s,
					(int)strlen (s), 0);
			output (");");
		}
		output_newline ();
		return;
	}
	output_prefix ();
	output ("cob_cancel_field (");
	output_param (p->target, -1);
	if (current_prog->nested_prog_list) {
		gen_nested_tab = 1;
		output (", cob_nest_tab");
	} else {
		output (", NULL");
	}
	output (");");
	output_newline ();
}

/* PERFORM */

static void
output_perform_call (struct cb_label *lb, struct cb_label *le)
{
	struct cb_para_label	*p;
	struct label_list	*l;
	const char *name = (const char *)lb->name;

	skip_line_num = 0;
	if (lb == current_prog->all_procedure || lb->flag_is_debug_sect) {
		output_line ("/* DEBUGGING Callback PERFORM %s */", name);
	} else if (le == NULL || lb == le) {
		if (current_statement && current_statement->statement == STMT_PERFORM) {
			output_line ("/* PERFORM %s */", name);
		} else {
			output_line ("/* USE PROCEDURE %s */", name);
		}
	} else {
		output_line ("/* PERFORM %s THRU %s */", name, (const char *)le->name);
	}

	/* Save current independent segment pointers */
	if (current_prog->flag_segments && last_section
	 && last_section->section_id != lb->section_id) {
		p = last_section->para_label;
		for (; p; p = p->next) {
			if (p->para->segment > 49 &&
			    p->para->flag_alter) {
				output_line ("save_label_%s%d = label_%s%d;",
					     CB_PREFIX_LABEL, p->para->id,
					     CB_PREFIX_LABEL, p->para->id);
			}
		}
	}
	/* Zap target independent labels */
	if (current_prog->flag_segments && last_segment != lb->segment) {
		if (lb->flag_section) {
			p = lb->para_label;
		} else if (lb->section) {
			p = lb->section->para_label;
		} else {
			p = NULL;
		}
		for (; p; p = p->next) {
			if (p->para->segment > 49 &&
			    p->para->flag_alter) {
				output_line ("label_%s%d = 0;",
					     CB_PREFIX_LABEL, p->para->id);
			}
		}
	}

	/* Update debugging name */
	if (current_prog->flag_gen_debug && lb->flag_real_label &&
	    (current_prog->all_procedure || lb->flag_debugging_mode)) {
		output_stmt (cb_build_debug (cb_debug_name, name, NULL));
	}

	skip_line_num = 0;
	output_line ("frame_ptr++;");
	if (cb_flag_stack_extended) {
		/* CHECKME: Is there a reference that provides the source position? */
		output_line ("frame_ptr->module_stmt = module->module_stmt;");
		output_line ("frame_ptr->section_name = module->section_name;");
		output_line ("frame_ptr->paragraph_name = module->paragraph_name;");
		output_line ("module->frame_ptr = frame_ptr;");
	}
	if (cb_flag_stack_check) {
		output_line ("if (frame_ptr == frame_overflow)");
		output_line ("\tcob_fatal_error (COB_FERROR_STACK);");
	}
	output_line ("frame_ptr->perform_through = %d;", le ? le->id : lb->id);
	if (!cb_flag_computed_goto) {
		l = cobc_parse_malloc (sizeof (struct label_list));
		l->next = label_cache;
		l->id = cb_id;
		if (label_cache == NULL) {
			l->call_num = 0;
		} else {
			l->call_num = label_cache->call_num + 1;
		}
		label_cache = l;
		output_line ("frame_ptr->return_address_num = %d;", l->call_num);
		output_line ("goto %s%d;", CB_PREFIX_LABEL, lb->id);
		output_line ("%s%d:", CB_PREFIX_LABEL, cb_id);
	} else {
		output_line ("frame_ptr->return_address_ptr = &&%s%d;",
			     CB_PREFIX_LABEL, cb_id);
		output_line ("goto %s%d;", CB_PREFIX_LABEL, lb->id);
		output_line ("%s%d:", CB_PREFIX_LABEL, cb_id);
	}
	output_line ("frame_ptr--;");
	if (cb_flag_stack_extended) {
		output_line ("module->module_stmt = module->frame_ptr->module_stmt;");
		output_line ("module->section_name = module->frame_ptr->section_name;");
		output_line ("module->paragraph_name = module->frame_ptr->paragraph_name;");
		output_line ("module->frame_ptr = frame_ptr;");
	}
	cb_id++;

	if (current_prog->flag_segments && last_section
	 && last_section->section_id != lb->section_id) {
		/* Restore current independent segment pointers */
		p = last_section->para_label;
		for (; p; p = p->next) {
			if (p->para->segment > 49 &&
			    p->para->flag_alter) {
				output_line ("label_%s%d = save_label_%s%d;",
					     CB_PREFIX_LABEL, p->para->id,
					     CB_PREFIX_LABEL, p->para->id);
			}
		}
		/* Zap target independent labels */
		if (lb->flag_section) {
			p = lb->para_label;
		} else if (lb->section) {
			p = lb->section->para_label;
		} else {
			p = NULL;
		}
		for (; p; p = p->next) {
			if (p->para->segment > 49 &&
			    p->para->flag_alter) {
				output_line ("label_%s%d = 0;",
					     CB_PREFIX_LABEL, p->para->id);
			}
		}
	}
}

static void
output_perform_exit (struct cb_label *l)
{
	if (l->flag_global) {
		output_newline ();
		output_line ("/* Implicit GLOBAL DECLARATIVE return */");
		output_line ("if (entry == %d) {", l->id);
		output_line ("  cob_module_leave (module);");
		if (cb_flag_stack_on_heap || current_prog->flag_recursive) {
			output_line ("  cob_free (frame_stack);");
			output_line ("  cob_free (cob_procedure_params);");
			output_line ("  cob_module_free (&module);");
		}
		output_line ("  return 0;");
		output_line ("}");
	}
	output_newline ();
	memset(last_line_num, ' ', 30);

	if (l->flag_declarative_exit) {
		output_line ("/* Implicit DECLARATIVE return */");
	} else if (l->flag_default_handler) {
		output_line ("/* Implicit Default Error Handler return */");
	} else {
		output_line ("/* Implicit PERFORM return */");
	}

	if (cb_perform_osvs && current_prog->prog_type == COB_MODULE_TYPE_PROGRAM) {
		output_line
		    ("for (temp_index = frame_ptr; temp_index->perform_through; temp_index--) {");
		output_line ("  if (temp_index->perform_through == %d) {", l->id);
		output_line ("    frame_ptr = temp_index;");
		if (!cb_flag_computed_goto) {
			output_line ("    goto P_switch;");
		} else {
			output_line ("    goto *frame_ptr->return_address_ptr;");
		}
		output_line ("  }");
		output_line ("}");
	} else {
		output_line ("if (frame_ptr->perform_through == %d)", l->id);
		if (!cb_flag_computed_goto) {
			output_line ("  goto P_switch;");
		} else {
			output_line ("  goto *frame_ptr->return_address_ptr;");
		}
	}

	if (l->flag_fatal_check) {
		output_newline ();
		output_line ("/* Fatal error if reached */");
		output_line ("cob_fatal_error (COB_FERROR_GLOBAL);");
	} else
	if (current_program->flag_report) {
		output_newline ();
		output_line ("/* Report Fatal error if reached */");
		output_line ("cob_fatal_error (COB_FERROR_GLOBAL);");
	}
}

static void
output_funcall_debug (cb_tree x)
{
	struct cb_funcall	*p;
	cb_tree			l;
	cb_tree			z;
	int			i;

	p = CB_FUNCALL (x);
	if (p->name[0] == '$') {
		z = p->argv[0];
		if (CB_REF_OR_FIELD_P (z) &&
		    cb_code_field (z)->flag_field_debug) {
			/* DEBUG */
			output_stmt (cb_build_debug (cb_debug_name,
				     (const char *)cb_code_field (z)->name, NULL));
			output_stmt (cb_build_debug (cb_debug_contents,
						     NULL, z));
			output_perform_call (cb_code_field (z)->debug_section, NULL);
		}
		z = p->argv[1];
		if (CB_REF_OR_FIELD_P (z) &&
		    cb_code_field (z)->flag_field_debug) {
			/* DEBUG */
			output_stmt (cb_build_debug (cb_debug_name,
				     (const char *)cb_code_field (z)->name, NULL));
			output_stmt (cb_build_debug (cb_debug_contents,
						     NULL, z));
			output_perform_call (cb_code_field (z)->debug_section, NULL);
		}
		return;
	}
	for (i = 0; i < p->argc; i++) {
		if (p->varcnt && i + 1 == p->argc) {
			for (l = p->argv[i]; l; l = CB_CHAIN (l)) {
				output_param (CB_VALUE (l), i);
				z = CB_VALUE (l);
				if (CB_REF_OR_FIELD_P (z) &&
				    cb_code_field (z)->flag_field_debug) {
					/* DEBUG */
					output_stmt (cb_build_debug (cb_debug_name,
						     (const char *)cb_code_field (z)->name, NULL));
					output_stmt (cb_build_debug (cb_debug_contents,
								     NULL, z));
					output_perform_call (cb_code_field (z)->debug_section, NULL);
				}
				i++;
			}
		} else {
			z = p->argv[i];
			if (CB_REF_OR_FIELD_P (z) &&
			    cb_code_field (z)->flag_field_debug) {
				/* DEBUG */
				output_stmt (cb_build_debug (cb_debug_name,
					     (const char *)cb_code_field (z)->name, NULL));
				output_stmt (cb_build_debug (cb_debug_contents,
							     NULL, z));
				output_perform_call (cb_code_field (z)->debug_section, NULL);
			}
		}
	}
}

static void
output_cond_debug (cb_tree x)
{
	struct cb_binary_op	*p;

	switch (CB_TREE_TAG (x)) {
	case CB_TAG_FUNCALL:
		output_funcall_debug (x);
		break;
	case CB_TAG_LIST:
		break;
	case CB_TAG_BINARY_OP:
		p = CB_BINARY_OP (x);
		switch (p->op) {
		case '!':
			output_cond_debug (p->x);
			break;

		case '&':
		case '|':
			output_cond_debug (p->x);
			output_cond_debug (p->y);
			break;

		case '=':
		case '<':
		case '[':
		case '>':
		case ']':
		case '~':
			output_cond_debug (p->x);
			break;

		default:
			if (CB_REF_OR_FIELD_P (x) &&
			    cb_code_field (x)->flag_field_debug) {
				output_stmt (cb_build_debug (cb_debug_name,
					     (const char *)cb_code_field (x)->name, NULL));
				output_stmt (cb_build_debug (cb_debug_contents,
							     NULL, x));
				output_perform_call (cb_code_field (x)->debug_section, NULL);
			}
			break;
		}
		break;
	default:
		break;
	}
}

static void
output_perform_once (struct cb_perform *p)
{
	if (p->body && CB_PAIR_P (p->body)) {
		output_perform_call (CB_LABEL (cb_ref (CB_PAIR_X (p->body))),
				     CB_LABEL (cb_ref (CB_PAIR_Y (p->body))));
	} else {
		output_stmt (p->body);
	}
	if (p->cycle_label) {
		output_stmt (cb_ref (p->cycle_label));
	}
}

static void
output_perform_until (struct cb_perform *p, cb_tree l)
{
	struct cb_perform_varying	*v;
	cb_tree				next;

	if (l == NULL) {
#if 0 /* FIXME: add back as option, because not conforming to ISO */
		if (CB_EXCEPTION_ENABLE (COB_EC_BOUND_SUBSCRIPT)) {
			cb_tree	xn;
			/* Check all INDEXED BY variables used in VARYING */
			for (xn = p->varying; xn; xn = CB_CHAIN (xn)) {
				v = CB_PERFORM_VARYING (CB_VALUE (xn));
				if (v->name
				 && CB_REF_OR_FIELD_P (v->name)) {
					struct cb_field	*f = CB_FIELD_PTR (v->name);
					if (f->flag_indexed_by
					 && f->index_qual) {
						f = f->index_qual;
						output_prefix ();
						output ("cob_check_subscript (");
						output_integer (v->name);
						output (", ");
						if (f->depending) {
							output_integer (f->depending);
							output (", \"%s\", 1", f->name);
						} else {
							output ("%d, \"%s\", 0", f->occurs_max, f->name);
						}
						output (");");
						output_newline ();
					}
				}
			}
		}
#endif
		/* Perform body at the end */
		output_perform_once (p);
		return;
	}

	v = CB_PERFORM_VARYING (CB_VALUE (l));
	next = CB_CHAIN (l);

	skip_line_num++;
	output_line ("for (;;)");
	output_block_open ();

	if (next && CB_PERFORM_VARYING (CB_VALUE (next))->name) {
		output_move (CB_PERFORM_VARYING (CB_VALUE (next))->from,
			     CB_PERFORM_VARYING (CB_VALUE (next))->name);
		/* DEBUG */
		if (current_prog->flag_gen_debug) {
			struct cb_field	*f = CB_FIELD (cb_ref (CB_PERFORM_VARYING (CB_VALUE (next))->name));
			if (f->flag_field_debug) {
				output_stmt (cb_build_debug (cb_debug_name,
					     (const char *)f->name, NULL));
				output_stmt (cb_build_debug (cb_debug_contents,
							     NULL, CB_PERFORM_VARYING (CB_VALUE (next))->name));
				output_perform_call (f->debug_section, NULL);
			}
		}

	}

	if (p->test == CB_AFTER) {
		output_perform_until (p, next);
	}

	/* DEBUG */
	if (current_prog->flag_gen_debug) {
		output_cond_debug (v->until);
	}

	/* Note: should always have a line reference, test code to be removed later */
#ifdef COB_TREE_DEBUG
	/* LCOV_EXCL_START */
	if (!v->until->source_line) {
		/* untranslated as unlinkely internal-check-only message */
		cobc_err_msg ("call to output_stmt -> TAG_IF without source reference");
		cobc_abort_terminate (1);
	}
	/* LCOV_EXCL_STOP */
#endif
	output_source_reference (v->until, STMT_UNTIL);

	output_prefix ();
	output ("if (");
	output_cond (v->until, 0);
	output (")");
	output_newline ();
	output_line ("  break;");

	if (p->test == CB_BEFORE) {
		output_perform_until (p, next);
	}

	if (v->step) {
		output_source_reference (v->step, STMT_VARYING);
		output_stmt (v->step);
	}

	output_block_close ();
}

static void
output_perform (struct cb_perform *p)
{
	struct cb_perform_varying	*v;
	struct cb_field			*f;

	switch (p->perform_type) {
	case CB_PERFORM_EXIT:
		if (CB_LABEL (p->data)->flag_return) {
			output_perform_exit (CB_LABEL (p->data));
		}
		break;
	case CB_PERFORM_ONCE:
		output_perform_once (p);
		break;
	case CB_PERFORM_TIMES:
		output_prefix ();
		output ("for (n%d = ", loop_counter);
		output_param (cb_build_cast_llint (p->data), 0);
		output ("; n%d > 0; n%d--)", loop_counter, loop_counter);
		output_newline ();
		loop_counter++;
		output_block_open ();
		last_line = -1; /* force statement reference output at begin of loop */
		output_perform_once (p);
		output_block_close ();
		break;
	case CB_PERFORM_UNTIL:
		v = CB_PERFORM_VARYING (CB_VALUE (p->varying));
		if (v->name) {
			output_move (v->from, v->name);
			/* DEBUG */
			if (current_prog->flag_gen_debug) {
				f = CB_FIELD (cb_ref (v->name));
				if (f->flag_field_debug) {
					output_stmt (cb_build_debug (cb_debug_name,
						     (const char *)f->name, NULL));
					output_stmt (cb_build_debug (cb_debug_contents,
								     NULL, v->name));
					output_perform_call (f->debug_section, NULL);
				}
			}

		}
		output_perform_until (p, p->varying);
		break;
	case CB_PERFORM_FOREVER:
		output_line ("for (;;)");
		output_block_open ();
		last_line = -1; /* force statement reference output at begin of loop */
		output_perform_once (p);
		output_block_close ();
		break;
	default:
		break;
	}
	if (p->exit_label) {
		output_stmt (cb_ref (p->exit_label));
	}
}

static void
output_debug_item (const struct cb_debug *dbg)
{
	const size_t	size = cb_code_field (dbg->target)->size;
	const size_t	copy_size = dbg->size > size ? size : dbg->size;
	if (!dbg->value) {
		/* content of variable */
		struct cb_field *f = CB_FIELD_PTR (dbg->fld);
		/* address may change so we may have NULL or invalid pointer */
		if (f->flag_item_based || f->storage == CB_STORAGE_LINKAGE) {
#if 0		/* FIXME: this should be replaced in 4.x by a call to libcob
			   which checks for NULL, and for invalid access via handler,
			   then outputs the appropriate value */
			struct cb_field * ff = real_field_founder (f);
			output_prefix ();
			output ("cob_set_verified_data (");
			output_data (dbg->target);
			output (", ");
			output_data (CB_TREE(ff));
			output (", " CB_FMT_LLU ", %u); ", f->offset, size);
			output_newline ();
#else

			const char *null_rep = "<NULL>";
			f = real_field_founder (f);
			/* in this case - pre-fill with space, then set var / null_rep */
			output_prefix ();
			output ("memset (");
			output_data (dbg->target);
			output (", ' ', %u);", (unsigned int)size);
			output_newline ();
			output_prefix ();
			output ("if (");
			output_data (CB_TREE (f));
			output (" == NULL)");
			output_newline ();
			output_prefix ();
			output ("\t""memcpy (");
			output_data (dbg->target);
			output (", %s%d", CB_PREFIX_STRING, lookup_string (null_rep));
			output (", %u);", (unsigned int)strlen (null_rep));
			output_newline ();
			output_line ("else");
			output_prefix ();
			output ("\t""memcpy (");
			output_data (dbg->target);
			output (", ");
			output_data (dbg->fld);
			output (", %u);", (unsigned int)copy_size);
			output_newline ();
#endif
		} else {
			/* normal field without changing address, copy data up to max*/
			output ("memcpy (");
			output_data (dbg->target);
			output (", ");
			output_data (dbg->fld);
			output (", %u);", (unsigned int)copy_size);
			output_newline ();
			/* ... filled up with space */
			if (copy_size != size) {
				output_prefix ();
				output ("memset (");
				output_data (dbg->target);
				output (" + %u, ' ', %u);",
					(unsigned int)dbg->size, (unsigned int)(size - dbg->size));
				output_newline ();
			}
		}
		return;
	}

	/* pre-defined string */
	output_prefix ();
	output ("memcpy (");
	output_data (dbg->target);
	output (", ");
	output ("%s%d", CB_PREFIX_STRING, lookup_string (dbg->value));
	output (", %u);", (unsigned int)copy_size);
	output_newline ();
	/* ... filled up with space */
	if (copy_size != size) {
		output_prefix ();
		output ("memset (");
		output_data (dbg->target);
		output (" + %u, ' ', %u);",
			(unsigned int)dbg->size, (unsigned int)(size - dbg->size));
		output_newline ();
	}
}

static void
output_file_error (struct cb_file *pfile)
{
	struct cb_file		*fl;
	cb_tree			l;

	if (current_prog->flag_gen_debug) {
		output_stmt (cb_build_debug (cb_debug_contents,
					     "USE PROCEDURE", NULL));
	}
	for (l = current_prog->local_file_list; l; l = CB_CHAIN (l)) {
		fl = CB_FILE (CB_VALUE (l));
		if (!strcmp (pfile->name, fl->name)) {
			output_perform_call (fl->handler, NULL);
			return;
		}
	}
	for (l = current_prog->global_file_list; l; l = CB_CHAIN (l)) {
		fl = CB_FILE (CB_VALUE (l));
		if (!strcmp (pfile->name, fl->name)) {
			if (fl->handler_prog == current_prog) {
				output_perform_call (fl->handler, NULL);
			} else {
				if (fl->handler_prog->nested_level) {
					output_line ("%s_%d_ (%d);",
						fl->handler_prog->program_id,
						fl->handler_prog->toplev_count,
						fl->handler->id);
				} else {
					output_line ("%s_ (%d);",
						fl->handler_prog->program_id,
						fl->handler->id);
				}
			}
			return;
		}
	}
	output_perform_call (pfile->handler, NULL);
}

/* GO TO */

static void
output_goto_1 (cb_tree x)
{
	struct cb_label		*lb = CB_LABEL (x);

	if (current_prog->flag_segments && last_segment != lb->segment) {
		/* Zap independent labels */
		struct cb_para_label	*p;
		if (lb->flag_section) {
			p = lb->para_label;
		} else if (lb->section) {
			p = lb->section->para_label;
		} else {
			p = NULL;
		}
		for (; p; p = p->next) {
			if (p->para->segment > 49
			 && p->para->flag_alter) {
				output_line ("label_%s%d = 0;",
					     CB_PREFIX_LABEL, p->para->id);
			}
		}
	}

	/* Check for debugging on procedure */
	if (current_prog->flag_gen_debug && lb->flag_real_label
	  && (current_prog->all_procedure || lb->flag_debugging_mode)) {
		output_stmt (cb_build_debug (cb_debug_name,
					     (const char *)lb->name, NULL));
		output_move (cb_space, cb_debug_contents);
	}

	output_line ("goto %s%d;", CB_PREFIX_LABEL, lb->id);
}

/* extension in test, may be removed again */
static void
output_goto_entry (cb_tree target)
{
	cb_tree l;
	struct cb_reference	*r = CB_REFERENCE (target);
	const char *name = r->word->name;
	for (l = current_program->entry_list_goto; l; l = CB_CHAIN (l)) {
		struct cb_label *label = CB_LABEL (CB_VALUE (l));
		if (strcmp (name, label->name) == 0) {
			output_line ("goto %s%d;", CB_PREFIX_LABEL, label->id);
			return;
		}
	}
	cb_error_x (target, _("No ENTRY FOR GO TO '%s'"), name);
}


static void
output_goto (struct cb_goto *p)
{
	cb_tree		l;
	struct cb_field	*f;
	int		i;

	if (cb_flag_prof) {
		/* Output this only if we are exiting the paragraph... */
		if ( !(p->flags & CB_GOTO_FLAG_SAME_PARAGRAPH) ){
			output_line ("cob_prof_goto (prof_info);");
		}
	}

	i = 1;
	if (p->depending) {
		/* Check for debugging on the DEPENDING item */
		if (current_prog->flag_gen_debug) {
			f = CB_FIELD (cb_ref (p->depending));
			if (f->flag_all_debug) {
				output_stmt (cb_build_debug (cb_debug_name,
					     (const char *)f->name, NULL));
				output_stmt (cb_build_debug (cb_debug_contents,
							     NULL, p->depending));
				output_perform_call (f->debug_section, NULL);
			}
		}
		output_prefix ();
		output ("switch (");
		output_param (cb_build_cast_int (p->depending), 0);
		output (")");
		output_newline ();
		output_block_open ();
		for (l = p->target; l; l = CB_CHAIN (l)) {
			cb_tree target = CB_VALUE (l);
			cb_tree ref = cb_try_ref (target);
			output_indent_level -= indent_adjust_level;
			output_line ("case %d:", i++);
			output_indent_level += indent_adjust_level;
			if (ref != cb_error_node) {
				output_goto_1 (ref);
			} else {
				output_goto_entry (target);
			}
		}
		output_block_close ();
	} else if (p->target == NULL
	        || p->target == cb_int1) {
		needs_exit_prog = 1;
		/* EXIT FUNCTION */
		if (current_prog->prog_type == COB_MODULE_TYPE_FUNCTION) {
			output_line ("goto exit_function;");
		/* GOBACK (target = cb_int1), possibly implied */
		} else if (p->target == cb_int1
		        || cb_flag_implicit_init || current_prog->nested_level) {
			output_line ("goto exit_program;");
		/* EXIT PROGRAM */
		} else {
			/* Ignore if not a callee */
			output_line ("if (module->next)");
			output_line ("  goto exit_program;");
		}
	} else {
		cb_tree target = p->target;
		cb_tree ref = cb_try_ref (target);
		if (ref != cb_error_node) {
			output_goto_1 (ref);
		} else {
			output_goto_entry (target);
		}
	}
}

/* ALTER */

static void
output_alter (struct cb_alter *p)
{
	struct cb_label	*l1;
	struct cb_label	*l2;

	l1 = CB_LABEL (CB_REFERENCE(p->source)->value);
	l2 = CB_LABEL (CB_REFERENCE(p->target)->value);
	output_line ("label_%s%d = %d;", CB_PREFIX_LABEL, l1->id, l2->id);

	/* Check for debugging on procedure name */
	if (current_prog->flag_gen_debug && l1->flag_real_label &&
	    (current_prog->all_procedure || l1->flag_debugging_mode)) {
		output_stmt (cb_build_debug (cb_debug_name,
					     (const char *)l1->name, NULL));
		output_stmt (cb_build_debug (cb_debug_contents,
					     (const char *)l2->name, NULL));
		if (current_prog->all_procedure) {
			output_perform_call (current_prog->all_procedure, NULL);
		} else if (l1->flag_debugging_mode) {
			output_perform_call (l1->debug_section, NULL);
		}
	}
}

/* conditions IF / WHEN / PRSENT-WHEN */

static void
output_if (const struct cb_if *ip)
{
	int			skip_else;
	if (ip->stmt1 == NULL
	 && ip->stmt2 == NULL) {
		if (ip->statement != STMT_IF) {
			output_line ("/* WHEN has code omitted */");
		} else {
			output_line ("/* IF has code omitted */");
		}
		return;
	}

	if (ip->statement != STMT_IF) {
		output_newline ();
		if (ip->test == cb_true
		 && cb_flag_remove_unreachable) {
			output_line ("/* WHEN is always TRUE */");
		} else if (ip->test == cb_false
			&& cb_flag_remove_unreachable) {
			output_line ("/* WHEN is always FALSE */");
		} else
		if (CB_TREE_TAG (ip->test) == CB_TAG_BINARY_OP) {
			const struct cb_binary_op	*bop = CB_BINARY_OP (ip->test);
			cb_tree		w = NULL;
			if (bop->op == '!') {
				w = bop->x;
			} else if (bop->y) {
				w = bop->y;
			} else if (bop->x) {
				w = bop->x;
			}
			if (w == cb_true) {
				output_line ("/* WHEN is always %s */",
					bop->op == '!' ? "FALSE" : "TRUE");
			} else if (w == cb_false) {
				output_line ("/* WHEN is always %s */",
					bop->op != '!' ? "FALSE" : "TRUE");
			} else {
				w = ip->test;
				/* LCOV_EXCL_START */
				if (!ip->test->source_line) {
					/* untranslated as unlikely internal-check-only message */
					cobc_err_msg ("Unexpected call to output_stmt -> TAG_IF (BINARY) without source reference");
					output_line ("/* WHEN */");
				/* LCOV_EXCL_STOP */
				} else {
					output_source_reference (w, STMT_WHEN);
				}
			}
		} else if (ip->test->source_line) {
			output_line ("/* Line: %-10d: WHEN */", ip->test->source_line);
			if (last_line != ip->test->source_line
			 || last_stmt != STMT_WHEN) {
				/* Output source location as code */
				output_line_and_trace_info (ip->test, STMT_WHEN);
				last_stmt = STMT_WHEN;
			}
		/* LCOV_EXCL_START TODO - REMOVE when verified that we never reach this */
		} else {
			output_line ("/* WHEN */");
		/* LCOV_EXCL_STOP */
		}
		output_newline ();
	}

	/* Really PRESENT WHEN for Report field/line */
	if (ip->statement == STMT_PRESENT_WHEN
	 && ip->stmt1 == NULL
	 && ip->stmt2 != NULL) {
		struct cb_field *p2 = (struct cb_field *)ip->stmt2;
		const char *target;
		char	fldname[64];
		if (p2->report_flag & COB_REPORT_LINE) {
			sprintf (fldname, "%s%d",CB_PREFIX_REPORT_LINE,p2->id);
			target = "Line";
		} else {
			target = "Field";
			if (p2->report_field_name == NULL) {
				sprintf (fldname,"%s%d",CB_PREFIX_REPORT_FIELD,++report_field_id);
				p2->report_field_name = cobc_parse_strdup (fldname);
			} else {
				strcpy (fldname, p2->report_field_name);
			}
		}
		output_line ("/* PRESENT WHEN %s: %d */", target, p2->common.source_line);
		output_prefix ();
		output ("if (");
		output_cond (ip->test, 0);
		output (")");
		output_newline ();
		output_line ("{");
		output_line ("\t%s.suppress = 0;", fldname);
		output_line ("} else {");
		output_line ("\t%s.suppress = 1;", fldname);
		output_line ("}");
		return;
	}

	if (ip->test == cb_false
	 && ip->stmt1 == NULL
	 && cb_flag_remove_unreachable) {
		output_line (" /* FALSE condition and code omitted */");
		skip_else = 1;
	} else {
		skip_else = 0;
		output_prefix ();
		output ("if (");
		output_cond (ip->test, 0);
		output (")");
		output_newline ();
		output_block_open ();
		if (ip->stmt1) {
			output_stmt (ip->stmt1);
		} else {
			output_line ("; /* Nothing */");
		}
		output_block_close ();
	}

	if (ip->stmt2) {
		if (!skip_else) {
			output_line ("else");
		}
		output_line ("{");
		output_indent_level += indent_adjust_level;
		if (ip->statement == STMT_IF) {
			output_line ("/* ELSE */");
		} else {
			output_line ("/* WHEN */");
		}
		output_stmt (ip->stmt2);
		output_indent_level -= indent_adjust_level;
		output_line ("}");
	}
}

/* JSON/XML GENERATE suppress checks */

static void
output_ml_tree_suppress_cond (struct cb_ml_generate_tree *tree)
{
	output_prefix ();
	if (tree->type == CB_ML_ATTRIBUTE) {
		output ("%s%d.is_suppressed = ", CB_PREFIX_ML_ATTR, tree->id);
	} else {
		output ("%s%d.is_suppressed = ", CB_PREFIX_ML_TREE, tree->id);
	}
	output_cond (tree->suppress_cond, 0);
	output (";");
	output_newline ();
}

static int
one_tree_in_list_is_never_suppressed (struct cb_ml_generate_tree *tree)
{
	for (; tree; tree = tree->sibling) {
		if (!tree->suppress_cond) {
			return 1;
		}
	}

	return 0;
}

static int
one_child_or_attr_is_never_suppressed (struct cb_ml_generate_tree *tree)
{
	return one_tree_in_list_is_never_suppressed (tree->attrs)
		|| one_tree_in_list_is_never_suppressed (tree->children);
}

static void
output_all_tree_list_suppressed_cond (struct cb_ml_generate_tree *tree,
				      const char *prefix,
				      int * const cond_emitted)
{
	for (; tree; tree = tree->sibling) {
		if (*cond_emitted) {
			output (" && ");
		} else {
			*cond_emitted = 1;
		}
		output ("%s%d.is_suppressed", prefix, tree->id);
	}

}

static void
output_parent_tree_suppress_check (struct cb_ml_generate_tree *tree)
{
	int	child_check_emitted = 0;

	if (one_child_or_attr_is_never_suppressed (tree)) {
		/* In this case, tree is always emitted; no check is needed. */
		return;
	}

	output_prefix ();
	output ("%s%d.is_suppressed |= ", CB_PREFIX_ML_TREE, tree->id);
	output_all_tree_list_suppressed_cond (tree->attrs, CB_PREFIX_ML_ATTR,
					      &child_check_emitted);
	output_all_tree_list_suppressed_cond (tree->children, CB_PREFIX_ML_TREE,
					      &child_check_emitted);
	output (";");
	output_newline ();

}

static void
output_ml_suppress_checks (struct cb_ml_suppress_checks * const suppress_checks)
{
	struct cb_ml_generate_tree	*orig_tree = suppress_checks->tree;
	struct cb_ml_generate_tree	*tree;

	/*
	  To resolve dependency problems, start from last child of last element.
	*/
	if (orig_tree->children) {
		tree = get_last_child (orig_tree);
	} else if (orig_tree->attrs) {
		tree = get_last_attr (orig_tree);
	} else {
		tree = orig_tree;
	}

	for (;;) {
		if (tree->suppress_cond) {
			output_ml_tree_suppress_cond (tree);
		}
		/*
		  Suppress the (non-root) element if all its children are
		  suppressed.
		*/
		if ((tree->children || tree->attrs) && tree != orig_tree) {
			output_parent_tree_suppress_check (tree);
		}

		if (tree == orig_tree) {
			break;
		} else {
			tree = get_prev_ml_tree_entry (tree);
		}
	}
}

/* Output statement */

static int
get_ec_code_for_handler (const enum cb_handler_type handler_type)
{
	switch (handler_type) {
	case AT_END_HANDLER:
		return CB_EXCEPTION_CODE (COB_EC_I_O_AT_END);
	case EOP_HANDLER:
		/* FIXME: handler should actually also check for COB_EC_I_O_EOP_OVERFLOW */
		return CB_EXCEPTION_CODE (COB_EC_I_O_EOP);
	case INVALID_KEY_HANDLER:
		return CB_EXCEPTION_CODE (COB_EC_I_O_INVALID_KEY);
	/* LCOV_EXCL_START */
	default:
		cobc_err_msg (_("unexpected handler type: %d"), (int) handler_type);
		COBC_ABORT ();
	/* LCOV_EXCL_STOP */
	}
}

static void
output_ferror_stmt (const struct cb_statement *stmt)
{
	output_line ("if (cob_glob_ptr->cob_exception_code != 0)");
	output_block_open ();
	if (stmt->ex_handler) {
		output_line ("if (cob_glob_ptr->cob_exception_code == 0x%04x)",
			     get_ec_code_for_handler (stmt->handler_type));
		output_block_open ();
		output_stmt (stmt->ex_handler);
		output_block_close ();
		output_line ("else");
		output_block_open ();
	}
	output_file_error (CB_FILE (stmt->file));
	output_block_close ();
	if (stmt->ex_handler) {
		output_block_close ();
	}
	if (stmt->not_ex_handler || stmt->handler3) {
		output_line ("else");
		output_block_open ();
		if (stmt->handler3) {
			output_stmt (stmt->handler3);
		}
		if (stmt->not_ex_handler) {
			output_stmt (stmt->not_ex_handler);
		}
		output_block_close ();
	}
}

static void
output_module_source_for_tree (cb_tree x)
{
	if (!x->source_file) {
		return;
	}
	if (last_line != x->source_line) {
		output_line ("module->module_stmt = 0x%08X;",
			COB_SET_LINE_FILE (x->source_line,
				lookup_source (x->source_file)));
		last_line = x->source_line;
	}
}

static void
output_section_info (const struct cb_label *lp)
{
	if (CB_TREE (lp) == cb_standard_error_handler) {
		return;
	}
	if (lp->flag_dummy_exit) {
		return;
	}

	if (cb_flag_source_location || cb_flag_trace) {
		output_module_source_for_tree (CB_TREE (lp));
	}

	if (lp->flag_entry) {
		if (cb_flag_trace) {
			output_line ("cob_trace_entry (%s%d);",
					 CB_PREFIX_STRING, lookup_string (lp->orig_name));
		}
		return;
	}

	if (lp->flag_section) {
		if (!lp->flag_dummy_section) {
			const int str_num = lookup_string (lp->orig_name);
			output_line ("module->section_name = %s%d;", CB_PREFIX_STRING, str_num);
			output_line ("module->paragraph_name = NULL;");
			if (cb_flag_trace) {
				output_line ("cob_trace_sect (%s%d);", CB_PREFIX_STRING, str_num);
			}
		} else {
			output_line ("module->section_name = NULL;");
			output_line ("module->paragraph_name = NULL;");
			if (cb_flag_trace) {
				output_line ("cob_trace_sect (NULL);");
			}
		}
		return;
	}

	if (!lp->flag_dummy_paragraph) {
		const int str_num = lookup_string (lp->orig_name);
		output_line ("module->paragraph_name = %s%d;", CB_PREFIX_STRING, str_num);
		if (cb_flag_trace) {
			output_line ("cob_trace_para (%s%d);", CB_PREFIX_STRING, str_num);
		}
	} else {
		if (cb_flag_trace) {
			output_line ("cob_trace_para (NULL);");
		}
	}
}

static void
output_line_and_trace_info (cb_tree x, const enum cob_statement stmnt)
{
	const char *stmnt_enum = cb_statement_enum_name[stmnt];

	if (  (cb_flag_c_line_directives
		|| cb_flag_source_location
		|| cb_cob_line_num)
	 && x->source_line) {
		output_cobol_info (x);
		if (cb_flag_source_location) {
			output_line ("module->statement = %s;", stmnt_enum);
		}
		if (cb_flag_source_location
		 && cb_flag_trace) {
			output_line ("if ((module->flag_debug_trace & COB_MODULE_READYTRACE))");
			output_line ("   cob_trace_statement (%s);", stmnt_enum);
		} else if (cb_flag_c_line_directives) {
			output_line ("cob_nop ();");
			output_c_info ();
		}
	} else {
		if (cb_flag_source_location ) {
			output_line ("module->statement = %s;", stmnt_enum);
		}
		if (cb_flag_source_location
		 && cb_flag_trace) {
			output_line ("if ((module->flag_debug_trace & COB_MODULE_READYTRACE))");
			output_line ("   cob_trace_statement (%s);", stmnt_enum);
		}
	}
}

static void
output_source_reference (cb_tree tree, const enum cob_statement statement)
{
	/* Output source location as comment */
	output_line ("/* Line: %-10d: %-19s: %s */",
			tree->source_line, cb_statement_name[statement],
			tree->source_file);

	if (statement == STMT_UNTIL) {
		last_line = -1;	/* force generation of source location */
	}
	/* Output source location as code */
	if (cb_flag_source_location) {
		if (last_line != tree->source_line) {
			output_line ("module->module_stmt = 0x%08X;",
				COB_SET_LINE_FILE (tree->source_line, lookup_source (tree->source_file)));
		}
	}
	if (last_line != tree->source_line
	 || last_stmt != statement) {
		/* Output source location as code */
		output_line_and_trace_info (tree, statement);
		last_line = tree->source_line;
		last_stmt = statement;
	}
}

static void
output_label_info (cb_tree x, const struct cb_label *lp)
{
	if (lp->flag_dummy_section || lp->flag_dummy_paragraph) {
		return;
	}

	output_newline ();

	if (lp->flag_dummy_exit) {
		output_line ("/* Implicit EXIT label */");
		return;
	} else if (lp->flag_next_sentence) {
		output_line ("/* Implicit NEXT SENTENCE label */");
		return;
	}

	output_prefix ();
	if (x->source_file) {
		output ("/* Line: %-10d: ", x->source_line);
	} else {
		output ("/* ");
	}
	if (lp->flag_section) {
		output ("Section   %-24s", (const char *)lp->name);
		excp_current_section = (const char *)lp->name;
		excp_current_paragraph = NULL;
	} else if (lp->flag_entry) {
		output ("Entry     %-24s", lp->orig_name);
		excp_current_section = NULL;
		excp_current_paragraph = NULL;
	} else {
		output ("Paragraph %-24s", (const char *)lp->name);
		excp_current_paragraph = (const char *)lp->name;
	}
	if (x->source_file) {
		output (": %s */", x->source_file);
	} else {
		output ("*/");
	}
	skip_line_num = 2;
	output_newline ();
}

static void
output_alter_check (const struct cb_label *lp)
{
	struct cb_alter_id	*aid;

	output_local ("static int\tlabel_%s%d = 0;\n",
		     CB_PREFIX_LABEL, lp->id);
	if (current_prog->flag_segments) {
		output_local ("static int\tsave_label_%s%d = 0;\n",
			     CB_PREFIX_LABEL, lp->id);
	}
	output_newline ();
	output_line ("/* ALTER processing */");
	output_line ("switch (label_%s%d)",
		     CB_PREFIX_LABEL, lp->id);
	output_block_open ();
	for (aid = lp->alter_gotos; aid; aid = aid->next) {
		output_line ("case %d:", aid->goto_id);
		output_line ("goto %s%d;", CB_PREFIX_LABEL, aid->goto_id);
	}
	output_block_close ();
	output_newline ();
}

static void
output_level_2_ex_condition (const int level_2_ec)
{
	output_line ("if ( (cob_glob_ptr->cob_exception_code & 0xff00) == 0x%04x)",
		     CB_EXCEPTION_CODE (level_2_ec));
}

static void
output_display_accept_ex_condition (const enum cb_handler_type handler_type)
{
	int	imp_ec;

	output_line ("if ((cob_glob_ptr->cob_exception_code & 0xff00) == 0x%04x",
		     CB_EXCEPTION_CODE (COB_EC_SCREEN));

	if (handler_type == DISPLAY_HANDLER) {
		imp_ec = COB_EC_IMP_DISPLAY;
	} else { /* ACCEPT_HANDLER */
		imp_ec = COB_EC_IMP_ACCEPT;
	}
	output_line ("     || cob_glob_ptr->cob_exception_code == 0x%04x)",
		     CB_EXCEPTION_CODE (imp_ec));
}

static void
output_ec_condition_for_handler (const enum cb_handler_type handler_type)
{

	switch (handler_type) {
	case DISPLAY_HANDLER:
		output_display_accept_ex_condition (DISPLAY_HANDLER);
		break;

	case ACCEPT_HANDLER:
		output_display_accept_ex_condition (ACCEPT_HANDLER);
		break;

	case SIZE_ERROR_HANDLER:
		output_level_2_ex_condition (COB_EC_SIZE);
		break;

	case OVERFLOW_HANDLER:
		output_level_2_ex_condition (COB_EC_OVERFLOW);
		break;

	case XML_HANDLER:
		output_level_2_ex_condition (COB_EC_XML);
		break;

	case JSON_HANDLER:
		output_level_2_ex_condition (COB_EC_JSON);
		break;

	case DELETE_FILE_HANDLER:
		output_level_2_ex_condition (COB_EC_DELETE_FILE);
		break;

	/* LCOV_EXCL_START */
	default:
		cobc_err_msg (_("unexpected handler type: %d"), (int) handler_type);
		COBC_ABORT ();
	/* LCOV_EXCL_STOP */
	}
}

static void
output_handler (const struct cb_statement *stmt)
{
	if (stmt->file) {
		output_ferror_stmt (stmt);
		return;
	}

	if (stmt->ex_handler) {
		output_ec_condition_for_handler (stmt->handler_type);
		output_block_open ();
		output_stmt (stmt->ex_handler);
		output_block_close ();
		if (stmt->not_ex_handler) {
			output_line ("else");
		}
	}
	if (stmt->not_ex_handler) {
		if (stmt->ex_handler == NULL) {
			output_line ("if (!cob_glob_ptr->cob_exception_code)");
		}
		output_block_open ();
		output_stmt (stmt->not_ex_handler);
		output_block_close ();
	}
}


/*
 * For OPEN and file has SELECT fields which are LINKAGE or BASED
 * set the current address of the field at OPEN time
 */
static void
output_file_variable (cb_tree x, const struct cb_file *fl,
	const struct cb_funcall *c, const char *set_field, int always)
{
	struct cb_field		*f;

	if (x == NULL || !CB_REF_OR_FIELD_P (x)) {
		return;
	}
	f = cb_code_field(x);
	if (!f) {
		return;
	}
	/* Check: do we need to set the fields address at OPEN time? */
	if (f->flag_local
	 || f->flag_item_based
	 || f->flag_local_storage
	 || f->storage == CB_STORAGE_LINKAGE
	 || f->storage == CB_STORAGE_LOCAL) {
		/* result: yes */
	} else {
		return;
	}

	if (strcmp (c->name, "cob_open") == 0
	 || strcmp (c->name, "cob_extfh_open") == 0
	 || always) {
		output_prefix ();
		output ("%s%s->%s = ", CB_PREFIX_FILE, fl->cname, set_field);
		output_param (x, -1);
		output (";");
		output_newline ();
	}
}

static void
force_cache (struct cb_field * const f)
{
	FILE	*savetarget = output_target;

	output_target = NULL;
	output_param (cb_build_field_reference (f, NULL), 0);
	output_target = savetarget;
}

static void
output_debug_stmts (cb_tree debug_checks)
{
	output_line ("if (cob_glob_ptr->cob_debugging_mode)");
	output_block_open ();
	output_stmt (debug_checks);
	output_block_close ();
}

static void
output_label_as_c (const struct cb_label *lp)
{
	unsigned char buff[COB_MINI_BUFF];
	unsigned char *ptr = (unsigned char *)&buff;
	cob_encode_program_id ((unsigned char*)lp->orig_name, ptr,
		COB_MINI_MAX, COB_FOLD_UPPER);
	if (*ptr == '_') ptr++;
	if (lp->flag_section) {
		/* SECTION label */
		output_line ("SECTION_%s:\t%s;", ptr, "cob_nop ()");
	} else if (lp->flag_entry_for_goto) {
		/* ENTRY FOR GOTO label */
		if (cb_flag_source_location) {
			const char *stmnt_enum
				= cb_statement_enum_name[STMT_ENTRY_FOR_GO_TO];
			output_line ("ENTRY_GOTO_%s:\tmodule->statement = %s;",
				ptr, stmnt_enum);
		} else {
			output_line ("ENTRY_GOTO_%s:\t%s;", ptr, "cob_nop ()");
		}
	} else if (lp->flag_entry) {
		/* ENTRY label */
		if (cb_flag_source_location) {
			const char *stmnt_enum
				= cb_statement_enum_name[STMT_ENTRY];
			output_line ("ENTRY_%s:\tmodule->statement = %s;",
				ptr, stmnt_enum);
		} else {
			output_line ("ENTRY_%s:\t%s;", ptr, "cob_nop ()");
		}
	} else {
		/* Paragraph label */
		/* note: paragraphs need a suffix, both to not break some macro
		   names, and most important to prevent duplicates:
		   COBOL allows multiple pagraphs with the same name, even in the
		   same section; C allows only one per function and with our current
		   generation that means one identical generated paragraph
		   name "per program" */
		if (cb_flag_source_location) {
			const char *stmnt_enum
				= cb_statement_enum_name[STMT_ENTRY];
			output_line ("PARAGRAPH_%s_l_%d:\tmodule->statement = %s;",
				ptr, lp->id, stmnt_enum);
		} else {
			output_line ("PARAGRAPH_%s_l_%d:\t%s;", ptr, lp->id, "cob_nop ()");
		}
	}
}

static void
output_label (const struct cb_label *lp)
{
	if (lp->flag_skip_label) {
		return;
	}
	if (cb_flag_section_exit_check
	 && lp->flag_section
	 && !lp->flag_dummy_section) {
		if (last_section
		 && last_section->flag_declaratives
		 && !lp->flag_declaratives) {
			last_section = NULL;
		}
		if (last_section != NULL) {
			output_line ("cob_check_beyond_exit (%s%d);"
				"\t/* prevent fall-through */", CB_PREFIX_STRING,
				lookup_string (last_section->name));
		}
	}
	output_label_info (CB_TREE(lp), lp);
	if (lp->flag_section) {
		struct cb_para_label	*pal;
		for (pal = lp->para_label; pal; pal = pal->next) {
			if (pal->para->segment > 49
			 && pal->para->flag_alter) {
				output_line ("label_%s%d = 0;",
					CB_PREFIX_LABEL, pal->para->id);
			}
		}
		last_segment = lp->segment;
		last_section = lp;
	}
	if (lp->flag_begin) {
		output_line ("%s%d:;", CB_PREFIX_LABEL, lp->id);
	}
	if (!lp->flag_dummy_exit
	 && !lp->flag_dummy_section
	 && !lp->flag_dummy_paragraph
	 && !lp->flag_default_handler) {
		if (cb_flag_c_line_directives) {
			output_cobol_info (CB_TREE(lp));
		}
		if (cb_flag_c_labels) {
			output_label_as_c (lp);
			if (cb_flag_c_line_directives) {
				output_c_info ();
			}
		} else {
			if (cb_flag_c_line_directives) {
				output_line ("cob_nop ();");
				output_c_info ();
			}
		}
	}

	/* Check for runtime debug flag */
	if (current_prog->flag_debugging && lp->flag_is_debug_sect) {
		output_line ("if (!cob_glob_ptr->cob_debugging_mode)");
		output_line ("\tgoto %s%d;",
			CB_PREFIX_LABEL, CB_LABEL (lp->exit_label)->id);
	}

	if (cb_flag_source_location
	 || cb_flag_trace) {
		output_section_info (lp);
	}
	last_line = -1;	/* force generation of source location */

	/* Check procedure debugging */
	if (current_prog->flag_gen_debug && lp->flag_real_label) {
		output_stmt (cb_build_debug (cb_debug_name,
			(const char*)lp->name, NULL));
		if (current_prog->all_procedure) {
			output_perform_call (current_prog->all_procedure, NULL);
		} else if (lp->flag_debugging_mode) {
			output_perform_call (lp->debug_section, NULL);
		}
	}

	/* Check ALTER processing */
	if (lp->flag_alter) {
		output_alter_check (lp);
	}
}

static void
output_assign (const struct cb_assign *ap)
{
	struct cb_field		*f1, *f2;
	if (CB_TREE_CLASS (ap->var) == CB_CLASS_NUMERIC
	 || CB_TREE_CLASS (ap->var) == CB_CLASS_ALPHANUMERIC
	 || CB_TREE_CLASS (ap->var) == CB_CLASS_ALPHABETIC) {
		f1 = cb_code_field(ap->var);
		if (!f1->flag_real_binary
		 && !f1->flag_binary_assign
		 && !(f1->usage == CB_USAGE_COMP_X && f1->size == 1)
		 && f1->pic
		 && f1->usage != CB_USAGE_LENGTH) {
			output_prefix ();
		 	if (f1->usage == CB_USAGE_COMP_X) {
				output ("cob_set_compx (");
				output_param (ap->var, 0);
				output (", (cob_s64_t)");
				output_integer (ap->val);
				output (");\n");
				return;
			} else
			if (CB_NUMERIC_LITERAL_P (ap->val)
			 && CB_LITERAL (ap->val)->scale == 0
			 && cb_get_long_long (ap->val) < cob_exp10_ll[f1->pic->digits]) {
				output ("cob_set_llcon (");
				output_param (ap->var, 0);
			} else {
				output ("cob_set_llint (");
				output_param (ap->var, 0);
				output (", ");
				output (CB_FMT_LLD_F, cob_exp10_ll[f1->pic->digits]);
			}
			output (", (cob_s64_t)");
			output_integer (ap->val);
			output (");\n");
			return;
		}
	}
#ifdef	COB_NON_ALIGNED		/* Nonaligned */
	if (CB_TREE_CLASS (ap->var) == CB_CLASS_POINTER
	 || CB_TREE_CLASS (ap->val) == CB_CLASS_POINTER) {
		/* Pointer assignment */
		output_block_open ();
		output_line ("void *temp_ptr;");

		/* temp_ptr = source address; */
		output_prefix ();
		if (ap->val == cb_null || ap->val == cb_zero) {
			/* MOVE NULL ... */
			output ("temp_ptr = 0;");
		} else if (CB_TREE_TAG (ap->val) == CB_TAG_CAST) {
			/* MOVE ADDRESS OF val ... */
			const struct cb_cast *cp = CB_CAST (ap->val);
			output ("temp_ptr = ");
			switch (cp->cast_type) {
			case CB_CAST_ADDRESS:
				output_data (cp->val);
				break;
			case CB_CAST_PROGRAM_POINTER:
				output ("cob_call_field (");
				output_param (ap->val, -1);
				if (current_prog->nested_prog_list) {
					gen_nested_tab = 1;
					output (", cob_nest_tab, 0, %d)",
						cb_fold_call);
				} else {
					output (", NULL, 0, %d)",
						cb_fold_call);
				}
				break;
			/* LCOV_EXCL_START */
			default:
				cobc_err_msg (_("unexpected cast type: %d"),
					cp->cast_type);
				COBC_ABORT ();
			/* LCOV_EXCL_STOP */
			}
			output (";");
		} else {
			/* MOVE val ... */
			output ("memcpy(&temp_ptr, ");
			output_data (ap->val);
			output (", sizeof(temp_ptr));");
		}
		output_newline ();

		/* Destination address = temp_ptr; */
		output_prefix ();
		if (CB_TREE_TAG (ap->var) == CB_TAG_CAST) {
			/* SET ADDRESS OF var ... */
			const struct cb_cast *cp = CB_CAST (ap->var);
			/* LCOV_EXCL_START */
			if (cp->cast_type != CB_CAST_ADDRESS) {
				cobc_err_msg (_("unexpected tree type: %d"),
					cp->cast_type);
				COBC_ABORT ();
			}
			/* LCOV_EXCL_STOP */
			output_data (cp->val);
			output (" = temp_ptr;");
		} else {
			/* MOVE ... TO var */
			output ("memcpy(");
			output_data (ap->var);
			output (", &temp_ptr, sizeof(temp_ptr));");
		}
		output_newline ();

		output_block_close ();
	} else {
		/* Numeric assignment */
		output_prefix ();
		output_integer (ap->var);
		output (" = ");
		output_integer (ap->val);
		if (inside_check == 0) {
			output (";");
			output_newline ();
			if (CB_TREE_TAG (ap->var) == CB_TAG_CAST
			 && CB_CAST (ap->var)->cast_type == CB_CAST_ADDRESS
			 && CB_TREE_TAG (ap->val) == CB_TAG_CAST
			 && CB_CAST (ap->val)->cast_type == CB_CAST_ADDRESS) {
				f1 = cb_code_field (CB_CAST(ap->var)->val);
				if (!f1->flag_field) {
					force_cache (f1);
				}
				if (f1->flag_any_length) {
					f2 = cb_code_field (CB_CAST(ap->val)->val);
					if (!f2->flag_field) {
						force_cache (f2);
					}
					output_line ("%s%d.size = %s%d.size;",
							CB_PREFIX_FIELD, f1->id,
							CB_PREFIX_FIELD, f2->id);
				}
			}
		} else {
			inside_stack[inside_check - 1] = 1;
		}
	}
#else	/* Nonaligned */
	/* Numeric assignment */
	output_prefix ();
	output_integer (ap->var);
	output (" = ");
	output_integer (ap->val);
	if (inside_check == 0) {
		output (";");
		output_newline ();
		if (CB_TREE_TAG (ap->var) == CB_TAG_CAST
		 && CB_CAST (ap->var)->cast_type == CB_CAST_ADDRESS
		 && CB_TREE_TAG (ap->val) == CB_TAG_CAST
		 && CB_CAST (ap->val)->cast_type == CB_CAST_ADDRESS) {
			f1 = cb_code_field (CB_CAST(ap->var)->val);
			if (f1->flag_any_length) {
				f2 = cb_code_field (CB_CAST(ap->val)->val);
				if (!f2->flag_field) {
					force_cache (f2);
				}
				output_line ("%s%d.size = %s%d.size;",
						CB_PREFIX_FIELD, f1->id,
						CB_PREFIX_FIELD, f2->id);
			}
		}
	} else {
		inside_stack[inside_check - 1] = 1;
	}
#endif	/* Nonaligned */
}

static void
output_stmt (cb_tree x)
{
	stack_id = 0;
	if (x == NULL) {
		output_line (";");
		return;
	}
	/* LCOV_EXCL_START */
	if (x == cb_error_node) {
		/* untranslated as unexpected */
		cobc_err_msg ("unexpected error_node parameter");
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */

	if (inside_check != 0) {
		if (inside_stack[inside_check - 1] != 0) {
			inside_stack[inside_check - 1] = 0;
			output (",");
			output_newline ();
		}
	}

	if (x->source_line) {
		cb_source_file = x->source_file;
		cb_source_line = -x->source_line;
		/* cb_source_column = x->source_column; */
	}

	switch (CB_TREE_TAG (x)) {
	case CB_TAG_STATEMENT: {
		const struct cb_statement	*p = CB_STATEMENT (x);
		cb_tree		debug_checks = current_prog->flag_gen_debug ? p->debug_check : NULL;
		/* note: p->name and x->sourcefile/line are always available here */

		/* Output source location, but only if it isn't an implicit statement */
		if (!p->flag_implicit) {
			/* Output source location as a comment */
			skip_line_num = 4;
			/* Output source location as code */
			output_source_reference (x, p->statement);
			/* USE FOR DEBUGGING: pre-fill DEBUG-LINE
			   FIXME: postpone to actual DEBUGGING procedure,
			          using module->module_stmt there
			*/
			if (current_prog->flag_gen_debug
			 && !p->flag_in_debug) {
				output_prefix ();
				output ("memcpy (");
				output_data (cb_debug_line);
				output (", \"%6d\", 6);", x->source_line);
				output_newline ();
			}
			last_line = x->source_line;
			skip_line_num = 0;
#if 0		/* pass reference; needs adjustment in error.c to
			   say "codegen" instead of "compile" */
			cb_source_file = x->source_file;
			cb_source_line = x->source_line;
#endif
		}

		if (!p->file) {

			if (p->ex_handler || p->not_ex_handler) {
				output_line ("COB_RESET_EXCEPTION (0);");
			} else
			if (cobc_wants_debug) {
				output_line ("cob_global_exception = -1;");
			}

		} else {

			const struct cb_file	*fl = CB_FILE (p->file);

			if (p->body && CB_VALUE (p->body)) {
				cb_tree body_value = CB_VALUE (p->body);
				switch (CB_TREE_TAG (body_value)) {
				case CB_TAG_FUNCALL: {
					struct cb_funcall	*c = CB_FUNCALL (body_value);
					if (fl->organization == COB_ORG_RELATIVE) {
						output_file_variable (fl->key, fl, c, "keys[0].field", 1);
					}
					output_file_variable (fl->assign, fl, c, "assign", 0);
					break;
				}
				case CB_TAG_LIST:
				case CB_TAG_DEBUG:
					/* CHECKME: anything needed here? */
					break;
				/* LCOV_EXCL_START */
				default:
					cobc_err_msg (_("unexpected tree tag: %d"),
						(int)CB_TREE_TAG (body_value));
					COBC_ABORT ();
				/* LCOV_EXCL_END */
				}
			/* LCOV_EXCL_START */
			} else {
				cobc_err_msg ("unexpected state");
				COBC_ABORT ();
			}
			/* LCOV_EXCL_END */

			if (p->flag_retry_forever) {
				output_line ("cob_file_set_retry (%s%s,COB_RETRY_FOREVER,0);",
									CB_PREFIX_FILE, fl->cname);
			} else
			if (p->flag_retry_times) {
				output_prefix ();
				output ("cob_file_set_retry (%s%s,COB_RETRY_TIMES,",
									CB_PREFIX_FILE, fl->cname);
				output_integer (p->retry);
				output (");");
				output_newline ();
			} else
			if (p->flag_retry_seconds) {
				output_prefix ();
				output ("cob_file_set_retry (%s%s,COB_RETRY_SECONDS,",
									CB_PREFIX_FILE, fl->cname);
				output_integer (p->retry);
				output (");");
				output_newline ();
			} else
			if (p->flag_advancing_lock) {
				output_line ("cob_file_set_retry (%s%s,COB_ADVANCING_LOCK,0);",
									CB_PREFIX_FILE, fl->cname);
			}
		}

		if (p->null_check) {
			output_stmt (p->null_check);
		}

		if (p->body) {
			output_stmt (p->body);
		}

		/* USE FOR DEBUGGING: Output field debugging statements */
		/* FIXME: for conditionals like IF / EVALUATE + WHEN this should be
		          generated before the check; for statements with body like
		          DISPLAY / CALL [NOT] ON EXCEPTION this comes too late,
		          should be included in all possible branches as first item */
		if (debug_checks) {
			if (p->not_ex_handler && p->statement == STMT_ALLOCATE) {
				/* HACK: postpone to after internal handler code */
			} else {
				output_debug_stmts (debug_checks);
				debug_checks = NULL;
			}
		}

		/* Special debugging callback for START / DELETE */
		/* Must be done immediately after I/O and before */
		/* status check */
		if (current_prog->flag_gen_debug && p->file && p->flag_callback) {
			const struct cb_file* fl = CB_FILE (p->file);
			output_line ("save_exception_code = cob_global_exception;");
			output_stmt (cb_build_debug (cb_debug_name, fl->name, NULL));
			output_move (cb_space, cb_debug_contents);
			output_perform_call (fl->debug_section, NULL);
			output_line ("cob_global_exception = save_exception_code;");
			need_save_exception = 1;
		}

		if (p->ex_handler || p->not_ex_handler
		 || (p->file && CB_EXCEPTION_ENABLE (COB_EC_I_O))) {
			output_handler (p);
			if (debug_checks) {
				output_debug_stmts (debug_checks);
			}
		}
		break;
	}
	case CB_TAG_LABEL:
		output_label (CB_LABEL(x));
		break;

	case CB_TAG_FUNCALL:
		output_prefix ();
		output_funcall (x);
		if (inside_check == 0) {
			output (";");
			output_newline ();
		} else {
			inside_stack[inside_check - 1] = 1;
		}
		break;

	case CB_TAG_ASSIGN:
		output_assign (CB_ASSIGN (x));
		break;

	case CB_TAG_INITIALIZE:
		output_initialize (CB_INITIALIZE (x));
		break;

	case CB_TAG_SEARCH:
		output_search (CB_SEARCH (x));
		break;

	case CB_TAG_CALL:
		output_call (CB_CALL (x));
		break;

	case CB_TAG_GOTO:
		output_goto (CB_GOTO (x));
		break;

	case CB_TAG_CANCEL:
		output_cancel (CB_CANCEL (x));
		break;

	case CB_TAG_SET_ATTR: {
		const struct cb_set_attr *sap = CB_SET_ATTR (x);
		output_set_attribute (sap->fld, sap->val_on, sap->val_off);
		break;
	}
	case CB_TAG_XML_PARSE:
		output_xml_parse (CB_XML_PARSE (x));
		break;

	case CB_TAG_ALTER:
		output_alter (CB_ALTER (x));
		break;

	case CB_TAG_IF:
		output_if (CB_IF (x));
		break;

	case CB_TAG_PERFORM:
		output_perform (CB_PERFORM (x));
		break;

	/* "common" CONTINUE, note:
	   CONTINUE AFTER exp SECONDS is already translated into a funcall */
	case CB_TAG_CONTINUE:
		output_line (";");
		break;

	case CB_TAG_LIST:
		if (cb_flag_extra_brace) {
			output_block_open ();
		}
		for (; x; x = CB_CHAIN (x)) {
			output_stmt (CB_VALUE (x));
		}
		if (cb_flag_extra_brace) {
			output_block_close ();
		}
		break;

	case CB_TAG_REFERENCE:
		output_stmt (CB_REFERENCE(x)->value);
		break;

	case CB_TAG_DIRECT:
		if (CB_DIRECT (x)->flag_is_direct) {
			if (CB_DIRECT (x)->flag_new_line) {
				output_newline ();
			}
			output_line ("%s", (const char *)(CB_DIRECT (x)->line));
		} else {
			output_newline ();
			output_line ("/* %s */", (const char *)(CB_DIRECT (x)->line));
		}
		break;

	/* setting DEBUG-ITEM */
	case CB_TAG_DEBUG:
		if (current_prog->flag_gen_debug) {
			output_debug_item (CB_DEBUG (x));
		}
		break;

	case CB_TAG_DEBUG_CALL:
		output_perform_call (CB_DEBUG_CALL(x)->target, NULL);
		break;

	case CB_TAG_ML_SUPPRESS_CHECKS:
		output_ml_suppress_checks (CB_ML_SUPPRESS_CHECKS (x));
		break;
	/* LCOV_EXCL_START */
	default:
		CB_TREE_TAG_UNEXPECTED_ABORT (x);
	/* LCOV_EXCL_STOP */
	}
}

/* File definition */

static int
output_file_allocation (struct cb_file *f)
{
	if (f->flag_global) {
		output_storage ("\n/* Global file %s */\n", f->name);
		output_storage ("static cob_file\t\t*%s%s = NULL;\n",CB_PREFIX_FILE, f->cname);
		if (f->organization == COB_ORG_RELATIVE
		 && f->key == NULL) {
			int i = lookup_attr (COB_TYPE_NUMERIC_DISPLAY, 0, 0, 0, NULL, 0);
			output_storage ("static unsigned char\t%s%s_recnum[12+1] = \"000000000000\";\n",
				CB_PREFIX_SEQUENCE, f->cname);
			output_storage ("static cob_field %s%s_recnum = { 12, (cob_u8_ptr)%s%s_recnum, &%s%d };\n",
				CB_PREFIX_FIELD, f->cname,
				CB_PREFIX_SEQUENCE, f->cname,
				CB_PREFIX_ATTR, i);
		}
	} else {
		output_local ("\n/* File %s */\n", f->name);
		output_local ("static cob_file\t\t*%s%s = NULL;\n",CB_PREFIX_FILE, f->cname);
		if (f->organization == COB_ORG_RELATIVE
		 && f->key == NULL) {
			int i = lookup_attr (COB_TYPE_NUMERIC_DISPLAY, 0, 0, 0, NULL, 0);
			output_local ("static unsigned char\t%s%s_recnum[12+1] = \"000000000000\";\n",
				CB_PREFIX_SEQUENCE, f->cname);
			output_local ("static cob_field %s%s_recnum = { 12, (cob_u8_ptr)%s%s_recnum, &%s%d };\n",
				CB_PREFIX_FIELD, f->cname,
				CB_PREFIX_SEQUENCE, f->cname,
				CB_PREFIX_ATTR, i);
		}
	}

	if (f->linage) {
		return 1;
	}
	return 0;
}

static void
output_key_components (struct cb_file* f, struct cb_key_component* key_component, int key)
{
	int		parts;
	struct cb_key_component* comp = key_component;
	COB_UNUSED(f);
	COB_UNUSED(key);
	if (key_component != NULL) {
		for (parts = 0; comp != NULL; comp = comp->next, ++parts);
		output (",");
		if (parts > 1) {
			output_newline ();
			output_indent_level += 18;
			output_prefix ();
		}
		output ("%d",parts);
		for (comp = key_component; comp != NULL; comp = comp->next) {
			output (",");
			output_param (comp->component, -1);
		}
		if (parts > 1) {
			output_indent_level -= 18;
		}
	} else {
		output (",0,NULL");
	}
	output (");");
	output_newline ();
}

static cb_tree
get_indexed_file_key_colseq (const struct cb_file *f, const struct cb_alt_key *ak)
{
	const cb_tree	key = ak ? ak->key : f->key;
	const cb_tree	key_col = ak ? ak->collating_sequence_key : f->collating_sequence_key;
	const int	type = cb_tree_type (key, cb_code_field (key));
	cb_tree		col = NULL;

	/* We only apply a collating sequence if the key is alphanumeric / display */
	if ((type & COB_TYPE_ALNUM) || (type == COB_TYPE_NUMERIC_DISPLAY)) {
		col = key_col ? key_col : f->collating_sequence;
#if 0	/* TODO: this should be done for national, when available */
	} else if (type & COB_TYPE_NATIONAL) {
		col = key_col_n ? key_col_n : f->collating_sequence_n;
#endif
	}

	if (col != NULL && CB_REFERENCE_P (col)) {
		return cb_ref(col);
	} else {
		return NULL;
	}
}

static void
output_file_initialization (struct cb_file *f)
{
	int		nkeys;
	char		nxt[8];
	char		features[128];
#define FNAME_SIZE	64
	char		file_name[FNAME_SIZE], extname[FNAME_SIZE + 2];
#undef FNAME_SIZE
	const char	*org_name = "0";
	const char	*acc_name = "0";
	const char	*fmt_name = "0";
	const char	*file_features = "0";
	switch (f->organization) {
	case COB_ORG_SEQUENTIAL:
		org_name = "COB_ORG_SEQUENTIAL";
		break;
	case COB_ORG_RELATIVE:
		org_name = "COB_ORG_RELATIVE";
		break;
	case COB_ORG_LINE_SEQUENTIAL:
		org_name = "COB_ORG_LINE_SEQUENTIAL";
		break;
	case COB_ORG_INDEXED:
		org_name = "COB_ORG_INDEXED";
		break;
	case COB_ORG_SORT:
		org_name = "COB_ORG_SORT";
		break;
	/* LCOV_EXCL_START */
	default:
		cobc_err_msg ("unexpected file type: %d",
			 (int)f->organization);
		COBC_ABORT ();
	/* LCOV_EXCL_STOP */
	}
	switch (f->access_mode) {
	case COB_ACCESS_SEQUENTIAL:
		acc_name = "COB_ACCESS_SEQUENTIAL";
		break;
	case COB_ACCESS_RANDOM:
		acc_name = "COB_ACCESS_RANDOM";
		break;
	case COB_ACCESS_DYNAMIC:
		acc_name = "COB_ACCESS_DYNAMIC";
		break;
	/* LCOV_EXCL_START */
	default:
		cobc_err_msg ("unexpected access type: %d",
			 (int)f->access_mode);
		COBC_ABORT ();
	/* LCOV_EXCL_STOP */
	}
	if (!f->flag_line_adv
	 && !f->flag_has_organization
	 && f->organization == COB_ORG_LINE_SEQUENTIAL
	 && strcmp(file_features,"0") == 0) {
		file_features = "COB_FILE_LS_DEFAULT";
		fmt_name = "COB_FILE_IS_DFLT";
	} else
	if (cb_mf_files
	 && cb_std_define != CB_STD_85) {	/* Not if cobol85 test suite */
		fmt_name = "COB_FILE_IS_MF";
		if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
			file_features = "COB_FILE_LS_NULLS";
		}
	} else {
		fmt_name = "COB_FILE_IS_DFLT";
	}
	strcpy (features,"");
	strcpy (nxt,"");
	if (f->file_status) {
		sprintf(&features[strlen(features)],"%sCOB_SELECT_FILE_STATUS",nxt);
		strcpy(nxt,"|");
	}
	if (f->linage) {
		sprintf(&features[strlen(features)],"%sCOB_SELECT_LINAGE",nxt);
		strcpy(nxt,"|");
	}
	if (f->flag_external) {
		sprintf(&features[strlen(features)],"%sCOB_SELECT_EXTERNAL",nxt);
		strcpy(nxt,"|");
	}
	if (f->special) { /* Special assignment */
		sprintf(&features[strlen(features)],"%s%d",nxt,f->special);
		strcpy(nxt,"|");
	}
	if (features[0] < ' ')
		strcpy(features,"0");

	output_line ("/* File initialization for %s */", f->name);
	sprintf (file_name, "%s%s", CB_PREFIX_FILE, f->cname);
	if (f->organization == COB_ORG_RELATIVE
	 || f->organization == COB_ORG_INDEXED) {
		struct cb_alt_key *l;
		nkeys = 1;
		for (l = f->alt_key_list; l; l = l->next) {
			nkeys++;
		}
	} else {
		nkeys = 0;
	}
	if (f->flag_external) {
		sprintf (extname, "\"%s\"", file_name);
	} else {
		strcpy (extname, "NULL");
	}
	/* TODO: generate enum names here */
	output_line ("cob_file_create (&%s, %s, \"%s\",", file_name, extname, f->name);
	output_indent_level += 17;
	output_line ("%s,%s,%d,",org_name,acc_name,f->optional);
	output_line ("%s,%s,%d,%d,%d,",fmt_name,features,nkeys,f->record_min,f->record_max);
	output_prefix ();
	output_param (f->assign, -1);
	output (",");
	output_param (CB_TREE (f->record), -1);
	output (");");
	output_indent_level -= 17;
	output_newline ();

	nkeys = 1;
	/* Output RELATIVE/RECORD KEY's */
	if (f->organization == COB_ORG_RELATIVE
	 || f->organization == COB_ORG_INDEXED) {
		struct cb_alt_key	*l;
		output_prefix ();
		output ("cob_file_set_key (%s, 0, ", file_name);
		if (f->organization == COB_ORG_RELATIVE
		 && f->key == NULL) {
			output ("&%s%s_recnum", CB_PREFIX_FIELD, f->cname);
		} else {
			output_param (f->key, -1);
		}
		output (", %d, 0, -1, NULL", f->flag_primary_dups);
		output_key_components (f, f->component_list, 0);
		if (f->organization == COB_ORG_INDEXED) {
			cb_tree col = get_indexed_file_key_colseq (f, NULL);
			if (col != NULL) {
				output_prefix ();
				output ("cob_file_set_key_extra (%s, 0, -1, -1, NULL, ", file_name);
				output_param (col, -1);
				output (");");
				output_newline ();
			}
		}
		for (l = f->alt_key_list; l; l = l->next) {
			output_prefix ();
			output ("cob_file_set_key (%s, %d, ", file_name, nkeys);
			output_param (l->key, -1);
			if (l->keycompress)
				output (", (%d << 8) + %d, 0" , l->keycompress, l->duplicates);
			else
				output (", %d, 0" , l->duplicates);
			if (l->suppress
			 && CB_LITERAL_P(l->suppress)) {
				struct cb_literal	*lit = CB_LITERAL (l->suppress);
				output (", %d, \"%.*s\"", lit->size, lit->size, lit->data);
			} else
			if (l->tf_suppress) {
				if (isprint((char)l->char_suppress))
					output (", 0, (const unsigned char *)\"%c\"",l->char_suppress);
				else
					output (", 0, (const unsigned char *)\"\\%03o\"",l->char_suppress);
			} else {
				output (", -1, NULL");
			}
			output_key_components (f, l->component_list, nkeys);
			if (f->organization == COB_ORG_INDEXED) {
				cb_tree col = get_indexed_file_key_colseq (f, l);
				if (col != NULL) {
					output_prefix ();
					output ("cob_file_set_key_extra (%s, %d, -1, -1, NULL, ", file_name, nkeys);
					output_param (col, -1);
					output (");");
					output_newline ();
				}
			}
			nkeys++;
		}
	}

	if (f->flag_line_adv
	 || f->record_depending
	 || strcmp(file_features,"0") != 0) {
		output_prefix ();
		output ("cob_file_set_attr (%s,",file_name);
		if (f->record_depending) {
			output_param (f->record_depending, -1);
		} else {
			output ("NULL");
		}
		output (",%d,%s", f->flag_line_adv,file_features);
		output (",NULL");	/* codeset */
		output (",NULL");	/* password */
		output (",NULL");	/* cryptkey */
		output (");");
		output_newline ();
	}

	if (f->lock_mode) {
		strcpy(nxt,"");
		output_prefix ();
		output ("cob_file_set_lock (%s,",file_name);
		if ((f->lock_mode & COB_LOCK_OPEN_EXCLUSIVE)) {
			output("%sCOB_LOCK_OPEN_EXCLUSIVE",nxt);
			strcpy(nxt,"|");
		}
		if ((f->lock_mode & COB_LOCK_EXCLUSIVE)) {
			output("%sCOB_LOCK_EXCLUSIVE",nxt);
			strcpy(nxt,"|");
		}
		if ((f->lock_mode & COB_LOCK_MANUAL)) {
			output("%sCOB_LOCK_MANUAL",nxt);
			strcpy(nxt,"|");
		}
		if ((f->lock_mode & COB_LOCK_AUTOMATIC)) {
			output("%sCOB_LOCK_AUTOMATIC",nxt);
			strcpy(nxt,"|");
		}
		if ((f->lock_mode & COB_LOCK_MULTIPLE)) {
			output("%sCOB_LOCK_MULTIPLE",nxt);
			strcpy(nxt,"|");
		}
		if ((f->lock_mode & COB_LOCK_ROLLBACK)) {
			output("%sCOB_LOCK_ROLLBACK",nxt);
			strcpy(nxt,"|");
		}
		if (nxt[0] < ' ')
			output("%d",f->lock_mode);
		output (");");
		output_newline ();
	}

	if (f->linage) {
		output_prefix ();
		output ("cob_file_set_linage (%s,",file_name);
		output_param (f->linage, -1);
		output (",");
		output_param (f->linage_ctr, -1);
		output (",");
		if (f->latfoot) {
			output_param (f->latfoot, -1);
		} else {
			output ("NULL");
		}
		output (",");
		output_newline ();
		output_indent_level += 18;
		output_prefix ();
		if (f->lattop) {
			output_param (f->lattop, -1);
		} else {
			output ("NULL");
		}
		output (",");
		if (f->latbot) {
			output_param (f->latbot, -1);
		} else {
			output ("NULL");
		}
		output (");");
		output_indent_level -= 18;
		output_newline ();
	}

	if (f->organization == COB_ORG_RELATIVE
	 || f->organization == COB_ORG_INDEXED) {
		if ((f->flag_sql_xfd || cb_all_files_xfd)
		 && cb_flag_sql_xfd) {
			if (f->sql_name) {
				output_line ("cob_file_xfdname (%s%s, \"%s\");", 
								CB_PREFIX_FILE, f->cname, f->sql_name);
			}
			output_xfd_file (f);
		}
	}

	if (f->organization != COB_ORG_SORT
	 && f->code_set) {
		/* pass CODE-SET as collation */
		const char *alph_write, *alph_read;
		switch (f->code_set->alphabet_type) {
		case CB_ALPHABET_ASCII:
			alph_read = "cob_ascii_ebcdic";
			alph_write = "cob_ebcdic_ascii";
			gen_ebcdic_ascii |= 1;
			gen_ascii_ebcdic |= 1;
			break;
		case CB_ALPHABET_EBCDIC:
			alph_read = "cob_ebcdic_ascii";
			alph_write = "cob_ascii_ebcdic";
			gen_ebcdic_ascii |= 1;
			gen_ascii_ebcdic |= 1;
			break;
		/* case CB_ALPHABET_CUSTOM: */
		default:
			alph_read = alph_write = NULL;
			break;
		}

		output_line ("%s%s->sort_collating = %s;", CB_PREFIX_FILE, f->cname, alph_write);
		output_line ("%s%s->code_set_read = %s;", CB_PREFIX_FILE, f->cname, alph_read);
		if (f->code_set_items) {
			const unsigned int items = cb_list_length (CB_TREE (f->code_set_items));
			unsigned int i = 0;
			struct cb_list	*l;
			output_line ("%s%s->nconvert_fields = %u;", CB_PREFIX_FILE, f->cname, items);
			output_line ("%s%s->convert_field = cob_malloc (sizeof (struct __cob_field) * %u);",
				CB_PREFIX_FILE, f->cname, items);
			for (l = f->code_set_items; l; l = CB_LIST (l->chain)) {
				output_prefix ();
				output ("COB_SET_FLD (%s%s->convert_field[%u], %d, ",
					CB_PREFIX_FILE, f->cname, i++, CB_FIELD_PTR (l->value)->size);
				output_data (l->value);
				output (", NULL );");
				output_newline ();
				if (!l->chain) {
					break;
				}
			}
		}
	}

	output_line ("cob_file_complete (%s);",file_name);
	output_newline ();
}

/* Screen definition */

static void
output_screen_definition (struct cb_field *p)
{
	int	type;

	if (p->sister) {
		output_screen_definition (p->sister);
	}
	if (p->children) {
		output_screen_definition (p->children);
	}

	type = (p->children ? COB_SCREEN_TYPE_GROUP :
		p->values ? COB_SCREEN_TYPE_VALUE :
		(p->size > 0) ? COB_SCREEN_TYPE_FIELD : COB_SCREEN_TYPE_ATTRIBUTE);
	if (type == COB_SCREEN_TYPE_FIELD || type == COB_SCREEN_TYPE_VALUE) {
		p->count++;
	}

	output_local ("static cob_screen\t%s%d;\n", CB_PREFIX_SCR_FIELD, p->id);
}

static void
output_screen_init (struct cb_field *p, struct cb_field *previous)
{
	const int	type = (p->children ? COB_SCREEN_TYPE_GROUP :
		p->values ? COB_SCREEN_TYPE_VALUE :
		(p->size > 0) ? COB_SCREEN_TYPE_FIELD : COB_SCREEN_TYPE_ATTRIBUTE);

	output_prefix ();
	output ("COB_SET_SCREEN (%s%d, %d, 0x" CB_FMT_LLX ", ",
		CB_PREFIX_SCR_FIELD, p->id, type, p->screen_flag);

	if (p->sister && p->sister->level != 1) {
		output ("&%s%d, ", CB_PREFIX_SCR_FIELD, p->sister->id);
	} else {
		output ("NULL, ");
	}

	if (previous && previous->level != 1) {
		output ("&%s%d, ", CB_PREFIX_SCR_FIELD, previous->id);
	} else {
		output ("NULL, ");
	}

	output_newline ();
	output_prefix ();
	output ("\t\t  ");

	if (type == COB_SCREEN_TYPE_GROUP) {
		output ("&%s%d, ", CB_PREFIX_SCR_FIELD, p->children->id);
	} else {
		output ("NULL, ");
	}

	if (p->parent) {
		output ("&%s%d, ", CB_PREFIX_SCR_FIELD, p->parent->id);
	} else {
		output ("NULL, ");
	}

	if (type == COB_SCREEN_TYPE_FIELD) {
		output_param (cb_build_field_reference (p, NULL), -1);
	} else {
		output ("NULL");
	}
	output (", ");

	output_newline ();
	output_prefix ();
	output ("\t\t  ");

	if (type == COB_SCREEN_TYPE_VALUE) {
		/* Need a field reference here */
		output_param (cb_build_field_reference (p, NULL), -1);
	} else {
		output ("NULL");
	}
	output (", ");

	if (p->screen_line) {
		output_param (p->screen_line, 0);
	} else {
		output ("NULL");
	}
	output (", ");

	if (p->screen_column) {
		output_param (p->screen_column, 0);
	} else {
		output ("NULL");
	}
	output (", ");

	output_newline ();
	output_prefix ();
	output ("\t\t  ");

	if (p->screen_foreg) {
		output_param (p->screen_foreg, 0);
	} else {
		output ("NULL");
	}
	output (", ");

	if (p->screen_backg) {
		output_param (p->screen_backg, 0);
	} else {
		output ("NULL");
	}
	output (", ");

	if (p->screen_prompt) {
		output_param (p->screen_prompt, 0);
	} else {
		output ("NULL");
	}
	output (", %d);", p->occurs_min);

	output_newline ();

	/* TODO: pass information for USAGE CONTROL items here */

	if (p->parent) {
		/* Generate useless reference to avoid C compile warning */
		output_prefix ();
		output("COB_UNUSED(");
		output_param (cb_build_field_reference (p->parent, NULL), -1);
		output (");");
		output_newline ();
	}

	if (p->children) {
		output_screen_init (p->children, NULL);
	}
	if (p->sister) {
		output_screen_init (p->sister, p);
	}
}

/* JSON/XML GENERATE trees */

static void
output_ml_attrs_init (struct cb_ml_generate_tree *attr)
{
	for (; attr; attr = attr->sibling) {
		output_prefix ();
		output ("cob_set_ml_attr (&%s%d, ", CB_PREFIX_ML_ATTR, attr->id);

		output_param (attr->name, -1);
		output (", ");

		output_param (attr->value, -1);
		output (", 0, ");

		if (attr->sibling) {
			output ("&%s%d", CB_PREFIX_ML_ATTR, attr->sibling->id);
		} else {
			output ("NULL");
		}
		output (");");
		output_newline ();
	}
}

static void
output_ml_elt_init (struct cb_ml_generate_tree *tree)
{
	output_prefix ();
	output ("cob_set_ml_tree (&%s%d, ", CB_PREFIX_ML_TREE, tree->id);

	output_param (tree->name, -1);

	if (tree->attrs) {
		output (", &%s%d, ", CB_PREFIX_ML_ATTR, tree->attrs->id);
	} else {
		output (", NULL, ");
	}

	if (tree->value) {
		output_param (tree->value, -1);
	} else {
		output ("NULL");
	}

	output (", 0, ");

	if (tree->children) {
		output ("&%s%d, ", CB_PREFIX_ML_TREE, tree->children->id);
	} else {
		output ("NULL, ");
	}

	if (tree->sibling) {
		output ("&%s%d", CB_PREFIX_ML_TREE, tree->sibling->id);
	} else {
		output ("NULL");
	}

	output (");");
	output_newline ();
}

static void
output_ml_generate_init (struct cb_ml_generate_tree *tree)
{
	for (; tree; tree = tree->sibling) {
		if (tree->attrs) {
			output_ml_attrs_init (tree->attrs);
		}
		if (tree->children) {
			output_ml_generate_init (tree->children);
		}
		output_ml_elt_init (tree);
	}
}

/* Handle REPORTs */

/* Report data definition */

/* Individual fields of the report(s) */
static int report_col_pos = 1;
static void
output_report_data (struct cb_field *p)
{
	if (p->storage == CB_STORAGE_REPORT) {
		output_emit_field (cb_build_field_reference (p, NULL), NULL);
		if (p->report_sum_counter) {
			output_emit_field (p->report_sum_counter, "SUM");
		}
		if (p->report_control) {
			output_emit_field (p->report_control, "CONTROL");
		}
		if (p->children) {
			output_report_data (p->children);
		}
	}
	if (p->sister) {
		output_report_data (p->sister);
	}
}

static void
output_report_sum_control_field (struct cb_field *p)
{
	cb_tree	l,x;
	struct cb_field *f;
	if (p == NULL) {
		return;
	}
	if (p->storage == CB_STORAGE_REPORT) {
		if (p->level == 01) {
			output_base (p, 1U);
		}
		if (p->report_sum_counter) {
			output_base (cb_code_field (p->report_sum_counter), 1U);
		}
		if (p->report_control) {
			output_base (cb_code_field (p->report_control), 1U);
		}
		if (p->report_source && CB_REF_OR_FIELD_P (p->report_source)) {
			output_base (cb_code_field (p->report_source), 1U);
		}
		for (l = p->report_sum_list; l; l = CB_CHAIN (l)) {
			x = CB_VALUE (l);
			if (!CB_REF_OR_FIELD_P (x)) {
				if (CB_PURPOSE (l)) {
					x = CB_PURPOSE (l);
				} else {
					/* TODO: comment about when we get in here */
					continue;
				}
			}
			f = cb_code_field (x);
			output_base (f, 1);
			if (!f->flag_field) {
				FILE	*savetarget;
				struct field_list	*fl;
				savetarget = output_target;
				output_target = NULL;
				output_field (x);

				fl = cobc_parse_malloc (sizeof (struct field_list));
				fl->x = x;
				fl->f = f;
				fl->curr_prog = excp_current_program_id;
				if (f->index_type != CB_INT_INDEX
				 && (   f->flag_is_global
				     || current_prog->flag_file_global)) {
					fl->next = field_cache;
					field_cache = fl;
				} else {
					fl->next = local_field_cache;
					local_field_cache = fl;
				}

				f->flag_field = 1;
				output_target = savetarget;
			}
		}
		if (p->children) {
			output_report_sum_control_field (p->children);
		}
	}
	if (p->sister) {
		output_report_sum_control_field (p->sister);
	}
}

static void
output_report_summed_field (struct cb_field *p)
{
	if (p == NULL) {
		return;
	}

	if (p->storage == CB_STORAGE_REPORT) {
		cb_tree	l, x;
		struct cb_field *f;

		for (l = p->report_sum_list; l; l = CB_CHAIN (l)) {
			x = CB_VALUE (l);
			if (!CB_REF_OR_FIELD_P (x)) {
				if (CB_PURPOSE (l)) {
					x = CB_PURPOSE (l);
				} else {
					/* TODO: comment about when we get in here */
					continue;
				}
			}
			f = cb_code_field (x);
			if (f->storage == CB_STORAGE_WORKING
			&& !(f->report_flag & COB_REPORT_REF_EMITTED)) {
				output_emit_field (cb_build_field_reference (f, NULL), NULL);
			}
		}
		if (p->children) {
			output_report_summed_field (p->children);
		}
	}
	if (p->sister) {
		output_report_summed_field (p->sister);
	}
}

/* Report definition */

static void
output_report_control (struct cb_report *p, int id, cb_tree ctl, cb_tree nx)
{
	struct cb_field *s;
	struct cb_field *f;
	cb_tree	l, x;
	int	i, bfound, prvid, seq;

	x = CB_VALUE (ctl);
	s = cb_code_field(x);
	if(nx) {
		output_report_control(p, id, nx, CB_CHAIN(nx));
	}
	output_local("/* Report %s: CONTROL %s */\n",p->name,s->name);
	prvid = 0;
	for(i = 0; i < p->num_lines; i++) {
		if(p->line_ids[i]->report_control) {
			struct cb_field *c = cb_code_field (p->line_ids[i]->report_control);
			if(c == s) {
				f = p->line_ids[i];
				if(f->report_flag & COB_REPORT_CONTROL_HEADING) {
					output_local("/* CONTROL HEADING: %s */\n",s->name);
				} else if(f->report_flag & COB_REPORT_CONTROL_FOOTING) {
					output_local("/* CONTROL FOOTING: %s */\n",s->name);
				}
				output_local("static cob_report_control_ref %s%d = {",
						CB_PREFIX_REPORT_REF,p->line_ids[i]->id);
				if(prvid == 0) {
					output_local("NULL,");
				} else {
					output_local("&%s%d,",CB_PREFIX_REPORT_REF,prvid);
				}
				output_local("&%s%d",CB_PREFIX_REPORT_LINE,p->line_ids[i]->id);
				output_local("};\n");
				prvid = p->line_ids[i]->id;
			}
		}
	}
	output_local ("static cob_report_control   %s%d_%d\t= {", CB_PREFIX_REPORT_CONTROL,id,s->id);
	if(nx) {
		output_local("&%s%d_%d,",CB_PREFIX_REPORT_CONTROL,id,cb_code_field(CB_VALUE(nx))->id);
	} else {
		output_local("NULL,");
	}
	output_local ("\"%s\",",s->name);
	output_local("&%s%d,NULL,NULL",CB_PREFIX_FIELD,s->id);
	bfound = 0;
	/* CB_PREFIX_REPORT_REF */
	for(i= p->num_lines-1; i >= 0; i--) {
		if(p->line_ids[i]->report_control) {
			struct cb_field *c = cb_code_field (p->line_ids[i]->report_control);
			if(c == s) {
				bfound = 1;
				output_local(",&%s%d",CB_PREFIX_REPORT_REF,p->line_ids[i]->id);
				break;
			}
		}
	}
	if(!bfound) {
		printf("Control field %s is not referenced in report\n",s->name);
		output_local(",NULL");
	}
	seq = i = 0;
	for (l = p->controls; l; l = CB_CHAIN (l)) {
		x = CB_VALUE (l);
		f = cb_code_field(x);
		i++;
		if(s == f) {
			seq = i;
			break;
		}
	}
	output_local(",%d,0,0,0,0",seq);
	output_local("};\n");
}

static void
output_report_field_cmt (struct cb_field *f)
{
	struct cb_field *s;

	if (f->report_source
	 || f->report_control
	 || (f->report_flag & COB_REPORT_PRESENT)) {
		output_local("\t\t/* ");
		if(f->report_source_txt) {
			output_local("%s, SOURCE %s; ",get_field_name (f),f->report_source_txt);
		}
		if((f->report_flag & COB_REPORT_PRESENT)) {
			output_local("PRESENT ");
			if((f->report_flag & COB_REPORT_BEFORE))
				output_local("BEFORE ");
			else
				output_local("AFTER ");
			if((f->report_flag & COB_REPORT_ALL))
				output_local("ALL ");
			if((f->report_flag & COB_REPORT_PAGE))
				output_local("PAGE ");
			if(f->report_control) {
				s = cb_code_field (f->report_control);
				if((f->report_flag & COB_REPORT_PAGE))
					output_local("OR ");
				if(s) output_local("%s; ",s->name);
			}
		} else
		if(f->report_control) {
			s = cb_code_field (f->report_control);
			if(s) output_local("CONTROL %s; ",s->name);
		}
		output_local("*/\n");
	}
}

static void
output_report_one_field (struct cb_report *r, struct cb_field *f, int idx, int occ)
{
	cb_tree	value;
	char	field_name[16];
	struct cb_field *p;

	output_report_field_cmt (f);

	if (f->report_field_name != NULL)
		sprintf (field_name, "%s%d", CB_PREFIX_REPORT_FIELD, ++report_field_id);
	while (idx == 0
		|| idx <= occ) {
		if (idx <= 1) {
			if (f->report_field_name == NULL) {
				sprintf (field_name, "%s%d", CB_PREFIX_REPORT_FIELD, ++report_field_id);
				f->report_field_name = cobc_parse_strdup (field_name);
			} else {
				strcpy (field_name, f->report_field_name);
			}
		}

		output_local ("static cob_report_field %s\t= {%s,", field_name, report_field_name);
		if (idx > 1)
			output_local ("&%s%d_%d,", CB_PREFIX_FIELD, f->id, idx);
		else
			output_local ("&%s%d,", CB_PREFIX_FIELD, f->id);

		sprintf (report_field_name, "&%s", field_name);

		if (f->report_field_from) {
			output_local ("/* FROM %s */{", f->report_field_from->name);
			p = real_field_founder (f->report_field_from);
			if (f->report_field_size > 1)
				output_local ("%d,", f->report_field_size);
			else
				output_local ("%d,", (int)f->report_field_from->size);
			output_local ("%s%d", CB_PREFIX_BASE, p->id);
			if (f->report_field_offset > 0)
				output_local (" + %d", f->report_field_offset);
			else if (f->report_field_from->offset > 0)
				output_local (" + %d", f->report_field_from->offset);
			output_local (",");
			output_attr (cb_build_field_reference (f->report_field_from, NULL));
			output_local ("},");
		} else if (f->report_from) {
			p = cb_code_field (f->report_from);
			output_local ("/* COMPUTE */{%d,", p->size);
			output_base (p, 0);
			output_local (",");
			output_attr (cb_build_field_reference (p, NULL));
			output_local ("},");
		} else {
			output_local ("{0,NULL,NULL},");
		}
		if (f->report_sum_counter) {
			output_local ("/* SUM */");
			output_param (f->report_sum_counter, 0);
		} else {
			output_local ("NULL");
		}
		output_local (",");
		if(f->report_control) {
			output_local("/* CONTROL */");
			output_param (f->report_control, 0);
		} else {
			output_local("NULL");
		}
		output_local(",");
		value = NULL;
		if (f->values) {
			if (idx > 0 && idx <= occ) {
				value = cb_list_entry (f->values, idx);
				if (value == NULL) {
					value = cb_list_entry (f->values, -1);
				}
			} else {
				value = CB_VALUE (f->values);
			}
		} else if (f->report_source
			  && CB_LITERAL_P (f->report_source)) {
			value = f->report_source;
		}
		if (value
		 && CB_TREE_TAG (value) == CB_TAG_LITERAL) {
			char	*val, *out;
			size_t	ref_size;
			struct cb_literal	*lit = CB_LITERAL (value);
			int		i, j, fsz, lsz;
			if (lit->all) {
				ref_size = f->size;
			} else {
				ref_size = lit->size;
			}
			val = (char *)cobc_malloc (ref_size * 2 + 2);
			out = (char *)cobc_malloc (ref_size * 2 + 2);
			if (lit->all) {
				fsz = (int)f->size;
				lsz = (int)lit->size;
				i = 0;
				while (i < fsz) {
					if (fsz < (i + lsz))
						j = fsz - i;
					else
						j = lsz;
					if (j <= 0) break;
					memcpy (&val[i], lit->data, j);
					i += j;
				}
				val[i] = 0;
			} else {
				memcpy (val, lit->data, lit->size);
			}
			for (i = j = 0; j < ref_size && val[j] != 0; j++) {
				if (val[j] == '"'
				 || val[j] == '\\')	/* Fix string for C code */
					out[i++] = '\\';
				out[i++] = val[j];
			}
			out[i] = 0;
			output_local ("\"%s\",%d,", out, (int)ref_size);
			cobc_free ((void*) val);
			cobc_free ((void*) out);
		} else {
			output_local ("NULL,0,");
		}
		if (f->step_count < f->size)
			f->step_count = f->size;
		if (f->report_column <= 0)	/* No COLUMN was given */
			f->report_column = 1;
		if ((f->report_flag&~(COB_REPORT_EMITTED|COB_REPORT_COLUMN_PLUS)) == 0) {
			output_local ("0x0,%d", f->report_line);
		} else {
			output_local ("0x%X,%d", f->report_flag&~COB_REPORT_EMITTED, f->report_line);
		}
		if (idx) {
			value = cb_list_entry (f->report_column_list, idx);
			if (value) {
				output (",%d ", cb_get_int (value));
			} else {
				output_local (",%d", f->report_column);
			}
		} else {
			output_local (",%d", f->report_column);
		}
		output_local (",%d", f->step_count);
		output_local (",%d", f->next_group_line);
		output_local (",%d", f->level);
		output_local (",0,0,0"); /* reportio flags: group_indicate, suppress, present_now */
		output_local ("};\n");
		if (idx > 0 && idx < occ) {
			idx++;
			sprintf (field_name, "%s%d", CB_PREFIX_REPORT_FIELD, ++report_field_id);
			continue;
		}
		break;
	}
}

static void
output_report_def_fields (int bgn, int id, struct cb_field *f, struct cb_report *r)
{
	struct cb_field *p;
	int			idxtbl[COB_MAX_SUBSCRIPTS+1];
	int			occtbl[COB_MAX_SUBSCRIPTS+1];
	struct cb_field	*pftbl[COB_MAX_SUBSCRIPTS+1];
	int		occ, idx, lines, depon;

	if (bgn == 1) {
		strcpy (report_field_name, "NULL");
	}

	if (bgn == 0
	 && (f->report_flag & COB_REPORT_LINE)) {	/* Start of next Line */
		return;
	}
	if (f->storage != CB_STORAGE_REPORT
	 || f->report != r)
		return;
	if (f->sister) {
		output_report_def_fields (0, id, f->sister, r);
	}

	if (f->children) {
		f->report_flag |= COB_REPORT_GROUP_ITEM;
	}
	if (f->report_when) {
		f->report_flag |= COB_REPORT_HAD_WHEN;
	}
	for (idx=0; idx <= COB_MAX_SUBSCRIPTS; idx++) {
		idxtbl[idx] = 0;
		pftbl[idx] = NULL;
	}
	occ = 1;
	depon = lines = idx = 0;
	for (p = f; p; p = p->parent) {
		if (p->flag_occurs
		 && p->occurs_max > 1) {
			occ = occ * p->occurs_max;
			pftbl [idx] = p;
			occtbl[idx] = p->occurs_max;
			idx++;
			if ((p->report_flag & COB_REPORT_LINE))
				lines++;
			if (p->depending)
				depon++;
		}
	}
	for (p = f; p; p = p->parent) {
		if (p->flag_occurs) {
			if (p->report_column_list != NULL
			 && f->values != NULL
			 && f->depending == NULL
			 && idx == 1
			 && cb_list_length (p->report_column_list) == p->occurs_max) {
				occ = p->occurs_max;
				break;
			}
			if (p->values != NULL
			 && cb_list_length (p->values) == occ) {
				occ = p->occurs_max;
				break;
			}
			if (f->children) {
				output_report_def_fields (0, id, f->children, r);
			} else
			if (p->values != NULL) {
				occ = p->occurs_max;
				break;
			}
			return;		/* OCCURS is handled via emitted code */
		}
	}

	if (f->children) {
		output_report_def_fields (0, id, f->children, r);
	}

	if (f->report_field_from == NULL
	 && f->report_sum_counter == NULL
	 && f->report_when == NULL
	 && f->values == NULL
	 && f->report_from == NULL
	 && (f->report_source == NULL || !CB_LITERAL_P (f->report_source))
	 && f->report_control == NULL)	/* This field has nothing to do */
		return;

	if (idx > 1) {
		int	k, max, pos = 1;
		for (k = 0; k < idx; k++) {
			p = pftbl [k];
			if (p
			 && (p->report_flag & COB_REPORT_LINE)) {
				break;
			}
			max = occtbl[k+1];
			while (idxtbl[k+1]++ <= occtbl[k+1]) {
				output_report_one_field (r, f, pos, pos + occtbl[k+1] - 1);
				pos += occtbl[k+1];
				max += occtbl[k+1];
			}
		}
	} else {
		output_report_one_field (r, f, idx, occ);
	}
}

static void
output_report_define_lines (int top, struct cb_report *r, struct cb_field *f, int idx)
{
	struct cb_field *n, *c, *p;
	char	fname[128];

	if (f == NULL)
	    return;
	n = f->sister;
	c = f->children;
	if (n
	 && n->storage != CB_STORAGE_REPORT)
		n = NULL;
	if (n
	 && n->report != r)
		n = NULL;
	if (c
	 && c->storage != CB_STORAGE_REPORT)
		c = NULL;
	if (n
	 && (n->report_flag & COB_REPORT_LINE)) {
		output_report_define_lines(top, r, n, 0);
	}
	if (c
	 && (c->report_flag & COB_REPORT_LINE)) {
		if (c->flag_occurs
		 && !chk_field_variable_size (c)) {
			int x;
			for (x = 1; x <= c->occurs_max; x++) {
				c->report_flag &= ~COB_REPORT_LINE_EMITTED;
				output_report_define_lines(0, r, c, x);
			}
		} else {
			output_report_define_lines(0, r, c, 0);
		}
	} else {
		c = NULL;
	}
	if (!top)
		c = NULL;

	if (f->report_flag & COB_REPORT_LINE_EMITTED)	/* Already emitted? */
		return;
	f->report_flag |= COB_REPORT_LINE_EMITTED;

	if (f->report_flag & COB_REPORT_PAGE_HEADING) {
		strcpy (fname," PAGE HEADING");
	} else if (f->report_flag & COB_REPORT_PAGE_FOOTING) {
		strcpy (fname, "PAGE HEADING");
	} else if (f->report_flag & COB_REPORT_HEADING) {
		strcpy (fname, "REPORT HEADING");
	} else if (f->report_flag & COB_REPORT_FOOTING) {
		strcpy (fname, "REPORT FOOTING");
	} else if (f->report_flag & COB_REPORT_CONTROL_HEADING) {
		strcpy (fname, "CONTROL HEADING");
	} else if (f->report_flag & COB_REPORT_CONTROL_FOOTING) {
		strcpy (fname, "CONTROL FOOTING");
	} else if (f->report_flag & COB_REPORT_CONTROL_FOOTING_FINAL) {
		strcpy (fname, "CONTROL FOOTING FINAL");
	} else if (f->report_flag & COB_REPORT_CONTROL_HEADING_FINAL) {
		strcpy (fname, "CONTROL HEADING FINAL");
	} else {
		sprintf (fname, "Line %d", f->common.source_line);
	}
	if (f->children)
		strcat (fname, " GROUP");
	if (f->report_control) {
		sprintf(&fname[strlen (fname)], " %s",
			cb_code_field (f->report_control)->name);
	}
	if (memcmp (f->name, "FILLER ", 7) == 0
	 || f->flag_filler) {
		strcat (fname, " of ");
	} else {
		strcat (fname, " ");
		strcat (fname, f->name);
		strcat (fname, " of ");
	}
	output_local ("\n/* %s%s ", fname, r->name);
	if (f->report_source_id) {
		output_local ("SourceId:%d ", f->report_source_id);
	}
	if ((f->report_flag & COB_REPORT_LINE)
	&& f->children
	&& (f->children->report_flag & COB_REPORT_LINE)) {
		printf("Warning: Ignoring nested LINE %s %d\n",
			(f->report_flag & COB_REPORT_LINE_PLUS) ? "PLUS" : "",
			f->report_line);
		f->report_line = 0;
		f->report_flag &= ~COB_REPORT_LINE_PLUS;
		f->report_flag &= ~COB_REPORT_LINE;
	}
	if (f->report_flag & COB_REPORT_LINE)
		output_local ("LINE %s %d ",
			(f->report_flag & COB_REPORT_LINE_PLUS) ? "PLUS" : "",
			f->report_line);
	output_local ("*/\n");
	strcpy (report_field_name, "NULL");
	if ((f->report_flag & COB_REPORT_LINE)
	 && f->children != NULL) {
		output_report_def_fields (1, f->id, f->children, r);
	} else if (f->children == NULL) {
		if (f->report_flag & COB_REPORT_LINE) {
			output_report_def_fields (1, f->id, f, r);
		}
	}
	if (f->report_decl_id
	 && f->report_source_id == 0) {
		f->report_source_id = ++r_source_id;
	}
	if (f->report_source_id == 0
	 && (f->report_flag & COB_REPORT_LINE)) {
		int		id = any_source_moves (r, f, 1);
		if (id > 0) {
			for (p = f->parent; p; p = p->parent) {
				if (p->report_source_id) {
					f->report_source_id = p->report_source_id;
					break;
				}
			}
			if (f->report_source_id == 0) {
				f->report_source_id = id;
			}
		}
	}
	output_local ("static cob_report_line  %s%d\t= {", CB_PREFIX_REPORT_LINE, f->id);
	if (n == NULL) {
		output_local ("NULL,");
	} else if (n->level > 1
	 && !(n->report_flag & COB_REPORT_LINE)) {
		output_local ("NULL, ");
	} else {
		output_local ("&%s%d,", CB_PREFIX_REPORT_LINE, n->id);
	}
	if (c == NULL)
		output_local ("NULL,");
	else
		output_local ("&%s%d,", CB_PREFIX_REPORT_LINE, c->id);
	output_local ("%s,", report_field_name);
	if (f->report_control) {
		output_param (f->report_control, 0);
	} else {
		output_local ("NULL");
	}
	output_local (",&%s%d", CB_PREFIX_FIELD, f->id);
	output_local (",%d", f->report_decl_id);
	output_local (",0x%X,%d,%d,%d",f->report_flag&~COB_REPORT_EMITTED,
				 f->report_line, f->step_count, f->next_group_line);
	output_local (",0x%X,0", f->report_flag&~COB_REPORT_EMITTED);
	output_local (", %d", f->report_source_id);
	output_local (",%5d", f->common.source_line);
	output_local ("};\n");
}

static int sum_prv = 0;
static int sum_nxt = 0;

/* Find data field for given internal SUM counter */
struct cb_field *
get_sum_data_field (struct cb_report *r, struct cb_field *f)
{
	int	k;
	for (k=0; k < r->num_sums; k++) {
		if (r->sums[k*2 + 0] == f) {
			return r->sums[k*2 + 1];
		}
		if (r->sums[k*2 + 1] == f) {
			return r->sums[k*2 + 0];
		}
	}
	return NULL;
}

/*
 * Generate list of SUM counters
 */
static void
output_report_sum_counters (const int top, struct cb_field *f, struct cb_report *r)
{
	struct cb_field *n, *c, *p, *z;
	cb_tree	l, x;
	char	fname[64];
	int	ctl_foot, sub_ttl, cross_foot, computed;

	n = f->sister;
	c = f->children;
	if (n
	 && n->storage != CB_STORAGE_REPORT)
		n = NULL;
	if (n
	 && n->report != r)
		n = NULL;
	if (c
	 && c->storage != CB_STORAGE_REPORT)
		c = NULL;
	if (n) {
		output_report_sum_counters (top, n, r);
	}
	if (c) {
		output_report_sum_counters (0, c, r);
	}
	if (!top) {
		c = NULL;
	}

	if (f->report_sum_list == NULL)
		return;
	if (f->report_flag & COB_REPORT_SUM_EMITTED)	/* Was this already emitted? */
		return;
	f->report_flag |= COB_REPORT_SUM_EMITTED;

	if (f->flag_filler) {
		if (f->report_flag & COB_REPORT_PAGE_HEADING) {
			strcpy (fname, "PAGE HEADING");
		} else if (f->report_flag & COB_REPORT_PAGE_FOOTING) {
			strcpy (fname, "PAGE HEADING");
		} else if (f->report_flag & COB_REPORT_CONTROL_HEADING) {
			strcpy (fname, "CONTROL HEADING");
		} else if (f->report_flag & COB_REPORT_CONTROL_FOOTING) {
			strcpy (fname, "CONTROL FOOTING");
		} else if (f->report_flag & COB_REPORT_CONTROL_FOOTING_FINAL) {
			strcpy (fname, "CONTROL FOOTING FINAL");
		} else if (f->report_flag & COB_REPORT_CONTROL_HEADING_FINAL) {
			strcpy (fname, "CONTROL HEADING FINAL");
		} else {
			strcpy (fname, "");
		}
		if (f->report_control) {
			sprintf (&fname[strlen (fname)], " %s", cb_code_field (f->report_control)->name);
		}
	} else if (memcmp (f->name, "FILLER ", 7) == 0) {
		sprintf (fname, "Source %d", f->common.source_line);
	} else {
		sprintf (fname, "%s", f->name);
	}
	output_local ("\n/* %s SUM ", fname);
	for (l = f->report_sum_list; l; l = CB_CHAIN (l)) {
		x = CB_VALUE (l);
		if (CB_REF_OR_FIELD_P (x)) {
			output_local ("%s ", cb_code_field(x)->name);
		} else
		if (CB_NUMERIC_LITERAL_P (x)) {
			output_local ("%s ", cb_name (x));
		} else
		if (CB_BINARY_OP_P (x)) {
			output_local ("%s ", cb_name (x));
		}
	}
	if (f->report_flag & COB_REPORT_RESET_FINAL)
		output_local (" RESET ON FINAL ");
	if (f->report_reset) {
		output_local (" RESET ON %s ", cb_code_field (f->report_reset)->name);
	}
	output_local ("*/\n");
	ctl_foot = sub_ttl = cross_foot = computed = 0;
	x = NULL;
	z = NULL;
	x = CB_VALUE (f->report_sum_list);
	if (CB_REF_OR_FIELD_P (x)) {
		z = get_sum_data_field (r, cb_code_field (x));
		if (z) {
			sub_ttl = 1;
		}
	} else
	if (!CB_LITERAL_P (x)) {
		if (CB_PURPOSE (f->report_sum_list)) {
			x = CB_PURPOSE (f->report_sum_list);
			computed = 1;
		}
	}

	output_local ("static cob_report_sum_ctr %s%d = {", CB_PREFIX_REPORT_SUM_CTR, ++sum_nxt);
	if (sum_prv) {
		output_local ("&%s%d,", CB_PREFIX_REPORT_SUM_CTR, sum_prv);
	} else {
		output_local ("NULL,");
	}
	output_local ("\"%s\",", fname);
	if (CB_LITERAL_P (x)) {
		output ("(cob_field *)&%s%d,", CB_PREFIX_CONST, cb_lookup_literal (current_prog, x, 0));
	} else
	if (x) {
		output_local ("&%s%d,", CB_PREFIX_FIELD, cb_code_field(x)->id);
	} else {
		output_local ("/* NO SUM! */NULL,");
	}
	if (f->report_sum_counter) {
		output_local ("&%s%d,",CB_PREFIX_FIELD, cb_code_field (f->report_sum_counter)->id);
		z = get_sum_data_field (r, cb_code_field (f->report_sum_counter));
	} else {
		output_local ("NULL,");
		z = NULL;
	}
	if (z) {
		output_local ("&%s%d,", CB_PREFIX_FIELD, z->id);
	} else {
		output_local ("NULL,");
	}
	for (p = f; p; p = p->parent) {
		if (p->report_control) {
			output_local ("&%s%d_%d,",
				CB_PREFIX_REPORT_CONTROL, r_ctl_id, cb_code_field (p->report_control)->id);
			break;
		} else if (p->report_flag & COB_REPORT_CONTROL_FOOTING_FINAL) {
			ctl_foot = 1;
			output_local ("NULL,");
			break;
		}
	}
	if (p == NULL) {
		output_local ("NULL,");
	}
	if (f && f->report_flag & COB_REPORT_RESET_FINAL) {
		output_local ("1");
	} else {
		output_local ("0");
	}
	output_local (",%d,%d,%d,%d", ctl_foot, sub_ttl, cross_foot, computed);
	output_local ("};\n");
	sum_prv = sum_nxt;
}

static void
output_report_definition (struct cb_report *p, struct cb_report *n)
{
	int	i;
	struct cb_field *s = NULL;
	cb_tree	l;

	output_local ("\n");
	for (i= p->num_lines - 1; i >= 0; i--) {
		if (p->line_ids[i]->level == 1)
			output_report_define_lines (1, p, p->line_ids[i], 0);
	}
	output_local ("\n");
	if (p->controls) {
		for (l = p->controls; l; l = CB_CHAIN (l)) {
			s = cb_code_field (l);
			s->count++;
		}
		output_report_control (p, ++r_ctl_id, p->controls, CB_CHAIN (p->controls));
		output_local ("\n");
	}
	sum_prv = 0;
	for (i= p->num_lines - 1; i >= 0; i--) {
		if (p->line_ids[i]->level == 1) {
			output_report_sum_counters (1, p->line_ids[i], p);
		}
	}

	output_local ("\n");
	output_local ("static cob_report %s%s = {0x%X,\t",
							CB_PREFIX_REPORT, p->cname, COB_REPORT_VERSION);
	output_local ("\"%s\",\n\t\t", p->name);
	if (n != NULL) {
		output_local ("&%s%s,", CB_PREFIX_REPORT, n->cname);	/* next report */
	} else {
		output_local ("NULL,");
	}
	output_local ("0,");	/* 'go_label' number for call backs */
	output_local ("NULL,");	/* report file (address set at run-time) */
	if (p->page_counter) {
		output_param (p->page_counter, 0);
		output_local (",");
	} else {
		output_local ("NULL,");
	}
	if (p->line_counter) {
		output_param (p->line_counter, 0);
		output_local (",");
	} else {
		output_local ("NULL,");
	}
	if (p->num_lines > 0) {
		output_local ("&%s%d,", CB_PREFIX_REPORT_LINE, p->line_ids[0]->id);
	} else {
		output_local ("NULL,");
	}
	if (p->t_heading_final != NULL) {
		output_local ("&%s%d,", CB_PREFIX_REPORT_LINE, p->t_heading_final->id);
	} else {
		output_local ("NULL,");
	}
	if (p->t_footing_final != NULL) {
		output_local ("&%s%d,", CB_PREFIX_REPORT_LINE, p->t_footing_final->id);
	} else {
		output_local ("NULL,");
	}
	if (p->controls) {
		s = cb_code_field(p->controls);
		output_local ("&%s%d_%d,", CB_PREFIX_REPORT_CONTROL, r_ctl_id, s->id);
	} else {
		output_local ("NULL,");
	}
	if (sum_prv > 0) {
		output_local ("&%s%d,", CB_PREFIX_REPORT_SUM_CTR, sum_prv);
	} else {
		output_local ("NULL,");
	}
	output_local (" %d,\n", p->sum_exec);
	output_local ("\t\t%d,%d,%d,%d,%d,%d,%d,\n",
			p->lines, p->columns, p->heading,
			p->first_detail, p->last_control,
			p->last_detail, p->footing);
	output_local ("\t\t0,0,0,0,0,");
	output_local ("%d,%d\n", p->control_final, p->global);
	output_local ("};\n");
}

static void
output_report_list (cb_tree l, cb_tree n)
{
	cb_tree nl;
	struct cb_report	*rep, *nxrep;

	if (CB_LIST_P (l))
		rep = CB_REPORT_PTR (CB_VALUE(l));
	else
		rep = CB_REPORT_PTR (l);
	if (n != NULL) {
		if (CB_LIST_P (l))
			nxrep = CB_REPORT_PTR (CB_VALUE(n));
		else
			nxrep = CB_REPORT_PTR (l);
	} else {
		nxrep = NULL;
	}
	nl = CB_CHAIN (l);
	output_emit_field (rep->line_counter, NULL);
	output_emit_field (rep->page_counter, NULL);
	if (nl) {
		output_report_list (nl, CB_CHAIN (nl));
	}
	output_report_definition (rep, nxrep);
}

static void
output_report_init (struct cb_report *rep)
{
	output_prefix ();
	output ("cob_set_report (&%s%s, ", CB_PREFIX_REPORT, rep->cname);

	if (rep->file) {
		output ("%s%s", CB_PREFIX_FILE, rep->file->cname);
	} else {
		output ("NULL");
	}
	output (");");
	output_newline ();

}

static int
any_source_moves (struct cb_report *r, struct cb_field *f, int first)
{
	cb_tree	x, l;
	if (f->report_decl_id != 0
	 && f->report_source_id == 0) {
		f->report_source_id = ++r_source_id;
	}
	if (f->report_source_id != 0)
		return f->report_source_id;
	if (f->report_source
	 && CB_BINARY_OP_P (f->report_source))
		return ++r_source_id;
	if (f->report_field_from == NULL
	&& (f->report_vary_list
	 || f->report_source)) {
		return ++r_source_id;
	}
	for (l = f->report_sum_list; l; l = CB_CHAIN (l)) {
		x = CB_VALUE (l);
		if (CB_BINARY_OP_P (x)) {
			if (r->sum_exec == 0)
				r->sum_exec = ++r_source_id;
		}
	}
	if (f->children) {
		int id = any_source_moves (r, f->children, 0);
		if (id)
			return id;
	}
	if (f->sister && !first) {
		return any_source_moves (r, f->sister, 0);
	}
	return 0;
}

static void
output_report_source (struct cb_report *rep)
{
	struct cb_field	*f, *c;

	for (f=rep->records; f; f = f->sister) {
		f->report_source_id = any_source_moves (rep, f, 1);
		if (f->report_source_id != 0
		 && !(f->report_flag & COB_REPORT_LINE)) {
			for (c = f->children; c; c = c->children) {
				if ((c->report_flag & COB_REPORT_LINE)
				 && c->report_source_id == 0) {
					c->report_source_id = f->report_source_id;
					break;
				}
			}
		}
	}
}

static int	report_nest_vary = 0;
static struct cb_field *rpt_idx[COB_MAX_SUBSCRIPTS+1];

static cb_tree
build_field_sub (struct cb_field *f)
{
	int		sub;
	cb_tree	x = cb_build_field_reference (f, NULL);
	struct cb_reference	*r = CB_REFERENCE (x);
	if (report_nest_vary > 0) {
		r->subs = CB_LIST_INIT (cb_build_field_reference (rpt_idx[0], NULL));
		for (sub = 1; sub < report_nest_vary; sub++) {
			cb_list_add (r->subs, cb_build_field_reference (rpt_idx[sub], NULL));
		}
	}
	return x;
}

static void
output_report_move_sums (struct cb_field *f, int first)
{
	cb_tree	x, l;
	int		nested;
	for (l = f->report_sum_list; l; l = CB_CHAIN (l)) {
		x = CB_VALUE (l);
		if (CB_BINARY_OP_P (x)) {
			output_line ("/* Compute %s */", cb_name (x));
			output_prefix ();
			output ("cob_move (");
			output_param (x, -1);
			output (", ");
			output_param (CB_PURPOSE (l), -1);
			output (");");
			output_newline ();
		}
	}
	if (f->children) {
		nested = report_nest_vary;
		output_report_move_sums (f->children, 0);
		while (report_nest_vary > 0
			&& report_nest_vary >= nested) {
			output_block_close ();
			report_nest_vary--;
		}
	}
	if (f->sister && !first) {
		output_report_move_sums (f->sister, 0);
	}
}

static void
output_report_move_source (struct cb_field *f, int first)
{
	struct cb_field *var;
	struct cb_vary	*vry;
	cb_tree	l;
	int		i, nested;
	if (f->report_vary_list) {
		output_block_open ();
		output_line ("int  ix_%d;", f->id);
		for (l = f->report_vary_list, i=0; l; l = CB_CHAIN (l), i++) {
			output_prefix ();
			vry = CB_VARY (CB_VALUE (l));
			var = cb_code_field (vry->var);
			rpt_idx[report_nest_vary] = var;
			output ("%s%d = ", CB_PREFIX_BASE,var->id);
			if (vry->from) {
				output_integer (vry->from);
			} else {
				output ("1");
			}
			output (";\t\t");
			output ("/* VARYING %s ", var->name);
			output ("(%s%d) ", CB_PREFIX_BASE, var->id);
			if (vry->from) {
				output ("FROM %s ", cb_name (vry->from));
			} else {
				output ("FROM 1 ");
			}
			if (vry->by) {
				output ("BY %s ", cb_name (vry->by));
			} else {
				output ("BY 1 ");
			}
			output ("*/");
			output_newline ();
		}
		output_prefix ();
		output ("for (ix_%d = 1; ix_%d <= ", f->id, f->id);
		if (f->depending)
			output_integer (f->depending);
		else
			output ("%d", f->occurs_max);
		output ("; ix_%d++",f->id);
		for (l = f->report_vary_list; l; l = CB_CHAIN (l)) {
			output (", ");
			vry = CB_VARY (CB_VALUE (l));
			var = cb_code_field (vry->var);
			output ("%s%d += ", CB_PREFIX_BASE, var->id);
			if (vry->by) {
				output_integer (vry->by);
			} else {
				output ("1");
			}
		}
		output (")");
		report_nest_vary++;
		output_newline ();
		output_block_open ();
	}
	if (f->report_source) {
		if (f->report_source_txt == NULL) {
			output_line ("/* Move To %s */",
				get_field_name (f));
		} else {
			output_line ("/* Move %s To %s */", f->report_source_txt,
				get_field_name (f));
		}
		stack_id = 0;
		output_prefix ();
		output ("cob_move (");
		output_param (f->report_source, -1);
		output (", ");
		if (f->report_from) {
			output_param (f->report_from, -1);
		} else {
			output_param (build_field_sub (f), -1);
		}
		output (");");
		output_newline ();
	}
	if (f->children) {
		nested = report_nest_vary;
		output_report_move_source (f->children, 0);
		while (report_nest_vary > 0
			&& report_nest_vary >= nested) {
			output_block_close ();
			report_nest_vary--;
		}
	}
	if (f->sister && !first) {
		output_report_move_source (f->sister, 0);
	}
	if (f->report_vary_list) {
		output_block_close ();
	}
}

static void
output_report_source_move (struct cb_report *rep)
{
	struct cb_field	*f;
	int		first = 1;
	char	wrk[64];

	if (rep->sum_exec) {
		output_line ("/* Compute values for report %s */", rep->cname);
		output_indent_level = 0;
		output_line ("rw_src_%d: ",rep->id);
		output_indent_level = 4;
		output_line ("switch (%s%s.exec_source)", CB_PREFIX_REPORT, rep->cname);
		output_block_open ();
		first = 0;
		output_line ("case %d: /* Compute SUMs */", rep->sum_exec);
		output_indent_level += 2;
		for (f=rep->records; f; f = f->sister) {
			output_report_move_sums (f, 1);
		}
		output_line ("break;");
		output_newline ();
		output_indent_level -= 2;
	}

	for (f=rep->records; f; f = f->sister) {
		if (f->report_decl_id != 0
		 && f->report_source_id == 0) {
			f->report_source_id = ++r_source_id;
		}
		if (f->report_source_id) {
			rep->has_source_move = 1;
			if (first) {
				output_line ("/* Set SOURCE values for report %s */", rep->cname);
				output_indent_level = 0;
				output_line ("rw_src_%d: ",rep->id);
				output_indent_level = 4;
				output_line ("switch (%s%s.exec_source)", CB_PREFIX_REPORT, rep->cname);
				output_block_open ();
				first = 0;
			}
			if (f->occurs_max > 1) {
				sprintf (wrk, " OCCURS %d", f->occurs_max);
			} else {
				wrk[0] = 0;
			}
			output_line ("case %d: /* Set SOURCE for line %d: %s%s */",
				f->report_source_id, f->common.source_line, f->name, wrk);
			output_indent_level += 2;
			if (f->report_decl_id) {
				output_line ("frame_ptr++;\t/* PERFORM Declaratives Before */");
				output_line ("frame_ptr->perform_through = %d;", f->report_decl_id);
				perform_label (CB_PREFIX_LABEL, f->report_decl_id, -1);
				output_line ("frame_ptr--;");
			}
			report_nest_vary = 0;
			output_report_move_source (f, 1);
			while (report_nest_vary > 0) {
				output_block_close ();
				report_nest_vary--;
			}
			output_line ("break;");
			output_newline ();
			output_indent_level -= 2;
		}
	}
	if (first) {
		/* Nothing really needed but emit label and return code */
		rep->has_source_move = 0;
		output_indent_level = 0;
		output_line ("    /* No SOURCE to handle for %s */", rep->cname);
		output_line ("rw_src_%d: ", rep->id);
	} else {
		/* Finish off switch */
		output_line ("default:");
		output_line ("  break;");
		output_block_close ();
		output_line ("%s%s.exec_source = -%s%s.exec_source;",
						CB_PREFIX_REPORT, rep->cname,
						CB_PREFIX_REPORT, rep->cname);
	}
	output_indent_level = 4;
	if (!cb_flag_computed_goto) {
		output_line ("goto P_switch;");
	} else {
		output_line ("goto *frame_ptr->return_address_ptr;");
	}
	output_indent_level = 2;
}

/* Alphabet-name */

static void
output_alphabet_name_definition (struct cb_alphabet_name *p)
{
	const int is_national_alphabet = p->alphabet_target == CB_ALPHABET_NATIONAL;
	const int maxchar = is_national_alphabet
						? COB_MAX_CHAR_NATIONAL : COB_MAX_CHAR_ALPHANUMERIC;
	const int size = is_national_alphabet
						? (COB_MAX_CHAR_NATIONAL + 1) * COB_NATIONAL_SIZE : COB_MAX_CHAR_ALPHANUMERIC + 1;
	int		i;

	if (p->alphabet_type != CB_ALPHABET_CUSTOM) {
		return;
	}

	/* Output the table */
	output_local ("static const unsigned char %s%s[%d] = {\n",
		      CB_PREFIX_SEQUENCE, p->cname, size);
	i = 0;
	for (i = 0; ; i++) {
		if (is_national_alphabet) {
			/* FIXME: this isn't tested at all and likely needs
			   adjustments in the runtime */
			output_local (" 0x%02x, 0x%02x",
				(p->values[i] >> 8) & 0xFF, p->values[i] & 0xFF);
		} else {
			output_local (" %d", p->values[i]);
		}
		if (i == maxchar) {
			output_local ("};\n");
			break;
		}
		if (i % 16 == 15) {
			output_local (",\n");
		} else {
			output_local (",");
		}
	}
	i = lookup_attr (is_national_alphabet ? COB_TYPE_NATIONAL : COB_TYPE_ALPHANUMERIC,
			0, 0, 0, NULL, 0);
	output_local ("static cob_field %s%s = { %d, (cob_u8_ptr)%s%s, &%s%d };\n",
		CB_PREFIX_FIELD, p->cname,
		size,
		CB_PREFIX_SEQUENCE, p->cname,
		CB_PREFIX_ATTR, i);
	output_local ("\n");
}

/* Class definition */

static void
output_class_name_definition (struct cb_class_name *p)
{
	cb_tree		l;
	cb_tree		x;
	unsigned char	*data;
	size_t		i;
	size_t		size;
	int		n;
	int		lower;
	int		upper;
	int		vals[256];

	output_line ("static int");
	output_line ("%s (cob_field *f)", p->cname);
	output_block_open ();
	output_line ("size_t\ti;");
	output_newline ();
	output_line ("for (i = 0; i < f->size; i++)");
	output_block_open ();
	output_line ("switch (f->data[i]) {");
	memset (vals, 0, sizeof(vals));
	for (l = p->list; l; l = CB_CHAIN (l)) {
		x = CB_VALUE (l);
		if (CB_PAIR_P (x)) {
			lower = cb_literal_value (CB_PAIR_X (x));
			upper = cb_literal_value (CB_PAIR_Y (x));
			for (n = lower; n <= upper; ++n) {
				vals[n] = 1;
			}
		} else {
			if (CB_NUMERIC_LITERAL_P (x)) {
				vals[cb_literal_value (x)] = 1;
			} else if (x == cb_space) {
				vals[' '] = 1;
			} else if (x == cb_zero) {
				vals['0'] = 1;
			} else if (x == cb_quote) {
				if (cb_flag_apostrophe) {
					vals['\''] = 1;
				} else {
					vals['"'] = 1;
				}
			} else if (x == cb_null) {
				vals[0] = 1;
			} else if (x == cb_low) {
				vals[0] = 1;
			} else if (x == cb_high) {
				vals[255] = 1;
			} else {
				size = CB_LITERAL (x)->size;
				data = CB_LITERAL (x)->data;
				for (i = 0; i < size; i++) {
					vals[data[i]] = 1;
				}
			}
		}
	}
	for (i = 0; i < 256; ++i) {
		if (vals[i]) {
			output_line ("case %d:", (int)i);
		}
	}
	output_line ("    break;");
	output_line ("default:");
	output_line ("    return 0;");
	output_line ("}");
	output_block_close ();
	output_line ("return 1;");
	output_block_close ();
	output_newline ();
}

static void
output_class_names (struct cb_program *prog)
{
	cb_tree	l;

	if (!prog->nested_level && prog->class_name_list) {
		output_line ("/* Class names */");
		for (l = prog->class_name_list; l; l = CB_CHAIN (l)) {
			output_class_name_definition (CB_CLASS_NAME (CB_VALUE (l)));
		}
	}
}

static void
output_initial_values (struct cb_field *f)
{
	struct cb_field	*p;
	cb_tree		x;

	for (p = f; p; p = p->sister) {
		if (p->flag_item_based
		 || p->flag_external
		 || p->flag_is_typedef
		 || p->level == 99) {
			continue;
		}
		/* For special registers */
		if (p->flag_no_init && !p->count) {
			continue;
		}
		/* note: the initial value of INDEXED BY items is undefined per standard,
		   but earlier versions always set this explict to 1 on first entry;
		   we now make this depending on its value, set depending on cb_init_indexed_by
		   and on cb_implicit_init */
		if (p->flag_indexed_by && cb_default_byte == CB_DEFAULT_BYTE_NONE) {
			continue;
		}
		x = cb_build_field_reference (p, NULL);
		output_line ("/* initialize field %s */", f->name);
		output_stmt (cb_build_initialize (x, cb_true, NULL, 1, STMT_INIT_STORAGE, 0));
		output_newline ();
	}
}

const char *
cb_open_mode_to_string (const enum cob_open_mode mode)
{
	switch (mode) {
	case COB_OPEN_CLOSED:	return CB_XSTRINGIFY (COB_OPEN_CLOSED);
	case COB_OPEN_INPUT:	return CB_XSTRINGIFY(COB_OPEN_INPUT);
	case COB_OPEN_OUTPUT:	return CB_XSTRINGIFY (COB_OPEN_OUTPUT);
	case COB_OPEN_I_O:	return CB_XSTRINGIFY (COB_OPEN_I_O);
	case COB_OPEN_EXTEND:	return CB_XSTRINGIFY (COB_OPEN_EXTEND);
	case COB_OPEN_LOCKED:	return CB_XSTRINGIFY (COB_OPEN_LOCKED);
	}
	/* LCOV_EXCL_START */
	cobc_err_msg ("unexpected cob_open_mode");
	COBC_ABORT ();
	/* LCOV_EXCL_STOP */
}

static void
output_error_handler (struct cb_program *prog)
{
	struct handler_struct	*hstr;
	size_t			seen;
	unsigned int	parameter_count, pcounter, hcounter;

	recent_prog = prog;
	output_newline ();
	seen = 0;
	for (hcounter = COB_OPEN_INPUT; hcounter <= COB_OPEN_EXTEND; hcounter++) {
		if (prog->global_handler[hcounter].handler_label) {
			seen = 1;
			break;
		}
	}
	skip_line_num = 0;
	output_stmt (cb_standard_error_handler);
	skip_line_num = 1;
	output_newline ();
	if (seen) {
		output_line ("switch (cob_glob_ptr->cob_error_file->last_open_mode)");
		output_block_open ();
		for (hcounter = COB_OPEN_INPUT; hcounter <= COB_OPEN_EXTEND; hcounter++) {
			hstr = &prog->global_handler[hcounter];
			if (hstr->handler_label) {
				output_line ("case %s:", cb_open_mode_to_string (hcounter));
				output_block_open ();
				if (prog == hstr->handler_prog) {
					output_perform_call (hstr->handler_label, NULL);
				} else {
					output_prefix ();
					if (hstr->handler_prog->nested_level) {
						output ("%s_%d_ (%d",
							hstr->handler_prog->program_id,
							hstr->handler_prog->toplev_count,
							hstr->handler_label->id);
					} else {
						output ("%s_ (%d",
							hstr->handler_prog->program_id,
							hstr->handler_label->id);
					}
					parameter_count = cb_list_length (hstr->handler_prog->parameter_list);
					for (pcounter = 0; pcounter < parameter_count; pcounter++) {
						output (", NULL");
					}
					output (");");
					output_newline ();
				}
				output_line ("break;");
				output_block_close ();
			}
		}
		output_line ("default:");
		output_block_open ();
	}
	output_line ("if (!(cob_glob_ptr->cob_error_file->flag_select_features & COB_SELECT_FILE_STATUS)) {");
	output_line ("\tcob_fatal_error (COB_FERROR_FILE);");
	if (cb_standard_error_handler) {		/* Emit 'goto' to avoid unreferenced label C warning */
		output_line ("    goto %s%d;", CB_PREFIX_LABEL,
				     CB_LABEL (cb_standard_error_handler)->id);
	}
	output_line ("}");
	if (seen) {
		output_line ("break;");
		output_block_close ();
		output_block_close ();
	}
	output_perform_exit (CB_LABEL (cb_standard_error_handler));
	output_newline ();
	output_line ("/* Fatal error if reached */");
	output_line ("cob_fatal_error (COB_FERROR_CODEGEN);");
	output_newline ();
}

static void
output_module_register_init (cb_tree reg, const char *name)
{
	if (!reg) {
		output_line ("module->%s = NULL;", name);
		return;
	}

	if (CB_REFERENCE_P (reg)) {
		reg = cb_ref (reg);
	}
	if (CB_FIELD_P (reg) && !CB_FIELD (reg)->count) {
		output_line ("module->%s = NULL;", name);
		return;
	}

	output_prefix ();
	output ("module->%s = ", name);
	output_param (reg, -1);
	output (";");
	output_newline ();
}

/* outputs function for setting variables in the module structure
   that reference static values */
static void
output_module_init_function (struct cb_program *prog)
{
	output_indent_level = 0;
	if (!prog->nested_level) {
		output_line ("/* Initialize module structure for %s */",
			prog->orig_program_id);
		output_line ("static void %s_module_init (cob_module *module)",
			prog->program_id);
	} else {
		output_line ("/* Initialize module structure for %s (nested %d) */",
			prog->program_id, prog->toplev_count);
		output_line ("static void %s_%d_module_init (cob_module *module)",
			prog->program_id, prog->toplev_count);
	}
	output_block_open ();

#if	0  /* Module comments, maybe extend and only include if
          explicit requested via flag (auto-active for -c + -g)? */
	output_line ("/* Next pointer, Parameter list pointer, Module name, */");
	output_line ("/* Module formatted date, Module source, */");
	output_line ("/* Module entry, Module cancel, */");
	output_line ("/* Collating, CRT status, CURSOR, */");
	output_line ("/* Module reference count, Module path, Module active, */");
	output_line ("/* Module date, Module time, */");
	output_line ("/* Module type, Number of USING parameters, Return type */");
	output_line ("/* Current parameter count */");
	output_line ("/* Display sign, Decimal point, Currency symbol, */");
	output_line ("/* Numeric separator, File name mapping, Binary truncate, */");
	output_line ("/* Alternate numeric display, Host sign, No physical cancel */");
	output_line ("/* Flag main program, Fold call, Exit after CALL */");
#endif

	recent_prog = prog;
	/* Do not initialize next pointer, parameter list pointer + count */
	output_line ("module->module_name = \"%s\";", prog->orig_program_id);
	output_line ("module->module_formatted_date = COB_MODULE_FORMATTED_DATE;");
	output_line ("module->module_source = COB_SOURCE_FILE;");
	output_line ("module->gc_version = COB_PACKAGE_VERSION;");
	if (!prog->nested_level) {
		output_line ("module->module_entry.funcptr = (void *(*)())%s;",
			     prog->program_id);
		if (prog->prog_type == COB_MODULE_TYPE_FUNCTION) {
			output_line ("module->module_cancel.funcptr = NULL;");
		} else {
			output_line ("module->module_cancel.funcptr = (void *(*)())%s_;",
				     prog->program_id);
		}
	} else {
		output_line ("module->module_entry.funcvoid = NULL;");
		output_line ("module->module_cancel.funcvoid = NULL;");
	}

	if (!cobc_flag_main && non_nested_count > 1) {
		output_line ("module->module_ref_count = &cob_reference_count;");
	} else {
		output_line ("module->module_ref_count = NULL;");
	}
	output_line ("module->module_path = &cob_module_path;");
	output_line ("module->module_active = 0;");
	output_line ("module->module_date = COB_MODULE_DATE;");
	output_line ("module->module_time = COB_MODULE_TIME;");
	output_line ("module->module_type = %u;", prog->prog_type);
	output_line ("module->module_param_cnt = %u;", prog->num_proc_params);
#if 0 /* currently not checked anywhere, may use for void or more general type */
	output_line ("module->module_returning = %u;", prog->flag_void ? 0 : 1);
#endif
	output_line ("module->ebcdic_sign = %d;", cb_ebcdic_sign);
	output_line ("module->decimal_point = '%c';", prog->decimal_point);
	output_line ("module->currency_symbol = '%c';", prog->currency_symbol);
	output_line ("module->numeric_separator = '%c';", prog->numeric_separator);
	output_line ("module->flag_filename_mapping = %d;", cb_filename_mapping);
	output_line ("module->flag_binary_truncate = %d;", cb_binary_truncate);
	output_line ("module->flag_pretty_display = %d;", cb_pretty_display);
	output_line ("module->flag_host_sign = %d;", cb_host_sign);
	output_line ("module->flag_no_phys_canc = %d;", no_physical_cancel);
	output_line ("module->flag_main = %d;", cobc_flag_main);
	output_line ("module->flag_fold_call = %d;", cb_fold_call);
	output_line ("module->flag_dialect = COB_DIALECT_%s;", cb_dialect);
	if (cb_mf_files
	 && cb_std_define != CB_STD_85) {	/* Not if cobol85 test suite */
		output_line ("module->flag_file_format = COB_FILE_IS_MF;");
	} else {
		output_line ("module->flag_file_format = COB_FILE_IS_DFLT;");
	}
	output_line ("module->flag_exit_program = 0;");
	{
		int	opt = 0;
		if (cb_flag_traceall) {
			opt |= COB_MODULE_TRACE;
			opt |= COB_MODULE_TRACEALL;
		} else
		if (cb_flag_trace) {
			opt |= COB_MODULE_TRACE;
		}
#if 0 /* currently unused */
		if (cobc_wants_debug
		 || cb_flag_dump) {
			opt |= COB_MODULE_DEBUG;
		}
#endif
		output_line ("module->flag_debug_trace |= %d;", opt);
	}
	output_line ("module->flag_dump_sect = 0x%02X;", cb_flag_dump);
	output_line ("module->flag_dump_ready = %u;", cb_flag_dump ? 1 : 0);
	output_line ("module->xml_mode = %u;", cb_xml_parse_xmlss);
	output_line ("module->module_stmt = 0;");
	if (source_cache) {
		output_line ("module->module_sources = %ssource_files;",
			CB_PREFIX_STRING);
	} else {
		output_line ("module->module_sources = NULL;");
	}

	output_block_close ();
	output_newline ();
}

/* outputs code for setting variables in the module structure
   that reference non-static values */
static void
output_module_init_non_static (struct cb_program *prog)
{
	output_module_register_init (prog->collating_sequence, "collating_sequence");
	if (prog->crt_status && cb_code_field (prog->crt_status)->count) {
		output_prefix ();
		output ("module->crt_status = ");
		output_param (cb_ref (prog->crt_status), -1);
		output (";");
		output_newline ();
	} else {
		output_line ("module->crt_status = NULL;");
	}

	/* TODO for later: Check for possible implementation without adding a multitude
	   of module local registers to cob_module structure */
	output_module_register_init (prog->cursor_pos, "cursor_pos");

	output_module_register_init (CB_TREE (prog->xml_code), "xml_code");
	output_module_register_init (CB_TREE (prog->xml_event), "xml_event");
	output_module_register_init (CB_TREE (prog->xml_information), "xml_information");
	output_module_register_init (CB_TREE (prog->xml_namespace), "xml_namespace");
	output_module_register_init (CB_TREE (prog->xml_namespace_prefix), "xml_namespace_prefix");
	output_module_register_init (CB_TREE (prog->xml_nnamespace), "xml_nnamespace");
	output_module_register_init (CB_TREE (prog->xml_nnamespace_prefix), "xml_nnamespace_prefix");
	output_module_register_init (CB_TREE (prog->xml_ntext), "xml_ntext");
	output_module_register_init (CB_TREE (prog->xml_text), "xml_text");

	output_module_register_init (CB_TREE (prog->json_code), "json_code");
	output_module_register_init (CB_TREE (prog->json_status), "json_status");
}

static void
output_module_init (struct cb_program *prog)
{
	if (!prog->nested_level) {
		output_line ("%s_module_init (module);",
			prog->program_id);
	} else {
		output_line ("%s_%d_module_init (module);",
			prog->program_id, prog->toplev_count);
	}
	output_newline ();
	output_module_init_non_static (prog);
	output_newline ();
}

/* Common setup logic for pickup_param routines */
static struct cb_field *
setup_param (cb_tree l, int *is_value_parm, int *is_any_numeric)
{
	struct cb_field *f;

	f = cb_code_field (CB_VALUE (l));
	if (CB_PURPOSE (l)
	 && (CB_PURPOSE_INT (l) == CB_CALL_BY_VALUE
	  || CB_PURPOSE_INT (l) == CB_CALL_BY_CONTENT)) {
		*is_value_parm = 1;
	} else {
		*is_value_parm = 0;
	}
	if (f->flag_any_numeric
	 || (f->pic && f->pic->category == CB_CATEGORY_NUMERIC)) {
		*is_any_numeric = 1;
	} else {
		*is_any_numeric = 0;
	}

	/* Force PROCEDURE/ENTRY USING fields to cache */
	if (!f->flag_field) {
		force_cache (f);
	}

	return f;
}

/* Set given parameter address to NULL */
static void
set_param_to_null (cb_tree l)
{
	struct cb_field *f;
	f = cb_code_field (CB_VALUE (l));

	output_line ("%s%d.data = NULL;", CB_PREFIX_FIELD, f->id);
	return;
}

/* Pickup parameter knowing the caller is COBOL */
static void
pickup_cob_param (cb_tree l, cob_u32_t inc)
{
	char	wrk[64];
	cb_tree	x;
	struct cb_field *f;
	int	is_value_parm, is_any_numeric;

	f = setup_param (l, &is_value_parm, &is_any_numeric);

	x = cb_build_field_reference (f, NULL);

	f->flag_data_set = 0;
	if (is_value_parm) {
		if (f->flag_any_length) {
			sprintf(wrk,"module->next->cob_procedure_params[%u]->size",inc);
		} else {
			strcpy(wrk,"0");
		}
		output_line ("if (cob_glob_ptr->cob_call_params > %u) { /* BY VALUE %s */", inc, f->name);
		output_line ("    cob_alloc_move(%s[%u], &%s%d, %s);",
				     "module->next->cob_procedure_params", inc,
				     CB_PREFIX_FIELD, f->id, wrk);
		if (!cb_sticky_linkage) {
			output_line ("} else {");
			output_line ("    %s%d.data = NULL;", CB_PREFIX_FIELD, f->id);
		}
		output_line ("}");
	} else
	if (f->flag_any_length) {
		output_line ("if (cob_glob_ptr->cob_call_params > %u", inc);
		output_line ("&&  module->next");
		if (is_any_numeric) {
			output_line ("&&  %s[%u]) { /* ANY NUMERIC %s */",
					"module->next->cob_procedure_params", inc, f->name);
			output_indent_level += 2;
			/* Copy complete structure */
			output_line ("    %s%d = *(%s[%u]);",
				     CB_PREFIX_FIELD, f->id,
				     "module->next->cob_procedure_params", inc);
		} else {
			output_line ("&&  %s[%u]) { /* BY REFERENCE %s */",
					"module->next->cob_procedure_params", inc, f->name);
			output_indent_level += 2;
			/* Copy size */
			output_line ("%s%d.size = %s[%u]->size;",
				     CB_PREFIX_FIELD, f->id,
				     "module->next->cob_procedure_params", inc);
			/* Copy data address */
			output_line ("%s%d.data = %s[%u]->data;",
				     CB_PREFIX_FIELD, f->id,
				     "module->next->cob_procedure_params", inc);
		}
		if (!cb_sticky_linkage) {
			output_indent_level -= 2;
			output_line ("} else {");
			output_indent_level += 2;
			output_line ("%s%d.data = NULL;", CB_PREFIX_FIELD, f->id);
		}
		output_indent_level -= 2;
		output_line ("}");
	} else {
		output_line ("if (cob_glob_ptr->cob_call_params > %u)", inc);
		output_indent_level += 2;
		output_prefix ();
		output ("%s%d.data = (cob_u8_t*)", CB_PREFIX_FIELD, f->id);
		output_data (x);
		output ("; /* %s */", f->name);
		output_newline ();
		output_indent_level -= 2;
	}
	f->flag_data_set = 1;
}

/* Pickup parameter size for ANY LENGTH */
static void
pickup_any_length (cb_tree l, cob_u32_t inc)
{
	struct cb_field *f;
	int	is_value_parm, is_any_numeric;

	f = setup_param (l, &is_value_parm, &is_any_numeric);

	if (f->flag_any_length) {
		f->flag_data_set = 0;
		output_line ("if (module->module_num_params > %u && "
				 "module->next && "
				 "module->next->cob_procedure_params[%u])",
				inc, inc);
		if (f->flag_any_numeric) {	/* Copy complete structure */
			output_line ("  %s%d = *(module->next->cob_procedure_params[%u]);",
					 CB_PREFIX_FIELD, f->id, inc);
		} else {			/* Copy size */
			output_line ("  %s%d.size = module->next->cob_procedure_params[%u]->size;",
					 CB_PREFIX_FIELD, f->id, inc);
		}
		output_prefix ();
		output ("%s%d.data = ", CB_PREFIX_FIELD, f->id);
		output_data (CB_VALUE (l));
		output (";");
		output_newline ();
		f->flag_data_set = 1;
	}
}

/* Pickup parameter knowing the caller is C */
static void
pickup_c_param (cb_tree l, cob_u32_t inc, int is_enter)
{
	char	wrk[64];
	cb_tree	x;
	struct cb_field *f;
	int	is_value_parm, is_any_numeric;

	f = setup_param (l, &is_value_parm, &is_any_numeric);

	x = cb_build_field_reference (f, NULL);

	f->flag_data_set = 0;
	if (is_value_parm) {
		if (f->flag_any_length) {
			sprintf(wrk,"module->next->cob_procedure_params[%u]->size",inc);
		} else {
			strcpy(wrk,"0");
		}
		output_prefix ();
		output ("%s%d.data = (cob_u8_t*)", CB_PREFIX_FIELD, f->id);
		output_data (x);
		output (";");
		output_newline ();
		if (f->flag_any_length) {
			output_line ("if (%s%d.data != NULL)",CB_PREFIX_FIELD, f->id);
			output_line ("    %s%d.size = strlen((char*)%s%d.data);",
							CB_PREFIX_FIELD, f->id,
							CB_PREFIX_FIELD, f->id);
		}
	} else
	if (f->flag_any_length) {
		output_prefix ();
		output ("%s%d.data = (cob_u8_t*)", CB_PREFIX_FIELD, f->id);
		output_data (x);
		output (";");
		output_newline ();
		if (is_any_numeric) {
			output_line ("if (%s%d.data != NULL) {", CB_PREFIX_FIELD, f->id);
			output_indent_level += 2;
			output_line ("%s%d.size = strlen((char*)%s%d.data);",
							CB_PREFIX_FIELD, f->id,
							CB_PREFIX_FIELD, f->id);
			output_line ("if (%s%d.size > 0 && %s%d.size < 20)",
						CB_PREFIX_FIELD, f->id,CB_PREFIX_FIELD, f->id);
			output_line ("  %s%d.attr = cob_alloc_attr(COB_TYPE_NUMERIC_DISPLAY,%s%d.size,0,0);",
						CB_PREFIX_FIELD, f->id, CB_PREFIX_FIELD, f->id);
			output_indent_level -= 2;
			output_line ("}");
		} else {
			output_line ("if (%s%d.data != NULL)", CB_PREFIX_FIELD, f->id);
			output_line ("  %s%d.size = strlen((char*)%s%d.data);",
							CB_PREFIX_FIELD, f->id,
							CB_PREFIX_FIELD, f->id);
		}
	} else {
		if (is_enter) {
			if (inc == 0) {
				output_line ("if (cob_glob_ptr->cob_call_params >= 0)");
				output_indent_level += 2;
			} else if (inc > 0) {
				output_line ("if (cob_glob_ptr->cob_call_params == 0");
				output_line ("||  cob_glob_ptr->cob_call_params > %u)", inc);
				output_indent_level += 2;
			}
		}
		output_prefix ();
		output ("%s%d.data = (cob_u8_t*)", CB_PREFIX_FIELD, f->id);
		output_data (x);
		output ("; /* %s */", f->name);
		output_newline ();
		if (is_enter) {
			if (inc >= 0)
				output_indent_level -= 2;
		}
	}
	f->flag_data_set = 1;
}

static void
clear_report_line (struct cb_field *f)
{
	if (f->storage != CB_STORAGE_REPORT)
		return;
	if (f->flag_base) {
		output_prefix ();
		output ("memset (");
		output_base (f, 0);
		output (", ' ', %d);", f->size);
		output_newline ();
	}
	if (f->sister) {
		clear_report_line (f->sister);
	}
	if (f->children) {
		clear_report_line (f->children);
	}
}

static void
pickup_param (cb_tree l, int i)
{
	cb_tree	x;
	struct cb_field *f;
	int	is_value_parm, is_any_numeric;

	f = setup_param (l, &is_value_parm, &is_any_numeric);

	if (is_value_parm
	 || f->flag_any_length) {
		output_line ("if (cob_glob_ptr->cob_call_from_c) {");
		output_indent_level += 2;
		pickup_c_param (l, i, 1);
		output_indent_level -= 2;
		output_line ("} else {");
		output_indent_level += 2;
		pickup_cob_param (l, i);
		output_indent_level -= 2;
		output_line ("}");
	} else {
		x = cb_build_field_reference (f, NULL);
		f->flag_data_set = 0;
		output_prefix ();
		output ("%s%d.data = (cob_u8_t*)", CB_PREFIX_FIELD, f->id);
		output_data (x);
		output ("; /* %s */", f->name);
		output_newline ();
		f->flag_data_set = 1;
	}
}

static void
output_internal_function (struct cb_program *prog, cb_tree parameter_list)
{
	cb_tree			l;
	cb_tree			l2;
	cb_tree			using_list;
	struct cb_field		*f;
	struct cb_program	*next_prog;
	struct cb_file		*fl;
	struct cb_report	*rep;
	char			*p;
	struct label_list	*pl;
	struct cb_alter_id	*cpl;
	struct call_list	*clp;
	struct base_list	*bl;
	struct literal_list	*m;
	const char		*s;
	cob_u32_t		inc, i, j;
	unsigned int		name_hash;
	int			parmnum;
	int			seen;

	recent_prog = prog;
	/* Program function */
	if (prog->prog_type == COB_MODULE_TYPE_FUNCTION) {
		output_line ("static cob_field *");
		output ("%s_ (const int entry",
			prog->program_id);
	} else if (!prog->nested_level) {
		output_line ("static int");
		output ("%s_ (const int entry", prog->program_id);
	} else {
		output_line ("static int");
		output ("%s_%d_ (const int entry",
			prog->program_id, prog->toplev_count);
	}
	parmnum = 0;
	if (!prog->flag_chained) {
		for (l = parameter_list; l; l = CB_CHAIN (l)) {
			if (l == parameter_list) {
				output (", ");
			}
			if (parmnum && !(parmnum % 2)) {
				output_newline ();
				output ("\t");
			}
			output ("cob_u8_t *%s%d",
				CB_PREFIX_BASE, cb_code_field (CB_VALUE (l))->id);
			if (CB_CHAIN (l)) {
				output (", ");
			}
			parmnum++;
		}
	}
	output (")");
	output_newline ();
	output_block_open ();

	/* Program local variables */
	output_line ("/* Program local variables */");
	output_line ("#include \"%s\"", prog->local_include->local_include_name);
	output_newline ();

	/* Alphabet-names */
	if (prog->alphabet_name_list) {
		for (l = prog->alphabet_name_list; l; l = CB_CHAIN (l)) {
			output_alphabet_name_definition (CB_ALPHABET_NAME (CB_VALUE (l)));
		}
	}

	output_line ("/* Entry point name_hash values */");
	output_line ("static const unsigned int %sname_hash [] = {",CB_PREFIX_STRING);
	if (cb_list_length (prog->entry_list) > 1) {
		for (inc = 0, l = prog->entry_list; l; inc++, l = CB_CHAIN (l)) {
			name_hash = cob_get_name_hash (CB_LABEL (CB_PURPOSE (l))->name);
			output_line ("\t0x%X,\t/* %u: %s */",
				name_hash, inc, CB_LABEL (CB_PURPOSE (l))->name);
		}
	} else {
		name_hash = cob_get_name_hash (prog->orig_program_id);
		output_line ("\t0x%X,\t/* %s */",
			name_hash, prog->orig_program_id);
	}
	output_line ("0};");

	/* Module initialization indicator */

	output_local ("/* Module initialization indicator */\n");
	output_local ("static unsigned int\tinitialized = 0;\n\n");
	if (prog->flag_recursive) {
		output_local ("/* Module structure pointer for recursive */\n");
		output_local ("cob_module\t\t*module = NULL;\n\n");
	} else {
		output_local ("/* Module structure pointer */\n");
		output_local ("static cob_module\t*module = NULL;\n\n");
	}

	output_local ("/* Global variable pointer */\n");
	output_local ("cob_global\t\t*cob_glob_ptr;\n\n");


	/* Decimal structures */
	if (prog->decimal_index_max) {
		output_local ("/* Decimal structures */\n");
		for (inc = 0; inc < prog->decimal_index_max; inc++) {
			output_local ("cob_decimal\t*%s%d = NULL;\n", CB_PREFIX_DECIMAL, inc);
		}
		output_local ("\n");
	}

	/* External items */
	seen = 0;
	for (f = prog->working_storage; f; f = f->sister) {
		if (f->flag_external) {
			if (f->flag_is_global) {
				bl = cobc_parse_malloc (sizeof (struct base_list));
				bl->f = f;
				bl->curr_prog = excp_current_program_id;
				bl->next = globext_cache;
				globext_cache = bl;
				continue;
			}
			if (!seen) {
				seen = 1;
				output_local ("/* EXTERNAL items */\n");
			}
			output_local ("static unsigned char\t*%s%d = NULL;",
				      CB_PREFIX_BASE, f->id);
			output_local ("  /* %s */\n", f->name);
		}
	}
	if (seen) {
		output_local ("\n");
	}
	for (l = prog->file_list; l; l = CB_CHAIN (l)) {
		f = CB_FILE (CB_VALUE (l))->record;
		if (f->flag_external) {
			if (f->flag_is_global) {
				bl = cobc_parse_malloc (sizeof (struct base_list));
				bl->f = f;
				bl->curr_prog = excp_current_program_id;
				bl->next = globext_cache;
				globext_cache = bl;
				continue;
			}
			output_local ("static unsigned char\t*%s%d = NULL;",
				      CB_PREFIX_BASE, f->id);
			output_local ("  /* %s */\n", f->name);
		}
	}

	/* Allocate files */
	if (prog->file_list) {
		inc = 0;
		for (l = prog->file_list; l; l = CB_CHAIN (l)) {
			inc += output_file_allocation (CB_FILE (CB_VALUE (l)));
		}
		if (inc) {
			output_local ("\n/* LINAGE pointer */\n");
			output_local ("static cob_linage\t\t*lingptr;\n");
		}
	}

	/* BASED working-storage */
	seen = 0;
	for (f = prog->working_storage; f; f = f->sister) {
		if (f->redefines) {
			continue;
		}
		if (f->flag_item_based) {
			if (!seen) {
				seen = 1;
				output_local("\n/* BASED WORKING-STORAGE SECTION */\n");
			}
			output_local ("static unsigned char\t*%s%d = NULL; /* %s */\n",
				CB_PREFIX_BASE, f->id, f->name);
		}
	}
	if (seen) {
		output_local ("\n");
	}

	/* BASED local-storage */
	seen = 0;
	for (f = prog->local_storage; f; f = f->sister) {
		if (f->redefines) {
			continue;
		}
		if (f->flag_item_based) {
			if (!seen) {
				seen = 1;
				output_local("\n/* BASED LOCAL-STORAGE */\n");
			}
			output_local ("static unsigned char\t*%s%d = NULL; /* %s */\n",
				CB_PREFIX_BASE, f->id, f->name);
		}
	}
	if (seen) {
		output_local ("\n");
	}

	/* Dangling linkage section items */
	seen = 0;
	for (f = prog->linkage_storage; f; f = f->sister) {
		if (f->redefines) {
			continue;
		}
		for (l = parameter_list; l; l = CB_CHAIN (l)) {
			if (f == cb_code_field (CB_VALUE (l))) {
				break;
			}
		}
		if (l == NULL) {
			if (!seen) {
				seen = 1;
				output_local ("\n/* LINKAGE SECTION (Items not referenced by USING clause) */\n");
			}
			if (!f->flag_is_returning) {
				output_local ("static ");
			}
			output_local ("unsigned char\t*%s%d = NULL;  /* %s */\n",
				     CB_PREFIX_BASE, f->id, f->name);
		}
	}
	if (seen) {
		output_local ("\n");
	}

	/* Screens */
	if (prog->screen_storage) {
		optimize_defs[COB_SET_SCREEN] = 1;
		output_local ("\n/* Screens */\n\n");
		output_screen_definition (prog->screen_storage);
		output_local ("\n");
	}

	if (prog->report_storage) {
		optimize_defs[COB_SET_REPORT] = 1;
	}

	if (prog->ml_trees) {
		output_local ("\n/* JSON/XML GENERATE trees */\n");
		output_ml_trees_definitions (prog->ml_trees);
	}

	/* Save variables for global callback */
	if (prog->flag_global_use && parameter_list) {
		output_local ("/* Parameter save */\n");
		for (l = parameter_list; l; l = CB_CHAIN (l)) {
			f = cb_code_field (CB_VALUE (l));
			output_local ("static unsigned char\t*save_%s%d;\n",
				CB_PREFIX_BASE, f->id);
		}
		output_local ("\n");
	}

	/* Start of function proper */
	output_line ("/* Start of function code */");
	output_newline ();

	/* CANCEL callback */
	if (prog->prog_type == COB_MODULE_TYPE_PROGRAM) {
		output_line ("/* CANCEL callback */");
		output_line ("if (entry < 0) {");
		output_line ("\tif (entry == -20)");
		output_line ("\t\tgoto P_clear_decimal;");
		output_line ("\tgoto P_cancel;");
		output_line ("}");
		output_newline ();
	}

	output_line ("/* Check initialized, check module allocated, */");
	output_line ("/* set global pointer, */");
	output_line ("/* push module stack, save call parameter count */");
	output_line ("if (cob_module_global_enter (&module, &cob_glob_ptr, %d, entry, %sname_hash))",
				      cb_flag_implicit_init, CB_PREFIX_STRING);
	if (prog->prog_type == COB_MODULE_TYPE_FUNCTION) {
		output_line ("\treturn NULL;");
	} else {
		output_line ("\treturn -1;");
	}
	output_newline ();

	if (prog->flag_chained) {
		output_line ("/* Check program with CHAINING being main program */");
		output_line ("if (cob_glob_ptr->cob_current_module->next)");
		output_line ("\tcob_fatal_error (COB_FERROR_CHAINING);");
		output_newline ();
	}

	/* Recursive module initialization */
	if (prog->flag_recursive) {
		output_module_init (prog);
	}

	/* Module Parameters */
	output_line ("/* Set address of module parameter list */");
	if (cb_flag_stack_on_heap || prog->flag_recursive) {
		output_line ("cob_procedure_params = cob_malloc (%dU * sizeof(void *));",
			     prog->max_call_param ? prog->max_call_param : 1);
	}
	output_line ("module->cob_procedure_params = cob_procedure_params;");
	output_newline ();
	if (cb_flag_symbols
	 && prog->prog_type == 0
	 && !prog->flag_recursive) {
		output_line ("module->num_symbols = %s_num_sym;",prog->program_id);
		output_line ("module->module_symbols = %s_sym_tab;",prog->program_id);
	} else {
		output_line ("module->num_symbols = 0;");
		output_line ("module->module_symbols = NULL;");
	}

	if (cb_flag_stack_on_heap || prog->flag_recursive) {
		const char *frame_type = (cb_flag_stack_extended) ? "cob_frame_ext" : "cob_frame";
		if (prog->flag_recursive && cb_stack_size == 255) {
			i = 63;
		} else {
			i = cb_stack_size;
		}
		output_line ("/* Set recursive frame stack pointer */");
		output_line ("frame_stack = cob_malloc (%dU * sizeof(struct %s));",
			i, frame_type);
		output_line ("frame_ptr = frame_stack;");
	} else {
		output_line ("/* Set frame stack pointer */");
		output_line ("frame_ptr = frame_stack;");
		output_line ("frame_ptr->perform_through = 0;");
		if (cb_flag_computed_goto) {
			output_line ("frame_ptr->return_address_ptr = &&P_cgerror;");
		}
		i = cb_stack_size;
	}
	if (cb_flag_stack_check) {
		output_line ("frame_overflow = frame_ptr + %d - 1;", i);
	}
	if (cb_flag_stack_extended) {
		output_line ("module->frame_ptr = frame_stack;");
	}

	/* To Avoid C compiler warning: -Wunused-but-set-variable */
	if (cb_flag_stack_check) {
		output_line ("if (frame_ptr == frame_overflow) {}");
	}

	output_newline ();

	/* Set up LOCAL-STORAGE size */
	if (prog->local_storage) {
		for (f = prog->local_storage; f; f = f->sister) {
			if (f->flag_item_based || f->flag_local_alloced) {
				continue;
			}
			if (f->redefines) {
				continue;
			}
			/* LCOV_EXCL_START */
			if (f->flag_item_78) {
				cobc_err_msg (_("unexpected CONSTANT item"));
				COBC_ABORT ();
			}
			/* LCOV_EXCL_STOP */
#if 0	/* TODO: add data fence for LOCAL STORAGE */
			if (f->flag_used_in_call) {
				/* buffer for data fence */
				local_mem += ((5 + COB_MALLOC_ALIGN) &
					~COB_MALLOC_ALIGN);
			}
#endif
			f->flag_local_storage = 1;
			f->flag_local_alloced = 1;
			f->mem_offset = local_mem;
			local_mem += compute_align_size (f->memory_size, 16);
#if 0	/* TODO: add data fence for LOCAL STORAGE */
			if (f->flag_used_in_call) {
				/* buffer for data fence */
				local_mem += ((5 + COB_MALLOC_ALIGN) &
					~COB_MALLOC_ALIGN);
			}
#endif
		}
	}

	/* Initialization */

	/* Allocate and initialize LOCAL storage */
	if (prog->local_storage) {
		if (local_mem) {
			output_line ("/* Allocate LOCAL storage */");
			output_line ("cob_local_ptr = cob_malloc (%dU);",
				     local_mem);
			if (prog->flag_global_use
		 	 || cb_flag_symbols) {
				output_line ("cob_local_save = cob_local_ptr;");
			}
		}
		output_newline ();
		output_line ("/* Initialize LOCAL storage */");
		output_initial_values (prog->local_storage);
		output_newline ();
	}

	output_line ("/* Initialize rest of program */");
	output_line ("if (initialized == 0) {");
	output_line ("\tgoto P_initialize;");
	output_line ("}");
	output_line ("P_ret_initialize:");
	output_newline ();

	if (prog->decimal_index_max) {
		output_line ("/* Allocate decimal numbers */");
		output_prefix ();
		if (prog->flag_recursive) {
			output ("cob_decimal_push (%u", prog->decimal_index_max);
		} else {
			output ("cob_decimal_alloc (%u", prog->decimal_index_max);
		}
		for (inc = 0; inc < prog->decimal_index_max; inc++) {
			output (", &%s%u", CB_PREFIX_DECIMAL, inc);
		}
		output (");");
		output_newline ();
		output_newline ();
	}

	/* Global entry dispatch */
	if (prog->global_list) {
		output_line ("/* Global entry dispatch */");
		output_newline ();
		for (l = prog->global_list; l; l = CB_CHAIN (l)) {
			output_line ("if (entry == %d) {",
					CB_LABEL (CB_VALUE (l))->id);
			if (local_mem
		 	 && cb_flag_symbols) {
				output_line ("\tcob_local_ptr = cob_local_save;");
			}
			for (l2 = parameter_list; l2; l2 = CB_CHAIN (l2)) {
				f = cb_code_field (CB_VALUE (l2));
				output_line ("\t%s%d = save_%s%d;",
					CB_PREFIX_BASE, f->id,
					CB_PREFIX_BASE, f->id);
			}
			output_line ("\tgoto %s%d;",
					CB_PREFIX_LABEL,
					CB_LABEL (CB_VALUE (l))->id);
			output_line ("}");
		}
		output_newline ();
	}

	if (prog->report_list) {
		for (l = prog->report_list; l; l = CB_CHAIN (l)) {
			rep = CB_REPORT_PTR (CB_VALUE(l));
			output_report_source (rep);
		}
	}

	if (!prog->flag_recursive) {
		output_line ("/* Increment module active */");
		output_line ("module->module_active++;");
		output_newline ();
	}

	if (!cobc_flag_main && non_nested_count > 1) {
		output_line ("/* Increment module reference count */");
		output_line ("cob_reference_count++;");
		output_newline ();
	}

	/* Initialize W/S and files unconditionally when INITIAL program */
	if (prog->flag_initial) {
		if (prog->working_storage) {
			output_line ("/* Initialize INITIAL program WORKING-STORAGE */");
			output_initial_values (prog->working_storage);
			output_newline ();
		}
		if (prog->file_list) {
			for (l = prog->file_list; l; l = CB_CHAIN (l)) {
				output_file_initialization (CB_FILE (CB_VALUE (l)));
			}
		}

		/* Do Reports again here */
		if (prog->report_list) {
			optimize_defs[COB_SET_REPORT] = 1;
			output_line ("/* Init Reports for INITIAL program */");
			for (l = prog->report_list; l; l = CB_CHAIN (l)) {
				rep = CB_REPORT_PTR (CB_VALUE(l));
				output_report_init (rep);
			}
			output_newline ();
		}
	}

	if (prog->num_proc_params) {
		if (!cb_sticky_linkage
	 	 && !prog->flag_chained
	 	 && prog->prog_type != COB_MODULE_TYPE_FUNCTION) {
			output_line ("/* No sticky-linkage so NULL LINKAGE addresses */");
			for (l = parameter_list; l; l = CB_CHAIN (l)) {
				set_param_to_null (l);
			}
			output_newline ();
		}
		output_line ("/* Store last parameters for possible later lookup */");
		output_local ("/* Last USING parameters for possible later lookup */\n");
		for (l = parameter_list; l; l = CB_CHAIN (l)) {
			f = cb_code_field (CB_VALUE (l));
			output_prefix ();
			output ("last_");
			output_base (f, 0);
			output (" = ");
			output_base (f, 0);
			output (";");
			output_newline ();
			output_local ("static unsigned char\t*last_%s%d;\n",
				CB_PREFIX_BASE, f->id);
		}
		output_newline ();
	}

	/* Set up ANY length items */
	if (cb_list_length (prog->entry_list) <= 1
	 && !prog->flag_chained) {
		seen = 0;
		for (l = parameter_list, inc = 0; l; l = CB_CHAIN (l), inc++) {
			f = cb_code_field (CB_VALUE (l));
			if (f->flag_any_length) {
				if (!seen) {
					seen = 1;
					name_hash = cob_get_name_hash (prog->orig_program_id);
					output_line ("if (cob_glob_ptr->cob_call_name_hash == 0x%X) {", name_hash);
					output_indent_level += 2;
					output_line ("/* Initialize ANY LENGTH parameters */");
					output_line ("module->module_num_params = cob_glob_ptr->cob_call_params;");
				}
				pickup_any_length (l, inc);
			}
		}
		if (seen) {
			output_indent_level -= 2;
			output_line ("}");
			output_newline ();
		}

		if (cb_list_length (parameter_list) > 0
		 && prog->prog_type != COB_MODULE_TYPE_FUNCTION) {
			int	basic_param = 1;
			for (l = parameter_list; l && basic_param; l = CB_CHAIN (l)) {
				struct cb_field *f2;
				int	is_value_parm, is_any_numeric;

				f2 = setup_param (l, &is_value_parm, &is_any_numeric);
				if (is_value_parm
				|| is_any_numeric
				|| f2->flag_any_length) {
					basic_param = 0;
					break;
				}
			}
			name_hash = cob_get_name_hash (prog->orig_program_id);
			output_line ("if (cob_glob_ptr->cob_call_name_hash != 0x%X) {", name_hash);
			output_indent_level += 2;
			output_line ("cob_glob_ptr->cob_call_from_c = 1; /* Called by C */");
			for (inc = 0, l = parameter_list; l; l = CB_CHAIN (l), inc++) {
				pickup_c_param (l, inc, !basic_param);
			}
			output_line ("cob_glob_ptr->cob_call_params = %u;", inc);
			output_indent_level -= 2;
			output_line ("} else {");
			output_indent_level += 2;
			output_line ("cob_glob_ptr->cob_call_from_c = 0; /* Called by COBOL */");
			for (inc = 0, l = parameter_list; l; l = CB_CHAIN (l), inc++) {
				pickup_cob_param (l, inc);
			}
			output_indent_level -= 2;
			output_line ("}");
			output_line ("cob_glob_ptr->cob_call_name_hash = %u;", 0);
		}
		output_newline ();
	}

	/* Call parameters */
	if (prog->cb_call_params && cb_code_field (prog->cb_call_params)->count) {
		output_line ("/* Set NUMBER-OF-CALL-PARAMETERS (independent from LINKAGE) */");
		output_prefix ();
		output_integer (prog->cb_call_params);
		output (" = cob_glob_ptr->cob_call_params;");
		output_newline ();
		output_newline ();
	}

	output_line ("/* Save number of call params */");
	output_line ("module->module_num_params = cob_glob_ptr->cob_call_params;");
	output_newline ();

	if (prog->prog_type == COB_MODULE_TYPE_FUNCTION
	 && CB_FIELD_PTR(prog->returning)->storage == CB_STORAGE_LINKAGE) {
		struct cb_field *ret = CB_FIELD_PTR (prog->returning);
		output_line ("/* Storage for returning item */");
		output_prefix ();
		output_data (prog->returning);
		output (" = cob_malloc (%uU);", ret->memory_size);
		output_newline ();
		output_newline ();
	}

	if (prog->flag_global_use && parameter_list) {
		output_line ("/* Parameter save */");
		for (l = parameter_list; l; l = CB_CHAIN (l)) {
			f = cb_code_field (CB_VALUE (l));
			output_line ("save_%s%d = %s%d;",
				CB_PREFIX_BASE, f->id,
				CB_PREFIX_BASE, f->id);
		}
		output_newline ();
	}

	/* Classification */
	if (prog->classification) {
		if (prog->classification == cb_int1) {
			output_line ("cob_set_locale (NULL, COB_LC_CLASS);");
		} else {
			output_prefix ();
			output ("cob_set_locale (");
			output_param  (prog->classification, -1);
			output (", COB_LC_CTYPE);");
		}
		output_newline ();
	}

	/* Entry dispatch */
	output_line ("/* Entry dispatch */");
	if (cb_flag_prof) {
		output_line ("if (!prof_info) {");
		output_line (
			"\tprof_info = cob_prof_init_module (module, prof_procedures, %d);",
			prog->procedure_list_len);
		output_line ("}");

		/* Prevent CANCEL from dlclose() the module, because
		   we keep pointers to static data there. */
		output_line ("if (prof_info) { module->flag_no_phys_canc = 1; }");

		output_line ("cob_prof_enter_procedure (prof_info, 0);");
	}
	if (cb_flag_stack_extended) {
		/* entry marker = first frameptr is the one with
		   an empty (instead of NULL) section name */;
		output_line ("module->frame_ptr->section_name = \"\";");
	}
	if (cb_list_length (prog->entry_list) > 1) {
		output_line ("if (module->next == NULL)");
		output_line ("  cob_glob_ptr->cob_call_from_c = 1;");
		output_newline ();
		output_line ("switch (entry)");
		output_block_open ();
		for (l = prog->entry_list, inc = 0; l; l = CB_CHAIN (l), inc++) {
			cb_tree lx = CB_PURPOSE (l);
			using_list = CB_VALUE (CB_VALUE (l));
			if (using_list) {
				output_line ("case %u: /* Initialize %d parameters for '%s' */",inc,
						cb_list_length(using_list),CB_LABEL (CB_PURPOSE (l))->name);
				output_indent_level += 2;
				for (j=0,l2 = using_list; l2; l2 = CB_CHAIN (l2), j++) {
					pickup_param (l2, j);
				}
				output_indent_level -= 2;
			} else {
				output_line ("case %u: /* No parameters for '%s' */",inc,
						CB_LABEL (CB_PURPOSE (l))->name);
			}
			if (cb_flag_stack_extended) {
				output_line ("  module->frame_ptr->paragraph_name = \"%s\";",
					CB_LABEL (lx)->orig_name);
				output_line ("  module->frame_ptr->module_stmt = 0x%08X;",
					COB_SET_LINE_FILE (lx->source_line, lookup_source (lx->source_file)));
			}
			output_line ("  cob_glob_ptr->cob_call_params = %u;", 0);
			output_line ("  goto %s%d;", CB_PREFIX_LABEL,
				     CB_LABEL (lx)->id);
		}
		output_block_close ();
		output_line ("/* This should never be reached */");
		output_line ("cob_fatal_error (COB_FERROR_MODULE);");
	} else {
		l = CB_PURPOSE (prog->entry_list);
		if (cb_flag_stack_extended) {
			output_line ("module->frame_ptr->paragraph_name = \"%s\";",
				CB_LABEL (l)->orig_name);
			output_line ("module->frame_ptr->module_stmt = 0x%08X;",
				COB_SET_LINE_FILE (l->source_line, lookup_source (l->source_file)));
		}
		name_hash = cob_get_name_hash (CB_LABEL (l)->name);
		output_line ("cob_glob_ptr->cob_call_params = %u;", 0);
		output_line ("goto %s%d;", CB_PREFIX_LABEL,
			     CB_LABEL (l)->id);
	}
	output_newline ();

	/* PROCEDURE DIVISION */
	output_line ("/* PROCEDURE DIVISION */");
	for (l = prog->exec_list; l; l = CB_CHAIN (l)) {
		output_stmt (CB_VALUE (l));
	}

	/* End of program / function */

	/* Output source location as code */
	if (cb_flag_source_location) {
		struct cb_tree_common	loc;
		loc.source_file = prog->common.source_file;
		loc.source_line = prog->last_source_line;
		loc.source_column = 0;
		output_newline ();
		output_line ("/* Line: %-10d: last source line                  :%s */",
			prog->last_source_line, prog->common.source_file);
		if (cb_flag_c_line_directives) {
			output_cobol_info (&loc);
		}
		output_line ("module->module_stmt = 0x%08X;",
			COB_SET_LINE_FILE (prog->last_source_line,
			  lookup_source (prog->common.source_file)));
		if (cb_flag_c_line_directives) {
			output_c_info ();
			output_line ("cob_nop ();");
		}
		output_newline ();
	}

	if (cb_flag_implicit_goback_check && last_section) {
		if (!cb_flag_source_location) {
			output_newline ();
		}
		output_line ("cob_check_beyond_exit (%s%d);"
			"\t/* prevent fall-through */", CB_PREFIX_STRING,
			lookup_string ("PROCEDURE DIVISION"));
	}

	output_newline ();

	if (current_prog->prog_type == COB_MODULE_TYPE_FUNCTION) {
		output_line ("/* Function exit */");
		output_newline ();
		if (needs_exit_prog) {
			output_line ("exit_function:");
			output_newline ();
		}
	} else {
		output_line ("/* Program exit */");
		output_newline ();
		if (needs_exit_prog) {
			output_line ("exit_program:");
			output_newline ();
		}
	}
	if (cb_flag_prof){
		output_line ("cob_prof_exit_procedure (prof_info, 0);");
	}
	if (!prog->flag_recursive) {
		output_line ("/* Decrement module active count */");
		output_line ("if (module->module_active) {");
		output_line ("\tmodule->module_active--;");
		output_line ("}");
		output_newline ();
	}

	if (!cobc_flag_main && non_nested_count > 1) {
		output_line ("/* Decrement module reference count */");
		output_line ("if (cob_reference_count) {");
		output_line ("\tcob_reference_count--;");
		output_line ("}");
		output_newline ();
	}

	if (gen_dynamic) {
		output_line ("/* Deallocate dynamic FUNCTION-ID fields */");
		for (inc = 0; inc < gen_dynamic; inc++) {
			output_line ("if (cob_dyn_%u) {", inc);
			output_line ("  if (cob_dyn_%u->data) {", inc);
			output_line ("    cob_free (cob_dyn_%u->data);", inc);
			output_line ("  }");
			output_line ("  cob_free (cob_dyn_%u);", inc);
			output_line ("  cob_dyn_%u = NULL;", inc);
			output_line ("}");
		}
		output_newline ();
	}

	if (prog->local_storage) {
		output_line ("/* Deallocate LOCAL storage */");
		if (local_mem) {
			output_line ("if (cob_local_ptr) {");
			output_line ("\tcob_free (cob_local_ptr);");
			output_line ("\tcob_local_ptr = NULL;");
			if (current_prog->flag_global_use
		 	 || cb_flag_symbols) {
				output_line ("\tcob_local_save = NULL;");
			}
			output_line ("}");
		}
		for (f = prog->local_storage; f; f = f->sister) {
			if (f->flag_item_based) {
				output_line ("if (%s%d) {", CB_PREFIX_BASE, f->id);
				output_line ("\tcob_free_alloc (&%s%d, NULL);",
					     CB_PREFIX_BASE, f->id);
				output_line ("\t%s%d = NULL;", CB_PREFIX_BASE, f->id);
				output_line ("}");
			}
		}
		output_newline ();
	}

	if (prog->decimal_index_max && prog->flag_recursive) {
		output_line ("/* Free decimal structures */");
		output_prefix ();
		output ("cob_decimal_pop (%u", prog->decimal_index_max);
		for (inc = 0; inc < prog->decimal_index_max; inc++) {
			output (", %s%u", CB_PREFIX_DECIMAL, inc);
		}
		output (");");
		output_newline ();
		output_newline ();
	}

	if (cb_flag_stack_on_heap || prog->flag_recursive) {
		output_line ("/* Free frame stack / call parameters */");
		output_line ("cob_free (frame_stack);");
		output_line ("cob_free (cob_procedure_params);");
		output_newline ();
	}

	if (cb_flag_trace) {
		output_line ("if ((module->flag_debug_trace & COB_MODULE_READYTRACE))");
		output_line ("   cob_trace_exit (%s%d);",
			     CB_PREFIX_STRING, lookup_string(excp_current_program_id));
		output_newline ();
	}

	output_line ("cob_module_leave (module);");
	output_newline ();

	if (prog->flag_recursive) {
		output_line ("/* Free for recursive module */");
		output_line ("cob_module_free (&module);");
		output_newline ();
	}

	/* Implicit CANCEL for INITIAL program */
	if (prog->flag_initial) {
		output_line ("/* CANCEL for INITIAL program */");
		output_prefix ();
		if (!prog->nested_level) {
			output ("%s_ (-1", prog->program_id);
		} else {
			output ("%s_%d_ (-1", prog->program_id,
				prog->toplev_count);
		}
		if (!prog->flag_chained) {
			for (l = parameter_list; l; l = CB_CHAIN (l)) {
				output (", NULL");
			}
		}
		output (");");
		output_newline ();
		output_newline ();
	}

	if (prog->prog_type == COB_MODULE_TYPE_FUNCTION) {
		output_prefix ();
		output ("return cob_function_return (");
		output_param (prog->returning, -1);
		output (")");
	} else {
		output_line ("/* Program return */");
		if (prog->returning && prog->cb_return_code) {
			output_move (prog->returning, prog->cb_return_code);
		}
		output_prefix ();
		output ("return ");
		if (prog->cb_return_code) {
			output_integer (prog->cb_return_code);
		} else {
			output ("0");
		}
	}
	output (";");
	output_newline ();

	/* Error handlers */
	if (prog->file_list
	 || prog->flag_gen_error
	 || has_global_file) {
		output_error_handler (prog);
	}

	/* Program initialization */

	output_newline ();
	output_line ("/* Program initialization */");
	output_line ("P_initialize:");
	output_newline ();

	/* Check matching version in program init */
	if ( (cb_flag_use_constructor == 0	/* if constructor option disabled */
#ifdef _WIN32
	   || prog->flag_main 	/* or under Win32 (where we can only use DllMain) for executables */
#endif
	     )
	 /* no use in generating that for nested programs, as the outest program must be started first */
	 && !prog->nested_level) {
		output_line ("cob_check_version (COB_SOURCE_FILE, COB_PACKAGE_VERSION, COB_PATCH_LEVEL);");
		output_newline ();
	}

	/* Resolve user functions */
	for (clp = func_call_cache; clp; clp = clp->next) {
		output_line ("func_%s.funcvoid = cob_resolve_func (\"%s\");",
			      clp->call_name, clp->call_name);
	}

	if (cobc_flag_main && !prog->nested_level) {
		output_line ("cob_module_path = cob_glob_ptr->cob_main_argv0;");
		output_newline ();
	}

	/* Module initialization */
	if (!prog->flag_recursive) {
		output_module_init (prog);
	}

	/* Setup up CANCEL callback */
	if ((call_cache || func_call_cache)
	 && (cb_flag_memory_check & CB_MEMCHK_POINTER)) {
		output_line ("/* Initialize call-pointer memory fence */");
		output_line ("memcpy (call_fence_pre, \"\\xFF\\xFE\\xFD\\xFC\\xFB\\xFA\\xFF\", 8);");
		output_line ("memcpy (call_fence_post, \"\\xFA\\xFB\\xFC\\xFD\\xFE\\xFF\\xFA\", 8);");
		output_newline ();
	}

	/* Setup up CANCEL callback */
	if (!prog->nested_level && prog->prog_type == COB_MODULE_TYPE_PROGRAM) {
		output_line ("/* Initialize cancel callback */");
		output_line ("cob_set_cancel (module);");
		output_newline ();
	}

	/* Initialize EXTERNAL files */
	for (l = prog->file_list; l; l = CB_CHAIN (l)) {
		f = CB_FILE (CB_VALUE (l))->record;
		if (f->flag_external) {
			char	string_buffer[COB_MINI_BUFF];
			strcpy (string_buffer, f->name);
			for (p = string_buffer; *p; p++) {
				if (*p == '-' || *p == ' ') {
					*p = '_';
				}
			}
			output_line ("%s%d = cob_external_addr (\"%s\", %d);",
				     CB_PREFIX_BASE, f->id, string_buffer,
				     CB_FILE (CB_VALUE (l))->record_max);
		}
	}

	gen_init_working = 1;		/* Disable use of DEPENDING ON fields */
	/* Initialize WORKING-STORAGE EXTERNAL items */
	for (f = prog->working_storage; f; f = f->sister) {
		if (f->redefines
		 || f->flag_filler
		 || !f->flag_external
		 || f->ename == NULL)
			continue;
		output_prefix ();
		output_base (f, 0);
		output (" = cob_external_addr (\"%s\", %d);",
			f->ename, f->size);
		output_newline ();
	}

	/* Initialize WORKING-STORAGE/files if not INITIAL program */
	if (!prog->flag_initial) {
		if (prog->working_storage) {
			char wrk[48];
			if (cb_default_byte >= 0) {
				if (isprint (cb_default_byte))
					sprintf (wrk,", Default byte: '%c'", cb_default_byte);
				else
					sprintf (wrk,", Default byte: 0x%02X", cb_default_byte);
			} else {
				strcpy(wrk,"");
			}
			output_line ("/* Initialize WORKING-STORAGE%s */",wrk);
			output_initial_values (prog->working_storage);
			output_newline ();
		}
		if (prog->file_list) {
			for (l = prog->file_list; l; l = CB_CHAIN (l)) {
				output_file_initialization (CB_FILE (CB_VALUE (l)));
			}
		}
	}

	seen = 0;
	for (m = prog->decimal_constants; m; m = m->next) {
		if (!seen) {
			seen = 1;
			output_line ("/* Set Decimal Constant values */");
		}
		output_line ("%s%d = &%s%d;", CB_PREFIX_DEC_CONST, m->id,
			     CB_PREFIX_DEC_FIELD, m->id);
		output_line ("cob_decimal_init (%s%d);", CB_PREFIX_DEC_CONST, m->id);
		output_line ("cob_decimal_set_field (%s%d, (cob_field *)&%s%d);",
			     CB_PREFIX_DEC_CONST, m->id,
			     CB_PREFIX_CONST, m->id);
		output_newline ();
	}
	if (seen) {
		output_newline ();
	}

#if	0	/* BWT coerce linkage to picture */
	/* Manage linkage section */
	if (prog->linkage_storage) {
		output_line ("/* Initialize LINKAGE */");
		output_coerce_linkage (prog->linkage_storage);
		output_newline ();
	}
#endif

	if (prog->screen_storage) {
		output_line ("/* Initialize SCREEN items */");
		/* Initialize items with VALUE */
		output_initial_values (prog->screen_storage);
		/* output structure, note: this can be quite complex
		   and nested, therefore this isn't done in the header */
		output_screen_init (prog->screen_storage, NULL);
		output_newline ();
	}

	if (prog->report_storage) {
		output_line ("/* Initialize REPORT data lines */");
		/* Initialize items with VALUE */
		for (l = prog->report_list; l; l = CB_CHAIN (l)) {
			rep = CB_REPORT_PTR (CB_VALUE(l));
			if (rep) {
				for (f = rep->records; f; f = f->sister) {
					/* Clear report lines to SPACES */
					clear_report_line (f);
				}
			}
		}
		output_newline ();
	}

	/* JSON/XML GENERATE trees */
	if (prog->ml_trees) {
		optimize_defs[COB_SET_ML_TREE] = 1;
		output_line ("/* Initialize JSON/XML GENERATE output trees */");
		output_ml_generate_init (prog->ml_trees);
		output_newline ();
	}

	/* Reports */
	if (prog->report_list) {
		optimize_defs[COB_SET_REPORT] = 1;
		output_newline ();
		output_line ("/* Init Reports */");
		output_newline ();
		for (l = prog->report_list; l; l = CB_CHAIN (l)) {
			rep = CB_REPORT_PTR (CB_VALUE(l));
			output_report_init (rep);
		}
		output_newline ();
	}

	gen_init_working = 0;		/* re-enable use of DEPENDING ON fields */

	/* ensure references needed to prevent compilation warnings/errors*/
	output_line ("if (0 == 1) goto P_cgerror;");

	output_line ("initialized = 1;");
	output_line ("goto P_ret_initialize;");

	output_newline ();
	if (prog->report_list) {
		for (l = prog->report_list; l; l = CB_CHAIN (l)) {
			rep = CB_REPORT_PTR (CB_VALUE(l));
			output_report_source_move (rep);
		}
		output_newline ();
	}

	/* Frame stack jump table for compiler without computed goto */
	if (!cb_flag_computed_goto) {
		output_newline ();
		output_line ("/* Frame stack jump table */");
		output_line ("P_switch:");
		if (label_cache) {
			output_line (" switch (frame_ptr->return_address_num) {");
			for (pl = label_cache; pl; pl = pl->next) {
				output_line (" case %d:", pl->call_num);
				output_line ("   goto %s%d;", CB_PREFIX_LABEL, pl->id);
			}
			output_line (" }");
		}
	}
	output_line ("P_cgerror:");
	output_line ("\tcob_fatal_error (COB_FERROR_CODEGEN);");
	output_newline ();

	/* Set up CANCEL callback code */

	if (prog->prog_type != COB_MODULE_TYPE_PROGRAM) {
		goto prog_cancel_end;
	}

	output_newline ();
	output_line ("/* CANCEL callback handling */");
	output_line ("P_cancel:");
	output_newline ();
	output_line ("if (!initialized)");
	output_line ("\treturn 0;");
	output_line ("if (module && module->module_active)");
	output_line ("\tcob_fatal_error (COB_FERROR_CANCEL);");
	output_newline ();

	if (prog->flag_main) {
		goto cancel_end;
	}

	next_prog = prog->next_program;

	/* Check for implicit cancel of contained programs */
	for (; next_prog; next_prog = next_prog->next_program) {
		if (next_prog->nested_level == prog->nested_level + 1) {
			output_prefix ();
			output ("(void)%s_%d_ (-1", next_prog->program_id,
				next_prog->toplev_count);
			for (inc = 0; inc < next_prog->num_proc_params; ++inc) {
				output (", NULL");
			}
			output (");");
			output_newline ();
		}
	}

	/* Close files on cancel */
	for (l = prog->file_list; l; l = CB_CHAIN (l)) {
		fl = CB_FILE (CB_VALUE (l));
		if (fl->organization != COB_ORG_SORT) {
			/* CHECKME: Shouldn't we raise a runtime warning when still open? */
			output_line ("if (%s%s->open_mode != COB_OPEN_CLOSED)",
					CB_PREFIX_FILE, fl->cname);
			output_line ("\tcob_close (%s%s, NULL, COB_CLOSE_NORMAL, 1);",
					CB_PREFIX_FILE, fl->cname);
			if (!fl->flag_external) {
				output_line ("cob_file_destroy (&%s%s);",
					CB_PREFIX_FILE, fl->cname);
			}
		} else {
			output_line ("cob_cache_free (%s%s);",
					CB_PREFIX_FILE, fl->cname);
			output_line ("%s%s = NULL;",
					CB_PREFIX_FILE, fl->cname);
		}
	}

	/* Clear alter indicators */
	for (cpl = prog->alter_gotos; cpl; cpl = cpl->next) {
		output_line ("label_%s%d = 0;",
			     CB_PREFIX_LABEL, cpl->goto_id);
		if (prog->flag_segments) {
			output_line ("save_label_%s%d = 0;",
				     CB_PREFIX_LABEL, cpl->goto_id);
		}
	}

	/* Release BASED storage */
	/* CHECKME: Is that really correct for RECURSIVE programs? */
	for (f = prog->working_storage; f; f = f->sister) {
		if (f->flag_item_based) {
			output_line ("if (%s%d) {", CB_PREFIX_BASE, f->id);
			output_line ("\tcob_free_alloc (&%s%d, NULL);",
				     CB_PREFIX_BASE, f->id);
			output_line ("}");
		}
	}

	/* Clear CALL pointers */
	for (clp = call_cache; clp; clp = clp->next) {
		output_line ("call_%s.funcvoid = NULL;", clp->call_name);
	}
	for (clp = func_call_cache; clp; clp = clp->next) {
		output_line ("func_%s.funcvoid = NULL;", clp->call_name);
	}

	/* Clear sticky-linkage pointers */
	if (cb_sticky_linkage && !prog->flag_chained) {
		for (l = prog->parameter_list; l; l = CB_CHAIN (l)) {
			output_line ("cob_parm_%d = NULL;",
				      cb_code_field (CB_VALUE (l))->id);
		}
	}

	/* Clear RETURN-CODE */
	if (!prog->nested_level && prog->cb_return_code) {
		output_prefix ();
		output_integer (current_prog->cb_return_code);
		output (" = 0;");
		output_newline ();
	}

	output_line ("cob_module_free (&module);");
	output_newline ();

cancel_end:
	output_line ("initialized = 0;");
	output_newline ();
	output_line ("P_clear_decimal:");
	seen = 0;
	for (m = prog->decimal_constants; m; m = m->next) {
		if (!seen) {
			seen = 1;
			output_line ("/* Clear Decimal Constant values */");
		}
		output_line ("cob_decimal_clear (%s%d);", CB_PREFIX_DEC_CONST, m->id);
		output_line ("%s%d = NULL;", CB_PREFIX_DEC_CONST, m->id);
	}
	if (seen) {
		output_newline ();
	}
	output_line ("return 0;");
	output_newline ();
	/* End of CANCEL callback code */

prog_cancel_end:
	output_block_close ();
	output_newline ();
	if (prog->prog_type == COB_MODULE_TYPE_FUNCTION) {
		s = "FUNCTION-ID";
	} else {
		s = "PROGRAM-ID";
	}
	output_line ("/* End %s '%s' */", s, prog->orig_program_id);
	output_newline ();
	output_module_init_function (prog);
}

/* Output the entry function for a COBOL function. */
static void
output_function_entry_function (struct cb_program *prog, cb_tree entry,
				const int gencode)
{
	const char		*entry_name;
	cb_tree			using_list;
	cb_tree			l;
	cob_u32_t		parmnum;
	cob_u32_t		n;

	recent_prog = prog;
	entry_name = CB_LABEL (CB_PURPOSE (entry))->name;
	using_list = CB_VALUE (CB_VALUE (entry));

	if (gencode) {
		output_line ("/* ENTRY '%s' */", entry_name);
		output_newline ();
		output_line ("cob_field *");
		output ("%s (", entry_name);
#if 0 /* TODO for 4.0: set the attributes from the field given outside on the stack */
		output ("cob_field *cob_fret, const int cob_pam");
#else
		output ("cob_field **cob_fret, const int cob_pam");
#endif
	} else {
#if	(defined(_WIN32) || defined(__CYGWIN__)) && !defined(__clang__)
		if (!prog->nested_level) {
			output ("__declspec(dllexport) ");
		}
#endif
		output ("cob_field\t\t*%s (", entry_name);
		output ("cob_field **, const int");
	}
	parmnum = 0;
	if (using_list) {
		output (", ");
		n = 0;
		for (l = using_list; l; l = CB_CHAIN (l), ++n, ++parmnum) {
			if (!gencode) {
				output ("cob_field *");
			} else {
				output ("cob_field *f%u", n);
			}
			if (CB_CHAIN (l)) {
				output (", ");
			}
		}
	}
	if (!gencode) {
		/* Finish prototype and return */
		output (");");
		output_newline ();
		return;
	}
	output (")");
	output_newline ();

	output_block_open ();
	output_line ("struct cob_func_loc\t*floc;");
#if 0 /* TODO for 4.0: set the attributes from the field given outside on the stack */
	output_line ("cob_field*\t*ret_fld;");
#else
	output_line ("cob_field\t*ret = NULL;");
#endif
	output_newline ();

	output_line ("/* Save environment */");
	output_prefix ();
	output ("floc = cob_save_func (cob_fret, cob_pam, %u", parmnum);
	for (n = 0; n < parmnum; ++n) {
		output (", f%u", n);
	}
	output (");");
	output_newline ();

	output_prefix ();
#if 0 /* TODO for 4.0: set the attributes from the field given outside on the stack */
	output ("ret_fld = %s_ (0", prog->program_id);
#else
	output ("floc->ret_fld = %s_ (0", prog->program_id);
#endif
	if (parmnum != 0) {
		output (", ");
		for (n = 0; n < parmnum; ++n) {
			output ("floc->data[%u]", n);
			if (n != parmnum - 1) {
				output (", ");
			}
		}
	}
	output (");");
	output_newline ();
#if 0 /* TODO for 4.0: set the attributes from the field given outside on the stack */
	output_line ("COB_SET_FLD ((*cob_fret), ret_fld->size, ret_fld, ret_fld->attr);");
#else
	output_line ("if (floc->ret_fld != NULL) {");
	output_line ("  **cob_fret = *floc->ret_fld;");
	output_line ("  ret = *cob_fret;");
	output_line ("}");
#endif
	output_newline ();

	output_line ("/* Restore environment */");
	output_line ("cob_restore_func (floc);");
	output_line ("return ret;");
	output_block_close ();
	output_newline ();
}

/* Returns NULL if it could not deduce the parameter type. */
static const char *
try_get_by_value_parameter_type (const enum cb_usage usage,
				 cb_tree param_list_elt)
{
	if (usage == CB_USAGE_FLOAT) {
		return "float";
	} else if (usage == CB_USAGE_DOUBLE) {
		return "double";
	} else if (usage == CB_USAGE_LONG_DOUBLE) {
		return "long double";
	} else if (usage == CB_USAGE_FP_BIN32) {
		return "cob_u32_t";
	} else if (usage == CB_USAGE_FP_BIN64
		   || usage == CB_USAGE_FP_DEC64) {
		return "cob_u64_t";
	} else if (usage == CB_USAGE_FP_BIN128
		   || usage == CB_USAGE_FP_DEC128) {
		return "cob_fp_128";
	} else if (CB_TREE_CLASS (CB_VALUE (param_list_elt))
		   == CB_CLASS_NUMERIC) {
		return get_size_parameter_type(
			CB_SIZES_INT (param_list_elt),
			CB_SIZES_INT_UNSIGNED (param_list_elt));
	}

	return NULL;
}


static void
output_program_entry_function_parameters (cb_tree using_list, const int gencode,
					  const char ** const s_type)
{
	cob_u32_t	n = 0;
	cb_tree		l;
	struct cb_field	*f;
	const char	*type;

	for (l = using_list; l; l = CB_CHAIN (l), ++n) {
		f = cb_code_field (CB_VALUE (l));
		switch (CB_PURPOSE_INT (l)) {
		case CB_CALL_BY_VALUE:
			type = try_get_by_value_parameter_type (f->usage, l);
			if (type) {
				if (gencode) {
					output ("%s %s%d", type, CB_PREFIX_BASE,
						f->id);
				} else {
					output ("%s", type);
				}

				if (cb_sticky_linkage) {
					s_type[n] = "";
				} else {
					s_type[n] = "(cob_u8_ptr)&";
				}
				break;
			}

			/* Fall through */
		case CB_CALL_BY_REFERENCE:
		case CB_CALL_BY_CONTENT:
			if (gencode) {
				output ("cob_u8_t *%s%d",
					CB_PREFIX_BASE, f->id);
			} else {
				output ("cob_u8_t *");
			}
			s_type[n] = "";
			break;

		default:
			break;
		}

		if (CB_CHAIN (l)) {
			output (", ");
		}
	}
}

static void
output_entry_function (struct cb_program *prog, cb_tree entry,
		       cb_tree parameter_list, const int gencode)
{
	const char		*entry_name = CB_LABEL (CB_PURPOSE (entry))->name;
	cb_tree			using_list = CB_VALUE (CB_VALUE (entry));
	cb_tree			l = CB_PURPOSE (CB_VALUE (entry));	/* entry convention */
	cb_tree			l1;
	cb_tree			l2;
	struct cb_field		*f;
	struct cb_field		*f1;
	struct cb_field		*f2;
	const char		*s;
	const char		*s2;
	const char		*s_prefix;
	const char		*s_type[MAX_CALL_FIELD_PARAMS];
	cob_u32_t		parmnum;
	cob_u32_t		n;
	int			sticky_ids[MAX_CALL_FIELD_PARAMS] = { 0 };
	int			sticky_nonp[MAX_CALL_FIELD_PARAMS] = { 0 };
	int			entry_convention = 0;

	/* LCOV_EXCL_START */
	if (!l || !(CB_INTEGER_P (l) || CB_NUMERIC_LITERAL_P (l))) {
		/* not translated as it is a highly unlikely internal abort */
		cobc_err_msg ("Missing/wrong internal entry convention!");
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */

	if (CB_INTEGER_P (l)) {
		entry_convention = CB_INTEGER (l)->val;
	} else if (CB_NUMERIC_LITERAL_P (l)) {
		entry_convention = cb_get_int (l);
	}

	if (gencode) {
		output_line ("/* ENTRY '%s' */", entry_name);
		output_newline ();
	} else {
		if (!prog->nested_level) {
			output ("COB_EXT_EXPORT ");
		}
	}

	/* Output return type. */
	if ( (prog->nested_level && !prog->flag_void)
	 ||  (prog->flag_main && !prog->flag_recursive
	   && !strcmp (prog->program_id, entry_name))) {
		output ("static ");
	}
	if (prog->flag_void) {
		output ("void");
	} else {
		output ("int");
	}
	if (entry_convention & CB_CONV_STDCALL) {
		output (" __stdcall");
	}
	if (gencode) {
		output_newline ();
	} else {
		output ("\t\t");
	}

	/* Output function name */
	if (prog->nested_level) {
		output ("%s_%d__ (", entry_name, prog->toplev_count);
	} else {
		output ("%s (", entry_name);
	}

	/* Output parameter list */

	if (prog->flag_chained) {
		using_list = NULL;
		parameter_list = NULL;
	}

	if (!gencode) {
		/* Finish prototype and return */
		if (using_list) {
			output_program_entry_function_parameters (using_list, 0, s_type);
			output (");");
		} else {
			output ("void);");
		}
		output_newline ();
		return;
	}

	output_program_entry_function_parameters (using_list, 1, s_type);
	output (")");
	output_newline ();

	output_block_open ();

	/* By value pointer fields */
	for (l2 = using_list; l2; l2 = CB_CHAIN (l2)) {
		f2 = cb_code_field (CB_VALUE (l2));
		if (CB_PURPOSE_INT (l2) == CB_CALL_BY_VALUE &&
		    (f2->usage == CB_USAGE_POINTER 
			|| f2->usage == CB_USAGE_PROGRAM_POINTER)) {
			output_line ("unsigned char\t\t*ptr_%d;", f2->id);
		}
	}

	/*
	  We have to cater for sticky-linkage here at the entry point
	  site. Doing it in the internal function is too late as we then do not
	  have the information as to possible ENTRY clauses.
	*/

	/* Sticky linkage parameters */
	if (cb_sticky_linkage && using_list) {
		for (l = using_list, parmnum = 0; l; l = CB_CHAIN (l), parmnum++) {
			f = cb_code_field (CB_VALUE (l));
			sticky_ids[parmnum] = f->id;
			if (CB_PURPOSE_INT (l) == CB_CALL_BY_VALUE) {
				s = try_get_by_value_parameter_type (f->usage, l);
				if (f->usage == CB_USAGE_FP_BIN128
				 || f->usage == CB_USAGE_FP_DEC128) {
					s2 = "{{0, 0}}";
				} else {
					s2 = "0";
				}

				if (s) {
					output_line ("static %s\tcob_parm_l_%d = %s;",
						s, f->id, s2);
					sticky_nonp[parmnum] = 1;
				}
			}
		}
	}

	/* FIXME: add check for COB_EC_PROGRAM_ARG_MISMATCH here,
	   including checking for OPTIONAL items.
	   See comment in typeck.c (cb_build_identifier), too. */

	/* Sticky linkage set up */
	if (cb_sticky_linkage && using_list) {
		if ((parmnum = cb_list_length(using_list)) > 1) {
			output_line (" cob_global *cob_glob_ptr = cob_get_global_ptr();");
			parmnum = 0;
			output_line ("if (cob_glob_ptr->cob_call_name_hash != 0x%X)",
									cob_get_name_hash (prog->orig_program_id));
			output_line ("    cob_glob_ptr->cob_call_params = %u;", cb_list_length(using_list));
			output_line ("/* Set the parameter list */");
			output_line ("switch (cob_glob_ptr->cob_call_params) {");
			for (l = using_list; l; l = CB_CHAIN (l), parmnum++) {
				if (parmnum == 0) {
					continue;
				}
				output_line ("case %u:", parmnum);
				for (n = 0; n < parmnum; ++n) {
					if (sticky_nonp[n]) {
						output_line ("\tcob_parm_l_%d = %s%d;",
							sticky_ids[n], CB_PREFIX_BASE,
							sticky_ids[n]);
						output_line ("\tcob_parm_%d = (cob_u8_ptr)&cob_parm_l_%d;",
							sticky_ids[n],
							sticky_ids[n]);
					} else {
						output_line ("\tcob_parm_%d = %s%d;",
							sticky_ids[n], CB_PREFIX_BASE,
							sticky_ids[n]);
					}
				}
				output_line ("\tbreak;");
			}
			output_line ("default:");
		}
		for (n = 0; n < parmnum; ++n) {
			if (sticky_nonp[n]) {
				output_line ("\tcob_parm_l_%d = %s%d;",
					sticky_ids[n], CB_PREFIX_BASE,
					sticky_ids[n]);
				output_line ("\tcob_parm_%d = (cob_u8_ptr)&cob_parm_l_%d;",
					sticky_ids[n],
					sticky_ids[n]);
			} else {
				output_line ("\tcob_parm_%d = %s%d;",
					sticky_ids[n], CB_PREFIX_BASE,
					sticky_ids[n]);
			}
		}
		if (cb_list_length(using_list) > 1) {
			output_line ("\tbreak;");
			output_line ("}");
		}
	}

	if (cb_sticky_linkage) {
		s_prefix = "cob_parm_";
	} else {
		s_prefix = CB_PREFIX_BASE;
	}

	for (l2 = using_list; l2; l2 = CB_CHAIN (l2)) {
		f2 = cb_code_field (CB_VALUE (l2));
		if (CB_PURPOSE_INT (l2) == CB_CALL_BY_VALUE
		 && (   f2->usage == CB_USAGE_POINTER
			 || f2->usage == CB_USAGE_PROGRAM_POINTER)) {
			output_line ("ptr_%d = %s%d;",
				f2->id, s_prefix, f2->id);
		}
	}

	output_prefix ();
	if (prog->flag_void) {
		output ("(void)");
	} else {
		output ("return ");
	}
	if (!prog->nested_level) {
		output ("%s_ (%d", prog->program_id, progid);
	} else {
		output ("%s_%d_ (%d", prog->program_id, prog->toplev_count, progid);
	}

	if (using_list || parameter_list) {

		/* Output parameter list for final function call. */
		for (l1 = parameter_list; l1; l1 = CB_CHAIN (l1)) {
			f1 = cb_code_field (CB_VALUE (l1));
			n = 0;
			for (l2 = using_list; l2; l2 = CB_CHAIN (l2), ++n) {
				f2 = cb_code_field (CB_VALUE (l2));
				if (strcasecmp (f1->name, f2->name) == 0) {
					switch (CB_PURPOSE_INT (l2)) {
					case CB_CALL_BY_VALUE:
						if (f2->usage == CB_USAGE_POINTER 
						 || f2->usage == CB_USAGE_PROGRAM_POINTER) {
							output (", (cob_u8_ptr)&ptr_%d", f2->id);
							break;
						}
						/* Fall through */
					case CB_CALL_BY_REFERENCE:
					case CB_CALL_BY_CONTENT:
						output (", %s%s%d",
							s_type[n], s_prefix, f2->id);
						break;
					default:
						break;
					}
					break;
				}
			}
			if (l2 == NULL) {
				if (cb_sticky_linkage) {
					output (", %s%d",
						s_prefix, f1->id);
				} else {
					output (", NULL");
				}
			}
		}
	}
	output (");");
	output_newline ();
	output_block_close ();
	output_newline ();
}

static void
output_function_prototypes (struct cb_program *prog)
{
	struct cb_program	*cp;
	struct cb_file		*f;
	cb_tree			l;

	/* LCOV_EXCL_START */
	if (!prog) {
		/* checked to keep the analyzer happy, TODO: real fix later */
		cobc_err_msg (_ ("call to '%s' with invalid parameter '%s'"),
			"output_function_prototypes", "prog");
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */

	output ("/* Function prototypes */");
	output_newline ();
	output_newline ();

	for (cp = prog; cp; cp = cp->next_program) {
		/*
		  Collect all items used as parameters in the PROCEDURE DIVISION
		  header and ENTRY statements in the parameter list.
		*/
		cb_tree		entry;
		for (entry = cp->entry_list; entry; entry = CB_CHAIN (entry)) {
			cb_tree		entry_param, prog_param;
			for (entry_param = CB_VALUE (CB_VALUE (entry)); entry_param;
			     entry_param = CB_CHAIN (entry_param)) {
				for (prog_param = cp->parameter_list; prog_param;
				     prog_param = CB_CHAIN (prog_param)) {
					if (strcasecmp (cb_code_field (CB_VALUE (entry_param))->name,
							cb_code_field (CB_VALUE (prog_param))->name) == 0) {
						break;
					}
				}
				if (prog_param == NULL) {
					cp->parameter_list = cb_list_add (cp->parameter_list, CB_VALUE (entry_param));
				}
			}
		}

		if (cp->flag_main) {
			/* Output prototype for main program wrapper. */
			if (!cp->flag_recursive) {
				output ("static int\t\t%s ();",
					cp->program_id);
			} else {
				output ("int\t\t\t%s ();",
					cp->program_id);
			}
			output_newline ();
#if	(defined(_WIN32) || defined(__CYGWIN__)) && !defined(__clang__)
			for (l = cp->entry_list; l; l = CB_CHAIN (l)) {
				const char *entry_name = CB_LABEL (CB_PURPOSE (l))->name;
				if (0 == strcmp (entry_name, cp->program_id)) {
					continue;
				}
				output_entry_function (cp, l, cp->parameter_list, 0);
			}
#endif
		} else {
			/* Output implementation of other program wrapper. */
			if (cp->prog_type == COB_MODULE_TYPE_PROGRAM) {
				for (l = cp->entry_list; l; l = CB_CHAIN (l)) {
					output_entry_function (cp, l, cp->parameter_list, 0);
				}
			} else {
				for (l = cp->entry_list; l; l = CB_CHAIN (l)) {
					output_function_entry_function (cp, l, 0);
				}
			}
		}

		/* Output prototype for the actual function */
		if (cp->prog_type == COB_MODULE_TYPE_FUNCTION) {
			non_nested_count++;
			output ("static cob_field\t*%s_ (const int",
				cp->program_id);
		} else if (!cp->nested_level) {
			non_nested_count++;
			output ("static int\t\t%s_ (const int",
				cp->program_id);
		} else {
			output ("static int\t\t%s_%d_ (const int",
				cp->program_id, cp->toplev_count);
		}

		/* Output prototype parameters */
		if (!cp->flag_chained) {
			for (l = cp->parameter_list; l; l = CB_CHAIN (l)) {
				output (", cob_u8_t *");
				if (cb_sticky_linkage) {
					output_storage ("static cob_u8_t\t\t\t*cob_parm_%d = NULL;\n",
							cb_code_field (CB_VALUE (l))->id);
				}
			}
		}

		output (");");
		output_newline ();

		/* prototype for file specific EXTFH function */
		for (l = prog->file_list; l; l = CB_CHAIN (l)) {
			f =  CB_FILE (CB_VALUE (l));
			if (f->extfh) {
				const char *extfh_value = CB_CONST (f->extfh)->val;
				/* FIXME: add to an external call chain instead, multiple files
						  may use the same but not-default function */
				if (strcmp (prog->extfh, extfh_value) != 0
				 && strcmp ("EXTFH", extfh_value) != 0) {
					/* note: we may not use COB_EXT_IMPORT here as that only works with
					         entry points in libraries, not with direct provided object files
							 which we at least expect in our testsuite */
					output_line ("extern int %s (unsigned char *opcode, FCD3 *fcd);",
						extfh_value);
				}
			}

		}

		/* prototype for module initialization */
		if (!cp->nested_level) {
			output_line ("static void\t\t%s_module_init (cob_module *module);",
				cp->program_id);
		} else {
			output_line ("static void\t\t%s_%d_module_init (cob_module *module);",
				cp->program_id, cp->toplev_count);
		}

	}

	/* prototype for general EXTFH function */
	if (prog->file_list && prog->extfh
	 && strcmp ("EXTFH", prog->extfh) != 0) {
					/* note: we may not use COB_EXT_IMPORT here as that only works with
					         entry points in libraries, not with direct provided object files
							 which we at least expect in our testsuite */
		output ("extern int %s (unsigned char *opcode, FCD3 *fcd);", prog->extfh);
		output_newline ();
	}

	output_newline ();
}

static void
output_main_function (struct cb_program *prog)
{
	output_line ("/* Main function */");
	if (!cb_flag_winmain) {
		output_line ("int");
		output_line ("main (int argc, char **argv)");
		output_block_open ();
		output_line ("cob_init (argc, argv);");
	} else {
		output_line ("int WINAPI");
		output_line ("WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance, PSTR pCmdLine, int nCmdShow)");
		output_block_open ();
		output_line ("cob_init (__argc, __argv);");
	}
	output_line ("cob_stop_run (%s ());", prog->program_id);
	output_block_close ();
	output_newline ();
}

static void
output_header (const char *locbuff, const struct cb_program *cp)
{
	int	i;

	if (!output_target) {
		return;
	}

	output_line ("/* Generated by           cobc %s.%d */",
			PACKAGE_VERSION, PATCH_LEVEL);
	output_line ("/* Generated from         %s */", cb_source_file);
	if (*locbuff) {
		output_line ("/* Generated at           %s */", locbuff);
	}
	output_line ("/* GnuCOBOL build date    %s */", cb_cobc_build_stamp);
	output_line ("/* GnuCOBOL package date  %s */", COB_TAR_DATE);
	output ("/* Compile command        ");
	for (i = 0; i < cb_saveargc; i++) {
		output ("%s ", cb_saveargv[i]);
	}
	output_line ("*/");
	output_newline ();
	if (cp) {
		output_line ("/* Program local variables for '%s' */",
				cp->orig_program_id);
		output_newline ();
	}
}

/* Symbol Table requested so make sure all 01/77 symbols have cob_field */
static void
emit_base_symbols (struct cb_program *prog)
{
	struct cb_file	*fl;
	struct cb_field	*f;
	cb_tree			l;
	if (cb_flag_symbols) {
		for (l = prog->file_list; l; l = CB_CHAIN (l)) {
			fl = CB_FILE(CB_VALUE (l));
			if (!fl->record) continue;
			for (f = fl->record->sister; f; f = f->sister) {
				count_all_fields (f);
			}
		}
		for (f = prog->working_storage; f; f = f->sister) {
			count_all_fields (f);
		}
		for (f = prog->local_storage; f; f = f->sister) {
			count_all_fields (f);
		}
		for (f = prog->linkage_storage; f; f = f->sister) {
			count_all_fields (f);
		}
		for (f = prog->screen_storage; f; f = f->sister) {
			count_all_fields (f);
		}
	}
}

static void
output_cob_prof_data ( struct cb_program * program )
{
	if (cb_flag_prof) {
		struct cb_procedure_list *l;
		char sep = ' ';

		output_local ("/* cob_prof data */\n\n");

		output_local ("static const int nprocedures = %d;\n",
			      program->procedure_list_len);
		output_local ("static struct cob_prof_procedure prof_procedures[%d] = {\n",
			      program->procedure_list_len);
		sep = ' ';
		for (l = program->procedure_list; l; l=l->next) {
			output_local ("  %c { \"%s\", \"%s\", %d,  %d, %d }\n",
				      sep,
				      l->proc.text,
				      l->proc.file,
				      l->proc.line,
				      l->proc.section,
				      l->proc.kind
				);
			sep = ',';
		}
		output_local ("};\n");

		output_local ("static int fallthrough_label = 0;\n");
		output_local ("static struct cob_prof_module *prof_info;\n");

		output_local ("\n/* End of cob_prof data */\n");

		program->procedure_list = NULL;
		program->procedure_list_len = 0;
		program->prof_current_section = -1;
		program->prof_current_paragraph = -1;
	}
}

void
codegen (struct cb_program *prog, const char *translate_name)
{
	const int set_xref = cb_listing_xref;
	int subsequent_call = 0;
	int has_global_file_level = 0 ;
	codegen_init (prog, translate_name);

	/* Temporarily disable cross-reference during C generation */
	cb_listing_xref = 0;

	for (;;) {
		codegen_internal (current_program, subsequent_call);
		if (!current_program->next_program) {
			break;
		}
		subsequent_call = 1;
		/* set has_global_file if needed, only for sub-programs of this program */
		if (current_program->flag_file_global
		 && current_program->next_program->nested_level >
		    current_program->nested_level) {
			if (!has_global_file) {
				has_global_file = 1 ;
				has_global_file_level = current_program->nested_level ;
			}
		} else {
			if (has_global_file
			 && current_program->next_program->nested_level <=
			    has_global_file_level ){
				has_global_file = 0 ;
			}
		}
		current_program = current_program->next_program;
	}
	current_program = prog;
	cb_listing_xref = set_xref;

	codegen_finalize ();
}

void
codegen_init (struct cb_program *prog, const char *translate_name)
{
	char timestamp_buffer[COB_MINI_BUFF];

	current_program = prog;
	current_section = NULL;
	current_paragraph = NULL;
	current_statement = NULL;
	cb_source_line = 0;

	output_line_number = 1;
	output_name = (char*)translate_name;
	/* escape output name for C string */
	if (strchr (output_name, '\\')) {
		char buff[COB_MEDIUM_BUFF];
		int pos = 0;
		char* s;
		for (s = output_name; *s; s++) {
			if (*s == '\\') {
				buff[pos++] = '\\';
			}
			buff[pos++] = *s;
		}
		buff[pos] = 0;
		output_name = cobc_check_string (buff);
	}
	gen_ascii_ebcdic = 0;
	gen_ebcdic_ascii = 0;
	gen_native = 0;
	gen_figurative = 0;
	non_nested_count = 0;
	working_mem = 0;
	pic_cache = NULL;
	base_cache = NULL;
	globext_cache = NULL;
	field_cache = NULL;

	{
		struct cb_program* cp;
		for (cp = prog; cp; cp = cp->next_program) {
			struct cb_program	*xp = current_prog;
			current_prog = cp;
			emit_base_symbols (cp);
			current_prog = xp;
		}
	}

	strftime (timestamp_buffer, (size_t)COB_MINI_MAX,
		"%b %d %Y %H:%M:%S", &current_compile_tm);

	output_target = yyout;
	output_header (timestamp_buffer, NULL);
	output_target = cb_storage_file;
	output_header (timestamp_buffer, NULL);
	{
		struct cb_program *cp;
		for (cp = prog; cp; cp = cp->next_program) {
			if (cp->flag_prototype) {
				continue;
			}
			output_target = cp->local_include->local_fp;
			output_header (timestamp_buffer, cp);
		}
	}
	output_target = yyout;

	output_standard_includes (prog);
	output_gnucobol_defines (timestamp_buffer);

	output_newline ();
#if defined(HAVE_SIGACTION) && !defined(_WIN32)
	if (cb_flag_dump) {
		output_line ("/* Variables to catch abort during DUMP */");
		output_line ("#include <signal.h>");
		output_line ("static jmp_buf save_sig_env;");
		output_line ("static void catch_sig_jmp (int sig)");
		output_line ("{ longjmp(save_sig_env, sig); }");
		output_newline ();
	}
#endif
	output_line ("/* Global variables */");
	output ("#include \"%s\"", cb_storage_file_name);
	output_newline ();
	output_newline ();

	output_function_prototypes (prog);
}

/* Check matching version via constructor attribute / DllMain */
static void output_so_load_version_check (struct cb_program *prog)
{
#if defined (_WIN32)
	if (prog->flag_main) {
		return;
	}
	output_newline ();
	output_line ("#include \"windows.h\"");
	output_line ("BOOL WINAPI DllMain (HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved);");
	output_line ("BOOL WINAPI DllMain (HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved)");
	output_block_open ();
	output_line ("if (fdwReason == DLL_PROCESS_ATTACH)");
	output_line ("\tcob_check_version (COB_SOURCE_FILE, COB_PACKAGE_VERSION, COB_PATCH_LEVEL);");
	output_line ("return TRUE;");
	output_block_close ();
	output_newline ();
#else
	output_newline ();
	output_line ("static void gc_module_so_init () __attribute__ ((constructor));");
	output_line ("static void gc_module_so_init ()");
	output_block_open ();
	output_line ("cob_check_version (COB_SOURCE_FILE, COB_PACKAGE_VERSION, COB_PATCH_LEVEL);");
	output_block_close ();
	output_newline ();
#endif
}

void
codegen_internal (struct cb_program *prog, const int subsequent_call)
{
	cb_tree			l;
	int			i;

	struct cb_report *rep;

	/* skip prototypes */
	if (prog->flag_prototype) {
		return;
	}

	/* Clear local program stuff */
	current_prog = prog;
	param_id = 0;
	stack_id = 0;
	num_cob_fields = 0;
	progid = 0;
	loop_counter = 0;
	output_indent_level = 0;
	last_line = 0;
	needs_exit_prog = cb_flag_c_labels;	/* always add the exit _label_ in this case */
	gen_custom = 0;
	gen_nested_tab = 0;
	gen_dynamic = 0;
	local_mem = 0;
	local_working_mem = 0;
	need_save_exception = 0;
	last_segment = 0;
	last_section = NULL;
	call_cache = NULL;
	func_call_cache = NULL;
	static_call_cache = NULL;
	label_cache = NULL;
	local_base_cache = NULL;
	local_field_cache = NULL;
	inside_check = 0;
	for (i = 0; i < COB_INSIDE_SIZE; ++i) {
		inside_stack[i] = 0;
	}
	excp_current_program_id = prog->orig_program_id;
	excp_current_section = NULL;
	excp_current_paragraph = NULL;
	i_len_used = 0;
	memset ((void *)i_counters, 0, sizeof (i_counters));

	output_target = yyout;
	cb_local_file = prog->local_include->local_fp;

	output_class_names (prog);

	/* Main function */
	if (prog->flag_main) {
		output_main_function (prog);
	}

	/* Functions */
	if (!subsequent_call) {
		output ("/* Functions */");
		output_newline ();
		if (cb_flag_use_constructor == 1) {
			output_so_load_version_check (prog);
		}
	}

	if (prog->prog_type == COB_MODULE_TYPE_FUNCTION) {
		output_line ("/* FUNCTION-ID '%s' */", prog->orig_program_id);
		output_newline ();
		for (l = prog->entry_list; l; l = CB_CHAIN (l)) {
			output_function_entry_function (prog, l, 1);
		}
	} else {
		output_line ("/* PROGRAM-ID '%s' */", prog->orig_program_id);
		output_newline ();
		for (l = prog->entry_list; l; l = CB_CHAIN (l)) {
			output_entry_function (prog, l, prog->parameter_list, 1);
			progid++;
		}
	}

	/* Report data fields */
	if (prog->report_storage) {
		for (l = prog->report_list; l; l = CB_CHAIN (l)) {
			if (!CB_VALUE (l)) {
				continue;
			}
			rep = CB_REPORT_PTR (CB_VALUE(l));
			if (rep) {
				count_all_fields (rep->records);
			}
		}
	}

	output_internal_function (prog, prog->parameter_list);

	if (!prog->next_program) {
		output_line ("/* End functions */");
		output_newline ();
	}

	if (((gen_native | gen_ascii_ebcdic) > 1)
	 || gen_ebcdic_ascii || prog->alphabet_name_list) {
		(void)lookup_attr (COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL, 0);
	}

	output_target = cb_storage_file;

	/* Program local stuff */

	output_call_cache ();
	output_nested_call_table (prog);

	output_local_indexes ();
	output_perform_times_counters ();
	output_local_implicit_fields ();
	output_debugging_fields (prog);
	output_local_storage_pointer (prog);
	output_call_parameter_stack_pointers (prog);
	output_frame_stack (prog);
	output_dynamic_field_function_id_pointers ();

	if (prog->report_storage) {
		output_target = prog->local_include->local_fp;
		for (l = prog->report_list; l; l = CB_CHAIN (l)) {
			if (!CB_VALUE (l)) {
				continue;
			}
			rep = CB_REPORT_PTR (CB_VALUE(l));
			if (rep) {
				output_report_sum_control_field (rep->records);
			}
		}
	}

	output_local_base_cache ();
	output_local_fields (prog);
	output_cob_prof_data (prog);

	/* Report data fields */
	if (prog->report_storage) {
		int comment_gen = 0;
		for (l = prog->report_list; l; l = CB_CHAIN (l)) {
			if (!CB_VALUE (l)) {
				continue;
			}
			rep = CB_REPORT_PTR (CB_VALUE(l));
			if (rep) {
				if (!comment_gen) {
					comment_gen = 1;
					output_target = prog->local_include->local_fp;
					output_local ("\n/* Report data fields */\n\n");
				}
				output_emit_field (rep->line_counter, NULL);
				output_emit_field (rep->page_counter, NULL);
				report_col_pos = 1;
				output_report_data (rep->records);
				output_local ("\n");
			}
		}
		if (comment_gen) {
			output_local ("\n");
			output_target = cb_storage_file;
		}
	}

	/* Reports */
	if (prog->report_list) {
		/* Switch to local storage file */
		output_target = prog->local_include->local_fp;
		optimize_defs[COB_SET_REPORT] = 1;
		output_local ("\n/* Reports */\n");
		output_report_list (prog->report_list, CB_CHAIN (prog->report_list));
		output_local ("\n/* End of Reports */\n");
		/* Switch to main storage file */
		output_target = cb_storage_file;
	}

	/* Decimal constants */
	{
		struct literal_list* m = literal_cache;
		int comment_gen = 0;
		for (; m; m = m->next) {
			if (m->make_decimal) {
				if (!comment_gen) {
					comment_gen = 1;
					output_local ("\n/* Decimal constants */\n");
				}
				output_local ("static\tcob_decimal\t%s%d;\n",
						CB_PREFIX_DEC_FIELD, m->id);
				output_local ("static\tcob_decimal\t*%s%d = NULL;\n",
						CB_PREFIX_DEC_CONST, m->id);
			}
		}
		if (comment_gen) {
			output_local ("\n");
		}
	}
}

void
codegen_finalize (void)
{
	/* Finalize the main include file */

	if (!cobc_flag_main && non_nested_count > 1) {
		output_storage ("\n/* Module reference count */\n");
		output_storage ("static unsigned int\t\tcob_reference_count = 0;\n");
	}

	output_storage ("\n/* Module path */\n");
	output_storage ("static const char\t\t*cob_module_path = NULL;\n");

	output_globext_cache ();
	output_nonlocal_base_cache ();
	output_pic_cache ();
	output_attributes ();
	output_nonlocal_field_cache ();
	output_literals_figuratives_and_constants ();
	output_collating_tables ();
	output_string_cache ();
	output_source_cache ();

	/* Optimizer output */
	{
		enum cb_optim		optidx;
		for (optidx = COB_OPTIM_MIN; optidx < COB_OPTIM_MAX; ++optidx) {
			if (optimize_defs[optidx]) {
				cob_gen_optim (optidx);
				output_storage ("\n");
			}
		}
	}

	/* Clean up by clearing these */
	attr_cache = NULL;
	literal_cache = NULL;
	string_cache = NULL;
	string_id = 1;
	source_cache = NULL;
	source_id = 1;
}


#ifndef	HAVE_DESIGNATED_INITS
void
cobc_init_codegen (void)
{
	cb_statement_enum_name[STMT_UNKNOWN] = CB_STRINGIFY (STMT_UNKNOWN);
#define COB_STATEMENT(ename,str) \
	cb_statement_enum_name[ename] = CB_STRINGIFY (ename);
#include "../libcob/statement.def"
#undef COB_STATEMENT
}
#endif

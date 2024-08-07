/*
   Copyright (C) 2001-2023 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Simon Sobisch, Ron Norman,
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


#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#ifdef	HAVE_STRINGS_H
#include <strings.h>
#endif
#include <ctype.h>
#include <limits.h>
#ifndef LLONG_MAX
#ifdef LONG_LONG_MAX
#define LLONG_MAX LONG_LONG_MAX
#define ULLONG_MAX ULONG_LONG_MAX
#elif defined _I64_MAX
#define LLONG_MAX _I64_MAX
#define ULLONG_MAX _UI64_MAX
#else
#error compiler misses maximum for 64bit integer
#endif
#endif

#include "cobc.h"
#include "../libcob/coblocal.h"
#include "tree.h"
#define _PARSER_H	/* work around bad Windows SDK header */
#include "parser.h"

#define PIC_ALPHABETIC		0x01
#define PIC_NUMERIC		0x02
#define PIC_NATIONAL		0x04
#define PIC_EDITED		0x08
#define PIC_NUMERIC_FLOATING		0x10
#define PIC_ALPHANUMERIC	(PIC_ALPHABETIC | PIC_NUMERIC)
#define PIC_ALPHABETIC_EDITED	(PIC_ALPHABETIC | PIC_EDITED)
#define PIC_ALPHANUMERIC_EDITED	(PIC_ALPHANUMERIC | PIC_EDITED)
#define PIC_NUMERIC_EDITED	(PIC_NUMERIC | PIC_EDITED)
#define PIC_FLOATING_EDITED	(PIC_NUMERIC | PIC_NUMERIC_FLOATING | PIC_EDITED)
#define PIC_UTF8			(PIC_ALPHANUMERIC)	/* TODO: handle separately */
#define PIC_NATIONAL_EDITED	(PIC_NATIONAL | PIC_EDITED)

/* Local variables */

static const enum cb_class category_to_class_table[] = {
	CB_CLASS_UNKNOWN,	/* CB_CATEGORY_UNKNOWN */
	CB_CLASS_ALPHABETIC,	/* CB_CATEGORY_ALPHABETIC */
	CB_CLASS_ALPHANUMERIC,	/* CB_CATEGORY_ALPHANUMERIC */
	CB_CLASS_ALPHANUMERIC,	/* CB_CATEGORY_ALPHANUMERIC_EDITED */
	CB_CLASS_BOOLEAN,	/* CB_CATEGORY_BOOLEAN */
	CB_CLASS_INDEX,		/* CB_CATEGORY_INDEX */
	CB_CLASS_NATIONAL,	/* CB_CATEGORY_NATIONAL */
	CB_CLASS_NATIONAL,	/* CB_CATEGORY_NATIONAL_EDITED */
	CB_CLASS_NUMERIC,	/* CB_CATEGORY_NUMERIC */
	CB_CLASS_ALPHANUMERIC,	/* CB_CATEGORY_NUMERIC_EDITED */
	CB_CLASS_OBJECT,	/* CB_CATEGORY_OBJECT_REFERENCE */
	CB_CLASS_POINTER,	/* CB_CATEGORY_DATA_POINTER */
	CB_CLASS_POINTER	/* CB_CATEGORY_PROGRAM_POINTER */
};

static int category_is_alphanumeric[] = {
	0,	/* CB_CATEGORY_UNKNOWN */
	1,	/* CB_CATEGORY_ALPHABETIC */
	1,	/* CB_CATEGORY_ALPHANUMERIC */
	1,	/* CB_CATEGORY_ALPHANUMERIC_EDITED */
	0,	/* CB_CATEGORY_BOOLEAN */
	0,	/* CB_CATEGORY_INDEX */
	0,	/* CB_CATEGORY_NATIONAL */
	0,	/* CB_CATEGORY_NATIONAL_EDITED */
	0,	/* CB_CATEGORY_NUMERIC */
	1,	/* CB_CATEGORY_NUMERIC_EDITED */
	0,	/* CB_CATEGORY_OBJECT_REFERENCE */
	0,	/* CB_CATEGORY_DATA_POINTER */
	0	/* CB_CATEGORY_PROGRAM_POINTER */
};
static int category_is_national[] = {
	0,	/* CB_CATEGORY_UNKNOWN */
	0,	/* CB_CATEGORY_ALPHABETIC */
	0,	/* CB_CATEGORY_ALPHANUMERIC */
	0,	/* CB_CATEGORY_ALPHANUMERIC_EDITED */
	0,	/* CB_CATEGORY_BOOLEAN */
	0,	/* CB_CATEGORY_INDEX */
	1,	/* CB_CATEGORY_NATIONAL */
	1,	/* CB_CATEGORY_NATIONAL_EDITED */
	0,	/* CB_CATEGORY_NUMERIC */
	0,	/* CB_CATEGORY_NUMERIC_EDITED */
	0,	/* CB_CATEGORY_OBJECT_REFERENCE */
	0,	/* CB_CATEGORY_DATA_POINTER */
	0	/* CB_CATEGORY_PROGRAM_POINTER */
};


/* note: integrating cached integers help to decrease memory usage for
         compilation of source with many similar integer values,
		 but leads to a slow-down of 2-40%, depending how many identical
		 integer values are cached/searched
*/
#ifndef CACHED_INTEGERS
#define CACHED_INTEGERS 0
#endif
#if CACHED_INTEGERS
struct int_node {
	struct int_node	*next;
	struct cb_integer *node;
};
static struct int_node		*int_node_table = NULL;
#ifdef USE_INT_HEX /* Simon: using this increases the struct and we
		 *should* pass the flags as constants in any case... */
static struct int_node		*int_node_table_hex = NULL;
#endif
#endif

static char			*scratch_buff = NULL;
static int			filler_id = 1;
static int			class_id = 0;
static int			toplev_count;
static int			after_until = 0;
static char			err_msg[COB_MINI_BUFF];
static struct cb_program	*container_progs[64];
static const char		* const cb_const_subs[] = {
	"i0",
	"i1",
	"i2",
	"i3",
	"i4",
	"i5",
	"i6",
	"i7",
	"i8",
	"i9",
	"i10",
	"i11",
	"i12",
	"i13",
	"i14",
	"i15",
	NULL
};

static const struct cb_intrinsic_table	userbp =
	{ "USER FUNCTION", "cob_user_function",
	  CB_INTR_USER_FUNCTION, USER_FUNCTION_NAME, 1, 0, 0, CB_CATEGORY_NUMERIC,
	  0 };

/* Global variables */

/* Constants */

cb_tree cb_any;
cb_tree cb_true;
cb_tree cb_false;
cb_tree cb_null;
cb_tree cb_zero;
cb_tree cb_one;
cb_tree cb_space;
cb_tree cb_low;
cb_tree cb_high;
cb_tree cb_norm_low;
cb_tree cb_norm_high;
cb_tree cb_quote;
cb_tree cb_int0;
cb_tree cb_int1;
cb_tree cb_int2;
cb_tree cb_int3;
cb_tree cb_int4;
cb_tree cb_int5;
cb_tree cb_int6;
cb_tree cb_int7;
cb_tree cb_int8;
cb_tree cb_int16;
cb_tree cb_i[COB_MAX_SUBSCRIPTS];
cb_tree cb_error_node;

cb_tree cb_intr_whencomp = NULL;

cb_tree cb_standard_error_handler = NULL;

unsigned int	gen_screen_ptr = 0;

#ifdef	HAVE_DESIGNATED_INITS
#define COB_STATEMENT(ename,str)	, [ename] = str
const char	*cb_statement_name[STMT_MAX_ENTRY] = {
	[STMT_UNKNOWN] = "UNKNOWN"
	/* note: STMT_MAX_ENTRY left out here */
#include "../libcob/statement.def"
};
#undef COB_STATEMENT
#else
const char	*cb_statement_name[STMT_MAX_ENTRY];
#endif

#if 0 /* TODO remove if not needed */
static	int	save_expr_line = 0;
static	char	*save_expr_file = NULL;
#endif
static	cb_tree cb_zero_lit;
static	int	prev_expr_line = 0;
static	int	prev_expr_pos = 0;
#define EXPR_WARN_PER_LINE 8
static	int	prev_expr_warn[EXPR_WARN_PER_LINE] = {0,0,0,0,0,0,0,0};
static	int	prev_expr_tf[EXPR_WARN_PER_LINE] = {0,0,0,0,0,0,0,0};

/* Local functions */

static int
was_prev_warn (int linen, int tf)
{
	int	i;
	if (cb_exp_line != prev_expr_line) {
		prev_expr_line = cb_exp_line;
		for (i = 0; i < EXPR_WARN_PER_LINE; i++) {
			prev_expr_warn[i] = 0;
			prev_expr_tf[i] = -9999;
		}
	}
	for (i=0; i < EXPR_WARN_PER_LINE; i++) {
		if (prev_expr_warn[i] == linen) {
			if (tf < 0
			 && prev_expr_tf[i] == -tf) {
				return 1;
			}
			if (prev_expr_tf[i] == tf) {
				return 1;
			}
			prev_expr_tf [i] = tf;
			return 0;
		}
	}
	prev_expr_pos = (prev_expr_pos + 1) % EXPR_WARN_PER_LINE;
	prev_expr_warn [prev_expr_pos] = linen;
	prev_expr_tf [prev_expr_pos] = tf;
	return 0;
}

/* get best position (note: in the case of constants y/x point to DATA-DIVISION) */
static void
copy_file_line (cb_tree e, cb_tree y, cb_tree x)
{
	if (y == cb_zero || x == cb_zero) {
		e->source_line = prev_expr_line = cb_exp_line;
		e->source_file = cb_source_file;
	} else if (y && x && y->source_line > x->source_line) {
		e->source_file = y->source_file;
		e->source_line = y->source_line;
		e->source_column = y->source_column;
#if 0 /* TODO remove if not needed */
		save_expr_file = (char *)y->source_file;
		save_expr_line = y->source_line;
#endif
	} else if (!x && y && y->source_line) {
		e->source_file = y->source_file;
		e->source_line = y->source_line;
		e->source_column = y->source_column;
#if 0 /* TODO remove if not needed */
		save_expr_file = (char *)e->source_file;
		save_expr_line = e->source_line;
#endif
	} else if (x && x->source_line) {
		e->source_file = x->source_file;
		e->source_line = x->source_line;
		e->source_column = x->source_column;
#if 0 /* TODO remove if not needed */
		save_expr_file = (char *)e->source_file;
		save_expr_line = e->source_line;
	} else if (y || x) {
		e->source_line = cb_exp_line;
		e->source_file = cb_source_file;
	} else if (save_expr_line) {
		e->source_file = save_expr_file;
		e->source_line = save_expr_line;
#endif
	} else {
		e->source_line = cb_exp_line;
		e->source_file = cb_source_file;
	}
}

/* compute hash value of COBOL word */
static size_t
word_hash (const unsigned char *s)
{
	size_t	val;
	size_t	pos;

	/* Hash a name */
	/* We multiply by position to get a better distribution */
	val = 0;
	pos = 1;
	for (; *s; s++, pos++) {
		val += *s * pos;
	}
#if	0	/* RXWRXW - Hash remainder */
	return val % CB_WORD_HASH_SIZE;
#endif
	return val & CB_WORD_HASH_MASK;
}

/* look up word (case insensitive) */
static void
lookup_word (struct cb_reference *p, const char *name)
{
	struct cb_word	*w;
	size_t		val;

	/* build uppercase variant (we don't want the hash to differentiate those) */
	unsigned char word[COB_MAX_WORDLEN + 1];
	{
		size_t i;
		size_t len = strlen (name);
		if (len > COB_MAX_WORDLEN) {
#if 0	/* leave to post-processing for now, just cut for the hash function */
			cobc_err_msg ("unexpected word length: %u", (unsigned int)len);
			COBC_ABORT ();
#else
			len = COB_MAX_WORDLEN;
#endif
		}
			for (i = 0; i < len; ++i) {
				word[i] = (cob_u8_t)toupper ((unsigned char)name[i]);
			}
			word[i] = 0;
		}
	val = word_hash (word);

	/* Find an existing word */
	if (likely(current_program)) {
		/* checking only "very similar" words that share the same hash */
		for (w = current_program->word_table[val]; w; w = w->next) {
#if 1	/* TODO we currently use words "as written first" use an all-upper
		   approach post 3.1 */
			if (strcasecmp (w->name, name) == 0) {
#else
			if (strcmp (w->name, (char *)word) == 0) {
#endif
				p->word = w;
				p->hashval = val;
				p->flag_duped = 1;
				return;
			}
		}
	}

	/* Create new word */
	w = cobc_parse_malloc (sizeof (struct cb_word));
#if 1	/* TODO we currently use words "as written first" use an all-upper
		   approach post 3.1 */
	w->name = cobc_parse_strdup (name);
#else
	w->name = cobc_parse_strdup ((char *)word);
#endif

	/* Insert it into the table */
	if (likely(current_program)) {
		w->next = current_program->word_table[val];
		current_program->word_table[val] = w;
	}
	p->word = w;
	p->hashval = val;
}

#define CB_FILE_ERR_REQUIRED	1
#define CB_FILE_ERR_INVALID_FT	2
#define CB_FILE_ERR_INVALID		3

static void
file_error (cb_tree name, const char *clause, const char errtype)
{
	switch (errtype) {
	case CB_FILE_ERR_REQUIRED:
		cb_error_x (name, _("%s clause is required for file '%s'"),
			clause, CB_NAME (name));
		break;
	case CB_FILE_ERR_INVALID_FT:
		cb_error_x (name, _("%s clause is invalid for file '%s' (file type)"),
			clause, CB_NAME (name));
		break;
	case CB_FILE_ERR_INVALID:
		cb_error_x (name, _("%s clause is invalid for file '%s'"),
			clause, CB_NAME (name));
		break;
	}
}


static void
check_code_set_items_are_subitems_of_records (struct cb_file * const file)
{
	struct cb_list		*l;
	cb_tree			r;
	struct cb_field		*f;
	cb_tree			first_ref = NULL;
	struct cb_field		*first_record = NULL;
	struct cb_field		*current_record;

	/*
	  Check each item belongs to this FD, is not a record and are all in the
	  same record.
	 */
	for (l = file->code_set_items; l; l = CB_LIST (l->chain)) {

		r = l->value;
		f = CB_FIELD (cb_ref (r));

		if (f->level == 1) {
			cb_error_x (r, _("FOR item '%s' is a record"),
				    cb_name (r));
		}

		for (current_record = f; current_record->parent;
		     current_record = current_record->parent);

		if (first_ref) {
			if (current_record != first_record) {
				cb_error_x (r, _("FOR item '%s' is in different record to '%s'"),
					    cb_name (r), cb_name (first_ref));
			}
		} else {
			first_ref = r;
			first_record = current_record;
		}

		if (current_record->file != file) {
			cb_error_x (r, _("FOR item '%s' is not in a record associated with '%s'"),
				    cb_name (r), cb_name (CB_TREE (file)));
		}

		if (!l->chain) {
			break;
		}
	}
}

/* Tree */

static void *
make_tree (const enum cb_tag tag, const enum cb_category category,
	   const size_t size)
{
	cb_tree x;

	x = cobc_parse_malloc (size);
	x->tag = tag;
	x->category = category;
	return x;
}

static cb_tree
make_constant (const enum cb_category category, const char *val)
{
	struct cb_const *p;

	p = make_tree (CB_TAG_CONST, category, sizeof (struct cb_const));
	p->val = val;
	return CB_TREE (p);
}

static cb_tree
make_constant_label (const char *name)
{
	struct cb_label *p;

	p = CB_LABEL (cb_build_label (cb_build_reference (name), NULL));
	p->flag_begin = 1;
	return CB_TREE (p);
}

/* snip literal for output, if too long or,
   unlikely error case, has a line break;
   'buff' to write into with a size of at least CB_ERR_LITMAX + 1
   'literal_data' to get data from */
char *
literal_for_diagnostic (char *buff, const char *literal_data) {

	const size_t size = strlen (literal_data);
	char *bad_pos;

	if (size < CB_ERR_LITMAX) {
		memcpy (buff, literal_data, size + 1);
	} else {
		memcpy (buff, literal_data, CB_ERR_LITMAX - 1);
		buff[CB_ERR_LITMAX] = '\0';
	}

	/* this previously happened because of a bug in pplex.l;
	   as this is a seldom-called function and only inspects
	   up to CB_ERR_LITMAX chars here, we leave this in as
	   initializer for 'bad_pos' and as additional security net */
	bad_pos = strchr (buff, '\n');

	if ( size >= CB_ERR_LITMAX
	 || (bad_pos && bad_pos - buff + 4 > CB_ERR_LITMAX)) {
		char *long_pos = buff + CB_ERR_LITMAX - 4;
		if (!bad_pos
		 || bad_pos > long_pos) {
			bad_pos = long_pos;
		}
	}

	if (bad_pos) {
		strcpy (bad_pos, " ...");
	}
	return buff;
}

/* Recursively find/generate a name for the object x. */
static size_t
cb_name_1 (char *s, cb_tree x, const int size)
{
	const char			*orig = s;
	size_t size_real;

	if (!x) {
		size_real = snprintf (s, size, "(void pointer)");
		goto game_over;
	}
	switch (CB_TREE_TAG (x)) {
	case CB_TAG_CONST:
		if (x == cb_any) {
			size_real = snprintf (s, size, "ANY");
		} else if (x == cb_true) {
			size_real = snprintf (s, size, "TRUE");
		} else if (x == cb_false) {
			size_real = snprintf (s, size, "FALSE");
		} else if (x == cb_null) {
			size_real = snprintf (s, size, "NULL");
		} else if (x == cb_zero) {
			size_real = snprintf (s, size, "ZERO");
		} else if (x == cb_space) {
			size_real = snprintf (s, size, "SPACE");
		} else if (x == cb_low || x == cb_norm_low) {
			size_real = snprintf (s, size, "LOW-VALUE");
		} else if (x == cb_high || x == cb_norm_high) {
			size_real = snprintf (s, size, "HIGH-VALUE");
		} else if (x == cb_quote) {
			size_real = snprintf (s, size, "QUOTE");
		} else if (x == cb_error_node) {
			size_real = snprintf (s, size, "%s", _("internal error node"));
		} else {
			size_real = snprintf (s, size, "%s", _("unknown constant"));
		}
		break;

	case CB_TAG_LITERAL:
		/* should only be called for diagnostic messages,
		   so limit as in scanner.l:  */
		if (CB_TREE_CLASS (x) == CB_CLASS_NUMERIC) {
			size_real = snprintf (s, size, "%s", (char *)CB_LITERAL (x)->data);
		} else {
			char	lit_buff[CB_ERR_LITMAX + 1] = { 0 };
			size_real = snprintf (s, size, _("literal \"%s\""),
				literal_for_diagnostic (lit_buff, (char *)CB_LITERAL (x)->data));
		}
		break;

	case CB_TAG_FIELD: {
		const struct cb_field *f = CB_FIELD (x);
		if (f->flag_filler) {
			size_real = snprintf (s, size, "FILLER");
		} else {
			size_real = snprintf (s, size, "%s", f->name);
		}
		break;
	}

	case CB_TAG_REFERENCE: {
		struct cb_reference *p = CB_REFERENCE (x);
		char buff[COB_SMALL_BUFF];
		size_t size_element;
		if (p->flag_filler_ref) {
			size_real = snprintf (s, size, "FILLER");
		} else {
			size_real = snprintf (s, size, "%s", p->word->name);
		}
		if (size_real > size) goto game_over;
		s += size_real;
		if (p->subs && CB_VALUE(p->subs) != cb_int1) {
			cb_tree		l;
			char	*s_orig = s;
			if (size_real + 5 > size) {
				/* drop that " (X[,Y ...]) */
				return size_real;
			}
			size_element = sprintf (s, " (");
			size_real += size_element;
			s += size_element;
			p->subs = cb_list_reverse (p->subs);
			for (l = p->subs; l; l = CB_CHAIN (l)) {
				size_element = cb_name_1 (buff, CB_VALUE (l), COB_SMALL_BUFF);
				if (size_real + size_element + 2 > size) {
					/* replacement: "(X[,Y ...])" */
					size_element = sprintf (s_orig, "(<>");
					s = s_orig + size_element;
					break;
				}
				size_element = sprintf (s, "%s%s", buff, CB_CHAIN (l) ? ", " : "");
				size_real += size_element;
				s += size_element;
			}
			p->subs = cb_list_reverse (p->subs);
			s += sprintf (s, ")");
			size_real = s - orig;
		}
		if (p->offset) {
			size_t	size_refmod;
			size_element = cb_name_1 (buff, p->offset, COB_SMALL_BUFF);
			if (size_real + size_element + 6 >= size) {
				/* drop that " (X:Y) [in Z]" */
				return size_real;
			}
			if (p->length) {
				size_refmod = sprintf (s, " (%s:", buff);
				size_element = cb_name_1 (buff, p->length, COB_SMALL_BUFF);
				if (size_real + size_refmod + size_element + 1  >= size) {
					/* replacement: "(X:Y)" (dropping possible "in XYZ") */
					size_element = sprintf (s, "(<>:)");
					return size_real + size_element;	
				}
				size_refmod += sprintf (s + size_refmod, "%s)", buff);
				s += size_refmod;
			} else {
				size_refmod = sprintf (s, " (%s:)", buff);
			}
			size_real += size_refmod;
			s += size_refmod;
		}
		if (p->chain) {
			size_element = cb_name_1 (buff, p->chain, COB_SMALL_BUFF);
			if (size_real + size_element + 4 >= size) {
				return s - orig;	/* drop that " in XYZ" */
			}
			s += sprintf (s, " in %s", buff);
		}
		return s - orig;
	}

	case CB_TAG_LABEL:
		size_real = snprintf (s, size, "%s", (char *)(CB_LABEL (x)->name));
		break;

	case CB_TAG_ALPHABET_NAME:
		size_real = snprintf (s, size, "%s", CB_ALPHABET_NAME (x)->name);
		break;

	case CB_TAG_CLASS_NAME:
		size_real = snprintf (s, size, "%s", CB_CLASS_NAME (x)->name);
		break;

	case CB_TAG_LOCALE_NAME:
		size_real = snprintf (s, size, "%s", CB_LOCALE_NAME (x)->name);
		break;

	case CB_TAG_PROTOTYPE:
		size_real = snprintf (s, size, "%s", (char*)(CB_PROTOTYPE (x)->name));
		break;

	case CB_TAG_BINARY_OP: {
		const struct cb_binary_op *cbop = CB_BINARY_OP (x);
		char	buff [COB_SMALL_BUFF];
		size_t	size_element;
		if (cbop->op == '@') {
			size_element = cb_name_1 (buff, cbop->x, COB_SMALL_BUFF);
			if (size_element + 3 >= size) {
				size_real = snprintf (s, size, "<@OP>");
				goto game_over;
			}
			return sprintf (s, "(%s)", buff);
		} else if (cbop->op == '!') {
			size_element = cb_name_1 (buff, cbop->x, COB_SMALL_BUFF);
			if (size_element + 1 >= size) {
				size_real = snprintf (s, size, "<!OP>");
				goto game_over;
			}
			return sprintf (s, "!%s", buff);
		} else {
			size_element = cb_name_1 (buff, cbop->x, COB_SMALL_BUFF);
			if (size_element + 6 >= size) {
				size_real = snprintf (s, size, "<OP %c>", cbop->op);
				goto game_over;
			}
			size_real = sprintf (s, "(%s %c ", buff, cbop->op);
			size_element = cb_name_1 (buff, cbop->y, COB_SMALL_BUFF);
			if (size_element + size_real + 1 >= size) {
				size_real = snprintf (s, size, "<OP %c>", cbop->op);
				goto game_over;
			}
			size_real += sprintf (s + size_real, " %s)", buff);
			return size_real;
		}
	}

	case CB_TAG_FUNCALL: {
		const struct cb_funcall *cbip = CB_FUNCALL (x);
		const int i_max = cbip->argc;
		int i;
		size_real = snprintf (s, size, "%s", cbip->name);
		if (size_real + 4 > size) goto game_over;
		s += size_real;
		for (i = 0; i < i_max; i++) {
			const size_t size_left = size - (s - orig);
			char *s_orig = s;
			size_t size_element;
			size_element = snprintf (s, size_left, (i == 0) ? "(" : ", ");
			size_element += cb_name_1 (s + size_element, cbip->argv[i], size_left);
			if (size_element > size_left + 4) {
				/* if we don't have enough room: go out leaving s unchanged */
				s_orig[0] = '\0';
				goto game_over;
			}
			size_real += size_element;
			s += size_element;
		}
		sprintf (s, ")");
		size_real++;
		break;
	}

	/* LCOV_EXCL_START */
	case CB_TAG_LIST: {
		cb_tree l;
		size_real = snprintf (s, size, "LIST");
		if (size_real + 4 > size) goto game_over;
		s += size_real;
		for (l = x; l; l = CB_CHAIN (l)) {
			const size_t size_left = size - (s - orig);
			char *s_orig = s;
			size_t size_element;
			size_element = snprintf (s, size_left, (l == x) ? ": " : ", ");
			size_element += cb_name_1 (s + size_element, CB_VALUE (l), size_left);
			if (size_element > size_left + 4) {
				/* if we don't have enough room: go out leaving s unchanged */
				s_orig[0] = '\0';
				goto game_over;
			}
			size_real += size_element;
			s += size_element;
		}
		sprintf (s, ")");
		size_real++;
		break;
	}
	/* LCOV_EXCL_STOP */

	/* LCOV_EXCL_START */
	case CB_TAG_TAB_VALS: {
		size_real = snprintf (s, size, "VALUE (table-format) ");
		size_real += cb_name_1 (s + size_real, CB_TAB_VALS (x)->values, size - size_real);
		break;
	}
	/* LCOV_EXCL_STOP */

	case CB_TAG_INTRINSIC: {
		const struct cb_intrinsic *cbit = CB_INTRINSIC (x);
		if (!cbit->isuser) {
			size_real = snprintf (s, size, "FUNCTION %s", cbit->intr_tab->name);
		} else
		if (cbit->name && CB_REFERENCE_P (cbit->name)
		 && CB_REFERENCE(cbit->name)->word) {
			size_real = snprintf (s, size, "USER FUNCTION %s", CB_REFERENCE (cbit->name)->word->name);
		} else {
			size_real = snprintf (s, size, "USER FUNCTION");
		}
		break;
	}

	case CB_TAG_FILE:
		size_real = snprintf (s, size, "FILE %s", CB_FILE (x)->name);
		break;

	case CB_TAG_REPORT:
		size_real = snprintf (s, size, "REPORT %s", CB_REPORT_PTR (x)->name);
		break;

	case CB_TAG_REPORT_LINE: {
		struct cb_reference *p;
		struct cb_field *f;
#if 1	/* FIXME: Why do we need the unchecked cast here? */
		p = (struct cb_reference *)x;
#else
		p = CB_REFERENCE (x);
#endif
		f = CB_FIELD (p->value);
		size_real = snprintf (s, size, "REPORT LINE %s", f->name);
		break;
	}

	case CB_TAG_CD:
		size_real = snprintf (s, size, "%s", CB_CD (x)->name);
		break;

	/* LCOV_EXCL_START */
	default:
		CB_TREE_TAG_UNEXPECTED_ABORT (x);
	}
	/* LCOV_EXCL_STOP */

game_over:
	/* when called recursive we could be truncated,
	   don't report more than we actually wr*/
	if (size_real >= size) {
		size_real = size - 1;
	}
	return size_real;
}

static cb_tree
make_intrinsic_typed (cb_tree name, const struct cb_intrinsic_table *cbp,
		const enum cb_category cat, cb_tree args,
		cb_tree field, cb_tree refmod, const int isuser)
{
	struct cb_intrinsic *x;

#if	0	/* RXWRXW Leave in, we may need this */
	cb_tree			l;
	for (l = args; l; l = CB_CHAIN(l)) {
		switch (CB_TREE_TAG (CB_VALUE(l))) {
		case CB_TAG_CONST:
		case CB_TAG_INTEGER:
		case CB_TAG_LITERAL:
		case CB_TAG_DECIMAL:
		case CB_TAG_FIELD:
		case CB_TAG_REFERENCE:
		case CB_TAG_INTRINSIC:
			break;
		default:
			/* untranslated until used, then with a better string than "tag %d" */
			cb_error ("FUNCTION '%s' has invalid/not supported arguments - tag %d",
				cbp->name, CB_TREE_TAG(l));
			return cb_error_node;
		}
	}
#endif

	x = make_tree (CB_TAG_INTRINSIC, cat, sizeof (struct cb_intrinsic));
	x->name = name;
	x->args = args;
	x->intr_tab = cbp;
	x->intr_field = field;
	x->isuser = isuser;
	if (refmod) {
		x->offset = CB_PAIR_X (refmod);
		x->length = CB_PAIR_Y (refmod);
	}
	return CB_TREE (x);
}


static cb_tree
make_intrinsic (cb_tree name, const struct cb_intrinsic_table *cbp,
		cb_tree args, cb_tree field, cb_tree refmod, const int isuser)
{
	return make_intrinsic_typed (name, cbp, cbp->category, args, field, refmod, isuser);
}

static cb_tree
global_check (struct cb_reference *r, cb_tree items, size_t *ambiguous)
{
	cb_tree			candidate;
	struct cb_field		*p;
	cb_tree			v;
	cb_tree			c;

	candidate = NULL;
	for (; items; items = CB_CHAIN (items)) {
		/* Find a candidate value by resolving qualification */
		v = CB_VALUE (items);
		c = r->chain;
		if (CB_FIELD_P (v)) {
			if (!CB_FIELD (v)->flag_is_global) {
				continue;
			}
			/* In case the value is a field, it might be qualified
			   by its parent names and a file name */
			if (CB_FIELD (v)->flag_indexed_by) {
				p = CB_FIELD (v)->index_qual;
			} else {
				p = CB_FIELD (v)->parent;
			}
			/* Resolve by parents */
			for (; p; p = p->parent) {
				if (c && strcasecmp (CB_NAME (c), p->name) == 0) {
					c = CB_REFERENCE (c)->chain;
				}
			}

			/* Resolve by file */
			if (c && CB_REFERENCE (c)->chain == NULL) {
				if (CB_WORD_COUNT (c) == 1 &&
				    CB_FILE_P (cb_ref (c)) &&
				    (CB_FILE (cb_ref (c)) == cb_field_founder (CB_FIELD (v))->file)) {
					c = CB_REFERENCE (c)->chain;
				}
			}
		}
		/* A well qualified value is a good candidate */
		if (c == NULL) {
			if (candidate == NULL) {
				/* Keep the first candidate */
				candidate = v;
			} else {
				/* Multiple candidates and possibly ambiguous */
				*ambiguous = 1;
			}
		}
	}
	return candidate;
}

static int
iso_8601_func (const enum cb_intr_enum intr)
{
	return intr == CB_INTR_FORMATTED_CURRENT_DATE
		|| intr == CB_INTR_FORMATTED_DATE
		|| intr == CB_INTR_FORMATTED_DATETIME
		|| intr == CB_INTR_FORMATTED_TIME
		|| intr == CB_INTR_INTEGER_OF_FORMATTED_DATE
		|| intr == CB_INTR_SECONDS_FROM_FORMATTED_TIME
		|| intr == CB_INTR_TEST_FORMATTED_DATETIME;
}

static int
valid_format (const enum cb_intr_enum intr, const char *format)
{
	char	decimal_point = current_program->decimal_point;

	/* Precondition: iso_8601_func (intr) */

	switch (intr) {
	case CB_INTR_FORMATTED_CURRENT_DATE:
		return cob_valid_datetime_format (format, decimal_point);
	case CB_INTR_FORMATTED_DATE:
		return cob_valid_date_format (format);
	case CB_INTR_FORMATTED_DATETIME:
		return cob_valid_datetime_format (format, decimal_point);
	case CB_INTR_FORMATTED_TIME:
		return cob_valid_time_format (format, decimal_point);
	case CB_INTR_INTEGER_OF_FORMATTED_DATE:
		return cob_valid_date_format (format)
			|| cob_valid_datetime_format (format, decimal_point);
	case CB_INTR_SECONDS_FROM_FORMATTED_TIME:
		return cob_valid_time_format (format, decimal_point)
			|| cob_valid_datetime_format (format, decimal_point);
	case CB_INTR_TEST_FORMATTED_DATETIME:
		return cob_valid_time_format (format, decimal_point)
			|| cob_valid_date_format (format)
			|| cob_valid_datetime_format (format, decimal_point);
	default:
		cb_error (_("invalid date/time function: '%d'"), intr);
		/* Ignore the content of the format */
		return 1;
	}
}

static const char *
try_get_constant_data (cb_tree val)
{
	if (val == NULL) {
		return NULL;
	} else if (CB_LITERAL_P (val)) {
		return (char *) CB_LITERAL (val)->data;
	} else if (CB_CONST_P (val)) {
		return CB_CONST (val)->val;
	} else {
		return NULL;
	}
}

static int
valid_const_date_time_args (const cb_tree tree, const struct cb_intrinsic_table *intr,
			    cb_tree args)
{
	cb_tree		arg = CB_VALUE (args);
	const char	*data;

	/* Precondition: iso_8601_func (intr->intr_enum) */

	data = try_get_constant_data (arg);
	if (data != NULL) {
		if (!valid_format (intr->intr_enum, data)) {
			cb_error_x (tree, _("FUNCTION '%s' has invalid date/time format"),
				intr->name);
			return 0;
		}
		return 1;
	}
	cb_warning_x (cb_warn_additional, tree,
		_("FUNCTION '%s' has format in variable"), intr->name);
	return 1;
}

static cb_tree
get_last_elt (cb_tree l)
{
	while (CB_CHAIN (l)) {
		l = CB_CHAIN (l);
	}
	return l;
}

static int
get_data_from_const (cb_tree const_val, unsigned char **data)
{
	if (const_val == cb_space) {
		*data = (unsigned char *)" ";
	} else if (const_val == cb_zero) {
		*data = (unsigned char *)"0";
	} else if (const_val == cb_quote) {
		if (cb_flag_apostrophe) {
			*data = (unsigned char *)"'";
		} else {
			*data = (unsigned char *)"\"";
		}
	} else if (const_val == cb_norm_low) {
		*data = (unsigned char *)"\0";
	} else if (const_val == cb_norm_high) {
		*data = (unsigned char *)"\255";
	} else if (const_val == cb_null) {
		*data = (unsigned char *)"\0";
	} else {
		return 1;
	}

	return 0;
}

static int
get_data_and_size_from_lit (cb_tree x, unsigned char **data, size_t *size)
{
	if (CB_LITERAL_P (x)) {
		*data = CB_LITERAL (x)->data;
		*size = CB_LITERAL (x)->size;
	} else if (CB_CONST_P (x)) {
		*size = 1;
		if (get_data_from_const (x, data)) {
			return 1;
		}
	} else {
		return 1;
	}

	return 0;
}

static struct cb_literal *
concat_literals (const cb_tree left, const cb_tree right)
{
	struct cb_literal	*p;
	unsigned char		*ldata;
	unsigned char		*rdata;
	size_t			lsize;
	size_t			rsize;

	if (get_data_and_size_from_lit (left, &ldata, &lsize)) {
		return NULL;
	}
	if (get_data_and_size_from_lit (right, &rdata, &rsize)) {
		return NULL;
	}

	p = make_tree (CB_TAG_LITERAL, left->category, sizeof (struct cb_literal));
	p->data = cobc_parse_malloc (lsize + rsize + 1U);
	p->size = lsize + rsize;

	memcpy (p->data, ldata, lsize);
	memcpy (p->data + lsize, rdata, rsize);

	return p;
}

static int
is_unconditionally_suppressed (const struct cb_field *record, cb_tree suppress_list)
{
	cb_tree	l;
	struct cb_ml_suppress_clause	*suppress_clause;

	for (l = suppress_list; l; l = CB_CHAIN (l)) {
		suppress_clause = CB_ML_SUPPRESS (CB_VALUE (l));
		if (!suppress_clause->when_list
		    && suppress_clause->target == CB_ML_SUPPRESS_IDENTIFIER
		    && cb_ref (suppress_clause->identifier) == CB_TREE (record)) {
			/*
			  This is indeed the only case we need to check - all
			  other SUPPRESS targets require a WHEN clause.
			*/
			return 1;
		}
	}

	return 0;
}

static cb_tree
get_ml_name (cb_tree record, cb_tree name_list, enum cb_ml_type type)
{
	cb_tree	l;
	cb_tree	name_pair;

	if (type == CB_ML_CONTENT) {
		return cb_null;
	}

	for (l = name_list; l; l = CB_CHAIN (l)) {
		name_pair = CB_VALUE (l);
		if (cb_ref (CB_PAIR_X (name_pair)) == record) {
		        return CB_PAIR_Y (name_pair);
		}
	}

	return cb_build_alphanumeric_literal (cb_name (record),
					      strlen (cb_name (record)));
}

static enum cb_ml_type
get_ml_type (cb_tree record, cb_tree type_list, const int default_to_attr)
{
	cb_tree	l;
	cb_tree	type_pair;

	for (l = type_list; l; l = CB_CHAIN (l)) {
		type_pair = CB_VALUE (l);
		if (cb_ref (CB_PAIR_X (type_pair)) == record) {
		        return (enum cb_ml_type) CB_INTEGER ((CB_PAIR_Y (type_pair)))->val;
		}
	}

	if (default_to_attr
	    && (!CB_FIELD (record)->children
		&& !CB_FIELD (record)->flag_filler
		&& !CB_FIELD (record)->flag_occurs)) {
		return CB_ML_ATTRIBUTE;
	} else {
		return CB_ML_ELEMENT;
	}
}

static int
is_target_of_suppress_identifier (cb_tree record, struct cb_ml_suppress_clause *clause)
{
	return clause->target == CB_ML_SUPPRESS_IDENTIFIER
		&& cb_ref (clause->identifier) == record;
}

static int
is_target_of_suppress_type (cb_tree record, enum cb_ml_type type,
			    struct cb_ml_suppress_clause *clause)
{
	if (clause->target != CB_ML_SUPPRESS_TYPE) {
		return 0;
	}

	if (clause->ml_type != CB_ML_ANY_TYPE
	    && clause->ml_type != type) {
		return 0;
	}

	if (clause->category == CB_ML_SUPPRESS_CAT_NUMERIC) {
		return cb_tree_category (record) == CB_CATEGORY_NUMERIC;
	} else if (clause->category == CB_ML_SUPPRESS_CAT_NONNUMERIC) {
		return cb_tree_category (record) != CB_CATEGORY_NUMERIC;
	} else { /* CB_ML_SUPPRESS_CAT_ANY */
		return 1;
	}
}

static cb_tree
build_condition_token_list (cb_tree record, cb_tree when_list)
{
	cb_tree	l;
	cb_tree	cond = NULL;
	cb_tree record_ref;

	for (l = when_list; l; l = CB_CHAIN (l)) {
		if (!cond) {
			record_ref = cb_build_field_reference (CB_FIELD (record), NULL);
			cond = cb_build_list (cb_int ('x'), record_ref, NULL);
		} else {
			cond = cb_build_list (cb_int ('|'), NULL, cond);
		}
		cond = cb_build_list (cb_int ('='), NULL, cond);
		cond = cb_build_list (cb_int ('x'), CB_VALUE (l), cond);
	}

	return cond;
}

static int
is_suppress_all_or_applicable_suppress_type (cb_tree record,
					     enum cb_ml_type type,
					     struct cb_ml_suppress_clause *suppress_clause)
{
	return suppress_clause->target == CB_ML_SUPPRESS_ALL
		|| is_target_of_suppress_type (record, type, suppress_clause);
}

static cb_tree
get_suppress_cond (cb_tree record, enum cb_ml_type type,
		   cb_tree suppress_list)
{
	cb_tree	l;
	struct cb_ml_suppress_clause	*suppress_clause;
	struct cb_ml_suppress_clause	*last_applicable_suppress_id = NULL;
	cb_tree suppress_cond = NULL;

	if (!record) {
		/* TODO: Output check that all child elements are suppressed */
		/* TODO: Move this check to the callee? */
		return NULL;
	}

	/*
	  Find the last SUPPRESS-identifier phrase which applies to record. Use
	  that if it exists.
	*/
	for (l = suppress_list; l; l = CB_CHAIN (l)) {
		suppress_clause = CB_ML_SUPPRESS (CB_VALUE (l));
		if (is_target_of_suppress_identifier (record, suppress_clause)) {
			last_applicable_suppress_id = suppress_clause;
		}
	}

	if (last_applicable_suppress_id) {
		suppress_cond = build_condition_token_list (record, last_applicable_suppress_id->when_list);
	} else {
		/*
		  If record is not the subject of a SUPPRESS-identifier phrase,
		  apply all the WHEN's from all the applicable generic SUPPRESS
		  phrases.
		 */
		for (l = suppress_list; l; l = CB_CHAIN (l)) {
			suppress_clause = CB_ML_SUPPRESS (CB_VALUE (l));
			if (!suppress_clause || !is_suppress_all_or_applicable_suppress_type (record, type, suppress_clause)) {
				continue;
			}

			suppress_cond = build_condition_token_list (record, suppress_clause->when_list);
		}
	}

	if (suppress_cond) {
		/* Convert list of tokens into actual condition */
	        suppress_cond = cb_build_cond (cb_build_expr (cb_list_reverse (suppress_cond)));
		cb_end_cond (suppress_cond);
	}

	return suppress_cond;
}

static void
append_to_tree_list (struct cb_ml_generate_tree * * const head,
		     struct cb_ml_generate_tree * * const tail,
		     struct cb_ml_generate_tree *x)
{
        if (*head) {
		(*tail)->sibling = x;
		x->prev_sibling = *tail;
	} else {
		*head = x;
		x->prev_sibling = NULL;
	}
	*tail = x;
}

static void
set_ml_attrs_and_children (struct cb_field *record, const int children_are_attrs,
			    cb_tree name_list, cb_tree type_list,
			    cb_tree suppress_list,
			    struct cb_ml_generate_tree * const * const tree)
{
	struct cb_field			*child;
	cb_tree			        child_tree_or_null;
	struct cb_ml_generate_tree	*child_tree;
	struct cb_ml_generate_tree	*last_attr = NULL;
	struct cb_ml_generate_tree	*last_child_tree = NULL;

	(*tree)->children = NULL;
	(*tree)->attrs = NULL;
	for (child = record->children; child; child = child->sister) {
		if (cb_field_is_ignored_in_ml_gen (child)) {
			continue;
		}

		if (child->children) {
			child_tree_or_null = cb_build_ml_tree (child, children_are_attrs,
								0,
								name_list, type_list,
								suppress_list);
		} else {
			child_tree_or_null = cb_build_ml_tree (child, 0,
								children_are_attrs,
								name_list, type_list,
								suppress_list);
		}

		if (!child_tree_or_null) {
			continue;
		}
		child_tree = CB_ML_TREE (child_tree_or_null);
		child_tree->parent = *tree;
		child_tree->sibling = NULL;

		if (child_tree->type == CB_ML_ATTRIBUTE) {
			append_to_tree_list (&((*tree)->attrs), &last_attr,
					     child_tree);
		} else {
			append_to_tree_list (&((*tree)->children),
					     &last_child_tree, child_tree);
		}
	}
}

/* Global functions */

char *
cb_to_cname (const char *s)
{
	char		*copy;
	unsigned char	*p;

	copy = cobc_parse_strdup (s);
	for (p = (unsigned char *)copy; *p; p++) {
		if (*p == '-' || *p == ' ') {
			*p = '_';
		} else {
			*p = (cob_u8_t)toupper (*p);
		}
	}
	return copy;
}

struct cb_literal *
build_literal (const enum cb_category category, const void *data,
	       const size_t size)
{
	struct cb_literal *p;

	p = make_tree (CB_TAG_LITERAL, category, sizeof (struct cb_literal));
	p->data = cobc_parse_malloc (size + 1U);
	p->size = size;
	memcpy (p->data, data, size);
	return p;
}

char *
cb_name (cb_tree x)
{
	char	*s;
	char	tmp[COB_SMALL_BUFF];
	size_t	tlen;

	tlen = cb_name_1 (tmp, x, COB_SMALL_MAX);
	s = cobc_parse_malloc (tlen + 1);
	memcpy (s, tmp, tlen);

	return s;
}

cb_tree
cb_exhbit_literal (cb_tree x)
{
	char	*s;
	char	tmp[COB_NORMAL_BUFF];
	size_t	tlen;

	tlen = cb_name_1 (tmp, x, COB_NORMAL_MAX);
	s = cobc_parse_malloc (tlen + 4);
	memcpy (s, tmp, tlen);
	memcpy (s + tlen, " = ", 4);
	return CB_TREE (build_literal (CB_CATEGORY_ALPHANUMERIC, s, tlen + 3));
}

enum cb_category
cb_tree_category (cb_tree x)
{
	struct cb_cast		*p;
	struct cb_reference	*r;
	struct cb_field		*f;

	if (CB_INVALID_TREE (x)) {
		return CB_CATEGORY_UNKNOWN;
	}

	/* LCOV_EXCL_START */
	if (x->category >= CB_CATEGORY_ERROR) {
		cobc_err_msg (_("call to '%s' with invalid parameter '%s'"),
			"cb_tree_category", "x");
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */
	if (x->category != CB_CATEGORY_UNKNOWN) {
		return x->category;
	}

	switch (CB_TREE_TAG (x)) {
	case CB_TAG_CAST:
		p = CB_CAST (x);
		switch (p->cast_type) {
		case CB_CAST_ADDRESS:
		case CB_CAST_ADDR_OF_ADDR:
			x->category = CB_CATEGORY_DATA_POINTER;
			break;
		case CB_CAST_PROGRAM_POINTER:
			x->category = CB_CATEGORY_PROGRAM_POINTER;
			break;
		/* LCOV_EXCL_START */
		default:
			cobc_err_msg (_("unexpected cast type: %d"),
					(int)(p->cast_type));
			COBC_ABORT ();
		/* LCOV_EXCL_STOP */
		}
		break;
	case CB_TAG_REFERENCE:
		r = CB_REFERENCE (x);
		x->category = cb_tree_category (r->value);
		if (r->offset) {
			switch (x->category) {
			case CB_CATEGORY_ALPHANUMERIC:
			case CB_CATEGORY_NATIONAL:
				break;
			case CB_CATEGORY_NATIONAL_EDITED:
				x->category = CB_CATEGORY_NATIONAL;
				break;
			default:
				x->category = CB_CATEGORY_ALPHANUMERIC;
			}
		}
		break;
	case CB_TAG_FIELD:
		f = CB_FIELD (x);
		if (f->children) {
			/* CHECKME: may should be alphabetic/national/... depending on the content */
			x->category = CB_CATEGORY_ALPHANUMERIC;
		} else {
			switch (f->level) {
			case 66:
				if (f->rename_thru) {
					/* CHECKME: may should be alphabetic/national/... depending on the content */
					x->category = CB_CATEGORY_ALPHANUMERIC;
				} else {
					x->category = cb_tree_category (CB_TREE (f->redefines));
				}
				break;
			case 88:
				x->category = CB_CATEGORY_BOOLEAN;
				break;
			default:
				if (f->usage == CB_USAGE_POINTER) {
					x->category = CB_CATEGORY_DATA_POINTER;
				} else if (f->usage == CB_USAGE_PROGRAM_POINTER) {
					x->category = CB_CATEGORY_PROGRAM_POINTER;
				} else if (f->pic) {
					x->category = f->pic->category;
				/* FIXME: Hack for CGI to not abort */
				} else if (f->flag_is_external_form) {
					x->category = CB_CATEGORY_ALPHANUMERIC;
				} else {
					x->category = CB_CATEGORY_UNKNOWN;
				}
				break;
			}
		}
		break;
	case CB_TAG_ALPHABET_NAME:
	case CB_TAG_LOCALE_NAME:
		x->category = CB_CATEGORY_ALPHANUMERIC;
		break;
	case CB_TAG_BINARY_OP:
		x->category = CB_CATEGORY_BOOLEAN;
		break;
	case CB_TAG_INTRINSIC:
		x->category = CB_INTRINSIC(x)->intr_tab->category;
		break;
	default:
#if	0	/* RXWRXW - Tree tag */
		cobc_err_msg (_("unknown tree tag: %d, category: %d"),
				(int)CB_TREE_TAG (x), (int)x->category);
		COBC_ABORT ();
#endif
		return CB_CATEGORY_UNKNOWN;
	}

	return x->category;
}

enum cb_class
cb_tree_class (cb_tree x)
{
	return category_to_class_table[CB_TREE_CATEGORY (x)];
}

int
cb_category_is_alpha (cb_tree x)
{
	return category_is_alphanumeric[CB_TREE_CATEGORY (x)];
}

int
cb_category_is_national (cb_tree x)
{
	return category_is_national[CB_TREE_CATEGORY (x)];
}

static int
cb_category_is_alpha_or_national (cb_tree x)
{
	enum cb_category cat = CB_TREE_CATEGORY (x);
	return category_is_alphanumeric[cat]
		|| category_is_national[cat];
}

int
cb_tree_type (const cb_tree x, const struct cb_field *f)
{
	if (f->children) {
		return COB_TYPE_GROUP;
	}

	switch (CB_TREE_CATEGORY (x)) {
	case CB_CATEGORY_ALPHABETIC:
	case CB_CATEGORY_ALPHANUMERIC:
		return COB_TYPE_ALPHANUMERIC;
	case CB_CATEGORY_ALPHANUMERIC_EDITED:
		return COB_TYPE_ALPHANUMERIC_EDITED;
	case CB_CATEGORY_NATIONAL:
		return COB_TYPE_NATIONAL;
	case CB_CATEGORY_NATIONAL_EDITED:
		return COB_TYPE_NATIONAL_EDITED;
	case CB_CATEGORY_NUMERIC:
		switch (f->usage) {
		case CB_USAGE_DISPLAY:
			return COB_TYPE_NUMERIC_DISPLAY;
		case CB_USAGE_BINARY:
		case CB_USAGE_COMP_5:
		case CB_USAGE_COMP_X:
		case CB_USAGE_COMP_N:
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
			return COB_TYPE_NUMERIC_BINARY;
		case CB_USAGE_FLOAT:
			return COB_TYPE_NUMERIC_FLOAT;
		case CB_USAGE_DOUBLE:
			return COB_TYPE_NUMERIC_DOUBLE;
		case CB_USAGE_LONG_DOUBLE:
			return COB_TYPE_NUMERIC_L_DOUBLE;
		case CB_USAGE_PACKED:
		case CB_USAGE_COMP_6:
			return COB_TYPE_NUMERIC_PACKED;
		case CB_USAGE_FP_BIN32:
			return COB_TYPE_NUMERIC_FP_BIN32;
		case CB_USAGE_FP_BIN64:
			return COB_TYPE_NUMERIC_FP_BIN64;
		case CB_USAGE_FP_BIN128:
			return COB_TYPE_NUMERIC_FP_BIN128;
		case CB_USAGE_FP_DEC64:
			return COB_TYPE_NUMERIC_FP_DEC64;
		case CB_USAGE_FP_DEC128:
			return COB_TYPE_NUMERIC_FP_DEC128;
		/* FIXME: is neither numeric nor "cobc"-boolean */
		case CB_USAGE_BIT:
			return COB_TYPE_BOOLEAN;
		case CB_USAGE_NATIONAL:
#if 0	/* FIXME: both are wrong... but numeric possibly best for "unfinished" */
			return COB_TYPE_NATIONAL_EDITED;
#else
			return COB_TYPE_NUMERIC_DISPLAY;
#endif
		/* LCOV_EXCL_START */
		default:
			cobc_err_msg (_("unexpected numeric USAGE: %d"),
					(int)f->usage);
			COBC_ABORT ();
		/* LCOV_EXCL_STOP */
		}
	case CB_CATEGORY_NUMERIC_EDITED:
	case CB_CATEGORY_FLOATING_EDITED:
		return COB_TYPE_NUMERIC_EDITED;
	case CB_CATEGORY_OBJECT_REFERENCE:
	case CB_CATEGORY_DATA_POINTER:
	case CB_CATEGORY_PROGRAM_POINTER:
		return COB_TYPE_NUMERIC_BINARY;
	/* LCOV_EXCL_START */
	default:
		cobc_err_msg (_("unexpected category: %d"),
				(int)CB_TREE_CATEGORY (x));
		COBC_ABORT ();
	/* LCOV_EXCL_STOP */
	}
	/* NOT REACHED */
#ifndef _MSC_VER
	return 0;	/* LCOV_EXCL_LINE */
#endif
}

int
cb_fits_int (const cb_tree x)
{
	switch (CB_TREE_TAG (x)) {
	case CB_TAG_LITERAL: {
		const struct cb_literal	*l = CB_LITERAL (x);
		const unsigned char	*p;
		size_t			size;
		if (l->scale > 0) {
			return 0;
		}
		for (size = 0, p = l->data; size < l->size; ++size, ++p) {
			if (*p != (unsigned char)'0') {
				break;
			}
		}
		size = l->size - size - l->scale;
		if (size < 10) {
			return 1;
		}
		if (size > 10) {
			return 0;
		} else {	/* size exactly 10 */
			const char		*s;
			if (l->sign < 0) {
				s = "2147483648";
			} else {
				s = "2147483647";
			}
			if (memcmp (p, s, 10U) > 0) {
				return 0;
			}
		}
		return 1;
	}
	case CB_TAG_FIELD: {
		const struct cb_field	*f = CB_FIELD (x);
		if (f->children) {
			return 0;
		}
		switch (f->usage) {
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
			return 1;
		case CB_USAGE_BINARY:
		case CB_USAGE_COMP_5:
		case CB_USAGE_COMP_X:
		case CB_USAGE_COMP_N:
			if (f->pic->scale <= 0 && f->size <= (int)sizeof (int)) {
				return 1;
			}
			return 0;
		case CB_USAGE_DISPLAY:
			if (f->size < 10) {
				if (!f->pic || f->pic->scale <= 0) {
					return 1;
				}
			}
			return 0;
		case CB_USAGE_PACKED:
		case CB_USAGE_COMP_6:
			if (f->pic->scale <= 0 && f->pic->digits < 10) {
				return 1;
			}
			return 0;
		default:
			return 0;
		}
	}
	case CB_TAG_REFERENCE:
		return cb_fits_int (CB_REFERENCE (x)->value);
	case CB_TAG_INTEGER:
		return 1;
	case CB_TAG_CAST:
		return cb_fits_int (CB_CAST (x)->val);
	default:
		if (x == cb_zero) {
			return 1;
		}
		return 0;
	}
}

int
cb_fits_long_long (const cb_tree x)
{
	switch (CB_TREE_TAG (x)) {
	case CB_TAG_LITERAL: {
		const struct cb_literal *l = CB_LITERAL (x);
		const unsigned char *p;
		size_t			size;
		l = CB_LITERAL (x);
		if (l->scale > 0) {
			return 0;
		}
		for (size = 0, p = l->data; size < l->size; ++size, ++p) {
			if (*p != (unsigned char)'0') {
				break;
			}
		}
		size = l->size - size - l->scale;
		if (size < 19) {
			return 1;
		}
		if (size > 19) {
			return 0;
		} else {	/* size exactly 19 */
			const char *s;
			if (l->sign < 0) {
				s = "9223372036854775808";
			} else {
				s = "9223372036854775807";
			}
			if (memcmp (p, s, 19U) > 0) {
				return 0;
			}
		}
		return 1;
	}
	case CB_TAG_FIELD: {
		const struct cb_field	*f = CB_FIELD (x);
		if (f->children) {
			return 0;
		}
		switch (f->usage) {
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
			return 1;
		case CB_USAGE_BINARY:
		case CB_USAGE_COMP_5:
		case CB_USAGE_COMP_X:
		case CB_USAGE_COMP_N:
			if (f->pic->scale <= 0 &&
			    f->size <= (int)sizeof (cob_s64_t)) {
				return 1;
			}
			return 0;
		case CB_USAGE_DISPLAY:
			if (f->pic->scale <= 0 && f->size < 19) {
				return 1;
			}
			return 0;
		case CB_USAGE_PACKED:
		case CB_USAGE_COMP_6:
			if (f->pic->scale <= 0 && f->pic->digits < 19) {
				return 1;
			}
			return 0;
		default:
			return 0;
		}
	}
	case CB_TAG_REFERENCE:
		return cb_fits_long_long (CB_REFERENCE (x)->value);
	case CB_TAG_INTEGER:
		return 1;
	case CB_TAG_CAST:
		return cb_fits_long_long (CB_CAST (x)->val);
	default:
		if (x == cb_zero) {
			return 1;
		}
		return 0;
	}
}

static void
error_numeric_literal (const char *literal)
{
	char		lit_out[39];
	/* snip literal for output, if too long */
	cobc_elided_strcpy (lit_out, literal, sizeof (lit_out), 1);
	cb_error (_("invalid numeric literal: '%s'"), lit_out);
	cb_error ("%s", err_msg);
}

/* Check numeric literal length, postponed from scanner.l (scan_numeric) */
static void
check_lit_length (const int unsigned size, const char *lit)
{
	if (unlikely(size > COB_MAX_DIGITS)) {
		/* Absolute limit */
		snprintf (err_msg, COB_MINI_MAX,
			_("literal length %d exceeds maximum of %d digits"),
			size, COB_MAX_DIGITS);
		error_numeric_literal (lit);
	} else if (unlikely(size > cb_numlit_length)) {
		snprintf (err_msg, COB_MINI_MAX,
			_("literal length %d exceeds %d digits"),
			size, cb_numlit_length);
		error_numeric_literal (lit);
	}
}

int
cb_get_int (const cb_tree x)
{
	struct cb_literal	*l;
	const char		*s;
	unsigned int	size, i;
	int			val;

	if (x == NULL || x == cb_error_node)	return 0;
	if (CB_INTEGER_P (x)) return CB_INTEGER (x)->val;

	/* LCOV_EXCL_START */
	if (!CB_LITERAL_P (x)) {
		/* not translated as it is a highly unlikely internal abort */
		cobc_err_msg ("invalid literal cast");
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */
	l = CB_LITERAL (x);

	/* Skip leading zeroes */
	for (i = 0; i < l->size; i++) {
		if (l->data[i] != '0') {
			break;
		}
	}

	/* Check numeric literal length, postponed from scanner.l (scan_numeric) */
	size = l->size - i;
	if (l->scale < 0) {
		size = size - l->scale;
	}
	check_lit_length (size, (const char *)l->data + i);

	/* Check numeric literal length matching requested output type */
#if INT_MAX >= 9223372036854775807
	if (unlikely(size >= 19U)) {
		if (l->sign < 0) {
			s = "9223372036854775808";
		} else {
			s = "9223372036854775807";
		}
		if (size > 19U || memcmp (&l->data[i], s, 19U) > 0) {
			cb_error (_("numeric literal '%s' exceeds limit '%s'"), &l->data[i], s);
			return INT_MAX;
		}
	}
#elif INT_MAX >= 2147483647
	if (unlikely(size >= 10U)) {
		if (l->sign < 0) {
			s = "2147483648";
		} else {
			s = "2147483647";
		}
		if (size > 10U || memcmp (&l->data[i], s, 10U) > 0) {
			cb_error (_("numeric literal '%s' exceeds limit '%s'"), &l->data[i], s);
			return INT_MAX;
		}
	}
#else
#error compiler maximum for INT seems to be 16bit
#endif

	val = 0;
	for (; i < l->size; i++) {
		val = val * 10 + l->data[i] - '0';
	}
	if (val && l->sign < 0) {
		val = -val;
	}
	return val;
}

cob_s64_t
cb_get_long_long (const cb_tree x)
{
	struct cb_literal	*l;
	const char		*s;
	unsigned int	size, i;
	cob_s64_t		val;

	/* LCOV_EXCL_START */
	if (!CB_LITERAL_P (x)) {
		/* not translated as it is a highly unlikely internal abort */
		cobc_err_msg ("invalid literal cast");
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */
	l = CB_LITERAL (x);

	/* Skip leading zeroes */
	for (i = 0; i < l->size; i++) {
		if (l->data[i] != '0') {
			break;
		}
	}

	/* Check numeric literal length, postponed from scanner.l (scan_numeric) */
	size = l->size - i;
	if (l->scale < 0) {
		size = size - l->scale;
	}
	check_lit_length (size, (const char *)l->data + i);

	/* Check numeric literal length matching requested output type */
	if (unlikely (size >= 19U)) {
		if (l->sign < 0) {
			s = "9223372036854775808";
		} else {
			s = "9223372036854775807";
		}
		if (size > 19U || memcmp (&(l->data[i]), s, 19U) > 0) {
			cb_error (_("numeric literal '%s' exceeds limit '%s'"), &l->data[i], s);
			return LLONG_MAX;
		}
	}

	val = 0;
	for (; i < l->size; i++) {
		val = val * 10 + (l->data[i] & 0x0F);
	}
	if (val && l->sign < 0) {
		val = -val;
	}
	return val;
}

cob_u64_t
cb_get_u_long_long (const cb_tree x)
{
	struct cb_literal	*l;
	const char		*s;
	unsigned int	size, i;
	cob_u64_t		val;

	/* LCOV_EXCL_START */
	if (!CB_LITERAL_P (x)) {
		/* not translated as it is a highly unlikely internal abort */
		cobc_err_msg ("invalid literal cast");
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */
	l = CB_LITERAL (x);

	/* Skip leading zeroes */
	for (i = 0; i < l->size; i++) {
		if (l->data[i] != '0') {
			break;
		}
	}

	/* Check numeric literal length, postponed from scanner.l (scan_numeric) */
	size = l->size - i;
	if (l->scale < 0) {
		size = size - l->scale;
	}
	check_lit_length(size, (const char *)l->data + i);

	/* Check numeric literal length matching requested output type */
	if (unlikely(size >= 20U)) {
		s = "18446744073709551615";
		if (size > 20U || memcmp (&(l->data[i]), s, 20U) > 0) {
			cb_error (_("numeric literal '%s' exceeds limit '%s'"), &l->data[i], s);
			return ULLONG_MAX;
		}
	}
	val = 0;
	for (; i < l->size; i++) {
		val = val * 10 + (l->data[i] & 0x0F);
	}
	return val;
}

void
cb_init_parse_constants (void)
{
	int	i;

	cb_error_node = make_constant (CB_CATEGORY_UNKNOWN, NULL);
	cb_any = make_constant (CB_CATEGORY_UNKNOWN, NULL);
	cb_true = make_constant (CB_CATEGORY_BOOLEAN, "1");
	cb_false = make_constant (CB_CATEGORY_BOOLEAN, "0");
	cb_null = make_constant (CB_CATEGORY_DATA_POINTER, "0");
	cb_zero = make_constant (CB_CATEGORY_NUMERIC, "&cob_all_zero");
	cb_space = make_constant (CB_CATEGORY_ALPHANUMERIC, "&cob_all_space");
	cb_low = make_constant (CB_CATEGORY_ALPHANUMERIC, "&cob_all_low");
	cb_norm_low = cb_low;
	cb_high = make_constant (CB_CATEGORY_ALPHANUMERIC, "&cob_all_high");
	cb_norm_high = cb_high;
	cb_quote = make_constant (CB_CATEGORY_ALPHANUMERIC, "&cob_all_quote");
	cb_one = cb_build_numeric_literal (0, "1", 0);
	cb_zero_lit = cb_build_numeric_literal (0, "0", 0);
	cb_int0 = cb_int (0);
	cb_int1 = cb_int (1);
	cb_int2 = cb_int (2);
	cb_int3 = cb_int (3);
	cb_int4 = cb_int (4);
	cb_int5 = cb_int (5);
	cb_int6 = cb_int (6);
	cb_int7 = cb_int (7);
	cb_int8 = cb_int (8);
	cb_int16 = cb_int (16);
	for (i = 0; i < COB_MAX_SUBSCRIPTS; i++) {
		cb_i[i] = make_constant (CB_CATEGORY_NUMERIC, cb_const_subs[i]);
	}
	cb_standard_error_handler = make_constant_label ("Default Error Handler");
	CB_LABEL (cb_standard_error_handler)->flag_default_handler = 1;
	memset (container_progs, 0, sizeof(container_progs));
}

/* List */

cb_tree
cb_build_list (cb_tree purpose, cb_tree value, cb_tree chain)
{
	struct cb_list *p;

	p = make_tree (CB_TAG_LIST, CB_CATEGORY_UNKNOWN, sizeof (struct cb_list));
	p->chain = chain;
	p->value = value;
	p->purpose = purpose;

	/* Set location to that of initial element. */
	if (value) {
		CB_TREE(p)->source_file = value->source_file;
		CB_TREE(p)->source_line = value->source_line;
		CB_TREE(p)->source_column = value->source_column;
	}

	return CB_TREE (p);
}

cb_tree
cb_list_append (cb_tree l1, cb_tree l2)
{
	if (l1 == NULL) {
		return l2;
	}
	CB_CHAIN (get_last_elt (l1)) = l2;
	return l1;
}

cb_tree
cb_list_add (cb_tree l, cb_tree x)
{
	return cb_list_append (l, CB_LIST_INIT (x));
}

cb_tree
cb_pair_add (cb_tree l, cb_tree x, cb_tree y)
{
	return cb_list_append (l, CB_BUILD_PAIR (x, y));
}

/* Reverse a list of trees,
   NOTE: changes the passed list directly! */
cb_tree
cb_list_reverse (cb_tree l)
{
	cb_tree	next;
	cb_tree	last;

	last = NULL;
	for (; l; l = next) {
		next = CB_CHAIN (l);
		CB_CHAIN (l) = last;
		last = l;
	}
	return last;
}

unsigned int
cb_list_length (cb_tree l)
{
	unsigned int	n = 0;
	if (l != cb_error_node) {
		for (; l; l = CB_CHAIN (l)) {
			n++;
		}
	}
	return n;
}

int
cb_list_map (cb_tree (*func) (cb_tree x), cb_tree l)
{
	int ret = 0;
	for (; l; l = CB_CHAIN (l)) {
		if ((CB_VALUE (l) = func (CB_VALUE (l))) == cb_error_node) {
			ret = 1;
		}
	}
	return ret;
}

unsigned int
cb_next_length (struct cb_next_elem *l)
{
	unsigned int	n;

	n = 0;
	for (; l; l = l->next) {
		n++;
	}
	return n;
}

/* Link value into the reference */

const char *
cb_define (cb_tree name, cb_tree val)
{
	struct cb_word *w;

	w = CB_REFERENCE (name)->word;
	w->items = cb_list_add (w->items, val);
	w->count++;
	val->source_file = name->source_file;
	val->source_line = name->source_line;
	CB_REFERENCE (name)->value = val;
	return w->name;
}

/* Program */

static struct nested_list *
add_contained_prog (struct nested_list *parent_list, struct cb_program *child_prog)
{
	struct nested_list	*nlp;

	/* Check for reuse */
	for (nlp = parent_list; nlp; nlp = nlp->next) {
		if (nlp->nested_prog == child_prog) {
			return parent_list;
		}
	}
	nlp = cobc_parse_malloc (sizeof (struct nested_list));
	nlp->next = parent_list;
	nlp->nested_prog = child_prog;
	return nlp;
}

struct cb_program *
cb_build_program (struct cb_program *last_program, const int nest_level)
{
	struct cb_program	*p;
	struct cb_program	*q;

	if (!last_program) {
		toplev_count = 0;
	}
	cb_reset_78 ();
	cobc_in_procedure = 0;
	cobc_in_repository = 0;
	cb_clear_real_field ();

	p = cobc_parse_malloc (sizeof (struct cb_program));
	memset (p, 0, sizeof (struct cb_program));
	p->word_table = cobc_parse_malloc (CB_WORD_TABLE_SIZE);

	p->common.tag = CB_TAG_PROGRAM;
	p->common.category = CB_CATEGORY_UNKNOWN;

	p->common.source_file = cobc_parse_strdup (cb_source_file);
	p->common.source_line = cb_source_line;

	p->next_program = last_program;
	p->nested_level = nest_level;
	p->decimal_point = '.';
	p->currency_symbol = '$';
	p->numeric_separator = ',';
	if (cb_call_extfh) {
		p->extfh = cobc_parse_strdup (cb_call_extfh);
	}

	p->prof_current_section = -1;
	p->prof_current_paragraph = -1;
	p->prof_current_call = -1;

	/* Save current program as actual at it's level */
	container_progs[nest_level] = p;
	if (nest_level
	 && last_program /* <- silence warnings */) {
		/* Contained program */
		/* Inherit from upper level */
		p->global_file_list = last_program->global_file_list;
		p->collating_sequence = last_program->collating_sequence;
		p->classification = last_program->classification;
		p->mnemonic_spec_list = last_program->mnemonic_spec_list;
		p->class_spec_list = last_program->class_spec_list;
		p->interface_spec_list = last_program->interface_spec_list;
		p->function_spec_list = last_program->function_spec_list;
		p->user_spec_list = last_program->user_spec_list;
		p->program_spec_list = last_program->program_spec_list;
		p->property_spec_list = last_program->property_spec_list;
		p->alphabet_name_list = last_program->alphabet_name_list;
		p->symbolic_char_list = last_program->symbolic_char_list;
		p->class_name_list = last_program->class_name_list;
		p->locale_list = last_program->locale_list;
		p->decimal_point = last_program->decimal_point;
		p->numeric_separator = last_program->numeric_separator;
		p->currency_symbol = last_program->currency_symbol;
		p->entry_convention = last_program->entry_convention;
		p->flag_trailing_separate = last_program->flag_trailing_separate;
		p->flag_console_is_crt = last_program->flag_console_is_crt;
		/* RETURN-CODE is global for contained programs */
		if (last_program->cb_return_code) {
			p->cb_return_code = last_program->cb_return_code;
			CB_FIELD_PTR (last_program->cb_return_code)->flag_is_global = 1;
		}
		p->toplev_count = last_program->toplev_count;
		/* Add program to itself for possible recursion */
		p->nested_prog_list = add_contained_prog (p->nested_prog_list, p);
		/* Add contained program to it's parent */
		q = container_progs[nest_level - 1];
		q->nested_prog_list = add_contained_prog (q->nested_prog_list, p);
	} else {
		/* Top level program */
		p->toplev_count = toplev_count++;
		functions_are_all = cb_flag_functions_all;
		cb_reset_global_78 ();
		/* Recursive check disabled? Then handle all programs as recursive */
		if (!cb_flag_recursive_check) {
			p->flag_recursive = 1;
		}
	}
	return p;
}

void
cb_add_common_prog (struct cb_program *prog)
{
	struct cb_program	*q;

	/* Here we are sure that nested >= 1 */
	q = container_progs[prog->nested_level - 1];
	q->common_prog_list = add_contained_prog (q->common_prog_list, prog);
}

void
cb_insert_common_prog (struct cb_program *prog, struct cb_program *comprog)
{
	prog->nested_prog_list = add_contained_prog (prog->nested_prog_list,
						     comprog);
}

/* LCOV_EXCL_START */
const char *
cb_enum_explain (const enum cb_tag tag)
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
	case CB_TAG_SCHEMA_NAME:
		return "XML-SCHEMA";
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
	case CB_TAG_XML_PARSE:
		return "XML PARSE";
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
	case CB_TAG_VARY:
		return "REPORT VARYING";
	case CB_TAG_TAB_VALS:
		return "VALUE list (table-format)";
	default: 
		{
			/* whenever we get here, someone missed to add to the list above... */
			static char errmsg[31];
			snprintf (errmsg, 30, "UNKNOWN: %d", (int)tag);
			return errmsg;
		}
	}
}
/* LCOV_EXCL_STOP */


/* Integer */

static COB_INLINE COB_A_INLINE cb_tree
cb_int_uncached (const int n)
{
	struct cb_integer* y;
	cb_tree		x;

	/* Do not use make_tree here as we want a main_malloc
	   instead of parse_malloc! */
	y = cobc_main_malloc (sizeof (struct cb_integer));
	y->val = n;

	x = CB_TREE (y);
	x->tag = CB_TAG_INTEGER;
	x->category = CB_CATEGORY_NUMERIC;
	x->source_file = cb_source_file;
	x->source_line = cb_source_line;

	return x;
}

#if CACHED_INTEGERS
cb_tree
cb_int (const int n)
{
	struct int_node		*p;
	cb_tree		x;

	/* performance note: the following loop used 3% (according to callgrind)
		of the complete time spent in a sample run with
		-fsyntax-only on 880 production code files (2,500,000 LOC)
		according to gcov we entered this function 629684 times with only 280 new
		entries but the loop produces a lot of comparisions:
		for: 122441668, if: 122441388
		second-sample: one-file 430,000 LOC with many numbers: takes 36 % of the time
	*/
	for (p = int_node_table; p; p = p->next) {
		if (p->node->val == n) {
			return CB_TREE (p->node);
		}
	}

	x = cb_int_uncached (n);

	p = cobc_main_malloc (sizeof (struct int_node));
	p->node = CB_INTEGER(x);
	p->next = int_node_table;
	int_node_table = p;

	return x;
}

cb_tree
cb_int_hex (const int n)
{
#ifdef USE_INT_HEX /* Simon: using this increases the struct and we
		 *should* pass the flags as constants in any case... */
	struct int_node		*p;
	struct cb_integer	*y;
	cb_tree		x;

	/* note: we do need to do this here on a different cached note as we'd
	         set cached values to be generated as integers otherwise */
	for (p = int_node_table_hex; p; p = p->next) {
		if (p->node->val == n) {
			return CB_TREE (p->node);
		}
	}

	/* Do not use make_tree here as we want a main_malloc
	   instead of parse_malloc! */
	y = cobc_main_malloc (sizeof (struct cb_integer));
	y->val = n;
	y->hexval = 1;

	x = CB_TREE (y);
	x->tag = CB_TAG_INTEGER;
	x->category = CB_CATEGORY_NUMERIC;
	x->source_file = cb_source_file;
	x->source_line = cb_source_line;

	p = cobc_main_malloc (sizeof (struct int_node));
	p->node = y;
	p->next = int_node_table_hex;
	int_node_table_hex = p;

	return x;
#else
	return cb_int (n);
#endif
}


#else	/* ! CACHED_INTEGERS */

cb_tree
cb_int (const int n)
{
	/* not yet allocated -> uncached */
	if (!cb_int16) return cb_int_uncached (n);

	switch (n) {
	case 0: return cb_int0;
	case 1: return cb_int1;
	case 2: return cb_int2;
	case 3: return cb_int3;
	case 4: return cb_int4;
	case 5: return cb_int5;
	case 6: return cb_int6;
	case 7: return cb_int7;
	case 8: return cb_int8;
	default: return cb_int_uncached (n);
	}
}

cb_tree
cb_int_hex (const int n)
{
#ifdef USE_INT_HEX /* Simon: using this increases the struct and we
		 *should* pass the flags as constants in any case... */
	cb_tree		x = cb_int_uncached (n);
	CB_INTEGER(x)->hexval = 1;
	return x;
#else
	return cb_int (n);
#endif
}

#endif /* ! CACHED_INTEGERS */

/* String */

cb_tree
cb_build_string (const void *data, const size_t size)
{
	struct cb_string *p;

	p = make_tree (CB_TAG_STRING, CB_CATEGORY_ALPHANUMERIC,
		       sizeof (struct cb_string));
	p->size = size;
	p->data = data;
	return CB_TREE (p);
}

/* Flags */

cb_tree
cb_flags_t (const cob_flags_t n)
{

	/* FIXME:

	   This ONLY works for the current version as we have one bit left before
	   we actually need the 64bit cob_flags_t that we use internally
	   in cobc (needed already for syntax checks) and in screenio
	   (needed soon, but not yet, hence the bitmask).

	   Ideally we either store the flags as string here or mark them and
	   output the flags in codegen as flags, making the code much more readable.
	*/

	return cb_int ((int) (n & 0xFFFFFFFF));
}

/* Code output and comment */

cb_tree
cb_build_comment (const char *str)
{
	struct cb_direct *p;

	p = make_tree (CB_TAG_DIRECT, CB_CATEGORY_ALPHANUMERIC,
		       sizeof (struct cb_direct));
	p->line = str;
	CB_TREE (p)->source_file = cb_source_file;
	CB_TREE (p)->source_line = cb_source_line;
	return CB_TREE (p);
}

cb_tree
cb_build_direct (const char *str, const unsigned int flagnl)
{
	cb_tree		x;

	x = cb_build_comment (str);
	CB_DIRECT (x)->flag_is_direct = 1;
	CB_DIRECT (x)->flag_new_line = flagnl;
	return x;
}

/* DEBUG */

cb_tree
cb_build_debug (const cb_tree target, const char *str, const cb_tree fld)
{
	struct cb_debug	*p;

	p = make_tree (CB_TAG_DEBUG, CB_CATEGORY_ALPHANUMERIC,
		       sizeof (struct cb_debug));
	p->target = target;
	if (str) {
		p->value = cobc_parse_strdup (str);
		p->fld = NULL;
		p->size = strlen (str);
	} else {
		p->value = NULL;
		p->fld = fld;
		p->size = (size_t)CB_FIELD_PTR (fld)->size;
	}
	CB_TREE (p)->source_file = cb_source_file;
	CB_TREE (p)->source_line = cb_source_line;
	return CB_TREE (p);
}

/* DEBUG Callback */

cb_tree
cb_build_debug_call (struct cb_label *target)
{
	struct cb_debug_call	*p;

	p = make_tree (CB_TAG_DEBUG_CALL, CB_CATEGORY_ALPHANUMERIC,
		       sizeof (struct cb_debug_call));
	p->target = target;
	CB_TREE (p)->source_file = cb_source_file;
	CB_TREE (p)->source_line = cb_source_line;
	return CB_TREE (p);
}

/* Alphabet-name */

cb_tree
cb_build_alphabet_name (cb_tree name)
{
	struct cb_alphabet_name *p;

	if (!name || name == cb_error_node) {
		return NULL;
	}
	p = make_tree (CB_TAG_ALPHABET_NAME, CB_CATEGORY_UNKNOWN,
		       sizeof (struct cb_alphabet_name));
	p->name = cb_define (name, CB_TREE (p));
	p->cname = cb_to_cname (p->name);
	return CB_TREE (p);
}

/* XML-Schema-name */

cb_tree
cb_build_schema_name (cb_tree name)
{
	struct cb_schema_name *p;

	if (!name || name == cb_error_node) {
		return NULL;
	}
	p = make_tree (CB_TAG_SCHEMA_NAME, CB_CATEGORY_UNKNOWN,
		       sizeof (struct cb_schema_name));
	p->name = cb_define (name, CB_TREE (p));
	return CB_TREE (p);
}

/* Class-name */

cb_tree
cb_build_class_name (cb_tree name, cb_tree list)
{
	struct cb_class_name	*p;

	if (!name || name == cb_error_node) {
		return NULL;
	}
	p = make_tree (CB_TAG_CLASS_NAME, CB_CATEGORY_BOOLEAN,
		       sizeof (struct cb_class_name));
	p->name = cb_define (name, CB_TREE (p));
	if (!scratch_buff) {
		scratch_buff = cobc_main_malloc ((size_t)COB_MINI_BUFF);
	}
	snprintf (scratch_buff, (size_t)COB_MINI_MAX, "cob_is_%s_%d",
		  cb_to_cname (p->name), class_id++);
	p->cname = cobc_parse_strdup (scratch_buff);
	p->list = list;
	return CB_TREE (p);
}

/* Locale-name */

cb_tree
cb_build_locale_name (cb_tree name, cb_tree list)
{
	struct cb_class_name	*p;

	if (!name || name == cb_error_node) {
		return NULL;
	}
	if (!CB_LITERAL_P (list) || CB_NUMERIC_LITERAL_P (list)) {
		cb_error (_("invalid LOCALE literal"));
		return cb_error_node;
	}
	p = make_tree (CB_TAG_LOCALE_NAME, CB_CATEGORY_UNKNOWN,
		       sizeof (struct cb_locale_name));
	p->name = cb_define (name, CB_TREE (p));
	p->cname = cb_to_cname (p->name);
	p->list = list;
	return CB_TREE (p);
}

/* System-name */

cb_tree
cb_build_system_name (const enum cb_system_name_category category, const int token)
{
	struct cb_system_name *p;

	p = make_tree (CB_TAG_SYSTEM_NAME, CB_CATEGORY_UNKNOWN,
		       sizeof (struct cb_system_name));
	p->category = category;
	p->token = token;
	return CB_TREE (p);
}

/* Literal */

cb_tree
cb_build_numeric_literal (int sign, const void *data, const int scale)
{
	struct cb_literal *p;
	cb_tree			l;
	/* using an intermediate char pointer for pointer arithmetic */
	const char	*data_chr_ptr = data;

#if 0 /* CHECKME - shouldn't this be what we want? */
	if (*data_chr_ptr == '-') {
		if (sign < 1) {
			sign = 1;
		} else {
			sign = -1;
		}
		data_chr_ptr++;
	} else if (*data_chr_ptr == '+') {
		if (sign < 1) {
			sign = -1;
		} else {
			sign = 1;
		}
		data_chr_ptr++;
	}
#else
	if (*data_chr_ptr == '-') {
		sign = -1;
		data_chr_ptr++;
	} else if (*data_chr_ptr == '+') {
		sign = 1;
		data_chr_ptr++;
	}
#endif
	data = data_chr_ptr;
	p = build_literal (CB_CATEGORY_NUMERIC, data, strlen (data));
	p->sign = (short)sign;
	p->scale = scale;

	l = CB_TREE (p);

	l->source_file = cb_source_file;
	l->source_line = cb_source_line;

	return l;
}

cb_tree
cb_build_numsize_literal (const void *data, const size_t size, const int sign)
{
	struct cb_literal *p;
	cb_tree			l;

	p = build_literal (CB_CATEGORY_NUMERIC, data, size);
	p->sign = (short)sign;

	l = CB_TREE (p);

	l->source_file = cb_source_file;
	l->source_line = cb_source_line;

	return l;
}

cb_tree
cb_build_literal_by_category(const void *data, const size_t size, int category)
{
	cb_tree			l;

	l = CB_TREE (build_literal (category, data, size));

	l->source_file = cb_source_file;
	l->source_line = cb_source_line;

	return l;
}


cb_tree
cb_build_alphanumeric_literal (const void *data, const size_t size)
{
	cb_tree			l;


#ifdef HAVE_ICONV
	size_t outsize = size;
	void * outdata = cobc_malloc (outsize);
	
	/* check if there is a command line input for the alphanumeric*/
	if(strcmp(cb_iconv.alphanumeric_source, "NONE") == 0){
		l = CB_TREE (build_literal (CB_CATEGORY_ALPHANUMERIC, data, size));
		l->source_file = cb_source_file;
		l->source_line = cb_source_line;
		return l;
	}

	if (cb_iconv.alphanumeric == (iconv_t)-1) {
		cobc_err_msg (_("iconv_open failed"));
	} else {
		size_t inbytesleft = size;
		size_t outbytesleft = outsize;

		char *inbuf = (char *)data;
		char * outbuf = (char *)outdata;
		
		size_t convResult = iconv (cb_iconv.alphanumeric, &inbuf, &inbytesleft, &outbuf, &outbytesleft);
		if(convResult == (size_t)-1) {
			switch (errno) {
			case E2BIG:
				cobc_err_msg (_("iconv failed: Insufficient output buffer space"));
				break;
			case EILSEQ:
				cobc_err_msg (_("iconv failed: Invalid multibyte sequence in the input"));
				break;
			case EINVAL:
				cobc_err_msg (_("iconv failed: Incomplete multibyte sequence in the input"));
				break;
			default:
				cobc_err_msg (_("iconv failed: Unknown error"));
				break;
			}
			cobc_free (outdata);
			l = CB_TREE (build_literal (CB_CATEGORY_ALPHANUMERIC, data, size));
			l->source_file = cb_source_file;
			l->source_line = cb_source_line;
			return l;
		}

		outsize -= outbytesleft;
	}
	l = CB_TREE (build_literal (CB_CATEGORY_ALPHANUMERIC, outdata, outsize));
	cobc_free (outdata);
#else
	l = CB_TREE (build_literal (CB_CATEGORY_ALPHANUMERIC, data, size));
#endif

    l->source_file = cb_source_file;
    l->source_line = cb_source_line;

    return l;
}

cb_tree
cb_build_national_literal (const void *data, const size_t size)
{
	cb_tree			l;
	
#ifdef HAVE_ICONV
	size_t outsize = size * COB_NATIONAL_SIZE;
	void * outdata = cobc_malloc (outsize);
	
	if (cb_iconv.national == (iconv_t)-1) {
		cobc_err_msg (_("iconv_open failed"));
	} else {
		size_t inbytesleft = size;
		size_t outbytesleft = outsize;

		char *inbuf = (char *)data;
		char *outbuf = (char *)outdata;
		
		size_t convResult = iconv (cb_iconv.national, &inbuf, &inbytesleft, &outbuf, &outbytesleft);
		if (convResult == (size_t)-1) {
			switch (errno) {
			case E2BIG:
				cobc_err_msg(_("iconv failed: Insufficient output buffer space"));
				break;
			case EILSEQ:
				cobc_err_msg(_("iconv failed: Invalid multibyte sequence in the input"));
				break;
			case EINVAL:
				cobc_err_msg(_("iconv failed: Incomplete multibyte sequence in the input"));
				break;
			default:
				cobc_err_msg(_("iconv failed: Unknown error"));
				break;
			}
			cobc_free (outdata);
			l = CB_TREE (build_literal (CB_CATEGORY_NATIONAL, data, size));
			l->source_file = cb_source_file;
			l->source_line = cb_source_line;
			return l;
		} else {
			outsize -= outbytesleft;
			outdata = cobc_realloc (outdata, outsize); /* Resize the outdata to the actual size */
		}

	}
#else
	/* poor-man's conversion iso-8859 -> utf-16le */
	/* "!a0" = x'21613000' -> nx'21006100300000' */
	size_t outsize = size * COB_NATIONAL_SIZE;
	unsigned char *outdata = cobc_malloc(outsize);
	const unsigned char *indata = (const unsigned char *)data;
	size_t i, j;

	for (i = 0, j = 0; i < size; i++, j += 2) {
		outdata[j] = indata[i];
		outdata[j + 1] = 0;
	}
#endif
	l = CB_TREE (build_literal (CB_CATEGORY_NATIONAL, outdata, outsize));
	cobc_free (outdata);

    l->source_file = cb_source_file;
    l->source_line = cb_source_line;

	free(outdata);

	return l;
}

cb_tree
cb_build_UTF8_literal (const void *data, const size_t size)
{
	cb_tree		l;
#ifdef HAVE_ICONV
	if (strcmp(cb_iconv.source, "UTF-8") == 0) {
		l = CB_TREE (build_literal (CB_CATEGORY_UTF8, data, size));
		l->source_file = cb_source_file;
		l->source_line = cb_source_line;
		return l;
	}
	{
		size_t outsize = size * 4;
		void * outdata = cobc_malloc (outsize);
		memset (outdata, ' ', outsize);

		if (cb_iconv.utf8 == (iconv_t)-1) {
			cobc_err_msg (_("iconv_open failed"));
		} else {
			size_t inbytesleft = size;
			size_t outbytesleft = outsize;

			char *inbuf = (char *)data;
			char * outbuf = (char *)outdata;
			
			size_t convResult = iconv(cb_iconv.utf8, &inbuf, &inbytesleft, &outbuf, &outbytesleft);
			if (convResult == (size_t)-1) {
				switch (errno) {
				case E2BIG:
					cobc_err_msg (_("iconv failed: Insufficient output buffer space"));
					break;
				case EILSEQ:
					cobc_err_msg (_("iconv failed: Invalid multibyte sequence in the input"));
					break;
				case EINVAL:
					cobc_err_msg (_("iconv failed: Incomplete multibyte sequence in the input"));
					break;
				default:
					cobc_err_msg (_("iconv failed: Unknown error"));
					break;
				}
				cobc_free (outdata);
				l = CB_TREE (build_literal (CB_CATEGORY_UTF8, data, size));
				l->source_file = cb_source_file;
				l->source_line = cb_source_line;
				return l;
			} else {
				outsize -= outbytesleft;
				outdata = cobc_realloc (outdata, outsize); /* Resize the outdata to the actual size */
			}
		}
		l = CB_TREE (build_literal (CB_CATEGORY_UTF8, outdata, outsize));
		cobc_free (outdata);
	}
#else
	l = CB_TREE (build_literal (CB_CATEGORY_UTF8, data, size));
#endif

	l->source_file = cb_source_file;
	l->source_line = cb_source_line;

	return l;
}


cb_tree
cb_concat_literals (const cb_tree x1, const cb_tree x2)
{
	struct cb_literal	*p;
	cb_tree			l;

	if (x1 == cb_error_node || x2 == cb_error_node) {
		return cb_error_node;
	}

	if ((x1->category != x2->category)) {
		cb_error_x (x1, _("only literals with the same category can be concatenated"));
		return cb_error_node;
	}

	if ((x1->category != CB_CATEGORY_ALPHANUMERIC)
	 && (x1->category != CB_CATEGORY_NATIONAL)
	 && (x1->category != CB_CATEGORY_BOOLEAN)) {
		cb_error_x (x1,
			_("only alphanumeric, utf-8, national or boolean literals may be concatenated"));
		return cb_error_node;
	}

	p = concat_literals (x1, x2);
	if (p == NULL) {
		return cb_error_node;
	}
	if (p->size > cb_lit_length) {
		char		lit_out[39] = { 0 };
		literal_for_diagnostic (lit_out, (void *)p->data);
		cb_error_x (x1, _("invalid literal: '%s'"), lit_out);
		cb_note_x (COB_WARNOPT_NONE, x1, _("literal length %d exceeds %d characters"),
			p->size, cb_lit_length);
		return cb_error_node;
	}

	l = CB_TREE (p);

	l->source_file = x1->source_file;
	l->source_line = x1->source_line;

	return l;
}

/* Decimal */

cb_tree
cb_build_decimal (const unsigned int id)
{
	struct cb_decimal *p;

	p = make_tree (CB_TAG_DECIMAL, CB_CATEGORY_NUMERIC,
		       sizeof (struct cb_decimal));
	p->id = id;
	return CB_TREE (p);
}

/* Decimal Literal */

cb_tree
cb_build_decimal_literal (const int id)
{
	struct cb_decimal *p;

	p = make_tree (CB_TAG_DECIMAL_LITERAL, CB_CATEGORY_NUMERIC,
		       sizeof (struct cb_decimal));
	p->id = id;
	return CB_TREE (p);
}

/* Picture */

struct cb_picture *
cb_build_binary_picture (const char *str, const cob_u32_t size,
			 const cob_u32_t sign)
{
	struct cb_picture	*pic;

	pic = make_tree (CB_TAG_PICTURE, CB_CATEGORY_NUMERIC,
			 sizeof (struct cb_picture));
	pic->orig = cobc_check_string (str);
	pic->size = size;
	pic->digits = size;
	pic->scale = 0;
	pic->have_sign = sign;
	pic->category = CB_CATEGORY_NUMERIC;
	return pic;
}

static COB_INLINE COB_A_INLINE int
is_simple_insertion_char (const char c)
{
	return c == 'B' || c == '0' || c == '/'
		|| (current_program && c == current_program->numeric_separator);
}

/*
  Returns the first and last characters of a floating insertion string.
	Returns a zero if no errors encountered or a 1 if an error is encountered.

	A floating insertion string is made up of two or more +'s, -'s or currency
	symbols, optionally with simple insertion characters between them.
	
	Note that the non punctuation characters are '.', ',', '/', V, B , 0
	
*/
static int
find_floating_insertion_str (const cob_pic_symbol *str,
			     const cob_pic_symbol **first,
			     const cob_pic_symbol **last,
			     const unsigned char float_char)
{

	int	non_punctuation_found = 0;
	unsigned char	non_punctuation_char;

	for (; str->symbol != '\0'; ++str) {
		if (str->symbol == float_char) {
			if (*first == NULL) {
				*first = str;
			} else {
				if (non_punctuation_found) {
					*first = NULL;
					*last  = NULL;
					cb_error (_("floating '%c' symbols cannot have a '%c' between them"), float_char, non_punctuation_char);
					return 1;
				}
			}
			*last = str;
		} else if ( *first
					&& str->symbol != '.'
					&& str->symbol != ','
					&& str->symbol != 'V'
					&& str->symbol != 'B'
					&& str->symbol != '/'
					&& str->symbol != '0') {
			non_punctuation_found = 1;
			non_punctuation_char = str->symbol;
		}
	}

	return 0;
}

/* Number of character types in picture strings */
/*
  The 26 character types are:
  B  ,  .  +  +  + CR cs cs  Z  Z  +  + cs cs  9  A  L  S  V  P  P  1  U  N  E
  0           -  - DB        *  *  -  -           X
  /
  Duplicates indicate floating/non-floating insertion symbols and/or left/right
  of decimal point positon.
*/
#define CB_PIC_CHAR_TYPES 26
#define CB_FIRST_NON_P_DIGIT_CHAR_TYPE 9
#define CB_LAST_NON_P_DIGIT_CHAR_TYPE 15
#define CB_PIC_S_CHAR_TYPE 18

static int
char_to_precedence_idx (const cob_pic_symbol *str,
			const cob_pic_symbol *current_sym,
			const cob_pic_symbol *first_floating_sym,
			const cob_pic_symbol *last_floating_sym,
			const int before_decimal_point,
			const int non_p_digits_seen)
{
	const int	first_sym = str == current_sym;
	const int	second_sym = str + 1 == current_sym;
	const int	last_sym = (current_sym + 1)->symbol == '\0';
	const int	penultimate_sym
		= !last_sym && (current_sym + 2)->symbol == '\0';

	switch (current_sym->symbol) {
	case 'B':
	case '0':
	case '/':
		return 0;

	case '.':
	case ',':
		if (current_sym->symbol == (current_program ? current_program->decimal_point : '.')) {
			return 2;
		} else {
			return 1;
		}

		/* TODO: Allow floating-point PICTURE strings */
	/* case '+': */
		/* Exponent symbol */
		/* return 3; */

	case '+':
	case '-':
		if (!(first_floating_sym <= current_sym
		      && current_sym <= last_floating_sym)) {
			if (first_sym) {
				return 4;
			} else if (last_sym) {
				return 5;
			} else {
				/* Fudge char type - will still result in error */
				return 4;
			}
		} else {
			if (before_decimal_point) {
				return 11;
			} else {
				return 12;
			}
		}

	case 'C':
	case 'D':
		return 6;

	case 'Z':
	case '*':
		if (before_decimal_point) {
			return 9;
		} else {
			return 10;
		}

	case '9':
		return 15;

	case 'A':
	case 'X':
		return 16;

	case 'L':
		return 17;

	case 'S':
		return 18;

	case 'V':
		return 19;

	case 'P':
	        if (non_p_digits_seen && before_decimal_point) {
			return 20;
		} else {
			return 21;
		}

	case '1':
		return 22;

	case 'U':
		return 23;

	case 'N':
		return 24;

	case 'E':
		return 25;

	default:
		if (current_sym->symbol == (current_program ? current_program->currency_symbol : '$')) {
			if (!(first_floating_sym <= current_sym
			      && current_sym <= last_floating_sym)) {
				if (penultimate_sym || last_sym) {
					return 8;
				} else if (first_sym || second_sym) {
					return 7;
				} else {
					/* Fudge char type - will still result in error */
					return 7;
				}
			} else {
				if (before_decimal_point) {
					return 13;
				} else {
					return 14;
				}
			}
		} else {
			/*
			  Invalid characters have already been detected, so no
			  need to emit an error here.
			*/
			return -1;
		}
	}
}

static const char *
get_char_type_description (const int idx)
{
	switch (idx) {
	case 0:
		return _("B, 0 or /");
	case 1:
		if (current_program->numeric_separator == ',') {
			return ",";
		} else {
			return ".";
		}
	case 2:
		if (current_program->decimal_point == '.') {
			return ".";
		} else {
			return ",";
		}
	case 3:
		return _("the sign of the floating-point exponent");
	case 4:
		return _("a leading +/- sign");
	case 5:
		return _("a trailing +/- sign");
	case 6:
		return _("CR or DB");
	case 7:
		return _("a leading currency symbol");
	case 8:
		return _("a trailing currency symbol");
	case 9:
		return _("a Z or * which is before the decimal point");
	case 10:
		return _("a Z or * which is after the decimal point");
	case 11:
		return _("a floating +/- string which is before the decimal point");
	case 12:
		return _("a floating +/- string which is after the decimal point");
	case 13:
		return _("a floating currency symbol string which is before the decimal point");
	case 14:
		return _("a floating currency symbol string which is after the decimal point");
	case 15:
		return "9";
	case 16:
		return _("A or X");
	case 17:
		return "L";
	case 18:
		return "S";
	case 19:
		return "V";
	case 20:
		return _("a P which is before the decimal point");
	case 21:
		return _("a P which is after the decimal point");
	case 22:
		return "1";
	case 23:
		return "U";
	case 24:
		return "N";
	case 25:
		return "E";
	default:
		return NULL;
	}
}

static void
emit_precedence_error (const int preceding_idx, const int following_idx)
{
	const char	*preceding_descr = get_char_type_description (preceding_idx);
	const char	*following_descr = get_char_type_description (following_idx);


	if (following_descr && preceding_descr) {
		if (preceding_idx == following_idx) {
			cb_error (_("%s may only occur once in a PICTURE string"), preceding_descr);
		} else {
			cb_error (_("%s cannot follow %s"), following_descr, preceding_descr);
		}
	} else {
		cb_error (_("invalid PICTURE string detected"));
	}
}

static int
valid_char_order (const cob_pic_symbol *str, const int s_char_seen, const unsigned char float_char)
{
	const int	precedence_table[CB_PIC_CHAR_TYPES][CB_PIC_CHAR_TYPES] = {
		/*
		  Refer to the standard's PICTURE clause precedence rules for
		  complete explanation.

		  The entries for character `L' are based on the GCOS7 reference
		  manual.
		*/
		/*
		  B  ,  .  +  +  + CR cs cs  Z  Z  +  + cs cs  9  A  L  S  V  P  P  1  U  N  E
		  0           -  - DB        *  *  -  -           X
		  /
		*/
		{ 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0 },
		{ 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0 },
		{ 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
		{ 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0 },
		{ 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0 },
		{ 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
		{ 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0 },
		{ 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
		{ 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0 },
		{ 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
		{ 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0 },
		{ 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
		{ 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0 },
		{ 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1 },
		{ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 },
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
		{ 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0 },
		{ 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0 },
		{ 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0 },
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 },
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 },
		{ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0 },
		{ 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	};
	int		error_emitted[CB_PIC_CHAR_TYPES][CB_PIC_CHAR_TYPES] = {{ 0 }};
	int		chars_seen[CB_PIC_CHAR_TYPES] = { 0 };
	const cob_pic_symbol	*first_floating_sym = NULL;
	const cob_pic_symbol	*last_floating_sym = NULL;
	int		before_decimal_point = 1;
	int		idx;
	const cob_pic_symbol	*s;
	int		repeated;
	int		i;
	int		j;
	int		non_p_digits_seen = 0;
	int		error_detected = 0;
	const unsigned char	currency_symbol = (current_program ? current_program->currency_symbol : '$');

	chars_seen[CB_PIC_S_CHAR_TYPE] = s_char_seen;
	if (float_char == 0xFF) {
		cb_error (_("only one of the symbols '-' , '+' or '%c' may occur multiple times in a PICTURE string"), currency_symbol);
		return 0;
	}
	if (float_char) {
		if (find_floating_insertion_str (str, &first_floating_sym, &last_floating_sym, float_char)) {
			return 0;
		}
	}

	for (s = str; s->symbol != '\0'; ++s) {
		/* Perform the check twice if a character is repeated, e.g. to detect 9VV. */
		repeated = s->times_repeated > 1;
		for (i = 0; i <= repeated; ++i) {
			idx = char_to_precedence_idx (str, s,
						      first_floating_sym,
						      last_floating_sym,
						      before_decimal_point,
						      non_p_digits_seen);
			if ((idx == 8) && ((s + 1)->symbol == '9') && ((s +2)->symbol == '\0')) {
				idx = 7;
			}
			if (idx == -1) {
				continue;
			}
			if (idx >= CB_FIRST_NON_P_DIGIT_CHAR_TYPE
			 && idx <= CB_LAST_NON_P_DIGIT_CHAR_TYPE) {
				non_p_digits_seen = 1;
			}

			/*
			  Emit an error if the current character is following a
			  character it is not allowed to. Display an error once
			  for each combination detected.
			*/
			for (j = 0; j < CB_PIC_CHAR_TYPES; ++j) {
				if (chars_seen[j]
				 && !precedence_table[idx][j]
				 && !error_emitted[idx][j]) {
					emit_precedence_error (j, idx);
					error_emitted[idx][j] = 1;
					error_detected = 1;
				}
			}
			chars_seen[idx] = 1;

			if (s->symbol == 'V'
			 || (current_program && s->symbol == current_program->decimal_point)) {
				before_decimal_point = 0;
			}
		}
	}

	return !error_detected;
}

static cob_u64_t
get_pic_number_from_str (const unsigned char *str, unsigned int * const error_detected)
{
	cob_u32_t		num_sig_digits = 0;
	cob_u64_t		value = 0;
	const int	max_sig_digits = 10;

	/* Ignore leading zeroes */
	for (; *str == '0' && *str; str++);

	/* Get the value. */
	for (; *str != ')' && *str; str++) {
		if (!isdigit (*str)) {
			cb_error (_("number or constant in parentheses is not an unsigned integer"));
			*error_detected = 1;
			break;
		}

		num_sig_digits++;
		if (num_sig_digits <= max_sig_digits) {
			value = value * 10 + (*str - '0');
		} else if (*error_detected == 0) {
			cb_error (_("only up to %d significant digits are permitted within parentheses"),
				max_sig_digits);
			*error_detected = 1;
			return COB_MAX_FIELD_SIZE + 1;
		}
	}

	if (value == 0) {
		cb_error (_("number or constant in parentheses must be greater than zero"));
		*error_detected = 1;
	}

	return value;
}

static size_t
skip_bad_parentheses(const unsigned char *p)
{
	const unsigned char *pos = p;
	cb_error(_("parentheses must be preceded by a picture symbol"));

	do {
		++pos;
	} while (*pos != ')' && *pos != '\0');

	return pos - p;
}

/*
  Return the number in parentheses. p should point to the opening parenthesis.
  When the function returns, p will point to the closing parentheses or the null
  terminator.
*/
static cob_u64_t
get_number_in_parentheses (const unsigned char ** p,
			   unsigned int * const error_detected)
{
	const unsigned char	*open_paren = *p;
	const unsigned char	*close_paren = *p + 1;
	const unsigned char	*c;
	int			contains_name;

	while (*close_paren != ')' && *close_paren)	++close_paren;

	if (!*close_paren) {
		cb_error (_("unbalanced parentheses"));
		*error_detected = 1;
		return 1;
	}

	*p = close_paren;

	if (open_paren + 1 == close_paren) {
		cb_error (_("parentheses must contain an unsigned integer"));
		*error_detected = 1;
		return 1;
	}

	/* Find out if the parens contain a number or a constant-name. */
	contains_name = 0;
	for (c = open_paren + 1; c != close_paren; ++c) {
		if (*c == '(') {
			size_t skipped = skip_bad_parentheses(c);
			close_paren = c + skipped + 1;
			*error_detected = 1;
			while (*close_paren != ')' && *close_paren)	++close_paren;
			*p = close_paren;
			/* actually only partial fix - we only skip one "inner" parens... */
			return 1;
		} else if (!(isdigit (*c)
			 || *c == '.' || *c == '+' || *c == '-')) {
			contains_name = 1;
		}
	}

	if (contains_name) {
		size_t			name_length;
		char			*name_buff;
		cb_tree			item;
		struct cb_field *f = NULL;
		struct cb_literal *l = NULL;

		/* Copy name, CHECKME: Shouldn't we limit that - and can use
		   a fixed-buffer here instead? */
		name_length = close_paren - open_paren;
		name_buff = cobc_malloc (name_length);
		memcpy (name_buff, open_paren + 1, name_length - 1);
		name_buff[name_length - 1] = '\0';

		/* Build reference to name */
		item = cb_ref (cb_build_reference (name_buff));

		if (item == cb_error_node) {
			*error_detected = 1;
			cobc_free (name_buff);
			return 1;
		}
		if (CB_FIELD_P (item)) {
			f = CB_FIELD (item);
		}
		if (!(f && f->flag_item_78)) {
			cb_error (_("'%s' is not a constant-name"), name_buff);
			*error_detected = 1;
			cobc_free (name_buff);
			return 1;
		}

		if (CB_NUMERIC_LITERAL_P (f->values)) {
			l = CB_LITERAL (f->values);
		}

		if (!l
		 || l->scale != 0
		 || l->sign != 0) {
			cb_error (_("'%s' is not an unsigned positive integer"), name_buff);
			*error_detected = 1;
			cobc_free (name_buff);
			return 1;
		}

		return get_pic_number_from_str (l->data, error_detected);
	} else {
		return get_pic_number_from_str (open_paren + 1, error_detected);
	}
}

/* build picture from string; _always_ returns a cb_picture,
   but in case of errors during parsing the pic->size is zero */
struct cb_picture *
cb_build_picture (const char *str)
{
	struct cb_picture	*pic;
	static cob_pic_symbol	*pic_buff = NULL;
	char			err_chars[10] = { 0 };
	size_t			err_char_pos = 0;
	const unsigned char	*p;
	unsigned int		pic_str_len = 0;
	size_t			idx = 0;
	size_t			buff_cnt = 0;
	cob_u32_t		at_beginning;
	cob_u32_t		at_end;
	cob_u32_t		s_char_seen = 0;
	cob_u32_t		asterisk_seen = 0;
	cob_u32_t		z_char_seen = 0;
	cob_u32_t		c_count = 0;
	cob_u32_t		s_count = 0;
	cob_u32_t		s_edit_count = 0;
	cob_u32_t		v_count = 0;
	cob_u32_t		digits = 0;
	cob_u32_t		digits_exponent = 0;
	cob_u32_t		pos_count = 0;
	cob_u32_t		neg_count = 0;
	cob_u32_t		curency_count = 0;
	cob_u32_t		float_count = 0;
#if 0 /* currently unused */
	cob_u32_t		real_digits = 0;
#endif
	cob_u32_t		x_digits = 0;
	cob_u32_t		has_parens;
	cob_u32_t		error_detected = 0;
	int			category = 0;
	int			size = 0;
	int			scale = 0;
	int			n;
	unsigned char		c;
	unsigned char		float_char;
	const unsigned char	decimal_point = (current_program ? current_program->decimal_point : '.');
	const unsigned char	currency_symbol = (current_program ? current_program->currency_symbol : '$');

	unsigned char		first_last_char = '\0';
	unsigned char		second_last_char = '\0';

	pic = make_tree (CB_TAG_PICTURE, CB_CATEGORY_UNKNOWN,
			sizeof (struct cb_picture));

	if (strlen (str) == 0) {
		cb_error (_("missing PICTURE string"));
		return pic;
	}

	if (!pic_buff) {
		pic_buff = cobc_main_malloc ((size_t)COB_MINI_BUFF * sizeof(cob_pic_symbol));
	}

	p = (const unsigned char *)str;

	if (*p == '(') {
		size_t skipped = skip_bad_parentheses (p) + 1;
		p += skipped;
		pic_str_len += skipped;

		error_detected = 1;
	}

	for (; *p; p++) {
		n = 1;
		has_parens = 0;
		c = *p;
repeat:
		/* early check for picture characters with multiple characters */
		if ( (c == 'C' && p[1] == 'R')
		  || (c == 'D' && p[1] == 'B')) {
			p++;
			pic_str_len++;
		} else if (c == 'C') {
			cb_error(_("C must be followed by R"));
			error_detected = 1;
		} else if (c == 'D') {
			cb_error(_("D must be followed by B"));
			error_detected = 1;
		}
		/* handle repeated chars */
		if (p[1] == c) {
			n++, p++, pic_str_len++;
			goto repeat;
		}

		if (p[1] == '(') {
			cob_u64_t	paren_num, pic_num;
			has_parens = 1;
			++p;
			++pic_str_len;
			if (n != 1) {
				cb_warning (COBC_WARN_FILLER, _("uncommon parentheses"));
			}
			paren_num = get_number_in_parentheses (&p, &error_detected);
			/*
			  The number of digits of the number in parentheses is
			  counted in the length of the PICTURE string (not the
			  length of the constant-name, if one was used).
			*/
			pic_num = paren_num;
			for (; pic_num != 0; pic_num /= 10) {
				++pic_str_len;
			}
			if (p[1] == '(') {
				size_t skipped = skip_bad_parentheses(p);
				p += skipped;
				pic_str_len += skipped;
				error_detected = 1;
			}
			/* max value depends on grammar */
			if (paren_num > 999999999) {
				int delta = n - 1 + x_digits;
				switch (c) {
				case 'X':
				case 'A':
					if (paren_num + delta > INT_MAX) {
						paren_num = (cob_s64_t)INT_MAX - delta;
					}
					break;
				case 'N':
					if (paren_num * 2 + delta > INT_MAX) {
						paren_num = ((cob_s64_t)INT_MAX - delta) / 2;
					}
					break;
				default:
					/* much too much... */
					paren_num = 99999;
				}
			}
			n += (int)paren_num - 1;
		}
		if (category & PIC_NUMERIC_FLOATING) {
			if (c != '9') {
				char symbol[2] = { 0 };
				symbol[0] = c;
				cb_error (_("%s cannot follow %s"), symbol, _("exponent"));
				return pic;
			}
		}

		/* Check grammar and category */
		switch (c) {
		case '9':
			if (category & PIC_NUMERIC_FLOATING) {
				digits_exponent = n;
				break;
			}
			category |= PIC_NUMERIC;
			digits += n;
#if 0 /* currently unused */
			real_digits += n;
#endif
			if (v_count) {
				scale += n;
			}
			break;

		case 'X':
			category |= PIC_ALPHANUMERIC;
			x_digits += n;
			break;

		case 'U':
			/* this is only a hack and wrong,
			   adding UTF-8 type will need a separate
			   PIC, but this will need handling in both
			   the compiler and the runtime, so fake as
			   ALPHANUMERIC for now */
			category |= PIC_UTF8;
			x_digits += n * 4;
			break;

		case 'N':
			if (!(category & PIC_NATIONAL)) {
			category |= PIC_NATIONAL;
				CB_UNFINISHED ("USAGE NATIONAL");
			}
			x_digits += n * 2;
			break;

		case 'A':
			category |= PIC_ALPHABETIC;
			x_digits += n;
			break;

		case 'L':
			pic->variable_length = 1;
			(void) cb_verify (cb_picture_l,
					  _("PICTURE string with 'L' character"));
			if (idx != 0) {
				cb_error (_("L must be at start of PICTURE string"));
				error_detected = 1;
			}
			break;

		case 'S':
			category |= PIC_NUMERIC;
			if (s_count <= 1) {
				s_count += n;
				if (has_parens) {
					cb_warning (COBC_WARN_FILLER, _("uncommon parentheses"));
				}
				if (s_count > 1) {
					cb_error (_("%s may only occur once in a PICTURE string"), "S");
					error_detected = 1;
				}
			}
			if (idx != 0) {
				cb_error (_("S must be at start of PICTURE string"));
				error_detected = 1;
			}

			s_char_seen = 1;
			continue;

		case ',':
		case '.':
			category |= PIC_NUMERIC_EDITED;
			if (c != decimal_point) {
				break;
			}
			/* fall through */
		case 'V':
			category |= PIC_NUMERIC;
			v_count += n;
			if (has_parens) {
				cb_warning (COBC_WARN_FILLER, _("uncommon parentheses"));
			}
			if (v_count > 1) {
				error_detected = 1;
			}
			break;

		case 'P':
			category |= PIC_NUMERIC;
			at_beginning = 0;
			at_end = 0;
			switch (buff_cnt) {
			case 0:
				/* P..... */
				at_beginning = 1;
				break;
			case 1:
				/* VP.... */
				/* SP.... */
				if (first_last_char == 'V' || first_last_char == 'S') {
					at_beginning = 1;
				}
				break;
			case 2:
				/* SVP... */
				if (second_last_char == 'S' && first_last_char == 'V') {
					at_beginning = 1;
				}
				break;
			default:
				break;
			}
			if (p[1] == 0 || (p[1] == 'V' && p[2] == 0)) {
				/* .....P */
				/* ....PV */
				at_end = 1;
			}
			if (!at_beginning && !at_end) {
				cb_error (_("P must be at start or end of PICTURE string"));
				error_detected = 1;
			}
			if (at_beginning) {
				/* Implicit V */
				v_count++;
			} else {
				digits += n;
			}
			if (v_count) {
				scale += n;
			} else {
				scale -= n;
			}
			break;

		case '0':
		case 'B':
		case '/':
			category |= PIC_EDITED;
			break;

		case '*':
		case 'Z':
			if (c == '*') {
				asterisk_seen = 1;
			} else if (c == 'Z') {
				z_char_seen = 1;
			}

			if (asterisk_seen && z_char_seen) {
				cb_error (_("cannot have both Z and * in PICTURE string"));
				error_detected = 1;
			}

			category |= PIC_NUMERIC_EDITED;
			if (category & PIC_ALPHABETIC) {
				error_detected = 1;
			}
			digits += n;
			if (v_count) {
				scale += n;
			}
			break;

		case '+':
		case '-':
			if (c == '+')
				pos_count += n;
			else
				neg_count += n;
			category |= PIC_NUMERIC_EDITED;
			digits += n;
			if (s_edit_count == 0) {
				--digits;
			}
			if (v_count) {
				scale += n;
				if (s_edit_count == 0) {
					--scale;
				}
			}
			s_edit_count++;
			break;

		case '1':
			category |= PIC_NUMERIC;	/* FIXME: this is WRONG */
			digits += n;
#if 0 /* currently unused */
			real_digits += n;
#endif
			break;

		case 'C':
		case 'D':
			/* note: only reached if actually CR/DB, length adjusted already */
			category |= PIC_NUMERIC_EDITED;
			if (has_parens) {
				cb_warning (COBC_WARN_FILLER, _("uncommon parentheses"));
			}
			if (n != 1) {
				error_detected = 1;
			}

			s_edit_count++;
			break;

		case 'E':
			if (p[1] == '+') {
				category |= PIC_NUMERIC_FLOATING | PIC_NUMERIC_EDITED;
				p++;
				break;
			}
			/* fall through */

		default:
			if (c == currency_symbol) {
				curency_count += n;
				category |= PIC_NUMERIC_EDITED;
				if (c_count == 0) {
					digits += n - 1;
				} else {
					digits += n;
				}
				if (v_count) {
					if (c_count == 0) {
						scale += n - 1;
					} else {
						scale += n;
					}
				}
				c_count += n;
				break;
			}

			if (err_char_pos == sizeof err_chars) {
				return pic;
			}
			if (!strchr (err_chars, (int)c)) {
				err_chars[err_char_pos++] = (char)c;
				cb_error (_("invalid PICTURE character '%c'"), c);
				error_detected = 1;
			}
		}

		/* Calculate size */
		if (c != 'V' && c != 'P') {
			size += n;
		}
		if (c == 'C' || c == 'D') {
			size += n;
		}
		if (c == 'N') {
			size += n * (COB_NATIONAL_SIZE - 1);
		}
		if (c == 'U') {
			size += n * (4 - 1);
		}

		/* Store in the buffer */
		pic_buff[idx].symbol = c;
		pic_buff[idx].times_repeated = n;
		++idx;
		second_last_char = first_last_char;
		first_last_char = c;
		++buff_cnt;
		if (unlikely(idx == COB_MINI_MAX)) {
			break;
		}
	}
	pic_buff[idx].symbol = '\0';

	if (pic_str_len > cb_pic_length) {
		cb_error (_("PICTURE string may not contain more than %d characters; contains %d characters"),
			cb_pic_length, pic_str_len);
		error_detected = 1;
	}
	if (digits == 0 && x_digits == 0) {
		cb_error (_("PICTURE string must contain at least one of the set A, N, U, X, Z, 1, 9 and *; "
					"or at least two of the set +, - and the currency symbol"));
		error_detected = 1;
	}

	float_char = 0x00;

	if (pos_count > 1) {
		float_char = '+';
		float_count++;
	} 
	
	if (neg_count > 1) {
		float_char = '-';
		float_count++;
	}

	if (curency_count > 1) {
		float_char = currency_symbol;
		float_count++;
	} 

	if (float_count > 1)
		float_char = 0xFF;

	if (!valid_char_order (pic_buff, s_char_seen, float_char)) {
		error_detected = 1;
	}

	if (error_detected) {
		return pic;
	}

	/* Set picture */
	pic->orig = cobc_check_string (str);
	pic->size = size;
	pic->digits = digits;
	pic->scale = scale;
	pic->have_sign = (s_count || s_edit_count);
#if 0 /* currently unused */
	pic->real_digits = real_digits;
#endif

	/* Set picture category */
	switch (category) {
	case PIC_NUMERIC:
		pic->category = CB_CATEGORY_NUMERIC;
		if (digits > COB_MAX_DIGITS) {
			cb_error (_("numeric field cannot be larger than %d digits"), COB_MAX_DIGITS);
		}
		break;
	case PIC_ALPHANUMERIC:
		pic->category = CB_CATEGORY_ALPHANUMERIC;
		break;
	case PIC_NATIONAL:
		pic->category = CB_CATEGORY_NATIONAL;
		break;
	case PIC_ALPHABETIC:
		pic->category = CB_CATEGORY_ALPHABETIC;
		break;
	case PIC_FLOATING_EDITED:
		/* note: same messages in scanner.l */
		if (digits > COB_MAX_DIGITS) {
			cb_error (_("significand has more than %d digits"), COB_FLOAT_DIGITS_MAX);
		}
		switch (digits_exponent) {
		case 1: digits_exponent = 0; break;
		case 2: digits_exponent = 99; break;
		case 3: digits_exponent = 999; break;
		case 4: digits_exponent = 9999; break;
		default:
			cb_error (_("exponent has more than 4 digits"));
			digits_exponent = 9999;
		}
		/* No decimals; power up by scale difference */
		if (scale < 0) {
			scale -= digits_exponent;
		} else {
			scale += digits_exponent;
		}
		pic->scale = scale;
		pic->str = cobc_parse_malloc ((idx + 1) * sizeof(cob_pic_symbol));
		memcpy (pic->str, pic_buff, idx * sizeof(cob_pic_symbol));
		pic->category = CB_CATEGORY_FLOATING_EDITED;
		pic->lenstr = idx;
		break;
	case PIC_NUMERIC_EDITED:
		pic->str = cobc_parse_malloc ((idx + 1) * sizeof(cob_pic_symbol));
		memcpy (pic->str, pic_buff, idx * sizeof(cob_pic_symbol));
		pic->category = CB_CATEGORY_NUMERIC_EDITED;
		pic->lenstr = idx;
		break;
	case PIC_EDITED:
	case PIC_ALPHABETIC_EDITED:
	case PIC_ALPHANUMERIC_EDITED:
	case PIC_NATIONAL_EDITED:
		pic->str = cobc_parse_malloc ((idx + 1) * sizeof(cob_pic_symbol));
		memcpy (pic->str, pic_buff, idx * sizeof(cob_pic_symbol));
		if (category != PIC_NATIONAL_EDITED) {
			pic->category = CB_CATEGORY_ALPHANUMERIC_EDITED;
		} else {
			pic->category = CB_CATEGORY_NATIONAL_EDITED;
		}
		pic->lenstr = idx;
		pic->digits = x_digits;
		break;
	default:
		;
	}

	return pic;
}

/* REPORT: VARYING */

cb_tree
cb_build_vary (cb_tree var, cb_tree from, cb_tree by)
{
	struct cb_vary *vary
		= make_tree (CB_TAG_VARY, CB_CATEGORY_UNKNOWN, sizeof (struct cb_vary));
	vary->var = var;
	vary->from = from;
	vary->by = by;
	return CB_TREE (vary);
}

/* VALUE: multiple entries (table-format) */

cb_tree
cb_build_table_values (cb_tree values, cb_tree from, cb_tree to, cb_tree times)
{
	struct cb_table_values	*vals
		= make_tree (CB_TAG_TAB_VALS, CB_CATEGORY_UNKNOWN, sizeof (struct cb_table_values));
	vals->values = values;
	vals->from = from;
	vals->to = to;
	vals->repeat_times = times;
	return CB_TREE (vals);
}
/* Field */

cb_tree
cb_build_field (cb_tree name)
{
	struct cb_field *p;

	p = make_tree (CB_TAG_FIELD, CB_CATEGORY_UNKNOWN,
		       sizeof (struct cb_field));
	p->id = cb_field_id++;
	p->name = cb_define (name, CB_TREE (p));
	p->ename = NULL;
	p->usage = CB_USAGE_DISPLAY;
	p->storage = CB_STORAGE_WORKING;
	p->occurs_max = 1;
	return CB_TREE (p);
}

cb_tree
cb_build_implicit_field (cb_tree name, const int len)
{
	cb_tree	x;
	char	pic[32];

	x = cb_build_field (name);
	memset (pic, 0, sizeof(pic));
	snprintf (pic, sizeof(pic), "X(%d)", len);
	CB_FIELD (x)->pic = cb_build_picture (pic);
	cb_validate_field (CB_FIELD (x));
	return x;
}

cb_tree
cb_build_constant (cb_tree name, cb_tree value)
{
	cb_tree x;

	x = cb_build_field (name);
	x->category = cb_tree_category (value);
	CB_FIELD (x)->storage = CB_STORAGE_CONSTANT;
	CB_FIELD (x)->values = value;
	return x;
}

/* Add new field to hold data from given field */
cb_tree
cb_field_dup (struct cb_field *f, struct cb_reference *ref)
{
	cb_tree		x;
	struct cb_field *s;
	char		buff[COB_MINI_BUFF], pic[30];

	if (ref && ref->length
	 && CB_LITERAL_P (ref->length)) {
		sprintf (pic, "X(%d)", cb_get_int (ref->length));
	} else
	if (f->pic->category == CB_CATEGORY_NUMERIC
	 || f->pic->category == CB_CATEGORY_NUMERIC_EDITED) {
		const int	dig = f->pic->digits;
		const int	scale = f->pic->scale;
		if (scale > 0) {
			const int dec = dig - scale;
			if (dec == 0) {
				sprintf (pic,"SV9(%d)", scale);
			} else if (dec < 0) {
				sprintf (pic, "SP(%d)V9(%d)",-dec, scale);
			} else {
				sprintf (pic, "S9(%d)V9(%d)", dec, scale);
			}
		} else {
			sprintf (pic, "S9(%d)", dig);
		}
	} else {
		sprintf (pic, "X(%d)", f->size);
	}

	snprintf (buff, (size_t)COB_MINI_MAX, "COPY OF %s", f->name);
	x = cb_build_field (cb_build_reference (buff));
	s = CB_FIELD (x);
	s->pic = cb_build_picture (pic);
	if (f->pic->category == CB_CATEGORY_NUMERIC
	 || f->pic->category == CB_CATEGORY_NUMERIC_EDITED
	 || f->pic->category == CB_CATEGORY_FLOATING_EDITED) {
		s->values = CB_LIST_INIT (cb_zero);
	} else {
		s->values = CB_LIST_INIT (cb_space);
	}
	s->storage = CB_STORAGE_WORKING;
	s->usage = CB_USAGE_DISPLAY;
	s->count++;
	cb_validate_field (s);
	CB_FIELD_ADD (current_program->working_storage, s);
	return  cb_build_field_reference (s, NULL);
}

#if	0	/* RXWRXW - Field */
struct cb_field *
CB_FIELD_PTR (cb_tree x)
{
	if (CB_REFERENCE_P (x)) {
		return CB_FIELD (cb_ref (x));
	}
	return CB_FIELD (x);
}
#endif

struct cb_field *
cb_field_add (struct cb_field *f, struct cb_field *p)
{
	struct cb_field *t;

	if (f == NULL) {
		return p;
	}
	/* get to the last item, CHECKME: would be a good place for
	   optimizing if the list can get long... */
	for (t = f; t->sister; t = t->sister) {
		;
	}
	t->sister = p;
	return f;
}

/* get size of given field/literal (or its reference),
   returns FIELD_SIZE_UNKNOWN (-1) if size isn't known
   at compile time */
int
cb_field_size (const cb_tree x)
{

	switch (CB_TREE_TAG (x)) {
	case CB_TAG_LITERAL:
		return CB_LITERAL (x)->size;
	case CB_TAG_FIELD: {
		const struct cb_field *f = CB_FIELD (x);
		if (f->flag_any_length) {
			return FIELD_SIZE_UNKNOWN;
		}
		return f->size;
	}
	case CB_TAG_REFERENCE: {
		const struct cb_reference	*r = CB_REFERENCE (x);
		const struct cb_field		*f = CB_FIELD (r->value);
		if (r->length) {
			if (CB_LITERAL_P (r->length)) {
				return cb_get_int (r->length);
			} else {
				return FIELD_SIZE_UNKNOWN;
			}
		} else if (r->offset) {
			if (CB_LITERAL_P (r->offset)) {
				return f->size - cb_get_int (r->offset) + 1;
			} else {
				return FIELD_SIZE_UNKNOWN;
			}
		} else if (f->flag_any_length) {
			return FIELD_SIZE_UNKNOWN;
		} else {
			return f->size;
		}
	}
	case CB_TAG_CONST:
		/* depends on its actual usage */
		return FIELD_SIZE_UNKNOWN;

	/* LCOV_EXCL_START */
	default:
		CB_TREE_TAG_UNEXPECTED_ABORT (x);
	}
#ifndef _MSC_VER
	/* NOT REACHED */
	return -1;
#endif
	/* LCOV_EXCL_STOP */
}

/* returns the record field (level 01) of 'f', note that the
   record field may still have a REDEFINES */
struct cb_field *
cb_field_founder (const struct cb_field * const f)
{
	const struct cb_field	*ff;

	ff = f;
	while (ff->parent) {
		ff = ff->parent;
	}

#if 0	/* CHECKME: is something like that needed? */
	if (ff->level == 0
	 && ff->sister
	 && strstr (ff->name, " Record")) {	/* Skip to First 01 within FD */
		ff = ff->sister;
	}
#endif
	return (struct cb_field *)ff;
}

/* returns the first field that has an ODO below 'f', if any
   note: per standard there would be only 0 or 1 of those, but mind
   the supported extensions that allow nested ODO as well as
   the fact that 'f' may have an ODO on its own */
struct cb_field *
cb_field_variable_size (const struct cb_field *f)
{
	struct cb_field		*p;
	struct cb_field		*fc;

	for (fc = f->children; fc; fc = fc->sister) {
		if (fc->flag_picture_l) {
			continue;	/* seen as fixed-size */
		}
		if (fc->depending) {
			return fc;
		} 
		if ((p = cb_field_variable_size (fc)) != NULL) {
			return p;
		}
	}
	return NULL;
}

#if 0	/* unused */
/* check if field 'f' has a variable address (one of the fields
   before the current one has a DEPENDING ON)  */
unsigned int
cb_field_variable_address (const struct cb_field *f)
{
	const struct cb_field	*p;
	for (p = f->parent; p; f = f->parent, p = f->parent) {
		for (p = p->children; p != f; p = p->sister) {
			if (p->depending
			 || (!p->flag_picture_l && cb_field_variable_size (p))) {
				return 1;
			}
		}
	}
	return 0;
}
#endif

/* check if field 'pfld' is subordinate to field 'f' */
int
cb_field_subordinate (const struct cb_field *pfld, const struct cb_field *f)
{
	struct cb_field		*p;

	for (p = pfld->parent; p; p = p->parent) {
		if (p == f) {
			return 1;
		}
	}
	return 0;
}

/* SYMBOLIC CHARACTERS */

void
cb_build_symbolic_chars (const cb_tree sym_list, const cb_tree alphabet)
{
	cb_tree			l;
	cb_tree			x;
	cb_tree			x2;
	struct cb_alphabet_name	*ap;
	int			n;
	unsigned char		buff[4];

	if (alphabet) {
		ap = CB_ALPHABET_NAME (alphabet);
	} else {
		ap = NULL;
	}
	for (l = sym_list; l; l = CB_CHAIN (l)) {
		n = cb_get_int (CB_PURPOSE (l)) - 1;
		if (ap) {
			buff[0] = (unsigned char)ap->alphachr[n];
		} else {
			buff[0] = (unsigned char)n;
		}
		buff[1] = 0;
		x2 = cb_build_alphanumeric_literal (buff, (size_t)1);
		CB_LITERAL (x2)->all = 1;
		x = cb_build_constant (CB_VALUE (l), x2);
		CB_FIELD (x)->flag_item_78 = 1;
		CB_FIELD (x)->flag_is_global = 1;
		CB_FIELD (x)->flag_internal_constant = 1;
		CB_FIELD (x)->level = 1;
		(void)cb_validate_78_item (CB_FIELD (x), 0);
	}
}

/* resolve literal value from tree as integer */
int
cb_literal_value (cb_tree x)
{
	if (x == cb_space) {
		return ' ';
	} else if (x == cb_zero) {
		return '0';
	} else if (x == cb_quote) {
		return cb_flag_apostrophe ? '\'' : '"';
	} else if (x == cb_norm_low) {
		return 0;
	} else if (x == cb_norm_high) {
		return 255;
	} else if (x == cb_null) {
		return 0;
	} else if (CB_TREE_CLASS (x) == CB_CLASS_NUMERIC) {
		return cb_get_int (x) - 1;
	} else {
		return CB_LITERAL (x)->data[0];
	}
}

/* Report */

struct cb_report *
build_report (cb_tree name)
{
	struct cb_report *p;
	cb_tree		x, y;
	char		buff[COB_MINI_BUFF];

	p = make_tree (CB_TAG_REPORT, CB_CATEGORY_UNKNOWN, sizeof (struct cb_report));
	p->name = cb_define (name, CB_TREE (p));
	p->cname = cb_to_cname (p->name);

	/* Set up LINE-COUNTER / PAGE-COUNTER */
	snprintf (buff, (size_t)COB_MINI_MAX,
		  "LINE-COUNTER of %s", p->name);
	x = cb_build_field (cb_build_reference (buff));
	CB_FIELD (x)->usage = CB_USAGE_UNSIGNED_INT;
	CB_FIELD (x)->values = CB_LIST_INIT (cb_zero);
	CB_FIELD (x)->storage = CB_STORAGE_WORKING;
	CB_FIELD (x)->count++;
	cb_validate_field (CB_FIELD (x));
	p->line_counter = cb_build_field_reference (CB_FIELD (x), NULL);
	CB_FIELD_ADD (current_program->working_storage, CB_FIELD (x));

	snprintf (buff, (size_t)COB_MINI_MAX,
		  "PAGE-COUNTER of %s", p->name);
	y = cb_build_field (cb_build_reference (buff));
	CB_FIELD (y)->usage = CB_USAGE_UNSIGNED_INT;
	CB_FIELD (y)->values = CB_LIST_INIT (cb_zero);
	CB_FIELD (y)->storage = CB_STORAGE_WORKING;
	CB_FIELD (y)->count++;
	cb_validate_field (CB_FIELD (y));
	p->page_counter = cb_build_field_reference (CB_FIELD (y), NULL);
	CB_FIELD_ADD (current_program->working_storage, CB_FIELD (y));

	return p;
}

/* Add SUM counter to program */
void
build_sum_counter (struct cb_report *r, struct cb_field *f)
{
	cb_tree		x;
	struct cb_field *s;
	char		buff[COB_MINI_BUFF],pic[30];
	int		dec,dig;
	size_t	num_sums_size = ((size_t)r->num_sums + 2) * sizeof (struct cb_field *) * 2;
	size_t	num_sums_square = (size_t)r->num_sums * 2;

	/* Set up SUM COUNTER */
	if (f->report_sum_list == NULL)
		return;
	if (f->pic == NULL) {
		s = CB_FIELD_PTR (CB_VALUE(f->report_sum_list));
		cb_error_x (CB_TREE(f), _("needs PICTURE clause for SUM %s"), s->name);
		return;
	}
	if (f->pic->category != CB_CATEGORY_NUMERIC
	 && f->pic->category != CB_CATEGORY_NUMERIC_EDITED) {
		s = CB_FIELD_PTR (CB_VALUE(f->report_sum_list));
		cb_warning_x (COBC_WARN_FILLER, CB_TREE(f), 
					_("non-numeric PICTURE clause for SUM %s"), s->name);
	}

	if (f->flag_filler) {
		snprintf (buff, (size_t)COB_MINI_MAX, "SUM OF %s",
			CB_FIELD_PTR (CB_VALUE (f->report_sum_list))->name);
	} else {
		snprintf (buff, (size_t)COB_MINI_MAX, "SUM %s", f->name);
	}
	x = cb_build_field (cb_build_reference (buff));
	if (f->pic->digits == 0) {
		dig = 16;
	} else if(f->pic->digits > 17) {
		dig = 18;
	} else {
		dig = f->pic->digits + 2;
	}
	if ((dec = f->pic->scale) > 0) {
		if((dig-dec) == 0) {
			sprintf(pic,"SV9(%d)",dec);
		} else if((dig-dec) < 0) {
			sprintf(pic,"SP(%d)V9(%d)",-(dig-dec),dec);
		} else {
			sprintf(pic,"S9(%d)V9(%d)",dig-dec,dec);
		}
	} else {
		sprintf(pic,"S9(%d)",dig);
	}
	s = CB_FIELD (x);
	s->pic		= cb_build_picture (pic);
	s->values	= cb_zero;
	s->storage	= CB_STORAGE_WORKING;
	s->usage	= CB_USAGE_DISPLAY;
	s->count++;
	cb_validate_field (s);
	f->report_sum_counter = cb_build_field_reference (s, NULL);
	CB_FIELD_ADD (current_program->working_storage, s);

	if (r->sums == NULL) {
		r->sums = cobc_parse_malloc (num_sums_size);
	} else {
		r->sums = cobc_parse_realloc (r->sums, num_sums_size);
	}
	r->sums[num_sums_square + 0] = s;
	r->sums[num_sums_square + 1] = f;
	r->sums[num_sums_square + 2] = NULL;
	r->sums[num_sums_square + 3] = NULL;
	r->num_sums++;
}

void
finalize_report (struct cb_report *r, struct cb_field *records)
{
	struct cb_field		*p, *ff, *fld;
	struct cb_file		*f;
	struct cb_reference	*ref;
	int		k;

	if (!r->was_checked) {
		r->was_checked = 1;
		if (r->lines > 9999) {
			r->lines = 9999;
		}
		if (r->heading < 0) {
			r->heading = 0;
		}
		if (r->first_detail < 1) {
			if(r->first_detail <= 0
			&& !r->has_detail
			&& r->t_first_detail == NULL
			&& r->t_last_detail == NULL) {
				cb_warning_x (COBC_WARN_FILLER,
					CB_TREE(r), _("no DETAIL line defined in report %s"), r->name);
			}
			r->first_detail = 1;
		}
		if(r->t_lines == NULL
		&& r->t_columns == NULL
		&& r->t_heading == NULL
		&& r->t_first_detail == NULL
		&& r->t_last_detail == NULL
		&& r->t_last_control == NULL
		&& r->t_footing == NULL) {	/* No PAGE LIMITS set at run-time so check it now */
			if(r->first_detail <= 0) {
				cb_warning_x (COBC_WARN_FILLER,
					CB_TREE(r), _("no DETAIL line defined in report %s"),r->name);
			} else if(!(r->first_detail >= r->heading)) {
				cb_error_x (CB_TREE(r), _("PAGE LIMIT FIRST DETAIL should be >= HEADING"));
			}
			if(r->footing > 0 && !(r->footing >= r->heading)) {
				cb_error_x (CB_TREE(r), _("PAGE LIMIT FOOTING should be >= HEADING"));
			} else if(r->last_detail > 0 && !(r->last_detail >= r->first_detail)) {
				cb_error_x (CB_TREE(r), _("PAGE LIMIT LAST DETAIL should be >= FIRST DETAIL"));
			} else if(r->footing > 0 && !(r->footing >= r->last_detail)) {
				cb_error_x (CB_TREE(r), _("PAGE LIMIT FOOTING should be >= LAST DETAIL"));
			} else if(!(r->lines >= r->footing)) {
				cb_error_x (CB_TREE(r), _("PAGE LIMIT LINES should be >= FOOTING"));
			}
		}
		if (r->file) {
			r->file->flag_report = 1;
		}
	}

	/* ensure report record size is set large enough */
	for (k=0; k < 2; k++) {
		for (p = records; p; p = p->sister) {
			if (p->storage != CB_STORAGE_REPORT)
				continue;
			if ((p->report_flag &  COB_REPORT_LINE) || p->level == 1) {
				if (r->rcsz < p->size + p->offset) {
					r->rcsz = p->size + p->offset;
				}
				if (k == 1
				 && p->level == 1) {
					if (p->size < r->rcsz) {
						p->size = r->rcsz;
					}
					if (p->memory_size < r->rcsz) {
						p->memory_size = r->rcsz;
					}
				}
			}
			if (p->report_column > 0) {
				if (p->report_column - 1 + p->size > r->rcsz) {
					r->rcsz = p->report_column - 1 + p->size;
				}
			}
		}
	}

	for (p = records; p; p = p->sister) {
		if (p->report != NULL) {
			continue;
		}
		p->report = r;
		if (p->storage == CB_STORAGE_REPORT
		 && ((p->report_flag &  COB_REPORT_LINE) || p->level == 1)) {
			size_t size = ((size_t)r->num_lines + 2) * sizeof(struct cb_field *);
			if (r->line_ids == NULL) {
				r->line_ids = cobc_parse_malloc (size);
			} else {
				r->line_ids = cobc_parse_realloc (r->line_ids, size);
			}
			r->line_ids[r->num_lines++] = p;
			r->line_ids[r->num_lines] = NULL;	/* Clear next entry */
		}
		/* report source field */
		if (p->report_source
		 && CB_REF_OR_FIELD_P (p->report_source)) {
			fld = CB_FIELD_PTR (p->report_source);
			if (CB_TREE_TAG (p->report_source) == CB_TAG_REFERENCE) {
				ref = CB_REFERENCE (p->report_source);
				if (ref->offset || ref->length || ref->subs || fld->flag_local) {
					p->report_from = p->report_source;
					p->report_source = cb_field_dup (fld, ref);
				}
			}
			/* force generation of report source field
			   CHECKME: Why - it should be the target of an internal
			            MOVE or COMPUTE (for ROUNDED clause)
						which sets the reference */
			if (fld->count == 0) {
				fld->count = 1;
			}
		}
		if (p->report_sum_counter
		 && CB_REF_OR_FIELD_P (p->report_sum_counter)) {
			fld = CB_FIELD_PTR (p->report_sum_counter);
			/* force generation of report sum counter TODO: Check why */
			if (fld->count == 0) {
				fld->count = 1;
			}
		}
		/* force generation of report control counter TODO: Check why */
		if (p->report_control
		 && CB_REF_OR_FIELD_P (p->report_control)) {
			fld = CB_FIELD_PTR (p->report_control);
			if (fld->count == 0) {
				fld->count = 1;
			}
		}
		if (p->children) {
			finalize_report (r,p->children);
		}
	}

	for (p = records; p; p = p->sister) {
		if (p->report != r) {
			continue;
		}
		if (p->storage == CB_STORAGE_REPORT
		 && ((p->report_flag & COB_REPORT_LINE) || p->level == 1)) {
			if (p->size + p->offset > r->rcsz) {
				p->size = r->rcsz - p->offset ;
			}
			if (p->memory_size + p->offset > r->rcsz) {
				p->memory_size = r->rcsz - p->offset;
			}
		}
		if (p->level == 1
		 && p->report != NULL
		 && p->report->file != NULL) {
			f = p->report->file;
			for (ff = records; ff; ff = ff->sister) {
				if (f->record_max > 0
				 && ff->size > f->record_max) {
					f->record_max = ff->size;
				}
			}
			if (f->record_min < r->rcsz) {
				f->record_min = r->rcsz;
			}
			if (f->record_max < p->size) {
				f->record_max = r->rcsz;
			}
			if (f->record != NULL
			 && f->record->size < r->rcsz) {
				f->record->size = r->rcsz;
			}
		}
	}
	/* LCOV_EXCL_START */
	if (!r || !r->file) {
		/* checked to keep the analyzer happy, TODO: real fix later */
		cobc_err_msg (_("call to '%s' with invalid parameter '%s'"),
			"finalize_report", "r");
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */
	if (r->file->record_max < r->rcsz) {
		r->file->record_max = r->rcsz;
	}
	if (r->rcsz < r->file->record_max) {
		r->rcsz = r->file->record_max;
	}
}


/* File */

struct cb_file *
build_file (cb_tree name)
{
	struct cb_file *p;

	p = make_tree (CB_TAG_FILE, CB_CATEGORY_UNKNOWN, sizeof (struct cb_file));
	p->name = cb_define (name, CB_TREE (p));
	p->cname = cb_to_cname (p->name);
	if (current_program->extfh) { 		/* Default EXTFH module to use */
		p->extfh = make_constant (CB_CATEGORY_ALPHANUMERIC, current_program->extfh);
	} else {
		p->extfh = NULL;
	}

	p->organization = COB_ORG_SEQUENTIAL;
	p->access_mode = COB_ACCESS_SEQUENTIAL;
	p->handler = CB_LABEL (cb_standard_error_handler);
	p->handler_prog = current_program;
	p->exception_table = cobc_parse_malloc (sizeof (struct cb_exception)
						* cb_io_exception_table_len);
	memcpy (p->exception_table, cb_io_exception_table,
		sizeof (struct cb_exception) * cb_io_exception_table_len);

	return p;
}

void
validate_file (struct cb_file *f, cb_tree name)
{
	/* FIXME - Check ASSIGN clause
		Currently break's GnuCOBOL's extension for SORT FILEs having no need
		for an ASSIGN clause (tested in run_extensions "SORT ASSIGN ..."
		According to the Programmer's Guide for 1.1 the ASSIGN is totally
		ignored as the SORT is either done in memory (if there's enough space)
		or in a temporary disk file.
		For supporting this f->organization = COB_ORG_SORT is done when we
		see an SD in FILE SECTION for the file, while validate_file is called
		in INPUT-OUTPUT Section.
	*/
	if (!f->assign && f->organization != COB_ORG_SORT && !f->flag_fileid) {
		file_error (name, "ASSIGN", CB_FILE_ERR_REQUIRED);
	}
	/* Check RECORD/RELATIVE KEY clause */
	switch (f->organization) {
	case COB_ORG_INDEXED:
		if (f->key == NULL) {
			file_error (name, "RECORD KEY", CB_FILE_ERR_REQUIRED);
		} else if (f->alt_key_list) {
			int keynum = cb_next_length ((struct cb_next_elem *)f->alt_key_list) + 1;
			if (keynum > MAX_FILE_KEYS) {
				cb_error_x (name, _("maximum keys (%d/%d) exceeded for file '%s'"),
					keynum, MAX_FILE_KEYS, CB_NAME (name));
			}
		}
		break;
	case COB_ORG_RELATIVE:
		if (f->key == NULL && f->access_mode != COB_ACCESS_SEQUENTIAL) {
			file_error (name, "RELATIVE KEY", CB_FILE_ERR_REQUIRED);
		}
		if (f->alt_key_list) {
			file_error (name, "ALTERNATE", CB_FILE_ERR_INVALID_FT);
			f->alt_key_list = NULL;
		}
		break;
	default:
		if (f->key) {
			file_error (name, "RECORD", CB_FILE_ERR_INVALID_FT);
			f->key = NULL;
		}
		if (f->alt_key_list) {
			file_error (name, "ALTERNATE", CB_FILE_ERR_INVALID_FT);
			f->alt_key_list = NULL;
		}
		if (f->access_mode == COB_ACCESS_DYNAMIC ||
		    f->access_mode == COB_ACCESS_RANDOM) {
			file_error (name, "ORGANIZATION", CB_FILE_ERR_INVALID);
		}
		break;
	}
}

static void
validate_indexed_key_field (struct cb_file *f, struct cb_field *records,
					cb_tree key, struct cb_key_component *component_list,
					struct cb_alt_key *cbak)
{
	cb_tree			key_ref;
	cb_tree			l;

	struct cb_field		*k;
	struct cb_field		*p;
	struct cb_field		*v;

	int			field_end;

	int			cb;
	char			pic[32];
	struct cb_key_component	*key_component;
	struct cb_field		*composite_key;

	/* get reference (and check if it exists) */
	key_ref = cb_ref (key);
	if (key_ref == cb_error_node) {
		return;
	}
	k = CB_FIELD_PTR (key_ref);

	/* check alternate key */
	if (component_list != NULL) {
		/* compute composite key total length */
		cb = 0;
		for (key_component = component_list;
		     key_component != NULL;
		     key_component = key_component->next) {
			/* resolution of references in key components must be done here */
			key_ref = cb_ref (key_component->component);
			if (key_ref == cb_error_node) {
				cb_error_x (CB_TREE(f), _("invalid KEY item '%s', not in file '%s'"),
					k->name, f->name);
				return;
			}
			cb += cb_field_size(key_ref);
		}
		composite_key = (struct cb_field *)cb_ref(key);
		memset (pic, 0, sizeof(pic));
		sprintf (pic, "X(%d)", cb);
		if (composite_key->pic != NULL) {
			cobc_parse_free (composite_key->pic);
		}
		composite_key->pic = cb_build_picture (pic);
		cb_validate_field (composite_key);
	} else {
		/* Check that key file is actual part of the file's records */
		v = cb_field_founder (k);
		for (p = records; p; p = p->sister) {
			if (p == v) {
				break;
			}
		}
		if (!p) {
			cb_error_x (CB_TREE(f), _("invalid KEY item '%s', not in file '%s'"),
				  k->name, f->name);
			return;
		}
	}

	/* Validate minimum record size against key field's end */
	/* FIXME: calculate minimum length for all keys first and only check the biggest */
	if (f->record_min > 0) {
		field_end = k->offset + k->size;
		if (field_end > f->record_min) {
			cb_error_x (CB_TREE(k), _("minimal record length %d can not hold the key item '%s';"
						  " needs to be at least %d"), f->record_min, k->name, field_end);
		}
	}

	/* get key collating sequence, if any */
	for (l = f->collating_sequence_keys; l; l = CB_CHAIN (l)) {
		cb_tree alpha_key = CB_VALUE (l);
		if (key_ref == cb_ref (CB_PAIR_Y (alpha_key))) {
			if (cbak == NULL) {
				f->collating_sequence_key = CB_PAIR_X (alpha_key);
			} else {
				cbak->collating_sequence_key = CB_PAIR_X (alpha_key);
			}
		}
	}

	/* check collating sequence is not ignored */
	if (get_warn_opt_value (cb_warn_filler) != COBC_WARN_DISABLED
	 && CB_TREE_CLASS (k) != CB_CLASS_ALPHANUMERIC) {
		const char *source = "KEY";
		cb_tree colseq = (cbak == NULL)
			? f->collating_sequence_key
			: cbak->collating_sequence_key;
		cb_tree pos = colseq;
		if (colseq == NULL) {
			source = "FILE";
			colseq = f->collating_sequence;
			pos = key_ref;
		}
		if (colseq != NULL) {
			cb_warning_x (COBC_WARN_FILLER, CB_TREE (pos),
				      _("%s COLLATING SEQUENCE '%s' is ignored "
					"for non-alphanumeric key '%s'"),
				      source, CB_NAME (colseq), k->name);
		}
	}
}

void
finalize_file (struct cb_file *f, struct cb_field *records)
{
	struct cb_field		*p;
	struct cb_field		*v;
	cb_tree			l;
	cb_tree			x;

	/* stdin/stderr and LINE ADVANCING are L/S */
	if (f->special || f->flag_line_adv) {
		f->organization = COB_ORG_LINE_SEQUENTIAL;
	}
	if (f->flag_fileid && !f->assign) {
		f->assign = cb_build_alphanumeric_literal (f->name,
							   strlen (f->name));
	}

	/* associate records to file (separate and first for being able
	   to resolve references, for example in validate_indexed_key_field */
	if (records) {
		for (p = records; p; p = p->sister) {
			p->file = f;
		}
	} else if (f->flag_report) {
		/* in general: no record description needed for REPORTs, but RD entries
		*/
	} else {
		/* Hack: if called without records this is no normal file (but a report)
		   or no valid a file description was given */
		cb_error_x (CB_TREE(f), _("missing file description for %s"),
			cb_name(CB_TREE(f)));
	}

	/* Validate INDEXED key fields (RELATIVE keys can only be validated when
	   the whole DATA DIVISION has been processed) and apply GLOBAL. */
	if (f->organization == COB_ORG_INDEXED) {
		struct cb_alt_key	*cbak;
		if (f->key) {
			validate_indexed_key_field (f, records,
				f->key, f->component_list, NULL);
		}
		for (cbak = f->alt_key_list; cbak; cbak = cbak->next) {
			if (f->flag_global) {
				cb_tree key_tree = cb_ref (cbak->key);
				if (CB_FIELD_P(key_tree)) {
					CB_FIELD(key_tree)->flag_is_global = f->flag_global;
				}
			}
			validate_indexed_key_field (f, records,
				cbak->key, cbak->component_list, cbak);
		}
	}

	/* Check the record size if it is limited */
	if (f->flag_report) {
		for (p = records; p; p = p->sister) {
			if (f->record_max > 0
			 && p->size > f->record_max) {
				f->record_max = p->size;
			}
		}
	}

	/* Validate and set max and min record size */
	for (p = records; p; p = p->sister) {
		if (f->organization == COB_ORG_INDEXED
		 && p->size > MAX_FD_RECORD_IDX) {
			cb_error_x (CB_TREE (p),
				_("RECORD size (IDX) exceeds maximum allowed (%d)"), MAX_FD_RECORD_IDX);
			p->size = MAX_FD_RECORD_IDX;
		} else if (p->size > MAX_FD_RECORD) {
			cb_error_x (CB_TREE (p),
				_("RECORD size exceeds maximum allowed (%d)"), MAX_FD_RECORD);
			p->size = MAX_FD_RECORD;
		}
		if (f->record_min > 0) {
			if (p->size < f->record_min) {
				cb_warning_dialect_x (cb_records_mismatch_record_clause, CB_TREE (p),
					_("size of record '%s' (%d) smaller than minimum of file '%s' (%d)"),
					  p->name, p->size, f->name, f->record_min);
				if (cb_records_mismatch_record_clause < CB_ERROR) {
					cb_warning_x (COBC_WARN_FILLER, CB_TREE (p), _("file size adjusted"));
				}
				f->record_min = p->size;
			}
		}
		if (f->record_max > 0) {
			/* IBM docs: When the maximum record length determined
			   from the record description entries does not match
			   the length specified in the RECORD clause,
			   the maximum will be used. */
			if (p->size > f->record_max) {
				cb_warning_dialect_x (cb_records_mismatch_record_clause, CB_TREE (p),
					_("size of record '%s' (%d) larger than maximum of file '%s' (%d)"),
				 	  p->name, p->size, f->name, f->record_max);
				if (cb_warn_additional
				 && cb_records_mismatch_record_clause != CB_ERROR
				 && cb_records_mismatch_record_clause != CB_OK) {
					cb_warning_x (COBC_WARN_FILLER, CB_TREE (p), _("file size adjusted"));
				}
				f->record_max = p->size;
			}
		}
	}

	/* Compute the record size */
	if (f->record_min == 0
	 && records) {
		f->record_min = records->size;
	}
	for (p = records; p; p = p->sister) {
		v = cb_field_variable_size (p);
		if (v && v->offset + v->size * v->occurs_min < f->record_min) {
			f->record_min = v->offset + v->size * v->occurs_min;
		}
		if (p->size < f->record_min) {
			f->record_min = p->size;
		}
		if (p->size > f->record_max) {
			f->record_max = p->size;
		}
	}

	if (f->flag_check_record_varying_limits
	 && f->record_min == f->record_max) {
		cb_warning_x (cb_warn_additional, f->description_entry,
			_("RECORD VARYING specified without limits, but implied limits are equal"));
#if 0	/* CHECKME: Do we want this warning, possibly with a separate flag? */
		cb_warning (cb_warn_additional, _("%s clause ignored"), "RECORD VARYING");
#endif
		f->flag_check_record_varying_limits = 0;
	}

	if (f->flag_delimiter && f->record_min > 0
	 && f->record_min == f->record_max) {
		/* we have both SELECT (RECORD DELIMITER) and FD (records), first one
		   may contain much more entries so using the position of the second */
		cb_verify_x (f->description_entry, cb_record_delim_with_fixed_recs,
			_("RECORD DELIMITER clause on file with fixed-length records"));
	}

	/* Apply SAME clause */
	if (f->same_clause) {
		for (l = current_program->file_list; l; l = CB_CHAIN (l)) {
			if (CB_FILE (CB_VALUE (l))->same_clause == f->same_clause) {
				if (CB_FILE (CB_VALUE (l))->flag_finalized) {
					if (f->record_max > CB_FILE (CB_VALUE (l))->record->memory_size) {
						CB_FILE (CB_VALUE (l))->record->memory_size =
						    f->record_max;
					}
					f->record = CB_FILE (CB_VALUE (l))->record;
					for (p = records; p; p = p->sister) {
						p->file = f;
						p->redefines = f->record;
					}
					for (p = f->record->sister; p; p = p->sister) {
						if (!p->sister) {
							p->sister = records;
							break;
						}
					}
					f->flag_finalized = 1;
					return;
				}
			}
		}
	}
	
	/* Create record */
	if (f->record_max == 0) {
		f->record_max = 32;
		f->record_min = 32;
	}
	if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
		f->record_min = 0;
	}
	if (!scratch_buff) {
		scratch_buff = cobc_main_malloc ((size_t)COB_MINI_BUFF);
	}
	/* FIXME: when this text is changed test DEPENDING ON with ODOSLIDE fails
	          --> describe the issue here and use at least a define */
	snprintf (scratch_buff, (size_t)COB_MINI_MAX, "%s Record", f->name);
	f->record = CB_FIELD (cb_build_implicit_field (cb_build_reference (scratch_buff),
				f->record_max));
	f->record->sister = records;
	f->record->count++;
	if (f->flag_external) {
		current_program->flag_has_external = 1;
		f->record->flag_external = 1;
	}

	for (p = records; p; p = p->sister) {
		p->redefines = f->record;
		if (p->flag_is_global) {
			f->record->flag_is_global = 1;
		}
	}

	if (f->code_set_items) {
		check_code_set_items_are_subitems_of_records (f);
	}

	f->flag_finalized = 1;

	if (f->linage) {
		snprintf (scratch_buff, (size_t)COB_MINI_MAX,
			  "LINAGE-COUNTER %s", f->name);
		x = cb_build_field (cb_build_reference (scratch_buff));
		CB_FIELD (x)->usage = CB_USAGE_UNSIGNED_INT;
		CB_FIELD (x)->values = CB_LIST_INIT (cb_zero);
		CB_FIELD (x)->count++;
		cb_validate_field (CB_FIELD (x));
		f->linage_ctr = cb_build_field_reference (CB_FIELD (x), NULL);
		CB_FIELD_ADD (current_program->working_storage, CB_FIELD (x));
	}

#if	!defined (WITH_INDEX_EXTFH) && \
	!defined (WITH_DB) && \
	!defined (WITH_CISAM) && !defined(WITH_DISAM) && !defined(WITH_VBISAM)
	if (f->organization == COB_ORG_INDEXED) {
		char msg[80];
		snprintf (msg, sizeof (msg), "ORGANIZATION INDEXED; FD %s", f->name);
		cb_warning (cb_warn_unsupported,
			_("runtime is not configured to support %s"), msg);
	}
#endif
}

/* Communication description */

struct cb_cd *
cb_build_cd (cb_tree name)
{
	struct cb_cd	*p = make_tree (CB_TAG_CD, CB_CATEGORY_UNKNOWN,
					sizeof (struct cb_cd));

	p->name = cb_define (name, CB_TREE (p));

	return p;
}

void
cb_finalize_cd (struct cb_cd *cd, struct cb_field *records)
{
	struct cb_field	*p;

	if (cd->record) {
		cd->record->sister = records;
	} else {
		cd->record = records;
	}

	for (p = records; p; p = p->sister) {
		/* TODO: Check record size is exactly 87 chars */

		p->cd = cd;
		if (p != cd->record) {
			p->redefines = cd->record;
		}
	}
}

/* Reference */

cb_tree
cb_build_reference (const char *name)
{
	struct cb_reference	*r;
	cb_tree			x;

	r = make_tree (CB_TAG_REFERENCE, CB_CATEGORY_UNKNOWN,
		       sizeof (struct cb_reference));

	/* position of reference */
	r->section = current_section;
	r->paragraph = current_paragraph;

	/* statement this reference was used with for later checks */
	if (current_statement) {
		r->statement = current_statement->statement;
	}

	/* Look up / insert word into hash list */
	lookup_word (r, name);

	x = CB_TREE (r);

	/* position of tree */
	x->source_file = cb_source_file;
	x->source_line = cb_source_line;

	return x;
}

cb_tree
cb_build_filler (void)
{
	cb_tree		x;
	char		name[20];

	sprintf (name, "FILLER %d", filler_id++);
	x = cb_build_reference (name);
	x->source_line = cb_source_line;
	CB_REFERENCE (x)->flag_filler_ref = 1;
	return x;
}

/*
  Return a reference to the field f.
  If ref != NULL, other attributes are set to the same as ref.
*/
cb_tree
cb_build_field_reference (struct cb_field *f, cb_tree ref)
{
	cb_tree		x;
	struct cb_word	*word;

	x = cb_build_reference (f->name);
	word = CB_REFERENCE (x)->word;
	if (ref) {
		memcpy (x, ref, sizeof (struct cb_reference));
	}
	x->category = CB_CATEGORY_UNKNOWN;
	CB_REFERENCE (x)->word = word;
	CB_REFERENCE (x)->value = CB_TREE (f);
	return x;
}

static void
cb_define_system_name (const char *name)
{
	cb_tree x;
	cb_tree y;

	x = cb_build_reference (name);
	if (CB_WORD_COUNT (x) == 0) {
		y = get_system_name (name);
		/* Paranoid */
		if (y) {
			cb_define (x, y);
		}
	}
}

void
cb_set_system_names (void)
{
	cb_define_system_name ("CONSOLE");
	cb_define_system_name ("SYSIN");
	cb_define_system_name ("SYSIPT");
	cb_define_system_name ("STDIN");
	cb_define_system_name ("SYSOUT");
	cb_define_system_name ("STDOUT");
	cb_define_system_name ("SYSERR");
	cb_define_system_name ("STDERR");
	cb_define_system_name ("SYSLST");
	cb_define_system_name ("SYSLIST");
	cb_define_system_name ("FORMFEED");
}

static COB_INLINE COB_A_INLINE int
field_is_in_file_record (const cb_tree file,
			 const struct cb_field * const field)
{
	return CB_FILE_P (file)
		&& CB_FILE (file) == cb_field_founder (field)->file;
}

static COB_INLINE COB_A_INLINE int
field_is_in_cd_record (const cb_tree cd,
		       const struct cb_field * const field)
{
	return CB_CD_P (cd)
		&& CB_CD (cd) == cb_field_founder (field)->cd;
}

static cb_tree
cb_ref_internal (cb_tree x, const int emit_error)
{
	struct cb_reference	*r;
	struct cb_field		*p;
	cb_tree			candidate;
	cb_tree			items;
	cb_tree			cb1;
	cb_tree			cb2;
	cb_tree			v;
	cb_tree			c;
	struct cb_program	*prog;
	struct cb_word		*w;
	size_t			val;
	size_t			ambiguous;
	struct cb_label		*save_section;
	struct cb_label		*save_paragraph;

	if (CB_INVALID_TREE (x)) {
		return cb_error_node;
	}

	/* LCOV_EXCL_START */
	if (!CB_REFERENCE_P (x)) {
		cobc_err_msg (_("call to '%s' with invalid parameter '%s'"),
			"cb_ref", "x");
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */

	r = CB_REFERENCE (x);
	/* If this reference has already been resolved (and the value
	   has been cached), then just return the value */
	if (r->value) {
		if (cb_listing_xref && r->flag_receiving) {
			/* adjust the receiving flag as this will often be set on later calls only */
			if (CB_FIELD_P (r->value)) {
				cobc_xref_link (&CB_FIELD (r->value)->xref, r->common.source_line, 1);
			} else if (CB_FILE_P (r->value)) {
				cobc_xref_link (&CB_FILE (r->value)->xref, r->common.source_line, 1);
			}
		}
		return r->value;
	}

	/* Resolve the value */

	candidate = NULL;
	ambiguous = 0;
	for (items = r->word->items; items; items = CB_CHAIN (items)) {
		/* Find a candidate value by resolving qualification */
		v = CB_VALUE (items);
		c = r->chain;
		switch (CB_TREE_TAG (v)) {
		case CB_TAG_FIELD: {
			struct cb_field *fld = CB_FIELD (v);
			/* ignore sub-items of typedefs */
			if (fld->parent != NULL && cb_field_founder (fld)->flag_is_typedef) {
				continue;
			}
			/* In case the value is a field, it might be qualified
			   by its parent names and a file name */
			if (fld->flag_indexed_by) {
				p = fld->index_qual;
			} else {
				p = fld->parent;
			}
			/* Resolve by parents */
			for (; p; p = p->parent) {
				if (c && strcasecmp (CB_NAME (c), p->name) == 0) {
					c = CB_REFERENCE (c)->chain;
				}
			}

			/* Resolve by file or CD */
			if (c
			 && CB_REFERENCE (c)->chain == NULL
			 && CB_WORD_COUNT (c) == 1) {
				cb_tree tree = cb_ref (c);
				if (field_is_in_file_record (tree, fld)
				 || field_is_in_cd_record (tree, fld)) {
					c = CB_REFERENCE (c)->chain;
				}
			}
			break;
		}
		case CB_TAG_LABEL: {
			/* In case the value is a label, it might be qualified
			   by its section name */
			struct cb_label* s = CB_LABEL (v)->section;

			/* Unqualified paragraph name referenced within the section
			   is resolved without ambiguity check if not duplicated */
			if (c == NULL && r->offset && s == CB_LABEL (r->offset)) {
				for (cb1 = CB_CHAIN (items); cb1; cb1 = CB_CHAIN (cb1)) {
					cb2 = CB_VALUE (cb1);
					if (s == CB_LABEL (cb2)->section) {
						ambiguous = 1;
						goto raise_error;
					}
				}
				candidate = v;
				goto end;
			}

			/* Resolve by section name */
			if (c && s && strcasecmp (CB_NAME (c), (char *)s->name) == 0) {
				c = CB_REFERENCE (c)->chain;
			}

			break;
		}
		default:
			/* Other values cannot be qualified */
			break;
		}

		/* A well qualified value is a good candidate */
		if (c == NULL) {
			if (candidate == NULL) {
				/* Keep the first candidate */
				candidate = v;
			} else {
				/* Multiple candidates and possibly ambiguous */
				ambiguous = 1;
				/* Continue search because the reference might not
				   be ambiguous and exit loop by "goto end" later */
			}
		}
	}

	/* There is no candidate */
	if (candidate == NULL) {
		if (likely(current_program->nested_level <= 0)) {
			goto raise_error;
		}
		/* Nested program - check parents for GLOBAL candidate */
#if 0 /* RXWRXW */
		val = word_hash ((const unsigned char *)r->word->name);
#else
		val = r->hashval;
#endif
		prog = current_program;
		while (prog) {
			if (!cb_correct_program_order) {
				prog = prog->next_program;
			} else {
				prog = prog->next_program_ordered;
			}
			if (prog->nested_level >= current_program->nested_level) {
				continue;
			}
			for (w = prog->word_table[val]; w; w = w->next) {
				if (strcasecmp (r->word->name, w->name) == 0) {
					candidate = global_check (r, w->items, &ambiguous);
					if (candidate) {
						if (ambiguous) {
							goto raise_error;
						}
						if (CB_FILE_P(candidate)) {
							current_program->flag_gen_error = 1;
						}
						goto end;
					}
				}
			}
			if (prog->nested_level == 0) {
				break;
			}
		}
		goto raise_error;
	}

	/* Reference is ambiguous */
	if (ambiguous) {
		goto raise_error;
	}

end:
	if (CB_FIELD_P (candidate)) {
		CB_FIELD (candidate)->count++;
		if (CB_FIELD (candidate)->flag_invalid) {
			goto error;
		}
	} else if (CB_LABEL_P (candidate) && r->flag_alter_code) {
		CB_LABEL (candidate)->flag_alter = 1;
	}

	if (cb_listing_xref) {
		if (CB_FIELD_P (candidate)) {
			cobc_xref_link (&CB_FIELD (candidate)->xref, r->common.source_line, r->flag_receiving);
			cobc_xref_link_parent (CB_FIELD (candidate));
		} else if (CB_LABEL_P (candidate)) {
			cobc_xref_link (&CB_LABEL(candidate)->xref, r->common.source_line, 0);
		} else if (CB_FILE_P (candidate)) {
			cobc_xref_link (&CB_FILE (candidate)->xref, r->common.source_line, r->flag_receiving);
		}
	}

	r->value = candidate;
	return r->value;

raise_error:
	if (emit_error) {
		save_section = current_section;
		save_paragraph = current_paragraph;
		current_section = r->section;
		current_paragraph = r->paragraph;
		if (ambiguous) {
			ambiguous_error (x);
		} else {
			undefined_error (x);
		}
		current_section = save_section;
		current_paragraph = save_paragraph;
	}
	/* Fall through */

error:
	r->value = cb_error_node;
	return cb_error_node;
}

cb_tree
cb_ref (cb_tree x)
{
	return cb_ref_internal (x, 1);
}

cb_tree
cb_try_ref (cb_tree x)
{
	return cb_ref_internal (x, 0);
}

/* place literal value for display into given pointer
   note: must be char [COB_MAX_DIGITS + 2]) */
static char *
display_literal (char *disp, struct cb_literal *l, int offset, int scale)
{
	if (CB_NUMERIC_LITERAL_P(l)) {
		if (scale == 0) {
			snprintf (disp, COB_MAX_DIGITS + 1, "%s%s",
				(char *)(l->sign == -1 ? "-" : ""), (char* )(l->data + offset));
		} else if (scale > 0) {
			snprintf (disp, COB_MAX_DIGITS + 1, "%s%.*s.%.*s",
				(char *)(l->sign == -1 ? "-" : ""),
				(int)(l->size - l->scale - offset), (char *)(l->data + offset),
				scale, (char *)(l->data + l->size - l->scale));
		} else {
			snprintf (disp, COB_MAX_DIGITS + 1, "%s%s",
				(char *)(l->sign == -1 ? "-" : ""), (char *)(l->data + offset));
		}
	} else {
		snprintf (disp, COB_MAX_DIGITS + 1, "%s", (char *)(l->data + offset));
	}
	return disp;
}

enum cb_binary_op_flag		cb_next_binary_op_flag = 0;

/* Check if comparing field to literal is always TRUE or FALSE */
static cb_tree
compare_field_literal (cb_tree e, int swap, cb_tree x,
		enum cb_binary_op_op op, struct cb_literal *l)
{
	int	i, j, scale, fscale;
	int	alph_lit, zero_val;
	int	lit_start, lit_length, refmod_length;
	char	lit_disp[COB_MAX_DIGITS + 2];
	struct cb_field *f;
	enum cb_category	category;
	cob_u32_t		have_sign;
	struct cb_reference	*rl;

	enum cb_binary_op_flag flag = cb_next_binary_op_flag;

	cb_next_binary_op_flag = 0;

	/* LCOV_EXCL_START */
	if (!CB_REFERENCE_P (x)) {
		cobc_err_msg (_("call to '%s' with invalid parameter '%s'"),
			"compare_field_literal", "x");
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */

	f = CB_FIELD (cb_ref (x));
	/* ensure the reference was validated as this
	   also calculates the reference' picture and size */
	if (!f->flag_is_verified) {
		/* CHECKME: why are several fields not validated
		   at this point? Note: level 66 are outside of the tree,
		   but there are others... */
		cb_validate_field (f);
	}
	if (f->flag_any_length
	 || (f->pic == NULL && !f->children)) {
		return cb_any;
	}
	if (f->pic) {
		category = f->pic->category;
		fscale = f->pic->scale;
		have_sign = f->pic->have_sign;
	} else {
		/* no PICTURE but children, category depends on USAGE */
		switch (f->usage) {
		case CB_USAGE_NATIONAL:
			category = CB_CATEGORY_NATIONAL;
			break;
		case CB_USAGE_BIT:
			category = CB_CATEGORY_BOOLEAN;
			break;
		default:
			category = CB_CATEGORY_ALPHABETIC;
		}
		fscale = 0;
		have_sign = 0;
	}

	rl = CB_REFERENCE(x);
	if (rl->length && CB_LITERAL_P (rl->length)) {
		refmod_length = cb_get_int (rl->length);
	} else if (rl->offset && CB_LITERAL_P (rl->offset)) {
		refmod_length = f->size - cb_get_int (rl->offset) + 1;
	} else if (rl->length || rl->offset) {
		 /* Note: we leave reference mod of unknown size to run-time */
		return cb_any;
	} else {
		refmod_length = 0;
	}

	/* initial: set length and type of comparision literal */
	for (lit_length = l->size;
		  lit_length > 0 && l->data[lit_length - 1] == ' ';
		  lit_length--);

	alph_lit = 0;
	zero_val = 1;
	for (j = 0; l->data[j] != 0; j++) {
		if (!isdigit(l->data[j])) {
			alph_lit = 1;
			/* note: zero_val not checked in this case */
			break;
		}
		if (l->data[j] != '0') {
			zero_val = 0;
		}
	}

	if ((category != CB_CATEGORY_NUMERIC
	  && category != CB_CATEGORY_NUMERIC_EDITED
	  && category != CB_CATEGORY_FLOATING_EDITED)
	 || refmod_length) {
		 if (!refmod_length) {
			 refmod_length = f->size;
		 }
		 if (lit_length > refmod_length) {
			copy_file_line (e, CB_TREE(l), NULL);
			if (get_warn_opt_value (cb_warn_constant_expr)
			 && !was_prev_warn (e->source_line, 2)) {
				if (lit_length > f->size) {
					cb_warning_x (cb_warn_constant_expr, e,
							_("literal '%.38s' is longer than '%s'"),
							display_literal (lit_disp, l, 0, l->scale), f->name);
				} else {
					cb_warning_x (cb_warn_constant_expr, e,
							_("literal '%.38s' is longer than reference-modification of '%s'"),
							display_literal (lit_disp, l, 0, l->scale), f->name);
				}
			}
			if (cb_constant_folding) {
				switch (op) {
				case '=':
					return cb_false;
				case '~':
					return cb_true;
				default:
					/* nothing to do for constant folding */
					break;
				}
			}
		}
		return cb_any;
	}


	if (fscale < 0) {		/* Leave for run-time */
		return cb_any;
	}

	if (alph_lit) {
		copy_file_line (e, CB_TREE(l), NULL);
		if (get_warn_opt_value (cb_warn_constant_expr)
		 && category == CB_CATEGORY_NUMERIC
		 && !was_prev_warn (e->source_line, 3)) {
			cb_warning_x (cb_warn_constant_expr, e,
						_("literal '%s' is alphanumeric but '%s' is numeric"),
						display_literal (lit_disp, l, 0, l->scale), f->name);
		}
		return cb_any;
	}

	/* from here on: only check for issues with
	   numeric non-floating-point literals */

	/* FIXME: consolidate with checks in validate_move for numeric literals,
	   this should allow also a check for binary values (we currently only
	   call this when field is USAGE DISPLAY) */

	if (zero_val) {

		/* handle ZERO to be as simple as possible */
		lit_start = lit_length;
		lit_length = 1;
		scale = i = 0;

	} else {

		/* Adjust length for leading ZERO in literal */
		for (lit_start=0; l->data[lit_start] == '0'; lit_start++);
		lit_length -= lit_start;

		/* Adjust scale for trailing ZEROS in literal */
		scale = l->scale;
		i = lit_length;
		for (j = l->size;
			  scale > 0 && j > 0 && l->data[j-1] == '0';
			  j--,i--)
			scale--;
	}

	if (scale > 0
	 && fscale >= 0
	 && fscale < scale) {
		copy_file_line (e, CB_TREE(l), NULL);
		if (get_warn_opt_value (cb_warn_constant_expr)
		 && !was_prev_warn (e->source_line, 4)) {
			cb_warning_x (cb_warn_constant_expr, e,
						_("literal '%s' has more decimals than '%s'"),
						display_literal (lit_disp, l, lit_start, l->scale), f->name);
		}
		if (cb_constant_folding) {
			switch (op) {
			case '=':
				return cb_false;
			case '~':
				return cb_true;
			default:
				/* nothing to do for constant folding */
				break;
			}
		}
	}

	if (swap) {
		/* not: swap, not negate */
		switch (op) {
		case '>':
			op = '<';
			break;
		case ']':
			op = '[';
			break;
		case '<':
			op = '>';
			break;
		case '[':
			op = ']';
			break;
		default:
			break;
		}
		flag = flag == 0 ? BOP_OPERANDS_SWAPPED : 0;
	}

	/* check for digits in literal vs. field size */
	if ((i - scale) > 0
	 && (f->size - fscale) >= 0
	 && (i - scale) > (f->size - fscale)) {
		/* If Literal has more digits in whole portion than field can hold
		 * Then the literal value will never match the field contents
		 */
		copy_file_line (e, CB_TREE(l), NULL);
		if (get_warn_opt_value (cb_warn_constant_expr)
		&& !was_prev_warn (e->source_line, 4)) {
			cb_warning_x (cb_warn_constant_expr, e,
				_("literal '%s' has more digits than '%s'"),
				display_literal (lit_disp, l, lit_start, l->scale), f->name);
		}
		if (cb_constant_folding) {
			switch (op) {
			case '=':
				return cb_false;
			case '~':
				return cb_true;
			default:
				/* nothing to do for constant folding */
				break;
			}
			if (category == CB_CATEGORY_NUMERIC) {
				switch (op) {
				case '>':
				case ']':
					return cb_false;
				case '<':
				case '[':
					return cb_true;
				default:
					/* nothing to do for constant folding */
					break;
				}
			}
		}

	}


	/* Check for numeric issues.
	 * note: the actual result may be different if non-numeric
	 *       data is stored in the numeric fields - and may (later)
	 *       be dependent on compiler configuration flags;
	 *       therefore we don't set cb_true/cb_false here
	 */
	if (get_warn_opt_value (cb_warn_constant_expr) != COBC_WARN_DISABLED
	 && (op == '<' || op == '[' || op == '>' || op == ']')) {
		copy_file_line (e, CB_TREE(l), NULL);

		if (have_sign == 0) {
			/* comparison with zero */
			if (zero_val) {
				switch (op) {
				case '<':
					if (!was_prev_warn (e->source_line, 5)) {
						cb_warning_x (cb_warn_constant_expr, e,
							_("unsigned '%s' may not be %s %s"),
							f->name, explain_operator (op), "ZERO");
					}
					break;
				case ']':
					/* don't raise a warning for VALUE THRU
					   (we still can return cb_true here later),
					   and don't raise a warning if the bop was switched */
					if (flag != BOP_OPERANDS_SWAPPED
					 && current_statement->statement != STMT_VALUE_THRU
					 && !was_prev_warn (e->source_line, 5)) {
						cb_warning_x (cb_warn_constant_expr, e,
							_("unsigned '%s' may always be %s %s"),
							f->name, explain_operator (op), "ZERO");
					}
					break;
				default:
					break;
				}
			/* comparison with negative literal */
			} else if (l->sign < 0) {
				switch (op) {
				case '[':
					if (flag == BOP_OPERANDS_SWAPPED) {
						break;
					}
					/* fall through */
				case '<':
					if (!was_prev_warn (e->source_line, 5)) {
						cb_warning_x (cb_warn_constant_expr, e,
							_("unsigned '%s' may not be %s %s"),
							f->name, explain_operator (op),
							display_literal (lit_disp, l, lit_start, l->scale));
					}
					break;
				case ']':
					if (flag == BOP_OPERANDS_SWAPPED) {
						break;
					}
					/* fall through */
				case '>':
					if (!was_prev_warn (e->source_line, 5)) {
						cb_warning_x (cb_warn_constant_expr, e,
							_("unsigned '%s' may always be %s %s"),
							f->name, explain_operator (op),
							display_literal (lit_disp, l, lit_start, l->scale));
					}
					break;
				default:
					break;
				}
			}
		}

	    /* check for maximum value */
#if 0 /* we currently call this only when field is USAGE DISPLAY) */
		if ((f->usage == CB_USAGE_DISPLAY
		  || (cb_binary_truncate
		   && (f->usage == CB_USAGE_COMP_5
	        || f->usage == CB_USAGE_COMP_X
	        || f->usage == CB_USAGE_COMP_N
	        || f->usage == CB_USAGE_BINARY)))
		 && i == f->size) {
#else
		if (i == f->size) {
#endif

			for (j=0; l->data[lit_start + j] == '9'; j++);
			if (j != f->size) {
				/* all fine */
			} else if (l->sign < 0) {
				switch (op) {
				case '[':
					if (flag == BOP_OPERANDS_SWAPPED) {
						break;
					}
					/* fall through */
				case '<':
					if (!was_prev_warn (e->source_line, 5)) {
						cb_warning_x (cb_warn_constant_expr, e,
							_("'%s' may not be %s %s"),
							f->name, explain_operator ('<'),
							display_literal (lit_disp, l, lit_start, scale));
					}
					break;
				case ']':
					/* don't raise a warning for VALUE THRU
					   (we still can return cb_true here later) */
					if (flag != BOP_OPERANDS_SWAPPED
					 && current_statement->statement != STMT_VALUE_THRU
					 && !was_prev_warn (e->source_line, 5)) {
						cb_warning_x (cb_warn_constant_expr, e,
							_("'%s' may always be %s %s"),
							f->name, explain_operator (op),
							display_literal (lit_disp, l, lit_start, scale));
					}
					break;
				default:
					break;
				}
			} else {
				switch (op) {
				case ']':
					if (flag == BOP_OPERANDS_SWAPPED) {
						break;
					}
					/* fall through */
				case '>':
					if (!was_prev_warn (e->source_line, 5)) {
						cb_warning_x (cb_warn_constant_expr, e,
							_("'%s' may not be %s %s"),
							f->name, explain_operator ('>'),
							display_literal (lit_disp, l, lit_start, scale));
					}
					break;
				case '[':
					/* don't raise a warning for VALUE THRU
					   (we still can return cb_true here later) */
					if (flag != BOP_OPERANDS_SWAPPED
					 && current_statement->statement != STMT_VALUE_THRU
					 && !was_prev_warn (e->source_line, 5)) {
						cb_warning_x (cb_warn_constant_expr, e,
							_("'%s' may always be %s %s"),
							f->name, explain_operator (op),
							display_literal (lit_disp, l, lit_start, scale));
					}
					break;
				default:
					break;
				}
			}
		}
	}
	return cb_any;
}

/* Expression */
static int rel_bin_op = 0;

static enum cb_warn_opt
get_warnopt_for_constant (cb_tree x, cb_tree y)
{
	if (!CB_LITERAL_P (x)
	 || !CB_LITERAL_P (y)
	 || !CB_NUMERIC_LITERAL_P (x)
	 || !CB_NUMERIC_LITERAL_P (y)) {
		return cb_warn_constant_expr;
	}
	return cb_warn_constant_numlit_expr;
}

cb_tree
cb_build_binary_op (cb_tree x, const enum cb_binary_op_op op, cb_tree y)
{
	struct cb_binary_op	*p;
	enum cb_category	category = CB_CATEGORY_UNKNOWN;
	cob_s64_t		xval, yval, rslt;
	char			result[48];
	char			*llit, *rlit;
	int			i, j, xscale,yscale, rscale, warn_ok, warn_type;
	struct cb_literal 	*xl, *yl;
	cb_tree			relop, e;

	if (op == '@'
	 && y == NULL
	 && CB_NUMERIC_LITERAL_P (x) )	/* Parens around a Numeric Literal */
		return x;

	/* Simon: just ignore here as we already created
		   an error for that in another place */
	if (x == cb_error_node
	 || y == cb_error_node)
		return cb_error_node;

	/* setting an error tree to point to the correct expression
	   instead of the literal/var definition / current line */
	e = relop = cb_any;
	warn_ok = 1;
	warn_type = 1;
	copy_file_line (e, NULL, NULL);
	llit = rlit = NULL;

	switch (op) {
	case '+':
	case '-':
	case '*':
	case '/':
	case '^':
		/* Arithmetic operators */
		if (CB_TREE_CLASS (x) == CB_CLASS_POINTER ||
		    CB_TREE_CLASS (y) == CB_CLASS_POINTER) {
			category = CB_CATEGORY_DATA_POINTER;
			break;
		}
		x = cb_check_numeric_value (x);
		y = cb_check_numeric_value (y);
		if (x == cb_error_node || y == cb_error_node) {
			return cb_error_node;
		}
		/*
		 * If this is an operation between two simple integer numerics
		 * then resolve the value here at compile time -> "constant folding"
		 */
		if (cb_constant_folding
		 && CB_NUMERIC_LITERAL_P (x)
		 && CB_NUMERIC_LITERAL_P (y)) {
			xl = CB_LITERAL (x);
			yl = CB_LITERAL (y);

			if (xl->llit == 0
			 && xl->size >= (unsigned int)xl->scale
			 && yl->llit == 0
			 && yl->size >= (unsigned int)yl->scale
			 && xl->all == 0
			 && yl->all == 0) {
				xval = atoll((const char*)xl->data);
				if (xl->sign == -1) {
					xval = -xval;
				}
				yval = atoll((const char*)yl->data);
				if (yl->sign == -1) {
					yval = -yval;
				}
				xscale = xl->scale;
				cb_set_dmax (xscale);
				yscale = yl->scale;
				cb_set_dmax (yscale);
				rscale = 0;
				rslt = 0;
				if (op == '+' || op == '-') {
					while (xscale < yscale) {
						xval = xval * 10;
						xscale++;
					}
					while (xscale > yscale) {
						yval = yval * 10;
						yscale++;
					}
					rscale = xscale;
					if (op == '+')
						rslt = xval + yval;
					else
						rslt = xval - yval;
				} else if (op == '*') {
					rscale = xscale + yscale;
					rslt = xval * yval;
				} else if (op == '/' && yval != 0) {
					while (yscale > 0) {
						xval = xval * 10;
						yscale--;
					}
					rscale = xscale;
					if ((xval % yval) == 0) {
						rslt = xval / yval;
					}
				}
				while (rscale > 0
				    && rslt != 0
				    && (rslt % 10) == 0) {
					rslt = rslt / 10;
					rscale--;
				}
				switch (op) {
				case '+':
				case '-':
				case '*':
					sprintf (result, CB_FMT_LLD, rslt);
					return cb_build_numeric_literal (0, result, rscale);
					break;
				case '/':
					if (yval == 0) {				/* Avoid Divide by ZERO */
						cb_warning_x (COBC_WARN_FILLER, x, _("divide by constant ZERO"));
						break;
					}
					if (rslt != 0) {
						sprintf (result, CB_FMT_LLD, rslt);
						return cb_build_numeric_literal (0, result, rscale);
					}
					/* only calculate simple integer numerics */
					if (xl->scale != 0 || yl->scale != 0)
						break;
					if ((xval % yval) == 0) {
						sprintf (result, CB_FMT_LLD, xval / yval);
						return cb_build_numeric_literal (0, result, rscale);
					}
					break;
				case '^':
					/* only calculate simple integer numerics */
					if (xl->scale != 0
					 || yl->scale != 0
					 || yval < 0)
						break;
					if (yval == 0
					 || xval == 1) {
						strcpy(result,"1");
					} else {
						rslt = xval;
						while (--yval > 0) {
							rslt = rslt * xval;
						}
						sprintf (result, CB_FMT_LLD, rslt);
					}
					return cb_build_numeric_literal (0, result, 0);
				default:
					break;
				}
			}
		} else
		if (cb_constant_folding
		 && CB_NUMERIC_LITERAL_P (y)) {
			yl = CB_LITERAL (y);
			if (yl->scale == 0) {
				yval = atoll((const char*)yl->data);
				if ((op == '+' || op == '-') 
		 		 && !rel_bin_op 
				 && yval == 0) {		/* + or - ZERO does nothing */
					return x;
				}
				if ((op == '*' || op == '/') 
				 && yval == 1
				 && yl->sign != -1) {	/* * or / by ONE does nothing */
					return x;
				}
				if (op == '*'
				 && yval == 0) {		/* * ZERO is ZERO */
					return cb_zero_lit;
				}
			}
		}
		rel_bin_op = 0;
		category = CB_CATEGORY_NUMERIC;
		break;

	case 'n':
	case 'c':
	case 'd':
		rel_bin_op = 0;
		category = CB_CATEGORY_NUMERIC;
		break;

	case 'a':
	case 'o':
	case 'e':
	case 'l':
	case 'r':
		/* Bit-wise operators */
		x = cb_check_numeric_value (x);
		y = cb_check_numeric_value (y);
		if (x == cb_error_node || y == cb_error_node) {
			return cb_error_node;
		}
		if ((CB_REF_OR_FIELD_P (x)) 
		 && !(CB_FIELD_PTR (x)->usage == CB_USAGE_COMP_5
		  || CB_FIELD_PTR (x)->usage == CB_USAGE_COMP_X)) {
			cb_error_x (CB_TREE(current_statement), 
					_("%s should be COMP-X/COMP-5 for logical operator"),
					CB_FIELD_PTR (x)->name);
			return cb_error_node;
		}
		if ((CB_REF_OR_FIELD_P (y)) 
		 && !(CB_FIELD_PTR (y)->usage == CB_USAGE_COMP_5
		  || CB_FIELD_PTR (y)->usage == CB_USAGE_COMP_X)) {
			cb_error_x (CB_TREE(current_statement), 
					_("%s should be COMP-X/COMP-5 for logical operator"),
					CB_FIELD_PTR (y)->name);
			return cb_error_node;
		}
		if (cb_constant_folding
		&&  CB_NUMERIC_LITERAL_P (x)
		&&  CB_NUMERIC_LITERAL_P (y)) {
			xl = CB_LITERAL(x);
			yl = CB_LITERAL(y);
			if (xl->scale == 0
			&& yl->scale == 0) {
				xval = atoll((const char*)xl->data);
				if(xl->sign == -1) xval = -xval;
				yval = atoll((const char*)yl->data);
				if(yl->sign == -1) yval = -yval;
				if (op == 'a')
					sprintf (result, CB_FMT_LLD, xval & yval);
				else if (op == 'o')
					sprintf (result, CB_FMT_LLD, xval | yval);
				else if (op == 'e')
					sprintf (result, CB_FMT_LLD, xval ^ yval);
				else if (op == 'l')
					sprintf (result, CB_FMT_LLD, xval << yval);
				else if (op == 'r')
					sprintf (result, CB_FMT_LLD, xval >> yval);
				return cb_build_numeric_literal (0, result, 0);
			}
		}
		rel_bin_op = 0;
		category = CB_CATEGORY_NUMERIC;
		break;

	case '=':
	case '~':
	case '<':
	case '>':
	case '[':
	case ']':
		/* Relational operators */
		rel_bin_op = 1;
#if 0	/* note: already tested in the parser with (check_not_88_level) */
		if ((CB_REF_OR_FIELD_P (x))
		 && CB_FIELD_PTR (x)->level == 88) {
			/* because this code is not active and the translation would be new,
			   we don't have that gettextized */
			cb_error_x (e, "invalid expression: conditional on the left of numeric operator");
			return cb_error_node;
		}
		if ((CB_REF_OR_FIELD_P (y))
		 && CB_FIELD_PTR (y)->level == 88) {
			cb_error_x (e, "invalid expression: conditional on the right of numeric operator");
			return cb_error_node;
		}
#endif

		if (x == cb_zero) {
			xl = CB_LITERAL(cb_zero_lit);
			xl->common.source_line = prev_expr_line = cb_exp_line;
		} else if (CB_LITERAL_P (x)) {
			xl = CB_LITERAL (x);
		} else {
			xl = NULL;
		}
		if (y == cb_zero) {
			yl = CB_LITERAL(cb_zero_lit);
			yl->common.source_line = prev_expr_line = cb_exp_line;
		} else if (CB_LITERAL_P (y)) {
			yl = CB_LITERAL (y);
		} else {
			yl = NULL;
		}

		/* CHECKME: a call should also be possible when:

		    (f->usage == CB_USAGE_DISPLAY
		  || (cb_binary_truncate
		   && (f->usage == CB_USAGE_COMP_5
		    || f->usage == CB_USAGE_COMP_X
		    || f->usage == CB_USAGE_BINARY))

			Shouldn't it?
		*/

		if (CB_REF_OR_FIELD_P (y)
		 && CB_FIELD_PTR (y)->usage == CB_USAGE_DISPLAY
		 && (CB_LITERAL_P (x) || x == cb_zero)
		 && xl->all == 0) {
			relop = compare_field_literal (e, 1, y, op, xl);
		} else if (CB_REF_OR_FIELD_P (x)
		 && CB_FIELD_PTR (x)->usage == CB_USAGE_DISPLAY
		 && (CB_LITERAL_P (y) || y == cb_zero)
		 && yl->all == 0) {
			relop = compare_field_literal (e, 0, x, op, yl);
		/*
		 * If this is an operation between two simple integer numerics
		 * then resolve the value here at compile time -> "constant folding"
		 */
		} else if (cb_constant_folding
		 && CB_NUMERIC_LITERAL_P(x)
		 && CB_NUMERIC_LITERAL_P(y)) {
			xl = CB_LITERAL(x);
			yl = CB_LITERAL(y);
			llit = (char*)xl->data;
			rlit = (char*)yl->data;
			if (xl->llit == 0
			 && xl->scale == 0
		 	 && yl->llit == 0
			 && yl->scale == 0
			 && xl->sign == 0
			 && yl->sign == 0
			 && xl->all == 0
			 && yl->all == 0) {
				copy_file_line (e, y, x);
				xval = atoll((const char*)xl->data);
				yval = atoll((const char*)yl->data);
				switch (op) {
				case '=':
					warn_type = 51 + (xval * 2 + yval) % 5000;
					if (xval == yval) {
						relop = cb_true;
					} else {
						relop = cb_false;
					}
					break;
				case '~':
					warn_type = 52 + (xval * 2 + yval) % 5000;
					if (xval != yval) {
						relop = cb_true;
					} else {
						relop = cb_false;
					}
					break;
				case '>':
					warn_type = 53 + (xval * 2 + yval) % 5000;
					if (xval > yval) {
						relop = cb_true;
					} else {
						relop = cb_false;
					}
					break;
				case '<':
					warn_type = 54 + (xval * 2 + yval) % 5000;
					if (xval < yval) {
						relop = cb_true;
					} else {
						relop = cb_false;
					}
					break;
				case ']':
					warn_type = 55 + (xval * 2 + yval) % 5000;
					if (xval >= yval) {
						relop = cb_true;
					} else {
						relop = cb_false;
					}
					break;
				case '[':
					warn_type = 56 + (xval * 2 + yval) % 5000;
					if (xval <= yval) {
						relop = cb_true;
					} else {
						relop = cb_false;
					}
					break;
				default:
					/* never happens */
					break;
				}
			}
		/*
		 * If this is an operation between two literal strings
		 * then resolve the value here at compile time -> "constant folding"
		 *
		 * TODO: build cob_fields and call cob_cmp from libcob.
		 */
		} else if (cb_constant_folding
		 && CB_LITERAL_P (x)
		 && CB_LITERAL_P (y)
		 && !CB_NUMERIC_LITERAL_P (x)
		 && !CB_NUMERIC_LITERAL_P (y)) {
			const int colseq_p = CB_TREE_CLASS(x) == CB_CLASS_NATIONAL
				? current_program->collating_sequence_n != NULL
				: current_program->collating_sequence != NULL;
			copy_file_line (e, y, x);
			xl = CB_LITERAL(x);
			yl = CB_LITERAL(y);
			llit = (char*)xl->data;
			rlit = (char*)yl->data;
			for (i = j = 0; xl->data[i] != 0 && yl->data[j] != 0; i++,j++) {
				if (xl->data[i] != yl->data[j]) {
					break;
				}
			}
			if (xl->data[i] == 0
			 && yl->data[j] == ' ') {
				while (yl->data[j] == ' ') j++;
			} else
			if (xl->data[i] == ' '
			 && yl->data[j] == 0) {
				while (xl->data[i] == ' ') i++;
			}
			switch (op) {
			case '=':
				warn_type = 51;
				if (xl->data[i] == yl->data[j]) {
					relop = cb_true;
				} else {
					relop = cb_false;
				}
				break;
			case '~':
				warn_type = 52;
				if (xl->data[i] != yl->data[j]) {
					relop = cb_true;
				} else {
					relop = cb_false;
				}
				break;
			case '>':
				if (colseq_p) break;
				warn_type = 53;
				if (xl->data[i] > yl->data[j]) {
					relop = cb_true;
				} else {
					relop = cb_false;
				}
				break;
			case '<':
				if (colseq_p) break;
				warn_type = 54;
				if (xl->data[i] < yl->data[j]) {
					relop = cb_true;
				} else {
					relop = cb_false;
				}
				break;
			case ']':
				if (colseq_p) break;
				warn_type = 55;
				if (xl->data[i] >= yl->data[j]) {
					relop = cb_true;
				} else {
					relop = cb_false;
				}
				break;
			case '[':
				if (colseq_p) break;
				warn_type = 56;
				if (xl->data[i] <= yl->data[j]) {
					relop = cb_true;
				} else {
					relop = cb_false;
				}
				break;
			default:
				/* never happens */
				break;
			}
		}
		break;

	case '!':
	case '&':
	case '|':
		/* Logical operators */
		rel_bin_op = 1;
		if (CB_TREE_CLASS (x) != CB_CLASS_BOOLEAN
		 || (y && CB_TREE_CLASS (y) != CB_CLASS_BOOLEAN)) {
			copy_file_line (e, y, x);
			if (CB_NUMERIC_LITERAL_P(x)
			 && y
			 && CB_NUMERIC_LITERAL_P(y)) {
				xl = (void*)x;
				yl = (void*)y;
				llit = (char*)xl->data;
				rlit = (char*)yl->data;
				cb_error_x (e, _("invalid expression: %s %s %s"),
					llit, explain_operator (op), rlit);
			} else {
				cb_error_x (e, _("invalid expression: boolean expected with logical operator"));
			}
			return cb_error_node;
		}
		if ((x == cb_true || x == cb_false)
		 && (y == cb_true || y == cb_false)) {
			warn_ok = 0;
			if (op == '&') {
				if (x == cb_true && y == cb_true) {
					relop = cb_true;
				} else {
					relop = cb_false;
				}
			} else
			if (op == '|') {
				if (x == cb_true || y == cb_true) {
					relop = cb_true;
				} else {
					relop = cb_false;
				}
			}
		} else if (op == '!') {
			if (x == cb_true) {
				relop = cb_false;
				warn_ok = 0;
			} else if (x == cb_false) {
				relop = cb_true;
				warn_ok = 0;
			}
		}
		category = CB_CATEGORY_BOOLEAN;
		break;

	case '@':
		/* Parentheses */
		category = CB_TREE_CATEGORY (x);
		break;

	case 0:
		/* Operation on invalid elements */
		return cb_error_node;

	/* LCOV_EXCL_START */
	default:
		cobc_err_msg (_("unexpected operator: %d"), op);
		COBC_ABORT ();
	/* LCOV_EXCL_STOP */
	}

	if (relop == cb_true) {
		enum cb_warn_opt warn_opt = get_warnopt_for_constant (x, y);
		if (get_warn_opt_value (warn_opt) && warn_ok) {
			if (rlit && llit) {
				if (!was_prev_warn (e->source_line, warn_type)) {
					cb_warning_x (warn_opt, e,
						_("expression '%.38s' %s '%.38s' is always TRUE"),
						llit, explain_operator (op), rlit);
				}
			} else {
				if (!was_prev_warn (e->source_line, -warn_type)) {
					cb_warning_x (warn_opt, e,
						_("expression is always TRUE"));
				}
			}
			prev_expr_line = cb_exp_line = e->source_line;
		}
		return cb_true;
	}
	if (relop == cb_false) {
		enum cb_warn_opt warn_opt = get_warnopt_for_constant (x, y);
		if (get_warn_opt_value (warn_opt) && warn_ok) {
			if (rlit && llit) {
				if (!was_prev_warn (e->source_line, 9 + warn_type)) {
					cb_warning_x (warn_opt, e,
						_("expression '%.38s' %s '%.38s' is always FALSE"),
						llit, explain_operator (op), rlit);
				}
			} else {
				if (!was_prev_warn (e->source_line, -(9 + warn_type))) {
					cb_warning_x (warn_opt, e,
						_("expression is always FALSE"));
				}
			}
			prev_expr_line = cb_exp_line = e->source_line;
		}
		return cb_false;
	}

	p = make_tree (CB_TAG_BINARY_OP, category, sizeof (struct cb_binary_op));
	p->op = op;
	p->x = x;
	p->y = y;
	copy_file_line (CB_TREE (p), x, y);
	return CB_TREE (p);
}

cb_tree
cb_build_binary_list (cb_tree l, const int op)
{
	cb_tree e = CB_VALUE (l);
	for (l = CB_CHAIN (l); l; l = CB_CHAIN (l)) {
		e = cb_build_binary_op (e, op, CB_VALUE (l));
	}
	return e;
}

/* Function call */

cb_tree
cb_build_funcall (const char *name, const int argc,
		  const cb_tree a1, const cb_tree a2, const cb_tree a3,
		  const cb_tree a4, const cb_tree a5, const cb_tree a6,
		  const cb_tree a7, const cb_tree a8, const cb_tree a9,
		  const cb_tree a10, const cb_tree a11, const cb_tree a12,
		  const cb_tree a13, const cb_tree a14)
{
	struct cb_funcall *p;

	p = make_tree (CB_TAG_FUNCALL, CB_CATEGORY_BOOLEAN,
		       sizeof (struct cb_funcall));
	p->name = name;
	p->argc = argc;
	p->varcnt = 0;
	p->screenptr = gen_screen_ptr;
	p->argv[0] = a1;
	p->argv[1] = a2;
	p->argv[2] = a3;
	p->argv[3] = a4;
	p->argv[4] = a5;
	p->argv[5] = a6;
	p->argv[6] = a7;
	p->argv[7] = a8;
	p->argv[8] = a9;
	p->argv[9] = a10;
	p->argv[10] = a11;
	p->argv[11] = a12;
	p->argv[12] = a13;
	p->argv[13] = a14;
	return CB_TREE (p);
}

/* Type cast */

cb_tree
cb_build_cast (const enum cb_cast_type type, const cb_tree val)
{
	struct cb_cast		*p;
	enum cb_category	category;

	switch (type) {
	case CB_CAST_INTEGER:
	case CB_CAST_LONG_INT:
	case CB_CAST_LENGTH:
	case CB_CAST_NEGATIVE_INTEGER:
	case CB_CAST_NEGATIVE_LONG_INT:
		category = CB_CATEGORY_NUMERIC;
		break;
	default:
		category = CB_CATEGORY_UNKNOWN;
	}
	p = make_tree (CB_TAG_CAST, category, sizeof (struct cb_cast));
	p->cast_type = type;
	p->val = val;
	return CB_TREE (p);
}

cb_tree
cb_build_cast_int (const cb_tree val)
{
	struct cb_cast		*p;

	p = make_tree (CB_TAG_CAST, CB_CATEGORY_NUMERIC, sizeof (struct cb_cast));
	p->cast_type = CB_CAST_INTEGER;
	p->val = val;
	return CB_TREE (p);
}

cb_tree
cb_build_cast_llint (const cb_tree val)
{
	struct cb_cast		*p;

	p = make_tree (CB_TAG_CAST, CB_CATEGORY_NUMERIC, sizeof (struct cb_cast));
	p->cast_type = CB_CAST_LONG_INT;
	p->val = val;
	return CB_TREE (p);
}

/* Label */

cb_tree
cb_build_label (cb_tree name, struct cb_label *section)
{
	cb_tree		x;
	struct cb_label		*p;
	struct cb_para_label	*l;

	p = make_tree (CB_TAG_LABEL, CB_CATEGORY_UNKNOWN,
		       sizeof (struct cb_label));
	p->id = cb_id++;
	p->name = cb_define (name, CB_TREE (p));
	p->orig_name = p->name;
	p->section = section;
	if (section) {
		l = cobc_parse_malloc (sizeof(struct cb_para_label));
		l->next = section->para_label;
		l->para= p;
		section->para_label = l;
		p->section_id = p->section->id;
	} else {
		p->section_id = p->id;
	}
	x = CB_TREE (p);
	x->source_file = cb_source_file;
	x->source_line = cb_source_line;
	return x;
}

/* Assign */

cb_tree
cb_build_assign (const cb_tree var, const cb_tree val)
{
	struct cb_assign *p;

	p = make_tree (CB_TAG_ASSIGN, CB_CATEGORY_UNKNOWN,
		       sizeof (struct cb_assign));
	p->var = var;
	p->val = val;
	return CB_TREE (p);
}

/* INITIALIZE */

cb_tree
cb_build_initialize (const cb_tree var, const cb_tree val, const cb_tree rep,
		     const unsigned int def,
		     const enum cob_statement statement,
		     const unsigned int no_filler_init)
{
	struct cb_initialize *p;

	p = make_tree (CB_TAG_INITIALIZE, CB_CATEGORY_UNKNOWN,
		       sizeof (struct cb_initialize));
	p->var = var;
	p->val = val;
	p->rep = rep;
	p->flag_default = (cob_u8_t)def;
	p->statement = statement;
	p->flag_no_filler_init = (cob_u8_t)no_filler_init;
	return CB_TREE (p);
}

/* SEARCH */

cb_tree
cb_build_search (const int flag_all, const cb_tree table, const cb_tree var,
		 const cb_tree at_end, const cb_tree whens)
{
	struct cb_search *p;

	p = make_tree (CB_TAG_SEARCH, CB_CATEGORY_UNKNOWN,
		       sizeof (struct cb_search));
	p->flag_all = flag_all;
	p->table = table;
	p->var = var;
	p->at_end = at_end;
	p->whens = whens;
	return CB_TREE (p);
}

/* CALL */

cb_tree
cb_build_call (const cb_tree name, const cb_tree args, const cb_tree on_exception,
	       const cb_tree not_on_exception, const cb_tree returning,
	       const cob_u32_t is_system_call, const int convention)
{
	struct cb_call *p;

	p = make_tree (CB_TAG_CALL, CB_CATEGORY_UNKNOWN,
		       sizeof (struct cb_call));
	p->name = name;
	p->args = args;
	p->stmt1 = on_exception;
	p->stmt2 = not_on_exception;
	p->call_returning = returning;
	p->is_system = is_system_call;
	p->convention = convention;
	return CB_TREE (p);
}

cb_tree
cb_build_call_parameter (cb_tree arg, int call_mode, const int size_mode)
{
	cb_tree	res;
	if (call_mode != CB_CALL_BY_REFERENCE) {
		if (CB_FILE_P (arg)
		|| (CB_REFERENCE_P (arg) && CB_FILE_P (CB_REFERENCE (arg)->value))) {
			cb_error_x (CB_TREE (current_statement),
				    _("invalid file name reference"));
		} else if (call_mode == CB_CALL_BY_VALUE) {
			/* FIXME: compiler configuration needed, IBM allows one-byte
			          alphanumeric items [--> a `char`], too, while
			          COBOL 2002/2014 allow only numeric literals
			   --> revise after rw-merge */
			if (cb_category_is_alpha (arg)) {
				cb_warning_x (COBC_WARN_FILLER, arg,
					      _("BY CONTENT assumed for alphanumeric item '%s'"),
						  cb_name (arg));
				call_mode = CB_CALL_BY_CONTENT;
			} else if (cb_category_is_national (arg)) {
				cb_warning_x (COBC_WARN_FILLER, arg,
					      _("BY CONTENT assumed for national item '%s'"),
						  cb_name (arg));
				call_mode = CB_CALL_BY_CONTENT;
			} else if (arg == cb_zero) {
				/* conversion of single "constant" numeric literal */
				arg = CB_TREE(cb_build_numeric_literal (0, "0", 0));
			}
		}
	}
	
	res = CB_BUILD_PAIR (cb_int (call_mode), arg);
	if (call_mode == CB_CALL_BY_VALUE) {
		if (size_mode != CB_SIZE_UNSET) {
			CB_SIZES (res) = size_mode;
		} else {
#ifdef COB_64_BIT_POINTER
			CB_SIZES (res) = CB_SIZE_8;
#else
			CB_SIZES (res) = CB_SIZE_4;
#endif
		}
	}
	return res;
}

/* CANCEL */

cb_tree
cb_build_cancel (const cb_tree target)
{
	struct cb_cancel *p;

	p = make_tree (CB_TAG_CANCEL, CB_CATEGORY_UNKNOWN,
		       sizeof (struct cb_cancel));
	p->target = target;
	return CB_TREE (p);
}

/* ALTER */

cb_tree
cb_build_alter (const cb_tree source, const cb_tree target)
{
	struct cb_alter *p;

	p = make_tree (CB_TAG_ALTER, CB_CATEGORY_UNKNOWN,
		       sizeof (struct cb_alter));
	p->source = source;
	p->target = target;
	current_program->alter_list =
		cb_list_append (current_program->alter_list,
				CB_BUILD_PAIR (source, target));
	return CB_TREE (p);
}

/* GO TO */

cb_tree
cb_build_goto (const cb_tree target, const cb_tree depending, int flags)
{
	struct cb_goto *p;

	p = make_tree (CB_TAG_GOTO, CB_CATEGORY_UNKNOWN,
		       sizeof (struct cb_goto));
	p->target = target;
	p->depending = depending;
	p->flags = flags;
	return CB_TREE (p);
}

/* IF / WHEN / PRESENT WHEN */

cb_tree
cb_build_if (const cb_tree test, const cb_tree stmt1, const cb_tree stmt2,
	     const enum cob_statement generating_statement)
{
	struct cb_if *p;
	struct cb_binary_op	*bop;

	p = make_tree (CB_TAG_IF, CB_CATEGORY_UNKNOWN,
		       sizeof (struct cb_if));
	p->test = test;
	p->stmt1 = stmt1;
	p->stmt2 = stmt2;
	if (cb_flag_remove_unreachable) {
		if (test == cb_true) {		/* Always TRUE so skip 'else code' */
			p->stmt2 = NULL;
		} else if (test == cb_false) {	/* Always FALSE, so skip 'true code' */
			p->stmt1 = NULL;
		}
	}
	if (test
	 && CB_TREE_TAG (test) == CB_TAG_BINARY_OP) {
		bop = CB_BINARY_OP (test);
		if (bop->op == '!') {
			if (bop->x == cb_true) {
				p->stmt1 = NULL;
			} else if (bop->x == cb_false) {
				p->stmt2 = NULL;
			}
		}
	}
	p->statement = generating_statement;
	return CB_TREE (p);
}

/* PERFORM */

cb_tree
cb_build_perform (const enum cb_perform_type type)
{
	struct cb_perform *p;

	p = make_tree (CB_TAG_PERFORM, CB_CATEGORY_UNKNOWN,
		       sizeof (struct cb_perform));
	p->perform_type = type;
	return CB_TREE (p);
}

void
cb_build_perform_after_until(void)
{
	after_until = 1;
}

cb_tree
cb_build_perform_varying (cb_tree name, cb_tree from, cb_tree by, cb_tree until)
{
	struct cb_perform_varying	*p;
	cb_tree				x;
	cb_tree				l;

	p = make_tree (CB_TAG_PERFORM_VARYING, CB_CATEGORY_UNKNOWN,
		       sizeof (struct cb_perform_varying));
	p->name = name;
	p->from = from;
	p->until = until;

	if (until == cb_false) {
		cb_warning_x (cb_warn_additional, until,
			_("PERFORM FOREVER since UNTIL is always FALSE"));
	} else if (until == cb_true) {
		if (after_until) {
			cb_warning_x (cb_warn_additional, until,
			_("PERFORM ONCE since UNTIL is always TRUE"));
		} else {
			cb_warning_x (cb_warn_additional, until,
			_("PERFORM NEVER since UNTIL is always TRUE"));
		}
	}

	if (until) {
		cb_save_cond ();
	}
	if (until == cb_true
	 && !after_until) {
		cb_false_side ();	/* PERFORM body is NEVER executed */
	}

	after_until = 0;
	if (name) {
		l = cb_ref (name);
		if (l == cb_error_node) {
			p->step = NULL;
			return CB_TREE (p);
		}
		x = cb_build_add (name, by, cb_high);
		copy_file_line (x, by, NULL);

		if (current_program->flag_debugging &&
		    !current_statement->flag_in_debug &&
		    CB_FIELD_P (l) && CB_FIELD (l)->flag_field_debug) {
			p->step = CB_LIST_INIT (x);
			x = cb_build_debug (cb_debug_name, CB_FIELD_PTR (name)->name,
					    NULL);
			p->step = cb_list_add (p->step, x);
			x = cb_build_debug (cb_debug_contents, NULL, name);
			p->step = cb_list_add (p->step, x);
			x = cb_build_debug_call (CB_FIELD_PTR (name)->debug_section);
			p->step = cb_list_add (p->step, x);
		} else {
			p->step = x;
		}
	} else {
		p->step = NULL;
	}
	return CB_TREE (p);
}

/* Statement */

struct cb_statement *
cb_build_statement (enum cob_statement statement)
{
	struct cb_statement *p;

	p = make_tree (CB_TAG_STATEMENT, CB_CATEGORY_UNKNOWN,
		       sizeof (struct cb_statement));
	p->statement = statement;
	return p;
}

/* CONTINUE */

cb_tree
cb_build_continue (void)
{
	struct cb_continue *p;

	p = make_tree (CB_TAG_CONTINUE, CB_CATEGORY_UNKNOWN,
		       sizeof (struct cb_continue));
	return CB_TREE (p);
}

/* SET ATTRIBUTE */

cb_tree
cb_build_set_attribute (const struct cb_field *fld,
			const cob_flags_t val_on, const cob_flags_t val_off)
{
	struct cb_set_attr *p;

	p = make_tree (CB_TAG_SET_ATTR, CB_CATEGORY_UNKNOWN,
		       sizeof (struct cb_set_attr));
	p->fld = (struct cb_field *)fld;
	p->val_on = val_on;
	p->val_off = val_off;
	return CB_TREE (p);
}

/* XML PARSE */

cb_tree
cb_build_xml_parse (cb_tree data, cb_tree proc,
		      const int returning_national,
		      cb_tree encoding, cb_tree validation)
{
	struct cb_xml_parse *p;

	p = make_tree (CB_TAG_XML_PARSE, CB_CATEGORY_UNKNOWN,
		       sizeof (struct cb_xml_parse));
	p->data = data;
	p->proc = cb_build_perform_once (proc);
	p->encoding = encoding;
	p->validating = validation;
	p->returning_national = returning_national;
	p->common.source_file = current_statement->common.source_file;
	p->common.source_line = current_statement->common.source_line;
	return CB_TREE (p);
}

/* Prototypes */

static void
warn_if_no_definition_seen_for_prototype (const struct cb_prototype *proto)
{
	struct cb_program	*program;
	const char		*error_msg;

	program = cb_find_defined_program_by_id (proto->ext_name);
	if (program) {
		return;
	}

	if (get_warn_opt_value (cb_warn_ignored_initial_val) != COBC_WARN_DISABLED) {
		if (strcmp (proto->name, proto->ext_name) == 0) {
			/*
			  Warn if no definition seen for element with prototype-
			  name.
			*/
			if (proto->type == COB_MODULE_TYPE_FUNCTION) {
				error_msg = _("no definition/prototype seen for FUNCTION '%s'");
			} else { /* PROGRAM_TYPE */
				error_msg = _("no definition/prototype seen for PROGRAM '%s'");
			}
			cb_warning_x (cb_warn_prototypes, CB_TREE (proto), error_msg, proto->name);
		} else {
			/*
			  Warn if no definition seen for element with given
			  external-name.
			*/
			if (proto->type == COB_MODULE_TYPE_FUNCTION) {
				error_msg = _("no definition/prototype seen for FUNCTION with external name '%s'");
			} else { /* PROGRAM_TYPE */
				error_msg = _("no definition/prototype seen for PROGRAM with external name '%s'");
			}
			cb_warning_x (cb_warn_prototypes, CB_TREE (proto), error_msg, proto->ext_name);
		}
	}
}

cb_tree
cb_build_prototype (const cb_tree prototype_name, const cb_tree ext_name,
		    const enum cob_module_type type)
{
	struct cb_prototype	*prototype;

	prototype = make_tree (CB_TAG_PROTOTYPE, CB_CATEGORY_UNKNOWN,
			       sizeof (struct cb_prototype));
	CB_TREE (prototype)->source_line = prototype_name->source_line;

	/* Set prototype->name */
	if (CB_LITERAL_P (prototype_name)) {
		prototype->name =
			(const char *) CB_LITERAL (prototype_name)->data;
	} else {
		prototype->name = (const char *) CB_NAME (prototype_name);
	}

	/* Set prototype->ext_name */
	if (ext_name) {
		prototype->ext_name =
			(const char *) CB_LITERAL (ext_name)->data;
	} else if (CB_LITERAL_P (prototype_name)) {
		prototype->ext_name =
			(const char *) CB_LITERAL (prototype_name)->data;
	} else {
		prototype->ext_name = CB_NAME (prototype_name);
	}

	prototype->type = type;

	warn_if_no_definition_seen_for_prototype (prototype);

	return CB_TREE (prototype);
}

/* FUNCTION */

/* Build an internal reference to FUNCTION BYTE-LENGTH for resolving LENGTH OF special-register */
cb_tree
cb_build_any_intrinsic (cb_tree args)
{
	struct cb_intrinsic_table	*cbp;

	cbp = lookup_intrinsic ("BYTE-LENGTH", 1);
	return make_intrinsic (NULL, cbp, args, NULL, NULL, 0);
}

static enum cb_category
get_category_from_arguments (const struct cb_intrinsic_table *cbp, cb_tree args,
							 const int check_from, const int check_to,
							 const int with_alphabetic)
{
	enum cb_category result = cbp->category;
	enum cb_category arg_cat;
	cb_tree			l;
	cb_tree			arg;
	int argnum = 0;

	for (l = args; l; l = CB_CHAIN (l)) {

		argnum++;
		if (argnum < check_from) continue;
		if (check_to && argnum > check_to) break;

		arg = CB_VALUE (l);
		arg_cat = cb_tree_category (arg);

		if (arg_cat == CB_CATEGORY_NATIONAL_EDITED) {
			arg_cat = CB_CATEGORY_NATIONAL;
		} else if (arg_cat == CB_CATEGORY_ALPHABETIC && with_alphabetic) {
			/* unchanged */
		} else {
			arg_cat = CB_CATEGORY_ALPHANUMERIC;
		}

		/* first argument specifies the type */
		if (argnum == check_from) {
			result = arg_cat;
			continue;
		}

		/* check for national match */
		if (arg_cat == CB_CATEGORY_NATIONAL) {
			if (result != CB_CATEGORY_NATIONAL) {
				cb_error (_("FUNCTION '%s' has invalid argument"),
					cbp->name);
				cb_error (_("either all arguments or none should be of type %s"), "NATIONAL");
				return cbp->category;
			}
		} else if (result != CB_CATEGORY_ALPHANUMERIC) {
			result = CB_CATEGORY_ALPHANUMERIC;
		}
	}

	return result;
}

cb_tree
cb_build_intrinsic (cb_tree func, cb_tree args, cb_tree refmod,
		    const int isuser)
{
	struct cb_intrinsic_table	*cbp;
	cb_tree					x;
	struct cb_field			*fld;
	enum cb_category		catg;

	const char *name = CB_NAME (func);

	/* TODO: if all arguments are constants: build a cob_field,
	   then call into libcob to get the value and from there the string representation
	   inserting it here directly (-> numeric/alphanumeric/national constant,
	   which allows also for optimized use of it */

	int numargs = (int)cb_list_length (args);

	if (unlikely (isuser)) {
		if (refmod && CB_LITERAL_P (CB_PAIR_X (refmod))
		 && cb_get_int (CB_PAIR_X (refmod)) < 1) {
			cb_error_x (func, _("FUNCTION '%s' has invalid reference modification"), name);
			return cb_error_node;
		}
		if (refmod && CB_PAIR_Y (refmod)
		 && CB_LITERAL_P (CB_PAIR_Y (refmod))
		 && cb_get_int (CB_PAIR_Y (refmod)) < 1) {
			cb_error_x (func, _("FUNCTION '%s' has invalid reference modification"), name);
			return cb_error_node;
		}
		if (numargs > (int)current_program->max_call_param) {
			current_program->max_call_param = numargs;
		}
		return make_intrinsic (func, &userbp, args, cb_int1, refmod, 1);
	}

	cbp = lookup_intrinsic (name, 1);
	if (!cbp || cbp->active == CB_FEATURE_DISABLED) {
		cb_error_x (func, _("FUNCTION '%s' unknown"), name);
		return cb_error_node;
	}
	if (cbp->active == CB_FEATURE_NOT_IMPLEMENTED) {
		cb_error_x (func, _("FUNCTION '%s' is not implemented"), name);
		return cb_error_node;
	}
	if ((cbp->args == -1)) {
		if (numargs < cbp->min_args) {
			cb_error_x (func,
				_("FUNCTION '%s' has wrong number of arguments"),
				name);
			return cb_error_node;
		}
	} else {
		if (numargs > cbp->args || numargs < cbp->min_args) {
			cb_error_x (func,
					_("FUNCTION '%s' has wrong number of arguments"),
					name);
			return cb_error_node;
		}
	}
	if (refmod) {
		if (!cbp->refmod) {
			cb_error_x (func, _("FUNCTION '%s' cannot have reference modification"), name);
			return cb_error_node;
		}
		/* TODO: better check needed, see typeck.c (cb_build_identifier) */
		if (CB_LITERAL_P (CB_PAIR_X (refmod))
		 && cb_get_int (CB_PAIR_X (refmod)) < 1) {
			cb_error_x (func, _("FUNCTION '%s' has invalid reference modification"), name);
			return cb_error_node;
		}
		if (CB_PAIR_Y (refmod)
		 && CB_LITERAL_P (CB_PAIR_Y (refmod))
		 && cb_get_int (CB_PAIR_Y (refmod)) < 1) {
			cb_error_x (func, _("FUNCTION '%s' has invalid reference modification"), name);
			return cb_error_node;
		}
	}

	if (iso_8601_func (cbp->intr_enum)) {
		if (!valid_const_date_time_args (func, cbp, args)) {
			return cb_error_node;
		}
	}

	/* FIXME: Some FUNCTIONS need a test for / adjustment depending on their arguments' category:
	   * CONCATENATE/SUBSTITUTE/...
	     all should be of the same category alphanumeric/alphabetic vs. national
	   * MAX/REVERSE/TRIM/...
	     depending on the arguments' category the type of the function must be adjusted
	*/

	switch (cbp->intr_enum) {
	case CB_INTR_LENGTH:
	case CB_INTR_BYTE_LENGTH:
		x = CB_VALUE (args);
		if (CB_REF_OR_FIELD_P (x)) {
			fld = CB_FIELD_PTR (x);
			if (!cb_field_variable_size (fld)
			 && !fld->flag_any_length) {
				int 	len = fld->size;
				char	buff[32];
				if (cbp->intr_enum != CB_INTR_BYTE_LENGTH) {
					/* CHECKME: why don't we just check the category?
					   Maybe needs to enforce field validation (see cb_build_length) */
					if (fld->pic) {
						if (fld->pic->category == CB_CATEGORY_NATIONAL
						 || fld->pic->category == CB_CATEGORY_NATIONAL_EDITED) {
							len /= COB_NATIONAL_SIZE;
						} else if (fld->pic->orig && fld->pic->orig[0] == 'U') {
							len /= 4;
						}
					}
				}
				sprintf (buff, "%d", len);
				return cb_build_numeric_literal (0, buff, 0);
			}
		} else if (CB_LITERAL_P (x)) {
			unsigned int 	len = CB_LITERAL(x)->size;
			char	buff[32];
			if (cbp->intr_enum != CB_INTR_BYTE_LENGTH) {
				enum cb_category cat = CB_TREE_CATEGORY (x);
				/* CHECKME: why don't we just check the category?
				   Maybe needs to enforce field validation (see cb_build_length) */
				if (cat == CB_CATEGORY_NATIONAL
				 || cat == CB_CATEGORY_NATIONAL_EDITED) {
					len /= COB_NATIONAL_SIZE;
				}
			}
			sprintf (buff, "%u", len);
			return cb_build_numeric_literal (0, buff, 0);
		}
		return make_intrinsic (func, cbp, args, NULL, NULL, 0);

	case CB_INTR_WHEN_COMPILED:
		if (refmod) {
			return make_intrinsic (func, cbp,
				CB_LIST_INIT (cb_intr_whencomp), NULL, refmod, 0);
		} else {
			return cb_intr_whencomp;
		}

	/* single, numeric only argument */
	case CB_INTR_ABS:
	case CB_INTR_ACOS:
	case CB_INTR_ASIN:
	case CB_INTR_ATAN:
	case CB_INTR_COS:
	case CB_INTR_DATE_OF_INTEGER:
	case CB_INTR_DAY_OF_INTEGER:
	case CB_INTR_EXP:
	case CB_INTR_EXP10:
	case CB_INTR_FACTORIAL:
	case CB_INTR_FRACTION_PART:
	case CB_INTR_INTEGER:
	case CB_INTR_INTEGER_OF_DATE:
	case CB_INTR_INTEGER_OF_DAY:
	case CB_INTR_INTEGER_PART:
	case CB_INTR_LOG:
	case CB_INTR_LOG10:
	case CB_INTR_SIGN:
	case CB_INTR_SIN:
	case CB_INTR_SQRT:
	case CB_INTR_TAN:
	/* Fixme: should validate following are taking integers */
	case CB_INTR_TEST_DATE_YYYYMMDD:
	case CB_INTR_TEST_DAY_YYYYDDD:
		x = CB_VALUE (args);
		if (cb_tree_category (x) != CB_CATEGORY_NUMERIC) {
			cb_error_x (func, _("FUNCTION '%s' has invalid argument"), name);
			return cb_error_node;
		}
		return make_intrinsic (func, cbp, args, NULL, refmod, 0);

	case CB_INTR_ANNUITY:
	case CB_INTR_BOOLEAN_OF_INTEGER:
	case CB_INTR_CHAR:
	case CB_INTR_CHAR_NATIONAL:
	case CB_INTR_COMBINED_DATETIME:
	case CB_INTR_CURRENCY_SYMBOL:
	case CB_INTR_CURRENT_DATE:
	case CB_INTR_E:
	case CB_INTR_EXCEPTION_FILE:
	case CB_INTR_EXCEPTION_FILE_N:
	case CB_INTR_EXCEPTION_LOCATION:
	case CB_INTR_EXCEPTION_LOCATION_N:
	case CB_INTR_EXCEPTION_STATUS:
	case CB_INTR_EXCEPTION_STATEMENT:
	case CB_INTR_INTEGER_OF_BOOLEAN:
	case CB_INTR_INTEGER_OF_FORMATTED_DATE:
	case CB_INTR_LOCALE_DATE:
	case CB_INTR_LOCALE_TIME:
	case CB_INTR_LOCALE_TIME_FROM_SECS:
	case CB_INTR_MOD:
	case CB_INTR_MODULE_CALLER_ID:
	case CB_INTR_MODULE_DATE:
	case CB_INTR_MODULE_FORMATTED_DATE:
	case CB_INTR_MODULE_ID:
	case CB_INTR_MODULE_PATH:
	case CB_INTR_MODULE_SOURCE:
	case CB_INTR_MODULE_TIME:
	case CB_INTR_MON_DECIMAL_POINT:
	case CB_INTR_MON_THOUSANDS_SEP:
	case CB_INTR_NUM_DECIMAL_POINT:
	case CB_INTR_NUM_THOUSANDS_SEP:
	case CB_INTR_NUMVAL:
	case CB_INTR_NUMVAL_C:
	case CB_INTR_NUMVAL_F:
	case CB_INTR_ORD:
	case CB_INTR_PI:
	case CB_INTR_REM:
	case CB_INTR_SECONDS_FROM_FORMATTED_TIME:
	case CB_INTR_SECONDS_PAST_MIDNIGHT:
	case CB_INTR_STORED_CHAR_LENGTH:
	case CB_INTR_TEST_FORMATTED_DATETIME:
	case CB_INTR_TEST_NUMVAL:
	case CB_INTR_TEST_NUMVAL_C:
	case CB_INTR_TEST_NUMVAL_F:
		return make_intrinsic (func, cbp, args, NULL, refmod, 0);

	/* category has to be adjusted depending on arguments */
	case CB_INTR_FORMATTED_CURRENT_DATE:
	case CB_INTR_FORMATTED_DATE: {
		enum cb_category cat = get_category_from_arguments (cbp, args, 1, 1, 0);
		return make_intrinsic_typed (func, cbp, cat, args, NULL, refmod, 0);
		}
	case CB_INTR_REVERSE:
	case CB_INTR_TRIM:
	case CB_INTR_LOWER_CASE:
	case CB_INTR_UPPER_CASE: {
		enum cb_category cat = get_category_from_arguments (cbp, args, 1, 1, 1);
		return make_intrinsic_typed (func, cbp, cat, args, NULL, refmod, 0);
		}
	case CB_INTR_FORMATTED_TIME:
	case CB_INTR_FORMATTED_DATETIME: {
		enum cb_category cat = get_category_from_arguments (cbp, args, 1, 1, 0);
		return make_intrinsic_typed (func, cbp, cat, args, cb_int1, refmod, 0);
		}

	case CB_INTR_HIGHEST_ALGEBRAIC:
	case CB_INTR_LOWEST_ALGEBRAIC:
		/* TODO: resolve for all (?) values */
		x = CB_VALUE (args);
		if (!CB_REF_OR_FIELD_P (x)) {
			cb_error_x (func, _("FUNCTION '%s' has invalid argument"), name);
			return cb_error_node;
		}
		catg = cb_tree_category (x);
		if (catg != CB_CATEGORY_NUMERIC &&
		    catg != CB_CATEGORY_NUMERIC_EDITED) {
			cb_error_x (func, _("FUNCTION '%s' has invalid argument"), name);
			return cb_error_node;
		}
		return make_intrinsic (func, cbp, args, NULL, refmod, 0);

	case CB_INTR_CONTENT_LENGTH:
		x = CB_VALUE (args);
		if (cb_tree_category (x) != CB_CATEGORY_DATA_POINTER) {
			cb_error_x (func, _("FUNCTION '%s' has invalid argument"), name);
			return cb_error_node;
		}
		return make_intrinsic (func, cbp, args, NULL, NULL, 0);

	case CB_INTR_CONTENT_OF:
		x = CB_VALUE (args);
		if (cb_tree_category (x) != CB_CATEGORY_DATA_POINTER) {
			cb_error_x (func, _("FUNCTION '%s' has invalid argument"), name);
			return cb_error_node;
		}
		return make_intrinsic (func, cbp, args, cb_int1, refmod, 0);

	case CB_INTR_CONCATENATE:{
		enum cb_category cat = get_category_from_arguments (cbp, args, 1, 0, 1);
		return make_intrinsic_typed (func, cbp, cat, args, cb_int1, refmod, 0);
		}

	case CB_INTR_DISPLAY_OF:
	case CB_INTR_NATIONAL_OF:
		/* TODO: resolve for literals */
		return make_intrinsic (func, cbp, args, cb_int1, refmod, 0);


	case CB_INTR_BIT_OF:
	case CB_INTR_HEX_OF:
		/* TODO: resolve for literals */
		x = CB_VALUE (args);
		if (!CB_REF_OR_FIELD_P (x)
		 && !CB_LITERAL_P (x)) {
			cb_error_x (func, _ ("FUNCTION '%s' has invalid argument"), name);
			return cb_error_node;
		}
		return make_intrinsic (func, cbp, args, NULL, refmod, 0);
	case CB_INTR_BIT_TO_CHAR:
	case CB_INTR_HEX_TO_CHAR:
		/* TODO: resolve for literals */
		x = CB_VALUE (args);
		if (!CB_REF_OR_FIELD_P (x)
		  &&!CB_LITERAL_P (x)) {
			cb_error_x (func, _ ("FUNCTION '%s' has invalid argument"), name);
			return cb_error_node;
		}
		if (!cb_category_is_alpha (x)
		 || cb_field_size(x) % 2 != 0) {
			cb_error_x (func, _ ("FUNCTION '%s' has invalid argument"), name);
			return cb_error_node;
		}
		return make_intrinsic (func, cbp, args, NULL, refmod, 0);

	/* multiple, numeric only arguments */
	case CB_INTR_MEAN:
	case CB_INTR_MEDIAN:
	case CB_INTR_MIDRANGE:
	case CB_INTR_PRESENT_VALUE:
	case CB_INTR_RANGE:
	case CB_INTR_STANDARD_DEVIATION:
	case CB_INTR_SUM:
	case CB_INTR_VARIANCE:
		return make_intrinsic (func, cbp, args, cb_int1, NULL, 0);

	/* multiple, compatible only arguments */
	case CB_INTR_MAX:
	case CB_INTR_MIN:
		return make_intrinsic (func, cbp, args, cb_int1, NULL, 0);

	/* */
	case CB_INTR_DATE_TO_YYYYMMDD:
	case CB_INTR_DAY_TO_YYYYDDD:
	case CB_INTR_LOCALE_COMPARE:
	case CB_INTR_ORD_MAX:
	case CB_INTR_ORD_MIN:
	case CB_INTR_RANDOM:
	case CB_INTR_STANDARD_COMPARE:
	case CB_INTR_YEAR_TO_YYYY:
		return make_intrinsic (func, cbp, args, cb_int1, NULL, 0);

	/* currently GnuCOBOL only extension (submitted to COBOL 202x),
	   category adjusted depending on argument */
	case CB_INTR_SUBSTITUTE:
	case CB_INTR_SUBSTITUTE_CASE:
		if ((numargs % 2) == 0) {
			cb_error_x (func, _("FUNCTION '%s' has wrong number of arguments"), name);
			return cb_error_node;
		}

		/* TODO: follow-up arguments should be of same type */
		if (!cb_category_is_alpha_or_national (CB_VALUE (args))) {
			cb_error_x (func, _("FUNCTION '%s' has invalid first argument"), name);
			return cb_error_node;
		}
		{
		enum cb_category cat = get_category_from_arguments (cbp, args, 1, 0, 1);
		return make_intrinsic_typed (func, cbp, cat, args, cb_int1, refmod, 0);
		}

	default:
		cb_error_x (func, _("FUNCTION '%s' unknown"), name);
		return cb_error_node;
	}
}

/* JSON/XML GENERATE */

cb_tree
cb_build_ml_suppress_clause (void)
{
	struct cb_ml_suppress_clause *s;

	s = make_tree (CB_TAG_ML_SUPPRESS, CB_CATEGORY_UNKNOWN,
		       sizeof (struct cb_ml_suppress_clause));
	s->target = CB_ML_SUPPRESS_ALL;
	s->category = CB_ML_SUPPRESS_CAT_ANY;
	s->ml_type = CB_ML_ANY_TYPE;
	return CB_TREE (s);
}

cb_tree
cb_build_ml_tree (struct cb_field *record, const int children_are_attrs,
		  const int default_to_attr, cb_tree name_list,
		  cb_tree type_list, cb_tree suppress_list)
{
	struct cb_ml_generate_tree	*tree;

	if (is_unconditionally_suppressed (record, suppress_list)) {
		return NULL;
	}

	tree = make_tree (CB_TAG_ML_TREE, CB_CATEGORY_UNKNOWN,
			  sizeof (struct cb_ml_generate_tree));
	tree->sibling = NULL;
	tree->type = get_ml_type (CB_TREE (record), type_list, default_to_attr);
	tree->name = get_ml_name (CB_TREE (record), name_list, tree->type);
	if (tree->type == CB_ML_ATTRIBUTE) {
		tree->id = cb_ml_attr_id++;
	} else {
		tree->id = cb_ml_tree_id++;
	}

	set_ml_attrs_and_children (record, children_are_attrs, name_list,
				   type_list, suppress_list, &tree);

	/*
	  Note we test if the *record* has children. The tree may not have
	  children, e.g. if all the record's children are ATTRIBUTES or
	  are SUPPRESSed.
	*/
	if (record->children && tree->type == CB_ML_ELEMENT) {
		tree->value = NULL;
	} else {
		tree->value = CB_TREE (record);
	}

	tree->suppress_cond = get_suppress_cond (tree->value, tree->type,
						 suppress_list);

	return CB_TREE (tree);
}

cb_tree
cb_build_ml_suppress_checks (struct cb_ml_generate_tree *tree)
{
	struct cb_ml_suppress_checks	*check
		= make_tree (CB_TAG_ML_SUPPRESS_CHECKS, CB_CATEGORY_UNKNOWN,
			     sizeof (struct cb_ml_suppress_checks));
	check->tree = tree;
	return CB_TREE (check);
}


enum cb_colseq cb_default_colseq = CB_COLSEQ_NATIVE;
enum cb_colseq cb_default_file_colseq = CB_COLSEQ_NATIVE;

/* Decipher character conversion table names */
static int
cb_deciph_colseq_name (const char * const name, enum cb_colseq *colseq)
{
	if (!cb_strcasecmp (name, "ASCII")) {
		*colseq = CB_COLSEQ_ASCII;
	} else if (!cb_strcasecmp (name, "EBCDIC")) {
		*colseq = CB_COLSEQ_EBCDIC;
	} else if (!cb_strcasecmp (name, "NATIVE")) {
		*colseq = CB_COLSEQ_NATIVE;
	} else {
		return 1;
	}
	return 0;
}

int
cb_deciph_default_colseq_name (const char * const name)
{
	return cb_deciph_colseq_name (name, &cb_default_colseq);
}

int
cb_deciph_default_file_colseq_name (const char * const name)
{
	return cb_deciph_colseq_name (name, &cb_default_file_colseq);
}

/* Use constant strings to replace string comparisons by more
 * efficient pointer comparisons */
const char *cob_prof_function_call_str = "cob_prof_function_call";

void
cb_prof_procedure_division (struct cb_program *program,
			    const char *source_file,
			    int source_line)
{
	/* invariant: program always has index 0 */
	procedure_list_add (
		program,
		COB_PROF_PROCEDURE_MODULE,
		program->orig_program_id,
		0,
		source_file,
		source_line);
}

/* Returns a tree node for a funcall to one of the profiling
 functions, with the index of the procedure as argument (and a second
 argument for the entry point if meaningful). If the program, section
 or paragraph are being entered for the first time, register them into
 the procedure_list of the program.

 To avoid lookups, the current section and current paragraph are kept
 in the program record for immediate use when exiting.
*/
cb_tree
cb_build_prof_call (enum cb_prof_call prof_call,
		    struct cb_program  *program,
		    struct cb_label  *section,
		    struct cb_label  *paragraph,
		    const char  *entry,
		    cb_tree location)
{
	const char  *func_name = cob_prof_function_call_str;
	int          func_arg1 = -1;
	int          func_arg2 = -1;

	switch (prof_call){

	case COB_PROF_ENTER_SECTION:

		/* allocate section record and remember current section */
		program->prof_current_section =
			procedure_list_add (
				program,
				COB_PROF_PROCEDURE_SECTION,
				section->name,
				/* the current section will have
				 * procedure_list_list as index */
				program->procedure_list_len,
				section->common.source_file,
				section->common.source_line);
		program->prof_current_paragraph = -1;
		func_arg1 = program->prof_current_section;
		break;

	case COB_PROF_ENTER_PARAGRAPH:

		/* allocate section record and remember current section */
		program->prof_current_paragraph =
			procedure_list_add (
				program,
				COB_PROF_PROCEDURE_PARAGRAPH,
				paragraph->name,
				program->prof_current_section,
				paragraph->common.source_file,
				paragraph->common.source_line);
		func_arg1 = program->prof_current_paragraph;
		break;

		/* In the case of an ENTRY statement, add code before
		 * to the falling-through paragraph to avoid
		 * re-registering the entry into the paragraph. */
	case COB_PROF_STAYIN_PARAGRAPH:

		func_arg1 = program->prof_current_paragraph;
		break;

	case COB_PROF_USE_PARAGRAPH_ENTRY:

		func_arg1 = program->prof_current_paragraph;
		func_arg2 =
			procedure_list_add (
				program,
				COB_PROF_PROCEDURE_ENTRY,
				entry,
				/* section field of entry is in fact its paragraph */
				program->prof_current_paragraph,
				location->source_file,
				location->source_line);
		break;

	case COB_PROF_EXIT_PARAGRAPH:

		func_arg1 = program->prof_current_paragraph;
		/* Do not reinitialize, because we may have several of these
		   EXIT_PARAGRAPH, for example at EXIT SECTION.
		   program->prof_current_paragraph = -1; */
		break;

	case COB_PROF_EXIT_SECTION:

		func_arg1 = program->prof_current_section;
		/* reset current paragraph and section */
		program->prof_current_section = -1;
		program->prof_current_paragraph = -1;
		break;

	case COB_PROF_ENTER_CALL:

		/* allocate call record and remember current call */
		program->prof_current_call =
			procedure_list_add (
				program,
				COB_PROF_PROCEDURE_CALL,
				NULL,
				program->prof_current_paragraph,
				paragraph->common.source_file,
				paragraph->common.source_line);
		func_arg1 = program->prof_current_call;
		break;

	case COB_PROF_EXIT_CALL:

		/* We need to patch the last procedure to add the callee name and loc */
		program->procedure_list_last->proc.text = cobc_main_strdup (entry);
		program->procedure_list_last->proc.file = location->source_file;
		program->procedure_list_last->proc.line = location->source_line;

		func_arg1 = program->prof_current_call;
		program->prof_current_call = -1;
		break;

	}
	if (func_arg2 < 0){
		return CB_BUILD_FUNCALL_2 (func_name, cb_int (prof_call), cb_int (func_arg1));
	}
	return CB_BUILD_FUNCALL_3 (func_name, cb_int (prof_call), cb_int (func_arg1), cb_int (func_arg2));
}

/* Allocate a procedure description record and add it at the end of
 * the procedure_list of the current program. The index of the
 * procedure will be the position in the list. There is an invariant
 * that 0 is reserved for the record of the program module. */
int
procedure_list_add (
	struct cb_program *program,
	enum cob_prof_procedure_kind kind,
	const char *text,
	int section,
	const char *file,
	int line)
{
	struct cb_procedure_list	*p;
	int ret = program->procedure_list_len ;

	p = cobc_main_malloc (sizeof (struct cb_procedure_list));
	if (text){ p->proc.text = cobc_main_strdup (text); }
	p->proc.kind = kind;
	p->proc.file = file;
	p->proc.line = line;
	p->proc.section = section;
	p->next = NULL;

	if (program->procedure_list == NULL){
		program->procedure_list = p;
	} else {
		program->procedure_list_last->next = p;
	}
	program->procedure_list_last = p;

	program->procedure_list_len++;
	return ret;
}

#ifndef	HAVE_DESIGNATED_INITS
void
cobc_init_tree (void)
{
	cb_statement_name[STMT_UNKNOWN] = "UNKNOWN";
#define COB_STATEMENT(ename,str) \
	cb_statement_name[ename] = str;
#include "../libcob/statement.def"
#undef COB_STATEMENT
}
#endif

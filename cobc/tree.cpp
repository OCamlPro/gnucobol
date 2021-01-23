/*
   Copyright (C) 2001-2012, 2014-2017 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Simon Sobisch, Sergey Kashyrin,
   Ron Norman, Edward Hart

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


#include "config.h"

#ifdef __HP_aCC
	#define _INCLUDE_STDC__SOURCE_199901
	#define atoll(str) strtoll(str,NULL,10)
#endif
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>

#include "cobc.h"
#include "tree.h"
#include "parser.hpp"

#define PIC_ALPHABETIC		0x01
#define PIC_NUMERIC		0x02
#define PIC_NATIONAL		0x04
#define PIC_EDITED		0x08
#define PIC_ALPHANUMERIC	(PIC_ALPHABETIC | PIC_NUMERIC)
#define PIC_ALPHABETIC_EDITED	(PIC_ALPHABETIC | PIC_EDITED)
#define PIC_ALPHANUMERIC_EDITED	(PIC_ALPHANUMERIC | PIC_EDITED)
#define PIC_NUMERIC_EDITED	(PIC_NUMERIC | PIC_EDITED)
#define PIC_NATIONAL_EDITED	(PIC_NATIONAL | PIC_EDITED)

/* Local variables */

static const enum cb_class category_to_class_table[] = {
	CB_CLASS_UNKNOWN,		/* CB_CATEGORY_UNKNOWN */
	CB_CLASS_ALPHABETIC,	/* CB_CATEGORY_ALPHABETIC */
	CB_CLASS_ALPHANUMERIC,	/* CB_CATEGORY_ALPHANUMERIC */
	CB_CLASS_ALPHANUMERIC,	/* CB_CATEGORY_ALPHANUMERIC_EDITED */
	CB_CLASS_BOOLEAN,		/* CB_CATEGORY_BOOLEAN */
	CB_CLASS_INDEX,			/* CB_CATEGORY_INDEX */
	CB_CLASS_NATIONAL,		/* CB_CATEGORY_NATIONAL */
	CB_CLASS_NATIONAL,		/* CB_CATEGORY_NATIONAL_EDITED */
	CB_CLASS_NUMERIC,		/* CB_CATEGORY_NUMERIC */
	CB_CLASS_ALPHANUMERIC,	/* CB_CATEGORY_NUMERIC_EDITED */
	CB_CLASS_OBJECT,		/* CB_CATEGORY_OBJECT_REFERENCE */
	CB_CLASS_POINTER,		/* CB_CATEGORY_DATA_POINTER */
	CB_CLASS_POINTER		/* CB_CATEGORY_PROGRAM_POINTER */
};

static int category_is_alphanumeric[] = {
	0,	/* CB_CATEGORY_UNKNOWN */
	1,	/* CB_CATEGORY_ALPHABETIC */
	1,	/* CB_CATEGORY_ALPHANUMERIC */
	1,	/* CB_CATEGORY_ALPHANUMERIC_EDITED */
	0,	/* CB_CATEGORY_BOOLEAN */
	0,	/* CB_CATEGORY_INDEX */
	1,	/* CB_CATEGORY_NATIONAL */
	1,	/* CB_CATEGORY_NATIONAL_EDITED */
	0,	/* CB_CATEGORY_NUMERIC */
	1,	/* CB_CATEGORY_NUMERIC_EDITED */
	0,	/* CB_CATEGORY_OBJECT_REFERENCE */
	0,	/* CB_CATEGORY_DATA_POINTER */
	0	/* CB_CATEGORY_PROGRAM_POINTER */
};

struct int_node {
	int_node	* next;
	cb_tree		node;
	int		n;
};

// Visible from outside
int filler_id = 1;

static int_node *	int_node_table = NULL;
static char 	*	scratch_buff = NULL;
static int			class_id = 0;
static int			toplev_count;
static int			after_until = 0;
static char			err_msg[COB_MINI_BUFF];
static cb_program *	container_progs[64];
static const char *	const cb_const_subs[] = {
	"_i0",
	"_i1",
	"_i2",
	"_i3",
	"_i4",
	"_i5",
	"_i6",
	"_i7",
	"_i8",
	"_i9",
	"_i10",
	"_i11",
	"_i12",
	"_i13",
	"_i14",
	"_i15",
	NULL
};

static cb_intrinsic_table userbp = {
	"USER FUNCTION", "cob_user_function",
	CB_INTR_USER_FUNCTION, USER_FUNCTION_NAME,
	CB_FEATURE_DISABLED, 0, 0, CB_CATEGORY_NUMERIC, 0
};

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
cb_tree cb_i[COB_MAX_SUBSCRIPTS];
cb_tree cb_error_node;

cb_tree cb_intr_whencomp = NULL;

cb_tree cb_standard_error_handler = NULL;

unsigned int	gen_screen_ptr = 0;

static	int	save_expr_line = 0;
static	cb_tree cb_zero_lit;
static	char	* save_expr_file = NULL;
static	int	prev_expr_line = 0;
static	int	prev_expr_pos = 0;
#define EXPR_WARN_PER_LINE 8
static	int	prev_expr_warn[EXPR_WARN_PER_LINE] = {0, 0, 0, 0, 0, 0, 0, 0};
static	int	prev_expr_tf[EXPR_WARN_PER_LINE] = {0, 0, 0, 0, 0, 0, 0, 0};

/* Local functions */

static int
was_prev_warn(int linen, int tf)
{
	int	i;
	if(cb_exp_line != prev_expr_line) {
		prev_expr_line = cb_exp_line;
		for(i = 0; i < EXPR_WARN_PER_LINE; i++) {
			prev_expr_warn[i] = 0;
			prev_expr_tf[i] = -1;
		}
	}
	for(i = 0; i < EXPR_WARN_PER_LINE; i++) {
		if(prev_expr_warn[i] == linen) {
			if(prev_expr_tf[i] == tf) {
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
copy_file_line(cb_tree e, cb_tree y, cb_tree x)
{
	if(y == cb_zero || x == cb_zero) {
		e->source_line = prev_expr_line = cb_exp_line;
		e->source_file = cb_source_file;
	} else if(y && x && y->source_line > x->source_line) {
		e->source_file = y->source_file;
		e->source_line = y->source_line;
		e->source_column = y->source_column;
		save_expr_file = (char *)y->source_file;
		save_expr_line = y->source_line;
	} else if(!x && y && y->source_line) {
		e->source_file = y->source_file;
		e->source_line = y->source_line;
		e->source_column = y->source_column;
		save_expr_file = (char *)e->source_file;
		save_expr_line = e->source_line;
	} else if(x && x->source_line) {
		e->source_file = x->source_file;
		e->source_line = x->source_line;
		e->source_column = x->source_column;
		save_expr_file = (char *)e->source_file;
		save_expr_line = e->source_line;
	} else if(y || x) {
		e->source_line = cb_exp_line;
		e->source_file = cb_source_file;
	} else if(save_expr_line) {
		e->source_file = save_expr_file;
		e->source_line = save_expr_line;
	} else {
		e->source_line = cb_exp_line;
		e->source_file = cb_source_file;
	}
}

static size_t
hash(const unsigned char * s)
{
	/* Hash a name */
	/* We multiply by position to get a better distribution */
	size_t val = 0;
	size_t pos = 1;
	for(; *s; s++, pos++) {
		val += pos * toupper(*s);
	}
#if	0	/* RXWRXW - Hash remainder */
	return val % CB_WORD_HASH_SIZE;
#endif
	return val & CB_WORD_HASH_MASK;
}

static void
lookup_word(cb_reference * p, const char * name)
{
	size_t val = hash((const unsigned char *)name);
	/* Find an existing word */
	if(likely(current_program)) {
		for(cb_word * w = current_program->word_table[val]; w; w = w->next) {
			if(strcasecmp(w->name, name) == 0) {
				p->word = w;
				p->hashval = val;
				p->flag_duped = 1;
				return;
			}
		}
	}

	/* Create new word */
	cb_word * w = (cb_word *) cobc_parse_malloc(sizeof(cb_word));
	w->name = cobc_parse_strdup(name);

	/* Insert it into the table */
	if(likely(current_program)) {
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
file_error(cb_tree name, const char * clause, const char errtype)
{
	switch(errtype) {
	case CB_FILE_ERR_REQUIRED:
		cb_error_x(name, _("%s clause is required for file '%s'"),
				   clause, CB_NAME(name));
		break;
	case CB_FILE_ERR_INVALID_FT:
		cb_error_x(name, _("%s clause is invalid for file '%s' (file type)"),
				   clause, CB_NAME(name));
		break;
	case CB_FILE_ERR_INVALID:
		cb_error_x(name, _("%s clause is invalid for file '%s'"),
				   clause, CB_NAME(name));
		break;
	}
}


static void
check_code_set_items_are_subitems_of_records(struct cb_file * const file)
{
	cb_tree			first_ref = NULL;
	cb_field	*	first_record = NULL;

	/*
	  Check each item belongs to this FD, is not a record and are all in the
	  same record.
	 */
	for(cb_list * l = file->code_set_items; l; l = CB_LIST(l->chain)) {

		cb_tree r = l->value;
		cb_field * f = CB_FIELD(cb_ref(r));

		if(f->level == 1) {
			cb_error_x(r, _("FOR item '%s' is a record"),
					   cb_name(r));
		}

		cb_field * current_record;
		for(current_record = f; current_record->parent;
				current_record = current_record->parent);

		if(first_ref) {
			if(current_record != first_record) {
				cb_error_x(r, _("FOR item '%s' is in different record to '%s'"),
						   cb_name(r), cb_name(first_ref));
			}
		} else {
			first_ref = r;
			first_record = current_record;
		}

		if(current_record->file != file) {
			cb_error_x(r, _("FOR item '%s' is not in a record associated with '%s'"),
					   cb_name(r), cb_name(file));
		}

		if(!l->chain) {
			break;
		}
	}
}

/* Tree */

static void *
make_tree(const enum cb_tag tag, const enum cb_category category,
		  const size_t size)
{
	cb_tree x = (cb_tree) cobc_parse_malloc(size);
	x->tag = tag;
	x->category = category;
	return x;
}

static cb_tree
make_constant(const enum cb_category category, const char * val)
{
	cb_const * p = (cb_const *) make_tree(CB_TAG_CONST, category, sizeof(cb_const));
	p->val = val;
	return p;
}

static cb_tree
make_constant_label(const char * name)
{
	cb_label * p = CB_LABEL(cb_build_label(cb_build_reference(name), NULL));
	p->flag_begin = 1;
	return p;
}

static size_t
cb_name_1(char * s, cb_tree x)
{
	char * orig = s;
	if(!x) {
		strcpy(s, "(void pointer)");
		return strlen(orig);
	}
	switch(CB_TREE_TAG(x)) {
	case CB_TAG_CONST:
		if(x == cb_any) {
			strcpy(s, "ANY");
		} else if(x == cb_true) {
			strcpy(s, "TRUE");
		} else if(x == cb_false) {
			strcpy(s, "FALSE");
		} else if(x == cb_null) {
			strcpy(s, "NULL");
		} else if(x == cb_zero) {
			strcpy(s, "ZERO");
		} else if(x == cb_space) {
			strcpy(s, "SPACE");
		} else if(x == cb_low || x == cb_norm_low) {
			strcpy(s, "LOW-VALUE");
		} else if(x == cb_high || x == cb_norm_high) {
			strcpy(s, "HIGH-VALUE");
		} else if(x == cb_quote) {
			strcpy(s, "QUOTE");
		} else if(x == cb_error_node) {
			strcpy(s, _("internal error node"));
		} else {
			strcpy(s, _("unknown constant"));
		}
		break;

	case CB_TAG_LITERAL:
		/* TODO: for warning/error messages (do we use this for other parts?):
		   use cb_word_length as max-length for the output */
		if(CB_TREE_CLASS(x) == CB_CLASS_NUMERIC) {
			strcpy(s, (char *)CB_LITERAL(x)->data);
		} else {
			sprintf(s, "\"%s\"", (char *)CB_LITERAL(x)->data);
		}
		break;

	case CB_TAG_FIELD: {
		cb_field * f = CB_FIELD(x);
		if(f->flag_filler) {
			strcpy(s, "FILLER");
		} else {
			strcpy(s, f->name);
		}
	}
	break;

	case CB_TAG_REFERENCE: {
		cb_reference * p = CB_REFERENCE(x);
		if(p->flag_filler_ref) {
			s += sprintf(s, "FILLER");
		} else {
			s += sprintf(s, "%s", p->word->name);
		}
		if(p->subs && CB_VALUE(p->subs) != cb_int1) {
			s += sprintf(s, " (");
			p->subs = cb_list_reverse(p->subs);
			for(cb_tree l = p->subs; l; l = CB_CHAIN(l)) {
				s += cb_name_1(s, CB_VALUE(l));
				s += sprintf(s, CB_CHAIN(l) ? ", " : ")");
			}
			p->subs = cb_list_reverse(p->subs);
		}
		if(p->offset) {
			s += sprintf(s, " (");
			s += cb_name_1(s, p->offset);
			s += sprintf(s, ":");
			if(p->length) {
				s += cb_name_1(s, p->length);
			}
			strcpy(s, ")");
		}
		if(p->chain) {
			s += sprintf(s, " in ");
			s += cb_name_1(s, p->chain);
		}
	}
	break;

	case CB_TAG_LABEL:
		sprintf(s, "%s", (char *)(CB_LABEL(x)->name));
		break;

	case CB_TAG_ALPHABET_NAME:
		sprintf(s, "%s", CB_ALPHABET_NAME(x)->name);
		break;

	case CB_TAG_CLASS_NAME:
		sprintf(s, "%s", CB_CLASS_NAME(x)->name);
		break;

	case CB_TAG_LOCALE_NAME:
		sprintf(s, "%s", CB_LOCALE_NAME(x)->name);
		break;

	case CB_TAG_BINARY_OP: {
		cb_binary_op * cbop = CB_BINARY_OP(x);
		if(cbop->op == '@') {
			s += sprintf(s, "(");
			s += cb_name_1(s, cbop->x);
			s += sprintf(s, ")");
		} else if(cbop->op == '!') {
			s += sprintf(s, "!");
			s += cb_name_1(s, cbop->x);
		} else {
			s += sprintf(s, "(");
			s += cb_name_1(s, cbop->x);
			s += sprintf(s, " %c ", cbop->op);
			s += cb_name_1(s, cbop->y);
			strcpy(s, ")");
		}
	}
	break;

	case CB_TAG_FUNCALL: {
		cb_funcall * cbip = CB_FUNCALL(x);
		s += sprintf(s, "%s", cbip->name);
		for(int i = 0; i < cbip->argc; i++) {
			s += sprintf(s, (i == 0) ? "(" : ", ");
			s += cb_name_1(s, cbip->argv[i]);
		}
		s += sprintf(s, ")");
	}
	break;

	case CB_TAG_INTRINSIC: {
		cb_intrinsic * cbit = CB_INTRINSIC(x);
		if(!cbit->isuser) {
			sprintf(s, "FUNCTION %s", cbit->intr_tab->name);
		} else if(cbit->name && CB_REFERENCE_P(cbit->name)
				  && CB_REFERENCE(cbit->name)->word) {
			sprintf(s, "USER FUNCTION %s", CB_REFERENCE(cbit->name)->word->name);
		} else {
			sprintf(s, "USER FUNCTION");
		}
	}
	break;

	case CB_TAG_FILE:
		sprintf(s, "FILE %s", CB_FILE(x)->name);
		break;

	default:
		sprintf(s, _("unexpected tree tag %d"), (int)CB_TREE_TAG(x));
	}

	return strlen(orig);
}

static cb_tree
make_intrinsic(cb_tree name, cb_intrinsic_table * cbp, cb_tree args,
			   cb_tree field, cb_tree refmod, const int isuser)
{
#if	0	/* RXWRXW Leave in, we may need this */
	cb_tree			l;
	for(l = args; l; l = CB_CHAIN(l)) {
		switch(CB_TREE_TAG(CB_VALUE(l))) {
		case CB_TAG_CONST:
		case CB_TAG_INTEGER:
		case CB_TAG_LITERAL:
		case CB_TAG_DECIMAL:
		case CB_TAG_FIELD:
		case CB_TAG_REFERENCE:
		case CB_TAG_INTRINSIC:
			break;
		default:
			cb_error(_("FUNCTION %s has invalid/not supported arguments - Tag %d"),
					 cbp->name, CB_TREE_TAG(l));
			return cb_error_node;

		}
	}
#endif

	cb_intrinsic * x = (cb_intrinsic *) make_tree(CB_TAG_INTRINSIC, cbp->category, sizeof(cb_intrinsic));
	x->name = name;
	x->args = args;
	x->intr_tab = cbp;
	x->intr_field = field;
	x->isuser = isuser;
	if(refmod) {
		x->offset = CB_PAIR_X(refmod);
		x->length = CB_PAIR_Y(refmod);
	}
	return x;
}

static cb_tree
global_check(cb_reference * r, cb_tree items, size_t * ambiguous)
{
	cb_tree candidate = NULL;
	for(; items; items = CB_CHAIN(items)) {
		/* Find a candidate value by resolving qualification */
		cb_tree v = CB_VALUE(items);
		cb_tree c = r->chain;
		if(CB_FIELD_P(v)) {
			if(!CB_FIELD(v)->flag_is_global) {
				continue;
			}
			/* In case the value is a field, it might be qualified
			   by its parent names and a file name */
			cb_field * p;
			if(CB_FIELD(v)->flag_indexed_by) {
				p = CB_FIELD(v)->index_qual;
			} else {
				p = CB_FIELD(v)->parent;
			}
			/* Resolve by parents */
			for(; p; p = p->parent) {
				if(c && strcasecmp(CB_NAME(c), p->name) == 0) {
					c = CB_REFERENCE(c)->chain;
				}
			}

			/* Resolve by file */
			if(c && CB_REFERENCE(c)->chain == NULL) {
				if(CB_WORD_COUNT(c) == 1 &&
						CB_FILE_P(cb_ref(c)) &&
						(CB_FILE(cb_ref(c)) == cb_field_founder(CB_FIELD(v))->file)) {
					c = CB_REFERENCE(c)->chain;
				}
			}
		}
		/* A well qualified value is a good candidate */
		if(c == NULL) {
			if(candidate == NULL) {
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
iso_8601_func(const enum cb_intr_enum intr)
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
valid_format(const enum cb_intr_enum intr, const char * format)
{
	char	decimal_point = current_program->decimal_point;

	/* Precondition: iso_8601_func (intr) */

	switch(intr) {
	case CB_INTR_FORMATTED_CURRENT_DATE:
		return cob_valid_datetime_format(format, decimal_point);
	case CB_INTR_FORMATTED_DATE:
		return cob_valid_date_format(format);
	case CB_INTR_FORMATTED_DATETIME:
		return cob_valid_datetime_format(format, decimal_point);
	case CB_INTR_FORMATTED_TIME:
		return cob_valid_time_format(format, decimal_point);
	case CB_INTR_INTEGER_OF_FORMATTED_DATE:
		return cob_valid_date_format(format)
			   || cob_valid_datetime_format(format, decimal_point);
	case CB_INTR_SECONDS_FROM_FORMATTED_TIME:
		return cob_valid_time_format(format, decimal_point)
			   || cob_valid_datetime_format(format, decimal_point);
	case CB_INTR_TEST_FORMATTED_DATETIME:
		return cob_valid_time_format(format, decimal_point)
			   || cob_valid_date_format(format)
			   || cob_valid_datetime_format(format, decimal_point);
	default:
		cb_error(_("invalid date/time function: '%d'"), intr);
		/* Ignore the content of the format */
		return 1;
	}
}

static const char *
try_get_constant_data(cb_tree val)
{
	if(val == NULL) {
		return NULL;
	} else if(CB_LITERAL_P(val)) {
		return (char *) CB_LITERAL(val)->data;
	} else if(CB_CONST_P(val)) {
		return CB_CONST(val)->val;
	} else {
		return NULL;
	}
}

static int
valid_const_date_time_args(const cb_tree tree, const struct cb_intrinsic_table * intr,
						   cb_tree args)
{
	cb_tree		arg = CB_VALUE(args);
	const char	* data;
	int		error_found = 0;

	/* Precondition: iso_8601_func (intr->intr_enum) */

	data = try_get_constant_data(arg);
	if(data != NULL) {
		if(!valid_format(intr->intr_enum, data)) {
			cb_error_x(tree, _("FUNCTION '%s' has invalid date/time format"),
					   intr->name);
			error_found = 1;
		}
	} else {
		cb_warning_x(COBC_WARN_FILLER, tree, _("FUNCTION '%s' has format in variable"),
					 intr->name);
	}

	return !error_found;
}

static cb_tree
get_last_elt(cb_tree l)
{
	while(CB_CHAIN(l)) {
		l = CB_CHAIN(l);
	}
	return l;
}

#if !defined(_BSD_SOURCE) && !defined (COB_STRFTIME) && !defined (HAVE_TIMEZONE)
static void
warn_cannot_get_utc(const cb_tree tree, const enum cb_intr_enum intr,
					cb_tree args)
{
	const char	* data = try_get_constant_data(CB_VALUE(args));
	int		is_variable_format = data == NULL;
	int		is_constant_utc_format
		= data != NULL && strchr(data, 'Z') != NULL;
	int		is_formatted_current_date
		= intr == CB_INTR_FORMATTED_CURRENT_DATE;
	cb_tree		last_arg = get_last_elt(args);
	int	        has_system_offset_arg
		= (intr == CB_INTR_FORMATTED_DATETIME
		   || intr == CB_INTR_FORMATTED_TIME)
		  && last_arg->tag == CB_TAG_INTEGER
		  && ((struct cb_integer *) last_arg)->val == 1;

	if(!is_formatted_current_date && !has_system_offset_arg) {
		return;
	}
	/* Fixme: this should not be an error by default as we may compile
	   for a different system */

	if(is_variable_format) {
		cb_warning_x(tree, _(COBC_WARN_FILLER, "cannot find the UTC offset on this system"));
	} else if(is_constant_utc_format) {
		cb_error_x(tree, _("cannot find the UTC offset on this system"));
	}
}
#endif

static int
get_data_from_const(cb_tree const_val, unsigned char ** data)
{
	if(const_val == cb_space) {
		*data = (unsigned char *)" ";
	} else if(const_val == cb_zero) {
		*data = (unsigned char *)"0";
	} else if(const_val == cb_quote) {
		if(cb_flag_apostrophe) {
			*data = (unsigned char *)"'";
		} else {
			*data = (unsigned char *)"\"";
		}
	} else if(const_val == cb_norm_low) {
		*data = (unsigned char *)"\0";
	} else if(const_val == cb_norm_high) {
		*data = (unsigned char *)"\255";
	} else if(const_val == cb_null) {
		*data = (unsigned char *)"\0";
	} else {
		return 1;
	}

	return 0;
}

static int
get_data_and_size_from_lit(cb_tree x, unsigned char ** data, size_t * size)
{
	if(CB_LITERAL_P(x)) {
		*data = CB_LITERAL(x)->data;
		*size = CB_LITERAL(x)->size;
	} else if(CB_CONST_P(x)) {
		*size = 1;
		if(get_data_from_const(x, data)) {
			return 1;
		}
	} else {
		return 1;
	}

	return 0;
}

static struct cb_literal *
concat_literals(const cb_tree left, const cb_tree right)
{
	unsigned char * ldata;
	unsigned char * rdata;
	size_t lsize;
	size_t rsize;

	if(get_data_and_size_from_lit(left, &ldata, &lsize)) {
		return NULL;
	}
	if(get_data_and_size_from_lit(right, &rdata, &rsize)) {
		return NULL;
	}

	cb_literal * p = (cb_literal *) make_tree(CB_TAG_LITERAL, left->category, sizeof(struct cb_literal));
	p->data = (unsigned char *) cobc_parse_malloc(lsize + rsize + 1U);
	p->size = (unsigned int)(lsize + rsize);

	memcpy(p->data, ldata, lsize);
	memcpy(p->data + lsize, rdata, rsize);

	return p;
}

/* Global functions */

char *
cb_to_cname(const char * s)
{
	char * copy = cobc_parse_strdup(s);
	for(unsigned char * p = (unsigned char *)copy; *p; p++) {
		if(*p == '-' || *p == ' ') {
			*p = '_';
		} else {
			*p = (cob_u8_t)toupper(*p);
		}
	}
	return copy;
}

cb_literal *
build_literal(const enum cb_category category, const void * data,
			  const size_t size)
{
	cb_literal * p = (cb_literal *) make_tree(CB_TAG_LITERAL, category, sizeof(cb_literal));
	p->data = (unsigned char *) cobc_parse_malloc(size + 1U);
	p->size = (unsigned int) size;
	memcpy(p->data, data, size);
	return p;
}

char *
cb_name(cb_tree x)
{
	char tmp[COB_NORMAL_BUFF] = { 0 };
	int tlen = (int) cb_name_1(tmp, x);

	char * s = (char *) cobc_parse_malloc(tlen + 1);
	strncpy(s, tmp, tlen);
	return s;
}

enum cb_category
cb_tree_category(cb_tree x) {
	if(x == cb_error_node)
	{
		return(enum cb_category)0;
	}
	if(x->category != CB_CATEGORY_UNKNOWN)
	{
		return x->category;
	}

	switch(CB_TREE_TAG(x))
	{
	case CB_TAG_CAST: {
		cb_cast * p = CB_CAST(x);
		switch(p->cast_type) {
		case CB_CAST_ADDRESS:
		case CB_CAST_ADDR_OF_ADDR:
			x->category = CB_CATEGORY_DATA_POINTER;
			break;
		case CB_CAST_PROGRAM_POINTER:
			x->category = CB_CATEGORY_PROGRAM_POINTER;
			break;
		default:
			/* LCOV_EXCL_START */
			cobc_err_msg(_("unexpected cast type: %d"),
			(int)(p->cast_type));
			COBC_ABORT();
			/* LCOV_EXCL_STOP */
		}
	}
	break;
	case CB_TAG_REFERENCE: {
		cb_reference * r = CB_REFERENCE(x);
		if(r->offset) {
			x->category = CB_CATEGORY_ALPHANUMERIC;
		} else {
			x->category = cb_tree_category(r->value);
		}
	}
	break;
	case CB_TAG_FIELD: {
		cb_field * f = CB_FIELD(x);
		if(f->children) {
			x->category = CB_CATEGORY_ALPHANUMERIC;
		} else if(f->usage == CB_USAGE_POINTER && f->level != 88) {
			x->category = CB_CATEGORY_DATA_POINTER;
		} else if(f->usage == CB_USAGE_PROGRAM_POINTER && f->level != 88) {
			x->category = CB_CATEGORY_PROGRAM_POINTER;
		} else {
			switch(f->level) {
			case 66:
				if(f->rename_thru) {
					x->category = CB_CATEGORY_ALPHANUMERIC;
				} else {
					x->category = cb_tree_category(f->redefines);
				}
				break;
			case 88:
				x->category = CB_CATEGORY_BOOLEAN;
				break;
			default:
				if(f->pic) {
					x->category = f->pic->category;
					/* FIXME: Hack for CGI to not abort */
				} else if(f->flag_is_external_form) {
					x->category = CB_CATEGORY_ALPHANUMERIC;
				} else {
					x->category = CB_CATEGORY_UNKNOWN;
				}
				break;
			}
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
		cobc_err_msg(_("unknown tree tag: %d, category: %d"),
					 (int)CB_TREE_TAG(x), (int)x->category);
		COBC_ABORT();
#endif
		return CB_CATEGORY_UNKNOWN;
	}

	return x->category;
}

cb_class
cb_tree_class(cb_tree x)
{
	return category_to_class_table[CB_TREE_CATEGORY(x)];
}

int
cb_category_is_alpha(cb_tree x)
{
	return category_is_alphanumeric[CB_TREE_CATEGORY(x)];
}

int
cb_tree_type(const cb_tree x, const cb_field * f)
{
	if(f->children) {
		return COB_TYPE_GROUP;
	}

	switch(CB_TREE_CATEGORY(x)) {
	case CB_CATEGORY_ALPHABETIC:
	case CB_CATEGORY_ALPHANUMERIC:
		return COB_TYPE_ALPHANUMERIC;
	case CB_CATEGORY_ALPHANUMERIC_EDITED:
		return COB_TYPE_ALPHANUMERIC_EDITED;
	case CB_CATEGORY_NUMERIC:
		switch(f->usage) {
		case CB_USAGE_DISPLAY:
			return COB_TYPE_NUMERIC_DISPLAY;
		case CB_USAGE_BINARY:
		case CB_USAGE_COMP_5:
		case CB_USAGE_COMP_X:
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
		case CB_USAGE_PACKED:
		case CB_USAGE_COMP_6:
			return COB_TYPE_NUMERIC_PACKED;
		case CB_USAGE_LONG_DOUBLE:
			return COB_TYPE_NUMERIC_L_DOUBLE;
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
		default:
			/* LCOV_EXCL_START */
			cobc_err_msg(_("unexpected numeric USAGE: %d"),
						 (int)f->usage);
			COBC_ABORT();
			/* LCOV_EXCL_STOP */
		}
	case CB_CATEGORY_NUMERIC_EDITED:
		return COB_TYPE_NUMERIC_EDITED;
	case CB_CATEGORY_OBJECT_REFERENCE:
	case CB_CATEGORY_DATA_POINTER:
	case CB_CATEGORY_PROGRAM_POINTER:
		return COB_TYPE_NUMERIC_BINARY;
	default:
		/* LCOV_EXCL_START */
		cobc_err_msg(_("unexpected category: %d"),
					 (int)CB_TREE_CATEGORY(x));
		COBC_ABORT();
		/* LCOV_EXCL_STOP */
	}
	/* NOT REACHED */
#ifndef _MSC_VER
	return 0;	/* LCOV_EXCL_LINE */
#endif
}

int
cb_fits_int(const cb_tree x)
{
	switch(CB_TREE_TAG(x)) {
	case CB_TAG_LITERAL: {
		cb_literal * l = CB_LITERAL(x);
		if(l->scale > 0) {
			return 0;
		}
		size_t size = 0;
		const unsigned char * p = l->data;
		for(; size < l->size; ++size, ++p) {
			if(*p != (unsigned char)'0') {
				break;
			}
		}
		size = l->size - size;
		if(size < 10) {
			return 1;
		}
		if(size > 10) {
			return 0;
		}
		const char * s;
		if(l->sign < 0) {
			s = "2147483648";
		} else {
			s = "2147483647";
		}
		if(memcmp(p, s, (size_t)10) > 0) {
			return 0;
		}
		return 1;
	}
	case CB_TAG_FIELD: {
		cb_field * f = CB_FIELD(x);
		if(f->children) {
			return 0;
		}
		switch(f->usage) {
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
			if(f->pic->scale <= 0 && f->size <= (int)sizeof(int)) {
				return 1;
			}
			return 0;
		case CB_USAGE_DISPLAY:
			if(f->size < 10) {
				if(!f->pic || f->pic->scale <= 0) {
					return 1;
				}
			}
			return 0;
		case CB_USAGE_PACKED:
		case CB_USAGE_COMP_6:
			if(f->pic->scale <= 0 && f->pic->digits < 10) {
				return 1;
			}
			return 0;
		default:
			return 0;
		}
	}
	case CB_TAG_REFERENCE:
		return cb_fits_int(CB_REFERENCE(x)->value);
	case CB_TAG_INTEGER:
		return 1;
	default:
		return 0;
	}
}

int
cb_fits_long_long(const cb_tree x)
{
	switch(CB_TREE_TAG(x)) {
	case CB_TAG_LITERAL: {
		cb_literal * l = CB_LITERAL(x);
		if(l->scale > 0) {
			return 0;
		}
		size_t size = 0;
		const unsigned char * p = l->data;
		for(; size < l->size; ++size, ++p) {
			if(*p != (unsigned char)'0') {
				break;
			}
		}
		size = l->size - size;
		if(size < 19) {
			return 1;
		}
		if(size > 19) {
			return 0;
		}
		const char * s;
		if(l->sign < 0) {
			s = "9223372036854775808";
		} else {
			s = "9223372036854775807";
		}
		if(memcmp(p, s, (size_t)19) > 0) {
			return 0;
		}
		return 1;
	}
	case CB_TAG_FIELD: {
		cb_field * f;
		f = CB_FIELD(x);
		if(f->children) {
			return 0;
		}
		switch(f->usage) {
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
			if(f->pic->scale <= 0 &&
					f->size <= (int)sizeof(cob_s64_t)) {
				return 1;
			}
			return 0;
		case CB_USAGE_DISPLAY:
			if(f->pic->scale <= 0 && f->size < 19) {
				return 1;
			}
			return 0;
		case CB_USAGE_PACKED:
		case CB_USAGE_COMP_6:
			if(f->pic->scale <= 0 && f->pic->digits < 19) {
				return 1;
			}
			return 0;
		default:
			return 0;
		}
	}
	case CB_TAG_REFERENCE:
		return cb_fits_long_long(CB_REFERENCE(x)->value);
	case CB_TAG_INTEGER:
		return 1;
	default:
		return 0;
	}
}

static void
error_numeric_literal(const char * literal)
{
	char		lit_out[39];

	/* snip literal for output, if too long */
	strncpy(lit_out, literal, 38);
	if(strlen(literal) > 38) {
		strcpy(lit_out + 35, "...");
	} else {
		lit_out[38] = '\0';
	}
	cb_error(_("invalid numeric literal: '%s'"), lit_out);
	cb_error("%s", err_msg);
}

/* Check numeric literal length, postponed from scanner.l (scan_numeric) */
static void
check_lit_length(const unsigned int size, const char * lit)
{
	if(unlikely(size > COB_MAX_DIGITS)) {
		/* Absolute limit */
		snprintf(err_msg, COB_MINI_MAX,
				 _("literal length %d exceeds maximum of %d digits"),
				 size, COB_MAX_DIGITS);
		error_numeric_literal(lit);
	} else if(unlikely(size > cb_numlit_length)) {
		snprintf(err_msg, COB_MINI_MAX,
				 _("literal length %d exceeds %d digits"),
				 size, cb_numlit_length);
		error_numeric_literal(lit);
	}
}

int
cb_get_int(const cb_tree x)
{
	if(!CB_LITERAL_P(x)) {
		/* LCOV_EXCL_START */
		cobc_err_msg(_("invalid literal cast"));
		COBC_ABORT();
		/* LCOV_EXCL_STOP */
	}
	cb_literal * l = CB_LITERAL(x);
	/* Skip leading zeroes */
	size_t i;
	for(i = 0; i < l->size; i++) {
		if(l->data[i] != '0') {
			break;
		}
	}

	size_t size = l->size - i;
	const char * s;

	/* Check numeric literal length, postponed from scanner.l (scan_numeric) */
	check_lit_length((int) size, (const char *)l->data + i);
	/* Check numeric literal length matching requested output type */
#if INT_MAX >= 9223372036854775807
	if(unlikely(size >= 19U)) {
		if(l->sign < 0) {
			s = "9223372036854775808";
		} else {
			s = "9223372036854775807";
		}
		if(size > 19U || memcmp(&l->data[i], s, (size_t)19) > 0) {
			cb_error(_("numeric literal '%s' exceeds limit '%s'"), &l->data[i], s);
			return INT_MAX;
		}
	}
#elif INT_MAX >= 2147483647
	if(unlikely(size >= 10U)) {
		if(l->sign < 0) {
			s = "2147483648";
		} else {
			s = "2147483647";
		}
		if(size > 10U || memcmp(&l->data[i], s, (size_t)10) > 0) {
			cb_error(_("numeric literal '%s' exceeds limit '%s'"), &l->data[i], s);
			return INT_MAX;
		}
	}
#else
	if(unlikely(size >= 5U)) {
		if(l->sign < 0) {
			s = "32768";
		} else {
			s = "32767";
		}
		if(size == 5U || memcmp(&l->data[i], s, (size_t)5) > 0) {
			cb_error(_("numeric literal '%s' exceeds limit '%s'"), &l->data[i], s);
			return INT_MAX;
		}
	}
#endif

	int val = 0;
	for(; i < l->size; i++) {
		val = val * 10 + l->data[i] - '0';
	}
	if(val && l->sign < 0) {
		val = -val;
	}
	return val;
}

cob_s64_t
cb_get_long_long(const cb_tree x)
{
	if(!CB_LITERAL_P(x)) {
		/* LCOV_EXCL_START */
		cobc_err_msg(_("invalid literal cast"));
		COBC_ABORT();
		/* LCOV_EXCL_STOP */
	}
	cb_literal * l = CB_LITERAL(x);
	/* Skip leading zeroes */
	size_t i;
	for(i = 0; i < l->size; i++) {
		if(l->data[i] != '0') {
			break;
		}
	}

	size_t size = l->size - i;
	/* Check numeric literal length, postponed from scanner.l (scan_numeric) */
	check_lit_length((int) size, (const char *)l->data + i);
	/* Check numeric literal length matching requested output type */
	if(unlikely(size >= 19U)) {
		char * s;
		if(l->sign < 0) {
			s = "9223372036854775808";
		} else {
			s = "9223372036854775807";
		}
		if(size == 19U || memcmp(&(l->data[i]), s, (size_t)19) > 0) {
			cb_error(_("numeric literal '%s' exceeds limit '%s'"), &l->data[i], s);
			return LLONG_MAX;
		}
	}

	cob_s64_t val = 0;
	for(; i < l->size; i++) {
		val = val * 10 + (l->data[i] & 0x0F);
	}
	if(val && l->sign < 0) {
		val = -val;
	}
	return val;
}

cob_u64_t
cb_get_u_long_long(const cb_tree x)
{
	cb_literal * l = CB_LITERAL(x);
	/* Skip leading zeroes */
	size_t i;
	for(i = 0; i < l->size; i++) {
		if(l->data[i] != '0') {
			break;
		}
	}

	size_t size = l->size - i;
	/* Check numeric literal length, postponed from scanner.l (scan_numeric) */
	check_lit_length((int) size, (const char *)l->data + i);
	/* Check numeric literal length matching requested output type */
	if(unlikely(size >= 20U)) {
		char * s = "18446744073709551615";
		if(size == 20U || memcmp(&(l->data[i]), s, (size_t)20) > 0) {
			cb_error(_("numeric literal '%s' exceeds limit '%s'"), &l->data[i], s);
			return ULLONG_MAX;
		}
	}
	cob_u64_t val = 0;
	for(; i < l->size; i++) {
		val = val * 10 + (l->data[i] & 0x0F);
	}
	return val;
}

void
cb_init_constants(void)
{
	cb_error_node = make_constant(CB_CATEGORY_UNKNOWN, NULL);
	cb_any = make_constant(CB_CATEGORY_UNKNOWN, NULL);
	cb_true = make_constant(CB_CATEGORY_BOOLEAN, "1");
	cb_false = make_constant(CB_CATEGORY_BOOLEAN, "0");
	cb_null = make_constant(CB_CATEGORY_DATA_POINTER, "0");
	cb_zero = make_constant(CB_CATEGORY_NUMERIC, "&cob_all_zero");
	cb_space = make_constant(CB_CATEGORY_ALPHANUMERIC, "&cob_all_space");
	cb_low = make_constant(CB_CATEGORY_ALPHANUMERIC, "&cob_all_low");
	cb_norm_low = cb_low;
	cb_high = make_constant(CB_CATEGORY_ALPHANUMERIC, "&cob_all_high");
	cb_norm_high = cb_high;
	cb_quote = make_constant(CB_CATEGORY_ALPHANUMERIC, "&cob_all_quote");
	cb_one = cb_build_numeric_literal(0, "1", 0);
	cb_zero_lit = cb_build_numeric_literal(0, "0", 0);
	cb_int0 = cb_int(0);
	cb_int1 = cb_int(1);
	cb_int2 = cb_int(2);
	cb_int3 = cb_int(3);
	cb_int4 = cb_int(4);
	cb_int5 = cb_int(5);
	cb_int6 = cb_int(6);
	for(int i = 0; i < COB_MAX_SUBSCRIPTS; i++) {
		cb_i[i] = make_constant(CB_CATEGORY_NUMERIC, cb_const_subs[i]);
	}
	cb_standard_error_handler = make_constant_label("Default Error Handler");
	CB_LABEL(cb_standard_error_handler)->flag_default_handler = 1;
	memset(container_progs, 0, sizeof(container_progs));
}

/* List */

cb_tree
cb_build_list(cb_tree purpose, cb_tree value, cb_tree chain)
{
	cb_list * p = (cb_list *) make_tree(CB_TAG_LIST, CB_CATEGORY_UNKNOWN, sizeof(cb_list));
	p->chain = chain;
	p->value = value;
	p->purpose = purpose;

	/* Set location to that of initial element. */
	if(value) {
		p->source_file = value->source_file;
		p->source_line = value->source_line;
		p->source_column = value->source_column;
	}

	return p;
}

cb_tree
cb_list_append(cb_tree l1, cb_tree l2)
{
	if(l1 == NULL) {
		return l2;
	}
	CB_CHAIN(get_last_elt(l1)) = l2;
	return l1;
}

cb_tree
cb_list_add(cb_tree l, cb_tree x)
{
	return cb_list_append(l, CB_LIST_INIT(x));
}

cb_tree
cb_pair_add(cb_tree l, cb_tree x, cb_tree y)
{
	return cb_list_append(l, CB_BUILD_PAIR(x, y));
}

cb_tree
cb_list_reverse(cb_tree l)
{
	cb_tree last = NULL;
	cb_tree next;
	for(; l; l = next) {
		next = CB_CHAIN(l);
		CB_CHAIN(l) = last;
		last = l;
	}
	return last;
}

unsigned int
cb_list_length(cb_tree l)
{
	if(l == cb_error_node) {
		return 0;
	}
	unsigned int n = 0;
	for(; l; l = CB_CHAIN(l)) {
		n++;
	}
	return n;
}

void
cb_list_map(cb_tree(*func)(cb_tree x), cb_tree l)
{
	for(; l; l = CB_CHAIN(l)) {
		CB_VALUE(l) = func(CB_VALUE(l));
	}
}

/* Link value into the reference */

const char *
cb_define(cb_tree name, cb_tree val)
{
	cb_word * w = CB_REFERENCE(name)->word;
	w->items = cb_list_add(w->items, val);
	w->count++;
	val->source_file = name->source_file;
	val->source_line = name->source_line;
	CB_REFERENCE(name)->value = val;
	return w->name;
}

/* Program */

static nested_list *
add_contained_prog(nested_list * parent_list, cb_program * child_prog)
{
	/* Check for reuse */
	nested_list * nlp;
	for(nlp = parent_list; nlp; nlp = nlp->next) {
		if(nlp->nested_prog == child_prog) {
			return parent_list;
		}
	}
	nlp = (nested_list *) cobc_parse_malloc(sizeof(nested_list));
	nlp->next = parent_list;
	nlp->nested_prog = child_prog;
	return nlp;
}

cb_program *
cb_build_program(cb_program * last_program, const int nest_level)
{
	if(!last_program) {
		toplev_count = 0;
	}
	cb_reset_78();
	cobc_in_procedure = 0;
	cobc_in_repository = 0;
	cb_clear_real_field();

	cb_program * p = (cb_program *) cobc_parse_malloc(sizeof(cb_program));
	memset(p, 0, sizeof(cb_program));
	p->word_table = (cb_word **) cobc_parse_malloc(CB_WORD_TABLE_SIZE);

	p->tag = CB_TAG_PROGRAM;
	p->category = CB_CATEGORY_UNKNOWN;

	p->next_program = last_program;
	p->nested_level = nest_level;
	p->decimal_point = '.';
	p->currency_symbol = '$';
	p->numeric_separator = ',';
	/* Save current program as actual at it's level */
	container_progs[nest_level] = p;
	if(nest_level && last_program /* <- silence warnings */) {
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
		if(last_program->cb_return_code) {
			p->cb_return_code = last_program->cb_return_code;
			CB_FIELD_PTR(last_program->cb_return_code)->flag_is_global = 1;
		}
		p->toplev_count = last_program->toplev_count;
		/* Add program to itself for possible recursion */
		p->nested_prog_list = add_contained_prog(p->nested_prog_list, p);
		/* Add contained program to it's parent */
		cb_program * q = container_progs[nest_level - 1];
		q->nested_prog_list = add_contained_prog(q->nested_prog_list, p);
	} else {
		/* Top level program */
		p->toplev_count = toplev_count++;
		functions_are_all = cb_flag_functions_all;
		cb_reset_global_78();
		/* Recursive check disabled? Then handle all programs as recursive */
		if(!cb_flag_recursive_check) {
			p->flag_recursive = 1;
		}
	}
	return p;
}

void
cb_add_common_prog(cb_program * prog)
{
	/* Here we are sure that nested >= 1 */
	cb_program * q = container_progs[prog->nested_level - 1];
	q->common_prog_list = add_contained_prog(q->common_prog_list, prog);
}

void
cb_insert_common_prog(cb_program * prog, cb_program * comprog)
{
	prog->nested_prog_list = add_contained_prog(prog->nested_prog_list,
							 comprog);
}

/* Integer */

cb_tree
cb_int(const int n)
{
	for(int_node * p = int_node_table; p; p = p->next) {
		if(p->n == n) {
			return p->node;
		}
	}

	/* Do not use make_tree here as we want a main_malloc
	   instead of parse_malloc! */
	cb_integer * x = (cb_integer *) cobc_main_malloc(sizeof(cb_integer));
	x->tag = CB_TAG_INTEGER;
	x->category = CB_CATEGORY_NUMERIC;
	x->val = n;

	int_node * p = (int_node *) cobc_main_malloc(sizeof(int_node));
	p->n = n;
	p->node = x;
	p->next = int_node_table;
	int_node_table = p;
	return x;
}

cb_tree
cb_int_hex(const int n)
{
	cb_tree x = cb_int(n);
	CB_INTEGER(x)->hexval = 1;
	return x;
}

/* String */

cb_tree
cb_build_string(const void * data, const size_t size)
{
	cb_string * p = (cb_string *) make_tree(CB_TAG_STRING, CB_CATEGORY_ALPHANUMERIC, sizeof(cb_string));
	p->size = size;
	p->data = (const unsigned char *) data;
	return p;
}

/* Flags */

cb_tree
cb_flags_t(const cob_flags_t n)
{
	/* FIXME:
	   This ONLY works for the current version as we have one bit left before
	   we actually need the 64bit cob_flags_t that we use internally
	   in cobc (needed already for syntax checks) and in screenio
	   (needed soon, but not yet, hence the bitmask).

	   Ideally we either store the flags as string here or mark them and
	   output the flags in codegen as flags, making the code much more readable.
	*/
	return cb_int((const int)n);
}

/* Code output and comment */

cb_tree
cb_build_comment(const char * str)
{
	cb_direct * p = (cb_direct *) make_tree(CB_TAG_DIRECT, CB_CATEGORY_ALPHANUMERIC, sizeof(cb_direct));
	p->line = str;
	p->source_file = cb_source_file;
	p->source_line = cb_source_line;
	return p;
}

cb_tree
cb_build_direct(const char * str, const unsigned int flagnl)
{
	cb_tree x = cb_build_comment(str);
	CB_DIRECT(x)->flag_is_direct = 1;
	CB_DIRECT(x)->flag_new_line = flagnl;
	return x;
}

/* DEBUG */

cb_tree
cb_build_debug(const cb_tree target, const char * str, const cb_tree fld)
{
	cb_debug * p = (cb_debug *) make_tree(CB_TAG_DEBUG, CB_CATEGORY_ALPHANUMERIC, sizeof(cb_debug));
	p->target = target;
	if(str) {
		p->value = cobc_parse_strdup(str);
		p->fld = NULL;
		p->size = strlen(str);
	} else {
		p->value = NULL;
		p->fld = fld;
		p->size = (size_t)CB_FIELD_PTR(fld)->size;
	}
	p->source_file = cb_source_file;
	p->source_line = cb_source_line;
	return p;
}

/* DEBUG Callback */

cb_tree
cb_build_debug_call(cb_label * target)
{
	cb_debug_call * p = (cb_debug_call *) make_tree(CB_TAG_DEBUG_CALL, CB_CATEGORY_ALPHANUMERIC, sizeof(cb_debug_call));
	p->target = target;
	p->source_file = cb_source_file;
	p->source_line = cb_source_line;
	return p;
}

/* Alphabet-name */

cb_tree
cb_build_alphabet_name(cb_tree name)
{
	if(!name || name == cb_error_node) {
		return NULL;
	}
	cb_alphabet_name * p = (cb_alphabet_name *) make_tree(CB_TAG_ALPHABET_NAME, CB_CATEGORY_UNKNOWN, sizeof(cb_alphabet_name));
	p->name = cb_define(name, p);
	p->cname = cb_to_cname(p->name);
	return p;
}

/* Class-name */

cb_tree
cb_build_class_name(cb_tree name, cb_tree list)
{
	if(!name || name == cb_error_node) {
		return NULL;
	}
	cb_class_name * p = (cb_class_name *) make_tree(CB_TAG_CLASS_NAME, CB_CATEGORY_BOOLEAN, sizeof(cb_class_name));
	p->name = cb_define(name, p);
	if(!scratch_buff) {
		scratch_buff = (char *) cobc_main_malloc((size_t)COB_MINI_BUFF);
	}
	snprintf(scratch_buff, (size_t)COB_MINI_MAX, "cob_is_%s_%d",
			 cb_to_cname(p->name), class_id++);
	p->cname = cobc_parse_strdup(scratch_buff);
	p->list = list;
	return p;
}

/* Locale-name */

cb_tree
cb_build_locale_name(cb_tree name, cb_tree list)
{
	if(!name || name == cb_error_node) {
		return NULL;
	}
	if(!CB_LITERAL_P(list) || CB_NUMERIC_LITERAL_P(list)) {
		cb_error(_("invalid LOCALE literal"));
		return cb_error_node;
	}
	cb_class_name * p = (cb_class_name *) make_tree(CB_TAG_LOCALE_NAME, CB_CATEGORY_UNKNOWN, sizeof(cb_locale_name));
	p->name = cb_define(name, p);
	p->cname = cb_to_cname(p->name);
	p->list = list;
	return p;
}

/* System-name */

cb_tree
cb_build_system_name(const enum cb_system_name_category category, const int token)
{
	cb_system_name * p = (cb_system_name *) make_tree(CB_TAG_SYSTEM_NAME, CB_CATEGORY_UNKNOWN, sizeof(cb_system_name));
	p->category = category;
	p->token = token;
	return p;
}

/* Literal */

cb_tree
cb_build_numeric_literal(int sign, const void * data, const int scale)
{
	/* using an intermediate char pointer for pointer arithmetic */
	const char	* data_chr_ptr = (const char *) data;

	if(*data_chr_ptr == '-') {
		sign = -1;
		data_chr_ptr++;
	} else if(*data_chr_ptr == '+') {
		sign = 1;
		data_chr_ptr++;
	}
	data = data_chr_ptr;
	cb_literal * p = (cb_literal *) build_literal(CB_CATEGORY_NUMERIC, data, strlen((const char *) data));
	p->sign = (short)sign;
	p->scale = scale;
	p->source_file = cb_source_file;
	p->source_line = cb_source_line;
	return p;
}

cb_tree
cb_build_numsize_literal(const void * data, const size_t size, const int sign)
{
	cb_literal * p = build_literal(CB_CATEGORY_NUMERIC, data, size);
	p->sign = (short)sign;
	p->source_file = cb_source_file;
	p->source_line = cb_source_line;
	return p;
}

cb_tree
cb_build_alphanumeric_literal(const void * data, const size_t size)
{
	cb_tree l = build_literal(CB_CATEGORY_ALPHANUMERIC, data, size);
	l->source_file = cb_source_file;
	l->source_line = cb_source_line;
	return l;
}

cb_tree
cb_build_national_literal(const void * data, const size_t size)
{
	cb_tree l = build_literal(CB_CATEGORY_NATIONAL, data, size);
	l->source_file = cb_source_file;
	l->source_line = cb_source_line;
	return l;
}

cb_tree
cb_concat_literals(const cb_tree x1, const cb_tree x2)
{
	if(x1 == cb_error_node || x2 == cb_error_node) {
		return cb_error_node;
	}

	if((x1->category != x2->category)) {
		cb_error_x(x1, _("only literals with the same category can be concatenated"));
		return cb_error_node;
	}

	if((x1->category != CB_CATEGORY_ALPHANUMERIC) &&
			(x1->category != CB_CATEGORY_NATIONAL) &&
			(x1->category != CB_CATEGORY_BOOLEAN)) {
		cb_error_x(x1, _("only alpanumeric, national or boolean literals may be concatenated"));
		return cb_error_node;
	}

	cb_literal * p = concat_literals(x1, x2);
	if(p == NULL) {
		return cb_error_node;
	}
	if(p->size > cb_lit_length) {
		/* shorten literal for output */
		char lit_out[39];
		strncpy(lit_out, (char *)p->data, 38);
		strcpy(lit_out + 35, "...");
		cb_error_x(x1, _("invalid literal: '%s'"), lit_out);
		cb_error_x(x1, _("literal length %d exceeds %d characters"),
				   p->size, cb_lit_length);
		return cb_error_node;
	}

	p->source_file = x1->source_file;
	p->source_line = x1->source_line;
	return p;
}

/* Decimal */

cb_tree
cb_build_decimal(const unsigned int id)
{
	cb_decimal * p = (cb_decimal *) make_tree(CB_TAG_DECIMAL, CB_CATEGORY_NUMERIC, sizeof(cb_decimal));
	p->id = id;
	return p;
}

/* Decimal Literal */

cb_tree
cb_build_decimal_literal(const int id)
{
	cb_decimal * p = (cb_decimal *) make_tree(CB_TAG_DECIMAL_LITERAL, CB_CATEGORY_NUMERIC, sizeof(cb_decimal));
	p->id = id;
	return p;
}

/* Picture */

cb_picture *
cb_build_binary_picture(const char * str, const cob_u32_t size,
						const cob_u32_t sign)
{
	cb_picture * pic = (cb_picture *) make_tree(CB_TAG_PICTURE, CB_CATEGORY_NUMERIC, sizeof(cb_picture));
	pic->orig = (char *) cobc_check_string(str);
	pic->size = size;
	pic->digits = size;
	pic->scale = 0;
	pic->have_sign = sign;
	pic->category = CB_CATEGORY_NUMERIC;
	return pic;
}

static COB_INLINE COB_A_INLINE int
is_simple_insertion_char(const char c)
{
	return c == 'B' || c == '0' || c == '/'
		   || c == current_program->numeric_separator;
}

/*
  Returns the first and last characters of a floating insertion string.

  A floating insertion string is made up of two adjacent +'s, -'s or currency
  symbols to each other, optionally with simple insertion characters between them.
*/
static void
find_floating_insertion_str(const cob_pic_symbol * str,
							const cob_pic_symbol ** first,
							const cob_pic_symbol ** last)
{
	const cob_pic_symbol	* last_non_simple_insertion = NULL;
	char			floating_char = ' ';

	*first = NULL;
	*last = NULL;

	for(; str->symbol != '\0'; ++str) {
		if(!*first && (str->symbol == '+' || str->symbol == '-'
					   || str->symbol == current_program->currency_symbol)) {
			if(last_non_simple_insertion
					&& last_non_simple_insertion->symbol == str->symbol) {
				*first = last_non_simple_insertion;
				floating_char = str->symbol;
				continue;
			} else if(str->times_repeated > 1) {
				*first = str;
				floating_char = str->symbol;
				continue;
			}
		}


		if(!*first && !is_simple_insertion_char(str->symbol)) {
			last_non_simple_insertion = str;
		} else if(*first && !(is_simple_insertion_char(str->symbol)
							  || str->symbol == floating_char)) {
			*last = str - 1;
			break;
		}
	}

	if(str->symbol == '\0' && *first) {
		*last = str - 1;
		return;
	} else if(!(str->symbol == current_program->decimal_point
				|| str->symbol == 'V')) {
		return;
	}

	/*
	  Check whether all digits after the decimal point are also part of the
	  floating insertion string. If they are, set *last to the last
	  character in the string.
	*/
	++str;
	for(; str->symbol != '\0'; ++str) {
		if(!(is_simple_insertion_char(str->symbol)
				|| str->symbol == floating_char)) {
			return;
		}
	}
	*last = str - 1;
}

static int
char_to_precedence_idx(const cob_pic_symbol * str,
					   const cob_pic_symbol * current_sym,
					   const cob_pic_symbol * first_floating_sym,
					   const cob_pic_symbol * last_floating_sym,
					   const int before_decimal_point,
					   const int non_p_digits_seen)
{
	const int	first_sym = str == current_sym;
	const int	second_sym = str + 1 == current_sym;
	const int	last_sym = (current_sym + 1)->symbol == '\0';
	const int	penultimate_sym
		= !last_sym && (current_sym + 2)->symbol == '\0';

	switch(current_sym->symbol) {
	case 'B':
	case '0':
	case '/':
		return 0;

	case '.':
	case ',':
		if(current_sym->symbol == current_program->decimal_point) {
			return 2;
		} else {
			return 1;
		}

	/* To-do: Allow floating-point PICTURE strings */
	/* case '+': */
	/* Exponent symbol */
	/* return 3; */

	case '+':
	case '-':
		if(!(first_floating_sym <= current_sym
				&& current_sym <= last_floating_sym)) {
			if(first_sym) {
				return 4;
			} else if(last_sym) {
				return 5;
			} else {
				/* Fudge char type - will still result in error */
				return 4;
			}
		} else {
			if(before_decimal_point) {
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
		if(before_decimal_point) {
			return 9;
		} else {
			return 10;
		}

	case '9':
		return 15;

	case 'A':
	case 'X':
		return 16;

	case 'S':
		return 17;

	case 'V':
		return 18;

	case 'P':
		if(non_p_digits_seen && before_decimal_point) {
			return 19;
		} else {
			return 20;
		}

	case '1':
		return 21;

	case 'N':
		return 22;

	case 'E':
		return 23;

	default:
		if(current_sym->symbol == current_program->currency_symbol) {
			if(!(first_floating_sym <= current_sym
					&& current_sym <= last_floating_sym)) {
				if(first_sym || second_sym) {
					return 7;
				} else if(penultimate_sym || last_sym) {
					return 8;
				} else {
					/* Fudge char type - will still result in error */
					return 7;
				}
			} else {
				if(before_decimal_point) {
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
get_char_type_description(const int idx)
{
	switch(idx) {
	case 0:
		return _("B, 0 or /");
	case 1:
		if(current_program->numeric_separator == ',') {
			return ",";
		} else {
			return ".";
		}
	case 2:
		if(current_program->decimal_point == '.') {
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
		return "S";
	case 18:
		return "V";
	case 19:
		return _("a P which is before the decimal point");
	case 20:
		return _("a P which is after the decimal point");
	case 21:
		return "1";
	case 22:
		return "N";
	case 23:
		return "E";
	default:
		return NULL;
	}
}

static void
emit_precedence_error(const int preceding_idx, const int following_idx)
{
	const char	* preceding_descr = get_char_type_description(preceding_idx);
	const char	* following_descr = get_char_type_description(following_idx);


	if(following_descr && preceding_descr) {
		if(preceding_idx == following_idx) {
			cb_error(_("%s may only occur once in a PICTURE string"), preceding_descr);
		} else {
			cb_error(_("%s cannot follow %s"), following_descr, preceding_descr);
		}
	} else {
		cb_error(_("invalid PICTURE string detected"));
	}
}

static bool
valid_char_order(const cob_pic_symbol * str, const int s_char_seen)
{
	const int	precedence_table[24][24] = {
		/*
		  Refer to the standard's PICTURE clause precedence rules for
		  complete explanation.
		*/
		/*
		  B  ,  .  +  +  + CR cs cs  Z  Z  +  + cs cs  9  A  S  V  P  P  1  N  E
		  0           -  - DB        *  *  -  -           X
		  /
		*/
		{ 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0 },
		{ 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0 },
		{ 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 },
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
		{ 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0 },
		{ 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0 },
		{ 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
		{ 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0 },
		{ 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
		{ 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0 },
		{ 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
		{ 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0 },
		{ 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
		{ 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0 },
		{ 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1 },
		{ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0 },
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
		{ 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0 },
		{ 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0 },
		{ 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0 },
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 },
		{ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0 },
		{ 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 },
	};
	int		error_emitted[24][24] = {{ 0 }};
	int		chars_seen[24] = { 0 };
	const cob_pic_symbol	* first_floating_sym;
	const cob_pic_symbol	* last_floating_sym;
	int		before_decimal_point = 1;
	int		idx;
	const cob_pic_symbol	* s;
	int		repeated;
	int		i;
	int		j;
	int		non_p_digits_seen = 0;
	bool	error_detected = false;

	chars_seen[17] = s_char_seen;
	find_floating_insertion_str(str, &first_floating_sym, &last_floating_sym);

	for(s = str; s->symbol != '\0'; ++s) {
		/* Perform the check twice if a character is repeated, e.g. to detect 9VV. */
		repeated = s->times_repeated > 1;
		for(i = 0; i < 1 + repeated; ++i) {
			idx = char_to_precedence_idx(str, s,
										 first_floating_sym,
										 last_floating_sym,
										 before_decimal_point,
										 non_p_digits_seen);
			if(idx == -1) {
				continue;
			} else if(9 <= idx && idx <= 15) {
				non_p_digits_seen = 1;
			}

			/*
			  Emit an error if the current character is following a
			  character it is not allowed to. Display an error once
			  for each combination detected.
			*/
			for(j = 0; j < 24; ++j) {
				if(chars_seen[j]
						&& !precedence_table[idx][j]
						&& !error_emitted[idx][j]) {
					emit_precedence_error(j, idx);
					error_emitted[idx][j] = 1;
					error_detected = true;
				}
			}
			chars_seen[idx] = 1;

			if(s->symbol == 'V'
					|| s->symbol == current_program->decimal_point) {
				before_decimal_point = 0;
			}
		}
	}

	return !error_detected;
}

static int
get_pic_number_from_str(const unsigned char * str, bool & error_detected)
{
	cob_u32_t		num_sig_digits = 0;
	int			value = 0;

	/* Ignore leading zeroes */
	for(; *str == '0' && *str; str++);

	/* Get the value. */
	for(; *str != ')' && *str; str++) {
		if(!isdigit(*str)) {
			cb_error(_("number or constant in parentheses is not an unsigned integer"));
			error_detected = true;
			break;
		}

		num_sig_digits++;
		if(num_sig_digits <= 9) {
			value = value * 10 + (*str - '0');
		} else if(num_sig_digits == 10) {
			cb_error(_("only up to 9 significant digits are permitted within parentheses"));
			error_detected = true;
		}
	}

	if(value == 0) {
		cb_error(_("number or constant in parentheses must be greater than zero"));
		error_detected = true;
	}

	return value;
}

/*
  Return the number in parentheses. p should point to the opening parenthesis.
  When the function returns, p will point to the closing parentheses or the null
  terminator.
*/
static int
get_number_in_parentheses(const unsigned char ** const p,
						  bool & error_detected,
						  bool & end_pic_processing)
{
	const unsigned char	* open_paren = *p;
	const unsigned char	* close_paren;
	const unsigned char	* c;
	int			contains_name = 0;
	size_t			name_length;
	char		*	name_buff;
	cb_tree			item;
	cb_tree			item_value;

	for(close_paren = *p; *close_paren != ')' && *close_paren;
			++close_paren);

	if(!*close_paren) {
		cb_error(_("unbalanced parentheses"));
		*p = close_paren;
		/* There are no more informative messages to display. */
		end_pic_processing = true;
		return 1;
	}

	/* Find out if the parens contain a number or a constant-name. */
	for(c = open_paren + 1; c != close_paren; ++c) {
		if(!(isdigit(*c) || *c == '.' || *c == '+'
				|| *c == '-')) {
			contains_name = 1;
			break;
		}
	}

	*p = close_paren;

	if(open_paren + 1 == close_paren) {
		cb_error(_("parentheses must contain (a constant-name defined as) a positive integer"));
		error_detected = true;
		return 1;
	}

	if(contains_name) {
		/* Copy name */
		name_length = close_paren - open_paren;
		name_buff = (char *) cobc_parse_malloc(name_length);
		strncpy(name_buff, (char *) open_paren + 1, name_length);
		name_buff[name_length - 1] = '\0';

		/* Build reference to name */
		item = cb_ref(cb_build_reference(name_buff));

		if(item == cb_error_node) {
			error_detected = true;
			return 1;
		} else if(!(CB_FIELD_P(item)
					&& CB_FIELD(item)->flag_item_78)) {
			cb_error(_("'%s' is not a constant-name"), name_buff);
			error_detected = true;
			return 1;
		}

		item_value = CB_VALUE(CB_FIELD(item)->values);
		if(!CB_NUMERIC_LITERAL_P(item_value)) {
			cb_error(_("'%s' is not a numeric literal"), name_buff);
			error_detected = true;
			return 1;
		} else if(CB_LITERAL(item_value)->scale != 0) {
			cb_error(_("'%s' is not an integer"), name_buff);
			error_detected = true;
			return 1;
		} else if(CB_LITERAL(item_value)->sign != 0) {
			cb_error(_("'%s' is not unsigned"), name_buff);
			error_detected = true;
			return 1;
		}

		cobc_parse_free(name_buff);

		return get_pic_number_from_str(CB_LITERAL(item_value)->data,
									   error_detected);
	} else {
		return get_pic_number_from_str(open_paren + 1,
									   error_detected);
	}
}

cb_tree
cb_build_picture(const char * str)
{
	cb_picture * pic = (cb_picture *) make_tree(CB_TAG_PICTURE, CB_CATEGORY_UNKNOWN, sizeof(cb_picture));
	static cob_pic_symbol * pic_buff = NULL;
	unsigned int pic_str_len = 0;
	size_t		idx = 0;
	size_t		buff_cnt = 0;
	bool		s_char_seen = false;
	bool		asterisk_seen = false;
	bool		z_char_seen = false;
	bool		cur_char_seen = false;
	cob_u32_t	s_count = 0;
	cob_u32_t	v_count = 0;
	cob_u32_t	digits = 0;
	cob_u32_t	real_digits = 0;
	cob_u32_t	x_digits = 0;
	int			category = 0;
	int			size = 0;
	int			scale = 0;
	int			paren_num;
	bool		end_immediately = false;
	unsigned char first_last_char = '\0';
	unsigned char second_last_char = '\0';
	bool		error_detected = false;


	if(strlen(str) == 0) {
		cb_error(_("missing PICTURE string"));
		goto end;
	}

	if(!pic_buff) {
		pic_buff = (cob_pic_symbol *) cobc_main_malloc((size_t)COB_MINI_BUFF * sizeof(cob_pic_symbol));
	}

	for(const unsigned char * p = (const unsigned char *)str; *p; p++) {
		int n = 1;
		unsigned char c = *p;
repeat:
		/* Count the number of repeated chars */
		while(p[1] == c) {
			p++, n++, pic_str_len++;
		}

		if(*p == ')' && p[1] == '(') {
			cb_error(_("only one set of parentheses is permitted"));
			error_detected = true;

			do {
				++p;
				++pic_str_len;
			} while(*p != ')' && *p != '\0');
		} else if(p[1] == '(') {
			++p;
			++pic_str_len;
			paren_num = get_number_in_parentheses(&p, error_detected, end_immediately);
			if(end_immediately) {
				goto end;
			}

			n += paren_num - 1;
			/*
			  The number of digits of the number in parentheses is
			  counted in the length of the PICTURE string (not the
			  length of the constant-name, if one was used).
			*/
			for(; paren_num != 0; paren_num /= 10) {
				++pic_str_len;
			}

			goto repeat;
		}

		/* Check grammar and category */
		switch(c) {
		case 'A':
			category |= PIC_ALPHABETIC;
			x_digits += n;
			break;

		case 'X':
			category |= PIC_ALPHANUMERIC;
			x_digits += n;
			break;

		case '9':
			category |= PIC_NUMERIC;
			digits += n;
			real_digits += n;
			if(v_count) {
				scale += n;
			}
			break;

		case 'N':
			if(!(category & PIC_NATIONAL)) {
				category |= PIC_NATIONAL;
				CB_UNFINISHED("USAGE NATIONAL");
			}
			x_digits += n;
			break;

		case 'S':
			category |= PIC_NUMERIC;
			if(s_count <= 1) {
				s_count += n;
				if(s_count > 1) {
					cb_error(_("%s may only occur once in a PICTURE string"), "S");
					error_detected = true;
				}
			}
			if(idx != 0) {
				cb_error(_("S must be at start of PICTURE string"));
				error_detected = true;
			}

			s_char_seen = true;
			continue;

		case ',':
		case '.':
			category |= PIC_NUMERIC_EDITED;
			if(c != current_program->decimal_point) {
				break;
			}
		/* fall through */
		case 'V':
			category |= PIC_NUMERIC;
			v_count += n;
			if(v_count > 1) {
				error_detected = true;
			}
			break;

		case 'P': {
			category |= PIC_NUMERIC;
			bool at_beginning = false;
			bool at_end = false;
			switch(buff_cnt) {
			case 0:
				/* P..... */
				at_beginning = true;
				break;
			case 1:
				/* VP.... */
				/* SP.... */
				if(first_last_char == 'V' || first_last_char == 'S') {
					at_beginning = true;
				}
				break;
			case 2:
				/* SVP... */
				if(second_last_char == 'S' && first_last_char == 'V') {
					at_beginning = true;
				}
				break;
			default:
				break;
			}
			if(p[1] == 0 || (p[1] == 'V' && p[2] == 0)) {
				/* .....P */
				/* ....PV */
				at_end = true;
			}
			if(!at_beginning && !at_end) {
				cb_error(_("P must be at start or end of PICTURE string"));
				error_detected = true;
			}
			if(at_beginning) {
				/* Implicit V */
				v_count++;
			}
			digits += n;
			if(v_count) {
				scale += n;
			} else {
				scale -= n;
			}
			break;
		}
		case '0':
		case 'B':
		case '/':
			category |= PIC_EDITED;
			break;

		case '*':
		case 'Z': {
			if(c == '*') {
				asterisk_seen = true;
			} else if(c == 'Z') {
				z_char_seen = true;
			}

			if(asterisk_seen && z_char_seen) {
				cb_error(_("cannot have both Z and * in PICTURE string"));
				error_detected = true;
			}

			category |= PIC_NUMERIC_EDITED;
			if(category & PIC_ALPHABETIC) {
				error_detected = true;
			}
			digits += n;
			if(v_count) {
				scale += n;
			}
			break;
		}
		case '+':
		case '-':
			category |= PIC_NUMERIC_EDITED;
			digits += n - 1;
			if(s_count > 0) {
				++digits;
			}
			s_count++;
			break;

		case 'C':
			category |= PIC_NUMERIC_EDITED;
			if(p[1] != 'R') {
				cb_error(_("C must be followed by R"));
				error_detected = true;
			} else {
				p++;
				pic_str_len++;
			}

			s_count++;
			break;

		case 'D':
			category |= PIC_NUMERIC_EDITED;

			if(p[1] != 'B') {
				cb_error(_("D must be followed by B"));
				error_detected = true;
			} else {
				p++;
				pic_str_len++;
			}

			s_count++;
			break;

		default:
			if(c == current_program->currency_symbol) {
				category |= PIC_NUMERIC_EDITED;
				if(!cur_char_seen) {
					digits += n - 1;
					cur_char_seen = true;
				} else {
					digits += n;
				}
				break;
			}

			cb_error(_("invalid PICTURE character '%c'"), c);
			error_detected = true;
		}

		/* Calculate size */
		if(c != 'V' && c != 'P') {
			size += n;
		}
		if(c == 'C' || c == 'D') {
			size += n;
		}
		if(c == 'N') {
			size += n * (COB_NATIONAL_SIZE - 1);
		}

		/* Store in the buffer */
		pic_buff[idx].symbol = c;
		pic_buff[idx].times_repeated = n;
		++idx;
		second_last_char = first_last_char;
		first_last_char = c;
		++buff_cnt;
		if(unlikely(idx == COB_MINI_MAX)) {
			break;
		}
	}
	pic_buff[idx].symbol = '\0';

	if(pic_str_len > cb_pic_length) {
		cb_error(_("PICTURE string may not contain more than %d characters; contains %d characters"),
				 cb_pic_length, pic_str_len);
		error_detected = true;
	}
	if(digits == 0 && x_digits == 0) {
		cb_error(_("PICTURE string must contain at least one of the set A, N, X, Z, 1, 9 and *; "
				   "or at least two of the set +, - and the currency symbol"));
		error_detected = true;
	}
	if(!valid_char_order(pic_buff, s_char_seen)) {
		error_detected = true;
	}

	if(error_detected) {
		goto end;
	}

	/* Set picture */
	pic->orig = (char *) cobc_check_string(str);
	pic->size = size;
	pic->digits = digits;
	pic->scale = scale;
	pic->have_sign = s_count;
	pic->real_digits = real_digits;

	/* Set picture category */
	switch(category) {
	case PIC_ALPHABETIC:
		pic->category = CB_CATEGORY_ALPHABETIC;
		break;
	case PIC_NUMERIC:
		pic->category = CB_CATEGORY_NUMERIC;
		if(digits > COB_MAX_DIGITS) {
			cb_error(_("numeric field cannot be larger than %d digits"), COB_MAX_DIGITS);
		}
		break;
	case PIC_ALPHANUMERIC:
	case PIC_NATIONAL:
		pic->category = CB_CATEGORY_ALPHANUMERIC;
		break;
	case PIC_NUMERIC_EDITED:
		pic->str = (cob_pic_symbol *) cobc_parse_malloc((idx + 1) * sizeof(cob_pic_symbol));
		memcpy(pic->str, pic_buff, idx * sizeof(cob_pic_symbol));
		pic->category = CB_CATEGORY_NUMERIC_EDITED;
		pic->lenstr = (int) idx;
		break;
	case PIC_EDITED:
	case PIC_ALPHABETIC_EDITED:
	case PIC_ALPHANUMERIC_EDITED:
	case PIC_NATIONAL_EDITED:
		pic->str = (cob_pic_symbol *) cobc_parse_malloc((idx + 1) * sizeof(cob_pic_symbol));
		memcpy(pic->str, pic_buff, idx * sizeof(cob_pic_symbol));
		pic->category = CB_CATEGORY_ALPHANUMERIC_EDITED;
		pic->lenstr = (int) idx;
		pic->digits = x_digits;
		break;
	default:
		break;
	}

end:
	return pic;
}

/* Field */

cb_tree
cb_build_field(cb_tree name)
{
	cb_field * p = (cb_field *) make_tree(CB_TAG_FIELD, CB_CATEGORY_UNKNOWN, sizeof(cb_field));
	p->id = cb_field_id++;
	p->name = cb_define(name, p);
	p->ename = NULL;
	p->usage = CB_USAGE_DISPLAY;
	p->storage = CB_STORAGE_WORKING;
	p->occurs_max = 1;
	return p;
}

cb_tree
cb_build_implicit_field(cb_tree name, const int len)
{
	cb_tree x = cb_build_field(name);
	char pic[32];
	memset(pic, 0, sizeof(pic));
	snprintf(pic, sizeof(pic), "X(%d)", len);
	CB_FIELD(x)->pic = CB_PICTURE(cb_build_picture(pic));
	cb_validate_field(CB_FIELD(x));
	return x;
}

cb_tree
cb_build_constant(cb_tree name, cb_tree value)
{
	cb_tree x = cb_build_field(name);
	x->category = cb_tree_category(value);
	CB_FIELD(x)->storage = CB_STORAGE_CONSTANT;
	CB_FIELD(x)->values = CB_LIST_INIT(value);
	return x;
}

#if	0	/* RXWRXW - Field */
cb_field *
CB_FIELD_PTR(cb_tree x)
{
	if(CB_REFERENCE_P(x)) {
		return CB_FIELD(cb_ref(x));
	}
	return CB_FIELD(x);
}
#endif

cb_field *
cb_field_add(cb_field * f, cb_field * p)
{
	if(f == NULL) {
		return p;
	}
	cb_field * t;
	for(t = f; t->sister; t = t->sister) {
		;
	}
	t->sister = p;
	return f;
}

cb_field *
cb_field_founder(const cb_field * f)
{
	const cb_field * ff = f;
	while(ff->parent) {
		ff = ff->parent;
	}
	return (cb_field *)ff;
}

cb_field *
cb_field_variable_size(const cb_field * f)
{
	for(cb_field * fc = f->children; fc; fc = fc->sister) {
		if(fc->depending) {
			return fc;
		}
		cb_field * p = cb_field_variable_size(fc);
		if(p != NULL) {
			return p;
		}
	}
	return NULL;
}

unsigned int
cb_field_variable_address(const cb_field * fld)
{
	const cb_field * f = fld;
	for(cb_field * p = f->parent; p; f = f->parent, p = f->parent) {
		for(p = p->children; p != f; p = p->sister) {
			if(p->depending || cb_field_variable_size(p)) {
				return 1;
			}
		}
	}
	return 0;
}

/* Check if field 'pfld' is subordinate to field 'f' */

int
cb_field_subordinate(const cb_field * pfld, const cb_field * f)
{
	for(cb_field * p = pfld->parent; p; p = p->parent) {
		if(p == f) {
			return 1;
		}
	}
	return 0;
}

/* SYMBOLIC CHARACTERS */

void
cb_build_symbolic_chars(const cb_tree sym_list, const cb_tree alphabet)
{
	cb_alphabet_name * ap;
	if(alphabet) {
		ap = CB_ALPHABET_NAME(alphabet);
	} else {
		ap = NULL;
	}
	for(cb_tree l = sym_list; l; l = CB_CHAIN(l)) {
		int n = cb_get_int(CB_PURPOSE(l)) - 1;
		unsigned char buff[4];
		if(ap) {
			buff[0] = (unsigned char)ap->alphachr[n];
		} else {
			buff[0] = (unsigned char)n;
		}
		buff[1] = 0;
		cb_tree x2 = cb_build_alphanumeric_literal(buff, (size_t)1);
		CB_LITERAL(x2)->all = 1;
		cb_tree x = cb_build_constant(CB_VALUE(l), x2);
		CB_FIELD(x)->flag_item_78 = 1;
		CB_FIELD(x)->flag_is_global = 1;
		CB_FIELD(x)->flag_internal_constant = 1;
		CB_FIELD(x)->level = 1;
		(void)cb_validate_78_item(CB_FIELD(x), 0);
	}
}

/* Report */

cb_report *
build_report(cb_tree name)
{
	cb_report * p = (cb_report *) make_tree(CB_TAG_REPORT, CB_CATEGORY_UNKNOWN, sizeof(cb_report));
	p->name = cb_define(name, p);
	p->cname = cb_to_cname(p->name);

#if	0	/* RXWRXW RP */
	p->organization = COB_ORG_SEQUENTIAL;
	p->access_mode = COB_ACCESS_SEQUENTIAL;
	p->handler = CB_LABEL(cb_standard_error_handler);
	p->handler_prog = current_program;
#endif
	return p;
}

/* File */

cb_file *
build_file(cb_tree name)
{
	cb_file * p = (cb_file *) make_tree(CB_TAG_FILE, CB_CATEGORY_UNKNOWN, sizeof(cb_file));
	p->name = cb_define(name, p);
	p->cname = cb_to_cname(p->name);

	p->organization = COB_ORG_SEQUENTIAL;
	p->access_mode = COB_ACCESS_SEQUENTIAL;
	p->handler = CB_LABEL(cb_standard_error_handler);
	p->handler_prog = current_program;
	return p;
}

void
validate_file(cb_file * f, cb_tree name)
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
	if(!f->assign && f->organization != COB_ORG_SORT && !f->flag_fileid) {
		file_error(name, "ASSIGN", CB_FILE_ERR_REQUIRED);
	}
	/* Check RECORD/RELATIVE KEY clause */
	switch(f->organization) {
	case COB_ORG_INDEXED:
		if(f->key == NULL) {
			file_error(name, "RECORD KEY", CB_FILE_ERR_REQUIRED);
		}
		break;
	case COB_ORG_RELATIVE:
		if(f->key == NULL && f->access_mode != COB_ACCESS_SEQUENTIAL) {
			file_error(name, "RELATIVE KEY", CB_FILE_ERR_REQUIRED);
		}
		if(f->alt_key_list) {
			file_error(name, "ALTERNATE", CB_FILE_ERR_INVALID_FT);
			f->alt_key_list = NULL;
		}
		break;
	default:
		if(f->key) {
			file_error(name, "RECORD", CB_FILE_ERR_INVALID_FT);
			f->key = NULL;
		}
		if(f->alt_key_list) {
			file_error(name, "ALTERNATE", CB_FILE_ERR_INVALID_FT);
			f->alt_key_list = NULL;
		}
		if(f->access_mode == COB_ACCESS_DYNAMIC ||
				f->access_mode == COB_ACCESS_RANDOM) {
			file_error(name, "ORGANIZATION", CB_FILE_ERR_INVALID);
		}
		break;
	}
}

static void
validate_indexed_key_field(cb_file * f, cb_field * records, cb_tree key)
{
	cb_tree			key_ref;
	cb_field	*	k;
	cb_field	*	p;
	cb_field	*	v;

	int			field_end;

	/* get reference (and check if it exists) */
	key_ref = cb_ref(key);
	if(key_ref == cb_error_node) {
		return;
	}
	k = CB_FIELD_PTR(key_ref);

	/* Check that key file is actual part of the file's records */
	v = cb_field_founder(k);
	for(p = records; p; p = p->sister) {
		if(p == v) {
			break;
		}
	}
	if(!p) {
		cb_error_x(f, _("invalid KEY item '%s', not in file '%s'"),
				   k->name, f->name);
		return;
	}

	/* Validate minimum record size against key field's end */
	/* FIXME: calculate minumum length for all keys first and only check the biggest */
	if(f->record_min > 0) {
		field_end = k->offset + k->size;
		if(field_end > f->record_min) {
			cb_error_x(k, _("minimal record length %d can not hold the key item '%s';"
							" needs to be at least %d"), f->record_min, k->name, field_end);
		}
	}
}

void
finalize_file(cb_file * f, cb_field * records)
{
	/* stdin/stderr and LINE ADVANCING are L/S */
	if(f->special || f->flag_line_adv) {
		f->organization = COB_ORG_LINE_SEQUENTIAL;
	}
	if(f->flag_fileid && !f->assign) {
		f->assign = cb_build_alphanumeric_literal(f->name,
					strlen(f->name));
	}

	/* associate records to file (seperate and first for being able
	   to resolve references, for example in validate_indexed_key_field */
	for(cb_field * p = records; p; p = p->sister) {
		p->file = f;
	}

	/* Validate INDEXED key fields (RELATIVE keys can only be validated when
	   the whole data division has been processed). */
	if(f->organization == COB_ORG_INDEXED) {
		if(f->key) {
			validate_indexed_key_field(f, records, f->key);
		}
		if(f->alt_key_list) {
			for(cb_alt_key * cbak = f->alt_key_list; cbak; cbak = cbak->next) {
				validate_indexed_key_field(f, records, cbak->key);
			}
		}
	}

	/* Check the record size if it is limited */
	for(cb_field * p = records; p; p = p->sister) {
		if(f->record_min > 0) {
			if(p->size < f->record_min) {
				cb_error_x(p,
						   _("size of record '%s' (%d) smaller than minimum of file '%s' (%d)"),
						   p->name, p->size, f->name, f->record_min);
			}
		}
		if(f->record_max > 0) {
			if(p->size > f->record_max) {
				cb_error_x(p,
						   _("size of record '%s' (%d) larger than maximum of file '%s' (%d)"),
						   p->name, p->size, f->name, f->record_max);
			}
		}
	}

	/* Compute the record size */
	if(f->record_min == 0) {
		if(records) {
			f->record_min = records->size;
		} else {
			f->record_min = 0;
		}
	}
	for(cb_field * p = records; p; p = p->sister) {
		cb_field * v = cb_field_variable_size(p);
		if(v && v->offset + v->size * v->occurs_min < f->record_min) {
			f->record_min = v->offset + v->size * v->occurs_min;
		}
		if(p->size < f->record_min) {
			f->record_min = p->size;
		}
		if(p->size > f->record_max) {
			f->record_max = p->size;
		}
	}

	if(f->organization == COB_ORG_INDEXED) {
		if(f->record_max > MAX_FD_RECORD_IDX)  {
			f->record_max = MAX_FD_RECORD_IDX;
			cb_error(_("file '%s': record size (IDX) %d exceeds maximum allowed (%d)"),
					 f->name, f->record_max, MAX_FD_RECORD_IDX);
		}
	} else if(f->record_max > MAX_FD_RECORD) {
		cb_error(_("file '%s': record size %d exceeds maximum allowed (%d)"),
				 f->name, f->record_max, MAX_FD_RECORD);
	}

	if(f->same_clause) {
		for(cb_tree l = current_program->file_list; l; l = CB_CHAIN(l)) {
			if(CB_FILE(CB_VALUE(l))->same_clause == f->same_clause) {
				if(CB_FILE(CB_VALUE(l))->flag_finalized) {
					if(f->record_max > CB_FILE(CB_VALUE(l))->record->memory_size) {
						CB_FILE(CB_VALUE(l))->record->memory_size =
						f->record_max;
					}
					f->record = CB_FILE(CB_VALUE(l))->record;
					for(cb_field * p = records; p; p = p->sister) {
						p->file = f;
						p->redefines = f->record;
					}
					for(cb_field * p = f->record->sister; p; p = p->sister) {
						if(!p->sister) {
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
	if(f->record_max == 0) {
		f->record_max = 32;
		f->record_min = 32;
	}
	if(f->organization == COB_ORG_LINE_SEQUENTIAL) {
		f->record_min = 0;
	}
	if(!scratch_buff) {
		scratch_buff = (char *) cobc_main_malloc((size_t)COB_MINI_BUFF);
	}
	snprintf(scratch_buff, (size_t)COB_MINI_MAX, "%s Record", f->name);
	f->record = CB_FIELD(cb_build_implicit_field(cb_build_reference(scratch_buff),
						 f->record_max));
	f->record->sister = records;
	f->record->count++;
	if(f->flag_external) {
		current_program->flag_has_external = 1;
		f->record->flag_external = 1;
	}

	for(cb_field * p = records; p; p = p->sister) {
		p->redefines = f->record;
#if	1	/* RXWRXW - Global/External */
		if(p->flag_is_global) {
			f->record->flag_is_global = 1;
		}
#endif
	}

	if(f->code_set_items) {
		check_code_set_items_are_subitems_of_records(f);
	}

	f->flag_finalized = 1;
	if(f->linage) {
		snprintf(scratch_buff, (size_t)COB_MINI_MAX,
				 "LINAGE-COUNTER %s", f->name);
		cb_tree x = cb_build_field(cb_build_reference(scratch_buff));
		CB_FIELD(x)->usage = CB_USAGE_UNSIGNED_INT;
		CB_FIELD(x)->values = CB_LIST_INIT(cb_zero);
		CB_FIELD(x)->count++;
		cb_validate_field(CB_FIELD(x));
		f->linage_ctr = cb_build_field_reference(CB_FIELD(x), NULL);
		CB_FIELD_ADD(current_program->working_storage, CB_FIELD(x));
	}
}

/* Communication description */

struct cb_cd *
cb_build_cd(cb_tree name)
{
	cb_cd * p = (cb_cd *) make_tree(CB_TAG_CD, CB_CATEGORY_UNKNOWN, sizeof(cb_cd));
	p->name = cb_define(name, p);

	return p;
}

void
cb_finalize_cd(struct cb_cd * cd, struct cb_field * records)
{
	struct cb_field	* p;

	if(cd->record) {
		cd->record->sister = records;
	} else {
		cd->record = records;
	}

	for(p = records; p; p = p->sister) {
		/* TO-DO: Check record size is exactly 87 chars */

		p->cd = cd;
		if(p != cd->record) {
			p->redefines = cd->record;
		}
	}
}

/* Reference */

cb_tree
cb_build_reference(const char * name)
{
	cb_reference * p = (cb_reference *) make_tree(CB_TAG_REFERENCE, CB_CATEGORY_UNKNOWN, sizeof(cb_reference));
	/* Look up / insert word into hash list */
	lookup_word(p, name);
	p->source_file = cb_source_file;
	p->source_line = cb_source_line;
	return p;
}

cb_tree
cb_build_filler(void)
{
	char name[20];
	sprintf(name, "FILLER %d", filler_id++);
	cb_tree x = cb_build_reference(name);
	x->source_line = cb_source_line;
	CB_REFERENCE(x)->flag_filler_ref = 1;
	return x;
}

cb_tree
cb_build_field_reference(cb_field * f, cb_tree ref)
{
	cb_tree x = cb_build_reference(f->name);
	cb_word * word = CB_REFERENCE(x)->word;
	if(ref) {
		memcpy(x, ref, sizeof(cb_reference));
	}
	x->category = CB_CATEGORY_UNKNOWN;
	CB_REFERENCE(x)->word = word;
	CB_REFERENCE(x)->value = f;
	return x;
}

static void
cb_define_system_name(const char * name)
{
	cb_tree x = cb_build_reference(name);
	if(CB_WORD_COUNT(x) == 0) {
		cb_tree y = get_system_name(name);
		/* Paranoid */
		if(y) {
			cb_define(x, y);
		}
	}
}

void
cb_set_system_names(void)
{
	cb_define_system_name("CONSOLE");
	cb_define_system_name("SYSIN");
	cb_define_system_name("SYSIPT");
	cb_define_system_name("STDIN");
	cb_define_system_name("SYSOUT");
	cb_define_system_name("STDOUT");
	cb_define_system_name("SYSERR");
	cb_define_system_name("STDERR");
	cb_define_system_name("SYSLST");
	cb_define_system_name("SYSLIST");
	cb_define_system_name("FORMFEED");
}

static COB_INLINE COB_A_INLINE int
field_is_in_file_record(const cb_tree file,
						const struct cb_field * const field)
{
	return CB_FILE_P(file)
		   && CB_FILE(file) == cb_field_founder(field)->file;
}

static COB_INLINE COB_A_INLINE int
field_is_in_cd_record(const cb_tree cd,
					  const struct cb_field * const field)
{
	return CB_CD_P(cd)
		   && CB_CD(cd) == cb_field_founder(field)->cd;
}

cb_tree
cb_ref(cb_tree x)
{
	if(CB_INVALID_TREE(x)) {
		return cb_error_node;
	}
	cb_reference * r = CB_REFERENCE(x);
	/* If this reference has already been resolved(and the value
	   has been cached), then just return the value */
	if(r->value) {
		if(cb_listing_xref && r->flag_receiving) {
			/* adjust the receiving flag as this will often be set on later calls only */
			if(CB_FIELD_P(r->value)) {
				cobc_xref_link(&CB_FIELD(r->value)->xref, r->source_line, 1);
			} else if(CB_FILE_P(r->value)) {
				cobc_xref_link(&CB_FILE(r->value)->xref, r->source_line, 1);
			}
		}
		return r->value;
	}

	/* Resolve the value */

	cb_tree candidate = NULL;
	size_t ambiguous = 0;
	cb_tree items = r->word->items;
	for(; items; items = CB_CHAIN(items)) {
		/* Find a candidate value by resolving qualification */
		cb_tree v = CB_VALUE(items);
		cb_tree c = r->chain;
		switch(CB_TREE_TAG(v)) {
		case CB_TAG_FIELD: {
			/* In case the value is a field, it might be qualified
			   by its parent names and a file name */
			cb_field * p;
			if(CB_FIELD(v)->flag_indexed_by) {
				p = CB_FIELD(v)->index_qual;
			} else {
				p = CB_FIELD(v)->parent;
			}
			/* Resolve by parents */
			for(; p; p = p->parent) {
				if(c && strcasecmp(CB_NAME(c), p->name) == 0) {
					c = CB_REFERENCE(c)->chain;
				}
			}

			/* Resolve by file or CD */
			if(c && CB_REFERENCE(c)->chain == NULL
					&& CB_WORD_COUNT(c) == 1) {
				if(field_is_in_file_record(cb_ref(c), CB_FIELD(v))
						|| field_is_in_cd_record(cb_ref(c), CB_FIELD(v))) {
					c = CB_REFERENCE(c)->chain;
				}
			}
		}
		break;
		case CB_TAG_LABEL: {
			/* In case the value is a label, it might be qualified
			   by its section name */
			cb_label * s = CB_LABEL(v)->section;

			/* Unqualified paragraph name referenced within the section
			   is resolved without ambiguity check if not duplicated */
			if(c == NULL && r->offset && s == CB_LABEL(r->offset)) {
				for(cb_tree cb1 = CB_CHAIN(items); cb1; cb1 = CB_CHAIN(cb1)) {
					cb_tree cb2 = CB_VALUE(cb1);
					if(s == CB_LABEL(cb2)->section) {
						ambiguous_error(x);
						goto error;
					}
				}
				candidate = v;
				goto end;
			}

			/* Resolve by section name */
			if(c && s && strcasecmp(CB_NAME(c), (char *)s->name) == 0) {
				c = CB_REFERENCE(c)->chain;
			}
		}
		break;
		default:
			/* Other values cannot be qualified */
			break;
		}

		/* A well qualified value is a good candidate */
		if(c == NULL) {
			if(candidate == NULL) {
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
	if(candidate == NULL) {
		if(likely(current_program->nested_level <= 0)) {
			goto undef_error;
		}
		/* Nested program - check parents for GLOBAL candidate */
		ambiguous = 0;
		/* RXWRXW
				val = hash((const unsigned char *)r->word->name);
		*/
		size_t val = r->hashval;
		for(cb_program * prog = current_program->next_program; prog; prog = prog->next_program) {
			if(prog->nested_level >= current_program->nested_level) {
				continue;
			}
			for(cb_word * w = prog->word_table[val]; w; w = w->next) {
				if(strcasecmp(r->word->name, w->name) == 0) {
					candidate = global_check(r, w->items, &ambiguous);
					if(candidate) {
						if(ambiguous) {
							ambiguous_error(x);
							goto error;
						}
						if(CB_FILE_P(candidate)) {
							current_program->flag_gen_error = 1;
						}
						goto end;
					}
				}
			}
			if(prog->nested_level == 0) {
				break;
			}
		}
		goto undef_error;
	}

	/* Reference is ambiguous */
	if(ambiguous) {
		ambiguous_error(x);
		goto error;
	}

end:
	if(CB_FIELD_P(candidate)) {
		CB_FIELD(candidate)->count++;
		if(CB_FIELD(candidate)->flag_invalid) {
			goto error;
		}
	} else if(CB_LABEL_P(candidate) && r->flag_alter_code) {
		CB_LABEL(candidate)->flag_alter = 1;
	}

	if(cb_listing_xref) {
		if(CB_FIELD_P(candidate)) {
			cobc_xref_link(&CB_FIELD(candidate)->xref, r->source_line, r->flag_receiving);
			cobc_xref_link_parent(CB_FIELD(candidate));
		} else if(CB_LABEL_P(candidate)) {
			cobc_xref_link(&CB_LABEL(candidate)->xref, r->source_line, 0);
		} else if(CB_FILE_P(candidate)) {
			cobc_xref_link(&CB_FILE(candidate)->xref, r->source_line, r->flag_receiving);
		}
	}

	r->value = candidate;
	return r->value;

undef_error:
	undefined_error(x);
	/* Fall through */

error:
	r->value = cb_error_node;
	return cb_error_node;
}

static char *
display_literal(char * disp, struct cb_literal * l)
{
	if(CB_NUMERIC_LITERAL_P(l)) {
		if(l->scale == 0) {
			snprintf(disp, 38, "%s%.36s", (char *)(l->sign == -1 ? "-" : ""), (char *)l->data);
		} else if(l->scale > 0) {
			snprintf(disp, 38, "%s%.*s.%s", (char *)(l->sign == -1 ? "-" : ""),
					 l->size - l->scale, l->data, (char *)l->data + l->size - l->scale);
		} else {
			snprintf(disp, 38, "%s%.36s", (char *)(l->sign == -1 ? "-" : ""), (char *)l->data);
		}
	} else {
		sprintf(disp, "%.38s", (char *)l->data);
	}
	return disp;
}

/* Check if comparing field to literal is always TRUE or FALSE */
static cb_tree
compare_field_literal(cb_tree e, int swap, cb_tree x, const int op, struct cb_literal * l)
{
	int	i, j, scale;
	int	alph_lit, ref_mod, zero_val;
	char	lit_disp[40];
	struct cb_field * f;

	f = CB_FIELD(cb_ref(x));
	if(f->flag_any_length
			|| f->pic == NULL) {
		return cb_any;
	}

	ref_mod = 0;
	if(CB_REFERENCE_P(x)) {
		if(CB_REFERENCE(x)->offset
				|| CB_REFERENCE(x)->length) {
			ref_mod = 1;
		}
	}

	for(i = (int) strlen((const char *)l->data); i > 0 && l->data[i - 1] == ' '; i--);
	alph_lit = 0;
	zero_val = 1;
	for(j = 0; l->data[j] != 0; j++) {
		if(!isdigit(l->data[j])) {
			alph_lit = 1;
		}
		if(l->data[j] != '0') {
			zero_val = 0;
		}
	}

	if((f->pic->category != CB_CATEGORY_NUMERIC
			&& f->pic->category != CB_CATEGORY_NUMERIC_EDITED)
			|| ref_mod) {
		if(i > f->size
				&& !ref_mod) {	/* Leave reference mod to run-time */
			copy_file_line(e, l, NULL);
			if(cb_warn_constant_expr
					&& !was_prev_warn(e->source_line, 2)) {
				cb_warning_x(cb_warn_constant_expr, e,
							 _("literal '%.38s' is longer than %s"),
							 display_literal(lit_disp, l), f->name);
			}
			switch(op) {
			case '=':
				return cb_false;
			case '~':
				return cb_true;
			}
		}
		return cb_any;
	}

	if(f->pic->scale < 0) {	/* Leave for run-time */
		return cb_any;
	}

	scale = l->scale;
	for(j = (int) strlen((const char *)l->data); scale > 0 && j > 0 && l->data[j - 1] == '0'; j--) {
		scale--;
	}

	if(scale > 0
			&& f->pic->scale >= 0
			&& f->pic->scale < scale) {
		copy_file_line(e, l, NULL);
		if(cb_warn_constant_expr
				&& !was_prev_warn(e->source_line, 4)) {
			cb_warning_x(cb_warn_constant_expr, e,
						 _("literal '%s' has more decimals than %s"),
						 display_literal(lit_disp, l), f->name);
		}
		switch(op) {
		case '=':
			return cb_false;
		case '~':
			return cb_true;
		}
	}

	if(alph_lit) {
		copy_file_line(e, l, NULL);
		if(cb_warn_constant_expr
				&& f->pic->category == CB_CATEGORY_NUMERIC
				&& !was_prev_warn(e->source_line, 3)) {
			cb_warning_x(cb_warn_constant_expr, e,
						 _("literal '%s' is alphanumeric but %s is numeric"),
						 display_literal(lit_disp, l), f->name);
		}
		return cb_any;
	}

	/* Adjust for leading ZERO & trailing ZEROS|SPACES in literal */
	for(i = (int) strlen((const char *)l->data); i > 0 && l->data[i - 1] == ' '; i--);
	for(j = 0; l->data[j] == '0'; j++, i--);
	scale = l->scale;
	for(j = (int) strlen((const char *)l->data); scale > 0 && j > 0 && l->data[j - 1] == '0'; j--, i--) {
		scale--;
	}

	/* If Literal has more digits in whole portion than field can hold
	 * Then the literal value will never match the field contents
	 */
	if((i - scale) >= 0
			&& (f->size - f->pic->scale) >= 0
			&& (i - scale) > (f->size - f->pic->scale)) {
		copy_file_line(e, l, NULL);
		if(cb_warn_constant_expr
				&& !was_prev_warn(e->source_line, 4)) {
			cb_warning_x(cb_warn_constant_expr, e,
						 _("literal '%s' has more digits than %s"),
						 display_literal(lit_disp, l), f->name);
		}
		switch(op) {
		case '=':
			return cb_false;
		case '~':
			return cb_true;
		}
		if(f->pic->category == CB_CATEGORY_NUMERIC) {
			switch(op) {
			case '>':
			case ']':
				return swap ? cb_true : cb_false;
			case '<':
			case '[':
				return swap ? cb_false : cb_true;
			}
		}
	}


	if(cb_warn_constant_expr && f->pic->have_sign == 0) {
		/* note: the actual result may be different if non-numeric
		data is stored in the numeric fields - and may (later)
		be dependent on compiler configuration flags;
		therefore we don't set cb_true/cb_false here */
		/* comparision with zero */
		if(zero_val) {
			switch(op) {
			case '<':
				copy_file_line(e, l, NULL);
				if(!was_prev_warn(e->source_line, 5)) {
					cb_warning_x(cb_warn_constant_expr, e,
								 _("unsigned '%s' may not be %s %s"),
								 f->name, explain_operator(op), "ZERO");
				}
				break;
			case ']':
				copy_file_line(e, l, NULL);
				if(!was_prev_warn(e->source_line, 5)) {
					cb_warning_x(cb_warn_constant_expr, e,
								 _("unsigned '%s' may always be %s %s"),
								 f->name, explain_operator(op), "ZERO");
				}
				break;
			default:
				break;
			}
			/* comparision with negative literal */
		} else if(l->sign < 0) {
			switch(op) {
			case '<':
			case '[':
				copy_file_line(e, l, NULL);
				if(!was_prev_warn(e->source_line, 5)) {
					cb_warning_x(cb_warn_constant_expr, e,
								 _("unsigned '%s' may not be %s %s"),
								 f->name, explain_operator(op), display_literal(lit_disp, l));
				}
				break;
			case '>':
			case ']':
				copy_file_line(e, l, NULL);
				if(!was_prev_warn(e->source_line, 5)) {
					cb_warning_x(cb_warn_constant_expr, e,
								 _("unsigned '%s' may always be %s %s"),
								 f->name, explain_operator(op), display_literal(lit_disp, l));
				}
				break;
			default:
				break;
			}
		}
	}
	return cb_any;
}

/* Expression */

cb_tree
cb_build_binary_op(cb_tree x, const int op, cb_tree y)
{
	cb_category category = CB_CATEGORY_UNKNOWN;
	cob_s64_t	xval, yval, rslt;
	char		result[48];
	char	*	llit, *rlit;
	int			i, j, xscale, yscale, rscale;
	cb_literal *	xl, *yl;
	cb_tree		relop, e;

	if(op == '@'
			&& y == NULL
			&& CB_NUMERIC_LITERAL_P(x)) {	/* Parens around a Numeric Literal */
		return x;
	}

	/* setting an error tree to point to the correct expression
	   instead of the literal/var definition / current line */
	e = relop = cb_any;
	copy_file_line(e, NULL, NULL);
	llit = rlit = NULL;

	switch(op) {
	case '+':
	case '-':
	case '*':
	case '/':
	case '^':
		/* Arithmetic operators */
		if(CB_TREE_CLASS(x) == CB_CLASS_POINTER ||
				CB_TREE_CLASS(y) == CB_CLASS_POINTER) {
			category = CB_CATEGORY_DATA_POINTER;
			break;
		}
		x = cb_check_numeric_value(x);
		y = cb_check_numeric_value(y);
		if(x == cb_error_node || y == cb_error_node) {
			return cb_error_node;
		}
		/*
		 * If this is an operation between two simple integer numerics
		 * then resolve the value here at compile time -> "constant folding"
		 */
		if(cb_constant_folding
				&&  CB_NUMERIC_LITERAL_P(x)
				&&  CB_NUMERIC_LITERAL_P(y)) {
			xl = CB_LITERAL(x);
			yl = CB_LITERAL(y);

			if(xl->llit == 0
					&& xl->size >= (unsigned int)xl->scale
					&& yl->llit == 0
					&& yl->size >= (unsigned int)yl->scale
					&& xl->all == 0
					&& yl->all == 0) {
				xval = atoll((const char *)xl->data);
				if(xl->sign == -1) {
					xval = -xval;
				}
				yval = atoll((const char *)yl->data);
				if(yl->sign == -1) {
					yval = -yval;
				}
				xscale = xl->scale;
				yscale = yl->scale;
				rscale = 0;
				rslt = 0;
				if(op == '+' || op == '-') {
					while(xscale < yscale) {
						xval = xval * 10;
						xscale++;
					}
					while(xscale > yscale) {
						yval = yval * 10;
						yscale++;
					}
					rscale = xscale;
					if(op == '+') {
						rslt = xval + yval;
					} else {
						rslt = xval - yval;
					}
				} else if(op == '*') {
					rscale = xscale + yscale;
					rslt = xval * yval;
				} else if(op == '/' && yval != 0) {
					while(yscale > 0) {
						xval = xval * 10;
						yscale--;
					}
					rscale = xscale;
					if((xval % yval) == 0) {
						rslt = xval / yval;
					}
				}
				while(rscale > 0
						&& rslt != 0
						&& (rslt % 10) == 0) {
					rslt = rslt / 10;
					rscale--;
				}
				switch(op) {
				case '+':
				case '-':
				case '*':
					sprintf(result, CB_FMT_LLD, rslt);
					return cb_build_numeric_literal(0, result, rscale);
					break;
				case '/':
					if(yval == 0) {				/* Avoid Divide by ZERO */
						cb_warning_x(COBC_WARN_FILLER, x, _("divide by constant ZERO"));
						break;
					}
					if(rslt != 0) {
						sprintf(result, CB_FMT_LLD, rslt);
						return cb_build_numeric_literal(0, result, rscale);
					}
					/* only calculate simple integer numerics */
					if(xl->scale != 0 || yl->scale != 0) {
						break;
					}
					if((xval % yval) == 0) {
						sprintf(result, CB_FMT_LLD, xval / yval);
						return cb_build_numeric_literal(0, result, rscale);
					}
					break;
				case '^':
					/* only calculate simple integer numerics */
					if(xl->scale != 0
							|| yl->scale != 0
							|| yval < 0) {
						break;
					}
					if(yval == 0
							|| xval == 1) {
						strcpy(result, "1");
					} else {
						rslt = xval;
						while(--yval > 0) {
							rslt = rslt * xval;
						}
						sprintf(result, CB_FMT_LLD, rslt);
					}
					return cb_build_numeric_literal(0, result, 0);
				default:
					break;
				}
			}
		}
		category = CB_CATEGORY_NUMERIC;
		break;

	case '=':
	case '~':
	case '<':
	case '>':
	case '[':
	case ']':
		/* Relational operators */
		if((CB_REF_OR_FIELD_P(x)) &&
				CB_FIELD(cb_ref(x))->level == 88) {
			cb_error_x(e, _("invalid expression"));
			return cb_error_node;
		}
		if((CB_REF_OR_FIELD_P(y)) &&
				CB_FIELD(cb_ref(y))->level == 88) {
			cb_error_x(e, _("invalid expression"));
			return cb_error_node;
		}

		if(x == cb_zero) {
			xl = CB_LITERAL(cb_zero_lit);
			xl->source_line = prev_expr_line = cb_exp_line;
		} else if(CB_LITERAL_P(x)) {
			xl = CB_LITERAL(x);
		} else {
			xl = NULL;
		}
		if(y == cb_zero) {
			yl = CB_LITERAL(cb_zero_lit);
			yl->source_line = prev_expr_line = cb_exp_line;
		} else if(CB_LITERAL_P(y)) {
			yl = CB_LITERAL(y);
		} else {
			yl = NULL;
		}

		if(CB_REF_OR_FIELD_P(y)
				&& CB_FIELD(cb_ref(y))->usage == CB_USAGE_DISPLAY
				&& (CB_LITERAL_P(x) || x == cb_zero)
				&& xl->all == 0) {
			relop = compare_field_literal(e, 1, y, op, xl);
		} else if(CB_REF_OR_FIELD_P(x)
				  && CB_FIELD(cb_ref(x))->usage == CB_USAGE_DISPLAY
				  && (CB_LITERAL_P(y) || y == cb_zero)
				  && yl->all == 0) {
			relop = compare_field_literal(e, 0, x, op, yl);
			/*
			 * If this is an operation between two simple integer numerics
			 * then resolve the value here at compile time -> "constant folding"
			 */
		} else if(cb_constant_folding
				  &&  CB_NUMERIC_LITERAL_P(x)
				  && CB_NUMERIC_LITERAL_P(y)) {
			xl = CB_LITERAL(x);
			yl = CB_LITERAL(y);
			llit = (char *)xl->data;
			rlit = (char *)yl->data;
			if(xl->llit == 0
					&& xl->scale == 0
					&& yl->llit == 0
					&& yl->scale == 0
					&& xl->sign == 0
					&& yl->sign == 0
					&& xl->all == 0
					&& yl->all == 0) {
				copy_file_line(e, y, x);
				xval = atoll((const char *)xl->data);
				yval = atoll((const char *)yl->data);
				switch(op) {
				case '=':
					if(xval == yval) {
						relop = cb_true;
					} else {
						relop = cb_false;
					}
					break;
				case '~':
					if(xval != yval) {
						relop = cb_true;
					} else {
						relop = cb_false;
					}
					break;
				case '>':
					if(xval > yval) {
						relop = cb_true;
					} else {
						relop = cb_false;
					}
					break;
				case '<':
					if(xval < yval) {
						relop = cb_true;
					} else {
						relop = cb_false;
					}
					break;
				case ']':
					if(xval >= yval) {
						relop = cb_true;
					} else {
						relop = cb_false;
					}
					break;
				case '[':
					if(xval <= yval) {
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
			 */
		} else if(cb_constant_folding
				  &&  CB_LITERAL_P(x)
				  && CB_LITERAL_P(y)
				  && !CB_NUMERIC_LITERAL_P(x)
				  && !CB_NUMERIC_LITERAL_P(y)) {
			copy_file_line(e, y, x);
			xl = CB_LITERAL(x);
			yl = CB_LITERAL(y);
			llit = (char *)xl->data;
			rlit = (char *)yl->data;
			for(i = j = 0; xl->data[i] != 0 && yl->data[j] != 0; i++, j++) {
				if(xl->data[i] != yl->data[j]) {
					break;
				}
			}
			if(xl->data[i] == 0
					&& yl->data[j] == ' ') {
				while(yl->data[j] == ' ') {
					j++;
				}
			} else if(xl->data[i] == ' '
					  && yl->data[j] == 0) {
				while(xl->data[i] == ' ') {
					i++;
				}
			}
			switch(op) {
			case '=':
				if(xl->data[i] == yl->data[j]) {
					relop = cb_true;
				} else {
					relop = cb_false;
				}
				break;
			case '~':
				if(xl->data[i] != yl->data[j]) {
					relop = cb_true;
				} else {
					relop = cb_false;
				}
				break;
			case '>':
				if(xl->data[i] > yl->data[j]) {
					relop = cb_true;
				} else {
					relop = cb_false;
				}
				break;
			case '<':
				if(xl->data[i] < yl->data[j]) {
					relop = cb_true;
				} else {
					relop = cb_false;
				}
				break;
			case ']':
				if(xl->data[i] >= yl->data[j]) {
					relop = cb_true;
				} else {
					relop = cb_false;
				}
				break;
			case '[':
				if(xl->data[i] <= yl->data[j]) {
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
		category = CB_CATEGORY_BOOLEAN;
		break;

	case '!':
	case '&':
	case '|':
		/* Logical operators */
		if(CB_TREE_CLASS(x) != CB_CLASS_BOOLEAN
				|| (y && CB_TREE_CLASS(y) != CB_CLASS_BOOLEAN)) {
			copy_file_line(e, y, x);
			if(CB_NUMERIC_LITERAL_P(x)
					&& y
					&& CB_NUMERIC_LITERAL_P(y)) {
				xl = CB_LITERAL(x);
				yl = CB_LITERAL(y);
				llit = (char *)xl->data;
				rlit = (char *)yl->data;
				cb_error_x(e, _("invalid expression: %s %s %s"),
						   llit, explain_operator(op), rlit);
			} else {
				cb_error_x(e, _("invalid expression"));
			}
			return cb_error_node;
		}
		if((x == cb_true || x == cb_false)
				&& (y == cb_true || y == cb_false)) {
			if(op == '&') {
				if(x == cb_true && y == cb_true) {
					relop = cb_true;
				} else {
					relop = cb_false;
				}
			} else if(op == '|') {
				if(x == cb_true || y == cb_true) {
					relop = cb_true;
				} else {
					relop = cb_false;
				}
			}
		} else if(op == '!') {
			if(x == cb_true) {
				relop = cb_false;
			} else if(x == cb_false) {
				relop = cb_true;
			}
		}
		category = CB_CATEGORY_BOOLEAN;
		break;

	case '@':
		/* Parentheses */
		category = CB_TREE_CATEGORY(x);
		break;

	case 0:
		/* Operation on invalid elements */
		cb_error_x(e, _("invalid expression"));
		return cb_error_node;

	default:
		/* LCOV_EXCL_START */
		cobc_err_msg(_("unexpected operator: %d"), op);
		COBC_ABORT();
		/* LCOV_EXCL_STOP */
	}

	if(relop == cb_true) {
		if(cb_warn_constant_expr) {
			if(rlit && llit) {
				if(!was_prev_warn(e->source_line, 1)) {
					cb_warning_x(cb_warn_constant_expr, e,
								 _("expression '%.38s' %s '%.38s' is always TRUE"),
								 llit, explain_operator(op), rlit);
				}
			} else {
				if(!was_prev_warn(e->source_line, -1)) {
					cb_warning_x(cb_warn_constant_expr, e,
								 _("expression is always TRUE"));
				}
			}
			prev_expr_line = cb_exp_line = e->source_line;
		}
		return cb_true;
	}
	if(relop == cb_false) {
		if(cb_warn_constant_expr) {
			if(rlit && llit) {
				if(!was_prev_warn(e->source_line, 0)) {
					cb_warning_x(cb_warn_constant_expr, e,
								 _("expression '%.38s' %s '%.38s' is always FALSE"),
								 llit, explain_operator(op), rlit);
				}
			} else {
				if(!was_prev_warn(e->source_line, -2)) {
					cb_warning_x(cb_warn_constant_expr, e,
								 _("expression is always FALSE"));
				}
			}
			prev_expr_line = cb_exp_line = e->source_line;
		}
		return cb_false;
	}

	cb_binary_op * p = (cb_binary_op *) make_tree(CB_TAG_BINARY_OP, category, sizeof(cb_binary_op));
	p->op = op;
	p->x = x;
	p->y = y;
	return p;
}

cb_tree
cb_build_binary_list(cb_tree l, const int op)
{
	cb_tree e = CB_VALUE(l);
	for(l = CB_CHAIN(l); l; l = CB_CHAIN(l)) {
		e = cb_build_binary_op(e, op, CB_VALUE(l));
	}
	return e;
}

/* Function call */

cb_tree
cb_build_funcall(const char * name, const int argc,
				 const cb_tree a1, const cb_tree a2, const cb_tree a3,
				 const cb_tree a4, const cb_tree a5, const cb_tree a6,
				 const cb_tree a7, const cb_tree a8, const cb_tree a9,
				 const cb_tree a10, const cb_tree a11)
{
	cb_funcall * p = (cb_funcall *) make_tree(CB_TAG_FUNCALL, CB_CATEGORY_BOOLEAN, sizeof(cb_funcall));
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
	return p;
}

/* Type cast */

cb_tree
cb_build_cast(const enum cb_cast_type type, const cb_tree val)
{
	cb_category category;
	if(type == CB_CAST_INTEGER) {
		category = CB_CATEGORY_NUMERIC;
	} else {
		category = CB_CATEGORY_UNKNOWN;
	}
	cb_cast * p = (cb_cast *) make_tree(CB_TAG_CAST, category, sizeof(cb_cast));
	p->cast_type = type;
	p->val = val;
	return p;
}

cb_tree
cb_build_cast_int(const cb_tree val)
{
	cb_cast * p = (cb_cast *) make_tree(CB_TAG_CAST, CB_CATEGORY_NUMERIC, sizeof(cb_cast));
	p->cast_type = CB_CAST_INTEGER;
	p->val = val;
	return p;
}

cb_tree
cb_build_cast_llint(const cb_tree val)
{
	cb_cast * p = (cb_cast *) make_tree(CB_TAG_CAST, CB_CATEGORY_NUMERIC, sizeof(cb_cast));
	p->cast_type = CB_CAST_LONG_INT;
	p->val = val;
	return p;
}

/* Label */

cb_tree
cb_build_label(cb_tree name, cb_label * section)
{
	cb_label * p = (cb_label *) make_tree(CB_TAG_LABEL, CB_CATEGORY_UNKNOWN, sizeof(cb_label));
	p->id = cb_id++;
	p->name = cb_define(name, p);
	p->orig_name = p->name;
	p->section = section;
	if(section) {
		cb_para_label * l = (cb_para_label *) cobc_parse_malloc(sizeof(cb_para_label));
		l->next = section->para_label;
		l->para = p;
		section->para_label = l;
		p->section_id = p->section->id;
	} else {
		p->section_id = p->id;
	}
	return p;
}

/* Assign */

cb_tree
cb_build_assign(const cb_tree var, const cb_tree val)
{
	cb_assign * p = (cb_assign *) make_tree(CB_TAG_ASSIGN, CB_CATEGORY_UNKNOWN, sizeof(cb_assign));
	p->var = var;
	p->val = val;
	return p;
}

/* INITIALIZE */

cb_tree
cb_build_initialize(const cb_tree var, const cb_tree val, const cb_tree rep,
					const unsigned int def,
					const unsigned int is_statement,
					const unsigned int no_filler_init)
{
	cb_initialize * p = (cb_initialize *) make_tree(CB_TAG_INITIALIZE, CB_CATEGORY_UNKNOWN, sizeof(cb_initialize));
	p->var = var;
	p->val = val;
	p->rep = rep;
	p->flag_default = (cob_u8_t)def;
	p->flag_init_statement = (cob_u8_t)is_statement;
	p->flag_no_filler_init = (cob_u8_t)no_filler_init;
	return p;
}

/* SEARCH */

cb_tree
cb_build_search(const int flag_all, const cb_tree table, const cb_tree var,
				const cb_tree end_stmt, const cb_tree whens)
{
	cb_search * p = (cb_search *) make_tree(CB_TAG_SEARCH, CB_CATEGORY_UNKNOWN, sizeof(cb_search));
	p->flag_all = flag_all;
	p->table = table;
	p->var = var;
	p->end_stmt = end_stmt;
	p->whens = whens;
	return p;
}

/* CALL */

cb_tree
cb_build_call(const cb_tree name, const cb_tree args, const cb_tree stmt1,
			  const cb_tree stmt2, const cb_tree returning,
			  const cob_u32_t is_system_call, const int convention)
{
	cb_call * p = (cb_call *) make_tree(CB_TAG_CALL, CB_CATEGORY_UNKNOWN, sizeof(cb_call));
	p->name = name;
	p->args = args;
	p->stmt1 = stmt1;
	p->stmt2 = stmt2;
	p->call_returning = returning;
	p->is_system = is_system_call;
	p->convention = convention;
	return p;
}

/* CANCEL */

cb_tree
cb_build_cancel(const cb_tree target)
{
	cb_cancel * p = (cb_cancel *) make_tree(CB_TAG_CANCEL, CB_CATEGORY_UNKNOWN, sizeof(cb_cancel));
	p->target = target;
	return p;
}

/* ALTER */

cb_tree
cb_build_alter(const cb_tree source, const cb_tree target)
{
	cb_alter * p = (cb_alter *) make_tree(CB_TAG_ALTER, CB_CATEGORY_UNKNOWN, sizeof(cb_alter));
	p->source = source;
	p->target = target;
	current_program->alter_list =
	cb_list_append(current_program->alter_list,
				   CB_BUILD_PAIR(source, target));
	return p;
}

/* GO TO */

cb_tree
cb_build_goto(const cb_tree target, const cb_tree depending)
{
	cb_goto * p = (cb_goto *) make_tree(CB_TAG_GOTO, CB_CATEGORY_UNKNOWN, sizeof(cb_goto));
	p->target = target;
	p->depending = depending;
	return p;
}

/* IF */

cb_tree
cb_build_if(const cb_tree test, const cb_tree stmt1, const cb_tree stmt2,
			const unsigned int is_if)
{
	cb_if * p = (cb_if *) make_tree(CB_TAG_IF, CB_CATEGORY_UNKNOWN, sizeof(cb_if));
	p->test = test;
	p->stmt1 = stmt1;
	p->stmt2 = stmt2;
	if(test == cb_true) {		/* Always TRUE so skip 'else code' */
		p->stmt2 = NULL;
	} else if(test == cb_false) {	/* Always FALSE, so skip 'true code' */
		p->stmt1 = NULL;
	}
	if(p->test && CB_TREE_TAG(p->test) == CB_TAG_BINARY_OP) {
		cb_binary_op * bop = CB_BINARY_OP(p->test);
		if(bop->op == '!') {
			if(bop->x == cb_true) {
				p->stmt1 = NULL;
			} else if(bop->x == cb_false) {
				p->stmt2 = NULL;
			}
		}
	}
	p->is_if = is_if;
	return p;
}

/* PERFORM */

cb_tree
cb_build_perform(const enum cb_perform_type type)
{
	cb_perform * p = (cb_perform *) make_tree(CB_TAG_PERFORM, CB_CATEGORY_UNKNOWN, sizeof(cb_perform));
	p->perform_type = type;
	return p;
}

void
cb_build_perform_after_until(void)
{
	after_until = 1;
}

cb_tree
cb_build_perform_varying(cb_tree name, cb_tree from, cb_tree by, cb_tree until)
{
	cb_perform_varying * p = (cb_perform_varying *) make_tree(CB_TAG_PERFORM_VARYING, CB_CATEGORY_UNKNOWN, sizeof(cb_perform_varying));
	p->name = name;
	p->from = from;
	p->until = until;
	if(warningopt) {
		cb_source_line--;
		if(until == cb_false) {
			cb_warning(COBC_WARN_FILLER, _("PERFORM FOREVER since UNTIL is always FALSE"));
		} else if(until == cb_true) {
			if(after_until) {
				cb_warning(COBC_WARN_FILLER, _("PERFORM ONCE since UNTIL is always TRUE"));
			} else {
				cb_warning(COBC_WARN_FILLER, _("PERFORM NEVER since UNTIL is always TRUE"));
			}
		}
		cb_source_line++;
	}

	if(until) {
		cb_save_cond();
	}
	if(until == cb_true
			&& !after_until) {
		cb_false_side();	/* PERFORM body is NEVER executed */
	}

	after_until = 0;
	if(name) {
		if(name == cb_error_node) {
			p->step = NULL;
			return p;
		}
		cb_tree l = cb_ref(name);
		cb_tree x = cb_build_add(name, by, cb_high);
		if(current_program->flag_debugging &&
				!current_statement->flag_in_debug &&
				CB_FIELD_P(l) && CB_FIELD(l)->flag_field_debug) {
			p->step = CB_LIST_INIT(x);
			x = cb_build_debug(cb_debug_name, CB_FIELD_PTR(name)->name,
							   NULL);
			p->step = cb_list_add(p->step, x);
			x = cb_build_debug(cb_debug_contents, NULL, name);
			p->step = cb_list_add(p->step, x);
			x = cb_build_debug_call(CB_FIELD_PTR(name)->debug_section);
			p->step = cb_list_add(p->step, x);
		} else {
			p->step = x;
		}
	} else {
		p->step = NULL;
	}
	return p;
}

/* Statement */

cb_statement *
cb_build_statement(const char * name)
{
	cb_statement * p = (cb_statement *) make_tree(CB_TAG_STATEMENT, CB_CATEGORY_UNKNOWN, sizeof(cb_statement));
	p->name = name;
	return p;
}

/* CONTINUE */

cb_tree
cb_build_continue(void)
{
	cb_continue * p = (cb_continue *) make_tree(CB_TAG_CONTINUE, CB_CATEGORY_UNKNOWN, sizeof(cb_continue));
	return p;
}

/* SET ATTRIBUTE */

cb_tree
cb_build_set_attribute(const cb_field * fld,
					   const cob_flags_t val_on, const cob_flags_t val_off)
{
	cb_set_attr * p = (cb_set_attr *) make_tree(CB_TAG_SET_ATTR, CB_CATEGORY_UNKNOWN, sizeof(cb_set_attr));
	p->fld = (cb_field *)fld;
	p->val_on = val_on;
	p->val_off = val_off;
	return p;
}

/* Prototypes */

static void
warn_if_no_definition_seen_for_prototype(cb_prototype * proto)
{
	struct cb_program	* program;
	const char	*	error_msg;

	program = cb_find_defined_program_by_id(proto->ext_name);
	if(program) {
		return;
	}

	if(cb_warn_prototypes) {
		if(strcmp(proto->name, proto->ext_name) == 0) {
			/*
			  Warn if no definition seen for element with prototype-
			  name.
			*/
			if(proto->type == CB_FUNCTION_TYPE) {
				error_msg = _("no definition/prototype seen for function '%s'");
			} else { /* PROGRAM_TYPE */
				error_msg = _("no definition/prototype seen for program '%s'");
			}
			cb_warning_x(cb_warn_prototypes, proto, error_msg, proto->name);
		} else {
			/*
			  Warn if no definition seen for element with given
			  external-name.
			*/
			if(proto->type == CB_FUNCTION_TYPE) {
				error_msg = _("no definition/prototype seen for function with external name '%s'");
			} else { /* PROGRAM_TYPE */
				error_msg = _("no definition/prototype seen for program with external name '%s'");
			}
			cb_warning_x(cb_warn_prototypes, proto, error_msg, proto->ext_name);
		}
	}
}

cb_tree
cb_build_prototype(const cb_tree prototype_name, const cb_tree ext_name,
				   const int type)
{
	cb_prototype * prototype = (cb_prototype *) make_tree(CB_TAG_PROTOTYPE, CB_CATEGORY_UNKNOWN,
							   sizeof(cb_prototype));
	prototype->source_line = prototype_name->source_line;

	/* Set prototype->name */
	if(CB_LITERAL_P(prototype_name)) {
		prototype->name =
		(const char *) CB_LITERAL(prototype_name)->data;
	} else {
		prototype->name = (const char *) CB_NAME(prototype_name);
	}

	/* Set prototype->ext_name */
	if(ext_name) {
		prototype->ext_name =
		(const char *) CB_LITERAL(ext_name)->data;
	} else if(CB_LITERAL_P(prototype_name)) {
		prototype->ext_name =
		(const char *) CB_LITERAL(prototype_name)->data;
	} else {
		prototype->ext_name = CB_NAME(prototype_name);
	}

	prototype->type = type;

	warn_if_no_definition_seen_for_prototype(prototype);

	return prototype;
}

/* FUNCTION */

/* Build an internal reference to FUNCTION BYTE-LENGTH for resolving LENGTH OF special-register */
cb_tree
cb_build_any_intrinsic(cb_tree args)
{
	cb_intrinsic_table * cbp = lookup_intrinsic("BYTE-LENGTH", 1);
	return make_intrinsic(NULL, cbp, args, NULL, NULL, 0);
}

cb_tree
cb_build_intrinsic(cb_tree name, cb_tree args, cb_tree refmod,
				   const int isuser)
{
	int numargs = (int) cb_list_length(args);
	if(unlikely(isuser)) {
		if(refmod && CB_LITERAL_P(CB_PAIR_X(refmod)) &&
				cb_get_int(CB_PAIR_X(refmod)) < 1) {
			cb_error_x(name, _("FUNCTION '%s' has invalid reference modification"), CB_NAME(name));
			return cb_error_node;
		}
		if(refmod && CB_PAIR_Y(refmod) &&
				CB_LITERAL_P(CB_PAIR_Y(refmod)) &&
				cb_get_int(CB_PAIR_Y(refmod)) < 1) {
			cb_error_x(name, _("FUNCTION '%s' has invalid reference modification"), CB_NAME(name));
			return cb_error_node;
		}
		if(numargs > (int) current_program->max_call_param) {
			current_program->max_call_param = numargs;
		}
		return make_intrinsic(name, &userbp, args, cb_int1, refmod, 1);
	}

	cb_intrinsic_table * cbp = lookup_intrinsic(CB_NAME(name), 1);
	if(!cbp || cbp->active == CB_FEATURE_DISABLED) {
		cb_error_x(name, _("FUNCTION '%s' unknown"), CB_NAME(name));
		return cb_error_node;
	}
	if(cbp->active == CB_FEATURE_NOT_IMPLEMENTED) {
		cb_error_x(name, _("FUNCTION '%s' is not implemented"),
				   cbp->name);
		return cb_error_node;
	}
	if((cbp->args == -1)) {
		if(numargs < cbp->min_args) {
			cb_error_x(name,
					   _("FUNCTION '%s' has wrong number of arguments"),
					   cbp->name);
			return cb_error_node;
		}
	} else {
		if(numargs > cbp->args || numargs < cbp->min_args) {
			cb_error_x(name,
					   _("FUNCTION '%s' has wrong number of arguments"),
					   cbp->name);
			return cb_error_node;
		}
	}
	if(refmod) {
		if(!cbp->refmod) {
			cb_error_x(name, _("FUNCTION '%s' can not have reference modification"), cbp->name);
			return cb_error_node;
		}
		/* TODO: better check needed, see typeck.c (cb_build_identifier) */
		if(CB_LITERAL_P(CB_PAIR_X(refmod)) &&
				cb_get_int(CB_PAIR_X(refmod)) < 1) {
			cb_error_x(name, _("FUNCTION '%s' has invalid reference modification"), cbp->name);
			return cb_error_node;
		}
		if(CB_PAIR_Y(refmod) && CB_LITERAL_P(CB_PAIR_Y(refmod)) &&
				cb_get_int(CB_PAIR_Y(refmod)) < 1) {
			cb_error_x(name, _("FUNCTION '%s' has invalid reference modification"), cbp->name);
			return cb_error_node;
		}
	}

	if(iso_8601_func(cbp->intr_enum)) {
		if(!valid_const_date_time_args(name, cbp, args)) {
			return cb_error_node;
		}
#if !defined(_BSD_SOURCE) && !defined (COB_STRFTIME) && !defined (HAVE_TIMEZONE)
		warn_cannot_get_utc(name, cbp->intr_enum, args);
#endif
	}

	switch(cbp->intr_enum) {
	case CB_INTR_LENGTH:
	case CB_INTR_BYTE_LENGTH: {
		cb_tree x = CB_VALUE(args);
		if(CB_LITERAL_P(x)) {
			return cb_build_length(x);
		}
		return make_intrinsic(name, cbp, args, NULL, NULL, 0);
	}
	case CB_INTR_WHEN_COMPILED:
		if(refmod) {
			return make_intrinsic(name, cbp, CB_LIST_INIT(cb_intr_whencomp), NULL, refmod, 0);
		}
		return cb_intr_whencomp;

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
	case CB_INTR_TEST_DATE_YYYYMMDD:
	case CB_INTR_TEST_DAY_YYYYDDD: {
		cb_tree x = CB_VALUE(args);
		if(cb_tree_category(x) != CB_CATEGORY_NUMERIC) {
			cb_error_x(name, _("FUNCTION '%s' has invalid parameter"), cbp->name);
			return cb_error_node;
		}
		return make_intrinsic(name, cbp, args, NULL, refmod, 0);
	}
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
	case CB_INTR_FORMATTED_CURRENT_DATE:
	case CB_INTR_FORMATTED_DATE:
	case CB_INTR_INTEGER_OF_BOOLEAN:
	case CB_INTR_INTEGER_OF_FORMATTED_DATE:
	case CB_INTR_LOCALE_DATE:
	case CB_INTR_LOCALE_TIME:
	case CB_INTR_LOCALE_TIME_FROM_SECS:
	case CB_INTR_LOWER_CASE:
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
	case CB_INTR_REVERSE:
	case CB_INTR_SECONDS_FROM_FORMATTED_TIME:
	case CB_INTR_SECONDS_PAST_MIDNIGHT:
	case CB_INTR_STORED_CHAR_LENGTH:
	case CB_INTR_TEST_FORMATTED_DATETIME:
	case CB_INTR_TEST_NUMVAL:
	case CB_INTR_TEST_NUMVAL_C:
	case CB_INTR_TEST_NUMVAL_F:
	case CB_INTR_TRIM:
	case CB_INTR_UPPER_CASE:
		return make_intrinsic(name, cbp, args, NULL, refmod, 0);

	case CB_INTR_HIGHEST_ALGEBRAIC:
	case CB_INTR_LOWEST_ALGEBRAIC: {
		cb_tree x = CB_VALUE(args);
		if(!CB_REF_OR_FIELD_P(x)) {
			cb_error_x(name, _("FUNCTION '%s' has invalid parameter"), cbp->name);
			return cb_error_node;
		}
		cb_category catg = cb_tree_category(x);
		if(catg != CB_CATEGORY_NUMERIC &&
				catg != CB_CATEGORY_NUMERIC_EDITED) {
			cb_error_x(name, _("FUNCTION '%s' has invalid parameter"), cbp->name);
			return cb_error_node;
		}
		return make_intrinsic(name, cbp, args, NULL, refmod, 0);
	}

	case CB_INTR_CONCATENATE:
	case CB_INTR_DISPLAY_OF:
	case CB_INTR_FORMATTED_DATETIME:
	case CB_INTR_FORMATTED_TIME:
	case CB_INTR_NATIONAL_OF:
		return make_intrinsic(name, cbp, args, cb_int1, refmod, 0);

	case CB_INTR_DATE_TO_YYYYMMDD:
	case CB_INTR_DAY_TO_YYYYDDD:
	case CB_INTR_LOCALE_COMPARE:
	case CB_INTR_MAX:
	case CB_INTR_MEAN:
	case CB_INTR_MEDIAN:
	case CB_INTR_MIDRANGE:
	case CB_INTR_MIN:
	case CB_INTR_ORD_MAX:
	case CB_INTR_ORD_MIN:
	case CB_INTR_PRESENT_VALUE:
	case CB_INTR_RANDOM:
	case CB_INTR_RANGE:
	case CB_INTR_STANDARD_COMPARE:
	case CB_INTR_STANDARD_DEVIATION:
	case CB_INTR_SUM:
	case CB_INTR_VARIANCE:
	case CB_INTR_YEAR_TO_YYYY:
		return make_intrinsic(name, cbp, args, cb_int1, NULL, 0);
	case CB_INTR_SUBSTITUTE:
	case CB_INTR_SUBSTITUTE_CASE:
		if((numargs % 2) == 0) {
			cb_error_x(name, _("FUNCTION '%s' has wrong number of arguments"), cbp->name);
			return cb_error_node;
		}
#if	0	/* RXWRXW - Substitute param 1 */
		x = CB_VALUE(args);
		if(!CB_REF_OR_FIELD_P(x)) {
			cb_error_x(name, _("FUNCTION '%s' has invalid first parameter"), cbp->name);
			return cb_error_node;
		}
#endif
		return make_intrinsic(name, cbp, args, cb_int1, refmod, 0);

	default:
		cb_error_x(name, _("FUNCTION '%s' unknown"), CB_NAME(name));
		return cb_error_node;
	}
}

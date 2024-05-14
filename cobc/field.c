/*
   Copyright (C) 2001-2024 Free Software Foundation, Inc.
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
#include <limits.h>

#include "cobc.h"
#include "tree.h"
#include "../libcob/coblocal.h"

/* sanity checks */
#if COB_MAX_FIELD_SIZE >= INT_MAX
#error COB_MAX_FIELD_SIZE is too big, must be less than INT_MAX
#endif
#if COB_MAX_FIELD_SIZE_LINKAGE >= INT_MAX
#error COB_MAX_FIELD_SIZE_LINKAGE is too big, must be less than INT_MAX
#endif
#if COB_MAX_UNBOUNDED_SIZE >= INT_MAX
#error COB_MAX_UNBOUNDED_SIZE is too big, must be less than INT_MAX
#endif

/* Function prototypes */
static unsigned int	validate_field_1 (struct cb_field *f);
static unsigned int validate_multi_value (const struct cb_field * const f);

/* Global variables */

cb_tree			cb_depend_check = NULL;
size_t			cb_needs_01 = 0;

/* Local variables */

static struct cb_field	*last_real_field = NULL;
static int		occur_align_size = 0;
static const unsigned char	pic_digits[] = { 2, 4, 7, 9, 12, 14, 16, 18 };
#define CB_MAX_OPS	32
static int			op_pos = 1, op_val_pos;
static char			op_type	[CB_MAX_OPS+1];
static char			op_prec	[CB_MAX_OPS+1];
static cob_s64_t	op_val	[CB_MAX_OPS+1];
static int			op_scale[CB_MAX_OPS+1];

/* Is constant expression list in value really an expression? */
static int
cb_is_expr (cb_tree ch)
{
	cb_tree			t, l;
	int				num;

	if (op_pos >= 0) {
		for (num = 0; num < CB_MAX_OPS; num++) {
			op_type [num] = ' ';
			op_prec [num] = 0;
			op_val	[num] = 0;
		}
	}
	op_pos = op_val_pos = -1;
	num = 0;
	for (l = ch; l; l = CB_CHAIN (l)) {
		t = CB_VALUE (l);
		if (t && CB_LITERAL_P (t)) {
			if (++num > 1)
				return 1;
		}
	}
	return 0;
}

static void
cb_eval_op ( void )
{
	cob_s64_t	lval, rval, xval;
	int			lscale, rscale, xscale;

	if (op_pos >= 0
	 && op_val_pos > 0) {
		lval = op_val [op_val_pos-1];
		lscale = op_scale [op_val_pos-1];
		rval = op_val [op_val_pos];
		rscale = op_scale [op_val_pos];
		op_val_pos--;
		switch (op_type [op_pos]) {
		case '+':
		case '-':
			while (lscale > rscale) {
				rval = rval * 10;
				rscale++;
			}
			while (lscale < rscale) {
				lval = lval * 10;
				lscale++;
			}
			xscale = lscale;
			if (op_type [op_pos] == '+')
				xval = lval + rval;
			else
				xval = lval - rval;
			break;
		case '*':
			xscale = lscale + rscale;
			xval = lval * rval;
			break;
		case '/':
			while (rscale > 0) {
				lval = lval * 10;
				rscale--;
			}
			if (rval == 0) {
				xscale = 0;
				xval = 0;
				cb_error (_("constant expression has Divide by ZERO"));
			} else {
				xscale = lscale;
				xval = lval / rval;
			}
			break;
		case '^':
			while (rscale > 0) {	/* Only whole number exponents */
				rval = rval / 10;
				rscale--;
			}
			if (rval == 0 || lval == 1) {
				xval = 1;
				xscale = 0;
			} else {
				xval = lval;
				xscale = lscale;
				while(--rval > 0) {
					xscale = xscale + lscale;
					xval = xval * lval;
				}
			}
			break;
		case '&':
			xscale = 0;
			xval = (lval && rval);
			break;
		case '|':
			xscale = 0;
			xval = (lval || rval);
			break;
		case '>':
			xscale = 0;
			xval = (lval > rval);
			break;
		case '<':
			xscale = 0;
			xval = (lval < rval);
			break;
		case '=':
			xscale = 0;
			xval = (lval == rval);
			break;
		case ']':
			xscale = 0;
			xval = (lval >= rval);
			break;
		case '[':
			xscale = 0;
			xval = (lval <= rval);
			break;
		case '~':
			xscale = 0;
			xval = (lval != rval);
			break;
		case '(':
			cb_error (_("missing right parenthesis"));
			op_pos--;
			return;
		default:
			op_pos--;
			return;
		}
		op_pos--;
		while (xscale > 0
			&& (xval % 10) == 0) {
			xscale--;
			xval = xval / 10;
		}
		op_scale [op_val_pos] = xscale;
		op_val [op_val_pos] = xval;
	}
}

static void
cb_push_op ( char op, int prec )
{
	while (op_pos >= 0
	   &&  op_val_pos > 0
	   &&  prec > 0
	   &&  op_type [op_pos] != '('
	   &&  prec <= op_prec [op_pos]) {
		cb_eval_op ();
	}
	if (op_pos >= CB_MAX_OPS) {
		cb_error (_("expression stack overflow at %d entries for operation '%c'"), op_pos, op);
		return;
	}
	op_pos++;
	op_type [op_pos] = op;
	op_prec [op_pos] = (char) prec;
}

/* Evaluate expression and store as new Numeric Literal */
static cb_tree
cb_evaluate_expr (cb_tree ch, int normal_prec)
{
	cb_tree			t, l;
	cob_s64_t		xval;
	int				unop = 1, xscale, k;
	char			result[48];
	struct cb_literal	*lp;

	for (l = ch; l; l = CB_CHAIN (l)) {
		t = CB_VALUE (l);
		if (t && CB_LITERAL_P (t)) {
			lp = CB_LITERAL (t);
			if (CB_NUMERIC_LITERAL_P (t)) {
				xval = atoll((const char *)lp->data);
				xscale = lp->scale;
				if (unop) {
					if (lp->sign < 0)		/* Unary op, change sign */
						xval = -xval;
				} else {
					if (lp->sign < 0) {		/* Treat 'sign' as binary op */
						cb_push_op ('-', 4);
					} else if (lp->sign > 0) {
						cb_push_op ('+', 4);
					}
				}
				while (xscale > 0
					&& (xval % 10) == 0) {	/* Remove decimal zeros */
					xscale--;
					xval = xval / 10;
				}
				if (op_val_pos >= CB_MAX_OPS) {
					cb_error (_("expression stack overflow at %d entries"), op_val_pos);
					return cb_error_node;
				}
				op_val_pos++;
				op_val [op_val_pos] = xval;
				op_scale [op_val_pos] = xscale;
				unop = 0;
			} else {
				switch (lp->data[0]) {
				case '(':
					cb_push_op ('(', 0);
					unop = 1;
					break;
				case ')':
					unop = 0;
					for (k=op_pos; k > 0 && op_type[k] != '('; k--);
					if (op_type [k] != '(')
						cb_error (_("missing left parenthesis"));
					while (op_pos >= 0
					   &&  op_val_pos > 0) {
						if (op_type [op_pos] == '(') {
							break;
						}
						cb_eval_op ();
					}
					if (op_pos >= 0
					 && op_type [op_pos] == '(')
						op_pos--;
					break;
				case '+':
					cb_push_op ('+', 4);
					unop = 1;
					break;
				case '-':
					cb_push_op ('-', 4);
					unop = 1;
					break;
				case '*':
					cb_push_op ('*', normal_prec ? 6 : 4);
					unop = 1;
					break;
				case '/':
					cb_push_op ('/', normal_prec ? 6 : 4);
					unop = 1;
					break;
				case '&':
					cb_push_op ('&', normal_prec ? 8 : 4);
					unop = 1;
					break;
				case '|':
					cb_push_op ('|', normal_prec ? 8 : 4);
					unop = 1;
					break;
				case '^':
					cb_push_op ('^', normal_prec ? 7 : 4);
					unop = 1;
					break;
				default:
					cb_error (_("invalid operator '%s' in expression"),lp->data);
					break;
				}
			}
		}
	}
	while (op_pos >= 0
	   &&  op_val_pos > 0) {
		if (op_type [op_pos] == '(') {
			cb_error (_("missing right parenthesis"));
			op_pos--;
			continue;
		}
		cb_eval_op ();
	}
	if (op_pos >= 0) {
		if (op_type[op_pos] == '(') {
			cb_error (_("missing right parenthesis"));
		} else {
			cb_error (_("'%c' operator misplaced"), op_type [op_pos]);
		}
	}
	xval	= op_val [0];
	xscale	= op_scale [0];
	while (xscale > 0) {		/* Reduce to 'fixed point numeric' */
		xscale--;
		xval = xval / 10;
	}
	while (xscale < 0) {		/* Reduce to 'fixed point numeric' */
		xscale++;
		xval = xval * 10;
	}
	sprintf (result, CB_FMT_LLD, xval);
	return cb_build_numeric_literal (0, result, xscale);
}

int
cb_get_level (cb_tree x)
{
#if 1 /* level always contains a valid tree with valid numeric values only
         --> all validation is done in scanner.l */
	return atoi (CB_NAME (x));
#else
	const unsigned char	*p;
	const char		*name;
	int			level;

	if (CB_INVALID_TREE (x)) {
		return 0;
	}
	name = CB_NAME (x);
	level = 0;
	/* Get level */
	for (p = (const unsigned char *)name; *p; p++) {
		if (!isdigit ((int)(*p))) {
			goto level_error;
		}
		level = level * 10 + (COB_D2I(*p));
		if (level > 88) {
			goto level_error;
		}
	}

	/* Check level */
	switch (level) {
	case 66:
	case 77:
	case 78:
	case 88:
		break;
	default:
		if (level < 1 || level > 49) {
			goto level_error;
		}
		break;
	}

	return level;

level_error:
	cb_error_x (x, _("invalid level number '%s'"), name);
	return 0;
#endif
}

cb_tree
cb_build_field_tree (const int level, cb_tree name, struct cb_field *last_field,
		     enum cb_storage storage, struct cb_file *fn,
		     const int expl_level)
{
	struct cb_reference	*r;
	struct cb_field		*f;
	struct cb_field		*p;
	cb_tree			l;
	cb_tree			x;
	int			lv;

	if (!expl_level) {
		/* note: the level number is always a valid tree here, but the
		   name may be a defined constant which leads to an error node */
		if (name == cb_error_node) {
			return cb_error_node;
		}
		/* Check the level number */
		lv = level;
#if 0 /*level is always valid --> 01 thru 49, 77, 66, 78, 88 */
		if (!lv) {
			return cb_error_node;
		}
#endif
	} else {
		lv = expl_level;
	}

	/* Build the field */
	r = CB_REFERENCE (name);
	f = CB_FIELD (cb_build_field (name));
	f->storage = storage;
	last_real_field = last_field;
	if (lv == 78) {
		f->level = 01;
		f->flag_item_78 = 1;
		f->flag_constant = 0;
		return CB_TREE (f);
	} else {
		f->level = lv;
	}
	/* copy EXTERNAL / GLOBAL attribute from file to record */
	if (f->level == 01 && storage == CB_STORAGE_FILE && fn) {
		if (fn->flag_external) {
			f->flag_external = 1;
			current_program->flag_has_external = 1;
		} else if (fn->flag_global) {
			f->flag_is_global = 1;
		}
	}
	if (last_field) {
		if (last_field->level == 77 && f->level != 01
		 && f->level != 77 && f->level != 66 && f->level != 88) {
			cb_error_x (name, _("level number must begin with 01 or 77"));
			return cb_error_node;
		}
	}

	/* Checks for redefinition */
	if (get_warn_opt_value (cb_warn_redefinition)
	 && r->word->count > 1 && !r->flag_filler_ref) {
		if (f->level == 01 || f->level == 77) {
			redefinition_warning (name, NULL);
		} else {
			for (l = r->word->items; l; l = CB_CHAIN (l)) {
				x = CB_VALUE (l);
				if (!CB_FIELD_P (x)
				 || CB_FIELD (x)->level == 01
				 || CB_FIELD (x)->level == 77
				 || ( last_field
				   && f->level == last_field->level
				   && CB_FIELD (x)->parent == last_field->parent)) {
					redefinition_warning (name, x);
					break;
				}
			}
		}
	}

	if (last_field && last_field->level == 88) {
		last_field = last_field->parent;
	}

	/* Link the field into the tree */
	if (f->level == 01 || f->level == 77) {
		/* Top level */
		cb_needs_01 = 0;
		if (last_field) {
			cb_field_founder (last_field)->sister = f;
		}
	} else if (!last_field || cb_needs_01) {
		/* Invalid top level */
		cb_error_x (name, _("level number must begin with 01 or 77"));
		return cb_error_node;
	} else if (f->level == 66) {
		/* Level 66 */
		f->parent = cb_field_founder (last_field);
		for (p = f->parent->children; p && p->sister; p = p->sister) ;
		if (p) {
			p->sister = f;
		}
	} else if (f->level == 88) {
		/* Level 88 */
		f->parent = last_field;
		if (last_real_field && last_real_field->level == 88) {
			/* Level 88 sister */
			last_real_field->sister = f;
		} else {
			/* First Level 88 on this item */
			last_field->validation = f;
			last_field = f;
		}
	} else if (f->level > last_field->level) {
		/* Lower level */
		last_field->children = f;
		f->parent = last_field;
	} else if (f->level == last_field->level) {
		/* Same level; note:
		   last_field is a group if coming from "goto" */
same_level:
		last_field->sister = f;
		f->parent = last_field->parent;
	} else {
		/* Upper level */
		for (p = last_field->parent; p /* <- silence warnings */; p = p->parent) {
			if (p->level == f->level) {
				last_field = p;
				goto same_level;
			}
			if (cb_relax_level_hierarchy && p->level < f->level) {
				break;
			}
		}
		/* always generate dummy filler field to prevent
		   parsing of follow-on fields to fail the same way */
		if (p) /* <- silence warnings */ {
			cb_tree			dummy_fill = cb_build_filler ();
			struct cb_field	*field_fill = CB_FIELD (cb_build_field (dummy_fill));
			field_fill->level = f->level;
			field_fill->flag_filler = 1;
			field_fill->storage = storage;
			field_fill->children = p->children;
			field_fill->parent = p;
			for (p = p->children; p; p = p->sister) {
				p->parent = field_fill;
			}
			field_fill->parent->children = field_fill;
			field_fill->sister = f;
			f->parent = field_fill->parent;
			/* last_field = field_fill; */
		}
		if (cb_relax_level_hierarchy) {
			cb_warning_x (COBC_WARN_FILLER, name,
				_("no previous data item of level %02d"),
				f->level);
		} else {
			cb_error_x (name,
				_("no previous data item of level %02d"),
				f->level);
		}
	}

	/* Inherit parents properties */
	if (f->parent) {
		struct cb_field *parent = f->parent;
		f->usage = parent->usage;
		f->indexes = parent->indexes;
		f->flag_sign_leading = parent->flag_sign_leading;
		f->flag_sign_separate = parent->flag_sign_separate;
		f->flag_is_global = parent->flag_is_global;
		if (f->level <= 66) {
			f->flag_volatile = parent->flag_volatile;
		}
		if (f->storage == CB_STORAGE_SCREEN) {
			f->screen_foreg   = parent->screen_foreg;
			f->screen_backg   = parent->screen_backg;
			f->screen_prompt  = parent->screen_prompt;
			f->screen_control = parent->screen_control;
			f->screen_color   = parent->screen_color;
		}
	}

	return CB_TREE (f);
}

cb_tree
cb_build_full_field_reference (struct cb_field* field)
{
	cb_tree ret = NULL;
	cb_tree ref = NULL;

	for (; field; field = field->parent) {
		if (!field->flag_filler) {
			cb_tree rchain = cb_build_reference (field->name);
			if (ref) {
				CB_REFERENCE (ref)->chain = rchain;
			} else {
				ret = rchain;
			}
			ref = rchain;
		}
	}

	return ret;
}

struct cb_field *
cb_resolve_redefines (struct cb_field *field, cb_tree redefines)
{
	struct cb_field		*f;
	struct cb_reference	*r;
	const char		*name;
	cb_tree			x;
	cb_tree			candidate = NULL;
	cb_tree			items;

	r = CB_REFERENCE (redefines);
	name = CB_NAME (redefines);
	x = CB_TREE (field);

	/* Check qualification */
	if (r->chain) {
		cb_error_x (x, _("'%s' cannot be qualified here"), name);
		return NULL;
	}

	/* Check subscripts */
	if (r->subs) {
		cb_error_x (x, _("'%s' cannot be subscripted here"), name);
		return NULL;
	}

	/* Get last defined name */
	/* note: chaining over these are much faster than chaining over the complete
	         parent using strcasecmp */
	for (items = r->word->items; items; items = CB_CHAIN (items)) {
		const cb_tree value = CB_VALUE (items);
		if (value != x && CB_FIELD_P (value)) {
			candidate = value;
			/* we want to get the last, so no "break" here */
		}
	}
	if (!candidate) {
		if (field->parent) {
			cb_error_x (x, _("'%s' is not defined in '%s'"),
				name, field->parent->name);
		} else {
			undefined_error (redefines);
		}
		return NULL;
	}
	f = CB_FIELD_PTR (candidate);

	/* Check if candidate is in the current group (if any) */
	if (field->parent && field->parent != f->parent) {
		cb_error_x (x, _ ("'%s' is not defined in '%s'"),
			name, field->parent->name);
		return NULL;
	}

	/* Check level number */
	if (f->level != field->level) {
		cb_error_x (x, _("level number of REDEFINES entries must be identical"));
		return NULL;
	}

	if (!cb_indirect_redefines && f->redefines) {
		cb_error_x (x, _("'%s' is not the original definition"), f->name);
		return NULL;
	}

	/* Return the original definition */
	while (f->redefines) {
		f = f->redefines;
	}
	return f;
}

static void copy_into_field_recursive (struct cb_field *, struct cb_field *, const int);

static void
copy_duplicated_field_into_field (struct cb_field *field, struct cb_field *target,
	const int level, const int outer_indexes, const enum cb_storage storage)
{
	cb_tree	x;
	if (!field->flag_filler && field->name) {
		x = cb_build_field_tree (0, cb_build_reference (field->name),
			target, storage, NULL, level);
	} else {
		x = cb_build_field_tree (0, cb_build_filler (),
			target, storage, NULL, level);
	}
	if (x == cb_error_node) {
		return;
	}
	copy_into_field_recursive (field, CB_FIELD (x), outer_indexes);
}

static void
copy_validation (struct cb_field *source, struct cb_field *target)
{
	struct cb_field *val, *last_val;
#if 0 /* in case we want to allow combining condition-names of typedef and field */
	for (last_val = target->validation; last_val; last_val = last_val->sister) {
		/* get to the last validation entry*/
		if (!last_val->sister) {
			break;
		}
	}
#else
	if (target->validation) {
		(void) cb_syntax_check_x (CB_TREE (target->validation), _("duplicate %s"), "level  88");
	}
#endif
	for (val = source->validation; val; val = val->sister) {
		/* create content-name and link into the reference list */
		cb_tree x = cb_build_field_tree (88, cb_build_reference (val->name),
			target, target->storage, target->file, 0);
		last_val = CB_FIELD (x);
		/* directly assign the typef's value + false (no need for copy) */
		last_val->values = val->values;
		last_val->false_88 = val->false_88;
	}
}

static void
copy_children (struct cb_field *child, struct cb_field *target,
	const int level, const int outer_indexes, const enum cb_storage storage)
{
	int level_child;

	if (child->level > level) {
		level_child = child->level;
	} else {
		level_child = level + 1;
		/* ensure that we don't set the "virtual level number" to one of
		   the "special" level numbers */
		if (level_child == 66 || level_child == 78 || level_child == 88) {
			level_child++;
		} else if (level_child == 77) {
			level_child = 79;
		}
	}

	copy_duplicated_field_into_field (child, target, level_child,
		outer_indexes, storage);
}

#define field_attribute_copy(attribute)	\
	if (source->attribute) target->attribute = source->attribute
#define field_attribute_override(attribute)	\
	target->attribute = source->attribute

static void
copy_into_field_recursive (struct cb_field *source, struct cb_field *target,
			const int outer_indexes)
{
	field_attribute_override (usage);

	field_attribute_override (occurs_min);
	field_attribute_override (occurs_max);
	field_attribute_override (flag_occurs);

	if (CB_VALID_TREE (source->depending)) {
#if 0	/* TODO: check if DEPENDING field is part of the original TYPEDEF,
		   if yes then full-qualify the reference */
		struct cb_field *dep_field = CB_FIELD_PTR (source->depending);
		struct cb_field *field;
		target->depending = cb_build_reference (CB_NAME(source->depending));
		dep_field = dep_field->parent;
		if (dep_field) {
			for (field = target->parent; field; field = field->parent) {
				if (dep_field == field) {
					cb_tree rchain = cb_build_full_field_reference (field);
					CB_REFERENCE (target->depending)->chain = rchain;
					break;
				}
			}
		}
#else
		target->depending = cb_build_reference (CB_NAME (source->depending));
#endif
		CB_ADD_TO_CHAIN (target->depending, current_program->reference_list);
	}
	field_attribute_override (nkeys);
	if (source->keys) {
		int	i;

		/* create reference chain all the way up
		   as later fields may have same name */
		const cb_tree rchain = cb_build_full_field_reference (target);

		target->keys = cobc_parse_malloc (sizeof (struct cb_key) * target->nkeys);
		for (i = 0; i < target->nkeys; i++) {
			const struct cb_reference* r = CB_REFERENCE (source->keys[i].key);
			const cb_tree ref = cb_build_reference (r->word->name);
			CB_REFERENCE (ref)->chain = rchain;
			target->keys[i].key = ref;
			CB_ADD_TO_CHAIN (ref, current_program->reference_list);
			field_attribute_override (keys[i].dir);
		}
	}
	if (source->index_list) {
		cb_tree x;
		target->index_list = NULL;
		for (x = source->index_list; x; x = CB_CHAIN (x)) {
			cb_tree ind_ref = cb_build_reference (CB_FIELD_PTR (CB_VALUE (x))->name);
			cb_tree entry = cb_build_index (ind_ref, cb_int1, 1U, target);
			CB_FIELD_PTR (entry)->index_type = CB_STATIC_INT_INDEX;
			if (!target->index_list) {
				target->index_list = CB_LIST_INIT (entry);
			} else {
				target->index_list = cb_list_add (target->index_list, entry);
			}
		}
	}

	field_attribute_override (values);
	field_attribute_override (flag_blank_zero);
	field_attribute_override (flag_justified);
	field_attribute_override (flag_sign_clause);
	field_attribute_override (flag_sign_leading);
	field_attribute_override (flag_sign_separate);
	field_attribute_override (flag_synchronized);
	field_attribute_override (flag_sync_right);
	field_attribute_override (flag_sync_left);
	field_attribute_override (flag_any_length);
	field_attribute_override (flag_any_numeric);
	field_attribute_override (flag_invalid);
	field_attribute_override (flag_item_based);
	field_attribute_override (flag_is_pointer);
	/* Note: attributes must be handled both here and in copy_into_field */

	/* TODO: add copying of align clause and other boolean/bit stuff once added */

	if (CB_VALID_TREE (source->redefines)) {
		cb_tree ref = cb_build_reference (source->redefines->name);
		target->redefines = cb_resolve_redefines (target, ref);
	}

	/* copy all level 88 */
	if (source->validation) {
		copy_validation (source, target);
	}

	if (source->children) {
		copy_children (source->children, target, target->level, outer_indexes, target->storage);
	} else if (source->pic){
		/* take over internal PICTURE representation as-is, no use in re-building
		   that from scratch and handle calculated ->pic special */
		target->pic = cobc_parse_malloc (sizeof (struct cb_picture));
		memcpy (target->pic, source->pic, sizeof (struct cb_picture));
	}

	if (source->sister) {
		/* for children: all sister entries need to be copied */
		copy_duplicated_field_into_field (source->sister,
			target, target->level, outer_indexes, target->storage);
	}
	/* special case: normally incremented during parse */
	target->indexes = source->indexes + outer_indexes;
	cb_validate_field (target);
}


/* note: same message in parser.y */
static void
duplicate_clause_message (cb_tree x, const char *clause)
{
	(void) cb_syntax_check_x (x, _("duplicate %s clause"), clause);
}

void
copy_into_field (struct cb_field *source, struct cb_field *target)
{
#if 0
	cb_tree	external_definition = target->external_definition;
#endif

	/* note: EXTERNAL is always applied from the typedef (if level 1/77),
			 but may be specified on the field;
	   note: MF has different syntax rules and _only_ allows it on the field */
	if (target->level == 1 || target->level == 77) {
		field_attribute_copy (flag_external);
		if (target->flag_external
		 && !target->ename) {
#if 1	/* CHECKME: Which one to use? Possibly depending on AS clause? */
			target->ename = source->ename;
#else
			target->ename = cb_to_cname (target->name);
#endif
		}
	}
	target->usage = source->usage;
	target->common.category = source->common.category;

	/* Note: The attributes GLOBAL and SELECT WHEN are never included;
	         SAME AS does not include EXTERNAL, but the TYPEDEF  */

	if (source->values) {
		if (target->values) {
			duplicate_clause_message (target->values, "VALUE");
		} else {
			target->values = source->values;
		}
	}
	field_attribute_copy (flag_blank_zero);
	field_attribute_copy (flag_justified);
	field_attribute_copy (flag_sign_clause);
	field_attribute_copy (flag_sign_leading);
	field_attribute_copy (flag_sign_separate);
	if (source->flag_synchronized
	 && !target->flag_synchronized) {
		target->flag_synchronized = source->flag_synchronized;
		target->flag_sync_right = source->flag_sync_right;
		target->flag_sync_left = source->flag_sync_left;
	}	
	field_attribute_override (flag_any_length);
	field_attribute_override (flag_any_numeric);
	field_attribute_override (flag_invalid);
	field_attribute_copy (flag_item_based);
	field_attribute_override (flag_is_pointer);
	/* Note: attributes must be handled both here and in copy_into_field_recursive */

	/* copy all level 88 */
	if (source->validation) {
		copy_validation (source, target);
	}

	if (unlikely (!target->like_modifier)) {
		if (source->children) {
			copy_children (source->children, target, target->level, target->indexes, target->storage);
		} else if (source->pic) {
			/* take over internal PICTURE representation as-is, no use in re-building
			   that from scratch and in handling calculated ->pic special */
			target->pic = cobc_parse_malloc (sizeof (struct cb_picture));
			memcpy (target->pic, source->pic, sizeof (struct cb_picture));
		}
	} else {
		struct cb_picture *new_pic = NULL;
		int modifier = cb_get_int (target->like_modifier);
		if (modifier) {
			switch (target->usage) {

			case CB_USAGE_COMP_X:
			case CB_USAGE_COMP_N:
				if (target->pic->category == CB_CATEGORY_ALPHANUMERIC) {
					char		pic[8];
					unsigned char		newsize;
					if (target->pic->size > 8) {
						newsize = 36;
					} else {
						newsize = pic_digits[target->pic->size - 1];
					}
					newsize += (unsigned char)modifier;
					if (newsize > 36) {
						newsize = 36;
					}
					sprintf (pic, "9(%u)", newsize);
					new_pic = cb_build_picture (pic);
					break;
				}

			case CB_USAGE_BINARY:
			case CB_USAGE_PACKED:
			case CB_USAGE_COMP_5:
			case CB_USAGE_COMP_6:
				if (target->pic->orig[0] == '9') {
					char		pic[38];
					/* only a prototype here,
					   TODO: add handling for S and friends... */
					if (modifier > 0) {
						sprintf (pic, "9(%d)", modifier);
						strcat (pic, target->pic->orig);
						new_pic = cb_build_picture (pic);
					} else {
						CB_PENDING_X (CB_TREE (target), "LIKE ... negative-integer");
					}
				} else {
					cb_error_x (CB_TREE (target), _ ("%s clause not compatible with PIC %s"),
						"LIKE", target->pic->orig);
					target->flag_invalid = 1;
				}
				break;

			case CB_USAGE_DISPLAY:
			case CB_USAGE_NATIONAL:
				break;

			default:
				cb_error_x (CB_TREE (target), _("%s clause not compatible with USAGE %s"),
					"LIKE", cb_get_usage_string (target->usage));
				target->flag_invalid = 1;
			}

#if 0		/* TODO, also syntax-check for usage here */
			if (target->cat is_numeric) {
				sprintf (pic, "9(%d)", size_implied);
			} else {
				sprintf (pic, "X(%d)", size_implied);
			}
			new_pic = cb_build_picture (pic);
#endif
		}
		if (new_pic) {
			target->pic = new_pic;
		} else if (target->pic) {
			/* CHECKME: is there any use in re-building the PIC? */
			target->pic = cb_build_picture (target->pic->orig);
		}
	}

	/* adjust reference counter to allow "no codegen" if only used as type */
	source->count--;
#if 0
	target->count--;
	target->external_definition = external_definition;
#endif

	/* validate field to ensure applying its own attributes
	   in relation to its childs) */
	cb_validate_field (target);
}

static COB_INLINE COB_A_INLINE void
emit_incompatible_pic_and_usage_error (cb_tree item, const enum cb_usage usage)
{
	cb_error_x (item, _("%s clause not compatible with USAGE %s"),
		    "PICTURE", cb_get_usage_string (usage));
}

static COB_INLINE COB_A_INLINE int
is_numeric_usage (const enum cb_usage usage)
{
	switch (usage) {
	case CB_USAGE_DISPLAY:
	case CB_USAGE_NATIONAL:
	case CB_USAGE_OBJECT:
		return 0;
	/* case CB_USAGE_ERROR: assume numeric */
	default:
		return 1;
	}
}

static cb_tree
get_first_value (const struct cb_field * const f)
{
	cb_tree x = f->values;
	if (CB_INVALID_TREE (x))
		return NULL;	/* no value / error */

	if (!CB_LIST_P (x))
		return x;		/* simple VALUE */

	x = CB_VALUE (x);
	if (CB_TAB_VALS_P (x)) {
		x = CB_TAB_VALS (x)->values;
		x = CB_VALUE (x);
	}
	return x;
}

/* create an implicit picture for items that miss it but need one,
   return 1 if not possible */
static unsigned int
create_implicit_picture (struct cb_field *f)
{
	cb_tree first_value = get_first_value (f);
	cb_tree			x = CB_TREE (f);
	char			*pp;
	struct cb_literal	*lp;
	int			size_implied = 1;
	int			is_numeric = 0;
	int			ret;
	char			pic[24];

	if (first_value) {
		if (CB_LITERAL_P (first_value)) {
			size_implied = (int)CB_LITERAL (first_value)->size;
			is_numeric = CB_NUMERIC_LITERAL_P (first_value);
		} else if (CB_CONST_P (first_value)) {
			size_implied = 1;
			if (first_value == cb_zero) {
				is_numeric = 1;
			} else {
				is_numeric = 0;
			}
		/* LCOV_EXCL_START */
		} else {
			CB_TREE_TAG_UNEXPECTED_ABORT (x);
		/* LCOV_EXCL_STOP */
		}
	} else {
		first_value = NULL;
	}

	if (!first_value) {
		/* FIXME: ensure this in another place */
		if (f->flag_item_78) {
			level_require_error (x, "VALUE");
			return 1;
		}
		is_numeric = is_numeric_usage (f->usage);
	}

	if (f->storage == CB_STORAGE_SCREEN) {
		cb_tree impl_tree = f->screen_from ? f->screen_from : f->screen_to ? f->screen_to : NULL;
		if (impl_tree) {
			if (impl_tree == cb_error_node) {
				size_implied = 1;	/* go on to allow further checks */
			}
			if (CB_INTRINSIC_P (impl_tree) || CB_CONST_P (impl_tree)) {
				size_implied = FIELD_SIZE_UNKNOWN;
			} else {
				if (CB_INTRINSIC_P (impl_tree) || CB_CONST_P (impl_tree)) {
					size_implied = FIELD_SIZE_UNKNOWN;
				} else {
					size_implied = cb_field_size (impl_tree);
					is_numeric = CB_TREE_CATEGORY (impl_tree) == CB_CATEGORY_NUMERIC;
				}
			}
		} else if (first_value) {
			/* done later*/
		} else {
			f->flag_no_field = 1;
			f->pic = cb_build_picture ("X");
			return 0;
		}

		if (size_implied == FIELD_SIZE_UNKNOWN) {
			cb_error_x (x, _("PICTURE clause required for '%s'"),
				    cb_name (x));
			size_implied = 1;	/* go on to allow further checks */
		}

		if (is_numeric) {
			sprintf (pic, "9(%d)", size_implied);
		} else {
			sprintf (pic, "X(%d)", size_implied);
		}
		f->pic = cb_build_picture (pic);
		if (f->size < size_implied) {
			f->size = size_implied;
		}
		return 0;
	}

	if (f->storage == CB_STORAGE_REPORT) {
		if (f->report_source || f->report_sum_counter) {
			cb_error_x (x, _("PICTURE clause required for '%s'"),
				cb_name (x));
		}
		if (first_value) {
			sprintf (pic, "X(%d)", size_implied);
		} else if (f->report_source) {
			size_implied = 1;
			if (CB_LITERAL_P (f->report_source)) {
				size_implied = (int)CB_LITERAL(f->report_source)->size;
			} else if (CB_FIELD_P (f->report_source)) {
				/* CHECKME: A source of type BINARY may need a different size */
				size_implied = (int)CB_FIELD(f->report_source)->size;
			}
			sprintf (pic, "X(%d)", size_implied);
		} else {
			/* CHECKME: Where do we want to generate a not-field in the C code?
			            instead of raising an error here? */
			f->flag_no_field = 1;
			size_implied = 1;
			strcpy (pic, "X");
		}
		f->pic = cb_build_picture (pic);
		if (f->size < size_implied) {
			f->size = size_implied;
		}
		return 0;
	}

	if (f->flag_item_78 && first_value && CB_LITERAL_P (first_value)) {
#if 0	/* CHECKME: Do we need this here? */
		f->count++;
#endif
		lp = CB_LITERAL (first_value);
		if (CB_NUMERIC_LITERAL_P (first_value)) {
			memset (pic, 0, sizeof (pic));
			pp = pic;
			if (lp->sign) {
				*pp++ = 'S';
			}
			size_implied = (int)lp->size - lp->scale;
			if (size_implied) {
				pp += sprintf (pp, "9(%d)", size_implied);
			}
			if (lp->scale) {
				sprintf (pp, "V9(%d)", lp->scale);
			}
			if (lp->size < 10) {
				f->usage = CB_USAGE_COMP_5;
				f->size = lp->size;	/* CHECKME: that seems wrong */
			} else {
				f->usage = CB_USAGE_DISPLAY;
				f->size = lp->size;
			}
			f->pic = cb_build_picture (pic);
			f->pic->category = CB_CATEGORY_NUMERIC;
		} else {
			sprintf (pic, "X(%d)", (int)lp->size);
			f->pic = cb_build_picture (pic);
			f->pic->category = CB_CATEGORY_ALPHANUMERIC;
			f->usage = CB_USAGE_DISPLAY;
			f->size = lp->size;
		}
		return 0;
	}

	ret = 0;

	if (f->level == 1 || f->level == 77 || !first_value) {
		cb_error_x (x, _("PICTURE clause required for '%s'"),
			    cb_name (x));
		ret = 1;
	}

	if (first_value && CB_NUMERIC_LITERAL_P (first_value)) {
		if (!is_numeric_usage(f->usage)) {
			cb_error_x (x, _("a non-numeric literal is expected for '%s'"),
					cb_name (x));
		}
		if (!ret) {
			cb_error_x (x, _("PICTURE clause required for '%s'"),
					cb_name (x));
			ret = 1;
		}
	}

	/* CHECKME: should we raise an error for !cb_relaxed_syntax_checks? */
	if (!ret) {
		cb_warning_x (cb_warn_additional, x,
			    _("defining implicit picture size %d for '%s'"),
			    size_implied, cb_name (x));
	}
	if (is_numeric) {
		sprintf (pic, "9(%d)", size_implied);
	} else {
		sprintf (pic, "X(%d)", size_implied);
	}
	f->pic = cb_build_picture (pic);
	f->pic->category = CB_CATEGORY_ALPHANUMERIC;
	f->usage = CB_USAGE_DISPLAY;
	if (f->size < size_implied) {
		f->size = size_implied;
	}
	return ret;
}

/* note: this also adjusts the field! */
static unsigned int
validate_any_length_item (struct cb_field *f)
{
	cb_tree	x = CB_TREE (f);

	if (f->storage != CB_STORAGE_LINKAGE) {
		cb_error_x (x, _("'%s' ANY LENGTH only allowed in LINKAGE"), cb_name (x));
		return 1;
	}
	if (f->level != 01) {
		cb_error_x (x, _("'%s' ANY LENGTH must be 01 level"), cb_name (x));
		return 1;
	}
	if (f->flag_item_based || f->flag_external) {
		cb_error_x (x, _("'%s' ANY LENGTH cannot be BASED/EXTERNAL"), cb_name (x));
		return 1;
	}
	if (f->flag_occurs || f->depending || f->children || f->values || f->flag_blank_zero) {
		cb_error_x (x, _("'%s' ANY LENGTH has invalid definition"), cb_name (x));
		return 1;
	}

	if (!f->pic) {
		const char *pic = f->flag_any_numeric ? "9" : "X";
		f->pic = cb_build_picture (pic);
		return 0;
	}
	
	if (f->flag_any_numeric) {
		if (f->pic->category != CB_CATEGORY_NUMERIC) {
			cb_error_x (x, _("'%s' ANY NUMERIC must be PIC 9"),
				  f->name);
		}
	} else if (f->pic->category != CB_CATEGORY_ALPHANUMERIC
			&& f->pic->category != CB_CATEGORY_NATIONAL
			&& f->pic->category != CB_CATEGORY_BOOLEAN) {
		cb_error_x (x, _("'%s' ANY LENGTH must be PIC X, PIC U, PIC N or PIC 1"),
			  f->name);
	}
	/*
	  TODO: Replace pic->category check with f->usage == CB_USAGE_NATIONAL.
	  Currently NATIONAL items are marked as having USAGE DISPLAY.
	*/
	if (!((f->pic->size == 1 && f->usage == CB_USAGE_DISPLAY)
	      || (f->pic->size == 2 && f->pic->category == CB_CATEGORY_NATIONAL))) {
		if (f->flag_any_numeric) {
			cb_error_x (x, _("'%s' ANY NUMERIC has invalid definition"), cb_name (x));
		} else {
			cb_error_x (x, _("'%s' ANY LENGTH has invalid definition"), cb_name (x));
		}
		return 1;
	}

	return 0;
}

static void
validate_external (const struct cb_field * const f)
{
	const cb_tree	x = CB_TREE (f);

	if (f->level != 01 && f->level != 77) {
		cb_error_x (x, _("'%s' EXTERNAL must be specified at 01/77 level"), cb_name (x));
	}
	if (f->storage != CB_STORAGE_WORKING &&
		f->storage != CB_STORAGE_FILE) {
		cb_error_x (x, _("'%s' EXTERNAL can only be specified in WORKING-STORAGE section"),
				cb_name (x));
	}
	if (f->flag_item_based) {
		cb_error_x (x, _("'%s' EXTERNAL and BASED are mutually exclusive"), cb_name (x));
	}
	if (f->redefines) {
		cb_error_x (x, _("'%s' EXTERNAL not allowed with REDEFINES"), cb_name (x));
	}
}

static void
validate_based (const struct cb_field * const f)
{
	const cb_tree	x = CB_TREE (f);

	if (f->storage != CB_STORAGE_WORKING &&
		f->storage != CB_STORAGE_LOCAL &&
		f->storage != CB_STORAGE_LINKAGE) {
		cb_error_x (x, _("'%s' BASED not allowed here"), cb_name (x));
	}
	if (f->redefines) {
		cb_error_x (x, _("'%s' BASED not allowed with REDEFINES"), cb_name (x));
	}
	if (f->level != 01 && f->level != 77) {
		cb_error_x (x, _("'%s' BASED only allowed at the 01 and 77 levels"), cb_name (x));
	}
}

static void
validate_occurs (const struct cb_field * const f)
{
	const cb_tree		x = CB_TREE (f);
	const struct cb_field	*p;

	if (f->level == 01 || f->level == 77) {
		cb_verify_x (x, cb_top_level_occurs_clause, "01/77 OCCURS");
	}

	/* Validate OCCURS DEPENDING */
	if (f->depending) {
		/* Cache field for later checking as the depending field may not be
		   available until the program is completely parsed */
		cb_depend_check = cb_list_add (cb_depend_check, x);

		if (!cb_odoslide && !cb_complex_odo) {
			/* The data item that contains a OCCURS DEPENDING clause shall not
			   be subordinate to a data item that has an OCCURS clause */
			for (p = f->parent; p; p = p->parent) {
				if (p->flag_picture_l) continue;
				if (p->flag_occurs) {
					cb_error_x (CB_TREE (p),
						    _("'%s' cannot have an OCCURS clause due to '%s'"),
						    cb_name (CB_TREE (p)),
						    cb_name (x));
					break;
				}
			}
		}
	}
}

static void
validate_redefines (const struct cb_field * const f)
{
	const cb_tree		x = CB_TREE (f);
	const struct cb_field	*p;

	/* Check OCCURS */
	if (f->redefines->flag_occurs) {
		cb_warning_x (COBC_WARN_FILLER, x,
			      _("the original definition '%s' should not have an OCCURS clause"),
			      f->redefines->name);
	}
	/* Check ANY LENGTH */
	if (f->redefines->flag_any_length) {
		cb_error_x (x,
			      _("the original definition '%s' should not have an ANY LENGTH clause"),
			      f->redefines->name);
	}

	/* Check definition */
	for (p = f->redefines->sister; p && p != f; p = p->sister) {
		if (!p->redefines) {
			cb_error_x (x, _("REDEFINES must follow the original definition"));
			break;
		}
	}

	/* Check variable occurrence */
	if (f->depending
	 || (!f->flag_picture_l && cb_field_variable_size (f))) {
		cb_error_x (x, _("'%s' cannot be variable length"), f->name);
	}
	if (!f->redefines->flag_picture_l && cb_field_variable_size (f->redefines)) {
		cb_error_x (x, _("the original definition '%s' cannot be variable length"),
			    f->redefines->name);
	}
}

/* Perform group-specific validation of f. */
static unsigned int
validate_group (struct cb_field *f)
{
	cb_tree		x = CB_TREE (f);
	unsigned int	ret = 0;

	if (f->pic) {
		group_error (x, "PICTURE");
	}
	if (f->flag_justified) {
		if (!f->flag_picture_l) {
			group_error (x, "JUSTIFIED RIGHT");
		} else {
			cb_error_x (x, _("'%s' cannot have JUSTIFIED RIGHT clause"),
				    cb_name (x));
		}
	}
	if (f->flag_blank_zero) {
		if (!f->flag_picture_l) {
			group_error (x, "BLANK WHEN ZERO");
		} else {
			cb_error_x (x, _("'%s' cannot have BLANK WHEN ZERO clause"),
				    cb_name (x));
		}
	}

	if (f->storage == CB_STORAGE_SCREEN
	 && (f->screen_from || f->screen_to || f->values || f->pic)) {
		cb_error_x (x, _("SCREEN group item '%s' has invalid clause"),
			    cb_name (x));
		ret = 1;
	}
	
	if (f->values && CB_LIST_P (f->values)) {
		ret |= validate_multi_value (f);
	}

	for (f = f->children; f; f = f->sister) {
		ret |= validate_field_1 (f);
	}

	return ret;
}

static unsigned int
validate_pic (struct cb_field *f)
{
	int	need_picture;
	cb_tree	x = CB_TREE (f);

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
	case CB_USAGE_OBJECT:
	case CB_USAGE_POINTER:
	case CB_USAGE_PROGRAM_POINTER:
	case CB_USAGE_FLOAT:
	case CB_USAGE_DOUBLE:
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
		need_picture = 0;
		break;
	case CB_USAGE_ERROR:
		return 1;
	default:
		need_picture = !f->flag_is_external_form;
		break;
	}

	if (f->pic == NULL && need_picture) {
		/* try to built an implicit picture, stop if not possible */
		if (create_implicit_picture (f)) {
			return 1;
		}
		if (f->pic
		 && f->pic->size > f->size)
		f->size = f->pic->size;
	}

	/* ACUCOBOL/RM-COBOL-style COMP-1 ignores the PICTURE clause. */
	if (f->flag_comp_1 && cb_binary_comp_1) {
		return 0;
	}

#if 0 /* CHECKME - come back later to this (possible 4.x only feature) */
	/* Check for Group attributes to be carried to elementary field */
	if (!f->flag_validated
	 && cb_nonnumeric_with_numeric_group_usage == CB_OK
	 && f->parent
	 && !f->children) {
		struct cb_field *p;
		if (f->flag_usage_defined
		 && is_numeric_field (f)) {
			for (p = f->parent; p; p = p->parent) {
				if (p->usage != CB_USAGE_DISPLAY
				 && f->usage != p->usage) {
					cb_error_x (x, _("%s USAGE %s incompatible with %s USAGE %s"),
							p->flag_filler?"FILLER":p->name, cb_get_usage_string (p->usage),
							f->flag_filler?"FILLER":f->name, cb_get_usage_string (f->usage));
					break;
				}
			}
		}
		if (!f->flag_usage_defined
		 && is_numeric_field (f)) {
			for (p = f->parent; p; p = p->parent) {
				if (p->usage != CB_USAGE_DISPLAY) {
					f->usage = p->usage;
					break;
				}
			}
		}
		/* TODO: handle this "per dialect", some disallow this (per ANSI85) or ignore it */
		if (!f->flag_synchronized
		 && f->parent
		 && (f->usage == CB_USAGE_BINARY
		  || f->usage == CB_USAGE_FLOAT
		  || f->usage == CB_USAGE_DOUBLE
		  || f->usage == CB_USAGE_LONG_DOUBLE
		  || f->usage == CB_USAGE_UNSIGNED_SHORT
		  || f->usage == CB_USAGE_SIGNED_SHORT
		  || f->usage == CB_USAGE_UNSIGNED_INT
		  || f->usage == CB_USAGE_SIGNED_INT
		  || f->usage == CB_USAGE_UNSIGNED_LONG
		  || f->usage == CB_USAGE_SIGNED_LONG
		  || f->usage == CB_USAGE_COMP_5
		  || f->usage == CB_USAGE_COMP_6
		  || f->usage == CB_USAGE_FP_DEC64
		  || f->usage == CB_USAGE_FP_DEC128
		  || f->usage == CB_USAGE_FP_BIN32
		  || f->usage == CB_USAGE_FP_BIN64
		  || f->usage == CB_USAGE_FP_BIN128
		  || f->usage == CB_USAGE_LONG_DOUBLE)) {
			struct cb_field *p;
			for (p = f->parent; p; p = p->parent) {
				if (p->flag_synchronized) {
					f->flag_synchronized = 1;
					break;
				}
			}
		}
		/* ignore sync for binary items */
		if (f->flag_synchronized
		 && cb_binary_sync_clause == CB_IGNORE) {
			switch (f->usage) {
			case CB_USAGE_SIGNED_SHORT:
			case CB_USAGE_UNSIGNED_SHORT:
			case CB_USAGE_SIGNED_INT:
			case CB_USAGE_UNSIGNED_INT:
			case CB_USAGE_SIGNED_LONG:
			case CB_USAGE_UNSIGNED_LONG:
				f->flag_synchronized = 0;
				break;
			default:
				break;
			}
		}
		if (f->pic
		 && f->pic->category == CB_CATEGORY_NUMERIC
		 && f->flag_sign_separate == 0
		 && f->flag_sign_leading == 0) {
			for (p = f->parent; p; p = p->parent) {
				if (p->flag_sign_separate
				 || p->flag_sign_leading) {
					f->flag_sign_separate = p->flag_sign_separate;
					f->flag_sign_leading  = p->flag_sign_leading;
					break;
				}
			}
		}
	}
	f->flag_validated = 1;
#endif

	/* if picture is not needed it is an error to specify it
	   note: we may have set the picture internal */
	if (f->pic != NULL && !f->pic->flag_is_calculated && !need_picture) {
		cb_error_x (x, _("'%s' cannot have PICTURE clause"),
			    cb_name (x));
	}

	return 0;
}

static int
validate_usage (struct cb_field * const f)
{
	cb_tree	x = CB_TREE (f);

	/* note: we check for "only accaptable USAGE" for SCREEN and REPORT SECTION
	   indirectly in the parser (we used to check for DISPLAY here) */

	switch (f->usage) {
	case CB_USAGE_BINARY:
	case CB_USAGE_PACKED:
	case CB_USAGE_BIT:
		if (f->pic
		 && f->pic->category != CB_CATEGORY_NUMERIC) {
			emit_incompatible_pic_and_usage_error (x, f->usage);
			return 1;
		}
		break;
	case CB_USAGE_COMP_6:
		if (f->pic
		 && f->pic->category != CB_CATEGORY_NUMERIC) {
			emit_incompatible_pic_and_usage_error (x, f->usage);
			return 1;
		}
		if (f->pic
		 && f->pic->have_sign) {
			cb_warning_x (COBC_WARN_FILLER, x, _("'%s' COMP-6 with sign - changing to COMP-3"), cb_name (x));
			f->usage = CB_USAGE_PACKED;
		}
		break;
	case CB_USAGE_COMP_5:
	case CB_USAGE_COMP_X:
	case CB_USAGE_COMP_N:
		if (f->pic
		 && f->pic->category != CB_CATEGORY_NUMERIC
		 && f->pic->category != CB_CATEGORY_ALPHANUMERIC) {
			emit_incompatible_pic_and_usage_error (x, f->usage);
			return 1;
		}
		break;
	default:
		break;
	}
	return 0;
}

static void
validate_sign (const struct cb_field * const f)
{
	const cb_tree	x = CB_TREE (f);

	if (!(f->pic && f->pic->have_sign)) {
		cb_error_x (x, _("elementary items with SIGN clause must have S in PICTURE"));
	} else if (f->usage != CB_USAGE_DISPLAY
			&& f->usage != CB_USAGE_NATIONAL) {
		cb_error_x (x, _("elementary items with SIGN clause must be USAGE DISPLAY or NATIONAL"));
	}
}

static void
validate_justified_right (const struct cb_field * const f)
{
	const cb_tree	x = CB_TREE (f);

	/* TODO: Error if no PIC? */

	if (f->flag_justified
	 && f->pic
	 && f->pic->category != CB_CATEGORY_ALPHABETIC
	 && f->pic->category != CB_CATEGORY_ALPHANUMERIC
	 && f->pic->category != CB_CATEGORY_BOOLEAN
	 && f->pic->category != CB_CATEGORY_NATIONAL) {
		cb_error_x (x, _("'%s' cannot have JUSTIFIED RIGHT clause"), cb_name (x));
	}
}

static void
validate_blank_when_zero (const struct cb_field * const f)
{
	const cb_tree	x = CB_TREE (f);

	if (f->pic
	 && f->pic->have_sign
	 && f->pic->category != CB_CATEGORY_NUMERIC_EDITED) {
		cb_error_x (x, _("'%s' cannot have S in PICTURE string and BLANK WHEN ZERO"),
			    cb_name (x));
	}

	if (f->usage != CB_USAGE_DISPLAY && f->usage != CB_USAGE_NATIONAL) {
		cb_error_x (x, _("'%s' cannot have BLANK WHEN ZERO without being USAGE DISPLAY or NATIONAL"),
			    cb_name (x));
	}

	if (f->pic) {
		int		i;
		switch (f->pic->category) {
		case CB_CATEGORY_NUMERIC:
			break;
		case CB_CATEGORY_NUMERIC_EDITED:
			for (i = 0; i < f->pic->lenstr; ++i) {
				if (f->pic->str[i].symbol == '*') {
					cb_error_x (x, _("'%s' cannot have * in PICTURE string and BLANK WHEN ZERO"),
						    cb_name (x));
					break;
				}
			}
			break;
		default:
			cb_error_x (x, _("'%s' is not numeric, so cannot have BLANK WHEN ZERO"), cb_name (x));
			break;
		}
	}
}

/* Validate multiple VALUEs */
static unsigned int
validate_multi_value (const struct cb_field * const f)
{

	int num_of_values = 0;
	int	total_occurs, k;

	if (!CB_TAB_VALS_P (CB_VALUE (f->values))) {
		/* simple option: list of literals */
		num_of_values = cb_list_length (f->values);
	} else {
		cb_tree vals;
		int repeated_to_end = 0;
		for (vals = f->values; vals; vals = CB_CHAIN (vals)) {
			/* FIXME: this is wrong, works only if "FROM" is 1
						and there is REPEATED TO END / TO ... */
			const struct cb_table_values *val_entries = CB_TAB_VALS (CB_VALUE (vals));
			int entries_in_this_list = cb_list_length (val_entries->values);
			cb_tree repeated = val_entries->repeat_times;
			if (repeated == cb_null) {
				if (repeated_to_end++) {
					/* TODO: check exact syntax, may only be specified once */
				}
				repeated = NULL;
			}
			if (repeated) {
				entries_in_this_list *= cb_get_int (repeated);
			}
			/* TODO: Check that there is no overlapping */
			num_of_values += entries_in_this_list;
		}
	}

	{
		const struct cb_field	*p = f;
		total_occurs = 1;
		do {
			if (p->flag_occurs
			 && p->occurs_max > 1) {
				total_occurs *= p->occurs_max;
			}
			p = p->parent;
		} while (p);
	}
	k = num_of_values - total_occurs;
	if (k > 0) {
		cb_error_x (CB_TREE (f),
			_("elements in VALUE clause for '%s' (%d) exceed max amount (%d)"),
			f->name, num_of_values, total_occurs);
		return 1;
	}
	return 0;
}

static void
validate_elem_value (struct cb_field * const f)
{
	/* check for table format VALUES [ARE] in non-occurs field */
	if (CB_LIST_P (f->values) && CB_TAB_VALS_P (CB_LIST (f->values)->value)) {
		const struct cb_field	*p = f;
		do {
			if (p->flag_occurs) {
				break;
			}
			p = p->parent;
		} while (p);
		if (!p) {
			const cb_tree		x = CB_TREE (f);
			const cb_tree		first_tabval = CB_LIST (f->values)->value;
			const struct cb_table_values *vals = CB_TAB_VALS (first_tabval);
			cb_error_x (x,
				_("unexpected VALUES ARE for elementary item"));
			/* ensure to have an expected "direct" value entry, relying
			   on now unreferenced parse trees to be freed later on */
			f->values = CB_LIST (vals->values)->value;
		}
	}

	/* ISO+IEC+1989-2002: 13.16.42.2-10 */
	if (get_warn_opt_value (cb_warn_ignored_initial_val) != COBC_WARN_DISABLED) {
		const cb_tree		x = CB_TREE (f);
		const struct cb_field	*p;
		for (p = f; p; p = p->parent) {
			if (p->flag_external) {
				cb_warning_x (cb_warn_ignored_initial_val, x,
					      _("initial VALUE clause ignored for %s item '%s'"),
					      "EXTERNAL", cb_name (CB_TREE(f)));
			} else if (p->redefines) {
				cb_warning_x (cb_warn_ignored_initial_val, x,
					      _("initial VALUE clause ignored for %s item '%s'"),
					      "REDEFINES", cb_name (CB_TREE(f)));
			}
		}
	}
}

static void
warn_full_on_numeric_items_is_useless (const struct cb_field * const f)
{
	if ((f->screen_flag & COB_SCREEN_FULL)
	 && f->pic && f->pic->category == CB_CATEGORY_NUMERIC) {
		cb_warning_x (cb_warn_additional, CB_TREE (f),
			_("FULL has no effect on numeric items; you may want REQUIRED or PIC Z"));
	}
}

static int
has_std_needed_screen_clause (const struct cb_field * const f)
{
	cb_tree first_value = get_first_value (f);
	return (f ->pic
	     && (f->screen_from
	      || f->screen_to
	      || (first_value && CB_NUMERIC_LITERAL_P (first_value))))
	     || (first_value
	      && (CB_LITERAL_P (first_value)
	       || CB_CONST_P   (first_value))
	      && (CB_TREE_CATEGORY (first_value) == CB_CATEGORY_ALPHANUMERIC
	       || CB_TREE_CATEGORY (first_value) == CB_CATEGORY_BOOLEAN
	       || CB_TREE_CATEGORY (first_value) == CB_CATEGORY_NATIONAL))
	     || f->screen_flag & COB_SCREEN_BELL
	     || f->screen_flag & COB_SCREEN_BLANK_LINE
	     || f->screen_flag & COB_SCREEN_BLANK_SCREEN
	     || f->screen_flag & COB_SCREEN_ERASE_EOL
	     || f->screen_flag & COB_SCREEN_ERASE_EOS;
}

static void
error_value_figurative_constant(const struct cb_field * const f)
{
	cb_tree first_value = get_first_value (f);
	if (first_value && cb_is_figurative_constant (first_value)) {
		cb_error_x (CB_TREE (f),
			_("VALUE may not contain a figurative constant"));
	}
}

static void
error_both_full_and_justified (const struct cb_field * const f)
{
	if ((f->screen_flag & COB_SCREEN_FULL) && f->flag_justified) {
		cb_error_x (CB_TREE (f),
			_("cannot specify both %s and %s"), "FULL", "JUSTIFIED");
	}
}

static int
warn_from_to_using_without_pic (const struct cb_field * const f)
{

	if ((f->screen_from || f->screen_to) && !f->pic) {
		const cb_tree	x = CB_TREE (f);
		/* TODO: Change to dialect option */
		cb_warning_x (cb_warn_additional, x,
			_("'%s' has FROM, TO or USING without PIC; PIC will be implied"),
			cb_name (x));
		/* TODO: Add setting of PIC below here or move warnings to the code which sets the PIC */
		return 1;
	} else {
		return 0;
	}
}

static int
warn_pic_for_numeric_value_implied (const struct cb_field * const f)
{
	cb_tree first_value = get_first_value (f);
	if (first_value && CB_NUMERIC_LITERAL_P (first_value)) {
		const cb_tree	x = CB_TREE (f);
		/* TODO: Change to dialect option */
		cb_warning_x (cb_warn_additional, x,
			_("'%s' has numeric VALUE without PIC; PIC will be implied"),
			cb_name (x));
		/* TODO: Add setting of PIC below here or move warnings to the code which sets the PIC */
		return 1;
	} else {
		return 0;
	}
}

static void
error_both_pic_and_value (const struct cb_field * const f)
{
	if (f->pic && f->values) {
		cb_error_x (CB_TREE (f), _("cannot specify both %s and %s"), "PIC", "VALUE");
	}
}

static void
error_pic_without_from_to_using (const struct cb_field * const f)
{
	if (f->pic && !(f->screen_from || f->screen_to)) {
		cb_error_x (CB_TREE (f), _("cannot have PIC without FROM, TO or USING"));
	}
}

static void
error_pic_for_numeric_value (const struct cb_field * const f)
{
	cb_tree first_value = get_first_value (f);
	if (first_value && CB_NUMERIC_LITERAL_P (first_value)) {
		cb_error_x (CB_TREE (f), _("cannot have numeric VALUE without PIC"));
	}
}

static void
error_from_to_using_without_pic (const struct cb_field * const f)
{
	/* TODO: Replace warning, like in validate_elem_screen_clauses_std? */
	if ((f->screen_from || f->screen_to) && !f->pic) {
		cb_error_x (CB_TREE (f), _("cannot have FROM, TO or USING without PIC"));
	}
}

static void
error_value_numeric (const struct cb_field * const f)
{
	cb_tree first_value = get_first_value (f);
	if (first_value
	 && CB_TREE_CATEGORY (first_value) == CB_CATEGORY_NUMERIC) {
		cb_error_x (CB_TREE (f), _("VALUE item may not be numeric"));
	}
}

static void
error_no_screen_clause_needed_by_xopen (const struct cb_field * const f)
{
	if (!(f->pic
	   || f->screen_column
	   || f->screen_flag & COB_SCREEN_BELL
	   || f->screen_flag & COB_SCREEN_BLANK_LINE
	   || f->screen_flag & COB_SCREEN_BLANK_SCREEN
	   || f->screen_line
	   || f->values)) {
		const cb_tree	x = CB_TREE (f);
		cb_error_x (x, _("'%s' needs a PIC, COL, LINE, VALUE, BELL or BLANK clause"),
			    cb_name (x));
	}
}

static void
validate_elem_screen_clauses_std (struct cb_field * const f)
{
	const cb_tree	x = CB_TREE (f);

	if (!has_std_needed_screen_clause (f)) {
		if (f->pic) {
			cb_error_x (x, _("'%s' cannot have PIC without FROM, TO, USING or numeric VALUE"),
				    cb_name (x));
		} else if (f->values) {
			/* TODO: Add setting of PIC below here or move warnings to the code which sets the PIC */
			error_pic_for_numeric_value (f);
		} else if (f->screen_from || f->screen_to) {
			error_from_to_using_without_pic (f);
		} else {
			cb_error_x (x, _("'%s' needs a PIC, FROM, TO, USING, VALUE, BELL, BLANK or ERASE clause"),
				    cb_name (x));
		}
	}

	error_both_full_and_justified (f);

	error_value_figurative_constant (f);
}

static void
validate_elem_screen_clauses_mf (const struct cb_field * const f)
{
	const cb_tree	x = CB_TREE (f);

	error_no_screen_clause_needed_by_xopen (f);

	error_both_pic_and_value (f);
	error_pic_without_from_to_using (f);

	/*
	  The below rule isn't explicitly stated, but it follows from the
	  PICTURE's general rule which says the PIC character string determines
	  the length and category of the item.
	*/
	warn_from_to_using_without_pic (f);

	error_value_figurative_constant (f);
	error_value_numeric (f);

	if (!f->screen_to
	  && ((f->screen_flag & COB_SCREEN_AUTO)
	   || (f->screen_flag & COB_SCREEN_FULL)
	   || (f->screen_flag & COB_SCREEN_PROMPT)
	   || (f->screen_flag & COB_SCREEN_REQUIRED)
	   || (f->screen_flag & COB_SCREEN_SECURE))) {
		cb_error_x (x, _("cannot use AUTO, FULL, PROMPT, REQUIRED or SECURE on elementary item without TO or USING"));
	}
	if (!f->screen_from && !f->screen_to
	  && (f->flag_blank_zero
	   || f->flag_justified
	   || f->flag_occurs
	   || f->flag_sign_clause)) {
		cb_error_x (x, _("cannot use BLANK WHEN ZERO, JUSTIFIED, OCCURS or SIGN on item without FROM, TO or USING"));
	}
}

static void
validate_elem_screen_clauses_rm (struct cb_field *f)
{
	error_both_pic_and_value (f);
	error_pic_without_from_to_using (f);
	error_from_to_using_without_pic (f);

	error_value_numeric (f);

	if (!f->pic) {
		const cb_tree	x = CB_TREE (f);
		if ((f->screen_flag & COB_SCREEN_AUTO)
		 || (f->screen_flag & COB_SCREEN_FULL)
		 || (f->screen_flag & COB_SCREEN_REQUIRED)
		 || (f->screen_flag & COB_SCREEN_SECURE)) {
			cb_error_x (x, _("cannot use AUTO, FULL, REQUIRED or SECURE on elementary item without FROM, TO or USING"));
		}
		if (f->flag_blank_zero
		 || f->flag_justified
		 || f->flag_sign_clause) {
			cb_error_x (x, _("cannot use BLANK WHEN ZERO, JUSTIFIED or SIGN without FROM, TO or USING"));
		}
	}
}

static void
validate_elem_screen_clauses_acu (struct cb_field *f)
{
	error_both_pic_and_value (f);
	error_pic_without_from_to_using (f);

	error_value_numeric (f);

	warn_from_to_using_without_pic (f);
	if (!f->pic) {
		const cb_tree	x = CB_TREE (f);
		if (f->flag_blank_zero) {
			cb_error_x (x, _("cannot have BLANK WHEN ZERO without PIC"));
		}
		if (f->flag_justified) {
			cb_error_x (x, _("cannot have JUSTIFIED without PIC"));
		}
	}
}

static void
validate_elem_screen_clauses_xopen (struct cb_field *f)
{
	const cb_tree	x = CB_TREE (f);

	error_no_screen_clause_needed_by_xopen (f);

	error_both_pic_and_value (f);
	error_pic_without_from_to_using (f);
	error_from_to_using_without_pic (f);

	error_value_numeric (f);

	if (!f->screen_to && !f->screen_from
	 && (f->screen_flag & COB_SCREEN_AUTO)) {
		cb_error_x (x, _("cannot have AUTO without FROM, TO or USING"));
	}
	if (!f->screen_to
	  && ((f->screen_flag & COB_SCREEN_FULL)
	   || (f->screen_flag & COB_SCREEN_REQUIRED))) {
		cb_error_x (x, _("cannot use FULL or REQUIRED on item without TO or USING"));
	}

	error_both_full_and_justified (f);

	if ((f->screen_flag & COB_SCREEN_SECURE)) {
		if (f->screen_from) {
			cb_error_x (x, _("SECURE can be used with TO only"));
		} else if (!f->screen_to) {
			cb_error_x (x, _("SECURE must be used with TO"));
		}
	}
}

static void
warn_has_no_useful_clause (const struct cb_field * const f)
{
	if (!(  f->screen_column
	     || f->screen_flag & COB_SCREEN_BELL
	     || f->screen_flag & COB_SCREEN_BLANK_LINE
	     || f->screen_flag & COB_SCREEN_BLANK_SCREEN
	     || f->screen_flag & COB_SCREEN_ERASE_EOL
	     || f->screen_flag & COB_SCREEN_ERASE_EOS
	     || f->screen_from
	     || f->screen_line
	     || f->screen_to
	     || f->values)) {
		cb_warning_x (COBC_WARN_FILLER, CB_TREE (f),
			      _("'%s' does nothing"), cb_name (CB_TREE (f)));
	}
}

static void
validate_elem_screen_clauses_gc (const struct cb_field * const f)
{
	/* We aim for the least restrictive rules possible. */
	warn_has_no_useful_clause (f);
	warn_from_to_using_without_pic (f);
	warn_pic_for_numeric_value_implied (f);
}

static void
validate_elem_screen (struct cb_field *f)
{
	switch (cb_screen_section_clauses) {
	case CB_STD_SCREEN_RULES:
		validate_elem_screen_clauses_std (f);
		break;
	case CB_MF_SCREEN_RULES:
		validate_elem_screen_clauses_mf (f);
		break;
	case CB_ACU_SCREEN_RULES:
		validate_elem_screen_clauses_acu (f);
		break;
	case CB_RM_SCREEN_RULES:
		validate_elem_screen_clauses_rm (f);
		break;
	case CB_XOPEN_SCREEN_RULES:
		validate_elem_screen_clauses_xopen (f);
		break;
	case CB_GC_SCREEN_RULES:
		validate_elem_screen_clauses_gc (f);
		break;
	}

	warn_full_on_numeric_items_is_useless (f);
}

/* Perform validation of a non-66-or-88-level elementary item.
   note: this actually adjusts the field ! */
static unsigned int
validate_elementary_item (struct cb_field *f)
{
	unsigned int	ret;

	ret = validate_usage (f);
	if (f->flag_sign_clause) {
		validate_sign (f);
	}
	validate_justified_right (f);

	if (f->flag_blank_zero) {
		validate_blank_when_zero (f);
	}

	if (f->values) {
		validate_elem_value (f);
		if (CB_LIST_P (f->values)) {
			ret |= validate_multi_value (f);
		}
	}
	if (!ret && f->storage == CB_STORAGE_SCREEN) {
		validate_elem_screen (f);
	}

	/* Validate PICTURE (adjusts the field if an implicit PIC is created) */
	ret |= validate_pic (f);

	/* TODO: This is not validation and should be elsewhere. */
	switch (f->usage) {
	case CB_USAGE_DISPLAY:
		if (current_program
		 && current_program->flag_trailing_separate
		 && f->pic
		 && f->pic->category == CB_CATEGORY_NUMERIC
		 && !f->flag_sign_leading) {
			f->flag_sign_separate = 1;
		}
		break;
	case CB_USAGE_SIGNED_CHAR:
		f->usage = CB_USAGE_COMP_5;
		f->pic = cb_build_binary_picture ("BINARY-CHAR", 2, 1);
		f->flag_real_binary = 1;
		break;
	case CB_USAGE_SIGNED_SHORT:
		f->usage = CB_USAGE_COMP_5;
		f->pic = cb_build_binary_picture ("BINARY-SHORT", 4, 1);
		f->flag_real_binary = 1;
		break;
	case CB_USAGE_SIGNED_INT:
		f->usage = CB_USAGE_COMP_5;
		f->pic = cb_build_binary_picture ("BINARY-LONG", 9, 1);
		f->flag_real_binary = 1;
		break;
	case CB_USAGE_SIGNED_LONG:
		f->usage = CB_USAGE_COMP_5;
		f->pic = cb_build_binary_picture ("BINARY-DOUBLE", 18, 1);
		f->flag_real_binary = 1;
		break;
	case CB_USAGE_UNSIGNED_CHAR:
		f->usage = CB_USAGE_COMP_5;
		f->pic = cb_build_binary_picture ("BINARY-CHAR", 2, 0);
		f->flag_real_binary = 1;
		break;
	case CB_USAGE_UNSIGNED_SHORT:
		f->usage = CB_USAGE_COMP_5;
		f->pic = cb_build_binary_picture ("BINARY-SHORT", 4, 0);
		f->flag_real_binary = 1;
		break;
	case CB_USAGE_UNSIGNED_INT:
		f->usage = CB_USAGE_COMP_5;
		f->pic = cb_build_binary_picture ("BINARY-LONG", 9, 0);
		f->flag_real_binary = 1;
		break;
	case CB_USAGE_POINTER:
		if (cb_numeric_pointer) {
			f->pic = cb_build_binary_picture ("BINARY-DOUBLE", 18, 0);
			f->flag_real_binary = 1;
		}
		break;
	case CB_USAGE_UNSIGNED_LONG:
		f->usage = CB_USAGE_COMP_5;
		f->pic = cb_build_binary_picture ("BINARY-DOUBLE", 18, 0);
		f->flag_real_binary = 1;
		break;
	case CB_USAGE_COMP_5:
		f->flag_real_binary = 1;
		break;
	default:
		break;
	}

	/* TODO: Also move, this is not validation */
	if (f->flag_blank_zero
	 && f->pic
	 && f->pic->category == CB_CATEGORY_NUMERIC) {
		cob_pic_symbol	*pstr;
		int		n;
		/* Reconstruct the picture string */
		if (f->pic->scale > 0) {
			/* Size for genned string */
			if (f->pic->have_sign) {
				n = 4;
			} else {
				n = 3;
			}
			f->pic->str = cobc_parse_malloc ((size_t)n * sizeof (cob_pic_symbol));
			pstr = f->pic->str;
			if (f->pic->have_sign) {
				pstr->symbol = '+';
				pstr->times_repeated = 1;
				++pstr;
			}
			pstr->symbol = '9';
			pstr->times_repeated = (int)f->pic->digits - f->pic->scale;
			++pstr;
			pstr->symbol = 'V';
			pstr->times_repeated = 1;
			++pstr;

			pstr->symbol = '9';
			pstr->times_repeated = f->pic->scale;
			++pstr;

			f->pic->size++;
		} else {
			/* Size for genned string */
			if (f->pic->have_sign) {
				n = 2;
			} else {
				n = 1;
		}
			f->pic->str = cobc_parse_malloc ((size_t)n * sizeof(cob_pic_symbol));
			pstr = f->pic->str;
			if (f->pic->have_sign) {
				pstr->symbol = '+';
				pstr->times_repeated = 1;
				++pstr;
		}
			pstr->symbol = '9';
			pstr->times_repeated = f->pic->digits;
		}
		f->pic->lenstr = n;
		f->pic->category = CB_CATEGORY_NUMERIC_EDITED;
	}

	return ret;
}

static unsigned int
validate_field_1 (struct cb_field *f)
{
	cb_tree		x;
	int			sts = 0;

#if 0
	/* LCOV_EXCL_START */
	if (unlikely (!f)) {	/* checked to keep the analyzer happy */
		cobc_err_msg (_("call to %s with NULL pointer"), "validate_field_1");
		COBC_ABORT();
	}
	/* LCOV_EXCL_STOP */
#endif

	if (f->flag_invalid) {
			return 1;
		}
			 
	x = CB_TREE (f);
	if (f->level == 77) {
		if (f->storage != CB_STORAGE_WORKING
		 && f->storage != CB_STORAGE_LOCAL
		 && f->storage != CB_STORAGE_LINKAGE) {
			cb_error_x (x, _("'%s' 77 level is not allowed here"), cb_name (x));
		}
	}

	if (f->flag_any_length) {
		return validate_any_length_item (f);
	}

	if (f->flag_external) {
		validate_external (f);
	} else
	if (f->flag_item_based) {
		validate_based (f);
	}

	if (f->flag_occurs) {
		/* TODO: Not validation, so should not be in this function! */
		cb_tree		l;
		for (l = f->index_list; l; l = CB_CHAIN (l)) {
			CB_FIELD_PTR (CB_VALUE (l))->flag_is_global = f->flag_is_global;
		}
		/* END: Not validation */
		validate_occurs (f);
	}

	if (f->level == 66) {
		/* no check for redefines here */
		return 0;
	}
	if (f->redefines) {
		/* CHECKME - seems to be missing:
		   COBOL 202x doesn't allow REDEFINES in SCREEN/REPORT */
		validate_redefines (f);
	}
	
	if (f->storage == CB_STORAGE_REPORT) {
		if (f->report_num_col > 1) {
			if (f->flag_occurs) {
				cb_error_x (CB_TREE (f), _("OCCURS and multi COLUMNs is not allowed"));
			} else {
				/* FIXME: this is not a "real" validation*/
				f->occurs_max = f->occurs_min = f->report_num_col;
				f->flag_occurs = 1;
				f->indexes = 1;
			}
		}
		if ((f->report_flag & COB_REPORT_LINE)
		 && !(f->report_flag & COB_REPORT_LINE_PLUS)
		 && f->parent
		 && f->parent->children != f) {
			/* check all _previous_ definitions for a LINE clause,
			   if it is there then drop the LINE clause of this field */
			struct cb_field	*c;
			for (c = f->parent->children; c && c != f; c = c->sister) {
				if ((c->report_flag & COB_REPORT_LINE)
				 && !(c->report_flag & COB_REPORT_LINE_PLUS)
				 && c->report_line == f->report_line) {
					cb_warning_x (cb_warn_additional, CB_TREE (f),
						_("duplicate LINE %d ignored"), f->report_line);
					f->report_line = 0;
					f->report_flag &= ~COB_REPORT_LINE;
					break;
				}
			}
		}
	}

	if (f->children) {
		sts = validate_group (f);
	} else {
		sts = validate_elementary_item (f);
	}
	return sts;
}

static void
setup_parameters (struct cb_field *f)
{
	if (f->children) {
		/* Group field */
		unsigned int	flag_local = !!f->flag_local;
		for (f = f->children; f; f = f->sister) {
			f->flag_local = flag_local;
			setup_parameters (f);
		}
		return;
	}

	/* Regular field */
	/* Determine the class */
	switch (f->usage) {
	case CB_USAGE_BINARY:
#ifndef WORDS_BIGENDIAN
		if (cb_binary_byteorder == CB_BYTEORDER_BIG_ENDIAN) {
			f->flag_binary_swap = 1;
		}
#endif
		break;

	case CB_USAGE_INDEX:
	case CB_USAGE_HNDL:
	case CB_USAGE_HNDL_WINDOW:
	case CB_USAGE_HNDL_SUBWINDOW:
	case CB_USAGE_HNDL_FONT:
	case CB_USAGE_HNDL_THREAD:
	case CB_USAGE_HNDL_MENU:
	case CB_USAGE_HNDL_VARIANT:
	case CB_USAGE_HNDL_LM:
		f->pic = cb_build_picture ("S9(9)");
		f->pic->flag_is_calculated = 1;
#if 0
		/* REMIND: The category should be set, but doing so causes
		 * other problems as more checks need to be added to
		 * accept a category of CB_CATEGORY_INDEX so this change
		 * is deferred until a later time
		 * RJN: Nov 2017
		*/
		f->pic->category = CB_CATEGORY_INDEX;
#endif
		break;

	case CB_USAGE_LENGTH:
		f->pic = cb_build_picture ("9(9)");
		f->pic->flag_is_calculated = 1;
		break;

	case CB_USAGE_POINTER:
	case CB_USAGE_PROGRAM_POINTER:
#ifdef COB_64_BIT_POINTER
		f->pic = cb_build_picture ("9(17)");
#else
		f->pic = cb_build_picture ("9(10)");
#endif
		f->pic->flag_is_calculated = 1;
		break;
	case CB_USAGE_FLOAT:
		f->pic = cb_build_picture ("S9(7)V9(8)");
		f->pic->flag_is_calculated = 1;
		break;
	case CB_USAGE_LONG_DOUBLE:
		f->pic = cb_build_picture ("S9(19)V9(19)");
		f->pic->flag_is_calculated = 1;
		break;
	case CB_USAGE_DOUBLE:
		f->pic = cb_build_picture ("S9(17)V9(17)");
		f->pic->flag_is_calculated = 1;
		break;
	case CB_USAGE_FP_DEC64:
		/* RXWRXW - Scale Fix me */
		f->pic = cb_build_picture ("S9(17)V9(16)");
		f->pic->flag_is_calculated = 1;
		break;
	case CB_USAGE_FP_DEC128:
		/* RXWRXW - Scale Fix me */
		f->pic = cb_build_picture ("S999V9(34)");
		f->pic->flag_is_calculated = 1;
		break;

	case CB_USAGE_COMP_5:
		f->flag_real_binary = 1;
		/* Fall-through */
	case CB_USAGE_COMP_X:
	case CB_USAGE_COMP_N:
		if (f->pic->category == CB_CATEGORY_ALPHANUMERIC) {
			if (f->pic->size > 8) {
				f->pic = cb_build_picture ("9(36)");
			} else {
				char		pic[8];
				sprintf (pic, "9(%u)", pic_digits[f->pic->size - 1]);
				f->pic = cb_build_picture (pic);
			}
		}
#ifndef WORDS_BIGENDIAN
		if (f->usage == CB_USAGE_COMP_X &&
			cb_binary_byteorder == CB_BYTEORDER_BIG_ENDIAN) {
			f->flag_binary_swap = 1;
		}
		if (f->usage == CB_USAGE_COMP_N) {
			f->flag_binary_swap = 1;
		}
#endif
		break;

	case CB_USAGE_DISPLAY:
		/* in case of usage display we often don't have the category
		   setup correctly, work around this explicit resolving it here */
		cb_tree_category (CB_TREE (f));
		break;

	default:
		break;
	}
}

static void
compute_binary_size (struct cb_field *f, const int size)
{
	switch (cb_binary_size) {
	case CB_BINARY_SIZE_1_2_4_8:
		f->size = ((size <= 2) ? 1 :
			   (size <= 4) ? 2 :
			   (size <= 9) ? 4 : (size <= 18) ? 8 : 16);
		return;
	case CB_BINARY_SIZE_2_4_8:
		if (f->flag_real_binary && size <= 2) {
			f->size = 1;
		} else {
			f->size = ((size <= 4) ? 2 :
				   (size <= 9) ? 4 : (size <= 18) ? 8 : 16);
		}
		return;
	case CB_BINARY_SIZE_1__8:
		if (f->pic->have_sign) {
			switch (size) {
			case 0:
			case 1:
			case 2:
				f->size = 1;
				return;
			case 3:
			case 4:
				f->size = 2;
				return;
			case 5:
			case 6:
				f->size = 3;
				return;
			case 7:
			case 8:
			case 9:
				f->size = 4;
				return;
			case 10:
			case 11:
				f->size = 5;
				return;
			case 12:
			case 13:
			case 14:
				f->size = 6;
				return;
			case 15:
			case 16:
				f->size = 7;
				return;
			case 17:
			case 18:
				f->size = 8;
				return;
			case 19:
			case 20:
			case 21:
				f->size = 9;
				return;
			case 22:
			case 23:
				f->size = 10;
				return;
			case 24:
			case 25:
			case 26:
				f->size = 11;
				return;
			case 27:
			case 28:
				f->size = 12;
				return;
			case 29:
			case 30:
			case 31:
				f->size = 13;
				return;
			case 32:
			case 33:
				f->size = 14;
				return;
			case 34:
			case 35:
				f->size = 15;
				return;
			default:
				f->size = 16;
				return;
			}
		}
		switch (size) {
		case 0:
		case 1:
		case 2:
			f->size = 1;
			return;
		case 3:
		case 4:
			f->size = 2;
			return;
		case 5:
		case 6:
		case 7:
			f->size = 3;
			return;
		case 8:
		case 9:
			f->size = 4;
			return;
		case 10:
		case 11:
		case 12:
			f->size = 5;
			return;
		case 13:
		case 14:
			f->size = 6;
			return;
		case 15:
		case 16:
			f->size = 7;
			return;
		case 17:
		case 18:
		case 19:
			f->size = 8;
			return;
		case 20:
		case 21:
			f->size = 9;
			return;
		case 22:
		case 23:
		case 24:
			f->size = 10;
			return;
		case 25:
		case 26:
			f->size = 11;
			return;
		case 27:
		case 28:
			f->size = 12;
			return;
		case 29:
		case 30:
		case 31:
			f->size = 13;
			return;
		case 32:
		case 33:
			f->size = 14;
			return;
		case 34:
		case 35:
		case 36:
			f->size = 15;
			return;
		default:
			f->size = 16;
			return;
		}
		return;
#if 0	/* how should this happen ... */
	default:
		f->size = size;
		return;
#endif
	}
}

static struct cb_field *
get_last_child (struct cb_field *f)
{
	while (f->children) {
		f = f->children;
		while (f->sister) {
			f = f->sister;
		}
	}
	return f;
}

static void
set_report_field_offset (struct cb_field *f)
{
	struct cb_field *pp;

#if 0 /* That would be a bad error as this function is only called for report_column > 0 */
	if (f->storage != CB_STORAGE_REPORT) {
		return;
	}
#endif
	if (!(f->report_flag & COB_REPORT_COLUMN_PLUS)) {
		f->offset = f->report_column - 1;		/* offset based on COLUMN value */
		return;
	}
	pp = f->parent;
	if (pp) {
		if (pp->children == f) {
			f->offset = f->report_column - 1;	/* First in line */
		} else {
			struct cb_field *c;
			for (c = pp->children; c; c = c->sister) {	/* Find previous field */
				if (c->sister == f) {
					if (c->occurs_max > 1) {
						f->offset = c->offset + c->size * c->occurs_max + f->report_column;
					}
					else {
						f->offset = c->offset + c->size + f->report_column;
					}
					break;
				}
			}
		}
	}
}

/* get highest numeric integer value that may be stored in field 'f',
   everything after the decimal point is stripped, non-numeric and float
   data return 0 */
static int
get_max_int_val (struct cb_field *f)
{
	int max_val;

	switch (f->usage) {
	case CB_USAGE_BINARY:
	case CB_USAGE_COMP_5:
	case CB_USAGE_COMP_X:
	case CB_USAGE_COMP_N:
	case CB_USAGE_INDEX:
		if (f->flag_real_binary || !cb_binary_truncate) {
			max_val = f->size * 8;
			if (f->pic->have_sign) {
				max_val--;
			}
			max_val = cob_s32_pow (2, max_val);
			break;
		}
		/* Fall-through */
	case CB_USAGE_DISPLAY:
		if (CB_TREE_CATEGORY (f) != CB_CATEGORY_NUMERIC
		 && CB_TREE_CATEGORY (f) != CB_CATEGORY_NUMERIC_EDITED) {
			return 0;
		}
		/* Fall-through */
	case CB_USAGE_PACKED:
	case CB_USAGE_COMP_6:
		max_val = cob_s32_pow (10, f->pic->digits) - 1;
		break;
	default:
		return 0;
	}

	if (f->pic->scale > 0) {
		return max_val / cob_s32_pow (10, f->pic->scale);
	}
	if (f->pic->scale < 0) {
		return max_val * cob_s32_pow (10, -f->pic->scale);
	}
	return max_val;
}

/* computes this field's and its children's size and offset */
static int
compute_size (struct cb_field *f)
{
	const int max_size = f->storage == CB_STORAGE_LINKAGE
		? COB_MAX_FIELD_SIZE_LINKAGE
		: COB_MAX_FIELD_SIZE;

	if (f->level == 66) {	/* RENAMES */
		if (f->rename_thru) {
			f->size = f->rename_thru->offset
			        + f->rename_thru->size - f->redefines->offset;
		} else if (f->redefines) {
#if 0	/* FIXME: redefine loop, possibly also below */
			if (f->redefines->size == 0) {
				f->size = compute_size (f->redefines);
			} else {
				f->size = f->redefines->size;
			}
#else
			f->size = f->redefines->size;
#endif
		} else {
			f->size = 1;	/* error case: invalid REDEFINES */
		}
		return f->size;
	}

	/* early exit if we're already calculated as "too big" */
	if (f->size == max_size + 1) {
		return f->size;
	}

	if (f->children) {
		cob_s64_t		size_check = 0;
		int		align_size;
		int		pad;
		int		unbounded_items = 0;
		int		unbounded_parts = 1;

		struct cb_field	*c;
		if (f->storage == CB_STORAGE_REPORT
		 && (f->report_flag & COB_REPORT_LINE) ) {
			f->offset = 0;
		}

		/* Groups */
		if (f->flag_synchronized) {
			/* TODO: handle this "per dialect", some disallow this (per ANSI85) or ignore it */
			cb_warning_x (cb_warn_additional, CB_TREE (f),
				_("ignoring SYNCHRONIZED for group item '%s'"),
				cb_name (CB_TREE (f)));
		}
unbounded_again:
		size_check = 0;
		occur_align_size = 1;
		for (c = f->children; c; c = c->sister) {
			if (c->redefines) {
				c->offset = c->redefines->offset;
				compute_size (c);
				/* Increase the size if redefinition is larger */
				if (c->level != 66
				 && c->size * c->occurs_max >
				    c->redefines->size * c->redefines->occurs_max) {
					if (cb_verify_x (CB_TREE (c), cb_larger_redefines, _("larger REDEFINES"))) {
						struct cb_field *c0;
						int		maxsz = c->redefines->size * c->redefines->occurs_max;
						for (c0 = c->redefines->sister; c0 != c; c0 = c0->sister) {
							if (c0->size * c0->occurs_max > maxsz) {
								maxsz = c0->size * c0->occurs_max;
							}
						}
						if (c->size * c->occurs_max > maxsz) {
							size_check += ((cob_s64_t)c->size * c->occurs_max) - maxsz;
						}
					}
					if (cb_larger_redefines >= CB_WARNING) {
						cb_note_x (cb_warn_dialect, CB_TREE (c),
							_("size of '%s' larger than size of '%s'"),
							c->name, c->redefines->name);
					}
				}
			} else {
				c->offset = f->offset + (int) size_check;
				compute_size (c);
				if (c->flag_unbounded && CB_VALID_TREE (c->depending)) {
					cb_tree dep = cb_ref (c->depending);
					if (CB_FIELD_P (dep)) {
						const int max_odo_value = get_max_int_val (CB_FIELD (dep));
						unbounded_items++;
						/* computed MAX */
						{
							/* size above the field  [there is no sister for UNBOUNDED]*/
							cob_s64_t size_above = 0;
							struct cb_field *curr_fld = c;
							struct cb_field *p_fld = f;
							while (p_fld) {
								struct cb_field *p_fld_c;
								for (p_fld_c = p_fld->children; p_fld_c != curr_fld; p_fld_c = p_fld_c->sister) {
									if (p_fld_c->size == 0) {
										compute_size (p_fld_c);
									}
									size_above += p_fld_c->size;
								}
								curr_fld = p_fld;
								p_fld = p_fld->parent;
							}
							/* calculated size */
							c->occurs_max = (   (COB_MAX_UNBOUNDED_SIZE - size_above)
							                  / (c->size * unbounded_parts)
							                ) - 1;
							/* maximum possible in ODO field */
							if (max_odo_value != 0
							 && max_odo_value < c->occurs_max) {
								c->occurs_max = max_odo_value;
							}
						}

					} else {
						c->depending = cb_error_node;
					}
				}
				size_check += (cob_s64_t)c->size * c->occurs_max;

				if (c->report_column > 0) { 		/* offset based on COLUMN value */
					set_report_field_offset(c);
				}

				/* Word alignment */
				if (c->flag_synchronized) {
					align_size = 1;
					switch (c->usage) {
					case CB_USAGE_BINARY:
					case CB_USAGE_COMP_5:
					case CB_USAGE_COMP_X:
					case CB_USAGE_COMP_N:
					case CB_USAGE_FLOAT:
					case CB_USAGE_DOUBLE:
					case CB_USAGE_LONG_DOUBLE:
					case CB_USAGE_FP_BIN32:
					case CB_USAGE_FP_BIN64:
					case CB_USAGE_FP_BIN128:
					case CB_USAGE_FP_DEC64:
					case CB_USAGE_FP_DEC128:
						if (c->size == 2
						 || c->size == 4
						 || c->size == 8
						 || c->size == 16) {
							align_size = c->size;
						}
						break;
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
						align_size = sizeof (int);
						break;
					case CB_USAGE_OBJECT:
					case CB_USAGE_POINTER:
					case CB_USAGE_PROGRAM_POINTER:
						align_size = sizeof (void *);
						break;
					default:
						break;
					}
					if (c->offset % align_size != 0) {
						pad = align_size - (c->offset % align_size);
						c->offset += pad;
						size_check += pad;
					}
					if (align_size > occur_align_size) {
						occur_align_size = align_size;
					}
				}
			}

			if (c->sister == NULL
			 && c->storage == CB_STORAGE_REPORT) {	/* To set parent size */
				cob_s64_t calc = (cob_s64_t)c->offset + c->size;
				if (calc > size_check)
					size_check = calc;
			}
		}

		/* Ensure items within OCCURS are aligned correctly. */
		if (f->occurs_max > 1
		 && occur_align_size > 1
		 && (size_check % occur_align_size) != 0) {
			pad = occur_align_size - (size_check % occur_align_size);
			size_check += pad;
			/*
			  Add padding to last item, which will be (partly)
			  responsible for misalignment. If the item is not SYNC,
			  we have no problem. If it is SYNC, then it has been
			  aligned on a smaller boundary than occur_align_size: a
			  2-, 4- or 8-byte boundary. The needed padding will
			  be a multiple of 2, 4 or 8 bytes, so adding extra
			  padding will not break its alignment.
			*/
			if (f->children) {
				get_last_child (f)->offset += pad;
			} else {
				/* TODO: add appropriate message (untranslated) */
				COBC_ABORT ();	/* LCOV_EXCL_LINE */
			}
		}
		/* size check for group items */
		if (unbounded_items) {
			if (size_check > COB_MAX_UNBOUNDED_SIZE) {
				/* Hack: run again for finding the correct max_occurs for unbounded items */
				if (unbounded_parts == 1 && unbounded_items != 1) {
					unbounded_parts = unbounded_items;
				} else {
					unbounded_parts++;
				}
				goto unbounded_again;
			}
		} else if (size_check > max_size) {
			cb_error_x (CB_TREE (f),
					_("'%s' cannot be larger than %d bytes"),
					f->name, max_size);
			size_check = max_size + 1;
		}
		if (size_check <= INT_MAX) {
			f->size = (int) size_check;
		} else {
			f->size = INT_MAX;
		}
	} else if (!f->flag_is_external_form) {
		int		size = 0;
		/* Elementary item */

		if (f->report_column > 0) { 		/* offset based on COLUMN value */
			set_report_field_offset (f);
		}

		switch (f->usage) {
		case CB_USAGE_COMP_X:
		case CB_USAGE_COMP_N:
			if (f->pic->category == CB_CATEGORY_ALPHANUMERIC) {
				break;
			}
			size = f->pic->size;
			f->size = ((size <= 2) ? 1 : (size <= 4) ? 2 :
				   (size <= 7) ? 3 : (size <= 9) ? 4 :
				   (size <= 12) ? 5 : (size <= 14) ? 6 :
				   (size <= 16) ? 7 : (size <= 19) ? 8 :
				   (size <= 21) ? 9 : (size <= 24) ? 10 :
				   (size <= 26) ? 11 : (size <= 28) ? 12 :
				   (size <= 31) ? 13 : (size <= 33) ? 14 :
				   (size <= 36) ? 15 : 16);
			break;
		case CB_USAGE_BINARY:
		case CB_USAGE_COMP_5:
			size = f->pic->size;
#if	0	/* RXWRXW - Max binary */
			if (size > COB_MAX_BINARY) {
				f->flag_binary_swap = 0;
				size = 38;
				cb_error_x (CB_TREE (f),
					    _("'%s' binary field cannot be larger than %d digits"),
					    f->name, COB_MAX_BINARY);
			}
#else
			if (size > 18) {
				f->flag_binary_swap = 0;
				size = 18;
				cb_error_x (CB_TREE (f),
					    _("'%s' binary field cannot be larger than %d digits"),
					    f->name, 18);
			}
#endif
			compute_binary_size (f, size);
			break;
		case CB_USAGE_ERROR:
			if (f->pic == NULL) {
				/* should only happen for fields where we already raised
				   an error and could not create an implied PICTURE either */
				f->size = 1;
				break;
			}
			/* Fall-through */
		case CB_USAGE_DISPLAY:
#if 0	/* should be always available here */
			if (f->pic == NULL) {
				break;
			}
#endif
			/* boolean items without USAGE BIT */
			if (f->pic->category == CB_CATEGORY_BOOLEAN) {
				f->size = f->pic->size / 8;
				if (f->pic->size % 8 != 0) {
					f->size++;
				}
				break;
			}
			f->size = f->pic->size;
			if (f->pic->have_sign && f->flag_sign_separate) {
				f->size++;
			}
			/* note: size check for single items > INT_MAX done in tree.c */
			if (f->size > max_size) {
				cb_error_x (CB_TREE (f),
						_("'%s' cannot be larger than %d bytes"),
						f->name, max_size);
				f->size = max_size + 1;
			}
			break;
		case CB_USAGE_NATIONAL:
#if 0	/* should be always available here */
			if (f->pic == NULL) {
				break;
			}
#endif
			f->size = f->pic->size * COB_NATIONAL_SIZE;
			break;
		case CB_USAGE_PACKED:
#if 0	/* should be always available here */
			if (f->pic == NULL) {
				break;
			}
#endif
			f->size = f->pic->size / 2 + 1;
			break;
		case CB_USAGE_COMP_6:
#if 0	/* should be always available here */
			if (f->pic == NULL) {
				break;
			}
#endif
			f->size = (f->pic->size + 1) / 2;
			break;
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
			f->size = sizeof (int);
			break;
		case CB_USAGE_FLOAT:
			f->size = sizeof (float);
			break;
		case CB_USAGE_DOUBLE:
			f->size = sizeof (double);
			break;
		case CB_USAGE_LONG_DOUBLE:
			f->size = 16;	/* sizeof (long double) */
			break;
		case CB_USAGE_FP_BIN32:
			f->size = 4;
			break;
		case CB_USAGE_FP_BIN64:
		case CB_USAGE_FP_DEC64:
			f->size = 8;
			break;
		case CB_USAGE_FP_BIN128:
		case CB_USAGE_FP_DEC128:
			f->size = 16;
			break;
		case CB_USAGE_OBJECT:
		case CB_USAGE_POINTER:
		case CB_USAGE_PROGRAM_POINTER:
			f->size = sizeof (void *);
			break;
		case CB_USAGE_BIT:
			/* note: similar is found in DISPLAY */
			f->size = f->pic->size / 8;
			if (f->pic->size % 8 != 0) {
				f->size++;
			}
			break;
		/* LCOV_EXCL_START */
		default:
			/* as this is an unexpected message, only for requested reports,
			   leave untranslated */
			cobc_err_msg ("unexpected USAGE: %d", (int)f->usage);
			COBC_ABORT ();
		/* LCOV_EXCL_STOP */
		}
	}

	/* COBOL standard: The size of redefining field should not be larger
	   than the size of redefined field unless the redefined field is
	   level 01 and non-external */
	if (f->level == 1 && f->redefines
	 && (f->size * f->occurs_max > f->redefines->size * f->redefines->occurs_max)) {
		/* note: when allowed the redefined field is NOT size-adjusted here */
		if (f->redefines->flag_external) {
			if (!cb_verify_x (CB_TREE (f), cb_larger_redefines, _("larger REDEFINES"))
			 || cb_larger_redefines == CB_WARNING) {
				cb_note_x (cb_warn_dialect, CB_TREE (f),
					_("size of '%s' larger than size of '%s'"),
					f->name, f->redefines->name);
			}
		} else {
			cb_warning_x (cb_warn_larger_01_redefines, CB_TREE (f),
				_("size of '%s' larger than size of '%s'"),
				f->name, f->redefines->name);
		}
	}

	return f->size;
}

/* Check for default-matching VALUE, and depending on "default init"
   replace by cb_zero/cb_space and return 1 if the value matches the default. */
static int
cleanup_field_value (struct cb_field* f, cb_tree *val)
{
	switch (CB_TREE_CATEGORY (f)) {
	case CB_CATEGORY_NUMERIC:
		if (CB_LITERAL_P (*val)) {
			const struct cb_literal* lit = CB_LITERAL (*val);
			char *p = (char*)lit->data;
			char *end = p + lit->size - 1;
			/* note: literal data has no sign or decimal period any more */
			if (*end == '0') {
				while (p < end && *p == '0') p++;
				if (p == end) *val = cb_zero;
			}
		}
		if (*val == cb_zero
		 && !f->flag_internal_register
		 && cb_default_byte == CB_DEFAULT_BYTE_INIT
		 && ( f->storage == CB_STORAGE_WORKING
		   || f->storage == CB_STORAGE_LOCAL)
		 && !f->flag_sign_separate) {
			return 1;
		}
		break;
	case CB_CATEGORY_NATIONAL:
		if (CB_LITERAL_P (*val)) {
			const struct cb_literal *lit = CB_LITERAL (*val);
			char *p = (char*)lit->data;
			char *end = p + lit->size - 1;
			if (lit->size % COB_NATIONAL_SIZE != 0) {
				break;
			}
			if (*end == ' ') {
				while (p < end && p[0] == 0x00 && p[1] == ' ') p += 2;
				if (p == end) *val = cb_space;
			}
		}
		if (*val == cb_space
		 && !f->flag_internal_register
		 && ( cb_default_byte == CB_DEFAULT_BYTE_INIT)
		 && ( f->storage == CB_STORAGE_WORKING
		   || f->storage == CB_STORAGE_LOCAL)
		 && !f->children) {
			return 1;
		}
		break;
	case CB_CATEGORY_ALPHANUMERIC:
		if (CB_LITERAL_P (*val)) {
			const struct cb_literal *lit = CB_LITERAL (*val);
			char *p = (char*)lit->data;
			char *end = p + lit->size - 1;
			if (*end == ' ') {
				while (p < end && *p == ' ') p++;
				if (p == end) *val = cb_space;
			}
		}
		if (*val == cb_space
		 && !f->flag_internal_register
		 && ( cb_default_byte == CB_DEFAULT_BYTE_INIT
		   || cb_default_byte == ' ')
		 && ( f->storage == CB_STORAGE_WORKING
		   || f->storage == CB_STORAGE_LOCAL)
		 && !f->children) {
			return 1;
		}
		break;
	default:
		break;
	}

	return 0;
}

/* validate list of simple VALUEs */
static int
validate_field_value_list (cb_tree values, struct cb_field* f)
{
	int ret = 0;
	int ret_single;
	int no_relevant_value = 1;
	for (; values; values = CB_CHAIN (values)) {
		cb_tree value = CB_VALUE (values);
		ret_single = validate_move (value, CB_TREE (f), 1, NULL);
		if (!ret_single) {
			/* possible cleanup for value */
			if (!cleanup_field_value (f, &value)) {
				no_relevant_value = 0;
			}
		} else {
			ret++;
		}
	}
	if (no_relevant_value
	 && !ret) {
#if 0 /* this idea was nice, but we need a hint for
         INITIALIZE BY VALUE, so don't drop the value; otherwise
         fails "752. run_misc.at:10849: dump feature with NULL address ..." */
		/* if deemed that no value is necessary: drop that completely */
		f->values = NULL;
#endif
	}
	return ret;
}

/* validate if field's VALUE clause matches
   the necesary rules;
   remove VALUE clause if it matches the default setting
   to improve codegen
   recursively called for the field's sub-fields */
static int
validate_field_value (struct cb_field *f)
{
	int ret = 0;
	if (f->values) {
		if (f->flag_picture_l) {
			cb_error_x (CB_TREE (f),
				    _("%s and %s are mutually exclusive"),
				    _("variable-length PICTURE"), "VALUE");
			f->values = NULL;
			return 1;
		} else {
			cb_tree x = f->values;
			int ret_single = 0;
			if (!CB_LIST_P (x)) {
				/* simple, single VALUE */
				ret_single = validate_move (x, CB_TREE (f), 1, NULL);
				if (!ret_single) {
					/* possible cleanup for value */
					if (cleanup_field_value (f, &x)) {
#if 0 /* this idea was nice, but we need a hint for
         INITIALIZE BY VALUE, so don't drop the value; otherwise
         fails "752. run_misc.at:10849: dump feature with NULL address ..." */
						/* if deemed that no value is necessary: drop that completely */
						f->values = NULL;
#endif
					}
				} else {
					ret++;
				}

			} else if (!CB_TAB_VALS_P (CB_VALUE (x))) {
				/* list of simple VALUEs */ 
				ret += validate_field_value_list (x, f);
			} else {
				/* list of complex entries (table-format) for OCCURS */
				for (; x; x = CB_CHAIN (x)) {
					const struct cb_table_values *vals
						= CB_TAB_VALS (CB_VALUE (x));
					/* check for the value entry types */
					ret += validate_field_value_list (vals->values, f);
				}
			}
		}
	}

	if (f->children) {
		for (f = f->children; f; f = f->sister) {
			ret += validate_field_value (f);
		}
	}

	return ret;
}

void
cb_validate_field (struct cb_field *f)
{
	if (f->flag_is_verified) {
		return;
	}
	if (validate_field_1 (f) != 0) {
		f->flag_invalid = 1;
		return;
	}
	if (f->flag_item_78) {
		f->flag_is_verified = 1;
		return;
	}

	/* Set up parameters */
	if (f->storage == CB_STORAGE_LOCAL ||
	    f->storage == CB_STORAGE_LINKAGE ||
	    f->flag_item_based) {
		f->flag_local = 1;
	}
	if (f->storage == CB_STORAGE_LINKAGE || f->flag_item_based) {
		f->flag_base = 1;
	}
	setup_parameters (f);

	/* Compute size */
	occur_align_size = 1;
	compute_size (f);
	if (!f->redefines) {
		f->memory_size = f->size * f->occurs_max;
	} else if (f->redefines->memory_size < f->size * f->occurs_max) {
		f->redefines->memory_size = f->size * f->occurs_max;
	}

	if (!f->flag_internal_register) {
		/* don't check internal ones, these ought to be fine
		   and would otherwise be checked on each run of cobc */
		validate_field_value (f);
	}
	if (f->flag_is_global) {
		struct cb_field		*c;
#if 0 /* CHECKME: Why should we adjust the field count here? */
		f->count++;
		for (c = f->children; c; c = c->sister) {
			c->flag_is_global = 1;
			c->count++;
	}
#else
		for (c = f->children; c; c = c->sister) {
			c->flag_is_global = 1;
		}
#endif
	}

	f->flag_is_verified = 1;
}

void
cb_validate_88_item (struct cb_field *f)
{
	if (CB_VALID_TREE (f->parent)
	 && CB_TREE_CLASS (f->parent) == CB_CLASS_NUMERIC) {

		cb_tree l;

		for (l = f->values; l; l = CB_CHAIN (l)) {
			cb_tree x = CB_VALUE (l);
			/* for list A THRU C, X, Z we have another list */
			if (CB_LIST_P (x)) {
				x = CB_VALUE (x);
			}
			if (CB_TREE_CLASS (x) != CB_CLASS_NUMERIC) {
				if (CB_CONST_P (x)) x = CB_TREE (f);
				cb_error_x (x, _("literal type does not match numeric data type"));
			}
		}
	}
}

struct cb_field *
cb_validate_78_item (struct cb_field *f, const cob_u32_t no78add)
{
	cb_tree			x;
	cob_u32_t		noadd, prec;

	if (!f) {
		return last_real_field;
	}
	if (f->flag_internal_constant) {	/* Keep all internal CONSTANTs */
		prec = 1;
	} else if (f->flag_constant) {		/* 01 CONSTANT is verified in parser.y */
		prec = 1;
	} else {
		cb_verify (cb_constant_78, "78 VALUE");
		prec = 0;
	}

	if (CB_LIST_P (f->values) && cb_is_expr (f->values) ) {
		f->values = cb_evaluate_expr (f->values, prec);
	}

	x = CB_TREE (f);
	noadd = no78add;
	if (CB_INVALID_TREE (f->values)) {
		level_require_error (x, "VALUE");
		noadd = 1;
	} else if (CB_LIST_P (f->values)
	        && CB_INVALID_TREE (CB_VALUE (f->values))) {
		noadd = 1;
	}

	if (!noadd) {
		cb_add_78 (f);
	}
	return last_real_field;
}

static struct cb_field *
get_next_record_field (const struct cb_field *f)
{
	if (f->children) {
		return f->children;
	}

	while (f) {
		if (f->sister) {
			return f->sister;
		} else {
			f = f->parent;
		}
	}

	return NULL;
}

static int
error_if_rename_thru_is_before_redefines (const struct cb_field * const item)
{
	struct cb_field	*f = cb_field_founder (item->redefines);

	/*
	  Perform depth-first search on the record containing the RENAMES items.
	*/
	while (f) {
		/* Error if we find rename_thru before redefines */
		if (f == item->rename_thru) {
			cb_error_x (CB_TREE (item),
				    _("THRU item '%s' may not come before '%s'"),
				    cb_name (CB_TREE (item->rename_thru)),
				    cb_name (CB_TREE (item->redefines)));
			return 1;
		} else if (f == item->redefines) {
			return 0;
		}

		f = get_next_record_field (f);
	}

	return 0;
}

static int
error_if_is_or_in_occurs (const struct cb_field * const field,
			  const struct cb_field * const referring_field)
{
	struct cb_field *parent;
	int	ret = 0;

	if (field->flag_occurs) {
		cb_error_x (CB_TREE (referring_field),
			    _("RENAMES cannot start/end at the OCCURS item '%s'"),
			    cb_name (CB_TREE (field)));
		ret = 1;
	}

	for (parent = field->parent; parent; parent = parent->parent) {
		if (parent->flag_occurs) {
			cb_error_x (CB_TREE (referring_field),
				    _("cannot use RENAMES on part of the table '%s'"),
				    cb_name (CB_TREE (parent)));
			ret = 1;
		}
	}

	return ret;
}

static int
error_if_invalid_type_in_renames_range (const struct cb_field * const item)
{
	const struct cb_field	*end;
	const struct cb_field	*f = item->redefines;
	enum cb_category	category;
	int ret = 0;

	/* Find last item in RENAMES range */
	if (item->rename_thru) {
		if (item->rename_thru->children) {
			end = get_last_child (item->rename_thru);
		} else {
			end = item->rename_thru;
		}
	} else {
		end = item->redefines;
	}

	/*
	  Check all items are not pointers, object references or OCCURS
	  DEPENDING tables.
	*/
	while (f) {
		category = cb_tree_category (CB_TREE (f));
		if (category == CB_CATEGORY_OBJECT_REFERENCE
		 || category == CB_CATEGORY_DATA_POINTER
		 || category == CB_CATEGORY_PROGRAM_POINTER) {
			cb_error_x (CB_TREE (item),
				    _("RENAMES may not contain '%s' as it is a pointer or object reference"),
				    cb_name (CB_TREE (f)));
			ret = 1;
		} else if (f->depending) {
			cb_error_x (CB_TREE (item),
				    _("RENAMES may not contain '%s' as it is an OCCURS DEPENDING table"),
				    cb_name (CB_TREE (f)));
			ret = 1;
		}

		if (f == end) {
			break;
		} else {
			f = get_next_record_field (f);
		}
	}
	return ret;
}

static int
error_if_invalid_level_for_renames (struct cb_field const *field, cb_tree ref)
{
	int	level = field->level;

	if (level == 1 || level == 66 || level == 77) {
		/* don't pass error here as this should not invalidate the field */
		cb_verify_x (ref, cb_renames_uncommon_levels,
			_("RENAMES of 01-, 66- and 77-level items"));
	} else if (level == 88) {
		cb_error_x (ref, _("RENAMES may not reference a level 88"));
		return 1;
	}
	return 0;
}

int
cb_validate_renames_item (struct cb_field *item,
	cb_tree ref_renames, cb_tree ref_thru)
{
	const cb_tree	item_tree = CB_TREE (item);
	const char	*redefines_name = cb_name (CB_TREE (item->redefines));
	const char	*rename_thru_name = cb_name (CB_TREE (item->rename_thru));
	struct cb_field *founder;
	struct cb_field *f;
	int ret = 0;

	if (error_if_invalid_level_for_renames (item->redefines, ref_renames)) {
		return 1;
	}

	founder = cb_field_founder (item->redefines);
	if (item->parent != founder) {
		cb_error_x (item_tree,
			    _("'%s' must immediately follow the record '%s'"),
			    cb_name (item_tree),
			    cb_name (CB_TREE (founder)));
		ret = 1;
	}

	if (item->redefines == item->rename_thru) {
		cb_error_x (item_tree,
			    _("THRU item must be different to '%s'"),
			    redefines_name);
		ret = 1;
	} else if (item->rename_thru) {
		if (founder != cb_field_founder (item->rename_thru)) {
			cb_error_x (item_tree,
					_("'%s' and '%s' must be in the same record"),
					redefines_name, rename_thru_name);
			return 1;
		}
		if (error_if_rename_thru_is_before_redefines (item)
		 || error_if_invalid_level_for_renames (item->rename_thru, ref_thru)) {
			return 1;
		}
		for (f = item->rename_thru; f; f = f->parent) {
			if (f->parent == item->redefines) {
				cb_error_x (item_tree,
						_("THRU item '%s' may not be subordinate to '%s'"),
						rename_thru_name, redefines_name);
				return 1;
			}
		}
	}
	ret |= error_if_invalid_type_in_renames_range (item);

	if (!error_if_is_or_in_occurs (item->redefines, item)
	 && item->rename_thru) {
		ret |= error_if_is_or_in_occurs (item->rename_thru, item);
	}

	return ret;
}

void
cb_clear_real_field (void)
{
	last_real_field = NULL;
}

struct cb_field *
cb_get_real_field (void)
{
	return last_real_field;
}

const char *
cb_get_usage_string (const enum cb_usage usage)
{
	switch (usage) {
	case CB_USAGE_BINARY:
		return "COMP";
	case CB_USAGE_BIT:
		return "BIT";
	case CB_USAGE_COMP_5:
		return "COMP-5";
	case CB_USAGE_COMP_X:
		return "COMP-X";
	case CB_USAGE_COMP_N:
		return "COMP-N";
	case CB_USAGE_DISPLAY:
		return "DISPLAY";
	case CB_USAGE_FLOAT:
		return "COMP-1";
		/* return "FLOAT-SHORT"; */
	case CB_USAGE_DOUBLE:
		return "COMP-2";
		/* return "FLOAT-LONG"; */
	case CB_USAGE_LONG_DOUBLE:
		return "FLOAT-EXTENDED";
	case CB_USAGE_INDEX:
		return "INDEX";
	case CB_USAGE_NATIONAL:
		return "NATIONAL";
	case CB_USAGE_OBJECT:
		return "OBJECT REFERENCE";
	case CB_USAGE_PACKED:
		return "COMP-3";
		/* return "PACKED-DECIMAL"; */
	case CB_USAGE_POINTER:
		return "POINTER";
	case CB_USAGE_LENGTH:
		/* Probably---generates a cob_u32_t item.*/
		return "BINARY-LONG";
	case CB_USAGE_PROGRAM_POINTER:
		return "PROGRAM-POINTER";
	case CB_USAGE_UNSIGNED_CHAR:
		return "UNSIGNED-CHAR";
	case CB_USAGE_SIGNED_CHAR:
		return "SIGNED-CHAR";
	case CB_USAGE_UNSIGNED_SHORT:
		return "UNSIGNED-SHORT";
	case CB_USAGE_SIGNED_SHORT:
		return "SIGNED-SHORT";
	case CB_USAGE_UNSIGNED_INT:
		return "UNSIGNED-INT";
	case CB_USAGE_SIGNED_INT:
		return "SIGNED-INT";
	case CB_USAGE_UNSIGNED_LONG:
		return "UNSIGNED-LONG";
	case CB_USAGE_SIGNED_LONG:
		return "SIGNED-LONG";
	case CB_USAGE_COMP_6:
		return "COMP-6";
	case CB_USAGE_FP_DEC64:
		return "FLOAT-DECIMAL-16";
	case CB_USAGE_FP_DEC128:
		return "FLOAT-DECIMAL-34";
	case CB_USAGE_FP_BIN32:
		return "FLOAT-BINARY-32";
	case CB_USAGE_FP_BIN64:
		return "FLOAT-BINARY-64";
	case CB_USAGE_FP_BIN128:
		return "FLOAT-BINARY-128";
	case CB_USAGE_HNDL:
		return "HANDLE";
	case CB_USAGE_HNDL_WINDOW:
		return "HANDLE OF WINDOW";
	case CB_USAGE_HNDL_SUBWINDOW:
		return "HANDLE OF SUBWINDOW";
	case CB_USAGE_HNDL_FONT:
		return "HANDLE OF FONT";
	case CB_USAGE_HNDL_THREAD:
		return "HANDLE OF THREAD";
	case CB_USAGE_HNDL_MENU:
		return "HANDLE OF MENU";
	case CB_USAGE_HNDL_VARIANT:
		return "VARIANT";
	case CB_USAGE_HNDL_LM:
		return "HANDLE OF LAYOUT-MANAGER";
	/* LCOV_EXCL_START */
	default:
		/* as this is an unexpected message, only for requested reports,
		   leave untranslated */
		cb_error ("unexpected USAGE: %d", usage);
		COBC_ABORT ();
	/* LCOV_EXCL_STOP */
	}
}

int
cb_is_figurative_constant (const cb_tree x)
{
	return x == cb_null
		|| x == cb_zero
		|| x == cb_space
		|| x == cb_low
		|| x == cb_norm_low
		|| x == cb_high
		|| x == cb_norm_high
		|| x == cb_quote
		|| (CB_REFERENCE_P (x)
		 && CB_REFERENCE (x)->subs == NULL
		 && CB_REFERENCE (x)->flag_all);
}

int
cb_field_is_ignored_in_ml_gen (struct cb_field * const f)
{
	return f->flag_filler || f->redefines || f->rename_thru;
}

/*
   Copyright (C) 2001-2019 Free Software Foundation, Inc.

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
   along with GnuCOBOL.  If not, see <http://www.gnu.org/licenses/>.
*/

/* #define DEBUG_REPLACE */

#include "config.h"
// #include "defaults.h"
#include "cobc/cobc.h"
#include "libcob/common.h"
#include "tree.h"


#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <sys/stat.h>
#include <sys/types.h>

// #include <bsd/vis.h>

#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#endif

#define COUNT_OF(x) (sizeof(x)/sizeof(x[0]))

void cb_tree_print( const cb_tree tr, FILE *output );

static void print_label( const struct cb_label *tr, FILE *output );
static void print_report( const struct cb_report * tr, FILE *output );
static void print_program( const struct cb_program *tr, FILE* output );

const char *
cb_tag_str(enum cb_tag tag)
{
	switch (tag) {
	case CB_TAG_CONST: return "CONSTANT";
	case CB_TAG_INTEGER: return "INTEGER";
	case CB_TAG_STRING: return "STRING";
	case CB_TAG_ALPHABET_NAME: return "ALPHABET";
	case CB_TAG_CLASS_NAME: return "CLASS";
	case CB_TAG_LOCALE_NAME: return "LOCALE";
	case CB_TAG_SYSTEM_NAME: return "SYSTEM";
	case CB_TAG_LITERAL: return "LITERAL";
	case CB_TAG_DECIMAL: return "DECIMAL";
	case CB_TAG_FIELD: return "FIELD";
	case CB_TAG_FILE: return "FILE";
	case CB_TAG_REPORT: return "REPORT";
	case CB_TAG_REFERENCE: return "REFERENCE";
	case CB_TAG_BINARY_OP: return "BINARY OP";
	case CB_TAG_FUNCALL: return "FUNCTION CALL";
	case CB_TAG_CAST: return "CAST";
	case CB_TAG_INTRINSIC: return "INTRINSIC";
	case CB_TAG_LABEL: return "LABEL";
	case CB_TAG_ASSIGN: return "ASSIGN";
	case CB_TAG_INITIALIZE: return "INITIALIZE";
	case CB_TAG_SEARCH: return "SEARCH";
	case CB_TAG_CALL: return "CALL";
	case CB_TAG_GOTO: return "GO TO";
	case CB_TAG_IF: return "IF";
	case CB_TAG_PERFORM: return "PERFORM";
	case CB_TAG_STATEMENT: return "STATEMENT";
	case CB_TAG_CONTINUE: return "CONTINUE";
	case CB_TAG_CANCEL: return "CANCEL";
	case CB_TAG_ALTER: return "ALTER";
	case CB_TAG_SET_ATTR: return "SET ATTRIBUTE";
	case CB_TAG_PERFORM_VARYING: return "PERFORM";
	case CB_TAG_PICTURE: return "PICTURE";
	case CB_TAG_LIST: return "LIST";
	case CB_TAG_DIRECT: return "DIRECT";
	case CB_TAG_DEBUG: return "DEBUG";
	case CB_TAG_DEBUG_CALL: return "DEBUG CALL";
	case CB_TAG_PROGRAM: return "PROGRAM";
	case CB_TAG_PROTOTYPE: return "PROTOTYPE";
	case CB_TAG_DECIMAL_LITERAL: return "DECIMAL LITERAL";
	case CB_TAG_REPORT_LINE: return "REPORT LINE";
	case CB_TAG_ML_SUPPRESS: return "ML SUPPRESS CLAUSE";
	case CB_TAG_ML_TREE: return "ML OUTPUT TREE";
	case CB_TAG_ML_SUPPRESS_CHECKS: return "ML SUPPRESS CHECKS";
        case CB_TAG_CD: return "CD";
#ifdef COBC_VERSION4
	case CB_TAG_VARY: return "VARY";
#endif
	}

	static char unknown[64];

	sprintf(unknown, "UNKNOWN: %d", tag);
	return unknown;
}

const char *
cb_category_str( enum cb_category category ) {
	switch(category) {
	case CB_CATEGORY_UNKNOWN: return "UNKNOWN";
	case CB_CATEGORY_ALPHABETIC: return "ALPHABETIC";
	case CB_CATEGORY_ALPHANUMERIC: return "ALPHANUMERIC";
	case CB_CATEGORY_ALPHANUMERIC_EDITED: return "ALPHANUMERIC_EDITED";
	case CB_CATEGORY_BOOLEAN: return "BOOLEAN";
	case CB_CATEGORY_INDEX: return "INDEX";
	case CB_CATEGORY_NATIONAL: return "NATIONAL";
	case CB_CATEGORY_NATIONAL_EDITED: return "NATIONAL_EDITED";
	case CB_CATEGORY_NUMERIC: return "NUMERIC";
	case CB_CATEGORY_NUMERIC_EDITED: return "NUMERIC_EDITED";
	case CB_CATEGORY_OBJECT_REFERENCE: return "OBJECT_REFERENCE";
	case CB_CATEGORY_DATA_POINTER: return "DATA_POINTER";
	case CB_CATEGORY_PROGRAM_POINTER: return "PROGRAM_POINTER";
	case CB_CATEGORY_FLOATING_EDITED: return "FLOATING_EDITED";
	case CB_CATEGORY_ERROR: return "ERROR";
	}
	static char unknown[64];

	sprintf(unknown, "UNKNOWN: %d", category);
	return unknown;

}

static const char *
cob_pic_symbol_str( const cob_pic_symbol *pic ) {
  if( pic == NULL )
    return "(not yet computed)";
  
  static char line[80];
  sprintf(line,
          "symbol = '%c'\n"
          "times_repeated = %d\n",
          pic->symbol, pic->times_repeated);
  return line;
}

#define CASE_OF(prefix, name) case prefix ## name: return #name
#define CASE_NAME(name) case name: return #name

static
const char *
cb_ml_suppress_target_str( enum cb_ml_suppress_target target ) {
	switch(target) {
		CASE_OF(CB_ML_, SUPPRESS_IDENTIFIER);
		CASE_OF(CB_ML_, SUPPRESS_ALL);
		CASE_OF(CB_ML_, SUPPRESS_TYPE);
	}
        return "UNKNOWN";
}

static
const char *
cb_ml_suppress_category_str( enum cb_ml_suppress_category category ) {
	switch(category) {
		CASE_OF(CB_ML_, SUPPRESS_CAT_NUMERIC);
		CASE_OF(CB_ML_, SUPPRESS_CAT_NONNUMERIC);
		CASE_OF(CB_ML_, SUPPRESS_CAT_ANY);
	}
        return "UNKNOWN";
}

static
const char *
cb_ml_type_str( enum cb_ml_type type ) {
	switch(type) {
		CASE_OF(CB_ML_, ATTRIBUTE);
		CASE_OF(CB_ML_, ELEMENT);
		CASE_OF(CB_ML_, CONTENT);
		CASE_OF(CB_ML_, ANY_TYPE);
	}
        return "UNKNOWN";
}

static
const char *
cb_index_type_str( enum cb_index_type type ) {
	switch(type) {
		CASE_OF(CB_, NORMAL_INDEX);
		CASE_OF(CB_, INT_INDEX);
		CASE_OF(CB_, STATIC_INT_INDEX);
		CASE_OF(CB_, STATIC_INT_VARYING);
	}
        return "UNKNOWN";
}

static
const char *
cb_storage_str( enum cb_storage type ) {
	switch(type) {
		CASE_OF(CB_STORAGE_, CONSTANT);
		CASE_OF(CB_STORAGE_, FILE);
		CASE_OF(CB_STORAGE_, WORKING);
		CASE_OF(CB_STORAGE_, LOCAL);
		CASE_OF(CB_STORAGE_, LINKAGE);
		CASE_OF(CB_STORAGE_, SCREEN);
		CASE_OF(CB_STORAGE_, REPORT);
		CASE_OF(CB_STORAGE_, COMMUNICATION);
	}
        return "UNKNOWN";
}

static
const char *
cb_usage_str( enum cb_usage type ) {
	switch(type) {
		CASE_OF(CB_USAGE_, BINARY);
		CASE_OF(CB_USAGE_, BIT);
		CASE_OF(CB_USAGE_, COMP_5);
		CASE_OF(CB_USAGE_, COMP_X);
		CASE_OF(CB_USAGE_, DISPLAY);
		CASE_OF(CB_USAGE_, FLOAT);
		CASE_OF(CB_USAGE_, DOUBLE);
		CASE_OF(CB_USAGE_, INDEX);
		CASE_OF(CB_USAGE_, NATIONAL);
		CASE_OF(CB_USAGE_, OBJECT);
		CASE_OF(CB_USAGE_, PACKED);
		CASE_OF(CB_USAGE_, POINTER);
		CASE_OF(CB_USAGE_, LENGTH);
		CASE_OF(CB_USAGE_, PROGRAM_POINTER);
		CASE_OF(CB_USAGE_, UNSIGNED_CHAR);
		CASE_OF(CB_USAGE_, SIGNED_CHAR);
		CASE_OF(CB_USAGE_, UNSIGNED_SHORT);
		CASE_OF(CB_USAGE_, SIGNED_SHORT);
		CASE_OF(CB_USAGE_, UNSIGNED_INT);
		CASE_OF(CB_USAGE_, SIGNED_INT);
		CASE_OF(CB_USAGE_, UNSIGNED_LONG);
		CASE_OF(CB_USAGE_, SIGNED_LONG);
		CASE_OF(CB_USAGE_, COMP_6);
		CASE_OF(CB_USAGE_, FP_DEC64);
		CASE_OF(CB_USAGE_, FP_DEC128);
		CASE_OF(CB_USAGE_, FP_BIN32);
		CASE_OF(CB_USAGE_, FP_BIN64);
		CASE_OF(CB_USAGE_, FP_BIN128);
		CASE_OF(CB_USAGE_, LONG_DOUBLE);
		CASE_OF(CB_USAGE_, HNDL);
		CASE_OF(CB_USAGE_, HNDL_WINDOW);
		CASE_OF(CB_USAGE_, HNDL_SUBWINDOW);
		CASE_OF(CB_USAGE_, HNDL_FONT);
		CASE_OF(CB_USAGE_, HNDL_THREAD);
		CASE_OF(CB_USAGE_, HNDL_MENU);
		CASE_OF(CB_USAGE_, HNDL_VARIANT);
		CASE_OF(CB_USAGE_, HNDL_LM);
		CASE_OF(CB_USAGE_, COMP_N);
		CASE_OF(CB_USAGE_, ERROR);
#ifdef COBC_VERSION4
		CASE_OF(CB_USAGE_, CONTROL);
#endif
	}
        return "UNKNOWN";
}

static
const char *
cb_intr_enum_str( enum cb_intr_enum intr ) {

	switch(intr) {
		CASE_OF(CB_INTR_, ABS);
		CASE_OF(CB_INTR_, ACOS);
		CASE_OF(CB_INTR_, ANNUITY);
		CASE_OF(CB_INTR_, ASIN);
		CASE_OF(CB_INTR_, ATAN);
		CASE_OF(CB_INTR_, BOOLEAN_OF_INTEGER);
		CASE_OF(CB_INTR_, BYTE_LENGTH);
		CASE_OF(CB_INTR_, CHAR);
		CASE_OF(CB_INTR_, CHAR_NATIONAL);
		CASE_OF(CB_INTR_, COMBINED_DATETIME);
		CASE_OF(CB_INTR_, CONCATENATE);
		CASE_OF(CB_INTR_, CONTENT_LENGTH);
		CASE_OF(CB_INTR_, CONTENT_OF);
		CASE_OF(CB_INTR_, COS);
		CASE_OF(CB_INTR_, CURRENCY_SYMBOL);
		CASE_OF(CB_INTR_, CURRENT_DATE);
		CASE_OF(CB_INTR_, DATE_OF_INTEGER);
		CASE_OF(CB_INTR_, DATE_TO_YYYYMMDD);
		CASE_OF(CB_INTR_, DAY_OF_INTEGER);
		CASE_OF(CB_INTR_, DAY_TO_YYYYDDD);
		CASE_OF(CB_INTR_, DISPLAY_OF);
		CASE_OF(CB_INTR_, E);
		CASE_OF(CB_INTR_, EXCEPTION_FILE);
		CASE_OF(CB_INTR_, EXCEPTION_FILE_N);
		CASE_OF(CB_INTR_, EXCEPTION_LOCATION);
		CASE_OF(CB_INTR_, EXCEPTION_LOCATION_N);
		CASE_OF(CB_INTR_, EXCEPTION_STATEMENT);
		CASE_OF(CB_INTR_, EXCEPTION_STATUS);
		CASE_OF(CB_INTR_, EXP);
		CASE_OF(CB_INTR_, EXP10);
		CASE_OF(CB_INTR_, FACTORIAL);
		CASE_OF(CB_INTR_, FORMATTED_CURRENT_DATE);
		CASE_OF(CB_INTR_, FORMATTED_DATE);
		CASE_OF(CB_INTR_, FORMATTED_DATETIME);
		CASE_OF(CB_INTR_, FORMATTED_TIME);
		CASE_OF(CB_INTR_, FRACTION_PART);
		CASE_OF(CB_INTR_, HIGHEST_ALGEBRAIC);
		CASE_OF(CB_INTR_, INTEGER);
		CASE_OF(CB_INTR_, INTEGER_OF_BOOLEAN);
		CASE_OF(CB_INTR_, INTEGER_OF_DATE);
		CASE_OF(CB_INTR_, INTEGER_OF_DAY);
		CASE_OF(CB_INTR_, INTEGER_OF_FORMATTED_DATE);
		CASE_OF(CB_INTR_, INTEGER_PART);
		CASE_OF(CB_INTR_, LENGTH);
		CASE_OF(CB_INTR_, LOCALE_COMPARE);
		CASE_OF(CB_INTR_, LOCALE_DATE);
		CASE_OF(CB_INTR_, LOCALE_TIME);
		CASE_OF(CB_INTR_, LOCALE_TIME_FROM_SECS);
		CASE_OF(CB_INTR_, LOG);
		CASE_OF(CB_INTR_, LOG10);
		CASE_OF(CB_INTR_, LOWER_CASE);
		CASE_OF(CB_INTR_, LOWEST_ALGEBRAIC);
		CASE_OF(CB_INTR_, MAX);
		CASE_OF(CB_INTR_, MEAN);
		CASE_OF(CB_INTR_, MEDIAN);
		CASE_OF(CB_INTR_, MIDRANGE);
		CASE_OF(CB_INTR_, MIN);
		CASE_OF(CB_INTR_, MOD);
		CASE_OF(CB_INTR_, MODULE_CALLER_ID);
		CASE_OF(CB_INTR_, MODULE_DATE);
		CASE_OF(CB_INTR_, MODULE_FORMATTED_DATE);
		CASE_OF(CB_INTR_, MODULE_ID);
		CASE_OF(CB_INTR_, MODULE_PATH);
		CASE_OF(CB_INTR_, MODULE_SOURCE);
		CASE_OF(CB_INTR_, MODULE_TIME);
		CASE_OF(CB_INTR_, MON_DECIMAL_POINT);
		CASE_OF(CB_INTR_, MON_THOUSANDS_SEP);
		CASE_OF(CB_INTR_, NATIONAL_OF);
		CASE_OF(CB_INTR_, NUM_DECIMAL_POINT);
		CASE_OF(CB_INTR_, NUM_THOUSANDS_SEP);
		CASE_OF(CB_INTR_, NUMVAL);
		CASE_OF(CB_INTR_, NUMVAL_C);
		CASE_OF(CB_INTR_, NUMVAL_F);
		CASE_OF(CB_INTR_, ORD);
		CASE_OF(CB_INTR_, ORD_MAX);
		CASE_OF(CB_INTR_, ORD_MIN);
		CASE_OF(CB_INTR_, PI);
		CASE_OF(CB_INTR_, PRESENT_VALUE);
		CASE_OF(CB_INTR_, RANDOM);
		CASE_OF(CB_INTR_, RANGE);
		CASE_OF(CB_INTR_, REM);
		CASE_OF(CB_INTR_, REVERSE);
		CASE_OF(CB_INTR_, SECONDS_FROM_FORMATTED_TIME);
		CASE_OF(CB_INTR_, SECONDS_PAST_MIDNIGHT);
		CASE_OF(CB_INTR_, SIGN);
		CASE_OF(CB_INTR_, SIN);
		CASE_OF(CB_INTR_, SQRT);
		CASE_OF(CB_INTR_, STANDARD_COMPARE);
		CASE_OF(CB_INTR_, STANDARD_DEVIATION);
		CASE_OF(CB_INTR_, STORED_CHAR_LENGTH);
		CASE_OF(CB_INTR_, SUBSTITUTE);
		CASE_OF(CB_INTR_, SUBSTITUTE_CASE);
		CASE_OF(CB_INTR_, SUM);
		CASE_OF(CB_INTR_, TAN);
		CASE_OF(CB_INTR_, TEST_DATE_YYYYMMDD);
		CASE_OF(CB_INTR_, TEST_DAY_YYYYDDD);
		CASE_OF(CB_INTR_, TEST_FORMATTED_DATETIME);
		CASE_OF(CB_INTR_, TEST_NUMVAL);
		CASE_OF(CB_INTR_, TEST_NUMVAL_C);
		CASE_OF(CB_INTR_, TEST_NUMVAL_F);
		CASE_OF(CB_INTR_, TRIM);
		CASE_OF(CB_INTR_, UPPER_CASE);
		CASE_OF(CB_INTR_, USER_FUNCTION);
		CASE_OF(CB_INTR_, VARIANCE);
		CASE_OF(CB_INTR_, WHEN_COMPILED);
		CASE_OF(CB_INTR_, BASECONVERT);
		CASE_OF(CB_INTR_, BIT_OF);
		CASE_OF(CB_INTR_, BIT_TO_CHAR);
		CASE_OF(CB_INTR_, CONVERT);
		CASE_OF(CB_INTR_, FIND_STRING);
		CASE_OF(CB_INTR_, HEX_OF);
		CASE_OF(CB_INTR_, HEX_TO_CHAR);
		CASE_OF(CB_INTR_, MODULE_NAME);
		CASE_OF(CB_INTR_, YEAR_TO_YYYY);
	}
        return "UNKNOWN";
}

static
const char *
cb_feature_mode_str( enum cb_feature_mode mode ) {
	switch(mode) {
		CASE_OF(CB_FEATURE_, ACTIVE);
		CASE_OF(CB_FEATURE_, DISABLED);
		CASE_OF(CB_FEATURE_, MUST_BE_ENABLED);
		CASE_OF(CB_FEATURE_, NOT_IMPLEMENTED);
	}
        return "UNKNOWN";
}

static
const char *
cb_cast_type_str( enum cb_cast_type type ) {
	switch(type) {
		CASE_OF(CB_CAST_, INTEGER);
		CASE_OF(CB_CAST_, LONG_INT);
		CASE_OF(CB_CAST_, ADDRESS);
		CASE_OF(CB_CAST_, ADDR_OF_ADDR);
		CASE_OF(CB_CAST_, LENGTH);
		CASE_OF(CB_CAST_, PROGRAM_POINTER);
	}
        return "UNKNOWN";
}

static
const char *
cb_perform_type_str( enum cb_perform_type type ) {
	switch(type) {
		CASE_OF(CB_PERFORM_, EXIT);
		CASE_OF(CB_PERFORM_, ONCE);
		CASE_OF(CB_PERFORM_, TIMES);
		CASE_OF(CB_PERFORM_, UNTIL);
		CASE_OF(CB_PERFORM_, FOREVER);
	}
        return "UNKNOWN";
}


static
const char *
cb_handler_type_str( enum cb_handler_type type ) {
	switch(type) {
		CASE_NAME(NO_HANDLER);
		CASE_NAME(DISPLAY_HANDLER);
		CASE_NAME(ACCEPT_HANDLER);
		CASE_NAME(SIZE_ERROR_HANDLER);
		CASE_NAME(OVERFLOW_HANDLER);
		CASE_NAME(AT_END_HANDLER);
		CASE_NAME(EOP_HANDLER);
		CASE_NAME(INVALID_KEY_HANDLER);
		CASE_NAME(XML_HANDLER);
		CASE_NAME(JSON_HANDLER);
		CASE_NAME(MCS_HANDLER);
#ifdef COBC_VERSION4
		CASE_NAME(DELETE_FILE_HANDLER);
#endif
        }
        return "UNKNOWN";
}


static const char *
pretty_print( const char input[], size_t len ) {
  /*
	static char *output;

	if ((output = realloc(output, 1 + len)) == NULL ) {
		return output;
	}
	int n = strnvis(output, input, len, VIS_CSTYLE);
	assert(n <= len);
	output[n] = '\0';
	return output;
  */
  return input;
}

static void
print_common( const struct cb_tree_common* tr, FILE* output ) {
	fprintf( output,
		 "{\n"
		 "  tag = %s,\n"
		 "  category = %s,\n"
		 "  source_file = %s,\n"
		 "  source_line = %d,\n"
		 "  source_column = %d\n"
		 "}\n",
		 cb_tag_str(tr->tag), cb_category_str(tr->category),
		 tr->source_file != NULL ? tr->source_file : "(none)",
                 tr->source_line, tr->source_column );
}

struct flag_day { char name[32]; bool flag; };

static void
print_flags( const struct flag_day flags[], size_t len, FILE* output)
{
	for( const struct flag_day *p = flags; p < flags + len; p++ ) {
		if( !p->flag ) {
			continue;
		}
		fprintf(output, "%-32s = true\n", p->name);
	}
}

static void
print_xref( const struct cb_xref xref, FILE* output ) {
	fprintf(output, "xref: skip = %d:\n", xref.skip);
	for( struct cb_xref_elem *p = xref.head;
	     p != NULL && p != xref.tail;
	     p = p->next ) {
		fprintf(output,
			"{line = %d, receive = %d}\n",
			p->line, p->receive);
	}
}

static void
print_call_xref( const struct cb_call_xref xref, FILE* output ) {
	fprintf(output, "call_xref:\n");

	struct cb_call_elem *p = xref.head;
	for( ; p != NULL && p != xref.tail; p = p->next ) {
		fprintf(output,
			"name = %s\n"
			"is_identifier = %d\n"
			"is_system = %d\n",
			p-> name,
			p-> is_identifier,
			p-> is_system);

		print_xref(p->xref, output);
	}
}

static
void
print_ml_generate_tree( struct cb_ml_generate_tree *tr, FILE* output ) {
	fprintf(output,
		"id = %d\n"
		"type = %s\n",
		tr->id,
		cb_ml_type_str(tr->type));

	cb_tree_print(tr->name, output);
	cb_tree_print(tr->value, output);
	cb_tree_print(tr->suppress_cond, output);

        print_ml_generate_tree(tr->attrs, output);
        print_ml_generate_tree(tr->parent, output);
        print_ml_generate_tree(tr->children, output);
        print_ml_generate_tree(tr->prev_sibling, output);
        print_ml_generate_tree(tr->sibling, output);
}

static void
print_field2(  const struct cb_field *fld, FILE *output, int all );
static void
print_field2(  const struct cb_field *fld, FILE *output, int all ) {
  if( fld != NULL ){
	fprintf(output,
		"name = %s\n" "ename = %s\n",
		fld->name, fld->ename);

        if( all ){
	struct field_t {
		char name[20];
		struct cb_field * field;
	} const fields[] = {
		{ "parent",     fld->parent },
		{ "children",   fld->children },
		{ "validation", fld->validation },
		{ "sister",     fld->sister },
		{ "redefines",  fld->redefines },
		{ "thru",       fld->rename_thru },
		{ "qual",       fld->index_qual },
		{ "vsize",      fld->vsize },
	}, *eofields = fields + COUNT_OF(fields);

	for( const struct field_t *p = fields; p < eofields; p++ ) {
		fprintf(output, "%s:\n", p->name);
		print_field2(p->field, output, 0);
	}
        }
	cb_tree_print( CB_TREE(fld->file), output );
	cb_tree_print( CB_TREE(fld->cd), output );

        if( fld->keys != NULL ){
          fprintf(output, "keys, (dir = %d):\n", fld->keys->dir);
          cb_tree_print( fld->keys->key, output );
          cb_tree_print( fld->keys->ref, output );
          cb_tree_print( fld->keys->val, output );
          fprintf(output, "end keys\n");
        }


	cb_tree_print( CB_TREE(fld->pic), output );

	print_label(fld->debug_section, output);
	print_report( fld->report, output );

	struct forest { char name[20]; cb_tree tree; } const trees[] = {
		{ "screen_line",        fld->screen_line },
		{ "screen_column",      fld->screen_column },
		{ "screen_from",        fld->screen_from },
		{ "screen_to",          fld->screen_to },
		{ "screen_foreg",       fld->screen_foreg },
		{ "screen_backg",       fld->screen_backg },
		{ "screen_prompt",      fld->screen_prompt },
		{ "report_source",      fld->report_source },
		{ "report_from",        fld->report_from },
		{ "report_sum_counter", fld->report_sum_counter },
		{ "report_sum_list",    fld->report_sum_list },
		{ "report_sum_upon",    fld->report_sum_upon },
		{ "report_reset",       fld->report_reset },
		{ "report_control",     fld->report_control },
		{ "report_when",        fld->report_when },
		{ "report_column_list", fld->report_column_list }
	}, *eotrees = trees + COUNT_OF(trees);

	for( const struct forest *p = trees; p < eotrees; p++ ) {
		fprintf(output, "%s:\n", p->name);
		cb_tree_print(p->tree, output);
	}

	print_xref( fld->xref, output);

	fprintf(output,
		"id = %d\n"
		"size = %d\n"
		"level = %d\n"
		"memory_size = %d\n"
#ifdef COBC_VERSION4
		"compx_size = %d\n"
#endif
		"offset = %d\n"
		"occurs_min = %d\n"
		"occurs_max = %d\n"
		"indexes = %d\n"
		"count = %d\n"
		"mem_offset = %d\n"
		"nkeys = %d\n"
		"param_num = %d\n"
		"screen_flag = %lld\n"
		"report_flag = %d\n"
		"report_line = %d\n"
		"report_column = %d\n"
		"report_num_col = %d\n"
		"report_decl_id = %d\n"
		"step_count = %d\n"
		"next_group_line = %d\n"
		"vaddr = %u\n"
		"odo_level = %u\n",
		fld->id,
		fld->size,
		fld->level,
		fld->memory_size,
#ifdef COBC_VERSION4
		fld->compx_size,
#endif
		fld->offset,
		fld->occurs_min,
		fld->occurs_max,
		fld->indexes,
		fld->count,
		fld->mem_offset,
		fld->nkeys,
		fld->param_num,
		fld->screen_flag,
		fld->report_flag,
		fld->report_line,
		fld->report_column,
		fld->report_num_col,
		fld->report_decl_id,
		fld->step_count,
		fld->next_group_line,
		fld->vaddr,
		fld->odo_level );

	fprintf(output,
		"index_type = %s\n"
		"storage = %s\n"
		"usage = %s\n",
		cb_index_type_str(fld->index_type),
		cb_storage_str(fld->storage),
		cb_usage_str(fld->usage) );

	struct flag_day flags[] = {
		{ "base", fld->flag_base },
		{ "external", fld->flag_external },
		{ "local_storage", fld->flag_local_storage },
		{ "is_global", fld->flag_is_global },
		{ "local", fld->flag_local },
		{ "occurs", fld->flag_occurs },
		{ "sign_clause", fld->flag_sign_clause },
		{ "sign_separate", fld->flag_sign_separate },
		{ "sign_leading", fld->flag_sign_leading },
		{ "blank_zero", fld->flag_blank_zero },
		{ "justified", fld->flag_justified },
		{ "binary_swap", fld->flag_binary_swap },
		{ "real_binary", fld->flag_real_binary },
		{ "is_pointer", fld->flag_is_pointer },
		{ "item_78", fld->flag_item_78 },
		{ "any_length", fld->flag_any_length },
		{ "item_based", fld->flag_item_based },
		{ "is_external_form", fld->flag_is_external_form },
		{ "filler", fld->flag_filler },
		{ "synchronized", fld->flag_synchronized },
		{ "invalid", fld->flag_invalid },
		{ "field", fld->flag_field },
		{ "chained", fld->flag_chained },
#ifdef COBC_VERSION4
		{ "data_set", fld->flag_data_set },
#endif
		{ "is_verified", fld->flag_is_verified },
		{ "is_c_long", fld->flag_is_c_long },
		{ "is_pdiv_parm", fld->flag_is_pdiv_parm },
		{ "is_pdiv_opt", fld->flag_is_pdiv_opt },
		{ "indexed_by", fld->flag_indexed_by },
		{ "local_alloced", fld->flag_local_alloced },
		{ "no_init", fld->flag_no_init },
		{ "vsize_done", fld->flag_vsize_done },
		{ "vaddr_done", fld->flag_vaddr_done },
		{ "odo_relative", fld->flag_odo_relative },
		{ "field_debug", fld->flag_field_debug },
		{ "all_debug", fld->flag_all_debug },
		{ "no_field", fld->flag_no_field },
		{ "any_numeric", fld->flag_any_numeric },
		{ "is_returning", fld->flag_is_returning },
		{ "unbounded", fld->flag_unbounded },
		{ "constant", fld->flag_constant },
		{ "internal_constant", fld->flag_internal_constant },
		{ "comp_1", fld->flag_comp_1 },
		{ "volatile", fld->flag_volatile },
#ifdef COBC_VERSION4
		{ "validated", fld->flag_validated },
#endif
	};

	print_flags(flags, COUNT_OF(flags), output);
  }
}

static void
print_field(  const struct cb_field *fld, FILE *output ) {
    print_field2( fld, output, 1);
}
 
static void
print_label2(  const struct cb_label *tr, FILE *output, int all );
static void
  print_label2(  const struct cb_label *tr, FILE *output, int all ) {
  if( tr != NULL ){
	//// print_common(tr->common, output);

	fprintf(output,
		"name = %s\n"
		"orig_name = %s\n"
		"id = %d\n"
		"section_id = %d\n"
		"segment = %d\n",
		tr->name,
		tr->orig_name,
		tr->id,
		tr->section_id,
		tr->segment );

        if( all ){
          print_label2( tr->section, output, 0 );
          print_label2( tr->debug_section, output, 0 );
          
          for( struct cb_para_label *p = tr->para_label;
               p != NULL; p = p->next ) {
            print_label2(p->para, output, 0);
          }
        }

	print_xref(tr->xref, output);
	cb_tree_print( tr->exit_label, output );

	fprintf(output, "alter_gotos:\n");
	for( struct cb_alter_id *p = tr->alter_gotos;
	     p != NULL; p = p->next ) {
		static int i=0;
		fprintf(output, "%d: %d\n", ++i, p->goto_id);
	}

	struct flag_day flags[] = {
		{ "section", tr->flag_section },
		{ "entry", tr->flag_entry },
		{ "begin", tr->flag_begin },
		{ "return", tr->flag_return },
		{ "real_label", tr->flag_real_label },
		{ "global", tr->flag_global },
		{ "declarative_exit", tr->flag_declarative_exit },
		{ "declaratives", tr->flag_declaratives },
		{ "fatal_check", tr->flag_fatal_check },
		{ "dummy_section", tr->flag_dummy_section },
		{ "dummy_paragraph", tr->flag_dummy_paragraph },
		{ "dummy_exit", tr->flag_dummy_exit },
		{ "next_sentence", tr->flag_next_sentence },
		{ "default_handler", tr->flag_default_handler },
		{ "statement", tr->flag_statement },
		{ "first_is_goto", tr->flag_first_is_goto },
		{ "alter", tr->flag_alter },
		{ "debugging_mode", tr->flag_debugging_mode },
		{ "is_debug_sect", tr->flag_is_debug_sect },
		{ "skip_label", tr->flag_skip_label }
	};
	print_flags(flags, COUNT_OF(flags), output);
  }
}

static void
print_label(  const struct cb_label *tr, FILE *output ) {
  print_label2( tr, output, 1 );
}
static void
print_alphabet_name(  const struct cb_alphabet_name * tr, FILE *output ) {
  if( tr != NULL ){
	fprintf(output, "name = %s\n" "cname = %s\n", tr->name, tr->cname);

	cb_tree_print( tr->custom_list, output );

	fprintf(output,
		"alphabet_type = %u\n"
		"low_val_char = %d\n"
		"high_val_char = %d\n",
		tr->alphabet_type,
		tr->low_val_char,
		tr->high_val_char);

	if( false ) { // set to true to print value arrays
		return;
	}

	fprintf(output, "Collating values\n");
	for( const int *p = tr->values;
	     p < tr->values + sizeof(tr->values); p++ ) {
		fprintf(output, "%2x", *p);
		if( (1 + (p - tr->values)) % 16 == 0 ) {
			printf("\n");
		}
	}

	fprintf(output, "Actual alphachr\n");
	for( const int *p = tr->alphachr;
	     p < tr->alphachr + sizeof(tr->alphachr); p++ ) {
		fprintf(output, "%2x", *p);
		if( (1 + (p - tr->alphachr)) % 16 == 0 ) {
			printf("\n");
		}
	}
  }
}

static void
print_picture( const struct cb_picture *pic, FILE* output ) {
		fprintf(output,
			"orig = %s\n"
			"str = %s\n"
			"size = %d\n"
			"lenstr = %d\n"
			"category = %s\n"
			"digits = %u\n"
			"scale = %d\n"
			"have_sign = %u\n",
			pic->orig,
			cob_pic_symbol_str(pic->str),
			pic->size,
			pic->lenstr,
			cb_category_str(pic->category),
			pic->digits,
			pic->scale,
			pic->have_sign);

		struct flag_day flags[] = {
			{ "flag_is_calculated",
			  pic->flag_is_calculated } };
		print_flags(flags, COUNT_OF(flags), output);
}

static const struct cb_word *
print_word( const struct cb_word *word, FILE* output ) {
	fprintf(output, "name = %s\n" "count = %d%s\n",
		word->name, word->count, word->error? " (ERROR)" : "");
	cb_tree_print(word->items, output);

	return word->next;
}

static void
print_reference(  const struct cb_reference *tr, FILE *output ) {
	cb_tree_print(tr->chain, output);
	cb_tree_print(tr->value, output);
	cb_tree_print(tr->subs, output);
	cb_tree_print(tr->offset, output);
	cb_tree_print(tr->length, output);
	cb_tree_print(tr->check, output);

	print_word( tr->word, output);
	print_label( tr->section, output);
	print_label( tr->paragraph, output);
	print_label( tr->debug_section, output);

	fprintf(output, "%zu\n", tr->hashval);

	struct flag_day flags[] = {
		{ "flag_receiving", tr->flag_receiving },
		{ "flag_all", tr->flag_all },
		{ "flag_in_decl", tr->flag_in_decl },
		{ "flag_decl_ok", tr->flag_decl_ok },
		{ "flag_alter_code", tr->flag_alter_code },
		{ "flag_debug_code", tr->flag_debug_code },
		{ "flag_all_debug", tr->flag_all_debug },
		{ "flag_target", tr->flag_target },
		{ "flag_optional", tr->flag_optional },
		{ "flag_ignored", tr->flag_ignored },
		{ "flag_filler_ref", tr->flag_filler_ref },
		{ "flag_duped", tr->flag_duped },
	};
	print_flags(flags, COUNT_OF(flags), output);
}

static void
print_list(  const struct cb_list *tr, FILE *output ) {
	fprintf(output, "sizes = %d\n", tr->sizes);
        if( tr->chain != NULL )
          cb_tree_print(tr->chain, output);
	cb_tree_print(tr->value, output);
	cb_tree_print(tr->purpose, output);
}

static void
print_file(  const struct cb_file *tr, FILE *output ) {
	fprintf(output,
		"name = %s\n" "cname = %s\n", tr->name, tr->cname);

	struct forest { char name[20]; cb_tree tree; } const trees[] = {
		{ "assign", CB_TREE(tr->assign) },
		{ "file_status", CB_TREE(tr->file_status) },
		{ "sharing", CB_TREE(tr->sharing) },
		{ "key", CB_TREE(tr->key) },
		{ "password", CB_TREE(tr->password) },
		{ "record_depending", CB_TREE(tr->record_depending) },
		{ "reports", CB_TREE(tr->reports) },
		{ "linage", CB_TREE(tr->linage) },
		{ "linage_ctr", CB_TREE(tr->linage_ctr) },
		{ "latfoot", CB_TREE(tr->latfoot) },
		{ "lattop", CB_TREE(tr->lattop) },
		{ "latbot", CB_TREE(tr->latbot) },
		{ "extfh", CB_TREE(tr->extfh) },
	}, *eotrees = trees + COUNT_OF(trees);

	for( const struct forest *p = trees; p < eotrees; p++ ) {
		fprintf(output, "%s:\n", p->name);
		cb_tree_print(p->tree, output);
	}

	fprintf(output, "component_list:\n");
	for( struct cb_key_component *p = tr->component_list;
	     p != NULL; p = p->next ) {
		cb_tree_print(p->component, output);
	}

	fprintf(output, "alt_key_list:\n");
	for( struct cb_alt_key *p = tr->alt_key_list;
	     p != NULL; p = p->next ) {
		fprintf(output,
			"duplicates = %d\n "
			"offset = %d\n "
			"tf_suppress = %d\n "
			"char_suppress = %d\n ",
			p->duplicates,
			p->offset,
			p->tf_suppress,
			p->char_suppress );
		cb_tree_print(p->key, output);
		cb_tree_print(p->password, output);

		fprintf(output, "subcomponent_list:\n");
		for( struct cb_key_component *alt = p->component_list;
		     alt != NULL; alt = alt->next ) {
			cb_tree_print(alt->component, output);
		}
	}

	print_field(tr->record, output);

	print_label( tr->handler, output);
	print_program( tr->handler_prog, output);
	print_label( tr->debug_section, output);
	print_alphabet_name(tr->code_set, output);
	print_list( tr->code_set_items, output);
	print_xref( tr->xref, output);

	fprintf(output,
		"record_min = %d\n"
		"record_max = %d\n"
		"optional = %d\n"
		"organization = %d\n"
		"access_mode = %d\n"
		"lock_mode = %d\n"
#ifdef COBC_VERSION4
		"fd_share_mode = %d\n"
#endif
		"special = %d\n"
		"same_clause = %d\n",
		tr->record_min,
		tr->record_max,
		tr->optional,
		tr->organization,
		tr->access_mode,
		tr->lock_mode,
#ifdef COBC_VERSION4
		tr->fd_share_mode,
#endif
		tr->special,
		tr->same_clause);

	struct flag_day flags[] = {
		{ "flag_finalized", tr->flag_finalized },
		{ "flag_external", tr->flag_external },
		{ "flag_ext_assign", tr->flag_ext_assign },
		{ "flag_fileid", tr->flag_fileid },
		{ "flag_global", tr->flag_global },
		{ "flag_fl_debug", tr->flag_fl_debug },
		{ "flag_line_adv", tr->flag_line_adv },
		{ "flag_delimiter", tr->flag_delimiter },
		{ "flag_report", tr->flag_report },
		{ "flag_check_record_varying_limits",
		  tr->flag_check_record_varying_limits },
	};
	print_flags(flags, COUNT_OF(flags), output);
}

static void
print_report(  const struct cb_report * tr, FILE *output ) {
  if( tr != NULL ){
	print_common(& tr->common, output);

	fprintf(output, "name = %s\n" "cname = %s\n", tr->name, tr->cname);

	print_file(tr->file, output);

        cb_tree_print(tr->line_counter, output);
        cb_tree_print(tr->page_counter, output);
        cb_tree_print(tr->code_clause, output);
        cb_tree_print(tr->controls, output);
        cb_tree_print(tr->t_lines, output);
        cb_tree_print(tr->t_columns, output);
        cb_tree_print(tr->t_heading, output);
        cb_tree_print(tr->t_first_detail, output);
        cb_tree_print(tr->t_last_control, output);
        cb_tree_print(tr->t_last_detail, output);
        cb_tree_print(tr->t_footing, output);

	fprintf(output,
		"lines = %d\n"
		"columns = %d\n"
		"heading = %d \n"
		"first_detail = %d \n"
		"last_control = %d \n"
		"last_detail = %d \n"
		"footing = %d \n"
		"num_lines = %d \n"
		"num_sums = %d \n"
		"rcsz = %d \n"
		"id = %d \n",
		tr->lines,
		tr->columns,
		tr->heading,
		tr->first_detail,
		tr->last_control,
		tr->last_detail,
		tr->footing,
		tr->num_lines,
		tr->num_sums,
		tr->rcsz,
		tr->id);

	print_field(tr->records, output);

	for( int i=0; tr->line_ids && tr->line_ids[i]; i++ ) {
		print_field(tr->line_ids[i], output);
	}

	for( int i=0; tr->sums && tr->sums[i]; i++ ) {
		print_field(tr->sums[i], output);
	}

	const struct flag_day flags[] = {
		{ "control_final", tr->control_final },
		{ "global", tr->global },
		{ "has_declarative", tr->has_declarative },
		{ "has_detail", tr->has_detail },
	};
	print_flags(flags, COUNT_OF(flags), output);
  }
}

static void
print_local_filename( const struct local_filename * local, FILE* output ) {
	fprintf(output,
		"local_name = %s\n"
		"local_include_name = %s\n"
		"local_fp = %p\n",
		local->local_name,
		local->local_include_name,
		local->local_fp);
}

static void
print_program(const struct cb_program *tr, FILE* output ) {
	if( !tr ) return;

	print_program(tr->next_program, output);
	print_program(tr->next_program_ordered, output);

	fprintf(output,
		"program_name = %s\n"
		"program_id = %s\n"
		"source_name = %s\n"
		"orig_program_id = %s\n",
		tr->program_name,
		tr->program_id,
		tr->source_name,
		tr->orig_program_id);

	if( tr->word_table ) {
		const struct cb_word * p = tr->word_table[0];
		while (p) {
			p = print_word(p, output);
		}
	}

	for( const struct local_filename *p = tr->local_include;
	     p != NULL; p = p->next ) {
		print_local_filename(p, output);
	}

	struct nested_list *list[] = {
		tr->nested_prog_list, tr->common_prog_list, NULL };

	for( struct nested_list **pp = list; *pp; pp++ ) {
		for( struct nested_list* p = *pp; p; p = p->next ) {
			print_program(p->nested_prog, output);
		}
	}

	cb_tree_print(tr->entry_list, output);
	cb_tree_print(tr->file_list, output);
	cb_tree_print(tr->cd_list, output);
	cb_tree_print(tr->exec_list, output);
	cb_tree_print(tr->label_list, output);
	cb_tree_print(tr->reference_list, output);
	cb_tree_print(tr->alphabet_name_list, output);
	cb_tree_print(tr->symbolic_char_list, output);
	cb_tree_print(tr->class_name_list, output);
	cb_tree_print(tr->parameter_list, output);
	cb_tree_print(tr->locale_list, output);
	cb_tree_print(tr->global_list, output);
	cb_tree_print(tr->report_list, output);
	cb_tree_print(tr->alter_list, output);
	cb_tree_print(tr->debug_list, output);
	cb_tree_print(tr->cb_return_code, output);
	cb_tree_print(tr->cb_sort_return, output);
	cb_tree_print(tr->cb_call_params, output);
	cb_tree_print(tr->mnemonic_spec_list, output);
	cb_tree_print(tr->class_spec_list, output);
	cb_tree_print(tr->interface_spec_list, output);
	cb_tree_print(tr->function_spec_list, output);
	cb_tree_print(tr->user_spec_list, output);
	cb_tree_print(tr->program_spec_list, output);
	cb_tree_print(tr->property_spec_list, output);

	//// NULL terminated list, or what? tr->alter_gotos

	print_field(tr->working_storage, output);
	print_field(tr->local_storage, output);
	print_field(tr->linkage_storage, output);
	print_field(tr->screen_storage, output);
	print_field(tr->report_storage, output);
	cb_tree_print(tr->local_file_list, output);
	cb_tree_print(tr->global_file_list, output);

	for( int i=0; i < COUNT_OF(tr->global_handler); i++ ) {
		print_label(tr->global_handler[i].handler_label, output);
		print_program(tr->global_handler[i].handler_prog, output);
	}

	cb_tree_print(tr->collating_sequence, output);
	cb_tree_print(tr->collating_sequence_n, output);
	cb_tree_print(tr->classification, output);
	cb_tree_print(tr->apply_commit, output);
	cb_tree_print(tr->cursor_pos, output);
	cb_tree_print(tr->crt_status, output);
	cb_tree_print(tr->xml_code, output);
	cb_tree_print(tr->xml_event, output);
	cb_tree_print(tr->xml_information, output);
	cb_tree_print(tr->xml_namespace, output);
	cb_tree_print(tr->xml_nnamespace, output);
	cb_tree_print(tr->xml_namespace_prefix, output);
	cb_tree_print(tr->xml_nnamespace_prefix, output);
	cb_tree_print(tr->xml_ntext, output);
	cb_tree_print(tr->xml_text, output);
	cb_tree_print(tr->json_code, output);
	cb_tree_print(tr->json_status, output);
	cb_tree_print(tr->returning, output);

	print_label(tr->all_procedure, output);

	print_call_xref(tr->call_xref, output);
	print_ml_generate_tree(tr->ml_trees, output);

	fprintf(output,
		"extfh = %s\n"
		"last_source_line = %d\n"
		"loop_counter = %d\n"
		"decimal_index = %u\n"
		"decimal_index_max = %u\n"
		"nested_level = %d\n"
		"num_proc_params = %u\n"
		"toplev_count = %d\n"
		"max_call_param = %u\n"
		"decimal_point = %c\n"
		"currency_symbol = %c\n"
		"numeric_separator = %c\n"
		"prog_type = %c\n",
		tr->extfh,
		tr->last_source_line,
		tr->loop_counter,
		tr->decimal_index,
		tr->decimal_index_max,
		tr->nested_level,
		tr->num_proc_params,
		tr->toplev_count,
		tr->max_call_param,
		tr->decimal_point,
		tr->currency_symbol,
		tr->numeric_separator,
		tr->prog_type);

        cb_tree_print(tr->entry_convention, output);

	struct flag_day flags[] = {
		{ "main", tr->flag_main },
		{ "common", tr->flag_common },
		{ "initial", tr->flag_initial },
		{ "recursive", tr->flag_recursive },
		{ "screen", tr->flag_screen },
		{ "validated", tr->flag_validated },
		{ "chained", tr->flag_chained },
		{ "global_use", tr->flag_global_use },
		{ "gen_error", tr->flag_gen_error },
		{ "file_global", tr->flag_file_global },
		{ "has_external", tr->flag_has_external },
		{ "segments", tr->flag_segments },
		{ "trailing_separate", tr->flag_trailing_separate },
		{ "console_is_crt", tr->flag_console_is_crt },
		{ "debugging", tr->flag_debugging },
		{ "gen_debug", tr->flag_gen_debug },
		{ "save_exception", tr->flag_save_exception },
		{ "report", tr->flag_report },
		{ "void", tr->flag_void }
	};
        print_flags(flags, COUNT_OF(flags), output);
}

static void
intrinsic_table_print(const struct cb_intrinsic_table *tr, FILE* output ) {
	fprintf(output,
		"name = %s\n"
		"intr_routine = %s\n"
		"intr_enum = %s\n"
		"token = %d\n"
		"active = %s\n"
		"args = %d\n"
		"min_args = %d\n"
		"category = %s\n"
		"refmod = %u\n",
		tr->name,
		tr->intr_routine,
		cb_intr_enum_str(tr->intr_enum),
		tr->token,
		cb_feature_mode_str(tr->active),
		tr->args,
		tr->min_args,
		cb_category_str(tr->category),
		tr->refmod);
}

static void
print_attr_struct( struct cb_attr_struct *tr, FILE* output ) {
  if( tr != NULL ){
	fprintf(output, "0x%llx\n", tr->dispattrs);

	cb_tree_print(tr->fgc, output);
	cb_tree_print(tr->bgc, output);
	cb_tree_print(tr->scroll, output);
	cb_tree_print(tr->timeout, output);
	cb_tree_print(tr->prompt, output);
	cb_tree_print(tr->size_is, output);
  }
}

void
cb_tree_print( const cb_tree tr, FILE *output ) {
	cb_tree subtree;

        if( tr == NULL ) return;
	if( output == NULL ) return;

	print_common(tr, output);

	switch( tr->tag ) {
	case CB_TAG_CONST:
		fprintf(output, "val = %s\n", CB_CONST(tr)->val);
		break;
	case CB_TAG_INTEGER:
		fprintf(output,
			"val = %d\n" // "hexvax = %x\n"
                        ,
			CB_INTEGER(tr)->val//, CB_INTEGER(tr)->hexval
                        );
		break;
	case CB_TAG_STRING: {
		const char *data = pretty_print(CB_STRING(tr)->data,
						CB_STRING(tr)->size);
		fprintf(output,
			"data = %s\n" "size = %zu\n",
			CB_STRING(tr)->data, CB_STRING(tr)->size);
	        } break;

	case CB_TAG_ALPHABET_NAME:
		print_alphabet_name(CB_ALPHABET_NAME(tr), output);
		break;

	case CB_TAG_CLASS_NAME:
	case CB_TAG_LOCALE_NAME:
		fprintf(output, "name = %s\n" "cname = %s\n",
			CB_LOCALE_NAME(tr)->name, CB_LOCALE_NAME(tr)->cname);

		switch(tr->tag) { // again
		case CB_TAG_CLASS_NAME:
			subtree = CB_CLASS_NAME(tr)->list;
			break;
		case CB_TAG_LOCALE_NAME:
			subtree = CB_LOCALE_NAME(tr)->list;
			break;
		default:
			assert(false);
		}
		cb_tree_print( subtree, output );

		break;

	case CB_TAG_SYSTEM_NAME:
		subtree = CB_SYSTEM_NAME(tr)->value;
		cb_tree_print( subtree, output );
		fprintf(output,
			"category = %s\n" "token = %d\n",
			cb_category_str(CB_SYSTEM_NAME(tr)->category),
			CB_SYSTEM_NAME(tr)->token);
		break;

	case CB_TAG_LITERAL:
		fprintf(output,
			"data = %s\n"
			"size = %d\n"
			"scale = %d\n"
			"llit = %d\n"
			"sign = %hd\n"
			"all = %hd\n",
			CB_LITERAL(tr)->data,
			CB_LITERAL(tr)->size,
			CB_LITERAL(tr)->scale,
			CB_LITERAL(tr)->llit,
			CB_LITERAL(tr)->sign,
			CB_LITERAL(tr)->all  );
		break;

	case CB_TAG_DECIMAL:
		fprintf(output, "id = %u\n", CB_DECIMAL(tr)->id);
		break;

	case CB_TAG_FIELD:
		print_field(CB_FIELD(tr), output);
		break;

	case CB_TAG_FILE:
		print_file(CB_FILE(tr), output);
		break;

	case CB_TAG_REPORT:
		print_report(CB_REPORT(tr), output);
		break;

	case CB_TAG_CD:
		fprintf(output, "name = %s\n" "field_debug = %d\n",
			CB_CD(tr)->name, CB_CD(tr)->flag_field_debug);
		print_field(CB_CD(tr)->record, output);
		print_label(CB_CD(tr)->debug_section, output);
		break;

	case CB_TAG_REFERENCE:
		print_reference(CB_REFERENCE(tr), output);
	        break;

	case CB_TAG_BINARY_OP:
		fprintf(output, "op = %d\n" "flag = %u\n",
			CB_BINARY_OP(tr)->op,
			CB_BINARY_OP(tr)->flag);

		cb_tree_print(CB_BINARY_OP(tr)->x, output);
		cb_tree_print(CB_BINARY_OP(tr)->y, output);
		break;

	case CB_TAG_FUNCALL:
		fprintf(output,
			"name = %s\n"
			"argc = %d\n"
			"varcnt = %d\n"
			"screenptr = %u\n"
			"nolitcast = %u\n"
			"tree:\n",
			CB_FUNCALL(tr)->name,
			CB_FUNCALL(tr)->argc,
			CB_FUNCALL(tr)->varcnt,
			CB_FUNCALL(tr)->screenptr,
			CB_FUNCALL(tr)->nolitcast);

		for( int i=0; i < COUNT_OF(CB_FUNCALL(tr)->argv); i++ ) {
			cb_tree p = CB_FUNCALL(tr)->argv[i];
			if (!p) break;
			cb_tree_print( p, output);
		}
		break;

	case CB_TAG_CAST:
		fprintf(output, "cast_type = %s\n",
			cb_cast_type_str(CB_CAST(tr)->cast_type));
		cb_tree_print(CB_CAST(tr)->val, output);
		break;

	case CB_TAG_INTRINSIC:
		fprintf(output, "isuser = %d\n", CB_INTRINSIC(tr)->isuser);

		cb_tree_print(CB_INTRINSIC(tr)->name, output);
		cb_tree_print(CB_INTRINSIC(tr)->args, output);
		cb_tree_print(CB_INTRINSIC(tr)->intr_field, output);

		intrinsic_table_print(CB_INTRINSIC(tr)->intr_tab, output);

		cb_tree_print(CB_INTRINSIC(tr)->offset, output);
		cb_tree_print(CB_INTRINSIC(tr)->length, output);
		break;

	/* Statements */
	case CB_TAG_LABEL:
		print_label(CB_LABEL(tr), output);
		break;

	case CB_TAG_ASSIGN:
		cb_tree_print(CB_ASSIGN(tr)->var, output);
		cb_tree_print(CB_ASSIGN(tr)->val, output);
		break;

	case CB_TAG_INITIALIZE:
		fprintf(output,
			"flag_default = 0x%x\n"
			"flag_init_statement = 0x%x\n"
			"flag_no_filler_init = 0x%x\n"
			"padding = 0x%x\n",
			CB_INITIALIZE(tr)->flag_default,
			CB_INITIALIZE(tr)->flag_init_statement,
			CB_INITIALIZE(tr)->flag_no_filler_init,
			CB_INITIALIZE(tr)->padding);

		cb_tree_print(CB_INITIALIZE(tr)->var, output);
		cb_tree_print(CB_INITIALIZE(tr)->val, output);
		cb_tree_print(CB_INITIALIZE(tr)->rep, output);
		break;

	case CB_TAG_SEARCH:
		fprintf(output, "flag_all = %d\n", CB_SEARCH(tr)->flag_all);
		cb_tree_print(CB_SEARCH(tr)->table, output);
		cb_tree_print(CB_SEARCH(tr)->var, output);
#ifdef COBC_VERSION4
		cb_tree_print(CB_SEARCH(tr)->end_stmt, output);
#else
		cb_tree_print(CB_SEARCH(tr)->at_end, output);
#endif
		cb_tree_print(CB_SEARCH(tr)->whens, output);
		break;

	case CB_TAG_CALL:
		fprintf(output,
			"is_system = %u\n"
			"convention = %d\n",
			CB_CALL(tr)->is_system,
			CB_CALL(tr)->convention);

		cb_tree_print(CB_CALL(tr)->name, output);
		cb_tree_print(CB_CALL(tr)->args, output);
		cb_tree_print(CB_CALL(tr)->stmt1, output);
		cb_tree_print(CB_CALL(tr)->stmt2, output);
		cb_tree_print(CB_CALL(tr)->call_returning, output);
		break;

	case CB_TAG_GOTO:
		cb_tree_print(CB_GOTO(tr)->target, output);
		cb_tree_print(CB_GOTO(tr)->depending, output);
		break;

	case CB_TAG_IF:
		fprintf(output, "is_if = %u\n", CB_IF(tr)->is_if);
		cb_tree_print(CB_IF(tr)->test, output);
		cb_tree_print(CB_IF(tr)->stmt1, output);
		cb_tree_print(CB_IF(tr)->stmt2, output);
		break;

	case CB_TAG_PERFORM:
		fprintf(output, "perform_type = %s\n",
			cb_perform_type_str(CB_PERFORM(tr)->perform_type));
		cb_tree_print(CB_PERFORM(tr)->test, output);
		cb_tree_print(CB_PERFORM(tr)->body, output);
		cb_tree_print(CB_PERFORM(tr)->data, output);
		cb_tree_print(CB_PERFORM(tr)->varying, output);
		cb_tree_print(CB_PERFORM(tr)->exit_label, output);
		cb_tree_print(CB_PERFORM(tr)->cycle_label, output);
		break;

	case CB_TAG_STATEMENT:
		fprintf(output, "name = %s\n" "handler_type = %s\n",
			CB_STATEMENT(tr)->name,
			cb_handler_type_str(CB_STATEMENT(tr)->handler_type));

		cb_tree_print(CB_STATEMENT(tr)->body, output);
		cb_tree_print(CB_STATEMENT(tr)->file, output);
		cb_tree_print(CB_STATEMENT(tr)->ex_handler, output);
		cb_tree_print(CB_STATEMENT(tr)->not_ex_handler, output);
		cb_tree_print(CB_STATEMENT(tr)->handler3, output);
		cb_tree_print(CB_STATEMENT(tr)->null_check, output);
		cb_tree_print(CB_STATEMENT(tr)->debug_check, output);
		cb_tree_print(CB_STATEMENT(tr)->debug_nodups, output);
#ifdef COBC_VERSION4
		cb_tree_print(CB_STATEMENT(tr)->retry, output);
#endif
                
		print_attr_struct(CB_STATEMENT(tr)->attr_ptr, output);

		struct flag_day flags[] = {
			{ "no_based", CB_STATEMENT(tr)->flag_no_based },
			{ "in_debug", CB_STATEMENT(tr)->flag_in_debug },
			{ "merge", CB_STATEMENT(tr)->flag_merge },
			{ "callback", CB_STATEMENT(tr)->flag_callback },
			{ "implicit", CB_STATEMENT(tr)->flag_implicit },
#ifdef COBC_VERSION4
			{ "retry_times", CB_STATEMENT(tr)->flag_retry_times },
			{ "retry_seconds",
			  CB_STATEMENT(tr)->flag_retry_seconds },
			{ "retry_forever",
			  CB_STATEMENT(tr)->flag_retry_forever },
			{ "advancing_lock",
			  CB_STATEMENT(tr)->flag_advancing_lock },
			{ "ignore_lock", CB_STATEMENT(tr)->flag_ignore_lock }
#endif
		};
		print_flags(flags, COUNT_OF(flags), output);
		break;

	case CB_TAG_CONTINUE:
		break; // no elements

	case CB_TAG_CANCEL:
		cb_tree_print(CB_CANCEL(tr)->target, output);
		break;

	case CB_TAG_ALTER:
		cb_tree_print(CB_ALTER(tr)->source, output);
		cb_tree_print(CB_ALTER(tr)->target, output);
		break;

	case CB_TAG_SET_ATTR:
		fprintf(output,
			"val_on  = %lld\n"
			"val_off = %lld\n",
			CB_SET_ATTR(tr)->val_on,
			CB_SET_ATTR(tr)->val_off);

		print_field(CB_SET_ATTR(tr)->fld, output);
		break;

		/* Miscellaneous */
	case CB_TAG_PERFORM_VARYING:
		cb_tree_print(CB_PERFORM_VARYING(tr)->name, output);
		cb_tree_print(CB_PERFORM_VARYING(tr)->from, output);
		cb_tree_print(CB_PERFORM_VARYING(tr)->step, output);
		cb_tree_print(CB_PERFORM_VARYING(tr)->until, output);
		break;

	case CB_TAG_PICTURE:
		print_picture(CB_PICTURE(tr), output);
		break;

	case CB_TAG_LIST:
		print_list(CB_LIST(tr), output);
		break;

	case CB_TAG_DIRECT:
		fprintf(output,
			"line = %s\n"
			"flag_is_direct = %u\n"
			"flag_new_line = %u\n",
			CB_DIRECT(tr)->line,
			CB_DIRECT(tr)->flag_is_direct,
			CB_DIRECT(tr)->flag_new_line);
		break;

		case CB_TAG_DEBUG:
			fprintf(output,
				"value = '%*s'\n"
				"size = %zu\n",
				CB_DEBUG(tr)->size?
				(int)CB_DEBUG(tr)->size :
				(int)strlen(CB_DEBUG(tr)->value),
				CB_DEBUG(tr)->value,
				CB_DEBUG(tr)->size);

			cb_tree_print(CB_DEBUG(tr)->target, output);
			cb_tree_print(CB_DEBUG(tr)->fld, output);
			break;

	case CB_TAG_DEBUG_CALL:
		print_label(CB_DEBUG_CALL(tr)->target, output);
		break;

	case CB_TAG_PROGRAM:
		print_program(CB_PROGRAM(tr), output);
		break;

	case CB_TAG_PROTOTYPE:
		fprintf(output,
			"name = %s"
			"ext_name = %s"
			"type = %d",
			CB_PROTOTYPE(tr)->name,
			CB_PROTOTYPE(tr)->ext_name,
			CB_PROTOTYPE(tr)->type);
		break;

	case CB_TAG_DECIMAL_LITERAL:
		fprintf(output, "id = %u\n", CB_DECIMAL(tr)->id);
		break;

	case CB_TAG_REPORT_LINE:
		fprintf(output,
			"what structure does CB_TAG_REPORT_LINE have?\n");
		break;

	case CB_TAG_ML_SUPPRESS:
		fprintf(output,
			"target = %s\n"
			"ml_type = %s\n"
			"category = %s\n",
			cb_ml_suppress_target_str(CB_ML_SUPPRESS(tr)->target),
			cb_ml_type_str(CB_ML_SUPPRESS(tr)->ml_type),
			cb_ml_suppress_category_str(CB_ML_SUPPRESS(tr)->category));

			cb_tree_print(CB_ML_SUPPRESS(tr)->identifier, output);
			cb_tree_print(CB_ML_SUPPRESS(tr)->when_list, output);
			break;

	case CB_TAG_ML_TREE:
		fprintf(output,
			"what structure does CB_TAG_ML_TREE have?\n");
		break;

	case CB_TAG_ML_SUPPRESS_CHECKS:
		print_ml_generate_tree(CB_ML_SUPPRESS_CHECKS(tr)->tree, output);
		break;
#ifdef COBC_VERSION4
	case CB_TAG_VARY:
		fprintf(output, "CB_TAG_VARY ???\n");
		break;
#endif
        }
}

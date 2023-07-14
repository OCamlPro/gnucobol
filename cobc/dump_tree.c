
#include "dump_tree_gen.c"

void cb_dump_tree_to_file (struct cb_program *prog, const char *filename, const char* flags)
{
	cb_print (prog);
}


#if 0
/*
 Copyright (C) 2001-2023 Free Software Foundation, Inc.
 Written by Keisuke Nishida, Roger While, Simon Sobisch, Ron Norman,
 Edward Hart, Fabrice Le Fessant

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
#include <sys/stat.h>

#include "cobc.h"
#include "../libcob/coblocal.h"
#include "tree.h"

#define INDENT_STEP 3
#define MAX_INDENT 200

/* used to create the indentation spaces */
static char space_buffer[INDENT_STEP*MAX_INDENT+1];

/* used to know if we have already encountered a pointer */
static void *pointer_buffer[MAX_INDENT];

/* used to know if we need a record/list internal delimiter */
static void *nfields_buffer[MAX_INDENT];

/* current indentation */
static int indent = 0;

/* field descriptor of tree dump */
static FILE* fd = NULL;

static const int max_spaces = INDENT_STEP * MAX_INDENT;

/* flag: whether we should print cb_tree_common header */
int cb_dump_tree_with_common = 0;

/* flag: whether we should print locations, only in with_common=1 mode */
int cb_dump_tree_with_loc = 0;

/* flag: whether we should print types */
int cb_dump_tree_with_type = 0;

/* flag: whether we should add a field with record pointer address */
int cb_dump_tree_with_pointer = 0;

/* flag: whether we should indent the file */
int cb_dump_tree_with_indent = 1;

/* flag: whether we should print newlines in the file */
int cb_dump_tree_with_newlines = 1;

/* flag: print a message at the end */
int cb_dump_tree_print_message = 1;

static void indent_init (void)
{
	int i;
	for (i=0; i< max_spaces; i++) space_buffer[i] = ' ';
	space_buffer[max_spaces] = 0;
}

static void dump_indent (void)
{
	if (cb_dump_tree_with_indent && cb_dump_tree_with_newlines){
		int nspaces = INDENT_STEP * indent;
		if (nspaces > max_spaces) nspaces = max_spaces;
		fprintf (fd, "%s", space_buffer + max_spaces - nspaces);
	}
}

static const char* newline(void)
{
	if (cb_dump_tree_with_newlines) return "\n";
	return "";
}

static int known_pointer (void* x)
{
	int i ;
	for (i=0; i<indent; i++) {
		if (pointer_buffer[i] == x) return 1;
	}
	pointer_buffer[indent] = x;
	return 0;
}

enum formatter_kind {
	FORMAT_JSON,
	FORMAT_OCAML
};

typedef struct formatter {
	enum formatter_kind format_kind;
	const char* format_header ;
	const char* format_trailer ;

	const char* format_begin_field ;
	const char* format_end_field ;

	const char* format_begin_record ;
	const char* format_delim_record ;
	const char* format_last_delim_record ;
	const char* format_end_record ;

	const char* format_begin_list ;
	const char* format_delim_list ;
	const char* format_last_delim_list ;
	const char* format_end_list ;

	const char* format_begin_flags;
	const char* format_begin_flag;
	const char* format_end_flag;
	const char* format_end_flags;

	const char* format_null ;
	const char* format_begin_constr ;
	const char* format_delim_constr ;
	const char* format_end_constr ;
	const char* format_begin_pointer ;
	const char* format_end_pointer ;
	const char* format_begin_char ;
	const char* format_end_char ;
	const char* format_begin_string ;
	const char* format_end_string ;
	const char* format_begin_int ;
	const char* format_end_int ;
} formatter;

static struct formatter fmt;

static void set_ocaml_format(void)
{
	/* OCaml format. Suitable to load inside the OCaml
	   interpreter for automated processing.

	   If you change the format of type in format_header, you should also
	   modify the format_date variable.
	*/

	fmt.format_kind = FORMAT_OCAML;
	fmt.format_header = "\n"
		"type t =\n"
		"| NULL\n"
		"| INT of int\n"
		"| CHAR of char\n"
		"| STRING of string\n"
		"| CONSTR of string * t\n"
		"| LIST of t list\n"
		"| RECORD of ( string * t) list\n"
		"| POINTER of int64\n"
		"| FLAGS of string list\n"
		"let format_date = \"GNUCOBOL-2023-07-15\"\n"
		"\n"
		"let tree =\n"
		;

	fmt.format_trailer = "\n"
		"let () =\n"
		"  let oc = open_out_bin \"tree.cbb\" in\n"
		"  output_value oc ( format_date : string );\n"
		"  output_value oc ( tree : t);\n"
		"  close_out oc\n"
		"\n";
	fmt.format_begin_field = "\"";
	fmt.format_end_field = "\", ";

	fmt.format_begin_record = "RECORD [";
	fmt.format_delim_record = ";" ;
	fmt.format_last_delim_record = ";";
	fmt.format_end_record = "]";

	fmt.format_begin_list = "LIST [";
	fmt.format_delim_list =  ";";
	fmt.format_last_delim_list =  ";";
	fmt.format_end_list = "]";

	fmt.format_null =   "NULL";
	fmt.format_begin_constr = "CONSTR (\"";
	fmt.format_delim_constr = "\", ";
	fmt.format_end_constr = ")";
	fmt.format_begin_pointer = "POINTER ";
	fmt.format_end_pointer = "L";
	fmt.format_begin_char = "CHAR '";
	fmt.format_end_char = "'";
	fmt.format_begin_string = "STRING \"";
	fmt.format_end_string = "\"";
	fmt.format_begin_int = "INT( ";
	fmt.format_end_int = ")";

	fmt.format_begin_flags = "FLAGS [";
	fmt.format_begin_flag = "\"";
	fmt.format_end_flag = "\";";
	fmt.format_end_flags = "]";
}

static void set_json_format (void)
{
	/* Default is to use JSON */
	/* Standard JSON format. Tests validated by jsonlint-php. */

	fmt.format_kind = FORMAT_JSON;
	fmt.format_header = "";
	fmt.format_trailer = "";

	fmt.format_begin_field = "\"";
	fmt.format_end_field = "\": ";

	fmt.format_begin_record = "{";
	fmt.format_delim_record = ",";
	fmt.format_last_delim_record = "";
	fmt.format_end_record = "}";

	fmt.format_begin_list = "[";
	fmt.format_delim_list = ",";
	fmt.format_last_delim_list = "";
	fmt.format_end_list = "]";

	fmt.format_begin_flags = "[";
	fmt.format_begin_flag = "\"";
	fmt.format_end_flag = "\",";
	fmt.format_end_flags = " NULL]";

	fmt.format_null = "null";
	fmt.format_begin_constr = "{ \"type_\": \"";
	fmt.format_delim_constr = "\", \"value_\": ";
	fmt.format_end_constr = " }";
	fmt.format_begin_pointer = "\"";
	fmt.format_end_pointer = "\"";
	fmt.format_begin_char =  "\"";
	fmt.format_end_char = "\"";
	fmt.format_begin_string =  "\"";
	fmt.format_end_string = "\"";
	fmt.format_begin_int = "";
	fmt.format_end_int = "";
}

static
void set_format_by_file_ext (const char* filename)
{
	const char *arg = filename + strlen(filename);

	while ( arg > filename && *arg != '.' ) arg--;

	if (!strcasecmp(arg, ".ml")){

		set_ocaml_format ();

		cb_dump_tree_with_common = 1;
		cb_dump_tree_with_loc = 1;
		cb_dump_tree_with_type = 1;
		cb_dump_tree_with_pointer = 1;
		cb_dump_tree_with_indent = 1;
		cb_dump_tree_with_newlines = 1;

		return;
	}

	set_json_format ();
}

static
void set_flags (const char* flags)
{
	int sign = 1;
	int i;
	int len = strlen(flags);

	for(i=0; i< len; i++){
		switch(flags[i]){
		case '+': sign = 1; break;
		case '-': sign = 0; break;
		case 'c':
			cb_dump_tree_with_common = sign;
			break;
		case 'l':
			cb_dump_tree_with_loc = sign;
			if (cb_dump_tree_with_loc){
				cb_dump_tree_with_common = 1;
			}
			break;
		case 't':
			cb_dump_tree_with_type = sign;
			break;
		case 'p':
			cb_dump_tree_with_pointer = sign;
			break;
		case 'i':
			cb_dump_tree_with_indent = sign;
			if (cb_dump_tree_with_indent)
				cb_dump_tree_with_newlines = 1;
			break;
		case 'n':
			cb_dump_tree_with_newlines = sign;
			if (!cb_dump_tree_with_newlines)
				cb_dump_tree_with_indent = 0;
			break;
		case 'm':
			cb_dump_tree_print_message = sign;
		case 'J':
			set_json_format ();
			break;
		case 'O':
			set_ocaml_format ();
			break;
		case 'A':
			cb_dump_tree_with_loc = sign;
			cb_dump_tree_with_common = sign;
			cb_dump_tree_with_type = sign;
			cb_dump_tree_with_pointer = sign;
			cb_dump_tree_with_indent = 1-sign;
			cb_dump_tree_with_newlines = 1-sign;
			break;
		default:
			cobc_err_exit ("--dump-tree-flags: unknown flag '%c', expecting '+-cltpinA'", flags[i]);
		}
	}
}

#define FIELD_NAME(field_name)						\
	dump_sequence_delim (fmt.format_delim_record);		\
	dump_indent();						\
	fprintf (fd, "%s%s%s", fmt.format_begin_field, field_name, fmt.format_end_field); \
	fflush (fd);

#define FIELD_SET(field_name) FIELD_NAME(#field_name)

#define FIELD(struct_name, field_name)					\
	if ( x -> field_name ){						\
		FIELD_SET(field_name);					\
		dump_##struct_name ( x -> field_name );		\
		fflush (fd);						\
	}

#define FIELD_INLINE(struct_name, field_name)				\
	FIELD_SET(field_name);						\
	dump_##struct_name ( & x -> field_name );			\
	fflush (fd);


#define FIELD_INLINE_TODO(struct_name, field_name)			\
	FIELD_SET(field_name);						\
	dump_constr_string ("TODO", #struct_name);			\
	fflush (fd);

#define FIELD_TODO(struct_name, field_name)				\
	if ( x -> field_name ){						\
		FIELD_INLINE_TODO (struct_name, field_name);		\
	}

#define FIELD_STOP(struct_name, field_name)				\
	if ( x -> field_name ){						\
		FIELD_SET(field_name);					\
		dump_constr_pointer ("STOP", x);			\
		fflush (fd);						\
	}

#define BEGIN_COMMON_RECORD()						\
	fprintf (fd, "%s", fmt.format_begin_record);			\
	nfields_buffer[indent] = 0;					\
	indent++

#define END_RECORD()							\
	dump_sequence_end (fmt.format_last_delim_record);		\
	indent--;							\
	dump_indent();						\
	fprintf (fd, "%s", fmt.format_end_record)

#define BEGIN_RECORD(struct_name)					\
	if (!x) { fprintf (fd, "%s", fmt.format_null); return; }	\
	BEGIN_COMMON_RECORD ();						\
	if (cb_dump_tree_with_type){					\
		FIELD_SET(type_);					\
		dump_string (#struct_name);				\
	}								\
	if (cb_dump_tree_with_pointer){				\
		FIELD_SET(address_);					\
		dump_pointer (x);					\
	}								\
	if (known_pointer(x)) {						\
		FIELD_SET(ellipsis_);					\
		dump_int (1);						\
		END_RECORD ();						\
		return;							\
	}

#define BEGIN_TREE_RECORD(struct_name)				\
	BEGIN_RECORD(struct_name);				\
	FIELD_INLINE (cb_tree_common, common)

#define END_LIST()						\
	dump_sequence_end (fmt.format_last_delim_record);	\
	indent--;						\
	dump_indent ();					\
	fprintf (fd, "%s", fmt.format_end_list)

#define BEGIN_LIST()							\
	fprintf (fd, "%s", fmt.format_begin_list);			\
	if (!x) { fprintf(fd, "%s", fmt.format_end_list); return; }	\
	nfields_buffer[indent] = 0;					\
	indent++

#define LIST_ELEM(struct_name, elem)				\
	dump_sequence_delim (fmt.format_delim_list);		\
	dump_indent();					\
	dump_##struct_name (elem);

#define CASE(constr)	case constr: return #constr

static void dump_cb_tree (cb_tree x);
static void dump_cb_field (struct cb_field *x);
static void dump_cb_label (struct cb_label *x);
static void dump_cb_file (struct cb_file *x);
static void dump_cb_cd (struct cb_cd *x);
static void dump_cb_report (struct cb_report *x);

static
void dump_sequence_delim (const char *delim)
{
	if (nfields_buffer[indent-1])
		fprintf (fd, "%s%s", delim, newline());
	else
		fprintf (fd, "%s", newline());
	nfields_buffer[indent-1]++;
}

static
void dump_sequence_end (const char *delim)
{
	if (nfields_buffer[indent-1])
		fprintf (fd, "%s%s", delim, newline());
	else
		fprintf (fd, "%s", newline());
}

static
void dump_int (int x)
{
	fprintf (fd, "%s%d%s", fmt.format_begin_int, x, fmt.format_end_int);
}

static
void dump_uint (unsigned int x)
{
	fprintf (fd, "%s%d%s", fmt.format_begin_int, x, fmt.format_end_int);
}

static
void dump_cob_u32_t (cob_u32_t x)
{
	fprintf (fd, "%s%d%s", fmt.format_begin_int, x, fmt.format_end_int);
}

static
void dump_size_t (size_t x)
{
	fprintf (fd, "%s%lu%s", fmt.format_begin_int, x, fmt.format_end_int);
}

static
void dump_uchar (unsigned char x)
{
	fprintf (fd, "%s%c%s", fmt.format_begin_char, x, fmt.format_end_char);
}

static
void dump_string (const char* x)
{
	if (x){
		int len = strlen(x);
		int found = -1;
		int i;
		for (i=0; i<len; i++){
			char c = x[i];
			switch (c){
			case '"':
			case '\\':
				found = i;
			default:
				if (c < 32) found = i;
			}
		}
		fprintf (fd, "%s", fmt.format_begin_string);
		if (found >= 0){
			for (i=0; i<=found; i++){
				int c = x[i];
				switch (c){
				case '"':
				case '\\':
					fputc ('\\', fd);
				}
				if (c<32){
					if (c<0) c = 256+c;
					switch (fmt.format_kind){
					case FORMAT_OCAML:
						fprintf (fd, "\\%03d", c);
						break;
					case FORMAT_JSON:
						fprintf (fd, "\\u%04x", c);
						break;
					}
				} else {
					fputc (c, fd);
				}
			}
			x = x + found+1;
		}
		fprintf (fd, "%s", x);
		fprintf (fd, "%s", fmt.format_end_string);
	} else {
		fprintf (fd, "%s", fmt.format_null);
	}
}

static
void dump_constr_string (const char* type, const char* value)
{
	if (cb_dump_tree_with_common){
		fprintf (fd, "%s%s%s", fmt.format_begin_constr, type, fmt.format_delim_constr);
		dump_string (value);
		fprintf (fd, "%s", fmt.format_end_constr);
	} else {
		dump_string (value);
	}
}

static
void dump_pointer (void* x)
{
	fprintf (fd, "%s%p%s", fmt.format_begin_pointer, x, fmt.format_end_pointer);
}

static
void dump_constr_pointer (const char* type, void* value)
{
	if (cb_dump_tree_with_pointer){
		fprintf (fd, "%s%s%s", fmt.format_begin_constr, type, fmt.format_delim_constr);
		dump_pointer (value);
		fprintf (fd, "%s", fmt.format_end_constr);
	} else {
		dump_string (type);
	}
}

static
void dump_ustring (const unsigned char* x)
{
	dump_string( (const char*)x);
}

static
void dump_cb_tag (enum cb_tag x)
{
	dump_constr_string ("cb_tag", cb_enum_explain (x));
}

static
const char* string_of_cb_category (enum cb_category x)
{
	switch (x){
		CASE (CB_CATEGORY_UNKNOWN);
		CASE (CB_CATEGORY_ALPHABETIC);
		CASE (CB_CATEGORY_ALPHANUMERIC);
		CASE (CB_CATEGORY_ALPHANUMERIC_EDITED);
		CASE (CB_CATEGORY_BOOLEAN);
		CASE (CB_CATEGORY_INDEX);
		CASE (CB_CATEGORY_NATIONAL);
		CASE (CB_CATEGORY_NATIONAL_EDITED);
		CASE (CB_CATEGORY_NUMERIC);
		CASE (CB_CATEGORY_NUMERIC_EDITED);
		CASE (CB_CATEGORY_OBJECT_REFERENCE);
		CASE (CB_CATEGORY_DATA_POINTER);
		CASE (CB_CATEGORY_PROGRAM_POINTER);
		CASE (CB_CATEGORY_FLOATING_EDITED);
		CASE (CB_CATEGORY_ERROR);
	}
	return "CB_CATEGORY_UNKNOWN";
}

static
void dump_cb_category (enum cb_category x)
{
	dump_constr_string ("cb_category", string_of_cb_category(x));
}

#if 0
const char* cb_explain_class (enum cb_class x)
{
	switch (x){
	CASE (CB_CLASS_UNKNOWN);
	CASE (CB_CLASS_ALPHABETIC);
	CASE (CB_CLASS_ALPHANUMERIC);
	CASE (CB_CLASS_BOOLEAN);
	CASE (CB_CLASS_INDEX);
	CASE (CB_CLASS_NATIONAL);
	CASE (CB_CLASS_NUMERIC);
	CASE (CB_CLASS_OBJECT);
	CASE (CB_CLASS_POINTER);
	}
	return "CB_CLASS_UNKNOWN";
}
#endif

/* Storage sections */
static
const char* string_of_cb_storage (enum cb_storage x)
{
	switch (x){
	CASE (CB_STORAGE_CONSTANT);
	CASE (CB_STORAGE_FILE);
	CASE (CB_STORAGE_WORKING);
	CASE (CB_STORAGE_LOCAL);
	CASE (CB_STORAGE_LINKAGE);
	CASE (CB_STORAGE_SCREEN);
	CASE (CB_STORAGE_REPORT);
	CASE (CB_STORAGE_COMMUNICATION);
	}
	return "CB_STORAGE_UNKNOWN";
}

static
void dump_cb_storage (enum cb_storage x)
{
	dump_constr_string ( "cb_storage", string_of_cb_storage (x));
}

static
const char* string_of_cob_statement (enum cob_statement x)
{
	switch (x){
	case STMT_UNKNOWN: return "STMT_UNKNOWN";
#define COB_STATEMENT(stmt, string) case stmt: return string;
#include "../libcob/statement.def"	/* located and installed next to common.h */
	case STMT_MAX_ENTRY: return "STMT_MAX_ENTRY";
	}
	return "COB_STATEMENT_UNKNOWN";
}

static
void dump_cob_statement (enum cob_statement x)
{
	dump_constr_string ( "cob_statement", string_of_cob_statement (x));
}


static
const char* string_of_cb_usage (enum cb_usage x)
{
	switch (x){
	CASE (CB_USAGE_BINARY);
	CASE (CB_USAGE_BIT);
	CASE (CB_USAGE_COMP_5);
	CASE (CB_USAGE_COMP_X);
	CASE (CB_USAGE_DISPLAY);
	CASE (CB_USAGE_FLOAT);
	CASE (CB_USAGE_DOUBLE);
	CASE (CB_USAGE_INDEX);
	CASE (CB_USAGE_NATIONAL);
	CASE (CB_USAGE_OBJECT);
	CASE (CB_USAGE_PACKED);
	CASE (CB_USAGE_POINTER);
	CASE (CB_USAGE_LENGTH);
	CASE (CB_USAGE_PROGRAM_POINTER);
	CASE (CB_USAGE_UNSIGNED_CHAR);
	CASE (CB_USAGE_SIGNED_CHAR);
	CASE (CB_USAGE_UNSIGNED_SHORT);
	CASE (CB_USAGE_SIGNED_SHORT);
	CASE (CB_USAGE_UNSIGNED_INT);
	CASE (CB_USAGE_SIGNED_INT);
	CASE (CB_USAGE_UNSIGNED_LONG);
	CASE (CB_USAGE_SIGNED_LONG);
	CASE (CB_USAGE_COMP_6);
	CASE (CB_USAGE_FP_DEC64);
	CASE (CB_USAGE_FP_DEC128);
	CASE (CB_USAGE_FP_BIN32);
	CASE (CB_USAGE_FP_BIN64);
	CASE (CB_USAGE_FP_BIN128);
	CASE (CB_USAGE_LONG_DOUBLE);
	CASE (CB_USAGE_HNDL);
	CASE (CB_USAGE_HNDL_WINDOW);
	CASE (CB_USAGE_HNDL_SUBWINDOW);
	CASE (CB_USAGE_HNDL_FONT);
	CASE (CB_USAGE_HNDL_THREAD);
	CASE (CB_USAGE_HNDL_MENU);
	CASE (CB_USAGE_HNDL_VARIANT);
	CASE (CB_USAGE_HNDL_LM);
	CASE (CB_USAGE_COMP_N);
	CASE (CB_USAGE_ERROR);
	}
	return "CB_USAGE_UNKNOWN";
}

static
void dump_cb_usage (enum cb_usage x)
{
	dump_constr_string ( "cb_usage", string_of_cb_usage (x));
}


static
const char* string_of_cb_cast_type (enum cb_cast_type x)
{
	switch (x){
	CASE (CB_CAST_INTEGER);
	CASE (CB_CAST_NEGATIVE_INTEGER);
	CASE (CB_CAST_LONG_INT);
	CASE (CB_CAST_NEGATIVE_LONG_INT);
	CASE (CB_CAST_ADDRESS);
	CASE (CB_CAST_ADDR_OF_ADDR);
	CASE (CB_CAST_LENGTH);
	CASE (CB_CAST_PROGRAM_POINTER);
	}
	return "CB_CAST_UNKNWON";
}

static
void dump_cb_cast_type (enum cb_cast_type x)
{
	dump_constr_string ( "cb_cast_type", string_of_cb_cast_type (x));
}

#if 0

static
const char* string_of_cb_intr_enum (enum cb_intr_enum x)
{
	switch (x){
	CASE (CB_INTR_ABS);
	CASE (CB_INTR_ACOS);
	CASE (CB_INTR_ANNUITY);
	CASE (CB_INTR_ASIN);
	CASE (CB_INTR_ATAN);
	CASE (CB_INTR_BASECONVERT);
	CASE (CB_INTR_BIT_OF);
	CASE (CB_INTR_BIT_TO_CHAR);
	CASE (CB_INTR_BOOLEAN_OF_INTEGER);
	CASE (CB_INTR_BYTE_LENGTH);
	CASE (CB_INTR_CHAR);
	CASE (CB_INTR_CHAR_NATIONAL);
	CASE (CB_INTR_COMBINED_DATETIME);
	CASE (CB_INTR_CONCATENATE);
	CASE (CB_INTR_CONTENT_LENGTH);
	CASE (CB_INTR_CONTENT_OF);
	CASE (CB_INTR_CONVERT);
	CASE (CB_INTR_COS);
	CASE (CB_INTR_CURRENCY_SYMBOL);
	CASE (CB_INTR_CURRENT_DATE);
	CASE (CB_INTR_DATE_OF_INTEGER);
	CASE (CB_INTR_DATE_TO_YYYYMMDD);
	CASE (CB_INTR_DAY_OF_INTEGER);
	CASE (CB_INTR_DAY_TO_YYYYDDD);
	CASE (CB_INTR_DISPLAY_OF);
	CASE (CB_INTR_E);
	CASE (CB_INTR_EXCEPTION_FILE);
	CASE (CB_INTR_EXCEPTION_FILE_N);
	CASE (CB_INTR_EXCEPTION_LOCATION);
	CASE (CB_INTR_EXCEPTION_LOCATION_N);
	CASE (CB_INTR_EXCEPTION_STATEMENT);
	CASE (CB_INTR_EXCEPTION_STATUS);
	CASE (CB_INTR_EXP);
	CASE (CB_INTR_EXP10);
	CASE (CB_INTR_FACTORIAL);
	CASE (CB_INTR_FIND_STRING);
	CASE (CB_INTR_FORMATTED_CURRENT_DATE);
	CASE (CB_INTR_FORMATTED_DATE);
	CASE (CB_INTR_FORMATTED_DATETIME);
	CASE (CB_INTR_FORMATTED_TIME);
	CASE (CB_INTR_FRACTION_PART);
	CASE (CB_INTR_HEX_OF);
	CASE (CB_INTR_HEX_TO_CHAR);
	CASE (CB_INTR_HIGHEST_ALGEBRAIC);
	CASE (CB_INTR_INTEGER);
	CASE (CB_INTR_INTEGER_OF_BOOLEAN);
	CASE (CB_INTR_INTEGER_OF_DATE);
	CASE (CB_INTR_INTEGER_OF_DAY);
	CASE (CB_INTR_INTEGER_OF_FORMATTED_DATE);
	CASE (CB_INTR_INTEGER_PART);
	CASE (CB_INTR_LENGTH);
	CASE (CB_INTR_LOCALE_COMPARE);
	CASE (CB_INTR_LOCALE_DATE);
	CASE (CB_INTR_LOCALE_TIME);
	CASE (CB_INTR_LOCALE_TIME_FROM_SECS);
	CASE (CB_INTR_LOG);
	CASE (CB_INTR_LOG10);
	CASE (CB_INTR_LOWER_CASE);
	CASE (CB_INTR_LOWEST_ALGEBRAIC);
	CASE (CB_INTR_MAX);
	CASE (CB_INTR_MEAN);
	CASE (CB_INTR_MEDIAN);
	CASE (CB_INTR_MIDRANGE);
	CASE (CB_INTR_MIN);
	CASE (CB_INTR_MOD);
	CASE (CB_INTR_MODULE_CALLER_ID);
	CASE (CB_INTR_MODULE_DATE);
	CASE (CB_INTR_MODULE_FORMATTED_DATE);
	CASE (CB_INTR_MODULE_ID);
	CASE (CB_INTR_MODULE_NAME);
	CASE (CB_INTR_MODULE_PATH);
	CASE (CB_INTR_MODULE_SOURCE);
	CASE (CB_INTR_MODULE_TIME);
	CASE (CB_INTR_MON_DECIMAL_POINT);
	CASE (CB_INTR_MON_THOUSANDS_SEP);
	CASE (CB_INTR_NATIONAL_OF);
	CASE (CB_INTR_NUM_DECIMAL_POINT);
	CASE (CB_INTR_NUM_THOUSANDS_SEP);
	CASE (CB_INTR_NUMVAL);
	CASE (CB_INTR_NUMVAL_C);
	CASE (CB_INTR_NUMVAL_F);
	CASE (CB_INTR_ORD);
	CASE (CB_INTR_ORD_MAX);
	CASE (CB_INTR_ORD_MIN);
	CASE (CB_INTR_PI);
	CASE (CB_INTR_PRESENT_VALUE);
	CASE (CB_INTR_RANDOM);
	CASE (CB_INTR_RANGE);
	CASE (CB_INTR_REM);
	CASE (CB_INTR_REVERSE);
	CASE (CB_INTR_SECONDS_FROM_FORMATTED_TIME);
	CASE (CB_INTR_SECONDS_PAST_MIDNIGHT);
	CASE (CB_INTR_SIGN);
	CASE (CB_INTR_SIN);
	CASE (CB_INTR_SQRT);
	CASE (CB_INTR_STANDARD_COMPARE);
	CASE (CB_INTR_STANDARD_DEVIATION);
	CASE (CB_INTR_STORED_CHAR_LENGTH);
	CASE (CB_INTR_SUBSTITUTE);
	CASE (CB_INTR_SUBSTITUTE_CASE);
	CASE (CB_INTR_SUM);
	CASE (CB_INTR_TAN);
	CASE (CB_INTR_TEST_DATE_YYYYMMDD);
	CASE (CB_INTR_TEST_DAY_YYYYDDD);
	CASE (CB_INTR_TEST_FORMATTED_DATETIME);
	CASE (CB_INTR_TEST_NUMVAL);
	CASE (CB_INTR_TEST_NUMVAL_C);
	CASE (CB_INTR_TEST_NUMVAL_F);
	CASE (CB_INTR_TRIM);
	CASE (CB_INTR_UPPER_CASE);
	CASE (CB_INTR_USER_FUNCTION);
	CASE (CB_INTR_VARIANCE);
	CASE (CB_INTR_WHEN_COMPILED);
	CASE (CB_INTR_YEAR_TO_YYYY);
	}
	return "CB_INTR_ENUM_UNKNOWN";
}

static
void dump_cb_intr_enum (enum cb_intr_enum x)
{
	dump_constr_string ( "cb_intr_enum", string_of_cb_intr_enum (x));
}
#endif

static
const char* string_of_cb_perform_type (enum cb_perform_type x)
{
	switch (x){
	CASE (CB_PERFORM_EXIT);
	CASE (CB_PERFORM_ONCE);
	CASE (CB_PERFORM_TIMES);
	CASE (CB_PERFORM_UNTIL);
	CASE (CB_PERFORM_FOREVER);
	}
	return "CB_PERFORM_UNKNOWN";
}

static
void dump_cb_perform_type (enum cb_perform_type x)
{
	dump_constr_string ( "cb_perform_type", string_of_cb_perform_type (x));
}

static
const char* string_of_cb_index_type (enum cb_index_type x)
{
	switch (x){
	CASE (CB_NORMAL_INDEX);
	CASE (CB_INT_INDEX);
	CASE (CB_STATIC_INT_INDEX);
	CASE (CB_STATIC_INT_VARYING);
	}
	return "CB_INDEX_TYPE_UNKNOWN";
}

static
void dump_cb_index_type (enum cb_index_type x)
{
	dump_constr_string ( "cb_index_type", string_of_cb_index_type (x));
}


static
void dump_cb_trees (cb_tree* x)
{
	int i;
	BEGIN_LIST ();
	for (i=0; x[i] != NULL; i++){
		LIST_ELEM(cb_tree, x[i]);
	}
	END_LIST ();
}

static
void dump_cb_tree_common (struct cb_tree_common *x)
{
	if (cb_dump_tree_with_common){
		BEGIN_COMMON_RECORD ();
		FIELD (cb_tag, tag);
		FIELD (cb_category, category);
		if (cb_dump_tree_with_loc) {
			FIELD (string, source_file);
			FIELD (int,source_line);
			FIELD (int,source_column);
		}
		END_RECORD ();
	} else {
		dump_cb_tag (x->tag);
	}
}

static
const char* string_of_cb_ml_type (enum cb_ml_type x)
{
	switch(x){
	CASE (CB_ML_ATTRIBUTE);
	CASE (CB_ML_ELEMENT);
	CASE (CB_ML_CONTENT);
	CASE (CB_ML_ANY_TYPE);
	}
	return "CB_ML_TYPE_UNKNOWN";
}

static
void dump_cb_ml_type (enum cb_ml_type x)
{
	dump_constr_string ("cb_ml_type", string_of_cb_ml_type(x));
}

static
const char* string_of_cob_module_type (enum cob_module_type x)
{
	switch(x){
	CASE (COB_MODULE_TYPE_PROGRAM);
	CASE (COB_MODULE_TYPE_FUNCTION);
	}
	return "COB_MODULE_TYPE_UNKNOWN";
}

static
void dump_cob_module_type (enum cob_module_type x)
{
	dump_constr_string ("cob_module_type", string_of_cob_module_type(x));
}


static
const char* string_of_cb_system_name_category (enum cb_system_name_category x)
{
	switch(x){
		CASE(CB_DEVICE_NAME);
		CASE(CB_SWITCH_NAME);
		CASE(CB_FEATURE_NAME);
		CASE(CB_CALL_CONVENTION_NAME);
		CASE(CB_CODE_NAME);
		CASE(CB_COMPUTER_NAME);
		CASE(CB_EXTERNAL_LOCALE_NAME);
		CASE(CB_LIBRARY_NAME);
		CASE(CB_TEXT_NAME);
	}
	return "CB_SYSTEM_NAME_CATEGORY_UNKNOWN";
}

static
void dump_cb_system_name_category (enum cb_system_name_category x)
{
	dump_constr_string ("cb_system_name_category", string_of_cb_system_name_category(x));
}

static
const char* string_of_cb_assign_type (enum cb_assign_type x)
{
	switch(x){
		CASE(CB_ASSIGN_VARIABLE_DEFAULT);
		CASE(CB_ASSIGN_VARIABLE_REQUIRED);
		CASE(CB_ASSIGN_EXT_FILE_NAME_REQUIRED);
	}
	return "CB_ASSIGN_TYPE_UNKNOWN";
}

static
void dump_cb_assign_type (enum cb_assign_type x)
{
	dump_constr_string ("cb_assign_type", string_of_cb_assign_type(x));
}

static
const char* string_of_cob_file_org (enum cob_file_org x)
{
	switch(x){
	CASE(COB_ORG_SEQUENTIAL);
	CASE(COB_ORG_LINE_SEQUENTIAL);
	CASE(COB_ORG_RELATIVE);
	CASE(COB_ORG_INDEXED);
	CASE(COB_ORG_SORT);
	CASE(COB_ORG_MAX);
	CASE(COB_ORG_MESSAGE);
	}
	return "COB_FILE_ORG_UNKNOWN";
}

static
void dump_cob_file_org (enum cob_file_org x)
{
	dump_constr_string ("cob_file_org", string_of_cob_file_org(x));
}

static
const char* string_of_cob_file_access_mode (enum cob_file_access_mode x)
{
	switch(x){
	CASE(COB_ACCESS_UNDEFINED);
	CASE(COB_ACCESS_SEQUENTIAL);
	CASE(COB_ACCESS_DYNAMIC);
	CASE(COB_ACCESS_RANDOM);
	}
	return "COB_FILE_ACCESS_MODE_UNKNOWN";
}

static
void dump_cob_file_access_mode (enum cob_file_access_mode x)
{
	dump_constr_string ("cob_file_access_mode", string_of_cob_file_access_mode(x));
}

static
const char* string_of_cob_open_mode (enum cob_open_mode x)
{
	switch(x){
	CASE(COB_OPEN_CLOSED);
	CASE(COB_OPEN_INPUT);
	CASE(COB_OPEN_OUTPUT);
	CASE(COB_OPEN_I_O);
	CASE(COB_OPEN_EXTEND);
	CASE(COB_OPEN_LOCKED);
	}
	return "COB_OPEN_MODE_UNKNOWN";
}

static
void dump_handler_struct (struct handler_struct *x)
{
	BEGIN_RECORD (handler_struct);
	FIELD (cb_label, handler_label);
	FIELD_STOP (cb_program, handler_prog);
	END_RECORD ();
}

static
void dump_cb_ml_generate_tree (struct cb_ml_generate_tree *x)
{
	BEGIN_TREE_RECORD (cb_ml_generate_tree);
	/* Name of the ML element to generate */
	FIELD (cb_tree, name);
	/* The type of the ML element to generate */
	FIELD (cb_ml_type, type);
	/* The content of the ML element to generate */
	FIELD (cb_tree, value);
	/* The condition under which generation of the element is suppressed */
	FIELD (cb_tree, suppress_cond);
	/* ID for this struct when dump */
	FIELD (int, id);
	/* Attributes for this element */
	FIELD (cb_ml_generate_tree, attrs);
	/* Parent ML element */
	FIELD (cb_ml_generate_tree, parent);
	/* Children ML elements */
	FIELD (cb_ml_generate_tree, children);
	/* Preceding ML elements */
	FIELD_STOP (cb_ml_generate_tree, prev_sibling);
	/* Following ML elements */
	FIELD_STOP (cb_ml_generate_tree	*, sibling);
	END_RECORD ();
}

static void dump_cb_alter_id (struct cb_alter_id *x)
{
	BEGIN_RECORD (cb_alter_id);
	FIELD (int, goto_id);
	FIELD (cb_alter_id, next);
	END_RECORD ();
}

static
void dump_cb_program (struct cb_program *x)
{
	int i;
	
	BEGIN_TREE_RECORD (cb_program);

	FIELD_STOP (struct cb_program*, next_program_ordered);	/* Nested/contained
							 when cb_correct_program_order is set */

	FIELD (string, program_name);		/* Internal program-name */
	FIELD (string, program_id);		/* Demangled external PROGRAM-ID */
	FIELD (string, source_name);		/* Source name */
	FIELD (string, orig_program_id);	/* Original external PROGRAM-ID */

	FIELD_TODO (struct cb_word **, word_table);		/* Name hash table */
	FIELD_TODO (struct local_filename*, local_include);		/* Local include info */
	FIELD_STOP (struct nested_list*, nested_prog_list);	/* Callable contained */
	FIELD_STOP (struct nested_list*, common_prog_list);	/* COMMON contained */

	FIELD (cb_tree, entry_list);		/* Entry point list */
	FIELD (cb_tree, entry_list_goto);	/* Special Entry point list */
	FIELD (cb_tree, file_list);		/* File list */
	FIELD (cb_tree, cd_list);		/* CD list */
	FIELD (cb_tree, exec_list);		/* Executable statements */
	FIELD (cb_tree, label_list);		/* Label list */
	FIELD (cb_tree, reference_list);		/* Reference list */
	FIELD (cb_tree, alphabet_name_list);	/* ALPHABET list */
	FIELD (cb_tree, symbolic_char_list);	/* SYMBOLIC list */
	FIELD (cb_tree, class_name_list);	/* CLASS list */
	FIELD (cb_tree, schema_name_list);	/* XML-SCHEMA list */
	FIELD (cb_tree, parameter_list);		/* USING parameters */
	FIELD (cb_tree, locale_list);		/* LOCALE list */
	FIELD (cb_tree, global_list);		/* GLOBAL list */
	FIELD (cb_tree, report_list);		/* REPORT list */
	FIELD (cb_tree, perform_thru_list);		/* list of PERFORM THRU */
	FIELD (cb_tree, alter_list);		/* ALTER list */
	FIELD (cb_tree, debug_list);		/* DEBUG ref list */
	FIELD (cb_tree, cb_return_code);		/* RETURN-CODE */
	FIELD (cb_tree, cb_sort_return);		/* SORT-RETURN */
	FIELD (cb_tree, cb_call_params);		/* Number of CALL params */
	FIELD (cb_tree, mnemonic_spec_list);	/* MNEMONIC spec */
	FIELD (cb_tree, class_spec_list);	/* CLASS spec */
	FIELD (cb_tree, interface_spec_list);	/* INTERFACE spec */
	FIELD (cb_tree, function_spec_list);	/* FUNCTION spec */
	FIELD (cb_tree, user_spec_list);		/* User FUNCTION spec */
	FIELD (cb_tree, program_spec_list);	/* PROGRAM spec */
	FIELD (cb_tree, property_spec_list);	/* PROPERTY spec */

	FIELD (cb_alter_id, alter_gotos);		/* ALTER ids */

	FIELD (cb_field, working_storage);	/* WORKING-STORAGE */
	FIELD (cb_field, local_storage);		/* LOCAL-STORAGE */
	FIELD (cb_field, linkage_storage);	/* LINKAGE */
	FIELD (cb_field, screen_storage);	/* SCREEN */
	FIELD (cb_field, report_storage);	/* REPORT */
	FIELD (cb_tree, local_file_list);	/* Local files */
	FIELD (cb_tree, global_file_list);	/* Global files */

	FIELD_SET(global_handler);
	BEGIN_COMMON_RECORD ();
	for (i=0; i<CB_MAX_GLOBAL_HANDLERS; i++){
		if (x->global_handler[i].handler_label){
			FIELD_NAME ( string_of_cob_open_mode(i) );
			dump_handler_struct (& x->global_handler[i]);
		}
	}
	END_RECORD ();

	FIELD (cb_tree, collating_sequence);	/* COLLATING */
	FIELD (cb_tree, collating_sequence_n);	/* COLLATING FOR NATIONAL*/
	FIELD (cb_tree, classification);		/* CLASSIFICATION */
	FIELD (cb_tree, apply_commit);		/* APPLY COMMIT file- and data-items */
	FIELD (cb_tree, cursor_pos);		/* CURSOR */
	FIELD (cb_tree, crt_status);		/* CRT STATUS */
	FIELD (cb_field, xml_code);		/* XML-CODE */
	FIELD (cb_field, xml_event);		/* XML-EVENT */
	FIELD (cb_field, xml_information);	/* XML-INFORMATION */
	FIELD (cb_field, xml_namespace);		/* XML-NAMESPACE */
	FIELD (cb_field, xml_nnamespace);		/* XML-NNAMESPACE */
	FIELD (cb_field, xml_namespace_prefix);	/* XML-NAMESPACE-PREFIX */
	FIELD (cb_field, xml_nnamespace_prefix);	/* XML-NNAMESPACE-PREFIX */
	FIELD (cb_field, xml_ntext);		/* XML-NTEXT */
	FIELD (cb_field, xml_text);		/* XML-TEXT */
	FIELD (cb_field, json_code);		/* JSON-CODE */
	FIELD (cb_field, json_status);		/* JSON-STATUS */
	FIELD (cb_tree, returning);		/* RETURNING */

	FIELD (cb_label, all_procedure);		/* DEBUGGING */
	FIELD_INLINE_TODO (struct cb_call_xref,	call_xref);		/* CALL Xref list */
	FIELD (cb_ml_generate_tree, ml_trees);	/* XML GENERATE trees */

	FIELD (string, extfh);		/* CALLFH for this program */

	FIELD (int,last_source_line);	/* Line of (implicit) END PROGRAM/FUNCTION */


	FIELD (int, loop_counter);			/* Loop counters */
	FIELD (uint, decimal_index);			/* cob_decimal count of this program */
	FIELD (uint, decimal_index_max);		/* program group's max cob_decimal */
	FIELD (int, nested_level);			/* Nested program level */
	FIELD (uint, num_proc_params);		/* PROC DIV params */
	FIELD (int, toplev_count);			/* Top level source count */
	FIELD (uint, max_call_param);			/* Max params */

	FIELD (uchar, decimal_point);			/* '.' or ',' */
	FIELD (uchar, currency_symbol);		/* '$' or user-specified */
	FIELD (uchar, numeric_separator);		/* ',' or '.' */

	FIELD (cob_module_type, prog_type);	/* Program type (program = 0, function = 1) */
	FIELD (cb_tree, entry_convention);	/* ENTRY convention / PROCEDURE convention */

	FIELD (uint, flag_main	);	/* Gen main function */
	FIELD (uint, flag_common	);	/* COMMON PROGRAM */
	FIELD (uint, flag_initial	);	/* INITIAL PROGRAM */
	FIELD (uint, flag_recursive	);	/* RECURSIVE PROGRAM */
	FIELD (uint, flag_screen	);	/* Have SCREEN SECTION */
	FIELD (uint, flag_validated	);	/* End program validate */
	FIELD (uint, flag_chained	);	/* PROCEDURE CHAINING */
	FIELD (uint, flag_global_use	);	/* USE GLOBAL */

	FIELD (uint, flag_gen_error	);	/* Gen error routine */
	FIELD (uint, flag_file_global);	/* Global FD */
	FIELD (uint, flag_has_external);	/* Has EXTERNAL */
	FIELD (uint, flag_segments	);	/* Has segments */
	FIELD (uint, flag_trailing_separate);	/* TRAILING SEPARATE */
	FIELD (uint, flag_console_is_crt);	/* CONSOLE IS CRT */
	FIELD (uint, flag_debugging	);	/* DEBUGGING MODE */
	FIELD (uint, flag_gen_debug	);	/* DEBUGGING MODE */

	FIELD (uint, flag_save_exception);	/* Save exception code */
	FIELD (uint, flag_report	);	/* Have REPORT SECTION */
	FIELD (uint, flag_void	);	/* void return for subprogram */
	FIELD (uint, flag_decimal_comp);	/* program group has decimal computations */
	FIELD (uint, flag_prototype	);	/* Is a prototype */

	FIELD (cb_program, next_program);
	END_RECORD ();
}

static
void dump_cb_list (struct cb_list* x)
{
	BEGIN_LIST ();
	for (; x; x = (struct cb_list*) x->chain) {
		dump_sequence_delim (fmt.format_delim_list);
		dump_indent ();
		if (x->purpose || x->sizes){
			BEGIN_RECORD(cb_list);
			FIELD (cb_tree, purpose);
			FIELD (int, sizes);
			FIELD (cb_tree, value);
			END_RECORD ();
		} else {
			dump_cb_tree (x->value);
		}
	}
	END_LIST ();
}

static
void dump_cb_direct (struct cb_direct *x)
{
	BEGIN_TREE_RECORD (cb_direct);
	FIELD (string, line);	/* Line redirect */
	FIELD (int, flag_is_direct);	/* Is directed */
	FIELD (int, flag_new_line);	/* Need new line */
	END_RECORD ();
}

static
void dump_cb_const (struct cb_const *x)
{
	BEGIN_TREE_RECORD (cb_const);
	FIELD (string, val);		/* Constant value */
	END_RECORD ();
}

static
void dump_cb_debug (struct cb_debug *x)
{
	BEGIN_TREE_RECORD (cb_debug);
	FIELD (cb_tree, target);		/* Target for debug */
	FIELD (string, value);		/* Value for debug */
	FIELD (cb_tree, fld);		/* Reference */
	FIELD (size_t, size);		/* Size if relevant */
	END_RECORD ();
}

static
void dump_cb_debug_call (struct cb_debug_call *x)
{
	BEGIN_TREE_RECORD (cb_debug_call);
	FIELD (cb_label, target);	/* Target label */
	END_RECORD ();
}

static
void dump_cb_integer (struct cb_integer *x)
{
	BEGIN_TREE_RECORD (cb_integer);
	FIELD (int, val);		/* Integer value */
#ifdef USE_INT_HEX
	/* Simon: using this increases the struct and we
   *should* pass the flags as constants in any case... */
	FIELD (uint, hexval);		/* Dump hex value */
#endif
	END_RECORD ();
}

static
void dump_cb_string (struct cb_string *x)
{
	BEGIN_TREE_RECORD (cb_string);
	FIELD (ustring, data);		/* Data */
	FIELD (size_t, size);		/* Data size */
	END_RECORD ();
}

static
void dump_cb_alphabet_name (struct cb_alphabet_name *x)
{
	BEGIN_TREE_RECORD (cb_alphabet_name);
	FIELD (string, name);		/* Original name */
	FIELD (string, cname);		/* Name used in C */
	FIELD (cb_tree, custom_list);	/* Custom ALPHABET / LOCALE reference */
	FIELD (uint, alphabet_target);	/* ALPHANUMERIC or NATIONAL */
	FIELD (uint, alphabet_type);	/* ALPHABET type */
	FIELD (int, low_val_char);	/* LOW-VALUE */
	FIELD (int, high_val_char);	/* HIGH-VALUE */
	FIELD_INLINE_TODO (int[256], values);	/* Collating values */
	FIELD_INLINE_TODO (int[256], alphachr);	/* Actual values */
	END_RECORD ();
}

static
void dump_cb_class_name (struct cb_class_name *x)
{
	BEGIN_TREE_RECORD (cb_class_name);
	FIELD (string, name);		/* Original name */
	FIELD (string, cname);		/* Name used in C */
	FIELD (cb_tree, list);		/* List of CLASS definitions */
	END_RECORD ();
}

static
void dump_cb_locale_name (struct cb_locale_name *x)
{
	BEGIN_TREE_RECORD (cb_locale_name);
	FIELD (string, name);		/* Original name */
	FIELD (string, cname);		/* Name used in C */
	FIELD (cb_tree, list);		/* List of locale definitions */
	END_RECORD ();
}

static
void dump_cb_system_name (struct cb_system_name *x)
{
	BEGIN_TREE_RECORD (cb_system_name);
	FIELD (cb_tree, value);		/* System value */
	FIELD (cb_system_name_category, category);	/* System category */
	FIELD (int, token);		/* Device attributes */
	END_RECORD ();
}

static
void dump_cb_schema_name (struct cb_schema_name *x)
{
	BEGIN_TREE_RECORD (cb_schema_name);
	FIELD (string, name);		/* Original name */
	FIELD (string, data);		/* file name */
	END_RECORD ();
}

static
void dump_cb_literal (struct cb_literal *x)
{
	BEGIN_TREE_RECORD (cb_literal);
	FIELD (ustring, data);	/* Literal data */
	FIELD (cob_u32_t, size);	/* Literal size */
	FIELD (int, scale);	/* Numeric scale */
	FIELD (cob_u32_t, llit);	/* 'L' literal */
	FIELD (int, sign);	/* unsigned: 0 negative: -1 positive: 1 */
	FIELD (int, all);	/* ALL */
	END_RECORD ();
}


static
void dump_cb_decimal (struct cb_decimal *x)
{
	BEGIN_TREE_RECORD (cb_decimal);
	FIELD (uint, id);		/* ID for this decimal */
	END_RECORD ();
}


static
void dump_cb_picture (struct cb_picture *x)
{
	BEGIN_TREE_RECORD (cb_picture);
	FIELD (string, orig);		/* Original picture string */
	FIELD_TODO (cob_pic_symbol*, str);		/* Picture string */
	FIELD (int, size);		/* Byte size */
	FIELD (int, lenstr);		/* Length of picture string */
	FIELD (cb_category, category);	/* Field category */
	FIELD (cob_u32_t, digits);		/* Number of digit places */
	FIELD (int, scale);		/* 1/10^scale */
#if 0 /* currently unused */
	FIELD (cob_u32_t, real_digits);	/* Real number of digits */
#endif
	FIELD (cob_u32_t, have_sign);	/* Have 'S' */
	FIELD (uint, flag_is_calculated);	/* is calculated */
	FIELD (uint, variable_length);	/* Starts with 'L' */
	END_RECORD ();
}

static
void dump_cb_vary (struct cb_vary *x)
{
	BEGIN_TREE_RECORD (cb_vary);
	FIELD (cb_tree, var);					/* Variable name being VARYed */
	FIELD (cb_tree, from);					/* Starting value */
	FIELD (cb_tree, by);						/* Increment value */
	END_RECORD ();
}


static
void dump_cb_table_values (struct cb_table_values *x)
{
	BEGIN_TREE_RECORD (cb_table_values);
	FIELD (cb_tree, values);					/* list of literals*/
	FIELD (cb_tree, from);					/* NULL or list of subscripts start */
	FIELD (cb_tree, to);  					/* NULL or list of subscripts stop */
	FIELD (cb_tree, repeat_times);			/* NULL or integer to repeat the values,
	   		   					 or cb_null for "repeat to end" */
	END_RECORD ();
}

static
void dump_cb_key_component (struct cb_key_component *x)
{
	BEGIN_RECORD (cb_key_component);
	FIELD (cb_tree, component);
	FIELD (cb_key_component, next);
	END_RECORD ();
}

static
void dump_cb_alt_key (struct cb_alt_key *x)
{
	BEGIN_RECORD (cb_alt_key);
	FIELD (cb_tree,	key);			/* Key item */
	FIELD (cb_tree,	password);			/* Password item */
	FIELD (cb_tree,	collating_sequence_key);	/* COLLATING */
	FIELD (int, duplicates);		/* DUPLICATES */
	FIELD (int, offset);			/* Offset from start */
	FIELD (int, tf_suppress);		/* !0 for SUPPRESS */
	FIELD (int, char_suppress);		/* character to test for suppress */
	FIELD (cb_key_component, component_list);	/* List of fields making up key */
	FIELD (cb_alt_key, next);
	END_RECORD ();
}

#define BEGIN_FLAGS() \
	fprintf (fd, "%s", fmt.format_begin_flags)
#define FLAG(flag) \
	if (x & flag){ fprintf (fd, "%s%s%s", fmt.format_begin_flag, #flag, fmt.format_end_flag); }
#define END_FLAGS() \
	fprintf (fd, "%s", fmt.format_end_flags)

static void dump_cob_flags_t (cob_flags_t x)
{
	BEGIN_FLAGS ();
	FLAG(COB_SCREEN_LINE_PLUS);
	FLAG(COB_SCREEN_LINE_MINUS);
	FLAG(COB_SCREEN_COLUMN_PLUS);
	FLAG(COB_SCREEN_COLUMN_MINUS);
	FLAG(COB_SCREEN_AUTO);
	FLAG(COB_SCREEN_BELL);
	FLAG(COB_SCREEN_BLANK_LINE);
	FLAG(COB_SCREEN_BLANK_SCREEN);
	FLAG(COB_SCREEN_BLINK);
	FLAG(COB_SCREEN_ERASE_EOL);
	FLAG(COB_SCREEN_ERASE_EOS);
	FLAG(COB_SCREEN_FULL);
	FLAG(COB_SCREEN_HIGHLIGHT);
	FLAG(COB_SCREEN_LOWLIGHT);
	FLAG(COB_SCREEN_REQUIRED);
	FLAG(COB_SCREEN_REVERSE);
	FLAG(COB_SCREEN_SECURE);
	FLAG(COB_SCREEN_UNDERLINE);
	FLAG(COB_SCREEN_OVERLINE);
	FLAG(COB_SCREEN_PROMPT);
	FLAG(COB_SCREEN_UPDATE);
	FLAG(COB_SCREEN_INPUT);
	FLAG(COB_SCREEN_SCROLL_DOWN);
	FLAG(COB_SCREEN_INITIAL);
	FLAG(COB_SCREEN_NO_ECHO);
	FLAG(COB_SCREEN_LEFTLINE);
	FLAG(COB_SCREEN_NO_DISP);
	FLAG(COB_SCREEN_EMULATE_NL);
	FLAG(COB_SCREEN_UPPER);
	FLAG(COB_SCREEN_LOWER);
	FLAG(COB_SCREEN_CONV);
	FLAG(COB_SCREEN_GRAPHICS);
	FLAG(COB_SCREEN_RIGHTLINE);
	FLAG(COB_SCREEN_TAB);
	FLAG(COB_SCREEN_NO_UPDATE);
	FLAG(COB_SCREEN_SCROLL_UP);
	FLAG(COB_SCREEN_GRID);
	END_FLAGS();
}


static
void dump_cb_field (struct cb_field *x)
{
	BEGIN_TREE_RECORD (cb_field);
	FIELD (string, name);		/* Original name */
	FIELD (string, ename);		/* Externalized name */
	FIELD (cb_tree, depending);	/* OCCURS ... DEPENDING ON */
	FIELD (cb_tree, values);		/* VALUES, in the simple case: direct value;
						   for level 78 _can_ be a list (expression),
						   for level 88 and RW be either a list or direct value,
						   for VALUES ARE (table-format) a list of table_values */
	FIELD (cb_tree, false_88);	/* 88 FALSE clause */
	FIELD (cb_tree, index_list);	/* INDEXED BY */
	FIELD (cb_tree, external_form_identifier);	/* target of IDENTIFIED BY
								 (CGI template) */

	FIELD (cb_field, parent);	/* Upper level field (if any) */

	FIELD (cb_field, children);	/* Top of lower level fields */
	FIELD (cb_field, validation);	/* First level 88 field (if any) */
	FIELD_STOP (cb_field, sister);	/* Fields at the same level */
	FIELD (cb_field, redefines);	/* REDEFINES or RENAMES */
	FIELD (cb_field, rename_thru);	/* RENAMES THRU */
	FIELD (cb_field, index_qual);	/* INDEXED BY qualifier */
	FIELD (cb_file, file);		/* FD section file name */
	FIELD (cb_cd, cd);		/* CD name */
	FIELD_TODO (cb_key, keys);		/* SEARCH key */
	FIELD (cb_picture, pic);		/* PICTURE */
	FIELD (cb_field, vsize);		/* Variable size cache */
	FIELD (cb_label, debug_section);	/* DEBUG section */
	FIELD (cb_report, report);	/* RD section report name */

	FIELD_INLINE_TODO (cb_xref, xref);		/* xref elements */

	FIELD (cb_tree, screen_line);	/* LINE */
	FIELD (cb_tree, screen_column);	/* COLUMN */
	FIELD (cb_tree, screen_from);	/* TO and USING */
	FIELD (cb_tree, screen_to);	/* FROM and USING */
	FIELD (cb_tree, screen_foreg);	/* FOREGROUND */
	FIELD (cb_tree, screen_backg);	/* BACKGROUND */
	FIELD (cb_tree, screen_prompt);	/* PROMPT */
	FIELD (cb_tree, screen_control);	/* CONTROL identifier (variable named attributes) */
	FIELD (cb_tree, screen_color);	/* COLOR identifier (variable bit-shifted attributes) */
	FIELD (cb_tree, report_source);	/* SOURCE field */
	FIELD (cb_tree, report_from);	/* SOURCE field subscripted; so MOVE to report_source */
	FIELD (cb_tree, report_sum_counter);/* SUM counter */
	FIELD (cb_tree, report_sum_list);/* SUM field(s) */
	FIELD (cb_tree, report_sum_upon);/* SUM ... UPON detailname */
	FIELD (cb_tree, report_reset);	/* RESET ON field */
	FIELD (cb_tree, report_control);	/* CONTROL identifier */
	FIELD (cb_tree, report_when);	/* PRESENT WHEN condition */
	FIELD (cb_tree, report_column_list);/* List of Column Numbers */
	/* note: the following rw-specific fields are only set for parsing, no codegen in 3.x yet */
	FIELD (cb_tree, report_vary_list);/* VARYING identifier with FROM arith + BY arith */
#if 0 /* items from 4.x */
	FIELD (string, report_source_txt);	/* SOURCE as text string */
	FIELD (string, report_field_name);	/* Name used for this REPORT field */
	FIELD (cb_field, report_field_from);	/* 'field' used as SOURCE */
	FIELD (int, report_field_offset);
	FIELD (int, report_field_size);
#endif
	FIELD (cb_tree, same_as);	/* SAME AS data-name (points to field) */
	FIELD (cb_tree, external_definition);	/* by SAME AS / LIKE data-name or
						   by type-name (points to field) */
	FIELD (cb_tree, like_modifier);	/* set for LIKE, may contain a length modifier */

	FIELD (int, id);		/* Field id */
	FIELD (int, size);		/* Field size */
	FIELD (int, level);		/* Level number */
	FIELD (int, memory_size);	/* Memory size */
	FIELD (int, offset);		/* Byte offset from 01 level */
	FIELD (int, occurs_min);	/* OCCURS <min> */
	FIELD (int, occurs_max);	/* OCCURS [... TO] <max> */
	FIELD (int, indexes);	/* Indices count (OCCURS) */

	FIELD (int, count);		/* Reference count */
	FIELD (int, mem_offset);	/* Memory offset */
	FIELD (int, nkeys);		/* Number of keys */
	FIELD (int, param_num);	/* CHAINING param number */
	FIELD (cob_flags_t, screen_flag);	/* Flags used in SCREEN SECTION */
	FIELD (int, report_flag);	/* Flags used in REPORT SECTION */
	FIELD (int, report_line);	/* LINE */
	FIELD (int, report_column);	/* COLUMN (first value) */
	FIELD (int, report_num_col);	/* Number of COLUMNs defined */
	FIELD (int, report_decl_id);	/* Label id of USE FOR REPORTING */
#if 0 /* items from 4.x */
	FIELD (int, report_source_id);	/* Label id of MOVE SOURCE values */
#endif
	FIELD (int, step_count);	/* STEP in REPORT */
	FIELD (int, next_group_line);	/* NEXT GROUP [PLUS] line */
	FIELD (uint, vaddr);		/* Variable address cache */
	FIELD (uint, odo_level);	/* ODO level (0 = no ODO item)
						 could be direct ODO (check via depending)
						 or via subordinate) */
	FIELD (cb_index_type, index_type);	/* Type of generated index */

	FIELD (cb_storage, storage);	/* Storage section */
	FIELD (cb_usage, usage);		/* USAGE */

	/* Flags */
	FIELD (uint, flag_base);		/* Has memory allocation */
	FIELD (uint, flag_external);		/* EXTERNAL */
	FIELD (uint, flag_local_storage);	/* LOCAL storage */
	FIELD (uint, flag_is_global);		/* Is GLOBAL */

	FIELD (uint, flag_local	);	/* Has local scope */
	FIELD (uint, flag_occurs);	/* OCCURS */
	FIELD (uint, flag_sign_clause);	/* Any SIGN clause */
	FIELD (uint, flag_sign_separate);	/* SIGN IS SEPARATE */
	FIELD (uint, flag_sign_leading);	/* SIGN IS LEADING */
	FIELD (uint, flag_blank_zero);	/* BLANK WHEN ZERO */
	FIELD (uint, flag_justified);	/* JUSTIFIED RIGHT */
	FIELD (uint, flag_binary_swap);	/* Binary byteswap */

	FIELD (uint, flag_real_binary);	/* BINARY-CHAR/SHORT/LONG/DOUBLE */
	FIELD (uint, flag_is_pointer);	/* Is POINTER */
	FIELD (uint, flag_item_78 );	/* Is a constant by 78 level,
					   01 CONSTANT or SYMBOLIC CONSTANT */
	FIELD (uint, flag_any_length);	/* Is ANY LENGTH */
	FIELD (uint, flag_item_based);	/* Is BASED */
	FIELD (uint, flag_is_external_form );		/* Is EXTERNAL-FORM */
	FIELD (uint, flag_filler);	/* Implicit/explicit filler */
	FIELD (uint, flag_synchronized);	/* SYNCHRONIZED */

	FIELD (uint, flag_invalid);	/* Is broken */
	FIELD (uint, flag_field	);	/* Has been internally cached */
	FIELD (uint, flag_chained);	/* CHAINING item */
	FIELD (uint, flag_anylen_done);	/* ANY LENGTH is set up */
	FIELD (uint, flag_is_verified);	/* Has been verified */
	FIELD (uint, flag_is_c_long);	/* Is BINARY-C-LONG */
	FIELD (uint, flag_is_pdiv_parm);	/* Is PROC DIV USING */
	FIELD (uint, flag_is_pdiv_opt);	/* Is PROC DIV USING OPTIONAL */

	FIELD (uint, flag_indexed_by);	/* INDEXED BY item */
	FIELD (uint, flag_local_alloced);	/* LOCAL storage is allocated */
	FIELD (uint, flag_no_init);	/* No initialize unless used */
	FIELD (uint, flag_vsize_done);	/* Variable size cached */
	FIELD (uint, flag_vaddr_done);	/* Variable address cached */
	FIELD (uint, flag_odo_relative);	/* complex-odo: item address depends
							on size of a different (ODO) item */
	FIELD (uint, flag_field_debug);	/* DEBUGGING */
	FIELD (uint, flag_all_debug);	/* DEBUGGING */

	FIELD (uint, flag_no_field);	/* SCREEN/REPORT dummy field */
	FIELD (uint, flag_any_numeric);	/* Is ANY NUMERIC */
	FIELD (uint, flag_is_returning);	/* Is RETURNING item */
	FIELD (uint, flag_unbounded);	/* OCCURS UNBOUNDED */
	FIELD (uint, flag_comp_1);	/* Is USAGE COMP-1 */
	FIELD (uint, flag_volatile);	/* VOLATILE */
	FIELD (uint, flag_constant);	/* Is 01 AS CONSTANT */
	FIELD (uint, flag_internal_constant);	/* Is an internally generated CONSTANT */

	FIELD (uint, flag_used_in_call );	/* Is used in CALL (only set for level 01/77),
						   currently not set for EXTERNAL item or when in LOCAL-STORAGE / LINKAGE */
	FIELD (uint, flag_sync_left );	/* SYNCHRONIZED LEFT */
	FIELD (uint, flag_sync_right );	/* SYNCHRONIZED RIGHT */
	FIELD (uint, flag_internal_register);	/* Is an internally generated register */
	FIELD (uint, flag_is_typedef );	/* TYPEDEF  */
	FIELD (uint, flag_picture_l );	/* Is USAGE PICTURE L */
	END_RECORD ();
}


static
void dump_cb_para_label (struct cb_para_label *x)
{
	BEGIN_RECORD (cb_para_label);
	FIELD (cb_label, para);
	FIELD (cb_para_label, next);
	END_RECORD ();
}

static
void dump_cb_label (struct cb_label *x)
{
	BEGIN_TREE_RECORD (cb_label);
	FIELD (string, name);			/* Name */
	FIELD (string, orig_name);		/* Original name */
	FIELD (cb_label, section);		/* Parent SECTION */
	FIELD (cb_label, debug_section);		/* DEBUG SECTION */
	FIELD (cb_para_label, para_label);		/* SECTION Paragraphs */
	FIELD_INLINE_TODO (struct cb_xref, xref);		/* xref elements */
	FIELD (cb_tree, exit_label);		/* EXIT label */
	FIELD (cb_alter_id, alter_gotos);		/* ALTER ids */
	FIELD (int, id);			/* Unique id */
	FIELD (int, section_id);		/* SECTION id */
	FIELD (int, segment);		/* Segment number */

	FIELD (uint, flag_section	);	/* Section */
	FIELD (uint, flag_entry	);	/* Entry */
	FIELD (uint, flag_begin	);	/* Begin label */
	FIELD (uint, flag_return	);	/* End label */
	FIELD (uint, flag_real_label	);	/* Is real label */
	FIELD (uint, flag_global	);	/* GLOBAL */
	FIELD (uint, flag_declarative_exit);	/* Final EXIT */
	FIELD (uint, flag_declaratives);	/* DECLARATIVES */

	FIELD (uint, flag_fatal_check);	/* Fatal check */
	FIELD (uint, flag_dummy_section);	/* Dummy MAIN */
	FIELD (uint, flag_dummy_paragraph);	/* Dummy MAIN */
	FIELD (uint, flag_dummy_exit	);	/* Dummy EXIT */
	FIELD (uint, flag_next_sentence);	/* NEXT SENTENCE */
	FIELD (uint, flag_default_handler);	/* Error handler */
	FIELD (uint, flag_statement	);	/* Has statement */
	FIELD (uint, flag_first_is_goto);	/* 1st is GO TO */

	FIELD (uint, flag_alter	);	/* ALTER code */
	FIELD (uint, flag_debugging_mode);	/* DEBUGGING MODE */
	FIELD (uint, flag_is_debug_sect);	/* DEBUGGING sect */
	FIELD (uint, flag_skip_label	);	/* Skip label gen */
	FIELD (uint, flag_entry_for_goto);	/* is ENTRY FOR GO TO */
	END_RECORD ();
}

static
void dump_cb_file (struct cb_file *x)
{
	int i;

	BEGIN_TREE_RECORD (cb_file);
	FIELD (string, name);			/* Original name */
	FIELD (string, cname);			/* Name used in C */
	/* SELECT */
	FIELD (cb_tree, assign);			/* ASSIGN */
	FIELD (cb_tree, file_status);		/* FILE STATUS */
	FIELD (cb_tree, sharing);		/* SHARING */
	FIELD (cb_tree, key);			/* Primary RECORD KEY */
	FIELD (cb_tree, password);			/* Password item for file or primary key */
	FIELD (cb_key_component, component_list);	/* List of fields making up primary key */
	FIELD (cb_alt_key, alt_key_list);		/* ALTERNATE RECORD KEY */
	FIELD (cb_tree, collating_sequence_key);	/* COLLATING */
	FIELD (cb_tree, collating_sequence);	/* COLLATING */
	FIELD (cb_tree, collating_sequence_n);	/* COLLATING FOR NATIONAL*/
	FIELD (cb_tree, collating_sequence_keys);	/* list of postponed COLLATING OF */
	/* FD/SD */
	FIELD (cb_tree, description_entry);	/* FD / SD entry rerference for warnings + errors */
	FIELD (cb_field, record);		/* Record descriptions */
	FIELD (cb_tree, record_depending);	/* RECORD DEPENDING */
	FIELD (cb_tree, reports);		/* REPORTS */
	FIELD (cb_tree, linage);			/* LINAGE */
	FIELD (cb_tree, linage_ctr);		/* LINAGE COUNTER */
	FIELD (cb_tree, latfoot);		/* LINAGE FOOTING */
	FIELD (cb_tree, lattop);			/* LINAGE TOP */
	FIELD (cb_tree, latbot);			/* LINAGE BOTTOM */
	FIELD (cb_tree, extfh);			/* EXTFH module to call for I/O */
	FIELD (cb_label, handler);		/* Error handler */
	FIELD_STOP (cb_program, handler_prog);		/* Prog where defined */
	FIELD (cb_label, debug_section);		/* DEBUG SECTION */
	FIELD (cb_alphabet_name, code_set);		/* CODE-SET */
	FIELD (cb_list, code_set_items);	/* CODE-SET FOR items */
	FIELD_INLINE_TODO (struct cb_xref, xref);			/* xref elements */
	FIELD (int, record_min);		/* RECORD CONTAINS */
	FIELD (int, record_max);		/* RECORD CONTAINS */
	FIELD (int, optional);		/* OPTIONAL */
	FIELD (cob_file_org, organization);		/* ORGANIZATION */
	FIELD ( cob_file_access_mode, access_mode);		/* ACCESS MODE */
	FIELD (int, lock_mode);		/* LOCK MODE */
	FIELD (int, special);		/* Special file */
	FIELD (int, same_clause);		/* SAME clause */
	FIELD (cb_assign_type, assign_type);	/* How to interpret ASSIGN clause */
	FIELD (uint, flag_finalized);	/* Is finalized */
	FIELD (uint, flag_external);	/* Is EXTERNAL */
	FIELD (uint, flag_ext_assign);	/* ASSIGN EXTERNAL */
	FIELD (uint, flag_fileid);	/* ASSIGN DISK */
	FIELD (uint, flag_global);	/* Is GLOBAL */
	FIELD (uint, flag_fl_debug);	/* DEBUGGING */
	FIELD (uint, flag_line_adv);	/* LINE ADVANCING */
	FIELD (uint, flag_delimiter);	/* RECORD DELIMITER */
	FIELD (uint, flag_report);	/* Used by REPORT */
	/* Implied RECORD VARYING limits need checking */
	FIELD (uint, flag_check_record_varying_limits);
	/* Whether the file's ASSIGN is like "ASSIGN word", not "ASSIGN
     EXTERNAL/DYNAMIC/USING/... word" */
	FIELD (uint, flag_assign_no_keyword );
	/* Exceptions enabled for file */
	FIELD_SET(exception_table);
	BEGIN_COMMON_RECORD ();
	for (i=1; x->exception_table[i].name != NULL; i++){
		if (x->exception_table[i].enable){
			FIELD_NAME (x->exception_table[i].name);
			dump_constr_string ("enable",  "ENABLE");
		}
	}
	END_RECORD ();
	END_RECORD ();
}


static
void dump_cb_cd (struct cb_cd *x)
{
	BEGIN_TREE_RECORD (cb_cd);
	FIELD (string, name);			/* Name */
	FIELD (cb_field, record);		/* Record descriptions */
	FIELD (cb_label, debug_section);	/* DEBUG section */
	FIELD (int, flag_field_debug);	    /* DEBUGGING */
	END_RECORD ();
}



static
void dump_cb_reference (struct cb_reference *x)
{
	BEGIN_TREE_RECORD (cb_reference);
	FIELD (cb_tree, chain);		/* Next qualified name */
	FIELD (cb_tree, value);		/* Item referred to */
	FIELD (cb_tree, subs);		/* List of subscripts */
	FIELD (cb_tree, offset);		/* Reference mod offset */
	FIELD (cb_tree, length);		/* Reference mod length */
	FIELD (cb_tree, check);		/* Runtime checks */
	FIELD (cob_statement,	statement);	/* statement that uses this reference */
	FIELD_TODO (struct cb_word*, word);		/* Pointer to word list */
	FIELD (cb_label, section);	/* Current section */
	FIELD (cb_label, paragraph);	/* Current paragraph */
	FIELD (cb_label, debug_section);	/* Debug section */
	FIELD (size_t, hashval);	/* Hash value of name */
	FIELD (uint, flag_receiving);	/* Reference target */
	FIELD (uint, flag_all);	/* ALL */
	FIELD (uint, flag_in_decl);	/* In DECLARATIVE */
	FIELD (uint, flag_alter_code);	/* Needs ALTER code */
	FIELD (uint, flag_debug_code);	/* Needs DEBUG code */
	FIELD (uint, flag_all_debug);	/* Needs ALL DEBUG code */
	FIELD (uint, flag_target);	/* DEBUG item is target */
	FIELD (uint, flag_optional);	/* Definition optional */
	FIELD (uint, flag_ignored);	/* Part of ignored code */
	FIELD (uint, flag_filler_ref);	/* Ref to FILLER */
	FIELD (uint, flag_duped);	/* Duplicate name */
	END_RECORD ();
}

static
const char* string_of_cb_binary_op_flag (enum cb_binary_op_flag x)
{
	switch(x){
	case BOP_RESOLVE_AS_INTEGER: return "BOP_RESOLVE_AS_INTEGER";
	case BOP_OPERANDS_SWAPPED: return "BOP_OPERANDS_SWAPPED";
	}
	return "CB_BINARY_OP_FLAG_UNKNOWN";
}

static
void dump_cb_binary_op_flag (enum cb_binary_op_flag x)
{
	dump_constr_string ("cb_binary_op_flag", string_of_cb_binary_op_flag(x));
}

static
const char* string_of_cb_binary_op_op (enum cb_binary_op_op x)
{
	switch(x){
	case BOP_INVALID: return "BOP_INVALID";
	case BOP_PLUS: return "BOP_PLUS";
	case BOP_MINUS: return "BOP_MINUS";
	case BOP_MULT: return "BOP_MULT";
	case BOP_DIV: return "BOP_DIV";
	case BOP_POW: return "BOP_POW";
	case BOP_EQ: return "BOP_EQ";
	case BOP_GT: return "BOP_GT";
	case BOP_LT: return "BOP_LT";
	case BOP_LE: return "BOP_LE";
	case BOP_GE: return "BOP_GE";
	case BOP_NE: return "BOP_NE";
	case BOP_NOT: return "BOP_NOT";
	case BOP_AND: return "BOP_AND";
	case BOP_OR: return "BOP_OR";
	case BOP_PARENS: return "BOP_PARENS";
	case BOP_BITWISE_NOT: return "BOP_BITWISE_NOT";
	case BOP_BITWISE_AND: return "BOP_BITWISE_AND";
	case BOP_BITWISE_OR: return "BOP_BITWISE_OR";
	case BOP_BITWISE_XOR: return "BOP_BITWISE_XOR";
	case BOP_SHIFT_L: return "BOP_SHIFT_L";
	case BOP_SHIFT_R: return "BOP_SHIFT_R";
	case BOP_SHIFT_LC: return "BOP_SHIFT_LC";
	case BOP_SHIFT_RC: return "BOP_SHIFT_RC";
	}
	return "CB_BINARY_OP_OP_UNKNOWN";
}

static
void dump_cb_binary_op_op (enum cb_binary_op_op x)
{
	dump_constr_string ("cb_binary_op_op", string_of_cb_binary_op_op(x));
}

static
const char* string_of_cb_handler_type (enum cb_handler_type x)
{
	switch(x){
	CASE(NO_HANDLER);
	CASE(DISPLAY_HANDLER);
	CASE(ACCEPT_HANDLER);
	CASE(SIZE_ERROR_HANDLER);
	CASE(OVERFLOW_HANDLER);
	CASE(AT_END_HANDLER);
	CASE(EOP_HANDLER);
	CASE(INVALID_KEY_HANDLER);
	CASE(XML_HANDLER);
	CASE(JSON_HANDLER);
	CASE(MCS_HANDLER);
	}
	return "CB_HANDLER_TYPE_UNKNOWN";
}

static
void dump_cb_handler_type (enum cb_handler_type x)
{
	dump_constr_string ("cb_handler_type", string_of_cb_handler_type(x));
}

static
void dump_cb_binary_op (struct cb_binary_op *x)
{
	BEGIN_TREE_RECORD (cb_binary_op);
	FIELD (cb_tree, x);		/* LHS */
	FIELD (cb_tree, y);		/* RHS */
	FIELD (cb_binary_op_op, op);		/* Operation */
	FIELD (cb_binary_op_flag, flag);		/* Special usage */
	END_RECORD ();
}



static
void dump_cb_funcall (struct cb_funcall *x)
{
	BEGIN_TREE_RECORD (cb_funcall);
	FIELD (string, name);		/* Function name */
	FIELD (cb_trees, argv);	/* Function arguments */
	FIELD (int, argc);		/* Number of arguments */
	FIELD (int, varcnt);		/* Variable argument count */
	FIELD (uint, screenptr);	/* SCREEN usage */
	FIELD (uint, nolitcast);	/* No cast for literals */
	END_RECORD ();
}


static
void dump_cb_cast (struct cb_cast *x)
{
	BEGIN_TREE_RECORD (cb_cast);
	FIELD (cb_tree, val);
	FIELD (cb_cast_type, cast_type);
	END_RECORD ();
}

static
void dump_cb_assign (struct cb_assign *x)
{
	BEGIN_TREE_RECORD (cb_assign);
	FIELD (cb_tree, var);
	FIELD (cb_tree, val);
	END_RECORD ();
}

static
void dump_cb_intrinsic (struct cb_intrinsic *x)
{
	BEGIN_TREE_RECORD (cb_intrinsic);
	FIELD (cb_tree, name);		/* INTRINSIC name */
	FIELD (cb_tree, args);		/* Arguments */
	FIELD (cb_tree, intr_field);	/* Field to use */
	FIELD_TODO (struct cb_intrinsic_table*, intr_tab);	/* Table pointer */
	FIELD (cb_tree, offset);		/* Reference mod */
	FIELD (cb_tree, length);		/* Reference mod */
	FIELD (int, isuser);		/* User function */
	END_RECORD ();
}


static
void dump_cb_initialize (struct cb_initialize *x)
{
	BEGIN_TREE_RECORD (cb_initialize);
	FIELD (cb_tree, var);			/* Field */
	FIELD (cb_tree, val);			/* ALL (cb_true) or category (cb_int) TO VALUE */
	FIELD (cb_tree, rep);			/* Replacing */
	FIELD (cob_statement,	statement);	/* INITIALIZE statement */
	FIELD (uint, flag_default);		/* Default */
	FIELD (uint, flag_no_filler_init);	/* No FILLER initialize */
	FIELD (uint, padding);		/* Padding */
	END_RECORD ();
}


static
void dump_cb_search (struct cb_search *x)
{
	BEGIN_TREE_RECORD (cb_search);
	FIELD (cb_tree, table);		/* Reference to table name */
	FIELD (cb_tree, var);		/* VARYING field */
	FIELD (cb_tree, at_end);		/* AT END (pair of position and statements) */
	FIELD (cb_tree, whens);		/* WHEN (conditions and statements)
	   			  		 [for not SEARCH ALL: list of those] */
	FIELD (int, flag_all);	/* SEARCH ALL */
	END_RECORD ();
}


static
void dump_cb_xml_parse (struct cb_xml_parse *x)
{
	BEGIN_TREE_RECORD (cb_xml_parse);
	FIELD (cb_tree, data);		/* XML data (field identifier) */
	FIELD (cb_tree, proc);		/* PROCESSING PROCEDURE (internally as PERFORM ...) */
	FIELD (cb_tree, encoding);		/* ENCODING codepage (optional) */
	FIELD (cb_tree, validating);		/* VALIDATING source (optional) */
	FIELD (int, returning_national);	/* RETURNING NATIONAL */
	END_RECORD ();
}

static
void dump_cb_call (struct cb_call *x)
{
	BEGIN_TREE_RECORD (cb_call);
	FIELD (cb_tree, name);		/* CALL name */
	FIELD (cb_tree, args);		/* Arguments */
	FIELD (cb_tree, stmt1);		/* ON EXCEPTION */
	FIELD (cb_tree, stmt2);		/* NOT ON EXCEPTION */
	FIELD (cb_tree, call_returning);	/* RETURNING */
	FIELD (cob_u32_t, is_system);	/* System call */
	FIELD (int, convention);	/* CALL convention */
	END_RECORD ();
}

static
void dump_cb_cancel (struct cb_cancel *x)
{
	BEGIN_TREE_RECORD (cb_cancel);
	FIELD (cb_tree, target);		/* CANCEL target(s) */
	END_RECORD ();
}

static
void dump_cb_alter (struct cb_alter *x)
{
	BEGIN_TREE_RECORD (cb_alter);
	FIELD (cb_tree, source);		/* ALTER source paragraph */
	FIELD (cb_tree, target);		/* ALTER target GO TO paragraph */
	END_RECORD ();
}

static
void dump_cb_goto (struct cb_goto *x)
{
	BEGIN_TREE_RECORD (cb_goto);
	FIELD (cb_tree, target);		/* Procedure name(s) */
	FIELD (cb_tree, depending);	/* DEPENDING */
	END_RECORD ();
}

static
void dump_cb_if (struct cb_if *x)
{
	BEGIN_TREE_RECORD (cb_if);
	FIELD (cb_tree, test);		/* Condition */
	FIELD (cb_tree, stmt1);		/* Statement list */
	FIELD (cb_tree, stmt2);		/* ELSE/WHEN statement list */
	FIELD (cob_statement, statement);	/* statement IF/WHEN/PRESENT WHEN */
	END_RECORD ();
}

static
void dump_cb_perform_varying (struct cb_perform_varying *x)
{
	BEGIN_TREE_RECORD (cb_perform_varying);
	FIELD (cb_tree, name);		/* VARYING item */
	FIELD (cb_tree, from);		/* FROM */
	FIELD (cb_tree, step);		/* Increment */
	FIELD (cb_tree, until);		/* UNTIL */
	END_RECORD ();
}

static
void dump_cb_perform (struct cb_perform *x)
{
	BEGIN_TREE_RECORD (cb_perform);
	FIELD (cb_tree, test);		/* Condition */
	FIELD (cb_tree, body);		/* Statements */
	FIELD (cb_tree, data);		/* TIMES or procedure */
	FIELD (cb_tree, varying);	/* VARYING */
	FIELD (cb_tree, exit_label);	/* Implicit exit label */
	FIELD (cb_tree, cycle_label);	/* EXIT PERFORM CYCLE */
	FIELD (cb_perform_type, perform_type);	/* Perform type */
	END_RECORD ();
}


static
void dump_cb_attr_struct (struct cb_attr_struct *x)
{
	BEGIN_RECORD (cb_attr_struct);
	FIELD (cb_tree, fgc);		/* FOREGROUND COLOR */
	FIELD (cb_tree, bgc);		/* BACKGROUND COLOR */
	FIELD (cb_tree, scroll);		/* SCROLL */
	FIELD (cb_tree, timeout);	/* TIMEOUT */
	FIELD (cb_tree, prompt);		/* PROMPT */
	FIELD (cb_tree, size_is);	/* [PROTECTED] SIZE [IS] */
	FIELD (cb_tree, control);		/* CONTROL [IS] (named attributes) */
	FIELD (cb_tree, color);		/* CONTROL (bit-shifted attributes) */
	FIELD (cb_tree, cursor);		/* CURSOR */
	FIELD (cob_flags_t, dispattrs);	/* Attributes */
	END_RECORD ();
}


static
void dump_cb_statement (struct cb_statement *x)
{
	BEGIN_TREE_RECORD (cb_statement);

	FIELD (cob_statement,	statement);		/* Statement */
	FIELD (cb_tree, body);			/* Statement body */
	FIELD (cb_tree, file);			/* File reference */
	FIELD (cb_tree, ex_handler);		/* Exception handler */
	FIELD (cb_tree, not_ex_handler);		/* Exception handler */
	FIELD (cb_tree, handler3);		/* INTO clause */
	FIELD (cb_tree, null_check);		/* NULL check */
	FIELD (cb_tree, debug_check);		/* Field DEBUG */
	FIELD (cb_tree, debug_nodups);		/* Field DEBUG dups */
	FIELD (cb_attr_struct, attr_ptr);		/* Attributes */
	FIELD (cb_handler_type, handler_type);		/* Handler type */
	FIELD (uint, flag_no_based);	/* Check BASED */
	FIELD (uint, flag_in_debug);	/* In DEBUGGING */
	FIELD (uint, flag_callback);	/* DEBUG Callback */
	FIELD (uint, flag_implicit);	/* Is an implicit statement */
	END_RECORD ();
}


static
void dump_cb_continue (struct cb_continue *x)
{
	BEGIN_TREE_RECORD (cb_continue);
	END_RECORD ();
}


static
void dump_cb_set_attr (struct cb_set_attr *x)
{
	BEGIN_TREE_RECORD (cb_set_attr);
	FIELD (cb_field, fld);
	FIELD (cob_flags_t, val_on);
	FIELD (cob_flags_t, val_off);
	END_RECORD ();
}

static
void dump_cb_report (struct cb_report *x)
{
	BEGIN_TREE_RECORD (cb_report);
	FIELD (string, name);		/* Original name */
	FIELD (string, cname);		/* Name used in C */
	FIELD (cb_file, file);		/* File */
	FIELD (cb_tree, line_counter);	/* LINE-COUNTER */
	FIELD (cb_tree, page_counter);	/* PAGE-COUNTER */
	FIELD (cb_tree, code_clause);	/* CODE */
	FIELD (cb_tree, controls);	/* CONTROLS */
	FIELD (cb_tree, t_lines);	/* PAGE LIMIT LINES */
	FIELD (cb_tree, t_columns);	/* PAGE LIMIT COLUMNS */
	FIELD (cb_tree, t_heading);	/* HEADING */
	FIELD (cb_tree, t_first_detail);	/* FIRST DE */
	FIELD (cb_tree, t_last_control);	/* LAST CH */
	FIELD (cb_tree, t_last_detail);	/* LAST DE */
	FIELD (cb_tree, t_footing);	/* FOOTING */
	FIELD (int, lines);		/* PAGE LIMIT LINES */
	FIELD (int, columns);	/* PAGE LIMIT COLUMNS */
	FIELD (int, heading);	/* HEADING */
	FIELD (int, first_detail);	/* FIRST DE */
	FIELD (int, last_control);	/* LAST CH */
	FIELD (int, last_detail);	/* LAST DE */
	FIELD (int, footing);	/* FOOTING */
	FIELD (cb_field, records);	/* First record definition of report */
	FIELD (int, num_lines);	/* Number of Lines defined */
	if (x->num_lines>0){
		int i;
		FIELD_SET(num_lines);
		BEGIN_LIST();
		for (i=0; i<x->num_lines; i++){
			LIST_ELEM (cb_field, x->line_ids[i]);
		}
		END_LIST();
	}
	FIELD (int, num_sums);	/* Number of SUM counters defined */
	if (x->num_sums>0){
		int i;
		FIELD_SET(sums);
		BEGIN_LIST();
		for (i=0; i<x->num_sums; i++){
			LIST_ELEM (cb_field, x->sums[i]);
		}
		END_LIST();
	}
	FIELD (int, rcsz);		/* Longest record */
	FIELD (int, id);		/* unique id for this report */
	FIELD (uint, control_final);/* CONTROL FINAL declared */
	FIELD (uint, global);	/* IS GLOBAL declared */
	FIELD (uint, has_declarative);/* Has Declaratives Code to be executed */
	FIELD (uint, has_detail);	/* Has DETAIL line */
	FIELD (uint, has_source_move);/* Has Code to MOVE SOURCE values */
	FIELD (uint, was_checked);
	END_RECORD ();
}

static
void dump_cb_prototype (struct cb_prototype *x)
{
	BEGIN_TREE_RECORD (cb_prototype);
	/* Name of prototype in the REPOSITORY */
	FIELD (string, name);
	/* External name of the prototype/definition */
	FIELD (string, ext_name);
	FIELD (cob_module_type, type);
	END_RECORD ();
}

static
const char* string_of_cb_ml_suppress_target (enum cb_ml_suppress_target x)
{
	switch (x){
	CASE (CB_ML_SUPPRESS_IDENTIFIER);
	CASE (CB_ML_SUPPRESS_ALL);
	CASE (CB_ML_SUPPRESS_TYPE);
	}
	return "CB_ML_SUPPRESS_TARGET_UNKNOWN";
}

static
void dump_cb_ml_suppress_target (enum cb_ml_suppress_target x)
{
	dump_constr_string ( "cb_ml_suppress_target", string_of_cb_ml_suppress_target (x));
}


static
const char* string_of_cb_ml_suppress_category (enum cb_ml_suppress_category x)
{
	switch (x){
	CASE (CB_ML_SUPPRESS_CAT_NUMERIC);
	CASE (CB_ML_SUPPRESS_CAT_NONNUMERIC);
	CASE (CB_ML_SUPPRESS_CAT_ANY);
	}
	return "CB_ML_SUPPRESS_CATEGORY_UNKNOWN";
}

static
void dump_cb_ml_suppress_category (enum cb_ml_suppress_category x)
{
	dump_constr_string ( "cb_ml_suppress_category", string_of_cb_ml_suppress_category (x));
}


static
void dump_cb_ml_suppress_clause (struct cb_ml_suppress_clause *x)
{
	BEGIN_TREE_RECORD (cb_ml_suppress_clause);
	/* What thing(s) the SUPPRESS clause applies to */
	FIELD (cb_ml_suppress_target, target);
	/* If the target is IDENTIFIER, then the item targetted */
	FIELD (cb_tree, identifier);
	/* What values the thing(s) should have to be SUPPRESSed */
	FIELD (cb_tree, when_list);
	/* If the target is TYPE, then the type of ML elements to apply to */
	FIELD (cb_ml_type, ml_type);
	/* If the target is TYPE, then the categories of items (of ML type
	 ml_type) to apply to */
	FIELD (cb_ml_suppress_category, category);
	END_RECORD ();
}

static
void dump_cb_ml_suppress_checks (struct cb_ml_suppress_checks *x)
{
	BEGIN_TREE_RECORD (cb_ml_suppress_checks);
	FIELD (cb_ml_generate_tree, tree);
	END_RECORD ();
}

static
void dump_cb_tree (cb_tree x)
{
	if (!x) { fprintf (fd, "NULL"); return; }
	switch (x -> tag){
	case CB_TAG_CONST: dump_cb_const ( CB_CONST(x) ); break;	/* Constant value */
	case CB_TAG_INTEGER: dump_cb_integer ( CB_INTEGER(x) ); break;		/* Integer constant */
	case CB_TAG_STRING: dump_cb_string ( CB_STRING(x) ); break;		/* String constant */
	case CB_TAG_ALPHABET_NAME: dump_cb_alphabet_name ( CB_ALPHABET_NAME(x) ); break;	/* Alphabet-name */
	case CB_TAG_CLASS_NAME: dump_cb_class_name ( CB_CLASS_NAME(x) ); break;	/* Class-name */
	case CB_TAG_LOCALE_NAME: dump_cb_locale_name ( CB_LOCALE_NAME(x) ); break;	/* Locale-name */
	case CB_TAG_SYSTEM_NAME: dump_cb_system_name ( CB_SYSTEM_NAME(x) ); break;	/* System-name */
	case CB_TAG_SCHEMA_NAME: dump_cb_schema_name ( CB_SCHEMA_NAME(x) ); break;	/* xml-schema-name */
	case CB_TAG_LITERAL: dump_cb_literal ( CB_LITERAL(x) ); break;		/* Numeric/alphanumeric literal */
	case CB_TAG_DECIMAL: dump_cb_decimal ( CB_DECIMAL(x) ); break;		/* Decimal number */
	case CB_TAG_FIELD: dump_cb_field ( CB_FIELD(x) ); break;		/* User-defined variable */
	case CB_TAG_FILE: dump_cb_file ( CB_FILE(x) ); break;		/* File description */
	case CB_TAG_REPORT: dump_cb_report ( CB_REPORT(x) ); break;		/* Report description */
	case CB_TAG_CD: dump_cb_cd ( CB_CD(x) ); break;		/* Communication description */
	/* Expressions */
	case CB_TAG_REFERENCE: dump_cb_reference ( CB_REFERENCE(x) ); break;
	case CB_TAG_BINARY_OP: dump_cb_binary_op ( CB_BINARY_OP(x) ); break;	/* Binary operation */
	case CB_TAG_FUNCALL: dump_cb_funcall ( CB_FUNCALL(x) ); break;		/* Run-time function call */
	case CB_TAG_CAST: dump_cb_cast ( CB_CAST(x) ); break;		/* Type cast */
	case CB_TAG_INTRINSIC: dump_cb_intrinsic ( CB_INTRINSIC(x) ); break;	/* Intrinsic function */
	/* Statements */
	case CB_TAG_LABEL: dump_cb_label ( CB_LABEL(x) ); break;		/* Label statement */
	case CB_TAG_ASSIGN: dump_cb_assign ( CB_ASSIGN(x) ); break;		/* Assignment statement */
	case CB_TAG_INITIALIZE: dump_cb_initialize ( CB_INITIALIZE(x) ); break;	/* INITIALIZE statement */
	case CB_TAG_SEARCH: dump_cb_search ( CB_SEARCH(x) ); break;		/* SEARCH statement */
	case CB_TAG_CALL: dump_cb_call ( CB_CALL(x) ); break;		/* CALL statement */
	case CB_TAG_GOTO: dump_cb_goto ( CB_GOTO(x) ); break;		/* GO TO statement */
	case CB_TAG_IF: dump_cb_if ( CB_IF(x) ); break;		/* IF statement / WHEN clause / PRESENT WHEN clause */
	case CB_TAG_PERFORM: dump_cb_perform ( CB_PERFORM(x) ); break;		/* PERFORM statement */
	case CB_TAG_STATEMENT: dump_cb_statement ( CB_STATEMENT(x) ); break;	/* General statement */
	case CB_TAG_CONTINUE: dump_cb_continue ( CB_CONTINUE(x) ); break;	/* CONTINUE statement */
	case CB_TAG_CANCEL: dump_cb_cancel ( CB_CANCEL(x) ); break;		/* CANCEL statement */
	case CB_TAG_ALTER: dump_cb_alter ( CB_ALTER(x) ); break;		/* ALTER statement */
	case CB_TAG_SET_ATTR: dump_cb_set_attr ( CB_SET_ATTR(x) ); break;	/* SET ATTRIBUTE statement */
	case CB_TAG_XML_PARSE: dump_cb_xml_parse ( CB_XML_PARSE(x) ); break;	/* XML PARSE statement */
	/* Miscellaneous */
	case CB_TAG_PERFORM_VARYING: dump_cb_perform_varying ( CB_PERFORM_VARYING(x) ); break;	/* PERFORM VARYING parameter */
	case CB_TAG_PICTURE: dump_cb_picture ( CB_PICTURE(x) ); break;		/* PICTURE clause */
	case CB_TAG_LIST: dump_cb_list ( CB_LIST (x) ); break;		/* List */
	case CB_TAG_DIRECT: dump_cb_direct ( CB_DIRECT(x) ); break;		/* Code dump or comment */
	case CB_TAG_DEBUG: dump_cb_debug ( CB_DEBUG(x) ); break;		/* Debug item set */
	case CB_TAG_DEBUG_CALL: dump_cb_debug_call ( CB_DEBUG_CALL(x) ); break;	/* Debug callback */
	case CB_TAG_PROGRAM: dump_cb_program ( CB_PROGRAM(x) ); break;		/* Program */
	case CB_TAG_PROTOTYPE: dump_cb_prototype ( CB_PROTOTYPE(x) ); break;	/* Prototype */
	case CB_TAG_DECIMAL_LITERAL: dump_cb_decimal ( CB_DECIMAL_LITERAL(x) ); break;	/* Decimal Literal */
		/* TODO: check this one */
	case CB_TAG_REPORT_LINE: dump_cb_tree_common ( x ); break;	/* Report line description */
	case CB_TAG_ML_SUPPRESS: dump_cb_ml_suppress_clause ( CB_ML_SUPPRESS(x) ); break;	/* JSON/XML GENERATE SUPPRESS clause */
	case CB_TAG_ML_TREE: dump_cb_ml_generate_tree ( CB_ML_TREE(x) ); break;	/* JSON/XML GENERATE dump tree */
	case CB_TAG_ML_SUPPRESS_CHECKS: dump_cb_ml_suppress_checks ( CB_ML_SUPPRESS_CHECKS(x) ); break;	/* JSON/XML GENERATE SUPPRESS checks */
	case CB_TAG_VARY: dump_cb_vary ( CB_VARY(x) ); break;			/* Report line description */
	case CB_TAG_TAB_VALS: dump_cb_table_values (CB_TAB_VALS(x)); break;			/* VALUE entries in table-format */

	}
}

void cb_dump_tree_to_file (struct cb_program *prog, const char *filename, const char* flags)
{
	char *env;
	int close_fd = 1;

	set_format_by_file_ext (filename);
	env = getenv ("COB_DUMP_TREE_FLAGS");
	if (env) set_flags (env);
	if (flags) set_flags (flags);

	indent_init ();

	if (!strcmp(filename, COB_DASH) || !strcmp(filename, "stdout")){
		close_fd = 0;
		fd = stdout;
	} else
		if (!strcmp(filename, "stderr")){
			close_fd = 0;
			fd = stderr;
		} else {
			int len = strlen (filename);
			struct stat st;
			if (len > 0 && filename[len-1] == '/'){
				filename = cobc_main_stradd_dup (filename, prog->program_id);
			}
			if (!stat (filename, &st)) unlink(filename);
			fd = fopen (filename, "w");
		}
	if (!fd){
		cb_perror (0, "cobc: %s: %s", filename, cb_get_strerror ());
	}

	fprintf (fd, "%s", fmt.format_header);
	dump_cb_program (prog);
	fprintf (fd, "%s", fmt.format_trailer);

	if (close_fd) {
		fclose (fd);
		if (cb_dump_tree_print_message){
			fprintf (stderr, "File \"%s\" generated\n", filename);
		}
	}
}

#endif

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

/* field descriptor of tree dump */
static FILE* fd = NULL;

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



static void print_struct_cb_tree_common (int indent, int ptr_uid, struct cb_tree_common *ptr);
static void print_struct_cb_tree_common_ptr (int indent, cb_tree cb);
static void print_int_array (int indent, int n, int * tab);
/* static void print_char_array (int indent, int n, char * tab);*/

static const char * get_indent (int indent){
   static int   current_indent = 0;
   static char* indent_string = NULL;
   static int   current_size ;
   if (indent == 0) return "";

   if (indent >= current_size){
     int i;
     if (indent_string != NULL) free (indent_string);
     current_size = indent + 100 ;
     indent_string = (char*) malloc ( current_size );
     current_indent = current_size-1;
     for (i=0; i<current_indent ; i++) indent_string[i] = ' ';
     indent_string [current_size-1] = 0;
   }
   if (indent != current_indent){
     indent_string [current_indent] = ' ';
     indent_string [indent] = 0;
     current_indent = indent;
   }
   return indent_string;
}

#define PRINT_NULL(indent,ty)			\
	fprintf (fd, "NULL")
#define PRINT_ENUM_TY(s)                                  \
 /*   const char* ty = s */
#define PRINT_ENUM(indent,ty,s)                           \
	fprintf (fd, "%s", s)
#define PRINT_ENUM_UNKNOWN(indent,ty,s)                   \
	fprintf (fd, "Unknown enum %d", s)

#define PRINT_STRUCT_KNOWN(indent, ty, ptr_uid) \
	fprintf (fd, "ALREADY_SEEN(%s --> MB%03d)", ty, ptr_uid)
#define PRINT_STRUCT_TY(s)                                \
	const char* ty = s
#define PRINT_STRUCT_PTR_TY(s)                            \
	const char* ty = s
#define PRINT_STRUCT_BEGIN(indent,ty, ptr_uid)			\
	if (ptr_uid != 0)					\
		fprintf (fd, "// MEMORY_BLOCK(MB%03d):\n", ptr_uid);	\
	else							\
		fprintf (fd, "// INLINE_BLOCK:\n");		\
	fprintf (fd, "%s{ //[%s]\n", get_indent (indent), ty)
#define PRINT_STRUCT_END(indent,ty)				\
	fprintf (fd, "%s} //[%s]", get_indent (indent), ty)
#define PRINT_VOID(indent)                                \
	fprintf(fd, "<void>")
#define PRINT_FUNCTION(indent)                            \
	fprintf(fd, "<function>")
#define PRINT_STRUCT_FIELD_BEGIN(indent,name)             \
	fprintf (fd, "%s " #name ": ", get_indent (indent) )
#define PRINT_STRUCT_FIELD_END(indent)                    \
	fprintf (fd, ";\n")
#define PRINT_STRUCT_FIELD_END(indent)                    \
	fprintf (fd, ";\n")


#define PRINT_ARRAY_BEGIN(indent)		\
	fprintf (fd, "[\n%s", get_indent (indent))
#define PRINT_ARRAY_SEP(indent)                           \
	fprintf (fd, ",\n%s", get_indent (indent))
#define PRINT_ARRAY_END(indent)                           \
	fprintf (fd, "]")



#define PRINT_UNKNOWN_SELECTOR(indent,ty, tag)            \
        fprintf (fd, "%s UNKNOWN %s(%d)\n", get_indent(indent), ty, tag);


static void print_size_t (int indent, size_t x)
{
	fprintf (fd, "%ld", x);
}

static void print_int (int indent, int x)
{
	fprintf (fd, "%d", x);
}

static void print_char (int indent, char x)
{
	fprintf (fd, "'%c'(%d)", x, x);
}

static void print_uchar (int indent, unsigned char x)
{
	fprintf (fd, "%d", x);
}

static void print_uint (int indent, unsigned int x)
{
	fprintf (fd, "%d", x);
}

static void print_char_ptr (int indent, const char* x)
{
   if (x == NULL){
     PRINT_NULL(indent,ty);
   } else {
     fprintf (fd, "\"%s\"", x);
   }
}

static void print_uchar_ptr (int indent, const unsigned char* x)
{
   if (x == NULL){
     PRINT_NULL(indent,ty);
   } else {
     fprintf (fd, "%s", x);
   }
}

static void print_FILE_ptr (int indent, const FILE* x)
{
   if (x == NULL){
     PRINT_NULL(indent,ty);
   } else {
     fprintf (fd, "<SOME FILE*>");
   }
}

static void print_int_ptr (int indent, const int* x)
{
   if (x == NULL){
     PRINT_NULL(indent,ty);
   } else {
     fprintf (fd, "<SOME INT*>");
   }
}

static void print_int_array (int indent, int n, int *x)
{
   if (x == NULL){
     PRINT_NULL(indent,ty);
   } else {
     fprintf (fd, "<SOME INT[]>");
   }
}

static void print_cb_word_ptr2 (int indent, struct cb_word** x)
{
   if (x == NULL){
     PRINT_NULL(indent,ty);
   } else {
     fprintf (fd, "<SOME cb_word**>");
   }
}

static void print_cb_field_ptr2 (int indent, struct cb_field** x)
{
   if (x == NULL){
     PRINT_NULL(indent,ty);
   } else {
     fprintf (fd, "<SOME cb_field**>");
   }
}


#include "dump_tree_gen.c"

static void print_struct_cb_tree_common (int indent, int ptr_uid, struct cb_tree_common *ptr){

    print_enum_cb_tag (indent+2, ptr->tag);
    fprintf (fd, "/");
    print_enum_cb_category (indent+2, ptr->category);
    fprintf (fd, "@");
    print_char_ptr (indent+2, ptr->source_file);
    fprintf (fd, ":");
    print_int (indent+2, ptr->source_line);
    
	#if 0
	
    PRINT_STRUCT_TY("struct cb_tree_common");
    PRINT_STRUCT_BEGIN(indent, ty, ptr_uid);
    indent += 2;

    PRINT_STRUCT_FIELD_BEGIN(indent,tag);

    print_enum_cb_tag (indent+2, ptr->tag);

    PRINT_STRUCT_FIELD_END(indent);

    PRINT_STRUCT_FIELD_BEGIN(indent,category);

    print_enum_cb_category (indent+2, ptr->category);

    PRINT_STRUCT_FIELD_END(indent);

                          if( ptr->source_file != NULL ){

    PRINT_STRUCT_FIELD_BEGIN(indent,source_file);

    print_char_ptr (indent+2, ptr->source_file);

    PRINT_STRUCT_FIELD_END(indent);

                          }

                          if( ptr->source_line != 0 ){

    PRINT_STRUCT_FIELD_BEGIN(indent,source_line);

    print_int (indent+2, ptr->source_line);

    PRINT_STRUCT_FIELD_END(indent);

                          }

                          if( ptr->source_column != 0 ){

    PRINT_STRUCT_FIELD_BEGIN(indent,source_column);

    print_int (indent+2, ptr->source_column);

    PRINT_STRUCT_FIELD_END(indent);

                          }

    indent -= 2;
    PRINT_STRUCT_END(indent, ty);

    #endif
}

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

void cb_dump_tree_to_file (struct cb_program *prog, const char *filename, const char* flags)
{
	char *env;
	int close_fd = 1;

	set_format_by_file_ext (filename);
	env = getenv ("COB_DUMP_TREE_FLAGS");
	if (env) set_flags (env);
	if (flags) set_flags (flags);

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
	cb_print_tree ( (cb_tree) prog);
	fprintf (fd, "%s", fmt.format_trailer);

	if (close_fd) {
		fclose (fd);
		if (cb_dump_tree_print_message){
			fprintf (stderr, "File \"%s\" generated\n", filename);
		}
	}
}

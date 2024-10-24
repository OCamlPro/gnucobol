/*
 Copyright (C) 2023-2024 Free Software Foundation, Inc.
 Written by Fabrice Le Fessant

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

/* flag: whether we should indent the file. Enabled by default */
static int print_tree_with_indent = 1;

/* flag: whether we print fields that are 0 or NULL. Disabled by default */
static int print_zero_fields = 0;

static const char * get_indent (int indent);
static void print_struct_cb_tree_common (int indent, int ptr_uid, struct cb_tree_common *ptr);
static void print_struct_cb_tree_common_ptr (int indent, cb_tree cb);
static void print_int_array (int indent, int n, int * tab);

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
     fprintf (fd, "\"%s\"", x);
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


#include "dump_ast_gen.c"

static void print_struct_cb_tree_common (int indent, int ptr_uid, struct cb_tree_common *ptr){

    print_enum_cb_tag (indent+2, ptr->tag);
    fprintf (fd, "/");
    print_enum_cb_category (indent+2, ptr->category);
    fprintf (fd, "@");
    print_char_ptr (indent+2, ptr->source_file);
    fprintf (fd, ":");
    print_int (indent+2, ptr->source_line);
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
		case 'i':
			print_tree_with_indent = sign;
			break;
		case 'z':
			print_zero_fields = sign;
			break;
		case 'A':
			print_zero_fields = sign;
			print_tree_with_indent = sign ;
			break;
		default:
			cobc_err_exit ("-fdump-ast-flags: unknown flag '%c', expecting '+-cltpinA'", flags[i]);
		}
	}
}

void cb_dump_ast_to_file (struct cb_program *prog, const char *filename, const char* flags)
{
	int close_fd = 1;

	if (flags) set_flags (flags);

	if (!strcmp(filename, COB_DASH)){
		close_fd = 0;
		fd = stdout;
	} else {
		int len = strlen (filename);
		struct stat st;
		if (!stat (filename, &st)) unlink(filename);
		fd = fopen (filename, "w");
	}
	if (!fd){
		cb_perror (0, "cobc: %s: %s", filename, cb_get_strerror ());
		return ;
	}

	cb_print_tree ( (cb_tree) prog);

	if (close_fd) {
		fclose (fd);
	}
}


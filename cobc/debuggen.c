/*
   Copyright (C) 2023 Free Software Foundation, Inc.
   Written by David Declerck.

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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cobc.h"
#include "tree.h"

/* List of known COBOL files (including copies), each with an id */
struct file_list {
	struct file_list	*next;
	const char		*name;
	int			id;
};

static struct file_list		*files = NULL;
static int			nb_files = 0;
static int			file_id = 0;

/* Mapping of C file locations to COBOL file locations */
struct ref_list {
	struct ref_list		*next;
	int			c_line;
	int			cob_file_id;
	int			cob_line;
};

static struct ref_list		*refs = NULL;
static int			nb_refs = 0;

/* Add a file to the list of known files */
static int
add_file (const char *file_name)
{
	struct file_list *f = files;
	while (f != NULL) {
		if (strcmp(file_name, f->name) == 0) {
			return f->id;
		}
		f = f->next;
	}
	f = malloc(sizeof(struct file_list));
	f->name = file_name;
	f->id = ++file_id;
	f->next = files;
	files = f;
	++nb_files;
	return f->id;
}

/* Reverse the list of known files */
static void
rev_files (void)
{
	struct file_list *f = files, *pf = NULL, *nf = NULL;
	while (f != NULL) {
		nf = f->next;
		f->next = pf;
		pf = f;
		f = nf;
	}
	files = pf;
}

/* Clear the list of known files */
static void
clear_files (void)
{
	struct file_list *f = files, *nf = NULL;
	while (f != NULL) {
		nf = f->next;
		free(f);
		f = nf;
	}
	files = NULL;
	nb_files = 0;
	file_id = 0;
}

/* Add a C/COBOL line mapping to the the source map */
static void
add_ref (int c_line, const char *cob_file_name, int cob_line)
{
	struct ref_list *r = NULL;
	if (!strcmp(cob_file_name, "register-definition")) {
		/* Skip those, as they are not user code */
		return;
	}
	r = malloc(sizeof(struct ref_list));
	r->c_line = c_line;
	r->cob_file_id = add_file(cob_file_name);
	r->cob_line = cob_line;
	r->next = refs;
	refs = r;
	++nb_refs;
}

/* Reverse the source map */
static void
rev_refs (void)
{
	struct ref_list *r = refs, *pr = NULL, *nr = NULL;
	while (r != NULL) {
		nr = r->next;
		r->next = pr;
		pr = r;
		r = nr;
	}
	refs = pr;
}

/* Clear the source map */
static void
clear_refs (void)
{
	struct ref_list *r = refs, *nr = NULL;
	while (r != NULL) {
		nr = r->next;
		free(r);
		r = nr;
	}
	refs = NULL;
	nb_refs = 0;
}

/* Output an 8-bit integer */
static size_t
output_int8 (char i8, FILE *stream)
{
	return fwrite(&i8, 1, 1, stream);
}

/* Output a 16-bit integer in big-endian format */
static size_t
output_int16 (short i16, FILE *stream)
{
	char buf[2];
	buf[0] = (char)((i16 & 0xFF00) >> 8);
	buf[1] = (char)(i16 & 0x00FF);
	return fwrite(buf, 2, 1, stream);
}

/* Output a 32-bit integer in big-endian format */
static size_t
output_int32 (int i32, FILE *stream)
{
	char buf[4];
	buf[0] = (char)((i32 & 0xFF000000) >> 24);
	buf[1] = (char)((i32 & 0x00FF0000) >> 16);
	buf[2] = (char)((i32 & 0x0000FF00) >> 8);
	buf[3] = (char)(i32 & 0x000000FF);
	return fwrite(buf, 4, 1, stream);
}

/* Output a non-null-terminated string prefixed with its 8-bit size */
static size_t
output_string_sz8 (const char *str, FILE *stream)
{
	size_t sz = strlen(str);
	output_int8((char)sz, stream);
	fputs(str, stream);
	return sz + 1;
}

/* Output a non-null-terminated string prefixed with its 16-bit size */
static size_t
output_string_sz16 (const char *str, FILE *stream)
{
	size_t sz = strlen(str);
	output_int16((short)sz, stream);
	fputs(str, stream);
	return sz + 2;
}

/* Output the debug informations relative to fields */
static void
output_debug_infos_fields (const struct cb_field *fields, FILE *df)
{
	const struct cb_field *f = NULL;
	char buf[16];
	int nb_fields = 0;
	int i = 0;

	/* Compute and output the number of relevant fields */
	for (f = fields; f != NULL; f = f->sister) {
		if (!f->flag_internal_register && strcmp(f->name, "COB-CRT-STATUS")) {
			++nb_fields;
		}
	}
	output_int32(nb_fields, df);

	/* Output relevant information for each field */
	for (f = fields; f != NULL; f = f->sister) {
		if (!f->flag_internal_register && strcmp(f->name, "COB-CRT-STATUS")) {

			/* (1 byte) Level */
			output_int8(f->level, df);

			/* (string) COBOL field name */
			output_string_sz8(f->name, df);

			/* (1 byte) Indexes */
			output_int8((char)f->indexes, df);

			/* (4 byte) Occurs <min> */
			output_int32(f->occurs_min, df);

			/* (4 byte) Occurs <max> */
			output_int32(f->occurs_max, df);

			/* (1 byte) Usage */
			output_int8((char)f->usage, df);

			/* (1 byte) Field type/category */
			output_int8((char)CB_TREE_CATEGORY(f), df);

			/* (1 byte) Flag: has picture */
			output_int8((f->pic == NULL) ? 0 : 1, df);

			/* Handle picture if present */
			if (f->pic != NULL) {

				/* (1 byte) Digits */
				output_int8((char)f->pic->digits, df);

				/* (1 byte) Scale (1.10 ^ scale) */
				output_int8((char)f->pic->scale, df);

				/* (1 byte) Sign */
				output_int8((char)f->pic->have_sign, df);

				/* (1 byte) Number of symbols/count */
				output_int8((char)f->pic->lenstr, df);
				for (i = 0; i < f->pic->lenstr; ++i) {
					output_int8((char)f->pic->str[i].symbol, df);
					output_int8((char)f->pic->str[i].times_repeated, df);
				}
			}

			/* (string) C variable name */
			snprintf(buf, sizeof(buf), "b_%d", f->id);
			output_string_sz8(buf, df);

			/* (4 bytes) Offset from level 01 */
			output_int32(f->offset, df);

			/* (4 bytes) Field size */
			output_int32(f->size, df);

			/* Output children fields (if any) */
			output_debug_infos_fields(f->children, df);
		}
	}
}

/* Output all debug informations */
static void
output_debug_infos (const struct cb_program *prog, const char *c_file)
{
	const struct file_list *f = NULL;
	const struct ref_list *r = NULL;
	const struct cb_program *p = NULL;
	FILE *df = NULL;
	char fname[1024];
	int nb_progs = 0;
	int i = 0;

	/* Build the debug database file name */
	strncpy(fname, prog->common.source_file, sizeof(fname) - 1);
	i = strnlen(fname, sizeof(fname));
	while ((i >= 0) && (fname[i] != '.')) --i;
	strncpy(&fname[i+1], "cdb", sizeof(fname) - (i + 1));

	/* Open the debug database file */
	df = fopen(fname, "wb");
	if (df == NULL) {
		return;
	}

	/* Output file signature and version (GCDB = GnuCobol DataBase) */
	fputs("GCDB", df);
	output_int16(0x0001, df);

	/* Output the list of COBOL files with their ids */
	output_int32(nb_files, df);
	for (f = files; f != NULL; f = f->next) {
		output_string_sz16(f->name, df);
	}

	/* Output the name of the C file */
	output_string_sz16(c_file, df);

	/* Output the source mapping */
	output_int32(nb_refs, df);
	for (r = refs; r != NULL; r = r->next) {
		output_int32(r->c_line, df);
		output_int16((short)r->cob_file_id, df);
		output_int32(r->cob_line, df);
	}

	/* Compute and output the number of programs */
	for (p = prog; p != NULL; p = p->next_program) {
		++nb_progs;
	}
	output_int32(nb_progs, df);

	/* For each program, output its various storages */
	for (p = prog; p != NULL; p = p->next_program) {
		output_string_sz8(p->orig_program_id, df);
		output_debug_infos_fields(p->working_storage, df);
		output_debug_infos_fields(p->local_storage, df);
		output_debug_infos_fields(p->linkage_storage, df);
		output_debug_infos_fields(p->screen_storage, df);
		output_debug_infos_fields(p->report_storage, df);
	}

	fclose(df);
}

void
debuggen_init (void)
{
	clear_files();
	clear_refs();
}

void
debuggen_add_ref (int c_line, const char *cob_file_name, int cob_line)
{
	add_ref(c_line, cob_file_name, cob_line);
}

void
debuggen_finalize (const struct cb_program *prog, const char *c_file)
{
	rev_files();
	rev_refs();
	output_debug_infos(prog, c_file);
}

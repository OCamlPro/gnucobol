/*
   Copyright (C) 2003-2012, 2014-2021 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Simon Sobisch, Ron Norman

   This file is part of GnuCOBOL.

   The GnuCOBOL runtime library is free software: you can redistribute it
   and/or modify it under the terms of the GNU Lesser General Public License
   as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   GnuCOBOL is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with GnuCOBOL.  If not, see <https://www.gnu.org/licenses/>.
*/


#include <config.h>

#ifndef	_GNU_SOURCE
#define _GNU_SOURCE	1
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>

#include "sysdefines.h"

/* Force symbol exports */
#define	COB_LIB_EXPIMP
#include "libcob.h"
#include "coblocal.h"
#include "cobcapi.h"

static cob_field_attr	const_float_attr =
			{COB_TYPE_NUMERIC_DOUBLE, 8, 0, COB_FLAG_HAVE_SIGN, NULL};
static cob_field_attr	const_binll_attr =
			{COB_TYPE_NUMERIC_BINARY, 18, 0, COB_FLAG_HAVE_SIGN, NULL};
static cob_field_attr	const_binull_attr =
			{COB_TYPE_NUMERIC_BINARY, 18, 0, 0, NULL};

static size_t		call_lastsize = 0;
static void			*call_buffer = NULL;
static cob_global	*cobglobptr = NULL;
static cob_settings	*cobsetptr = NULL;

/* Local functions */
static void *
cob_get_buff (const size_t buffsize)
{
	if (buffsize > call_lastsize) {
		call_lastsize = buffsize;
		cob_free (call_buffer);
		call_buffer = cob_fast_malloc (buffsize);
	}
	return call_buffer;
}


void
cob_init_cobcapi (cob_global *lptr, cob_settings* sptr)
{
	cobglobptr = lptr;
	cobsetptr = sptr;
} 

void
cob_exit_cobcapi (void)
{
	if (call_buffer) {
		cob_free (call_buffer);
		call_buffer = NULL;
		call_lastsize = 0;
	}
}

/******************************************
 * Routines for C interface with COBOL
 */

cob_field *
cob_get_param_field (int n, const char *caller_name)
{
	if (cobglobptr == NULL
	 || COB_MODULE_PTR == NULL) {
		/* note: same message in call.c */
		cob_runtime_warning_external (caller_name, 1,
			_("cob_init() has not been called"));
		return NULL;
	}
	if (n < 1
	 || n > cobglobptr->cob_call_params) {
		cob_runtime_warning_external (caller_name, 1,
			_("parameter %d is not within range of %d"),
			n, cobglobptr->cob_call_params);
		return NULL;
	}
	if (COB_MODULE_PTR->cob_procedure_params[n - 1] == NULL) {
		cob_runtime_warning_external (caller_name, 1,
			_("parameter %d is NULL"), n);
		return NULL;
	}
	return COB_MODULE_PTR->cob_procedure_params[n - 1];
}

int
cob_get_name_line ( char *prog, int *line )
{
	int	k;
	if (line != NULL)
		*line = COB_GET_LINE_NUM(COB_MODULE_PTR->module_stmt);
	if (prog != NULL) {
		strcpy(prog, COB_MODULE_PTR->module_name);
		for (k=strlen(prog); k > 0 && prog[k-1] == ' '; k--)
			prog[k-1] = 0;
	}
	return COB_GET_LINE_NUM(COB_MODULE_PTR->module_stmt);
}

int
cob_get_num_params ( void )
{
	if (cobglobptr) {
		return cobglobptr->cob_call_params;
	}

	/* note: same message in call.c */
	cob_runtime_warning_external ("cob_get_num_params", 1,
		_("cob_init() has not been called"));
	return -1;
}

int
cob_get_param_type (int n)
{
	cob_field* f = cob_get_param_field (n, "cob_get_param_type");
	return cob_get_field_type (f);
}

int
cob_get_param_size (int n)
{
	cob_field	*f = cob_get_param_field (n, "cob_get_param_size");

	if (f == NULL)
		return -1;
	return (int)f->size;
}

int
cob_get_param_sign (int n)
{
	cob_field	*f = cob_get_param_field (n, "cob_get_param_sign");
	if (f == NULL)
		return -1;
	if (COB_FIELD_HAVE_SIGN(f)) {
		return 1;
	}
	return 0;
}

int
cob_get_param_scale (int n)
{
	cob_field	*f = cob_get_param_field (n, "cob_get_param_scale");
	if (f == NULL) 
		return -1;
	return (int)f->attr->scale;
}

int
cob_get_param_digits (int n)
{
	cob_field	*f = cob_get_param_field (n, "cob_get_param_digits");
	if (f == NULL) 
		return -1;
	return (int)f->attr->digits;
}

int
cob_get_param_constant (int n)
{
	cob_field	*f = cob_get_param_field (n, "cob_get_param_constant");
	return cob_get_field_constant (f);
}

const char *
cob_get_param_str (int n, char *buffer, size_t size)
{
	cob_field	*f = cob_get_param_field (n, "cob_get_param_str");
	return cob_get_field_str (f, buffer, size);
}

const char *
cob_get_param_str_buffered (int n)
{
	cob_field	*f = cob_get_param_field (n, "cob_get_param_str_buffered");
	return cob_get_field_str_buffered (f);
}

int
cob_put_param_str (int n, const char *str)
{
	cob_field	*f = cob_get_param_field (n, "cob_put_param_str");
	return cob_put_field_str (f, str);
}

void *
cob_get_param_data (int n)
{
	cob_field	*f = cob_get_param_field (n, "cob_get_param_data");
	if (f == NULL) {
		return NULL;
	}
	return (void*)f->data;
}

double
cob_get_dbl_param (int n)
{
	void		*cbl_data;
	double		val;
	cob_field	temp;
	cob_field_attr   float_attr;
	cob_field	*f = cob_get_param_field (n, "cob_get_dbl_param");

	if (f == NULL) {
		return (double)-1;
	}
	cbl_data = f->data;

	switch (f->attr->type) {
	case COB_TYPE_NUMERIC_FLOAT:
		return (double)cob_get_comp1 (cbl_data);
	case COB_TYPE_NUMERIC_DOUBLE:
		return (double)cob_get_comp2 (cbl_data);
	default:
		memcpy(&float_attr, &const_float_attr, sizeof(cob_field_attr));
		float_attr.scale = f->attr->scale;
		temp.size = 8;
		temp.data = (unsigned char *)&val;
		temp.attr = &float_attr;
		cob_move (f, &temp);
		return (double)val;
	}
}

void
cob_put_dbl_param (int n, double val)
{
	void		*cbl_data;
	cob_field	temp;
	cob_field_attr   float_attr;
	cob_field	*f = cob_get_param_field (n, "cob_get_dbl_param");

	if (f == NULL) {
		return;
	}
	cbl_data = f->data;

	switch (f->attr->type) {
	case COB_TYPE_NUMERIC_FLOAT:
		cob_put_comp1 ((float)val, cbl_data);
		return;
	case COB_TYPE_NUMERIC_DOUBLE:
		cob_put_comp2 (val, cbl_data);
		return;
	default:
		memcpy(&float_attr, &const_float_attr, sizeof(cob_field_attr));
		float_attr.scale = f->attr->scale;
		temp.size = 8;
		temp.data = (unsigned char *)&val;
		temp.attr = &float_attr;
		cob_move (&temp, f);
		return;
	}
}

cob_s64_t
cob_get_s64_param (int n)
{
	void		*cbl_data;
	int		size;
	cob_s64_t	val;
	double		dbl;
	cob_field	temp;
	cob_field	*f = cob_get_param_field (n, "cob_get_s64_param");

	if (f == NULL) {
		return -1;
	}
	cbl_data = f->data;
	size = f->size;

	switch (f->attr->type) {
	case COB_TYPE_NUMERIC_DISPLAY:
		return cob_get_s64_pic9 (cbl_data, size);
	case COB_TYPE_NUMERIC_BINARY:
#ifndef WORDS_BIGENDIAN
		if (!COB_FIELD_BINARY_SWAP (f)) {
			return cob_get_s64_comp5 (cbl_data, size);
		}
#endif
		return cob_get_s64_compx (cbl_data, size);
	case COB_TYPE_NUMERIC_PACKED:
		return cob_get_s64_comp3 (cbl_data, size);
	case COB_TYPE_NUMERIC_FLOAT:
		dbl = cob_get_comp1 (cbl_data);
		val = (cob_s64_t)dbl; /* possible data loss is explicit requested */
		return val;
	case COB_TYPE_NUMERIC_DOUBLE:
		dbl = cob_get_comp2 (cbl_data);
		val = (cob_s64_t)dbl; /* possible data loss is explicit requested */
		return val;
	case COB_TYPE_NUMERIC_EDITED:
		return cob_get_s64_pic9 (cbl_data, size);
	default:
		temp.size = 8;
		temp.data = (unsigned char *)&val;
		temp.attr = &const_binll_attr;
		const_binll_attr.scale = f->attr->scale;
		cob_move (f, &temp);
		return val;
	}
}

cob_u64_t
cob_get_u64_param (int n)
{
	void		*cbl_data;
	int		size;
	cob_u64_t	val;
	double		dbl;
	cob_field	temp;
	cob_field	*f = cob_get_param_field (n, "cob_get_u64_param");

	if (f == NULL) {
		return 0;
	}

	cbl_data = f->data;
	size    = f->size;
	switch (COB_MODULE_PTR->cob_procedure_params[n - 1]->attr->type) {
	case COB_TYPE_NUMERIC_DISPLAY:
		return cob_get_u64_pic9 (cbl_data, size);
	case COB_TYPE_NUMERIC_BINARY:
#ifndef WORDS_BIGENDIAN
		if (!COB_FIELD_BINARY_SWAP (f)) {
			return cob_get_u64_comp5 (cbl_data, size);
		}
#endif
		return cob_get_u64_compx (cbl_data, size);
	case COB_TYPE_NUMERIC_PACKED:
		return cob_get_u64_comp3 (cbl_data, size);
	case COB_TYPE_NUMERIC_FLOAT:
		dbl = cob_get_comp1 (cbl_data);
		val = (cob_u64_t)dbl; /* possible data loss is explicit requested */
		return val;
	case COB_TYPE_NUMERIC_DOUBLE:
		dbl = cob_get_comp2 (cbl_data);
		val = (cob_u64_t)dbl; /* possible data loss is explicit requested */
		return val;
	case COB_TYPE_NUMERIC_EDITED:
		return cob_get_u64_pic9 (cbl_data, size);
	default:
		temp.size = 8;
		temp.data = (unsigned char *)&val;
		temp.attr = &const_binull_attr;
		const_binull_attr.scale = f->attr->scale;
		cob_move (f, &temp);
		return val;
	}
}

char *
cob_get_picx_param (int n, void *char_field, size_t char_len)
{
	cob_field	*f = cob_get_param_field (n, "cob_get_picx_param");
	if (f == NULL) {
		return NULL;
	}
	return cob_get_picx (f->data, f->size, char_field, char_len);
}

int
cob_get_field_type (const cob_field *f)
{
	if (f == NULL) {
		return -1;
	}
	if (f->attr->type == COB_TYPE_NUMERIC_BINARY) {
		if (COB_FIELD_REAL_BINARY (f)) {
			return COB_TYPE_NUMERIC_COMP5;
		}
#ifndef WORDS_BIGENDIAN
		if (!COB_FIELD_BINARY_SWAP (f)) {
			return COB_TYPE_NUMERIC_COMP5;
		}
#endif
	}
	return (int)f->attr->type;
}

int
cob_get_field_size (const cob_field *f)
{
	if (f == NULL) {
		return -1;
	}
	return (int)f->size;
}

int
cob_get_field_sign (const cob_field *f)
{
	if (f == NULL) {
		return -1;
	}
	return COB_FIELD_HAVE_SIGN (f);
}

int
cob_get_field_scale (const cob_field *f)
{
	if (f == NULL) {
		return -1;
	}
	return (int)f->attr->scale;
}

int
cob_get_field_digits (const cob_field *f)
{
	if (f == NULL) {
		return -1;
	}
	return (int)f->attr->digits;
}

int
cob_get_field_constant (const cob_field *f)
{
	if (f == NULL) {
		return -1;
	}
#if 0 /* maybe previously? got the contant value? */
	return COB_FIELD_CONSTANT (f);
#else
	if (f == NULL) 
		return -1;
	if (COB_FIELD_CONTENT(f)) 
		return 3;
	if (COB_FIELD_VALUE(f)) 
		return 2;
	if (COB_FIELD_CONSTANT(f)) 
		return 1;
	return 0;
#endif
}

const char *
cob_get_field_str (const cob_field *f, char *buffer, size_t size)
{
	if (f == NULL) {
		return _("NULL field");
	}
	/* variable field's and empty literals may be of zero size */
	if (f->size == 0) {
		return "";
	}
	/* check if field has data assigned (may be a BASED / LINKAGE item) */
	if (f->data == NULL) {
		return _("field not allocated");
	}
	if (!buffer || !size) {
		cob_runtime_warning_external ("cob_get_field_str", 0, "bad buffer/size");
		return "";
	}
	{
		FILE *fp;
#ifdef HAVE_FMEMOPEN
		fp = fmemopen (buffer, size, "w");
#else
		fp = cob_create_tmpfile ("display");
#endif
		if (fp) {
			unsigned char pretty = COB_MODULE_PTR->flag_pretty_display;
			COB_MODULE_PTR->flag_pretty_display = 1;
			cob_display_common (f, fp);
#ifndef HAVE_FMEMOPEN
			{
				int cur_pos = ftell (fp);
				if (cur_pos >= 0) {
					size_t	pos = (size_t) cur_pos;
					fseek (fp, 0, SEEK_SET);
					fread ((void*)buffer, 1, pos, fp);
					if (size > pos) buffer[pos] = 0;
				}
			}
#endif
			fclose (fp);
			COB_MODULE_PTR->flag_pretty_display = pretty;
		}
	}
	return buffer;
}

const char *
cob_get_field_str_buffered (const cob_field *f)
{
	char	*buff = NULL;
	size_t	size = cob_get_field_size (f) + 1;

	if (size > 0) {
		if (size < 32) {
			size = 32;
		}
		buff = cob_get_buff (size);
	}
	return cob_get_field_str (f, buff, size);
}

int
cob_put_field_str (const cob_field *dst, const char *str)
{
	const cob_field_attr	const_alpha_attr =
			{COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
	cob_field wrk;

	if (!dst ||!str) return EINVAL;

	/* come back later for DYNAMIC LENGTH fields */
	if (dst->size <= 0) return EINVAL;

	if (COB_FIELD_CONSTANT (dst)) {
		cob_runtime_warning_external ("cob_put_field_str", 1,
			_ ("attempt to over-write constant field with '%s'"),
			str);
		return EINVAL;
	}


	wrk.attr = &const_alpha_attr;
	wrk.size = strlen (str);
	wrk.data = (unsigned char *)str;

	if (COB_FIELD_IS_NUMERIC (dst)) {
		if (COB_FIELD_TYPE (dst) & COB_TYPE_NUMERIC_FLOAT
		 || COB_FIELD_TYPE (dst) & COB_TYPE_NUMERIC_DOUBLE) {
			if (cob_check_numval_f (&wrk)) return 1;
			wrk = *cob_intr_numval_f (&wrk);
		} else {
			if (cob_check_numval (&wrk, NULL, 0, 1)) return 1;
			wrk = *cob_intr_numval (&wrk);
		}
	}
	cob_move (&wrk, (cob_field *)dst);
	return 0;
}

void
cob_put_s64_param (int n, cob_s64_t val)
{
	void		*cbl_data;
	int		size;
	float		flt;
	double		dbl;
	cob_field	temp;
	cob_field	*f = cob_get_param_field (n, "cob_put_s64_param");

	if (f == NULL) {
		return;
	}

	if (COB_FIELD_CONSTANT (f)) {
		cob_runtime_warning_external ("cob_put_s64_param", 1,
			_("attempt to over-write constant parameter %d with " CB_FMT_LLD),
			n, val);
		return;
	}
	cbl_data = f->data;
	size = f->size;
	switch (f->attr->type) {
	case COB_TYPE_NUMERIC_DISPLAY:
		cob_put_s64_pic9 (val, cbl_data, size);
		return;
	case COB_TYPE_NUMERIC_BINARY:
#ifndef WORDS_BIGENDIAN
		if (!COB_FIELD_BINARY_SWAP (f)) {
			cob_put_s64_comp5 (val, cbl_data, size);
			return;
		}
#endif
		cob_put_s64_compx (val, cbl_data, size);
		return;
	case COB_TYPE_NUMERIC_PACKED:
		cob_put_s64_comp3 (val, cbl_data, size);
		return;
	case COB_TYPE_NUMERIC_FLOAT:
		flt = (float)val;  /* possible data loss is explicit requested */
		cob_put_comp1 (flt, cbl_data);
		return;
	case COB_TYPE_NUMERIC_DOUBLE:
		dbl = (double)val; /* possible data loss is explicit requested */
		cob_put_comp2 (dbl, cbl_data);
		return;
	default:	/* COB_TYPE_NUMERIC_EDITED, ... */
		temp.size = 8;
		temp.data = (unsigned char *)&val;
		temp.attr = &const_binll_attr;
		const_binll_attr.scale = f->attr->scale;
		cob_move (&temp, f);
		return;
	}
}

void
cob_put_u64_param (int n, cob_u64_t val)
{
	void		*cbl_data;
	int		size;
	float		flt;
	double		dbl;
	cob_field	temp;
	cob_field	*f = cob_get_param_field (n, "cob_put_u64_param");

	if (f == NULL) {
		return;
	}

	if (COB_FIELD_CONSTANT (f)) {
		cob_runtime_warning_external ("cob_put_u64_param", 1,
			_("attempt to over-write constant parameter %d with " CB_FMT_LLD),
			n, val);
		return;
	}
	cbl_data = f->data;
	size = f->size;
	switch (f->attr->type) {
	case COB_TYPE_NUMERIC_DISPLAY:
		cob_put_u64_pic9 (val, cbl_data, size);
		return;
	case COB_TYPE_NUMERIC_BINARY:
#ifndef WORDS_BIGENDIAN
		if (!COB_FIELD_BINARY_SWAP (f)) {
			cob_put_u64_comp5 (val, cbl_data, size);
			return;
		}
#endif
		cob_put_u64_compx (val, cbl_data, size);
		return;
	case COB_TYPE_NUMERIC_PACKED:
		cob_put_u64_comp3 (val, cbl_data, size);
		return;
	case COB_TYPE_NUMERIC_FLOAT:
		flt = (float)val;  /* possible data loss is explicit requested */
		cob_put_comp1 (flt, cbl_data);
		return;
	case COB_TYPE_NUMERIC_DOUBLE:
		dbl = (double)val;  /* possible data loss is explicit requested */
		cob_put_comp2 (dbl, cbl_data);
		return;
	default:	/* COB_TYPE_NUMERIC_EDITED, ... */
		temp.size = 8;
		temp.data = (unsigned char *)&val;
		temp.attr = &const_binll_attr;
		const_binll_attr.scale = f->attr->scale;
		cob_move (&temp, f);
		return;
	}
}

void
cob_put_picx_param (int n, void *char_field)
{
	cob_field	*f = cob_get_param_field (n, "cob_put_picx_param");

	if (f == NULL || char_field == NULL) {
		return;
	}

	if (COB_FIELD_CONSTANT (f)) {
		cob_runtime_warning_external ("cob_put_picx_param", 1,
			_("attempt to over-write constant parameter %d with '%s'"),
			n, (char*)char_field);
		return;
	}

	cob_put_picx (f->data, f->size, char_field);
}

void *
cob_get_grp_param (int n, void *char_field, size_t len)
{
	cob_field	*f = cob_get_param_field (n, "cob_get_grp_param");

	if (f == NULL) {
		return NULL;
	}
	if (len == 0) {
		len = f->size;
	}

	if (char_field == NULL) {
		if (len < f->size) {
			len = f->size;
		}
		char_field = cob_malloc (len);
	}
	memcpy (char_field, f->data, f->size);
	return char_field;
}

void
cob_put_grp_param (int n, void *char_field, size_t len)
{
	cob_field	*f = cob_get_param_field (n, "cob_put_grp_param");

	if (f == NULL || char_field == NULL) {
		return;
	}

	if (COB_FIELD_CONSTANT (f)) {
		cob_runtime_warning_external ("cob_put_grp_param", 1,
			"attempt to over-write constant parameter %d", n);
		return;
	}

	if (len == 0 || len > f->size) {
		len = f->size;
	}
	memcpy (f->data, char_field, len);
}

/* Create copy of field and mark as a CONSTANT */
void
cob_field_constant (cob_field *f, cob_field *t, cob_field_attr *a, void *d)
{
	memcpy((void*)t, (void*)f, sizeof(cob_field));
	memcpy((void*)a, (void*)f->attr, sizeof(cob_field_attr));
	t->data = d;
	t->attr = a;
	a->flags |= COB_FLAG_CONSTANT;
	memmove((void*)t->data, (void*)f->data, f->size);
}

/* Create copy of field and mark as a VALUE */
void
cob_field_value (cob_field *f, cob_field *t, cob_field_attr *a, void *d)
{
	memcpy((void*)t, (void*)f, sizeof(cob_field));
	memcpy((void*)a, (void*)f->attr, sizeof(cob_field_attr));
	t->data = d;
	t->attr = a;
	a->flags |= COB_FLAG_VALUE;
	memmove((void*)t->data, (void*)f->data, f->size);
}

/* Create copy of field and mark as a CONTENT */
void
cob_field_content (cob_field *f, cob_field *t, cob_field_attr *a, void *d)
{
	memcpy((void*)t, (void*)f, sizeof(cob_field));
	memcpy((void*)a, (void*)f->attr, sizeof(cob_field_attr));
	t->data = d;
	t->attr = a;
	a->flags |= COB_FLAG_CONTENT;
	memmove((void*)t->data, (void*)f->data, f->size);
}

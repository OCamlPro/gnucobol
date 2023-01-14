/*
   Copyright (C) 2003-2012, 2014-2022 Free Software Foundation, Inc.
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


#include "config.h"

#ifndef	_GNU_SOURCE
#define _GNU_SOURCE	1
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <string.h>
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
#include "common.h"
#include "coblocal.h"
#include "cobcapi.h"

static const cob_field_attr	all_display_attr =
				{COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
static cob_field_attr	const_float_attr =
			{COB_TYPE_NUMERIC_DOUBLE, 8, 0, COB_FLAG_HAVE_SIGN, NULL};
static cob_field_attr	const_binll_attr =
			{COB_TYPE_NUMERIC_BINARY, 18, 0, COB_FLAG_HAVE_SIGN, NULL};
static cob_field_attr	const_binull_attr =
			{COB_TYPE_NUMERIC_BINARY, 18, 0, 0, NULL};

static size_t		capi_lastsize = 0;
static void			*capi_buffer = NULL;
static cob_global	*cobglobptr = NULL;
static cob_settings	*cobsetptr = NULL;
static struct watch_list {
		struct watch_list	*next;
		char				*field_ref;
		cob_module			*mod;
		cob_field			f;
		void				*saved;
}	*head_watch	= NULL;

/* Local functions */
static void *
cob_get_buff (const size_t buffsize)
{
	if (buffsize > capi_lastsize) {
		capi_lastsize = buffsize;
		cob_free (capi_buffer);
		capi_buffer = cob_fast_malloc (buffsize);
	}
	return capi_buffer;
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
	struct watch_list *wl,*wp;
	if (capi_buffer) {
		cob_free (capi_buffer);
		capi_buffer = NULL;
		capi_lastsize = 0;
	}
	for (wl = head_watch; wl; wl = wp) {
		cob_free (wl->field_ref);
		cob_free (wl->saved);
		wp = wl->next;
		cob_free (wl);
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
cob_get_name_line (char *prog, int *line)
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
cob_get_num_params (void)
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
	cob_field	*f = cob_get_param_field (n, "cob_get_param_type");
	return cob_get_field_type (f);
}

int
cob_get_param_size (int n)
{
	cob_field	*f = cob_get_param_field (n, "cob_get_param_size");
	return cob_get_field_size (f);
}

int
cob_get_param_sign (int n)
{
	cob_field	*f = cob_get_param_field (n, "cob_get_param_sign");
	return cob_get_field_sign (f);
}

int
cob_get_param_scale (int n)
{
	cob_field	*f = cob_get_param_field (n, "cob_get_param_scale");
	return cob_get_field_scale (f);
}

int
cob_get_param_digits (int n)
{
	cob_field	*f = cob_get_param_field (n, "cob_get_param_digits");
	return cob_get_field_digits (f);
}

int
cob_get_param_constant (int n)
{
	cob_field	*f = cob_get_param_field (n, "cob_get_param_constant");
	return cob_get_field_constant (f);
}

int
cob_get_param_right (int n)
{
	cob_field	*f = cob_get_param_field (n, "cob_get_param_constant");
	return cob_get_field_right (f);
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
#if 0 /* CHECKME: previously returned "is constant yes=1, no=0";
         possibly split into two functions */
	return COB_FIELD_CONSTANT (f);
#else
	if (COB_FIELD_CONTENT(f)) 
		return 3;
	if (COB_FIELD_VALUE(f)) 
		return 2;
	if (COB_FIELD_CONSTANT(f)) 
		return 1;
	return 0;
#endif
}

int
cob_get_field_right (const cob_field *f)
{
	if (f == NULL) {
		return -1;
	}
	if (COB_FIELD_JUSTIFIED(f)) 
		return 1;
	return 0;
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
			/* TODO: at least for numeric items: verify minimal length of buffer
			         as cob_display_common will not check the size there */
			unsigned char pretty = COB_MODULE_PTR->flag_pretty_display;
			COB_MODULE_PTR->flag_pretty_display = 1;
			cob_display_common (f, fp);
			COB_MODULE_PTR->flag_pretty_display = pretty;
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
	cob_field	s[1];

	if (f == NULL || char_field == NULL) {
		return;
	}

	if (COB_FIELD_CONSTANT (f)) {
		cob_runtime_warning_external ("cob_put_picx_param", 1,
			_("attempt to over-write constant parameter %d with '%s'"),
			n, (char*)char_field);
		return;
	}

	if (COB_FIELD_JUSTIFIED (f)) {
		s->attr = &all_display_attr;
		s->data = char_field;
		s->size = strlen (char_field);
		cob_move (s, f);
	} else {
		cob_put_picx (f->data, f->size, char_field);
	}
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

static int
get_name (int pos, char *field_ref, char *fld)
{
	int i;
	while (field_ref[pos] == ' ') pos++;
	for (i=0; field_ref[pos] != 0 
		&& (isalnum (field_ref[pos]) || field_ref[pos] == '-'); ) {
		fld[i++] = field_ref[pos++];
	}
	fld[i] = 0;
	if (field_ref[pos] == ',') pos++;
	while (field_ref[pos] == ' ') pos++;
	if (field_ref[pos] == ',') pos++;
	return pos;
}

static int
find_field (cob_module  *mod, char *name)
{
	cob_symbol  *sym = mod->module_symbols;
	int	k;
	for (k=0; k < mod->num_symbols; k++) {
		if (sym[k].name == NULL)
			continue;
		if (strcasecmp (sym[k].name, name) == 0) {
			return k;
		}
	}
	return -1;
}

static int
get_value (int pos, cob_module  *mod, char *field_ref, char *fld)
{
	int		i;
	cob_field	s;
	char	fld2[48], numval[48];
	cob_symbol  *sym = mod->module_symbols;

	strcpy(fld,"-1");
	pos = get_name (pos, field_ref, fld2);
	for (i=0; fld2[i] != 0 && isdigit(fld2[i]); i++);
	if (fld2[i] != 0) {
		i = find_field (mod, fld2);
		if (i < 0) {
			return pos;
		}
		cob_sym_get_field (&s, sym, i);
		cob_get_field_str (&s, numval, sizeof(numval));
		strcpy(fld,numval);
	} else {
		strcpy(fld, fld2);
	}
	return pos;
}

static int
cob_parse_field (cob_module  *mod, cob_field *f, char *field_ref)
{
	int		i, j, k, of, subs;
	int		subval[12], refbgn,reflen;
	char	fld1[8][48], fld2[48];
	cob_symbol  *sym = mod->module_symbols;

	j = get_name (0, field_ref, fld1[of=0]);
	while (field_ref[j] != 0) {
		if (field_ref[j] == '(') 
			break;
		j = get_name (j, field_ref, fld2);
		if (strcasecmp (fld2,"OF") == 0)
			continue;
		strcpy (fld1[++of], fld2);
	}
	k = find_field (mod, fld1[of]);
	if (k < 0) {
		return 1;
	}
	while (of > 0) {
		of--;
		while (k < mod->num_symbols) {
			if (sym[k].name == NULL)
				continue;
			if (strcasecmp (sym[k].name, fld1[of]) == 0) {
				break;
			}
			k++;
		}
	}
	if (sym[k].name == NULL
	 || strcasecmp (sym[k].name, fld1[0]) != 0) {
		return 1;
	}
	cob_sym_get_field (f, sym, k);
	if (f->data == NULL)
		return 1;
	refbgn = reflen = 0;
	if (field_ref[j] == '(') {			/* Subscripts */
		for (subs = 0; subs < 12; subs++)
			subval[subs] = 1;
		j++;
		for (subs = 0; subs < 12; subs++) {
			if (field_ref[j] == ')'
			 || field_ref[j] == 0)
				break;
			j = get_value (j, mod, field_ref, fld2);
			if (field_ref[j] == ':') {	/* Must be ref mod and not subscript */
				refbgn = atoi (fld2);
				j = get_value (j+1, mod, field_ref, fld2);
				if (field_ref[j] == ')') j++;
				reflen = atoi (fld2);
				break;
			}
			subval[subs] = atoi (fld2);
			if (subval[subs] == -1)
				return 1;
		}
		if (sym[k].subscripts < subs) {
			subs = sym[k].subscripts;
		}
		i = 0;
		for (j = k; subs > 0; j = sym[j].parent) {
			if (sym[j].occurs > 1) {
				subs--;
				if (subval[subs] < 1) subval[subs] = 1;
				if (subval[subs] > sym[j].occurs) subval[subs] = sym[j].occurs;
				i = i + ((subval[subs] - 1) * sym[j].size);
			}
		}
		f->data = f->data + i;
	}
	if (field_ref[j] == ')') j++;
	while (field_ref[j] == ' ') j++;

	if (field_ref[j] == '(') {	/* Reference modification */
		j = get_value (j+1, mod, field_ref, fld2);
		if (field_ref[j] != ':')
			return 1;
		refbgn = atoi (fld2);
		j = get_value (j+1, mod, field_ref, fld2);
		if (field_ref[j] == ')') j++;
		reflen = atoi (fld2);
	}
	if (refbgn > 0) {
		refbgn--;
		if (reflen == 0) {
			reflen = f->size - refbgn;
		}
		f->size = reflen;
		f->data = f->data + refbgn;
	}
	return 0;
}

/* 
 * Using cob_symbols table, parse and evaluation the field reference
 * and return the value
 * 'mod_name' is the module name, if NULL, then current module
 * 'field_ref' is the field reference, name, name of grp, subscripts etc
 * 'buflen' is length of buf
 * 'buf' receives the contents of the field
 *
 * return is ZERO, if all ok, else an error status
 */
static int
cob_setup_field (char *mod_name, int buflen, char *buf, cob_module **xmod)
{
	cob_module	*mod;

	if (cobglobptr == NULL
	 || COB_MODULE_PTR == NULL) {
		snprintf(buf,(size_t)buflen,"Not initialized");
		return 1;
	}
	if (mod_name == NULL) {
		mod = COB_MODULE_PTR;
	} else {
		for (mod = COB_MODULE_PTR; mod; mod = mod->next) {
			if (strcasecmp (mod_name, mod->module_name) == 0)
				break;
		}
		if (mod == NULL) {
			snprintf(buf,(size_t)buflen,"Module %s not found",mod_name);
			return 1;
		}
	}
	if (mod->module_symbols == NULL
	 || mod->num_symbols < 1) { 		/* No cob_symbol table present */
		snprintf (buf,(size_t)buflen,"Error: %s not compiled with -debug",mod->module_name);
		return 1;
	}
	*xmod = mod;
	return 0;
}

/* 
 * Using cob_symbols table, parse and evaluation the field reference
 * and return the value
 * 'mod_name' is the module name, if NULL, then current module
 * 'field_ref' is the field reference, name, name of grp, subscripts etc
 * 'buflen' is length of buf
 * 'buf' receives the contents of the field
 *
 * return is ZERO, if all ok, else an error status
 */
int
cob_get_field_value (char *mod_name, char *field_ref, int buflen, char *buf)
{
	cob_module	*mod;
	int			sts;
	cob_field	f;
	*buf = 0;
	if ( (sts=cob_setup_field (mod_name, buflen, buf, &mod)) != 0)
		return sts;

	sts = cob_parse_field (mod, &f, field_ref);
	if (sts) {
		snprintf (buf,(size_t)buflen,"%s is undefined",field_ref);
		return sts;
	}
	cob_get_field_str (&f, buf, (size_t)buflen);
	return 0;
}

/* 
 * Using cob_symbols table, parse and set the field value
 * 'mod_name' is the module name, if NULL, then current module
 * 'field_ref' is the field reference, name, name of grp, subscripts etc
 * 'buflen' is maximum length of 'buf'
 * 'buf' holds the new value as character data 
 *
 * return is ZERO, if all ok, else an error status
 */
int
cob_put_field_value (char *mod_name, char *field_ref, int buflen, char *buf)
{
	cob_module	*mod;
	int			sts;
	cob_field	f;
	if ( (sts = cob_setup_field (mod_name, buflen, buf, &mod)) != 0)
		return sts;

	sts = cob_parse_field (mod, &f, field_ref);
	if (sts) {
		snprintf(buf,(size_t)buflen,"%s is undefined",field_ref);
		return sts;
	}
	cob_put_field_str (&f, buf);
	return 0;
}

/* 
 * Using cob_symbols table, parse and set the field value to watch for changes
 * 'mod_name' is the module name, if NULL, then current module
 * 'field_ref' is the field reference, name, name of grp, subscripts etc
 *
 * return is ZERO, if all ok, else an error status
 */
int
cob_watch_field_value (char *mod_name, char *field_ref)
{
	cob_module	*mod;
	int			sts;
	char		buf[256];
	cob_field	f;
	struct watch_list	*wl;
	if ( (sts = cob_setup_field (mod_name, (int)sizeof(buf), buf, &mod)) != 0)
		return sts;

	sts = cob_parse_field (mod, &f, field_ref);
	if (sts) {
		snprintf(buf,sizeof(buf),"%s is undefined",field_ref);
		return sts;
	}
	for (wl = head_watch; wl; wl = wl->next) {
		if (strcasecmp (field_ref, wl->field_ref) == 0
		 && wl->mod == mod) {	/* Already in list */
			memcpy (wl->saved, wl->f.data, wl->f.size);
			return 0;
		}
	}
	wl = cob_malloc (sizeof(struct watch_list));
	wl->field_ref = cob_strdup (field_ref);
	memcpy (&wl->f, &f, sizeof(cob_field));
	wl->saved = cob_malloc (f.size);
	memcpy (wl->saved, f.data, f.size);
	wl->next = head_watch;
	wl->mod = mod;
	head_watch = wl;
	return 0;
}

/* 
 * Using cob_symbols table, parse and remove the field from watch list
 * 'mod_name' is the module name, if NULL, then current module
 * 'field_ref' is the field reference, name, name of grp, subscripts etc
 *
 * return is ZERO, if all ok, else an error status
 */
int
cob_watch_field_free (char *mod_name, char *field_ref)
{
	cob_module	*mod;
	char		buf[256];
	cob_field	f;
	struct watch_list	*wl, *wp;
	if ( cob_setup_field (mod_name, (int)sizeof(buf), buf, &mod))
		return 1;

	if (cob_parse_field (mod, &f, field_ref)) 
		return 1;
	for (wp = wl = head_watch; wl; wl = wl->next) {
		if (strcasecmp (field_ref, wl->field_ref) == 0
		 && wl->mod == mod) {	/* In list, remove and free it */
			if (wl == head_watch)
				head_watch = wl->next;
			else
				wp->next = wl->next;
			cob_free (wl->field_ref);
			cob_free (wl->saved);
			cob_free (wl);
			return 0;
		}
		wp = wl;
	}
	return 0;
}

/* 
 * Scan the watch list looking for a field that has changed
 * 'mod_name' gets the module name
 * 'field_ref' gets the field reference
 *
 * return is ZERO, if nothing changed, else return 1 and update mod_name & field_ref
 */
int
cob_watch_check (char *mod_name, char *field_ref)
{
	struct watch_list	*wl;
	for (wl = head_watch; wl; wl = wl->next) {
		if (memcmp (wl->saved, wl->f.data, wl->f.size) != 0) {
			memcpy (wl->saved, wl->f.data, wl->f.size);
			strcpy(mod_name, wl->mod->module_name);
			strcpy(field_ref, wl->field_ref);
			return 1;
		}
	}
	return 0;
}

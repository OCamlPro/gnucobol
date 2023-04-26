/*
   Copyright (C) 2002-2014, 2016-2020, 2022-2023 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Edward Hart, Simon Sobisch

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

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>

/* include internal and external libcob definitions, forcing exports */
#define	COB_LIB_EXPIMP
#include "coblocal.h"

enum inspect_type {
	INSPECT_ALL		= 0,
	INSPECT_LEADING = 1,
	INSPECT_FIRST	= 2,
	INSPECT_TRAILING	= 3	
};
#define DLM_DEFAULT_NUM		8U

struct dlm_struct {
	cob_field	uns_dlm;
	cob_u32_t	uns_all;
};

/* Local variables */

static cob_global		*cobglobptr = NULL;

static const cob_field_attr	const_alpha_attr =
				{COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
static const cob_field_attr	const_strall_attr =
				{COB_TYPE_ALPHANUMERIC_ALL, 0, 0, 0, NULL};

static cob_field		*inspect_var;
static unsigned char		*inspect_data;
static unsigned char		*inspect_start;
static unsigned char		*inspect_end;
static unsigned char		*inspect_mark;	/* buffer to marker only: 0/1 */
static size_t			inspect_mark_size;	/* size of internal marker elements, increased up to
											   the maximum needed (biggest target field size) */
static size_t			inspect_mark_min;	/* min. position of the marker set by the last initialize */
static size_t			inspect_mark_max;	/* max. position of the marker set by the last initialize */
static unsigned char		*inspect_repdata;	/* contains data for REPLACING which is applied at end */
static size_t			inspect_repdata_size;	/* size of internal repdata buffer, increased up to
											   the maximum needed (biggest target field size) */
static size_t			inspect_size;
static cob_u32_t		inspect_replacing;	/* marker about current operation being INSPECT REPLACING */
static int			inspect_sign;
static cob_field		inspect_var_copy;

static cob_field		*string_dst;
static cob_field		*string_ptr;
static cob_field		*string_dlm;
static cob_field		string_dst_copy;
static cob_field		string_ptr_copy;
static cob_field		string_dlm_copy;
static int			string_offset;

static struct dlm_struct	*dlm_list;
static cob_field		*unstring_src;
static cob_field		*unstring_ptr;
static size_t			dlm_list_size;		/* size of internal delimiter elements, increased up to
											   the maximum needed (amount of DELIMITED BY),
											   actual size of dlm_list is calculated by
											   dlm_list_size * sizeof(dlm_struct) */
static cob_field		unstring_src_copy;
static cob_field		unstring_ptr_copy;
static int			unstring_offset;
static int			unstring_count;
static int			unstring_ndlms;

static unsigned char		*figurative_ptr;
static size_t			figurative_size;

static cob_field		alpha_fld;
static cob_field		str_cob_low;

/* Local functions */

static void
cob_str_memcpy (cob_field *dst, unsigned char *src, const int size)
{
	cob_field	temp;

	temp.size = size;
	temp.data = src;
	temp.attr = &const_alpha_attr;
	cob_move (&temp, dst);
}

static void
alloc_figurative (const cob_field *f1, const cob_field *f2)
{

	unsigned char		*s;
	size_t			size1;
	size_t			size2;
	size_t			n;

	size2 = f2->size;
	if (size2 > figurative_size) {
		if (figurative_ptr) {
			cob_free (figurative_ptr);
		}
		figurative_ptr = cob_malloc (size2);
		figurative_size = size2;
	}
	size1 = 0;
	s = figurative_ptr;
	for (n = 0; n < size2; ++n, ++s) {
		*s = f1->data[size1];
		size1++;
		if (size1 >= f1->size) {
			size1 = 0;
		}
	}
	alpha_fld.size = size2;
	alpha_fld.data = figurative_ptr;
}

/* (re-)allocates the used replace buffer as necessary
   This is a must-have for REPLACING as the original data may not be
   changed to correctly handle multiple replacements with BEFORE/AFTER
   clauses */
static COB_INLINE COB_A_INLINE void
setup_repdata (void)
{
	/* implementation note:
	   A version that memcpy'd the complete inspect_data to inspect_repdata
	   on first use, then memcpy back in cob_inspect_finish was tested but
	   dropped. While it has the benefit that memory breakpoints in the COBOL
	   data are only triggered once and always shows the result-to-be and
	   uses an optimized memcpy instead of a manual loop it involves much more
	   memory operations than commonly necessary - because normally only a small
	   percentage of the data is actually replaced.
	   A version that used inspect_repdata for CONVERTING was also dropped as
	   we don't need the additional memory there. */
	if (inspect_size > inspect_repdata_size) {
		if (inspect_repdata) {
			cob_free (inspect_repdata);
			inspect_repdata_size = inspect_size;
		} else if (inspect_size < COB_NORMAL_BUFF) {
			inspect_repdata_size = COB_NORMAL_BUFF;
		} else {
			inspect_repdata_size = inspect_size;
		}
		/* data content does not matter as we only used marked positions at end */
		inspect_repdata = cob_fast_malloc (inspect_repdata_size + 1);
	}
}

static COB_INLINE COB_A_INLINE unsigned char *
inspect_find_data (const cob_field *str)
{
	const unsigned char *data = str->data;
	const size_t	len = str->size;

	register unsigned char *p = inspect_start;
	unsigned char *const end_p = inspect_end - len + 1;

	if (p > end_p) {
		return NULL;
	}

	while (p != end_p) {
		if (memcmp (p, data, len) == 0) {
			return p;
		}
		p++;
	}
	return NULL;
}

static COB_INLINE COB_A_INLINE void
set_inspect_mark (const size_t pos, const size_t length)
{
	const size_t pos_end = pos + length - 1;
	memset (inspect_mark + pos, 1, length);
	if ((inspect_mark_min == 0 && inspect_mark[inspect_mark_min] == 0)
	 || pos < inspect_mark_min) {
		inspect_mark_min = pos;
	}
	if (pos_end > inspect_mark_max) {
		inspect_mark_max = pos_end;
	}
}

/* check for an area in the marker to be non-zero */
static COB_INLINE COB_A_INLINE int
is_marked (size_t pos, size_t length)
{
	/* no need to check further if there's no mark or no possible overlap ... */
	if (inspect_mark[inspect_mark_min] == 0
	 || inspect_mark_max <  pos
	 || inspect_mark_min >= pos + length) {
		return 0;
	}
	/* ... or if the minimal/max mark are within the range to check */
	if (inspect_mark_min >= pos
	 || inspect_mark_max < pos + length) {
		return 1;
	}

	/* we have a possible overlap - check if that's also for real */
	{
		register size_t i;
		
		for (i = 0; i < length; ++i) {
			if (inspect_mark[pos + i] != 0) {
				return 1;
			}
		}
	}
	return 0;
}

static void
inspect_common_no_replace (cob_field *f1, cob_field *f2,
	const enum inspect_type type, const size_t pos, const size_t inspect_len)
{
	register size_t		i;
	int		n = 0;

	if (type == INSPECT_TRAILING) {
		const size_t	i_max = inspect_len - f2->size; /* no + 1 here */
		size_t	first_marker = 0;
		for (i = i_max; ; --i) {
			/* Find matching substring */
			if (memcmp (i + inspect_start, f2->data, f2->size) == 0) {
				/* when not marked yet: count, mark and skip handled positions */
				if (!is_marked (pos + i, f2->size)) {
					n++;
					first_marker = i;
					i -= f2->size - 1;
				}
				if (i == 0) {
					break;
				}
			} else {
				break;
			}
		}
		/* set the marker so we won't iterate over this area again */
		if (n) {
			set_inspect_mark (pos + first_marker, inspect_len - first_marker);
		}
	} else if (type == INSPECT_LEADING) {
		const size_t	i_max = inspect_len - f2->size + 1;
		size_t	last_marker = 0;
		for (i = 0; i < i_max; ++i) {
			/* Find matching substring */
			if (memcmp (i + inspect_start, f2->data, f2->size) == 0) {
				/* when not marked yet: count, skip handled positions and set mark pos */
				if (!is_marked (pos + i, f2->size)) {
					n++;
					i += f2->size - 1;
					last_marker = i;
				}
			} else {
				break;
			}
		}
		/* set the marker so we won't iterate over this area again */
		if (n) {
			set_inspect_mark (pos, last_marker);
		}
	/* note: same code as for LEADING, moved out as we don't need to check
	   LEADING for _every_ byte in that tight loop */
	} else {
		const size_t	i_max = inspect_len - f2->size + 1;
		for (i = 0; i < i_max; ++i) {
			/* Find matching substring */
			if (memcmp (i + inspect_start, f2->data, f2->size) == 0) {
				const size_t checked_pos = pos + i;
				/* when not marked yet: count, mark and skip handled positions */
				if (!is_marked (checked_pos, f2->size)) {
					n++;
					/* set the marker so we won't iterate over this area again */
					set_inspect_mark (checked_pos, f2->size);
					if (type == INSPECT_FIRST) {
						break;
					}
					i += f2->size - 1;
				}
			}
		}
	}

	if (n != 0) {
		cob_add_int (f1, n, 0);
	}
}

static COB_INLINE COB_A_INLINE int
do_mark (const size_t pos, const size_t length, unsigned char *replace_data)
{
	if (is_marked (pos, length)) {
		return 0;	/* it is, nothing to do here */
	}
	/* nothing done there yet, so: */

	/* 1 - handle possible replacing */
	setup_repdata ();
	memcpy (inspect_repdata + pos, replace_data, length);

	/* 2 - set the marker so we won't iterate over this area again */
	set_inspect_mark (pos, length);

	/* 3 - let the caller handle pos adjustment */
	return 1;
}

static void
inspect_common_replacing (cob_field *f1, cob_field *f2,
	const enum inspect_type type, const size_t pos, const size_t inspect_len)
{
	register size_t		i;

	if (type == INSPECT_TRAILING) {
		const size_t	i_max = inspect_len - f2->size; /* no + 1 here */
		for (i = i_max; ; --i) {
			/* Find matching substring */
			if (memcmp (i + inspect_start, f2->data, f2->size) == 0) {
				/* when not marked yet: count, mark and skip handled positions */
				if (do_mark (pos + i, f2->size, f1->data)) {
					i -= f2->size - 1;
				}
				if (i == 0) {
					break;
				}
			} else {
				break;
			}
		}
	} else if (type == INSPECT_LEADING) {
		const size_t	i_max = inspect_len - f2->size + 1;
		for (i = 0; i < i_max; ++i) {
			/* Find matching substring */
			if (memcmp (i + inspect_start, f2->data, f2->size) == 0) {
				/* when not marked yet: count, mark and skip handled positions */
				if (do_mark (pos + i, f2->size, f1->data)) {
					i += f2->size - 1;
				}
			} else {
				break;
			}
		}
	/* note: same code as for LEADING, moved out as we don't need to check
	   LEADING for _every_ byte in that tight loop */
	} else {
		const size_t	i_max = inspect_len - f2->size + 1;
		for (i = 0; i < i_max; ++i) {
			/* Find matching substring */
			if (memcmp (i + inspect_start, f2->data, f2->size) == 0) {
				/* when not marked yet: count, mark and skip handled positions */
				if (do_mark (pos + i, f2->size, f1->data)) {
					if (type == INSPECT_FIRST) {
						break;
					}
					i += f2->size - 1;
				}
			}
		}
	}
}

static void
inspect_common (cob_field *f1, cob_field *f2, const enum inspect_type type)
{
	const size_t	pos = inspect_start - inspect_data;
	const size_t	inspect_len = inspect_end - inspect_start;

	if (inspect_len == 0) {
		/* inspecting either a zero-length field or
		   AFTER ... has not found a place to start the conversion */
		return;
	}

	if (unlikely (!f1)) {
		f1 = &str_cob_low;
	}
	if (unlikely (!f2)) {
		f2 = &str_cob_low;
	}

	/* note: inspect_common_no_replace and inspect_common_replacing share most
	   of its code; still moved out as this allows for further optimizations;
	   only optimization left: separate entry function and codegen for single
	   target as this does not need a marker at all */
	if (!inspect_replacing) {
		if (f2->size > inspect_len) {
			return;
		}
		inspect_common_no_replace (f1, f2, type, pos, inspect_len);
	} else {
		if (f1->size != f2->size) {
			if (COB_FIELD_TYPE (f1) == COB_TYPE_ALPHANUMERIC_ALL) {
				alloc_figurative (f1, f2);
				f1 = &alpha_fld;
			} else {
				cob_set_exception (COB_EC_RANGE_INSPECT_SIZE);
				return;
			}
		}
		if (f2->size > inspect_len) {
			return;
		}
		inspect_common_replacing (f1, f2, type, pos, inspect_len);
	}
}

/* Global functions */

/* INSPECT */
/* an INSPECT is split into multiple parts:
   one-time cob_inspect_init   (setting up memory and markers)
   multiple:
	cob_inspect_start          (setting inspect_start/end)
	cob_inspect_before         (optional, adjusting inspect_end)
	cob_inspect_after          (optional, adjusting inspect_start)
   one of:
	cob_inspect_characters/cob_inspect_converting (until 3.2)/cob_inspect_all/
	cob_inspect_leading/cob_inspect_trailing/cob_inspect_first
   one-time cob_inspect_finish (copying the REPLACING characters back) */

static COB_INLINE COB_A_INLINE void
cob_inspect_init_common (cob_field *var)
{
	if (COB_FIELD_HAVE_SIGN (var) && !COB_FIELD_SIGN_SEPARATE(var)) {
		/* it is allowed to TRANSFORM / INSPECT a numeric display signed element;
		   if it isn't stored separately we need to "remove" it here and add it back
		   in inspect_finish */
		inspect_var_copy = *var;
		inspect_var = &inspect_var_copy;
		inspect_sign = cob_real_get_sign (var);
	} else {
		inspect_var = NULL;
	}
	inspect_size = COB_FIELD_SIZE (var);
	inspect_data = COB_FIELD_DATA (var);
	inspect_start = NULL;
	inspect_end = NULL;

	cobglobptr->cob_exception_code = 0;
}
void
cob_inspect_init (cob_field *var, const cob_u32_t replacing)
{
	cob_inspect_init_common (var);
	inspect_replacing = replacing;

	if (inspect_size > inspect_mark_size) {
		if (inspect_mark) {
			cob_free (inspect_mark);
			inspect_mark_size = inspect_size;
		} else if (inspect_size < COB_NORMAL_BUFF) {
			inspect_mark_size = COB_NORMAL_BUFF;
		} else {
			inspect_mark_size = inspect_size;
		}
		/* initialize to zero */
		inspect_mark = cob_malloc (inspect_mark_size + 1);
	} else if (inspect_mark_size != 0 && inspect_mark[inspect_mark_min] != 0) {
		const size_t init_len = inspect_mark_max - inspect_mark_min + 1;
		memset (inspect_mark + inspect_mark_min, 0, init_len);
	}
	inspect_mark_min = inspect_mark_max = 0;
}

/* an INSPECT CONVERTING / TRANSFORM is split into multiple parts:
   one-time cob_inspect_init_converting
        --> cob_inspect_init_common    (setting up memory)
   multiple:
	cob_inspect_start       (setting inspect_start/end)
	cob_inspect_before        (optional, adjusting inspect_end)
	cob_inspect_after         (optional, adjusting inspect_start)
   one-time cob_inspect_converting (actual converstion) */

void
cob_inspect_init_converting (cob_field *var)
{
	cob_inspect_init_common (var);
	inspect_replacing = 0;	/* only set for pre 3.2 compat because of cob_inspect_finish */
}

void
cob_inspect_start (void)
{
	inspect_start = inspect_data;
	inspect_end = inspect_data + inspect_size;
}

void
cob_inspect_before (const cob_field *str)
{
	unsigned char *data_pos = inspect_find_data (str);
	if (data_pos) {
		inspect_end = data_pos;
	}
}

void
cob_inspect_after (const cob_field *str)
{
	unsigned char *data_pos = inspect_find_data (str);
	if (data_pos) {
		inspect_start = data_pos + str->size;
	} else {
		inspect_start = inspect_end;
	}
}

void
cob_inspect_characters (cob_field *f1)
{
	const size_t	pos = inspect_start - inspect_data;
	const size_t	inspect_len = inspect_end - inspect_start;
	const unsigned char	*mark_pos = inspect_mark + pos;
	const unsigned char * const mark_end = mark_pos + inspect_len;

	if (inspect_len == 0) {
		/* inspecting either a zero-length field or
		   AFTER ... has not found a place to start the conversion */
		return;
	}

	if (inspect_replacing) {
		/* INSPECT REPLACING CHARACTERS BY f1 */
		const unsigned char repl_by = *f1->data;
		unsigned char	*repdata;
		setup_repdata ();
		repdata = inspect_repdata + pos;
		if (is_marked (pos, inspect_len)) {
			/* at least a partial marking - so iterate */
			while (mark_pos != mark_end) {
				/* replace all positions in the original data where
				   we did not set a replacement for so far */
				if (*mark_pos++ == 0) {
					*repdata = repl_by;
				}
				repdata++;
			}
		} else {
			/* that area is "free to go", so memset */
			memset (repdata, repl_by, inspect_len);
		}
	} else {
		/* INSPECT TALLYING f1 CHARACTERS */
		if (is_marked (pos, inspect_len)) {
			/* at least a partial marking - so iterate */
			int	n = 0;
			/* Note: field->size and therefore INSPECT target's size are
			         guaranteed to be < INT_MAX */
			while (mark_pos != mark_end) {
				if (*mark_pos++ == 0) {
					n++;
				}
			}
			if (n > 0) {
				cob_add_int (f1, n, 0);
			}
		} else {
			/* common case: no markers in the length to check */
			cob_add_int (f1, (int)inspect_len, 0);
		}
	}
	set_inspect_mark (pos, inspect_len);
}

void
cob_inspect_all (cob_field *f1, cob_field *f2)
{
	inspect_common (f1, f2, INSPECT_ALL);
}

void
cob_inspect_leading (cob_field *f1, cob_field *f2)
{
	inspect_common (f1, f2, INSPECT_LEADING);
}

void
cob_inspect_first (cob_field *f1, cob_field *f2)
{
	inspect_common (f1, f2, INSPECT_FIRST);
}

void
cob_inspect_trailing (cob_field *f1, cob_field *f2)
{
	inspect_common (f1, f2, INSPECT_TRAILING);
}

void
cob_inspect_converting (const cob_field *f1, const cob_field *f2)
{
	const size_t	inspect_len = inspect_end - inspect_start;

	if (inspect_len == 0) {
		/* our task is to convert either a zero-length field or
		   AFTER ... has not found a place to start the conversion */
		return;
	}

	if (unlikely (!f1)) {
		f1 = &str_cob_low;
	}
	if (unlikely (!f2)) {
		f2 = &str_cob_low;
	}
	if (f1->size != f2->size) {
		if (COB_FIELD_TYPE (f2) == COB_TYPE_ALPHANUMERIC_ALL) {
			alloc_figurative (f2, f1);
			f2 = &alpha_fld;
		} else {
			cob_set_exception (COB_EC_RANGE_INSPECT_SIZE);
			return;
		}
	}

	/* test _all_ positions of the inspect target against
	   all entries of CONVERTING position by position */
	{
		unsigned char * cur_data = inspect_data + (inspect_start - inspect_data);
		unsigned char * const cur_data_end = cur_data + inspect_len;
		
#if 1 /* table-approach, _much faster_, _should_ be portable */
		char conv_tab[256] = { 0 };		/* using 256 to remove the need to use offset */
		char conv_set[256] = { 0 };
		
		/* pre-fill conversion table, skipping duplicates */
		{
			const unsigned char *conv_to   = f2->data;
			const unsigned char *conv_from = f1->data;
			const unsigned char * const conv_from_end = f1->data + f1->size;
			while (conv_from < conv_from_end) {
				if (conv_set[*conv_from] == 0) {
					conv_set[*conv_from] = 1;
					conv_tab[*conv_from] = *conv_to;
				}
				conv_from++, conv_to++;
			}
		}
		/* iterate over target converting with table */
		while (cur_data < cur_data_end) {
			if (conv_set[*cur_data]) {
				*cur_data = conv_tab[*cur_data];
			}
			cur_data++;
		}
#else
		const size_t	conv_len = f1->size;
		const unsigned char *conv_to = f2->data;
		const unsigned char *conv_from = f1->data;
		const unsigned char * const conv_from_end = conv_from + conv_len;

		while (cur_data < cur_data_end) {
			conv_to = f2->data;
			conv_from = f1->data;
			while (conv_from < conv_from_end) {
				if (*cur_data == *conv_from) {
					*cur_data = *conv_to;
					/* note: as we always have exactly 1 target 1 run,
					   there's no need to mark anything here,
					   done last with revision 4592 */
					break;
				}
				conv_from++, conv_to++;
			}
			cur_data++;
		}
#endif
	}

	/* note: copied here for 3.2+ as cob_inspect_finish is not generated
	         for TRANSFORM/INSPECT CONVERTING any more */
	if (inspect_var) {
		cob_real_put_sign (inspect_var, inspect_sign);
	}
}

void
cob_inspect_finish (void)
{
	/* Note: this is not called any more for TRANSFORM/INSPECT CONVERTING
	         since GnuCOBOL 3.2 codegen (only for "old modules")! */

	if (inspect_replacing
	 && inspect_repdata_size != 0	/* check for first INSPECT REPLACING having zero length */
	 && inspect_mark[inspect_mark_min] != 0) {
		/* copy over replace data from first to last changed position */
		size_t	i;
		for (i = inspect_mark_min; i <= inspect_mark_max; ++i) {
			if (inspect_mark[i] != 0) {
				inspect_data[i] = inspect_repdata[i];
			}
		}
#if 0	/* drop data copy because of security issues
		[may only be done upon request]; if not active and
		that contains sensitive data do an INSPECT against a field
		of the same size to overwrite the buffer */
		memset (inspect_repdata + inspect_mark_min, 0,
		        inspect_mark_max - inspect_mark_min + 1);
#endif
	}

	if (inspect_var) {
		cob_real_put_sign (inspect_var, inspect_sign);
	}
}

/* STRING */

void
cob_string_init (cob_field *dst, cob_field *ptr)
{
	string_dst_copy = *dst;
	string_dst = &string_dst_copy;
	string_ptr = NULL;
	if (ptr) {
		string_ptr_copy = *ptr;
		string_ptr = &string_ptr_copy;
	}
	string_offset = 0;
	cobglobptr->cob_exception_code = 0;

	if (string_ptr) {
		string_offset = cob_get_int (string_ptr) - 1;
		if (string_offset < 0
		 || string_offset >= (int)string_dst->size) {
			cob_set_exception (COB_EC_OVERFLOW_STRING);
		}
	}
}

void
cob_string_delimited (cob_field *dlm)
{
	string_dlm = NULL;
	if (dlm) {
		string_dlm_copy = *dlm;
		string_dlm = &string_dlm_copy;
	}
}

void
cob_string_append (cob_field *src)
{
	size_t	src_size;
	int	i;
	int	size;

	if (cobglobptr->cob_exception_code) {
		return;
	}

	src_size = src->size;
	if (!src_size) {
		return;
	}
	if (string_dlm) {
		size = (int)(src_size - string_dlm->size + 1);
		for (i = 0; i < size; ++i) {
			if (memcmp (src->data + i, string_dlm->data,
				    string_dlm->size) == 0) {
				src_size = i;
				break;
			}
		}
	}

	if (src_size <= string_dst->size - string_offset) {
		memcpy (string_dst->data + string_offset, src->data, src_size);
		string_offset += (int) src_size;
	} else {
		size = (int)(string_dst->size - string_offset);
		memcpy (string_dst->data + string_offset, src->data, (size_t)size);
		string_offset += size;
		cob_set_exception (COB_EC_OVERFLOW_STRING);
	}
}

void
cob_string_finish (void)
{
	if (string_ptr) {
		cob_set_int (string_ptr, string_offset + 1);
	}
}

/* UNSTRING */
/* an UNSTRING is split into multiple parts:
   one-time cob_unstring_init  (setting up memory and static variables)
   0..n :
    cob_unstring_delimited  (setting delimiter struct entries)
   1..n:
	cob_unstring_into       (to handle a single target
	                         [optional with counter and/or delimiter])
   optional:
     cob_unstring_tallying  setting TALLYING (amount of targets set)
   one-time cob_unstring_finish (setting the string pointer / overflow exception) */

void
cob_unstring_init (cob_field *src, cob_field *ptr, const size_t num_dlm)
{
	unstring_src_copy = *src;
	unstring_src = &unstring_src_copy;
	unstring_ptr = NULL;
	if (ptr) {
		unstring_ptr_copy = *ptr;
		unstring_ptr = &unstring_ptr_copy;
	}

	unstring_offset = 0;
	unstring_count = 0;
	unstring_ndlms = 0;
	cobglobptr->cob_exception_code = 0;
	if (num_dlm > dlm_list_size) {
		if (dlm_list) {
			cob_free (dlm_list);
			dlm_list_size = num_dlm;
		} else if (num_dlm < DLM_DEFAULT_NUM) {
			dlm_list_size = DLM_DEFAULT_NUM;
		} else {
			dlm_list_size = num_dlm;
		}
		dlm_list = cob_malloc (dlm_list_size * sizeof(struct dlm_struct));
	}

	if (unstring_ptr) {
		unstring_offset = cob_get_int (unstring_ptr) - 1;
		if (unstring_offset < 0 || unstring_offset >= (int)unstring_src->size) {
			cob_set_exception (COB_EC_OVERFLOW_UNSTRING);
		}
	}
}

void
cob_unstring_delimited (cob_field *dlm, const cob_u32_t all)
{
	dlm_list[unstring_ndlms].uns_dlm = *dlm;
	dlm_list[unstring_ndlms].uns_all = all;
	unstring_ndlms++;
}

void
cob_unstring_into (cob_field *dst, cob_field *dlm, cob_field *cnt)
{
	unsigned char	*dlm_data;
	unsigned char	*start;
	size_t		dlm_size = 0;
	int		match_size = 0;

	if (cobglobptr->cob_exception_code) {
		/* commonly COB_EC_OVERFLOW_UNSTRING: the specified WITH POINTER was
		   too big, all other functions  must be returned early;
		   TODO: adjust cobc to only call if cob_unstring_init was
		         sucessfull / has no exception */
		return;
	}

	if (unstring_offset >= (int)unstring_src->size) {
		/* overflow from the last iteration (multiple INTO targets) */
		return;
	}

	dlm_data = NULL;
	start = unstring_src->data + unstring_offset;

	/* no delimiter - just split into DELIMITED BY SIZE */
	if (unstring_ndlms == 0) {
		 /* necessary for correct unstring offset: minimal size */
		/* Note: field->size and therefore offset
		   are guaranteed to be < INT_MAX by cobc */
		match_size = cob_min_int ((int)COB_FIELD_SIZE (dst),
					  (int)unstring_src->size - unstring_offset);
		cob_str_memcpy (dst, start, match_size);
		unstring_offset += match_size;

	/* DELIMITED BY [ALL] x [.. OR [ALL] z] */
	} else {

		const int	srsize = (int)unstring_src->size;
		unsigned char	*p;
		unsigned char	*dp;
		int		found = 0;

		/* note: duplicate code for performance as most cases
		         have either none or a single delimiter */
		if (unstring_ndlms == 1) {
			const struct dlm_struct dlms = dlm_list[0];
			const int     dlsize = (int) dlms.uns_dlm.size;
			const unsigned char *s = unstring_src->data + srsize - dlsize + 1;
			dp = dlms.uns_dlm.data;

			for (p = start; p < s; ++p) {
				if (!memcmp (p, dp, (size_t)dlsize)) {         /* delimiter matches */
					match_size = (int)(p - start);             /* count in */
					cob_str_memcpy (dst, start, match_size);   /* into */
					unstring_offset += match_size + dlsize;    /* with pointer */
					dlm_data = dp;
					dlm_size = dlsize;
					if (dlms.uns_all) {                     /* delimited by all */
						for (p += dlsize; p < s; p += dlsize) {
							if (memcmp (p, dp, (size_t)dlsize)) {
								break;
							}
							unstring_offset += dlsize;
						}
					}
					found = 1;
					break;
				}
			}
		} else {
			const unsigned char *s = unstring_src->data + srsize;
			int		i;
			for (p = start; p < s; ++p) {
				for (i = 0; i < unstring_ndlms; ++i) {
					const struct dlm_struct dlms = dlm_list[i];
					const int     dlsize = (int)dlms.uns_dlm.size;
					const unsigned char *s2 = s - dlsize + 1;
					if (p > s2) {
						continue;
					}
					dp = dlms.uns_dlm.data;
					if (!memcmp (p, dp, (size_t)dlsize)) {         /* delimiter matches */
						match_size = (int)(p - start);             /* count in */
						cob_str_memcpy (dst, start, match_size);   /* into */
						unstring_offset += match_size + dlsize;    /* with pointer */
						dlm_data = dp;
						dlm_size = dlsize;
						if (dlms.uns_all) {                     /* delimited by all */
							for (p += dlsize; p < s2; p += dlsize) {
								if (memcmp (p, dp, (size_t)dlsize)) {
									break;
								}
								unstring_offset += dlsize;
							}
						}
						found = 1;
						break;
					}
				}
				if (found) {
					break;
				}
			}
		}

		/* if none of the delimiters matched, match to end */
		if (!found) {
			match_size = (int)(unstring_src->size - unstring_offset);
			cob_str_memcpy (dst, start, match_size);
			unstring_offset = (int) unstring_src->size;
		}
	}
	unstring_count++;

	/* note: per any known dialect both DELIMITER IN and COUNT IN are only
	         allowed if there is a DELIMITED BY phrase; the GnuCOBOL parser
			 does allow this (did so since the first implementation) */

	/* set DELIMITER IN */
	if (dlm) {
		if (dlm_data) {
			cob_str_memcpy (dlm, dlm_data, (int) dlm_size);
		} else if (COB_FIELD_IS_NUMERIC (dlm)) {
			cob_set_int (dlm, 0);
		} else {
			memset (dlm->data, ' ', dlm->size);
		}
	}

	/* set COUNT IN */
	if (cnt) {
		cob_set_int (cnt, match_size);
	}
}

void
cob_unstring_tallying (cob_field *f)
{
	cob_add_int (f, unstring_count, 0);
}

void
cob_unstring_finish (void)
{
	if (unstring_offset < (int)unstring_src->size) {
		/* overflow from any iteration -> overflow exception */
		cob_set_exception (COB_EC_OVERFLOW_UNSTRING);
	}

	if (unstring_ptr) {
		cob_set_int (unstring_ptr, unstring_offset + 1);
	}
}

/* Initialization/Termination */

void
cob_exit_strings (void)
{
	if (inspect_mark) {
		cob_free (inspect_mark);
		inspect_mark = NULL;
	}
	inspect_mark_size = inspect_mark_min = inspect_mark_max = 0;
	if (inspect_repdata) {
		cob_free (inspect_repdata);
		inspect_repdata = NULL;
	}
	inspect_repdata_size = 0;

	if (dlm_list) {
		cob_free (dlm_list);
		dlm_list = NULL;
	}
	dlm_list_size = 0;

	if (figurative_ptr) {
		cob_free (figurative_ptr);
		figurative_ptr = NULL;
	}
	figurative_size = 0;
}

void
cob_init_strings (cob_global *lptr)
{
	cobglobptr = lptr;

	figurative_ptr = NULL;
	figurative_size = 0;

	alpha_fld.size = 0;
	alpha_fld.data = NULL;
	alpha_fld.attr = &const_alpha_attr;

	str_cob_low.size = 1;
	str_cob_low.data = (cob_u8_ptr)"\0";
	str_cob_low.attr = &const_strall_attr;
}

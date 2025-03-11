/*
   Copyright (C) 2002-2014, 2016-2020, 2022-2024 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Edward Hart, Simon Sobisch, Boris
   Eng

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
#include <sys/types.h>

/* include internal and external libcob definitions, forcing exports */
#define	COB_LIB_EXPIMP
#include "coblocal.h"

enum inspect_type {
	INSPECT_UNSET = 0,
	INSPECT_ALL,
	INSPECT_LEADING,
	INSPECT_FIRST,
	INSPECT_TRAILING
};
#define DLM_DEFAULT_NUM		8U

struct dlm_struct {
	cob_field	uns_dlm;
	cob_u32_t	uns_all;
};

struct cob_inspect_state {
	cob_field	    	  *var;
	unsigned char		  *data;
	unsigned char		  *start;
	unsigned char		  *end;
	unsigned char		  *mark;	/* buffer to marker only: 0/1 */
	size_t		    	  mark_size;	/* size of internal marker elements, increased up to
                                 the maximum needed (biggest target field size) */
	size_t			      mark_min;	/* min. position of the marker set by the last initialize */
	size_t			      mark_max;	/* max. position of the marker set by the last initialize */
	unsigned char		  *repdata;	/* contains data for REPLACING which is applied at end */
	size_t			      repdata_size;	/* size of internal repdata buffer, increased up to
                                   the maximum needed (biggest target field size) */
	size_t			      size;
	cob_u32_t		      replacing;	/* marker about current operation being INSPECT REPLACING */
	int			          sign;
	cob_field			  var_copy;	/* use a copy to account for overwritten temporary field, see cob_inspect_init_common_intern */
	enum inspect_type type;
};

struct cob_string_state {
	cob_field		*dst;
	cob_field		*ptr;
	cob_field		*dlm;
	cob_field		dst_copy;	/* use a copy to account for overwritten temporary field, see cob_string_init_intern */
	cob_field		ptr_copy;	/* use a copy to account for overwritten temporary field, see cob_string_init_intern */
	cob_field		dlm_copy;	/* use a copy to account for overwritten temporary field, see cob_string_delimited_intern */
	int		    	offset;	/* value associated with WITH POINTER clauses */
};

struct cob_unstring_state {
	struct dlm_struct		*dlm_list;
	cob_field           *src;
	cob_field           *ptr;
	size_t              dlm_list_size;	/* size of internal delimiter elements, increased up to
                                         the maximum needed (amount of DELIMITED BY),
                                         actual size of dlm_list is calculated by
                                         dlm_list_size * sizeof(dlm_struct) */
	cob_field	    src_copy;	/* use a copy to account for overwritten temporary field, see cob_unstring_init_intern */
	cob_field	    ptr_copy;	/* use a copy to account for overwritten temporary field, see cob_unstring_init_intern */
	int                 offset;
	unsigned int        count;
	unsigned int        ndlms;
};

/* Local variables */

static cob_global		*cobglobptr = NULL;

static const cob_field_attr	const_alpha_attr =
				{COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
static const cob_field_attr	const_strall_attr =
				{COB_TYPE_ALPHANUMERIC_ALL, 0, 0, 0, NULL};

COB_TLS struct cob_inspect_state	share_inspect_state;
COB_TLS struct cob_string_state		share_string_state;
COB_TLS struct cob_unstring_state	share_unstring_state;

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
	const size_t		size2 = f2->size;

#if 1	/* size1 is always 1 here, so several optimizations possible */
	if (*f1->data == ' ' && size2 <= COB_SPACES_ALPHABETIC_BYTE_LENGTH) {
		alpha_fld.size = size2;
		alpha_fld.data = (unsigned char *) COB_SPACES_ALPHABETIC;
		return;
	}
	if (*f1->data == '0' && size2 <= COB_ZEROES_ALPHABETIC_BYTE_LENGTH) {
		alpha_fld.size = size2;
		alpha_fld.data = (unsigned char *) COB_ZEROES_ALPHABETIC;
		return;
	}

	if (size2 > figurative_size) {
		if (figurative_ptr) {
			cob_free (figurative_ptr);
		}
		figurative_ptr = cob_malloc (size2);
		figurative_size = size2;
	}

	memset (figurative_ptr, *f1->data, size2);
#else
	if (size2 > figurative_size) {
		if (figurative_ptr) {
			cob_free (figurative_ptr);
		}
		figurative_ptr = cob_malloc (size2);
		figurative_size = size2;
	}

	{
		unsigned char	*s = figurative_ptr;
		size_t		n = size2;
		size_t		size1 = 0;
		while (n != 0) {
			if (size1 >= f1->size) {
				size1 = 0;
			}
			*s++ = f1->data[size1++];
			--n;
		}
	}
#endif

	alpha_fld.size = size2;
	alpha_fld.data = figurative_ptr;
}

/* (re-)allocates the used replace buffer as necessary
   This is a must-have for REPLACING as the original data may not be
   changed to correctly handle multiple replacements with BEFORE/AFTER
   clauses */
static COB_INLINE COB_A_INLINE void
setup_repdata (struct cob_inspect_state *st)
{
	/* implementation note:
	   A version that memcpy'd the complete data to repdata
	   on first use, then memcpy back in cob_inspect_finish was tested but
	   dropped. While it has the benefit that memory breakpoints in the COBOL
	   data are only triggered once and always shows the result-to-be and
	   uses an optimized memcpy instead of a manual loop it involves much more
	   memory operations than commonly necessary - because normally only a small
	   percentage of the data is actually replaced.
	   A version that used repdata for CONVERTING was also dropped as
	   we don't need the additional memory there. */
	if (st->size > st->repdata_size) {
		if (st->repdata) {
			cob_free (st->repdata);
			st->repdata_size = st->size;
		} else if (st->size < COB_NORMAL_BUFF) {
			st->repdata_size = COB_NORMAL_BUFF;
		} else {
			st->repdata_size = st->size;
		}
		/* data content does not matter as we only used marked positions at end */
		st->repdata = cob_fast_malloc (st->repdata_size + 1);
	}
}

static COB_INLINE COB_A_INLINE unsigned char *
inspect_find_data (struct cob_inspect_state *st, const cob_field *str)
{
	const unsigned char *data = str->data;
	const size_t	len = str->size;

	register unsigned char *p = st->start;
	unsigned char *const end_p = st->end - len + 1;

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
set_inspect_mark (
	struct cob_inspect_state *st,
	const size_t pos,
	const size_t length
)
{
	const size_t pos_end = pos + length - 1;
	memset (st->mark + pos, 1, length);
	if ((st->mark_min == 0 && st->mark[st->mark_min] == 0)
	 || pos < st->mark_min) {
		st->mark_min = pos;
	}
	if (pos_end > st->mark_max) {
		st->mark_max = pos_end;
	}
}

/* check for an area in the marker to be non-zero */
static COB_INLINE COB_A_INLINE int
is_marked (struct cob_inspect_state *st, size_t pos, size_t length)
{
	/* no need to check further if there's no mark or no possible overlap ... */
	if (st->mark[st->mark_min] == 0
	 || st->mark_max <  pos
	 || st->mark_min >= pos + length) {
		return 0;
	}
	/* ... or if the minimal/max mark are within the range to check */
	if (st->mark_min >= pos
	 || st->mark_max < pos + length) {
		return 1;
	}

	/* we have a possible overlap - check if that's also for real */
	{
		register size_t i;

		for (i = 0; i < length; ++i) {
			if (st->mark[pos + i] != 0) {
				return 1;
			}
		}
	}
	return 0;
}

static void
inspect_common_no_replace (
	struct cob_inspect_state *st,
	cob_field *f1,
	cob_field *f2,
	const size_t pos,
	const size_t len
)
{
	register size_t		i;
	int		n = 0;

	if (st->type == INSPECT_TRAILING) {
		const size_t	i_max = len - f2->size; /* no + 1 here */
		size_t	first_marker = 0;
		for (i = i_max; ; --i) {
			/* Find matching substring */
			if (memcmp (i + st->start, f2->data, f2->size) == 0) {
				/* when not marked yet: count, mark and skip handled positions */
				if (!is_marked (st, pos + i, f2->size)) {
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
			set_inspect_mark (st, pos + first_marker, len - first_marker);
		}
	} else if (st->type == INSPECT_LEADING) {
		const size_t	i_max = len - f2->size + 1;
		size_t	last_marker = 0;
		for (i = 0; i < i_max; ++i) {
			/* Find matching substring */
			if (memcmp (i + st->start, f2->data, f2->size) == 0) {
				/* when not marked yet: count, skip handled positions and set mark pos */
				if (!is_marked (st, pos + i, f2->size)) {
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
			set_inspect_mark (st, pos, last_marker);
		}
	/* note: same code as for LEADING, moved out as we don't need to check
	   LEADING for _every_ byte in that tight loop */
	} else {
		const size_t	i_max = len - f2->size + 1;
		for (i = 0; i < i_max; ++i) {
			/* Find matching substring */
			if (memcmp (i + st->start, f2->data, f2->size) == 0) {
				const size_t checked_pos = pos + i;
				/* when not marked yet: count, mark and skip handled positions */
				if (!is_marked (st, checked_pos, f2->size)) {
					n++;
					/* set the marker so we won't iterate over this area again */
					set_inspect_mark (st, checked_pos, f2->size);
					if (st->type == INSPECT_FIRST) {
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
do_mark (
	struct cob_inspect_state *st,
	const size_t pos,
	const size_t length,
	unsigned char *replace_data
)
{
	if (is_marked (st, pos, length)) {
		return 0;	/* it is, nothing to do here */
	}
	/* nothing done there yet, so: */

	/* 1 - handle possible replacing */
	setup_repdata (st);
	memcpy (st->repdata + pos, replace_data, length);

	/* 2 - set the marker so we won't iterate over this area again */
	set_inspect_mark (st, pos, length);

	/* 3 - let the caller handle pos adjustment */
	return 1;
}

static void
inspect_common_replacing (
	struct cob_inspect_state *st,
	cob_field *f1,
	cob_field *f2,
	const size_t pos,
	const size_t len
)
{
	register size_t		i;

	if (st->type == INSPECT_TRAILING) {
		const size_t	i_max = len - f2->size; /* no + 1 here */
		for (i = i_max; ; --i) {
			/* Find matching substring */
			if (memcmp (i + st->start, f2->data, f2->size) == 0) {
				/* when not marked yet: count, mark and skip handled positions */
				if (do_mark (st, pos + i, f2->size, f1->data)) {
					i -= f2->size - 1;
				}
				if (i == 0) {
					break;
				}
			} else {
				break;
			}
		}
	} else if (st->type == INSPECT_LEADING) {
		const size_t	i_max = len - f2->size + 1;
		for (i = 0; i < i_max; ++i) {
			/* Find matching substring */
			if (memcmp (i + st->start, f2->data, f2->size) == 0) {
				/* when not marked yet: count, mark and skip handled positions */
				if (do_mark (st, pos + i, f2->size, f1->data)) {
					i += f2->size - 1;
				}
			} else {
				break;
			}
		}
	/* note: same code as for LEADING, moved out as we don't need to check
	   LEADING for _every_ byte in that tight loop */
	} else {
		const size_t	i_max = len - f2->size + 1;
		for (i = 0; i < i_max; ++i) {
			/* Find matching substring */
			if (memcmp (i + st->start, f2->data, f2->size) == 0) {
				/* when not marked yet: count, mark and skip handled positions */
				if (do_mark (st, pos + i, f2->size, f1->data)) {
					if (st->type == INSPECT_FIRST) {
						break;
					}
					i += f2->size - 1;
				}
			}
		}
	}
}

static void
inspect_common (
	struct cob_inspect_state *st,
	cob_field *f1,
	cob_field *f2
)
{
	const size_t	pos = st->start - st->data;
	const size_t	len = st->end - st->start;

	if (len == 0) {
		/* inspecting either a zero-length field or
		   AFTER ... has not found a place to start the conversion */
		return;
	}

	if (!f1) {
		f1 = &str_cob_low;
	}
	if (!f2) {
		f2 = &str_cob_low;
	}

	/* note: inspect_common_no_replace and inspect_common_replacing share most
	   of its code; still moved out as this allows for further optimizations;
	   only optimization left: separate entry function and codegen for single
	   target as this does not need a marker at all */
	if (!st->replacing) {
		if (f2->size > len) {
			return;
		}
		inspect_common_no_replace (st, f1, f2, pos, len);
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
		if (f2->size > len) {
			return;
		}
		inspect_common_replacing (st, f1, f2, pos, len);
	}
}

/* Global functions */

/* INSPECT */
/* an INSPECT is split into multiple parts:
   one-time cob_inspect_init   (setting up memory and markers)
   multiple:
	cob_inspect_start          (setting start/end)
	cob_inspect_before         (optional, adjusting end)
	cob_inspect_after          (optional, adjusting start)
   one of:
	cob_inspect_characters/cob_inspect_converting (until 3.2)/cob_inspect_all/
	cob_inspect_leading/cob_inspect_trailing/cob_inspect_first
   one-time cob_inspect_finish (copying the REPLACING characters back) */

static COB_INLINE COB_A_INLINE void
cob_inspect_init_common_intern (struct cob_inspect_state *st, cob_field *var)
{
	if (COB_FIELD_HAVE_SIGN (var) && !COB_FIELD_SIGN_SEPARATE(var)) {
		/* it is allowed to TRANSFORM / INSPECT a numeric display signed element;
		   if it isn't stored separately we need to "remove" it here and add it back
		   in inspect_finish; note: we only handle NUMERIC DISPLAY here */
		/* var may be a temporary field (created from refmod or indexing),
		   which might be overwritten between subsequent calls to the
                   different cob_inspect_* functions */
		st->var_copy = *var;
		st->var = &st->var_copy;
		st->sign = cob_real_get_sign (var, 0);
	} else {
		st->var = NULL;
	}
	st->size = COB_FIELD_SIZE (var);
	st->data = COB_FIELD_DATA (var);
	st->start = NULL;
	st->end = NULL;
	st->mark_size = 0;
	st->repdata_size = 0;

	cobglobptr->cob_exception_code = 0;
}
static void
cob_inspect_init_intern (struct cob_inspect_state *st, cob_field *var, const cob_u32_t replacing)
{
	cob_inspect_init_common_intern (st, var);
	st->replacing = replacing;

	if (st->size > st->mark_size) {
		if (st->mark) {
			cob_free (st->mark);
			st->mark_size = st->size;
		} else if (st->size < COB_NORMAL_BUFF) {
			st->mark_size = COB_NORMAL_BUFF;
		} else {
			st->mark_size = st->size;
		}
		/* initialize to zero */
		st->mark = cob_malloc (st->mark_size + 1);
	} else if (st->mark_size != 0 && st->mark[st->mark_min] != 0) {
		const size_t init_len = st->mark_max - st->mark_min + 1;
		memset (st->mark + st->mark_min, 0, init_len);
	}
	st->mark_min = st->mark_max = 0;
}
void
cob_inspect_init (cob_field *var, const cob_u32_t replacing)
{
	cob_inspect_init_intern (&share_inspect_state, var, replacing);
}

/* an INSPECT CONVERTING / TRANSFORM is split into multiple parts:
   one-time cob_inspect_init_converting
        --> cob_inspect_init_common    (setting up memory)
   multiple:
	cob_inspect_start       (setting start/end)
	cob_inspect_before        (optional, adjusting end)
	cob_inspect_after         (optional, adjusting start)
   one-time cob_inspect_converting/cob_inspect_translating (actual converstion) */

static void
cob_inspect_init_converting_intern (struct cob_inspect_state *st, cob_field *var)
{
	cob_inspect_init_common_intern (st, var);
	st->replacing = 0;	/* only set for pre 3.2 compat because of cob_inspect_finish */
}
void
cob_inspect_init_converting (cob_field *var)
{
	cob_inspect_init_converting_intern (&share_inspect_state, var);
}

static void
cob_inspect_start_intern (struct cob_inspect_state *st)
{
	st->start = st->data;
	st->end = st->data + st->size;
}
void
cob_inspect_start (void)
{
	cob_inspect_start_intern (&share_inspect_state);
}

static void
cob_inspect_before_intern (struct cob_inspect_state *st, const cob_field *str)
{
	unsigned char *data_pos = inspect_find_data (st, str);
	if (data_pos)
		st->end = data_pos;
}
void
cob_inspect_before (const cob_field *str)
{
	cob_inspect_before_intern (&share_inspect_state, str);
}

static void
cob_inspect_after_intern (struct cob_inspect_state *st, const cob_field *str)
{
	unsigned char *data_pos = inspect_find_data (st, str);
	if (data_pos)
		st->start = data_pos + str->size;
	else
		st->start = st->end;
}
void
cob_inspect_after (const cob_field *str)
{
	cob_inspect_after_intern (&share_inspect_state, str);
}

static void
cob_inspect_characters_intern (struct cob_inspect_state *st, cob_field *f1)
{
	const size_t	pos = st->start - st->data;
	const size_t	len = st->end - st->start;
	const unsigned char	*mark_pos = st->mark + pos;
	const unsigned char * const mark_end = mark_pos + len;

	if (len == 0) {
		/* inspecting either a zero-length field or
		   AFTER ... has not found a place to start the conversion */
		return;
	}

	if (st->replacing) {
		/* INSPECT REPLACING CHARACTERS BY f1 (= size 1) */
		const unsigned char repl_by = *f1->data;
		unsigned char	*repdata;
		setup_repdata (st);
		repdata = st->repdata + pos;
		if (is_marked (st, pos, len)) {
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
			memset (repdata, repl_by, len);
		}
	} else {
		/* INSPECT TALLYING f1 CHARACTERS */
		if (is_marked (st, pos, len)) {
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
			cob_add_int (f1, (int)len, 0);
		}
	}
	set_inspect_mark (st, pos, len);
}
void
cob_inspect_characters (cob_field *f1)
{
	cob_inspect_characters_intern(&share_inspect_state, f1);
}

static void
cob_inspect_all_intern (struct cob_inspect_state *st, cob_field *f1, cob_field *f2)
{
	st->type = INSPECT_ALL;
	inspect_common (st, f1, f2);
}
void
cob_inspect_all (cob_field *f1, cob_field *f2)
{
	cob_inspect_all_intern (&share_inspect_state, f1, f2);
}

static void
cob_inspect_leading_intern (struct cob_inspect_state *st, cob_field *f1, cob_field *f2)
{
	st->type = INSPECT_LEADING;
	inspect_common (st, f1, f2);
}
void
cob_inspect_leading (cob_field *f1, cob_field *f2)
{
	cob_inspect_leading_intern (&share_inspect_state, f1, f2);
}

static void
cob_inspect_first_intern (struct cob_inspect_state *st, cob_field *f1, cob_field *f2)
{
	st->type = INSPECT_FIRST;
	inspect_common (st, f1, f2);
}
void
cob_inspect_first (cob_field *f1, cob_field *f2)
{
	cob_inspect_first_intern (&share_inspect_state, f1, f2);
}

static void
cob_inspect_trailing_intern (struct cob_inspect_state *st, cob_field *f1, cob_field *f2)
{
	st->type = INSPECT_TRAILING;
	inspect_common (st, f1, f2);
}
void
cob_inspect_trailing (cob_field *f1, cob_field *f2)
{
	cob_inspect_trailing_intern (&share_inspect_state, f1, f2);
}

static void
cob_inspect_converting_intern (
	struct cob_inspect_state *st,
	const cob_field *f1,
	const cob_field *f2
)
{
	const size_t	len = st->end - st->start;

	if (len == 0) {
		/* our task is to convert either a zero-length field or
		   AFTER ... has not found a place to start the conversion */
		goto end;
	}

	if (!f1) {
		f1 = &str_cob_low;
	}
	if (!f2) {
		f2 = &str_cob_low;
	}
	if (f1->size != f2->size) {
		if (COB_FIELD_TYPE (f2) == COB_TYPE_ALPHANUMERIC_ALL) {
			alloc_figurative (f2, f1);
			f2 = &alpha_fld;
		} else {
			cob_set_exception (COB_EC_RANGE_INSPECT_SIZE);
			goto end;
		}
	}

	/* test _all_ positions of the inspect target against
	   all entries of CONVERTING position by position */
	{
		unsigned char * cur_data = st->data + (st->start - st->data);
		unsigned char * const cur_data_end = cur_data + len;

#if 1 /* table-approach, _much faster_, _should_ be portable */
		/* pre-filled conversion table */
		unsigned char conv_tab[256] = {
			  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
			 16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
			 32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,
			 48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
			 64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,
			 80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
			 96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
			112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
			128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
			144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
			160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175,
			176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191,
			192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207,
			208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223,
			224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239,
			240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255
		};

		/* update conversion table with from/to, skipping duplicates */
		{
			const unsigned char *conv_to   = f2->data;
			const unsigned char *conv_from = f1->data;
			const unsigned char * const conv_from_end = conv_from + f1->size;
			char conv_set[256] = { 0 };
			while (conv_from < conv_from_end) {
				if (conv_set[*conv_from] == 0) {
					conv_set[*conv_from] = 1;
					conv_tab[*conv_from] = *conv_to;
				}
				conv_from++, conv_to++;
			}
		}
		/* iterate over target converting with full table */
		while (cur_data < cur_data_end) {
			*cur_data = conv_tab[*cur_data];
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

end:
	/* note: copied here for 3.2+ as cob_inspect_finish is not generated
	         for TRANSFORM/INSPECT CONVERTING any more */
	if (st->var) {
		/* FIXME: needs test cases for all "goto end" cases above,
		   ideally with a SIGN SEPARATE variable */
		cob_real_put_sign (st->var, st->sign);
	}
}
void
cob_inspect_converting (const cob_field *f1, const cob_field *f2)
{
	cob_inspect_converting_intern (&share_inspect_state, f1, f2);
}

/* note: currently not used by cobc (disabled unfinished prototype) */
static void
cob_inspect_translating_intern (struct cob_inspect_state *st, const unsigned char *conv_table)
{
	const size_t	len = st->end - st->start;

	if (len == 0) {
		/* our task is to convert either a zero-length field or
		   AFTER ... has not found a place to start the conversion
		   --> nothing to do here */
	} else {
		/* directly convert _all_ positions of the inspect target using the
		   pre-generated conversion table */
		unsigned char * cur_data = st->data + (st->start - st->data);
		unsigned char * const cur_data_end = cur_data + len;

		/* iterate over target converting with full table */
		while (cur_data < cur_data_end) {
			*cur_data = conv_table[*cur_data];
			cur_data++;
		}
	}

	if (st->var) {
		cob_real_put_sign (st->var, st->sign);
	}
}
void
cob_inspect_translating (const unsigned char* conv_table)
{
	cob_inspect_translating_intern (&share_inspect_state, conv_table);
}

static void
cob_inspect_finish_intern (struct cob_inspect_state *st)
{
	/* Note: this is not called any more for TRANSFORM/INSPECT CONVERTING
	         since GnuCOBOL 3.2 codegen (only for "old modules")! */

	if (st->replacing
	 && st->repdata_size != 0	/* check for first INSPECT REPLACING having zero length */
	 && st->mark[st->mark_min] != 0) {
		/* copy over replace data from first to last changed position */
		size_t	i;
		for (i = st->mark_min; i <= st->mark_max; ++i) {
			if (st->mark[i] != 0) {
				st->data[i] = st->repdata[i];
			}
		}
#if 0	/* drop data copy because of security issues
		[may only be done upon request]; if not active and
		that contains sensitive data do an INSPECT against a field
		of the same size to overwrite the buffer */
		memset (st->repdata + st->mark_min, 0,
		        st->mark_max - st->mark_min + 1);
#endif
	}

	if (st->var) {
		cob_real_put_sign (st->var, st->sign);
	}
}
void
cob_inspect_finish (void)
{
	cob_inspect_finish_intern (&share_inspect_state);
}


/* STRING */
/* a STRING is split into multiple parts:
   one-time cob_string_init  (setting up memory and static variables)
   1..n :
    cob_string_delimited    (setting delimiter struct entries)
     1..n:
	   cob_string_append    (to handle a single source)
   one-time cob_string_finish (setting the string pointer) */

static void
cob_string_init_intern (struct cob_string_state *st, cob_field *dst, cob_field *ptr)
{
	/* dst and ptr may be temporary fields (created from refmod or indexing),
	   which might be overwritten between subsequent calls to the
	   different cob_string_* functions */
	st->dst_copy = *dst;
	st->dst = &st->dst_copy;
	st->ptr = NULL;
	if (ptr) {
		st->ptr_copy = *ptr;
		st->ptr = &st->ptr_copy;
	}
	st->offset = 0;
	cobglobptr->cob_exception_code = 0;

	if (st->ptr) {
		st->offset = cob_get_int (st->ptr) - 1;
		if (st->offset < 0 || st->offset >= (int)st->dst->size) {
			cob_set_exception (COB_EC_OVERFLOW_STRING);
		}
	}
}
void
cob_string_init (cob_field *dst, cob_field *ptr)
{
	cob_string_init_intern (&share_string_state, dst, ptr);
}

static void
cob_string_delimited_intern (struct cob_string_state *st, cob_field *dlm)
{
	/* dlm may be a temporary field (created from refmod or indexing),
	   which might be overwritten between subsequent calls to the
	   different cob_string_* functions */
	if (dlm) {
		st->dlm_copy = *dlm;
		st->dlm = &st->dlm_copy;
	} else {
		st->dlm = NULL;
	}
}
void
cob_string_delimited (cob_field *dlm)
{
	cob_string_delimited_intern (&share_string_state, dlm);
}

static void
cob_string_append_intern (struct cob_string_state *st, cob_field *src)
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
	if (st->dlm) {
		size = (int)(src_size - st->dlm->size + 1);
		for (i = 0; i < size; ++i) {
			if (memcmp (src->data + i, st->dlm->data,
				    st->dlm->size) == 0) {
				src_size = i;
				break;
			}
		}
	}

	if (src_size <= st->dst->size - st->offset) {
		memcpy (st->dst->data + st->offset, src->data, src_size);
		st->offset += (int) src_size;
	} else {
		size = (int)(st->dst->size - st->offset);
		memcpy (st->dst->data + st->offset, src->data, (size_t)size);
		st->offset += size;
		cob_set_exception (COB_EC_OVERFLOW_STRING);
	}
}
void
cob_string_append (cob_field *src)
{
	cob_string_append_intern (&share_string_state, src);
}

static void
cob_string_finish_intern (struct cob_string_state *st)
{
	if (st->ptr) {
		cob_set_int (st->ptr, st->offset + 1);
	}
}
void
cob_string_finish (void)
{
	cob_string_finish_intern (&share_string_state);
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

static void
cob_unstring_init_intern (
	struct cob_unstring_state *st,
	cob_field *src,
	cob_field *ptr,
	const size_t num_dlm
)
{
	/* src and ptr may be temporary fields (created from refmod or indexing),
	   which might be overwritten between subsequent calls to the
	   different cob_unstring_* functions */
	st->src_copy = *src;
	st->src = &st->src_copy;
	st->ptr = NULL;
	if (ptr) {
		st->ptr_copy = *ptr;
		st->ptr = &st->ptr_copy;
	}

	st->offset = 0;
	st->count = 0;
	st->ndlms = 0;
	st->dlm_list_size = 0;
	cobglobptr->cob_exception_code = 0;
	if (num_dlm > st->dlm_list_size) {
		if (st->dlm_list) {
			cob_free (st->dlm_list);
			st->dlm_list_size = num_dlm;
		} else if (num_dlm < DLM_DEFAULT_NUM) {
			st->dlm_list_size = DLM_DEFAULT_NUM;
		} else {
			st->dlm_list_size = num_dlm;
		}
		st->dlm_list = cob_malloc (st->dlm_list_size * sizeof(struct dlm_struct));
	}

	if (st->ptr) {
		st->offset = cob_get_int (st->ptr) - 1;
		if (st->offset < 0 || st->offset >= (int)st->src->size) {
			cob_set_exception (COB_EC_OVERFLOW_UNSTRING);
		}
	}
}
void
cob_unstring_init (
	cob_field *src,
	cob_field *ptr,
	const size_t num_dlm
)
{
	cob_unstring_init_intern (&share_unstring_state, src, ptr, num_dlm);
}

static void
cob_unstring_delimited_intern (struct cob_unstring_state *st, cob_field *dlm, const cob_u32_t all)
{
	st->dlm_list[st->ndlms].uns_dlm = *dlm;
	st->dlm_list[st->ndlms].uns_all = all;
	st->ndlms++;
}
void
cob_unstring_delimited (cob_field *dlm, const cob_u32_t all)
{
	cob_unstring_delimited_intern (&share_unstring_state, dlm, all);
}

static void
cob_unstring_into_intern (
	struct cob_unstring_state *st,
	cob_field *dst,
	cob_field *dlm,
	cob_field *cnt
)
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

	if (st->offset >= (int)st->src->size) {
		/* overflow from the last iteration (multiple INTO targets) */
		return;
	}

	dlm_data = NULL;
	start = st->src->data + st->offset;

	/* no delimiter - just split into DELIMITED BY SIZE */
	if (st->ndlms == 0) {
		 /* necessary for correct unstring offset: minimal size */
		/* Note: field->size and therefore offset
		   are guaranteed to be < INT_MAX by cobc */
		match_size = cob_min_int ((int)COB_FIELD_SIZE (dst),
					  (int)st->src->size - st->offset);
		cob_str_memcpy (dst, start, match_size);
		st->offset += match_size;

	/* DELIMITED BY [ALL] x [.. OR [ALL] z] */
	} else {

		const int	srsize = (int)st->src->size;
		unsigned char	*p;
		unsigned char	*dp;
		int		found = 0;

		/* note: duplicate code for performance as most cases
		         have either none or a single delimiter */
		if (st->ndlms == 1) {
			const struct dlm_struct dlms = st->dlm_list[0];
			const int     dlsize = (int) dlms.uns_dlm.size;
			const unsigned char *s = st->src->data + srsize - dlsize + 1;
			dp = dlms.uns_dlm.data;

			for (p = start; p < s; ++p) {
				if (!memcmp (p, dp, (size_t)dlsize)) {         /* delimiter matches */
					match_size = (int)(p - start);             /* count in */
					cob_str_memcpy (dst, start, match_size);   /* into */
					st->offset += match_size + dlsize;    /* with pointer */
					dlm_data = dp;
					dlm_size = dlsize;
					if (dlms.uns_all) {                     /* delimited by all */
						for (p += dlsize; p < s; p += dlsize) {
							if (memcmp (p, dp, (size_t)dlsize)) {
								break;
							}
							st->offset += dlsize;
						}
					}
					found = 1;
					break;
				}
			}
		} else {
			const unsigned char *s = st->src->data + srsize;
			int		i;
			for (p = start; p < s; ++p) {
				for (i = 0; i < st->ndlms; ++i) {
					const struct dlm_struct dlms = st->dlm_list[i];
					const int     dlsize = (int)dlms.uns_dlm.size;
					const unsigned char *s2 = s - dlsize + 1;
					if (p > s2) {
						continue;
					}
					dp = dlms.uns_dlm.data;
					if (!memcmp (p, dp, (size_t)dlsize)) {         /* delimiter matches */
						match_size = (int)(p - start);             /* count in */
						cob_str_memcpy (dst, start, match_size);   /* into */
						st->offset += match_size + dlsize;    /* with pointer */
						dlm_data = dp;
						dlm_size = dlsize;
						if (dlms.uns_all) {                     /* delimited by all */
							for (p += dlsize; p < s2; p += dlsize) {
								if (memcmp (p, dp, (size_t)dlsize)) {
									break;
								}
								st->offset += dlsize;
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
			match_size = (int)(st->src->size - st->offset);
			cob_str_memcpy (dst, start, match_size);
			st->offset = (int) st->src->size;
		}
	}
	st->count++;

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
cob_unstring_into (
	cob_field *dst,
	cob_field *dlm,
	cob_field *cnt
)
{
	cob_unstring_into_intern (&share_unstring_state, dst, dlm, cnt);
}

static void
cob_unstring_tallying_intern (struct cob_unstring_state *st, cob_field *f)
{
	cob_add_int (f, st->count, 0);
}
void
cob_unstring_tallying (cob_field *f)
{
	cob_unstring_tallying_intern (&share_unstring_state, f);
}

static void
cob_unstring_finish_intern (struct cob_unstring_state *st)
{
	if (st->offset < (int)st->src->size) {
		/* overflow from any iteration -> overflow exception */
		cob_set_exception (COB_EC_OVERFLOW_UNSTRING);
	}

	if (st->ptr) {
		cob_set_int (st->ptr, st->offset + 1);
	}
}
void
cob_unstring_finish (void)
{
	cob_unstring_finish_intern (&share_unstring_state);
}

/* Initialization/Termination */

void
cob_exit_strings ()
{
	struct cob_inspect_state *sti = &share_inspect_state;
	struct cob_unstring_state *stu = &share_unstring_state;

	if (sti->mark) {
		cob_free (sti->mark);
		sti->mark = NULL;
	}
	sti->mark_size = sti->mark_min = sti->mark_max = 0;
	if (sti->repdata) {
		cob_free (sti->repdata);
		sti->repdata = NULL;
	}
	sti->repdata_size = 0;

	if (stu->dlm_list) {
		cob_free (stu->dlm_list);
		stu->dlm_list = NULL;
	}
	stu->dlm_list_size = 0;

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

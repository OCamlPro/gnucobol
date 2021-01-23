/*
   Copyright (C) 2005-2012, 2014-2017 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Sergey Kashyrin

   This file is part of GnuCOBOL C++.

   The GnuCOBOL C++ runtime library is free software: you can redistribute it

   and/or modify it under the terms of the GNU Lesser General Public License
   as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   GnuCOBOL C++ is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with GnuCOBOL C++.  If not, see <http://www.gnu.org/licenses/>.
*/


#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>

/* Force symbol exports */
#define	COB_LIB_EXPIMP

#include "libcob.h"
#include "coblocal.h"

#define INSPECT_ALL			0
#define INSPECT_LEADING		1
#define INSPECT_FIRST		2
#define INSPECT_TRAILING	3

#define DLM_DEFAULT_NUM		8U

struct dlm_struct {
	cob_field	uns_dlm;
	cob_u32_t	uns_all;
	dlm_struct() : uns_dlm(0, 0, 0), uns_all(0) {}
};

/* Local variables */

static const cob_field_attr	const_alpha_attr(COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL);
static const cob_field_attr	const_strall_attr(COB_TYPE_ALPHANUMERIC_ALL, 0, 0, 0, NULL);

static cob_field 	*		inspect_var;
static unsigned char 	*	inspect_data;
static unsigned char 	*	inspect_start;
static unsigned char 	*	inspect_end;
static int			*		inspect_mark;
static int					inspect_mark_size;
static int					inspect_size;
static cob_u32_t			inspect_replacing;
static int					inspect_sign;
static cob_field			inspect_var_copy(0, 0, 0);

static cob_field 	*		string_dst;
static cob_field 	*		string_ptr;
static cob_field 	*		string_dlm;
static cob_field			string_dst_copy(0, 0, 0);
static cob_field			string_ptr_copy(0, 0, 0);
static cob_field			string_dlm_copy(0, 0, 0);
static int					string_offset;

static dlm_struct 	*		dlm_list;
static cob_field 	*		unstring_src;
static cob_field 	*		unstring_ptr;
static size_t				dlm_list_size;
static cob_field			unstring_src_copy(0, 0, 0);
static cob_field			unstring_ptr_copy(0, 0, 0);
static int					unstring_offset;
static int					unstring_count;
static int					unstring_ndlms;

static unsigned char 	*	figurative_ptr;
static size_t				figurative_size;

static cob_field			alpha_fld(0, 0, &const_alpha_attr);
static cob_field			str_cob_low(1, (cob_u8_ptr)"\0", &const_strall_attr);

/* Local functions */

static void
cob_str_memcpy(cob_field * dst, unsigned char * src, const int size)
{
	cob_field temp(size, src, &const_alpha_attr);
	cob_move(&temp, dst);
}

static void
alloc_figurative(const cob_field * f1, const cob_field * f2)
{
	size_t size2 = f2->size;
	if(size2 > figurative_size) {
		if(figurative_ptr) {
			delete [] figurative_ptr;
		}
		figurative_ptr = new unsigned char[size2];
		figurative_size = size2;
	}
	size_t size1 = 0;
	unsigned char * s = figurative_ptr;
	for(size_t n = 0; n < size2; ++n, ++s) {
		*s = f1->data[size1];
		size1++;
		if(size1 >= f1->size) {
			size1 = 0;
		}
	}
	alpha_fld.size = size2;
	alpha_fld.data = figurative_ptr;
}

static void
inspect_common(cob_field * f1, cob_field * f2, const int type)
{
	size_t n = 0;

	if(unlikely(!f1)) {
		f1 = &str_cob_low;
	}
	if(unlikely(!f2)) {
		f2 = &str_cob_low;
	}

	if(inspect_replacing && f1->size != f2->size) {
		if(COB_FIELD_TYPE(f1) == COB_TYPE_ALPHANUMERIC_ALL) {
			alloc_figurative(f1, f2);
			f1 = &alpha_fld;
		} else {
			cob_set_exception(COB_EC_RANGE_INSPECT_SIZE);
			return;
		}
	}

	int * mark = &inspect_mark[inspect_start - inspect_data];
	int len = (int)(inspect_end - inspect_start);
	if(type == INSPECT_TRAILING) {
		for(int i = len - (int)f2->size; i >= 0; --i) {
			/* Find matching substring */
			if(memcmp(inspect_start + i, f2->data, f2->size) == 0) {
				/* Check if it is already marked */
				size_t j;
				for(j = 0; j < f2->size; ++j) {
					if(mark[i + j] != -1) {
						break;
					}
				}
				/* If not, mark and count it */
				if(j == f2->size) {
					for(j = 0; j < f2->size; ++j) {
						mark[i + j] = inspect_replacing ? f1->data[j] : 1;
					}
					i -= (int)(f2->size - 1);
					n++;
				}
			} else {
				break;
			}
		}
	} else {
		for(int i = 0; i < (int)(len - f2->size + 1); ++i) {
			/* Find matching substring */
			if(memcmp(inspect_start + i, f2->data, f2->size) == 0) {
				/* Check if it is already marked */
				size_t j;
				for(j = 0; j < f2->size; ++j) {
					if(mark[i + j] != -1) {
						break;
					}
				}
				/* If not, mark and count it */
				if(j == f2->size) {
					for(j = 0; j < f2->size; ++j) {
						mark[i + j] = inspect_replacing ? f1->data[j] : 1;
					}
					i += (int)(f2->size - 1);
					n++;
					if(type == INSPECT_FIRST) {
						break;
					}
				}
			} else if(type == INSPECT_LEADING) {
				break;
			}
		}
	}

	if(n > 0 && !inspect_replacing) {
		cob_add_int(f1, (int) n, 0);
	}
}

/* Global functions */

/* INSPECT */

void
cob_inspect_init(cob_field * var, const cob_u32_t replacing)
{
	if(unlikely(COB_FIELD_IS_NUMDISP(var))) {
		inspect_var_copy = *var;
		inspect_var = &inspect_var_copy;
		inspect_sign = COB_GET_SIGN(var);
	} else {
		inspect_var = NULL;
	}
	inspect_size = (int) COB_FIELD_SIZE(var);
	inspect_data = COB_FIELD_DATA(var);
	inspect_replacing = replacing;
	inspect_start = NULL;
	inspect_end = NULL;
	if(inspect_size > inspect_mark_size) {
		if(inspect_mark) {
			delete [] inspect_mark;
		}
		inspect_mark = new int[inspect_size];
		inspect_mark_size = inspect_size;
	}
	for(int i = 0; i < inspect_size; ++i) {
		inspect_mark[i] = -1;
	}
	cob_set_exception(0);
}

void
cob_inspect_start(void)
{
	inspect_start = inspect_data;
	inspect_end = inspect_data + inspect_size;
}

void
cob_inspect_before(const cob_field * str)
{
	for(unsigned char * p = inspect_start; p < inspect_end - str->size + 1; ++p) {
		if(memcmp(p, str->data, str->size) == 0) {
			inspect_end = p;
			return;
		}
	}
}

void
cob_inspect_after(const cob_field * str)
{
	for(unsigned char * p = inspect_start; p < inspect_end - str->size + 1; ++p) {
		if(memcmp(p, str->data, str->size) == 0) {
			inspect_start = p + str->size;
			return;
		}
	}
	inspect_start = inspect_end;
}

void
cob_inspect_characters(cob_field * f1)
{
	int * mark = &inspect_mark[inspect_start - inspect_data];
	int len = (int)(inspect_end - inspect_start);
	if(inspect_replacing) {
		/* INSPECT REPLACING CHARACTERS f1 */
		for(int i = 0; i < len; ++i) {
			if(mark[i] == -1) {
				mark[i] = f1->data[0];
			}
		}
	} else {
		/* INSPECT TALLYING f1 CHARACTERS */
		int n = 0;
		for(int i = 0; i < len; ++i) {
			if(mark[i] == -1) {
				mark[i] = 1;
				n++;
			}
		}
		if(n > 0) {
			cob_add_int(f1, n, 0);
		}
	}
}

void
cob_inspect_all(cob_field * f1, cob_field * f2)
{
	inspect_common(f1, f2, INSPECT_ALL);
}

void
cob_inspect_leading(cob_field * f1, cob_field * f2)
{
	inspect_common(f1, f2, INSPECT_LEADING);
}

void
cob_inspect_first(cob_field * f1, cob_field * f2)
{
	inspect_common(f1, f2, INSPECT_FIRST);
}

void
cob_inspect_trailing(cob_field * f1, cob_field * f2)
{
	inspect_common(f1, f2, INSPECT_TRAILING);
}

void
cob_inspect_converting(const cob_field * f1, const cob_field * f2)
{
	if(unlikely(!f1)) {
		f1 = &str_cob_low;
	}
	if(unlikely(!f2)) {
		f2 = &str_cob_low;
	}
	if(f1->size != f2->size) {
		if(COB_FIELD_TYPE(f2) == COB_TYPE_ALPHANUMERIC_ALL) {
			alloc_figurative(f2, f1);
			f2 = &alpha_fld;
		} else {
			cob_set_exception(COB_EC_RANGE_INSPECT_SIZE);
			return;
		}
	}

	size_t len = (size_t)(inspect_end - inspect_start);
	for(size_t j = 0; j < f1->size; ++j) {
		for(size_t i = 0; i < len; ++i) {
			if(inspect_mark[i] == -1 && inspect_start[i] == f1->data[j]) {
				inspect_start[i] = f2->data[j];
				inspect_mark[i] = 1;
			}
		}
	}
}

void
cob_inspect_finish(void)
{
	if(inspect_replacing) {
		for(int i = 0; i < inspect_size; ++i) {
			if(inspect_mark[i] != -1) {
				inspect_data[i] = (unsigned char) inspect_mark[i];
			}
		}
	}

	if(unlikely(inspect_var)) {
		COB_PUT_SIGN(inspect_var, inspect_sign);
	}
}

/* STRING */

void
cob_string_init(cob_field * dst, cob_field * ptr)
{
	string_dst_copy = *dst;
	string_dst = &string_dst_copy;
	string_ptr = NULL;
	if(ptr) {
		string_ptr_copy = *ptr;
		string_ptr = &string_ptr_copy;
	}
	string_offset = 0;
	cob_set_exception(0);

	if(string_ptr) {
		string_offset = cob_get_int(string_ptr) - 1;
		if(string_offset < 0 ||
				string_offset >= (int)string_dst->size) {
			cob_set_exception(COB_EC_OVERFLOW_STRING);
		}
	}
}

void
cob_string_delimited(cob_field * dlm)
{
	string_dlm = NULL;
	if(dlm) {
		string_dlm_copy = *dlm;
		string_dlm = &string_dlm_copy;
	}
}

void
cob_string_append(cob_field * src)
{
	if(cob_get_exception_code()) {
		return;
	}

	size_t src_size = src->size;
	if(!src_size) {
		return;
	}
	if(string_dlm) {
		int size = (int)(src_size - string_dlm->size + 1);
		for(int i = 0; i < size; ++i) {
			if(memcmp(src->data + i, string_dlm->data, string_dlm->size) == 0) {
				src_size = i;
				break;
			}
		}
	}

	if(src_size <= string_dst->size - string_offset) {
		memcpy(string_dst->data + string_offset, src->data, src_size);
		string_offset += (int) src_size;
	} else {
		int size = (int)(string_dst->size - string_offset);
		memcpy(string_dst->data + string_offset, src->data, (size_t)size);
		string_offset += size;
		cob_set_exception(COB_EC_OVERFLOW_STRING);
	}
}

void
cob_string_finish(void)
{
	if(string_ptr) {
		cob_set_int(string_ptr, string_offset + 1);
	}
}

/* UNSTRING */

void
cob_unstring_init(cob_field * src, cob_field * ptr, const size_t num_dlm)
{
	unstring_src_copy = *src;
	unstring_src = &unstring_src_copy;
	unstring_ptr = NULL;
	if(ptr) {
		unstring_ptr_copy = *ptr;
		unstring_ptr = &unstring_ptr_copy;
	}

	unstring_offset = 0;
	unstring_count = 0;
	unstring_ndlms = 0;
	cob_set_exception(0);
	if(num_dlm > dlm_list_size) {
		delete [] dlm_list;
		dlm_list = new dlm_struct[num_dlm];
		dlm_list_size = num_dlm;
	}

	if(unstring_ptr) {
		unstring_offset = cob_get_int(unstring_ptr) - 1;
		if(unstring_offset < 0 || unstring_offset >= (int)unstring_src->size) {
			cob_set_exception(COB_EC_OVERFLOW_UNSTRING);
		}
	}
}

void
cob_unstring_delimited(cob_field * dlm, const cob_u32_t all)
{
	dlm_list[unstring_ndlms].uns_dlm = *dlm;
	dlm_list[unstring_ndlms].uns_all = all;
	unstring_ndlms++;
}

void
cob_unstring_into(cob_field * dst, cob_field * dlm, cob_field * cnt)
{
	size_t dlm_size = 0;
	int match_size = 0;

	if(cob_get_exception_code()) {
		return;
	}

	if(unstring_offset >= (int)unstring_src->size) {
		return;
	}

	unsigned char * start = unstring_src->data + unstring_offset;
	unsigned char * dlm_data = NULL;
	if(unstring_ndlms == 0) {
		match_size = cob_min_int((int)COB_FIELD_SIZE(dst),
								 (int)unstring_src->size - unstring_offset);
		cob_str_memcpy(dst, start, match_size);
		unstring_offset += match_size;
	} else {
		bool brkpt = false;
		int srsize = (int) unstring_src->size;
		unsigned char * s = unstring_src->data + srsize;
		for(unsigned char * p = start; p < s; ++p) {
			for(int i = 0; i < unstring_ndlms; ++i) {
				int dlsize = (int) dlm_list[i].uns_dlm.size;
				unsigned char * dp = dlm_list[i].uns_dlm.data;
				if(p + dlsize > s) {
					continue;
				}
				if(!memcmp(p, dp, (size_t)dlsize)) {			// delimiter equal
					match_size = (int)(p - start);			// count in
					cob_str_memcpy(dst, start, match_size);	// into
					unstring_offset += match_size + dlsize;	// with pointer
					dlm_data = dp;
					dlm_size = dlsize;
					if(dlm_list[i].uns_all) {
						for(p += dlsize; p < s; p += dlsize) {
							if(p + dlsize > s) {
								break;
							}
							if(memcmp(p, dp, (size_t)dlsize)) {
								break;
							}
							unstring_offset += dlsize;
						}
					}
					brkpt = true;
					break;
				}
			}
			if(brkpt) {
				break;
			}
		}
		if(!brkpt) {
			/* No match */
			match_size = (int)(unstring_src->size - unstring_offset);
			cob_str_memcpy(dst, start, match_size);
			unstring_offset = (int) unstring_src->size;
			dlm_data = NULL;
		}
	}
	unstring_count++;

	if(dlm) {
		if(dlm_data) {
			cob_str_memcpy(dlm, dlm_data, (int) dlm_size);
		} else if(COB_FIELD_IS_NUMERIC(dlm)) {
			cob_set_int(dlm, 0);
		} else {
			memset(dlm->data, ' ', dlm->size);
		}
	}

	if(cnt) {
		cob_set_int(cnt, match_size);
	}
}

void
cob_unstring_tallying(cob_field * f)
{
	cob_add_int(f, unstring_count, 0);
}

void
cob_unstring_finish(void)
{
	if(unstring_offset < (int)unstring_src->size) {
		cob_set_exception(COB_EC_OVERFLOW_UNSTRING);
	}

	if(unstring_ptr) {
		cob_set_int(unstring_ptr, unstring_offset + 1);
	}
}

/* Initialization/Termination */

void
cob_exit_strings(void)
{
	if(inspect_mark) {
		delete [] inspect_mark;
		inspect_mark = NULL;
	}
	if(dlm_list) {
		delete [] dlm_list;
		dlm_list = NULL;
	}
	if(figurative_ptr) {
		delete [] figurative_ptr;
		figurative_ptr = NULL;
	}
	figurative_size = 0;
}

void
cob_init_strings(void)
{
	inspect_mark = new int[512];
	inspect_mark_size = 512;
	dlm_list = new dlm_struct[DLM_DEFAULT_NUM];
	dlm_list_size = DLM_DEFAULT_NUM;
	figurative_ptr = NULL;
	figurative_size = 0;
	alpha_fld.size = 0;
	alpha_fld.data = NULL;
	alpha_fld.attr = &const_alpha_attr;
	str_cob_low.size = 1;
	str_cob_low.data = (cob_u8_ptr)"\0";
	str_cob_low.attr = &const_strall_attr;
}

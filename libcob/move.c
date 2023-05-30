/*
   Copyright (C) 2002-2012, 2014-2020, 2022-2023 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Simon Sobisch, Ron Norman,
   Edwart Hard

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
#include <limits.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <math.h>

#ifdef	HAVE_LOCALE_H
#include <locale.h>
#endif

/* include internal and external libcob definitions, forcing exports */
#define	COB_LIB_EXPIMP
#include "coblocal.h"

static cob_global	*cobglobptr;
static cob_settings	*cobsetptr;

#if	0	/* RXWRXW local edit symbols */
static unsigned int	cob_locale_edit;
static unsigned char	cob_lc_dec;
static unsigned char	cob_lc_thou;
#endif

static const cob_field_attr	const_alpha_attr =
				{COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
static const cob_field_attr	const_bin_attr =
				{COB_TYPE_NUMERIC_BINARY, 9, 0,
				 COB_FLAG_HAVE_SIGN | COB_FLAG_REAL_BINARY, NULL};
static const cob_field_attr	const_binll_attr =
				{COB_TYPE_NUMERIC_BINARY, 20, 0,
				 COB_FLAG_HAVE_SIGN | COB_FLAG_REAL_BINARY, NULL};
static const cob_field_attr	all_display_attr =
				{COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
static const cob_field_attr	all_numeric_display_attr =
				{COB_TYPE_NUMERIC_DISPLAY, COB_MAX_DIGITS, 0,
				 0, NULL};

static unsigned char all_numeric_data[COB_MAX_DIGITS];
static const cob_field	all_numeric_field  = 
				{COB_MAX_DIGITS, all_numeric_data,
				 &all_numeric_display_attr};

static const int	cob_exp10[10] = {
	1,
	10,
	100,
	1000,
	10000,
	100000,
	1000000,
	10000000,
	100000000,
	1000000000
};

static const cob_s64_t	cob_exp10_ll[19] = {
	COB_S64_C(1),
	COB_S64_C(10),
	COB_S64_C(100),
	COB_S64_C(1000),
	COB_S64_C(10000),
	COB_S64_C(100000),
	COB_S64_C(1000000),
	COB_S64_C(10000000),
	COB_S64_C(100000000),
	COB_S64_C(1000000000),
	COB_S64_C(10000000000),
	COB_S64_C(100000000000),
	COB_S64_C(1000000000000),
	COB_S64_C(10000000000000),
	COB_S64_C(100000000000000),
	COB_S64_C(1000000000000000),
	COB_S64_C(10000000000000000),
	COB_S64_C(100000000000000000),
	COB_S64_C(1000000000000000000)
};

static void
store_common_region (cob_field *f, const unsigned char *data,
		     const size_t size, const int scale, const int verified_data)
{
	const int	fsize = (int) COB_FIELD_SIZE (f);
	unsigned char *fdata = COB_FIELD_DATA (f);

	const int	lf1 = -scale;
	const int	lf2 = -COB_FIELD_SCALE (f);
	const int	lcf = cob_max_int (lf1, lf2);

	const int	hf1 = (int) size + lf1;
	const int	hf2 = fsize + lf2;
	const int	gcf = cob_min_int (hf1, hf2);

	/* the target may have leading/trailing additional zeroes
	   and in rare cases, we may be out of scale competely;
	   we pre-set all positions as this saves a bunch of
	   calculations which outweight the benefit of not
	   writing over the data two times */
	memset (fdata, '0', fsize);

	/* note: skipping zeroes in the source data was tested but
	   has shown to be slower than copying those along */

	if (gcf > lcf) {
		if (verified_data) {
			memcpy (fdata + hf2 - gcf, data + hf1 - gcf, gcf - lcf);
		} else {
			register const unsigned char	*src = data + hf1 - gcf;
			register unsigned char		*dst = fdata + hf2 - gcf;
			register const unsigned char	*end = dst + gcf - lcf;

			while (dst < end) {
				const char src_data = *src++;
#if 0		/* seems to be the best result, ..." */
				/* we don't want to set bad data, so
				   only take the half byte */
				*dst = COB_I2D (COB_D2I (src_data));
#else		/* but does not match the "expected" MF result, which is: */
				if (src_data == ' ' || src_data == 0) /* already set: *dst = '0'; */ ;
				else *dst = COB_I2D (src_data - '0');
#endif
				++dst;
			}
		}
	}
}

static COB_INLINE COB_A_INLINE cob_s64_t
cob_binary_mget_sint64 (const cob_field * const f)
{
	cob_s64_t	n = 0;
	const size_t	fsiz = 8U - f->size;

#ifndef WORDS_BIGENDIAN
	if (COB_FIELD_BINARY_SWAP (f)) {
		if (COB_FIELD_HAVE_SIGN (f)) {
			memcpy (&n, f->data, f->size);
			n = COB_BSWAP_64 (n);
			/* Shift with sign */
			n >>= (cob_s64_t)8 * fsiz;
		} else {
			memcpy ((unsigned char *)&n + fsiz, f->data, f->size);
			n = COB_BSWAP_64 (n);
		}
	} else {
		if (COB_FIELD_HAVE_SIGN (f)) {
			memcpy ((unsigned char *)&n + fsiz, f->data, f->size);
			/* Shift with sign */
			n >>= (cob_s64_t)8 * fsiz;
		} else {
			memcpy (&n, f->data, f->size);
		}
	}
#else	/* WORDS_BIGENDIAN */
	if (COB_FIELD_HAVE_SIGN (f)) {
		memcpy (&n, f->data, f->size);
		/* Shift with sign */
		n >>= 8 * fsiz;
	} else {
		memcpy ((unsigned char *)&n + fsiz, f->data, f->size);
	}
#endif	/* WORDS_BIGENDIAN */
	return n;
}

static COB_INLINE COB_A_INLINE cob_u64_t
cob_binary_mget_uint64 (const cob_field * const f)
{
	cob_u64_t		n = 0;
	const size_t	fsiz = 8 - f->size;

#ifndef WORDS_BIGENDIAN
	if (COB_FIELD_BINARY_SWAP (f)) {
		memcpy ((unsigned char *)&n + fsiz, f->data, f->size);
		n = COB_BSWAP_64 (n);
	} else {
		memcpy (&n, f->data, f->size);
	}
#else	/* WORDS_BIGENDIAN */
	memcpy (((unsigned char *)&n) + fsiz, f->data, f->size);
#endif	/* WORDS_BIGENDIAN */

	return n;
}

static COB_INLINE COB_A_INLINE void
cob_binary_mset_sint64 (cob_field *f, cob_s64_t n)
{
	const cob_s64_t		fsiz = 8 - f->size;
#ifndef WORDS_BIGENDIAN
	unsigned char	*s;

	if (COB_FIELD_BINARY_SWAP (f)) {
		n = COB_BSWAP_64 (n);
		s = (unsigned char *)&n + fsiz;
	} else {
		s = (unsigned char *)&n;
	}
	memcpy (f->data, s, f->size);
#else	/* WORDS_BIGENDIAN */
	memcpy (f->data, (unsigned char *)&n + fsiz, f->size);
#endif	/* WORDS_BIGENDIAN */
}

static COB_INLINE COB_A_INLINE void
cob_binary_mset_uint64 (cob_field *f, cob_u64_t n)
{
#ifndef WORDS_BIGENDIAN
	unsigned char	*s;

	if (COB_FIELD_BINARY_SWAP (f)) {
		n = COB_BSWAP_64 (n);
		s = ((unsigned char *)&n) + 8 - f->size;
	} else {
		s = (unsigned char *)&n;
	}
	memcpy (f->data, s, f->size);
#else	/* WORDS_BIGENDIAN */
	memcpy (f->data, ((unsigned char *)&n) + 8 - f->size, f->size);
#endif	/* WORDS_BIGENDIAN */
}

/* Display */

static void
cob_move_alphanum_to_display (cob_field *f1, cob_field *f2)
{
	register unsigned char	*s1 = f1->data;
	register unsigned char	*s2 = COB_FIELD_DATA (f2);
	const unsigned char	*e1 = s1 + f1->size;
	const unsigned char	*e2 = s2 + COB_FIELD_SIZE (f2);
	const unsigned char	dec_pt = COB_MODULE_PTR->decimal_point;
	const unsigned char	num_sep = COB_MODULE_PTR->numeric_separator;
	int		sign;
	int		count;
	int		size;

	/* Initialize */
	memset (f2->data, '0', f2->size);

	/* Skip white spaces */
	for (; s1 < e1; ++s1) {
		if (!isspace (*s1)) {
			break;
		}
	}

	/* Check for sign */
	sign = 0;
	if (s1 != e1) {
		if (*s1 == '+' || *s1 == '-') {
			sign = (*s1++ == '+') ? 1 : -1;
		}
	}

	/* Count the number of digits before decimal point */
	count = 0;
	{
		register unsigned char	*p;
		for (p = s1; p < e1 && *p != dec_pt; ++p) {
			/* note: as isdigit is locale-aware (slower and not what we want),
			   we use a range check instead */
			if (*p >= '0' && *p <= '9') {
				++count;
			}
		}
	}

	/* Find the start position */
	size = (int) COB_FIELD_SIZE (f2) - COB_FIELD_SCALE (f2);
	if (count < size) {
		s2 += size - count;
	} else {
		while (count-- > size) {
			while (*s1 < '0' || *s1 > '9') {
				s1++;
			}
			s1++;
		}
	}

	/* Move */
	count = 0;
	for (; s1 < e1 && s2 < e2; ++s1) {
		if (*s1 >= '0' && *s1 <= '9') {
			*s2++ = *s1;
		} else if (*s1 == dec_pt) {
			if (count++ > 0) {
				goto error;
			}
		} else if (!(isspace (*s1) || *s1 == num_sep)) {
			goto error;
		}
	}

	COB_PUT_SIGN (f2, sign);
	return;

error:
	memset (f2->data, '0', f2->size);
	COB_PUT_SIGN (f2, 0);
}

static void
cob_move_display_to_display (cob_field *f1, cob_field *f2)
{
	const int	sign = COB_GET_SIGN (f1);

	store_common_region (f2, COB_FIELD_DATA (f1), COB_FIELD_SIZE (f1),
			     COB_FIELD_SCALE (f1), 0);

	COB_PUT_SIGN (f1, sign);
	COB_PUT_SIGN (f2, sign);
}

static void
cob_move_display_to_alphanum (cob_field *f1, cob_field *f2)
{
	unsigned char	*data1 = COB_FIELD_DATA (f1);
	unsigned char	*data2 = f2->data;
	const size_t		size1 = COB_FIELD_SIZE (f1);
	size_t		size2 = f2->size;
	/* note: can't use the "adjust" variant here as we don't convert to digit;
	   CHECKME for using a buffer instead of original data or intermediate_move */
	const int		sign = COB_GET_SIGN (f1);
	int		diff;
	int		zero_size;

	if (unlikely (COB_FIELD_SCALE (f1) < 0)) {
		/* Scaling */
		zero_size = (int)-COB_FIELD_SCALE (f1);
	} else {
		zero_size = 0;
	}
	if (unlikely (COB_FIELD_JUSTIFIED (f2))) {
		/* Justified right */
		if (zero_size) {
			/* Implied 0 ('P's) */
			zero_size = cob_min_int (zero_size, (int)size2);
			size2 -= zero_size;
			memset (data2 + size2, '0', (size_t) zero_size);
		}
		if (size2) {
			diff = (int)(size2 - size1);
			if (diff > 0) {
				/* Padding */
				memset (data2, ' ', (size_t)diff);
				data2 += diff;
				size2 -= diff;
			}
			memmove (data2, data1 + size1 - size2, size2);
		}
	} else {
		diff = (int)(size2 - size1);
		if (diff < 0) {
			memmove (data2, data1, size2);
		} else {
			memmove (data2, data1, size1);
			if (zero_size) {
				/* Implied 0 ('P's) */
				zero_size = cob_min_int (zero_size, diff);
				memset (data2 + size1, '0', (size_t)zero_size);
				diff -= zero_size;
			}
			if (diff) {
				/* Padding */
				memset (data2 + size1 + zero_size, ' ',
					(size_t)diff);
			}
		}
	}

	COB_PUT_SIGN (f1, sign);
}

static void
cob_move_alphanum_to_alphanum (cob_field *f1, cob_field *f2)
{
	unsigned char	*data1;
	unsigned char	*data2;
	size_t		size1;
	size_t		size2;

	data1 = f1->data;
	size1 = f1->size;
	data2 = f2->data;
	size2 = f2->size;
	if (size1 >= size2) {
		/* Move string with truncation */
		if (COB_FIELD_JUSTIFIED (f2)) {
			memmove (data2, data1 + size1 - size2, size2);
		} else {
			memmove (data2, data1, size2);
		}
	} else {
		/* Move string with padding */
		if (COB_FIELD_JUSTIFIED (f2)) {
			memset (data2, ' ', size2 - size1);
			memmove (data2 + size2 - size1, data1, size1);
		} else {
			memmove (data2, data1, size1);
			memset (data2 + size1, ' ', size2 - size1);
		}
	}
}

/* Binary Coded Decimal (BCD) - PACKED-DECIMAL (COMP-3) and COMP-6 */

void
cob_move_display_to_packed (cob_field *f1, cob_field *f2)
{
	unsigned char	*data1 = COB_FIELD_DATA (f1);
	const int		sign = COB_GET_SIGN_ADJUST (f1);
	const short		scale1 = COB_FIELD_SCALE (f1);
	const short		scale2 = COB_FIELD_SCALE (f2);
	unsigned short	 	digits1;
	unsigned short		digits2;
	register unsigned int	i;

	register unsigned char	*p;

	/* 99P -> 3 digits, scale -1 --> real digits are less */
	if (scale1 >= 0) {
		digits1 = COB_FIELD_DIGITS (f1);
	} else {
		digits1 = COB_FIELD_DIGITS (f1) + scale1;
	}
	if (scale2 >= 0) {
		digits2 = COB_FIELD_DIGITS (f2);
	} else {
		digits2 = COB_FIELD_DIGITS (f2) + scale2;
	}
	if (COB_FIELD_NO_SIGN_NIBBLE (f2)) {
		i = digits2 % 2;
	} else {
		i = 1 - (digits2 % 2);
	}

	/* skip not available positions */
	p = data1 + (digits1 - scale1) - (digits2 - scale2);
	while (p < data1) {
		p++; i++;	/* note: both p and i are digits */
	}

	/* zero out target, then transfer data */
	memset (f2->data, 0, f2->size);
	{
		register unsigned char	*q = f2->data + i / 2;
		const unsigned int i_end = digits2 + 1;
		/* FIXME: get rid of that, adjust i_end accordingly and always end at sign byte */
		const unsigned char *f2_end = f2->data + f2->size - 1;
		const unsigned char *p_end_calc = data1 + digits1;
		const unsigned char *p_end = p_end_calc > f2_end ? f2_end : p_end_calc;

		if ((i % 2) == 1) {
			*q++ = COB_D2I (*p++);
			i++;
		}
		/* note: for performance reasons we write "full bytes" only, this means that for COMP-3
		   we'll read 1 byte "too much = after" from the DISPLAY data;
		   it is believed that this won't raise a SIGBUS anywhere, but we will need to "clean"
		   the half-byte before setting the sign */

		/* check for necessary loop (until we not need the p_end check) */
		if (i_end - i < (unsigned int)(p_end - p + 1) / 2) {
			while (i <= i_end) {
				*q = (unsigned char) (*p << 4)	/* -> dropping the higher bits = no use in COB_D2I */
					+ COB_D2I (*(p + 1));
				q++;
				p += 2;
				i += 2;
			}
		} else {
			while (p < p_end) {
				*q = (unsigned char) (*p << 4)	/* -> dropping the higher bits = no use in COB_D2I */
					+ COB_D2I (*(p + 1));
				q++;
				p += 2;
			}
		}
	}

	COB_PUT_SIGN_ADJUSTED (f1, sign);

	if (COB_FIELD_NO_SIGN_NIBBLE (f2)) {
		return;
	}

	p = f2->data + f2->size - 1;	/* TODO: ending at the sign byte means we can drop that */
	if (!COB_FIELD_HAVE_SIGN (f2)) {
		*p |= 0x0F;
	} else if (sign < 0) {
		*p = (*p & 0xF0) | 0x0D;
	} else {
		*p = (*p & 0xF0) | 0x0C;
	}
}

void
cob_move_packed_to_display (cob_field *f1, cob_field *f2)
{
	unsigned char	buff[COB_MAX_DIGITS + 1];
	register unsigned char	*b = buff;
	register unsigned char	*d = f1->data;
	unsigned char	*d_end = d + f1->size - 1;
	const short	scale = COB_FIELD_SCALE (f1);
	unsigned short		digits;

	if (scale >= 0) {
		digits = COB_FIELD_DIGITS (f1);
	} else {
		/* 99P -> 3 digits, scale -1 --> real digits are less */
		digits = COB_FIELD_DIGITS (f1) + scale;
	}

	if (COB_FIELD_NO_SIGN_NIBBLE (f1)) {
		/* Unpack COMP-6 to string */
		const size_t offset = digits % 2;
		if (offset == 1) {
			*b++ = COB_I2D (*d++ & 0x0F);
		}
		while (d <= d_end) {
			*b++ = COB_I2D (*d >> 4);
			*b++ = COB_I2D (*d++ & 0x0F);
		}

		/* Store */
		store_common_region (f2, buff, digits, COB_FIELD_SCALE (f1), 1);
		COB_PUT_SIGN (f2, 0);
	} else {
		/* Unpack PACKED-DECIMAL / COMP-3 to integer */
		const size_t offset = 1 - digits % 2;
		if (offset == 1) {
			*b++ = COB_I2D (*d++ & 0x0F);
		}
		while (d < d_end) {
			*b++ = COB_I2D (*d >> 4);
			*b++ = COB_I2D (*d++ & 0x0F);
		}
		*b++ = COB_I2D (*d >> 4);

		/* Store */
		store_common_region (f2, buff, digits, COB_FIELD_SCALE (f1), 1);
		COB_PUT_SIGN (f2, ((*d & 0x0F) == 0x0D) ? -1 : 1);
	}

}

/* Floating point */

static void
cob_move_fp_to_fp (cob_field *src, cob_field *dst)
{
	const int src_type = COB_FIELD_TYPE (src);
	const int dst_type = COB_FIELD_TYPE (dst);

	long double	lfp;
	double	dfp;
	float	ffp;

	if (src_type == COB_TYPE_NUMERIC_FLOAT) {
		memmove ((void *)&ffp, src->data, sizeof (float));
		dfp = (double)ffp;
		lfp = ffp;
	} else if (src_type == COB_TYPE_NUMERIC_DOUBLE) {
		memmove ((void *)&dfp, src->data, sizeof (double));
		ffp = (float)dfp;
		lfp = dfp;
	} else {
		memmove ((void*)&lfp, src->data, sizeof (long double));
		dfp = (double)lfp;
		ffp = (float)dfp;
	}
	if (dst_type == COB_TYPE_NUMERIC_FLOAT) {
		memmove (dst->data, (void *)&ffp, sizeof (float));
	} else if (dst_type == COB_TYPE_NUMERIC_DOUBLE) {
		memmove (dst->data, (void *)&dfp, sizeof (double));
	} else{
		memmove (dst->data, (void *)&lfp, sizeof (long double));
	}
}

/* Binary integer */


/* move from one to the other binary field ignoring the scale
   --> these must match */
static void
cob_move_binary_to_binary (cob_field *f1, cob_field *f2)
{
	union {
		cob_u64_t		uval;
		cob_s64_t		sval;
	}		ul64;
	unsigned int	sign = 0;

	if (COB_FIELD_HAVE_SIGN (f1)) {
		ul64.sval = cob_binary_mget_sint64 (f1);
		if (ul64.sval < 0) {
			sign = 1;
		}
		if (COB_FIELD_BINARY_TRUNC (f2)) {
			/* while we "ignore" the scale for the value we possibly need
			   it for adjusting the digits for binary truncation */
			const short	scale = COB_FIELD_SCALE (f2);
			unsigned short		digits;
			if (scale >= 0) {
				digits = COB_FIELD_DIGITS (f2);
			} else {
				/* 99P -> 3 digits, scale -1 --> real digits are less */
				digits = COB_FIELD_DIGITS (f2) + scale;
			}
			ul64.sval %= cob_exp10_ll[digits];
		}
	} else {
		ul64.uval = cob_binary_mget_uint64 (f1);
		if (COB_FIELD_BINARY_TRUNC (f2)) {
			/* while we "ignore" the scale for the value we possibly need
			   it for adjusting the digits for binary truncation */
			const short	scale = COB_FIELD_SCALE (f2);
			unsigned short		digits;
			if (scale >= 0) {
				digits = COB_FIELD_DIGITS (f2);
			} else {
				/* 99P -> 3 digits, scale -1 --> real digits are less */
				digits = COB_FIELD_DIGITS (f2) + scale;
			}
			ul64.uval %= cob_exp10_ll[digits];
		}
	}
	if (COB_FIELD_HAVE_SIGN (f2)) {
		cob_binary_mset_sint64 (f2, ul64.sval);
	} else {
		if (sign) {
			cob_binary_mset_uint64 (f2, (cob_u64_t)(-ul64.sval));
		} else {
			cob_binary_mset_uint64 (f2, ul64.uval);
		}
	}
}

static void
cob_move_display_to_binary (cob_field *f1, cob_field *f2)
{
	const unsigned char	*data1 = COB_FIELD_DATA (f1);
	const size_t		size1 = COB_FIELD_SIZE (f1);
	const size_t	size = size1 - COB_FIELD_SCALE (f1) + COB_FIELD_SCALE (f2);
	cob_u64_t		val = 0;
	size_t		i;
	int			sign;
	unsigned short target_digits;
	
	/* truncate on request - by adjusting start position */
	if (COB_FIELD_BINARY_TRUNC (f2)) {
		const short	scale = COB_FIELD_SCALE (f2) ;
		const unsigned short	digits = COB_FIELD_DIGITS (f2) ;
		if (scale > digits) {
			/* PP9 -> scale 3, but we want only the last "real" digit */
			target_digits = digits;
		} else {
			/* 9PP -> scale -2, digits 3, we want only the first "real" digit */
			target_digits = scale > 0 ? digits : digits + scale;
		}
		if (target_digits > size) {
			target_digits = (unsigned short)size;
		}
	} else {
		target_digits = (unsigned short)size;
	}

	/* Skip leading zeros (and zero-like-data like space/low-value) */
	for (i = size - target_digits; i < size1; ++i) {
		if (COB_D2I (data1[i]) != 0) {
			break;
		}
		target_digits--;
	}

	if (target_digits > 19) {
		/* possible overflow - move with GMP */
		int opt = 0;
		if (COB_FIELD_BINARY_TRUNC (f2)
		 && !COB_FIELD_REAL_BINARY (f2)) {
			opt = COB_STORE_TRUNC_ON_OVERFLOW;
		}
		cob_decimal_setget_fld (f1, f2, opt);
		return;
	}

	/* Get value */
	sign = COB_GET_SIGN_ADJUST (f1);
	for ( ; i < size; ++i) {
		if (i < size1) {
			val = val * 10 + COB_D2I (data1[i]);
		} else {
			val *= 10;
		}
	}

	if (COB_FIELD_HAVE_SIGN (f2)) {
		cob_s64_t	val2;
		if (sign < 0) {
			val2 = (cob_s64_t)-1 * val;
		} else {
			val2 = (cob_s64_t)1 * val;
		}
		cob_binary_mset_sint64 (f2, val2);
	} else {
		cob_binary_mset_uint64 (f2, val);
	}

	COB_PUT_SIGN_ADJUSTED (f1, sign);
}

static void
cob_move_binary_to_display (cob_field *f1, cob_field *f2)
{
	cob_u64_t		val;
	int			i;
	int			sign;
	unsigned char	buff[32];

	sign = 1;
	/* Get value */
	if (COB_FIELD_HAVE_SIGN (f1)) {
		cob_s64_t		val2 = cob_binary_mget_sint64 (f1);
		if (val2 < 0) {
			sign = -1;
			val = (cob_u64_t)-1 * val2;
		} else {
			val = (cob_u64_t)1 * val2;
		}
	} else {
		val = cob_binary_mget_uint64 (f1);
	}

	/* Convert to string; note: we do this on ourself as this has proven
	   to be much faster than calling "sprintf (buff, CB_FMT_LLU, val)" */
	i = 20;
	while (val > 0) {
		buff[--i] = COB_I2D (val % 10);
		val /= 10;
	}

	/* Store */
	store_common_region (f2, buff + i, (size_t)20 - i, COB_FIELD_SCALE (f1), 1);

	COB_PUT_SIGN (f2, sign);
}

/* Edited */

/* create numeric edited field, note: non-display fields
   get "unpacked" first via indirect_move, then be edited
   from display using this function */ 
static void
cob_move_display_to_edited (cob_field *f1, cob_field *f2)
{
	register unsigned char	*dst = f2->data;
	register unsigned char	*src;
	const cob_pic_symbol	*p;
	unsigned char	*min = COB_FIELD_DATA (f1);
	unsigned char	*max = min + COB_FIELD_SIZE (f1);
	unsigned char	*end = f2->data + f2->size;
	unsigned char	*decimal_point = NULL;
	/* note: can't use the "adjust" variant here as we don't convert to digit
	   to explicit keep invalid data "as is";
	   CHECKME for using a buffer instead of original data */
	const int		sign = COB_GET_SIGN (f1);
	int		neg = (sign < 0) ? 1 : 0;
	int		count = 0;
	int		count_sign = 1;
	int		count_curr = 1;
	int		trailing_sign = 0;
	int		trailing_curr = 0;
	int		is_zero = 1;
	int		suppress_zero = 1;
	int		sign_first = 0;
	int		p_is_left = 0;
	int		has_b = 0;
	int		repeat;
	int		n;
	unsigned char	pad = ' ';
	unsigned char	x;
	unsigned char	c;
	unsigned char	sign_symbol = 0;
	unsigned char	curr_symbol = 0;
	const unsigned char	dec_symbol = COB_MODULE_PTR->decimal_point == ','
	                   	           ? ',' : '.';
	const unsigned char	currency = COB_MODULE_PTR->currency_symbol;
	int		floating_insertion = 0;
	unsigned char	*last_fixed_insertion_pos = NULL;
	unsigned char   last_fixed_insertion_char = '\0';

	/* Setup counters (only before decimal point) */
	/*
	  TO-DO: This is computed in cb_build_picture; add computed results to
	  cb_field / new overlay cb_field_edited and use those.
	*/
	for (p = COB_FIELD_PIC (f2); p->symbol; ++p) {
		c = p->symbol;
		repeat = p->times_repeated;
		if (c == '9' || c == 'Z' || c == '*') {
			count += repeat;
			count_sign = 0;
			count_curr = 0;
		} else if (count_curr && c == currency) {
			count += repeat;
		} else if (count_sign && (c == '+' || c == '-')) {
			count += repeat;
		} else if (c == 'P') {
			if (count == 0) {
				p_is_left = 1;
				break;
			} else {
				count += repeat;
				count_sign = 0;
				count_curr = 0;
			}
		} else if (c == 'V' || c == dec_symbol) {
			break;
		}
	}

	/* now insert data to destination */
	src = max - COB_FIELD_SCALE (f1) - count;
	for (p = COB_FIELD_PIC (f2); p->symbol; ++p) {
		c = p->symbol;
		n = p->times_repeated;
		for (; n > 0; n--, ++dst) {
			switch (c) {

			case '9':
				x = (min <= src && src < max) ? *src++ : (src++, '0');
				if (x != '0') {
					is_zero = 0;
				}
				suppress_zero = 0;
				trailing_sign = 1;
				trailing_curr = 1;
				*dst = x;
				break;

			case 'Z':
			case '*':
				x = (min <= src && src < max) ? *src++ : (src++, '0');
				if (x != '0') {
					is_zero = suppress_zero = 0;
				}
				trailing_sign = 1;
				trailing_curr = 1;
				pad = (c == '*') ? '*' : ' ';
				*dst = suppress_zero ? pad : x;
				break;

			case '+':
			case '-':
				x = (min <= src && src < max) ? *src++ : (src++, '0');
				if (x != '0') {
					is_zero = suppress_zero = 0;
				}
				if (trailing_sign) {
					/* Check negative and not zero */
					if (neg && !is_zero) {
						*dst = '-';
					} else if (c == '+') {
						*dst = '+';
					} else {
						*dst = ' ';
					}
					--end;
				} else if (dst == f2->data || suppress_zero) {
					*dst = pad;
					sign_symbol = c;
					if (!curr_symbol) {
						++sign_first;
					}
				} else {
					*dst = x;
				}

				if (n > 1 || last_fixed_insertion_char == c) {
					floating_insertion = 1;
				} else if (!trailing_sign) {
					if (last_fixed_insertion_pos) {
						*last_fixed_insertion_pos = last_fixed_insertion_char;
					}
					last_fixed_insertion_pos = dst;
					last_fixed_insertion_char = c;
				}
				break;

			case '.':
			case ',':
				if (c == dec_symbol) {
					*dst = dec_symbol;
					decimal_point = dst;
				} else {
					if (suppress_zero) {
						*dst = pad;
					} else {
						*dst = c;
					}
				}
				break;

			case 'V':
				--dst;
				decimal_point = dst;
				break;

			case '0':
			case '/':
				*dst = c;
				break;

			case 'B':
				if (suppress_zero) {
					*dst = pad;
				} else {
					*dst = 'B';
					has_b = 1;
				}
				break;

			case 'P':
				if (p_is_left) {
					++src;
					--dst;
				}
				break;

			case 'C':
			case 'D':
				end = dst;
				/* Check negative and not zero */
				if (neg && !is_zero) {
					if (c == 'C') {
						memcpy (dst, "CR", (size_t)2);
					} else {
						memcpy (dst, "DB", (size_t)2);
					}
				} else {
					memset (dst, ' ', (size_t)2);
				}
				dst++;
				break;

			default:
				/* LCOV_EXCL_START */
				if (c != currency) {
					/* should never happen, consider remove [also the reason for not translating that] */
					cob_runtime_error ("cob_move_display_to_edited: invalid PIC character %c", c);
					*dst = '?';	/* Invalid PIC */
					break;
				}
				/* LCOV_EXCL_STOP */

				x = (min <= src && src < max) ? *src++ : (src++, '0');
				if (x != '0') {
					is_zero = suppress_zero = 0;
				}
				if (trailing_curr) {
					*dst = currency;
					--end;
				} else if (dst == f2->data || suppress_zero) {
					*dst = pad;
					curr_symbol = currency;
				} else {
					*dst = x;
				}
				if (n > 1 || last_fixed_insertion_char == c) {
					floating_insertion = 1;
				} else if (!trailing_curr) {
					if (last_fixed_insertion_pos) {
						*last_fixed_insertion_pos = last_fixed_insertion_char;
					}
					last_fixed_insertion_pos = dst;
					last_fixed_insertion_char = c;
				}
				break;
			}
		}
	}

	if (sign_symbol) {
		/* Check negative and not zero */
		if (neg && !is_zero) {
			sign_symbol = '-';
		} else if (sign_symbol != '+') {
			sign_symbol = ' ';
		}
	}

	if (suppress_zero || (is_zero && COB_FIELD_BLANK_ZERO (f2))) {
		/* All digits are zeros */
		if (pad == ' ' || COB_FIELD_BLANK_ZERO (f2)) {
			memset (f2->data, ' ', f2->size);
		} else {
			for (dst = f2->data; dst < f2->data + f2->size; ++dst) {
				if (*dst != dec_symbol) {
					*dst = pad;
				}
			}
		}
	} else {
		/* Put zero after the decimal point if necessary */
		if (decimal_point) {
			for (dst = decimal_point + 1; dst < end; ++dst) {
				switch (*dst) {
				case '0':
				case '1':
				case '2':
				case '3':
				case '4':
				case '5':
				case '6':
				case '7':
				case '8':
				case '9':
#if 1	/* CHECKME: Why should we have a comma in here, necessary as shown in NIST NC,
     	   (TODO: add this to the internal testsuite, must fail if commented out)
     	   but not skip a period? */
				case ',':
				case '.':
#endif
				case '+':
				case '-':
				case '/':
				case 'B':
					break;
				default:
					*dst = '0';
				}
			}
		}

		/* Put sign or currency symbol at the beginning */
		if (sign_symbol || curr_symbol) {
			if (floating_insertion) {
				for (dst = end - 1; dst > f2->data; --dst) {
					if (*dst == ' ') {
						break;
					}
				}
				if (sign_symbol && curr_symbol) {
					/*
					  Only one of $ and +/- can be floating
					  in any given picture, so the symbol
					  which comes after the other must be
					  the one which floats.
					*/
					if (sign_first) {
						*dst = curr_symbol;
					} else {
						*dst = sign_symbol;
					}
				} else if (sign_symbol) {
					*dst = sign_symbol;
				} else {
					*dst = curr_symbol;
				}
			} else {
				if (last_fixed_insertion_char == currency) {
					*last_fixed_insertion_pos = curr_symbol;
				} else { /* + or - */
					*last_fixed_insertion_pos = sign_symbol;
				}
			}
		}

		/* Replace all leading 'B's by pad, others by space */
		if (has_b) {
			for (dst = f2->data; dst < end; ++dst) {
				if (*dst == 'B') {
					if (has_b) {
						*dst = pad;
					} else {
						*dst = ' ';
					}
				} else {
					has_b = 0;	/* non-starting characters seen */
				}
			}
		}
	}

	COB_PUT_SIGN (f1, sign);
}

static void
cob_move_edited_to_display (cob_field *f1, cob_field *f2)
{
	unsigned char	*p;
	unsigned char	*buff;
	size_t		i;
	int		sign = 0;
	int		scale = 0;
	int		count = 0;
	int		have_point = 0;
	unsigned char	cp;
	unsigned char	dec_pt;

	dec_pt = COB_MODULE_PTR->decimal_point;
	buff = cob_malloc (f1->size);
	p = buff;
	/* De-edit */
	for (i = 0; i < f1->size; ++i) {
		cp = f1->data[i];
		switch (cp) {
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			*p++ = cp;
			if (have_point) {
				++scale;
			}
			break;
		case '.':
		case ',':
			if (cp == dec_pt) {
				have_point = 1;
			}
			break;
		case '-':
		case 'C':
			sign = -1;
			break;
		}
	}
	/* Count number of digit places after decimal point in case of 'V', 'P' */
	if (scale == 0) {
		const cob_pic_symbol	*pic_symbol;
		for (pic_symbol = COB_FIELD_PIC (f1); pic_symbol->symbol; ++pic_symbol) {
			const unsigned char	c = pic_symbol->symbol;
			const int		n = pic_symbol->times_repeated;
			if (c == '9' || c == '0' || c == 'Z' || c == '*') {
				if (have_point) {
					scale += n;
				} else {
					count += n;
				}
			} else if (c == 'P') {
				if (count == 0) {
					have_point = 1;
					scale += n;
				} else {
					scale -= n;
				}
			} else if (c == 'V') {
				have_point = 1;
			}
		}
	}

	/* Store */
	store_common_region (f2, buff, (size_t)(p - buff), scale, 0);

	COB_PUT_SIGN (f2, sign);
	cob_free (buff);
}

static void
cob_move_alphanum_to_edited (cob_field *f1, cob_field *f2)
{
	const cob_pic_symbol	*p;
	unsigned char	*dst = f2->data;
	unsigned char	*src = COB_FIELD_DATA (f1);
	const unsigned char	*max = src + COB_FIELD_SIZE (f1);
	/* note: can't use the "adjust" variant here as we don't convert to digit;
	   CHECKME for using a buffer instead of original data or intermediate_move */
	const int	sign = COB_GET_SIGN (f1);

	for (p = COB_FIELD_PIC (f2); p->symbol; ++p) {
		const unsigned char	c = p->symbol;
		int 	n = p->times_repeated;
		for (; n > 0; --n) {
			switch (c) {
			case 'A':
			case 'X':
			case '9':
				*dst++ = (src < max) ? *src++ : ' ';
				break;
			case '0':
			case '/':
				*dst++ = c;
				break;
			case 'B':
				*dst++ = ' ';
				break;
			default:
				*dst++ = '?';	/* Invalid PIC */
			}
		}
	}

	COB_PUT_SIGN (f1, sign);
}

/* MOVE dispatcher */

static void
indirect_move (void (*func) (cob_field *src, cob_field *dst),
	       cob_field *src, cob_field *dst,
	       const size_t size, const int scale)
{
	cob_field	field;
	cob_field_attr	attr;

	if (size <= 2 * COB_MAX_DIGITS) {
		unsigned char buff[2 * COB_MAX_DIGITS] = { 0 };
		COB_FIELD_INIT (size, buff, &attr);
		COB_ATTR_INIT (COB_TYPE_NUMERIC_DISPLAY, (unsigned short) size, (short) scale,
				COB_FLAG_HAVE_SIGN, NULL);
		func (src, &field);
		cob_move (&field, dst);
	} else {
		COB_FIELD_INIT (size, cob_malloc (size), &attr);
		COB_ATTR_INIT (COB_TYPE_NUMERIC_DISPLAY, (unsigned short) size, (short) scale,
				COB_FLAG_HAVE_SIGN, NULL);
		func (src, &field);
		cob_move (&field, dst);
		cob_free (field.data);
	}
}

static void
cob_move_all (cob_field *src, cob_field *dst)
{
	unsigned char		*p;
	size_t			digcount;
	cob_field		temp;

	if (likely(COB_FIELD_IS_ALNUM (dst))) {
		if (likely(src->size == 1)) {
			memset (dst->data, src->data[0], dst->size);
		} else {
			size_t			i;
			digcount = src->size;
			for (i = 0; i < dst->size; ++i) {
				dst->data[i] = src->data[i % digcount];
			}
		}
		return;
	}
	if (!COB_FIELD_IS_NUMERIC (dst)) {
		/* note: this also applies to numeric-edited where a MOVE ALL
		         has to be an alphanumeric MOVE */
		temp.attr = &all_display_attr;
		digcount = dst->size;
	} else if (likely(src->size == 1)) {
		memset (all_numeric_data, src->data[0], COB_MAX_DIGITS);
		cob_move ((cob_field *)&all_numeric_field, dst);
		return;
	} else {
		temp.attr = &all_numeric_display_attr;
		digcount = COB_MAX_DIGITS;	/* CHECKME: when do we enter here? */
	}
	p = cob_malloc (digcount);
	temp.size = digcount;
	temp.data = p;
	if (likely(src->size == 1)) {
		/* most common: ALL 0 (or 9 or ...) -> fill data */
		memset (p, src->data[0], digcount);
	} else {
		/* possible: ALL 123   -> fill with pieces */
		size_t			i;
		/* TODO: use "doubled memmove" instead */
		for (i = 0; i < digcount; ++i) {
			p[i] = src->data[i % src->size];
		}
	}

	cob_move (&temp, dst);
	cob_free (p);
}

/*
 * Move data the same way as 'MVC' instruction on IBM works,
 * left to right, byte by byte
 */
void
cob_move_ibm (void *dst, void *src, const int len)
{
	register char	*dest = dst;
	register char	*srce = src;
	const char	*end = srce + len;
	while (srce != end) {
		*dest++ = *srce++;
	}
}

/*
 * Propagate table (1) throughout the table
 * (used by INITIALIZE)
 */
void
cob_init_table (void *tbl, const size_t len, const size_t occ)
{
	char	*m = (char*)tbl + len;
	size_t	i = len;
	size_t	j = 1;
	if (occ < 1)
		return;
	do {
		j = j * 2;
		memcpy ((void*)m, tbl, i);
		m = m + i;
		i = i * 2;
	} while ((j * 2) < occ);
	if (j < occ) {
		memcpy ((void*)m, tbl, len * (occ - j));
	}
}

void
cob_move (cob_field *src, cob_field *dst)
{
	int		opt;
	cob_field	temp;
	unsigned char	data[2];

	if (src == dst) {
		return;
	}
	if (dst->size == 0) {
		/* TODO: for dynamic sized items: allocate and go on */
		return;
	}
	if (unlikely (src->size == 0)) {
		temp.size = 1;
		temp.data = data;
		temp.attr = &const_alpha_attr;
		data[0] = ' ';
		data[1] = 0;
		src = &temp;
	}
	if (COB_FIELD_TYPE (src) == COB_TYPE_ALPHANUMERIC_ALL) {
		cob_move_all (src, dst);
		return;
	}

	/* Non-elementary move */
	if (COB_FIELD_TYPE (src) == COB_TYPE_GROUP
	 || COB_FIELD_TYPE (dst) == COB_TYPE_GROUP) {
		cob_move_alphanum_to_alphanum (src, dst);
		return;
	}

	opt = 0;
	if (COB_FIELD_TYPE (dst) == COB_TYPE_NUMERIC_BINARY) {
		if (COB_FIELD_BINARY_TRUNC (dst)
		 && !COB_FIELD_REAL_BINARY (dst)) {
			opt = COB_STORE_TRUNC_ON_OVERFLOW;
		}
	}

	/* Elementary move */
	switch (COB_FIELD_TYPE (src)) {
	case COB_TYPE_NUMERIC_DISPLAY:
		switch (COB_FIELD_TYPE (dst)) {
		case COB_TYPE_NUMERIC_FLOAT:
		case COB_TYPE_NUMERIC_DOUBLE:
		case COB_TYPE_NUMERIC_L_DOUBLE:
		case COB_TYPE_NUMERIC_FP_BIN32:
		case COB_TYPE_NUMERIC_FP_BIN64:
		case COB_TYPE_NUMERIC_FP_BIN128:
		case COB_TYPE_NUMERIC_FP_DEC64:
		case COB_TYPE_NUMERIC_FP_DEC128:
			cob_decimal_setget_fld (src, dst, 0);
			return;
		case COB_TYPE_NUMERIC_DISPLAY:
			cob_move_display_to_display (src, dst);
			return;
		case COB_TYPE_NUMERIC_PACKED:
			cob_move_display_to_packed (src, dst);
			return;
		case COB_TYPE_NUMERIC_BINARY:
		case COB_TYPE_NUMERIC_COMP5:
			cob_move_display_to_binary (src, dst);
			return;
		case COB_TYPE_NUMERIC_EDITED:
			cob_move_display_to_edited (src, dst);
			return;
		case COB_TYPE_ALPHANUMERIC_EDITED:
			if (COB_FIELD_SCALE (src) < 0
			 || COB_FIELD_SCALE (src) > COB_FIELD_DIGITS (src)) {
				/* Expand P's */
				indirect_move (cob_move_display_to_display, src, dst,
						(size_t)cob_max_int (COB_FIELD_DIGITS (src), COB_FIELD_SCALE (src)),
						cob_max_int (0, COB_FIELD_SCALE (src)));
				return;
			} else {
				cob_move_alphanum_to_edited (src, dst);
				return;
			}
		default:
			cob_move_display_to_alphanum (src, dst);
			return;
		}

	case COB_TYPE_NUMERIC_PACKED:
		switch (COB_FIELD_TYPE (dst)) {
		case COB_TYPE_NUMERIC_DISPLAY:
			cob_move_packed_to_display (src, dst);
			return;
		case COB_TYPE_NUMERIC_BINARY:
		case COB_TYPE_NUMERIC_COMP5:
#if 0		/* indirect move is more expensive, check it for improvements */
			if (opt == COB_STORE_TRUNC_ON_OVERFLOW) {
				/* note: "dst" is only possible when binary-trunc */
				indirect_move (cob_move_packed_to_display, src, dst,
						COB_FIELD_DIGITS (dst), COB_FIELD_SCALE (dst));
			} else {
				indirect_move (cob_move_packed_to_display, src, dst,
						COB_FIELD_DIGITS (src), COB_FIELD_SCALE (src));
			}
#else
			cob_decimal_setget_fld (src, dst, opt);
#endif
			return;
		case COB_TYPE_NUMERIC_PACKED:
			/* TODO: add handling of negative scales to cob_move_bcd */
			if (COB_FIELD_SCALE (src) >= 0
			 && COB_FIELD_SCALE (dst) >= 0) {
				cob_move_bcd (src, dst);
				return;
			}
		case COB_TYPE_NUMERIC_DOUBLE:
		case COB_TYPE_NUMERIC_FLOAT:
		case COB_TYPE_NUMERIC_L_DOUBLE:
		case COB_TYPE_NUMERIC_FP_BIN32:
		case COB_TYPE_NUMERIC_FP_BIN64:
		case COB_TYPE_NUMERIC_FP_BIN128:
		case COB_TYPE_NUMERIC_FP_DEC64:
		case COB_TYPE_NUMERIC_FP_DEC128:
			cob_decimal_setget_fld (src, dst, 0);
			return;
		default:
			indirect_move (cob_move_packed_to_display, src, dst,
					(size_t)(COB_FIELD_DIGITS(src)),
					COB_FIELD_SCALE (src));
			return;
		}

	case COB_TYPE_NUMERIC_BINARY:
	case COB_TYPE_NUMERIC_COMP5:
		switch (COB_FIELD_TYPE (dst)) {
		case COB_TYPE_NUMERIC_BINARY:
		case COB_TYPE_NUMERIC_COMP5:
			{
				const short	src_scale = COB_FIELD_SCALE (src);
				const short	dst_scale = COB_FIELD_SCALE (dst);
				unsigned short	digits;
				if (src_scale == dst_scale) {
					cob_move_binary_to_binary (src, dst);
					return;
				}
				if (src_scale >= 0) {
					digits = COB_FIELD_DIGITS (src);
				} else {
					/* 99P -> 3 digits, scale -1 --> real digits are less */
					digits = COB_FIELD_DIGITS (src) + src_scale;
				}
				if (digits < 19) {
					if (dst_scale <= 0) {
						const short digits_adjust = dst_scale - src_scale;
						if (digits_adjust < 0) {
							cob_field	field;
							cob_s64_t	val = cob_binary_mget_sint64 (src);
							val /= cob_exp10_ll[-digits_adjust];	/* adjust value to match target scale */
							COB_FIELD_INIT (sizeof (cob_s64_t), (unsigned char *)&val, &const_binll_attr);
							cob_move_binary_to_binary (&field, dst);
							return;
						}
						if (digits + digits_adjust < 19) {
							cob_field	field;
							cob_s64_t	val = cob_binary_mget_sint64 (src);
							val *= cob_exp10_ll[digits_adjust];	/* adjust value to match target scale */
							COB_FIELD_INIT (sizeof (cob_s64_t), (unsigned char *)&val, &const_binll_attr);
							cob_move_binary_to_binary (&field, dst);
							return;
						}
#if 0		/* indirect_move is much more expensive, check it for improvements */
					} else {
						if (opt == COB_STORE_TRUNC_ON_OVERFLOW) {
							/* note: "dst" is only possible when binary-trunc */
							indirect_move (cob_move_binary_to_display, src, dst,
									COB_FIELD_DIGITS (dst), COB_FIELD_SCALE (dst));
						} else {
							indirect_move (cob_move_binary_to_display, src, dst,
									COB_FIELD_DIGITS (src), COB_FIELD_SCALE (src));
						}
#endif
					}
				}
				cob_decimal_setget_fld (src, dst, opt);
				return;
			}
		case COB_TYPE_NUMERIC_DISPLAY:
			cob_move_binary_to_display (src, dst);
			return;
		case COB_TYPE_NUMERIC_PACKED:
			{
				const short src_digits = COB_FIELD_DIGITS (src);
				if (src_digits < 19) {
					const short src_scale = COB_FIELD_SCALE (src);
					const short dst_scale = COB_FIELD_SCALE (dst);
					const short diff_scale = src_scale - dst_scale;
					if (src_digits - diff_scale < 19) {
						cob_s64_t	val = cob_binary_mget_sint64 (src);
						if (diff_scale <= 0) {
							val *= cob_exp10_ll[-diff_scale];
						} else {
							val /= cob_exp10_ll[diff_scale];
						}
						if (val >= INT_MIN && val <= INT_MAX) {
							cob_set_packed_int (dst, (int)val);
							return;
						}
					}
				}
			}
			cob_decimal_setget_fld (src, dst, 0);
			return;
		case COB_TYPE_NUMERIC_DOUBLE:
		case COB_TYPE_NUMERIC_FLOAT:
		case COB_TYPE_NUMERIC_L_DOUBLE:
		case COB_TYPE_NUMERIC_FP_BIN32:
		case COB_TYPE_NUMERIC_FP_BIN64:
		case COB_TYPE_NUMERIC_FP_BIN128:
		case COB_TYPE_NUMERIC_FP_DEC64:
		case COB_TYPE_NUMERIC_FP_DEC128:
			cob_decimal_setget_fld (src, dst, 0);
			return;
		case COB_TYPE_NUMERIC_EDITED:
			indirect_move (cob_move_binary_to_display, src, dst,
					(size_t)COB_MAX_DIGITS,
					COB_FIELD_SCALE (src));
			return;
		default:
			indirect_move (cob_move_binary_to_display, src, dst,
					(size_t)(COB_FIELD_DIGITS(src)),
					COB_FIELD_SCALE (src));
			return;
		}

	case COB_TYPE_NUMERIC_EDITED:
		switch (COB_FIELD_TYPE (dst)) {
		case COB_TYPE_NUMERIC_DISPLAY:
			cob_move_edited_to_display (src, dst);
			return;
		case COB_TYPE_NUMERIC_PACKED:
		case COB_TYPE_NUMERIC_BINARY:
		case COB_TYPE_NUMERIC_COMP5:
		case COB_TYPE_NUMERIC_EDITED:
			indirect_move (cob_move_edited_to_display, src, dst,
					(size_t)(2 * COB_MAX_DIGITS),
					COB_MAX_DIGITS);
			return;
		case COB_TYPE_NUMERIC_FLOAT:
		case COB_TYPE_NUMERIC_DOUBLE:
		case COB_TYPE_NUMERIC_L_DOUBLE:
		case COB_TYPE_NUMERIC_FP_BIN32:
		case COB_TYPE_NUMERIC_FP_BIN64:
		case COB_TYPE_NUMERIC_FP_BIN128:
		case COB_TYPE_NUMERIC_FP_DEC64:
		case COB_TYPE_NUMERIC_FP_DEC128:
			cob_decimal_setget_fld (src, dst, 0);
			return;
		case COB_TYPE_ALPHANUMERIC_EDITED:
			cob_move_alphanum_to_edited (src, dst);
			return;
		default:
			cob_move_alphanum_to_alphanum (src, dst);
			return;
		}

	case COB_TYPE_NUMERIC_FLOAT:
		switch (COB_FIELD_TYPE (dst)) {
		case COB_TYPE_NUMERIC_FLOAT:
			memmove (dst->data, src->data, sizeof(float));
			return;
		case COB_TYPE_NUMERIC_DOUBLE:
		case COB_TYPE_NUMERIC_L_DOUBLE:
			cob_move_fp_to_fp (src, dst);
			return;
		case COB_TYPE_NUMERIC_BINARY:
		case COB_TYPE_NUMERIC_COMP5:
			cob_decimal_setget_fld (src, dst, opt);
			return;
		case COB_TYPE_NUMERIC_PACKED:
		case COB_TYPE_NUMERIC_DISPLAY:
		case COB_TYPE_NUMERIC_FP_BIN32:
		case COB_TYPE_NUMERIC_FP_BIN64:
		case COB_TYPE_NUMERIC_FP_BIN128:
		case COB_TYPE_NUMERIC_FP_DEC64:
		case COB_TYPE_NUMERIC_FP_DEC128:
			cob_decimal_setget_fld (src, dst, 0);
			return;
		default:
			cob_decimal_move_temp (src, dst);
			return;
		}

	case COB_TYPE_NUMERIC_DOUBLE:
		switch (COB_FIELD_TYPE (dst)) {
		case COB_TYPE_NUMERIC_DOUBLE:
			memmove (dst->data, src->data, sizeof(double));
			return;
		case COB_TYPE_NUMERIC_FLOAT:
		case COB_TYPE_NUMERIC_L_DOUBLE:
			cob_move_fp_to_fp (src, dst);
			return;
		case COB_TYPE_NUMERIC_BINARY:
		case COB_TYPE_NUMERIC_COMP5:
			cob_decimal_setget_fld (src, dst, opt);
			return;
		case COB_TYPE_NUMERIC_PACKED:
		case COB_TYPE_NUMERIC_DISPLAY:
		case COB_TYPE_NUMERIC_FP_BIN32:
		case COB_TYPE_NUMERIC_FP_BIN64:
		case COB_TYPE_NUMERIC_FP_BIN128:
		case COB_TYPE_NUMERIC_FP_DEC64:
		case COB_TYPE_NUMERIC_FP_DEC128:
			cob_decimal_setget_fld (src, dst, 0);
			return;
		default:
			cob_decimal_move_temp (src, dst);
			return;
		}

	case COB_TYPE_NUMERIC_L_DOUBLE:
		switch (COB_FIELD_TYPE (dst)) {
		case COB_TYPE_NUMERIC_L_DOUBLE:
			memmove (dst->data, src->data, sizeof(double));
			return;
		case COB_TYPE_NUMERIC_DOUBLE:
		case COB_TYPE_NUMERIC_FLOAT:
			cob_move_fp_to_fp (src, dst);
			return;
		case COB_TYPE_NUMERIC_BINARY:
		case COB_TYPE_NUMERIC_COMP5:
			cob_decimal_setget_fld (src, dst, opt);
			return;
		case COB_TYPE_NUMERIC_PACKED:
		case COB_TYPE_NUMERIC_DISPLAY:
		case COB_TYPE_NUMERIC_FP_BIN32:
		case COB_TYPE_NUMERIC_FP_BIN64:
		case COB_TYPE_NUMERIC_FP_BIN128:
		case COB_TYPE_NUMERIC_FP_DEC64:
		case COB_TYPE_NUMERIC_FP_DEC128:
			cob_decimal_setget_fld (src, dst, 0);
			return;
		default:
			cob_decimal_move_temp (src, dst);
			return;
		}

	case COB_TYPE_NUMERIC_FP_DEC64:
		switch (COB_FIELD_TYPE (dst)) {
		case COB_TYPE_NUMERIC_BINARY:
		case COB_TYPE_NUMERIC_COMP5:
			cob_decimal_setget_fld (src, dst, opt);
			return;
		case COB_TYPE_NUMERIC_FP_DEC64:
			memmove (dst->data, src->data, (size_t)8);
			return;
		case COB_TYPE_NUMERIC_FLOAT:
		case COB_TYPE_NUMERIC_DOUBLE:
		case COB_TYPE_NUMERIC_L_DOUBLE:
		case COB_TYPE_NUMERIC_PACKED:
		case COB_TYPE_NUMERIC_DISPLAY:
		case COB_TYPE_NUMERIC_FP_BIN32:
		case COB_TYPE_NUMERIC_FP_BIN128:
		case COB_TYPE_NUMERIC_FP_DEC128:
			cob_decimal_setget_fld (src, dst, 0);
			return;
		default:
			cob_decimal_move_temp (src, dst);
			return;
		}
	case COB_TYPE_NUMERIC_FP_DEC128:
		switch (COB_FIELD_TYPE (dst)) {
		case COB_TYPE_NUMERIC_BINARY:
		case COB_TYPE_NUMERIC_COMP5:
			cob_decimal_setget_fld (src, dst, opt);
			return;
		case COB_TYPE_NUMERIC_FP_DEC128:
			memmove (dst->data, src->data, (size_t)16);
			return;
		case COB_TYPE_NUMERIC_FLOAT:
		case COB_TYPE_NUMERIC_DOUBLE:
		case COB_TYPE_NUMERIC_L_DOUBLE:
		case COB_TYPE_NUMERIC_PACKED:
		case COB_TYPE_NUMERIC_DISPLAY:
		case COB_TYPE_NUMERIC_FP_BIN32:
		case COB_TYPE_NUMERIC_FP_BIN64:
		case COB_TYPE_NUMERIC_FP_BIN128:
		case COB_TYPE_NUMERIC_FP_DEC64:
			cob_decimal_setget_fld (src, dst, 0);
			return;
		default:
			cob_decimal_move_temp (src, dst);
			return;
		}
	default:
		switch (COB_FIELD_TYPE (dst)) {
		case COB_TYPE_NUMERIC_DISPLAY:
			cob_move_alphanum_to_display (src, dst);
			return;
		case COB_TYPE_NUMERIC_PACKED:
		case COB_TYPE_NUMERIC_BINARY:
		case COB_TYPE_NUMERIC_COMP5:
		case COB_TYPE_NUMERIC_EDITED:
			indirect_move (cob_move_alphanum_to_display, src, dst,
					(size_t)(2 * COB_MAX_DIGITS),
					COB_MAX_DIGITS);
			return;
		case COB_TYPE_NUMERIC_FLOAT:
		case COB_TYPE_NUMERIC_DOUBLE:
		case COB_TYPE_NUMERIC_L_DOUBLE:
		case COB_TYPE_NUMERIC_FP_BIN32:
		case COB_TYPE_NUMERIC_FP_BIN64:
		case COB_TYPE_NUMERIC_FP_BIN128:
		case COB_TYPE_NUMERIC_FP_DEC64:
		case COB_TYPE_NUMERIC_FP_DEC128:
			cob_decimal_setget_fld (src, dst, 0);
			return;
		case COB_TYPE_ALPHANUMERIC_EDITED:
			cob_move_alphanum_to_edited (src, dst);
			return;
		default:
			cob_move_alphanum_to_alphanum (src, dst);
			return;
		}
	}
}

/* Convenience functions */

static int
cob_packed_get_int (cob_field *field)
{
	register int	val;
	register unsigned char *d = field->data;
	register unsigned char *d_end = d + field->size - 1;

	if (COB_FIELD_NO_SIGN_NIBBLE (field)) {
		/* Unpack COMP-6 to integer */
		const size_t	offset = COB_FIELD_DIGITS (field) % 2;
		if (offset == 1) {
			val = *d++ & 0x0F;
		} else {
			val = 0;
		}
		while (d <= d_end) {
			val = val * 10
			    + (*d >> 4);
			val = val * 10
			    + (*d++ & 0x0F);
		}
	} else {
		/* Unpack PACKED-DECIMAL / COMP-3 to integer */
		const size_t	offset = 1 - COB_FIELD_DIGITS (field) % 2;
		if (offset == 1) {
			val = *d++ & 0x0F;
		} else {
			val = 0;
		}
		while (d < d_end) {
			val = val * 10
			    + (*d >> 4);
			val = val * 10
			    + (*d++ & 0x0F);
		}
		val = val * 10
		    + (*d >> 4);
		if ((*d & 0x0F) == 0x0D) {
			val = -val;
		}
	}
	return val;	
}

static cob_s64_t
packed_get_long_long (cob_field *field)
{
	const short	scale = COB_FIELD_SCALE (field);
	register cob_s64_t		val;
	register unsigned char	*d = field->data;
	register unsigned char	*d_end = d + field->size - 1;

	if (COB_FIELD_NO_SIGN_NIBBLE (field)) {
		/* Unpack COMP-6 to integer */
		const size_t	offset = COB_FIELD_DIGITS (field) % 2;
		if (offset == 1) {
			val = *d++ & 0x0F;
		} else {
			val = 0;
		}
		while (d <= d_end) {
			val = val * 10
			    + (*d >> 4);
			val = val * 10
			    + (*d++ & 0x0F);
		}
	} else {
		/* Unpack PACKED-DECIMAL / COMP-3 to integer */
		const size_t	offset = 1 - COB_FIELD_DIGITS (field) % 2;
		if (offset == 1) {
			val = *d++ & 0x0F;
		} else {
			val = 0;
		}
		while (d < d_end) {
			val = val * 10
			    + (*d >> 4);
			val = val * 10
			    + (*d++ & 0x0F);
		}
		val = val * 10
		    + (*d >> 4);
		if ((*d & 0x0F) == 0x0D) {
			val = -val;
		}
	}
	if (scale < 0) {
		val *= cob_exp10_ll[-scale];
	} else {
		val /= cob_exp10_ll[scale];
	}
	return val;
}

static int
cob_display_get_int (cob_field *f)
{
	const short	scale = COB_FIELD_SCALE (f);
	const unsigned char	*data = COB_FIELD_DATA (f);
	const int		sign = COB_GET_SIGN_ADJUST (f);
	size_t		size = COB_FIELD_SIZE (f);
	size_t		i;
	int		val = 0;

	/* Skip leading zeros (and zero-like-data like space/low-value) */
	for (i = 0; i < size; ++i) {
		if (COB_D2I (data[i]) != 0) {
			break;
		}
	}

	/* Get value */
	if (scale < 0) {
		for (; i < size; ++i) {
			val = val * 10 + COB_D2I (data[i]);
		}
		val *= cob_exp10[-scale];
	} else {
		size -= scale;
		for (; i < size; ++i) {
			val = val * 10 + COB_D2I (data[i]);
		}
	}
	if (sign < 0) {
		val = -val;
	}

	COB_PUT_SIGN_ADJUSTED (f, sign);
	return val;
}

static cob_s64_t
display_get_long_long (cob_field *f)
{
	const short	scale = COB_FIELD_SCALE (f);
	const unsigned char	*data = COB_FIELD_DATA (f);
	const int		sign = COB_GET_SIGN_ADJUST (f);
	size_t		size = COB_FIELD_SIZE (f);
	size_t		i;
	cob_s64_t	val = 0;

	/* Skip leading zeros (and zero-like-data like space/low-value) */
	for (i = 0; i < size; ++i) {
		if (COB_D2I (data[i]) != 0) {
			break;
		}
	}

	/* Get value */
	if (scale < 0) {
		for (; i < size; ++i) {
			val = val * 10 + COB_D2I (data[i]);
		}
		val *= cob_exp10_ll[-scale];
	} else {
		size -= scale;
		for (; i < size; ++i) {
			val = val * 10 + COB_D2I (data[i]);
		}
	}
	if (sign < 0) {
		val = -val;
	}

	COB_PUT_SIGN_ADJUSTED (f, sign);
	return val;
}

void
cob_set_int (cob_field *f, const int n)
{
	cob_field	field;
	COB_FIELD_INIT (sizeof (int), (unsigned char *)&n, &const_bin_attr);
	cob_move (&field, f);
}

/* note: removes decimal part - per design */
int
cob_get_int (cob_field *f)
{
	switch (COB_FIELD_TYPE (f)) {
	case COB_TYPE_NUMERIC_DISPLAY:
		return cob_display_get_int (f);
	case COB_TYPE_NUMERIC_PACKED:
		return cob_packed_get_int (f);
	case COB_TYPE_NUMERIC_BINARY:
	case COB_TYPE_NUMERIC_COMP5:
		{
			cob_s64_t	val = cob_binary_mget_sint64 (f);
			const short	scale = COB_FIELD_SCALE (f);
			if (scale == 0) {
				return (int)val;		/* 999 */
			}
			if (scale > 0) {
				return (int)(val / cob_exp10_ll[scale]);	/* 999v9 */
			}
			return (int)(val * cob_exp10_ll[-scale]);	/* 999PP */
		}
	default:
		{
			cob_field	field;
			int		val;
			COB_FIELD_INIT (sizeof (int), (unsigned char *)&val, &const_bin_attr);
			cob_move (f, &field);
			return val;
		}
	}
}

void
cob_set_llint (cob_field *f, const cob_s64_t n)
{
	cob_field	field;
	COB_FIELD_INIT (sizeof (cob_s64_t), (unsigned char *)&n, &const_binll_attr);
	cob_move (&field, f);
}


/* note: removes decimal part - per design */
cob_s64_t
cob_get_llint (cob_field *f)
{
	switch (COB_FIELD_TYPE (f)) {
	case COB_TYPE_NUMERIC_DISPLAY:
		return display_get_long_long (f);
	case COB_TYPE_NUMERIC_BINARY:
	case COB_TYPE_NUMERIC_COMP5:
		{
			cob_s64_t	val = cob_binary_mget_sint64 (f);
			const short	scale = COB_FIELD_SCALE (f);
			if (scale == 0) {
				return val;		/* 999 */
			}
			if (scale > 0) {
				return val / cob_exp10_ll[scale];	/* 999v9 */
			}
			return val * cob_exp10_ll[-scale];	/* 999PP */
		}
	case COB_TYPE_NUMERIC_PACKED:
		return packed_get_long_long (f);
	default:
		{
			cob_field	field;
			cob_s64_t	val;
			COB_FIELD_INIT (sizeof (cob_s64_t), (unsigned char *)&val, &const_binll_attr);
			cob_move (f, &field);
			return val;
		}
	}
}

void
cob_init_move (cob_global *lptr, cob_settings *sptr)
{
	cobglobptr = lptr;
	cobsetptr  = sptr;
}

/*
 * Routines for C application code to access COBOL data follow
 */
void
cob_put_u64_compx (cob_u64_t val, void *mem, int len)
{
#if !defined(WORDS_BIGENDIAN)
	cob_u64_t	ulong;
#endif
	cob_u32_t	uint;
	cob_u16_t	ushort;
	
#ifdef WORDS_BIGENDIAN
	switch (len) {
	case sizeof(int):
		uint = ((cob_u32_t)val);
		memcpy (mem, ((cob_u8_t*)&uint), sizeof(int));
		return;
	default:	/* Assume 64 bit value */
	case sizeof(cob_s64_t):
		memcpy (mem, ((cob_u8_t*)&val), sizeof(cob_s64_t));
		return;
	case sizeof(short):
		ushort = ((cob_u16_t)val);
		memcpy (mem, ((cob_u8_t*)&ushort), sizeof(short));
		return;
	case 1:
		*((cob_u8_t*)mem) = ((cob_u8_t)val);
		return;
	case 3:
	case 5:
	case 6:
	case 7:
		memcpy (mem, ((cob_u8_t*)&val) + (sizeof(cob_s64_t) - len), len);
	}
#else
	switch (len) {
	case sizeof(int):
		uint = COB_BSWAP_32 ((cob_u32_t)val);
		memcpy (mem, ((cob_u8_t*)&uint), sizeof(int));
		return;
	default:	/* Assume 64 bit value */
	case sizeof(cob_s64_t):
		ulong = COB_BSWAP_64 ((cob_u64_t)val);
		memcpy (mem, ((cob_u8_t*)&ulong), sizeof(cob_s64_t));
		return;
	case sizeof(short):
		ushort = COB_BSWAP_16 ((cob_u16_t)val);
		memcpy (mem, ((cob_u8_t*)&ushort), sizeof(short));
		return;
	case 1:
		*((cob_u8_t*)mem) = ((cob_u8_t)val);
		return;
	case 3:
	case 5:
	case 6:
	case 7:
		ulong = COB_BSWAP_64 (val);
		memcpy (mem, ((cob_u8_t*)&ulong) + (sizeof(cob_s64_t) - len), len);
	}
#endif
}

void
cob_put_u64_comp5 (cob_u64_t val, void *mem, int len)
{
	cob_u32_t	uint;
	cob_u16_t	ushort;
	switch (len) {
	case sizeof(int):
		uint = ((cob_u32_t)val);
		memcpy (mem, ((cob_u8_t*)&uint), sizeof(int));
		return;
	default:	/* Assume 64 bit value */
	case sizeof(cob_s64_t):
		memcpy (mem, ((cob_u8_t*)&val), sizeof(cob_s64_t));
		return;
	case sizeof(short):
		ushort = ((cob_u16_t)val);
		memcpy (mem, ((cob_u8_t*)&ushort), sizeof(short));
		return;
	case 1:
		*((cob_u8_t*)mem) = ((cob_u8_t)val);
		return;
	case 3:
	case 5:
	case 6:
	case 7:
		break;
	}
#if defined(WORDS_BIGENDIAN)
	memcpy (mem, ((cob_u8_t*)&val) + (sizeof(cob_s64_t) - len), len);
#else
	memcpy (mem, ((cob_u8_t*)&val), len);
#endif
}

void
cob_put_s64_compx (cob_s64_t val, void *mem, int len)
{
#if !defined(WORDS_BIGENDIAN)
	cob_s64_t	slong;
#endif
	cob_s32_t	sint;
	cob_s16_t	sshort;
#if defined(WORDS_BIGENDIAN)
	switch (len) {
	case sizeof(int):
		sint = ((cob_s32_t)val);
		memcpy(mem,((cob_u8_t*)&sint),sizeof(int));
		return;
	default:	/* Assume 64 bit value */
	case sizeof(cob_s64_t):
		memcpy(mem,((cob_u8_t*)&val),sizeof(cob_s64_t));
		return;
	case sizeof(short):
		sshort = ((cob_s16_t)val);
		memcpy(mem,((cob_u8_t*)&sshort),sizeof(short));
		return;
	case 1:
		*((cob_s8_t*)mem) = ((cob_s8_t)val);
		return;
	case 3:
	case 5:
	case 6:
	case 7:
		memcpy(mem,((cob_u8_t*)&val)+(sizeof(cob_s64_t)-len),len);
	}
#else
	switch (len) {
	case sizeof(int):
		sint = COB_BSWAP_32 ((cob_s32_t)val);
		memcpy(mem,((cob_u8_t*)&sint),sizeof(int));
		return;
	default:	/* Assume 64 bit value */
	case sizeof(cob_s64_t):
		slong = COB_BSWAP_64 ((cob_s64_t)val);
		memcpy(mem,((cob_u8_t*)&slong),sizeof(cob_s64_t));
		return;
	case sizeof(short):
		sshort = COB_BSWAP_16 ((cob_s16_t)val);
		memcpy(mem,((cob_u8_t*)&sshort),sizeof(short));
		return;
	case 1:
		*((cob_s8_t*)mem) = ((cob_s8_t)val);
		return;
	case 3:
	case 5:
	case 6:
	case 7:
		slong = COB_BSWAP_64 (val);
		memcpy(mem,((cob_u8_t*)&slong)+(sizeof(cob_s64_t)-len),len);
	}
#endif
	return;
}

void
cob_put_s64_comp5 (cob_s64_t val, void *mem, int len)
{
	cob_s32_t	sint;
	cob_s16_t	sshort;
	switch (len) {
	case sizeof(int):
		sint = ((cob_s32_t)val);
		memcpy (mem, ((cob_u8_t*)&sint), sizeof(int));
		return;
	default:	/* Assume 64 bit value */
	case sizeof(cob_s64_t):
		memcpy (mem, ((cob_u8_t*)&val), sizeof(cob_s64_t));
		return;
	case sizeof(short):
		sshort = ((cob_u16_t)val);
		memcpy (mem, ((cob_u8_t*)&sshort), sizeof(short));
		return;
	case 1:
		*((cob_u8_t*)mem) = ((cob_u8_t)val);
		return;
	case 3:
	case 5:
	case 6:
	case 7:
		break;
	}
#if defined(WORDS_BIGENDIAN)
	memcpy (mem, ((cob_u8_t*)&val) + (sizeof(cob_s64_t) - len), len);
#else
	memcpy (mem, ((cob_u8_t*)&val), len);
#endif
}

cob_u64_t
cob_get_u64_compx (void *mem, int len)
{
	cob_u64_t	ulong;
	cob_u32_t	uint;
	cob_u16_t	ushort;
#if defined(WORDS_BIGENDIAN)
	switch (len) {
	case sizeof(int):
		memcpy (((cob_u8_t*)&uint), mem, sizeof(int));
		return uint;
	default:	/* Assume 64 bit value */
	case sizeof(cob_s64_t):
		memcpy (((cob_u8_t*)&ulong), mem, sizeof(cob_s64_t));
		return ulong;
	case sizeof(short):
		memcpy (((cob_u8_t*)&ushort), mem, sizeof(short));
		return ushort;
	case 1:
		return *((cob_u8_t*)mem);
	case 3:
	case 5:
	case 6:
	case 7:
		break;
	}
	ulong = 0;
	memcpy (((cob_u8_t*)&ulong) + (sizeof(cob_s64_t) - len), mem, len);
	return ulong;
#else
	switch (len) {
	case sizeof(int):
		memcpy (((cob_u8_t*)&uint), mem, sizeof(int));
		return COB_BSWAP_32(uint);
	default:	/* Assume 64 bit value */
	case sizeof(cob_s64_t):
		memcpy (((cob_u8_t*)&ulong), mem, sizeof(cob_s64_t));
		return COB_BSWAP_64(ulong);
	case sizeof(short):
		memcpy (((cob_u8_t*)&ushort), mem, sizeof(short));
		return COB_BSWAP_16(ushort);
	case 1:
		return (*(cob_u8_t*)mem);
	case 3:
	case 5:
	case 6:
	case 7:
		break;
	}
	ulong = 0;
	memcpy (((cob_u8_t*)&ulong) + (sizeof(cob_s64_t) - len), mem, len);
	return COB_BSWAP_64(ulong);
#endif
}

cob_u64_t
cob_get_u64_comp5 (void *mem, int len)
{
	cob_u64_t	ulong;
	cob_u32_t	uint;
	cob_u16_t	ushort;
	switch (len) {
	case sizeof(int):
		memcpy (((cob_u8_t*)&uint), mem, sizeof(int));
		return uint;
	default:	/* Assume 64 bit value */
	case sizeof(cob_s64_t):
		memcpy (((cob_u8_t*)&ulong), mem, sizeof(cob_s64_t));
		return ulong;
	case sizeof(short):
		memcpy (((cob_u8_t*)&ushort), mem, sizeof(short));
		return ushort;
	case 1:
		return *((cob_u8_t*)mem);
	case 3:
	case 5:
	case 6:
	case 7:
		break;
	}
	ulong = 0;
#if defined(WORDS_BIGENDIAN)
	memcpy (((cob_u8_t*)&ulong) + (sizeof(cob_s64_t) - len), mem, len);
#else
	memcpy (((cob_u8_t*)&ulong), mem, len);
#endif
	return ulong;
}

cob_s64_t
cob_get_s64_comp5 (void *mem, int len)
{
	cob_s64_t	slong;
	int		sint;
	short		sshort;
	switch (len) {
	case sizeof(int):
		memcpy (((void *)&sint), mem, sizeof(int));
		return sint;
	default:	/* Assume 64 bit value */
	case sizeof(cob_s64_t):
		memcpy (((void*)&slong), mem, sizeof(cob_s64_t));
		return slong;
	case sizeof(short):
		memcpy (((void*)&sshort), mem, sizeof(short));
		return sshort;
	case 1:
		return *((signed char*)mem);
	case 3:
	case 5:
	case 6:
	case 7:
		break;
	}
	slong = 0;
#if defined(WORDS_BIGENDIAN)
	if (((cob_u8_t*)mem)[0] & 0x80)	{	/* Negative value */
		slong = -1;
	}
	memcpy (((cob_u8_t*)&slong) + (sizeof(cob_s64_t) - len), mem, len);
#else
	if (((cob_u8_t*)mem)[len - 1] & 0x80) {	/* Negative value; 2s complement */
		slong = -1;
	}
	memcpy (((void*)&slong), mem, len);
#endif
	return slong;
}

cob_s64_t
cob_get_s64_compx (void *mem, int len)
{
	cob_s64_t	slong;
	int		sint;
	short		sshort;
	
#if defined(WORDS_BIGENDIAN)
	switch (len) {
	case sizeof(int):
		memcpy (((cob_u8_t*)&sint), mem, sizeof(int));
		return sint;
	default:	/* Assume 64 bit value */
	case sizeof(cob_s64_t):
		memcpy (((cob_u8_t*)&slong), mem, sizeof(cob_s64_t));
		return slong;
	case sizeof(short):
		memcpy (((cob_u8_t*)&sshort), mem, sizeof(short));
		return sshort;
	case 1:
		return *((signed char*)mem);
	case 3:
	case 5:
	case 6:
	case 7:
		break;
	}
	slong = 0;
	if (((cob_u8_t*)mem)[0] & 0x80) {	/* Negative value */
		slong = -1;
	}
	memcpy (((cob_u8_t*)&slong) + (sizeof(cob_s64_t) - len), mem, len);
	return slong;
#else
	switch (len) {
	case sizeof(int):
		memcpy (((cob_u8_t*)&sint), mem, sizeof(int));
		sint = COB_BSWAP_32(sint);
		return (cob_s64_t)sint;
	default:	/* Assume 64 bit value */
	case sizeof(cob_s64_t):
		memcpy (((cob_u8_t*)&slong), mem, sizeof(cob_s64_t));
		slong =  COB_BSWAP_64(slong);
		return (cob_s64_t)slong;
	case sizeof(short):
		memcpy (((cob_u8_t*)&sshort), mem, sizeof(short));
		sshort = COB_BSWAP_16(sshort);
		return (cob_s64_t)(sshort);
	case 1:
		return (*(signed char*)mem);
	case 3:
	case 5:
	case 6:
	case 7:
		break;
	}
	slong = 0;
	if (((cob_u8_t*)mem)[0] & 0x80)	{ /* Negative value; 2s complement */
		slong = -1;
	}
	memcpy (((cob_u8_t*)&slong) + (sizeof(cob_s64_t) - len), mem, len);
	return (cob_s64_t)COB_BSWAP_64 (slong);
#endif
}

void
cob_put_s64_comp3 (cob_s64_t val, void *mem, int len)
{
	int		sign, dig1, dig2;
	cob_s64_t	num;
	cob_u8_t	*p = mem;

	if (val < 0) {
		num = -val;
		sign = 0x0D;
	} else {
		num = val;
		sign = 0x0C;
	}
	memset (mem, 0, len);
	p[--len] =  (cob_u8_t)((((num % 10) << 4) | sign) & 0xFF);
	num /= 10;
	while (num > 0
	    && len-- > 0) {
		dig1 = num % 10;
		num = num / 10;
		dig2 = num % 10;
		num = num / 10;
		p[len] = (cob_u8_t) ((dig2 << 4) | dig1);
	}
}

void
cob_put_u64_comp3 (cob_u64_t val, void *mem, int len)
{
	int		dig1, dig2;
	cob_u64_t	num = val;
	cob_u8_t 	*p = mem;

	memset (mem, 0, len);
	p[--len] =  (cob_u8_t)((((num % 10) << 4) | 0x0F) & 0xFF);
	num = num / 10;
	while (num > 0
	    && len-- > 0) {
		dig1 = num % 10;
		num = num / 10;
		dig2 = num % 10;
		num = num / 10;
		p[len] = (cob_u8_t) ((dig2 << 4) | dig1);
	}
}

cob_s64_t
cob_get_s64_comp3 (void *mem, int len)
{
	int		sign, j;
	cob_s64_t	val = 0;
	cob_u8_t	*p = mem;

	if ((p[len - 1] & 0x0F) == 0x0D) {
		sign = -1;
	} else {
		sign = 1;
	}
	for (j = 0; j < len - 1; j++) {
		val = val * 10 + ((unsigned int)(p[j] & 0xf0) >> 4);
		val = val * 10 + (p[j] & 0x0f);
	}
	val = val * 10 + ((unsigned int)(p[len - 1] & 0xf0) >> 4);

	return val * sign;
}

cob_u64_t
cob_get_u64_comp3 (void *mem, int len)
{
	int		j;
	cob_u64_t	val = 0;
	cob_u8_t	*p = mem;

	for (j = 0; j < len - 1; j++) {
		val = val * 10 + ((unsigned int)(p[j] & 0xF0) >> 4);
		val = val * 10 + (p[j] & 0x0F);
	}
	val = val * 10 + ((unsigned int)(p[len - 1] & 0xF0) >> 4);

	return val;
}

void
cob_put_u64_comp6 (cob_u64_t val, void *mem, int len)
{
	int		dig1, dig2;
	cob_u64_t	num = val;
	cob_u8_t 	*p = mem;

	memset (mem, 0, len);
	while (num > 0
	    && len-- > 0) {
		dig1 = num % 10;
		num = num / 10;
		dig2 = num % 10;
		num = num / 10;
		p[len] = (cob_u8_t) ((dig2 << 4) | dig1);
	}
}

cob_u64_t
cob_get_u64_comp6 (void *mem, int len)
{
	int		j;
	cob_u64_t	val = 0;
	cob_u8_t	*p = mem;

	for (j = 0; j < len; j++) {
		val = val * 10 + ((unsigned int)(p[j] & 0xF0) >> 4);
		val = val * 10 + (p[j] & 0x0F);
	}

	return val;
}

/* note: the 11th position is only there to keep the analyzer happy ...*/
static char ebcdic_pos[11] = "{ABCDEFGHI";
static char ebcdic_neg[11] = "}JKLMNOPQR";

void
cob_put_s64_pic9 (cob_s64_t val, void *mem, int len)
{
	cob_s64_t	num;
	cob_u8_t	*p = mem;

	if (!cobglobptr || !COB_MODULE_PTR) {
		return;
	}

	memset (mem, '0', len);
	if (val < 0) {
		num = -val;
		if (COB_MODULE_PTR->ebcdic_sign) {
			p[--len] = (cob_u8_t)ebcdic_neg[num % 10];
		} else {
			p[--len] = (cob_u8_t)('0' + (num % 10)) | 0x40;
		}
	} else {
		num = val;
		if (COB_MODULE_PTR->ebcdic_sign) {
			p[--len] = (cob_u8_t)ebcdic_pos[num % 10];
		} else {
			p[--len] =  (cob_u8_t)('0' + (num % 10));
		}
	}
	num = num / 10;
	while (num > 0
	    && len-- > 0) {
		p[len] = (cob_u8_t) ('0' + num % 10);
		num = num / 10;
	}
}

cob_s64_t
cob_get_s64_pic9 (void *mem, int len)
{
	cob_s64_t	val = 0;
	cob_u8_t	*p = mem;
	int		sign = 1;

	while (len-- > 1) {
		/* note: as isdigit is locale-aware (slower and not what we want),
		   we use a range check instead */
		if (*p >= '0' && *p <= '9') {
			val = val * 10 + COB_D2I (*p);
		} else if (*p == '-') {
			sign = -1;
		}
		p++;
	}
	if (*p >= '0' && *p <= '9') {
		val = val * 10 + COB_D2I (*p);
	} else if (*p == '-') {
		sign = -1;
	} else if (*p == '+') {
		sign = 1;
	} else if (COB_MODULE_PTR->ebcdic_sign) {
#ifndef	COB_EBCDIC_MACHINE
		switch(*p) {
		case '{': val = val * 10 + 0; sign =  1; break;
		case 'A': val = val * 10 + 1; sign =  1; break;
		case 'B': val = val * 10 + 2; sign =  1; break;
		case 'C': val = val * 10 + 3; sign =  1; break;
		case 'D': val = val * 10 + 4; sign =  1; break;
		case 'E': val = val * 10 + 5; sign =  1; break;
		case 'F': val = val * 10 + 6; sign =  1; break;
		case 'G': val = val * 10 + 7; sign =  1; break;
		case 'H': val = val * 10 + 8; sign =  1; break;
		case 'I': val = val * 10 + 9; sign =  1; break;
		case '}': val = val * 10 + 0; sign = -1; break;
		case 'J': val = val * 10 + 1; sign = -1; break;
		case 'K': val = val * 10 + 2; sign = -1; break;
		case 'L': val = val * 10 + 3; sign = -1; break;
		case 'M': val = val * 10 + 4; sign = -1; break;
		case 'N': val = val * 10 + 5; sign = -1; break;
		case 'O': val = val * 10 + 6; sign = -1; break;
		case 'P': val = val * 10 + 7; sign = -1; break;
		case 'Q': val = val * 10 + 8; sign = -1; break;
		case 'R': val = val * 10 + 9; sign = -1; break;
		}
#else
		if ((*p & 0xF0) == 0xC0) {
			sign = 1;
		} else if ((*p & 0xF0) == 0xD0) {
			sign = -1;
		}
		val = val * 10 + COB_D2I (*p);
#endif
	} else {
		cob_u8_t	dig_part = *p & 0x3F;
		if (dig_part >= '0' && dig_part <= '9') {
			val = val * 10 + COB_D2I (dig_part);
		}
		if (*p & 0x40) {
			sign = -1;
		}
	}

	return val * sign;
}

void
cob_put_u64_pic9 (cob_u64_t val, void *mem, int len)
{
	cob_u64_t	num = val;
	cob_u8_t	*p = mem;

	memset (mem, '0', len);
	while (num > 0
	    && len-- > 0) {
		p[len] = (cob_u8_t) ('0' + num % 10);
		num = num / 10;
	}
}

cob_u64_t
cob_get_u64_pic9 (void *mem, int len)
{
	cob_u64_t	val = 0;
	cob_u8_t	*p = mem;

	while (len-- > 0) {
		val = val * 10 + COB_D2I (*p);
		p++;
	}

	return val;
}

void
cob_put_comp1 (float val, void *mem)
{
	memcpy (mem, &val, sizeof(float));
}
void
cob_put_comp2 (double val, void *mem)
{
	memcpy (mem, &val, sizeof(double));
}
float
cob_get_comp1 (void *mem)
{
	float val;
	memcpy (&val, mem, sizeof(float));
	return val;
}
double
cob_get_comp2 (void *mem)
{
	double val;
	memcpy (&val, mem, sizeof(double));
	return val;
}

void
cob_put_pointer (void *val, void *mem)
{
	memcpy (mem, &val, sizeof(void *));
}

char *
cob_get_picx (void *cbl_data, size_t len, void *char_field, size_t num_chars)
{
	size_t		i;
	cob_u8_t	*p = cbl_data;

	for (i = len; i != 0 && (p[i - 1] == ' ' || p[i - 1] == 0); i--);

	if (char_field == NULL) {
		num_chars = i + 1;
		char_field = cob_malloc (num_chars);
	}
	
	if (i > num_chars - 1) {
		i = num_chars - 1;
	}
	
	memcpy (char_field, cbl_data, i);
	((char*)char_field)[i] = 0;
	return char_field;
}

void
cob_put_picx (void *cbl_data, size_t len, void *string)
{
	size_t	i, j;
	cob_u8_t	*p = cbl_data;
	j = strlen ((char*)string);
	if (j > len) {
		j = len;
	}
	memcpy (cbl_data, string, j);
	for (i = j; i < len; i++) {
		p[i] = ' ';
	}
}

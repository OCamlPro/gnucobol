/*
   Copyright (C) 2001-2012, 2014-2023 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Simon Sobisch, Ron Norman,
   Chuck Haatveet

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
#include <limits.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

#include <math.h>
#ifdef HAVE_FINITE_IEEEFP_H
#include <ieeefp.h>
#endif

#ifdef _WIN32
#ifndef isnan
#define isnan(x)	_isnan(x)
#endif
#endif

#if !defined(isinf)
#if defined(_WIN32)
#define isinf(x) ((_fpclass(x) == _FPCLASS_PINF) || (_fpclass(x) == _FPCLASS_NINF))
#else
#define isinf(x) (!ISFINITE(x))
#endif
#endif

/* include decimal definitions, allowing their use in common.h later */
#ifdef	HAVE_GMP_H
#include <gmp.h>
#elif defined HAVE_MPIR_H
#include <mpir.h>
#else
#error either HAVE_GMP_H or HAVE_MPIR_H needs to be defined
#endif

/* include internal and external libcob definitions, forcing exports */
#define	COB_LIB_EXPIMP
#include "coblocal.h"


#ifdef	HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifndef SIGFPE
#ifndef NSIG
#define NSIG 240
#endif
#define SIGFPE NSIG + 1
#endif

#define DECIMAL_CHECK(d1,d2) \
	if (d1->scale == COB_DECIMAL_NAN || \
	    d2->scale == COB_DECIMAL_NAN) { \
		d1->scale = COB_DECIMAL_NAN; \
		return; \
	}

/* Local variables */

static cob_global	*cobglobptr;

/* translation table for integer 0-99 to BCD byte */
static const unsigned char packed_bytes[] = {
	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09,
	0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19,
	0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29,
	0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39,
	0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49,
	0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59,
	0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69,
	0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79,
	0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89,
	0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99
};

/* translation table for BCD byte (2 digits) to integer;
   identical defined in move.c */
static const unsigned char pack_to_bin [] = {
#if 1	/* invalid BCD nibbles as zero */
     0,   1,   2,   3,   4,   5,   6,   7,   8,   9,   0,   0,   0,   0,   0,   0,
    10,  11,  12,  13,  14,  15,  16,  17,  18,  19,   0,   0,   0,   0,   0,   0,
    20,  21,  22,  23,  24,  25,  26,  27,  28,  29,   0,   0,   0,   0,   0,   0,
    30,  31,  32,  33,  34,  35,  36,  37,  38,  39,   0,   0,   0,   0,   0,   0,
    40,  41,  42,  43,  44,  45,  46,  47,  48,  49,   0,   0,   0,   0,   0,   0,
    50,  51,  52,  53,  54,  55,  56,  57,  58,  59,   0,   0,   0,   0,   0,   0,
    60,  61,  62,  63,  64,  65,  66,  67,  68,  69,   0,   0,   0,   0,   0,   0,
    70,  71,  72,  73,  74,  75,  76,  77,  78,  79,   0,   0,   0,   0,   0,   0,
    80,  81,  82,  83,  84,  85,  86,  87,  88,  89,   0,   0,   0,   0,   0,   0,
    90,  91,  92,  93,  94,  95,  96,  97,  98,  99,   0,   0,   0,   0,   0,   0,
     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0
#else	/* invalid BCD nibbles as translated since at least OC 1.1 */
     0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
    10,  11,  12,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,
    20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  32,  33,  34,  25,
    30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,
    40,  41,  42,  43,  44,  45,  46,  47,  48,  49,  50,  51,  52,  53,  54,  55,
    50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,  64,  65,
    60,  61,  62,  63,  64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,
    70,  71,  72,  73,  74,  75,  76,  77,  78,  79,  80,  81,  82,  83,  84,  85,
    80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
    90,  91,  92,  93,  94,  95,  96,  97,  98,  99, 100, 101, 102, 103, 104, 105,
   100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115,
   110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125,
   120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135,
   130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145,
   140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155,
   150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165
#endif
};

static cob_decimal	cob_d1;
static cob_decimal	cob_d2;
static cob_decimal	cob_d3;
static cob_decimal	cob_t1;
static cob_decimal	cob_t2;
static cob_decimal	cob_d_remainder;

static cob_decimal	*cob_decimal_base;

static mpz_t		cob_mexp;
static mpz_t		cob_mpzt;
static mpz_t		cob_mpzt2;
static mpz_t		cob_mpz_ten34m1;
static mpz_t		cob_mpz_ten16m1;
static mpz_t		cob_mpze10[COB_MAX_BINARY + 1];

static mpf_t		cob_mpft;
static mpf_t		cob_mpft_get;

static cob_u64_t	i64_spaced_out;

static unsigned char	packed_value[20];
static cob_u64_t	last_packed_val;
static int		cob_not_finite = 0;

/* Function prototypes */
static void cob_set_packed_u64 (cob_field * f, const cob_u64_t val, const int sign);

void
cob_gmp_free (void * ptr) {
/* mpir/gmp free functions */
#ifdef HAVE_MP_GET_MEMORY_FUNCTIONS
	void (*freefunc)(void *, size_t);
	mp_get_memory_functions (NULL, NULL, &freefunc);
	freefunc (ptr, strlen((char*) ptr) + 1);
#else
	free (ptr);
#endif
}

static COB_INLINE COB_A_INLINE cob_s64_t
cob_binary_get_sint64 (const cob_field * const f)
{
	cob_s64_t	n = 0;
	const size_t	fsiz = 8U - f->size;

#ifndef WORDS_BIGENDIAN
	if (COB_FIELD_BINARY_SWAP (f)) {
		memcpy (&n, f->data, f->size);
		n = COB_BSWAP_64 (n);
	} else {
		memcpy ((char *)&n + fsiz, f->data, f->size);
	}
#else	/* WORDS_BIGENDIAN */
	memcpy (&n, f->data, f->size);
#endif	/* WORDS_BIGENDIAN */
	/* Shift with sign */
	n >>= (cob_s64_t)8 * fsiz;

	return n;
}

static COB_INLINE COB_A_INLINE cob_u64_t
cob_binary_get_uint64 (const cob_field * const f)
{
	cob_u64_t		n = 0;

#ifndef WORDS_BIGENDIAN
	if (COB_FIELD_BINARY_SWAP (f)) {
		const size_t	fsiz = 8U - f->size;
		memcpy ((char *)&n + fsiz, f->data, f->size);
		n = COB_BSWAP_64 (n);
	} else {
		memcpy (&n, f->data, f->size);
	}
#else	/* WORDS_BIGENDIAN */
	const size_t	fsiz = 8U - f->size;
	memcpy ((char *)&n + fsiz, f->data, f->size);
#endif	/* WORDS_BIGENDIAN */

	return n;
}

static COB_INLINE COB_A_INLINE void
cob_binary_set_uint64 (cob_field *f, cob_u64_t n)
{
#ifndef WORDS_BIGENDIAN
	if (COB_FIELD_BINARY_SWAP (f)) {
		const size_t	fsiz = 8U - f->size;
		n = COB_BSWAP_64 (n);
		memcpy (f->data, (char *)&n + fsiz, f->size);
	} else {
		memcpy (f->data, (char *)&n, f->size);
	}
#else	/* WORDS_BIGENDIAN */
	const size_t	fsiz = 8U - f->size;
	memcpy (f->data, (char *)&n + fsiz, f->size);
#endif	/* WORDS_BIGENDIAN */
}

static COB_INLINE COB_A_INLINE void
cob_binary_set_int64 (cob_field *f, cob_s64_t n)
{
#ifndef WORDS_BIGENDIAN
	if (COB_FIELD_BINARY_SWAP (f)) {
		const size_t	fsiz = 8U - f->size;
		n = COB_BSWAP_64 (n);
		memcpy (f->data, (char *)&n + fsiz, f->size);
	} else {
		memcpy (f->data, &n, f->size);
	}
#else	/* WORDS_BIGENDIAN */
	const size_t	fsiz = 8U - f->size;
	memcpy (f->data, (char *)&n + fsiz, f->size);
#endif	/* WORDS_BIGENDIAN */
}

/* Decimal number */

void
cob_decimal_init2 (cob_decimal *d, const cob_uli_t initial_num_bits)
{
	mpz_init2 (d->value, initial_num_bits);
	d->scale = 0;
}

void
cob_decimal_init (cob_decimal *d)
{
	cob_decimal_init2 (d, COB_MPZ_DEF);
}

void
cob_decimal_clear (cob_decimal *d)
{
	if (d) {
		mpz_clear (d->value);
		d->scale = 0;
	}
}

/** setting a decimal field from an unsigned binary long int */
void
cob_decimal_set_ullint (cob_decimal *d, const cob_u64_t n)
{
#ifdef	COB_LI_IS_LL
	mpz_set_ui (d->value, (cob_uli_t)n);
#else
	mpz_set_ui (d->value, (cob_uli_t)(n >> 32));
	mpz_mul_2exp (d->value, d->value, 32);
	mpz_add_ui (d->value, d->value, (cob_uli_t)(n & 0xFFFFFFFFU));
#endif
	d->scale = 0;
}

/** setting a decimal field from a signed binary long int */
void
cob_decimal_set_llint (cob_decimal *d, const cob_s64_t n)
{
#ifdef	COB_LI_IS_LL
	mpz_set_si (d->value, (cob_sli_t)n);
#else
	cob_u64_t	uval;
	cob_u32_t	negative;

	negative = 0;
	if (n < 0) {
		negative = 1;
		uval = (cob_u64_t)-n;
	} else {
		uval = (cob_u64_t)n;
	}
	mpz_set_ui (d->value, (cob_uli_t)(uval >> 32));
	mpz_mul_2exp (d->value, d->value, 32);
	mpz_add_ui (d->value, d->value, (cob_uli_t)(uval & 0xFFFFFFFFU));
	if (negative) {
		mpz_neg (d->value, d->value);
	}
#endif
	d->scale = 0;
}

/* Decimal print, note: currently (GC3.1) only called by display/dump
   code from termio.c (cob_display) via cob_print_ieeedec) */
static void
cob_decimal_print (cob_decimal *d, FILE *fp)
{
	int	scale, len;
	char		*mza;

	if (d->scale == COB_DECIMAL_NAN) {
		fprintf (fp, "(Nan)");
		return;
	}
	if (d->scale == COB_DECIMAL_INF) {
		fprintf (fp, "(Inf)");
		return;
	}
	if (mpz_sgn (d->value) == 0) {
		fprintf (fp, "0E0");
		return;
	}
	mpz_set (cob_mpzt2, d->value);
	scale = d->scale;
	for ( ; ; ) {
		if (!mpz_divisible_ui_p (cob_mpzt2, 10UL)) {
			break;
		}
		mpz_tdiv_q_ui (cob_mpzt2, cob_mpzt2, 10UL);
		scale--;
	}
	mza = mpz_get_str (NULL, 10, cob_mpzt2);
	len = (int)strlen (mza);
	if (len > 0
	 && scale > 0
	 && scale < len) {
		fprintf (fp, "%.*s%c%.*s",
			len - scale, mza, '.',
			scale, mza + len - scale);
	} else if (scale == 0) {
		fprintf (fp, "%s", mza);
	} else {
		fprintf (fp, "%sE%d", mza, -scale);
	}
	cob_gmp_free (mza);
}

#define MAX_LLI_DIGITS_PLUS_1 20
#ifdef	COB_LI_IS_LL
#define MAX_LI_DIGITS_PLUS_1  20
#else
#define MAX_LI_DIGITS_PLUS_1  10
#endif

/* Get power of 10 as mpz_t */
static COB_INLINE COB_A_INLINE void
cob_pow_10 (mpz_t mexp, unsigned int n)
{
	if (n <= COB_MAX_BINARY) {
		mpz_set (mexp, cob_mpze10[n]);
	} else {
	/* bigger values are needed,
	   for example in FUNCTION RANDOM test (999)
	   and with extreme huge value (6499) for test
	   "FLOAT-DECIMALL w/o SIZE ERROR" to get huge
	   FD32 values with the right scale */
	/* TODO: add artificial limit with raising ON SIZE ERROR
	   as we otherwise run into an abort directly in GMP
	   for _real huge_ numbers */
		mpz_ui_pow_ui (mexp, 10UL, n);
	}
}

/* using multiplication / division by pre-stored mpz_t
   variables with power of 10 showed to be slower than
   using integer multiplication / division (on 2 test machines);
   if needed define PREFER_MPZ_MUL to use "old" code */
#ifndef PREFER_MPZ_MUL
 #ifdef	HAVE_DESIGNATED_INITS
const cob_uli_t cob_pow_10_uli_val[MAX_LI_DIGITS_PLUS_1] = {
	  1
	, 10
	, 100
	, 1000
	, 10000
	, 100000
	, 1000000
	, 10000000
	, 100000000
	, 1000000000UL
#ifdef	COB_LI_IS_LL
	, 10000000000
	, 100000000000
	, 1000000000000
	, 10000000000000
	, 100000000000000
	, 1000000000000000
	, 10000000000000000
	, 100000000000000000
	, 1000000000000000000
	, 10000000000000000000UL
#endif
};
  #define	cob_pow_10_uli(n)	cob_pow_10_uli_val[n]
 #else
static COB_INLINE COB_A_INLINE cob_uli_t
cob_pow_10_uli (unsigned int n)
{
	register cob_uli_t ret = 1;
	while (n > 0) {
		ret *= 10;
		--n;
	}
	return ret;
}
 #endif
#endif


/* scale - multiplicate mpz_t by power of 10 */
static COB_INLINE COB_A_INLINE void
cob_mul_by_pow_10 (mpz_t mexp, unsigned int n)
{
#ifndef PREFER_MPZ_MUL
	if (n < MAX_LI_DIGITS_PLUS_1) {
		mpz_mul_ui (mexp, mexp, cob_pow_10_uli (n));
		return;
	}
#endif
	cob_pow_10 (cob_mexp, n);
	mpz_mul (mexp, mexp, cob_mexp);
}

/* scale - divide mpz_t by power of 10 */
static COB_INLINE COB_A_INLINE void
cob_div_by_pow_10 (mpz_t mexp, unsigned int n)
{
#ifndef PREFER_MPZ_MUL
	if (n < MAX_LI_DIGITS_PLUS_1) {
		mpz_tdiv_q_ui (mexp, mexp, cob_pow_10_uli (n));
		return;
	}
#endif
	cob_pow_10 (cob_mexp, n);
	mpz_tdiv_q (mexp, mexp, cob_mexp);
}

/* d->value *= 10^n, d->scale += n
   n may not be 0! */
static void
shift_decimal (cob_decimal *d, int n)
{
	if (n > 0) {
		cob_mul_by_pow_10 (d->value, n);
	} else {
		cob_div_by_pow_10 (d->value, -n);
	}
	d->scale += n;
}

/* Align decimal */
static void
align_decimal (cob_decimal *d1, cob_decimal *d2)
{
	if (d1->scale < d2->scale) {
		shift_decimal (d1, d2->scale - d1->scale);
	} else if (d1->scale > d2->scale) {
		shift_decimal (d2, d1->scale - d2->scale);
	}
}

/* IEEE 754 floats */

static void
cob_decimal_adjust (cob_decimal *d, mpz_t max_value, int min_exp, int max_exp)
{
	/* Remove trailing ZEROS */
	int power_of_ten;	/* note: old versions have unsigned long, newer a typedef
	                 	   so we cast to rule them all... */
	power_of_ten = (int) mpz_remove (cob_t1.value, d->value, cob_mpze10[1]);
	if (power_of_ten != 0) {
		mpz_set (d->value, cob_t1.value);
		d->scale -= power_of_ten;
	}

	/* move comma to the left */
	while (mpz_cmpabs (d->value, max_value) > 0) {
		if (d->scale < min_exp)
			break;
		mpz_tdiv_q_ui (d->value, d->value, 10U);
		d->scale--;
	}
	if (mpz_cmpabs (d->value, max_value) > 0
	 || d->scale < min_exp
	 || d->scale > max_exp) {
		cob_set_exception (COB_EC_SIZE_OVERFLOW);
		return;
	}
}

static int
cob_decimal_get_ieee64dec (cob_decimal *d, cob_field *f, const int opt)
{
	cob_u64_t	expo;
	cob_u64_t	data;
	const int	sign = mpz_sgn (d->value);

	if (sign == 0) {
		memset (f->data, 0, (size_t)8);
		return 0;
	}
	if (sign == -1) {
		mpz_neg (d->value, d->value);
	}
	cob_decimal_adjust (d, cob_mpz_ten16m1, -369, 398);
	if (mpz_cmpabs (d->value, cob_mpz_ten16m1) > 0) {
		if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
			cob_set_exception (COB_EC_SIZE_OVERFLOW);
			return cobglobptr->cob_exception_code;
		}
		for ( ; ; ) {
			if (d->scale < -369) {
				break;
			}
			mpz_tdiv_q_ui (d->value, d->value, 10UL);
			d->scale--;
			if (mpz_cmpabs (d->value, cob_mpz_ten16m1) < 0) {
				break;
			}
		}
	}
	if (d->scale < -369 || d->scale > 398) {
		cob_set_exception (COB_EC_SIZE_OVERFLOW);
		return cobglobptr->cob_exception_code;
	}
	expo = (cob_u64_t)398 - d->scale;

	data = 0;
	mpz_export (&data, NULL, -1, (size_t)8, COB_MPZ_ENDIAN, (size_t)0, d->value);
	/* Move in exponent */
	if (mpz_sizeinbase (d->value, 2) > 53U) {
		data &= COB_64_SIGF_2;
		data |= (expo << 51U) | COB_DEC_EXTEND;
	} else {
		data &= COB_64_SIGF_1;
		data |= (expo << 53U);
	}
	if (sign == -1) {
		data |= COB_DEC_SIGN;
	}
	memcpy (f->data, &data, (size_t)8);
	return 0;
}

static void
cob_decimal_set_ieee64dec (cob_decimal *d, const cob_field *f)
{
	cob_u64_t	expo;
	cob_u64_t	data;
	int		has_negative_sign;

	/* bit 0 : sign bit */
	/* bits 1 - 4 : combination field */
	/* combination = 15 (all bits set) is inf/nan */
	/* combination > 11 (bits 1100) is extended exponent */
	/* Exponent length - 10 bits */

	memcpy (&data, f->data, sizeof(data));
	if (COB_64_IS_SPECIAL (data)) {
		/* Inf / Nan */
		mpz_set_ui (d->value, 1UL);
		d->scale = COB_DECIMAL_NAN;
		return;
	}
	has_negative_sign = !!(data & COB_DEC_SIGN);
	if (COB_64_IS_EXTEND (data)) {
		expo = (data & COB_64_EXPO_2) >> 51U;
		data &= COB_64_SIGF_2;
		data |= COB_64_OR_EXTEND;
		if (data > COB_U64_C(9999999999999999)) {
			mpz_set_ui (d->value, 0UL);
			d->scale = 0;
			return;
		}
	} else {
		expo = (data & COB_64_EXPO_1) >> 53U;
		data &= COB_64_SIGF_1;
	}
	if (!data) {
		/* Significand 0 */
		mpz_set_ui (d->value, 0UL);
		d->scale = 0;
		return;
	}
#ifdef	COB_LI_IS_LL
	mpz_set_ui (d->value, data);
#else
	mpz_set_ui (d->value, (cob_uli_t)(data >> 32));
	mpz_mul_2exp (d->value, d->value, 32);
	mpz_add_ui (d->value, d->value, (cob_uli_t)(data & 0xFFFFFFFFU));
#endif

	d->scale = (int)expo - 398;
	if (d->scale > 0) {
		cob_mul_by_pow_10 (d->value, d->scale);
		d->scale = 0;
	} else if (d->scale < 0) {
		d->scale = -(d->scale);
	}
	if (has_negative_sign) {
		mpz_neg (d->value, d->value);
	}
	if (d->scale < -369 || d->scale > 398) {
		cob_set_exception (COB_EC_SIZE_OVERFLOW);
		return;
	}
}

static int
cob_decimal_get_ieee128dec (cob_decimal *d, cob_field *f, const int opt)
{
	cob_u64_t	expo;
	cob_u64_t	data[2];
	const int	sign = mpz_sgn (d->value);

	if (sign == 0) {
		memset (f->data, 0, (size_t)16);
		return 0;
	}
	if (sign == -1) {
		mpz_neg (d->value, d->value);
	}
	cob_decimal_adjust (d, cob_mpz_ten34m1, -6111, 6176);
	if (mpz_cmpabs (d->value, cob_mpz_ten34m1) > 0) {
		if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
			cob_set_exception (COB_EC_SIZE_OVERFLOW);
			return cobglobptr->cob_exception_code;
		}
		for ( ; ; ) {
			if (d->scale < -6111) {
				break;
			}
			mpz_tdiv_q_ui (d->value, d->value, 10UL);
			d->scale--;
			if (mpz_cmpabs (d->value, cob_mpz_ten34m1) < 0) {
				break;
			}
		}
	}
	if (d->scale < -6111 || d->scale > 6176) {
		cob_set_exception (COB_EC_SIZE_OVERFLOW);
		return cobglobptr->cob_exception_code;
	}
	expo = (cob_u64_t)6176 - d->scale;

	data[0] = 0;
	data[1] = 0;
	mpz_export (data, NULL, -1, (size_t)16, COB_MPZ_ENDIAN, (size_t)0, d->value);
	/* Move in exponent */
	COB_128_MSW(data) &= COB_128_SIGF_1;
	COB_128_MSW(data) |= (expo << 49U);
	if (sign == -1) {
		COB_128_MSW(data) |= COB_DEC_SIGN;
	}
	memcpy (f->data, data, (size_t)16);
	return 0;
}

static void
cob_decimal_set_ieee128dec (cob_decimal *d, const cob_field *f)
{
	cob_u64_t	expo;
	cob_u64_t	data[2];
	int		has_negative_sign;

	/* bit 0 : sign bit */
	/* bits 1 - 4 : combination field */
	/* combination = 15 (all bits set) is inf/nan */
	/* combination > 11 (bits 1100) is extended exponent */
	/* Exponent length - 14 bits */

	memcpy (data, f->data, sizeof(data));
	if (COB_128_IS_SPECIAL (data)) {
		/* Inf / Nan */
		mpz_set_ui (d->value, 1UL);
		d->scale = COB_DECIMAL_NAN;
		return;
	}
	has_negative_sign = !!(COB_128_MSW(data) & COB_DEC_SIGN);
	if (COB_128_IS_EXTEND (data)) {
		expo = (COB_128_MSW(data) & COB_128_EXPO_2) >> 47U;
		COB_128_MSW(data) &= COB_128_SIGF_2;
		COB_128_MSW(data) |= COB_128_OR_EXTEND;
	} else {
		expo = (COB_128_MSW(data) & COB_128_EXPO_1) >> 49U;
		COB_128_MSW(data) &= COB_128_SIGF_1;
	}
	if (!COB_128_MSW(data) && !COB_128_LSW(data)) {
		/* Significand 0 */
		mpz_set_ui (d->value, 0UL);
		d->scale = 0;
		return;
	}
#ifdef	COB_LI_IS_LL
	mpz_set_ui (d->value, COB_128_MSW(data));
	mpz_mul_2exp (d->value, d->value, 64UL);
	mpz_add_ui (d->value, d->value, COB_128_LSW(data));
#else
	/* RXWRXW - Fixme */
	mpz_set_ui (d->value, (cob_uli_t)(COB_128_MSW(data) >> 32U));
	mpz_mul_2exp (d->value, d->value, 32UL);
	mpz_add_ui (d->value, d->value, (cob_uli_t)(COB_128_MSW(data) & 0xFFFFFFFFU));
	mpz_mul_2exp (d->value, d->value, 32UL);
	mpz_add_ui (d->value, d->value, (cob_uli_t)(COB_128_LSW(data) >> 32U));
	mpz_mul_2exp (d->value, d->value, 32UL);
	mpz_add_ui (d->value, d->value, (cob_uli_t)(COB_128_LSW(data) & 0xFFFFFFFFU));
#endif

	d->scale = (int)expo - 6176;
	if (d->scale > 0) {
		cob_mul_by_pow_10 (d->value, d->scale);
		d->scale = 0;
	} else if (d->scale < 0) {
		d->scale = -(d->scale);
	}
	if (has_negative_sign) {
		mpz_neg (d->value, d->value);
	}
	cob_decimal_adjust (d, cob_mpz_ten34m1, -6111, 6176);
	if (mpz_cmpabs (d->value, cob_mpz_ten34m1) > 0) {
		/* Non-canonical */
		cob_set_exception (COB_EC_SIZE_OVERFLOW);
		mpz_set_ui (d->value, 0UL);
		d->scale = 0;
		return;
	}
}

/* Decimal <-> GMP float */

static void
cob_decimal_set_mpf_core (cob_decimal *d, const mpf_t src)
{
	cob_sli_t	scale;
	cob_sli_t	len;

	/* we now convert from mpf to cob_decimal, to do so without
	   loosing anything we need to use an intermediate string conversion
	   (direct conversion to mpz would truncate non-integer parts,
		the string conversion provides us with the scale already) */
	{
		char buffer[COB_MAX_INTERMEDIATE_FLOATING_SIZE + 2];
		mpf_get_str (buffer, &scale, 10, COB_MAX_INTERMEDIATE_FLOATING_SIZE, src);
		len = (cob_sli_t)strlen (buffer);
		if (buffer[0] == '-') {
			mpz_set_str (d->value, buffer + 1, 10);
			mpz_neg (d->value, d->value);
			len--;
		} else {
			mpz_set_str (d->value, buffer, 10);
		}
	}

	len -= scale;

	if (len >= 0) {
		d->scale = len;
	} else {
		d->scale = 0;
		cob_mul_by_pow_10 (d->value, -len);
	}
}

void
cob_decimal_set_mpf (cob_decimal *d, const mpf_t src)
{
	if (!mpf_sgn (src)) {
		mpz_set_ui (d->value, 0);
		d->scale = 0;
		return;
	}
	cob_decimal_set_mpf_core (d, src);
}

void
cob_decimal_get_mpf (mpf_t dst, const cob_decimal *d)
{
	const cob_sli_t	scale = d->scale;

	mpf_set_z (dst, d->value);

	if (scale < 0) {
		cob_pow_10 (cob_mexp, (cob_uli_t)-scale);
		mpf_set_z (cob_mpft_get, cob_mexp);
		mpf_mul (dst, dst, cob_mpft_get);
	} else if (scale > 0) {
		cob_pow_10 (cob_mexp, (cob_uli_t)scale);
		mpf_set_z (cob_mpft_get, cob_mexp);
		mpf_div (dst, dst, cob_mpft_get);
	}
}

/* Double */

static void
cob_decimal_set_double (cob_decimal *d, const double v)
{
	/* checking for unlikely but possible exceptions
	   (CHECKME: that may should actually raise an EXCEPTION,
	    possibly depending on context) */
	{
		union {
			double		d1;
			cob_u64_t	l1;
		} ud;
		ud.d1 = v;
		/* FIXME: move "spaced out" out, to handle this scenario
		   for float (missing) and double in the caller */
		if (ud.l1 == 0 || ud.l1 == i64_spaced_out || !ISFINITE (v)) {
			mpz_set_ui (d->value, 0);
			d->scale = 0;
			return;
		}
	}

	/* we now convert from float to cob_decimal, to do so without
	   loosing anything we need to use an intermediate string conversion
	   (direct conversion to mpz would truncate non-integer parts,
	    the string conversion provides us with the scale already) */
	mpf_set_d (cob_mpft, v);
	cob_decimal_set_mpf_core (d, cob_mpft);
}

static double
cob_decimal_get_double (cob_decimal *d)
{
	double		v;

	cob_not_finite = 0;
	if (mpz_size (d->value) == 0) {
		return 0.0;
	}
	cob_decimal_get_mpf (cob_mpft, d);

	v = mpf_get_d (cob_mpft);
	if (!ISFINITE (v)) {
		cob_not_finite = 1;
		return 0.0;
	}
	return v;
}

/* PACKED-DECIMAL */

static COB_INLINE COB_A_INLINE int
cob_packed_get_sign (const cob_field *f)
{
	if (COB_FIELD_HAVE_SIGN (f)) {
		const unsigned char p = *(f->data + f->size - 1);
		return ((p & 0x0F) == 0x0D) ? -1 : 1;
	}
	return 0;
}

#if	0	/* RXWRXW - Buggy */
static void
cob_complement_packed (cob_field *f)
{
	unsigned char	*p;
	int		ndigs;
	int		tval;
	int		carry = 0;
	unsigned int	msn;

	ndigs = COB_FIELD_DIGITS(f) - COB_FIELD_SCALE (f);
	if (COB_FIELD_NO_SIGN_NIBBLE (f)) {
		msn = COB_FIELD_SCALE (f) % 2;
	} else {
		msn = 1 - (COB_FIELD_SCALE (f) % 2);
	}

	p = f->data + (ndigs / 2) - (1 - msn);
	while (ndigs--) {
		if (!msn) {
			tval = *p & 0x0F;
		} else {
			tval = (*p & 0xF0) >> 4;
		}
		tval += carry;
		if (tval > 0) {
			carry = 1;
			tval= 10 - tval;
		} else {
			carry = 0;
		}
		if (!msn) {
			*p = (*p & 0xF0) | tval;
			msn = 1;
		} else {
			*p = (*p & 0x0F) | (tval << 4);
			msn = 0;
			p--;
		}
	}
}

static int
cob_add_packed (cob_field *f, int val, const int opt)
{
	unsigned char	*p;
	int		sign;
	int		ndigs;
	int		tval;
	int		carry = 0;
	unsigned int	msn;
	unsigned int	subtr = 0;
	unsigned int	zeroes = 0;
	unsigned int	origdigs;
	unsigned char	savedata[48];

	ndigs = COB_FIELD_DIGITS(f) - COB_FIELD_SCALE (f);
	if (ndigs <= 0) {
		return 0;
	}

	if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
		memcpy (savedata, f->data, f->size);
	}

	if (COB_FIELD_NO_SIGN_NIBBLE (f)) {
		sign = 0;
		msn = COB_FIELD_SCALE (f) % 2;
		tval = msn;
	} else {
		sign = cob_packed_get_sign (f);
		msn = 1 - (COB_FIELD_SCALE (f) % 2);
		tval = 1 - (COB_FIELD_SCALE(f) % 2);
	}

	/* -x +v = -(x - v), -x -v = -(x + v) */
	if (sign == -1) {
		val = -val;
	}
	if (val < 0) {
		val = -val;
		subtr = 1;
	}
	p = f->data + (ndigs / 2) - (1 - tval);
	origdigs = ndigs;
	while (ndigs--) {
		if (val) {
			carry += (val % 10);
			val /= 10;
		}
		if (!msn) {
			tval = *p & 0x0F;
		} else {
			tval = (*p & 0xF0) >> 4;
		}
		if (subtr) {
			tval -= carry;
			if (tval < 0) {
				tval += 10;
				carry = 1;
			} else {
				carry = 0;
			}
		} else {
			tval += carry;
			if (tval > 9) {
				tval = (tval + 6) & 0x0F;
				carry = 1;
			} else {
				carry = 0;
			}
		}
		if (tval == 0) {
			zeroes++;
		}
		if (!msn) {
			*p = (*p & 0xF0) | tval;
			msn = 1;
		} else {
			*p = (*p & 0x0F) | (tval << 4);
			msn = 0;
			p--;
		}
	}
	if (sign) {
		p = f->data + f->size - 1;
		if (origdigs == zeroes) {
			*p = (*p & 0xF0) | 0x0C;
		} else if (subtr && carry) {
			cob_complement_packed (f);
			sign = -sign;
			if (sign == -1) {
				*p = (*p & 0xF0) | 0x0D;
			} else {
				*p = (*p & 0xF0) | 0x0C;
			}
		}
	} else if (subtr && carry) {
		cob_complement_packed (f);
	}
	if (opt && (carry || val)) {
		/* Overflow */
		cob_set_exception (COB_EC_SIZE_OVERFLOW);
		/* If we need to throw an exception */
		if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
			memcpy (f->data, savedata, f->size);
			return cobglobptr->cob_exception_code;
		}
	}
	return 0;
}
#endif

void
cob_set_packed_zero (cob_field *f)
{
	memset (f->data, 0, f->size);
	if (COB_FIELD_NO_SIGN_NIBBLE (f)) {
		return;
	}
	if (!COB_FIELD_HAVE_SIGN (f)) {
		*(f->data + f->size - 1) = 0x0F;
	} else {
		*(f->data + f->size - 1) = 0x0C;
	}
}

/* get the numeric value and scale from the given field and store it in the
   specified decimal */
static void
cob_decimal_set_packed (cob_decimal *d, cob_field *f)
{
	register unsigned char *p = f->data;
	const int nibtest = !!COB_FIELD_NO_SIGN_NIBBLE (f);
	const unsigned char *endp = p + f->size - 1 + nibtest;
	cob_uli_t	byteval;

	const short scale = COB_FIELD_SCALE (f);
	int 	digits;

	if (scale >= 0) {
		digits = COB_FIELD_DIGITS (f);
	} else {
		/* 99P -> 3 digits, scale -1 */
		digits = COB_FIELD_DIGITS (f) + scale;
	}
	if ((digits & 1) /* % 2 */ == nibtest) {
		byteval = *p++ & 0x0F;
		if (byteval == 0) {
			/* Skip leading ZEROs */
			while (p < endp
			 && *p == 0x00) {
				digits -= 2;
				p++;
			}
		}
	} else {
		byteval = 0;
		/* Skip leading ZEROs */
		while (p < endp
		 && *p == 0x00) {
			digits -= 2;
			p++;
		}
	}
	if (digits < MAX_LLI_DIGITS_PLUS_1) {
		/* note: similar logic in move.c (packed_get_long_long, packed_get_int)
		   so for all adjustments here - check there, too */
		register cob_u64_t val = byteval;

		for (; p < endp; p++) {
			val = val * 100 + pack_to_bin[*p];
		}

		if (!nibtest) {
			val = val * 10
				+ (*p >> 4);
		}
#ifdef	COB_LI_IS_LL
		mpz_set_ui (d->value, (cob_uli_t)val);
#else
		cob_decimal_set_ullint (d, val);
#endif

	} else {
		/* note: an implementation similar to display - expanding to string,
		   then convert to mpz from there - was tested and found to be slower */

		const unsigned char *endp_4digits = endp - 1;
		unsigned int	nonzero = !!byteval;

		mpz_set_ui (d->value, byteval);

		/* take 4 digits at once as long as possible to reduce GMP calls */
		for (; p < endp_4digits; p += 2) {
			if (nonzero) {
				mpz_mul_ui (d->value, d->value, 10000UL);
			}
			mpz_add_ui (d->value, d->value,
				    ( (int)pack_to_bin[*p] * 100)
				    + pack_to_bin[*(p + 1)]);
			/* because of the zero-skipping we are always nonzero here */
			nonzero = 1;
		}
		/* handling last digit + half-nibble */
		if (p < endp) {
			if (nonzero) {
				mpz_mul_ui (d->value, d->value, 100);
			}
			if (*p) {
				mpz_add_ui (d->value, d->value, pack_to_bin[*p]);
				nonzero = 1;
			}
			p++;
		}

		if (!nibtest) {
			if (nonzero) {
				mpz_mul_ui (d->value, d->value, 10);
			}
			mpz_add_ui (d->value, d->value, (cob_uli_t)(*p >> 4));
		}
	}

	if (cob_packed_get_sign (f) == -1) {
		mpz_neg (d->value, d->value);
	}
	d->scale = COB_FIELD_SCALE (f);
}

/* get the numeric value from the given decimal and store it in the
   specified BCD field (or, depending on opt, set overflow exception and return
   with the field unchanged);
   note: the scale is ignored so has to be aligned up-front */
static int
cob_decimal_get_packed (cob_decimal *d, cob_field *f, const int opt)
{
	char	buff[COB_MAX_BINARY * 2];
	register unsigned char	*p;
	unsigned char	*data;
	const int		sign = mpz_sgn (d->value);
	unsigned int	size, diff;	/* packed fields are 38 digits max */
	unsigned short		digits;

	/* handle sign and check for ZERO value (allows early exit) */
	if (sign == 1) {
		/* positive, nothing to do, most expected, so extra checked */
	} else if (sign == -1) {
		/* negative, switch */
		mpz_abs (d->value, d->value);
	} else /* sign == 0 */ {
		cob_set_packed_zero (f);
		return 0;
	}

	{
		const short	scale = COB_FIELD_SCALE (f);
		if (scale >= 0) {
			digits = COB_FIELD_DIGITS (f);
		} else {
			/* 99P -> 3 digits, scale -1 --> real digits are less */
			digits = COB_FIELD_DIGITS (f) + scale;
		}
	}

	/* Build string, note: we can't check the decimal size with mpz_sizeinbase,
	   as its result is "either exact or one too big" (for base != 2);
	   using gmp_snprintf to get both the string and the length was also
	   tested - and found to be much slower than the following code */

	/* get divisor that would overflow */
	cob_pow_10 (cob_mexp, digits);
	/* check if it is >= what we have */
	if (mpz_cmp (d->value, cob_mexp) >= 0) {
		/* Overflow */
		if ((opt & COB_STORE_NO_SIZE_ERROR) == 0) {
			cob_set_exception (COB_EC_SIZE_OVERFLOW);
			/* If the statement has ON SIZE ERROR, then throw
			   an exception, leaving the target unchanged */
			if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
				return cobglobptr->cob_exception_code;
			}
		}
		/* Other size, truncate digits, using the remainder */
		mpz_tdiv_r (cob_mexp, d->value, cob_mexp);
		/* integer setting, if possible */
		if (mpz_fits_ulong_p (cob_mexp)) {
			cob_set_packed_u64 (f, mpz_get_ui (cob_mexp), sign);
			return 0;
		}
		/* get truncated digits as string */
		(void) mpz_get_str (buff, 10, cob_mexp);
		/* note: truncation may lead to 100012 be changed to 00012
		         in which case mpz_get_str provides us with 12 */
	} else {
		/* integer setting, if possible */
		if (mpz_fits_ulong_p (d->value)) {
			cob_set_packed_u64 (f, mpz_get_ui (d->value), sign);
			return 0;
		}

		/* No overflow, so get string data as-is */
		(void) mpz_get_str (buff, 10, d->value);
	}

	/* zero-out memory, necessary as we skip leading zeroes */
	data = f->data;
	memset (data, 0, f->size);

	/* calculate starting half-byte */
	size = (unsigned int) strlen (buff);
	diff = (unsigned int) digits - size;

	if (COB_FIELD_NO_SIGN_NIBBLE (f)) {
		p = data + ((digits - 1) / 2) - ((size - 1) / 2);
		diff = (size % 2);
	} else {
		p = data + (digits / 2) - (size / 2);
		diff = 1 - (size % 2);
	}
	size += diff;
	/* set data starting from first half-byte with data until end */
	{
		register unsigned char *q = (unsigned char *)buff;
		register unsigned int	i = diff;

		if ((i % 2) == 1) {
			*p++ += COB_D2I (*q++);
			i++;
		}
		while (i < size) {
			*p++ = ((*q << 4) & 0xFF)	/* -> dropping the higher bits = no use in COB_D2I */
			   + COB_D2I (*(q + 1));
			q += 2; i += 2;
		}
	}

	if (COB_FIELD_NO_SIGN_NIBBLE (f)) {
		return 0;
	}

	/* add half-byte for sign,
	   note: we can directly use |= as we set the last half-bte to the trailing low-value
	   we got from mpz_get_str above */
	p = data + f->size - 1;
	if (!COB_FIELD_HAVE_SIGN (f)) {
		*p |= 0x0F;
	} else if (sign == -1) {
		*p |= 0x0D;
	} else {
		*p |= 0x0C;
	}

	return 0;
}

/* set the specified BCD field 'f' to the given unsigned long 'val';
   note: the scale is ignored so has to be aligned up-front */
static void
cob_set_packed_u64 (cob_field *f, const cob_u64_t val, const int sign)
{
	register unsigned char	*p;
	register cob_u64_t	n = val;

	/* zero out storage; necessary as we stop below when reaching leading zero */
	memset (f->data, 0, f->size);

	/* set last byte (half-byte digit, half-byte sign */
	p = f->data + f->size - 1;
	if (!COB_FIELD_NO_SIGN_NIBBLE (f)) {
		*p = (n % 10) << 4;
		if (!COB_FIELD_HAVE_SIGN (f)) {
			*p |= 0x0F;
		} else if (sign == -1) {
			*p |= 0x0D;
		} else {
			*p |= 0x0C;
		}
		n /= 10;
		p--;
	}

	/* set packed digits from end to front, stopping when zero */
	for (; n && p >= f->data; n /= 100, p--) {
		*p = packed_bytes[n % 100];
	}

	/* clean first half-byte to handle truncation
	   as we may have stored 200 in S99's padding byte */
	if (p <= f->data) {
		const short	scale = COB_FIELD_SCALE (f);
		short	digits;
		if (scale >= 0) {
			digits = COB_FIELD_DIGITS (f);
		} else {
			/* 99P -> 3 digits, scale -1 --> real digits are less */
			digits = COB_FIELD_DIGITS (f) + scale;
		}
		if (COB_FIELD_NO_SIGN_NIBBLE (f)) {
			if ((digits % 2) == 1) {
				*(f->data) &= 0x0FU;
			}
			return;
		}
		if ((digits % 2) == 0) {
			*(f->data) &= 0x0FU;
		}
	}
}

/* set the specified BCD field 'f' to the given integer 'val';
   note: the scale is ignored so has to be aligned up-front */
void
cob_set_packed_int (cob_field *f, const int val)
{
	if (val > 0) {
		/* positive (most likely) */
		cob_set_packed_u64 (f, (cob_u64_t)val, 1);
	} else if (val != 0) {
		/* negative */
		cob_set_packed_u64 (f, (cob_u64_t)-val, -1);
	} else /* val == 0 */ {
		cob_set_packed_zero (f);
	}
	return;
}

/* DISPLAY */

static void
cob_decimal_set_display (cob_decimal *d, cob_field *f)
{
	/* Note: handling invalid data - as we do - leads to a performance
	   decrease of > 8% */

	register unsigned char	*data = COB_FIELD_DATA (f);
	register unsigned int	size = (unsigned int) COB_FIELD_SIZE (f);
	const int	sign = COB_GET_SIGN_ADJUST (f);

	/* TODO: document special cases here */
	if (*data == 255) {
		cob_pow_10 (d->value, size);
		d->scale = COB_FIELD_SCALE (f);
		return;
	}
	if (*data == 0) {
		cob_pow_10 (d->value, size);
		mpz_neg (d->value, d->value);
		d->scale = COB_FIELD_SCALE (f);
		return;
	}

	/* Skip leading zeros (also invalid space/low-value
	   and valid positive/negative zero overpunch) */
	while (size > 1 && (COB_D2I (*data) == 0)) {
		size--;
		data++;
	}

	/* Set value */

	if (size < MAX_LI_DIGITS_PLUS_1) {
		/* note: we skipped leading zeros above, so either
		   "n > 0" " or "size = 0" afterwards */
		register cob_uli_t	n = COB_D2I (*data);
		data++;
		while (--size) {
			n = n * 10
			  + COB_D2I (*data);
			data++;
		}
		mpz_set_ui (d->value, n);

	} else if (size <= COB_MAX_INTERMEDIATE_FLOATING_SIZE) {

		/* Note: we can get here for example when resolving
		   a decimal from huge internal fields like the
		   numeric functions sin/asin/... which have 96 digits
		   or during computations with division;
		   as we do integrate the big buffer there's no
		   use in having an extra branch for COB_MAX_DIGITS
		   other than the security part */
		char buff[COB_MAX_INTERMEDIATE_FLOATING_SIZE + 1];
		if (COB_FIELD_SIZE (f) <= COB_MAX_DIGITS) {
			/* original field size hints at likely user-defined
			   field, which may include invalid data */
			register char *pp = buff, *end = buff + size;
			while (pp < end) {
				*pp++ = COB_I2D (COB_D2I (*data++));
			}
			*pp = 0;
		} else {
			/* bigger fields _must_ be _internal_ so there's no need to handle
			   invalid data via COB_D2I + COB_I2D and we can copy as-is */
			memcpy (buff, data, size);
			/* still we may need to unpunch the sign, which in this internal
			   case will always be at the last digit */
			if (sign == -1) {
			 	char *p = buff + size - 1;
			 	*p = COB_I2D (COB_D2I (*p));
			}
			buff[size] = 0;
		}
		mpz_set_str (d->value, (char *)buff, 10);

	} else {

		/* Note: we get very seldom get here, commonly for computations with
		   intrinsic functions like cob_intr_variance;
		   these fields _must_ be _internal_ so there's no need to handle
		   invalid data via COB_D2I + COB_I2D and we can copy as-is;
		   this code has shown to be faster than mpz ui multiplication */
		char	*buff = cob_fast_malloc ((size_t)size + 1U);
		memcpy (buff, data, size);
		/* still we may need to unpunch the sign, which in this internal
		   case will always be at the last digit */
		if (sign == -1) {
		 	char *p = buff + size - 1;
		 	*p = COB_I2D (COB_D2I (*p));
		}
		buff[size] = 0;
		mpz_set_str (d->value, buff, 10);
		cob_free (buff);
	}

	/* Set sign and scale */
	if (sign < 0) {
		mpz_neg (d->value, d->value);
	}
	d->scale = COB_FIELD_SCALE (f);

	COB_PUT_SIGN_ADJUSTED (f, sign);
}

/* store value from decimal into field of type numeric DISPLAY */
static int
cob_decimal_get_display (cob_decimal *d, cob_field *f, const int opt)
{
	unsigned char	*data = COB_FIELD_DATA (f);
	const int		sign = mpz_sgn (d->value);
	const size_t	fsize = COB_FIELD_SIZE (f);
	char	buff[COB_MAX_BINARY + 1];

	/* check for value zero (allows early exit) and handle sign */
	if (sign == 0) {
		memset (data, '0', fsize);
		COB_PUT_SIGN (f, 0);
		return 0;
	}
	if (sign == -1) {
		mpz_abs (d->value, d->value);
	}
	/* Build string, note: we can't check the decimal size with
	   mpz_sizeinbase, as its result is "either exact or one too big" */

	/* huge data, only for internal operations like intrinsic functions,
	   for example when directly called within intrinsic.c by
	   cob_alloc_field, cob_decimal_get_field ->  cob_decimal_get_display */
	if (fsize > COB_MAX_BINARY) {
		char *p = mpz_get_str (NULL, 10, d->value);
		const size_t size = strlen (p);
		const long long diff = fsize - size;
		if (diff < 0) {
			/* Overflow */
			if ((opt & COB_STORE_NO_SIZE_ERROR) == 0) {
				cob_set_exception (COB_EC_SIZE_OVERFLOW);

				/* If the statement has ON SIZE ERROR, then throw
				   an exception, leaving the target unchanged */
				if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
					cob_gmp_free (p);
					return cobglobptr->cob_exception_code;
				}
			}

			/* Other size, truncate digits */
			memcpy (data, p - diff, fsize);
		} else {
			/* No overflow */
			memset (data, '0', diff);
			memcpy (data + diff, p, size);
		}

		cob_gmp_free (p);
		COB_PUT_SIGN (f, sign);
		return 0;
	}

	/* get divisor that would overflow */
	cob_pow_10 (cob_mexp, (unsigned int) fsize);
	/* check if it is >= what we have */
	if (mpz_cmp (d->value, cob_mexp) >= 0) {
		/* Overflow */
		if ((opt & COB_STORE_NO_SIZE_ERROR) == 0) {
			cob_set_exception (COB_EC_SIZE_OVERFLOW);

			/* If the statement has ON SIZE ERROR, then throw
			   an exception, leaving the target unchanged */
			if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
				return cobglobptr->cob_exception_code;
			}
		}
		/* Other size, truncate digits, using the remainder */
		mpz_tdiv_r (cob_mexp, d->value, cob_mexp);
		(void) mpz_get_str (buff, 10, cob_mexp);
		/* note: truncation may lead to 100012 be changed to 00012
		         in which case mpz_get_str provides us with 12 */
	} else {
		/* No overflow, so get string data and fill with zero */
		(void) mpz_get_str (buff, 10, d->value);
	}
	/* copy and fill left with zero */
	{
		size_t		size, diff;
		size = strlen (buff);
		diff = fsize - size;
		memset (data, '0', diff);
		memcpy (data + diff, buff, size);
	}
	COB_PUT_SIGN (f, sign);
	return 0;
}

/* BINARY field -> decimal */

static void
cob_decimal_set_binary (cob_decimal *d, cob_field *f)
{
	if (f->size > sizeof(cob_u64_t)) {
		size_t	size;
		size_t	sizeb;
		int		idx, adj, order;
		cob_u64_t	data[2];
		unsigned char	*p;

		size = f->size;
#ifndef WORDS_BIGENDIAN
		if (!COB_FIELD_BINARY_SWAP (f)) {
			sizeb = size - 1;
			order = -1;			/* Little Endian */
			adj = 0;
		} else {
			sizeb = 0;
			order = 1;			/* Big Endian */
			adj = (sizeof (cob_u64_t) * 2) - size;
		}
#else
		sizeb = 0;
		order = 1;
		adj = 0;
#endif
		p = (unsigned char *)data;
		data[0] = data[1] = 0;
		if (COB_FIELD_HAVE_SIGN (f)
		 && (f->data[sizeb] & 0x80U)) {
			for (idx = 0; idx < size; ++idx) {
				p[idx+adj] = ~f->data[idx];
			}
			p[sizeb] &= 0x7F;
			mpz_import (d->value, 2, order, sizeof(cob_u64_t), order, 0, p);
			mpz_neg (d->value, d->value);
		} else {
			memcpy (p+adj, f->data, (size_t)f->size);
			mpz_import (d->value, 2, order, sizeof(cob_u64_t), order, 0, data);
		}
		d->scale = COB_FIELD_SCALE(f);
		return;
	}

#if	defined(COB_LI_IS_LL)
	if (COB_FIELD_HAVE_SIGN (f)) {
		mpz_set_si (d->value, cob_binary_get_sint64 (f));
	} else {
		mpz_set_ui (d->value, cob_binary_get_uint64 (f));
	}
#else
	{
		cob_u64_t		uval;
		cob_s64_t		val;
		size_t			negative;

		if (f->size <= 4) {
			if (COB_FIELD_HAVE_SIGN (f)) {
				mpz_set_si (d->value, (cob_sli_t) cob_binary_get_sint64 (f));
			} else {
				mpz_set_ui (d->value, (cob_uli_t) cob_binary_get_uint64 (f));
			}
		} else {
			negative = 0;
			if (COB_FIELD_HAVE_SIGN (f)) {
				val = cob_binary_get_sint64 (f);
				if (val < 0) {
					negative = 1;
					uval = (cob_u64_t)-val;
				} else {
					uval = (cob_u64_t)val;
				}
			} else {
				uval = cob_binary_get_uint64 (f);
			}
			mpz_set_ui (d->value, (cob_uli_t)(uval >> 32));
			mpz_mul_2exp (d->value, d->value, 32);
			mpz_add_ui (d->value, d->value, (cob_uli_t)(uval & 0xFFFFFFFFU));
			if (negative) {
				mpz_neg (d->value, d->value);
			}
		}
	}
#endif
	d->scale = COB_FIELD_SCALE (f);
}

static int
cob_decimal_get_binary (cob_decimal *d, cob_field *f, const int opt)
{
	const int	field_sign = COB_FIELD_HAVE_SIGN (f);
	const size_t	bitnum = (f->size * 8) - field_sign;
	size_t			overflow;

	if (mpz_size (d->value) == 0) {
		memset (f->data, 0, f->size);
		return 0;
	}
	overflow = 0;
	if (!field_sign
	 && mpz_sgn (d->value) == -1) {
		mpz_abs (d->value, d->value);
	}
	if (f->size > sizeof(cob_u64_t)) {
		int	neg;
		size_t	size;
		size_t	sizeb;
		size_t	count;
		int		idx, adj, order;
		cob_s64_t	data[2];
		unsigned char	*p;
		data[0] = data[1] = 0;
		size = f->size;
#ifndef WORDS_BIGENDIAN
		if (!COB_FIELD_BINARY_SWAP (f)) {
			sizeb = size - 1;
			order = -1;
			adj = 0;
		} else {
			sizeb = 0;
			order = 1;
			adj = (sizeof (cob_u64_t) * 2) - size;
		}
#else
		sizeb = 0;
		order = 1;
		adj = 0;
#endif
		neg = (int)mpz_sgn (d->value);
		/* Returns 0 when value is 0 */
		if (!neg) {
			memset (f->data, 0, (size_t)f->size);
			return 0;
		}
		mpz_export (data, &count, order, sizeof(cob_s64_t), order, (size_t)0, d->value);
		if (count == 1
		 && order == 1) {
			data[1] = data[0];
			data[0] = 0;
		}
		p = (unsigned char *)data;
		p += adj;
		memcpy (f->data, p, (size_t)f->size);
		if (neg  == -1) {
			for (idx = 0; idx < f->size; ++idx) {
				f->data[idx] = ~f->data[idx];
			}
			f->data[sizeb] |= 0x80;
		}
		return 0;
	}
	if (mpz_sizeinbase (d->value, 2) > bitnum) {
		if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
			goto overflow;
		}
		overflow = 1;
		/* Check if truncation to PIC digits is needed */
		if (opt & COB_STORE_TRUNC_ON_OVERFLOW) {
			const short	scale = COB_FIELD_SCALE (f);
			unsigned short	digits;
			if (scale >= 0) {
				digits = COB_FIELD_DIGITS (f);
			} else {
				/* 99P -> 3 digits, scale -1 --> real digits are less */
				digits = COB_FIELD_DIGITS (f) + scale;
			}
			mpz_tdiv_r (d->value, d->value, cob_mpze10[digits]);
		} else {
			mpz_fdiv_r_2exp (d->value, d->value, (f->size * 8));
		}
	} else if (opt && COB_FIELD_BINARY_TRUNC (f)) {
		const short	scale = COB_FIELD_SCALE (f);
		unsigned short	digits;
		if (scale >= 0) {
			digits = COB_FIELD_DIGITS (f);
		} else {
			/* 99P -> 3 digits, scale -1 --> real digits are less */
			digits = COB_FIELD_DIGITS (f) + scale;
		}
		if (mpz_cmpabs (d->value, cob_mpze10[digits]) >= 0) {
			/* Overflow */
			if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
				goto overflow;
			}
			overflow = 1;
			/* Check if truncation to PIC digits is needed */
			if (opt & COB_STORE_TRUNC_ON_OVERFLOW) {
				mpz_tdiv_r (d->value, d->value,
					    cob_mpze10[digits]);
			} else {
				mpz_fdiv_r_2exp (d->value, d->value, (f->size * 8));
			}
		}
	}
#ifdef	COB_LI_IS_LL
	if (!field_sign || (overflow && !(opt & COB_STORE_TRUNC_ON_OVERFLOW))) {
		cob_binary_set_uint64 (f, mpz_get_ui (d->value));
	} else {
		cob_binary_set_int64 (f, mpz_get_si (d->value));
	}
#else
	if (f->size <= 4) {
		if (!field_sign || (overflow && !(opt & COB_STORE_TRUNC_ON_OVERFLOW))) {
			cob_binary_set_uint64 (f, mpz_get_ui (d->value));
		} else {
			cob_binary_set_int64 (f, mpz_get_si (d->value));
		}
	} else {
		unsigned int	lo;
		mpz_fdiv_r_2exp (cob_mpzt, d->value, 32);
		mpz_fdiv_q_2exp (d->value, d->value, 32);
		lo = (unsigned int) mpz_get_ui (cob_mpzt);

		if (!field_sign || (overflow && !(opt & COB_STORE_TRUNC_ON_OVERFLOW))) {
			cob_u64_t	ullval = mpz_get_ui (d->value);
			ullval = (ullval << 32) | lo;
			cob_binary_set_uint64 (f, ullval);
		} else {
			cob_s64_t	llval = mpz_get_si (d->value);
			llval = (llval << 32) | lo;
			cob_binary_set_int64 (f, llval);
		}
	}
#endif
	if (!overflow) {
		return 0;
	}

overflow:
	/* Overflow */
	if ((opt & COB_STORE_NO_SIZE_ERROR) == 0) {
		cob_set_exception (COB_EC_SIZE_OVERFLOW);
	}
	return cobglobptr->cob_exception_code;
}

/* General field -> decimal */

void
cob_decimal_set_field (cob_decimal *dec, cob_field *field)
{
	switch (COB_FIELD_TYPE (field)) {
	case COB_TYPE_NUMERIC_BINARY:
	case COB_TYPE_NUMERIC_COMP5:
		cob_decimal_set_binary (dec, field);
		break;
	case COB_TYPE_NUMERIC_PACKED:
		cob_decimal_set_packed (dec, field);
		break;
	case COB_TYPE_NUMERIC_FLOAT:
		{
			float	fval;
			memcpy ((void *)&fval, field->data, sizeof(float));
			cob_decimal_set_double (dec, (double)fval);
			break;
		}
	case COB_TYPE_NUMERIC_DOUBLE:
		{
			double	dval;
			memcpy ((void *)&dval, field->data, sizeof(double));
			cob_decimal_set_double (dec, dval);
			break;
		}
	case COB_TYPE_NUMERIC_L_DOUBLE:
		{
			long double lval;
			double	dval;
			memcpy ((void *)&lval, field->data, sizeof(long double));
			dval = (double)lval;	/* need internal switching to mpfr ... */
			cob_decimal_set_double (dec, dval);
			break;
		}
	case COB_TYPE_NUMERIC_FP_DEC64:
		cob_decimal_set_ieee64dec (dec, field);
		break;
	case COB_TYPE_NUMERIC_FP_DEC128:
		cob_decimal_set_ieee128dec (dec, field);
		break;
	default:
		cob_decimal_set_display (dec, field);
		break;
	}
}

/* note: currently (GC3.1) only called by display/dump
   code from termio.c, with field type
   COB_TYPE_NUMERIC_FP_DEC64/COB_TYPE_NUMERIC_FP_DEC128 */
void
cob_print_ieeedec (const cob_field *f, FILE *fp)
{
	switch (COB_FIELD_TYPE (f)) {
	case COB_TYPE_NUMERIC_FP_DEC64:
		cob_decimal_set_ieee64dec (&cob_d3, f);
		break;
	case COB_TYPE_NUMERIC_FP_DEC128:
		cob_decimal_set_ieee128dec (&cob_d3, f);
		break;
	case COB_TYPE_NUMERIC_FLOAT:
		{
			float	fval;
			memcpy ((void *)&fval, f->data, sizeof(float));
			cob_decimal_set_double (&cob_d3, (double)fval);
			break;
		}
	case COB_TYPE_NUMERIC_DOUBLE:
		{
			double	dval;
			memcpy ((void *)&dval, f->data, sizeof(double));
			cob_decimal_set_double (&cob_d3, dval);
			break;
		}
	case COB_TYPE_NUMERIC_L_DOUBLE:
		{
			long double lval;
			double	dval;
			memcpy ((void *)&lval, f->data, sizeof(long double));
			dval = (double)lval;
			cob_decimal_set_double (&cob_d3, dval);
			break;
		}
	/* LCOV_EXCL_START */
	default:
		cob_runtime_error (_("invalid internal call of %s"), "cob_print_ieeedec");
		cob_hard_failure_internal ("libcob");
	/* LCOV_EXCL_STOP */
	}
	cob_decimal_print (&cob_d3, fp);
}

void
cob_print_realbin (const cob_field *f, FILE *fp, const int size)
{
	if (f->size > sizeof(cob_u64_t)) {
		cob_decimal_set_binary (&cob_d3, (cob_field*)f);
		cob_decimal_print (&cob_d3, fp);
		return;
	}

	if (COB_FIELD_HAVE_SIGN (f)) {
		const cob_s64_t val = cob_binary_get_sint64 (f);
		fprintf (fp, CB_FMT_PLLD, size, size, val);
	} else {
		const cob_u64_t	uval = cob_binary_get_uint64 (f);
		fprintf (fp, CB_FMT_PLLU, size, size, uval);
	}
}

/* do rounding on the decimal,
   returns 1 if needed and PROHIBITED */
static int
cob_decimal_do_round (cob_decimal *d, cob_field *f, const int opt)
{
	const int	sign = mpz_sgn (d->value);
	const int	scale = COB_FIELD_SCALE (f);
	unsigned int	adj;	/* scale adjustment */

	/* Nothing to do when value is 0 or when target has GE scale */
	if (sign == 0
	 || scale >= d->scale) {
		return 0;
	}

	switch (opt & ~(COB_STORE_MASK)) {
	case COB_STORE_TRUNCATION:
		return 0;
	case COB_STORE_PROHIBITED:
		return 1;
	case COB_STORE_AWAY_FROM_ZERO:
		adj = d->scale - scale;
		cob_pow_10 (cob_mpzt, adj);
		mpz_tdiv_r (cob_mpzt2, d->value, cob_mpzt);
		if (mpz_sgn (cob_mpzt2) != 0) {
			/* Not exact number */
			if (sign == -1) {
				mpz_sub (d->value, d->value, cob_mpzt);
			} else {
				mpz_add (d->value, d->value, cob_mpzt);
			}
		}
		return 0;
	case COB_STORE_NEAR_TOWARD_ZERO:
		adj = d->scale - scale - 1;
		cob_pow_10 (cob_mpzt, adj);
		mpz_mul_ui (cob_mpzt, cob_mpzt, 5UL);
		mpz_tdiv_r (cob_mpzt2, d->value, cob_mpzt);
		{
			int n = scale - d->scale + 1;
			if (n != 0) {
				shift_decimal (d, n);
			}
		}
		if (mpz_sgn (cob_mpzt2) == 0) {
			return 0;
		}
		if (sign == 1) {
			mpz_add_ui (d->value, d->value, 5UL);
		} else {
			mpz_sub_ui (d->value, d->value, 5UL);
		}
		return 0;
	case COB_STORE_TOWARD_GREATER:
		adj = d->scale - scale;
		cob_pow_10 (cob_mpzt, adj);
		mpz_tdiv_r (cob_mpzt2, d->value, cob_mpzt);
		if (mpz_sgn (cob_mpzt2) != 0) {
			/* Not exact number */
			if (sign == 1) {
				mpz_add (d->value, d->value, cob_mpzt);
			}
		}
		return 0;
	case COB_STORE_TOWARD_LESSER:
		adj = d->scale - scale;
		cob_pow_10 (cob_mpzt, adj);
		mpz_tdiv_r (cob_mpzt2, d->value, cob_mpzt);
		if (mpz_sgn (cob_mpzt2) != 0) {
			/* Not exact number */
			if (sign == -1) {
				mpz_sub (d->value, d->value, cob_mpzt);
			}
		}
		return 0;
	case COB_STORE_NEAR_EVEN:
		adj = d->scale - scale - 1;
		cob_pow_10 (cob_mpzt, adj);
		mpz_mul_ui (cob_mpzt, cob_mpzt, 5UL);
		mpz_tdiv_r (cob_mpzt, d->value, cob_mpzt);
		{
			int n = scale - d->scale + 1;
			if (n != 0) {
				shift_decimal (d, n);
			}
		}
		if (mpz_sgn (cob_mpzt) == 0) {
			adj = (unsigned int) mpz_tdiv_ui (d->value, 100UL);
			switch (adj) {
			case 5:
			case 25:
			case 45:
			case 65:
			case 85:
				return 0;
			}
		}
		if (sign == 1) {
			mpz_add_ui (d->value, d->value, 5UL);
		} else {
			mpz_sub_ui (d->value, d->value, 5UL);
		}
		return 0;
	case COB_STORE_NEAR_AWAY_FROM_ZERO:
	default:
		{
			int n = scale - d->scale + 1;
			if (n != 0) {
				shift_decimal (d, n);
			}
		}
		if (sign == 1) {
			mpz_add_ui (d->value, d->value, 5UL);
		} else {
			mpz_sub_ui (d->value, d->value, 5UL);
		}
		return 0;
	}
}

int
cob_decimal_get_field (cob_decimal *d, cob_field *f, const int opt)
{
	if (d->scale == COB_DECIMAL_NAN) {
		if (!cobglobptr->cob_exception_code
		 || !cob_last_exception_is (COB_EC_SIZE_ZERO_DIVIDE)) {
			cob_set_exception (COB_EC_SIZE_OVERFLOW);
		}
		return cobglobptr->cob_exception_code;
	}
	if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
		if (d->scale == COB_DECIMAL_INF) {
			cob_set_exception (COB_EC_SIZE_OVERFLOW);
			return cobglobptr->cob_exception_code;
		}
	}

	/* work copy */
	if (d != &cob_d1) {
		mpz_set (cob_d1.value, d->value);
		cob_d1.scale = d->scale;
		d = &cob_d1;
	}

	/* Rounding */
	if ((opt & COB_STORE_ROUND)) {
		if (cob_decimal_do_round (d, f, opt)) {
			cob_set_exception (COB_EC_SIZE_TRUNCATION);
			return cobglobptr->cob_exception_code;
		}
	}
	if (!COB_FIELD_IS_FP (f)) {
		/* Append or truncate decimal digits - useless for floating point */
		int n = COB_FIELD_SCALE (f) - d->scale;
		if (n != 0) {
			if (mpz_sgn (d->value) == 0) {
				d->scale = 0;
			} else {
				shift_decimal (d, n);
			}
		}
	}

	/* Store number */
	switch (COB_FIELD_TYPE (f)) {
	case COB_TYPE_NUMERIC_BINARY:
	case COB_TYPE_NUMERIC_COMP5:
		return cob_decimal_get_binary (d, f, opt);
	case COB_TYPE_NUMERIC_DISPLAY:
		return cob_decimal_get_display (d, f, opt);
	case COB_TYPE_NUMERIC_PACKED:
		return cob_decimal_get_packed (d, f, opt);
	case COB_TYPE_NUMERIC_FLOAT:
		{
			const float	fval = (float) cob_decimal_get_double (d);
			if ((opt & COB_STORE_KEEP_ON_OVERFLOW)
			 && (isinf (fval) || isnan(fval))) {
				cob_set_exception (COB_EC_SIZE_OVERFLOW);
				return cobglobptr->cob_exception_code;
			}
			if ((opt & COB_STORE_KEEP_ON_OVERFLOW)
			 && cob_not_finite) {
				cob_set_exception (COB_EC_SIZE_OVERFLOW);
				return cobglobptr->cob_exception_code;
			}
			memcpy (f->data, &fval, sizeof (float));
			return 0;
		}
	case COB_TYPE_NUMERIC_DOUBLE:
		{
			const double val = cob_decimal_get_double (d);
			if ((opt & COB_STORE_KEEP_ON_OVERFLOW)
			 && (isinf (val) || isnan (val))) {
				cob_set_exception (COB_EC_SIZE_OVERFLOW);
				return cobglobptr->cob_exception_code;
			}
			if ((opt & COB_STORE_KEEP_ON_OVERFLOW)
			 && cob_not_finite) {
				cob_set_exception (COB_EC_SIZE_OVERFLOW);
				return cobglobptr->cob_exception_code;
			}
			memcpy (f->data, &val, sizeof (double));
			return 0;
		}
	case COB_TYPE_NUMERIC_L_DOUBLE:
		{
			const double val = cob_decimal_get_double (d);
			const long double lval = val;
			if ((opt & COB_STORE_KEEP_ON_OVERFLOW)
			 && (isinf (val) || isnan (val))) {
				cob_set_exception (COB_EC_SIZE_OVERFLOW);
				return cobglobptr->cob_exception_code;
			}
			if ((opt & COB_STORE_KEEP_ON_OVERFLOW)
			 && cob_not_finite) {
				cob_set_exception (COB_EC_SIZE_OVERFLOW);
				return cobglobptr->cob_exception_code;
			}
			memcpy (f->data, &lval, sizeof (long double));
			return 0;
		}
	case COB_TYPE_NUMERIC_FP_DEC64:
		return cob_decimal_get_ieee64dec (d, f, opt);
	case COB_TYPE_NUMERIC_FP_DEC128:
		return cob_decimal_get_ieee128dec (d, f, opt);
	default:
		{
			cob_field		temp;
			cob_field_attr		attr;
			char buffer[COB_MAX_DIGITS];
			COB_ATTR_INIT (COB_TYPE_NUMERIC_DISPLAY, COB_FIELD_DIGITS(f),
					COB_FIELD_SCALE (f), COB_FLAG_HAVE_SIGN, NULL);
			temp.size = COB_FIELD_DIGITS(f);
			temp.data = (unsigned char *) &buffer;
			temp.attr = &attr;
			if (cob_decimal_get_display (d, &temp, opt) != 0) {
				return cobglobptr->cob_exception_code;
			}
			cob_move (&temp, f);
			return 0;
		}
	}
}

/* Decimal arithmetic */

void
cob_decimal_add (cob_decimal *d1, cob_decimal *d2)
{
	DECIMAL_CHECK (d1, d2);
	if (d1->scale != d2->scale) {
		if (mpz_sgn (d2->value) == 0) {
			return;
		}
		if (mpz_sgn (d1->value) == 0) {
			mpz_set (d1->value, d2->value);
			d1->scale = d2->scale;
			return;
		}
		mpz_set (cob_t2.value, d2->value);
		cob_t2.scale = d2->scale;
		align_decimal (d1, &cob_t2);
		mpz_add (d1->value, d1->value, cob_t2.value);
	} else {
		mpz_add (d1->value, d1->value, d2->value);
	}
}

void
cob_decimal_sub (cob_decimal *d1, cob_decimal *d2)
{
	DECIMAL_CHECK (d1, d2);
	if (d1->scale != d2->scale) {
		if (mpz_sgn (d2->value) == 0) {
			return;
		}
		mpz_set (cob_t2.value, d2->value);
		cob_t2.scale = d2->scale;
		align_decimal (d1, &cob_t2);
		mpz_sub (d1->value, d1->value, cob_t2.value);
	} else {
		mpz_sub (d1->value, d1->value, d2->value);
	}
}

/* Decimal <-> Decimal */

/* note: this will be removed in 4.x, only is in for
   post 3.x decimal patch, not used with 3.2 any more
   but possibly from old generated modules */
void
cob_decimal_set (cob_decimal *dst, cob_decimal *src)
{
	mpz_set (dst->value, src->value);
	dst->scale = src->scale;
}

void
cob_decimal_mul (cob_decimal *d1, cob_decimal *d2)
{
	DECIMAL_CHECK (d1, d2);
	d1->scale += d2->scale;
	mpz_mul (d1->value, d1->value, d2->value);
}

void
cob_decimal_div (cob_decimal *d1, cob_decimal *d2)
{
	DECIMAL_CHECK (d1, d2);

	/* Check for division by zero */
	if (mpz_sgn (d2->value) == 0) {
		d1->scale = COB_DECIMAL_NAN;

		/* FIXME: we currently don't handle the fatal exception correct
		   fatal->abort. We only should set it when it *doesn't* happen
		   within a arithmetic statement with SIZE error phrase and must
		   execute the appropriate USE statement, if any before the abort
		*/

		if (cobglobptr->cob_exception_code == -1) {
			cob_set_exception (COB_EC_SIZE_ZERO_DIVIDE);
			cob_fatal_error (COB_FERROR_DIV_ZERO);
		}

		cob_set_exception (COB_EC_SIZE_ZERO_DIVIDE);
		return;
	}
	if (mpz_sgn (d1->value) == 0) {
		d1->scale = 0;
		return;
	}
	d1->scale -= d2->scale;
	shift_decimal (d1, COB_MAX_DIGITS + ((d1->scale < 0) ? -d1->scale : 0));
	mpz_tdiv_q (d1->value, d1->value, d2->value);
}

int
cob_decimal_cmp (cob_decimal *d1, cob_decimal *d2)
{
	if (d1->scale != d2->scale) {
		mpz_set (cob_t1.value, d1->value);
		cob_t1.scale = d1->scale;
		mpz_set (cob_t2.value, d2->value);
		cob_t2.scale = d2->scale;
		align_decimal (&cob_t1, &cob_t2);
		return mpz_cmp (cob_t1.value, cob_t2.value);
	}
	return mpz_cmp (d1->value, d2->value);
}

/*
 * Shift 'd1' to have same scale as 'd2'
 */
void
cob_decimal_align (cob_decimal *d1, const int scale)
{
	if (d1->scale > scale) {
		shift_decimal (d1, scale - d1->scale);
	} else if (d1->scale < scale) {
		shift_decimal (d1, d1->scale - scale);
	}
}

/* Convenience functions that use either an optimized approach
   or internally convert to decimals as needed */

static int	cob_add_bcd (cob_field *fdst, cob_field *fsrc1, cob_field *fsrc2,
	int opt, const enum cob_statement stmt);

/* optimized ADD / SUBTRACT based on optimized BCD arithmetic */
static COB_INLINE COB_A_INLINE int
cob_addsub_optimized (cob_field *f1, cob_field *f2,
	const int opt, const enum cob_statement stmt)
{
	const unsigned short f1_type = COB_FIELD_TYPE (f1);
	const unsigned short f2_type = COB_FIELD_TYPE (f2);
	if (f1_type == COB_TYPE_NUMERIC_PACKED) {
		if (f2_type == COB_TYPE_NUMERIC_PACKED) {
			cob_add_bcd (f1, f2, f1, opt, stmt);
			return 1;
		} else {
			const unsigned short f2_digits = COB_FIELD_DIGITS (f2);
			if ((f2_type == COB_TYPE_NUMERIC_DISPLAY
			   && f2_digits <= COB_MAX_DIGITS)
			 || f2_type == COB_TYPE_NUMERIC_BINARY
			 || f2_type == COB_TYPE_NUMERIC_COMP5) {
				cob_field	field;
				cob_field_attr	attr;
				unsigned char buff[COB_MAX_DIGITS / 2 + 1];
				COB_FIELD_INIT (f2_digits / 2 + 1, buff, &attr);
				COB_ATTR_INIT (COB_TYPE_NUMERIC_PACKED, f2_digits,
					COB_FIELD_SCALE (f2), COB_FLAG_HAVE_SIGN, NULL);
				if (f2_type == COB_TYPE_NUMERIC_DISPLAY) {
					cob_move_display_to_packed (f2, &field);
				} else {
					/* wrapper handling P and other things */
					cob_move (f2, &field);
				}
				cob_add_bcd (f1, &field, f1, opt, stmt);
				return 1;
			}
		}
#if 0	/* on heavy computations this is (only) ~3% faster, with
    	   move_display_to_packed taking nearly 30%; come back later */
	} else
	if (f1_type == COB_TYPE_NUMERIC_DISPLAY) {
		const unsigned short f1_digits = COB_FIELD_DIGITS (f1);
		if (f1_digits <= COB_MAX_DIGITS) {
			cob_field	field1;
			cob_field_attr	attr1;
			unsigned char buff1[COB_MAX_DIGITS / 2 + 1];
			COB_FIELD_INIT_F (field1, f1_digits / 2 + 1, buff1, &attr1);
			COB_ATTR_INIT_A (attr1, COB_TYPE_NUMERIC_PACKED, f1_digits,
				COB_FIELD_SCALE (f1), COB_FLAG_HAVE_SIGN, NULL);
			if (f2_type == COB_TYPE_NUMERIC_PACKED) {
				cob_move_display_to_packed (f1, &field1);
				cob_add_bcd (&field1, f2, &field1, opt, stmt);
				cob_move_packed_to_display (&field1, f1);
				return 1;
			} else {
				const unsigned short f2_digits = COB_FIELD_DIGITS (f2);
				if ((f2_type == COB_TYPE_NUMERIC_DISPLAY
				   && f2_digits <= COB_MAX_DIGITS)
				 || f2_type == COB_TYPE_NUMERIC_BINARY
				 || f2_type == COB_TYPE_NUMERIC_COMP5) {
					cob_field	field2;
					cob_field_attr	attr2;
					unsigned char buff2[COB_MAX_DIGITS / 2 + 1];
					COB_FIELD_INIT_F (field2, f2_digits / 2 + 1, buff2, &attr2);
					COB_ATTR_INIT_A (attr2, COB_TYPE_NUMERIC_PACKED, f2_digits,
						COB_FIELD_SCALE (f2), COB_FLAG_HAVE_SIGN, NULL);
					cob_move_display_to_packed (f1, &field1);
					if (f2_type == COB_TYPE_NUMERIC_DISPLAY) {
						cob_move_display_to_packed (f2, &field2);
					} else {
						/* wrapper handling P and other things */
						cob_move (f2, &field2);
					}
					cob_move (f2, &field2);
					cob_add_bcd (&field1, &field2, &field1, opt, stmt);
					cob_move_packed_to_display (&field1, f1);
					return 1;
				}
			}
		}
#endif
	}

	/* no optimization done, execute ADD/SUBTRACT in the caller */
	return 0;
}

void
cob_add (cob_field *f1, cob_field *f2, const int opt)
{
	if (cob_addsub_optimized (f1, f2, opt, STMT_ADD)) {
		return;	/* optimized ADD done, get out */
	}
	cob_decimal_set_field (&cob_d1, f1);
	cob_decimal_set_field (&cob_d2, f2);
	cob_decimal_add (&cob_d1, &cob_d2);
	(void)cob_decimal_get_field (&cob_d1, f1, opt);
}

void
cob_sub (cob_field *f1, cob_field *f2, const int opt)
{
	if (cob_addsub_optimized (f1, f2, opt, STMT_SUBTRACT)) {
		return;	/* optimized SUBTRACT done, get out */
	}
	cob_decimal_set_field (&cob_d1, f1);
	cob_decimal_set_field (&cob_d2, f2);
	cob_decimal_sub (&cob_d1, &cob_d2);
	(void)cob_decimal_get_field (&cob_d1, f1, opt);
}

void
cob_mul (cob_field *f1, cob_field *f2, const int opt)
{
	cob_decimal_set_field (&cob_d1, f1);
	cob_decimal_set_field (&cob_d2, f2);
	cob_decimal_mul (&cob_d1, &cob_d2);
	(void)cob_decimal_get_field (&cob_d1, f1, opt);
}

void
cob_div (cob_field *f1, cob_field *f2, const int opt)
{
	cob_decimal_set_field (&cob_d1, f1);
	cob_decimal_set_field (&cob_d2, f2);
	cob_decimal_div (&cob_d1, &cob_d2);
	(void)cob_decimal_get_field (&cob_d1, f1, opt);
}

void
cob_div_quotient (cob_field *dividend, cob_field *divisor,
		  cob_field *quotient, const int opt)
{
	/* Note that cob_div_quotient and cob_div_remainder must remain */
	/* separate because of COBOL rules. The quotient must be fully */
	/* evaluated before the remainder item is evaluated */
	/* e.g. DIVIDE A BY B GIVING Z REMAINDER FLD (Z). */

	cob_decimal_set_field (&cob_d1, dividend);
	cob_decimal_set_field (&cob_d2, divisor);
	mpz_set (cob_d_remainder.value, cob_d1.value);
	cob_d_remainder.scale = cob_d1.scale;

	/* Compute quotient */
	cob_decimal_div (&cob_d1, &cob_d2);
	/* Check divide by zero - Exception is set in cob_decimal_div */
	if (cob_d1.scale == COB_DECIMAL_NAN) {
		/* Forces an early return from cob_div_remainder */
		cob_d_remainder.scale = COB_DECIMAL_NAN;
		return;
	}

	/* Set quotient */
	mpz_set (cob_d3.value, cob_d1.value);
	cob_d3.scale = cob_d1.scale;
	(void)cob_decimal_get_field (&cob_d1, quotient, opt);

	/* Truncate digits from the quotient */
	{
		int n = COB_FIELD_SCALE (quotient) - cob_d3.scale;
		if (n != 0) {
			if (mpz_sgn (cob_d3.value) == 0) {
				cob_d3.scale = 0;
			} else {
				shift_decimal (&cob_d3, n);
			}
		}
	}

	/* Compute remainder */
	cob_decimal_mul (&cob_d3, &cob_d2);
	cob_decimal_sub (&cob_d_remainder, &cob_d3);
}

void
cob_div_remainder (cob_field *fld_remainder, const int opt)
{
	(void)cob_decimal_get_field (&cob_d_remainder, fld_remainder, opt);
}

/* internal MOVE handling by converting 'src' to cob_decimal,
   then converting that back to 'dst'
   with optional truncation as specified in 'opt';
   while this is quite expensive it converts between every numeric data type
   with every attribute possible */
void
cob_decimal_setget_fld (cob_field *src, cob_field *dst, const int opt)
{
	cob_decimal_set_field (&cob_d1, src);
	(void)cob_decimal_get_field (&cob_d1, dst, opt | COB_STORE_NO_SIZE_ERROR);
}

/* shift the complete filled buffer one nibble left
   with 'ptr_buff' pointing to the start of a fixed-size 48byte buffer to
   accomodate 3 64bit integers for use of register shifting
   note: the longest decimal field is 20 bytes long but we need the
         extra 4 bytes to allow the use of register to do the shifting */
static void
cob_shift_left_nibble (unsigned char *ptr_buff, unsigned char *ptr_start_data_byte)
{
	/* this logic is copied from the insert_packed_aligned function so that
	   any changes to that function should probaby require that this function
	   be examined for */

	unsigned char carry_nibble;
	unsigned char move_nibble = 0xFF;

	/* calculate the length of data to be shifted, never much */
	const int len1 = 48 - (int)(ptr_start_data_byte - ptr_buff);

	/* add one to ensure the carry nibble is moved */
	register int shift_cntr = len1 + 1;

	/* point at the last byte in buffer as we will shift from right to left !! */
	register cob_u64_t *ptr_long = (cob_u64_t *)(ptr_buff + 48 - 8);

# ifndef  WORDS_BIGENDIAN
	cob_u64_t chunk;
# endif

	do {
# ifdef WORDS_BIGENDIAN
		/* shift and include old nibble */
		carry_nibble = (unsigned char)(*ptr_long >> 60);
		*ptr_long = (*ptr_long << 4);
		if (shift_cntr < len1) {
			*ptr_long |= move_nibble;
		}
# else
		/* load data to chunk, swap as necessary */
		chunk = COB_BSWAP_64 (*ptr_long);
		/* shift and include old nibble */
		carry_nibble = (unsigned char)(chunk >> 60);
		chunk = (chunk << 4);
		if (shift_cntr < len1) {
			chunk |= move_nibble;
		}
		/* swap as necessary, place in memory */
		*ptr_long = COB_BSWAP_64 (chunk);
# endif
		/* prepare for next round */
		move_nibble = carry_nibble;
		shift_cntr -= 8;
		ptr_long--;
	} while (shift_cntr > 0);
}


/* shift the complete filled buffer one nibble right
   with 'ptr_buff' pointing to the start of a fixed-size 48byte buffer to
   accomodate 3 64bit integers for use of register shifting
   for more details see cob_shift_left_nibble;
   note: the difference in this routine is that it will shift from left to right
         so we need to start a the byte BEFORE the start data byte */
static void
cob_shift_right_nibble (unsigned char *ptr_buff, unsigned char *ptr_start_data_byte)
{
	/* this logic is copied from the insert_packed_aligned function so that
	   any changes to that function should probaby require that this function
	   be examined for */

# ifndef WORDS_BIGENDIAN
	cob_u64_t chunk;
# endif

	/* note that the carry & move nibbles have to be 64 bit because we need
	    to use binary OR the high order bits when shifting to the right !! */
	cob_u64_t carry_nibble;
	cob_u64_t move_nibble = 0xFF;

	/* calculate the length of data to be shifted, never much */
	const int len1 = 48 - (int)(ptr_start_data_byte - ptr_buff);

	register int shift_cntr = len1;

	/* note that since we are shifting from left to right we have to start in the
	   first 64 bit area containing the high order 64 bit integer which contains
	   the starting position of the data to be shifted */
	register cob_u64_t *ptr_long = (cob_u64_t *)(ptr_buff + 48);
	do {
		ptr_long--;
	} while (ptr_long > (cob_u64_t *)ptr_start_data_byte);	/* we want to be there - or before! */

	do {
# ifdef WORDS_BIGENDIAN
		/* shift and include old nibble */
		carry_nibble = *ptr_long << 60;
		*ptr_long = (*ptr_long >> 4);
		if (shift_cntr < len1) {
			*ptr_long |= move_nibble;
		}
# else
		/* load data to chunk, swap as necessary */
		chunk = COB_BSWAP_64 (*ptr_long);
		/* shift and include old nibble */
		carry_nibble = chunk << 60;
		chunk = (chunk >> 4);
		if (shift_cntr < len1) {
			chunk |= move_nibble;
		}
		/* swap as necessary, place in memory */
		*ptr_long = COB_BSWAP_64 (chunk);
# endif
		/* prepare for next round */
		move_nibble = carry_nibble;
		shift_cntr -= 8;
		ptr_long++;
	} while (shift_cntr > 0);
}


/* optimized MOVE between any BCD fields, no matter their attributes;
   TODO: add handling of negative scales to cob_move_bcd */
void
cob_move_bcd (cob_field *f1, cob_field *f2)
{
	/************************************************************/
	/*                                                          */
	/*  Note that this routine will first check to see if data  */
	/*  shifting is required to meet the format of the          */
	/*  receiving field. If not then the data will be moved     */
	/*  directly from the input field to the receiving field    */
	/*  without the use of an intermediate buffer. If shifting  */
	/*  is required in either direction, then a 48 byte buffer  */
	/*  will be allocated to do the shifting. Note that 48      */
	/*  bytes is more than needed but the addition function     */
	/*  requires 48 bytes to do its shifting so this way we     */
	/*  can use the same functions.                             */
	/*                                                          */
	/*  When moving data to the left we need to strip the sign  */
	/*  when moving as it would be in the middle of the         */
	/*  receiving field.                                        */
	/*                                                          */
	/************************************************************/

	unsigned char	*fld1 = COB_FIELD_DATA (f1);
	unsigned char	*fld2 = COB_FIELD_DATA (f2);
	const size_t	fld1_size =  f1->size;
	const size_t	fld2_size =  f2->size;
	const int 	f2_has_no_sign_nibble = COB_FIELD_NO_SIGN_NIBBLE (f2);

	signed short		fld1_scale, fld2_scale, diff, offset;
	unsigned char	fld1_sign;
	int		move_left;

	if (COB_FIELD_NO_SIGN_NIBBLE (f1)) {
		fld1_sign = 0x00;
	} else {
		fld1_sign = *(fld1 + fld1_size - 1) & 0x0F;
	}

	/************************************************************/
	/*  Note that the scale is increased by 1 because the sign nibble will */
	/*  be converted to a zero during the process of moving the data. The sign */
	/*  will be added at the end of the process.                */
	/************************************************************/

	fld1_scale = !fld1_sign
		? COB_FIELD_SCALE (f1) :
		COB_FIELD_SCALE (f1) + 1;

	fld2_scale = f2_has_no_sign_nibble
		? COB_FIELD_SCALE (f2) :
		COB_FIELD_SCALE (f2) + 1;

	if (fld1_scale > fld2_scale) {
		move_left = 0;
		diff = fld1_scale - fld2_scale;
	} else {
		move_left = 1;
		diff = fld2_scale - fld1_scale;
	}


	/************************************************************/
	/*  Note that when the scale difference is a multiple of 2  */
	/*  then there is NO SHIFTING required. So we can move the  */
	/*  sending field directly into the receiving field.        */
	/************************************************************/

	if (!(diff & 1)) {	 /* -> diff % 2 == 0 */
		offset = diff >> 1;
		memset (fld2, 0, fld2_size);
		if (move_left) {
			const size_t llen = fld2_size - offset;
			if (fld1_size <= llen) {
				memcpy (fld2 + llen - fld1_size, fld1, fld1_size);
				if (fld1_sign) {
					*(fld2 + fld2_size - offset - 1) &= 0xF0;
				}
			} else {
				memcpy (fld2, fld1 + fld1_size - llen, llen);
				if (fld1_sign) {
					*(fld2 + llen - 1) &= 0xF0;
				}
			}
		} else {
			const size_t llen = fld1_size - offset;
			if (llen <= fld2_size) {
				memcpy (fld2 + fld2_size - llen, fld1, llen);
			} else {
				memcpy (fld2, fld1 + fld1_size - offset - fld2_size, fld2_size);
			}
		}

	} else {

		/************************************************************/
		/*  Note that when the scale difference is NOT a multiple   */
		/*  of 2 then SHIFTING of 1 nibble is required. To          */
		/*  accomplish this we will move the data to a 48 byte      */
		/*  buffer to do the actual shifting of the data before     */
		/*  moving to the receiving field                           */
		/************************************************************/

		unsigned char buff[48] = { 0 };
		offset = diff >> 1;

		if (move_left) {
			const size_t llen = 48 - offset;
			unsigned char *pos = buff + llen - fld1_size;
			memcpy (pos, fld1, fld1_size);
			if (fld1_sign) {
				*(buff + llen - 1) &= 0xF0;
			}
			cob_shift_left_nibble (buff, pos);
		} else {
			const size_t llen = fld1_size - offset;
			unsigned char *pos = buff + 48 - llen;
			memcpy (pos, fld1, llen);
			if (fld1_sign) {
				*(buff + llen - 1) &= 0xF0;
			}
			cob_shift_right_nibble (buff, pos);
		}

		memcpy (fld2, buff + 48 - fld2_size, fld2_size);
	}

	if (f2_has_no_sign_nibble) {
		/* clear pad nibble, if present */
		if (COB_FIELD_DIGITS (f2) & 1 /* -> digits % 2 == 1 */) {
			*fld2 &= 0x0F;
		}
	} else {
		/* set sign nibble */
		unsigned char *pos = fld2 + fld2_size - 1;
		if (!COB_FIELD_HAVE_SIGN (f2)) {
			*pos |= 0x0F;
		} else if (fld1_sign == 0x0D) {
			*pos = (*pos & 0xF0) | 0x0D;
		} else {
			*pos = (*pos & 0xF0) | 0x0C;
		}
		/* clear pad nibble, if present */
		if (!(COB_FIELD_DIGITS (f2) & 1) /* -> digits % 2 == 0 */) {
			*fld2 &= 0x0F;
		}
	}

}


/* Optimized arithmetic for BCD fiields */

static unsigned char   h2b[256] =
{ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 255, 255, 255, 255, 255, 255,
  10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 255, 255, 255, 255, 255, 255,
  20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 255, 255, 255, 255, 255, 255,
  30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 255, 255, 255, 255, 255, 255,
  40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 255, 255, 255, 255, 255, 255,
  50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 255, 255, 255, 255, 255, 255,
  60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 255, 255, 255, 255, 255, 255,
  70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 255, 255, 255, 255, 255, 255,
  80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 255, 255, 255, 255, 255, 255,
  90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 255, 255, 255, 255, 255, 255 };

static unsigned char   b2h[256] =
{ 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
  0x08, 0x09, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15,
  0x16, 0x17, 0x18, 0x19, 0x20, 0x21, 0x22, 0x23,
  0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x30, 0x31,
  0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39,
  0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
  0x48, 0x49, 0x50, 0x51, 0x52, 0x53, 0x54, 0x55,
  0x56, 0x57, 0x58, 0x59, 0x60, 0x61, 0x62, 0x63,
  0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x70, 0x71,
  0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79,
  0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
  0x88, 0x89, 0x90, 0x91, 0x92, 0x93, 0x94, 0x95,
  0x96, 0x97, 0x98, 0x99, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff };


static COB_INLINE COB_A_INLINE int
handle_bcd_rounding (int *byte, const int round_half_nibble,
	const int all_zeros, const int final_positive,
	const int opt)
{
	unsigned char	round;
	if (*byte < 0) {
		round = b2h[*byte + 100];
	} else
	if (*byte >= 100) {
		round = b2h[*byte - 100];
	} else {
		round = b2h[*byte];
	}

	if (round_half_nibble) {
		switch (opt & ~COB_STORE_MASK) {
		case COB_STORE_PROHIBITED:
			if ((opt & COB_STORE_NO_SIZE_ERROR) == 0) {
				if (!all_zeros || (round & 0x0F)) {
					cob_set_exception (COB_EC_SIZE_TRUNCATION);
					/* cobglobptr->cob_exception_code returned by caller */
					return 1;
				}
				return COB_EC_SIZE_TRUNCATION;
			}
			break;
		case COB_STORE_AWAY_FROM_ZERO:
			if (!all_zeros || (round & 0x0F)) {
				*byte += 10;
			}
			break;
		case COB_STORE_NEAR_TOWARD_ZERO:
			if ( (round & 0x0F) > 0x05
			 || ((round & 0x0F) == 0x05 && !all_zeros)) {
				*byte += 10;
			}
			break;
		case COB_STORE_TOWARD_GREATER:
			if (final_positive)
				if (!all_zeros || (round & 0x0F)) {
					*byte += 10;
				}
			break;
		case COB_STORE_TOWARD_LESSER:
			if (!final_positive)
				if (!all_zeros || (round & 0x0F)) {
					*byte += 10;
				}
			break;
		case COB_STORE_NEAR_EVEN:
			if (all_zeros && (round & 0x0F) == 0x05) {
				if (round & 0x10) {
					*byte += 5;
				}
			} else {
				*byte += 5;
			}
			break;
		case COB_STORE_NEAR_AWAY_FROM_ZERO:
		default:
			*byte += 5;
			break;
		}
	} else {
		switch (opt & ~COB_STORE_MASK) {
		case COB_STORE_PROHIBITED:
			if ((opt & COB_STORE_NO_SIZE_ERROR) == 0) {
				if (!all_zeros || (round)) {
					cob_set_exception (COB_EC_SIZE_TRUNCATION);
					/* cobglobptr->cob_exception_code returned by caller */
					return 1;
				}
			}
			break;
		case COB_STORE_AWAY_FROM_ZERO:
			if (!all_zeros || (round)) {
				*byte += 100;
			}
			break;
		case COB_STORE_NEAR_TOWARD_ZERO:
			if ((round > 0x50)
			 || (round == 0x50 && !all_zeros)) {
				*byte += 100;
			}
			break;
		case COB_STORE_TOWARD_GREATER:
			if (final_positive && (!all_zeros || (round))) {
				*byte += 100;
			}
			break;
		case COB_STORE_TOWARD_LESSER:
			if (!final_positive && (!all_zeros || (round))) {
				*byte += 100;
			}
			break;
		case COB_STORE_NEAR_EVEN:
			if (all_zeros && (round == 0x50)) {
				return -2;	/* "check next *byte" */
			} else {
				*byte += 50;
			}
			break;
		case COB_STORE_NEAR_AWAY_FROM_ZERO:
		default:
			*byte += 50;
			break;
		}
	}
	return -1;
}

static COB_INLINE COB_A_INLINE int
count_leading_zeros (unsigned char *ptr_char, int len)
{
	int  cntr = 0;
	while (*ptr_char++ == 0x00 && cntr++ < len);
	return cntr;
}

/* check for overflow and set the final sign into the working buffer */
static int
check_overflow_and_set_sign (cob_field *f, const int opt,
	int final_positive, unsigned char *buff, unsigned char *pos)
{
	const int	buff_size = 48 - (int)(pos - buff);
	const int	fsize = (int)f->size;
	int         cmp_size = buff_size - fsize;
	unsigned char *last_buff_pos = buff + 48 - 1;

	if (!COB_FIELD_NO_SIGN_NIBBLE (f)) {
		*last_buff_pos &= 0xF0;
	}

	if (cmp_size >= 0) {
		const int	lead_zeros = count_leading_zeros (pos, buff_size);
		const int	has_pad_digit =
			   (COB_FIELD_DIGITS (f) % 2 == 1 &&  COB_FIELD_NO_SIGN_NIBBLE (f))
			|| (COB_FIELD_DIGITS (f) % 2 == 0 && !COB_FIELD_NO_SIGN_NIBBLE (f));

		cmp_size -= lead_zeros;

		if ((cmp_size > 0)	/* more data than possible to store - definitely an overflow */
		 || (cmp_size == 0	/* check if we only expect a single digit but have two set */
		  && has_pad_digit
		  && *(pos + lead_zeros) > 0x0F)) {
			/* Overflow */
			if ((opt & COB_STORE_NO_SIZE_ERROR) == 0) {
				cob_set_exception (COB_EC_SIZE_OVERFLOW);
				if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
					/* cobglobptr->cob_exception_code returned by caller */
					return 1;
				}
			}
			if (has_pad_digit) {
				/* check and set last leading digit if needed */
				*(buff + 48 - f->size) &= 0x0F;
			}
#if 0	/* keep negative zero */
			if (!final_positive
			 && COB_FIELD_HAVE_SIGN (f)) {
				/* check for all zero after truncation - have that as positive */
				unsigned char *end = buff + 48;
				pos = end - fsize;
				final_positive = 1;		/* "guess" it is zero */
				while (pos < end) {
					if (*pos++) {
						final_positive = 0;	/* no, it isn't - set back and get out */
						break;
					}
				}
			}
#endif
		}
	}

#if 0
	/*  check for special truncation error:
	    a negative value which the receiver field does not allow

		note: the COBOL standard "since ever" asks only for exceptions
		      if absolute values do not fit a missing sign is silently dropped;
			  therefore no exception is returned */

	if (!final_positive && !COB_FIELD_HAVE_SIGN (f)) {
		if (opt & COB_STORE_CHECK_SIGN_DROP) {
			cob_runtime_error ("negative result value with unsigned result-item");
		}
	}
#endif
	
	/* note: we did clean the sign from the buffer when entering the function */
	if (!COB_FIELD_NO_SIGN_NIBBLE (f)) {
		if (!COB_FIELD_HAVE_SIGN (f)) {
			*last_buff_pos |= 0x0F;
		} else
		if (final_positive) {
			*last_buff_pos |= 0x0C;
		} else {
			*last_buff_pos |= 0x0D;
		}
	}

	return 0;

}

/* BCD (COMP-3/COMP-6) ADD / SUBTRACT with GIVING, where
   the GIVING field may be one of the source fields */
static int
cob_add_bcd (cob_field *fdst,
	cob_field *fsrc1, cob_field *fsrc2,
	int opt, const enum cob_statement stmt)
{
	/* function overview:

	   This function internally converts the source fields to COMP-6
	   while moving them to intermediate buffers, aligning them to
	   the scale of the field with the largest scale and keeping track
	   of the signs with indicator variables.
	   Note that for the addition logic to work correctly, the field
	   with the largest absolute value must be the positive field,
	   we therefore reverse the signs of the fields if necessary.

	   Requested rounding is done during the addition operation
	   and finally the data is moved to the receiving field after
	   checking for overflow and setting the final result sign. */

	register int	byte;
	register int	carry = 0;
	register int	loop_limit, cntr;

	unsigned char	fld1_buff[48] = { 0 };
	unsigned char	fld2_buff[48] = { 0 };

	const int	fld1_size = (int)fsrc1->size;
	const int	fld2_size = (int)fsrc2->size;
	const int	dest_size = (int)fdst->size;

	const unsigned char *src1_data = COB_FIELD_DATA (fsrc1);
	const unsigned char *src2_data = COB_FIELD_DATA (fsrc2);

	unsigned char 	fld1_sign = COB_FIELD_NO_SIGN_NIBBLE (fsrc1) ? 0x00
	          : *(src1_data + fld1_size - 1) & 0x0F;
	const unsigned char 	fld2_sign = COB_FIELD_NO_SIGN_NIBBLE (fsrc2) ? 0x00
	          : *(src2_data + fld2_size - 1) & 0x0F;

	const signed short	src1_scale = COB_FIELD_SCALE (fsrc1);
	const signed short	src2_scale = COB_FIELD_SCALE (fsrc2);
	const signed short	dst_scale = COB_FIELD_SCALE (fdst);

	const signed short	used_original_scale
		= src1_scale > src2_scale ? src1_scale : src2_scale;

	const signed short	fld1_scale = (fld1_sign == 0x00) ? src1_scale : src1_scale + 1;
	const signed short	fld2_scale = (fld2_sign == 0x00) ? src2_scale : src2_scale + 1;
	const signed short	dest_scale = (COB_FIELD_NO_SIGN_NIBBLE (fdst)) ? dst_scale : dst_scale + 1;

	const signed short	used_scale
		= fld1_scale > fld2_scale ? fld1_scale : fld2_scale;

	unsigned char	*fld1, *fld2;
	unsigned char	*rslt, *pos, *neg;

	int		rounding_digit = 0;
	int		rounding_byte = 0;
	int		final_positive = 1;
	int		check_rounding = 0;
	int		check_all_zeros = 0;
	int		all_zeros = 0;

	/* the following code is used to determine if we need to check for rounding
	   and if so, then which digit and byte we need to check for this;
	   note that we need to know both because the addition loop processes
	   a byte at a time rather than a digit;
	   because some rounding operations need to determine if the value is exactly
	   between the next higher digit, we keep track of "all digits to the right
	   of the rounding digit are zero" */
	if ((used_original_scale <= dst_scale)
	 || (used_scale < dest_scale)
	 || (opt & COB_STORE_TRUNCATION)
	 || !(opt & ~COB_STORE_MASK)) {
		check_rounding = 0;
		check_all_zeros = 0;
		all_zeros = 0;
	} else {
		/* for all those rounding options we need we need a check for
		   the result being exactly between the two nearest values */
		if (opt
		  & ( COB_STORE_AWAY_FROM_ZERO | COB_STORE_PROHIBITED
		    | COB_STORE_TOWARD_GREATER | COB_STORE_TOWARD_LESSER
		    | COB_STORE_NEAR_TOWARD_ZERO)) {
			check_all_zeros = 1;
			all_zeros = 1;
		}
		/* note that when calculating the rounding digit it is 1 digit to the right
		   of the last digit in the data which is kept;
		   also because there are 2 digits in every byte, before dividing by 2 we
		   need to add 1 so that we find the byte number where the rounding digit
		   exists; we finally subtract 1 to get to the C offzet (zero based) */
		rounding_digit = used_scale - dest_scale;
		if (!COB_FIELD_NO_SIGN_NIBBLE (fdst)) {
			rounding_digit += 1;
		}
		rounding_byte = (rounding_digit + 1) / 2;
		rounding_byte -= 1;
		check_rounding = 1;
	}


	/* transfer data of both BCD source fields to their buffers using the bigger
	   scale of them and drop the sign nibble (handled separately);
	   note: if the scale isn't identical and the buffer can't be placed at the
	         same position we finally shift it by one half-byte */

	/* most common case: same scale first */
	if (fld1_scale == fld2_scale) {
		fld1 = fld1_buff + 48 - fld1_size;
		memcpy (fld1, src1_data, fld1_size);
		if (fld1_sign) {
			*(fld1_buff + 48 - 1) &= 0xF0;
		}
		fld2 = fld2_buff + 48 - fld2_size;
		memcpy (fld2, src2_data, fld2_size);
		if (fld2_sign) {
			*(fld2_buff + 48 - 1) &= 0xF0;
		}
	} else
	/* source has smaller scale than target */
	if (fld1_scale < fld2_scale) {
		const int	diff = fld2_scale - fld1_scale;
		const int	offset = diff >> 1;
		fld2 = fld2_buff + 48 - fld2_size;
		memcpy (fld2, src2_data, fld2_size);
		if (fld2_sign) {
			*(fld2_buff + 48 - 1) &= 0xF0;
		}
		fld1 = fld1_buff + 48 - fld1_size - offset;
		memcpy (fld1, src1_data, fld1_size);
		if (fld1_sign) {
			*(fld1_buff + 48 - 1 - offset) &= 0xF0;
		}
		if (diff % 2 == 1) {
			cob_shift_left_nibble (fld1_buff, fld1);
			fld1--;
		}
	} else
	/* if (fld1_scale > fld2_scale) */
	{
		const int	diff = fld1_scale - fld2_scale;
		const int	offset = diff >> 1;
		fld1 = fld1_buff + 48 - fld1_size;
		memcpy (fld1, src1_data, fld1_size);
		if (fld1_sign) {
			*(fld1_buff + 48 - 1) &= 0xF0;
		}
		fld2 = fld2_buff + 48 - fld2_size - offset;
		memcpy (fld2, src2_data, fld2_size);
		if (fld2_sign) {
			*(fld2_buff + 48 - 1 - offset) &= 0xF0;
		}
		if (diff % 2 == 1) {
			cob_shift_left_nibble (fld2_buff, fld2);
			fld2--;
		}
	}

	/* do actual computation after finding the length of the size-adjusted buffer
	   (after possible padding + shifting to align size + scale);
	   note that the result will be stored in fld2_buff */

	/* find the length of the longer buffer data */
	if (fld1 - fld1_buff > fld2 - fld2_buff) {
		loop_limit = 48 - (int)(fld2 - fld2_buff) + 1;
	} else {
		loop_limit = 48 - (int)(fld1 - fld1_buff) + 1;
	}

	/* note: both fl1 and fld2 point at the _last_ position of the
	   buffers as we are processing right to left */
	fld1 = fld1_buff + 48 - 1;
	fld2 = fld2_buff + 48 - 1;
	rslt = fld2;

	/* determine if the signs are different for the two fields, this allows
	   us to use two sepearate addition loops, keeping the sign check
	   outside of those */

	if (stmt == STMT_SUBTRACT) {
		/* reverse the sign of field 1 to handle SUBTRACT mode */
		if (fld1_sign == 0x0D) {
			fld1_sign = 0x0C;
		} else {
			fld1_sign = 0x0D;
		}
	}

	/* we have an internal addition if either both signs are identical
	   or none is negative (note: each of 0x0f 0x0c 0x00 are positive) */
	if (fld1_sign == fld2_sign
	 || (fld1_sign != 0x0D
	  && fld2_sign != 0x0D)) {
		int		check_next_byte = 0;
		if (fld1_sign == 0x0D) {
			final_positive = 0;
		}

		for (cntr = 0; cntr < loop_limit; cntr++, fld1--, fld2--, rslt--) {
			/* actual addition */
			byte = carry + h2b[*fld1] + h2b[*fld2];

			if (check_rounding) {
				if (check_next_byte) {
					/* if we already did the rounding with NEAREST-EVEN and
					   the rounding digit was even, then we may adjust the
					   next byte here, then are "really done" with rounding */
					if (byte % 2 == 1) {
						byte += 1;
					}
					check_rounding = 0;
				} else
				if (cntr == rounding_byte) {
					/* position for rounding */
					int byte_adj = byte;
					int rtn = handle_bcd_rounding (&byte_adj, rounding_digit % 2 == 1,
						all_zeros, final_positive, opt);
					if (rtn == -1) {
						/* rounding done and finished, no further checks needed */
						byte = byte_adj;
						check_rounding = 0;
						check_all_zeros = 0;
					} else
					if (rtn == -2) {
						/* rounding done, still need to handle "check next byte" */
						check_next_byte = 1;
						check_all_zeros = 0;
					} else {
						/* exception */
						return cobglobptr->cob_exception_code;
					}
				} else {
					/* no rounding yet */
				}
			}

			if (byte >= 200) {
				byte -= 200;
				carry = 2;
			} else
			if (byte >= 100) {
				byte -= 100;
				carry = 1;
			} else {
				carry = 0;
			}

			if (check_all_zeros
			 && byte != 0) {
				check_all_zeros = 0;
				all_zeros = 0;
			}
			*rslt = b2h[byte];
		}
	} else {
		/* subtraction - check which field's data has the highest absolute value
		   to know how to do that */
		const int  compare_result = memcmp (fld1_buff + 48 - loop_limit,
			fld2_buff + 48 - loop_limit,
			loop_limit);
		int		check_next_byte = 0;
		/* subtract smaller value from larger - directly possibly to do... */
		if (compare_result < 0) {
			if (fld1_sign != 0x0D) {
				final_positive = 0;
			}
			neg = fld1;
			pos = fld2;
		} else
		/* ...but if the negative data has a greater absolute value, then we
		   need to reverse the sign for addition (setting the final sign as
		   negative already) */
		if (compare_result > 0) {
			if (fld1_sign == 0x0D) {
				final_positive = 0;
			}
			neg = fld2;
			pos = fld1;
		} else
		/* special case (as we have that information already) - both data are
		   identical, only sign is the reverse -> the result has to be zero */
		/* if (compare_result == 0) */ {
			cob_set_packed_zero (fdst);
			return 0;
		}

		for (cntr = 0; cntr < loop_limit; cntr++, neg--, pos--, rslt--) {
			/* actual subtraction */
			byte = carry + h2b[*pos] - h2b[*neg];

			if (check_rounding) {
				if (check_next_byte) {
					/* if we already did the rounding with NEAREST-EVEN and
					   the rounding digit was even, then we may adjust the
					   next byte here, then are "really done" with rounding */
					if (byte % 2 == 1) {
						byte += 1;
					}
					check_rounding = 0;
				} else
				if (cntr == rounding_byte) {
					int byte_adj = byte;
					int rtn = handle_bcd_rounding (&byte_adj, rounding_digit % 2 == 1,
						all_zeros, final_positive, opt);
					if (rtn == -1) {
						/* rounding done and finished, no further checks needed */
						byte = byte_adj;
						check_rounding = 0;
						check_all_zeros = 0;
					} else
					if (rtn == -2) {
						/* rounding done, still need to handle "check next byte" */
						check_next_byte = 1;
						check_all_zeros = 0;
					} else {
						/* exception */
						return cobglobptr->cob_exception_code;
					}
				} else {
					/* no rounding yet */
				}
			}

			if (byte < 0) {
				byte += 100;
				carry = -1;
			} else if (byte >= 100) {
				byte -= 100;
				carry = 1;
			} else {
				carry = 0;
			}

			if (check_all_zeros && byte != 0 && byte != 100 && byte != 200) {
				check_all_zeros = 0;
				all_zeros = 0;
			}
			*rslt = b2h[byte];
		}
	}


	/* computation done, check for SIZE ERROR, then copy result to target */

	/* reset fld2 to point to start of the data for following memmove / checks */
	fld2 = fld2_buff + 48 - loop_limit;

	if (used_scale == dest_scale) {
		/* most common case: identical scale, no need to shift buffer */
	} else
	if (used_scale > dest_scale) {
		const int	diff = used_scale - dest_scale;
		const int	offset = diff / 2;
		memmove (fld2 + offset, fld2, loop_limit - offset);
		memset (fld2, 0, offset);
		fld2 += offset;
		if (diff & 1) {
			cob_shift_right_nibble (fld2_buff, fld2);
		}
	/* may happen if fdst != fsrc2: */
	} else
	/* if (used_scale < dest_scale) */ {
		const int	diff = dest_scale - used_scale;
		const int	offset = diff / 2;
		memmove (fld2 - offset, fld2, loop_limit);
		memset (fld2_buff + 48 - offset, 0, offset);
		if (diff % 2 == 1) {
			cob_shift_left_nibble (fld2_buff, fld2);
		}
	}
	
	if (check_overflow_and_set_sign (fdst, opt, final_positive, fld2_buff, fld2)) {
		return cobglobptr->cob_exception_code;
	}
	memcpy (COB_FIELD_DATA (fdst), fld2_buff + 48 - dest_size, dest_size);
	return 0;
}


#if	0	/* RXWRXW - Buggy */

/* Optimized arithmetic for DISPLAY */

static int
display_add_int (unsigned char *data, const size_t size, int n, const int opt)
{
	unsigned char	*sp;
	size_t		carry = 0;
	int		i;
	int		is;

	sp = data + size;
	while (n > 0) {
		i = n % 10;
		n /= 10;

		/* Check for overflow */
		if (--sp < data) {
			return opt;
		}

		/* Perform addition */
		is = (*sp & 0x0F) + i + carry;
		if (is > 9) {
			carry = 1;
			*sp = '0' + ((is + 6) & 0x0F);
		} else {
			carry = 0;
			*sp = '0' + is;
		}
	}
	if (carry == 0) {
		return 0;
	}

	/* Carry up */
	while (--sp >= data) {
		if ((*sp += 1) <= (unsigned char)'9') {
			return 0;
		}
		*sp = '0';
	}
	return opt;
}

static int
display_sub_int (unsigned char *data, const size_t size, int n, const int opt)
{
	unsigned char	*sp;
	size_t		carry = 0;
	int		i;

	COB_UNUSED (opt);

	sp = data + size;
	while (n > 0) {
		i = n % 10;
		n /= 10;

		/* Check for overflow */
		if (--sp < data) {
			return 1;
		}

#if	0	/* RXWRXW - Garbage check */
		/* Correct garbage */
		*sp = (unsigned char)('0' + (*sp & 0x0F));
#endif
		/* Perform subtraction */
		if ((*sp -= i + carry) < '0') {
			carry = 1;
			*sp += 10;
		} else {
			carry = 0;
		}
	}
	if (carry == 0) {
		return 0;
	}

	/* Carry up */
	while (--sp >= data) {
#if	0	/* RXWRXW - Garbage check */
		/* Correct garbage */
		*sp = (unsigned char)('0' + (*sp & 0x0F));
#endif
		if ((*sp -= 1) >= (unsigned char)'0') {
			return 0;
		}
		*sp = '9';
	}
	return 1;
}

static int
cob_display_add_int (cob_field *f, int n, const int opt)
{
	const unsigned char	*data = COB_FIELD_DATA (f);
	size_t		size = COB_FIELD_SIZE (f);
	const size_t		osize = size;
	size_t		i;
	int		scale = COB_FIELD_SCALE (f);
	int		sign;
	unsigned char	tfield[256];

	if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
		memcpy (tfield, data, size);
	}
	/* -x +v = -(x - v), -x -v = -(x + v) */
	if (sign == -1) {
		n = -n;
	}

	if (scale < 0) {
		/* PIC 9(n)P(m) -> adjust "val"
		   by applying the same scale (cut integer positions) */
		if (-scale < 10) {
			/* Fix optimizer bug */
			while (scale) {
				++scale;
				n /= 10;
			}
		} else {
			scale = 0;
			n = 0;
		}
		if (n == 0) {
			return 0;
		}
	} else {
		/* PIC 9(n)V9(m) */
		size -= scale;
		if (!size) {
			cob_set_exception (COB_EC_SIZE_OVERFLOW);
			if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
				return cobglobptr->cob_exception_code;
			}
			return 0;
		}
	}
	sign = COB_GET_SIGN_ADJUST (f);

	if (n > 0) {
		/* Add n to the field */
		if (display_add_int (data, size, n, opt) != 0) {
			/* Overflow */
			cob_set_exception (COB_EC_SIZE_OVERFLOW);
			/* If we need to restore */
			if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
				memcpy (data, tfield, osize);
				return cobglobptr->cob_exception_code;
			}
		}
	} else if (n < 0) {
		/* Subtract n from the field */
		if (display_sub_int (data, size, -n, opt) != 0) {
			for (i = 0; i < size; ++i) {
				data[i] = COB_I2D (9 - COB_D2I (data[i]));
			}
			if (scale) {
				for (i = size; i < size + scale; ++i) {
					if (COB_D2I (data[i]) > 0) {
						data[i] = COB_I2D (10 - COB_D2I (data[i]));
					}
				}
			} else {
				(void)display_add_int (data, size, 1, 0);
			}
			sign = -sign;
		}
	}

	COB_PUT_SIGN_ADJUSTED (f, sign);
	return 0;
}
#endif	/* Buggy */

int
cob_add_int (cob_field *f, const int n, const int opt)
{
	const int type = COB_FIELD_TYPE (f);
	if (n == 0) {
		return 0;
	}
#if	0	/* RXWRXW - Buggy */
	if (type == COB_TYPE_NUMERIC_PACKED) {
		return cob_add_packed (f, n, opt);
	}
	if (type == COB_TYPE_NUMERIC_DISPLAY) {
		return cob_display_add_int (f, n, opt);
	}
#endif
	/* n is single digit value added to positive field */
	if (
#if 0 /* CHECKME: this does not work for negative numbers */
	    n > -10
#else
	    n > 0
#endif
	 && n < 10
	 && COB_FIELD_SCALE (f) == 0) {
		register int	val, carry;
		register unsigned char *p = f->data + f->size - 1;
		if (COB_FIELD_TYPE (f) == COB_TYPE_NUMERIC_PACKED
		 && cob_packed_get_sign (f) >= 0
		 && *(f->data) == 0x00) {
			if (!COB_FIELD_NO_SIGN_NIBBLE (f)) {
				val = (*p >> 4) + n;
				if (val > 9) {
					carry = 1;
					*p = (*p & 0x0F) | ((val - 10) << 4);
				} else if (val < 0) {
					carry = -1;
					*p = (*p & 0x0F) | ((val + 10) << 4);
				} else {
					*p = (*p & 0x0F) | (val << 4);
					return 0;
				}
				p--;
			} else {
				carry = n;
			}
			while (carry != 0) {
				val = (*p & 0x0F) + carry;
				if (val > 9) {
					carry = 1;
					*p = (*p & 0xF0) | (val - 10);
				} else if (val < 0) {
					carry = -1;
					*p = (*p & 0xF0) | (val + 10);
				} else {
					*p = (*p & 0xF0) | val;
					return 0;
				}
				val = ((*p & 0xF0) >> 4) + carry;
				if (val > 9) {
					carry = 1;
					*p = (*p & 0x0F) | ((val - 10) << 4);
				} else if (val < 0) {
					carry = -1;
					*p = (*p & 0x0F) | ((val + 10) << 4);
				} else {
					*p = (*p & 0x0F) | (val << 4);
					return 0;
				}
				if (p == f->data) {
					if (carry == -1) {	/* Set field to -1 */
						memset (f->data, 0, f->size);
						f->data [f->size - 1] = 0x1D;
					}
					break;
				}
				p--;
			}
			return 0;
		}
		if (COB_FIELD_TYPE (f) == COB_TYPE_NUMERIC_DISPLAY
		 && (!COB_FIELD_HAVE_SIGN (f)
		 	|| ((*p & 0x40) != 0x40))
		 && *(f->data) == '0') {
			carry = n;
			while (carry != 0) {
				val = COB_D2I(*p) + carry;
				if (val > 9) {
					*p = COB_I2D (val - 10);
					carry = 1;
				} else if (val < 0) {
					*p = COB_I2D (val + 10);
					carry = -1;
				} else {
					*p = COB_I2D (val);
					return 0;
				}
				if (p == f->data) {
					if (carry == -1) {	/* Set field to -1 */
						memset (f->data, '0', f->size);
						f->data [f->size - 1] = 0x71;
					}
					break;
				}
				p--;
			}
			return 0;
		}
	}

	/* Not optimized */

	if (type >= COB_TYPE_NUMERIC_FLOAT
	 && type <= COB_TYPE_NUMERIC_FP_BIN128) {
		mpz_set_si (cob_d2.value, (cob_sli_t) n);
		cob_decimal_set_field (&cob_d1, f);
		cob_d2.scale = 0;
		cob_decimal_add (&cob_d1, &cob_d2);
		return cob_decimal_get_field (&cob_d1, f, opt);
	}
	{
		int	scale = COB_FIELD_SCALE (f);
		int	val = n;
		if (scale < 0) {
			/* PIC 9(n)P(m) -> adjust "val"
			   by applying the same scale (cut integer positions) */
			if (scale <= -10) {
				/* an int cannot have > 10 digit positions, so
				   no need to do anything if scale is too small */
				return 0;
			}
			while (scale++) {
				val /= 10;
			}
			if (!val) {
				return 0;
			}
			/* not used anymore, but logically: scale = 0 here */
		}
		cob_decimal_set_field (&cob_d1, f);
		mpz_set_si (cob_d2.value, (cob_sli_t)val);
		if (cob_d1.scale > 0) {
			cob_mul_by_pow_10 (cob_d2.value, cob_d1.scale);
#if 0	/* second scale is unused, these are just the "logic" values */
			cob_d2.scale = cob_d1.scale;
		} else {
			cob_d2.scale = 0;
#endif
		}
		mpz_add (cob_d1.value, cob_d1.value, cob_d2.value);
		return cob_decimal_get_field (&cob_d1, f, opt);
	}
}

int
cob_sub_int (cob_field *f, const int n, const int opt)
{
	return cob_add_int (f, -n, opt);
}

int
cob_cmp_int (cob_field *f1, const int n)
{
	int sign;
	cob_decimal_set_field (&cob_d1, f1);
	sign = mpz_sgn (cob_d1.value);
	if (sign == 0) {
		return -n;
	} else if (sign == 1) {
		if (n <= 0) return 1;
	} else {
		if (n >= 0) return -1;
	}
	mpz_set_si (cob_d2.value, n);
	if (cob_d1.scale < 0) {
		shift_decimal (&cob_d1, -cob_d1.scale);
	} else if (cob_d1.scale > 0) {
#if 0	/* if we ever add a "cob_equ_int"
		   then this is to be added  there */
		if (has_decimal_places (cob_d1)) {
			return 1;
		}
#endif
		shift_decimal (&cob_d2, cob_d1.scale);
	}
	return mpz_cmp (cob_d1.value, cob_d2.value);
}

int
cob_cmp_uint (cob_field *f1, const unsigned int n)
{
	int sign;	/* no const as we need the decimal set before */

	cob_decimal_set_field (&cob_d1, f1);
	sign = mpz_sgn (cob_d1.value);
	if (sign == 0) {
		if (n > INT_MAX) return INT_MIN;
		return -(int)n;
	} else if (sign == 1) {
		if (n <= 0) return 1;
	} else {
		return -1;
	}
	mpz_set_ui (cob_d2.value, n);
	if (cob_d1.scale < 0) {
		shift_decimal (&cob_d1, -cob_d1.scale);
	} else if (cob_d1.scale > 0) {
		shift_decimal (&cob_d2, cob_d1.scale);
	}
	return mpz_cmp (cob_d1.value, cob_d2.value);
}

int
cob_cmp_llint (cob_field *f1, const cob_s64_t n)
{
	int sign;	/* no const as we need the decimal set before */

	cob_decimal_set_field (&cob_d1, f1);
	sign = mpz_sgn (cob_d1.value);
	if (sign == 0) {
		if (n > INT_MAX) return INT_MIN;
		if (n < INT_MIN) return INT_MAX;
		return -(int)n;
	} else if (sign == 1) {
		if (n <= 0) return 1;
	} else {
		if (n >= 0) return -1;
	}
#ifdef	COB_LI_IS_LL
	mpz_set_si (cob_d2.value, (cob_sli_t)n);
#else
	{
		cob_u64_t	uval;
		cob_u32_t	negative;

		if (n < 0) {
			negative = 1;
			uval = (cob_u64_t)-n;
		} else {
			negative = 0;
			uval = (cob_u64_t)n;
		}
		mpz_set_ui (cob_d2.value, (cob_uli_t)(uval >> 32));
		mpz_mul_2exp (cob_d2.value, cob_d2.value, 32);
		mpz_add_ui (cob_d2.value, cob_d2.value, (cob_uli_t)(uval & 0xFFFFFFFFU));
		if (negative) {
			mpz_neg (cob_d2.value, cob_d2.value);
		}
	}
#endif
	if (cob_d1.scale < 0) {
		shift_decimal (&cob_d1, -cob_d1.scale);
	} else if (cob_d1.scale > 0) {
		shift_decimal (&cob_d2, cob_d1.scale);
	}
	return mpz_cmp (cob_d1.value, cob_d2.value);
}

#ifdef COB_FLOAT_DELTA
#define TOLERANCE (double) COB_FLOAT_DELTA
#else
/* note: request for comment via NEWS file for possible
   adjustment in GnuCOBOL 4, until then this value is
   fixed */
#define TOLERANCE (double) 0.0000001
#endif
#define FLOAT_EQ(x,y,t) (fabs(((x-y)/x)) < t)

int
cob_cmp_float (cob_field *f1, cob_field *f2)
{
	double	d1,d2;
	const int f1_type = COB_FIELD_TYPE (f1);
	const int f2_type = COB_FIELD_TYPE (f2);
	if (f1_type == COB_TYPE_NUMERIC_FLOAT) {
		float	fl;
		memcpy (&fl, f1->data, sizeof (float));
		d1 = fl;
	} else if (f1_type == COB_TYPE_NUMERIC_DOUBLE) {
		memcpy (&d1, f1->data, sizeof (double));
	} else if (f1_type == COB_TYPE_NUMERIC_L_DOUBLE) {
		long double ld;
		memcpy (&ld ,f1->data, sizeof (long double));
		d1 = (double) ld; /* TODO: real compare, likely with mpfr */
	} else {
		cob_decimal_set_field (&cob_d1, f1);
		d1 = cob_decimal_get_double (&cob_d1);
	}
	if (f2_type == COB_TYPE_NUMERIC_FLOAT) {
		float	fl;
		memcpy (&fl, f2->data, sizeof (float));
		d2 = fl;
	} else if (f2_type == COB_TYPE_NUMERIC_DOUBLE) {
		memcpy (&d2, f2->data, sizeof (double));
	} else if (f2_type == COB_TYPE_NUMERIC_L_DOUBLE) {
		long double ld;
		memcpy (&ld, f2->data, sizeof (long double));
		d2 = (double) ld; /* TODO: real compare, likely with mpfr */
	} else {
		cob_decimal_set_field (&cob_d1, f2);
		d2 = cob_decimal_get_double (&cob_d1);
	}
	if (d1 == d2) {
		return 0;
	}
	if (d1 != 0.0 /* check for zero to ensure no SIGFPE in the following macro */
	 && FLOAT_EQ (d1, d2, TOLERANCE)) {
		return 0;
	}
	if (d1 < d2) {
		return -1;
	}
	return 1;
}

/* check for non-negative sign, if it is set, then check for nonzero data */
static COB_INLINE COB_A_INLINE int
packed_is_negative (cob_field *f)
{
	if (cob_packed_get_sign (f) == -1) {
		/* negative sign, validate for nonzero data */
		unsigned char	nullbuff[(COB_MAX_DIGITS / 2) + 1] = { 0 };
		/* nonzero "really negative" if any data is nonzero */
		if (memcmp (f->data, nullbuff, f->size - 1)) {
			return 1;
		}
		/* nonzero if byte with sign nibble has other data */
		if ((*(f->data + f->size - 1) != 0x0D)) {
			return 1;	/* extra data -> really negative */
		}
		/* all zero -> not negative, even with the sign telling so */
		return 0;
	}
	return 0;
}

#ifndef NO_BCD_COMPARE
static COB_INLINE COB_A_INLINE int
insert_packed_aligned (
	const cob_field *f1, const int no_sign_nibble_f1, const int scale1,
	const cob_field *f2, const int no_sign_nibble_f2, const int scale2,
	unsigned char *ptr_byte, unsigned char *ptr_byte2, const int buff_size)
{
	register unsigned char *ptr_byte1 = ptr_byte;

	const int len1 = (int)f1->size;
	const int len2 = (int)f2->size;

	int		compare_len, nibble_cntr, byte_cntr;

	/* calculate amount to shift left */
	nibble_cntr = scale2 - scale1;
	if (no_sign_nibble_f1 && !no_sign_nibble_f2) {
		nibble_cntr++;
	}

	/* insert data into initialized buffer at the end */
	byte_cntr = nibble_cntr >> 1;	/* nibbles dived by 2 = bytes */
	nibble_cntr &= 0x00000001;		/* modulo divide nibble by 2 */
	ptr_byte1 += buff_size - (len1 + byte_cntr);
	memcpy (ptr_byte1, COB_FIELD_DATA (f1), len1);
	if (!no_sign_nibble_f1) {
		*(ptr_byte1 + len1 - 1) &= 0xF0;	/* clear sign nibble */
	}

	if (nibble_cntr != 0) {

		/* shift the complete filled buffer one nibble left */
#ifdef ALTERNATIVE_PACKED_SWAP	/* should work portably, but is around 20% slower */
		register unsigned char *last_pos = ptr_byte1 + len1;
		*(ptr_byte1 - 1) = *ptr_byte1 >> 4;
		while (ptr_byte1 != last_pos) {
			*ptr_byte1 = (*ptr_byte1 << 4) | (*(ptr_byte1 + 1) >> 4);
			ptr_byte1++;
		}
#else

#	ifndef	WORDS_BIGENDIAN
		cob_u64_t chunk;
#	endif
		register cob_u64_t *ptr_long;
		unsigned char	carry_nibble, move_nibble;
		int shift_cntr;

		shift_cntr = len1 + 1; /* add one to ensure the carry nibble is moved */
		move_nibble = 0xFF;
		ptr_long = (cob_u64_t*)(ptr_byte1 + len1 - 8);
		do {
#	ifdef	WORDS_BIGENDIAN
			/* shift and include old nibble */
			carry_nibble = (unsigned char)(*ptr_long >> 60);
			*ptr_long = (*ptr_long << 4);
			if (shift_cntr < len1) {
				*ptr_long |= move_nibble;
			}
#	else
			/* load data to chunk, swap as necessary */
			chunk = COB_BSWAP_64 ((cob_u64_t)(*ptr_long));
			/* shift and include old nibble */
			carry_nibble = (unsigned char)(chunk >> 60);
			chunk = (chunk << 4);
			if (shift_cntr < len1) {
				chunk |= move_nibble;
			}
			/* swap as necessary, place in memory */
			*ptr_long = COB_BSWAP_64 ((cob_u64_t)(chunk));
#	endif
			/* prepare for next round */
			move_nibble = carry_nibble;
			shift_cntr -= 8;
			ptr_long--;
		} while (shift_cntr > 0);

#endif
		compare_len = len1 + byte_cntr + nibble_cntr;
	} else {
		compare_len = len1 + byte_cntr;
	}

	/* insert data2 into initialized buffer at the end */
	ptr_byte2 += buff_size - len2;
	memcpy (ptr_byte2, COB_FIELD_DATA (f2), len2);
	if (!no_sign_nibble_f2) {
		*(ptr_byte2 + len2 - 1) &= 0xF0;	/* clear sign nibble */
	}

	/* return length for compare */
	if (len2 > compare_len) {
		return len2;
	}
	return compare_len;
}

static COB_INLINE COB_A_INLINE int
decimal_convert_scale (
	const cob_field *f1, const int no_sign_nibble_f1, const int scale1,
	const cob_field *f2, const int no_sign_nibble_f2, const int scale2,
	const int both_are_negative)
{
	unsigned char	buff1[48] = {0}, buff2[48] = {0};
	unsigned char	*ptr_byte1, *ptr_byte2;

	int		compare_len;

	/* Note: we explicit do not drop the leftmost niobble for even digits (COMP-3) /
	   odd digits (COMP-6) - as at least MF compares those, too,
	   IBM presumably does the same */

	/* TODO: handle negative scale 99PPPP - and also take care for .PPPP9 */

	/* left or right buffer to shift? */
	if ((scale1 < scale2)
	 || (scale1 == scale2
	  && no_sign_nibble_f1)) {

		compare_len = insert_packed_aligned (
			f1, no_sign_nibble_f1, scale1,
			f2, no_sign_nibble_f2, scale2,
			buff1, buff2, 48
		);

	} else {
		compare_len = insert_packed_aligned (
			f2, no_sign_nibble_f2, scale2,
			f1, no_sign_nibble_f1, scale1,
			buff2, buff1, 48
		);

	}

	ptr_byte1 = buff1 + 48 - compare_len;
	ptr_byte2 = buff2 + 48 - compare_len;

	if (both_are_negative) {
		return memcmp (ptr_byte2, ptr_byte1, compare_len);
	} else {
		return memcmp (ptr_byte1, ptr_byte2, compare_len);
	}
}

int
cob_bcd_cmp (cob_field *f1, cob_field *f2)
{
	const int	f1_is_negative = packed_is_negative (f1);
	const int	f2_is_negative = packed_is_negative (f2);

	if (f1_is_negative && !f2_is_negative) {
		return -1;
	}
	if (!f1_is_negative && f2_is_negative) {
		return 1;
	}

	{
		const int	no_sign_nibble_f1 = COB_FIELD_NO_SIGN_NIBBLE (f1);
		const int	no_sign_nibble_f2 = COB_FIELD_NO_SIGN_NIBBLE (f2);

		const int	scale1 = COB_FIELD_SCALE (f1);
		const int	scale2 = COB_FIELD_SCALE (f2);

		if (f1->size == f2->size	/* note: we explicit ignore different digits here */
		 && no_sign_nibble_f1 == no_sign_nibble_f2
		 && scale1 == scale2) {
			/* Note: we explicit do not drop the higher bit for even digits (COMP-3) /
			   odd digits (COMP-6) - as at least MF compares those, too (but sometimes not?) */
			const unsigned char	*data1 = COB_FIELD_DATA (f1);
			const unsigned char	*data2 = COB_FIELD_DATA (f2);
			if (no_sign_nibble_f1) {
				/* in this case both have no sign, directly compare the positive values */
				const size_t	len = f1->size;
				return memcmp (data1, data2, len);
			} else {
				/* in this case both have a _possible_ sign, and are either both positive or negative */
				const size_t	len = f1->size - 1;
				int ret;
				/* compare data from left to right - all but half that includes sign nibble */
				if ((ret = memcmp (data1, data2, len)) == 0) {
					/* so far identical - compare upper half byte with sign nibble last */
					ret = (data1[len] & 0xF0) - (data2[len] & 0xF0);
				}
				/* swap compare result for negative values */
				if (f1_is_negative) {
					return -ret;
				} else {
					return ret;
				}
			}
		}

		return decimal_convert_scale (
			f1, no_sign_nibble_f1, scale1,
			f2, no_sign_nibble_f2, scale2,
			f1_is_negative	/* in this case both are non-negative */
		);
	}
}
# else

int
cob_bcd_cmp (cob_field *f1, cob_field *f2)
{
	/* Fallback: internal decimal compare (most expensive) */
	cob_decimal_set_field (&cob_d1, f1);
	cob_decimal_set_field (&cob_d2, f2);
	return cob_decimal_cmp (&cob_d1, &cob_d2);
}
#endif

#ifndef NO_DISPLAY_COMPARE
static COB_INLINE COB_A_INLINE int
compare_data (unsigned char *ptr1, unsigned char *ptr2, int len)
{
	unsigned char *ptr_end = ptr1 + len;

	do {
		const unsigned char p1 = COB_D2I (*ptr1);
		const unsigned char p2 = COB_D2I (*ptr2);
		if (p1 != p2) {
			if (p1 > p2) {
				return 1;
			}
			return -1;
		}
		ptr1++;
		ptr2++;
	} while (ptr1 != ptr_end);

	return 0;
}

static int
check_all_zero (unsigned char *ptr, int len)
{
	unsigned char *ptr_end = ptr + len;

	do {
		if (COB_D2I (*ptr++) != 0) {
			return 0;
		}
	} while (ptr != ptr_end);

	return 1;
}

int
cob_numeric_display_cmp (cob_field *f1, cob_field *f2)
{
	int		compare_result, len;
	int		f1_strt_offset, f1_end_offset;
	int		f2_strt_offset, f2_end_offset;

	/************************************************************/
	/*  first we need to get the scale, data size and sign of   */
	/*  each of the two cob fields. Note that if the signs are  */
	/*  different, then we only need to find if either of the   */
	/*  fields is non-zero.                                     */
	/************************************************************/

	const short 	scale1 = COB_FIELD_SCALE (f1);
	const short 	scale2 = COB_FIELD_SCALE (f2);
	/* numeric DISPLAY fields from a program are limited to COB_MAX_DIGITS,
	   internal created numeric DISPLAY fields from intrinsic functions
	   in intrinsic.c (cob_alloc_field, cob_decimal_move_temp) could
	   _theoretically_ use up to SHRT_MAX */
	const short 	size1 = (short)COB_FIELD_SIZE (f1);
	const short 	size2 = (short)COB_FIELD_SIZE (f2);
	unsigned char	*ptr_f1 = COB_FIELD_DATA (f1);
	unsigned char	*ptr_f2 = COB_FIELD_DATA (f2);

	const int	orig_sign1 = COB_GET_SIGN_ADJUST (f1);
	const int	orig_sign2 = COB_GET_SIGN_ADJUST (f2);

	int ret;

	const int	sign1 = orig_sign1 >= 0 ? 1 :
		check_all_zero (ptr_f1, size1) ? 1 : - 1;
	const int	sign2 = orig_sign2 >= 0 ? 1 :
		check_all_zero (ptr_f2, size2) ? 1 : -1;

	if (sign1 != sign2) {
		ret = sign1;
		goto over;
	}
	/************************************************************/
	/*  first we need to find the start and end of each field   */
	/*  relative to the decimal point so that we can compare    */
	/*  them correctly based on the position relative to the    */
	/*  decimal point.                                          */
	/*                                                          */
	/*  the comparison of two fields containing display         */
	/*  numeric data will result in one of three conditions     */
	/*                                                          */
	/*  one field will be a subset of the other.                */
	/*  one field would intersect with the other.               */
	/*  the fields would not intersect at all.                  */
	/*                                                          */
	/*  so these are the 3 conditions we need to check for.     */
	/*  this will result in 3 compares being needed.            */
	/************************************************************/

	f1_strt_offset = size1 - scale1;
	f1_end_offset = 0 - scale1;

	f2_strt_offset = size2 - scale2;
	f2_end_offset = 0 - scale2;

	/************************************************************/
	/*  the first compare will be to check the leading / pad    */
	/*  positions in either of the fields to check if they are  */
	/*  all zeros. if not then we don't need to compare any     */
	/*  farther, we are done.                                   */
	/************************************************************/


	if (f1_strt_offset > f2_strt_offset) {
		if (f2_strt_offset > f1_end_offset) {
			if (!check_all_zero (ptr_f1, (f1_strt_offset - f2_strt_offset))) {
				ret = sign1;
				goto over;
			}
			ptr_f1 += (f1_strt_offset - f2_strt_offset);
			f1_strt_offset -= (f1_strt_offset - f2_strt_offset);
		} else {
			if (!check_all_zero (ptr_f1, (f1_strt_offset - f1_end_offset))) {
				ret = sign1;
				goto over;
			}
			ptr_f1 += (f1_strt_offset - f1_end_offset);
			f1_strt_offset -= (f1_strt_offset - f1_end_offset);
		}
	} else if (f2_strt_offset > f1_strt_offset) {
		if (f1_strt_offset > f2_end_offset) {
			if (!check_all_zero (ptr_f2, (f2_strt_offset - f1_strt_offset))) {
				ret = -sign1;
				goto over;
			}
			ptr_f2 += (f2_strt_offset - f1_strt_offset);
			f2_strt_offset -= (f2_strt_offset - f1_strt_offset);
		} else {
			if (!check_all_zero (ptr_f2, (f2_strt_offset - f2_end_offset))) {
				ret = -sign1;
				goto over;
			}
			ptr_f2 += (f2_strt_offset - f2_end_offset);
			f2_strt_offset -= (f2_strt_offset - f2_end_offset);
		}
	}

	/************************************************************/
	/*  the second would be to check the overlapping positions  */
	/*  in both fields against each other. if they do not       */
	/*  match then we can set the return code to indicate       */
	/*  which is greater / lesser and we are done.              */
	/************************************************************/

	if (f1_strt_offset == f2_strt_offset) {
		len = (f1_end_offset > f2_end_offset) ? (f1_strt_offset - f1_end_offset)
			: (f2_strt_offset - f2_end_offset);
		if (len > 0) {
			compare_result = compare_data (ptr_f1, ptr_f2, len);
			if (compare_result) {
				ret = sign1 * compare_result;
				goto over;
			}
			ptr_f1 += len;
			f1_strt_offset -= len;
			ptr_f2 += len;
			f2_strt_offset -= len;
		}
	}

	/************************************************************/
	/*  if we have not exited yet that means that one of the    */
	/*  fields have trailing positions to the right of the      */
	/*  overlapping positions, so we have to check those for    */
	/*  all zeros. then we are done.                            */
	/************************************************************/

	if (f1_end_offset < f2_end_offset) {
		if (!check_all_zero (ptr_f1, (f1_strt_offset - f1_end_offset))) {
			ret = sign1;
			goto over;
		}
	} else if (f2_end_offset < f1_end_offset) {
		if (!check_all_zero (ptr_f2, (f2_strt_offset - f2_end_offset))) {
			ret = -sign1;
			goto over;
		}
	}
	ret = 0;

over:
	COB_PUT_SIGN_ADJUSTED (f1, orig_sign1);
	COB_PUT_SIGN_ADJUSTED (f2, orig_sign2);
	return ret;
}
# else

int
cob_numeric_display_cmp (cob_field *f1, cob_field *f2)
{
	/* Fallback: internal decimal compare (most expensive) */
	cob_decimal_set_field (&cob_d1, f1);
	cob_decimal_set_field (&cob_d2, f2);
	return cob_decimal_cmp (&cob_d1, &cob_d2);
}
#endif

/* compare USAGE DISPLAY numeric variable with no SIGN SEPARATE
   and no invalid ebcdic overpunch to zero */
int
cob_numeric_display_cmp_zero (cob_field *f)
{
	unsigned char *ptr = f->data;
	unsigned char *ptr_end = ptr + f->size;

	do {
		const unsigned char p = COB_D2I (*ptr);
		if (p != 0) {
			const char p_sign = *(ptr_end - 1) & 0xF0;
			int sign = p_sign == 0x70 || p_sign == 0xD0 || p_sign == 0xB0
			         ? -1 : 1;
			return sign;
		}
		ptr++;
	} while (ptr < ptr_end);

	return 0;
}

/* compare BCD field to zero */
int
cob_bcd_cmp_zero (cob_field *f)
{
	unsigned char	nullbuff[(COB_MAX_DIGITS / 2) + 1] = { 0 };

	if (COB_FIELD_NO_SIGN_NIBBLE(f)) {
		if (memcmp (f->data, nullbuff, f->size)) {
			return 1;
		}
	} else {
		if (memcmp (f->data, nullbuff, f->size - 1)) {
			return cob_packed_get_sign (f) == -1 ? -1 : 1;
		}
		/* nonzero if byte with sign nibble has content in data nibble */
		if ((*(f->data + f->size - 1) & 0xF0) != 0) {
			return cob_packed_get_sign (f) == -1 ? -1 : 1;
		}
	}
	return 0;
}

int
cob_numeric_cmp (cob_field *f1, cob_field *f2)
{
	const int f1_type = COB_FIELD_TYPE (f1);
	const int f2_type = COB_FIELD_TYPE (f2);

#ifndef NO_BCD_COMPARE
	/* do bcd compare if possible */
	if (f1_type == COB_TYPE_NUMERIC_PACKED
	 && f2_type == COB_TYPE_NUMERIC_PACKED) {
		/* for now skip negative scale, until this is added and tested */
		if (COB_FIELD_SCALE (f1) >= 0 && COB_FIELD_SCALE (f2) >= 0) {
			return cob_bcd_cmp (f1, f2);
		}
		/* CHECKME: possible create temporary bcd2 if only one is packed
		   and the other isn't float - then compare as BCD */
	}
#endif

#ifndef NO_DISPLAY_COMPARE
	/* do numeric display compare if possible */
	if (f1_type == COB_TYPE_NUMERIC_DISPLAY
	 && f2_type == COB_TYPE_NUMERIC_DISPLAY) {
		/* CHECKME: if not float then create a temporary display2 */
		return cob_numeric_display_cmp (f1, f2);
	}
#endif

	/* float needs special comparison */
	if ( (f1_type >= COB_TYPE_NUMERIC_FLOAT
	   && f1_type <= COB_TYPE_NUMERIC_L_DOUBLE)
	 ||  (f2_type >= COB_TYPE_NUMERIC_FLOAT
	   && f2_type <= COB_TYPE_NUMERIC_L_DOUBLE)) {
		return cob_cmp_float (f1, f2);
	}

	/* otherwise - preferably compare as integers */
	if (COB_FIELD_SCALE (f1) == COB_FIELD_SCALE (f2)
	 && COB_FIELD_DIGITS (f1) < 19
	 && COB_FIELD_DIGITS (f2) < 19) {
		if (COB_FIELD_SCALE (f1) == 0) {
			/* no scale, so get the data out directly */
			const cob_s64_t	f1_num = cob_get_llint (f1);
			const cob_s64_t	f2_num = cob_get_llint (f2);
			return (f1_num < f2_num) ? -1 : (f1_num > f2_num);
		} else {
			/* handle those fields as if they would have no scale */
			const cob_field_attr *a1 = f1->attr, *a2 = f2->attr;
			cob_field_attr ca1, ca2;
			cob_field c1, c2;
			COB_ATTR_INIT_A (ca1, a1->type, a1->digits, 0, a1->flags, a1->pic);
			COB_ATTR_INIT_A (ca2, a2->type, a2->digits, 0, a2->flags, a2->pic);
			COB_FIELD_INIT_F (c1, f1->size, f1->data, &ca1);
			COB_FIELD_INIT_F (c2, f2->size, f2->data, &ca2);
			{
				const cob_s64_t	f1_num = cob_get_llint (&c1);
				const cob_s64_t	f2_num = cob_get_llint (&c2);
				return (f1_num < f2_num) ? -1 : (f1_num > f2_num);
			}
		}
	}

	/* Fallback: internal decimal compare (most expensive) */
	cob_decimal_set_field (&cob_d1, f1);
	cob_decimal_set_field (&cob_d2, f2);
	return cob_decimal_cmp (&cob_d1, &cob_d2);
}

static int
cmp_packed_intern (cob_field *f, cob_u64_t n, const int both_are_negative)
{
	unsigned char		val1[MAX_LLI_DIGITS_PLUS_1];
	size_t	first_pos;
	unsigned char		*p;
	register size_t		size = f->size;

	/* 1) re-pack field-data to 20 bytes -> val1 */
	p = f->data;
	first_pos = sizeof(val1) - size;
	memset (val1, 0, first_pos);
	memcpy (val1 + first_pos, p, size);

#if 0  /* Note: we explicit do not drop the higher bit for even digits (COMP-3) /
          odd digits (COMP-6) - as at least MF compares those, too */
	if (COB_FIELD_NO_SIGN_NIBBLE (f)) {
		if ((COB_FIELD_DIGITS(f) % 2) == 1) {
			val1[first_pos] &= 0x0F;
		}
	} else {
		if ((COB_FIELD_DIGITS(f) % 2) == 0) {
			val1[first_pos] &= 0x0F;
		}
	}
#endif
	/* drop sign bit - we only compare both positive/negative here */
	if (!COB_FIELD_NO_SIGN_NIBBLE (f)) {
		val1[19] &= 0xF0;
	}

	/* 2) pack "n" to 20 bytes -> packed_value */
	if (n != last_packed_val) {
		/* otherwise we just leave the already packed value as-is */
		last_packed_val = n;
		memset (packed_value, 0, sizeof(packed_value));
		if (n) {
			p = &packed_value[19];
			if (!COB_FIELD_NO_SIGN_NIBBLE (f)) {
				*p = (n % 10) << 4;
				p--;
				n /= 10;
			}
			for (; n;) {
				size = n % 100;
				*p = (unsigned char)((size % 10) | ((size / 10) << 4));
				n /= 100;
				p--;
			}
		}
	}
	/* 3) byte-wise compare of val1 + packed_value */
	{
		register int		ret;
		for (size = 0; size < sizeof(val1); size++) {
			if ((ret = val1[size] - packed_value[size]) != 0) {
				if (both_are_negative) {
					return -ret;
				} else {
					return ret;
				}
			}
		}
	}
	return 0;
}

int
cob_cmp_packed (cob_field *f, const cob_s64_t val)
{
	if (COB_FIELD_DIGITS (f) >= 19) {
		const int	is_negative = packed_is_negative (f);

		/* Field negative, value positive */
		if (is_negative && val >= 0) {
			return -1;
		}
		/* Field positive, value negative */
		if (!is_negative && val < 0) {
			return 1;
		}

		/* Swap if both are negative */
		if (val < 0) {
			return cmp_packed_intern (f, (cob_u64_t)-val, 1);
		} else {
			return cmp_packed_intern (f, (cob_u64_t)val, 0);
		}
	} else {
		const cob_s64_t	n = cob_get_llint (f);
		return (n < val) ? -1 : (n > val);
	}
}

/* Numeric Display compares */

#ifdef	COB_EBCDIC_MACHINE
static unsigned int
cob_get_long_ascii_sign (const unsigned char *p, int *val)
{
	switch (*p) {
	case 'p':
		return 1;
	case 'q':
		*val = 1;
		return 1;
	case 'r':
		*val = 2;
		return 1;
	case 's':
		*val = 3;
		return 1;
	case 't':
		*val = 4;
		return 1;
	case 'u':
		*val = 5;
		return 1;
	case 'v':
		*val = 6;
		return 1;
	case 'w':
		*val = 7;
		return 1;
	case 'x':
		*val = 8;
		return 1;
	case 'y':
		*val = 9;
		return 1;
	}
	return 0;
}
#endif

static unsigned int
cob_get_long_ebcdic_sign (const unsigned char *p, int *val)
{
	switch (*p) {
	case '{':
		return 0;
	case 'A':
		*val = 1;
		return 0;
	case 'B':
		*val = 2;
		return 0;
	case 'C':
		*val = 3;
		return 0;
	case 'D':
		*val = 4;
		return 0;
	case 'E':
		*val = 5;
		return 0;
	case 'F':
		*val = 6;
		return 0;
	case 'G':
		*val = 7;
		return 0;
	case 'H':
		*val = 8;
		return 0;
	case 'I':
		*val = 9;
		return 0;
	case '}':
		return 1;
	case 'J':
		*val = 1;
		return 1;
	case 'K':
		*val = 2;
		return 1;
	case 'L':
		*val = 3;
		return 1;
	case 'M':
		*val = 4;
		return 1;
	case 'N':
		*val = 5;
		return 1;
	case 'O':
		*val = 6;
		return 1;
	case 'P':
		*val = 7;
		return 1;
	case 'Q':
		*val = 8;
		return 1;
	case 'R':
		*val = 9;
		return 1;
	}
	return 0;
}

int
cob_cmp_numdisp (const unsigned char *data, const size_t size,
		 const cob_s64_t n, const cob_u32_t has_sign)
{
	register const unsigned char	*p = data;
	const unsigned char	*p_end;
	register cob_s64_t		val = 0;

	if (!has_sign) {
		if (n < 0) {
			return 1;
		}
		p_end = p + size;
		while (p != p_end) {
			val = val * 10 + COB_D2I (*p++);
		}
		return (val < n) ? -1 : (val > n);
	}

	p_end = p + size - 1;
	while (p < p_end) {
		val = val * 10 + COB_D2I (*p++);
	}
	val *= 10;
	if (*p >= (unsigned char)'0' && *p <= (unsigned char)'9') {
		val += COB_D2I (*p);
	} else {
		if (COB_MODULE_PTR->ebcdic_sign) {
			int sign_val = 0;
			int sign = cob_get_long_ebcdic_sign (p, &sign_val);
			val += sign_val;
			if (sign) {
				val = -val;
			}
		} else {
#ifdef	COB_EBCDIC_MACHINE
			int sign_val = 0;
			int sign = cob_get_long_ascii_sign (p, &sign_val);
			val += sign_val;
			if (sign) {
				val = -val;
			}
#else
			if (*p >= (unsigned char)'p' && *p <= (unsigned char)'y') {
				val += (*p - (unsigned char)'p');
				val = -val;
			}
#endif
		}
	}
	return (val < n) ? -1 : (val > n);
}

/* "allocation of cob_decimals (internal structure pointing
   to initialized GMP storage) - just getting the pointer to
   one of the pre-allocated ones */
void
cob_decimal_alloc (const cob_u32_t params, ...)
{
	cob_decimal	**dec;
	cob_u32_t	i;
	va_list		args;

	va_start (args, params);
	for (i = 0; i < params; ++i) {
		dec = va_arg (args, cob_decimal **);
		*dec = cob_decimal_base + i;
	}
	va_end (args);
}

/* real allocation of (temporary) cob_decimals, which is relative slow
   because of necessary GMP initialization storage;
   caller must release the decimals with a later call to cob_decial_pop */
void
cob_decimal_push (const cob_u32_t params, ...)
{
	cob_decimal	**dec;
	cob_u32_t	i;
	va_list		args;

	va_start (args, params);
	for (i = 0; i < params; ++i) {
		dec = va_arg (args, cob_decimal **);
		*dec = cob_malloc (sizeof(cob_decimal));
		cob_decimal_init (*dec);
	}
	va_end (args);
}

/* release temporary decimals, allocated with cob_decimal_push */
void
cob_decimal_pop (const cob_u32_t params, ...)
{
	cob_decimal	*dec;
	cob_u32_t	i;
	va_list		args;

	va_start (args, params);
	for (i = 0; i < params; ++i) {
		dec = va_arg (args, cob_decimal *);
		mpz_clear (dec->value);
		cob_free (dec);
	}
	va_end (args);
}

/* Helper routines (pow functions for integers to not stumble over truncation from double)

  (int) pow ((double)10, (double)8) may be 9999999, not 10^8.
  This also applies to other powers. See http://stackoverflow.com/q/9704195.

  while using signed types we actually _expect_ only positive exponents
*/

#define POW_IMPL(type)	                           \
{                                                  \
	register type	ret;                           \
	if (power == 0 || base == 1 || base == -1) {   \
		return 1;                                  \
	}                                              \
	if (power < 0) {                               \
		/* division by zero */                     \
		if (base == 0) {                           \
			cob_raise (SIGFPE);                    \
		}                                          \
		/* too small -> (int)0 */                  \
		return 0;                                  \
	}                                              \
	ret = 1;                                       \
	while (power > 0) {                            \
		ret *= base;                               \
		--power;                                   \
	}                                              \
	return ret;                                    \
}

cob_s32_t
cob_s32_pow (cob_s32_t base, cob_s32_t power)
{
	POW_IMPL(cob_s32_t)
}
cob_s64_t
cob_s64_pow (cob_s64_t base, cob_s64_t power)
{
	POW_IMPL (cob_s64_t)
}
#undef POW_IMPL


/* Init/Exit routines */

void
cob_exit_numeric (void)
{
	cob_decimal	*d1;
	size_t		i;

	if (cob_decimal_base) {
		d1 = cob_decimal_base;
		for (i = 0; i < COB_MAX_DEC_STRUCT; d1++, i++) {
			mpz_clear (d1->value);
		}
		cob_free (cob_decimal_base);
	}

	mpz_clear (cob_d_remainder.value);

	mpz_clear (cob_d3.value);
	mpz_clear (cob_d2.value);
	mpz_clear (cob_d1.value);
	mpz_clear (cob_t2.value);
	mpz_clear (cob_t1.value);

	mpz_clear (cob_mexp);
	mpz_clear (cob_mpzt2);
	mpz_clear (cob_mpzt);

	mpz_clear (cob_mpz_ten34m1);
	mpz_clear (cob_mpz_ten16m1);
	for (i = 0; i <= COB_MAX_BINARY; i++) {
		mpz_clear (cob_mpze10[i]);
	}

	mpf_clear (cob_mpft_get);
	mpf_clear (cob_mpft);
}

void
cob_init_numeric (cob_global *lptr)
{
	cob_decimal	*d1;
	cob_u32_t	i;

	cobglobptr = lptr;

	memset (&packed_value, 0, sizeof(packed_value));
	memset (&i64_spaced_out, ' ' , sizeof (i64_spaced_out));
	last_packed_val = 0;

	mpf_init2 (cob_mpft, COB_MPF_PREC);
	mpf_init2 (cob_mpft_get, COB_MPF_PREC);

	for (i = 0; i <= COB_MAX_BINARY; i++) {
		mpz_init2 (cob_mpze10[i], 128UL);
		mpz_ui_pow_ui (cob_mpze10[i], 10UL, (cob_uli_t)i);
	}
	mpz_init_set (cob_mpz_ten16m1, cob_mpze10[16]);
	mpz_sub_ui (cob_mpz_ten16m1, cob_mpz_ten16m1, 1UL);
	mpz_init_set (cob_mpz_ten34m1, cob_mpze10[34]);
	mpz_sub_ui (cob_mpz_ten34m1, cob_mpz_ten34m1, 1UL);

	mpz_init2 (cob_mpzt, COB_MPZ_DEF);
	mpz_init2 (cob_mpzt2, COB_MPZ_DEF);
	mpz_init2 (cob_mexp, COB_MPZ_DEF);

	cob_decimal_init (&cob_d1);
	cob_decimal_init (&cob_d2);
	cob_decimal_init (&cob_d3);
	cob_decimal_init (&cob_d_remainder);
	cob_decimal_init (&cob_t1);
	cob_decimal_init (&cob_t2);

	cob_decimal_base = cob_malloc (COB_MAX_DEC_STRUCT * sizeof(cob_decimal));
	d1 = cob_decimal_base;
	for (i = 0; i < COB_MAX_DEC_STRUCT; d1++, i++) {
		cob_decimal_init (d1);
	}
}

/* BIT-WISE functions */
void
cob_logical_not (cob_decimal *d0, cob_decimal *d1)
{
	const cob_u64_t	u1 = mpz_get_ui (d1->value);
	const cob_u64_t	ur = ~ u1;
	cob_decimal_set_ullint (d0, ur);
}

void
cob_logical_or (cob_decimal *d0, cob_decimal *d1)
{
	const cob_u64_t	u0 = mpz_get_ui (d0->value);
	const cob_u64_t	u1 = mpz_get_ui (d1->value);
	const cob_u64_t	ur = u0 | u1;
	cob_decimal_set_ullint (d0, ur);
}

void
cob_logical_and (cob_decimal *d0, cob_decimal *d1)
{
	const cob_u64_t	u0 = mpz_get_ui (d0->value);
	const cob_u64_t	u1 = mpz_get_ui (d1->value);
	const cob_u64_t	ur = u0 & u1;
	cob_decimal_set_ullint (d0, ur);
}

void
cob_logical_xor (cob_decimal *d0, cob_decimal *d1)
{
	const cob_u64_t	u0 = mpz_get_ui (d0->value);
	const cob_u64_t	u1 = mpz_get_ui (d1->value);
	const cob_u64_t	ur = u0 ^ u1;
	cob_decimal_set_ullint (d0, ur);
}

void
cob_logical_left (cob_decimal *d0, cob_decimal *d1)
{
	const cob_u64_t	u0 = mpz_get_ui (d0->value);
	const cob_u64_t	u1 = mpz_get_ui (d1->value);
	const cob_u64_t	ur = u0 << u1;
	cob_decimal_set_ullint (d0, ur);
}

void
cob_logical_right (cob_decimal *d0, cob_decimal *d1)
{
	const cob_u64_t	u0 = mpz_get_ui (d0->value);
	const cob_u64_t	u1 = mpz_get_ui (d1->value);
	const cob_u64_t	ur = u0 >> u1;
	cob_decimal_set_ullint (d0, ur);
}

void			/* Circulare LEFT shift */
cob_logical_left_c (cob_decimal *d0, cob_decimal *d1, int bytes)
{
	const cob_u64_t	u0 = mpz_get_ui (d0->value);
	const cob_u64_t	u1 = mpz_get_ui (d1->value);
	const cob_u64_t	ur = (u0 << u1) | (u0 >> ((cob_u64_t)bytes * 8 - u1));
	cob_decimal_set_ullint (d0, ur);
}

void			/* Circulare RIGHT shift */
cob_logical_right_c (cob_decimal *d0, cob_decimal *d1, int bytes)
{
	const cob_u64_t	u0 = mpz_get_ui (d0->value);
	const cob_u64_t	u1 = mpz_get_ui (d1->value);
	const cob_u64_t	ur = (u0 >> u1) | (u0 << ((cob_u64_t)bytes * 8 - u1));
	cob_decimal_set_ullint (d0, ur);
}

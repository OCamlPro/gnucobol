/*
   Copyright (C) 2014-2017 Free Software Foundation, Inc.
   Written by Sergey Kashyrin

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
#include <ctype.h>
#include <errno.h>
#include <math.h>

/* Force symbol exports */
#define	COB_LIB_EXPIMP

#include "libcob.h"
#include "coblocal.h"

// overflow flag for numeric.cpp
int cob_not_finite = 0;

/////////////////////////////////////////////////////////////////////////////
// assignment operators

cob_decimal & cob_decimal::operator=(const cob_s64_t rhs)
{
	scale = 0;
#if defined(COB_LI_IS_LL) || defined(_WIN64)
	value = rhs;
#else
	signed int i = (signed int)(rhs >> 32);
	value = i;
	value <<= 32;
	unsigned int u = (unsigned int)(rhs & 0xFFFFFFFF);
	value += u;
#endif
	return *this;
}

cob_decimal & cob_decimal::operator=(const cob_u64_t rhs)
{
	scale = 0;
#if defined(COB_LI_IS_LL) || defined(_WIN64)
	value = rhs;
#else
	unsigned int u = (unsigned int)(rhs >> 32);
	value = u;
	value <<= 32;
	u = (unsigned int)(rhs & 0xFFFFFFFF);
	value += u;
#endif
	return *this;
}

cob_decimal & cob_decimal::operator=(const double rhs)
{
	cob_u64_t t1;
	memset(&t1, ' ', sizeof(t1));
	union {
		double		d1;
		cob_u64_t	l1;
	} ud;
	ud.d1 = rhs;
	if(ud.l1 == 0 || ud.l1 == t1 || !ISFINITE(rhs)) {
		value = 0;
		scale = 0;
		return *this;
	}

	mpf_class temp(rhs);

#ifdef _MSC_VER
	long lscale;
	std::string str = temp.get_str(lscale, 10);
#else
	mp_exp_t lscale;
	std::string str = temp.get_str(lscale, 10, 0);
#endif
	int len = (int) str.length();
	if(len == 0) {
		value = 0;
		scale = 0;
		return *this;
	}
	if(str[0] == '-') {
		--len;
	}

	value.set_str(str, 10);
	len -= lscale;
	if(len >= 0) {
		scale = len;
	} else {
		mpz_class tmpz;
		mpz_ui_pow_ui(tmpz.get_mpz_t(), 10UL, -len);
		value *= tmpz;
		scale = 0;
	}
	return *this;
}


/////////////////////////////////////////////////////////////////////////////
// operator +

cob_decimal cob_decimal::operator+(const cob_decimal & rhs) const
{
	cob_decimal res;
	if(scale == rhs.scale) {
		res.scale = scale;
		res.value = value + rhs.value;
		return res;
	}
	if(scale < rhs.scale) {
		mpz_ui_pow_ui(res.value.get_mpz_t(), 10, rhs.scale - scale);
		res.scale = rhs.scale;
		res.value *= value;
		res.value += rhs.value;
		return res;
	}
	mpz_ui_pow_ui(res.value.get_mpz_t(), 10, scale - rhs.scale);
	res.scale = scale;
	res.value *= rhs.value;
	res.value += value;
	return res;
}

cob_decimal cob_decimal::operator+(const signed int rhs) const
{
	cob_decimal res;
	if(scale < 0) {
		mpz_ui_pow_ui(res.value.get_mpz_t(), 10, -scale);
		res.value *= value;
		res.value += rhs;
		return res;
	}
	if(scale > 0) {
		mpz_class temp;
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, scale);
		res.value = temp * rhs + value;
		res.scale = scale;
		return res;
	}
	res.value = value + rhs;
	return res;
}

cob_decimal cob_decimal::operator+(const unsigned int rhs) const
{
	cob_decimal res;
	if(scale < 0) {
		mpz_ui_pow_ui(res.value.get_mpz_t(), 10, -scale);
		res.value *= value;
		res.value += rhs;
		return res;
	}
	if(scale > 0) {
		mpz_class temp;
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, scale);
		res.value = temp * rhs + value;
		res.scale = scale;
		return res;
	}
	res.value = value + rhs;
	return res;
}

cob_decimal cob_decimal::operator+(const cob_s64_t rhs) const
{
	cob_decimal res = rhs;
	if(scale < 0) {
		mpz_class temp;
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, -scale);
		temp *= value;
		res.value += temp;
		return res;
	}
	if(scale > 0) {
		mpz_class temp;
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, scale);
		res.value *= temp;
		res.scale = scale;
	}
	res.value += value;
	return res;
}

cob_decimal cob_decimal::operator+(const cob_u64_t rhs) const
{
	cob_decimal res = rhs;
	if(scale < 0) {
		mpz_class temp;
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, -scale);
		temp *= value;
		res.value += temp;
		return res;
	}
	if(scale > 0) {
		mpz_class temp;
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, scale);
		res.value *= temp;
		res.scale = scale;
	}
	res.value += value;
	return res;
}

/////////////////////////////////////////////////////////////////////////////
// operator +=

cob_decimal & cob_decimal::operator+=(const cob_decimal & rhs)
{
	if(scale == rhs.scale) {
		value += rhs.value;
		return *this;
	}
	cob_decimal temp;
	if(scale < rhs.scale) {
		mpz_ui_pow_ui(temp.value.get_mpz_t(), 10, rhs.scale - scale);
		scale = rhs.scale;
		value *= temp.value;
		value += rhs.value;
		return *this;
	}
	mpz_ui_pow_ui(temp.value.get_mpz_t(), 10, scale - rhs.scale);
	temp.value *= rhs.value;
	value += temp.value;
	return *this;
}

cob_decimal & cob_decimal::operator+=(const signed int rhs)
{
	if(scale == 0) {
		value += rhs;
		return *this;
	}
	if(scale < 0) {
		mpz_class temp;
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, -scale);
		value *= temp;
		scale = 0;
		value += rhs;
		return *this;
	}
	cob_decimal res = rhs;
	mpz_class temp;
	mpz_ui_pow_ui(temp.get_mpz_t(), 10, scale);
	res.value *= temp;
	value += res.value;
	return *this;
}

cob_decimal & cob_decimal::operator+=(const unsigned int rhs)
{
	if(scale == 0) {
		value += rhs;
		return *this;
	}
	if(scale < 0) {
		mpz_class temp;
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, -scale);
		value *= temp;
		scale = 0;
		value += rhs;
		return *this;
	}
	cob_decimal res = rhs;
	mpz_class temp;
	mpz_ui_pow_ui(temp.get_mpz_t(), 10, scale);
	res.value *= temp;
	value += res.value;
	return *this;
}

cob_decimal & cob_decimal::operator+=(const cob_s64_t rhs)
{
#if defined(COB_LI_IS_LL) || defined(_WIN64)
	if(scale == 0) {
		value += rhs;
		return *this;
	}
#endif
	cob_decimal res = rhs;
	if(scale < 0) {
		mpz_class temp;
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, -scale);
		value *= temp;
		scale = 0;
	} else if(scale > 0) {
		mpz_class temp;
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, scale);
		res.value *= temp;
	}
	value += res.value;
	return *this;
}

cob_decimal & cob_decimal::operator+=(const cob_u64_t rhs)
{
#if defined(COB_LI_IS_LL) || defined(_WIN64)
	if(scale == 0) {
		value += rhs;
		return *this;
	}
#endif
	cob_decimal res = rhs;
	if(scale < 0) {
		mpz_class temp;
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, -scale);
		value *= temp;
		scale = 0;
	} else if(scale > 0) {
		mpz_class temp;
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, scale);
		res.value *= temp;
	}
	value += res.value;
	return *this;
}

/////////////////////////////////////////////////////////////////////////////
// operator -

cob_decimal cob_decimal::operator-(const unsigned int rhs) const
{
	cob_decimal res;
	if(scale < 0) {
		mpz_ui_pow_ui(res.value.get_mpz_t(), 10, -scale);
		res.value *= value;
		res.value -= rhs;
		return res;
	}
	if(scale > 0) {
		mpz_class temp;
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, scale);
		res.value = value - (temp * rhs);
		res.scale = scale;
		return res;
	}
	res.value = value - rhs;
	return res;
}

cob_decimal cob_decimal::operator-(const cob_u64_t rhs) const
{
	cob_decimal res = rhs;
	if(scale < 0) {
		mpz_class temp;
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, -scale);
		temp *= value;
		res.value = temp - res.value;
		return res;
	}
	if(scale > 0) {
		mpz_class temp;
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, scale);
		res.value *= temp;
		res.scale = scale;
	}
	res.value = value - res.value;
	return res;
}

/////////////////////////////////////////////////////////////////////////////
// operator -=

cob_decimal & cob_decimal::operator-=(const unsigned int rhs)
{
	if(scale == 0) {
		value -= rhs;
		return *this;
	}
	if(scale < 0) {
		mpz_class temp;
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, -scale);
		value *= temp;
		scale = 0;
		value -= rhs;
		return *this;
	}
	cob_decimal res = rhs;
	mpz_class temp;
	mpz_ui_pow_ui(temp.get_mpz_t(), 10, scale);
	res.value *= temp;
	value -= res.value;
	return *this;
}

cob_decimal & cob_decimal::operator-=(const cob_u64_t rhs)
{
#if defined(COB_LI_IS_LL) || defined(_WIN64)
	if(scale == 0) {
		value -= rhs;
		return *this;
	}
#endif
	cob_decimal res = rhs;
	if(scale < 0) {
		mpz_class temp;
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, -scale);
		value *= temp;
		scale = 0;
	} else if(scale > 0) {
		mpz_class temp;
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, scale);
		res.value *= temp;
	}
	value -= res.value;
	return *this;
}

/////////////////////////////////////////////////////////////////////////////
// operator *

cob_decimal cob_decimal::operator*(const cob_s64_t rhs) const
{
#if defined(COB_LI_IS_LL) || defined(_WIN64)
	cob_decimal res = *this;
	res.value *= rhs;
	return res;
#else
	cob_decimal res = rhs;
	res.scale = scale;
	res.value *= value;
	return res;
#endif
}

cob_decimal cob_decimal::operator*(const cob_u64_t rhs) const
{
#if defined(COB_LI_IS_LL) || defined(_WIN64)
	cob_decimal res = *this;
	res.value *= rhs;
	return res;
#else
	cob_decimal res = rhs;
	res.scale = scale;
	res.value *= value;
	return res;
#endif
}

/////////////////////////////////////////////////////////////////////////////
// operator *=

cob_decimal & cob_decimal::operator*=(const cob_s64_t rhs)
{
#if defined(COB_LI_IS_LL) || defined(_WIN64)
	value *= rhs;
#else
	cob_decimal res = rhs;
	value *= res.value;
#endif
	return *this;
}

cob_decimal & cob_decimal::operator*=(const cob_u64_t rhs)
{
#if defined(COB_LI_IS_LL) || defined(_WIN64)
	value *= rhs;
#else
	cob_decimal res = rhs;
	value *= res.value;
#endif
	return *this;
}

/////////////////////////////////////////////////////////////////////////////
// operator /
cob_decimal cob_decimal::operator/(const cob_decimal & rhs) const
{
	cob_decimal res;
	res.scale = scale - rhs.scale;
	int sc = (res.scale < 0) ? -res.scale : 0;
	sc += COB_MAX_DIGITS;
	mpz_ui_pow_ui(res.value.get_mpz_t(), 10, sc);
	res.scale += sc;
	res.value *= value;
	res.value /= rhs.value;
	return res;
}

/////////////////////////////////////////////////////////////////////////////
// operator /=
cob_decimal & cob_decimal::operator/=(const cob_decimal & rhs)
{
	cob_decimal res;
	res.scale = scale - rhs.scale;
	int sc = (res.scale < 0) ? -res.scale : 0;
	sc += COB_MAX_DIGITS;
	mpz_ui_pow_ui(res.value.get_mpz_t(), 10, sc);
	scale = res.scale + sc;
	value *= res.value;
	value /= rhs.value;
	return *this;
}

/////////////////////////////////////////////////////////////////////////////
// cast functions

cob_decimal::operator signed int()
{
	if(scale < 0) {
		mpz_class temp;
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, -scale);
		temp *= value;
		return (signed int) temp.get_si();
	}
	if(scale > 0) {
		mpz_class temp;
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, scale);
		temp = value / temp;
		return (signed int) temp.get_si();
	}
	return (signed int) value.get_si();
}

cob_decimal::operator unsigned int()
{
	if(scale < 0) {
		mpz_class temp;
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, -scale);
		temp *= value;
		return (unsigned int) temp.get_ui();
	}
	if(scale > 0) {
		mpz_class temp;
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, scale);
		temp = value / temp;
		return (unsigned int) temp.get_ui();
	}
	return (unsigned int) value.get_ui();
}

cob_decimal::operator cob_s64_t()
{
#if defined(COB_LI_IS_LL) || defined(_WIN64)
	if(scale < 0) {
		mpz_class temp;
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, -scale);
		temp *= value;
		return (cob_s64_t) temp.get_si();
	}
	if(scale > 0) {
		mpz_class temp;
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, scale);
		temp = value / temp;
		return (cob_s64_t) temp.get_si();
	}
	return (cob_s64_t) value.get_si();
#else
	mpz_class temp;
	if(scale < 0) {
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, -scale);
		temp *= value;
	} else if(scale > 0) {
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, scale);
		temp = value / temp;
	} else {
		temp = value;
	}
	cob_s64_t ret = temp.get_ui() & 0x0FFFFFFFF;
	temp >>= 32;
	if(temp != 0) {
		ret |= ((cob_s64_t)(temp.get_ui() & 0x0FFFFFFFF)) << 32;
	}
	return ret;
#endif
}

cob_decimal::operator cob_u64_t()
{
#if defined(COB_LI_IS_LL) || defined(_WIN64)
	if(scale < 0) {
		mpz_class temp;
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, -scale);
		temp *= value;
		return (cob_u64_t) temp.get_ui();
	}
	if(scale > 0) {
		mpz_class temp;
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, scale);
		temp = value / temp;
		return (cob_u64_t) temp.get_ui();
	}
	return (cob_u64_t) value.get_ui();
#else
	mpz_class temp;
	if(scale < 0) {
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, -scale);
		temp *= value;
	} else if(scale > 0) {
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, scale);
		temp = value / temp;
	} else {
		temp = value;
	}
	cob_u64_t ret = temp.get_ui() & 0x0FFFFFFFF;
	temp >>= 32;
	if(temp != 0) {
		ret |= ((cob_u64_t)(temp.get_ui() & 0x0FFFFFFFF)) << 32;
	}
	return ret;
#endif
}

cob_decimal::operator double()
{
	cob_not_finite = 0;
	mpf_class val(value);
	mpz_class temp;
	if(scale > 0) {
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, scale);
		val = val / temp;
	} else if(scale < 0) {
		mpz_ui_pow_ui(temp.get_mpz_t(), 10, -scale);
		val *= temp;
	}
	double ret = val.get_d();
	if(!ISFINITE(ret)) {
		cob_not_finite = 1;
		return 0.;
	}
	return ret;
}

/////////////////////////////////////////////////////////////////////////////
// helper functions

void cob_decimal::print(FILE * f)
{
	std::string s = value.get_str();
	if(scale < 0) {
		int n = scale;
		do {
			s += '0';
		} while(++n);
	}
	if(scale > 0) {
		bool sign = false;
		if(s[0] == '-') {
			sign = true;
			s = s.substr(1);
		}
		while(((int)s.length()) <= scale) {
			s = '0' + s;
		}
		if(sign) {
			s = '-' + s;
		}
		s = s.substr(0, s.length() - scale) + '.' + s.substr(s.length() - scale);
	}
	fprintf(f, "%s\n", s.c_str());
}

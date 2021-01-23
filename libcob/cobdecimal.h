/*
   Copyright (C) 2005-2012, 2014-2016 Free Software Foundation, Inc.
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

//#ifndef COB_COBDECIMAL_H
//#define COB_COBDECIMAL_H

class cob_decimal
{
public:
	mpz_class	value;
	int			scale;	// Decimal scale

	/////////////////////////////////////////////////////////////////////////////
	// constructors/copy/destructor
	cob_decimal() : scale(0) {}
	~cob_decimal() {}
	cob_decimal(const cob_decimal & rhs)
	{
		scale = rhs.scale;
		value = rhs.value;
	};
	cob_decimal(const char * val, int scal) : value(val, 10), scale(scal) {}
	cob_decimal(const signed int rhs)
	{
		*this = rhs;
	}
	cob_decimal(const unsigned int rhs)
	{
		*this = rhs;
	}
	cob_decimal(const cob_s64_t rhs)
	{
		*this = rhs;
	}
	cob_decimal(const cob_u64_t rhs)
	{
		*this = rhs;
	}
	cob_decimal(const double rhs)
	{
		*this = rhs;
	}
	cob_decimal & operator=(const cob_decimal & rhs)
	{
		scale = rhs.scale;
		value = rhs.value;
		return *this;
	}

	/////////////////////////////////////////////////////////////////////////////
	// assignment operators
	cob_decimal & operator=(const signed int rhs)
	{
		scale = 0;
		value = rhs;
		return *this;
	}
	cob_decimal & operator=(const unsigned int rhs)
	{
		scale = 0;
		value = rhs;
		return *this;
	}
	cob_decimal & operator=(const cob_s64_t rhs);
	cob_decimal & operator=(const cob_u64_t rhs);
	cob_decimal & operator=(const double rhs);
	cob_decimal & operator=(mpz_class & mp)
	{
		value = mp;
		scale = 0;
		return *this;
	}

	/////////////////////////////////////////////////////////////////////////////
	// operator +
	cob_decimal operator+(const cob_decimal & rhs) const;
	cob_decimal operator+(const signed int rhs) const;
	cob_decimal operator+(const unsigned int rhs) const;
	cob_decimal operator+(const cob_s64_t rhs) const;
	cob_decimal operator+(const cob_u64_t rhs) const;

	/////////////////////////////////////////////////////////////////////////////
	// operator +=
	cob_decimal & operator+=(const cob_decimal & rhs);
	cob_decimal & operator+=(const signed int rhs);
	cob_decimal & operator+=(const unsigned int rhs);
	cob_decimal & operator+=(const cob_s64_t rhs);
	cob_decimal & operator+=(const cob_u64_t rhs);

	/////////////////////////////////////////////////////////////////////////////
	// operator -
	cob_decimal operator-() const
	{
		cob_decimal res;
		res.value = -value;
		res.scale = scale;
		return res;
	}
	cob_decimal operator-(const cob_decimal & rhs) const
	{
		return (*this + -rhs);
	}
	cob_decimal operator-(const signed int rhs) const
	{
		return (*this + -rhs);
	}
	cob_decimal operator-(const unsigned int rhs) const;
	cob_decimal operator-(const cob_s64_t rhs) const
	{
		return (*this + -rhs);
	}
	cob_decimal operator-(const cob_u64_t rhs) const;

	/////////////////////////////////////////////////////////////////////////////
	// operator -=
	cob_decimal & operator-=(const cob_decimal & rhs)
	{
		cob_decimal res = rhs;
		res.value = -res.value;
		*this += res;
		return *this;
	}
	cob_decimal & operator-=(const signed int rhs)
	{
		*this += -rhs;
		return *this;
	}
	cob_decimal & operator-=(const unsigned int rhs);
	cob_decimal & operator-=(const cob_s64_t rhs)
	{
		*this += -rhs;
		return *this;
	}
	cob_decimal & operator-=(const cob_u64_t rhs);

	/////////////////////////////////////////////////////////////////////////////
	// operator *
	cob_decimal operator*(const cob_decimal & rhs) const
	{
		cob_decimal res;
		res.scale = scale + rhs.scale;
		res.value = value * rhs.value;
		return res;
	}
	cob_decimal operator*(const signed int rhs) const
	{
		cob_decimal res = *this;
		res.value *= rhs;
		return res;
	}
	cob_decimal operator*(const unsigned int rhs) const
	{
		cob_decimal res = *this;
		res.value *= rhs;
		return res;
	}
	cob_decimal operator*(const cob_s64_t rhs) const;
	cob_decimal operator*(const cob_u64_t rhs) const;

	/////////////////////////////////////////////////////////////////////////////
	// operator *=
	cob_decimal & operator*=(const cob_decimal & rhs)
	{
		scale += rhs.scale;
		value *= rhs.value;
		return *this;
	}
	cob_decimal & operator*=(const signed int rhs)
	{
		value *= rhs;
		return *this;
	}
	cob_decimal & operator*=(const unsigned int rhs)
	{
		value *= rhs;
		return *this;
	}
	cob_decimal & operator*=(const cob_s64_t rhs);
	cob_decimal & operator*=(const cob_u64_t rhs);

	/////////////////////////////////////////////////////////////////////////////
	// operator /
	cob_decimal operator/(const cob_decimal & rhs) const;

	/////////////////////////////////////////////////////////////////////////////
	// operator /=
	cob_decimal & operator/=(const cob_decimal & rhs);

	/////////////////////////////////////////////////////////////////////////////
	// cast functions
	operator signed int();
	operator unsigned int();
	operator cob_s64_t();
	operator cob_u64_t();
	operator double();
	operator mpz_class & ()
	{
		return value;
	}

	/////////////////////////////////////////////////////////////////////////////
	// helper functions
	void print() { print(stdout);}
	void print(FILE * f);
};

template <class LHS> cob_decimal operator+(const LHS & lhs, const cob_decimal & rhs)
{
	return rhs + lhs;
}

template <class LHS> cob_decimal operator-(const LHS & lhs, const cob_decimal & rhs)
{
	return -rhs + lhs;
}

template <class LHS> cob_decimal operator*(const LHS & lhs, const cob_decimal & rhs)
{
	return rhs * lhs;
}

template <class LHS> cob_decimal operator/(const LHS & lhs, const cob_decimal & rhs)
{
	return ((cob_decimal)lhs) / rhs;
}

//#endif	/* COB_COBDECIMAL_H */

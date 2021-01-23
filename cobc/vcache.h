/*
   Copyright (C) 2005-2012, 2014-2017 Free Software Foundation, Inc.
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

#if !defined(_INC_VCACHE)
#define _INC_VCACHE

template <class T>
class vvector
{

private:
	T 	*	data;
	int		SZ;
	int		count;

public:
	vvector(int sz = 10)
	{
		SZ = sz;
		data = new T[SZ];
		count = 0;
	}

	~vvector()
	{
		delete [] data;
	}

public:
	int getCount()
	{
		return count;
	}

	void clear()
	{
		count = 0;
	}

	T & get(int i)
	{
		if(i < 0 || i >= count) {
			throw "vvector Index violation";
		}
		return data[i];
	}

	T & operator [](int i)
	{
		if(i < 0 || i >= count) {
			throw "vvector Index violation";
		}
		return data[i];
	}

	T remove(int i)
	{
		if(i < 0 || i >= count) {
			throw "vvector Index violation";
		}
		T t = data[i];
		if(i != count - 1) {
			memcpy(data + i, data + i + 1, (count - i - 1) * sizeof(T));
		}
		--count;
		return t;
	}

	void add(T t)
	{
		if(count == SZ) {
			T * d2 = new T[SZ + SZ];
			memcpy(d2, data, SZ * sizeof(T));
			delete data;
			data = d2;
			SZ += SZ;
		}
		data[count] = t;
		++count;
	}

	void set(int i, T t)
	{
		if(i < 0 || i >= count) {
			throw "vvector Index violation";
		}
		data[i] = t;
	}
};

template <class T>
class vholder
{
public:
	vholder *	next;
	char 	*	name;
	T			obj;
public:
	vholder(const char * nm, T val)
	{
		size_t i = strlen(nm) + 1;
		name = new char[i];
		memcpy(name, nm, i);
		obj = val;
		next = NULL;
	}

	~vholder()
	{
		delete [] name;
	}
};

template <class T>
class vcache
{

private:
	vholder<T> **	data;
	int				SZ;			// size of cache tables
	int				lastIx;
	vholder<T> *	lastH;
	int				count;

public:
	vcache(int sz = 1009)
	{
		SZ = sz;
		data = new vholder<T> * [SZ];
		for(int i = 0; i < SZ; ++i) {
			data[i] = NULL;
		}
		count = 0;
	}
	~vcache()
	{
		for(int i = 0; i < SZ; ++i) {
			for(vholder<T> * h = data[i]; h != NULL;) {
				vholder<T> * h2 = h;
				h = h->next;
				delete h2;
			}
		}
		delete [] data;
	}

private:
	int hash(const char * s)
	{
		int h = 0;
		for(int i = 0; i < 8 && *s != 0; ++i) {
			h <<= 3;
			h += (*s++ & 0x1F);
		}
		h &= 0x7FFFFFFF;
		h %= SZ;
		return h;
	}

public:
	int getCount()
	{
		return count;
	}

	vholder<T> * getFirstHolder()
	{
		for(lastIx = 0; lastIx < SZ; ++lastIx) {
			if(data[lastIx] != NULL) {
				break;
			}
		}
		if(lastIx >= SZ) {
			return NULL;
		}
		lastH = data[lastIx];
		return lastH;
	}

	vholder<T> * getNextHolder()
	{
		if(lastH == NULL) {
			return NULL;
		}
		lastH = lastH->next;
		if(lastH != NULL) {
			return lastH;
		}
		for(lastIx = lastIx + 1; lastIx < SZ; ++lastIx) {
			if(data[lastIx] != NULL) {
				break;
			}
		}
		if(lastIx >= SZ) {
			return NULL;
		}
		lastH = data[lastIx];
		return lastH;
	}

	T getFirstElement()
	{
		if(getFirstHolder() == NULL) {
			return NULL;
		}
		return lastH->obj;
	}

	T getNextElement()
	{
		if(lastH == NULL) {
			return NULL;
		}
		if(getNextHolder() == NULL) {
			return NULL;
		}
		return lastH->obj;
	}

	void clear()
	{
		for(int i = 0; i < SZ; ++i) {
			for(vholder<T> * h = data[i]; h != NULL;) {
				vholder<T> * h2 = h;
				h = h->next;
				delete h2;
			}
			data[i] = NULL;
		}
		count = 0;
	}

	T get(const char * s)
	{
		int i = hash(s);
		for(vholder<T> * h = data[i]; h != NULL; h = h->next) {
			if(0 == strcmp(s, h->name)) {
				return h->obj;
			}
		}
		return NULL;
	}

	T operator [](const char * s)
	{
		return get(s);
	}

	T remove(const char * s)
	{
		T v = get(s);
		if(v != NULL) {
			put(s, NULL);
		}
		return v;
	}

	void put(const char * s, T o)
	{
		int i = hash(s);
		if(data[i] == NULL) {
			if(o != NULL) {
				data[i] = new vholder<T>(s, o);
				++count;
			}
			return;
		}
		vholder<T> * h = data[i];
		vholder<T> * p = NULL;
		for(;;) {
			if(0 == strcmp(s, h->name)) {
				if(o != NULL) {
					h->obj = o;
					return;
				}
				if(p == NULL) {
					data[i] = h->next;
				} else {
					p->next = h->next;
				}
				delete h;
				--count;
				return;
			}
			if(h->next == NULL) {
				if(o != NULL) {
					h->next = new vholder<T>(s, o);
					++count;
				}
				return;
			}
			p = h;
			h = h->next;
		}
	}

};

#endif	// _INC_VCACHE

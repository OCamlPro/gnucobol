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

#ifndef	_GNU_SOURCE
	#define _GNU_SOURCE	1
#endif

#include <stdio.h>

/* Force symbol exports */
#define	COB_LIB_EXPIMP

#include "libcob.h"
#include "coblocal.h"

COB_EXPIMP int CBL_ERROR_PROC(unsigned char *, unsigned char *);

int CBL_ERROR_PROC(unsigned char * x, unsigned char * pptr)
{
	return cob_sys_error_proc(x, pptr);
}

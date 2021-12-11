/*
   Copyright (C) 2021 Free Software Foundation, Inc.
   Written by Christian Lademann

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

#ifndef __xad_h
#define __xad_h	1

#include	"xad-dtree.h"
#include	"xad-keymap.h"

#define TRC() (fprintf(stderr, "TRC:%s,%d\n", __FILE__, __LINE__))

#ifndef __xad_c
#	define EXTERN extern
#else
#	define EXTERN
#endif

EXTERN char *chomp (char *s);

#undef EXTERN
#endif

/*
   Copyright (C) 2020-2023 Free Software Foundation, Inc.
   Written by Simon Sobisch

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

#ifndef COB_VERSION_H

#define __LIBCOB_VERSION	3
#define __LIBCOB_VERSION_MINOR		3
#define __LIBCOB_VERSION_PATCHLEVEL	0	/* Note: possibly differs from patchelvel shown with cobc --version! */

#define __LIBCOB_RELEASE (__LIBCOB_VERSION * 10000 + __LIBCOB_VERSION_MINOR * 100 + __LIBCOB_VERSION_PATCHLEVEL)

#ifndef COB_EXPIMP
#if	((defined(_WIN32) || defined(__CYGWIN__)) && !defined(__clang__))
#define COB_EXPIMP	__declspec(dllimport) extern
#else
#define COB_EXPIMP	extern
#endif
#endif

COB_EXPIMP const char *libcob_version (void);
COB_EXPIMP int		set_libcob_version (int *, int *, int *);

COB_EXPIMP void		print_version (void);
COB_EXPIMP void		print_version_summary (void);

#endif

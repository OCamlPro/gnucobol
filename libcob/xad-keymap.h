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


#ifndef __xad_keymap_h
#define __xad_keymap_h	1

#include	"xad-dtree.h"

#ifndef __xad_keymap_c
#	define	EXTERN extern
#else
#	define	EXTERN
#endif

EXTERN int find_curses_key_number (char *keyname);
EXTERN int find_cob_key_number (char *keyname);
EXTERN int xad_parse_keystring (char *keystring, int *list, int max, int cobfcns);


EXTERN dtree_t *xad_init (char *keymappath);
EXTERN int xad_read_keymap (char *path, dtree_t *tree);
EXTERN int xad_is_initialized ();

EXTERN int xad_read_keymap_file (char *path);
EXTERN int xad_reset_keymap ();
EXTERN int xad_init_default_keymap ();
EXTERN int xad_add_keymap (char *pkey, char *pval);

EXTERN int xad_getch();
EXTERN int xad_ungetch(int ch);

#ifndef __xad_keymap_c
EXTERN dtree_t	*XAD_KEYMAP;
#endif

#undef EXTERN
#endif

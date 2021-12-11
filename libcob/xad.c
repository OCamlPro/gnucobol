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

#define	__xad_c

#include	<curses.h>
#include	<string.h>
#include	"xad.h"


char
*chomp (char *s) {
	int l;

	if (s == NULL)
		return NULL;

	if ((l = strlen (s)) == 0)
		return s;

	if (*(s + l - 1) == '\n' || *(s + l - 1) == '\r')
		*(s + l - 1) = '\0';

	if (l > 1)
		if (*(s + l - 2) == '\n' || *(s + l - 2) == '\r')
			*(s + l - 2) = '\0';

	return s;
}



#if 0
int main (int argc, char **argv) {
	dtree_t	*tree;
	char	ins[50];

	tree = xad_init ("keymap");

	dtree_debug (tree);

	fgets (ins, sizeof (ins) - 1, stdin);

	{
		int	v;

		initscr ();
		noecho ();
		cbreak ();
		keypad (stdscr, TRUE);

		while ((v = xad_getch ()) >= 0) {
			fprintf (stderr, "xad_getch:<%x> (%d)\r\n", v, v);
		}

		endwin ();
	}
}
#endif

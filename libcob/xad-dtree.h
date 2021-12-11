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

#ifndef __xad_dtree_h
#define __xad_dtree_h	1

#define TRC() (fprintf(stderr, "TRC:%s,%d\n", __FILE__, __LINE__))


struct dtree_s {
	int	key;
	int	value;
	void	*valdata;
	struct dtree_s	*same;
	struct dtree_s	*next;
};

typedef struct dtree_s dtree_t;

#define	DTREE_NULL	-1	/* NULL: undefined, etc. */
#define DTREE_IGNORE	-2	/* IGNORE: value is to be ignored */
#define DTREE_LIST	-3	/* LIST: multiple values */

#ifndef __xad_dtree_c
#	define EXTERN extern
#else
#	define EXTERN
#endif

EXTERN dtree_t	*dtree_alloc ();
EXTERN int	dtree_free (dtree_t *);
EXTERN dtree_t	*dtree_init ();
EXTERN int	dtree_destroy (dtree_t *);
EXTERN dtree_t	*dtree_add_branch (dtree_t *t, int k, int v, void *valdata);
EXTERN dtree_t	*dtree_match_next (dtree_t *t, int k, int *v);
EXTERN dtree_t	*dtree_add_string (dtree_t *tree, char *s, int v, void *valdata);
EXTERN dtree_t	*dtree_add_list (dtree_t *tree, int *list, int v, void *valdata);
EXTERN int	dtree_match_string (dtree_t *tree, char *s);
EXTERN int	dtree_match_list (dtree_t *tree, int *list, int len, int *matched, int *maxmatch, int *maxmatched, void **valdata);
EXTERN void	dtree_debug (dtree_t *tree);

EXTERN char *chomp (char *s);

#undef EXTERN
#endif

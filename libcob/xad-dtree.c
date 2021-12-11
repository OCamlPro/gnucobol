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

#define __xad_dtree_c 1

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	"xad-dtree.h"


/*
 * Allocate a single dtree-element
 */
dtree_t
*dtree_alloc () {
	dtree_t	*t = NULL;

	if ((t = (dtree_t *)malloc (sizeof(dtree_t))) == NULL)
		return NULL;

	t->same = t->next = NULL;
	t->key = t->value = DTREE_NULL;
	t->valdata = NULL;

	return t;
}


/*
 * Free a single dtree-element
 */
int
dtree_free (dtree_t *t) {
	if (t == NULL)
		return -1;

	if (t->valdata != NULL) {
		free ((void *)t->valdata);
	}

	free ((void *)t);

	return 0;
}


/*
 * Allocate and initialize a dtree
 */
dtree_t
*dtree_init () {
	return dtree_alloc ();
}


/*
 * Free a complete dtree
 */
int
dtree_destroy (dtree_t *t) {
	if (t == NULL) {
		return 0;
	}

	if (t->next != NULL) {
		dtree_destroy (t->next);
		t->next = NULL;
	}

	if (t->same != NULL) {
		dtree_destroy (t->same);
		t->same = NULL;
	}

	dtree_free (t);

	return 0;
}


dtree_t
*dtree_add_branch (dtree_t *t, int k, int v, void *valdata) {
	if (t == NULL || k < 0) {
		return NULL;
	}

	if (t->next == NULL) {
		t->next = dtree_alloc ();
	}

	if (t->next == NULL) {
		return NULL;
	}

	t = t->next;

	if (t->key < 0) {
		t->key = k;
		if (v != DTREE_NULL) {
			t->value = v;
			t->valdata = valdata;
		}
		return t;
	}

	do {
		if (t->key == k) {
			if (v != DTREE_NULL) {
				t->value = v;
				t->valdata = valdata;
			}
			return t;
		}

		if (t->same != NULL) {
			t = t->same;
		} else {
			if ((t->same = dtree_alloc ()) != NULL) {
				t = t->same;
				t->key = k;
				if (v != DTREE_NULL) {
					t->value = v;
					t->valdata = valdata;
				}
				return t;
			} else {
				break; // FIXME: Error-handling!
			}
		}
	} while (t != NULL);

	return NULL;
}


dtree_t
*dtree_match_next (dtree_t *t, int k, int *v) {
	int val = DTREE_NULL;

	if (t == NULL || k < 0) {
		return NULL;
	}

	if (t->next == NULL) {
		return NULL;
	}

	t = t->next;

	do {
		if (t->key == k) {
			val = t->value;
			break;
		}

		t = t->same;
	} while (t != NULL);

	if (v != NULL)
		*v = val;

	return t;
}


dtree_t
*dtree_add_string (dtree_t *tree, char *s, int v, void *valdata) {
	dtree_t	*t;
	char	*p;
	int	l, i;

	if (tree == NULL) {
		return NULL;
	}

	if (s == NULL) {
		return NULL;
	}

	if ((l = strlen(s)) == 0) {
		return NULL;
	}

	t = tree;

	for (i = 0; i < l; i++) {
		unsigned char c = (unsigned char)*(s + i);
		t = dtree_add_branch (t, c, (i < l - 1 ? DTREE_NULL : v), valdata);
	}

	return tree;
}


dtree_t
*dtree_add_list (dtree_t *tree, int *list, int v, void *valdata) {
	dtree_t	*t;
	int *e;
	int	l, i;

	if (tree == NULL) {
		return NULL;
	}

	if (list == NULL) {
		return NULL;
	}

	if (*list < 0) {
		return NULL;
	}

	t = tree;

	e = list;
	while (*e >= 0) {
		t = dtree_add_branch (t, *e, (*(e + 1) >= 0 ? DTREE_NULL : v), valdata);
		e++;
	}

	return tree;
}


int
dtree_match_string (dtree_t *tree, char *s) {
	dtree_t	*t = NULL;
	int	v = DTREE_NULL;
	int	l, i;

	if (tree == NULL) {
		return -1;
	}

	if (s == NULL) {
		return -1;
	}

	if ((l = strlen(s)) == 0) {
		return -1;
	}

	t = tree;

	for (i = 0; i < l && t != NULL; i++) {
		unsigned char c = (unsigned char)*(s + i);
		t = dtree_match_next (t, c, &v);

		if (v != DTREE_NULL) {
			return v;
		}

		if (t == NULL) {
			break;
		}
	}

	return DTREE_NULL;
}


int
dtree_match_list (dtree_t *tree, int *list, int len, int *matched, int *maxmatch, int *maxmatched, void **valdata) {
	dtree_t	*t = NULL;
	int	v = DTREE_NULL;
	int	l, i;
	int	mmv = DTREE_NULL;
	int	mml = 0;
	dtree_t	*mmt = NULL;

	if (matched != NULL) {
		*matched = 0;
	}

	if (tree == NULL || list == NULL || len <= 0) {
		return -1;
	}

	t = tree;

	for (i = 0; i < len; i++) {
		int c = *(list + i);
		t = dtree_match_next (t, c, &v);

		if (v != DTREE_NULL) {
			mmv = v;
			mml = i + 1;
			mmt = t;

			if (t->next == NULL) {
				break;
			}
		}

		if (t == NULL) {
			break;
		} else {
			v = DTREE_NULL;
			mmt = NULL; // CHECKME
		}
	}

	if (matched != NULL) {
		if (v != DTREE_NULL) {
			*matched = i + 1;
		} else if (i == len) {
			*matched = len;
		}
	}

	if (maxmatch != NULL) {
		*maxmatch = mmv;
	}

	if (maxmatched != NULL) {
		*maxmatched = mml;
	}

	if (valdata != NULL && mmt != NULL) {
		*valdata = mmt->valdata;
	}

	return v;
}


void
dtree_debug (dtree_t *tree) {
	if (tree == NULL) {
		return;
	}

	fprintf (stderr, "T-%x\tK-%x\tV-%x\tS-%x\tN-%x\tD-%x\n",
		(unsigned int)tree,
		(tree->key >= 0 ? tree->key : 0),
		(tree->value != 0 ? tree->value : 0),
		(unsigned int)tree->same,
		(unsigned int)tree->next,
		(unsigned int)tree->valdata
	);
	dtree_debug (tree->same);
	dtree_debug (tree->next);
}

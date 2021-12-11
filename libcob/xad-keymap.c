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


#define	__xad_keymap_c

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<curses.h>
#include	<libgen.h>
#include	"screenio-common.h"
#include	"xad-keymap.h"
#include	"xad-cobkeys.h"
#include	"xad-curseskeys.h"


/* global keymappings */
static dtree_t	*XAD_KEYMAP = NULL;


/* global input keys stack */
#define	XAD_STACK_SIZE	50
static int	XAD_STACK[XAD_STACK_SIZE];


/*
 * Translate a name to a curses key
 */
int
find_curses_key_number (char *keyname) {
	int	i;

	if (keyname == NULL) {
		return -1;
	}

	if (strlen (keyname) == 0) {
		return -1;
	}

	if (sscanf (keyname, "%*[Kk]%*[Ee]%*[Yy]%*[_-]%*[Ff](%d)", &i) == 1 || sscanf (keyname, "%*[Kk]%*[Ee]%*[Yy]%*[_-]%*[Ff]%d", &i) == 1) {
		return KEY_F(i);
	}

	i = -1;

	while (curses_keynames[++i].name) {
		if (strcasecmp (keyname, curses_keynames[i].name) == 0) {
			return (unsigned int)curses_keynames[i].num;
		}
	}

	return -1;
}


/*
 * Translate a name to a cobol key / function name
 */
int
find_cob_key_number (char *keyname) {
	int	i;

	if (keyname == NULL) {
		return -1;
	}

	if (strlen(keyname) == 0) {
		return -1;
	}

	i = -1;

	while (cob_keynames[++i].name) {
		if (strcasecmp (keyname, cob_keynames[i].name) == 0) {
			return (unsigned int)cob_keynames[i].num;
		}
	}

	return -1;
}


/*
 * Parse a keystring into a list of keys
 *
 * keystring: Textual definition of key or keys
 *
 * keys: Pointer to an array of int to receive the corresponding key-values
 * max: Number of elements of "keys"
 * cobfcns: 1: resolve key-symbols as cobol functions names; else use curses names
 */
int
xad_parse_keystring (char *keystring, int *keys, int max, int cobfcns) {
	// static int keys[20];
	int	keys_used, n, failed, i;
	int	key_n;
	char	*p, *q;
	char	p1[1024];

	if (keystring == NULL) {
		return NULL;
	}

	if (strlen (keystring) == 0) {
		return NULL;
	}

	// for (i = 0; i < (sizeof (keys) / sizeof (keys[0])); i++) {
	for (i = 0; i < max; i++) {
		// keys[i] = -1;
		keys[i] = DTREE_NULL;
		// *(keys + i) = -1;
		// *(keys + i) = DTREE_NULL;
	}

	if (strlen (keystring) == 1) {
		/* A single char is just that ... */
		keys[0] = (unsigned char) *keystring;
		return 1;
	}

	keys_used = 0;

	failed = 0;

	p = keystring;

	while (*p) {
		if (*p == ',')
			p++;
		else if (sscanf (p, "\"%[^\"]\"%n", p1, &n) == 1) {
			q = p1;
			while (*q) {
				char	c = *q;

				if (!cobfcns) {
					if (c == '\\') {
						int	step = 1;
						char	c2 = *(q + 1);
						char	c3, c4;

						switch (c2) {
						case '\\':	c = '\\'; 	break;
						case '0':	c = '\0'; 	break;        // null
						case 'a':	c = '\007';	break;        // bell
						case 'b':	c = '\010';	break;        // backspace
						case 'e':	c = '\033';	break;        // escape
						case 'f':	c = '\011';	break;        // formfeed
						case 'n':	c = '\012';	break;        // newline
						case 'r':	c = '\015';	break;        // carriage return
						case 't':	c = '\011';	break;        // tab

						default:
#define _xpk_is_oct(c)	((c) >= '0' && (c) <= '7')
#define _xpk_is_hex(c)	(((c) >= '0' && (c) <= '9') || ((c) >= 'a' && (c) <= 'f') || ((c) >= 'A' && (c) <= 'F'))
#define _xpk_hex(c)	(((c) >= '0' && (c) <= '9') ? ((c) - '0') : (((c) >= 'a' && (c) <= 'f') ? ((c) - 'a' + 10) : ((c) - 'A' + 10)))

							step = 0;

							if (_xpk_is_oct(c2)) {        // octal
								// assume "lazy evaluation"
								if ((c3 = *(q + 2)) && (c4 = *(q + 3))) {
									if (_xpk_is_oct (c3) && _xpk_is_oct (c4)) {
										c = (c2 - '0') * 64 + (c3 - '0') * 8 +
										    (c4 - '0');
										step = 3;
									}
								}
							} else if (c2 == 'x' || c2 == 'X') {        // hex
								// assume "lazy evaluation"
								if ((c3 = *(q + 2)) && (c4 = *(q + 3))) {
									if (_xpk_is_hex (c3) && _xpk_is_hex (c4)) {
										c = _xpk_hex (c3) * 16 + _xpk_hex (c4);
										step = 3;
									}
								}

							} else if (c2 == '^') {        // ctrl, ASCII-only!
								// assume "lazy evaluation"
								if ((c3 = *(q + 2))) {
									if ((c3 >= '@' && c3 <= '_')) {
										c = (c3 - '@');
										step = 2;
									} else if ((c3 >= 'a' && c3 <= 'z')) {
										c = (c3 - 'a') + 1;
										step = 2;
									}
								}
							}

							if (step <= 0)
								failed = 1;
							else
								q += step;

							break;
#undef _xpk_is_oct
#undef _xpk_is_hex
#undef _xpk_hex
						}
					}
				}

				// if (keys_used >= (sizeof (keys) / sizeof (keys[0]))) {
				if (keys_used >= max) {
					failed = 1;
				}

				if (failed) {
					break;
				}

				// keys[keys_used++] = (unsigned char)c;
				*(keys + keys_used++) = (unsigned char)c;
				q++;
			}

			p += n;
		} else if (!cobfcns && sscanf (p, "%*[Kk]%*[Ee]%*[Yy]%*[_-]%[A-Za-z0-9()_-]%n", p1, &n) == 1) {
			char	key_name[50];

			memset (key_name, 0, sizeof (key_name));
			strncpy (key_name, p, n);
			if ((key_n = find_curses_key_number (key_name)) >= 0) {
				// if (keys_used >= (sizeof (keys) / sizeof (keys[0]))) {
				if (keys_used >= max) {
					failed = 1;
					break;
				}

				// keys[keys_used++] = key_n;
				*(keys + keys_used++) = key_n;
			}

			p += n;
		} else if (cobfcns && sscanf (p, "%*[Cc]%*[Oo]%*[Bb]%*[_-]%[A-Za-z0-9()_-]%n", p1, &n) == 1) {
			char	key_name[50];

			memset (key_name, 0, sizeof (key_name));
			strncpy (key_name, p, n);
			if ((key_n = find_cob_key_number (key_name)) >= 0) {
				// if (keys_used >= (sizeof (keys) / sizeof (keys[0]))) {
				if (keys_used >= max) {
					failed = 1;
					break;
				}

				// keys[keys_used++] = key_n;
				*(keys + keys_used++) = key_n;
			}

			p += n;
		} else {
			failed = 1;
			break;
		}
	}

	if (failed) {
		for (i = 0; i < keys_used; i++) {
			// keys[i] = DTREE_NULL;
			*(keys + i) = DTREE_NULL;
		}

		return -1;
	}

	return keys_used;
}


/*
 * Debugging function
 */
void
xad_parsed_keystring_debug (int *l) {
	int *p = l;

	while (p && *p >= 0) {
		fprintf (stderr, "DTD <%x>\n", *p);
		p++;
	}

	fprintf (stderr, "\n");
}


/*
 * Add a mapping definition to a mapping tree
 *
 * tree: mapping tree to add to
 * pkey: textual representation of the keystroke sequence to map
 * pval: textual representation of the sequence of characters and functions to map to
 */
int
xad_add_keymapping (dtree_t *tree, char *pkey, char *pval) {
	int 	pkeys[20];
	int 	pvals[20];
	int 	*pvalsp = NULL;
	int	pvaln;

	if (tree == NULL || pkey == NULL || pval == NULL) {
		return -1;
	}

	if (xad_parse_keystring (pkey, pkeys, (sizeof (pkeys) / sizeof (pkeys[0])), 0) <= 0) {
		return -1;
	};

	pvaln = DTREE_NULL;

	/*
	 * If pval is just 1 char, assume that's the value.
	 * If pval is more than 1 char, try to parse it to a list of values. If that
	 * returns just 1 value, store that value, else store the list.
	 */
	if (strlen (pval) == 1) {
		pvaln = (int) *pval;
	} else {
		int n;

		n = xad_parse_keystring (pval, pvals, (sizeof (pvals) / sizeof (pvals[0])), 1);

		if (n == 1) {
			pvaln = pvals[0];
		} else if (n > 1) {
			if ((pvalsp = malloc (sizeof (int) * n)) == NULL) {
				// TODO: error, etc.
				return -1;
			}

			memcpy (pvalsp, pvals, sizeof (int) * n);
			pvaln = DTREE_LIST;
// xad_parsed_keystring_debug (pvals);
		}
	}

	if (pvaln != DTREE_NULL) {
		dtree_add_list (tree, pkeys, pvaln, pvalsp);
	}

	return 0;
}


/*
 * Read a file containing mapping definitions and add them to a mapping tree
 *
 * path: path of file
 * tree: mapping tree to add to
 */
int
xad_read_keymap (char *path, dtree_t *tree) {
	FILE	*inp;
	char	line[1024];
	char	pkey[1024];
	char	pval[1024];
	char	kpath[2048];
	char	*ln;

	if ((inp = fopen (path, "r")) == NULL) {
		/* TODO: error-handling */
		return -1;
	}

	while (fgets (line, sizeof (line) - 1, inp) != NULL) {
		chomp (line);

		ln = line;

		do {
			if (strlen (ln) <= 0) {
				break;
			}

			memset (pkey, 0, sizeof (pkey));
			memset (pval, 0, sizeof (pval));

			if (sscanf (ln, "%s %s", pkey, pval) < 2) {
				break;
			}

			if (strlen (pkey) <= 0 && strlen (pval) <= 0) {
				break;
			}

			if (strlen (pkey) > 0 && pkey[0] == '#') {
				break;
			}

			if (strcasecmp (pkey, "include") == 0) {
				/* include other files */
				/* TODO: prevent loops, prevent breakouts */

				if (*pval == '/') {
					strcpy(kpath, pval);
				} else {
					char *pc = strdup (path);
					snprintf (kpath, sizeof (kpath) - 1, "%s/%s", dirname (pc), pval);
					free (pc);
				}

				xad_read_keymap (kpath, tree);
				break;
			}

			xad_add_keymapping (tree, pkey, pval);
		} while (0);
	}

	fclose (inp);

	return 0;
}


/*
 * Add default mappings to a mapping tree
 */
static int
xad_set_default_keymap (dtree_t *tree) {
	int i;

#define _add_to_tree(k, v)	dtree_add_branch (tree, (k), (v), NULL)

	for (i = 0; i <= 32; i++) {
		_add_to_tree (KEY_F(i) , COB_SCR_F0 + i);
	}

	_add_to_tree (KEY_UP, COB_SCR_KEY_UP);
	_add_to_tree (KEY_DOWN, COB_SCR_KEY_DOWN);
	_add_to_tree(KEY_LEFT, COB_SCR_KEY_LEFT);
	_add_to_tree(KEY_RIGHT, COB_SCR_KEY_RIGHT);
	_add_to_tree(KEY_PPAGE, COB_SCR_PAGE_UP);
	_add_to_tree(KEY_NPAGE, COB_SCR_PAGE_DOWN);
	// _add_to_tree (KEY_TAB, COB_SCR_TAB);
	_add_to_tree(KEY_BTAB, COB_SCR_BACK_TAB);

	_add_to_tree ('\t', COB_SCR_TAB);

#undef _add_to_tree

	return 0;
}


/*
 * Initialize the XAD structures, possibly loading a keymap file
 */
dtree_t
*xad_init (char *keymappath) {
	dtree_t	*tree;
	int	loaded_keymap = 0;
	int	c, i;

	if ((tree = dtree_init ()) == NULL) {
		/* XAD_KEYMAP might be NULL or the current value */
		return XAD_KEYMAP;
	}

	/* CHECKME: maybe optionally? */
	for (c = 32; c < 256; c++) {
		dtree_add_branch (tree, c, c, NULL);
	}

	loaded_keymap = 0;
	if (keymappath != NULL) {
		loaded_keymap = (xad_read_keymap (keymappath, tree) == 0);
	}

	if (!loaded_keymap) {
		/* If no keymapfile was successfully loaded, use default mappings */
		(void) xad_set_default_keymap (tree);
	}

	if (XAD_KEYMAP != NULL) {
		dtree_destroy (XAD_KEYMAP);
		XAD_KEYMAP = NULL;
	}

	XAD_KEYMAP = tree;

// dtree_debug (XAD_KEYMAP);

	for (i = 0; i < XAD_STACK_SIZE; i++) {
		XAD_STACK[i] = -1;
	}

	return XAD_KEYMAP;
}


/*
 * Reset the keymap
 */
int
xad_reset_keymap () {
	if (XAD_KEYMAP == NULL) {
		return 0;
	}

	dtree_destroy (XAD_KEYMAP);
	XAD_KEYMAP = NULL;

	return 0;
}


/*
 * Set keymappings to compiled-in defaults
 */
int
xad_init_default_keymap () {
	(void) xad_init (NULL);
	return 0;
}


/*
 * Read and integrate a keymap-file into the keymap
 */
int
xad_read_keymap_file (char *keymappath) {
	dtree_t	*tree;

	if (keymappath == NULL) {
		return -1;
	}

	if (XAD_KEYMAP == NULL) {
		if (xad_init (keymappath) < 0) {
			return -1;
		}
	} else {
		if (xad_read_keymap (keymappath, XAD_KEYMAP) < 0) {
			return -1;
		}
	}

	return 0;
}


/*
 * Add a single keymapping to the keymap
 */
int
xad_add_keymap (char *pkey, char *pval) {
	if (XAD_KEYMAP == NULL) {
		if (xad_init (NULL) < 0) {
			return -1;
		}
	}

	return xad_add_keymapping (XAD_KEYMAP, pkey, pval);
}


/*
 * Check if XAD keymappings are currently initialized
 */
int
xad_is_initialized () {
	return (XAD_KEYMAP != NULL);
}


/*
 * Place a keystroke (character or function) as the next key to be returned by xad_getch
 */
int
xad_ungetch (int ch) {
	int	i;

	if (ch < 0) {
		return -1;
	}

	if (XAD_STACK[XAD_STACK_SIZE - 1] >= 0) {
		return -1;
	}

	for (i = XAD_STACK_SIZE - 1; i > 0; i--) {
		XAD_STACK[i] = XAD_STACK[i - 1];
	}

	XAD_STACK[0] = ch;

	return ch;
}


/*
 * Append a key (character or function) to the list of keys to be retirned by xad_getch
 */
int
xad_unshift_ch (int ch) {
	int	i;

	if (ch < 0) {
		return -1;
	}

	for (i = 0; i < XAD_STACK_SIZE - 1; i++) {
		if (XAD_STACK[i] < 0) {
			XAD_STACK[i] = ch;
			return ch;
		}
	}

	return -1;
}


/*
 * Return the next keystroke (character or function)
 */
int
xad_getch () {
	static int	istack[50];
	static int	istackp = 0;
	int 	i;
	int 	v;
	int	matched = 0;
	int	maxv = DTREE_NULL;
	int	maxl = 0;
	int	*valdata;
	int 	*valkeys;

	if (XAD_KEYMAP == NULL) {
		return -1;
	}

	while (1) {
		if (XAD_STACK[0] >= 0) {
			v = XAD_STACK[0];

			for (i = 0; i < XAD_STACK_SIZE - 1; i++) {
				XAD_STACK[i] = XAD_STACK[i + 1];
				XAD_STACK[i + 1] = -1;
				if (XAD_STACK[i] < 0) {
					break;
				}
			}

			return v;
		}

		v = dtree_match_list (XAD_KEYMAP, istack, istackp, &matched, &maxv, &maxl, &valdata);

		if (v == DTREE_NULL && istackp > 0) {
			if (matched < istackp) {
				// no complete match: pop the first element
				// and try again
				// v = istack[0];
				if (maxv != DTREE_NULL) {
					v = maxv;
				}

				if (maxl > 0) {
					matched = maxl;
				} else {
					matched = 1;
				}
			} else {
				// incomplete sequence: read next key and
				// try again
				matched = 0;
			}
		}

		if (matched > 0) {
			for (i = 0; i < istackp - matched; i++) {
				istack[i] = istack[i + matched];
			}

			istackp -= matched;
		}

		if (v == DTREE_LIST) {
			int *p;

			v = DTREE_IGNORE;

			if (valdata) {
				for (p = valdata; *p; p++) {
					xad_unshift_ch (*p);
				}
			}
		}

		if (v != DTREE_IGNORE) {
			if (v != DTREE_NULL) {
				return v;
			}

			if ((v = getch ()) != ERR) {
				if (istackp >= 50) {
					return -1;
				}

				istack[istackp++] = v;
			} else {
				break;
			}
		}
	}

	return -1;
}

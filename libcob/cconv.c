/*
   Copyright (C) 2005, 2006, 2013, 2022-2023 Free Software Foundation, Inc.
   Written by Roger While, Nicolas Berthier, Simon Sobisch, David Declerck

   This file is part of GnuCOBOL.

   The GnuCOBOL compiler is free software: you can redistribute it
   and/or modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   GnuCOBOL is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GnuCOBOL.  If not, see <https://www.gnu.org/licenses/>.
*/

#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <ctype.h>

/* include internal and external libcob definitions, forcing exports */
#define	COB_LIB_EXPIMP
#include "sysdefines.h"
#include "coblocal.h"

#ifdef	HAVE_DESIGNATED_INITS
static const unsigned char	lower_tab[256] = {
	['a'] = 'A',
	['b'] = 'B',
	['c'] = 'C',
	['d'] = 'D',
	['e'] = 'E',
	['f'] = 'F',
	['g'] = 'G',
	['h'] = 'H',
	['i'] = 'I',
	['j'] = 'J',
	['k'] = 'K',
	['l'] = 'L',
	['m'] = 'M',
	['n'] = 'N',
	['o'] = 'O',
	['p'] = 'P',
	['q'] = 'Q',
	['r'] = 'R',
	['s'] = 'S',
	['t'] = 'T',
	['u'] = 'U',
	['v'] = 'V',
	['w'] = 'W',
	['x'] = 'X',
	['y'] = 'Y',
	['z'] = 'Z'
};
static const unsigned char	upper_tab[256] = {
	['A'] = 'a',
	['B'] = 'b',
	['C'] = 'c',
	['D'] = 'd',
	['E'] = 'e',
	['F'] = 'f',
	['G'] = 'g',
	['H'] = 'h',
	['I'] = 'i',
	['J'] = 'j',
	['K'] = 'k',
	['L'] = 'l',
	['M'] = 'm',
	['N'] = 'n',
	['O'] = 'o',
	['P'] = 'p',
	['Q'] = 'q',
	['R'] = 'r',
	['S'] = 's',
	['T'] = 't',
	['U'] = 'u',
	['V'] = 'v',
	['W'] = 'w',
	['X'] = 'x',
	['Y'] = 'y',
	['Z'] = 'z'
};
#else
static unsigned char		lower_tab[256];
static unsigned char		upper_tab[256];
static const unsigned char	plower_tab[] = "abcdefghijklmnopqrstuvwxyz";
static const unsigned char	plower_val[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
#endif

static int
cob_convert_hex_digit (char h)
{
	if (h >= '0' && h <= '9') return COB_D2I (h);
	h = cob_toupper (h);
	if (h >= 'A' && h <= 'F') return 10 + h - 'A';
	else return -1;
}

static int
cob_convert_hex_byte (const char *h)
{
	const int d1 = cob_convert_hex_digit (h[0]);
	const int d2 = cob_convert_hex_digit (h[1]);
	if (d1 < 0 || d2 < 0) {
		return -1;
	} else {
		return d1 * 16 + d2;
	}
}

static const char *
cob_skip_blanks (const char *s)
{
	while (isspace((unsigned char)*s)) {
		++s;
	}
	return s;
}

/* Note: cobc uses this function to create tables that are embedded in the
   generated code, and we also make it available in libcob so that external
   tools that make use of the library can use this feature */
int
cob_load_collation (const char *col_name,
		    cob_u8_t *ebcdic_to_ascii,
		    cob_u8_t *ascii_to_ebcdic)
{
	cob_u8_t	table[512];
	char		hex[COB_SMALL_BUFF];
	const char	*hexptr;
	int			line;
	size_t		n, i;
	FILE		*f;
	char		filename[COB_FILE_BUFF];
	const char	*last_err_name = NULL;

	if (col_name[0] == '.' || col_name[0] == SLASH_CHAR
#ifdef _WIN32
	 || (col_name[0] != '\0' && col_name[1] == ':')
#endif
	   ) {
		/* If it's a path, use it as-is, including trailing NUL */
		n = strlen (col_name) + 1;
		if (n >= COB_FILE_MAX) {
			return -1;
		}
		memcpy (filename, col_name, n);
	} else {
		/* Otherwise, prepend the config dir and append the .ttbl suffix */
		const char	*config_dir = getenv ("COB_CONFIG_DIR");
		if (config_dir == NULL) {
			config_dir = COB_CONFIG_DIR;
		}
		n = snprintf (filename, COB_FILE_MAX, "%s%c%s.ttbl", config_dir, SLASH_CHAR, col_name);
		if (n >= COB_FILE_MAX) {
			return -1;
		}
	}

	/* FIXME: use conf_runtime_error / adjusted cob_load_config_file later */
	f = fopen (filename, "r");
	if (f == NULL) {
		cob_runtime_error (_("can't open translation table '%s'"), col_name);
		return -1;
	}

	i = 0;
	line = 0;
	while (fgets (hex, COB_SMALL_BUFF, f) != NULL) {
		++line;
		hexptr = cob_skip_blanks (hex);
		while (*hexptr != '\0' && *hexptr != '#') {
			int c = cob_convert_hex_byte (hexptr);
			if (c < 0) {
				if (col_name != last_err_name) {
					cob_runtime_error (_("errors in translation table '%s':"), col_name);
					last_err_name = col_name;
				}
				cob_runtime_error (_("invalid hex byte on line %d: '%c%c'"), line, hexptr[0], hexptr[1]);
			}
			if (i < 512) {
				table[i++] = (unsigned char)c;
			} else {
				cob_runtime_error (_("too much data in translation table '%s'"), col_name);
				fclose (f);
				return -1;
			}
			hexptr = cob_skip_blanks (hexptr + 2);
		}
	}

	if (ferror (f)) {
		cob_runtime_error (_("error reading translation table '%s'"), col_name);
		fclose (f);
		return -1;
	}
	if (feof (f) && i != 256 && i != 512) {
		if (i < 256) {
			cob_runtime_error (_("not enough data in translation table '%s'"), col_name);
		} else {
			cob_runtime_error (_("either not enough or too much data in translation table '%s'"), col_name);
		}
		fclose (f);
		return -1;
	}

	fclose (f);

	if (ebcdic_to_ascii != NULL) {
		memcpy (ebcdic_to_ascii, table, 256);
	}

	if (ascii_to_ebcdic != NULL) {
		if (i == 512) {
			memcpy (ascii_to_ebcdic, table + 256, 256);
		} else {
			for (i = 0; i < 256; ++i) {
				ascii_to_ebcdic[table[i]] = (cob_u8_t)i;
			}
		}
	}

	if (last_err_name != NULL) {
		return -1;
	}

	return 0;
}

/* Upper-casing for internal words using efficient 7bit C locale table lookup. */
unsigned char
cob_toupper (const unsigned char c)
{
	const unsigned char tab_entry = lower_tab[c];
	if (tab_entry) {
		return tab_entry;
	}
	return c;
}

/* Lower-casing for internal words using efficient 7bit C locale table lookup. */
unsigned char
cob_tolower (const unsigned char c)
{
	const unsigned char tab_entry = upper_tab[c];
	if (tab_entry) {
		return tab_entry;
	}
	return c;
}

/* stores the field's rtrimmed string content into the given buffer
   with maxlength, optionally doing upper-/lowercasing on the fly,
   returns negative values on error, otherwise size of the data
   processed */
int
cob_field_to_string (const cob_field *f, void *str, const size_t maxsize,
	const enum cob_case_modifier target_case)
{
	register unsigned char	*end, *data, *s;

	s = (unsigned char *)str;
	if (f == NULL) {
		snprintf (str, maxsize, "%s", ("NULL field"));
		*(s + maxsize - 1) = 0;
		return -1;
	}

	if (f->size == 0) {
		*s = 0;
		return -2;
	}
	data = f->data;
	/* check if field has data assigned (may be a BASED / LINKAGE item) */
	if (data == NULL) {
		snprintf (str, maxsize, "%s", ("field with NULL address"));
		*(s + maxsize - 1) = 0;
		return -3;
	}
	end = data + f->size - 1;
	while (end > data) {
		if (*end != ' ' && *end) {
			break;
		}
		end--;
	}
	if (*end == ' ' || *end == 0) {
		*s = 0;
		return 0;
	}

	/* note: the specified max does not contain the low-value */
	if ((size_t)(end - data) > maxsize) {
#if 0	/* Simon: it is likely not a good idea to just ignore the data */
		end = data + maxsize;
#else
		*s = 0;
		return -4;
#endif
	}
	switch (target_case) {
	case CCM_NONE:
		while (data <= end) {
			*s++ = *data++;
		}
		break;
	case CCM_LOWER:
		while (data <= end) {
			*s++ = cob_tolower (*data++);
		}
		break;
	case CCM_UPPER:
		while (data <= end) {
			*s++ = cob_toupper (*data++);
		}
		break;
	case CCM_LOWER_LOCALE:
		while (data <= end) {
			*s++ = (unsigned char)tolower (*data++);
		}
		break;
	case CCM_UPPER_LOCALE:
		while (data <= end) {
			*s++ = (unsigned char)toupper (*data++);
		}
		break;
	}
	*s = 0;
	/* note: we limit individual fields to be of size < INT_MAX in the compiler */
	return (int)(end + 1 - f->data);
}


#ifndef	HAVE_DESIGNATED_INITS
/* initialize the 7bit upper/lower table */
static void
init_upper_lower (void)
{
	const unsigned char *p, *v;

	memset (lower_tab, 0, sizeof (lower_tab));
	v = plower_val;
	p = plower_tab;
	for (; *p; ++p, ++v) {
		lower_tab[*p] = *v;
	}
	memset (upper_tab, 0, sizeof (upper_tab));
	p = plower_val;
	v = plower_tab;
	for (; *p; ++p, ++v) {
		upper_tab[*p] = *v;
	}
}
#endif


void
cob_init_cconv (cob_global *lptr)
{
	COB_UNUSED (lptr);
#ifndef	HAVE_DESIGNATED_INITS
	init_upper_lower ();
#endif
}

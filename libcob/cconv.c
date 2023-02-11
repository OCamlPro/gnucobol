/*
   Copyright (C) 2005,2006,2022-2023 Free Software Foundation, Inc.
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

/* Force symbol exports */
#define	COB_LIB_EXPIMP
#include "common.h"
#include "coblocal.h"

static int
cob_convert_hex_digit (char h)
{
	if (h >= '0' && h <= '9') return h - '0';
	else if (h >= 'A' && h <= 'F') return 10 + h - 'A';
	else if (h >= 'a' && h <= 'f') return 10 + h - 'a';
	else return -1;
}

static int
cob_convert_hex_byte (const char *h)
{
	int d1 = cob_convert_hex_digit (h[0]);
	int d2 = cob_convert_hex_digit (h[1]);
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
	const char	*config_dir;
	char		filename[COB_FILE_BUFF];
	const char	*last_err_name = NULL;

	if (col_name[0] == '.' || col_name[0] == SLASH_CHAR
#ifdef _WIN32
	 || col_name[0] != '\0' && col_name[1] == ':'
#endif
	   ) {
		/* If it's a path, use it as-is, including trailing NUL */
		n = strlen (col_name) + 1;
		if (n >= sizeof (filename)) {
			return -1;
		}
		memcpy (filename, col_name, n);
	} else {
		/* Otherwise, prepend the config dir and append the .ttbl suffix */
		config_dir = getenv ("COB_CONFIG_DIR");
		if (config_dir == NULL) {
			config_dir = COB_CONFIG_DIR;
		}
		n = strlen (config_dir) + strlen (col_name) + 7; /* slash + .ttbl + NUL */
		if (n >= sizeof (filename)) {
			return -1;
		}
		sprintf (filename, "%s%c%s.ttbl", config_dir, SLASH_CHAR, col_name);
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
	} else if (feof (f) && i != 256 && i != 512) {
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

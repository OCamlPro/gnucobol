/*
   Copyright (C) 2025 Free Software Foundation, Inc.
   Written by David Declerck, Simon Sobisch.

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
#include "cobc.h"
#include "tree.h" /* for cb_note */

#include <stdio.h>

#ifdef HAVE_ICONV

#include <iconv.h>
#include <limits.h>
#include <errno.h>

#define LINE_SIZE		16
#define EBCDIC_SUBST_CHAR	(char)0x3F
#define ASCII_SUBST_CHAR	(char)0x1A

static void
output_table (FILE *stream, const char *table)
{
	unsigned short i;
	for (i = 0; i <= UCHAR_MAX; ++i) {
		fprintf (stream, "%02X%c", (unsigned char)table[i],
				 ((i + 1) % LINE_SIZE == 0) ? '\n' : ' ');
	}
}

/* Build a pair of EBCDIC/ASCII translation tables using iconv */
int
gentable (FILE *stream, const char *code_ebcdic, const char *code_ascii)
{
	char ebcdic[UCHAR_MAX + 1], ascii[UCHAR_MAX + 1] = { 0 };
	char *ebcdic_ptr = ebcdic, *ascii_ptr = ascii;
	size_t ascii_size = UCHAR_MAX + 1;
	unsigned short i, nb_irreversible = 0;

	iconv_t ic = iconv_open (code_ascii, code_ebcdic);
	if (ic == (iconv_t)-1) {
		cb_error (_("conversion from %s to %s is not supported by your iconv implementation"),
				code_ascii, code_ebcdic);
		return -1;
	}

	for (i = 0; i <= UCHAR_MAX; ++i) {
		ebcdic[i] = (char)i;
	}

	/* Note: POSIX iconv performs an implementation-defined conversion when
	   encountering an untranslatable character (MUSL uses * and *BSD uses ?)
	   and returns the number of such non-reversible conversions performed.
	   GNU iconv instead sets errno to EILSEQ (unless //TRANSLIT is used).
	   To cope with these differences, we convert the table character by
	   character, and when encountering an untranslatable character, we
	   map it to the ASCII substitution character (0x1A). This leaves a
           number of unused ASCII characters: in the reverse translation table,
           those are mapped to the EBCDIC substitution character (0x3F). */

	for (i = 0; i <= UCHAR_MAX; ++i) {
		size_t ebcdic_size = 1;
		size_t res = iconv (ic, &ebcdic_ptr, &ebcdic_size, &ascii_ptr, &ascii_size);

		if (res != 0) {
			if (res == (size_t)-1) {
				if (errno == EILSEQ) {
					/* GNU iconv: an untranslatable character was met */
					*ascii_ptr = ASCII_SUBST_CHAR;
					++ebcdic_ptr; ++ascii_ptr;
					--ascii_size;
					++nb_irreversible;
				} else {
					cb_error (_("an error occurred after converting %ld characters"),
							(ebcdic_ptr - ebcdic));
					iconv_close (ic);
					return -1;
				}
			} else {
				/* POSIX iconv: a substitution was performed */
				*(ascii_ptr - 1) = ASCII_SUBST_CHAR;
				++nb_irreversible;
			}
		}
	}

	iconv_close (ic);

	fprintf (stream, "# GnuCOBOL %s <-> %s translation tables\n", code_ebcdic, code_ascii);

	fprintf (stream, "\n# %s to %s translation table\n\n", code_ebcdic, code_ascii);
	output_table (stream, ascii);

	fprintf (stream, "\n# %s to %s translation table\n\n", code_ascii, code_ebcdic);
	if (nb_irreversible == 0) {
		fprintf (stream, "# This translation being symmetric, the table is built from the previous one.\n\n");
	} else {
		/* Build the (partial) reverse translation table */
		memset (ebcdic, EBCDIC_SUBST_CHAR, UCHAR_MAX + 1);
		for (i = 0; i <= UCHAR_MAX; ++i) {
			ebcdic[(unsigned char)ascii[i]] = (char)i;
		}

		/* Restore the substitution character: as it is used several
                   times, the loop above probably did not set it correctly */
		ebcdic[(unsigned char)ASCII_SUBST_CHAR] = EBCDIC_SUBST_CHAR;

		output_table (stream, ebcdic);
		fprintf (stream, "\n");

		cb_note (COB_WARNOPT_NONE, 0,
			_("%d non-reversible conversions were performed, you might want to review the generated table"),
			nb_irreversible);
	}

	return 0;
}

#else

int
gentable (FILE *stream, const char *code_ebcdic, const char *code_ascii)
{
	COB_UNUSED (stream);
	COB_UNUSED (code_ebcdic);
	COB_UNUSED (code_ascii);

	cb_error (_("runtime is not configured to support %s"), "iconv");
	cb_error (_("translation table generation is not available"));

	return -1;
}

#endif /* HAVE_ICONV */

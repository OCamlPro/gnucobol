/*
   Copyright (C) 2025 Free Software Foundation, Inc.
   Written by David Declerck.

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

#include <stdio.h>

#ifdef HAVE_ICONV

#include <iconv.h>
#include <errno.h>
#include <time.h>

#define TABLE_SIZE		256
#define LINE_SIZE		16
#define EBCDIC_SUBST_CHAR	(char)0x3F

static int
current_year (void)
{
	time_t t = time (NULL);
	struct tm *tm = localtime (&t);
	return tm->tm_year + 1900;
}

static void
output_license (FILE *stream)
{
	fprintf(stream,
"# Copyright (C) %04d Free Software Foundation, Inc.\n"
"# Written by David Declerck.\n"
"#\n"
"# This file is part of the GnuCOBOL runtime.\n"
"#\n"
"# The GnuCOBOL runtime is free software: you can redistribute it\n"
"# and/or modify it under the terms of the GNU Lesser General Public License\n"
"# as published by the Free Software Foundation, either version 3 of the\n"
"# License, or (at your option) any later version.\n"
"#\n"
"# GnuCOBOL is distributed in the hope that it will be useful,\n"
"# but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
"# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
"# GNU General Public License for more details.\n"
"#\n"
"# You should have received a copy of the GNU Lesser General Public License\n"
"# along with GnuCOBOL.  If not, see <https://www.gnu.org/licenses/>.\n",
	current_year ());
}

static void
output_table (FILE *stream, const char *table)
{
	int i;
	for (i = 0; i < TABLE_SIZE; ++i) {
		fprintf (stream, "%02X%c", (unsigned char)table[i],
				 ((i+1) % LINE_SIZE == 0) ? '\n' : ' ');
	}
}

static int
translate_single_char (iconv_t ic, char *ebcdic_char, char *ascii_char)
{
	size_t ebcdic_size = 1, ascii_size = 1;
	size_t res = iconv (ic, &ebcdic_char, &ebcdic_size, &ascii_char, &ascii_size);
	iconv (ic, NULL, NULL, NULL, NULL);
	return (res == (size_t)0 ? 0 : -1);
}

/* We assume fromcode to be an EBCDIC variant and tocode to be an ASCII-based encoding */
int
gentable (FILE *stream, const char *fromcode, const char *tocode)
{
	char ebcdic[TABLE_SIZE], ascii[TABLE_SIZE] = { 0 };
	char *ebcdic_ptr = ebcdic, *ascii_ptr = ascii;
	size_t ebcdic_size = TABLE_SIZE, ascii_size = TABLE_SIZE;
	char ebcdic_subst = EBCDIC_SUBST_CHAR, ascii_subst = 0;
	iconv_t ic;
	size_t res;
	int i, nb_irreversible = 0;

	for (i = 0; i < TABLE_SIZE; ++i) {
		ebcdic[i] = (char)i;
	}

	ic = iconv_open (tocode, fromcode);
	if (ic == (iconv_t)-1) {
		fprintf(stderr, "Conversion from %s to %s is not supported by your iconv implementation.\n", fromcode, tocode);
		return -1;
	}

	/* Note: POSIX iconv performs an implementation-defined conversion when
	   encountering an untranslatable character (MUSL uses * and *BSD uses ?)
	   and returns the number of such non-reversible conversions performed.
	   GNU iconv instead sets errno to EILSEQ (unless //TRANSLIT is used).
	   To cope with these differences, we convert the table character by
	   character and when encountering an untranslatable character, we
	   "translate" it to the substitution character translated from the
	   EBCDIC substitution character (0x3F). */

	if (translate_single_char (ic, &ebcdic_subst, &ascii_subst) != 0) {
		fprintf(stderr, "Can not convert the substitution character from %s to %s.\n", fromcode, tocode);
		return -1;
	}

	for (i = 0; i < TABLE_SIZE; ++i) {
		ebcdic_size = 1;
		res = iconv (ic, &ebcdic_ptr, &ebcdic_size, &ascii_ptr, &ascii_size);
		if (res == (size_t)-1) {
			switch (errno) {
				/* GNU iconv: an untranslatable character was met */
				case EILSEQ:
					*ascii_ptr = ascii_subst;
					++ebcdic_ptr; ++ascii_ptr;
					++ebcdic_size; ++ascii_size;
					++nb_irreversible;
					break;
				case EINVAL:
				case E2BIG:
				default:
					fprintf(stderr, "An error occurred after converting %ld characters.\n", (ebcdic_ptr - ebcdic));
					iconv_close (ic);
					return -1;
			}
		/* POSIX iconv: a substitution was performed */
		} else if (res != (size_t)0) {
			*(ascii_ptr - 1) = ascii_subst;
			nb_irreversible += (int)res;
		}
	}

	iconv_close (ic);

	fprintf(stream, "# GnuCOBOL %s/%s translation tables\n\n", fromcode, tocode);
	output_license (stream);

	fprintf(stream, "\n# %s to %s translation table\n\n", fromcode, tocode);
	output_table (stream, ascii);

	fprintf(stream, "\n# %s to %s translation table\n\n", tocode, fromcode);
	if (nb_irreversible <= 0) {
		fprintf(stream, "# This translation being symmetric, the table is built from the previous one.\n\n");
	} else {
		/* Build the (partial) reverse translation table */
		memset(ebcdic, ebcdic_subst, TABLE_SIZE);
		for (i = 0; i < TABLE_SIZE; ++i) {
			ebcdic[(unsigned char)ascii[i]] = (char)i;
		}

		/* Restore the substitution character, as it is used
                   several times, we can't just reverse the table */
		ebcdic[(unsigned char)ascii_subst] = ebcdic_subst;

		output_table (stream, ebcdic);
		fprintf(stream, "\n");

		fprintf(stderr, "Warning: %d non-reversible conversions were performed, you might want to review the generated table.\n", nb_irreversible);
	}

	return 0;
}

#else

int
gentable (FILE *stream, const char *fromcode, const char *tocode)
{
	COB_UNUSED (stream);
	COB_UNUSED (fromcode);
	COB_UNUSED (tocode);

	fprintf(stderr, "Error: GnuCOBOL was not compiled with iconv support; translation table generation is not available.\n");

	return -1;
}

#endif /* HAVE_ICONV */

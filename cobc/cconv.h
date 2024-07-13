/*
   Copyright (C) 2005,2006,2022 Free Software Foundation, Inc.
   Written by Roger While, Nicolas Berthier, Simon Sobisch

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
#ifndef CB_CCONV_H
#define CB_CCONV_H

/* FIXME: inclusion of unistd.h is required for size_t.  As in cobc.h, this may
   require an additional installed header. */
#include "config.h"
#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#endif
#include "../libcob/common.h"

/* "default" (likely MF) EBCDIC to ASCII conversion table */
extern const cob_u8_t cob_ebcdic_ascii[256];

/* ASCII to "default" (likely MF) EBCDIC conversion table */
extern const cob_u8_t cob_ascii_ebcdic[256];

/* EBCDIC GCOS7 8-bit to ASCII conversion table:

   https://support.bull.com/ols/product/system/gcos7/gcos7-com/g7-dps7000/doc-com/docf/g/47X257TN27-oct2009/47A205UL04.pdf,
   p627.  Note one page is missing from this documentation, but the full table
   can be found in the French version. */
extern const cob_u8_t cob_gcos7ebcdic_ascii[256];

/* EBCDIC GCOS7 8-bit to "default" EBCDIC conversion table */
extern const cob_u8_t cob_gcos7ebcdic_ebcdic[256];

/* ASCII (8-bit) to EBCDIC GCOS7 conversion table */
extern const cob_u8_t cob_ascii_gcos7ebcdic[256];

/* Restricted conversions: */

/* ASCII to EBCDIC conversion table (restricted) */
extern const cob_u8_t cob_ascii_alt_ebcdic[256];

/* IBM EBCDIC to ASCII conversion table (restricted)

   cf https://www.ibm.com/docs/en/iis/11.3?topic=tables-ebcdic-ascii */
extern const cob_u8_t cob_ibmebcdic_ascii[256];

/* ASCII to IBM EBCDIC conversion table (restricted)

   cf https://www.ibm.com/docs/en/iis/11.3?topic=tables-ascii-ebcdic */
extern const cob_u8_t cob_ascii_ibmebcdic[256];

/* All supported conversions */
enum ebcdic_table {
	CB_EBCDIC_DEFAULT,
	CB_EBCDIC_RESTRICTED_GC,
	CB_EBCDIC_IBM,
	CB_EBCDIC_GCOS,
};

extern enum ebcdic_table cb_ebcdic_table;

int cobc_deciph_ebcdic_table_name (const char *const);

#endif /* CB_CCONV_H */

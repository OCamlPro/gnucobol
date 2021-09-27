/*
   Copyright (C) 2001-2012, 2014-2021 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Simon Sobisch, Edward Hart

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


#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <ctype.h>

#include "cobc.h"
#include "tree.h"

#define _PARSER_H	/* work around bad Windows SDK header */
#include "parser.h"

/* Local variables */

struct system_name_struct {
	const char				*name;
	const enum cb_system_name_category	category;
	const int				token;
	enum cb_feature_mode	active;
};

/* TODO: allow these to not only be enabled/disabled but defined by compiler configuration
         removing duplicates from this list (especially concerning the switches) */
static struct system_name_struct	system_name_table[] = {
	{"SYSIN",		CB_DEVICE_NAME,  CB_DEVICE_SYSIN, CB_FEATURE_ACTIVE},
	{"SYSIPT",		CB_DEVICE_NAME,  CB_DEVICE_SYSIN, CB_FEATURE_ACTIVE},
	{"STDIN",		CB_DEVICE_NAME,  CB_DEVICE_SYSIN, CB_FEATURE_ACTIVE},
	{"SYSOUT",		CB_DEVICE_NAME,  CB_DEVICE_SYSOUT, CB_FEATURE_ACTIVE},
	{"SYSLIST",		CB_DEVICE_NAME,  CB_DEVICE_SYSOUT, CB_FEATURE_ACTIVE},
	{"SYSLST",		CB_DEVICE_NAME,  CB_DEVICE_SYSOUT, CB_FEATURE_ACTIVE},
	{"SYSPCH",		CB_DEVICE_NAME,  CB_DEVICE_SYSPCH, CB_FEATURE_ACTIVE},
	{"SYSPUNCH",		CB_DEVICE_NAME,  CB_DEVICE_SYSPCH, CB_FEATURE_ACTIVE},
	{"STDOUT",		CB_DEVICE_NAME,  CB_DEVICE_SYSOUT, CB_FEATURE_ACTIVE},
	{"PRINT",		CB_DEVICE_NAME,  CB_DEVICE_SYSOUT, CB_FEATURE_ACTIVE},
	{"PRINTER",		CB_DEVICE_NAME,  CB_DEVICE_PRINTER, CB_FEATURE_ACTIVE},
	{"PRINTER-1",		CB_DEVICE_NAME,  CB_DEVICE_PRINTER, CB_FEATURE_ACTIVE},
	{"SYSERR",		CB_DEVICE_NAME,  CB_DEVICE_SYSERR, CB_FEATURE_ACTIVE},
	{"STDERR",		CB_DEVICE_NAME,  CB_DEVICE_SYSERR, CB_FEATURE_ACTIVE},
	{"CONSOLE",		CB_DEVICE_NAME,  CB_DEVICE_CONSOLE, CB_FEATURE_ACTIVE},
	{"C01",			CB_FEATURE_NAME, CB_FEATURE_C01, CB_FEATURE_ACTIVE},
	{"C02",			CB_FEATURE_NAME, CB_FEATURE_C02, CB_FEATURE_ACTIVE},
	{"C03",			CB_FEATURE_NAME, CB_FEATURE_C03, CB_FEATURE_ACTIVE},
	{"C04",			CB_FEATURE_NAME, CB_FEATURE_C04, CB_FEATURE_ACTIVE},
	{"C05",			CB_FEATURE_NAME, CB_FEATURE_C05, CB_FEATURE_ACTIVE},
	{"C06",			CB_FEATURE_NAME, CB_FEATURE_C06, CB_FEATURE_ACTIVE},
	{"C07",			CB_FEATURE_NAME, CB_FEATURE_C07, CB_FEATURE_ACTIVE},
	{"C08",			CB_FEATURE_NAME, CB_FEATURE_C08, CB_FEATURE_ACTIVE},
	{"C09",			CB_FEATURE_NAME, CB_FEATURE_C09, CB_FEATURE_ACTIVE},
	{"C10",			CB_FEATURE_NAME, CB_FEATURE_C10, CB_FEATURE_ACTIVE},
	{"C11",			CB_FEATURE_NAME, CB_FEATURE_C11, CB_FEATURE_ACTIVE},
	{"C12",			CB_FEATURE_NAME, CB_FEATURE_C12, CB_FEATURE_ACTIVE},
	{"S01",			CB_FEATURE_NAME, CB_FEATURE_FORMFEED, CB_FEATURE_ACTIVE},
	{"S02",			CB_FEATURE_NAME, CB_FEATURE_FORMFEED, CB_FEATURE_ACTIVE},
	{"S03",			CB_FEATURE_NAME, CB_FEATURE_FORMFEED, CB_FEATURE_ACTIVE},
	{"S04",			CB_FEATURE_NAME, CB_FEATURE_FORMFEED, CB_FEATURE_ACTIVE},
	{"S05",			CB_FEATURE_NAME, CB_FEATURE_FORMFEED, CB_FEATURE_ACTIVE},
	/*{"AFP-5A ",			CB_FEATURE_NAME, CB_FEATURE_AFP_5A , CB_FEATURE_ACTIVE},*/
	{"CSP",			CB_FEATURE_NAME, CB_FEATURE_FORMFEED, CB_FEATURE_ACTIVE},
	{"FORMFEED",		CB_FEATURE_NAME, CB_FEATURE_FORMFEED, CB_FEATURE_ACTIVE},
	{"TOP",		CB_FEATURE_NAME, CB_FEATURE_FORMFEED, CB_FEATURE_ACTIVE},
	{"CALL-CONVENTION",	CB_FEATURE_NAME, CB_FEATURE_CONVENTION, CB_FEATURE_ACTIVE},
	{"SWITCH-0",		CB_SWITCH_NAME,  CB_SWITCH_0, CB_FEATURE_ACTIVE},
	{"SWITCH-1",		CB_SWITCH_NAME,  CB_SWITCH_1, CB_FEATURE_ACTIVE},
	{"SWITCH-2",		CB_SWITCH_NAME,  CB_SWITCH_2, CB_FEATURE_ACTIVE},
	{"SWITCH-3",		CB_SWITCH_NAME,  CB_SWITCH_3, CB_FEATURE_ACTIVE},
	{"SWITCH-4",		CB_SWITCH_NAME,  CB_SWITCH_4, CB_FEATURE_ACTIVE},
	{"SWITCH-5",		CB_SWITCH_NAME,  CB_SWITCH_5, CB_FEATURE_ACTIVE},
	{"SWITCH-6",		CB_SWITCH_NAME,  CB_SWITCH_6, CB_FEATURE_ACTIVE},
	{"SWITCH-7",		CB_SWITCH_NAME,  CB_SWITCH_7, CB_FEATURE_ACTIVE},
	{"SWITCH-8",		CB_SWITCH_NAME,  CB_SWITCH_8, CB_FEATURE_ACTIVE},
	{"SWITCH-9",		CB_SWITCH_NAME,  CB_SWITCH_9, CB_FEATURE_ACTIVE},
	{"SWITCH-10",		CB_SWITCH_NAME,  CB_SWITCH_10, CB_FEATURE_ACTIVE},
	{"SWITCH-11",		CB_SWITCH_NAME,  CB_SWITCH_11, CB_FEATURE_ACTIVE},
	{"SWITCH-12",		CB_SWITCH_NAME,  CB_SWITCH_12, CB_FEATURE_ACTIVE},
	{"SWITCH-13",		CB_SWITCH_NAME,  CB_SWITCH_13, CB_FEATURE_ACTIVE},
	{"SWITCH-14",		CB_SWITCH_NAME,  CB_SWITCH_14, CB_FEATURE_ACTIVE},
	{"SWITCH-15",		CB_SWITCH_NAME,  CB_SWITCH_15, CB_FEATURE_ACTIVE},
	{"SWITCH-16",		CB_SWITCH_NAME,  CB_SWITCH_16, CB_FEATURE_ACTIVE},
	{"SWITCH-17",		CB_SWITCH_NAME,  CB_SWITCH_17, CB_FEATURE_ACTIVE},
	{"SWITCH-18",		CB_SWITCH_NAME,  CB_SWITCH_18, CB_FEATURE_ACTIVE},
	{"SWITCH-19",		CB_SWITCH_NAME,  CB_SWITCH_19, CB_FEATURE_ACTIVE},
	{"SWITCH-20",		CB_SWITCH_NAME,  CB_SWITCH_20, CB_FEATURE_ACTIVE},
	{"SWITCH-21",		CB_SWITCH_NAME,  CB_SWITCH_21, CB_FEATURE_ACTIVE},
	{"SWITCH-22",		CB_SWITCH_NAME,  CB_SWITCH_22, CB_FEATURE_ACTIVE},
	{"SWITCH-23",		CB_SWITCH_NAME,  CB_SWITCH_23, CB_FEATURE_ACTIVE},
	{"SWITCH-24",		CB_SWITCH_NAME,  CB_SWITCH_24, CB_FEATURE_ACTIVE},
	{"SWITCH-25",		CB_SWITCH_NAME,  CB_SWITCH_25, CB_FEATURE_ACTIVE},
	{"SWITCH-26",		CB_SWITCH_NAME,  CB_SWITCH_26, CB_FEATURE_ACTIVE},
	{"SWITCH-27",		CB_SWITCH_NAME,  CB_SWITCH_27, CB_FEATURE_ACTIVE},
	{"SWITCH-28",		CB_SWITCH_NAME,  CB_SWITCH_28, CB_FEATURE_ACTIVE},
	{"SWITCH-29",		CB_SWITCH_NAME,  CB_SWITCH_29, CB_FEATURE_ACTIVE},
	{"SWITCH-30",		CB_SWITCH_NAME,  CB_SWITCH_30, CB_FEATURE_ACTIVE},
	{"SWITCH-31",		CB_SWITCH_NAME,  CB_SWITCH_31, CB_FEATURE_ACTIVE},
	{"SWITCH-32",		CB_SWITCH_NAME,  CB_SWITCH_32, CB_FEATURE_ACTIVE},
	{"SWITCH-33",		CB_SWITCH_NAME,  CB_SWITCH_33, CB_FEATURE_ACTIVE},
	{"SWITCH-34",		CB_SWITCH_NAME,  CB_SWITCH_34, CB_FEATURE_ACTIVE},
	{"SWITCH-35",		CB_SWITCH_NAME,  CB_SWITCH_35, CB_FEATURE_ACTIVE},
	{"SWITCH-36",		CB_SWITCH_NAME,  CB_SWITCH_36, CB_FEATURE_ACTIVE},
	{"SW0",			CB_SWITCH_NAME,  CB_SWITCH_0, CB_FEATURE_DISABLED},
	{"SW1",			CB_SWITCH_NAME,  CB_SWITCH_1, CB_FEATURE_DISABLED},
	{"SW2",			CB_SWITCH_NAME,  CB_SWITCH_2, CB_FEATURE_DISABLED},
	{"SW3",			CB_SWITCH_NAME,  CB_SWITCH_3, CB_FEATURE_DISABLED},
	{"SW4",			CB_SWITCH_NAME,  CB_SWITCH_4, CB_FEATURE_DISABLED},
	{"SW5",			CB_SWITCH_NAME,  CB_SWITCH_5, CB_FEATURE_DISABLED},
	{"SW6",			CB_SWITCH_NAME,  CB_SWITCH_6, CB_FEATURE_DISABLED},
	{"SW7",			CB_SWITCH_NAME,  CB_SWITCH_7, CB_FEATURE_DISABLED},
	{"SW8",			CB_SWITCH_NAME,  CB_SWITCH_8, CB_FEATURE_DISABLED},
	{"SW9",			CB_SWITCH_NAME,  CB_SWITCH_9, CB_FEATURE_DISABLED},
	{"SW10",		CB_SWITCH_NAME,  CB_SWITCH_10, CB_FEATURE_DISABLED},
	{"SW11",		CB_SWITCH_NAME,  CB_SWITCH_11, CB_FEATURE_DISABLED},
	{"SW12",		CB_SWITCH_NAME,  CB_SWITCH_12, CB_FEATURE_DISABLED},
	{"SW13",		CB_SWITCH_NAME,  CB_SWITCH_13, CB_FEATURE_DISABLED},
	{"SW14",		CB_SWITCH_NAME,  CB_SWITCH_14, CB_FEATURE_DISABLED},
	{"SW15",		CB_SWITCH_NAME,  CB_SWITCH_15, CB_FEATURE_DISABLED},
	{"SWITCH 0",		CB_SWITCH_NAME,  CB_SWITCH_0, CB_FEATURE_DISABLED},
	{"SWITCH 1",		CB_SWITCH_NAME,  CB_SWITCH_1, CB_FEATURE_DISABLED},
	{"SWITCH 2",		CB_SWITCH_NAME,  CB_SWITCH_2, CB_FEATURE_DISABLED},
	{"SWITCH 3",		CB_SWITCH_NAME,  CB_SWITCH_3, CB_FEATURE_DISABLED},
	{"SWITCH 4",		CB_SWITCH_NAME,  CB_SWITCH_4, CB_FEATURE_DISABLED},
	{"SWITCH 5",		CB_SWITCH_NAME,  CB_SWITCH_5, CB_FEATURE_DISABLED},
	{"SWITCH 6",		CB_SWITCH_NAME,  CB_SWITCH_6, CB_FEATURE_DISABLED},
	{"SWITCH 7",		CB_SWITCH_NAME,  CB_SWITCH_7, CB_FEATURE_DISABLED},
	{"SWITCH 8",		CB_SWITCH_NAME,  CB_SWITCH_8, CB_FEATURE_DISABLED},
	{"SWITCH 9",		CB_SWITCH_NAME,  CB_SWITCH_9, CB_FEATURE_DISABLED},
	{"SWITCH 10",		CB_SWITCH_NAME,  CB_SWITCH_10, CB_FEATURE_DISABLED},
	{"SWITCH 11",		CB_SWITCH_NAME,  CB_SWITCH_11, CB_FEATURE_DISABLED},
	{"SWITCH 12",		CB_SWITCH_NAME,  CB_SWITCH_12, CB_FEATURE_DISABLED},
	{"SWITCH 13",		CB_SWITCH_NAME,  CB_SWITCH_13, CB_FEATURE_DISABLED},
	{"SWITCH 14",		CB_SWITCH_NAME,  CB_SWITCH_14, CB_FEATURE_DISABLED},
	{"SWITCH 15",		CB_SWITCH_NAME,  CB_SWITCH_15, CB_FEATURE_DISABLED},
	{"SWITCH 16",		CB_SWITCH_NAME,  CB_SWITCH_16, CB_FEATURE_DISABLED},
	{"SWITCH 17",		CB_SWITCH_NAME,  CB_SWITCH_17, CB_FEATURE_DISABLED},
	{"SWITCH 18",		CB_SWITCH_NAME,  CB_SWITCH_18, CB_FEATURE_DISABLED},
	{"SWITCH 19",		CB_SWITCH_NAME,  CB_SWITCH_19, CB_FEATURE_DISABLED},
	{"SWITCH 20",		CB_SWITCH_NAME,  CB_SWITCH_20, CB_FEATURE_DISABLED},
	{"SWITCH 21",		CB_SWITCH_NAME,  CB_SWITCH_21, CB_FEATURE_DISABLED},
	{"SWITCH 22",		CB_SWITCH_NAME,  CB_SWITCH_22, CB_FEATURE_DISABLED},
	{"SWITCH 23",		CB_SWITCH_NAME,  CB_SWITCH_23, CB_FEATURE_DISABLED},
	{"SWITCH 24",		CB_SWITCH_NAME,  CB_SWITCH_24, CB_FEATURE_DISABLED},
	{"SWITCH 25",		CB_SWITCH_NAME,  CB_SWITCH_25, CB_FEATURE_DISABLED},
	{"SWITCH 26",		CB_SWITCH_NAME,  CB_SWITCH_26, CB_FEATURE_DISABLED},
	{"SWITCH A",		CB_SWITCH_NAME,  CB_SWITCH_1, CB_FEATURE_DISABLED},
	{"SWITCH B",		CB_SWITCH_NAME,  CB_SWITCH_2, CB_FEATURE_DISABLED},
	{"SWITCH C",		CB_SWITCH_NAME,  CB_SWITCH_3, CB_FEATURE_DISABLED},
	{"SWITCH D",		CB_SWITCH_NAME,  CB_SWITCH_4, CB_FEATURE_DISABLED},
	{"SWITCH E",		CB_SWITCH_NAME,  CB_SWITCH_5, CB_FEATURE_DISABLED},
	{"SWITCH F",		CB_SWITCH_NAME,  CB_SWITCH_6, CB_FEATURE_DISABLED},
	{"SWITCH G",		CB_SWITCH_NAME,  CB_SWITCH_7, CB_FEATURE_DISABLED},
	{"SWITCH H",		CB_SWITCH_NAME,  CB_SWITCH_8, CB_FEATURE_DISABLED},
	{"SWITCH I",		CB_SWITCH_NAME,  CB_SWITCH_9, CB_FEATURE_DISABLED},
	{"SWITCH J",		CB_SWITCH_NAME,  CB_SWITCH_10, CB_FEATURE_DISABLED},
	{"SWITCH K",		CB_SWITCH_NAME,  CB_SWITCH_11, CB_FEATURE_DISABLED},
	{"SWITCH L",		CB_SWITCH_NAME,  CB_SWITCH_12, CB_FEATURE_DISABLED},
	{"SWITCH M",		CB_SWITCH_NAME,  CB_SWITCH_13, CB_FEATURE_DISABLED},
	{"SWITCH N",		CB_SWITCH_NAME,  CB_SWITCH_14, CB_FEATURE_DISABLED},
	{"SWITCH O",		CB_SWITCH_NAME,  CB_SWITCH_15, CB_FEATURE_DISABLED},
	{"SWITCH P",		CB_SWITCH_NAME,  CB_SWITCH_16, CB_FEATURE_DISABLED},
	{"SWITCH Q",		CB_SWITCH_NAME,  CB_SWITCH_17, CB_FEATURE_DISABLED},
	{"SWITCH R",		CB_SWITCH_NAME,  CB_SWITCH_18, CB_FEATURE_DISABLED},
	{"SWITCH S",		CB_SWITCH_NAME,  CB_SWITCH_19, CB_FEATURE_DISABLED},
	{"SWITCH T",		CB_SWITCH_NAME,  CB_SWITCH_20, CB_FEATURE_DISABLED},
	{"SWITCH U",		CB_SWITCH_NAME,  CB_SWITCH_21, CB_FEATURE_DISABLED},
	{"SWITCH V",		CB_SWITCH_NAME,  CB_SWITCH_22, CB_FEATURE_DISABLED},
	{"SWITCH W",		CB_SWITCH_NAME,  CB_SWITCH_23, CB_FEATURE_DISABLED},
	{"SWITCH X",		CB_SWITCH_NAME,  CB_SWITCH_24, CB_FEATURE_DISABLED},
	{"SWITCH Y",		CB_SWITCH_NAME,  CB_SWITCH_25, CB_FEATURE_DISABLED},
	{"SWITCH Z",		CB_SWITCH_NAME,  CB_SWITCH_26, CB_FEATURE_DISABLED},
	{"UPSI-0",		CB_SWITCH_NAME,  CB_SWITCH_0, CB_FEATURE_DISABLED},
	{"UPSI-1",		CB_SWITCH_NAME,  CB_SWITCH_1, CB_FEATURE_DISABLED},
	{"UPSI-2",		CB_SWITCH_NAME,  CB_SWITCH_2, CB_FEATURE_DISABLED},
	{"UPSI-3",		CB_SWITCH_NAME,  CB_SWITCH_3, CB_FEATURE_DISABLED},
	{"UPSI-4",		CB_SWITCH_NAME,  CB_SWITCH_4, CB_FEATURE_DISABLED},
	{"UPSI-5",		CB_SWITCH_NAME,  CB_SWITCH_5, CB_FEATURE_DISABLED},
	{"UPSI-6",		CB_SWITCH_NAME,  CB_SWITCH_6, CB_FEATURE_DISABLED},
	{"UPSI-7",		CB_SWITCH_NAME,  CB_SWITCH_7, CB_FEATURE_DISABLED},
	{"UPSI-8",		CB_SWITCH_NAME,  CB_SWITCH_8, CB_FEATURE_DISABLED},
	/* TO-DO: Figure out how TSW switches differ from USW switches and add them. */
	{"USW-0",		CB_SWITCH_NAME,  CB_SWITCH_0, CB_FEATURE_DISABLED},
	{"USW-1",		CB_SWITCH_NAME,  CB_SWITCH_1, CB_FEATURE_DISABLED},
	{"USW-2",		CB_SWITCH_NAME,  CB_SWITCH_2, CB_FEATURE_DISABLED},
	{"USW-3",		CB_SWITCH_NAME,  CB_SWITCH_3, CB_FEATURE_DISABLED},
	{"USW-4",		CB_SWITCH_NAME,  CB_SWITCH_4, CB_FEATURE_DISABLED},
	{"USW-5",		CB_SWITCH_NAME,  CB_SWITCH_5, CB_FEATURE_DISABLED},
	{"USW-6",		CB_SWITCH_NAME,  CB_SWITCH_6, CB_FEATURE_DISABLED},
	{"USW-7",		CB_SWITCH_NAME,  CB_SWITCH_7, CB_FEATURE_DISABLED},
	{"USW-8",		CB_SWITCH_NAME,  CB_SWITCH_8, CB_FEATURE_DISABLED},
	{"USW-9",		CB_SWITCH_NAME,  CB_SWITCH_9, CB_FEATURE_DISABLED},
	{"USW-10",		CB_SWITCH_NAME,  CB_SWITCH_10, CB_FEATURE_DISABLED},
	{"USW-11",		CB_SWITCH_NAME,  CB_SWITCH_11, CB_FEATURE_DISABLED},
	{"USW-12",		CB_SWITCH_NAME,  CB_SWITCH_12, CB_FEATURE_DISABLED},
	{"USW-13",		CB_SWITCH_NAME,  CB_SWITCH_13, CB_FEATURE_DISABLED},
	{"USW-14",		CB_SWITCH_NAME,  CB_SWITCH_14, CB_FEATURE_DISABLED},
	{"USW-15",		CB_SWITCH_NAME,  CB_SWITCH_15, CB_FEATURE_DISABLED},
	{"USW-16",		CB_SWITCH_NAME,  CB_SWITCH_16, CB_FEATURE_DISABLED},
	{"USW-17",		CB_SWITCH_NAME,  CB_SWITCH_17, CB_FEATURE_DISABLED},
	{"USW-18",		CB_SWITCH_NAME,  CB_SWITCH_18, CB_FEATURE_DISABLED},
	{"USW-19",		CB_SWITCH_NAME,  CB_SWITCH_19, CB_FEATURE_DISABLED},
	{"USW-20",		CB_SWITCH_NAME,  CB_SWITCH_20, CB_FEATURE_DISABLED},
	{"USW-21",		CB_SWITCH_NAME,  CB_SWITCH_21, CB_FEATURE_DISABLED},
	{"USW-22",		CB_SWITCH_NAME,  CB_SWITCH_22, CB_FEATURE_DISABLED},
	{"USW-23",		CB_SWITCH_NAME,  CB_SWITCH_23, CB_FEATURE_DISABLED},
	{"USW-24",		CB_SWITCH_NAME,  CB_SWITCH_24, CB_FEATURE_DISABLED},
	{"USW-25",		CB_SWITCH_NAME,  CB_SWITCH_25, CB_FEATURE_DISABLED},
	{"USW-26",		CB_SWITCH_NAME,  CB_SWITCH_26, CB_FEATURE_DISABLED},
	{"USW-27",		CB_SWITCH_NAME,  CB_SWITCH_27, CB_FEATURE_DISABLED},
	{"USW-28",		CB_SWITCH_NAME,  CB_SWITCH_28, CB_FEATURE_DISABLED},
	{"USW-29",		CB_SWITCH_NAME,  CB_SWITCH_29, CB_FEATURE_DISABLED},
	{"USW-30",		CB_SWITCH_NAME,  CB_SWITCH_30, CB_FEATURE_DISABLED},
	{"USW-31",		CB_SWITCH_NAME,  CB_SWITCH_31, CB_FEATURE_DISABLED}
};

#define	SYSTEM_TAB_SIZE	sizeof(system_name_table) / sizeof(struct system_name_struct)

static struct system_name_struct *lookup_system_name (const char *, const int);

/* Reserved word table, note: this list is sorted on startup in
   (initialize_reserved_words_if_needed), no need to care for EBCDIC */
/* Description */

/* Word # Statement has terminator # Is context sensitive (only for printing)
        # Token # Special context set # Special context test */

static struct cobc_reserved default_reserved_words[] = {
  { "3-D",			0, 1, THREEDIMENSIONAL,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "ABSENT",			0, 0, ABSENT,			/* IBM RW */
				0, 0
  },
  { "ACCEPT",			1, 0, ACCEPT,			/* 2002 */
				CB_CS_ACCEPT, 0
  },
  { "ACCESS",			0, 0, ACCESS,			/* 2002 */
				0, 0
  },
  { "ACTION",		0, 1, ACTION,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "ACTIVE-CLASS",		0, 0, -1,			/* 2002 */
				0, 0
  },
  { "ACTIVE-X",		1, 1, ACTIVEX,		/* ACU extension, very unlikely to be implemented */
				CB_CS_GRAPHICAL_CONTROL, CB_CS_DISPLAY | CB_CS_SCREEN
  },
  { "ACTUAL",			0, 1, ACTUAL,			/* OS/VS extension */
    				0, CB_CS_SELECT
  },
  { "ADD",			1, 0, ADD,			/* 2002 */
				0, 0
  },
  { "ADDRESS",			0, 0, ADDRESS,			/* 2002 */
				0, 0
  },
  { "ADJUSTABLE-COLUMNS",		0, 1, ADJUSTABLE_COLUMNS,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "ADVANCING",		0, 0, ADVANCING,		/* 2002 */
				0, 0
  },
  { "AFTER",			0, 0, AFTER,			/* 2002 */
				0, 0
  },
  { "ALIGNMENT",		0, 1, ALIGNMENT,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "ALIGNED",			0, 0, -1,			/* 2002 */
				0, 0
  },
  { "ALL",			0, 0, ALL,			/* 2002 */
				0, 0
  },
  { "ALLOCATE",			0, 0, ALLOCATE,			/* 2002 */
				CB_CS_ALLOCATE, 0
  },
  { "ALLOWING",		0, 1, ALLOWING,		/* ACU extension */
				0, CB_CS_OPEN
  },
  { "ALPHABET",			1, 0, ALPHABET,			/* 2002 */
				CB_CS_ALPHABET, 0
  },
  { "ALPHABETIC",		0, 0, ALPHABETIC,		/* 2002 */
				0, 0
  },
  { "ALPHABETIC-LOWER",		0, 0, ALPHABETIC_LOWER,		/* 2002 */
				0, 0
  },
  { "ALPHABETIC-UPPER",		0, 0, ALPHABETIC_UPPER,		/* 2002 */
				0, 0
  },
  { "ALPHANUMERIC",		0, 0, ALPHANUMERIC,		/* 2002 */
				0, 0
  },
  { "ALPHANUMERIC-EDITED",	0, 0, ALPHANUMERIC_EDITED,	/* 2002 */
				0, 0
  },
  { "ALSO",			0, 0, ALSO,			/* 2002 */
				0, 0
  },
  { "ALTER",			0, 0, ALTER,			/* 85 */
				0, 0
  },
  { "ALTERNATE",		0, 0, ALTERNATE,		/* 2002 */
				0, 0
  },
  { "AND",			0, 0, AND,			/* 2002 */
				0, 0
  },
  { "ANY",			0, 0, ANY,			/* 2002 */
				0, 0
  },
  { "ANYCASE",			0, 0, -1,			/* 2002 */
				0, 0
  },
  { "APPLY",			0, 1, APPLY,			/* 202x pending (C/S) */
				0, CB_CS_I_O_CONTROL
  },
  { "ARE",			0, 0, ARE,			/* 2002 */
				0, 0
  },
  { "AREA",			0, 0, AREA,			/* 2002 */
				0, 0
  },
  { "AREAS",			0, 0, AREA,			/* 2002 */
				0, 0
  },
  { "ARGUMENT-NUMBER",		0, 0, ARGUMENT_NUMBER,		/* Extension */
				0, 0
  },
  { "ARGUMENT-VALUE",		0, 0, ARGUMENT_VALUE,		/* Extension */
				0, 0
  },
  { "ARITHMETIC",		0, 1, ARITHMETIC,			/* 2002 (C/S) */
				0, CB_CS_OPTIONS
  },
  { "AS",			0, 0, AS,			/* 2002 */
				0, 0
  },
  { "ASCENDING",		0, 0, ASCENDING,		/* 2002 */
				0, 0
  },
  { "ASCII",			0, 1, ASCII,			/* Extension */
				0, CB_CS_ALPHABET
  },
  { "ASSIGN",			1, 0, ASSIGN,			/* 2002 */
				CB_CS_ASSIGN, 0
  },
  { "AT",			0, 0, AT,			/* 2002 */
				0, 0
  },
  { "ATTRIBUTE",		0, 1, ATTRIBUTE,		/* 2002 (C/S) */
				0, CB_CS_SET | CB_CS_XML_GENERATE
  },
  { "ATTRIBUTES",		0, 1, ATTRIBUTES,		/* IBM extension */
				0, CB_CS_XML_GENERATE
  },
  { "AUTHOR",			0, 1, AUTHOR,			/* 85 (later: C/S) */
				0, CB_CS_DAY /* HACK, we only want it to normally be not usable */
  },
  { "AUTO",			0, 1, AUTO,			/* 2002 (C/S), extension */
				0, CB_CS_ACCEPT | CB_CS_SCREEN | CB_CS_CALL
  },
  { "AUTO-DECIMAL",		0, 1, AUTO_DECIMAL,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "AUTO-SPIN",		0, 1, AUTO_SPIN,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "AUTOMATIC",		0, 0, AUTOMATIC,		/* 2002 */
				0, 0
	/* FIXME: 2014 Context-sensitive to LOCK MODE clause */
  },
  { "AWAY-FROM-ZERO",		0, 1, AWAY_FROM_ZERO,		/* 2014 (C/S) */
				0, CB_CS_ROUNDED
  },
  { "B-AND",			0, 0, -1,			/* 2002 */
				0, 0
  },
  { "B-NOT",			0, 0, -1,			/* 2002 */
				0, 0
  },
  { "B-OR",			0, 0, -1,			/* 2002 */
				0, 0
  },
  { "B-XOR",			0, 0, -1,			/* 2002 */
				0, 0
  },
  { "BACKGROUND-COLOR",		0, 1, BACKGROUND_COLOR,		/* 2002 (C/S) */
				0, CB_CS_ACCEPT | CB_CS_DISPLAY | CB_CS_SCREEN
  },
  { "BACKGROUND-HIGH",		0, 0, BACKGROUND_HIGH,		/* ACU extension */
				0, 0
  },
  { "BACKGROUND-LOW",		0, 0, BACKGROUND_LOW,		/* ACU extension */
				0, 0
  },
  { "BACKGROUND-STANDARD",		0, 0, BACKGROUND_STANDARD,		/* ACU extension */
				0, 0
  },
  { "BAR",			1, 1, BAR,			/* ACU extension */
				CB_CS_GRAPHICAL_CONTROL, CB_CS_DISPLAY | CB_CS_SCREEN
  },
  { "BASED",			0, 0, BASED,			/* 2002 */
				0, 0
  },
  { "BEFORE",			0, 0, BEFORE,			/* 2002 */
				0, 0
  },
  { "BELL",			0, 1, BELL,			/* 2002 (C/S) */
				0, CB_CS_ACCEPT | CB_CS_DISPLAY | CB_CS_SCREEN | CB_CS_SET
  },
  { "BINARY",			0, 0, BINARY,			/* 2002 */
				0, 0
  },
  { "BINARY-C-LONG",		0, 0, BINARY_C_LONG,		/* Extension */
				0, 0
  },
  { "BINARY-CHAR",		0, 0, BINARY_CHAR,		/* 2002 */
				0, 0
  },
  { "BINARY-DOUBLE",		0, 0, BINARY_DOUBLE,		/* 2002 */
				0, 0
  },
  { "BINARY-LONG",		0, 0, BINARY_LONG,		/* 2002 */
				0, 0
  },
  { "BINARY-SEQUENTIAL",	0, 1, BINARY_SEQUENTIAL,	/* Extension */
				0, CB_CS_DELIMITER
  },
  { "BINARY-SHORT",		0, 0, BINARY_SHORT,		/* 2002 */
				0, 0
  },
  { "BIT",			0, 0, BIT,			/* 2002 */
				0, 0
  },
  { "BITMAP",		1, 1, BITMAP,		/* ACU extension */
				CB_CS_GRAPHICAL_CONTROL, CB_CS_DISPLAY | CB_CS_SCREEN
  },
  { "BITMAP-END",		0, 1, BITMAP_END,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "BITMAP-HANDLE",		0, 1, BITMAP_HANDLE,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "BITMAP-NUMBER",		0, 1, BITMAP_NUMBER,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "BITMAP-START",		0, 1, BITMAP_START,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "BITMAP-TIMER",		0, 1, BITMAP_TIMER,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "BITMAP-TRAILING",		0, 1, BITMAP_TRAILING,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "BITMAP-TRANSPARENT-COLOR",		0, 1, BITMAP_TRANSPARENT_COLOR,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "BITMAP-WIDTH",		0, 1, BITMAP_WIDTH,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "BLANK",			0, 0, BLANK,			/* 2002 */
				0, 0
  },
  { "BLINK",			0, 1, BLINK,			/* 2002 (C/S) */
				0, CB_CS_ACCEPT | CB_CS_DISPLAY | CB_CS_SCREEN | CB_CS_SET
  },
  { "BLOCK",			0, 0, BLOCK,			/* 2002 */
				0, 0
  },
  { "BOOLEAN",			0, 0, -1,			/* 2002 */
				0, 0
  },
  { "BOTTOM",			0, 0, BOTTOM,			/* 2002 */
				0, 0
  },
  { "BOX",		0, 1, BOX,		/* ACU extension */
				0, CB_CS_DISPLAY
  },
  { "BOXED",		0, 1, BOXED,		/* ACU extension */
				0, CB_CS_DISPLAY
  },
  { "BULK-ADDITION",		0, 1, BULK_ADDITION,		/* ACU extension */
				0, CB_CS_OPEN
  },
  { "BUSY",		0, 1, BUSY,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "BUTTONS",		0, 1, BUTTONS,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "BY",			0, 0, BY,			/* 2002 */
				0, 0
  },
  { "BYTE-LENGTH",		0, 1, BYTE_LENGTH,		/* 2002 (C/S) */
				0, CB_CS_CONSTANT
  },
  { "C",			0, 1, C,			/* Extension: implicit defined CALL-CONVENTION */
				0, CB_CS_CALL | CB_CS_OPTIONS
  },
  { "CALENDAR-FONT",		0, 1, CALENDAR_FONT,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "CALL",			1, 0, CALL,			/* 2002 */
				CB_CS_CALL, 0
  },
  { "CANCEL",			0, 0, CANCEL,			/* 2002 */
				0, 0
  },
  { "CANCEL-BUTTON",		0, 1, CANCEL_BUTTON,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "CAPACITY",			0, 1, CAPACITY,			/* 2014 */
				0, CB_CS_OCCURS
  },
  { "CARD-PUNCH",		0, 1, CARD_PUNCH,		/* Extension */
				0, CB_CS_ASSIGN
  },
  { "CARD-READER",		0, 1, CARD_READER,		/* Extension */
				0, CB_CS_ASSIGN
  },
  { "CASSETTE",			0, 1, CASSETTE,			/* Extension */
				0, CB_CS_ASSIGN
  },
  { "CCOL",		0, 1, CCOL,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "CD",			0, 0, CD,			/* Communication Section */
				0, 0
  },
  { "CELL",		0, 1, CELL,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "CELL-COLOR",		0, 1, CELL_COLOR,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "CELL-DATA",		0, 1, CELL_DATA,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "CELL-FONT",		0, 1, CELL_FONT,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "CELL-PROTECTION",		0, 1, CELL_PROTECTION,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "CENTER",			0, 1, CENTER,			/* 2002 (C/S) */
				0, 0
	/* FIXME + Check: 2014 Context-sensitive to COLUMN clause */
  },
  { "CENTERED",		0, 1, CENTERED,		/* ACU extension */
	  0, CB_CS_DISPLAY
  },
  { "CENTERED-HEADINGS",		0, 1, CENTERED_HEADINGS,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "CENTURY-DATE",		0, 1, CENTURY_DATE,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "CF",			0, 0, CF,			/* 2002 */
				0, 0
  },
  { "CH",			0, 0, CH,			/* 2002 */
				0, 0
  },
  { "CHAIN",			0, 0, -1,			/* Extension */
				0, 0
  },
  { "CHAINING",			0, 0, CHAINING,			/* Extension */
				0, 0
  },
  { "CHANGED",			0, 1, CHANGED,			/* OSVS/MF */
				0, CB_CS_EXHIBIT
  },
  { "CHARACTER",		0, 0, CHARACTER,		/* 2002 */
				0, 0
  },
  { "CHARACTERS",		0, 0, CHARACTERS,		/* 85 (OBJECT-COMPUTER) 2002 */
				0, 0
  },
  { "CHECK-BOX",		1, 1, CHECK_BOX,		/* ACU extension */
				CB_CS_GRAPHICAL_CONTROL, CB_CS_DISPLAY | CB_CS_SCREEN
  },
  { "CLASS",			0, 0, CLASS,			/* 2002 */
				0, 0
  },
  { "CLASS-ID",			0, 0, -1,			/* 2002 */
				0, 0
  },
  { "CLASSIFICATION",		0, 1, CLASSIFICATION,		/* 2002 (C/S) */
				0, 0
	/* FIXME + Check: 2014 Context-sensitive to OBJECT-COMPUTER paragraph */
  },
  { "CLEAR-SELECTION",		0, 1, CLEAR_SELECTION,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "CLINE",		0, 1, CLINE,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "CLINES",		0, 1, CLINES,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "CLOSE",			0, 0, CLOSE,			/* 2002 */
				0, 0
  },
  { "COBOL",			0, 1, COBOL,			/* 2002
								   Extension: implicit defined CALL-CONVENTION */
				0, CB_CS_CALL | CB_CS_OPTIONS
  },
  { "CODE",			0, 0, CODE,			/* 2002 */
				0, 0
  },
  { "CODE-SET",			0, 0, CODE_SET,			/* 2002 */
				0, 0
  },
  { "COL",			0, 0, COL,			/* 2002 -> Does we need this or can we alias COLUMN? */
				0, 0
  },
  { "COLLATING",		0, 0, COLLATING,		/* 2002 */
				0, 0
  },
  { "COLOR",			0, 0, COLOR,			/* Extension */
				0, 0
  },
  { "COLORS",		0, 1, COLORS,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "COLS",			0, 0, COLS,			/* 2002 */
				0, 0
  },
  { "COLUMN",			0, 0, COLUMN,			/* 2002 */
				0, 0
  },
  { "COLUMN-COLOR",		0, 1, COLUMN_COLOR,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "COLUMN-DIVIDERS",		0, 1, COLUMN_DIVIDERS,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "COLUMN-FONT",		0, 1, COLUMN_FONT,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "COLUMN-HEADINGS",		0, 1, COLUMN_HEADINGS,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "COLUMN-PROTECTION",		0, 1, COLUMN_PROTECTION,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "COLUMNS",			0, 0, COLUMNS,			/* 2002 */
				0, 0
  },
  { "COMBO-BOX",		1, 1, COMBO_BOX,		/* ACU extension */
				CB_CS_GRAPHICAL_CONTROL, CB_CS_DISPLAY | CB_CS_SCREEN
  },
  { "COMMA",			0, 0, COMMA,			/* 2002 */
				0, 0
  },
  { "COMMAND-LINE",		0, 0, COMMAND_LINE,		/* Extension */
				0, 0
  },
  { "COMMIT",			0, 0, COMMIT,			/* Extension */
				0, 0
  },
  { "COMMON",			0, 0, COMMON,			/* 2002 */
				0, 0
  },
  { "COMMUNICATION",		0, 0, COMMUNICATION,		/* Communication Section */
				0, 0
  },
  { "COMP",			0, 0, COMP,			/* 2002 */
				0, 0
  },
  { "COMP-0",			0, 0, COMP_0,			/* Extension */
				0, 0
  },
  { "COMP-1",			0, 0, COMP_1,			/* Extension */
				0, 0
  },
  { "COMP-2",			0, 0, COMP_2,			/* Extension */
				0, 0
  },
  { "COMP-3",			0, 0, COMP_3,			/* Extension */
				0, 0
  },
  { "COMP-4",			0, 0, COMP_4,			/* Extension */
				0, 0
  },
  { "COMP-5",			0, 0, COMP_5,			/* Extension */
				0, 0
  },
  { "COMP-6",			0, 0, COMP_6,			/* Extension */
				0, 0
  },
  { "COMP-N",			0, 0, COMP_N,			/* Extension */
				0, 0
  },
  { "COMP-X",			0, 0, COMP_X,			/* Extension */
				0, 0
  },
  { "COMPUTATIONAL",		0, 0, COMP,			/* 2002 */
				0, 0
  },
  { "COMPUTATIONAL-0",		0, 0, COMP_0,			/* Extension */
				0, 0
  },
  { "COMPUTATIONAL-1",		0, 0, COMP_1,			/* Extension */
				0, 0
  },
  { "COMPUTATIONAL-2",		0, 0, COMP_2,			/* Extension */
				0, 0
  },
  { "COMPUTATIONAL-3",		0, 0, COMP_3,			/* Extension */
				0, 0
  },
  { "COMPUTATIONAL-4",		0, 0, COMP_4,			/* Extension */
				0, 0
  },
  { "COMPUTATIONAL-5",		0, 0, COMP_5,			/* Extension */
				0, 0
  },
  { "COMPUTATIONAL-6",		0, 0, COMP_6,			/* Extension */
				0, 0
  },
  { "COMPUTATIONAL-N",		0, 0, COMP_N,			/* Extension */
				0, 0
  },
  { "COMPUTATIONAL-X",		0, 0, COMP_X,			/* Extension */
				0, 0
  },
  { "COMPUTE",			1, 0, COMPUTE,			/* 2002 */
				0, 0
  },
  { "CONDITION",		0, 0, CONDITION,		/* 2002 */
				0, 0
  },
  { "CONFIGURATION",		0, 0, CONFIGURATION,		/* 2002 */
				0, 0
  },
  { "CONSTANT",			0, 0, CONSTANT,			/* 2002 */
				CB_CS_CONSTANT, 0
  },
  { "CONTAINS",			0, 0, CONTAINS,			/* 2002 */
				0, 0
  },
  { "CONTENT",			0, 0, CONTENT,			/* 2002 */
				0, 0
  },
  { "CONTINUE",			0, 0, CONTINUE,			/* 2002 */
				0, 0
  },
  { "CONTROL",			0, 0, CONTROL,			/* 2002 */
				0, 0
  },
  { "CONTROLS",			0, 0, CONTROLS,			/* 2002 */
				0, 0
  },
  { "CONVERSION",		0, 1, CONVERSION,		/* Extension */
				0, CB_CS_ACCEPT
  },
  { "CONVERTING",		0, 0, CONVERTING,		/* 2002 */
				0, 0
  },
  { "COPY",			0, 0, COPY,			/* 2002 */
				0, 0
  },
  { "COPY-SELECTION",		0, 1, COPY_SELECTION,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "CORE-INDEX",		0, 1, CORE_INDEX,		/* OS/VS extension */
				0, CB_CS_I_O_CONTROL
  },
  { "CORR",			0, 0, CORRESPONDING,		/* 2002 */
				0, 0
  },
  { "CORRESPONDING",		0, 0, CORRESPONDING,		/* 2002 */
				0, 0
  },
  { "COUNT",			0, 0, COUNT,			/* 2002 */
				0, 0
  },
  { "CRT",			0, 0, CRT,			/* 2002 */
				0, 0
  },
  { "CRT-UNDER",		0, 0, CRT_UNDER,		/* Extension */
				0, 0
  },
  { "CSIZE",		0, 1, CSIZE,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "CURRENCY",			0, 0, CURRENCY,			/* 2002 */
				0, 0
  },
  { "CURSOR",			0, 0, CURSOR,			/* 2002 */
				0, 0
  },
  { "CURSOR-COL",		0, 1, CURSOR_COL,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "CURSOR-COLOR",		0, 1, CURSOR_COLOR,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "CURSOR-FRAME-WIDTH",		0, 1, CURSOR_FRAME_WIDTH,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "CURSOR-ROW",		0, 1, CURSOR_ROW,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "CURSOR-X",		0, 1, CURSOR_X,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "CURSOR-Y",		0, 1, CURSOR_Y,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "CUSTOM-PRINT-TEMPLATE",		0, 1, CUSTOM_PRINT_TEMPLATE,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "CYCLE",			0, 1, CYCLE,			/* 2002 (C/S) */
				0, CB_CS_EXIT
  },
  { "CYL-INDEX",		0, 1, CYL_INDEX,		/* OS/VS extension */
				0, CB_CS_I_O_CONTROL
  },
  { "CYL-OVERFLOW",		0, 1, CYL_OVERFLOW,		/* OS/VS extension */
				0, CB_CS_I_O_CONTROL
  },
  { "DASHED",		0, 1, DASHED,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "DATA",			0, 0, DATA,			/* 2002 */
				0, 0
  },
  { "DATA-COLUMNS",		0, 1, DATA_COLUMNS,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "DATA-POINTER",		0, 0, -1,			/* 2002 */
				0, 0
  },
  { "DATA-TYPES",		0, 1, DATA_TYPES,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "DATE",			0, 0, DATE,			/* 2002 */
				CB_CS_DATE, 0
  },
  { "DATE-COMPILED",			0, 1, DATE_COMPILED,			/* 85 (later: C/S) */
				0, CB_CS_DAY /* HACK, we only want it to normally be not usable */
  },
  { "DATE-ENTRY",		1, 1, DATE_ENTRY,		/* ACU extension */
				CB_CS_GRAPHICAL_CONTROL, CB_CS_DISPLAY | CB_CS_SCREEN
  },
  { "DATE-MODIFIED",			0, 1, DATE_MODIFIED,			/* 85 (later: C/S) */
				0, CB_CS_DAY /* HACK, we only want it to normally be not usable */
  },
  { "DATE-WRITTEN",			0, 1, DATE_WRITTEN,			/* 85 (later: C/S) */
				0, CB_CS_DAY /* HACK, we only want it to normally be not usable */
  },
  { "DAY",			0, 0, DAY,			/* 2002 */
				CB_CS_DAY, 0
  },
  { "DAY-OF-WEEK",		0, 0, DAY_OF_WEEK,		/* 2002 */
				0, 0
  },
  { "DE",			0, 0, DE,			/* 2002 */
				0, 0
  },
  { "DEBUGGING",		0, 0, DEBUGGING,		/* 2002 */
				0, 0
  },
  { "DECIMAL-POINT",		0, 0, DECIMAL_POINT,		/* 2002 */
				0, 0
  },
  { "DECLARATIVES",		0, 0, DECLARATIVES,		/* 2002 */
				0, 0
  },
  { "DEFAULT",			0, 0, DEFAULT,			/* 2002 */
				0, 0
  },
  { "DEFAULT-BUTTON",		0, 1, DEFAULT_BUTTON,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "DEFAULT-FONT",		0, 0, DEFAULT_FONT,		/* ACU extension */
				0, 0				/* CHECKME: likely context sensitive */
  },
  { "DELETE",			1, 0, DELETE,			/* 2002 */
				0, 0
  },
  { "DELIMITED",		0, 0, DELIMITED,		/* 2002 */
				0, 0
  },
  { "DELIMITER",		0, 0, DELIMITER,		/* 2002 */
				CB_CS_DELIMITER, 0
  },
  { "DEPENDING",		0, 0, DEPENDING,		/* 2002 */
				0, 0
  },
  { "DESCENDING",		0, 0, DESCENDING,		/* 2002 */
				0, 0
  },
  { "DESTINATION",		0, 0, DESTINATION,		/* 2002 */
				0, 0
  },
  { "DESTROY",			0, 0, DESTROY,			/* ACU extension */
				0, 0
  },
  { "DETAIL",			0, 0, DETAIL,			/* 2002 */
				0, 0
  },
  { "DISABLE",			0, 0, DISABLE,			/* Communication Section */
				0, 0
  },
  { "DISC",			0, 1, DISC,			/* Extension */
				0, CB_CS_ASSIGN
  },
  { "DISK",			0, 1, DISK,			/* Extension */
				0, CB_CS_ASSIGN
  },
  { "DISP",			0, 1, DISP,			/* OS/VS extension */
				0, CB_CS_OPEN
  },
  { "DISPLAY",			1, 0, DISPLAY,			/* 2002 */
				0, 0
  },
  { "DISPLAY-COLUMNS",		0, 1, DISPLAY_COLUMNS,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "DISPLAY-FORMAT",		0, 1, DISPLAY_FORMAT,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "DIVIDE",			1, 0, DIVIDE,			/* 2002 */
				0, 0
  },
  { "DIVIDER-COLOR",		0, 1, DIVIDER_COLOR,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "DIVIDERS",		0, 1, DIVIDERS,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "DIVISION",			0, 0, DIVISION,			/* 2002 */
				0, 0
  },
  { "DOTDASH",		0, 1, DOTDASH,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "DOTTED",		0, 1, DOTTED,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "DOUBLE",		0, 0, FLOAT_LONG,		/* ACU extension */
				0, 0
  },
  { "DOWN",			0, 0, DOWN,			/* 2002 */
				0, 0
  },
  { "DRAG-COLOR",		0, 1, DRAG_COLOR,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "DROP-DOWN",		0, 1, DROP_DOWN,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "DROP-LIST",		0, 1, DROP_LIST,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "DUPLICATES",		0, 0, DUPLICATES,		/* 2002 */
				0, 0
  },
  { "DYNAMIC",			0, 0, DYNAMIC,			/* 2002 */
				0, 0
  },
  { "EBCDIC",			0, 1, EBCDIC,			/* Extension */
				0, CB_CS_ALPHABET
  },
  { "EC",			0, 0, EC,			/* 2002 */
				0, 0
  },
  { "ECHO",			0, 0, ECHO,			/* Extension */
				0, 0
  },
  { "EGI",			0, 0, EGI,			/* Communication Section */
				0, 0
  },
  { "ELEMENT",			0, 1, ELEMENT,			/* IBM extension */
				0, CB_CS_XML_GENERATE
  },
  { "ELSE",			0, 0, ELSE,			/* 2002 */
				0, 0
  },
  { "EMI",			0, 0, EMI,			/* Communication Section */
				0, 0
  },
  { "ENABLE",			0, 0, ENABLE,			/* Communication Section */
				0, 0
  },
  { "ENCODING",			0, 1, ENCODING,			/* IBM extension */
				0, CB_CS_XML_GENERATE | CB_CS_XML_PARSE
  },
  { "ENCRYPTION",			0, 1, ENCRYPTION,			/* ACU extension */
				0, CB_CS_SELECT
  },
  { "END",			0, 0, END,			/* 2002 */
				0, 0
  },
  { "END-ACCEPT",		0, 0, END_ACCEPT,		/* 2002 */
				0, 0
  },
  { "END-ADD",			0, 0, END_ADD,			/* 2002 */
				0, 0
  },
  { "END-CALL",			0, 0, END_CALL,			/* 2002 */
				0, 0
  },
  { "END-CHAIN",		0, 0, -1,			/* Extension */
				0, 0
  },
  { "END-COLOR",		0, 1, END_COLOR,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "END-COMPUTE",		0, 0, END_COMPUTE,		/* 2002 */
				0, 0
  },
  { "END-DELETE",		0, 0, END_DELETE,		/* 2002 */
				0, 0
  },
  { "END-DISPLAY",		0, 0, END_DISPLAY,		/* 2002 */
				0, 0
  },
  { "END-DIVIDE",		0, 0, END_DIVIDE,		/* 2002 */
				0, 0
  },
  { "END-EVALUATE",		0, 0, END_EVALUATE,		/* 2002 */
				0, 0
  },
  { "END-IF",			0, 0, END_IF,			/* 2002 */
				0, 0
  },
  { "END-JSON",			0, 0, END_JSON,			/* IBM extension */
   				0, 0
  },
  { "END-MODIFY",		0, 1, END_MODIFY,		/* ACU extension */
				0, CB_CS_INQUIRE_MODIFY
  },
  { "END-MULTIPLY",		0, 0, END_MULTIPLY,		/* 2002 */
				0, 0
  },
  { "END-OF-PAGE",		0, 0, EOP,			/* 2002 */
				0, 0
  },
  { "END-PERFORM",		0, 0, END_PERFORM,		/* 2002 */
				0, 0
  },
  { "END-READ",			0, 0, END_READ,			/* 2002 */
				0, 0
  },
  { "END-RECEIVE",		0, 0, END_RECEIVE,		/* Communication Section */
				0, 0
  },
  { "END-RETURN",		0, 0, END_RETURN,		/* 2002 */
				0, 0
  },
  { "END-REWRITE",		0, 0, END_REWRITE,		/* 2002 */
				0, 0
  },
  { "END-SEARCH",		0, 0, END_SEARCH,		/* 2002 */
				0, 0
  },
  { "END-START",		0, 0, END_START,		/* 2002 */
				0, 0
  },
  { "END-STRING",		0, 0, END_STRING,		/* 2002 */
				0, 0
  },
  { "END-SUBTRACT",		0, 0, END_SUBTRACT,		/* 2002 */
				0, 0
  },
  { "END-UNSTRING",		0, 0, END_UNSTRING,		/* 2002 */
				0, 0
  },
  { "END-WRITE",		0, 0, END_WRITE,		/* 2002 */
				0, 0
  },
  { "END-XML",			0, 0, END_XML,			/* IBM extension */
   				0, 0
  },
  { "ENGRAVED",		0, 1, ENGRAVED,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "ENSURE-VISIBLE",		0, 1, ENSURE_VISIBLE,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "ENTRY",			0, 0, ENTRY,			/* Extension */
				0, 0
  },
  { "ENTRY-CONVENTION",		0, 1, ENTRY_CONVENTION,		/* 2002 (C/S) */
				0, CB_CS_OPTIONS
  },
  { "ENTRY-FIELD",		1, 1, ENTRY_FIELD,		/* ACU extension */
				CB_CS_GRAPHICAL_CONTROL, CB_CS_DISPLAY | CB_CS_SCREEN
  },
  { "ENTRY-REASON",		0, 1, ENTRY_REASON,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "ENVIRONMENT",		0, 0, ENVIRONMENT,		/* 2002 */
				0, 0
  },
  { "ENVIRONMENT-NAME",		0, 0, ENVIRONMENT_NAME,		/* Extension */
				0, 0
  },
  { "ENVIRONMENT-VALUE",	0, 0, ENVIRONMENT_VALUE,	/* Extension */
				0, 0
  },
  { "EO",			0, 0, -1,			/* 2002 */
				0, 0
  },
  { "EOL",			0, 1, EOL,			/* 2002 (C/S) */
				0, CB_CS_ERASE
  },
  { "EOP",			0, 0, EOP,			/* 2002 */
				0, 0
  },
  { "EOS",			0, 1, EOS,			/* 2002 (C/S) */
				0, CB_CS_ERASE
  },
  { "EQUAL",			0, 0, EQUAL,			/* 2002 */
				0, 0
  },
  { "ERASE",			0, 1, ERASE,			/* 2002 (C/S) */
				CB_CS_ERASE, CB_CS_ACCEPT | CB_CS_DISPLAY | CB_CS_SCREEN | CB_CS_EXHIBIT
  },
  { "ERROR",			0, 0, ERROR,			/* 2002 */
				0, 0
  },
  { "ESCAPE",			0, 0, ESCAPE,			/* Extension */
				0, 0
  },
  { "ESCAPE-BUTTON",		0, 1, ESCAPE_BUTTON,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "ESI",			0, 0, ESI,			/* Communication Section */
				0, 0
  },
  { "EVALUATE",			1, 0, EVALUATE,			/* 2002 */
				0, 0
  },
  { "EVENT",			1, 0, EVENT,			/* ACU extension */
				0, 0
  },
  { "EVENT-LIST",		0, 1, EVENT_LIST,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "EVERY",			0, 1, EVERY,			/* IBM extension */
				0, CB_CS_I_O_CONTROL | CB_CS_XML_GENERATE
  },
  { "EXCEPTION",		0, 0, EXCEPTION,		/* 2002 */
				0, 0
  },
  { "EXCEPTION-OBJECT",		0, 0, -1,			/* 2002 */
				0, 0
  },
  { "EXCEPTION-VALUE",		0, 1, EXCEPTION_VALUE,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "EXCLUSIVE",		0, 0, EXCLUSIVE,		/* 2002 */
				0, 0
  },
  { "EXHIBIT",			1, 0, EXHIBIT,			/* OSVS/MF */
				CB_CS_EXHIBIT, 0
  },
  { "EXIT",			0, 0, EXIT,			/* 2002 */
				CB_CS_EXIT, 0
  },
  { "EXPAND",		0, 1, EXPAND,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "EXPANDS",			0, 1, -1,			/* 2002 (C/S) */
				0, 0
	/* FIXME: 2014 Context-sensitive to class-specifier and
	   interface-specifier of REPOSITORY paragraph */
  },
  { "EXTEND",			0, 0, EXTEND,			/* 2002 */
				0, 0
  },
  { "EXTENDED-SEARCH",		0, 1, EXTENDED_SEARCH,		/* OS/VS extension */
				0, CB_CS_I_O_CONTROL
  },
  { "EXTERN",			0, 1, TOK_EXTERN,		/* 2002 Implementor specific ENTRY-CONVENTION,
								   Extension: implicit defined CALL-CONVENTION */
				0, CB_CS_CALL | CB_CS_OPTIONS
  },
  { "EXTERNAL",			0, 0, EXTERNAL,			/* 2002 */
				0, 0
  },
  { "EXTERNAL-FORM",			0, 0, EXTERNAL_FORM,			/* ACU CGI extension */
				0, 0
  },
  { "F",			0, 1, F,			/* Extension */
				0, CB_CS_RECORDING
  },
  { "FACTORY",			0, 0, -1,			/* 2002 */
				0, 0
  },
  { "FALSE",			0, 0, TOK_FALSE,		/* 2002 */
				0, 0
  },
  { "FD",			0, 0, FD,			/* 2002 */
				0, 0
  },
  { "FH--FCD",			0, 1, FH__FCD,			/* MF extension */
				0, CB_CS_SET
  },
  { "FH--KEYDEF",			0, 1, FH__KEYDEF,			/* MF extension */
				0, CB_CS_SET
  },
  { "FILE",			0, 0, TOK_FILE,			/* 2002 */
				0, 0
  },
  { "FILE-CONTROL",		0, 0, FILE_CONTROL,		/* 2002 */
				0, 0
  },
  { "FILE-ID",			0, 0, FILE_ID,			/* Extension */
				0, 0
  },
  { "FILE-LIMIT",		0, 1, FILE_LIMIT,		/* OS/VS extension */
				0, CB_CS_SELECT
  },
  { "FILE-LIMITS",		0, 1, FILE_LIMITS,		/* OS/VS extension */
				0, CB_CS_SELECT
  },
  { "FILE-NAME",		0, 1, FILE_NAME,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "FILE-POS",		0, 1, FILE_POS,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "FILL-COLOR",		0, 1, FILL_COLOR,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "FILL-COLOR2",		0, 1, FILL_COLOR2,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "FILL-PERCENT",		0, 1, FILL_PERCENT,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "FILLER",			0, 0, FILLER,			/* 2002 */
				0, 0
  },
  { "FINAL",			0, 0, FINAL,			/* 2002 */
				0, 0
  },
  { "FINISH-REASON",		0, 1, FINISH_REASON,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "FIRST",			0, 0, FIRST,			/* 2002 */
				0, 0
  },
  { "FIXED",			0, 0, FIXED,			/* Extension */
				0, CB_CS_RECORDING
  },
  { "FIXED-FONT",		0, 0, FIXED_FONT,		/* ACU extension */
				0, 0				/* CHECKME: likely context sensitive */
  },
  { "FIXED-WIDTH",		0, 1, FIXED_WIDTH,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "FLAT",		0, 1, FLAT,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "FLAT-BUTTONS",		0, 1, FLAT_BUTTONS,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "FLOAT",		0, 0, FLOAT_SHORT,		/* ACU extension */
				0, 0
  },
  { "FLOAT-BINARY-128",		0, 0, -1,			/* 2014 */
				0, 0
  },
  { "FLOAT-BINARY-32",		0, 0, -1,			/* 2014 */
				0, 0
  },
  { "FLOAT-BINARY-64",		0, 0, -1,			/* 2014 */
				0, 0
  },
  { "FLOAT-DECIMAL-16",		0, 0, FLOAT_DECIMAL_16,		/* 2014 */
				0, 0
  },
  { "FLOAT-DECIMAL-34",		0, 0, FLOAT_DECIMAL_34,		/* 2014 */
				0, 0
  },
#if	0	/* RXWRXW - FP Decimal */
  { "FLOAT-DECIMAL-7",		0, 0, -1,			/* Extension */
				0, 0
  },
#endif
  /* note: may be set as alias for FLOAT-LONG to enable compilation,
           the actual precision seems to be compiler (version) specific */
  { "FLOAT-EXTENDED",		0, 0, -1,			/* 2002 */
				0, 0
  },
  { "FLOAT-INFINITY",		0, 0, -1,			/* 2014 */
				0, 0
  },
  { "FLOAT-LONG",		0, 0, FLOAT_LONG,		/* 2002 */
				0, 0
  },
  { "FLOAT-NOT-A-NUMBER",	0, 1, -1,			/* 2014 */
				0, 0
  },
  { "FLOAT-SHORT",		0, 0, FLOAT_SHORT,		/* 2002 */
				0, 0
  },
  { "FLOATING",			0, 0, FLOATING,			/* ACU extension */
				0, CB_CS_DISPLAY
  },
  { "FONT",			0, 0, FONT,			/* ACU extension */
				0, 0				/* CHECKME: likely context sensitive */
  },
  { "FOOTING",			0, 0, FOOTING,			/* 2002 */
				0, 0
  },
  { "FOR",			0, 0, FOR,			/* 2002 */
				0, 0
  },
  { "FOREGROUND-COLOR",		0, 1, FOREGROUND_COLOR,		/* 2002 (C/S) */
				0, CB_CS_ACCEPT | CB_CS_DISPLAY | CB_CS_SCREEN
  },
  { "FOREVER",			0, 1, FOREVER,			/* 2002 (C/S) */
				0, CB_CS_PERFORM | CB_CS_RETRY
  },
  { "FORMAT",			0, 0, -1,			/* 2002 */
				0, 0
  },
  { "FRAME",		1, 1, FRAME,		/* ACU extension */
				CB_CS_GRAPHICAL_CONTROL, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "FRAMED",		0, 1, FRAMED,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "FREE",			0, 0, FREE,			/* 2002 */
				0, 0
  },
  { "FROM",			0, 0, FROM,			/* 2002 */
				CB_CS_FROM, 0
  },
  { "FULL",			0, 1, FULL,			/* 2002 (C/S) */
				0, CB_CS_ACCEPT | CB_CS_DISPLAY | CB_CS_SCREEN
  },
  { "FULL-HEIGHT",		0, 1, FULL_HEIGHT,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "FUNCTION",			0, 0, FUNCTION,			/* 2002 */
				0, 0
  },
  { "FUNCTION-ID",		0, 0, FUNCTION_ID,		/* 2002 */
				0, 0
  },
  { "FUNCTION-POINTER",		0, 0, -1,			/* 2014 */
				0, 0
  },
  { "GENERATE",			0, 0, GENERATE,			/* 2002 */
				0, 0
  },
  { "GET",			0, 0, -1,			/* 2002 */
				0, 0
  },
  { "GIVING",			0, 0, GIVING,			/* 2002 */
				0, 0
  },
  { "GLOBAL",			0, 0, GLOBAL,			/* 2002 */
				0, 0
  },
  { "GO",			0, 0, GO,			/* 2002 */
				0, 0
  },
  { "GO-BACK",		0, 1, GO_BACK,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "GO-FORWARD",		0, 1, GO_FORWARD,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "GO-HOME",		0, 1, GO_HOME,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "GO-SEARCH",		0, 1, GO_SEARCH,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "GOBACK",			0, 0, GOBACK,			/* 2002 */
				0, 0
  },
  { "GRAPHICAL",		0, 1, GRAPHICAL,		/* ACU extension */
				0, CB_CS_DISPLAY
  },
  { "GREATER",			0, 0, GREATER,			/* 2002 */
				0, 0
  },
  { "GRID",			1, 1, GRID,			/* Extension (ACU control, MF) */
				CB_CS_GRAPHICAL_CONTROL, CB_CS_SCREEN
  },
  { "GROUP",			0, 0, GROUP,			/* 2002 */
				0, 0
  },
  { "GROUP-USAGE",		0, 0, -1,			/* 2002 */
				0, 0
  },
  { "GROUP-VALUE",		0, 1, GROUP_VALUE,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "HANDLE",			0, 0, HANDLE,			/* ACU extension */
				0, 0
  },
  { "HAS-CHILDREN",		0, 1, HAS_CHILDREN,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "HEADING",			0, 0, HEADING,			/* 2002 */
				0, 0
  },
  { "HEADING-COLOR",		0, 1, HEADING_COLOR,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "HEADING-DIVIDER-COLOR",		0, 1, HEADING_DIVIDER_COLOR,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "HEADING-FONT",		0, 1, HEADING_FONT,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "HEAVY",		0, 1, HEAVY,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "HEIGHT-IN-CELLS",		0, 1, HEIGHT_IN_CELLS,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "HIDDEN-DATA",		0, 1, HIDDEN_DATA,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "HIGH-COLOR",		0, 1, HIGH_COLOR,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "HIGH-VALUE",		0, 0, HIGH_VALUE,		/* 2002 */
				0, 0
  },
  { "HIGHLIGHT",		0, 1, HIGHLIGHT,		/* 2002 (C/S) */
				0, CB_CS_ACCEPT | CB_CS_DISPLAY | CB_CS_SCREEN | CB_CS_SET
  },
  { "HOT-TRACK",		0, 1, HOT_TRACK,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "HSCROLL",		0, 1, HSCROLL,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "HSCROLL-POS",		0, 1, HSCROLL_POS,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "I-O",			0, 0, I_O,			/* 2002 */
				0, 0
  },
  { "I-O-CONTROL",		1, 0, I_O_CONTROL,		/* 2002 */
				CB_CS_I_O_CONTROL, 0
  },
  { "ICON",			0, 1, ICON,			/* ACU extension */
				0, CB_CS_DISPLAY
  },
  { "ID",			0, 0, ID,			/* Extension */
				0, 0
  },
  { "IDENTIFICATION",		0, 0, IDENTIFICATION,		/* 2002 */
				0, 0
  },
  { "IDENTIFIED",			0, 0, IDENTIFIED,			/* ACU CGI extension */
				0, 0
  },
  { "IF",			1, 0, IF,			/* 2002 */
				0, 0
  },
  { "IGNORE",			0, 0, IGNORE,			/* Extension */
				0, 0
  },
  { "IGNORING",			0, 1, IGNORING,			/* 2002 (C/S) */
				0, CB_CS_READ
  },
  { "IMPLEMENTS",		0, 1, -1,			/* 2002 (C/S) */
				0, 0
	/* FIXME: 2014 Context-sensitive to FACTORY and OBJECT paragraph */
  },
  { "IN",			0, 0, IN,			/* 2002 */
				0, 0
  },
  { "INDEPENDENT",		0, 1, INDEPENDENT,		/* ACU extension */
				0, CB_CS_DISPLAY
  },
  { "INDEX",			0, 0, INDEX,			/* 2002 */
				0, 0
  },
  { "INDEXED",			0, 0, INDEXED,			/* 2002 */
				0, 0
  },
  { "INDICATE",			0, 0, INDICATE,			/* 2002 */
				0, 0
  },
  { "INHERITS",			0, 0, -1,			/* 2002 */
				0, 0
  },
  { "INITIAL",			0, 0, TOK_INITIAL,		/* 2002 */
				0, 0
  },
  { "INITIALIZE",		0, 0, INITIALIZE,		/* 2002 */
				0, 0
  },
  { "INITIALIZED",		0, 1, INITIALIZED,		/* 2002 */
				0, CB_CS_ALLOCATE | CB_CS_OCCURS
  },
  { "INITIATE",			0, 0, INITIATE,			/* 2002 */
				0, 0
  },
  { "INPUT",			0, 0, INPUT,			/* 2002 */
				0, 0
  },
  { "INPUT-OUTPUT",		0, 0, INPUT_OUTPUT,		/* 2002 */
				0, 0
  },
  { "INQUIRE",			1, 0, INQUIRE,			/* ACU extension */
				CB_CS_INQUIRE_MODIFY, 0
  },
  { "INSERTION-INDEX",			0, 1, INSERTION_INDEX,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "INSERT-ROWS",			0, 1, INSERT_ROWS,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "INSPECT",			0, 0, INSPECT,			/* 2002 */
				0, 0
  },
  {"INSTALLATION",			0, 1, INSTALLATION,			/* 85 (later: C/S) */
				0, CB_CS_DAY /* HACK, we only want it to normally be not usable */
  },
  { "INTERFACE",		0, 0, -1,			/* 2002 */
				0, 0
  },
  { "INTERFACE-ID",		0, 0, -1,			/* 2002 */
				0, 0
  },
  { "INTERMEDIATE",		0, 1, INTERMEDIATE,		/* 2014 (C/S) */
				0, CB_CS_OPTIONS
  },
  { "INTO",			0, 0, INTO,			/* 2002 */
				0, 0
  },
  { "INTRINSIC",		0, 1, INTRINSIC,		/* 2002 (C/S) */
				0, 0
	/* FIXME: 2014 Context-sensitive to function-specifier of the REPOSITORY paragraph */
  },
  { "INVALID",			0, 0, INVALID,			/* 2002 */
				0, 0
  },
  { "INVOKE",			0, 0, -1,			/* 2002 */
				0, 0
  },
  { "IS",			0, 0, IS,			/* 2002 */
				0, 0
  },
  { "ITEM",			0, 1, ITEM,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "ITEM-TEXT",			0, 1, ITEM_TEXT,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "ITEM-TO-ADD",			0, 1, ITEM_TO_ADD,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "ITEM-TO-DELETE",			0, 1, ITEM_TO_DELETE,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "ITEM-TO-EMPTY",			0, 1, ITEM_TO_EMPTY,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "ITEM-VALUE",			0, 1, ITEM_VALUE,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "JSON",			1, 0, JSON,			/* IBM extension */
				0, 0
  },
  { "JUST",			0, 0, JUSTIFIED,		/* 2002 */
				0, 0
  },
  { "JUSTIFIED",		0, 0, JUSTIFIED,		/* 2002 */
				0, 0
  },
  { "KEPT",			0, 0, KEPT,			/* Extension */
				0, 0
  },
  { "KEY",			0, 0, KEY,			/* 2002 */
				0, 0
  },
  { "KEYBOARD",			0, 1, KEYBOARD,			/* Extension */
				0, CB_CS_ASSIGN
  },
  { "LABEL",			0, 0, LABEL,			/* 85, ACU extension */
				0, 0
  },
  { "LABEL-OFFSET",			0, 1, LABEL_OFFSET,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "LARGE-FONT",		0, 0, LARGE_FONT,		/* ACU extension */
				0, 0				/* CHECKME: likely context sensitive */
  },
  { "LARGE-OFFSET",			0, 1, LARGE_OFFSET,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "LAST",			0, 0, LAST,			/* 2002 */
				0, 0
  },
  { "LAST-ROW",			0, 1, LAST_ROW,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "LAYOUT-DATA",		0, 1, LAYOUT_DATA,		/* ACU extension */
				0, CB_CS_INQUIRE_MODIFY	/* likely wrong context, fix later */
  },
  { "LAYOUT-MANAGER",		0, 0, LAYOUT_MANAGER,		/* ACU extension */
				0, 0				/* Check me: likely context sensitive */
  },
  { "LC_ALL",			0, 1, -1,			/* 2002 (C/S) */
				0, CB_CS_SET
  },
  { "LC_COLLATE",		0, 1, -1,			/* 2002 (C/S) */
				0, CB_CS_SET
  },
  { "LC_CTYPE",			0, 1, -1,			/* 2002 (C/S) */
				0, CB_CS_SET
  },
  { "LC_MESSAGES",		0, 1, -1,			/* 2002 (C/S) */
				0, CB_CS_SET
  },
  { "LC_MONETARY",		0, 1, -1,			/* 2002 (C/S) */
				0, CB_CS_SET
  },
  { "LC_NUMERIC",		0, 1, -1,			/* 2002 (C/S) */
				0, CB_CS_SET
  },
  { "LC_TIME",			0, 1, -1,			/* 2002 (C/S) */
				0, CB_CS_SET
  },
  { "LEADING",			0, 0, LEADING,			/* 2002 */
				0, 0
  },
  { "LEADING-SHIFT",			0, 1, LEADING_SHIFT,	/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "LEAVE",			0, 1, LEAVE,			/* OS/VS extension */
    				0, CB_CS_OPEN
  },
  { "LEFT",			0, 0, LEFT,			/* 2002 */
				0, 0
  },
  { "LEFT-JUSTIFY",		0, 0, -1,			/* Extension */
				0, 0
  },
  { "LEFT-TEXT",			0, 1, LEFT_TEXT,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "LEFTLINE",			0, 0, LEFTLINE,			/* Extension */
				0, 0
  },
  { "LENGTH",			0, 0, LENGTH,			/* 2002 */
				0, 0
  },
  { "LESS",			0, 0, LESS,			/* 2002 */
				0, 0
  },
  { "LIKE",			0, 0, LIKE,			/* ILE COBOL */
				0, 0
  },
  { "LIMIT",			0, 0, LIMIT,			/* 2002 */
				0, 0
  },
  { "LIMITS",			0, 0, LIMITS,			/* 2002 */
				0, 0
  },
  { "LINAGE",			0, 0, LINAGE,			/* 2002 */
				0, 0
  },
  { "LINAGE-COUNTER",		0, 0, LINAGE_COUNTER,		/* 2002 */
				0, 0
  },
  { "LINE",			0, 0, LINE,			/* 2002 */
				0, 0
  },
  { "LINE-COUNTER",		0, 0, LINE_COUNTER,		/* 2002 */
				0, 0
  },
  { "LINE-SEQUENTIAL",		0, 1, LINE_SEQUENTIAL,		/* Extension */
				0, CB_CS_DELIMITER
  },
  { "LINES",			0, 0, LINES,			/* 2002 */
				0, 0
  },
  { "LINES-AT-ROOT",			0, 1, LINES_AT_ROOT,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "LINKAGE",			0, 0, LINKAGE,			/* 2002 */
				0, 0
  },
  { "LIST-BOX",		1, 1, LIST_BOX,		/* ACU extension */
				CB_CS_GRAPHICAL_CONTROL, CB_CS_DISPLAY | CB_CS_SCREEN
  },
  { "LM-RESIZE",		0, 0, LM_RESIZE,		/* ACU extension */
				0, 0				/* CHECKME: likely context sensitive */
  },
  { "LOC",		0, 1, LOC,		/* IBM extension (ignored) */
				0, CB_CS_ALLOCATE
  },
  { "LOCAL-STORAGE",		0, 0, LOCAL_STORAGE,		/* 2002 */
				0, 0
  },
  { "LOCALE",			0, 0, LOCALE,			/* 2002 */
				0, 0
  },
  { "LOCK",			0, 0, LOCK,			/* 2002 */
				0, 0
  },
  { "LOCK-HOLDING",		0, 1, LOCK_HOLDING,		/* ACU extension */
				0, CB_CS_I_O_CONTROL
  },
  { "LONG-DATE",			0, 1, LONG_DATE,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "LOW-COLOR",			0, 1, LOW_COLOR,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "LOW-VALUE",		0, 0, LOW_VALUE,		/* 2002 */
				0, 0
  },
  { "LOWER",			0, 1, LOWER,			/* Extension */
				0, CB_CS_ACCEPT
  },
  { "LOWERED",			0, 1, LOWERED,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "LOWLIGHT",			0, 1, LOWLIGHT,			/* 2002 (C/S) */
				0, CB_CS_ACCEPT | CB_CS_DISPLAY | CB_CS_SCREEN | CB_CS_SET
  },
  { "MAGNETIC-TAPE",		0, 1, MAGNETIC_TAPE,		/* Extension */
				0, CB_CS_ASSIGN
  },
  { "MANUAL",			0, 0, MANUAL,			/* 2002 */
				0, 0
	/* FIXME: 2014 Context-sensitive to LOCK MODE clause */
  },
  { "MASS-UPDATE",		0, 1, MASS_UPDATE,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY | CB_CS_SELECT | CB_CS_OPEN
  },
  { "MASTER-INDEX",		0, 1, MASTER_INDEX,		/* OS/VS extension */
				0, CB_CS_I_O_CONTROL
  },
  { "MAX-LINES",		0, 1, MAX_LINES,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "MAX-PROGRESS",		0, 1, MAX_PROGRESS,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "MAX-TEXT",		0, 1, MAX_TEXT,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "MAX-VAL",		0, 1, MAX_VAL,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "MEDIUM-FONT",		0, 0, MEDIUM_FONT,		/* ACU extension */
				0, 0				/* CHECKME: likely context sensitive */
  },
  { "MEMORY",			0, 1, MEMORY,			/* 85 */
				0, CB_CS_OBJECT_COMPUTER
  },
  { "MENU",			0, 0, MENU,			/* ACU extension */
				0, 0				/* CHECKME: likely context sensitive */
  },
  { "MERGE",			0, 0, MERGE,			/* 2002 */
				0, 0
  },
  { "MESSAGE",			0, 0, MESSAGE,			/* Communication Section, COBOL 2014 MCS */
				0, 0
  },
  { "METHOD",			0, 0, -1,			/* 2002 */
				0, 0
  },
  { "METHOD-ID",		0, 0, -1,			/* 2002 */
				0, 0
  },
  { "MIN-VAL",		0, 1, MIN_VAL,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "MINUS",			0, 0, MINUS,			/* 2002 */
				0, 0
  },
  { "MODE",			0, 0, MODE,			/* 2002 */
				0, 0
  },
  { "MODIFY",			1, 0, MODIFY,			/* ACU extension */
				CB_CS_INQUIRE_MODIFY, 0
  },
  { "MODULES",			0, 1, MODULES,			/* 85 */
				0, CB_CS_OBJECT_COMPUTER
  },
  { "MOVE",			0, 0, MOVE,			/* 2002 */
				0, 0
  },
  { "MULTILINE",		0, 1, MULTILINE,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "MULTIPLE",			0, 0, MULTIPLE,			/* 2002 (C/S) */
				0, 0
	/* FIXME: 2014 Context-sensitive to LOCK ON phrase */
  },
  { "MULTIPLY",			1, 0, MULTIPLY,			/* 2002 */
				0, 0
  },
  { "NAME",			0, 1, NAME,			/* Extension */
				0, CB_CS_FROM | CB_CS_XML_GENERATE | CB_CS_JSON_GENERATE
  },
  { "NAMED",			0, 1, NAMED,			/* OSVS/MF */
				0, CB_CS_EXHIBIT
  },
  { "NAMESPACE",		0, 1, NAMESPACE,		/* IBM extension */
				0, CB_CS_XML_GENERATE
  },
  { "NAMESPACE-PREFIX",		0, 1, NAMESPACE_PREFIX,		/* IBM extension */
				0, CB_CS_XML_GENERATE
  },
  { "NATIONAL",			0, 0, NATIONAL,			/* 2002 */
				0, 0
  },
  { "NATIONAL-EDITED",		0, 0, NATIONAL_EDITED,		/* 2002 */
				0, 0
  },
  { "NATIVE",			0, 0, NATIVE,			/* 2002 */
				0, 0
  },
  { "NAVIGATE-URL",		0, 1, NAVIGATE_URL,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "NEAREST-AWAY-FROM-ZERO",	0, 1, NEAREST_AWAY_FROM_ZERO,	/* 2014 (C/S) */
				0, CB_CS_ROUNDED
	/* FIXME: 2014 ... and INTERMEDIATE ROUNDING clause  */
  },
  { "NEAREST-EVEN",		0, 1, NEAREST_EVEN,		/* 2014 (C/S) */
				0, CB_CS_ROUNDED
	/* FIXME: 2014 ... and INTERMEDIATE ROUNDING clause  */
  },
  { "NEAREST-TOWARD-ZERO",	0, 1, NEAREST_TOWARD_ZERO,	/* 2014 (C/S) */
				0, CB_CS_ROUNDED
	/* FIXME: 2014 ... and INTERMEDIATE ROUNDING clause  */
  },
  { "NEGATIVE",			0, 0, NEGATIVE,			/* 2002 */
				0, 0
  },
  { "NESTED",			0, 0, NESTED,			/* 2002 */
				0, 0
  },
  { "NEW",			0, 0, NEW,			/* 2002 */
				0, 0
  },
  { "NEXT",			0, 0, NEXT,			/* 2002 */
				0, 0
  },
  { "NEXT-ITEM",		0, 1, NEXT_ITEM,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "NO",			0, 0, NO,			/* 2002 */
				0, 0
  },
  { "NO-AUTOSEL",			0, 1, NO_AUTOSEL,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "NO-AUTO-DEFAULT",			0, 1, NO_AUTO_DEFAULT,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "NO-BOX",			0, 1, NO_BOX,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "NO-DIVIDERS",			0, 1, NO_DIVIDERS,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "NO-ECHO",			0, 0, NO_ECHO,			/* Extension */
				0, 0
  },
  { "NO-F4",			0, 1, NO_F4,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "NO-FOCUS",			0, 1, NO_FOCUS,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "NO-GROUP-TAB",			0, 1, NO_GROUP_TAB,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "NO-KEY-LETTER",			0, 1, NO_KEY_LETTER,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "NOMINAL",			0, 1, NOMINAL,			/* OS/VS extension */
				0, CB_CS_SELECT
  },
  { "NO-SEARCH",			0, 1, NO_SEARCH,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "NO-UPDOWN",			0, 1, NO_UPDOWN,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "NONE",			0, 1, -1,			/* 2002 (C/S) */
				0, 0
  },
  { "NONNUMERIC",		0, 1, NONNUMERIC,		/* IBM extension */
				0, CB_CS_XML_GENERATE
  },
  { "NORMAL",			0, 1, NORMAL,			/* 2002 (C/S) */
				0, CB_CS_STOP
  },
  { "NOT",			0, 0, NOT,			/* 2002 */
				0, 0
  },
  { "NOTAB",			0, 1, NOTAB,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "NOTHING",			0, 0, NOTHING,			/* Extension */
				0, 0
  },
  { "NOTIFY",			0, 1, NOTIFY,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "NOTIFY-CHANGE",			0, 1, NOTIFY_CHANGE,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "NOTIFY-DBLCLICK",			0, 1, NOTIFY_DBLCLICK,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "NOTIFY-SELCHANGE",			0, 1, NOTIFY_SELCHANGE,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "NULL",			0, 0, TOK_NULL,			/* 2002 */
				0, 0
  },
  { "NULLS",			0, 0, TOK_NULL,			/* Extension */
				0, 0
  },
  { "NUM-COL-HEADINGS",			0, 1, NUM_COL_HEADINGS,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "NUM-ROWS",			0, 1, NUM_ROWS,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "NUMBER",			0, 0, NUMBER,			/* 2002 */
				0, 0
  },
  { "NUMBERS",			0, 0, NUMBERS,			/* 2002 (C/S) */
				0, 0
	/* FIXME: 2014 Context-sensitive to LINE and COLUMN clause */
  },
  { "NUMERIC",			0, 0, NUMERIC,			/* 2002 */
				0, 0
  },
  { "NUMERIC-EDITED",		0, 0, NUMERIC_EDITED,		/* 2002 */
				0, 0
  },
  { "OBJECT",			0, 0, OBJECT,			/* 2002, ACU extension */
				0, 0
  },
  { "OBJECT-COMPUTER",		0, 0, OBJECT_COMPUTER,		/* 2002 */
				CB_CS_OBJECT_COMPUTER, 0
  },
  { "OBJECT-REFERENCE",		0, 0, -1,			/* 2002 */
				0, 0
  },
  { "OCCURS",			0, 0, OCCURS,			/* 2002 */
				CB_CS_OCCURS, 0
  },
  { "OF",			0, 0, OF,			/* 2002 */
				0, 0
  },
  { "OFF",			0, 0, OFF,			/* 2002 */
				0, 0
  },
  { "OK-BUTTON",			0, 1, OK_BUTTON,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "OMITTED",			0, 0, OMITTED,			/* 2002 */
				0, 0
  },
  { "ON",			0, 0, ON,			/* 2002 */
				0, 0
  },
  { "ONLY",			0, 0, ONLY,			/* 2002 (C/S) */
				0, 0
	/* FIXME: 2014 Context-sensitive to Object-view, SHARING clause, SHARING phrase, and USAGE clause */
  },
  { "OPEN",			1, 0, OPEN,			/* 2002 */
				CB_CS_OPEN, 0
  },
  { "OPTIONAL",			0, 0, OPTIONAL,			/* 2002 */
				0, 0
  },
  { "OPTIONS",			0, 0, OPTIONS,			/* 2002 */
				CB_CS_OPTIONS, 0
  },
  { "OR",			0, 0, OR,			/* 2002 */
				0, 0
  },
  { "ORDER",			0, 0, ORDER,			/* 2002 */
				0, 0
  },
  { "ORGANIZATION",		0, 0, ORGANIZATION,		/* 2002 */
				0, 0
  },
  { "OTHER",			0, 0, OTHER,			/* 2002 */
				0, 0
  },
  { "OTHERS",		0, 1, OTHERS,		/* ACU extension */
				0, CB_CS_OPEN
  },
  { "OUTPUT",			0, 0, OUTPUT,			/* 2002 */
				0, 0
  },
  { "OVERFLOW",			0, 0, TOK_OVERFLOW,		/* 2002 */
				0, 0
  },
  { "OVERLAP-LEFT",			0, 1, OVERLAP_LEFT,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "OVERLAP-TOP",			0, 1, OVERLAP_LEFT,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "OVERLINE",			0, 0, OVERLINE,			/* Extension */
				0, 0
  },
  { "OVERRIDE",			0, 0, -1,			/* 2002 */
				0, 0
  },
  { "PACKED-DECIMAL",		0, 0, PACKED_DECIMAL,		/* 2002 */
				0, 0
  },
  { "PADDING",			0, 0, PADDING,			/* 2002 */
				0, 0
  },
  { "PAGE",			0, 0, PAGE,			/* 2002 */
				0, 0
  },
  { "PAGE-COUNTER",		0, 0, PAGE_COUNTER,		/* 2002 */
				0, 0
  },
  { "PAGE-SETUP",			0, 1, PAGE_SETUP,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "PAGED",			0, 1, PAGED,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "PARAGRAPH",		0, 1, PARAGRAPH,		/* 2002 (C/S) */
				0, CB_CS_EXIT
  },
  { "PARENT",			0, 1, PARENT,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "PARSE",			0, 1, PARSE,			/* IBM extension */
   				0, 0
  },
  {"PASCAL",			0, 1, PASCAL,			/* Extension: implicit defined CALL-CONVENTION */
				0, CB_CS_CALL | CB_CS_OPTIONS
  },
  { "PASSWORD",			0, 1, PASSWORD,			/* IBM extension */
				0, CB_CS_SELECT
  },
  { "PERFORM",			1, 0, PERFORM,			/* 2002 */
				CB_CS_PERFORM, 0
  },
  { "PERMANENT",			0, 1, PERMANENT,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "PF",			0, 0, PF,			/* 2002 */
				0, 0
  },
  { "PH",			0, 0, PH,			/* 2002 */
				0, 0
  },
  { "PHYSICAL",			0, 0, PHYSICAL,			/* 2014, note:
	only listed as argument for LENGTH FUNCTIONS...  */
				0, 0
  },
  { "PIC",			0, 0, PICTURE,			/* 2002 */
				0, 0
  },
  { "PICTURE",			0, 0, PICTURE,			/* 2002 */
				0, 0
  },
  { "PIXEL",		0, 1, PIXEL,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "PLACEMENT",			0, 1, PLACEMENT,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "PLUS",			0, 0, PLUS,			/* 2002 */
				0, 0
  },
  { "POINTER",			0, 0, POINTER,			/* 2002 */
				0, 0
  },
  { "POP-UP",			0, 1, POP_UP,			/* ACU extension */
				0, CB_CS_DISPLAY
  },
  { "POS",			0, 0, POS,			/* ACU extension for AT POSITION */
	  0, 0
  },
  { "POSITION",			0, 0, POSITION,			/* 85 */
				0, 0
  },
  { "POSITION-SHIFT",			0, 1, POSITION_SHIFT,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "POSITIVE",			0, 0, POSITIVE,			/* 2002 */
				0, 0
  },
  { "PREFIXED",			0, 1, -1,			/* 2014 (C/S) */
				0, 0
	/* FIXME: 2014 Context-sensitive to ANY LENGTH STRUCTURE clause */
  },
  { "PRESENT",			0, 0, PRESENT,			/* 2002 */
				0, 0
  },
  { "PREVIOUS",			0, 1, PREVIOUS,			/* 2002 (C/S) */
				0, CB_CS_READ
  },
  { "PRINT",			0, 1, PRINT,			/* Extension */
				0, CB_CS_ASSIGN
  },
  { "PRINT-NO-PROMPT",			0, 1, PRINT_NO_PROMPT,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "PRINT-PREVIEW",			0, 1, PRINT_PREVIEW,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "PRINTER",			0, 1, PRINTER,			/* Extension */
				0, CB_CS_ASSIGN
  },
  { "PRINTER-1",		0, 1, PRINTER_1,		/* Extension */
				0, CB_CS_ASSIGN
  },
  { "PRINTING",			0, 0, PRINTING,			/* 2002 */
				0, 0
  },
  { "PRIORITY",			0, 0, PRIORITY,			/* ACU extension */
				0, 0				/* CHECKME: likely context sensitive */
  },
  { "PROCEDURE",		0, 0, PROCEDURE,		/* 2002 */
				0, 0
  },
  { "PROCEDURE-POINTER",	0, 0, PROGRAM_POINTER,		/* Extension */
				0, 0
  },
  { "PROCEDURES",		0, 0, PROCEDURES,		/* Extension */
				0, 0
  },
  { "PROCEED",			0, 0, PROCEED,			/* 85 */
				0, 0
  },
  { "PROCESSING",		0, 1, PROCESSING,		/* IBM extension */
				0, CB_CS_XML_PARSE
  },
  { "PROGRAM",			0, 0, PROGRAM,			/* 2002 */
				0, 0
  },
  { "PROGRAM-ID",		0, 0, PROGRAM_ID,		/* 2002 */
				0, 0
  },
  { "PROGRAM-POINTER",		0, 0, PROGRAM_POINTER,		/* 2002 */
				0, 0
  },
  { "PROGRESS",			0, 1, PROGRESS,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "PROHIBITED",		0, 1, PROHIBITED,		/* 2014 (C/S) */
				0, CB_CS_ROUNDED
	/* FIXME: 2014 ... and INTERMEDIATE ROUNDING clause clause */
  },
  { "PROMPT",			0, 0, PROMPT,			/* Extension */
				0, 0
  },
  { "PROPERTIES",			0, 1, PROPERTIES,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "PROPERTY",			0, 0, PROPERTY,			/* 2002, ACU extension */
				0, 0
  },
  { "PROTECTED",		0, 1, PROTECTED,		/* Extension PROTECTED SIZE */
				0, CB_CS_ACCEPT
  },
  { "PROTOTYPE",		0, 0, PROTOTYPE,			/* 2002 */
				0, 0
  },
  { "PURGE",			0, 0, PURGE,			/* Communication Section */
				0, 0
  },
  { "PUSH-BUTTON",		1, 1, PUSH_BUTTON,		/* ACU extension */
				CB_CS_GRAPHICAL_CONTROL, CB_CS_DISPLAY | CB_CS_SCREEN
  },
  { "QUERY-INDEX",			0, 1, QUERY_INDEX,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "QUEUE",			0, 0, QUEUE,			/* Communication Section */
				0, 0
  },
  { "QUOTE",			0, 0, QUOTE,			/* 2002 */
				0, 0
  },
  { "QUOTES",			0, 0, QUOTE,			/* 2002 */
				0, 0
  },
  { "RADIO-BUTTON",		1, 1, RADIO_BUTTON,		/* ACU extension */
				CB_CS_GRAPHICAL_CONTROL, CB_CS_DISPLAY | CB_CS_SCREEN
  },
  { "RAISE",			0, 0, RAISE,			/* 2002 */
				0, 0
  },
  { "RAISED",			0, 1, RAISED,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "RAISING",			0, 0, RAISING,			/* 2002 */
				0, 0
  },
  { "RANDOM",			0, 0, RANDOM,			/* 2002 */
				0, 0
  },
  { "RD",			0, 0, RD,			/* 2002 */
				0, 0
  },
  { "READ",			1, 0, READ,			/* 2002 */
				CB_CS_READ, 0
  },
  { "READ-ONLY",		0, 1, READ_ONLY,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "READERS",			0, 1, READERS,		/* ACU extension */
				0, CB_CS_OPEN
  },
  { "RECEIVE",			1, 0, RECEIVE,			/* Communication Section, 2014 MCS */
				0, 0
  },
  { "RECEIVED",			1, 0, RECEIVED,			/* 2014 MCS */
				0, 0
  },
  { "RECORD",			0, 0, RECORD,			/* 2002 */
				0, 0
  },
  { "RECORD-DATA",			0, 1, RECORD_DATA,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "RECORD-OVERFLOW",		0, 1, RECORD_OVERFLOW,		/* OS/VS extension */
				0, CB_CS_I_O_CONTROL
  },
  { "RECORD-TO-ADD",			0, 1, RECORD_TO_ADD,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "RECORD-TO-DELETE",			0, 1, RECORD_TO_DELETE,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "RECORDING",		0, 0, RECORDING,		/* Extension */
				CB_CS_RECORDING, 0
  },
  { "RECORDS",			0, 0, RECORDS,			/* 2002 */
				0, 0
  },
  { "RECURSIVE",		0, 1, RECURSIVE,		/* 2002 (C/S) */
				0, CB_CS_PROGRAM_ID
  },
  { "REDEFINES",		0, 0, REDEFINES,		/* 2002 */
				0, 0
  },
  { "REEL",			0, 0, REEL,			/* 2002 */
				0, 0
  },
  { "REFERENCE",		0, 0, REFERENCE,		/* 2002 */
				0, 0
  },
  { "REFERENCES",		0, 0, REFERENCES,		/* Obsolete */
				0, 0
  },
  { "REFRESH",			0, 1, REFRESH,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "REGION-COLOR",			0, 1, REGION_COLOR,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "RELATION",			0, 1, -1,			/* 2002 (C/S) */
				0, 0
	/* FIXME: 2014 Context-sensitive to VALIDATE-STATUS clause */
  },
  { "RELATIVE",			0, 0, RELATIVE,			/* 2002 */
				0, 0
  },
  { "RELEASE",			0, 0, RELEASE,			/* 2002 */
				0, 0
  },
  { "REMAINDER",		0, 0, REMAINDER,		/* 2002 */
				0, 0
  },
  {"REMARKS",			0, 1, REMARKS,			/* 85 (later: C/S) */
				0, CB_CS_DAY /* HACK, we only want it to normally be not usable */
  },
  { "REMOVAL",			0, 0, REMOVAL,			/* 2002 */
				0, 0
  },
  { "RENAMES",			0, 0, RENAMES,			/* 2002 */
				0, 0
  },
  { "REORG-CRITERIA",		0, 1, REORG_CRITERIA,		/* OS/VS extension */
				0, CB_CS_I_O_CONTROL
  },
  { "REPLACE",			0, 0, REPLACE,			/* 2002 */
				0, 0
  },
  { "REPLACING",		0, 0, REPLACING,		/* 2002 */
				0, 0
  },
  { "REPORT",			0, 0, REPORT,			/* 2002 */
				0, 0
  },
  { "REPORTING",		0, 0, REPORTING,		/* 2002 */
				0, 0
  },
  { "REPORTS",			0, 0, REPORTS,			/* 2002 */
				0, 0
  },
  { "REPOSITORY",		0, 0, REPOSITORY,		/* 2002 */
				0, 0
  },
  { "REQUIRED",			0, 1, REQUIRED,			/* 2002 (C/S) */
				0, CB_CS_ACCEPT | CB_CS_SCREEN
  },
  { "REREAD",			0, 1, REREAD,			/* OS/VS extension */
				0, CB_CS_OPEN
  },
  { "RERUN",			0, 1, RERUN,			/* IBM extension */
				0, CB_CS_I_O_CONTROL
  },
  { "RESERVE",			0, 0, RESERVE,			/* 2002 */
				0, 0
  },
  { "RESET",			0, 0, RESET,			/* 2002 */
				0, 0
  },
  { "RESET-GRID",			0, 1, RESET_GRID,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "RESET-LIST",			0, 1, RESET_LIST,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "RESET-TABS",			0, 1, RESET_TABS,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "RESUME",			0, 0, -1,			/* 2002 */
				0, 0
  },
  { "RETRY",			0, 0, RETRY,			/* 2002 */
				CB_CS_RETRY, 0
  },
  { "RETURN",			1, 0, RETURN,			/* 2002 */
				0, 0
  },
  { "RETURNING",		0, 0, RETURNING,		/* 2002 */
				0, 0
  },
  { "REVERSE",			0, 0, REVERSE,			/* Extension */
				0, 0
  },
  { "REVERSE-VIDEO",		0, 1, REVERSE_VIDEO,		/* 2002 (C/S) */
				0, CB_CS_ACCEPT | CB_CS_DISPLAY | CB_CS_SCREEN | CB_CS_SET
  },
  { "REVERSED",			0, 0, REVERSED,			/* Obsolete */
				0, 0
  },
  { "REWIND",			0, 0, REWIND,			/* 2002 */
				0, 0
  },
  { "REWRITE",			1, 0, REWRITE,			/* 2002 */
				0, 0
  },
  { "RF",			0, 0, RF,			/* 2002 */
				0, 0
  },
  { "RH",			0, 0, RH,			/* 2002 */
				0, 0
  },
  { "RIGHT",			0, 0, RIGHT,			/* 2002 */
				0, 0
  },
  { "RIGHT-ALIGN",			0, 1, RIGHT_ALIGN,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "RIGHT-JUSTIFY",		0, 0, -1,			/* Extension */
				0, 0
  },
  { "RIMMED",			0, 1, RIMMED,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "ROLLBACK",			0, 0, ROLLBACK,			/* Extension */
				0, 0
  },
  { "ROUNDED",			0, 0, ROUNDED,			/* 2002 */
				CB_CS_ROUNDED, 0
  },
  { "ROUNDING",			0, 1, ROUNDING,			/* 2002 (C/S) */
				0, CB_CS_OPTIONS
  },
  { "ROW-COLOR",			0, 1, ROW_COLOR,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "ROW-COLOR-PATTERN",			0, 1, ROW_COLOR_PATTERN,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "ROW-DIVIDERS",			0, 1, ROW_DIVIDERS,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "ROW-FONT",			0, 1, ROW_FONT,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "ROW-HEADINGS",			0, 1, ROW_HEADINGS,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "ROW-PROTECTION",			0, 1, ROW_PROTECTION,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "RUN",			0, 0, RUN,			/* 2002 */
				0, 0
  },
  { "S",			0, 1, S,			/* Extension */
				0, CB_CS_RECORDING
  },
  { "SAME",			0, 0, SAME,			/* 2002 */
				0, 0
  },
  { "SAVE-AS",			0, 1, SAVE_AS,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "SAVE-AS-NO-PROMPT",			0, 1, SAVE_AS_NO_PROMPT,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "SCREEN",			0, 0, SCREEN,			/* 2002 */
				0, 0
  },
  { "SCROLL",			0, 1, SCROLL,			/* Extension */
				0, CB_CS_ACCEPT | CB_CS_DISPLAY
  },
  { "SCROLL-BAR",			1, 1, SCROLL_BAR,			/* ACU extension */
				CB_CS_GRAPHICAL_CONTROL, CB_CS_DISPLAY | CB_CS_SCREEN
  },
  { "SD",			0, 0, SD,			/* 2002 */
				0, 0
  },
  { "SEARCH",			1, 0, SEARCH,			/* 2002 */
				0, 0
  },
  { "SEARCH-OPTIONS",			0, 1, SEARCH_OPTIONS,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "SEARCH-TEXT",			0, 1, SEARCH_TEXT,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "SECONDS",			0, 1, SECONDS,			/* 2002 (C/S) */
				0, CB_CS_RETRY
  },
  { "SECTION",			0, 0, SECTION,			/* 2002 */
				0, 0
  },
  { "SECURE",			0, 1, SECURE,			/* 2002 (C/S) */
				0, CB_CS_ACCEPT | CB_CS_DISPLAY | CB_CS_SCREEN
  },
  {"SECURITY",			0, 1, SECURITY,			/* 85 (later: C/S) */
				0, CB_CS_DAY /* HACK, we only want it to normally be not usable */
  },
  { "SEGMENT",			0, 0, SEGMENT,			/* Communication Section */
				0, 0
  },
  { "SEGMENT-LIMIT",		0, 0, SEGMENT_LIMIT,		/* 85 */
				0, 0
  },
  { "SELECT",			1, 0, SELECT,			/* 2002 */
				CB_CS_SELECT, 0
  },
  { "SELECT-ALL",			0, 1, SELECT_ALL,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "SELECTION-INDEX",			0, 1, SELECTION_INDEX,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "SELECTION-TEXT",			0, 1, SELECTION_TEXT,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "SELF",			0, 0, -1,			/* 2002 */
				0, 0
  },
  { "SELF-ACT",			0, 1, SELF_ACT,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "SEND",			0, 0, SEND,			/* Communication Section, 2014 MCS */
				0, 0
  },
  { "SENTENCE",			0, 0, SENTENCE,			/* 2002 */
				0, 0
  },
  { "SEPARATE",			0, 0, SEPARATE,			/* 2002 */
				0, 0
  },
  { "SEPARATION",			0, 1, SEPARATION,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "SEQUENCE",			0, 0, SEQUENCE,			/* 2002 */
				0, 0
  },
  { "SEQUENTIAL",		0, 0, SEQUENTIAL,		/* 2002 */
				0, 0
  },
  { "SET",			0, 0, SET,			/* 2002 */
				0, 0
  },
  { "SHADING",			0, 1, SHADING,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "SHADOW",		0, 1, SHADOW,		/* ACU extension */
				0, CB_CS_DISPLAY
  },
  { "SHARING",			0, 0, SHARING,			/* 2002 */
				0, 0
  },
#if 0 /* FIXME: 2014 Context-sensitive to ANY LENGTH STRUCTURE clause */
  { "SHORT",			0, 0, -1,			/* 2014 */
				0, 0
  },
#endif
  { "SHORT-DATE",			0, 1, SHORT_DATE,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "SHOW-LINES",			0, 1, SHOW_LINES,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "SHOW-NONE",			0, 1, SHOW_NONE,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "SHOW-SEL-ALWAYS",			0, 1, SHOW_SEL_ALWAYS,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "SIGN",			0, 0, SIGN,			/* 2002 */
				0, 0
  },
  { "SIGNED",			0, 0, SIGNED,			/* 2002 (C/S) */
				0, 0
	/* FIXME: 2014 Context-sensitive to ANY LENGTH STRUCTURE clause
	   and USAGE clause */
  },
  { "SIGNED-INT",		0, 0, SIGNED_INT,		/* Extension */
				0, 0
  },
  { "SIGNED-LONG",		0, 0, SIGNED_LONG,		/* Extension */
				0, 0
  },
  { "SIGNED-SHORT",		0, 0, SIGNED_SHORT,		/* Extension */
				0, 0
  },
  { "SIZE",			0, 0, SIZE,			/* 2002 */
				0, 0
  },
  { "SMALL-FONT",		0, 0, SMALL_FONT,		/* ACU extension */
				0, 0				/* CHECKME: likely context sensitive */
  },
  { "SORT",			0, 0, SORT,			/* 2002 */
				0, 0
  },
  { "SORT-MERGE",		0, 0, SORT_MERGE,		/* 2002 */
				0, 0
  },
  { "SORT-ORDER",			0, 1, SORT_ORDER,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "SOURCE",			0, 0, SOURCE,			/* 2002 */
				0, 0
  },
  { "SOURCE-COMPUTER",		0, 0, SOURCE_COMPUTER,		/* 2002 */
				0, 0
  },
  { "SOURCES",			0, 0, -1,			/* 2002 */
				0, 0
  },
  { "SPACE",			0, 0, SPACE,			/* 2002 */
				0, 0
  },
  { "SPACE-FILL",		0, 0, -1,			/* Extension */
				0, 0
  },
  { "SPACES",			0, 0, SPACE,			/* 2002 */
				0, 0
  },
  { "SPECIAL-NAMES",		0, 0, SPECIAL_NAMES,		/* 2002 */
				0, 0
  },
  { "SPINNER",			0, 1, SPINNER,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "SQUARE",			0, 1, SQUARE,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "STANDARD",			0, 0, STANDARD,			/* 2002 */
				0, 0
  },
  { "STANDARD-1",		0, 0, STANDARD_1,		/* 2002 */
				0, 0
  },
  { "STANDARD-2",		0, 0, STANDARD_2,		/* 2002 */
				0, 0
  },
  { "STANDARD-BINARY",		0, 1, STANDARD_BINARY,			/* 2014 (C/S) */
				0, CB_CS_OPTIONS
  },
  { "STANDARD-DECIMAL",		0, 1, STANDARD_DECIMAL,			/* 2014 (C/S) */
				0, CB_CS_OPTIONS
  },
  { "START",			1, 0, START,			/* 2002 */
				0, 0
  },
  { "START-X",			0, 1, START_X,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "START-Y",			0, 1, START_Y,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "STATEMENT",		0, 1, -1,			/* 2002 (C/S) */
				0, 0
	/* FIXME: 2014 Context-sensitive to RESUME statement */
  },
  { "STATIC",			0, 1, STATIC,			/* Extension: implicit defined CALL-CONVENTION */
				0, CB_CS_CALL
  },
  { "STATIC-LIST",			0, 1, STATIC_LIST,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "STATUS",			0, 0, STATUS,			/* 2002 */
				0, 0
  },
  { "STATUS-BAR",			1, 1, STATUS_BAR,			/* ACU extension */
				CB_CS_GRAPHICAL_CONTROL, CB_CS_DISPLAY | CB_CS_SCREEN
  },
  { "STATUS-TEXT",			0, 1, STATUS_TEXT,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "STDCALL",			0, 1, STDCALL,			/* Extension: implicit defined CALL-CONVENTION */
				0, CB_CS_CALL | CB_CS_OPTIONS
  },
  { "STEP",			0, 1, STEP,			/* 2002 (C/S) */
				0, CB_CS_OCCURS
  },
  { "STOP",			0, 0, STOP,			/* 2002 */
				CB_CS_STOP, 0
  },
  { "STRING",			1, 0, STRING,			/* 2002 */
				0, 0
  },
  { "STRONG",			0, 1, STRONG,			/* 2002 (C/S) */
				0, CB_CS_TYPEDEF
  },
  { "STYLE",			0, 1, STYLE,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "SUB-QUEUE-1",		0, 0, SUB_QUEUE_1,		/* Communication Section */
				0, 0
  },
  { "SUB-QUEUE-2",		0, 0, SUB_QUEUE_2,		/* Communication Section */
				0, 0
  },
  { "SUB-QUEUE-3",		0, 0, SUB_QUEUE_3,		/* Communication Section */
				0, 0
  },
  { "SUBTRACT",			1, 0, SUBTRACT,			/* 2002 */
				0, 0
  },
  { "SUBWINDOW",		0, 0, SUBWINDOW,		/* ACU extension */
				0, 0				/* CHECKME: likely context sensitive */
  },
  { "SUM",			0, 0, SUM,			/* 2002 */
				0, 0
  },
  { "SUPER",			0, 0, -1,			/* 2002 */
				0, 0
  },
  { "SUPPRESS",			0, 0, SUPPRESS,			/* 2002 */
				0, 0
  },
  { "SYMBOL",			0, 1, -1,			/* 2002 (C/S) */
				0, 0
	/* FIXME: 2014 Context-sensitive to CURRENCY clause */
  },
  { "SYMBOLIC",			0, 0, SYMBOLIC,			/* 2002 */
				0, 0
  },
  { "SYNC",			0, 0, SYNCHRONIZED,		/* 2002 */
				0, 0
  },
  { "SYNCHRONIZED",		0, 0, SYNCHRONIZED,		/* 2002 */
				0, 0
  },
  { "SYSTEM-DEFAULT",		0, 0, SYSTEM_DEFAULT,		/* 2002 */
				0, 0
  },
  { "SYSTEM-INFO",		0, 1, SYSTEM_INFO,		/* ACU extension */
				0, CB_CS_ACCEPT
  },
  { "SYSTEM-OFFSET",		0, 0, SYSTEM_OFFSET,		/* Extension */
				0, 0
  },
  { "TAB",			1, 1, TAB,			/* Extension */
				CB_CS_GRAPHICAL_CONTROL, CB_CS_ACCEPT | CB_CS_DISPLAY | CB_CS_SCREEN
  },
  { "TAB-TO-ADD",			0, 1, TAB_TO_ADD,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "TAB-TO-DELETE",			0, 1, TAB_TO_DELETE,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "TABLE",			0, 0, TABLE,			/* Communication Section */
				0, 0
  },
  { "TALLYING",			0, 0, TALLYING,			/* 2002 */
				0, 0
  },
  { "TAPE",			0, 1, TAPE,			/* 85 */
				0, CB_CS_ASSIGN
  },
  { "TEMPORARY",			0, 1, TEMPORARY,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "TERMINAL",			0, 0, TERMINAL,			/* Communication Section */
				0, 0
  },
  { "TERMINAL-INFO",		0, 1, TERMINAL_INFO,		/* ACU extension */
				0, CB_CS_ACCEPT
  },
  { "TERMINATE",		0, 0, TERMINATE,		/* 2002 */
				0, 0
  },
  { "TERMINATION-VALUE",			0, 1, TERMINATION_VALUE,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "TEST",			0, 0, TEST,			/* 2002 */
				0, 0
  },
  { "TEXT",			0, 0, TEXT,			/* Communication Section */
				0, 0
  },
  { "THAN",			0, 0, THAN,			/* 2002 */
				0, 0
  },
  { "THEN",			0, 0, THEN,			/* 2002 */
				0, 0
  },
  { "THREAD",			0, 0, THREAD,			/* ACU extension */
					0, 0
  },
  { "THREADS",			0, 0, THREADS,			/* ACU extension */
					0, 0
  },
  { "THROUGH",			0, 0, THRU,			/* 2002 */
				0, 0
  },
  { "THRU",			0, 0, THRU,			/* 2002 */
				0, 0
  },
  { "THUMB-POSITION",			0, 1, THUMB_POSITION,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "TILED-HEADINGS",			0, 1, TILED_HEADINGS,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "TIME",			0, 0, TIME,			/* 2002 */
				0, 0
  },
  { "TIME-OUT",			0, 1, TIME_OUT,			/* Ext (C/S) */
				0, CB_CS_ACCEPT
  },
  { "TIMES",			0, 0, TIMES,			/* 2002 */
				0, 0
  },
  { "TITLE",			0, 1, TITLE,			/* ACU extension */
				0, CB_CS_DISPLAY | CB_CS_INQUIRE_MODIFY
  },
  { "TITLE-POSITION",			0, 1, TITLE_POSITION,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "TO",			0, 0, TO,			/* 2002 */
				0, 0
  },
  { "TOP",			0, 0, TOP,			/* 2002 */
				0, 0
  },
  { "TOWARD-GREATER",		0, 1, TOWARD_GREATER,		/* 2014 (C/S) */
				0, CB_CS_ROUNDED
  },
  { "TOWARD-LESSER",		0, 1, TOWARD_LESSER,		/* 2014 (C/S) */
				0, CB_CS_ROUNDED
  },
  { "TRACK",			0, 1, TRACK,			/* OS/VS extension */
				0, CB_CS_SELECT | CB_CS_I_O_CONTROL
  },
  { "TRACKS",			0, 1, TRACKS,			/* OS/VS extension */
				0, CB_CS_SELECT | CB_CS_I_O_CONTROL
  },
  { "TRACK-AREA",		0, 1, TRACK_AREA,		/* OS/VS extension */
				0, CB_CS_SELECT
  },
  { "TRACK-LIMIT",		0, 1, TRACK_LIMIT,		/* OS/VS extension */
				0, CB_CS_SELECT
  },
  { "TRADITIONAL-FONT",		0, 0, TRADITIONAL_FONT,		/* ACU extension */
				0, 0					/* CHECKME: likely context sensitive */
  },
  { "TRAILING",			0, 0, TRAILING,			/* 2002 */
				0, 0
  },
  { "TRAILING-SHIFT",		0, 1, TRAILING_SHIFT,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "TRAILING-SIGN",		0, 0, -1,			/* Extension */
				0, 0
  },
  { "TRANSFORM",		0, 0, TRANSFORM,		/* OSVS */
				0, 0
  },
  { "TRANSPARENT",		0, 1, TRANSPARENT,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "TREE-VIEW",		1, 1, TREE_VIEW,		/* ACU extension */
				CB_CS_GRAPHICAL_CONTROL, CB_CS_DISPLAY | CB_CS_SCREEN
  },
  { "TRUE",			0, 0, TOK_TRUE,			/* 2002 */
				0, 0
  },
  { "TRUNCATION",		0, 1, TRUNCATION,		/* 2014 (C/S) */
				0, CB_CS_ROUNDED
	/* FIXME: 2014 ... and INTERMEDIATE ROUNDING phrase */
  },
  { "TYPE",			0, 0, TYPE,			/* 2002 */
				0, 0
  },
  { "TYPEDEF",			0, 0, TYPEDEF,			/* 2002 */
				CB_CS_TYPEDEF, 0
  },
  { "U",			0, 1, U,			/* Extension */
				0, CB_CS_RECORDING
  },
  { "UCS-4",			0, 1, UCS_4,			/* 2002 (C/S) */
				0, CB_CS_ALPHABET
  },
  { "UNBOUNDED",		0, 1, UNBOUNDED,			/* IBM V5 */
				0, CB_CS_OCCURS
  },
  { "UNDERLINE",		0, 1, UNDERLINE,		/* 2002 (C/S) */
				0, CB_CS_ACCEPT | CB_CS_DISPLAY | CB_CS_SCREEN | CB_CS_SET
  },
  { "UNFRAMED",			0, 1, UNFRAMED,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "UNIT",			0, 0, UNIT,			/* 2002 */
				0, 0
  },
  { "UNIVERSAL",		0, 0, -1,			/* 2002 */
				0, 0
  },
  { "UNLOCK",			0, 0, UNLOCK,			/* 2002 */
				0, 0
  },
  { "UNSIGNED",			0, 0, UNSIGNED,			/* 2002 (C/S) */
				0, 0
  },
  { "UNSIGNED-INT",		0, 0, UNSIGNED_INT,		/* Extension */
				0, 0
  },
  { "UNSIGNED-LONG",		0, 0, UNSIGNED_LONG,		/* Extension */
				0, 0
  },
  { "UNSIGNED-SHORT",		0, 0, UNSIGNED_SHORT,		/* Extension */
				0, 0
  },
  { "UNSORTED",			0, 1, UNSORTED,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "UNSTRING",			1, 0, UNSTRING,			/* 2002 */
				0, 0
  },
  { "UNTIL",			0, 0, UNTIL,			/* 2002 */
				0, 0
  },
  { "UP",			0, 0, UP,			/* 2002 */
				0, 0
  },
  { "UPDATE",			0, 0, UPDATE,			/* Extension */
				0, 0
  },
  { "UPDATERS",		0, 1, UPDATERS,		/* ACU extension */
				0, CB_CS_OPEN
  },
  { "UPON",			0, 0, UPON,			/* 2002 */
				0, 0
  },
  { "UPPER",			0, 1, UPPER,			/* Extension */
				0, CB_CS_ACCEPT
  },
  { "USAGE",			0, 0, USAGE,			/* 2002 */
				0, 0
  },
  { "USE",			0, 0, USE,			/* 2002 */
				0, 0
  },
  { "USE-ALT",			0, 1, USE_ALT,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "USE-RETURN",			0, 1, USE_RETURN,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "USE-TAB",			0, 1, USE_TAB,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "USER",			0, 1, USER,			/* Extension */
				0, CB_CS_FROM
  },
  { "USER-DEFAULT",		0, 0, USER_DEFAULT,		/* 2002 */
				0, 0
  },
  { "USING",			0, 0, USING,			/* 2002 */
				0, 0
  },
  { "UTF-16",			0, 1, UTF_16,			/* 2002 (C/S) */
				0, CB_CS_ALPHABET
  },
  { "UTF-8",			0, 1, UTF_8,			/* 2002 (C/S) */
				0, CB_CS_ALPHABET
  },
  { "V",			0, 1, V,			/* Extension */
				0, CB_CS_RECORDING
  },
  { "VAL-STATUS",		0, 0, -1,			/* 2002 */
				0, 0
  },
  { "VALID",			0, 0, -1,			/* 2002 */
				0, 0
  },
  { "VALIDATE",			0, 0, VALIDATE,			/* 2002 */
				0, 0
  },
  { "VALIDATE-STATUS",		0, 0, -1,			/* 2002 */
				0, 0
  },
  { "VALIDATING",		0, 1, VALIDATING,		/* IBM extension */
				0, CB_CS_XML_PARSE
  },
  { "VALUE",			0, 0, VALUE,			/* 2002 */
				0, 0
  },
  { "VALUE-FORMAT",			0, 1, VALUE_FORMAT,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "VARIABLE",			0, 1, VARIABLE,			/* Extension */
				0, CB_CS_RECORDING
  },
  { "VARIANT",			0, 0, VARIANT,			/* ACU extension */
				0, 0
  },
  { "VARYING",			0, 0, VARYING,			/* 2002 */
				0, 0
  },
  { "VERTICAL",			0, 1, VERTICAL,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "VERY-HEAVY",			0, 1, VERY_HEAVY,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "VIRTUAL-WIDTH",			0, 1, VIRTUAL_WIDTH,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "VOLATILE",			0, 0, VOLATILE,			/* IBM Extension */
				0, 0
  },
  { "VPADDING",			0, 1, VPADDING,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "VSCROLL",			0, 1, VSCROLL,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "VSCROLL-BAR",			0, 1, VSCROLL_BAR,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "VSCROLL-POS",			0, 1, VSCROLL_POS,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "VTOP",			0, 1, VTOP,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "WAIT",			0, 0, WAIT,			/* Extension */
				0, 0
  },
  { "WEB-BROWSER",		1, 1, WEB_BROWSER,		/* ACU extension */
				CB_CS_GRAPHICAL_CONTROL, CB_CS_DISPLAY | CB_CS_SCREEN
  },
  { "WHEN",			0, 0, WHEN,			/* 2002 */
				0, 0
  },
  { "WIDTH",			0, 1, WIDTH,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "WIDTH-IN-CELLS",		0, 1, WIDTH_IN_CELLS,		/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "WINDOW",			0, 0, WINDOW,			/* ACU extension */
				0, 0
  },
  { "WITH",			0, 0, WITH,			/* 2002 */
				0, 0
  },
  { "WORDS",			0, 0, WORDS,			/* 85 */
				0, 0
  },
  { "WORKING-STORAGE",		0, 0, WORKING_STORAGE,		/* 2002 */
				0, 0
  },
  { "WRAP",			0, 1, WRAP,			/* Extension */
				0, CB_CS_ACCEPT | CB_CS_DISPLAY
  },
  { "WRITE",			1, 0, WRITE,			/* 2002 */
				0, 0
  },
  { "WRITE-ONLY",		0, 1, WRITE_ONLY,		/* IBM extension */
				0, CB_CS_I_O_CONTROL
  },
  { "WRITE-VERIFY",		0, 1, WRITE_VERIFY,		/* OS/VS extension */
				0, CB_CS_I_O_CONTROL
  },
  { "WRITERS",		0, 1, WRITERS,		/* ACU extension */
				0, CB_CS_OPEN
  },
  { "X",			0, 1, X,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "XML",			1, 0, XML,			/* IBM extension */
   				0, 0
  },
  { "XML-DECLARATION",		0, 1, XML_DECLARATION,		/* IBM extension */
   				0, CB_CS_XML_GENERATE
  },
  { "Y",			0, 1, Y,			/* ACU extension */
				0, CB_CS_GRAPHICAL_CONTROL | CB_CS_INQUIRE_MODIFY
  },
  { "YYYYDDD",			0, 1, YYYYDDD,			/* 2002 (C/S) */
				0, CB_CS_DAY
  },
  { "YYYYMMDD",			0, 1, YYYYMMDD,			/* 2002 (C/S) */
				0, CB_CS_DATE
  },
  { "ZERO",			0, 0, ZERO,			/* 2002 */
				0, 0
  },
  { "ZERO-FILL",		0, 1, -1,			/* Extension */
				0, CB_CS_SCREEN
  }
};

static unsigned int	num_reserved_words;
#define NUM_DEFAULT_RESERVED_WORDS	\
	sizeof (default_reserved_words) / sizeof (struct cobc_reserved)

struct amendment_list {
	struct amendment_list	*next;	/* next pointer */
	char			*word;
	char			*alias_for;
#if 0 /* FIXME: store reference to origin */
	char			*defined_by;
#endif
	int			is_context_sensitive;
	int			to_add;
};

struct register_struct {
	const char				*name;
	const char				*definition;
	enum cb_feature_mode	active;
};

static size_t current_register = 0;

static struct register_struct	register_list[] = {
	{"ADDRESS OF", "USAGE POINTER", CB_FEATURE_ACTIVE},		/* FIXME: currently not handled the "normal" register way */
	{"COB-CRT-STATUS", "PICTURE 9(4) USAGE DISPLAY VALUE ZERO", CB_FEATURE_ACTIVE},	/* FIXME: currently not handled the "normal" register way */
	{"DEBUG-ITEM", "PICTURE X(n) USAGE DISPLAY", CB_FEATURE_ACTIVE},	/* not to be handled the "normal" register way, dending on use use of DEBUGGING code */
	{"LENGTH OF", "CONSTANT USAGE BINARY-LONG", CB_FEATURE_ACTIVE},	/* FIXME: currently not handled the "normal" register way */
	{"NUMBER-OF-CALL-PARAMETERS", "USAGE BINARY-LONG", CB_FEATURE_ACTIVE},	/* OpenCOBOL / GnuCOBOL extension, at least from 1.0+ */
	{"RETURN-CODE", "GLOBAL USAGE BINARY-LONG VALUE ZERO", CB_FEATURE_ACTIVE},
	{"SORT-RETURN", "GLOBAL USAGE BINARY-LONG VALUE ZERO", CB_FEATURE_ACTIVE},
	{"TALLY", "GLOBAL PICTURE 9(5) USAGE BINARY VALUE ZERO", CB_FEATURE_ACTIVE},
	{"COL", "PIC S9(4) USAGE COMP", CB_FEATURE_MUST_BE_ENABLED},	/* rare, normally conflicting --> must be explicit enabled */
	{"LIN", "PIC S9(4) USAGE COMP", CB_FEATURE_MUST_BE_ENABLED},	/* rare, only in combination with COL */
	{"WHEN-COMPILED", "CONSTANT PICTURE X(16) USAGE DISPLAY", CB_FEATURE_ACTIVE},
#if 0 /* ancient OSVS registers that need special runtime handling - low priority */
	{"CURRENT-DATE", "CONSTANT PICTURE X(8) USAGE DISPLAY", CB_FEATURE_MUST_BE_ENABLED},	/* ancient IBM extension, conflicts with COBOL85 amendment */
	{"TIME-OF-DAY", "CONSTANT PICTURE 9(6) USAGE DISPLAY", CB_FEATURE_MUST_BE_ENABLED},		/* ancient IBM extension */
#endif
	{"XML-CODE", "GLOBAL PICTURE S9(9) USAGE BINARY VALUE 0", CB_FEATURE_ACTIVE},
	/* {"XML-EVENT", "USAGE DISPLAY PICTURE X(30) VALUE SPACE", CB_FEATURE_ACTIVE}, */
	/* {"XML-INFORMATION", "PICTURE S9(9) USAGE BINARY VALUE 0", CB_FEATURE_ACTIVE}, */
	/* {"XML-NAMESPACE", "PIC X ANY LENGTH", CB_FEATURE_ACTIVE}, /\* FIXME: currently not handled the "normal" register way *\/ */
	/* {"XML-NAMESPACE-PREFIX", "PIC X ANY LENGTH", CB_FEATURE_ACTIVE}, /\* FIXME: currently not handled the "normal" register way *\/ */
	/* {"XML-NNAMESPACE", "PIC N ANY LENGTH", CB_FEATURE_ACTIVE}, /\* FIXME: currently not handled the "normal" register way *\/ */
	/* {"XML-NNAMESPACE-PREFIX", "PIC N ANY LENGTH", CB_FEATURE_ACTIVE}, /\* FIXME: currently not handled the "normal" register way *\/ */
	/* {"XML-NTEXT", "PIC N ANY LENGTH", CB_FEATURE_ACTIVE}, /\* FIXME: currently not handled the "normal" register way *\/ */
	/* {"XML-TEXT", "PIC X ANY LENGTH", CB_FEATURE_ACTIVE} /\* FIXME: currently not handled the "normal" register way *\/ */
	{"JSON-CODE", "GLOBAL PICTURE S9(9) USAGE BINARY VALUE 0", CB_FEATURE_ACTIVE}
	/* {"JSON-STATUS", "PIC X ANY LENGTH", CB_FEATURE_ACTIVE} /\* FIXME: currently not handled the "normal" register way *\/ */
};

#define	NUM_REGISTERS	sizeof(register_list) / sizeof(struct register_struct)

/* Intrinsic Function List */
/* Must be ordered on name for binary search */

/*	Name,		Routine,					*/
/*	Token,	Parser token,					*/
/*	Implemented, Number of arguments: Max [-1 = unlimited], Min,	*/
/*	Category,	Can refmod							*/

static struct cb_intrinsic_table function_list[] = {
  { "ABS",				"cob_intr_abs",
					CB_INTR_ABS, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  /* ACUCOBOL-extension (synonym for ABS) */
  { "ABSOLUTE-VALUE",		"cob_intr_abs",
					CB_INTR_ABS, FUNCTION_NAME,
					CB_FEATURE_DISABLED,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "ACOS",				"cob_intr_acos",
					CB_INTR_ACOS, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "ANNUITY",				"cob_intr_annuity",
					CB_INTR_ANNUITY, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	2, 2,
					CB_CATEGORY_NUMERIC, 0
  },
  { "ASIN",				"cob_intr_asin",
					CB_INTR_ASIN, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "ATAN",				"cob_intr_atan",
					CB_INTR_ATAN, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  /* IBM ENT */
  { "BIT-OF",		"cob_intr_bit_of",
					CB_INTR_BIT_OF, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_ALPHANUMERIC, 0
  },
  /* IBM ENT */
  { "BIT-TO-CHAR",		"cob_intr_bit_to_char",
					CB_INTR_BIT_TO_CHAR, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_ALPHANUMERIC, 0
  },
  { "BOOLEAN-OF-INTEGER",		"cob_intr_boolean_of_integer",
					CB_INTR_BOOLEAN_OF_INTEGER, FUNCTION_NAME,
					CB_FEATURE_NOT_IMPLEMENTED,	2, 2,
					CB_CATEGORY_NUMERIC, 0
  },
  { "BYTE-LENGTH",			"cob_intr_byte_length",
					CB_INTR_BYTE_LENGTH, LENGTH_FUNC,
					CB_FEATURE_ACTIVE,	2, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "CHAR",				"cob_intr_char",
					CB_INTR_CHAR, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_ALPHANUMERIC, 0
  },
  { "CHAR-NATIONAL",			"cob_intr_char_national",
					CB_INTR_CHAR_NATIONAL, FUNCTION_NAME,
					CB_FEATURE_NOT_IMPLEMENTED,	1, 1,
					CB_CATEGORY_NATIONAL, 0
  },
  { "COMBINED-DATETIME",		"cob_intr_combined_datetime",
					CB_INTR_COMBINED_DATETIME, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	2, 2,
					CB_CATEGORY_NUMERIC, 0
  },
  /* COBOL 202x */
  { "CONCAT",			"cob_intr_concatenate",
					CB_INTR_CONCATENATE, CONCATENATE_FUNC,
					CB_FEATURE_ACTIVE,	-1, 1,
					CB_CATEGORY_ALPHANUMERIC, 1
					/* Note: category changed to alphabetic/national
					   depending on the content, see cb_build_intrinsic */
  },
  /* OpenCOBOL */
  { "CONCATENATE",			"cob_intr_concatenate",
					CB_INTR_CONCATENATE, CONCATENATE_FUNC,
					CB_FEATURE_ACTIVE,	-1, 1,
					CB_CATEGORY_ALPHANUMERIC, 1
					/* Note: category changed to alphabetic/national
					   depending on the content, see cb_build_intrinsic */
  },
  /* GnuCOBOL */
  { "CONTENT-LENGTH",			"cob_intr_content_length",
					CB_INTR_CONTENT_LENGTH, CONTENT_LENGTH_FUNC,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  /* GnuCOBOL */
  { "CONTENT-OF",			"cob_intr_content_of",
					CB_INTR_CONTENT_OF, CONTENT_OF_FUNC,
					CB_FEATURE_ACTIVE,	2, 1,
					CB_CATEGORY_ALPHANUMERIC, 1
  },
  { "COS",				"cob_intr_cos",
					CB_INTR_COS, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "CURRENCY-SYMBOL",			"cob_intr_currency_symbol",
					CB_INTR_CURRENCY_SYMBOL, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	0, 0,
					CB_CATEGORY_ALPHANUMERIC, 0
  },
  { "CURRENT-DATE",			"cob_intr_current_date",
					CB_INTR_CURRENT_DATE, CURRENT_DATE_FUNC,
					CB_FEATURE_ACTIVE,	0, 0,
					CB_CATEGORY_ALPHANUMERIC, 1
  },
  { "DATE-OF-INTEGER",			"cob_intr_date_of_integer",
					CB_INTR_DATE_OF_INTEGER, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "DATE-TO-YYYYMMDD",			"cob_intr_date_to_yyyymmdd",
					CB_INTR_DATE_TO_YYYYMMDD, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	3, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "DAY-OF-INTEGER",			"cob_intr_day_of_integer",
					CB_INTR_DAY_OF_INTEGER, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "DAY-TO-YYYYDDD",			"cob_intr_day_to_yyyyddd",
					CB_INTR_DAY_TO_YYYYDDD, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	3, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "DISPLAY-OF",			"cob_intr_display_of",
					CB_INTR_DISPLAY_OF, DISPLAY_OF_FUNC,
					CB_FEATURE_NOT_IMPLEMENTED,	2, 1,
					CB_CATEGORY_ALPHANUMERIC, 1
  },
  { "E",				"cob_intr_e",
					CB_INTR_E, FUNCTION_NAME,
					CB_FEATURE_ACTIVE, 0, 0,
					CB_CATEGORY_NUMERIC, 0
  },
  { "EXCEPTION-FILE",			"cob_intr_exception_file",
					CB_INTR_EXCEPTION_FILE, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	0, 0,
					CB_CATEGORY_ALPHANUMERIC, 0
  },
  { "EXCEPTION-FILE-N",			"cob_intr_exception_file_n",
					CB_INTR_EXCEPTION_FILE_N, FUNCTION_NAME,
					CB_FEATURE_NOT_IMPLEMENTED,	0, 0,
					CB_CATEGORY_NATIONAL, 0
  },
  { "EXCEPTION-LOCATION",		"cob_intr_exception_location",
					CB_INTR_EXCEPTION_LOCATION, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	0, 0,
					CB_CATEGORY_ALPHANUMERIC, 0
  },
  { "EXCEPTION-LOCATION-N",		"cob_intr_exception_location_n",
					CB_INTR_EXCEPTION_LOCATION_N, FUNCTION_NAME,
					CB_FEATURE_NOT_IMPLEMENTED,	0, 0,
					CB_CATEGORY_NATIONAL, 0
  },
  { "EXCEPTION-STATEMENT",		"cob_intr_exception_statement",
					CB_INTR_EXCEPTION_STATEMENT, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	0, 0,
					CB_CATEGORY_ALPHANUMERIC, 0
  },
  { "EXCEPTION-STATUS",			"cob_intr_exception_status",
					CB_INTR_EXCEPTION_STATUS, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	0, 0,
					CB_CATEGORY_ALPHANUMERIC, 0
  },
  { "EXP",				"cob_intr_exp",
					CB_INTR_EXP, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "EXP10",				"cob_intr_exp10",
					CB_INTR_EXP10, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "FACTORIAL",			"cob_intr_factorial",
					CB_INTR_FACTORIAL, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "FORMATTED-CURRENT-DATE",		"cob_intr_formatted_current_date",
					CB_INTR_FORMATTED_CURRENT_DATE, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_ALPHANUMERIC, 1
					/* Note: category changed to national depending on the content,
					   see cb_build_intrinsic */
  },
  { "FORMATTED-DATE",			"cob_intr_formatted_date",
					CB_INTR_FORMATTED_DATE, FORMATTED_DATE_FUNC,
					CB_FEATURE_ACTIVE,	2, 2,
					CB_CATEGORY_ALPHANUMERIC, 1
					/* Note: category changed to national depending on the content,
					   see cb_build_intrinsic */
  },
  { "FORMATTED-DATETIME",		"cob_intr_formatted_datetime",
					CB_INTR_FORMATTED_DATETIME, FORMATTED_DATETIME_FUNC,
								/* including implicit SYSTEM-OFFSET arg */
					CB_FEATURE_ACTIVE,	  5, 4,
					CB_CATEGORY_ALPHANUMERIC, 1
					/* Note: category changed to national depending on the content,
					   see cb_build_intrinsic */
  },
  { "FORMATTED-TIME",			"cob_intr_formatted_time",
					CB_INTR_FORMATTED_TIME, FORMATTED_TIME_FUNC,
   							/* including implicit SYSTEM-OFFSET arg */
					CB_FEATURE_ACTIVE,	4, 3,
					CB_CATEGORY_ALPHANUMERIC, 1
					/* Note: category changed to national depending on the content,
					   see cb_build_intrinsic */
  },
  { "FRACTION-PART",			"cob_intr_fraction_part",
					CB_INTR_FRACTION_PART, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  /* IBM ENT */
  { "HEX-OF",		"cob_intr_hex_of",
					CB_INTR_HEX_OF, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_ALPHANUMERIC, 0
  },
  /* IBM ENT */
  { "HEX-TO-CHAR",		"cob_intr_hex_to_char",
					CB_INTR_HEX_TO_CHAR, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_ALPHANUMERIC, 0
  },
  { "HIGHEST-ALGEBRAIC",		"cob_intr_highest_algebraic",
					CB_INTR_HIGHEST_ALGEBRAIC, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "INTEGER",				"cob_intr_integer",
					CB_INTR_INTEGER, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "INTEGER-OF-BOOLEAN",		"cob_intr_integer_of_boolean",
					CB_INTR_INTEGER_OF_BOOLEAN, FUNCTION_NAME,
					CB_FEATURE_NOT_IMPLEMENTED,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "INTEGER-OF-DATE",			"cob_intr_integer_of_date",
					CB_INTR_INTEGER_OF_DATE, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "INTEGER-OF-DAY",			"cob_intr_integer_of_day",
					CB_INTR_INTEGER_OF_DAY, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "INTEGER-OF-FORMATTED-DATE",	"cob_intr_integer_of_formatted_date",
					CB_INTR_INTEGER_OF_FORMATTED_DATE, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	2, 2,
					CB_CATEGORY_NUMERIC, 0
  },
  { "INTEGER-PART",			"cob_intr_integer_part",
					CB_INTR_INTEGER_PART, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "LENGTH",				"cob_intr_length",
					CB_INTR_LENGTH, LENGTH_FUNC,
					CB_FEATURE_ACTIVE,	2, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "LENGTH-AN",			"cob_intr_byte_length",
					CB_INTR_BYTE_LENGTH, LENGTH_FUNC,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "LOCALE-COMPARE",			"cob_intr_locale_compare",
					CB_INTR_LOCALE_COMPARE, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	3, 2,
					CB_CATEGORY_ALPHANUMERIC, 0
  },
  { "LOCALE-DATE",			"cob_intr_locale_date",
					CB_INTR_LOCALE_DATE, LOCALE_DATE_FUNC,
					CB_FEATURE_ACTIVE,	2, 1,
					CB_CATEGORY_ALPHANUMERIC, 1
  },
  { "LOCALE-TIME",			"cob_intr_locale_time",
					CB_INTR_LOCALE_TIME, LOCALE_TIME_FUNC,
					CB_FEATURE_ACTIVE,	2, 1,
					CB_CATEGORY_ALPHANUMERIC, 1
  },
  { "LOCALE-TIME-FROM-SECONDS",		"cob_intr_lcl_time_from_secs",
					CB_INTR_LOCALE_TIME_FROM_SECS, LOCALE_TIME_FROM_FUNC,
					CB_FEATURE_ACTIVE,	2, 1,
					CB_CATEGORY_ALPHANUMERIC, 1
  },
  { "LOG",				"cob_intr_log",
					CB_INTR_LOG, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "LOG10",				"cob_intr_log10",
					CB_INTR_LOG10, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "LOWER-CASE",			"cob_intr_lower_case",
					CB_INTR_LOWER_CASE, LOWER_CASE_FUNC,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_ALPHANUMERIC, 1
					/* Note: category changed to national depending on the content,
					   see cb_build_intrinsic */
  },
  { "LOWEST-ALGEBRAIC",			"cob_intr_lowest_algebraic",
					CB_INTR_LOWEST_ALGEBRAIC, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "MAX",				"cob_intr_max",
					CB_INTR_MAX, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	-1, 1,
					CB_CATEGORY_NUMERIC, 0
					/* Note: category changed to alphanumeric/index/national depending on the content,
					   see cb_build_intrinsic */
  },
  { "MEAN",				"cob_intr_mean",
					CB_INTR_MEAN, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	-1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "MEDIAN",				"cob_intr_median",
					CB_INTR_MEDIAN, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	-1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "MIDRANGE",				"cob_intr_midrange",
					CB_INTR_MIDRANGE, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	-1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "MIN",				"cob_intr_min",
					CB_INTR_MIN, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	-1, 1,
					CB_CATEGORY_NUMERIC, 0
					/* Note: category changed to alphanumeric/index/national depending on the content,
					   see cb_build_intrinsic */
  },
  { "MOD",				"cob_intr_mod",
					CB_INTR_MOD, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	2, 2,
					CB_CATEGORY_NUMERIC, 0
  },
  { "MODULE-CALLER-ID",			"cob_intr_module_caller_id",
					CB_INTR_MODULE_CALLER_ID, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	0, 0,
					CB_CATEGORY_ALPHANUMERIC, 0
  },
  { "MODULE-DATE",			"cob_intr_module_date",
					CB_INTR_MODULE_DATE, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	0, 0,
					CB_CATEGORY_NUMERIC, 0
  },
  { "MODULE-FORMATTED-DATE",		"cob_intr_module_formatted_date",
					CB_INTR_MODULE_FORMATTED_DATE, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	0, 0,
					CB_CATEGORY_ALPHANUMERIC, 0
  },
  { "MODULE-ID",			"cob_intr_module_id",
					CB_INTR_MODULE_ID, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	0, 0,
					CB_CATEGORY_ALPHANUMERIC, 0
  },
  { "MODULE-PATH",			"cob_intr_module_path",
					CB_INTR_MODULE_PATH, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	0, 0,
					CB_CATEGORY_ALPHANUMERIC, 0
  },
  { "MODULE-SOURCE",			"cob_intr_module_source",
					CB_INTR_MODULE_SOURCE, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	0, 0,
					CB_CATEGORY_ALPHANUMERIC, 0
  },
  { "MODULE-TIME",			"cob_intr_module_time",
					CB_INTR_MODULE_TIME, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	0, 0,
					CB_CATEGORY_NUMERIC, 0
  },
  { "MONETARY-DECIMAL-POINT",		"cob_intr_mon_decimal_point",
					CB_INTR_MON_DECIMAL_POINT, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	0, 0,
					CB_CATEGORY_ALPHANUMERIC, 0
  },
  { "MONETARY-THOUSANDS-SEPARATOR",	"cob_intr_mon_thousands_sep",
					CB_INTR_MON_THOUSANDS_SEP, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	0, 0,
					CB_CATEGORY_ALPHANUMERIC, 0
  },
  { "NATIONAL-OF",			"cob_intr_national_of",
					CB_INTR_NATIONAL_OF, NATIONAL_OF_FUNC,
					CB_FEATURE_NOT_IMPLEMENTED, 2, 1,
					CB_CATEGORY_NATIONAL, 1
  },
  { "NUMERIC-DECIMAL-POINT",		"cob_intr_num_decimal_point",
					CB_INTR_NUM_DECIMAL_POINT, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	0, 0,
					CB_CATEGORY_ALPHANUMERIC, 0
  },
  { "NUMERIC-THOUSANDS-SEPARATOR",	"cob_intr_num_thousands_sep",
					CB_INTR_NUM_THOUSANDS_SEP, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	0, 0,
					CB_CATEGORY_ALPHANUMERIC, 0
  },
  { "NUMVAL",				"cob_intr_numval",
					CB_INTR_NUMVAL, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "NUMVAL-C",				"cob_intr_numval_c",
					CB_INTR_NUMVAL_C, NUMVALC_FUNC,
					CB_FEATURE_ACTIVE,	2, 2,
					CB_CATEGORY_NUMERIC, 0
  },
  { "NUMVAL-F",				"cob_intr_numval_f",
					CB_INTR_NUMVAL_F, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "ORD",				"cob_intr_ord",
					CB_INTR_ORD, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "ORD-MAX",				"cob_intr_ord_max",
					CB_INTR_ORD_MAX, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	-1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "ORD-MIN",				"cob_intr_ord_min",
					CB_INTR_ORD_MIN, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	-1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "PI",				"cob_intr_pi",
					CB_INTR_PI, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	0, 0,
					CB_CATEGORY_NUMERIC, 0
  },
  { "PRESENT-VALUE",			"cob_intr_present_value",
					CB_INTR_PRESENT_VALUE, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	-1, 2,
					CB_CATEGORY_NUMERIC, 0
  },
  { "RANDOM",				"cob_intr_random",
					CB_INTR_RANDOM, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 0,
					CB_CATEGORY_NUMERIC, 0
  },
  { "RANGE",				"cob_intr_range",
					CB_INTR_RANGE, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	-1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "REM",				"cob_intr_rem",
					CB_INTR_REM, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	2, 2,
					CB_CATEGORY_NUMERIC, 0
  },
  { "REVERSE",				"cob_intr_reverse",
					CB_INTR_REVERSE, REVERSE_FUNC,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_ALPHANUMERIC, 1
					/* Note: category changed to national depending on the content,
					   see cb_build_intrinsic */
  },
  { "SECONDS-FROM-FORMATTED-TIME",	"cob_intr_seconds_from_formatted_time",
					CB_INTR_SECONDS_FROM_FORMATTED_TIME, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	2, 2,
					CB_CATEGORY_NUMERIC, 0
  },
  { "SECONDS-PAST-MIDNIGHT",		"cob_intr_seconds_past_midnight",
					CB_INTR_SECONDS_PAST_MIDNIGHT, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	0, 0,
					CB_CATEGORY_NUMERIC, 0
  },
  { "SIGN",				"cob_intr_sign",
					CB_INTR_SIGN, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "SIN",				"cob_intr_sin",
					CB_INTR_SIN, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "SQRT",				"cob_intr_sqrt",
					CB_INTR_SQRT, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "STANDARD-COMPARE",			"cob_intr_standard_compare",
					CB_INTR_STANDARD_COMPARE, FUNCTION_NAME,
					CB_FEATURE_NOT_IMPLEMENTED, 4, 2,
					CB_CATEGORY_ALPHANUMERIC, 0
  },
  { "STANDARD-DEVIATION",		"cob_intr_standard_deviation",
					CB_INTR_STANDARD_DEVIATION, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	-1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  /* GnuCOBOL */
  { "STORED-CHAR-LENGTH",		"cob_intr_stored_char_length",
					CB_INTR_STORED_CHAR_LENGTH, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  /* GnuCOBOL (added incompatible to COBOL 202x CCP 1.3) */
  { "SUBSTITUTE",			"cob_intr_substitute",
					CB_INTR_SUBSTITUTE, SUBSTITUTE_FUNC,
					CB_FEATURE_ACTIVE,	-1, 3,
					CB_CATEGORY_ALPHANUMERIC, 1
					/* Note: category changed to alphabetic/national depending on the content,
					   see cb_build_intrinsic */
  },
  /* GnuCOBOL */
  { "SUBSTITUTE-CASE",			"cob_intr_substitute_case",
					CB_INTR_SUBSTITUTE_CASE, SUBSTITUTE_CASE_FUNC,
					CB_FEATURE_ACTIVE,	-1, 3,
					CB_CATEGORY_ALPHANUMERIC, 1
					/* Note: category changed to alphabetic/national depending on the content,
					   see cb_build_intrinsic */
  },
  { "SUM",				"cob_intr_sum",
					CB_INTR_SUM, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	-1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "TAN",				"cob_intr_tan",
					CB_INTR_TAN, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "TEST-DATE-YYYYMMDD",		"cob_intr_test_date_yyyymmdd",
					CB_INTR_TEST_DATE_YYYYMMDD, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "TEST-DAY-YYYYDDD",			"cob_intr_test_day_yyyyddd",
					CB_INTR_TEST_DAY_YYYYDDD, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "TEST-FORMATTED-DATETIME",		"cob_intr_test_formatted_datetime",
					CB_INTR_TEST_FORMATTED_DATETIME, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	2, 2,
					CB_CATEGORY_NUMERIC, 0
  },
  { "TEST-NUMVAL",			"cob_intr_test_numval",
					CB_INTR_TEST_NUMVAL, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "TEST-NUMVAL-C",			"cob_intr_test_numval_c",
					CB_INTR_TEST_NUMVAL_C, NUMVALC_FUNC,
					CB_FEATURE_ACTIVE,	2, 2,
					CB_CATEGORY_NUMERIC, 0
  },
  { "TEST-NUMVAL-F",			"cob_intr_test_numval_f",
					CB_INTR_TEST_NUMVAL_F, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "TRIM",				"cob_intr_trim",
					CB_INTR_TRIM, TRIM_FUNC,
					CB_FEATURE_ACTIVE,	2, 1,
					CB_CATEGORY_ALPHANUMERIC, 1
					/* Note: category changed to national depending on the content,
					   see cb_build_intrinsic */
  },
  { "UPPER-CASE",			"cob_intr_upper_case",
					CB_INTR_UPPER_CASE, UPPER_CASE_FUNC,
					CB_FEATURE_ACTIVE,	1, 1,
					CB_CATEGORY_ALPHANUMERIC, 1
					/* Note: category changed to national depending on the content,
					   see cb_build_intrinsic */
  },
  { "VARIANCE",				"cob_intr_variance",
					CB_INTR_VARIANCE, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	-1, 1,
					CB_CATEGORY_NUMERIC, 0
  },
  { "WHEN-COMPILED",			"cob_intr_when_compiled",
					CB_INTR_WHEN_COMPILED, WHEN_COMPILED_FUNC,
					CB_FEATURE_ACTIVE,	0, 0,
					CB_CATEGORY_ALPHANUMERIC, 1
  },
  { "YEAR-TO-YYYY",			"cob_intr_year_to_yyyy",
					CB_INTR_YEAR_TO_YYYY, FUNCTION_NAME,
					CB_FEATURE_ACTIVE,	3, 1,
					CB_CATEGORY_NUMERIC, 0
  }
};

#define	NUM_INTRINSICS	sizeof(function_list) / sizeof(struct cb_intrinsic_table)

#ifdef	HAVE_DESIGNATED_INITS
static const unsigned char	cob_lower_tab[256] = {
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
#else
static unsigned char		cob_lower_tab[256];
static const unsigned char	pcob_lower_tab[] = "abcdefghijklmnopqrstuvwxyz";
static const unsigned char	pcob_lower_val[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
#endif

struct list_reserved_line {
        char *word_and_status;
	char *aliases;
};

/* Local functions */


/*
  Upper-casing for reserved words.
  We use cob_lower_tab instead of toupper for efficiency.
*/
static COB_INLINE COB_A_INLINE unsigned char
cb_toupper (unsigned char c)
{
	if (cob_lower_tab[c]) {
		return cob_lower_tab[c];
	}
	return c;
}

/* comparisions _for reserved words_ */
static int
cb_strcasecmp (const void *s1, const void *s2)
{
	const unsigned char	*p1;
	const unsigned char	*p2;
	unsigned char		c1;
	unsigned char		c2;

	p1 = (const unsigned char *)s1;
	p2 = (const unsigned char *)s2;

	for (;;) {
		c1 = cb_toupper (*p1++);
		c2 = cb_toupper (*p2++);

		if (c1 != c2) {
			return c1 < c2 ? -1 : 1;
		}
		if (!c1) {
			break;
		}
	}
	return 0;
}

static int
reserve_comp (const void *p1, const void *p2)
{
	/* For efficiency, we use strcmp here instead of cb_strcasecmp. */
	return strcmp(((struct cobc_reserved *)p1)->name,
		      ((struct cobc_reserved *)p2)->name);
}

static int
intrinsic_comp (const void *p1, const void *p2)
{
	/* For efficiency, we use strcmp here instead of cb_strcasecmp. */
	return strcmp (p1, ((struct cb_intrinsic_table *)p2)->name);
}

static const char *
res_get_feature (const enum cb_system_name_category category)
{
	const char	*s;

	switch (category) {
	case CB_DEVICE_NAME:
		s = _("device name");
		break;
	case CB_SWITCH_NAME:
		s = _("switch name");
		break;
	case CB_FEATURE_NAME:
		s = _("feature name");
		break;
	/* LCOV_EXCL_START */
	default:
		s = _("unknown");
		break;
	/* LCOV_EXCL_STOP */
	}
	return s;
}

static struct cobc_reserved
create_dummy_reserved (const char *word)
{
	struct cobc_reserved	ret;

	ret.name = word;
	ret.nodegen = 0;
	ret.context_sens = 0;
	ret.token = -1;
	ret.context_set = 0;
	ret.context_test = 0;

	return ret;
}

static void
free_amendment_content (struct amendment_list *to_free)
{
	cobc_main_free (to_free->word);
	if (to_free->alias_for) {
		cobc_main_free (to_free->alias_for);
	}
}

static void
free_amendment (struct amendment_list *to_free)
{
	free_amendment_content (to_free);
	cobc_main_free (to_free);
}

static COB_INLINE COB_A_INLINE int
has_context_sensitive_indicator (const char *word, const size_t size)
{
	return word[size - 1] == '*';
}

/*
  For reserved words/registers/intrinsics: Copy the first len characters of source,
  uppercased, to dest. We use cob_lower_tab instead of toupper for efficiency.
*/
void
cb_strncpy_upper (char *dest, const char * source, size_t len)
{
	while (len--) {
		*dest++ = cb_toupper(*source++);
	}
}

#if 0 /* unused */
/* in-place upper-casing for reserved words/registers/intrinsics */
static void
cb_strupr (char *s) {
	while (*s) {
		*s = cb_toupper (*s);
		s++;
	}
}
#endif

static void
allocate_upper_case_str (const char *word, const size_t size,
					   char ** const out_str)
{
	*out_str = cobc_main_malloc (size + 1U);
	cb_strncpy_upper (*out_str, word, size);
	(*out_str)[size] = '\0';
}

static void
allocate_upper_case_str_removing_asterisk (const char *word, const size_t size,
					   char ** const out_str)
{
	if (has_context_sensitive_indicator (word, size)) {
		/* Don't copy the trailing asterisk */
		allocate_upper_case_str (word, size - 1, out_str);
	} else {
		allocate_upper_case_str (word, size, out_str);
	}
}

static COB_INLINE COB_A_INLINE void
initialize_word (const char *word, const size_t size,
		 struct amendment_list * const reserved)
{
	allocate_upper_case_str_removing_asterisk (word, size, &reserved->word);
}

static int
is_invalid_word (const char *word, const int size, const int space_allowed,
		 const char *fname, const int line)
{
	/* FIXME: Should use the current (dialect specific) maximum word length,
	          not the absolute maximum, check order of reading and add test case */
	if (size > COB_MAX_WORDLEN) {
		configuration_error (fname, line, 1,
				     _("reserved word (%s) must have less than %d characters"),
				     word, COB_MAX_WORDLEN);
		return 1;
	}

	{
		size_t i = 0;
		while (i < size) {
			const char c = cb_toupper(word[i++]);
			if ((c >= 'A' && c <= 'Z')
			 || (c >= '0' && c <= '9')
			 ||  c == '-' || c == '_' ) {
				continue;
			}
			if (c == ' ' && space_allowed) {
				if (i == 1 || word[i-2] != ' ') {
					continue;
				}
			}
			configuration_error (fname, line, 1,
						 _("reserved word (%s) may not contain '%c'"), word, c);
			return 1;
		}
	}
	return 0;
}

static void
initialize_alias_for (const char *alias_for,
		      struct amendment_list * const reserved,
		      const char *fname, const int line)
{
	size_t	size = strlen (alias_for);

	if (has_context_sensitive_indicator (alias_for, size)) {
		size--;
		configuration_warning (fname, line,
			_("ignored asterisk at end of alias target"));
	}
	if (is_invalid_word (alias_for, size, 0, fname, line)) {
		reserved->alias_for = NULL;
	} else {
		allocate_upper_case_str_removing_asterisk (alias_for, size, &reserved->alias_for);
	}
}

static struct cobc_reserved *
search_reserved_list (const char * const word, const int needs_uppercasing,
		      const struct cobc_reserved * const list, size_t list_len)
{
	static char		upper_word[43];
	size_t			word_len;
	const char		*sought_word;
	struct cobc_reserved    to_find;

	if (needs_uppercasing) {
		word_len = strlen (word) + 1;
		if (word_len > sizeof(upper_word)) {
			return NULL;
		}

		/* copy including terminating byte */
		cb_strncpy_upper (upper_word, word, word_len);
		sought_word = upper_word;
	} else {
		sought_word = word;
	}

	to_find = create_dummy_reserved (sought_word);
	return bsearch (&to_find, list, list_len, sizeof (struct cobc_reserved),
			reserve_comp);
}

static struct cobc_reserved *
find_default_reserved_word (const char * const word, const int needs_uppercasing)
{
	return search_reserved_list (word, needs_uppercasing,
				     default_reserved_words,
				     NUM_DEFAULT_RESERVED_WORDS);
}

static struct cobc_reserved
get_user_specified_reserved_word (struct amendment_list user_reserved)
{
	struct cobc_reserved	cobc_reserved = create_dummy_reserved (NULL);
	struct cobc_reserved	*p;

	cobc_reserved.name = cobc_main_malloc (strlen (user_reserved.word) + 1);
	strcpy ((char *) cobc_reserved.name, user_reserved.word);

	if (!user_reserved.alias_for) {
		cobc_reserved.context_sens
			= !!user_reserved.is_context_sensitive;
	} else {
		p = find_default_reserved_word (user_reserved.alias_for, 0);
		if (p) {
			cobc_reserved.token = p->token;
		} else {
			/* FIXME: can we point to the fname originally defining the word? */
			configuration_error (NULL, 0, 1,
					     _("alias target '%s' is not a default reserved word"),
					     user_reserved.alias_for);
		}
	}

	return cobc_reserved;
}

static int
followed_by_addition_for_same_word (const struct amendment_list * const addition)
{
	struct amendment_list	*l;

	/* Walk through the list after the first addition. */
	for (l = addition->next; l; l = l->next) {
		/* Look for elements with the same word. */
		/* NB: We can use strcmp instead of cb_strcasecmp because
		   everything is already uppercase. */
		if (!strcmp (addition->word, l->word)
		    && l->to_add) {
			return 1;
		}
	}

	return 0;
}

/*
  Returns 1 if a removal for the same word as addition is found (and removed).
*/
static int
try_remove_removal (struct amendment_list * const addition)
{
	struct amendment_list	*l = addition->next;
	struct amendment_list	*prev = addition;

	while (l) {
		/* Look for elements with the same word. */
		if (strcmp (addition->word, l->word)) {
			prev = l;
			l = l->next;
			continue;
		}

		if (!l->to_add) {
			prev->next = l->next;
			free_amendment (l);
			l = prev->next;

			return 1;
		}
	}

	return 0;
}

/*
  Reduce the amendment list to a list of additions. Any removals which are not
  cancelled out are deleted.
*/
static void
reduce_amendment_list (struct amendment_list **amendment_list)
{
	struct amendment_list	*l = *amendment_list;
	struct amendment_list	*prev = NULL;
	struct amendment_list	*next;
	int	delete_current_elt = 0;

	while (l) {
		if (l->to_add) {
			/*
			  Later duplicate additions take overwrite previous ones
			  and a removal and the previous addition cancel each
			  other out.
			*/
			delete_current_elt = followed_by_addition_for_same_word (l)
				|| try_remove_removal (l);
		} else {
			delete_current_elt = 1;
		}

		if (delete_current_elt) {
			next = l->next;
			if (prev) {
				prev->next = next;
			}
			if (l == *amendment_list) {
				*amendment_list = next;
			}
			free_amendment (l);
			l = next;

			delete_current_elt = 0;
		} else {
			prev = l;
			l = l->next;
		}
	}
}

/* hash function for _reserved words/intrinsics/..._ (no extended letters!) */
static int
hash_word (const cob_c8_t *word, const cob_u32_t mod)
{
	cob_u32_t	result = 0x811c9dc5;

	/* Perform 32-bit FNV1a hash */
	for (; *word; ++word) {
		/* CHECKME: all input should be upper-case already, but isn't */
		result ^= cb_toupper (*word);
		result *= (cob_u32_t) 0x1677619;
	}

	return result % mod;
}

#define HASHMAP(type, type_struct, word_member)				\
	static struct type_struct	**type##_map;			\
	static size_t	type##_map_arr_size;				\
	static unsigned int	num_##type##s;					\
									\
	static void							\
	init_##type##_map (void)					\
	{								\
		type##_map_arr_size = 512;				\
		num_##type##s = 0;					\
		type##_map = cobc_main_malloc (type##_map_arr_size * sizeof (void *)); \
	}								\
									\
	static COB_INLINE COB_A_INLINE int				\
	type##_hash (const char *word)					\
	{								\
		return hash_word ((const cob_c8_t *) word, type##_map_arr_size); \
	}								\
									\
	static COB_INLINE COB_A_INLINE int				\
	next_##type##_key (const unsigned int key)				\
	{								\
		if (key < type##_map_arr_size - 1) {			\
			return key + 1;					\
		} else {						\
			return 0;					\
		}							\
	}								\
									\
	static unsigned int							\
	find_key_for_##type (const char * const word)			\
	{								\
		unsigned int key;						\
									\
		for (key = type##_hash (word);				\
			/* FIXME: we currently cannot use strcmp here instead of cb_strcasecmp. */ \
			type##_map[key] && cb_strcasecmp (type##_map[key]->word_member, word); \
			key = next_##type##_key (key));			\
									\
		return key;						\
	}								\
									\
	static void							\
	realloc_##type##_map (const size_t new_size)			\
	{								\
		struct type_struct	**new_map = cobc_main_malloc (new_size * sizeof(void *)); \
		struct type_struct	**old_map = type##_map;		\
		size_t	old_size = type##_map_arr_size;			\
		unsigned int	i;						\
		unsigned int	key;						\
									\
		type##_map_arr_size = new_size;				\
		type##_map = new_map;					\
									\
		for (i = 0; i < old_size; ++i) {			\
			if (old_map[i]) {				\
				key = find_key_for_##type (old_map[i]->word_member); \
				type##_map[key] = old_map[i];		\
			}						\
		}							\
									\
		cobc_main_free (old_map);				\
	}								\
									\
									\
	static void							\
	free_##type##_with_key (const int key)				\
	{								\
		cobc_main_free (type##_map[key]);			\
		type##_map[key] = NULL;					\
	}								\
									\
	static int							\
	add_##type##_to_map (const struct type_struct val, const int overwrite) \
	{								\
		unsigned int	key;					\
		int	entry_already_there;				\
									\
		if (!type##_map) {					\
			init_##type##_map ();				\
		}							\
		/*							\
		  The "- 1" is there so there is always one NULL entry in the \
		  array. If there is not one and the array is full,	\
		  find_##type will not terminate when given a word which \
		  shares a hash with a different word.			\
		*/							\
		if (num_##type##s == type##_map_arr_size - 1) {		\
			realloc_##type##_map (type##_map_arr_size * 2); \
		}							\
									\
		key = find_key_for_##type (val.word_member);		\
		entry_already_there = !!type##_map[key];		\
		if (entry_already_there) {				\
			if (overwrite) {				\
				free_##type##_with_key (key);		\
			} else {					\
				return 1;				\
			}						\
		} else {						\
			++num_##type##s;				\
		}							\
									\
		type##_map[key] = cobc_main_malloc (sizeof (struct type_struct)); \
		*type##_map[key] = val;					\
		return entry_already_there;				\
	}

HASHMAP (reserved_word, cobc_reserved, name)
HASHMAP (amendment, amendment_list, word)


/* These functions are separate to suppress "unused function" warnings. */
static void
remove_reserved_word_from_map (const char * const word)
{
	int	key = find_key_for_reserved_word (word);
	int	deleted_key;
	int	following_entry_key;

	if (!reserved_word_map[key]) {
		return;
	}

	free_reserved_word_with_key (key);

	/*
	  Check all subsequent entries do not need to be moved.  We can stop at
	  the fist NULL entry since if any word after wanted the deleted
	  entry's key, it would be in the NULL position.
	*/
	deleted_key = key;
	for (key = next_reserved_word_key (key); reserved_word_map[key]; key = next_reserved_word_key (key)) {
		following_entry_key = find_key_for_reserved_word (reserved_word_map[key]->name);
		if (following_entry_key == deleted_key) {
			reserved_word_map[deleted_key] = reserved_word_map[key];
			reserved_word_map[key] = NULL;
			deleted_key = key;
		} else if (following_entry_key == key) {
			continue;
		}
		/* LCOV_EXCL_START */
		else {
			/* Should not happen */
			COBC_ABORT ();
		}
		/* LCOV_EXCL_STOP */
	}
}

static struct cobc_reserved *
find_reserved_word (const char * const word)
{
	return reserved_word_map[find_key_for_reserved_word (word)];
}

static void
get_reserved_words_with_amendments (void)
{
	unsigned int	i;
	struct amendment_list	*amendment;
	unsigned int	key;
	struct cobc_reserved	reserved;
	struct cobc_reserved	*p;

	if (cb_reserved_words == NULL) {
		/*
		  Prepend the default reserved words to the amendment list as
		  additions.
		*/
		for (i = 0; i < NUM_DEFAULT_RESERVED_WORDS; ++i) {
			amendment = cobc_main_malloc (sizeof (struct amendment_list));
			amendment->word = cobc_main_strdup (default_reserved_words[i].name);
			amendment->to_add = 1;
			amendment->is_context_sensitive = default_reserved_words[i].context_sens;

			if (add_amendment_to_map (*amendment, 0)) {
				key = find_key_for_amendment (amendment->word);
				amendment->next = amendment_map[key];
				amendment_map[key] = amendment;
			}
		}
	}

	/*
	  Populate reserved_word_map with data from default_reserved_words,
	  where possible. Free each word once processed.
	*/
	for (i = 0; i < amendment_map_arr_size; ++i) {
		reduce_amendment_list (&amendment_map[i]);

		if (!amendment_map[i]) {
			continue;
		}

		p = find_default_reserved_word (amendment_map[i]->word, 0);
		if (p && !amendment_map[i]->alias_for) {
			reserved = *p;
			if (!amendment_map[i]->is_context_sensitive) {
				/*
				  We only preserve context-sensitivity if the
				  user specifically asks for it.
				*/
				reserved.context_sens = 0;
				reserved.context_test = 0;
			}
		} else {
			reserved = get_user_specified_reserved_word (*amendment_map[i]);
		}
		add_reserved_word_to_map (reserved, 0);

		free_amendment_content (amendment_map[i]);
	        free_amendment_with_key (i);
	}
}

static void
get_reserved_words_from_default_list (void)
{
	int	i;

	init_reserved_word_map ();

	for (i = 0; i < NUM_DEFAULT_RESERVED_WORDS; ++i) {
		add_reserved_word_to_map (default_reserved_words[i], 0);
	}
}

static void
initialize_reserved_words_if_needed (void)
{
	if (!reserved_word_map) {
		/* The default reserved words list should be sorted, but
		   assuming so causes abstruse errors when a word is put in the
		   wrong place (e.g. when dealing with EBCDIC or hyphens). */
		qsort (default_reserved_words, NUM_DEFAULT_RESERVED_WORDS,
		       sizeof (struct cobc_reserved), reserve_comp);

		if (num_amendments) {
			get_reserved_words_with_amendments ();
		} else {
			get_reserved_words_from_default_list ();
		}
	}
}

static int
list_line_cmp (const void *l, const void *r)
{
	return strcmp (((const struct list_reserved_line *)l)->word_and_status,
		       ((const struct list_reserved_line *)r)->word_and_status);
}

static int
strcmp_for_qsort (const void *l, const void *r)
{
	return strcmp ((const char *)l, (const char *)r);
}

static void
get_aliases (const unsigned int key, struct list_reserved_line *line)
{
	int	given_token = reserved_word_map[key]->token;
	unsigned int	i;
	unsigned int	j;
	unsigned int	num_aliases = 0;
	unsigned int	aliases_str_len = 0;
	char	(*aliases)[COB_MAX_WORDLEN + 1];
	char	*aliases_str;

	if (given_token <= 0) {
		line->aliases = NULL;
		return;
	}

	/* Count number of aliases and their total length. */
	for (i = 0; i < reserved_word_map_arr_size; ++i) {
		if (i != key
		    && reserved_word_map[i]
		    && reserved_word_map[i]->token == given_token) {
			++num_aliases;
			aliases_str_len += strlen (reserved_word_map[i]->name);
		}
	}

	if (num_aliases == 0) {
		return;
	}

	/* Create array of aliases, then sort it. */
	aliases = cobc_malloc ((size_t)num_aliases * (COB_MAX_WORDLEN + 1) * sizeof (char));
	j = 0;
	for (i = 0; i < reserved_word_map_arr_size; ++i) {
		if (i != key
		    && reserved_word_map[i]
		    && reserved_word_map[i]->token == given_token) {
			strncpy (aliases[j], reserved_word_map[i]->name,
				 COB_MAX_WORDLEN);
			++j;
		}
	}
	qsort (aliases, num_aliases, COB_MAX_WORDLEN + 1, &strcmp_for_qsort);

	/* Build aliases string */
	aliases_str = cobc_malloc (strlen ("(aliased with ")
				   + aliases_str_len
				   + (num_aliases - 1) * strlen (", ")
				   + strlen (")")
				   + 1);
	strcpy (aliases_str, "(aliased with ");
	for (j = 0; j < num_aliases; ++j) {
		if (j != 0) {
			strcat (aliases_str, ", ");
		}
		strcat (aliases_str, aliases[j]);
	}
	strcat (aliases_str, ")");
	cobc_free (aliases);
	line->aliases = aliases_str;
}

/* Global functions */

/* TO-DO: Duplication with lookup_reserved_word */
int
is_reserved_word (const char *word)
{
	initialize_reserved_words_if_needed ();
	return !!find_reserved_word (word);
}

int
is_default_reserved_word (const char *word)
{
	return !!find_default_reserved_word (word, 1);
}

void
remove_context_sensitivity (const char *word, const int context)
{
	struct cobc_reserved *reserved =
		find_default_reserved_word (word, 1);

	if (reserved) {
		reserved->context_test ^= context;
	}
}

cb_tree
get_system_name (const char *name)
{
	struct system_name_struct *system_name = lookup_system_name (name, 0);

	if (system_name != NULL) {
		return cb_build_system_name (system_name->category,
			system_name->token);
	}
	return NULL;
}

/* get system name, revert word-combination of scanner.l,
   if necessary (e.g. SWITCH A <--> SWITCH_A) */
cb_tree
get_system_name_translated (cb_tree word)
{
	char system_name[COB_MAX_WORDLEN + 1];
	cb_tree res;

	system_name[COB_MAX_WORDLEN] = 0;
	strncpy(system_name, CB_NAME (word), COB_MAX_WORDLEN);
	if (system_name [6] == '_') {
		system_name [6] = ' ';
	}

	res = get_system_name(system_name);
	if (!res) {
		cb_error_x (word, _("invalid system-name '%s'"), system_name);
	}

	return res;
}

static void
append_amendment_at_word (struct amendment_list amendment)
{
	int	key;
	struct amendment_list	*l;

	if (add_amendment_to_map (amendment, 0)) {
		/*
		  If there is already an amendment for this word, append the
		  amendment to the word's amendment list.
		*/
		key = find_key_for_amendment (amendment.word);
		for (l = amendment_map[key]; l->next; l = l->next);
		l->next = cobc_main_malloc (sizeof (struct amendment_list));
		*(l->next) = amendment;
	}
}

/*
  parameter *word has no white space, may include context sensitive indicator
  and/or alias definition: a* a=b a*=b

  *word is a static char * when line < 0 !
*/
static void
add_amendment (const char *word, const char *fname, const int line,
	       const int to_add)
{
	struct amendment_list	amendment;
	size_t			size;
	char			*equal_sign_pos;
	int			context_sensitive;

	/* Check for alias and context sensitive indicator,
	   get and check the length of the word */
	equal_sign_pos = strchr (word, '=');
	if (equal_sign_pos) {
		size = equal_sign_pos - word;
	} else {
		size = strlen (word);
	}
	context_sensitive = has_context_sensitive_indicator (word, size);
	if (context_sensitive) {
		size--;
	}

	/*
	  Only verify entries that don't come from the default word list. Line 0
	  means the entry came from the command line. Line -1 means it came from
	  the default word list.
	*/
	if (line >= 0 && is_invalid_word (word, size, 0, fname, line)) {
		return;
	}

	amendment.is_context_sensitive = context_sensitive;
	amendment.to_add = to_add;
	amendment.next = NULL;
	initialize_word (word, size, &amendment);

	/* If it is an alias, copy what it is an alias for */
	if (to_add && equal_sign_pos) {
		initialize_alias_for (equal_sign_pos + 1, &amendment, fname,
				      line);
	} else {
		amendment.alias_for = NULL;
	}

	append_amendment_at_word (amendment);
}

void
add_reserved_word (const char *word, const char *fname, const int line)
{
	char upper_word[COB_MAX_WORDLEN + 1];
	size_t word_len = strlen (word) + 1;
	if (word_len > sizeof (upper_word)) {
		return;
	}
	cb_strncpy_upper (upper_word, word, word_len);
	add_amendment (upper_word, fname, line, 1);
}

static void
remove_reserved_word_internal (const char *upper_word, const char *fname, const int line)
{
	add_amendment (upper_word, fname, -1, 0);	/* "line" -1 as we don't want any check here */
}

void
remove_reserved_word (const char *word, const char *fname, const int line)
{
	char upper_word[COB_MAX_WORDLEN + 1];
	size_t word_len = strlen(word) + 1;
	if (word_len > sizeof (upper_word)) {
		return;
	}
	cb_strncpy_upper (upper_word, word, word_len);
	remove_reserved_word_internal(upper_word, fname, word_len);

	add_amendment (upper_word, fname, line, 0);
}

/* add reserved word to the current list, called as "target" of
   reserved word directives */
void
add_reserved_word_now (char * const word, char * const alias_for)
{
	struct amendment_list	amendment;

	/* Nothing to do if the word is already reserved */
	if (is_reserved_word (word)) {
		return;
	}

	/* LCOV_EXCL_START */
	if (alias_for && !is_default_reserved_word (alias_for)) {
		/* Should not happen */
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */

	amendment.word = word;
	amendment.alias_for = alias_for;
	amendment.is_context_sensitive = 0;
	amendment.to_add = 1;
	add_reserved_word_to_map (get_user_specified_reserved_word (amendment), 0);
}

void
remove_reserved_word_now (const char * const word)
{
	remove_reserved_word_from_map (word);
}

/* lookup function for the scanner to get the reserved-word toke
   for a given word (or NULL) */
struct cobc_reserved *
lookup_reserved_word (const char *name)
{
	struct cobc_reserved	*p;

	initialize_reserved_words_if_needed ();

	p = find_reserved_word (name);
	if (!p) {
		return NULL;
	}

	/* Check word is implemented */
	if (unlikely(p->token <= 0)) {
		/* Not implemented - If context sensitive, no error */
		if (!p->context_sens) {
			cb_error (_("'%s' is a reserved word, but isn't supported"), name);
		}
		return NULL;
	}

	/* Special actions / Context sensitive */
	if (p->context_set) {
		if (unlikely(p->context_test)) {
			/* Dependent words */
			if (!(cobc_cs_check & p->context_test)) {
				return NULL;
			}
		}
		cobc_cs_check |= p->context_set;
		return p;
	}

	if (p->context_test) {
		if (!(cobc_cs_check & p->context_test)) {
			return NULL;
		}
		/*
		  The only context-sensitive phrases outside the procedure division
		  we expect to manually reset cobc_cs_check are OPTIONS, SELECT,
		  I-O-CONTROL and SCREEN.

		  Note: Everything in the environment and identification division can
		  (and does) reset cobc_cs_check.
		*/
		if (!cobc_in_procedure
		 && !(cobc_cs_check & CB_CS_OPTIONS)
		 && !(cobc_cs_check & CB_CS_SELECT)
		 && !(cobc_cs_check & CB_CS_I_O_CONTROL)
		 && !(cobc_cs_check & CB_CS_SCREEN)) {
			cobc_cs_check = 0;
		}
		return p;
	}

	if (p->token == FUNCTION_ID) {
		cobc_cs_check = 0;
		cobc_force_literal = 1;
	} else if (p->token == INTRINSIC) {
		if (!cobc_in_repository) {
			return NULL;
		}
	} else if (p->token == PROGRAM_ID) {
		cobc_cs_check = CB_CS_PROGRAM_ID;
		cobc_force_literal = 1;
	} else if (p->token == REPOSITORY) {
		cobc_in_repository = 1;
	}

	return p;
}

struct cb_intrinsic_table *
lookup_intrinsic (const char *name, const int checkimpl)
{
	struct cb_intrinsic_table	*cbp;
	static char	upper_name[43];
	size_t		name_len = strlen (name);

	if (name_len > sizeof (upper_name) - 1) {
		return NULL;
	}

	/* copy including terminating byte */
	cb_strncpy_upper (upper_name, name, name_len + 1);

	cbp = bsearch (upper_name, function_list, NUM_INTRINSICS,
		sizeof (struct cb_intrinsic_table), intrinsic_comp);
	if (cbp && (checkimpl || cbp->active == CB_FEATURE_ACTIVE)) {
		return cbp;
	}
	return NULL;
}

static void
set_intrinsic_mode (struct cb_intrinsic_table *cbp, enum cb_feature_mode mode)
{
	/* FIXME: doesn't cater for not implemented -> disabled -> active [should be not implemented again] */
	if (cbp->active == CB_FEATURE_NOT_IMPLEMENTED && mode == CB_FEATURE_ACTIVE) {
		return;
	}
	cbp->active = mode;
}

static void
change_intrinsic (const char *name, const char *fname, const int line, enum cb_feature_mode mode)
{
	struct cb_intrinsic_table *cbp;
	size_t		i;

	/* Group "ALL" intrinsics */
	if (cb_strcasecmp (name, "DIALECT-ALL") == 0) {
		for (i = 0; i < NUM_INTRINSICS; ++i) {
			set_intrinsic_mode (&function_list[i], mode);
		}
		return;
	}

	cbp = lookup_intrinsic (name, 1);
	if (!cbp) {
		if (mode == CB_FEATURE_ACTIVE) {
			configuration_error (fname, line, 1, _("intrinsic function %s is unknown"), name);
		}
		return;
	}
	set_intrinsic_mode (cbp, mode);
}

void
activate_intrinsic (const char *name, const char *fname, const int line)
{
	change_intrinsic (name, fname, line, CB_FEATURE_ACTIVE);
}

void
deactivate_intrinsic (const char *name, const char *fname, const int line)
{
	change_intrinsic (name, fname, line, CB_FEATURE_DISABLED);
}

void
cb_list_intrinsics (void)
{
	const char	*t;
	char	argnum [20];
	size_t		i;

	putchar ('\n');
	printf ("%-32s%-16s%s\n",
		_("Intrinsic Function"), _("Implemented"), _("Parameters"));
	for (i = 0; i < NUM_INTRINSICS; ++i) {
		switch (function_list[i].active) {
		case CB_FEATURE_ACTIVE:
			t = _("Yes");
			break;
		case CB_FEATURE_NOT_IMPLEMENTED:
			t = _("No");
			break;
		default: /* CB_FEATURE_DISABLED */
			continue;
		}
		if (function_list[i].args == -1) {
			snprintf (argnum, sizeof (argnum) - 1, "%s", _("Unlimited"));
		} else if (function_list[i].args != function_list[i].min_args) {
			snprintf (argnum, sizeof (argnum) - 1, "%d - %d",
				(int)function_list[i].min_args, (int)function_list[i].args);
		} else {
			snprintf (argnum, sizeof (argnum) - 1, "%d", (int)function_list[i].args);
		}
		printf ("%-32s%-16s%s\n", function_list[i].name, t, argnum);
	}
}

void
cb_list_exceptions (void)
{
	size_t		i;

	putchar ('\n');
	printf ("%-32s", _("Exception Name"));	/* more to add later */
	for (i = COB_EC_ALL; i < cb_exception_table_len - 1; ++i) {
		if (i == COB_EC_ALL) {
			/* EC-ALL - level-1 EC to set all ECs, no indent */
			printf ("\n%s", CB_EXCEPTION_NAME (i));
		} else if ((CB_EXCEPTION_CODE(i) & 0x00FF) == 0) {
			/* EC level-2 EC, newline and indent by 2 */
			printf ("\n  %-26s", CB_EXCEPTION_NAME (i));
		} else {
			/* individual level-3 EC, including fatal marker */
			printf ("\n    %s%s", CB_EXCEPTION_NAME (i), CB_EXCEPTION_FATAL (i) ? " (f)" : "");
		}
	}
	putchar ('\n');
}

static struct register_struct *
lookup_register_internal (const char *upper_name, const int checkimpl)
{
	size_t		i;

	for (i = 0; i < NUM_REGISTERS; ++i) {
		/* For efficiency, we use strcmp instead of cb_strcasecmp. */
		if (strcmp (register_list[i].name, upper_name) == 0) {
			if (checkimpl || register_list[i].active != CB_FEATURE_MUST_BE_ENABLED) {
				return &register_list[i];
			}
			break;
		}
	}
	return NULL;
}

static struct register_struct *
lookup_register (const char *name, const int checkimpl)
{
	static char	upper_name[43];
	size_t		name_len = strlen (name);

	if (name_len > sizeof (upper_name) - 1) {
		return NULL;
	}

	/* copy including terminating byte */
	cb_strncpy_upper (upper_name, name, name_len + 1);
	return lookup_register_internal (upper_name, checkimpl);
}

/* add an entry to the register list, currently the definition is ignored,
   TODO: check definition and add a new special register accordingly */

void
add_register (const char *name_and_definition, const char *fname, const int line)
{
	const char	*name = name_and_definition;
	int		i;
	char		*definition;
	struct register_struct *special_register;
	char upper_name[COB_MAX_WORDLEN + 1];
	size_t name_len;

	/* Enable all registers, if requested. */
	if (cb_strcasecmp (name, "DIALECT-ALL") == 0) {
		for (i = 0; i < NUM_REGISTERS; ++i) {
			if (register_list[i].active != CB_FEATURE_MUST_BE_ENABLED) {
				/* TODO: add register here */
				register_list[i].active = CB_FEATURE_ACTIVE;
				/* Disable reserved word with same name. */
				remove_reserved_word_internal (register_list[i].name, fname, line);
			}
		}
		return;
	}

	/* Otherwise enable a named register. */

	/* note: we don't break at space as this would kill "ADDRESS OF"
	         and "PIC 9(05) USAGE ..." */
	definition = strpbrk (name_and_definition, "\t:=");
	if (definition) {
		*definition++ = 0;
	}

	name_len = strlen (name);
	if (is_invalid_word (name, name_len, 1, fname, line)) {
		return;
	}
	/* copy including terminating byte */
	cb_strncpy_upper (upper_name, name, name_len + 1);

	special_register = lookup_register_internal (upper_name, 1);
	if (!special_register) {
		if (!definition || *definition == 0) {
			configuration_error (fname, line, 1,
				_("special register %s is unknown, needs a definition"), name);
			return;
		}
#if 0	/* must be extended and tested before use... */
		cb_build_generic_register (name, definition);
#else
		configuration_error (fname, line, 1, _("special register %s is unknown"), name);
#endif
		return;
	}
	special_register->active = CB_FEATURE_ACTIVE;

	/* Disable reserved word with same name. */
	remove_reserved_word_internal (name, fname, line);
}

void
remove_register (const char *name, const char *fname, const int line)
{
	struct register_struct	*special_register;
	int	i;

	COB_UNUSED (fname);
	COB_UNUSED (line);

	if (cb_strcasecmp (name, "DIALECT-ALL") == 0) {
		for (i = 0; i < NUM_REGISTERS; ++i) {
			if (register_list[i].active != CB_FEATURE_MUST_BE_ENABLED) {
				/* TODO: when user-defined registers are possible: do
				   memory cleanup here */
				register_list[i].active = CB_FEATURE_DISABLED;
				/* Disable reserved word with same name. */
				remove_reserved_word_internal (register_list[i].name, fname,
					line);
			}
		}
	} else {
		if (strlen (name) > COB_MAX_WORDLEN) {
			return;
		}
		special_register = lookup_register (name, 1);
		if (!special_register) {
			return;
		}
		/* TODO: when user-defined registers are possible: do memory
		   cleanup here */
		special_register->active = CB_FEATURE_DISABLED;
		/* Disable reserved word with same name. */
		remove_reserved_word_internal (special_register->name, fname, line);
	}
}

const char *
cb_register_list_get_first (const char **definition)
{
	current_register = 0;
	return cb_register_list_get_next (definition);
}

const char *
cb_register_list_get_next (const char **definition)
{
	for (; current_register < NUM_REGISTERS; ++current_register) {
		if (register_list[current_register].active == CB_FEATURE_ACTIVE) {
			*definition = register_list[current_register].definition;
			return register_list[current_register++].name;
		}
	}
	return NULL;
}

const char *
cb_get_register_definition (const char *name)
{
	struct register_struct *special_register = lookup_register (name, 0);

	if (!special_register) {
		return NULL;
	}
	return special_register->definition;
}

void
cb_list_registers (void)
{
	size_t		i;
	const char	*name, *t;
	char name_display[COB_MINI_BUFF];

	/* TODO: implement creation from user-specified list (currently only enable/disable)
	   Note: will still be able to be referenced if not implemented,
	         but not set/read by libcob [still helps compilation but should raise a warning]
	*/

	putchar ('\n');
	printf ("%-32s%-16s%s\n",
		_("Internal registers"), _("Implemented"), _("Definition"));
	for (i = 0; i < NUM_REGISTERS; ++i) {
		switch (register_list[i].active) {
		case CB_FEATURE_ACTIVE:
			t = _("Yes");
			break;
		case CB_FEATURE_NOT_IMPLEMENTED:
			t = _("No");
			break;
		default: /* CB_FEATURE_DISABLED */
			continue;
		}
		if (strcmp (register_list[i].name, "LENGTH OF") != 0
		 && strcmp (register_list[i].name, "ADDRESS OF") != 0) {
			name = register_list[i].name;
		} else {
			snprintf (name_display, COB_MINI_MAX, "'%s' phrase", register_list[i].name);
			name = (const char *)&name_display;
		}
		printf ("%-32s%-16s%s\n", name, t, register_list[i].definition);
	}
}

static struct system_name_struct *
lookup_system_name (const char *name, const int checkimpl)
{
	size_t		i;

	for (i = 0; i < SYSTEM_TAB_SIZE; ++i) {
		if (cb_strcasecmp (system_name_table[i].name, name) == 0) {
			if (checkimpl || system_name_table[i].active != CB_FEATURE_DISABLED) {
				return &system_name_table[i];
			}
			break;
		}
	}
	return NULL;
}

static void
set_system_name_mode (struct system_name_struct *system_name, enum cb_feature_mode mode)
{
	/* FIXME: doesn't cater for not implemented -> disabled -> active [should be not implemented again] */
	if (system_name->active == CB_FEATURE_NOT_IMPLEMENTED && mode == CB_FEATURE_ACTIVE) {
		return;
	}
	system_name->active = mode;
}

static void
change_system_name (const char *name, const char *fname, const int line, enum cb_feature_mode mode)
{
	struct system_name_struct *system_name;
	size_t		i;


	/* some predefined groups first */
	if (cb_strcasecmp (name, "DIALECT-ALL") == 0) {
		for (i = 0; i < SYSTEM_TAB_SIZE; ++i) {
			set_system_name_mode (&system_name_table[i], mode);
		}
		return;
	} else if (cb_strcasecmp (name, "DIALECT-ALL-DEVICES") == 0) {
		for (i = 0; i < SYSTEM_TAB_SIZE; ++i) {
			if (system_name_table[i].category == CB_DEVICE_NAME) {
				set_system_name_mode (&system_name_table[i], mode);
			}
		}
		return;
	} else if (cb_strcasecmp (name, "DIALECT-ALL-SWITCHES") == 0) {
		for (i = 0; i < SYSTEM_TAB_SIZE; ++i) {
			if (system_name_table[i].category == CB_SWITCH_NAME) {
				set_system_name_mode (&system_name_table[i], mode);
			}
		}
		return;
	} else if (cb_strcasecmp (name, "DIALECT-ALL-FEATURES") == 0) {
		for (i = 0; i < SYSTEM_TAB_SIZE; ++i) {
			if (system_name_table[i].category == CB_FEATURE_NAME) {
				set_system_name_mode (&system_name_table[i], mode);
			}
		}
		return;
	}

	system_name = lookup_system_name (name, 1);
	if (!system_name) {
		if (mode == CB_FEATURE_ACTIVE) {
			configuration_error (fname, line, 1, _("unknown system-name '%s'"), name);
		}
		return;
	}
	set_system_name_mode (system_name, mode);
}

void
activate_system_name (const char *name, const char *fname, const int line)
{
	change_system_name (name, fname, line, CB_FEATURE_ACTIVE);
}

void
deactivate_system_name (const char *name, const char *fname, const int line)
{
	change_system_name (name, fname, line, CB_FEATURE_DISABLED);
}

void
cb_list_system_names (void)
{
	const char	*feature;
	size_t		i;

	putchar ('\n');
	puts (_("System names"));
	for (i = 0; i < SYSTEM_TAB_SIZE; ++i) {
		if (system_name_table[i].active == CB_FEATURE_DISABLED) {
			continue;
		}
		feature = res_get_feature (system_name_table[i].category);
		printf ("%-32s%s\n", system_name_table[i].name, feature);
	}
}


void
cb_list_reserved (void)
{
	struct list_reserved_line	*word_descriptions;
	const char	*p;
	size_t		i;
	size_t		j;

	initialize_reserved_words_if_needed ();

	/* Output header */
	putchar ('\n');
	printf ("%-32s%s\n", _("Reserved Words"), _("Implemented"));

	/* Build list of reserved words */
	word_descriptions = cobc_malloc (num_reserved_words * sizeof (struct list_reserved_line));
	j = -1;	/* planned integer overflow with the +1 below expected to be zero */
	for (i = 0; i < num_reserved_words; ++i) {
		do {
			++j;
		} while (!reserved_word_map[j]);

		if (reserved_word_map[j]->token > 0) {
			if (reserved_word_map[j]->context_sens) {
				p = _("Yes (Context sensitive)");
			} else {
				p = _("Yes");
			}
		} else {
			/* specify part of DEBUG-ITEM special register as implemented, if the register is there */
			if (strncmp("DEBUG-", reserved_word_map[j]->name, 6) == 0
			 && lookup_register_internal ("DEBUG-ITEM", 0)) {
				p = _("Yes");
			} else if (reserved_word_map[j]->context_sens) {
				p = _("No (Context sensitive)");
			} else {
				p = _("No");
			}
		}
		word_descriptions[i].word_and_status = cobc_malloc (COB_MAX_WORDLEN + 1);
		snprintf (word_descriptions[i].word_and_status, COB_MAX_WORDLEN,
			  "%-32s%s", reserved_word_map[j]->name, p);
		get_aliases (j, &word_descriptions[i]);
	}

	/* Display sorted list with aliases. */
	qsort (word_descriptions, num_reserved_words,
	       sizeof (struct list_reserved_line), &list_line_cmp);
	for (i = 0; i < num_reserved_words; ++i) {
		printf ("%s", word_descriptions[i].word_and_status);
		cobc_free (word_descriptions[i].word_and_status);
		if (word_descriptions[i].aliases) {
			printf (" %s", word_descriptions[i].aliases);
			cobc_free (word_descriptions[i].aliases);
		}
		putchar ('\n');
	}
	cobc_free (word_descriptions);

	/* note: starts with an empty line */
	cb_list_registers ();
}

#ifndef	HAVE_DESIGNATED_INITS
void
cobc_init_reserved (void)
{
	const unsigned char	*p;
	const unsigned char	*v;

	memset (cob_lower_tab, 0, sizeof(cob_lower_tab));
	p = pcob_lower_tab;
	v = pcob_lower_val;
	for (; *p; ++p, ++v) {
		cob_lower_tab[*p] = *v;
	}
}
#endif

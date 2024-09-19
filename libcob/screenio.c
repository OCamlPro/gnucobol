/*
   Copyright (C) 2001-2012, 2014-2024 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Simon Sobisch, Edward Hart,
   Chuck Haatvedt

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


#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef	HAVE_LOCALE_H
#include <locale.h>
#endif

#ifdef	_WIN32
#include <io.h>
#endif

#if defined (HAVE_NCURSESW_NCURSES_H)
#include <ncursesw/ncurses.h>
#include <ncursesw/panel.h>
#define WITH_EXTENDED_SCREENIO
#define WITH_PANELS
#elif defined (HAVE_NCURSESW_CURSES_H)
#include <ncursesw/curses.h>
#include <ncursesw/panel.h>
#define WITH_EXTENDED_SCREENIO
#define WITH_PANELS
#elif defined (HAVE_NCURSES_H)
#include <ncurses.h>
#include <panel.h>
#define WITH_EXTENDED_SCREENIO
#define WITH_PANELS
#elif defined (HAVE_NCURSES_NCURSES_H)
#include <ncurses/ncurses.h>
#include <ncurses/panel.h>
#define WITH_EXTENDED_SCREENIO
#define WITH_PANELS
#elif defined (HAVE_PDCURSES_H)
#define PDC_NCMOUSE		/* use ncurses compatible mouse API */
#include <pdcurses.h>
#include <panel.h>
#define WITH_EXTENDED_SCREENIO
#define WITH_PANELS
#elif defined (HAVE_PDCURSES_CURSES_H)
#define PDC_NCMOUSE		/* use ncurses compatible mouse API */
#include <pdcurses/curses.h>
#include <panel.h>
#define WITH_EXTENDED_SCREENIO
#define WITH_PANELS
#elif defined (HAVE_XCURSES_H)
#define PDC_NCMOUSE		/* use ncurses compatible mouse API */
#include <xcurses.h>
#define WITH_EXTENDED_SCREENIO
#elif defined (HAVE_XCURSES_CURSES_H)
#define PDC_NCMOUSE		/* use ncurses compatible mouse API */
#include <xcurses/curses.h>
#define WITH_EXTENDED_SCREENIO
#elif defined (HAVE_CURSES_H)
#define PDC_NCMOUSE	/* see comment above */
#include <curses.h>
#ifndef PDC_MOUSE_MOVED
#undef PDC_NCMOUSE
#endif
#define WITH_EXTENDED_SCREENIO
#endif

#if defined (__PDCURSES__)
/* Note: PDC will internally define NCURSES_MOUSE_VERSION with
   a recent version when PDC_NCMOUSE was defined;
   for older version define manually! */
#endif

/* work around broken system headers or compile flags defining
   NCURSES_WIDECHAR / PDC_WIDE but not including the actual definitions */
#if defined (NCURSES_WIDECHAR) && !defined (WACS_HLINE)
#undef NCURSES_WIDECHAR
#endif
#if defined (PDC_WIDE) && !defined (WACS_HLINE)
#undef PDC_WIDE
#endif
#if defined (NCURSES_WIDECHAR) || defined (PDC_WIDE)
#define WITH_WIDE_FUNCTIONS
#endif

/* include internal and external libcob definitions, forcing exports */
#define	COB_LIB_EXPIMP
#include "coblocal.h"

#ifdef	HAVE_CURSES_FREEALL
extern void	_nc_freeall (void);
#endif
#ifdef HAVE_MOUSEMASK
static mmask_t 	cob_mask_accept;	/* mask that is returned to COBOL ACCEPT */
static mmask_t 	cob_mask_routine;	/* mask that is returned to COBOL routines (reserved) */
#if defined BUTTON5_PRESSED	/* added in NCURSES_MOUSE_VERSION 2 */
#define COB_HAS_MOUSEWHEEL 1
#else
#undef COB_HAS_MOUSEWHEEL
#endif
#endif

struct cob_inp_struct {
	cob_screen		*scr;
	size_t			up_index;
	size_t			down_index;
	int			this_y;
	int			this_x;
};

enum screen_statement {
	ACCEPT_STATEMENT,
	DISPLAY_STATEMENT
};

#define	COB_INP_FLD_MAX		512U

#define	COB_INP_SIZE	(COB_INP_FLD_MAX * sizeof(struct cob_inp_struct))

#define	COB_CH_UL		((const chtype)'_')
#define	COB_CH_SP		((const chtype)' ')
#define	COB_CH_AS		((const chtype)'*')

/* Local variables */

static cob_global		*cobglobptr;
static cob_settings		*cobsetptr;

/* Local variables when screenio activated */

#ifdef	WITH_EXTENDED_SCREENIO
static const cob_field_attr	const_alpha_attr =
				{COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
static struct cob_inp_struct	*cob_base_inp;
static size_t			totl_index;
static size_t			cob_has_color;
static int			global_return;
static int			cob_current_y;
static int			cob_current_x;
static short		fore_color	/* "const" default foreground (pair 0 on init) */;
static short		back_color	/* "const" default background (pair 0 on init) */;
static short		stdscr_fore_color	/* "const" default foreground (pair 0 on init) */;
static short		stdscr_back_color	/* "const" default background (pair 0 on init) */;
static int			origin_y;
static int			origin_x;
static int			display_cursor_y;
static int			display_cursor_x;
static int			accept_cursor_y;
static int			accept_cursor_x;
static int			pending_accept;
static int			got_sys_char;
static int			save_cursor_x = 0;
static int			save_cursor_y = 0;
static unsigned int	curr_setting_insert_mode = INT_MAX;
static WINDOW		*mywin;

#ifdef WITH_PANELS
#define MAX_PANELS		20
#define CBL_NEW_WINDOW			1
#define CBL_DELETE_WINDOW		2
#define CBL_MOVE_WINDOW			3
#define CBL_SHOW_WINDOW			4
#define CBL_HIDE_WINDOW			5
#define CBL_TOP_WINDOW			6
#define CBL_BOTTOM_WINDOW		7

typedef struct	__PANEL_ENTRY
	{
		PANEL 		*pan;
		WINDOW		*win;
		int 		starty;
		int 		startx;
		int 		win_rows;
		int 		win_cols;
		int 		hidden;
		short		fg_color;
		short		bg_color;
		int 		pan_number;
	} PANEL_ENTRY,		*PPANEL_ENTRY;

typedef struct	__PARM_FLD
	{
		int 		pan_number;
		int 		starty;
		int 		startx;
		int 		win_rows;
		int 		win_cols;
		int			fg_color;
		int			bg_color;
		int 		hidden;
		int 		ret_code;
	} PARM_FLD, 	*PPARM_FLD;

	static PANEL_ENTRY	my_panels[MAX_PANELS] =
	{ { NULL, NULL, 0, 0, 0, 0, 0, 0, 0, 1} ,
		{ NULL, NULL, 0, 0, 0, 0, 0, 0, 0, 2} ,
		{ NULL, NULL, 0, 0, 0, 0, 0, 0, 0, 3} ,
		{ NULL, NULL, 0, 0, 0, 0, 0, 0, 0, 4} ,
		{ NULL, NULL, 0, 0, 0, 0, 0, 0, 0, 5} ,
		{ NULL, NULL, 0, 0, 0, 0, 0, 0, 0, 6} ,
		{ NULL, NULL, 0, 0, 0, 0, 0, 0, 0, 7} ,
		{ NULL, NULL, 0, 0, 0, 0, 0, 0, 0, 8} ,
		{ NULL, NULL, 0, 0, 0, 0, 0, 0, 0, 9} ,
		{ NULL, NULL, 0, 0, 0, 0, 0, 0, 0, 10} ,
		{ NULL, NULL, 0, 0, 0, 0, 0, 0, 0, 11} ,
		{ NULL, NULL, 0, 0, 0, 0, 0, 0, 0, 12} ,
		{ NULL, NULL, 0, 0, 0, 0, 0, 0, 0, 13} ,
		{ NULL, NULL, 0, 0, 0, 0, 0, 0, 0, 14} ,
		{ NULL, NULL, 0, 0, 0, 0, 0, 0, 0, 15} ,
		{ NULL, NULL, 0, 0, 0, 0, 0, 0, 0, 16} ,
		{ NULL, NULL, 0, 0, 0, 0, 0, 0, 0, 17} ,
		{ NULL, NULL, 0, 0, 0, 0, 0, 0, 0, 18} ,
		{ NULL, NULL, 0, 0, 0, 0, 0, 0, 0, 19} ,
		{ NULL, NULL, 0, 0, 0, 0, 0, 0, 0, 20 } };


#endif

#ifdef HAVE_MOUSEMASK
static unsigned int	curr_setting_mouse_flags = UINT_MAX;
#endif
#endif

/* Local function prototypes */

static int cob_screen_init	(void);

int check_panel_parm (int parm_type, PPARM_FLD parm);

void cob_update_all_windows (void);
/* Local functions */

/*
 * Cf. GNU cpp info manual, section 7, Pragmas
 *
 * Define a macro to use the _Pragma operator, restricted to modern gcc.
 * This section is experimental.  If it proves useful, it should be moved
 * to coblocal.h.
 *
 * The ICC guard is there because ICC, clang and many others
 * "helpfully" define __GNUC__, but often leave out some extensions.
 */
#if defined(__GNUC__) && ! defined(__ICC) && \
 (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 4))
#  define GCC_PRAGMA(w)				\
   _Pragma ("GCC diagnostic push")		\
   _Pragma (#w)
#  define GCC_POP()   _Pragma ("GCC diagnostic pop")
#else
#  define  GCC_PRAGMA(w)
#  define  GCC_POP()
#endif

GCC_PRAGMA( GCC diagnostic ignored "-Wunused-result" )
static void
cob_speaker_beep (void)
{
	int	fd;

	fd = fileno (stdout);
	if (fd >= 0) {
		(void)write (fd, "\a", (size_t)1);
	}
}
GCC_POP()

static COB_INLINE COB_A_INLINE void
init_cob_screen_if_needed (void)
{
	if (!cobglobptr) {
		cob_fatal_error (COB_FERROR_INITIALIZED);
	}
	if (!cobglobptr->cob_screen_initialized) {
		int ret = cob_screen_init ();
		if (ret) {
			/* possibly adjust all callers to raise an exception */
			cob_hard_failure ();
		}
	}
}

static void
get_crt3_status (int fret, unsigned char *crtstat)
{
	crtstat[0] = '0';
	crtstat[1] = '\0';
	crtstat[2] = '\0';

	switch (fret) {
	case 0:	/* OK */
		crtstat[0] = '0';
		crtstat[1] = '0';
		break;

	case 2005:	/* ESC */
		crtstat[0] = '1';
		crtstat[1] = '\0';
		break;

	case 8000:	/* NO_FIELD */
	case 9001:	/* MAX_FIELD */
		crtstat[0] = '9';
		crtstat[1] = '\0';
		break;

	case 8001:	/* TIMEOUT, CHECKME */
		crtstat[0] = '9';
		crtstat[1] = '\1';
		break;

	/* TODO: more case COB_SCR_... */

	default:
		if (fret >= 1001 && fret <= 1064) {
			/* function keys */
			crtstat[0] = '1';
			crtstat[1] = (unsigned char)(fret - 1000);
		} else if (fret >= 2001 && fret <= 2110) {
			/* exception keys */
			crtstat[0] = '2';
			crtstat[1] = (unsigned char)(fret - 2000);
		}
	}
}

static void
cob_set_crt3_status (cob_field* status_field, int fret)
{
	unsigned char	crtstat[3];
	get_crt3_status (fret, crtstat);

	memcpy (status_field->data, crtstat, 3);
}

static void
handle_status (const int fret, const enum screen_statement stmt)
{
	if (fret) {
		cob_set_exception (stmt == ACCEPT_STATEMENT ?
			COB_EC_IMP_ACCEPT : COB_EC_IMP_DISPLAY);
	}
	COB_ACCEPT_STATUS = fret;

	if (COB_MODULE_PTR && COB_MODULE_PTR->crt_status) {
		cob_field	*status_field = COB_MODULE_PTR->crt_status;
		if (COB_FIELD_IS_NUMERIC (status_field)) {
			cob_set_int (status_field, fret);
		} else if (status_field->size == 3) {
			cob_set_crt3_status (status_field, fret);
		} else {
			char	buff[23]; /* 10: make the compiler happy as "int" *could*
						         have more digits than we "assume" */
			sprintf (buff, "%4.4d", fret);
			memcpy (status_field->data, buff, 4U);
		}
	}
}

#ifdef	WITH_EXTENDED_SCREENIO

static COB_INLINE COB_A_INLINE void
refresh_mywin (WINDOW *curr_win)
{
	int		pos_y, pos_x;

	if (curr_win != stdscr) {
		getyx (curr_win, pos_y, pos_x);
		if (wmouse_trafo(curr_win, &pos_y, &pos_x, 1)) {
			move(pos_y, pos_x);
		}
	}

	wrefresh(curr_win);
	return;
}

static void
cob_beep (void)
{
	switch (COB_BEEP_VALUE) {
	case 1:
		(void)flash ();
		return;
	case 2:
		cob_speaker_beep ();
		return;
	case 9:
		return;
	default:
		(void)beep ();
		return;
	}
}

static void
raise_ec_on_invalid_line_or_col (const int line, const int column)
{
	int	max_y;
	int	max_x;

	getmaxyx (mywin, max_y, max_x);
	if (line < 0 || line >= max_y) {
		cob_set_exception (COB_EC_SCREEN_LINE_NUMBER);
	}
	if (column < 0 || column >= max_x) {
		cob_set_exception (COB_EC_SCREEN_STARTING_COLUMN);
	}
}

static int
cob_move_cursor (const int line, const int column)
{
	int status = wmove (mywin ,line, column);

	if (status == ERR) {
		raise_ec_on_invalid_line_or_col (line, column);
	}
	return status;
}

void
cob_set_cursor_pos (int line, int column)
{
	init_cob_screen_if_needed ();
	(void) wmove (mywin ,line, column);
}

#if 0 /* currently unused */
static void
cob_move_to_beg_of_last_line (void)
{
	int	max_y;
	int	max_x;

	getmaxyx (mywin, max_y, max_x);
	/* We don't need to check for exceptions here; it will always be fine */
	wmove (mywin ,max_y, 0);

	COB_UNUSED (max_x);
}
#endif


static short
cob_get_color_pair (const short fg_color, const short bg_color)
{
	/* default color (defined from terminal, read during init ) */
	if (fg_color == stdscr_fore_color && bg_color == stdscr_back_color) {
		return 0;
	}
	/* reserved color "all black", defined during init */
	if (fg_color == 0 && bg_color == 0) {
		return 1;
	}

	{
		/* some implementations (especially PDCursesMod 64-bit CHTYPE)
		   provide more color pairs than we currently support, limit appropriate */
		const short	max_clr_pairs = COLOR_PAIRS < SHRT_MAX ? (short)COLOR_PAIRS : SHRT_MAX - 1;
		short	color_pair_number;
		short	fg_defined, bg_defined;

		for (color_pair_number = 2; color_pair_number < max_clr_pairs; color_pair_number++) {

			pair_content (color_pair_number, &fg_defined, &bg_defined);

			/* check if we've already defined this color pair */
			if (fg_defined == fg_color && bg_defined == bg_color) {
				return color_pair_number;
			}

			/* check if we found a spare pair, defined as requested  */
			if (fg_defined == 0 && bg_defined == 0) {
				init_pair (color_pair_number, fg_color, bg_color);
				return color_pair_number;
			}
		}
	}

	/* none left - return default */
	return 0;
}

static int
cob_activate_color_pair (const short color_pair_number)
{
	int ret;

#ifdef	HAVE_COLOR_SET
	ret = wcolor_set (mywin ,color_pair_number, NULL);
#else
	ret = wattrset (mywin, COLOR_PAIR(color_pair_number));
#endif
	wbkgdset (mywin ,COLOR_PAIR(color_pair_number));

	return ret;
}


static int
cob_to_curses_color (const int cob_color, short *curses_color)
{
	if (cob_color < 0
	 || cob_color > 15) {
		/* "invalid" color - nothing to do */
		return -1;
	}

	/* compat for MF/ACU/... only use first 3 bits -> 0-7,
	   bit 4 is "included highlight/blink attribute" */
	switch (cob_color & 7) {
	case COB_SCREEN_BLACK:
		*curses_color = COLOR_BLACK;
		return 0;
	case COB_SCREEN_BLUE:
		*curses_color = COLOR_BLUE;
		return 0;
	case COB_SCREEN_GREEN:
		*curses_color = COLOR_GREEN;
		return 0;
	case COB_SCREEN_CYAN:
		*curses_color = COLOR_CYAN;
		return 0;
	case COB_SCREEN_RED:
		*curses_color = COLOR_RED;
		return 0;
	case COB_SCREEN_MAGENTA:
		*curses_color = COLOR_MAGENTA;
		return 0;
	case COB_SCREEN_YELLOW:
		*curses_color = COLOR_YELLOW;
		return 0;
	case COB_SCREEN_WHITE:
		*curses_color = COLOR_WHITE;
		return 0;
	default:
		return -1;
	}
}

/* compat for MF/ACU/... only first 3 bits are colors -> 0-7,
   bit 4 is "included highlight/blink attribute" -> an "extended" color */
static void
adjust_attr_from_extended_color (cob_flags_t *attr, const int color,
		const int handle_background)
{
	/* check for "valid" color and "extended attribute" set - return */
	if  (color >= 0
	 && (color & 8)) {
		if (handle_background) {
			*attr |= COB_SCREEN_BLINK;
		} else {
			*attr |= COB_SCREEN_HIGHLIGHT;
		}
	}
}

/* adjust screenio attributes and color values from numeric attributes
   added together in the COLOR field */
static void
adjust_attr_from_color_field (cob_flags_t *attr, cob_field *color,
	short *fg_color, short *bg_color)
{
	int col_attr = cob_get_int (color);	/* added numeric value */

	if (col_attr >= 131072) {
		col_attr -= 131072;
		/* BACKGROUND-HIGH pending */
	}
	if (col_attr >= 65536) {
		col_attr -= 65536;
		/* BACKGROUND-LOW pending */
	}
	if (col_attr >= 32768) {
		col_attr -= 32768;
		/* Protected - only relevant for SCREEN SECTION items
		   to have them temporarily as "output only"*/
	}
	if (col_attr >= 16384) {
		col_attr -= 16384;
		*attr |= COB_SCREEN_BLINK;
	}
	if (col_attr >= 8192) {
		col_attr -= 8192;
		*attr |= COB_SCREEN_UNDERLINE;
	}
	if (col_attr >= 4096) {
		col_attr -= 4096;
		*attr |= COB_SCREEN_HIGHLIGHT;
	}
	if (col_attr >= 2048) {
		col_attr -= 2048;
		*attr |= COB_SCREEN_LOWLIGHT;
	}
	if (col_attr >= 1024) {
		col_attr -= 1024;
		*attr |= COB_SCREEN_REVERSE;
	}

	if (col_attr >= 256) {
		col_attr -= 256;
		*bg_color = COLOR_WHITE;
	} else if (col_attr >= 224) {
		col_attr -= 224;
		*bg_color = COLOR_YELLOW;
	} else if (col_attr >= 192) {
		col_attr -= 192;
		*bg_color = COLOR_MAGENTA;
	} else if (col_attr >= 160) {
		col_attr -= 160;
		*bg_color = COLOR_RED;
	} else if (col_attr >= 128) {
		col_attr -= 128;
		*bg_color = COB_SCREEN_CYAN;
	} else if (col_attr >= 96) {
		col_attr -= 96;
		*bg_color = COLOR_GREEN;
	} else if (col_attr >= 64) {
		col_attr -= 64;
		*bg_color = COLOR_BLUE;
	} else if (col_attr >= 32) {
		col_attr -= 32;
		*bg_color = COLOR_BLACK;
	}

	if (col_attr >= 8) {
		col_attr -= 8;
		*fg_color = COLOR_WHITE;
	} else if (col_attr >= 7) {
		col_attr -= 7;
		*fg_color = COLOR_YELLOW;
	} else if (col_attr >= 6) {
		col_attr -= 6;
		*fg_color = COLOR_MAGENTA;
	} else if (col_attr >= 5) {
		col_attr -= 5;
		*fg_color = COLOR_RED;
	} else if (col_attr >= 4) {
		col_attr -= 4;
		*fg_color = COB_SCREEN_CYAN;
	} else if (col_attr >= 3) {
		col_attr -= 3;
		*fg_color = COLOR_GREEN;
	} else if (col_attr >= 2) {
		col_attr -= 2;
		*fg_color = COLOR_BLUE;
	} else if (col_attr >= 1) {
		col_attr -= 1;
		*fg_color = COLOR_BLACK;
	}
}

static int
get_cob_color_from_color_value (const char *p, const size_t len)
{
	/* translate number */
	{
		char *endptr;
		const int cob_color = strtol (p, &endptr, 10);
		/* number parsed - return as is */
		if (endptr != p) {
			return cob_color;
		}
	}

	/* text translation */
	{
		if (len == 5) {
			if (memcmp (p, "BLACK", 5) == 0) {
				return COB_SCREEN_BLACK;
			}
			if (memcmp (p, "WHITE", 5) == 0) {
				return COB_SCREEN_WHITE;
			}
			if (memcmp (p, "GREEN", 5) == 0) {
				return COB_SCREEN_GREEN;
			}
			return -1;
		}
		if (len == 4) {
			if (memcmp (p, "BLUE", 4) == 0) {
				return COB_SCREEN_BLUE;
			}
			if (memcmp (p, "CYAN", 4) == 0) {
				return COB_SCREEN_CYAN;
			}
			return -1;
		}
		if (len == 3) {
			if (memcmp (p, "RED", 3) == 0) {
				return COB_SCREEN_RED;
			}
			return -1;
		}
		if (len == 7) {
			if (memcmp (p, "MAGENTA", 7) == 0) {
				return COB_SCREEN_MAGENTA;
			}
			return -1;
		}
		if (len == 6 && memcmp (p, "YELLOW", 6) == 0) {
			return COB_SCREEN_YELLOW;
		}
	}

	return -1;
}

/* parse COBOL color name / number and set curses color number + attribute accordingly */
static int
handle_control_field_color (cob_flags_t *attr, const char *p, size_t len,
	short *curses_color, const int handle_background)
{
	const int cob_color = get_cob_color_from_color_value (p, len);
	int short curses_color_val;

	/* translate to curses, arly error return if not possible */
	if (cob_to_curses_color (cob_color, &curses_color_val) < 0) {
		return -1;
	}

	/* take over color */
	*curses_color = curses_color_val;

	/* attribute via renamed colors */
	adjust_attr_from_extended_color (attr, cob_color, handle_background);

	return 0;
}

struct parse_control
{
	const char *keyword;
	cob_flags_t     cobflag;
};

/* binary sorted list of known attribute names and their value,
   note : the actual list is compiler specific, we support all */
static struct parse_control control_attrs[] = {
	{ "AUTO"                , COB_SCREEN_AUTO         } ,
	{ "AUTO-SKIP"           , COB_SCREEN_AUTO         } ,
	{ "BACKGROUND-COLOR"    , 0                       } ,
	{ "BACKGROUND-COLOUR"   , 0                       } ,
	{ "BCOLOR"              , 0                       } ,
	{ "BEEP"                , COB_SCREEN_BELL         } ,
	{ "BELL"                , COB_SCREEN_BELL         } ,
	{ "BLANK LINE"          , COB_SCREEN_BLANK_LINE   } ,
	{ "BLANK SCREEN"        , COB_SCREEN_BLANK_SCREEN } ,
	{ "BLINK"               , COB_SCREEN_BLINK        } ,
	{ "CONVERT"             , COB_SCREEN_CONV         } ,
	{ "ECHO"                , COB_SCREEN_NO_ECHO      } ,
	{ "EMPTY-CHECK"         , -1                      } ,
	{ "ERASE EOL"           , COB_SCREEN_ERASE_EOL    } ,
	{ "ERASE EOS"           , COB_SCREEN_ERASE_EOS    } ,
	{ "FCOLOR"              , 0                       } ,
	{ "FOREGROUND-COLOR"    , 0                       } ,
	{ "FOREGROUND-COLOUR"   , 0                       } ,
	{ "FULL"                , COB_SCREEN_FULL         } ,
	{ "GRAPHICS"            , COB_SCREEN_GRAPHICS     } ,
	{ "GRID"                , COB_SCREEN_GRID         } ,
	{ "HIGH"                , COB_SCREEN_HIGHLIGHT    } ,
	{ "HIGHLIGHT"           , COB_SCREEN_HIGHLIGHT    } ,
	{ "JUST"                , -1                      } ,
	{ "JUSTIFY"             , -1                      } ,
	{ "LEFTLINE"            , COB_SCREEN_LEFTLINE     } ,
	{ "LENGTH-CHECK"        , COB_SCREEN_FULL         } ,
	{ "LOW"                 , COB_SCREEN_LOWLIGHT     } ,
	{ "LOWER"               , COB_SCREEN_LOWER        } ,
	{ "LOWLIGHT"            , COB_SCREEN_LOWLIGHT     } ,
	{ "NO-ECHO"             , COB_SCREEN_SECURE       } ,
	{ "OFF"                 , COB_SCREEN_SECURE       } ,
	{ "OVERLINE"            , COB_SCREEN_OVERLINE     } ,
	{ "PROMPT"              , COB_SCREEN_PROMPT       } ,
	{ "PROTECT"             , COB_SCREEN_NO_UPDATE    } ,
	{ "REQUIRED"            , COB_SCREEN_REQUIRED     } ,
	{ "REVERSE"             , COB_SCREEN_REVERSE      } ,
	{ "REVERSE-VIDEO"       , COB_SCREEN_REVERSE      } ,
	{ "RIGHT-JUSTIFY"       , -1                      } ,
	{ "RIGHTLINE"           , COB_SCREEN_RIGHTLINE    } ,	/* GC extension */
	{ "TAB"                 , COB_SCREEN_TAB          } ,
	{ "TRAILING"            , -1                      } ,
	{ "TRAILING-SIGN"       , -1                      } ,
	{ "UNDERLINE"           , COB_SCREEN_UNDERLINE    } ,
	{ "UPDATE"              , COB_SCREEN_UPDATE       } ,
	{ "UPPER"               , COB_SCREEN_UPPER        } ,
	{ "ZERO-FILL"           , -1                      }
};

#define COB_NUM_PCTRLS	sizeof(control_attrs) / sizeof(control_attrs[0])

static int
screen_attr_cmp (const void *p1, const void *p2)
{
	return strcmp (p1, ((struct parse_control *)p2)->keyword);
}

/* adjust screenio attributes and color values from named attributes
   in CONTROL field */
static void
adjust_attr_from_control_field (cob_flags_t *attr, cob_field *control,
	short *fg_color, short *bg_color)
{
	const char *token[COB_MINI_BUFF] = { 0 };	/* token positions */
	char buffer[COB_MEDIUM_BUFF];		/* token buffer */
	unsigned int  max_tokens, i;

	/* load CONTROL attribute as upper-case into buffer, then tokenize */
	{
		const char *token_deli = ",; \n\r\t";
		const char *p;
		char *q;
		cob_field_to_string (control, buffer, COB_LARGE_MAX, CCM_UPPER);

		i = 0;
		p = strtok (buffer, token_deli);
		if (p == NULL) {
			/* no token - early exit */
			return;
		}
		while (p != NULL) {
			if (i == COB_MINI_MAX) {
				break;
			}

			/* if one "resolved token" contains an equal sign
			   then push as multiple tokens */
			if (p[1] != 0
			 && (q = strchr (p, '=')) != NULL) {
				if (p != q) {
					/* not first entry - push first part */
					token[i++] = p;
					if (i == COB_MINI_MAX) {
						break;
					}
					*q = 0;
				}
				/* push equal sign */
				token[i++] = "=";

				/* setup for next iteration by new start position */
				p = q + 1;
				if (*p == 0) {
					p++;
					if (*p == 0) {
						break;
					}
				}
				continue;
			}

			token[i++] = p;
			p = strtok (NULL, token_deli);
		}
		max_tokens = i;
	}

	/* now parse the tokens */
	for (i = 0; i < max_tokens; i++) {
		const char *keyword = token[i];
		size_t len = strlen (keyword);

		int  no_indicator = 0, bg_color_token = 0;

		/* negation */
		if (len == 2 && memcmp (keyword, "NO", 2) == 0) {
			if (++i == max_tokens) {
				break;
			}
			keyword = token[i];
			len = strlen (keyword);
			no_indicator = 1;
		}

		/* two-token keywords */
		if (len == 5 && memcmp (keyword, "ERASE", 5) == 0) {
			if (++i == max_tokens) {
				break;
			}
			keyword = token[i];
			len = strlen (keyword);
			if (len == 3 && memcmp (keyword, "EOL", 3) == 0) {
				keyword = "ERASE EOL";
			} else
			if (len == 3 && memcmp (keyword, "EOS", 3) == 0) {
				keyword = "ERASE EOS";
			} else {
				continue;
			}
		}
		if (len == 5 && memcmp (keyword, "BLANK", 5) == 0) {
			if (++i == max_tokens) {
				break;
			}
			keyword = token[i];
			len = strlen (keyword);
			if (len == 4 && memcmp (keyword, "LINE", 4) == 0) {
				keyword = "BLANK LINE";
			} else
			if (len == 6 && memcmp (keyword, "SCREEN", 6) == 0) {
				keyword = "BLANK SCREEN";
			} else {
				continue;
			}
		}

		/* find token and get its attribute */
		{

			const struct parse_control *control_attr = bsearch (keyword,
				control_attrs, COB_NUM_PCTRLS, sizeof (struct parse_control),
				screen_attr_cmp);

			/* skip unknown / not implemented control attributes */
			if (control_attr == NULL
			 || control_attr->cobflag == -1) {
				continue;
			}

			/* normal attribute - apply and go on*/
			if (control_attr->cobflag != 0) {
				if (no_indicator == 0) {
					*attr |= control_attr->cobflag;
				} else {
					*attr &= ~(control_attr->cobflag);
				}
				continue;
			}

		}

		/* color attribute - check next token */
		bg_color_token = *keyword == 'B';
		if (++i == max_tokens) {
			break;
		}
		keyword = token[i];
		len = strlen (keyword);

		/* skip optional IS / = token */
		if ((len == 2 && memcmp (keyword, "IS", 2) == 0)
		 || (len == 1 && *keyword == '=')) {
			if (++i == max_tokens) {
				break;
			}
			keyword = token[i];
			len = strlen (keyword);
		}

		/* parse and handle color keyword */
		{
			int ret;
			if (bg_color_token) {
				ret = handle_control_field_color (attr, keyword, len, bg_color, 1);
			} else {
				ret = handle_control_field_color (attr, keyword, len, fg_color, 0);
			}
			/* if we could not parse the keyword as color value, then ignore
			   the color setting and put the non-color keyword back on the stack */
			if (ret != 0) {
				i--;
			}
		}

	}
}

static cob_flags_t
cob_screen_attr (cob_field *fgc, cob_field *bgc, cob_flags_t attr,
		 cob_field *control, cob_field *color, const enum screen_statement stmt)
{
	int		line;
	int		column;
	const int	cob_fg_color = fgc ? cob_get_int (fgc) : -1;
	const int	cob_bg_color = bgc ? cob_get_int (bgc) : -1;
	short		fg_color = -1;
	short		bg_color = -1;
	chtype		styles = A_NORMAL;

	/* pre-set color value */
	cob_to_curses_color (cob_fg_color, &fg_color);
	cob_to_curses_color (cob_bg_color, &bg_color);

	/* attribute via renamed colors */
	adjust_attr_from_extended_color (&attr, cob_fg_color, 0);
	adjust_attr_from_extended_color (&attr, cob_bg_color, 1);

	/* ACU / RM? extension that may override colors + some attributes */
	if (color) {
		adjust_attr_from_color_field (&attr, color, &fg_color, &bg_color);
	}

	/* CONTROL - extension to override attributes and colors */
	if (control) {
		adjust_attr_from_control_field (&attr, control, &fg_color, &bg_color);
	}

	/* curses attributes from (possibly adjusted) COBOL attributes;
	   note that several "may be ignored if not supported by the terminal"
	   and that OVERLINE / LEFTLINE / RIGHTLINE is a curses extension
	   in general */
	if (attr & COB_SCREEN_REVERSE) {
		styles |= A_REVERSE;
	}
	if (attr & COB_SCREEN_HIGHLIGHT) {
		styles |= A_BOLD;
	}
	if (attr & COB_SCREEN_LOWLIGHT) {
		styles |= A_DIM;
	}
	if (attr & COB_SCREEN_BLINK) {
		styles |= A_BLINK;
	}
	if (attr & COB_SCREEN_UNDERLINE) {
		styles |= A_UNDERLINE;
	}
#if defined (A_OVERLINE)
	if (attr & COB_SCREEN_OVERLINE) {
		styles |= A_OVERLINE;
	}
#endif
#if defined (A_LEFTLINE)
	if (attr & COB_SCREEN_LEFTLINE) {
		styles |= A_LEFTLINE;
	}
#endif
#if defined (A_RIGHTLINE)
	if (attr & COB_SCREEN_RIGHTLINE) {
		styles |= A_RIGHTLINE;
	}
#endif

	/* apply attributes */
	wattrset (mywin ,A_NORMAL);
	if (styles != A_NORMAL) {
		wattron (mywin ,styles);
	}

	/* apply colors */
	if (cob_has_color) {
		short		color_pair_number;
		if (fg_color == -1) {
			fg_color = fore_color;
		}
		if (bg_color == -1) {
			bg_color = back_color;
		}
		color_pair_number = cob_get_color_pair (fg_color, bg_color);
		cob_activate_color_pair (color_pair_number);
	}
	/* BLANK SCREEN colors the whole screen. */
	if (attr & COB_SCREEN_BLANK_SCREEN) {
		getyx (mywin, line, column);
		wclear (mywin);
		cob_move_cursor (line, column);
	}

	if (stmt == DISPLAY_STATEMENT) {
		/* BLANK LINE colors the whole line. */
		if (attr & COB_SCREEN_BLANK_LINE) {
			getyx (mywin, line, column);
			cob_move_cursor (line, 0);
			wclrtoeol (mywin);
			cob_move_cursor (line, column);
		}
		if (attr & COB_SCREEN_ERASE_EOL) {
			wclrtoeol (mywin);
		}
		if (attr & COB_SCREEN_ERASE_EOS) {
			wclrtobot (mywin);
		}
	}
	if (attr & COB_SCREEN_BELL) {
		cob_beep ();
	}
	return attr;
}

static int
cob_screen_init (void)
{
	if (cobglobptr->cob_screen_initialized) {
		return 0;
	}

	cob_base_inp = NULL;
	totl_index = 0;
	cob_has_color = 0;
	global_return = 0;
	cob_current_y = 0;
	cob_current_x = 0;
	fore_color = 0;
	back_color = 0;
	display_cursor_y = 0;
	display_cursor_x = 0;
	accept_cursor_y = 0;
	accept_cursor_x = 0;
	pending_accept = 0;
	got_sys_char = 0;

	fflush (stdout);
	fflush (stderr);

#if	0	/* RXWRXW sigtin */
#ifndef _WIN32
	/* If the process is backgrounded (running non interactively), */
	/* SIGTTIN or SIGTOU is emitted. If not caught, turns into a SIGSTP */
#ifdef	SIGTTIN
	signal(SIGTTIN, SIG_IGN);
#endif
#ifdef	SIGTTOU
	signal(SIGTTOU, SIG_IGN);
#endif
#endif
#endif

#if	0	/* RXWRXW - setlocale */
#ifdef	HAVE_SETLOCALE
	if (cobglobptr->cob_locale_orig) {
		setlocale (LC_ALL, cobglobptr->cob_locale_orig);
	}
	if (cobglobptr->cob_locale_ctype) {
		setlocale (LC_CTYPE, cobglobptr->cob_locale_ctype);
	}
#endif
#endif

	if (!initscr ()) {
		cob_runtime_error (_("failed to initialize curses"));
		return 1;
	}

	/* set mywin pointer to mywin pointer for future 
		implementation of panels functionality 
		Note that stdscr WINDOW pointer can not 
		be altered as the keyboard and mouse 
		still require this pointer */
	mywin = stdscr;

	cobglobptr->cob_screen_initialized = 1;
#ifdef	HAVE_USE_LEGACY_CODING
	use_legacy_coding (2);
#endif

#if	0	/* RXWRXW - setlocale */
#ifdef	HAVE_SETLOCALE
	if (cobglobptr->cob_locale) {
		setlocale (LC_ALL, cobglobptr->cob_locale);
		setlocale (LC_CTYPE, "C");
	}
#endif
#endif

	cbreak ();
	keypad (mywin, 1);
	nonl ();
	noecho ();
	if (has_colors ()) {
		start_color ();
		pair_content ((short)0, &fore_color, &back_color);
		stdscr_fore_color = fore_color;
		stdscr_back_color = back_color;
		/* fix bad settings of the terminal on start */
		if (fore_color == back_color) {
			if (fore_color == COLOR_BLACK) {
				fore_color = COLOR_WHITE;
			} else {
				back_color = COLOR_BLACK;
			}
			init_pair ((short)0, fore_color, back_color);
		}
		if (COLOR_PAIRS > 1) {
			cob_has_color = 1;
			/* explicit reserve pair 1 as all zero as we take this as "initialized" later on */
			init_pair ((short)1, 0, 0);
#ifdef __PDCURSES__
			/* pdcurses wincon + vt sets *ALL* pairs to default fg/bg of the terminal,
			   while ncurses initializes them to zero;
			   set all to zero here, allowing us to adjust them later as necessary */
			{
				/* some implementations (especially PDCursesMod 64-bit CHTYPE)
				   provide more color pairs than we currently support, limit appropriate */
				const short	max_clr_pairs = COLOR_PAIRS < SHRT_MAX ? (short)COLOR_PAIRS : SHRT_MAX - 1;
				short	color_pair_number;

				for (color_pair_number = 2; color_pair_number < max_clr_pairs; ++color_pair_number) {
					init_pair (color_pair_number, 0, 0);
					if (color_pair_number == SHRT_MAX) {
						break;
					}
				}
			}
#if defined (PDC_BUILD) && PDC_BUILD >= 3501
			if (!cobsetptr->cob_legacy) {
				PDC_set_blink (1);
				PDC_set_bold (1);
			}
#endif
#endif
		}
	}
	wattrset (mywin ,A_NORMAL);
	getmaxyx (mywin, COB_MAX_Y_COORD, COB_MAX_X_COORD);

	cob_settings_screenio ();

	/* Possible alternative definitions for ALT Keys */
#ifndef ALT_DEL
#	ifdef kDC3
#		define ALT_DEL   kDC3
#	endif
#endif
#ifndef ALT_LEFT
#	ifdef kLFT3
#		define ALT_LEFT   kLFT3
#	endif
#endif
#ifndef ALT_RIGHT
#	ifdef kRIT3
#		define ALT_RIGHT   kRIT3
#	endif
#endif

	/* When still missing - self define the keys */
	/* note: if define_key is not available rhe user will have to manually
	   assign terminfo values for the control strings to the given
	   KEY_MAX + n / __KEY_MIN - n values */

#ifndef HAVE_DEFINE_KEY
#define define_key(x,y)	/* do nothing */
#endif

#if defined (KEY_MAX) && KEY_MAX > 0
#define COB_NEW_KEY(n)		(KEY_MAX + n)
#elif defined (__KEY_MIN) && __KEY_MIN < 0
#define COB_NEW_KEY(n)		(__KEY_MIN - n)
#else
#ifdef HAVE_DEFINE_KEY
#error "Did not find a valid value for key definition. Please report this!"
#endif
#endif

#ifdef COB_NEW_KEY
#ifndef ALT_DEL
#define ALT_DEL                 COB_NEW_KEY(1)
	define_key("\033[3;3~", ALT_DEL);
#endif
#ifndef ALT_LEFT
#define ALT_LEFT                COB_NEW_KEY(2)
	define_key("\033[1;3D", ALT_LEFT);
#endif
#ifndef ALT_RIGHT
#define ALT_RIGHT               COB_NEW_KEY(3)
	define_key("\033[1;3C", ALT_RIGHT);
#endif
#else
#ifndef ALT_DEL
#define ALT_DEL                 KEY_SDC
#endif
#ifndef ALT_LEFT
#define ALT_LEFT                KEY_SLEFT
#endif
#ifndef ALT_RIGHT
#define ALT_RIGHT               KEY_ALEFT
#endif
#endif
	return 0;
}

static void
cob_convert_key (int *keyp, const cob_u32_t field_accept)
{
	/* Map key to KEY_xxx value */
	switch (*keyp) {
	case '\n':
	case '\r':
	case '\004':
	case '\032':
		*keyp = KEY_ENTER;
		break;
	case '\t':
		*keyp = KEY_STAB;
		break;
	case '\b':
	case 0177:
		*keyp = KEY_BACKSPACE;
		break;
	case KEY_EOL:
		*keyp = ALT_DEL;
		break;
	case KEY_CLOSE:
		*keyp = ALT_LEFT;
		break;
	case KEY_PREVIOUS:
		*keyp = ALT_LEFT;
		break;

#ifdef	KEY_A1
	/* A1, A3, C1, C3 are always present if A1 is defined */
	case KEY_A1:
		*keyp = KEY_HOME;
		break;
	case KEY_A3:
		*keyp = KEY_PPAGE;
		break;
	case KEY_C1:
		*keyp = KEY_END;
		break;
	case KEY_C3:
		*keyp = KEY_NPAGE;
		break;
	/* Any or all of A2, B1-3, C2 MAY be present */
	/* Note: B2 ignored */
#ifdef	KEY_A2
	case KEY_A2:
		*keyp = KEY_UP;
		break;
#endif
#ifdef	KEY_B1
	case KEY_B1:
		*keyp = KEY_LEFT;
		break;
#endif
#ifdef	KEY_B3
	case KEY_B3:
		*keyp = KEY_RIGHT;
		break;
#endif
#ifdef	KEY_C2
	case KEY_C2:
		*keyp = KEY_DOWN;
		break;
#endif

#if	defined(__PDCURSES__) && defined(PADSLASH)
	case PADSLASH:
		*keyp = '/';
		break;
	case PADSTAR:
		*keyp = '*';
		break;
	case PADMINUS:
		*keyp = '-';
		break;
	case PADPLUS:
		*keyp = '+';
		break;
	case PADENTER:
		*keyp = KEY_ENTER;
		break;
#ifdef	PAD0
	case PAD0:
		*keyp = KEY_IC;
		break;
	case PADSTOP:
		*keyp = KEY_DC;
		break;
#endif	/* PAD0 */
#endif	/* __PDCURSES__ */
#endif	/* KEY_A1 */
	default:
		break;
	}

	/* Check if key should be ignored */
	switch (*keyp) {
#if 0 /* 2012/08/30 removed to allow Tab key in extended Accept */
	case KEY_STAB:
		if (field_accept) {
			*keyp = 0;
		}
		break;
#endif
	case '\033':
		if (!COB_EXTENDED_STATUS || !COB_USE_ESC) {
			*keyp = 0;
		}
		break;
	case KEY_PPAGE:
	case KEY_NPAGE:
	case KEY_PRINT:
		if (!COB_EXTENDED_STATUS) {
			*keyp = 0;
		}
		break;
	case KEY_UP:
	case KEY_DOWN:
		if (field_accept && !COB_EXTENDED_STATUS) {
			*keyp = 0;
		}
		break;
	default:
		break;
	}
}

/* update field for the programs SPECIAL-NAMES CURSOR clause */
static void
pass_cursor_to_program (void)
{
	if (COB_MODULE_PTR && COB_MODULE_PTR->cursor_pos) {
		cob_field	*cursor_field = COB_MODULE_PTR->cursor_pos;
		int		sline;
		int		scolumn;
		getyx (mywin, sline, scolumn);
		sline++; scolumn++;	/* zero-based in curses */
		if (COB_FIELD_IS_NUMERIC (cursor_field) &&
			COB_FIELD_TYPE (cursor_field) != COB_TYPE_NUMERIC_DISPLAY) {
			sline *= 1000;
			sline += scolumn;
			cob_set_int (cursor_field, sline);
		} else {
			char	buff[23]; /* 10: make the compiler happy as "int" *could*
								 have more digits than we "assume" */
			switch (cursor_field->size) {
			case 4:
				sline *= 100;
				sline += scolumn;
				sprintf (buff, "%4.4d", sline);
				memcpy (cursor_field->data, buff, 4U);
				break;
			case 6:
				sline *= 1000;
				sline += scolumn;
				sprintf (buff, "%6.6d", sline);
				memcpy (cursor_field->data, buff, 6U);
				break;
			/* LCOV_EXCL_START */
			default:
				cob_fatal_error (COB_FERROR_CODEGEN);
			/* LCOV_EXCL_STOP */
			}
		}
	}
}

static int
get_line_and_col_from_field (cob_field* pos, int* line, int* column)
{
	const int maxsize = pos->size;
	int	max_line_column;
	int	pos_val;

	if (maxsize == 4) {
		max_line_column = 100;
	} else if (maxsize == 6) {
		max_line_column = 1000;
	} else {
		max_line_column = 1; /* set to some value that don't crash */
	}

	if (COB_FIELD_IS_NUMERIC (pos)) {
		pos_val = cob_get_int (pos);
	} else {
		char	buff[23]; /* 7: make the compiler happy as "int" *could*
							 have more digits than we "assume" */
		/* LCOV_EXCL_START */
		if (unlikely (max_line_column == 1)) {
			cob_fatal_error (COB_FERROR_CODEGEN);
		}
		/* LCOV_EXCL_STOP */
		memcpy (buff, pos->data, maxsize);
		buff[maxsize + 1] = 0;
		if (unlikely (!sscanf (buff, "%d", &pos_val))) {
			cob_fatal_error (COB_FERROR_CODEGEN);
		}
	}

	*line = (pos_val / max_line_column);
	*column = (pos_val % max_line_column);

	return max_line_column;
}

/* set given parameters to the programs SPECIAL-NAMES CURSOR clause or
  -1 if not provided */
static void
get_cursor_from_program (int *line, int *column)
{
	if (COB_MODULE_PTR && COB_MODULE_PTR->cursor_pos) {
		int ret = get_line_and_col_from_field
			(COB_MODULE_PTR->cursor_pos, line, column);
		/* LCOV_EXCL_START */
		if (unlikely (ret == 1)) {
			cob_fatal_error (COB_FERROR_CODEGEN);
		}
		/* LCOV_EXCL_STOP */
		*line = *line - 1;
		*column = *column - 1;
	} else {
		*line = *column = -1;
	}
}

static void
raise_ec_on_truncation (const int item_size)
{
	int	y;
	int	x;
	int	max_y;
	int	max_x;

	getyx (mywin, y, x);
	getmaxyx (mywin, max_y, max_x);

	if (x + item_size - 1 > max_x) {
		cob_set_exception (COB_EC_SCREEN_ITEM_TRUNCATED);
	}

	COB_UNUSED (y);
	COB_UNUSED (max_y);
}

static void
cob_addnstr (const char *data, const int size)
{
	raise_ec_on_truncation (size);
	waddnstr (mywin ,data, size);
}

/* variant of cob_addnstr that outputs each character separately,
   replacing special values by WACS symbols for CONTROL GRAPHICS */
static void
cob_addnstr_graph (const char *data, const int size)
{
	int	count;
	raise_ec_on_truncation (size);

	for (count = 0; count < size; count++) {
		const char c = *data++;
		switch (c) {
		case 'j':	/* lower-right corner */
#if defined (WITH_WIDE_FUNCTIONS)
			wadd_wch (mywin ,WACS_LRCORNER);
#else
			waddch (mywin ,ACS_LRCORNER);
#endif
			break;
		case 'J':	/* lower-right corner, double */
#if defined (WACS_D_LRCORNER) && defined (WITH_WIDE_FUNCTIONS)
			wadd_wch (mywin ,WACS_D_LRCORNER);
#elif defined (ACS_D_LRCORNER)
			waddch (mywin ,ACS_D_LRCORNER);
#else
			waddch (mywin ,(const chtype)'+');
#endif
			break;
		case 'k':	/* upper-right corner */
#if defined (WITH_WIDE_FUNCTIONS)
			wadd_wch (mywin ,WACS_URCORNER);
#else
			waddch (mywin ,ACS_URCORNER);
#endif
			break;
		case 'K':	/* upper-right corner, double */
#if defined (WACS_D_URCORNER) && defined (WITH_WIDE_FUNCTIONS)
			wadd_wch (mywin ,WACS_D_URCORNER);
#elif defined (ACS_D_URCORNER)
			waddch (mywin ,ACS_D_URCORNER);
#else
			waddch (mywin ,(const chtype)'+');
#endif
			break;
		case 'm':	/* lower-left corner */
#if defined (WITH_WIDE_FUNCTIONS)
			wadd_wch (mywin ,WACS_LLCORNER);
#else
			waddch (mywin ,ACS_LLCORNER);
#endif
			break;
		case 'M':	/* lower-left corner, double */
#if defined (WACS_D_LLCORNER) && defined (WITH_WIDE_FUNCTIONS)
			wadd_wch (mywin ,WACS_D_LLCORNER);
#elif defined (ACS_D_LLCORNER)
			waddch (mywin ,ACS_D_LLCORNER);
#else
			waddch (mywin ,(const chtype)'+');
#endif
			break;
		case 'l':	/* upper-left corner */
#if defined (WITH_WIDE_FUNCTIONS)
			wadd_wch (mywin ,WACS_ULCORNER);
#else
			waddch (mywin ,ACS_ULCORNER);
#endif
			break;
		case 'L':	/* upper-left corner, double */
#if defined (WACS_D_ULCORNER) && defined (WITH_WIDE_FUNCTIONS)
			wadd_wch (mywin ,WACS_D_ULCORNER);
#elif defined (ACS_D_ULCORNER)
			waddch (mywin ,ACS_D_ULCORNER);
#else
			waddch (mywin ,(const chtype)'+');
#endif
			break;
		case 'n':	/* plus */
#if defined (WITH_WIDE_FUNCTIONS)
			wadd_wch (mywin ,WACS_PLUS);
#else
			waddch (mywin ,ACS_PLUS);
#endif
			break;
		case 'N':	/* plus, double */
#if defined (WACS_D_PLUS) && defined (WITH_WIDE_FUNCTIONS)
			wadd_wch (mywin ,WACS_D_PLUS);
#elif defined (ACS_D_PLUS)
			waddch (mywin ,ACS_D_PLUS);
#else
			waddch (mywin ,(const chtype)'+');
#endif
			break;
		case 'q':	/* horizontal line */
#if defined (WITH_WIDE_FUNCTIONS)
			wadd_wch (mywin ,WACS_HLINE);
#else
			waddch (mywin ,ACS_HLINE);
#endif
			break;
		case 'Q':	/* horizontal line, double */
#if defined (WACS_D_HLINE) && defined (WITH_WIDE_FUNCTIONS)
			wadd_wch (mywin ,WACS_D_HLINE);
#elif defined (ACS_D_HLINE)
			waddch (mywin ,ACS_D_HLINE);
#else
			waddch (mywin ,(const chtype)'-');
#endif
			break;
		case 'x':	/* vertical line */
#if defined (WITH_WIDE_FUNCTIONS)
			wadd_wch (mywin ,WACS_VLINE);
#else
			waddch (mywin ,ACS_VLINE);
#endif
			break;
		case 'X':	/* vertical line, double */
#if defined (WACS_D_VLINE) && defined (WITH_WIDE_FUNCTIONS)
			wadd_wch (mywin ,WACS_D_VLINE);
#elif defined (ACS_D_VLINE)
			waddch (mywin ,ACS_D_VLINE);
#else
			waddch (mywin ,(const chtype)'|');
#endif
			break;
		case 't':	/* left tee */
#if defined (WITH_WIDE_FUNCTIONS)
			wadd_wch (mywin ,WACS_LTEE);
#else
			waddch (mywin ,ACS_LTEE);
#endif
			break;
		case 'T':	/* left tee , double */
#if defined (WACS_D_LTEE) && defined (WITH_WIDE_FUNCTIONS)
			wadd_wch (mywin ,WACS_D_LTEE);
#elif defined (ACS_D_LTEE)
			waddch (mywin ,ACS_D_LTEE);
#else
			waddch (mywin ,(const chtype)'+');
#endif
			break;
		case 'u':	/* right tee */
#if defined (WITH_WIDE_FUNCTIONS)
			wadd_wch (mywin ,WACS_RTEE);
#else
			waddch (mywin ,ACS_RTEE);
#endif
			break;
		case 'U':	/* right tee , double */
#if defined (WACS_D_RTEE) && defined (WITH_WIDE_FUNCTIONS)
			wadd_wch (mywin ,WACS_D_RTEE);
#elif defined (ACS_D_RTEE)
			waddch (mywin ,ACS_D_RTEE);
#else
			waddch (mywin ,(const chtype)'+');
#endif
			break;
		case 'v':	/* bottom tee */
#if defined (WITH_WIDE_FUNCTIONS)
			wadd_wch (mywin ,WACS_BTEE);
#else
			waddch (mywin ,ACS_BTEE);
#endif
			break;
		case 'V':	/* bottom tee , double */
#if defined (WACS_D_BTEE) && defined (WITH_WIDE_FUNCTIONS)
			wadd_wch (mywin ,WACS_D_BTEE);
#elif defined (ACS_D_BTEE)
			waddch (mywin ,ACS_D_BTEE);
#else
			waddch (mywin ,(const chtype)'+');
#endif
			break;
		case 'w':	/* top tee */
#if defined (WITH_WIDE_FUNCTIONS)
			wadd_wch (mywin ,WACS_TTEE);
#else
			waddch (mywin ,ACS_TTEE);
#endif
			break;
		case 'W':	/* top tee , double */
#if defined (WACS_D_TTEE) && defined (WITH_WIDE_FUNCTIONS)
			wadd_wch (mywin ,WACS_D_TTEE);
#elif defined (ACS_D_TTEE)
			waddch (mywin ,ACS_D_TTEE);
#else
			waddch (mywin ,(const chtype)'+');
#endif
			break;
		default:
			waddch (mywin ,(const chtype)c);
		}
	}
}

static void
cob_addch (const chtype c)
{
	raise_ec_on_truncation (1);
	waddch (mywin ,c);
}

/* Use only when raise_ec_on_truncation is called beforehand. */
static void
cob_addch_no_trunc_check (const chtype c)
{
	waddch (mywin ,c);
}

static void
cob_addnch (const int n, const chtype c)
{
	int	count;

	raise_ec_on_truncation (n);
	for (count = 0; count < n; count++) {
		cob_addch_no_trunc_check (c);
	}
}

static int
is_first_screen_item (cob_screen *s)
{
	do {
		if (s->prev) {
			return 0;
		}
		s = s->parent;
	} while (s);

	return 1;
}

static cob_screen *
get_last_child (cob_screen * const s)
{
	cob_screen	*child;

	for (child = s->child; child->next; child = child->next);

	if (child->child) {
		return get_last_child (child);
	} else {
		return child;
	}
}

static cob_screen *
get_prev_screen_item (cob_screen * const s)
{
	if (s->prev) {
		if (s->prev->child) {
			return get_last_child (s->prev);
		} else {
			return s->prev;
		}
	} else if (s->parent) {
		return s->parent;
	} else {
		return NULL;
	}
}


#define UPDATE_CLAUSE_FUNC(clause_name_upper, clause_name_lower)	\
	static void							\
	update_##clause_name_lower (cob_screen *s, int * const count,	\
				    int * const found_clause)		\
	{								\
		if (s->attr & COB_SCREEN_##clause_name_upper##_PLUS) {	\
			*count += cob_get_int (s->clause_name_lower);	\
		} else if (s->attr & COB_SCREEN_##clause_name_upper##_MINUS) { \
			*count -= cob_get_int (s->clause_name_lower);	\
		} else {						\
			*count += cob_get_int (s->clause_name_lower) - 1; \
			*found_clause = 1;				\
		}							\
	}

UPDATE_CLAUSE_FUNC (LINE, line)
UPDATE_CLAUSE_FUNC (COLUMN, column)

#undef UPDATE_CLAUSE_FUNC

static size_t
get_size (cob_screen *s)
{
	if (s->field) {
		return s->field->size;
	} else { /* s->value */
		return s->value->size;
	}

}
static void
get_screen_item_line_and_col (cob_screen *s, int * const line,
			      int * const col)
{
	int		found_line = 0;
	int		found_col = 0;
	int		is_screen_to_display = 1;
	int		is_elementary;

	*line = 0;
	*col = 0;

	/*
	  Determine the line/col by looking at the given item and then moving
	  backwards.
	*/
	for (; s; s = get_prev_screen_item (s)) {
		if (s->line && !found_line) {
			update_line (s, line, &found_line);
		}

		if (!found_col) {
			is_elementary = !s->child;

			if (!is_screen_to_display && is_elementary) {
				*col += get_size (s) - 1;
			}

			if (s->column) {
				update_column (s, col, &found_col);
			}

			if (s->line && !s->column) {
				found_col = 1;
			}

			if (!found_col && !s->column && is_elementary
			 && !is_first_screen_item (s)) {
				/*
				  Note that group items are excluded; the
				  standard assumes COL + 1, unless otherwise
				  specified, on all screen items. This seems
				  silly on group items, hence why this
				  non-standard extension.
				*/
				++(*col);
			}
		}

		is_screen_to_display = 0;
	}

	*line += origin_y;
	*col += origin_x;
}

static void
cob_screen_puts (cob_screen *s, cob_field *f, const cob_u32_t is_input,
		 const enum screen_statement stmt)
{
	unsigned char	*p;
	size_t		size;
	int		line;
	int		column;
	chtype		default_prompt_char;

	get_screen_item_line_and_col (s, &line, &column);

	/* Move to specified position */
	cob_move_cursor (line, column);
	cob_current_y = line;
	cob_current_x = column;
#if	0	/* RXWRXW - Attr */
	cob_screen_attr (s->foreg, s->backg, s->attr, NULL, NULL, stmt);
#endif
	if (s->attr & COB_SCREEN_INPUT) {
		cob_screen_attr (s->foreg, s->backg, s->attr, NULL, NULL, stmt);
		if (s->prompt) {
			default_prompt_char = s->prompt->data[0];
		} else {
			default_prompt_char = COB_CH_UL;
		}
		p = f->data;
		raise_ec_on_truncation (f->size);
		for (size = 0; size < f->size; size++, p++) {
			if (s->attr & COB_SCREEN_SECURE) {
				cob_addch_no_trunc_check (COB_CH_AS);
			} else if (*p <= ' ' && stmt == ACCEPT_STATEMENT) {
				cob_addch_no_trunc_check (default_prompt_char);
			} else {
				cob_addch_no_trunc_check ((const chtype)*p);
			}
		}
	} else if (!is_input) {
		const cob_flags_t attr = cob_screen_attr (s->foreg, s->backg, s->attr, NULL, NULL, stmt);
		if (attr & COB_SCREEN_GRAPHICS) {
			cob_addnstr_graph ((char *)f->data, (int)f->size);
		} else {
			cob_addnstr ((char *)f->data, (int)f->size);
		}
	} else {
		column += (int)f->size;
		cob_move_cursor (line, column);
	}

	if (stmt == DISPLAY_STATEMENT) {
		display_cursor_y = line;
		display_cursor_x = column + f->size;
	} else { /* ACCEPT_STATEMENT */
		accept_cursor_y = line;
		accept_cursor_x = column + f->size;
	}

	refresh_mywin (mywin);
}

static COB_INLINE COB_A_INLINE int
cob_field_is_numeric_or_numeric_edited (cob_field *field)
{
	return (COB_FIELD_IS_NUMERIC (field)
		|| COB_FIELD_TYPE (field) == COB_TYPE_NUMERIC_EDITED);
}

static int
field_is_empty (cob_screen *s)
{
	unsigned char	*data = s->field->data;
	size_t		size = s->field->size;
	size_t		i;

	for (i = 0; i < size; ++i) {
		if (!isspace (data[i])) {
			return 0;
		}
	}

	return 1;
}

static int
field_is_zero (cob_screen *s)
{
	unsigned char	*data = s->field->data;
	size_t		size = s->field->size;
	size_t		i;
	unsigned char	decimal_point = COB_MODULE_PTR->decimal_point;

	for (i = 0; i < size; ++i) {
		if (!(isspace (data[i]) || data[i] == '0'
		      || data[i] == decimal_point)) {
			return 0;
		}
	}

	return 1;
}

static int
pic_has_zero_suppression (const cob_pic_symbol *pic)
{
	int	i;

	for (i = 0; pic[i].symbol != '\0'; ++i) {
		/*
		  NB: + and - are floating-insertion editing characters, not
		  zero-suppression ones.
		*/
		if (pic[i].symbol == 'Z' || pic[i].symbol == '*') {
			return 1;
		}
	}

	return 0;
}

static int
get_num_int_digits_for_no_zero_sup (const cob_pic_symbol *pic)
{
	int	i;
	int	num_digits = 0;
	char	numeric_separator = COB_MODULE_PTR->numeric_separator;

	for (i = 0; pic[i].symbol != '\0'; ++i) {
		if (pic[i].symbol == '9'
		    || pic[i].symbol == 'Z'
		    || pic[i].symbol == '*') {
			num_digits += pic[i].times_repeated;
		} else if (!(pic[i].symbol == numeric_separator
			     || pic[i].symbol == 'B'
			     || pic[i].symbol == '0'
			     || pic[i].symbol == '/')
			   && num_digits != 0) {
			break;
		}
	}

	return num_digits;
}

static int
field_is_zero_or_no_zero_suppression (cob_screen *s)
{
	const cob_pic_symbol	*pic = COB_FIELD_PIC (s->field);
	size_t		i;
	size_t		size = COB_FIELD_SIZE (s->field);
	unsigned char	*data = COB_FIELD_DATA (s->field);
	int		num_integer_digits;
	int		num_digits_seen = 0;

	if (field_is_zero (s) || !pic_has_zero_suppression (pic)) {
		return 1;
	}

	num_integer_digits = get_num_int_digits_for_no_zero_sup (pic);

	/*
	  Verify there are sufficient non-zero digits before a decimal
	  point/the end to fill the integer part of the field.
	*/
	for (i = 0; i < size; ++i) {
		if (isdigit (data[i])) {
			if (data[i] != '0' || num_digits_seen != 0) {
				++num_digits_seen;
			}
		} else if (!isspace (data[i]) && num_digits_seen != 0) {
			break;
		}
	}

	return num_digits_seen >= num_integer_digits;
}

/* Assuming s->field is alphanumeric */
static int
field_is_full (cob_screen *s)
{
	unsigned char	*data = s->field->data;
	size_t		size = s->field->size;

	/* Per the standard, only the first and last chars need be non-space. */
	return !isspace (*data) && !isspace (*(data + size - 1));
}

static int
satisfied_full_clause (cob_screen *s)
{
	if (!(s->attr & COB_SCREEN_FULL)) {
		return 1;
	}

	if (COB_FIELD_IS_NUMERIC (s->field)) {
		return !field_is_zero (s);
	} else if (COB_FIELD_TYPE (s->field) == COB_TYPE_NUMERIC_EDITED) {
		return field_is_zero_or_no_zero_suppression (s);
	} else { /* field is alphanumeric */
		return field_is_full (s) || field_is_empty (s);
	}
}

static int
satisfied_required_clause (cob_screen *s)
{
	if (!(s->attr & COB_SCREEN_REQUIRED)) {
		return 1;
	}

	if (cob_field_is_numeric_or_numeric_edited (s->field)) {
		return !field_is_zero (s);
	} else { /* field is alphanumeric */
		return !field_is_empty (s);
	}
}

static int
valid_field_data (cob_field *field)
{
	int num_check;

	if (COB_FIELD_IS_NUMERIC (field)) {
		return cob_check_numval (field, NULL, 0, 0) == 0;
	} else if (field->attr->type == COB_TYPE_NUMERIC_EDITED) {
		num_check = cob_check_numval (field, NULL, 1, 0);
		/* test for all spaces which is valid in this case
		   and change to a one zero instead */
		if (num_check == (int)field->size + 1) {
			field->data[0] = '0';
			return 1;
		}
		return cob_check_numval (field, NULL, 1, 0) == 0;
	} else {
		return 1;
	}
}

static void
refresh_field (cob_screen *s)
{
	int		y;
	int		x;

	getyx (mywin, y, x);
	cob_screen_puts (s, s->field, cobsetptr->cob_legacy, ACCEPT_STATEMENT);
	cob_move_cursor (y, x);
}

static void
format_field (cob_screen *s)
{
	cob_field	field;
	size_t		size = s->field->size;
	unsigned char	*data;

	/*
	  We copy the data into another field and move it back to format the
	  numeric data neatly, rather than re-implement that logic here. We
	  assume the data is valid.
	*/
	data = cob_malloc (size);
	memcpy (data, s->field->data, size);
	COB_FIELD_INIT (size, data, s->field->attr);

	if (COB_FIELD_IS_NUMERIC (s->field)) {
		cob_move (cob_intr_numval (&field), s->field);
	} else if (field.attr->type == COB_TYPE_NUMERIC_EDITED) {
		cob_move (cob_intr_numval_c (&field, NULL), s->field);
	}

	cob_free (data);

	refresh_field (s);
}

/* Finalize field on leaving it: checks and conversions */
static int
finalize_field_input (cob_screen *s)
{
	/* Only numeric types need to be validated and formatted. */
	if (cob_field_is_numeric_or_numeric_edited (s->field)) {
		if (!valid_field_data (s->field)) {
			return 1;
		}
		format_field (s);
	}

	if (!satisfied_full_clause (s) || !satisfied_required_clause (s)) {
		return 1;
	}

	return 0;
}

static int
finalize_all_fields (struct cob_inp_struct *sptr, const size_t total_idx)
{
	const struct cob_inp_struct *end = sptr + total_idx;

	for (; sptr < end; ++sptr) {
		if (finalize_field_input (sptr->scr)) {
			return 1;
		}
	}

	return 0;
}


/* If off turn on, if on turn off;
   additional: switch between vertical bar cursor (on) and
   square cursor (off) - note: the cursor change may has no
   effect in all curses implementations / terminals */
static void
cob_toggle_insert ()
{
	if (COB_INSERT_MODE == 0) {
		COB_INSERT_MODE = 1;     /* on */
	}
	else {
		COB_INSERT_MODE = 0;     /* off */
	}
	cob_settings_screenio ();
}

#define SET_FLD_AND_DATA_REFS(curr_index,structure,scrdef,sline,scolumn,right_pos,field_data) \
	structure = cob_base_inp + curr_index;				\
	scrdef = structure->scr;							\
	sline = structure->this_y;							\
	scolumn = structure->this_x;						\
	right_pos = scolumn + (int)scrdef->field->size - 1;	\
	field_data = scrdef->field->data

#define SET_FLD_REFS(curr_index,structure,scrdef,sline,scolumn,right_pos) \
	structure = cob_base_inp + curr_index;				\
	scrdef = structure->scr;							\
	sline = structure->this_y;							\
	scolumn = structure->this_x;						\
	right_pos = scolumn + (int)scrdef->field->size - 1

/* find field by position, returns index for field or -1 if not found */
static int
find_field_by_pos (const int initial_curs, const int line, const int column) {
	struct cob_inp_struct	*sptr;
	cob_screen		*s;
	int			sline;
	int			scolumn;
	int			right_pos;

	size_t idx;

	for (idx = (size_t)initial_curs; idx < totl_index; idx++) {
		SET_FLD_REFS (idx, sptr, s, sline, scolumn, right_pos);
		if (line == sline
		 && column >= scolumn
		 && column <= right_pos) {
			return (int)idx;
		}
	}
	return -1;
}

#ifdef HAVE_MOUSEMASK
static int
mouse_to_exception_code (mmask_t mask) {
	int fret = -1;

	if (mask & BUTTON1_PRESSED) fret = 2041;
	else if (mask & BUTTON1_CLICKED) fret = 2041;
	else if (mask & BUTTON1_RELEASED) fret = 2042;
	else if (mask & BUTTON1_DOUBLE_CLICKED) fret = 2043;
	else if (mask & BUTTON1_TRIPLE_CLICKED) fret = 2043;
	else if (mask & BUTTON2_PRESSED) fret = 2044;
	else if (mask & BUTTON2_CLICKED) fret = 2044;
	else if (mask & BUTTON2_RELEASED) fret = 2045;
	else if (mask & BUTTON2_DOUBLE_CLICKED) fret = 2046;
	else if (mask & BUTTON2_TRIPLE_CLICKED) fret = 2046;
	else if (mask & BUTTON3_PRESSED) fret = 2047;
	else if (mask & BUTTON3_CLICKED) fret = 2047;
	else if (mask & BUTTON3_RELEASED) fret = 2048;
	else if (mask & BUTTON3_DOUBLE_CLICKED) fret = 2049;
	else if (mask & BUTTON3_TRIPLE_CLICKED) fret = 2049;
#if defined COB_HAS_MOUSEWHEEL
	else if (mask & BUTTON4_PRESSED) fret = 2080;
	else if (mask & BUTTON5_PRESSED) fret = 2081;
#endif
	else fret = 2040;	/* mouse-moved (assumed) */

#if defined COB_HAS_MOUSEWHEEL
	if (mask & BUTTON_SHIFT) {
		if (fret < 2080) {
			fret += 10;
		} else {
			fret += 4;
		}
	} else if (mask & BUTTON_CTRL) {
		if (fret < 2080) {
			fret += 20;
		} else {
			fret += 8;
		}
	} else if (mask & BUTTON_ALT) {
		if (fret < 2080) {
			fret += 30;
		} else {
			fret += 12;
		}
	}
#else
	if (mask & BUTTON_SHIFT) fret += 10;
	else if (mask & BUTTON_CTRL) fret += 20;
	else if (mask & BUTTON_ALT) fret += 12;
#endif

	return fret;
}
#endif

static int
is_number_with_pic_symbol (cob_field * const f, const char symbol)
{
	int	i;

	if (COB_FIELD_TYPE (f) != COB_TYPE_NUMERIC_EDITED) {
		return 0;
	}

	for (i = 0; f->attr->pic[i].symbol; ++i) {
		if (f->attr->pic[i].symbol == symbol) {
			return 1;
		}
	}

	return 0;
}

static int
has_decimal_point (cob_field * const f)
{
	return is_number_with_pic_symbol (f, COB_MODULE_PTR->decimal_point);
}

static int
get_pic_symbol_offset (cob_field * const f, const char symbol)
{
	int	offset = 0;
	int	i = 0;

	do {
		offset += f->attr->pic[i].times_repeated;
	} while (f->attr->pic[i++].symbol != symbol);

	return offset - 1;
}

static int
get_decimal_point_offset (cob_field * const f)
{
	return get_pic_symbol_offset (f, COB_MODULE_PTR->decimal_point);
}

/*
  Shift field on screen (for columns between scolumn and ccolumn inclusive)
  over to the left by one character. This overwrites the leftmost character.
*/
static void
shift_left (cob_screen * const s, const int cline, const int ccolumn,
	     const int right_pos, const int scolumn)
{
	int		offset;
	unsigned char	move_char;

	COB_UNUSED (right_pos);

	for (offset = 0; offset < ccolumn - scolumn; offset++) {
		move_char = s->field->data[offset + 1];
		s->field->data[offset] = move_char;

		cob_move_cursor (cline, offset + scolumn);
		if (move_char != ' ') {
			if (s->attr & COB_SCREEN_NO_ECHO) {
				cob_addch (COB_CH_SP);
			} else if (s->attr & COB_SCREEN_SECURE) {
				cob_addch (COB_CH_AS);
			} else {
				cob_addch (move_char);
			}
		}
	}

	/* Restore cursor to original position */
	cob_move_cursor (cline, ccolumn);
}

/*
  Shift field on screen (for columns between ccolumn and right_pos inclusive)
  over to the right by one character. This overwrites the rightmost character.
*/
static void
shift_right (cob_screen * const s, const int cline, const int ccolumn,
	     const int right_pos, const int scolumn)
{
	int		offset;
	unsigned char	move_char;

	for (offset = right_pos - scolumn; offset > ccolumn - scolumn; offset--) {
		move_char = s->field->data[offset - 1];
		s->field->data[offset] = move_char;

		cob_move_cursor (cline, offset + scolumn);
		if (move_char != ' ') {
			if (s->attr & COB_SCREEN_NO_ECHO) {
				cob_addch (COB_CH_SP);
			} else if (s->attr & COB_SCREEN_SECURE) {
				cob_addch (COB_CH_AS);
			} else {
				cob_addch (move_char);
			}
		}
	}

	/* Restore cursor to original position */
	cob_move_cursor (cline, ccolumn);
}

static int
at_offset_from_decimal_point (cob_field * const f, const int scolumn,
			      const int ccolumn, const int offset)
{
	return has_decimal_point (f)
		&& ccolumn == offset + scolumn + get_decimal_point_offset (f);
}

/* Move cursor and data pointer to initial position when entering a field */
static int
move_to_initial_field_pos (cob_field * const f, const int sline,
			   const int scolumn, const int right_pos,
			   const int to_right_side, unsigned char ** const data_ptr)
{
	int	offset;

	*data_ptr = f->data;

	if (COB_INSERT_MODE && cob_field_is_numeric_or_numeric_edited (f)) {
		if (has_decimal_point (f)) {
			if (to_right_side) {
				/* At leftmost decimal digit */
				offset = get_decimal_point_offset (f) + 1;
			} else {
				/* At rightmost integer digit */
				offset = get_decimal_point_offset (f) - 1;
			}
			*data_ptr += offset;
			return cob_move_cursor (sline, scolumn + offset);
		} else {
			/* At rightmost integer digit */
			*data_ptr += right_pos - scolumn;
			return cob_move_cursor (sline, right_pos);
		}
	} else {
		/* At start/end of field */
		if (to_right_side) {
			*data_ptr += right_pos - scolumn;
			return cob_move_cursor (sline, right_pos);
		} else {
			return cob_move_cursor (sline, scolumn);
		}
	}
}

/* TO-DO: Unify with "Shift data to insert character" logic */
static int
can_insert_left (cob_field * const f, const unsigned char leftmost_char,
		 const int ccolumn)
{
	if (cob_field_is_numeric_or_numeric_edited (f)) {
		if (has_decimal_point (f) &&
		    ccolumn > get_decimal_point_offset (f)) {
			return 0;
		} else {
			return leftmost_char == '0'
				|| leftmost_char == ' ';
		}
	} else {
		return leftmost_char == ' ';
	}
}

static void
cob_screen_get_all (const int initial_curs, const int accept_timeout)
{
	size_t			curr_index = (size_t)initial_curs;
	struct cob_inp_struct	*sptr;
	cob_screen		*s;
	int			sline;
	int			scolumn;
	int			right_pos;
	unsigned char		*p;
	unsigned char		*p2;
	unsigned char		move_char;
	int			keyp;
	int			cline;
	int			ccolumn;
	int			at_eof = 0;
	int			ungetched = 0;
	int			status;
	int			count;
	chtype			default_prompt_char;
	int			has_dp;
	int			integer_part_end;
	char			sign;
	int			fix_position = 0;
#ifdef HAVE_MOUSEMASK
	MEVENT		mevent;
#endif

	SET_FLD_AND_DATA_REFS (curr_index, sptr, s, sline, scolumn, right_pos, p);

	status = move_to_initial_field_pos (s->field, sline, scolumn, right_pos, 0, &p);
	if (status != ERR) {
		pending_accept = 0;
	}
	cob_screen_attr (s->foreg, s->backg, s->attr, NULL, NULL, ACCEPT_STATEMENT);

	/* position for the SPECIAL-NAMES CURSOR clause, if given */
	{
		int		cursor_clause_line;
		int		cursor_clause_col;
		get_cursor_from_program (&cursor_clause_line, &cursor_clause_col);
		if (cursor_clause_line > 0) {
			int		fld_index = find_field_by_pos (initial_curs, cursor_clause_line, cursor_clause_col);
			if (fld_index >= 0) {
				curr_index = fld_index;
				SET_FLD_AND_DATA_REFS (curr_index, sptr, s, sline, scolumn, right_pos, p);
				at_eof = 0;
				cob_screen_attr (s->foreg, s->backg, s->attr, NULL, NULL, ACCEPT_STATEMENT);
				cob_move_cursor (cursor_clause_line, cursor_clause_col);
			} else {
				/* note: COBOL 2002 states that in this case the CURSOR clause is ignored,
				         while MicroFocus and ACUCOBOL-GT day "the nearest field" */
			}
		}
	}

#ifdef HAVE_MOUSEMASK
	/* prevent warnings about not intialized structure */
	memset (&mevent, 0, sizeof (MEVENT));
#endif

	for (; ;) {
		if (s->prompt) {
			default_prompt_char = s->prompt->data[0];
		} else {
			default_prompt_char = COB_CH_UL;
		}

		refresh_mywin (mywin);
		errno = 0;
		wtimeout (mywin ,accept_timeout);
		keyp = getch ();

		/* FIXME: modularize (cob_screen_get_all, field_accept) and
		          use identical handling of keys wherever possible */

		if (keyp == ERR) {
			global_return = 8001;
			goto screen_return;
		}
		if (keyp > KEY_F0 && keyp < KEY_F(65)) {
			global_return = 1000 + keyp - KEY_F0;
			goto screen_return;
		}

#ifdef HAVE_MOUSEMASK
		/* get mouse event here, handle later */
		if (keyp == KEY_MOUSE) {
			getmouse (&mevent);
			/* in case of left double-click:
			   always translate to ENTER in SCREEN ACCEPT;
			   exception: user requested control of this */
			if (mevent.bstate & BUTTON1_DOUBLE_CLICKED
			 && !(cob_mask_accept & BUTTON1_DOUBLE_CLICKED)) {
				keyp = KEY_ENTER;
			}
		}
#endif

		cob_convert_key (&keyp, 0);
		if (keyp <= 0) {
			(void)flushinp ();
			cob_beep ();
			continue;
		}

		getyx (mywin, cline, ccolumn);

		switch (keyp) {
		case KEY_ENTER:
			if (finalize_all_fields (cob_base_inp, totl_index)) {
				cob_beep ();
				continue;
			}
			goto screen_return;
		case KEY_PPAGE:
			global_return = 2001;
			goto screen_return;
		case KEY_NPAGE:
			global_return = 2002;
			goto screen_return;
		case KEY_PRINT:
			global_return = 2006;
			goto screen_return;
		case '\033':
			global_return = 2005;
			goto screen_return;
		case KEY_STAB:
			finalize_field_input (s);

			if (curr_index < totl_index - 1) {
				curr_index++;
			} else {
				curr_index = 0;
			}
			SET_FLD_AND_DATA_REFS (curr_index, sptr, s, sline, scolumn, right_pos, p);
			at_eof = 0;
			move_to_initial_field_pos (s->field, sline, scolumn, right_pos, 0, &p);
			cob_screen_attr (s->foreg, s->backg, s->attr, NULL, NULL, ACCEPT_STATEMENT);
			continue;
		case KEY_BTAB:
			finalize_field_input (s);

			if (curr_index > 0) {
				curr_index--;
			} else {
				curr_index = totl_index - 1;
			}
			SET_FLD_AND_DATA_REFS (curr_index, sptr, s, sline, scolumn, right_pos, p);
			at_eof = 0;
			move_to_initial_field_pos (s->field, sline, scolumn, right_pos, ungetched, &p);
			ungetched = 0;
			cob_screen_attr (s->foreg, s->backg, s->attr, NULL, NULL, ACCEPT_STATEMENT);
			continue;
		case KEY_UP:
			finalize_field_input (s);

			curr_index = sptr->up_index;
			SET_FLD_AND_DATA_REFS (curr_index, sptr, s, sline, scolumn, right_pos, p);
			at_eof = 0;
			move_to_initial_field_pos (s->field, sline, scolumn, right_pos, 0, &p);
			cob_screen_attr (s->foreg, s->backg, s->attr, NULL, NULL, ACCEPT_STATEMENT);
			continue;
		case KEY_DOWN:
			finalize_field_input (s);

			curr_index = sptr->down_index;
			SET_FLD_AND_DATA_REFS (curr_index, sptr, s, sline, scolumn, right_pos, p);
			at_eof = 0;
			move_to_initial_field_pos (s->field, sline, scolumn, right_pos, 0, &p);
			cob_screen_attr (s->foreg, s->backg, s->attr, NULL, NULL, ACCEPT_STATEMENT);
			continue;
		case KEY_HOME:
			finalize_field_input (s);

			curr_index = 0;
			SET_FLD_AND_DATA_REFS (curr_index, sptr, s, sline, scolumn, right_pos, p);
			at_eof = 0;
			cob_move_cursor (sline, scolumn);
			cob_screen_attr (s->foreg, s->backg, s->attr, NULL, NULL, ACCEPT_STATEMENT);
			continue;
		case KEY_END:
			finalize_field_input (s);

			curr_index = totl_index - 1;
			SET_FLD_AND_DATA_REFS (curr_index, sptr, s, sline, scolumn, right_pos, p);
			at_eof = 0;
			cob_move_cursor (sline, scolumn);
			cob_screen_attr (s->foreg, s->backg, s->attr, NULL, NULL, ACCEPT_STATEMENT);
			continue;
		case KEY_BACKSPACE:
			/* Backspace key. */
			/* Don't allow backspacing over a decimal point */
			if (ccolumn > scolumn
			    && !at_offset_from_decimal_point (s->field, scolumn, ccolumn, 1)) {
				at_eof = 0;
				/* Shift remainder left with cursor. */
				for (count = ccolumn; count < right_pos + 1; count++) {
					/* Get character. */
					p2 = s->field->data + count - scolumn ;
					move_char = *p2;
					/* Move the character left. */
					p2 = s->field->data + count - scolumn - 1;
					*p2 = move_char;
					/* Update screen with moved character. */
					cob_move_cursor (cline, count - 1);
					if (s->attr & COB_SCREEN_NO_ECHO) {
						cob_addch (COB_CH_SP);
					} else if (s->attr & COB_SCREEN_SECURE) {
						cob_addch (COB_CH_AS);
					} else if (move_char == ' ') {
						cob_addch (default_prompt_char);
					} else {
						cob_addch (move_char);
					}
				}
				/* Put space as the right most character. */
				p2 = s->field->data + s->field->size - 1;
				if (COB_FIELD_IS_NUMERIC (s->field)) {
					*p2 = '0';
				} else {
					*p2 = ' ';
				}
				/* Add space to screen. */
				cob_move_cursor (cline, count - 1);
				if (s->attr & COB_SCREEN_NO_ECHO) {
					cob_addch (COB_CH_SP);
				} else if (s->attr & COB_SCREEN_SECURE) {
					cob_addch (COB_CH_AS);
				} else if (*p2 == ' ') {
					cob_addch (default_prompt_char);
				} else {
					cob_addch (*p2);
				}
				/* Move cursor left one from current. */
				ccolumn--;
				cob_move_cursor (cline, ccolumn);
				p--;
			} else {
				cob_beep ();
			}
			continue;
		case KEY_LEFT:
			if (ccolumn > scolumn) {
				ccolumn--;
				cob_move_cursor (cline, ccolumn);
				p = s->field->data + ccolumn - scolumn;
			} else {
				ungetched = 1;
				ungetch (KEY_BTAB);
			}
			continue;
		case KEY_RIGHT:
			if (ccolumn < right_pos) {
				ccolumn++;
				cob_move_cursor (cline, ccolumn);
				p = s->field->data + ccolumn - scolumn;
			} else {
				ungetch ('\t');
			}
			continue;
		case KEY_IC:
			/* Insert key toggle */
			cob_toggle_insert();
			continue;
		case KEY_DC:
			/* Delete key. */
			/* Don't allow deletion of decimal point */
			if (at_offset_from_decimal_point (s->field, scolumn, ccolumn, 0)) {
				cob_beep ();
				continue;
			}
			/* Delete character, move remainder left. */
			for (count = ccolumn; count < right_pos; count++) {
				/* Get character one position to right. */
				p2 = s->field->data + count - scolumn + 1;
				move_char = *p2;
				/* Move the character left. */
				p2 = s->field->data + count - scolumn;
				*p2 = move_char;
				/* Update screen with moved character. */
				cob_move_cursor (cline, count);
				if (s->attr & COB_SCREEN_NO_ECHO) {
					cob_addch (COB_CH_SP);
				} else if (s->attr & COB_SCREEN_SECURE) {
					cob_addch (COB_CH_AS);
				} else if (move_char == ' ') {
					cob_addch (default_prompt_char);
				} else {
					cob_addch (move_char);
				}
			}
			/* Put space as the right most character. */
			p2 = s->field->data + s->field->size - 1;
			if (COB_FIELD_IS_NUMERIC (s->field)) {
				*p2 = '0';
			} else {
				*p2 = ' ';
			}
			/* Add space to screen. */
			cob_move_cursor (cline, count);
			if (s->attr & COB_SCREEN_NO_ECHO) {
				cob_addch (COB_CH_SP);
			} else if (s->attr & COB_SCREEN_SECURE) {
				cob_addch (COB_CH_AS);
			} else if (*p2 == ' ') {
				cob_addch (default_prompt_char);
			} else {
				cob_addch (*p2);
			}
			/* Put cursor back to original position. */
			cob_move_cursor (cline, ccolumn);
			continue;

		case '.':
		case ',':
			if (keyp != COB_MODULE_PTR->decimal_point
			    || !has_decimal_point (s->field)) {
				break;
			}

			finalize_field_input (s);
			/* Move cursor to character after decimal point */
			ccolumn = scolumn + get_decimal_point_offset (s->field) + 1;
			cob_move_cursor (cline, ccolumn);
			p = s->field->data + ccolumn - scolumn;
			continue;

		case '+':
		case '-':
			if (is_number_with_pic_symbol (s->field, '+')) {
				sign = '+';
			} else if (is_number_with_pic_symbol (s->field, '-')) {
				sign = '-';
			} else if (is_number_with_pic_symbol (s->field, 'S')) {
				sign = 'S';
			} else {
				break;
			}

			finalize_field_input (s);

			/* Move cursor to sign */
			ccolumn = scolumn + get_pic_symbol_offset (s->field, sign);
			cob_move_cursor (cline, ccolumn);
			p = s->field->data + ccolumn - scolumn;
			/* Enter sign */
			break;

#ifdef HAVE_MOUSEMASK
		case KEY_MOUSE:
		{
			int mline = mevent.y;
			int mcolumn = mevent.x;
			if (!wmouse_trafo(mywin, &mline, &mcolumn, 0)) {
				cob_beep ();
				continue;
			}
			/* handle depending on state */
			if (mevent.bstate & BUTTON1_PRESSED
			 && COB_MOUSE_FLAGS & 1) {
				int fld_index = -1;
				/* if in current field, just move */
				if (mline == cline) {
					if (mcolumn >= scolumn
					 && mcolumn <= right_pos) {
						ccolumn = mcolumn;
						cob_move_cursor (cline, ccolumn);
						p = s->field->data + ccolumn - scolumn;
						continue;
					}
				}
				finalize_field_input (s);

				fld_index = find_field_by_pos (initial_curs, mline, mcolumn);
				if (fld_index >= 0) {
					curr_index = fld_index;
					SET_FLD_AND_DATA_REFS (curr_index, sptr, s, sline, scolumn, right_pos, p);
					at_eof = 0;
					cob_screen_attr (s->foreg, s->backg, s->attr, NULL, NULL, ACCEPT_STATEMENT);
					cob_move_cursor (mline, mcolumn);
					continue;
				}
			}
			mevent.bstate &= cob_mask_accept;
			if (mevent.bstate != 0) {
				global_return = mouse_to_exception_code (mevent.bstate);
				cob_move_cursor (mline, mcolumn);	/* move cursor to pass position */
				goto screen_return;
			}
			continue;
		}
#endif
		default:
			break;
		}

		/* Handle printable character. */
#if 0 /* FIXME: we can't handle anything > UCHAR_MAX here because of
		*p = (unsigned char) keyp;
		--> revise */
		if (keyp > 037 && keyp < (int)A_CHARTEXT) {
#else
		if (keyp > 037 && keyp <= UCHAR_MAX) {
#endif
			/* Numeric field check. */
			if (cob_field_is_numeric_or_numeric_edited (s->field)
			    && ((keyp != '+' && keyp != '-' && (keyp < '0' || keyp > '9'))
				|| at_offset_from_decimal_point (s->field, scolumn, ccolumn, 0))) {
				cob_beep ();
				continue;
			}

			/* Handle UPPER/LOWER. */
			if (s->attr & COB_SCREEN_UPPER) {
				keyp = toupper ((unsigned char)keyp);
			} else if (s->attr & COB_SCREEN_LOWER) {
				keyp = tolower ((unsigned char)keyp);
			}

			if (COB_INSERT_MODE) {
				/* Shift data to insert character */
				if (cob_field_is_numeric_or_numeric_edited (s->field)) {
					has_dp = has_decimal_point (s->field);
					if (has_dp) {
						integer_part_end = scolumn + get_decimal_point_offset (s->field);
					} else {
						integer_part_end = right_pos;
					}
					if (!has_dp
					    || ccolumn < integer_part_end) {
						/*
						  For non-decimal digits, insert
						  digit to right of cursor - so
						  01[2] becomes 12[3] after
						  pressing 3 (where "[2]" means
						  the cursor is over 2).

						  Beep if leftmost character is
						  significant.
						*/
						p2 = s->field->data;
						if (*p2 != '0' && *p2 != ' ') {
							cob_beep ();
							continue;
						}
						shift_left (s, cline, ccolumn,
							    integer_part_end,
							    scolumn);
						fix_position = 1;
					} else if (ccolumn == integer_part_end) {
						/* The cursor is at a decimal
						   point - make user move to one
						   side of it
						*/
						cob_beep ();
						continue;
					} else {
						/*
						  For decimal digits, insert
						  digit to left of cursor - so
						  0.[1]0 becomes 0.2[1] after
						  pressing 2.

						  Beep if rightmost character is
						  significant.
						*/
						p2 = s->field->data + right_pos - scolumn;
						if (*p2 != '0' && *p2 != ' ') {
							cob_beep ();
							continue;
						}
						shift_right (s, cline, ccolumn, right_pos, scolumn);
					}
				} else {
					/* check and beep if field is already full */
					p2 = s->field->data + right_pos - scolumn;
					if (*p2 != ' ') {
						cob_beep ();
						continue;
					}
					shift_right (s, cline, ccolumn, right_pos, scolumn);
				}
			}

			/* actual storing the key */
			*p = (unsigned char) keyp;

			/* Display character or '*' if secure. */
			if (s->attr & COB_SCREEN_SECURE) {
				cob_addch (COB_CH_AS);
			} else if (s->attr & COB_SCREEN_NO_ECHO) {
				cob_addch (COB_CH_SP);
			} else {
				cob_addch ((const chtype)keyp);
			}

			if (ccolumn == right_pos) {
				/* Auto-skip at end of field. */
				if (s->attr & COB_SCREEN_AUTO) {
					if (curr_index == totl_index - 1) {
						goto screen_return;
					} else {
						ungetch ('\t');
					}
				}
				cob_move_cursor (cline, ccolumn);

				/* check if we (still) are at last position and inform
				   user with a beep (after having processed his key) */
				/* TO-DO: Only beep if user has *just* entered a
				   last char and tries to enter a new
				   char. That is, don't beep after changing insert
				   mode or navigating to last character and
				   overwriting it. */
				if (at_eof) {
					cob_beep ();
				} else {
					at_eof = !COB_INSERT_MODE
						|| !can_insert_left (s->field, *s->field->data, ccolumn);
				}
			} else if (fix_position) {
				cob_move_cursor (cline, ccolumn);
				fix_position = 0;
			} else {
				p++;
			}
			continue;
		}
		(void)flushinp ();
		cob_beep ();
	}
screen_return:
	refresh_mywin (mywin);
}

static int
compare_yx (const void *m1, const void *m2)
{
	const struct cob_inp_struct	*s1;
	const struct cob_inp_struct	*s2;

	s1 = m1;
	s2 = m2;
	if (s1->this_y < s2->this_y) {
		return -1;
	}
	if (s1->this_y > s2->this_y) {
		return 1;
	}
	if (s1->this_x < s2->this_x) {
		return -1;
	}
	if (s1->this_x > s2->this_x) {
		return 1;
	}
	return 0;
}

static void
cob_screen_moveyx (cob_screen *s)
{
	int	y;
	int	x;
	int	line;
	int	column;

	if (s->line || s->column ||
	    s->attr & (COB_SCREEN_LINE_PLUS | COB_SCREEN_LINE_MINUS |
		       COB_SCREEN_COLUMN_PLUS | COB_SCREEN_COLUMN_MINUS)) {
		getyx (mywin, y, x);
		if (x < 0 || y < 0) {
			/* not translated as "testing only" (should not happen) */
			cob_runtime_warning ("negative values from getyx");
			x = y = 0;
		}
		/* Column adjust */
		if (x != 0) {
			x--;
		}
		if (!s->line) {
			line = y;
		} else {
			line = origin_y + cob_get_int (s->line);
			if (line < 0) {
				line = y;
			}
		}
		if (!s->column) {
			column = x;
		} else {
			column = origin_x + cob_get_int (s->column);
			if (column < 0) {
				column = x;
			}
		}
		if (s->attr & COB_SCREEN_LINE_PLUS) {
			line = y + line;
		} else if (s->attr & COB_SCREEN_LINE_MINUS) {
			line = y - line;
		}
		if (s->attr & COB_SCREEN_COLUMN_PLUS) {
			column = x + column;
		} else if (s->attr & COB_SCREEN_COLUMN_MINUS) {
			column = x - column;
		}

		cob_move_cursor (line, column);
		refresh_mywin (mywin);
		cob_current_y = line;
		cob_current_x = column;
	}
}

static size_t
cob_prep_input (cob_screen *s)
{
	struct cob_inp_struct	*sptr;

	switch (s->type) {
	case COB_SCREEN_TYPE_GROUP:
		cob_screen_moveyx (s);
		for (s = s->child; s; s = s->next) {
			cob_prep_input (s);
		}
		break;
	case COB_SCREEN_TYPE_FIELD:
		cob_screen_puts (s, s->field, cobsetptr->cob_legacy,
				 ACCEPT_STATEMENT);
		if (s->attr & COB_SCREEN_INPUT) {
			if (totl_index >= COB_INP_FLD_MAX) {
				return 1;
			}
			sptr = cob_base_inp + totl_index;
			sptr->scr = s;
			sptr->this_y = cob_current_y;
			sptr->this_x = cob_current_x;
			totl_index++;
		}
		break;
	case COB_SCREEN_TYPE_VALUE:
		cob_screen_puts (s, s->value, cobsetptr->cob_legacy,
				 ACCEPT_STATEMENT);
		if (s->occurs) {
			int		n;
			for (n = 1; n < s->occurs; ++n) {
				cob_screen_puts (s, s->value, cobsetptr->cob_legacy,
						 ACCEPT_STATEMENT);
			}
		}
		break;
	case COB_SCREEN_TYPE_ATTRIBUTE:
#if	0	/* RXWRXW - Attr */
		cob_screen_attr (s->foreg, s->backg, s->attr, NULL, NULL, stmt);
#endif
		break;
	default:
		break;
	}
	return 0;
}

static void
cob_screen_iterate (cob_screen *s)
{
	switch (s->type) {
	case COB_SCREEN_TYPE_GROUP:
		cob_screen_moveyx (s);
		for (s = s->child; s; s = s->next) {
			cob_screen_iterate (s);
		}
		break;
	case COB_SCREEN_TYPE_FIELD:
		cob_screen_puts (s, s->field, 0, DISPLAY_STATEMENT);
		break;
	case COB_SCREEN_TYPE_VALUE:
		cob_screen_puts (s, s->value, 0, DISPLAY_STATEMENT);
		if (s->occurs) {
			int		n;
			for (n = 1; n < s->occurs; ++n) {
				cob_screen_puts (s, s->value, 0,
						 DISPLAY_STATEMENT);
			}
		}
		break;
	case COB_SCREEN_TYPE_ATTRIBUTE:
		cob_screen_attr (s->foreg, s->backg, s->attr, NULL, NULL,
				 DISPLAY_STATEMENT);
		break;
	default:
		break;
	}
}

static void
get_line_column (cob_field *fline, cob_field *fcol, int *line, int *col)
{
	if (fline == NULL) {
		*line = 1;
	} else {
		*line = cob_get_int (fline);
	}

	if (fcol == NULL) {
		*col = 1;
	} else {
		*col = cob_get_int (fcol);
	}
}

static COB_INLINE COB_A_INLINE int
col_where_last_stmt_ended (const enum screen_statement stmt)
{
	return stmt == DISPLAY_STATEMENT ? display_cursor_x : accept_cursor_x;
}

static COB_INLINE COB_A_INLINE int
line_where_last_stmt_ended (const enum screen_statement stmt)
{
	return stmt == DISPLAY_STATEMENT ? display_cursor_y : accept_cursor_y;
}

/* resolve line + column from COBOL variables (several combinations + sizes)
   and store them in the passed sline/column */
static void
extract_line_and_col_vals (cob_field *line, cob_field *column,
			   const enum screen_statement stmt,
			   const int zero_line_col_allowed,
			   int *sline, int *scolumn)
{
	int cobol_line;
	int cobol_col;

	if (column == NULL) {
		if (line == NULL) {
			*sline = 0;
			*scolumn = 0;
			return;
		}
		if (line->size < 4) {
			/* this is used when only a LINE clause is specified,
			   not an AT clause */
			cobol_line = cob_get_int (line);
			cobol_col = 1;
		} else {
			/* common case: line actually contains both the
			   line and field numbers */
			int ret = get_line_and_col_from_field
				(line, &cobol_line, &cobol_col);
			/* LCOV_EXCL_START */
			if (unlikely (ret == 1)) {
#if 0			/* Throw an exception? EC-SCREEN-IMP-LINE-VAR-LENGTH? */
				cob_set_exception (COB_EC_SCREEN_IMP);
#else
				cob_fatal_error (COB_FERROR_CODEGEN);
#endif
			}
			/* LCOV_EXCL_STOP */
		}
	} else {
		get_line_column (line, column, &cobol_line, &cobol_col);
	}

	if (cobol_line == 0) {
		if (cobol_col == 0) {
			if (zero_line_col_allowed) {
				*sline = line_where_last_stmt_ended (stmt);
				*scolumn = col_where_last_stmt_ended (stmt);
			} else {
				cob_set_exception (COB_EC_SCREEN_LINE_NUMBER);
				cob_set_exception (COB_EC_SCREEN_STARTING_COLUMN);
				*sline = 0;
				*scolumn = 0;
			}
		} else {
			if (zero_line_col_allowed) {
				*sline = line_where_last_stmt_ended (stmt) + 1;
			} else {
				cob_set_exception (COB_EC_SCREEN_LINE_NUMBER);
				*sline = 0;
			}
			*scolumn = cobol_col - 1;
		}
	} else if (cobol_col == 0) {
		*sline = cobol_line - 1;
		if (zero_line_col_allowed) {
			*scolumn = col_where_last_stmt_ended (stmt);
		} else {
			cob_set_exception (COB_EC_SCREEN_STARTING_COLUMN);
			*scolumn = 0;
		}
	} else {
		*sline = cobol_line - 1;
		*scolumn = cobol_col - 1;
	}

	/* TO-DO: If scolumn == max_x + 1, go to start of next line */
}

static void
screen_display (cob_screen *s, const int line, const int column)
{
	int		status;

	origin_y = line;
	origin_x = column;

	status = cob_move_cursor (line, column);
	if (status != ERR) {
		pending_accept = 1;
	}
	cob_screen_iterate (s);
	refresh_mywin (mywin);
}

static int
get_accept_timeout (cob_field *ftimeout)
{
	if (ftimeout) {
		/* FIXME: the scale should come primarily from the module,
		   TODO:  add scale field to module_ptr */
		return cob_get_int (ftimeout) * COB_TIMEOUT_SCALE;
	} else {
		return -1;
	}
}

static void
screen_accept (cob_screen *s, const int line, const int column,
	       cob_field *ftimeout)
{
	struct cob_inp_struct	*sptr;
	struct cob_inp_struct	*sptr2;
	size_t			idx;
	size_t			n;
	size_t			posu;
	size_t			posd;
	size_t			prevy;
	size_t			firsty;
	int			starty;
	int			initial_curs;
	int			accept_timeout;

	init_cob_screen_if_needed ();
	if (!cob_base_inp) {
		cob_base_inp = cob_malloc (COB_INP_SIZE);
	} else {
		memset (cob_base_inp, 0, COB_INP_SIZE);
	}
	cobglobptr->cob_exception_code = 0;
	cob_current_y = 0;
	cob_current_x = 0;
	totl_index = 0;
	origin_y = line;
	origin_x = column;

	cob_move_cursor (line, column);

	/* Prepare input fields */
	if (cob_prep_input (s)) {
		pass_cursor_to_program ();
		handle_status (9001, ACCEPT_STATEMENT);
		return;
	}

	/* No input field is an error */
	if (!totl_index) {
		pass_cursor_to_program ();
		handle_status (8000, ACCEPT_STATEMENT);
		return;
	}

	accept_timeout = get_accept_timeout (ftimeout);

	/* Sort input fields on line, column
	   --> breaks standard and other vendors compatiblilty "in the order defined" */
	qsort (cob_base_inp, totl_index,
	       sizeof(struct cob_inp_struct), compare_yx);

	posu = 0;
	posd = 0;
	prevy = 0;
	firsty = 0;
	sptr = cob_base_inp;
	starty = sptr->this_y;
	initial_curs = -1;
	/* Set up array for Cursor UP/DOWN */
	for (n = 0; n < totl_index; n++) {
		sptr = cob_base_inp + n;
		if ((sptr->scr->attr & COB_SCREEN_INITIAL) && initial_curs < 0) {
			initial_curs = (int)n;
		}
		if (sptr->this_y > starty) {
			if (!firsty) {
				firsty = n;
			}
			starty = sptr->this_y;
			sptr2 = cob_base_inp + posd;
			for (idx = posd; idx < n; idx++, sptr2++) {
				sptr2->down_index = n;
			}
			posu = prevy;
			prevy = n;
			posd = n;
		}
		sptr->up_index = posu;
	}
	sptr = cob_base_inp;
	for (n = 0; n < firsty; n++, sptr++) {
		sptr->up_index = posd;
	}
	global_return = 0;
	if (initial_curs < 0) {
		initial_curs = 0;
	}
	cob_screen_get_all (initial_curs, accept_timeout);
	pass_cursor_to_program ();
	handle_status (global_return, ACCEPT_STATEMENT);
}

static void
field_display (cob_field *f, cob_flags_t fattr, const int line, const int column,
	       cob_field *fgc, cob_field *bgc, cob_field *fscroll,
	       cob_field *size_is, cob_field *control, cob_field *color)
{
	int	sline;
	int	scolumn;
	int	size_display, fsize;
	int	status;
	char	fig_const;	/* figurative constant character */

	/* LCOV_EXCL_START */
	if (unlikely (!f)) {
		cob_fatal_error (COB_FERROR_CODEGEN);
	}
	/* LCOV_EXCL_STOP */

	origin_y = 0;
	origin_x = 0;

	fsize = (int)f->size;
	if (size_is) {
		size_display = (unsigned int)cob_get_int (size_is);
		/* SIZE ZERO is ignored */
		if (size_display == 0) {
			size_display = fsize;
		}
	} else if (fattr & COB_SCREEN_NO_DISP) {
		size_display = 0;
	} else {
		size_display = fsize;
	}

	if (fscroll) {
		sline = cob_get_int (fscroll);
		if (fattr & COB_SCREEN_SCROLL_DOWN) {
			sline = -sline;
		}
		scrollok (mywin, 1);
		wscrl (mywin ,sline);
		scrollok (mywin, 0);
		refresh_mywin (mywin);
	}

	sline = line;
	scolumn = column;
	status = cob_move_cursor (sline, scolumn);
	if (status != ERR) {
		pending_accept = 1;
	}

	fattr = cob_screen_attr (fgc, bgc, fattr, control, color, DISPLAY_STATEMENT);

	if (!(fattr & COB_SCREEN_NO_DISP)) {
		/* figurative constant and WITH SIZE repeats the literal */
		if (size_is
		 && f->attr->type == COB_TYPE_ALPHANUMERIC_ALL) {
			if ((int)f->size == 1) {
				fig_const = f->data[0];
				cob_addnch (size_display, fig_const);
			} else {
				int i;
				for (i = 0; i < (size_display / fsize); ++i) {
					cob_addnstr ((char *)f->data, fsize);
				}
				cob_addnstr ((char *)f->data, size_display % fsize);
			}
		} else {
			if (fattr & COB_SCREEN_GRAPHICS) {
				cob_addnstr_graph ((char *)f->data, cob_min_int (size_display, fsize));
			} else {
				cob_addnstr ((char *)f->data, cob_min_int (size_display, fsize));
			}
			if (size_display > fsize) {
				/* WITH SIZE larger than field displays trailing spaces */
				cob_addnch (size_display - fsize, COB_CH_SP);
			}
		}
	}

	display_cursor_y = sline;
	display_cursor_x = scolumn + size_display;

	if (fattr & COB_SCREEN_EMULATE_NL) {
		if (++sline >= LINES) {
			sline = 0;
		}
		cob_move_cursor (sline, 0);
	}
	refresh_mywin (mywin);
}

static void
field_accept (cob_field *f, cob_flags_t fattr, const int sline, const int scolumn,
		  cob_field *fgc, cob_field *bgc, cob_field *fscroll, cob_field *ftimeout,
		  cob_field *prompt, cob_field *size_is, cob_field *cursor,
		  cob_field *control, cob_field *color)
{
	unsigned char	*p;
	unsigned char	*p2;
	size_t		count;
	int		keyp;
	int		fret = 0;
	int		cline = 0;
	size_t		ccolumn = 0;
	size_t		right_pos;
	int		at_eof = 0;
	unsigned char	move_char;      /* data shift character */
	int		status;
	chtype		prompt_char;    /* prompt character */
	chtype		default_prompt_char;
#ifdef HAVE_MOUSEMASK
	MEVENT		mevent;
#endif

	size_t		size_accept = 0;	/* final size to accept */
	cob_field	temp_field;

#if	0	/* RXWRXW - Screen update */
	cob_field	char_temp;
	unsigned char	space_buff[4];

	char_temp.data = space_buff;
	char_temp.attr = &const_alpha_attr;
	char_temp.size = 1;
	space_buff[0] = ' ';
	space_buff[1] = 0;
#endif

	origin_y = 0;
	origin_x = 0;
#ifdef HAVE_MOUSEMASK
	/* prevent warnings about not intialized structure */
	memset (&mevent, 0, sizeof (MEVENT));
#endif

	/* Set the default prompt character */
	if (prompt) {
		default_prompt_char = prompt->data[0];
	} else {
		default_prompt_char = COB_CH_UL;
	}
	init_cob_screen_if_needed ();


	if (fscroll) {
		keyp = cob_get_int (fscroll);
		if (fattr & COB_SCREEN_SCROLL_DOWN) {
			keyp = -keyp;
		}
		scrollok (mywin, 1);
		wscrl (mywin ,keyp);
		scrollok (mywin, 0);
		refresh_mywin (mywin);
	}
	cobglobptr->cob_exception_code = 0;

	status = cob_move_cursor (sline, scolumn);
	if (status != ERR) {
		pending_accept = 0;
	}

	cob_screen_attr (fgc, bgc, fattr, control, color, ACCEPT_STATEMENT);

	if (f) {
		if (size_is) {
			size_accept = cob_get_int (size_is);
			/* SIZE ZERO is ignored */
			if (size_accept < 1) {
				size_accept = (int)f->size;
			}
		} else {
			size_accept = f->size;
		}

		p = COB_TERM_BUFF;
		temp_field.data = COB_TERM_BUFF;
		temp_field.attr = &const_alpha_attr;
		temp_field.size = size_accept;
		if (fattr & COB_SCREEN_UPDATE) {
			cob_move (f, &temp_field);	/* updates COB_TERM_BUFF */
		} else {
			memset (COB_TERM_BUFF, ' ', size_accept);
		}

		raise_ec_on_truncation (size_accept);
		{
			const size_t disp_size = (size_t)cob_min_int (size_accept, f->size);
			const unsigned char *p_set = p;
			for (count = 0; count < disp_size; count++) {
				if (*p && *p != ' ') {
					p_set = p;
				}
				if (fattr & COB_SCREEN_SECURE) {
					cob_addch_no_trunc_check (COB_CH_AS);
				} else if (fattr & COB_SCREEN_NO_ECHO) {
					cob_addch_no_trunc_check (COB_CH_SP);
				} else if (fattr & COB_SCREEN_UPDATE) {
					cob_addch_no_trunc_check ((const chtype)*p++);
				} else if (COB_FIELD_IS_NUMERIC (f)) {
					cob_addch_no_trunc_check ('0');
				} else if (fattr & COB_SCREEN_PROMPT) {
					cob_addch_no_trunc_check (default_prompt_char);
				} else {
					cob_addch_no_trunc_check (COB_CH_SP);
				}
			}
			/* SIZE IS greater than field, blank out trailing screen */
			if (size_accept > f->size) {
				cob_addnch (size_accept - f->size, COB_CH_SP);
			}
			/* start position within the field, if specified (all 1-based) */
			{
				int cursor_off = 1;
				if (cursor) {
					/* position according to CURSOR clause */
					cursor_off = cob_get_int (cursor);
					if (cursor_off >= 1) {
						/* max: last_position with data */
						int last_data = p_set - COB_TERM_BUFF + 1;
						if (last_data < cursor_off) {
							cursor_off = last_data;
						}
					} else {
						cursor_off = 1;
					}
				} else {
					/* position from the SPECIAL-NAMES CURSOR clause */
					int		cursor_clause_line;
					int		cursor_clause_col;
					get_cursor_from_program (&cursor_clause_line, &cursor_clause_col);

					if (cursor_clause_line == sline
					 && cursor_clause_col > scolumn
					 && cursor_clause_col < scolumn + disp_size) {
						cursor_off = cursor_clause_col - scolumn + 1;
					}
				}
				wmove (mywin ,sline, scolumn + cursor_off - 1);
			}
		}
#if	0	/* RXWRXW - Screen update */
		if (!(fattr & COB_SCREEN_UPDATE)) {
			if (cob_field_is_numeric_or_numeric_edited (f)) {
				cob_set_int (f, 0);
			} else {
				cob_move (&char_temp, f);
			}
		}
#endif

		/* positioning for following ACCEPTs */
		accept_cursor_y = sline;
		accept_cursor_x = scolumn + size_accept;

		right_pos = scolumn + size_accept - 1;
		p = COB_TERM_BUFF;
	} else {
		right_pos = 0;
		p = NULL;
	}
	count = 0;

	wtimeout (mywin ,get_accept_timeout (ftimeout));

	/* Get characters from keyboard, processing each one. */
	for (; ;) {
		/* Show prompt characters. */
		if (f) {
			/* Get current line, column. */
			getyx (mywin, cline, ccolumn);
			/* Trailing prompts. */
			if (fattr & COB_SCREEN_NO_ECHO) {
				prompt_char = COB_CH_SP;
			} else if (COB_FIELD_IS_NUMERIC (f)) {
				prompt_char = '0';
			} else if (fattr & COB_SCREEN_PROMPT) {
				prompt_char = default_prompt_char;
			} else {
				prompt_char = COB_CH_SP;
			}
			for (count = right_pos; (int)count > scolumn - 1; count--) {
				/* Get character */
				p2 = COB_TERM_BUFF + count - scolumn;
				move_char = *p2;
				/* Field prompts. */
				if (COB_FIELD_IS_NUMERIC (f)) {
					/* Numeric prompt zeros. */
					if (move_char == '0') {
						cob_move_cursor (cline, count);
						cob_addch (prompt_char);
					} else {
						/* Switch to remove prompts from within field. */
						if (fattr & COB_SCREEN_NO_ECHO) {
							prompt_char = COB_CH_SP;
						} else if (fattr & COB_SCREEN_SECURE) {
							prompt_char = COB_CH_AS;
						} else {
							prompt_char = '0';
						}
					}
				} else {
					/* Alpha prompts. */
					if (move_char == ' ') {
						cob_move_cursor (cline, count);
						cob_addch (prompt_char);
					} else {
						/* Switch to remove prompts from within field. */
						if (fattr & COB_SCREEN_NO_ECHO) {
							prompt_char = COB_CH_SP;
						} else if (fattr & COB_SCREEN_SECURE) {
							prompt_char = COB_CH_AS;
						} else {
							prompt_char = COB_CH_SP;
						}
					}
				}
			}
			/* Cursor to current column. */
			cob_move_cursor (cline, ccolumn);
			/* Refresh screen. */
			refresh_mywin (mywin);
		} else if (mywin != stdscr) {
			refresh_mywin (mywin);
		}
		errno = 0;

		/* Get a character. */
		keyp = getch ();

		/* Key error - time out. */
		if (keyp == ERR) {
			fret = 8001;
			goto field_return;
		}
		/* Return function keys F1 through F64 */
		if (keyp > KEY_F0 && keyp < KEY_F(65)) {
			fret = 1000 + keyp - KEY_F0;
			goto field_return;
		}

		cob_convert_key (&keyp, 1U);
		if (keyp <= 0) {
			(void)flushinp ();
			cob_beep ();
			continue;
		}

#ifdef HAVE_MOUSEMASK
		/* get mouse event here, handle later */
		if (keyp == KEY_MOUSE) {
			getmouse (&mevent);
			/* in case of left double-click:
			   always translate to ENTER in SCREEN ACCEPT;
			   exception: user requested control of this */
			if (mevent.bstate & BUTTON1_DOUBLE_CLICKED
			 && !(cob_mask_accept & BUTTON1_DOUBLE_CLICKED)) {
				keyp = KEY_ENTER;
			}
		}
#endif

		/* Return special keys */
		switch (keyp) {
		case KEY_ENTER:
			/* Enter. */
			goto field_return;
		case KEY_PPAGE:
			/* Page up. */
			fret = 2001;
			goto field_return;
		case KEY_NPAGE:
			/* Page down. */
			fret = 2002;
			goto field_return;
		case KEY_UP:
			/* Up arrow. */
			fret = 2003;
			goto field_return;
		case KEY_DOWN:
			/* Down arrow. */
			fret = 2004;
			goto field_return;
		case KEY_PRINT:
			/* Print key. */
			/* pdcurses not returning this ? */
			fret = 2006;
			goto field_return;
		case 033:
			/* Escape key. */
			fret = 2005;
			goto field_return;
		case KEY_STAB:
			/* Tab key. */
			fret = 2007;
			goto field_return;
		case KEY_BTAB:
			/* Shift-Tab key, Back tab. */
			fret = 2008;
			goto field_return;
		default:
			break;
		}

		/* extension: ACCEPT OMITTED */
		if (unlikely (!f)) {
			/* special keys for ACCEPT OMITTED */
			switch (keyp) {
			case KEY_LEFT:
				fret = 2009;
				goto field_return;
			case KEY_RIGHT:
				fret = 2010;
				goto field_return;
			case KEY_IC:
				/* Insert key. */
				fret = 2011;
				goto field_return;
			case KEY_DC:
				/* Delete key. */
				fret = 2012;
				goto field_return;
			case KEY_BACKSPACE:
				/* Backspace key. */
				fret = 2013;
				goto field_return;
			case KEY_HOME:
				/* Home key. */
				fret = 2014;
				goto field_return;
			case KEY_END:
				/* End key. */
				fret = 2015;
				goto field_return;
#ifdef HAVE_MOUSEMASK
			case KEY_MOUSE:
			{
				int mline = mevent.y;
				int mcolumn = mevent.x;
			if (!wmouse_trafo(mywin, &mline, &mcolumn, 0)) {
				cob_beep ();
				continue;
			}
				mevent.bstate &= cob_mask_accept;
				if (mevent.bstate != 0) {
					fret = mouse_to_exception_code (mevent.bstate);
					cob_move_cursor (mline, mcolumn);	/* move cursor to pass position */
					goto field_return;
				}
			}
#endif
			default:
				(void)flushinp ();
				cob_beep ();
				continue;
			}
		}

		/* Positioning keys */
		switch (keyp) {
		case KEY_BACKSPACE:
			/* Backspace key. */
			if ((int) ccolumn > scolumn) {
				/* Shift remainder left with cursor. */
				for (count = ccolumn; count < right_pos + 1; count++) {
					/* Get character. */
					p2 = COB_TERM_BUFF + count - scolumn ;
					move_char = *p2;
					/* Move the character left. */
					p2 = COB_TERM_BUFF + count - scolumn - 1;
					*p2 = move_char;
					/* Update screen with moved character. */
					cob_move_cursor (cline, count - 1);
					if (fattr & COB_SCREEN_NO_ECHO) {
						cob_addch (COB_CH_SP);
					} else if (fattr & COB_SCREEN_SECURE) {
						cob_addch (COB_CH_AS);
					} else {
						cob_addch (move_char);
					}
				}
				/* Put space as the right most character. */
				p2 = COB_TERM_BUFF + size_accept - 1;
				if (fattr & COB_SCREEN_NO_ECHO) {
					*p2 = COB_CH_SP;
				} else if (COB_FIELD_IS_NUMERIC (f)) {
					*p2 = '0';
				} else {
					*p2 = COB_CH_SP;
				}
				/* Move cursor left one from current. */
				ccolumn--;
				cob_move_cursor (cline, ccolumn);
				p--;
			} else {
				cob_beep ();
			}
			at_eof = 0;
			continue;
		case KEY_HOME:
			/* HOME key. */
			/* Prepare for empty field. */
			move_char = ' ';
			/* Find non-blank character left to right. */
			for (count = scolumn; count <= right_pos; count++) {
				/* Get character. */
				p2 = COB_TERM_BUFF + count - scolumn;
				move_char = *p2;
				/* Stop at beginning non-blank character. */
				if (move_char != ' ') {
					break;
				}
			}
			/* Empty field. */
			if (move_char == ' ') {
				count = ccolumn;
			}
			/* Toggle between start of characters or start of field. */
			if (count != ccolumn) {
				/* Cursor to start of characters. */
				ccolumn = count;
				cob_move_cursor (cline, ccolumn);
				p = COB_TERM_BUFF + ccolumn - scolumn;
			} else {
				/* Cursor to start of field. */
				cob_move_cursor (sline, scolumn);
				p = COB_TERM_BUFF;
			}
			/* Reset */
			at_eof = 0;
			continue;
		case KEY_END:
			/* END key. */
			/* Prepare for empty field. */
			move_char = ' ';
			/* Find non-blank character right to left. */
			for (count = right_pos; (int) count >= scolumn; count--) {
				/* Get character. */
				p2 = COB_TERM_BUFF + count - scolumn;
				move_char = *p2;
				/* Stop at ending non-blank character. */
				if (move_char != ' ') {
					break;
				}
			}
			/* Empty field. */
			if (move_char == ' ') {
				count = ccolumn;
			} else {
				/* Cursor to first blank after ending character. */
				if (count != right_pos) {
					count++;
				}
			}
			/* Toggle between end of characters or end of field. */
			if (count != ccolumn) {
				/* Cursor after end character. */
				ccolumn = count;
				cob_move_cursor (cline, ccolumn);
				p = COB_TERM_BUFF + ccolumn - scolumn;
			} else {
				/* Cursor to end of size of field */
				cob_move_cursor (sline, right_pos);
				p = COB_TERM_BUFF + size_accept - 1;
			}
			/* Reset */
			at_eof = 0;
			continue;
		case KEY_LEFT:
		case ALT_LEFT:
			/* Left-arrow     KEY_LEFT auto-skip. */
			/* Alt-left-arrow ALT_LEFT no auto-skip. */
			at_eof = 0;
			if ((int) ccolumn > scolumn) {
				ccolumn--;
				cob_move_cursor (cline, ccolumn);
				p = COB_TERM_BUFF + ccolumn - scolumn;
				continue;
			}
			/* End of field, auto-skip, return left-arrow. */
			if (fattr & COB_SCREEN_AUTO && keyp == KEY_LEFT) {
				fret = 2009;
				goto field_return;
			}
			cob_beep ();
			continue;
		case KEY_RIGHT:
		case ALT_RIGHT:
			/* Right-arrow     KEY_RIGHT auto-skip. */
			/* Alt-right-arrow ALT_RIGHT no auto-skip. */
			if (ccolumn < right_pos) {
				ccolumn++;
				cob_move_cursor (cline, ccolumn);
				p = COB_TERM_BUFF + ccolumn - scolumn;
				continue;
			}
			/* End of field, auto-skip, return right-arrow. */
			if (fattr & COB_SCREEN_AUTO && keyp == KEY_RIGHT) {
				fret = 2010;
				goto field_return;
			}
			cob_beep ();
			continue;
		case KEY_IC:
			/* Insert key toggle */
			cob_toggle_insert ();
			continue;
		case KEY_DC:
			/* Delete key. */
			/* Delete character, move remainder left. */
			for (count = ccolumn; count < right_pos; count++) {
				/* Get character one position to right. */
				p2 = COB_TERM_BUFF + count - scolumn + 1;
				move_char = *p2;
				/* Move the character left. */
				p2 = COB_TERM_BUFF + count - scolumn;
				*p2 = move_char;
				/* Update screen with moved character. */
				cob_move_cursor (cline, count);
				if (fattr & COB_SCREEN_NO_ECHO) {
					cob_addch (COB_CH_SP);
				} else if (fattr & COB_SCREEN_SECURE) {
					cob_addch (COB_CH_AS);
				} else {
					cob_addch (move_char);
				}
			}
			/* Put space as the right most character. */
			p2 = COB_TERM_BUFF + size_accept - 1;
			if (fattr & COB_SCREEN_NO_ECHO) {
				*p2 = COB_CH_SP;
			} else if (COB_FIELD_IS_NUMERIC (f)) {
				*p2 = '0';
			} else {
				*p2 = COB_CH_SP;
			}
			/* Put cursor back to original position. */
			cob_move_cursor (cline, ccolumn);
			continue;
		case ALT_DEL:
			/* Alt-Delete key, erase cursor to end of field. */
			for (count = ccolumn; count <= right_pos; count++) {
				/* Character position. */
				p2 = COB_TERM_BUFF + count - scolumn;
				/* Blank character. */
				if (fattr & COB_FIELD_IS_NUMERIC (f)) {
					move_char = '0';
				} else {
					move_char = COB_CH_SP;
				}
				*p2 = move_char;
				/* Update screen with blank character. */
				cob_move_cursor (cline, count);
				if (fattr & COB_SCREEN_NO_ECHO) {
					cob_addch (COB_CH_SP);
				} else if (fattr & COB_SCREEN_SECURE) {
					cob_addch (COB_CH_AS);
				} else {
					cob_addch (move_char);
				}
			}
			/* Put cursor back to original position. */
			cob_move_cursor (cline, ccolumn);
			continue;

#ifdef HAVE_MOUSEMASK
		case KEY_MOUSE:
		{
			int mline = mevent.y;
			int mcolumn = mevent.x;
			if (!wmouse_trafo(mywin, &mline, &mcolumn, 0)) {
				cob_beep ();
				continue;
			}
			/* handle depending on state */
			if (mevent.bstate & BUTTON1_PRESSED
			 && COB_MOUSE_FLAGS & 1) {
				/* if in current field, just move */
				if (mline == cline) {
					if (mcolumn >= scolumn
					 && mcolumn <= (int)right_pos) {
						ccolumn = mcolumn;
						cob_move_cursor (cline, ccolumn);
						p = COB_TERM_BUFF + ccolumn - scolumn;
						continue;
					}
				}
				/* CHECKME: shouldn't we have a finalize here? */
			}
			mevent.bstate &= cob_mask_accept;
			if (mevent.bstate != 0) {
				fret = mouse_to_exception_code (mevent.bstate);
				cob_move_cursor (mline, mcolumn);	/* move cursor to pass position */
				goto field_return;
			}
			continue;
		}
#endif
		default:
			break;
		}

		/* Handle printable character. */
#if 0 /* FIXME: we can't handle anything > UCHAR_MAX here because of
		*p = (unsigned char) keyp;
		--> revise */
		if (keyp > 037 && keyp < (int)A_CHARTEXT) {
#else
		if (keyp > 037 && keyp <= UCHAR_MAX) {
#endif
			/* Numeric field check. */
			if (cob_field_is_numeric_or_numeric_edited (f)) {
				if (keyp < '0' || keyp > '9') {
					cob_beep ();
					continue;
				}
			}

			/* Handle UPPER/LOWER. */
			if (fattr & COB_SCREEN_UPPER) {
				keyp = toupper ((unsigned char)keyp);
			} else if (fattr & COB_SCREEN_LOWER) {
				keyp = tolower ((unsigned char)keyp);
			}

			/* Insert character, if requested. */
			if (COB_INSERT_MODE && size_accept > 1) {
				/* get last character in field */
				/* check and beep if field is already full,
				   ignore numeric fields for now */
				if (cob_field_is_numeric_or_numeric_edited (f)) {
					p2 = (unsigned char *)" ";
				} else {
					p2 = COB_TERM_BUFF + right_pos - scolumn;
				}
				if (*p2 != ' ') {
					cob_beep ();
					continue;
				}
				/* Move remainder to the right. */
				for (count = right_pos; count > ccolumn; count--) {
					/* Get character */
					p2 = COB_TERM_BUFF + count - scolumn - 1;
					move_char = *p2;
					/* Move character one right. */
					p2 = COB_TERM_BUFF + count - scolumn;
					*p2 = move_char;
					/* Update screen with moved character. */
					if ((int) count > scolumn) {
						cob_move_cursor (cline, count);
						if (move_char != ' ') {
							if (fattr & COB_SCREEN_NO_ECHO) {
								cob_addch (COB_CH_SP);
							} else if (fattr & COB_SCREEN_SECURE) {
								cob_addch (COB_CH_AS);
							} else {
								cob_addch (move_char);
							}
						}
					}
				}
				cob_move_cursor (cline, ccolumn);
			}

			/* actual storing the key */
			*p = (unsigned char)keyp;

			count = 1;
			/* Display character or '*' if secure. */
			if (fattr & COB_SCREEN_SECURE) {
				cob_addch (COB_CH_AS);
			} else if (fattr & COB_SCREEN_NO_ECHO) {
				cob_addch (COB_CH_SP);
			} else {
				cob_addch ((const chtype)keyp);
			}
			if (ccolumn == right_pos) {
				/* Auto-skip at end of field. */
				if (fattr & COB_SCREEN_AUTO) {
					break;
				}
				cob_move_cursor (cline, ccolumn);
				/* check if we (still) are at last position and inform
				   user with a beep (after having processed his key) */
				if (at_eof) {
					cob_beep ();
				} else {
					at_eof = 1;
				}
			} else {
				p++;
			}
			continue;
		}
		(void)flushinp ();
		cob_beep ();
	}


 field_return:
	/* return position within the field, if specified (all 1-based) */
	if (cursor) {
		/* horizontal position stored in CURSOR clause */
		if (!COB_FIELD_CONSTANT (cursor)) {
			getyx (mywin, cline, ccolumn);
			if (cline == sline) {
				cob_set_int (cursor, ccolumn + 1 - scolumn);
			}
		}
	} else {
		/* screen position stored in the SPECIAL-NAMES CURSOR clause */
		pass_cursor_to_program ();
	}
	handle_status (fret, ACCEPT_STATEMENT);
	if (f) {
		cob_move (&temp_field, f);
		cob_move_cursor (sline, right_pos + 1);
#if 0	/* possible cleanup to not "leak" input data */
		memset (COB_TERM_BUFF, ' ', size_accept);
#endif
	}
	refresh_mywin (mywin);
}

static void
field_accept_from_curpos (cob_field *f, cob_field *fgc,
	      cob_field *bgc, cob_field *fscroll, cob_field *ftimeout,
	      cob_field *prompt, cob_field *size_is, const cob_flags_t fattr)
{
	int		cline;
	size_t		ccolumn;

	/* Get current line, column. */
	getyx (mywin, cline, ccolumn);

	/* accept field */
	field_accept (f, (cob_flags_t)fattr, cline, ccolumn, fgc, bgc,
			fscroll, ftimeout, prompt, size_is, NULL, NULL, NULL);
}

static void
field_display_at_curpos (cob_field *f,
	cob_field *fgc, cob_field *bgc, cob_field *fscroll,
	cob_field *size_is, const cob_flags_t fattr)
{
	int		cline;
	size_t		ccolumn;

	/* Get current line, column. */
	getyx (mywin, cline, ccolumn);

	field_display (f, (cob_flags_t)fattr, cline, ccolumn,
			fgc, bgc, fscroll, size_is, NULL, NULL);
}

/* Global functions */

/* DISPLAY scr-name */
void
cob_screen_display (cob_screen *s, cob_field *line, cob_field *column,
		    const int zero_line_col_allowed)
{
	int	sline;
	int	scolumn;
	init_cob_screen_if_needed ();

	extract_line_and_col_vals (line, column, DISPLAY_STATEMENT,
				zero_line_col_allowed, &sline, &scolumn);
	screen_display (s, sline, scolumn);
}

/* ACCEPT scr-name */
void
cob_screen_accept (cob_screen *s, cob_field *line, cob_field *column,
		   cob_field *ftimeout, const int zero_line_col_allowed)
{
	int	sline;
	int	scolumn;

	extract_line_and_col_vals (line, column, ACCEPT_STATEMENT,
				zero_line_col_allowed, &sline, &scolumn);
	screen_accept (s, sline, scolumn, ftimeout);
}

/* DISPLAY scr-item WITH/AT */
void
cob_display_field (cob_field *f, const cob_flags_t fattr, const char *parms, ...)
{
	cob_field *line = NULL;
	cob_field *column = NULL;
	cob_field *fgc = NULL;
	cob_field *bgc = NULL;
	cob_field *fscroll = NULL;
	cob_field *size_is = NULL;
	cob_field *control = NULL;
	cob_field *color = NULL;

	/*
	  LINE/COL 0 is always allowed here as it is impossible to specify it in
	  the standard format (DISPLAY ... UPON CRT) and all implementations of
	  the extended screen format (DISPLAY ... WITH UNDERLINE, HIGHLIGHT, etc.)
	  require it.
	*/
	const int zero_line_col_allowed = 1;

	int 	sline;
	int 	scolumn;

	if (parms && *parms) {
		va_list 	args;
		const char *p = parms;

		va_start (args, parms);
		for (;;) {
			char type = *p++;
			if (type == 0) {
				break;
			}
			switch (type) {
			case 'p':	/* AT POS, currently combined, likely to be changed later */
			case 'l':	/* AT LINE */
				line = va_arg (args, cob_field *);
				break;
			case 'c':	/* AT COLUMN */
				column = va_arg (args, cob_field *);
				break;
			case 'f':	/* FOREGROUND-COLOR IS */
				fgc = va_arg (args, cob_field *);
				break;
			case 'b':	/* BACKGROUND-COLOR IS */
				bgc = va_arg (args, cob_field *);
				break;
			case 's':	/* SCROLL UP | DOWN */
				fscroll = va_arg (args, cob_field *);
				break;
			case 'S':	/* SIZE IS */
				size_is = va_arg (args, cob_field *);
				break;
			case 'C':	/* CONTROL -> variable named attributes */
				control = va_arg (args, cob_field *);
				break;
			case 'L':	/* CONTROL -> variable numeric added attributes */
				color = va_arg (args, cob_field *);
				break;
			default:
				/* unknown attributes are explicit ignored */
				break;
			}
			parms++;
		}
		va_end (args);
	}

	init_cob_screen_if_needed ();

	extract_line_and_col_vals (line, column, DISPLAY_STATEMENT,
			zero_line_col_allowed, &sline, &scolumn);
	field_display (f, fattr, sline, scolumn, fgc, bgc,
			fscroll, size_is, control, color);
}

/* ACCEPT scr-item WITH/AT */
void
cob_accept_field (cob_field *f, const cob_flags_t fattr, const char *parms, ...)
{
	cob_field *line = NULL;
	cob_field *column = NULL;
	cob_field *fgc = NULL;
	cob_field *bgc = NULL;
	cob_field *fscroll = NULL;
	cob_field *ftimeout = NULL;
	cob_field *prompt = NULL;
	cob_field *size_is = NULL;
	cob_field *control = NULL;
	cob_field *color = NULL;
	cob_field *cursor = NULL;

	const int zero_line_col_allowed = 1;	/* see comment in cob_display_field */

	int 	sline;
	int 	scolumn;

	if (parms && *parms) {
		va_list 	args;
		const char	*p = parms;

		va_start (args, parms);
		for (;;) {
			char type = *p++;
			if (type == 0) {
				break;
			}
			switch (type) {
			case 'p':	/* AT POS, currently combined, likely to be changed later */
			case 'l':	/* AT LINE */
				line = va_arg (args, cob_field *);
				break;
			case 'c':	/* AT COLUMN */
				column = va_arg (args, cob_field *);
				break;
			case 'f':	/* FOREGROUND-COLOR IS */
				fgc = va_arg (args, cob_field *);
				break;
			case 'b':	/* BACKGROUND-COLOR IS */
				bgc = va_arg (args, cob_field *);
				break;
			case 's':	/* SCROLL UP | DOWN */
				fscroll = va_arg (args, cob_field *);
				break;
			case 't':	/* TIME-OUT [AFTER] */
				ftimeout = va_arg (args, cob_field *);
				break;
			case 'P':	/* PROMPT CHARACTER OS */
				prompt = va_arg (args, cob_field *);
				break;
			case 'S':	/* SIZE IS */
				size_is = va_arg (args, cob_field *);
				break;
			case 'C':	/* CONTROL -> variable named attributes */
				control = va_arg (args, cob_field *);
				break;
			case 'L':	/* CONTROL -> variable numeric added attributes */
				color = va_arg (args, cob_field *);
				break;
			case 'R':	/* CURSOR -> offset within field */
				cursor = va_arg (args, cob_field *);
				break;
			default:
				/* unknown attributes are explicit ignored */
				break;
			}
			parms++;
		}
		va_end (args);
	}

	extract_line_and_col_vals (line, column, ACCEPT_STATEMENT,
			zero_line_col_allowed, &sline, &scolumn);
	field_accept (f, fattr, sline, scolumn, fgc, bgc,
			fscroll, ftimeout, prompt, size_is, cursor, control, color);
}

/* DISPLAY scr-item WITH/AT - compat-function for < 3.2 */
void
cob_field_display (cob_field *f, cob_field *line, cob_field *column,
		  cob_field *fgc, cob_field *bgc, cob_field *fscroll,
		  cob_field *size_is, const cob_flags_t fattr)
{
	const int zero_line_col_allowed = 1;	/* see comment in cob_display_field */

	int	sline;
	int	scolumn;

	init_cob_screen_if_needed ();
	extract_line_and_col_vals (line, column, DISPLAY_STATEMENT,
			zero_line_col_allowed, &sline, &scolumn);
	field_display (f, (cob_flags_t)fattr, sline, scolumn, fgc, bgc,
			fscroll, size_is, NULL, NULL);
}

/* ACCEPT scr-item WITH/AT - compat-function for < 3.2 */
void
cob_field_accept (cob_field *f, cob_field *line, cob_field *column,
		  cob_field *fgc, cob_field *bgc, cob_field *fscroll,
		  cob_field *ftimeout, cob_field *prompt, cob_field *size_is,
		  const cob_flags_t fattr)
{
	const int zero_line_col_allowed = 1;	/* see comment in cob_display_field */

	int	sline;
	int	scolumn;

	extract_line_and_col_vals (line, column, ACCEPT_STATEMENT,
			zero_line_col_allowed, &sline, &scolumn);
	field_accept (f, (cob_flags_t)fattr, sline, scolumn, fgc, bgc,
			fscroll, ftimeout, prompt, size_is, NULL, NULL, NULL);
}

/* x'E4' system call - clear screen */
int
cob_sys_clear_screen (void)
{
	init_cob_screen_if_needed ();

	wclear (mywin);
	refresh_mywin (mywin);
	cob_current_y = 0;
	cob_current_x = 0;
	return 0;
}

/* internal function to temporarily set "extended screen mode"
   to either on=1 or off=0;
   note: does _not_ adjust cob_screen_initialized */
void
cob_screen_set_mode (const cob_u32_t smode)
{
	/* note: called internally only, so no need to check for cobglobptr */

	if (!smode) {
		if (cobglobptr->cob_screen_initialized) {
			refresh_mywin (mywin);
			def_prog_mode ();
			endwin ();
		}
	} else {
		if (cobglobptr->cob_screen_initialized) {
			reset_prog_mode ();
			refresh_mywin (mywin);
		} else {
			cob_screen_init ();
		}
	}
}

/* display a C string without auto-newline */
int
cob_display_text (const char *text)
{
	cob_field	field;
	cob_field_attr	attr;

	init_cob_screen_if_needed ();

	if (text[0] == 0) return 0;

	COB_FIELD_INIT (strlen (text), (unsigned char *)text, &attr);
	COB_ATTR_INIT (COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL);

	field_display_at_curpos (&field, NULL, NULL, NULL, NULL, 0);

	return 0;
}

/* C: get a char x'01' thru x'255' or keyboard status > 1000  (or 0)
      without any prompt */
int
cob_get_char (void)
{
	cob_field		field;
	char			c = ' ';
	cob_field_attr		attr;

	init_cob_screen_if_needed ();

	COB_FIELD_INIT (1, (unsigned char *)&c, &attr);
	COB_ATTR_INIT (COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL);

	field_accept_from_curpos (&field, NULL, NULL, NULL, NULL, NULL, NULL,
		COB_SCREEN_AUTO | COB_SCREEN_NO_ECHO);

	/* CHECKME: MF docs are not clear: should this return 0 ? */
	if (c == ' ') {
		return COB_ACCEPT_STATUS;
#if EOF != -1
	} else if (c == EOF) {
		return -1;
#endif
	} else {
		return c;
	}
}

/* get a C string with given max-length - returns keyboard status */
int
cob_get_text (char *text, int size)
{
	cob_field	field;
	cob_field_attr	attr;

	init_cob_screen_if_needed ();

	if (size > 0) {
		COB_FIELD_INIT (size, (unsigned char *)text, &attr);
		COB_ATTR_INIT (COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL);
		field_accept_from_curpos (&field, NULL, NULL, NULL, NULL, NULL, NULL, 0);
	} else {
		field_accept (NULL, 0, 0, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
	}

	return COB_ACCEPT_STATUS;
}

/* display a formatted C string without auto-newline */
int
cob_display_formatted_text (const char *fmt, ...)
{
	int		size;
	cob_field	field;
	cob_field_attr	attr;
	va_list		ap;
	char		buff [COB_NORMAL_BUFF];

	init_cob_screen_if_needed ();

	va_start (ap, fmt);
	size = vsnprintf (buff, COB_NORMAL_BUFF, fmt, ap);
	va_end (ap);

	if (size < 0) {
		return -1;
	}

	if (buff[0] == 0) {
		return 0;
	}

	field.size = cob_min_int (size, COB_NORMAL_MAX);
	field.data = (unsigned char *)&buff;
	COB_ATTR_INIT (COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL);
	field.attr = &attr;

	field_display_at_curpos (&field, NULL, NULL, NULL, NULL, 0);

	return 0;
}

void
cob_exit_screen (void)
{
	cob_flags_t	flags;
	char		exit_msg[COB_MINI_BUFF];

	if (!cobglobptr) {
		return;
	}
	if (cobglobptr->cob_screen_initialized) {
		if (pending_accept && cobsetptr->cob_exit_wait) {
			/* FIXME: we likely should position to the last line since last cleanup before:
			          DISPLAY AT 1010 DISPLAY AT 0909 GOBACK overrides first DISPLAY */
			if (cobsetptr->cob_exit_msg[0] != 0) {
				snprintf (exit_msg, COB_MINI_BUFF, "\n%s ", cobsetptr->cob_exit_msg);
				cob_display_text (exit_msg);
			} else {
				cob_display_text (" ");
			}
			flags = COB_SCREEN_NO_ECHO;
			if (COB_MOUSE_FLAGS & 1024) {
				/* disable mouse movement to exit the following ACCEPT OMITTED */
				COB_MOUSE_FLAGS &= ~1024;
				cob_settings_screenio ();
			}
			field_accept_from_curpos (NULL, NULL, NULL, NULL, NULL, NULL, NULL, flags);
		}
		cobglobptr->cob_screen_initialized = 0;
#if 0 /* CHECKME: Shouldn't be necessary */
		wclear (mywin);
		cob_move_to_beg_of_last_line ();
#endif
		endwin (); /* ends curses' terminal mode */
		delwin (mywin);	/* free storage related to screen not active */
#ifdef	HAVE_CURSES_FREEALL
		/* cleanup storage that would otherwise be shown
		   to be "still reachable" with valgrind */
		_nc_freeall ();
#endif
		if (cob_base_inp) {
			cob_free (cob_base_inp);
			cob_base_inp = NULL;
		}
	}
	COB_ACCEPT_STATUS = 0;
}

/* minimal exit from curses screen - without any cleanup */
void
cob_exit_screen_from_signal (int ss_only)
{
	/* note: we don't care about memory cleanup here, as this may
	   lead to a lock or otherwise issues when our memory is broken
	   (=explicit when coming from SIGSEGV / SIGBUS / SIGABRT fom libc) */

	if (!cobglobptr) {
		return;
	}

	/* warning: some implementations of curses are not safe to request
	   exiting from curses mode! (ncurses >6 seems fine,
	   PDCurses only with PDCursesMod 4.3.5) */
#if (!defined (NCURSES_VERSION_MAJOR) || NCURSES_VERSION_MAJOR < 6) \
 && (!defined (PDC_BUILD) || PDC_BUILD < 4305)
	if (ss_only) return;
#else
	COB_UNUSED (ss_only);
#endif

	if (cobglobptr->cob_screen_initialized) {
		endwin ();
	}
}

#else	/* WITH_EXTENDED_SCREENIO */

static int
cob_screen_init (void)
{
	return -1;
}

void
cob_exit_screen (void)
{
	/* nothing possible to do here */
}

void
cob_exit_screen_from_signal (int signal_safe_only)
{
	/* nothing possible to do here */
	COB_UNUSED (signal_safe_only);
}

/* DISPLAY scr-name */
void
cob_screen_display (cob_screen *s, cob_field *line, cob_field *column,
		   const int zero_line_col_allowed)
{
	COB_UNUSED (s);
	COB_UNUSED (line);
	COB_UNUSED (column);
	COB_UNUSED (zero_line_col_allowed);
	handle_status (9000, DISPLAY_STATEMENT);
}

/* ACCEPT scr-name */
void
cob_screen_accept (cob_screen *s, cob_field *line,
		   cob_field *column, cob_field *ftimeout,
		   const int zero_line_col_allowed)
{
	static int first_accept = 1;
	COB_UNUSED (s);
	COB_UNUSED (line);
	COB_UNUSED (column);
	COB_UNUSED (ftimeout);
	COB_UNUSED (zero_line_col_allowed);
	if (first_accept) {
		first_accept = 0;
		cob_runtime_warning (_("runtime is not configured to support %s"),
			"screenio ACCEPT");
	}
	handle_status (9000, ACCEPT_STATEMENT);
}

/* DISPLAY scr-item WITH/AT */
void
cob_display_field (cob_field *f, const cob_flags_t fattr, const char * parms, ...)
{
	COB_UNUSED (f);
	COB_UNUSED (fattr);
	COB_UNUSED (parms);
	handle_status (9000, DISPLAY_STATEMENT);
}

/* ACCEPT scr-item WITH/AT */
void
cob_accept_field (cob_field *f, const cob_flags_t fattr, const char *parms, ...)
{
	static int first_accept = 1;
	COB_UNUSED (f);
	COB_UNUSED (fattr);
	COB_UNUSED (parms);
	if (first_accept) {
		first_accept = 0;
		cob_runtime_warning (_("runtime is not configured to support %s"),
			"screenio ACCEPT");
	}
	handle_status (9000, ACCEPT_STATEMENT);
}

/* DISPLAY scr-item WITH/AT - compat-function for < 3.2 */
void
cob_field_display (cob_field *f, cob_field *line, cob_field *column,
		  cob_field *fgc, cob_field *bgc, cob_field *fscroll,
		  cob_field *size_is, const cob_flags_t fattr)
{
	COB_UNUSED (line);
	COB_UNUSED (column);
	COB_UNUSED (fgc);
	COB_UNUSED (bgc);
	COB_UNUSED (fscroll);
	COB_UNUSED (size_is);
	cob_display_field (f, fattr, "");
}

/* ACCEPT scr-item WITH/AT - compat-function for < 3.2 */
void
cob_field_accept (cob_field *f, cob_field *line, cob_field *column,
		  cob_field *fgc, cob_field *bgc, cob_field *fscroll,
		  cob_field *ftimeout, cob_field *prompt,
		  cob_field *size_is, const cob_flags_t fattr)
{
	COB_UNUSED (line);
	COB_UNUSED (column);
	COB_UNUSED (fgc);
	COB_UNUSED (bgc);
	COB_UNUSED (fscroll);
	COB_UNUSED (ftimeout);
	COB_UNUSED (prompt);
	COB_UNUSED (size_is);
	cob_accept_field (f, fattr, "");
}

/* internal function to temporarily set "extended screen mode"
   to either on=1 or off=0;
   note: does _not_ adjust cob_screen_initialized */
void
cob_screen_set_mode (const cob_u32_t smode)
{
	COB_UNUSED (smode);
}

/* x'E4' system call - clear screen */
int
cob_sys_clear_screen (void)
{
	return 0;	/* CHECKME: Should likely set an error code */
}

#endif	/* WITH_EXTENDED_SCREENIO */

void
cob_screen_line_col (cob_field *f, const int get_columns)
{
	init_cob_screen_if_needed ();
#ifdef	WITH_EXTENDED_SCREENIO
	if (get_columns) {
		cob_set_int (f, (int)COLS);
	} else {
		cob_set_int (f, (int)LINES);
	}
#else
	if (get_columns) {
		cob_set_int (f, 80);
	} else {
		cob_set_int (f, 24);
	}
#endif
}

int
cob_sys_sound_bell (void)
{
	if (COB_BEEP_VALUE == 9) {
		return 0;
	}
#ifdef	WITH_EXTENDED_SCREENIO
	if (!cobglobptr->cob_screen_initialized
	 && COB_BEEP_VALUE != 2) {
		int ret = cob_screen_init ();
		if (ret) {
			cob_speaker_beep ();
			return ret;
		}
	}
	cob_beep ();
#else
	cob_speaker_beep ();
#endif
	return 0;
}

void
cob_accept_escape_key (cob_field *f)
{
	int status = COB_ACCEPT_STATUS;
	/* Note: MF + ACU set this to a 9(2) item, we do a translation here
	   (only works for USAGE DISPLAY!); its value is the 2 digits of the termination keys
	   TESTME (working as planned, same values in MF?) */
	if (f->size == 2 && status) {
		unsigned char	crtstat[3];
		get_crt3_status (status, crtstat);
		f->data[0] = crtstat[0];
		f->data[1] = crtstat[1];
	} else {
		cob_set_int (f, status);
	}
}

/* get CurSoR position on screen */
int
cob_sys_get_csr_pos (unsigned char *fld)
{
#ifdef	WITH_EXTENDED_SCREENIO
	const cob_field *f = COB_MODULE_PTR->cob_procedure_params[0];
	int	cline;
	int	ccol;
#endif

	COB_CHK_PARMS (CBL_GET_CSR_POS, 1);
	init_cob_screen_if_needed ();

#ifdef	WITH_EXTENDED_SCREENIO
	getyx (mywin, cline, ccol);
	if (f && f->size == 4) {
		/* group with sizes up to 64k (2 * 2 bytes)
		   as used by Fujitsu (likely with a limit of
		   254 which does _not_ apply to GnuCOBOL) */
		const cob_u16_t bline = (cob_u16_t) cline;
		const cob_u16_t bcol = (cob_u16_t) ccol;
		memcpy (f->data, &bline, 2);
		memcpy (f->data + 2, &bcol, 2);
	} else {
		/* group with sizes up to 255 (2 * 1 bytes)
		   as used by MicroFocus [including C wrappers!]) */
		fld[0] = (unsigned char)cline;
		fld[1] = (unsigned char)ccol;
	}

#else
	fld[0] = 1U;
	fld[1] = 1U;
#endif
	return 0;
}

/* COBOL: get a char (or x'00' for function keys)
   call a second time when getting x'00' leads to the function keys
   1001-1199 as x'01' - x'C7', 2001 - 2055 as x'C9' - x'FF'
   No implementation of MF function tables so far.
*/
int
cob_sys_get_char (unsigned char *fld)
{
#ifdef	WITH_EXTENDED_SCREENIO
	int ret;
#endif

	COB_CHK_PARMS (CBL_READ_KBD_CHAR, 1);
	/* note: screen init done in called cob_get_char */

#ifdef	WITH_EXTENDED_SCREENIO
	if (!got_sys_char) {
		ret = cob_get_char ();
		if (ret > 255) {
			*fld = 0;
			got_sys_char = 1;
		} else {
			*fld = (unsigned char) ret;
		}
	} else {
		got_sys_char = 0;
		if (COB_ACCEPT_STATUS == 0) {
			return cob_sys_get_char (fld);
		} else if (COB_ACCEPT_STATUS > 1000 && COB_ACCEPT_STATUS < 1201) {
			*fld = (unsigned char) (COB_ACCEPT_STATUS - 1000);
		} else if (COB_ACCEPT_STATUS > 2000 && COB_ACCEPT_STATUS < 2056) {
			*fld = (unsigned char) (COB_ACCEPT_STATUS - 1800);
		} else {
			return -1;
		}
	}
	return 0;
#else
	/* this function is only valid for extended screenio! */
	COB_UNUSED (fld);
	return -2;
#endif
}

/* set CurSoR position on screen,
   expects a group of binary fields: line + col*/
int
cob_sys_set_csr_pos (unsigned char *fld)
{
#ifdef	WITH_EXTENDED_SCREENIO
	const cob_field *f = COB_MODULE_PTR->cob_procedure_params[0];
	int	cline;
	int	ccol;
#endif

	COB_CHK_PARMS (CBL_SET_CSR_POS, 1);
	init_cob_screen_if_needed ();

#ifdef	WITH_EXTENDED_SCREENIO
	if (f && f->size == 4) {
		/* group with sizes up to 64k (2 * 2 bytes)
		   as used by Fujitsu (likely with a limit of
		   254 which does _not_ apply to GnuCOBOL) */
		cob_u16_t bline, bcol;
		memcpy (&bline, f->data, 2);
		memcpy (&bcol, f->data + 2, 2);
		cline = bline;
		ccol = bcol;
	} else {
		/* group with sizes up to 255 (2 * 1 bytes)
		   as used by MicroFocus [including C wrappers!]) */
		cline = fld[0];
		ccol= fld[1];
	}
	return wmove (mywin ,cline, ccol);
#else
	COB_UNUSED (fld);
	return 0;
#endif
}

/* CBL_GET_SCR_SIZE - get current screen size */
int
cob_sys_get_scr_size (unsigned char *line, unsigned char *col)
{
	COB_CHK_PARMS (CBL_GET_SCR_SIZE, 2);
	init_cob_screen_if_needed ();

#ifdef	WITH_EXTENDED_SCREENIO
	/* TODO: when COBOL: set by C routines, to also work for > UCHARMAX values */
	*line = (unsigned char)LINES;
	*col = (unsigned char)COLS;
	return 0;
#else
	*line = 24U;
	*col = 80U;
	cob_set_exception (COB_EC_IMP_FEATURE_DISABLED);
	return -1;
#endif
}

/* CBL_GC_SET_SCR_SIZE - set current screen size */
int
cob_sys_set_scr_size (unsigned char *line, unsigned char *col)
{
	COB_CHK_PARMS (CBL_SET_SCR_SIZE, 2);
	init_cob_screen_if_needed ();

#if !defined (WITH_EXTENDED_SCREENIO) || !defined (HAVE_RESIZE_TERM)
	COB_UNUSED (line);
	COB_UNUSED (col);
	cob_set_exception (COB_EC_IMP_FEATURE_DISABLED);
	return -1;
#else
	{
		const int screen_row = (int)*line;
		const int screen_col = (int)*col;
		const int ret = resize_term (screen_row, screen_col);
		if (ret != OK) {
			return ret;
		}
	}
	return 0;
#endif
}

/* save the current mywin screen to a file */
int
cob_sys_scr_dump(unsigned char *parm)
{
#ifdef	WITH_EXTENDED_SCREENIO
	int	result;
	FILE	*filep;
	const char *filename = cob_get_param_str_buffered (1);

	COB_CHK_PARMS (CBL_GC_SCR_DUMP, 1);
	init_cob_screen_if_needed ();

	if (filename && (filep = fopen(filename, "wb")) != NULL)
	{
		refresh();
		result = putwin(mywin, filep);
		fclose(filep);
		return result;
	}

	return ERR;
#else
	COB_UNUSED (parm);
	return -1;
#endif

}


/* restore the current mywin screen from a file */
int cob_sys_scr_restore(unsigned char *parm)
{
#ifdef	WITH_EXTENDED_SCREENIO
	int	result;
	FILE	*filep;
	const char	*filename = cob_get_param_str_buffered (1);

	COB_CHK_PARMS (CBL_GC_SCR_RESTORE, 1);
	init_cob_screen_if_needed ();

	if (filename && (filep = fopen(filename, "rb")) != NULL)
	{
		WINDOW *replacement = getwin(filep);
		fclose(filep);

		if (replacement)
		{
			result = overwrite(replacement, mywin);
			refresh();
			delwin(replacement);
			return result;
		}
	}

	return ERR;
#else
	COB_UNUSED (parm);
	return -1;
#endif

}

int
cob_get_scr_cols (void)
{
	init_cob_screen_if_needed();
#ifdef	WITH_EXTENDED_SCREENIO
	return (int)COLS;
#else
	return 80;
#endif
}

int
cob_get_scr_lines (void)
{
	init_cob_screen_if_needed();
#ifdef	WITH_EXTENDED_SCREENIO
	return (int)LINES;
#else
	return 24;
#endif
}

/* check and handle adjustments to settings concerning screenio */
void
cob_settings_screenio (void)
{
#ifdef	WITH_EXTENDED_SCREENIO
	if (!cobglobptr || !cobglobptr->cob_screen_initialized) {
		return;
	}

	/* requesting cursor type depending on both settings, 0 = hide,
	   1 = vertical bar cursor, 2 = square cursor; note: the cursor change
	   may not have an effect in all curses implementations / terminals */
	{
		static unsigned int	prev_cursor_mode = UINT_MAX; 
		const unsigned int new_cursor_mode
			= (COB_HIDE_CURSOR)?0U:((COB_INSERT_MODE)?1U:2U);
		if (prev_cursor_mode != new_cursor_mode) {
			(void) curs_set (new_cursor_mode);
			prev_cursor_mode = new_cursor_mode;
		}
	}

	/* extended ACCEPT status returns */
	if (cobsetptr->cob_extended_status == 0) {
		cobsetptr->cob_use_esc = 0;
	}

#ifdef HAVE_MOUSEINTERVAL
	mouseinterval (COB_MOUSE_INTERVAL);
#endif
#ifdef HAVE_MOUSEMASK
	if (curr_setting_mouse_flags != COB_MOUSE_FLAGS) {
		mmask_t 	mask_applied = cob_mask_routine;
		if (COB_MOUSE_FLAGS) {
			/* COB_MOUSE_FLAGS & 1 --> auto-handling active
			   note: currently missing in the accept handling:
			   click+drag within a field to mark it (should be
			   done in general when the SHIFT key + cursor is
			   used) [shown by reverse-video those positions]
			   and by delete remove the marked characters,
			   by typing removing them before adding the new ones
			   remove marker when positioning key is used or
			   mouse click into any field occurs */
			if (COB_MOUSE_FLAGS & 1) {
				mask_applied |= BUTTON1_PRESSED
					/* note: not done by ACUCOBOL (ENTER translation): */
					| BUTTON1_DOUBLE_CLICKED
					;
			}
			if (COB_MOUSE_FLAGS & 2) {
				cob_mask_accept |= BUTTON1_PRESSED;
			}
			if (COB_MOUSE_FLAGS & 4) {
				cob_mask_accept |= BUTTON1_RELEASED;
			}
			if (COB_MOUSE_FLAGS & 8) {
				cob_mask_accept |= BUTTON1_DOUBLE_CLICKED;
			}
			if (COB_MOUSE_FLAGS & 16) {
				cob_mask_accept |= BUTTON2_PRESSED;
			}
			if (COB_MOUSE_FLAGS & 32) {
				cob_mask_accept |= BUTTON2_RELEASED;
			}
			if (COB_MOUSE_FLAGS & 64) {
				cob_mask_accept |= BUTTON2_DOUBLE_CLICKED;
			}
			if (COB_MOUSE_FLAGS & 128) {
				cob_mask_accept |= BUTTON3_PRESSED;
			}
			if (COB_MOUSE_FLAGS & 256) {
				cob_mask_accept |= BUTTON3_RELEASED;
			}
			if (COB_MOUSE_FLAGS & 512) {
				cob_mask_accept |= BUTTON3_DOUBLE_CLICKED;
			}
			if (COB_MOUSE_FLAGS & 1024) {
				cob_mask_accept |= REPORT_MOUSE_POSITION;
			}
			/* 2048 cursor shape, seems irrelevant
			   16384 all windows,
			   only relevant when adding multiple windows */
			mask_applied |= cob_mask_accept;
		}
		mousemask (mask_applied, NULL);
		curr_setting_mouse_flags = COB_MOUSE_FLAGS;
	}
#endif
#endif
}

void
cob_init_screenio (cob_global *lptr, cob_settings *sptr)
{
	cobglobptr = lptr;
	cobsetptr  = sptr;
	if (!cobsetptr->cob_exit_msg || !cobsetptr->cob_exit_msg[0]) {
		cobsetptr->cob_exit_msg =
			cob_strdup (_("end of program, please press a key to exit"));
	}

	cob_settings_screenio ();
}

/* Check the parms used for the panel functions */
int
check_panel_parm (int parm_type, PPARM_FLD parm)
{
	if (mywin == stdscr) {
		getyx (stdscr, save_cursor_y, save_cursor_x);
		stdscr_fore_color = fore_color;
		stdscr_back_color = back_color;
	}
	
	parm->ret_code = 0;
	switch (parm_type) {
		case CBL_NEW_WINDOW:
			if (parm->starty > COB_MAX_Y_COORD)
				parm->ret_code = 2;
			if (parm->startx > COB_MAX_X_COORD)
				parm->ret_code = 4;
			if (parm->win_rows > COB_MAX_Y_COORD)
				parm->ret_code = 6;
			if (parm->win_cols > COB_MAX_X_COORD)
				parm->ret_code = 7;
			if (parm->bg_color < 0 || parm->bg_color > 7)
				parm->ret_code = 8;
			if (parm->fg_color < 0 || parm->fg_color > 7)
				parm->ret_code = 9;
			if (parm->fg_color == parm->bg_color)
				parm->ret_code = 11;
			break;
		case CBL_DELETE_WINDOW:
			if (parm->pan_number < 1 || parm->pan_number > MAX_PANELS)
				parm->ret_code = 10;
			break;
		case CBL_MOVE_WINDOW:
			if (parm->starty > COB_MAX_Y_COORD)
				parm->ret_code = 2;
			if (parm->startx > COB_MAX_X_COORD)
				parm->ret_code = 4;
			if (parm->pan_number < 1 || parm->pan_number > MAX_PANELS)
				parm->ret_code = 10;
			break;
		case CBL_HIDE_WINDOW:
			if (parm->pan_number < 1 || parm->pan_number > MAX_PANELS)
				parm->ret_code = 10;
			if (parm->hidden != 1 )
				parm->ret_code = 12;
			break;
		case CBL_SHOW_WINDOW:
			if (parm->pan_number < 1 || parm->pan_number > MAX_PANELS)
				parm->ret_code = 10;
			if (parm->hidden != 0 )
				parm->ret_code = 14;
			break;
		case CBL_TOP_WINDOW:
			if (parm->pan_number < 1 || parm->pan_number > MAX_PANELS)
				parm->ret_code = 16;
			break;
		case CBL_BOTTOM_WINDOW:
			if (parm->pan_number < 1 || parm->pan_number > MAX_PANELS)
				parm->ret_code = 18;
			break;
	}

	return parm->ret_code;
}

/*	this function updates all the windows which are not hidden 
	and then does the final "doupdate" to update the physical screen */
void
cob_update_all_windows (void)
{
	PANEL			*ptr_pan;
	WINDOW			*ptr_win;

	/* first get the top panel */

	ptr_pan =ceiling_panel(NULL);

	if (!ptr_pan) {
		mywin = stdscr;
		refresh_mywin (mywin);
		doupdate ();
		return;
	}

	for(;ptr_pan;) {
		ptr_win = panel_window (ptr_pan);
		wnoutrefresh(ptr_win);
		ptr_pan = panel_below(ptr_pan);
	}


	update_panels();
	refresh_mywin (mywin);
	doupdate();
}

/* CBL_GC_NEW_WINDOW - create a new panel / window pair */
int
cob_sys_new_window (void *a)
{

#ifndef WITH_PANELS
	return -1;
#endif

	PPARM_FLD		parm;
	PPANEL_ENTRY	pan_first = &my_panels[0];
	PPANEL_ENTRY	pan_last = &my_panels[MAX_PANELS];
	PPANEL_ENTRY	ptr_pan_entry;
	PANEL			*ptr_pan;
	WINDOW			*ptr_win;
	int				ret;
	short			fg_color;
	short			bg_color;
	short			color_pr;

	COB_CHK_PARMS (CBL_GC_NEW_WINDOW, 1);
	init_cob_screen_if_needed ();
	parm = a;
	ret = check_panel_parm(CBL_NEW_WINDOW, parm);
	switch (ret) {
		case 99:
			return	 -4;
			break;
		case 0:
			break;
		default:
			parm->ret_code = ret;
			return -1;
			break;
	}

	/* find empty slot in panel array */

	for(ptr_pan_entry = pan_first; ptr_pan_entry <= pan_last; ptr_pan_entry++) {
		if (ptr_pan_entry->pan == NULL)
			break;
	}

	/* if no empty slot exists then set error code */

	if (ptr_pan_entry > pan_last) {
		parm->ret_code = 20;
		return -1;
	}

	ptr_win = newwin(parm->win_rows,
					parm->win_cols,
					parm->starty - 1,
					parm->startx - 1);
	if (!ptr_win) {
		parm->ret_code = 22;
		return -1;
	}

	ptr_pan = new_panel(ptr_win);
	if (!ptr_pan) {
		parm->ret_code = 24;
		return -1;
	}

	cob_to_curses_color (parm->fg_color, &fg_color);
	cob_to_curses_color (parm->bg_color, &bg_color);

	parm->pan_number = ptr_pan_entry->pan_number;
	ptr_pan_entry->pan = ptr_pan;
	ptr_pan_entry->win = ptr_win;
	ptr_pan_entry->win_rows = parm->win_rows;
	ptr_pan_entry->win_cols = parm->win_cols;
	ptr_pan_entry->startx = parm->startx - 1;
	ptr_pan_entry->starty = parm->starty - 1;
	ptr_pan_entry->hidden = 0;
	ptr_pan_entry->fg_color = fg_color;
	ptr_pan_entry->bg_color = bg_color;

	if (set_panel_userptr (ptr_pan, ptr_pan_entry)) {
		parm->ret_code = 26;
		return -1;
	}

	mywin = ptr_win;
	color_pr = cob_get_color_pair(fg_color, bg_color);
	wbkgdset (mywin ,COLOR_PAIR(color_pr));
	werase(mywin);
	fore_color = ptr_pan_entry->fg_color;
	back_color = ptr_pan_entry->bg_color;
	cob_update_all_windows ();
	return 0;
}

/* CBL_GC_MOVE_WINDOW - move the panel within stdscr */
int
cob_sys_move_window (void *a)
{

#ifndef WITH_PANELS
	return -1;
#endif



	PPARM_FLD		parm;
	PPANEL_ENTRY	ptr_pan_entry;
	PANEL			*ptr_pan;
	int			ret;

	COB_CHK_PARMS (CBL_GC_NEW_WINDOW, 1);
	init_cob_screen_if_needed ();
	parm = a;
	ret = check_panel_parm(CBL_MOVE_WINDOW, parm);
	switch (ret) {
		case 99:
			return -4;
			break;
		case 0:
			break;
		default:
			parm->ret_code = ret;
			return -1;
			break;
	}

	ptr_pan_entry = &my_panels[parm->pan_number - 1];
	if (!ptr_pan_entry->pan) {
		parm->ret_code = 28;
		return -1;
	}

	ptr_pan = ptr_pan_entry->pan;

	if (!panel_hidden(ptr_pan_entry->pan)) {
		parm->ret_code = 40;
		return -1;
	}

	if (move_panel (ptr_pan,
			parm->starty - 1,
			parm->startx - 1) ) {
		parm->ret_code = 29;
		return -1;
	}

	ptr_pan_entry->startx = parm->startx - 1;
	ptr_pan_entry->starty = parm->starty - 1;

	ptr_pan = ceiling_panel(NULL);
	mywin = panel_window (ptr_pan);
	ptr_pan_entry = (PPANEL_ENTRY)panel_userptr(ptr_pan);
	fore_color = ptr_pan_entry->fg_color;
	back_color = ptr_pan_entry->bg_color;

	cob_update_all_windows ();
	return 0;
}

/* CBL_GC_SHOW_WINDOW - show the panel within stdscr */

int
cob_sys_show_window (void *a)
{

#ifndef WITH_PANELS
	return -1;
#endif

	PPARM_FLD		parm;
	PPANEL_ENTRY	ptr_pan_entry;
	PANEL			*ptr_pan;
	int				ret;

	COB_CHK_PARMS (CBL_GC_NEW_WINDOW, 1);
	init_cob_screen_if_needed ();
	parm = a;
	ret = check_panel_parm(CBL_SHOW_WINDOW, parm);
	switch (ret) {
		case 99:
			return -4;
			break;
		case 0:
			break;
		default:
			parm->ret_code = ret;
			return -1;
			break;
	}

	ptr_pan_entry = &my_panels[parm->pan_number - 1];
	if (!ptr_pan_entry->pan) {
		parm->ret_code = 28;
		return -1;
	}

	if (panel_hidden(ptr_pan_entry->pan)) {
		parm->ret_code = 42;
		return -1;
	}

	ptr_pan = ptr_pan_entry->pan;

	if (show_panel(ptr_pan)) {
		parm->ret_code = 30;
		return -1;
	}

	ptr_pan_entry->hidden = 0;

	ptr_pan = ceiling_panel(NULL);
	mywin = panel_window (ptr_pan);
	ptr_pan_entry = (PPANEL_ENTRY)panel_userptr(ptr_pan);
	fore_color = ptr_pan_entry->fg_color;
	back_color = ptr_pan_entry->bg_color;

	cob_update_all_windows ();
	return 0;

}

/* CBL_GC_HIDE_WINDOW - hide the panel within stdscr */
int
cob_sys_hide_window (void *a)
{

#ifndef WITH_PANELS
	return -1;
#endif

	PPARM_FLD		parm;
	PPANEL_ENTRY	ptr_pan_entry;
	PANEL			*ptr_pan;
	int				ret;

	COB_CHK_PARMS (CBL_GC_NEW_WINDOW, 1);
	init_cob_screen_if_needed ();
	parm = a;
	ret = check_panel_parm(CBL_HIDE_WINDOW, parm);
	switch (ret) {
		case 99:
			return -4;
			break;
		case 0:
			break;
		default:
			parm->ret_code = ret;
			return -1;
			break;
	}

	ptr_pan_entry = &my_panels[parm->pan_number - 1];
	if (!ptr_pan_entry->pan) {
		parm->ret_code = 28;
		return -1;
	}

	ptr_pan = ptr_pan_entry->pan;

	if (!panel_hidden(ptr_pan_entry->pan)) {
		parm->ret_code = 40;
		return -1;
	}

	if (hide_panel(ptr_pan)) {
		parm->ret_code = 32;
		return -1;
	}

	/*	note the following is needed in case the top panel
		was hidden. then the next panel will be on top */

	ptr_pan = ceiling_panel(NULL);

	if (ptr_pan) {
		mywin = panel_window (ptr_pan);
		ptr_pan_entry = (PPANEL_ENTRY)panel_userptr(ptr_pan);
		fore_color = ptr_pan_entry->fg_color;
		back_color = ptr_pan_entry->bg_color;
	} else {
		mywin = stdscr;
		move(save_cursor_y, save_cursor_x);
		fore_color = stdscr_fore_color;
		back_color = stdscr_back_color;
	}

	cob_update_all_windows ();
	return 0;

}


/* CBL_GC_TOP_WINDOW - top the panel within stdscr */
int
cob_sys_top_window (void *a)
{

#ifndef WITH_PANELS
	return -1;
#endif

	PPARM_FLD		parm;
	PPANEL_ENTRY	ptr_pan_entry;
	PANEL			*ptr_pan;
	int				ret;

	COB_CHK_PARMS (CBL_GC_NEW_WINDOW, 1);
	init_cob_screen_if_needed ();
	parm = a;
	ret = check_panel_parm(CBL_BOTTOM_WINDOW, parm);
	switch (ret) {
		case 99:
			return -4;
			break;
		case 0:
			break;
		default:
			parm->ret_code = ret;
			return -1;
			break;
	}

	ptr_pan_entry = &my_panels[parm->pan_number - 1];
	if (!ptr_pan_entry->pan) {
		parm->ret_code = 28;
		return -1;
	}

	ptr_pan = ptr_pan_entry->pan;

	if (!panel_hidden(ptr_pan_entry->pan)) {
		parm->ret_code = 40;
		return -1;
	}

	if (top_panel(ptr_pan)) {
		parm->ret_code = 34;
		return -1;
	}

	ptr_pan = ceiling_panel(NULL);
	mywin = panel_window (ptr_pan);
	ptr_pan_entry = (PPANEL_ENTRY)panel_userptr(ptr_pan);
	fore_color = ptr_pan_entry->fg_color;
	back_color = ptr_pan_entry->bg_color;

	cob_update_all_windows ();
	return 0;
}


/* CBL_GC_BOTTOM_WINDOW - top the panel within stdscr */
int
cob_sys_bottom_window (void *a)
{

#ifndef WITH_PANELS
	return -1;
#endif

	PPARM_FLD		parm;
	PPANEL_ENTRY	ptr_pan_entry;
	PANEL			*ptr_pan;
	int				ret;

	COB_CHK_PARMS (CBL_GC_NEW_WINDOW, 1);
	init_cob_screen_if_needed ();
	parm = a;
	ret = check_panel_parm(CBL_BOTTOM_WINDOW, parm);
	switch (ret) {
		case 99:
			return -4;
			break;
		case 0:
			break;
		default:
			parm->ret_code = ret;
			return -1;
			break;
	}

	ptr_pan_entry = &my_panels[parm->pan_number - 1];
	if (!ptr_pan_entry->pan) {
		parm->ret_code = 28;
		return -1;
	}

	ptr_pan = ptr_pan_entry->pan;

	if (!panel_hidden(ptr_pan_entry->pan)) {
		parm->ret_code = 40;
		return -1;
	}

	if (bottom_panel(ptr_pan)) {
		parm->ret_code = 34;
		return -1;
	}

	ptr_pan = (ceiling_panel(NULL));
	ptr_pan_entry = (PPANEL_ENTRY)(panel_userptr(ptr_pan));
	parm->pan_number = ptr_pan_entry->pan_number;

	ptr_pan = ceiling_panel(NULL);
	mywin = panel_window (ptr_pan);
	ptr_pan_entry = (PPANEL_ENTRY)panel_userptr(ptr_pan);
	fore_color = ptr_pan_entry->fg_color;
	back_color = ptr_pan_entry->bg_color;

	cob_update_all_windows ();
	return 0;
}

/* CBL_GC_DELETE_WINDOW - top the panel within stdscr */
int
cob_sys_delete_window (void *a)
{

#ifndef WITH_PANELS
	return -1;
#endif

	PPARM_FLD		parm;
	PPANEL_ENTRY	ptr_pan_entry;
	PANEL			*ptr_pan;
	int				ret;

	COB_CHK_PARMS (CBL_GC_NEW_WINDOW, 1);
	init_cob_screen_if_needed ();
	parm = a;
	ret = check_panel_parm(CBL_BOTTOM_WINDOW, parm);
	switch (ret) {
		case 99:
			return -4;
			break;
		case 0:
			parm->ret_code = 0;
			break;
		default:
			parm->ret_code = ret;
			return -1;
			break;
	}

	ptr_pan_entry = &my_panels[parm->pan_number - 1];
	if (!ptr_pan_entry->pan) {
		parm->ret_code = 28;
		return -1;
	}

	if (!panel_hidden(ptr_pan_entry->pan)) {
		show_panel( ptr_pan_entry->pan);
	}

	if (del_panel(ptr_pan_entry->pan)) {
		parm->ret_code = 38;
	}

	if (delwin(ptr_pan_entry->win)) {
		parm->ret_code = 36;
	}

	if (parm->ret_code) {
		return -1;
	}

	ptr_pan_entry->pan = NULL;
	ptr_pan_entry->win = NULL;
	ptr_pan_entry->win_rows = 0;
	ptr_pan_entry->win_cols = 0;
	ptr_pan_entry->startx = 0;
	ptr_pan_entry->starty = 0;
	ptr_pan_entry->hidden = 0;
	ptr_pan_entry->fg_color = 0;
	ptr_pan_entry->bg_color = 7;

	ptr_pan = ceiling_panel(NULL);

	if (ptr_pan) {
		mywin = panel_window (ptr_pan);
		ptr_pan_entry = (PPANEL_ENTRY)panel_userptr(ptr_pan);
		fore_color = ptr_pan_entry->fg_color;
		back_color = ptr_pan_entry->bg_color;
	} else {
		mywin = stdscr;
		move(save_cursor_y, save_cursor_x);
		fore_color = stdscr_fore_color;
		back_color = stdscr_back_color;
	}

	cob_update_all_windows ();
	return 0;
}

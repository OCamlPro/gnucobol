/*
   Copyright (C) 2001-2012, 2014-2020 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Simon Sobisch, Edward Hart, Christian Lademann

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

#include "sysdefines.h"

#if defined (HAVE_NCURSESW_NCURSES_H)
#include <ncursesw/ncurses.h>
#elif defined (HAVE_NCURSESW_CURSES_H)
#include <ncursesw/curses.h>
#elif defined (HAVE_NCURSES_H)
#include <ncurses.h>
#elif defined (HAVE_NCURSES_NCURSES_H)
#include <ncurses/ncurses.h>
#elif defined (HAVE_PDCURSES_H)
/* will internally define NCURSES_MOUSE_VERSION with
   a recent version (for older version define manually): */
#define PDC_NCMOUSE		/* use ncurses compatible mouse API */
#include <pdcurses.h>
#elif defined (HAVE_CURSES_H)
#define PDC_NCMOUSE	/* see comment above */
#include <curses.h>
#ifndef PDC_MOUSE_MOVED
#undef PDC_NCMOUSE
#endif
#endif

/* Force symbol exports */
#define	COB_LIB_EXPIMP
#include "libcob.h"
#include "coblocal.h"

#ifdef	HAVE_CURSES_FREEALL
extern void	_nc_freeall (void);
#endif
#ifdef NCURSES_MOUSE_VERSION
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
static short			fore_color	/* "const" default foreground (pair 0 on init) */;
static short			back_color	/* "const" default background (pair 0 on init) */;;
static int			origin_y;
static int			origin_x;
static int			display_cursor_y;
static int			display_cursor_x;
static int			accept_cursor_y;
static int			accept_cursor_x;
static int			pending_accept;
static int			got_sys_char;
static unsigned int	curr_setting_insert_mode = INT_MAX;
static int 			screen_lines;
static int 			screen_columns;
#ifdef NCURSES_MOUSE_VERSION
static int	curr_setting_mouse_flags = INT_MAX;
#endif
#endif

/* Local function prototypes when screenio activated */

#ifdef	WITH_EXTENDED_SCREENIO
static void cob_screen_init	(void);
#endif

#ifdef	WITH_EXTENDED_ACCDIS
#include "xad.h"

xad_attrs_t	XAD_ATTRIBUTES;

xad_attrs_t	XAD_CBLATTRS[8] = {
	COB_XAD_ATTR_HIGHLIGHT,		/* Bit 0 */
	COB_XAD_ATTR_UNDERLINE,		/* Bit 1 */
	COB_XAD_ATTR_REVERSE,		/* Bit 2 */
	COB_XAD_ATTR_BLINK,		/* Bit 3 */
	COB_XAD_ATTR_LOWLIGHT,		/* Bit 4 */
	COB_XAD_ATTR_ALTCHARSET,	/* Bit 5 */
	COB_XAD_ATTR_NODISPLAY,		/* Bit 6 */
	0				/* Bit 7 */
};

static void		cob_xad_init (void);
static xad_attrs_t	curses_to_xad_attrs (chtype);
static int		xad_to_cob_attrs (xad_attrs_t);
static unsigned char	xad_to_cbl_attrs (xad_attrs_t);
static xad_attrs_t	cbl_to_xad_attrs (unsigned char);
static unsigned char	curses_to_cbl_attrs (chtype);
static int		xad_getfgcol (xad_attrs_t *);
static int		xad_getbgcol (xad_attrs_t *);
static int		xad_setfgcol (int, xad_attrs_t *);
static int		xad_setbgcol (int, xad_attrs_t *);

static cob_field	*xad_init_int_field (cob_field *, int);
static cob_field	*xad_build_int_field (int);
static cob_field	*xad_build_shared_field (cob_field *, cob_field *, int);

static char		*field_to_str (cob_field *, char *, size_t);

cob_field 		*cob_field_alloc (int);
cob_field		*cob_field_clone (cob_field *);
void			cob_field_free (cob_field *);

#endif

/* Local functions */

static void
cob_speaker_beep (void)
{
	int	fd;

	fd = fileno (stdout);
	if (fd >= 0) {
		(void)write (fd, "\a", (size_t)1);
	}
}

static COB_INLINE COB_A_INLINE void
init_cob_screen_if_needed (void)
{
	if (!cobglobptr) {
		cob_fatal_error (COB_FERROR_INITIALIZED);
	}
#ifdef	WITH_EXTENDED_SCREENIO
	if (!cobglobptr->cob_screen_initialized) {
		cob_screen_init ();
	}
#endif
}

#ifdef	WITH_EXTENDED_SCREENIO

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

	getmaxyx (stdscr, max_y, max_x);
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
	int status = move (line, column);

	if (status == ERR) {
		raise_ec_on_invalid_line_or_col (line, column);
	}
	return status;
}

void
cob_set_cursor_pos (int line, int column)
{
	init_cob_screen_if_needed ();
	(void) move (line, column);
}

static void
cob_move_to_beg_of_last_line (void)
{
	int	max_y;
	int	max_x;

	getmaxyx (stdscr, max_y, max_x);
	/* We don't need to check for exceptions here; it will always be fine */
	move (max_y, 0);

	COB_UNUSED (max_x);
}

static short
cob_to_curses_color (cob_field *f, const short default_color)
{
	if (!f) {
		return default_color;
	}
	switch (cob_get_int (f)) {
	case COB_SCREEN_BLACK:
		return COLOR_BLACK;
	case COB_SCREEN_BLUE:
		return COLOR_BLUE;
	case COB_SCREEN_GREEN:
		return COLOR_GREEN;
	case COB_SCREEN_CYAN:
		return COLOR_CYAN;
	case COB_SCREEN_RED:
		return COLOR_RED;
	case COB_SCREEN_MAGENTA:
		return COLOR_MAGENTA;
	case COB_SCREEN_YELLOW:
		return COLOR_YELLOW;
	case COB_SCREEN_WHITE:
		return COLOR_WHITE;
	default:
		/* default also applies to -1 */
		return default_color;
	}
}

static short
cob_get_color_pair (const short fg_color, const short bg_color)
{
	/* default color (defined from terminal, read during init ) */
	if (fg_color == fore_color && bg_color == back_color) {
		return 0;
	}
	/* reserved color "all black", defined during init */
	if (fg_color == 0 && bg_color == 0) {
		return 1;
	}
	
	{
		short	color_pair_number;
		short	fg_defined, bg_defined;

		for (color_pair_number = 2; color_pair_number < COLOR_PAIRS; color_pair_number++) {

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
	ret = color_set (color_pair_number, NULL);
#else
	ret = attrset (COLOR_PAIR(color_pair_number));
#endif
	bkgdset (COLOR_PAIR(color_pair_number));

	return ret;
}

enum screen_statement {
	ACCEPT_STATEMENT,
	DISPLAY_STATEMENT
};

static void
cob_screen_attr (cob_field *fgc, cob_field *bgc, const cob_flags_t attr,
		 const enum screen_statement stmt)
{
	int		line;
	int		column;
	chtype		styles = A_NORMAL;

	attrset (A_NORMAL);
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
	if (attr & COB_SCREEN_ALTCHARSET) {
		styles |= A_ALTCHARSET;
	}
#if 0
	if (styles) {
		attron (styles);
	}
#endif
	if (cob_has_color) {
		short		fg_color;
		short		bg_color;
		short		color_pair_number;
		short		default_fore_color = fore_color;
		short		default_back_color = back_color;

#ifdef WITH_EXTENDED_ACCDIS	// FIXME: Make "compiletime-transparent"
		// if (attr & COB_SCREEN_USE_XAD_COLORS) { // CHECKME
			default_fore_color = xad_getfgcol (NULL);
			default_back_color = xad_getbgcol (NULL);
		// }
#endif

		fg_color = cob_to_curses_color (fgc, default_fore_color);
		bg_color = cob_to_curses_color (bgc, default_back_color);
		color_pair_number = cob_get_color_pair (fg_color, bg_color);
		cob_activate_color_pair (color_pair_number);
// fprintf (stderr, "fg_c:%d/%d,bg_c:%d/%d,cp_n:%d, ", fg_color, default_fore_color, bg_color, default_back_color, color_pair_number);
	}

	if (styles) {
		attron (styles);
	}

	/* BLANK SCREEN colors the whole screen. */
	if (attr & COB_SCREEN_BLANK_SCREEN) {
		getyx (stdscr, line, column);
		clear ();
		cob_move_cursor (line, column);
	}

// fprintf (stderr, "\n");
	if (stmt == DISPLAY_STATEMENT) {
		/* BLANK LINE colors the whole line. */
		if (attr & COB_SCREEN_BLANK_LINE) {
			getyx (stdscr, line, column);
			cob_move_cursor (line, 0);
			clrtoeol ();
			cob_move_cursor (line, column);
		}
		if (attr & COB_SCREEN_ERASE_EOL) {
			clrtoeol ();
		}
		if (attr & COB_SCREEN_ERASE_EOS) {
			clrtobot ();
		}
	}
	if (attr & COB_SCREEN_BELL) {
		cob_beep ();
	}
}

static void
cob_screen_init (void)
{
	if (cobglobptr->cob_screen_initialized) {
		return;
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

	/* Configurable screen geometry. CHECKME: Maybe runtime.cfg? */
	screen_lines = 24;
	screen_columns = 80;

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
		/* FIXME: likely should raise an exception instead */
		cob_stop_run (1);
	}
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
	keypad (stdscr, 1);
	nonl ();
	noecho ();
	if (has_colors ()) {
		start_color ();
		pair_content ((short)0, &fore_color, &back_color);
		/* fix bad settings of the terminal on start */
		if (fore_color == back_color) {
			if (fore_color == COLOR_BLACK) {
				fore_color = COLOR_WHITE;
			} else {
				back_color = COLOR_BLACK;
			}
			init_pair ((short)0, fore_color, back_color);
		}
		if (COLOR_PAIRS) {
			cob_has_color = 1;
			/* explicit reserve pair 1 as all zero as we take this as "initialized" later on */
			init_pair ((short)1, 0, 0);
#ifdef	HAVE_LIBPDCURSES
			/* pdcurses sets *ALL* pairs to default fg/bg, while ncurses initialize the to zero
			   set all to zero here, allowing us to adjust them later */
			{
				short	color_pair_number;
	
				for (color_pair_number = 2; color_pair_number < COLOR_PAIRS; ++color_pair_number) {
					init_pair (color_pair_number, 0, 0);
					if (color_pair_number == SHRT_MAX) {
						break;
					}
				}
			}
#endif
		}
	}
	attrset (A_NORMAL);
	getmaxyx (stdscr, COB_MAX_Y_COORD, COB_MAX_X_COORD);

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
	/* note: if define_key is not available the user will have to manually
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

#ifdef WITH_EXTENDED_ACCDIS
	(void) cob_xad_init ();
#endif
}

static void
get_screen_geometry (int *lines, int *columns) {
#ifdef WITH_EXTENDED_SCREENIO
	if (screen_lines <= 0) {
		*lines = (int)LINES;
	} else {
		*lines = screen_lines;
	}

	if (screen_columns <= 0) {
		*columns = (int)COLS;
	} else {
		*columns = screen_columns;
	}

	/* Paranoia protection ;-) */
	if (*lines <= 0) {
		*lines = 24;
	}

	if (*columns <= 0) {
		*columns = 80;
	}
#else
	*lines = 24;
	*columns = 80;
#endif
}


static void
set_screen_geometry (int lines, int columns) {
#ifdef WITH_EXTENDED_SCREENIO
	screen_lines = lines;
	screen_columns = columns;
#else
	return;
#endif
}


static int
get_screen_lines () {
	int	lines, columns;

	get_screen_geometry (&lines, &columns);

	return (lines);
}


static int
get_screen_columns () {
	int	lines, columns;

	get_screen_geometry (&lines, &columns);

	return (columns);
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


static int
cob_convert_key_to_function (const int keyp) {
	int	keyf = -1;

	switch (keyp) {
	case ERR:		return COB_SCR_TIME_OUT;
	case '\033':		return COB_SCR_ESC;
	// case '\r':
	// case '\n':
	case KEY_ENTER:		return COB_SCR_OK;
	case KEY_PPAGE:		return COB_SCR_PAGE_UP;
	case KEY_NPAGE:		return COB_SCR_PAGE_DOWN;
	case KEY_PRINT:		return COB_SCR_PRINT;
	case KEY_STAB:		return COB_SCR_TAB;
	case KEY_BTAB:		return COB_SCR_BACK_TAB;
	case KEY_UP:		return COB_SCR_KEY_UP;
	case KEY_DOWN:		return COB_SCR_KEY_DOWN;
	case KEY_HOME:		return COB_SCR_KEY_HOME;
	case KEY_END:		return COB_SCR_KEY_END;
	case KEY_BACKSPACE:	return COB_SCR_BACKSPACE;
	case KEY_LEFT:		return COB_SCR_KEY_LEFT;
	case KEY_RIGHT:		return COB_SCR_KEY_RIGHT;
	case KEY_IC:		return COB_SCR_INSERT;
	case KEY_DC:		return COB_SCR_DELETE;

#if 0 // TODO
#ifdef NCURSES_MOUSE_VERSION
	case KEY_MOUSE:
	{
		int mline = mevent.y;
		int mcolumn = mevent.x;
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
				cob_screen_attr (s->foreg, s->backg, s->attr, ACCEPT_STATEMENT);
				cob_move_cursor (mline, mcolumn);
				continue;
			}
		}
		mevent.bstate &= cob_mask_accept;
		if (mevent.bstate != 0) {
			global_return = mouse_to_exception_code (mevent.bstate);
			cob_move_cursor (mline, mcolumn);	// move cursor to pass position
			goto screen_return;
		}
		continue;
	}
#endif
#endif
	default:
		if (keyp >= KEY_F(1) && keyp <= KEY_F(64)) {
			return COB_SCR_F0 + keyp - KEY_F(0);
		}

		/* Handle printable character. */
#if 0 /* FIXME: we can't handle anything > UCHAR_MAX here because of
		*p = (unsigned char) keyp;
		--> revise */
		if (keyp > 037 && keyp < (int)A_CHARTEXT) {
#else
		if (keyp > 037 && keyp <= UCHAR_MAX) {
#endif
			return keyp;
		}
	}

	return ERR;
}


static void
cob_set_crt3_status (cob_field *status_field, int fret)
{
	unsigned char	crtstat[3];

	crtstat[0] = '0';
	crtstat[1] = '\0';
	crtstat[2] = '\0';

	switch (fret) {
	case COB_SCR_OK:
		crtstat[0] = '0';
		crtstat[1] = '0';
		break;

	case COB_SCR_ESC:
		crtstat[0] = '1';
		crtstat[1] = '\0';
		break;

	case COB_SCR_NO_FIELD:
	case COB_SCR_MAX_FIELD:
		crtstat[0] = '9';
		crtstat[1] = '\0';
		break;

	case COB_SCR_TIME_OUT:	/* CHECKME */
		crtstat[0] = '9';
		crtstat[1] = '\1';
		break;

	/* TODO: more case COB_SCR_... */

	default:
		if (fret >= COB_SCR_FUNCTION_KEYS_MIN && fret <= COB_SCR_FUNCTION_KEYS_MAX) {
			/* function keys */
			crtstat[0] = '1';
			crtstat[1] = (unsigned char)(fret - COB_SCR_FUNCTION_KEYS_MIN + 1);
		} else if (fret >= COB_SCR_EXCEPTION_KEYS_MIN && fret <= COB_SCR_EXCEPTION_KEYS_MAX) {
			/* exception keys */
			crtstat[0] = '2';
			crtstat[1] = (unsigned char)(fret - COB_SCR_EXCEPTION_KEYS_MIN + 1);
		}
	}

	memcpy (status_field->data, crtstat, 3);
}


static void
handle_status (const int fret)
{
	if (fret) {
		cob_set_exception (COB_EC_IMP_ACCEPT);
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

/* update field for the programs SPECIAL-NAMES CURSOR clause */
static void
pass_cursor_to_program (void)
{
	if (COB_MODULE_PTR && COB_MODULE_PTR->cursor_pos) {
		cob_field	*cursor_field = COB_MODULE_PTR->cursor_pos;
		int		sline;
		int		scolumn;
		getyx (stdscr, sline, scolumn);
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
		if (max_line_column == 1) {
			cob_fatal_error (COB_FERROR_CODEGEN);
		}
		/* LCOV_EXCL_STOP */
		memcpy (buff, pos->data, maxsize);
		buff[maxsize + 1] = 0;
		if (!sscanf (buff, "%d", &pos_val)) {
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
		if (ret == 1) {
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

	getyx (stdscr, y, x);
	getmaxyx (stdscr, max_y, max_x);

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
	addnstr (data, size);
}

static void
cob_addch (const chtype c)
{
	raise_ec_on_truncation (1);
	addch (c);
}

/* Use only when raise_ec_on_truncation is called beforehand. */
static void
cob_addch_no_trunc_check (const chtype c)
{
	addch (c);
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
get_screen_item_line_and_col (cob_screen * s, int * const line,
			      int * const col)
{
	int		found_line = 0;
	int		found_col = 0;
	int	        is_screen_to_display = 1;
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
	cob_screen_attr (s->foreg, s->backg, s->attr);
#endif
	if (s->attr & COB_SCREEN_INPUT) {
		cob_screen_attr (s->foreg, s->backg, s->attr, stmt);
		if (stmt == DISPLAY_STATEMENT || !is_input) {
			default_prompt_char = ' ';
		} else {
		if (s->prompt) {
			default_prompt_char = s->prompt->data[0];
		} else {
			default_prompt_char = COB_CH_UL;
		}
		}
		p = f->data;
		raise_ec_on_truncation (f->size);
		for (size = 0; size < f->size; size++, p++) {
			if (s->attr & COB_SCREEN_SECURE) {
				cob_addch_no_trunc_check (COB_CH_AS);
			} else if (*p <= ' ') {
				cob_addch_no_trunc_check (default_prompt_char);
			} else {
				cob_addch_no_trunc_check ((const chtype)*p);
			}
		}
	} else if (!is_input) {
		cob_screen_attr (s->foreg, s->backg, s->attr, stmt);
		cob_addnstr ((char *)f->data, (int)f->size);
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

	refresh ();
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

	getyx (stdscr, y, x);
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
			return idx;
		}
	}
	return -1;
}

#ifdef NCURSES_MOUSE_VERSION
static int
mouse_to_exception_code (mmask_t mask) {
	int fret = -1;

	if (mask & BUTTON1_PRESSED) fret = COB_SCR_LEFT_PRESSED; // 2041;
	else if (mask & BUTTON1_CLICKED) fret = COB_SCR_LEFT_PRESSED; // 2041;
	else if (mask & BUTTON1_RELEASED) fret = COB_SCR_LEFT_RELEASED; // 2042;
	else if (mask & BUTTON1_DOUBLE_CLICKED) fret = COB_SCR_LEFT_DBL_CLICK; // 2043;
	else if (mask & BUTTON1_TRIPLE_CLICKED) fret = COB_SCR_LEFT_DBL_CLICK; // 2043;
	else if (mask & BUTTON2_PRESSED) fret = COB_SCR_MID_PRESSED; // 2044;
	else if (mask & BUTTON2_CLICKED) fret = COB_SCR_MID_PRESSED; // 2044;
	else if (mask & BUTTON2_RELEASED) fret = COB_SCR_MID_RELEASED; // 2045;
	else if (mask & BUTTON2_DOUBLE_CLICKED) fret = COB_SCR_MID_DBL_CLICK; // 2046;
	else if (mask & BUTTON2_TRIPLE_CLICKED) fret = COB_SCR_MID_DBL_CLICK; // 2046;
	else if (mask & BUTTON3_PRESSED) fret = COB_SCR_RIGHT_PRESSED; // 2047;
	else if (mask & BUTTON3_CLICKED) fret = COB_SCR_RIGHT_PRESSED; // 2047;
	else if (mask & BUTTON3_RELEASED) fret = COB_SCR_RIGHT_RELEASED; // 2048;
	else if (mask & BUTTON3_DOUBLE_CLICKED) fret = COB_SCR_RIGHT_DBL_CLICK; // 2049;
	else if (mask & BUTTON3_TRIPLE_CLICKED) fret = COB_SCR_RIGHT_DBL_CLICK; // 2049;
#if defined COB_HAS_MOUSEWHEEL
	else if (mask & BUTTON4_PRESSED) fret = COB_SCR_WHEEL_UP; // 2080;
	else if (mask & BUTTON5_PRESSED) fret = COB_SCR_WHEEL_DOWN; // 2081;
#endif
	else fret = COB_SCR_MOUSE_MOVE; // 2040;	/* mouse-moved (assumed) */

#if defined COB_HAS_MOUSEWHEEL
	if (mask & BUTTON_SHIFT) {
		if (fret < COB_SCR_WHEEL_UP /* 2080 */) {
			fret += 10;
		} else {
			fret += 4;
		}
	} else if (mask & BUTTON_CTRL) {
		if (fret < COB_SCR_WHEEL_UP /* 2080 */) {
			fret += 20;
		} else {
			fret += 8;
		}
	} else if (mask & BUTTON_ALT) {
		if (fret < COB_SCR_WHEEL_UP /* 2080 */) {
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


int
cob_ungot_input = -1;

static int
cob_get_input (int *keyf, MEVENT *meventp, const int accept_timeout) {
	int	kf = -1;
	int	ch = -1;

	if (cob_ungot_input >= 0) {
		kf = cob_ungot_input;
		cob_ungot_input = -1;
	} else {

#ifdef NCURSES_MOUSE_VERSION
		if (meventp != NULL) {
			/* prevent warnings about not intialized structure */
			memset (meventp, 0, sizeof (MEVENT));
		}
#endif
		timeout (accept_timeout);

		if (xad_is_initialized ()) {
			kf = xad_getch ();
		} else {
			ch = getch ();
			cob_convert_key (&ch, 0);

			kf = cob_convert_key_to_function (ch);
		}
	}

	if (keyf != NULL) {
		*keyf = kf;
	}

	return kf;
}


void
cob_unget_input (const int inp) {
	cob_ungot_input = inp;
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
#ifdef NCURSES_MOUSE_VERSION
	MEVENT		mevent;
#endif

	SET_FLD_AND_DATA_REFS (curr_index, sptr, s, sline, scolumn, right_pos, p);

	status = move_to_initial_field_pos (s->field, sline, scolumn, right_pos, 0, &p);
	if (status != ERR) {
		pending_accept = 0;
	}
	cob_screen_attr (s->foreg, s->backg, s->attr, ACCEPT_STATEMENT);

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
				cob_screen_attr (s->foreg, s->backg, s->attr, ACCEPT_STATEMENT);
				cob_move_cursor (cursor_clause_line, cursor_clause_col);
			} else {
				/* note: COBOL 2002 states that in this case the CURSOR clause is ignored,
				         while MicroFocus and ACUCOBOL-GT day "the nearest field" */
			}
		}
	}

#ifdef NCURSES_MOUSE_VERSION
	/* prevent warnings about not intialized structure */
	memset (&mevent, 0, sizeof (MEVENT));
#endif

	for (; ;) {
		if (s->prompt) {
			default_prompt_char = s->prompt->data[0];
		} else {
			default_prompt_char = COB_CH_UL;
		}

		refresh ();
		errno = 0;

		cob_get_input (&keyp, NULL, accept_timeout);

		/* FIXME: modularize (cob_screen_get_all, field_accept) and
		          use identical handling of keys wherever possible */

		if (keyp == ERR) {
			// CHECKME
			global_return = COB_SCR_TIME_OUT;
			goto screen_return;
		}

		if (keyp >= COB_SCR_FUNCTION_KEYS_MIN && keyp <= COB_SCR_FUNCTION_KEYS_MAX) {
			global_return = keyp;
			goto screen_return;
		}

#if 0 // TODO
#ifdef NCURSES_MOUSE_VERSION
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
#endif

		if (keyp < 0) {
			(void)flushinp ();
			cob_beep ();
			continue;
		}

		getyx (stdscr, cline, ccolumn);

		switch (keyp) {
		case COB_SCR_STOP_RUN:
			cob_stop_run (0);
			break;

		case COB_SCR_OK: // KEY_ENTER:
			if (finalize_all_fields (cob_base_inp, totl_index)) {
				cob_beep ();
				continue;
			}
			goto screen_return;

		case COB_SCR_PAGE_UP: // KEY_PPAGE:
			global_return = keyp;
			goto screen_return;
		case COB_SCR_PAGE_DOWN: // KEY_NPAGE:
			global_return = keyp;
			goto screen_return;
		case COB_SCR_PRINT: // KEY_PRINT:
			global_return = keyp;
			goto screen_return;
		case COB_SCR_ESC: // '\033':
			global_return = keyp;
			goto screen_return;

		case COB_SCR_NEXT_FIELD:
		case COB_SCR_TAB: // KEY_STAB:
			finalize_field_input (s);

			if (curr_index < totl_index - 1) {
				curr_index++;
			} else {
				curr_index = 0;
			}
			SET_FLD_AND_DATA_REFS (curr_index, sptr, s, sline, scolumn, right_pos, p);
			at_eof = 0;
			move_to_initial_field_pos (s->field, sline, scolumn, right_pos, 0, &p);
			cob_screen_attr (s->foreg, s->backg, s->attr, ACCEPT_STATEMENT);
			continue;
		case COB_SCR_PREV_FIELD:
		case COB_SCR_BACK_TAB: // KEY_BTAB:
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
			cob_screen_attr (s->foreg, s->backg, s->attr, ACCEPT_STATEMENT);
			continue;
		case COB_SCR_KEY_UP: // KEY_UP:
		// case COB_SCR_PREV_FIELD:
			finalize_field_input (s);

			curr_index = sptr->up_index;
			SET_FLD_AND_DATA_REFS (curr_index, sptr, s, sline, scolumn, right_pos, p);
			at_eof = 0;
			move_to_initial_field_pos (s->field, sline, scolumn, right_pos, 0, &p);
			cob_screen_attr (s->foreg, s->backg, s->attr, ACCEPT_STATEMENT);
			continue;
		case COB_SCR_KEY_DOWN: // KEY_DOWN:
		// case COB_SCR_NEXT_FIELD:
			finalize_field_input (s);

			curr_index = sptr->down_index;
			SET_FLD_AND_DATA_REFS (curr_index, sptr, s, sline, scolumn, right_pos, p);
			at_eof = 0;
			move_to_initial_field_pos (s->field, sline, scolumn, right_pos, 0, &p);
			cob_screen_attr (s->foreg, s->backg, s->attr, ACCEPT_STATEMENT);
			continue;
		case COB_SCR_KEY_HOME: // KEY_HOME:
			finalize_field_input (s);

			curr_index = 0;
			SET_FLD_AND_DATA_REFS (curr_index, sptr, s, sline, scolumn, right_pos, p);
			at_eof = 0;
			cob_move_cursor (sline, scolumn);
			cob_screen_attr (s->foreg, s->backg, s->attr, ACCEPT_STATEMENT);
			continue;
		case COB_SCR_KEY_END: // KEY_END:
			finalize_field_input (s);

			curr_index = totl_index - 1;
			SET_FLD_AND_DATA_REFS (curr_index, sptr, s, sline, scolumn, right_pos, p);
			at_eof = 0;
			cob_move_cursor (sline, scolumn);
			cob_screen_attr (s->foreg, s->backg, s->attr, ACCEPT_STATEMENT);
			continue;
		case COB_SCR_BACKSPACE: // KEY_BACKSPACE:
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
		case COB_SCR_KEY_LEFT: // KEY_LEFT:
			if (ccolumn > scolumn) {
				ccolumn--;
				cob_move_cursor (cline, ccolumn);
				p = s->field->data + ccolumn - scolumn;
			} else {
				ungetched = 1;
				// xad_ungetch (KEY_BTAB);
				cob_unget_input (COB_SCR_PREV_FIELD);
			}
			continue;
		case COB_SCR_KEY_RIGHT: // KEY_RIGHT:
			if (ccolumn < right_pos) {
				ccolumn++;
				cob_move_cursor (cline, ccolumn);
				p = s->field->data + ccolumn - scolumn;
			} else {
				// xad_ungetch ('\t');
				cob_unget_input (COB_SCR_NEXT_FIELD);
			}
			continue;
		case COB_SCR_INSERT: // KEY_IC:
			/* Insert key toggle */
			cob_toggle_insert();
			continue;
		case COB_SCR_DELETE: // KEY_DC:
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
			
#if 0 // TODO
#ifdef NCURSES_MOUSE_VERSION
		case KEY_MOUSE:
		{
			int mline = mevent.y;
			int mcolumn = mevent.x;
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
					cob_screen_attr (s->foreg, s->backg, s->attr, ACCEPT_STATEMENT);
					cob_move_cursor (mline, mcolumn);
					continue;
				}
			}
			mevent.bstate &= cob_mask_accept;
			if (mevent.bstate != 0) {
				global_return = mouse_to_exception_code (mevent.bstate);
				cob_move_cursor (mline, mcolumn);	// move cursor to pass position
				goto screen_return;
			}
			continue;
		}
#endif
#endif // 0 (TODO)
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
				if (islower (keyp)) {
					keyp = toupper (keyp);
				}
			} else if (s->attr & COB_SCREEN_LOWER) {
				if (isupper (keyp)) {
					keyp = tolower (keyp);
				}
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
						/* TODO: Signal auto-skip out of the last field */
						goto screen_return;
					} else {
						cob_unget_input (COB_SCR_NEXT_FIELD);
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
	refresh ();
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

	x = y = line = column = 0;
	if (s->line || s->column ||
	    s->attr & (COB_SCREEN_LINE_PLUS | COB_SCREEN_LINE_MINUS |
		       COB_SCREEN_COLUMN_PLUS | COB_SCREEN_COLUMN_MINUS)) {
		getyx (stdscr, y, x);
		if (x < 0 || y < 0) {
			/* not translated as "testing only" (should not happen) */
			cob_runtime_warning ("negative values from getyx");
			x = y = 0;
		}
		/* Column adjust */
		if (x > 0) {
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
		refresh ();
		cob_current_y = line;
		cob_current_x = column;
	}
}

static size_t
cob_prep_input (cob_screen *s)
{
	struct cob_inp_struct	*sptr;
	int			n;

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
			for (n = 1; n < s->occurs; ++n) {
				cob_screen_puts (s, s->value, cobsetptr->cob_legacy,
						 ACCEPT_STATEMENT);
			}
		}
		break;
	case COB_SCREEN_TYPE_ATTRIBUTE:
#if	0	/* RXWRXW - Attr */
		cob_screen_attr (s->foreg, s->backg, s->attr);
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
	int	n;

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
			for (n = 1; n < s->occurs; ++n) {
				cob_screen_puts (s, s->value, 0,
						 DISPLAY_STATEMENT);
			}
		}
		break;
	case COB_SCREEN_TYPE_ATTRIBUTE:
		cob_screen_attr (s->foreg, s->backg, s->attr,
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
		} else {
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
				if (ret == 1) {
#if 0				/* Throw an exception? EC-SCREEN-IMP-LINE-VAR-LENGTH? */
					cob_set_exception (COB_EC_SCREEN_IMP);
#else
					cob_fatal_error (COB_FERROR_CODEGEN);
#endif
				}
				/* LCOV_EXCL_STOP */
			}
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
	init_cob_screen_if_needed ();

	origin_y = line;
	origin_x = column;

	status = cob_move_cursor (line, column);
	if (status != ERR) {
		pending_accept = 1;
	}
	cob_screen_iterate (s);
	refresh ();
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
		handle_status (COB_SCR_MAX_FIELD);
		return;
	}

	/* No input field is an error */
	if (!totl_index) {
		pass_cursor_to_program ();
		handle_status (COB_SCR_NO_FIELD);
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
	handle_status (global_return);
}

static void
field_display (cob_field *f, const int line, const int column, cob_field *fgc,
	       cob_field *bgc, cob_field *fscroll, cob_field *size_is,
	       const cob_flags_t fattr)
{
	int	sline;
	int	scolumn;
	int	size_display, fsize;
	int	status;
	char	fig_const;	/* figurative constant character */

	cob_flags_t	sattr = fattr;
	cob_field	*sfgc = NULL;
	cob_field	*sbgc = NULL;

	/* LCOV_EXCL_START */
	if (!f) {
		cob_fatal_error(COB_FERROR_CODEGEN);
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
		scrollok (stdscr, 1);
		scrl (sline);
		scrollok (stdscr, 0);
		refresh ();
	}

	sline = line;
	scolumn = column;
	status = cob_move_cursor (sline, scolumn);
	if (status != ERR) {
		pending_accept = 1;
	}

	/*
	 * Create temporary fields to contain values that might be changed by
	 * xad_set_attributes et.al. to avoid side effects
	 */
	sfgc = xad_build_int_field (fgc ? cob_get_int (fgc) : -1);
	sbgc = xad_build_int_field (bgc ? cob_get_int (bgc) : -1);

	/* set controllable attributes */
	xad_set_attributes (XAD_ATTRIBUTES, sfgc, sbgc, &sattr);

	cob_screen_attr (sfgc, sbgc, sattr, DISPLAY_STATEMENT);

	if (!(sattr & COB_SCREEN_NO_DISP)) {
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
			cob_addnstr ((char *)f->data, cob_min_int (size_display, fsize));
			if (size_display > fsize) {
				/* WITH SIZE larger than field displays trailing spaces */
				cob_addnch (size_display - fsize, COB_CH_SP);
			}
		}
	}

	display_cursor_y = sline;
	display_cursor_x = scolumn + size_display;

	if (sattr & COB_SCREEN_EMULATE_NL) {
		// if (++sline >= LINES) {
		if (++sline >= get_screen_lines ()) {
			sline = 0;
		}
		cob_move_cursor (sline, 0);
	}
	refresh ();

	if (sfgc != NULL) {
		cob_field_free (sfgc);
	}

	if (sbgc != NULL) {
		cob_field_free (sbgc);
	}
}

static void
field_accept (cob_field *f, const int sline, const int scolumn, cob_field *fgc,
	      cob_field *bgc, cob_field *fscroll, cob_field *ftimeout,
	      cob_field *prompt, cob_field *size_is, const cob_flags_t fattr)
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
	size_t		size_accept = 0;	/* final size to accept */
	cob_field	temp_field;
#if	0	/* RXWRXW - Screen update */
	cob_field	char_temp;
	unsigned char	space_buff[4];
#endif
#ifdef NCURSES_MOUSE_VERSION
	MEVENT		mevent;
#endif

	memset (COB_TERM_BUFF, ' ', (size_t)COB_MEDIUM_MAX);
	temp_field.data = COB_TERM_BUFF;
	temp_field.attr = &const_alpha_attr;
#if	0	/* RXWRXW - Screen update */
	char_temp.data = space_buff;
	char_temp.attr = &const_alpha_attr;
	char_temp.size = 1;
	space_buff[0] = ' ';
	space_buff[1] = 0;
#endif

	origin_y = 0;
	origin_x = 0;
#ifdef NCURSES_MOUSE_VERSION
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
		scrollok (stdscr, 1);
		scrl (keyp);
		scrollok (stdscr, 0);
		refresh ();
	}
	cobglobptr->cob_exception_code = 0;

	status = cob_move_cursor (sline, scolumn);
	if (status != ERR) {
		pending_accept = 0;
	}

	cob_screen_attr (fgc, bgc, fattr, ACCEPT_STATEMENT);

	if (f) {
		if (size_is) {
			size_accept = cob_get_int (size_is);
			/* SIZE ZERO is ignored */
			if (size_accept == 0) {
				size_accept = (int)f->size;
			}
		} else {
			size_accept = f->size;
		}

		p = COB_TERM_BUFF;
		temp_field.size = size_accept;
		if (fattr & COB_SCREEN_UPDATE) {
			cob_move (f, &temp_field);
		}

		raise_ec_on_truncation (size_accept);
		for (count = 0; count < (size_t) cob_min_int (size_accept, f->size); count++) {
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
		if (size_accept > (int)f->size) {
			cob_addnch (size_accept - f->size, COB_CH_SP);
		}
		cob_move_cursor (sline, scolumn);
#if	0	/* RXWRXW - Screen update */
		if (!(fattr & COB_SCREEN_UPDATE)) {
			if (cob_field_is_numeric_or_numeric_edited (f)) {
				cob_set_int (f, 0);
			} else {
				cob_move (&char_temp, f);
			}
		}
#endif

		accept_cursor_y = sline;
		accept_cursor_x = scolumn + size_accept;

		/* position for the SPECIAL-NAMES CURSOR clause, if given */
		{
			int		cursor_clause_line;
			int		cursor_clause_col;
			get_cursor_from_program (&cursor_clause_line, &cursor_clause_col);

			if (cursor_clause_line == sline
			 && cursor_clause_col > scolumn
			 && cursor_clause_col < scolumn + (int)f->size) {
				cob_move_cursor (cursor_clause_line, cursor_clause_col);
			}
		}

		right_pos = scolumn + size_accept - 1;
		p = COB_TERM_BUFF;
	} else {
		right_pos = 0;
		p = NULL;
	}
	count = 0;

	timeout (get_accept_timeout (ftimeout));

	/* Get characters from keyboard, processing each one. */
	for (; ;) {
		/* Show prompt characters. */
		if (f) {
			/* Get current line, column. */
			getyx (stdscr, cline, ccolumn);
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
			refresh ();
		}
		errno = 0;

		/* Get a character. */
		keyp = getch ();

		/* Key error - time out. */
		if (keyp == ERR) {
			fret = COB_SCR_TIME_OUT;
			goto field_return;
		}
		/* Return function keys F1 through F64 */
		if (keyp > KEY_F0 && keyp < KEY_F(65)) {
			fret = COB_SCR_F0 + keyp - KEY_F0;
			goto field_return;
		}

		cob_convert_key (&keyp, 1U);
		if (keyp <= 0) {
			(void)flushinp ();
			cob_beep ();
			continue;
		}

#ifdef NCURSES_MOUSE_VERSION
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
			fret = COB_SCR_PAGE_UP;
			goto field_return;
		case KEY_NPAGE:
			/* Page down. */
			fret = COB_SCR_PAGE_DOWN;
			goto field_return;
		case KEY_UP:
			/* Up arrow. */
			fret = COB_SCR_KEY_UP;
			goto field_return;
		case KEY_DOWN:
			/* Down arrow. */
			fret = COB_SCR_KEY_DOWN;
			goto field_return;
		case KEY_PRINT:
			/* Print key. */
			/* pdcurses not returning this ? */
			fret = COB_SCR_PRINT;
			goto field_return;
		case 033:
			/* Escape key. */
			fret = COB_SCR_ESC;
			goto field_return;
		case KEY_STAB:
			/* Tab key. */
			fret = COB_SCR_TAB;
			goto field_return;
		case KEY_BTAB:
			/* Shift-Tab key, Back tab. */
			fret = COB_SCR_BACK_TAB;
			goto field_return;
		default:
			break;
		}

		/* extension: ACCEPT OMITTED */
		if (!f) {
			/* special keys for ACCEPT OMITTED */
			switch (keyp) {
			case KEY_LEFT:
				fret = COB_SCR_KEY_LEFT;
				goto field_return;
			case KEY_RIGHT:
				fret = COB_SCR_KEY_RIGHT;
				goto field_return;
			case KEY_IC:
				/* Insert key. */
				fret = COB_SCR_INSERT;
				goto field_return;
			case KEY_DC:
				/* Delete key. */
				fret = COB_SCR_DELETE;
				goto field_return;
			case KEY_BACKSPACE:
				/* Backspace key. */
				fret = COB_SCR_BACKSPACE;
				goto field_return;
			case KEY_HOME:
				/* Home key. */
				fret = COB_SCR_KEY_HOME;
				goto field_return;
			case KEY_END:
				/* End key. */
				fret = COB_SCR_KEY_END;
				goto field_return;
#ifdef NCURSES_MOUSE_VERSION
			case KEY_MOUSE:
			{
				int mline = mevent.y;
				int mcolumn = mevent.x;
				mevent.bstate &= cob_mask_accept;
				if (mevent.bstate != 0) {
					fret = mouse_to_exception_code (mevent.bstate);
					cob_move_cursor (mline, mcolumn);	// move cursor to pass position
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
				fret = COB_SCR_KEY_LEFT;
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
				fret = COB_SCR_KEY_RIGHT;
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
			
#ifdef NCURSES_MOUSE_VERSION
		case KEY_MOUSE:
		{
			int mline = mevent.y;
			int mcolumn = mevent.x;
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
				cob_move_cursor (mline, mcolumn);	// move cursor to pass position
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
				if (islower (keyp)) {
					keyp = toupper (keyp);
				}
			} else if (fattr & COB_SCREEN_LOWER) {
				if (isupper (keyp)) {
					keyp = tolower (keyp);
				}
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
	pass_cursor_to_program ();
	handle_status (fret);
	if (f) {
		cob_move (&temp_field, f);
		cob_move_cursor (sline, right_pos + 1);
	}
	refresh ();
}

static void
field_accept_from_curpos (cob_field *f, cob_field *fgc,
	      cob_field *bgc, cob_field *fscroll, cob_field *ftimeout,
	      cob_field *prompt, cob_field *size_is, const cob_flags_t fattr)
{
	int		cline;
	size_t		ccolumn;

	/* Get current line, column. */
	getyx (stdscr, cline, ccolumn);

	/* accept field */
	field_accept (f, cline, ccolumn, fgc, bgc, fscroll, ftimeout, prompt, size_is, fattr);
}

static void
field_display_at_curpos (cob_field *f,
	cob_field *fgc, cob_field *bgc, cob_field *fscroll,
	cob_field *size_is, const cob_flags_t fattr)
{
	int		cline;
	size_t		ccolumn;

	/* Get current line, column. */
	getyx (stdscr, cline, ccolumn);

	field_display (f, cline, ccolumn, fgc, bgc, fscroll, size_is, fattr);
}

/* Global functions */

void
cob_screen_display (cob_screen *s, cob_field *line, cob_field *column,
		    const int zero_line_col_allowed)
{
	int	sline;
	int	scolumn;

	extract_line_and_col_vals (line, column, DISPLAY_STATEMENT,
				   zero_line_col_allowed, &sline, &scolumn);
	screen_display (s, sline, scolumn);
}

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

void
cob_field_display (cob_field *f, cob_field *line, cob_field *column,
		   cob_field *fgc, cob_field *bgc, cob_field *fscroll,
		   cob_field *size_is, const cob_flags_t fattr)
{
	int	sline;
	int	scolumn;

	init_cob_screen_if_needed ();
	/*
	  LINE/COL 0 is always allowed as it is impossible to specify it in the
	  standard format (DISPLAY ... UPON CRT) and all implementations of the
	  extended screen format (DISPLAY ... WITH UNDERLINE, HIGHLIGHT, etc.)
	  require it.
	*/
	extract_line_and_col_vals (line, column, DISPLAY_STATEMENT, 1, &sline,
				   &scolumn);
	field_display (f, sline, scolumn, fgc, bgc, fscroll, size_is, fattr);
}

void
cob_field_accept (cob_field *f, cob_field *line, cob_field *column,
		  cob_field *fgc, cob_field *bgc, cob_field *fscroll,
		  cob_field *ftimeout, cob_field *prompt, cob_field *size_is,
		  const cob_flags_t fattr)
{
	int	sline;
	int	scolumn;

	/* See above comment in cob_field_display. */
	extract_line_and_col_vals (line, column, ACCEPT_STATEMENT, 1, &sline,
				   &scolumn);
	field_accept (f, sline, scolumn, fgc, bgc, fscroll, ftimeout, prompt,
		      size_is, fattr);
}

int
cob_sys_clear_screen (void)
{
	init_cob_screen_if_needed ();

	clear ();
	refresh ();
	cob_current_y = 0;
	cob_current_x = 0;
	return 0;
}

void
cob_screen_set_mode (const cob_u32_t smode)
{
	init_cob_screen_if_needed ();

	if (!smode) {
		refresh ();
		def_prog_mode ();
		endwin ();
	} else {
		reset_prog_mode ();
		refresh ();
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
		field_accept (NULL, 0, 0, NULL, NULL, NULL, NULL, NULL, NULL, 0);
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
			field_accept_from_curpos (NULL, NULL, NULL, NULL, NULL, NULL, NULL, flags);
		}
		cobglobptr->cob_screen_initialized = 0;
		clear ();
		cob_move_to_beg_of_last_line ();
		delwin (stdscr);
		endwin ();
#ifdef	HAVE_CURSES_FREEALL
		_nc_freeall ();
#endif
		if (cob_base_inp) {
			cob_free (cob_base_inp);
			cob_base_inp = NULL;
		}
	}
	COB_ACCEPT_STATUS = 0;
}

#else	/* WITH_EXTENDED_SCREENIO */

void
cob_exit_screen (void)
{
}

void
cob_field_display (cob_field *f, cob_field *line, cob_field *column,
		   cob_field *fgc, cob_field *bgc, cob_field *fscroll,
		   cob_field *size_is, const cob_flags_t fattr)
{
	COB_UNUSED (f);
	COB_UNUSED (line);
	COB_UNUSED (column);
	COB_UNUSED (fgc);
	COB_UNUSED (bgc);
	COB_UNUSED (fscroll);
	COB_UNUSED (size_is);
	COB_UNUSED (fattr);
}

void
cob_field_accept (cob_field *f, cob_field *line, cob_field *column,
		  cob_field *fgc, cob_field *bgc, cob_field *fscroll,
		  cob_field *ftimeout, cob_field *prompt,
		  cob_field *size_is, const cob_flags_t fattr)
{
	COB_UNUSED (f);
	COB_UNUSED (line);
	COB_UNUSED (column);
	COB_UNUSED (fgc);
	COB_UNUSED (bgc);
	COB_UNUSED (fscroll);
	COB_UNUSED (ftimeout);
	COB_UNUSED (prompt);
	COB_UNUSED (size_is);
	COB_UNUSED (fattr);
}

void
cob_screen_display (cob_screen *s, cob_field *line, cob_field *column,
		    const int zero_line_col_allowed)
{
	COB_UNUSED (s);
	COB_UNUSED (line);
	COB_UNUSED (column);
	COB_UNUSED (zero_line_col_allowed);
}

void
cob_screen_accept (cob_screen *s, cob_field *line,
		   cob_field *column, cob_field *ftimeout,
		    const int zero_line_col_allowed)
{
	COB_UNUSED (s);
	COB_UNUSED (line);
	COB_UNUSED (column);
	COB_UNUSED (ftimeout);
	COB_UNUSED (zero_line_col_allowed);
}

void
cob_screen_set_mode (const cob_u32_t smode)
{
	COB_UNUSED (smode);
}

int
cob_sys_clear_screen (void)
{
	return 0;
}

#endif	/* WITH_EXTENDED_SCREENIO */

void
cob_screen_line_col (cob_field *f, const int l_or_c)
{
	init_cob_screen_if_needed ();
#ifdef	WITH_EXTENDED_SCREENIO
	if (!l_or_c) {
		// cob_set_int (f, (int)LINES);
		cob_set_int (f, get_screen_lines ());
	} else {
		// cob_set_int (f, (int)COLS);
		cob_set_int (f, get_screen_columns ());
	}
#else
	if (!l_or_c) {
		cob_set_int (f, 24);
	} else {
		cob_set_int (f, 80);
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
	if (!cobglobptr->cob_screen_initialized &&
	    COB_BEEP_VALUE != 2) {
		cob_screen_init ();
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
	cob_set_int (f, COB_ACCEPT_STATUS);
}

/* get CurSoR position on screen */
int
cob_sys_get_csr_pos (unsigned char *fld)
{
#ifdef	WITH_EXTENDED_SCREENIO
	int	cline;
	int	ccol;
#endif

	COB_CHK_PARMS (CBL_GET_CSR_POS, 1);
	init_cob_screen_if_needed ();

#ifdef	WITH_EXTENDED_SCREENIO
	getyx (stdscr, cline, ccol);
	fld[0] = (unsigned char)cline;
	fld[1] = (unsigned char)ccol;

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
#else
	COB_UNUSED (fld);
#endif
	return 0;
}

/* set CurSoR position on screen */
int
cob_sys_set_csr_pos (unsigned char *fld)
{
#ifdef	WITH_EXTENDED_SCREENIO
	int	cline;
	int	ccol;
#endif

	COB_CHK_PARMS (CBL_SET_CSR_POS, 1);
	init_cob_screen_if_needed ();

#ifdef	WITH_EXTENDED_SCREENIO
	cline = fld[0];
	ccol= fld[1];
	return move (cline, ccol);
#else
	COB_UNUSED (fld);
	return 0;
#endif
}

/* get current (physical) screen size */
int
cob_sys_get_scr_phys_size (unsigned char *line, unsigned char *col)
{
	COB_CHK_PARMS (CBL_GET_SCR_SIZE, 2);
	init_cob_screen_if_needed ();

#ifdef	WITH_EXTENDED_SCREENIO
	*line = (unsigned char)LINES;
	*col = (unsigned char)COLS;
#else
	*line = 24U;
	*col = 80U;
#endif
	return 0;
}

int
cob_get_scr_phys_cols (void)
{
	init_cob_screen_if_needed();
#ifdef	WITH_EXTENDED_SCREENIO
	return (int)COLS;
#else
	return 80;
#endif
}

int
cob_get_scr_phys_lines (void)
{
	init_cob_screen_if_needed();
#ifdef	WITH_EXTENDED_SCREENIO
	return (int)LINES;
#else
	return 24;
#endif
}

/* get current (configurable) screen geometry */
int
cob_sys_get_scr_size (unsigned char *line, unsigned char *col)
{
	COB_CHK_PARMS (CBL_GET_SCR_SIZE, 2);
	init_cob_screen_if_needed ();

#ifdef	WITH_EXTENDED_SCREENIO
	*line = (unsigned char)get_screen_lines ();
	*col = (unsigned char)get_screen_columns ();
#else
	*line = 24U;
	*col = 80U;
#endif
	return 0;
}

int
cob_get_scr_cols (void)
{
	init_cob_screen_if_needed();
#ifdef	WITH_EXTENDED_SCREENIO
	return get_screen_columns ();
#else
	return 80;
#endif
}

int
cob_get_scr_lines (void)
{
	init_cob_screen_if_needed();
#ifdef	WITH_EXTENDED_SCREENIO
	return get_screen_lines ();
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

	/* Extended ACCEPT status returns */
	if (cobsetptr->cob_extended_status == 0) {
		cobsetptr->cob_use_esc = 0;
	}

	if (curr_setting_insert_mode != COB_INSERT_MODE) {
		/* Depending on insert mode set vertical bar cursor (on)
		   or square cursor (off) - note: the cursor change may has no
		   effect in all curses implementations / terminals */
		if (COB_INSERT_MODE == 0) {
			(void)curs_set (2);	/* set square cursor */
		} else {
			(void)curs_set (1);	/* set vertical bar cursor */
		}
		curr_setting_insert_mode = COB_INSERT_MODE;
	}

#ifdef HAVE_MOUSEINTERVAL
	mouseinterval (COB_MOUSE_INTERVAL);
#endif
#ifdef NCURSES_MOUSE_VERSION
	if ((int)curr_setting_mouse_flags != (int)COB_MOUSE_FLAGS) {
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
		cobsetptr->cob_exit_msg = cob_strdup (_("end of program, please press a key to exit"));
	}

	cob_settings_screenio ();
}

#ifdef WITH_EXTENDED_ACCDIS

/*
 * Allocate a new cob_screen item
 */
cob_screen *
cob_screen_alloc () {
	cob_screen *s = NULL;

	if ((s = cob_malloc (sizeof (cob_screen))) == NULL) {
		return NULL;
	}

	s->next = s->prev =s->child = s->parent = NULL;
	s->field = s->value = s->line = s->column = NULL;
	s->foreg = s->backg = s->prompt = NULL;
	s->type = s->occurs = s->attr = 0;

	return s;
}


/*
 * Free a previously allocated cob_screen item
 */
void
cob_screen_free (cob_screen *s) {
	if (s == NULL) {
		return;
	}

	cob_free (s);
}


/*
 * Allocate a new cob_field item and it's attributes. If 'size' is > 0, allocate data accorgingly
 */
cob_field *
cob_field_alloc (int size) {
	cob_field *f = NULL;

	if ((f = cob_malloc (sizeof (cob_field))) == NULL) {
		return NULL;
	}

	f->size = 0;
	f->data = NULL;
	f->attr = NULL;

	if ((f->attr = cob_malloc (sizeof (cob_field_attr))) == NULL) {
		cob_free (f);
		return NULL;
	}

	if (size > 0) {
		if ((f->data = cob_malloc (size)) == NULL) {
			cob_free (f->attr);
			cob_free (f);
			return NULL;
		}

		f->size = size;
	}

	return f;
}


/*
 * Free a previously allocated cob_field item, it's attributes and it's "own" data
 */
void
cob_field_free (cob_field *f) {
	if (f == NULL) {
		return;
	}

	if (f->data != NULL) {
		/* Don't free() SHARED_DATA */
		if (!(f->attr && (f->attr->flags & COB_FLAG_SHARED_DATA))) {
			cob_free (f->data);
		}
	}

	if (f->attr != NULL) {
		cob_free (f->attr);
	}

	cob_free (f);
}


/*
 * Initialize XAD: Read keyboard mappings and initialize attributes
 */
void
cob_xad_init () {
	char	default_keymap_file[] = "keymap.map";
	char	*penv;
	char	keymap_file[COB_MEDIUM_MAX];

	if ((penv = getenv("COB_XAD_KEYMAP")) == NULL) {
		penv = default_keymap_file;
	}

	if (index (penv, SLASH_CHAR) != NULL) {
		snprintf (keymap_file, sizeof (keymap_file), "%s", penv);
	} else {
		snprintf (keymap_file, sizeof (keymap_file), "%s%c%s", COB_CONFIG_DIR, SLASH_CHAR, penv);
	}

	(void) xad_init (keymap_file);

	XAD_ATTRIBUTES = 0;
	xad_setbgcol (COLOR_BLACK, NULL);
	xad_setfgcol (COLOR_WHITE, NULL);
}


/*
 * Read a keyboard mappings file
 */
void
cob_xad_read_keymap (char *filename_) {
	char	*penv;
	char	keymap_file[COB_MEDIUM_MAX];

	if (filename_ == NULL) {
		return;
	}

	if (index (filename_, SLASH_CHAR) != NULL) {
		snprintf (keymap_file, sizeof (keymap_file), "%s", filename_);
	} else {
		snprintf (keymap_file, sizeof (keymap_file), "%s%c%s", COB_CONFIG_DIR, SLASH_CHAR, filename_);
	}

	(void) xad_init (keymap_file);
}


/*
 * Build a cob_screen item as indicated by a cob_xad_mask entry
 */
cob_screen *
xad_construct_screen (cob_xad_mask *m) {
	cob_screen	*s = NULL;

	if ((s = cob_screen_alloc ()) == NULL) {
		return NULL;
	}

	switch (COB_FIELD_TYPE (m->field)) {
	case COB_TYPE_GROUP:
		break;

	default:
		s->next = NULL;
		s->prev = NULL;
		s->child = NULL;
		s->parent = NULL;
		s->field = m->field;
		s->value = NULL;
		s->line = NULL;
		s->column = NULL;
		s->occurs = 0;
	}

	return s;
}


/*
 * Clone a cob_field item
 */
cob_field *
cob_field_clone (cob_field *fld) {
	cob_field	*f = NULL;

	if (fld == NULL) {
		return NULL;
	}

	if ((f = cob_field_alloc (fld->size)) == NULL) {
		return NULL;
	}

	memcpy(f->attr, fld->attr, sizeof (cob_field_attr));
	if (fld->attr->flags & COB_FLAG_SHARED_DATA) {
		f->size = fld->size;
		f->data = fld->data;
	} else {
		if (f->size > 0) {
			memcpy(f->data, fld->data, f->size);
		}
	}

	return f;
}


/*
 * Initialize a cob_field item to contain an int
 *
 * n:	value to initialize cob_field with
 */
static cob_field *
xad_init_int_field (cob_field *f, int n) {
	cob_field	field;
	cob_field_attr	attr;

	COB_FIELD_INIT (4, (unsigned char *)&n, &attr);
	COB_ATTR_INIT (COB_TYPE_NUMERIC_BINARY, 9, 0,
		       COB_FLAG_HAVE_SIGN | COB_FLAG_REAL_BINARY, NULL);

	return cob_field_clone (&field);
}


/*
 * Build a cob_field item containing an int
 *
 * n:	value to initialize cob_field with
 */
static cob_field *
xad_build_int_field (int n) {
	cob_field	field;
	cob_field_attr	attr;

	COB_FIELD_INIT (4, (unsigned char *)&n, &attr);
	COB_ATTR_INIT (COB_TYPE_NUMERIC_BINARY, 9, 0,
		       COB_FLAG_HAVE_SIGN | COB_FLAG_REAL_BINARY, NULL);

	return cob_field_clone (&field);
}


/*
 * Build a cob_field item copying specifications from a template and
 * pointing to data of another field at some offset
 *
 * template:	cob_field containing the field specifications
 * base:	cob_field containing the data to point to
 * offset:	offset into the data to point to
 */
static cob_field *
xad_build_shared_field (cob_field *template, cob_field *base, int offset) {
	cob_field	field;
	cob_field	*f;
	cob_field_attr	attr;
	if ((f = cob_field_alloc (0)) == NULL) {
		return NULL;
	}

	memcpy(&attr, template->attr, sizeof (cob_field_attr));
	attr.flags |= COB_FLAG_SHARED_DATA;
	memcpy(f->attr, &attr, sizeof (cob_field_attr));
	f->size = template->size;
	f->data = base->data + offset;

	return f;
}


/*
 * Auxilliary routine: Fix connections between cob_screen items
 */
void
xad_define_screen_fix_prev (cob_screen *s) {
	if (s == NULL) {
		return;
	}

	if (s->child != NULL) {
		s->child->prev = NULL;
		xad_define_screen_fix_prev (s->child);
	}

	if (s->next != NULL) {
		xad_define_screen_fix_prev (s->next);
	}
}


/*
 * Auxilliary routine: Move the dynamically created cob_fields from .value to .field
 */
void
xad_define_screen_fix_fields (cob_screen *s) {
	if (s == NULL) {
		return;
	}

	if (s->value != NULL) {
		s->field = s->value;
		s->value = NULL;
	}

	if (s->child != NULL) {
		xad_define_screen_fix_fields (s->child);
	}

	if (s->next != NULL) {
		xad_define_screen_fix_fields (s->next);
	}
}


/*
 * Create a structure of connected cob_screen items as indicated by a cob_xad_mask,
 * that can later on be used as if they had been defined in a SCREEN SECTION.
 * For each screen field a cob_field is created, pointing into the data of the
 * first field at the respective offset.
 * Fields or groups with an OCCURS clause are repeated accordingly.
 *
 * mask[]:	Array of cob_xad_mask items describing one field each
 * fgc:		default foreground color
 * bgc:		default background color
 * prompt:	default prompt character
 * fattr:	default field flags
 */
cob_screen *
xad_define_screen (cob_xad_mask mask[], cob_field *fgc, cob_field *bgc, cob_field *prompt, const cob_flags_t fattr) {
	cob_xad_mask	*m;
	cob_field	*base = NULL;
	cob_screen	*scr = NULL;
	cob_screen	*s = NULL;
	cob_field	*f = NULL;
	cob_screen	*prev = NULL;
	int		offset = 0;
	int		o_offset = 0;
	int		idx = 0;
	int		i;
	int		nr_cols = 80;
	int		mask_version = -1;

	typedef struct {
	    int		idx;
	    int		cnt;
	}		stack_t;
	stack_t		occ_stack[50];
	int		occ_stack_p = 0;

	/*
	// TODO: configurable geometry
	nr_cols = COLS;

	if (nr_cols < 1 || nr_cols > 200) {
		nr_cols = 80;
	}
	*/

	nr_cols = 80;

	offset = 0;
	s = NULL;
	prev = NULL;
	scr = NULL;

	i = 0;
	idx = 0;

	mask_version = 0;

	/* Recognize version */
	if (mask && mask->type == COB_XAD_MASK_TYPE_VERSION) {
		mask_version = mask->size;
		idx++;
	}

	/* The field of the first (group-) item will be our base - data container */
	base = (mask + idx)->field;

	while ((mask + idx) != NULL && (mask + idx)->field != NULL && ++i <= COB_INP_FLD_MAX) {
		m = mask + idx;

		// Has this element an OCCURS clause?
		if (m->occurs > 1) { // CHECKME: ... occurs 0 to 1 times dependind on ...?
			// Yes.
			// Is this our current OCCURS element?
			if (occ_stack_p > 0 && occ_stack[occ_stack_p - 1].idx == idx) {
				// Yes. Next loop
				occ_stack[occ_stack_p - 1].cnt -= 1;
			} else {
				// No. Save OCCURS info
				if (occ_stack_p < 50) {
					occ_stack[occ_stack_p].idx = idx;
					occ_stack[occ_stack_p].cnt = m->occurs - 1;
					occ_stack_p++;
				} // FIXME: Handle stack overflow
			}
		}

		o_offset = offset;
		prev = s;
		s = xad_construct_screen (m);
		s->prev = prev;
		if (scr == NULL) {
			scr = s;
		}

		/* HACK: Save m->field for now, as we might still need it. */
		s->field = m->field;
		s->line = xad_build_int_field (offset / nr_cols + 1);
		s->column = xad_build_int_field (offset % nr_cols + 1);

		// if (m->field->attr->type == COB_TYPE_GROUP) {
		if (m->type == COB_XAD_MASK_TYPE_GROUP) {
			s->type = COB_SCREEN_TYPE_GROUP;
		} else {
			if (m->type == COB_XAD_MASK_TYPE_FIELD) {
				s->type = COB_SCREEN_TYPE_FIELD;
				s->attr = (fattr | COB_SCREEN_INPUT | COB_SCREEN_USE_XAD_COLORS);
				s->field = m->field;
				s->foreg = fgc;
				s->backg = bgc;
				s->prompt = prompt;

				/* HACK: use s->value as temporory storage */
				s->value = xad_build_shared_field(s->field, base, o_offset);
			}

			offset += m->size;
			// offset += m->field->size;
		}

		if (m->parent != NULL) {
			cob_screen *p = s->prev;

			while (p != NULL) {
				if (p->field == m->parent) {
					cob_screen	*ch;

					if (p->child) {
						for (ch = p->child; ch->next; ch = ch->next);

						ch->next = s;
					} else {
						p->child = s;
					}
					s->next = NULL;
					s->parent = p;

					break;
				} else {
					p = p->prev;
				}
			}
		}

		idx++;

		// Are we within some OCCURS?
		if (occ_stack_p > 0) {
			int occ_idx = occ_stack[occ_stack_p - 1].idx;

			if ((mask + idx)->field == NULL || (mask + idx)->depth <= (mask + occ_idx)->depth) {
				// End of current OCCURS. Another loop?
				if (occ_stack[occ_stack_p - 1].cnt > 0) {
					// Yes. Set idx to start of OCCURS
					/// occ_stack[occ_stack_p - 1].count -= 1;
					idx = occ_stack[occ_stack_p - 1].idx;
				} else {
					// No. Leave idx on following element
					occ_stack_p--;
					occ_idx = -1;
					// TODO: Fix offset according to ODOSLIDE
				}
			}
		}
	}

	if (scr != NULL) {
		xad_define_screen_fix_prev (scr);
		xad_define_screen_fix_fields (scr);
	}

	return scr;
}


/*
 * Destroy and free a previously allocated cob_screen - item.
 * Take care not to free() data we didn't allocate.
 *
 * s: cob_screen - item
 */
void
xad_destroy_screen (cob_screen *s) {
	if (s->line) {
		cob_field_free (s->line);
		s->line = NULL;
	}

	if (s->column) {
		cob_field_free (s->column);
		s->column = NULL;
	}

	if (s->child) {
		xad_destroy_screen (s->child);
		s->child = NULL;
	}

	if (s->next) {
		xad_destroy_screen (s->next);
		s->next = NULL;
	}

	if (s->field && s->field->attr && (s->field->attr->flags & COB_FLAG_SHARED_DATA)) {
		/* assume dynamically created field */
		cob_field_free (s->field);
	}

	/* Don't free() stuff we didn't allocate. */

	cob_screen_free (s);
}


/*
 * Set cobol attributes and colors from xad_attrs
 *
 * xattrs:	attributes in xad-format
 * fgc:		foreground color to return
 * bgc:		background color to return
 * fattr:	field attributes to return
 */
void
xad_set_attributes (xad_attrs_t xattrs, cob_field *fgc, cob_field *bgc, cob_flags_t *fattr) {
	if (! (*fattr & COB_SCREEN_ATTRIBUTES)) {
		if (xattrs & COB_XAD_ATTR_BLINK) {
			*fattr |= COB_SCREEN_BLINK;
		}

		if (xattrs & COB_XAD_ATTR_HIGHLIGHT) {
			*fattr |= COB_SCREEN_HIGHLIGHT;
		}

		if (xattrs & COB_XAD_ATTR_LOWLIGHT) {
			*fattr |= COB_SCREEN_LOWLIGHT;
		}

		if (xattrs & COB_XAD_ATTR_REVERSE) {
			*fattr |= COB_SCREEN_REVERSE;
		}

		if (xattrs & COB_XAD_ATTR_UNDERLINE) {
			*fattr |= COB_SCREEN_UNDERLINE;
		}

		if (xattrs & COB_XAD_ATTR_ALTCHARSET) {
			*fattr |= COB_SCREEN_ALTCHARSET;
		}

		if (xattrs & COB_XAD_ATTR_NODISPLAY) {
			*fattr |= COB_SCREEN_NO_DISP;
		}
	}

	*fattr |= COB_SCREEN_USE_XAD_COLORS;
	if (fgc) {
		if (cob_get_int (fgc) == 0) { // CHECKME
			cob_set_int (fgc, (*fattr & COB_XAD_FGCOL) >> 16);
		}
	}

	if (bgc) {
		if (cob_get_int (bgc) == 0) { // CHECKME
			cob_set_int (bgc, (*fattr & COB_XAD_BGCOL) >> 24);
		}
	}
}


/*
 * Get the current xad-attributes (attributes and colors)
 *
 * p0:	cob_field to return complete attributes
 */
int
cbl_xad_getcolattrs (void *p0) {
	cob_set_int (COB_MODULE_PTR->cob_procedure_params[0], XAD_ATTRIBUTES);
	return 0;
}


/*
 * Set the current xad-attributes (attributes and colors)
 *
 * p0:	cob_field containing the complete attributes to set
 */
int
cbl_xad_setcolattrs (void *p0) {
	XAD_ATTRIBUTES = cob_get_int (COB_MODULE_PTR->cob_procedure_params[0]);
	return 0;
}


int
cbl_xad_getattrs (void *p0) {
	cob_set_int (COB_MODULE_PTR->cob_procedure_params[0], XAD_ATTRIBUTES & COB_XAD_ATTRS);
	return 0;
}


int
cbl_xad_setattrs (void *p0) {
	XAD_ATTRIBUTES = (XAD_ATTRIBUTES & ~COB_XAD_ATTRS) | (cob_get_int (COB_MODULE_PTR->cob_procedure_params[0]) & COB_XAD_ATTRS);
	return 0;
}


int
cbl_xad_setattr (void *p0, void *p1) {
	int		attr_num;
	int		attr_val;
	xad_attrs_t	attrs;

	attr_num = cob_get_int (COB_MODULE_PTR->cob_procedure_params[0]);
	attr_val = !!(cob_get_int (COB_MODULE_PTR->cob_procedure_params[1]));

	attrs = (XAD_ATTRIBUTES & COB_XAD_ATTRS) | (attr_val << (attr_num - 1));

	XAD_ATTRIBUTES = (XAD_ATTRIBUTES & ~COB_XAD_ATTRS) | attrs;
	return 0;
}


int
cbl_xad_setfgcol (void *p0) {
	int		color;

	color = cob_get_int (COB_MODULE_PTR->cob_procedure_params[0]) % 0xff;

	xad_setfgcol (color, NULL);
	return 0;
}


int
cbl_xad_setbgcol (void *p0) {
	int		color;

	color = cob_get_int (COB_MODULE_PTR->cob_procedure_params[0]) % 0xff;

	xad_setbgcol (color, NULL);

	return 0;
}


/*
 * Redraw the current screen content
 */
int
cbl_xad_redraw_scr (void *status_) {
	cob_field	*status = (cob_field *) COB_MODULE_PTR->cob_procedure_params[0];

	redrawwin (stdscr);

	/* TODO: status ... */
	return 0;
}


static int
xad_getfgcol (xad_attrs_t *xattrs) {
	xad_attrs_t	*p = xattrs;

	if (p == NULL) {
		p = &XAD_ATTRIBUTES;
	}

	return ((*p) & COB_XAD_FGCOL) >> 16;
}


static int
xad_setfgcol (int color, xad_attrs_t *xattrs) {
	xad_attrs_t	*p = xattrs;

	if (p == NULL) {
		p = &XAD_ATTRIBUTES;
	}

	*p = ((*p) & ~COB_XAD_FGCOL) | (color << 16);

	return 0;
}


static int
xad_getbgcol (xad_attrs_t *xattrs) {
	xad_attrs_t	*p = xattrs;

	if (p == NULL) {
		p = &XAD_ATTRIBUTES;
	}

	return ((*p) & COB_XAD_BGCOL) >> 24;
}


static int
xad_setbgcol (int color, xad_attrs_t *xattrs) {
	xad_attrs_t	*p = xattrs;

	if (p == NULL) {
		p = &XAD_ATTRIBUTES;
	}

	*p = ((*p) & ~COB_XAD_BGCOL) | (color << 24);

	return 0;
}


/*
 * Set cobol attributes and colors from CONTROL string
 *
 * ctrl:	CONTROL string
 * fgc:		foreground color to return
 * bgc:		background color to return
 * tmo:		timeout value to return
 * fattr:	field attributes to return
 */
static void
xad_set_ctrl_attributes (cob_field *ctrl, cob_field *fgc, cob_field *bgc, cob_field *tmo, cob_flags_t *fattr) {
	char	cs[1024];
	char	*p = NULL;
	char	*p1 = NULL;

	if (ctrl == NULL) {
		return;
	}

	if (ctrl->data == NULL) {
		return;
	}

	if (field_to_str (ctrl, cs, sizeof (cs) - 1) == NULL) {
		return;
	}

	for (p = cs; *p; p++) {
		*p = toupper (*p);
	}

	p = cs;
	while (*p) {
		cob_flags_t	c_attr;
		int	c_fgc, c_bgc, c_tmo, c_no;
		int	n, n0, n1;
		char	s[1024];

		c_attr = 0;
		c_fgc = c_bgc = c_tmo = -1;
		c_no = 0;
		n = n1 = 0;

//fprintf (stderr, "match_ctrl:<%s>\n", p);
		if (*p == ' ' || *p == '\t' || *p == ',' || *p == ';') {
			p++;
		} else if (sscanf (p, "%50s%n", &s, &n) >= 1) {
//fprintf (stderr, "match_word:<%s>\n", s);
			p += n;

			if (strcasecmp (s, "NO") == 0) {
				c_no = 1;
				if (sscanf (p, "%50s%n", &s, &n) < 1) {
					break;
				}

				p += n;
			}

			if (strcasecmp (s, "BLINK") == 0) {
				c_attr = COB_SCREEN_BLINK;
			} else if (strcasecmp (s, "HIGHLIGHT") == 0) {
				c_attr = COB_SCREEN_HIGHLIGHT;
			} else if (strcasecmp (s, "LOWLIGHT") == 0) {
				c_attr = COB_SCREEN_LOWLIGHT;
			} else if (strcasecmp (s, "REVERSE-VIDEO") == 0) {
				c_attr = COB_SCREEN_REVERSE;
			} else if (strcasecmp (s, "UNDERLINE") == 0) {
				c_attr = COB_SCREEN_UNDERLINE;
			} else if (
				strcasecmp (s, "BACKGROUND-COLOR") == 0 ||
				strcasecmp (s, "BACKGROUND-COLOUR") == 0
			) {
				n = 0;
				if (sscanf (p, " IS%n", &n) >= 0 && n > 0) {
//fprintf (stderr, "bgis:n:%d\n", n);
					p += n;
				}

				/* TODO: value by identifier */
				// n = n1 = 0;
				if (sscanf (p, " %d%n", &n1, &n) >= 1) {
					c_bgc = n1;
					p += n;
				}
			} else if (
				strcasecmp (s, "FOREGROUND-COLOR") == 0 ||
				strcasecmp (s, "FOREGROUND-COLOUR") == 0
			) {
				n = 0;
				if (sscanf (p, " IS%n", &n) >= 0 && n > 0) {
//fprintf (stderr, "is:%d\n", n);
					p += n;
				}

				/* TODO: value by identifier */
				// n = n1 = 0;
				if (sscanf (p, " %d%n", &n1, &n) >= 1) {
//fprintf (stderr, "n/n1:%d/%d\n", n, n1);
					c_fgc = n1;
					p += n;
				}
			} else if (strcasecmp (s, "TIMEOUT") == 0) {
				n = 0;
				if (sscanf (p, " IS%n", &n) >= 0 && n > 0) {
					p += n;
				}

				/* TODO: value by identifier */
				// n = n1 = 0;
				if (sscanf (p, " %d%n", &n1, &n) >= 1) {
//fprintf (stderr, "is:%d\n", n);
					c_tmo = n1;
					p += n;
				}
			}
		} else if (sscanf (p, "%*s%n", &n) == 1) {
			p += n;
		} else {
			break;
		}

//fprintf (stderr, "match_ctrl: attr:0x%x, fgc:%d, bgc:%d, tmo:%d\n", c_attr, c_fgc, c_bgc, c_tmo);
		if (c_attr != 0 && fattr != NULL) {
			*fattr |= c_attr;
		}

		if (c_fgc >= 0 && fgc != NULL) {
			cob_set_int (fgc, c_fgc);
		}

		if (c_bgc >= 0 && bgc != NULL) {
			cob_set_int (bgc, c_bgc);
		}

		if (c_tmo >= 0 && tmo != NULL) {
			cob_set_int (tmo, c_tmo);
		}
	}

//if (fgc != NULL) {
//	fprintf(stderr, "FGC:%d\n", cob_get_int (fgc));
//}
//if (bgc != NULL) {
//	fprintf(stderr, "BGC:%d\n", cob_get_int (bgc));
//}
}


void
cob_xad_accept (cob_field *f, cob_field *line, cob_field *column,
		  cob_field *fgc, cob_field *bgc, cob_field *fscroll,
		  cob_field *ftimeout, cob_field *prompt, cob_field *size_is,
		  const cob_flags_t fattr, cob_xad_mask *mask,
		  cob_field *ctrl)
{
	cob_screen	*s;
	int		sline;
	int		scolumn;
	cob_flags_t	sattr = fattr;
	cob_field	*sfgc = NULL;
	cob_field	*sbgc = NULL;
	cob_field	*stmo = NULL;

//fprintf (stderr, "cob_xad_accept__0\n");
//fprintf (stderr, "%s/%d (ctrl=%x)\n", __FILE__, __LINE__, ctrl);
	init_cob_screen_if_needed ();

	extract_line_and_col_vals (line, column, ACCEPT_STATEMENT, 1, &sline,
				   &scolumn);

	/*
	 * Create temporary fields to contain values that might be changed by
	 * xad_set_attributes et.al. to avoid side effects
	 */
	sfgc = xad_build_int_field (fgc ? cob_get_int (fgc) : -1);
	sbgc = xad_build_int_field (bgc ? cob_get_int (bgc) : -1);
	stmo = xad_build_int_field (ftimeout ? cob_get_int (ftimeout) : -1);

	xad_set_attributes (XAD_ATTRIBUTES, sfgc, sbgc, &sattr);

	if (ctrl != NULL) {
// fprintf (stderr, "(ctrl=<%s>)\n", ctrl->data);
		xad_set_ctrl_attributes (ctrl, sfgc, sbgc, stmo, &sattr);
	}

	s = xad_define_screen (mask, sfgc, sbgc, prompt, sattr);

	screen_accept (s, sline, scolumn, stmo);

	xad_destroy_screen (s);

	if (sfgc != NULL) {
		cob_field_free (sfgc);
	}

	if (sbgc != NULL) {
		cob_field_free (sbgc);
	}

	if (stmo != NULL) {
		cob_field_free (stmo);
	}
}


#if 0
void
cob_xad_accept__2 (cob_field **flds, const cob_flags_t fattr, cob_xad_mask *mask) {
	/*
	cob_field *f, cob_field *line, cob_field *column,
	cob_field *fgc, cob_field *bgc, cob_field *fscroll,
	cob_field *ftimeout, cob_field *prompt, cob_field *size_is,
	const cob_flags_t fattr, cob_xad_mask *mask,
	cob_field *ctrl
	*/

// fprintf (stderr, "cob_xad_accept__2\n");
	cob_xad_accept (
		flds + 0,	// f
		flds + 1,	// line
		flds + 2,	// column
		flds + 3,	// fgc
		flds + 4,	// bgc
		flds + 5,	// fscroll
		flds + 6,	// timeout
		flds + 7,	// prompt
		flds + 8,	// size_is
		fattr, mask,
		flds + 9	// ctrl
	);
}
#endif

void
cob_xad_display (cob_field *f, cob_field *line, cob_field *column,
		  cob_field *fgc, cob_field *bgc, cob_field *fscroll,
		  cob_field *size_is,
		  const cob_flags_t fattr, cob_xad_mask *mask,
		  cob_field *ctrl)
{
	cob_screen	*s;
	int		sline;
	int		scolumn;
	cob_flags_t	sattr = fattr;
	cob_field	*sfgc = NULL;
	cob_field	*sbgc = NULL;

	init_cob_screen_if_needed ();

	extract_line_and_col_vals (line, column, DISPLAY_STATEMENT, 1, &sline,
				   &scolumn);

	/*
	 * Create temporary fields to contain values that might be changed by
	 * xad_set_attributes et.al. to avoid side effects
	 */
	sfgc = xad_build_int_field (fgc ? cob_get_int (fgc) : -1);
	sbgc = xad_build_int_field (bgc ? cob_get_int (bgc) : -1);

	xad_set_attributes (XAD_ATTRIBUTES, sfgc, sbgc, &sattr);

	if (ctrl != NULL) {
//fprintf (stderr, "(ctrl=<%s>)\n", ctrl->data);
		xad_set_ctrl_attributes (ctrl, sfgc, sbgc, NULL, &sattr);
	}

	if (COB_FIELD_TYPE (f) == COB_TYPE_GROUP) {
		s = xad_define_screen (mask, sfgc, sbgc, NULL, sattr);

		screen_display (s, sline, scolumn);

		xad_destroy_screen (s);
	} else {
		field_display (f, sline, scolumn, sfgc, sbgc, fscroll, size_is, sattr);
	}

	if (sfgc != NULL) {
		cob_field_free (sfgc);
	}

	if (sbgc != NULL) {
		cob_field_free (sbgc);
	}
}


static xad_attrs_t
curses_to_xad_attrs (chtype ch) {
	chtype		curs_attrs;
	xad_attrs_t	xad_attrs = 0;
	short		pair;
	short		fgcol, bgcol;

	curs_attrs = ch & A_ATTRIBUTES;

	if (curs_attrs & A_REVERSE) {
		xad_attrs |= COB_XAD_ATTR_REVERSE;
	}

	if (curs_attrs & A_BOLD) {
		xad_attrs |= COB_XAD_ATTR_HIGHLIGHT;
	}

	if (curs_attrs & A_DIM) {
		xad_attrs |= COB_XAD_ATTR_LOWLIGHT;
	}

	if (curs_attrs & A_BLINK) {
		xad_attrs |= COB_XAD_ATTR_BLINK;
	}

	if (curs_attrs & A_UNDERLINE) {
		xad_attrs |= COB_XAD_ATTR_UNDERLINE;
	}
	
	if (curs_attrs & A_ALTCHARSET) {
		xad_attrs |= COB_XAD_ATTR_ALTCHARSET;
	}
	
	if ((pair = PAIR_NUMBER (ch)) != ERR) {
		if (pair_content (pair, &fgcol, &bgcol) != ERR) {
			xad_setfgcol (fgcol, &xad_attrs);
			xad_setbgcol (bgcol, &xad_attrs);
		}
	}

	return xad_attrs;
}


static chtype
xad_to_curses_attrs (xad_attrs_t xad_attrs) {
	chtype		curs_attrs;
	short		pair;
	short		fgcol, bgcol;

	curs_attrs = 0;

	if (xad_attrs & COB_XAD_ATTR_REVERSE) {
		curs_attrs |= A_REVERSE;
	}

	if (xad_attrs & COB_XAD_ATTR_HIGHLIGHT) {
		curs_attrs |= A_BOLD;
	}

	if (xad_attrs & COB_XAD_ATTR_LOWLIGHT) {
		curs_attrs |= A_DIM;
	}

	if (xad_attrs & COB_XAD_ATTR_BLINK) {
		curs_attrs |= A_BLINK;
	}

	if (xad_attrs & COB_XAD_ATTR_UNDERLINE) {
		curs_attrs |= A_UNDERLINE;
	}

	if (xad_attrs & COB_XAD_ATTR_ALTCHARSET) {
		curs_attrs |= A_ALTCHARSET;
	}

	fgcol = xad_getfgcol (&xad_attrs);
	bgcol = xad_getbgcol (&xad_attrs);

	pair = cob_get_color_pair (fgcol, bgcol);

	curs_attrs = (curs_attrs & ~A_COLOR) | COLOR_PAIR (pair);

	return curs_attrs;
}


static int
xad_to_cob_attrs (xad_attrs_t xad_attrs) {
	int		cob_attrs = 0;
	short		pair;
	short		fgcol, bgcol;

	if (xad_attrs & COB_XAD_ATTR_REVERSE) {
		cob_attrs |= COB_SCREEN_REVERSE;
	}

	if (xad_attrs & COB_XAD_ATTR_HIGHLIGHT) {
		cob_attrs |= COB_SCREEN_HIGHLIGHT;
	}

	if (xad_attrs & COB_XAD_ATTR_LOWLIGHT) {
		cob_attrs |= COB_SCREEN_LOWLIGHT;
	}

	if (xad_attrs & COB_XAD_ATTR_BLINK) {
		cob_attrs |= COB_SCREEN_BLINK;
	}

	if (xad_attrs & COB_XAD_ATTR_UNDERLINE) {
		cob_attrs |= COB_SCREEN_UNDERLINE;
	}

	if (xad_attrs & COB_XAD_ATTR_ALTCHARSET) {
		cob_attrs |= COB_SCREEN_ALTCHARSET;
	}

	return cob_attrs;
}


static unsigned char
xad_to_cbl_attrs (xad_attrs_t xad_attrs) {
	unsigned char	cbl_attrs = '\0';
	short		pair;
	short		fgcol, bgcol;
	int		i;

	for (i = 0; i < 8; i++) {
		if (xad_attrs & XAD_CBLATTRS[i]) {
			cbl_attrs |= (1U << i);
		}
	}

	return cbl_attrs;
}


static xad_attrs_t
cbl_to_xad_attrs (unsigned char cbl_attrs) {
	xad_attrs_t	xad_attrs = 0;
	int		i;

	for (i = 0; i < 8; i++) {
		if (cbl_attrs & (1U << i)) {
			xad_attrs |= XAD_CBLATTRS[i];
		}
	}

	return xad_attrs;
}


static unsigned char
curses_to_cbl_attrs (chtype ch) {
	return xad_to_cbl_attrs (curses_to_xad_attrs (ch));
}


static chtype
cbl_to_curses_attrs (unsigned char cbl_attrs) {
	return xad_to_curses_attrs (cbl_to_xad_attrs (cbl_attrs));
}


static int
cob_read_scr_chattrs (cob_field *pos, cob_field *chars, cob_field *attrs, int length, cob_field *status) {
	int		start, line, column, i;
	int		columns = 80; // FIXME: geometry
	chtype		ch;

	line = column = start = 0;

	if (pos->size == 2) {
		line = (int) pos->data[0];
		column = (int) pos->data[1];
	} else {
		i = cob_get_int (pos);
		line = (i / 100);
		column = i - line * 100;
	}

	start = line * columns + column;

	for (i = 0; i < length; i++) {
		line = (start + i) / columns;
		column = (start + i) % columns;

		ch = mvinch (line, column);

		if (chars != NULL) {
			if (i < chars->size) {
				*(chars->data + i) = (ch & A_CHARTEXT);
			}
		}

		if (attrs != NULL) {
			if (i < attrs->size) {
				*(attrs->data + i) = curses_to_cbl_attrs(ch);
			}
		}
	}

	if (0 && status != NULL) { // FIXME
		if (cob_field_is_numeric_or_numeric_edited (status)) { // CHECKME
			cob_set_int (status, 0);
		}
	}

	return 0;
}


int
cbl_read_scr_chattrs (void *position_, void *chars_, void *attrs_, void *length_, void *status_) {
	cob_field	*pos = (cob_field *) COB_MODULE_PTR->cob_procedure_params[0];
	cob_field	*chars = (cob_field *) COB_MODULE_PTR->cob_procedure_params[1];
	cob_field	*attrs = (cob_field *) COB_MODULE_PTR->cob_procedure_params[2];
	int		length = cob_get_int (COB_MODULE_PTR->cob_procedure_params[3]);
	cob_field	*status = (cob_field *) COB_MODULE_PTR->cob_procedure_params[4];

	return cob_read_scr_chattrs (pos, chars, attrs, length, status);
}


int
cbl_read_scr_attrs (void *position_, void *attrs_, void *length_, void *status_) {
	cob_field	*pos = (cob_field *) COB_MODULE_PTR->cob_procedure_params[0];
	cob_field	*attrs = (cob_field *) COB_MODULE_PTR->cob_procedure_params[1];
	int		length = cob_get_int (COB_MODULE_PTR->cob_procedure_params[2]);
	cob_field	*status = (cob_field *) COB_MODULE_PTR->cob_procedure_params[3];

	return cob_read_scr_chattrs (pos, NULL, attrs, length, status);
}


int
cbl_read_scr_chars (void *position_, void *chars_, void *length_, void *status_) {
	cob_field	*pos = (cob_field *) COB_MODULE_PTR->cob_procedure_params[0];
	cob_field	*chars = (cob_field *) COB_MODULE_PTR->cob_procedure_params[1];
	int		length = cob_get_int (COB_MODULE_PTR->cob_procedure_params[2]);
	cob_field	*status = (cob_field *) COB_MODULE_PTR->cob_procedure_params[3];

	return cob_read_scr_chattrs (pos, chars, NULL, length, status);
}


static int
cob_write_scr_chattrs (cob_field *pos, cob_field *chars, cob_field *attrs, int length, cob_field *status, int repeat_first) {
	int		start, line, column, i;
	int		columns = -1; // 80; // FIXME: geometry
	int		lines = -1; // 24; // FIXME: geometry
	chtype		ch;
	unsigned char	ca;

	get_screen_geometry (&lines, &columns);

	line = column = start = 0;

	if (pos->size == 2) {
		line = (int) pos->data[0];
		column = (int) pos->data[1];
	} else {
		i = cob_get_int (pos);
		line = (i / 100);
		column = i - line * 100;
	}

	start = line * columns + column;

	for (i = 0; i < length && i + start < lines * columns; i++) {
		line = (start + i) / columns;
		column = (start + i) % columns;

		ch = mvinch (line, column);

		if (chars != NULL && chars->data != NULL && chars->size > 0) {
			if (i < chars->size || repeat_first) {
				if (repeat_first) {
					ca = *(chars->data);
				} else {
					ca = *(chars->data + i);
				}

				ch = (ch & ~A_CHARTEXT) | (ca & A_CHARTEXT);
			}
		}

		if (attrs != NULL && attrs->data != NULL && attrs->size > 0) {
			if (i < attrs->size || repeat_first) {
				if (repeat_first) {
					ca = *(attrs->data);
				} else {
					ca = *(attrs->data + i);
				}

				ch = (ch & ~A_ATTRIBUTES) | cbl_to_curses_attrs(ca);
			}
		}

		mvaddch (line, column, ch);
	}

	refresh ();

	if (0 && status != NULL) { // FIXME
		if (cob_field_is_numeric_or_numeric_edited (status)) { // CHECKME
			cob_set_int (status, 0);
		}
	}

	return 0;
}


int
cbl_write_scr_chattrs (void *position_, void *chars_, void *attrs_, void *length_, void *status_) {
	cob_field	*pos = (cob_field *) COB_MODULE_PTR->cob_procedure_params[0];
	cob_field	*chars = (cob_field *) COB_MODULE_PTR->cob_procedure_params[1];
	cob_field	*attrs = (cob_field *) COB_MODULE_PTR->cob_procedure_params[2];
	int		length = cob_get_int (COB_MODULE_PTR->cob_procedure_params[3]);
	cob_field	*status = (cob_field *) COB_MODULE_PTR->cob_procedure_params[4];

	return cob_write_scr_chattrs (pos, chars, attrs, length, status, 0);
}


int
cbl_write_scr_attrs (void *position_, void *attrs_, void *length_, void *status_) {
	cob_field	*pos = (cob_field *) COB_MODULE_PTR->cob_procedure_params[0];
	cob_field	*attrs = (cob_field *) COB_MODULE_PTR->cob_procedure_params[1];
	int		length = cob_get_int (COB_MODULE_PTR->cob_procedure_params[2]);
	cob_field	*status = (cob_field *) COB_MODULE_PTR->cob_procedure_params[3];

	return cob_write_scr_chattrs (pos, NULL, attrs, length, status, 0);
}


int
cbl_write_scr_chars (void *position_, void *chars_, void *length_, void *status_) {
	cob_field	*pos = (cob_field *) COB_MODULE_PTR->cob_procedure_params[0];
	cob_field	*chars = (cob_field *) COB_MODULE_PTR->cob_procedure_params[1];
	int		length = cob_get_int (COB_MODULE_PTR->cob_procedure_params[2]);
	cob_field	*status = (cob_field *) COB_MODULE_PTR->cob_procedure_params[3];

	return cob_write_scr_chattrs (pos, chars, NULL, length, status, 0);
}


int
cbl_write_scr_chars_attr (void *position_, void *chars_, void *length_, void *attr_, void *status_) {
	cob_field	*pos = (cob_field *) COB_MODULE_PTR->cob_procedure_params[0];
	cob_field	*chars = (cob_field *) COB_MODULE_PTR->cob_procedure_params[1];
	int		length = cob_get_int (COB_MODULE_PTR->cob_procedure_params[2]);
	cob_field	*attr = (cob_field *) COB_MODULE_PTR->cob_procedure_params[3];
	cob_field	*status = (cob_field *) COB_MODULE_PTR->cob_procedure_params[4];

	return cob_write_scr_chattrs (pos, chars, attr, length, status, 1);
}


int
cbl_write_scr_n_attr (void *position_, void *attr_, void *length_, void *status_) {
	cob_field	*pos = (cob_field *) COB_MODULE_PTR->cob_procedure_params[0];
	cob_field	*attr = (cob_field *) COB_MODULE_PTR->cob_procedure_params[1];
	int		length = cob_get_int (COB_MODULE_PTR->cob_procedure_params[2]);
	cob_field	*status = (cob_field *) COB_MODULE_PTR->cob_procedure_params[3];

	return cob_write_scr_chattrs (pos, NULL, attr, length, status, 1);
}


int
cbl_write_scr_n_char (void *position_, void *char_, void *length_, void *status_) {
	cob_field	*pos = (cob_field *) COB_MODULE_PTR->cob_procedure_params[0];
	cob_field	*chr = (cob_field *) COB_MODULE_PTR->cob_procedure_params[1];
	int		length = cob_get_int (COB_MODULE_PTR->cob_procedure_params[2]);
	cob_field	*status = (cob_field *) COB_MODULE_PTR->cob_procedure_params[3];

	return cob_write_scr_chattrs (pos, chr, NULL, length, status, 1);
}


int
cbl_write_scr_n_chattr (void *position_, void *char_, void *attr_, void *length_, void *status_) {
	cob_field	*pos = (cob_field *) COB_MODULE_PTR->cob_procedure_params[0];
	cob_field	*chr = (cob_field *) COB_MODULE_PTR->cob_procedure_params[1];
	cob_field	*attr = (cob_field *) COB_MODULE_PTR->cob_procedure_params[2];
	int		length = cob_get_int (COB_MODULE_PTR->cob_procedure_params[3]);
	cob_field	*status = (cob_field *) COB_MODULE_PTR->cob_procedure_params[4];

	return cob_write_scr_chattrs (pos, chr, attr, length, status, 1);
}


int
cbl_swap_scr_chattrs (void *position_, void *chars_, void *attrs_, void *length_, void *status_) {
	cob_field	*pos = (cob_field *) COB_MODULE_PTR->cob_procedure_params[0];
	cob_field	*chars = (cob_field *) COB_MODULE_PTR->cob_procedure_params[1];
	cob_field	*attrs = (cob_field *) COB_MODULE_PTR->cob_procedure_params[2];
	int		length = cob_get_int (COB_MODULE_PTR->cob_procedure_params[3]);
	cob_field	*status = (cob_field *) COB_MODULE_PTR->cob_procedure_params[4];
	cob_field	*fchars = NULL;
	cob_field	*fattrs = NULL;

	if (chars != NULL) {
		fchars = cob_field_clone (chars);
	}

	if (attrs != NULL) {
		fattrs = cob_field_clone (attrs);
	}

	cob_read_scr_chattrs (pos, chars, attrs, length, status);

	cob_write_scr_chattrs (pos, fchars, fattrs, length, status, 0);

	if (fattrs != NULL) {
		cob_free (fattrs);
	}

	if (fchars != NULL) {
		cob_free (fchars);
	}

	return 0;
}


int
cbl_xad_read_scr (void *start_, void *length_, void *chars_, void *attrs_) {
	int		start = cob_get_int (COB_MODULE_PTR->cob_procedure_params[0]);
	int		length = cob_get_int (COB_MODULE_PTR->cob_procedure_params[1]);
	cob_field	*chars = (cob_field *) COB_MODULE_PTR->cob_procedure_params[2];
	cob_field	*attrs = (cob_field *) COB_MODULE_PTR->cob_procedure_params[3];
	int		line, column, todo, i;
	chtype		ch;
	xad_attrs_t	xattr;

	for (i = 0; i < length; i++) {
		line = (start + i) / 80; // FIXME: geometry
		column = (start + i) % 80; // FIXME: geometry

		if ((ch = mvinch (line, column)) != ERR) {
			if (chars != NULL) {
				if (i < chars->size) {
					*(chars->data + i) = (ch & A_CHARTEXT);
				}
			}

			if (attrs != NULL) {
				xattr = 0;
				xattr |= (ch & A_REVERSE) ? COB_SCREEN_REVERSE : 0;
				xattr |= (ch & A_BOLD) ? COB_SCREEN_HIGHLIGHT : 0;
				xattr |= (ch & A_DIM) ? COB_SCREEN_LOWLIGHT : 0;
				xattr |= (ch & A_BLINK) ? COB_SCREEN_BLINK : 0;
				xattr |= (ch & A_UNDERLINE) ? COB_SCREEN_UNDERLINE : 0;
				xattr |= (ch & A_ALTCHARSET) ? COB_SCREEN_ALTCHARSET : 0;

				if ((i + 1) * sizeof (xattr) <= attrs->size) {
					memcpy (attrs->data + (i * sizeof (xattr)), &xattr, sizeof(xattr));
				}
			}
		}
	}

	return 0;
}


int
cbl_xad_setattrbit (void *bitnr_, void *attr_) {
	int		bitnr = cob_get_int (COB_MODULE_PTR->cob_procedure_params[0]);
	int		attr = cob_get_int (COB_MODULE_PTR->cob_procedure_params[1]);

	if (bitnr < 0 || bitnr > 7) {
		return -1;
	}

	XAD_CBLATTRS[bitnr] = (attr & COB_XAD_ATTRS);

	return 0;
}


int
cbl_xad_setattrbyte (void *abyte_) {
	cob_field	*abyte = (cob_field *) COB_MODULE_PTR->cob_procedure_params[0];
	xad_attrs_t	xattrs = XAD_ATTRIBUTES;

	if (COB_FIELD_IS_NUMERIC (abyte)) {
		xattrs = cbl_to_xad_attrs (cob_get_int (abyte));
	} else {
		xattrs = cbl_to_xad_attrs ((unsigned char) abyte->data[0]);
	}

	XAD_ATTRIBUTES = (XAD_ATTRIBUTES & ~COB_XAD_ATTRS) | (xattrs & COB_XAD_ATTRS);

	attrset (cbl_to_curses_attrs (XAD_ATTRIBUTES));

	return 0;
}


int
cbl_xad_getattrbyte (void *abyte_) {
	cob_field	*abyte = (cob_field *) COB_MODULE_PTR->cob_procedure_params[0];

	if (COB_FIELD_IS_NUMERIC (abyte)) {
		cob_set_int (abyte, xad_to_cbl_attrs (XAD_ATTRIBUTES));
	} else {
		abyte->data[0] = xad_to_cbl_attrs (XAD_ATTRIBUTES);
	}

	return 0;
}


/*
 * Define the meaning of the bits of the attribute byte
 *
 * p0:	String of up to 8 characters, each defining one bit of the
 * 	attribute byte in increasing order (b0, b1, ... b7).
 *
 * 	H:	highlight attribute
 * 	U:	underline attribute
 * 	R:	reverse attribute
 * 	B:	blink attribute
 * 	L:	lowlight attribute
 * 	A:	alternate character set
 * 	N:	"no display" attribute
 * 	0:	remove meaning
 * 	C:	colorpair number in upto 4 remaining bits (TODO, not implemented)
 *
 * 	Other character are ignored and the respective bit remains unchanged.
 * 	The default definition for the attribute byte is "HURBLAN0"
 */
int
cbl_xad_setattrbits (void *p0_) {
	cob_field	*p0 = (cob_field *) COB_MODULE_PTR->cob_procedure_params[0];

	if (COB_FIELD_IS_ALNUM (p0) && p0->size > 0) {
		int	i;
		for (i = 0; i < 8 && i < p0->size; i++) {
			switch (*(p0->data + i)) {
			case 'H': XAD_CBLATTRS[i] = COB_XAD_ATTR_HIGHLIGHT; break;
			case 'U': XAD_CBLATTRS[i] = COB_XAD_ATTR_UNDERLINE; break;
			case 'R': XAD_CBLATTRS[i] = COB_XAD_ATTR_REVERSE; break;
			case 'B': XAD_CBLATTRS[i] = COB_XAD_ATTR_BLINK; break;
			case 'L': XAD_CBLATTRS[i] = COB_XAD_ATTR_LOWLIGHT; break;
			case 'A': XAD_CBLATTRS[i] = COB_XAD_ATTR_ALTCHARSET; break;
			case 'N': XAD_CBLATTRS[i] = COB_XAD_ATTR_NODISPLAY; break;
			case '0': XAD_CBLATTRS[i] = 0; break;
			case 'C':
#if 0 /* TODO ... */
				/* TODO: Encode color pair number into up to 4 remaining bits of attribute byte */
				for (j = i; j < 4; j++) {
					if (j < 8) {
						XAD_CBLATTRS[j] = COB_XAD_ATTR_COLORPAIR + j;
					}
				}
#endif
				i += 4;
				break;
			}
		}
	}

	return 0;
}


/*
 * Return current definition of the bits of the attribute byte
 *
 * p0:	alphanumeric item to return the current definition according to 'cbl_setattrbits' in
 */

int
cbl_xad_getattrbits (void *p0_) {
	cob_field	*p0 = (cob_field *) COB_MODULE_PTR->cob_procedure_params[0];

	if (COB_FIELD_IS_ALNUM (p0) && p0->size > 0) {
		int	i;
		char	c;

		memset (p0->data, ' ', p0->size);

		for (i = 0; i < 8 && i < p0->size; i++) {
			*(p0->data + i) = ' ';

			switch (XAD_CBLATTRS[i]) {
			case COB_XAD_ATTR_HIGHLIGHT:	*(p0->data + i) = 'H'; break;
			case COB_XAD_ATTR_UNDERLINE:	*(p0->data + i) = 'U'; break;
			case COB_XAD_ATTR_REVERSE:	*(p0->data + i) = 'R'; break;
			case COB_XAD_ATTR_BLINK:	*(p0->data + i) = 'B'; break;
			case COB_XAD_ATTR_LOWLIGHT:	*(p0->data + i) = 'L'; break;
			case COB_XAD_ATTR_ALTCHARSET:	*(p0->data + i) = 'A'; break;
			case COB_XAD_ATTR_NODISPLAY:	*(p0->data + i) = 'N'; break;
			case 0:				*(p0->data + i) = '0'; break;
#if 0 /* TODO ... */
			case COB_XAD_ATTR_COLORPAIR_0:
			case COB_XAD_ATTR_COLORPAIR_1:
			case COB_XAD_ATTR_COLORPAIR_2:
			case COB_XAD_ATTR_COLORPAIR_3:
				break;
#endif
			}
		}
	}

	return 0;
}


/* Keyboard routines */

int
cbl_read_kbd_char (void *rchar_, void *status_) {
	cob_field	*rchar = (cob_field *) COB_MODULE_PTR->cob_procedure_params[0];
	cob_field	*status = (cob_field *) COB_MODULE_PTR->cob_procedure_params[1];
	chtype		ch;

	if (rchar->size < 1) {
		return 0;
	}

	if ((ch = getch()) != ERR) {
		rchar->data[0] = ch & A_CHARTEXT;
	}

	/* TODO: status, etc. */

	return 0;
}


/*
 * Disable XAD keyboard mappings
 */
int
cbl_xad_reset_keymap (void *status_) {
	cob_field	*status = (cob_field *) COB_MODULE_PTR->cob_procedure_params[0];

	xad_reset_keymap ();

	/* TODO: status, etc. */

	return 0;
}


/*
 * Load default XAD keyboard mappings
 */
int
cbl_xad_default_keymap (void *status_) {
	cob_field	*status = (cob_field *) COB_MODULE_PTR->cob_procedure_params[0];

	xad_init_default_keymap ();

	/* TODO: status, etc. */

	return 0;
}


/*
 * Copy a field to a null-terminated string
 * (essentially taken from intrinsic.c: copy_data_to_null_terminated_str())
 */
static char
*field_to_str (cob_field *f, char *out_str, size_t max_len) {
	size_t	i;
	size_t	m = -1;

	for (i = 0; i < f->size; i++) {
		if (!isspace ((int) *(f->data + i))) {
			m = i;
		}
	}

	m += 1;

	if (m > max_len) {
		m = max_len;
	}

	strncpy (out_str, (char *)f->data, m);
	out_str[m] = '\0';

	return out_str;
}


int
cbl_xad_read_keymapfile (void *filename_, void *status_) {
	cob_field	*filename = (cob_field *) COB_MODULE_PTR->cob_procedure_params[0];
	cob_field	*status = (cob_field *) COB_MODULE_PTR->cob_procedure_params[1];
	char		path[1024];

	if (field_to_str (filename, path, sizeof (path) - 1) == NULL) {
		return -1;
	}

	xad_read_keymap_file (path);

	/* TODO: status, etc. */

	return 0;
}


int
cbl_xad_add_keymapping (void *pkey_, void *pval_) {
	cob_field	*f_pkey = (cob_field *) COB_MODULE_PTR->cob_procedure_params[0];
	cob_field	*f_pval = (cob_field *) COB_MODULE_PTR->cob_procedure_params[1];
	cob_field	*status = (cob_field *) COB_MODULE_PTR->cob_procedure_params[2];
	char		pkey[1024];
	char		pval[1024];

	if (field_to_str (f_pkey, pkey, sizeof (pkey) - 1) == NULL) {
		return -1;
	}

	if (field_to_str (f_pval, pval, sizeof (pval) - 1) == NULL) {
		return -1;
	}

	xad_add_keymap (pkey, pval);

	/* TODO: status, etc. */

	return 0;
}


#endif

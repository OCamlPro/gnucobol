/*
   Copyright (C) 2001-2012, 2014-2020 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Simon Sobisch

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

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>

#include "cobc.h"
#include "tree.h"

static char		*errnamebuff = NULL;
static struct cb_label	*last_section = NULL;
static struct cb_label	*last_paragraph = NULL;
static char		last_file[48] = "";

static int conf_error_displayed = 0;
static int last_error_line = 0;
static const char	*last_error_file = "unknown";	/* no gettext for static initializer */
static FILE			*sav_lst_file = NULL;
static int		ignore_error = 0;

size_t				cb_msg_style;

DECLNORET static void		cobc_too_many_errors (void) COB_A_NORETURN;

static void
print_error_prefix (const char *file, int line, const char *prefix)
{
	if (file) {
		if (line > 0) {
			if (cb_msg_style == CB_MSG_STYLE_MSC) {
				fprintf (stderr, "%s(%d): ", file, line);
			} else {
				fprintf (stderr, "%s:%d: ", file, line);
			}
		} else {
			fprintf (stderr, "%s: ", file);
		}
	}
	if (prefix) {
		fprintf (stderr, "%s", prefix);
	}
}

static void
print_error (const char *file, int line, const char *prefix,
	     const char *fmt, va_list ap, const char *diagnostic_option)
{
	static char errmsg[COB_SMALL_BUFF];

	if (!file) {
		file = cb_source_file;
	}
	if (!line) {
		line = cb_source_line;
	}

	if (file != NULL
	 && strcmp(file,last_file) != 0) {
		snprintf(last_file,sizeof(last_file)-1,"%s",file);
		last_section = NULL;
		last_paragraph = NULL;
	}

	/* Print section and/or paragraph name */
	if (current_section != last_section) {
		if (current_section && !current_section->flag_dummy_section) {
			if (file) {
				fprintf (stderr, "%s: ", file);
			}
			fprintf (stderr, _("in section '%s':"),
				(char *)current_section->name);
			fputs ("\n", stderr);
		}
		last_section = current_section;
		last_paragraph = NULL;
	}
	if (current_paragraph != last_paragraph) {
		if (current_paragraph && !current_paragraph->flag_dummy_paragraph) {
			if (file) {
				fprintf (stderr, "%s: ", file);
			}
			fprintf (stderr, _("in paragraph '%s':"),
				(char *)current_paragraph->name);
			fputs ("\n", stderr);
		}
		last_paragraph = current_paragraph;
	}

	/* Print the error */
	print_error_prefix (file, line, prefix);
	vsnprintf (errmsg, COB_SMALL_MAX, fmt, ap);
	if (diagnostic_option) {
		fprintf (stderr, "%s [%s]\n", errmsg, diagnostic_option);
	} else {
		fprintf (stderr, "%s\n", errmsg);
	}

	if (cb_src_list_file) {
		cb_add_error_to_listing (file, line, prefix, errmsg);
	}
}

static void
cobc_too_many_errors (void)
{
	if (cb_diagnostic_show_option) {
		char diagnostic_option[COB_MINI_BUFF];
		if (cb_max_errors == -1) {
			snprintf (diagnostic_option, COB_MINI_MAX,
				"-Wfatal-errors");
		} else {
			snprintf (diagnostic_option, COB_MINI_MAX,
				"-fmax-errors=%d", cb_max_errors);
		}
		fprintf (stderr, "cobc: %s [%s]\n", _("too many errors"), diagnostic_option);
	} else {
		fprintf (stderr, "cobc: %s\n", _("too many errors"));
	}
	cobc_abort_terminate (0);
}


void
cb_inclusion_note (const char *file, int line)
{
	/* Print the inclusion error */
	fprintf (stderr, _("in file included from "));
	if (line > 0) {
		if (cb_msg_style == CB_MSG_STYLE_MSC) {
			fprintf (stderr, "%s(%d):\n", file, line);
		} else {
			fprintf (stderr, "%s:%d:\n", file, line);
		}
	} else {
		fprintf (stderr, "%s:\n", file);
	}
}

static void
configuration_error_head (void)
{
	if (conf_error_displayed) {
		return;
	}
	conf_error_displayed = 1;
	fputs (_("configuration error:"), stderr);
	putc ('\n', stderr);
}

/* reentrant version of strerror */
char *
cb_get_strerror (void)
{
#ifdef HAVE_STRERROR
	return (char *)cobc_main_strdup (strerror (errno));
#else
	char * msg;
	msg = cobc_main_malloc (COB_SMALL_BUFF);
	snprintf (msg, COB_SMALL_MAX, _("system error %d"), errno);
	return msg;
#endif
}

/* set the value for "ignore errors because instruction is
   in a constant FALSE path which gets no codegen at all"
   if state is -1, don't set the value 

   returns the value which was active on call
*/
int
cb_set_ignore_error (int state)
{
	int prev = ignore_error;
	if (state != -1) {
		ignore_error = state;
	}
	return prev;
}

void 
cb_add_error_to_listing (const char *file, int line,
		const char *prefix, char *errmsg)
{
	/* If we have a file, queue message for listing processing */
	if (cb_current_file) {
		struct list_error	*err;
		struct list_files	*cfile;

		/* set up listing error */
		err = cobc_malloc (sizeof (struct list_error));
		err->line = line;
		if (file) {
			err->file = cobc_strdup (file);
		} else {
			err->file = NULL;
		}
		if (prefix) {
			err->prefix = cobc_strdup (prefix);
		} else {
			err->prefix = NULL;
		}
		err->msg = cobc_strdup (errmsg);

		/* set correct listing entry for this file */
		cfile = cb_current_file;
		if (!cfile->name
		 || (file && strcmp (cfile->name, file))) {
			cfile = cfile->copy_head;
			while (cfile) {
				if (file && cfile->name
				 && !strcmp (cfile->name, file)) {
					break;
				}
				cfile = cfile->next;
			}
		}
		/* if file doesn't exist in the list then add to current file */
		if (!cfile) {
			cfile = cb_current_file;
		}
		/* add error to listing entry - in order */
#if 0	/* note: we don't need to check 'file' here because of cfile's scope */
		if (!err->file) {
			struct list_error	*old_err;
			for (old_err = cfile->err_head; old_err; old_err = old_err->next) {
				if (old_err->file) {
					continue;
				}
				if (old_err->line > err->line) {
					break;
				}
				err->prev = old_err;
			}
		} else {
			struct list_error* old_err;
			for (old_err = cfile->err_head; old_err; old_err = old_err->next) {
				if (!old_err
				 || strcmp (old_err->file, err->file)) {
					continue;
				}
				if (old_err->line > err->line) {
					break;
				}
				err->prev = old_err;
			}
		}
#else
		{
			struct list_error* old_err;
			for (old_err = cfile->err_head; old_err; old_err = old_err->next) {
				if (old_err->line > err->line) {
					break;
				}
				err->prev = old_err;
			}
		}
#endif

		if (!err->prev) {
			err->next = cfile->err_head;
			cfile->err_head = err;
		} else {
			err->next = err->prev->next;
			err->prev->next = err;
		}

		/* Otherwise, just write error to the listing file */
	} else {
		if (file) {
			if (line > 0) {
				if (cb_msg_style == CB_MSG_STYLE_MSC) {
					fprintf (stderr, "%s(%d): ", file, line);
				} else {
					fprintf (stderr, "%s:%d: ", file, line);
				}
			} else {
				fprintf (cb_src_list_file, "%s: ", file);
			}
		}
		if (prefix) {
			fprintf (cb_src_list_file, "%s ", prefix);
		}
		fprintf (cb_src_list_file, "%s\n", errmsg);
	}
}

static char warning_option_buff[COB_MINI_BUFF];
static char *warning_option_text (const enum cb_warn_opt opt, const enum cb_warn_val pref)
{
	const char *opt_name;

	if (!cb_diagnostic_show_option) {
		return NULL;
	}
	switch (opt) {
#define	CB_WARNDEF(opt_val,name,doc)	case opt_val: opt_name = name; break;
#define	CB_ONWARNDEF(opt_val,name,doc)	case opt_val: opt_name = name; break;
#define	CB_NOWARNDEF(opt_val,name,doc)	case opt_val: opt_name = name; break;
#define	CB_ERRWARNDEF(opt_val,name,doc)	case opt_val: opt_name = name; break;
#include "warning.def"
#undef	CB_WARNDEF
#undef	CB_ONWARNDEF
#undef	CB_NOWARNDEF
#undef	CB_ERRWARNDEF
		/* LCOV_EXCL_START */
	case COB_WARNOPT_NONE:
	case COB_WARNOPT_MAX:
	default:
		/* This should never happen (and therefore doesn't get a translation) */
		cobc_err_msg ("unexpected warning option value: %d", opt);
		COBC_ABORT ();
		/* LCOV_EXCL_STOP */
	};
	sprintf (warning_option_buff, "%s%s",
		pref != COBC_WARN_AS_ERROR ? "-W" : "-Werror=", opt_name);
	return warning_option_buff;
}

void
cb_warning (const enum cb_warn_opt opt, const char *fmt, ...)
{
	va_list ap;
	const enum cb_warn_val pref = cb_warn_opt_val[opt];

	if (pref == COBC_WARN_DISABLED) {
		return;
	}

	va_start (ap, fmt);
	if (pref != COBC_WARN_AS_ERROR) {
		print_error (NULL, 0, _("warning: "),  fmt, ap, warning_option_text (opt, pref));
	} else {
		print_error (NULL, 0, _("error: "),  fmt, ap, warning_option_text (opt, pref));
	}
	va_end (ap);

	if (sav_lst_file) {
		return;
	}
	if (pref == COBC_WARN_AS_ERROR) {
		if (++errorcount > cb_max_errors) {
			cobc_too_many_errors ();
		}
	} else {
		warningcount++;
	}
}

void
cb_error_always (const char *fmt, ...)
{
	va_list ap;

	cobc_in_repository = 0;
	va_start (ap, fmt);
	print_error (NULL, 0, _("error: "), fmt, ap, NULL);
	va_end (ap);

	if (sav_lst_file) {
		return;
	}
	if (++errorcount > cb_max_errors) {
		cobc_too_many_errors ();
	}
}

/* raise error (or warning if current branch is not generated) */
void
cb_error (const char *fmt, ...)
{
	va_list ap;
	const enum cb_warn_opt	opt = cb_warn_ignored_error;
	const enum cb_warn_val	pref = cb_warn_opt_val[opt];

	cobc_in_repository = 0;

	if (ignore_error && pref == COBC_WARN_DISABLED) {
		return;
	}

	va_start (ap, fmt);
	if (!ignore_error) {
		print_error (NULL, 0, _("error: "), fmt, ap, NULL);
	} else if (pref == COBC_WARN_AS_ERROR) {
		print_error (NULL, 0, _("error: "), fmt, ap, warning_option_text (opt, pref));
	} else {
		print_error (NULL, 0, _("warning: "), fmt, ap, warning_option_text (opt, pref));
	}
	va_end (ap);

	if (sav_lst_file) {
		return;
	}
	if (ignore_error && pref != COBC_WARN_AS_ERROR) {
		warningcount++;
	} else {
		if (++errorcount > cb_max_errors) {
			cobc_too_many_errors ();
		}
	}
}

void
cb_perror (const int config_error, const char *fmt, ...)
{
	va_list ap;

	if (config_error) {
		configuration_error_head();
	}

	va_start (ap, fmt);
	print_error (NULL, 0, "", fmt, ap, NULL);
	va_end (ap);


	if (++errorcount > cb_max_errors) {
		cobc_too_many_errors ();
	}
}

/* Warning/error for pplex.l input routine */
/* At this stage we have not parsed the newline so that */
/* cb_source_line needs to be adjusted by newline_count in pplex.l */

void
cb_plex_warning (const enum cb_warn_opt opt, const size_t sline, const char *fmt, ...)
{
	va_list ap;
	const enum cb_warn_val pref = cb_warn_opt_val[opt];

	if (pref == COBC_WARN_DISABLED) {
		return;
	}

	va_start (ap, fmt);
	if (pref != COBC_WARN_AS_ERROR) {
		print_error (NULL, cb_source_line + (int)sline, _("warning: "), fmt, ap, warning_option_text (opt, pref));
	} else {
		print_error (NULL, cb_source_line + (int)sline, _("error: "), fmt, ap, warning_option_text (opt, pref));
	}
	va_end (ap);

	if (sav_lst_file) {
		return;
	}
	if (pref == COBC_WARN_AS_ERROR) {
		if (++errorcount > cb_max_errors) {
			cobc_too_many_errors ();
		}
	} else {
		warningcount++;
	}
}

void
cb_plex_error (const size_t sline, const char *fmt, ...)
{
	va_list ap;

	va_start (ap, fmt);
	print_error (NULL, cb_source_line + (int)sline, ("error: "), fmt, ap, NULL);
	va_end (ap);

	if (sav_lst_file) {
		return;
	}
	if (++errorcount > cb_max_errors) {
		cobc_too_many_errors ();
	}
}

unsigned int
cb_plex_verify (const size_t sline, const enum cb_support tag,
		const char *feature)
{
	switch (tag) {
	case CB_OK:
		return 1;
	case CB_WARNING:
		cb_plex_warning (cb_warn_dialect, sline, _("%s used"), feature);
		return 1;
	case CB_ARCHAIC:
		cb_plex_warning (cb_warn_archaic, sline, _("%s is archaic in %s"),
			feature, cb_config_name);
		return 1;
	case CB_OBSOLETE:
		cb_plex_warning (cb_warn_obsolete, sline, _("%s is obsolete in %s"),
			feature, cb_config_name);
		return 1;
	case CB_SKIP:
		return 0;
	case CB_IGNORE:
		cb_plex_warning (cb_warn_additional, sline, _("%s ignored"), feature);
		return 0;
	case CB_ERROR:
		cb_plex_error (sline, _("%s used"), feature);
		return 0;
	case CB_UNCONFORMABLE:
		cb_plex_error (sline, _("%s does not conform to %s"),
			feature, cb_config_name);
		return 0;
	default:
		break;
	}
	return 0;
}

/* Warning/Error for config.c */
void
configuration_warning (const char *fname, const int line, const char *fmt, ...)
{
	va_list args;

	conf_error_displayed = 0;
	fputs (_("configuration warning:"), stderr);
	fputc (' ', stderr);

	/* Prefix */
	if (fname != last_error_file
	 || line != last_error_line) {
		last_error_file = fname;
		last_error_line = line;
		print_error_prefix (fname, line, NULL);
	}

	/* Body */
	va_start(args, fmt);
	vfprintf (stderr, fmt, args);
	va_end(args);

	/* Postfix */
	putc('\n', stderr);
	fflush(stderr);

	if (sav_lst_file) {
		return;
	}
	warningcount++;
}

void
configuration_error (const char *fname, const int line,
                     const int finish_error, const char *fmt, ...)
{
	va_list args;

	configuration_error_head ();

	/* Prefix */
	if (fname != last_error_file
	 || line != last_error_line) {
		last_error_file = fname;
		last_error_line = line;
		print_error_prefix (fname, line, NULL);
	}

	/* Body */
	va_start (args, fmt);
	vfprintf (stderr, fmt, args);
	va_end (args);

	/* Postfix */
	if (!finish_error) {
		putc (';', stderr);
		putc ('\n', stderr);
		putc ('\t', stderr);
		return;
	}

	putc ('\n', stderr);
	fflush (stderr);

	if (sav_lst_file) {
		return;
	}

	if (++errorcount > cb_max_errors) {
		cobc_too_many_errors ();
	}
}

/* Generic warning/error routines */
void
cb_warning_x (const enum cb_warn_opt opt, cb_tree x, const char *fmt, ...)
{
	va_list ap;
	const enum cb_warn_val pref = cb_warn_opt_val[opt];

	if (pref == COBC_WARN_DISABLED) {
		return;
	}

	va_start (ap, fmt);
	if (pref != COBC_WARN_AS_ERROR) {
		print_error (x->source_file, x->source_line, _ ("warning: "), fmt, ap, warning_option_text (opt, pref));
	} else {
		print_error (x->source_file, x->source_line, _ ("error: "), fmt, ap, warning_option_text (opt, pref));
	}
	va_end (ap);

	if (sav_lst_file) {
		return;
	}
	if (pref == COBC_WARN_AS_ERROR) {
		if (++errorcount > cb_max_errors) {
			cobc_too_many_errors ();
		}
	} else {
		warningcount++;
	}
}

/* raise a warning (or error, or nothing) depending on a dialect option */
void
cb_warning_dialect_x (const enum cb_support tag, cb_tree x, const char *fmt, ...)
{
	const char	*file = NULL;
	int		line = 0;
	va_list 	ap;

	if (tag == CB_OK) {
		return;
	}

	if (x) {
		file = x->source_file;
		line = x->source_line;
	}

	va_start (ap, fmt);
	print_error (file, line, (tag == CB_ERROR || tag == CB_UNCONFORMABLE) ? _("error: ") : _("warning: "),
		fmt, ap, NULL);
	va_end (ap);

	if (sav_lst_file) {
		return;
	}
	if (tag == CB_ERROR || tag == CB_UNCONFORMABLE) {
		if (++errorcount > cb_max_errors) {
			cobc_too_many_errors ();
		}
	} else {
		warningcount++;
	}
}

void
cb_note_x (const enum cb_warn_opt opt, cb_tree x, const char *fmt, ...)
{
	va_list ap;

	va_start (ap, fmt);
	if (opt != COB_WARNOPT_NONE) {
		const enum cb_warn_val	pref = cb_warn_opt_val[opt];
		if (pref == COBC_WARN_DISABLED) {
			return;
		}
		print_error (x->source_file, x->source_line, _("note: "),
			fmt, ap, warning_option_text (opt, pref));
	} else {
		print_error (x->source_file, x->source_line, _("note: "),
			fmt, ap, NULL);
	}
	va_end (ap);
}

void
cb_error_x (cb_tree x, const char *fmt, ...)
{
	va_list ap;
	const enum cb_warn_opt	opt = cb_warn_ignored_error;
	const enum cb_warn_val	pref = cb_warn_opt_val[opt];

	if (ignore_error && pref == COBC_WARN_DISABLED) {
		return;
	}

	va_start (ap, fmt);
	if (!ignore_error) {
		print_error (x->source_file, x->source_line, _("error: "),
			fmt, ap, NULL);
	} else if (pref == COBC_WARN_AS_ERROR) {
		print_error (x->source_file, x->source_line, _("error: "),
			fmt, ap, warning_option_text (opt, pref));
	} else {
		print_error (x->source_file, x->source_line, _("warning: "),
			fmt, ap, warning_option_text (opt, pref));
	}
	va_end (ap);

	if (sav_lst_file) {
		return;
	}
	if (ignore_error && pref != COBC_WARN_AS_ERROR) {
		warningcount++;
	} else {
		if (++errorcount > cb_max_errors) {
			cobc_too_many_errors ();
		}
	}
}

/**
 * verify if the given compiler option is supported by the current std/configuration
 * \param	x	tree whose position is used for raising warning/errors
 * \return	1 = ok/warning/obsolete, 0 = skip/ignore/error/unconformable
 */
unsigned int
cb_verify_x (cb_tree x, const enum cb_support tag, const char *feature)
{
	int	ignore_error_sav;

	if (!x) {
		x = cobc_parse_malloc (sizeof (struct cb_tree_common));
		x->source_file = NULL;
		x->source_line = 0;
	}

	switch (tag) {
	case CB_OK:
		return 1;
	case CB_WARNING:
		cb_warning_x (cb_warn_dialect, x, _("%s used"), feature);
		return 1;
	case CB_ARCHAIC:
		cb_warning_x (cb_warn_archaic, x, _("%s is archaic in %s"),
			feature, cb_config_name);
		return 1;
	case CB_OBSOLETE:
		cb_warning_x (cb_warn_obsolete, x, _("%s is obsolete in %s"),
			feature, cb_config_name);
		return 1;
	case CB_SKIP:
		return 0;
	case CB_IGNORE:
		cb_warning_x (cb_warn_additional, x, _("%s ignored"), feature);
		return 0;
	case CB_ERROR:
		/* Fall-through */
	case CB_UNCONFORMABLE:
		/* raise error in any case */
		ignore_error_sav = cb_set_ignore_error (0);
		if (tag == CB_ERROR) {
			cb_error_x (x, _("%s used"), feature);
		} else {
			cb_error_x (x, _("%s does not conform to %s"), feature, cb_config_name);
		}
		(void) cb_set_ignore_error (ignore_error_sav);
		return 0;

	/* LCOV_EXCL_START */
	default:
		/* This should never happen (and therefore doesn't get a translation) */
		cobc_err_msg ("unexpected compiler option value: %d", tag);
		COBC_ABORT ();
	/* LCOV_EXCL_STOP */
	}
#ifndef _MSC_VER
	/* not reached */
	return 0;
#endif
}

/**
 * verify if the given compiler option is supported by the current std/configuration
 * current position is used for raising warning/errors
 * \returns	1 = ok/warning/obsolete, 0 = skip/ignore/error/unconformable
 */
unsigned int
cb_verify (const enum cb_support tag, const char *feature)
{
	return cb_verify_x (NULL, tag, feature);
}

void
redefinition_error (cb_tree x)
{
	struct cb_word	*w;

	w = CB_REFERENCE (x)->word;
	cb_error_x (x, _("redefinition of '%s'"), w->name);
	if (w->items) {
		if (CB_VALUE (w->items)->source_line == 0) {
			return;
		}
		listprint_suppress ();
		cb_note_x (cb_warn_redefinition, CB_VALUE (w->items),
			_("'%s' previously defined here"), w->name);
		listprint_restore ();
	}
}

void
redefinition_warning (cb_tree x, cb_tree y)
{
	struct cb_word	*w;
	cb_tree		z;

	/* early exit if warning disabled */
	{
		const enum cb_warn_opt	opt = cb_warn_redefinition;
		const enum cb_warn_val	pref = cb_warn_opt_val[opt];
		if (pref == COBC_WARN_DISABLED) {
			return;
		}
	}

	w = CB_REFERENCE (x)->word;
	cb_warning_x (cb_warn_redefinition, x, _("redefinition of '%s'"), w->name);

	z = NULL;
	if (y) {
		z = y;
	} else if (w->items) {
		z = CB_VALUE (w->items);
	}

	if (z) {
		if (z->source_line == 0) {
			return;
		}
		listprint_suppress ();
		cb_note_x (cb_warn_redefinition, z, _("'%s' previously defined here"), w->name);
		listprint_restore ();
	}
}

void
undefined_error (cb_tree x)
{
	struct cb_reference	*r = CB_REFERENCE (x);
	cb_tree			c;
	const char		*error_message;

	if (!errnamebuff) {
		errnamebuff = cobc_main_malloc ((size_t)COB_NORMAL_BUFF);
	}

	/* Get complete variable name */
	snprintf (errnamebuff, (size_t)COB_NORMAL_MAX, "%s", CB_NAME (x));
	errnamebuff[COB_NORMAL_MAX] = 0;
	if (r->chain) {
		for (c = r->chain; c; c = CB_REFERENCE (c)->chain) {
			strcat (errnamebuff, " IN ");
			strcat (errnamebuff, CB_NAME (c));
		}
		error_message = _("'%s' is not defined");
	} else {
		if (is_reserved_word (CB_NAME (x))) {
			error_message = _("'%s' cannot be used here");
		} else if (is_default_reserved_word (CB_NAME (x))) {
			error_message = _("'%s' is not defined, but is a reserved word in another dialect");
		} else {
			error_message = _("'%s' is not defined");
		}
	}

	if (r->flag_optional) {
		cb_warning_x (cb_warn_additional, x, error_message, errnamebuff);
	} else {
		cb_error_x (x, error_message, errnamebuff);
	}
}

void
ambiguous_error (cb_tree x)
{
	struct cb_word	*w;
	struct cb_field	*p;
	struct cb_label	*l2;
	cb_tree		l;
	cb_tree		y;

	w = CB_REFERENCE (x)->word;
	if (w->error == 0) {
		if (!errnamebuff) {
			errnamebuff = cobc_main_malloc ((size_t)COB_NORMAL_BUFF);
		}
		/* Display error the first time */
		snprintf (errnamebuff, (size_t)COB_NORMAL_MAX, "%s", CB_NAME (x));
		errnamebuff[COB_NORMAL_MAX] = 0;
		for (l = CB_REFERENCE (x)->chain; l; l = CB_REFERENCE (l)->chain) {
			strcat (errnamebuff, " IN ");
			strcat (errnamebuff, CB_NAME (l));
		}
		/* FIXME: qualification may not be possible, would need the code below to execute first */
		cb_error_x (x, _("'%s' is ambiguous; needs qualification"), errnamebuff);
		w->error = 1;

		/* Display all fields with the same name */
		for (l = w->items; l; l = CB_CHAIN (l)) {
			y = CB_VALUE (l);
			errnamebuff[0] = 0;
			strcat (errnamebuff, w->name);
			switch (CB_TREE_TAG (y)) {
			case CB_TAG_FIELD:
				for (p = CB_FIELD (y)->parent; p; p = p->parent) {
					strcat (errnamebuff, " IN ");
					strcat (errnamebuff, cb_name (CB_TREE(p)));
				}
				break;
			case CB_TAG_LABEL:
				l2 = CB_LABEL (y);
				if (l2->section) {
					strcat (errnamebuff, " IN ");
					strcat (errnamebuff,
						(const char *)(l2->section->name));
				}
				break;
			default:
				break;
			}
			if (y->source_line == 0) {
				continue;
			}
			listprint_suppress ();
		if (y->source_line == 0) {
			if (cb_get_register_definition (w->name)) {
				cb_note_x (COB_WARNOPT_NONE, x, _("'%s' is a special register"), w->name);
			} else {
				cb_note_x (COB_WARNOPT_NONE, x, _("'%s' internally defined"), errnamebuff);
			}
		} else {
			cb_note_x (COB_WARNOPT_NONE, y, _("'%s' defined here"), errnamebuff);
		}
			listprint_restore ();
		}
	}
}

/* error routine for flex */
void
flex_fatal_error (const char *msg, const char * filename, const int line_num)
{
	/* LCOV_EXCL_START */
	cobc_err_msg (_ ("fatal error: %s"), msg);
	cobc_abort (filename, line_num);
	/* LCOV_EXCL_STOP */
}

void
group_error (cb_tree x, const char *clause)
{
	cb_error_x (x, _("group item '%s' cannot have %s clause"),
		    cb_name (x), clause);
}


void
level_require_error (cb_tree x, const char *clause)
{
	const char		*s;
	const struct cb_field	*f;

	s = cb_name (x);
	f = CB_FIELD_PTR (x);
	if (f->flag_item_78) {
		cb_error_x (x, _("constant item '%s' requires a %s clause"), s, clause);
	} else {
		cb_error_x (x, _("level %02d item '%s' requires a %s clause"), f->level,
			    s, clause);
	}
}

void
level_except_error (cb_tree x, const char *clause)
{
	const char		*s;
	const struct cb_field	*f;

	s = cb_name (x);
	f = CB_FIELD_PTR (x);
	if (f->flag_item_78) {
		cb_error_x (x, _("constant item '%s' can only have a %s clause"), s, clause);
	} else {
		cb_error_x (x, _("level %02d item '%s' can only have a %s clause"), f->level,
			    s, clause);
	}
}

/* routines for temporary disable listing output of warnings/errors */
void
listprint_suppress (void)
{
	if (cb_src_list_file) {
		sav_lst_file = cb_src_list_file;
		cb_src_list_file = NULL;
	}
}

void
listprint_restore (void)
{
	if (sav_lst_file) {
		cb_src_list_file = sav_lst_file;
		sav_lst_file = NULL;
	}
}

/*
   Copyright (C) 2004-2012, 2014-2021 Free Software Foundation, Inc.
   Written by Roger While, Simon Sobisch, Brian Tiffin

   This file is part of GnuCOBOL.

   The GnuCOBOL module loader is free software: you can redistribute it
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

#ifdef	HAVE_LOCALE_H
#include <locale.h>
#endif
#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "libcob/sysdefines.h"
#include "libcob.h"
#include "tarstamp.h"

#include "libcob/cobgetopt.h"

static int arg_shift = 1;
static int print_runtime_wanted = 0;
static signed int	verbose_output = 0;

static const char short_options[] = "+hirc:VvqM:";

#define	CB_NO_ARG	no_argument
#define	CB_RQ_ARG	required_argument
#define	CB_OP_ARG	optional_argument

static const struct option long_options[] = {
	{"help",		CB_NO_ARG, NULL, 'h'},
	{"version",   		CB_NO_ARG, NULL, 'V'},
	{"verbose",		CB_NO_ARG, NULL, 'v'},
	{"brief",		CB_NO_ARG, NULL, 'q'},
	{"info",		CB_NO_ARG, NULL, 'i'},
	{"runtime-config",		CB_NO_ARG, NULL, 'r'},
	{"config",		CB_RQ_ARG, NULL, 'C'},
	{"module",		CB_RQ_ARG, NULL, 'm'},
	{NULL, 0, NULL, 0}
};

#ifdef ENABLE_NLS
#include "gettext.h"	/* from lib/ */
#define _(s)		gettext(s)
#define N_(s)		gettext_noop(s)
#else
#define _(s)		s
#define N_(s)		s
#endif


/**
 * Display cobcrun build and version date
 */
static void
cobcrun_print_version (void)
{
	char	cob_build_stamp[COB_MINI_BUFF];
	char	month[64];
	int status, day, year;

	/* Set up build time stamp */
	memset (cob_build_stamp, 0, (size_t)COB_MINI_BUFF);
	memset (month, 0, sizeof(month));
	day = 0;
	year = 0;
	status = sscanf (__DATE__, "%s %d %d", month, &day, &year);
	/* LCOV_EXCL_START */
	if (status != 3) {
		snprintf (cob_build_stamp, (size_t)COB_MINI_MAX,
			"%s %s", __DATE__, __TIME__);
	/* LCOV_EXCL_STOP */
	} else {
		snprintf (cob_build_stamp, (size_t)COB_MINI_MAX,
			  "%s %2.2d %4.4d %s", month, day, year, __TIME__);
	}

	printf ("cobcrun (%s) %s.%d\n", PACKAGE_NAME, PACKAGE_VERSION, PATCH_LEVEL);
	puts ("Copyright (C) 2021 Free Software Foundation, Inc.");
	printf (_("License GPLv3+: GNU GPL version 3 or later <%s>"), "https://gnu.org/licenses/gpl.html");
	putchar ('\n');
	puts (_("This is free software; see the source for copying conditions.  There is NO\n"
	        "warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE."));
	printf (_("Written by %s"), "Roger While, Simon Sobisch, Brian Tiffin");
	putchar ('\n');
	printf (_("Built     %s"), cob_build_stamp);
	putchar ('\n');
	printf (_("Packaged  %s"), COB_TAR_DATE);
	putchar ('\n');
}

/**
 * Display cobcrun help
 */
static void
cobcrun_print_usage (char * prog)
{
	puts (_("GnuCOBOL module loader"));
	putchar ('\n');
	printf (_("Usage: %s [options] PROGRAM [parameter ...]"), prog);
	putchar ('\n');
	printf (_("  or:  %s options"), prog);
	putchar ('\n');
	putchar ('\n');
	puts (_("Options:"));
	puts (_("  -h, --help                      display this help and exit"));
	puts (_("  -V, --version                   display cobcrun and runtime version and exit"));
	puts (_("  -i, --info                      display runtime information (build/environment)"));
	puts (_("  -v, --verbose                   display extended output with --info"));
#if 0 /* Simon: currently only removing the path from cobcrun in output --> don't show */
	puts (_("  -q, --brief                     reduced displays"));
#endif
	puts (_("  -c <file>, --config=<file>      set runtime configuration from <file>"));
	puts (_("  -r, --runtime-config            display current runtime configuration\n"
	        "                                  (value and origin for all settings)"));
	puts (_("  -M <module>, --module=<module>  set entry point module name and/or load path\n"
			"                                  where -M module prepends any directory to the\n"
			"                                  dynamic link loader library search path\n"
			"                                  and any basename to the module preload list\n"
			"                                  (COB_LIBRARY_PATH and/or COB_PRELOAD)"));
	putchar ('\n');
	printf (_("Report bugs to: %s\n" 
			  "or (preferably) use the issue tracker via the home page."), "bug-gnucobol@gnu.org");
	putchar ('\n');
	printf (_("GnuCOBOL home page: <%s>"), "https://www.gnu.org/software/gnucobol/");
	putchar ('\n');
	printf (_("General help using GNU software: <%s>"), "https://www.gnu.org/gethelp/");
	putchar ('\n');
}

/**
 * split into path and file, or just path, or just file
 * returns allocated strings (possible emtpy) for both
 *  Note: cob_free must be called with *pathname and *filename
 *        for releasing memory after use
 */
static void
cobcrun_split_path_file (char** pathname, char** filename, char *pf)
{
	char *pos = pf;
	char *next_pos;

	char sav;

	/* set pos to last slash (if any) */
	while ((next_pos = strpbrk (pos + 1, "\\/")) != NULL) {
		pos = next_pos;
	}
	/* copy string up to last slash as pathname (possible emtpy) */
	sav = *pos;
	*pos = 0;
	*pathname = cob_strdup (pf);
	*pos = sav;

	/* set pos to first character after last slash (if any) */
	if (pf != pos) {
		pos++;
	}

	/* copy string after last slash as filename (possible emtpy) */
	*filename = cob_strdup (pos);
}

/**
 * Prepend a new directory path to the library search COB_LIBRARY_PATH
 * and setup a module COB_PRE_LOAD, for each component included.
 */
static const char *
cobcrun_initial_module (char *module_argument)
{
	char	*pathname, *filename;
	char	env_space[COB_MEDIUM_BUFF], *envptr;
	/* FIXME: split in two functions (one setting module, one setting path)
	          after allowing module with path in COB_PRE_LOAD */

	/* LCOV_EXCL_START */
	if (!module_argument) {
		/* never reached (getopt ensures that we have an argument),
		   just in to keep the analyzer happy */
		return "missing argument";
	}
	/* LCOV_EXCL_STOP */

	/* See if we have a /dir/path/module, or a /dir/path/ or a module (no slash) */
	cobcrun_split_path_file (&pathname, &filename, module_argument);
	if (*pathname) {
		/* TODO: check content, see libcob/common.h */
		envptr = getenv ("COB_LIBRARY_PATH");
		if (envptr
		 && strlen (envptr) + strlen (pathname) + 1 < COB_MEDIUM_MAX) {
			memset (env_space, 0, COB_MEDIUM_BUFF);
			snprintf (env_space, COB_MEDIUM_MAX, "%s%c%s",
				pathname, PATHSEP_CHAR, envptr);
			env_space[COB_MEDIUM_MAX] = 0; /* fixing code analyser warning */
			(void) cob_setenv ("COB_LIBRARY_PATH", env_space, 1);
		} else {
			(void) cob_setenv ("COB_LIBRARY_PATH", pathname, 1);
		}
	}
	cob_free((void *)pathname);

	if (*filename) {
		/* TODO: check content, see libcob/common.h */
		envptr = getenv ("COB_PRE_LOAD");
		if (envptr
			&& strlen (envptr) + strlen (filename) + 1 < COB_MEDIUM_MAX) {
			memset (env_space, 0, COB_MEDIUM_BUFF);
			snprintf (env_space, COB_MEDIUM_MAX, "%s%c%s", filename,
				PATHSEP_CHAR, envptr);
			env_space[COB_MEDIUM_MAX] = 0; /* fixing code analyser warning */
			(void) cob_setenv ("COB_PRE_LOAD", env_space, 1);
		} else {
			(void) cob_setenv ("COB_PRE_LOAD", filename, 1);
		}
	}
	cob_free ((void *)filename);
	return NULL;
}

/**
 * process the cobcrun command options
 */
static void
process_command_line (int argc, char *argv[])
{
	int			c, idx;
	const char		*err_msg;
	
#if defined (_WIN32) || defined (__DJGPP__)
	if (!getenv ("POSIXLY_CORRECT")) {
		/* Translate command line arguments from DOS/WIN to UNIX style */
		int argnum = 0;
		while (++argnum < argc) {
			if (strrchr(argv[argnum], '/') == argv[argnum]) {
				if (argv[argnum][1] == '?' && !argv[argnum][2]) {
					argv[argnum] = (char *)"--help";
					continue;
				}
				argv[argnum][0] = '-';
			}
		}
	}
#endif

	/* c = -1 if idx > argc or argv[idx] has non-option */
	while ((c = cob_getopt_long_long (argc, argv, short_options,
					  long_options, &idx, 1)) >= 0) {
		switch (c) {
		case '?':
			/* Unknown option or ambiguous */
			exit (EXIT_FAILURE);

		case 'c':
		case 'C':
			/* -c <file>, --config=<file> */
			/* LCOV_EXCL_START */
			if (strlen (cob_optarg) > COB_SMALL_MAX) {
				fputs (_("invalid configuration file name"), stderr);
				putc ('\n', stderr);
				fflush (stderr);
				exit (EXIT_FAILURE);
			}
			/* LCOV_EXCL_STOP */
			arg_shift++;
			(void) cob_setenv ("COB_RUNTIME_CONFIG", cob_optarg, 1);
			/* shift argument again if two part argument was used */
			if (c == 'c') {
				arg_shift++;
			}
			break;

		case 'h':
			/* --help */
			cobcrun_print_usage (argv[0]);
			exit (EXIT_SUCCESS);

		case 'i':
			/* --info */
			cob_init_nomain (0, &argv[0]);
			print_info_detailed (verbose_output);
			exit (EXIT_SUCCESS);

		case 'q':
			/* --brief : reduced reporting */
			/* resets -verbose and removes the path to cobcrun in argv[0] */
			verbose_output = 0;
			strcpy (argv[0], "cobcrun");	/* set for simple compare in test suite
										   and other static output */
			arg_shift++;
			break;

		case 'v':
			/* --verbose : Verbose reporting */
			verbose_output++;
			arg_shift++;
			break;

		case 'r':
			/* --runtime-conf */
			print_runtime_wanted = 1;
			arg_shift++;
			break;

		case 'V':
			/* --version */
			cob_init_nomain (0, &argv[0]);
			cobcrun_print_version ();
			putchar ('\n');
			print_version ();
			if (verbose_output) {
				putchar ('\n');
				print_version_summary ();
			}
			exit (EXIT_SUCCESS);

		case 'M':
		case 'm':
			/* -M <module>, --module=<module> */
			arg_shift++;
			err_msg = cobcrun_initial_module (cob_optarg);
			if (err_msg != NULL) {
				fprintf (stderr, _("invalid module argument '%s'"), cob_optarg);
				putc ('\n', stderr);
				fputs (err_msg, stderr);
				fflush (stderr);
				exit (EXIT_FAILURE);
			}
			/* shift argument again if two part argument was used */
			if (c == 'M') {
				arg_shift++;
			}
			break;

		/* LCOV_EXCL_START */
		default:
			/* not translated as it is an unlikely internal error: */
			fprintf (stderr, "missing evaluation of command line option '%c'", c);
			putc ('\n', stderr);
			fputs (_("Please report this!"), stderr);
			fflush (stderr);
			exit (EXIT_FAILURE);
		/* LCOV_EXCL_STOP */

		}
	}
}

/**
 * cobcrun, for invoking entry points from dynamic shared object libraries
 */
int
main (int argc, char **argv)
{
	cob_call_union	unifunc;

#ifdef	HAVE_SETLOCALE
	setlocale (LC_ALL, "");
#endif

	/* minimal initialization of the environment like binding textdomain,
	   allowing test to be run under WIN32 (implied in cob_init(),
	   no need to call outside of GnuCOBOL) */
	cob_common_init (NULL);

	process_command_line (argc, argv);

	/* At least one option or module name needed */
	if (argc <= arg_shift) {
		if (print_runtime_wanted) {
			cob_init_nomain (0, &argv[0]);
			print_runtime_conf ();
			cob_stop_run (EXIT_SUCCESS);
		}
		fprintf (stderr, _("%s: missing PROGRAM name"), argv[0]);
		putc ('\n', stderr);
		fprintf (stderr, _("Try '%s --help' for more information."), argv[0]);
		putc ('\n', stderr);
		fflush (stderr);
		return 1;
	}

	if (strlen (argv[arg_shift]) > COB_MAX_NAMELEN) {
		/* note: we allow up to COB_MAX_WORDLEN for relaxed syntax... */
		fprintf (stderr, _("%s: PROGRAM name exceeds %d characters"), argv[0], COB_MAX_NAMELEN);
		putc ('\n', stderr);
		fflush (stderr);
		return 1;
	}

	/* Initialize the COBOL system, resolve the PROGRAM name */
	/*   and invoke, wrapped in a STOP RUN, if found */
	/*   note: we use cob_init_nomain here as there are no functions */
	/*         linked here we want to provide for the COBOL environment */
	cob_init_nomain (argc - arg_shift, &argv[arg_shift]);
	if (print_runtime_wanted) {
		print_runtime_conf ();
		putc ('\n', stdout);
	}
	/* Note: cob_resolve_cobol takes care for call errors, no need to check here */
	unifunc.funcvoid = cob_resolve_cobol (argv[arg_shift], 0, 1);
	cob_stop_run (unifunc.funcint());
}

/*
   Copyright (C) 2001-2022 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Ron Norman, Simon Sobisch,
   Brian Tiffin, Edward Hart, Dave Pitts

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
#include <string.h>

#include "cobc.h"

void
cobc_print_usage (char * prog)
{
	puts (_("GnuCOBOL compiler for most COBOL dialects with lots of extensions"));
	putchar ('\n');
	printf (_("Usage: %s [options]... file..."), prog);
	putchar ('\n');
	putchar ('\n');

	cobc_print_usage_common_options ();

	cobc_print_usage_warnings ();

	cobc_print_usage_flags ();

	cobc_print_usage_dialect ();

	putchar ('\n');
	printf (_("Report bugs to: %s\n"
	          "or (preferably) use the issue tracker via the home page."),
			"bug-gnucobol@gnu.org");
	putchar ('\n');
	puts (_("GnuCOBOL home page: <https://www.gnu.org/software/gnucobol/>"));
	puts (_("General help using GNU software: <https://www.gnu.org/gethelp/>"));
}

void
cobc_print_usage_common_options (void)
{
	puts (_("Options:"));
	puts (_("  -h, --help            display this help and exit"));
	puts (_("  -V, --version         display compiler version information and exit"));
	puts (_("  -dumpversion          display compiler version and exit"));
	puts (_("  -i, --info            display compiler information (build/environment)\n"
	        "                        and exit"));
	puts (_("  -v, --verbose         verbose mode, display additional information;\n"
		    "                        multiple -v options increase the verbosity,\n"
		    "                        the maximum is 3 as follows:\n"
		    "                        (1) display compiler version and the commands\n"
	        "                        invoked by the compiler,\n"
		    "                        (2) pass verbose option to assembler/compiler\n"
		    "                        (3) pass verbose option to linker"));
	puts (_("  -q, --brief           reduced displays, commands invoked not shown"));
	puts (_("  -###                  like -v but commands not executed"));
	puts (_("  -x                    build an executable program"));
	puts (_("  -m                    build a dynamically loadable module (default)"));
	puts (_("  -j [<args>], --job[=<args>]\trun program after build, passing <args>"));
	puts (_("  -std=<dialect>        warnings/features for a specific dialect\n"
	        "                        <dialect> can be one of:\n"
	        "                        default, cobol2014, cobol2002, cobol85, xopen,\n"
	        "                        ibm-strict, ibm, mvs-strict, mvs,\n"
	        "                        mf-strict, mf, bs2000-strict, bs2000,\n"
	        "                        acu-strict, acu, rm-strict, rm, gcos-strict,\n"
	        "                        gcos;\n"
	        "                        see configuration files in directory config"));
	puts (_("  -F, --free            use free source format (alias for -fformat=free)"));
	puts (_("  --fixed               use fixed source format (default; alias for\n"
		"                        -fformat=fixed)"));
	puts (_("  -O, -O2, -O3, -Os     enable optimization"));
	puts (_("  -O0                   disable optimization"));
	puts (_("  -g                    enable C compiler debug and stack check"));
	puts (_("  -d, --debug           enable all run-time error checking,\n"
	        "                        equal to -fstack-check -fec=EC-ALL"));
	/* duplicated here from flags.def to place it next to -debug */
	puts (_("  -fec=<exception-name>\tenable code generation for <exception-name>,\n"
	        "                        see --list-exceptions for the possible values,\n"
	        "                        sets -fsource-location"));
	puts (_("  -fno-ec=<exception-name>\tdisable code generation for <exception-name>"));
	puts (_("  -o <file>             place the output into <file>"));
	puts (_("  -b                    combine all input files into a single\n"
	        "                        dynamically loadable module"));
	puts (_("  -E                    preprocess only; do not compile or link"));
	puts (_("  -C                    translation only; convert COBOL to C"));
	puts (_("  -S                    compile only; output assembly file"));
	puts (_("  -c                    compile and assemble, but do not link"));
	puts (_("  -T <file>             generate and place a wide program listing into <file>"));
	puts (_("  -t <file>             generate and place a program listing into <file>"));
	puts (_("  --tlines=<lines>      specify lines per page in listing, default = 55"));
#if 0 /* to be hidden later, use -f[no-]tsymbols instead */
	puts (_("  --tsymbols            specify symbols in listing, use -ftsymbols instead"));
#endif
	puts (_("  -P[=<dir or file>]    generate preprocessed program listing (.lst)"));
#ifndef COB_INTERNAL_XREF
	puts (_("  -X, --Xref            generate cross reference through 'cobxref'\n"
	        "                        (V. Coen's 'cobxref' must be in path)"));
#else
	puts (_("  -X, --Xref            specify cross reference in listing"));
#endif
	puts (_("  -I <directory>        add <directory> to copy/include search path"));
	puts (_("  -L <directory>        add <directory> to library search path"));
	puts (_("  -l <lib>              link the library <lib>"));
	puts (_("  -A <options>          add <options> to the C compile phase"));
	puts (_("  -Q <options>          add <options> to the C link phase"));
	puts (_("  -D <define>           define <define> for COBOL compilation"));
	puts (_("  -K <entry>            generate CALL to <entry> as static"));
	puts (_("  --conf=<file>         user-defined dialect configuration; see -std"));
	puts (_("  --list-reserved       display reserved words"));
	puts (_("  --list-intrinsics     display intrinsic functions"));
	puts (_("  --list-mnemonics      display mnemonic names"));
	puts (_("  --list-exceptions     display exception names"));
	puts (_("  --list-system         display system routines"));
	puts (_("  --save-temps[=<dir>]  save intermediate files\n"
	        "                        * default: current directory"));
	puts (_("  -ext <extension>      add file extension for resolving COPY"));
	putchar ('\n');
}

void
cobc_print_usage_warnings (void)
{
	puts (_("Warning options:"));
	puts (_("  -Wall                 enable most warnings (all except as noted below)"));
	puts (_("  -Wextra               like -Wall but enable some extra warning flags"));
	puts (_("  -w                    disable all warnings"));
	puts (_("  -Wno-<warning>        disable warning enabled by default, -Wall or -Wextra"));
#define	CB_WARNDEF(opt,name,doc)		\
	puts (doc);
#define	CB_ONWARNDEF(opt,name,doc)		\
	puts (doc);							\
	/* TRANSLATORS: This msgid is appended to msgid for -Wno-pending and others */ \
	puts (_("                        * ALWAYS active"));
#define	CB_NOWARNDEF(opt,name,doc)		\
	puts (doc);							\
	/* TRANSLATORS: This msgid is appended to msgid for -Wpossible-truncate and others */ \
	puts (_("                        * NOT set with -Wall"));
#define	CB_ERRWARNDEF(var,name,doc)		\
	puts (doc);
#include "warning.def"
#undef	CB_WARNDEF
#undef	CB_ONWARNDEF
#undef	CB_NOWARNDEF
#undef	CB_ERRWARNDEF
	puts (_("  -Werror               treat all warnings as errors"));
	puts (_("  -Wno-error            don't treat warnings as errors"));
	puts (_("  -Werror=<warning>     treat specified <warning> as error"));
	puts (_("  -Wno-error=<warning>  don't treat specified <warning> as error"));
	putchar ('\n');
}

static void
cobc_print_active (const char *doc, const int print_help)
{
	if (!print_help) {
		return;
	}
	puts (doc);
}

void
cobc_print_usage_flags (void)
{
	puts (_("Compiler options:"));
#define	CB_FLAG(var,print_help,name,doc)		\
	cobc_print_active (doc, print_help);
#define	CB_FLAG_ON(var,print_help,name,doc)		\
	cobc_print_active (doc, print_help);
#define	CB_FLAG_RQ(var,print_help,name,def,opt,doc)		\
	cobc_print_active (doc, print_help);
#define	CB_FLAG_NQ(print_help,name,opt,doc)		\
	cobc_print_active (doc, print_help);
#define	CB_FLAG_OP(print_help,name,opt,doc)		\
	cobc_print_active (doc, print_help);
#include "flag.def"
#undef	CB_FLAG
#undef	CB_FLAG_ON
#undef	CB_FLAG_RQ
#undef	CB_FLAG_NQ
#undef	CB_FLAG_OP
	cobc_print_active (
	_("  -fibmcomp             sets -fbinary-size=2-4-8 -fsynchronized-clause=ok\n"
	  "  -fno-ibmcomp          sets -fbinary-size=1--8  -fsynchronized-clause=ignore"), 1);
	putchar ('\n');
}

static void
cobc_print_config_flag (const char *name, const char *doc,
		 const char *odoc)
{
	char		buff[78];

	if (!doc) {
		return;
	}
	if (odoc) {
		snprintf (buff, sizeof (buff) - 1, "%s=%s", name, odoc);
		buff [77] = 0;	/* keep analyzer happy ... */
		name = (const char *) &buff;
	}
	if (strlen (name) <= 19) {
		printf ("  -f%-19s  %s\n", name, doc);
	} else {
		printf ("  -f%s\t%s\n", name, doc);
	}
}

void
cobc_print_usage_dialect (void)
{
	puts (_("Compiler dialect configuration options:"));
#define	CB_CONFIG_STRING(var,name,doc)		\
	cobc_print_config_flag (name, doc, _("<value>"));
#define	CB_CONFIG_INT(var,name,min,max,odoc,doc)		\
	cobc_print_config_flag (name, doc, odoc);
#define	CB_CONFIG_SINT(var,name,min,max,odoc,doc)		\
	cobc_print_config_flag (name, doc, odoc);
#define	CB_CONFIG_SIZE(var,name,min,max,odoc,doc)		\
	cobc_print_config_flag (name, doc, odoc);
#define	CB_CONFIG_ANY(type,var,name,doc)		\
	cobc_print_config_flag (name, doc, _("<value>"));
#define	CB_CONFIG_BOOLEAN(var,name,doc)		\
	cobc_print_config_flag (name, doc, NULL);
#define	CB_CONFIG_SUPPORT(var,name,doc)		\
	cobc_print_config_flag (name, doc, _("<support>"));
#include "config.def"
#undef	CB_CONFIG_ANY
#undef	CB_CONFIG_INT
#undef	CB_CONFIG_SINT
#undef	CB_CONFIG_SIZE
#undef	CB_CONFIG_STRING
#undef	CB_CONFIG_BOOLEAN
#undef	CB_CONFIG_SUPPORT
	putchar ('\t');
	puts (_("where <support> is one of the following:"));
	putchar ('\t');
	/* Note: separated as translators should be able to replace the single quote */
	printf (_("'%s'"), "ok");
	putchar (',');
	putchar (' ');
	printf (_("'%s'"), "warning");
	putchar (',');
	putchar (' ');
	printf (_("'%s'"), "archaic");
	putchar (',');
	putchar (' ');
	printf (_("'%s'"), "obsolete");
	putchar (',');
	putchar (' ');
	printf (_("'%s'"), "skip");
	putchar (',');
	putchar (' ');
	printf (_("'%s'"), "ignore");
	putchar (',');
	putchar (' ');
	printf (_("'%s'"), "error");
	putchar (',');
	putchar (' ');
	printf (_("'%s'"), "unconformable");
	putchar ('\n');
	cobc_print_config_flag ("not-reserved", _("word to be taken out of the reserved words list"), _("<word>"));
	cobc_print_config_flag ("reserved", _("word to be added to reserved words list"), _("<word>"));
	cobc_print_config_flag ("reserved", _("word to be added to reserved words list as alias"), _("<word>:<alias>"));
	cobc_print_config_flag ("not-register", _("special register to disable"), _("<word>"));
	cobc_print_config_flag ("register", _("special register to enable"),
		_("<word> or <word>:<definition>, where definition uses backslash escaped spaces"));
	putchar ('\n');
}

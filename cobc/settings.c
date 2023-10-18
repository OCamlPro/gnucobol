/*
   Copyright (C) 2001-2023 Free Software Foundation, Inc.

   Authors:
   Keisuke Nishida, Roger While, Ron Norman, Simon Sobisch, Brian Tiffin,
   Edward Hart, Dave Pitts, Fabrice Le Fessant

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

#include "tarstamp.h"
#include "config.h"

#include <sys/types.h>
#include <sys/stat.h>
#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <string.h>
#ifdef	HAVE_STRINGS_H
#include <strings.h>
#endif
#include <ctype.h>
#include <time.h>
#include <limits.h>

#include "cobc.h"
#include "tree.h"

/*
Settings are used to override GnuCOBOL host config, i.e. parameters
that are used to adapt GnuCOBOL to the system running it. By default,
these parameters are guessed at compilation time. However, it happens
that we want to change them at runtime:

- For relocation, i.e. to install GnuCOBOL in a different directory
  from the one in which it was supposed to be.

- For retargetting, i.e. to target a different C compiler or linker
  that the one that was detected at compile time.

Use `gnucobol --settings` to output the current settings. The output
can be copied into a file, that will later be read by GnuCOBOL:

```
$ gnucobol --settings > gnucobol.settings
$ gnucobol --settings=gnucobol.settings
```

You can then modify any option within the file.

A file containing these settings can be specified in 3 different ways,
the first working way in this order will be used:

* Use of the argument `--settings=FILE`
* Use of the environment variable `COBC_SETTINGS`
* Use of `../etc/gnucobol/gnucobol.settings` from the directory
  containing the `cobc` executable

If a file is specified, an error while loading the file is fatal.

Once the settings have been loaded, a few variables can be modified if
the `COBC_IS_RELOCATABLE` setting is set to 1. This setting can also
be overwritten by setting the `COBC_IS_RELOCATABLE` environment
variable. In this case, the following variables are modified
relatively to the directory containing `cobc` (`$bindir`):

* `COB_CFLAGS`: `-I $bindir/../include` is added in front
* `COB_CONFIG_DIR`: set to `$bindir/../share/gnucobol/config`
* `COB_COPY_DIR`: set to `$bindir/../share/gnucobol/copy`
* `COB_LIBRARY_PATH`: set to `$bindir/../lib/gnucobol`
* `COB_LIBS`: `-L$bindir/../lib` is added in front

TODO in codegen.c:
* COB_ALIGN_PRAGMA_8
* USE_INT_HEX
* COB_NON_ALIGNED
* COB_SHORT_BORK
* COB_EBCDIC_MACHINE
* GEN_CHAR_AS_UINT
* GEN_SINGLE_MEMCPY
* NO_INIT_SOURCE_LOC
* COB_64_BIT_POINTER
* COB_TREE_DEBUG
* COBC_HAS_CUTOFF_FLAG

TODO in typeck.c:
* COB_EBCDIC_MACHINE
* COB_NON_ALIGNED
* COB_SHORT_BORK
* COB_ALLOW_UNALIGNED
* COB_64_BIT_POINTER
* WITH_EXTENDED_SCREENIO
* WIN32
* WITH_XML2

TODO in parser.y:
* COB_EBCDIC_MACHINE
* COB_32_BIT_LONG

TODO in ppparse.y:
* #if	' ' == 0x20
* #elif	' ' == 0x40


 */

#ifndef COB_DEBUG_FLAGS
#ifdef	_MSC_VER
#define COB_DEBUG_FLAGS ""
#else
#error		missing definition of COB_DEBUG_FLAGS
#endif
#endif

/* For all boolean options, we must force their existence */
#ifdef _MSC_VER
#define MSC_VER 1
#else
#define MSC_VER 0
#endif

#ifdef __CYGWIN__
#define CYGWIN 1
#else
#define CYGWIN 0
#endif

#ifdef __clang__
#define CLANG 1
#else
#define CLANG 0
#endif

#ifdef _WIN32
#define WIN32 1
#else
#define WIN32 0
#endif

#ifdef __WATCOMC__
#define WATCOMC 1
#else
#define WATCOMC 0
#endif

#ifdef __BORLANDC__
#define BORLANDC 1
#else
#define BORLANDC 0
#endif

#ifdef __arm__
#define ARM 1
#else
#define ARM 0
#endif

#ifdef COB_NON_ALIGNED
#define COB_NON_ALIGNED 1
#else
#define COB_NON_ALIGNED 0
#endif

#if !defined (_GNU_SOURCE) && defined (_XOPEN_SOURCE_EXTENDED)
#define XOPEN_SOURCE_EXTENDED 1
#else
#define XOPEN_SOURCE_EXTENDED 0
#endif

#ifdef COB_KEYWORD_INLINE
#define COB_INLINE_KEYWORD CB_XSTRINGIFY(COB_KEYWORD_INLINE)
#else
#define COB_INLINE_KEYWORD NULL
#endif


#ifndef COBC_IS_RELOCATABLE
#define COBC_IS_RELOCATABLE 0
#endif

#ifndef HAVE_ATTRIBUTE_ALIGNED
#define HAVE_ATTRIBUTE_ALIGNED 0
#endif

#ifndef HAVE_GMP_H
#define HAVE_GMP_H 0
#endif

#ifndef HAVE_MPIR_H
#define HAVE_MPIR_H 0
#endif

#ifndef WORDS_BIGENDIAN
#define WORDS_BIGENDIAN 0
#endif

#ifndef HAVE_ATTRIBUTE_CONSTRUCTOR
#define HAVE_ATTRIBUTE_CONSTRUCTOR 0
#endif

#define STRING_SETTING(help,name) \
	const char* cb_setting_##name = name;
#define BOOL_SETTING(help,name) \
	int cb_setting_##name = name;
#include "settings.def"
#undef STRING_SETTING
#undef BOOL_SETTING

/* string settings */

struct cb_string_setting {
	const char* name ;
	const char* help ;
	const char** value ;
};

static struct cb_string_setting string_settings[] = {
#define STRING_SETTING(help,name) \
	{ #name, help, &cb_setting_##name },
#define BOOL_SETTING(help,name)
#include "settings.def"
#undef STRING_SETTING
#undef BOOL_SETTING
	{ NULL, NULL, NULL }
};

/* boolean settings */

struct cb_bool_setting {
	const char* name ;
	const char* help ;
	int* value ;
};

static struct cb_bool_setting bool_settings[] = {
#define STRING_SETTING(help,name)
#define BOOL_SETTING(help,name) \
	{ #name, help, &cb_setting_##name },
#include "settings.def"
#undef STRING_SETTING
#undef BOOL_SETTING
	{ NULL, NULL, NULL }
};


/* Could be put in a generic library, with cobc_strdup, etc. */
static char*
cobc_concat (const char *s1, const char *s2, const char *s3, const char *s4)
{
	size_t	calcsize = 1;
	char* s;

	if (!s1) {
		return NULL;
	}

	calcsize += strlen (s1) +
		( s2 ? strlen (s2) : 0 ) +
		( s3 ? strlen (s3) : 0 ) +
		( s4 ? strlen (s4) : 0 ) + 1;

	/* LCOV_EXCL_START */
	if (calcsize >= 131072) {
		/* Arbitrary limit */
		cobc_err_exit (_("parameter buffer size exceeded"));
	}
	/* LCOV_EXCL_STOP */
	s = cobc_malloc (calcsize);
	strcat (s, s1);
	if (s2) {
		strcat (s, s2);
	}
	if (s3) {
		strcat (s, s3);
	}
	if (s4) {
		strcat (s, s4);
	}
	return s;
}


static char * cobc_executable_name(void)
{
  char name[COB_NORMAL_BUFF];
#if defined(__linux__)
  struct stat st;
  int retcode;

  retcode = readlink("/proc/self/exe", name, COB_NORMAL_MAX);
  if (retcode == -1 || retcode == COB_NORMAL_MAX) {
	  return NULL;
  }
  name[retcode] = 0;
  /* Make sure that the contents of /proc/self/exe is a regular file.
     (Old Linux kernels return an inode number instead.) */
  if (stat(name, &st) == -1 || ! S_ISREG(st.st_mode)) {
	  return NULL;
  }
  return cobc_strdup (name);

#elif defined(__APPLE__)
  unsigned int namelen;

  namelen = COB_NORMAL_MAX;
  if (_NSGetExecutablePath(name, &namelen) == 0) {
	  return name;
  }
  return NULL;

#else
  return NULL;

#endif
}



void cb_settings_print (void)
{
	printf ("# Current settings:\n");

#define STRING_SETTING(help,name) \
	printf ( "\n# %s\n", help); \
	printf ( #name": %s\n", cb_setting_##name);
#define BOOL_SETTING(help,name) \
	printf ( "\n# %s\n", help); \
	printf ( #name": %s\n", cb_setting_##name ? "1" : "0");
#include "settings.def"
#undef STRING_SETTING
#undef BOOL_SETTING

	printf ("\n");
}

static void really_load_settings (const char *filename){
	char buff[COB_SMALL_BUFF];
	int  line ;
	char *value;
	char *cp;
	FILE *fp;
	struct cb_string_setting *ss = string_settings;
	struct cb_bool_setting *bs = bool_settings;

	fp = fopen (filename, "r");
	if (fp == NULL) {
		cobc_err_exit ("%s: %s", filename, cb_get_strerror ());
	}

	line = 0;
	while (fgets (buff, COB_SMALL_MAX, fp)) {
		line++;
		value = NULL;
		cp = buff;
		if (*cp == '#') continue;
		while (*cp != 0 && *cp != '\r' && *cp != '\n')
		{
			if (!value && *cp == ':' && cp[1] == ' '){
				*cp = 0;
				cp += 2;
				value = cp;
			} else {
				cp++;
			}
		}
		*cp = 0;
		if (*buff == 0 && (!value || *value == 0)) continue;
		if (!value){
			cobc_err_exit ("%s:%d: invalid setting format",
				       filename, line);
		}

		/* For now, we start with a linear search. In the future,
		   we could require the options to be sorted at their
		   definition and use a dichotomy for faster lookup. */

		while (ss->name){
			if (!strcmp(ss->name, buff)){
				*(ss->value) = cobc_strdup (value);
				break;
			}
			ss++;
		}
		if (ss->name) continue;


		while (bs->name){
			if (!strcmp(bs->name, buff)){
				if ( value[0] == '0' && value[1] == 0 ){
					*(bs->value) = 0;
					break;
				} 
				if ( value[0] == '1' && value[1] == 0 ){
					*(bs->value) = 1;
					break;
				}
				cobc_err_exit ("%s:%d: wrong value '%s' for '%s', must be '0' or '1'",
					       filename, line, value, buff);
			}
			bs++;
		}
		if (bs->name) continue;

		cobc_err_exit ("%s:%d: setting '%s' does not exist",
			       filename, line, buff);
	}
	fclose (fp);
}


void cb_settings_load (const char* filename)
{
	char *cobc_dirname = cobc_executable_name ();

	if (cobc_dirname){
		int len = strlen(cobc_dirname);
		while (len>0){
			if (cobc_dirname[len-1] == '/' || cobc_dirname[len-1] == '\\') break;
			len--;
		}
		cobc_dirname[len] = 0;
	}

	if (!filename){
		/* If no settings filename is provided by argument or
		   by env variable, let's check if there is a file
		   relative to the binary position. */
		struct stat st;
		filename = cobc_concat (cobc_dirname,
					"../etc/gnucobol/gnucobol.settings",
					NULL, NULL);
		if (!filename || stat(filename, &st) == -1 || ! S_ISREG(st.st_mode)) {
			filename = NULL;
		}
	}

	if (filename){
		really_load_settings (filename);
	}

	if (cob_getenv_direct ("COBC_IS_RELOCATABLE"))
		cb_setting_COBC_IS_RELOCATABLE = 1;
	if (cb_setting_COBC_IS_RELOCATABLE){
		const char* prefix =
			cobc_concat (cobc_dirname, "..", NULL, NULL);
		const char* prefix_include =
			cb_setting_MSC_VER ?
			"/I \"" :
			cb_setting_WATCOMC ?
			"-i\"" :
			"-I\"";
		const char* prefix_lib =
			cb_setting_MSC_VER ?
			"/LIBPATH:\"" :
			"-L\"";

		cb_setting_COB_CFLAGS =
			cobc_concat (prefix_include,
				     prefix,
				     "/include\" ",
				     cb_setting_COB_CFLAGS);
		cb_setting_COB_CONFIG_DIR =
			cobc_concat (prefix, "/share/gnucobol/config", NULL, NULL);
		cb_setting_COB_COPY_DIR =
			cobc_concat (prefix, "/share/gnucobol/copy", NULL, NULL);
		cb_setting_COB_LIBRARY_PATH =
			cobc_concat (prefix, "/lib/gnucobol", NULL, NULL);
		cb_setting_COB_LIBS =
			cobc_concat (prefix_lib,
				     prefix,
				     "/lib\" ",
				     cb_setting_COB_LIBS);
	}
}

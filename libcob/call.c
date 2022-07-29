/*
   Copyright (C) 2003-2012, 2014-2022 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Simon Sobisch, Ron Norman

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

#ifndef	_GNU_SOURCE
#define _GNU_SOURCE	1
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_FMEMOPEN
#if defined(HAVE_DECL_FMEMOPEN) && HAVE_DECL_FMEMOPEN == 0
/* function available and working, declaration missing on AIX... */
FILE *fmemopen (void *buf, size_t size, const char *mode);
#endif
#endif

#include <errno.h>

/*	NOTE - The following variable should be uncommented when
	it is known that dlopen(NULL) is borked.
	This is known to be true for some PA-RISC HP-UX 11.11 systems.
	This is fixed with HP patch PHSS_28871. (There are newer but this
	fixes dlopen/dlsym problems)
*/
/* #define COB_BORKED_DLOPEN */

#ifdef	_WIN32

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <io.h>	/* for access */

static HMODULE
lt_dlopen (const char *x)
{
	if (x == NULL) {
		return GetModuleHandle (NULL);
	}
	return LoadLibrary(x);
}

static void *
lt_dlsym (HMODULE hmod, const char *p)
{
	union {
		FARPROC		modaddr;
		void		*voidptr;
	} modun;

	modun.modaddr = GetProcAddress(hmod, p);
	return modun.voidptr;
}

#define lt_dlclose(x)	FreeLibrary(x)
#define	lt_dlinit()
#define	lt_dlexit()
#define lt_dlhandle	HMODULE

#if	0	/* RXWRXW - dlerror */
static char	errbuf[64];
static char *
lt_dlerror (void)
{
	sprintf(errbuf, _("LoadLibrary/GetProcAddress error %d"), (int)GetLastError());
	return errbuf;
}
#endif

#elif	defined(USE_LIBDL)
/* note: only defined in configure when HAVE_DLFCN_H is true and dlopen can be linked */
#include <dlfcn.h>

#define lt_dlopen(x)	dlopen(x, RTLD_LAZY | RTLD_GLOBAL)
#define lt_dlsym(x,y)	dlsym(x, y)
#define lt_dlclose(x)	dlclose(x)
#define lt_dlerror()	dlerror()
#define	lt_dlinit()
#define	lt_dlexit()
#define lt_dlhandle	void *

#else

#include <ltdl.h>

#endif

/* Force symbol exports */
#define	COB_LIB_EXPIMP
#include "common.h"
#include "coblocal.h"

#define	COB_MAX_COBCALL_PARMS	16
#define	CALL_BUFF_SIZE		256U
#define	CALL_BUFF_MAX		(CALL_BUFF_SIZE - 1U)

#define HASH_SIZE		131U

/* Call table */
#if	0	/* Alternative hash structure */
#define	COB_ALT_HASH
#endif

struct call_hash {
	struct call_hash	*next;		/* Linked list next pointer */
	const char		*name;		/* Original called name */
	void			*func;		/* Function address */
	cob_module		*module;	/* Program module structure */
	lt_dlhandle		handle;		/* Handle to loaded module */
	const char		*path;		/* Full path of module */
	unsigned int		no_phys_cancel;	/* No physical cancel */
};

struct struct_handle {
	struct struct_handle	*next;		/* Linked list next pointer */
	const char		*path;		/* Path of module */
	lt_dlhandle		handle;		/* Handle to loaded module */
};

struct system_table {
	const char		*syst_name;
	cob_call_union		syst_call;
};

/* Local variables */

#ifdef	COB_ALT_HASH
static struct call_hash		*call_table;
#else
static struct call_hash		**call_table;
#endif

static struct struct_handle	*base_preload_ptr;
static struct struct_handle	*base_dynload_ptr;

static cob_global		*cobglobptr = NULL;
static cob_settings		*cobsetptr = NULL;

static char			**resolve_path;
static char			*resolve_error;
static char			*resolve_alloc;
static char			*resolve_error_buff;
static void			*call_buffer;
static char			*call_filename_buff;

#ifndef	COB_BORKED_DLOPEN
static lt_dlhandle		mainhandle;
#endif

static size_t			call_lastsize;
static size_t			resolve_size = 0;
static unsigned int		cob_jmp_primed;
static cob_field_attr	const_float_attr =
			{COB_TYPE_NUMERIC_DOUBLE, 8, 0, COB_FLAG_HAVE_SIGN, NULL};
static cob_field_attr	const_binll_attr =
			{COB_TYPE_NUMERIC_BINARY, 18, 0, COB_FLAG_HAVE_SIGN, NULL};
static cob_field_attr	const_binull_attr =
			{COB_TYPE_NUMERIC_BINARY, 18, 0, 0, NULL};

#undef	COB_SYSTEM_GEN
#define	COB_SYSTEM_GEN(cob_name, pmin, pmax, c_name)	\
	{ cob_name, {(void *(*)(void *))c_name} },

static const struct system_table	system_tab[] = {
#include "system.def"
	{ NULL, {NULL} }
};
#undef	COB_SYSTEM_GEN

static const unsigned char	hexval[] = "0123456789ABCDEF";

#ifdef	HAVE_DESIGNATED_INITS
static const unsigned char	valid_char[256] = {
	['0'] = 1,
	['1'] = 1,
	['2'] = 1,
	['3'] = 1,
	['4'] = 1,
	['5'] = 1,
	['6'] = 1,
	['7'] = 1,
	['8'] = 1,
	['9'] = 1,
	['A'] = 1,
	['B'] = 1,
	['C'] = 1,
	['D'] = 1,
	['E'] = 1,
	['F'] = 1,
	['G'] = 1,
	['H'] = 1,
	['I'] = 1,
	['J'] = 1,
	['K'] = 1,
	['L'] = 1,
	['M'] = 1,
	['N'] = 1,
	['O'] = 1,
	['P'] = 1,
	['Q'] = 1,
	['R'] = 1,
	['S'] = 1,
	['T'] = 1,
	['U'] = 1,
	['V'] = 1,
	['W'] = 1,
	['X'] = 1,
	['Y'] = 1,
	['Z'] = 1,
	['_'] = 1,
	['a'] = 1,
	['b'] = 1,
	['c'] = 1,
	['d'] = 1,
	['e'] = 1,
	['f'] = 1,
	['g'] = 1,
	['h'] = 1,
	['i'] = 1,
	['j'] = 1,
	['k'] = 1,
	['l'] = 1,
	['m'] = 1,
	['n'] = 1,
	['o'] = 1,
	['p'] = 1,
	['q'] = 1,
	['r'] = 1,
	['s'] = 1,
	['t'] = 1,
	['u'] = 1,
	['v'] = 1,
	['w'] = 1,
	['x'] = 1,
	['y'] = 1,
	['z'] = 1
};
#else
static int					init_valid_char = 1;
static unsigned char		valid_char[256];
static const unsigned char	pvalid_char[] =
	"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz";
#endif

/* Local functions */

static void
set_resolve_error (int module_type)
{
	resolve_error = resolve_error_buff;
	if (module_type == COB_MODULE_TYPE_PROGRAM) {
		cob_set_exception (COB_EC_PROGRAM_NOT_FOUND);
	} else {
		cob_set_exception (COB_EC_FUNCTION_NOT_FOUND);
	}
}

static int last_entry_is_working_directory (const char *buff, const char *pstr)
{
	const size_t pos = pstr - buff;	/* always > 2 */
	if (buff[pos - 1] == '.'
	 && buff[pos - 2] == PATHSEP_CHAR) {
		return 1;
	}
	return 0;
}

/* resolves the actual library path used from
   * COB_LIBRARY_PATH runtime setting
   * "." as current working direktory [if not included already: prefixed]
   * COB_LIBRARY_PATH inbuilt (which normally includes modules
     like CBL_OC_DUMP) [if not included already: appended]
*/
static void
cob_set_library_path ()
{
	char		buff[COB_MEDIUM_BUFF];
	char		*p;
	char		*pstr;
	size_t		i;
	struct stat	st;

	int 		flag;

	/* Clear the previous path */
	if (resolve_path) {
		cob_free (resolve_path);
		cob_free (resolve_alloc);
	}

	/* setup buffer and count number of separators,
	   check for "." */
	i = 1;
	pstr = buff + 2; /* leaving place for prefixing the working directory */
	buff[0] = ' ';
	buff[1] = PATHSEP_CHAR;
	flag = 0;
	if (cobsetptr->cob_library_path != NULL
	 && strcmp (cobsetptr->cob_library_path, ".") != 0) {
		for (p = cobsetptr->cob_library_path; *p; p++, pstr++) {
#ifdef	_WIN32
			if (*p == '/') {
				*pstr = '\\';
				continue;
			}
#else
			if (*p == '\\') {
				*pstr = '/';
				continue;
			}
#endif
			if (*p == PATHSEP_CHAR) {
				i++;
				flag |= last_entry_is_working_directory (buff, pstr);
			}
			*pstr = *p;
		}
		*pstr = PATHSEP_CHAR;
		i++;
		flag |= last_entry_is_working_directory (buff, pstr);
		pstr++;
	}

#ifdef COB_LIBRARY_PATH
	if (COB_LIBRARY_PATH[0] != 0
	 && strcmp (COB_LIBRARY_PATH, ".") != 0) {
		for (p = (char *)COB_LIBRARY_PATH; *p; p++, pstr++) {
#ifdef	_WIN32
			if (*p == '/') {
				*pstr = '\\';
				continue;
			}
#else
			if (*p == '\\') {
				*pstr = '/';
				continue;
			}
#endif
			if (*p == PATHSEP_CHAR) {
				i++;
			}
			*pstr = *p;
		}
	}
#endif
	*pstr = 0;

	/* prefix working directory if missing */
	if (!flag) {
		buff[0] = '.';
		i++;
		p = buff;
	} else {
		p = buff + 2;
	}
	/* Build path array */
	resolve_alloc = cob_strdup (p);
	pstr = resolve_alloc;

	resolve_path = cob_malloc (sizeof (char *) * i);
	resolve_size = 0;

	for (; ; ) {
		p = strtok (pstr, PATHSEP_STR);
		if (!p) {
			break;
		}
		pstr = NULL;

		/* check if directory
		   (note: entries like X:\ _must_ be specified with trailing slash !) */
		if (stat (p, &st) || !(S_ISDIR (st.st_mode))) {
			/* possibly raise a warning, maybe only if explicit asked */
			continue;
		}

		/* remove trailing slash from entry (always added on use) */
		i = strlen (p) - 1;
		if (p[i] == SLASH_CHAR) {
			p[i] = 0;
		}

		/* check if we have this path already */
		flag = 0;
		for (i = 0; i < resolve_size; i++) {
			if (strcmp (resolve_path[i], p) == 0) {
				flag = 1;
				break;
			}
		}

		/* finally: new entry for the resolve path */
		if (flag == 0) {
			resolve_path[resolve_size++] = p;
		}
	}
}

static void
do_cancel_module (struct call_hash *p, struct call_hash **base_hash,
		  struct call_hash *prev)
{
	struct struct_handle	*dynptr;
	int	(*cancel_func)(const int, void *, void *, void *, void *);
	int nocancel;
	nocancel = 0;

	/* FIXME: check the modules entry point and take care of
	   CBL_ERROR_PROC / CBL_EXIT_PROC which defines: If a program containing
	   an exit/error procedure is canceled, the exit/error procedure is removed. */

	if (!p->module) {
		return;
	}
	if (!p->module->module_cancel.funcvoid) {
		return;
	}
	if (p->module->flag_no_phys_canc) {
		nocancel = 1;
	}
	/* This should be impossible */
	/* LCOV_EXCL_START */
	if (p->module->module_active) {
		nocancel = 1;
	}
	/* LCOV_EXCL_STOP */
	if (p->module->module_ref_count
	 && *p->module->module_ref_count) {
		nocancel = 1;
	}
#ifdef _MSC_VER
#pragma warning(suppress: 4113) /* funcint is a generic function prototype */
	cancel_func = p->module->module_cancel.funcint;
#else
	cancel_func = p->module->module_cancel.funcint;
#endif
	(void)cancel_func (-1, NULL, NULL, NULL, NULL);
	p->module = NULL;

	if (nocancel) {
		return;
	}
	if (!cobsetptr->cob_physical_cancel) {
		return;
	}
	if (p->no_phys_cancel) {
		return;
	}
	if (!p->handle) {
		return;
	}

	lt_dlclose (p->handle);

	dynptr = base_dynload_ptr;
	for (; dynptr; dynptr = dynptr->next) {
		if (dynptr->handle == p->handle) {
			dynptr->handle = NULL;
		}
	}

	if (!prev) {
		*base_hash = p->next;
	} else {
		prev->next = p->next;
	}
	if (p->name) {
		cob_free ((void *)(p->name));
	}
	if (p->path) {
		cob_free ((void *)(p->path));
	}
	cob_free (p);
}

static void *
cob_get_buff (const size_t buffsize)
{
	if (buffsize > call_lastsize) {
		call_lastsize = buffsize;
		cob_free (call_buffer);
		call_buffer = cob_fast_malloc (buffsize);
	}
	return call_buffer;
}

static void
cache_dynload (const char *path, lt_dlhandle handle)
{
	struct struct_handle	*dynptr;

	for (dynptr = base_dynload_ptr; dynptr; dynptr = dynptr->next) {
		if (!strcmp (path, dynptr->path)) {
			if (!dynptr->handle) {
				dynptr->handle = handle;
				return;
			}
		}
	}
	dynptr = cob_malloc (sizeof (struct struct_handle));
	dynptr->path = cob_strdup (path);
	dynptr->handle = handle;
	dynptr->next = base_dynload_ptr;
	base_dynload_ptr = dynptr;
}

static void
add_to_preload (const char *path, lt_dlhandle libhandle, struct struct_handle *last_elem)
{
	struct struct_handle *preptr;

	preptr = cob_malloc (sizeof (struct struct_handle));
	preptr->path = cob_strdup (path);
	preptr->handle = libhandle;

#if defined(_WIN32) || defined(__CYGWIN__)
	/*
	 * Observation: dlopen (POSIX) and lt_dlopen (UNIX) are overloading
	 * symbols with equal name. So if we load two libraries with equal
	 * named symbols, the last one wins and is loaded.
	 * LoadLibrary (Win32) ignores any equal named symbol
	 * if another library with this symbol was already loaded.
	 *
	 * In Windows (including MinGW/CYGWIN) we need to load modules
	 * in the same order as we save them to COB_PRE_LOAD due to issues
	 * if we have got two modules with equal entry points.
	 */
	if (last_elem) {
		last_elem->next = preptr;
	} else {
		preptr->next = NULL;
		base_preload_ptr = preptr;
	}
#else
	COB_UNUSED (last_elem);
	preptr->next = base_preload_ptr;
	base_preload_ptr = preptr;
#endif

	if (!cobsetptr->cob_preload_str) {
		cobsetptr->cob_preload_str = cob_strdup(path);
	} else {
		cobsetptr->cob_preload_str = cob_strcat((char*) PATHSEP_STR, cobsetptr->cob_preload_str, 2);
		cobsetptr->cob_preload_str = cob_strcat((char*) path, cobsetptr->cob_preload_str, 2);
	}
}

/* preload module, returns:
   * 0 if not possible,
   * 1 if added (also adds full module path to cob_preload_str in this case)
   * 2 if already preloaded
   * 3 if previously CALLed and not CANCELed, now marked as pre-loaded (no physical cancel)
 */
static size_t
cache_preload (const char *path)
{
	struct struct_handle	*preptr;
	lt_dlhandle		libhandle;
	struct struct_handle	*last_elem = NULL;	/* only set in win32 */

	/* Check for duplicate in pre-load */
	for (preptr = base_preload_ptr; preptr; preptr = preptr->next) {
		if (!strcmp (path, preptr->path)) {
			return 2;
		}
#if defined(_WIN32) || defined(__CYGWIN__)
		/* Save last element of preload list */
		if (!preptr->next) last_elem = preptr;
#endif
	}

	/* Check for duplicate in already loaded programs;
	   this will be empty during initial setup but likely set
	   on calls of cob_try_preload later on (only expected when
	   done via interactive debugger) */
	if (call_buffer
#ifndef	COB_ALT_HASH
	 && call_table) {
		struct call_hash	*p;
		size_t	i;
		for (i = 0; i < HASH_SIZE; ++i) {
			p = call_table[i];
#else
	 ) {
			p = call_table;
#endif
			for (; p;) {
				if ((p->path && !strcmp (path, p->path))
				 || (p->name && !strcmp (path, p->name))) {
					p->no_phys_cancel = 1;
					add_to_preload (path, p->handle, last_elem);
					return 3;
				}
				p = p->next;
			}
		}
#ifndef	COB_ALT_HASH
	}
#endif

	if (access (path, R_OK) != 0) {
		return 0;
	}

	libhandle = lt_dlopen (path);
	if (!libhandle) {
		return 0;
	}

	add_to_preload (path, libhandle, last_elem);

	return 1;
}

#ifndef	COB_ALT_HASH
static COB_INLINE unsigned int
hash (const unsigned char *s)
{
	unsigned int	val = 0;

	while (*s) {
		val += *s++;
	}
	return val % HASH_SIZE;
}
#endif

static void
insert (const char *name, void *func, lt_dlhandle handle,
	cob_module *module, const char *path,
	const unsigned int nocanc)
{
	struct call_hash	*p;
#ifndef	COB_ALT_HASH
	unsigned int		val;
#endif

	p = cob_malloc (sizeof (struct call_hash));
	p->name = cob_strdup (name);
	p->func = func;
	p->handle = handle;
	p->module = module;
	if (path) {
#if	defined(HAVE_CANONICALIZE_FILE_NAME)
		/* Malloced path or NULL */
		p->path = canonicalize_file_name (path);
#elif	defined(HAVE_REALPATH)
		char	*s;

		s = cob_malloc ((size_t)COB_NORMAL_BUFF);
		if (realpath (path, s) != NULL) {
			p->path = cob_strdup (s);
		}
		cob_free (s);
#elif	defined	(_WIN32)
		/* Malloced path or NULL */
		p->path = _fullpath (NULL, path, 1);
#endif
		if (!p->path) {
			p->path = cob_strdup (path);
		}
	}
	p->no_phys_cancel = nocanc;
#ifdef	COB_ALT_HASH
	p->next = call_table;
	call_table = p;
#else
	val = hash ((const unsigned char *)name);
	p->next = call_table[val];
	call_table[val] = p;
#endif
}

static void *
lookup (const char *name)
{
	struct call_hash	*p;

#ifdef	COB_ALT_HASH
	p = call_table;
#else
	p = call_table[hash ((const unsigned char *)name)];
#endif
	for (; p; p = p->next) {
		if (strcmp (name, p->name) == 0) {
			return p->func;
		}
	}
	return NULL;
}

static int
cob_encode_invalid_chars (const unsigned char* const name,
	unsigned char* const name_buff,
	const int buff_size, int *external_pos)
{
	const unsigned char *s = name;
	int pos = *external_pos;

#ifndef	HAVE_DESIGNATED_INITS
	if (init_valid_char) {
		const unsigned char	*pv;
		init_valid_char = 0;
		memset (valid_char, 0, sizeof(valid_char));
		for (pv = pvalid_char; *pv; ++pv) {
			valid_char[*pv] = 1;
		}
	}
#endif

	/* Encode invalid letters */
	for (; *s; ++s) {
		if (pos >= buff_size - 3) {
			name_buff[pos] = 0;
			return -pos;
		}
		if (likely (valid_char[*s])) {
			name_buff[pos++] = *s;
		} else {
			name_buff[pos++] = (unsigned char)'_';
			if (*s == (unsigned char)'-') {
				name_buff[pos++] = (unsigned char)'_';
			} else {
				name_buff[pos++] = hexval[*s / 16U];
				name_buff[pos++] = hexval[*s % 16U];
			}
		}
	}

	*external_pos = pos;
	return pos;
}

/** encode given name
  \param name to encode
  \param name_buff to place the encoded name to (unchanged, when no encoding necessary)
  \param buff_size available
  \param fold_case may be COB_FOLD_UPPER or COB_FOLD_LOWER
  \return size of the encoded name, negative if the buffer size would be exceeded
 */
int
cob_encode_program_id (const unsigned char *const name,
	unsigned char *const name_buff,
	const int buff_size, const int fold_case)
{
	int pos = 0;
	/* Encode the initial digit */
	if (unlikely (isdigit(name[0]))) {
		name_buff[pos++] = (unsigned char)'_';
	}
	/* Encode invalid letters */
	cob_encode_invalid_chars (name, name_buff, buff_size, &pos);

	name_buff[pos] = 0;

	/* Check case folding */
	switch (fold_case) {
	case COB_FOLD_NONE:
		break;
	case COB_FOLD_UPPER:
	{
		unsigned char *p;
		for (p = name_buff; *p; p++) {
			if (islower (*p)) {
				*p = (cob_u8_t)toupper (*p);
			}
		}
		break;
	}
	case COB_FOLD_LOWER:
	{
		unsigned char *p;
		for (p = name_buff; *p; p++) {
			if (isupper (*p)) {
				*p = (cob_u8_t)tolower (*p);
			}
		}
		break;
	}
	default:
		break;
	}
	
	return pos;
}

static void *
cob_resolve_internal (const char *name, const char *dirent,
	const int fold_case, int module_type)
{
	void			*func;
	struct struct_handle	*preptr;
	lt_dlhandle		handle;
	size_t			i;
	char call_entry_buff[COB_MINI_BUFF];	/* entry name, possibly encoded */
	unsigned char call_module_buff[COB_MAX_NAMELEN + 1];
	const unsigned char *s;

	/* LCOV_EXCL_START */
	if (unlikely(!cobglobptr)) {
		cob_fatal_error (COB_FERROR_INITIALIZED);
	}
	/* LCOV_EXCL_STOP */
	cobglobptr->cob_exception_code = 0;

	/* Search the cache */
	func = lookup (name);
	if (func) {
		return func;
	}

	if (strlen (name) > COB_MAX_NAMELEN) {
		/* note: we allow up to COB_MAX_WORDLEN for relaxed syntax... */
		snprintf (resolve_error_buff, (size_t)CALL_BUFF_MAX,
			module_type == COB_MODULE_TYPE_PROGRAM
			? _("%s: PROGRAM name exceeds %d characters")
			: _("%s: FUNCTION name exceeds %d characters"),
			name, COB_MAX_NAMELEN);
		set_resolve_error (module_type);
		return NULL;
	}

	/* Encode program name, including case folding,
	   put to call_entry_buff, may tripple the size */
	cob_encode_program_id ((const unsigned char *)name,
		(unsigned char *)call_entry_buff,
		COB_MINI_MAX, fold_case);

#ifndef	COB_BORKED_DLOPEN
	/* Search the main program */
	if (mainhandle != NULL) {
		/* warning: on GNU systems this will also searched in all
		  currently loaded linked libraries, but possibly not on MSVC! */
		func = lt_dlsym (mainhandle, call_entry_buff);
		if (func != NULL) {
			insert (name, func, mainhandle, NULL, NULL, 1);
			resolve_error = NULL;
			return func;
		}
	}
#endif

	/* Search preloaded modules */
	for (preptr = base_preload_ptr; preptr; preptr = preptr->next) {
		func = lt_dlsym (preptr->handle, call_entry_buff);
		if (func != NULL) {
			insert (name, func, preptr->handle,	NULL, preptr->path, 1);
			resolve_error = NULL;
			return func;
		}
	}

	/* Search dynamic modules */
	for (preptr = base_dynload_ptr; preptr; preptr = preptr->next) {
		if (!preptr->handle) {
			continue;
		}
		func = lt_dlsym (preptr->handle, call_entry_buff);
		if (func != NULL) {
			insert (name, func, preptr->handle,
				NULL, preptr->path, 1);
			resolve_error = NULL;
			return func;
		}
	}

#if	0	/* RXWRXW RTLD */
#if	defined(USE_LIBDL) && defined (RTLD_DEFAULT)
	func = lt_dlsym (RTLD_DEFAULT, call_entry_buff);
	if (func != NULL) {
		insert (name, func, NULL, NULL, NULL, 1);
		resolve_error = NULL;
		return func;
	}
#endif
#endif
	/* Search external modules */
	resolve_error_buff[CALL_BUFF_MAX] = 0;
#ifdef	__OS400__
	strcpy (call_filename_buff, name);
	for (p = call_filename_buff; *p; ++p) {
		*p = (cob_u8_t)toupper(*p);
	}
	handle = lt_dlopen (call_filename_buff);
	if (handle != NULL) {
		/* Candidate for future calls */
		cache_dynload (call_filename_buff, handle);
		func = lt_dlsym (handle, call_entry_buff);
		if (func != NULL) {
			insert (name, func, handle, NULL, call_filename_buff, 0);
			resolve_error = NULL;
			return func;
		}
	}
#else
	/* Check if *module name* needs conversion */
	/* CHECKME: why do we separate this by COB_LOAD_CASE
	   from the entry points, which are a compile-time setup only? */
	if (unlikely (cobsetptr->name_convert != 0)) {
		cob_u8_t* p = call_module_buff;
		for (s = (const unsigned char *)name; *s; ++s, ++p) {
			if (cobsetptr->name_convert == 1 && isupper (*s)) {
				*p = (cob_u8_t)tolower (*s);
			} else if (cobsetptr->name_convert == 2 && islower (*s)) {
				*p = (cob_u8_t)toupper (*s);
			} else {
				*p = *s;
			}
		}
		*p = 0;
		s = call_module_buff;
	} else {
		s = (const unsigned char *)name;
	}

	if (dirent) {
		snprintf (call_filename_buff, (size_t)COB_NORMAL_MAX,
			  "%s%s.%s", dirent, (char *)s, COB_MODULE_EXT);
		call_filename_buff[COB_NORMAL_MAX] = 0;
		if (access (call_filename_buff, R_OK) != 0) {
			snprintf (resolve_error_buff, (size_t)CALL_BUFF_MAX,
				  "module '%s' not found", name);
			set_resolve_error (module_type);
			return NULL;
		}
		handle = lt_dlopen (call_filename_buff);
		if (handle != NULL) {
			/* Candidate for future calls */
			cache_dynload (call_filename_buff, handle);
			func = lt_dlsym (handle, call_entry_buff);
			if (func != NULL) {
				insert (name, func, handle, NULL,
					call_filename_buff, 0);
				resolve_error = NULL;
				return func;
			}
		}
		snprintf (resolve_error_buff, (size_t)CALL_BUFF_MAX,
			  "entry point '%s' not found", (const char *)s);
		set_resolve_error (module_type);
		return NULL;
	}
	for (i = 0; i < resolve_size; ++i) {
		if (resolve_path[i] == NULL) {
			snprintf (call_filename_buff, (size_t)COB_NORMAL_MAX,
				  "%s.%s", (char *)s, COB_MODULE_EXT);
		} else {
			snprintf (call_filename_buff, (size_t)COB_NORMAL_MAX,
				  "%s%c%s.%s", resolve_path[i],
				  SLASH_CHAR, (char *)s, COB_MODULE_EXT);
		}
		call_filename_buff[COB_NORMAL_MAX] = 0;
		if (access (call_filename_buff, R_OK) == 0) {
			handle = lt_dlopen (call_filename_buff);
			if (handle != NULL) {
				/* Candidate for future calls */
				cache_dynload (call_filename_buff, handle);
				func = lt_dlsym (handle, call_entry_buff);
				if (func != NULL) {
					insert (name, func, handle, NULL,
						call_filename_buff, 0);
					resolve_error = NULL;
					return func;
				}
			}
			snprintf (resolve_error_buff, (size_t)CALL_BUFF_MAX,
				  "entry point '%s' not found", (const char *)s);
			set_resolve_error (module_type);
			return NULL;
		}
	}
#endif
	snprintf (resolve_error_buff, (size_t)CALL_BUFF_MAX,
		  "module '%s' not found", name);
	set_resolve_error (module_type);
	return NULL;
}

static const char *
cob_chk_dirp (const char *name)
{
	const char	*p;
	const char	*q;

	q = NULL;
	for (p = name; *p; p++) {
		if (*p == '/' || *p == '\\') {
			q = p + 1;
		}
	}
	if (q) {
		return q;
	}
	return name;
}

static char *
cob_chk_call_path (const char *name, char **dirent)
{
	char	*p;
	char	*q;
	size_t	size1;
	size_t	size2;

	*dirent = NULL;
	q = NULL;
	size2 = 0;
	for (p = (char *)name, size1 = 0; *p; p++, size1++) {
		if (*p == '/' || *p == '\\') {
			q = p + 1;
			size2 = size1 + 1;
		}
	}
	if (q) {
		p = cob_strdup (name);
		p[size2] = 0;
		*dirent = p;
		for (; *p; p++) {
#ifdef	_WIN32
			if (*p == '/') {
				*p = '\\';
			}
#else
			if (*p == '\\') {
				*p = '/';
			}
#endif
		}
		return q;
	}
	return (char *)name;
}

/* Global functions */

const char *
cob_resolve_error (void)
{
	const char	*p;

	if (!resolve_error) {
		p = _("indeterminable error in resolve of COBOL CALL");
	} else {
		p = resolve_error;
		resolve_error = NULL;
	}
	return p;
}

void
cob_call_error (void)
{
	cob_runtime_error ("%s", cob_resolve_error ());
	cob_hard_failure ();
}

void
cob_set_cancel (cob_module *m)
{
	struct call_hash	*p;

#ifdef	COB_ALT_HASH
	p = call_table;
#else
	p = call_table[hash ((const unsigned char *)(m->module_name))];
#endif
	for (; p; p = p->next) {
		if (strcmp (m->module_name, p->name) == 0) {
			p->module = m;
			/* Set path in program module structure */
			if (p->path && m->module_path && !*(m->module_path)) {
				*(m->module_path) = p->path;
			}
			return;
		}
	}
	insert (m->module_name, m->module_entry.funcvoid, NULL, m, NULL, 1);
}

void *
cob_resolve (const char *name)
{
	void	*p;
	char	*entry;
	char	*dirent;

	entry = cob_chk_call_path (name, &dirent);
	p = cob_resolve_internal (entry, dirent, 0, COB_MODULE_TYPE_PROGRAM);
	if (dirent) {
		cob_free (dirent);
	}
	return p;
}

void *
cob_resolve_cobol (const char *name, const int fold_case, const int errind)
{
	void	*p;
	char	*entry;
	char	*dirent;

	entry = cob_chk_call_path (name, &dirent);
	p = cob_resolve_internal (entry, dirent, fold_case, COB_MODULE_TYPE_PROGRAM);
	if (dirent) {
		cob_free (dirent);
	}
	if (unlikely(!p)) {
		if (errind) {
			cob_call_error ();
		}
		cob_set_exception (COB_EC_PROGRAM_NOT_FOUND);
	}
	return p;
}

void *
cob_resolve_func (const char *name)
{
	void	*p;

	p = cob_resolve_internal (name, NULL, 0, COB_MODULE_TYPE_FUNCTION);
	if (unlikely(!p)) {
		/* Note: exception raised above */
		cob_runtime_error (_("user-defined FUNCTION '%s' not found"), name);
		cob_hard_failure ();
	}
	return p;
}

void *
cob_call_field (const cob_field *f, const struct cob_call_struct *cs,
		const unsigned int errind, const int fold_case)
{
	void				*p;
	const struct cob_call_struct	*s;
	const struct system_table	*psyst;
	char				*buff;
	char				*entry;
	char				*dirent;

	/* LCOV_EXCL_START */
	if (unlikely(!cobglobptr)) {
		cob_fatal_error (COB_FERROR_INITIALIZED);
	}
	/* LCOV_EXCL_STOP */

	buff = cob_get_buff (f->size + 1);
	cob_field_to_string (f, buff, f->size);

	/* check for uncommon leading space - trim it */
	if (*buff == ' ') {
		size_t				len;
		/* same warning as in cobc/typeck.c */
		cob_runtime_warning (
			_("'%s' literal includes leading spaces which are omitted"), buff);
		len = strlen (buff);
		while (*buff == ' ') {
			memmove (buff, buff + 1, --len);
		}
		buff[len] = 0;
	}

	entry = cob_chk_call_path (buff, &dirent);

	/* Check if system routine */
	for (psyst = system_tab; psyst->syst_name; ++psyst) {
		if (!strcmp (entry, psyst->syst_name)) {
			if (dirent) {
				cob_free (dirent);
			}
			return psyst->syst_call.funcvoid;
		}
	}


	/* Check if contained program */
	for (s = cs; s && s->cob_cstr_name; s++) {
		if (!strcmp (entry, s->cob_cstr_name)) {
			if (dirent) {
				cob_free (dirent);
			}
			return s->cob_cstr_call.funcvoid;
		}
	}

	p = cob_resolve_internal (entry, dirent, fold_case, COB_MODULE_TYPE_PROGRAM);
	if (dirent) {
		cob_free (dirent);
	}
	if (unlikely(!p)) {
		if (errind) {
			cob_call_error ();
		} else {
			cob_set_exception (COB_EC_PROGRAM_NOT_FOUND);
			return NULL;
		}
	}
	return p;
}

void
cob_cancel (const char *name)
{
	const char		*entry;
	struct call_hash	*p;
	struct call_hash	**q;
	struct call_hash	*r;

	/* LCOV_EXCL_START */
	if (unlikely(!cobglobptr)) {
		cob_fatal_error (COB_FERROR_INITIALIZED);
	}
	if (unlikely(!name)) {
		cob_runtime_error (_("NULL parameter passed to '%s'"), "cob_cancel");
		cob_hard_failure ();
	}
	/* LCOV_EXCL_STOP */

	/* CANCEL ALL (acu extension) */
	if (strcmp (name, "CANCEL ALL") == 0) {
		/* TODO: add list of all modules in CALL (with marker preloaded or not)
		   then when setting the COB_MODULE_PTR via cob_module_global_enter add
		   it also to this new list;
		   then drop in CANCEL and use here canceling non-active COBOL
		   and - for physical cancel only - also the "not COBOL" ones */
		return;
	}

	entry = cob_chk_dirp (name);

#ifdef	COB_ALT_HASH
	q = &call_table;
	p = *q;
#else
	q = &call_table[hash ((const unsigned char *)entry)];
	p = *q;
#endif
	r = NULL;
	for (; p; p = p->next) {
		if (strcmp (entry, p->name) == 0) {
			do_cancel_module (p, q, r);
			return;
		}
		r = p;
	}
}

void
cob_cancel_field (const cob_field *f, const struct cob_call_struct *cs)
{
	char				*name;
	const char			*entry;
	const struct cob_call_struct	*s;

	int	(*cancel_func)(const int, void *, void *, void *, void *);

	/* LCOV_EXCL_START */
	if (unlikely(!cobglobptr)) {
		cob_fatal_error (COB_FERROR_INITIALIZED);
	}
	/* LCOV_EXCL_STOP */
	if (!f || f->size == 0) {
		return;
	}
	name = cob_get_buff (f->size + 1);
	cob_field_to_string (f, name, f->size);
	entry = cob_chk_dirp (name);

	/* Check if contained program */
	for (s = cs; s && s->cob_cstr_name; s++) {
		if (!strcmp (entry, s->cob_cstr_name)) {
			if (s->cob_cstr_cancel.funcvoid) {
#ifdef _MSC_VER
#pragma warning(suppress: 4113) /* funcint is a generic function prototype */
				cancel_func = s->cob_cstr_cancel.funcint;
#else
				cancel_func = s->cob_cstr_cancel.funcint;
#endif
				(void)cancel_func (-1, NULL, NULL, NULL,
						   NULL);
			}
			return;
		}
	}
	cob_cancel (entry);
}

int
cob_call (const char *name, const int argc, void **argv)
{
	void			*pargv[MAX_CALL_FIELD_PARAMS] = { 0 };
	cob_call_union		unifunc;
	int			i;

	/* LCOV_EXCL_START */
	if (unlikely(!cobglobptr)) {
		cob_fatal_error (COB_FERROR_INITIALIZED);
	}
	if (unlikely(!name)) {
		cob_runtime_error (_("NULL parameter passed to '%s'"), "cob_call");
		cob_hard_failure ();
	}
	if (argc < 0 || argc > MAX_CALL_FIELD_PARAMS) {
		cob_runtime_error (_("invalid number of arguments passed to '%s'"), "cob_call");
		cob_hard_failure ();
	}
	/* LCOV_EXCL_STOP */
	unifunc.funcvoid = cob_resolve_cobol (name, 0, 1);
	/* Set number of parameters */
	cobglobptr->cob_call_params = argc;
	for (i = 0; i < argc; ++i) {
		pargv[i] = argv[i];
	}
#if	MAX_CALL_FIELD_PARAMS == 16 || \
	MAX_CALL_FIELD_PARAMS == 36 || \
	MAX_CALL_FIELD_PARAMS == 56 || \
	MAX_CALL_FIELD_PARAMS == 76 || \
	MAX_CALL_FIELD_PARAMS == 96 || \
    MAX_CALL_FIELD_PARAMS == 192 || \
    MAX_CALL_FIELD_PARAMS == 252
#else
#error	"Invalid MAX_CALL_FIELD_PARAMS value"
#endif
	i =  unifunc.funcint (pargv[0], pargv[1], pargv[2], pargv[3]
				,pargv[4], pargv[5], pargv[6], pargv[7]
				,pargv[8], pargv[9], pargv[10], pargv[11]
				,pargv[12], pargv[13], pargv[14], pargv[15]
#if	MAX_CALL_FIELD_PARAMS > 16
				,pargv[16], pargv[17], pargv[18], pargv[19]
				,pargv[20], pargv[21], pargv[22], pargv[23]
				,pargv[24], pargv[25], pargv[26], pargv[27]
				,pargv[28], pargv[29], pargv[30], pargv[31]
				,pargv[32], pargv[33], pargv[34], pargv[35]
#if	MAX_CALL_FIELD_PARAMS > 36
				,pargv[36], pargv[37], pargv[38], pargv[39]
				,pargv[40], pargv[41], pargv[42], pargv[43]
				,pargv[44], pargv[45], pargv[46], pargv[47]
				,pargv[48], pargv[49], pargv[50], pargv[51]
				,pargv[52], pargv[53], pargv[54], pargv[55]
#if	MAX_CALL_FIELD_PARAMS > 56
				,pargv[56], pargv[57], pargv[58], pargv[59]
				,pargv[60], pargv[61], pargv[62], pargv[63]
				,pargv[64], pargv[65], pargv[66], pargv[67]
				,pargv[68], pargv[69], pargv[70], pargv[71]
				,pargv[72], pargv[73], pargv[74], pargv[75]
#if	MAX_CALL_FIELD_PARAMS > 76
				,pargv[76], pargv[77], pargv[78], pargv[79]
				,pargv[80], pargv[81], pargv[82], pargv[83]
				,pargv[84], pargv[85], pargv[86], pargv[87]
				,pargv[88], pargv[89], pargv[90], pargv[91]
				,pargv[92], pargv[93], pargv[94], pargv[95]
#if	MAX_CALL_FIELD_PARAMS > 96
				,pargv[96], pargv[97], pargv[98], pargv[99]
				,pargv[100], pargv[101], pargv[102], pargv[103]
				,pargv[104], pargv[105], pargv[106], pargv[107]
				,pargv[108], pargv[109], pargv[110], pargv[111]
				,pargv[112], pargv[113], pargv[114], pargv[115]
				,pargv[116], pargv[117], pargv[118], pargv[119]
				,pargv[120], pargv[121], pargv[122], pargv[123]
				,pargv[124], pargv[125], pargv[126], pargv[127]
				,pargv[128], pargv[129], pargv[130], pargv[131]
				,pargv[132], pargv[133], pargv[134], pargv[135]
				,pargv[136], pargv[137], pargv[138], pargv[139]
				,pargv[140], pargv[141], pargv[142], pargv[143]
				,pargv[144], pargv[145], pargv[146], pargv[147]
				,pargv[148], pargv[149], pargv[130], pargv[131]
				,pargv[152], pargv[153], pargv[154], pargv[155]
				,pargv[160], pargv[161], pargv[162], pargv[163]
				,pargv[164], pargv[165], pargv[166], pargv[167]
				,pargv[168], pargv[169], pargv[170], pargv[171]
				,pargv[172], pargv[173], pargv[174], pargv[175]
				,pargv[176], pargv[177], pargv[178], pargv[179]
				,pargv[180], pargv[181], pargv[182], pargv[183]
				,pargv[184], pargv[185], pargv[186], pargv[187]
				,pargv[188], pargv[189], pargv[190], pargv[191]
#if	MAX_CALL_FIELD_PARAMS > 192
				,pargv[192], pargv[193], pargv[194], pargv[195]
				,pargv[200], pargv[201], pargv[202], pargv[203]
				,pargv[204], pargv[205], pargv[206], pargv[207]
				,pargv[208], pargv[209], pargv[210], pargv[211]
				,pargv[212], pargv[213], pargv[214], pargv[215]
				,pargv[216], pargv[217], pargv[218], pargv[219]
				,pargv[220], pargv[221], pargv[222], pargv[223]
				,pargv[224], pargv[225], pargv[226], pargv[227]
				,pargv[228], pargv[229], pargv[230], pargv[231]
				,pargv[232], pargv[233], pargv[234], pargv[235]
				,pargv[240], pargv[241], pargv[242], pargv[243]
				,pargv[244], pargv[245], pargv[246], pargv[247]
				,pargv[248], pargv[249], pargv[250], pargv[251]
#endif
#endif
#endif
#endif
#endif
#endif
				);
	return i;
}

int
cob_func (const char *name, const int argc, void **argv)
{
	int	ret;

	ret = cob_call (name, argc, argv);
	cob_cancel (name);
	return ret;
}

#ifndef COB_WITHOUT_JMP
/* save jump structure, normally called by cobsetjmp which wraps it into setjmp */
void *
cob_savenv (struct cobjmp_buf *jbuf)
{
	/* LCOV_EXCL_START */
	if (unlikely(!cobglobptr)) {
		cob_fatal_error (COB_FERROR_INITIALIZED);
	}
	if (unlikely(!jbuf)) {
		cob_runtime_error (_("NULL parameter passed to '%s'"), "cob_savenv");
		cob_hard_failure ();
	}
	if (cob_jmp_primed) {
		cob_runtime_error (_("multiple call to 'cob_setjmp'"));
		cob_hard_failure ();
	}
	/* LCOV_EXCL_STOP */
	cob_jmp_primed = 1;
	return jbuf->cbj_jmp_buf;
}

void *
cob_savenv2 (struct cobjmp_buf *jbuf, const int jsize)
{
	COB_UNUSED (jsize);
	return cob_savenv (jbuf);
}

void
cob_longjmp (struct cobjmp_buf *jbuf)
{
	/* LCOV_EXCL_START */
	if (unlikely(!cobglobptr)) {
		cob_fatal_error (COB_FERROR_INITIALIZED);
	}
	if (unlikely(!jbuf)) {
		cob_runtime_error (_("NULL parameter passed to '%s'"), "cob_longjmp");
		cob_hard_failure ();
	}
	if (!cob_jmp_primed) {
		cob_runtime_error (_("call to 'cob_longjmp' with no prior 'cob_setjmp'"));
		cob_hard_failure ();
	}
	/* LCOV_EXCL_STOP */
	cob_jmp_primed = 0;
	longjmp (jbuf->cbj_jmp_buf, 1);
}
#endif

void
cob_exit_call (void)
{
	struct struct_handle	*h;
	struct struct_handle	*j;

	if (call_filename_buff) {
		cob_free (call_filename_buff);
		call_filename_buff = NULL;
	}
	if (call_buffer) {
		cob_free (call_buffer);
		call_buffer = NULL;
	}
	if (resolve_error_buff) {
		cob_free (resolve_error_buff);
		resolve_error_buff = NULL;
	}
	if (resolve_alloc) {
		cob_free (resolve_alloc);
		resolve_alloc = NULL;
	}
	if (resolve_path) {
		cob_free (resolve_path);
		resolve_path = NULL;
		resolve_size = 0;
	}

#ifndef	COB_ALT_HASH
	if (call_table) {
		struct call_hash	*p;
		struct call_hash	*q;
		size_t			i;
		for (i = 0; i < HASH_SIZE; ++i) {
			p = call_table[i];
#else
			p = call_table;
#endif
			for (; p;) {
				q = p;
				p = p->next;
				if (q->name) {
					cob_free ((void *)q->name);
				}
				if (q->path) {
					cob_free ((void *)q->path);
				}
				cob_free (q);
			}
#ifndef	COB_ALT_HASH
		}
		if (call_table) {
			cob_free (call_table);
		}
		call_table = NULL;
	}
#endif

	for (h = base_preload_ptr; h;) {
		j = h;
		if (h->path) {
			cob_free ((void *)h->path);
		}
		if (h->handle) {
			lt_dlclose (h->handle);
		}
		h = h->next;
		cob_free (j);
	}
	base_preload_ptr = NULL;
	for (h = base_dynload_ptr; h;) {
		j = h;
		if (h->path) {
			cob_free ((void *)h->path);
		}
		if (h->handle) {
			lt_dlclose (h->handle);
		}
		h = h->next;
		cob_free (j);
	}
	base_dynload_ptr = NULL;

#if	!defined(_WIN32) && !defined(USE_LIBDL)
	lt_dlexit ();
#if	0	/* RXWRXW - ltdl leak */
#ifndef	COB_BORKED_DLOPEN
	/* Weird - ltdl leaks mainhandle - This appears to work but .. */
	if (mainhandle) {
		cob_free (mainhandle);
	}
#endif
#endif
#endif

}

/* try to load specified module from all entries in COB_LIBRARY_PATH
   return values see cache_preload */
static
size_t cob_try_preload (const char* module_name)
{
	char		buff[COB_MEDIUM_BUFF];

#ifdef	__OS400__
	char	*b = buff;
	char	*t;

	for (t = module_name; *t; ++t, ++b) {
		*b = toupper (*t);
	}
	*b = 0;

	return cache_preload (buff);
#else
	size_t				i, ret;

	for (i = 0; i < resolve_size; ++i) {
		snprintf (buff, (size_t)COB_MEDIUM_MAX,
			"%s%c%s.%s",
			resolve_path[i], SLASH_CHAR, module_name, COB_MODULE_EXT);
		ret = cache_preload (buff);
		if (ret) {
			return ret;
		}
	}
	/* If not found, try just using the name as-is */
	return cache_preload (module_name);
#endif
}

void
cob_init_call (cob_global *lptr, cob_settings* sptr, const int check_mainhandle)
{
#ifndef	HAVE_DESIGNATED_INITS
	const unsigned char		*pv;
#endif

	cobglobptr = lptr;
	cobsetptr = sptr;

	base_preload_ptr = NULL;
	base_dynload_ptr = NULL;
	resolve_path = NULL;
	resolve_alloc = NULL;
	resolve_error = NULL;
	call_buffer = NULL;
	call_lastsize = 0;
	cob_jmp_primed = 0;

#ifndef	HAVE_DESIGNATED_INITS
	init_valid_char = 0;
	memset (valid_char, 0, sizeof(valid_char));
	for (pv = pvalid_char; *pv; ++pv) {
		valid_char[*pv] = 1;
	}
#endif

	/* Big enough for anything from libdl/libltdl */
	resolve_error_buff = cob_malloc ((size_t)CALL_BUFF_SIZE);

#ifndef	COB_ALT_HASH
	call_table = cob_malloc (sizeof (struct call_hash *) * HASH_SIZE);
#else
	call_table = NULL;
#endif

	/* set static vars resolve_path (data in resolve_alloc) and resolve_size */
	cob_set_library_path ();

	lt_dlinit ();

#ifndef	COB_BORKED_DLOPEN
	/* only set main handle if not started by cobcrun as this
	   saves a check for exported functions in every CALL
	*/
	if (check_mainhandle) {
		mainhandle = lt_dlopen (NULL);
	} else {
		mainhandle = NULL;
	}
#endif

	call_filename_buff = cob_malloc ((size_t)COB_NORMAL_BUFF);

	if (cobsetptr->cob_preload_str != NULL) {
		char	*p;
		char	*s;

		/* using temporary buffer as cob_preload_str
		   will be adjusted in the call and contains only
		   the loaded modules after this call */
		p = cob_strdup (cobsetptr->cob_preload_str);

		cob_free (cobsetptr->cob_preload_str);
		cobsetptr->cob_preload_str = NULL;

		s = strtok (p, PATHSEP_STR);
		for (; s; s = strtok (NULL, PATHSEP_STR)) {
			(void)cob_try_preload (s);
		}
		cob_free (p);
	}
	call_buffer = cob_fast_malloc ((size_t)CALL_BUFF_SIZE);
	call_lastsize = CALL_BUFF_SIZE;
}

/******************************************
 * Routines for C interface with COBOL
 */

cob_field *
cob_get_param_field (int n, const char *caller_name)
{
	if (cobglobptr == NULL
	 || COB_MODULE_PTR == NULL) {
		/* note: same message in call.c */
		cob_runtime_warning_external (caller_name, 1,
			_("cob_init() has not been called"));
		return NULL;
	}
	if (n < 1
	 || n > cobglobptr->cob_call_params) {
		cob_runtime_warning_external (caller_name, 1,
			_("parameter %d is not within range of %d"),
			n, cobglobptr->cob_call_params);
		return NULL;
	}
	if (COB_MODULE_PTR->cob_procedure_params[n - 1] == NULL) {
		cob_runtime_warning_external (caller_name, 1,
			_("parameter %d is NULL"), n);
		return NULL;
	}
	return COB_MODULE_PTR->cob_procedure_params[n - 1];
}

int
cob_get_num_params (void)
{
	if (cobglobptr) {
		return cobglobptr->cob_call_params;
	}

	/* note: same message in call.c */
	cob_runtime_warning_external ("cob_get_num_params", 1,
		_("cob_init() has not been called"));
	return -1;
}

int
cob_get_param_type (int n)
{
	cob_field	*f = cob_get_param_field (n, "cob_get_param_type");
	return cob_get_field_type (f);
}

int
cob_get_param_size (int n)
{
	cob_field	*f = cob_get_param_field (n, "cob_get_param_size");
	return cob_get_field_size (f);
}

int
cob_get_param_sign (int n)
{
	cob_field	*f = cob_get_param_field (n, "cob_get_param_sign");
	return cob_get_field_sign (f);
}

int
cob_get_param_scale (int n)
{
	cob_field	*f = cob_get_param_field (n, "cob_get_param_scale");
	return cob_get_field_scale (f);
}

int
cob_get_param_digits (int n)
{
	cob_field	*f = cob_get_param_field (n, "cob_get_param_digits");
	return cob_get_field_digits (f);
}

int
cob_get_param_constant (int n)
{
	cob_field	*f = cob_get_param_field (n, "cob_get_param_constant");
	return cob_get_field_constant (f);
}

const char *
cob_get_param_str (int n, char *buffer, size_t size)
{
	cob_field	*f = cob_get_param_field (n, "cob_get_param_str");
	return cob_get_field_str (f, buffer, size);
}

const char *
cob_get_param_str_buffered (int n)
{
	cob_field	*f = cob_get_param_field (n, "cob_get_param_str_buffered");
	return cob_get_field_str_buffered (f);
}

int
cob_put_param_str (int n, const char *str)
{
	cob_field	*f = cob_get_param_field (n, "cob_put_param_str");
	return cob_put_field_str (f, str);
}

void *
cob_get_param_data (int n)
{
	cob_field	*f = cob_get_param_field (n, "cob_get_param_data");
	if (f == NULL) {
		return NULL;
	}
	return (void*)f->data;
}

double
cob_get_dbl_param (int n)
{
	void		*cbl_data;
	double		val;
	cob_field	temp;
	cob_field_attr   float_attr;
	cob_field	*f = cob_get_param_field (n, "cob_get_dbl_param");

	if (f == NULL) {
		return (double)-1;
	}
	cbl_data = f->data;

	switch (f->attr->type) {
	case COB_TYPE_NUMERIC_FLOAT:
		return (double)cob_get_comp1 (cbl_data);
	case COB_TYPE_NUMERIC_DOUBLE:
		return (double)cob_get_comp2 (cbl_data);
	default:
		memcpy(&float_attr, &const_float_attr, sizeof(cob_field_attr));
		float_attr.scale = f->attr->scale;
		temp.size = 8;
		temp.data = (unsigned char *)&val;
		temp.attr = &float_attr;
		cob_move (f, &temp);
		return (double)val;
	}
}

void
cob_put_dbl_param (int n, double val)
{
	void		*cbl_data;
	cob_field	temp;
	cob_field_attr   float_attr;
	cob_field	*f = cob_get_param_field (n, "cob_get_dbl_param");

	if (f == NULL) {
		return;
	}
	cbl_data = f->data;

	switch (f->attr->type) {
	case COB_TYPE_NUMERIC_FLOAT:
		cob_put_comp1 ((float)val, cbl_data);
		return;
	case COB_TYPE_NUMERIC_DOUBLE:
		cob_put_comp2 (val, cbl_data);
		return;
	default:
		memcpy(&float_attr, &const_float_attr, sizeof(cob_field_attr));
		float_attr.scale = f->attr->scale;
		temp.size = 8;
		temp.data = (unsigned char *)&val;
		temp.attr = &float_attr;
		cob_move (&temp, f);
		return;
	}
}

cob_s64_t
cob_get_s64_param (int n)
{
	void		*cbl_data;
	int		size;
	cob_s64_t	val;
	double		dbl;
	cob_field	temp;
	cob_field	*f = cob_get_param_field (n, "cob_get_s64_param");

	if (f == NULL) {
		return -1;
	}
	cbl_data = f->data;
	size = f->size;

	switch (f->attr->type) {
	case COB_TYPE_NUMERIC_DISPLAY:
		return cob_get_s64_pic9 (cbl_data, size);
	case COB_TYPE_NUMERIC_BINARY:
#ifndef WORDS_BIGENDIAN
		if (!COB_FIELD_BINARY_SWAP (f)) {
			return cob_get_s64_comp5 (cbl_data, size);
		}
#endif
		return cob_get_s64_compx (cbl_data, size);
	case COB_TYPE_NUMERIC_PACKED:
		return cob_get_s64_comp3 (cbl_data, size);
	case COB_TYPE_NUMERIC_FLOAT:
		dbl = cob_get_comp1 (cbl_data);
		val = (cob_s64_t)dbl; /* possible data loss is explicit requested */
		return val;
	case COB_TYPE_NUMERIC_DOUBLE:
		dbl = cob_get_comp2 (cbl_data);
		val = (cob_s64_t)dbl; /* possible data loss is explicit requested */
		return val;
	case COB_TYPE_NUMERIC_EDITED:
		return cob_get_s64_pic9 (cbl_data, size);
	default:
		temp.size = 8;
		temp.data = (unsigned char *)&val;
		temp.attr = &const_binll_attr;
		const_binll_attr.scale = f->attr->scale;
		cob_move (f, &temp);
		return val;
	}
}

cob_u64_t
cob_get_u64_param (int n)
{
	void		*cbl_data;
	int		size;
	cob_u64_t	val;
	double		dbl;
	cob_field	temp;
	cob_field	*f = cob_get_param_field (n, "cob_get_u64_param");

	if (f == NULL) {
		return 0;
	}

	cbl_data = f->data;
	size    = f->size;
	switch (COB_MODULE_PTR->cob_procedure_params[n - 1]->attr->type) {
	case COB_TYPE_NUMERIC_DISPLAY:
		return cob_get_u64_pic9 (cbl_data, size);
	case COB_TYPE_NUMERIC_BINARY:
#ifndef WORDS_BIGENDIAN
		if (!COB_FIELD_BINARY_SWAP (f)) {
			return cob_get_u64_comp5 (cbl_data, size);
		}
#endif
		return cob_get_u64_compx (cbl_data, size);
	case COB_TYPE_NUMERIC_PACKED:
		return cob_get_u64_comp3 (cbl_data, size);
	case COB_TYPE_NUMERIC_FLOAT:
		dbl = cob_get_comp1 (cbl_data);
		val = (cob_u64_t)dbl; /* possible data loss is explicit requested */
		return val;
	case COB_TYPE_NUMERIC_DOUBLE:
		dbl = cob_get_comp2 (cbl_data);
		val = (cob_u64_t)dbl; /* possible data loss is explicit requested */
		return val;
	case COB_TYPE_NUMERIC_EDITED:
		return cob_get_u64_pic9 (cbl_data, size);
	default:
		temp.size = 8;
		temp.data = (unsigned char *)&val;
		temp.attr = &const_binull_attr;
		const_binull_attr.scale = f->attr->scale;
		cob_move (f, &temp);
		return val;
	}
}

char *
cob_get_picx_param (int n, void *char_field, size_t char_len)
{
	cob_field	*f = cob_get_param_field (n, "cob_get_picx_param");
	if (f == NULL) {
		return NULL;
	}
	return cob_get_picx (f->data, f->size, char_field, char_len);
}

int
cob_get_field_type (const cob_field *f)
{
	if (f == NULL) {
		return -1;
	}
	if (f->attr->type == COB_TYPE_NUMERIC_BINARY) {
		if (COB_FIELD_REAL_BINARY (f)) {
			return COB_TYPE_NUMERIC_COMP5;
		}
#ifndef WORDS_BIGENDIAN
		if (!COB_FIELD_BINARY_SWAP (f)) {
			return COB_TYPE_NUMERIC_COMP5;
		}
#endif
	}
	return (int)f->attr->type;
}

int
cob_get_field_size (const cob_field *f)
{
	if (f == NULL) {
		return -1;
	}
	return (int)f->size;
}

int
cob_get_field_sign (const cob_field *f)
{
	if (f == NULL) {
		return -1;
	}
	return COB_FIELD_HAVE_SIGN (f);
}

int
cob_get_field_scale (const cob_field *f)
{
	if (f == NULL) {
		return -1;
	}
	return (int)f->attr->scale;
}

int
cob_get_field_digits (const cob_field *f)
{
	if (f == NULL) {
		return -1;
	}
	return (int)f->attr->digits;
}

int
cob_get_field_constant (const cob_field *f)
{
	if (f == NULL) {
		return -1;
	}
	return COB_FIELD_CONSTANT (f);
}

const char *
cob_get_field_str (const cob_field *f, char *buffer, size_t size)
{
	if (unlikely (f == NULL)) {
		return _("NULL field");
	}
	/* variable field's and empty literals may be of zero size */
	if (unlikely (f->size == 0)) {
		return "";
	}
	/* check if field has data assigned (may be a BASED / LINKAGE item) */
	if (unlikely (f->data == NULL)) {
		return _("field not allocated");
	}
	if (!buffer || !size) {
		cob_runtime_warning_external ("cob_get_field_str", 0, "bad buffer/size");
		return "";
	}
	{
		FILE *fp;
#ifdef HAVE_FMEMOPEN
		fp = fmemopen (buffer, size, "w");
#else
		fp = cob_create_tmpfile ("display");
#endif
		if (fp) {
			unsigned char pretty = COB_MODULE_PTR->flag_pretty_display;
			COB_MODULE_PTR->flag_pretty_display = 1;
			cob_display_common (f, fp);
			COB_MODULE_PTR->flag_pretty_display = pretty;
#ifndef HAVE_FMEMOPEN
			{
				int cur_pos = ftell (fp);
				if (cur_pos >= 0) {
					size_t	pos = (size_t) cur_pos;
					fseek (fp, 0, SEEK_SET);
					fread ((void*)buffer, 1, pos, fp);
					if (size > pos) buffer[pos] = 0;
				}
			}
#endif
			fclose (fp);
		}
	}
	return buffer;
}

const char *
cob_get_field_str_buffered (const cob_field *f)
{
	char	*buff = NULL;
	size_t	size = cob_get_field_size (f) + 1;

	if (size > 0) {
		if (size < 32) {
			size = 32;
		}
		buff = cob_get_buff (size);
	}
	return cob_get_field_str (f, buff, size);
}

int
cob_put_field_str (const cob_field *dst, const char *str)
{
	const cob_field_attr	const_alpha_attr =
			{COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
	cob_field wrk;

	if (!dst ||!str) return EINVAL;

	/* come back later for DYNAMIC LENGTH fields */
	if (dst->size <= 0) return EINVAL;

	if (COB_FIELD_CONSTANT (dst)) {
		cob_runtime_warning_external ("cob_put_field_str", 0,
			_ ("attempt to over-write constant field with '%s'"),
			str);
		return EINVAL;
	}


	wrk.attr = &const_alpha_attr;
	wrk.size = strlen (str);
	wrk.data = (unsigned char *)str;

	if (COB_FIELD_IS_NUMERIC (dst)) {
		if (COB_FIELD_TYPE (dst) & COB_TYPE_NUMERIC_FLOAT
		 || COB_FIELD_TYPE (dst) & COB_TYPE_NUMERIC_DOUBLE) {
			if (cob_check_numval_f (&wrk)) return 1;
			wrk = *cob_intr_numval_f (&wrk);
		} else {
			if (cob_check_numval (&wrk, NULL, 0, 1)) return 1;
			wrk = *cob_intr_numval (&wrk);
		}
	}
	cob_move (&wrk, (cob_field *)dst);
	return 0;
}

void
cob_put_s64_param (int n, cob_s64_t val)
{
	void		*cbl_data;
	int		size;
	float		flt;
	double		dbl;
	cob_field	temp;
	cob_field	*f = cob_get_param_field (n, "cob_put_s64_param");

	if (f == NULL) {
		return;
	}

	if (COB_FIELD_CONSTANT (f)) {
		cob_runtime_warning_external ("cob_put_s64_param", 1,
			_("attempt to over-write constant parameter %d with " CB_FMT_LLD),
			n, val);
		return;
	}
	cbl_data = f->data;
	size = f->size;
	switch (f->attr->type) {
	case COB_TYPE_NUMERIC_DISPLAY:
		cob_put_s64_pic9 (val, cbl_data, size);
		return;
	case COB_TYPE_NUMERIC_BINARY:
#ifndef WORDS_BIGENDIAN
		if (!COB_FIELD_BINARY_SWAP (f)) {
			cob_put_s64_comp5 (val, cbl_data, size);
			return;
		}
#endif
		cob_put_s64_compx (val, cbl_data, size);
		return;
	case COB_TYPE_NUMERIC_PACKED:
		cob_put_s64_comp3 (val, cbl_data, size);
		return;
	case COB_TYPE_NUMERIC_FLOAT:
		flt = (float)val;  /* possible data loss is explicit requested */
		cob_put_comp1 (flt, cbl_data);
		return;
	case COB_TYPE_NUMERIC_DOUBLE:
		dbl = (double)val; /* possible data loss is explicit requested */
		cob_put_comp2 (dbl, cbl_data);
		return;
	default:	/* COB_TYPE_NUMERIC_EDITED, ... */
		temp.size = 8;
		temp.data = (unsigned char *)&val;
		temp.attr = &const_binll_attr;
		const_binll_attr.scale = f->attr->scale;
		cob_move (&temp, f);
		return;
	}
}

void
cob_put_u64_param (int n, cob_u64_t val)
{
	void		*cbl_data;
	int		size;
	float		flt;
	double		dbl;
	cob_field	temp;
	cob_field	*f = cob_get_param_field (n, "cob_put_u64_param");

	if (f == NULL) {
		return;
	}

	if (COB_FIELD_CONSTANT (f)) {
		cob_runtime_warning_external ("cob_put_u64_param", 1,
			_("attempt to over-write constant parameter %d with " CB_FMT_LLD),
			n, val);
		return;
	}
	cbl_data = f->data;
	size = f->size;
	switch (f->attr->type) {
	case COB_TYPE_NUMERIC_DISPLAY:
		cob_put_u64_pic9 (val, cbl_data, size);
		return;
	case COB_TYPE_NUMERIC_BINARY:
#ifndef WORDS_BIGENDIAN
		if (!COB_FIELD_BINARY_SWAP (f)) {
			cob_put_u64_comp5 (val, cbl_data, size);
			return;
		}
#endif
		cob_put_u64_compx (val, cbl_data, size);
		return;
	case COB_TYPE_NUMERIC_PACKED:
		cob_put_u64_comp3 (val, cbl_data, size);
		return;
	case COB_TYPE_NUMERIC_FLOAT:
		flt = (float)val;  /* possible data loss is explicit requested */
		cob_put_comp1 (flt, cbl_data);
		return;
	case COB_TYPE_NUMERIC_DOUBLE:
		dbl = (double)val;  /* possible data loss is explicit requested */
		cob_put_comp2 (dbl, cbl_data);
		return;
	default:	/* COB_TYPE_NUMERIC_EDITED, ... */
		temp.size = 8;
		temp.data = (unsigned char *)&val;
		temp.attr = &const_binll_attr;
		const_binll_attr.scale = f->attr->scale;
		cob_move (&temp, f);
		return;
	}
}

void
cob_put_picx_param (int n, void *char_field)
{
	cob_field	*f = cob_get_param_field (n, "cob_put_picx_param");

	if (f == NULL || char_field == NULL) {
		return;
	}

	if (COB_FIELD_CONSTANT (f)) {
		cob_runtime_warning_external ("cob_put_picx_param", 1,
			_("attempt to over-write constant parameter %d with '%s'"),
			n, (char*)char_field);
		return;
	}

	cob_put_picx (f->data, f->size, char_field);
}

void *
cob_get_grp_param (int n, void *char_field, size_t len)
{
	cob_field	*f = cob_get_param_field (n, "cob_get_grp_param");

	if (f == NULL) {
		return NULL;
	}
	if (len == 0) {
		len = f->size;
	}

	if (char_field == NULL) {
		if (len < f->size) {
			len = f->size;
		}
		char_field = cob_malloc (len);
	}
	memcpy (char_field, f->data, f->size);
	return char_field;
}

void
cob_put_grp_param (int n, void *char_field, size_t len)
{
	cob_field	*f = cob_get_param_field (n, "cob_put_grp_param");

	if (f == NULL || char_field == NULL) {
		return;
	}

	if (COB_FIELD_CONSTANT (f)) {
		cob_runtime_warning_external ("cob_put_grp_param", 1,
			"attempt to over-write constant parameter %d", n);
		return;
	}

	if (len == 0 || len > f->size) {
		len = f->size;
	}
	memcpy (f->data, char_field, len);
}

/* Create copy of field and mark as a CONSTANT */
void
cob_field_constant (cob_field *f, cob_field *t, cob_field_attr *a, void *d)
{
	memcpy((void*)t, (void*)f, sizeof(cob_field));
	memcpy((void*)a, (void*)f->attr, sizeof(cob_field_attr));
	t->data = d;
	t->attr = a;
	a->flags |= COB_FLAG_CONSTANT;
	memcpy((void*)t->data, (void*)f->data, f->size);
}

/*
   Copyright (C) 2002-2012, 2014-2020 Free Software Foundation, Inc.
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

/* this file handles the obsolete OpenCOBOL external file handlers */

/* Force symbol exports */
#define	COB_LIB_EXPIMP
#include "fileio.h"

#if defined(WITH_INDEX_EXTFH) || defined(WITH_SEQRA_EXTFH)
static int extfh_dummy () { return 91; }
static struct cob_fileio_funcs **fileio_funcs = NULL;
static const char *io_rtn_name[COB_IO_MAX+1] = {
	"SEQUENTIAL",
	"LINE SEQUENTIAL",
	"RELATIVE",
	"CISAM",
	"DISAM",
	"VBISAM",
	"BDB",
	"VISAM",
	"IXEXT",
	"SQEXT",
	"RLEXT",
	"ODBC",
	"OCI",
	"LMDB",
	""
};

static COB_INLINE int
get_io_ptr (cob_file *f)
{
	if (fileio_funcs[f->io_routine] == NULL) {
		cob_runtime_error (_("ERROR I/O routine %s is not present"),
							io_rtn_name[f->io_routine]);
	}
	return f->io_routine;
}
#endif


#ifdef WITH_INDEX_EXTFH

void cob_index_init_fileio (cob_file_api *);

/* Local variables */

static void cob_index_exit_fileio (cob_file_api *a);
static int indexed_open		(cob_file_api *, cob_file *, char *, const int, const int);
static int indexed_close	(cob_file_api *, cob_file *, const int);
static int indexed_start	(cob_file_api *, cob_file *, const int, cob_field *);
static int indexed_read		(cob_file_api *, cob_file *, cob_field *, const int);
static int indexed_read_next	(cob_file_api *, cob_file *, const int);
static int indexed_write	(cob_file_api *, cob_file *, const int);
static int indexed_delete	(cob_file_api *, cob_file *);
static int indexed_rewrite	(cob_file_api *, cob_file *, const int);

static struct cob_fileio_funcs ext_indexed_funcs = {
	indexed_open,
	indexed_close,
	indexed_start,
	indexed_read,
	indexed_read_next,
	indexed_write,
	indexed_rewrite,
	indexed_delete,
	(void*)extfh_dummy,
	cob_index_init_fileio,
	cob_index_exit_fileio,
	(void*)extfh_dummy,
	(void*)extfh_dummy,
	(void*)extfh_dummy,
	(void*)extfh_dummy
};

extern void	extfh_cob_init_fileio	(const struct cob_fileio_funcs *,
					const struct cob_fileio_funcs *,
					const struct cob_fileio_funcs *,
					int (*)(cob_file *, const int));
extern void	extfh_cob_exit_fileio	(void);

extern void extfh_indexed_unlock	(cob_file *);
extern int extfh_indexed_locate		(cob_file *, char *);
extern int extfh_indexed_open		(cob_file *, char *, const int, const int);
extern int extfh_indexed_close		(cob_file *, const int);
extern int extfh_indexed_start		(cob_file *, const int, cob_field *);
extern int extfh_indexed_read		(cob_file *, cob_field *, const int);
extern int extfh_indexed_read_next	(cob_file *, const int);
extern int extfh_indexed_write		(cob_file *, const int);
extern int extfh_indexed_delete		(cob_file *);
extern int extfh_indexed_rewrite	(cob_file *, const int);

/* OPEN INDEXED file */
static int
indexed_open (cob_file_api *a, cob_file *f, char *filename, const int mode, const int sharing)
{
	int		ret;

	f->io_routine = COB_IO_IXEXT;
	ret = extfh_indexed_locate (f, filename);
	switch (ret) {
	case COB_NOT_CONFIGURED:
		a->chk_file_mapping (f);
		if (access (filename, F_OK) && errno == ENOENT) {
			if (mode != COB_OPEN_OUTPUT && f->flag_optional == 0) {
				return COB_STATUS_35_NOT_EXISTS;
			}
		}
		break;
	case COB_STATUS_00_SUCCESS:
		break;
	default:
		return ret;
	}
	ret = extfh_indexed_open (f, filename, mode, sharing);
	switch (ret) {
	case COB_STATUS_00_SUCCESS:
		f->open_mode = mode;
		break;
	case COB_STATUS_35_NOT_EXISTS:
		if (f->flag_optional) {
			f->open_mode = mode;
			f->flag_nonexistent = 1;
			f->flag_end_of_file = 1;
			f->flag_begin_of_file = 1;
			return COB_STATUS_05_SUCCESS_OPTIONAL;
		}
		break;
	}
	return ret;
}

/* Close the INDEXED file */
static int
indexed_close (cob_file_api *a, cob_file *f, const int opt)
{
	COB_UNUSED (a);
	return extfh_indexed_close (f, opt);
}


/* START INDEXED file with positioning */
static int
indexed_start (cob_file_api *a, cob_file *f, const int cond, cob_field *key)
{
	COB_UNUSED (a);
	return extfh_indexed_start (f, cond, key);
}

/* Random READ of the INDEXED file  */
static int
indexed_read (cob_file_api *a, cob_file *f, cob_field *key, const int read_opts)
{
	COB_UNUSED (a);
	return extfh_indexed_read (f, key, read_opts);
}

/* Sequential READ of the INDEXED file */
static int
indexed_read_next (cob_file_api *a, cob_file *f, const int read_opts)
{
	COB_UNUSED (a);
	return extfh_indexed_read_next (f, read_opts);
}

/* WRITE to the INDEXED file  */
static int
indexed_write (cob_file_api *a, cob_file *f, const int opt)
{
	COB_UNUSED (a);
	return extfh_indexed_write (f, opt);
}


/* DELETE record from the INDEXED file  */
static int
indexed_delete (cob_file_api *a, cob_file *f)
{
	COB_UNUSED (a);
	return extfh_indexed_delete (f);
}

/* REWRITE record to the INDEXED file  */
static int
indexed_rewrite (cob_file_api *a, cob_file *f, const int opt)
{
	COB_UNUSED (a);
	return extfh_indexed_rewrite (f, opt);
}

/* Initialization/Termination
   cobsetpr-values with type ENV_PATH or ENV_STR
   like bdb_home and cob_file_path are taken care in cob_exit_common()!
*/

static void
cob_index_exit_fileio (cob_file_api *a)
{
	COB_UNUSED (a);
	extfh_cob_exit_fileio ();
}

void
cob_index_init_fileio (cob_file_api *a)
{
	fileio_funcs = a->io_funcs;
	a->io_funcs[COB_IO_IXEXT] = &ext_indexed_funcs;

	extfh_cob_init_fileio ( a->io_funcs[COB_IO_SEQUENTIAL],
							a->io_funcs[COB_IO_LINE_SEQUENTIAL],
							a->io_funcs[COB_IO_RELATIVE],
							a->cob_file_write_opt );
}

#endif

/*
 * Old SEQRA EXTFH interface
 */
#ifdef WITH_SEQRA_EXTFH
extern void extfh_seqra_unlock		(cob_file *);
extern int extfh_seqra_locate		(cob_file *, char *);
extern int extfh_cob_file_open		(cob_file *, char *, const int, const int);
extern int extfh_cob_file_close		(cob_file *, const int);
extern int extfh_sequential_read	(cob_file *, const int);
extern int extfh_sequential_write	(cob_file *, const int);
extern int extfh_sequential_rewrite	(cob_file *, const int);
extern int extfh_relative_start		(cob_file *, const int, cob_field *);
extern int extfh_relative_read		(cob_file *, cob_field *, const int);
extern int extfh_relative_read_next	(cob_file *, const int);
extern int extfh_relative_write		(cob_file *, const int);
extern int extfh_relative_rewrite	(cob_file *, const int);
extern int extfh_relative_delete	(cob_file *);
void cob_seqra_init_fileio (cob_file_api *);
static int seqra_open (cob_file_api *a, cob_file *f, char *filename, const int mode, const int sharing);
static int seqra_close (cob_file_api *a, cob_file *f, const int opt);
static void cob_seqra_exit_fileio (cob_file_api *a);

static int sequential_read	(cob_file_api *, cob_file *, const int);
static int sequential_write	(cob_file_api *, cob_file *, const int);
static int sequential_rewrite	(cob_file_api *, cob_file *, const int);
static int relative_start	(cob_file_api *, cob_file *, const int, cob_field *);
static int relative_read	(cob_file_api *, cob_file *, cob_field *, const int);
static int relative_read_next	(cob_file_api *, cob_file *, const int);
static int relative_write	(cob_file_api *, cob_file *, const int);
static int relative_rewrite	(cob_file_api *, cob_file *, const int);
static int relative_delete	(cob_file_api *, cob_file *);


static struct cob_fileio_funcs ext_sequential_funcs = {
	seqra_open,
	seqra_close,
	(void*)extfh_dummy,
	(void*)extfh_dummy,
	sequential_read,
	sequential_write,
	sequential_rewrite,
	(void*)extfh_dummy,
	(void*)extfh_dummy,
	cob_seqra_init_fileio,
	cob_seqra_exit_fileio,
	(void*)extfh_dummy,
	(void*)extfh_dummy,
	(void*)extfh_dummy,
	(void*)extfh_dummy
};

static struct cob_fileio_funcs ext_relative_funcs = {
	seqra_open,
	seqra_close,
	relative_start,
	relative_read,
	relative_read_next,
	relative_write,
	relative_rewrite,
	(void*)extfh_dummy,
	(void*)extfh_dummy,
	cob_seqra_init_fileio,
	cob_seqra_exit_fileio,
	(void*)extfh_dummy,
	(void*)extfh_dummy,
	(void*)extfh_dummy,
	(void*)extfh_dummy
};

extern void	extfh_cob_init_fileio	(const struct cob_fileio_funcs *,
					const struct cob_fileio_funcs *,
					const struct cob_fileio_funcs *,
					int (*)(cob_file *, const int));
extern void	extfh_cob_exit_fileio	(void);

static int
seqra_open (cob_file_api *a, cob_file *f, char *filename, const int mode, const int sharing)
{
	/* Note filename points to file_open_name */
	/* cob_chk_file_mapping manipulates file_open_name directly */

	int		ret;

	f->share_mode = sharing;
	ret = extfh_seqra_locate (f, filename);
	switch (ret) {
	case COB_NOT_CONFIGURED:
		a->chk_file_mapping (f);
		if (access (filename, F_OK) && errno == ENOENT) {
			if (mode != COB_OPEN_OUTPUT && f->flag_optional == 0) {
				return COB_STATUS_35_NOT_EXISTS;
			}
		}
		break;
	case COB_STATUS_00_SUCCESS:
		break;
	default:
		return ret;
	}
	ret = extfh_cob_file_open (f, filename, mode, sharing);
	switch (ret) {
	case COB_STATUS_00_SUCCESS:
		f->open_mode = mode;
		break;
	case COB_STATUS_35_NOT_EXISTS:
		if (f->flag_optional) {
			f->open_mode = mode;
			f->flag_nonexistent = 1;
			f->flag_end_of_file = 1;
			f->flag_begin_of_file = 1;
			return COB_STATUS_05_SUCCESS_OPTIONAL;
		}
		break;
	}
	return ret;
}

static int
seqra_close (cob_file_api *a, cob_file *f, const int opt)
{
	COB_UNUSED (a);
	return extfh_cob_file_close (f, opt);
}

/* SEQUENTIAL */

static int
sequential_read (cob_file_api *a, cob_file *f, const int read_opts)
{
	int	extfh_ret;

	extfh_ret = extfh_sequential_read (f, read_opts);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
	if(f->organization == COB_ORG_LINE_SEQUENTIAL)
		f->io_routine = COB_IO_LINE_SEQUENTIAL;
	else
		f->io_routine = COB_IO_SEQUENTIAL;
	return fileio_funcs[get_io_ptr (f)]->read_next (a, f, read_opts);
}

/* WRITE */
static int
sequential_write (cob_file_api *a, cob_file *f, const int opt)
{
	int	extfh_ret;

	extfh_ret = extfh_sequential_write (f, opt);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
	if(f->organization == COB_ORG_LINE_SEQUENTIAL)
		f->io_routine = COB_IO_LINE_SEQUENTIAL;
	else
		f->io_routine = COB_IO_SEQUENTIAL;
	return fileio_funcs[get_io_ptr (f)]->write (a, f, opt);
}

/* REWRITE */
static int
sequential_rewrite (cob_file_api *a, cob_file *f, const int opt)
{
	int	extfh_ret;

	extfh_ret = extfh_sequential_rewrite (f, opt);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
	if(f->organization == COB_ORG_LINE_SEQUENTIAL)
		f->io_routine = COB_IO_LINE_SEQUENTIAL;
	else
		f->io_routine = COB_IO_SEQUENTIAL;
	return fileio_funcs[get_io_ptr (f)]->rewrite (a, f, opt);
}

/* RELATIVE  START */
static int
relative_start (cob_file_api *a, cob_file *f, const int cond, cob_field *k)
{
	int	extfh_ret;

	extfh_ret = extfh_relative_start (f, cond, k);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
	if(f->organization == COB_ORG_LINE_SEQUENTIAL)
		f->io_routine = COB_IO_LINE_SEQUENTIAL;
	else
		f->io_routine = COB_IO_SEQUENTIAL;
	return fileio_funcs[get_io_ptr (f)]->start (a, f, cond, k);
}

/* RELATIVE  READ */
static int
relative_read (cob_file_api *a, cob_file *f, cob_field *k, const int read_opts)
{
	int	extfh_ret;

	extfh_ret = extfh_relative_read (f, k, read_opts);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
	f->io_routine = COB_IO_RELATIVE;
	return fileio_funcs[get_io_ptr (f)]->read (a, f, k, read_opts);
}

/* RELATIVE  READ NEXT */
static int
relative_read_next (cob_file_api *a, cob_file *f, const int read_opts)
{
	int		extfh_ret;

	extfh_ret = extfh_relative_read_next (f, read_opts);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
	f->io_routine = COB_IO_RELATIVE;
	return fileio_funcs[get_io_ptr (f)]->read_next (a, f, read_opts);
}

/* RELATIVE  WRITE */
static int
relative_write (cob_file_api *a, cob_file *f, const int opt)
{
	int	extfh_ret;

	extfh_ret = extfh_relative_write (f, opt);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
	f->io_routine = COB_IO_RELATIVE;
	return fileio_funcs[get_io_ptr (f)]->write (a, f, opt);
}

/* RELATIVE  REWRITE */
static int
relative_rewrite (cob_file_api *a, cob_file *f, const int opt)
{
	int	extfh_ret;

	extfh_ret = extfh_relative_rewrite (f, opt);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
	f->io_routine = COB_IO_RELATIVE;
	return fileio_funcs[get_io_ptr (f)]->rewrite (a, f, opt);
}

/* RELATIVE  DELETE */
static int
relative_delete (cob_file_api *a, cob_file *f)
{
	int	extfh_ret;

	extfh_ret = extfh_relative_delete (f);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
	f->io_routine = COB_IO_RELATIVE;
	return fileio_funcs[get_io_ptr (f)]->recdelete (a, f);
}

static void
cob_seqra_exit_fileio (cob_file_api *a)
{
	COB_UNUSED (a);
	extfh_cob_exit_fileio ();
}

void
cob_seqra_init_fileio (cob_file_api *a)
{
	fileio_funcs = a->io_funcs;
	a->io_funcs[COB_IO_SQEXT] = &ext_sequential_funcs;
	a->io_funcs[COB_IO_RLEXT] = &ext_relative_funcs;

	extfh_cob_init_fileio ( a->io_funcs[COB_IO_SEQUENTIAL],
							a->io_funcs[COB_IO_LINE_SEQUENTIAL],
							a->io_funcs[COB_IO_RELATIVE],
							a->cob_file_write_opt );
}
#endif

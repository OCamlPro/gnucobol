/*
   Copyright (C) 2002-2012, 2014-2022 Free Software Foundation, Inc.
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

#define _LFS64_LARGEFILE		1
#define _LFS64_STDIO			1
#define _FILE_OFFSET_BITS		64
#define _LARGEFILE64_SOURCE		1
#ifdef	_AIX
#define _LARGE_FILES			1
#endif	/* _AIX */
#if defined(__hpux__) && !defined(__LP64__)
#define _APP32_64BIT_OFF_T		1
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>

#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef	HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef	_WIN32

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <direct.h>
#include <io.h>
#if defined (__WATCOMC__) || defined (__ORANGEC__)
#define	fdcobsync	fsync
#else
#define	fdcobsync	_commit
#endif
#if !defined(__BORLANDC__) && !defined(__WATCOMC__) && !defined(__ORANGEC__)
#define	getcwd		_getcwd
#define	chdir		_chdir
#define	mkdir(path,mode)		_mkdir(path)
#define	rmdir		_rmdir
#define	open		_open
#define	close		_close
#define	unlink		_unlink
#define	fdopen		_fdopen
#ifndef lseek
#define lseek		_lseeki64
#endif
#endif

#ifndef	_O_TEMPORARY
#define	_O_TEMPORARY	0
#endif

#else	/* _WIN32 */

#if	defined(HAVE_FDATASYNC)
#if defined(HAVE_DECL_FDATASYNC) && HAVE_DECL_FDATASYNC == 0
/* function available and working, declaration missing on MacOS... */
int fdatasync(int fd);
#endif
#define	fdcobsync	fdatasync
#else
#define	fdcobsync	fsync
#endif

#ifndef	O_BINARY
#define	O_BINARY	0
#endif

#endif	/* _WIN32 */

#if !defined (EDEADLK) && defined (EDEADLOCK)
#define EDEADLK EDEADLOCK
#endif

#ifdef	WITH_DB

#include <db.h>

#elif	defined(WITH_CISAM) || defined(WITH_DISAM) || defined(WITH_VBISAM)

#define	WITH_ANY_ISAM
#include <signal.h>

#define	COB_WITH_STATUS_02

#ifdef	WITH_CISAM
#include <isam.h>
#define	isfullclose(x)	isclose (x)
#define ISRECNUM isrecnum
#define ISERRNO  iserrno
#define ISRECLEN isreclen
#endif

#ifdef	WITH_DISAM
#ifndef DISAM_NO_ISCONFIG
#include <isconfig.h>
#ifndef ISCOBOL_STATS
#undef	COB_WITH_STATUS_02
#endif
#endif
#include <disam.h>
#define	isfullclose(x)	isclose (x)
#define ISRECNUM isrecnum
#define ISERRNO  iserrno
#define ISRECLEN isreclen
#endif

#ifdef	WITH_VBISAM
#include <vbisam.h>
/* VB-ISAM does not set dup key status */
#undef	COB_WITH_STATUS_02
#if defined(VB_RTD)
/* Since VBISAM 2.1.1: access to isrecnum iserrno etc is no longer global */
static	vb_rtd_t *vbisam_rtd = NULL;

#define ISRECNUM vbisam_rtd->isrecnum
#define ISERRNO  vbisam_rtd->iserrno
#define ISRECLEN vbisam_rtd->isreclen
#else
#define ISRECNUM isrecnum
#define ISERRNO  iserrno
#define ISRECLEN isreclen
#endif
#endif

#endif

/* Force symbol exports */
#define	COB_LIB_EXPIMP
#include "common.h"
#include "coblocal.h"

#ifdef	WITH_ANY_ISAM

/* Isam File handler packet */

struct indexfile {
	char		*filename;	/* ISAM data file name */
	char		*savekey;	/* Area to save last primary key read */
	char		*recwrk;	/* Record work/save area */
	size_t		nkeys;		/* Actual keys in file */
	int		isfd;		/* ISAM file number */
	long		recnum;		/* Last record number read */
	long		saverecnum;	/* isrecnum of next record to process */
	int		saveerrno;	/* savefileposition errno */
	int		lmode;		/* File lock mode for 'isread' */
	int		startcond;	/* Previous 'start' condition value */
	int		readdir;	/* Read direction: ISPREV or ISNEXT */
	int		lenkey;		/* Length of savekey area */
	int		eofpending;	/* End of file pending */
	int		readdone;	/* A 'read' has been successfully done */
	int		startiscur;	/* The 'start' record is current */
	int		wrkhasrec;	/* 'recwrk' holds the next|prev record */
	struct keydesc	key[1];		/* Table of key information */
					/* keydesc is defined in (d|c|vb)isam.h */
};

/* Return total length of the key */
static int
indexed_keylen (struct indexfile *fh, int idx)
{
	int totlen, part;
	totlen = 0;
	for (part = 0; part < fh->key[idx].k_nparts; part++) {
		totlen += fh->key[idx].k_part[part].kp_leng;
	}
	return totlen;
}

/* Save key for given index into 'savekey'
   Return total length of the key */
static int
indexed_savekey (struct indexfile *fh, unsigned char *data, int idx)
{
	int totlen, part;
	totlen = 0;
	if (data == NULL) {
		data = (unsigned char*)fh->recwrk;
	}
	for (part = 0; part < fh->key[idx].k_nparts; part++) {
		memcpy (fh->savekey + totlen,
			data  + fh->key[idx].k_part[part].kp_start,
			fh->key[idx].k_part[part].kp_leng);
		totlen += fh->key[idx].k_part[part].kp_leng;
	}
	return totlen;
}

/* Copy key for given index from 'savekey' back to recwrk
   Return total length of the key */
static int
indexed_restorekey (struct indexfile *fh, unsigned char *data, int idx)
{
	int totlen, part;
	totlen = 0;
	if (data == NULL) {
		data = (unsigned char*)fh->recwrk;
	}
	for (part = 0; part < fh->key[idx].k_nparts; part++) {
		memcpy (data  + fh->key[idx].k_part[part].kp_start,
			fh->savekey + totlen,
			fh->key[idx].k_part[part].kp_leng);
		totlen += fh->key[idx].k_part[part].kp_leng;
	}
	return totlen;
}

/* Compare key for given index 'savekey' to recwrk
   Return compare status */
static int
indexed_cmpkey (struct indexfile *fh, unsigned char *data, int idx, int partlen)
{
	int sts, part, totlen,cl;
	totlen = sts = 0;
	if (partlen <= 0) {
		partlen = indexed_keylen(fh, idx);
	}
	for (part = 0; part < fh->key[idx].k_nparts && partlen > 0; part++) {
		cl = partlen > fh->key[idx].k_part[part].kp_leng ? fh->key[idx].k_part[part].kp_leng : partlen;
		sts = memcmp(	data  + fh->key[idx].k_part[part].kp_start,
					fh->savekey + totlen, cl);
		if (sts != 0) {
			return sts;
		}
		totlen += fh->key[idx].k_part[part].kp_leng;
		partlen -= fh->key[idx].k_part[part].kp_leng;
	}
	return sts;
}

/* Build 'keydesc' from 'cob_file_key'
   Return total length of the key */
static int
indexed_keydesc (cob_file *f, struct keydesc *kd, cob_file_key *key)
{
	int	keylen,part;
	memset (kd,0,sizeof (struct keydesc));
	kd->k_flags = key->tf_duplicates ? ISDUPS : ISNODUPS;
	if (key->count_components < 1) {
		/* backward compatibility for not generated components (pre 3.0) */
		kd->k_nparts = 1;
		kd->k_start = key->offset;
		kd->k_leng = key->field->size;
		kd->k_type = CHARTYPE;
		if (key->tf_suppress) {
#ifdef NULLKEY
			kd->k_flags |= NULLKEY;
			kd->k_type = CHARTYPE | (key->char_suppress << 8);
#else
#if 0	/* TODO: SUPPRESS string not merged yet */
			f->flag_write_chk_dups = 1;
#endif
			kd->k_flags = ISDUPS;
#endif
		}
		keylen = kd->k_leng;
	} else {
		/* multi- and single field key as component */
		/* LCOV_EXCL_START */
		if (key->count_components > COB_MAX_KEYCOMP) {
			/* not translated as this is safety guard unlikely to be ever triggered */
			cob_runtime_warning ("module specifies %d key components, "
				"this runtime ignores all parts greater than %d",
				key->count_components, COB_MAX_KEYCOMP);
			key->count_components = COB_MAX_KEYCOMP;
		}
		/* LCOV_EXCL_STOP */
		keylen = 0;
		for (part=0; part < key->count_components; part++) {
			kd->k_part[part].kp_start = key->component[part]->data - f->record->data;
			kd->k_part[part].kp_leng = key->component[part]->size;
			keylen += kd->k_part[part].kp_leng;
			kd->k_part[part].kp_type = CHARTYPE;
			if (key->tf_suppress) {
#ifdef NULLKEY
				kd->k_flags |= NULLKEY;
				kd->k_part[part].kp_type = CHARTYPE | (key->char_suppress << 8);
#else
#if 0	/* TODO: SUPPRESS string not merged yet */
				f->flag_write_chk_dups = 1;
#endif
				kd->k_flags = ISDUPS;
#endif
			}
		}
		kd->k_nparts = part;
	}
#if defined(WITH_DISAM) || defined(WITH_VBISAM)
	kd->k_len = keylen;		/* Total length of this key */
#endif
	return keylen;
}

/* Compare 'keydesc' to 'keydesc'
   Return 0 if equal, else 1 */
static int
indexed_keycmp (struct keydesc *k1, struct keydesc *k2)
{
	int	part;
	if (k1->k_flags != k2->k_flags) {
		return 1;
	}
	if (k1->k_nparts != k2->k_nparts) {
		return 1;
	}
	for (part=0; part < k1->k_nparts; part++) {
		if (k1->k_part[part].kp_start != k2->k_part[part].kp_start) {
			return 1;
		}
		if (k1->k_part[part].kp_leng != k2->k_part[part].kp_leng) {
			return 1;
		}
		if (k1->k_part[part].kp_type != k2->k_part[part].kp_type) {
			return 1;
		}
	}
	return 0;
}

/* Return index number for given key */
static int
indexed_findkey (cob_file *f, cob_field *kf, int *fullkeylen, int *partlen)
{
	int 	k,part;
	*fullkeylen = *partlen = 0;
	for (k = 0; k < f->nkeys; ++k) {
		if (f->keys[k].field
		&&  f->keys[k].count_components <= 1
		&&  f->keys[k].field->data == kf->data) {
			*fullkeylen = f->keys[k].field->size;
			*partlen = kf->size;
			f->mapkey = k;
			return k;
		}
	}
	for (k = 0; k < f->nkeys; ++k) {
		if (f->keys[k].count_components > 1) {
			if ((f->keys[k].field
			&&  f->keys[k].field->data == kf->data
			&&  f->keys[k].field->size == kf->size)
			||  (f->keys[k].component[0]->data == kf->data)) {
				for (part=0; part < f->keys[k].count_components; part++) {
					*fullkeylen += f->keys[k].component[part]->size;
				}
				if (f->keys[k].field
				 && f->keys[k].field->data == kf->data) {
					*partlen = kf->size;
				} else {
					*partlen = *fullkeylen;
				}
				f->mapkey = k;
				return k;
			}
		}
	}
	return -1;
}

#endif

/* Define some characters for checking LINE SEQUENTIAL data content */
#define COB_CHAR_CR	'\r'
#define COB_CHAR_FF	'\f'
#define COB_CHAR_LF	'\n'
#define COB_CHAR_SPC	' '
#define COB_CHAR_TAB	'\t'
#ifdef COB_EBCDIC_MACHINE
#define COB_CHAR_BS	0x16
#define COB_CHAR_ESC	0x27
#define COB_CHAR_SI	0x0F
#else
#define COB_CHAR_BS	0x08
#define COB_CHAR_ESC	0x1B
#define COB_CHAR_SI	0x0F
#endif

struct file_list {
	struct file_list	*next;
	cob_file		*file;
};

#ifdef	WORDS_BIGENDIAN
#define	COB_MAYSWAP_16(x)	((unsigned short)(x))
#define	COB_MAYSWAP_32(x)	((unsigned int)(x))
#else
#define	COB_MAYSWAP_16(x)	(COB_BSWAP_16((unsigned short)(x)))
#define	COB_MAYSWAP_32(x)	(COB_BSWAP_32((unsigned int)(x)))
#endif

/* SORT definitions */

#define COBSORTEND		1
#define COBSORTABORT		2
#define COBSORTFILEERR		3
#define COBSORTNOTOPEN		4


/* Sort item */
struct cobitem {
	struct cobitem		*next;
	unsigned char		end_of_block;
	unsigned char		block_byte;
	unsigned char		unique[sizeof (size_t)];
	unsigned char		item[1];
};

/* Sort memory chunk */
struct sort_mem_struct {
	struct sort_mem_struct	*next;
	unsigned char		*mem_ptr;
};

/* Sort queue structure */
struct queue_struct {
	struct cobitem		*first;
	struct cobitem		*last;
	size_t			count;
};

/* Sort temporary file structure */
struct file_struct {
	FILE			*fp;
	size_t			count;	/* Count of blocks in temporary files */
};

/* Sort base structure */
struct cobsort {
	void			*pointer;
	struct cobitem		*empty;
	void			*sort_return;
	cob_field		*fnstatus;
	struct sort_mem_struct	*mem_base;
	size_t			unique;
	size_t			size;
	size_t			alloc_size;
	size_t			mem_size;
	size_t			mem_used;
	size_t			mem_total;
	size_t			chunk_size;
	size_t			r_size;
	size_t			w_size;
	size_t			switch_to_file;
	unsigned int		retrieving;
	unsigned int		files_used;
	int			destination_file;
	int			retrieval_queue;
	struct queue_struct	queue[4];
	struct file_struct	file[4];
};

/* End SORT definitions */


/* Local variables */

static cob_global	*cobglobptr = NULL;
static cob_settings	*cobsetptr = NULL;

static unsigned int	eop_status = 0;
static unsigned int	check_eop_status = 0;
static int		cob_vsq_len = 0;
static int		last_operation_open = 0;

static struct file_list	*file_cache = NULL;

static char		*file_open_env = NULL;
static char		*file_open_name = NULL;
static char		*file_open_buff = NULL;

static char		*runtime_buffer = NULL;

static const int	status_exception[] = {
	0,				/* 0x */
	COB_EC_I_O_AT_END,		/* 1x */
	COB_EC_I_O_INVALID_KEY,		/* 2x */
	COB_EC_I_O_PERMANENT_ERROR,	/* 3x */
	COB_EC_I_O_LOGIC_ERROR,		/* 4x */
	COB_EC_I_O_RECORD_OPERATION,	/* 5x */
	COB_EC_I_O_FILE_SHARING,	/* 6x */
	COB_EC_I_O_RECORD_CONTENT,	/* 7x, currently unused */
	COB_EC_I_O,			/* Unused */
	COB_EC_I_O_IMP			/* 9x */
};

static const char	* const prefix[] = { "DD_", "dd_", "" };
#define NUM_PREFIX	sizeof (prefix) / sizeof (char *)

static int dummy_delete		(cob_file *);
#if 0	/* not needed any more */
static int dummy_rnxt_rewrite	(cob_file *, const int);
#endif
static int dummy_read		(cob_file *, cob_field *, const int);
static int dummy_start		(cob_file *, const int, cob_field *);

static int cob_file_open	(cob_file *, char *, const enum cob_open_mode, const int);
static int cob_file_close	(cob_file *, const int);
static int cob_savekey (cob_file *f, int idx, unsigned char *data);
static int cob_file_write_opt	(cob_file *, const int);

static int sequential_read	(cob_file *, const int);
static int sequential_write	(cob_file *, const int);
static int sequential_rewrite	(cob_file *, const int);
static int lineseq_read		(cob_file *, const int);
static int lineseq_write	(cob_file *, const int);
static int lineseq_rewrite	(cob_file *, const int);
static int relative_start	(cob_file *, const int, cob_field *);
static int relative_read	(cob_file *, cob_field *, const int);
static int relative_read_next	(cob_file *, const int);
static int relative_write	(cob_file *, const int);
static int relative_rewrite	(cob_file *, const int);
static int relative_delete	(cob_file *);

static int indexed_open		(cob_file *, char *, const enum cob_open_mode, const int);
static int indexed_close	(cob_file *, const int);
static int indexed_start	(cob_file *, const int, cob_field *);
static int indexed_read		(cob_file *, cob_field *, const int);
static int indexed_read_next	(cob_file *, const int);
static int indexed_write	(cob_file *, const int);
static int indexed_delete	(cob_file *);
static int indexed_rewrite	(cob_file *, const int);

static const struct cob_fileio_funcs indexed_funcs = {
	indexed_open,
	indexed_close,
	indexed_start,
	indexed_read,
	indexed_read_next,
	indexed_write,
	indexed_rewrite,
	indexed_delete
};

static const struct cob_fileio_funcs sequential_funcs = {
	cob_file_open,
	cob_file_close,
	dummy_start,
	dummy_read,
	sequential_read,
	sequential_write,
	sequential_rewrite,
	dummy_delete
};

static const struct cob_fileio_funcs lineseq_funcs = {
	cob_file_open,
	cob_file_close,
	dummy_start,
	dummy_read,
	lineseq_read,
	lineseq_write,
	lineseq_rewrite,
	dummy_delete
};

static const struct cob_fileio_funcs relative_funcs = {
	cob_file_open,
	cob_file_close,
	relative_start,
	relative_read,
	relative_read_next,
	relative_write,
	relative_rewrite,
	relative_delete
};

static const struct cob_fileio_funcs	*fileio_funcs[COB_ORG_MAX] = {
	&sequential_funcs,
	&lineseq_funcs,
	&relative_funcs,
	&indexed_funcs,
	NULL
};

#if	defined(WITH_INDEX_EXTFH) || defined(WITH_SEQRA_EXTFH)
extern void	extfh_cob_init_fileio	(const struct cob_fileio_funcs *,
					const struct cob_fileio_funcs *,
					const struct cob_fileio_funcs *,
					int (*)(cob_file *, const int));
extern void	extfh_cob_exit_fileio	(void);
#endif

#ifdef	WITH_INDEX_EXTFH
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
#endif

#ifdef	WITH_SEQRA_EXTFH
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
#endif

static void		cob_file_fcd_sync	(cob_file *);
static void		cob_fcd_file_sync	(cob_file *, char *);
static void		free_extfh_fcd (void);

#ifdef	WITH_DB

static DB_ENV		*bdb_env = NULL;
static char		*bdb_buff = NULL;
static const char	**bdb_data_dir = NULL;
static void		*record_lock_object = NULL;
static size_t		rlo_size = 0;
static unsigned int	bdb_lock_id = 0;

#define DB_PUT(db,flags)	db->put (db, NULL, &p->key, &p->data, flags)
#define DB_GET(db,flags)	db->get (db, NULL, &p->key, &p->data, flags)
#define DB_SEQ(db,flags)	db->c_get (db, &p->key, &p->data, flags)
#define DB_DEL(db,key,flags)	db->del (db, NULL, key, flags)
#define DB_CLOSE(db)		db->close (db, 0)
#define DB_SYNC(db)		db->sync (db, 0)
#define	cob_dbtsize_t		u_int32_t

#if	defined(WORDS_BIGENDIAN)
/* Big Endian then leave 'int' alone */
#define	COB_DUPSWAP(x)		((unsigned int)(x))

#elif	defined(COB_BDB_BAD_DUPNO) || 1 /* FIXME: may be added to a file specific flag */
/* Want to retain incorrect storing of Little Endian value backwards */
#define	COB_DUPSWAP(x)		((unsigned int)(x))

#else
/* Little Endian so swap byte around to have dupno value stored in bigendian sequence */
#define	COB_DUPSWAP(x)		(COB_BSWAP_32((unsigned int)(x)))
#endif

#define DBT_SET(key,fld)			\
	key.data = fld->data;			\
	key.size = (cob_dbtsize_t) fld->size

struct indexed_file {
	DB		**db;		/* Database handlers */
	DBC		**cursor;
	char		*filename;	/* Needed for record locks */
	unsigned char	*last_key;	/* The last key written */
	unsigned char	*temp_key;	/* Used for temporary storage */
	unsigned char	**last_readkey;	/* The last key read */
	unsigned int	*last_dupno;	/* The last number of duplicates read */
	int		*rewrite_sec_key;
	int		maxkeylen;
	int		primekeylen;
	unsigned char	*savekey;	/* Work area for saving key value */
	unsigned char	*suppkey;	/* Work area for saving key value */
	unsigned char	*saverec;	/* For saving copy of record */
	DBT		key;
	DBT		data;
	DB_LOCK		bdb_file_lock;
	DB_LOCK		bdb_record_lock;
	int			key_index;
	unsigned int	bdb_lock_id;
	int		write_cursor_open;
	int		record_locked;
	int		filenamelen;
};

static int
bdb_findkey (cob_file *f, cob_field *kf, int *fullkeylen, int *partlen)
{
	int 	k, part;

	*fullkeylen = *partlen = 0;
	for (k = 0; k < f->nkeys; ++k) {
		if (f->keys[k].field
		&&  f->keys[k].count_components <= 1
		&&  f->keys[k].field->data == kf->data) {
			*fullkeylen = f->keys[k].field->size;
			*partlen = kf->size;
			return k;
		}
	}
	for (k = 0; k < f->nkeys; ++k) {
		if (f->keys[k].count_components > 1) {
			if ( (f->keys[k].field
			   && f->keys[k].field->data == kf->data
			   && f->keys[k].field->size == kf->size)
			 ||  (f->keys[k].component[0]->data == kf->data)) {
				for (part = 0; part < f->keys[k].count_components; part++) {
					*fullkeylen += f->keys[k].component[part]->size;
				}
				if (f->keys[k].field
				 && f->keys[k].field->data == kf->data) {
					*partlen = kf->size;
				} else {
					*partlen = *fullkeylen;
				}
				return (int)k;
			}
		}
	}
	return -1;
}

/* Return total length of the key */
static int
bdb_keylen (cob_file *f, int idx)
{
	int totlen, part;

	if (idx < 0 || idx > f->nkeys) {
		return -1;
	}
	if (f->keys[idx].count_components > 0) {
		totlen = 0;
		for (part = 0; part < f->keys[idx].count_components; part++) {
			totlen += f->keys[idx].component[part]->size;
		}
		return totlen;
	}
	return f->keys[idx].field->size;
}

/* Save key for given index from 'record' into 'keyarea',
   returns total length of the key */
static int
bdb_savekey (cob_file *f, unsigned char *keyarea, unsigned char *record, int idx)
{
	int totlen, part;

	if (f->keys[idx].count_components > 0) {
		totlen = 0;
		for (part = 0; part < f->keys[idx].count_components; part++) {
			memcpy (keyarea + totlen,
				record + (f->keys[idx].component[part]->data - f->record->data),
				f->keys[idx].component[part]->size);
			totlen += f->keys[idx].component[part]->size;
		}
		return totlen;
	}
	memcpy (keyarea, record + f->keys[idx].offset, f->keys[idx].field->size);
	return (int)f->keys[idx].field->size;
}

static void
bdb_setkey (cob_file *f, int idx)
{
	struct indexed_file	*p;
	int	len;

	p = f->file;
	memset (p->savekey, 0, p->maxkeylen);
	len = bdb_savekey (f, p->savekey, f->record->data, idx);
	p->key.data = p->savekey;
	p->key.size = (cob_dbtsize_t) len;
}

/* Compare key for given index 'keyarea' to 'record'.
   returns compare status */
static int
bdb_cmpkey (cob_file *f, unsigned char *keyarea, unsigned char *record, int idx, int partlen)
{
	int sts, part, totlen;
	size_t	cl;

	if (partlen <= 0) {
		partlen = bdb_keylen(f, idx);
		/* LCOV_EXCL_START */
		if (partlen <= 0) {
			cob_runtime_error (_("invalid internal call of %s"), "bdb_cmpkey");
			cob_hard_failure_internal ();
		}
		/* LCOV_EXCL_STOP */
	}
	if (f->keys[idx].count_components > 0) {
		totlen = 0;
		for (part = 0; part < f->keys[idx].count_components && partlen > 0; part++) {
			cl = partlen > f->keys[idx].component[part]->size ? f->keys[idx].component[part]->size : partlen;
			sts = memcmp (keyarea + totlen,
					record + (f->keys[idx].component[part]->data - f->record->data),
					cl);
			if (sts != 0) {
				return sts;
			}
			totlen += f->keys[idx].component[part]->size;
			partlen -= f->keys[idx].component[part]->size;
		}
		return 0;
	}
	cl = partlen > f->keys[idx].field->size ? f->keys[idx].field->size : partlen;
	return memcmp (keyarea, record  + f->keys[idx].offset, cl);
}

/* Is given key data all SUPPRESS char,
   returns 1 if key has all SUPPRESS char */
static int
bdb_suppresskey (cob_file *f, int idx)
{
	unsigned char ch_sprs;
	int 	i,len;
	struct indexed_file	*p;

	if (!f->keys[idx].tf_suppress) {
		return 0;
	}
	ch_sprs = f->keys[idx].char_suppress & 0xFF;
	p = f->file;
	len = bdb_savekey(f, p->suppkey, f->record->data, idx);
	for (i = 0; i < len; i++) {
		if (p->suppkey[i] != ch_sprs)
			return 0;
	}
	return 1;
}

#endif	/* WITH_DB */


/* Local functions */

static int
dummy_delete (cob_file *f)
{
	COB_UNUSED (f);

	return COB_STATUS_91_NOT_AVAILABLE;
}

#if 0	/* not needed any more */
static int
dummy_rnxt_rewrite (cob_file *f, const int opt)
{
	COB_UNUSED (f);
	COB_UNUSED (opt);

	return COB_STATUS_91_NOT_AVAILABLE;
}
#endif

static int
dummy_read (cob_file *f, cob_field *key, const int read_opts)
{
	COB_UNUSED (f);
	COB_UNUSED (key);
	COB_UNUSED (read_opts);

	return COB_STATUS_91_NOT_AVAILABLE;
}

static int
dummy_start (cob_file *f, const int cond, cob_field *key)
{
	COB_UNUSED (f);
	COB_UNUSED (cond);
	COB_UNUSED (key);

	return COB_STATUS_91_NOT_AVAILABLE;
}

/* Check for DD_xx, dd_xx, xx environment variables for a filename
   or a part specified with 'src';
   returns either the value or NULL if not found in the environment
   Note: MF only checks for xx if the variable started with a $,
	     ACUCOBOL only checks for xx in general ... */
static char *
cob_chk_file_env (const char *src)
{
	char		*p;
	char		*q;
	char		*s;
	size_t		i;

	/* GC-sanity rule: no environment handling if src starts with period */
	if (*src == '.') {
		return NULL;
	}

	/* no mapping if filename begins with a slash [externally checked], hyphen or digits
	   (taken from "Programmer's Guide to File Handling, Chapter 2: File Naming") */
	switch (*file_open_name) {
	case '-':
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
		return NULL;
	default:
		break;
	}

	q = cob_strdup (src);
	s = q;
	if (unlikely (cobsetptr->cob_env_mangle)) {
		for (i = 0; i < strlen (s); ++i) {
			if (!isalnum ((int)s[i])) {
				s[i] = '_';
			}
		}
	} else {
		for (i = 0; i < strlen (s); ++i) {
			if (s[i] == '.') {
				s[i] = '_';
			}
		}
	}
	p = NULL;
	for (i = 0; i < NUM_PREFIX; ++i) {
		snprintf (file_open_env, (size_t)COB_FILE_MAX, "%s%s",
			  prefix[i], s);
		file_open_env[COB_FILE_MAX] = 0;
		p = getenv (file_open_env);
		if (p && *p) {
			break;
		}
		p = NULL;
	}
	cob_free (q);
	return p;
}

/* checks if 'src' containes a / or \ */
static int
has_directory_separator (char *src)
{
	for (; *src; src++) {
		if (*src == '/' || *src == '\\') {
			return 1;
		}
	}
	return 0;
}

/* checks if 'src' looks like starting with  name */
static int
looks_absolute (char *src)
{
	/* no file path adjustment if filename is absolute
	   because it begins with a slash (or win-disk-drive) */
	if (src[0] == '/'
	 || src[0] == '\\'
#ifdef _WIN32
	 || src[1] == ':'
#endif
		) {
		return 1;
	}
	return 0;
}

/* checks for special ACUCOBOL-case: file that start with hyphen [note: -P not supported]
   no translation at all, name starts after first non-space */
static int
has_acu_hyphen (char *src)
{
	if ( src[0] == '-'
	 && (src[1] == 'F' || src[1] == 'D' || src[1] == 'f' || src[1] == 'd')
	 && isspace((cob_u8_t)src[2])) {
		return 1;
	}
	return 0;
}

/* do acu translation, 'src' may not be file_open_buff! */
static void
do_acu_hyphen_translation (char *src)
{
	/* maybe store device type to "adjust locking rules" */
	/* find first non-space and return it in the original storage  */
	for (src = src + 3; *src && isspace ((cob_u8_t)*src); src++);
	
	strncpy (file_open_buff, src, (size_t)COB_FILE_MAX);
	file_open_buff[COB_FILE_MAX] = 0;
	strncpy (file_open_name, file_open_buff, (size_t)COB_FILE_MAX);
}

static void
cob_chk_file_mapping (void)
{
	char		*p;
	char		*src;
	char		*dst;
	char		*saveptr;
	char		*orig;
	unsigned int	dollar;

	/* no mapping at all if explicit disabled on compile-time (dialect configuration)*/
	if (unlikely (!COB_MODULE_PTR->flag_filename_mapping)) {
		return;
	}

	/* Special ACUCOBOL-case: file that start with hyphen [note: -P not supported]
	   no translation at all, name starts after first non-space */
	if (has_acu_hyphen (file_open_name)) {
		do_acu_hyphen_translation (file_open_name);
		return ;
	}

	src = file_open_name;

	/* Simple case - No separators [note: this is also the ACU and Fujitsu way] */
	if (!looks_absolute(src)
	 && !has_directory_separator(src)) {
		/* Ignore leading dollar */
		if (*src == '$') {
			src++;
		}
		/* Check for DD_xx, dd_xx, xx environment variables */
		/* Note: ACU and Fujitsu would only check for xx */
		/* If not found, use as is, possibly including the dollar character */
		if ((p = cob_chk_file_env (src)) != NULL) {
			strncpy (file_open_name, p, (size_t)COB_FILE_MAX);
			/* Note: ACU specifies: "repeated until variable can't be resolved" 
			   we don't apply this and will not in the future
			   [recursion is only one of the problems] */
			if (looks_absolute (src)) {
				return;
			}
			if (has_acu_hyphen (file_open_name)) {
				do_acu_hyphen_translation (file_open_name);
				return ;
			}
		}
		/* apply COB_FILE_PATH if set (similar to ACUCOBOL's FILE-PREFIX)
		   MF and Fujistu simply don't have that - not set by default,
		   so no compatilibity issue here */
		if (cobsetptr->cob_file_path) {
			snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s%c%s",
				  cobsetptr->cob_file_path, SLASH_CHAR, file_open_name);
			file_open_buff[COB_FILE_MAX] = 0;
			strncpy (file_open_name, file_open_buff,
				 (size_t)COB_FILE_MAX);
		}
		return;
	}

	/* Complex */

	/* Note: ACU and Fujitsu would return the value back and stop here */

	/* Isolate first element (everything before the slash) */
	/* If it starts with a $, mark and skip over the $ */
	/* Try mapping on resultant string - DD_xx, dd_xx, xx */
	/* If successful, use the mapping */
	/* If not, use original element EXCEPT if we started */
	/* with a $, in which case we ignore the element AND */
	/* the following slash */

	dst = file_open_buff;
	*dst = 0;

	if (*src != '$') {
		dollar = 0;
	} else {
		dollar = 1;
		src++;
	}

	orig = cob_strdup (src);
	saveptr = orig;

	/* strtok strips leading delimiters */
	if (*src == '/' || *src == '\\') {
		strcpy (file_open_buff, SLASH_STR);
	} else {
		file_open_buff[COB_FILE_MAX] = 0;
		p = strtok (orig, "/\\");
		orig = NULL;
		if ((src = cob_chk_file_env (p)) != NULL) {
			strncpy (file_open_buff, src, (size_t)COB_FILE_MAX);
			dollar = 0;
		} else if (!dollar) {
			strncpy (file_open_buff, p, (size_t)COB_FILE_MAX);
		}
	}
	file_open_buff[COB_FILE_MAX] = 0;
	/* First element completed, loop through remaining */
	/* elements delimited by slash */
	/* Check only for $ from now on; includes the DD_xx/dd_xx/xx mapping */
	src = NULL;
	for (; ;) {
		p = strtok (orig, "/\\");
		if (!p) {
			break;
		}
		if (!orig) {
			if (!dollar) {
				strcat (file_open_buff, SLASH_STR);
			}
		} else {
			orig = NULL;
		}
		if (*p != '$') {
			dollar = 0;
		} else {
			dollar = 1;
			p++;
		}
		if (dollar && (src = cob_chk_file_env (p)) != NULL) {
			strncat (file_open_buff, src, (size_t)COB_FILE_MAX);
			src = NULL;
		} else if (!dollar) {
			strncat (file_open_buff, p, (size_t)COB_FILE_MAX);
			src = NULL;
		} else {
			src = p - 1;
		}
	}
	/* if we have a final $something that cannot be resolved - use as plain name */
	if (src) {
		strncat (file_open_buff, src, (size_t)COB_FILE_MAX);
	}
	strcpy (file_open_name, file_open_buff);
	cob_free (saveptr);

	if (looks_absolute (file_open_name)) {
		return;
	}
	/* apply COB_FILE_PATH if set (similar to ACUCOBOL's FILE-PREFIX) */
	if (cobsetptr->cob_file_path) {
		snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s%c%s",
			cobsetptr->cob_file_path, SLASH_CHAR, file_open_name);
		file_open_buff[COB_FILE_MAX] = 0;
		strncpy (file_open_name, file_open_buff,
			(size_t)COB_FILE_MAX);
	}

}

static void
cob_sync (cob_file *f)
{
#ifdef	WITH_DB
	struct indexed_file	*p;
	size_t			i;
#elif	defined(WITH_ANY_ISAM)
	struct indexfile	*fh;
#endif

	if (f->organization == COB_ORG_INDEXED) {
#ifdef	WITH_DB
		p = f->file;
		if (p) {
			for (i = 0; i < f->nkeys; ++i) {
				if (p->db[i]) {
					DB_SYNC (p->db[i]);
				}
			}
		}
#elif	defined(WITH_ANY_ISAM)
		fh = f->file;
		if (fh) {
			isflush (fh->isfd);
		}
#endif
		return;
	}
	if (f->organization != COB_ORG_SORT) {
		if (f->file) {
			fflush ((FILE *)f->file);
		}
		if (f->fd >= 0) {
			fdcobsync (f->fd);
		}
	}
}


typedef struct __cob_file_key_pre3 {
	cob_field	*field;	/* Key field */
	int		flag;	/* WITH DUPLICATES (for RELATIVE/INDEXED) */
					/* ASCENDING/DESCENDING (for SORT) */
	unsigned int	offset;			/* Offset of field */
} cob_file_key_pre3;

static void
cob_cache_file (cob_file *f)
{
	struct file_list	*l;

	for (l = file_cache; l; l = l->next) {
		if (f == l->file) {
			return;
		}
	}
	l = cob_malloc (sizeof (struct file_list));
	l->file = f;
	l->next = file_cache;
	file_cache = l;

	/* reset bad file_key attributes, 3.0 had split "flags" and added fields
	   before the offset; since 2.2 the memory is allocated in libcob via
	   (cob_file_malloc), but the module is the one that needs to (and does)
	   set the file and key attributes. If the module is older than 3.0 it has
	   set those "at the wrong place" - reset here; only possible because we
	   know that the full memory is allocated and because each module has its
	   own cob_file pointer that only itself set the cob_file_key attributes */
	if (COB_MODULE_PTR->gc_version && COB_MODULE_PTR->gc_version[0] == '2') {
		int 	k;
		/* copy over "old view" into local fields */
		cob_file_key_pre3 * old_view = (cob_file_key_pre3 *)f->keys;
		cob_file_key_pre3 * old_keys = cob_malloc (sizeof (cob_file_key_pre3) * f->nkeys);
		for (k = 0; k < f->nkeys; ++k) {
			old_keys[k].field = old_view[k].field;
			old_keys[k].flag = old_view[k].flag;
			old_keys[k].offset = old_view[k].offset;
		}
		/* copy local fields into actual data */
		for (k = 0; k < f->nkeys; ++k) {
			memset(&f->keys[k], 0, sizeof(cob_file_key));
			f->keys[k].field = old_keys[k].field;
			if (f->organization == COB_ORG_SORT) {
				f->keys[k].flag = old_keys[k].flag;	/* tf_ascending not used in 3.x... */
			} else {
				f->keys[k].tf_duplicates = old_keys[k].flag;
			}
			f->keys[k].offset = old_keys[k].offset;
		}
		cob_free (old_keys);
	}
	
}

static void
save_status (cob_file *f, cob_field *fnstatus, const int status)
{
	cobglobptr->cob_error_file = f;
	if (likely(status == 0)) {
		memset (f->file_status, '0', (size_t)2);
		/* EOP is non-fatal therefore 00 status but needs exception */
		if (eop_status == 0) {
			cobglobptr->cob_exception_code = 0;
		} else {
#if 0 /* correct thing to do, but then also needs to have codegen adjusted
         --> module-incompatibility --> 4.x */
			cob_set_exception (eop_status);
#else
			cob_set_exception (COB_EC_I_O_EOP);
#endif
			eop_status = 0;
		}
		if (unlikely (cobsetptr->cob_do_sync)) {
			cob_sync (f);
		}
	} else {
		cob_set_exception (status_exception[status / 10]);
		f->file_status[0] = (unsigned char)COB_I2D (status / 10);
		f->file_status[1] = (unsigned char)COB_I2D (status % 10);
	}
	if (fnstatus) {
		memcpy (fnstatus->data, f->file_status, (size_t)2);
	}
	if (f->fcd) {
		cob_file_fcd_sync (f);			/* Copy cob_file to app's FCD */
	}
}

/* Regular file */

/* Translate errno status to COBOL status,
   Note: always sets either an error or the given default value */
static int
errno_cob_sts (const int default_status)
{
	switch (errno) {
#ifdef EDQUOT
	case EDQUOT:
#endif
	case ENOSPC:
		return COB_STATUS_34_BOUNDARY_VIOLATION;
	case EPERM:
	case EACCES:
	case EISDIR:
		return COB_STATUS_37_PERMISSION_DENIED;
	case ENOENT:
		return COB_STATUS_35_NOT_EXISTS;
	default:
		return default_status;
	}
}

#define COB_CHECKED_FPUTC(char_to_write,fstream)	do { \
		const int character = (int)char_to_write; \
		if (unlikely (putc (character, fstream) != character)) { \
			return errno_cob_sts (COB_STATUS_30_PERMANENT_ERROR); \
		} \
	} ONCE_COB /* LCOV_EXCL_LINE */

#define COB_CHECKED_WRITE(fd,string,length_to_write)	do { \
		const size_t length = (size_t)length_to_write; \
		if (unlikely (write (fd, string, length) != length)) { \
			return errno_cob_sts (COB_STATUS_30_PERMANENT_ERROR); \
		} \
	} ONCE_COB /* LCOV_EXCL_LINE */

#define COB_CHECKED_FWRITE(fstream,string,length_to_write)	do { \
		const size_t length = (size_t)length_to_write; \
		if (unlikely (fwrite (string, 1, length, fstream) != length)) { \
			return errno_cob_sts (COB_STATUS_30_PERMANENT_ERROR); \
		} \
	} ONCE_COB /* LCOV_EXCL_LINE */

static size_t
file_linage_check (cob_file *f)
{
	cob_linage	*lingptr;

	lingptr = f->linorkeyptr;
	lingptr->lin_lines = cob_get_int (lingptr->linage);
	if (lingptr->lin_lines < 1) {
		goto linerr;
	}
	if (lingptr->latfoot) {
		lingptr->lin_foot = cob_get_int (lingptr->latfoot);
		if (lingptr->lin_foot < 1 ||
		    lingptr->lin_foot > lingptr->lin_lines) {
			goto linerr;
		}
	} else {
		lingptr->lin_foot = 0;
	}
	if (lingptr->lattop) {
		lingptr->lin_top = cob_get_int (lingptr->lattop);
		if (lingptr->lin_top < 0) {
			goto linerr;
		}
	} else {
		lingptr->lin_top = 0;
	}
	if (lingptr->latbot) {
		lingptr->lin_bot = cob_get_int (lingptr->latbot);
		if (lingptr->lin_bot < 0) {
			goto linerr;
		}
	} else {
		lingptr->lin_bot = 0;
	}
	return 0;
linerr:
	cob_set_int (lingptr->linage_ctr, 0);
	return 1;
}

static int
cob_linage_write_opt (cob_file *f, const int opt)
{
	cob_linage		*lingptr;
	FILE		*fp = (FILE *)f->file;
	int			i;
	int			n;

	lingptr = f->linorkeyptr;
	if (unlikely (opt & COB_WRITE_PAGE)) {
		i = cob_get_int (lingptr->linage_ctr);
		if (i == 0) {
			return COB_STATUS_57_I_O_LINAGE;
		}
		n = lingptr->lin_lines;
		for (; i < n; ++i) {
			COB_CHECKED_FPUTC ('\n', fp);
		}
		for (i = 0; i < lingptr->lin_bot; ++i) {
			COB_CHECKED_FPUTC ('\n', fp);
		}
		if (file_linage_check (f)) {
			return COB_STATUS_57_I_O_LINAGE;
		}
		for (i = 0; i < lingptr->lin_top; ++i) {
			COB_CHECKED_FPUTC ('\n', fp);
		}
		cob_set_int (lingptr->linage_ctr, 1);
	} else if (opt & COB_WRITE_LINES) {
		n = cob_get_int (lingptr->linage_ctr);
		if (n == 0) {
			return COB_STATUS_57_I_O_LINAGE;
		}
		cob_add_int (lingptr->linage_ctr, opt & COB_WRITE_MASK, 0);
		i = cob_get_int (lingptr->linage_ctr);
		/* Set EOP status if requested */
		if (check_eop_status && lingptr->lin_foot) {
			if (i >= lingptr->lin_foot) {
				eop_status = COB_EC_I_O_EOP;
			}
		}
		if (i > lingptr->lin_lines) {
			/* Set EOP status if requested */
			if (check_eop_status) {
				eop_status = COB_EC_I_O_EOP_OVERFLOW;
			}
			for (; n < lingptr->lin_lines; ++n) {
				COB_CHECKED_FPUTC ('\n', fp);
			}
			for (i = 0; i < lingptr->lin_bot; ++i) {
				COB_CHECKED_FPUTC ('\n', fp);
			}
			if (file_linage_check (f)) {
				return COB_STATUS_57_I_O_LINAGE;
			}
			cob_set_int (lingptr->linage_ctr, 1);
			for (i = 0; i < lingptr->lin_top; ++i) {
				COB_CHECKED_FPUTC ('\n', fp);
			}
		} else {
			for (i = (opt & COB_WRITE_MASK) - 1; i > 0; --i) {
				COB_CHECKED_FPUTC ('\n', fp);
			}
		}
	}
	return 0;
}

static unsigned int
cob_seq_write_opt (cob_file *f, const int opt)
{
	int	i;

	if (opt & COB_WRITE_LINES) {
		i = opt & COB_WRITE_MASK;
		if (!i) {
			/* AFTER/BEFORE 0 */
			COB_CHECKED_WRITE (f->fd, "\r", 1);
		} else {
			for (i = opt & COB_WRITE_MASK; i > 0; --i) {
				COB_CHECKED_WRITE (f->fd, "\n", 1);
			}
		}
	} else if (opt & COB_WRITE_PAGE) {
		COB_CHECKED_WRITE (f->fd, "\f", 1);
	}
	return 0;
}

static int
cob_file_write_opt (cob_file *f, const int opt)
{
	FILE	*fp = (FILE *)f->file;
	int		i;

	if (unlikely (f->flag_select_features & COB_SELECT_LINAGE)) {
		return cob_linage_write_opt (f, opt);
	}
	if (opt & COB_WRITE_LINES) {
		i = opt & COB_WRITE_MASK;
		if (!i) {
			/* AFTER/BEFORE 0 */
			COB_CHECKED_FPUTC ('\r', fp);
		} else {
			for (; i > 0; --i) {
				COB_CHECKED_FPUTC ('\n', fp);
			}
		}
	} else if (opt & COB_WRITE_PAGE) {
		COB_CHECKED_FPUTC ('\f', fp);
	}
	return 0;
}

/*
 * Open (record) Sequential and Relative files
 *  with just an 'fd' (No FILE *)
 */
static int
cob_fd_file_open (cob_file *f, char *filename,
		const enum cob_open_mode mode, const int sharing,
		unsigned int	nonexistent)
{
	int		fd;
	int		fdmode;
	int		fperms;

	COB_UNUSED (sharing);	/* used in 4.x */

	fdmode = O_BINARY;
	fperms = 0;
	switch (mode) {
	case COB_OPEN_INPUT:
		fdmode |= O_RDONLY;
		break;
	case COB_OPEN_OUTPUT:
		nonexistent = 0;
		fdmode |= O_CREAT | O_TRUNC;
		if (f->organization == COB_ORG_RELATIVE) {
			fdmode |= O_RDWR;
		} else {
			fdmode |= O_WRONLY;
		}
#ifdef	_WIN32
		fperms = _S_IREAD | _S_IWRITE ;
#else
		fperms = COB_FILE_MODE;
#endif
		break;
	case COB_OPEN_I_O:
		if (nonexistent) {
			fdmode |= O_CREAT | O_RDWR;
#ifdef	_WIN32
			fperms = _S_IREAD | _S_IWRITE ;
#else
			fperms = COB_FILE_MODE;
#endif
		} else {
			fdmode |= O_RDWR;
		}
		break;
	case COB_OPEN_EXTEND:
		fdmode |= O_CREAT | O_RDWR | O_APPEND;
#ifdef	_WIN32
		fperms = _S_IREAD | _S_IWRITE ;
#else
		fperms = COB_FILE_MODE;
#endif
		break;
	/* LCOV_EXCL_START */
	default:
		cob_runtime_error (_("invalid internal call of %s"), "cob_fd_file_open");
		cob_fatal_error (COB_FERROR_CODEGEN);
	/* LCOV_EXCL_STOP */
	}

	errno = 0;
	fd = open (filename, fdmode, fperms);

	switch (errno) {
	case 0:
		if (mode == COB_OPEN_EXTEND && fd >= 0) {
			lseek (fd, (off_t) 0, SEEK_END);
		}
		f->open_mode = mode;
		break;
	case ENOENT:
		if (mode == COB_OPEN_EXTEND || mode == COB_OPEN_OUTPUT) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		if (f->flag_optional) {
			f->fd = fd;
			f->open_mode = mode;
			f->flag_nonexistent = 1;
			f->flag_end_of_file = 1;
			f->flag_begin_of_file = 1;
			return COB_STATUS_05_SUCCESS_OPTIONAL;
		}
		return COB_STATUS_35_NOT_EXISTS;
	case EACCES:
	case EISDIR:
	case EROFS:
		return COB_STATUS_37_PERMISSION_DENIED;
	case EAGAIN:
		return COB_STATUS_61_FILE_SHARING;
	default:
		return COB_STATUS_30_PERMANENT_ERROR;
	}

#ifdef	HAVE_FCNTL
	/* Lock the file */
	if (memcmp (filename, "/dev/", (size_t)5)) {
		struct flock	lock;
		memset ((void *)&lock, 0, sizeof (struct flock));
		if (mode != COB_OPEN_INPUT) {
			lock.l_type = F_WRLCK;
		} else {
			lock.l_type = F_RDLCK;
		}
		lock.l_whence = SEEK_SET;
		lock.l_start = 0;
		lock.l_len = 0;
		errno = 0;
		if (fcntl (fd, F_SETLK, &lock) < 0) {
			int		ret = errno;
			f->open_mode = COB_OPEN_CLOSED;
			close (fd);
			switch (ret) {
			case EACCES:
			case EAGAIN:
#ifdef EDEADLK
			case EDEADLK:
#endif
				return COB_STATUS_61_FILE_SHARING;
			default:
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
	}
#elif defined _WIN32
	{
		HANDLE osHandle = (HANDLE)_get_osfhandle (fd);
		if (osHandle != INVALID_HANDLE_VALUE) {
			DWORD flags = LOCKFILE_FAIL_IMMEDIATELY;
			OVERLAPPED fromStart = {0};
			if (mode != COB_OPEN_INPUT) flags |= LOCKFILE_EXCLUSIVE_LOCK;
			if (!LockFileEx (osHandle, flags, 0, MAXDWORD, MAXDWORD, &fromStart)) {
				f->open_mode = COB_OPEN_CLOSED;
				close (fd);
				return COB_STATUS_61_FILE_SHARING;
			}
		}
	}
#endif
	f->fd = fd;
	f->record_off = -1;
#if 0	/* Simon: disabled, this function is expected to not use a file */
	{
		const char *fmode;
		if (mode == COB_OPEN_INPUT) {
			fmode = "r";
		} else if (mode == COB_OPEN_I_O) {
			if (nonexistent)
				fmode = "w+";
			else
				fmode = "r+";
		} else if (mode == COB_OPEN_EXTEND) {
			fmode = "";
		} else {
			fmode = "w";
		}
		f->file = (void*)fdopen(f->fd, fmode);
	}
#endif
	if (f->flag_optional && nonexistent) {
		return COB_STATUS_05_SUCCESS_OPTIONAL;
	}
	return 0;
}

static int
cob_file_open (cob_file *f, char *filename,
		const enum cob_open_mode mode, const int sharing)
{
	/* Note filename points to file_open_name */
	/* cob_chk_file_mapping manipulates file_open_name directly */

#ifdef	WITH_SEQRA_EXTFH
	int		ret;

	ret = extfh_seqra_locate (f, filename);
	switch (ret) {
	case COB_NOT_CONFIGURED:
		cob_chk_file_mapping ();
		errno = 0;
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

#else

	FILE			*fp;
	const char		*fmode;
	cob_linage		*lingptr;
	unsigned int		nonexistent;

	/* Note filename points to file_open_name */
	/* cob_chk_file_mapping manipulates file_open_name directly */

	cob_chk_file_mapping ();

	nonexistent = 0;
	errno = 0;
	if (access (filename, F_OK)) {
		if (errno == ENOENT) {
			if (mode != COB_OPEN_OUTPUT && f->flag_optional == 0) {
				return COB_STATUS_35_NOT_EXISTS;
			}
			nonexistent = 1;
#if 0 /* CHECKME: how to handle stuff like ENOTDIR here ?*/
		} else if (errno == ENOTDIR) {
				return COB_STATUS_30_PERMANENT_ERROR;
		} else {
			...
#endif
		}
	}
	
	f->file = NULL;
	f->fd = -1;
	
	if (f->organization != COB_ORG_LINE_SEQUENTIAL) {
		return cob_fd_file_open (f, filename, mode, sharing, nonexistent);
	}
	
	fmode = NULL;
	/* Open the file */
	switch (mode) {
	case COB_OPEN_INPUT:
		if (!cobsetptr->cob_unix_lf) {
			fmode = "r";
		} else {
			fmode = "rb";
		}
		break;
	case COB_OPEN_OUTPUT:
		if (!cobsetptr->cob_unix_lf) {
			fmode = "w";
		} else {
			fmode = "wb";
		}
		break;
	case COB_OPEN_I_O:
		fmode = "r+";
		break;
	case COB_OPEN_EXTEND:
		/* Problem on WIN32 (tested _MSC_VER 1500 and GCC build) if file isn't there: */
		/* Both modes create the file and return a bad pointer */
		/* Mode "a+"  sets EINVAL, further actions on the file do work */
		/* Mode "ab+" doesn't set errno, but we don't want a binary file */
		/* Possible Solutions: */
		/* a) Create the file and reopen it with a+ */
		/* b) Check this stuff in EINVAL and just go on */
		if (!cobsetptr->cob_unix_lf) {
			fmode = "a+";
		} else {
			fmode = "ab+";
		}
		break;
	/* LCOV_EXCL_START */
	default:
		cob_runtime_error (_("invalid internal call of %s"), "cob_file_open");
		cob_fatal_error (COB_FERROR_CODEGEN);
	/* LCOV_EXCL_STOP */
	}

	errno = 0;
	fp = fopen (filename, fmode);
	switch (errno) {
	case 0:
		f->open_mode = mode;
		break;
	case EINVAL:
		if (f->flag_optional && nonexistent) {
			f->open_mode = mode;
		} else {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		break;
	case ENOENT:
		if (mode == COB_OPEN_EXTEND || mode == COB_OPEN_OUTPUT) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		if (f->flag_optional) {
			f->file = NULL;
			f->fd = -1;
			f->open_mode = mode;
			f->flag_nonexistent = 1;
			f->flag_end_of_file = 1;
			f->flag_begin_of_file = 1;
			return COB_STATUS_05_SUCCESS_OPTIONAL;
		}
		return COB_STATUS_35_NOT_EXISTS;
	case EACCES:
	case EISDIR:
	case EROFS:
		return COB_STATUS_37_PERMISSION_DENIED;
	case EAGAIN:
		return COB_STATUS_61_FILE_SHARING;
	default:
		return COB_STATUS_30_PERMANENT_ERROR;
	}

	if (unlikely (f->flag_select_features & COB_SELECT_LINAGE)) {
		if (file_linage_check (f)) {
			if (fp) {
				fclose (fp);
			}
			return COB_STATUS_57_I_O_LINAGE;
		}
		f->flag_needs_top = 1;
		lingptr = f->linorkeyptr;
		cob_set_int (lingptr->linage_ctr, 1);
	}

	if (fp) {
		f->fd = fileno (fp);
	} else {
		f->fd = -1;
	}

#ifdef	HAVE_FCNTL
	/* Lock the file */
	if (fp && memcmp (filename, "/dev/", (size_t)5)) {
		struct flock		lock;
		memset ((void *)&lock, 0, sizeof (struct flock));
		if (mode != COB_OPEN_INPUT) {
			lock.l_type = F_WRLCK;
		} else {
			lock.l_type = F_RDLCK;
		}
		lock.l_whence = SEEK_SET;
		lock.l_start = 0;
		lock.l_len = 0;
		errno = 0;
		if (fcntl (f->fd, F_SETLK, &lock) < 0) {
			int ret = errno;
			f->open_mode = COB_OPEN_CLOSED;
			f->fd = -1;
			fclose (fp);
			switch (ret) {
			case EACCES:
			case EAGAIN:
#ifdef EDEADLK
			case EDEADLK:
#endif
				return COB_STATUS_61_FILE_SHARING;
			default:
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
	}
#elif defined _WIN32
	{
		/* Lock the file */
		if (fp) {
			HANDLE osHandle = (HANDLE)_get_osfhandle (f->fd);
			if (osHandle != INVALID_HANDLE_VALUE) {
				DWORD flags = LOCKFILE_FAIL_IMMEDIATELY;
				OVERLAPPED fromStart = {0};
				if (mode != COB_OPEN_INPUT) flags |= LOCKFILE_EXCLUSIVE_LOCK;
				if (!LockFileEx (osHandle, flags, 0, MAXDWORD, MAXDWORD, &fromStart)) {
					f->open_mode = COB_OPEN_CLOSED;
					f->fd = -1;
					fclose (fp);
					return COB_STATUS_61_FILE_SHARING;
				}
			}
		}
	}
#endif
	f->file = fp;
	if (f->flag_optional && nonexistent) {
		return COB_STATUS_05_SUCCESS_OPTIONAL;
	}
	return 0;

#endif
}

static int
cob_file_close (cob_file *f, const int opt)
{
#ifdef	WITH_SEQRA_EXTFH
	return extfh_cob_file_close (f, opt);
#else

	switch (opt) {
	case COB_CLOSE_LOCK:
		/* meaning (not file-sharing related):
		   file may not be opened in *this runtime unit* again */
		/* TODO: set flag here */
		/* Fall through */
	case COB_CLOSE_NORMAL:
	case COB_CLOSE_NO_REWIND:
		if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
			if (f->flag_needs_nl
			 && !(f->flag_select_features & COB_SELECT_LINAGE)) {
				f->flag_needs_nl = 0;
				putc ('\n', (FILE *)f->file);
			}
		} else if (f->flag_needs_nl) {
			f->flag_needs_nl = 0;
			if (f->fd >= 0) {
				COB_CHECKED_WRITE (f->fd, "\n", 1);
			}
		}
#ifdef	HAVE_FCNTL
		/* Unlock the file */
		if (f->fd >= 0) {
			struct flock lock;
			memset ((void *)&lock, 0, sizeof (struct flock));
			lock.l_type = F_UNLCK;
			lock.l_whence = SEEK_SET;
			lock.l_start = 0;
			lock.l_len = 0;
			errno = 0;
			if (fcntl (f->fd, F_SETLK, &lock) == -1) {
#if 1 /* CHECKME - What is the correct thing to do here? */
				/* not translated as "testing only" */
				cob_runtime_warning ("issue during unlock (%s), errno: %d", "cob_file_close", errno);
#endif
			}
		}
#elif defined _WIN32
		{
			HANDLE osHandle = (HANDLE)_get_osfhandle (f->fd);
			if (osHandle != INVALID_HANDLE_VALUE) {
				/* CHECKME: Should this use UnlockFileEx ? */
				if (!UnlockFile (osHandle, 0, 0, MAXDWORD, MAXDWORD)) {
					const DWORD last_error = GetLastError ();
					if (last_error != 158) {	/* no locked region */
#if 1 /* CHECKME - What is the correct thing to do here? */
						/* not translated as "testing only" */
						cob_runtime_warning ("issue during UnLockFile (%s), lastError: " CB_FMT_LLU,
							"cob_file_close", (cob_u64_t)last_error);
#endif
					}
				}
			}
		}
#endif
		/* Close the file */
		if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
			if (f->file) {
				fclose ((FILE *)f->file);
				f->file = NULL;
			}
		} else {
			if (f->fd >= 0) {
				close (f->fd);
				f->fd = -1;
			}
		}
		if (opt == COB_CLOSE_NO_REWIND) {
			f->open_mode = COB_OPEN_CLOSED;
			return COB_STATUS_07_SUCCESS_NO_UNIT;
		}
		return COB_STATUS_00_SUCCESS;
	default:
		if (f->fd >= 0 && f->open_mode != COB_OPEN_INPUT) {
			fdcobsync (f->fd);
		}
		return COB_STATUS_07_SUCCESS_NO_UNIT;
	}
#endif
}

static int
open_next (cob_file *f)
{
	if (f->flag_is_concat
	 && *f->nxt_filename != 0) {
#if 0 /* Note: file specific features are 4.x only .... */
		char	*nx = strchr(f->nxt_filename,file_setptr->cob_concat_sep[0]);
#else
		char	*nx = strchr(f->nxt_filename,cobsetptr->cob_concat_sep[0]);
#endif
		close (f->fd);
		if (f->file) {
			fclose (f->file);
		}
		f->fd = -1;
		f->file = NULL;
		if (nx) {
			*nx = 0;
			if (f->open_mode == COB_OPEN_I_O) {
				f->fd = open (f->nxt_filename, O_RDWR);
			} else {
				f->fd = open (f->nxt_filename, O_RDONLY);
			}
			f->nxt_filename = nx + 1;
		} else {
			if (f->open_mode == COB_OPEN_I_O) {
				f->fd = open (f->nxt_filename, O_RDWR);
			} else {
				f->fd = open (f->nxt_filename, O_RDONLY);
			}
			f->flag_is_concat = 0;
			if (f->org_filename) {
				cob_cache_free (f->org_filename);
				f->org_filename = NULL;
			}
		}
		if (f->fd != -1) {
			if (f->open_mode == COB_OPEN_INPUT) {
				f->file = (void*)fdopen(f->fd, "r");
			} else { 
				f->file = (void*)fdopen(f->fd, "r+");
			}
			return 1;
		}
	}
	return 0;
}


/* SEQUENTIAL */

static int
sequential_read (cob_file *f, const int read_opts)
{
	int	bytesread;
	union {
		unsigned char	sbuff[4];
		unsigned short	sshort[2];
		unsigned int	sint;
	} recsize;

#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_sequential_read (f, read_opts);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#else
	COB_UNUSED (read_opts);
#endif

again:
	if (unlikely (f->flag_operation != 0)) {
		f->flag_operation = 0;
		/* Get current file position */
		f->record_off = lseek (f->fd, (off_t)0, SEEK_CUR);
	}

	if (unlikely (f->record_min != f->record_max)) {
		/* Read record size */
		bytesread = read (f->fd, recsize.sbuff, cob_vsq_len);
		if (bytesread == 0
		 && open_next (f)) {
			goto again;
		}
		if (unlikely (bytesread != cob_vsq_len)) {
			if (bytesread == 0) {
				return COB_STATUS_10_END_OF_FILE;
			} else {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
		switch (cobsetptr->cob_varseq_type) {
		case 1:
			f->record->size = COB_MAYSWAP_32 (recsize.sint);
			break;
		case 2:
			f->record->size = recsize.sint;
			break;
		default:
			f->record->size = COB_MAYSWAP_16 (recsize.sshort[0]);
			break;
		}
	}

	/* Read record */
	bytesread = read (f->fd, f->record->data, f->record->size);
	if (bytesread == 0
	 && open_next (f)) {
		goto again;
	}

	/* CODE-SET conversion */
	if (f->sort_collating) {
		const unsigned char* rec_end = f->record->data + bytesread;
		if (f->nconvert_fields) {
			/* CODE-SET FOR - convert specific area only */
			size_t ic;
			for (ic = 0; ic < f->nconvert_fields; ic++) {
				const cob_field to_conv = f->convert_field[ic];
				const unsigned char* to_conv_end = to_conv.data + to_conv.size;
				const unsigned char* conv_end = rec_end < to_conv_end ? rec_end : to_conv_end;
				unsigned char* p;
				for (p = to_conv.data; p < conv_end; p++) {
					*p = f->code_set_read[*p];
				}
			}
		} else {
			/* CODE-SET conversion for complete record */
			unsigned char* p;
			for (p = f->record->data; p < rec_end; p++) {
				*p = f->code_set_read[*p];
			}
		}
	}
	if (unlikely (bytesread != (int)f->record->size)) {
		if (bytesread == 0) {
			return COB_STATUS_10_END_OF_FILE;
		/* LCOV_EXCL_START */
		} else if (bytesread < 0) {
			return COB_STATUS_30_PERMANENT_ERROR;
		/* LCOV_EXCL_STOP */
		} else {
			return COB_STATUS_04_SUCCESS_INCOMPLETE;
		}
	}
	return COB_STATUS_00_SUCCESS;
}

static int
sequential_write (cob_file *f, const int opt)
{
	union {
		unsigned char	sbuff[4];
		unsigned short	sshort[2];
		unsigned int	sint;
	} recsize;

#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_sequential_write (f, opt);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif

	if (unlikely (f->flag_operation == 0)) {
		f->flag_operation = 1;
		/* Get current file position */
		f->record_off = lseek (f->fd, (off_t)0, SEEK_CUR);
	}

	/* WRITE AFTER */
	if (unlikely (opt & COB_WRITE_AFTER)) {
		if (cob_seq_write_opt (f, opt)) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		f->flag_needs_nl = 1;
	}

	if (unlikely (f->record_min != f->record_max)) {
		/* Write record size */

		switch (cobsetptr->cob_varseq_type) {
		case 1:
			recsize.sint = COB_MAYSWAP_32 (f->record->size);
			break;
		case 2:
			recsize.sint = f->record->size;
			break;
		default:
			recsize.sint = 0;
			recsize.sshort[0] = COB_MAYSWAP_16 (f->record->size);
			break;
		}
		COB_CHECKED_WRITE (f->fd, recsize.sbuff, cob_vsq_len);
	}

	/* Write record */
	COB_CHECKED_WRITE (f->fd, f->record->data, f->record->size);

	/* WRITE BEFORE */
	if (unlikely (opt & COB_WRITE_BEFORE)) {
		if (cob_seq_write_opt (f, opt)) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		f->flag_needs_nl = 0;
	}

	return COB_STATUS_00_SUCCESS;
}

static int
sequential_rewrite (cob_file *f, const int opt)
{
#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_sequential_rewrite (f, opt);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#else
	COB_UNUSED (opt);
#endif
	f->flag_operation = 1;
#if 1 /* old operation, going backwards */
	if (lseek (f->fd, -(off_t) f->record->size, SEEK_CUR) == (off_t)-1) {
#else /* new one 4x, from the known file position */
	if (lseek (f->fd, f->record_off, SEEK_CUR) == (off_t)-1) {
#endif
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	COB_CHECKED_WRITE (f->fd, f->record->data, f->record->size);
	return COB_STATUS_00_SUCCESS;
}

/* LINE SEQUENTIAL */

#define IS_BAD_CHAR(x) (x < ' ' && x != COB_CHAR_BS && x != COB_CHAR_ESC \
					 && x != COB_CHAR_FF && x != COB_CHAR_SI && x != COB_CHAR_TAB)

static int
lineseq_read (cob_file *f, const int read_opts)
{
	FILE	*fp;
	unsigned char	*dataptr;
	size_t		i = 0;
	int		n;
	int		sts = COB_STATUS_00_SUCCESS;

#ifdef	WITH_SEQRA_EXTFH
	int		extfh_ret;

	extfh_ret = extfh_sequential_read (f, read_opts);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#else
	COB_UNUSED (read_opts);
#endif

	dataptr = f->record->data;
again:
	fp = (FILE *)f->file;
	f->record_off = ftell (fp);	/* Save position at start of line */
	for (; ;) {
		n = getc (fp);
		if (unlikely (n == EOF)) {
			if (!i) {
				if (open_next (f)) {
					goto again;
				}
				return COB_STATUS_10_END_OF_FILE;
			} else {
				break;
			}
		}
		if (n == '\r') {
			int next = getc (fp);
			if (next == '\n') {
				/* next is LF -> so ignore CR */
				n = '\n';
			} else {
				/* looks like \r was part of the data,
				   re-position and pass to COBOL data
				   after validation */
				fseek (fp, -1, SEEK_CUR);
			}
		}
		if (n == '\n') {
			break;
		}
		/* CODE-SET conversion for complete record */
		if (n < UCHAR_MAX && f->sort_collating && !f->nconvert_fields) {
			n = f->code_set_read[(unsigned char)n];
		}
#if 0 /* Note: file specific features are 4.x only ... */
		if ((f->file_features & COB_FILE_LS_VALIDATE) {
#else
		if (cobsetptr->cob_ls_validate
		 && !f->flag_line_adv
		 && !f->nconvert_fields) {
#endif
			if ((IS_BAD_CHAR (n) 
			  || (n > 0x7E && !isprint(n)))) {
				sts = COB_STATUS_09_READ_DATA_BAD;
			}
		} else
		if (cobsetptr->cob_ls_nulls) {
			if (n == 0) {
				n = getc (fp);
				/* NULL-Encoded -> should be less than a space */
				if (n == EOF || (unsigned char)n >= ' ') {		
					return COB_STATUS_71_BAD_CHAR;
				}
			/* Not NULL-Encoded, may not be less than a space */
			} else if (!f->nconvert_fields && (unsigned char)n < ' ') {
				return COB_STATUS_71_BAD_CHAR;
			}
		}
#if 0	/* From trunk - CHECKME: When should this be done?
     	   Only for LS_VALIDATE / LS_NULLS? */
		/* Skip NEW PAGE on reading */
		if (n == '\f') {
			continue;
		}
#endif
		if (i < f->record_max) {
			*dataptr++ = (unsigned char)n;
			i++;
			if (i == f->record_max
			 && (cobsetptr->cob_ls_split)) {
				/* If record is too long, then simulate end
				 * so balance becomes the next record read */
				off_t	k = 1;
				n = getc (fp);
				if (n == '\r') {
					n = getc (fp);
					k++;
				}
				if (n != '\n') {
					fseek (fp, -k, SEEK_CUR);
					sts = COB_STATUS_06_READ_TRUNCATE;
				}
				break;
			}
		} else
		if (i == f->record_max) {
			sts = COB_STATUS_04_SUCCESS_INCOMPLETE;
		}
	}
	/* CODE-SET FOR - convert specific area only */
	if (f->sort_collating && f->nconvert_fields) {
		const unsigned char* rec_end = f->record->data + i;
		size_t ic;
		for (ic = 0; ic < f->nconvert_fields; ic++) {
			const cob_field to_conv = f->convert_field[ic];
			const unsigned char *to_conv_end = to_conv.data + to_conv.size;
			const unsigned char *conv_end = rec_end < to_conv_end ? rec_end : to_conv_end;
			unsigned char * p;
			for (p = to_conv.data; p < conv_end; p++) {
				n = *p = f->code_set_read[*p];
				if ((IS_BAD_CHAR (n)
				 || (n > 0x7E && !isprint (n)))) {
					sts = COB_STATUS_09_READ_DATA_BAD;
				}
			}
		}
	}
	if (i < f->record_max) {
		/* Fill the record with spaces */
		memset ((unsigned char *)f->record->data + i, ' ',
			f->record_max - i);
	}
	f->record->size = i;
#ifdef READ_WRITE_NEEDS_FLUSH
	if (f->open_mode == COB_OPEN_I_O)	/* Required on some systems */
		fflush (fp);
#endif
	return sts;
}

/* Determine the size to be written */
static size_t
lineseq_size (cob_file *f)
{
	size_t size,i;
#if 0 /* Note: file specific features are 4.x only ... */
	if ((f->file_features & COB_FILE_LS_FIXED)) {
#else
	if (unlikely (cobsetptr->cob_ls_fixed != 0)) {
#endif
		return f->record->size;
	}
	if (f->variable_record) {
		f->record->size = (size_t)cob_get_int (f->variable_record);
		if (f->record->size > f->record_max) {
			f->record->size = f->record_max;
		}
	}
	if (f->record->size < f->record_min) {
		f->record->size = f->record_min;
	}
	if (f->record->size == 0) {
		return 0;
	}
	for (i = f->record->size - 1; ; --i) {
		if (f->record->data[i] != ' ') {
			i++;
			break;
		}
		if (i == 0) break;
	}
	size = i;
	return size;
}

static int
lineseq_write (cob_file *f, const int opt)
{
	FILE	*fp = (FILE *)f->file;
	unsigned char		*p;
	cob_linage		*lingptr;
	const size_t		size = f->record->size;
	int			ret;

#ifdef	WITH_SEQRA_EXTFH
	int		extfh_ret;

	extfh_ret = extfh_sequential_write (f, opt);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif


	if (unlikely (f->flag_select_features & COB_SELECT_LINAGE)) {
		if (f->flag_needs_top) {
			int i;
			f->flag_needs_top = 0;
			lingptr = f->linorkeyptr;
			for (i = 0; i < lingptr->lin_top; ++i) {
				COB_CHECKED_FPUTC ('\n', fp);
			}
		}
	}
	/* WRITE AFTER */
	if (opt & COB_WRITE_AFTER) {
		ret = cob_file_write_opt (f, opt);
		if (ret) {
			return ret;
		}
		f->flag_needs_nl = 1;
	}

	f->record_off = ftell (fp);	/* Save file position at start of line */
	/* Write to the file */
	if (size) {
		size_t i;
		errno = 0;
#if 0 /* Note: file specific features are 4.x only ... */
		if ((f->file_features & COB_FILE_LS_VALIDATE)) {
#else
		if (cobsetptr->cob_ls_validate
		 && !f->flag_line_adv
		 && !f->sort_collating /* pre-validated */) {
#endif
			p = f->record->data;
			for (i = 0; i < size; ++i, ++p) {
				if (IS_BAD_CHAR (*p)) {
					return COB_STATUS_71_BAD_CHAR;
				}
			}
			COB_CHECKED_FWRITE (fp, f->record->data, size);
		} else if (cobsetptr->cob_ls_nulls) {
			p = f->record->data;
			for (i = 0; i < size; ++i, ++p) {
				if (*p < ' ') {
					COB_CHECKED_FPUTC (0, fp);
				}
				COB_CHECKED_FPUTC ((*p), fp);
			}
		} else {
			COB_CHECKED_FWRITE (fp, f->record->data, size);
		}
	}

	if (unlikely (f->flag_select_features & COB_SELECT_LINAGE)) {
		COB_CHECKED_FPUTC ('\n', fp);
	} else if (cobsetptr->cob_ls_uses_cr) {
		if ((opt & COB_WRITE_PAGE)
		 || (opt & COB_WRITE_BEFORE && f->flag_needs_nl)) {
			COB_CHECKED_FPUTC ('\r', fp);
		} else if ((opt == 0) ) {
			COB_CHECKED_FPUTC ('\r', fp);
		}
	}

	if ((opt == 0) 
	&& !(f->flag_select_features & COB_SELECT_LINAGE)
#if 0 /* TODO: activate on merge of file_features from trunk */
	&& ((f->file_features & COB_FILE_LS_LF)
	 || (f->file_features & COB_FILE_LS_CRLF))
#endif
	){
		/* At least add 1 LF */
		COB_CHECKED_FPUTC ('\n', fp);
		f->flag_needs_nl = 0;
	}

	/* WRITE BEFORE */
	if (opt & COB_WRITE_BEFORE) {
		ret = cob_file_write_opt (f, opt);
		if (ret) {
			return ret;
		}
		f->flag_needs_nl = 0;
	}
#ifdef READ_WRITE_NEEDS_FLUSH
	if (f->open_mode == COB_OPEN_I_O)	/* Required on some systems */
		fflush (fp);
#endif

	return COB_STATUS_00_SUCCESS;
}

static int
lineseq_rewrite (cob_file *f, const int opt)
{
	FILE	*fp = (FILE *)f->file;
	unsigned char	*p;
	const size_t		size = f->record->size;
	size_t		psize, slotlen;
	off_t		curroff;

#if 0 /* pipes are GC4+ only feature */
	if (f->flag_is_pipe) 
		return COB_STATUS_30_PERMANENT_ERROR;
#endif

	curroff = ftell (fp);	/* Current file position */

	p = f->record->data;
	psize = size;
	slotlen = curroff - f->record_off - 1;
#if 0 /* Note: file specific features are 4.x only ... */
	if ((f->file_features & COB_FILE_LS_CRLF)) {
		slotlen--;
	}
	if ((f->file_features & COB_FILE_LS_NULLS)) {
#else
	if (cobsetptr->cob_ls_uses_cr) {
		slotlen--;
	}
	if (cobsetptr->cob_ls_nulls) {
#endif
		size_t j;
		for (j = 0; j < size; j++) {
			if (p[j] < ' ') {
				psize++;
			}
		}
	}

	if (psize > slotlen) {
		return COB_STATUS_44_RECORD_OVERFLOW;
	}

	if (fseek (fp, (off_t)f->record_off, SEEK_SET) != 0) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}

	/* Write to the file */
	if (size > 0) {
#if 0 /* Note: file specific features are 4.x only ... */
		if ((f->file_features & COB_FILE_LS_VALIDATE)) {
#else
		if (cobsetptr->cob_ls_validate
		 && !f->flag_line_adv) {
#endif
			size_t i;
			p = f->record->data;
			for (i = 0; i < size; ++i, ++p) {
				if (IS_BAD_CHAR (*p)) {
					return COB_STATUS_71_BAD_CHAR;
				}
			}
			COB_CHECKED_FWRITE (fp, f->record->data, size);
		} else
#if 0 /* Note: file specific features are 4.x only ... */
		if ((f->file_features & COB_FILE_LS_NULLS)) {
#else
		if (cobsetptr->cob_ls_nulls) {
#endif
			size_t i, j;
			p = f->record->data;
			for (i=j=0; j < size; j++) {
				if (p[j] < ' ') {
					if (j - i > 0) {
						COB_CHECKED_FWRITE (fp, &p[i], j - i);
					}
					i = j + 1;
					COB_CHECKED_FPUTC (0x00, fp);
					COB_CHECKED_FPUTC (p[j], fp);
				}
			}
			if (i < size) {
				COB_CHECKED_FWRITE (fp, &p[i],(int)size - i);
			}
		} else {
			COB_CHECKED_FWRITE (fp, f->record->data, size);
		}
		if (psize < slotlen) {
			/* In case new record was shorter, pad with spaces */
			size_t i;
			for (i = psize; i < slotlen; i++) {
				COB_CHECKED_FPUTC (' ', fp);
			}
		}
	}

	if (fseek (fp, curroff, SEEK_SET) != 0) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
#ifdef READ_WRITE_NEEDS_FLUSH
	if (f->open_mode == COB_OPEN_I_O)	/* Required on some systems */
		fflush (fp);
#endif

	return COB_STATUS_00_SUCCESS;
}

/* RELATIVE */

static int
relative_start (cob_file *f, const int cond, cob_field *k)
{
	off_t		off;
	size_t		relsize;
	int		kindex;
	int		ksindex;
	int		kcond;
	struct stat	st;

#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_relative_start (f, cond, k);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif

	if (fstat (f->fd, &st) != 0 || st.st_size == 0) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}

	relsize = f->record_max + sizeof (f->record->size);

	/* Get the index */
	switch (cond) {
	case COB_FI:
		kcond = COB_GE;
		kindex = 0;
		break;
	case COB_LA:
		kcond = COB_LE;
		kindex = st.st_size / relsize;
		kindex--;
		break;
	case COB_LT:
	case COB_LE:
		kcond = cond;
		kindex = cob_get_int (k) - 1;
		/* Check against current file size */
		ksindex = st.st_size / relsize;
		ksindex--;
		if (kindex > ksindex) {
			kindex = ksindex;
			if (cond == COB_LT) {
				/* Cater for decrement below */
				kindex++;
			}
		}
		break;
	default:
		kcond = cond;
		kindex = cob_get_int (k) - 1;
		break;
	}

	if (kindex < 0) {
		/* Only valid ops are GE and GT in this case */
		switch (kcond) {
		case COB_GE:
			kindex = 0;
			break;
		case COB_GT:
			/* Set to cater for increment below */
			kindex = -1;
			break;
		default:
			return COB_STATUS_23_KEY_NOT_EXISTS;
		}
	}

	if (kcond == COB_LT) {
		kindex--;
		if (kindex < 0) {
			return COB_STATUS_23_KEY_NOT_EXISTS;
		}
	} else if (kcond == COB_GT) {
		kindex++;
	}

	f->flag_operation = 0;

	/* Seek index */
	for (;;) {
		if (kindex < 0) {
			break;
		}
		off = (off_t)kindex * relsize;
		if (off >= st.st_size) {
			if (kcond == COB_LT || kcond == COB_LE) {
				kindex--;
				continue;
			}
			break;
		}
		if (lseek (f->fd, off, SEEK_SET) == (off_t)-1) {
			break;
		}

		/* Check if a valid record */
		if (read (f->fd, &f->record->size, sizeof (f->record->size))
		    == sizeof (f->record->size) && f->record->size > 0) {
#if	0	/* RXWRXW - Set key - COBOL standards */
			cob_set_int (k, kindex + 1);
#endif
			lseek (f->fd, off, SEEK_SET);
			return COB_STATUS_00_SUCCESS;
		}

		switch (kcond) {
		case COB_EQ:
			return COB_STATUS_23_KEY_NOT_EXISTS;
		case COB_LT:
		case COB_LE:
			kindex--;
			break;
		case COB_GT:
		case COB_GE:
			kindex++;
			break;
		}
	}
	return COB_STATUS_23_KEY_NOT_EXISTS;
}

static int
relative_read (cob_file *f, cob_field *k, const int read_opts)
{
	off_t	off;
	size_t	relsize;
	int	relnum;
#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_relative_read (f, k, read_opts);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#else
	COB_UNUSED (read_opts);
#endif

	if (unlikely (f->flag_operation != 0)) {
		f->flag_operation = 0;
		lseek (f->fd, (off_t)0, SEEK_CUR);
	}

	relnum = cob_get_int (k) - 1;
	if (relnum < 0) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	relsize = f->record_max + sizeof (f->record->size);
	off = (off_t)relnum * relsize;
	if (lseek (f->fd, off, SEEK_SET) == (off_t)-1 ||
	    read (f->fd, &f->record->size, sizeof (f->record->size))
		   != sizeof (f->record->size)) {
			return COB_STATUS_23_KEY_NOT_EXISTS;
	}

	if (f->record->size == 0) {
		lseek (f->fd, off, SEEK_SET);
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}

	if (read (f->fd, f->record->data, f->record_max) != (int)f->record_max) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	return COB_STATUS_00_SUCCESS;
}

static int
relative_read_next (cob_file *f, const int read_opts)
{
	off_t	curroff;
	off_t	relsize;
	int		relnum;
	int		bytesread;
	cob_u32_t	moveback;
	struct stat	st;

#ifdef	WITH_SEQRA_EXTFH
	int		extfh_ret;

	extfh_ret = extfh_relative_read_next (f, read_opts);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif

	if (unlikely (f->flag_operation != 0)) {
		f->flag_operation = 0;
		lseek (f->fd, (off_t)0, SEEK_CUR);
	}

	relsize = ((off_t) f->record_max) + sizeof (f->record->size);
	if (fstat (f->fd, &st) != 0 || st.st_size == 0) {
		return COB_STATUS_10_END_OF_FILE;
	}
	/* LCOV_EXCL_START */
	if (st.st_size < relsize) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	/* LCOV_EXCL_STOP */

	curroff = lseek (f->fd, (off_t)0, SEEK_CUR);
	moveback = 0;

	switch (read_opts & COB_READ_MASK) {
	case COB_READ_FIRST:
		curroff = lseek (f->fd, (off_t)0, SEEK_SET);
		break;
	case COB_READ_LAST:
		curroff = st.st_size - relsize;
		curroff = lseek (f->fd, curroff, SEEK_SET);
		moveback = 1;
		break;
	case COB_READ_PREVIOUS:
		if (f->flag_first_read) {
			break;
		} else if (curroff > relsize) {
			curroff -= (relsize * 2);
			curroff = lseek (f->fd, curroff, SEEK_SET);
		} else {
			return COB_STATUS_10_END_OF_FILE;
		}
		moveback = 1;
		break;
	case COB_READ_NEXT:
	default:
		break;
	}

	for (;;) {
		bytesread = read (f->fd, &f->record->size, sizeof (f->record->size));
		if (bytesread != sizeof (f->record->size)) {
			if (bytesread != 0) {
				return COB_STATUS_30_PERMANENT_ERROR;
			} else {
				break;
			}
		}

		if (f->record->size > 0) {
			if (read (f->fd, f->record->data, f->record_max) != (int)f->record_max) {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
			if (f->keys[0].field) {
				relnum = (int)((curroff / relsize) + 1);
				cob_set_int (f->keys[0].field, 0);
				if (cob_add_int (f->keys[0].field, relnum,
						 COB_STORE_KEEP_ON_OVERFLOW) != 0) {
					/* reset position after read */
					(void) lseek (f->fd, curroff, SEEK_SET);
					return COB_STATUS_14_OUT_OF_KEY_RANGE;
				}
			}
			if (moveback) {
				curroff -= relsize;
				curroff = lseek (f->fd, curroff, SEEK_SET);
			}
			return COB_STATUS_00_SUCCESS;
		}
		if (moveback) {
			if (curroff > relsize) {
				curroff -= (relsize * 2);
				curroff = lseek (f->fd, curroff, SEEK_SET);
			} else {
				break;
			}
		} else {
			curroff = lseek (f->fd, (off_t) f->record_max, SEEK_CUR);
		}
	}
	return COB_STATUS_10_END_OF_FILE;
}

static int
relative_write (cob_file *f, const int opt)
{
	off_t	off;
	size_t	size;
	size_t	relsize;
	int	i;
	int	kindex;
#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_relative_write (f, opt);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#else
	COB_UNUSED (opt);
#endif

	if (unlikely (f->flag_operation == 0)) {
		f->flag_operation = 1;
		lseek (f->fd, (off_t)0, SEEK_CUR);
	}

	relsize = f->record_max + sizeof (f->record->size);
	if (f->access_mode != COB_ACCESS_SEQUENTIAL) {
		kindex = cob_get_int (f->keys[0].field) - 1;
		if (kindex < 0) {
			return COB_STATUS_24_KEY_BOUNDARY;
		}
		off = ((off_t)relsize * kindex);
		if (lseek (f->fd, off, SEEK_SET) == (off_t)-1) {
			return COB_STATUS_24_KEY_BOUNDARY;
		}
		if (read (f->fd, &size, sizeof (size)) > 0) {
			if (size > 0) {
				return COB_STATUS_22_KEY_EXISTS;
			}
		}
	} else {
		off = lseek (f->fd, (off_t)0, SEEK_CUR);
	}
	/* reset position after read;
	   TODO: add a test case (when disabled: internal tests pass,
	   NIST IX fail) */
	(void)lseek (f->fd, off, SEEK_SET);

	COB_CHECKED_WRITE (f->fd, &f->record->size, sizeof (f->record->size));
	COB_CHECKED_WRITE (f->fd, f->record->data, f->record_max);

	/* Update RELATIVE KEY */
	if (f->access_mode == COB_ACCESS_SEQUENTIAL) {
		if (f->keys[0].field) {
			off += relsize;
			i = (int)(off / relsize);
			cob_set_int (f->keys[0].field, i);
		}
	}

	return COB_STATUS_00_SUCCESS;
}

static int
relative_rewrite (cob_file *f, const int opt)
{
	off_t	off;
	size_t	relsize;
	int	relnum;
#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_relative_rewrite (f, opt);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#else
	COB_UNUSED (opt);
#endif

	f->flag_operation = 1;
	if (f->access_mode == COB_ACCESS_SEQUENTIAL) {
		lseek (f->fd, -(off_t) f->record_max, SEEK_CUR);
	} else {
		relsize = f->record_max + sizeof (f->record->size);
		relnum = cob_get_int (f->keys[0].field) - 1;
		if (relnum < 0) {
			return COB_STATUS_24_KEY_BOUNDARY;
		}
		off = (off_t)relnum * relsize;
		if (lseek (f->fd, off, SEEK_SET) == (off_t)-1 ||
		    read (f->fd, &f->record->size, sizeof (f->record->size))
			   != sizeof (f->record->size)) {
				return COB_STATUS_23_KEY_NOT_EXISTS;
		}
		lseek (f->fd, (off_t)0, SEEK_CUR);
	}

	COB_CHECKED_WRITE (f->fd, f->record->data, f->record_max);
	return COB_STATUS_00_SUCCESS;
}

static int
relative_delete (cob_file *f)
{
	off_t	off;
	size_t	relsize;
	int	relnum;
#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_relative_delete (f);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif

	f->flag_operation = 1;
	relnum = cob_get_int (f->keys[0].field) - 1;
	if (relnum < 0) {
		return COB_STATUS_24_KEY_BOUNDARY;
	}
	relsize = f->record_max + sizeof (f->record->size);
	off = (off_t)relnum * relsize;
	if (lseek (f->fd, off, SEEK_SET) == (off_t)-1
	 || read (f->fd, &f->record->size, sizeof (f->record->size))
		 != sizeof (f->record->size)) {
			return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	/* reset position after read;
	   TODO: add a test case (when disabled: internal tests pass,
	   NIST IX fail) */
	(void)lseek (f->fd, off, SEEK_SET);

	f->record->size = 0;
	COB_CHECKED_WRITE (f->fd, &f->record->size, sizeof (f->record->size));
	lseek (f->fd, (off_t) f->record_max, SEEK_CUR);
	return COB_STATUS_00_SUCCESS;
}

/* INDEXED */

#ifdef	WITH_ANY_ISAM

/* Translate ISAM status to COBOL status */
static int
fisretsts (const int default_status)
{
	switch (ISERRNO) {
	case 0:
		return COB_STATUS_00_SUCCESS;
	case ENOREC:
		return COB_STATUS_23_KEY_NOT_EXISTS;
	case EENDFILE:
		if (default_status != COB_STATUS_23_KEY_NOT_EXISTS) {
			return COB_STATUS_10_END_OF_FILE;
		}
		break;
	case EDUPL:
	case EKEXISTS:
		return COB_STATUS_22_KEY_EXISTS;
	case EPERM:
	case EACCES:
	case EISDIR:
		return COB_STATUS_37_PERMISSION_DENIED;
	case ENOENT:
		return COB_STATUS_35_NOT_EXISTS;
	case EBADFILE:
		return COB_STATUS_30_PERMANENT_ERROR;
	case ELOCKED:
		return COB_STATUS_51_RECORD_LOCKED;
	case EFLOCKED:
		return COB_STATUS_61_FILE_SHARING;
	case ENOCURR:
		if (default_status != COB_STATUS_10_END_OF_FILE) {
			return COB_STATUS_21_KEY_INVALID;
		}
		break;
	default:
		break;
	}
	return default_status;
}

/* Free memory for indexfile packet */

static void
freefh (struct indexfile *fh)
{
	if (fh == NULL) {
		return;
	}
	if (fh->filename) {
		cob_free ((void *)fh->filename);
	}
	if (fh->savekey) {
		cob_free ((void *)fh->savekey);
	}
	if (fh->recwrk) {
		cob_free ((void *)fh->recwrk);
	}
	cob_free ((void *)fh);
}

/* Restore ISAM file positioning */
static void
restorefileposition (cob_file *f)
{
	struct indexfile	*fh;
	struct keydesc		k0;

	fh = f->file;
	memset ((void *)&k0, 0, sizeof (k0));
	if (fh->saverecnum >= 0) {
		/* Switch back to index */
		ISRECNUM = fh->saverecnum;
		/* Switch to recnum mode */
		isstart (fh->isfd, &k0, 0, (void *)fh->recwrk, ISEQUAL);
		/* Read by record number */
		isread (fh->isfd, (void *)fh->recwrk, ISEQUAL);
		/* Read by current key value */
		isstart (fh->isfd, &fh->key[f->curkey], 0,
			 (void *)fh->recwrk, ISGTEQ);
		isread (fh->isfd, (void *)fh->recwrk, ISGTEQ);
		while (ISRECNUM != fh->saverecnum) {
			/* Read back into position */
			if (isread (fh->isfd, (void *)fh->recwrk, ISNEXT)) {
				break;
			}
		}
		if (ISRECNUM == fh->saverecnum) {
			if (fh->readdir == ISNEXT) {
				/* Back off by one so next read gets this */
				isread (fh->isfd, (void *)fh->recwrk, ISPREV);
			} else {
				isread (fh->isfd, (void *)fh->recwrk, ISNEXT);
			}
		}
	} else if (fh->readdone && f->curkey == 0) {
		indexed_restorekey(fh, NULL, 0);
		isstart (fh->isfd, &fh->key[f->curkey], 0,
			 (void *)fh->recwrk, ISGTEQ);
	}
}

/* Save ISAM file positioning information for later 'restorefileposition' */

static void
savefileposition (cob_file *f)
{
	struct indexfile	*fh;

	fh = f->file;
	if (f->curkey >= 0 && fh->readdir != -1) {
		/* Switch back to index */
		if (fh->wrkhasrec != fh->readdir) {
			fh->eofpending = 0;
			fh->wrkhasrec = 0;
			/* Read next record in file */
			if (isread (fh->isfd, (void *)fh->recwrk, fh->readdir)) {
				fh->saverecnum = -1;
				fh->saveerrno = ISERRNO;
				if (fh->saveerrno == EENDFILE ||
				    fh->saveerrno == ENOREC)  {
					fh->eofpending = fh->readdir;
				}
			} else {
				fh->saverecnum = ISRECNUM;
				fh->saveerrno = 0;
			}
			/* Restore saved record data */
			memcpy (fh->recwrk, f->record->data, f->record_max);
		}
	} else {
		fh->saverecnum = -1;
	}
}
#endif	/* WITH_ANY_ISAM */

#ifdef	WITH_DB

#if	0	/* RXWRXW - BDB msg */
static void
bdb_msgcall_set (DB_ENV *dbe, const char *err)
{
	COB_UNUSED (dbe);

	cob_runtime_error (_("BDB error: %s"), err);
	/* FIXME: check if possible to be passed to original caller returning status 39 */
	cob_hard_failure ()
}

static void
bdb_errcall_set (DB_ENV *dbe, const char *prefix, const char *err)
{
	COB_UNUSED (dbe);

	cob_runtime_error (_("BDB error: %s %s"), prefix, err);
	/* FIXME: check if possible to be passed to original caller returning status 39 */
	cob_hard_failure ()
}
#endif

static int
join_environment (void)
{
	cob_u32_t	flags;
	int		ret;

	ret = db_env_create (&bdb_env, 0);
	if (ret) {
		cob_runtime_error (_("cannot join BDB environment (%s), error: %d %s"),
				   "env_create", ret, db_strerror (ret));
		return ret;
	}
#if	0	/* RXWRXW - BDB msg */
	bdb_env->set_errcall (bdb_env, bdb_errcall_set);
#if (DB_VERSION_MAJOR > 4) || ((DB_VERSION_MAJOR == 4) && (DB_VERSION_MINOR > 2))
	bdb_env->set_msgcall (bdb_env, bdb_msgcall_set);
#endif
#else
	bdb_env->set_errfile (bdb_env, stderr);
#if (DB_VERSION_MAJOR > 4) || ((DB_VERSION_MAJOR == 4) && (DB_VERSION_MINOR > 2))
	bdb_env->set_msgfile (bdb_env, stderr);
#endif
#endif
	bdb_env->set_cachesize (bdb_env, 0, 2*1024*1024, 0);
	bdb_env->set_alloc (bdb_env, cob_malloc, realloc, cob_free);
	flags = DB_CREATE | DB_INIT_MPOOL | DB_INIT_CDB;
	ret = bdb_env->open (bdb_env, cobsetptr->bdb_home, flags, 0);
	if (ret) {
		cob_runtime_error (_("cannot join BDB environment (%s), error: %d %s"),
				   "env->open", ret, db_strerror (ret));
		bdb_env->close (bdb_env, 0);
		bdb_env = NULL;
		return ret;
	}
#if (DB_VERSION_MAJOR > 4) || ((DB_VERSION_MAJOR == 4) && (DB_VERSION_MINOR > 1))
	bdb_env->get_data_dirs (bdb_env, &bdb_data_dir);
#endif
	bdb_env->lock_id (bdb_env, &bdb_lock_id);
	return 0;
}

static void
set_dbt (struct indexed_file *p, DBT *dbt, const char *key, const unsigned int keylen)
{
	size_t	len = keylen + p->filenamelen + 1;
	if (len > rlo_size) {
		record_lock_object = cob_realloc (record_lock_object, rlo_size, len);
		rlo_size = len;
	}
	memcpy ((char *)record_lock_object, p->filename,
		(size_t)(p->filenamelen + 1));
	memcpy ((char *)record_lock_object + p->filenamelen + 1, key,
		(size_t)keylen);
	memset (dbt, 0, sizeof (DBT));
	dbt->size = (cob_dbtsize_t) len;
	dbt->data = record_lock_object;
}


static int
lock_record (cob_file *f, const char *key, const unsigned int keylen)
{
	struct indexed_file	*p = f->file;
	DBT			dbt;
	int			ret;

	set_dbt (p, &dbt, key, keylen);
	ret = bdb_env->lock_get (bdb_env, p->bdb_lock_id, DB_LOCK_NOWAIT,
				&dbt, DB_LOCK_WRITE, &p->bdb_record_lock);
	if (!ret) {
		p->record_locked = 1;
	}
	return ret;
}

static int
test_record_lock (cob_file *f, const char *key, const unsigned int keylen)
{
	struct indexed_file	*p = f->file;
	DBT			dbt;
	DB_LOCK			test_lock;
	int			ret;

	set_dbt (p, &dbt, key, keylen);
	ret = bdb_env->lock_get (bdb_env, p->bdb_lock_id, DB_LOCK_NOWAIT,
				&dbt, DB_LOCK_WRITE, &test_lock);
	if (!ret) {
		bdb_env->lock_put (bdb_env, &test_lock);
	}
	return ret;
}

static int
unlock_record (cob_file *f)
{
	struct indexed_file	*p = f->file;

	if (p->record_locked == 0) {
		return 0;
	}
	p->record_locked = 0;
	return bdb_env->lock_put (bdb_env, &p->bdb_record_lock);
}

/* Get the next number in a set of duplicates */
static unsigned int
get_dupno (cob_file *f, const cob_u32_t i)
{
	struct indexed_file	*p = f->file;
	int			ret;
	unsigned int		dupno;

	dupno = 0;
	bdb_setkey(f, i);
	memcpy (p->temp_key, p->key.data, (size_t)p->maxkeylen);
	p->db[i]->cursor (p->db[i], NULL, &p->cursor[i], 0);
	ret = DB_SEQ (p->cursor[i], DB_SET_RANGE);
	while (ret == 0 && memcmp (p->key.data, p->temp_key, (size_t)p->key.size) == 0) {
		memcpy (&dupno, (cob_u8_ptr)p->data.data + p->primekeylen, sizeof (unsigned int));
		ret = DB_SEQ (p->cursor[i], DB_NEXT);
	}
	p->cursor[i]->c_close (p->cursor[i]);
	p->cursor[i] = NULL;
	dupno = COB_DUPSWAP(dupno);
	return ++dupno;
}

/* read file with all alternate keys that don't allow duplicates
   to check if records exist already, returns 1 if true */
static int
check_alt_keys (cob_file *f, const int rewrite)
{
	struct indexed_file	*p = f->file;
	size_t			i;

	for (i = 1; i < f->nkeys; ++i) {
		if (!f->keys[i].tf_duplicates) {
			bdb_setkey (f, i);
			if (DB_GET (p->db[i], 0) == 0) {
				if (rewrite) {
					if (bdb_cmpkey (f, p->data.data, f->record->data, 0, 0)) {
						return 1;
					}
				} else {
					return 1;
				}
			}
		}
	}
	return 0;
}

static int
indexed_write_internal (cob_file *f, const int rewrite, const int opt)
{
	struct indexed_file	*p = f->file;
	cob_u32_t		i, len;
	unsigned int		dupno;
	cob_u32_t		flags;
	int			close_cursor, ret;

	if (bdb_env) {
		flags = DB_WRITECURSOR;
	} else {
		flags = 0;
	}
	ret = COB_STATUS_00_SUCCESS;
	if (p->write_cursor_open) {
		close_cursor = 0;
	} else {
		p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], flags);
		p->write_cursor_open = 1;
		close_cursor = 1;
	}

	/* Check duplicate alternate keys */
	if (f->nkeys > 1 && !rewrite) {
		if (check_alt_keys (f, 0)) {
			if (close_cursor) {
				p->cursor[0]->c_close (p->cursor[0]);
				p->cursor[0] = NULL;
				p->write_cursor_open = 0;
			}
			return COB_STATUS_22_KEY_EXISTS;
		}
		bdb_setkey (f, 0);
	}

	/* Write data */
	if (p->cursor[0]->c_get (p->cursor[0], &p->key, &p->data, DB_SET) == 0) {
		if (close_cursor) {
			p->cursor[0]->c_close (p->cursor[0]);
			p->cursor[0] = NULL;
			p->write_cursor_open = 0;
		}
		return COB_STATUS_22_KEY_EXISTS;
	}
	p->data.data = f->record->data;
	p->data.size = (cob_dbtsize_t) f->record->size;
	p->cursor[0]->c_put (p->cursor[0], &p->key, &p->data, DB_KEYFIRST);

	/* Write secondary keys */
	p->data = p->key;
	for (i = 1; i < f->nkeys; ++i) {
		if (rewrite && ! p->rewrite_sec_key[i]) {
			continue;
		}
		if (bdb_suppresskey (f, i)) {
			continue;
		}
		bdb_setkey (f, i);
		if (f->keys[i].tf_duplicates) {
			flags =  0;
			dupno = get_dupno (f, i);
			if (dupno > 1) {
				ret = COB_STATUS_02_SUCCESS_DUPLICATE;
			}
			dupno = COB_DUPSWAP (dupno);
			len = bdb_savekey(f, p->temp_key, f->record->data, 0);
			p->data.data = p->temp_key;
			p->data.size = len;
			memcpy (((char*)(p->data.data)) + p->data.size, &dupno, sizeof (unsigned int));
			p->data.size += sizeof (unsigned int);
		} else {
			len = bdb_savekey(f, p->temp_key, f->record->data, 0);
			p->data.data = p->temp_key;
			p->data.size = len;
			flags = DB_NOOVERWRITE;
			dupno = 0;
		}
		bdb_setkey (f, i);

		if (DB_PUT (p->db[i], flags) != 0) {
			if (close_cursor) {
				p->cursor[0]->c_close (p->cursor[0]);
				p->cursor[0] = NULL;
				p->write_cursor_open = 0;
			}
			return COB_STATUS_22_KEY_EXISTS;
		}
	}

	if (opt & COB_WRITE_LOCK) {
		if (bdb_env != NULL) {
			bdb_setkey (f, 0);
			if (lock_record (f, p->key.data, p->key.size)) {
				if (close_cursor) {
					p->cursor[0]->c_close (p->cursor[0]);
					p->cursor[0] = NULL;
					p->write_cursor_open = 0;
				}
				return COB_STATUS_51_RECORD_LOCKED;
			}
		}
	}
	if (close_cursor) {
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
		p->write_cursor_open = 0;
	}
	return ret;
}

static int
indexed_start_internal (cob_file *f, const int cond, cob_field *key,
			const int read_opts, const int test_lock)
{
	struct indexed_file	*p = f->file;
	int			ret, len, fullkeylen, partlen;
	unsigned int		dupno;
	int			key_index;

	dupno = 0;
	ret = 0;
	/* Look up for the key */
	key_index = bdb_findkey (f, key, &fullkeylen, &partlen);
	if (key_index < 0) {
		f->mapkey = -1;
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	p->key_index = key_index;
	f->curkey = (short)key_index;

	/* Search */
	bdb_setkey (f, p->key_index);
	p->key.size = partlen;		/* may be partial key */
	/* The open cursor makes this function atomic */
	if (p->key_index != 0) {
		p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], 0);
	}
	p->db[p->key_index]->cursor (p->db[p->key_index], NULL, &p->cursor[p->key_index], 0);
	if (cond == COB_FI) {
		ret = DB_SEQ (p->cursor[p->key_index], DB_FIRST);
	} else if (cond == COB_LA) {
		ret = DB_SEQ (p->cursor[p->key_index], DB_LAST);
	} else {
		ret = DB_SEQ (p->cursor[p->key_index], DB_SET_RANGE);
	}
	switch (cond) {
	case COB_EQ:
		if (ret == 0) {
			ret = bdb_cmpkey (f, p->key.data, f->record->data, p->key_index, partlen);
		}
		break;
	case COB_LT:
		if (ret != 0) {
			ret = DB_SEQ (p->cursor[p->key_index], DB_LAST);
		} else {
			ret = DB_SEQ (p->cursor[p->key_index], DB_PREV);
		}
		break;
	case COB_LE:
		if (ret != 0) {
			ret = DB_SEQ (p->cursor[p->key_index], DB_LAST);
		} else if (bdb_cmpkey (f, p->key.data, f->record->data, p->key_index, partlen) != 0) {
			ret = DB_SEQ (p->cursor[p->key_index], DB_PREV);
		} else if (f->keys[p->key_index].tf_duplicates) {
			ret = DB_SEQ (p->cursor[p->key_index], DB_NEXT_NODUP);
			if (ret != 0) {
				ret = DB_SEQ (p->cursor[p->key_index], DB_LAST);
			} else {
				ret = DB_SEQ (p->cursor[p->key_index], DB_PREV);
			}
		}
		break;
	case COB_GT:
		while (ret == 0 && bdb_cmpkey (f, p->key.data, f->record->data, p->key_index, partlen) == 0) {
			ret = DB_SEQ (p->cursor[p->key_index], DB_NEXT);
		}
		break;
	case COB_GE:
		/* nothing */
		break;
	case COB_FI:
		/* nothing */
		break;
	case COB_LA:
		/* nothing */
		break;
	}

	if (ret == 0 && p->key_index > 0) {
		/* Temporarily save alternate key */
		len = p->key.size;
		memcpy (p->temp_key, p->key.data, len);
		if (f->keys[p->key_index].tf_duplicates) {
			memcpy (&dupno, (cob_u8_ptr)p->data.data + p->primekeylen, sizeof (unsigned int));
			dupno = COB_DUPSWAP (dupno);
		}
		p->key.data = p->data.data;
		p->key.size = p->primekeylen;
		ret = DB_GET (p->db[0], 0);
	}

	if (ret == 0 && test_lock) {
		if (!(read_opts & COB_READ_IGNORE_LOCK)) {
			ret = test_record_lock (f, p->key.data, p->key.size);
			if (ret) {
				p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
				p->cursor[p->key_index] = NULL;
				if (p->key_index != 0) {
					p->cursor[0]->c_close (p->cursor[0]);
					p->cursor[0] = NULL;
				}
				return COB_STATUS_51_RECORD_LOCKED;
			}
		}
		if (read_opts & COB_READ_LOCK) {
			ret = lock_record (f, p->key.data, p->key.size);
			if (ret) {
				p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
				p->cursor[p->key_index] = NULL;
				if (p->key_index != 0) {
					p->cursor[0]->c_close (p->cursor[0]);
					p->cursor[0] = NULL;
				}
				return COB_STATUS_51_RECORD_LOCKED;
			}
		}
	}

	if (ret == 0) {
		if (p->key_index == 0) {
			memcpy (p->last_readkey[0], p->key.data, p->primekeylen);
		} else {
			int keylen = bdb_keylen (f, p->key_index);
			/* LCOV_EXCL_START */
			if (partlen <= 0) {
				cob_runtime_error (_("invalid internal call of %s"),
					"indexed_start_internal/bdb_keylen");
				cob_hard_failure_internal ();
			}
			/* LCOV_EXCL_STOP */
			memcpy (p->last_readkey[p->key_index],
				    p->temp_key, keylen);
			memcpy (p->last_readkey[p->key_index + f->nkeys],
				    p->key.data, p->primekeylen);
			if (f->keys[p->key_index].tf_duplicates) {
				p->last_dupno[p->key_index] = dupno;
			}
		}
	}

	p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
	p->cursor[p->key_index] = NULL;
	if (p->key_index != 0) {
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
	}

	return (ret == 0) ? COB_STATUS_00_SUCCESS : COB_STATUS_23_KEY_NOT_EXISTS;
}

static int
indexed_delete_internal (cob_file *f, const int rewrite)
{
	struct indexed_file	*p = f->file;
	int			i,len;
	DBT			prim_key;
	cob_u32_t		flags;
	int			close_cursor;

	if (bdb_env) {
		flags = DB_WRITECURSOR;
	} else {
		flags = 0;
	}
	if (p->write_cursor_open) {
		close_cursor = 0;
	} else {
		p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], flags);
		p->write_cursor_open = 1;
		close_cursor = 1;
	}
	if (bdb_env != NULL) {
		unlock_record (f);
	}
	/* Find the primary key */
	if (f->access_mode != COB_ACCESS_SEQUENTIAL) {
		bdb_setkey(f, 0);
	}
	if (DB_SEQ (p->cursor[0], DB_SET) != 0
	 && f->access_mode != COB_ACCESS_SEQUENTIAL) {
		if (close_cursor) {
			p->cursor[0]->c_close (p->cursor[0]);
			p->cursor[0] = NULL;
			p->write_cursor_open = 0;
		}
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	if (bdb_env != NULL) {
		if (test_record_lock (f, p->key.data, p->key.size)) {
			if (close_cursor) {
				p->cursor[0]->c_close (p->cursor[0]);
				p->cursor[0] = NULL;
				p->write_cursor_open = 0;
			}
			return COB_STATUS_51_RECORD_LOCKED;
		}
	}
	prim_key = p->key;
	memcpy(p->saverec, p->data.data, p->data.size);		/* Save old record image */
	memcpy(p->temp_key,prim_key.data,prim_key.size);	/* Save primary key value */
	prim_key.data = p->temp_key;

	/* Delete the secondary keys */
	for (i = 1; i < f->nkeys; ++i) {
		len = bdb_savekey(f, p->savekey, p->saverec, i);
		p->key.data = p->savekey;
		p->key.size = (cob_dbtsize_t) len;
		/* rewrite: no delete if secondary key is unchanged */
		if (rewrite) {
			bdb_savekey (f, p->suppkey, p->saverec, i);
			p->rewrite_sec_key[i] = bdb_cmpkey(f, p->suppkey, f->record->data, i, 0);
			if (!p->rewrite_sec_key[i]) {
				continue;
			}
		}
		if (!f->keys[i].tf_duplicates) {
			DB_DEL (p->db[i], &p->key, 0);
		} else {
			DBT	sec_key = p->key;

			p->db[i]->cursor (p->db[i], NULL, &p->cursor[i], flags);
			if (DB_SEQ (p->cursor[i], DB_SET_RANGE) == 0) {
				while (sec_key.size == p->key.size
				&& memcmp (p->key.data, sec_key.data, (size_t)sec_key.size) == 0) {
					if (memcmp (p->data.data, prim_key.data, (size_t)prim_key.size) == 0) {
						p->cursor[i]->c_del (p->cursor[i], 0);
					}
					if (DB_SEQ (p->cursor[i], DB_NEXT) != 0) {
						break;
					}
				}
			}
			p->cursor[i]->c_close (p->cursor[i]);
			p->cursor[i] = NULL;
		}
	}

	/* Delete the record */
	p->cursor[0]->c_del (p->cursor[0], 0);

	if (close_cursor) {
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
		p->write_cursor_open = 0;
	}
	return COB_STATUS_00_SUCCESS;
}

/* Check if a file exists in bdb data dirs */

static int
is_absolute (const char *filename)
{
#ifdef	_WIN32
	if (filename[0] == '/' || filename[0] == '\\') {
		return 1;
	} else {
		if (isalpha (filename[0]) && filename[1] == ':' &&
		  (filename[2] == '/' || filename[2] == '\\')) {
			return 1;
		} else {
			return 0;
		}
	}
#else
	if (filename[0] == '/') {
		return 1;
	} else {
		return 0;
	}
#endif
}

static int
bdb_nofile (const char *filename)
{
	cob_u32_t	i;

	if (!bdb_env || is_absolute (filename)) {
		errno = 0;
		if (access (filename, F_OK) && errno == ENOENT) {
			return 1;
		}
		return 0;
	}

	for (i = 0; bdb_data_dir && bdb_data_dir[i]; ++i) {
		if (is_absolute (bdb_data_dir[i])) {
			snprintf (bdb_buff, (size_t)COB_SMALL_MAX, "%s%c%s",
				  bdb_data_dir[i], SLASH_CHAR, filename);
		} else {
			snprintf (bdb_buff, (size_t)COB_SMALL_MAX, "%s%c%s%c%s",
				  cobsetptr->bdb_home, SLASH_CHAR, bdb_data_dir[i], SLASH_CHAR, filename);
		}
		bdb_buff[COB_SMALL_MAX] = 0;	/* silence analyzer */
		errno = 0;
		if (access (bdb_buff, F_OK) == 0 || errno != ENOENT) {
			return 0;
		}
	}
	if (i == 0) {
		snprintf (bdb_buff, (size_t)COB_SMALL_MAX, "%s%c%s",
			  cobsetptr->bdb_home, SLASH_CHAR, filename);
		bdb_buff[COB_SMALL_MAX] = 0; /* silence analyzer */
		errno = 0;
		if (access (bdb_buff, F_OK) == 0 || errno != ENOENT) {
			return 0;
		}
	}
	return 1;
}

#endif	/* WITH_DB */

/* Delete file */

static void
indexed_file_delete (cob_file *f, const char *filename)
{
#ifdef	WITH_ANY_ISAM
	COB_UNUSED (f);

	snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s.idx", filename);
	file_open_buff[COB_FILE_MAX] = 0;
	unlink (file_open_buff);
	snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s.dat", filename);
	file_open_buff[COB_FILE_MAX] = 0;
	unlink (file_open_buff);
#elif	defined(WITH_DB)
	size_t	i;

	for (i = 0; i < f->nkeys; ++i) {
		if (i == 0) {
			snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s",
				  filename);
		} else {
			snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s.%d",
				  filename, (int)i);
		}
		file_open_buff[COB_FILE_MAX] = 0;
		errno = 0;
		unlink (file_open_buff);
	}
#else
	COB_UNUSED (f);
	COB_UNUSED (filename);
#endif
}

/* OPEN INDEXED file */

static int
indexed_open (cob_file *f, char *filename,
		const enum cob_open_mode mode, const int sharing)
{
	/* Note filename points to file_open_name */
	/* cob_chk_file_mapping manipulates file_open_name directly */

#ifdef	WITH_INDEX_EXTFH
	int		ret;

	ret = extfh_indexed_locate (f, filename);
	switch (ret) {
	case COB_NOT_CONFIGURED:
		cob_chk_file_mapping ();
		errno = 0;
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

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh;
	size_t			k;
	int			ret,len;
	int			omode;
	int			lmode;
	int			vmode;
	int			dobld;
	int			isfd;
	int			checkvalue;
	struct keydesc		kd;
	struct dictinfo		di;		/* Defined in (c|d|vb)isam.h */

	COB_UNUSED (sharing);

	cob_chk_file_mapping ();

	if (mode == COB_OPEN_INPUT) {
		checkvalue = R_OK;
	} else {
		checkvalue = R_OK | W_OK;
	}

	snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s.idx", filename);
	file_open_buff[COB_FILE_MAX] = 0;
	errno = 0;
	if (access (file_open_buff, checkvalue)) {
		if (!(errno == ENOENT &&
		      (mode == COB_OPEN_OUTPUT || f->flag_optional == 1))) {
			switch (errno) {
			case ENOENT:
				return COB_STATUS_35_NOT_EXISTS;
			case EACCES:
				return COB_STATUS_37_PERMISSION_DENIED;
			default:
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
	}

	snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s.dat", filename);
	file_open_buff[COB_FILE_MAX] = 0;
	errno = 0;
	if (access (file_open_buff, checkvalue)) {
		if (!(errno == ENOENT &&
		      (mode == COB_OPEN_OUTPUT || f->flag_optional == 1))) {
			switch (errno) {
			case ENOENT:
				return COB_STATUS_35_NOT_EXISTS;
			case EACCES:
				return COB_STATUS_37_PERMISSION_DENIED;
			default:
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
	}

	ret = COB_STATUS_00_SUCCESS;
	omode = 0;
	lmode = 0;
	vmode = 0;
	dobld = 0;
	isfd = -1;
#ifdef	ISVARLEN
	if (f->record_min != f->record_max) {
		vmode = ISVARLEN;
		ISRECLEN = f->record_min;
	}
#endif
	if (!f->lock_mode) {
		if (mode != COB_OPEN_INPUT) {
			lmode = ISEXCLLOCK;
		} else {
			lmode = ISMANULOCK;
		}
	} else if ((f->lock_mode & COB_FILE_EXCLUSIVE)) {
		lmode = ISEXCLLOCK;
	} else if ((f->lock_mode & COB_LOCK_AUTOMATIC) && mode != COB_OPEN_INPUT) {
		lmode = ISAUTOLOCK;
	} else {
		lmode = ISMANULOCK;
	}
	switch (mode) {
	case COB_OPEN_INPUT:
		omode = ISINPUT;
		break;
	case COB_OPEN_OUTPUT:
		lmode = ISEXCLLOCK;
		omode = ISOUTPUT;
		ISERRNO = 0;
		isfd = isopen ((void *)filename, ISINPUT | ISEXCLLOCK | vmode);
		if (ISERRNO == EFLOCKED) {
			return COB_STATUS_61_FILE_SHARING;
		} else {
			if (isfd >= 0) {
				isfullclose (isfd);
			}
			iserase ((void *)filename);
			ISERRNO = 0;
		}
		dobld = 1;
		break;
	case COB_OPEN_I_O:
		omode = ISINOUT;
		break;
	case COB_OPEN_EXTEND:
		lmode = ISEXCLLOCK;
		omode = ISINOUT;
		break;
	}
	fh = cob_malloc (sizeof (struct indexfile) +
			 ((sizeof (struct keydesc)) * (f->nkeys + 1)));
	/* Copy index information */
	for (k = 0; k < f->nkeys; ++k) {
		len = indexed_keydesc(f, &fh->key[k], &f->keys[k]);
		if (fh->lenkey < len) {
			fh->lenkey = len;
		}
	}
	ISERRNO = 0;
	fh->lmode = 0;
	if (dobld) {
dobuild:
		isfd = isbuild ((void *)filename, (int)f->record_max, &fh->key[0],
				vmode | ISINOUT | ISEXCLLOCK);
#if 0 /* activate on later merge of locking enhancements */
		f->flag_file_lock = 1;
#endif
		if (ISERRNO == EEXIST
#if 1 /* CHECKME: guard added by Simon, needed ? */
		 && omode == ISOUTPUT
#endif
		 && isfd < 0) {
			/* Erase file and redo the 'isbuild' */
			iserase ((void *)filename);
			isfd = isbuild ((void *)filename, (int)f->record_max, &fh->key[0],
					vmode | ISINOUT | ISEXCLLOCK);
#if 0 /* activate on later merge of locking enhancements */
			f->flag_file_lock = 1;
#endif
		}
	} else {
		if (lmode == ISAUTOLOCK) {
			fh->lmode = ISLOCK;
			lmode = ISMANULOCK;
		}
		isfd = isopen ((void *)filename, omode | lmode | vmode);
		if (isfd < 0) {
			if (ISERRNO == EFLOCKED)
				return COB_STATUS_61_FILE_SHARING;
			if (f->flag_optional) {
				if (mode == COB_OPEN_EXTEND
				 || mode == COB_OPEN_I_O) {
					dobld = 1;
					ret = COB_STATUS_05_SUCCESS_OPTIONAL;
					goto dobuild;
				}
				freefh (fh);
				f->open_mode = mode;
				f->flag_end_of_file = 1;
				f->flag_begin_of_file = 1;
				if (f->flag_nonexistent) {
					return COB_STATUS_00_SUCCESS;
				}
				f->flag_nonexistent = 1;
				return COB_STATUS_05_SUCCESS_OPTIONAL;
			}
		} else {
			memset(&di, 0, sizeof (di));
			isindexinfo (isfd, (void *)&di, 0);
			/* Mask off ISVARLEN */
			fh->nkeys = di.di_nkeys & 0x7F;
			if (fh->nkeys != f->nkeys) {
				ret = COB_STATUS_39_CONFLICT_ATTRIBUTE;
			}
			if (f->record_max != di.di_recsize) {
				ret = COB_STATUS_39_CONFLICT_ATTRIBUTE;
			}
			for (k = 0; k < fh->nkeys && !ret; ++k) {
				memset (&fh->key[k], 0, sizeof (struct keydesc));
				isindexinfo (isfd, &fh->key[k], (int)(k+1));
				if (fh->lenkey < indexed_keylen(fh, k)) {
					fh->lenkey = indexed_keylen(fh, k);
				}
				/* Verify that COBOL keys match exactly to real ISAM keys */
				len = indexed_keydesc(f, &kd, &f->keys[k]);
				if (fh->lenkey < len) {
					fh->lenkey = len;
				}
				if(indexed_keycmp(&kd, &fh->key[k]) != 0) {
					ret = COB_STATUS_39_CONFLICT_ATTRIBUTE;
					break;
				}
			}
		}
	}
	if (isfd < 0) {
		ret = fisretsts (COB_STATUS_30_PERMANENT_ERROR);
		freefh (fh);
		return ret;
	}
	if (ret > 9) {
		isfullclose (isfd);
		freefh (fh);
		return ret;
	}
	if (dobld) {
		for (k = 1; k < f->nkeys; ++k) {
			ISERRNO = 0;
			if (isaddindex (isfd, &fh->key[k])) {
				ret = COB_STATUS_39_CONFLICT_ATTRIBUTE;
			}
		}
		if (ret > 9) {
			isfullclose (isfd);
			iserase ((void *)filename);
			freefh (fh);
			return ret;
		}
	}
	f->file = fh;
	f->open_mode = mode;
	fh->isfd = isfd;
	fh->filename = cob_strdup (filename);
	fh->savekey = cob_malloc ((size_t)(fh->lenkey + 1));
	fh->recwrk = cob_malloc ((size_t)(f->record_max + 1));
	/* Active index is unknown at this time */
	f->curkey = -1;
	f->flag_nonexistent = 0;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
	return ret;

#elif	defined(WITH_DB)

	struct indexed_file	*p;
	size_t			i;
	size_t			j;
	size_t			maxsize;
	db_lockmode_t		lock_mode;
	int			handle_created;
	cob_u32_t		flags = 0;
	int			ret = 0;
	int			nonexistent;
#if	0	/* RXWRXW - Access check BDB Human */
	int			checkvalue;
#endif

	COB_UNUSED (sharing);
	if (cobsetptr->bdb_home != NULL
	 && bdb_env == NULL) {		/* Join BDB, on first OPEN of INDEXED file */
		if (join_environment ()) {
#if 0	/* better return a permanent or sharing error */
			cob_hard_failure ();
#else
			return COB_STATUS_61_FILE_SHARING;
#endif
		}
	}
	cob_chk_file_mapping ();

#if	0	/* RXWRXW - Access check BDB Human */
	if (mode == COB_OPEN_INPUT) {
		checkvalue = R_OK;
	} else {
		checkvalue = R_OK | W_OK;
	}
#endif

	nonexistent = 0;
	if (bdb_nofile (filename)) {
		nonexistent = 1;
		if (mode != COB_OPEN_OUTPUT && f->flag_optional == 0) {
			return COB_STATUS_35_NOT_EXISTS;
		}
	}

	/* broken definition, may happen with EXTFH */
	if (f->nkeys == 0) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}

	p = cob_malloc (sizeof (struct indexed_file));
	f->curkey = -1;
	if (bdb_env != NULL) {
		if (mode == COB_OPEN_OUTPUT || mode == COB_OPEN_EXTEND ||
		    (f->lock_mode & COB_FILE_EXCLUSIVE) ||
		    (mode == COB_OPEN_I_O && !f->lock_mode)) {
			lock_mode = DB_LOCK_WRITE;
		} else {
			lock_mode = DB_LOCK_READ;
		}
		p->key.size = (cob_dbtsize_t) strlen (filename);
		p->key.data = filename;
		ret = bdb_env->lock_get (bdb_env, bdb_lock_id, DB_LOCK_NOWAIT,
					&p->key, lock_mode, &p->bdb_file_lock);
		if (ret) {
			cob_free (p);
			if (ret == DB_LOCK_NOTGRANTED) {
				return COB_STATUS_61_FILE_SHARING;
			} else {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
	}

	switch (mode) {
	case COB_OPEN_INPUT:
		flags |= DB_RDONLY;
		break;
	case COB_OPEN_OUTPUT:
		flags |= DB_CREATE;
		break;
	case COB_OPEN_I_O:
	case COB_OPEN_EXTEND:
		flags |= DB_CREATE;
		break;
	/* LCOV_EXCL_START */
	default:
		cob_runtime_error (_("invalid internal call of %s"), "indexed_open");
		cob_fatal_error (COB_FERROR_CODEGEN);
	/* LCOV_EXCL_STOP */
	}

	p->db = cob_malloc (sizeof (DB *) * f->nkeys);
	p->cursor = cob_malloc (sizeof (DBC *) * f->nkeys);
	p->filenamelen = (int) strlen (filename);
	p->last_readkey = cob_malloc (sizeof (unsigned char *) * 2 * f->nkeys);
	p->last_dupno = cob_malloc (sizeof (unsigned int) * f->nkeys);
	p->rewrite_sec_key = cob_malloc (sizeof (int) * f->nkeys);
	maxsize = p->primekeylen = bdb_keylen(f, 0);
	for (i = 1; i < f->nkeys; ++i) {
		j = bdb_keylen(f, i);
		if( j > maxsize)
			maxsize = j;
	}
	p->maxkeylen = maxsize;

	for (i = 0; i < f->nkeys; ++i) {
		/* File name */
		runtime_buffer[COB_FILE_MAX] = 0;
		if (i == 0) {
			snprintf (runtime_buffer, (size_t)COB_FILE_MAX, "%s",
				  filename);
		} else {
			snprintf (runtime_buffer, (size_t)COB_FILE_MAX, "%s.%d",
				 filename, (int)i);
		}
#if	0	/* RXWRXW - Access check BDB Human */
		ret = access (runtime_buffer, checkvalue);
		if (ret != 0) {
			if (errno == ENOENT &&
			    (mode == COB_OPEN_OUTPUT || f->flag_optional == 1)) {
				ret = 0;
				/* Check here if the directory exists ? */
#if	0	/* RXWRXW - Check dir */
				if (!directory exists) {
					ret = ENOENT;
				} else {
					ret = 0;
				}
#endif
			} else {
				ret = errno;
			}
			if (ret != 0) {
				switch (ret) {
				case ENOENT:
					ret = COB_STATUS_35_NOT_EXISTS;
					break;
				case EACCES:
					ret = COB_STATUS_37_PERMISSION_DENIED;
					break;
				default:
					ret = COB_STATUS_30_PERMANENT_ERROR;
					break;
				}
				/* FIXME: BDB cleanup is missing here */
				return ret;
			}
		}
#endif

		/* btree info */
		ret = db_create (&p->db[i], bdb_env, 0);
		if (!ret) {
			handle_created = 1;
			if (mode == COB_OPEN_OUTPUT) {
				if (bdb_env) {
					bdb_env->dbremove (bdb_env, NULL, runtime_buffer, NULL, 0);
				} else {
					/* FIXME: test "First READ on empty SEQUENTIAL INDEXED file ..."
					   on OPEN-OUTPUT results with MinGW & BDB 6 in
					   BDB1565 DB->pget: method not permitted before handle's open method
					*/
					p->db[i]->remove (p->db[i], runtime_buffer, NULL, 0);
					ret = db_create (&p->db[i], bdb_env, 0);
				}
			}
			if (!ret) {
				if (f->keys[i].tf_duplicates) {
					p->db[i]->set_flags (p->db[i], DB_DUP);
				}
			}
		} else {
			handle_created = 0;
		}
		/* Open db */
		if (!ret) {
			/* FIXME: test "First READ on empty SEQUENTIAL INDEXED file ..."
			   on OPEN-OUTPUT results with MinGW & BDB 6 in
			   BDB0588 At least one secondary cursor must be specified to DB->join
			*/
			ret = p->db[i]->open (p->db[i], NULL, runtime_buffer, NULL,
						DB_BTREE, flags, COB_FILE_MODE);
		}
		if (ret) {
			for (j = 0; j < i; ++j) {
				DB_CLOSE (p->db[j]);
			}
			if (handle_created) {
				DB_CLOSE (p->db[i]);
			}
			cob_free (p->db);
			cob_free (p->last_readkey);
			cob_free (p->last_dupno);
			cob_free (p->rewrite_sec_key);
			cob_free (p->cursor);
			if (bdb_env != NULL) {
				bdb_env->lock_put (bdb_env, &p->bdb_file_lock);
			}
			cob_free (p);
			switch (ret) {
			case DB_LOCK_NOTGRANTED:
				return COB_STATUS_61_FILE_SHARING;
			case ENOENT:
				if (mode == COB_OPEN_EXTEND ||
				    mode == COB_OPEN_OUTPUT) {
					return COB_STATUS_30_PERMANENT_ERROR;
				}
				if (f->flag_optional) {
					if (mode == COB_OPEN_I_O) {
						return COB_STATUS_30_PERMANENT_ERROR;
					}
					f->open_mode = mode;
					f->flag_nonexistent = 1;
					f->flag_end_of_file = 1;
					f->flag_begin_of_file = 1;
					/* RXWRXW - Check directory exists? */
					return COB_STATUS_05_SUCCESS_OPTIONAL;
				}
				return COB_STATUS_35_NOT_EXISTS;
			default:
				return COB_STATUS_30_PERMANENT_ERROR;
			}

		}

		p->last_readkey[i] = cob_malloc (maxsize);
		p->last_readkey[f->nkeys + i] = cob_malloc (maxsize);
	}

	p->temp_key = cob_malloc (maxsize + sizeof (unsigned long));
	p->savekey  = cob_malloc (maxsize + sizeof (unsigned long));
	p->suppkey  = cob_malloc (maxsize + sizeof (unsigned long));
	p->saverec  = cob_malloc (f->record_max + sizeof (unsigned long));
	f->file = p;
	p->key_index = 0;
	p->last_key = NULL;

	memset ((void *)&p->key, 0, sizeof (DBT));
	memset ((void *)&p->data, 0, sizeof (DBT));
	p->filename = cob_malloc (strlen (filename) + 1);
	strcpy (p->filename, filename);
	p->write_cursor_open = 0;
	p->record_locked = 0;
	if (bdb_env != NULL) {
		bdb_env->lock_id (bdb_env, &p->bdb_lock_id);
	}

	bdb_setkey(f, 0);
	p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], 0);
	ret = DB_SEQ (p->cursor[0], DB_FIRST);
	p->cursor[0]->c_close (p->cursor[0]);
	p->cursor[0] = NULL;
	if (!ret) {
		memcpy (p->last_readkey[0], p->key.data, (size_t)p->key.size);
		if (p->data.data != NULL
		 && p->data.size > 0
		 && p->data.size > f->record_max) {
			return COB_STATUS_39_CONFLICT_ATTRIBUTE;
		}
	} else {
		p->data.data = NULL;
	}

	f->open_mode = mode;
	if (f->flag_optional 
	 && nonexistent
	 && mode != COB_OPEN_OUTPUT) {
		return COB_STATUS_05_SUCCESS_OPTIONAL;
	}
	return 0;

#else
	static int first_idx_open = 1;

	COB_UNUSED (f);
	COB_UNUSED (filename);
	COB_UNUSED (sharing);
	COB_UNUSED (mode);

	if (first_idx_open) {
		first_idx_open = 0;
		cob_runtime_warning (_("runtime is not configured to support %s"),
			"ORGANIZATION INDEXED");
	}
	return COB_STATUS_91_NOT_AVAILABLE;
#endif
}

/* Close the INDEXED file */

static int
indexed_close (cob_file *f, const int opt)
{
#ifdef	WITH_INDEX_EXTFH

	return extfh_indexed_close (f, opt);

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh;

	COB_UNUSED (opt);

	fh = f->file;
	if (fh == NULL) {
		return COB_STATUS_00_SUCCESS;
	}
	if (fh->isfd >= 0) {
		isfullclose (fh->isfd);
	}
	freefh (fh);
	f->file = NULL;
	return COB_STATUS_00_SUCCESS;

#elif	defined(WITH_DB)

	struct indexed_file	*p;
	int			i;

	COB_UNUSED (opt);

	p = f->file;
	/* Close DB's */
	for (i = 0; i < (int)f->nkeys; ++i) {
		if (p->cursor[i]) {
			p->cursor[i]->c_close (p->cursor[i]);
		}
	}
	for (i = (int)f->nkeys - 1; i >= 0; --i) {
		if (p->db[i]) {
			DB_CLOSE (p->db[i]);
		}
		cob_free (p->last_readkey[i]);
		cob_free (p->last_readkey[f->nkeys + i]);
	}

	if (p->last_key) {
		cob_free (p->last_key);
	}
	cob_free (p->temp_key);
	cob_free (p->savekey);
	cob_free (p->suppkey);
	cob_free (p->saverec);
	cob_free (p->db);
	cob_free (p->last_readkey);
	cob_free (p->last_dupno);
	cob_free (p->rewrite_sec_key);
	cob_free (p->filename);
	cob_free (p->cursor);
	if (bdb_env != NULL) {
		unlock_record (f);
		bdb_env->lock_put (bdb_env, &p->bdb_file_lock);
		bdb_env->lock_id_free (bdb_env, p->bdb_lock_id);
	}
	cob_free (p);
	f->file = NULL;

	return COB_STATUS_00_SUCCESS;

#else
	COB_UNUSED (f);
	COB_UNUSED (opt);

	return COB_STATUS_91_NOT_AVAILABLE;

#endif
}


/* START INDEXED file with positioning */

static int
indexed_start (cob_file *f, const int cond, cob_field *key)
{
#ifdef	WITH_INDEX_EXTFH

	return extfh_indexed_start (f, cond, key);

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh;
	int			k;
	int			mode;
	int			klen,fullkeylen,partlen;
	int			savecond;

	fh = f->file;
	f->flag_read_done = 0;
	f->flag_first_read = 0;
	fh->readdone = 0;
	fh->eofpending = 0;
	fh->startiscur = 0;
	fh->wrkhasrec = 0;
	if (f->flag_nonexistent) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	k = indexed_findkey(f, key, &fullkeylen, &partlen);
	if(k < 0) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	/* Use size of data field; This may indicate a partial key */
	klen = partlen;
	if (klen < 1 || klen > fullkeylen) {
		/* Max key length for this index */
		klen = fullkeylen;
	}
	mode = ISGTEQ;
	fh->startiscur = 1;
	savecond = cond;
	switch (cond) {
	case COB_EQ:
		mode = ISEQUAL;
		fh->readdir = ISNEXT;
		break;
	case COB_GE:
		mode = ISGTEQ;
		fh->readdir = ISNEXT;
		break;
	case COB_GT:
		mode = ISGREAT;
		fh->readdir = ISNEXT;
		break;
	case COB_LE:
		mode = ISGTEQ;
		fh->readdir = ISPREV;
		break;
	case COB_LT:
		mode = ISGTEQ;
		fh->readdir = ISPREV;
		break;
	case COB_FI:
		mode = ISFIRST;
		fh->readdir = ISNEXT;
		break;
	case COB_LA:
		mode = ISLAST;
		fh->readdir = ISPREV;
		break;
	default:
		return COB_STATUS_21_KEY_INVALID;
	}
	if (isstart (fh->isfd, &fh->key[k], klen, (void *)f->record->data, mode)) {
		if (cond == COB_LE || cond == COB_LT) {
			if (isstart (fh->isfd, &fh->key[k], klen, (void *)f->record->data, ISLAST)) {
				f->curkey = -1;
				f->mapkey = -1;
				fh->startcond = -1;
				fh->readdir = -1;
				fh->startiscur = 0;
				return fisretsts (COB_STATUS_23_KEY_NOT_EXISTS);
			} else {
				savecond = COB_LA;
			}
		} else {
			f->curkey = -1;
			f->mapkey = -1;
			fh->startcond = -1;
			fh->readdir = -1;
			fh->startiscur = 0;
			return fisretsts (COB_STATUS_23_KEY_NOT_EXISTS);
		}
	}
	fh->startcond = savecond;
	indexed_savekey(fh, f->record->data, k);
	f->curkey = k;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
	f->flag_first_read = 1;
	return COB_STATUS_00_SUCCESS;

#elif	defined(WITH_DB)

	return indexed_start_internal (f, cond, key, 0, 0);

#else
	COB_UNUSED (f);
	COB_UNUSED (cond);
	COB_UNUSED (key);

	return COB_STATUS_91_NOT_AVAILABLE;
#endif
}

/* Random READ of the INDEXED file  */

static int
indexed_read (cob_file *f, cob_field *key, const int read_opts)
{
#ifdef	WITH_INDEX_EXTFH

	return extfh_indexed_read (f, key, read_opts);

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh;
	int			k,fullkeylen,partlen;
	int			ret;
	int			lmode;

	fh = f->file;
	fh->eofpending = 0;
	fh->startiscur = 0;
	fh->wrkhasrec = 0;
	if (f->flag_nonexistent) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	k = indexed_findkey(f, key, &fullkeylen, &partlen);
	if(k < 0) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	if (f->curkey != (int)k) {
		/* Switch to this index */
		isstart (fh->isfd, &fh->key[k], 0,
			 (void *)f->record->data, ISEQUAL);
		f->curkey = k;
		fh->wrkhasrec = 0;
	}
	fh->startcond = -1;
	lmode = 0;
	if (read_opts & COB_READ_LOCK) {
		lmode = ISLOCK;
	} else if (read_opts & COB_READ_WAIT_LOCK) {
		lmode = ISLCKW;
	} else if ((f->lock_mode & COB_LOCK_AUTOMATIC)) {
		if (f->open_mode != COB_OPEN_INPUT) {
			if (!(read_opts & COB_READ_IGNORE_LOCK)) {
				lmode = ISLOCK;
			}
		}
	}
#ifdef	ISSKIPLOCK
	if (read_opts & COB_READ_IGNORE_LOCK) {
		lmode = ISSKIPLOCK;
	}
#endif
	if ((fh->lmode & ISLOCK) && !(f->lock_mode & COB_LOCK_MULTIPLE)) {
		isrelease (fh->isfd);
	}
	ISERRNO = 0;
	fh->readdir = -1;
	ret = COB_STATUS_00_SUCCESS;
	if (isread (fh->isfd, (void *)f->record->data, ISEQUAL | lmode)) {
		ret = fisretsts (COB_STATUS_21_KEY_INVALID);
	}
	if (unlikely (ret != 0)) {
		memset (fh->savekey, 0, fh->lenkey);
		fh->recnum = 0;
		fh->readdone = 0;
		return ret;
	}
	f->flag_first_read = 0;
	f->flag_read_done = 1;
	fh->readdone = 1;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
	indexed_savekey(fh, f->record->data, 0);
	fh->recnum = ISRECNUM;
#ifdef	ISVARLEN
	if (f->record_min != f->record_max) {
		f->record->size = ISRECLEN;
	}
#endif
	return 0;

#elif	defined(WITH_DB)

	struct indexed_file	*p;
	int			ret;
	int			bdb_opts;
	int			test_lock;

	p = f->file;
	test_lock = 0;
	bdb_opts = read_opts;
	if (bdb_env != NULL) {
		if (f->open_mode != COB_OPEN_I_O ||
		    (f->lock_mode & COB_FILE_EXCLUSIVE)) {
			bdb_opts &= ~COB_READ_LOCK;
		} else if ((f->lock_mode & COB_LOCK_AUTOMATIC) &&
			   !(bdb_opts & COB_READ_NO_LOCK)) {
			bdb_opts |= COB_READ_LOCK;
		}
		unlock_record (f);
		test_lock = 1;
	} else {
		bdb_opts &= ~COB_READ_LOCK;
	}

	ret = indexed_start_internal (f, COB_EQ, key, bdb_opts, test_lock);
	if (ret != COB_STATUS_00_SUCCESS) {
		return ret;
	}

	f->record->size = p->data.size;
	if (f->record->size > f->record_max) {
		f->record->size = f->record_max;
		ret = COB_STATUS_43_READ_NOT_DONE;
	} else {
		ret = COB_STATUS_00_SUCCESS;
	}
	memcpy (f->record->data, p->data.data, f->record->size);

	return ret;

#else
	COB_UNUSED (f);
	COB_UNUSED (key);
	COB_UNUSED (read_opts);

	return COB_STATUS_91_NOT_AVAILABLE;
#endif
}

/* Sequential READ of the INDEXED file */

static int
indexed_read_next (cob_file *f, const int read_opts)
{
#ifdef	WITH_INDEX_EXTFH

	return extfh_indexed_read_next (f, read_opts);

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh;
	int			ret;
	int			lmode;
	int			domoveback;

	fh = f->file;
	ret = COB_STATUS_00_SUCCESS;
	lmode = 0;

	if (f->curkey == -1) {
		/* Switch to primary index */
		isstart (fh->isfd, &fh->key[0], 0, NULL, ISFIRST);
		f->curkey = 0;
		fh->readdir = ISNEXT;
		fh->startcond = -1;
		fh->startiscur = 0;
		fh->wrkhasrec = 0;
	}
	if (read_opts & COB_READ_LOCK) {
		lmode = ISLOCK;
	} else if (read_opts & COB_READ_WAIT_LOCK) {
		lmode = ISLCKW;
	} else
	if ((f->lock_mode & COB_LOCK_AUTOMATIC)
	 && f->open_mode != COB_OPEN_INPUT) {
		if (!(read_opts & COB_READ_IGNORE_LOCK)) {
			lmode = ISLOCK;
		}
	}
#ifdef	ISSKIPLOCK
	if (read_opts & COB_READ_IGNORE_LOCK) {
		lmode |= ISSKIPLOCK;
	}
#endif
	if ((fh->lmode & ISLOCK) && !(f->lock_mode & COB_LOCK_MULTIPLE)) {
		isrelease (fh->isfd);
	}
	ISERRNO = 0;
	ret = COB_STATUS_00_SUCCESS;
	switch (read_opts & COB_READ_MASK) {
	case COB_READ_NEXT:
		fh->readdir = ISNEXT;
		if (fh->eofpending == ISNEXT) {
			fh->eofpending = 0;
			fh->wrkhasrec = 0;
			return COB_STATUS_10_END_OF_FILE;
		}
		if (fh->startiscur) {
			if (fh->startcond == COB_LA) {
				if (isread (fh->isfd, (void *)f->record->data, ISLAST | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			} else if (fh->startcond == COB_FI) {
				if (isread (fh->isfd, (void *)f->record->data, ISFIRST | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			} else if (isread (fh->isfd, (void *)f->record->data, ISCURR)) {
				ret = fisretsts (COB_STATUS_10_END_OF_FILE);
			} else {
				switch (fh->startcond) {
				case COB_GE:
					domoveback = 0;
					while (ISERRNO == 0
					&& indexed_cmpkey(fh, f->record->data, f->curkey, 0) == 0) {
						isread (fh->isfd, (void *)f->record->data, ISPREV);
						domoveback = 1;
					}
					if (domoveback) {
						isread (fh->isfd, (void *)f->record->data, ISERRNO == 0 ? ISNEXT : ISFIRST);
					}
					break;
				case COB_LE:
					domoveback = 0;
					while (ISERRNO == 0
					&& indexed_cmpkey(fh, f->record->data, f->curkey, 0) == 0) {
						isread (fh->isfd, (void *)f->record->data, ISNEXT);
						domoveback = 1;
					}
					if (domoveback) {
						isread (fh->isfd, (void *)f->record->data, ISERRNO == 0 ? ISPREV : ISLAST);
					}
					break;
				case COB_LT:
					isread (fh->isfd, (void *)f->record->data, ISPREV);
					while (ISERRNO == 0
					&& indexed_cmpkey(fh, f->record->data, f->curkey, 0) >= 0) {
						isread (fh->isfd, (void *)f->record->data, ISPREV);
					}
					break;
				case COB_GT:
					while (ISERRNO == 0
					&& indexed_cmpkey(fh, f->record->data, f->curkey, 0) <= 0) {
						isread (fh->isfd, (void *)f->record->data, ISNEXT);
					}
					break;
				}
				if (isread (fh->isfd, (void *)f->record->data, ISCURR | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			}
			fh->startcond = -1;
			fh->startiscur = 0;
		} else if (fh->wrkhasrec == ISNEXT) {
			memcpy (f->record->data, fh->recwrk, f->record_max);
			if (fh->lmode & ISLOCK) {
				/* Now lock 'peek ahead' record */
				if (isread (fh->isfd, (void *)f->record->data, ISCURR | fh->lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			}
		} else {
			if (fh->wrkhasrec == ISPREV) {
				isread (fh->isfd, (void *)f->record->data, ISNEXT);
				fh->wrkhasrec = 0;
			}
			if (isread (fh->isfd, (void *)f->record->data, ISNEXT | lmode)) {
				ret = fisretsts (COB_STATUS_10_END_OF_FILE);
			}
		}
		break;
	case COB_READ_PREVIOUS:
		fh->readdir = ISPREV;
		if (fh->eofpending == ISPREV) {
			fh->eofpending = 0;
			fh->wrkhasrec = 0;
			return COB_STATUS_10_END_OF_FILE;
		}
		if (fh->startiscur) {
			if (fh->startcond == COB_FI) {
				if (isread (fh->isfd, (void *)f->record->data, ISFIRST | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			} else if (fh->startcond == COB_LA) {
				if (isread (fh->isfd, (void *)f->record->data, ISLAST | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			} else if (isread (fh->isfd, (void *)f->record->data, ISCURR | lmode)) {
				ret = fisretsts (COB_STATUS_10_END_OF_FILE);
			} else {
				switch (fh->startcond) {
				case COB_LE:
					if(indexed_cmpkey(fh, f->record->data, f->curkey, 0) > 0)
						domoveback = 1;
					else
						domoveback = 0;
					while (ISERRNO == 0
					&& indexed_cmpkey(fh, f->record->data, f->curkey, 0) == 0) {
						isread (fh->isfd, (void *)f->record->data, ISNEXT);
						domoveback = 1;
					}
					if (domoveback) {
						isread (fh->isfd, (void *)f->record->data, ISPREV);
					}
					break;
				case COB_LT:
					while (ISERRNO == 0
					&& indexed_cmpkey(fh, f->record->data, f->curkey, 0) >= 0) {
						isread (fh->isfd, (void *)f->record->data, ISPREV);
					}
					break;
				case COB_GT:
					while (ISERRNO == 0
					&& indexed_cmpkey(fh, f->record->data, f->curkey, 0) <= 0) {
						isread (fh->isfd, (void *)f->record->data, ISNEXT);
					}
					break;
				case COB_GE:
					while (ISERRNO == 0
					&& indexed_cmpkey(fh, f->record->data, f->curkey, 0) < 0) {
						isread (fh->isfd, (void *)f->record->data, ISNEXT);
					}
					break;
				}
				if (isread (fh->isfd, (void *)f->record->data, ISCURR | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			}
			fh->startcond = -1;
			fh->startiscur = 0;
		} else if (fh->wrkhasrec == ISPREV) {
			memcpy (f->record->data, fh->recwrk, f->record_max);
			if (fh->lmode & ISLOCK) {
				/* Now lock 'peek ahead' record */
				if (isread (fh->isfd, (void *)f->record->data,
				    ISCURR | fh->lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			}
		} else {
			if (fh->wrkhasrec == ISNEXT) {
				isread (fh->isfd, (void *)f->record->data, ISPREV);
				fh->wrkhasrec = 0;
			}
			if (isread (fh->isfd, (void *)f->record->data, ISPREV | lmode)) {
				ret = fisretsts (COB_STATUS_10_END_OF_FILE);
			}
		}
		break;
	case COB_READ_FIRST:
		fh->readdir = ISNEXT;
		if (isread (fh->isfd, (void *)f->record->data, ISFIRST | lmode)) {
			ret = fisretsts (COB_STATUS_10_END_OF_FILE);
		}
		break;
	case COB_READ_LAST:
		fh->readdir = ISPREV;
		if (isread (fh->isfd, (void *)f->record->data, ISLAST | lmode)) {
			ret = fisretsts (COB_STATUS_10_END_OF_FILE);
		}
		break;
	default:
		fh->readdir = ISNEXT;
		if (isread (fh->isfd, (void *)f->record->data, ISNEXT | lmode)) {
			ret = fisretsts (COB_STATUS_10_END_OF_FILE);
		}
		break;
	}
	if (unlikely (ret != 0)) {
		memset (fh->savekey, 0, fh->lenkey);
		fh->recnum = 0;
		fh->readdone = 0;
		fh->wrkhasrec = 0;
		return ret;
	}
	fh->eofpending = 0;
	f->flag_first_read = 0;
	f->flag_read_done = 1;
	fh->readdone = 1;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
	indexed_savekey(fh, f->record->data, 0);
	fh->recnum = ISRECNUM;
#ifdef	ISVARLEN
	if (f->record_min != f->record_max) {
		f->record->size = ISRECLEN;
	}
#endif

#ifdef	COB_WITH_STATUS_02
	if((isstat1 == '0') && (isstat2 == '2')) {
		return COB_STATUS_02_SUCCESS_DUPLICATE;
	}
#endif
	return 0;

#elif	defined(WITH_DB)

	struct indexed_file	*p;
	int			ret;
	int			read_nextprev;
	cob_u32_t		nextprev;
	int			file_changed;
	int			bdb_opts;
	unsigned int		dupno;

	p = f->file;
	nextprev = DB_NEXT;
	dupno = 0;
	file_changed = 0;

	bdb_opts = read_opts;
	if (bdb_env != NULL) {
		if (f->open_mode != COB_OPEN_I_O
		 || (f->lock_mode & COB_FILE_EXCLUSIVE)) {
			bdb_opts &= ~COB_READ_LOCK;
		} else if ((f->lock_mode & COB_LOCK_AUTOMATIC) 
			&& !(bdb_opts & COB_READ_NO_LOCK)) {
			bdb_opts |= COB_READ_LOCK;
		}
		unlock_record (f);
	} else {
		bdb_opts &= ~COB_READ_LOCK;
	}

	if (unlikely (bdb_opts & COB_READ_PREVIOUS)) {
		if (f->flag_end_of_file) {
			nextprev = DB_LAST;
		} else {
			nextprev = DB_PREV;
		}
	} else if (f->flag_begin_of_file) {
		nextprev = DB_FIRST;
	}
	/* The open cursor makes this function atomic */
	if (p->key_index != 0) {
		p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], 0);
	}
	p->db[p->key_index]->cursor (p->db[p->key_index], NULL, &p->cursor[p->key_index], 0);

	if (f->flag_first_read) {
		/* Data is read in indexed_open or indexed_start */
		if (p->data.data == NULL
		 || (f->flag_first_read == 2 && nextprev == DB_PREV)) {
			p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
			p->cursor[p->key_index] = NULL;
			if (p->key_index != 0) {
				p->cursor[0]->c_close (p->cursor[0]);
				p->cursor[0] = NULL;
			}
			return COB_STATUS_10_END_OF_FILE;
		}
		/* Check if previously read data still exists */
		p->key.size = (cob_dbtsize_t) bdb_keylen(f,p->key_index);
		p->key.data = p->last_readkey[p->key_index];
		ret = DB_SEQ (p->cursor[p->key_index], DB_SET);
		if (!ret && p->key_index > 0) {
			if (f->keys[p->key_index].tf_duplicates) {
				memcpy (&dupno, (cob_u8_ptr)p->data.data + p->primekeylen, sizeof (unsigned int));
				dupno = COB_DUPSWAP (dupno);
				while (ret == 0
				   && memcmp (p->key.data, p->last_readkey[p->key_index], (size_t)p->key.size) == 0
				   && dupno < p->last_dupno[p->key_index]) {
					ret = DB_SEQ (p->cursor[p->key_index], DB_NEXT);
					memcpy (&dupno, (cob_u8_ptr)p->data.data + p->primekeylen, sizeof (unsigned int));
					dupno = COB_DUPSWAP (dupno);
				}
				if (ret == 0
				 && memcmp (p->key.data, p->last_readkey[p->key_index], (size_t)p->key.size) == 0
				 && dupno == p->last_dupno[p->key_index]) {
					ret = memcmp (p->last_readkey[p->key_index + f->nkeys], p->data.data, p->primekeylen);
				} else {
					ret = 1;
				}
			} else {
				ret = memcmp (p->last_readkey[p->key_index + f->nkeys], p->data.data, p->primekeylen);
			}
			if (!ret) {
				p->key.size = (cob_dbtsize_t) p->primekeylen;
				p->key.data = p->last_readkey[p->key_index + f->nkeys];
				ret = DB_GET (p->db[0], 0);
			}
		}
		file_changed = ret;
		if (bdb_env != NULL && !file_changed) {
			if (!(bdb_opts & COB_READ_IGNORE_LOCK)) {
				ret = test_record_lock (f, p->key.data, p->key.size);
				if (ret) {
					p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
					p->cursor[p->key_index] = NULL;
					if (p->key_index != 0) {
						p->cursor[0]->c_close (p->cursor[0]);
						p->cursor[0] = NULL;
					}
					return COB_STATUS_51_RECORD_LOCKED;
				}
			}
			if (bdb_opts & COB_READ_LOCK) {
				ret = lock_record (f, p->key.data, p->key.size);
				if (ret) {
					p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
					p->cursor[p->key_index] = NULL;
					if (p->key_index != 0) {
						p->cursor[0]->c_close (p->cursor[0]);
						p->cursor[0] = NULL;
					}
					return COB_STATUS_51_RECORD_LOCKED;
				}
			}
		}
	}
	if (!f->flag_first_read || file_changed) {
		if (nextprev == DB_FIRST || nextprev == DB_LAST) {
			read_nextprev = 1;
		} else {
			p->key.size = (cob_dbtsize_t) bdb_keylen(f,p->key_index);
			p->key.data = p->last_readkey[p->key_index];
			ret = DB_SEQ (p->cursor[p->key_index], DB_SET_RANGE);
			/* ret != 0 possible, records may be deleted since last read */
			if (ret != 0) {
				if (nextprev == DB_PREV) {
					nextprev = DB_LAST;
					read_nextprev = 1;
				} else {
					p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
					p->cursor[p->key_index] = NULL;
					if (p->key_index != 0) {
						p->cursor[0]->c_close (p->cursor[0]);
						p->cursor[0] = NULL;
					}
					return COB_STATUS_10_END_OF_FILE;
				}
			} else {
				if (memcmp (p->key.data, p->last_readkey[p->key_index], (size_t)p->key.size) == 0) {
					if (p->key_index > 0 && f->keys[p->key_index].tf_duplicates) {
						memcpy (&dupno, (cob_u8_ptr)p->data.data + p->primekeylen, sizeof (unsigned int));
						dupno = COB_DUPSWAP (dupno);
						while (ret == 0
						 && memcmp (p->key.data, p->last_readkey[p->key_index], (size_t)p->key.size) == 0
						 && dupno < p->last_dupno[p->key_index]) {
							ret = DB_SEQ (p->cursor[p->key_index], DB_NEXT);
							memcpy (&dupno, (cob_u8_ptr)p->data.data + p->primekeylen, sizeof (unsigned int));
							dupno = COB_DUPSWAP (dupno);
						}
						if (ret != 0) {
							if (nextprev == DB_PREV) {
								nextprev = DB_LAST;
								read_nextprev = 1;
							} else {
								p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
								p->cursor[p->key_index] = NULL;
								if (p->key_index != 0) {
									p->cursor[0]->c_close (p->cursor[0]);
									p->cursor[0] = NULL;
								}
								return COB_STATUS_10_END_OF_FILE;
							}
						} else {
							if (memcmp (p->key.data, p->last_readkey[p->key_index], (size_t)p->key.size) == 0 &&
								dupno == p->last_dupno[p->key_index]) {
								read_nextprev = 1;
							} else {
								if (nextprev == DB_PREV) {
									read_nextprev = 1;
								} else {
									read_nextprev = 0;
								}
							}
						}
					} else {
						read_nextprev = 1;
					}
				} else {
					if (nextprev == DB_PREV) {
						read_nextprev = 1;
					} else {
						read_nextprev = 0;
					}
				}
			}
		}
		if (read_nextprev) {
			ret = DB_SEQ (p->cursor[p->key_index], nextprev);
			if (ret != 0) {
				p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
				p->cursor[p->key_index] = NULL;
				if (p->key_index != 0) {
					p->cursor[0]->c_close (p->cursor[0]);
					p->cursor[0] = NULL;
				}
				return COB_STATUS_10_END_OF_FILE;
			}
		}

		if (p->key_index > 0) {
			/* Temporarily save alternate key */
			memcpy (p->temp_key, p->key.data, (size_t)p->key.size);
			if (f->keys[p->key_index].tf_duplicates) {
				memcpy (&dupno, (cob_u8_ptr)p->data.data + p->primekeylen, sizeof (unsigned int));
				dupno = COB_DUPSWAP (dupno);
			}
			p->key.data = p->data.data;
			p->key.size = p->primekeylen;
			ret =  DB_GET (p->db[0], 0);
			if (ret != 0) {
				p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
				p->cursor[p->key_index] = NULL;
				p->cursor[0]->c_close (p->cursor[0]);
				p->cursor[0] = NULL;
				return COB_STATUS_23_KEY_NOT_EXISTS;
			}
		}
		if (bdb_env != NULL) {
			if (!(bdb_opts & COB_READ_IGNORE_LOCK)) {
				ret = test_record_lock (f, p->key.data, p->key.size);
				if (ret) {
					p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
					p->cursor[p->key_index] = NULL;
					if (p->key_index != 0) {
						p->cursor[0]->c_close (p->cursor[0]);
						p->cursor[0] = NULL;
					}
					return COB_STATUS_51_RECORD_LOCKED;
				}
			}
			if (bdb_opts & COB_READ_LOCK) {
				ret = lock_record (f, p->key.data, p->key.size);
				if (ret) {
					p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
					p->cursor[p->key_index] = NULL;
					if (p->key_index != 0) {
						p->cursor[0]->c_close (p->cursor[0]);
						p->cursor[0] = NULL;
					}
					return COB_STATUS_51_RECORD_LOCKED;
				}
			}
		}
		if (p->key_index == 0) {
			memcpy (p->last_readkey[0], p->key.data, (size_t)p->key.size);
		} else {
			memcpy (p->last_readkey[p->key_index], p->temp_key,
				    bdb_keylen(f,p->key_index));
			memcpy (p->last_readkey[p->key_index + f->nkeys], p->key.data, p->primekeylen);
			if (f->keys[p->key_index].tf_duplicates) {
				p->last_dupno[p->key_index] = dupno;
			}
		}
	}

	p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
	p->cursor[p->key_index] = NULL;
	if (p->key_index != 0) {
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
	}

	f->record->size = p->data.size;
	if (f->record->size > f->record_max) {
		f->record->size = f->record_max;
		ret = COB_STATUS_43_READ_NOT_DONE;
	} else {
		ret = COB_STATUS_00_SUCCESS;
	}
	memcpy (f->record->data, p->data.data, f->record->size);

	return ret;

#else
	COB_UNUSED (f);
	COB_UNUSED (read_opts);

	return COB_STATUS_91_NOT_AVAILABLE;
#endif
}


/* WRITE to the INDEXED file  */

static int
indexed_write (cob_file *f, const int opt)
{
#ifdef	WITH_INDEX_EXTFH

	return extfh_indexed_write (f, opt);

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh;

	COB_UNUSED (opt);

	fh = f->file;
	if (f->flag_nonexistent) {
		return COB_STATUS_48_OUTPUT_DENIED;
	}
	if (f->access_mode == COB_ACCESS_SEQUENTIAL
	&&  indexed_cmpkey(fh, f->record->data, 0, 0) <= 0) {
		return COB_STATUS_21_KEY_INVALID;
	}

#ifdef	ISVARLEN
	if (f->record_min != f->record_max) {
		ISRECLEN = f->record->size;
	}
#endif
	if (unlikely (iswrite (fh->isfd, (void *)f->record->data))) {
		if (f->access_mode == COB_ACCESS_SEQUENTIAL
		 && f->open_mode == COB_OPEN_OUTPUT
#if 0	/* CHECKME */
		 && f->flag_set_isam
#endif
		 && ISERRNO == EDUPL) {
			return COB_STATUS_21_KEY_INVALID;
		}
		return fisretsts (COB_STATUS_49_I_O_DENIED);
	}
	indexed_savekey(fh, f->record->data, 0);

#ifdef	COB_WITH_STATUS_02
	if((isstat1 == '0') && (isstat2 == '2')) {
		return COB_STATUS_02_SUCCESS_DUPLICATE;
	}
#endif
	return 0;

#elif	defined(WITH_DB)

	struct indexed_file	*p;
	int			ret;

	if (f->flag_nonexistent) {
		return COB_STATUS_48_OUTPUT_DENIED;
	}
	p = f->file;
	if (bdb_env != NULL) {
		unlock_record (f);
	}

	/* Check record key */
	bdb_setkey (f, 0);
	if (!p->last_key) {
		p->last_key = cob_malloc ((size_t)p->maxkeylen);
	} else if (f->access_mode == COB_ACCESS_SEQUENTIAL &&
		   memcmp (p->last_key, p->key.data, (size_t)p->key.size) > 0) {
		return COB_STATUS_21_KEY_INVALID;
	}
	memcpy (p->last_key, p->key.data, (size_t)p->key.size);
	
	ret = indexed_write_internal (f, 0, opt);

	if (f->access_mode == COB_ACCESS_SEQUENTIAL
	 && f->open_mode == COB_OPEN_OUTPUT
#if 0	/* CHECKME */
	 && f->flag_set_isam
#endif
	 && ret == COB_STATUS_22_KEY_EXISTS) {
		return COB_STATUS_21_KEY_INVALID;
	}
	return ret;

#else
	COB_UNUSED (f);
	COB_UNUSED (opt);

	return COB_STATUS_91_NOT_AVAILABLE;
#endif
}


/* DELETE record from the INDEXED file  */

static int
indexed_delete (cob_file *f)
{
#ifdef	WITH_INDEX_EXTFH

	return extfh_indexed_delete (f);

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh;
	int			ret;

	fh = f->file;
	ret = COB_STATUS_00_SUCCESS;
	if (f->flag_nonexistent) {
		return COB_STATUS_49_I_O_DENIED;
	}
	if (f->curkey == -1) {
		/* Switch to primary index */
		isstart (fh->isfd, &fh->key[0], 0,
			 (void *)f->record->data, ISEQUAL);
		f->curkey = 0;
		fh->readdir = ISNEXT;
	} else {
		savefileposition (f);
		if (f->curkey != 0) {
			/* Switch to primary index */
			isstart (fh->isfd, &fh->key[0], 0,
				 (void *)f->record->data, ISEQUAL);
		}
	}
	if (isread (fh->isfd, (void *)f->record->data, ISEQUAL | ISLOCK)) {
		ret = fisretsts (COB_STATUS_21_KEY_INVALID);
	} else if (isdelete (fh->isfd, (void *)f->record->data)) {
		ret = fisretsts (COB_STATUS_49_I_O_DENIED);
	}
	restorefileposition (f);
	return ret;

#elif	defined(WITH_DB)

	if (f->flag_nonexistent) {
		return COB_STATUS_49_I_O_DENIED;
	}
	return indexed_delete_internal (f, 0);

#else
	COB_UNUSED (f);

	return COB_STATUS_91_NOT_AVAILABLE;
#endif
}

/* REWRITE record to the INDEXED file  */

static int
indexed_rewrite (cob_file *f, const int opt)
{
#ifdef	WITH_INDEX_EXTFH

	return extfh_indexed_rewrite (f, opt);

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh;
	size_t			k;
	int			ret, retdup;

	COB_UNUSED (opt);

	fh = f->file;
	ret = retdup = COB_STATUS_00_SUCCESS;
	if (f->flag_nonexistent) {
		return COB_STATUS_49_I_O_DENIED;
	}

	if (f->access_mode == COB_ACCESS_SEQUENTIAL
	&&  indexed_cmpkey(fh, f->record->data, 0, 0) != 0) {
		return COB_STATUS_21_KEY_INVALID;
	}
	if (f->curkey >= 0) {
		/* Index is active */
		/* Save record data */
		memcpy (fh->recwrk, f->record->data, f->record_max);
/* RXWRXW - readdir */
		fh->readdir = ISNEXT;
		savefileposition (f);
		memcpy (fh->recwrk, f->record->data, f->record_max);
		if (f->curkey != 0) {
			/* Activate primary index */
			isstart (fh->isfd, &fh->key[0], 0, (void *)fh->recwrk,
				 ISEQUAL);
		}
		/* Verify record exists */
		if (isread (fh->isfd, (void *)fh->recwrk, ISEQUAL)) {
			restorefileposition (f);
			return COB_STATUS_21_KEY_INVALID;
		}
		for (k = 1; k < f->nkeys && ret == COB_STATUS_00_SUCCESS; ++k) {
			if (fh->key[k].k_flags & ISDUPS) {
				continue;
			}
			memcpy (fh->recwrk, f->record->data, f->record_max);
			isstart (fh->isfd, &fh->key[k], fh->key[k].k_leng,
				 (void *)fh->recwrk, ISEQUAL);
			if (!isread (fh->isfd, (void *)fh->recwrk, ISEQUAL)
			 && ISRECNUM != fh->recnum) {
				ret = COB_STATUS_22_KEY_EXISTS;
				break;
			}
		}
		if (ret == COB_STATUS_00_SUCCESS) {
			memcpy (fh->recwrk, f->record->data, f->record_max);
			isstart (fh->isfd, &fh->key[0], 0, (void *)fh->recwrk,
				 ISEQUAL);
			if (isread (fh->isfd, (void *)fh->recwrk, ISEQUAL | ISLOCK)) {
				ret = fisretsts (COB_STATUS_49_I_O_DENIED);
			} else {
#ifdef	ISVARLEN
				if (f->record_min != f->record_max) {
					ISRECLEN = f->record->size;
				}
#endif
				if (isrewcurr (fh->isfd, (void *)f->record->data)) {
					ret = fisretsts (COB_STATUS_49_I_O_DENIED);
				}
			}
		}

#ifdef	COB_WITH_STATUS_02
		if (!ret && (isstat1 == '0') && (isstat2 == '2')) {
			retdup = COB_STATUS_02_SUCCESS_DUPLICATE;
		}
#endif
		restorefileposition (f);

	} else {

		memcpy (fh->recwrk, f->record->data, f->record_max);
		if (isread (fh->isfd, (void *)fh->recwrk, ISEQUAL | ISLOCK)) {
			ret = fisretsts (COB_STATUS_49_I_O_DENIED);
		} else {
#ifdef	ISVARLEN
			if (f->record_min != f->record_max) {
				ISRECLEN = f->record->size;
			}
#endif
			if (isrewrite (fh->isfd, (void *)f->record->data)) {
				ret = fisretsts (COB_STATUS_49_I_O_DENIED);
			}
#ifdef	COB_WITH_STATUS_02
			if (!ret && (isstat1 == '0') && (isstat2 == '2')) {
				retdup = COB_STATUS_02_SUCCESS_DUPLICATE;
			}
#endif
		}
		if (!ret) {
			if ((f->lock_mode & COB_LOCK_AUTOMATIC)
			 && !(f->lock_mode & COB_LOCK_MULTIPLE)) {
				isrelease (fh->isfd);
			}
#ifdef	COB_WITH_STATUS_02
			if ((isstat1 == '0') && (isstat2 == '2')) {
				retdup = COB_STATUS_02_SUCCESS_DUPLICATE;
			}
#endif
		}
	}
	if (retdup) {
		/* FIXME: use (is_suppressed_key_value) or similar to verify
		   that the duplicate this is not a SUPPRESSed KEY */
		return retdup;
	}
	return ret;

#elif	defined(WITH_DB)

	struct indexed_file	*p;
	int			ret;
	cob_u32_t		flags;

	if (f->flag_nonexistent) {
		return COB_STATUS_49_I_O_DENIED;
	}
	p = f->file;
	if (bdb_env) {
		flags = DB_WRITECURSOR;
	} else {
		flags = 0;
	}
	p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], flags);
	p->write_cursor_open = 1;
	if (bdb_env != NULL) {
		unlock_record (f);
	}

	/* Check duplicate alternate keys */
	if (check_alt_keys (f, 1)) {
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
		p->write_cursor_open = 0;
		return COB_STATUS_22_KEY_EXISTS;
	}

	/* Delete the current record */
	ret = indexed_delete_internal (f, 1);

	if (ret != COB_STATUS_00_SUCCESS) {
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
		p->write_cursor_open = 0;
		return ret;
	}

	/* Write data */
	bdb_setkey(f, 0);
	ret = indexed_write_internal (f, 1, opt);

	p->cursor[0]->c_close (p->cursor[0]);
	p->cursor[0] = NULL;
	p->write_cursor_open = 0;
	return ret;

#else
	COB_UNUSED (f);
	COB_UNUSED (opt);

	return COB_STATUS_91_NOT_AVAILABLE;
#endif
}


static void
cob_file_unlock (cob_file *f)
{
	if (COB_FILE_SPECIAL(f)) {
		return;
	}

	if (f->open_mode != COB_OPEN_CLOSED &&
	    f->open_mode != COB_OPEN_LOCKED) {
		if (f->organization == COB_ORG_SORT) {
			return;
		}
		if (f->organization != COB_ORG_INDEXED) {
#ifndef	WITH_SEQRA_EXTFH
			if (f->fd >= 0) {
				fdcobsync (f->fd);
			}
#ifdef	HAVE_FCNTL
			if (!(f->lock_mode & COB_FILE_EXCLUSIVE)) {
				/* Unlock the file */
				if (f->fd >= 0) {
					struct flock	lock;
					memset ((void *)&lock, 0, sizeof (struct flock));
					lock.l_type = F_UNLCK;
					lock.l_whence = SEEK_SET;
					lock.l_start = 0;
					lock.l_len = 0;
					if (fcntl (f->fd, F_SETLK, &lock) == -1) {
#if 1 /* CHECKME - What is the correct thing to do here? */
						/* not translated as "testing only" */
						cob_runtime_warning ("issue during unlock (%s), errno: %d",
							"cob_file_unlock", errno);
#endif
					}
				}
			}
#elif defined _WIN32
			{
				HANDLE osHandle = (HANDLE)_get_osfhandle (f->fd);
				if (osHandle != INVALID_HANDLE_VALUE) {
					if (!UnlockFile (osHandle, 0, 0, MAXDWORD, MAXDWORD)) {
#if 1 /* CHECKME - What is the correct thing to do here? */
						/* not translated as "testing only" */
						cob_runtime_warning ("issue during UnLockFile (%s), lastError: " CB_FMT_LLU,
							"cob_file_unlock", (cob_u64_t)GetLastError());
#endif
					}
				}
			}
#endif

#endif
		} else {
#ifdef	WITH_INDEX_EXTFH
			extfh_indexed_unlock (f);
#elif	defined(WITH_DB) || defined(WITH_ANY_ISAM)
			if (f->file) {
#if	defined(WITH_DB)
				if (bdb_env != NULL) {
					struct indexed_file	*p = f->file;
					unlock_record (f);
					bdb_env->lock_put (bdb_env, &p->bdb_file_lock);
				}
#else
				struct indexfile	*fh = f->file;
				isrelease (fh->isfd);
#endif
			}
#endif
		}
	}
}

/* Global functions */

/*
 * Allocate memory for 'IS EXTERNAL' cob_file
 */
void
cob_file_external_addr (const char *exname,
		cob_file **pfl, cob_file_key **pky,
		const int nkeys, const int linage)
{
	cob_file	**epfl = cob_external_addr (exname, sizeof (cob_file *));

	if (cobglobptr->cob_initial_external) {
		/* if the pointer was setup the first time:
		   allocate the file and store the address for next request */
		cob_file_malloc (pfl, pky, nkeys, linage);
		*epfl = *pfl;
	} else {
		/* external pointer available - get the address stored
		   and set / check keys */
		cob_file	*fl = *pfl = *epfl;
		/* already allocated, just pass on */		
		if (pky != NULL) {
			*pky = fl->keys;
		}
#if 0	/* TODO: verify file attributes (here or in the caller?) */
		if (fl->nkeys != nkeys) {
			/* reallocate if KEYCHECK and bigger / raise exception otherwise ? */
		}
		if (linage > 0
		 && fl->linorkeyptr == NULL) {
			/* CHECKME: is this allowed to happen? */
		}
#endif
	}
}

/*
 * Allocate memory for cob_file
 */
void
cob_file_malloc (cob_file **pfl, cob_file_key **pky,
		 const int nkeys, const int linage)
{
	cob_file	*fl;
	fl = cob_cache_malloc (sizeof (cob_file));
	fl->file_version = COB_FILE_VERSION;
	fl->nkeys = (size_t)nkeys;	/* casting away bad difference... */

	if (nkeys > 0
	 && pky != NULL) {
		*pky = fl->keys = cob_cache_malloc (sizeof (cob_file_key) * nkeys);
	}

	if (linage > 0) {
		fl->linorkeyptr = cob_cache_malloc (sizeof (cob_linage));
	}
	*pfl = fl;
}

/*
 * Free memory for cob_file
 */
void
cob_file_free (cob_file **pfl, cob_file_key **pky)
{
	cob_file	*fl;
	if (pky != NULL) {
		if (*pky != NULL) {
			cob_cache_free (*pky);
			*pky = NULL;
		}
	}
	if (pfl != NULL && *pfl != NULL) {
		fl = *pfl;
		if (fl->linorkeyptr) {
			cob_cache_free (fl->linorkeyptr);
			fl->linorkeyptr = NULL;
		}
		if (fl->org_filename) {
			cob_cache_free (fl->org_filename);
			fl->org_filename = NULL;
		}
		if (fl->convert_field) {
			cob_free (fl->convert_field);
			fl->convert_field = NULL;
		}
		cob_cache_free (*pfl);
		*pfl = NULL;
	}
}


void
cob_unlock_file (cob_file *f, cob_field *fnstatus)
{
	cob_file_unlock (f);
	save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
}

/*
 * Prepare for Open of data file; Used by EXTFH routines)
 */
static void
cob_pre_open (cob_file *f)
{
	f->flag_nonexistent = 0;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
	f->flag_first_read = 2;
	f->flag_operation = 0;

	/* Obtain the file name */
	if (f->fcd && f->fcd->fnamePtr && f->fcd->fnamePtr[0]) {	
		/* copy fieldname and cut at last space */
		/* CHECKME: at least in the open case we directly copied
		            the assign field before and could use the code below */
		size_t i, max;
		max = LDCOMPX2 (f->fcd->fnameLen);
		if (max > (size_t)COB_FILE_MAX) {
			max = (size_t)COB_FILE_MAX;
		}
		strncpy (file_open_name, f->fcd->fnamePtr, max);
		for (i = max - 1; ; i--) {
			if (file_open_name[i] && file_open_name[i] != ' ') {
				file_open_name[i + 1] = 0;
				break;
			}
			if (!i) {
				break;
			}
		}
	} else
	if (f->assign != NULL
	 && f->assign->data != NULL) {
		cob_field_to_string (f->assign, file_open_name, (size_t)COB_FILE_MAX);
	}
}

/*
 * Open the data file
 */

void
cob_open (cob_file *f, const int mode, const int sharing, cob_field *fnstatus)
{
	/*: GC4: mode as cob_open_mode */

	last_operation_open = 1;

	/* File was previously closed with lock */
	if (f->open_mode == COB_OPEN_LOCKED) {
		save_status (f, fnstatus, COB_STATUS_38_CLOSED_WITH_LOCK);
		return;
	}

	/* File is already open */
	if (f->open_mode != COB_OPEN_CLOSED) {
		save_status (f, fnstatus, COB_STATUS_41_ALREADY_OPEN);
		return;
	}

	f->flag_read_done = 0;
	f->curkey = -1;
	f->mapkey = -1;

	f->last_open_mode = mode;
	f->flag_nonexistent = 0;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
	f->flag_first_read = 2;
	f->flag_operation = 0;
	f->lock_mode &= ~COB_LOCK_OPEN_EXCLUSIVE;
	f->lock_mode |= sharing;

	if (f->fcd) {
		cob_fcd_file_sync (f, file_open_name);		/* Copy app's FCD to cob_file */
	}

	cob_pre_open (f);

	if (unlikely (COB_FILE_STDIN (f))) {
		if (mode != COB_OPEN_INPUT) {
			save_status (f, fnstatus, COB_STATUS_30_PERMANENT_ERROR);
			return;
		}
		f->file = stdin;
		f->fd = fileno (stdin);
		f->open_mode = mode;
		save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
		return;
	}
	if (unlikely (COB_FILE_STDOUT (f))) {
		if (mode != COB_OPEN_OUTPUT) {
			save_status (f, fnstatus, COB_STATUS_30_PERMANENT_ERROR);
			return;
		}
		f->file = stdout;
		f->fd = fileno (stdout);
		f->open_mode = mode;
		save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
		return;
	}

	if (f->assign == NULL) {
		/* CHECKME: that _seems_ to be a codegen error, but may also happen with EXTFH */
		cob_runtime_error (_("ERROR FILE %s has ASSIGN field is NULL"),
							f->select_name);
		save_status (f, fnstatus, COB_STATUS_31_INCONSISTENT_FILENAME);
		return;
	}
	if (f->assign->data == NULL) {
#if 0 /* we don't raise an error in other places and a similar error is raised in cob_fatal_error */
		cob_runtime_error ("file %s has ASSIGN field with NULL address",
			f->select_name);
#endif
		save_status (f, fnstatus, COB_STATUS_31_INCONSISTENT_FILENAME);
		return;
	}

	if (file_open_name[0] == 0) {
		save_status (f, fnstatus, COB_STATUS_31_INCONSISTENT_FILENAME);
		return;
	}

	cob_cache_file (f);

	f->flag_is_concat = 0;
#if 0 /* Note: file specific features are 4.x only ... */
	if (file_setptr->cob_concat_name
	 && (f->organization == COB_ORG_SEQUENTIAL
	  || f->organization == COB_ORG_LINE_SEQUENTIAL) 
	 && (mode == COB_OPEN_INPUT 
	  || mode == COB_OPEN_I_O)
	 && strchr(file_open_name, file_setptr->cob_concat_sep[0]) != NULL
	 && file_open_name[0] != '>'
	 && file_open_name[0] != '<'
	 && file_open_name[0] != '|') {
		f->flag_is_concat = 1;
		f->org_filename = cob_strdup (file_open_name);
		f->nxt_filename = strchr (f->org_filename, file_setptr->cob_concat_sep[0]);
		*f->nxt_filename++ = 0;
		file_open_name = f->org_filename;
	}
#else
	if (cobsetptr->cob_concat_name
	 && (f->organization == COB_ORG_SEQUENTIAL
	  || f->organization == COB_ORG_LINE_SEQUENTIAL) 
	 && (mode == COB_OPEN_INPUT 
	  || mode == COB_OPEN_I_O)
	 && strchr(file_open_name, cobsetptr->cob_concat_sep[0]) != NULL
	 && file_open_name[0] != '>'
	 && file_open_name[0] != '<'
	 && file_open_name[0] != '|') {
		f->flag_is_concat = 1;
		f->org_filename = cob_strdup (file_open_name);
		f->nxt_filename = strchr (f->org_filename, cobsetptr->cob_concat_sep[0]);
		*f->nxt_filename++ = 0;
		file_open_name = f->org_filename;
	}
#endif

	/* Open the file */
	save_status (f, fnstatus,
		     fileio_funcs[(int)f->organization]->open (f, file_open_name,
								mode, sharing));
}

void
cob_close (cob_file *f, cob_field *fnstatus, const int opt, const int remfil)
{
	struct file_list	*l;
	struct file_list	*m;
	int			ret;

	f->flag_read_done = 0;
	f->flag_operation = 0;

	f->lock_mode &= ~COB_LOCK_OPEN_EXCLUSIVE;

	if (COB_FILE_SPECIAL (f)) {
		f->open_mode = COB_OPEN_CLOSED;
		f->file = NULL;
		f->fd = -1;
		save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
		return;
	}

	if (unlikely (remfil)) {
		/* Remove from cache - Needed for CANCEL */
		/* Setting m silences false compiler warning */
		m = file_cache;
		for (l = file_cache; l; l = l->next) {
			if (f == l->file) {
				if (l == file_cache) {
					file_cache = l->next;
				} else {
					m->next = l->next;
				}
				cob_free (l);
				break;
			}
			m = l;
		}
	}

	if (f->open_mode == COB_OPEN_CLOSED) {
		save_status (f, fnstatus, COB_STATUS_42_NOT_OPEN);
		return;
	}

	if (f->flag_nonexistent) {
		ret = COB_STATUS_00_SUCCESS;
	} else {
		ret = fileio_funcs[(int)f->organization]->close (f, opt);
	}

	if (ret == COB_STATUS_00_SUCCESS) {
		switch (opt) {
		case COB_CLOSE_LOCK:
			f->open_mode = COB_OPEN_LOCKED;
			break;
		default:
			f->open_mode = COB_OPEN_CLOSED;
			break;
		}
	}

	save_status (f, fnstatus, ret);
}

#if	0	/* RXWRXW - unlock */
void
cob_unlock (cob_file *f)
{
	int	ret;

	f->flag_read_done = 0;

	if (f->open_mode == COB_OPEN_CLOSED) {
		save_status (f, fnstatus, COB_STATUS_42_NOT_OPEN);
		return;
	}

	if (f->flag_nonexistent) {
		ret = COB_STATUS_00_SUCCESS;
	} else {
		ret = fileio_funcs[(int)f->organization]->close (f, opt);
	}

	save_status (f, fnstatus, ret);
}
#endif

void
cob_start (cob_file *f, const int cond, cob_field *key,
	   cob_field *keysize, cob_field *fnstatus)
{
	int		ret;
	cob_field	tempkey;

	f->flag_read_done = 0;
	f->flag_first_read = 0;

	if (unlikely (f->open_mode != COB_OPEN_I_O
		       && f->open_mode != COB_OPEN_INPUT)) {
		save_status (f, fnstatus, COB_STATUS_47_INPUT_DENIED);
		return;
	}

	if (unlikely (f->access_mode == COB_ACCESS_RANDOM)) {
		save_status (f, fnstatus, COB_STATUS_47_INPUT_DENIED);
		return;
	}

	if (f->flag_nonexistent) {
		save_status (f, fnstatus, COB_STATUS_23_KEY_NOT_EXISTS);
		return;
	}

	if (unlikely (keysize)) {
		int		size = cob_get_int (keysize);
		if (size < 1 || size > (int)key->size) {
			save_status (f, fnstatus, COB_STATUS_23_KEY_NOT_EXISTS);
			return;
		}
		tempkey = *key;
		tempkey.size = (size_t)size;
		ret = fileio_funcs[(int)f->organization]->start (f, cond, &tempkey);
	} else {
		ret = fileio_funcs[(int)f->organization]->start (f, cond, key);
	}
	if (ret == COB_STATUS_00_SUCCESS) {
		f->flag_end_of_file = 0;
		f->flag_begin_of_file = 0;
		f->flag_first_read = 1;
	} else {
		f->flag_end_of_file = 1;
		f->flag_begin_of_file = 0;
		f->flag_first_read = 1;
	}

	save_status (f, fnstatus, ret);
}

void
cob_read (cob_file *f, cob_field *key, cob_field *fnstatus, const int read_opts)
{
	int	ret;

	f->flag_read_done = 0;

	if (unlikely (f->open_mode != COB_OPEN_INPUT
		       && f->open_mode != COB_OPEN_I_O)) {
		save_status (f, fnstatus, COB_STATUS_47_INPUT_DENIED);
		return;
	}

	if (unlikely (f->flag_nonexistent)) {
		if (f->flag_first_read == 0) {
			save_status (f, fnstatus, COB_STATUS_23_KEY_NOT_EXISTS);
			return;
		}
		f->flag_first_read = 0;
		save_status (f, fnstatus, COB_STATUS_10_END_OF_FILE);
		return;
	}

	/* Sequential read at the end of file is an error */
	if (key == NULL) {
		if (unlikely (f->flag_end_of_file
		           && !(read_opts & COB_READ_PREVIOUS))) {
			save_status (f, fnstatus, COB_STATUS_46_READ_ERROR);
			return;
		}
		if (unlikely (f->flag_begin_of_file
			       && (read_opts & COB_READ_PREVIOUS))) {
			save_status (f, fnstatus, COB_STATUS_46_READ_ERROR);
			return;
		}
		ret = fileio_funcs[(int)f->organization]->read_next (f, read_opts);
	} else {
		ret = fileio_funcs[(int)f->organization]->read (f, key, read_opts);
	}

	switch (ret) {
	case COB_STATUS_00_SUCCESS:
	case COB_STATUS_02_SUCCESS_DUPLICATE:
	case COB_STATUS_04_SUCCESS_INCOMPLETE:
	case COB_STATUS_06_READ_TRUNCATE:
	case COB_STATUS_09_READ_DATA_BAD:
		f->flag_first_read = 0;
		f->flag_read_done = 1;
		f->flag_end_of_file = 0;
		f->flag_begin_of_file = 0;
		if (f->variable_record) {
			cob_set_int (f->variable_record, (int) f->record->size);
		}
		break;
	case COB_STATUS_10_END_OF_FILE:
		if (read_opts & COB_READ_PREVIOUS) {
			f->flag_begin_of_file = 1;
		} else {
			f->flag_end_of_file = 1;
		}
		break;
	}

	save_status (f, fnstatus, ret);
}

static int
is_suppressed_key_value (cob_file *f, const int idx)
{
	if (idx < 0 || idx >= (int)f->nkeys) {
		return -1;
	}
#if 0	/* TODO: SUPPRESS string not merged yet */
	if (f->keys[idx].len_suppress > 0) {
		(void) cob_savekey (f, idx, f->keys[idx].field->data);
		if (memcmp (f->keys[idx].field->data,
			        f->keys[idx].str_suppress,
			        f->keys[idx].len_suppress) == 0) {
			return 1;
		}
	} else
#endif
	if (f->keys[idx].tf_suppress) {
		int pos = cob_savekey (f, idx, f->keys[idx].field->data);
		for (pos = 0;
			 pos < (int)f->keys[idx].field->size
		  && f->keys[idx].field->data[pos] == (unsigned char)f->keys[idx].char_suppress;
			 pos++); 
		/* All SUPPRESS char ? */
		if (pos == f->keys[idx].field->size) {
			return 1;
		}
	}
	return 0;
}


void
cob_read_next (cob_file *f, cob_field *fnstatus, const int read_opts)
{
	int	ret,idx;

	f->flag_read_done = 0;

	if (unlikely (f->open_mode != COB_OPEN_INPUT
		       && f->open_mode != COB_OPEN_I_O)) {
		save_status (f, fnstatus, COB_STATUS_47_INPUT_DENIED);
		return;
	}

	if (unlikely (f->flag_nonexistent)) {
		if (f->flag_first_read == 0) {
			save_status (f, fnstatus, COB_STATUS_46_READ_ERROR);
			return;
		}
		f->flag_first_read = 0;
		save_status (f, fnstatus, COB_STATUS_10_END_OF_FILE);
		return;
	}

	/* Sequential read at the end of file is an error */
	if (unlikely (f->flag_end_of_file && !(read_opts & COB_READ_PREVIOUS))) {
		save_status (f, fnstatus, COB_STATUS_46_READ_ERROR);
		return;
	}
	if (unlikely (f->flag_begin_of_file && (read_opts & COB_READ_PREVIOUS))) {
		save_status (f, fnstatus, COB_STATUS_46_READ_ERROR);
		return;
	}

Again:
	ret = fileio_funcs[(int)f->organization]->read_next (f, read_opts);

	switch (ret) {
	case COB_STATUS_00_SUCCESS:
	case COB_STATUS_02_SUCCESS_DUPLICATE:
	case COB_STATUS_04_SUCCESS_INCOMPLETE:
	case COB_STATUS_06_READ_TRUNCATE:
	case COB_STATUS_09_READ_DATA_BAD:
		/* If record has suppressed key, skip it */
		/* This is to catch CISAM, old VBISAM, ODBC & OCI */
		if (f->organization == COB_ORG_INDEXED) {
			idx = f->curkey;
			if (f->mapkey >= 0) {	/* FD has Indexes in alternate appearance */
				idx = f->mapkey;
			}
			if (is_suppressed_key_value (f, idx) > 0) {
				/* SUPPRESS -> so skip */
				goto Again;
			}
		}

		f->flag_first_read = 0;
		f->flag_read_done = 1;
		f->flag_end_of_file = 0;
		f->flag_begin_of_file = 0;
		if (f->variable_record) {
			cob_set_int (f->variable_record, (int) f->record->size);
		}
		break;
	case COB_STATUS_10_END_OF_FILE:
		if (read_opts & COB_READ_PREVIOUS) {
			f->flag_begin_of_file = 1;
		} else {
			f->flag_end_of_file = 1;
		}
		break;
	}

	save_status (f, fnstatus, ret);
}

/* CODE-SET conversion for re-/write */
static unsigned char *
get_code_set_converted_data (cob_file *f)
{
	const size_t size = f->record->size;
	unsigned char *real_rec_data = f->record->data;
	unsigned char *converted_copy = cob_malloc (size);
	if (!converted_copy) return NULL;

	if (f->nconvert_fields) {
		/* CODE-SET FOR - convert specific areas only */
		const unsigned char* rec_end = converted_copy + size;
		size_t ic;
		memcpy (converted_copy, real_rec_data, size);
		for (ic = 0; ic < f->nconvert_fields; ic++) {
			const cob_field to_conv = f->convert_field[ic];
			const unsigned char* to_conv_end = to_conv.data + to_conv.size;
			const unsigned char* conv_end = rec_end < to_conv_end ? rec_end : to_conv_end;
			unsigned char* p;
			for (p = to_conv.data; p < conv_end; p++) {
				*p = f->sort_collating[*p];
			}
		}
	} else {
		/* CODE-SET FOR - convert complete record */
		const unsigned char *rec_end = real_rec_data + size;
		unsigned char *d, *p;
		for (d = converted_copy, p = real_rec_data; p < rec_end; d++, p++) {
			*d = f->sort_collating[*p];
		}
	}

	return converted_copy;
}

void
cob_write (cob_file *f, cob_field *rec, const int opt, cob_field *fnstatus,
	   const unsigned int check_eop)
{
	f->flag_read_done = 0;

	if (f->access_mode == COB_ACCESS_SEQUENTIAL) {
		if (unlikely (f->open_mode != COB_OPEN_OUTPUT
			       && f->open_mode != COB_OPEN_EXTEND)) {
			save_status (f, fnstatus, COB_STATUS_48_OUTPUT_DENIED);
			return;
		}
	} else {
		if (unlikely (f->open_mode != COB_OPEN_OUTPUT
			       && f->open_mode != COB_OPEN_I_O)) {
			save_status (f, fnstatus, COB_STATUS_48_OUTPUT_DENIED);
			return;
		}
	}

	if (f->variable_record) {
		f->record->size = (size_t)cob_get_int (f->variable_record);
		if (unlikely (f->record->size > rec->size)) {
			f->record->size = rec->size;
		}
	} else {
		f->record->size = rec->size;
	}

	if (f->record->size < f->record_min || f->record_max < f->record->size) {
		save_status (f, fnstatus, COB_STATUS_44_RECORD_OVERFLOW);
		return;
	}

	check_eop_status = check_eop;

	if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
		/* Re-Determine the size to be written (done here so possible
		   CODE-SET conversions do not convert trailing spaces when
		   not part of the record [= fixed length] */
		const size_t size = lineseq_size (f);
		/* early pre-validation for data we'd otherwise convert */
		if (cobsetptr->cob_ls_validate
		 && !f->flag_line_adv
		 && f->sort_collating) {
			const unsigned char *p = f->record->data;
			size_t i;
			for (i = 0; i < size; ++i, ++p) {
				if (IS_BAD_CHAR (*p)) {
					save_status (f, fnstatus, COB_STATUS_71_BAD_CHAR);
					return;
				}
			}
		}
		f->record->size = size;
	}

	/* CODE-SET conversion (write from converted shadow-copy) */
	if (f->organization != COB_ORG_SORT && f->sort_collating) {
		unsigned char *real_rec_data = f->record->data;
		unsigned char *converted_copy = get_code_set_converted_data (f);
		if (!converted_copy) {
			save_status (f, fnstatus, COB_STATUS_30_PERMANENT_ERROR);
			return;
		}
		f->record->data = converted_copy;
		save_status (f, fnstatus,
				fileio_funcs[(int)f->organization]->write (f, opt));
		f->record->data = real_rec_data;
		cob_free (converted_copy);
		return;
	}

	save_status (f, fnstatus,
		     fileio_funcs[(int)f->organization]->write (f, opt));
}

void
cob_rewrite (cob_file *f, cob_field *rec, const int opt, cob_field *fnstatus)
{
	int	read_done;

	read_done = f->flag_read_done;
	f->flag_read_done = 0;

	if (unlikely (f->open_mode != COB_OPEN_I_O)) {
		save_status (f, fnstatus, COB_STATUS_49_I_O_DENIED);
		return;
	}

	if (f->access_mode == COB_ACCESS_SEQUENTIAL && !read_done) {
		save_status (f, fnstatus, COB_STATUS_43_READ_NOT_DONE);
		return;
	}

	if (unlikely (f->organization == COB_ORG_SEQUENTIAL)) {
		if (f->record->size != rec->size) {
			save_status (f, fnstatus, COB_STATUS_44_RECORD_OVERFLOW);
			return;
		}

		if (f->variable_record) {
			if (f->record->size != (size_t)cob_get_int (f->variable_record)) {
				save_status (f, fnstatus, COB_STATUS_44_RECORD_OVERFLOW);
				return;
			}
		}
	}

	if (f->variable_record) {
		f->record->size = (size_t)cob_get_int (f->variable_record);
		if (unlikely(f->record->size > rec->size)) {
			f->record->size = rec->size;
		}
		if (f->record->size < f->record_min || f->record_max < f->record->size) {
			save_status (f, fnstatus, COB_STATUS_44_RECORD_OVERFLOW);
			return;
		}
	} else {
		f->record->size = rec->size;
	}

	if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
		/* Re-Determine the size to be written (done here so possible
		   CODE-SET conversions do not convert trailing spaces when
		   not part of the record [= fixed length] */
		f->record->size = lineseq_size (f);
	}

	/* CODE-SET conversion (rewrite from converted shadow-copy) */
	if (f->organization != COB_ORG_SORT && f->sort_collating) {
		unsigned char *real_rec_data = f->record->data;
		unsigned char *converted_copy = get_code_set_converted_data (f);
		if (!converted_copy) {
			save_status (f, fnstatus, COB_STATUS_30_PERMANENT_ERROR);
			return;
		}
		f->record->data = converted_copy;
		save_status (f, fnstatus,
				 fileio_funcs[(int)f->organization]->rewrite (f, opt));
		f->record->data = real_rec_data;
		cob_free (converted_copy);
		return;
	}

	save_status (f, fnstatus,
		     fileio_funcs[(int)f->organization]->rewrite (f, opt));
}

void
cob_delete (cob_file *f, cob_field *fnstatus)
{
	int	read_done;

	read_done = f->flag_read_done;
	f->flag_read_done = 0;

	if (unlikely (f->open_mode != COB_OPEN_I_O)) {
		save_status (f, fnstatus, COB_STATUS_49_I_O_DENIED);
		return;
	}

	if (f->access_mode == COB_ACCESS_SEQUENTIAL && !read_done) {
		save_status (f, fnstatus, COB_STATUS_43_READ_NOT_DONE);
		return;
	}

	save_status (f, fnstatus,
		     fileio_funcs[(int)f->organization]->fdelete (f));
}

void
cob_commit (void)
{
	struct file_list	*l;

	for (l = file_cache; l; l = l->next) {
		if (l->file) {
			cob_file_unlock (l->file);
		}
	}
}

void
cob_rollback (void)
{
	struct file_list	*l;

	for (l = file_cache; l; l = l->next) {
		if (l->file) {
			cob_file_unlock (l->file);
		}
	}
}

void
cob_delete_file (cob_file *f, cob_field *fnstatus)
{
	if (f->organization == COB_ORG_SORT) {
		save_status (f, fnstatus, COB_STATUS_30_PERMANENT_ERROR);
		return;
	}

	/* File was previously closed with lock */
	if (f->open_mode == COB_OPEN_LOCKED) {
		save_status (f, fnstatus, COB_STATUS_38_CLOSED_WITH_LOCK);
		return;
	}

	/* File is open */
	if (f->open_mode != COB_OPEN_CLOSED) {
		save_status (f, fnstatus, COB_STATUS_41_ALREADY_OPEN);
		return;
	}

	if (unlikely (COB_FILE_STDIN (f) || COB_FILE_STDOUT (f))) {
		save_status (f, fnstatus, COB_STATUS_30_PERMANENT_ERROR);
		return;
	}

	/* Obtain the file name */
	cob_field_to_string (f->assign, file_open_name, (size_t)COB_FILE_MAX);
	cob_chk_file_mapping ();

	if (f->organization != COB_ORG_INDEXED) {
#ifdef	WITH_SEQRA_EXTFH
		save_status (f, fnstatus, COB_STATUS_91_NOT_AVAILABLE);
		return;
#else
		unlink (file_open_name);
#endif
	} else {
#ifdef	WITH_INDEX_EXTFH
		save_status (f, fnstatus, COB_STATUS_91_NOT_AVAILABLE);
		return;
#else
		indexed_file_delete (f, file_open_name);
#endif
	}
	save_status (f, fnstatus, errno_cob_sts(COB_STATUS_00_SUCCESS));
}

/* Return index number for given key */
int
cob_findkey (cob_file *f, cob_field *kf, int *fullkeylen, int *partlen)
{
	int 	k,part;
	*fullkeylen = *partlen = 0;

	for (k = 0; k < f->nkeys; ++k) {
		if (f->keys[k].field
		 && f->keys[k].count_components <= 1
		 && f->keys[k].field->data == kf->data) {
#if 0 /* pending merge of r1411 */
			f->last_key = f->keys[k].field;
#endif
			*fullkeylen = f->keys[k].field->size;
			*partlen = kf->size;
			return k;
		}
	}
	for (k = 0; k < f->nkeys; ++k) {
		if (f->keys[k].count_components > 1) {
			if ((f->keys[k].field
			 &&  f->keys[k].field->data == kf->data
			 &&  f->keys[k].field->size == kf->size)
			 || (f->keys[k].component[0]->data == kf->data)) {
#if 0 /* pending merge of r1411 */
				f->last_key = f->keys[k].field;
#endif
				for (part=0; part < f->keys[k].count_components; part++)
					*fullkeylen += f->keys[k].component[part]->size;
				if (f->keys[k].field
				 && f->keys[k].field->data == kf->data)
					*partlen = kf->size;
				else
					*partlen = *fullkeylen;
				return k;
			}
		}
	}
	return -1;
}

/* Copy key data and return length of data copied */
static int
cob_savekey (cob_file *f, int idx, unsigned char *data)
{
	int 	len,part;

	if (f->keys[idx].field == NULL)
		return -1;
	if (f->keys[idx].count_components <= 1) {
		if (data != f->keys[idx].field->data) {
			memcpy (data, f->keys[idx].field->data, f->keys[idx].field->size);
		}
		return (int)f->keys[idx].field->size;
	}
	for (len=part=0; part < f->keys[idx].count_components; part++) {
		memcpy (&data[len], f->keys[idx].component[part]->data,
							f->keys[idx].component[part]->size);
		len += f->keys[idx].component[part]->size;
	}
	return len;
}

/* System routines */

static void *
cob_str_from_fld (const cob_field *f)
{
	void		*mptr;
	unsigned char	*s;
	size_t		i, n, j;
#if	0	/* Quotes in file */
	int		quote_switch;

	quote_switch = 0;
#endif

	if (!f) {
		return cob_malloc ((size_t)1);
	}
	for (i = f->size - 1; i > 0; --i) {
		if (f->data[i] != ' ' && f->data[i] != 0) {
			i++;
			break;
		}
	}
	/* i is 0 or > 0 */
	mptr = cob_malloc (i + 1);
	s = mptr;
	j = 0;
	for (n = 0; n < i; ++n) {
		if (f->data[n] == '"') {
			continue;
		}
		s[j++] = f->data[n];
#if	0	/* Quotes in file */
		if (f->data[n] == '"') {
			quote_switch = !quote_switch;
			continue;
		}
		s[j] = f->data[n];
		if (quote_switch) {
			j++;
			continue;
		}
		if (s[j] == ' ' || s[j] == 0) {
			s[j] = 0;
			break;
		}
		j++;
#endif
	}
	return mptr;
}

/* actual processing for CBL_OPEN_FILE and CBL_CREATE_FILE */
static int
open_cbl_file (unsigned char *file_name, unsigned char *file_access,
	       unsigned char *file_handle, const int file_flags)
{
	int	flag = O_BINARY;
	int	fd;

	COB_UNUSED (file_name);

	if (!COB_MODULE_PTR->cob_procedure_params[0]) {
		memset (file_handle, -1, (size_t)4);
		return -1;
	}
	flag |= file_flags;
	switch (*file_access & 0x3F) {
		case 1:
			flag |= O_RDONLY;
			break;
		case 2:
			flag |= O_CREAT | O_TRUNC | O_WRONLY;
			break;
		case 3:
			flag |= O_RDWR;
			break;
		default:
			cob_runtime_warning (_("call to CBL_OPEN_FILE with wrong access mode: %d"), *file_access & 0x3F);
			memset (file_handle, -1, (size_t)4);
			return -1;
	}

	{
		char	*fn = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[0]);
		strncpy (file_open_name, fn, (size_t)COB_FILE_MAX);
		file_open_name[COB_FILE_MAX] = 0;
		cob_free (fn);
	}

	cob_chk_file_mapping ();

	fd = open (file_open_name, flag, COB_FILE_MODE);
	if (fd < 0) {
		memset (file_handle, -1, (size_t)4);
		return 35;
	}
	memcpy (file_handle, &fd, (size_t)4);
	return 0;
}

/* entry point for library routine CBL_OPEN_FILE */
int
cob_sys_open_file (unsigned char *file_name, unsigned char *file_access,
		   unsigned char *file_lock, unsigned char *file_dev,
		   unsigned char *file_handle)
{
	COB_UNUSED (file_lock);
	COB_UNUSED (file_dev);

	COB_CHK_PARMS (CBL_OPEN_FILE, 5);

#ifdef	WORDS_BIGENDIAN
	/* if value is passed as numeric literal, it becomes an 'int' so value is in 4th byte */
	if (file_access[0] == 0
	 && file_access[1] == 0
	 && file_access[2] == 0)
		file_access += 3;
	if (file_lock[0] == 0
	 && file_lock[1] == 0
	 && file_lock[2] == 0)
		file_lock += 3;
	if (file_dev[0] == 0
	 && file_dev[1] == 0
	 && file_dev[2] == 0)
		file_dev += 3;
#endif

	return open_cbl_file (file_name, file_access, file_handle, 0);
}

/* entry point for library routine CBL_CREATE_FILE */
int
cob_sys_create_file (unsigned char *file_name, unsigned char *file_access,
		     unsigned char *file_lock, unsigned char *file_dev,
		     unsigned char *file_handle)
{
	/*
	 * @param: file_access : 1 (read-only), 2 (write-only), 3 (both)
	 * @param: file_lock : not implemented, set 0
	 * @param: file_dev : not implemented, set 0
	 */

	COB_CHK_PARMS (CBL_CREATE_FILE, 5);

#ifdef	WORDS_BIGENDIAN
	/* if value is passed as numeric literal, it becomes an 'int' so value is in 4th byte */
	if (file_access[0] == 0
	 && file_access[1] == 0
	 && file_access[2] == 0)
		file_access += 3;
	if (file_lock[0] == 0
	 && file_lock[1] == 0
	 && file_lock[2] == 0)
		file_lock += 3;
	if (file_dev[0] == 0
	 && file_dev[1] == 0
	 && file_dev[2] == 0)
		file_dev += 3;
#endif

	if (*file_lock != 0) {
		cob_runtime_warning (_("call to CBL_CREATE_FILE with wrong file_lock: %d"), *file_lock);
	}
	if (*file_dev != 0) {
		cob_runtime_warning (_("call to CBL_CREATE_FILE with wrong file_dev: %d"), *file_dev);
	}

	return open_cbl_file (file_name, file_access, file_handle, O_CREAT | O_TRUNC);
}

/* entry point and processing for library routine CBL_READ_FILE */
int
cob_sys_read_file (unsigned char *file_handle, unsigned char *file_offset,
		   unsigned char *file_len, unsigned char *flags,
		   unsigned char *buf)
{
	cob_s64_t	off;
	int		fd;
	int		len;
	int		rc;
	struct stat	st;

	COB_CHK_PARMS (CBL_READ_FILE, 5);

	memcpy (&fd, file_handle, (size_t)4);
	memcpy (&off, file_offset, (size_t)8);
	memcpy (&len, file_len, (size_t)4);
#ifndef	WORDS_BIGENDIAN
	off = COB_BSWAP_64 (off);
	len = COB_BSWAP_32 (len);
#endif
	if (lseek (fd, (off_t)off, SEEK_SET) == (off_t)-1) {
		return -1;
	}

	if (len > 0) {
		rc = read (fd, buf, (size_t)len);
		if (rc < 0) {
			rc = -1;
		} else if (rc == 0) {
			rc = 10;
		} else {
			rc = 0;
		}
	} else {
		rc = 0;
	}
	if ((*flags & 0x80) != 0) {
		if (fstat (fd, &st) < 0) {
			return -1;
		}
		off = st.st_size;
#ifndef	WORDS_BIGENDIAN
		off = COB_BSWAP_64 (off);
#endif
		memcpy (file_offset, &off, (size_t)8);
	}
	return rc;
}

/* entry point and processing for library routine CBL_WRITE_FILE */
int
cob_sys_write_file (unsigned char *file_handle, unsigned char *file_offset,
		    unsigned char *file_len, unsigned char *flags,
		    unsigned char *buf)
{
	cob_s64_t	off;
	int		fd;
	int		len;
	int		rc;

	COB_UNUSED (flags);

	COB_CHK_PARMS (CBL_WRITE_FILE, 5);

	memcpy (&fd, file_handle, (size_t)4);
	memcpy (&off, file_offset, (size_t)8);
	memcpy (&len, file_len, (size_t)4);
#ifndef	WORDS_BIGENDIAN
	off = COB_BSWAP_64 (off);
	len = COB_BSWAP_32 (len);
#endif
	if (lseek (fd, (off_t)off, SEEK_SET) == (off_t)-1) {
		return -1;
	}
	rc = (int) write (fd, buf, (size_t)len);
	if (rc != len) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	return COB_STATUS_00_SUCCESS;
}

/* entry point and processing for library routine CBL_CLOSE_FILE */
int
cob_sys_close_file (unsigned char *file_handle)
{
	int	fd;

	COB_CHK_PARMS (CBL_CLOSE_FILE, 1);

	memcpy (&fd, file_handle, (size_t)4);
	return close (fd);
}

/* entry point and processing for library routine CBL_FLUSH_FILE
   (flush bytestream file handle, got from CBL_OPEN_FILE) */
int
cob_sys_flush_file (unsigned char *file_handle)
{
	int	fd;

	COB_CHK_PARMS (CBL_FLUSH_FILE, 1);

	memcpy (&fd, file_handle, sizeof (int));
	return fdcobsync (fd);
}

/* entry point and processing for library routine CBL_DELETE_FILE */
int
cob_sys_delete_file (unsigned char *file_name)
{
	int	ret;

	COB_UNUSED (file_name);

	COB_CHK_PARMS (CBL_DELETE_FILE, 1);

	if (!COB_MODULE_PTR->cob_procedure_params[0]) {
		return -1;
	}

	{
		char	*fn = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[0]);
		strncpy (file_open_name, fn, (size_t)COB_FILE_MAX);
		file_open_name[COB_FILE_MAX] = 0;
		cob_free (fn);
	}
	cob_chk_file_mapping ();

	ret = unlink (file_open_name);
	if (ret) {
		return 128;
	}
	return 0;
}

/* entry point and processing for library routine CBL_COPY_FILE,
   does a direct read + write of the complete file */
int
cob_sys_copy_file (unsigned char *fname1, unsigned char *fname2)
{
	int	flag = O_BINARY;
	int	ret;
	int	i;
	int	fd1, fd2;

	COB_UNUSED (fname1);
	COB_UNUSED (fname2);

	COB_CHK_PARMS (CBL_COPY_FILE, 2);

	if (!COB_MODULE_PTR->cob_procedure_params[0]) {
		return -1;
	}
	if (!COB_MODULE_PTR->cob_procedure_params[1]) {
		return -1;
	}

	{
		char	*fn = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[0]);
		strncpy (file_open_name, fn, (size_t)COB_FILE_MAX);
		file_open_name[COB_FILE_MAX] = 0;
		cob_free (fn);
	}
	cob_chk_file_mapping ();

	flag |= O_RDONLY;
	fd1 = open (file_open_name, flag, 0);
	if (fd1 < 0) {
		return -1;
	}

	{
		char	*fn = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[1]);
		strncpy (file_open_name, fn, (size_t)COB_FILE_MAX);
		file_open_name[COB_FILE_MAX] = 0;
		cob_free (fn);
	}
	cob_chk_file_mapping ();

	flag &= ~O_RDONLY;
	flag |= O_CREAT | O_TRUNC | O_WRONLY;
	fd2 = open (file_open_name, flag, COB_FILE_MODE);
	if (fd2 < 0) {
		close (fd1);
		return -1;
	}

	ret = 0;
	while ((i = read (fd1, file_open_buff, COB_FILE_BUFF)) > 0) {
		if (write (fd2, file_open_buff, (size_t)i) != (size_t)i) {
			ret = -1;
			break;
		}
	}
	close (fd1);
	close (fd2);
	return ret;
}

/* entry point and processing for library routine CBL_CHECK_FILE_EXIST */
int
cob_sys_check_file_exist (unsigned char *file_name, unsigned char *file_info)
{
	struct tm	*tm;
	cob_s64_t	sz;
	struct stat	st;
	short		y;
	short		d, m, hh, mm, ss;

	COB_UNUSED (file_name);

	COB_CHK_PARMS (CBL_CHECK_FILE_EXIST, 2);

	if (!COB_MODULE_PTR->cob_procedure_params[0]
	 || !COB_MODULE_PTR->cob_procedure_params[1]) {
		return -1;
	}
	if (COB_MODULE_PTR->cob_procedure_params[1]->size < 16U) {
		cob_runtime_error (_("'%s' - File detail area is too short"), "CBL_CHECK_FILE_EXIST");
#if 0 /* should be handled by the caller,
		TODO: check for better return code (or the one from MF/ACU) */
		cob_hard_failure ();
#else
		return -1;
#endif
	}

	{
		char	*fn = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[0]);
		strncpy (file_open_name, fn, (size_t)COB_FILE_MAX);
		cob_free (fn);
	}
	cob_chk_file_mapping ();

	if (stat (file_open_name, &st) < 0) {
		return 35;
	}

	sz = (cob_s64_t)st.st_size;
	tm = localtime (&st.st_mtime);
	d = (short)tm->tm_mday;
	m = (short)(tm->tm_mon + 1);
	y = (short)(tm->tm_year + 1900);
	hh = (short)tm->tm_hour;
	mm = (short)tm->tm_min;
	/* Leap seconds ? */
	if (tm->tm_sec >= 60) {
		ss = 59;
	} else {
		ss = (short)tm->tm_sec;
	}

#ifndef	WORDS_BIGENDIAN
	sz = COB_BSWAP_64 (sz);
	y = COB_BSWAP_16 (y);
#endif
	memcpy (file_info, &sz, (size_t)8);
	file_info[8] = (unsigned char)d;
	file_info[9] = (unsigned char)m;
	memcpy (file_info+10, &y, (size_t)2);
	file_info[12] = (unsigned char)hh;
	file_info[13] = (unsigned char)mm;
	file_info[14] = (unsigned char)ss;
	file_info[15] = 0;
	return 0;
}

/* entry point and processing for library routine CBL_RENAME_FILE */
int
cob_sys_rename_file (unsigned char *fname1, unsigned char *fname2)
{
	char	localbuff [COB_FILE_BUFF];
	int 	ret;

	COB_UNUSED (fname1);
	COB_UNUSED (fname2);

	COB_CHK_PARMS (CBL_RENAME_FILE, 2);

	if (!COB_MODULE_PTR->cob_procedure_params[0]) {
		return -1;
	}
	if (!COB_MODULE_PTR->cob_procedure_params[1]) {
		return -1;
	}

	{
		char	*fn = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[0]);
		strncpy (file_open_name, fn, (size_t)COB_FILE_MAX);
		file_open_name[COB_FILE_MAX] = 0;
		cob_free (fn);
	}
	cob_chk_file_mapping ();
	strncpy (localbuff, file_open_name, (size_t)COB_FILE_MAX);
	localbuff[COB_FILE_MAX] = 0;

	{
		char	*fn = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[1]);
		strncpy (file_open_name, fn, (size_t)COB_FILE_MAX);
		file_open_name[COB_FILE_MAX] = 0;
		cob_free (fn);
	}
	cob_chk_file_mapping ();

	ret = rename (localbuff, file_open_name);
	if (ret) {
		return 128;
	}
	return 0;
}

/* entry point and processing for library routine CBL_GET_CURRENT_DIR */
int
cob_sys_get_current_dir (const int flags, const int dir_length,
			 unsigned char *dir)
{
	char	*dirname;
	int	dir_size;
	int	has_space;

	COB_CHK_PARMS (CBL_GET_CURRENT_DIR, 3);

	if (dir_length < 1) {
		return 128;
	}
	if (flags) {
		return 129;
	}
	memset (dir, ' ', (size_t)dir_length);
	dirname = getcwd (NULL, (size_t)0);
	if (dirname == NULL) {
		return 128;
	}
	dir_size = (int) strlen (dirname);
	has_space = 0;
	if (strchr (dirname, ' ')) {
		has_space = 2;
	}
	if (dir_size + has_space > dir_length) {
		cob_free (dirname);
		return 128;
	}
	if (has_space) {
		*dir = '"';
		memcpy (&dir[1], dirname, (size_t)dir_size);
		dir[dir_size + 1] = '"';
	} else {
		memcpy (dir, dirname, (size_t)dir_size);
	}
	cob_free (dirname);
	return 0;
}

/* entry point and processing for library routine CBL_CREATE_DIR */
int
cob_sys_create_dir (unsigned char *dir)
{
	char	*fn;
	int	ret;

	COB_UNUSED (dir);

	COB_CHK_PARMS (CBL_CREATE_DIR, 1);

	if (!COB_MODULE_PTR->cob_procedure_params[0]) {
		return -1;
	}
	fn = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[0]);
	ret = mkdir (fn, 0770);
	cob_free (fn);
	if (ret) {
		return 128;
	}
	return 0;
}

/* entry point and processing for library routine CBL_CHANGE_DIR */
int
cob_sys_change_dir (unsigned char *dir)
{
	char	*fn;
	int	ret;

	COB_UNUSED (dir);

	COB_CHK_PARMS (CBL_CHANGE_DIR, 1);

	if (!COB_MODULE_PTR->cob_procedure_params[0]) {
		return -1;
	}
	fn = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[0]);
	ret = chdir (fn);
	cob_free (fn);
	if (ret) {
		return 128;
	}
	return 0;
}

/* entry point and processing for library routine CBL_DELETE_DIR */
int
cob_sys_delete_dir (unsigned char *dir)
{
	char	*fn;
	int	ret;

	COB_UNUSED (dir);

	COB_CHK_PARMS (CBL_DELETE_DIR, 1);

	if (!COB_MODULE_PTR->cob_procedure_params[0]) {
		return -1;
	}
	fn = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[0]);
	ret = rmdir (fn);
	cob_free (fn);
	if (ret) {
		return 128;
	}
	return 0;
}

/* entry point for C$MAKEDIR, processing in cob_sys_create_dir */
int
cob_sys_mkdir (unsigned char *dir)
{
	int		ret;

	COB_CHK_PARMS (C$MAKEDIR, 1);

	ret = cob_sys_create_dir (dir);
	if (ret < 0) {
		ret = 128;
	}
	return ret;
}

/* entry point for C$CHDIR, processing in cob_sys_change_dir */
int
cob_sys_chdir (unsigned char *dir, unsigned char *status)
{
	int		ret;

	COB_UNUSED (status);

	COB_CHK_PARMS (C$CHDIR, 2);

	ret = cob_sys_change_dir (dir);
	if (ret < 0) {
		ret = 128;
	}
	cob_set_int (COB_MODULE_PTR->cob_procedure_params[1], ret);
	return ret;
}

/* entry point for C$COPY, processing in cob_sys_copy_file */
int
cob_sys_copyfile (unsigned char *fname1, unsigned char *fname2,
		  unsigned char *file_type)
{
	int		ret;

	/* RXW - Type is not yet evaluated */
	COB_UNUSED (file_type);

	COB_CHK_PARMS (C$COPY, 3);

	if (cobglobptr->cob_call_params < 3) {
		return 128;
	}
	ret = cob_sys_copy_file (fname1, fname2);
	if (ret < 0) {
		ret = 128;
	}
	return ret;
}

/* entry point and processing for C$FILEINFO */
int
cob_sys_file_info (unsigned char *file_name, unsigned char *file_info)
{
	char			*fn;
	struct tm		*tm;
	cob_u64_t		sz;
	unsigned int		dt;
	short			y;
	short			d, m, hh, mm, ss;
	struct stat		st;

	COB_UNUSED (file_name);

	COB_CHK_PARMS (C$FILEINFO, 2);

	if (cobglobptr->cob_call_params < 2
	 || !COB_MODULE_PTR->cob_procedure_params[0]) {
		return 128;
	}
	if (!COB_MODULE_PTR->cob_procedure_params[1]) {
		return 128;
	}
	if (COB_MODULE_PTR->cob_procedure_params[1]->size < 16U) {
		cob_runtime_error (_("'%s' - File detail area is too short"), "C$FILEINFO");
#if 0 /* should be handled by the caller,
		TODO: check for better return code (or the one from ACU) */
		cob_hard_failure ();
#else
		return 128;
#endif
	}

	fn = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[0]);
	if (stat (fn, &st) < 0) {
		cob_free (fn);
		return 35;
	}
	cob_free (fn);
	sz = (cob_u64_t)st.st_size;
	tm = localtime (&st.st_mtime);
	d = (short)tm->tm_mday;
	m = (short)(tm->tm_mon + 1);
	y = (short)(tm->tm_year + 1900);
	hh = (short)tm->tm_hour;
	mm = (short)tm->tm_min;
	/* Leap seconds ? */
	if (tm->tm_sec >= 60) {
		ss = 59;
	} else {
		ss = (short)tm->tm_sec;
	}

#ifndef	WORDS_BIGENDIAN
	sz = COB_BSWAP_64 (sz);
#endif
	memcpy (file_info, &sz, (size_t)8);
	dt = (y * 10000) + (m * 100) + d;
#ifndef	WORDS_BIGENDIAN
	dt = COB_BSWAP_32 (dt);
#endif
	memcpy (file_info + 8, &dt, (size_t)4);
	dt = (hh * 1000000) + (mm * 10000) + (ss * 100);
#ifndef	WORDS_BIGENDIAN
	dt = COB_BSWAP_32 (dt);
#endif
	memcpy (file_info + 12, &dt, (size_t)4);
	return 0;
}

/* entry point for C$DELETE, processing in cob_sys_delete_file */
int
cob_sys_file_delete (unsigned char *file_name, unsigned char *file_type)
{
	int	ret;

	/* RXW - Type is not yet evaluated */
	COB_UNUSED (file_type);

	COB_CHK_PARMS (C$DELETE, 2);

	if (cobglobptr->cob_call_params < 2 ||
	    !COB_MODULE_PTR->cob_procedure_params[0]) {
		return 128;
	}
	ret = cob_sys_delete_file (file_name);
	if (ret < 0) {
		ret = 128;
	}
	return ret;
}

/* SORT */

static int
sort_cmps (const unsigned char *s1, const unsigned char *s2, const size_t size,
	   const unsigned char *col)
{
	size_t			i;
	int			ret;

	if (unlikely (col)) {
		for (i = 0; i < size; ++i) {
			if ((ret = col[s1[i]] - col[s2[i]]) != 0) {
				return ret;
			}
		}
	} else {
		for (i = 0; i < size; ++i) {
			if ((ret = s1[i] - s2[i]) != 0) {
				return ret;
			}
		}
	}
	return 0;
}

static COB_INLINE void
unique_copy (unsigned char *s1, const unsigned char *s2)
{
	size_t	size;

	size = sizeof (size_t);
	do {
		*s1++ = *s2++;
	} while (--size);
}

static int
cob_file_sort_compare (struct cobitem *k1, struct cobitem *k2, void *pointer)
{
	cob_file	*f;
	size_t		i;
	size_t		u1;
	size_t		u2;
	int		cmp;
	cob_field	f1;
	cob_field	f2;

	f = pointer;
	for (i = 0; i < f->nkeys; ++i) {
		f1 = f2 = *(f->keys[i].field);
		f1.data = k1->item + f->keys[i].offset;
		f2.data = k2->item + f->keys[i].offset;
		if (unlikely (COB_FIELD_IS_NUMERIC (&f1))) {
			cmp = cob_numeric_cmp (&f1, &f2);
		} else {
			cmp = sort_cmps (f1.data, f2.data, f1.size,
					 f->sort_collating);
		}
		if (cmp != 0) {
			return (f->keys[i].flag == COB_ASCENDING) ? cmp : -cmp;
		}
	}
	unique_copy ((unsigned char *)&u1, k1->unique);
	unique_copy ((unsigned char *)&u2, k2->unique);
	if (u1 < u2) {
		return -1;
	}
	return 1;
}

static void
cob_free_list (struct cobsort *hp)
{
	struct sort_mem_struct	*s1;
	struct sort_mem_struct	*s2;

	s1 = hp->mem_base;
	for (; s1;) {
		s2 = s1;
		s1 = s1->next;
		cob_free (s2->mem_ptr);
		cob_free (s2);
	}
}

static struct cobitem *
cob_new_item (struct cobsort *hp, const size_t size)
{
	struct cobitem		*q;
	struct sort_mem_struct	*s;

	COB_UNUSED (size);

	/* Creation of an empty item */
	if (unlikely (hp->empty != NULL)) {
		q = hp->empty;
		hp->empty = q->next;
		q->block_byte = 0;
		q->next = NULL;
		q->end_of_block = 0;
		return (void *)q;
	}
	if (unlikely ((hp->mem_used + hp->alloc_size) > hp->mem_size)) {
		s = cob_fast_malloc (sizeof (struct sort_mem_struct));
		s->mem_ptr = cob_fast_malloc (hp->chunk_size);
		s->next = hp->mem_base;
		hp->mem_base = s;
		hp->mem_size = hp->chunk_size;
		hp->mem_total += hp->chunk_size;
		hp->mem_used = 0;
	}
	q = (struct cobitem *)(hp->mem_base->mem_ptr + hp->mem_used);
	hp->mem_used += hp->alloc_size;
	if (unlikely (hp->mem_total >= cobsetptr->cob_sort_memory)) {
		if ((hp->mem_used + hp->alloc_size) > hp->mem_size) {
			hp->switch_to_file = 1;
		}
	}
	q->block_byte = 0;
	q->next = NULL;
	q->end_of_block = 0;
	return q;
}

FILE *
cob_create_tmpfile (const char *ext)
{
	FILE		*fp;
	char		*filename;
	int		fd;

	filename = cob_malloc ((size_t)COB_FILE_BUFF);
	cob_temp_name (filename, ext);
	cob_incr_temp_iteration ();
#ifdef	_WIN32
	fd = open (filename,
		    _O_CREAT | _O_TRUNC | _O_RDWR | _O_BINARY | _O_TEMPORARY,
		    _S_IREAD | _S_IWRITE);
#else
	fd = open (filename, O_CREAT | O_TRUNC | O_RDWR | O_BINARY, COB_FILE_MODE);
#endif
	if (fd < 0) {
		cob_free (filename);
		return NULL;
	}
	(void)unlink (filename);
	fp = fdopen (fd, "w+b");
	if (!fp) {
		close (fd);
	}
	cob_free (filename);
	return fp;
}

static int
cob_get_sort_tempfile (struct cobsort *hp, const int n)
{
	if (hp->file[n].fp == NULL) {
		hp->file[n].fp = cob_create_tmpfile (NULL);
#if 0 /* error to be handled by the caller */
		if (hp->file[n].fp == NULL) {
			cob_runtime_error (_("SORT is unable to acquire temporary file"));
			cob_hard_failure ();
		}
#endif
	} else {
		rewind (hp->file[n].fp);
	}
	hp->file[n].count = 0;
	return hp->file[n].fp == NULL;
}

static int
cob_sort_queues (struct cobsort *hp)
{
	struct cobitem	*q;
	int		source;
	int		destination;
	int		move;
	int		n;
	int		end_of_block[2];

	source = 0;
	while (hp->queue[source + 1].count != 0) {
		destination = source ^ 2;
		hp->queue[destination].first = NULL;
		hp->queue[destination].count = 0;
		hp->queue[destination + 1].first = NULL;
		hp->queue[destination + 1].count = 0;
		for (;;) {
			end_of_block[0] = hp->queue[source].count == 0;
			end_of_block[1] = hp->queue[source + 1].count == 0;
			if (end_of_block[0] && end_of_block[1]) {
				break;
			}
			while (!end_of_block[0] || !end_of_block[1]) {
				if (end_of_block[0]) {
					move = 1;
				} else if (end_of_block[1]) {
					move = 0;
				} else {
					n = cob_file_sort_compare
						(hp->queue[source].first,
						hp->queue[source + 1].first,
						hp->pointer);
					move = n < 0 ? 0 : 1;
				}
				q = hp->queue[source + move].first;
				if (q->end_of_block) {
					end_of_block[move] = 1;
				}
				hp->queue[source + move].first = q->next;
				if (hp->queue[destination].first == NULL) {
					hp->queue[destination].first = q;
				} else {
					hp->queue[destination].last->next = q;
				}
				hp->queue[destination].last = q;
				hp->queue[source + move].count--;
				hp->queue[destination].count++;
				q->next = NULL;
				q->end_of_block = 0;
			}
			hp->queue[destination].last->end_of_block = 1;
			destination ^= 1;
		}
		source = destination & 2;
	}
	return source;
}

static int
cob_read_item (struct cobsort *hp, const int n)
{
	FILE	*fp;

	fp = hp->file[n].fp;
	if (getc (fp) != 0) {
		hp->queue[n].first->end_of_block = 1;
	} else {
		hp->queue[n].first->end_of_block = 0;
		/* LCOV_EXCL_START */
		if (unlikely (fread (hp->queue[n].first->unique,
				hp->r_size, (size_t)1, fp) != 1)) {
			return 1;
		}
		/* LCOV_EXCL_STOP */
	}
	return 0;
}

static int
cob_write_block (struct cobsort *hp, const int n)
{
	struct cobitem	*q;
	FILE		*fp;

	fp = hp->file[hp->destination_file].fp;
	for (;;) {
		q = hp->queue[n].first;
		if (q == NULL) {
			break;
		}
		/* LCOV_EXCL_START */
		if (unlikely (fwrite (&(q->block_byte),
				hp->w_size, (size_t)1, fp) != 1)) {
			return 1;
		}
		/* LCOV_EXCL_STOP */
		hp->queue[n].first = q->next;
		q->next = hp->empty;
		hp->empty = q;
	}
	hp->queue[n].count = 0;
	hp->file[hp->destination_file].count++;
	/* LCOV_EXCL_START */
	if (unlikely (putc (1, fp) != 1)) {
		return 1;
	}
	/* LCOV_EXCL_STOP */
	return 0;
}

static void
cob_copy_check (cob_file *to, cob_file *from)
{
	unsigned char	*toptr;
	unsigned char	*fromptr;
	size_t		tosize;
	size_t		fromsize;

	toptr = to->record->data;
	fromptr = from->record->data;
	tosize = to->record->size;
	fromsize = from->record->size;
	if (unlikely (tosize > fromsize)) {
		memcpy (toptr, fromptr, fromsize);
		memset (toptr + fromsize, ' ', tosize - fromsize);
	} else {
		memcpy (toptr, fromptr, tosize);
	}
}

static int
cob_file_sort_process (struct cobsort *hp)
{
	int	i;
	int	source;
	int	destination;
	int	n;
	int	move;
	int	res;

	hp->retrieving = 1;
	n = cob_sort_queues (hp);
#if	0	/* RXWRXW - Cannot be true */
	/* LCOV_EXCL_START */
	if (unlikely (n < 0)) {
		return COBSORTABORT;
	}
	/* LCOV_EXCL_STOP */
#endif
	if (likely(!hp->files_used)) {
		hp->retrieval_queue = n;
		return 0;
	}
	/* LCOV_EXCL_START */
	if (unlikely (cob_write_block (hp, n))) {
		return COBSORTFILEERR;
	}
	/* LCOV_EXCL_STOP */
	for (i = 0; i < 4; ++i) {
		hp->queue[i].first = hp->empty;
		hp->empty = hp->empty->next;
		hp->queue[i].first->next = NULL;
	}
	rewind (hp->file[0].fp);
	rewind (hp->file[1].fp);
	/* LCOV_EXCL_START */
	if (unlikely (cob_get_sort_tempfile (hp, 2))) {
		return COBSORTFILEERR;
	}
	if (unlikely (cob_get_sort_tempfile (hp, 3))) {
		return COBSORTFILEERR;
	}
	/* LCOV_EXCL_STOP */
	source = 0;
	while (hp->file[source].count > 1) {
		destination = source ^ 2;
		hp->file[destination].count = 0;
		hp->file[destination + 1].count = 0;
		while (hp->file[source].count > 0) {
			/* LCOV_EXCL_START */
			if (unlikely (cob_read_item (hp, source))) {
				return COBSORTFILEERR;
			}
			/* LCOV_EXCL_STOP */
			if (hp->file[source + 1].count > 0) {
				/* LCOV_EXCL_START */
				if (unlikely (cob_read_item (hp, source + 1))) {
					return COBSORTFILEERR;
				}
				/* LCOV_EXCL_STOP */
			} else {
				hp->queue[source + 1].first->end_of_block = 1;
			}
			while (!hp->queue[source].first->end_of_block ||
			       !hp->queue[source + 1].first->end_of_block) {
				if (hp->queue[source].first->end_of_block) {
					move = 1;
				} else if (hp->queue[source + 1].first->end_of_block) {
					move = 0;
				} else {
					res = cob_file_sort_compare
						(hp->queue[source].first,
						hp->queue[source + 1].first,
						hp->pointer);
					move = res < 0 ? 0 : 1;
				}
				/* LCOV_EXCL_START */
				if (unlikely (fwrite (
				    &(hp->queue[source + move].first->block_byte),
				    hp->w_size, (size_t)1,
				    hp->file[destination].fp) != 1)) {
					return COBSORTFILEERR;
				}
				if (unlikely(cob_read_item (hp, source + move))) {
					return COBSORTFILEERR;
				}
				/* LCOV_EXCL_STOP */
			}
			hp->file[destination].count++;
			/* LCOV_EXCL_START */
			if (unlikely (putc (1, hp->file[destination].fp) != 1)) {
				return COBSORTFILEERR;
			}
			/* LCOV_EXCL_STOP */
			hp->file[source].count--;
			hp->file[source + 1].count--;
			destination ^= 1;
		}
		source = destination & 2;
		rewind (hp->file[0].fp);
		rewind (hp->file[1].fp);
		rewind (hp->file[2].fp);
		rewind (hp->file[3].fp);
	}
	hp->retrieval_queue = source;
	/* LCOV_EXCL_START */
	if (unlikely (cob_read_item (hp, source))) {
		return COBSORTFILEERR;
	}
	if (unlikely (cob_read_item (hp, source + 1))) {
		return COBSORTFILEERR;
	}
	/* LCOV_EXCL_STOP */
	return 0;
}

static int
cob_file_sort_submit (cob_file *f, const unsigned char *p)
{
	struct cobsort		*hp;
	struct cobitem		*q;
	struct queue_struct	*z;
	int			n;

	hp = f->file;
	if (unlikely (!hp)) {
		return COBSORTNOTOPEN;
	}
	if (unlikely (hp->retrieving)) {
		return COBSORTABORT;
	}
	if (unlikely (hp->switch_to_file)) {
		if (!hp->files_used) {
			/* LCOV_EXCL_START */
			if (unlikely (cob_get_sort_tempfile (hp, 0))) {
				return COBSORTFILEERR;
			}
			if (unlikely (cob_get_sort_tempfile (hp, 1))) {
				return COBSORTFILEERR;
			}
			/* LCOV_EXCL_STOP */
			hp->files_used = 1;
			hp->destination_file = 0;
		}
		n = cob_sort_queues (hp);
#if	0	/* RXWRXW - Cannot be true */
		/* LCOV_EXCL_START */
		if (unlikely (n < 0)) {
			return COBSORTABORT;
		}
		/* LCOV_EXCL_STOP */
#endif
		/* LCOV_EXCL_START */
		if (unlikely (cob_write_block (hp, n))) {
			return COBSORTFILEERR;
		}
		/* LCOV_EXCL_STOP */
		hp->destination_file ^= 1;
	}
	q = cob_new_item (hp, sizeof (struct cobitem) + hp->size);
	q->end_of_block = 1;
	unique_copy (q->unique, (const unsigned char *)&(hp->unique));
	hp->unique++;
	memcpy (q->item, p, hp->size);
	if (hp->queue[0].count <= hp->queue[1].count) {
		z = &hp->queue[0];
	} else {
		z = &hp->queue[1];
	}
	q->next = z->first;
	z->first = q;
	z->count++;
	return 0;
}

static int
cob_file_sort_retrieve (cob_file *f, unsigned char *p)
{
	struct cobsort		*hp;
	struct cobitem		*next;
	struct queue_struct	*z;
	int			move;
	int			source;
	int			res;

	hp = f->file;
	if (unlikely (!hp)) {
		return COBSORTNOTOPEN;
	}
	if (unlikely (!hp->retrieving)) {
		res = cob_file_sort_process (hp);
		if (res) {
			return res;
		}
	}
	if (unlikely (hp->files_used)) {
		source = hp->retrieval_queue;
		if (hp->queue[source].first->end_of_block) {
			if (hp->queue[source + 1].first->end_of_block) {
				return COBSORTEND;
			}
			move = 1;
		} else if (hp->queue[source + 1].first->end_of_block) {
			move = 0;
		} else {
			res = cob_file_sort_compare (hp->queue[source].first,
						hp->queue[source + 1].first,
						hp->pointer);
			move = res < 0 ? 0 : 1;
		}
		memcpy (p, hp->queue[source + move].first->item, hp->size);
		/* LCOV_EXCL_START */
		if (unlikely (cob_read_item (hp, source + move))) {
			return COBSORTFILEERR;
		}
		/* LCOV_EXCL_STOP */
	} else {
		z = &hp->queue[hp->retrieval_queue];
		if (z->first == NULL) {
			return COBSORTEND;
		}
		memcpy (p, z->first->item, hp->size);
		next = z->first->next;
		z->first->next = hp->empty;
		hp->empty = z->first;
		z->first = next;
	}
	return 0;
}

void
cob_file_sort_using (cob_file *sort_file, cob_file *data_file)
{
	int		ret;

	cob_open (data_file, COB_OPEN_INPUT, 0, NULL);
	for (;;) {
		cob_read_next (data_file, NULL, COB_READ_NEXT);
		if (data_file->file_status[0] != '0') {
			break;
		}
		cob_copy_check (sort_file, data_file);
		ret = cob_file_sort_submit (sort_file, sort_file->record->data);
		if (ret) {
			break;
		}
	}
	cob_close (data_file, NULL, COB_CLOSE_NORMAL, 0);
}

void
cob_file_sort_giving (cob_file *sort_file, const size_t varcnt, ...)
{
	cob_file	**fbase;
	struct cobsort	*hp;
	size_t		i;
	int		ret;
	int		opt;
	va_list		args;

	fbase = cob_malloc (varcnt * sizeof (cob_file *));
	va_start (args, varcnt);
	for (i = 0; i < varcnt; ++i) {
		fbase[i] = va_arg (args, cob_file *);
	}
	va_end (args);
	for (i = 0; i < varcnt; ++i) {
		cob_open (fbase[i], COB_OPEN_OUTPUT, 0, NULL);
	}
	for (;;) {
		ret = cob_file_sort_retrieve (sort_file, sort_file->record->data);
		if (ret) {
			if (ret == COBSORTEND) {
				sort_file->file_status[0] = '1';
				sort_file->file_status[1] = '0';
			} else {
				hp = sort_file->file;
				if (hp->sort_return) {
					*(int *)(hp->sort_return) = 16;
				}
				sort_file->file_status[0] = '3';
				sort_file->file_status[1] = '0';
			}
			break;
		}
		for (i = 0; i < varcnt; ++i) {
			if (COB_FILE_SPECIAL (fbase[i]) ||
			    fbase[i]->organization == COB_ORG_LINE_SEQUENTIAL) {
				opt = COB_WRITE_BEFORE | COB_WRITE_LINES | 1;
			} else {
				opt = 0;
			}
			fbase[i]->record->size = fbase[i]->record_max;
			cob_copy_check (fbase[i], sort_file);
			cob_write (fbase[i], fbase[i]->record, opt, NULL, 0);
		}
	}
	for (i = 0; i < varcnt; ++i) {
		cob_close (fbase[i], NULL, COB_CLOSE_NORMAL, 0);
	}
	cob_free (fbase);
}

void
cob_file_sort_init (cob_file *f, const unsigned int nkeys,
		    const unsigned char *collating_sequence,
		    void *sort_return, cob_field *fnstatus)
{
	struct cobsort	*p;
	size_t		n;

	p = cob_malloc (sizeof (struct cobsort));
	p->fnstatus = fnstatus;
	p->size = f->record_max;
	p->r_size = f->record_max + sizeof (size_t);
	p->w_size = f->record_max + sizeof (size_t) + 1;
	n = sizeof (struct cobitem) - offsetof (struct cobitem, item);
	if (f->record_max <= n) {
		p->alloc_size = sizeof (struct cobitem);
	} else {
		p->alloc_size = offsetof (struct cobitem, item) + f->record_max;
	}
	if (p->alloc_size % sizeof (void *)) {
		p->alloc_size += sizeof (void *) - (p->alloc_size % sizeof (void *));
	}
	p->chunk_size = cobsetptr->cob_sort_chunk;
	if (p->chunk_size % p->alloc_size) {
		p->chunk_size += p->alloc_size - (p->chunk_size % p->alloc_size);
	}
	p->pointer = f;
	if (sort_return) {
		p->sort_return = sort_return;
		*(int *)sort_return = 0;
	}
	p->mem_base = cob_fast_malloc (sizeof (struct sort_mem_struct));
	p->mem_base->mem_ptr = cob_fast_malloc (p->chunk_size);
	p->mem_base->next = NULL;
	p->mem_size = p->chunk_size;
	p->mem_total = p->chunk_size;
	f->file = p;
	f->keys = cob_malloc (sizeof (cob_file_key) * nkeys);
	f->nkeys = 0;
	if (collating_sequence) {
		f->sort_collating = collating_sequence;
	} else {
		f->sort_collating = COB_MODULE_PTR->collating_sequence;
	}
	save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
}

void
cob_file_sort_init_key (cob_file *f, cob_field *field, const int flag,
			const unsigned int offset)
{
	f->keys[f->nkeys].field = field;
	f->keys[f->nkeys].flag = flag;
	f->keys[f->nkeys].offset = offset;
	f->nkeys++;
}

void
cob_file_sort_close (cob_file *f)
{
	struct cobsort	*hp;
	cob_field	*fnstatus;
	size_t		i;

	fnstatus = NULL;
	hp = f->file;
	if (likely(hp)) {
		fnstatus = hp->fnstatus;
		cob_free_list (hp);
		for (i = 0; i < 4; ++i) {
			if (hp->file[i].fp != NULL) {
				fclose (hp->file[i].fp);
			}
		}
		cob_free (hp);
	}
	if (f->keys) {
		cob_free (f->keys);
	}
	f->file = NULL;
	save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
}

void
cob_file_release (cob_file *f)
{
	struct cobsort	*hp;
	cob_field	*fnstatus;
	int		ret;

	fnstatus = NULL;
	hp = f->file;
	if (likely(hp)) {
		fnstatus = hp->fnstatus;
	}
	ret = cob_file_sort_submit (f, f->record->data);
	if (!ret) {
		save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
		return;
	}
	if (likely(hp && hp->sort_return)) {
		*(int *)(hp->sort_return) = 16;
	}
	save_status (f, fnstatus, COB_STATUS_30_PERMANENT_ERROR);
}

void
cob_file_return (cob_file *f)
{
	struct cobsort	*hp;
	cob_field	*fnstatus;
	int		ret;

	fnstatus = NULL;
	hp = f->file;
	if (likely(hp)) {
		fnstatus = hp->fnstatus;
	}
	ret = cob_file_sort_retrieve (f, f->record->data);
	switch (ret) {
	case 0:
		save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
		return;
	case COBSORTEND:
		save_status (f, fnstatus, COB_STATUS_10_END_OF_FILE);
		return;
	}
	if (likely(hp && hp->sort_return)) {
		*(int *)(hp->sort_return) = 16;
	}
	save_status (f, fnstatus, COB_STATUS_30_PERMANENT_ERROR);
}

char *
cob_get_filename_print (cob_file* file, const int show_resolved_name)
{
	/* Obtain the file name */
	cob_field_to_string (file->assign, file_open_env, (size_t)COB_FILE_MAX);
	if (show_resolved_name) {
		strncpy (file_open_name, file_open_env, (size_t)COB_FILE_MAX);
		file_open_name[COB_FILE_MAX] = 0;
		cob_chk_file_mapping ();
	}

	if (show_resolved_name
	 && strcmp (file_open_env, file_open_name)) {
		sprintf (runtime_buffer, "%s ('%s' => %s)",
			file->select_name, file_open_env, file_open_name);
	} else {
		sprintf (runtime_buffer, "%s ('%s')",
			file->select_name, file_open_env);
	}
	return runtime_buffer;
}

/* Initialization/Termination
   cobsetpr-values with type ENV_PATH or ENV_STR
   like bdb_home and cob_file_path are taken care in cob_exit_common()!
*/

void
cob_exit_fileio_msg_only (void)
{
	struct file_list	*l;
	static int output_done = 0;

	if (output_done) {
		return;
	}
	output_done = 1;

	for (l = file_cache; l; l = l->next) {
		if (l->file
		 && l->file->open_mode != COB_OPEN_CLOSED
		 && l->file->open_mode != COB_OPEN_LOCKED
		 && !l->file->flag_nonexistent
		 && !COB_FILE_SPECIAL (l->file)) {
			cob_runtime_warning (_("implicit CLOSE of %s"),
				cob_get_filename_print (l->file, 0));
		}
	}
}

void
cob_exit_fileio (void)
{
	struct file_list	*l;
	struct file_list	*p;

	for (l = file_cache; l; l = l->next) {
		if (l->file
		 && l->file->open_mode != COB_OPEN_CLOSED
		 && l->file->open_mode != COB_OPEN_LOCKED
		 && !l->file->flag_nonexistent
		 && !COB_FILE_SPECIAL (l->file)) {
			cob_close (l->file, NULL, COB_CLOSE_NORMAL, 0);
		}
	}
#ifdef	WITH_DB
	if (bdb_env) {
		bdb_env->lock_id_free (bdb_env, bdb_lock_id);
		bdb_env->close (bdb_env, 0);
		bdb_env = NULL;
	}
	if (record_lock_object) {
		cob_free (record_lock_object);
		record_lock_object = NULL;
	}
	if (bdb_buff) {
		cob_free (bdb_buff);
		bdb_buff = NULL;
	}

#elif	defined(WITH_ANY_ISAM)
#ifndef	WITH_DISAM
	(void)iscleanup ();
#endif
#endif

#if	defined(WITH_INDEX_EXTFH) || defined(WITH_SEQRA_EXTFH)
	extfh_cob_exit_fileio ();
#endif

	if (runtime_buffer) {
		cob_free (runtime_buffer);
		runtime_buffer = NULL;
	}

	free_extfh_fcd ();

	for (l = file_cache; l;) {
		p = l;
		l = l->next;
		cob_free (p);
	}
	file_cache = NULL;
}

void
cob_init_fileio (cob_global *lptr, cob_settings *sptr)
{

	cobglobptr = lptr;
	cobsetptr  = sptr;
	file_cache = NULL;
	eop_status = 0;
	check_eop_status = 0;
	if (cobsetptr->cob_sort_chunk > (cobsetptr->cob_sort_memory / 2)) {
		cobsetptr->cob_sort_chunk = cobsetptr->cob_sort_memory / 2;
	}

	if (cobsetptr->cob_varseq_type == 3) {
		cob_vsq_len = 2;
	} else {
		cob_vsq_len = 4;
	}

	runtime_buffer = cob_fast_malloc ((size_t)(4 * COB_FILE_BUFF));
	file_open_env = runtime_buffer + COB_FILE_BUFF;
	file_open_name = runtime_buffer + (2 * COB_FILE_BUFF);
	file_open_buff = runtime_buffer + (3 * COB_FILE_BUFF);

#ifdef	WITH_DB
	bdb_env = NULL;
	bdb_data_dir = NULL;
	rlo_size = COB_SMALL_BUFF;
	record_lock_object = cob_malloc (rlo_size);
	bdb_buff = cob_malloc ((size_t)COB_SMALL_BUFF);
#endif

#ifdef VB_RTD
	if (vbisam_rtd == NULL) {	/* VB-ISAM 2.1.1 run-time pointer */
		vbisam_rtd = VB_GET_RTD;
	}
#endif

#if	defined(WITH_INDEX_EXTFH) || defined(WITH_SEQRA_EXTFH)
	extfh_cob_init_fileio (&sequential_funcs, &lineseq_funcs,
			       &relative_funcs, &cob_file_write_opt);
#endif
}

/********************************************************************************/
/* Following routines are for the External File Handler interface commonly used */
/********************************************************************************/
static struct fcd_file {
	struct fcd_file	*next;
	FCD3		*fcd;
	FCD2		*fcd2;
	cob_file	*f;
	int			sts;
	int			free_fcd;
	int			free_select;
} *fcd_file_list = NULL;
static const cob_field_attr alnum_attr = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
static const cob_field_attr compx_attr = {COB_TYPE_NUMERIC_BINARY, 0, 0, 0, NULL};

static void copy_keys_fcd_to_file (FCD3 *fcd, cob_file *f, int doall);
static int	EXTFH3 (unsigned char *opcode, FCD3 *fcd);

/*
 * Free up allocated memory
 */
static void
free_extfh_fcd (void)
{
	struct fcd_file	*ff,*nff;

	for(ff = fcd_file_list; ff; ff = nff) {
		nff = ff->next;
		if (ff->free_select) {
			cob_cache_free ((void*)ff->f->select_name);
		}
		if (ff->free_fcd) {
			if (ff->fcd->fnamePtr != NULL) {
				cob_cache_free ((void*)(ff->fcd->fnamePtr));
			}
			cob_cache_free((void*)ff->fcd);
		} else {
			cob_cache_free((void*)ff->f);
		}
		cob_cache_free((void*)ff);
	}
}

/*
 * Update FCD from cob_file
 */
static void
update_file_to_fcd (cob_file *f, FCD3 *fcd, unsigned char *fnstatus)
{
	if (fnstatus)
		memcpy (fcd->fileStatus, fnstatus, 2);
	else if (f->file_status)
		memcpy (fcd->fileStatus, f->file_status, 2);
	/* FIXME: use switch here */
	if (f->open_mode == COB_OPEN_CLOSED)
		fcd->openMode = OPEN_NOT_OPEN;
	else if( f->open_mode == COB_OPEN_INPUT)
		fcd->openMode = OPEN_INPUT;
	else if (f->open_mode == COB_OPEN_OUTPUT)
		fcd->openMode = OPEN_OUTPUT;
	else if (f->open_mode == COB_OPEN_I_O)
		fcd->openMode = OPEN_IO;
	else if (f->open_mode == COB_OPEN_EXTEND)
		fcd->openMode = OPEN_EXTEND;
	STCOMPX4(f->record_min,fcd->minRecLen);
	STCOMPX4(f->record_max, fcd->maxRecLen);
	if (f->record) {
		STCOMPX4(f->record->size, fcd->curRecLen);
	} else {
		STCOMPX4(f->record_max, fcd->curRecLen);
	}
	if (f->record_min == f->record_max)
		fcd->recordMode = REC_MODE_FIXED;
	else
		fcd->recordMode = REC_MODE_VARIABLE;
	if (f->organization == COB_ORG_SEQUENTIAL) {
		fcd->fileOrg = ORG_SEQ;
		STCOMPX2(0, fcd->refKey);
	} else if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
		fcd->fileOrg = ORG_LINE_SEQ;
		STCOMPX2(0, fcd->refKey);
#if 0 /* Note: file specific features are 4.x only ... */
		if((f->file_features & COB_FILE_LS_CRLF))
			fcd->fstatusType |= MF_FST_CRdelim;
		if((f->file_features & COB_FILE_LS_NULLS))
			fcd->fstatusType |= MF_FST_InsertNulls;
		if((f->file_features & COB_FILE_LS_FIXED))
			fcd->fstatusType |= MF_FST_NoStripSpaces;
#else /* ... so we use the current global setting as info only */
		if((cobsetptr->cob_ls_uses_cr))
			fcd->fstatusType |= MF_FST_CRdelim;
		if((cobsetptr->cob_ls_nulls))
			fcd->fstatusType |= MF_FST_InsertNulls;
		if((cobsetptr->cob_ls_fixed))
			fcd->fstatusType |= MF_FST_NoStripSpaces;
#endif
	} else if (f->organization == COB_ORG_RELATIVE) {
		fcd->fileOrg = ORG_RELATIVE;
		STCOMPX2(0, fcd->refKey);
	} else
	if (f->organization == COB_ORG_INDEXED) {
		fcd->fileOrg = ORG_INDEXED;
	}
}

/*
 * Copy 'cob_file' to FCD based information
 */
static void
copy_file_to_fcd (cob_file *f, FCD3 *fcd)
{
	/* FIXME: ACCESS_USER_STAT is originally ignored, should we store / set it here? */
	if (f->access_mode == COB_ACCESS_SEQUENTIAL)
		fcd->accessFlags = ACCESS_SEQ;
	else if (f->access_mode == COB_ACCESS_RANDOM)
		fcd->accessFlags = ACCESS_RANDOM;
	else if (f->access_mode == COB_ACCESS_DYNAMIC)
		fcd->accessFlags = ACCESS_DYNAMIC;
	if (f->flag_optional) {
		fcd->otherFlags &= ~OTH_NOT_OPTIONAL;
		fcd->otherFlags |= OTH_OPTIONAL;
	} else {
		fcd->otherFlags &= ~OTH_OPTIONAL;
		fcd->otherFlags |= OTH_NOT_OPTIONAL;
	}
	if (f->flag_select_features & COB_SELECT_EXTERNAL)
		fcd->otherFlags |= OTH_EXTERNAL;
	if (f->flag_line_adv)
		fcd->otherFlags |= OTH_LINE_ADVANCE;

	STCOMPX2(sizeof(FCD3),fcd->fcdLen);
	fcd->fcdVer = FCD_VER_64Bit;
	fcd->gcFlags |= MF_CALLFH_GNUCOBOL;
	if (f->record_min != f->record_max) {
		fcd->recordMode = REC_MODE_VARIABLE;
	} else {
		fcd->recordMode = REC_MODE_FIXED;
	}
	if (f->fcd != fcd && fcd->fnamePtr) {
		cob_cache_free ((void*)fcd->fnamePtr);
		fcd->fnamePtr = NULL;
	}
	if (fcd->fnamePtr == NULL) {
		/* when missing add cached name, "early" freed
		   on EXTFH close, otherwise on fileio teardown */
		/* CHECKME: we should likely use cob_malloc/cob_free for this */
		char	assign_to[COB_FILE_BUFF];
		size_t	fnlen;
		if (f->assign) {
			cob_field_to_string (f->assign, assign_to, COB_FILE_MAX);
		} else if (f->select_name) {
			strncpy (assign_to, f->select_name, COB_FILE_MAX);
			assign_to[COB_FILE_MAX] = 0;
		} else {
			assign_to[0] = 0;
		}
		fnlen = strlen (assign_to);
		fcd->fnamePtr = cob_cache_malloc (fnlen + 1);
		strcpy (fcd->fnamePtr, assign_to);
		STCOMPX2(fnlen, fcd->fnameLen);
	}
	fcd->openMode |= OPEN_NOT_OPEN;
	STCOMPX2(0, fcd->refKey);
	if ((f->lock_mode & COB_LOCK_EXCLUSIVE)
	 || (f->lock_mode & COB_LOCK_OPEN_EXCLUSIVE))
		fcd->lockMode = FCD_LOCK_EXCL_LOCK;
	else if(f->lock_mode == COB_LOCK_MANUAL)
		fcd->lockMode = FCD_LOCK_MANU_LOCK;
	else if(f->lock_mode == COB_LOCK_AUTOMATIC)
		fcd->lockMode = FCD_LOCK_AUTO_LOCK;
	fcd->recPtr = f->record->data;
	if (f->organization == COB_ORG_INDEXED) {
		unsigned int	kdblen,idx,keypos,keycomp,k,nkeys;
		KDB	*kdb;
		EXTKEY	*key;

		STCOMPX2(0, fcd->refKey);
		fcd->fileOrg = ORG_INDEXED;
		fcd->fileFormat = MF_FF_CISAM;
		/* Copy Key information from cob_file to FCD */
		for (idx=keycomp=0; idx < f->nkeys; idx++) {
			if (f->keys[idx].count_components <= 1) {
				keycomp++;
			} else {
				keycomp += f->keys[idx].count_components;
			}
		}
		if (fcd->kdbPtr == NULL
		 && f->nkeys > 0) {
#if 0 /* Change in r4202 - to be applied with a comment, old:
			nkeys = f->nkeys < 16 ? 16 : f->nkeys;	/ * allocate at least 16 keys CHECKME: why? */
#else /* Limit keys to 16 entries - CHECKME: why? */
			nkeys = f->nkeys < 16 ? f->nkeys : 16;
#endif
			kdblen = sizeof(KDB) - sizeof(kdb->key) + (sizeof(KDB_KEY) * nkeys) + (sizeof(EXTKEY) * keycomp);
			nkeys = f->nkeys;
			fcd->kdbPtr = kdb = cob_cache_malloc (kdblen + sizeof(EXTKEY));
			STCOMPX2(kdblen, kdb->kdbLen);
			STCOMPX2(nkeys, kdb->nkeys);
		} else if (fcd->kdbPtr == NULL) {
			nkeys = 16;
			kdblen = sizeof(KDB) - sizeof(kdb->key) + (sizeof(KDB_KEY) * nkeys) + (sizeof(EXTKEY) * keycomp);
			nkeys = 0;
			fcd->kdbPtr = kdb = cob_cache_malloc (kdblen + sizeof(EXTKEY));
			STCOMPX2(kdblen, kdb->kdbLen);
			STCOMPX2(nkeys, kdb->nkeys);
		} else {
			kdb = fcd->kdbPtr;
			nkeys = LDCOMPX2(kdb->nkeys);
			if (nkeys <= 1 && f->nkeys > 0)
				nkeys = f->nkeys;
			else if (nkeys > f->nkeys)
				nkeys = f->nkeys;
			else
				nkeys = 1;
		}
		keypos = (sizeof(KDB_KEY) * nkeys) + sizeof(KDB) - sizeof(kdb->key);
		STCOMPX2(nkeys, kdb->nkeys);
		for(idx=0; idx < nkeys; idx++) {
			key = (EXTKEY*)((char*)((char*)kdb) + keypos);
			STCOMPX2(keypos, kdb->key[idx].offset);
			kdb->key[idx].keyFlags = 0;
			kdb->key[idx].sparse = 0;
			if(f->keys[idx].tf_duplicates)
				kdb->key[idx].keyFlags |= KEY_DUPS;
			if(f->keys[idx].tf_suppress) {
				kdb->key[idx].keyFlags |= KEY_SPARSE;
				kdb->key[idx].sparse = (unsigned char)f->keys[idx].char_suppress;
			}
			if(f->keys[idx].count_components <= 1) {
				if (f->keys[idx].field == NULL)
					continue;
				STCOMPX2(1,kdb->key[idx].count);
				STCOMPX4(f->keys[idx].offset, key->pos);
				STCOMPX4(f->keys[idx].field->size, key->len);
				keypos = keypos + sizeof(EXTKEY);
			} else {
				STCOMPX2(f->keys[idx].count_components, kdb->key[idx].count);
				for(k=0; k < (int)f->keys[idx].count_components; k++) {
					key = (EXTKEY*)((char*)((char*)kdb) + keypos);
					key->desc = 0;
					key->type = 0;
					if (f->keys[idx].component[k] == NULL)
						continue;
					STCOMPX4(f->keys[idx].component[k]->data - f->record->data, key->pos);
					STCOMPX4(f->keys[idx].component[k]->size, key->len);
					keypos = keypos + sizeof(EXTKEY);
				}
			}
		}
	}
	update_file_to_fcd (f, fcd, NULL);
}

/*
 * Update 'cob_file' from 'FCD' information
 */
static void
update_fcd_to_file (FCD3* fcd, cob_file *f, cob_field *fnstatus, int wasOpen)
{
	if (wasOpen >= 0) {
		cobglobptr->cob_error_file = f;
		if (isdigit(fcd->fileStatus[0]) && fcd->fileStatus[1] != '0') {
			cob_set_exception (status_exception[(fcd->fileStatus[0] - '0')]);
		} else {
			cobglobptr->cob_exception_code = 0;
		}
		if (f->file_status) {
			memcpy (f->file_status, fcd->fileStatus, 2);
		}
		if (fnstatus) {
			memcpy (fnstatus->data, fcd->fileStatus, 2);
		}
	}
	if (wasOpen > 0) {
		if((fcd->openMode & OPEN_NOT_OPEN))
			f->open_mode = 0;
		else if((fcd->openMode&0x7f) == OPEN_INPUT)
			f->open_mode = COB_OPEN_INPUT;
		else if((fcd->openMode&0x7f) == OPEN_OUTPUT)
			f->open_mode = COB_OPEN_OUTPUT;
		else if((fcd->openMode&0x7f) == OPEN_EXTEND)
			f->open_mode = COB_OPEN_EXTEND;
		else if((fcd->openMode&0x7f) == OPEN_IO)
			f->open_mode = COB_OPEN_I_O;
	}
	f->record_min = LDCOMPX4(fcd->minRecLen);
	f->record_max = LDCOMPX4(fcd->maxRecLen);
	if (f->record == NULL) {
		f->record = cob_cache_malloc(sizeof(cob_field));
		f->record->data = fcd->recPtr;
		f->record->attr = &alnum_attr;
	}
	f->record->size = LDCOMPX4(fcd->curRecLen);
	if (f->record->size < f->record_min)
		f->record->size = f->record_min;
	else if (f->record->size > f->record_max)
		f->record->size = f->record_max;

	if (f->record->data != fcd->recPtr
	 && fcd->recPtr != NULL) {
		f->record->data = fcd->recPtr;
		if (fcd->fileOrg == ORG_INDEXED) {
			copy_keys_fcd_to_file (fcd, f, 1);
		}
	}

	if((fcd->lockMode & FCD_LOCK_EXCL_LOCK))
		f->lock_mode = COB_LOCK_EXCLUSIVE;
	else if((fcd->lockMode & FCD_LOCK_MANU_LOCK))
		f->lock_mode = COB_LOCK_MANUAL;
	else if((fcd->lockMode & FCD_LOCK_AUTO_LOCK))
		f->lock_mode = COB_LOCK_AUTOMATIC;

}

static void
copy_keys_fcd_to_file (FCD3 *fcd, cob_file *f, int doall)
{
	int		k, p, parts, parts_seen, off, klen;
	EXTKEY	*key;
	parts_seen = 0;
	for (k=0; k < (int)f->nkeys; k++) {
		if (fcd->kdbPtr->key[k].keyFlags & KEY_SPARSE) {
			f->keys[k].char_suppress = fcd->kdbPtr->key[k].sparse;
			f->keys[k].tf_suppress = 1;
		} else {
			f->keys[k].tf_suppress = 0;
		}
		if (fcd->kdbPtr->key[k].keyFlags & KEY_DUPS) {
			f->keys[k].tf_duplicates = 1;
		} else {
			f->keys[k].tf_duplicates = 0;
		}
		parts = LDCOMPX2(fcd->kdbPtr->key[k].count);
		if (parts < 1) {
			parts = 1;
		}
		f->keys[k].count_components = (short)parts;
		/* offset from KDB, calculate by number of keys
		   (expecting consecutive definition, which is _not_ guaranteed, but common) */
		off   = LDCOMPX2(fcd->kdbPtr->key[k].offset);
		if (off == 0) {
			off = sizeof(KDB_GLOBAL)
			    + sizeof(KDB_KEY) * f->nkeys
			    + sizeof(EXTKEY) * parts_seen;
		}
		parts_seen += parts;
		key   = (EXTKEY*) ((char*)(fcd->kdbPtr) + off);
		if (f->keys[k].offset == 0)
			f->keys[k].offset = LDCOMPX4(key->pos);
		if (f->keys[k].field == NULL
		 || doall
		 || f->keys[k].offset != LDCOMPX4(key->pos)
		 || (parts == 1 && f->keys[k].field->size != LDCOMPX4(key->len))) {
			if (f->keys[k].field == NULL)
				f->keys[k].field = cob_cache_malloc(sizeof(cob_field));
			if (f->record
			 && f->record->data)
				f->keys[k].field->data = f->record->data + LDCOMPX4(key->pos);
			f->keys[k].field->attr = &alnum_attr;
			f->keys[k].field->size = LDCOMPX4(key->len);
			f->keys[k].offset = LDCOMPX4(key->pos);
		}
		klen = 0;
		for (p=0; p < parts; p++) {
			if (f->keys[k].component[p] == NULL)
				f->keys[k].component[p] = cob_cache_malloc(sizeof(cob_field));
			if (f->record
			 && f->record->data)
				f->keys[k].component[p]->data = f->record->data + LDCOMPX4(key->pos);
			f->keys[k].component[p]->attr = &alnum_attr;
			f->keys[k].component[p]->size = LDCOMPX4(key->len);
			klen += LDCOMPX4(key->len);
			key   = (EXTKEY*) ((char*)(key) + sizeof(EXTKEY));
		}
		if (f->keys[k].field == NULL)
			f->keys[k].field = cob_cache_malloc(sizeof(cob_field));
		if (parts > 1
		 && f->keys[k].field != NULL) {
			if (f->keys[k].field->data == NULL
			 || f->keys[k].field->size != klen) {
				f->keys[k].field->data = cob_cache_malloc ((size_t)klen);
				f->keys[k].field->size = klen;
			}
		}
	}
}

/*
 * Copy 'FCD' to 'cob_file' based information
 */
static void
copy_fcd_to_file (FCD3* fcd, cob_file *f, struct fcd_file *fcd_list_entry)
{
	int	k, min, max;

	if((fcd->accessFlags & 0x7F) == ACCESS_SEQ)
		f->access_mode = COB_ACCESS_SEQUENTIAL;
	else if((fcd->accessFlags & 0x7F) == ACCESS_RANDOM)
		f->access_mode = COB_ACCESS_RANDOM;
	else if((fcd->accessFlags & 0x7F) == ACCESS_DYNAMIC)
		f->access_mode = COB_ACCESS_DYNAMIC;
	if((fcd->otherFlags & OTH_EXTERNAL))
		f->flag_select_features |= COB_SELECT_EXTERNAL;
	f->flag_optional = 0;
	if((fcd->otherFlags & OTH_OPTIONAL))
		f->flag_optional = 1;
	if((fcd->otherFlags & OTH_NOT_OPTIONAL))
		f->flag_optional = 0;
	if((fcd->otherFlags & OTH_LINE_ADVANCE))
		f->flag_line_adv = 1;
	else
		f->flag_line_adv = 0;

#if 0 /* FIXME */
	if (fcd->recordMode == REC_MODE_FIXED) {
		/* TODO */
	} else {
		/* TODO */
	}
#endif

	if(fcd->fileOrg == ORG_INDEXED) {
		f->organization = COB_ORG_INDEXED;
	} else if(fcd->fileOrg == ORG_SEQ) {
		f->organization = COB_ORG_SEQUENTIAL;
	} else if(fcd->fileOrg == ORG_LINE_SEQ) {
		f->organization = COB_ORG_LINE_SEQUENTIAL;
#if 0 /* note file_features are a 4.x-only feature */
#ifdef	_WIN32
		if (file_setptr->cob_unix_lf &&
		  !(fcd->fstatusType & MF_FST_CRdelim)) {
			f->file_features |= COB_FILE_LS_LF;
		} else {
			f->file_features |= COB_FILE_LS_CRLF;
		}
#else
		if ((fcd->fstatusType & MF_FST_CRdelim)) {
			f->file_features |= COB_FILE_LS_CRLF;
		} else {
			f->file_features |= COB_FILE_LS_LF;
		}
#endif
		if((fcd->fstatusType & MF_FST_InsertNulls))
			f->file_features |= COB_FILE_LS_NULLS;
		if((fcd->fstatusType & MF_FST_NoStripSpaces))
			f->file_features |= COB_FILE_LS_FIXED;
#endif
	} else if(fcd->fileOrg == ORG_RELATIVE) {
		f->organization = COB_ORG_RELATIVE;
		if (f->keys == NULL)
			f->keys = cob_cache_malloc(sizeof(cob_file_key));
		if (f->keys[0].field == NULL)
			f->keys[0].field = cob_cache_malloc(sizeof(cob_field));
		f->keys[0].field->data = cob_cache_malloc (4);
		f->keys[0].field->attr = &compx_attr;
		f->keys[0].field->size = 4;
	} else {
		f->organization = COB_ORG_MAX;
	}

	/* Try for some record size */
	min = LDCOMPX4(fcd->minRecLen);
	if (min < 0) {
		min = 0;
		STCOMPX4 (min, fcd->minRecLen);
	}
	k   = LDCOMPX4(fcd->curRecLen);
	if (k < min) {
		k = min;
		STCOMPX4 (k, fcd->curRecLen);
	}
	max = LDCOMPX4(fcd->maxRecLen);
	if (max < k) {
		max = k;
		STCOMPX4 (max, fcd->maxRecLen);
	}
	f->record_min = min;
	f->record_max = max;
	/* Allocate cob_file fields as needed and copy from FCD */
	if (f->record == NULL
	 && fcd->recPtr != NULL
	 && k > 0) {
		f->record = cob_cache_malloc(sizeof(cob_field));
		f->record->data = fcd->recPtr;
		f->record->size = k;
		f->record->attr = &alnum_attr;
	}
#if 1 /* CHECKME: not in trunk - not needed ? */
	if (f->file_status == NULL) {
		f->file_status = cob_cache_malloc (2);
	}
#endif

	/* CHECKME: possibly only check/adjust assign + select at OPEN/GET-INFO */

	if (f->assign == NULL
	 && fcd->fnamePtr != NULL) {
		f->assign = cob_cache_malloc(sizeof(cob_field));
		f->assign->data = (unsigned char*)fcd->fnamePtr;
		f->assign->size = LDCOMPX2(fcd->fnameLen);
		f->assign->attr = &alnum_attr;
	}

	/* build select name from assign value, if missing */
	if (f->select_name == NULL
	 && f->assign != NULL) {
		char	fdname[49];
		char	*origin = (char*)f->assign->data;
		/* limit filename to last element after
		   path separator, when specified */
		for (k=(int)f->assign->size; k; k--) {
			if (f->assign->data[k] == SLASH_CHAR
#ifdef	_WIN32
			 || f->assign->data[k] == '/'
#endif
			) {
				origin = (char*)&f->assign->data[k+1];
				break;
			}
		}
		/* now copy that until the first space/low-value (max 48) as upper-case */
		for (k=0; origin[k] && origin[k] > ' ' && k < 48; k++) {
			fdname[k] = (char)toupper((int)origin[k]);
		}
		fdname[k] = 0;
		/* we don't necessarily know later if we built the name ourself,
		   so we need to cache the storage */
		f->select_name = cob_cache_malloc (k);
		memcpy ((void *)f->select_name, fdname, k);
		if (fcd_list_entry) {
			fcd_list_entry->free_select = 1;
		}
	}

	if (f->organization == COB_ORG_INDEXED) {
		if (f->keys == NULL) {
			if (fcd->kdbPtr != NULL
			 && LDCOMPX2(fcd->kdbPtr->nkeys) > 0) {
				/* Copy Key information from FCD to cob_file,
				   CHECKME: possibly only for ORG_DETERMINE + OP-DELETE-FILE ? */
				f->nkeys = LDCOMPX2(fcd->kdbPtr->nkeys);
				if (f->nkeys > MAX_FILE_KEYS) {
					/* CHECKME - Should this result in any error handling? */
					cob_runtime_warning (_("maximum keys (%d/%d) exceeded for file '%s'"),
						(int)f->nkeys, MAX_FILE_KEYS, cob_get_filename_print (f->file, 0));
					f->nkeys = MAX_FILE_KEYS;
				}
				f->keys = cob_cache_malloc (sizeof(cob_file_key) * f->nkeys);
				copy_keys_fcd_to_file (fcd, f, 0);
			} else {
				f->keys = cob_cache_malloc(sizeof(cob_file_key));
			}
		} else if (f->nkeys > 0
		        && fcd->kdbPtr != NULL
		        && LDCOMPX2(fcd->kdbPtr->nkeys) >= (int)f->nkeys) {
			copy_keys_fcd_to_file (fcd, f, 0);
		}
	}
	update_fcd_to_file (fcd, f, NULL, 0);
}

/*
 * Construct FCD based on information from 'cob_file'
 */
static FCD3 *
find_fcd (cob_file *f)
{
	FCD3	*fcd;
	struct fcd_file	*ff;
	for (ff = fcd_file_list; ff; ff=ff->next) {
		if (ff->f == f) {
			return ff->fcd;
		}
	}
	fcd = cob_cache_malloc (sizeof(FCD3));
	copy_file_to_fcd (f, fcd);
	ff = cob_cache_malloc (sizeof(struct fcd_file));
	ff->next = fcd_file_list;
	ff->fcd = fcd;
	ff->f = f;
	ff->free_fcd = 1;
	fcd_file_list = ff;
	return fcd;
}


#if !COB_64_BIT_POINTER
/*
 * Construct FCD based on information from 'FCD2'
 */
static FCD3 *
find_fcd2 (FCD2 *fcd2)
{
	FCD3	*fcd;
	struct fcd_file	*ff;
	for (ff = fcd_file_list; ff; ff=ff->next) {
		if (ff->fcd2 == fcd2) {
			return ff->fcd;
		}
	}
	fcd = cob_cache_malloc (sizeof(FCD3));
	fcd->fcdVer = FCD_VER_64Bit;
	STCOMPX2 (sizeof (FCD3), fcd->fcdLen);
	ff = cob_cache_malloc (sizeof(struct fcd_file));
	ff->next = fcd_file_list;
	ff->fcd = fcd;
	ff->fcd2 = fcd2;
	ff->free_fcd = 2;
	fcd_file_list = ff;
	return fcd;
}

/*
 * Free FCD from 'FCD2'
 */
static void
free_fcd2 (FCD2 *fcd2)
{
	struct fcd_file	*ff, *pff;
	pff = NULL;
	for (ff = fcd_file_list; ff; ff=ff->next) {
		if (ff->fcd2 == fcd2) {
			if (pff) {
				pff->next = ff->next;
			} else {
				fcd_file_list = ff->next;
			}
			if (ff->fcd) {
				cob_cache_free ((void*)ff->fcd);
			}
			if (ff->f) {
				cob_cache_free ((void*)ff->f);
			}
			cob_cache_free ((void*)ff);
			return;
		}
		pff = ff;
	}
	return;
}

/* Convert FCD2 into FCD3 format, note: explicit no checks here
   as those have to be in EXTFH3 / fileio later */
static FCD3 *
fcd2_to_fcd3 (FCD2 *fcd2)
{
	FCD3 *fcd = find_fcd2 (fcd2);
	memcpy (fcd->fileStatus, fcd2->fileStatus, 2);
	fcd->fileOrg = fcd2->fileOrg;
	fcd->accessFlags = fcd2->accessFlags;
	fcd->openMode = fcd2->openMode;
	fcd->recordMode = fcd2->recordMode;
	fcd->fileFormat = fcd2->fileFormat;
	fcd->lockMode = fcd2->lockMode;
	fcd->otherFlags = fcd2->otherFlags;
	fcd->fstatusType = fcd2->fstatusType;
	fcd->compType = fcd2->compType;
	fcd->blockSize = fcd2->blockSize;
	fcd->gcFlags = fcd2->gcFlags;
	fcd->gcFlags |= MF_CALLFH_GNUCOBOL;
	fcd->lockMode = fcd2->lockMode;
	fcd->fsv2Flags = fcd2->fsv2Flags;
	fcd->confFlags = fcd2->confFlags;
	fcd->confFlags2 = fcd2->confFlags2;
	fcd->idxCacheSz = fcd2->idxCacheSz;
	fcd->idxCacheArea = fcd2->idxCacheArea;
	STCOMPX4 (LDCOMPX2 (fcd2->curRecLen), fcd->curRecLen);
	STCOMPX4 (LDCOMPX2 (fcd2->minRecLen), fcd->minRecLen);
	STCOMPX4 (LDCOMPX2 (fcd2->maxRecLen), fcd->maxRecLen);
	if (fcd->fileOrg == ORG_INDEXED) {
		memset (fcd->lineCount, 0, 2);
		memcpy (fcd->refKey, fcd2->refKey, 2);
	} else {
		memcpy (fcd->lineCount, fcd2->refKey, 2);
		memset (fcd->refKey, 0, 2);
	}
	memcpy (fcd->effKeyLen, fcd2->effKeyLen, 2);
	memcpy (fcd->fnameLen, fcd2->fnameLen, 2);
	memcpy (fcd->relByteAdrs, fcd2->relByteAdrs64, 8);
	fcd->fileHandle = fcd2->fileHandle2;
	fcd->recPtr = fcd2->recPtr2;
	fcd->fnamePtr = fcd2->fnamePtr2;
	fcd->kdbPtr = fcd2->kdbPtr2;
	return fcd;
}

/* Convert FCD3 to FCD2 format */
static void
fcd3_to_fcd2 (FCD3 *fcd, FCD2 *fcd2)
{
#if 0 /* Should be already set externally */
	fcd2->fcdVer = FCD2_VER;
	STCOMPX2 (sizeof(FCD2), fcd2->fcdLen);
#endif
	memcpy (fcd2->fileStatus, fcd->fileStatus, 2);
	fcd2->fileOrg = fcd->fileOrg;
	fcd2->accessFlags = fcd->accessFlags;
	fcd2->openMode = fcd->openMode;
	fcd2->recordMode = fcd->recordMode;
	fcd2->fileFormat = fcd->fileFormat;
	fcd2->lockMode = fcd->lockMode;
	fcd2->otherFlags = fcd->otherFlags;
	fcd2->fstatusType = fcd->fstatusType;
	fcd2->compType = fcd->compType;
	fcd2->blockSize = fcd->blockSize;
	fcd2->gcFlags = fcd->gcFlags;
	fcd2->fsv2Flags = fcd->fsv2Flags;
	fcd2->confFlags = fcd->confFlags;
	fcd2->confFlags2 = fcd->confFlags2;
	fcd2->idxCacheSz = fcd->idxCacheSz;
	fcd2->idxCacheArea = fcd->idxCacheArea;
	STCOMPX2 (LDCOMPX4 (fcd->curRecLen), fcd2->curRecLen);
	STCOMPX2 (LDCOMPX4 (fcd->minRecLen), fcd2->minRecLen);
	STCOMPX2 (LDCOMPX4 (fcd->maxRecLen), fcd2->maxRecLen);
	if (fcd->fileOrg == ORG_INDEXED) {
		memcpy (fcd2->refKey, fcd->refKey, 2);
	} else {
		memcpy (fcd2->refKey, fcd->lineCount, 2);
	}
	memcpy (fcd2->effKeyLen, fcd->effKeyLen, 2);
	memcpy (fcd2->fnameLen, fcd->fnameLen, 2);
	memcpy (fcd2->relByteAdrs64, fcd->relByteAdrs, 8);
	fcd2->fileHandle2 = fcd->fileHandle;
	fcd2->recPtr2 = fcd->recPtr;
	fcd2->fnamePtr2 = fcd->fnamePtr;
	fcd2->kdbPtr2 = fcd->kdbPtr;
}
#endif

/*
 * Construct cob_file based on information from 'FCD'
 */
static cob_file *
find_file (FCD3 *fcd)
{
	cob_file	*f;
	struct fcd_file	*ff;
	for (ff = fcd_file_list; ff; ff=ff->next) {
		if (ff->fcd == fcd) {
			f = ff->f;
			if (f == NULL) {
				/* entry in fcd_file_list found, but has no cob_file, create below */
				break;
			}
			/* entry in fcd_file_list found with cob_file, all done */
			return f;
		}
	}
	/* create cob_file */
	f = cob_cache_malloc (sizeof(cob_file));
	f->file_version = COB_FILE_VERSION;
	f->open_mode = COB_OPEN_CLOSED;
	f->fcd = fcd;
	/* attach it to our fcd_file_list, if not found above create a new one */
	if (!ff) {
		ff = cob_cache_malloc (sizeof (struct fcd_file));
		ff->free_fcd = 0;
		ff->next = fcd_file_list;
		ff->fcd = fcd;
		fcd_file_list = ff;
	}
	ff->f = f;
	copy_fcd_to_file (fcd, f, ff);

	return f;
}

/*
 * If record area not allocated then allocate it
 * If record address has changed then adjust accordingly
 *                       including the INDEXED keys
 */
static void
update_record_and_keys_if_necessary (cob_file * f, FCD3 *fcd)
{
	if (f->record == NULL) {
		/* that's actually an error, it seems */
		return;
	}
	if (f->record->data != fcd->recPtr
	 || f->record->data == NULL) {
		if (fcd->recPtr != NULL) {
			f->record->data = fcd->recPtr;
		} else {
			f->record->data = cob_cache_malloc (f->record_max + 1);
			/* CHECKME: is that somewhere freed? */
		}
		f->record->size = LDCOMPX4(fcd->curRecLen);
		f->record->attr = &alnum_attr;
		f->record_min = LDCOMPX4(fcd->minRecLen);
		f->record_max = LDCOMPX4(fcd->maxRecLen);
		if (f->record->size < f->record_min)
			f->record->size = f->record_min;
		if (f->record->size > f->record_max)
			f->record->size = f->record_max;
		if (fcd->fileOrg == ORG_INDEXED) {
			copy_keys_fcd_to_file (fcd, f, 1);
		}
	}
}

static void
save_fcd_status (FCD3 *fcd, int sts)
{
	struct fcd_file	*ff;
	for (ff = fcd_file_list; ff; ff=ff->next) {
		if (ff->fcd == fcd) {
			ff->sts = sts;
			return;
		}
	}
}

/*
 * NOTES: It would be best if 'cob_file' had a pointer to the full/complete file name
 *        ISAM & BDB already keep this in a separate structure
 *        The filename should be passed via EXTFH interface
 */

/*
 * OPEN file
 */
void
cob_extfh_open (
	int (*callfh)(unsigned char *opcode, FCD3 *fcd),
	cob_file *f, const int mode, const int sharing, cob_field *fnstatus)
{
	unsigned char opcode[2];
	FCD3	*fcd;
	int	sts;

	COB_UNUSED (sharing);

	fcd = find_fcd(f);
	f->last_open_mode = mode;
	if (mode == COB_OPEN_OUTPUT)
		STCOMPX2(OP_OPEN_OUTPUT, opcode);
	else if (mode == COB_OPEN_I_O)
		STCOMPX2(OP_OPEN_IO, opcode);
	else if (mode == COB_OPEN_EXTEND)
		STCOMPX2(OP_OPEN_EXTEND, opcode);
	else
		STCOMPX2(OP_OPEN_INPUT, opcode);

	/* Keep table of 'fcd' created */
	sts = callfh (opcode, fcd);
	if (f->file_status) {
		if (memcmp(f->file_status,"00",2) == 0
		 || memcmp(f->file_status,"05",2) == 0) {
			fcd->openMode &= ~OPEN_NOT_OPEN;
		}
	} else {
		fcd->openMode &= ~OPEN_NOT_OPEN;
	}
	update_fcd_to_file (fcd, f, fnstatus, 1);
	save_fcd_status (fcd, sts);
}

/*
 * CLOSE file
 */
void
cob_extfh_close (
	int (*callfh)(unsigned char *opcode, FCD3 *fcd),
	cob_file *f, cob_field *fnstatus, const int opt, const int remfil)
{
	unsigned char opcode[2];
	FCD3	*fcd;
	struct fcd_file	*ff,*pff;

	COB_UNUSED (remfil);

	fcd = find_fcd(f);
	STCOMPX4(opt, fcd->opt);
	STCOMPX2(OP_CLOSE, opcode);

	/* Keep table of 'fcd' created */
	(void)callfh (opcode, fcd);
	update_fcd_to_file (fcd, f, fnstatus, 0);

	pff = NULL;
	for (ff = fcd_file_list; ff; ff=ff->next) {
		if (ff->fcd == fcd) {
			if (pff)
				pff->next = ff->next;
			else
				fcd_file_list = ff->next;
			if (ff->free_fcd) {
				if (ff->fcd->fnamePtr != NULL)
					cob_cache_free ((void*)(ff->fcd->fnamePtr));
				cob_cache_free((void*)ff->fcd);
			} else {
				cob_cache_free((void*)ff->f);
			}
			cob_cache_free((void*)ff);
			break;
		}
		pff = ff;
	}
}

/*
 * START
 */
void
cob_extfh_start (
	int (*callfh)(unsigned char *opcode, FCD3 *fcd),
	cob_file *f, const int cond, cob_field *key, cob_field *keysize, cob_field *fnstatus)
{
	unsigned char opcode[2];
	FCD3	*fcd;
	int	recn;
	int	keyn,keylen,partlen;

	fcd = find_fcd(f);
	if (f->organization == COB_ORG_INDEXED) {
		keyn = cob_findkey(f,key,&keylen,&partlen);
		STCOMPX2(keyn, fcd->refKey);
		if (keysize)
			partlen = cob_get_int (keysize);
		STCOMPX2(partlen, fcd->effKeyLen);
		STCOMPX2(keyn, fcd->refKey);
		STCOMPX2(OP_READ_RAN, opcode);
	} else if(f->organization == COB_ORG_RELATIVE) {
		memset(fcd->relKey,0,sizeof(fcd->relKey));
		recn = cob_get_int(f->keys[0].field);
		STCOMPX4(recn, LSUCHAR(fcd->relKey+4));
		STCOMPX2(OP_READ_RAN, opcode);
	}

	switch(cond) {
	case COB_EQ:	STCOMPX2(OP_START_EQ, opcode); break;
	case COB_GE:	STCOMPX2(OP_START_GE, opcode); break;
	case COB_LE:	STCOMPX2(OP_START_LE, opcode); break;
	case COB_GT:	STCOMPX2(OP_START_GT, opcode); break;
	case COB_LT:	STCOMPX2(OP_START_LT, opcode); break;
	case COB_FI:	STCOMPX2(OP_START_FI, opcode); break;
	case COB_LA:	STCOMPX2(OP_START_LA, opcode); break;
	default:
			STCOMPX2(OP_START_EQ_ANY, opcode); break;
	}

	(void)callfh (opcode, fcd);
	update_fcd_to_file (fcd, f, fnstatus, 0);
}

/*
 * READ
 */
void
cob_extfh_read (
	int (*callfh)(unsigned char *opcode, FCD3 *fcd),
	cob_file *f, cob_field *key, cob_field *fnstatus, const int read_opts)
{
	unsigned char opcode[2];
	FCD3	*fcd;
	int	recn;
	int	keyn,keylen,partlen;

	fcd = find_fcd(f);
	STCOMPX4 (read_opts, fcd->opt);
	if(key == NULL) {
		if((read_opts & COB_READ_PREVIOUS)) {
			STCOMPX2(OP_READ_PREV, opcode);
		} else {
			STCOMPX2(OP_READ_SEQ, opcode);
		}
		if(f->organization == COB_ORG_RELATIVE) {
			memset (fcd->relKey, 0, sizeof(fcd->relKey));
			recn = cob_get_int (f->keys[0].field);
			STCOMPX4(recn, LSUCHAR(fcd->relKey+4));
			if (f->access_mode != COB_ACCESS_SEQUENTIAL)
				STCOMPX2(OP_READ_RAN, opcode);
		}
	} else if(f->organization == COB_ORG_INDEXED) {
		keyn = cob_findkey (f, key, &keylen, &partlen);
		STCOMPX2(keyn, fcd->refKey);
		STCOMPX2(keylen, fcd->effKeyLen);
		STCOMPX2(OP_READ_RAN, opcode);
	} else if(f->organization == COB_ORG_RELATIVE) {
		memset (fcd->relKey, 0, sizeof(fcd->relKey));
		recn = cob_get_int (key);
		STCOMPX4(recn, LSUCHAR(fcd->relKey+4));
		STCOMPX2(OP_READ_RAN, opcode);
	} else {
		STCOMPX2(OP_READ_SEQ, opcode);
	}

	(void)callfh (opcode, fcd);
	update_fcd_to_file (fcd, f, fnstatus, 0);
}

/*
 * READ next
 */
void
cob_extfh_read_next (
	int (*callfh)(unsigned char *opcode, FCD3 *fcd),
	cob_file *f, cob_field *fnstatus, const int read_opts)
{
	unsigned char opcode[2];
	FCD3	*fcd;
	int	recn;

	fcd = find_fcd(f);
	STCOMPX4(read_opts, fcd->opt);
	if((read_opts & COB_READ_PREVIOUS)) {
		STCOMPX2(OP_READ_PREV, opcode);
	} else {
		STCOMPX2(OP_READ_SEQ, opcode);
	}
	if(f->organization == COB_ORG_RELATIVE) {
		memset (fcd->relKey, 0, sizeof(fcd->relKey));
		recn = cob_get_int (f->keys[0].field);
		STCOMPX4(recn, LSUCHAR(fcd->relKey+4));
	}

	(void)callfh (opcode, fcd);
	update_fcd_to_file (fcd, f, fnstatus, 0);
}
/*
 * WRITE
 */
void
cob_extfh_write (
	int (*callfh)(unsigned char *opcode, FCD3 *fcd),
	cob_file *f, cob_field *rec, const int opt, cob_field *fnstatus, const unsigned int check_eop)
{
	unsigned char opcode[2];
	FCD3	*fcd;
	int	recn;

	fcd = find_fcd(f);
	STCOMPX2(OP_WRITE, opcode);
	STCOMPX2(check_eop, fcd->eop);
	STCOMPX4(opt, fcd->opt);
	if (f->variable_record) {
		f->record->size = (size_t)cob_get_int (f->variable_record);
		if (unlikely(f->record->size > rec->size)) {
			f->record->size = rec->size;
		}
	} else {
		f->record->size = rec->size;
	}
	STCOMPX4(f->record->size,fcd->curRecLen);
	fcd->recPtr = rec->data;
	if (f->organization == COB_ORG_RELATIVE) {
		memset (fcd->relKey, 0, sizeof(fcd->relKey));
		recn = cob_get_int (f->keys[0].field);
		STCOMPX4(recn, LSUCHAR(fcd->relKey+4));
	}

	(void)callfh (opcode, fcd);
	update_fcd_to_file (fcd, f, fnstatus, 0);
}

/*
 * REWRITE
 */
void
cob_extfh_rewrite (
	int (*callfh)(unsigned char *opcode, FCD3 *fcd),
	cob_file *f, cob_field *rec, const int opt, cob_field *fnstatus)
{
	unsigned char opcode[2];
	FCD3	*fcd;
	int	recn;

	fcd = find_fcd(f);
	STCOMPX2 (OP_REWRITE, opcode);
	STCOMPX4 (rec->size, fcd->curRecLen);
	STCOMPX4 (opt, fcd->opt);
	fcd->recPtr = rec->data;
	if (f->organization == COB_ORG_RELATIVE) {
		memset (fcd->relKey ,0, sizeof(fcd->relKey));
		recn = cob_get_int (f->keys[0].field);
		STCOMPX4 (recn, LSUCHAR (fcd->relKey + 4));
	}
	STCOMPX4(rec->size,fcd->curRecLen);
	fcd->recPtr = rec->data;
	if(f->organization == COB_ORG_RELATIVE) {
		memset (fcd->relKey, 0, sizeof(fcd->relKey));
		recn = cob_get_int (f->keys[0].field);
		STCOMPX4(recn, LSUCHAR(fcd->relKey+4));
	}

	(void)callfh (opcode, fcd);
	update_fcd_to_file (fcd, f, fnstatus, 0);
}

/*
 * DELETE
 */
void
cob_extfh_delete (
	int (*callfh)(unsigned char *opcode, FCD3 *fcd),
	cob_file *f, cob_field *fnstatus)
{
	unsigned char opcode[2];
	FCD3	*fcd;
	int	recn;

	fcd = find_fcd (f);
	STCOMPX2 (OP_DELETE, opcode);
	if (f->organization == COB_ORG_RELATIVE) {
		memset (fcd->relKey, 0, sizeof(fcd->relKey));
		recn = cob_get_int (f->keys[0].field);
		STCOMPX4 (recn, LSUCHAR(fcd->relKey + 4));
	}
	if (f->organization == COB_ORG_RELATIVE) {
		memset (fcd->relKey, 0, sizeof(fcd->relKey));
		recn = cob_get_int (f->keys[0].field);
		STCOMPX4(recn, LSUCHAR(fcd->relKey+4));
	}

	(void)callfh (opcode, fcd);
	update_fcd_to_file (fcd, f, fnstatus, 0);
}

/* COBOL wrapper for EXTFH call to prevent warnings about FCD3 structure
   with additional checks */
int
cob_sys_extfh (const void *opcode_ptr, void *fcd_ptr)
{
	FCD3 *fcd = (FCD3 *) fcd_ptr;

	COB_CHK_PARMS (EXTFH, 2);

	if (cobglobptr->cob_call_params < 2
	 || !COB_MODULE_PTR->cob_procedure_params[0]
	 || !COB_MODULE_PTR->cob_procedure_params[1]
	 || COB_MODULE_PTR->cob_procedure_params[1]->size < 2) {
		cob_set_exception (COB_EC_PROGRAM_ARG_MISMATCH);
		return 1;	/* correct? */
	}
	if (COB_MODULE_PTR->cob_procedure_params[0]->size < 2
	 || COB_MODULE_PTR->cob_procedure_params[1]->size < 5) {
		fcd->fileStatus[0] = '9';
		fcd->fileStatus[1] = 161;
		cob_set_exception (COB_EC_PROGRAM_ARG_MISMATCH);
		return 1;	/* correct? */
	}

#if !COB_64_BIT_POINTER	/* Only FCD3 is accepted for 64 bit, otherwise we can convert */
	if (COB_MODULE_PTR->cob_procedure_params[1]->size >= sizeof(FCD2)
	 && fcd->fcdVer == FCD2_VER) {
		/* EXTFH with auto-conversion FCD2<->FCD3*/
		int retsts = EXTFH ((unsigned char *)opcode_ptr, fcd);
		if (retsts) {
			cob_set_exception (COB_EC_PROGRAM_ARG_MISMATCH);
		}
		return retsts;
	}
#endif

	if (COB_MODULE_PTR->cob_procedure_params[1]->size < sizeof(FCD3)
	 || fcd->fcdVer != FCD_VER_64Bit) {
		fcd->fileStatus[0] = '9';
		fcd->fileStatus[1] = 161;
		cob_set_exception (COB_EC_PROGRAM_ARG_MISMATCH);
		if (fcd->fcdVer != FCD_VER_64Bit) {
			cob_runtime_warning (_("ERROR: EXTFH called with FCD version %d"), fcd->fcdVer);
		}
		return 1;	/* correct? */
	}
	return EXTFH3 ((unsigned char *)opcode_ptr, fcd);
}

/* 
 * Sync FCD3 values to cob_file values
 */
static void
cob_fcd_file_sync (cob_file *f, char *external_file_open_name)
{
	COB_UNUSED (external_file_open_name);
	copy_fcd_to_file (f->fcd, f, NULL);
}

/* 
 * Sync cob_file values to FCD3 values
 */
static void
cob_file_fcd_sync (cob_file *f)
{
	if (last_operation_open == 1) {
		last_operation_open = 0;
		copy_file_to_fcd (f, f->fcd);
	} else {
		update_file_to_fcd (f, f->fcd, NULL);
	}
	return;
}
/* 
 * Return address of FH--FCD for the given file
 * Create the FCD3 as needed
 */
void
cob_file_fcd_adrs (cob_file *f, void *pfcd)
{
	FCD3	*fcd = NULL;
	/* LCOV_EXCL_START */
	if (f == NULL) {
		cob_runtime_error (_("invalid internal call of %s"), "cob_file_fcd_adrs");
		cob_hard_failure_internal ();
	}
	/* LCOV_EXCL_STOP */
	if (f->fcd == NULL) {
		f->fcd = find_fcd (f);
	}
	fcd = f->fcd;
	if (fcd->openMode == OPEN_NOT_OPEN) {
		cob_pre_open (f);
	}
	if (fcd->kdbPtr == NULL) {
		copy_file_to_fcd (f, fcd);
	}
	memcpy (pfcd, &f->fcd, sizeof(void *));
	return;
}

/* 
 * Return address of FH--KEYDEF for the given file
 * Create the FCD3 is needed
 */
void
cob_file_fcdkey_adrs (cob_file *f, void *pkey)
{
	FCD3	*fcd = NULL;
	/* LCOV_EXCL_START */
	if (f == NULL) {
		cob_runtime_error (_("invalid internal call of %s"), "cob_file_fcdkey_adrs");
		cob_hard_failure_internal ();
	}
	/* LCOV_EXCL_STOP */
	cob_file_fcd_adrs (f, &fcd);
	memcpy (pkey, &f->fcd->kdbPtr, sizeof(void *));
	return;
}

/*
 * EXTFH: maybe called by user own 'callfh' routine
 *        to call normal fileio routine in fileio.c
 */
int
EXTFH (unsigned char *opcode, FCD3 *fcd)
{
	if (opcode == NULL || fcd == NULL) {
		cob_runtime_warning (_("call to '%s' with invalid parameter '%s'"),
			"EXTFH", opcode == NULL ? "opcode" : "fcd");
		return -1;
	}
#if !COB_64_BIT_POINTER
	if (fcd->fcdVer == FCD2_VER) {
		int		rtnsts;
		FCD2 *fcd2 = (FCD2 *) fcd;

		fcd = fcd2_to_fcd3 (fcd2);
		if (fcd == NULL) {
			cob_runtime_warning (_("call to '%s' with invalid parameter '%s'"),
				"EXTFH", "fcd");
			return -1;
		}
		rtnsts = EXTFH3 (opcode, fcd);
		/* Convert FCD3 back to FCD2 format */
		fcd3_to_fcd2 (fcd, fcd2);

		if (opcode[1] == OP_CLOSE) {
			free_fcd2 (fcd2);
		}

		return rtnsts;
	}
#endif
	return EXTFH3 (opcode, fcd);
}

/*
 * EXTFH: internal routine
 */
static int
EXTFH3 (unsigned char *opcode, FCD3 *fcd)
{
	/* TODO check CODE-SET conversion for lsq/seq read/write */

	int	opcd,sts,opts,eop,k;
	unsigned char	fnstatus[2];	/* storage for local file status field */
	unsigned char	keywrk[80];
	/* different cob_fields as some ABI functions operate on those */
	cob_field fs[1];
	cob_field key[1];
	cob_field rec[1];
	cob_file *f;

	if (fcd->fcdVer != FCD_VER_64Bit) {
		fcd->fileStatus[0] = '9';
		fcd->fileStatus[1] = 161;
		cob_runtime_warning (_("ERROR: EXTFH called with FCD version %d"), fcd->fcdVer);
		return 1;
	}
	sts = opts = 0;
	/* create a local file status field as different ABI functions expect it */
	fs->data = fnstatus;
	fs->size = sizeof(fnstatus);
	fs->attr = &alnum_attr;
#if 0	/* why? */
	memcpy (fnstatus, "00", 2);
	memcpy (fcd->fileStatus, "00", 2);
#endif

	if (cobglobptr == NULL) {	/* Auto Init GnuCOBOL runtime */
		cob_init (0, NULL);
		/* COB_MODULE_PTR (part of cobglobptr structure) was not set,
		   add to allow tracing and to get better messages on fileio errors */
		COB_MODULE_PTR = cob_malloc( sizeof(cob_module) );
		COB_MODULE_PTR->module_name = "GnuCOBOL-fileio";
		COB_MODULE_PTR->module_source = "GnuCOBOL-fileio";
		COB_MODULE_PTR->module_formatted_date = "2021/09/21 12:01:20";
	}

	if (*opcode == 0xFA) {
		opcd = 0xFA00 + opcode[1];
	} else {
		opcd = opcode[1];
	}

	/* Look for fcd in table and if found use associated 'cob_file' after copying values over */
	/* If fcd is not found, then 'callfh' created it, so create a new 'cob_file' and table that */
	f = find_file (fcd);

	update_record_and_keys_if_necessary (f, fcd);

org_handling:
	switch (fcd->fileOrg) {
	case ORG_INDEXED:
		k = LDCOMPX2(fcd->refKey);
		if (k >= 0 
		 && k <= (int)f->nkeys
		 && f->keys[k].field) {
			key->size = f->keys[k].field->size;
			key->attr = f->keys[k].field->attr;
			if (f->keys[k].count_components <= 1) {
				key->data = f->record->data + f->keys[k].offset;
			} else {
				key->data = f->keys[k].field->data;
			}
		} else {
			memset (keywrk, 0, sizeof(keywrk));
			key->size = sizeof(keywrk);
			key->attr = &alnum_attr;
			key->data = keywrk;
		}
		f->organization = COB_ORG_INDEXED;
		break;
	case ORG_RELATIVE:
		cob_set_int (f->keys[0].field, LDCOMPX4(LSUCHAR(fcd->relKey+4)));
		memcpy(&key, f->keys[0].field, sizeof(cob_field));
		f->organization = COB_ORG_RELATIVE;
		break;
	case ORG_SEQ:
		f->organization = COB_ORG_SEQUENTIAL;
		break;
	case ORG_LINE_SEQ:
		f->organization = COB_ORG_LINE_SEQUENTIAL;
		break;
	case ORG_DETERMINE:
		if (opcd == OP_GETINFO) {
			if (fcd->fnamePtr) {
				k = strlen(fcd->fnamePtr);
				if (k > LDCOMPX2(fcd->fnameLen))
					k = LDCOMPX2(fcd->fnameLen);
				while (k > 0 && fcd->fnamePtr[k-1] == ' ')
					--k;
				STCOMPX2(k,fcd->fnameLen);
				memcpy (file_open_name, fcd->fnamePtr, k);
				file_open_name[k] = 0;
#if 0	/* GC 4.x+ only */
				f->flag_auto_type = 1;
				f->flag_keycheck = 0;
#endif
				if (f->record == NULL
				 && fcd->recPtr != NULL
				 && LDCOMPX4(fcd->curRecLen) > 0) {
					f->record = cob_cache_malloc(sizeof(cob_field));
					f->record->data = fcd->recPtr;
					f->record->attr = &alnum_attr;
					f->record->size = LDCOMPX4(fcd->maxRecLen);
					f->record_min = LDCOMPX4(fcd->minRecLen);
					f->record_max = LDCOMPX4(fcd->maxRecLen);
				}
				cob_chk_file_mapping ();
#if 0	/* GC 4.x+ only */
				f->organization = COB_ORG_MAX;	/* To Force file.dd to be processed */
				sts = cob_read_dict (f, fname, 1);
				if (sts == -1) { /* Not present; check file */
					int ftype = indexed_file_type (f, fname);
					if (ftype >= 0) {
						f->io_routine = (unsigned char)ftype;
						f->organization = COB_ORG_INDEXED;
						return 0;
					}
				}
#endif
			}
			copy_file_to_fcd (f, fcd);
			return sts;
		}
		/* if we already registered this FCD to a file we can copy the old type */
		if (f->organization == COB_ORG_INDEXED) {
			fcd->fileOrg = ORG_INDEXED;
		} else if (f->organization == COB_ORG_SEQUENTIAL) {
			fcd->fileOrg = ORG_SEQ;
		} else if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
			fcd->fileOrg = ORG_LINE_SEQ;
		} else if (f->organization == COB_ORG_RELATIVE) {
			fcd->fileOrg = ORG_RELATIVE;
		}
		if (fcd->fileOrg != ORG_DETERMINE) {
			goto org_handling;
#if 0
		} else {
			/* TODO: magic to get file type, for example try to idx-open the file */
			if (fcd->fileOrg != ORG_DETERMINE) {
				goto org_handling;
			}
#endif
		}
		/* Fall through */
	default:
		fcd->fileStatus[0] = '9';
		fcd->fileStatus[1] = 161;
		cob_runtime_warning (_("ERROR: EXTFH called with wrong file organization %d"), fcd->fileOrg);
		return -1;
	}

	if (fcd->fnamePtr == NULL) {
		fcd->fileStatus[0] = '9';
		fcd->fileStatus[1] = 141;
		cob_runtime_warning (_("ERROR: EXTFH called with no %s pointer"),"filename");
		return -1;
	}

	if (f->record
	 && f->record->size < f->record_min) {
		f->record->size = f->record_min;
	}

	/* handle OPEN/CLOSE operations */

	switch (opcd) {
	case OP_OPEN_INPUT:
	case OP_OPEN_INPUT_NOREWIND:
	case OP_OPEN_INPUT_REVERSED:
		cob_open (f, COB_OPEN_INPUT, 0, fs);
		if (f->organization == COB_ORG_INDEXED
		 && f->file_status[0] == '0') {	/* 00 or 05 are both ok */
			f->open_mode = COB_OPEN_INPUT;
		}
		f->record_min = LDCOMPX4(fcd->minRecLen);
		f->record_max = LDCOMPX4(fcd->maxRecLen);
		STCOMPX4(f->record_max,fcd->curRecLen);
		update_file_to_fcd (f, fcd, fnstatus);
#if 0 /* Simon: general mapping needed, depending
                on a compile time switch (also for other dialects)
                --> no "single place extension" here */
		if (f->organization == COB_ORG_INDEXED
		 && memcmp (f->file_status, "61", 2) == 0) {/* 61 --> 9A for MF */
			memcpy (fcd->fileStatus,"9A", 2);
		}
#endif
		return sts;

	case OP_OPEN_OUTPUT:
	case OP_OPEN_OUTPUT_NOREWIND:
		cob_open (f, COB_OPEN_OUTPUT, 0, fs);
		if (f->organization == COB_ORG_INDEXED
		 && f->file_status[0] == '0') {	/* 00 or 05 are both ok */
			f->open_mode = COB_OPEN_OUTPUT;
		}
		update_file_to_fcd (f, fcd, fnstatus);
#if 0 /* Simon: general mapping needed, depending
                on a compile time switch (also for other dialects)
                --> no "single place extension" here */
		if (f->organization == COB_ORG_INDEXED
		 && memcmp (f->file_status, "61", 2) == 0) {/* 61 --> 9A for MF */
			memcpy (fcd->fileStatus,"9A", 2);
		}
#endif
		return sts;

	case OP_OPEN_IO:
		cob_open (f, COB_OPEN_I_O, 0, fs);
		if (f->organization == COB_ORG_INDEXED
		 && (memcmp (f->file_status, "00", 2) == 0
		  || memcmp (f->file_status, "05", 2) == 0
		  || memcmp (f->file_status, "35", 2) == 0)) {
			f->open_mode = COB_OPEN_I_O;
		}
		update_file_to_fcd (f, fcd, fnstatus);
#if 0 /* Simon: general mapping needed, depending
                on a compile time switch (also for other dialects)
                --> no "single place extension" here */
		if (f->organization == COB_ORG_INDEXED
		 && memcmp (f->file_status, "61", 2) == 0) {/* 61 --> 9A for MF */
			memcpy (fcd->fileStatus,"9A", 2);
		}
#endif
		return sts;

	case OP_OPEN_EXTEND:
		cob_open (f, COB_OPEN_EXTEND, 0, fs);
		if (f->organization == COB_ORG_INDEXED
		 && f->file_status[0] == '0') {	/* 00 or 05 are both ok */
			f->open_mode = COB_OPEN_EXTEND;
		}
		update_file_to_fcd (f, fcd, fnstatus);
#if 0 /* Simon: general mapping needed, depending
                on a compile time switch (also for other dialects)
                --> no "single place extension" here */
		if (f->organization == COB_ORG_INDEXED
		 && memcmp (f->file_status, "61", 2) == 0) {/* 61 --> 9A for MF */
			memcpy (fcd->fileStatus,"9A", 2);
		}
#endif
		return sts;

	case OP_CLOSE:
	case OP_CLOSE_REEL:
		cob_close (f, fs, COB_CLOSE_NORMAL, 0);
		update_file_to_fcd (f, fcd, fnstatus);
		return sts;

	case OP_CLOSE_LOCK:
		cob_close (f, fs, COB_CLOSE_LOCK, 0);
		update_file_to_fcd (f, fcd, fnstatus);
		return sts;

	case OP_CLOSE_REMOVE:
		cob_close (f, fs, COB_CLOSE_UNIT_REMOVAL, 0);
		update_file_to_fcd (f, fcd, fnstatus);
		return sts;

	case OP_CLOSE_NO_REWIND:
	case OP_CLOSE_NOREWIND:
		cob_close (f, fs, COB_CLOSE_NO_REWIND, 0);
		update_file_to_fcd (f, fcd, fnstatus);
		return sts;

	case OP_FLUSH:
		cob_sync (f);
		return sts;

	case OP_DELETE_FILE:
		cob_delete_file (f, fs);
		memcpy (fcd->fileStatus, fnstatus, 2);
		return sts;

	case OP_GETINFO:			/* Nothing needed here */
		return sts;

	case OP_COMMIT:
		cob_commit ();
		return sts;

	case OP_ROLLBACK:
		cob_rollback ();
		return sts;
	}

	/* handle record related operations */

	if (fcd->recPtr == NULL) {
		fcd->fileStatus[0] = '9';
		fcd->fileStatus[1] = 141;
		cob_runtime_warning (_("ERROR: EXTFH called with no %s pointer; Function %d"),
						"record", opcd & 0xFF);
		return -1;
	}

	/* create a local record field as following ABI functions expect it */
	rec->data = fcd->recPtr;
	rec->size = LDCOMPX4(fcd->curRecLen);
	rec->attr = &alnum_attr;

#if 0	/* CHECKME: why should we adjust the access mode?
	If wrong file, status should be raised in the following functions */
	if (f->organization == COB_ORG_INDEXED
	&& (f->open_mode == COB_OPEN_I_O
	 || f->open_mode == COB_OPEN_OUTPUT)) {
		fcd->accessFlags = ACCESS_DYNAMIC;
		f->access_mode = COB_ACCESS_DYNAMIC;
	}
#endif

	switch (opcd) {
	case OP_READ_PREV:
	case OP_READ_PREV_LOCK:
	case OP_READ_PREV_NO_LOCK:
	case OP_READ_PREV_KEPT_LOCK:
		opts = COB_READ_PREVIOUS;
		if (opcd == OP_READ_PREV_LOCK)
			opts |= COB_READ_LOCK;
		else if (opcd == OP_READ_PREV_NO_LOCK)
			opts |= COB_READ_NO_LOCK;
		else if (opcd == OP_READ_PREV_KEPT_LOCK)
			opts |= COB_READ_KEPT_LOCK;
		cob_read_next (f, fs, opts);
		update_file_to_fcd (f, fcd, fnstatus);
		break;

	case OP_READ_SEQ:
	case OP_READ_SEQ_LOCK:
	case OP_READ_SEQ_NO_LOCK:
	case OP_READ_SEQ_KEPT_LOCK:
		opts = COB_READ_NEXT;
		if (opcd == OP_READ_SEQ_LOCK)
			opts |= COB_READ_LOCK;
		else if (opcd == OP_READ_SEQ_NO_LOCK)
			opts |= COB_READ_NO_LOCK;
		else if (opcd == OP_READ_SEQ_KEPT_LOCK)
			opts |= COB_READ_KEPT_LOCK;
		cob_read_next (f, fs, opts);
		update_file_to_fcd (f, fcd, NULL);
		break;

	case OP_STEP_NEXT:
	case OP_STEP_NEXT_LOCK:
	case OP_STEP_NEXT_NO_LOCK:
	case OP_STEP_NEXT_KEPT_LOCK:
		opts = COB_READ_NEXT;
		if (opcd == OP_STEP_NEXT_LOCK)
			opts |= COB_READ_LOCK;
		else if (opcd == OP_STEP_NEXT_NO_LOCK)
			opts |= COB_READ_NO_LOCK;
		else if (opcd == OP_STEP_NEXT_KEPT_LOCK)
			opts |= COB_READ_KEPT_LOCK;
		cob_read_next (f, fs, opts);
		update_file_to_fcd (f, fcd, NULL);
		break;

	case OP_STEP_FIRST:
	case OP_STEP_FIRST_LOCK:
	case OP_STEP_FIRST_NO_LOCK:
	case OP_STEP_FIRST_KEPT_LOCK:
		opts = COB_READ_FIRST;
		if (opcd == OP_STEP_FIRST_LOCK)
			opts |= COB_READ_LOCK;
		else if (opcd == OP_STEP_FIRST_NO_LOCK)
			opts |= COB_READ_NO_LOCK;
		else if (opcd == OP_STEP_FIRST_KEPT_LOCK)
			opts |= COB_READ_KEPT_LOCK;
		cob_read_next (f, fs, opts);
		update_file_to_fcd (f, fcd, NULL);
		break;

	case OP_READ_RAN:
	case OP_READ_RAN_LOCK:
	case OP_READ_RAN_NO_LOCK:
	case OP_READ_RAN_KEPT_LOCK:
		opts = LDCOMPX4(fcd->opt);
		if (opcd == OP_READ_RAN_LOCK)
			opts |= COB_READ_LOCK;
		else if (opcd == OP_READ_RAN_NO_LOCK)
			opts |= COB_READ_NO_LOCK;
		else if (opcd == OP_READ_RAN_KEPT_LOCK)
			opts |= COB_READ_KEPT_LOCK;
		cob_read (f, key, fs, opts);
		update_file_to_fcd (f, fcd, fnstatus);
		break;

	case OP_WRITE:
		if (f->record
		 && rec->size >= LDCOMPX4(fcd->minRecLen)
		 && rec->size <= LDCOMPX4(fcd->maxRecLen)) {
			f->record->size = rec->size;
		}
		if (rec->size < f->record_min) {
			rec->size = f->record_min; 
		}
		eop = LDCOMPX2(fcd->eop);
		opts = LDCOMPX4(fcd->opt);
		cob_write (f, rec, opts, fs, eop);
		update_file_to_fcd (f, fcd, fnstatus);
		break;

	case OP_REWRITE:
		if (f->record
		 && rec->size >= LDCOMPX4(fcd->minRecLen)
		 && rec->size <= LDCOMPX4(fcd->maxRecLen)) {
			f->record->size = rec->size;
		}
		if (rec->size < f->record_min) {
			rec->size = f->record_min; 
		}
		opts = LDCOMPX4(fcd->opt);
		cob_rewrite (f, rec, opts, fs);
		update_file_to_fcd (f, fcd, fnstatus);
		break;

	case OP_DELETE:
		cob_delete (f, fs);
		update_file_to_fcd (f, fcd, fnstatus);
		break;

	case OP_START_EQ:
		cob_start (f, COB_EQ, key, NULL, fs);
		update_file_to_fcd (f, fcd, fnstatus);
		break;

	case OP_START_GE:
		cob_start (f, COB_GE, key, NULL, fs);
		update_file_to_fcd (f, fcd, fnstatus);
		break;

	case OP_START_LE:
		cob_start (f, COB_LE, key, NULL, fs);
		update_file_to_fcd (f, fcd, fnstatus);
		break;

	case OP_START_LT:
		cob_start (f, COB_LT, key, NULL, fs);
		update_file_to_fcd (f, fcd, fnstatus);
		break;

	case OP_START_GT:
		cob_start (f, COB_GT, key, NULL, fs);
		update_file_to_fcd (f, fcd, fnstatus);
		break;

	case OP_START_FI:
		cob_start (f, COB_FI, key, NULL, fs);
		update_file_to_fcd (f, fcd, fnstatus);
		break;

	case OP_START_LA:
		cob_start (f, COB_LA, key, NULL, fs);
		update_file_to_fcd (f, fcd, fnstatus);
		break;

	case OP_UNLOCK_REC:
		cob_unlock_file (f, fs);
		update_file_to_fcd (f, fcd, fnstatus);
		break;

	/* Similar for other possible 'opcode' values */
	default:
		fcd->fileStatus[0] = '9';
#if 0	/* MF seems to return 142 file not open, possibly only until opened;
		   we use 100->invalid file operation */
		fcd->fileStatus[1] = 142;
#else
		fcd->fileStatus[1] = 100;
#endif
		cob_runtime_warning (_("ERROR: EXTFH called with unknown Function %d"),
				opcd & 0xFF);
		/* MF does not set the return-code in this case - we possible want to do that */
		break;
	}
	return sts;
}

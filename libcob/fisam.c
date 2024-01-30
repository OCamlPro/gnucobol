/*
   Copyright (C) 2002-2012, 2014-2024 Free Software Foundation, Inc.
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

#include "fileio.h"

/* the common build system only compiles this file if any isam is to be included
   and _includes_ this file via libcob*.c instead of compiling it,
   but legacy hard-wired ones like VS need this "all file" check) */
#if	defined(WITH_CISAM) || defined(WITH_DISAM) || defined(WITH_VBISAM) || defined(WITH_VISAM)

#if defined(IS_ISAM_LIB)
/* the common way to use this source is one library for each configured
   ISAM handler, depending on FOR_xISAM defines in the build system
   --> drop all but the currently active WITH_ defines */
#ifdef WITH_CISAM
#undef WITH_CISAM
#endif
#ifdef WITH_DISAM
#undef WITH_DISAM
#endif
#ifdef WITH_VBISAM
#undef WITH_VBISAM
#endif
#ifdef WITH_VISAM
#undef WITH_VISAM
#endif

#ifdef FOR_CISAM
#define WITH_CISAM
#endif
#ifdef FOR_DISAM
#define WITH_DISAM
#endif
#ifdef FOR_VBISAM
#define WITH_VBISAM
#endif
#ifdef FOR_VISAM
#define WITH_VISAM
#endif
#endif

#define ISRECNUM isrecnum
#define ISERRNO  iserrno
#define ISRECLEN isreclen
#define ISSTAT1 isstat1
#define ISSTAT2 isstat2
#ifdef cobglobptr
#undef cobglobptr
#endif
#ifdef cobsetptr
#undef cobsetptr
#endif
#define cobglobptr	isam_globptr
#define cobsetptr	isam_setptr
static	cob_global		*isam_globptr;
static	cob_settings	*isam_setptr;
#if defined(WITH_VBISAM) && defined(WITH_DISAM)
#undef WITH_DISAM
#endif

/* C-ISAM specifics */
#if	defined(WITH_CISAM)
#include <isam.h>
#define	isfullclose(x)	isclose (x)
#define	COB_WITH_STATUS_02

/* D-ISAM specifics */
#elif	defined(WITH_DISAM)
#define	isfullclose(x)	isclose (x)
/* current D-ISAM */
#if defined(HAVE_ISCONFIG_H) && defined(HAVE_ISINTSTD_H)
#include <isconfig.h>
#if defined(HAVE_ISWRAP_H)
#include <iswrap.h>
#endif
#ifdef ISMAXKEY
#define MAXKEYLEN ISMAXKEY
#endif
#if defined (ISCOBOL_STATS) && (ISCOBOL & ISCOBOL_STATS)
/* the additional status is a configurable feature with DISAM */
#ifndef COB_WITH_STATUS_02 
#define	COB_WITH_STATUS_02
#endif
#endif

#else   /* old version of DISAM */
#include <disam.h>
/* isstat1 / isstat2 implementation unclear */
#endif

/* V-ISAM specifics (updated free VBISAM) */
#elif	defined(WITH_VISAM)
#include <visam.h>
#ifdef VB_MAX_KEYLEN
#ifdef MAXKEYLEN
#undef MAXKEYLEN
#endif
#define MAXKEYLEN VB_MAX_KEYLEN
#endif
#if defined(VB_RTD)
#undef VB_RTD
#endif
#if !defined(COB_WITH_STATUS_02)
#define COB_WITH_STATUS_02
#endif

/* VBISAM specifics (either "old" one or updated, or VBISAM 2.2) */
#elif	defined(WITH_VBISAM)
#include <vbisam.h>
#if defined(VBISAM_VERSION)
#define	COB_WITH_STATUS_02 /* new VBISAM (2.2+ supports the 02 status) */
#endif
#ifdef VB_MAX_KEYLEN
#define MAXKEYLEN VB_MAX_KEYLEN
#endif

/* Since VBISAM 2.1.1: access to isrecnum iserrno etc is no longer global */
#if defined(VB_RTD)
static	vb_rtd_t *vbisam_rtd = NULL;
#undef ISRECNUM 
#define ISRECNUM vbisam_rtd->isrecnum
#undef ISERRNO 
#define ISERRNO  vbisam_rtd->iserrno
#undef ISRECLEN
#define ISRECLEN vbisam_rtd->isreclen
#undef ISSTAT1
#define ISSTAT1 vbisam_rtd->isstat1
#undef ISSTAT2
#define ISSTAT2 vbisam_rtd->isstat2
#endif
#endif

#if (ISEXTENDED == 1)
#define ISPID ispid
#else
#define ISPID 0
#endif

#ifndef ISVARLEN
/* ISAM handler does not support variable length records */
#define ISVARLEN 0
#endif   
/**********************************************************************/

#ifndef MAXKEYLEN
#define MAXKEYLEN 240
#endif

#ifndef MAXNUMKEYS
#define MAXNUMKEYS 32
#endif

#ifdef COB_WITH_STATUS_02
#define COB_CHECK_DUP(s) s ? s : \
		(ISSTAT1 == '0' && ISSTAT2 == '2' && !f->flag_read_no_02) ? \
		COB_STATUS_02_SUCCESS_DUPLICATE : 0
#else
#define COB_CHECK_DUP(s) s ? s : s 
#endif

/* Isam File handler packet */

struct indexfile {
	char	*filename;	/* ISAM data file name */
	char	*savekey;	/* Area to save last primary key read */
	char	*recwrk;	/* Record work/save area */
	char	*recorg;	/* Original Record save area */
	int		isopenmode;	/* Options used for isopen */
	int		nkeys;		/* Actual keys in file */
	int		isfd;		/* ISAM file number */
	long	recnum;		/* Last record number read */
	long	saverecnum;	/* isrecnum of next record to process */
	long	duprecnum;	/* isrecnum of dups_ahead process */
	int		saveerrno;	/* savefileposition errno */
	int		lmode;		/* File lock mode for 'isread' */
	int		startcond;	/* Previous 'start' condition value */
	int		startfail;	/* Status of Previous 'start' */
	int		readdir;	/* Read direction: ISPREV or ISNEXT */
	int		lenkey;		/* Length of savekey area */
	int		eofpending;	/* End of file pending */
	int		readdone;	/* A 'read' has been successfully done */
	int		startiscur;	/* The 'start' record is current */
	int		wrkhasrec;	/* 'recwrk' holds the next|prev record */
	unsigned char idxmap[MAXNUMKEYS];
	struct keydesc	key[1];		/* Table of key information */
					/* keydesc is defined in (d|c|vb)isam.h */
};

/* Local variables */

static int isam_open	(cob_file_api *a, cob_file *, char *, const int, const int);
static int isam_close	(cob_file_api *a, cob_file *, const int);
static int isam_start	(cob_file_api *a, cob_file *, const int, cob_field *);
static int isam_read	(cob_file_api *a, cob_file *, cob_field *, const int);
static int isam_read_next(cob_file_api *a, cob_file *, const int);
static int isam_write	(cob_file_api *a, cob_file *, const int);
static int isam_delete	(cob_file_api *a, cob_file *);
static int isam_rewrite	(cob_file_api *a, cob_file *, const int);
static int isam_file_delete (cob_file_api *a, cob_file *f, char *name);
static int isam_sync (cob_file_api *a, cob_file *f);
static int isam_commit (cob_file_api *a, cob_file *f);
static int isam_rollback (cob_file_api *a, cob_file *f);
static char *isam_version (void);
static void cob_isam_exit_fileio (cob_file_api *a);

COB_EXT_EXPORT void cob_isam_init_fileio (cob_file_api *a);

static int 
isam_dummy ()
{
	return 0;
}

static const struct cob_fileio_funcs ext_indexed_funcs = {
	isam_open,
	isam_close,
	isam_start,
	isam_read,
	isam_read_next,
	isam_write,
	isam_rewrite,
	isam_delete,
	isam_file_delete,
	cob_isam_init_fileio,
	cob_isam_exit_fileio,
	isam_dummy,			/* forking */
	isam_sync,			/* sync */
	isam_commit,		/* commit */
	isam_rollback,		/* rollback */
	isam_dummy,			/* unlock */
	isam_version
};

/* max_keycomp may be reduced depending on the x-ISAM configuration */
static int max_keycomp = COB_MAX_KEYCOMP;
static const char *isam_name = "xISAM";

/* Local functions */


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
		sts = memcmp( data + fh->key[idx].k_part[part].kp_start,
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
	int	keylen,part,keytype;
	/* LCOV_EXCL_START */
	if (key->count_components < 1) {
		/* Should not happen as this was generated pre GC 3.0 */
		cob_runtime_error ("file i/o version mismatch");
		cob_stop_run (1);
	}
	/* LCOV_EXCL_STOP */

	memset (kd,0,sizeof (struct keydesc));
	kd->k_flags = key->tf_duplicates ? ISDUPS : ISNODUPS;
	if (key->tf_compress		/* MF COBOL #SET KEYCOMPRESS */
	&& !key->tf_suppress) {		/* Not supported if Suppress Character */
		if (key->tf_compress & 1)
			kd->k_flags += DCOMPRESS;
		if (key->tf_compress & 2)
			kd->k_flags += LCOMPRESS;
		if (key->tf_compress & 4)
			kd->k_flags += TCOMPRESS;
	}

	/* multi- and single field key as component */
	/* LCOV_EXCL_START */
	if (key->count_components > max_keycomp) {
		/* not translated as this is safety guard unlikely to be ever triggered */
		cob_runtime_warning ("module specifies %d key components, "
			"%s runtime ignores all parts greater than %d",
			key->count_components, isam_name, max_keycomp);
		key->count_components = max_keycomp;
	}
	/* LCOV_EXCL_STOP */

	keylen = 0;
	for (part=0; part < key->count_components && part < max_keycomp; part++) {
		kd->k_part[part].kp_start = key->component[part]->data - f->record->data;
		kd->k_part[part].kp_leng = key->component[part]->size;
		keylen += kd->k_part[part].kp_leng;
		keytype = CHARTYPE;
		if (key->component[part]->attr->type == COB_TYPE_NUMERIC_FLOAT) {
			keytype = FLOATTYPE;
		} else if (key->component[part]->attr->type == COB_TYPE_NUMERIC_DOUBLE) {
			keytype = DOUBLETYPE;
		} else if (key->component[part]->attr->type == COB_TYPE_NUMERIC_BINARY) {
#if defined(INTTYPE) && defined(LONGTYPE)
#ifdef WORDS_BIGENDIAN
			if (key->component[part]->size == 2) {
				keytype = MINTTYPE;		/* 'short' */
			} else if (key->component[part]->size == 4) {
				keytype = MLONGTYPE;	/* 'int' */
			}
#else
			if (COB_FIELD_BINARY_SWAP(key->component[part])) {
				if (key->component[part]->size == 2) {
					keytype = INTTYPE;		/* 'short' */
				} else if (key->component[part]->size == 4) {
					keytype = LONGTYPE;		/* 'int' */
				}
			} else {
				if (key->component[part]->size == 2) {
					keytype = MINTTYPE;		/* 'short' */
				} else if (key->component[part]->size == 4) {
					keytype = MLONGTYPE;	/* 'int' */
				}
			}
#endif
#endif
		} else if (key->component[part]->attr->type == COB_TYPE_NUMERIC_COMP5
				&& COB_FIELD_REAL_BINARY(key->component[part])) {
#if defined(MINTTYPE) && defined(MLONGTYPE)
			if (key->component[part]->size == 2) {
				keytype = MINTTYPE;		/* Machine 'short' */
			} else if (key->component[part]->size == 4) {
				keytype = MLONGTYPE;	/* Machine 'int' */
			}
#endif
		}
		kd->k_part[part].kp_type = keytype;
		if (key->tf_suppress) {
#ifdef NULLKEY
			kd->k_flags |= NULLKEY;
			kd->k_part[part].kp_type = keytype | (key->char_suppress << 8);
#else
			f->flag_write_chk_dups = 1;
			kd->k_flags = ISDUPS;
#endif
		}
	}
	kd->k_nparts = part;

	kd->k_len = keylen;		/* Total length of this key */
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

static int
isam_sync (cob_file_api *a, cob_file *f)
{
	struct indexfile	*fh;

	COB_UNUSED (a);
#ifdef ISSYNCWR
	COB_UNUSED (f);
	COB_UNUSED (fh);
#else
	if (f->organization == COB_ORG_INDEXED
	 && (f->file_features & COB_FILE_SYNC)
	 && f->open_mode != COB_OPEN_INPUT
	 && f->open_mode != COB_OPEN_CLOSED) {
		fh = f->file;
		if (fh
		 && fh->isfd > 0) {
			isflush (fh->isfd);
		}
	}
#endif
	return 0;
}

static int
isam_commit (cob_file_api *a, cob_file *f)
{
	COB_UNUSED (a);
#ifndef ISSYNCWR
	if (f->organization == COB_ORG_INDEXED
	 && (f->file_features & COB_FILE_SYNC)
	 && f->open_mode != COB_OPEN_INPUT
	 && f->open_mode != COB_OPEN_CLOSED) {
		struct indexfile	*fh = f->file;
		if (fh
		 && fh->isfd > 0) {
			isflush (fh->isfd);
		}
	}
#endif
	f->flag_was_updated = 0;
	return 0;
}

static int
isam_rollback (cob_file_api *a, cob_file *f)
{
	COB_UNUSED (a);
	COB_UNUSED (f);

	f->flag_was_updated = 0;
	return 0;
}

/* INDEXED */

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
	case EDEADLK:
		return COB_STATUS_52_DEAD_LOCK;
	case ENOLCK:
		return COB_STATUS_53_MAX_LOCKS;
	case EFLOCKED:
		return COB_STATUS_61_FILE_SHARING;
	case ENOCURR:
		if (default_status != COB_STATUS_10_END_OF_FILE) {
			return COB_STATUS_21_KEY_INVALID;
		}
		break;
	case ENOTOPEN:
		return COB_STATUS_49_I_O_DENIED;
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
	if (fh->recorg) {
		cob_free ((void *)fh->recorg);
	}
	cob_free ((void *)fh);
}

/* Restore ISAM file positioning */
static void
restorefileposition (cob_file *f)
{
	struct indexfile	*fh;
	struct keydesc		k0;
	int		sverrno = ISERRNO;

	fh = f->file;
	memset ((void *)&k0, 0, sizeof (k0));
	ISERRNO = 0;
	if (fh->saverecnum >= 0) {
		/* Record# of 'next' record in the index */
		ISRECNUM = fh->saverecnum;
		/* Switch to record number mode */
		isstart (fh->isfd, &k0, 0, (void *)fh->recwrk, ISEQUAL);
		/* Read by record number */
		ISERRNO = 0;
		isread (fh->isfd, (void *)fh->recwrk, ISCURR);
		/* Read by active key value */
		isstart (fh->isfd, &fh->key[f->curkey], 0, (void *)fh->recwrk, ISGTEQ);
		isread (fh->isfd, (void *)fh->recwrk, ISGTEQ);
		while (ISRECNUM != fh->saverecnum) {
			/* Read back into position */
			if (isread (fh->isfd, (void *)fh->recwrk, ISNEXT)) {
				break;
			}
		}
		if (ISRECNUM == fh->saverecnum
		 && fh->saverecnum != fh->duprecnum) {	/* Leave here for read dup check */
			if (fh->readdir == ISNEXT) {
				/* Back off by one so next read gets this */
				isread (fh->isfd, (void *)fh->recwrk, ISPREV);
			} else {
				isread (fh->isfd, (void *)fh->recwrk, ISNEXT);
			}
		}
	} else if (fh->readdone && f->curkey == 0) {
		indexed_restorekey(fh, NULL, 0);
		isstart (fh->isfd, &fh->key[f->curkey], 0, (void *)fh->recwrk, ISGTEQ);
	}
	ISERRNO = sverrno;
}

/* Save ISAM file positioning information for later 'restorefileposition' */

static void
savefileposition (cob_file *f)
{
	struct indexfile	*fh;

	fh = f->file;
	fh->saverecnum = -1;
	fh->duprecnum = 0;
	if (f->curkey >= 0 && fh->readdir != -1) {
		/* Switch back to index */
		memcpy (fh->recwrk, f->record->data, f->record_max);
		if (fh->wrkhasrec != fh->readdir) {
			fh->eofpending = 0;
			fh->wrkhasrec = 0;
			/* Read next record in file */
			if (isread (fh->isfd, (void *)fh->recwrk, fh->readdir)) {
				fh->saverecnum = -1;
				fh->saveerrno = ISERRNO;
				if (fh->saveerrno == EENDFILE 
				 || fh->saveerrno == ENOREC)  {
					fh->eofpending = fh->readdir;
				}
			} else {
				fh->saverecnum = ISRECNUM;
				fh->saveerrno = 0;
			}
			/* Restore saved record data */
			memcpy (fh->recwrk, f->record->data, f->record_max);
		}
	}
}

/* Get length of variable length record */
static COB_INLINE COB_A_INLINE void
get_isreclen (cob_file *f)
{
	if (f->record_min != f->record_max) {
#if ISVARLEN == 0
		/* This ISAM is not supporting Variable length so simulate using trailling NULs */
		int k;
		if (f->variable_record) {
			for (k = f->record_max; k > f->record_min && f->record->data[k-1] == 0; k--);
			f->record->size = k;
			cob_set_int (f->variable_record, (int) k);
		}
#else
		f->record->size = ISRECLEN;
		if (f->record->size < f->record_max)
			memset(&f->record->data[f->record->size], 0, f->record_max - f->record->size);
		if (f->variable_record)
			cob_set_int (f->variable_record, (int) f->record->size);
#endif
	}
}

/* Set isreclen for ISAM handler of variable length record */
static COB_INLINE COB_A_INLINE void
set_isreclen (cob_file *f)
{
	if (f->record_min != f->record_max) {
		ISRECLEN = f->record->size;
#if ISVARLEN == 0
		/* This ISAM is not supporting Variable length so simulate using trailling NULs */
		if (f->record->size < f->record_max)
			memset(&f->record->data[f->record->size], 0, f->record_max - f->record->size);
#endif
	}
}

/*
 * Open ISAM File, if locked retry as requested
 */
static int
isopen_retry(cob_file *f, char *filename, int mode)
{
	int	isfd, retry, interval;

	retry = interval = 0;
	if ((f->retry_mode & COB_RETRY_FOREVER)) {
		retry = -1;
	} else
	if ((f->retry_mode & COB_RETRY_SECONDS)) {
		retry = 1;
		interval = f->retry_seconds>0?f->retry_seconds:
			(isam_setptr->cob_retry_seconds>0?isam_setptr->cob_retry_seconds:1);
	} else
	if ((f->retry_mode & COB_RETRY_TIMES)) {
		retry = f->retry_times>0?f->retry_times:
			(isam_setptr->cob_retry_times>0?isam_setptr->cob_retry_times:1);
		interval = isam_setptr->cob_retry_seconds>0?isam_setptr->cob_retry_seconds:1;
	}
	if(retry > 0) {
		retry = retry * interval * COB_RETRY_PER_SECOND ;
		interval = 1000 / COB_RETRY_PER_SECOND ;
	}
	isfd = isopen ((void *)filename, mode);
	while(isfd < 0 && retry != 0) {
		if (ISERRNO != EFLOCKED) 
			break;
		if(retry > 0) {
			retry--;
			cob_sleep_msec(interval);
		}
		isfd = isopen ((void *)filename, mode);
	}
	if (isfd >= 0
	 && (mode & ISEXCLLOCK))
		f->flag_file_lock = 1;	
	else
		f->flag_file_lock = 0;	
	return isfd;
}

/*
 * Read ISAM record, if locked retry as requested
 */
static int
isread_retry(cob_file *f, void *data, int mode)
{
	int	isfd, sts, retry, interval;
	struct indexfile	*fh;

	fh = f->file;
	isfd = fh->isfd;

	retry = interval = 0;
	if ((f->retry_mode & COB_RETRY_FOREVER)) {
		retry = -1;
	} else
	if ((f->retry_mode & COB_RETRY_SECONDS)) {
		retry = 1;
		interval = f->retry_seconds>0?f->retry_seconds:
			(isam_setptr->cob_retry_seconds>0?isam_setptr->cob_retry_seconds:1);
	} else
	if ((f->retry_mode & COB_RETRY_TIMES)) {
		retry = f->retry_times>0?f->retry_times:
			(isam_setptr->cob_retry_times>0?isam_setptr->cob_retry_times:1);
		interval = isam_setptr->cob_retry_seconds>0?isam_setptr->cob_retry_seconds:1;
	}
	if(retry > 0) {
		retry = retry * interval * COB_RETRY_PER_SECOND ;
		interval = 1000 / COB_RETRY_PER_SECOND ;
	}
	do {
		ISERRNO = 0;
		sts = isread (isfd, data, mode);
#ifdef	ISSKIPLOCK
		if ((mode & ISSKIPLOCK))
			break;
#endif
		if (!(mode & ISLOCK))
			break;
		if (ISERRNO != ELOCKED
		 || retry == 0
		 || sts == 0) 
			break;
		if(retry > 0) {
			retry--;
			cob_sleep_msec(interval);
		}
	} while(sts != 0 && retry != 0);
	if (ISERRNO == ELOCKED
	 || ISERRNO == EDEADLK
	 || ISERRNO == EFLOCKED) {
		f->blockpid = ISPID;
	}
	return sts;
}

/* Delete file */

static int
isam_file_delete (cob_file_api *a, cob_file *f, char *filename)
{
	struct stat	st;
	char	file_name_buf [COB_FILE_MAX];

	COB_UNUSED (a);
	COB_UNUSED (f);

	snprintf (file_name_buf, (size_t)COB_FILE_MAX, "%s.idx", filename);
	unlink (file_name_buf);
	snprintf (file_name_buf, (size_t)COB_FILE_MAX, "%s.dat", filename);
	if (stat(file_name_buf, &st) != 0) {	/* Micro Focus naming style has no .dat */
		snprintf (file_name_buf, (size_t)COB_FILE_MAX, "%s", filename);
	}
	unlink (file_name_buf);
	return 0;
}

/* Return ISAM handler version */

static char *
isam_version (void)
{
	static char	msg[64];
	int		ln = 0;
	strcpy(msg,"NO ISAM");
#ifdef NPARTS
	if (max_keycomp > NPARTS)
		max_keycomp = NPARTS;
#endif
#ifdef ISMAXPARTS
	if (max_keycomp > ISMAXPARTS)
		max_keycomp = ISMAXPARTS;
#endif
#if defined(WITH_CISAM)
	ln = sprintf(msg, "C-ISAM");
#elif defined(WITH_DISAM)
	ln = sprintf(msg, "D-ISAM %s",isversnumber);
#elif defined(WITH_VBISAM)
#ifdef VBISAM_VERSION
	ln = sprintf(msg, "VBISAM %s",VBISAM_VERSION);
#elif defined  (VB_RTD)
	ln = sprintf(msg, "VBISAM (RTD)");
#else
	ln = sprintf(msg, "VBISAM");
#endif
#elif defined(WITH_VISAM)
#ifdef VISAM_VERSION
	ln = sprintf(msg, "V-ISAM %s",VISAM_VERSION);
#else
	ln = sprintf(msg, "V-ISAM in C-ISAM mode");
#endif
#endif
	if (max_keycomp < COB_MAX_KEYCOMP)
		sprintf(&msg[ln]," (max %d key parts)",max_keycomp);
	return msg;
}

/* OPEN INDEXED file */
static int
isam_set_mode (cob_file *f)
{
	int	fmode = 0;
#if defined(WITH_VISAM)
	if (f->flag_vb_isam) 		/* Set default ISAM format */
		fmode = ISMVBISAM;
	else
		fmode = ISMCISAM;
	if (f->flag_isnodat) 		/* Do not use '.dat' */
		fmode |= ISMNODAT;
	if (f->isam_duplen == 2
	 || f->isam_duplen == 4) {	/* Size of dups counter */
		fmode |= f->isam_duplen << ISMDUPSHIFT;
	}
	if (f->isam_idxsz > 0) {	/* Size of index blocks */
		fmode |= f->isam_idxsz << ISMIDXSHIFT;
	}
#endif
	return fmode;
}

static int
isam_open (cob_file_api *a, cob_file *f, char *filename, const int mode, const int sharing)
{
	/* Note filename points to file_open_name */
	/* cob_chk_file_mapping manipulates file_open_name directly */

	struct indexfile	*fh, *fh2;
	int			k;
	int			ret,len,j;
	int			omode, fmode, lmode, vmode;
	int			dobld;
	int			isfd;
	int			checkvalue;
	struct stat st;
	struct keydesc		kd;
	struct dictinfo		di;		/* Defined in (c|d|vb)isam.h */

#if defined(WITH_CISAM)
	f->io_routine = COB_IO_CISAM;
#elif defined(WITH_DISAM)
	f->io_routine = COB_IO_DISAM;
#elif defined(WITH_VBISAM)
	f->io_routine = COB_IO_VBISAM;
#elif defined(WITH_VISAM)
	f->io_routine = COB_IO_VISAM;
#endif
	if (mode == COB_OPEN_INPUT) {
		checkvalue = R_OK;
	} else {
		checkvalue = R_OK | W_OK;
	}
	fmode = 0;

	if (stat(filename, &st) != -1
	 && S_ISDIR(st.st_mode)) {	/* Filename is a directory */
		return COB_XSTATUS_IS_DIR;
	}

	snprintf (a->file_open_buff, (size_t)COB_FILE_MAX, "%s.idx", filename);
	errno = 0;
	if (access (a->file_open_buff, checkvalue)) {
		fmode = isam_set_mode (f);
		if (!(errno == ENOENT && (mode == COB_OPEN_OUTPUT || f->flag_optional == 1))) {
			switch (errno) {
			case ENOENT:
				return COB_STATUS_35_NOT_EXISTS;
			case EACCES:
				return COB_STATUS_37_PERMISSION_DENIED;
			default:
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
#if defined(WITH_VISAM)
	} else {
		unsigned char	idxhdr[32];
		int		idxsz;
		/* The file already exists and if it is VBISAM format then retain that */
		isfd = open (a->file_open_buff, 0);
		if (isfd >= 0
		 && read (isfd, (void*)idxhdr, 32) == 32) {
			if (idxhdr[0] == 'V'
			 && idxhdr[1] == 'B'
			 && idxhdr[2] == 0x02
			 && idxhdr[3] == 0x02) {	/* VBISAM signature */
				fmode = ISMVBISAM;
				if (f->isam_idxsz > 0) {
					fmode |= f->isam_idxsz << ISMIDXSHIFT;
				} else {	/* Retain the index block size */
					idxsz = ((idxhdr[6] << 8) + idxhdr[7]) + 1;
					fmode |= (idxsz / 512) << ISMIDXSHIFT;
				}
			} else
			if (idxhdr[0] == 0xFE
			 && idxhdr[1] == 0x53
			 && idxhdr[2] == 0x02
			 && idxhdr[3] == 0x02) {	/* C-ISAM signature */
				fmode = ISMCISAM;
				if (f->isam_duplen == 2
				 || f->isam_duplen == 4) {
					fmode |= f->isam_duplen << ISMDUPSHIFT;
				} else 
				if (idxhdr[10] == 0x07)	 {	/* Retain DUPLEN value */
					fmode |= idxhdr[11] << ISMDUPSHIFT;
				}
				if (f->isam_idxsz > 0) {
					fmode |= f->isam_idxsz << ISMIDXSHIFT;
				} else {	/* Retain the index block size */
					idxsz = ((idxhdr[6] << 8) + idxhdr[7]) + 1;
					fmode |= (idxsz / 512) << ISMIDXSHIFT;
				}
			}
		} else {
			fmode = isam_set_mode (f);
		}
		if (isfd >= 0)
			close (isfd);
#endif
	}

	if (f->flag_isnodat) { 		/* Do not use '.dat' */
		snprintf (a->file_open_buff, (size_t)COB_FILE_MAX, "%s", filename);
#ifdef ISMNODAT
		fmode |= ISMNODAT;
#endif
	} else {
		snprintf (a->file_open_buff, (size_t)COB_FILE_MAX, "%s.dat", filename);
	}
	errno = 0;
#if defined(WITH_DISAM) || defined(WITH_VISAM)
	if (access (a->file_open_buff, checkvalue)
	&& (errno == ENOENT) ) {	/* DISAM/V-ISAM will handle files with MF naming style */
		errno = 0;
		snprintf (a->file_open_buff, (size_t)COB_FILE_MAX, "%s", filename);
#ifdef ISMNODAT
		if (f->flag_isnodat) 		/* Do not use '.dat' */
			fmode |= ISMNODAT;
#endif
	}
#endif
	if (access (a->file_open_buff, checkvalue)) {
		if (!(errno == ENOENT && (mode == COB_OPEN_OUTPUT || f->flag_optional == 1))) {
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
	dobld = 0;
	isfd = -1;
	if (f->record_min != f->record_max) {
		vmode = ISVARLEN;
		ISRECLEN = f->record_min;
	} else {
		vmode = 0;
	}

#ifdef ISSYNCWR
	if ((f->file_features & COB_FILE_SYNC)) {
		vmode |= ISSYNCWR;			/* Sync I/O to disk */
	}
#endif

	if ((f->share_mode & COB_SHARE_NO_OTHER)
	 || (f->lock_mode & COB_FILE_EXCLUSIVE) ) {
		lmode = ISEXCLLOCK;
	} else if (!f->lock_mode) {
		if (mode != COB_OPEN_INPUT) {
			lmode = ISEXCLLOCK;
		} else {
			lmode = ISMANULOCK;
		}
	} else if ((f->lock_mode & COB_LOCK_AUTOMATIC) 
			&& mode != COB_OPEN_INPUT) {
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
		omode = ISINOUT;
		ISERRNO = 0;
		isfd = isopen ((void *)filename, ISINPUT | lmode | vmode);
		if (ISERRNO == EFLOCKED) {
			if (isfd >= 0) {
				isfullclose (isfd);
			}
			return COB_STATUS_61_FILE_SHARING;
		}
		if (isfd >= 0) {
			isfullclose (isfd);
		}
		isam_file_delete (a, f, filename);
		ISERRNO = 0;
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
	for(k=0; k < MAXNUMKEYS; k++)
		fh->idxmap[k] = k;
	ISERRNO = 0;
	fh->lmode = 0;
	if (dobld) {
dobuild:
		if (f->record_min != f->record_max) {
			ISRECLEN = f->record_min;
		}
		vmode |= (omode | lmode | fmode);
		isfd = isbuild ((void *)filename, (int)f->record_max, &fh->key[0], vmode);
		if (isfd < 0) {
			if (ISERRNO == EFLOCKED)
				return COB_STATUS_61_FILE_SHARING;
			if ((ISERRNO == EEXIST || ISERRNO == EBADARG)) {
				/* Erase file and redo the 'isbuild' */
				isam_file_delete (a, f, filename);
				if (f->record_min != f->record_max) {
					ISRECLEN = f->record_min;
				}
				ISERRNO = 0;
				isfd = isbuild ((void *)filename, (int)f->record_max, &fh->key[0], vmode);
				f->flag_file_lock = 1;
			}
		} else {
			f->flag_file_lock = 1;
		}
	} else {
		if (lmode == ISAUTOLOCK
		&& (f->lock_mode & COB_LOCK_MULTIPLE)) {
			lmode = ISMANULOCK;	
		}
		if (lmode == ISMANULOCK) {
			fh->lmode = ISLOCK; 	/* fileio will handle Record locking */
		}
		isfd = isopen_retry (f, (char *)filename, omode | lmode | vmode);
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
			if (fh->nkeys != f->nkeys
			 && f->flag_keycheck) {
				ret = COB_STATUS_39_CONFLICT_ATTRIBUTE;
			} else if (fh->nkeys > f->nkeys) {
				/* More keys in file than COBOL has defined */
				fh2 = cob_malloc (sizeof(struct indexfile) +
						 ((sizeof (struct keydesc)) * (fh->nkeys + 1)));
				memcpy (fh2, fh, sizeof(struct indexfile) +
						((sizeof (struct keydesc)) * (f->nkeys + 1)));
				cob_free (fh);
				fh = fh2;
			}
			if (f->record_max != di.di_recsize) {
				if (f->flag_auto_type) {
					f->record_min = f->record_max = di.di_recsize;
					f->record->size = di.di_recsize;
					if (f->variable_record)
						cob_set_int (f->variable_record, (int) f->record->size);
				} else
				if (f->flag_keycheck
				|| f->record_max < di.di_recsize) {
					ret = COB_STATUS_39_CONFLICT_ATTRIBUTE;
				}
			}
			if (!f->flag_keycheck) {
				/* Copy real ISAM file key information */
				for (k = 0; k < fh->nkeys && !ret; ++k) {
					memset (&fh->key[k], 0, sizeof(struct keydesc));
					isindexinfo (isfd, &fh->key[k], (int)(k+1));
					if (fh->lenkey < indexed_keylen(fh, k)) {
						fh->lenkey = indexed_keylen(fh, k);
					}
				}
				/* Verify that COBOL keys defined match some real ISAM key */
				for (j = 0; j < f->nkeys && !ret; ++j) {
					indexed_keydesc(f, &kd, &f->keys[j]);
					for (k = 0; k < fh->nkeys; ++k) {
						if (indexed_keycmp(&kd, &fh->key[k]) == 0) {
							fh->idxmap[j] = k;
							break;
						}
					}
					if (k >= fh->nkeys) {
						if (mode != COB_OPEN_INPUT
						 || f->access_mode != COB_ACCESS_SEQUENTIAL
						 || !f->flag_auto_type) {
							ret = COB_STATUS_39_CONFLICT_ATTRIBUTE;
						}
					}
				}
			} else {
				for (k = 0; k < fh->nkeys && !ret; ++k) {
					memset (&fh->key[k], 0, sizeof(struct keydesc));
					isindexinfo (isfd, &fh->key[k], (int)(k+1));
					if (fh->lenkey < indexed_keylen(fh, k)) {
						fh->lenkey = indexed_keylen(fh, k);
					}
					/* Verify that COBOL keys match exactly to real ISAM keys */
					len = indexed_keydesc(f, &kd, &f->keys[k]);
					if (fh->lenkey < len) {
						fh->lenkey = len;
					}
					if (indexed_keycmp(&kd, &fh->key[k]) != 0) {
						ret = COB_STATUS_39_CONFLICT_ATTRIBUTE;
						break;
					}
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
		f->flag_was_updated = 1;
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
	fh->isopenmode = omode | lmode | vmode;
	fh->savekey = cob_malloc ((size_t)(fh->lenkey + 1));
	fh->recwrk = cob_malloc ((size_t)(f->record_max + 1));
	/* Active index is unknown at this time */
	f->curkey = -1;
	f->flag_nonexistent = 0;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
#ifndef COB_WITH_STATUS_02
	if (f->flag_read_chk_dups) {
		int k;
		for (k = 0; k < f->nkeys; ++k) {
			if ((fh->key[k].k_flags & ISDUPS))
				break;
		}
		if (k >= f->nkeys)			/* No duplicate keys */
			f->flag_read_chk_dups = 0;
	}
#endif

	return ret;
}

/* Close the INDEXED file */

static int
isam_close (cob_file_api *a, cob_file *f, const int opt)
{
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
}


/* START INDEXED file with positioning */

static int
isam_start (cob_file_api *a, cob_file *f, const int cond, cob_field *key)
{
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
	fh->startfail = 0;
	fh->wrkhasrec = 0;
	if (f->flag_nonexistent) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	k = cob_findkey_attr (f, key, &fullkeylen, &partlen);
	f->mapkey = k;
	if (k < 0) {
		fh->startfail = 1;
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	k = fh->idxmap[k];
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
				fh->startfail = 1;
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
			fh->startfail = 1;
			return fisretsts (COB_STATUS_23_KEY_NOT_EXISTS);
		}
	}
	if (COB_MODULE_PTR->flag_file_format == COB_FILE_IS_MF	/* MF format files */
	 && ISERRNO == 0
	 && (cond == COB_LE || cond == COB_LT)) {
		isread (fh->isfd, (void *)fh->recwrk, ISPREV);
		if (ISERRNO == EENDFILE) {
			f->curkey = -1;
			f->mapkey = -1;
			fh->startcond = -1;
			fh->readdir = -1;
			fh->startiscur = 0;
			fh->startfail = 1;
			return fisretsts (COB_STATUS_23_KEY_NOT_EXISTS);
		}
		isstart (fh->isfd, &fh->key[k], klen, (void *)f->record->data, mode);
	}
	fh->startcond = savecond;
	indexed_savekey(fh, f->record->data, k);
	f->curkey = k;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
	f->flag_first_read = 1;
	return COB_STATUS_00_SUCCESS;
}

/* Random READ of the INDEXED file  */

static int
isam_read (cob_file_api *a, cob_file *f, cob_field *key, const int read_opts)
{
	struct indexfile	*fh;
	int			k,fullkeylen,partlen;
	int			ret;
	int			lmode;

	fh = f->file;
	f->blockpid = 0;
	fh->eofpending = 0;
	fh->startiscur = 0;
	fh->wrkhasrec = 0;
	if (f->flag_nonexistent) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	k = cob_findkey_attr (f, key, &fullkeylen, &partlen);
	if (k < 0) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	f->mapkey = k;
	k = fh->idxmap[k];
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
		if (f->retry_mode == 0
		|| (f->retry_mode & COB_RETRY_FOREVER)) {
			lmode = ISLCKW;		/* ISAM library will wait FOREVER! */
		} else {
			lmode = ISLOCK;		/* isread_retry will handle the retries */
		}
	} else if ((f->lock_mode & COB_LOCK_AUTOMATIC)
		&& (f->open_mode != COB_OPEN_INPUT) ) {
		lmode = ISLOCK;
	}
	if ((read_opts & COB_READ_IGNORE_LOCK)
	 || (read_opts & COB_READ_NO_LOCK) ) {
		lmode &= ~ISLOCK;
	}
	if ((fh->lmode & ISLOCK) 
	 && !(f->lock_mode & COB_LOCK_MULTIPLE)) {
		isrelease (fh->isfd);
	}
	ISERRNO = 0;
	fh->readdir = -1;
	ret = COB_STATUS_00_SUCCESS;
	if (isread_retry (f, (void *)f->record->data, ISEQUAL | lmode)) {
		ret = fisretsts (COB_STATUS_21_KEY_INVALID);
	}
	if ((ret != COB_STATUS_00_SUCCESS)) {
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
	get_isreclen (f);
	return 0;
}

/* Sequential READ of the INDEXED file */

static int
isam_read_next (cob_file_api *a, cob_file *f, const int read_opts)
{
	struct indexfile	*fh;
	int			ret;
	int			lmode, skip_read;
	int			domoveback, dupcheck;

	COB_UNUSED (a);
	fh = f->file;
	ret = COB_STATUS_00_SUCCESS;
	lmode = 0;
	dupcheck = 0;

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
	} else if ((f->lock_mode & COB_LOCK_AUTOMATIC)
	 	&&  f->open_mode != COB_OPEN_INPUT) {
		if (!(read_opts & COB_READ_IGNORE_LOCK)) {
			lmode = ISLOCK;
		}
	}
#ifdef	ISSKIPLOCK
	if ((f->retry_mode & COB_ADVANCING_LOCK)
	 || (read_opts & COB_READ_ADVANCING_LOCK)) {
		lmode |= ISSKIPLOCK;
	}
#endif
	if ((read_opts & COB_READ_IGNORE_LOCK)) {
		lmode &= ~ISLOCK;
	}

	if ((fh->lmode & ISLOCK) 
	 && !(f->lock_mode & COB_LOCK_MULTIPLE)) {
		isrelease (fh->isfd);
	}
	skip_read = ISNEXT;

	ISERRNO = 0;
	ret = COB_STATUS_00_SUCCESS;
	switch (read_opts & COB_READ_MASK) {
	case COB_READ_NEXT:
		if (fh->startfail) {		/* START failed */
			return COB_STATUS_46_READ_ERROR;
		}
		fh->readdir = ISNEXT;
		if (fh->eofpending == ISNEXT) {
			fh->eofpending = 0;
			fh->wrkhasrec = 0;
			return COB_STATUS_10_END_OF_FILE;
		}
		if (fh->startiscur) {
			if (fh->startcond == COB_LA) {
				skip_read = ISPREV;
				if (isread_retry (f, (void *)f->record->data, ISLAST | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			} else if (fh->startcond == COB_FI) {
				if (isread_retry (f, (void *)f->record->data, ISFIRST | lmode)) {
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
				if (isread_retry (f, (void *)f->record->data, ISCURR | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
				dupcheck = 1;
			}
			fh->startcond = -1;
			fh->startiscur = 0;
		} else if (fh->wrkhasrec == ISNEXT) {
			memcpy (f->record->data, fh->recwrk, f->record_max);
			fh->wrkhasrec = 0;
			dupcheck = 1;
			if (fh->lmode & ISLOCK) {
				/* Now lock 'peek ahead' record */
				if (isread_retry (f, (void *)f->record->data, ISCURR | fh->lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			}
		} else {
			if (fh->wrkhasrec == ISPREV) {
				isread (fh->isfd, (void *)f->record->data, ISNEXT);
				fh->wrkhasrec = 0;
			}
			if (isread_retry (f, (void *)f->record->data, ISNEXT | lmode)) {
				ret = fisretsts (COB_STATUS_10_END_OF_FILE);
			}
		}
		break;
	case COB_READ_PREVIOUS:
		if (fh->startfail) {		/* START failed */
			return COB_STATUS_46_READ_ERROR;
		}
		skip_read = ISPREV;
		fh->readdir = ISPREV;
		if (fh->eofpending == ISPREV) {
			fh->eofpending = 0;
			fh->wrkhasrec = 0;
			return COB_STATUS_10_END_OF_FILE;
		}
		if (fh->startiscur) {
			if (fh->startcond == COB_FI) {
				if (isread_retry (f, (void *)f->record->data, ISFIRST | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			} else if (fh->startcond == COB_LA) {
				skip_read = ISPREV;
				if (isread_retry (f, (void *)f->record->data, ISLAST | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			} else if (isread_retry (f, (void *)f->record->data, ISCURR | lmode)) {
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
						isread (fh->isfd, (void *)f->record->data, ISERRNO == 0 ? ISPREV : ISLAST);
						skip_read = ISPREV;
					}
					break;
				case COB_LT:
					isread (fh->isfd, (void *)f->record->data, ISPREV);
					if (ISERRNO == EENDFILE
					 && COB_MODULE_PTR->flag_file_format == COB_FILE_IS_MF) { /* MF format files */
						ret = COB_STATUS_23_KEY_NOT_EXISTS;
						break;
					}
					while (ISERRNO == 0
					&& indexed_cmpkey(fh, f->record->data, f->curkey, 0) >= 0) {
						isread (fh->isfd, (void *)f->record->data, ISPREV);
						skip_read = ISPREV;
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
				if (isread_retry (f, (void *)f->record->data, ISCURR | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
				dupcheck = 1;
			}
			fh->startcond = -1;
			fh->startiscur = 0;
		} else if (fh->wrkhasrec == ISPREV) {
			memcpy (f->record->data, fh->recwrk, f->record_max);
			fh->wrkhasrec = 0;
			dupcheck = 1;
			if (fh->lmode & ISLOCK) {
				/* Now lock 'peek ahead' record */
				if (isread_retry (f, (void *)f->record->data, ISCURR | fh->lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			}
		} else {
			if (fh->wrkhasrec == ISNEXT) {
				isread (fh->isfd, (void *)f->record->data, ISPREV);
				fh->wrkhasrec = 0;
			}
			skip_read = ISPREV;
			if (isread_retry (f, (void *)f->record->data, ISPREV | lmode)) {
				ret = fisretsts (COB_STATUS_10_END_OF_FILE);
			}
		}
		break;
	case COB_READ_FIRST:
		fh->readdir = ISNEXT;
		fh->wrkhasrec = 0;
		if (isread_retry (f, (void *)f->record->data, ISFIRST | lmode)) {
			ret = fisretsts (COB_STATUS_10_END_OF_FILE);
		}
		break;
	case COB_READ_LAST:
		skip_read = ISPREV;
		fh->readdir = ISPREV;
		fh->wrkhasrec = 0;
		if (isread_retry (f, (void *)f->record->data, ISLAST | lmode)) {
			ret = fisretsts (COB_STATUS_10_END_OF_FILE);
		}
		break;
	default:
		fh->readdir = ISNEXT;
		if (fh->wrkhasrec == ISNEXT) {
			memcpy (f->record->data, fh->recwrk, f->record_max);
			fh->wrkhasrec = 0;
		} else
		if (isread_retry (f, (void *)f->record->data, ISNEXT | lmode)) {
			ret = fisretsts (COB_STATUS_10_END_OF_FILE);
		}
		break;
	}
	while (ret == COB_STATUS_51_RECORD_LOCKED
	&& ((f->retry_mode & COB_ADVANCING_LOCK)
	 || (read_opts & COB_READ_ADVANCING_LOCK))) {
		ret = COB_STATUS_00_SUCCESS;
		if (isread_retry (f, (void *)f->record->data, skip_read | lmode)) {
			ret = fisretsts (COB_STATUS_10_END_OF_FILE);
		}
	}
	if ((ret != 0)) {
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
	get_isreclen (f);

#ifdef COB_WITH_STATUS_02
	/* If first read after a START then Read ahead to see if next record has same key value */
	/* This is required as the ISAM handler does not yet know if reading ascending or desc */
	if (dupcheck
	 && skip_read != 0
	 && f->curkey > 0
	 && !f->flag_read_no_02
	 && (fh->key[f->curkey].k_flags & ISDUPS)) {
		indexed_savekey(fh, f->record->data, f->curkey);
		fh->saverecnum = ISRECNUM;
		isread (fh->isfd, (void *)fh->recwrk, skip_read);
		ret = fisretsts (0);

		if (ret == COB_STATUS_00_SUCCESS) {
			if (indexed_cmpkey(fh, (void*)fh->recwrk, f->curkey, 0) == 0) {
				ret = COB_STATUS_02_SUCCESS_DUPLICATE;
				ISSTAT1 = '0';
				ISSTAT2 = '2';
			}
			fh->duprecnum = fh->saverecnum;
			restorefileposition (f);
		} else {
			if (skip_read == ISNEXT) {
				isread (fh->isfd, (void *)fh->recwrk, ISLAST);
			} else {
				isread (fh->isfd, (void *)fh->recwrk, ISFIRST);
			}
			ret = COB_STATUS_00_SUCCESS;
		}
		fh->saverecnum = -1;
		fh->duprecnum = 0;
		fh->wrkhasrec = 0;
		memset (fh->savekey, 0, fh->lenkey);
	}
	return COB_CHECK_DUP (ret);
#else
	/* Read ahead to see if next record has same key value */
	if (ret == COB_STATUS_00_SUCCESS
	 && f->flag_read_chk_dups
	 && fh->readdir != 0
	 && f->curkey > 0
	 && (fh->key[f->curkey].k_flags & ISDUPS)) {
		indexed_savekey(fh, f->record->data, f->curkey);
		fh->saverecnum = ISRECNUM;
		isread (fh->isfd, (void *)fh->recwrk, fh->readdir);
		ret = fisretsts (0);
		if (ret == COB_STATUS_00_SUCCESS) {
			if (indexed_cmpkey(fh, (void*)fh->recwrk, f->curkey, 0) == 0) {
				ret = COB_STATUS_02_SUCCESS_DUPLICATE;
				ISSTAT1 = '0';
				ISSTAT2 = '2';
			}
			fh->duprecnum = fh->saverecnum;
			restorefileposition (f);
		} else {
			if (fh->readdir == ISNEXT) {
				isread (fh->isfd, (void *)fh->recwrk, ISLAST);
			} else {
				isread (fh->isfd, (void *)fh->recwrk, ISFIRST);
			}
			ret = COB_STATUS_00_SUCCESS;
		}
		fh->saverecnum = -1;
		fh->duprecnum = 0;
		fh->wrkhasrec = 0;
		memset(fh->savekey, 0, fh->lenkey);
	} else {
		ret = COB_CHECK_DUP (ret);
	}
	return ret;
#endif
}

/* WRITE to the INDEXED file  */

static int
isam_write (cob_file_api *a, cob_file *f, const int opt)
{
	struct indexfile	*fh;
	int			ret, retdup;

	COB_UNUSED (a);
	fh = f->file;
	if (f->flag_nonexistent) {
		return COB_STATUS_48_OUTPUT_DENIED;
	}
	if (f->access_mode == COB_ACCESS_SEQUENTIAL
	 && f->open_mode == COB_OPEN_OUTPUT
	 && !f->flag_set_isam
	 && indexed_cmpkey(fh, f->record->data, 0, 0) <= 0) {
		return COB_STATUS_21_KEY_INVALID;
	}
	retdup = ret = COB_STATUS_00_SUCCESS;

#ifndef COB_WITH_STATUS_02
	if (f->flag_read_chk_dups) {
		int k;
		savefileposition (f);
		for (k = 0; k < f->nkeys; ++k) {
			if (fh->key[k].k_flags & ISDUPS) {
				memcpy (fh->recwrk, f->record->data, f->record_max);
				isstart (fh->isfd, &fh->key[k], fh->key[k].k_len, 
						(void *)fh->recwrk, ISEQUAL);
				if (!isread (fh->isfd, (void *)fh->recwrk, ISEQUAL)) {
					retdup = COB_STATUS_02_SUCCESS_DUPLICATE;
					break;
				}
			}
		}
		restorefileposition (f);
	}
#endif
	set_isreclen (f);
	if ((opt & COB_WRITE_LOCK)
	 && !(f->lock_mode & COB_LOCK_AUTOMATIC) 
	 && !f->flag_file_lock) {
		/* WRITE and make it 'current' */
		if ((iswrcurr (fh->isfd, (void *)f->record->data))) {
			return fisretsts (COB_STATUS_49_I_O_DENIED);
		}
		ret = COB_CHECK_DUP (ret);
		/* Then read placing lock on the record */
		if (isread_retry (f, (void *)f->record->data, ISCURR | ISLOCK)) {
			return fisretsts (COB_STATUS_49_I_O_DENIED);
		}
	} else {
		if ((iswrite (fh->isfd, (void *)f->record->data))) {
			if (f->access_mode == COB_ACCESS_SEQUENTIAL
			 && f->open_mode == COB_OPEN_OUTPUT
			 && f->flag_set_isam
			 && ISERRNO == EDUPL) {
				return COB_STATUS_21_KEY_INVALID;
			}
			return fisretsts (COB_STATUS_49_I_O_DENIED);
		}
		ret = COB_CHECK_DUP (ret);
	}
	indexed_savekey(fh, f->record->data, 0);

	if (ret == COB_STATUS_00_SUCCESS
	 && retdup != COB_STATUS_00_SUCCESS)
		ret = retdup;
	return ret;
}


/* DELETE record from the INDEXED file  */

static int
isam_delete (cob_file_api *a, cob_file *f)
{
	struct indexfile	*fh;
	int			ret;

	COB_UNUSED (a);
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
		fh->saverecnum = -1;
	} else {
		savefileposition (f);
		if (f->curkey != 0) {
			/* Switch to primary index */
			isstart (fh->isfd, &fh->key[0], 0,
				 	(void *)f->record->data, ISEQUAL);
		}
	}
	ISERRNO = 0;
	if (isread_retry (f, (void *)f->record->data, ISEQUAL | ISLOCK)) {
		ret = fisretsts (COB_STATUS_21_KEY_INVALID);
	} else {
		if (isdelete (fh->isfd, (void *)f->record->data)) {
			ret = fisretsts (COB_STATUS_49_I_O_DENIED);
		}
	}
	restorefileposition (f);
	if (!(f->lock_mode & COB_LOCK_MULTIPLE)) {
		isrelease (fh->isfd);
	}
	return ret;
}

/* REWRITE record to the INDEXED file  */

static int
isam_rewrite (cob_file_api *a, cob_file *f, const int opt)
{
	struct indexfile	*fh;
	int			k;
	int			ret, retdup;
#ifndef COB_WITH_STATUS_02
	int			svky;
#endif

	COB_UNUSED (a);
	fh = f->file;
	retdup = ret = COB_STATUS_00_SUCCESS;
	if (f->flag_nonexistent) {
		return COB_STATUS_49_I_O_DENIED;
	}

	if (f->access_mode == COB_ACCESS_SEQUENTIAL
	&&  indexed_cmpkey(fh, f->record->data, 0, 0) != 0) {
		return COB_STATUS_21_KEY_INVALID;
	}
	if (fh->recorg == NULL) {
		fh->recorg = cob_malloc ((size_t)(f->record_max + 1));
	}

#ifndef COB_WITH_STATUS_02
	svky = f->curkey;
	if (f->flag_read_chk_dups
	 || f->curkey >= 0) { 	
		if (f->curkey < 0) {
			isstart (fh->isfd, &fh->key[0], fh->key[0].k_len, (void *)f->record->data, ISEQUAL);
			f->curkey = 0;
		}
#else
	if (f->curkey >= 0) {
#endif
		/* Index is active */
		/* Save record data */
		memcpy (fh->recwrk, f->record->data, f->record_max);
		fh->readdir = ISNEXT;
		savefileposition (f);
		memcpy (fh->recwrk, f->record->data, f->record_max);
		if (f->curkey != 0) {
			/* Activate primary index */
			isstart (fh->isfd, &fh->key[0], fh->key[0].k_len, (void *)fh->recwrk, ISEQUAL);
		}
		/* Verify record exists */
		if (isread (fh->isfd, (void *)fh->recwrk, ISEQUAL)) {
			restorefileposition (f);
			return COB_STATUS_21_KEY_INVALID;
		}
		fh->duprecnum = ISRECNUM;
		if (fh->recorg != NULL)
			memcpy (fh->recorg, fh->recwrk, f->record_max);
		for (k = 0; k < f->nkeys && ret == COB_STATUS_00_SUCCESS; ++k) {
			if (fh->key[k].k_flags & ISDUPS) {
#ifndef COB_WITH_STATUS_02
				if (f->flag_read_chk_dups
				 && retdup == COB_STATUS_00_SUCCESS) {
					/* If new record did not change this key then skip check */
					indexed_savekey(fh, (void*)fh->recorg, k);
					if (indexed_cmpkey(fh, (void *)f->record->data, k, 0) == 0) {
						continue;
					}

					memcpy (fh->recwrk, f->record->data, f->record_max);
					indexed_savekey(fh, (void*)fh->recwrk, k);
					isstart (fh->isfd, &fh->key[k], fh->key[k].k_len, 
							(void *)fh->recwrk, ISEQUAL);
					while (ISERRNO == 0) {
						if (isread (fh->isfd, (void *)fh->recwrk, ISNEXT))
							break;
						if(ISRECNUM == fh->duprecnum)
							continue;
						if (indexed_cmpkey(fh, (void *)fh->recwrk, k, 0) == 0) {
							retdup = COB_STATUS_02_SUCCESS_DUPLICATE;
							break;
						}
					}
					f->curkey = svky;
				}
#endif
				ret = COB_STATUS_00_SUCCESS;
			} else 
			if (k > 0) {
				memcpy (fh->recwrk, f->record->data, f->record_max);
				ISERRNO = 0;
				isstart (fh->isfd, &fh->key[k], fh->key[k].k_len, (void *)fh->recwrk, ISEQUAL);
				if (ISERRNO == 0
				 && isread (fh->isfd, (void *)fh->recwrk, ISEQUAL) == 0
				 && ISRECNUM != fh->recnum) {
					ret = COB_STATUS_22_KEY_EXISTS;
					break;
				}
			}
		}
		fh->duprecnum = 0;
		if (ret == COB_STATUS_00_SUCCESS) {
			memcpy (fh->recwrk, f->record->data, f->record_max);
			isstart (fh->isfd, &fh->key[0], 0, (void *)fh->recwrk, ISEQUAL);
			if (isread_retry (f, (void *)fh->recwrk, ISEQUAL | ISLOCK)) {
				ret = fisretsts (COB_STATUS_49_I_O_DENIED);
			} else {
				set_isreclen (f);
				if (isrewcurr (fh->isfd, (void *)f->record->data)) {
					ret = fisretsts (COB_STATUS_49_I_O_DENIED);
				}
				ret = COB_CHECK_DUP (ret);
			}
		}

		ret = COB_CHECK_DUP (ret);
		restorefileposition (f);
		if (ret == COB_STATUS_00_SUCCESS
		 && retdup != COB_STATUS_00_SUCCESS)
			ret = retdup;

	} else {

		memcpy (fh->recwrk, f->record->data, f->record_max);
		if (isread_retry (f, (void *)fh->recwrk, ISEQUAL | ISLOCK)) {
			ret = fisretsts (COB_STATUS_49_I_O_DENIED);
		} else {
			set_isreclen (f);
			if (isrewrite (fh->isfd, (void *)f->record->data)) {
				ret = fisretsts (COB_STATUS_49_I_O_DENIED);
			}
			ret = COB_CHECK_DUP (ret);
		}
		ret = COB_CHECK_DUP (ret);
	}
	if (!ret) {
		ret = COB_CHECK_DUP (ret);
		if ((f->lock_mode & COB_LOCK_AUTOMATIC)) {
			if (!(f->lock_mode & COB_LOCK_MULTIPLE)) {
				isrelease (fh->isfd);
			}
		} else {
			if (!(f->lock_mode & COB_LOCK_MULTIPLE)) {
				if (!(opt & COB_WRITE_LOCK)) {
					isrelease (fh->isfd);
				}
			} else
			if ((opt & COB_WRITE_NO_LOCK)) {
				isrelease (fh->isfd);
			}
		}
	} else if (ret) {
		isrelease (fh->isfd);
	}
	return ret;
}

static void
cob_isam_exit_fileio (cob_file_api *a)
{
	COB_UNUSED (a);
#ifndef	WITH_DISAM
	(void)iscleanup ();
#endif
}

void
cob_isam_init_fileio (cob_file_api *a)
{
#ifdef NPARTS
	if (max_keycomp > NPARTS)
		max_keycomp = NPARTS;
#endif
#ifdef ISMAXPARTS
	if (max_keycomp > ISMAXPARTS)
		max_keycomp = ISMAXPARTS;
#endif
#if defined(WITH_DISAM)
	a->io_funcs[COB_IO_DISAM] = (void*) &ext_indexed_funcs;
	isam_name = "D-ISAM";
#elif defined(WITH_CISAM)
	a->io_funcs[COB_IO_CISAM] = (void*) &ext_indexed_funcs;
	isam_name = "C-ISAM";
#elif defined(WITH_VISAM)
	a->io_funcs[COB_IO_VISAM] = (void*) &ext_indexed_funcs;
	isam_name = "V-ISAM";
#ifdef VB_RTD
	if (vbisam_rtd == NULL) {	/* VBISAM 2.2 run-time pointer */
		vbisam_rtd = VB_GET_RTD;
	}
#endif
#elif defined(WITH_VBISAM)
	a->io_funcs[COB_IO_VBISAM] = (void*) &ext_indexed_funcs;
	isam_name = "VBISAM";
#ifdef VB_RTD
	if (vbisam_rtd == NULL) {	/* VBISAM 2.1.1 run-time pointer */
		vbisam_rtd = VB_GET_RTD;
	}
#endif
#endif
	isam_globptr = a->glbptr;
	isam_setptr = a->setptr;
}

#endif

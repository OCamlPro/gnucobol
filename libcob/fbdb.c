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


#include "fileio.h"

#ifdef WITH_DB

#include <db.h>

void cob_bdb_init_fileio (cob_file_api *a);

/* Local variables */

static int ix_bdb_sync		(cob_file_api *,cob_file *f);
static int ix_bdb_open		(cob_file_api *,cob_file *, char *, const int, const int);
static int ix_bdb_close		(cob_file_api *,cob_file *, const int);
static int ix_bdb_start		(cob_file_api *,cob_file *, const int, cob_field *);
static int ix_bdb_read		(cob_file_api *,cob_file *, cob_field *, const int);
static int ix_bdb_read_next	(cob_file_api *,cob_file *, const int);
static int ix_bdb_write		(cob_file_api *,cob_file *, const int);
static int ix_bdb_delete	(cob_file_api *,cob_file *);
static int ix_bdb_file_delete(cob_file_api *, cob_file *, char *);
static int ix_bdb_rewrite	(cob_file_api *,cob_file *, const int);
static int ix_bdb_file_unlock (cob_file_api *a, cob_file *f);
static void ix_bdb_exit_fileio (cob_file_api *a);
static int ix_bdb_fork (cob_file_api *a);
static char * ix_bdb_version (void);

static int ix_bdb_dummy () { return 0; }

static const struct cob_fileio_funcs ext_indexed_funcs = {
	ix_bdb_open,
	ix_bdb_close,
	ix_bdb_start,
	ix_bdb_read,
	ix_bdb_read_next,
	ix_bdb_write,
	ix_bdb_rewrite,
	ix_bdb_delete,
	ix_bdb_file_delete,
	cob_bdb_init_fileio,
	ix_bdb_exit_fileio,
	ix_bdb_fork,
	ix_bdb_sync,
	(void*)ix_bdb_dummy,	/* commit */
	(void*)ix_bdb_dummy,	/* rollback */
	ix_bdb_file_unlock,
	ix_bdb_version
};

static DB_ENV	*bdb_env = NULL;
static char		*bdb_home_dir = NULL;
static char		*bdb_buff = NULL;
static const char	**bdb_data_dir = NULL;
static void		*record_lock_object = NULL;
static size_t	rlo_size = 0;
static unsigned int	bdb_lock_id = 0;
static int		bdb_join = 1;

#define DB_PUT(db,flags)	db->put (db, NULL, &p->key, &p->data, flags)
#define DB_GET(db,flags)	db->get (db, NULL, &p->key, &p->data, flags)
#define DB_DEL(db,key,flags)	db->del (db, NULL, key, flags)
#define DB_CLOSE(db)		db->close (db, 0)
#define DB_SYNC(db)		db->sync (db, 0)
#if (DB_VERSION_MAJOR > 4) || ((DB_VERSION_MAJOR == 4) && (DB_VERSION_MINOR > 6))
#define DB_SEQ(db,flags)	db->get (db, &p->key, &p->data, flags)
#define DB_CPUT(db,flags)	db->put (db, &p->key, &p->data, flags)
#define DB_CDEL(db,flags)	db->del (db, flags)
#else
#define DB_SEQ(db,flags)	db->c_get (db, &p->key, &p->data, flags)
#define DB_CPUT(db,flags)	db->c_put (db, &p->key, &p->data, flags)
#define DB_CDEL(db,flags)	db->c_del (db, flags)
#endif
#define	cob_dbtsize_t		u_int32_t

#define	COB_DUPSWAP(x)		bdb_dupswap(f,(unsigned int)(x))

#define DBT_SET(key,fld)			\
	key.data = fld->data;			\
	key.size = (cob_dbtsize_t) fld->size
#define COB_MAX_BDB_LOCKS 32

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
	int		start_cond;
	int		key_index;
	unsigned int	bdb_lock_id;
	int		write_cursor_open;
	int		filenamelen;
	int		bdb_lock_num;
	int		bdb_lock_max;
	int		file_lock_set;
	DB_LOCK		bdb_file_lock;
	DB_LOCK		bdb_record_lock;
	DB_LOCK		*bdb_locks;
};

static unsigned int
bdb_dupswap (cob_file *f, unsigned int value)
{
	if (!f->flag_little_endian
	 && !f->flag_big_endian)
		return ((unsigned int)(value));	/* Native format */
#if	defined(WORDS_BIGENDIAN)
	if (f->flag_little_endian)
		return (COB_BSWAP_32((unsigned int)(value)));	/* big */
	return ((unsigned int)(value));
#else
	if (f->flag_big_endian)
		return (COB_BSWAP_32((unsigned int)(value)));	/* little */
	return ((unsigned int)(value));
#endif
}

static void
bdb_setkey (cob_file *f, int idx)
{
	struct indexed_file	*p;
	int	len;

	p = f->file;
	memset (p->savekey, 0, p->maxkeylen);
	len = db_savekey (f, p->savekey, f->record->data, idx);
	memset(&p->key,0,sizeof(p->key));
	p->key.data = p->savekey;
	p->key.size = (cob_dbtsize_t) len;
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
	len = db_savekey(f, p->suppkey, f->record->data, idx);
	for (i = 0; i < len; i++) {
		if (p->suppkey[i] != ch_sprs)
			return 0;
	}
	return 1;
}

/* Open the 'write cursor' if needed and return 0 is already open */
static int
bdb_open_cursor(cob_file *f, int for_write)
{
	struct indexed_file	*p;
	int		flags;

	p = f->file;
	if(p->write_cursor_open)
		return 0;		/* It is already open */
	if (bdb_env && for_write) {
		flags = DB_WRITECURSOR;
	} else {
		flags = 0;
	}
	p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], flags);
	p->write_cursor_open = 1;
	return 1;
}

/* Close the 'write cursor' if needed and return 0 is already closed */
static int
bdb_close_cursor(cob_file *f)
{
	struct indexed_file	*p;

	p = f->file;
	p->write_cursor_open = 0;
	if(p->cursor[0] == NULL)
		return 0;		/* It is already closed */
#if (DB_VERSION_MAJOR > 4) || ((DB_VERSION_MAJOR == 4) && (DB_VERSION_MINOR > 6))
	p->cursor[0]->close (p->cursor[0]);
#else
	p->cursor[0]->c_close (p->cursor[0]);
#endif
	p->cursor[0] = NULL;
	return 1;
}

/* Close the 'cursor' on a specific index */
static int
bdb_close_index(cob_file *f, int index)
{
	struct indexed_file	*p;

	p = f->file;
	if(p->cursor[index] == NULL)
		return 0;		/* It is already closed */
#if (DB_VERSION_MAJOR > 4) || ((DB_VERSION_MAJOR == 4) && (DB_VERSION_MINOR > 6))
	p->cursor[index]->close (p->cursor[index]);
#else
	p->cursor[index]->c_close (p->cursor[index]);
#endif
	p->cursor[index] = NULL;
	return 1;
}


/* Local functions */

static int
ix_bdb_sync (cob_file_api *a, cob_file *f)
{
	struct indexed_file	*p;
	int			i;
	COB_UNUSED(a);

	if (f->organization == COB_ORG_INDEXED) {
		p = f->file;
		if (p) {
			for (i = 0; i < (int)f->nkeys; ++i) {
				if (p->db[i]) {
					DB_SYNC (p->db[i]);
				}
			}
		}
	}
	return 0;
}

static char *
ix_bdb_version (void)
{
	int	major, minor, patch;
	static char buff[64];
	major = 0, minor = 0, patch = 0;
	db_version (&major, &minor, &patch);
	if (major == DB_VERSION_MAJOR 
	 && minor == DB_VERSION_MINOR) {
		snprintf (buff, 55, _("BDB, version %d.%d.%d"),
							major, minor, patch);
	} else {
		snprintf (buff, 55, _("BDB, version %d.%d.%d (compiled with %d.%d)"),
							major, minor, patch, 
							DB_VERSION_MAJOR, DB_VERSION_MINOR);
	}
	return buff;
}

/* INDEXED */

static int bdb_err_tear_down = 0;

#if (DB_VERSION_MAJOR > 4)
static void
bdb_err_event (DB_ENV *env, u_int32_t event, void *info)
{
	const char	*msg = NULL;
	COB_UNUSED (env);
	COB_UNUSED (info);
	
	if (bdb_err_tear_down) return;

	switch (event) {
#ifdef DB_EVENT_FAILCHK_PANIC
	case DB_EVENT_FAILCHK_PANIC:
		msg = "FailChk_Panic";
		/* fall-thru */
#endif
#ifdef DB_EVENT_PANIC
	case DB_EVENT_PANIC:
		if (msg != NULL) msg = "Panic";
		/* unset BDB environment as we cannot do anything with it any more */
		bdb_env = NULL;
		bdb_err_tear_down = 1;
		break;
#endif
#ifdef DB_EVENT_EVENT_MUTEX_DIED
	case DB_EVENT_MUTEX_DIED:
		msg = "Mutex Died"; break;
#endif
#ifdef  DB_EVENT_WRITE_FAILED 
	case DB_EVENT_WRITE_FAILED:
		msg = "WriteFailed"; break;
#endif
	default: msg = "unknown"; break;
	}
	cob_runtime_error (_("BDB (%s), error: %d %s"), "fatal error", event, msg);
	cob_stop_run (1);
}
#endif

static void
join_environment (cob_file_api *a)
{
	cob_u32_t	flags;
	int		ret, k;

	if (file_setptr->bdb_home == NULL) {
		/* Default to the current directory */
		file_setptr->bdb_home = strdup(".");
	} else if (file_setptr->bdb_home[0] <= ' '
		|| strcasecmp(file_setptr->bdb_home,"no") == 0
		|| strcasecmp(file_setptr->bdb_home,"false") == 0) {
		/* This effectively disables record/file locking */
		/* But prevents the BDB control files from being created */
		return;
	}
	if(file_setptr->bdb_home)
		bdb_home_dir = cob_strdup(file_setptr->bdb_home);
	ret = db_env_create (&bdb_env, 0);
	if (ret) {
		cob_runtime_error (_("cannot join BDB environment (%s), error: %d %s"),
				   "env_create", ret, db_strerror (ret));
		cob_stop_run (1);
	}
	bdb_env->set_errfile (bdb_env, stderr);
#if (DB_VERSION_MAJOR > 4) || ((DB_VERSION_MAJOR == 4) && (DB_VERSION_MINOR > 2))
	bdb_env->set_msgfile (bdb_env, stderr);
	if (a->file_paths) {
		for(k=0; a->file_paths[k] != NULL; k++) {
			ret = bdb_env->set_data_dir (bdb_env, a->file_paths[k]);
		}
	}
#endif
	bdb_env->set_cachesize (bdb_env, 0, 2*1024*1024, 0);
	bdb_env->set_alloc (bdb_env, cob_malloc, realloc, cob_free);
	flags = DB_CREATE | DB_INIT_MPOOL | DB_INIT_CDB;
	ret = bdb_env->open (bdb_env, file_setptr->bdb_home, flags, 0);
	if (ret) {
		cob_runtime_error (_("cannot join BDB environment (%s), error: %d %s"),
				   "env->open", ret, db_strerror (ret));
		bdb_env->close (bdb_env, 0);
		bdb_env = NULL;
		cob_stop_run (1);
	}
#if (DB_VERSION_MAJOR > 4) || ((DB_VERSION_MAJOR == 4) && (DB_VERSION_MINOR > 1))
	bdb_env->get_data_dirs (bdb_env, &bdb_data_dir);
#endif
	bdb_env->lock_id (bdb_env, &bdb_lock_id);
	bdb_env->set_lk_detect (bdb_env, DB_LOCK_DEFAULT);
#if (DB_VERSION_MAJOR > 4)
	bdb_env->set_event_notify(bdb_env, (void*)bdb_err_event);
#endif
}

/* Impose lock on 'file' using BDB locking */
static int
bdb_lock_file (cob_file *f, char *filename, int lock_mode)
{
	struct indexed_file	*p;
	int			ret, retry, interval;
	DBT			dbt;

	if (bdb_env == NULL) 
		return 0;
	p = f->file;
	ret = 0;
	p->file_lock_set = 0;
	retry = interval = 0;
	if ((f->retry_mode & COB_RETRY_FOREVER)) {
		retry = -1;
	} else
	if ((f->retry_mode & COB_RETRY_SECONDS)) {
		retry = 1;
		interval = f->retry_seconds>0?f->retry_seconds:
			(file_setptr->cob_retry_seconds>0?file_setptr->cob_retry_seconds:1);
	} else
	if ((f->retry_mode & COB_RETRY_TIMES)) {
		retry = f->retry_times>0?f->retry_times:
			(file_setptr->cob_retry_times>0?file_setptr->cob_retry_times:1);
		interval = file_setptr->cob_retry_seconds>0?file_setptr->cob_retry_seconds:1;
	}
	if(retry > 0) {
		retry = retry * interval * COB_RETRY_PER_SECOND ;
		interval = 1000 / COB_RETRY_PER_SECOND ;
	}
	do {
		memset(&dbt,0,sizeof(dbt));
		dbt.size = (cob_dbtsize_t) strlen (filename);
		dbt.data = filename;
		ret = bdb_env->lock_get (bdb_env, bdb_lock_id, DB_LOCK_NOWAIT,
					&dbt, lock_mode, &p->bdb_file_lock);
		if (ret == 0) {
			p->file_lock_set = 1;
			break;
		}
		if (ret == DB_LOCK_DEADLOCK)
			return COB_STATUS_52_DEAD_LOCK;
		if(ret != DB_LOCK_NOTGRANTED) {
			break;
		}
		if (retry > 0) {
			retry--;
			cob_sleep_msec(interval);
		}
	} while (ret != 0 && retry != 0);

	if(ret == DB_LOCK_NOTGRANTED) 
		return COB_STATUS_61_FILE_SHARING;
	if (ret) {
		cob_runtime_error (_("BDB (%s), error: %d %s"),
				   "file lock_get", ret, db_strerror (ret));
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	return ret;
}

/* Impose lock on record and table it */
static int
bdb_lock_record (cob_file *f, const char *key, const unsigned int keylen)
{
	struct indexed_file	*p;
	size_t			len;
	int			j, k, ret, retry, interval;
	DBT			dbt;

	if (bdb_env == NULL) 
		return 0;
	p = f->file;
	ret = 0;
	retry = interval = 0;
	if ((f->retry_mode & COB_RETRY_FOREVER)) {
		retry = -1;
	} else
	if ((f->retry_mode & COB_RETRY_SECONDS)) {
		retry = 1;
		interval = f->retry_seconds>0?f->retry_seconds:
			(file_setptr->cob_retry_seconds>0?file_setptr->cob_retry_seconds:1);
	} else
	if ((f->retry_mode & COB_RETRY_TIMES)) {
		retry = f->retry_times>0?f->retry_times:
			(file_setptr->cob_retry_times>0?file_setptr->cob_retry_times:1);
		interval = file_setptr->cob_retry_seconds>0?file_setptr->cob_retry_seconds:1;
	}

	len = keylen + p->filenamelen + 1;
	if (len > rlo_size) {
		cob_free (record_lock_object);
		record_lock_object = cob_malloc (len);
		rlo_size = len;
	}
	memcpy ((char *)record_lock_object, p->filename, (size_t)(p->filenamelen + 1));
	memcpy ((char *)record_lock_object + p->filenamelen + 1, key, (size_t)keylen);

	if(retry > 0) {
		retry = retry * interval * COB_RETRY_PER_SECOND;
		interval = 1000 / COB_RETRY_PER_SECOND;
	}

	do {
		memset(&dbt,0,sizeof(dbt));
		dbt.size = (cob_dbtsize_t) len;
		dbt.data = record_lock_object;
		ret = bdb_env->lock_get (bdb_env, bdb_lock_id, retry==-1?0:DB_LOCK_NOWAIT,
					&dbt, DB_LOCK_WRITE, &p->bdb_record_lock);
		if (ret == 0)
			break;
		if (ret == DB_LOCK_DEADLOCK)
			return COB_STATUS_52_DEAD_LOCK;
		if(ret != DB_LOCK_NOTGRANTED) {
			break;
		}
		if (retry > 0) {
			retry--;
			cob_sleep_msec(interval);
		}
	} while (ret != 0 && retry != 0);

	if (!ret) {
		if (p->bdb_lock_max == 0) {
			p->bdb_lock_max = COB_MAX_BDB_LOCKS;
			p->bdb_locks = cob_malloc(p->bdb_lock_max * sizeof(DB_LOCK));
			p->bdb_lock_num = 0;
		}
		if (p->bdb_lock_num+1 >= p->bdb_lock_max) {
			p->bdb_locks = cob_realloc(p->bdb_locks, p->bdb_lock_max, 
							(p->bdb_lock_max + COB_MAX_BDB_LOCKS) * sizeof(DB_LOCK));
			p->bdb_lock_max += COB_MAX_BDB_LOCKS;
		}
		for(k = 0; k < p->bdb_lock_num; k++) {
			if (memcmp(&p->bdb_record_lock, &p->bdb_locks[k], sizeof(DB_LOCK)) == 0) {
				/* Move to end of lock table for later: bdb_unlock_last */
				for (j=k;  j < p->bdb_lock_num; j++) {
					memcpy (&p->bdb_locks[j], &p->bdb_locks[j+1], sizeof(DB_LOCK));
				}
				memcpy (&p->bdb_locks[p->bdb_lock_num-1], &p->bdb_record_lock, sizeof(DB_LOCK));
				/* Release lock just acquired as it is a duplicate */
				ret = bdb_env->lock_put (bdb_env, &p->bdb_record_lock);
				return ret;
			}
		}
		if (p->bdb_lock_num < p->bdb_lock_max) {
			p->bdb_locks [ p->bdb_lock_num++ ] = p->bdb_record_lock;
		}
	}
	if(ret == DB_LOCK_NOTGRANTED) 
		return COB_STATUS_51_RECORD_LOCKED;
	if (ret) {
		cob_runtime_error (_("BDB (%s), error: %d %s"),
				   "lock_get", ret, db_strerror (ret));
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	return ret;
}

static int
bdb_test_record_lock (cob_file *f, const char *key, const unsigned int keylen)
{
	struct indexed_file	*p;
	size_t			len;
	int			j, k, ret, retry, interval;
	DBT			dbt;
	DB_LOCK			test_lock;

	if (bdb_env == NULL) 
		return 0;
	p = f->file;
	ret = 0;
	retry = interval = 0;
	if ((f->retry_mode & COB_RETRY_FOREVER)) {
		retry = -1;
	} else
	if ((f->retry_mode & COB_RETRY_SECONDS)) {
		retry = 1;
		interval = f->retry_seconds>0?f->retry_seconds:
			(file_setptr->cob_retry_seconds>0?file_setptr->cob_retry_seconds:1);
	} else
	if ((f->retry_mode & COB_RETRY_TIMES)) {
		retry = f->retry_times>0?f->retry_times:
			(file_setptr->cob_retry_times>0?file_setptr->cob_retry_times:1);
		interval = file_setptr->cob_retry_seconds>0?file_setptr->cob_retry_seconds:1;
	}
	len = keylen + p->filenamelen + 1;
	if (len > rlo_size) {
		cob_free (record_lock_object);
		record_lock_object = cob_malloc (len);
		rlo_size = len;
	}
	memcpy ((char *)record_lock_object, p->filename, (size_t)(p->filenamelen + 1));
	memcpy ((char *)record_lock_object + p->filenamelen + 1, key, (size_t)keylen);
	memset(&test_lock,0,sizeof(test_lock));
	if(retry > 0) {
		retry = retry * interval * COB_RETRY_PER_SECOND ;
		interval = 1000 / COB_RETRY_PER_SECOND ;
	}
	do {
		memset(&dbt,0,sizeof(dbt));
		dbt.size = (cob_dbtsize_t) len;
		dbt.data = record_lock_object;
		ret = bdb_env->lock_get (bdb_env, bdb_lock_id, DB_LOCK_NOWAIT,
					&dbt, DB_LOCK_WRITE, &test_lock);
		if (ret == 0)
			break;
		if (ret == DB_LOCK_DEADLOCK)
			return COB_STATUS_52_DEAD_LOCK;
		if(ret != DB_LOCK_NOTGRANTED) {
			break;
		}
		if (retry > 0) {
			retry--;
			cob_sleep_msec(interval);
		}
	} while (ret != 0 && retry != 0);

	if (!ret) {
		if (p->bdb_lock_num > 0) {
			for(k = 0; k < p->bdb_lock_num; k++) {
				if (memcmp(&test_lock, &p->bdb_locks[k], sizeof(DB_LOCK)) == 0) {
					/* Move to end of lock table for later: bdb_unlock_last */
					for (j=k;  j < p->bdb_lock_num; j++) {
						memcpy (&p->bdb_locks[j], &p->bdb_locks[j+1], sizeof(DB_LOCK));
					}
					memcpy (&p->bdb_locks[p->bdb_lock_num-1], &test_lock, sizeof(DB_LOCK));
					break;
				}
			}
		}
		ret = bdb_env->lock_put (bdb_env, &test_lock);/* Release lock just acquired */
	}
	if(ret == DB_LOCK_NOTGRANTED) 
		return COB_STATUS_51_RECORD_LOCKED;
	if (ret) {
		cob_runtime_error (_("BDB (%s), error: %d %s"),
				   "lock_get", ret, db_strerror (ret));
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	return ret;
}

static int
bdb_unlock_all (cob_file *f)
{
	struct indexed_file	*p;
	int			ret = 0, k;

	p = f->file;
	if (p->bdb_lock_num == 0
	 || bdb_env == NULL) {
		return 0;
	}
	if (p->bdb_lock_num > 0) {
		for (k=p->bdb_lock_num-1; k >= 0; k--) {
			ret = bdb_env->lock_put (bdb_env, &p->bdb_locks[k]);
		}
		p->bdb_lock_num = 0;
	} else {
		ret = bdb_env->lock_put (bdb_env, &p->bdb_record_lock);
	}
	if (ret) {
		cob_runtime_error (_("BDB (%s), error: %d %s"),
				   "lock_put", ret, db_strerror (ret));
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	return ret;
}

static int
bdb_unlock_last (cob_file *f)
{
	struct indexed_file	*p;
	int			ret = 0;

	p = f->file;
	if (p->bdb_lock_num == 0
	 || bdb_env == NULL) {
		return 0;
	}
	if (p->bdb_lock_num > 0) {
		p->bdb_lock_num--;
		ret = bdb_env->lock_put (bdb_env, &p->bdb_locks[p->bdb_lock_num]);
	}
	if (ret) {
		cob_runtime_error (_("BDB (%s), error: %d %s"),
				   "lock_put", ret, db_strerror (ret));
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	return ret;
}

static int
bdb_test_lock_advance(cob_file *f, int nextprev, int skip_lock)
{
	struct indexed_file	*p;
	int			ret;

	p = f->file;
	ret = bdb_test_record_lock (f, p->key.data, p->key.size);
	while (ret == COB_STATUS_51_RECORD_LOCKED
	   &&  skip_lock) {
		ret = DB_SEQ (p->cursor[p->key_index], nextprev);
		if (ret == DB_NOTFOUND)
			return COB_STATUS_10_END_OF_FILE;
		if (!ret) {
			ret = bdb_test_record_lock (f, p->key.data, p->key.size);
		}
	}
	return ret;
}

static int
bdb_lock_advance(cob_file *f, int nextprev, int skip_lock)
{
	struct indexed_file	*p;
	int			ret;

	p = f->file;
	ret = bdb_lock_record (f, p->key.data, p->key.size);
	while (ret == COB_STATUS_51_RECORD_LOCKED
	   &&  skip_lock) {
		ret = DB_SEQ (p->cursor[p->key_index], nextprev);
		if (ret == DB_NOTFOUND)
			return COB_STATUS_10_END_OF_FILE;
		if (!ret) {
			ret = bdb_test_record_lock (f, p->key.data, p->key.size);
		}
	}
	return ret;
}

/* Get the next number in a set of duplicates */
static unsigned int
get_dupno (cob_file *f, const cob_u32_t i)
{
	struct indexed_file	*p;
	int			ret;
	unsigned int		dupno;

	p = f->file;
	dupno = 0;
	bdb_setkey(f, i);
	memcpy (p->temp_key, p->key.data, (size_t)p->maxkeylen);
	p->db[i]->cursor (p->db[i], NULL, &p->cursor[i], 0);
	ret = DB_SEQ (p->cursor[i], DB_SET_RANGE);
	while (ret == 0 && memcmp (p->key.data, p->temp_key, (size_t)p->key.size) == 0) {
		memcpy (&dupno, (cob_u8_ptr)p->data.data + p->primekeylen, sizeof (unsigned int));
		ret = DB_SEQ (p->cursor[i], DB_NEXT);
	}
	bdb_close_index (f, i);
	dupno = COB_DUPSWAP(dupno);
	return ++dupno;
}

/* read file with all alternate keys that don't allow duplicates
   to check if records exist already, returns 1 if true */
static int
check_alt_keys (cob_file *f, const int rewrite)
{
	struct indexed_file	*p;
	int			i;
	int			ret;

	p = f->file;
	for (i = 1; i < (int)f->nkeys; ++i) {
		if (!f->keys[i].tf_duplicates) {
			bdb_setkey (f, i);
			ret = DB_GET (p->db[i], 0);
			if (ret == 0) {
				if (rewrite) {
					if (db_cmpkey (f, p->data.data, f->record->data, 0, 0)) {
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
ix_bdb_write_internal (cob_file *f, const int rewrite, const int opt)
{
	struct indexed_file	*p;
	cob_u32_t		i, len;
	unsigned int		dupcnt = 0;
	unsigned int		dupno = 0;
	cob_u32_t		flags = 0;
	int			close_cursor, ret;

	p = f->file;
	close_cursor = bdb_open_cursor (f, 1);

	/* Check duplicate alternate keys */
	if (!rewrite) {
		if (f->nkeys > 1 && check_alt_keys (f, 0)) {
			bdb_close_cursor (f);
			return COB_STATUS_22_KEY_EXISTS;
		}
		bdb_setkey (f, 0);
	}

	/* Write data */
	if (DB_SEQ (p->cursor[0], DB_SET) == 0) {
		bdb_close_cursor (f);
		return COB_STATUS_22_KEY_EXISTS;
	}
	p->data.data = f->record->data;
	p->data.size = (cob_dbtsize_t) f->record->size;
	DB_CPUT(p->cursor[0], DB_KEYFIRST);

	/* Write secondary keys */
	p->data = p->key;
	for (i = 1; i < f->nkeys; ++i) {
		if (rewrite && ! p->rewrite_sec_key[i]) {
			continue;
		}
		if (bdb_suppresskey (f, i))
			continue;
		bdb_setkey (f, i);
		memset(&p->data,0,sizeof(p->data));
		if (f->keys[i].tf_duplicates) {
			flags =  0;
			dupno = get_dupno(f, i);
			dupno = COB_DUPSWAP (dupno);
			if (dupno > dupcnt)
				dupcnt = dupno;
			len = db_savekey(f, p->temp_key, f->record->data, 0);
			p->data.data = p->temp_key;
			p->data.size = (cob_dbtsize_t)len;
			memcpy (((char*)(p->data.data)) + p->data.size, &dupno, sizeof (unsigned int));
			p->data.size += sizeof (unsigned int);
		} else {
			len = db_savekey(f, p->temp_key, f->record->data, 0);
			p->data.data = p->temp_key;
			p->data.size = (cob_dbtsize_t)len;
			flags = DB_NOOVERWRITE;
			dupno = 0;
		}
		bdb_setkey (f, i);

		ret = DB_PUT (p->db[i], flags);
#if (DB_VERSION_MAJOR < 6)
		if (ret == ENOENT) {	/* This is strange, but BDB 5.3 was returning ENOENT sometimes */
			ret = DB_PUT (p->db[i], 0);
		}
#endif
		if (ret != 0) {
			bdb_close_cursor (f);
			return COB_STATUS_22_KEY_EXISTS;
		}
	}

	if (close_cursor)
		bdb_close_cursor (f);

	if ((opt & COB_WRITE_LOCK)
	 && bdb_env != NULL) {
		bdb_setkey (f, 0);
		if (bdb_lock_record (f, p->key.data, p->key.size)) {
			bdb_close_cursor (f);
			return COB_STATUS_51_RECORD_LOCKED;
		}
	}
	if (!f->flag_read_no_02
	 && dupcnt > 1) {
		return COB_STATUS_02_SUCCESS_DUPLICATE;
	}
	return COB_STATUS_00_SUCCESS;
}

static int
ix_bdb_start_internal (cob_file *f, const int cond, cob_field *key,
			const int read_opts, const int test_lock)
{
	struct indexed_file	*p;
	int			ret, len, fullkeylen, partlen;
	unsigned int		dupno;
	int			key_index;

	dupno = 0;
	ret = 0;
	p = f->file;
	p->start_cond = cond;
	/* Look up for the key */
	key_index = db_findkey (f, key, &fullkeylen, &partlen);
	if (key_index < 0) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	p->key_index = (unsigned int)key_index;
	f->curkey = (short)key_index;

	/* Search */
	bdb_setkey (f, p->key_index);
	p->key.size = (cob_dbtsize_t)partlen;	/* may be partial key */
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
			ret = db_cmpkey (f, p->key.data, f->record->data, p->key_index, partlen);
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
		} else if (db_cmpkey (f, p->key.data, f->record->data, p->key_index, partlen) != 0) {
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
		while (ret == 0 
			&& db_cmpkey (f, p->key.data, f->record->data, p->key_index, partlen) == 0) {
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

	if (p->key_index > 0)
		bdb_close_index (f, p->key_index);
	bdb_close_cursor (f);

	if (ret == 0 && test_lock) {
		if (!(read_opts & COB_READ_IGNORE_LOCK)
		 && !(read_opts & COB_READ_NO_LOCK)
		 && !(read_opts & COB_READ_LOCK)) {
			ret = bdb_test_record_lock (f, p->key.data, p->key.size);
			if (ret) {
				bdb_close_index (f, p->key_index);
				bdb_close_cursor (f);
				return ret;
			}
		}
		if (read_opts & COB_READ_LOCK) {
			ret = bdb_lock_record (f, p->key.data, p->key.size);
			if (ret) {
				bdb_close_index (f, p->key_index);
				bdb_close_cursor (f);
				return ret;
			}
		}
	}

	if (ret == 0) {
		if (p->key_index == 0) {
			memcpy (p->last_readkey[0], p->key.data, p->primekeylen);
		} else {
			int keylen = db_keylen (f, p->key_index);
			if (partlen <= 0) {
				cob_runtime_error (_("invalid internal call of %s"), "ix_bdb_start_internal/db_keylen");
				cob_runtime_error (_("Please report this!"));
				cob_stop_run (1);
			}
			memcpy (p->last_readkey[p->key_index],
				    p->temp_key, keylen);
			memcpy (p->last_readkey[p->key_index + f->nkeys], p->key.data, p->primekeylen);
			if (f->keys[p->key_index].tf_duplicates) {
				p->last_dupno[p->key_index] = dupno;
			}
		}
	}

	bdb_close_index (f, p->key_index);
	if (p->key_index != 0) {
		bdb_close_cursor (f);
	}

	return (ret == 0) ? COB_STATUS_00_SUCCESS : COB_STATUS_23_KEY_NOT_EXISTS;
}

static int
ix_bdb_delete_internal (cob_file *f, const int rewrite, int bdb_opts)
{
	struct indexed_file	*p;
	int			i,len;
	DBT			prim_key;
	int			ret;
	cob_u32_t		flags;
	int			close_cursor = 0;
	COB_UNUSED(bdb_opts);

	p = f->file;
	if (!(f->lock_mode & COB_LOCK_MULTIPLE)) {
		bdb_unlock_all (f);
	}
	/* Find the primary key */
	if (f->access_mode != COB_ACCESS_SEQUENTIAL) {
		bdb_setkey(f, 0);
	}
	if (bdb_env != NULL) {
		ret = bdb_test_record_lock (f, p->key.data, p->key.size);
		if (ret) {
			bdb_close_cursor (f);
			return ret;
		}
	}
	if (bdb_env) {
		flags = DB_WRITECURSOR;
	} else {
		flags = 0;
	}
	close_cursor = bdb_open_cursor (f, 1);
	ret = DB_SEQ (p->cursor[0], DB_SET);
	if (ret != 0 && f->access_mode != COB_ACCESS_SEQUENTIAL) {
		bdb_close_cursor (f);
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	prim_key = p->key;
	memcpy(p->saverec, p->data.data, p->data.size);		/* Save old record image */
	memcpy(p->temp_key,prim_key.data,prim_key.size);	/* Save primary key value */
	prim_key.data = p->temp_key;

	/* Delete the secondary keys */
	for (i = 1; i < (int)(f->nkeys); ++i) {
		len = db_savekey(f, p->savekey, p->saverec, i);
		p->key.data = p->savekey;
		p->key.size = (cob_dbtsize_t) len;
		/* rewrite: no delete if secondary key is unchanged */
		if (rewrite) {
			db_savekey(f, p->suppkey, p->saverec, i);
			p->rewrite_sec_key[i] = db_cmpkey(f, p->suppkey, f->record->data, i, 0);
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
						ret = DB_CDEL(p->cursor[i], 0);
					}
					if (DB_SEQ (p->cursor[i], DB_NEXT) != 0) {
						break;
					}
				}
			}
			bdb_close_index (f, i);
		}
	}

	/* Delete the record */
	ret = DB_CDEL(p->cursor[0], 0);

	if (close_cursor && !rewrite) {
		bdb_close_cursor (f);
	}
	return COB_STATUS_00_SUCCESS;
}

/* Delete file */
static int
ix_bdb_file_delete (cob_file_api *a, cob_file *f, char *filename)
{
	int	i;
	char	file_open_buff[COB_FILE_MAX+1];
	COB_UNUSED(a);

	for (i = 0; i < (int)f->nkeys; ++i) {
		if (i == 0) {
			snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s", filename);
		} else {
			snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s.%d", filename, (int)i);
		}
		file_open_buff[COB_FILE_MAX] = 0;
		unlink (file_open_buff);
	}
	return 0;
}

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

/* Check if a file exists in bdb data dirs */
static int
bdb_nofile (const char *filename)
{
	cob_u32_t	i;

	if (!bdb_env || is_absolute (filename)) {
		errno = 0;
		if (bdb_buff)
			strcpy(bdb_buff, filename);
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
				  file_setptr->bdb_home, SLASH_CHAR, bdb_data_dir[i], SLASH_CHAR, filename);
		}
		bdb_buff[COB_SMALL_MAX] = 0;	/* silence analyzer */
		errno = 0;
		if (access (bdb_buff, F_OK) == 0 || errno != ENOENT) {
			return 0;
		}
	}
	if (i == 0) {
		snprintf (bdb_buff, (size_t)COB_SMALL_MAX, "%s%c%s",
			  file_setptr->bdb_home, SLASH_CHAR, filename);
		bdb_buff[COB_SMALL_MAX] = 0; /* silence analyzer */
		errno = 0;
		if (access (bdb_buff, F_OK) == 0 || errno != ENOENT) {
			return 0;
		}
	}
	return 1;
}

/* OPEN INDEXED file */
static int
ix_bdb_open (cob_file_api *a, cob_file *f, char *filename, const int mode, const int sharing)
{
	struct indexed_file	*p;
	int			i;
	int			j;
	int			maxsize;
	db_lockmode_t		lock_mode;
	int			handle_created;
	cob_u32_t		flags = 0;
	int			ret = 0;
	int			nonexistent;
	struct stat	st;
	char		runtime_buffer[COB_FILE_MAX+1];
	COB_UNUSED (sharing);

	if (bdb_join) {			/* Join BDB, on first OPEN of INDEXED file */
		join_environment (a);
		bdb_join = 0;
	}

	nonexistent = 0;
	if (stat(filename, &st) != -1
	 && S_ISDIR(st.st_mode)) {	/* Filename is a directory */
		return COB_XSTATUS_IS_DIR;
	}
	if (bdb_nofile (filename)) {
		nonexistent = 1;
		if (mode != COB_OPEN_OUTPUT && f->flag_optional == 0) {
			return COB_STATUS_35_NOT_EXISTS;
		}
	}

	p = cob_malloc (sizeof (struct indexed_file));
	f->flag_file_lock = 0;	
	f->curkey = -1;
	if (bdb_env != NULL) {
		if ((f->share_mode & COB_SHARE_ALL_OTHER)) {
			lock_mode = DB_LOCK_READ;
		} else
		if (mode == COB_OPEN_OUTPUT 
		 || mode == COB_OPEN_EXTEND 
		 || (f->share_mode & COB_SHARE_NO_OTHER)
		 || (f->lock_mode & COB_FILE_EXCLUSIVE) 
		 || (mode == COB_OPEN_I_O && !f->lock_mode)) {
			lock_mode = DB_LOCK_WRITE;
			f->flag_file_lock = 1;	
		} else {
			lock_mode = DB_LOCK_READ;
		}
		f->file = p;
		ret = bdb_lock_file (f, filename, lock_mode);
		if (ret) {
			cob_free (p);
			f->file = NULL;
			return ret;
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
	}

	if (mode != COB_OPEN_OUTPUT) {
		if (bdb_nofile(filename) == 0) {
			ret = a->cob_read_dict (f, bdb_buff, !f->flag_keycheck);
		} else {
			ret = a->cob_read_dict (f, filename, !f->flag_keycheck);
		}
		if (ret == -1) {
			/* no .dd found - just go on */
		} else if (ret) {
			return ret;
		}
	}

	p->db = cob_malloc (sizeof (DB *) * f->nkeys);
	p->cursor = cob_malloc (sizeof (DBC *) * f->nkeys);
	p->filenamelen = (int) strlen (filename);
	p->last_readkey = cob_malloc (sizeof (unsigned char *) * 2 * f->nkeys);
	p->last_dupno = cob_malloc (sizeof (unsigned int) * f->nkeys);
	p->rewrite_sec_key = cob_malloc (sizeof (int) * f->nkeys);
	maxsize = p->primekeylen = db_keylen(f, 0);
	for (i = 1; i < f->nkeys; ++i) {
		j = db_keylen(f, i);
		if( j > maxsize)
			maxsize = j;
	}
	p->maxkeylen = maxsize;

	for (i = 0; i < f->nkeys; ++i) {
		/* File name */
		runtime_buffer[COB_FILE_MAX] = 0;
		if (i == 0) {
			snprintf (runtime_buffer, (size_t)COB_FILE_MAX, "%s", filename);
		} else {
			snprintf (runtime_buffer, (size_t)COB_FILE_MAX, "%s.%d", filename, (int)i);
		}

		/* btree info */
		ret = db_create (&p->db[i], bdb_env, 0);
		if (!ret) {
			handle_created = 1;
			if (f->flag_big_endian) {
				ret = p->db[i]->set_lorder (p->db[i], 1234);
				if (ret) {
					cob_runtime_error (_("cannot set BDB byteorder (%s), error: %d %s"),
							   "set_lorder", ret, db_strerror (ret));
					return COB_STATUS_39_CONFLICT_ATTRIBUTE;
				}
			} else if (f->flag_little_endian) {
				ret = p->db[i]->set_lorder (p->db[i], 4321);
				if (ret) {
					cob_runtime_error (_("cannot set BDB byteorder (%s), error: %d %s"),
							   "set_lorder", ret, db_strerror (ret));
					return COB_STATUS_39_CONFLICT_ATTRIBUTE;
				}
			}
			if (mode == COB_OPEN_OUTPUT) {
				if (bdb_env) {
					if (!bdb_nofile(runtime_buffer)) {
						ret = bdb_env->dbremove (bdb_env, NULL, runtime_buffer, NULL, 0);
						if (ret == ENOENT)
							ret = 0;
					}
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
			if (ret == 0
			 && i == 0
			 && mode == COB_OPEN_OUTPUT
			 && bdb_nofile(runtime_buffer) == 0) {
				a->cob_write_dict(f, bdb_buff);
			}
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
				if(p->file_lock_set) {
					bdb_env->lock_put (bdb_env, &p->bdb_file_lock);
					p->file_lock_set = 0;
				}
			}
			cob_free (p);
			switch (ret) {
			case DB_LOCK_NOTGRANTED:
				return COB_STATUS_61_FILE_SHARING;
			case ENOENT:
				if (mode == COB_OPEN_EXTEND 
				 || mode == COB_OPEN_OUTPUT) {
					return COB_STATUS_35_NOT_EXISTS;
				}
				if (f->flag_optional) {
					if (mode == COB_OPEN_I_O) {
						return COB_STATUS_30_PERMANENT_ERROR;
					}
					f->open_mode = (unsigned char)mode;
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
	if (bdb_env != NULL) {
		bdb_env->lock_id (bdb_env, &p->bdb_lock_id);
	}

	bdb_setkey(f, 0);
	p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], 0);
	ret = DB_SEQ (p->cursor[0], DB_FIRST);
	bdb_close_cursor (f);
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

	f->open_mode = (unsigned char)mode;
	if (f->flag_optional 
	 && nonexistent
	 && mode != COB_OPEN_OUTPUT) {
		return COB_STATUS_05_SUCCESS_OPTIONAL;
	}
	return 0;
}

/* Close the INDEXED file */

static int
ix_bdb_close (cob_file_api *a, cob_file *f, const int opt)
{
	struct indexed_file	*p;
	int			i;

	COB_UNUSED (a);
	COB_UNUSED (opt);

	p = f->file;
	if (bdb_env != NULL) {
		bdb_unlock_all (f);
		if (p->file_lock_set) {
			bdb_env->lock_put (bdb_env, &p->bdb_file_lock);
			p->file_lock_set = 0;
		}
	}
	if (!bdb_err_tear_down) {
		/* Close DB's */
		for (i = 0; i < (int)f->nkeys; ++i) {
			if (p->cursor[i]) {
				bdb_close_index (f, i);
			}
		}
	}
	for (i = (int)f->nkeys - 1; i >= 0; --i) {
		if (p->db[i] && !bdb_err_tear_down) {
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
	if (p->bdb_locks)
		cob_free (p->bdb_locks);
	if (bdb_env != NULL) {
		bdb_env->lock_id_free (bdb_env, p->bdb_lock_id);
	}
	cob_free (p);

	return COB_STATUS_00_SUCCESS;
}


/* START INDEXED file with positioning */

static int
ix_bdb_start (cob_file_api *a, cob_file *f, const int cond, cob_field *key)
{
	COB_UNUSED (a);

	return ix_bdb_start_internal (f, cond, key, 0, 0);
}

/* Random READ of the INDEXED file  */

static int
ix_bdb_read (cob_file_api *a, cob_file *f, cob_field *key, const int read_opts)
{
	struct indexed_file	*p;
	int			ret;
	int			bdb_opts;
	int			test_lock;

	COB_UNUSED (a);
	p = f->file;
	test_lock = 0;
	bdb_opts = read_opts;
	if (bdb_env != NULL) {
		if (read_opts & COB_READ_LOCK) {
			bdb_opts |= COB_READ_LOCK;
		} else if (read_opts & COB_READ_WAIT_LOCK) {
			if (f->retry_mode == 0
			|| (f->retry_mode & COB_RETRY_FOREVER)) {
				bdb_opts |= COB_READ_LOCK;
			} else {
				bdb_opts |= COB_READ_LOCK;
			}
		} else if ((f->lock_mode & COB_LOCK_AUTOMATIC)
			&& (f->open_mode != COB_OPEN_INPUT) ) {
			bdb_opts |= COB_READ_LOCK;
		}
		if ((bdb_opts & COB_READ_IGNORE_LOCK)
		 || (bdb_opts & COB_READ_NO_LOCK) ) {
			bdb_opts &= ~COB_READ_LOCK;
		}
		if (f->open_mode != COB_OPEN_I_O
		 || f->flag_file_lock) {
			bdb_opts &= ~COB_READ_LOCK;
		} else if ((f->lock_mode & COB_LOCK_AUTOMATIC) 
			&& !(bdb_opts & COB_READ_NO_LOCK)) {
			bdb_opts |= COB_READ_LOCK;
		}
		if ((bdb_opts & COB_READ_LOCK)
		 && !(f->lock_mode & COB_LOCK_MULTIPLE)) {
			bdb_unlock_all (f);
		}
		test_lock = 1;
	} else {
		bdb_opts &= ~COB_READ_LOCK;
	}

	ret = ix_bdb_start_internal (f, COB_EQ, key, bdb_opts, test_lock);
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
}

/* Sequential READ of the INDEXED file */

static int
ix_bdb_read_next (cob_file_api *a, cob_file *f, const int read_opts)
{
	struct indexed_file	*p;
	int			ret;
	int			read_nextprev,skip_lock;
	cob_u32_t		nextprev;
	int			file_changed;
	int			bdb_opts;
	unsigned int		dupno;

	COB_UNUSED (a);
	p = f->file;
	nextprev = DB_NEXT;
	dupno = 0;
	file_changed = 0;

	bdb_opts = read_opts;
	skip_lock = 0;
	if (bdb_env != NULL) {
		if (f->open_mode != COB_OPEN_I_O 
		 || f->flag_file_lock) {
			bdb_opts &= ~COB_READ_LOCK;
		} else if ((f->lock_mode & COB_LOCK_AUTOMATIC) 
			&& !(bdb_opts & COB_READ_NO_LOCK)) {
			bdb_opts |= COB_READ_LOCK;
		}
		if ((f->retry_mode & COB_ADVANCING_LOCK)
		|| (read_opts & COB_READ_ADVANCING_LOCK)) {
			bdb_opts |= COB_READ_LOCK;
			skip_lock = 1;
		} else if (read_opts & COB_READ_LOCK) {
			bdb_opts |= COB_READ_LOCK;
		} else if (read_opts & COB_READ_WAIT_LOCK) {
			bdb_opts |= COB_READ_LOCK;
		} else if ((f->lock_mode & COB_LOCK_AUTOMATIC)
			&&  f->open_mode != COB_OPEN_INPUT) {
			if ((read_opts & COB_READ_IGNORE_LOCK)) {
				bdb_opts &= ~COB_READ_LOCK;
			} else {
				bdb_opts |= COB_READ_LOCK;
			}
		}
		if ((bdb_opts & COB_READ_LOCK)
		 && !(f->lock_mode & COB_LOCK_MULTIPLE)) {
			bdb_unlock_all (f);
		}
	} else {
		bdb_opts &= ~COB_READ_LOCK;
	}

	if (bdb_opts & COB_READ_PREVIOUS) {
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
		/* Data is read in ix_bdb_open or ix_bdb_start */
		if (p->data.data == NULL
		 || (f->flag_first_read == 2 && nextprev == DB_PREV)) {
			bdb_close_index (f, p->key_index);
			if (p->key_index != 0) {
				bdb_close_cursor (f);
			}
			return COB_STATUS_10_END_OF_FILE;
		}
		/* Check if previously read data still exists */
		p->key.size = (cob_dbtsize_t) db_keylen(f,p->key_index);
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
			if (skip_lock
			 && !(bdb_opts & COB_READ_IGNORE_LOCK)
			 && !(bdb_opts & COB_READ_LOCK)) {
				ret = bdb_test_lock_advance (f, nextprev, skip_lock);
				if (ret) {
					bdb_close_index (f, p->key_index);
					bdb_close_cursor (f);
					return ret;
				}
			}
			if (bdb_opts & COB_READ_LOCK) {
				ret = bdb_lock_advance (f, nextprev, skip_lock);
				if (ret) {
					bdb_close_index (f, p->key_index);
					bdb_close_cursor (f);
					if (ret == DB_NOTFOUND)
						return COB_STATUS_10_END_OF_FILE;
					return COB_STATUS_51_RECORD_LOCKED;
				}
			}
		}
	}
	if (!f->flag_first_read || file_changed) {
		if (nextprev == DB_FIRST || nextprev == DB_LAST) {
			read_nextprev = 1;
		} else {
			p->key.size = (cob_dbtsize_t) db_keylen(f,p->key_index);
			p->key.data = p->last_readkey[p->key_index];
			ret = DB_SEQ (p->cursor[p->key_index], DB_SET_RANGE);
			/* ret != 0 possible, records may be deleted since last read */
			if (ret != 0) {
				if (nextprev == DB_PREV) {
					nextprev = DB_LAST;
					read_nextprev = 1;
				} else {
					bdb_close_index (f, p->key_index);
					if (p->key_index != 0) {
						bdb_close_cursor (f);
					}
					return COB_STATUS_10_END_OF_FILE;
				}
			} else {
				if (memcmp (p->key.data, p->last_readkey[p->key_index], (size_t)p->key.size) == 0) {
					if (p->key_index > 0 
					 && f->keys[p->key_index].tf_duplicates) {
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
								bdb_close_index (f, p->key_index);
								if (p->key_index != 0) {
									bdb_close_cursor (f);
								}
								return COB_STATUS_10_END_OF_FILE;
							}
						} else {
							if (memcmp (p->key.data, p->last_readkey[p->key_index], (size_t)p->key.size) == 0 
							 && dupno == p->last_dupno[p->key_index]) {
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
				bdb_close_index (f, p->key_index);
				if (p->key_index != 0) {
					bdb_close_cursor (f);
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
				bdb_close_index (f, p->key_index);
				bdb_close_cursor (f);
				return COB_STATUS_23_KEY_NOT_EXISTS;
			}
		}
		if (bdb_env != NULL) {
			if (skip_lock
			&& !(bdb_opts & COB_READ_IGNORE_LOCK)) {
				ret = bdb_test_lock_advance (f, nextprev, skip_lock);
				if (ret) {
					bdb_close_index (f, p->key_index);
					bdb_close_cursor (f);
					return ret;
				}
			}
			if (bdb_opts & COB_READ_LOCK) {
				ret = bdb_lock_advance (f, nextprev, skip_lock);
				if (ret != 0) {
					bdb_close_index (f, p->key_index);
					bdb_close_cursor (f);
					if (ret == DB_NOTFOUND)
						return COB_STATUS_10_END_OF_FILE;
					return COB_STATUS_51_RECORD_LOCKED;
				}
			}
		}
		if (p->key_index == 0) {
			memcpy (p->last_readkey[0], p->key.data, (size_t)p->key.size);
		} else {
			memcpy (p->last_readkey[p->key_index], p->temp_key,
				    db_keylen(f,p->key_index));
			memcpy (p->last_readkey[p->key_index + f->nkeys], p->key.data, p->primekeylen);
			if (f->keys[p->key_index].tf_duplicates) {
				p->last_dupno[p->key_index] = dupno;
			}
		}
	}

	f->record->size = p->data.size;
	if (f->record->size > f->record_max) {
		f->record->size = f->record_max;
		ret = COB_STATUS_43_READ_NOT_DONE;
	} else {
		ret = COB_STATUS_00_SUCCESS;
	}
	memcpy (f->record->data, p->data.data, f->record->size);

	if (p->key_index > 0
	 && f->keys[p->key_index].tf_duplicates
	 &&	!f->flag_read_no_02
	 && p->start_cond == 0
	 && ret == COB_STATUS_00_SUCCESS) {
		if (nextprev == DB_FIRST)
			nextprev = DB_NEXT;
		else if (nextprev == DB_LAST) 
			nextprev = DB_PREV;
		ret = DB_SEQ (p->cursor[p->key_index], nextprev);
		if (ret == 0
		 && memcmp (p->key.data, p->last_readkey[p->key_index], (size_t)p->key.size) == 0) {
			ret = COB_STATUS_02_SUCCESS_DUPLICATE;
		} else {
			ret = COB_STATUS_00_SUCCESS;
		}
	}

	p->start_cond = 0;
	bdb_close_index (f, p->key_index);
	if (p->key_index != 0) {
		bdb_close_cursor (f);
	}

	return ret;
}


/* WRITE to the INDEXED file  */

static int
ix_bdb_write (cob_file_api *a, cob_file *f, const int opt)
{
	struct indexed_file	*p;
	int			ret;

	COB_UNUSED (a);
	if (f->flag_nonexistent) {
		return COB_STATUS_48_OUTPUT_DENIED;
	}
	p = f->file;
	if (!(f->lock_mode & COB_LOCK_MULTIPLE)) {
		bdb_unlock_all (f);
	}

	/* Check record key */
	bdb_setkey (f, 0);
	if (!p->last_key) {
		p->last_key = cob_malloc ((size_t)p->maxkeylen);
	} else if (f->access_mode == COB_ACCESS_SEQUENTIAL
			 && f->open_mode == COB_OPEN_OUTPUT
			 && !f->flag_set_isam
			 && memcmp (p->last_key, p->key.data, (size_t)p->key.size) > 0) {
		return COB_STATUS_21_KEY_INVALID;
	}
	memcpy (p->last_key, p->key.data, (size_t)p->key.size);

	ret =  ix_bdb_write_internal (f, 0, opt);
	bdb_close_cursor (f);
	if (f->access_mode == COB_ACCESS_SEQUENTIAL
	 && f->open_mode == COB_OPEN_OUTPUT
	 && f->flag_set_isam
	 && ret == COB_STATUS_22_KEY_EXISTS) {
		return COB_STATUS_21_KEY_INVALID;
	}
	return ret;
}


/* DELETE record from the INDEXED file  */

static int
ix_bdb_delete (cob_file_api *a, cob_file *f)
{
	int			ret;

	COB_UNUSED (a);
	if (f->flag_nonexistent) {
		return COB_STATUS_49_I_O_DENIED;
	}
	ret = ix_bdb_delete_internal (f, 0, 0);
	bdb_close_cursor (f);
	return ret;
}

/* REWRITE record to the INDEXED file  */

static int
ix_bdb_rewrite (cob_file_api *a, cob_file *f, const int opt)
{
	int			ret;

	COB_UNUSED (a);
	if (f->flag_nonexistent) {
		return COB_STATUS_49_I_O_DENIED;
	}
	if (!(f->lock_mode & COB_LOCK_MULTIPLE)) {
		bdb_unlock_all (f);
	}

	/* Check duplicate alternate keys */
	if (check_alt_keys (f, 1)) {
		return COB_STATUS_22_KEY_EXISTS;
	}

	/* Delete the current record */
	ret = ix_bdb_delete_internal (f, 1, opt);

	if (ret != COB_STATUS_00_SUCCESS) {
		bdb_close_cursor (f);
		if (ret == COB_STATUS_23_KEY_NOT_EXISTS)
			return COB_STATUS_21_KEY_INVALID;
		return ret;
	}

	/* Write data */
	bdb_setkey(f, 0);
	ret = ix_bdb_write_internal (f, 1, opt);
	bdb_close_cursor (f);

	if (ret == COB_STATUS_00_SUCCESS
	 || ret == COB_STATUS_02_SUCCESS_DUPLICATE) {
		if ((f->lock_mode & COB_LOCK_AUTOMATIC)) {
			if (!(f->lock_mode & COB_LOCK_MULTIPLE)) {
				bdb_unlock_all (f);
			}
		} else {
			if (!(f->lock_mode & COB_LOCK_MULTIPLE)) {
				if (!(opt & COB_WRITE_LOCK)) {
					bdb_unlock_all (f);
				}
			} else
			if ((opt & COB_WRITE_LOCK)) {
				bdb_unlock_last (f);
			} else
			if ((opt & COB_WRITE_NO_LOCK)) {
				bdb_unlock_all (f);
			}
		}
	} else if (ret) {
		bdb_unlock_all (f);
	}
	return ret;
}


static int
ix_bdb_file_unlock (cob_file_api *a, cob_file *f)
{
	COB_UNUSED (a);
	if (COB_FILE_SPECIAL(f)) {
		return 0;
	}
	if (f->organization == COB_ORG_SORT) {
		return 0;
	}

	if (f->open_mode != COB_OPEN_CLOSED 
	 && f->open_mode != COB_OPEN_LOCKED) {
		if (f->file) {
			if (bdb_env != NULL && f->file) {
				struct indexed_file	*p = f->file;
				bdb_unlock_all (f);
				if(p->file_lock_set) {
					bdb_env->lock_put (bdb_env, &p->bdb_file_lock);
					p->file_lock_set = 0;
				}     
				bdb_env->lock_put (bdb_env, &p->bdb_file_lock);
			}
		}
	}
	return 0;
}

/* Call this routine when a new process has been forked */
static int
ix_bdb_fork (cob_file_api *a)
{
	COB_UNUSED (a);
	bdb_lock_id = 0;
	if(bdb_env) {
		bdb_env->lock_id (bdb_env, &bdb_lock_id);
		bdb_env->set_lk_detect (bdb_env, DB_LOCK_DEFAULT);
	}
	return 0;
}

static void
ix_bdb_exit_fileio (cob_file_api *a)
{
	COB_UNUSED (a);
	if(record_lock_object) {
		cob_free (record_lock_object);
		record_lock_object = NULL;
	}
	if (bdb_env) {
		DB_LOCKREQ	lckreq[1];
		memset(lckreq,0,sizeof(DB_LOCKREQ));
		lckreq[0].op = DB_LOCK_PUT_ALL;
		bdb_env->lock_vec (bdb_env, bdb_lock_id, 0, lckreq, 1, NULL);
		bdb_env->lock_id_free (bdb_env, bdb_lock_id);
		bdb_env->close (bdb_env, 0);
		if (bdb_home_dir != NULL
		 && db_env_create (&bdb_env, 0) == 0) {
			bdb_env->remove (bdb_env, bdb_home_dir, 0);
		}
		if(bdb_home_dir)
			cob_free(bdb_home_dir);
		bdb_home_dir = NULL;
		bdb_env = NULL;
	}
	if (record_lock_object) {
		cob_free (record_lock_object);
		record_lock_object = NULL;
		rlo_size = 0;
	}
	if (bdb_buff) {
		cob_free (bdb_buff);
		bdb_buff = NULL;
	}
}

void
cob_bdb_init_fileio (cob_file_api *a)
{
	a->io_funcs[COB_IO_BDB] = (void*)&ext_indexed_funcs;
	bdb_env = NULL;
	bdb_data_dir = NULL;
	record_lock_object = cob_malloc ((size_t)1024);
	bdb_buff = cob_malloc ((size_t)COB_SMALL_BUFF);
	rlo_size = 1024;
}

#endif

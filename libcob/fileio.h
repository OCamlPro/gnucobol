/*
   Copyright (C) 2002-2012, 2014-2023 Free Software Foundation, Inc.
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

#define cobglobptr file_globptr
#define cobsetptr file_setptr

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

#ifndef EDEADLK
#ifdef EDEADLOCK		/* SCO name for EDEADLK */
#define EDEADLK EDEADLOCK
#else 
#define EDEADLK 99
#endif
#endif

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
#define ftruncate	_chsize
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

#include "sysdefines.h"
#ifndef MAXNUMKEYS
#define MAXNUMKEYS 32
#endif

/* Force symbol exports */
#define	COB_LIB_EXPIMP
#include "common.h"
#include "coblocal.h"

#ifdef	WORDS_BIGENDIAN
#define	COB_MAYSWAP_16(x)	((unsigned short)(x))
#define	COB_MAYSWAP_32(x)	((unsigned int)(x))
#else
#define	COB_MAYSWAP_16(x)	(COB_BSWAP_16((unsigned short)(x)))
#define	COB_MAYSWAP_32(x)	(COB_BSWAP_32((unsigned int)(x)))
#endif

/* File API struct passed to all I/O functions */
typedef struct _cob_file_api {
	cob_global		*glbptr;
	cob_settings	*setptr;
	struct cob_fileio_funcs **io_funcs;
	char			**file_paths;
	char			*file_open_buff;
	void	(*add_file_cache)	(cob_file *);
	void	(*del_file_cache)	(cob_file *);
	void	(*chk_file_mapping)	(cob_file *f, char *filename);
	int		(*cob_write_dict)	(cob_file *f, char *filename);
	int		(*cob_read_dict)	(cob_file *f, char *filename, int updt);
	int		(*cob_file_write_opt) (cob_file *f, const int opt);
} cob_file_api;

/* File I/O function pointer structure */
struct cob_fileio_funcs {
	int	(*open)			(cob_file_api *, cob_file *, char *, const int, const int);
	int	(*close)		(cob_file_api *, cob_file *, const int);
	int	(*start)		(cob_file_api *, cob_file *, const int, cob_field *);
	int	(*read)			(cob_file_api *, cob_file *, cob_field *, const int);
	int	(*read_next)	(cob_file_api *, cob_file *, const int);
	int	(*write)		(cob_file_api *, cob_file *, const int);
	int	(*rewrite)		(cob_file_api *, cob_file *, const int);
	int	(*recdelete)	(cob_file_api *, cob_file *);

	int	(*fildelete)	(cob_file_api *, cob_file *, char *);
	void (*ioinit)		(cob_file_api *);
	void (*ioexit)		(cob_file_api *);
	int	(*iofork)		(cob_file_api *);
	int	(*iosync)		(cob_file_api *, cob_file *);
	int	(*commit)		(cob_file_api *, cob_file *);
	int	(*rollback)		(cob_file_api *, cob_file *);
	int	(*iounlock)		(cob_file_api *, cob_file *);
	char * (*ioversion)	(void);
};

COB_EXPIMP	cob_global		*file_globptr;
COB_EXPIMP	cob_settings	*file_setptr;

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


#define COB_STATUS_BASE					100
/* Extended Status values */
enum cob_status_extended {
	COB_XSTATUS = COB_STATUS_BASE,
	COB_XSTATUS_IS_DIR,
	COB_XSTATUS_NOT_DIR,
	COB_XSTATUS_NOT_FILE,
	COB_XSTATUS_MAX
};

COB_HIDDEN int cob_write_dict	(cob_file *f, char *filename);
COB_HIDDEN int cob_read_dict	(cob_file *f, char *filename, int updt);
COB_HIDDEN int indexed_file_type	(cob_file *f, char *filename);
COB_HIDDEN void cob_chk_file_mapping	(cob_file *f, char *filename);
COB_HIDDEN void cob_file_save_status	(cob_file *f, cob_field *fnstatus, const int status);
COB_HIDDEN void cob_file_sync	(cob_file *f);

#ifdef	WITH_DB
void	cob_bdb_init_fileio (cob_file_api *);
#endif
#ifdef	WITH_LMDB
void	cob_lmdb_init_fileio (cob_file_api *);
#endif

COB_HIDDEN int cob_findkey_attr (cob_file *f, cob_field *kf, int *fullkeylen, int *partlen);

#if defined(WITH_ODBC) || defined(WITH_OCI) || defined(WITH_DB) || defined(WITH_LMDB)
/* Routines in fsqlxfd.c common to all Database interfaces */
COB_HIDDEN int db_keylen (cob_file *f, int idx);
COB_HIDDEN int db_savekey (cob_file *f, unsigned char *keyarea, unsigned char *record, int idx);
COB_HIDDEN int db_cmpkey (cob_file *f, unsigned char *keyarea, unsigned char *record, int idx, int partlen);
#endif
#if defined(WITH_ODBC) || defined(WITH_OCI)
#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif

#define SQL_BIND_NO		0
#define SQL_BIND_COLS	2
#define SQL_BIND_PRMS	4
#define SQL_BIND_EQ		8
#define SQL_BIND_WHERE	16
#define SQL_BIND_NORID	32

typedef struct sql_stmt {
	void    	*handle;		/* Database 'handle' */
	char		*text;			/* SQL statement text */
	int			status;			/* Recent status */
	int			readopts;		/* Recent status */
	cob_u32_t	preped:1;		/* has been Prepared for execution */
	cob_u32_t	bound:1;		/* Variables have been bound */
	cob_u32_t	params:1;		/* Parameters are bound */
	cob_u32_t	iscursor:1;		/* CURSOR is open */
	cob_u32_t	isdesc:1;		/* ORDER DESC */
	short		bindpos;		/* Last column position bound to statement */
} SQL_STMT;

/*
 * Holds Database State information
 */
struct db_state {
	cob_u32_t	isopen:1;		/* Connected to database */
	cob_u32_t	isodbc:1;		/* ODBC interface active */
	cob_u32_t	isoci:1;		/* Oracle Call Interface active */
	cob_u32_t	oracle:1;		/* DB is Oracle */
	cob_u32_t	mssql:1;		/* DB is Microsoft SQL Server */
	cob_u32_t	mssqlnfu:1;		/* DB is SQL Server does not accept FOR UPDATE */
	cob_u32_t	mysql:1;		/* DB is MySQL */
	cob_u32_t	mariadb:1;		/* DB is MySQL (MariaDB) */
	cob_u32_t	db2:1;			/* Using IBM DB2 (Untested) */
	cob_u32_t	postgres:1;		/* Using PostgreSQL */
	cob_u32_t	sqlite:1;		/* Using SQLite (limited testing) */
	cob_u32_t	autocommit:1;	/* Running in AUTO COMMIT mode */
	cob_u32_t	scanForNulls:1;	/* Check for NULL columns returned */
	cob_u32_t	attachDbName:1;	/* Attach to specific Oracle instance name */
	cob_u32_t	no_for_update:1;/* DB does not accept FOR UPDATE */
	 
	int		dbStatus;			/* Status of last DB call */
	int		dbFatalStatus;		/* Fatal Status from last DB call */
	char	odbcState[6];		/* Long ODBC status code */
	short	indsz;				/* Sizeof SQL Indicator */
	char	dbType[32];			/* Actual DB type */
	char	dbSchema[32];		/* Schema name */
	char	dbSid[32];			/* DB 'session id' (OCI) */
	char	dbName[32];			/* DB Name 'session id' (OCI) */
	char	dbUser[32];			/* DB UserId to connect with */
	char	dbPwd[32];			/* DB Password to connect with */
	char	dbDsn[32];			/* DB DSN to connect with */
	char	dbCon[256];			/* Full connect string */

	int		dbVer;				/* Data Base version */
	int		updatesDone;		/* # Updates done since last COMMIT */
	int		commitInterval;		/* COMMIT every N updates */
#define BIGCOMMIT	0x7FFFFFF
	int		intRecWait;			/* Time to wait for record lock */
	int		nRecWaitTry;		/* Retry counter for lock */
	int		nMaxRetry;			/* Max retries for lock */
	int		arrayFetch;			/* Size of array fetch */

	/*	Various Status Codes, Actual value set by Data Base Interface */
	/*			for checking  'dbStatus'	Oracle value as example   */
	int		dbStsOk;			/*    0: Operation OK */
	int		dbStsNullCol;		/* 1405: Operation OK, some Column was NULL */
	int		dbStsNotFound;		/*  100: Record not found */
	int		dbStsNotFound2;		/* 1403: Record not found */
	int		dbStsDupKey;		/*    1: Duplicate Key */
	int		dbStsRecLock;		/*   54: Record Locked */
	int		dbStsDeadLock;		/*   60: Dead lock detected */
	int		dbStsNoSpace;		/* 1653: Out of disk space */
	int		dbStsInvlNum;		/* 1722: Invalid number   */
	int		dbStsBadRowid;		/* 1410: bad ROWID */
	int		dbStsNoTable;		/* 1146: Table does not exist */

	char	lastErrMsg[80];		/* Recent DB Error msg */
	cob_file_api	*a;	
	void	*dbHome;			/* ORACLE_HOME value */

	void	*dbEnvH;			/* DB Environment handle */
	void	*dbDbcH;			/* DB database handle */
	void	*dbErrH;			/* DB Error Handle */
	void	*dbSvcH;			/* DB Service Context Handle */
	void	*dbSvrH;			/* DB Server Handle */
	void	*dbSesH;			/* DB Session Handle */
	void	*dbBindV;			/* DB Bind Variable handle */
	void	*dbhnd1;			/* DB spare handles */
	void	*dbhnd2;
	void	*dbhnd3;
};

/*
 * Holds one action/description
 */
struct map_xfd {
	enum {
		XC_DATA = 1,
		XC_GOTO,
		XC_WHEN
	} 		cmd;
	enum {
		XO_NULL = 0,
		XO_GE = 1,
		XO_GT,
		XO_LE,
		XO_LT,
		XO_EQ,
		XO_NE,
		XO_AND,
		XO_OR,
		XO_NOT
	} 		opcode;			/* Operation code */
	int		type;			/* Data type (COB_XFDT_xxxx) */
	int		offset;			/* Offset to data field within record */
	int		size;			/* Size of COBOL data field */
	int		digits;			/* Digits in field */
	int		scale;			/* Decimal scale of field, decimal places */
	int		sqlsize;		/* Size for holding SQL data */
	int		hostType;		/* Host/C data type */
	int		sqlType;		/* SQL Column type */
	int		sqlColSize;		/* SQL Column size */
	int		sqlDecimals;	/* Decimal places */
	int		sqlinlen;		/* Length of data returned from SQL */
	int		sqloutlen;		/* Length of data given to SQL */
	int		level;			/* Original COBOL data level number */
	int		nRlen4;			/* Oracle column length (int) */
	short	target;			/* Target position */
	short	jumpto;			/* Resolved target position */
	short	lncolname;		/* Length of column name */
	short	lnvalue;		/* Length of 'value' */
	short	colpos;			/* Position in 'map' of this column def */
	short	nRlen2;			/* Oracle column length (short) */
	short	nRcode;			/* Oracle column return code */
	char	valnum;			/* Value is numeric */
	char	setnull;		/* Indicator was/is NULL */
	char	notnull;		/* Column is set NOT NULL */
	char	iskey;			/* Column is a key field */
	char	*colname;		/* Column name */
	char	*value;			/* Value to test */
	struct sql_date	*dtfrm;	/* Date format to use */
	unsigned char *sdata;	/* SQL data storage area (within 'sqlbf') */
	int		*ind;			/* SQL Indicator */
	cob_field		recfld;	/* Data field found in File record area */
	cob_field_attr	recattr;
	cob_pic_symbol	recpic[6];
	cob_field		sqlfld;	/* Data field found in SQL buffer area */
	cob_field_attr	sqlattr;
	cob_pic_symbol	sqlpic[6];
};

/*
 * Defines a key
 */
#define MAXKEYCOLS	32
struct key_xfd {
	unsigned char	keyn;		/* Key # */
	unsigned char	dups;		/* 1 if DUPS allowed */
	unsigned char	sup;		/* 1 if SUPPRESS (but not supported by ODBC/OCI */
	unsigned char	supchar;	/* Character to indicate key suppression */
	unsigned char	*str_sup;	/* Suppress if this string appears */
	short			ncols;		/* Number of Columns in index */
	short			lncols;		/* Length of all column names in index */
	short			col[MAXKEYCOLS];	/* Offset in file_xfd.map to column def */
	int				lncreate;
	char			*create_index;	/* SQL CREATE INDEX */
	SQL_STMT		count_eq;	/* SELECT COUNT(*) WHERE index EQ */
	SQL_STMT		where_eq;	/* SELECT WHERE index EQ */
	SQL_STMT		where_ne;	/* SELECT WHERE index NE */
	SQL_STMT		where_le;	/* SELECT WHERE index LE */
	SQL_STMT		where_lt;	/* SELECT WHERE index LT */
	SQL_STMT		where_ge;	/* SELECT WHERE index GE */
	SQL_STMT		where_gt;	/* SELECT WHERE index GT */
	SQL_STMT		where_fi;	/* SELECT index first */
	SQL_STMT		where_la;	/* SELECT index last */
	SQL_STMT		where_ndup;	/* SELECT WHERE index EQ next duplicate */
	SQL_STMT		where_pdup;	/* SELECT WHERE index EQ previous duplicate */
};
#define COB_COUNT	16			/* Build SELECT COUNT(*) for given index */
#define COB_NDUP	17			/* Build SELECT COUNT(*) WHERE next duplicate */
#define COB_PDUP	18			/* Build SELECT COUNT(*) WHERE previous duplicate */

/*
 * Primary table for in memory XFD
 */
struct file_xfd {
	cob_file *fl;			/* File used */
	char	*tablename;		/* SQL Table Name */
	int		nmap;			/* Number of data mapping directives */
	struct map_xfd	*map;	/* Table of data mapping directives */
	unsigned char	*sqlbf;	/* Large buffer for SQL data */
	SQL_STMT	insert;		/* Insert statement */
	SQL_STMT	update;		/* Update statement */
	SQL_STMT	delete;		/* Delete statement */
	char	*select;		/* List of columns for a select statement */
	char	*create_table;	/* SQL CREATE TABLE */
	char	*create_sequence;/* SQL CREATE SEQUENCE */
	int		lncreate;
	int		lnselect;		/* Length of all column for SELECT */
	int		lnind;			/* Length of one 'SQL Indicator' */
	int		nkeys;			/* Number of indexes on table */
	int		ndate;			/* Number of unique 'date formats' used */
	int		nlbl;			/* Number of labels used */
	int		ncols;			/* Number of columns */
	int		maxcolnmln;		/* Length of longest column name */
	int		lncols;			/* Length of all Column names */
	int		fileorg;		/* cob_file.organization */
	int		*xlbl;			/* Label to map[subscript] table */
	int		hasrid;			/* A rid_tablename column is used */
	long	 recnum;		/* Current rowid value */
	void	*precnum;		/* Recnum SQL data area */
	struct sql_date	**date;	/* Date formats used */
	SQL_STMT	*start;		/* Active SELECT statement */
	struct key_xfd	*key[MAXNUMKEYS];
};

/* Routines in fsqlxfd.c common to ODBC/OCI interfaces */
COB_HIDDEN struct file_xfd* cob_load_xfd (struct db_state *db, cob_file *fl, char *alt_name, 
											int indsize, int skiprid);
COB_HIDDEN void 	cob_dump_xfd (struct file_xfd *fx, FILE *fo);
COB_HIDDEN void 	cob_load_ddl (struct db_state *db, struct file_xfd *fx);
COB_HIDDEN char *	getSchemaEnvName (struct db_state *db, char *envnm, const char *suf, char *out);
COB_HIDDEN void 	logSchemaEnvName (struct db_state *db, const char *suffix);
COB_HIDDEN char *	cob_sql_stmt (struct db_state *, struct file_xfd *, char *, int, int, int);
COB_HIDDEN SQL_STMT *	cob_sql_select (struct db_state *, struct file_xfd *, int, int, int, void (*freeit)());
COB_HIDDEN void 	cob_xfd_to_file (struct db_state *db, struct file_xfd *fx, cob_file *fl);
COB_HIDDEN void 	cob_xfd_to_ddl (struct db_state *db, struct file_xfd *fx, FILE *fo);
COB_HIDDEN void 	cob_file_to_xfd (struct db_state *db, struct file_xfd *fx, cob_file *fl);
COB_HIDDEN void		cob_index_to_xfd (struct db_state *db, struct file_xfd *fx, cob_file *fl, 
					int idx, int cond);
COB_HIDDEN void		cob_index_clear (struct db_state *db, struct file_xfd *fx, cob_file *fl, int idx);
COB_HIDDEN void		cob_xfd_swap_data (char *p1, char *p2, int len);
COB_HIDDEN void		cob_drop_xfd (struct file_xfd *fx);
COB_HIDDEN void 	cob_sql_dump_stmt (struct db_state  *db, char *stmt, int doall);
COB_HIDDEN void 	cob_sql_dump_data (struct db_state *db, struct file_xfd *fx);
COB_HIDDEN void 	cob_sql_dump_index (struct db_state *db, struct file_xfd *fx, int idx);

#endif

#ifdef	WITH_ODBC
COB_EXPIMP void	cob_odbc_init_fileio (cob_file_api *);
#endif
#ifdef	WITH_OCI
COB_EXPIMP void	cob_oci_init_fileio (cob_file_api *);
#endif

#if defined(WITH_CISAM) || defined(WITH_DISAM) || defined(WITH_VBISAM) || defined(WITH_VISAM)
COB_EXPIMP void	cob_isam_init_fileio (cob_file_api *);
#endif

/* cob_file_dict values */
#define COB_DICTIONARY_NO	0
#define COB_DICTIONARY_MIN	1
#define COB_DICTIONARY_ALL	2

/* cob_file_dups values */
#define COB_DUPS_DEFAULT	0
#define COB_DUPS_NEVER		1
#define COB_DUPS_ALWAYS		2


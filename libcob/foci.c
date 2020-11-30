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


/* Force symbol exports */
#define	COB_LIB_EXPIMP
#include "fileio.h"

#ifdef WITH_OCI
#ifdef WITH_ODBC
#undef WITH_ODBC
#endif

#include <oci.h>

void cob_oci_init_fileio (cob_file_api *a);

/* Local variables */

static int oci_sync			(cob_file_api *, cob_file *);
static int oci_commit		(cob_file_api *, cob_file *);
static int oci_rollback		(cob_file_api *, cob_file *);
static int oci_open			(cob_file_api *, cob_file *, char *, const int, const int);
static int oci_close		(cob_file_api *, cob_file *, const int);
static int oci_start		(cob_file_api *, cob_file *, const int, cob_field *);
static int oci_read			(cob_file_api *, cob_file *, cob_field *, const int);
static int oci_read_next	(cob_file_api *, cob_file *, const int);
static int oci_write		(cob_file_api *, cob_file *, const int);
static int oci_delete		(cob_file_api *, cob_file *);
static int oci_file_delete	(cob_file_api *, cob_file *, char *);
static int oci_rewrite		(cob_file_api *, cob_file *, const int);
static int oci_file_unlock	(cob_file_api *, cob_file *);
static void oci_exit_fileio	(cob_file_api *);
static int oci_fork 		(cob_file_api *);

static const struct cob_fileio_funcs oci_indexed_funcs = {
	oci_open,
	oci_close,
	oci_start,
	oci_read,
	oci_read_next,
	oci_write,
	oci_rewrite,
	oci_delete,
	oci_file_delete,
	cob_oci_init_fileio,
	oci_exit_fileio,
	oci_fork,
	oci_sync,
	oci_commit,
	oci_rollback,
	oci_file_unlock
};

static int		db_join = 1;
static struct db_state db[1];
static char	varFetch[80];

struct indexed_file {
	struct file_xfd	*fx;
	int		startcond;
	int		maxkeylen;
	int		primekeylen;
	enum {
		LMANULOCK = 0,
		LAUTOLOCK = 1,
		LEXCLLOCK = 2,
	} lmode;
	unsigned char	*savekey;	/* Work area for saving key value */
	unsigned char	*suppkey;	/* Work area for saving key value */
	unsigned char	*saverec;	/* For saving copy of record */
};

/* Local functions */

/**************************************************
	Check Status from an Oracle call
	Return 0 if OK to proceed;
	Return 1 if Not OK to proceed;
**************************************************/
static int
chkSts(
	struct db_state		*db, 
	char		*msg, 
	int			ociSts)
{
	int			i;
#if defined(__linux__)
	ub4			oraStatus;
#else
	sb4			oraStatus;
#endif
	char		*env, errMsg[COB_SMALL_BUFF];

	memset(db->odbcState, 0, sizeof(db->odbcState));
	if (ociSts == OCI_SUCCESS) {
		db->dbStatus = 0;
		db->scanForNulls = FALSE;
		return 0;
	}
	db->dbStatus = ociSts;

	memset(db->lastErrMsg,0,sizeof(db->lastErrMsg));
	if (ociSts == OCI_SUCCESS_WITH_INFO) {
		db->dbStatus = 0;
		db->scanForNulls = TRUE;
		return 0;
	}

	if (db->dbStatus == db->dbStsNotFound2)		/* MODE=ANSI 'Not found' */
		db->dbStatus = db->dbStsNotFound;		/* Set internal 'Not found' status */
	else if(db->dbStatus == db->dbStsNullCol)	/* Ignore NULL Column warning */
		db->dbStatus = 0;
	else if(db->dbStatus == 2114)				/* Ignore "Closing a closed cursor" */
		db->dbStatus = 0;
	if (db->dbStatus == 0)
		return 0;
	if (db->dbStatus == db->dbStsNotFound) {
		db->scanForNulls = FALSE;
		return db->dbStsNotFound;
	}

	if (msg == NULL) msg = (void*)"?";
	oraStatus = 0;
	strcpy(errMsg,"");
	OCIErrorGet(db->dbErrH, 1, (text*)NULL, (void*)&oraStatus, 
						(void*)errMsg, (int)sizeof(errMsg)-1, OCI_HTYPE_ERROR);
	if (oraStatus < 0)
		db->dbStatus = -oraStatus;
	else
		db->dbStatus = oraStatus;
	i = strlen(errMsg);
	if (errMsg[i-1] == '\n')
		errMsg[--i] = 0;
	snprintf(db->lastErrMsg,sizeof(db->lastErrMsg),"%s",errMsg);
	for(i=0; i < sizeof(db->lastErrMsg) && db->lastErrMsg[i] != '\n'; i++);
	while(i < sizeof(db->lastErrMsg))
		db->lastErrMsg[i++] = 0;

	if (db->dbStatus == db->dbStsNotFound2)		/* MODE=ANSI 'Not found' */
		db->dbStatus = db->dbStsNotFound;		/* Set internal 'Not found' status */
	else if(db->dbStatus == db->dbStsNullCol)	/* Ignore NULL Column warning */
		db->dbStatus = 0;
	else if(db->dbStatus == 2114)				/* Ignore "Closing a closed cursor" */
		db->dbStatus = 0;

	if (db->dbStatus == 0)
		return 0;

	if (memcmp(errMsg,"ORA-00604",9) == 0		/* 604 means there is some other error */
	&& (env = strstr(errMsg+9,"ORA-")) != NULL) {
		i = strlen(env);
		memset(db->lastErrMsg,0,sizeof(db->lastErrMsg));
		snprintf(db->lastErrMsg,sizeof(db->lastErrMsg),"%s",env);
		for(i=0; i < sizeof(db->lastErrMsg) && db->lastErrMsg[i] != '\n'; i++);
		while(i < sizeof(db->lastErrMsg))
			db->lastErrMsg[i++] = 0;
		i = atoi(&env[4]);						/* Primary error code */
		if (i > 0 && i < 10000)
			db->dbStatus = i;
		DEBUG_LOG ("db",("%s Status 604 due to %d: %s\n",msg,db->dbStatus,db->lastErrMsg));
	}


	if (db->dbStatus == db->dbStsNoTable) {
		DEBUG_LOG("db",("%.40s Status of %d '%s'\n", msg, db->dbStatus, errMsg));
		return 1;
	}

	if (db->dbStatus != 0 
	&& db->dbStatus != db->dbStsNotFound) {
		if (db->dbStatus == db->dbStsRecLock		/* FOR UPDATE NOWAIT and its held! */
		&& db->intRecWait > 1000
		&& db->nMaxRetry > 0) {
			db->nRecWaitTry++;
			cob_sleep_msec(db->intRecWait);			/* Pause a while */
			return 1;								/* Skip logging error message */
		}
		if (db->dbStatus == 3114
		 || db->dbStatus == 3113) {
			db->isopen = FALSE;
			db->dbFatalStatus = db->dbStatus;
		} else
		if (db->dbFatalStatus == 0
		&& db->dbStatus > 1000) {
			db->dbFatalStatus = db->dbStatus;
		}
		DEBUG_LOG("db",("%.40s Status of %d, fatal %d\n", 
							msg, db->dbStatus, db->dbFatalStatus));
		DEBUG_LOG("db",("    : %s\n",errMsg));
	}
	if ( (db->dbFatalStatus >= 0)
	&&   (db->dbStatus < 0)
	&&   (db->dbStatus != db->dbStsRecLock)
	&&   (db->dbStatus != db->dbStsDupKey)
	&&   (db->dbStatus != 1722)
	&&   (db->dbStatus != 1410)
	&&   (db->dbStatus != db->dbStsNotFound) ) {
		db->dbFatalStatus = db->dbStatus;
	}

	return 1;
}

/*
 * Break the xxx_CON string up into the individual fields
 *       user@network-sid/password
 *       user/password
 *       network-sid
 */
static void
splitConnectString(struct db_state *db, char *env)
{
	char	temp[256];
	int		j,k,stp;
	unsigned char	bUserSet = FALSE;
	unsigned char	bPwdSet = FALSE;
	unsigned char	bSidSet = FALSE;

	temp[j=0] = 0;
	stp = 0;	/* Userid is first */
	snprintf(db->dbCon,sizeof(db->dbCon),"%s",env);
	for(k=0; env[k] != 0; k++) {
		if(env[k] == '/') {
			temp[j] = 0;
			if(stp == 2) {
				bSidSet = TRUE;
				snprintf(db->dbName,sizeof(db->dbName),"%s",temp);
				db->attachDbName = TRUE;
			} else {
				snprintf(db->dbUser,sizeof(db->dbUser),"%s",temp);
				bUserSet = TRUE;
			}
			temp[j=0] = 0;
			stp = 1;
		} else if(env[k] == '@') {
			temp[j] = 0;
			snprintf(db->dbUser,sizeof(db->dbUser),"%s",temp);
			bUserSet = TRUE;
			temp[j=0] = 0;
			stp = 2;
		} else {
			temp[j++] = env[k];
			temp[j] = 0;
		}
	}
	if(temp[0] > ' ') {
		if(stp == 0		/* No / or @ so use as SID value */
		|| stp == 2) {
			snprintf(db->dbName,sizeof(db->dbName),"%s",temp);
			db->attachDbName = TRUE;
			bSidSet = TRUE;
		} else {
			snprintf(db->dbPwd,sizeof(db->dbPwd),"%s",temp);
			bPwdSet = TRUE;
		}
	}
	if(bUserSet) {
		env = (void*)malloc(strlen(db->dbUser)+20);
		sprintf((char*)env,"ORACLE_UID=%s",db->dbUser);
		putenv( (char*)env );
	}
	if(bPwdSet) {
		env = (void*)malloc(strlen(db->dbPwd)+20);
		sprintf((char*)env,"ORACLE_PWD=%s",db->dbPwd);
		putenv( (char*)env );
	}
	if(bSidSet) {
		env = (void*)malloc(strlen(db->dbName)+20);
		sprintf((char*)env,"ORACLE_SID=%s",db->dbName);
		putenv( (char*)env );
	}
}

static int
oci_commit (cob_file_api *a, cob_file *f)
{
#ifdef COB_DEBUG_LOG
	char	msg[24];
#endif
	COB_UNUSED (a);
	COB_UNUSED (f);
	if (!db->isopen)
		return 0;
	if (f->last_operation == COB_LAST_COMMIT) {
		DEBUG_LOG("db",("COMMIT from application!\n"));
		db->autocommit = FALSE;
	} else if (db->updatesDone < db->commitInterval
		    && f->last_operation != COB_LAST_CLOSE)
		return 0;
	if (chkSts(db,(char*)"Commit",
			OCITransCommit(db->dbSvcH, db->dbErrH, OCI_DEFAULT))) {
		db->updatesDone = 0;
		return COB_STATUS_30_PERMANENT_ERROR;
	}
#ifdef COB_DEBUG_LOG
	if (db->updatesDone <= 0
	 || db->updatesDone > (int)BIGCOMMIT)
		strcpy(msg,"");
	else
		sprintf(msg,"%d ",db->updatesDone);
	DEBUG_LOG("db",("%s Commit %supdates\n",db->dbType,msg));
#endif
	db->updatesDone = 0;
	return 0;
}

static int
oci_rollback (cob_file_api *a, cob_file *f)
{
#ifdef COB_DEBUG_LOG
	char	msg[24];
#endif
	COB_UNUSED (a);
	COB_UNUSED (f);
	if (!db->isopen)
		return 0;
	if (f->last_operation == COB_LAST_ROLLBACK) {
		DEBUG_LOG("db",("ROLLBACK from application!\n"));
		db->autocommit = FALSE;
	} else if (db->updatesDone < 1)
		return 0;
	if (chkSts(db,(char*)"Rollback",
			OCITransRollback(db->dbSvcH, db->dbErrH, OCI_DEFAULT))) {
		db->updatesDone = 0;
		return COB_STATUS_30_PERMANENT_ERROR;
	}
#ifdef COB_DEBUG_LOG
	if (db->updatesDone <= 0
	 || db->updatesDone > (int)BIGCOMMIT)
		strcpy(msg,"");
	else
		sprintf(msg,"%d ",db->updatesDone);
	DEBUG_LOG("db",("%s Rollback %supdates\n",db->dbType,msg));
#endif
	db->updatesDone = 0;
	return 0;
}

/****************************************************
	Bind just column to return data 
****************************************************/
static int
bindColumn(
	struct db_state	*db,
	struct file_xfd *fx,
	SQL_STMT    *s,
	struct map_xfd *col,
	int			pos)
{
	char	msg[64];
	if (col->cmd == XC_DATA
	 && col->colname) {
		col->hostType = SQLT_STR;
		if (col->dtfrm) {
			col->sqlType = SQLT_DAT;
		} else if (col->type == COB_XFDT_FLOAT) {
			if (col->size == sizeof(double))
				col->hostType = SQLT_FLT;
			else
				col->hostType = SQLT_FLT;
			col->sqlType = SQLT_FLT;
			col->sqlColSize = col->size;
		} else if (col->type == COB_XFDT_BIN) {
			col->sqlColSize = col->size;
			col->hostType = SQLT_BIN;
			col->sqlType = SQLT_BIN;
		} else if (col->valnum) {
			col->sqlType = SQLT_NUM;
		} else {
			col->sqlType = SQLT_CHR;
		}
	}
	sprintf(msg,"BindColumn %s.%s Pos %d",fx->tablename,col->colname,pos);
	db->dbBindV = NULL;
	col->nRlen2 = col->sqlColSize;
	col->nRlen4 = col->sqlColSize;
	if (chkSts(db,msg,
			OCIDefineByPos(s->handle, (OCIDefine **)&db->dbBindV, db->dbErrH,
				(ub4)pos, (ub1*)col->sdata, (sword)col->sqlColSize, 
				(ub2)col->hostType, (ub2*)col->ind, 
				(ub2*)&col->nRlen2, (ub2*)&col->nRcode, OCI_DEFAULT))) {
		return 1;
	}
	return 0;
}

/****************************************************
	Bind just one column as parameter to statment 
****************************************************/
static int
bindParam(
	struct db_state	*db,
	struct file_xfd *fx,
	SQL_STMT    *s,
	struct map_xfd *col,
	int			pos)
{
	char	msg[64];
	ub2		prmtype;
	if (col->cmd == XC_DATA
	 && col->ind) {
		if (col->setnull) {
			*(ub2*)col->ind = -1;
		} else {
			*(ub2*)col->ind = 0;
		}
	}
	db->dbBindV = NULL;
	col->nRlen2 = col->sqlColSize;
	col->nRlen4 = col->sqlColSize;
	if (col->type == COB_XFDT_BIN) 
		prmtype = SQLT_BIN;
	else
		prmtype = col->hostType;
	sprintf(msg,"BindParam %s.%s Pos %d",fx->tablename,col->colname,pos);
	if (chkSts(db,msg,
			OCIBindByPos(s->handle, (OCIBind **)&db->dbBindV, db->dbErrH,
						(ub4)pos, (ub1*)col->sdata, (sword)col->sqlColSize,
						(sword)prmtype, (ub2*)col->ind, NULL, 
						(ub2*)&col->nRcode, 0, NULL, OCI_DEFAULT))) {
		return 1;
	}
	return 0;
}

static void
oci_free_all_handles ( struct db_state	*db)
{
	db->isopen = FALSE;			/* Data Base is NOT connected */
	if (db->dbSesH) {
		OCIHandleFree ( db->dbSesH, OCI_HTYPE_SESSION);
		db->dbSesH = NULL;
	}
	if (db->dbSvcH) {
		OCIHandleFree ( db->dbSvcH, OCI_HTYPE_SVCCTX);
		db->dbSvcH = NULL;
	}
	if (db->dbSvrH) {
		OCIHandleFree ( db->dbSvrH, OCI_HTYPE_SERVER);
		db->dbSvrH = NULL;
	}
	if (db->dbErrH) {
		OCIHandleFree ( db->dbErrH, OCI_HTYPE_ERROR);
		db->dbErrH = NULL;
	}
	if (db->dbSesH) {
		OCIHandleFree ( db->dbSesH, OCI_HTYPE_SESSION);
		db->dbSesH = NULL;
	}
	if (db->dbEnvH) {
		OCIHandleFree ( db->dbEnvH, OCI_HTYPE_ENV);
		db->dbEnvH = NULL;
	}
}

static int
oci_set_nulls (
	struct db_state	*db,
	struct file_xfd *fx)
{
	int		k;
	for (k=0; k < fx->nmap; k++) {
		if (fx->map[k].cmd == XC_DATA
		 && fx->map[k].ind) {
			if (fx->map[k].setnull) {
				*(ub2*)fx->map[k].ind = -1;
			} else {
				*(ub2*)fx->map[k].ind = 0;
			}
		}
	}
	return 0;
}

static int
oci_any_nulls (
	struct db_state	*db,
	struct file_xfd *fx)
{
	int		k,ln;
	for (k=0; k < fx->nmap; k++) {
		if (fx->map[k].cmd == XC_DATA
		 && fx->map[k].ind) {
			ln = fx->map[k].sqlinlen = fx->map[k].sqlsize;
			if(fx->map[k].hostType == SQLT_STR) {
				ln = (int)strlen((char*)fx->map[k].sdata);
			}
			if (*(short*)fx->map[k].ind == -1) {
				fx->map[k].setnull = TRUE;
			} else if(*(short*)fx->map[k].ind == 0) {
				fx->map[k].setnull = FALSE;
				if (fx->map[k].sqlinlen > ln)
					fx->map[k].sqlinlen = ln;
			} else {
				fx->map[k].setnull = FALSE;
				fx->map[k].sqlinlen = fx->map[k].nRlen2;
				if (fx->map[k].sqlinlen > ln)
					fx->map[k].sqlinlen = ln;
			}
		}
	}
	return 0;
}

static int
oci_setup_stmt (
	struct db_state	*db,
	struct file_xfd *fx,
	SQL_STMT		*s,
	int				bindtype,
	int				idx)
{
	int		k,pos;
	if (!s->handle) {
		if (chkSts(db,(char*)"Alloc Stmt Handle",
				OCIHandleAlloc(db->dbEnvH,&s->handle,OCI_HTYPE_STMT, 0, NULL))){
			DEBUG_LOG("db",("OCIHandleAlloc %.40s status %d; Failed!\n",s->text,db->dbStatus));
			s->status = db->dbStatus;
			return db->dbStatus;
		}
		s->preped = FALSE;
		s->bound = FALSE;
		s->params = FALSE;
		s->iscursor = FALSE;
	}
	if (!s->preped) {
		if (chkSts(db,(char*)"Prepare Stmt",
				OCIStmtPrepare(s->handle,db->dbErrH,(text*)s->text,strlen(s->text),
								OCI_NTV_SYNTAX,OCI_DEFAULT))){
			DEBUG_LOG("db",("OCIPrepare %.40s status %d; Failed!\n",s->text,db->dbStatus));
			s->status = db->dbStatus;
			return db->dbStatus;
		}
		s->preped = TRUE;
	}
	if (!s->params 
	 && (bindtype & SQL_BIND_PRMS)) {
		pos = 0;
		for (k=0; k < fx->nmap; k++) {
			if (fx->map[k].cmd == XC_DATA
			 && fx->map[k].colname) {
				bindParam (db, fx, s, &fx->map[k], ++pos);
			}
		}
		s->bindpos = pos;
		s->params = TRUE;
	} else
	if (!s->bound 
	 && (bindtype & SQL_BIND_COLS)) {
		pos = 0;
		for (k=0; k < fx->nmap; k++) {
			if (fx->map[k].cmd == XC_DATA
			 && fx->map[k].colname) {
				bindColumn (db, fx, s, &fx->map[k], ++pos);
			}
		}
		s->bindpos = 0;
		s->bound = TRUE;
	}

	if ((bindtype & SQL_BIND_EQ))	{			/* Index columns bind once each */
		pos = s->bindpos;
		for (k=0; k < fx->key[idx]->ncols; k++) {
			bindParam (db, fx, s, &fx->map[fx->key[idx]->col[k]], ++pos);
		}
	} else if ((bindtype & SQL_BIND_WHERE)) {	/* Index Columns for complex WHERE */
		pos = s->bindpos;
		for (k=0; k < fx->key[idx]->ncols-1; k++) {
			bindParam (db, fx, s, &fx->map[fx->key[idx]->col[k]], ++pos);
			bindParam (db, fx, s, &fx->map[fx->key[idx]->col[k]], ++pos);
		}
		bindParam (db, fx, s, &fx->map[fx->key[idx]->col[k]], ++pos);
	}
	s->status = 0;
	return 0;
}

static int
oci_row_count (
	struct db_state	*db,
	SQL_STMT		*s)
{
	ub4	count = (ub4)-1;
	OCIAttrGet (s->handle, OCI_HTYPE_STMT, &count, 0, OCI_ATTR_ROW_COUNT, db->dbErrH);
	return (int)count;
}

static void
oci_close_stmt ( SQL_STMT *s)
{
	if (s == NULL
	 || s->handle == NULL)
		return;
	s->iscursor = FALSE;
	s->status = 0;
	return;
}

static void
oci_free_stmt ( SQL_STMT *s)
{
	if (s == NULL
	 || s->handle == NULL)
		return;
	OCIHandleFree(s->handle, OCI_HTYPE_STMT);
	s->handle = NULL;
	s->preped = FALSE;
	s->bound = FALSE;
	s->params = FALSE;
	s->iscursor = FALSE;
	s->status = 0;
	if (s->text)
		cob_free (s->text);
	s->text = NULL;
	return;
}

static int
oci_sync (cob_file_api *a, cob_file *f)
{
	COB_UNUSED (a);
	COB_UNUSED (f);
	if (!db->isopen)
		return 0;
	if (db->updatesDone > 0) {
		db->updatesDone = 0;
		if (chkSts(db,(char*)"Commit",
				OCITransCommit(db->dbSvcH, db->dbErrH, OCI_DEFAULT)))
			return COB_STATUS_30_PERMANENT_ERROR;
		DEBUG_LOG("db",("OCI Sync/Commit completed\n"));
	}
	return 0;
}

/****************************************************
	Issue one simple SQL statment, no variables
		Return 0 if OK to proceed;
		Return !0 if Not OK to proceed;
*****************************************************/
static int
ociStmt(
	struct db_state	*db,
	char	*stmt)
{
	void	*stmtHndl;
	int		len, rtn = 0;
	char	msg[80];
	ub4		iters = 1;
	ub2		nRlen2, nRcode;

	if (chkSts(db,(char*)"Alloc stmtHandle",
			OCIHandleAlloc(db->dbEnvH,&stmtHndl,OCI_HTYPE_STMT, 0, NULL))){
		DEBUG_LOG("db",("OCIHandleAlloc %s status %d; Failed!\n",stmt,db->dbStatus));
		return db->dbStatus;
	}

	len = strlen(stmt);
	if (len <= 0) {
		return 0;
	}
	snprintf(msg,sizeof(msg),"Prep: %.50s",stmt);
	db->dbStatus = 0;
	if(chkSts(db,(char*)msg, 
				OCIStmtPrepare(stmtHndl,db->dbErrH,
								(text*)stmt,len,OCI_NTV_SYNTAX,OCI_DEFAULT))) {
		DEBUG_LOG("db",("OCIStmtPrepare status %d; Failed!\n",db->dbStatus));
	} else {
		snprintf(msg,sizeof(msg),"Exec: %.50s",stmt);
		db->dbStatus = 0;
		if (strncasecmp(stmt,"SELECT ",7) == 0) 
			iters = 0;
		chkSts(db,(char*)msg,
				OCIStmtExecute(db->dbSvcH,stmtHndl,db->dbErrH,
										iters,0,NULL,NULL,OCI_DEFAULT));
	}
	rtn = db->dbStatus;
	if (db->dbStatus == 0
	 && strncasecmp(stmt,"SELECT ",7) == 0) {
		chkSts(db,msg,
			OCIDefineByPos(stmtHndl, (OCIDefine **)&db->dbBindV, db->dbErrH,
				(ub4)1, (ub1*)varFetch, (sword)sizeof(varFetch),
				(ub2)SQLT_CHR, (ub2*)NULL,
				(ub2*)&nRlen2, (ub2*)&nRcode, OCI_DEFAULT));
		varFetch[0] = 0;
		if (chkSts(db,(char*)"Fetch Stmt",
			OCIStmtFetch2(stmtHndl,db->dbErrH,1,OCI_FETCH_NEXT,0,OCI_DEFAULT))) {
			DEBUG_LOG("db",("Fetch: %.50s; Sts %d\n",stmt,db->dbStatus));
			rtn = db->dbStatus;
		} else {
			DEBUG_LOG("db",("Fetch: %.50s; '%s' OK\n",stmt,varFetch));
		}
	}
	OCIHandleFree(stmtHndl, OCI_HTYPE_STMT); 
	return rtn;
}

/****************************************************
	Issue one SQL statment to count records with matching key value
		Return number of rows with same key
*****************************************************/
static int
ociCountIndex(
	struct db_state	*db,
	cob_file *f,
	struct file_xfd *fx,
	int		idx)
{
	int		rtn, pos, i, j, notsup;
	ub2		nRlen2, nRcode;
	ub1		supchr;
	struct map_xfd *col;

	if (fx->key[idx]->count_eq.handle == NULL) {
		if (chkSts(db,(char*)"Alloc stmtHandle",
				OCIHandleAlloc(db->dbEnvH,&fx->key[idx]->count_eq.handle,OCI_HTYPE_STMT, 0, NULL))){
			DEBUG_LOG("db",("OCIHandleAlloc status %d; Failed!\n",db->dbStatus));
			return db->dbStatus;
		}
	}

	if (fx->key[idx]->count_eq.text == NULL)
		cob_sql_select (db, fx, idx, COB_COUNT, 0, oci_free_stmt);
	varFetch[0] = 0;
	if (f->keys[idx].tf_suppress) {
		notsup = 0;
		if (f->keys[idx].len_suppress <= 1) {
			supchr = f->keys[idx].char_suppress;
			for (j=0; j < fx->key[idx]->ncols && !notsup; j++) {
				col = &fx->map[fx->key[idx]->col[j]];
				if (col->sdata[0] == supchr) {
					for (i=0; i < col->sqlColSize && col->sdata[i] == f->keys[idx].char_suppress; i++);
					if (i < col->sqlColSize)
						notsup = 1;
				}
			}
			if (notsup) 	/* This key value is suppressed */
				return -1;
		} else {
			col = &fx->map[fx->key[idx]->col[0]];
			if (memcmp(col->sdata, f->keys[idx].str_suppress, f->keys[idx].len_suppress) == 0)
				return -1;
		}
	}

	if (!fx->key[idx]->count_eq.preped) {
		if(chkSts(db,(char*)"Peek prepare", 
					OCIStmtPrepare(fx->key[idx]->count_eq.handle,db->dbErrH,
									(text*)fx->key[idx]->count_eq.text,
									strlen(fx->key[idx]->count_eq.text),OCI_NTV_SYNTAX,OCI_DEFAULT))) {
			DEBUG_LOG("db",("OCIStmtPrepare status %d; Failed!\n",db->dbStatus));
		} else {
			pos = 0;
			for (j=0; j < fx->key[idx]->ncols; j++) {
				bindParam (db, fx, &fx->key[idx]->count_eq, &fx->map[fx->key[idx]->col[j]], ++pos);
			}
			fx->key[idx]->count_eq.preped = 1;
		}
	}
	db->dbStatus = 0;
	chkSts(db,(char*)"Peek Exec",
			OCIStmtExecute(db->dbSvcH,fx->key[idx]->count_eq.handle,db->dbErrH,
									0,0,NULL,NULL,OCI_DEFAULT));
	chkSts(db,(char*)"Peek Define",
		OCIDefineByPos(fx->key[idx]->count_eq.handle, (OCIDefine **)&db->dbBindV, db->dbErrH,
			(ub4)1, (ub1*)varFetch, (sword)sizeof(varFetch),
			(ub2)SQLT_CHR, (ub2*)NULL,
			(ub2*)&nRlen2, (ub2*)&nRcode, OCI_DEFAULT));
	if (chkSts(db,(char*)"Peek Fetch",
		OCIStmtFetch2(fx->key[idx]->count_eq.handle,db->dbErrH,1,OCI_FETCH_NEXT,0,OCI_DEFAULT))) {
		return 0;
	}
	rtn = atoi(varFetch);
	return rtn;
}

static void
oci_create_table (
	struct db_state	*db,
	struct file_xfd *fx)
{
	int	k;
	cob_load_ddl (db, fx);
	if (fx->create_table == NULL) {
		db->dbStatus = db->dbStsNoTable;
		return;
	}
	if (ociStmt (db, fx->create_table)) {
		db->dbStatus = db->dbStsNoTable;
		return;
	}
	if (fx->fileorg == COB_ORG_RELATIVE) 
		return;
	for (k=0; k < fx->nkeys && fx->key[k]->create_index; k++) {
		if (ociStmt (db, fx->key[k]->create_index)) {
			DEBUG_LOG ("db",("k%d: %s\n",k,fx->key[k]->create_index)); 
			db->dbStatus = db->dbStsNoTable;
			return;
		}
	}
}

/* INDEXED */

static void
join_environment (cob_file_api *a)
{
	char	*env, *p, tmp[256];

	db_join = -1;
	db->isopen = FALSE;
	memset(db,0,sizeof(struct db_state));
	db->dbStsOk			= 0;
	db->dbStsDupKey		= 1;
	db->dbStsNotFound	= 1403;
	db->dbStsNotFound2	= 100;
	db->dbStsNoTable	= 942;
	db->isoci			= TRUE;
	db->isodbc			= FALSE;
	db->oracle			= TRUE;
	db->dbStsRecLock	= 54;		/*  Oracle row locked by other */
	strcpy(db->dbType,"Oracle OCI");
	db->dbStsDeadLock	= 60;
	db->dbStsNoSpace	= 1653;
	db->dbStsNullCol	= 1405;
	db->dbStsInvlNum	= 1722;
	db->dbStsBadRowid	= 1410;
	if ((env=getSchemaEnvName(db,tmp,"_HOME",NULL)) != NULL) {
		DEBUG_LOG("db",("Env: %s -> %s\n",tmp,env));
		db->dbHome = env;
	} else {
		db->dbHome = (char*)"/usr/oracle";
	}

	if (chkSts(db,(char*)"Alloc EnvH",
		OCIEnvCreate((OCIEnv**)&db->dbEnvH,OCI_DEFAULT|OCI_NO_UCB, 
					NULL, NULL, NULL, NULL, 0, NULL))) {
		DEBUG_LOG("db",("OCIAllocHandle Env status %d; Failed!\n",db->dbStatus));
		return;
	}
	if (chkSts(db,(char*)"Alloc SvcH",
		OCIHandleAlloc(db->dbEnvH, &db->dbSvcH, OCI_HTYPE_SVCCTX, 0, NULL))) {
		DEBUG_LOG("db",("OCIAllocHandle Svc status %d; Failed!\n",db->dbStatus));
		return;
	}
	if (chkSts(db,(char*)"Alloc ErrH",
		OCIHandleAlloc(db->dbEnvH, &db->dbErrH, OCI_HTYPE_ERROR, 0, NULL))) {
		DEBUG_LOG("db",("OCIAllocHandle Err status %d; Failed!\n",db->dbStatus));
		return;
	}
	if (chkSts(db,(char*)"Alloc SvrH",
		OCIHandleAlloc(db->dbEnvH, &db->dbSvrH, OCI_HTYPE_SERVER, 0, NULL))) {
		DEBUG_LOG("db",("OCIAllocHandle Svr status %d; Failed!\n",db->dbStatus));
		return;
	}
	if (chkSts(db,(char*)"Alloc SesH",
		OCIHandleAlloc(db->dbEnvH, &db->dbSesH, OCI_HTYPE_SESSION, 0, NULL))) {
		DEBUG_LOG("db",("OCIAllocHandle Ses status %d; Failed!\n",db->dbStatus));
		return;
	}
	if (chkSts(db,(char*)"Alloc Env",
		OCIEnvCreate((OCIEnv**)&db->dbEnvH, OCI_OBJECT, 0, NULL, NULL, NULL,0,NULL))) {
		DEBUG_LOG("db",("OCIEnvCreate OBJECT status %d; Failed!\n",db->dbStatus));
		return;
	}

	if ((env=getSchemaEnvName(db,tmp,"_CON",db->dbCon)) != NULL) {
		DEBUG_LOG("db",("Env: %s -> %s\n",tmp,env));
		splitConnectString (db, env);
	} else {
		if ((env=getSchemaEnvName(db,tmp,"_SID",db->dbSid)) != NULL) {
			DEBUG_LOG("db",("Env: %s -> %s\n",tmp,env));
		}
		if ((env=getSchemaEnvName(db,tmp,"_UID",db->dbUser)) != NULL) {
			DEBUG_LOG("db",("Env: %s -> %s\n",tmp,env));
		}
		if ((env=getSchemaEnvName(db,tmp,"_PWD",db->dbPwd)) != NULL) {
			DEBUG_LOG("db",("Env: %s -> %s\n",tmp,env));
		}
	}
	if((env=getSchemaEnvName(db,tmp,"_COMMIT",NULL)) != NULL) {
		DEBUG_LOG("db",("Env: %s -> %s\n",tmp,env));
		db->commitInterval = atoi(env);
	} else {
		db->commitInterval = (int)BIGCOMMIT;
	}
	if (db->dbName[0] > ' ' 
	 && db->attachDbName) {
		sprintf(tmp,"Attach DBNAME=%s",db->dbName);
		if (chkSts(db, (char*)tmp, 
				OCIServerAttach(db->dbSvrH, db->dbErrH,
							(text*)db->dbName, strlen(db->dbName), OCI_DEFAULT) ) ) {
			oci_free_all_handles (db);
			return;
		}
	} else {
		sprintf(tmp,"Attach Default %s",db->dbSid);
		if (chkSts(db, (char*)tmp, 
				OCIServerAttach(db->dbSvrH, db->dbErrH,
							(text*)NULL, 0, OCI_DEFAULT) ) ) {
			oci_free_all_handles (db);
			return;
		}
	}

	chkSts(db,(char*)"AttrSet",OCIAttrSet(	db->dbSvcH, OCI_HTYPE_SVCCTX,
								db->dbSvrH, 0, OCI_ATTR_SERVER, db->dbErrH ));
	if(db->dbStatus) {
		DEBUG_LOG("db",("OCIAttrSet Server status %d; Failed!\n",db->dbStatus));
		return;
	}
	DEBUG_LOG("db",("OCIAttrSet Server ready!\n"));

	chkSts(db,(char*)"AttrSet",OCIAttrSet(	db->dbSvcH, OCI_HTYPE_SVCCTX,
								db->dbSvrH, 0, OCI_ATTR_SERVER, db->dbErrH ));
	if(db->dbStatus) {
		DEBUG_LOG("db",("OCIAttrSet Server status %d; Failed!\n",db->dbStatus));
		return;
	}

	chkSts(db,(char*)"AttrSet Uid",OCIAttrSet(	db->dbSesH, OCI_HTYPE_SESSION,
								(text *)db->dbUser, strlen(db->dbUser), 
										OCI_ATTR_USERNAME, db->dbErrH ));
	if(db->dbStatus) {
		DEBUG_LOG("db",("OCIAttrSet User status %d; Failed!\n",db->dbStatus));
		return;
	}

	chkSts(db,(char*)"AttrSet Pwd",OCIAttrSet(	db->dbSesH, OCI_HTYPE_SESSION,
								(text *)db->dbPwd, strlen(db->dbPwd), 
										OCI_ATTR_PASSWORD, db->dbErrH ));
	if(db->dbStatus) {
		DEBUG_LOG("db",("OCIAttrSet Password status %d; Failed!\n",db->dbStatus));
		return;
	}

	chkSts(db, (char*)"Session Begin", OCISessionBegin(db->dbSvcH, db->dbErrH, db->dbSesH,
												OCI_CRED_RDBMS, OCI_DEFAULT));
	if (db->dbStatus == -1017
	 || db->dbStatus ==  1017) { /* Invalid User/pass */
		DEBUG_LOG("db",(" %s: User %s, Pwd %s\n",db->dbType,db->dbUser,db->dbPwd));
		return;
	}
	if (db->dbStatus) {
		DEBUG_LOG("db",("SessionBegin status %d; Failed!\n",db->dbStatus));
		DEBUG_LOG("db",("%s: User %s, Pwd %s\n",db->dbType,db->dbUser,db->dbPwd));
		return;
	}
	if (chkSts(db,(char*)"AttrSet Ses",OCIAttrSet( db->dbSvcH, OCI_HTYPE_SVCCTX,
								db->dbSesH, 0, OCI_ATTR_SESSION, db->dbErrH ))) {
		DEBUG_LOG("db",("OCIAttrSet Session status %d; Failed!\n",db->dbStatus));
		return;
	}

	if (chkSts(db,(char*)"AttrSet Ses",
			OCIServerVersion( db->dbSvcH, db->dbErrH, 
							(text*)tmp, sizeof(tmp), OCI_HTYPE_SVCCTX))) {
		DEBUG_LOG("db",("OCIAttrSet Session status %d; Failed!\n",db->dbStatus));
		return;
	}
	DEBUG_LOG("db",("%s\n",tmp));
	if ((env = cob_str_case_str (tmp,"Release")) != NULL) {
		int	num = 0;
		env += 8;
		while(*env == ' ') env++;
		for (p=env; *p != ' '; p++) {
			if (*p == '.') num++;
			if (num > 1) break;
		}
		*p = 0;
		snprintf(db->dbType,sizeof(db->dbType),"OCI Oracle %s",env);
		db->dbVer = atoi(env);
	}
	if (db->dbVer < 10)
		db->dbVer = 10;

	if ((env=getSchemaEnvName(db,tmp,"_TRC",NULL)) != NULL) {
		if (ociStmt(db,(char*)"ALTER SESSION SET SQL_TRACE = TRUE"))
			return;
	}
	if (db->oracle) {
		/* The runtime code uses DECIMAL POINT internally */
		if (ociStmt(db,(char*)"ALTER SESSION SET NLS_NUMERIC_CHARACTERS = '.,'")) {
			return;
		}

		/* Set The default format for handling DATE fields */
		if (db->dateFormat != NULL
		&& strlen(db->dateFormat) > 0) {
			DEBUG_LOG("db",("NOTE: Default DATE field format is '%s'\n",db->dateFormat));
			sprintf(tmp,"ALTER SESSION SET NLS_DATE_FORMAT = '%s'",db->dateFormat);
			if (ociStmt(db,tmp)) {
				return;
			}
		} else {
			db->dateFormat = (char*)"YYYYMMDD";
			DEBUG_LOG("db",("NOTE: Set DATE format to '%s'\n",db->dateFormat));
			sprintf(tmp,"ALTER SESSION SET NLS_DATE_FORMAT = '%s'",db->dateFormat);
			if (ociStmt(db,tmp)) {
				return;
			}
		}
		strcpy(tmp,"ALTER SESSION SET OPTIMIZER_MODE = FIRST_ROWS");
		DEBUG_LOG("db",("NOTE: %s\n",tmp));
		ociStmt(db,tmp);
	}

	db_join = 0;			/* All connect steps completed */
	DEBUG_LOG("db",("%s successful connection\n",db->dbType));
	db->isopen = TRUE;
	db->autocommit = TRUE;	/* Default to AUTO COMMIT every update */
}

/* Delete file */
static int
oci_file_delete (cob_file_api *a, cob_file *f, char *filename)
{
	struct indexed_file	*p;
	char		buff[COB_FILE_MAX+1];
	struct file_xfd	*fx;

	DEBUG_LOG("db",("DELETE FILE %s\n",f->select_name));
	if (db_join) {			/* Join DataBase, on first OPEN of INDEXED file */
		join_environment (a);
		if (db_join < 0) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
	}
	if (f->file == NULL) {
		fx = cob_load_xfd (f, NULL, sizeof(ub2));
		if (fx == NULL) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		fx->gentable = a->setptr->cob_create_table;
		p = cob_malloc (sizeof (struct indexed_file));
		f->file = p;
		f->flag_file_lock = 0;	
		f->curkey = -1;
		p->fx = fx;
	}
	p = f->file;
	fx = p->fx;
	snprintf(buff,sizeof(buff),"DROP TABLE %s",fx->tablename);
	if (f->open_mode == COB_OPEN_CLOSED) {
		oci_close (a, f, 0);
	}
	DEBUG_LOG("db",("%s\n",buff));
	if (ociStmt(db,buff)
	 && db->dbStatus == db->dbStsNoTable) {
		return 0;
	} 
	if (db->dbStatus != db->dbStsOk) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}

	return 0;
}

/* OPEN INDEXED file */
static int
oci_open (cob_file_api *a, cob_file *f, char *filename, const int mode, const int sharing)
{
	struct indexed_file	*p;
	int				i, k, ln;
	char		buff[COB_FILE_MAX+1];
#ifdef COB_DEBUG_LOG
	const char	*optyp = "?";
#endif
	struct file_xfd	*fx;

	fx = cob_load_xfd (f, NULL, sizeof(ub2));
	if (fx == NULL) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	fx->gentable = a->setptr->cob_create_table;
#ifdef COB_DEBUG_LOG
	if (mode == COB_OPEN_INPUT)
		optyp = "INPUT";
	else if (mode == COB_OPEN_I_O)
		optyp = "IO";
	else if (mode == COB_OPEN_OUTPUT)
		optyp = "OUTPUT";
	else
		optyp = "EXTEND";
#endif
	if (db_join) {			/* Join DataBase, on first OPEN of INDEXED file */
		join_environment (a);
		if (db_join < 0) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
	}

	p = cob_malloc (sizeof (struct indexed_file));
	f->file = p;
	f->flag_file_lock = 0;	
	f->curkey = -1;
	p->startcond = -1;
	p->fx = fx;
	p->primekeylen = db_keylen (f, 0);
	p->maxkeylen = p->primekeylen;
	for (i=1; i < MAXNUMKEYS && i < f->nkeys; i++) {
		ln = db_keylen (f, i);
		if (ln < 0)
			break;
		if (ln > p->maxkeylen)
			p->maxkeylen = ln;
	}

	switch (mode) {
	case COB_OPEN_OUTPUT:
		snprintf(buff,sizeof(buff),"TRUNCATE TABLE %s",fx->tablename);
		if (ociStmt(db,buff)
		 && db->dbStatus == db->dbStsNoTable) {
			oci_create_table (db, fx);
		} 
		if (db->dbStatus != db->dbStsOk) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		break;
	case COB_OPEN_I_O:
	case COB_OPEN_EXTEND:
	case COB_OPEN_INPUT:
		snprintf(buff,sizeof(buff),"SELECT 1 FROM %s WHERE 1 = 0",fx->tablename);
		if (ociStmt(db,buff)
		 && db->dbStatus == db->dbStsNoTable) {
			oci_create_table (db, fx);
			if (db->dbStatus != db->dbStsOk)
				return COB_STATUS_30_PERMANENT_ERROR;
		} else if (db->dbStatus != db->dbStsNotFound) {
				return COB_STATUS_30_PERMANENT_ERROR;
		}
		break;
	}

	snprintf(buff,sizeof(buff),"SELECT MAX(rid_%s) FROM %s",fx->tablename,fx->tablename);
	strcpy(varFetch,"0");
	if (mode != COB_OPEN_OUTPUT
	 && !ociStmt(db,(char*)buff)) {
		f->max_rec_num = atol (varFetch);
	}

	if ((f->share_mode & COB_SHARE_NO_OTHER)
	 || (f->lock_mode & COB_FILE_EXCLUSIVE) ) {
		p->lmode = LEXCLLOCK;
	} else if ((f->lock_mode & COB_LOCK_AUTOMATIC) && mode != COB_OPEN_INPUT) {
		p->lmode = LAUTOLOCK;
	} else {
		p->lmode = LMANULOCK;
	}

	if (p->lmode == LEXCLLOCK) {
		if (db->mysql) {
			snprintf(buff,sizeof(buff),"LOCK TABLES %s %s",fx->tablename,
						mode == COB_OPEN_INPUT?"READ":"WRITE");
			if (ociStmt(db,buff))
				return COB_STATUS_30_PERMANENT_ERROR;
		}
	}

	f->open_mode = mode;
	f->last_open_mode = mode;
	f->flag_nonexistent = 0;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
	p->savekey = cob_malloc ((size_t)(p->maxkeylen + 1));
	p->suppkey = cob_malloc ((size_t)(p->maxkeylen + 1));
	p->saverec = cob_malloc ((size_t)(f->record_max + 1));
	for (k=0; k < fx->nmap; k++) {
		if (fx->map[k].cmd == XC_DATA
		 && fx->map[k].colname) {
			fx->map[k].hostType = SQLT_STR;
			if (fx->map[k].dtfrm) {
				fx->map[k].sqlType = SQLT_DAT;
			} else if (fx->map[k].type == COB_XFDT_FLOAT) {
				if (fx->map[k].size == sizeof(double))
					fx->map[k].hostType = SQLT_FLT;
				else
					fx->map[k].hostType = SQLT_FLT;
				fx->map[k].sqlType = SQLT_FLT;
				fx->map[k].sqlColSize = fx->map[k].size;
			} else if (fx->map[k].type == COB_XFDT_BIN) {
				fx->map[k].sqlColSize = fx->map[k].size;
				fx->map[k].hostType = SQLT_BIN;
				fx->map[k].sqlType = SQLT_BIN;
			} else if (fx->map[k].valnum) {
				fx->map[k].sqlType = SQLT_NUM;
			} else {
				fx->map[k].sqlType = SQLT_CHR;
			}
		}
	}
	DEBUG_LOG("db",("OPEN %s %s\n",optyp,f->select_name));

	return COB_STATUS_00_SUCCESS;
}

/* Close the INDEXED file */

static int
oci_close (cob_file_api *a, cob_file *f, const int opt)
{
	struct indexed_file	*p;
	struct file_xfd	*fx;
	int		k;

	if (opt == COB_CLOSE_ABORT) {
		oci_rollback (a, f);
	} else
	if (db->updatesDone > 0) {
		db->updatesDone = db->commitInterval + 1;	/* Force COMMIT */
		oci_commit (a, f);
	}
	p = f->file;

	if (p) {
		if (p->fx) {
			fx = p->fx;
			oci_free_stmt  (&fx->insert);
			oci_free_stmt  (&fx->delete);
			oci_free_stmt  (&fx->update);
			oci_free_stmt  (fx->start);
			fx->start = NULL;
			for (k=0; k < fx->nkeys; k++) {
				oci_free_stmt  (&fx->key[k]->count_eq);
				oci_free_stmt  (&fx->key[k]->where_eq);
				oci_free_stmt  (&fx->key[k]->where_ge);
				oci_free_stmt  (&fx->key[k]->where_gt);
				oci_free_stmt  (&fx->key[k]->where_le);
				oci_free_stmt  (&fx->key[k]->where_lt);
				oci_free_stmt  (&fx->key[k]->where_ne);
				oci_free_stmt  (&fx->key[k]->where_fi);
				oci_free_stmt  (&fx->key[k]->where_la);
			}
			cob_drop_xfd (fx);
		}
		if (p->savekey != NULL) cob_free (p->savekey);
		if (p->suppkey != NULL) cob_free (p->suppkey);
		if (p->saverec != NULL) cob_free (p->saverec);
		cob_free (p);
	}
	f->file = NULL;
	f->open_mode = COB_OPEN_CLOSED;
	DEBUG_LOG("db",("CLOSE %s\n",f->select_name));

	return COB_STATUS_00_SUCCESS;
}


/* START INDEXED file with positioning */

static int
oci_start (cob_file_api *a, cob_file *f, const int cond, cob_field *key)
{
	int		ky, klen, partlen, paramtype;
	struct indexed_file	*p;
	struct file_xfd	*fx;
	COB_UNUSED (a);

	ky = cob_findkey (f, key, &klen, &partlen);
	if (ky < 0) {
		DEBUG_LOG("db",("Start key not found!\n"));
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	p = f->file;
	fx = p->fx;
	p->startcond = cond;
	f->curkey = ky;
	paramtype = SQL_BIND_NO;

	oci_close_stmt (fx->start);
	fx->start = NULL;
	switch (cond) {
	case COB_EQ:
	case COB_NE:
		fx->start = cob_sql_select (db, fx, ky, cond, 0, oci_free_stmt);
		paramtype = SQL_BIND_EQ;
		break;
	case COB_GE:
	case COB_GT:
	case COB_LE:
	case COB_LT:
		fx->start = cob_sql_select (db, fx, ky, cond, 0, oci_free_stmt);
		paramtype = SQL_BIND_WHERE;
		break;
	case COB_FI:
		fx->start = cob_sql_select (db, fx, ky, cond, 0, oci_free_stmt);
		paramtype = SQL_BIND_NO;
		break;
	case COB_LA:
		fx->start = cob_sql_select (db, fx, ky, cond, 0, oci_free_stmt);
		paramtype = SQL_BIND_NO;
		break;
	}
	DEBUG_LOG("db",("Start %s index %d  Bind %02X\n",f->select_name,ky,paramtype));
	cob_index_to_xfd (db, fx, f, ky);
	oci_setup_stmt (db, fx, fx->start, SQL_BIND_COLS|paramtype, ky);
	if (fx->start->status) {
		fx->start = NULL;
		cob_sql_dump_data (db, fx);
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	cob_sql_dump_stmt (db, fx->start->text, FALSE);
	if (chkSts(db,(char*)"Start",
			OCIStmtExecute(db->dbSvcH,fx->start->handle,db->dbErrH,
			0,0,NULL,NULL,OCI_DEFAULT))){
		return COB_STATUS_30_PERMANENT_ERROR;
	}

	return COB_STATUS_00_SUCCESS;
}

/* Random READ of the INDEXED file  */

static int
oci_read (cob_file_api *a, cob_file *f, cob_field *key, const int read_opts)
{
	struct indexed_file	*p;
	struct file_xfd	*fx;
	struct map_xfd *col;
	int			k, ky, pos, klen, partlen;
	int			ret = COB_STATUS_00_SUCCESS;
	COB_UNUSED (a);

	p = f->file;
	fx = p->fx;
	if (fx->fileorg == COB_ORG_RELATIVE) {
		ky = 0;
	} else {
		ky = cob_findkey (f, key, &klen, &partlen);
		if (ky < 0) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
	}
	f->curkey = ky;
	p->startcond = -1;
	if (fx->start)
		oci_close_stmt (fx->start);
	fx->start = cob_sql_select (db, fx, ky, COB_EQ, read_opts, oci_free_stmt);
	oci_close_stmt (fx->start);
	cob_index_to_xfd (db, fx, f, ky);
	oci_set_nulls (db, fx);
	oci_setup_stmt (db, fx, fx->start, SQL_BIND_COLS, 0);
	if (fx->start->status) {
		fx->start = NULL;
		cob_sql_dump_data (db, fx);
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	pos = 0;
	for (k=0; k < fx->key[ky]->ncols; k++) {
		col = &fx->map[fx->key[ky]->col[k]];
		bindParam (db, fx, fx->start, col, ++pos);
	}
	if (chkSts(db,(char*)"Read Exec",
			OCIStmtExecute(db->dbSvcH,fx->start->handle,db->dbErrH,
							0,0,NULL,NULL,OCI_DEFAULT))){
		if (db->dbStatus == db->dbStsNotFound)
			return COB_STATUS_23_KEY_NOT_EXISTS;
		if (db->dbStatus == 30006)
			return COB_STATUS_61_FILE_SHARING;
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	if (chkSts(db,(char*)"Read", 
			OCIStmtFetch(fx->start->handle,db->dbErrH,
					1,OCI_FETCH_NEXT,OCI_DEFAULT))) {
		DEBUG_LOG("db",("Read: %.40s...; Sts %d '%.5s'\n",fx->start->text,
									db->dbStatus,db->odbcState));
		cob_sql_dump_stmt (db, fx->start->text, FALSE);
		cob_sql_dump_index (db, fx, ky);
		if (db->dbStatus == db->dbStsNotFound)
			ret = COB_STATUS_23_KEY_NOT_EXISTS;
		else if (db->dbStatus == 30006)
			ret = COB_STATUS_61_FILE_SHARING;
		else
			ret = COB_STATUS_30_PERMANENT_ERROR;
	} else {
		DEBUG_LOG("db",("Read: %s; OK\n",f->select_name));
		oci_any_nulls (db, fx);
		cob_xfd_to_file (db, fx, f);
	}

	return ret;
}

/* Sequential READ of the INDEXED file */

static int
oci_read_next (cob_file_api *a, cob_file *f, const int read_opts)
{
	struct indexed_file	*p;
	struct file_xfd	*fx;
	int			ky;
	int			opts = (int)read_opts & COB_READ_MASK;
	int			ret = COB_STATUS_00_SUCCESS;
	ub4			excmode = OCI_DEFAULT;
	COB_UNUSED (a);

	if (f->open_mode == COB_OPEN_CLOSED)
		return COB_STATUS_49_I_O_DENIED;
	p = f->file;
	fx = p->fx;
	if (f->curkey < 0) {
		f->curkey = 0;
		cob_index_clear (db, fx, f, 0);
		opts =  COB_READ_FIRST;
		cob_sql_dump_index (db, fx, 0);
	}
	ky = f->curkey;
	if (f->flag_read_chk_dups
	 && f->curkey > 0
	 && f->keys[f->curkey].tf_duplicates == 1) {
		excmode = OCI_STMT_SCROLLABLE_READONLY;
	}
	switch (opts & COB_READ_MASK) {
	default:
    case COB_READ_NEXT:                 
		if (p->startcond != COB_GT) {
			fx->start = cob_sql_select (db, fx, ky, COB_GT, read_opts, oci_free_stmt);
			oci_close_stmt (fx->start);
			oci_setup_stmt (db, fx, fx->start, SQL_BIND_COLS|SQL_BIND_WHERE, f->curkey);
			if (chkSts(db,(char*)"Read Next Exec",
					OCIStmtExecute(db->dbSvcH,fx->start->handle,db->dbErrH,
							0,0,NULL,NULL,excmode))){
				return COB_STATUS_30_PERMANENT_ERROR;
			}
			p->startcond = COB_GT;
		}
		if (fx->start
		 && !fx->start->isdesc) {
			if (chkSts(db,(char*)"Read Next",
					OCIStmtFetch2(fx->start->handle,db->dbErrH,
							1,OCI_FETCH_NEXT,0,OCI_DEFAULT))) {
				DEBUG_LOG("db",("Read Next: %.50s; Sts %d\n",fx->start->text,db->dbStatus));
				if (db->dbStatus == db->dbStsNotFound)
					ret = COB_STATUS_10_END_OF_FILE;
				else
					ret = COB_STATUS_30_PERMANENT_ERROR;
			} else {
				DEBUG_LOG("db",("Read Next: %s; OK\n",f->select_name));
				oci_any_nulls (db, fx);
				cob_xfd_to_file (db, fx, f);
			}
		} else {
			ret = COB_STATUS_10_END_OF_FILE;
		}
		break;
	case COB_READ_PREVIOUS:
		if (p->startcond != COB_LT) {
			fx->start = cob_sql_select (db, fx, ky, COB_LT, read_opts, oci_free_stmt);
			oci_close_stmt (fx->start);
			oci_setup_stmt (db, fx, fx->start, SQL_BIND_COLS|SQL_BIND_WHERE, f->curkey);
			if (chkSts(db,(char*)"Read Prev Exec",
					OCIStmtExecute(db->dbSvcH,fx->start->handle,db->dbErrH,
							0,0,NULL,NULL,excmode))){
				return COB_STATUS_30_PERMANENT_ERROR;
			}
			p->startcond = COB_LT;
		}
		if (fx->start
		 && fx->start->isdesc) {
			if (chkSts(db,(char*)"Read Prev",
					OCIStmtFetch2(fx->start->handle,db->dbErrH,
							1,OCI_FETCH_NEXT,0,OCI_DEFAULT))) {
				DEBUG_LOG("db",("Read Prev: %.50s; Sts %d\n",fx->start->text,db->dbStatus));
				if (db->dbStatus == db->dbStsNotFound)
					ret = COB_STATUS_10_END_OF_FILE;
				else
					ret = COB_STATUS_30_PERMANENT_ERROR;
			} else {
				DEBUG_LOG("db",("Read Prev: %s; OK\n",f->select_name));
				oci_any_nulls (db, fx);
				cob_xfd_to_file (db, fx, f);
			}
		} else {
			ret = COB_STATUS_10_END_OF_FILE;
		}
		break;
	case COB_READ_FIRST:
		fx->start = cob_sql_select (db, fx, ky, COB_FI, read_opts, oci_free_stmt);
		oci_close_stmt (fx->start);
		oci_setup_stmt (db, fx, fx->start, SQL_BIND_COLS, 0);
		if (fx->start->status) {
			fx->start = NULL;
			cob_sql_dump_data (db, fx);
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		p->startcond = COB_GT;
		if (chkSts(db,(char*)"Exec First",
				OCIStmtExecute(db->dbSvcH,fx->start->handle,db->dbErrH,
							0,0,NULL,NULL,excmode))){
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		if (chkSts(db,(char*)"Read First", 
					OCIStmtFetch2(fx->start->handle,db->dbErrH,
							1,OCI_FETCH_NEXT,0,OCI_DEFAULT))) {
			DEBUG_LOG("db",("Read First: %.50s; Sts %d\n",fx->start->text,db->dbStatus));
			if (db->dbStatus == db->dbStsNotFound)
				ret = COB_STATUS_10_END_OF_FILE;
			else
				ret = COB_STATUS_30_PERMANENT_ERROR;
		} else {
			DEBUG_LOG("db",("Read First: %s; OK\n",f->select_name));
			oci_any_nulls (db, fx);
			cob_xfd_to_file (db, fx, f);
		}
		break;
	case COB_READ_LAST:
		fx->start = cob_sql_select (db, fx, ky, COB_LA, read_opts, oci_free_stmt);
		oci_close_stmt (fx->start);
		oci_setup_stmt (db, fx, fx->start, SQL_BIND_COLS, 0);
		if (fx->start->status) {
			fx->start = NULL;
			cob_sql_dump_data (db, fx);
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		if (chkSts(db,(char*)"Read Last",
				OCIStmtExecute(db->dbSvcH,fx->start->handle,db->dbErrH,
							0,0,NULL,NULL,excmode))){
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		p->startcond = COB_LT;
		if (chkSts(db,(char*)"Read Last",
					OCIStmtFetch2(fx->start->handle,db->dbErrH,
							1,OCI_FETCH_NEXT,0,OCI_DEFAULT))) {
			DEBUG_LOG("db",("Read Last: %.50s; Sts %d\n",fx->start->text,db->dbStatus));
			if (db->dbStatus == db->dbStsNotFound)
				ret = COB_STATUS_10_END_OF_FILE;
			else
				ret = COB_STATUS_30_PERMANENT_ERROR;
		} else {
			DEBUG_LOG("db",("Read Last: %s; OK\n",f->select_name));
			oci_any_nulls (db, fx);
			cob_xfd_to_file (db, fx, f);
		}
		break;
	}

	if (ret == COB_STATUS_00_SUCCESS
	 && f->flag_read_chk_dups
	 && f->curkey > 0
	 && f->keys[f->curkey].tf_duplicates == 1) {
		int	klen;
		memcpy (p->saverec, f->record->data, f->record_max);
		klen = db_savekey (f, p->savekey, f->record->data, f->curkey);
		if (! OCIStmtFetch2(fx->start->handle,db->dbErrH,
							1,OCI_FETCH_NEXT,0,OCI_DEFAULT)) {
			cob_xfd_to_file (db, fx, f);
			db_savekey (f, p->suppkey, f->record->data, f->curkey);
			if (memcmp(p->suppkey, p->savekey, klen) == 0) {
				ret = COB_STATUS_02_SUCCESS_DUPLICATE;
			}
			chkSts(db,(char*)"Peek reset",
					OCIStmtFetch2(fx->start->handle,db->dbErrH,
							1,OCI_FETCH_PRIOR,0,OCI_DEFAULT));
		}
		memcpy (f->record->data, p->saverec, f->record_max);
	}

	return ret;
}


/* WRITE to the INDEXED file  */

static int
oci_write (cob_file_api *a, cob_file *f, const int opt)
{
	struct indexed_file	*p;
	struct file_xfd	*fx;
	int			k, num;
	int			ret = COB_STATUS_00_SUCCESS;
	COB_UNUSED (a);

	p = f->file;
	fx = p->fx;
	if (fx->insert.text == NULL) {
		fx->insert.text = cob_sql_stmt (db, fx, (char*)"INSERT", 0, 0, 0);
	}

	cob_file_to_xfd (db, fx, f);

	oci_set_nulls (db, fx);
	if (f->flag_read_chk_dups) {
		for (k=1; k < fx->nkeys; k++) {
			if (f->keys[k].tf_duplicates == 1) {
				num = ociCountIndex (db, f, fx, k);
				if( num > 0)
					ret = COB_STATUS_02_SUCCESS_DUPLICATE;
			}
		}
	}

	if (!fx->insert.preped) {
		oci_setup_stmt (db, fx, &fx->insert, SQL_BIND_PRMS, 0);
	}
	if (chkSts(db,(char*)"Exec INSERT",
				OCIStmtExecute(db->dbSvcH,fx->insert.handle,db->dbErrH,
							1,0,NULL,NULL,OCI_DEFAULT))){
		if (db->dbStatus == db->dbStsDupKey) {
			DEBUG_LOG("db",("%.60s Duplicate; Failed!\n",fx->insert.text));
			ret = COB_STATUS_22_KEY_EXISTS;
		} else {
			DEBUG_LOG("db",("OCIExecute %.40s status %d; Failed!\n",fx->insert.text,db->dbStatus));
			ret = COB_STATUS_30_PERMANENT_ERROR;
			cob_sql_dump_data (db, fx);
		}
		return ret;
	}
	db->updatesDone++;
	if (db->dbStatus != 0) {
		DEBUG_LOG("db",("WRITE: %.40s... status %d; Not Good!\n",fx->insert.text,db->dbStatus));
	} else if (fx->fileorg == COB_ORG_RELATIVE) {
		DEBUG_LOG("db",("WRITE: %.40s... Rec# %d; Good!\n",fx->insert.text,(int)f->cur_rec_num));
	} else {
		DEBUG_LOG("db",("WRITE: %.40s...  Good!\n",fx->insert.text));
	}
	if (db->autocommit)
		oci_commit (a,f);

	return ret;
}


/* DELETE record from the INDEXED file  */

static int
oci_delete (cob_file_api *a, cob_file *f)
{
	struct indexed_file	*p;
	struct file_xfd	*fx;
	int			k, pos;
	int			ret = COB_STATUS_00_SUCCESS;
	COB_UNUSED (a);

	if (f->open_mode == COB_OPEN_INPUT
	 || f->open_mode == COB_OPEN_CLOSED)
		return COB_STATUS_49_I_O_DENIED;
	p = f->file;
	fx = p->fx;
	if (fx->delete.text == NULL) {
		fx->delete.text = cob_sql_stmt (db, fx, (char*)"DELETE", 0, 0, 0);
	}

	cob_index_to_xfd (db, fx, f, 0);

	if (!fx->delete.preped) {
		oci_setup_stmt (db, fx, &fx->delete, SQL_BIND_NO, 0);
		pos = 0;
		for (k=0; k < fx->key[0]->ncols; k++) {
			bindParam (db, fx, &fx->delete, &fx->map[fx->key[0]->col[k]], ++pos);
		}
	}
	if (chkSts(db,(char*)"Exec DELETE",
			OCIStmtExecute(db->dbSvcH,fx->delete.handle,db->dbErrH,
							1,0,NULL,NULL,OCI_DEFAULT))){
		DEBUG_LOG("db",("OCIExecute %.40s status %d; Failed!\n",fx->delete.text,db->dbStatus));
		if (db->dbStatus == db->dbStsNotFound)
			ret = COB_STATUS_23_KEY_NOT_EXISTS;
		else
			ret = COB_STATUS_21_KEY_INVALID;
		cob_sql_dump_data (db, fx);
		return ret;
	}
	k = oci_row_count (db, &fx->delete);
	if (k == 0)
		ret = COB_STATUS_23_KEY_NOT_EXISTS;
	else if (k > 1)
		ret = COB_STATUS_30_PERMANENT_ERROR;
	db->updatesDone++;
	DEBUG_LOG("db",("DELETE: %s status %d; %d deleted, return %02d\n",f->select_name,
							db->dbStatus,k,ret));
	if (db->autocommit)
		oci_commit (a,f);

	return ret;
}

/* REWRITE record to the INDEXED file  */

static int
oci_rewrite (cob_file_api *a, cob_file *f, const int opt)
{
	struct indexed_file	*p;
	struct file_xfd	*fx;
	int			k, pos, num, klen;
	int			ret = COB_STATUS_00_SUCCESS;
	COB_UNUSED (a);

	if (f->open_mode == COB_OPEN_INPUT
	 || f->open_mode == COB_OPEN_CLOSED)
		return COB_STATUS_49_I_O_DENIED;
	p = f->file;
	fx = p->fx;
	if (fx->update.text == NULL) {
		fx->update.text = cob_sql_stmt (db, fx, (char*)"UPDATE", 0, 0, 0);
	}

	if (f->flag_read_chk_dups) {
		DEBUG_LOG("db",("REWRITE: %s, begin check dups\n",f->select_name));
		memcpy (p->saverec, f->record->data, f->record_max);
		if(!oci_read (a, f, f->keys[0].field, 0)) {
			for (k=1; k < fx->nkeys && ret == COB_STATUS_00_SUCCESS; k++) {
				if (f->keys[k].tf_duplicates == 1) {
					klen = db_savekey (f, p->suppkey, f->record->data, k);
					db_savekey (f, p->savekey, p->saverec, k);
					if (memcmp(p->suppkey, p->savekey, klen) != 0) {
						cob_xfd_swap_data ((char*)p->saverec, (char*)f->record->data, f->record_max);
						cob_index_to_xfd (db, fx, f, k);	/* Put new data into Index */
						num = ociCountIndex (db, f, fx, k);
						if (num > 0) {
							ret = COB_STATUS_02_SUCCESS_DUPLICATE;
						}
						cob_xfd_swap_data ((char*)p->saverec, (char*)f->record->data, f->record_max);
						cob_index_to_xfd (db, fx, f, k);	/* Put old data back into Index */
					}
				}
			}
		}
		memcpy (f->record->data, p->saverec, f->record_max);
	}

	cob_file_to_xfd (db, fx, f);

	oci_set_nulls (db, fx);
	if (!fx->update.preped) {
		oci_setup_stmt (db, fx, &fx->update, SQL_BIND_PRMS, 0);
		pos = fx->update.bindpos;
		for (k=0; k < fx->key[0]->ncols; k++) {
			bindParam (db, fx, &fx->update, &fx->map[fx->key[0]->col[k]], ++pos);
		}
	}

	if (chkSts(db,(char*)"Exec UPDATE",
			OCIStmtExecute(db->dbSvcH,fx->update.handle,db->dbErrH,
							1,0,NULL,NULL,OCI_DEFAULT))){
		if (db->dbStatus == db->dbStsDupKey) {
			DEBUG_LOG("db",("%.60s Duplicate; Failed!\n",fx->update.text));
			ret = COB_STATUS_22_KEY_EXISTS;
		} else {
			DEBUG_LOG("db",("OCIExecute %.40s status %d; Failed!\n",fx->update.text,db->dbStatus));
			ret = COB_STATUS_30_PERMANENT_ERROR;
			cob_sql_dump_data (db, fx);
		}
		return ret;
	}
	k = oci_row_count (db, &fx->update);
	if (k == 0)
		ret = COB_STATUS_21_KEY_INVALID;
	else if (k > 1)
		ret = COB_STATUS_30_PERMANENT_ERROR;
	db->updatesDone++;
	DEBUG_LOG("db",("REWRITE: %s, status %d; %d updated, return %02d%s!\n",f->select_name,
						db->dbStatus,k,ret,db->autocommit?"; Commit":""));
	if (db->autocommit)
		oci_commit (a,f);

	return ret;
}


static int
oci_file_unlock (cob_file_api *a, cob_file *f)
{
	COB_UNUSED (a);
	COB_UNUSED (f);

	return 0;
}

/* Call this routine when a new process has been forked */
static int
oci_fork (cob_file_api *a)
{
	COB_UNUSED (a);
	return 0;
}

static void
oci_exit_fileio (cob_file_api *a)
{
	COB_UNUSED (a);
}

void
cob_oci_init_fileio (cob_file_api *a)
{
	a->io_funcs[COB_IO_OCI] = (void*)&oci_indexed_funcs;
}

#endif

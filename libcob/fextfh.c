/*
   Copyright (C) 2002-2012, 2014-2021 Free Software Foundation, Inc.
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
} *fcd_file_list = NULL;
static const cob_field_attr alnum_attr = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
static const cob_field_attr compx_attr = {COB_TYPE_NUMERIC_BINARY, 0, 0, 0, NULL};

static void copy_keys_fcd_to_file (FCD3 *fcd, cob_file *f, int doall);
static int	EXTFH3 (unsigned char *opcode, FCD3 *fcd);

/*
 * Free up allocated memory
 */
void
free_extfh_fcd (void)
{
	struct fcd_file	*ff,*nff;

	for(ff = fcd_file_list; ff; ff = nff) {
		nff = ff->next;
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
	else
		memcpy (fcd->fileStatus,f->file_status,2);
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
	} else {	/* CHECKME: is this correct? */
		STCOMPX4(f->record_max, fcd->curRecLen);
	}
	if (f->record_min == f->record_max)
		fcd->recordMode = REC_MODE_FIXED;
	else
		fcd->recordMode = REC_MODE_VARIABLE;
	if(f->organization == COB_ORG_SEQUENTIAL) {
		fcd->fileOrg = ORG_SEQ;
		STCOMPX2(0, fcd->refKey);
	} else if(f->organization == COB_ORG_LINE_SEQUENTIAL) {
		fcd->fileOrg = ORG_LINE_SEQ;
		STCOMPX2(0, fcd->refKey);
		if((f->file_features & COB_FILE_LS_CRLF))
			fcd->fstatusType |= MF_FST_CRdelim;
		if((f->file_features & COB_FILE_LS_NULLS))
			fcd->fstatusType |= MF_FST_InsertNulls;
		if((f->file_features & COB_FILE_LS_FIXED))
			fcd->fstatusType |= MF_FST_NoStripSpaces;
	} else if(f->organization == COB_ORG_RELATIVE) {
		fcd->fileOrg = ORG_RELATIVE;
		STCOMPX2(0, fcd->refKey);
	} else
	if (f->organization == COB_ORG_INDEXED) {
		fcd->fileOrg = ORG_INDEXED;
		fcd->fileFormat = MF_FF_CISAM;
		if (f->io_routine == COB_IO_VBISAM)
			fcd->fileFormat = MF_FF_VBISAM;
#ifdef WITH_DISAM
		else if (f->io_routine == COB_IO_DISAM)
			fcd->fileFormat = MF_FF_DISAM;
#endif
#ifdef WITH_DB
		else if (f->io_routine == COB_IO_BDB)
			fcd->fileFormat = MF_FF_BDB;
#endif
#ifdef WITH_LMDB
		else if (f->io_routine == COB_IO_LMDB)
			fcd->fileFormat = MF_FF_LMDB;
#endif
	}
}

/*
 * Copy 'cob_file' to FCD based information
 */
static void
copy_file_to_fcd (cob_file *f, FCD3 *fcd)
{
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
	if (f->trace_io)
		fcd->gcFlags |= MF_CALLFH_TRACE;
	else
		fcd->gcFlags &= ~MF_CALLFH_TRACE;
	if (f->io_stats)
		fcd->gcFlags |= MF_CALLFH_STATS;
	else
		fcd->gcFlags &= ~MF_CALLFH_STATS;
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
		if (f->flag_vb_isam) {
			fcd->fileFormat = MF_FF_VBISAM;
		} else if (f->io_routine == COB_IO_ODBC) {
			fcd->fileFormat = MF_FF_ODBC;
		} else if (f->io_routine == COB_IO_OCI) {
			fcd->fileFormat = MF_FF_OCI;
		} else if (f->io_routine == COB_IO_BDB) {
			fcd->fileFormat = MF_FF_BDB;
		} else if (f->io_routine == COB_IO_LMDB) {
			fcd->fileFormat = MF_FF_LMDB;
		}
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
			nkeys = f->nkeys < 16 ? f->nkeys : 16;
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
	update_file_to_fcd(f, fcd, NULL);
}

/*
 * Update 'cob_file' from 'FCD' information
 */
static void
update_fcd_to_file (FCD3* fcd, cob_file *f, cob_field *fnstatus, int wasOpen)
{
	int status;

	if (wasOpen >= 0) {
		cobglobptr->cob_error_file = f;
		if (isdigit(fcd->fileStatus[0]) && fcd->fileStatus[1] != '0') {
			cob_set_exception (status_exception[(fcd->fileStatus[0] - '0')]);
		} else {
			cobglobptr->cob_exception_code = 0;
		}
		memcpy(f->file_status, fcd->fileStatus, 2);
		if (fnstatus) {
			memcpy(fnstatus->data, fcd->fileStatus, 2);
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

	if (fcd->gcFlags & MF_CALLFH_TRACE)
		f->trace_io = 1;
	else
		f->trace_io = 0;
	if (fcd->gcFlags & MF_CALLFH_STATS)
		f->io_stats = 1;
	else
		f->io_stats = 0;

	if((fcd->lockMode & FCD_LOCK_EXCL_LOCK))
		f->lock_mode = COB_LOCK_EXCLUSIVE;
	else if((fcd->lockMode & FCD_LOCK_MANU_LOCK))
		f->lock_mode = COB_LOCK_MANUAL;
	else if((fcd->lockMode & FCD_LOCK_AUTO_LOCK))
		f->lock_mode = COB_LOCK_AUTOMATIC;

	if (wasOpen < 0)
		return;
	status = 0;
	if(isdigit(fcd->fileStatus[0])) {
		status = fcd->fileStatus[0] - '0';
	}
	status = status * 10;
	if(isdigit(fcd->fileStatus[1]))
		status += (fcd->fileStatus[1] - '0');

	/* Call save_status to get trace & stats done */
	cob_file_save_status (f, fnstatus, status);
}

static void
copy_keys_fcd_to_file (FCD3 *fcd, cob_file *f, int doall)
{
	int		k, p, parts, parts_seen, off, klen;
	EXTKEY	*key;
	f->flag_keycheck = 0;
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
copy_fcd_to_file (FCD3* fcd, cob_file *f)
{
	int	k, min, max;

	if(fcd->accessFlags == ACCESS_SEQ)
		f->access_mode = COB_ACCESS_SEQUENTIAL;
	else if(fcd->accessFlags == ACCESS_RANDOM)
		f->access_mode = COB_ACCESS_RANDOM;
	else if(fcd->accessFlags == ACCESS_DYNAMIC)
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

	if(fcd->fileOrg == ORG_INDEXED) {
		f->organization = COB_ORG_INDEXED;
		if (fcd->fileFormat == MF_FF_DEFAULT) {
#ifdef WITH_INDEXED
			f->io_routine = WITH_INDEXED;
#elif WITH_CISAM
			f->io_routine = COB_IO_CISAM;
#elif WITH_DISAM
			f->io_routine = COB_IO_DISAM;
#elif WITH_VISAM
			f->io_routine = COB_IO_VISAM;
#elif WITH_VBISAM
			f->io_routine = COB_IO_VBISAM;
#elif WITH_DB
			f->io_routine = COB_IO_BDB;
#elif WITH_LMDB
			f->io_routine = COB_IO_LMDB;
#endif
			f->flag_auto_type = 1;
			f->flag_keycheck = 0;
		} else if (fcd->fileFormat == MF_FF_CISAM)
			f->io_routine = COB_IO_CISAM;
		else if (fcd->fileFormat == MF_FF_DISAM)
			f->io_routine = COB_IO_DISAM;
		else if (fcd->fileFormat == MF_FF_VISAM)
			f->io_routine = COB_IO_VISAM;
		else if (fcd->fileFormat == MF_FF_VBISAM)
			f->io_routine = COB_IO_VBISAM;
		else if (fcd->fileFormat == MF_FF_BDB)
			f->io_routine = COB_IO_BDB;
		else if (fcd->fileFormat == MF_FF_LMDB)
			f->io_routine = COB_IO_LMDB;
	} else if(fcd->fileOrg == ORG_SEQ) {
		f->organization = COB_ORG_SEQUENTIAL;
	} else if(fcd->fileOrg == ORG_LINE_SEQ) {
		f->organization = COB_ORG_LINE_SEQUENTIAL;
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
	} else if(fcd->fileOrg == ORG_RELATIVE) {
		f->organization = COB_ORG_RELATIVE;
		if (f->keys == NULL)
			f->keys = cob_cache_malloc(sizeof(cob_file_key));
		if (f->keys[0].field == NULL)
			f->keys[0].field = cob_cache_malloc(sizeof(cob_field));
		f->keys[0].field->data = cob_cache_malloc (4);
		f->keys[0].field->attr = &compx_attr;
		f->keys[0].field->size = 4;
		if (!f->flag_set_type
		 && COB_MODULE_PTR
		 && COB_MODULE_PTR->flag_file_format == COB_FILE_IS_MF) {
			f->file_format = COB_FILE_IS_MF;
			f->flag_set_type = 1;
		}
	} else {
		f->organization = COB_ORG_MAX;
	}

	/* Try for some record size */
	min = LDCOMPX4(fcd->minRecLen);
	max = LDCOMPX4(fcd->maxRecLen);
	k   = LDCOMPX4(fcd->curRecLen);
	if (min < 0)	/* Should not happen, but we're unsigned ... */
		min = 0;
	f->record_min = min;
	if (max > 0) {
		f->record_max = max;
	} else if (k > 0) {
		max = k;
		STCOMPX4 (k, fcd->maxRecLen);
	}
	if (k < min)
		k = min;
		STCOMPX4 (min, fcd->curRecLen);
	if (k > max)
		k = max;
		STCOMPX4 (max, fcd->curRecLen);
	/* Allocate cob_file fields as needed and copy from FCD */
	if (f->record == NULL
	 && fcd->recPtr != NULL
	 && k > 0	/* CHECKME: should we allocate with 'max' instead? */) {
		f->record = cob_cache_malloc(sizeof(cob_field));
		f->record->data = fcd->recPtr;
		f->record->size = k;
		f->record->attr = &alnum_attr;
	}
	if (f->assign == NULL
	 && fcd->fnamePtr != NULL) {
		f->assign = cob_cache_malloc(sizeof(cob_field));
		f->assign->data = (unsigned char*)fcd->fnamePtr;
		f->assign->size = LDCOMPX2(fcd->fnameLen);
		f->assign->attr = &alnum_attr;
	}
	if (f->select_name == NULL) {
		char	fdname[49];
		f->select_name = (char*)f->assign->data;
		for (k=0; k < (int)(f->assign->size); k++) {
			if (f->assign->data[k] == '/') {
				f->select_name = (char*)&f->assign->data[k+1];
			}
		}
		for (k=0; f->select_name[k] > ' ' && k < 48; k++) {
			fdname[k] = (char)toupper((int)f->select_name[k]);
		}
		fdname[k] = 0;
		f->select_name = cob_strdup (fdname);
	}
	if (f->organization == COB_ORG_INDEXED) {
		if (f->keys == NULL) {
			if (fcd->kdbPtr != NULL
			 && LDCOMPX2(fcd->kdbPtr->nkeys) > 0) {
				/* Copy Key information from FCD to cob_file,
				   CHECKME: possibly only for ORG_DETERMINE ? */
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

/* Convert FCD2 into FCD3 format */
static FCD3 *
fcd2_to_fcd3 (FCD2 *fcd2)
{
	FCD3 *fcd;
	if (fcd2->fnamePtr2 == NULL) {
		fcd2->fileStatus[0] = '9';
		fcd2->fileStatus[1] = 141;
		cob_runtime_warning (_("ERROR: EXTFH called with no %s pointer"),"filename");
		return NULL;
	}
	fcd = find_fcd2 (fcd2);
	memcpy (fcd->fileStatus, "00", 2);
	fcd->fcdVer = FCD_VER_64Bit;
	STCOMPX2 (sizeof(FCD3), fcd->fcdLen);
	fcd->fileOrg = fcd2->fileOrg;
	fcd->accessFlags = fcd2->accessFlags;
	fcd->openMode = fcd2->openMode;
	fcd->recordMode = fcd2->recordMode;
	fcd->fileFormat = fcd2->fileFormat;
	fcd->lockMode = fcd2->lockMode;
	fcd->otherFlags = fcd2->otherFlags;
	fcd->fstatusType = fcd2->fstatusType;
	fcd->compType = fcd2->compType;
	fcd->gcFlags = fcd2->gcFlags;
	fcd->gcFlags |= MF_CALLFH_GNUCOBOL;
	fcd->lockMode = fcd2->lockMode;
	fcd->fsv2Flags = fcd2->fsv2Flags;
	fcd->confFlags = fcd2->confFlags;
	fcd->idxCacheSz = fcd2->idxCacheSz;
	fcd->idxCacheArea = fcd2->idxCacheArea;
	STCOMPX4 (LDCOMPX2 (fcd2->curRecLen), fcd->curRecLen);
	STCOMPX4 (LDCOMPX2 (fcd2->minRecLen), fcd->minRecLen);
	STCOMPX4 (LDCOMPX2 (fcd2->maxRecLen), fcd->maxRecLen);
	memcpy (fcd->refKey, fcd2->refKey, 2);
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
	STCOMPX2 (sizeof(FCD2), fcd2->fcdLen);
	memcpy (fcd2->fileStatus, fcd->fileStatus, 2);
	fcd2->fcdVer = FCD2_VER;
	fcd2->fileOrg = fcd->fileOrg;
	fcd2->accessFlags = fcd->accessFlags;
	fcd2->openMode = fcd->openMode;
	fcd2->recordMode = fcd->recordMode;
	fcd2->fileFormat = fcd->fileFormat;
	fcd2->lockMode = fcd->lockMode;
	fcd2->otherFlags = fcd->otherFlags;
	fcd2->fstatusType = fcd->fstatusType;
	fcd2->compType = fcd->compType;
	fcd2->gcFlags = fcd->gcFlags;
	fcd2->fsv2Flags = fcd->fsv2Flags;
	fcd2->confFlags = fcd->confFlags;
	fcd2->idxCacheSz = fcd->idxCacheSz;
	fcd2->idxCacheArea = fcd->idxCacheArea;
	STCOMPX2 (LDCOMPX4 (fcd->curRecLen), fcd2->curRecLen);
	STCOMPX2 (LDCOMPX4 (fcd->minRecLen), fcd2->minRecLen);
	STCOMPX2 (LDCOMPX4 (fcd->maxRecLen), fcd2->maxRecLen);
	memcpy (fcd2->refKey, fcd->refKey, 2);
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
			if (ff->f == NULL) {
				f = cob_cache_malloc (sizeof(cob_file));
				f->file_version = COB_FILE_VERSION;
				f->open_mode = COB_OPEN_CLOSED;
				ff->f = f;
				copy_fcd_to_file (fcd, f);
			}
			f = ff->f;
			goto returnit;
		}
	}
	f = cob_cache_malloc (sizeof(cob_file));
	f->file_version = COB_FILE_VERSION;
	f->open_mode = COB_OPEN_CLOSED;
	copy_fcd_to_file (fcd, f);
	ff = cob_cache_malloc (sizeof(struct fcd_file));
	ff->next = fcd_file_list;
	ff->fcd = fcd;
	ff->f = f;
	ff->free_fcd = 0;
	fcd_file_list = ff;

returnit:
	/* If record area not allocated then allocate it
	 * If record address has changed then adjust accordingly
	 *                   including the INDEXED keys
	 */
	if (f->record != NULL
	 && (f->record->data != fcd->recPtr
	  || f->record->data == NULL)) {
		 if (fcd->recPtr != NULL)
			f->record->data = fcd->recPtr;
		else
			f->record->data = cob_cache_malloc (f->record_max + 1);
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

	return f;
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

	f->last_operation = COB_LAST_OPEN;
	fcd = find_fcd(f);
	f->share_mode = (unsigned char)sharing;
	f->last_open_mode = (unsigned char)mode;
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
	if (memcmp(f->file_status,"00",2) == 0
	 || memcmp(f->file_status,"05",2) == 0) {
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

	f->last_operation = COB_LAST_CLOSE;
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

	f->last_operation = COB_LAST_START;
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

	f->last_operation = COB_LAST_READ;
	fcd = find_fcd(f);
	STCOMPX4 (read_opts, fcd->opt);
	if(key == NULL) {
		f->last_operation = COB_LAST_READ_SEQ;
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

	f->last_operation = COB_LAST_READ_SEQ;
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

	f->last_operation = COB_LAST_WRITE;
	fcd = find_fcd(f);
	STCOMPX2(OP_WRITE, opcode);
	STCOMPX2(check_eop, fcd->eop);
	STCOMPX4(opt, fcd->opt);
	if (f->variable_record) {
		f->record->size = (size_t)cob_get_int (f->variable_record);
		if (f->record->size > rec->size) {
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

	f->last_operation = COB_LAST_REWRITE;
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

	f->last_operation = COB_LAST_DELETE;
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
	 || COB_MODULE_PTR->cob_procedure_params[1]->size < 5) {
		cob_set_exception (COB_EC_PROGRAM_ARG_MISMATCH);
		return 1;	/* correct? */
	}
#if COB_64_BIT_POINTER
	/* Only FCD3 is accepted */
	if (COB_MODULE_PTR->cob_procedure_params[1]->size < sizeof(FCD3)) {
		fcd->fileStatus[0] = '9';
		fcd->fileStatus[1] = 161;
		if (fcd->fcdVer != FCD_VER_64Bit) {
			cob_set_exception (COB_EC_PROGRAM_ARG_MISMATCH);
#if 1
			cob_runtime_warning (_("ERROR: EXTFH called with FCD version %d"), fcd->fcdVer);
#else
			cob_runtime_error (_("ERROR: EXTFH called with FCD version %d"), fcd->fcdVer);
			exit(-1);
#endif
		}
		return 1;	/* correct? */
	}

	return EXTFH3 ((unsigned char *)opcode_ptr, fcd);
#else
	if (COB_MODULE_PTR->cob_procedure_params[1]->size < sizeof(FCD3)) {
		if (fcd->fcdVer == FCD2_VER
		 && COB_MODULE_PTR->cob_procedure_params[1]->size >= sizeof(FCD2)) {
			/* EXTFH with auto-conversion FCD2<->FCD3*/
			return EXTFH ((unsigned char *)opcode_ptr, fcd);
		}

		fcd->fileStatus[0] = '9';
		fcd->fileStatus[1] = 161;
		cob_set_exception (COB_EC_PROGRAM_ARG_MISMATCH);
		if (fcd->fcdVer != FCD_VER_64Bit) {
			cob_runtime_warning (_("ERROR: EXTFH called with FCD version %d"), fcd->fcdVer);
		}
		return 1;	/* correct? */
	}

	return EXTFH3 ((unsigned char *)opcode_ptr, fcd);
#endif
}

/* 
 * Sync FCD3 values to cob_file values
 */
void
cob_fcd_file_sync (cob_file *f, char *file_open_name)
{
	if (f == NULL
	 || f->fcd == NULL) 
		return;
	if (f->last_operation == COB_LAST_OPEN) {
		copy_fcd_to_file (f->fcd, f);
		if (f->fcd && f->fcd->fnamePtr)
			strncpy(file_open_name, f->fcd->fnamePtr, LDCOMPX2(f->fcd->fnameLen));
	} else {
		update_fcd_to_file (f->fcd, f, NULL, -1);
	}
	return;
}

/* 
 * Sync cob_file values to FCD3 values
 */
void
cob_file_fcd_sync (cob_file *f)
{
	if (f == NULL
	 || f->fcd == NULL)  {
		return;
	}
	if (f->last_operation == COB_LAST_OPEN) {
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
	if (f == NULL) {
		cob_runtime_error (_("invalid internal call of %s"), "cob_file_fcd_adrs");
		cob_runtime_error (_("Please report this!"));
		cob_stop_run (1);
	}
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
	if (f == NULL) {
		cob_runtime_error (_("invalid internal call of %s"), "cob_file_fcdkey_adrs");
		cob_runtime_error (_("Please report this!"));
		cob_stop_run (1);
	}
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
#if! COB_64_BIT_POINTER
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
	int	opcd,sts,opts,eop,k;
	unsigned char	fnstatus[2],keywrk[80];
	char fname[COB_FILE_MAX];
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
	fs->data = fnstatus;
	fs->size = sizeof(fnstatus);
	fs->attr = &alnum_attr;
	memcpy (fnstatus, "00", 2);
	memcpy (fcd->fileStatus, "00", 2);

	if (cobglobptr == NULL) {	/* Auto Init GnuCOBOL runtime */
		cob_init (0, NULL);
		/* COB_MODULE_PTR (part of cobglobptr structure) was not set,
		   add to allow tracing and to get better messages on fileio errors */
		COB_MODULE_PTR = cob_malloc( sizeof(cob_module) );
		COB_MODULE_PTR->module_name = "GnuCOBOL-fileio";
		COB_MODULE_PTR->module_source = "GnuCOBOL-fileio";
		COB_MODULE_PTR->module_formatted_date = "2021/07/02 12:01:20";
	}

	if (*opcode == 0xFA) {
		opcd = 0xFA00 + opcode[1];
	} else {
		opcd = opcode[1];
	}

	/* Look for fcd in table and if found use associated 'cob_file' after copying values over */
	/* If fcd is not found, then 'callfh' created it, so create a new 'cob_file' and table that */
	f = find_file (fcd);

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
				memcpy (fname, fcd->fnamePtr, k);
				fname[k] = 0;
				f->flag_auto_type = 1;
				f->flag_keycheck = 0;
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
				f->organization = COB_ORG_MAX;	/* To Force file.dd to be processed */
				cob_chk_file_mapping (f, fname);
				sts = cob_read_dict (f, fname, 1);
				if (sts == -1) { /* Not present; check file */
					int ftype = indexed_file_type (f, fname);
					if (ftype >= 0) {
						f->io_routine = (unsigned char)ftype;
						f->organization = COB_ORG_INDEXED;
						return 0;
					}

				}
				copy_file_to_fcd (f, fcd);
			}
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
	 && f->record->size < f->record_min)
		f->record->size = f->record_min; 
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
		/* TODO: general mapping needed, depending
		   on a compile time switch (also for other dialects)
		   --> no "single place extension" here */
		if (f->organization == COB_ORG_INDEXED
		 && memcmp (f->file_status, "61", 2) == 0) {/* 61 --> 9A for MF */
			memcpy (fcd->fileStatus,"9A", 2);
		}
		return sts;

	case OP_OPEN_OUTPUT:
	case OP_OPEN_OUTPUT_NOREWIND:
		cob_open (f, COB_OPEN_OUTPUT, 0, fs);
		if (f->organization == COB_ORG_INDEXED
		 && f->file_status[0] == '0') {	/* 00 or 05 are both ok */
			f->open_mode = COB_OPEN_OUTPUT;
		}
		update_file_to_fcd (f, fcd, fnstatus);
		if (f->organization == COB_ORG_INDEXED
		 && memcmp (f->file_status, "61", 2) == 0) {/* 61 --> 9A for MF */
			memcpy (fcd->fileStatus,"9A", 2);
		}
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
		if (f->organization == COB_ORG_INDEXED
		 && memcmp (f->file_status, "61", 2) == 0) {/* 61 --> 9A for MF */
			memcpy (fcd->fileStatus,"9A", 2);
		}
		return sts;

	case OP_OPEN_EXTEND:
		cob_open (f, COB_OPEN_EXTEND, 0, fs);
		if (f->organization == COB_ORG_INDEXED
		 && f->file_status[0] == '0') {	/* 00 or 05 are both ok */
			f->open_mode = COB_OPEN_EXTEND;
		}
		update_file_to_fcd (f, fcd, fnstatus);
		if (f->organization == COB_ORG_INDEXED
		 && memcmp (f->file_status, "61", 2) == 0) {/* 61 --> 9A for MF */
			memcpy (fcd->fileStatus,"9A", 2);
		}
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

	case OP_GETINFO:			/* Nothing needed here */
		return sts;
	}

	rec->data = fcd->recPtr;
	rec->size = LDCOMPX4(fcd->curRecLen);
	rec->attr = &alnum_attr;

	if (fcd->recPtr == NULL) {
		fcd->fileStatus[0] = '9';
		fcd->fileStatus[1] = 141;
		cob_runtime_warning (_("ERROR: EXTFH called with no %s pointer; Function %d"),
						"record",opcd&0xFF);
		return -1;
	}

	if (f->organization == COB_ORG_INDEXED
	&& (f->open_mode == COB_OPEN_I_O
	 || f->open_mode == COB_OPEN_OUTPUT)) {
		fcd->accessFlags = ACCESS_DYNAMIC;
		f->access_mode = COB_ACCESS_DYNAMIC;
	}
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
		 && LDCOMPX4(fcd->curRecLen) >= LDCOMPX4(fcd->minRecLen)
		 && LDCOMPX4(fcd->curRecLen) <= LDCOMPX4(fcd->maxRecLen)) {
			f->record->size = LDCOMPX4(fcd->curRecLen);
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
		 && LDCOMPX4(fcd->curRecLen) >= LDCOMPX4(fcd->minRecLen)
		 && LDCOMPX4(fcd->curRecLen) <= LDCOMPX4(fcd->maxRecLen)) {
			f->record->size = LDCOMPX4(fcd->curRecLen);
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

	case OP_COMMIT:
		cob_commit();
		break;

	case OP_ROLLBACK:
		cob_rollback();
		break;

	case OP_DELETE_FILE:
		cob_delete_file (f, fs, 0);
		memcpy(fcd->fileStatus, fnstatus, 2);
		break;

	case OP_FLUSH:
		cob_file_sync (f);
		break;

	case OP_UNLOCK_REC:
		cob_unlock_file (f, fs);
		update_file_to_fcd (f, fcd, fnstatus);
		break;

	/* Similar for other possible 'opcode' values */
	default:
		/* Some sort of error message */
		break;
	}
	return sts;
}

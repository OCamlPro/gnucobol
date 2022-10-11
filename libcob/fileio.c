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


/*
 * This is the main File I/O module and it includes all routines called by
 * generated application code plus SORT, SEQUENTIAL & RELATIVE file support
 *
 * fileio.h    is a header for inclusion in all fileio modules
 * fileio.c    is this module and is the primary driver
 * fisam.c     has the C/D/V/VB-ISAM interface code for INDEXED files
 * fbdb.c      has the BDB code for INDEXED files
 * flmdb.c     has the LMDB code for INDEXED files
 * fodbc.c     has the ODBC code for INDEXED files
 * foci.c      has the OCI (Oracle) code for INDEXED files
 * fsqlxfd.c   has routines common to ODBC, OCI, BDB, LMDB
 * fextfh.c    has the EXTFH code (defacto standard used by MicroFocus, IBM, ...) 
 * focextfh.c  has code for obsolete OpenCOBOL WITH_INDEX_EXTFH/WITH_SEQRA_EXTFH 
 *
 */
#include "fileio.h"
#include "cobcapi.h"	/* for helper functions */
#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#endif

#ifdef	HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifdef	HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#ifndef STDIN_FILENO
#define STDIN_FILENO  fileno(stdin)
#endif
#ifndef STDOUT_FILENO
#define STDOUT_FILENO fileno(stdout)
#endif
#ifndef STDERR_FILENO
#define STDERR_FILENO fileno(stderr)
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

cob_global	*file_globptr = NULL;
cob_settings	*file_setptr = NULL;

static unsigned int	delete_file_status = 0;
static unsigned int	eop_status = 0;
static unsigned int	check_eop_status = 0;

static struct file_list	*file_cache = NULL;

static char		*file_open_env = NULL;
static char		*file_open_name = NULL;
static char		*file_open_buff = NULL;
static char		*file_open_io_env = NULL;	/* IO_filename env value */

static char		*runtime_buffer = NULL;

static int		chk_file_path = 1;

static const cob_field_attr	const_alpha_attr = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
static cob_file_api file_api = {NULL, NULL};

static int	qblfd = -1;
static char	qblfilename[256];
static unsigned int qblhdrsz = 0;
/* Each record in the QBL file starts with this header */
static struct cob_qbl_hdr {
	char		type[2];	/* Record type */
#define QBL_DEF		"FD"	/* File definition */	
#define QBL_OPEN	"FO"	/* File was Opened */	
#define QBL_CLOSE	"FC"	/* File was Closed */	
#define QBL_BEFORE	"B4"	/* Before image a record */
#define QBL_AFTER	"AF"	/* After image a record */
#define QBL_NEW		"NU"	/* Image of new record */
#define QBL_DELETE	"DE"	/* Image of deleted record */
	char		name[14];	/* File name */
	cob_u8_t	recnum[4];	/* Relative Record Number */
	cob_u8_t	filepos[8];	/* Offset into file where written */
	cob_u8_t	data[1];	/* alloc this record based on max record size */
} *qbl_hdr = NULL;
static void	*qbl_tmp = NULL;
#define SZ_QBLHDR (sizeof (struct cob_qbl_hdr) - 1)

static char	**file_paths = NULL;
static const char	* const prefix[] = { "DD_", "dd_", "" };
#define NUM_PREFIX	sizeof (prefix) / sizeof (char *)

static int dummy_stub		() {return 0;};
static int dummy_91			() {return COB_STATUS_91_NOT_AVAILABLE;};

static int cob_set_file_format(cob_file *, char *, int);
static void cob_set_file_defaults (cob_file *);
static int cob_savekey (cob_file *f, int idx, unsigned char *data);
static int cob_file_open	(cob_file_api *, cob_file *, char *, const int, const int);
static int cob_file_close	(cob_file_api *, cob_file *, const int);
static int cob_file_write_opt	(cob_file *, const int);

static int sequential_read	(cob_file_api *, cob_file *, const int);
static int sequential_write	(cob_file_api *, cob_file *, const int);
static int sequential_rewrite	(cob_file_api *, cob_file *, const int);
static int lineseq_read		(cob_file_api *, cob_file *, const int);
static int lineseq_write	(cob_file_api *, cob_file *, const int);
static int lineseq_rewrite	(cob_file_api *, cob_file *, const int);
static int relative_start	(cob_file_api *, cob_file *, const int, cob_field *);
static int relative_read	(cob_file_api *, cob_file *, cob_field *, const int);
static int relative_read_next	(cob_file_api *, cob_file *, const int);
static int relative_write	(cob_file_api *, cob_file *, const int);
static int relative_rewrite	(cob_file_api *, cob_file *, const int);
static int relative_delete	(cob_file_api *, cob_file *);

static struct cob_fileio_funcs sequential_funcs = {
	cob_file_open,
	cob_file_close,
	(void*)dummy_91,
	(void*)dummy_91,
	sequential_read,
	sequential_write,
	sequential_rewrite,
	(void*)dummy_91,
	(void*)dummy_stub,
	(void*)dummy_stub,
	(void*)dummy_stub,
	(void*)dummy_stub,
	(void*)dummy_stub,
	(void*)dummy_stub,
	(void*)dummy_stub,
	(void*)dummy_stub
};

static struct cob_fileio_funcs lineseq_funcs = {
	cob_file_open,
	cob_file_close,
	(void*)dummy_91,
	(void*)dummy_91,
	lineseq_read,
	lineseq_write,
	lineseq_rewrite,
	(void*)dummy_91,
	(void*)dummy_stub,
	(void*)dummy_stub,
	(void*)dummy_stub,
	(void*)dummy_stub,
	(void*)dummy_stub,
	(void*)dummy_stub,
	(void*)dummy_stub,
	(void*)dummy_stub
};

static struct cob_fileio_funcs relative_funcs = {
	cob_file_open,
	cob_file_close,
	relative_start,
	relative_read,
	relative_read_next,
	relative_write,
	relative_rewrite,
	relative_delete,
	(void*)dummy_stub,
	(void*)dummy_stub,
	(void*)dummy_stub,
	(void*)dummy_stub,
	(void*)dummy_stub,
	(void*)dummy_stub,
	(void*)dummy_stub,
	(void*)dummy_stub
};

static struct cob_fileio_funcs not_available_funcs = {
	(void*)dummy_91,
	(void*)dummy_91,
	(void*)dummy_91,
	(void*)dummy_91,
	(void*)dummy_91,
	(void*)dummy_91,
	(void*)dummy_91,
	(void*)dummy_91,
	(void*)dummy_stub,
	(void*)dummy_stub,
	(void*)dummy_stub,
	(void*)dummy_stub,
	(void*)dummy_stub,
	(void*)dummy_stub,
	(void*)dummy_stub,
	(void*)dummy_stub
};

static struct cob_fileio_funcs	*fileio_funcs[COB_IO_MAX] = {
	&sequential_funcs, &lineseq_funcs, &relative_funcs,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	&not_available_funcs
};

#if defined (__CYGWIN__)
#define LIB_PRF		"cyg"
#else
#define LIB_PRF		"lib"
#endif

#if defined(_WIN32) || defined(__CYGWIN__)
#define LIB_SUF		"-1." COB_MODULE_EXT
#else
#define LIB_SUF		"." COB_MODULE_EXT
#endif

static struct {
	int		loaded;			/* Module is loaded and ready */
	int		config;			/* Module was configured into compiler */
	int		dbase;			/* Handled by database */
	const char * name;
	const char * module;
	const char * entry;
	const char * desc;
} io_rtns [COB_IO_MAX+1] = {
	{1,1,0,"SEQUENTIAL",NULL,NULL,NULL},
	{1,1,0,"LINE",NULL,NULL,NULL},
	{1,1,0,"RELATIVE",NULL,NULL,NULL},
	{0,0,0,"CISAM",LIB_PRF "cobci" LIB_SUF, "cob_isam_init_fileio","C-ISAM"},
	{0,0,0,"DISAM",LIB_PRF "cobdi" LIB_SUF, "cob_isam_init_fileio","D-ISAM"},
	{0,0,0,"VBISAM",LIB_PRF "cobvb" LIB_SUF, "cob_isam_init_fileio","VB-ISAM"},
	{0,0,1,"BDB",LIB_PRF "cobdb" LIB_SUF, "cob_bdb_init_fileio",NULL},
	{0,0,0,"VISAM",LIB_PRF "cobvc" LIB_SUF, "cob_isam_init_fileio","V-ISAM"},
	{0,0,0,"IXEXT",NULL,NULL,NULL},
	{0,0,0,"SQEXT",NULL,NULL,NULL},
	{0,0,0,"RLEXT",NULL,NULL,NULL},
	{0,0,1,"ODBC",LIB_PRF "cobod" LIB_SUF, "cob_odbc_init_fileio",NULL},
	{0,0,1,"OCI",LIB_PRF "coboc" LIB_SUF, "cob_oci_init_fileio",NULL},
	{0,0,1,"LMDB",LIB_PRF "coblm" LIB_SUF, "cob_lmdb_init_fileio",NULL},
	{0,0,0,"NOT AVAILABLE",NULL,NULL,NULL},
	{0,0,0,NULL,NULL,NULL,NULL}
};
#ifdef	WITH_INDEX_EXTFH
void cob_index_init_fileio (cob_file_api *);
#endif

#ifdef	WITH_SEQRA_EXTFH
void cob_seqra_init_fileio (cob_file_api *);
#endif


/* Local functions */

static int
isdirvalid (char *filename)
{
#ifndef	_WIN32
	struct stat st;
#endif
	char	tmp[COB_NORMAL_BUFF];
	int		ln = strlen (filename);

	errno = 0;
	if (*filename == ':'
	 || *filename == '<'
	 || *filename == '>'
	 || *filename == '|')
		return 1;
	if (memcmp (filename, "/dev/", (size_t)5) == 0)  
		return 1;

	strcpy (tmp, filename);
	while (--ln > 0) {
#ifndef	_WIN32
	/* TODO: This code needs to be tested on Windows and adjusted as needed */
	/* For now it is effectively disabled on Windows */
		if (tmp[ln] == SLASH_CHAR) {
			tmp[ln] = 0;
			errno = 0;
			if (stat(tmp, &st) == -1) {
				errno = ENOENT;
				return 0;
			}
			if (!S_ISDIR(st.st_mode)) {
				errno = ENOTDIR;
				return 0;
			}
		}
#endif
	}
	errno = 0;
	return 1;
}


static int
isdirname (char *value)
{
#ifdef	_WIN32
	if (value[0] == '\\'
	 || value[1] == ':'
	 || value[0] == '/') 
		return 1;
#else
	if (value[0] == '/') 
		return 1;
#endif
	return 0;
}

static COB_INLINE enum cob_file_operation
get_io_ptr (cob_file *f)
{
	if (fileio_funcs[f->io_routine] == NULL) {
		cob_runtime_error (_("ERROR I/O routine %s is not present"),
							io_rtns[f->io_routine].name);
		f->io_routine = COB_IO_NOT_AVAIL;
	}
	return f->io_routine;
}

/* 
 * Set file position. pos == -1 means End-of-File
 * You must set the position for both 'fd' and 'file' 
 *  or things get out of sync
 */
static off_t
set_file_pos (cob_file *f, off_t pos)
{
	off_t  newpos = -1;
	if (io_rtns[f->io_routine].dbase)
		return 0;
	if(f->organization == COB_ORG_LINE_SEQUENTIAL) {	/* Uses 'f->file' */
		if (pos == -1) {	/* Seek to end of file */
			if (f->fd > 0) {
				newpos = lseek(f->fd, 0, SEEK_END);
			}
			if (f->file) {
				fseek((FILE*)f->file, 0, SEEK_END);
				newpos = ftell ((FILE*)f->file);
			}
		} else {
			if (f->fd > 0) {
				newpos = lseek(f->fd, pos, SEEK_SET);
			}
			if (f->file) {
				fseek((FILE*)f->file, pos, SEEK_SET);
				newpos = ftell ((FILE*)f->file);
			}
		}
	} else {			/* Uses 'f->fd' */
		if (pos == -1) {	/* Seek to end of file */
			if (f->file) {
				fseek((FILE*)f->file, 0, SEEK_END);
				newpos = ftell ((FILE*)f->file);
			}
			if (f->fd > 0) {
				newpos = lseek(f->fd, 0, SEEK_END);
			}
		} else {
			if (f->file) {
				fseek((FILE*)f->file, pos, SEEK_SET);
				newpos = ftell ((FILE*)f->file);
			}
			if (f->fd > 0) {
				newpos = lseek(f->fd, pos, SEEK_SET);
			}
		}
	}
	return newpos;
}

/* file_format: see COB_FILE_IS_xx */
static const char *file_format[12] = {"0","1","2","3","B32","B64","L32","L64","?","?","gc","mf"};
static const char *dict_ext = "dd";

#if defined(WITH_INDEXED)
static const char ix_routine = WITH_INDEXED;
#else
#if defined(WITH_CISAM)
static const char ix_routine = COB_IO_CISAM;
#elif defined(WITH_DISAM)
static const char ix_routine = COB_IO_DISAM;
#elif defined(WITH_VISAM)
static const char ix_routine = COB_IO_VISAM;
#elif defined(WITH_VBISAM)
static const char ix_routine = COB_IO_VBISAM;
#elif defined(WITH_DB)
static const char ix_routine = COB_IO_BDB;
#elif defined(WITH_LMDB)
static const char ix_routine = COB_IO_LMDB;
#elif defined(WITH_ODBC)
static const char ix_routine = COB_IO_ODBC;
#elif defined(WITH_OCI)
static const char ix_routine = COB_IO_OCI;
#elif	WITH_INDEX_EXTFH
static const char ix_routine = COB_IO_IXEXT;
#else
static const char ix_routine = COB_IO_IXEXT;
#endif
#endif
static const cob_field_attr alnum_attr = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};

/*
 * Copy ISAM file info as documented in C-ISAM manual
 */
static void
isam_to_file (FILE *fdin, cob_file *f, unsigned char *hbuf)
{
	unsigned char ibuf[4096], *p, *pp, cmprs;
	int		idxblksz, idxblk, k, part, adj;
	int		ttlen, klen, kpos, bgn, kdln, supchr;
	idxblksz = LDCOMPX2 (hbuf+6) + 1;
	f->nkeys = LDCOMPX2 (hbuf+8);
	f->record_max = LDCOMPX2 (hbuf+13);
	f->record_min = LDCOMPX2 (hbuf+13);
	if (!f->flag_vb_isam)
		idxblk = LDCOMPX4 (hbuf+15);
	else
		idxblk = LDCOMPX4 (hbuf+19);
	memset(ibuf,0,sizeof(ibuf));
	if (idxblksz > sizeof(ibuf))
		idxblksz = sizeof(ibuf);
	errno = 0;
	if (fseek (fdin, (idxblk-1)*idxblksz, SEEK_SET) == 0
	 && fread (ibuf, 1, (size_t)idxblksz, fdin) == (size_t)idxblksz) {
		f->keys = cob_cache_realloc (f->keys, sizeof (cob_file_key) * f->nkeys);
		if (!f->flag_vb_isam) {
			bgn = 7;
			supchr = 4;
			p = &ibuf[6];
		} else {
			bgn = 11;
			supchr = 5;
			p = &ibuf[10];
		}
		for(k=0; k < f->nkeys; k++) {
			kdln = LDCOMPX2 (p);
			cmprs = p[bgn-1] << 1;
			if (cmprs)
				adj = 1;
			else
				adj = 0;
			pp = &p[bgn];
			p = &p[kdln];
			f->keys[k].keyn = k;
			if ((*pp & 0x80) == 0x80)
				f->keys[k].tf_duplicates = 1;
			else
				f->keys[k].tf_duplicates = 0;
			f->keys[k].tf_suppress = 0;
			ttlen = 0;
			for (part = 0; pp < p && part < COB_MAX_KEYCOMP; part++, pp+=(adj+5)) {
				klen = LDCOMPX2 (pp) & 0x7FF;
				kpos = LDCOMPX2 (pp+2);
				if ((cmprs & 0x20) == 0x20) {
					f->keys[k].tf_suppress = 1;
					f->keys[k].char_suppress = pp[supchr];
				}
				ttlen += klen;
				if (f->record->data) {
					if (part == 0) {
						f->keys[k].field = cob_cache_malloc(sizeof(cob_field));
						f->keys[k].field->data = f->record->data + kpos;
						f->keys[k].field->attr = &alnum_attr;
						f->keys[k].field->size = klen;
						f->keys[k].offset = kpos;
					}
					f->keys[k].component[part] = cob_cache_malloc(sizeof(cob_field));
					f->keys[k].component[part]->data = f->record->data + kpos;
					f->keys[k].component[part]->attr = &alnum_attr;
					f->keys[k].component[part]->size = klen;
				}
			}
			f->keys[k].count_components = part;
		}
	}
}

/* 
 * Determine which of C|D|VB-ISAM the file is
 * by checking for certain signatures in the filename.idx
 */
int
indexed_file_type (cob_file *f, char *filename)
{
	char temp[COB_FILE_MAX];
	unsigned char hbuf[4096];
	struct stat st;
	int		idx;
	FILE	*fdin;

	if (stat(filename, &st) != -1) {
		if (S_ISDIR(st.st_mode)) {	/* Filename is a directory */
			sprintf(temp,"%s%cdata.mdb",filename,SLASH_CHAR);
			if (stat(temp, &st) != -1) {
				return COB_IO_LMDB;
			}
			return -1;
		}
	}
	sprintf(temp,"%s.idx",filename);
	fdin = fopen(temp,"r");
	if(fdin == NULL) {
		fdin = fopen(filename,"r");
		if(fdin == NULL) {
			return -1;
		}
		memset(hbuf,0,sizeof(hbuf));
		if (fread(hbuf, 1, sizeof(hbuf), fdin) < 256) {
			fclose(fdin);
			return -1;
		}
		fclose(fdin);
		for(idx=1; idx < 32; idx++) {
			sprintf(temp,"%s.%d",filename,idx);
			if (stat(temp, &st) == -1) 
				break;
		}
		/* Check for BDB signature: may be big or little endian */
		if(memcmp(&hbuf[12],"\x62\x31\x05\x00",4) == 0)
			return COB_IO_BDB;
		if(memcmp(&hbuf[12],"\x00\x05\x31\x62",4) == 0)
			return COB_IO_BDB;
		if(memcmp(&hbuf[12],"\x61\x15\x06\x00",4) == 0)
			return COB_IO_BDB;
		if(memcmp(&hbuf[12],"\x00\x06\x15\x61",4) == 0)
			return COB_IO_BDB;
		return -1;
	}
	memset(hbuf,0,sizeof(hbuf));
	if (fread(hbuf, 1, sizeof(hbuf), fdin) < 256) {
		fclose(fdin);
		return -1;
	}

	if(hbuf[0] == 0xFE 
	&& hbuf[1] == 0x53				/* C|D-ISAM marker */
	&& hbuf[2] == 0x02) {
		int idxlen;
		idxlen = (((hbuf[6] << 8) & 0xFF00) | hbuf[7]) + 1;
		f->organization = COB_ORG_INDEXED;
		if (f->flag_keycheck == 0) {	/* Update cob_file with ISAM details */
			f->flag_vb_isam = 0;
			isam_to_file (fdin, f, hbuf);
		}
		fclose(fdin);
		/* D-ISAM and C-ISAM are interchangable */
		if (idxlen <= sizeof(hbuf)
		 && (memcmp(hbuf+idxlen-4,"dism",4) == 0
		  || memcmp(hbuf+idxlen-4,"DISa",4) == 0))	/* DISa is D-ISAM 7.2 */
#if defined(WITH_DISAM)
			return COB_IO_DISAM;
#elif defined(WITH_VISAM)
			return COB_IO_VISAM;	/* Use V-ISAM if present */
#elif defined(WITH_CISAM)
			return COB_IO_CISAM;
#else
			return -1;
#endif
		else
#if defined(WITH_VISAM)
			return COB_IO_VISAM;
#elif defined(WITH_CISAM)
			return COB_IO_CISAM;
#elif defined(WITH_DISAM)
			return COB_IO_DISAM;
#else
			return -1;
#endif
	} else
	if(hbuf[0] == 'V' 
	&& hbuf[1] == 'B'			/* VB-ISAM file marker */
	&& hbuf[2] == 0x02) {
		f->organization = COB_ORG_INDEXED;
		if (f->flag_keycheck == 0) {	/* Update cob_file with ISAM details */
			f->flag_vb_isam = 1;
			isam_to_file (fdin, f, hbuf);
		}
		fclose(fdin);
#if defined(WITH_VISAM)
		return COB_IO_VISAM;	/* Use V-ISAM if present */
#else
		return COB_IO_VBISAM;
#endif
	} else
	if(hbuf[0] == 0x33
	&& hbuf[1] == 0xFE) {		/* Micro Focus format */
		fclose(fdin);
		/* Currently not supported!! */
		#define COB_IO_MFIDX4 COB_IO_NOT_AVAIL
		return COB_IO_MFIDX4;
	}
	fclose(fdin);
	return -1;
}

static int
keycmp (char *keyword, const char *val)
{
	while (*keyword && *val) {
		if (toupper(*keyword) != toupper(*val)) {
			if (!((*keyword == '-' || *keyword == '_')
			 && (*val == '-' || *val == '_')))
				break;
		}
		keyword++;
		val++;
	}
	return (toupper(*keyword)-toupper(*val));
}

/*
 * Write data file description to a string 
 */
static void
write_file_def (cob_file *f, char *out)
{
	int	idx,j;
	int	k = 0;

	out[k] = 0;
	if(f->organization == COB_ORG_INDEXED) {
		if (f->flag_vb_isam
		 || f->io_routine == COB_IO_VBISAM)
			k += sprintf(&out[k],"type=IX format=%s ","vbisam");
		else if (f->io_routine == COB_IO_VISAM)
			k += sprintf(&out[k],"type=IX format=%s ","cisam");
		else if (f->io_routine == COB_IO_DISAM)
			k += sprintf(&out[k],"type=IX format=%s ","disam");
		else
			k += sprintf(&out[k],"type=IX format=%s ",io_rtns[f->io_routine].name);
	} else if(f->organization == COB_ORG_RELATIVE) {
		k += sprintf(&out[k],"type=RL");
		if(f->file_format < 12)
			k += sprintf(&out[k],",%s",file_format[f->file_format]);
	} else if(f->organization == COB_ORG_SEQUENTIAL) {
		if ((f->flag_line_adv & COB_SET_ADVANCING))
			k += sprintf(&out[k],"type=SA");
		else
			k += sprintf(&out[k],"type=SQ");
		if(f->file_format < 12)
			k += sprintf(&out[k],",%s",file_format[f->file_format]);
	} else if(f->organization == COB_ORG_LINE_SEQUENTIAL) {
		if((f->flag_line_adv & COB_LINE_ADVANCE))
			k += sprintf(&out[k],"type=LA");
		else
			k += sprintf(&out[k],"type=LS");
		if(f->file_format == COB_FILE_IS_MF)
			k += sprintf(&out[k],",mf");
		else if(f->file_format == COB_FILE_IS_GC)
			k += sprintf(&out[k],",gc");
		if((f->file_features & COB_FILE_LS_LF))
			k += sprintf(&out[k],",lf");
		if((f->file_features & COB_FILE_LS_CRLF))
			k += sprintf(&out[k],",crlf");
		if((f->file_features & COB_FILE_LS_NULLS))
			k += sprintf(&out[k],",ls_nulls");
		if(f->flag_ls_instab)
			k += sprintf(&out[k],",ls_instab");
		if((f->file_features & COB_FILE_LS_FIXED))
			k += sprintf(&out[k],",ls_fixed");
		if((f->file_features & COB_FILE_LS_VALIDATE))
			k += sprintf(&out[k],",ls_validate");
		if((f->file_features & COB_FILE_LS_SPLIT))
			k += sprintf(&out[k],",ls_split");
	}
	if (f->flag_big_endian)
		k += sprintf(&out[k],",big-endian");
	else if (f->flag_little_endian)
		k += sprintf(&out[k],",little-endian");

	if(f->organization == COB_ORG_RELATIVE) {
		if (f->io_routine == COB_IO_ODBC)
			k += sprintf(&out[k]," format=ODBC ");
		else if (f->io_routine == COB_IO_OCI)
			k += sprintf(&out[k]," format=OCI ");
	}
	if(f->organization == COB_ORG_LINE_SEQUENTIAL) {
		k += sprintf(&out[k]," recsz=%d ",(int)(f->record_max));
	} else if(f->record_min != f->record_max) {
		k += sprintf(&out[k]," maxsz=%d ",(int)(f->record_max));
		k += sprintf(&out[k],"minsz=%d ",(int)(f->record_min));
	} else {
		k += sprintf(&out[k]," recsz=%d ",(int)(f->record_max));
	}

	if (f->xfdschema)
		k += sprintf(&out[k],"schema='%s' ",f->xfdschema);
	if (f->xfdname)
		k += sprintf(&out[k],"table='%s' ",f->xfdname);

	if (f->organization == COB_ORG_INDEXED
	 && f->nkeys > 0) {
		/* Write Key information from cob_file */
		k += sprintf(&out[k],"nkeys=%d ",(int)(f->nkeys));
		for(idx=0; idx < (int)f->nkeys; idx++) {
			k += sprintf(&out[k],"key%d=(",idx+1);
			if(f->keys[idx].count_components <= 1) {
				k += sprintf(&out[k],"%d:%d",f->keys[idx].offset,(int)(f->keys[idx].field->size));
			} else {
				for(j=0; j < f->keys[idx].count_components; j++) {
					k += sprintf(&out[k],"%d:%d",(int)(f->keys[idx].component[j]->data - f->record->data),
										(int)(f->keys[idx].component[j]->size));
					if(j+1 < f->keys[idx].count_components)
						k += sprintf(&out[k],",");
				}
			}
			k += sprintf(&out[k],") ");
			if (f->keys[idx].tf_duplicates) {
				k += sprintf(&out[k],"dup%d=Y ",idx+1);
			}
			if (f->keys[idx].len_suppress > 0
			 && f->keys[idx].str_suppress != NULL) {
				k += sprintf(&out[k],"skip%d='%.*s' ",idx+1,
								f->keys[idx].len_suppress,f->keys[idx].str_suppress);
			} else
			if (f->keys[idx].tf_suppress) {
				if (isalnum(f->keys[idx].char_suppress)
				 || f->keys[idx].char_suppress == '@'
				 || f->keys[idx].char_suppress == '#'
				 || f->keys[idx].char_suppress == '$'
				 || f->keys[idx].char_suppress == '*')
					k += sprintf(&out[k],"sup%d='%c' ",idx+1,f->keys[idx].char_suppress);
				else
					k += sprintf(&out[k],"sup%d=x'%02X' ",idx+1,f->keys[idx].char_suppress);
			}
		}
		if (f->flag_read_chk_dups)
			k += sprintf(&out[k],"dups_ahead=always ");
		else if (f->flag_read_no_02)
			k += sprintf(&out[k],"dups_ahead=never ");
	} else {
		switch (f->file_format) {
		case COB_FILE_IS_GC:
			k += sprintf(&out[k],"format=gc ");
			break;
		case COB_FILE_IS_MF:
			k += sprintf(&out[k],"format=mf ");
			break;
		case COB_FILE_IS_GCVS0:		
			k += sprintf(&out[k],"format=0 ");
			break;
		case COB_FILE_IS_GCVS1:
			k += sprintf(&out[k],"format=1 ");
			break;
		case COB_FILE_IS_GCVS2:
			k += sprintf(&out[k],"format=2 ");
			break;
		case COB_FILE_IS_GCVS3:
			k += sprintf(&out[k],"format=3 ");
			break;
		case COB_FILE_IS_B32:	
			k += sprintf(&out[k],"format=b32 ");
			break;
		case COB_FILE_IS_B64:	
			k += sprintf(&out[k],"format=b64 ");
			break;
		case COB_FILE_IS_L32:
			k += sprintf(&out[k],"format=l32 ");
			break;
		case COB_FILE_IS_L64:
			k += sprintf(&out[k],"format=l64 ");
			break;
		default:
			break;
		}
	}
}

static char *
cob_dd_prms ( char *p, char *p1, char *p2 )
{
	while (*p == ' ') p++;
	*p1 = 0;
	if (*p == '(') p++;
	if (*p == ',') p++;
	if (*p == ';') p++;
	while (*p == ' ') p++;
	while (*p != 0   && *p != ' ' 
		&& *p != ',' && *p != ':'
		&& *p != ';' && *p != ')')
		*p1++ = *p++;
	*p1 = 0;
	if (*p == ':') {
		p++;
		if (p2 != NULL) {
			*p2 = 0;
			while (*p != 0 && *p != ' ' 
				&& *p != ',' && *p != ')')
				*p2++ = *p++;
			*p2 = 0;
		}
	}
	if (*p == ')') p++;
	while (*p == ' ') p++;
	if (*p == ')') p++;
	return p;
}

static void
cob_order_keys (cob_file *f)
{
	int		didswap = 1;
	int		k;
	cob_file_key kx;
	while (didswap) {
		didswap = 0;
		for (k=0; k < (int)f->nkeys-1; k++) {
			if (f->keys[k].keyn > f->keys[k+1].keyn) {
				didswap = 1;
				memcpy(&kx, &f->keys[k], sizeof(cob_file_key));
				memcpy(&f->keys[k], &f->keys[k+1], sizeof(cob_file_key));
				memcpy(&f->keys[k+1], &kx, sizeof(cob_file_key));
			}
		}
	}
}

/*
 * Parse one key definition and update 'cob_file'
 */
static int
cob_key_def (cob_file *f, const int keyn, char *p, int keycheck)
{
	int		k, part, parts, loc, len, ttl;
	char	p1[32], p2[32];
	int		cloc[COB_MAX_KEYCOMP],clen[COB_MAX_KEYCOMP];
	if (f->flag_redo_keydef)
		keycheck = 0;
	cloc[0] = clen[0] = 0;
	ttl = 0;
	for (parts = 0; parts < COB_MAX_KEYCOMP; parts++) {
		p = cob_dd_prms (p, p1, p2);
		cloc[parts] = atoi (p1);
		clen[parts] = atoi (p2);
		ttl += clen[parts];
		if(*p != ',') {
			parts++;
			break;
		}
	}
	if (parts >= COB_MAX_KEYCOMP) {
		return COB_STATUS_39_CONFLICT_ATTRIBUTE;
	}
	loc = cloc[0];
	len = clen[0];
	for (k=0; k < (int)f->nkeys && f->keys[k].field != NULL; k++) {
		if (parts == 1) {
			if (f->keys[k].count_components > 1) 
				continue;
			if ((f->keys[k].component[0]
			 && cloc[0] == (int)(f->keys[k].component[0]->data - f->record->data)
			 && clen[0] == (int)f->keys[k].component[0]->size)
			|| (cloc[0] == (int)(f->keys[k].field->data - f->record->data)
			 && clen[0] == (int)f->keys[k].field->size)) {
				goto valid;	/* index matches */
			}
		} else if (parts == f->keys[k].count_components) {
			for (part = 0; part < parts; part++) {
				if(f->keys[k].component[part] == NULL) {
					return COB_STATUS_39_CONFLICT_ATTRIBUTE;
				}
				if ((int)f->keys[k].component[part]->size != clen[part]
				 || (int)(f->keys[k].component[part]->data - f->record->data) != cloc[part]) {
					break;
				}
			}
			if (part == parts) {		/* Found the index */
				goto valid;
			}
		}
	}
	if (keycheck
	 && (k >= (int)f->nkeys || f->keys[k].field != NULL)) {
		return COB_STATUS_39_CONFLICT_ATTRIBUTE;
	}

	if (f->flag_redo_keydef) {		/* Update index definitions */
		for (k=0; k < (int)f->nkeys && f->keys[k].field != NULL; k++);
		if (k >= (int)f->nkeys)
			return 0;
	}
	/* No match so add this index to table */
	loc = cloc[0];
	len = clen[0];
	f->keys[k].field = cob_cache_malloc (sizeof(cob_field));
	f->keys[k].field->attr = &const_alpha_attr;
	f->keys[k].offset = loc;
	if (parts == 1) {
		f->keys[k].field->size = len;
		f->keys[k].field->data = f->record->data + loc;
		if ((int)(f->keys[k].offset) != loc
		 || (int)(f->keys[k].field->size) != len) {
			f->keys[k].offset = loc;
			f->keys[k].field->size = len;
			f->keys[k].field->data = f->record->data + loc;
		}
		f->keys[k].component[0] = f->keys[k].field;
		f->keys[k].count_components = 1;

	} else {

		f->keys[k].field->size = ttl;
		f->keys[k].field->data = cob_cache_malloc ((size_t)ttl+1);
		f->keys[k].count_components = (short)parts;
		for (part = 0; part < parts; part++) {
			loc = cloc[part];
			len = clen[part];
			f->keys[k].component[part] = cob_cache_malloc (sizeof(cob_field));
			f->keys[k].component[part]->attr = &const_alpha_attr;
			f->keys[k].component[part]->size = len;
			f->keys[k].component[part]->data = f->record->data + loc;
		}
	}

valid:
	f->keys[k].keyn = (unsigned char)(keyn - 1);
	if (keyn == f->nkeys) {
		cob_order_keys (f);
	}
	return 0;
}

/* Make sure the QBL record buffer is large enough */
static void
set_qbl_buf (int recsize)
{
	recsize = (recsize + 512) / 512;
	recsize = recsize * 512;
	if (qbl_hdr == NULL) {
		qbl_hdr = cob_cache_malloc (recsize + SZ_QBLHDR);
		qbl_tmp = cob_cache_malloc (recsize + 1);
		qblhdrsz = recsize;
	} else if (qblhdrsz < recsize) {
		qbl_hdr = cob_cache_realloc (qbl_hdr, recsize + SZ_QBLHDR);
		qbl_tmp = cob_cache_realloc (qbl_tmp, recsize + 1);
		qblhdrsz = recsize;
	}
}

/*
 * Open temp file used for Commit/Rollback or audit/journal
 */
static int
cob_open_qbl (char *filename, int makeit, int append)
{
	int fd, mode;
	errno = 0;
	mode = O_RDWR;
	if (append)
		mode |= O_APPEND;
	if (makeit) 
		mode |= O_CREAT;
	fd = open (filename, mode, 0666);
	if (errno) {
		cob_runtime_warning (_("Error opening %s; %s"),
			filename, strerror(errno));
	}
	return fd;
}

static void
cob_close_qbl (int fd, char *filename, int remove)
{
	errno = 0;
	close (fd);
	if (remove && errno == 0)
		unlink (filename);
	if (errno) {
		cob_runtime_warning (_("Error closing %s; %s"),
			filename, strerror(errno));
	}
	return;
}

static off_t
writeBlock (int fd, void *data, int len)
{
	off_t numwrite, ttlwrite;
	char	*buf = (char*)data;
	numwrite = ttlwrite = 0;
	while (ttlwrite < len) {
		numwrite = write (fd, (void*)(&buf [ttlwrite]), (size_t)(len - ttlwrite));
		if (numwrite < 0)
			return -1;
		ttlwrite += numwrite;
	}
	return ttlwrite;
}

static off_t
readBlock (int fd, void *data, int len)
{
	off_t numread, ttlread;
	char	*buf = (char*)data;
	numread = ttlread = 0;
	while (ttlread < len) {
		errno = 0;
		numread = read (fd, (void*)(&buf [ttlread]), (size_t)(len - ttlread));
		if (numread <= 0
		 || errno != 0)
			return -1;
		ttlread += numread;
	}
	return ttlread;
}

/*
 * Write the QBL record with header/trailer to end of file
 * returns -1 on any error
 * Otherwise returns the position in the file where the record starts
 */
#define CHKSEED 0xE7
#define CHKMARK '@'
static off_t
cob_write_qbl (int fd, int len, void *data)
{
	unsigned char head[6], tail[6], chk;
	off_t	pos;

	if (len < 0) {
		len = 0;
	}
	len += SZ_QBLHDR;
	head[0] = tail[5] = CHKMARK;
	head[5] = tail[0] = len & 0xFF;
	head[4] = tail[1] = (len >> 8) & 0xFF;
	head[3] = tail[2] = (len >> 16) & 0xFF;
	head[2] = tail[3] = (len >> 24) & 0xFF;
	chk = CHKSEED ^ head[2];
	chk = chk ^ head[3];
	chk = chk ^ head[4];
	chk = chk ^ head[5];
	head[1] = tail[4] = chk;
	pos = lseek (fd, 0, SEEK_END);
	if (pos == -1)
		return -1;
	if (writeBlock (fd, head, 6) != 6)
		return -1;
	if (writeBlock (fd, qbl_hdr, SZ_QBLHDR) != SZ_QBLHDR)
		return -1;
	if (len > 0
	 && writeBlock (fd, data, len) != (off_t)len)
		return -1;
	if (writeBlock (fd, tail, 6) != 6)
		return -1;
	return pos;
}

/*
 * Read QBL record from file
 * 'dir'  if -1 then read backwards from 'pos'
 *        if 'pos' is -1 then start at end of the file
 * 
 * 'dir'  if >= 0 then read forwards from 'pos'
 *        if 'pos' is -1 then start at beginning of the file
 * *reclen has the record lenght stored in it
 * *date  receives the record
 *
 * off_t is returned as the position that can be used on the next call
 *       if -1 the end/beginning of the file was reached
 */
static off_t
cob_read_qbl (int fd, int *reclen, off_t pos, int dir)
{
	off_t newpos;
	off_t len;
	unsigned char mark[6], chk;
	if (fd == -1)
		return -1;
	if (dir >= 0) {
		if (pos == -1)
			pos = 0;
		errno = 0;
		newpos = lseek (fd, pos, SEEK_SET);
		if (errno == EINVAL)
			return -1;
		if (errno != 0) {
			cob_runtime_warning ("QBL: Read error %s: at %ld\n",
				strerror(errno), (long)pos);
			return -1;
		}
		if (readBlock (fd, mark, 6) == -1) {
			cob_runtime_warning ("QBL: Bad read header at %ld of %s",
				(long)newpos, qblfilename);
			return -1;
		}
		chk = CHKSEED ^ mark[2];
		chk = chk ^ mark[3];
		chk = chk ^ mark[4];
		chk = chk ^ mark[5];
		if (mark[0] != CHKMARK
		 || mark[1] != chk) {
			cob_runtime_warning ("QBL: Bad check sum %02X at %ld of %s",
				chk, (long)pos, qblfilename);
			return -1;
		}
		len = (mark[2] << 24) | (mark[3] << 16) | (mark[4] << 8) | mark[5];
		*reclen = len - SZ_QBLHDR;
		if (readBlock (fd, qbl_hdr, len) == -1) {
			cob_runtime_warning ("QBL: Bad read record at %ld of %s",
				(long)newpos, qblfilename);
			return -1;
		}
		return newpos + len + 12;
	} else {
		errno = 0;
		if (pos == -1) {
			pos = 0;
			newpos = lseek (fd, 0, SEEK_END);
			newpos = lseek (fd, newpos-6, SEEK_SET);
		} else if (pos == 0) {
			return -1;
		} else {
			newpos = lseek (fd, pos-6, SEEK_SET);
		}
		if (errno == EINVAL)
			return -1;
		if (readBlock (fd, mark, 6) == -1) {
			cob_runtime_warning ("QBL: Bad read header at %ld of %s",
				(long)newpos, qblfilename);
			return -1;
		}
		chk = CHKSEED ^ mark[3];
		chk = chk ^ mark[2];
		chk = chk ^ mark[1];
		chk = chk ^ mark[0];
		if (mark[5] != CHKMARK
		 || mark[4] != chk) {
			cob_runtime_warning ("QBL: Bad check sum %02X at %ld of %s",
				chk, (long)pos, qblfilename);
			return -1;
		}
		len = (mark[3] << 24) | (mark[2] << 16) | (mark[1] << 8) | mark[0];
		*reclen = len - SZ_QBLHDR;
		lseek (fd, newpos - len - SZ_QBLHDR, SEEK_SET);
		if (readBlock (fd, qbl_hdr, len) == -1) {
			cob_runtime_warning ("QBL: Bad read record at %ld of %s",
				(long)newpos, qblfilename);
			return -1;
		}
		return newpos - len - 6 - SZ_QBLHDR;
	}
}

static void
cob_put_qbl (cob_file *f, const char *type)
{
	off_t	pos = 0;
	cob_u64_t	recnum = 0;
	if (qblfd == -1) {
		return;
	}
	memcpy(qbl_hdr->type, type, 2);
	strncpy(qbl_hdr->name, f->select_name, sizeof(qbl_hdr->name)-1);
	if(f->organization == COB_ORG_RELATIVE) {
		recnum = (cob_u64_t)cob_get_int (f->keys[0].field);
	}
	cob_put_u64_compx (recnum, qbl_hdr->recnum, 4);
	cob_put_u64_compx (f->record_off, qbl_hdr->filepos, 8);
	if (f->organization == COB_ORG_SEQUENTIAL
	 || f->organization == COB_ORG_LINE_SEQUENTIAL) {
		if (memcmp(type, QBL_NEW, 2) == 0)
			pos = cob_write_qbl (qblfd, 0, qbl_hdr->data);
		else
			pos = cob_write_qbl (qblfd, (int)f->record->size+f->record_prefix, qbl_hdr->data);
	} else {
		pos = cob_write_qbl (qblfd, (int)f->record->size, f->record->data);
	}

	if (pos == -1) {	/* Unable to write; Must do a ROLLBACK */
		cob_runtime_error (_("I/O error writing QBL: %s"), qblfilename);
		cob_rollback ();
	}
}

static char	module_errmsg[256];
static char	module_msg[256];
/*
 * Dynamically Load the given I/O routine
 */
static int
cob_load_module (int iortn)
{
	void (*ioinit)(cob_file_api *);

	if (iortn > COB_IO_MAX) {
		/* possibly cob_runtime_error here */
		return -1;
	}
	if (io_rtns[iortn].loaded
	 || io_rtns[iortn].module == NULL) {
		return 0;
	}

	if (!io_rtns[iortn].config) {
		sprintf (module_errmsg, _("runtime not configured for feature/library"));
		return 1;
	}

	module_errmsg[0] = 0;
	ioinit = (void (*)(cob_file_api *))cob_load_lib (io_rtns[iortn].module, io_rtns[iortn].entry, module_errmsg);
	if (ioinit == NULL) {
		/* recheck with libcob */
		ioinit = (void (*)(cob_file_api *))cob_load_lib ("libcob-5", io_rtns[iortn].entry, NULL);
	}
	if (ioinit == NULL) {
		return 1;
	}

	ioinit (&file_api);
	io_rtns[iortn].loaded = 1;
	return 0;
}

static void
output_io_module_error (int iortn)
{
	const char* desc = io_rtns[iortn].desc ? io_rtns[iortn].desc : io_rtns[iortn].name;

	/* note: module_errmsg includes both the module name and error */
	cob_runtime_error (_("I/O routine %s cannot be loaded: %s"),
		desc, module_errmsg);
}

/*
 * Return a string with I/O module version information
 */
const char *
cob_io_version (const int iortn, const int verbose)
{
	if (iortn > COB_IO_MAX) return _("unsupported");
	if (verbose < 0) return io_rtns[iortn].desc ? 
						io_rtns[iortn].desc : io_rtns[iortn].name;

	cob_load_module (iortn);
	if (fileio_funcs[iortn] == NULL) {
		if (verbose) {
			output_io_module_error (iortn);
		}
		sprintf (module_msg, "%s %s (%s)", io_rtns[iortn].name,
			_("unknown"), "missing");
		return module_msg;
	}
	if (fileio_funcs[iortn]->ioversion == NULL) {
		sprintf (module_msg, "%s %s (%s)", 
			io_rtns[iortn].desc ? io_rtns[iortn].desc : io_rtns[iortn].name,
			_("unknown"), "no version");
		return module_msg;
	}
	return fileio_funcs[iortn]->ioversion ();
}

/*
 * Return a string with I/O module name
 */
const char *
cob_io_name (const int iortn)
{
	if (iortn > COB_IO_MAX) return _("unsupported");

	return io_rtns[iortn].name;
}

int
cob_write_dict (cob_file *f, char *filename)
{
	char	outdd[COB_FILE_MAX], outbuf[4096];
	FILE	*fo;

	if (file_setptr->cob_file_dict == COB_DICTIONARY_NO)
		return 0;
	if (*filename == '.'
	 || *filename == SLASH_CHAR)
		sprintf(outdd,"%s.%s",filename,dict_ext);
	else if (file_setptr->cob_dictionary_path != NULL)
		sprintf(outdd,"%s%c%s.%s",file_setptr->cob_dictionary_path,
					SLASH_CHAR,filename,dict_ext);
	else
		sprintf(outdd,"%s.%s",filename,dict_ext);
	fo = fopen(outdd,"w");
	if(fo == NULL) {
		return 1;
	}
	write_file_def (f, outbuf);
	fprintf(fo,"%s\r\n",outbuf);
	fclose(fo);
	return 0;
}

/*
 * Read description of data file from text file and check description,
 * sets file attributes for valid descriptions
 */
int				/* Return 1 on broken definition, -1 on "not found" else 0 */
cob_read_dict (cob_file *f, char *filename, int updt)
{
	char	inpdd[COB_FILE_MAX], ddbuf[2048], *sdir;
	FILE	*fi;
	int		line, ret;

	if (file_setptr->cob_file_dict == COB_DICTIONARY_NO)
		return 0;
	if (*filename == '.'
	 || *filename == SLASH_CHAR)
		sprintf(inpdd,"%s.%s",filename,dict_ext);
	else if (file_setptr->cob_dictionary_path != NULL)
		sprintf(inpdd,"%s%c%s.%s",file_setptr->cob_dictionary_path,
					SLASH_CHAR,filename,dict_ext);
	else
		sprintf(inpdd,"%s.%s",filename,dict_ext);
	fi = fopen(inpdd,"r");
	if (fi == NULL) {		/* Not present so check for OCI/ODBC */
		if ((sdir = getenv("COB_SCHEMA_DIR")) == NULL)
			sdir = (char*)COB_SCHEMA_DIR;
		sprintf(inpdd,"%s%c%s.%s",sdir,SLASH_CHAR,filename,dict_ext);
		fi = fopen(inpdd,"r");
		if (fi == NULL)
			return -1;
	}
	line = 0;
	ret = 0;
	while (fgets (ddbuf, sizeof(ddbuf)-1, fi) != NULL) {
		if (ddbuf[0] == '#')/* Skip Comment lines */
			continue;
		line++;
		ret = cob_set_file_format(f, ddbuf, updt);	/* Set defaults for file type */
		if (ret) {
			break;
		}
	}
	fclose(fi);
	return ret;
}

/* Check for file options from environment variables */
static char *
cob_chk_file_env (cob_file *f, const char *src)
{
	char		*p;
	char		*q;
	char		*s;
	const char	*t;
	unsigned int	 i;

	if (file_setptr->cob_env_mangle) {
		q = cob_strdup (src);
		s = q;
		for (i = 0; s[i] != 0; ++i) {
			if (!isalnum ((int)s[i])) {
				s[i] = '_';
			}
		}
	} else {
		q = NULL;
		s = (char *)src;
	}

	if ((file_open_io_env = cob_get_env ("IO_OPTIONS", NULL)) != NULL) {
		cob_set_file_format (f, file_open_io_env, 1);	/* Set initial defaults */
	}
	if (f->organization == COB_ORG_INDEXED) {
		t = "IX";
	} else if (f->organization == COB_ORG_SEQUENTIAL) {
		t = "SQ";
	} else if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
		if((f->flag_line_adv & COB_LINE_ADVANCE))
			t = "LA";
		else
			t = "LS";
	} else if (f->organization == COB_ORG_RELATIVE) {
		t = "RL";
	} else {
		t = "IO";
	}
	snprintf (file_open_env, (size_t)COB_FILE_MAX, "%s_OPTIONS", t);
	if ((file_open_io_env = cob_get_env (file_open_env, NULL)) == NULL) {
		snprintf (file_open_env, (size_t)COB_FILE_MAX, "%s_options", t);
		file_open_env[0] = (char)tolower(file_open_env[0]);
		file_open_env[1] = (char)tolower(file_open_env[1]);
		file_open_io_env = cob_get_env (file_open_env, NULL);
	}
	if (file_open_io_env != NULL) {
		cob_set_file_format (f, file_open_io_env, 1);	/* Defaults for file type */
	}

	/* Check for IO_filename with file specific options */
	file_open_io_env = NULL;
	snprintf (file_open_env, (size_t)COB_FILE_MAX, "%s%s", "IO_", s);
	if ((file_open_io_env = cob_get_env (file_open_env, NULL)) == NULL) {
		snprintf (file_open_env, (size_t)COB_FILE_MAX, "%s%s", "io_", s);
		if ((file_open_io_env = cob_get_env (file_open_env, NULL)) == NULL) {
			for (i = 0; file_open_env[i] != 0; ++i) {	/* Try all Upper Case */
				if(islower((unsigned char)file_open_env[i]))
					file_open_env[i] = (char)toupper((unsigned char)file_open_env[i]);
			}
			file_open_io_env = cob_get_env (file_open_env, NULL);
		}
	}
	if (file_open_io_env == NULL) {	/* Re-check for IO_fdname */
		snprintf (file_open_env, (size_t)COB_FILE_MAX, "%s%s", "IO_", f->select_name);
		if ((file_open_io_env = cob_get_env (file_open_env, NULL)) == NULL) {
			snprintf (file_open_env, (size_t)COB_FILE_MAX, "%s%s", "io_", f->select_name);
			if ((file_open_io_env = cob_get_env (file_open_env, NULL)) == NULL) {
				for (i = 0; file_open_env[i] != 0; ++i) {	/* Try all Upper Case */
					if(islower((unsigned char)file_open_env[i]))
						file_open_env[i] = (unsigned char)toupper((int)file_open_env[i]);
				}
				file_open_io_env = cob_get_env (file_open_env, NULL);
			}
		}
	}

	if (COB_MODULE_PTR
	&& !COB_MODULE_PTR->flag_filename_mapping) {	/* No DD_name checks */
		strcpy (file_open_env, file_open_name);
		if (q) {
			cob_free (q);
		}
		return NULL;
	}

	p = NULL;
	for (i = 0; i < NUM_PREFIX; ++i) {
		snprintf (file_open_env, (size_t)COB_FILE_MAX, "%s%s", prefix[i], s);
		file_open_env[COB_FILE_MAX] = 0;
		if ((p = cob_get_env (file_open_env, NULL)) != NULL) {
			break;
		}
	}
	if (p == NULL) {		/* Try all Upper case env var name */
		for (i = 0; i < NUM_PREFIX; ++i) {
			snprintf (file_open_env, (size_t)COB_FILE_MAX, "%s%s", prefix[i], s);
			file_open_env[COB_FILE_MAX] = 0;
			for (i = 0; file_open_env[i] != 0; ++i) {
				if (islower ((unsigned char)file_open_env[i])) {
					file_open_env[i] = (char)toupper((unsigned char)file_open_env[i]);
				}
			}
			if ((p = cob_get_env (file_open_env, NULL)) != NULL) {
				break;
			}
		}
		if (p == NULL) {
			strcpy (file_open_env, file_open_name);
		}
	}
	if (q) {
		cob_free (q);
	}
	return p;
}

void
cob_chk_file_mapping (cob_file *f, char *filename)
{
	char		*p;
	char		*src;
	char		*dst;
	char		*saveptr;
	char		*orig;
	unsigned int	dollar;
	int		k;

	if (filename)
		strcpy(file_open_name, filename);
	/* Misuse "dollar" here to indicate a separator */
	dollar = 0;
	for (p = file_open_name; *p; p++) {
		if (*p == '/' || *p == '\\') {
			dollar = 1;
			break;
		}
	}

	src = file_open_name;

	/* Simple case - No separators */
	if (dollar == 0) {
		/* Ignore leading dollar */
		if (*src == '$') {
			src++;
		}
		/* Check for DD_xx, dd_xx, xx environment variables */
		/* If not found, use as is including the dollar character */
		if ((p = cob_chk_file_env (f, src)) != NULL) {
			strncpy (file_open_name, p, (size_t)COB_FILE_MAX);
		} else if (file_paths) {
			for(k=0; file_paths[k] != NULL; k++) {
				snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s%c%s",
					  file_paths[k], SLASH_CHAR, file_open_name);
				file_open_buff[COB_FILE_MAX] = 0;
				if (access (file_open_buff, F_OK) == 0) {
					break;
				}
#if defined(WITH_CISAM) || defined(WITH_DISAM) || defined(WITH_VBISAM) || defined(WITH_VISAM)
				/* ISAM may append '.dat' to file name */
				snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s%c%s.dat",
					  file_paths[k], SLASH_CHAR, file_open_name);
				file_open_buff[COB_FILE_MAX] = 0;
				if (access (file_open_buff, F_OK) == 0) {
					snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s%c%s",
						  file_paths[k], SLASH_CHAR, file_open_name);
					file_open_buff[COB_FILE_MAX] = 0;
					break;
				}
#endif
			}
			if (file_paths[k] == NULL) {
				snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s%c%s",
					  file_paths[0], SLASH_CHAR, file_open_name);
				file_open_buff[COB_FILE_MAX] = 0;
			}
			strncpy (file_open_name, file_open_buff, (size_t)COB_FILE_MAX);
		}
		return;
	}

	/* Complex */
	/* Isolate first element (everything before the slash) */
	/* If it starts with a slash, it's absolute, do nothing */
	/* Else if it starts with a $, mark and skip over the $ */
	/* Try mapping on resultant string - DD_xx, dd_xx, xx */
	/* If successful, use the mapping */
	/* If not, use original element EXCEPT if we started */
	/* with a $, in which case, we ignore the element AND */
	/* the following slash */

	dollar = 0;
	dst = file_open_buff;
	*dst = 0;

	if (*src == '$') {
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
		if ((src = cob_chk_file_env (f, p)) != NULL) {
			strncpy (file_open_buff, src, (size_t)COB_FILE_MAX);
			dollar = 0;
		} else if (!dollar) {
			strncpy (file_open_buff, p, (size_t)COB_FILE_MAX);
		}
	}
	/* First element completed, loop through remaining */
	/* elements delimited by slash */
	/* Check each for $ mapping */
	for (; ;) {
		p = strtok (orig, "/\\");
		if (!p) {
			break;
		}
		if (!orig) {
			if (dollar) {
				dollar = 0;
			} else {
				strcat (file_open_buff, SLASH_STR);
			}
		} else {
			orig = NULL;
		}
		if (*p == '$' && (src = cob_chk_file_env (f, p + 1)) != NULL) {
			strncat (file_open_buff, src, (size_t)COB_FILE_MAX);
		} else {
			strncat (file_open_buff, p, (size_t)COB_FILE_MAX);
		}
	}
	strcpy (file_open_name, file_open_buff);
	cob_free (saveptr);
}

void
cob_file_sync (cob_file *f)
{
	if (f->organization == COB_ORG_INDEXED) {
		fileio_funcs[get_io_ptr (f)]->iosync (&file_api, f);
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
}

static void
cob_cache_del (cob_file *f)
{
	struct file_list	*l, *m;

	m = NULL;
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

/* Set options for LINE SEQUENTIAL file */
static void
cob_set_ls_defaults (cob_file *f)
{
	if (f->organization == COB_ORG_SEQUENTIAL) {
		f->io_routine = COB_IO_SEQUENTIAL;
		f->file_features &= ~COB_FILE_LS_CRLF;
		if ((f->flag_line_adv & COB_SET_SEQUENTIAL)) {	/* type = SQ */
			f->flag_line_adv &= ~COB_RECORD_ADVANCE;
			f->flag_line_adv &= ~COB_LINE_ADVANCE;
			if (f->file_format == COB_FILE_IS_MF)
				f->flag_line_adv |= COB_RECORD_ADVANCE;
		} else
		if ((f->flag_line_adv & COB_SET_ADVANCING)) {	/* type = SA */
			f->flag_line_adv &= ~COB_RECORD_ADVANCE;
			f->flag_line_adv &= ~COB_LINE_ADVANCE;
			if (f->record_min == f->record_max) {
				if (f->file_format == COB_FILE_IS_MF) {
					f->flag_line_adv |= COB_LINE_ADVANCE;	/* MF bugs and all */
					f->file_features |= COB_FILE_LS_CRLF;
				} else {
					f->flag_line_adv |= COB_RECORD_ADVANCE;
				}
			}
		} else {										/* Set defaults */
			if (f->file_format == COB_FILE_IS_MF) {		/* MF defaults */
				if (f->record_min == f->record_max		/* Fixed Size so CR/LF may be used */
				 && !(f->flag_line_adv & COB_RECORD_ADVANCE))
					f->file_features |= COB_FILE_LS_CRLF;
			} else {
				f->flag_line_adv &= ~COB_LINE_ADVANCE;	
				if ((f->flag_line_adv & COB_RECORD_ADVANCE))
					f->file_features &= ~COB_FILE_LS_CRLF;
			}
		}
		return;
	}

	if (f->organization != COB_ORG_LINE_SEQUENTIAL)
		return;
	f->io_routine = COB_IO_LINE_SEQUENTIAL;
	f->file_features |= COB_FILE_LS_SPLIT;
#ifdef	_WIN32
	f->file_features |= COB_FILE_LS_CRLF;
#else
	f->file_features &= ~COB_FILE_LS_CRLF;
	f->file_features |= COB_FILE_LS_LF;
#endif
	f->file_features &= ~COB_FILE_LS_FIXED;
	f->file_features &= ~COB_FILE_LS_VALIDATE;
	f->file_features &= ~COB_FILE_LS_NULLS;
	if (f->file_format == COB_FILE_IS_MF) {		/* Set MF defaults */
		f->file_features |= COB_FILE_LS_NULLS;
		if((f->flag_line_adv & COB_LINE_ADVANCE))
			f->file_features |= COB_FILE_LS_CRLF;
	} else
	if (f->file_format == COB_FILE_IS_DFLT	
	 || f->file_format == COB_FILE_IS_GC) { 	/* Set GC defaults */
		f->file_features |= COB_FILE_LS_VALIDATE;
	}
	if(file_setptr->cob_ls_fixed == 1)
		f->file_features |= COB_FILE_LS_FIXED;
	else if(file_setptr->cob_ls_fixed == 0)
		f->file_features &= ~COB_FILE_LS_FIXED;
#ifdef	_WIN32
	if(file_setptr->cob_unix_lf == 1) {
		f->file_features &= ~COB_FILE_LS_CRLF;
		f->file_features |= COB_FILE_LS_LF;
	} else {
		f->file_features |= COB_FILE_LS_CRLF;
	}
#else
	f->file_features |= COB_FILE_LS_LF;
#endif

	if(file_setptr->cob_ls_instab == 1)
		f->flag_ls_instab = 1;
	else
		f->flag_ls_instab = 0;

	if(file_setptr->cob_ls_split == 1)
		f->file_features |= COB_FILE_LS_SPLIT;
	else if(file_setptr->cob_ls_split == 0)
		f->file_features &= ~COB_FILE_LS_SPLIT;

	if(file_setptr->cob_ls_nulls == 1)
		f->file_features |= COB_FILE_LS_NULLS;
	else if(file_setptr->cob_ls_nulls == 0)
		f->file_features &= ~COB_FILE_LS_NULLS;

	if (file_setptr->cob_ls_validate == 1
	 && !(f->flag_line_adv & COB_LINE_ADVANCE))
		f->file_features |= COB_FILE_LS_VALIDATE;
	else if (file_setptr->cob_ls_validate == 0)
		f->file_features &= ~COB_FILE_LS_VALIDATE;

	if ((f->file_features & COB_FILE_LS_DEFAULT)) {
		f->file_features |= COB_FILE_LS_FIXED;
		f->file_features &= ~COB_FILE_LS_VALIDATE;
	}
}

/*
 * Set file format based on defaults, runtime.cfg 
 */
static void
cob_set_file_defaults (cob_file *f)
{
	/*
	 * Set default I/O routine
	 */
	if (f->organization == COB_ORG_INDEXED) {
		f->io_routine = ix_routine;
		if (file_setptr->cob_file_vbisam) {
			f->flag_vb_isam = 1;			/* Old VB-ISAM format wanted */
#if defined(WITH_VBISAM)
			f->io_routine = COB_IO_VBISAM;
#elif defined(WITH_VISAM)
			f->io_routine = COB_IO_VISAM;
#endif
		}
		if (file_setptr->cob_file_isnodat)
			f->flag_isnodat = 1;			/* No '.dat' extension */
		else
			f->flag_isnodat = 0;
		f->flag_read_chk_dups = 0;
		f->flag_read_no_02 = 0;
		if (file_setptr->cob_file_dups == COB_DUPS_ALWAYS) {
			f->flag_read_chk_dups = 1;
		} else if (file_setptr->cob_file_dups == COB_DUPS_NEVER) {
			f->flag_read_no_02 = 1;
		}
		if (f->fcd) {
			if (f->fcd->fileFormat == MF_FF_CISAM)
#ifdef WITH_CISAM
				f->io_routine = COB_IO_CISAM
#else
				f->io_routine = ix_routine;
#endif
#ifdef WITH_DISAM
			else if (f->fcd->fileFormat == MF_FF_DISAM)
				f->io_routine = COB_IO_DISAM;
#endif
#ifdef WITH_VBISAM
			else if (f->fcd->fileFormat == MF_FF_VBISAM)
				f->io_routine = COB_IO_VBISAM;
#endif
#ifdef WITH_VISAM
			else if (f->fcd->fileFormat == MF_FF_VISAM)
				f->io_routine = COB_IO_VISAM;
#endif
#ifdef WITH_ODBC
			else if (f->fcd->fileFormat == MF_FF_ODBC)
				f->io_routine = COB_IO_ODBC;
#endif
#ifdef WITH_OCI
			else if (f->fcd->fileFormat == MF_FF_OCI)
				f->io_routine = COB_IO_OCI;
#endif
#ifdef WITH_DB
			else if (f->fcd->fileFormat == MF_FF_BDB)
				f->io_routine = COB_IO_BDB;
#endif
#ifdef WITH_LMDB
			else if (f->fcd->fileFormat == MF_FF_LMDB)
				f->io_routine = COB_IO_LMDB;
#endif
		}
	} else if (f->organization == COB_ORG_SEQUENTIAL) {
		f->io_routine = COB_IO_SEQUENTIAL;
		f->nkeys = 0;
	} else if (f->organization == COB_ORG_RELATIVE) {
		f->io_routine = COB_IO_RELATIVE;
		f->nkeys = 0;
	} else if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
		f->io_routine = COB_IO_LINE_SEQUENTIAL;
		f->nkeys = 0;
	}
	if (f->flag_updt_file)
		return;

	f->trace_io = file_setptr->cob_trace_io ? 1 : 0;
	f->io_stats = file_setptr->cob_stats_record ? 1 : 0;
	f->flag_keycheck = file_setptr->cob_keycheck ? 1 : 0;
	f->flag_do_qbl = 0;
	if (f->organization == COB_ORG_RELATIVE
	 || f->organization == COB_ORG_INDEXED
	 || f->organization == COB_ORG_SEQUENTIAL
	 || f->organization == COB_ORG_LINE_SEQUENTIAL) {
		f->flag_do_qbl = file_setptr->cob_file_rollback ? 1 : 0;
	}
	if ((f->lock_mode & COB_LOCK_ROLLBACK))		/* Had APPLY COMMIT */
		f->flag_do_qbl = 1;
	if(file_setptr->cob_do_sync)
		f->file_features |= COB_FILE_SYNC;
	else
		f->file_features &= ~COB_FILE_SYNC;
	f->dflt_times = file_setptr->cob_retry_times;
	f->dflt_seconds = file_setptr->cob_retry_seconds;
	f->dflt_share = (unsigned char)file_setptr->cob_share_mode;
	f->dflt_retry = (unsigned short)file_setptr->cob_retry_mode;
	if (file_setptr->cob_bdb_byteorder == COB_BDB_IS_BIG)
		f->flag_big_endian = 1;
	else if (file_setptr->cob_bdb_byteorder == COB_BDB_IS_LITTLE)
		f->flag_little_endian = 1;
	if (f->dflt_retry == 0) {
		if (f->dflt_times > 0)
			f->dflt_retry |= COB_RETRY_TIMES;
		if (f->dflt_seconds > 0)
			f->dflt_retry |= COB_RETRY_SECONDS;
	}

	if (file_setptr->cob_file_format == COB_FILE_IS_MF
	 && !f->flag_set_type) {
		f->file_format = COB_FILE_IS_MF;
	}
	if (f->file_format == COB_FILE_IS_DFLT	/* File type not set by compiler; Set default */
	 || f->file_format == COB_FILE_IS_GC) {
		if (f->organization == COB_ORG_SEQUENTIAL) {
			if (f->record_min != f->record_max) {
				f->file_format = (unsigned char)file_setptr->cob_varseq_type;
			} else {
				f->file_format = COB_FILE_IS_GCVS0;
			}
		} else
		if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
			f->file_format = COB_FILE_IS_GC;
		} else
		if (f->organization == COB_ORG_RELATIVE) {
			if (f->record_min != f->record_max) {
				f->file_format = (unsigned char)file_setptr->cob_varrel_type;
			} else {
				f->file_format = (unsigned char)file_setptr->cob_fixrel_type;
			}
		}
	}

	cob_set_ls_defaults (f);
}

/*
 * Set file format based on IO_filename options
 */
static int
cob_set_file_format (cob_file *f, char *defstr, int updt)
{
	int		i,j,k,settrue,ivalue,nkeys,keyn,idx;
	unsigned int	maxrecsz = 0;
	char	qt,option[64],value[COB_FILE_BUFF];
	int ret = 0;

	if (f->record)
		maxrecsz = (unsigned int)f->record->size;
	if (f->record_max > maxrecsz)
		maxrecsz = f->record_max;
	nkeys = f->nkeys;
	if(defstr != NULL) {				/* Special options for this file */
		for(i=0; defstr[i] != 0; ) {
			while(isspace(defstr[i])	/* Skip option separators */
			|| defstr[i] == ','
			|| defstr[i] == ';') i++;
			if(defstr[i] == 0)
				break;
			ivalue = 0;
			for(j=0; j < sizeof(option)-1 && !isspace(defstr[i])
				&& defstr[i] != ','
				&& defstr[i] != ';'
				&& defstr[i] != '='
				&& defstr[i] != 0; ) {	/* Collect one option */
				option[j++] = defstr[i++];
			}
			option[j] = 0;
			value[0] = 0;
			qt = 0;
			settrue = 1;
			if(strncasecmp(option,"no-",3) == 0) {
				memmove(option,&option[3],j);
				settrue = 0;
			} else
			if(strncasecmp(option,"no_",3) == 0) {
				memmove(option,&option[3],j);
				settrue = 0;
			} else
			if(strncasecmp(option,"no",2) == 0) {
				memmove(option,&option[2],j);
				settrue = 0;
			}
			if(defstr[i] == '=') {
				i++;
				while(defstr[i] == ' ') i++;
				if(defstr[i] == '(') {
					i++;
					for(j=0; j < sizeof(value)-1 
						&& defstr[i] != ')'
						&& defstr[i] != 0; ) {	/* Collect complete option */
						value[j++] = defstr[i++];
					}
					if(defstr[i] == ')') i++;
					value[j] = 0;
				} else if(defstr[i] == '"') {
					qt = '"';
					i++;
					for(j=0; j < sizeof(value)-1 
						&& defstr[i] != '"'
						&& defstr[i] != 0; ) {	/* Collect complete option */
						value[j++] = defstr[i++];
					}
					value[j] = 0;
					if(defstr[i] == '"') i++;
				} else if(defstr[i] == '\'') {
					qt = '\'';
					i++;
					for(j=0; j < sizeof(value)-1 
						&& defstr[i] != '\''
						&& defstr[i] != 0; ) {	/* Collect complete option */
						value[j++] = defstr[i++];
					}
					value[j] = 0;
					if(defstr[i] == '\'') i++;
				} else {
					for(j=0; j < sizeof(value)-1 && !isspace(defstr[i])
						&& defstr[i] != ','
						&& defstr[i] != ';'
						&& defstr[i] != 0; ) {	/* Collect one option */
						if(isdigit(defstr[i]))
							ivalue = ivalue * 10 + defstr[i] - '0';
						value[j++] = defstr[i++];
					}
					value[j] = 0;
					if(value[0] == '1'
					|| toupper((unsigned char)value[0]) == 'T'
					|| toupper((unsigned char)value[0]) == 'Y'
					|| strcasecmp(value,"on") == 0)
						settrue = 1;
					if(value[0] == '0'
					|| toupper((unsigned char)value[0]) == 'F'
					|| toupper((unsigned char)value[0]) == 'N'
					|| strcasecmp(value,"off") == 0)
						settrue = 0;
				}
			}

			if(strcasecmp(option,"sync") == 0) {
				if(settrue)
					f->file_features |= COB_FILE_SYNC;
				else
					f->file_features &= ~COB_FILE_SYNC;
				continue;
			}
			if(strcasecmp(option,"trace") == 0) {
				f->trace_io = settrue;
				continue;
			}
			if(strcasecmp(option,"stats") == 0) {
				f->io_stats = settrue;
				continue;
			}
			if(strcasecmp(option,"keycheck") == 0) {
				f->flag_keycheck = settrue;
				continue;
			}
			if(keycmp(option,"retry_times") == 0) {
				f->dflt_times = atoi(value);
				f->dflt_retry |= COB_RETRY_TIMES;
				continue;
			}
			if(keycmp(option,"retry_seconds") == 0) {
				f->dflt_seconds = atoi(value);
				f->dflt_retry |= COB_RETRY_SECONDS;
				continue;
			}
			if(strcasecmp(option,"rollback") == 0) {
				f->flag_do_qbl = settrue;
				if (settrue)
					f->lock_mode |= COB_LOCK_ROLLBACK|COB_LOCK_MULTIPLE;
				continue;
			}
			if (strcasecmp(option,"type") == 0) {
				if (updt
				 && (((f->organization == COB_ORG_SEQUENTIAL 
				   || f->organization == COB_ORG_RELATIVE)
				  && (f->access_mode == COB_ACCESS_SEQUENTIAL))
				 || f->organization >= COB_ORG_MAX
				 || f->flag_updt_file)) {
					if(strcasecmp(value,"IX") == 0) {
						f->organization = COB_ORG_INDEXED;
						f->flag_set_isam = 1;
					} else if(strcasecmp(value,"RL") == 0) {
						f->organization = COB_ORG_RELATIVE;
						f->flag_set_isam = 0;
					} else if(strcasecmp(value,"DA") == 0) {
						f->organization = COB_ORG_RELATIVE;
						f->flag_set_isam = 0;
					} else if(strcasecmp(value,"SQ") == 0) {
						f->organization = COB_ORG_SEQUENTIAL;
						f->flag_line_adv |= COB_SET_SEQUENTIAL;
						f->flag_line_adv &= ~COB_SET_ADVANCING;
						f->flag_set_isam = 0;
					} else if(strcasecmp(value,"SA") == 0) {
						f->organization = COB_ORG_SEQUENTIAL;
						f->flag_set_isam = 0;
						f->flag_line_adv &= ~COB_SET_SEQUENTIAL;
						f->flag_line_adv |= COB_SET_ADVANCING;
					} else if(strcasecmp(value,"LS") == 0) {
						f->organization = COB_ORG_LINE_SEQUENTIAL;
						f->flag_line_adv = 0;
						f->flag_set_isam = 0;
					} else if(strcasecmp(value,"LA") == 0) {
						f->organization = COB_ORG_LINE_SEQUENTIAL;
						f->flag_line_adv = COB_LINE_ADVANCE;
						f->flag_set_isam = 0;
					}
					cob_set_file_defaults (f);
					if(strcasecmp(value,"DA") == 0) {
						f->file_format = COB_FILE_IS_MF;
						f->flag_set_type = 1;
					}
				}
				continue;
			}
			if (strcasecmp(option,"recsz") == 0) {
				if (!f->flag_updt_file) {
					if (ivalue <= 0)
						continue;
					if (maxrecsz > 0 && ivalue > (int)maxrecsz)
						continue;
					if (ivalue == f->record_max && ivalue == f->record_min)
						continue;
				}
				f->flag_redef = 1;
				f->record_min = f->record_max = ivalue;
				if (f->record == NULL) {
					f->record = cob_cache_malloc(sizeof(cob_field));
					f->record->data = cob_cache_malloc (ivalue + 1);
					f->record->attr = &alnum_attr;
				}
				f->record->size = ivalue;
				if (f->variable_record) {
					cob_set_int (f->variable_record, (int) f->record->size);
				}
				if (f->file_format == COB_FILE_IS_MF) {
					f->record_prefix = 0;
					f->file_header = 0;
				}
				continue;
			}
			if (strcasecmp(option,"maxsz") == 0) {
				if (!f->flag_updt_file) {
					if (ivalue <= 0)
						continue;
					if (maxrecsz > 0 && ivalue > (int)maxrecsz)
						continue;
					if (ivalue == f->record_max && ivalue == f->record_min)
						continue;
				}
				if(f->record_min == f->record_max)
					f->record_min = f->record_max = ivalue;
				else
					f->record_max = ivalue;
				f->flag_redef = 1;
				continue;
			}
			if (strcasecmp(option,"minsz") == 0) {
				if (!f->flag_updt_file) {
					if(ivalue <= 0 || ivalue > (int)maxrecsz)
						continue;
					if(ivalue == (int)f->record_max)
						continue;
				}
				f->record_min = ivalue;
				f->flag_redef = 1;
				continue;
			}
			if (strcasecmp(option,"limit") == 0) {	/* LIMIT rows read by database */
				f->limitreads = ivalue;
				if (f->limitreads < 0)
					f->limitreads = 0;
				continue;
			}
			if(strcasecmp(option,"format") == 0) {
				for (j=k=0; value[k] != 0; k++) {	/* remove embedded '-' or '_' */
					if (value[k] != '-'
					 && value[k] != '_')
						value[j++] = value[k];
				}
				value[j] = 0;
				for(j=0; j < COB_IO_MAX; j++) {
					if(strcasecmp(value,io_rtns[j].name) == 0) {
						if (j == COB_IO_VBISAM) {
							f->flag_vb_isam = 1;	/* Build in VB-ISAM format */
						} else {
							f->flag_vb_isam = 0;
						}
#if defined(WITH_VISAM)
#if !defined(WITH_CISAM)
						if (j == COB_IO_CISAM) {	/* C-ISAM not present but V-ISAM is */
							j = COB_IO_VISAM;
						}
#endif
#if !defined(WITH_DISAM)
						if (j == COB_IO_DISAM) {	/* D-ISAM not present but V-ISAM is */
							j = COB_IO_VISAM;
						}
#endif
#if !defined(WITH_VBISAM)
						if (j == COB_IO_VBISAM) {	/* VB-ISAM not present but V-ISAM is */
							j = COB_IO_VISAM;
						}
#endif
#elif defined(WITH_DISAM)
						if (j == COB_IO_CISAM) {	/* C-ISAM not present but D-ISAM is */
							j = COB_IO_DISAM;
						}
#endif
						if (io_rtns[j].module) {	/* all ORGANIZATION INDEXED are handled via external modules */
							f->flag_set_isam = 1;
						}
						f->io_routine = (unsigned char)j;
						break;
					}
				}
				if(j >= COB_IO_MAX) {
					if(strcasecmp(value,"auto") == 0) {
						f->flag_auto_type = 1;
					} else if (strcasecmp(value,"mf") == 0) {
						f->file_format = COB_FILE_IS_MF;
						f->flag_set_type = 1;
						cob_set_ls_defaults (f);
					} else if (strcasecmp(value,"gc") == 0
							|| strcasecmp(value,"gc3") == 0) {
						f->file_format = COB_FILE_IS_GC;
						f->flag_set_type = 1;
						cob_set_ls_defaults (f);
					} else if (strcasecmp(value,"0") == 0) {
						f->file_format = COB_FILE_IS_GCVS0;
					} else if (strcasecmp(value,"1") == 0) {
						f->file_format = COB_FILE_IS_GCVS1;
					} else if (strcasecmp(value,"2") == 0) {
						f->file_format = COB_FILE_IS_GCVS2;
					} else if (strcasecmp(value,"3") == 0) {
						f->file_format = COB_FILE_IS_GCVS3;
					} else if (strcasecmp(value,"b4") == 0
						|| strcasecmp(value,"b32") == 0) {
						f->file_format = COB_FILE_IS_B32;
					} else if (strcasecmp(value,"l4") == 0
						|| strcasecmp(value,"l32") == 0) {
						f->file_format = COB_FILE_IS_L32;
					} else if (strcasecmp(value,"b8") == 0
						|| strcasecmp(value,"b64") == 0) {
						f->file_format = COB_FILE_IS_B64;
					} else if (strcasecmp(value,"l8") == 0
						|| strcasecmp(value,"l64") == 0) {
						f->file_format = COB_FILE_IS_L64;
					} else {
						cob_runtime_warning (_("I/O routine %s is not known for %s"),
											value,file_open_env);
					}
				}
				continue;
			}
			if(strcasecmp(option,"dups_ahead") == 0) {
				f->flag_read_chk_dups = 0;
				f->flag_read_no_02 = 0;
				if(strcasecmp(value,"always") == 0) {
					f->flag_read_chk_dups = 1;
				} else if(strcasecmp(value,"never") == 0) {
					f->flag_read_no_02 = 1;
				} else if(strcasecmp(value,"default") != 0) {
					cob_runtime_warning (_("dups_ahead option %s is not valid for %s"),
											value,file_open_env);
				}
				continue;
			}
			if(strcasecmp(option,"schema") == 0) {
				if (isdirname(value)) {
					f->xfdschema = cob_strdup (value);
				} else {
					f->xfdschema = cob_cache_malloc (strlen(value) + strlen(COB_SCHEMA_DIR) + 8);
					sprintf((char*)f->xfdschema, "%s%c%s",COB_SCHEMA_DIR,SLASH_CHAR,value);
				}
				continue;
			}
			if(strcasecmp(option,"table") == 0) {
				f->xfdname = cob_strdup (value);
				continue;
			}
			if(keycmp(option,"big_endian") == 0) {
				f->flag_big_endian = 1;
				f->flag_little_endian = 0;
				continue;
			}
			if(keycmp(option,"little_endian") == 0) {
				f->flag_big_endian = 0;
				f->flag_little_endian = 1;
				continue;
			}
			if(keycmp(option,"isnodat") == 0) {
				f->flag_isnodat = settrue;
				continue;
			}
			if(keycmp(option,"retry_forever") == 0) {
				f->dflt_retry = COB_RETRY_FOREVER;
				continue;
			}
			if(keycmp(option,"retry_never") == 0) {
				f->dflt_retry = COB_RETRY_NEVER;
				continue;
			}
			if(keycmp(option,"ignore_lock") == 0) {
				f->dflt_retry |= COB_IGNORE_LOCK;
				continue;
			}
			if(keycmp(option,"advancing_lock") == 0) {
				f->dflt_retry |= COB_ADVANCING_LOCK;
				continue;
			}
			if(keycmp(option,"share_all") == 0) {
				f->dflt_share = COB_SHARE_ALL_OTHER;
				f->share_mode = f->dflt_share;
				continue;
			}
			if(keycmp(option,"share_read") == 0) {
				f->dflt_share = COB_SHARE_READ_ONLY;
				f->share_mode = f->dflt_share;
				continue;
			}
			if(keycmp(option,"share_no") == 0) {
				f->dflt_share = COB_SHARE_NO_OTHER;
				f->share_mode = f->dflt_share;
				continue;
			}
			if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
				if(keycmp(option,"ls_nulls") == 0) {
					if(settrue)
						f->file_features |= COB_FILE_LS_NULLS;
					else
						f->file_features &= ~COB_FILE_LS_NULLS;
					continue;
				}
				if(keycmp(option,"ls_fixed") == 0) {
					if(settrue)
						f->file_features |= COB_FILE_LS_FIXED;
					else
						f->file_features &= ~COB_FILE_LS_FIXED;
					continue;
				}
				if(keycmp(option,"ls_split") == 0) {
					if(settrue)
						f->file_features |= COB_FILE_LS_SPLIT;
					else
						f->file_features &= ~COB_FILE_LS_SPLIT;
					continue;
				}
				if(keycmp(option,"ls_validate") == 0) {
					if(settrue)
						f->file_features |= COB_FILE_LS_VALIDATE;
					else
						f->file_features &= ~COB_FILE_LS_VALIDATE;
					continue;
				}
				if(keycmp(option,"ls_instab") == 0) {
					if(settrue)
						f->flag_ls_instab = 1;
					else
						f->flag_ls_instab = 0;
					continue;
				}
				if(strcasecmp(option,"crlf") == 0) {
					if(settrue)
						f->file_features |= COB_FILE_LS_CRLF;
					else
						f->file_features &= ~COB_FILE_LS_CRLF;
					continue;
				}
				if(strcasecmp(option,"lf") == 0) {
					if(settrue) {
						f->file_features &= ~COB_FILE_LS_CRLF;
						f->file_features |= COB_FILE_LS_LF;
					} else {
						f->file_features &= ~COB_FILE_LS_LF;
					}
					continue;
				}
				if(strcasecmp(option,"mf") == 0) {	/* LS file like MF would do */
					f->flag_set_type = 1;
					f->file_format = COB_FILE_IS_MF;
					cob_set_ls_defaults (f);
					continue;
				}
				if (strcasecmp(option,"gc") == 0
				 || strcasecmp(option,"gc3") == 0) {	/* LS file like GnuCOBOL used to do */
					f->flag_set_type = 1;
					f->file_format = COB_FILE_IS_GC;
					cob_set_ls_defaults (f);
					continue;
				}
			}
			if(strcasecmp(option,"mf") == 0) {
				if(settrue) {
					f->file_format = COB_FILE_IS_MF;
					f->flag_set_type = 1;
					cob_set_ls_defaults (f);
					continue;
				}
				continue;
			}
			if (strcasecmp(option,"gc") == 0
			 || strcasecmp(option,"gc3") == 0) {
				if(settrue) {
					f->file_format = COB_FILE_IS_GC;
#ifdef WITH_VARSEQ 
					if (f->organization == COB_ORG_SEQUENTIAL
					 && f->record_min != f->record_max)	/* Variable length Sequential */
						f->file_format = WITH_VARSEQ;
#endif
					f->flag_set_type = 1;
					cob_set_ls_defaults (f);
					continue;
				}
				continue;
			}
			if (f->organization == COB_ORG_SEQUENTIAL
			 && f->record_min != f->record_max) {		/* Variable length Sequential */
#ifdef WITH_VARSEQ 
				f->file_format = WITH_VARSEQ;
#endif
				if(strcasecmp(option,"0") == 0) {
					f->file_format = COB_FILE_IS_GCVS0;
				} else
				if(strcasecmp(option,"1") == 0) {
					f->file_format = COB_FILE_IS_GCVS1;
				} else
				if(strcasecmp(option,"2") == 0) {
					f->file_format = COB_FILE_IS_GCVS2;
				} else
				if(strcasecmp(option,"3") == 0) {
					f->file_format = COB_FILE_IS_GCVS3;
				} else
				if(strcasecmp(option,"b4") == 0
				|| strcasecmp(option,"b32") == 0) {
					f->file_format = COB_FILE_IS_B32;
				} else
				if(strcasecmp(option,"l4") == 0
				|| strcasecmp(option,"l32") == 0) {
					f->file_format = COB_FILE_IS_L32;
				} else
				if(strcasecmp(option,"b8") == 0
				|| strcasecmp(option,"b64") == 0) {
					f->file_format = COB_FILE_IS_B64;
				} else
				if(strcasecmp(option,"l8") == 0
				|| strcasecmp(option,"l64") == 0) {
					f->file_format = COB_FILE_IS_L64;
				}
				f->flag_set_type = 1;
			}
			if (f->organization == COB_ORG_RELATIVE) {	/* Relative format */
				if(strcasecmp(option,"b4") == 0
				|| strcasecmp(option,"b32") == 0) {
					f->file_format = COB_FILE_IS_B32;
				} else
				if(strcasecmp(option,"l4") == 0
				|| strcasecmp(option,"l32") == 0) {
					f->file_format = COB_FILE_IS_L32;
				} else
				if(strcasecmp(option,"b8") == 0
				|| strcasecmp(option,"b64") == 0) {
					f->file_format = COB_FILE_IS_B64;
				} else
				if(strcasecmp(option,"l8") == 0
				|| strcasecmp(option,"l64") == 0) {
					f->file_format = COB_FILE_IS_L64;
				}
				f->flag_set_type = 1;
			}
			if (f->organization == COB_ORG_INDEXED) {
				if (strcasecmp(option,"nkeys") == 0) {
					nkeys = ivalue;
					if (f->nkeys != nkeys
					 && !updt
					 && f->flag_keycheck) {
						ret = COB_STATUS_39_CONFLICT_ATTRIBUTE;
						break;
					}
					if (updt
					 && f->nkeys != nkeys) {
						if(nkeys > f->nkeys) {
							f->keys = cob_cache_realloc (f->keys, sizeof (cob_file_key) * nkeys);
							f->flag_redo_keydef = 1;
						}
						f->nkeys = nkeys;
					} else {
						nkeys = f->nkeys;
					}
					continue;
				}
				if (strncasecmp(option,"key",3) == 0) {
					keyn = atoi (&option[3]);
					if (keyn > f->nkeys
					 && !f->flag_keycheck
					 && !f->flag_updt_file
					 && !updt) {			/* Skip this key def */
						continue;
					}
					if (keyn > nkeys) {
						if (!f->flag_updt_file)
							continue;
						f->nkeys = nkeys = keyn;
						f->keys = cob_cache_realloc (f->keys, sizeof (cob_file_key) * nkeys);
						f->flag_redo_keydef = 1;
					}
					ret = cob_key_def (f, keyn, value, updt?0:f->flag_keycheck);
					if (ret != 0) break;
					
				} else if (strcasecmp(option,"duplen") == 0) {
					f->isam_duplen = ivalue;
				} else if (strcasecmp(option,"indexsz") == 0) {
					if (ivalue > (16*1024))
						ivalue = (16*1024);
					if (ivalue < 0)
						ivalue = 0;
					f->isam_idxsz = (ivalue + 511) / 512;
				} else if (strncasecmp(option,"dup",3) == 0) {
					keyn = atoi (&option[3]);
					idx = keyn - 1;
					if (keyn > nkeys) {
						if (!f->flag_updt_file)
							continue;
						f->nkeys = nkeys = keyn;
						f->keys = cob_cache_realloc (f->keys, sizeof (cob_file_key) * nkeys);
						f->flag_redo_keydef = 1;
					}

					if (toupper(value[0]) == 'Y'
					 && f->flag_keycheck
					 && !updt
					 && !f->keys[idx].tf_duplicates)
						break;
					if (toupper(value[0]) == 'Y')
						f->keys[idx].tf_duplicates = 1;
					else
						f->keys[idx].tf_duplicates = 0;
				} else if (strncasecmp(option,"sup",3) == 0) {
					unsigned char subchr;
					keyn = atoi (&option[3]);
					if (keyn > nkeys) {
						if (!f->flag_updt_file)
							continue;
						f->nkeys = nkeys = keyn;
						f->keys = cob_cache_realloc (f->keys, sizeof (cob_file_key) * nkeys);
						f->flag_redo_keydef = 1;
					}
					idx = keyn - 1;
					if (qt != 0) {
						subchr = value[0];
					} else if (value[0] == 'x') {
						subchr = (unsigned char) strtol (&value[2], NULL, 16);
					} else {
						subchr = 0;
					}
					if (f->flag_keycheck
					 && !updt
					 && !f->keys[idx].tf_suppress)
						break;
					f->keys[idx].char_suppress = subchr;
					f->keys[idx].tf_suppress = 1;
				} else if (strncasecmp(option,"skip",4) == 0) {
					keyn = atoi (&option[4]);
					if (keyn > nkeys) {
						if (!f->flag_updt_file)
							continue;
						f->nkeys = nkeys = keyn;
						f->keys = cob_cache_realloc (f->keys, sizeof (cob_file_key) * nkeys);
						f->flag_redo_keydef = 1;
					}
					idx = keyn - 1;
					if (f->flag_keycheck
					 && !updt
					 && !f->keys[idx].tf_suppress)
						break;
					f->keys[idx].str_suppress = (unsigned char *)cob_strdup (value);
					f->keys[idx].len_suppress = (short)strlen (value);
				} else if (strncasecmp(option,"drop",4) == 0) {
					keyn = atoi (&option[4]);
					if(keyn > f->nkeys)
						continue;
					if (keyn == f->nkeys) {
						f->nkeys--;
						nkeys = f->nkeys;
						continue;
					}
					while (keyn < f->nkeys) {
						memcpy (&f->keys[keyn-1], &f->keys[keyn], sizeof (f->keys[keyn]));
						keyn++;
					}
					f->nkeys--;
					nkeys = f->nkeys;
					continue;
				}
			}
		}

		/* If SHARE or RETRY given, then override application choices */
		if(f->dflt_retry != 0) {
			f->retry_mode = f->dflt_retry;
			f->retry_times = f->dflt_times;
			f->retry_seconds = f->dflt_seconds;
		}
	}

	f->record_off = -1;
	f->flag_begin_of_file = 1;
	f->record_prefix = 0;
	f->file_header = 0;
	/* Set File type specific values */
	if (f->organization == COB_ORG_SEQUENTIAL) {
		f->record_slot = f->record_max + f->record_prefix;
		if(f->record_min != f->record_max) {
			if(f->file_format == COB_FILE_IS_GC
			|| f->file_format == COB_FILE_IS_GCVS0
			|| f->file_format == COB_FILE_IS_GCVS1
			|| f->file_format == COB_FILE_IS_GCVS2) {
				f->record_prefix = 4;
				f->record_slot = f->record_max + f->record_prefix;
			} else 
			if(f->file_format == COB_FILE_IS_GCVS3) {
				f->record_prefix = 2;
				f->record_slot = f->record_max + f->record_prefix;
			} else
			if(f->file_format == COB_FILE_IS_L32
			|| f->file_format == COB_FILE_IS_B32) {
				f->record_prefix = 4;
				f->record_slot = f->record_max + f->record_prefix;
			} else
			if(f->file_format == COB_FILE_IS_MF) {
				f->record_prefix = 4;
				f->file_header = 128;
				f->record_slot = f->record_max + f->record_prefix + 1;
			} else {
				f->record_prefix = 4;
				f->record_slot = f->record_max + f->record_prefix;
			} 
		}
	} else
	if (f->organization == COB_ORG_RELATIVE) {
		f->record_prefix = sizeof(size_t);
		f->record_slot = f->record_max + f->record_prefix;
		if(f->file_format == COB_FILE_IS_B32
		|| f->file_format == COB_FILE_IS_L32) {
			f->record_prefix = 4;
			f->record_slot = f->record_max + f->record_prefix;
		} else 
		if(f->file_format == COB_FILE_IS_B64
		|| f->file_format == COB_FILE_IS_L64) {
			f->record_prefix = 8;
			f->record_slot = f->record_max + f->record_prefix;
		} else 
		if(f->file_format == COB_FILE_IS_MF) {
			if(f->record_min != f->record_max) {	/* Variable length Relative */
				if(f->record_max < 4096) {
					f->record_prefix = 2;
				} else {
					f->record_prefix = 4;
				}
				f->file_header = 128;
				f->record_slot = f->record_max + f->record_prefix + 1;
			} else {
				f->record_prefix = 0;
				f->record_slot = f->record_max + 1;
			}
		} 
	}
	return ret;
}

#ifdef	HAVE_FCNTL
#if defined(HAVE_SIGACTION) && defined(SIGALRM)
static void catch_alarm(int sig) { }
#endif

/*
 * Issue File/Record lock
 */
static int
lock_record(
	cob_file *f, 
	unsigned int recnum, 
	int 	forwrite, 
	int 	*errsts)
{
#if defined(HAVE_SIGACTION) && defined(SIGALRM)
	struct sigaction sigact, prvact;
	int		wait_time;
#endif
	int		lock_type, retry, interval;
	unsigned long	pos;
	unsigned int	rcsz;
	struct flock	lck;

	lock_type = forwrite ? F_WRLCK : F_RDLCK;
	f->blockpid = 0;
	retry = interval = 0;
	if (f->retry_mode == 0) {
		/* Nothing else to do */
	} else 
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
	if(recnum == 0) {			/* Lock entire file */
		pos = 0;
		rcsz = 0;
		f->flag_file_lock = 0;
	} else {
		rcsz = (unsigned int)f->record_max;
		if(rcsz <= 0)
			rcsz = 2;
		if(f->record_slot <= 0)
			f->record_slot = rcsz + 1;
		pos = (unsigned long)(f->file_header+((recnum-1)*f->record_slot));
	}
	memset(&lck,0,sizeof(struct flock));
	lck.l_type = lock_type;
	lck.l_whence = SEEK_SET;
	lck.l_start = pos;
	lck.l_len = rcsz;
	if (fcntl (f->fd, F_SETLK, &lck) != -1) {
		*errsts = 0;
		if(recnum == 0
		&& forwrite) 			/* File locked for Exclusive use */
			f->flag_file_lock = 1;	
		return 1;			/* Got the lock so all is good */
	}
	*errsts = errno;
	if(retry == 0)	{			/* No RETRY, so return with no lock */
		if(errno == EAGAIN) {
			lck.l_type = lock_type;
			lck.l_whence = SEEK_SET;
			lck.l_start = pos;
			lck.l_len = rcsz;
			if (fcntl (f->fd, F_GETLK, &lck) == -1) {
				if(lck.l_pid == cob_sys_getpid()) {	/* Is locked by me */
					return 1;
				}
			}
		}
		f->blockpid = lck.l_pid;
		return 0;
	}
	if(interval <= 0)
		interval = COB_RETRY_PER_SECOND ;

#if defined(HAVE_SIGACTION) && defined(SIGALRM)
	if(retry > 0) {				/* Negative means wait forever */
		memset(&prvact,0,sizeof(sigact));
		prvact.sa_handler = SIG_DFL;
		memset(&sigact,0,sizeof(sigact));
		sigact.sa_handler = catch_alarm;
		sigaction(SIGALRM, &sigact, &prvact);
		wait_time = retry * interval;
		alarm(wait_time);
	}
	if (fcntl (f->fd, F_SETLKW, &lck) != -1) {
		if(retry > 0) {
			sigaction(SIGALRM, &prvact, NULL);
			alarm(0);
		}
		*errsts = 0;
		if(recnum == 0
		&& forwrite) 			/* File locked for Exclusive use */
			f->flag_file_lock = 1;	
		return 1;			/* Got the lock so all is good */
	}
	*errsts = errno;
	if(retry > 0) {
		sigaction(SIGALRM, &prvact, NULL);
		alarm(0);
		if(*errsts == EINTR)		/* Timed out, so return EAGAIN */
			*errsts = EAGAIN;
	}
#else
	if (retry > 0) {
		retry = retry * 4;
		interval = (interval * 1000) / 4;	
		while(retry-- > 0) {
			lck.l_type = lock_type;
			lck.l_whence = SEEK_SET;
			lck.l_start = pos;
			lck.l_len = rcsz;
			*errsts = 0;
			if (fcntl (f->fd, F_SETLK, &lck) != -1) {
				if(recnum == 0
				&& forwrite) 	/* File locked for Exclusive use */
					f->flag_file_lock = 1;	
				return 1;	/* Got the lock so all is good */
			}
			*errsts = errno;
			cob_sleep_msec(interval);
		}
	} else {
		while(1) {
			lck.l_type = lock_type;
			lck.l_whence = SEEK_SET;
			lck.l_start = pos;
			lck.l_len = rcsz;
			*errsts = 0;
			if (fcntl (f->fd, F_SETLK, &lck) != -1) {
				if(recnum == 0
				&& forwrite) 	/* File locked for Exclusive use */
					f->flag_file_lock = 1;	
				return 1;	/* Got the lock so all is good */
			}
			*errsts = errno;
			cob_sleep_msec(250);
		}
	}
#endif
	f->blockpid = lck.l_pid;
	return 0;				/* Record is not locked! */
}

/*
 * Un-Lock 'recnum' with system
 */
static int
unlock_record(cob_file *f, unsigned int recnum)
{
	unsigned long pos;
	unsigned int rcsz;
	struct flock lck;

	if(recnum == 0) {			/* Un-Lock entire file */
		pos = 0;
		rcsz = 0;
		f->flag_file_lock = 0;
	} else {
		rcsz = (unsigned int)f->record_max;
		if(rcsz <= 0)
			rcsz = 2;
		if(f->record_slot <= 0)
			f->record_slot = rcsz + 1;
		pos = (unsigned long)(f->file_header+((recnum-1)*f->record_slot));
	}
	lck.l_type = F_UNLCK;
	lck.l_whence = SEEK_SET;
	lck.l_start = pos;
	lck.l_len = rcsz;
	errno = 0;
	if (fcntl (f->fd, F_SETLK, &lck) != -1) {
		return 1;					/* Released the lock so all is good */
	}
	f->blockpid = 0;
	return 0;						/* Record is not locked! */
}

#else
	/* System does not even have 'fcntl' so no explicit Record/File lock is used */
	/* TODO: check later for possible fall-back [at least WIN32]*/
static int
lock_record(
	cob_file *f, 
	unsigned int recnum, 
	int 	forwrite, 
	int 	*errsts)
{
	COB_UNUSED (f);
	COB_UNUSED (recnum);
	COB_UNUSED (forwrite);
	*errsts = 0;
	return 1;
}

static int
unlock_record(cob_file *f, unsigned int recnum) 
{ 
	COB_UNUSED (f);
	COB_UNUSED (recnum);
	return 1;
}

#endif

/*
 * Determine if file should be locked 
 */
static int
set_file_lock(cob_file *f, const char *filename, int open_mode) 
{
	int	lock_mode, ret;

	f->flag_record_lock = 0;
	f->flag_file_lock = 0;
	if (memcmp (filename, "/dev/", (size_t)5) == 0) 	/* Do not lock Devices */
		return 0;

	if ((f->share_mode & COB_SHARE_ALL_OTHER)
	 && ((open_mode == COB_OPEN_INPUT) || (open_mode == COB_OPEN_I_O))) {/* File is SHARE ALL */
		f->flag_record_lock = 1;
		return 0;
	} 

	/* Lock the file */
	if ((f->share_mode & COB_SHARE_ALL_OTHER)) {
		if (open_mode == COB_OPEN_OUTPUT)
			lock_mode = 1;
		else
			lock_mode = 0;
	} else if ((open_mode != COB_OPEN_INPUT)
		|| (f->share_mode & COB_SHARE_NO_OTHER)
		|| (f->share_mode & COB_LOCK_OPEN_EXCLUSIVE)
		|| (f->lock_mode & COB_FILE_EXCLUSIVE) ) {
		lock_mode = 1;
	} else {
		lock_mode = 0;
	}

	lock_record (f, 0, lock_mode, &ret);
	if (ret != 0) {
		if(f->file)
			fclose(f->file);
		else
			close (f->fd);
		f->fd = -1;
		f->file = NULL;
		f->open_mode = COB_OPEN_CLOSED;
		switch (ret) {
		case EACCES:
		case EAGAIN:
			return COB_STATUS_61_FILE_SHARING;
		case EDEADLK:
			return COB_STATUS_52_DEAD_LOCK;
		default:
			return COB_STATUS_30_PERMANENT_ERROR;
		}
	}
	if (!f->flag_file_lock) {
		if ((open_mode != COB_OPEN_INPUT)
		 && (f->share_mode & COB_SHARE_ALL_OTHER)) {
			f->flag_record_lock = 1;
		}
	}
	return 0;
}

/*
 * Determine if current record should be locked and if previous lock to be released
 */
static void
set_lock_opts(cob_file *f, unsigned int read_opts) 
{
	f->flag_lock_mode = 0;		/* READ lock */
	if (f->retry_mode == 0 
	 && f->dflt_retry != 0) {	/* Use IO_filename RETRY values */
		f->retry_mode = f->dflt_retry;
		f->retry_times = f->dflt_times;
		f->retry_seconds = f->dflt_seconds;
	}
	if (f->flag_file_lock) {	/* File is EXCLUSIVE */
		f->flag_lock_rec = 0;
		f->flag_lock_rls = 0;
		return;
	}
	if (!f->lock_mode) {
		if (f->open_mode != COB_OPEN_INPUT) {
			f->flag_lock_rec = 0;
			f->flag_lock_rls = 0;
		} else {
			f->flag_lock_rec = 1;
			f->flag_lock_rls = 1;
		}
	} else if (f->flag_file_lock) {
		f->flag_lock_rec = 0;
		f->flag_lock_rls = 0;
	} else if ((f->lock_mode & COB_LOCK_AUTOMATIC) 
		&& (f->open_mode != COB_OPEN_INPUT)) {
		f->flag_lock_rec = 1;
		if ((f->lock_mode & COB_LOCK_MULTIPLE)) {
			f->flag_lock_rls = 0;
		} else {
			f->flag_lock_rls = 1;
		}
	} else {
		f->flag_lock_rec = 1;
		if ((f->lock_mode & COB_LOCK_MULTIPLE)) {
			f->flag_lock_rls = 0;
		} else {
			f->flag_lock_rls = 1;
		}
	}

	if ((read_opts & COB_READ_IGNORE_LOCK)
	 || (f->retry_mode & COB_IGNORE_LOCK)) {
		f->flag_lock_rec = 0;
		f->flag_lock_rls = 0;
		f->flag_lock_mode = 0;
	} else
	if ( (read_opts & COB_READ_LOCK) ) {
		f->flag_lock_rec = 1;
		f->flag_lock_mode = 1;
	} else
	if ( (read_opts & COB_READ_WAIT_LOCK) ) {
		f->flag_lock_rec = 1;
		f->flag_lock_mode = 1;
	} else
	if ( (read_opts & COB_READ_NO_LOCK) ) {
		f->flag_lock_rec = 0;
		f->flag_lock_rls = 0;
	} else
	if ( (read_opts & COB_READ_KEPT_LOCK) ) {
		f->flag_lock_rec = 1;
		f->flag_lock_rls = 0;
	}

	if (f->flag_lock_rls && f->prev_lock) {
		unlock_record (f, f->prev_lock);
		f->prev_lock = 0;
	}
}

static COB_INLINE void
cob_trace_record (cob_file *f, int indent)
{
	fprintf (file_setptr->cob_trace_file, "%*s : ",
		indent, "Record");
	cob_print_field (file_setptr->cob_trace_file, f->record,
		indent + 3, file_setptr->cob_dump_width);
}

static COB_INLINE void
cob_trace_record_key (cob_file *f, int indent)
{
	if (!f->last_key) {
		return;
	}
	fprintf (file_setptr->cob_trace_file, "%*s : ", indent,
		f->organization == COB_ORG_RELATIVE ? "Record#" : "Key");
	cob_print_field (file_setptr->cob_trace_file, f->last_key,
		indent + 3, file_setptr->cob_dump_width);
}

static COB_INLINE void
cob_trace_record_number (cob_file *f, int indent)
{
	if (!f->last_key
	 || f->organization != COB_ORG_RELATIVE) {
		return;
	}
	fprintf (file_setptr->cob_trace_file, "%*s : ",
		indent, "Record#");
	cob_print_field (file_setptr->cob_trace_file, f->last_key,
		indent + 3, file_setptr->cob_dump_width);
}

void
cob_file_save_status (cob_file *f, cob_field *fnstatus, const int status)
{
	int	k, indent = 15;

	file_globptr->cob_error_file = f;
	if (status == 0) {
		memset (f->file_status, '0', (size_t)2);
		/* EOP is non-fatal therefore 00 status but needs exception */
		if (eop_status) {
			eop_status = 0;
			cob_set_exception (COB_EC_I_O_EOP);
		} else {
			file_globptr->cob_exception_code = 0;
		}
		if ((f->file_features & COB_FILE_SYNC)) {
			cob_file_sync (f);
		}
	} else if (status > COB_STATUS_BASE) {
		if (delete_file_status) {
			delete_file_status = 0;
			cob_set_exception (COB_EC_DELETE_FILE);
		}
		memset (f->file_status, '9', (size_t)2);
		switch (status) {
		case COB_XSTATUS_IS_DIR:
			memcpy (f->file_status, "37", 2);	/* Default to old GnuCOBOL status */
			cob_set_exception (status_exception[3]);
			if (COB_MODULE_PTR) {
				if (COB_MODULE_PTR->flag_dialect == COB_DIALECT_MF) {
					f->file_status[0] = '9';
					f->file_status[1] = 21;
				}
			}
			break;
		case COB_XSTATUS_NOT_DIR:
			memcpy (f->file_status, "30", 2);	/* Default to old GnuCOBOL status */
			cob_set_exception (status_exception[3]);
			if (COB_MODULE_PTR) {
				if (COB_MODULE_PTR->flag_dialect == COB_DIALECT_MF) {
					f->file_status[0] = '9';
					f->file_status[1] = 9;
				}
			}
			break;
		case COB_XSTATUS_NOT_FILE:
			memcpy (f->file_status, "35", 2);	/* Default to old GnuCOBOL status */
			cob_set_exception (status_exception[3]);
			if (COB_MODULE_PTR) {
				if (COB_MODULE_PTR->flag_dialect == COB_DIALECT_MF) {
					f->file_status[0] = '9';
					f->file_status[1] = 9;
				}
			}
			break;
		}
	} else {
		if (delete_file_status) {
			delete_file_status = 0;
			cob_set_exception (COB_EC_DELETE_FILE);
		} else {
			cob_set_exception (status_exception[status / 10]);
		}
		f->file_status[0] = (unsigned char)COB_I2D (status / 10);
		f->file_status[1] = (unsigned char)COB_I2D (status % 10);
	}
	if (fnstatus) {
		memcpy (fnstatus->data, f->file_status, (size_t)2);
	}

	if (f->trace_io
	 && f->last_operation != COB_LAST_NONE) {
		/* If necessary open, so that I/O can be traced by itself */
		cob_check_trace_file ();
		fprintf (file_setptr->cob_trace_file, "%*s", indent-3, "");
		switch (f->last_operation) {
			default:
				fprintf (file_setptr->cob_trace_file,
					"Unknown I/O on %s Status: %.2s\n",
					f->select_name, f->file_status);
				break;
			case COB_LAST_CLOSE:
				fprintf (file_setptr->cob_trace_file,
					"CLOSE %s Status: %.2s\n",
					f->select_name, f->file_status);
				break;
			case COB_LAST_COMMIT:
				fprintf (file_setptr->cob_trace_file,
					"COMMIT %s Status: %.2s\n",
					f->select_name, f->file_status);
				break;
			case COB_LAST_ROLLBACK:
				fprintf (file_setptr->cob_trace_file,
					"ROLLBACK %s Status: %.2s\n",
					f->select_name, f->file_status);
				break;
			case COB_LAST_OPEN:
				fprintf (file_setptr->cob_trace_file,
					"OPEN %s %s -> '%s' Status: %.2s\n",
					f->open_mode == COB_OPEN_INPUT  ? "INPUT" :
					f->open_mode == COB_OPEN_OUTPUT ? "OUTPUT" :
					f->open_mode == COB_OPEN_I_O    ? "I_O" :
					f->open_mode == COB_OPEN_EXTEND ? "EXTEND" : "", 
					f->select_name,
					file_open_name ? file_open_name : "",
					f->file_status);
				break;
			case COB_LAST_DELETE_FILE:
				fprintf (file_setptr->cob_trace_file,
					"DELETE FILE %s Status: %.2s\n",
					f->select_name, f->file_status);
				break;
			case COB_LAST_READ:
				fprintf (file_setptr->cob_trace_file,
					"READ %s Status: %.2s\n",
					f->select_name, f->file_status);
				if (status == 0) {
					cob_trace_record (f, indent);
				}
				cob_trace_record_key (f, indent);
				break;
			case COB_LAST_START:
				fprintf (file_setptr->cob_trace_file,
					"START %s Status: %.2s\n",
					f->select_name, f->file_status);
				cob_trace_record_key (f, indent);
				break;
			case COB_LAST_READ_SEQ:
				fprintf (file_setptr->cob_trace_file,
					"READ Sequential %s Status: %.2s\n",
					f->select_name, f->file_status);
				if (status == 0) {
					cob_trace_record (f, indent);
				}
				cob_trace_record_number (f, indent);
				break;
			case COB_LAST_WRITE:
				fprintf (file_setptr->cob_trace_file,
					"WRITE %s Status: %.2s\n",
					f->select_name, f->file_status);
				cob_trace_record (f, indent);
				cob_trace_record_number (f, indent);
				break;
			case COB_LAST_REWRITE:
				fprintf (file_setptr->cob_trace_file,
					"REWRITE %s Status: %.2s\n",
					f->select_name, f->file_status);
				cob_trace_record (f, indent);
				cob_trace_record_number (f, indent);
				break;
			case COB_LAST_DELETE:
				fprintf (file_setptr->cob_trace_file,
					"DELETE %s Status: %.2s\n",
					f->select_name, f->file_status);
# if 1	/* CHECKME: We likely should print only the key here */
				cob_trace_record (f, indent);
				cob_trace_record_number (f, indent);
#else
				cob_trace_record_key (f, indent);
#endif
				break;
		 }
	}

	if (f->io_stats
	 && file_setptr->cob_stats_filename
	 && f->last_operation != COB_LAST_NONE) {
		/* Gather stats for record related commands only */
		if (f->last_operation < COB_LAST_OPEN) {
			int stat = (int)f->last_operation - 1;
			f->stats[stat].rqst_io++;
			if (status != 0
			 && status != 2) {
				f->stats[stat].fail_io++;
			}
		}
		/* Write stats out on FILE Close */
		else if (f->last_operation == COB_LAST_CLOSE) {
			FILE	*fo = NULL;
			struct stat	st;
			if (stat (file_setptr->cob_stats_filename, &st) == -1) {
				fo = fopen (file_setptr->cob_stats_filename, "w");
				if (fo) {
					const char	*iotype[11];
					iotype[COB_LAST_READ]	 = "READ";
					iotype[COB_LAST_WRITE]	 = "WRITE";
					iotype[COB_LAST_REWRITE] = "REWRITE";
					iotype[COB_LAST_DELETE]	 = "DELETE";
					iotype[COB_LAST_START]	 = "START";
					iotype[COB_LAST_READ_SEQ]= "READ_SEQ";
					fprintf (fo, "%19s%s," ,"Time", " Source, FDSelect");
					for (k = 1; k < (int)COB_LAST_OPEN; k++) {
						fprintf (fo, ", %s", iotype[k]);
					}
					for (k = 1; k < (int)COB_LAST_OPEN; k++) {
						fprintf (fo, ", X%s", iotype[k]);
					}
					fprintf (fo, "\n");
					fclose (fo);
					fo = NULL;
				}
			}
			fo = fopen (file_setptr->cob_stats_filename, "a");
			if (fo) {
				struct cob_time tod = cob_get_current_date_and_time ();
				fprintf (fo, "%04d/%02d/%02d %02d:%02d:%02d,",
						tod.year,tod.month,tod.day_of_month,
						tod.hour,tod.minute,tod.second);
				/* CHECKME: Shouldn't this be always set with GC4+? */
				fprintf (fo, "%s,%s",
					COB_MODULE_PTR && COB_MODULE_PTR->module_source ? 
					COB_MODULE_PTR->module_source : "unknown",
					f->select_name);
				for (k = 0; k < (int)COB_LAST_OPEN - 1; k++) {
					fprintf (fo, ", %d", f->stats[k].rqst_io);
				}
				for (k = 0; k < (int)COB_LAST_OPEN - 1; k++) {
					fprintf (fo, ", %d", f->stats[k].fail_io);
				}
				fprintf (fo,"\n");
				fclose (fo);
			}
			for (k = 0; k < (int)COB_LAST_OPEN - 1; k++) {		/* Reset counts on CLOSE */
				f->stats[k].rqst_io = 0;
				f->stats[k].fail_io = 0;
			}
		}
	}
	if (f->fcd) {
		cob_file_fcd_sync (f);			/* Copy cob_file to app's FCD */
	}
	f->last_operation = COB_LAST_NONE;		/* Avoid double count/trace */
	f->retry_mode = f->dflt_retry;
	f->retry_times = f->dflt_times;
	f->retry_seconds = f->dflt_seconds;
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
		return COB_STATUS_37_PERMISSION_DENIED;
	case EISDIR:
		return COB_XSTATUS_IS_DIR;
	case ENOTDIR:
		return COB_XSTATUS_NOT_DIR;
	case ENOENT:
		return COB_STATUS_35_NOT_EXISTS;
	default:
		return default_status;
	}
}

#define COB_CHECKED_WRITE(fd,string,length)	do { \
		if (write (fd, string, (size_t)length) != (size_t)length) { \
			return errno_cob_sts (COB_STATUS_30_PERMANENT_ERROR); \
		} \
	} ONCE_COB /* LCOV_EXCL_LINE */

#define COB_CHECKED_FPUTC(character,fstream)	do { \
		if (fputc ((int)character, fstream) != (int)character) { \
			return errno_cob_sts (COB_STATUS_30_PERMANENT_ERROR); \
		} \
	} ONCE_COB /* LCOV_EXCL_LINE */

#define COB_CHECKED_FWRITE(fstream,string,length)	do { \
		if (fwrite (string, 1, (size_t)length, fstream) != (size_t)length) { \
			return errno_cob_sts (COB_STATUS_30_PERMANENT_ERROR); \
		} \
	} ONCE_COB /* LCOV_EXCL_LINE */

static size_t
file_linage_check (cob_file *f)
{
	cob_linage	*lingptr;

	lingptr = f->linage;
	lingptr->lin_lines = cob_get_int (lingptr->linage);
	if (lingptr->lin_lines < 1) {
		goto linerr;
	}
	if (lingptr->latfoot) {
		lingptr->lin_foot = cob_get_int (lingptr->latfoot);
		if (lingptr->lin_foot < 1
		 || lingptr->lin_foot > lingptr->lin_lines) {
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
	FILE			*fp;
	int			i;
	int			n;

	fp = (FILE *)f->file;
	lingptr = f->linage;
	if (opt & COB_WRITE_PAGE) {
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
				eop_status = 1;
			}
		}
		if (i > lingptr->lin_lines) {
			/* Set EOP status if requested */
			if (check_eop_status) {
				eop_status = 1;
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

/*
 * For variable length SEQUENTIAL write the record prefix
 */
static unsigned int
cob_seq_write_rcsz (cob_file *f, const int rcsz)
{
	union {
		unsigned char	sbuff[4];
		unsigned short	sshort[2];
		unsigned int	sint;
	} recsize;

	if (f->record_min != f->record_max) { 
		recsize.sint = 0;
		switch (f->file_format) {
		case COB_FILE_IS_GC:
		case COB_FILE_IS_GCVS0:
			recsize.sshort[0] = COB_MAYSWAP_16 (rcsz);
			break;
		case COB_FILE_IS_GCVS1:
			recsize.sint = COB_MAYSWAP_32 (rcsz);
			break;
		case COB_FILE_IS_GCVS2:
			recsize.sint = rcsz;
			break;
		case COB_FILE_IS_GCVS3:
			recsize.sshort[0] = COB_MAYSWAP_16 (rcsz);
			break;
		case COB_FILE_IS_B32:		/* Was varseq 2 on Big Endian system */
			STCOMPX4(rcsz, recsize.sbuff);
			break;
		case COB_FILE_IS_L32:		/* Was varseq 2 on Little Endian system */
			STBINLE4(rcsz, recsize.sbuff);
			break;
		case COB_FILE_IS_MF:
			if(f->record_prefix == 2) {
				STCOMPX2(rcsz, recsize.sbuff);
			} else {
				STCOMPX4(rcsz, recsize.sbuff);
			}
			recsize.sbuff[0] |= 0x40;
			break;
		default:
			recsize.sshort[0] = COB_MAYSWAP_16 (rcsz);
			break;
		}

		if (write (f->fd, recsize.sbuff, f->record_prefix) !=
			     (int)f->record_prefix) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
	}
	return 0;
}

static unsigned int
cob_seq_write_opt (cob_file *f, const int opt)
{
	int	i;
	char	*tmp;

	if ((f->flag_line_adv & COB_RECORD_ADVANCE)) {
		f->file_features &= ~COB_FILE_LS_CRLF;
		f->file_features &= ~COB_FILE_LS_LF;
		f->flag_needs_cr = 0;
		f->flag_needs_nl = 0;
		i = opt & COB_WRITE_MASK;
		if ((opt & COB_WRITE_LINES) 
		 && i > 0) {
			cob_seq_write_rcsz (f, f->record_max);
			tmp = cob_malloc (f->record_max);
			while(i-- > 0) {
				COB_CHECKED_WRITE (f->fd, tmp, f->record_max);
			}
			cob_free (tmp);
		}
		return 0;
	}
	if (opt & COB_WRITE_LINES) {
		i = opt & COB_WRITE_MASK;
		if (!i) {
			/* AFTER/BEFORE 0 */
			if (f->flag_needs_cr)
				COB_CHECKED_FPUTC ('\r', (FILE *)f->file);
			f->flag_needs_cr = 0;
		} else {
			if ((f->file_features & COB_FILE_LS_CRLF)
			 && f->file_format == COB_FILE_IS_MF) {
				if (f->flag_needs_cr)
					COB_CHECKED_FPUTC ('\r', (FILE *)f->file);
				f->record_off++;
				f->flag_needs_cr = 0;
			}   
			for (i = opt & COB_WRITE_MASK; i > 0; --i) {
				COB_CHECKED_FPUTC ('\n', (FILE *)f->file);
			}
		}
		cob_file_sync (f);
	} else if (opt & COB_WRITE_PAGE) {
		if ((f->file_features & COB_FILE_LS_CRLF)) {
			if (f->flag_needs_cr)
				COB_CHECKED_FPUTC ('\r', (FILE *)f->file);
			f->flag_needs_cr = 0;
		}
		COB_CHECKED_FPUTC ('\f', (FILE *)f->file);
		cob_file_sync (f);
	}
	return 0;
}

static int
cob_file_write_opt (cob_file *f, const int opt)
{
	int	i;

	if (f->flag_is_pipe) 
		return COB_STATUS_00_SUCCESS;

	if (f->flag_select_features & COB_SELECT_LINAGE) {
		return cob_linage_write_opt (f, opt);
	}
	if (opt & COB_WRITE_LINES) {
		i = opt & COB_WRITE_MASK;
		if (!i) {
			/* AFTER/BEFORE 0 */
			if (f->flag_needs_cr)
				COB_CHECKED_FPUTC ('\r', (FILE *)f->file);
			f->flag_needs_cr = 0;
		} else {
			for (; i > 0; --i) {
				if (f->flag_needs_cr) {
					COB_CHECKED_FPUTC ('\r', (FILE *)f->file);
					f->flag_needs_cr = 0;
				}
				COB_CHECKED_FPUTC ('\n', (FILE *)f->file);
			}
		}
	} else if (opt & COB_WRITE_PAGE) {
		if ((f->file_features & COB_FILE_LS_CRLF)) {
			if (f->flag_needs_cr) {
				COB_CHECKED_FPUTC ('\r', (FILE *)f->file);
				f->flag_needs_cr = 0;
			}
		}
		COB_CHECKED_FPUTC ('\f', (FILE *)f->file);
	}
	return 0;
}

/* 
 * Check if input file is Micro Focus variable length format
 *         (Refer to Micro Focus file format documentation for details)
 */
static unsigned char mfhdrmark2[4] = {0x30,0x7E,0x00,0x00};
static unsigned char mfhdrmark4[4] = {0x30,0x00,0x00,0x7C};
static int
check_mf_format(cob_file *f, char *filename)
{
	FILE	*fd;
	int	ln,minrcsz,maxrcsz;
	unsigned char mfhdr[128];

	fd = fopen(filename,"r");
	if(fd == NULL) {
		return 0;
	}

	memset(mfhdr,0,sizeof(mfhdr));
	ln = fread(mfhdr,1,sizeof(mfhdr),fd);
	minrcsz = LDCOMPX4(((unsigned char *)&mfhdr[58]));
	maxrcsz = LDCOMPX4(((unsigned char *)&mfhdr[54]));

	/* Check for file header markers and sanity checks on record size info */
	if(ln == sizeof(mfhdr)
	&& (memcmp(mfhdr,mfhdrmark2,4) == 0 || memcmp(mfhdr,mfhdrmark4,4) == 0)
	&& mfhdr[36] == 0x00
	&& mfhdr[37] == 0x3E
	&& (mfhdr[39] == 0x01 || mfhdr[39] == 0x03)
	&& (mfhdr[48] == 0x01 || mfhdr[48] == 0x00)
	&& minrcsz > 0
	&& maxrcsz < (60*1024*1024)
	&& minrcsz <= maxrcsz) {
		if(memcmp(mfhdr,mfhdrmark4,4) == 0) {
			f->record_prefix  = 4;
		} else {
			f->record_prefix  = 2;
		}
		if(maxrcsz > (int)f->record_max) {
			cob_runtime_error (_("ERROR FILE %s has record size %d exceeds %d in program"),
							f->select_name,maxrcsz,(int)f->record_max);
		} else {
			f->record_min  = minrcsz;
			f->record_max  = maxrcsz;
		}
		f->file_header = 128;
		f->record_off  = -1;		/* At start of file */
		f->file_format = COB_FILE_IS_MF;
		if(mfhdr[39] == 0x03) {		/* Relative format */
			f->record_slot = f->record_max + 1 + f->record_prefix;
		} else {
			f->record_slot = 0;	/* Unused for sequential format */
		}
	} else {
		fclose(fd);
		return 0;
	}
	fclose(fd);
	return 1;
}

/*
 * Write the MF style file header for variable sequential & relative files
 */
static int			/* Return -1 on error, else 0 */
write_mf_header(cob_file *f, char *filename)
{
	FILE	*fd;
	int	k;
	char	wrk[16];
	unsigned char mfhdr[128];
	time_t	nowis;
	struct tm *lclNow;

	fd = fopen(filename,"w");
	if(fd == NULL) {
		return -1;
	}
	memset(mfhdr,0,sizeof(mfhdr));
	if(f->record_max < 4096) {
		memcpy(mfhdr,mfhdrmark2,4);
		f->record_prefix = 2;
	} else {
		memcpy(mfhdr,mfhdrmark4,4);
		f->record_prefix = 4;
	}
	time(&nowis);
	lclNow = localtime(&nowis);
	strftime(wrk,sizeof(wrk),"%y%m%d%H%M%S00",lclNow);
	memcpy(&mfhdr[8],wrk,14);
	memcpy(&mfhdr[22],wrk,14);
	mfhdr[37] = 0x3E;
	if(f->organization == COB_ORG_RELATIVE)
		mfhdr[39] = 3;
	else
	if(f->organization == COB_ORG_SEQUENTIAL)
		mfhdr[39] = 1;
	if(f->record_min != f->record_max)
		mfhdr[48] = 1;
	else
		mfhdr[48] = 0;
	STCOMPX4(f->record_max,LSUCHAR(&mfhdr[54]));
	STCOMPX4(f->record_min,LSUCHAR(&mfhdr[58]));
	k = fwrite(mfhdr, sizeof(mfhdr), 1, fd);
	fclose(fd);
	if(k != 1)
		return -1;
	f->file_header = 128;
	f->record_off  = -1;		/* At start of file */
	return 0;
}

/*
 * Open (record) Sequential and Relative files
 *  with just an 'fd' (No FILE *)
 */
static int
cob_fd_file_open (cob_file *f, char *filename, const int mode, const int sharing)
{
	int		fd;
	int		fdmode;
	int		fperms;
	unsigned int	nonexistent;
	int		ret;
	COB_UNUSED(sharing);

	/* Note filename points to file_open_name */
	/* cob_chk_file_mapping manipulates file_open_name directly */
	f->blockpid = 0;

	if (!f->flag_file_map) {
		cob_chk_file_mapping (f, NULL);
		f->flag_file_map = 1;
		ret = cob_set_file_format(f, file_open_io_env, 1);		/* Set file format */
		if (ret) {
			return ret;
		}
	}

	nonexistent = 0;
	if (mode != COB_OPEN_EXTEND 
	 && mode != COB_OPEN_OUTPUT
	 && !f->flag_optional
	 && !isdirvalid (filename)) {
		return COB_XSTATUS_NOT_DIR;
	}
	errno = 0;
	if (access (filename, F_OK) && errno == ENOENT) {
		if (mode != COB_OPEN_OUTPUT && f->flag_optional == 0) {
			return COB_STATUS_35_NOT_EXISTS;
		}
		nonexistent = 1;
	}

	if ((f->organization == COB_ORG_RELATIVE || f->organization == COB_ORG_SEQUENTIAL)
	 && nonexistent == 0
	 && !f->flag_set_type
	 && (mode == COB_OPEN_INPUT || mode == COB_OPEN_I_O || mode == COB_OPEN_EXTEND) ) {
		if (f->file_format == COB_FILE_IS_MF
		 && f->record_min == f->record_max) {
			/* Fixed size records so No file header to check */
		} else
		if (f->file_format == COB_FILE_IS_MF
		 && !check_mf_format(f, filename)) {
			f->file_format = COB_FILE_IS_GCVS0;	/* Try GNU Cobol format */
			f->record_prefix = 4;
			f->file_header = 0;
			(void)cob_set_file_format(f, file_open_io_env, 1);	/* Reset file format options */
		} else
		if (f->file_format != COB_FILE_IS_MF
		 && check_mf_format(f, filename)) {
			f->file_format = COB_FILE_IS_MF;	/* Use Micro Focus format */
		}
	}

	fdmode = O_BINARY;
	fperms = 0;
	f->fd = -1;
	f->flag_file_lock = 0;
	switch (mode) {
	case COB_OPEN_INPUT:
		if ((f->share_mode & COB_SHARE_NO_OTHER)
		 || (f->lock_mode & COB_FILE_EXCLUSIVE) ) {
			/* fcntl with WRLCK requires file to be opened RDWR */
			fdmode |= O_RDWR;
		} else {
			fdmode |= O_RDONLY;
		}
		break;
	case COB_OPEN_OUTPUT:
		nonexistent = 0;
		fdmode |= O_CREAT | O_TRUNC;
		if (f->organization == COB_ORG_RELATIVE) {
			fdmode |= O_RDWR;
		} else {
			fdmode |= O_WRONLY;
		}
		fperms = COB_FILE_MODE;
		break;
	case COB_OPEN_I_O:
		if (nonexistent) {
			fdmode |= O_CREAT | O_RDWR;
			fperms = COB_FILE_MODE;
		} else {
			fdmode |= O_RDWR;
		}
		break;
	case COB_OPEN_EXTEND:
		fdmode |= O_CREAT | O_RDWR | O_APPEND;
		fperms = COB_FILE_MODE;
		break;
	}

	errno = 0;
	fd = open (filename, fdmode, fperms);
	ret = errno;

	switch (ret) {
	case 0:
		f->open_mode = (unsigned char)mode;
		break;
	case ENOENT:
		if (mode == COB_OPEN_EXTEND || mode == COB_OPEN_OUTPUT) {
	 		if (!isdirvalid (filename))
				return COB_XSTATUS_NOT_DIR;
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		if (f->flag_optional) {
			f->open_mode = (unsigned char)mode;
			f->flag_nonexistent = 1;
			f->flag_end_of_file = 1;
			f->flag_begin_of_file = 1;
			return COB_STATUS_05_SUCCESS_OPTIONAL;
		}
		return COB_STATUS_35_NOT_EXISTS;
	case EACCES:
	case EROFS:
		return COB_STATUS_37_PERMISSION_DENIED;
	case EISDIR:
		return COB_XSTATUS_IS_DIR;
	case ENOTDIR:
		return COB_XSTATUS_NOT_DIR;
	case EAGAIN:
		return COB_STATUS_61_FILE_SHARING;
	default:
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	f->fd = fd;
	if (mode == COB_OPEN_INPUT) {
       f->file = (void*)fdopen(f->fd, "r");
	} else if (mode == COB_OPEN_I_O) {
       if (nonexistent)
           f->file = (void*)fdopen(f->fd, "w+");
       else
           f->file = (void*)fdopen(f->fd, "r+");
	} else if (mode == COB_OPEN_EXTEND) {
           f->file = (void*)fdopen(f->fd, "a");
	} else {
       f->file = (void*)fdopen(f->fd, "w");
	}
	if (errno != 0) { 	/* should never happen here... */
		close (fd);
		f->file = NULL;
		f->fd = -1;
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	if (mode == COB_OPEN_EXTEND) {
		set_file_pos (f, -1);
	}
	if ((mode == COB_OPEN_OUTPUT || (mode == COB_OPEN_I_O && nonexistent))
	&&  f->file_format == COB_FILE_IS_MF) {	/* Write MF file header */
		if(f->record_min != f->record_max) {
			write_mf_header(f, filename);
			f->record_off = set_file_pos (f, (off_t)f->file_header);
		} else {
			f->record_prefix = 0;
		}
	} else
	if ((f->organization == COB_ORG_RELATIVE || f->organization == COB_ORG_SEQUENTIAL)
	&&  f->file_format == COB_FILE_IS_MF
	&&  (mode == COB_OPEN_INPUT || mode == COB_OPEN_I_O) ) {
		f->record_off = set_file_pos (f, (off_t)f->file_header);
	} else if (mode == COB_OPEN_EXTEND) {
		f->record_off = set_file_pos (f, -1);
	} else {
		f->record_off = -1;
	}

	if (f->organization == COB_ORG_RELATIVE
	 && f->access_mode == COB_ACCESS_SEQUENTIAL) {
		struct stat	st;
		if (f->keys[0].field) {
			cob_set_int (f->keys[0].field, 0);
		}
		f->cur_rec_num = 0;
		if (f->open_mode == COB_OPEN_OUTPUT) {
			f->max_rec_num = 0;
		} else if (fstat (f->fd, &st) == 0) {
			f->max_rec_num = (st.st_size - f->file_header) / f->record_slot;
		}
		if (f->max_rec_num < 0)
			f->max_rec_num = 0;
	}

	if ((ret=set_file_lock(f, filename, mode)) != 0)
		return ret;
	if (f->flag_optional && nonexistent) {
		return COB_STATUS_05_SUCCESS_OPTIONAL;
	}
	return 0;
}

#define dMaxArgs 16
static int
cob_file_open (cob_file_api *a, cob_file *f, char *filename, const int mode, const int sharing)
{
	/* Note filename points to file_open_name */
	/* cob_chk_file_mapping manipulates file_open_name directly */

	int		ret;
	struct stat st;
	FILE			*fp;
	const char		*fmode;
	cob_linage		*lingptr;
	unsigned int	nonexistent;

	f->share_mode = (unsigned char)sharing;
	if(!f->flag_file_map) {
		cob_chk_file_mapping (f, NULL);
		f->flag_file_map = 1;
	}
	f->flag_is_pipe = 0;
	f->flag_is_std = 0;

	if (file_setptr->cob_file_dict == COB_DICTIONARY_ALL
	 && mode == COB_OPEN_OUTPUT)
		a->cob_write_dict(f, filename); 

	if (f->organization != COB_ORG_LINE_SEQUENTIAL) {
		return cob_fd_file_open (f, filename, mode, sharing);
	}

	nonexistent = 0;
	errno = 0;
	f->file_pid = 0;
	f->flag_is_pipe = 0;
	if (filename[0] == '>') {
		if (mode != COB_OPEN_OUTPUT) 
			return COB_STATUS_37_PERMISSION_DENIED;
		if (!file_setptr->cob_unix_lf) {
			fmode = "w";
		} else {
			fmode = "wb";
		}
		fp = popen (filename + 1, fmode);
		if (fp == NULL)
			return COB_STATUS_30_PERMANENT_ERROR;
		f->file = fp;
		if (fp) {
			f->fd = fileno (fp);
		} else {
			f->fd = -1;
		}
		f->fileout = f->file;
		f->fdout = f->fd;
		f->flag_is_pipe = 1;
		f->open_mode = (unsigned char)mode;
		return 0;
	}
	if (filename[0] == '<') {
		if (mode != COB_OPEN_INPUT) 
			return COB_STATUS_37_PERMISSION_DENIED;
		if (!file_setptr->cob_unix_lf) {
			fmode = "r";
		} else {
			fmode = "rb";
		}
		fp = popen (filename + 1, fmode);
		if (fp == NULL)
			return COB_STATUS_30_PERMANENT_ERROR;
		f->file = fp;
		f->fileout = NULL;
		f->fdout = -1;
		if (fp) {
			f->fd = fileno (fp);
		} else {
			f->fd = -1;
		}
		f->flag_is_pipe = 1;
		f->open_mode = (unsigned char)mode;
		return 0;
	}
	if (strcmp (filename,":CI:") == 0) {
		if (mode != COB_OPEN_INPUT) 
			return COB_STATUS_37_PERMISSION_DENIED;
		f->flag_select_features |= COB_SELECT_STDIN;
		f->flag_select_features &= ~COB_SELECT_STDOUT;
		f->file = stdin;
		f->fileout = NULL;
		f->fdout = -1;
		f->fd = STDIN_FILENO;
		f->open_mode = (unsigned char)mode;
		f->flag_is_std = 1;
		return 0;
	}
	if (strcmp (filename,":CO:") == 0) {
		if (mode != COB_OPEN_OUTPUT) 
			return COB_STATUS_37_PERMISSION_DENIED;
		f->flag_select_features &= ~COB_SELECT_STDIN;
		f->flag_select_features |= COB_SELECT_STDOUT;
		f->file = stdout;
		f->fileout = stdout;
		f->fdout = STDOUT_FILENO;
		f->fd = STDOUT_FILENO;
		f->open_mode = (unsigned char)mode;
		f->flag_is_std = 1;
		return 0;
	}
	if (strcmp (filename,":CE:") == 0) {
		if (mode != COB_OPEN_OUTPUT) 
			return COB_STATUS_37_PERMISSION_DENIED;
		f->flag_select_features &= ~COB_SELECT_STDIN;
		f->flag_select_features |= COB_SELECT_STDOUT;
		f->file = stderr;
		f->fileout = stderr;
		f->fdout = STDERR_FILENO;
		f->fd = STDERR_FILENO;
		f->open_mode = (unsigned char)mode;
		f->flag_is_std = 1;
		return 0;
	}

	if (filename[0] == '|') {
#if defined (HAVE_UNISTD_H) && !(defined (_WIN32))
		pid_t	s_pid;
		int		j, k;
		int		p_fds[2], c_fds[2];
		char* args[dMaxArgs];
		if (mode != COB_OPEN_I_O) 
			return COB_STATUS_37_PERMISSION_DENIED;
		filename++;
		while(*filename == ' ')
			filename++;
		args[0] = filename;
		for (j=k=0; filename[k] != 0; k++) {
			if (filename[k] == ' ') {
				filename[k] = 0;
				while(filename[k+1] == ' ')
					k++;
				if(j >= dMaxArgs)
					break;
				if (filename[k+1] != 0)
					args[++j] = &filename[k+1];
			}
		}
		args[++j] = NULL;

		if (pipe(p_fds) < 0
		 || pipe(c_fds) < 0
		 || (s_pid = fork()) < 0) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		if (s_pid > 0) {	/* Parent process */
			close (p_fds[0]);
			close (c_fds[1]);
			f->fdout = p_fds[1];
			f->fd = c_fds[0];
			errno = 0;
			f->file = (void*)fdopen(f->fd, "r");
			errno = 0;
			f->fileout = (void*)fdopen(f->fdout, "w");
			f->flag_is_pipe = 1;
			f->open_mode = (unsigned char)mode;
			f->file_features &= ~COB_FILE_LS_NULLS;
			f->file_features &= ~COB_FILE_LS_VALIDATE;
			f->flag_ls_instab = 0;
			f->file_pid = s_pid;
			signal (SIGPIPE, SIG_IGN);
			return COB_STATUS_00_SUCCESS;
		} else {			/* Child process */
			close (p_fds[1]);
			close (c_fds[0]);
			if (p_fds[0] != STDIN_FILENO) {
				if(dup2(p_fds[0], STDIN_FILENO) != STDIN_FILENO) {
					cob_runtime_error (_("ERROR FILE %s opening pipe"), f->select_name);
					cob_stop_run (-1);
				}
				close (p_fds[0]);
			}
			if (c_fds[1] != STDOUT_FILENO) {
				if(dup2(c_fds[1], STDOUT_FILENO) != STDOUT_FILENO) {
					cob_runtime_error (_("ERROR FILE %s opening pipe"), f->select_name);
					cob_stop_run (-1);
				}
				close (c_fds[1]);
			}
			execv (filename, args);
			fprintf(stderr,"%s: Command not found.\n",filename);
			fflush(stderr);
			exit(-1);
		}
#else
		return COB_STATUS_48_OUTPUT_DENIED;
#endif
	}


	if (stat(filename, &st) != -1
	 && S_ISDIR(st.st_mode)) {	/* Filename is a directory */
		return COB_XSTATUS_IS_DIR;
	}
	if (!isdirvalid (filename))
		return COB_XSTATUS_NOT_DIR;
	errno = 0;
	if (access (filename, F_OK) && errno == ENOENT) {
		nonexistent = 1;
		if (mode != COB_OPEN_OUTPUT && f->flag_optional == 0) {
			return COB_STATUS_35_NOT_EXISTS;
		}
	}

	fp = NULL;
	fmode = NULL;
	/* Open the file */
	switch (mode) {
	case COB_OPEN_INPUT:
		if ((f->share_mode & COB_SHARE_NO_OTHER)
		 || (f->lock_mode & COB_FILE_EXCLUSIVE) ) {
			fmode = "r+";
#ifndef _WIN32
		} else 
		if (!file_setptr->cob_unix_lf) {
			fmode = "r";
#endif
		} else {
			fmode = "rb";
		}
		break;
	case COB_OPEN_OUTPUT:
#ifdef _WIN32
		fmode = "wb";
#else
		if (!file_setptr->cob_unix_lf) {
			fmode = "w";
		} else {
			fmode = "wb";
		}
#endif
		break;
	case COB_OPEN_I_O:
		fmode = "rb+";
		break;
	case COB_OPEN_EXTEND:
		/* Problem on WIN32 (tested _MSC_VER 1500 and GCC build) if file isn't there: */
		/* Both modes create the file and return a bad pointer */
		/* Mode "a+"  sets EINVAL, further actions on the file do work */
		/* Mode "ab+" doesn't set errno, but we don't want a binary file */
		/* Possible Solutions: */
		/* a) Create the file and reopen it with a+ */
		/* b) Check this stuff in EINVAL and just go on */
#ifdef _WIN32
		fmode = "ab+";
#else
		if (!file_setptr->cob_unix_lf) {
			fmode = "a+";
		} else {
			fmode = "ab+";
		}
#endif
		break;
	/* LCOV_EXCL_START */
	default:
		cob_fatal_error(COB_FERROR_CODEGEN);
	/* LCOV_EXCL_STOP */
	}

	errno = 0;
	f->last_write_mode = COB_LAST_WRITE_UNKNOWN;
	fp = fopen (filename, fmode);
	f->file = fp;
	if (fp) {
		f->fd = fileno (fp);
	} else {
		f->fd = -1;
	}
	switch (errno) {
	case 0:
		f->open_mode = (unsigned char)mode;
		if (f->file_features & COB_FILE_LS_CRLF)
			f->flag_needs_cr = 1;
		break;
	case EINVAL:
		if (f->flag_optional && nonexistent) {
			f->open_mode = (unsigned char)mode;
		} else {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		break;
	case ENOENT:
		if (!isdirvalid (filename))
			return COB_XSTATUS_NOT_DIR;
		if (mode == COB_OPEN_EXTEND || mode == COB_OPEN_OUTPUT) {
			return COB_XSTATUS_NOT_DIR;
		}
		if (f->flag_optional) {
			f->open_mode = (unsigned char)mode;
			f->flag_nonexistent = 1;
			f->flag_end_of_file = 1;
			f->flag_begin_of_file = 1;
			return COB_STATUS_05_SUCCESS_OPTIONAL;
		}
		return COB_STATUS_35_NOT_EXISTS;
	case EACCES:
	case EROFS:
		return COB_STATUS_37_PERMISSION_DENIED;
	case EAGAIN:
		return COB_STATUS_61_FILE_SHARING;
	case EISDIR:
		return COB_XSTATUS_IS_DIR;
	case ENOTDIR:
		return COB_XSTATUS_NOT_DIR;
	default:
		return COB_STATUS_30_PERMANENT_ERROR;
	}

	if (f->flag_select_features & COB_SELECT_LINAGE) {
		if (file_linage_check (f)) {
			if (fp) {
				fclose (fp);
			}
			f->file = NULL;
			f->fd = -1;
			return COB_STATUS_57_I_O_LINAGE;
		}
		f->flag_needs_top = 1;
		lingptr = f->linage;
		cob_set_int (lingptr->linage_ctr, 1);
	}
	(void)cob_set_file_format(f, file_open_io_env, 1);		/* Set file format */

	if (mode == COB_OPEN_EXTEND) {
		f->record_off = set_file_pos (f, -1);
		if(f->lock_mode == 0
		&& f->share_mode == 0)
			return 0;
	}
	if (mode == COB_OPEN_INPUT
	 || mode == COB_OPEN_I_O) {
		f->record_off = set_file_pos (f, f->file_header);
		if(f->lock_mode == 0
		&& f->share_mode == 0)
			return 0;
	}
	if ((ret=set_file_lock(f, filename, mode)) != 0) {
		return ret;
	}
	if (f->flag_optional && nonexistent) {
		return COB_STATUS_05_SUCCESS_OPTIONAL;
	}

	return 0;
}

static int
cob_file_close (cob_file_api *a, cob_file *f, const int opt)
{
	COB_UNUSED (a);

	if (!f->flag_is_pipe)
		f->record_off = ftell ((FILE *)f->file);	/* Ending file position */
	f->flag_close_pend = 0;
	switch (opt) {
	case COB_CLOSE_LOCK:
		/* meaning (not file-sharing related):
		   file may not be opened in *this runtime unit* again */
		/* TODO: set flag here */
		/* Fall through */
	case COB_CLOSE_NORMAL:
	case COB_CLOSE_NO_REWIND:
		if ((f->flag_line_adv & COB_LINE_ADVANCE)
		 && (f->file_features & COB_FILE_LS_CRLF)
		 && f->last_write_mode != COB_LAST_WRITE_UNKNOWN) {
			if (f->flag_needs_cr) {
				COB_CHECKED_FPUTC ('\r', (FILE *)f->file);
				f->flag_needs_cr = 0;
			}
		}
		if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
			if (f->flag_needs_nl 
			 && f->file_format != COB_FILE_IS_MF
			 && !(f->flag_select_features & COB_SELECT_LINAGE)) {
				putc ('\n', (FILE *)f->file);
			}
			f->flag_needs_nl = 0;
		} else if (f->organization == COB_ORG_SEQUENTIAL) {
			if ((f->file_features & COB_FILE_LS_CRLF)
			 && f->last_write_mode != COB_LAST_WRITE_UNKNOWN
			 && f->flag_needs_nl) {
				if (f->fd >= 0) {
					COB_CHECKED_WRITE (f->fd, "\n", 1);
				}
			}
			f->flag_needs_nl = 0;
		} else if (f->flag_needs_nl) {
			f->flag_needs_nl = 0;
			if (f->fd >= 0) {
				COB_CHECKED_WRITE (f->fd, "\n", 1);
			}
		}
#ifdef	HAVE_FCNTL
		/* Unlock the file */
		if (f->fd >= 0
		 && !f->flag_is_pipe) {
			struct flock lock;
			memset ((void *)&lock, 0, sizeof (struct flock));
			lock.l_type = F_UNLCK;
			lock.l_whence = SEEK_SET;
			lock.l_start = 0;
			lock.l_len = 0;
			if (fcntl (f->fd, F_SETLK, &lock) == -1) {
				cob_runtime_warning ("issue during unlock (%s), errno: %d", "cob_file_close", errno);
			}
		}
#endif
		/* Close the file */
		if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
			if (f->flag_is_pipe) {
				if (f->file_pid) {
					if (f->file)
						fclose (f->file);
					if (f->fileout
					 && f->fileout != f->file) 
						fclose (f->fileout);
#if defined (HAVE_UNISTD_H) && !(defined (_WIN32))
					{
						int	sts;
						errno = 0;
						kill (f->file_pid, 0);
						if (errno == ESRCH) {
							waitpid (f->file_pid, &sts, WNOHANG);
						}
						else {
							cob_sleep_msec (50);
							kill (f->file_pid, SIGKILL);
							cob_sleep_msec (50);
							waitpid (f->file_pid, &sts, 0);
						}
				}
#elif defined (_WIN32)
					{
						char buff[COB_MINI_BUFF];
						snprintf((char *)&buff, COB_MINI_MAX, "TASKKILL    /PID %d", f->file_pid);
						system(buff);
					}
#endif
				} else {
					pclose ((FILE *)f->file);
				}
				f->flag_is_pipe = 0;
				f->file = f->fileout = NULL;
				f->fd = f->fdout = -1;
				f->file_pid = 0;
				return COB_STATUS_00_SUCCESS;
			}
#ifdef _MSC_VER /* explicit only stream or fd close with this compiler */
			if (f->file != NULL) {
				fclose ((FILE *)f->file);
				f->file = NULL;
				f->fd = -1;
			}
#endif
		} else {
			if (f->fd >= 0) {
				close (f->fd);
				f->fd = -1;
#ifdef _MSC_VER
				f->file = NULL;
#endif
			}
		}
#ifndef _MSC_VER /* explicit disallowed there, is that the same for other compilers? */
		if (f->file != NULL) {
			fclose ((FILE *)f->file);
			f->file = NULL;
			f->fd = -1;
		}
#endif
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
}

static int
open_next (cob_file *f)
{
	if (f->flag_is_concat
	 && *f->nxt_filename != 0) {
		char	*nx = strchr(f->nxt_filename,file_setptr->cob_concat_sep[0]);
		close (f->fd);
		if (f->file)
			fclose (f->file);
		f->fd = -1;
		f->file = NULL;
		if (nx) {
			*nx = 0;
			if (f->open_mode == COB_OPEN_I_O)	
				f->fd = open (f->nxt_filename, O_RDWR);
			else
				f->fd = open (f->nxt_filename, O_RDONLY);
			f->nxt_filename = nx + 1;
		} else {
			if (f->open_mode == COB_OPEN_I_O)	
				f->fd = open (f->nxt_filename, O_RDWR);
			else
				f->fd = open (f->nxt_filename, O_RDONLY);
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
sequential_read (cob_file_api *a, cob_file *f, const int read_opts)
{
	int	bytesread,padlen;
	union {
		unsigned char	sbuff[4];
		unsigned short	sshort[2];
		unsigned int	sint;
	} recsize;

	COB_UNUSED (a);
	COB_UNUSED (read_opts);

again:
	if (f->flag_operation != 0) {
		f->flag_operation = 0;
	}
	if(f->record_off == -1) {
		f->record_off = set_file_pos (f, (off_t)f->file_header); /* Set current file position */
	} else {
		f->record_off = lseek (f->fd, 0, SEEK_CUR);	/* Get current file position */
		set_file_pos (f, (off_t)f->record_off);
	}

	if (f->record_min != f->record_max) {
		/* Read record size */

		bytesread = read (f->fd, recsize.sbuff, f->record_prefix);
		if (bytesread == 0
		 && open_next (f))
			goto again;
		if (bytesread != (int)f->record_prefix) {
			if (bytesread == 0) {
				return COB_STATUS_10_END_OF_FILE;
			} else {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
		switch (f->file_format) {
		case COB_FILE_IS_GC:
		case COB_FILE_IS_GCVS0:		/* short size plus 2 NULs */
			f->record->size = COB_MAYSWAP_16 (recsize.sshort[0]);
			break;
		case COB_FILE_IS_GCVS1:
			f->record->size = COB_MAYSWAP_32 (recsize.sint);
			break;
		case COB_FILE_IS_GCVS2:
			f->record->size = recsize.sint;
			break;
		case COB_FILE_IS_GCVS3:
			f->record->size = COB_MAYSWAP_16 (recsize.sshort[0]);
			break;
		case COB_FILE_IS_B32:		/* Was varseq 2 on Big Endian system */
			f->record->size = LDCOMPX4(recsize.sbuff);
			break;
		case COB_FILE_IS_L32:		/* Was varseq 2 on Little Endian system */
			f->record->size = LDBINLE4(recsize.sbuff);
			break;
		case COB_FILE_IS_MF:
			if(f->record_prefix == 2) {
				f->record->size = ((recsize.sbuff[0] & 0x0F) << 8) + recsize.sbuff[1];
			} else {
				f->record->size = ((recsize.sbuff[0] & 0x0F) << 24) + (recsize.sbuff[1] << 16) 
						+ (recsize.sbuff[2] << 8) + recsize.sbuff[3];
			}
			break;
		default:
			f->record->size = COB_MAYSWAP_16 (recsize.sshort[0]);
			break;
		}
	}

	/* Read record */
	bytesread = read (f->fd, f->record->data, f->record->size);
	if (bytesread == 0
	 && open_next (f))
		goto again;

	if (f->record_min != f->record_max
	&&  f->file_format == COB_FILE_IS_MF) {
		padlen = ((f->record->size + f->record_prefix + 3) / 4 * 4) - (f->record->size + f->record_prefix);
		if(padlen > 0)
			if (read(f->fd, recsize.sbuff, padlen) != padlen) /* Read past padding chars */
				return COB_STATUS_30_PERMANENT_ERROR;
	}
	if (bytesread != (int)f->record->size) {
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
sequential_write (cob_file_api *a, cob_file *f, int opt)
{
	int	padlen;

	COB_UNUSED (a);
	if (f->flag_operation == 0) {
		f->flag_operation = 1;
	}

	if (f->record_min != f->record_max)	/* No ADVANCING if Variable Length */
		opt = 0;
	if (!(f->flag_line_adv & (COB_LINE_ADVANCE|COB_RECORD_ADVANCE))
	 && f->file_format != COB_FILE_IS_MF)
		opt = 0;

	/* WRITE AFTER */
	if (opt & COB_WRITE_AFTER) {
		if (cob_seq_write_opt (f, opt)) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		f->flag_needs_nl = 1;
		f->last_write_mode = COB_LAST_WRITE_AFTER;
	} else if (f->flag_needs_cr
		 	&& f->last_write_mode != COB_LAST_WRITE_UNKNOWN) {
		COB_CHECKED_FPUTC ('\r', (FILE *)f->file);
		f->flag_needs_cr = 0;
		cob_file_sync (f);
	}

	if (f->open_mode == COB_OPEN_EXTEND
	 && f->file_header == 0) {
		f->record_off = set_file_pos (f, -1);
	} else if(f->record_off == -1) {
		f->record_off = set_file_pos (f, (off_t)f->file_header);
	} else {
		f->record_off = lseek (f->fd, 0, SEEK_CUR);	/* Get current file position */
		set_file_pos (f, (off_t)f->record_off);
	}

	cob_seq_write_rcsz (f, f->record->size);

	/* Write record */
	COB_CHECKED_WRITE (f->fd, f->record->data, f->record->size);

	if (f->record_min != f->record_max
	 && f->file_format == COB_FILE_IS_MF) {
		padlen = ((f->record->size + f->record_prefix + 3) / 4 * 4) - (f->record->size + f->record_prefix);
		while(padlen-- > 0)
			if(write(f->fd, " ",1) != 1)
				return COB_STATUS_30_PERMANENT_ERROR;
	}
	if (f->record_min == f->record_max		/* Fixed Size so CR/LF may be used */
	 && (f->file_features & COB_FILE_LS_CRLF))
		f->flag_needs_cr = 1;

	/* WRITE BEFORE */
	if (opt & COB_WRITE_BEFORE) {
		if (cob_seq_write_opt (f, opt)) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		f->flag_needs_nl = 0;
		f->last_write_mode = COB_LAST_WRITE_BEFORE;
	}
	if (!(opt & COB_WRITE_BEFORE)
	 && !(opt & COB_WRITE_AFTER))
		f->last_write_mode = COB_LAST_WRITE_UNKNOWN;

	return COB_STATUS_00_SUCCESS;
}

static int
sequential_rewrite (cob_file_api *a, cob_file *f, const int opt)
{
	union {
		unsigned char	sbuff[4];
		unsigned short	sshort[2];
		unsigned int	sint;
	} recsize;
	int	bytesread, rcsz, padlen;
	COB_UNUSED (a);
	COB_UNUSED (opt);

	f->flag_operation = 1;
	if (f->record_off != -1) {
		if (lseek (f->fd, f->record_off, SEEK_SET) == -1) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
	} else {
		f->record_off = lseek (f->fd, 0, SEEK_CUR);	/* Get current file position */
		if (lseek (f->fd, (off_t)(f->record_off - f->record->size), SEEK_SET) == -1) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
	}
	set_file_pos (f, (off_t)f->record_off);
	rcsz = f->record->size;
	padlen = 0;
	if (f->record_min != f->record_max
	&& f->record_prefix > 0) {
		bytesread = read (f->fd, recsize.sbuff, f->record_prefix);
		if (bytesread != (int)f->record_prefix) {
			if (bytesread == 0) {
				return COB_STATUS_10_END_OF_FILE;
			} else {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
		switch (f->file_format) {
		case COB_FILE_IS_GC:
		case COB_FILE_IS_GCVS0:
			rcsz = COB_MAYSWAP_16 (recsize.sshort[0]);
			break;
		case COB_FILE_IS_GCVS1:
			rcsz = COB_MAYSWAP_32 (recsize.sint);
			break;
		case COB_FILE_IS_GCVS2:
			rcsz = recsize.sint;
			break;
		case COB_FILE_IS_GCVS3:
			rcsz = COB_MAYSWAP_16 (recsize.sshort[0]);
			break;
		case COB_FILE_IS_B32:		/* Was varseq 2 on Big Endian system */
			rcsz = LDCOMPX4(recsize.sbuff);
			break;
		case COB_FILE_IS_L32:		/* Was varseq 2 on Little Endian system */
			rcsz = LDBINLE4(recsize.sbuff);
			break;
		case COB_FILE_IS_MF:
			if (f->record_prefix == 2) {
				rcsz = ((recsize.sbuff[0] & 0x0F) << 8) + recsize.sbuff[1];
			} else {
				rcsz = ((recsize.sbuff[0] & 0x0F) << 24) + (recsize.sbuff[1] << 16) 
					+ (recsize.sbuff[2] << 8) + recsize.sbuff[3];
			}
			padlen = ((rcsz + f->record_prefix + 3) / 4 * 4) - (rcsz + f->record_prefix);
			break;
		default:
			rcsz = COB_MAYSWAP_16 (recsize.sshort[0]);
			break;
		}
		if ((rcsz + padlen) < f->record->size)
			return COB_STATUS_30_PERMANENT_ERROR;
	}
	if (rcsz > f->record_max)
		return COB_STATUS_30_PERMANENT_ERROR;
	if (f->flag_do_qbl
	 && qblfd != -1) {
		if (lseek (f->fd, f->record_off, SEEK_SET) == -1)
			return COB_STATUS_30_PERMANENT_ERROR;
		if (read (f->fd, qbl_hdr->data, (size_t)rcsz+f->record_prefix) 
					!= (int)rcsz+f->record_prefix) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		if (set_file_pos (f, (off_t)f->record_off+f->record_prefix) == -1)
			return COB_STATUS_30_PERMANENT_ERROR;
	}
	if (write (f->fd, f->record->data, (size_t)rcsz) != (int)rcsz) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	if (f->record_min != f->record_max
	&&  f->file_format == COB_FILE_IS_MF) {
		while(padlen-- > 0)
			if(write(f->fd, " ",1) != 1)
				return COB_STATUS_30_PERMANENT_ERROR;
	}
	return COB_STATUS_00_SUCCESS;
}

#define IS_BAD_CHAR(x) (x < ' ' && x != COB_CHAR_BS && x != COB_CHAR_ESC \
					 && x != COB_CHAR_FF && x != COB_CHAR_SI && x != COB_CHAR_TAB)
/* LINE SEQUENTIAL */
static int
lineseq_read (cob_file_api *a, cob_file *f, const int read_opts)
{
	unsigned char	*dataptr;
	size_t		i = 0;
	int		sts = COB_STATUS_00_SUCCESS;
	int		n;

	COB_UNUSED (a);
	COB_UNUSED (read_opts);

	dataptr = f->record->data;
	if (f->file == NULL)
		return COB_STATUS_30_PERMANENT_ERROR;
again:
	if (!f->flag_is_pipe)
		f->record_off = ftell ((FILE *)f->file);	/* Save position at start of line */
	for (; ;) {
		n = getc ((FILE *)f->file);
		if (n == EOF) {
			if (!i) {
				if (open_next (f))
					goto again;
				return COB_STATUS_10_END_OF_FILE;
			} else {
				break;
			}
		}
		if (n == 0
		 && (f->file_features & COB_FILE_LS_NULLS)) {
			n = getc ((FILE *)f->file);
			/* LCOV_EXCL_START */
			if (n == EOF) {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
			if ((f->file_features & COB_FILE_LS_VALIDATE)
			 && (unsigned char)n >= ' ') {		/* Should be less than a space */
				return COB_STATUS_71_BAD_CHAR;
			}
			/* LCOV_EXCL_STOP */
		} else
		if (n == COB_CHAR_TAB
		 && f->flag_ls_instab) {
			/* FIXME: EXPANDTAB should be separate */
			*dataptr++ = ' ';
			i++;
			while ((i % 8) != 0
				&& i < f->record_max) {
				*dataptr++ = ' ';
				i++;
			}
			continue;
		} else {
			if (n == '\r') {
				continue;			/* Ignore CR on reading */
			}
			if (n == '\n') {
				break;
			}
			if (n == '\f')			/* Skip NEW PAGE on reading */
				continue;
			if ((f->file_features & COB_FILE_LS_VALIDATE)
			 && (IS_BAD_CHAR (n) 
			  || (n > 0x7E && !isprint(n)))) {
				return COB_STATUS_09_READ_DATA_BAD;
			}
		}
		if (i < f->record_max) {
			*dataptr++ = (unsigned char)n;
			i++;
			if (i == f->record_max
			 && (f->file_features & COB_FILE_LS_SPLIT)) {
				/* If record is too long, then simulate end
				 * so balance becomes the next record read */
				long	k = 1;
				n = getc ((FILE *)f->file);
				if (n == '\r') {
					n = getc ((FILE *)f->file);
					k++;
				}
				if (n != '\n') {
					fseek((FILE*)f->file, -k, SEEK_CUR);
					if (!(COB_MODULE_PTR
					 && COB_MODULE_PTR->flag_dialect == COB_DIALECT_MF))
						sts = COB_STATUS_06_READ_TRUNCATE;
				}
				break;
			}
		}
	}
	if (i < f->record_max) {
		/* Fill the record with spaces */
		memset ((unsigned char *)f->record->data + i, ' ',
					f->record_max - i);
	}
	f->record->size = i;
	if (f->open_mode == COB_OPEN_I_O)	/* Required on some systems */
		fflush((FILE*)f->file); 
	return sts;
}

/* Determine the size to be written */
static size_t
lineseq_size (cob_file *f)
{
	size_t size,i;
	if ((f->file_features & COB_FILE_LS_FIXED))
		return f->record->size;
	if (f->variable_record) {
		f->record->size = (size_t)cob_get_int (f->variable_record);
		if (f->record->size > f->record_max) {
			f->record->size = f->record_max;
		}
	}
	if (f->record->size < f->record_min)
		f->record->size = f->record_min;
	if (f->record->size == 0)
		return 0;
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
lineseq_write (cob_file_api *a, cob_file *f, const int opt)
{
	unsigned char		*p;
	cob_linage		*lingptr;
	size_t			size;
	int			ret;
	FILE		*fo;
	COB_UNUSED (a);

	/* Determine the size to be written */
	size = lineseq_size (f);

	fo = (FILE*)f->file;
	if (f->flag_is_pipe) {
		if (f->fdout >= 0) {
			fo = (FILE*)f->fileout;
		}
	} else {
		if (f->flag_select_features & COB_SELECT_LINAGE) {
			if (f->flag_needs_top) {
				int i;
				f->flag_needs_top = 0;
				lingptr = f->linage;
				for (i = 0; i < lingptr->lin_top; ++i) {
					COB_CHECKED_FPUTC ('\n', fo);
				}
			}
		}
		/* WRITE AFTER */
		if ((opt & COB_WRITE_AFTER)
		 &&  !f->flag_is_pipe) {
			if (f->flag_select_features & COB_SELECT_LINAGE) {
				ret = cob_linage_write_opt (f, opt);
				if (ret)
					return ret;
			} else
			if (f->last_write_mode == COB_LAST_WRITE_BEFORE) {
				COB_CHECKED_FPUTC ('\n', (FILE *)f->file);
				f->flag_needs_nl = 0;
				f->flag_needs_cr = 0;
			} else { 
				ret = cob_file_write_opt (f, opt);
				if (ret)
					return ret;
				f->flag_needs_nl = 1;
			}
			f->last_write_mode = COB_LAST_WRITE_AFTER;
		}

		f->record_off = ftell ((FILE *)f->file);	/* Save file position at start of line */
	}

	if ((opt & COB_WRITE_BEFORE) 
	 && f->last_write_mode != COB_LAST_WRITE_BEFORE) {
		if (f->flag_needs_cr
		 && f->last_write_mode != COB_LAST_WRITE_UNKNOWN) {
			COB_CHECKED_FPUTC ('\r', (FILE *)f->file);
		}
		f->flag_needs_cr = 0;
		f->last_write_mode = COB_LAST_WRITE_BEFORE;
	} 
	/* Write to the file */
	if (size) {
		if ((f->file_features & COB_FILE_LS_CRLF))
			f->flag_needs_cr = 1;
		if (f->file_features & COB_FILE_LS_NULLS) {
			size_t i, j, k, t;
			p = f->record->data;
			for (i=j=0; j < (int)size; j++) {
				if (p[j] < ' ') {
					if ((j - i) > 0) {
						COB_CHECKED_FWRITE(fo, &p[i], j - i);
					}
					i = j + 1;
					COB_CHECKED_FPUTC(0x00, fo);
					COB_CHECKED_FPUTC(p[j], fo);
				} else
				if (p[j] == ' '
				 && p[j+1] == ' '
				 && j < (size - 2)
				 && i < j
				 && f->flag_ls_instab) {
					t = ((j + 7) / 8) * 8;
					for(k=j; k < size && p[k] == ' '; k++);
					k = (k / 8) * 8;
					if (k >= t
					 && t < size) {
						if ((j - i) > 0) {
							COB_CHECKED_FWRITE(fo, &p[i], j - i);
						}
						while (t <= k) {
							COB_CHECKED_FPUTC(COB_CHAR_TAB, fo);
							t += 8;
						}
						j = k - 1;
						i = j + 1;
						continue;
					}
				}
			}
			if (i < size) {
				ret = fwrite (&p[i],(int)size - i, 1, fo);
				if (ret <= 0) {
					return errno_cob_sts (COB_STATUS_30_PERMANENT_ERROR);
				}
			}
		} else {
			if (f->file_features & COB_FILE_LS_VALIDATE) {
				int i;
				p = f->record->data;
				for (i = 0; i < (int)size; ++i, ++p) {
					if (IS_BAD_CHAR (*p)) {
						return COB_STATUS_71_BAD_CHAR;
					}
				}
			}
			ret = fwrite (f->record->data, size, (size_t)1, fo);
			/* LCOV_EXCL_START */
			if (ret != 1) {
				return errno_cob_sts (COB_STATUS_30_PERMANENT_ERROR);
			};
			/* LCOV_EXCL_STOP */
		}
	}
	if (f->flag_is_pipe) {
		COB_CHECKED_FPUTC ('\n', fo);
		fflush(fo);
		f->flag_needs_nl = 0;
		return COB_STATUS_00_SUCCESS;
	}

	if (f->flag_select_features & COB_SELECT_LINAGE) {
		COB_CHECKED_FPUTC ('\n', fo);
		f->flag_needs_nl = 0;
	} else
	if ((f->file_features & COB_FILE_LS_CRLF)) {
		if ((opt & COB_WRITE_PAGE)
		 || ((opt & COB_WRITE_BEFORE) && f->flag_needs_nl)) {
			/* CHECKME - possible bug, see discussion board */
		} else if (opt == 0) {
			if (f->flag_needs_cr) {
				COB_CHECKED_FPUTC ('\r', fo);
				f->flag_needs_cr = 0;
			}
		}
	}

	if ((opt == 0) 
	&& !(f->flag_select_features & COB_SELECT_LINAGE)
	&& ((f->file_features & COB_FILE_LS_LF)
	 || (f->file_features & COB_FILE_LS_CRLF))){
		/* At least add 1 LF */
		if (f->flag_needs_cr) {
			COB_CHECKED_FPUTC ('\r', fo);
			f->flag_needs_cr = 0;
		}
		COB_CHECKED_FPUTC ('\n', fo);
		f->flag_needs_nl = 0;
	}

	/* WRITE BEFORE */
	if (opt & COB_WRITE_BEFORE) {
		ret = cob_file_write_opt (f, opt);
		if (ret) {
			return ret;
		}
		f->flag_needs_nl = 0;
		f->last_write_mode = COB_LAST_WRITE_BEFORE;
	}
	if (f->open_mode == COB_OPEN_I_O)	/* Required on some systems */
		fflush((FILE*)f->file); 

	return COB_STATUS_00_SUCCESS;
}

static int
lineseq_rewrite (cob_file_api *a, cob_file *f, const int opt)
{
	unsigned char	*p;
	size_t		size, psize, slotlen, rcsz;
	off_t		curroff, savepos;
	COB_UNUSED (a);

	if (f->flag_is_pipe) 
		return COB_STATUS_30_PERMANENT_ERROR;

	curroff = ftell ((FILE *)f->file);	/* Current file position */
	size = lineseq_size (f);

	p = f->record->data;
	psize = size;
	slotlen = curroff - f->record_off - 1;
	if ((f->file_features & COB_FILE_LS_CRLF)) 
		slotlen--;
	if ((f->file_features & COB_FILE_LS_NULLS)
	 || f->flag_ls_instab) {
		size_t j;
		for (j = 0; j < size; j++) {
			if ((f->file_features & COB_FILE_LS_NULLS)
			 && p[j] < ' ') {
				psize++;
			} else
			if (f->flag_ls_instab
			 && memcmp(&p[j],"        ",8) == 0
			 && j < (size - 2)) {
				while (memcmp(&p[j], "        ", 8) == 0
					&& j < size) {
					j += 8;
					psize -= 6;
				}
			}
		}
	}

	if (psize > slotlen) {
		return COB_STATUS_44_RECORD_OVERFLOW;
	}

	if (fseek((FILE*)f->file, (off_t)f->record_off, SEEK_SET) != 0) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	if (f->flag_do_qbl
	 && qblfd != -1) {			/* Save the before image */
		rcsz = f->record->size;
		savepos = (off_t)f->record_off;
		memcpy (qbl_tmp, f->record->data, rcsz);
		lineseq_read (a, f, 0);
		memcpy (qbl_hdr->data, f->record->data, f->record->size);
		while(f->record->size < f->record_max) {
			qbl_hdr->data[f->record->size++] = ' ';
		}
		cob_put_qbl (f, QBL_BEFORE);
		memcpy (f->record->data, qbl_tmp, rcsz);
		f->record_off = savepos;
		if (fseek((FILE*)f->file, (off_t)savepos, SEEK_SET) != 0) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
	}

	/* Write to the file */
	if (size > 0) {
		if ((f->file_features & COB_FILE_LS_NULLS)
		 || f->flag_ls_instab) {
			size_t i, j, k, t;
			p = f->record->data;
			for (i=j=0; j < size; j++) {
				if ((f->file_features & COB_FILE_LS_NULLS)
				 && p[j] < ' ') {
					if (j - i > 0) {
						COB_CHECKED_FWRITE(f->file, &p[i], j - i);
					}
					i = j + 1;
					COB_CHECKED_FPUTC(0x00, (FILE*)f->file);
					COB_CHECKED_FPUTC(p[j], (FILE*)f->file);
				} else
				/* FIXME: EXPANDTAB should be separate and possibly
				          [Test needed with MF INSERTTAB yes, EXPANDTAB no]
				          handle TAB *Data* here (we write too much in
						  case of TAB not being at 8 char bounds
				          */
				if (f->flag_ls_instab
				 && j < (size - 2)
				 && p[j] == ' '
				 && p[j+1] == ' '
				 && i < j) {
					t = ((j + 7) / 8) * 8;
					for(k=j; k < size && p[k] == ' '; k++);
					k = (k / 8) * 8;
					if (k >= t
					 && t < size) {
						if ((j - i) > 0) {
							COB_CHECKED_FWRITE(f->file, &p[i], j - i);
						}
						while (t <= k) {
							COB_CHECKED_FPUTC(COB_CHAR_TAB, (FILE*)f->file);
							t += 8;
						}
						j = k - 1;
						i = j + 1;
						continue;
					}
				}
			}
			if (i < size) {
				COB_CHECKED_FWRITE(f->file, &p[i],(int)size - i);
			}
		} else {
			if ((f->file_features & COB_FILE_LS_VALIDATE)) {
				int i;
				p = f->record->data;
				for (i = 0; i < (int)size; ++i, ++p) {
					if (IS_BAD_CHAR(*p)) {
						return COB_STATUS_71_BAD_CHAR;
					}
				}
			}
			COB_CHECKED_FWRITE(f->file, f->record->data, size);
		}
		if (psize < slotlen) {
			/* In case new record was shorter, pad with spaces */
			size_t i;
			for (i = psize; i < slotlen; i++) {
				COB_CHECKED_FPUTC(' ', (FILE*)f->file);
			}
		}
	}

	if (fseek((FILE*)f->file, (off_t)curroff, SEEK_SET) != 0) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	if (f->open_mode == COB_OPEN_I_O)	/* Required on some systems */
		fflush((FILE*)f->file); 

	return COB_STATUS_00_SUCCESS;
}

/* RELATIVE */
/*
 * Return size of relative record at given offset
 */
static int
relative_read_size (cob_file *f, off_t off, int *isdeleted)
{
	size_t	relsize = 0;
	unsigned char rechdr[8];

	*isdeleted = 0;
	if (set_file_pos (f, off) == -1)
		return -1;
	if (f->record_prefix > 0) {
		memset (rechdr,0,sizeof(rechdr));
		if (read (f->fd, rechdr, f->record_prefix) != f->record_prefix) {
			return -1;
		}
		switch (f->file_format) {
		case COB_FILE_IS_B32:		/* Was 32bit Big Endian system */
			relsize = LDCOMPX4(rechdr);
			break;
		case COB_FILE_IS_B64:		/* Was 64bit Big Endian system */
			relsize = LDCOMPX4(((unsigned char *)&rechdr[4]));
			break;
		case COB_FILE_IS_L32:		/* Was 32bit Little Endian system */
			relsize = LDBINLE4(rechdr);
			break;
		case COB_FILE_IS_L64:		/* Was 64bit Little Endian system */
			relsize = LDBINLE4(rechdr);
			break;
		case COB_FILE_IS_MF:
			if (f->record_prefix == 2) {
				relsize = ((rechdr[0] & 0x0F) << 8) + rechdr[1];
			} else {
				relsize = ((rechdr[0] & 0x0F) << 24) + (rechdr[1] << 16) 
					+ (rechdr[2] << 8) + rechdr[3];
			}
			if ((rechdr[0] & 0x20)) {
				relsize = 0;	/* Deleted record */
			}
			break;
		default:
			memcpy (&relsize, rechdr, sizeof(relsize));	/* Local native 'size_t' */
			break;
		}
		if (relsize <= 0) {
			*isdeleted = 1;
		}
		return (int)relsize;
	} else
	if (f->file_format == COB_FILE_IS_MF) {
		if (lseek (f->fd, (off + (off_t)f->record_slot - 1), SEEK_SET) == -1 ) {
			return -1;
		}
		rechdr[0] = 0;
		if (read (f->fd, rechdr, 1) != 1)
			return COB_STATUS_30_PERMANENT_ERROR;
		set_file_pos (f, off);
		if (rechdr[0] == 0) {
			*isdeleted = 1;
			return 0;
		} else {
			return (int)f->record_max;
		}
	}
	return 0;
}

/* RELATIVE  START */
static int
relative_start (cob_file_api *a, cob_file *f, const int cond, cob_field *k)
{
	off_t		off;
	size_t		relsize;
	int		kindex;
	int		ksindex;
	int		kcond, isdeleted;
	struct stat	st;
	COB_UNUSED (a);

	if (fstat (f->fd, &st) != 0 || st.st_size == 0) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}

	/* Get the index */
	f->flag_first_read = 0;
	switch (cond) {
	case COB_FI:
		kcond = COB_GE;
		kindex = 0;
		f->flag_first_read = 1;
		break;
	case COB_LA:
		kcond = COB_LE;
		kindex = (st.st_size - f->file_header) / f->record_slot - 1;
		break;
	case COB_LT:
	case COB_LE:
		kcond = cond;
		kindex = cob_get_int (k) - 1;
		/* Check against current file size */
		ksindex = (st.st_size - f->file_header) / f->record_slot - 1;
		if (kindex > ksindex) {
			kindex = ksindex + 1;
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
		off = kindex * f->record_slot + f->file_header;
		if (off >= st.st_size) {
			if (kcond == COB_LT || kcond == COB_LE) {
				kindex--;
				continue;
			}
			break;
		}
		relsize = relative_read_size(f, off, &isdeleted);

		/* Check if a valid record */
		if (relsize > 0 && !isdeleted) {
			f->record_off = off;
			set_file_pos (f, off);	/* Set file position to start of record */
			if (f->access_mode == COB_ACCESS_SEQUENTIAL
			&&  f->keys[0].field) {
				f->cur_rec_num = (((off - f->file_header) / f->record_slot) + 1);
				cob_set_int (f->keys[0].field, (int)f->cur_rec_num);
			}
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

/*
 * Read relative record at given offset
 */
static int
relative_read_off (cob_file *f, off_t off)
{
	unsigned char recmark[2];
	size_t	relsize = 0;
	int	isdeleted=0;

	relsize = relative_read_size(f, off, &isdeleted);
	if(relsize < 0) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}

	if (relsize == 0 || isdeleted) {
		f->record->size = 0;
		set_file_pos (f, off);
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}

	if (relsize > f->record_max) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}

	if (read (f->fd, f->record->data, (size_t)relsize) != relsize) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	f->record->size = relsize;
	f->record_off = off;

	if (f->keys[0].field) {
		f->cur_rec_num = (((off - f->file_header) / f->record_slot) + 1);
		cob_set_int (f->keys[0].field, 0);
		if (cob_add_int (f->keys[0].field, (int)f->cur_rec_num, COB_STORE_KEEP_ON_OVERFLOW) != 0) {
			set_file_pos (f, off);
			return COB_STATUS_14_OUT_OF_KEY_RANGE;
		}
	}
	if (f->file_format == COB_FILE_IS_MF) {
		if(f->record_min != f->record_max) {
			lseek (f->fd, (off + (off_t)f->record_slot - 1), SEEK_SET);
		}
		if (read (f->fd, recmark, 1) != 1)	/* Active Record marker */
			return COB_STATUS_30_PERMANENT_ERROR;
		if (recmark[0] == 0x00) {	/* Flagged Deleted */
			f->record->size = 0;
			set_file_pos (f, off);
			return COB_STATUS_23_KEY_NOT_EXISTS;
		}
	}
	return COB_STATUS_00_SUCCESS;
}

static int
relative_read (cob_file_api *a, cob_file *f, cob_field *k, const int read_opts)
{
	off_t	off;
	int	relnum,errsts;
	struct stat	st;
	COB_UNUSED (a);

	if (f->flag_operation != 0) {
		f->flag_operation = 0;
	}

	relnum = cob_get_int (k) - 1;
	if (relnum < 0) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	off = relnum * f->record_slot + f->file_header;

	if (fstat (f->fd, &st) != 0 || st.st_size == 0) {
		return COB_STATUS_10_END_OF_FILE;
	}
	if(off >= st.st_size) {
		if (f->access_mode == COB_ACCESS_SEQUENTIAL) 
			return COB_STATUS_10_END_OF_FILE;
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	set_lock_opts (f, read_opts);
	if(f->flag_lock_rec) {
		lock_record (f, relnum+1, f->flag_lock_mode, &errsts);
		if (errsts != 0) {
			switch (errsts) {
			case EACCES:
			case EAGAIN:
				return COB_STATUS_51_RECORD_LOCKED;
			case EDEADLK:
				return COB_STATUS_52_DEAD_LOCK;
			case ENOLCK:
				return COB_STATUS_53_MAX_LOCKS;
			default:
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
	}
	return relative_read_off(f, off);
}

static int
relative_read_next (cob_file_api *a, cob_file *f, const int read_opts)
{
	off_t	curroff;
	off_t	relsize;
	int		relnum;
	cob_u32_t	moveback;
	struct stat	st;
	int		sts = 0;
	int		errsts;
	COB_UNUSED (a);

	if (f->flag_operation != 0) {
		f->flag_operation = 0;
	}

	relsize = (off_t)f->record_slot;
	if (fstat (f->fd, &st) != 0 || st.st_size == 0) {
		return COB_STATUS_10_END_OF_FILE;
	}
	/* LCOV_EXCL_START */
	if (st.st_size < relsize) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	/* LCOV_EXCL_STOP */

	if(f->record_off == -1) {
		curroff = lseek (f->fd, (off_t)f->file_header, SEEK_SET);	/* Set current file position */
	} else {
		curroff = lseek (f->fd, 0, SEEK_CUR);	/* Get current file position */
	}
	if (f->flag_operation != 0) {
		f->flag_operation = 0;
	}
	moveback = 0;

	switch (read_opts & COB_READ_MASK) {
	case COB_READ_FIRST:
		curroff = f->file_header;
		break;
	case COB_READ_LAST:
		curroff = st.st_size - f->record_slot;
		moveback = 1;
		break;
	case COB_READ_PREVIOUS:
		if (f->flag_first_read) {
			break;
		} else if (curroff > (f->record_slot + f->file_header)) {
			curroff -= (f->record_slot * 2);
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
		if(st.st_size <= curroff)
			break;
		set_lock_opts (f, read_opts);
		if(f->flag_lock_rec) {
			relnum = ((curroff - f->file_header) / f->record_slot) + 1;
			lock_record (f, relnum, f->flag_lock_mode, &errsts);
			if (errsts != 0) {
				switch (errsts) {
				case EACCES:
				case EAGAIN:
					if ((f->retry_mode & COB_ADVANCING_LOCK)
					 || (read_opts & COB_READ_ADVANCING_LOCK))
						goto next_record;
					return COB_STATUS_51_RECORD_LOCKED;
				case EDEADLK:
					if ((f->retry_mode & COB_ADVANCING_LOCK)
					 || (read_opts & COB_READ_ADVANCING_LOCK))
						goto next_record;
					return COB_STATUS_52_DEAD_LOCK;
				case ENOLCK:
					return COB_STATUS_53_MAX_LOCKS;
				default:
					return COB_STATUS_30_PERMANENT_ERROR;
				}
			}
		}
		sts = relative_read_off (f, curroff);

		if (sts == COB_STATUS_00_SUCCESS) {
			set_file_pos (f, curroff + f->record_slot);
			return COB_STATUS_00_SUCCESS;
		}
		if (sts == COB_STATUS_30_PERMANENT_ERROR
		||  sts == COB_STATUS_10_END_OF_FILE
		||  sts == COB_STATUS_14_OUT_OF_KEY_RANGE) {
			return sts;
		}
next_record:
		if (moveback) {
			if (curroff > (f->record_slot + f->file_header)) {
				curroff -= (f->record_slot * 2);
			} else {
				break;
			}
		} else {
			curroff += f->record_slot;
		}
	}
	return COB_STATUS_10_END_OF_FILE;
}

/*
 * Write Relative record prefix
 */
static int
relative_write_size (cob_file *f, off_t off, int recsize)
{
	size_t	relsize = 0;
	unsigned char rechdr[8];

	if (lseek (f->fd, off, SEEK_SET) == -1 ) {
		return -1;
	}
	f->record_off = off;
	if (f->record_prefix > 0) {
		memset(rechdr,0,sizeof(rechdr));
		switch (f->file_format) {
		case COB_FILE_IS_B32:		/* Was 32bit Big Endian system */
			STCOMPX4(recsize, rechdr);
			break;
		case COB_FILE_IS_B64:		/* Was 64bit Big Endian system */
			STCOMPX4(recsize,((unsigned char *)&rechdr[4]));
			break;
		case COB_FILE_IS_L32:		/* Was 32bit Little Endian system */
			STBINLE4(recsize, rechdr);
			break;
		case COB_FILE_IS_L64:		/* Was 64bit Little Endian system */
			STBINLE4(recsize, rechdr);
			break;
		case COB_FILE_IS_MF:
			if(f->record_prefix == 2) {
				STCOMPX2(recsize, rechdr);
			} else {
				STCOMPX4(recsize, rechdr);
			}
			rechdr[0] |= 0x40;
			break;
		default:
			relsize = recsize;
			memcpy(rechdr, &relsize, sizeof(relsize));	/* Local native 'size_t' */
			break;
		}
		if (write (f->fd, rechdr, f->record_prefix) != f->record_prefix) {
			return -1;
		}
	}
	return recsize;
}

static int
relative_padout(cob_file *f, char pad, int len)
{
	unsigned char wrk[32];
	memset(wrk, pad, sizeof(wrk));
	while(len > sizeof(wrk)) {
		/* Pad out record on disk */
		if (write (f->fd, wrk, sizeof(wrk)) != sizeof(wrk))
			return 1;
		len -= sizeof(wrk);
	}
	if(len > 0)
		if (write (f->fd, wrk, len) != len)
			return 1;
	return 0;
}

static int
relative_write (cob_file_api *a, cob_file *f, const int opt)
{
	off_t	off;
	size_t	relsize;
	int	isdeleted=0;
	int	kindex,rcsz;
	struct stat	st;
	COB_UNUSED (opt);
	COB_UNUSED (a);

	rcsz = (int)f->record->size;
	if (f->flag_operation == 0) {
		f->flag_operation = 1;
	}

	f->last_key = f->keys[0].field;
	if (f->access_mode != COB_ACCESS_SEQUENTIAL) {
		kindex = cob_get_int (f->keys[0].field) - 1;
		if (kindex < 0) {
			return COB_STATUS_24_KEY_BOUNDARY;
		}
		off = (off_t) (f->file_header + f->record_slot * kindex);
		if (fstat (f->fd, &st) != 0) {
			return COB_STATUS_10_END_OF_FILE;
		}
		if(off < st.st_size) {
			relsize = relative_read_size(f, off, &isdeleted);
			if ((long)relsize < 0)
				return COB_STATUS_30_PERMANENT_ERROR;
			if ((long)relsize > 0) {
				return COB_STATUS_22_KEY_EXISTS;
			}
		} else {
			off = set_file_pos (f, off);	/* Set current file position */
		}
	} else {
		if(f->record_off == -1) {
			off = (off_t)lseek (f->fd, (off_t)f->file_header, SEEK_SET);	/* Set current file position */
		} else {
			off = (off_t)lseek (f->fd, 0, SEEK_CUR);	/* Get current file position */
		}
	}

	if (f->variable_record) {
		f->record->size = (size_t)cob_get_int (f->variable_record);
		if (f->record->size > rcsz) {
			f->record->size = rcsz;
		}
	} else {
		f->record->size = rcsz;
	}

	relsize = relative_write_size(f, off, f->record->size);
	if (relsize < 0) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	if (write (f->fd, f->record->data, f->record->size) != (int)f->record->size) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	if (relative_padout(f, ' ', f->record_max - f->record->size)) /* Pad out with SPACES */
		return COB_STATUS_30_PERMANENT_ERROR;

	if (f->file_format == COB_FILE_IS_MF) {
		if ((f->file_features & COB_FILE_LS_CRLF)) {	/* Windows format */
			if (write (f->fd, "\r", 1) != 1)
				return COB_STATUS_30_PERMANENT_ERROR;
		}
		if (write (f->fd, "\n", 1) != 1)
			return COB_STATUS_30_PERMANENT_ERROR;
	}

	/* Update RELATIVE KEY */
	if (f->access_mode == COB_ACCESS_SEQUENTIAL) {
		if (f->keys[0].field) {
			f->cur_rec_num = ((off + f->record_slot - f->file_header) / f->record_slot);
			cob_set_int (f->keys[0].field, (int)f->cur_rec_num);
		}
	}

	return COB_STATUS_00_SUCCESS;
}

static int
relative_rewrite (cob_file_api *a, cob_file *f, const int opt)
{
	off_t	off;
	size_t	relsize;
	int	relnum,isdeleted=0,errsts;

	COB_UNUSED (a);
	f->flag_operation = 1;
	f->last_key = f->keys[0].field;
	if (f->access_mode == COB_ACCESS_SEQUENTIAL) {
		off = (off_t)f->record_off;
		relnum = (off - f->file_header) / f->record_slot;
	} else {
		relnum = cob_get_int (f->keys[0].field) - 1;
		if (relnum < 0) {
			return COB_STATUS_24_KEY_BOUNDARY;
		}
		off = f->file_header + relnum * f->record_slot;
	}
	relsize = relative_read_size(f, off, &isdeleted);
	if (relsize < 0)
		return COB_STATUS_30_PERMANENT_ERROR;
	if (relsize == 0 || isdeleted) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}

	set_lock_opts (f, opt);
	if (f->variable_record) {
		f->record->size = (size_t)cob_get_int (f->variable_record);
		if (f->record->size > f->record_max) {
			f->record->size = f->record_max;
		}
	}
	relsize = relative_write_size(f, off, f->record->size);
	if (relsize < 0) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	if (f->flag_record_lock) {
		lock_record (f, relnum+1, 1, &errsts);
		if (errsts != 0) {
			switch (errsts) {
			case EACCES:
			case EAGAIN:
				return COB_STATUS_51_RECORD_LOCKED;
			case EDEADLK:
				return COB_STATUS_52_DEAD_LOCK;
			case ENOLCK:
				return COB_STATUS_53_MAX_LOCKS;
			default:
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
	}
	if (write (f->fd, f->record->data, f->record->size) != (int)f->record->size) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	if (relative_padout(f, ' ', f->record_max - f->record->size)) /* Pad out with SPACES */
		return COB_STATUS_30_PERMANENT_ERROR;

	if (f->file_format == COB_FILE_IS_MF) {
		if(f->record_min == f->record_max) {	/* Fixed size */
			if (write (f->fd, "\n", 1) != 1)
				return COB_STATUS_30_PERMANENT_ERROR;
		} else {
			lseek (f->fd, (off + (off_t)f->record_slot), SEEK_SET);
		}
	}
	if (f->access_mode == COB_ACCESS_SEQUENTIAL) {
		f->record_off = lseek (f->fd, 0, SEEK_CUR);	/* Save current file position */
		set_file_pos (f, f->record_off);
	}
	if (f->flag_record_lock) {
		if ((f->lock_mode & COB_LOCK_MULTIPLE)) {
			if ((opt & COB_WRITE_NO_LOCK)) {
				unlock_record (f, relnum+1);
			}
		} else {
			if (!(opt & COB_WRITE_LOCK)) {
				unlock_record (f, relnum+1);
			}
		}
	}
	return COB_STATUS_00_SUCCESS;
}

static int
relative_delete (cob_file_api *a, cob_file *f)
{
	off_t	off;
	size_t	relsize;
	struct stat	st;
	unsigned char rechdr[8];
	int	relnum,isdeleted,errsts;

	COB_UNUSED (a);
	f->flag_operation = 1;
	relnum = cob_get_int (f->keys[0].field) - 1;
	if (relnum < 0) {
		return COB_STATUS_24_KEY_BOUNDARY;
	}
	off = f->file_header + relnum * f->record_slot;
	relsize = relative_read_size(f, off, &isdeleted);
	if (relsize < 0)
		return COB_STATUS_30_PERMANENT_ERROR;
	if (relsize == 0 || isdeleted) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	fstat (f->fd, &st);
	if ((off + f->record_slot) >= st.st_size) {	/* Last of file so truncate */
		if (ftruncate (f->fd, off))
			return COB_STATUS_30_PERMANENT_ERROR;
		f->max_rec_num = (st.st_size - f->file_header) / f->record_slot;
		return COB_STATUS_00_SUCCESS;
	}

	set_lock_opts (f, 0);
	if ( (f->record_off = set_file_pos (f, off)) == -1 )
		return -1;
	if (f->flag_record_lock) {
		lock_record (f, relnum+1, 1, &errsts);
		if (errsts != 0) {
			switch (errsts) {
			case EACCES:
			case EAGAIN:
				return COB_STATUS_51_RECORD_LOCKED;
			case EDEADLK:
				return COB_STATUS_52_DEAD_LOCK;
			case ENOLCK:
				return COB_STATUS_53_MAX_LOCKS;
			default:
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
	}
	memset(rechdr,0,sizeof(rechdr));
	if (f->record_prefix > 0) {
		switch (f->file_format) {
		case COB_FILE_IS_B32:		/* Was 32bit Big Endian system */
			STCOMPX4(0, rechdr);
			break;
		case COB_FILE_IS_B64:		/* Was 64bit Big Endian system */
			STCOMPX4(0,((unsigned char *)&rechdr[4]));
			break;
		case COB_FILE_IS_L32:		/* Was 32bit Little Endian system */
			STBINLE4(0, rechdr);
			break;
		case COB_FILE_IS_L64:		/* Was 64bit Little Endian system */
			STBINLE4(0, rechdr);
			break;
		case COB_FILE_IS_MF:
			if(f->record_prefix == 2) {
				STCOMPX2(relsize, rechdr);
			} else {
				STCOMPX4(relsize, rechdr);
			}
			rechdr[0] |= 0x20;
			break;
		default:
			relsize = 0;
			memcpy(rechdr, &relsize, sizeof(relsize));	/* Local native 'size_t' */
			break;
		}
		if (write (f->fd, rechdr, f->record_prefix) != f->record_prefix) {
			return -1;
		}
		if (f->file_format == COB_FILE_IS_MF) {
			if (lseek (f->fd, (off + (off_t)(f->record_slot - 1)), SEEK_SET) == -1 ) {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
			rechdr[0] = 0;
			if (write (f->fd, rechdr, 1) != 1)	/* 0x00 means deleted record */
				return COB_STATUS_30_PERMANENT_ERROR;
		}
	} else
	if (f->file_format == COB_FILE_IS_MF) {
		if (lseek (f->fd, (off + (off_t)f->record_max), SEEK_SET) == -1 ) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		rechdr[0] = 0;
		if (write (f->fd, rechdr, 1) != 1)	/* 0x00 means deleted record */
			return COB_STATUS_30_PERMANENT_ERROR;
	}
	set_file_pos (f, (off_t)f->record_off);
	if (f->flag_record_lock) {
		unlock_record (f, relnum+1);
	}
	return COB_STATUS_00_SUCCESS;
}

static void
cob_file_unlock (cob_file *f)
{
	if (COB_FILE_SPECIAL(f)
	 || f->flag_is_pipe) {
		return;
	}

	if (f->open_mode != COB_OPEN_CLOSED 
	 && f->open_mode != COB_OPEN_LOCKED) {
		if (f->organization == COB_ORG_SORT) {
			return;
		}
		if (f->organization != COB_ORG_INDEXED) {
			if (f->fd >= 0) {
				fdcobsync (f->fd);
			}
#ifdef	HAVE_FCNTL
			if (f->flag_file_lock) {
				/* Unlock the file */
				if (f->fd >= 0) {
					struct flock	lock;
					memset ((void *)&lock, 0, sizeof (struct flock));
					lock.l_type = F_UNLCK;
					lock.l_whence = SEEK_SET;
					lock.l_start = 0;
					lock.l_len = 0;
					if (fcntl (f->fd, F_SETLK, &lock) == -1) {
						cob_runtime_warning ("issue during unlock (%s), errno: %d",
							"cob_file_unlock", errno);
					}
				}
			}
#endif

		} else {
			fileio_funcs[get_io_ptr (f)]->iounlock (&file_api, f);
		}
	}
}

/* Global functions */

/*
 * Allocate memory for cob_file
 */
void
cob_file_create (
	cob_file **	pfl, 
	const char *exname,
	const char *select_name,
	const int	fileorg,
	const int	accessmode,
	const int	optional,
	const int	format,
	const int	select_features,
	const int	nkeys,
	const int	minrcsz,
	const int	maxrcsz,
	cob_field *	assign,
	cob_field *	record)
{
	cob_file	*fl;
	int select = select_features;
	int	extra = 4;
	if (exname == NULL) {
		fl = cob_cache_malloc (sizeof (cob_file) + extra);
		fl->file_version = COB_FILE_VERSION;
	} else {
		fl = cob_external_addr (exname, sizeof (cob_file) + extra);
		select |= COB_SELECT_EXTERNAL;
		if (fl->file_version == 0)
			fl->file_version = COB_FILE_VERSION;
	}
	if (!fl->flag_ready) {
		if (nkeys > 0
		 && fl->keys == NULL) {
			fl->keys = cob_cache_malloc (sizeof (cob_file_key) * nkeys);
		}
		fl->nkeys = nkeys;
		memset(fl->file_status,'0',4);
		fl->select_name = select_name;
		fl->organization = (enum cob_file_org)fileorg;
		fl->access_mode = (enum cob_file_access)accessmode;
		fl->flag_optional = (unsigned char)optional;
		fl->file_format = (unsigned char)format;
		fl->flag_select_features = (unsigned char)select;
		fl->assign = assign;
		fl->record = record;
		fl->record_min = minrcsz;
		fl->record_max = maxrcsz;
		fl->open_mode = COB_OPEN_CLOSED;
		fl->tran_open_mode = COB_OPEN_CLOSED;
		fl->fd = -1;
		fl->flag_vb_isam = 0;
	}
	*pfl = fl;
}

/*
 * Free memory for cob_file
 */
void
cob_file_destroy (cob_file **pfl)
{
	cob_file	*fl;
	if (pfl != NULL 
	 && *pfl != NULL) {
		fl = *pfl;
		if (fl->linage) {
			cob_cache_free (fl->linage);
			fl->linage = NULL;
		}
		if (fl->keys) {
			cob_cache_free (fl->keys);
			fl->keys = NULL;
		}
		if (fl->org_filename) {
			cob_cache_free (fl->org_filename);
			fl->org_filename = NULL;
		}
		cob_cache_free (fl);
		*pfl = NULL;
	}
}

/*
 * Set some attributes of the file
 */
void
cob_file_set_attr (
	cob_file *	fl,
	cob_field *	varsize,
	const int	lineadv,
	const int	features,
	const unsigned char	*codeset,
	cob_field * password,
	cob_field * cryptkey)
{
	if (fl->flag_ready)
		return;
	COB_UNUSED(codeset);
	fl->variable_record = varsize;
	fl->flag_line_adv = (unsigned char)lineadv;
	fl->file_features = (unsigned char)features;
	if(password) {
		/* Nothing implemented at this time */
	}
	if(cryptkey) {
		/* Nothing implemented at this time */
	}
	if(codeset) {
		/* Nothing implemented at this time */
	}
}

/*
 * Define an index of the file
 */
void
cob_file_set_key (
	cob_file *	fl,
	const int	keyn,
	cob_field *	key,
	const int	dups,
	const int	ascdesc,
	const int	len_suppress,
	const unsigned char	*suppress,
	const int	parts,
	...)				/* cob_field * for each component */
{
	cob_field	*kp;
	va_list     args;
	int			i;

	if (keyn > (int)fl->nkeys
	 || fl->flag_ready)
		return;
	fl->keys[keyn].keyn = (unsigned char)keyn;
	fl->keys[keyn].tf_ascending = COB_ASCENDING;
	fl->keys[keyn].field = key;
	if (key != NULL)
		fl->keys[keyn].offset = (unsigned int)(key->data - fl->record->data);
	else
		fl->keys[keyn].offset = 0;
	fl->keys[keyn].tf_duplicates = dups ? 1 : 0;
	fl->keys[keyn].tf_ascending = (unsigned char)ascdesc;
	if (len_suppress < 0
	 || suppress == NULL) {
		fl->keys[keyn].tf_suppress = 0;
		fl->keys[keyn].char_suppress = 0;
	} else  {
		if (len_suppress == 0) {
			fl->keys[keyn].tf_suppress = 1;
			fl->keys[keyn].char_suppress = (unsigned char)*suppress;
		} else {
			fl->keys[keyn].len_suppress = (short)len_suppress;
			fl->keys[keyn].str_suppress = (unsigned char*)suppress;
			if (!dups) {
				fl->keys[keyn].tf_duplicates = 2;	/* Precheck on RE/WRITE */
				fl->flag_write_chk_dups = 1;
			}
		}
	}
	if (parts > 0) {
		fl->keys[keyn].count_components = (short)parts;
		va_start (args, parts);
		for (i=0; i < parts && i < COB_MAX_KEYCOMP; i++) {
			kp = va_arg (args, cob_field *);
			fl->keys[keyn].component[i] = kp;
			if (i == 0)
				fl->keys[keyn].offset = (unsigned int)(kp->data - fl->record->data);
		}
		va_end (args);
	} else {
		fl->keys[keyn].count_components = 1;
		fl->keys[keyn].component[0] = key;
	}
}

/*
 * Extra Define for index of the file
 */
void
cob_file_set_key_extra (
	cob_file *	fl,
	const int	keyn,
	const int	compress,
	const int	encrypt,
	cob_field *	password,
	const unsigned char	*collate)
{
	COB_UNUSED(compress);
	COB_UNUSED(encrypt);
	COB_UNUSED(collate);

	if (keyn > (int)fl->nkeys
	 || fl->flag_ready)
		return;
	if(password) {
		/* Nothing implemented at this time */
	}
}

/*
 * Set the file LINAGE
 */
void
cob_file_set_linage (
	cob_file *	fl,
	cob_field	*linage,		/* LINAGE */
	cob_field	*linage_ctr,	/* LINAGE-COUNTER */
	cob_field	*latfoot,		/* LINAGE FOOTING */
	cob_field	*lattop,		/* LINAGE AT TOP */
	cob_field	*latbot)		/* LINAGE AT BOTTOM */
{
	cob_linage *l;
	if (fl->linage == NULL) {
		fl->linage = cob_cache_malloc (sizeof (cob_linage));
	}
	l = (cob_linage *)fl->linage;
	l->linage = linage;
	l->linage_ctr = linage_ctr;
	l->latfoot = latfoot;
	l->lattop = lattop;
	l->latbot = latbot;
	l->lin_lines = 0;
	l->lin_foot = 0;
	l->lin_top = 0;
	l->lin_bot = 0;
}

/*
 * Set the file lock/retry option
 */
void
cob_file_set_retry (
	cob_file *	fl,
	const int	mode,
	const int	value)
{
	fl->retry_mode = (unsigned short)mode;
	if (mode == COB_RETRY_TIMES)
		fl->retry_times = value;
	else if (mode == COB_RETRY_SECONDS)
		fl->retry_seconds = value;

	if (!fl->flag_ready) {
		fl->dflt_retry = (unsigned short)mode;
		if (mode == COB_RETRY_TIMES)
			fl->dflt_times = value;
		else if (mode == COB_RETRY_SECONDS)
			fl->dflt_seconds = value;
	}
}

/*
 * Set the file lock option
 */
void
cob_file_set_lock (
	cob_file *	fl,
	const int	mode)
{
	fl->lock_mode = (unsigned char)mode;
}

/*
 * Setup of the file is now complete 
 */
void
cob_file_complete ( cob_file * fl)
{
	fl->flag_ready = 1;
}

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
 * Save the XFD name for this file
 */
void
cob_file_xfdname (cob_file *fl, const char *name)
{
	fl->xfdname = name;
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
	fl->open_mode = COB_OPEN_CLOSED;
	fl->nkeys = nkeys;	

	if (nkeys > 0
	 && pky != NULL) {
		*pky = fl->keys = cob_cache_malloc (sizeof (cob_file_key) * nkeys);
	}

	if (linage > 0) {
		fl->linage = cob_cache_malloc (sizeof (cob_linage));
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
		if (fl->linage) {
			cob_cache_free (fl->linage);
			fl->linage = NULL;
		}
		if (*pfl != NULL) {
			cob_cache_free (*pfl);
			*pfl = NULL;
		}
	}
}


void
cob_unlock_file (cob_file *f, cob_field *fnstatus)
{
	cob_file_unlock (f);
	cob_file_save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
}

/*
 * Prepare for Open of data file; Used by fextfh.c
 */
void
cob_pre_open (cob_file *f)
{
	f->flag_file_map = 0;
	f->flag_nonexistent = 0;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
	f->flag_first_read = 2;
	f->flag_operation = 0;
	f->record_off = 0;
	f->max_rec_num = 0;
	f->cur_rec_num = 0;

	cob_set_file_defaults (f);

	/* Obtain the file name */
	if (f->assign != NULL
	 && f->assign->data != NULL) {
		cob_field_to_string (f->assign, file_open_name, (size_t)COB_FILE_MAX);

		f->flag_file_map = 1;
		cob_chk_file_mapping (f, NULL);

		cob_set_file_format (f, file_open_io_env, 1);
	}

	if (f->organization == COB_ORG_INDEXED
	 && f->flag_auto_type) {
		int		ftype;
		ftype = indexed_file_type (f, file_open_name);
		if (ftype >= 0) {
			f->record_min = f->record_max;
			f->io_routine = (unsigned char)ftype;
		}
	}

	if (f->share_mode == 0
	 && f->lock_mode == 0) {
		/* Set default file sharing */
		if (f->last_open_mode == COB_OPEN_INPUT) {
			if (f->dflt_share != 0)
				f->share_mode = f->dflt_share;
			else
				f->share_mode = COB_SHARE_READ_ONLY;
		} else {
			f->share_mode = COB_LOCK_OPEN_EXCLUSIVE|COB_SHARE_NO_OTHER;
		}
	}
}

/*
 * Prepare for Open of data file; Used by cobfile.c
 */
void
cob_pre_open_def (cob_file *f, char *setdef, char *isdef, int checkfile)
{
	f->flag_file_map = 1;
	f->flag_nonexistent = 0;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
	f->flag_first_read = 2;
	f->flag_operation = 0;
	f->flag_updt_file = 0;
	f->record_off = 0;
	f->max_rec_num = 0;
	f->cur_rec_num = 0;
	if (f->assign != NULL
	 && f->assign->data != NULL) {
		cob_field_to_string (f->assign, file_open_name, (size_t)COB_FILE_MAX);
	}

	cob_set_file_defaults (f);
	f->flag_updt_file = 1;
	f->flag_keycheck = 0;

	(void)cob_set_file_format (f, setdef, 1);

	if (f->organization == COB_ORG_INDEXED
	 && checkfile) {
		int		ftype;
		f->flag_keycheck = 0;
		ftype = indexed_file_type (f, file_open_name);
		if (ftype >= 0) {
			f->record_min = f->record_max;
			f->io_routine = (unsigned char)ftype;
		}
	}
	if (isdef)
		write_file_def (f, isdef);
}

/*
 * Open the data file
 */
void
cob_open (cob_file *f, const int mode, const int sharing, cob_field *fnstatus)
{
	char	*cp;
	if (f->file_version != COB_FILE_VERSION) {
		cob_runtime_error (_("ERROR FILE %s does not match current version; Recompile the program"),
							f->select_name);
		cob_stop_run (1);
	}

	f->last_operation = COB_LAST_OPEN;
	if (f->flag_auto_type)
		f->flag_keycheck = 0;
	if ((f->lock_mode & COB_LOCK_ROLLBACK)
	 && f->flag_close_pend) {
		f->flag_end_of_file = 0;
		f->flag_begin_of_file = 0;
		if (mode == COB_OPEN_INPUT
		 || mode == COB_OPEN_I_O) {
			f->flag_first_read = 2;
			if (f->organization != COB_ORG_INDEXED) {
				f->record_off = set_file_pos (f, (off_t)f->file_header);
				f->flag_begin_of_file = 1;
				f->cur_rec_num = 0;
			}
		} else if (mode == COB_OPEN_EXTEND) {
			if (f->organization != COB_ORG_INDEXED) {
				f->record_off = set_file_pos (f, -1);
				f->flag_end_of_file = 1;
				if (f->record_off == f->file_header)
					f->flag_begin_of_file = 1;
			}
		} else if (mode == COB_OPEN_OUTPUT) {
			/* FIXME: What to do here? */
			cob_runtime_warning ("Open %s OUTPUT with CLOSE pending", f->select_name);
			f->cur_rec_num = f->max_rec_num = 0;
		}
		f->last_open_mode = (unsigned char)mode;
		f->open_mode = (unsigned char)mode;
		f->share_mode = (unsigned char)sharing;
		f->lock_mode |= COB_LOCK_MULTIPLE;
		if ((f->share_mode & COB_LOCK_OPEN_EXCLUSIVE))
			f->share_mode |= COB_SHARE_NO_OTHER;
		cob_file_save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
		return;
	}

	if (f->flag_io_tran
	 && f->flag_close_pend) {
		cob_file_save_status (f, fnstatus,
				 fileio_funcs[get_io_ptr (f)]->open (&file_api, f, file_open_name,
									mode, sharing));
		return;
	}

	f->flag_read_done = 0;
	f->curkey = -1;
	f->mapkey = -1;

	/* File was previously closed with lock */
	if (f->open_mode == COB_OPEN_LOCKED) {
		cob_file_save_status (f, fnstatus, COB_STATUS_38_CLOSED_WITH_LOCK);
		return;
	}

	/* File is already open */
	if (f->open_mode != COB_OPEN_CLOSED) {
		cob_file_save_status (f, fnstatus, COB_STATUS_41_ALREADY_OPEN);
		return;
	}

	f->last_open_mode = (unsigned char)mode;
	f->share_mode = (unsigned char)sharing;
	if ((f->share_mode & COB_LOCK_OPEN_EXCLUSIVE))
		f->share_mode |= COB_SHARE_NO_OTHER;
	if (mode == COB_OPEN_OUTPUT)
		f->cur_rec_num = f->max_rec_num = 0;

	if (f->fcd) {
		cob_fcd_file_sync (f, file_open_name);		/* Copy app's FCD to cob_file */
	}

	cob_pre_open (f);

	if (COB_FILE_STDIN (f)) {
		if (mode != COB_OPEN_INPUT) {
			cob_file_save_status (f, fnstatus, COB_STATUS_30_PERMANENT_ERROR);
			return;
		}
		f->file = stdin;
		f->fd = fileno (stdin);
		f->open_mode = (unsigned char)mode;
		cob_file_save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
		return;
	}
	if (COB_FILE_STDOUT (f)) {
		if (mode != COB_OPEN_OUTPUT) {
			cob_file_save_status (f, fnstatus, COB_STATUS_30_PERMANENT_ERROR);
			return;
		}
		f->file = stdout;
		f->fd = fileno (stdout);
		f->open_mode = (unsigned char)mode;
		cob_file_save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
		return;
	}

	if (f->assign == NULL) {
		cob_runtime_error (_("ERROR FILE %s has ASSIGN field is NULL"),
							f->select_name);
		cob_file_save_status (f, fnstatus, COB_STATUS_31_INCONSISTENT_FILENAME);
		return;
	}
	if (f->assign->data == NULL) {
		cob_file_save_status (f, fnstatus, COB_STATUS_31_INCONSISTENT_FILENAME);
		return;
	}

	if (file_open_name[0] == 0) {
		cob_file_save_status (f, fnstatus, COB_STATUS_31_INCONSISTENT_FILENAME);
		return;
	}

	if (!io_rtns[f->io_routine].loaded) {
		int ret = cob_load_module (f->io_routine);
		if (ret) {
			output_io_module_error (f->io_routine);
			cob_file_save_status (f, fnstatus, COB_STATUS_91_NOT_AVAILABLE);
		}
	}
	cob_cache_file (f);

	f->flag_is_concat = 0;
	if (file_setptr->cob_concat_name
	 && (f->organization == COB_ORG_SEQUENTIAL
	  || f->organization == COB_ORG_LINE_SEQUENTIAL) 
	 && (mode == COB_OPEN_INPUT 
	  || mode == COB_OPEN_I_O)
	 && (cp = strchr(file_open_name,file_setptr->cob_concat_sep[0])) != NULL
	 && file_open_name[0] != '>'
	 && file_open_name[0] != '<'
	 && file_open_name[0] != '|') {
		f->flag_is_concat = 1;
		f->org_filename = cob_strdup (file_open_name);
		f->nxt_filename = strchr(f->org_filename,file_setptr->cob_concat_sep[0]);
		*f->nxt_filename++ = 0;
		file_open_name = f->org_filename;
	}

	if (!f->flag_optional
	 && !isdirvalid(file_open_name)) {
		if (errno == ENOTDIR) {
			cob_file_save_status (f, fnstatus, COB_XSTATUS_NOT_DIR);
			return;
		}
		if (errno == ENOENT) {
			if ((mode != COB_OPEN_OUTPUT && mode != COB_OPEN_EXTEND) 
			 || f->organization == COB_ORG_INDEXED) 
				cob_file_save_status (f, fnstatus, COB_XSTATUS_NOT_FILE);
			else
				cob_file_save_status (f, fnstatus, COB_XSTATUS_NOT_DIR);
			return;
		}
	}

	/* Open the file */
	cob_file_save_status (f, fnstatus,
		     fileio_funcs[get_io_ptr (f)]->open (&file_api, f, file_open_name,
								mode, sharing));
	if (f->file_status[0] == '0'
	 && !f->flag_io_tran
	 && f->flag_do_qbl ) {
		set_qbl_buf (f->record_max);
		if (qblfd == -1) {
			cob_temp_name (qblfilename, ".qbl");
			qblfd = cob_open_qbl (qblfilename,1,0);
		}
	}
}

void
cob_close (cob_file *f, cob_field *fnstatus, const int opt, const int remfil)
{
	int			ret;

	f->last_operation = COB_LAST_CLOSE;
	f->flag_read_done = 0;
	f->record_off = 0;

	if (COB_FILE_SPECIAL (f)) {
		f->open_mode = COB_OPEN_CLOSED;
		f->file = NULL;
		f->fd = -1;
		if (f->flag_is_std) {
			f->flag_is_pipe = 0;
			f->flag_is_std = 0;
			f->file = f->fileout = NULL;
			f->fd = f->fdout = -1;
			f->file_pid = 0;
			f->flag_select_features &= ~COB_SELECT_STDIN;
			f->flag_select_features &= ~COB_SELECT_STDOUT;
		}
		cob_file_save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
		return;
	}

	if (remfil) {	/* Remove from cache - Needed for CANCEL */
		cob_cache_del (f);
	}

	if (f->open_mode == COB_OPEN_CLOSED) {
		cob_file_save_status (f, fnstatus, COB_STATUS_42_NOT_OPEN);
		return;
	}

	if ((f->lock_mode & COB_LOCK_ROLLBACK)
	 && f->flag_was_updated) {
		if (f->tran_open_mode == COB_OPEN_CLOSED)
			f->tran_open_mode = f->last_open_mode;
		f->flag_close_pend = 1;
		f->open_mode = COB_OPEN_CLOSED;
		cob_file_save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
		return;
	}

	if (f->flag_nonexistent) {
		ret = COB_STATUS_00_SUCCESS;
	} else {
		ret = fileio_funcs[get_io_ptr (f)]->close (&file_api, f, opt);
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

	cob_file_save_status (f, fnstatus, ret);
	f->flag_file_map = 0;
	f->flag_nonexistent = 0;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
	f->flag_first_read = 2;
	f->max_rec_num = 0;
	f->cur_rec_num = 0;
}

void
cob_start (cob_file *f, const int cond, cob_field *key, 
						cob_field *keysize, cob_field *fnstatus)
{
	int		ret;
	cob_field	tempkey;

	f->last_operation = COB_LAST_START;
	f->last_key = key;
	f->flag_read_done = 0;
	f->flag_first_read = 0;

	if (f->open_mode != COB_OPEN_I_O
	 && f->open_mode != COB_OPEN_INPUT) {
		cob_file_save_status (f, fnstatus, COB_STATUS_47_INPUT_DENIED);
		return;
	}

	if (f->access_mode == COB_ACCESS_RANDOM) {
		cob_file_save_status (f, fnstatus, COB_STATUS_47_INPUT_DENIED);
		return;
	}

	if (f->flag_nonexistent) {
		cob_file_save_status (f, fnstatus, COB_STATUS_23_KEY_NOT_EXISTS);
		return;
	}

	if (keysize) {
		int		size = cob_get_int (keysize);
		if (size < 1 || size > (int)key->size) {
			cob_file_save_status (f, fnstatus, COB_STATUS_23_KEY_NOT_EXISTS);
			return;
		}
		tempkey = *key;
		tempkey.size = (size_t)size;
		f->last_key = &tempkey;
		ret = fileio_funcs[get_io_ptr (f)]->start (&file_api, f, cond, &tempkey);
	} else {
		ret = fileio_funcs[get_io_ptr (f)]->start (&file_api, f, cond, key);
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

	cob_file_save_status (f, fnstatus, ret);
}

void
cob_read (cob_file *f, cob_field *key, cob_field *fnstatus, const int read_opts)
{
	int	ret;

	f->flag_read_done = 0;
	f->last_operation = COB_LAST_READ;
	f->last_key = key;

	if (f->open_mode != COB_OPEN_INPUT
	 && f->open_mode != COB_OPEN_I_O) {
		cob_file_save_status (f, fnstatus, COB_STATUS_47_INPUT_DENIED);
		return;
	}

	if (f->flag_nonexistent) {
		if (f->flag_first_read == 0) {
			cob_file_save_status (f, fnstatus, COB_STATUS_23_KEY_NOT_EXISTS);
			return;
		}
		f->flag_first_read = 0;
		cob_file_save_status (f, fnstatus, COB_STATUS_10_END_OF_FILE);
		return;
	}

	if (f->organization == COB_ORG_RELATIVE) {
		if (f->access_mode == COB_ACCESS_SEQUENTIAL) {
			if (f->cur_rec_num < 1)
				f->cur_rec_num = 1;
			else
				f->cur_rec_num++;
			cob_set_int (f->keys[0].field, (int)f->cur_rec_num);
		} else {
			f->cur_rec_num = cob_get_int (f->keys[0].field);
		}
	}
	/* Sequential read at the end of file is an error */
	if (key == NULL) {
		f->last_operation = COB_LAST_READ_SEQ;
		if (f->flag_end_of_file 
		 && !(read_opts & COB_READ_PREVIOUS)) {
			cob_file_save_status (f, fnstatus, COB_STATUS_46_READ_ERROR);
			return;
		}
		if (f->flag_begin_of_file
		 && (read_opts & COB_READ_PREVIOUS)) {
			cob_file_save_status (f, fnstatus, COB_STATUS_46_READ_ERROR);
			return;
		}
		ret = fileio_funcs[get_io_ptr (f)]->read_next (&file_api, f, read_opts);
	} else {
		ret = fileio_funcs[get_io_ptr (f)]->read (&file_api, f, key, read_opts);
	}

	switch (ret) {
	case COB_STATUS_00_SUCCESS:
	case COB_STATUS_02_SUCCESS_DUPLICATE:
	case COB_STATUS_06_READ_TRUNCATE:
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

	cob_file_save_status (f, fnstatus, ret);
}

static int
is_suppressed_key_value (cob_file *f, const int idx)
{
	if (idx < 0 || idx >= (int)f->nkeys) {
		return -1;
	}
	if (f->keys[idx].len_suppress > 0) {
		(void) cob_savekey (f, idx, f->keys[idx].field->data);
		if (memcmp (f->keys[idx].field->data,
			        f->keys[idx].str_suppress,
			        f->keys[idx].len_suppress) == 0) {
			return 1;
		}
	} else
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

static int
cob_chk_dups (cob_file *f)
{
	int		k;
	int		ret = COB_STATUS_00_SUCCESS;

	set_qbl_buf (f->record_max);
	memcpy (qbl_tmp, f->record->data, f->record->size);

	for (k = 0; k < (int)f->nkeys; ++k) {
		if (f->keys[k].tf_duplicates == 2) {
			memcpy (f->record->data, qbl_tmp, f->record->size);
			if (is_suppressed_key_value (f, k) > 0) {
				continue;
			}
			ret = fileio_funcs[get_io_ptr (f)]->read (&file_api, f, f->keys[k].field, 0);
			if (ret == COB_STATUS_00_SUCCESS
			 || ret == COB_STATUS_02_SUCCESS_DUPLICATE) {
				ret = COB_STATUS_22_KEY_EXISTS;
				break;
			}
			ret = COB_STATUS_00_SUCCESS;
		}
	}

	memcpy (f->record->data, qbl_tmp, f->record->size);
	return ret;
}

void
cob_read_next (cob_file *f, cob_field *fnstatus, const int read_opts)
{
	int	ret,idx;

	f->last_operation = COB_LAST_READ_SEQ;
	f->flag_read_done = 0;

	if (f->open_mode != COB_OPEN_INPUT
	 && f->open_mode != COB_OPEN_I_O) {
		cob_file_save_status (f, fnstatus, COB_STATUS_47_INPUT_DENIED);
		return;
	}

	if (f->flag_nonexistent) {
		if (f->flag_first_read == 0) {
			cob_file_save_status (f, fnstatus, COB_STATUS_46_READ_ERROR);
			return;
		}
		f->flag_first_read = 0;
		cob_file_save_status (f, fnstatus, COB_STATUS_10_END_OF_FILE);
		return;
	}

	/* Sequential read at the end of file is an error */
	if (f->flag_end_of_file && !(read_opts & COB_READ_PREVIOUS)) {
		cob_file_save_status (f, fnstatus, COB_STATUS_46_READ_ERROR);
		return;
	}
	if (f->flag_begin_of_file && (read_opts & COB_READ_PREVIOUS)) {
		cob_file_save_status (f, fnstatus, COB_STATUS_46_READ_ERROR);
		return;
	}

Again:
	if (f->organization == COB_ORG_RELATIVE) {
		if (f->access_mode == COB_ACCESS_SEQUENTIAL) {
			if (read_opts & COB_READ_PREVIOUS) {
				if (f->cur_rec_num < 1)
					f->cur_rec_num = 1;
				f->cur_rec_num--;
			} else {
				if (f->cur_rec_num < 1)
					f->cur_rec_num = 1;
				else
					f->cur_rec_num++;
			}
			cob_set_int (f->keys[0].field, (int)f->cur_rec_num);
		} else {
			f->cur_rec_num = cob_get_int (f->keys[0].field);
		}
	}
	ret = fileio_funcs[get_io_ptr (f)]->read_next (&file_api, f, read_opts);

	switch (ret) {
	case COB_STATUS_00_SUCCESS:
	case COB_STATUS_02_SUCCESS_DUPLICATE:
	case COB_STATUS_06_READ_TRUNCATE:
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
	case COB_STATUS_49_I_O_DENIED:
		f->open_mode = COB_OPEN_CLOSED;
		break;
	}

	cob_file_save_status (f, fnstatus, ret);
}

void
cob_write (cob_file *f, cob_field *rec, const int opt, cob_field *fnstatus,
	   const unsigned int check_eop)
{
	int		ret;

	f->last_operation = COB_LAST_WRITE;
	f->last_key = NULL;
	f->flag_read_done = 0;

	if (f->flag_is_pipe) {
		if (f->open_mode == COB_OPEN_INPUT) {
			cob_file_save_status (f, fnstatus, COB_STATUS_48_OUTPUT_DENIED);
			return;
		}
	} else
	if (f->access_mode == COB_ACCESS_SEQUENTIAL) {
		if (f->open_mode != COB_OPEN_OUTPUT
		 && f->open_mode != COB_OPEN_EXTEND) {
			cob_file_save_status (f, fnstatus, COB_STATUS_48_OUTPUT_DENIED);
			return;
		}
		f->cur_rec_num++;
	} else {
		if (f->open_mode != COB_OPEN_OUTPUT
		 && f->open_mode != COB_OPEN_I_O) {
			cob_file_save_status (f, fnstatus, COB_STATUS_48_OUTPUT_DENIED);
			return;
		}
	}

	if (f->variable_record) {
		f->record->size = (size_t)cob_get_int (f->variable_record);
		if (f->record->size > rec->size) {
			f->record->size = rec->size;
		}
	} else if (f->flag_redef) {
		f->record->size = f->record_max;
	} else {
		f->record->size = rec->size;
	}

	if (f->record->size < f->record_min || f->record_max < f->record->size) {
		cob_file_save_status (f, fnstatus, COB_STATUS_44_RECORD_OVERFLOW);
		return;
	}

	if (f->flag_write_chk_dups) {
		if ((ret = cob_chk_dups (f)) != 0) {
			cob_file_save_status (f, fnstatus, ret);
			return;
		}
	}

	if (f->organization == COB_ORG_RELATIVE) {
		if (f->access_mode == COB_ACCESS_SEQUENTIAL 
		 && (f->open_mode == COB_OPEN_OUTPUT 
		  || f->open_mode == COB_OPEN_EXTEND)) {
			f->cur_rec_num = f->max_rec_num + 1;
			if (f->cur_rec_num < 1)
				f->cur_rec_num = 1;
			cob_set_int (f->keys[0].field, (int)f->cur_rec_num);
		} else {
			f->cur_rec_num = cob_get_int (f->keys[0].field);
		}
	}
	check_eop_status = check_eop;
	f->flag_was_updated = 1;
	cob_file_save_status (f, fnstatus,
		     fileio_funcs[get_io_ptr (f)]->write (&file_api, f, opt));
	if (f->cur_rec_num > f->max_rec_num
	 && f->file_status[0] == '0')
		f->max_rec_num = f->cur_rec_num;
	f->flag_begin_of_file = 0;

	if (f->file_status[0] == '0'
	 && f->flag_do_qbl) {
		cob_put_qbl (f, QBL_NEW);
	}
}

void
cob_rewrite (cob_file *f, cob_field *rec, const int opt, cob_field *fnstatus)
{
	int	read_done, ret;

	read_done = f->flag_read_done;
	f->flag_read_done = 0;
	f->last_operation = COB_LAST_REWRITE;
	f->last_key = NULL;

	if (!f->flag_do_rollback) {
		if (f->open_mode != COB_OPEN_I_O) {
			cob_file_save_status (f, fnstatus, COB_STATUS_49_I_O_DENIED);
			return;
		}

		if (f->access_mode == COB_ACCESS_SEQUENTIAL && !read_done) {
			cob_file_save_status (f, fnstatus, COB_STATUS_43_READ_NOT_DONE);
			return;
		}
	}

	if (f->organization == COB_ORG_SEQUENTIAL) {
		if (f->record->size != rec->size) {
			cob_file_save_status (f, fnstatus, COB_STATUS_44_RECORD_OVERFLOW);
			return;
		}

		if (f->variable_record) {
			if (f->record->size != (size_t)cob_get_int (f->variable_record)) {
				cob_file_save_status (f, fnstatus, COB_STATUS_44_RECORD_OVERFLOW);
				return;
			}
		}
	}

	if (f->variable_record) {
		f->record->size = (size_t)cob_get_int (f->variable_record);
		if (f->record->size > rec->size) {
			f->record->size = rec->size;
		}
		if (f->record->size < f->record_min || f->record_max < f->record->size) {
			cob_file_save_status (f, fnstatus, COB_STATUS_44_RECORD_OVERFLOW);
			return;
		}
	} else if (f->flag_redef) {
		f->record->size = f->record_max;
	} else {
		f->record->size = rec->size;
	}

	if (f->flag_write_chk_dups) {
		if ((ret = cob_chk_dups (f)) != 0) {
			cob_file_save_status (f, fnstatus, ret);
			return;
		}
	}
	f->flag_was_updated = 1;
	if (f->flag_do_qbl
	 && !f->flag_io_tran
	 && (f->organization == COB_ORG_INDEXED
	  || f->organization == COB_ORG_RELATIVE)) {
		memcpy (qbl_tmp, f->record->data, f->record_max);
		ret = fileio_funcs[get_io_ptr (f)]->read (&file_api, f, f->keys[0].field, 0);
		if (ret == 0) 
			cob_put_qbl (f, QBL_BEFORE);
		memcpy (f->record->data, qbl_tmp, f->record_max);
	}

	cob_file_save_status (f, fnstatus,
		     fileio_funcs[get_io_ptr (f)]->rewrite (&file_api, f, opt));

	if (f->file_status[0] == '0'
	 && f->flag_do_qbl
	 && f->organization == COB_ORG_SEQUENTIAL) {
		cob_put_qbl (f, QBL_BEFORE);
	}
}

void
cob_delete (cob_file *f, cob_field *fnstatus)
{
	int	read_done;

	read_done = f->flag_read_done;
	f->flag_read_done = 0;
	f->last_operation = COB_LAST_DELETE;

	if (!f->flag_do_rollback) {
		if (f->open_mode != COB_OPEN_I_O) {
			cob_file_save_status (f, fnstatus, COB_STATUS_49_I_O_DENIED);
			return;
		}

		if (f->access_mode == COB_ACCESS_SEQUENTIAL && !read_done) {
			cob_file_save_status (f, fnstatus, COB_STATUS_43_READ_NOT_DONE);
			return;
		}
		if (f->flag_do_qbl
		 && !f->flag_io_tran
		 && (f->organization == COB_ORG_INDEXED
		  || f->organization == COB_ORG_RELATIVE)) {
			set_qbl_buf (f->record_max);
			memcpy (qbl_tmp, f->record->data, f->record_max);
			if (fileio_funcs[get_io_ptr (f)]->read (&file_api, f, f->keys[0].field, 0) == 0)
				cob_put_qbl (f, QBL_DELETE);
			memcpy (f->record->data, qbl_tmp, f->record_max);
		}
	}

	f->flag_was_updated = 1;
	cob_file_save_status (f, fnstatus,
		     fileio_funcs[get_io_ptr (f)]->recdelete (&file_api, f));
}

void
cob_commit (void)
{
	struct file_list	*l;
	cob_file	*f;

	for (l = file_cache; l; l = l->next) {
		if (l->file == NULL)
			continue;
		f = l->file;
		if (f->flag_was_updated) {
			if (f->flag_io_tran) {
				f->last_operation = COB_LAST_COMMIT;
				fileio_funcs[get_io_ptr (f)]->commit (&file_api, f);
			} else if (f->flag_was_updated) {
				f->last_operation = COB_LAST_COMMIT;
				fileio_funcs[get_io_ptr (f)]->iosync (&file_api, f);
			}
			f->flag_was_updated = 0;
		}
		if (f->flag_close_pend) {	/* Close was pending commit/rollback */
			f->flag_close_pend = 0;
			if (f->tran_open_mode != COB_OPEN_CLOSED
			 && f->open_mode == COB_OPEN_CLOSED) {
				f->open_mode = f->tran_open_mode;
				cob_close (f, NULL, COB_CLOSE_NORMAL, 0);
			}
		}
		f->tran_open_mode = COB_OPEN_CLOSED;
	}
	if (qblfd != -1) {
		if (ftruncate (qblfd, 0)) {
			cob_runtime_error (_("I/O error doing COMMIT"));
			cob_close_qbl ( qblfd, qblfilename, 1);
			qblfd = -1;
		}
	}
}

void
cob_rollback (void)
{
	struct file_list	*l;
	off_t	qblpos = -1;
	cob_file	*f;
	int			reclen, svreclen, dorollback = 0;
	cob_u32_t	recnum, svrecnum;
	off_t		filepos = 0;
	off_t		svfileoff = 0;
	struct cob_qbl_hdr hdr[1];

	for (l = file_cache; l; l = l->next) {
		if (l->file == NULL)
			continue;
		f = l->file;
		if (f->flag_io_tran
		 && f->flag_was_updated) {
			f->last_operation = COB_LAST_ROLLBACK;
			fileio_funcs[get_io_ptr (f)]->rollback (&file_api, f);
			f->flag_was_updated = 0;
		}
		if (f->open_mode == COB_OPEN_CLOSED		/* Close pending commit/rollback */
		 && f->flag_close_pend) {	
			if (f->tran_open_mode != COB_OPEN_CLOSED) {	/* Mark OPEN I-O for rollback */
				f->open_mode = COB_OPEN_I_O;
			}
		}
		if (f->flag_was_updated
		 && f->flag_do_qbl
		 && qblfd != -1) {
			dorollback = 1;
		}
		f->flag_was_updated = 0;
	}
	memset(hdr,0,SZ_QBLHDR);
	svrecnum = 0;
	while (dorollback) {
		qblpos = cob_read_qbl (qblfd, &reclen, qblpos, -1);
		if (qblpos == -1)
			break;
		for (l = file_cache; l; l = l->next) {
			if (l->file == NULL)
				continue;
			f = l->file;
			strncpy(hdr->name, f->select_name, sizeof(hdr->name)-1);
			if (strcmp(hdr->name, qbl_hdr->name) == 0) {
				f->flag_do_qbl = 0;
				f->flag_do_rollback = 1;
				svreclen = f->record->size;
				svfileoff = f->record_off;
				memcpy (qbl_tmp, f->record->data, f->record_max);
				f->record->size = reclen;
				if (reclen > 0)
					memcpy (f->record->data, qbl_hdr->data, reclen);
				if (f->organization == COB_ORG_INDEXED) {
					if (memcmp(qbl_hdr->type, QBL_NEW, 2) == 0) {
						cob_delete (f, NULL);
					} else if (memcmp(qbl_hdr->type, QBL_BEFORE, 2) == 0) {
						cob_rewrite (f, f->record, 0, NULL);
					} else if (memcmp(qbl_hdr->type, QBL_DELETE, 2) == 0) {
						cob_write (f, f->record, 0, NULL, 0);
					}
				} else
				if (f->organization == COB_ORG_RELATIVE) {
					svrecnum = (cob_u32_t)cob_get_int (f->keys[0].field);
					recnum = (cob_u32_t)cob_get_u64_compx (qbl_hdr->recnum, 4);
					filepos = cob_get_u64_compx (qbl_hdr->filepos, 8);
					cob_set_int (f->keys[0].field, recnum);
					if (memcmp(qbl_hdr->type, QBL_NEW, 2) == 0) {
						cob_delete (f, NULL);
					} else if (memcmp(qbl_hdr->type, QBL_BEFORE, 2) == 0) {
						cob_rewrite (f, f->record, 0, NULL);
					} else if (memcmp(qbl_hdr->type, QBL_DELETE, 2) == 0) {
						cob_write (f, f->record, 0, NULL, 0);
					}
					cob_set_int (f->keys[0].field, svrecnum);
					f->record_off = svfileoff;
				} else
				if (f->organization == COB_ORG_SEQUENTIAL) {
					filepos = cob_get_u64_compx (qbl_hdr->filepos, 8);
					f->record_off = filepos;
					f->flag_read_done = 1;
					if (memcmp(qbl_hdr->type, QBL_NEW, 2) == 0) {
						if (ftruncate (f->fd, (off_t)filepos)) {
							cob_runtime_error (_("I/O error doing ROLLBACK of %s"),
													f->select_name);
						}
					} else if (memcmp(qbl_hdr->type, QBL_BEFORE, 2) == 0) {
						cob_rewrite (f, f->record, 0, NULL);
					}
				} else
				if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
					filepos = cob_get_u64_compx (qbl_hdr->filepos, 8);
					f->record_off = filepos;
					f->flag_read_done = 1;
					if (memcmp(qbl_hdr->type, QBL_NEW, 2) == 0) {
						if (ftruncate (f->fd, (off_t)filepos)) {
							cob_runtime_error (_("I/O error doing ROLLBACK of %s"),
													f->select_name);
						}
					} else if (memcmp(qbl_hdr->type, QBL_BEFORE, 2) == 0) {
						set_file_pos (f, filepos);
						lineseq_read (NULL, f, COB_READ_NEXT);
						if (reclen > 0) {
							memcpy (f->record->data, qbl_hdr->data, reclen);
						}
						f->record->size = reclen;
						cob_rewrite (f, f->record, 0, NULL);
					}
				}
				memcpy (f->record->data, qbl_tmp, f->record_max);
				f->record->size = svreclen;
				f->flag_do_qbl = 1;
				f->flag_do_rollback = 0;
				break;
			}
		}
	}
	for (l = file_cache; l; l = l->next) {
		if (l->file == NULL)
			continue;
		f = l->file;
		if (f->open_mode == COB_OPEN_CLOSED		/* Close pending commit/rollback */
		 && f->flag_close_pend) {	
			cob_open (f, f->tran_open_mode, 0, NULL);
			f->tran_open_mode = COB_OPEN_CLOSED;
		}
		f->flag_close_pend = 0;
		f->flag_was_updated = 0;
	}

	if (dorollback
	 && qblfd != -1) {
		if (ftruncate (qblfd, 0)) {
			cob_runtime_error (_("I/O error doing ROLLBACK"));
			cob_close_qbl ( qblfd, qblfilename, 1);
			qblfd = -1;
		}
	}
}

void
cob_delete_file (cob_file *f, cob_field *fnstatus, const int override)
{
	COB_UNUSED (override);
	f->last_operation = COB_LAST_DELETE_FILE;
	if (f->organization == COB_ORG_SORT) {
		cob_file_save_status (f, fnstatus, COB_STATUS_30_PERMANENT_ERROR);
		return;
	}

	/* File was previously closed with lock */
	if (f->open_mode == COB_OPEN_LOCKED) {
		cob_file_save_status (f, fnstatus, COB_STATUS_38_CLOSED_WITH_LOCK);
		return;
	}

	/* File is open */
	if (f->open_mode != COB_OPEN_CLOSED) {
		cob_file_save_status (f, fnstatus, COB_STATUS_41_ALREADY_OPEN);
		return;
	}

	if (COB_FILE_STDIN (f) || COB_FILE_STDOUT (f)) {
		cob_file_save_status (f, fnstatus, COB_STATUS_30_PERMANENT_ERROR);
		return;
	}

	if (!io_rtns[f->io_routine].loaded) {
		int ret = cob_load_module (f->io_routine);
		if (ret) {
			output_io_module_error (f->io_routine);
			cob_file_save_status (f, fnstatus, COB_STATUS_91_NOT_AVAILABLE);
		}
	}

	if ((f->lock_mode & COB_LOCK_ROLLBACK)
	 && f->flag_was_updated) {	/* FIXME: Should have done a COMMIT before this */
		cob_file_save_status (f, fnstatus, COB_STATUS_37_PERMISSION_DENIED);
		return;
	}

	/* Obtain the file name */
	cob_field_to_string (f->assign, file_open_name, (size_t)COB_FILE_MAX);
	cob_chk_file_mapping (f, NULL);

	errno = 0;
	delete_file_status = 1;
	if (f->organization != COB_ORG_INDEXED) {
		unlink (file_open_name);
	} else {
		cob_file_save_status (f, fnstatus,
			fileio_funcs[get_io_ptr (f)]->fildelete (&file_api, f, file_open_name));
		delete_file_status = 0;
		return;
	}
	if (errno == ENOENT) {
		cob_file_save_status (f, fnstatus, 5);
		if (COB_MODULE_PTR
		 && COB_MODULE_PTR->flag_dialect == COB_DIALECT_MF) {
			f->file_status[0] = '9';
			f->file_status[1] = 13;
			if (fnstatus) {
				memcpy (fnstatus->data, f->file_status, (size_t)2);
			}
		}
	} else {
		cob_file_save_status (f, fnstatus, errno_cob_sts(COB_STATUS_00_SUCCESS));
	}
	delete_file_status = 0;
}

/* Return index number for given key */
int
cob_findkey (cob_file *f, cob_field *kf, int *fullkeylen, int *partlen)
{
	int 	k,part;
	*fullkeylen = *partlen = 0;

	for (k = 0; k < (int)f->nkeys; ++k) {
		if (f->keys[k].field
		 && f->keys[k].count_components <= 1
		 && f->keys[k].field->data == kf->data) {
			f->last_key = f->keys[k].field;
			*fullkeylen = f->keys[k].field->size;
			*partlen = kf->size;
			return k;
		}
	}
	for (k = 0; k < (int)f->nkeys; ++k) {
		if (f->keys[k].count_components > 1) {
			if ((f->keys[k].field
			 &&  f->keys[k].field->data == kf->data
			 &&  f->keys[k].field->size == kf->size)
			 || (f->keys[k].component[0]->data == kf->data)) {
				f->last_key = f->keys[k].field;
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
cob_param_no_quotes (int n)
{
	int		i, j;
	char	*s;

	s = cob_get_picx_param (n, NULL, 0);
	if (s == NULL)
		return NULL;
	for (i = j = 0; s[j] != 0; j++) {
		if (s[j] == '"') {
			continue;
		}
		s[i++] = s[j];
	}
	s[i] = 0;
	return (void*)s;
}

static int
open_cbl_file (cob_u8_ptr file_name, int file_access,
	       	cob_u8_ptr file_handle, const int file_flags)
{
	char	*fn;
	int	flag = O_BINARY;
	int	fd;

	COB_UNUSED (file_name);

	fn = cob_param_no_quotes (1);
	if (fn == NULL) {
		memset (file_handle, -1, (size_t)4);
		return -1;
	}
	flag |= file_flags;
	switch (file_access & 0x3F) {
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
			cob_runtime_warning (_("call to CBL_OPEN_FILE with wrong access mode: %d"), file_access & 0x3F);
			memset (file_handle, -1, (size_t)4);
			return -1;
	}
	fd = open (fn, flag, COB_FILE_MODE);
	if (fd < 0) {
		cob_free (fn);
		memset (file_handle, -1, (size_t)4);
		return 35;
	}
	cob_free (fn);
	memcpy (file_handle, &fd, (size_t)4);
	return 0;
}

int
cob_sys_open_file (unsigned char *file_name, unsigned char *file_access,
		   unsigned char *file_lock, unsigned char *file_dev,
		   unsigned char *file_handle)
{
	COB_UNUSED (file_access);
	COB_UNUSED (file_lock);
	COB_UNUSED (file_dev);

	COB_CHK_PARMS (CBL_OPEN_FILE, 5);

	return open_cbl_file (file_name, (int)cob_get_s64_param (2), file_handle, 0);
}

int
cob_sys_create_file (unsigned char *file_name, unsigned char *file_access,
		     unsigned char *file_lock, unsigned char *file_dev,
		     unsigned char *file_handle)
{
	int		p_lock, p_dev;	
	COB_UNUSED (file_access);
	COB_UNUSED (file_lock);
	COB_UNUSED (file_dev);
	/*
	 * @param: file_access : 1 (read-only), 2 (write-only), 3 (both)
	 * @param: file_lock : not implemented, set 0
	 * @param: file_dev : not implemented, set 0
	 */
	p_lock = (int)cob_get_s64_param (3);
	p_dev  = (int)cob_get_s64_param (4);

	COB_CHK_PARMS (CBL_CREATE_FILE, 5);

	if (p_lock != 0) {
		cob_runtime_warning (_("call to CBL_CREATE_FILE with wrong file_lock: %d"), p_lock);
	}
	if (p_dev != 0) {
		cob_runtime_warning (_("call to CBL_CREATE_FILE with wrong file_dev: %d"), p_dev);
	}

	return open_cbl_file (file_name, (int)cob_get_s64_param (2), file_handle, O_CREAT | O_TRUNC);
}

int
cob_sys_read_file (unsigned char *file_handle, unsigned char *file_offset,
		   unsigned char *file_len, unsigned char *flags,
		   unsigned char *buf)
{
	cob_s64_t	off;
	int		fd;
	size_t	len;
	int		rc;
	struct stat	st;
	cob_u8_ptr	p_flags;

	COB_CHK_PARMS (CBL_READ_FILE, 5);

	memcpy (&fd, file_handle, (size_t)4);
	COB_UNUSED (file_offset);
	COB_UNUSED (file_len);
	COB_UNUSED (flags);
	off = cob_get_s64_param (2);
	len = (size_t)cob_get_s64_param (3);
	p_flags = cob_get_param_data (4);

	if (lseek (fd, (off_t)off, SEEK_SET) == -1) {
		return -1;
	}
	if (len > 0) {
		rc = read (fd, buf, len);
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
	if ((*p_flags & 0x80) != 0) {
		if (fstat (fd, &st) < 0) {
			return -1;
		}
		cob_put_s64_param ( 2, (cob_s64_t) st.st_size);
	}
	return rc;
}

int
cob_sys_write_file (unsigned char *file_handle, unsigned char *file_offset,
		    unsigned char *file_len, unsigned char *flags,
		    unsigned char *buf)
{
	cob_s64_t	off;
	int		fd;
	size_t	len;
	int		rc;

	COB_UNUSED (flags);
	COB_UNUSED (file_len);
	COB_UNUSED (file_offset);

	COB_CHK_PARMS (CBL_WRITE_FILE, 5);

	memcpy (&fd, file_handle, (size_t)4);
	off = cob_get_s64_param (2);
	len = (size_t)cob_get_s64_param (3);
	if (lseek (fd, (off_t)off, SEEK_SET) == -1) {
		return -1;
	}
	rc = (int) write (fd, buf, (size_t)len);
	if (rc != (int)len) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	return COB_STATUS_00_SUCCESS;
}

int
cob_sys_close_file (unsigned char *file_handle)
{
	int	fd;

	COB_CHK_PARMS (CBL_CLOSE_FILE, 1);

	memcpy (&fd, file_handle, (size_t)4);
	return close (fd);
}

int
cob_sys_flush_file (unsigned char *file_handle)
{
	COB_UNUSED (file_handle);

	COB_CHK_PARMS (CBL_FLUSH_FILE, 1);

	return 0;
}

int
cob_sys_delete_file (unsigned char *file_name)
{
	char	*fn;
	int	ret;

	COB_UNUSED (file_name);

	COB_CHK_PARMS (CBL_DELETE_FILE, 1);

	fn = cob_param_no_quotes (1);
	if (fn == NULL) {
		return -1;
	}
	ret = unlink (fn);
	cob_free (fn);
	if (ret) {
		return 128;
	}
	return 0;
}

int
cob_sys_copy_file (unsigned char *fname1, unsigned char *fname2)
{
	char	*fn1;
	char	*fn2;
	int	flag = O_BINARY;
	int	ret;
	int	i;
	int	fd1, fd2;

	COB_UNUSED (fname1);
	COB_UNUSED (fname2);

	COB_CHK_PARMS (CBL_COPY_FILE, 2);

	fn1 = cob_param_no_quotes (1);
	if (fn1 == NULL) {
		return -1;
	}
	fn2 = cob_param_no_quotes (2);
	if (fn2 == NULL) {
		cob_free (fn1);
		return -1;
	}
	flag |= O_RDONLY;
	fd1 = open (fn1, flag, 0);
	if (fd1 < 0) {
		cob_free (fn1);
		cob_free (fn2);
		return -1;
	}
	flag &= ~O_RDONLY;
	flag |= O_CREAT | O_TRUNC | O_WRONLY;
	fd2 = open (fn2, flag, COB_FILE_MODE);
	if (fd2 < 0) {
		close (fd1);
		cob_free (fn1);
		cob_free (fn2);
		return -1;
	}

	ret = 0;
	while ((i = read (fd1, file_open_buff, COB_FILE_BUFF)) > 0) {
		if ((size_t)write (fd2, file_open_buff, (size_t)i) != (size_t)i) {
			ret = -1;
			break;
		}
	}
	close (fd1);
	close (fd2);
	cob_free (fn1);
	cob_free (fn2);
	return ret;
}

int
cob_sys_check_file_exist (unsigned char *file_name, unsigned char *file_info)
{
	char		*fn;
	struct tm	*tm;
	cob_s64_t	sz;
	struct stat	st;
	short		y;
	short		d, m, hh, mm, ss;

	COB_UNUSED (file_name);

	COB_CHK_PARMS (CBL_CHECK_FILE_EXIST, 2);

	fn = cob_param_no_quotes (1);
	if (fn == NULL) {
		return -1;
	}
	if (cob_get_param_size(2) < 16) {
		cob_runtime_error (_("'%s' - File detail area is too short"),
			"CBL_CHECK_FILE_EXIST");
		cob_stop_run (1);
	}

	if (stat (fn, &st) < 0) {
		cob_free (fn);
		return 35;
	}
	cob_free (fn);
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

int
cob_sys_rename_file (unsigned char *fname1, unsigned char *fname2)
{
	char	*fn1;
	char	*fn2;
	int	ret;

	COB_UNUSED (fname1);
	COB_UNUSED (fname2);

	COB_CHK_PARMS (CBL_RENAME_FILE, 2);

	fn1 = cob_param_no_quotes (1);
	if (fn1 == NULL) {
		return -1;
	}
	fn2 = cob_param_no_quotes (2);
	if (fn2 == NULL) {
		cob_free (fn1);
		return -1;
	}
	ret = rename (fn1, fn2);
	cob_free (fn1);
	cob_free (fn2);
	if (ret) {
		return 128;
	}
	return 0;
}

int
cob_sys_get_current_dir (const int p1, const int p2, unsigned char *p3)
{
	char	*dirname, *dir;
	int	dir_size, dir_length, flags;
	int	has_space;

	COB_UNUSED (p1);
	COB_UNUSED (p2);
	COB_UNUSED (p3);
	COB_CHK_PARMS (CBL_GET_CURRENT_DIR, 3);

	flags = (int)cob_get_s64_param (1);
	dir_length = (int)cob_get_s64_param (2);
	dir = cob_get_param_data (3);

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
	free (dirname);
	return 0;
}

int
cob_sys_create_dir (unsigned char *dir)
{
	char	*fn;
	int	ret;

	COB_UNUSED (dir);

	COB_CHK_PARMS (CBL_CREATE_DIR, 1);

	fn = cob_param_no_quotes (1);
	if (fn == NULL) {
		return -1;
	}
	ret = mkdir (fn, 0770);
	cob_free (fn);
	if (ret) {
		return 128;
	}
	return 0;
}

int
cob_sys_change_dir (unsigned char *dir)
{
	char	*fn;
	int	ret;

	COB_UNUSED (dir);

	COB_CHK_PARMS (CBL_CHANGE_DIR, 1);

	fn = cob_param_no_quotes (1);
	if (fn == NULL) {
		return -1;
	}
	ret = chdir (fn);
	cob_free (fn);
	if (ret) {
		return 128;
	}
	return 0;
}

int
cob_sys_delete_dir (unsigned char *dir)
{
	char	*fn;
	int	ret;

	COB_UNUSED (dir);

	COB_CHK_PARMS (CBL_DELETE_DIR, 1);

	fn = cob_param_no_quotes (1);
	if (fn == NULL) {
		return -1;
	}
	ret = rmdir (fn);
	cob_free (fn);
	if (ret) {
		return 128;
	}
	return 0;
}

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
	cob_put_s64_param (2, (cob_s64_t)ret);
	return ret;
}

int
cob_sys_copyfile (unsigned char *fname1, unsigned char *fname2,
		  unsigned char *file_type)
{
	int		ret;

	/* RXW - Type is not yet evaluated */
	COB_UNUSED (file_type);

	COB_CHK_PARMS (C$COPY, 3);

	if (cob_get_num_params () < 3) {
		return 128;
	}
	ret = cob_sys_copy_file (fname1, fname2);
	if (ret < 0) {
		ret = 128;
	}
	return ret;
}

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

	if (cob_get_num_params () < 2 ) {
		return 128;
	}
	fn = cob_param_no_quotes (1);
	if (fn == NULL) {
		return -1;
	}
	if (cob_get_param_size(2) < 16) {
		cob_runtime_error (_("'%s' - File detail area is too short"), "C$FILEINFO");
		cob_stop_run (1);
	}

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

int
cob_sys_file_delete (unsigned char *file_name, unsigned char *file_type)
{
	int	ret;
	char	*fn;

	/* RXW - Type is not yet evaluated */
	COB_UNUSED (file_type);

	COB_CHK_PARMS (C$DELETE, 2);
	if (cob_get_num_params () < 2 ) {
		return 128;
	}
	fn = cob_param_no_quotes (1);
	if (fn == NULL) {
		return -1;
	}

	ret = cob_sys_delete_file (file_name);
	if (ret < 0) {
		ret = 128;
	}
	cob_free (fn);
	return ret;
}

/* SORT */

static int
sort_cmps (const unsigned char *s1, const unsigned char *s2, const size_t size,
	   const unsigned char *col)
{
	size_t			i;
	int			ret;

	if (col) {
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
	int		i;
	size_t		u1;
	size_t		u2;
	int		cmp;
	cob_field	f1;
	cob_field	f2;

	f = pointer;
	for (i = 0; i < (int)f->nkeys; ++i) {
		f1 = f2 = *(f->keys[i].field);
		f1.data = k1->item + f->keys[i].offset;
		f2.data = k2->item + f->keys[i].offset;
		if (COB_FIELD_IS_NUMERIC (&f1)) {
			cmp = cob_numeric_cmp (&f1, &f2);
		} else {
			cmp = sort_cmps (f1.data, f2.data, f1.size,
					 f->sort_collating);
		}
		if (cmp != 0) {
			return (f->keys[i].tf_ascending == COB_ASCENDING) ? cmp : -cmp;
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
	if (hp->empty != NULL) {
		q = hp->empty;
		hp->empty = q->next;
		q->block_byte = 0;
		q->next = NULL;
		q->end_of_block = 0;
		return (void *)q;
	}
	if ((hp->mem_used + hp->alloc_size) > hp->mem_size) {
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
	if (hp->mem_total >= file_setptr->cob_sort_memory) {
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
	fd = open (filename,
		    O_CREAT | O_TRUNC | O_RDWR | O_BINARY | COB_OPEN_TEMPORARY,
		    COB_FILE_MODE);
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
		if (hp->file[n].fp == NULL) {
			cob_runtime_error (_("SORT is unable to acquire temporary file"));
			cob_stop_run (1);
		}
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
		if (fread (hp->queue[n].first->unique,
				hp->r_size, (size_t)1, fp) != 1) {
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
		if (fwrite (&(q->block_byte),
				hp->w_size, (size_t)1, fp) != 1) {
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
	if (putc (1, fp) != 1) {
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
	if (tosize > fromsize) {
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
	if (n < 0) {
		return COBSORTABORT;
	}
	/* LCOV_EXCL_STOP */
#endif
	if (!hp->files_used) {
		hp->retrieval_queue = n;
		return 0;
	}
	/* LCOV_EXCL_START */
	if (cob_write_block (hp, n)) {
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
	if (cob_get_sort_tempfile (hp, 2)) {
		return COBSORTFILEERR;
	}
	if (cob_get_sort_tempfile (hp, 3)) {
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
			if (cob_read_item (hp, source)) {
				return COBSORTFILEERR;
			}
			/* LCOV_EXCL_STOP */
			if (hp->file[source + 1].count > 0) {
				/* LCOV_EXCL_START */
				if (cob_read_item (hp, source + 1)) {
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
				if (fwrite (
				    &(hp->queue[source + move].first->block_byte),
				    hp->w_size, (size_t)1,
				    hp->file[destination].fp) != 1) {
					return COBSORTFILEERR;
				}
				if (cob_read_item (hp, source + move)) {
					return COBSORTFILEERR;
				}
				/* LCOV_EXCL_STOP */
			}
			hp->file[destination].count++;
			/* LCOV_EXCL_START */
			if (putc (1, hp->file[destination].fp) != 1) {
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
	if (cob_read_item (hp, source)) {
		return COBSORTFILEERR;
	}
	if (cob_read_item (hp, source + 1)) {
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
	if (!hp) {
		return COBSORTNOTOPEN;
	}
	if (hp->retrieving) {
		return COBSORTABORT;
	}
	if (hp->switch_to_file) {
		if (!hp->files_used) {
			/* LCOV_EXCL_START */
			if (cob_get_sort_tempfile (hp, 0)) {
				return COBSORTFILEERR;
			}
			if (cob_get_sort_tempfile (hp, 1)) {
				return COBSORTFILEERR;
			}
			/* LCOV_EXCL_STOP */
			hp->files_used = 1;
			hp->destination_file = 0;
		}
		n = cob_sort_queues (hp);
#if	0	/* RXWRXW - Cannot be true */
		/* LCOV_EXCL_START */
		if (n < 0) {
			return COBSORTABORT;
		}
		/* LCOV_EXCL_STOP */
#endif
		/* LCOV_EXCL_START */
		if (cob_write_block (hp, n)) {
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
	if (!hp) {
		return COBSORTNOTOPEN;
	}
	if (!hp->retrieving) {
		res = cob_file_sort_process (hp);
		if (res) {
			return res;
		}
	}
	if (hp->files_used) {
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
		if (cob_read_item (hp, source + move)) {
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
			if (COB_FILE_SPECIAL (fbase[i]) 
			 || fbase[i]->organization == COB_ORG_LINE_SEQUENTIAL) {
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
	p->chunk_size = file_setptr->cob_sort_chunk;
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
	} else if (COB_MODULE_PTR) {
		f->sort_collating = COB_MODULE_PTR->collating_sequence;
	}
	cob_file_save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
}

void
cob_file_sort_init_key (cob_file *f, cob_field *field, const int flag,
			const unsigned int offset)
{
	f->keys[f->nkeys].field = field;
	f->keys[f->nkeys].tf_ascending = (unsigned int)flag;
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
	if (hp) {
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
	f->fd = -1;
	cob_file_save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
}

void
cob_file_release (cob_file *f)
{
	struct cobsort	*hp;
	cob_field	*fnstatus;
	int		ret;

	fnstatus = NULL;
	hp = f->file;
	if (hp) {
		fnstatus = hp->fnstatus;
	}
	ret = cob_file_sort_submit (f, f->record->data);
	if (!ret) {
		cob_file_save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
		return;
	}
	if (hp && hp->sort_return) {
		*(int *)(hp->sort_return) = 16;
	}
	cob_file_save_status (f, fnstatus, COB_STATUS_30_PERMANENT_ERROR);
}

void
cob_file_return (cob_file *f)
{
	struct cobsort	*hp;
	cob_field	*fnstatus;
	int		ret;

	fnstatus = NULL;
	hp = f->file;
	if (hp) {
		fnstatus = hp->fnstatus;
	}
	ret = cob_file_sort_retrieve (f, f->record->data);
	switch (ret) {
	case 0:
		cob_file_save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
		return;
	case COBSORTEND:
		cob_file_save_status (f, fnstatus, COB_STATUS_10_END_OF_FILE);
		return;
	}
	if (hp && hp->sort_return) {
		*(int *)(hp->sort_return) = 16;
	}
	cob_file_save_status (f, fnstatus, COB_STATUS_30_PERMANENT_ERROR);
}

char *
cob_get_filename_print (cob_file* file, const int show_resolved_name)
{
	/* Obtain the file name */
	cob_field_to_string (file->assign, file_open_env, (size_t)COB_FILE_MAX);
	if (show_resolved_name) {
		strncpy (file_open_name, file_open_env, (size_t)COB_FILE_MAX);
		file_open_name[COB_FILE_MAX] = 0;
		cob_chk_file_mapping (file, NULL);
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
	int		k;

	if (qblfd != -1) {
		cob_close_qbl ( qblfd, qblfilename, 1);
		qblfd = -1;
	}
	for(k=0; k < COB_IO_MAX; k++) {
		if(fileio_funcs[k] != NULL) {
			fileio_funcs[k]->ioexit (&file_api);
		}
	}

	if (runtime_buffer) {
		cob_free (runtime_buffer);
		runtime_buffer = NULL;
	}
	if (qbl_hdr) {
		cob_cache_free (qbl_hdr);
		qbl_hdr = NULL;
	}
	if (qbl_tmp) {
		cob_cache_free (qbl_tmp);
		qbl_tmp = NULL;
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
	char	*p;
	int	i,k;

	runtime_buffer = cob_fast_malloc ((size_t)(4 * COB_FILE_BUFF) + 4);
	file_open_env = runtime_buffer + COB_FILE_BUFF;
	file_open_name = runtime_buffer + (2 * COB_FILE_BUFF);
	file_open_buff = runtime_buffer + (3 * COB_FILE_BUFF);

	file_api.glbptr = file_globptr = lptr;
	file_api.setptr = file_setptr  = sptr;
	file_api.add_file_cache = cob_cache_file;
	file_api.del_file_cache = cob_cache_del;
	file_api.cob_write_dict = cob_write_dict;
	file_api.cob_read_dict = cob_read_dict;
	file_api.io_funcs = fileio_funcs;
	file_api.chk_file_mapping = cob_chk_file_mapping;
	file_api.cob_file_write_opt = cob_file_write_opt;
	file_api.file_open_buff = file_open_buff;

	if(chk_file_path) {
		chk_file_path = 0;
		if (file_setptr->cob_file_path) {
			for(i=k=0; file_setptr->cob_file_path[i] != 0; i++) {
				if(file_setptr->cob_file_path[i] == PATHSEP_CHAR)
					k++;
			}
			/* Split list of paths apart */
			file_paths = cob_malloc (sizeof(void*) * (k+2));
			p = cob_strdup (file_setptr->cob_file_path);
			file_paths[0] = p;
			for(i=k=0; p[i] != 0; i++) {
				if(p[i] == PATHSEP_CHAR) {
					p[i] = 0;
					file_paths[++k] = &p[i+1];
				}
			}
			file_paths[++k] = NULL;
		}
	}
	file_api.file_paths = file_paths;

	file_cache = NULL;
	eop_status = 0;
	check_eop_status = 0;
	if (file_setptr->cob_sort_chunk > (file_setptr->cob_sort_memory / 2)) {
		file_setptr->cob_sort_chunk = file_setptr->cob_sort_memory / 2;
	}

	if(file_setptr->cob_file_format == COB_FILE_IS_MF) {	/* all MF format files */
		if(file_setptr->cob_varseq_type == COB_FILE_IS_GC
		|| file_setptr->cob_varseq_type == 0)
			file_setptr->cob_varseq_type = COB_FILE_IS_MF;
		if(file_setptr->cob_varrel_type == COB_FILE_IS_GC)
			file_setptr->cob_varrel_type = COB_FILE_IS_MF;
		if(file_setptr->cob_fixrel_type == COB_FILE_IS_GC)
			file_setptr->cob_fixrel_type = COB_FILE_IS_MF;
	} else
	if(file_setptr->cob_file_format == COB_FILE_IS_GC) {	/* all GC3 format files */
		if(file_setptr->cob_varseq_type == COB_FILE_IS_MF)
			file_setptr->cob_varseq_type = COB_FILE_IS_GC;
		if(file_setptr->cob_varrel_type == COB_FILE_IS_MF)
			file_setptr->cob_varrel_type = COB_FILE_IS_GC;
		if(file_setptr->cob_fixrel_type == COB_FILE_IS_MF)
			file_setptr->cob_fixrel_type = COB_FILE_IS_GC;
	}

#if defined(WITH_STATIC_ISAM)
	cob_isam_init_fileio (&file_api);
#if defined(WITH_INDEXED)
	io_rtns [WITH_INDEXED].loaded = 1;
	io_rtns [WITH_INDEXED].config = 1;
	/* V-ISAM can handle all of C|D|VB-ISAM format files */
#if (WITH_INDEXED == COB_IO_VISAM) 
#if !defined(WITH_CISAM)
	io_rtns [COB_IO_CISAM].loaded = 1;
	file_api.io_funcs[COB_IO_CISAM] = file_api.io_funcs[COB_IO_VISAM];
#endif
#if !defined(WITH_DISAM)
	io_rtns [COB_IO_DISAM].loaded = 1;
	file_api.io_funcs[COB_IO_DISAM] = file_api.io_funcs[COB_IO_VISAM];
#endif
#if !defined(WITH_VBISAM)
	io_rtns [COB_IO_VBISAM].loaded = 1;
	file_api.io_funcs[COB_IO_VBISAM] = file_api.io_funcs[COB_IO_VISAM];
#endif
#endif
#endif
#endif

#if defined(WITH_INDEX_EXTFH)
	cob_index_init_fileio (&file_api);
	io_rtns [COB_IO_IXEXT].loaded = 1;
	io_rtns [COB_IO_IXEXT].config = 1;
#endif

#if defined(WITH_SEQRA_EXTFH)
	cob_seqra_init_fileio (&file_api);
	io_rtns [COB_IO_SQEXT].loaded = 1;
	io_rtns [COB_IO_SQEXT].config = 1;
	io_rtns [COB_IO_RAEXT].loaded = 1;
	io_rtns [COB_IO_RAEXT].config = 1;
#endif
#if defined(WITH_CISAM)
	io_rtns [COB_IO_CISAM].config = 1;
#endif
#if defined(WITH_DISAM)
	io_rtns [COB_IO_DISAM].config = 1;
#endif
#if defined(WITH_VISAM)
	io_rtns [COB_IO_VISAM].config = 1;
#endif
#if defined(WITH_VBISAM)
	io_rtns [COB_IO_VBISAM].config = 1;
#endif
#if defined(WITH_DB)
	io_rtns [COB_IO_BDB].config = 1;
#endif
#if	defined(WITH_LMDB)
	io_rtns [COB_IO_LMDB].config = 1;
#endif
#if	defined(WITH_ODBC)
	io_rtns [COB_IO_ODBC].config = 1;
#endif
#if	defined(WITH_OCI)
	io_rtns [COB_IO_OCI].config = 1;
#endif

}

/* Call this routine when a new process has been forked */
void
cob_fork_fileio (cob_global *lptr, cob_settings *sptr)
{
	int	k;
	COB_UNUSED (lptr);
	COB_UNUSED (sptr);
	for(k=0; k < COB_IO_MAX; k++) {
		if (fileio_funcs[k] != NULL
		 && fileio_funcs[k]->iofork != NULL) {
			fileio_funcs[k]->iofork (&file_api);
		}
	}
}


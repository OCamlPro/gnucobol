/*
   Copyright (C) 2023 Free Software Foundation, Inc.
   Written by Emilien Lemaire and Fabrice Le Fessant.

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

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <stdarg.h>

#include "config.h"

/* include internal and external libcob definitions, forcing exports */
#define COB_LIB_EXPIMP
#include "coblocal.h"

#include "tarstamp.h"
#include "common.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef	_WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef MOUSE_MOVED
#include <process.h>
#include <io.h>
#include <fcntl.h>	/* for _O_BINARY only */
#endif

#include <sys/time.h>

/* Remember static and dynamic configuration */
static cob_global               *cobglobptr = NULL;
static cob_settings             *cobsetptr = NULL;
static const char* cob_jor_filename ;

static int is_active = 0;
static char* jor_buffer ;
static int jor_size ;
static char* jor_position ;
static int jor_name_counter ;

static struct timeval tv0;

static int field_secs ;
static int field_usecs ;
static int field_status ;
static int field_reason ;
static int field_file ;
static int field_name ;
static int field_truncated ;
static int field_start ;
static int field_exit ;
static int field_duration ;
static int field_time ;
static int field_args ;

static int field_file_op[COB_JOR_FILE_OPERATIONS] ;

static const char* field_file_op_name[COB_JOR_FILE_OPERATIONS] = {
	"write-try",
	"write-ok",
	"read-try",
	"read-ok",
	"start-try",
	"start-ok",
	"open-try",
	"open-ok",
	"close-try",
	"close-ok",
};

#define JOR_MIN_END_SIZE 256
#define JOR_VERSION        1

/* JOR format:

 * the idea is to have it binary (for size) and extensible (a tool
  should be able to read both new and old versions without problem)

  HEADER: 8 bytes
  * u32: total size of JOR (including the HEADER)
  * u8: version (currently 1)
  * u8[3]: padding (not used)

  JOURNAL: a concatenation of records

  * RECORD:
    * u16: size of record (including this size)
    * u8: opcode
    * PAYLOAD: depends on opcode

  * OPCODES:
    * 0 = NEW FIELD NAME: defines the identifier associated with a new field
          names. Identifiers should be consecutive in 0.255
        payload:
        * u8: identifier
        * u8: size of field name = LEN
        * u8[LEN]: field name

    * 1 = LOCAL FIELD: defines a new field, or rewrite an existing field. The
          field as an identifier (the name of the field), a level (level 0
          is a toplevel value, level N>0 is a field inside the latest record
          at level N-1) and a value.
        payload:
        * u8: level
        * u8: field identifier
        * u8: type identifier TYPE
        * u8[sizeof(TYPE)]: value of type TYPE

  * TYPES:
    * 0 = record (fields are added in new records)
      size: 0
    * 1 = uint8
      * u8: value
    * 2 = int8
      * i8: value
    * 3 = uint16
      * u16: value
    * 4 = int16
      * i16: value
    * 5 = uint32
      * u32: value
    * 6 = int32
      * i32: value
    * 7 = uint64
      * u64: value
    * 8 = int64
      * i64: value
    * 9 = double
      * u64: IEEE float
    * 10 = string8 (string of size < 256)
      * u8: LEN of string (without ending 0)
      * u8[LEN] : string
    * 11 = string16 (string of size > 255)
      * u16: LEN of string (without ending 0)
      * u8[LEN] : string

A typical JOR extracted in textual form will look like:

start = {
  time = {
     secs = UnixTime (secs)
     usecs = UnixTime (remaining usecs)
  }
  args = {
    name  = "cobcrun"
    name = "MXAUTV78"
  }
}

file = {
  name = "FILE"
  open-try = 1
  open-ok = 1
  read-try = 10
  read-ok = 9
  close-try = 1
  close-ok = 1
}

exit = {
  status = 0
  reason = "STOP RUN"
  time = {
     secs = ...
     usecs = ...
  }
  duration = {
     secs = ...
     usecs = ...
  }
}

 */

#define RECORD_BEGIN(opcode)	{	       \
	char *jor_record_begin = jor_position; \
	jor_position += 2;		       \
	*jor_position++ = opcode
#define RECORD_END()					\
	*((cob_u16_t*) jor_record_begin) =		\
		jor_position - jor_record_begin;	\
	*((cob_u32_t*) jor_buffer) =			\
		jor_position - jor_buffer;		\
	}
#define RECORD_FIELD(level,field,type)		\
	*jor_position++ = level;		\
	*jor_position++ = field;		\
	*jor_position++ = type

#define OPCODE_NEW_NAME 0
#define OPCODE_LOCAL_FIELD 1

#define TYPE_RECORD     0
#define TYPE_UINT8      1
#define TYPE_INT8       2
#define TYPE_UINT16     3
#define TYPE_INT16      4
#define TYPE_UINT32     5
#define TYPE_INT32      6
#define TYPE_UINT64     7
#define TYPE_INT64      8
#define TYPE_FLOAT      9
#define TYPE_STRING8   10
#define TYPE_STRING16  11

static int jor_field_name (const char* s)
{
	int id = jor_name_counter++;
	int len = strlen (s);
	RECORD_BEGIN (OPCODE_NEW_NAME);
	*jor_position++ = id;
	*jor_position++ = len;
	memcpy (jor_position, s, len);
	jor_position += len;
	RECORD_END ();
	return id;
}

static void jor_field_record (int level, int field)
{
	RECORD_BEGIN (OPCODE_LOCAL_FIELD);
	RECORD_FIELD (level, field, TYPE_RECORD);
	RECORD_END ();
}

static void jor_field_uint8 (int level, int field, cob_u8_t v)
{
	RECORD_BEGIN (OPCODE_LOCAL_FIELD);
	RECORD_FIELD (level, field, TYPE_UINT8);
	*jor_position++ = v;
	RECORD_END ();
}
static void jor_field_uint32 (int level, int field, cob_u32_t v)
{
	RECORD_BEGIN (OPCODE_LOCAL_FIELD);
	RECORD_FIELD (level, field, TYPE_UINT32);
	* ( (cob_u32_t*)jor_position) = v;
	jor_position += 4;
	RECORD_END ();
}
static void jor_field_string8 (int level, int field, const char* v)
{
	int len = v != NULL ? strlen(v) : 0;
	RECORD_BEGIN (OPCODE_LOCAL_FIELD);
	RECORD_FIELD (level, field, TYPE_STRING8);
	*jor_position++ = len;
	if (len>0){
		memcpy ( jor_position, v, len);
		jor_position += len;
	}
	RECORD_END ();
}

static
char* jor_allocate(int *size)
{
	return cob_malloc(*size);
}

static
void jor_save (const char* filename, char* buffer, int len)
{
	FILE *fd = fopen (filename, "w");
	const char* s = jor_buffer;
	fwrite (s, len, 1, fd);
	fclose (fd);
}

static struct cob_jor_funcs jor_funcs = {
	jor_save,
	jor_allocate
};

void cob_jor_set_funcs (struct cob_jor_funcs *f)
{
	jor_funcs.save = f->save;
	jor_funcs.allocate = f->allocate;
}

void cob_init_jor (cob_global *lptr, cob_settings *sptr,
		   int cob_argc, char** cob_argv)
{
	int i;

	if (!!cobsetptr) return ;

	cobglobptr = lptr;
	cobsetptr  = sptr;
	cob_jor_filename = cob_strdup (cobsetptr->cob_jor_filename);

	/* Check that these fields have been correctly initialized
	   by the developer. */
	if (COB_JOR_FILE_OPERATIONS != COB_JOR_AFTER_LAST_OPERATION){
		fprintf (stderr, "COB_JOR_FILE_OPERATIONS = %d\n",
			 COB_JOR_FILE_OPERATIONS);
		fprintf (stderr, "COB_JOR_AFTER_LAST_OPERATION = %d\n",
			 COB_JOR_AFTER_LAST_OPERATION);
		exit (2);
	}
	if ( field_file_op_name[COB_JOR_FILE_OPERATIONS-1] == NULL ){
		fprintf (stderr,
			 "field_file_op_name[%d] not initialized\n",
			 COB_JOR_FILE_OPERATIONS-1);
		exit (2);
	}

	if (!cobsetptr->cob_jor_enable &&
	    /* testsuite clears COB_JOR_ENABLE... */
	    !getenv ("COB_JOR_ENABLED")) return ;

	if (cob_argc == 0) return ;

	is_active = 1;

	/* Initialize JOR buffer */
	jor_buffer = jor_funcs.allocate
		(& cobsetptr->cob_jor_max_size);

	jor_size = cobsetptr->cob_jor_max_size ;
	jor_position = jor_buffer;

	jor_buffer +=4;    /* size of journal */
	*jor_position = JOR_VERSION; /* version */
	jor_position += 3; /* padding */

	/* Initialize JOR field names */
	field_start = jor_field_name ("start");
	field_exit = jor_field_name ("exit");
	field_duration = jor_field_name ("duration");
	field_secs = jor_field_name ("secs");
	field_usecs = jor_field_name ("usecs");
	field_status = jor_field_name ("status");
	field_reason = jor_field_name ("reason");
	field_file = jor_field_name ("file");
	field_name = jor_field_name ("name");
	field_time = jor_field_name ("time");
	field_args = jor_field_name ("args");

	/* Start storing information */
	gettimeofday (&tv0, NULL);
	jor_field_record (0, field_start);
	jor_field_record (1, field_time);
	jor_field_uint32 (2, field_secs, tv0.tv_sec);
	jor_field_uint32 (2, field_usecs, tv0.tv_usec);
	jor_field_record (1, field_args);
	for (i=0; i<cob_argc; i++){
		jor_field_string8 (1, field_name,
				   cob_argv[i]);
	}
}

static int
jor_truncate(void)
{
	static int truncated = 0;

	if (truncated) return 1;

	if ( jor_position - jor_buffer > jor_size - JOR_MIN_END_SIZE){
		truncated = 1;
		field_truncated = jor_field_name ("truncated");
		jor_field_uint8 (0, field_truncated, 1);
	}

	return 0;
}

void cob_jor_file_operation (cob_file *f, enum cob_jor_file_operation op)
{
	if (is_active){
		cob_u32_t *counter ;

		if (!f->jor){
			if (jor_truncate()) return;
			f->jor = cob_malloc (sizeof(cob_file_jor));
			jor_field_record (0, field_file);
			jor_field_string8 (1, field_name,
					  f->select_name);
		}

		counter = f->jor->ops[op];

		if (counter == NULL){
			int field = field_file_op[op];

			if (jor_truncate()) return;

			if( field == 0 ){
				field = jor_field_name (field_file_op_name[op]);
				field_file_op[op] = field;
			}

			jor_field_record (0, field_file);
			jor_field_uint32 (1, field, 0);
			counter = (cob_u32_t*)  (jor_position-4);
			f->jor->ops[op] = counter ;
		}
		/* counting is "just" allocating the counter in place */
		(*counter)++;
	}
}

void cob_jor_exit(int code, const char* fmt, ...)
{
	if (is_active){
		va_list args ;
		static char exit_reason[COB_MINI_BUFF] = { 0 };
		struct timeval tv;
		int len;

		gettimeofday (&tv, NULL);

		va_start (args, fmt);
		vsnprintf (exit_reason, COB_MINI_BUFF, fmt, args);
		va_end (args);

		jor_field_record (0, field_exit);
		jor_field_uint8 (1, field_status, code);
		jor_field_string8 (1, field_reason, exit_reason);

		if ( tv.tv_usec < tv0.tv_usec ){
			tv.tv_usec += 1000000;
		tv.tv_sec--;
		}
		tv.tv_usec -= tv0.tv_usec;
		tv.tv_sec -= tv0.tv_sec;

		jor_field_record (1, field_time);
		jor_field_uint32 (2, field_secs, tv.tv_sec);
		jor_field_uint32 (2, field_usecs, tv.tv_usec);

		jor_field_record (1, field_duration);
		jor_field_uint32 (2, field_secs, tv.tv_sec);
		jor_field_uint32 (2, field_usecs, tv.tv_usec);

		len = jor_position - jor_buffer;
		jor_funcs.save (cob_jor_filename, jor_buffer, len);
	}
}



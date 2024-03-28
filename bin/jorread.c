/*
   Copyright (C) 2024 Free Software Foundation, Inc.
   Written by Fabrice LE FESSANT

   This file is part of GnuCOBOL.

   The GnuCOBOL module loader is free software: you can redistribute it
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

#include "tarstamp.h"
#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

#ifdef	HAVE_LOCALE_H
#include <locale.h>
#endif
#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <arpa/inet.h>

#include "../libcob/common.h"
#include "../libcob/cobgetopt.h"

#define GET_U16(addr)				\
	(*((cob_u16_t*) (addr)))

#define GET_U32(addr)				\
	(*((cob_u32_t*) (addr)))

#define GET_S16(addr)				\
	( *((cob_s16_t*) (addr)))

#define GET_S32(addr)				\
	(*((cob_s32_t*) (addr)))


#define MAX_LEVELS 10

static void jor_read (char *filename)
{
	FILE *fd = fopen (filename, "r");
	char *buffer;
	char header[JOR_HEADER_SIZE];
	int len;
	int version;
	int size;
	char *fields[256];
	char *position;
	int current_level;
	char spaces[3+2*MAX_LEVELS];
	int first_field[MAX_LEVELS];

	memset (spaces, ' ', sizeof(spaces));
	if ( !fd ){
		fprintf (stderr, "Error: could not open file %s\n", filename);
		exit (2);

	}

	len = fread (header, 1, JOR_HEADER_SIZE, fd);

	if ( len<JOR_HEADER_SIZE ){
		fprintf (stderr, "Error: truncated file %s\n", filename);
		exit (2);

	}
	if ( memcmp (header, JOR_MAGIC, JOR_MAGIC_LEN) ){
		fclose (fd);
		fprintf (stderr, "Error: file %s is not a GnuCOBOL JOR file\n",
			 filename);
		exit (2);
	}

	size = GET_U32(header+JOR_MAGIC_LEN);
	version = header[JOR_MAGIC_LEN+4];

	if (size < JOR_HEADER_SIZE){
		fprintf (stderr, "Error: corrupted file %s (wrong size %d)\n",
			 filename, size);
		exit (2);
	}

	size -= JOR_HEADER_SIZE;

	if (version>1){
		fprintf (stderr, "Error: file %s version %d too high\n",
			 filename, version);
		exit (2);
	}

	buffer = cob_malloc (size);

	len = fread (buffer, 1, size, fd);

	if (len < size){
		fprintf (stderr, "Warning: corrupted file %s, size %d < expected %d\n",
			 filename, len, size);
	}

	fprintf (stderr, "Reading file %s with content size %d\n",
		 filename, size);
	position = buffer;
	current_level = 0;

	printf ("{\n");
	first_field[0] = 1;

	while (position - buffer < size){
		int record_size = GET_U16 (position);
		char *next_position = position+record_size;
		int opcode = position[2];

		switch (opcode){
		case OPCODE_NEW_NAME: {
			// fprintf (stderr, "opcode OPCODE_NEW_NAME\n");
			int id = position[3];
			int slen = position[4];
			char *s = cob_malloc(slen+1);

			memcpy (s, position+5, slen);
			s[slen] = 0;
			fields[id] = s;

			// fprintf (stderr, "Field %d is '%s'\n", id, s);
			break;
		}
		case OPCODE_LOCAL_FIELD: {
			// fprintf (stderr, "opcode OPCODE_LOCAL_FIELD\n");
			int level = position[3];
			int id = position[4];
			int type = position[5];

			while (level < current_level){
				spaces[current_level*2] = 0;
				printf ("%s}", spaces);
				spaces[current_level*2] = ' ';
				current_level--;
			}

			if (!first_field[current_level]){
				printf (",\n");
			} else {
				first_field[current_level] = 0;
			}

			spaces[2+current_level*2] = 0;
			printf ("%s\"%s\" : ", spaces, fields[id]);
			spaces[2+current_level*2] = ' ';

			switch (type){
			case TYPE_RECORD:
				printf ("{\n");
				current_level = level+1;
				first_field[current_level] = 1;
				break;
			case TYPE_UINT8: {
				cob_u8_t value = position[6];
				printf ("%d", value);
				break;
			}
			case TYPE_INT8: {
				int value = position[6];
				printf ("%d", value);
				break;
			}
			case TYPE_UINT16: {
				cob_u16_t value = GET_U16 (position+6);
				printf ("%d", value);
				break;
			}
			case TYPE_INT16: {
				cob_s16_t value = GET_S16 (position+6);
				printf ("%d", value);
				break;
			}
			case TYPE_UINT32: {
				cob_u32_t value = GET_U32 (position+6);
				printf ("%d", value);
				break;
			}
			case TYPE_INT32: {
				cob_s32_t value = GET_S32 (position+6);
				printf ("%d", value);
				break;
			}
			case TYPE_FLOAT: {
				double value = ((double*) (position+6))[0];
				printf ("%f", value);
				break;
			}
			case TYPE_STRING16: {
				exit (2);
			}
			case TYPE_STRING8: {
				int len = position[6];
				char buf[256];

				memcpy (buf, position+7, len);
				buf[len]=0;
				printf ("\"%s\"", buf);
				break;
			}
			}
			break;
		}
		default:
			fprintf (stderr, "Error: file %s contains an unknown opcode %d",
				 filename, opcode);
		}

		position = next_position ;
		// fprintf (stderr, "record_size = %d\n", record_size);
	}

	while (0 < current_level){
		spaces[current_level*2] = 0;
		printf ("%s}\n", spaces);
		spaces[current_level*2] = ' ';
		current_level--;
	}

	printf ("}\n");

	fprintf (stderr, "done\n");
	fclose (fd);
}

int main (int argc, char** argv)
{

	int i;

	for (i=1; i<argc; i++){
		jor_read (argv[i]);
	}
	return 0;
}

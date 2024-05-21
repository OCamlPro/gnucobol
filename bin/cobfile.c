/*
   Copyright (C) 2021-2022 Free Software Foundation, Inc.
   Written by Ron Norman

   This file is part of GnuCOBOL.

   The GnuCOBOL cobfile program is free software: you can redistribute it
   and/or modify it under the terms of the GNU General Public License
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
     Program:  cobfile.c

     Function: This program is used to create/execute
               file copy/convert programs.
*/

#include	"config.h"
#include	<stdio.h>
#include	<stdlib.h>
#ifdef	HAVE_UNISTD_H
#include	<unistd.h>
#endif
#include	<string.h>
#include	<time.h>
#ifdef	HAVE_SYS_TIME_H
#include	<sys/time.h>
#endif
#include	<sys/types.h>
#include	<sys/stat.h>
#include	<ctype.h>
#include	<libcob.h>
#include	<stdarg.h>
#include	<errno.h>
#include	"tarstamp.h"
#include	"libcob/cobgetopt.h"
#include	"libcob/sysdefines.h"
#if defined(HAVE_READLINE)
#include <readline/readline.h>
#include <readline/history.h>
#endif

/* needed for time checks */
#ifdef	HAVE_LOCALE_H
#include <locale.h>
#endif


#if	defined(ENABLE_NLS) && defined(COB_NLS_RUNTIME)
#include	"defaults.h" /* get LOCALEDIR */
#include "gettext.h"	/* from lib/ */
#define _(s)		gettext(s)
#define N_(s)		gettext_noop(s)
#else
#define _(s)		s
#define N_(s)		s
#endif


#define TRUE 1
#define FALSE 0
static time_t	nowis;
static int be_quiet = 0;
static int keep_code = 0;
static int batchin = 0;
static const cob_field_attr	const_alpha_attr =
				{COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
static const cob_field_attr	all_numeric_display_attr =
				{COB_TYPE_NUMERIC_DISPLAY, COB_MAX_DIGITS, 0,
				 0, NULL};
static char prompt[64] = "cobfile:>";
static char	cmdfile[256] = "";
static char	cblout[48] = "CPYFILE";
static char	proglwr[32] = "CPYFILE";
static char	progid[32] = "CPYFILE";
static char cobcopts[128] = "-ffile-format=mf";
static char copyin[256] = "";
static char copyout[256] = "";
static char fileindef[256] = "";
static const char *copyext[] = {"",".cpy",".cbl",".cob",".CPY",".CBL",".COB",NULL};
static char *copysearch = NULL;
static char	progver[64] = "* Created by cobfile: ";
static char	inorg[48] = "RELATIVE";
static char	*inkeys[16] = {NULL,NULL,NULL};
static char wrkstr[1024];
static char inkeydesc[1024];
static char skipif[1024] = "";
static char skipnum[12] = "0";
static char copynum[12] = "0";
static char	outorg[48] = "RELATIVE";
static char	*outkeys[16] = {NULL,NULL,NULL};
static char outkeydesc[1024];
static int	outNl = 1;
static FILE	*fo = NULL;
static FILE	*fcmd = NULL;
/********************************/
/**  Source code for emission  **/
/********************************/
typedef struct {
	char		type;	/* type of statement  */
	const char	*text;	/* Text to be emitted */
} Source;

static Source ident[] = {
	{0," IDENTIFICATION DIVISION.\n"},
	{0," PROGRAM-ID. "},
	{0,progid},
	{0,".\n"},
	{0,progver},
	{0,"\n"},
	{0," ENVIRONMENT DIVISION.\n"},
	{0," CONFIGURATION SECTION.\n"},
	{0," INPUT-OUTPUT  SECTION.\n"},
	{0," FILE-CONTROL.\n"},
	{0,"     SELECT INFILE\n"},
	{0,"     ASSIGN EXTERNAL INFILE\n"},
	{0,"     ORGANIZATION "},
	{0,inorg},
	{0,"\n"},
	{0,"     ACCESS IS SEQUENTIAL\n"},
	{0,inkeydesc},
	{0,"     FILE STATUS IS IN-STAT.\n"},
	{0," \n"},
	{0,"     SELECT OUTFILE\n"},
	{0,"     ASSIGN EXTERNAL OUTFILE\n"},
	{0,"     ORGANIZATION "},
	{0,outorg},
	{0,"\n"},
	{0,"     ACCESS IS SEQUENTIAL\n"},
	{0,outkeydesc},
	{0,"     FILE STATUS IS OUT-STAT.\n"},
	{0," \n"},
	{0," DATA DIVISION.\n"},
	{0," FILE SECTION.\n"},
	{0,NULL}};
static Source inputfd[] = {
	{0," FD INFILE\n"},
	{0,"     BLOCK CONTAINS 5 RECORDS.\n"},
	{0,NULL}};
static Source outputfd[] = {
	{0," FD OUTFILE\n"},
	{0,"     BLOCK CONTAINS 5 RECORDS.\n"},
	{0,NULL}};
static Source working[] = {
	{0," WORKING-STORAGE SECTION.\n"},
	{0," 01  IN-STAT              PIC XX    VALUE '00'.\n"},
	{0," 01  OUT-STAT             PIC XX    VALUE '00'.\n"},
	{0," 01  IN-RECNUM            PIC 9(12) VALUE 0.\n"},
	{0," 01  OUT-RECNUM           PIC 9(12) VALUE 0.\n"},
	{0," 01  RECS-READ            PIC 9(12) VALUE 0.\n"},
	{0," 01  RECS-WRITTEN         PIC 9(12) VALUE 0.\n"},
	{0," 01  SKIP-RECS            PIC 9(12) VALUE "},
	{0,skipnum},
	{0,".\n"},
	{0," 01  COPY-RECS            PIC 9(12) VALUE "},
	{0,copynum},
	{0,".\n"},
	{0," \n"},
	{0,NULL}};
static Source procedure[] = {
	{0," PROCEDURE DIVISION.\n"},
	{0," MAIN-100.\n"},
	{0,"     OPEN INPUT INFILE.\n"},
	{0,"     IF IN-STAT NOT = '00'\n"},
	{0,"         DISPLAY 'ERROR ' IN-STAT ' OPENING INPUT FILE'\n"},
	{0,"         STOP RUN\n"},
	{0,"     END-IF.\n"},
	{0,"     OPEN OUTPUT OUTFILE.\n"},
	{0,"     IF OUT-STAT NOT = '00'\n"},
	{0,"         DISPLAY 'ERROR ' OUT-STAT ' OPENING OUTPUT FILE'\n"},
	{0,"         STOP RUN\n"},
	{0,"     END-IF.\n"},
	{0,"     INITIALIZE OUT-REC.\n"},
	{0," \n"},
	{0," GET-NEXT-100.\n"},
	{0,"     READ INFILE\n"},
	{0,"     \tAT END GO TO END-PROG-100\n"},
	{0,"     END-READ.\n"},
	{0,"     IF  IN-STAT NOT = '00'\n"},
	{0,"     AND IN-STAT NOT = '02'\n"},
	{0,"         DISPLAY 'ERROR ' IN-STAT ' READING INPUT'\n"},
	{0,"         GO TO END-PROG-100\n"},
	{0,"     END-IF.\n"},
	{0,"     ADD 1  TO RECS-READ.\n"},
	{0,"     IF  SKIP-RECS > 0\n"},
	{0,"     AND SKIP-RECS <= RECS-READ\n"},
	{0,"         GO TO GET-NEXT-100.\n"},
	{0,skipif},
	{0," \n"},
	{0,NULL}};
static Source endprog[] = {
	{0,"     WRITE OUT-REC.\n"},
	{0,"     IF  OUT-STAT NOT = '00'\n"},
	{0,"     AND OUT-STAT NOT = '02'\n"},
	{0,"         DISPLAY 'ERROR ' OUT-STAT ' WRITING OUTPUT'\n"},
	{0,"         GO TO END-PROG-100\n"},
	{0,"     END-IF.\n"},
	{0,"     ADD 1  TO RECS-WRITTEN.\n"},
	{0,"     IF  COPY-RECS > 0\n"},
	{0,"     AND COPY-RECS >= RECS-WRITTEN\n"},
	{0,"         GO TO END-PROG-100.\n"},
	{0,"     GO TO GET-NEXT-100.\n"},
	{0," \n"},
	{0," END-PROG-100.\n"},
	{0,"     DISPLAY RECS-READ    ' records read'.\n"},
	{0,"     DISPLAY RECS-WRITTEN ' records written'.\n"},
	{0,"     CLOSE INFILE.\n"},
	{0,"     CLOSE OUTFILE.\n"},
	{0,"     STOP RUN.\n"},
	{0,NULL}};

static const char short_options[] = "hqVki:p:D:";

#define	CB_NO_ARG	no_argument
#define	CB_RQ_ARG	required_argument
#define	CB_OP_ARG	optional_argument

static const struct option long_options[] = {
	{"in",			CB_RQ_ARG, NULL, 'i'},
	{"prog",		CB_RQ_ARG, NULL, 'p'},
	{"define",		CB_RQ_ARG, NULL, 'D'},
	{"keep",		CB_NO_ARG, NULL, 'k'},
	{"help",		CB_NO_ARG, NULL, 'h'},
	{"quiet",		CB_NO_ARG, NULL, 'q'},
	{"version",     CB_NO_ARG, NULL, 'V'},
	{NULL, 0, NULL, 0}
};

/* Remove trailing CR, LF and spaces */
static int
trim_line(char *buf)
{
	int	j,k;
	k = strlen(buf);
	while (k > 0
		&& (buf[k-1] == '\r' || buf[k-1] == '\n')) {
		buf[--k] = 0;
	}
	while (k > 0
		&& buf[k-1] == ' ') {
		buf[--k] = 0;
	}
	for (j=k=0; buf[k] != 0; ) {
		if (buf[k] == ' ' && buf[k+1] == ' ') {
			k++;
		} else if (j==0 && buf[k] == ' ') {
			k++;
		} else {
			buf[j++] = buf[k++];
		}
	}
	buf[j] = 0;
	return k;
}

/* Read a line from stdin */
static char *
getLine (char *buf)
{
	int		k;
	char	ibuf[1024];
getnext:
	if (fcmd != NULL) {
		if (fgets (ibuf, sizeof(ibuf)-1, fcmd) == NULL) {
			fclose (fcmd);
			fcmd = NULL;
			return NULL;
		}
	} else {
#if defined(HAVE_READLINE)
	{
		char	*p;
		if (batchin) {
			if (fgets (ibuf, sizeof(ibuf)-1, stdin) == NULL)
				return NULL;
		} else {
			if ((p = readline (prompt)) == NULL)
				return NULL;
			strcpy (ibuf,p);
			free (p);
			if (ibuf[0] >= ' '
			 && ibuf[0] != '*'
			 && ibuf[0] != '#')
				add_history (ibuf);
		}
	}
#else
	if (!batchin) {
		fputs (prompt, stdout);
		fflush(stdout);
	}
	if (fgets (ibuf, sizeof(ibuf)-1, stdin) == NULL)
		return NULL;
#endif
	}
	trim_line (ibuf);
	if (ibuf[0] == '*'
	 || ibuf[0] == '#')
		goto getnext;
	for (k=0; memcmp(&ibuf[k],"  ",2)==0; k++);
	strcpy (buf, &ibuf[k]);
	return buf;
}

/* Make a new 'cob_field' */
static cob_field *
makeField (int size)
{
	cob_field *f;

	f = calloc (1, sizeof(cob_field));
	f->size = size;
	f->attr = &const_alpha_attr;
	f->data = calloc (1, size);
	return f;
}

/* Make a new 'cob_field' */
static cob_field *
makeField9 (int size)
{
	cob_field *f;

	f = calloc (1, sizeof(cob_field));
	f->size = size;
	f->attr = &all_numeric_display_attr;
	f->data = calloc (1, size);
	return f;
}

/* Drop a 'cob_field' */
static void
dropField (cob_field *f)
{
	if (f == NULL)
		return;
	if (f->data)
		free ((void*)f->data);
	free ((void*)f);
}

/* Does the 'word' match within the string */
static int
matchWord (const char *word, unsigned char *defs, char *str, int *pos )
{
	int	j, k, doeq, doend;
	int	ln = strlen (word);
	char	*newstr, marker, endmark;
	marker = endmark = 3;
	doend = doeq = 0;
	if (word[ln-1] == '=') {
		marker = '=';
		doeq = 1;
		ln--;
	} else if (word[ln-1] == '[') {
		marker = '[';
		endmark = ']';
		doend = doeq = 1;
		ln--;
	}
	if (strncasecmp (defs + *pos, word, ln) != 0)
		return 0;
	k = *pos + ln;
	while (isspace(defs[k])) k++;
	if (doeq) {
		if (defs[k] != marker)
			return 0;
		k++;
		while (isspace(defs[k])) k++;
	}
	if (doend) {
		for (j = 0; defs[k] != endmark
			&& defs[k] != 0; )
		str[j++] = defs[k++];
	} else {
		for (j = 0; defs[k] > ' '
			&& defs[k] != ','
			&& defs[k] != ';'
			&& defs[k] != endmark
			&& defs[k] != 0; )
		str[j++] = defs[k++];
	}
	str[j] = 0;
	if (defs[k] == endmark)
		k++;
	memset (defs + *pos, ' ', k - *pos);
	*pos = k - 1;
	newstr = cob_expand_env_string (str);
	strcpy (str, newstr);
	cob_free (newstr);
	return 1;
}

/* Find the COPY book */
static char *
findCopy (char *book)
{
	char	path[512], *p, *t;
	int		k,pl;
	FILE	*fi;

	strcpy (path, book);
	fi = fopen (path, "r");
	if (fi != NULL) {
		strcpy (book, path);
		fclose (fi);
		return book;
	}
	for (p = copysearch; p && *p != 0; p = t) {
		t = strchr (p, PATHSEP_CHAR);
		if (t == NULL) {
			pl = sprintf (path, "%s", p);
			t = p + pl;
		} else {
			pl = sprintf (path, "%.*s", (int)(t-p), p);
			t++;
		}
		for (k = 0; copyext[k] != NULL; k++) {
			sprintf(&path[pl],"%c%s%s",SLASH_CHAR,book,copyext[k]);
			fi = fopen (path, "r");
			if (fi != NULL) {
				strcpy (book, path);
				fclose (fi);
				return book;
			}
		}
	}
	return NULL;
}

static void
getRecsz (cob_file *fl, char *def)
{
	int	k, rcsz;
	for (k=0; def[k] != 0; k++) {
		if (strncasecmp (&def[k], "recsz=",6) == 0) {
			rcsz = atoi (&def[k+6]);
			fl->record_max = rcsz;
			fl->record_min = rcsz;
			break;
		}
		if (strncasecmp (&def[k], "maxsz=",6) == 0) {
			rcsz = atoi (&def[k+6]);
			fl->record_max = rcsz;
		}
		if (strncasecmp (&def[k], "minsz=",6) == 0) {
			rcsz = atoi (&def[k+6]);
			fl->record_min = rcsz;
		}
	}
}

/* Parse out the file definition */
static void
parseFile (cob_file *fl, const char *select, int rcsz,
		unsigned char *defs, char *copy, char *keys[])
{
	char	val[1024], both[1024+18], filename[1024];
	int		j, k, ln, idx;
	cob_field *flsts;

	strcpy (copy, "");
	keys[0] = NULL;
	idx = 1;
	fl->select_name = strdup (select);
	fl->file_version = COB_FILE_VERSION;
	fl->organization = COB_ORG_SEQUENTIAL;
	fl->access_mode = COB_ACCESS_SEQUENTIAL;
	for (k=0; defs[k] != 0; k++) {
		if (k==0
		 || isspace(defs[k-1])) {
			if (matchWord ("FILE=", defs, val, &k)) {
				ln = strlen (val);
				dropField (fl->assign);
				fl->assign = makeField (ln + 2);
				memcpy(fl->assign->data, val, ln);
				fl->assign->data[ln] = 0;
			} else
			if (matchWord ("COPY=", defs, val, &k)) {
				strcpy (both, val);
				if (!findCopy (both)) {
					printf ("Unable to find COPY %s\n",val);
				} else {
					strcpy(copy, val);
				}
			} else
			if (matchWord ("PRIMARY[", defs, val, &k)) {
				keys[0] = strdup (val);
			} else
			if (matchWord ("UNIQUE[", defs, val, &k)) {
				keys[idx++] = strdup (val);
			} else
			if (matchWord ("INDEX[", defs, val, &k)) {
				strcat (val,"\n\t\t\tWITH DUPLICATES");
				keys[idx++] = strdup (val);
			} else
			if (matchWord ("SUPPRESS[", defs, val, &k)) {
				sprintf (both,"%s\n\t\t\tSUPPRESS WHEN %s",keys[idx-1],val);
				free (keys[idx-1]);
				keys[idx-1] = strdup (both);
			}
		}
	}
	for (j=k=0; defs[k] != 0; ) {
		if (defs[k] == ' ' && defs[k+1] == ' ') {
			k++;
		} else {
			defs[j++] = defs[k++];
		}
	}
	defs[j] = 0;
	fl->record_max = rcsz;
	getRecsz (fl, defs);

	if (strcmp (select,"INPUT") == 0) {
		cob_pre_open_def (fl, defs, fileindef, 1);
		getRecsz (fl, fileindef);
	} else if (fileindef[0] > ' ') {
		fl->nkeys = 0;
		getRecsz (fl, fileindef);
		sprintf(both,"%s %s",fileindef, defs);
		cob_pre_open_def (fl, both, val, 0);
		if (fl->assign
		 && fl->assign->data) {
			flsts = makeField (2);
			cob_delete_file (fl, flsts, 0);
			dropField (flsts);
			sprintf(filename,"%s.dat",fl->assign->data);
			unlink (filename);
			sprintf(filename,"%s.idx",fl->assign->data);
			unlink (filename);
		}
		getRecsz (fl, val);
	} else {
		cob_pre_open_def (fl, defs, NULL, 0);
	}
}

/*****************************************************************/
/* "output" a formated string to the output file                 */
/*****************************************************************/
static void
output(const char *fmt, ...)
{
	va_list ap;
	char buf[ 4096 ];
	int	i, ln, col, hang;

	va_start( ap, fmt );
	vsprintf( buf, fmt, ap );

	i = col = hang = 0;
	ln = strlen(buf);
	while (i < ln) {
		if(outNl
		&& buf[i] != '\n') {
			fprintf(fo,"      ");
			col = 6;
		}
		if(buf[i] == '!') {
			fputs("     ", fo);	/* Skip over to AREA B */
			i++;
			col += 5;
		}
		while (buf[i] != '\n' && i < ln) {
			if (buf[i] == '\t') {
				do {
					fputc (' ', fo);
					col++;
				} while ( (col % 4) != 3 );
			} else if (buf[i] == '~') {
				hang = col;
			} else if (buf[i] == '@') {
				while (col < hang) {
					fputc (' ', fo);
					col++;
				}
			} else {
				fputc(buf[i], fo);
				col++;
			}
			i++;
		}
		if (buf[i] == '\n') {
			fputc(buf[i++], fo);
			outNl = TRUE;
			col = 0;
		}
	}

	if(buf[ln-1] == '\n')
		outNl = TRUE;
	else
		outNl = FALSE;

	va_end( ap );
	return;
}

/*****************************************************************/
/** Write an array of strings to the output file                **/
/*****************************************************************/
static void
outArray(Source	*array)
{
	int		i;
	char	buf[1024];
	for(i=0; array[i].text != NULL; i++) {
		if(strlen(array[i].text) < 1)
			continue;
		strcpy(buf,array[i].text);

		switch( array[i].type ) {
		default:
		case 0:
			output(buf);
			break;
		}
	}
}

/*
* Output version information
*/
static void
gcd_print_version (void)
{
	char	cob_build_stamp[COB_MINI_BUFF];
	char	month[64];
	int 	status, day, year;

	/* Set up build time stamp */
	memset (cob_build_stamp, 0, (size_t)COB_MINI_BUFF);
	memset (month, 0, sizeof(month));
	day = 0;
	year = 0;
	status = sscanf (__DATE__, "%s %d %d", month, &day, &year);
	if (status == 3) {
		snprintf (cob_build_stamp, (size_t)COB_MINI_MAX,
			"%s %02d %04d %s", month, day, year, __TIME__);
	} else {
		snprintf (cob_build_stamp, (size_t)COB_MINI_MAX,
			"%s %s", __DATE__, __TIME__);
	}

	printf ("cobfile (%s) %s.%d\n",
		PACKAGE_NAME, PACKAGE_VERSION, PATCH_LEVEL);
	puts ("Copyright (C) 2022 Free Software Foundation, Inc.");
	puts (_("License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>"));
	puts (_("This is free software; see the source for copying conditions.  There is NO\n"
		"warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE."));
	printf (_("Written by %s"), "Ron Norman");
	putchar ('\n');
	printf (_("Built     %s"), cob_build_stamp);
	putchar ('\n');
	printf (_("Packaged  %s"), COB_TAR_DATE);
	putchar ('\n');
}

/*
 * Display program usage information
*/
static void
gcd_usage (char *prog)
{
	puts (_("Copy/convert data files for GnuCOBOL"));
	putchar ('\n');
	printf (_("usage: %s [options]"), prog);
	putchar ('\n');
	puts (_("Options:"));
	puts (_("  -i  cmdfile    Read commands from this file"));
	puts (_("  -p  progid     Define PROGRAM-ID "));
	puts (_("  -k, --keep     Keep the generated COBOL code"));
	puts (_("  -D  env=val    Define environment variable"));
	puts (_("  -h, --help     display this help and exit"));
	puts (_("  -V, --version  display version and exit"));
	putchar ('\n');
	printf (_("Report bugs to: %s or\n"
		"use the preferred issue tracker via home page"), "bug-gnucobol@gnu.org");
	putchar ('\n');
	puts (_("GnuCOBOL home page: <http://www.gnu.org/software/gnucobol/>"));
	puts (_("General help using GNU software: <http://www.gnu.org/gethelp/>"));
}

static void
set_option (char *binary, int opt, char *arg)
{
	switch(opt) {
	case 'q':
		be_quiet = 1;
		break;

	case 'k':
		keep_code = 1;
		break;

	case 'i':
		strcpy(cmdfile,arg);
		fcmd = fopen (cmdfile, "r");
		if (fcmd == NULL) {
			printf("Error %s opening '%s' input\n",strerror(errno),cmdfile);
		}
		break;

	case 'p':
		strcpy(progid,arg);
		break;

	case 'D':
		putenv (arg);
		break;

	case '?':
	default:
		printf(_("unknown parameter '%c' for %s"),opt,binary);
		putchar ('\n');
		gcd_usage((char*)"cobfile");
		exit(2);
		break;

	case 'h':
		gcd_usage((char*)"cobfile");
		exit(2);
		break;

	case 'V':
		gcd_print_version ();
		exit(2);
		break;
	}
}

static const char *
getOrg (int org)
{
	switch (org) {
	case COB_ORG_SEQUENTIAL:		return "SEQUENTIAL";
	case COB_ORG_LINE_SEQUENTIAL:	return "LINE SEQUENTIAL";
	case COB_ORG_RELATIVE:			return "RELATIVE";
	case COB_ORG_INDEXED:			return "INDEXED";
	default: return "UNKNOWN ORG";
	}
}

static char *
addOfRec (char *str, const char *rec)
{
	int	j,k,dolast;
	dolast = 1;
	for(j=k=0; str[k] != 0; ) {
		if (str[k] == ',') {
			j += sprintf(&wrkstr[j], " OF %s,\n@",rec);
			k++;
			continue;
		}
		if (str[k] == '\n' && dolast) {
			j += sprintf(&wrkstr[j], " OF %s",rec);
			dolast = 0;
		}
		wrkstr[j++] = str[k++];
	}
	if (dolast)
		j += sprintf(&wrkstr[j], " OF %s",rec);
	return wrkstr;
}

/* Generate Copy program */
static void
genCopy (cob_file *fli, cob_file *flo)
{
	int		k;

	strcpy(inorg, getOrg (fli->organization));
	strcpy(inkeydesc,"");
	if (fli->organization == COB_ORG_RELATIVE) {
		strcpy(inkeydesc," \tRELATIVE KEY IS IN-RECNUM\n");
	} else
	if (fli->organization == COB_ORG_INDEXED) {
		sprintf (inkeydesc,
				"!RECORD KEY IS PRIMARY-KEY-1\n!\tSOURCE IS ~%s\n",
					addOfRec(inkeys[0],"IN-REC"));
		for (k=1; inkeys[k] != NULL; k++)
			sprintf (&inkeydesc[strlen(inkeydesc)],
				"!ALTERNATE RECORD KEY IS SECONDARY-KEY-%d\n!\tSOURCE IS ~%s\n",
					k+1,addOfRec(inkeys[k],"IN-REC"));
	}
	strcpy(outkeydesc,"");
	if (flo->organization == COB_ORG_RELATIVE) {
		strcpy(outkeydesc," \tRELATIVE KEY IS IN-RECNUM\n");
	} else
	if (flo->organization == COB_ORG_INDEXED) {
		sprintf (outkeydesc,
				"!RECORD KEY IS O-PRIMARY-KEY-1\n!\tSOURCE IS ~%s\n",
					addOfRec(outkeys[0],"OUT-REC"));
		for (k=1; outkeys[k] != NULL; k++)
			sprintf (&outkeydesc[strlen(outkeydesc)],
				"!ALTERNATE RECORD KEY IS O-SECONDARY-KEY-%d\n!\tSOURCE IS ~%s\n",
					k+1,addOfRec(outkeys[k],"OUT-REC"));
	}
	strcpy(outorg, getOrg (flo->organization));
	for (k=0; progid[k] != 0; k++)
		progid[k] = toupper(progid[k]);
	sprintf(progver,"* Created by cobfile: %s",ctime (&nowis));
	outArray (ident);
	outArray (inputfd);
	fflush (fo);
	output ("\n 01  IN-REC.\n");
	if (copyin[0] > ' ') {
		output ("!COPY %s.\n",copyin);
	} else {
		output ("!05  FILLER\tPIC X(%d).\n",fli->record_max);
	}
	output (" \n");
	outArray (outputfd);
	fflush (fo);
	output ("\n 01  OUT-REC.\n");
	if (copyout[0] > ' ') {
		output ("!COPY %s.\n",copyout);
	} else {
		output ("!05  FILLER\tPIC X(%d).\n",flo->record_max);
	}
	output (" \n");
	outArray (working);
	outArray (procedure);
	if (copyout[0] > ' '
	 && copyin[0] > ' ') {
		output (" \tMOVE CORRESPONDING IN-REC\tTO OUT-REC.\n");
	} else {
		output (" \tMOVE IN-REC\tTO OUT-REC.\n");
	}
	outArray (endprog);
}

/* Copy the data file */
static int
copyFile (cob_file *fi, cob_file *fo, int skip, int ncopy)
{
	cob_field *fists, *fosts;
	int		recs,written;
	fists = makeField (2);
	fi->file_version = COB_FILE_VERSION;
	fo->file_version = COB_FILE_VERSION;
	if (fi->organization == COB_ORG_RELATIVE) {
		fi->keys[0].field = makeField9 (COB_MAX_DIGITS);
	}
	fi->flag_keycheck = 0;
	fi->flag_auto_type = 1;
	cob_open (fi, COB_OPEN_INPUT, 0, fists);
	if (memcmp(fists->data,"00",2) != 0) {
		printf("Status %.2s opening %s for input\n",fists->data,fi->assign->data);
		return 1;
	}
	fosts = makeField (2);
	if (fo->organization == COB_ORG_RELATIVE) {
		fo->keys[0].field = makeField9 (COB_MAX_DIGITS);
	}

	cob_open (fo, COB_OPEN_OUTPUT, 0, fosts);
	if (memcmp(fosts->data,"00",2) != 0) {
		printf("Status %.2s opening %s for output\n",fosts->data,fo->assign->data);
		return 1;
	}
	/* Copy data */
	written = recs = 0;
	while (1) {
		cob_read_next (fi, fists, COB_READ_NEXT);
		if (fists->data[0] > '0') {
			if (memcmp(fists->data,"10",2) != 0)
				printf("READ status %.2s\n",fists->data);
			break;
		}
		recs++;
		if (skip > 0
		 && recs <= skip)
			continue;
		memcpy (fo->record->data, fi->record->data, fi->record_max);
		cob_write (fo, fo->record, 0, fosts, 0);
		if (fosts->data[0] > '0') {
			printf("WRITE status %.2s\n",fosts->data);
			break;
		}
		written++;
		if (ncopy > 0
		 && written >= ncopy)
			break;
	}
	if (recs == written)
		printf("Copied %d records\n",recs);
	else
		printf("Read %d records and wrote %d records\n",recs,written);

	/* Close files */
	cob_close (fi, fists, 0, 0);
	dropField (fists);
	cob_close (fo, fosts, 0, 0);
	dropField (fosts);
	return 0;
}

/*
 * M A I N L I N E   Starts here
 */
int
main(
	int		argc,
	char	*argv[])
{
	int		opt,idx,i,j,k,skip,ncopy;
	FILE	*ref;
	cob_file flin[1], flout[1];
	unsigned char	*env, *p;
	unsigned char	buf[1024], conffile[256], val[128];
	unsigned char	cmd[2560], indef[2560], outdef[2560];

#ifdef	HAVE_SETLOCALE
	setlocale (LC_ALL, "");
#endif
	time (&nowis);
	sprintf (progid, "CPYFIL%02d", (int)(nowis % 97));

	/* Process cobfile.conf from current directory */
	strcpy(conffile,"cobfile.conf");
	ref = fopen(conffile,"r");
	if(ref == NULL) {
		/* Check for cobfile.conf in $HOME directory */
		if ((env = getenv("HOME")) != NULL) {
			sprintf(conffile,"%s/cobfile.conf",env);
			ref = fopen(conffile,"r");
		}
	}
	if(ref == NULL) {
		/* Check for cobfile.conf in config directory */
		if ((env = getenv("COB_CONFIG_DIR")) != NULL) {
			sprintf(conffile,"%s/cobfile.conf",env);
		} else {
			sprintf(conffile,"%s/cobfile.conf",COB_CONFIG_DIR);
		}
		ref = fopen(conffile,"r");
	}
	if(ref) {
		while (fgets(buf,sizeof(buf),ref) != NULL) {
			k = trim_line (buf);
			if (buf[0] == '-') {	/* Option for cobfile ?*/
				opt = buf[1];
				for (i=2; isspace(buf[i]); i++);
				set_option(conffile,opt,&buf[i]);
			}
		}
		fclose(ref);
	}

	idx = 0;
	cob_optind = 1;
	while ((opt = cob_getopt_long_long (argc, argv, short_options,
					  long_options, &idx, 1)) >= 0) {
		set_option(argv[0], opt, cob_optarg);
	}

	if(!isatty(0)) {
		batchin = 1;
	} else if (fcmd == NULL) {
		printf("Commands end with a semicolon;\n");
		printf("To exit enter:   quit;\n");
	}
	cob_extern_init ();
	if ((env = getenv("COB_COPY_DIR")) != NULL) {
		copysearch = strdup (env);
	}
	if ((env = getenv("COBCPY")) != NULL) {
		if (copysearch == NULL) {
			copysearch = strdup (env);
		} else {
			p = malloc (strlen (env) + strlen (copysearch) + 3);
			sprintf(p, "%s%c%s",copysearch,PATHSEP_CHAR,env);
			free (copysearch);
			copysearch = p;
		}
	}
	k = 0;
	cmd[0] = fileindef[0] = indef[0] = outdef[0] = 0;
	inkeys[0] = outkeys[0] = 0;
	memset (flin,  0, sizeof(cob_file));
	memset (flout, 0, sizeof(cob_file));
	flin->record = makeField (65500);
	flout->record = makeField (65500);
	while (getLine (buf)) {
		if (strcasecmp (buf, "quit;") == 0)
			break;
		if (k > 0)
			cmd[k++] = ' ';
		if ((strlen(buf) + k) >= (sizeof(cmd)-1)) {
			printf("Command is too long\n");
			exit (-1);
			break;
		}
		strcpy (&cmd[k], buf);
		k = strlen(cmd);
		skip = ncopy = 0;
		if (cmd[k-1] == ';') {
			cmd[k-1] = ' ';
			if (strncasecmp (cmd,"INPUT ",6) == 0) {
				strcpy (indef, cmd+6);
				flin->flag_auto_type = 1;
				parseFile (flin, "INPUT", 256, indef, copyin, inkeys);
			} else if (strncasecmp (cmd,"OUTPUT ",7) == 0) {
				strcpy (outdef, cmd+7);
				parseFile (flout, "OUTPUT", flin->record_max, outdef, copyout, outkeys);
			} else if (strncasecmp (cmd,"COPY ",5) == 0) {
				if (indef[0] < ' ') {
					printf("INPUT file is not defined\n");
					continue;
				}
				if (outdef[0] < ' ') {
					printf("OUTPUT file is not defined\n");
					continue;
				}
				for (k=5; cmd[k] != 0; k++) {
					if (isspace(cmd[k-1])) {
						if (matchWord ("SKIP=", cmd, val, &k)) {
							skip = atoi (val);
						} else
						if (matchWord ("COPY=", cmd, val, &k)) {
							ncopy = atoi (val);
						}
					}
				}
				trim_line (fileindef);
				trim_line (outdef);
				printf("COPY %s\n     '%s'\n",flin->assign->data,fileindef);
				printf("  TO %s\n     '%s'\n",flout->assign->data,outdef);
				copyFile (flin, flout, skip, ncopy);
				cmd[0] = fileindef[0] = indef[0] = outdef[0] = 0;
			} else if (strncasecmp (cmd,"GEN ",4) == 0
					|| strncasecmp (cmd,"RUN ",4) == 0) {
				int runit = 0;
				if (strncasecmp (cmd,"RUN ",4) == 0)
					runit = 1;
				for (k=4; cmd[k] != 0; k++) {
					if (isspace(cmd[k-1])) {
						if (matchWord ("SKIPIF[", cmd, val, &k)) {
							j = sprintf(skipif," \tIF %s\n",val);
							j += sprintf(&skipif[j]," \t\tGO TO GET-NEXT-100.\n");
						} else
						if (matchWord ("SKIP=", cmd, val, &k)) {
							sprintf (skipnum,"%d",atoi (val));
						} else
						if (matchWord ("COBC[", cmd, val, &k)) {
							strcpy(cobcopts,val);
						} else
						if (matchWord ("COPY=", cmd, val, &k)) {
							sprintf (copynum,"%d",atoi (val));
						} else
						if (matchWord ("PROGRAMID=", cmd, val, &k)) {
							strcpy(progid,val);
						}
					}
				}
				for (k=0; progid[k] != 0; k++)
					proglwr[k] = tolower(progid[k]);
				proglwr[k] = 0;
				sprintf(cblout,"%s.cbl",proglwr);
				fo = fopen (cblout,"w");
				if (fo == NULL) {
					printf("Error %s opening '%s' output\n",strerror(errno),cblout);
					exit(-1);
				}
				genCopy (flin, flout);
				fclose(fo);

				/* CHECKME: maybe execute directly without script
				            if 'runit' is set; to do so 'setenv' the values here,
				            then call cobc with additional '-j' option */
				sprintf (cmd,"cobc -x %s %s",cobcopts,cblout);
				if (!be_quiet)
					printf("Compiling: %s\n",cmd);
				k = system (cmd);
				if (k != 0) {
					printf("Compile of %s; Error status %d\n",cmd,k);
				} else {
					if (!be_quiet)
						printf("Compile of %s; completed\n",cblout);
					sprintf(cblout,"%s.sh",proglwr);
					fo = fopen (cblout,"w");
					if (fo == NULL) {
						printf("Error %s opening '%s' output\n",strerror(errno),cblout);
						exit(-1);
					}
					/* TODO: add variant building and executing .cmd for WIN32 */
					fprintf(fo,"#!/bin/sh\n");
					fprintf(fo,"export DD_INFILE=%s\n",flin->assign->data);
					fprintf(fo,"export IO_INFILE=\"keycheck=no %s\"\n",indef);
					fprintf(fo,"export DD_OUTFILE=%s\n",flout->assign->data);
					fprintf(fo,"export IO_OUTFILE=\"%s\"\n",outdef);
					fprintf(fo,"%s\n",proglwr);
					fclose(fo);
					sprintf (cmd,"chmod a+x %s",cblout);
					k = system (cmd);
					if (runit && k == 0) {
						printf("Executing %s;\n",cblout);
						sprintf (cmd,"./%s",cblout);
						k = system (cmd);
						if (k)
							printf("Execution of %s; Error status %d\n",cmd,k);
					} else {
						if (!be_quiet)
							printf("Script %s; ready\n",cblout);
					}
				}
				cmd[0] = fileindef[0] = indef[0] = outdef[0] = 0;
				inkeys[0] = outkeys[0] = 0;
				if (runit && !keep_code) {
					sprintf(cblout,"%s.cbl",proglwr);
					unlink (cblout);
					sprintf(cblout,"%s.sh",proglwr);
					unlink (cblout);
					unlink (proglwr);
				}
			} else if (strncasecmp (cmd,"QUIT ",5) == 0) {
				break;
			} else if (strncasecmp (cmd,"EXIT ",5) == 0) {
				break;
			} else {
				if (strncasecmp (cmd,"HELP ",5) != 0)
					printf("Unknown command [%s]\n",cmd);
				if (!batchin) {
					gcd_usage((char*)"cobfile");
					printf("To exit enter:  quit;\n");
				}
			}
			k = 0;
		}
	}

	exit (0);
	return 0;
}

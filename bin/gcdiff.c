/*
   Copyright (C) 2017, 2020 Free Software Foundation, Inc.
   Written by Ron Norman, Simon Sobisch

   This file is part of GnuCOBOL.

   The GnuCOBOL diff helper program is free software: you can redistribute it
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
     Program:  gcdiff.c

     Function: This program is used to compare GnuCOBOL test results 
               and handle expected difference such as Date/Time
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
#include	"tarstamp.h"
#include	"libcob/cobgetopt.h"

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

/* Support for gcdiff from stdin */
#define GCD_DASH			"-"

static	char	ign_char = '~';		/* This 'char' in reference file ignores same byte position in test file */
static	int		ign_spaces = 0;		/* If '1' then all spaces are ignored */
/* If '1' then trailing spaces are kept and if last char is 'underscore' it is removed */
static	int		keep_trailing_spaces = 0;	
static	int		be_quiet = 0;		/* Be less wordy */
static	int		unify = 0;			/* Display changes 'unify' style */
static	int		time_tol = (60*5);	/* Times need to be this close */
static	struct stat st_ref;
static	struct stat st_test;
static	time_t	nowis;
static	char	referencefile[256] = "";
static	char	testfile[256] = "";

static const char short_options[] = "hqwuUVr:t:C:e:I:f:T:x:v:";

#define	CB_NO_ARG	no_argument
#define	CB_RQ_ARG	required_argument
#define	CB_OP_ARG	optional_argument

static const struct option long_options[] = {
	{"help",		CB_NO_ARG, NULL, 'h'},
	{"quiet",		CB_NO_ARG, NULL, 'q'},
	{"spaces",		CB_NO_ARG, NULL, 'w'},
	{"ref",			CB_RQ_ARG, NULL, 'r'},
	{"test",		CB_RQ_ARG, NULL, 't'},
	{"ign-char",	CB_RQ_ARG, NULL, 'C'},
	{"ignore",		CB_RQ_ARG, NULL, 'e'},
	{"skip",		CB_RQ_ARG, NULL, 'I'},
	{"file-time",	CB_RQ_ARG, NULL, 'f'},
	{"verify-time",	CB_RQ_ARG, NULL, 'v'},
	{"current-time",CB_RQ_ARG, NULL, 'T'},
	{"tolerance",	CB_RQ_ARG, NULL, 'x'},
	{"version",     CB_NO_ARG, NULL, 'V'},
	{"unified",     CB_NO_ARG, NULL, 'u'},
	{"keep-spaces",	CB_NO_ARG, NULL, 'U'},
	{NULL, 0, NULL, 0}
};


#define MAX_TEMPLATES 64
static struct template_t {
	short	len;				/* Length of 'pat' */
	short	is_num;				/* Alpha letters are really Digits */
	enum {
		NOT_TIME = 0,
		MODIFY_TIME  = 1,		/* Compare to 'testfile' modification time */
		CURRENT_TIME = 2,		/* Compare to current time */
		VERIFY_TIME  = 3,		/* Just verify reasonable value */
		IS_DAY  = 4,			/* 'dd' matches ' #' or '##'  less than 32 */
		IS_VER  = 5				/* Compiler version pattern */
	} is_time;					/* Reconstruct and verify date/time */
	char	*pat;
} templates[MAX_TEMPLATES] = {
	{24,1,VERIFY_TIME,	(char*)"DDD MMM dd HH:MM:SS YYYY"},
	{20,1,MODIFY_TIME,	(char*)"MMM DD YYYY HH:MI:SS"},
	{20,1,CURRENT_TIME,	(char*)"MMM DD YYYY HH-MI-SS"},
	{20,1,VERIFY_TIME,	(char*)"MMM DD YYYY HH:MM:SS"},
	{19,1,VERIFY_TIME,	(char*)"YYYY/MM/DD HH:MI:SS"},
	{10,1,NOT_TIME,		(char*)"YYYY/MM/DD"},
	{ 8,1,NOT_TIME,		(char*)"HH:MM:SS"},
	{ 8,1,CURRENT_TIME,	(char*)"HH:MI:SS"},
	{ 8,1,NOT_TIME,		(char*)"YY/MM/DD"},
	{ 7,0,IS_VER,		(char*)" V.R.P "},
	{ 5,1,NOT_TIME,		(char*)"HH:MM"},
	{ 5,1,CURRENT_TIME,	(char*)"HH:MI"},
	{ 4,1,NOT_TIME,		(char*)"YYYY"},
	{ 4,1,IS_DAY,		(char*)" dd "},
	{ 3,0,NOT_TIME,		(char*)"MMM"},
	{ 3,0,NOT_TIME,		(char*)"DDD"},
	{-1,0,NOT_TIME,(char*)0}
};

static const char *days[7] =
						{"Sunday",  "Monday","Tuesday","Wednesday",
						 "Thursday","Friday","Saturday"};
static const char *months[12] = 
						{"January",  "February","March",   "April",
						 "May",      "June",    "July",    "August",
						 "September","October", "November","December"};

#define MAX_SKIP 64
static struct {
	short	len;
	char	*pat;
} skip_lines[MAX_TEMPLATES];

/* Bubble sort Templates for longest first */
static void
sort_templates()
{
	int		i,j;
	struct template_t tt;

	i = 1;
	while ( i ) {
		i = 0;
		for (j=0; templates[j].len != -1; j++) {
			if (templates[j+1].len != -1
			 && templates[j].len < templates[j+1].len) {
				i = 1;		/* Swap is being done */
				memcpy(&tt, &templates[j], sizeof(struct template_t));
				memcpy(&templates[j], &templates[j+1],sizeof(struct template_t));
				memcpy(&templates[j+1], &tt, sizeof(struct template_t));
			}
		}
	}
}

/* Add (or update) template string */
static void
add_template(char *string, int num, int istime)
{
	int		i, len;
	len = strlen(string);
	for (i=0; i < MAX_TEMPLATES-1; i++) {
		if (templates[i].len == -1) {
			templates[i].pat = strdup(string);
			templates[i].len = len;
			templates[i].is_num = num;
			templates[i].is_time = istime;
			sort_templates();
			break;
		}
		if (strcmp(templates[i].pat,string) == 0) {
			templates[i].is_num = num;
			templates[i].is_time = istime;
			break;
		}
	}
}

static void
print_template(const char *opt, const char *what, int type)
{
	int		i,k;
	const char	*nl = "";

	k = 100;
	for (i=0; i < MAX_TEMPLATES-1 && templates[i].len > 0; i++) {
		if (templates[i].is_time == type) {
			if (k + templates[i].len > 58) {
				printf("%s%16s %s : ",nl,what,opt);
				k = 0;
			}
			if (k > 0)
				printf(", ");
			k += templates[i].len + 4;
			printf("%.*s",templates[i].len,templates[i].pat);
			what = " ";
			opt = "  ";
			nl = "\n";
		}
	}
	if (k > 0)
		putchar('\n');
}

static int
num_val( char *s, int len)
{
	int		i, val;
	for (i=val=0; i < len; i++) {
		if (isdigit(*s))
			val = val * 10 + (*s - '0');
		s++;
	}
	return val;
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

	printf ("gcdiff (%s) %s.%d\n",
		PACKAGE_NAME, PACKAGE_VERSION, PATCH_LEVEL);
	puts ("Copyright (C) 2017 Free Software Foundation, Inc.");
	puts (_("License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>"));
	puts (_("This is free software; see the source for copying conditions.  There is NO\n"
		"warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE."));
	printf (_("Written by %s"), "Ron Norman, Simon Sobisch");
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
gcd_usage (char *prog, char * referencefile)
{
	int		i, k;

	puts (_("Compare GnuCOBOL test case files"));
	putchar ('\n');
	printf (_("usage: %s [options] referencefile testfile"), prog);
	putchar ('\n');
	putchar ('\n');
	puts (_("Options:"));
	printf (_("  -C x           character 'x' indicates ignore"));
	if (ign_char) {
		printf ("; %s: '%c'", _("default"), ign_char);
	}
	putchar('\n');
	puts (_("  -e STR         string STR is ignored"));
	puts (_("  -n STR         string STR is ignored; alpha chars are DIGITS in testfile"));
	puts (_("  -f STR         STR is date/time pattern; date/time in testfile must be\n"
	        "                     close to modification time of testfile"));
	puts (_("  -T STR         STR is date/time pattern; date/time in testfile must be\n"
	        "                     close to current time of day"));
	puts (_("  -v STR         STR is date/time pattern; verify date/time in testfile"));
	puts (_("  -I STR         if STR is on line of referencefile, ignore complete line"));
	printf (_("  -x secs        seconds of difference allowed in time compare; default: %d"),time_tol);
	putchar ('\n');
	puts (_("  -w             ignore all spaces"));
	puts (_("  -U             Keep trailing spaces (remove underscore)"));
	puts (_("  -h, -help      display this help and exit"));
	puts (_("  -V, -version   display version and exit"));
	putchar ('\n');
	puts (_("  referencefile  base text file (reference case) to compare with"));
	puts (_("  testfile       text file created by the test case to be compared"));
	if (referencefile) {
		sort_templates();
		putchar ('\n');
		printf (_("patterns looked for in  '%s'"),
						referencefile[0] > ' '?referencefile:"referencefile");
		putchar ('\n');
		print_template ("-T","current time",CURRENT_TIME);
		print_template ("-v","verify time",VERIFY_TIME);
		print_template ("-f","'testfile' time",MODIFY_TIME);
		print_template ("-e","just ignore",NOT_TIME);
		if (skip_lines[0].len > 0) {
			putchar ('\n');
			puts (_("default strings to cause line to be ignored"));
			for (i=k=0; i < MAX_SKIP-1 && skip_lines[i].len > 0; i++) {
				k += skip_lines[i].len;
				if (k + skip_lines[i].len > 70) {
					putchar('\n');
					k = 0;
				}
				printf("%.*s  ",skip_lines[i].len,skip_lines[i].pat);
			}
			putchar ('\n');
		}
	}
	putchar ('\n');
	printf (_("Report bugs to: %s or\n"
		"use the preferred issue tracker via home page"), "bug-gnucobol@gnu.org");
	putchar ('\n');
	puts (_("GnuCOBOL home page: <http://www.gnu.org/software/gnucobol/>"));
	puts (_("General help using GNU software: <http://www.gnu.org/gethelp/>"));
}

static int
get_hex(char *buf, long *val)
{
	long	tval = 0;
	int		i;
	for (i=0; buf[i] != 0; i++) {
		if (buf[i] >= '0'
		 && buf[i] <= '9') {
			tval = tval << 4 + (buf[i] - '0');
		} else
		if (buf[i] >= 'a'
		 && buf[i] <= 'f') {
			tval = tval << 4 + (buf[i] - 'a' + 10);
		} else
		if (buf[i] >= 'A'
		 && buf[i] <= 'F') {
			tval = tval << 4 + (buf[i] - 'A' + 10);
		} else {
			break;
		}
	}
	*val = tval;
	return i;
}

static int
trim_line(char *buf)
{
	int	k;
	k = strlen(buf);
	while (k > 0
		&& (buf[k-1] == '\r' || buf[k-1] == '\n')) {
		buf[--k] = 0;
	}
	if (keep_trailing_spaces) {	/* Keep trailing spaces */
		if (buf[k-1] == '_')	/* But remove any underscore */
			buf[--k] = 0;
	} else {
		while (k > 0
			&& buf[k-1] == ' ') {
			buf[--k] = 0;
		}
	}
	return k;
}

/*
 * Compare 'ref' to 'rslt'
 */
static int
compare_file(FILE *ref, FILE *rslt, FILE *rpt)
{
	char	rbuf[4096], nbuf[4096];
	const char *tagout, *tagin;
	int		i, j, k, n, t, val, numdiff, linenum;
	int		nx, rx;
	long	nval, rval;
	struct tm tval, *ptm;
	time_t	time_sec, time_diff;

	sort_templates();
	if (ref == NULL
	 || ferror(ref)
	 || feof(ref))
		return 2;
	if (rslt == NULL
	 || ferror(rslt)
	 || feof(rslt))
		return 2;
	if (unify) {
		tagout = "-";
		tagin  = "+";
	} else {
		tagout = "< ";
		tagin  = "> ";
	}
	linenum = numdiff = 0;
	memset(nbuf,0,sizeof(nbuf));
	memset(rbuf,0,sizeof(rbuf));
	while (fgets(rbuf,sizeof(rbuf),ref) != NULL) {
		k = trim_line (rbuf);
		if (fgets(nbuf,sizeof(nbuf),rslt) == NULL)
			break;
		j = trim_line (nbuf);
		linenum++;

		for (t=0; skip_lines[t].len > 0; t++) {
			if (strstr(rbuf, skip_lines[t].pat) != NULL)	/* Is string on the line */
				break;
		}
		if (skip_lines[t].len > 0)			/* Ignore complete line */
			continue;
		for (i=j=0; i < k; i++,j++) {
			if (ign_spaces) {
				 while (rbuf[i] == ' ' && i < k) i++;
				 while (nbuf[j] == ' ' && j < k) j++;
			}
			for (t=0; templates[t].len > 0; t++) {
				if (memcmp(templates[t].pat, &rbuf[i], templates[t].len) == 0)
					break;
			}
			if (templates[t].len > 0) {
				if (templates[t].is_time == IS_DAY) {	/*  " dd " */
					if (num_val(&nbuf[i+1], 2) <= 31) {
						i += templates[t].len - 1;
						j += templates[t].len - 1;
						continue;
					}
					goto mis_match;
				}
				if (templates[t].is_time == NOT_TIME) {
					i += templates[t].len - 1;
					j += templates[t].len - 1;
					continue;
				}
				if (templates[t].is_time == IS_VER) {	/* Version pattern */
					i += templates[t].len - 1;
					if(nbuf[j] == ' ') j++;
					if(!isdigit(nbuf[j]))
						goto mis_match;
					while (nbuf[j] != ' '				/* Skip past version stamp */
						&& nbuf[j] != ','
						&& nbuf[j] != 0)
						j++;
					continue;
				}
				if (templates[t].is_time == MODIFY_TIME
				 || templates[t].is_time == CURRENT_TIME
				 || templates[t].is_time == VERIFY_TIME) {	/* Valid date/time expected */
					ptm = localtime(&nowis);
					memcpy(&tval, (void*)ptm, sizeof(struct tm));

					for (n=0; n < templates[t].len; i++,j++,n++) {
						if (rbuf[i] == nbuf[j])
							continue;
						if (memcmp(&rbuf[i],"YYYY",4) == 0) {
							tval.tm_year = num_val (&nbuf[i], 4) - 1900;
							i+=3,j+=3,n+=3;
						} else if (memcmp(&rbuf[i],"YY",2) == 0) {
							tval.tm_year = num_val (&nbuf[i], 2);
							if (tval.tm_year < 70)
								tval.tm_year += 100;	/* 20yy */
							i++,j++,n++;
						} else if (memcmp(&rbuf[i],"MMM",3) == 0) {
							tval.tm_mon = -1;
							for (val = 0; val < 12; val++) {
								if (strncasecmp(&nbuf[i],months[val],3) == 0) {
									tval.tm_mon = val;
									break;
								}
							}
							i+=2,j+=2,n+=2;
						} else if (memcmp(&rbuf[i],"MM",2) == 0) {
							tval.tm_mon  = num_val (&nbuf[i], 2) - 1;
							i++,j++,n++;
						} else if (memcmp(&rbuf[i],"DDD",3) == 0) {
							tval.tm_wday = -1;
							for (val = 0; val < 7; val++) {
								if (strncasecmp(&nbuf[i],days[val],3) == 0) {
									tval.tm_wday = val;
									break;
								}
							}
						} else if (memcmp(&rbuf[i],"DD",2) == 0) {
							tval.tm_mday = num_val (&nbuf[i], 2);
							i++,j++,n++;
						} else if (memcmp(&rbuf[i],"dd",2) == 0) {
							tval.tm_mday = num_val (&nbuf[i], 2);
							i++,j++,n++;
						} else if (memcmp(&rbuf[i],"HH",2) == 0) {
							tval.tm_hour = num_val (&nbuf[i], 2);
							i++,j++,n++;
						} else if (memcmp(&rbuf[i],"MI",2) == 0) {
							tval.tm_min  = num_val (&nbuf[i], 2);
							i++,j++,n++;
						} else if (memcmp(&rbuf[i],"SS",2) == 0) {
							tval.tm_sec  = num_val (&nbuf[i], 2);
							i++,j++,n++;
						}
					}
					time_sec = mktime(&tval);
					if(templates[t].is_time == CURRENT_TIME) {
						if (time_sec < nowis)
							time_diff = nowis - time_sec;
						else
							time_diff = time_sec - nowis;
						if (time_diff > time_tol) {
							printf(_("Time: %04d/%02d/%02d %02d:%02d:%02d too far off current time"),
									tval.tm_year+1900, tval.tm_mon+1, tval.tm_mday,
									tval.tm_hour, tval.tm_min, tval.tm_sec);
							printf(" %04d/%02d/%02d %02d:%02d:%02d\n",
									ptm->tm_year+1900, ptm->tm_mon+1, ptm->tm_mday,
									ptm->tm_hour, ptm->tm_min, ptm->tm_sec);
							goto mis_match;
						}
					} else
					if(templates[t].is_time == MODIFY_TIME) {
						if (time_sec < st_test.st_mtime)
							time_diff = st_test.st_mtime - time_sec;
						else
							time_diff = time_sec - st_test.st_mtime;
						if (time_diff > time_tol) {
							ptm = localtime(&st_test.st_mtime);
							printf(_("Time: %04d/%02d/%02d %02d:%02d:%02d too far off file time"),
									tval.tm_year+1900, tval.tm_mon+1, tval.tm_mday,
									tval.tm_hour, tval.tm_min, tval.tm_sec);
							printf(" %04d/%02d/%02d %02d:%02d:%02d\n",
									ptm->tm_year+1900, ptm->tm_mon+1, ptm->tm_mday,
									ptm->tm_hour, ptm->tm_min, ptm->tm_sec);
							goto mis_match;
						}
					} else
					if(templates[t].is_time == VERIFY_TIME) {
						if (tval.tm_mon < 0
						 || tval.tm_mon > 11) {
							printf(_("Time: %04d/%02d/%02d %02d:%02d:%02d has invalid month"),
									tval.tm_year+1900, tval.tm_mon+1, tval.tm_mday,
									tval.tm_hour, tval.tm_min, tval.tm_sec);
							putchar ('\n');
							goto mis_match;
						}
						if (tval.tm_mday < 1
						 || tval.tm_mday > 31) {
							printf(_("Time: %04d/%02d/%02d %02d:%02d:%02d has invalid day"),
									tval.tm_year+1900, tval.tm_mon+1, tval.tm_mday,
									tval.tm_hour, tval.tm_min, tval.tm_sec);
							putchar ('\n');
							goto mis_match;
						}
						if (tval.tm_hour < 0
						 || tval.tm_hour > 24) {
							printf(_("Time: %04d/%02d/%02d %02d:%02d:%02d has invalid hour"),
									tval.tm_year+1900, tval.tm_mon+1, tval.tm_mday,
									tval.tm_hour, tval.tm_min, tval.tm_sec);
							putchar ('\n');
							goto mis_match;
						}
						if (tval.tm_min < 0
						 || tval.tm_min > 60) {
							printf(_("Time: %04d/%02d/%02d %02d:%02d:%02d has invalid minutes"),
									tval.tm_year+1900, tval.tm_mon+1, tval.tm_mday,
									tval.tm_hour, tval.tm_min, tval.tm_sec);
							putchar ('\n');
							goto mis_match;
						}
						if (tval.tm_sec < 0
						 || tval.tm_sec > 60) {
							printf(_("Time: %04d/%02d/%02d %02d:%02d:%02d has invalid seconds"),
									tval.tm_year+1900, tval.tm_mon+1, tval.tm_mday,
									tval.tm_hour, tval.tm_min, tval.tm_sec);
							putchar ('\n');
							goto mis_match;
						}
					}
					i += templates[t].len;
					j += templates[t].len;
				} else 
				if (templates[t].is_num) {	/* Numeric data expected */
					for (n=0; n < templates[t].len; i++,j++,n++) {
						if (rbuf[i] == nbuf[j]
						 || nbuf[j] == ' ')
							continue;
						if (!isdigit(nbuf[j])) {
							while (n < templates[t].len-1)
								i++,j++,n++;
							goto mis_match;
						}
					}
				} else {
					i += templates[t].len;
					j += templates[t].len;
				}
				i--; j--;
				continue;
			}
			if (memcmp(&rbuf[i-1]," 0x",3) == 0
			 && memcmp(&nbuf[j-1]," 0x",3) == 0) {	/* Hex value; collect and compare */
				rx = get_hex (&rbuf[i+2], &rval);
				nx = get_hex (&nbuf[j+2], &nval);
				if (nval == rval) {					/* Hex values match */
					i = i + rx + 1;
					j = j + nx + 1;
				}
			}
			if (rbuf[i] == nbuf[j])
				continue;
			if (rbuf[i] == ign_char)
				continue;
mis_match:
			numdiff++;
			if (!be_quiet) {
				if (unify) {
					fprintf(rpt,"@@ %d @@\n",linenum);
					fprintf(rpt,"%s%s\n",tagout,rbuf);
					fprintf(rpt,"%s%s\n",tagin,nbuf);
				} else {
					fprintf(rpt,"%dc%d\n",linenum,linenum);
					fprintf(rpt,"%s%s\n",tagout,rbuf);
					fprintf(rpt,"---\n");
					fprintf(rpt,"%s%s\n",tagin,nbuf);
				}
			}
			break;
		}
	}
	k = 0;
	if (!feof(rslt) ) {
		if(fgets(nbuf,sizeof(nbuf),rslt) != NULL) {
			numdiff++;
			j = trim_line (nbuf);
			fprintf(rpt,"%da\n",linenum);
			fprintf(rpt,"%s%s\n",tagin,nbuf);
			k++;
		}
	}
	if (feof(ref)
	 && feof(rslt))
		return 0;
	if (!feof(rslt) 
	 && !be_quiet) {
		while (fgets(nbuf,sizeof(nbuf),rslt) != NULL) {
			j = trim_line (nbuf);
			if (k == 0)
				fprintf(rpt,"%da\n",linenum);
			fprintf(rpt,"%s%s\n",tagin,nbuf);
			k++;
			numdiff++;
		}
	}
	if (numdiff > 0)
		return 1;
	return 1;
}

static void
set_option (char *binary, int opt, char *arg)
{
	int		i;
	switch(opt) {
	case 'w':
		ign_spaces = 1;
		break;
	case 'q':
		be_quiet = 1;
		break;
	case 'u':
		unify = 1;
		break;
	case 'U':
		keep_trailing_spaces = 1;
		break;
	case 'C':
		ign_char = arg[0];
		break;
	case 'r':
		strcpy(referencefile,arg);
		break;
	case 't':
		strcpy(testfile,arg);
		break;

	case 'e':		/* Ignore this 'string' */
		add_template(arg, 0, NOT_TIME);
		break;

	case 'n':		/* Ignore this 'string', ALPHA chars are really digits */
		add_template(arg, 1, NOT_TIME);
		break;

	case 'f':		/* Check date/time in testfile against testfile modification time */
		add_template(arg, 1, MODIFY_TIME);
		break;

	case 'v':		/* Verify valid date/time in testfile */
		add_template(arg, 1, VERIFY_TIME);
		break;

	case 'x':		/* Set date/time tolerance in seconds */
		time_tol = (int)atol(arg);
		break;

	case 'T':		/* Check date/time in testfile against current time */
		add_template(arg, 1, CURRENT_TIME);
		break;

	case 'I':		/* Ignore complete line based on given 'string' */
		for (i=0; i < MAX_SKIP-1; i++) {
			if (skip_lines[i].len == -1) {
				skip_lines[i].pat = strdup(arg);
				skip_lines[i].len = strlen(arg);
				break;
			}
		}
		break;

	case '?':
	default:
		printf(_("unknown parameter '%c' for %s"),opt,binary);
		putchar ('\n');
		gcd_usage((char*)"gcdiff", NULL);
		exit(2);
		break;

	case 'h':
		gcd_usage((char*)"gcdiff", referencefile);
		exit(2);
		break;

	case 'V':
		gcd_print_version ();
		exit(2);
		break;
	}
}

/*
 * M A I N L I N E   Starts here
 */
int
main(
	int		argc,
	char	*argv[])
{
	int		opt,idx,i,k;
	FILE	*ref,*rslt;
	char	buf[1024];

#ifdef	HAVE_SETLOCALE
	setlocale (LC_ALL, "");
#endif
	for (i=0; i < MAX_TEMPLATES; i++) {
		if (templates[i].len == -1) {
			while (i < MAX_TEMPLATES) {
				templates[i].len = -1;
				templates[i].pat = NULL;
				i++;
			}
		} else if (templates[i].pat != NULL) {
			templates[i].len = strlen(templates[i].pat);
		}
	}
	for (i=0; i < MAX_SKIP; i++) {
		skip_lines[i].len = -1;
		skip_lines[i].pat = NULL;
	}
	memset(referencefile,0,sizeof(referencefile));
	memset(testfile,0,sizeof(testfile));

	/* Process gcdiff.conf from current directory */
	ref = fopen("gcdiff.conf","r");
	if(ref == NULL) {
		/* Check for gcdiff.conf in config directory */
		sprintf(testfile,"%s/gcdiff.conf",COB_CONFIG_DIR);
		ref = fopen("gcdiff.conf","r");
	}
	if(ref) {
		while (fgets(buf,sizeof(buf),ref) != NULL) {
			k = trim_line (buf);
			if (buf[0] == '-') {	/* Option for gcdiff ?*/
				opt = buf[1];
				for (i=2; isspace(buf[i]); i++);
				set_option((char*)"gcdiff.conf",opt,&buf[i]);
			}
		}
		fclose(ref);
	}
	memset(referencefile,0,sizeof(referencefile));
	memset(testfile,0,sizeof(testfile));

	idx = 0;
	cob_optind = 1;
	while ((opt = cob_getopt_long_long (argc, argv, short_options,
					  long_options, &idx, 1)) >= 0) {
		set_option(argv[0], opt, cob_optarg);
	}

	if (cob_optind < argc
	 && referencefile[0] <= ' ') {
		strcpy(referencefile,argv[cob_optind++]);
	}
	if (cob_optind < argc
	 && testfile[0] <= ' ') {
		strcpy(testfile,argv[cob_optind++]);
	}
	if (referencefile[0] <= ' ') {
		puts (_("missing 'referencefile'"));
		putchar ('\n');
		gcd_usage(argv[0], NULL);
		exit(2);
	}
	if (testfile[0] <= ' ') {
		puts (_("missing 'testfile'"));
		putchar ('\n');
		gcd_usage(argv[0], NULL);
		exit(2);
	}

	sort_templates();
	time(&nowis);

	if (strcmp(referencefile, GCD_DASH) == 0) {
		ref = stdin;
		st_ref.st_atime = nowis;
		st_ref.st_ctime = nowis;
		st_ref.st_mtime = nowis;
	} else {
		stat (referencefile, &st_ref);
		ref = fopen(referencefile,"r");
	}
	if (ref == NULL) {
		perror(referencefile);
		exit(2);
	}
	if (strcmp(testfile, GCD_DASH) == 0) {
		rslt = stdin;
		st_test.st_atime = nowis;
		st_test.st_ctime = nowis;
		st_test.st_mtime = nowis;
	} else {
		stat (testfile, &st_test);
		rslt = fopen(testfile,"r");
	}
	if (rslt == NULL) {
		perror(testfile);
		exit(2);
	}
	k = compare_file (ref, rslt, stdout);
	if (ref != stdin)
		fclose(ref);
	if (rslt != stdin)
		fclose(rslt);
	exit(k);
	return k;
}

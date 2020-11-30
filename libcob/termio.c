/*
   Copyright (C) 2001-2012, 2014-2020 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Simon Sobisch, Edward Hart

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


#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include <errno.h>
#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <time.h>

/* Force symbol exports */
#define	COB_LIB_EXPIMP
#include "libcob.h"
#include "coblocal.h"

/* Local variables */

static cob_global		*cobglobptr = NULL;
static cob_settings		*cobsetptr = NULL;

static char no_syspunch_error_raised = 0;
#define MAX_PREV 256
static char dump_prev[MAX_PREV];
static int	dump_idx = -1;
static int	dump_skip = -1;
static int	dump_tbl = -1;

static const unsigned short	bin_digits[] =
	{ 1, 3, 5, 8, 10, 13, 15, 17, 20 };

static const cob_field_attr	const_alpha_attr =
				{COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};

/* DISPLAY */

static void
display_numeric (cob_field *f, FILE *fp)
{
	int		i;
	unsigned short	digits;
	signed short	scale;
	int		size;
	cob_field_attr	attr;
	cob_field	temp;

	digits = COB_FIELD_DIGITS (f);
	scale = COB_FIELD_SCALE (f);
	size = digits + (COB_FIELD_HAVE_SIGN (f) ? 1 : 0);
	if (size >= COB_MEDIUM_MAX) {
		fputs (_("(Not representable)"), fp);
		return;
	}
	COB_ATTR_INIT (COB_TYPE_NUMERIC_DISPLAY, digits, scale, 0, NULL);
	temp.size = size;
	temp.data = COB_TERM_BUFF;
	temp.attr = &attr;
	if (COB_FIELD_HAVE_SIGN (f)) {
		attr.flags = COB_FLAG_HAVE_SIGN | COB_FLAG_SIGN_SEPARATE;
		if (COB_FIELD_SIGN_LEADING (f) ||
		    COB_FIELD_TYPE (f) != COB_TYPE_NUMERIC_DISPLAY) {
			attr.flags |= COB_FLAG_SIGN_LEADING;
		}
	}

	cob_move (f, &temp);
	for (i = 0; i < size; ++i) {
		putc (temp.data[i], fp);
	}
}

static void
pretty_display_numeric (cob_field *f, FILE *fp)
{
	cob_pic_symbol	*p;
	unsigned char	*q = COB_TERM_BUFF;
	int		i;
	unsigned short	digits = COB_FIELD_DIGITS (f);
	signed short	scale = COB_FIELD_SCALE (f);
	int		size = digits + !!COB_FIELD_HAVE_SIGN (f) + !!scale;
	cob_field_attr	attr;
	cob_field	temp;
	cob_pic_symbol	pic[6] = {{ '\0' }};


	if (size > COB_MEDIUM_MAX) {
		fputs (_("(Not representable)"), fp);
		return;
	}
	temp.size = size;
	temp.data = q;
	temp.attr = &attr;
	COB_ATTR_INIT (COB_TYPE_NUMERIC_EDITED, digits, scale, 0,
		       (const cob_pic_symbol *)pic);
	p = pic;

	if (COB_FIELD_HAVE_SIGN (f)) {
		if (COB_FIELD_SIGN_SEPARATE (f)
		 && !COB_FIELD_SIGN_LEADING(f)) {
			/* done later */
		} else {
			p->symbol = '+';
			p->times_repeated = 1;
			++p;
		}
	}
	if (scale > 0) {
		if (digits - scale > 0) {
			p->symbol = '9';
			p->times_repeated = digits - scale;
			++p;
		}
		
		p->symbol = COB_MODULE_PTR->decimal_point;
		p->times_repeated = 1;
		++p;

		p->symbol = '9';
		p->times_repeated = scale;
		++p;
	} else {
		p->symbol = '9';
		p->times_repeated = digits;
		++p;
	}
	if (COB_FIELD_HAVE_SIGN (f)) {
		if (COB_FIELD_SIGN_SEPARATE (f)
		 && !COB_FIELD_SIGN_LEADING(f)) {
			p->symbol = '+';
			p->times_repeated = 1;
			++p;
		}
	}
	p->symbol = '\0';

	cob_move (f, &temp);
	for (i = 0; i < size; ++i) {
		if(q[i] != 0)
			putc (q[i], fp);
	}
}

static void
display_alnum (const cob_field *f, FILE *fp)
{
	size_t	i;

	for (i = 0; i < f->size; ++i) {
		putc (f->data[i], fp);
	}
}

/* Check for alternate styles of Not A Number and convert to just NaN
   and removes the leading zero from the Exponent
   note: not all environments provide display of negative /quiet NaN,
   some write data as 2.1212121E+37 while other use 2.1212121E+037 */
static void
clean_double (char *wrk)
{
	char *pos = strrchr (wrk, 'E');

	if (pos) {
		pos += 2; /* skip E+ */
		if (pos[0] == '0') {
			memmove (pos, pos + 1, strlen (pos));
		}
		return;
	}

	if (strcmp(wrk,"-NAN") == 0
	 || strcmp(wrk,"-NaNQ") == 0
	 || strcmp(wrk,"-NaN") == 0
	 || strcmp(wrk,"NAN") == 0
	 || strcmp(wrk,"NaNQ") == 0) {
		strcpy(wrk,"NaN");
	}
}

void
cob_display_common (const cob_field *f, FILE *fp)
{
	unsigned char	*p;
	union {
		double		f1doub;
		float		f1float;
	} un;
	int		n;
	char	wrk[48];

	if (f->size == 0) {
		return;
	}

	switch (COB_FIELD_TYPE (f)) {
	case COB_TYPE_NUMERIC_DOUBLE:
		memcpy (&un.f1doub, f->data, sizeof (double));
		sprintf (wrk, "%-.16G", un.f1doub);
		clean_double (wrk);
		fprintf (fp, "%s", wrk);
		return;
	case COB_TYPE_NUMERIC_FLOAT:
		memcpy (&un.f1float, f->data, sizeof (float));
		sprintf (wrk, "%-.8G", (double)un.f1float);
		clean_double (wrk);
		fprintf (fp, "%s", wrk);
		return;
	case COB_TYPE_NUMERIC_FP_DEC64:
	case COB_TYPE_NUMERIC_FP_DEC128:
		cob_print_ieeedec (f, fp);
		return;
	default:
		break;
	}
	if (COB_FIELD_IS_POINTER (f)) {
		fprintf (fp, "0x");
#ifdef	WORDS_BIGENDIAN
		p = f->data;
		for (n = 0; n < sizeof(void *); ++n, ++p) {
#else
		p = f->data + sizeof(void *) - 1;
		for (n = sizeof(void *) - 1; n >= 0; --n, --p) {
#endif
			fprintf (fp, "%x%x", *p >> 4, *p & 0xF);
		}
		return;
	} else if (COB_FIELD_TYPE(f) == COB_TYPE_NUMERIC_COMP5) {
		cob_print_realbin (f, fp, f->attr->digits);
		return;
	} else if (COB_FIELD_REAL_BINARY(f) 
			|| (COB_FIELD_TYPE(f) == COB_TYPE_NUMERIC_BINARY 
			 && !COB_MODULE_PTR->flag_pretty_display)) {
		cob_print_realbin (f, fp, bin_digits[f->size]);
		return;
	} else if (COB_FIELD_IS_NUMERIC (f)) {
		if (COB_MODULE_PTR->flag_pretty_display) {
			pretty_display_numeric ((cob_field *)f, fp);
		} else {
			display_numeric ((cob_field *)f, fp);
		}
		return;
	}
	display_alnum (f, fp);
}

void
cob_display (const int to_device, const int newline, const int varcnt, ...)
{
	FILE		*fp;
	cob_field	*f;
	int		i;
	int		nlattr, close_fp, pclose_fp;
	cob_u32_t	disp_redirect;
	va_list		args;
	const char *mode;

	disp_redirect = 0;
	pclose_fp = close_fp = 0;
	
	/* display to device ? */
	if (to_device == 2) {	/* PRINTER */
		/* display to external specified print file handle */
		if (cobsetptr->cob_display_print_file) {
			fp = cobsetptr->cob_display_print_file;
		/* display to configured print file */
		} else if (cobsetptr->cob_display_print_filename != NULL) {
			if (!cobsetptr->cob_unix_lf) {
				mode = "a";
			} else {
				mode = "ab";
			}
			fp = fopen (cobsetptr->cob_display_print_filename, mode);
			if (fp == NULL) {
				fp = stderr;
			} else {
				close_fp = 1;
			}
#ifdef HAVE_POPEN
		/* display to configured print command (piped) */
		} else if (cobsetptr->cob_display_print_pipe != NULL) {
			if (!cobsetptr->cob_unix_lf) {
				mode = "w";
			} else {
				/* Note: this doesn't seem to help with pipes :-( */
				mode = "wb";
			}
			fp = popen (cobsetptr->cob_display_print_pipe, mode);
			if (fp == NULL) {
				fp = stderr;
			} else {
				pclose_fp = 1;
			}
#endif
		/* fallback: display to the defined SYSOUT */
		} else {
			fp = stdout;
			if (cobglobptr->cob_screen_initialized) {
				if (!COB_DISP_TO_STDERR) {
					disp_redirect = 1;
				} else {
					fp = stderr;
				}
			}
		}
	} else if (to_device == 1) {	/* SYSERR */
		fp = stderr;
	} else if (to_device == 3) {	/* SYSPCH */
		/* open if not available but specified */
		if (!cobsetptr->cob_display_punch_file
		 && cobsetptr->cob_display_punch_filename != NULL) {
			if (!cobsetptr->cob_unix_lf) {
				mode = "w";
			} else {
				/* Note: this doesn't seem to help with pipes :-( */
				mode = "wb";
			}
			fp = fopen (cobsetptr->cob_display_punch_filename, mode);
			if (fp == NULL) {
				cob_runtime_warning (_("cannot open %s (=%s)"),
					"COB_DISPLAY_PUNCH_FILE", cobsetptr->cob_display_punch_filename);
				cob_free (cobsetptr->cob_display_punch_filename);
				cobsetptr->cob_display_punch_filename = NULL;
			} else {
				cobsetptr->cob_display_punch_file = fp;
			}
		}
		/* display to already opened punch file */
		if (cobsetptr->cob_display_punch_file) {
			fp = cobsetptr->cob_display_punch_file;
		} else {
			cob_set_exception (COB_EC_IMP_DISPLAY);	/* come back to this later... */
			if (!no_syspunch_error_raised) {
				no_syspunch_error_raised = 1;
				cob_runtime_warning (_("COB_DISPLAY_PUNCH_FILE is invalid, output to SYSPUNCH skipped"));
			}
			return;
		}
	} else {		/* general (SYSOUT) */
		fp = stdout;
		if (cobglobptr->cob_screen_initialized) {
			if (!COB_DISP_TO_STDERR) {
				disp_redirect = 1;
			} else {
				fp = stderr;
			}
		}
	}

	nlattr = newline ? COB_SCREEN_EMULATE_NL : 0;
	va_start (args, varcnt);
	for (i = 0; i < varcnt; ++i) {
		f = va_arg (args, cob_field *);
		if (unlikely (disp_redirect)) {
			cob_field_display (f, NULL, NULL, NULL, NULL,
					   NULL, NULL, nlattr);
		} else {
			cob_display_common (f, fp);
		}
	}
	va_end (args);

	if (newline && !disp_redirect) {
		putc ('\n', fp);
		fflush (fp);
	}
#ifdef HAVE_POPEN
	if (pclose_fp) {
		pclose (fp);
	}
#endif
	if (close_fp) {
		fclose (fp);
	}
}

static int
is_field_display (cob_field *f)
{
	size_t	i;
	for (i = 0; i < f->size; i++) {
		if (f->data[i] < ' '
		 || f->data[i] > 0x7F)
			return 0;
	}
	return 1;
}

static void
display_alnum_dump (cob_field *f, FILE *fp, unsigned int indent, unsigned int max_width)
{
	unsigned int	i, j, pos, lowv, highv, spacev, zerov, printv, delv, len, colsize;
	unsigned int	bgn, duplen;
	char	wrk[200], prev[MAX_PREV];

	lowv = highv = spacev = zerov = printv = delv = 0;
	colsize = max_width - indent - 2;
	for (i = 0; i < f->size; i++) {
		if (f->data[i] == 0x00) {
			lowv++;
			delv++;
		} else if (f->data[i] == 0xFF) {
			highv++;
		} else if (f->data[i] == ' ') {
			spacev++;
			printv++;
		} else if (f->data[i] == '0') {
			zerov++;
			printv++;
		} else
		if (f->data[i] >= ' '
		 && f->data[i] <= 0x7F
		 && isprint(f->data[i])) {
			printv++;
		} else
		if (f->data[i] == '\b'
		 || f->data[i] == '\f'
		 || f->data[i] == '\n'
		 || f->data[i] == '\r'
		 || f->data[i] == '\t') {
			delv++;
		}
	}

	if (spacev == f->size) {
		fprintf (fp, "ALL SPACES");
		return;
	}
	if (zerov == f->size) {
		fprintf (fp, "ALL ZEROES");
		return;
	}

	if (lowv == f->size) {
		fprintf (fp, "ALL LOW-VALUES");
		return;
	}
	if (highv == f->size) {
		fprintf (fp, "ALL HIGH-VALUES");
		return;
	}

	for (len = f->size; len > 0 && f->data[len-1] == ' '; len--);

	if (lowv > 0
	 && (lowv+printv) == f->size) {
		for (len = f->size; len > 0 && f->data[len-1] == 0x00; len--);
		if ((len+lowv) == f->size) {
			for (i=0; len > colsize; i+=colsize,len-=colsize) {
				fprintf(fp,"'%.*s'\n%*s",colsize,&f->data[i],indent," ");
			}
			if (len <= colsize) {
				fprintf(fp,"'%.*s'",len,&f->data[i]);
			}
			fprintf(fp,"\n%*s trailing LOW-VALUES",indent-8," ");
			return;
		}
	}

	if (printv == f->size) {
		duplen = bgn = 0;
		for (i=0; len > colsize; i+=colsize,len-=colsize) {
			if (colsize < MAX_PREV) {
				if (i == 0) {
					memcpy(prev, f->data, colsize);
				} else if (memcmp(prev, &f->data[i], colsize) == 0) {
					duplen = 0;
					bgn = i + 1;
					while(memcmp(prev, &f->data[i], colsize) == 0
						&& len > colsize) {
						duplen += colsize;
						i += colsize;
						len -= colsize;
					}
					i -= colsize;
					len += colsize;
					fprintf (fp, "%5u thru %u same as above\n%*s", 
								bgn, bgn+duplen, indent-6, " ");
					memcpy(prev, &f->data[i], colsize);
					continue;
				}
				memcpy(prev, &f->data[i], colsize);
			}
			if(i > 0)
				fprintf(fp,"%5u:",i);
			fprintf (fp, "'%.*s'\n%*s", colsize, &f->data[i], indent-6, " ");
		}
		if(i > 0)
			fprintf(fp,"%5u:",i);
		if (len <= colsize) {
			fprintf (fp, "'%.*s'", len, &f->data[i]);
			return;
		}
	}

	if ((delv + printv) == f->size) {
		for (i = 0; i < f->size; ) {
			for (j=0; j < colsize && i < f->size; j++,i++) {
				if (f->data[i] == '\0')
					fprintf(fp,"\\0"), j++;
				else if (f->data[i] == '\\')
					fprintf(fp,"\\\\"), j++;
				else if (f->data[i] == '\r')
					fprintf(fp,"\\r"), j++;
				else if (f->data[i] == '\n')
					fprintf(fp,"\\n"), j++;
				else if (f->data[i] == '\t')
					fprintf(fp,"\\t"), j++;
				else if (f->data[i] == '\b')
					fprintf(fp,"\\b"), j++;
				else if (f->data[i] == '\f')
					fprintf(fp,"\\f"), j++;
				else
					fprintf(fp,"%c",f->data[i]);
			}
			if (i < f->size) {
				fprintf (fp, "\n%*s%5u : ", indent - 8, " ", i + 1);
			}
		}
		return;
	}

	if (colsize > sizeof (wrk) - 1) {
		colsize = sizeof (wrk) - 1;
	}
	if (colsize > 9) {
		colsize = colsize / 9;
		colsize = colsize * 9;
	}

	colsize = colsize / 4;
	colsize = colsize * 4;
	for (i = 0; i < f->size; ) {
		if (colsize < MAX_PREV
		 && i < (f->size - colsize)) {
			if (i > 0
			 && memcmp(prev, &f->data[i], colsize/2) == 0) {
				duplen = 0;
				bgn = i;
				while(memcmp(prev, &f->data[i], colsize/2) == 0
					&& i < (f->size - colsize/2)) {
					duplen += colsize/2;
					i += colsize/2;
				}
				fprintf (fp, "--- %u thru %u same as above ---", bgn+1, bgn+duplen);
				if (i < f->size) {
					fprintf (fp, "\n%*s", indent, " ");
				}
			}
			memcpy(prev, &f->data[i], colsize);
		}
		wrk[0] = 0;
		pos = i + 1;
		for (j=0; j < colsize && i < f->size; j+=2,i++) {
			if (f->data[i] >= ' '
			 && f->data[i] <= 0x7F) {
				fprintf(fp," %c",f->data[i]);
				sprintf (&wrk[j],"%02X",f->data[i]);
			} else {
				fprintf(fp,"  ");
				sprintf (&wrk[j],"%02X",f->data[i]);
			}
			if ((j+2) < colsize
			 && ((i+1) % 4) == 0
			 && (i+1) < f->size) {
				fprintf(fp," ");
				j++;
				wrk[j+1] = ' ';
				wrk[j+2] = 0;
			}
		}
		fprintf (fp, "\n%*s%5u x %s", indent-8, " ", pos, wrk);
		if (i < f->size) {
			fprintf (fp, "\n%*s", indent, " ");
		}
	}
}

static int	dump_null_adrs = 0;

/* Output for DUMP purposes */
void
cob_dump_output (const char *str)
{
	FILE	*fp = cob_get_dump_file ();
	fprintf (fp, "\n%s\n**********************\n", str);
}

/* Output file header for DUMP purposes */
void
cob_dump_file (const char *name, cob_file *fl)
{
	FILE	*fp = cob_get_dump_file ();
	const char *mode;

	switch (fl->open_mode) {
	case COB_OPEN_CLOSED:
		mode = "CLOSED";
		break;
	case COB_OPEN_LOCKED:
		mode = "LOCKED";
		break;
	default:
		mode = "OPEN";
		break;
	}
	/* check only included for 3.1rc1 - compat */
	if (name) {
		fprintf (fp, "\n%s\n**********************\n", name);
	}
	fprintf (fp, "   File is %s\n", mode);
	fprintf (fp, "   FILE STATUS  '%.2s'\n", fl->file_status);
}

/* Output field for DUMP purposes */
void
cob_dump_field (const int level, const char *name, 
		cob_field *f_addr, const int offset, const int indexes, ...)
{
	FILE	*fp = cob_get_dump_file ();

	/* fieldname and additional for subscripts: " ()" + indexes (max-size 7 + ",") */
	char	vname[COB_MAX_WORDLEN + 1 + 3 + COB_MAX_SUBSCRIPTS * (7 + 1)];
	char	lvlwrk[16];
	int 	adjust, indent;
	cob_field	f[1];

	if (level < 0) {	/* Special directive, only included for 3.1rc1 - compat */
		if (level == -1) {
			cob_dump_output (name);
		} else
		if (level == -2) {
			cob_dump_file (NULL, (cob_file*)name);
		}
		return;
	}

	memcpy (f, f_addr, sizeof (cob_field));
	memcpy (vname, name, COB_MAX_WORDLEN);
	adjust = offset;

	if (indexes > 0) {
		va_list	ap;
		int	idx, subscript, size;
		strcat (vname, " (");
		va_start (ap, indexes);
		for (idx = 1; idx <= indexes; idx++) {
			subscript = va_arg (ap, int);
			size = va_arg (ap, int);
			adjust = adjust + (subscript * size);
			if (idx > 1) {
				strcat (vname,",");
			}
			sprintf (&vname[strlen(vname)], "%d", subscript + 1);
		}
		va_end (ap);
		strcat (vname,")");
		if (indexes == 1
		 && size >= f->size
		 && size < MAX_PREV) {
			if (subscript == 0) {
				dump_idx = subscript;
				dump_skip = subscript;
				if(dump_tbl < 0) {
					memcpy(dump_prev, f->data + adjust, size);
					dump_tbl = 1;
				} else {
					dump_tbl++;
				}
			} else if (subscript == dump_skip) {
				return;
			} else if (subscript != dump_idx) {
				if (memcmp(dump_prev, f->data + adjust, size) == 0) {
					fprintf (fp, "%-10s ... occurrence (%d) same as (%d)\n", 
									" ", subscript+1, dump_idx+1);
					dump_skip = subscript;
					return;
				} 
				memcpy(dump_prev, f->data + adjust, size);
				dump_idx = subscript;
				dump_skip = -1;
				dump_tbl = -1;
			}
		}
	} else {
		dump_idx = dump_tbl = dump_skip = -1;
	}
	if (f->data) {
		f->data += adjust;
	}
	if (level == 77
	 || level == 1) {
		indent = 0;
		sprintf(lvlwrk,"%02d",level);
		if (f->data != NULL) {
			dump_null_adrs = 0;
		} else {
			dump_null_adrs = 1;
			if (COB_FIELD_TYPE (f) == COB_TYPE_GROUP) {
				strcat (vname, ".");
			}
			fprintf (fp, "%-10s%-30s <NULL> address\n", lvlwrk, vname);
			return;
		}
	} else if (dump_null_adrs) {
		/* Skip printing as previous Group had no address */
		return;
	} else {
		/* TODO: try to find a better algorithm for indent
			(with level 1, 10, 15 + ... we have one at 5 and all others on 7),
			maybe use a configuration here?	*/
		indent = level / 2;
		if (indent > 7)
			indent = 7;
		sprintf (lvlwrk, "%*s%02d", indent, " ", level);
	}
	if (f->data == NULL) {
		fprintf (fp, "%-10s%-30s <CODEGEN ERROR, PLEASE REPORT THIS!>\n", lvlwrk, vname);
		return;
	}
	if (f->attr->type == COB_TYPE_GROUP) {
		strcat(vname,".");
		fprintf (fp, "%-10s%s\n", lvlwrk, vname);
	} else {
		fprintf (fp, "%-10s%-30s ", lvlwrk, vname);
		if (strlen (vname) > 30) {
			fprintf (fp, "\n%-*s", 41, " ");
		}
		if (  (COB_FIELD_TYPE (f) == COB_TYPE_NUMERIC_EDITED
			|| COB_FIELD_TYPE (f) == COB_TYPE_NUMERIC_DISPLAY)
		 && !is_field_display (f)) {
			display_alnum_dump (f, fp, 41, cobsetptr->cob_dump_width);
		} else
		if (COB_FIELD_TYPE (f) == COB_TYPE_ALPHANUMERIC
		 || COB_FIELD_TYPE (f) == COB_TYPE_ALPHANUMERIC_EDITED
		 || f->size > 39) {
			display_alnum_dump (f, fp, 41, cobsetptr->cob_dump_width);
		} else {
			fprintf (fp, " "); 
			cob_display_common (f, fp);
		}
		fprintf (fp, "\n");
	}
}

void
cob_print_field (FILE *fp, cob_field *f, int indent, int width)
{
	if (f->data == NULL) {
		fprintf(fp," <NULL> address");
	} else if (!is_field_display(f)
		&& (f->attr->type == COB_TYPE_NUMERIC_EDITED
		 || f->attr->type == COB_TYPE_NUMERIC_DISPLAY)) {
		display_alnum_dump (f, fp, indent, width);
	} else if (f->attr->type == COB_TYPE_ALPHANUMERIC
		|| f->attr->type == COB_TYPE_ALPHANUMERIC_EDITED
		|| f->attr->type == COB_TYPE_GROUP
		|| f->size > 39) {
		display_alnum_dump (f, fp, indent, width);
	} else {
		fprintf (fp," ");
		cob_display_common (f, fp);
	}
	fprintf(fp,"\n");
}

/* ACCEPT */

void
cob_accept (cob_field *f)
{
	unsigned char	*p;
	size_t		size;
	int		ipchr;
	cob_field	temp;

	if (cobglobptr->cob_screen_initialized) {
		cob_field_accept (f, NULL, NULL, NULL, NULL,
				  NULL, NULL, NULL, NULL,
				  COB_SCREEN_PROMPT);
		return;
	}
	if (COB_MODULE_PTR->crt_status) {
		if (COB_FIELD_IS_NUMERIC (COB_MODULE_PTR->crt_status)) {
			cob_set_int (COB_MODULE_PTR->crt_status, 0);
		} else {
			memset (COB_MODULE_PTR->crt_status->data, '0', (size_t)4);
		}
	}
	/* extension: ACCEPT OMITTED */
	if (unlikely (!f)) {
		for (; ; ) {
			ipchr = getchar ();
			if (ipchr == '\n' || ipchr == EOF) {
				break;
			} else if (ipchr == 03) {
				cob_raise (2);
			}
		}
		return;
	}
	p = COB_TERM_BUFF;
	temp.data = p;
	temp.attr = &const_alpha_attr;
	size = 0;
	/* Read a line */
	for (; size < COB_MEDIUM_MAX; ) {
		ipchr = getchar ();
		if (unlikely (ipchr == EOF)) {
			cob_set_exception (COB_EC_IMP_ACCEPT);
			if (!size) {
				size = 1;
				p[0] = ' ';
				p[1] = 0;
			}
			break;
		} else if (ipchr == 03) {
			cob_raise (2);
		} else if (ipchr == '\n') {
			break;
		}
		p[size++] = (char) ipchr;
	}
	temp.size = size;
	if (COB_FIELD_TYPE(f) == COB_TYPE_NUMERIC_DISPLAY) {
		if (temp.size > f->size) {
			temp.size = f->size;
		}
	}
	cob_move (&temp, f);
}

/*
 * Move numeric value into working field with trailing NULs
 * Then 'pretty_display_numeric' will skip outputing the NULs
 */
void
cob_field_int_display (cob_field *i, cob_field *f)
{
	memset(f->data, 0, f->size);
	sprintf((char *)(f->data), "%d", *(int *)i->data);
}

void
cob_init_termio (cob_global *lptr, cob_settings *sptr)
{
	cobglobptr = lptr;
	cobsetptr  = sptr;
}

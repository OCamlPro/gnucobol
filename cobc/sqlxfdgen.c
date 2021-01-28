/*
   Copyright (C) 2019-2021 Free Software Foundation, Inc.
   Written by Ron Norman, Simon Sobisch

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


#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <limits.h>

#include "tarstamp.h"

#include "cobc.h"
#include "tree.h"

#define MAX_XFD 24
static int	hasxfd = 0;
static char xfd[MAX_XFD][80];

#define MAX_DATE 24
static int	ndate = 0;
static char dateformat[MAX_DATE][40];

#define MAX_OCC_NEST 16
static char eol[6] = "";
static char prefix[8] = "";
static int	prefixlen = 0;
static int	next_lbl = 1;
static short COMPtoDig[10]   = {3,8,11,13,16,18,21,23,27};	/* SQL storage size for Binary field */
static const char *sqlnames[] = {
	"BIGINT",
	"CHAR", 
	"CONSTRAINT",
	"CREATE",
	"DATE",
	"DATETIME",
	"DECIMAL",
	"DOUBLE",
	"FLOAT",
	"IDENTITY",
	"INDEX",
	"INTEGER",
	"KEY",
	"NOT",
	"NULL",
	"NUMBER",
	"PRIMARY",
	"SEQUENCE",
	"SMALLINT",
	"TABLE",
	"TIME",
	"TIMESTAMP",
	"UNIQUE",
	"VARCHAR", 
	"VARCHAR2", 
	NULL};


void
cb_save_xfd (char *str)
{
	if (cb_sqldb_name == NULL) {
		hasxfd = 0;
		return;
	}
	if (hasxfd >= MAX_XFD) {
		cb_error (_("XFD table overflow at: %s"), str);
		return;
	}
	if (strcasecmp(str,"ALL") == 0) {	/* ALL files are implied XFD */
		cb_all_files_xfd = 1;
		return;
	}
	strcpy(xfd[hasxfd],str);
	hasxfd++;
}

/* Local functions */

static int
find_date (struct cb_field *f)
{
	int		k;
	for (k=0; k < ndate; k++) {
		if (strcmp (dateformat[k], f->sql_date_format) == 0)
			return k;
	}
	return -1;
}

static void
save_date (struct cb_field *f)
{
	do {
		if (f->level < 1
		 || f->level >= 66) {
			f = f->sister;
			if (f == NULL)
				return;
			continue;
		}
		if (f->children) {
			save_date (f->children);
		}
		if (f->sql_date_format && ndate < MAX_DATE) {
			int		k;
			for (k=0; k < ndate; k++) {
				if (strcmp (dateformat[k], f->sql_date_format) == 0)
					break;
			}
			if (k == ndate) {
				strcpy (dateformat[ndate++], f->sql_date_format);
			}
		}
		f = f->sister;
	} while (f);
}

/*
 * Parse the Date Format String
 * Returns NULL if all good, else address of bad character in 'format'
 */
static char *
cb_date_str ( struct sql_date *sdf, char *format)
{
static struct sql_date lcl[1];	/* Make static as it may be returned */
	int	len, pos, extra;
	char	*dp;

	if (sdf == NULL)
		sdf = lcl;

	memset((void*)sdf,0,sizeof(struct sql_date));
	strcpy(sdf->format,format);
	len = strlen(sdf->format);
	if (sdf->format[0] == '\'') {
		if (sdf->format[len-1] == '\'')
			sdf->format[len-1] = 0;
		memmove(sdf->format,sdf->format+1,len);
	} else if (sdf->format[0] == '"') {
		if (sdf->format[len-1] == '"')
			sdf->format[len-1] = 0;
		memmove(sdf->format,sdf->format+1,len);
	}
	sdf->hasTime = 0;
	sdf->hasDate = 0;
	sdf->yyRule = ' ';
	dp = sdf->format;
	len = pos = extra = 0;
	while(*dp != 0) {
		len = 0;
		if(*dp == 'Y') {			/* Year */
			sdf->hasDate = 1;
			sdf->yyPos = (unsigned char)pos;
			while(*dp == 'Y') {
				len++;
				dp++;
				pos++;
			}
			sdf->yyLen = (unsigned char)len;
			if(*dp == '+'				/* '+' Add to YY to get full year */
			|| *dp == '%') {			/* '%' define pivot year to compute full year */
				sdf->yyRule = *dp;
				dp++;
				sdf->yyAdj = 0;
				while(isdigit(*dp)) {
					sdf->yyAdj = (sdf->yyAdj * 10) + (*dp - '0');
					dp++;
				}
			}
		} else if(*dp == 'M') {
			if(dp[1] == 'I') {		/* MInutes */
				sdf->hasTime = 1;
				sdf->miPos = (unsigned char)pos;
				sdf->miLen = 2;
				dp += 2;
				pos += 2;
			} else {
				sdf->hasDate = 1;
				sdf->mmPos = (unsigned char)pos;
				while(*dp == 'M') {	/* Month */
					len++;
					dp++;
					pos++;
				}
				sdf->mmLen = (unsigned char)len;
			}
		} else if(*dp == 'N') {		/* Minutes of hour */
			sdf->hasTime = 1;
			sdf->miPos = (unsigned char)pos;
			while(*dp == 'N') {
				len++;
				dp++;
				pos++;
			}
			sdf->miLen = len;
		} else if(*dp == 'D') {		/* Day of Month */
			sdf->hasDate = 1;
			sdf->ddPos = (unsigned char)pos;
			while(*dp == 'D') {
				len++;
				dp++;
				pos++;
			}
			sdf->ddLen = len;
		} else if(*dp == 'J') {		/* Julian Day of year */
			sdf->hasDate = 1;
			sdf->ddPos = (unsigned char)pos;
			while(*dp == 'J') {
				len++;
				dp++;
				pos++;
			}
			sdf->ddLen = len;
		} else if(*dp == 'E') {		/* Julian Day of year */
			sdf->hasDate = 1;
			sdf->ddPos = (unsigned char)pos;
			while(*dp == 'E') {
				len++;
				dp++;
				pos++;
			}
			sdf->ddLen = len;
		} else if(*dp == 'C') {		/* Century */
			sdf->hasDate = 1;
			sdf->ccPos = (unsigned char)pos;
			while(*dp == 'C') {
				len++;
				dp++;
				pos++;
			}
			sdf->ccLen = len;
		} else if(*dp == 'H') {		/* Hour */
			sdf->hasTime = 1;
			sdf->hhPos = (unsigned char)pos;
			while(*dp == 'H') {
				len++;
				dp++;
				pos++;
			}
			sdf->hhLen = (unsigned char)len;
			if(memcmp(dp,"24",2) == 0)
				dp += 2;
			else if(memcmp(dp,"12",2) == 0)
				dp += 2;
		} else if(*dp == 'S') {		/* Seconds */
			sdf->hasTime = 1;
			sdf->ssPos = pos;
			while(*dp == 'S') {
				len++;
				dp++;
				pos++;
			}
			sdf->ssLen = len;
		} else if(*dp == 'T') {		/* Hundredths of Second */
			sdf->hasTime = 1;
			sdf->huPos = pos;
			while(*dp == 'T') {
				len++;
				dp++;
				pos++;
			}
			sdf->huLen = (unsigned char)len;
		} else if(*dp == 'U') {		/* Hundredths of Second */
			sdf->hasTime = 1;
			sdf->huPos = pos;
			while(*dp == 'U') {
				len++;
				dp++;
				pos++;
			}
			sdf->huLen = (unsigned char)len;
		} else if (*dp == '/' 
				|| *dp == '-' 
				|| *dp == ' ' 
				|| *dp == '.' 
				|| *dp == ',' 
				|| *dp == ':') {	/* Noise/editing characters */
			dp++;
			pos++;
			extra++;
		} else {
			return dp;
		}
	}
	sdf->digits = (unsigned char)(sdf->ccLen + sdf->yyLen + sdf->mmLen + sdf->ddLen
				+ sdf->hhLen + sdf->miLen + sdf->ssLen + sdf->huLen
				+ extra);
	return NULL;
}

static char *
cb_get_param (char *p, char *prm, int skipeq)
{
	char	eq = 0x01;
	char	qt = 0x00;
	if (skipeq)
		eq = '=';
	while (isspace(*p) || *p == eq) p++;
	if (*p == '"' || *p == '\'') {
		qt = *p;
		do {
			*prm++ = *p++;
		} while (*p != 0 && *p != qt);
		if (*p == qt)
			*prm++ = *p++;
	} else {
		while (*p != 0 && *p != ',' 
			&& *p != eq && !isspace(*p)) {
			*prm++ = *p++;
		}
	}
	*prm = 0;
	while (*p == ',' || isspace(*p)) p++;
	return p;
}

static void
cb_use_name (struct cb_field *f, char *n)
{
	if(*n > ' ') {
		if (f->sql_name) {
			cb_source_line--;
			cb_warning (cb_warn_additional, _("XFD replaced %s with %s for %s"), 
								f->sql_name, n, f->name);
			cb_source_line++;
		}
		f->sql_name = cobc_parse_strdup (n);
	}
}

static int
compstr(char *tst, const char *val)
{
	int k;
	for (k=0; tst[k] != 0; k++) {
		if (tst[k] == '-' || tst[k] == '_') {
			if (val[k] != '-' && val[k] != '_')
				return 1;
			continue;
		}
		if (toupper (tst[k]) != toupper (val[k]))
			return 1;
	}
	if (val[k] != 0)
		return 1;
	return 0;
}

void
cb_parse_xfd (struct cb_file *fn, struct cb_field *f)
{
	int		i, k, skipeq;
	char	*p, *pw, expr[COB_NORMAL_BUFF];
	char	p1[64], p2[64], p3[64], p4[64], p5[64], p6[64];
	char	*prm[6];
	struct sql_date sdf[1];
	if (hasxfd <= 0)
		return;
	prm[0] = p1;
	prm[1] = p2;
	prm[2] = p3;
	prm[3] = p4;
	prm[4] = p5;
	prm[5] = p6;
	if (!fn->flag_sql_xfd) {
		fn->max_sql_name_len = 24;
		fn->flag_sql_trim_prefix = 1;
		fn->flag_sql_xfd = 1;
		fn->flag_sql_keep_filler = 0;
	}
	for(k=0; k < hasxfd; k++) {
		pw = cb_get_param (xfd[k], p1, 1);
		if (compstr(p1,"WHEN") == 0
		 || compstr(p1,"AND") == 0
		 || compstr(p1,"OR") == 0)
			skipeq = 0;
		else
			skipeq = 1;
		p = cb_get_param (pw, p2, skipeq);
		p = cb_get_param (p, p3, skipeq);
		p = cb_get_param (p, p4, skipeq);
		p = cb_get_param (p, p5, skipeq);
		p = cb_get_param (p, p6, skipeq);
		if (compstr(p1,"USE") == 0) {
			strcpy(p1,p2);
			strcpy(p2,p3);
			strcpy(p3,p4);
			strcpy(p4,p5);
			strcpy(p5,p6);
			strcpy(p6,"");
		}
		if (compstr(p1,"NAME") == 0 
		 && p2[0] > ' ') {
			if (f->level == 1
			 && fn->sql_name == NULL) {
				strcpy (expr,p2);
				for(i=0; expr[i] != 0; i++) {
					if(isupper(expr[i]))
						expr[i] = (char)tolower(expr[i]);
				}
				for (i=0; sqlnames[i] != NULL; i++) {
					if (strcasecmp(sqlnames[i],expr) == 0) {
						strcat(expr,"_x");
						break;
					}
				}
				fn->sql_name = cobc_parse_strdup (expr);
			} else {
				cb_use_name (f, p2);
			}
			for (i=2; i < 5; i++) {
				if (compstr(prm[i],"KEEP") == 0
				 && compstr(prm[i+1],"FILLER") == 0) {
					fn->flag_sql_keep_filler = 1;
				}
				if (compstr(prm[i],"KEEP") == 0
				 && compstr(prm[i+1],"PREFIX") == 0) {
					fn->flag_sql_trim_prefix = 0;
				}
			}
		} else if (compstr(p1,"GROUP") == 0) {
			f->flag_sql_group = 1;
			if (compstr(p2,"BINARY") == 0) {
				f->flag_sql_binary = 1;
				cb_use_name (f, p3);
			} else if (compstr(p2,"ALPHA") == 0) {
				f->flag_sql_char = 1;
				cb_use_name (f, p3);
			} else if (compstr(p2,"CHAR") == 0) {
				f->flag_sql_char = 1;
				cb_use_name (f, p3);
			} else if (compstr(p2,"VAR_LENGTH") == 0) {
				f->flag_sql_varchar = 1;
				cb_use_name (f, p3);
			} else if (compstr(p2,"VARCHAR") == 0) {
				f->flag_sql_varchar = 1;
				cb_use_name (f, p3);
			} else if (compstr(p2,"NUMERIC") == 0) {
				f->flag_sql_numeric = 1;
				cb_use_name (f, p3);
			} else {
				cb_use_name (f, p2);
			}
		} else if (compstr(p1,"BINARY") == 0) {
			f->flag_sql_binary = 1;
			f->flag_sql_group = 1;
			cb_use_name (f, p2);
		} else if (compstr(p1,"ALPHA") == 0) {
			f->flag_sql_char = 1;
			f->flag_sql_group = 1;
			cb_use_name (f, p2);
		} else if (compstr(p1,"CHAR") == 0) {
			f->flag_sql_char = 1;
			f->flag_sql_group = 1;
			cb_use_name (f, p2);
		} else if (compstr(p1,"VAR_LENGTH") == 0) {
			f->flag_sql_varchar = 1;
			f->flag_sql_group = 1;
			cb_use_name (f, p2);
		} else if (compstr(p1,"VARCHAR") == 0) {
			f->flag_sql_varchar = 1;
			f->flag_sql_group = 1;
			cb_use_name (f, p2);
		} else if (compstr(p1,"NUMERIC") == 0) {
			f->flag_sql_numeric = 1;
			cb_use_name (f, p2);
		} else if (compstr(p1,"DATE") == 0) {
			char	*err;
			int		len;
			if(p2[0] > ' ') {
				len = strlen(p2);
				if (p2[0] == '\'') {
					if (p2[len-1] == '\'')
						p2[len-1] = 0;
					memmove(p2,p2+1,len);
				} else if (p2[0] == '"') {
					if (p2[len-1] == '"')
						p2[len-1] = 0;
					memmove(p2,p2+1,len);
				}
				f->sql_date_format = cobc_parse_strdup (p2);
			} else {
				f->sql_date_format = cobc_parse_strdup ("YYYYMMDD");
			}
			if ((err = cb_date_str (sdf, f->sql_date_format)) != NULL) {
				cb_source_line--;
				cb_error (_("DATE %s incorrect at '%c'"), f->sql_date_format, *err);
				cb_source_line++;
				cobc_parse_free (f->sql_date_format);
				f->sql_date_format = NULL;
			} else {
				if (sdf->hasDate)
					f->flag_sql_date = 1;
				else if (sdf->hasTime)
					f->flag_sql_time = 1;
			}
			cb_use_name (f, p3);
		} else if (compstr(p1,"WHEN") == 0) {
			if (f->sql_when == NULL) {
				snprintf(expr,sizeof(expr),"%s",pw);
			} else {
				snprintf(expr,sizeof(expr),"(%s) OR (%s)",f->sql_when,pw);
				cobc_parse_free (f->sql_when);
			}
			f->sql_when = cobc_parse_strdup (expr);
		} else if (compstr(p1,"AND") == 0
				|| compstr(p1,"OR") == 0) {
			if (f->sql_when == NULL) {
				snprintf(expr,sizeof(expr),"%s",pw);
			} else {
				snprintf(expr,sizeof(expr),"(%s) %s (%s)",f->sql_when,p1,pw);
				cobc_parse_free (f->sql_when);
			}
			f->sql_when = cobc_parse_strdup (expr);
		} else {
			cb_source_line--;
			cb_warning (cb_warn_additional, _("XFD unknown %s %s"), p1, p2);
			cb_source_line++;
		}
	}
	hasxfd = 0;
}

static struct cb_field *
cb_code_field (cb_tree x)
{
	if (CB_REFERENCE_P (x)) {
		if (!CB_REFERENCE (x)->value) {
			return CB_FIELD (cb_ref (x));
		}
		return CB_FIELD (CB_REFERENCE (x)->value);
	}
	if (CB_LIST_P (x)) {
		return cb_code_field (CB_VALUE (x));
	}
	return CB_FIELD (x);
}

static int
is_all_dispx (struct cb_field *f)
{
	if (f->children
	 && !is_all_dispx (f->children))
		return 0;
	if (f->usage != CB_USAGE_DISPLAY) 
		return 0;
	if (f->sister
	 && !is_all_dispx (f->sister))
		return 0;
	return 1;
}

/* Is this field all DISPLAY data */
static int
is_all_display (struct cb_field *f)
{
	if (f->children) 
		return is_all_dispx (f->children);
	if (f->usage != CB_USAGE_DISPLAY) 
		return 0;
	return 1;
}

/* Return the SQL column name */
static char *
get_col_name (struct cb_file *fl, struct cb_field *f, int sub, int idx[])
{
	static char name[85];
	int		i,j;
	if (f->sql_name) {
		strcpy(name,f->sql_name);
	} else if (f->flag_filler) {
		strcpy(name,"");
		if (!f->flag_sql_filler) {
			f->flag_sql_filler = 1;
			j = cb_source_line;
			cb_source_line = f->common.source_line;
			cb_warning (cb_warn_additional, 
						_("Use '$XFD NAME xxxx' to assign name to FILLER"));
			cb_source_line = j;
		}
		if (f->sql_filler_id == 0)
			f->sql_filler_id = ++fl->sql_filler_id;
		sprintf(name,"filler_%d_x",f->sql_filler_id);
	} else {
		i = 0;
		if (prefixlen > 0
		 && strncasecmp(f->name, prefix, prefixlen) == 0)
			i = prefixlen;
		for(j=0; f->name[i] != 0; i++) {
			if (f->name[i] == '-'
			 || f->name[i] == ' ') {
				if (!fl->flag_sql_trim_dash)
					name[j++] = '_';
			} else {
				name[j++] = f->name[i];
			}
		}
		name[j] = 0;
	}
	j = strlen(name);
	if (j > fl->max_sql_name_len
	 && fl->max_sql_name_len > 0)
		name[j=fl->max_sql_name_len] = 0;
	for(i=0; i < j; i++) {
		if(isupper(name[i]))
			name[i] = (char)tolower(name[i]);
	}
	if (sub > 0) {
		for (i=0; i < sub; i++) {
			j += sprintf(&name[j],"_%02d",idx[i]);
		}
	} else {
		for (i=0; sqlnames[i] != NULL; i++) {
			if (strcasecmp(sqlnames[i],name) == 0) {
				strcat(name,"_x");
				break;
			}
		}
	}
	return name;
}

/* Return the SQL column data type */
static char *
get_col_type (struct cb_field *f)
{
	static char datatype[85];
	if (f->flag_sql_binary) {
		sprintf(datatype,"BINARY(%d)",f->size);
	} else
	if (f->flag_sql_char) {
		sprintf(datatype,"CHAR(%d)",f->size);
	} else
	if (f->flag_sql_varchar) {
		sprintf(datatype,"VARCHAR(%d)",f->size);
	} else
	if (f->flag_sql_group) {
		sprintf(datatype,"CHAR(%d)",f->size);
	} else
	if (f->flag_sql_numeric) {
		sprintf(datatype,"DECIMAL(%d)",f->size);
	} else
	if (f->flag_sql_date) {
		sprintf(datatype,"DATE");
	} else if (f->flag_sql_time) {
		sprintf(datatype,"TIME");
	} else {
		switch (f->usage) {
		case CB_USAGE_BINARY:
		case CB_USAGE_COMP_5:
		case CB_USAGE_COMP_X:
		case CB_USAGE_COMP_N:
		case CB_USAGE_PACKED:
			if (f->pic) {
				if (f->pic->scale > 0)
					sprintf(datatype,"DECIMAL(%d,%d)",f->pic->digits,f->pic->scale);
				else
					sprintf(datatype,"DECIMAL(%d)",f->pic->digits);
			} else {
				sprintf(datatype,"DECIMAL(%d)",f->size);
			}
			return datatype;
		case CB_USAGE_COMP_6:
			if (f->pic) {
				if (f->pic->scale > 0)
					sprintf(datatype,"DECIMAL(%d,%d)",f->pic->digits,f->pic->scale);
				else
					sprintf(datatype,"DECIMAL(%d)",f->pic->digits);
			} else {
				sprintf(datatype,"DECIMAL(%d)",f->size*2);
			}
			return datatype;
		case CB_USAGE_DISPLAY:
			if (f->pic
			&& f->pic->category == CB_CATEGORY_NUMERIC) {
				if (f->pic->scale > 0)
					sprintf(datatype,"DECIMAL(%d,%d)",f->pic->digits,f->pic->scale);
				else
					sprintf(datatype,"DECIMAL(%d)",f->pic->digits);
			} else {
				sprintf(datatype,"CHAR(%d)",f->size);
			}
			return datatype;
		case CB_USAGE_FLOAT:
			return (char*)"FLOAT(23)";
		case CB_USAGE_DOUBLE:
			return (char*)"FLOAT(53)";
		case CB_USAGE_UNSIGNED_CHAR:
		case CB_USAGE_SIGNED_CHAR:
		case CB_USAGE_UNSIGNED_SHORT:
		case CB_USAGE_SIGNED_SHORT:
		case CB_USAGE_UNSIGNED_INT:
		case CB_USAGE_SIGNED_INT:
		case CB_USAGE_UNSIGNED_LONG:
		case CB_USAGE_SIGNED_LONG:
			return (char*)"INTEGER";
		default:
			cb_error (_("%s unexpected USAGE: %d"), __FILE__, f->usage);
		}
		sprintf(datatype,"CHAR(%d)",f->size);
	}
	return datatype;
}

/* Return the XFD data type value */
static char *
get_xfd_type (struct cb_field *f)
{
	int		sqlsz = f->size + 1;
	int		sqltype = COB_XFDT_PICX;
	static char datatype[85];
	if (f->flag_sql_binary) {
		sqltype = COB_XFDT_BIN;
	} else
	if (f->flag_sql_char) {
		sqltype = COB_XFDT_PICX;
	} else
	if (f->flag_sql_varchar) {
		sqltype = COB_XFDT_VARX;
	} else
	if (f->flag_sql_group) {
		sqltype = COB_XFDT_PICX;
	} else {
		switch (f->usage) {
		case CB_USAGE_BINARY:
		case CB_USAGE_LENGTH:
			if (f->pic
			&& f->pic->category == CB_CATEGORY_NUMERIC) {
				sqlsz = f->pic->digits + 3;
				if (sqlsz < COMPtoDig[f->size-1])
					sqlsz = COMPtoDig[f->size-1];
				if (f->pic->have_sign > 0)
					sqltype = COB_XFDT_COMPS;
				else
					sqltype = COB_XFDT_COMPU;
			} else {
				sqlsz = COMPtoDig[f->size-1];
				sqltype = COB_XFDT_COMPU;
			}
			break;
		case CB_USAGE_COMP_5:
			if (f->pic
			&& f->pic->category == CB_CATEGORY_NUMERIC) {
				sqlsz = f->pic->digits + 3;
				if (sqlsz < COMPtoDig[f->size-1])
					sqlsz = COMPtoDig[f->size-1];
				if (f->pic->have_sign > 0)
					sqltype = COB_XFDT_COMP5S;
				else
					sqltype = COB_XFDT_COMP5U;
			} else {
				sqlsz = COMPtoDig[f->size-1];
				sqltype = COB_XFDT_COMP5U;
			}
			break;
		case CB_USAGE_COMP_X:
			sqlsz = COMPtoDig[f->size-1];
			sqltype = COB_XFDT_COMPX;
			break;
		case CB_USAGE_PACKED:
			if (f->pic) {
				sqlsz = f->pic->digits + 3;
				if (f->pic->have_sign > 0)
					sqltype = COB_XFDT_PACKS;
				else
					sqltype = COB_XFDT_PACKU;
			} else {
				sqlsz = f->size * 2 + 2;
				sqltype = COB_XFDT_PACKU;
			}
			break;
		case CB_USAGE_COMP_6:
			sqltype = COB_XFDT_COMP6;
			if (f->pic)
				sqlsz = f->pic->digits + 1;
			else
				sqlsz = f->size * 2 + 1;
			break;
		case CB_USAGE_DISPLAY:
			if (f->pic
			&& f->pic->category == CB_CATEGORY_NUMERIC) {
				sqlsz = f->size + 3;
				if (f->pic->have_sign > 0)
					sqltype = COB_XFDT_PIC9S;
				else
					sqltype = COB_XFDT_PIC9U;
			} else {
				sqltype = COB_XFDT_PICX;
			}
			break;
		case CB_USAGE_FLOAT:
		case CB_USAGE_DOUBLE:
			sqlsz = 36;
			sqltype = COB_XFDT_FLOAT;
			break;
		case CB_USAGE_UNSIGNED_CHAR:
		case CB_USAGE_UNSIGNED_SHORT:
		case CB_USAGE_UNSIGNED_INT:
		case CB_USAGE_UNSIGNED_LONG:
			sqlsz = COMPtoDig[f->size-1];
			sqltype = COB_XFDT_COMP5U;
			break;
		case CB_USAGE_SIGNED_CHAR:
		case CB_USAGE_SIGNED_SHORT:
		case CB_USAGE_SIGNED_INT:
		case CB_USAGE_SIGNED_LONG:
			sqlsz = COMPtoDig[f->size-1];
			sqltype = COB_XFDT_COMP5S;
			break;
		default:
			cb_error (_("%s unexpected USAGE: %d for SQL/XFD"), __FILE__, f->usage);
			sqltype = COB_XFDT_BIN;
		}
	}
	if (f->sql_date_format
	 && sqlsz < 32)
		sqlsz = 32;
	sprintf(datatype,"%02d,%04d",sqltype,sqlsz);
	return datatype;
}

/* Is the field also used as a 'key' for the file */
static int
is_key_field (struct cb_file *fl, struct cb_field *f)
{
	struct cb_alt_key	*l;
	struct cb_key_component *c;

	if (fl->component_list) {
		for (c = fl->component_list; c; c = c->next) {
			if (f == cb_code_field (c->component))
				return 1;
		}
	} else if(fl->key) {
		if (f == cb_code_field (fl->key))
			return 1;
	}
	for (l = fl->alt_key_list; l; l = l->next) {
		if (l->component_list) {
			for (c = l->component_list; c; c = c->next) {
				if (f == cb_code_field (c->component))
					return 1;
			}
		} else {
			if (f == cb_code_field (l->key))
				return 1;
		}
	}
	return 0;
}

#define MAX_NEST 24
static char *
out_part(char *exp)
{
	static char wrk[256];
	char	lop[80],rop[80],opcd[32];
	int		i,j;
	for(j=0; exp[j] == ' '; j++);
	for(i=0; exp[j] != 0 && exp[j] != ' '; j++) {
		if (exp[j] == ' '
		 && exp[j+1] == ' ')
			continue;
		lop[i++] = exp[j];
	}
	lop[i] = 0;
	while(exp[j] == ' ') j++;
	for(i=0; exp[j] != 0 && exp[j] != ' '; j++) {
		if (exp[j] == ' '
		 && exp[j+1] == ' ')
			continue;
		opcd[i++] = exp[j];
	}
	opcd[i] = 0;
	while(exp[j] == ' ') j++;
	for(i=0; exp[j] != 0; j++) {
		if (exp[j] == ' '
		 && exp[j+1] == ' ')
			continue;
		rop[i++] = exp[j];
	}
	rop[i] = 0;
	snprintf(wrk,sizeof(wrk)-1,"%s,%s,%s",opcd,lop,rop);
	return wrk;
}

static void
write_postfix(FILE *fx, int golbl, char *expr)
{
	int		k,nexp,nopcd,gto;
	int 	opcode[MAX_NEST];
	char	partexp[MAX_NEST][68], *p;

	nexp = nopcd = gto = 0;
	for(k=0; k < MAX_NEST; k++) {
		opcode[k] = 0;
		memset(partexp[k],0,68);
	}
	for (p = expr; *p != 0 && nexp < MAX_NEST; ) {
		if (*p == '(') {
			p++;
			opcode[nexp++] = '(';
		} if (*p == ')') {
			p++;
			while (nexp > 0
				&& opcode[nexp-1] != '(') {
				gto = 0;
				if (nexp == 1 && *p == 0) {
					gto = golbl;
					golbl = 0;
				}
				if (nopcd > 1) {
					fprintf(fx,"C,0,%.255s\n",out_part(partexp[nopcd-2]));
					fprintf(fx,"C,0,%.255s\n",out_part(partexp[nopcd-1]));
					nopcd -= 2;
				} else if (nopcd > 0) {
					fprintf(fx,"C,0,%.255s\n",out_part(partexp[--nopcd]));
				}
				if (opcode[nexp-1] == 'A') {
					fprintf(fx,"C,%d,%s\n",gto,"&&");
				} else if (opcode[nexp-1] == 'O') {
					fprintf(fx,"C,%d,%s\n",gto,"||");
				} else if (opcode[nexp-1] == '!') {
					fprintf(fx,"C,%d,%s\n",gto,"!");
				}
				nexp--;
			}
			if (nexp > 0
			 && opcode[nexp-1] == '(') {
				nexp--;
			} else if(*p == 0) {
				break;
			} else {
				cb_warning (cb_warn_additional, _("Incorrect XFD expression: %s"),expr);
				break;
			}
		} else if (strncasecmp(p," AND ",5) == 0) {
			p += 5;
			opcode[nexp++] = 'A';
		} else if (strncasecmp(p," OR ",4) == 0) {
			p += 4;
			opcode[nexp++] = 'O';
		} else if (strncasecmp(p,"NOT ",4) == 0) {
			p += 4;
			opcode[nexp++] = '!';
		} else if (memcmp(p," && ",4) == 0) {
			p += 4;
			opcode[nexp++] = 'A';
		} else if (memcmp(p," || ",4) == 0) {
			p += 4;
			opcode[nexp++] = 'O';
		} else if (memcmp(p," || ",4) == 0) {
			p += 4;
			opcode[nexp++] = 'O';
		} else {
			for(k=0; *p != 0 && k < 64; k++,p++) {
				if (strncasecmp(p," AND ",5) == 0
				 || strncasecmp(p," OR ",4) == 0
				 || strncasecmp(p," && ",4) == 0
				 || strncasecmp(p," || ",4) == 0
				 || *p == '('
				 || *p == ')')
					break;
				partexp[nopcd][k] = *p;
			}
			partexp[nopcd][k] = 0;
			if (k > 0)
				nopcd++;
		}
	}
	while (nopcd > 0) {
		if (nopcd > 1) {
			fprintf(fx,"C,0,%.255s\n",out_part(partexp[nopcd-2]));
			fprintf(fx,"C,0,%.255s\n",out_part(partexp[nopcd-1]));
			nopcd -= 2;
		} else if (nopcd > 0) {
			gto = 0;
			if (nexp == 0) {
				gto = golbl;
				golbl = 0;
			}
			fprintf(fx,"C,%d,%.255s\n",gto,out_part(partexp[--nopcd]));
		}
	}
	while (nexp > 0) {
		gto = 0;
		if (nexp == 1) {
			gto = golbl;
			golbl = 0;
		}
		if (opcode[nexp-1] == 'A') {
			fprintf(fx,"C,%d,%s\n",gto,"&&");
		} else if (opcode[nexp-1] == 'O') {
			fprintf(fx,"C,%d,%s\n",gto,"||");
		} else if (opcode[nexp-1] == '!') {
			fprintf(fx,"C,%d,%s\n",gto,"!");
		}
		nexp--;
	}
	if(golbl > 0)
		fprintf(fx,"C,%d\n",golbl);
}

static struct cb_field *
find_field (struct cb_field *f, char *name)
{
	struct cb_field *s;
	if (compstr((char*)f->name,(const char*)name) == 0)
		return f;
	if (f->children) {
		if ((s = find_field (f->children, name)) != NULL)
			return s;
	}
	for (f=f->sister; f; f = f->sister) {
		if ((s = find_field (f, name)) != NULL)
			return s;
	}
	return NULL;
}

static struct cb_field *
check_redefines (FILE *fx, struct cb_file *fl, struct cb_field *f, int sub, int idx[])
{
	int		i, j, k;
	int		numrdf, numwhen, numother, numdisp, toother;
	int		dowhen = 1, savelbl;
	char	expr[COB_NORMAL_BUFF], name[80];
	struct cb_field *s, *l, *n, *oth, *x;

	if (f->flag_sql_binary
	 || f->flag_sql_char
	 || f->flag_sql_varchar) {
		f->flag_sql_group = 1;
	}
	if (f->flag_sql_group)
		return f;
	n = f;
	savelbl = next_lbl;
	if (f->sister
	 && f->sister->redefines == f) {
		numrdf = numwhen = numother = numdisp = 0;
		s = f;
		oth = NULL;
		/* Check REDEFINES */
		do {
			l = s;
			numrdf++;
			if (is_all_display (s))
				numdisp++;
			if (s->sql_when) {
				k = strlen(s->sql_when);
				while (k > 0 
					&& s->sql_when[k-1] == ' ')
					s->sql_when[--k] = 0;
				if (compstr(s->sql_when,"OTHER") == 0)
					numother++;
				else
					numwhen++;
			} else {
				oth = s;
			}
			if (fx)
				s->step_count = next_lbl++;
			if (s->sister == NULL)
				break;
			s = s->sister;
		} while (s->redefines == f);
		if (fx)
			l->next_group_line = next_lbl++;

		if (numdisp == numrdf
		 && numwhen == 0) {		/* Just PIC X REDEFINES PIC X */
			n = l;
			dowhen = 0;
		} else
		if (numother == 0
		 && numrdf - numwhen == 1) {
			if (oth)
				oth->sql_when = cobc_parse_strdup ("OTHER");
		} else
		if ((numwhen+numother) != numrdf) {
			if (!f->flag_sql_binary) {
				if ((numwhen + numother) > 0)
					cb_warning (cb_warn_additional, _("%s has incomplete WHEN rules"),f->name);
				f->flag_sql_binary = 1;
				f->flag_sql_group = 1;
				dowhen = 0;
				cb_warning (cb_warn_additional, _("Process %s as BINARY data"),f->name);
			}
		}
		/* Emit When Conditions */
		if (!dowhen) {
			next_lbl = savelbl;
			s = f;
			do {
				s->step_count = 0;
				s->next_group_line = 0;
				if (s->sister == NULL)
					break;
				s = s->sister;
			} while (s->redefines == f);
		} else
		if (fx) {
			s = f;
			toother = 0;
			do {
				if (s == NULL)
					break;
				if (s->sql_when) {
					if (compstr(s->sql_when,"OTHER") == 0) {
						toother = s->step_count;
					} else {
						expr[0] = 0;
						for (i=j=0; s->sql_when[i] != 0; ) {
							if (s->sql_when[i] == ' '
							 && s->sql_when[i+1] == ' ') {
								i++;
								continue;
							}
							if (s->sql_when[i] == ' ') {
								strcat(expr," ");
								i++;
								continue;
							}
							if (s->sql_when[i] == '\''
							 || s->sql_when[i] == '"') {
								char	qt = s->sql_when[i];
								k = strlen(expr);
								do {
									expr[k++] = s->sql_when[i++];
								} while (s->sql_when[i] != qt
									&& s->sql_when[i] != 0);
								if (s->sql_when[i] != 0)
									expr[k++] = s->sql_when[i++];
								expr[k] = 0;
								continue;
							}
							if (isalnum(s->sql_when[i])) {
								j = 0;
								while(isalnum(s->sql_when[i])
									|| s->sql_when[i] == '-') 
									name[j++] = s->sql_when[i++];
								name[j] = 0;
								x = find_field (fl->record, name);
								if (x) {
									strcat(expr,get_col_name(fl,x,sub,idx));
								} else {
									strcat(expr,name);
								}
								continue;
							}
							k = strlen(expr);
							expr[k] = s->sql_when[i++];
							expr[k+1] = 0;
						}
						write_postfix (fx, s->step_count, expr);
					}
					if (l == NULL)
						break;
					s->report_decl_id = l->next_group_line;
				}
				if (s->sister == NULL)
					break;
				s = s->sister;
			} while (s->redefines == f);
			if (toother > 0) {
				if(f->step_count != toother)
					fprintf(fx,"G,%d\n",toother);
				else
					f->step_count = 0;
			}
		}
	}
	return n;
}

static void
write_xfd (FILE *fx, struct cb_file *fl, struct cb_field *f, int sub, int idx[])
{
	fprintf(fx, "F,%04d,%04d,", (int)f->offset, (int)f->size);
	fprintf(fx, "%s,", get_xfd_type (f));
	if (f->pic
	 && f->pic->category == CB_CATEGORY_NUMERIC) {
		fprintf( fx, "%d,%d,", (int)f->pic->digits, (int)f->pic->scale);
	} else {
		fprintf (fx, "0,0,");
	}
	if (f->sql_date_format) {
		fprintf (fx, "%d", find_date (f) + 1);
	}
	fprintf(fx,",%02d,%s\n",f->level,get_col_name(fl,f,sub,idx));
}

static void
write_field (struct cb_file *fl, struct cb_field *f, FILE *fs, FILE *fx, int sub, int idx[])
{
	struct cb_field *s;
	do {
		if (f->level < 1
		 || f->level >= 66) {
			f = f->sister;
			if (f == NULL)
				return;
			continue;
		}
		if (f->flag_filler
		 && f->sql_name == NULL
		 && !fl->flag_sql_keep_filler) {	/* Skip FILLER fields */
			f = f->sister;
			continue;
		}
		if (f->redefines == NULL
		 && f->sister
		 && f->sister->redefines == f)
			check_redefines (fx, fl, f, sub, idx);

		if (f->step_count > 0) {
			fprintf(fx,"L,%d\n",f->step_count);
			f->step_count = 0;
		}
		if (f->occurs_max > 1
		 && f->flag_occurs) {
			idx[sub] = 1;
			f->flag_occurs = 0;
			while (idx[sub] <= f->occurs_max) {
				if (sub >= MAX_OCC_NEST) {
					cb_error (_("%s nested occurs exceeds: %d"), __FILE__, MAX_OCC_NEST);
				}
				write_field (fl,f,fs,fx,sub+1,idx);
				idx[sub]++;
			}
			f->flag_occurs = 1;
			f = f->sister;
			if (f == NULL)
				return;
			continue;
		}
		if (f->children
		 && is_key_field (fl,f)
		 && is_all_display (f)) {
			f->flag_sql_group = 1;
		}
		if (f->flag_sql_group) {
			fprintf(fs,"%s   %-40s %s",eol,get_col_name(fl,f,sub,idx),get_col_type (f));
			write_xfd (fx,fl,f,sub,idx);
			strcpy(eol,",\n");
			s = f;
			while (s->sister
			 && s->sister->redefines == f) {	/* Skip Group Redefines */
				s = s->sister;
			}
			f = s;
		} else if (f->children) {
			write_field (fl,f->children,fs,fx,sub,idx);
		} else {
			fprintf(fs,"%s   %-40s %s",eol,get_col_name(fl,f,sub,idx),get_col_type (f));
			strcpy(eol,",\n");
			write_xfd (fx,fl,f,sub,idx);
		}
		if (is_key_field (fl,f)) 
			fprintf(fs," NOT NULL");
		if (f->occurs_max > 1
		 && !f->flag_occurs) 
			return;
		f = check_redefines (NULL, fl, f, sub, idx);
		if (f->report_decl_id > 0) {
			if (f->report_decl_id != f->next_group_line)
				fprintf(fx,"G,%d\n",f->report_decl_id);
			f->report_decl_id = 0;
		}
		if (f->next_group_line > 0) {
			fprintf(fx,"L,%d\n",f->next_group_line);
			f->next_group_line = 0;
		}
		f = f->sister;
	} while (f);
}

static void
check_prefix (struct cb_field *f)
{
	if (prefixlen <= 0)
		return;
	do {
		if (f->children && f->sister == NULL) {
			check_prefix (f->children);
		} else if (!f->flag_filler) {
			if (strncasecmp(f->name,prefix,prefixlen) != 0) {
				prefixlen = 0;
				return;
			}
		}
		if (f->children)
			check_prefix (f->children);
		f = f->sister;
	} while (f);
}

/* Write out the DDL and XFD files */
void
output_xfd_file (struct cb_file *fl)
{
	char	outname[COB_FILE_BUFF], tblname[64], time_stamp[32];
	FILE	*fx, *fs;
	struct tm	*loctime;
	time_t		sectime;
	struct cb_field		*f;
	struct cb_alt_key	*l;
	struct cb_key_component *c;
	struct sql_date sdf[1];
	int		i,j,k,sub,idx[MAX_OCC_NEST];

	if (fl->record_min != fl->record_max) {
		cb_warning (COBC_WARN_FILLER,
			_("FD %s; SQL requires fixed size records"), fl->name);
		return;
	}
	if (!fl->flag_sql_xfd) {
		fl->max_sql_name_len = 24;
		fl->flag_sql_trim_prefix = 1;
		fl->flag_sql_xfd = 1;
		cb_parse_xfd (fl, fl->record);
	}
	f = fl->record;
	if(f->level < 1)
		f = f->sister;
	if(f->storage != CB_STORAGE_FILE)
		return;
	for (sub=0; sub < MAX_OCC_NEST; sub++)
		idx[sub] = 0;
	sub = 0;
	next_lbl = 1;
	sectime = time (NULL);
	loctime = localtime (&sectime);
	if (loctime) {
		strftime (time_stamp, (size_t)COB_MINI_MAX,
			  		"%b %d %Y %H:%M:%S", loctime);
	} else {
		strcpy(time_stamp,"Time unknown");
	}
	if (fl->sql_name) {
		strcpy(tblname,fl->sql_name);
	} else if(fl->assign
		&& CB_LITERAL_P(fl->assign)) {
		struct cb_literal   *lit = CB_LITERAL (fl->assign); 
		char *	ps;
		char *	p = (char*)lit->data;
		int		ln = lit->size;
		if (ln > sizeof(tblname)-1) {
			p += ln - sizeof(tblname) + 1;
			ln = sizeof(tblname)-1;
		}
		if ((ps = strrchr (p, SLASH_CHAR)) != NULL) {
			ln -= p - (ps + 1);
			p = ps + 1;
		}
		sprintf(tblname,"%.*s",ln,p);
	} else {
		strcpy(tblname,fl->cname);
	}
	k = strlen(tblname);
	for(i=j=0; i < k; i++) {
		if (tblname[i] == '-')
			tblname[j++] = '_';
		else if(isalnum(tblname[i]))
			tblname[j++] = (char)tolower(tblname[i]);
	}
	tblname[j] = 0;
	strcpy(prefix,"");
	prefixlen = 0;
	if (fl->flag_sql_trim_prefix) {
		f = fl->record;
		while (f 
			&& (f->level <= 1 || f->flag_filler)) {
			if (f->children)
				f = f->children;
			else 
				f = f->sister;
		}
		while (f && f->children && f->sister == NULL) {
			f = f->children;
		}
		while (f 
			&& (f->level <= 1 || f->flag_filler)) {
			if (f->children)
				f = f->children;
			else 
				f = f->sister;
		}
		if (f) {
			for(k=0; k < 7; k++) {
				prefix[k] = f->name[k];
				if (prefix[k] == 0)
					break;
				if (prefix[k] == '-') {
					k++;
					prefix[k] = 0;
					break;
				}
			}
			prefixlen = k;
		}
		if (prefixlen > 0) {
			f = fl->record;
			if(f->level < 1 && f->sister)
				f = f->sister;
			check_prefix (f);
		}
	}

	sprintf(outname,"%s%s%s.xd",cob_schema_dir,SLASH_STR,tblname);
	if (cb_unix_lf) {
		fx = fopen (outname, "wb");
	} else {
		fx = fopen (outname, "w");
	}
	if (fx == NULL) {
		cb_warning (cb_warn_additional, _("Unable to open %s; '%s'"),outname,cb_get_strerror ());
		return;
	}
	sprintf(outname,"%s%s%s.ddl",cob_schema_dir,SLASH_STR,tblname);
	if (cb_unix_lf) {
		fs = fopen (outname, "wb");
	} else {
		fs = fopen (outname, "w");
	}
	if (fs == NULL) {
		cb_warning (cb_warn_additional, _("Unable to open %s; '%s'"),outname,cb_get_strerror ());
		return;
	}
	ndate = 0;
	for (f=fl->record->sister; f; f = f->sister) {
		save_date (f);
	}
	fprintf(fx,"# Generated on %s from %s\n",time_stamp,cb_source_file);
	fprintf(fx,"H,1,%s,%d,',','.',0,%d\n",tblname,ndate,fl->organization);
	for (k=0; k < ndate; k++) {
		cb_date_str (sdf, dateformat[k]);
		fprintf(fx,"D,%d,'%s'",k+1,dateformat[k]);
		fprintf(fx,",%d,%d,%d",sdf->digits,sdf->hasDate,sdf->hasTime);
		if (sdf->yyRule > ' ')
			fprintf(fx,",%c,%d",sdf->yyRule,sdf->yyAdj);
		else
			fprintf(fx,",,0");
		fprintf(fx,",%d:%d",sdf->yyPos,sdf->yyLen);
		fprintf(fx,",%d:%d",sdf->mmPos,sdf->mmLen);
		fprintf(fx,",%d:%d",sdf->ddPos,sdf->ddLen);
		fprintf(fx,",%d:%d",sdf->hhPos,sdf->hhLen);
		fprintf(fx,",%d:%d",sdf->miPos,sdf->miLen);
		fprintf(fx,",%d:%d",sdf->ssPos,sdf->ssLen);
		fprintf(fx,",%d:%d",sdf->ccPos,sdf->ccLen);
		fprintf(fx,",%d:%d",sdf->huPos,sdf->huLen);
		fprintf(fx,"\n");
	}
	fprintf(fs,"DROP TABLE %s;\n",tblname);
	fprintf(fs,"CREATE TABLE %s (\n",tblname);
	sub = 0;
	strcpy(eol,"");
	for (f=fl->record->sister; f; f = f->sister) {
		write_field (fl, f, fs, fx, sub, idx);
	}
	if (fl->organization == COB_ORG_RELATIVE) {
		fprintf(fs,"%s   rid_%-30s       BIGINT PRIMARY KEY",eol,tblname);
		fprintf(fx,"F,%04d,%04d,",(int)fl->record->size,4);
		fprintf(fx,"%02d,0015,",COB_XFDT_COMP5U);
		fprintf(fx,"12,0,,00,rid_%s\n",tblname);
		fprintf(fx,"K,0,N,N,,rid_%s\n",tblname);
	}
	fprintf(fs,"\n);\n");
	if (fl->organization == COB_ORG_INDEXED) {
		fprintf(fs,"CREATE UNIQUE INDEX pk_%s ON %s ",tblname,tblname);
		fprintf(fx,"K,0,N,N,,");
		if (fl->component_list) {
			fprintf(fs,"(");
			strcpy(eol,"");
			for (c = fl->component_list; c; c = c->next) {
				f = cb_code_field (c->component);
				fprintf(fs,"%s%s",eol,get_col_name(fl,f,0,idx));
				fprintf(fx,"%s%s",eol,get_col_name(fl,f,0,idx));
				strcpy(eol,",");
			}
			fprintf(fs,");\n");
			fprintf(fx,"\n");
		} else if(fl->key) {
			f = cb_code_field (fl->key);
			fprintf(fs,"(%s);\n",get_col_name(fl,f,0,idx));
			fprintf(fx,"%s\n",get_col_name(fl,f,0,idx));
		}
		k = 1;
		for (l = fl->alt_key_list; l; l = l->next) {
			fprintf(fs,"CREATE %sINDEX k%d_%s ON %s ",
					l->duplicates||l->tf_suppress?"":"UNIQUE ",k,tblname,tblname);
			fprintf(fx,"K,%d,%s,",k,l->duplicates?"Y":"N");
			if (l->suppress
			 && CB_LITERAL_P(l->suppress)) {
				struct cb_literal	*lit = CB_LITERAL (l->suppress);
				fprintf(fx,"Y,\"%.*s\",",lit->size,lit->data);
			} else
			if (l->tf_suppress) {
				fprintf(fx,"Y,0x%02X,",l->char_suppress);
			} else {
				fprintf(fx,"N,,");
			}
			if (l->component_list) {
				fprintf(fs,"(");
				strcpy(eol,"");
				for (c = l->component_list; c; c = c->next) {
					f = cb_code_field (c->component);
					fprintf(fs,"%s%s",eol,get_col_name(fl,f,0,idx));
					fprintf(fx,"%s%s",eol,get_col_name(fl,f,0,idx));
					strcpy(eol,",");
				}
				fprintf(fs,");\n");
				fprintf(fx,"\n");
			} else {
				f = cb_code_field (l->key);
				fprintf(fs,"(%s);\n",get_col_name(fl,f,0,idx));
				fprintf(fx,"%s\n",get_col_name(fl,f,0,idx));
			}
			k++;
		}
	}
	fclose(fs);
	fclose(fx);
	ndate = 0;
}

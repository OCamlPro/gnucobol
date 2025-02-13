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


#include "fileio.h"

/* Routines in fsqlxfd.c common to all interfaces */

/* Return index number for given key and set length attributes */
int
cob_findkey_attr (cob_file *f, cob_field *kf, int *fullkeylen, int *partlen)
{
	int 	k, part;
	*fullkeylen = *partlen = 0;

	for (k = 0; k < f->nkeys; ++k) {
		cob_field *key = f->keys[k].field;
		if (key
		 && key->data == kf->data
		 && f->keys[k].count_components <= 1) {
			*fullkeylen = key->size;
			*partlen = kf->size;
			return k;
		}
	}
	for (k = 0; k < f->nkeys; ++k) {
		if (f->keys[k].count_components > 1) {
			cob_field *key = f->keys[k].field;
			if ((key
			  && key->data == kf->data
			  && key->size == kf->size)
			 || (f->keys[k].component[0]->data == kf->data)) {
				for (part = 0; part < f->keys[k].count_components; part++) {
					*fullkeylen += f->keys[k].component[part]->size;
				}
				if (key
				 && key->data == kf->data) {
					*partlen = key->size;
				} else {
					*partlen = *fullkeylen;
				}
				return k;
			}
		}
	}
	return -1;
}

#if defined(WITH_ODBC) || defined(WITH_OCI) || defined(WITH_DB) || defined(WITH_LMDB)
/* Routines in fsqlxfd.c common to all Database interfaces */


/* Return total length of the key */
int
db_keylen (cob_file *f, int idx)
{
	int totlen, part;

	if (idx < 0 || idx > (int)(f->nkeys)) {
		return -1;
	}
	if (f->keys[idx].count_components > 0) {
		totlen = 0;
		for (part = 0; part < f->keys[idx].count_components; part++) {
			totlen += f->keys[idx].component[part]->size;
		}
		return totlen;
	}
	return f->keys[idx].field->size;
}

/* Save key for given index from 'record' into 'keyarea',
   returns total length of the key */
int
db_savekey (cob_file *f, unsigned char *keyarea, unsigned char *record, int idx)
{
	int totlen, part;

	if (f->keys[idx].count_components > 1) {
		totlen = 0;
		for (part = 0; part < f->keys[idx].count_components; part++) {
			memcpy (keyarea + totlen,
				record + (f->keys[idx].component[part]->data - f->record->data),
				f->keys[idx].component[part]->size);
			totlen += f->keys[idx].component[part]->size;
		}
		return totlen;
	}
	memcpy (keyarea, record + f->keys[idx].offset, f->keys[idx].field->size);
	return (int)f->keys[idx].field->size;
}

/* Compare key for given index 'keyarea' to 'record'.
   returns compare status */
int
db_cmpkey (cob_file *f, unsigned char *keyarea, unsigned char *record, int idx, int partlen)
{
	int sts, part, totlen;
	size_t	cl;

	if (partlen <= 0) {
		partlen = db_keylen(f, idx);
		/* LCOV_EXCL_START */
		if (partlen <= 0) {
			cob_runtime_error (_("invalid internal call of %s"), "db_cmpkey");
			cob_hard_failure_internal ("libcob");
		}
		/* LCOV_EXCL_STOP */
	}
	if (f->keys[idx].count_components > 0) {
		totlen = 0;
		for (part = 0; part < f->keys[idx].count_components && partlen > 0; part++) {
			cl = (size_t)partlen > f->keys[idx].component[part]->size
					? f->keys[idx].component[part]->size : (size_t)partlen;
			sts = memcmp (keyarea + totlen,
					record + (f->keys[idx].component[part]->data - f->record->data),
					cl);
			if (sts != 0) {
				return sts;
			}
			totlen += f->keys[idx].component[part]->size;
			partlen -= f->keys[idx].component[part]->size;
		}
		return 0;
	}
	cl = partlen > (int)(f->keys[idx].field->size) ? (int)(f->keys[idx].field->size) : partlen;
	return memcmp (keyarea, record + f->keys[idx].offset, cl);
}

#endif

/* Routines common to both ODBC and OCI interfaces */
#if defined(WITH_ODBC) || defined(WITH_OCI)

static char	high_value[6];	/* value to replace HIGH-VALUES in index */
static int	len_high_value = 0;

/* Is fld all the given character */
static int
isAllChar (void *data, int len, char c)
{
	char	*fld = (char*)data;
	while (len > 0 && *fld == c) {
		len--;
		fld++;
	}
	if (len == 0)
		return 1;
	return 0;
}

#ifdef COB_DEBUG_LOG
static char *
hex_dump (unsigned char *in, int len, char *out)
{
	char	*rtn = out;
	int		k;
	if (len <= 0) {
		strcpy(out,"e-m-p-t-y");
		return out;
	}
	while (len > 1
		&& in[len-1] == 0x00)
		len--;
	for(k=0; k < len && isprint(in[k]); k++);
	if (k == len) {
		if (len < 40) {
			sprintf(out,"'%.*s'",len,in);
		} else {
			sprintf(out,"'%.40s'...",in);
		}
		return out;
	}
	if (len > 30) len = 30;
	strcpy(out,"0x");
	out += 2;
	while (len > 0) {
		sprintf(out,"%02X",*in);
		len--;
		out += 2;
		in++;
	}
	return rtn;
}
#endif

static char *
getNum(char *p, int *val)
{
	int	v = 0;
	if (*p == ',') p++;
	while(isdigit (*p)) 
		v = v * 10 + (*p++ - '0');
	*val = v;
	if (*p == ',') p++;
	return p;
}

static char *
getPosLen(char *p, unsigned char *pos, unsigned char *len)
{
	int	v = 0;
	if (*p == ',') p++;
	while(isdigit (*p)) 
		v = v * 10 + (*p++ - '0');
	*pos = (unsigned char)v;
	v = 0;
	if (*p == ':') p++;
	while(isdigit (*p)) 
		v = v * 10 + (*p++ - '0');
	*len = (unsigned char)v;
	if (*p == ',') p++;
	return p;
}

static char *
getStr(char *p, char *str)
{
	char	qt;
	if (*p == '"' || *p == '\'') {
		qt = *p++;
	} else if (*p == ',') {
		*str = 0;
		p++;
		return p;
	} else {
		qt = 1;
	}
	while(*p != qt && *p >= ' ')
		*str++ = *p++;
	*str = 0;
	if (*p == qt) p++;
	if (*p == ',') p++;
	return p;
}

static char *
getPrm(char *p, char *str)
{
	char	qt;
	if (*p == '"' || *p == '\'') {
		qt = *p++;
	} else if (*p == ',') {
		*str = 0;
		p++;
		return p;
	} else {
		qt = ',';
	}
	while(*p != qt && *p >= ' ')
		*str++ = *p++;
	*str = 0;
	if (*p == qt) p++;
	if (*p == ',' && qt != ',') p++;
	return p;
}

static void
new_label (struct file_xfd *fx, int lbl)
{
	void	*xlbl;
	if (lbl < fx->nlbl)
		return;
	xlbl = cob_malloc (sizeof(int) * (lbl+1));
	if (fx->xlbl) {
		memcpy (xlbl, fx->xlbl, sizeof(int) * fx->nlbl);
		cob_free (fx->xlbl);
	}
	fx->xlbl = xlbl;
	fx->nlbl = lbl+1;
}

static struct map_xfd *
new_xfd (struct file_xfd *fx, int cmd)
{
	struct map_xfd *mx;
	if (fx->map == NULL) {
		mx = cob_malloc (sizeof(struct map_xfd));
		fx->nmap = 1;
		fx->map = mx;
	} else {
		mx = cob_malloc (sizeof(struct map_xfd) * (fx->nmap + 1));
		memcpy (mx, fx->map, sizeof(struct map_xfd) * fx->nmap);
		cob_free (fx->map);
		fx->map = mx;
		mx = &fx->map[fx->nmap];
		fx->nmap++;
	}
	mx->cmd = cmd;
	return mx;
}

static cob_pic_symbol *
bld_picture (cob_pic_symbol *pic, int sign, int digits, int scale)
{
	cob_pic_symbol *p = pic;
	if (sign == 0
	 && digits == 0
	 && scale == 0)
		return NULL;
	if (sign > 0) {
		p->symbol = '+';
		p->times_repeated = 1;
		++p;
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
	if (sign < 0) {
		p->symbol = '+';
		p->times_repeated = 1;
		++p;
	}
	p->symbol = '\0';
	return pic;
}

static void
bld_fields (struct map_xfd *mx, cob_file *fl)
{
	int		numsz;
	if (fl->organization == COB_ORG_RELATIVE
	 && mx->level == 0) {
		memcpy(&mx->recfld, fl->keys[0].field, sizeof(cob_field));
		memset(mx->sdata,0,mx->sqlsize);
		if (mx->sqlsize > 13)
			mx->sqlsize = 13;
		mx->sqlattr.type = COB_TYPE_NUMERIC_DISPLAY;
		mx->sqlattr.pic = bld_picture (mx->sqlpic, 0, 12, 0);
		mx->sqlfld.size = mx->sqlsize-1;
		mx->sqlfld.data = mx->sdata;
		mx->sqlfld.attr = &mx->sqlattr;
		mx->sqlattr.digits = 12;
		mx->sqlattr.scale = 0;
		mx->sqloutlen = (int)mx->sqlfld.size;
		return;
	}
	mx->recfld.size = mx->size;
	mx->recfld.data = fl->record->data + mx->offset;
	mx->recfld.attr = &mx->recattr;
	mx->recattr.digits = mx->digits;
	mx->recattr.scale = mx->scale;
	mx->sqlfld.size = mx->sqlsize-1;
	mx->sqlfld.data = mx->sdata;
	mx->sqlfld.attr = &mx->sqlattr;
	mx->sqlattr.digits = mx->digits;
	mx->sqlattr.scale = mx->scale;
	switch (mx->type) {
	case COB_XFDT_COMPS:
		mx->recattr.type = COB_TYPE_NUMERIC_BINARY;
		mx->recattr.flags = COB_FLAG_HAVE_SIGN;
#ifndef WORDS_BIGENDIAN
		mx->recattr.flags |= COB_FLAG_BINARY_SWAP;
#endif
		mx->sqlattr.type = mx->scale == 0 ? COB_TYPE_NUMERIC_DISPLAY : COB_TYPE_NUMERIC_EDITED;
		mx->sqlattr.flags = COB_FLAG_HAVE_SIGN|COB_FLAG_SIGN_SEPARATE|COB_FLAG_SIGN_LEADING;
		mx->sqlattr.pic = bld_picture (mx->sqlpic, 1, mx->digits, mx->scale);
		break;
	case COB_XFDT_COMPU:
		mx->recattr.type = COB_TYPE_NUMERIC_BINARY;
#ifndef WORDS_BIGENDIAN
		mx->recattr.flags |= COB_FLAG_BINARY_SWAP;
#endif
		mx->sqlattr.type = mx->scale == 0 ? COB_TYPE_ALNUM : COB_TYPE_NUMERIC_EDITED;
		mx->sqlattr.pic = mx->scale == 0 ? NULL : bld_picture (mx->sqlpic, 0, mx->digits, mx->scale);
		break;
	case COB_XFDT_COMPX:
		mx->recattr.type = COB_TYPE_NUMERIC_BINARY;
#ifndef WORDS_BIGENDIAN
		mx->recattr.flags |= COB_FLAG_BINARY_SWAP;
#endif
		mx->sqlattr.type = mx->scale == 0 ? COB_TYPE_NUMERIC_DISPLAY : COB_TYPE_NUMERIC_EDITED;
		break;
	case COB_XFDT_COMP5S:
		mx->recattr.type = COB_TYPE_NUMERIC_BINARY;
		mx->recattr.flags = COB_FLAG_HAVE_SIGN|COB_FLAG_REAL_BINARY;
		mx->sqlattr.type = mx->scale == 0 ? COB_TYPE_NUMERIC_DISPLAY : COB_TYPE_NUMERIC_EDITED;
		mx->sqlattr.flags = COB_FLAG_HAVE_SIGN|COB_FLAG_SIGN_SEPARATE|COB_FLAG_SIGN_LEADING;
		mx->sqlattr.pic = bld_picture (mx->sqlpic, 1, mx->digits, mx->scale);
		break;
	case COB_XFDT_COMP5U:
		mx->recattr.type = COB_TYPE_NUMERIC_BINARY;
		mx->recattr.flags = COB_FLAG_REAL_BINARY;
		mx->sqlattr.type = mx->scale == 0 ? COB_TYPE_NUMERIC_DISPLAY : COB_TYPE_NUMERIC_EDITED;
		mx->sqlattr.pic = bld_picture (mx->sqlpic, 0, mx->digits, mx->scale);
		break;
	case COB_XFDT_COMP5IDX:
	case COB_XFDT_COMP5REL:
		mx->recattr.type = COB_TYPE_NUMERIC_BINARY;
		mx->recattr.flags = COB_FLAG_REAL_BINARY;
		mx->sqlattr.type = COB_TYPE_NUMERIC_DISPLAY;
		mx->sqlattr.pic = bld_picture (mx->sqlpic, 0, mx->digits, mx->scale);
		break;
	case COB_XFDT_FLOAT:
		mx->recattr.type = mx->size < 7 ? COB_TYPE_NUMERIC_FLOAT : COB_TYPE_NUMERIC_DOUBLE;
		mx->sqlattr.type = COB_TYPE_NUMERIC_EDITED;
		mx->sqlattr.flags = COB_FLAG_HAVE_SIGN|COB_FLAG_SIGN_SEPARATE|COB_FLAG_SIGN_LEADING;
		mx->sqlattr.pic = bld_picture (mx->sqlpic, 1, mx->digits, mx->scale);
		break;
	case COB_XFDT_PACKS:
		mx->recattr.type = COB_TYPE_NUMERIC_PACKED;
		mx->recattr.flags = COB_FLAG_HAVE_SIGN;
		mx->sqlattr.type = mx->scale == 0 ? COB_TYPE_NUMERIC_DISPLAY : COB_TYPE_NUMERIC_EDITED;
		mx->sqlattr.flags = COB_FLAG_HAVE_SIGN|COB_FLAG_SIGN_SEPARATE|COB_FLAG_SIGN_LEADING;
		mx->sqlattr.pic = bld_picture (mx->sqlpic, 1, mx->digits, mx->scale);
		break;
	case COB_XFDT_PACKU:
		mx->recattr.type = COB_TYPE_NUMERIC_PACKED;
		mx->recattr.flags = COB_FLAG_REAL_BINARY;
		mx->sqlattr.type = mx->scale == 0 ? COB_TYPE_NUMERIC_DISPLAY : COB_TYPE_NUMERIC_EDITED;
		mx->sqlattr.pic = bld_picture (mx->sqlpic, 0, mx->digits, mx->scale);
		break;
	case COB_XFDT_COMP6:
		mx->recattr.type = COB_TYPE_NUMERIC_PACKED;
		mx->recattr.flags = COB_FLAG_NO_SIGN_NIBBLE;
		mx->sqlattr.type = mx->scale == 0 ? COB_TYPE_NUMERIC_DISPLAY : COB_TYPE_NUMERIC_EDITED;
		mx->sqlattr.pic = bld_picture (mx->sqlpic, 0, mx->digits, mx->scale);
		break;
	case COB_XFDT_PICA:
	case COB_XFDT_PICX:
	case COB_XFDT_VARX:
		mx->recattr.type = COB_TYPE_ALNUM;
		mx->sqlattr.type = COB_TYPE_ALNUM;
		break;
	case COB_XFDT_PICN:
	case COB_XFDT_PICW:
		mx->recattr.type = COB_TYPE_NATIONAL;
		mx->sqlattr.type = COB_TYPE_NATIONAL;
		break;
	case COB_XFDT_PIC9L:
		mx->recattr.type = COB_TYPE_NUMERIC_DISPLAY;
		mx->recattr.flags = COB_FLAG_HAVE_SIGN|COB_FLAG_SIGN_LEADING;
		mx->sqlattr.type = mx->scale == 0 ? COB_TYPE_NUMERIC_DISPLAY : COB_TYPE_NUMERIC_EDITED;
		mx->sqlattr.flags = COB_FLAG_HAVE_SIGN|COB_FLAG_SIGN_SEPARATE|COB_FLAG_SIGN_LEADING;
		mx->sqlattr.pic = bld_picture (mx->sqlpic, 1, mx->digits, mx->scale);
		break;
	case COB_XFDT_PIC9LS:
		mx->recattr.type = COB_TYPE_NUMERIC_DISPLAY;
		mx->recattr.flags = COB_FLAG_HAVE_SIGN|COB_FLAG_SIGN_SEPARATE|COB_FLAG_SIGN_LEADING;
		mx->sqlattr.type = mx->scale == 0 ? COB_TYPE_NUMERIC_DISPLAY : COB_TYPE_NUMERIC_EDITED;
		mx->sqlattr.flags = COB_FLAG_HAVE_SIGN|COB_FLAG_SIGN_SEPARATE|COB_FLAG_SIGN_LEADING;
		mx->sqlattr.pic = bld_picture (mx->sqlpic, 1, mx->digits, mx->scale);
		break;
	case COB_XFDT_PIC9T:
		mx->recattr.type = COB_TYPE_NUMERIC_DISPLAY;
		mx->recattr.flags = COB_FLAG_HAVE_SIGN|COB_FLAG_SIGN_SEPARATE;
		mx->sqlattr.type = mx->scale == 0 ? COB_TYPE_NUMERIC_DISPLAY : COB_TYPE_NUMERIC_EDITED;
		mx->sqlattr.flags = COB_FLAG_HAVE_SIGN|COB_FLAG_SIGN_SEPARATE|COB_FLAG_SIGN_LEADING;
		mx->sqlattr.pic = bld_picture (mx->sqlpic, 1, mx->digits, mx->scale);
		break;
	case COB_XFDT_PIC9TS:
		mx->recattr.type = COB_TYPE_NUMERIC_DISPLAY;
		mx->recattr.flags = COB_FLAG_HAVE_SIGN|COB_FLAG_SIGN_SEPARATE;
		mx->sqlattr.type = mx->scale == 0 ? COB_TYPE_NUMERIC_DISPLAY : COB_TYPE_NUMERIC_EDITED;
		mx->sqlattr.flags = COB_FLAG_HAVE_SIGN|COB_FLAG_SIGN_SEPARATE|COB_FLAG_SIGN_LEADING;
		mx->sqlattr.pic = bld_picture (mx->sqlpic, 1, mx->digits, mx->scale);
		break;
	case COB_XFDT_PIC9S:
		mx->recattr.type = COB_TYPE_NUMERIC_DISPLAY;
		mx->recattr.flags = COB_FLAG_HAVE_SIGN;
		mx->sqlattr.type = mx->scale == 0 ? COB_TYPE_NUMERIC_DISPLAY : COB_TYPE_NUMERIC_EDITED;
		mx->sqlattr.flags = COB_FLAG_HAVE_SIGN|COB_FLAG_SIGN_SEPARATE|COB_FLAG_SIGN_LEADING;
		mx->sqlattr.pic = bld_picture (mx->sqlpic, 1, mx->digits, mx->scale);
		break;
	case COB_XFDT_PIC9U:
		mx->recattr.type = COB_TYPE_NUMERIC_DISPLAY;
		mx->recattr.flags = 0;
		mx->sqlattr.type = mx->scale == 0 ? COB_TYPE_NUMERIC_DISPLAY : COB_TYPE_NUMERIC_EDITED;
		mx->sqlattr.flags = 0;
		mx->sqlattr.pic = bld_picture (mx->sqlpic, 0, mx->digits, mx->scale);
		break;
	}
	if (mx->dtfrm) {
		if (mx->dtfrm->hasTime && mx->dtfrm->hasDate)
			mx->sqlColSize = mx->sqlsize = 26;
		else
			mx->sqlColSize = mx->sqlsize = 16;
		mx->sqlfld.size = mx->sqlsize;
	} else if (mx->digits > 0) {		/* Set size of SQL numeric field exact */
		numsz = mx->digits;
		if (mx->sqlattr.flags & COB_FLAG_HAVE_SIGN)
			numsz++;
		if (mx->scale != 0)
			numsz++;
		if (numsz < mx->sqlsize-1)
			mx->sqlfld.size = numsz;
	}
	mx->sqloutlen = (int)mx->sqlfld.size;
}

/* 
 * Evaluate the expression and return the next position in table
 */
#define MAXVAL 16
/*static const char vop[10][5] = {".",">=",">","<=","<","=","!=","&&","||","!"}; */
static int
eval_expr (struct file_xfd *fx, int  pos)
{
	int		kdata,col,rslt;
	int		val[MAXVAL];
	char	buf[256];
	cob_field		temp;
	cob_field_attr	attr;
	COB_ATTR_INIT (COB_TYPE_NUMERIC_DISPLAY, 0, 0, 0, NULL);                 
	temp.data = (unsigned char *)buf;
	temp.attr = &attr;
	for (kdata=0; kdata < MAXVAL; kdata++)
		val[kdata] = 0;
	kdata = 0;
	while (fx->map[pos].cmd == XC_WHEN) {
		if (fx->map[pos].opcode == XO_AND) {
			kdata--;
			val[kdata-1] = val[kdata] && val[kdata-1];
		} else if (fx->map[pos].opcode == XO_OR) {
			kdata--;
			val[kdata-1] = val[kdata] || val[kdata-1];
		} else if (fx->map[pos].opcode == XO_NOT) {
			val[kdata-1] = !val[kdata-1];
		} else {
			col = fx->map[pos].colpos;
			temp.size = fx->map[col].sqlsize-1;
			if (temp.size >= sizeof(buf))
				temp.size = sizeof(buf)-1;
			memset(buf,0,temp.size+1);
			if (fx->map[pos].valnum) {
				COB_ATTR_INIT (COB_TYPE_NUMERIC_DISPLAY, 0, 0, 0, NULL);                 
				cob_move (&fx->map[col].recfld, &temp);
				rslt = atoi(fx->map[pos].value) - atoi(buf);
			} else {
				COB_ATTR_INIT (COB_TYPE_ALNUM, 0, 0, 0, NULL);                 
				cob_move (&fx->map[col].recfld, &temp);
				rslt = strcasecmp(fx->map[pos].value, buf);
			}
			if (rslt < 0) rslt = -1;
			if (rslt > 0) rslt = 1;
			switch (fx->map[pos].opcode) {
			case XO_EQ:
				if (rslt == 0)
					val[kdata++] = TRUE;
				else
					val[kdata++] = FALSE;
				break;
			case XO_NE:
				if (rslt != 0)
					val[kdata++] = TRUE;
				else
					val[kdata++] = FALSE;
				break;
			case XO_GT:
				if (rslt > 0)
					val[kdata++] = TRUE;
				else
					val[kdata++] = FALSE;
				break;
			case XO_GE:
				if (rslt >= 0)
					val[kdata++] = TRUE;
				else
					val[kdata++] = FALSE;
				break;
			case XO_LT:
				if (rslt < 0)
					val[kdata++] = TRUE;
				else
					val[kdata++] = FALSE;
				break;
			case XO_LE:
				if (rslt <= 0)
					val[kdata++] = TRUE;
				else
					val[kdata++] = FALSE;
				break;
			default:
				break;
			}
		}

		if (fx->map[pos].target > 0
		 && kdata > 0
		 && val[kdata-1]) {
			rslt = fx->map[pos].target;		/* Skip forward to target position */
			rslt = fx->xlbl[rslt];
			do {
				pos++;
				if (fx->map[pos].cmd == XC_DATA)
					fx->map[pos].setnull = 1;
			} while (pos < rslt);
			return rslt;
		}
		if (fx->map[pos].target > 0) {
			kdata = 0;
		}
		pos++;
	}
	return pos;
}

static int
cob_find_xfd_col (struct file_xfd *fx, char *colname)
{
	int		k;
	for (k=0; k < fx->nmap; k++) {
		if (fx->map[k].cmd == XC_DATA
		 && fx->map[k].colname
		 && strcasecmp (fx->map[k].colname, colname) == 0)
			return k;
	}
	return -1;
}

static int
getInt(char *data, int pos, int len)
{
	int	val;
	val = 0;
	while(len-- > 0 
	&& (isdigit(data[pos]) || data[pos] == ' ')) {
		if(data[pos] == ' ') {
			val = (val * 10);
			pos++;
		} else {
			val = (val * 10) + (data[pos++] - '0');
		}
	}
	return val;
}

static void
putNum(char *data, int pos, int len, int val)
{
	char	wrk[20];
	if(len <= 0)
		return;
	sprintf(wrk,"%0*d",len,val);
	memcpy(data+pos,wrk,len);
}

static short daysinmonth[] = {
	31, 28, 31, 30, 31, 30,
	31, 31, 30, 31, 30, 31
};
static int				/* 1 if leap year, 0 otherwise */
getDays(
	short	*days,		/* Fill in array of 12 shorts with days in each month */
	int		year) 		/* The year for which to compute leap and days per month */
{
	int k,leap;
	leap = ( year % 4 == 0 ) 
		&& ( ( year % 100 != 0 ) || ( year % 400 == 0 ) );
	for(k=0; k < 12; k++)
		days[k] = daysinmonth[k];
	days[1] += leap;
	return leap;
}

/*
	Convert DISPLAY digits into SQL Internal date format
*/
static int					/* Return length of date text */
convert_to_date(
	struct db_state	*db,
	struct sql_date	*df,	/* Date format */
	char		*datain,	/* Display data from record */
	int			inlen,		/* Length of 'datain' */
	char		*dataout,	/* Output date in YYYY-MM-DD HH:MM:SS.hhh format */
	int			outlen,		/* Length of output area */
	int			*dateOk)
{
	struct {
		short	year;
		short	month;
		short	day;
		short	hour;
		short	minute;
		short	second;
		short	hund;
	} date;
	int		k,secsaftmid;
	short	days[12],cc;
	int		bDateBad = FALSE;
	int		bDateZero = TRUE;
	int		bDateNines = TRUE;

	for(k=0; k < inlen && isspace(datain[k]); k++);
	while(k < inlen 
	&& (bDateZero || bDateNines)) {
		if(datain[k] == 0) {
			k++;
			if(k > 2)
				break;
		}
		if(datain[k] != '0') {
			bDateZero = FALSE;
		}
		if(datain[k] != '9') {
			bDateNines = FALSE;
		}
		k++;
	}

	memset(&date,0,sizeof(date));
	date.year	= getInt(datain,df->yyPos,df->yyLen);
	date.month	= getInt(datain,df->mmPos,df->mmLen);
	if (df->ddLen > 0)
		date.day = getInt(datain,df->ddPos,df->ddLen);
	else
		date.day = 1;
	if (df->ccLen > 0) {
		cc = getInt(datain,df->ccPos,df->ccLen);
		date.year = date.year + cc * 100;
	}
	if(date.year == 0
	&& date.month == 0
	&& date.day == 0)		/* YYMMDD is all ZERO so assume date is ZERO */
		bDateZero = TRUE;
	if (df->ccLen == 0) {
		if(df->yyRule == '+')
			date.year += df->yyAdj;
		else if(df->yyRule == '%')
			date.year += ((date.year < df->yyAdj) ? 2000 : 1900);
		else if (df->yyLen < 4)
			date.year += 2000;
	}
	getDays(days,date.year);
	if(df->ddLen > 2
	&& !bDateZero
	&& !bDateNines) {		/* Must be day of year */
		/* Convert day of year into MMDD */
		date.month = 1;
		for(k=0; k < 12 && date.day > days[k]; k++) {
			date.day -= days[k];
			date.month++;
		}
	}
	if(df->hhLen > 0) {
		date.hour	= getInt(datain,df->hhPos,df->hhLen);
		date.minute	= getInt(datain,df->miPos,df->miLen);
		date.second	= getInt(datain,df->ssPos,df->ssLen);
		if (df->huLen > 0)
			date.hund	= getInt(datain,df->huPos,df->huLen);
		if(date.hour > 23
		|| date.minute > 59
		|| date.second > 59)
			bDateBad = TRUE;
	} else if(df->ssLen > 4) {		/* Seconds past Midnight */
		secsaftmid	= getInt(datain,df->ssPos,df->ssLen);
		date.hour	= (secsaftmid / (60*60));
		secsaftmid	= secsaftmid - (date.hour * (60*60));
		date.minute	= secsaftmid / 60;
		date.second	= secsaftmid - (date.minute * 60);
	}
	if (df->hasDate) {
		if(date.month < 1 || date.month > 12)
			bDateBad = TRUE;
		if(date.day < 1 || date.day > days[date.month-1])
			bDateBad = TRUE;
	}
	if(bDateZero) {
		if(db->mssql)
			date.year = 1753;
		else if(db->mysql)
			date.year = 1000;
		else
			date.year = 1;
		date.month = 1;
		date.day = 1;
		date.hour = date.minute = date.second = 0;
		bDateBad = FALSE;
	} else if(bDateNines) {
		date.year = 9999;
		date.month = 12;
		date.day = 31;
		date.hour = 23;
		date.minute = 59;
		date.second = 59;
		bDateBad = FALSE;
	}
	if(db->isodbc
	&& bDateBad) {
		if(date.hour > 23)
			date.hour = 23;
		if(date.minute > 59)
			date.minute = 59;
		if(date.second > 59)
			date.second = 59;
		if(date.month < 1)
			date.month = 1;
		if(date.month > 12)
			date.month = 12;
		if(date.day < 1)
			date.day = 1;
		if(date.day > days[date.month-1])
			date.day = days[date.month-1];
	} else if(bDateBad) { 
		if(date.month < 1)		/* Fix Date for Oracle */
			date.month = 1;
		if(date.month > 12)
			date.month = 12;
		if(date.day < 1)
			date.day = 1;
		if(date.day > days[date.month-1])
			date.day = days[date.month-1];
	}
	*dateOk = !bDateBad;
	if(db->oracle) {
		if(df->hasDate && df->hasTime) {
			k = sprintf(dataout,"%04d-%02d-%02d %02d:%02d:%02d.%06d",
								date.year,date.month,date.day,
								date.hour,date.minute,date.second,date.hund);
		} else {
			if (date.year == 0) date.year = 2000;
			if (date.month == 0) date.month = 1;
			if (date.day == 0)	date.day = 1;
			k = sprintf(dataout,"%04d%02d%02d%02d%02d%02d",
								date.year,date.month,date.day,
								date.hour,date.minute,date.second);
		}
	} else {
		if (df->hasTime && !df->hasDate) {
			if(df->huLen > 0)
				k = sprintf(dataout,"%02d:%02d:%02d.%03d",
								date.hour,date.minute,date.second,date.hund);
			else
				k = sprintf(dataout,"%02d:%02d:%02d",
								date.hour,date.minute,date.second);
		} else
		if(df->hasTime && df->hasDate) {
			if (date.year == 0) date.year = 2000;
			if (date.month == 0) date.month = 1;
			if (date.day == 0)	date.day = 1;
			if (db->mysql)
				k = sprintf(dataout,"%04d-%02d-%02d %02d:%02d:%02d",
								date.year,date.month,date.day,
								date.hour,date.minute,date.second);
			else
				k = sprintf(dataout,"%04d-%02d-%02d %02d:%02d:%02d.%03d",
								date.year,date.month,date.day,
								date.hour,date.minute,date.second,date.hund);
		} else {
			k = sprintf(dataout,"%04d-%02d-%02d",date.year,date.month,date.day);
		}
	}
	return k;
}

/*
	Convert Date from SQL back into format used by application 
*/
static int
convert_from_date(
	struct db_state	*db,
	struct sql_date	*df,	/* Date format */
	char		*sqldate,	/* DATE as returned from SQL */
	int			inlen,		/* length of the SQL date data */
	char		*dataout,	/* Output date in format defined by 'sql_date' */
	int			outlen)		/* Length of output area */
{
	struct {
		short	year;
		short	month;
		short	day;
		short	hour;
		short	minute;
		short	second;
		short	hund;
	} date;
	int		k,dlen,day,minyear,secsaftmid;
	short	days[12];
	char	*pd,pdata[40];

	if(db->mssql)
		minyear = 1753;
	else if(db->mysql)
		minyear = 1000;
	else
		minyear = 1;
	memset(&date,0,sizeof(date));
	pd = sqldate;
	if(inlen >= sizeof(pdata))
		pd[sizeof(pdata)-1] = 0;
	for(dlen=k=0; pd[k] != 0 && k < inlen; k++) {
		if(isdigit(pd[k]))
			pdata[dlen++] = pd[k];
	}
	while(dlen < 20)
		pdata[dlen++] = '0';
	pdata[dlen] = 0;
	if(db->oracle) {
		date.year	= getInt(pdata,0,4);
		date.month	= getInt(pdata,4,2);
		date.day	= getInt(pdata,6,2);
		date.hour	= getInt(pdata,8,2);
		date.minute	= getInt(pdata,10,2);
		date.second	= getInt(pdata,12,2);
		date.hund	= getInt(pdata,14,6);
	} else {
		if (sqldate[2] == ':'
		 && sqldate[5] == ':') {
			date.hour	= getInt(sqldate,0,2);
			date.minute	= getInt(sqldate,3,2);
			date.second	= getInt(sqldate,6,2);
			if(((char*)sqldate)[8] == '.') {
				if (db->mssql)
					date.hund	= getInt(sqldate,9,6);
				else
					date.hund	= getInt(sqldate,9,3);
			}
		} else 
		if (sqldate[4] == '-'
		 && sqldate[7] == '-'
		 && sqldate[10] < ' ') {
			date.year	= getInt(sqldate,0,4);
			date.month	= getInt(sqldate,5,2);
			date.day	= getInt(sqldate,8,2);
		} else {
			date.year	= getInt(sqldate,0,4);
			date.month	= getInt(sqldate,5,2);
			date.day	= getInt(sqldate,8,2);
			if(dlen > 10) {
				date.hour	= getInt(sqldate,11,2);
				date.minute	= getInt(sqldate,14,2);
				date.second	= getInt(sqldate,17,2);
				if(((char*)sqldate)[19] == '.')
					date.hund	= getInt(sqldate,20,3);
			}
		}
	}
	if(date.year == minyear
	&& date.month == 1
	&& date.day == 1
	&& date.hour == 0) {
		memset(dataout,'0',outlen);
		dataout[outlen] = 0;
		return outlen;
	}
	if(date.year == 9999
	&& date.month == 12
	&& date.day == 31) {
		memset(dataout,'9',outlen);
		dataout[outlen] = 0;
		return outlen;
	}
	if(df->yyRule == '+') {
		date.year -= df->yyAdj;
	} else if(df->yyRule == '#') {	/* ER TDATE$ format */
		date.year -= df->yyAdj;
	} else if(df->yyRule == '%' 
		|| (df->yyLen < 4 && df->ccLen == 0)) {
		if(date.year >= 2000)
			date.year -= 2000;
		else if(date.year >= 1900)
			date.year -= 1900;
	}
	memset(dataout,0,outlen);
	if (df->yyLen == 2)
		putNum(dataout,df->yyPos,df->yyLen,date.year%100);
	else
		putNum(dataout,df->yyPos,df->yyLen,date.year);
	if (df->ccLen > 0)
		putNum(dataout,df->ccPos,df->ccLen,date.year/100);
	if(df->ddLen > 2) {	/* Must be day of year */
		getDays(days,date.year);
		/* Convert MMDD into day of year */
		day = 0;
		for(k=1; k < date.month; k++) {
			day += days[k-1];
		}
		day += date.day;
		putNum(dataout,df->ddPos,df->ddLen,day);
	} else {
		putNum(dataout,df->mmPos,df->mmLen,date.month);
		putNum(dataout,df->ddPos,df->ddLen,date.day);
	}
	if(df->hhLen > 0) {
		putNum(dataout,df->hhPos,df->hhLen,date.hour);
		putNum(dataout,df->miPos,df->miLen,date.minute);
		putNum(dataout,df->ssPos,df->ssLen,date.second);
		if(df->huLen > 0)
			putNum(dataout,df->huPos,df->huLen,date.hund);
	} else if(df->ssLen > 4) {		/* Seconds past midnight */
		secsaftmid = date.hour * (60*60) + date.minute * 60 + date.second;
		putNum(dataout,df->ssPos,df->ssLen,secsaftmid);
	}
	return strlen(dataout);
}

/*
 * Read the 'file.xd' and create table of fields, etc
 * 'indsize' is the size of the SQL Indicator field
 */
struct file_xfd *
cob_load_xfd (struct db_state *db, cob_file *fl, char *alt_name, int indsize, int skiprid)
{
	char	xfdbuf[COB_NORMAL_BUFF],*sdir,*fname,*p,*mp;
	char	colname[80], tblname[80], asgname[256];
	char	dups[4], sup[4], supchar[80], rowcol[90];
	char	opcode[16],tstval[48], commachr[8], decchr[8];
	int		signopt;
	int		i,j,k,lbl,keyn,xfdver;
	int		ncols, lncols, lndata;
	unsigned char	supch, qt;
	struct file_xfd *fx;
	struct map_xfd	*mx = NULL;
	FILE	*fi;

	xfdver = 1;
	ncols = lncols = lndata = 0;
	if (indsize < 4)
		indsize = sizeof(long);
	if (fl->xfdschema != NULL)
		sdir = (char*)fl->xfdschema;
	else if ((sdir = getenv("COB_SCHEMA_DIR")) == NULL)
		sdir = (char*)COB_SCHEMA_DIR;
	if (alt_name != NULL) {
		fname = alt_name;
	} else if(fl->xfdname != NULL
		&& fl->xfdname[0] > ' ') {
		fname = (char*)fl->xfdname;
	} else if (fl->assign) {
		cob_field_to_string (fl->assign, asgname, (size_t)255, CCM_NONE);
		if ((p = strrchr(asgname, SLASH_CHAR)) != NULL)
			fname = p + 1;
		else
			fname = asgname;
	} else {
		fname = (char*)fl->select_name;
	}
	i = k = snprintf (xfdbuf,sizeof(xfdbuf)-4,"%s%s",sdir,SLASH_STR);
	for(j=0; fname[j] != 0 && k < (sizeof(xfdbuf)-4); j++) {
		if (fname[j] == '-')
			xfdbuf[k] = '_';
		else if (isalnum(fname[j]))
			xfdbuf[k++] = tolower(fname[j]);
	}
	xfdbuf[k] = 0;
	strcpy(tblname,&xfdbuf[i]);
	strcpy(&xfdbuf[k],".xd");
	fi = fopen (xfdbuf,"r");
	if (fi == NULL
	 && fname != (char*)fl->select_name) {
		fname = (char*)fl->select_name;
		k = i;
		for(j=0; fname[j] != 0 && k < (sizeof(xfdbuf)-4); j++) {
			if (fname[j] == '-')
				xfdbuf[k] = '_';
			else if (isalnum(fname[j]))
				xfdbuf[k++] = tolower(fname[j]);
		}
		xfdbuf[k] = 0;
		strcpy(tblname,&xfdbuf[i]);
		strcpy(&xfdbuf[k],".xd");
		fi = fopen (xfdbuf,"r");
	}
	if (fi == NULL) {
		cob_runtime_warning (_("Error '%s' opening '%s'"),cob_get_strerror (),xfdbuf);
		return NULL;
	}
	sprintf(rowcol,"rid_%s",tblname);
	fx = cob_malloc (sizeof (struct file_xfd));
	/* Is there any need for the rid column */
	if (fl->flag_read_no_02 == 1
	 && fl->limitreads == 0) {
		fx->hasrid = 0;
		skiprid = 1;
	}
	while(fgets (xfdbuf,sizeof(xfdbuf),fi) != NULL) {
		if (xfdbuf[0] == '#'
		 || xfdbuf[0] == '*')
			continue;
		xfdbuf[sizeof(xfdbuf)-1] = 0;
		for(k=strlen (xfdbuf); k > 0 
					&& (xfdbuf[k-1] == '\n' 
					 || xfdbuf[k-1] == '\r' 
					 || isspace (xfdbuf[k-1])); )
			xfdbuf[--k] = 0;
		if (xfdbuf[0] == 'F') {	/* Data field definition */
			mx = new_xfd (fx, XC_DATA);
			p = getNum (&xfdbuf[2], &mx->offset);
			p = getNum (p, &mx->size);
			p = getNum (p, &mx->type);
			p = getNum (p, &mx->sqlsize);
			p = getNum (p, &mx->digits);
			p = getNum (p, &mx->scale);
			if (isdigit(*p)) {
				p = getNum (p, &k);
				mx->dtfrm = fx->date[k];
			} else if (*p == ',') {
				p++;
			} else {
				p = getNum (p, &k);
			}
			if (mx->digits > 0) {
				mx->valnum = TRUE;
				mx->sqlColSize = mx->digits + 2;
				if (mx->scale > 0) {
					mx->sqlDecimals = mx->scale;
					mx->sqlColSize++;
				}
			} else {
				mx->valnum = FALSE;
				mx->sqlColSize = mx->sqlsize;
			}
			p = getNum (p, &mx->level);
			p = getStr (p, colname);
			if (skiprid
			 && strcmp (colname,rowcol) == 0) {
				fx->nmap--;
				continue;
			}
			mx->colname = cob_strdup (colname);
			mx->lncolname = strlen(colname);
			if (mx->lncolname > fx->maxcolnmln)
				fx->maxcolnmln = mx->lncolname;
			if (mx->dtfrm) {
				if (mx->dtfrm->hasTime && mx->dtfrm->hasDate)
					mx->sqlColSize = mx->sqlsize = 26;
				else
					mx->sqlColSize = mx->sqlsize = 16;
			}
			ncols++;
			lncols += mx->lncolname;
			fx->lncols = lncols;
			fx->ncols = ncols;
			k = (mx->sqlsize + sizeof(long) + 4) / sizeof(long);
			k = k * sizeof(long);
			lndata += k;
			continue;
		}
		if (xfdbuf[0] == 'L') {	/* Define a new label */
			p = getNum (&xfdbuf[2], &lbl);
			new_label (fx, lbl);
			if (fx->map[fx->nmap-1].target == lbl
			 && fx->map[fx->nmap-1].cmd == XC_GOTO) {
				fx->nmap--;					/* remove useless GOTO */
			} else {
				fx->xlbl[lbl] = fx->nmap;
			}
			continue;
		}
		if (xfdbuf[0] == 'G') {		/* Goto a label */
			p = getNum (&xfdbuf[2], &lbl);
			new_label (fx, lbl);
			mx = new_xfd (fx, XC_GOTO);
			mx->target = lbl;
			continue;
		}
		if (xfdbuf[0] == 'C') {		/* Condition to test */
			p = getNum (&xfdbuf[2], &lbl);
			if (lbl > 0)
				new_label (fx, lbl);
			mx = new_xfd (fx, XC_WHEN);
			mx->target = lbl;
			k = strlen(p);
			while (p[k-1] == '\r'
				|| p[k-1] == '\n'
				|| p[k-1] == ' ')
				p[--k] = 0;
			p = getPrm (p,opcode);
			p = getPrm (p,colname);
			supch = '\'';
			mx->valnum = 0;
			if (*p == '\'' || *p == '"') {
				supch = *p;
			} else if (isdigit(*p)) {
				mx->valnum = 1;
			}
			p = getPrm (p,tstval);
			if (colname[0] > ' ') {
				mx->colname = cob_strdup (colname);
				mx->lncolname = strlen(colname);
			}
			if (tstval[0] >= ' ') {
				mx->value = cob_strdup (tstval);
				mx->lnvalue = strlen(tstval);
			}
			if (strcmp(opcode,"!=") == 0)
				mx->opcode = XO_NE;
			else if (strcmp(opcode,"=") == 0)
				mx->opcode = XO_EQ;
			else if (strcmp(opcode,">=") == 0)
				mx->opcode = XO_GE;
			else if (strcmp(opcode,"<=") == 0)
				mx->opcode = XO_LE;
			else if (strcmp(opcode,"<") == 0)
				mx->opcode = XO_LT;
			else if (strcmp(opcode,">") == 0)
				mx->opcode = XO_GT;
			else if (strcmp(opcode,"!") == 0)
				mx->opcode = XO_NOT;
			else if (strcmp(opcode,"&&") == 0)
				mx->opcode = XO_AND;
			else if (strcmp(opcode,"||") == 0)
				mx->opcode = XO_OR;
			else
				mx->opcode = XO_NULL;
			continue;
		}
		if (xfdbuf[0] == 'K') {		/* Key definition */
			p = getNum (&xfdbuf[2], &keyn);
			if (keyn >= MAXNUMKEYS) {
				cob_runtime_error (_("%s XFD has more than %d indexes!"),
								fl->select_name,MAXNUMKEYS-1);
				cob_stop_run (1);
			}
			p = getPrm (p, dups);
			if (dups[0] == 'R') {		/* Used for unique record number */
				if (skiprid)
					continue;
				fx->hasrid = 1;
				continue;
			}
			p = getPrm (p, sup);
			if (keyn+1 > fx->nkeys)
				fx->nkeys = keyn+1;
			qt = *p;
			p = getPrm (p, supchar);
			if (memcmp(supchar,"0x",2) == 0) {
				supch = (unsigned char) strtol (supchar, NULL, 16);
			} else {
				supch = supchar[0];
			}
			if (fx->key[keyn] != NULL) {
				cob_runtime_error (_("%s XFD has duplicate index %d !"),
								fl->select_name,keyn);
				cob_stop_run (1);
			}
			fx->key[keyn] = cob_malloc (sizeof (struct key_xfd));
			if (dups[0] == 'Y')
				fx->key[keyn]->dups = TRUE;
			else
				fx->key[keyn]->dups = FALSE;
			if (sup[0] == 'Y')
				fx->key[keyn]->sup = TRUE;
			else
				fx->key[keyn]->sup = FALSE;
			if (qt == '"') {
				fx->key[keyn]->str_sup = (unsigned char*)cob_strdup (supchar);
			} else {
				fx->key[keyn]->supchar = supch;
			}
			do {
				p = getPrm (p, colname);
				if (skiprid
				 && strcmp (colname,rowcol) == 0)
					continue;
				if ((k = cob_find_xfd_col (fx, colname)) < 0) {
					cob_runtime_error (_("%s XFD index %d has %s undefined!"),
									fl->select_name,keyn,colname);
					cob_stop_run (1);
				}
				fx->key[keyn]->col[fx->key[keyn]->ncols] = k;
				fx->key[keyn]->ncols++;
				fx->key[keyn]->lncols += fx->map[k].lncolname;
				fx->map[k].notnull = TRUE;
				fx->map[k].iskey = TRUE;
				if (fx->key[keyn]->ncols >= MAXKEYCOLS) {
					cob_runtime_error (_("%s XFD index %d has too many columns!"),
									fl->select_name,keyn);
					cob_stop_run (1);
				}
			} while(*p > ' ');
			continue;
		}
		if (xfdbuf[0] == 'D') {		/* Date format */
			struct sql_date *df;
			p = getNum (&xfdbuf[2], &k);
			if (k <= fx->ndate) {
				df = cob_malloc (sizeof(struct sql_date));
				fx->date[k] = df;
				p = getPrm (p, df->format);
				p = getNum (p, &k);
				df->digits = (unsigned char)k;
				p = getNum (p, &k);
				df->hasDate = (unsigned char)k;
				p = getNum (p, &k);
				df->hasTime = (unsigned char)k;
				p = getPrm (p,opcode);
				df->yyRule = (unsigned char)opcode[0];
				if (df->yyRule < ' ')
					df->yyRule = ' ';
				p = getNum (p, &k);
				df->yyAdj = (short)k;
				p = getPosLen (p, &df->yyPos,&df->yyLen);
				p = getPosLen (p, &df->mmPos,&df->mmLen);
				p = getPosLen (p, &df->ddPos,&df->ddLen);
				p = getPosLen (p, &df->hhPos,&df->hhLen);
				p = getPosLen (p, &df->miPos,&df->miLen);
				p = getPosLen (p, &df->ssPos,&df->ssLen);
				p = getPosLen (p, &df->ccPos,&df->ccLen);
				p = getPosLen (p, &df->huPos,&df->huLen);
			}
			continue;
		}
		if (xfdbuf[0] == 'H') {		/* Header line */
			p = getNum (&xfdbuf[2], &xfdver);
			p = getPrm (p, tblname);
			p = getNum (p, &fx->ndate);
			p = getPrm (p, commachr);
			p = getPrm (p, decchr);
			p = getNum (p, &signopt);
			p = getNum (p, &fx->fileorg);
			fx->date = cob_malloc (sizeof(void*) * (fx->ndate + 1));
			continue;
		}
	}
	fx->tablename = cob_strdup (tblname);
	fx->lnind = indsize;
	fx->fl = fl;
	/*
	 * Assign storage for SQL data and 'indicator'
	 */
	fx->sqlbf = cob_malloc (lndata + ((ncols + 2) * indsize));
	mp = (char*)&fx->sqlbf[lndata + indsize];
	j = 0;
	for (i=0; i < fx->nmap; i++) {
		if (fx->map[i].cmd == XC_DATA) {
			k = (fx->map[i].sqlsize + sizeof(long) + 4) / sizeof(long);
			k = (k * sizeof(long));
			fx->map[i].sdata = &fx->sqlbf[j];
			if (fx->map[i].type == COB_XFDT_COMP5IDX
			 || fx->map[i].type == COB_XFDT_COMP5REL)
				fx->precnum = &fx->sqlbf[j];
			j += k;
			memset(mp,0,indsize);
			fx->map[i].ind = (void*)mp;
			mp += indsize;
			bld_fields (&fx->map[i],fl);
		} else if (fx->map[i].cmd == XC_WHEN) {
			if (fx->map[i].colname) {
				if ((lbl = cob_find_xfd_col (fx, fx->map[i].colname)) < 0) {
					cob_runtime_error (_("%s XFD index %d has %s undefined!"),
									fl->select_name,keyn,colname);
					cob_stop_run (1);
				}
				fx->map[i].colpos = lbl;
			}
		}
	}
	fclose(fi);
	return fx;
}

/*
 * Read the 'file.ddl' and save CREATE TABLE/INDEX
 */
void 
cob_load_ddl (struct db_state  *db, struct file_xfd *fx)
{
	char	xfdbuf[COB_NORMAL_BUFF],*sdir,*p;
	int		j,k, idx, ctsz, cisz;
	FILE	*fi,*fo;

	if (fx->create_table)
		cob_free (fx->create_table);
	fx->create_table = NULL;
	if ((sdir = getenv("COB_SCHEMA_DIR")) == NULL)
		sdir = (char*)COB_SCHEMA_DIR;
	k = sprintf (xfdbuf, "%s%s%s.ddl",sdir,SLASH_STR,fx->tablename);
	fi = fopen (xfdbuf,"r");
	if (fi == NULL) {
		fi = fopen (xfdbuf,"w");
		cob_xfd_to_ddl (db, fx, fi) ;
		fclose(fi);
		fi = fopen (xfdbuf,"r");
	}
	if (fi == NULL) {
		cob_runtime_warning (_("Error '%s' opening '%s'"),cob_get_strerror (),xfdbuf);
		return;
	}
	sprintf (xfdbuf, "%s%s%s.",sdir,SLASH_STR,fx->tablename);
	if (db->sqlite) {
		strcat(xfdbuf,"sql");
	} else if (db->mssql) {
		strcat(xfdbuf,"mssql");
	} else if (db->postgres) {
		strcat(xfdbuf,"psql");
	} else if (db->mysql) {
		strcat(xfdbuf,"mysql");
	} else if (db->isoci) {
		strcat(xfdbuf,"isoci");
	} else {
		strcat(xfdbuf,"odbc");
	}
	fo = fopen (xfdbuf,"w");
	cisz = 128;
	ctsz = 128;
	idx = -1;
	fx->create_table = cob_malloc (ctsz);
	fx->lncreate = 0;
	for (k=0; k < fx->nkeys; k++) {
		if (fx->key[k]->create_index)
			cob_free (fx->key[k]->create_index);
		fx->key[k]->lncreate = 0;
		fx->key[k]->create_index = cob_malloc (cisz);
	}
	while(fgets (xfdbuf,sizeof(xfdbuf),fi) != NULL) {
		if (xfdbuf[0] == '#'
		 || xfdbuf[0] == '-')
			goto getnext;
		xfdbuf[sizeof(xfdbuf)-1] = 0;
		for(k=strlen (xfdbuf); k > 0 
					&& (xfdbuf[k-1] == '\n' 
					 || xfdbuf[k-1] == '\r' 
					 || isspace (xfdbuf[k-1])); )
			xfdbuf[--k] = 0;
		for (k=0; isspace(xfdbuf[k]); k++);
		for (j=0; xfdbuf[k] != 0; ) {
			if (xfdbuf[k] == ' '
			 && xfdbuf[k+1] == ' ') {
				k++;
				continue;
			}
			xfdbuf[j++] = xfdbuf[k++];
		}
		xfdbuf[j] = 0;
		if (strncasecmp(xfdbuf,"DROP ",5) == 0)
			goto getnext;
		if (strncasecmp(xfdbuf,"CREATE TABLE ",13) == 0) {
			idx = -1;
		} else if (strncasecmp(xfdbuf,"CREATE SEQUENCE ",16) == 0) {
			if (db->mssql)
				continue;
			if (xfdbuf[j-1] == ';')
				xfdbuf[--j] = 0;
			fx->create_sequence = cob_strdup (xfdbuf);
			strcat(xfdbuf,";");
			goto getnext;
		} else if (strncasecmp(xfdbuf,"CREATE UNIQUE INDEX ",20) == 0) {
			p = &xfdbuf[20];
			if (strncasecmp (p, "pk_",3) == 0)
				idx = 0;
			else if (p[0] == 'k' && isdigit(p[1]))
				idx = atoi(&p[1]);
		} else if (strncasecmp(xfdbuf,"CREATE INDEX ",13) == 0) {
			p = &xfdbuf[13];
			if (p[0] == 'k' && isdigit(p[1]))
				idx = atoi(&p[1]);
		}
		if (idx == -1) {
			if (fx->lncreate + j >= ctsz-2) {
				fx->create_table = cob_realloc (fx->create_table, ctsz, ctsz + 256);
				ctsz += 256;
			}
			if (db->isoci) {
				if ((p = cob_str_case_str (xfdbuf," BIGINT ")) != NULL) {
					memcpy (p," INT    ",8);
				}
				if ((p = cob_str_case_str (xfdbuf," BINARY(")) != NULL) {
					memcpy (p,"    RAW(",8);
				} else
				if ((p = cob_str_case_str (xfdbuf," BINARY ")) != NULL) {
					memcpy (p,"    RAW ",8);
				}
				if ((p = cob_str_case_str (xfdbuf," TIME,")) != NULL) {
					memcpy (p," DATE",5);
				}
			} else if (db->mssql) {
				if ((p = cob_str_case_str (xfdbuf," INTEGER DEFAULT seq_")) != NULL) {
					fx->create_table = cob_realloc (fx->create_table, ctsz, ctsz + 256);
					ctsz += 256;
					strcpy (p," INT IDENTITY");
					j = strlen (xfdbuf);
				}
			} else if (db->sqlite) {
				if ((p = cob_str_case_str (xfdbuf," BIGINT ")) != NULL) {
					k = strlen (xfdbuf);
					memmove (p+9, p+8, k - (p - xfdbuf) + 1);
					memcpy (p," INTEGER ",9);
				}
				if ((p = cob_str_case_str (xfdbuf," INTEGER DEFAULT seq_")) != NULL) {
					strcpy (p," INTEGER PRIMARY KEY ");
					j = strlen (xfdbuf);
				}
			} else if (db->mysql) {
				if ((p = cob_str_case_str (xfdbuf," INTEGER DEFAULT seq_")) != NULL) {
					fx->create_table = cob_realloc (fx->create_table, ctsz, ctsz + 256);
					ctsz += 256;
					strcpy (p," INT NOT NULL AUTO_INCREMENT PRIMARY KEY");
					j = strlen (xfdbuf);
				}
			} else if (db->postgres) {
				if ((p = cob_str_case_str (xfdbuf," INTEGER DEFAULT seq_")) != NULL) {
					sprintf (p," BIGINT DEFAULT NEXTVAL('seq_%s')",fx->tablename);
					j = strlen (xfdbuf);
				}
			}
			strcpy(&fx->create_table[fx->lncreate], xfdbuf);
			fx->lncreate += j;
			if (fx->create_table[fx->lncreate-1] == ';') {
				fx->create_table[fx->lncreate-1] = 0;
				fx->create_table = cob_realloc (fx->create_table, ctsz, fx->lncreate + 2);
				ctsz = fx->lncreate;
				fx->lncreate--;
			}
			fx->lncreate = strlen(fx->create_table);
		} else {
			strcpy (&fx->key[idx]->create_index[fx->key[idx]->lncreate], xfdbuf);
			fx->key[idx]->lncreate += j;
			if (fx->key[idx]->create_index[fx->key[idx]->lncreate-1] == ';') {
				fx->key[idx]->create_index[fx->key[idx]->lncreate-1] = 0;
				fx->key[idx]->create_index = cob_realloc (fx->key[idx]->create_index,
								cisz, fx->key[idx]->lncreate + 2);
				fx->key[idx]->lncreate--;
			}
		}
getnext:
		if (fo) fprintf(fo,"%s\n",xfdbuf);
	}
	fclose (fi);
	if (fo) fclose (fo);
	return;
}

void
cob_sql_dump_stmt (struct db_state	*db, char *stmt, int doall)
{
#ifndef COB_DEBUG_LOG
	COB_UNUSED (db);
	COB_UNUSED (stmt);
	COB_UNUSED (doall);
#else
	int	j,k,len,frm;
	len = strlen(stmt);
	if (db->dbStatus != 0)
		DEBUG_LOG("db",("DB Status %d : %s\n",db->dbStatus,db->odbcState));
	if (strncasecmp(stmt,"SELECT ",7) == 0) {
		if (doall) {
			for (frm=0; frm < len; frm++)
				if(strncasecmp(&stmt[frm]," FROM ",6) == 0)
					break;
			frm++;
			k = 0;
			while ((frm - k) > 60) {
				for(j=k+59; j > (k + 20)
					&& stmt[j] != ','
					&& stmt[j] != ' '; j--);
				DEBUG_LOG("db",("%.*s\n",j+1-k,&stmt[k]));
				k = j + 1;
			}
			if (k < frm)
				DEBUG_LOG("db",("%.*s\n",frm-k,&stmt[k]));
			k = frm;
		} else {
			DEBUG_LOG("db",("%.60s %s\n",stmt,len>60?"...":""));
			for (k=50; k < len; k++)
				if(strncasecmp(&stmt[k]," FROM ",6) == 0)
					break;
			k++;
		}
		while ((len - k) > 60) {
			for(j=k+59; j > (k + 20)
				&& stmt[j] != ','
				&& stmt[j] != ' '; j--);
			DEBUG_LOG("db",("%.*s\n",j+1-k,&stmt[k]));
			k = j + 1;
		}
		if (k < len)
			DEBUG_LOG("db",("%s;\n",&stmt[k]));
	} else {
		if (doall) {
			k = 0;
			while ((len - k) > 60) {
				for(j=k+59; j > (k + 20)
					&& stmt[j] != ','
					&& stmt[j] != ' '; j--);
				DEBUG_LOG("db",("%.*s\n",j+1-k,&stmt[k]));
				k = j + 1;
			}
			if (k < len)
				DEBUG_LOG("db",("%.*s;\n",len-k,&stmt[k]));
			k = len;
		} else {
			DEBUG_LOG("db",("%.60s %s\n",stmt,len>60?"...":""));
			k = 60;
		}
		while ((len - k) > 60) {
			for(j=k+59; j > (k + 20)
				&& stmt[j] != ','
				&& stmt[j] != ' '; j--);
			DEBUG_LOG("db",("%.*s\n",j+1-k,&stmt[k]));
			k = j + 1;
		}
		if (k < len)
			DEBUG_LOG("db",("%s;\n",&stmt[k]));
	}
#endif
}

void
cob_sql_dump_data (
	struct db_state	*db,
	struct file_xfd *fx)
{
#ifndef COB_DEBUG_LOG
	COB_UNUSED (db);
	COB_UNUSED (fx);
#else
	int		k,pos,len;
	char	hexwrk[80];
	for (k=pos=0; k < fx->nmap; k++) {
		if (fx->map[k].cmd == XC_DATA
		 && fx->map[k].colname) {
			pos++;
			len = fx->map[k].sqlinlen;
			if (len <= 0)
				len = strlen((char*)fx->map[k].sdata);
			hex_dump( fx->map[k].sdata, len, hexwrk);
			DEBUG_LOG("db",("Dump%3d: %-*s %s\n",pos,
								fx->maxcolnmln,fx->map[k].colname,
								fx->map[k].setnull?"NULL":hexwrk));
		}
	}
	return;
#endif
}

void
cob_sql_dump_index (
	struct db_state	*db,
	struct file_xfd *fx,
	int			idx)
{
#ifndef COB_DEBUG_LOG
	COB_UNUSED (db);
	COB_UNUSED (fx);
	COB_UNUSED (idx);
#else
	int	i,k,len;
	char	hexwrk[80];
	for (i=0; i < fx->key[idx]->ncols; i++) {
		k = fx->key[idx]->col[i];
		if (fx->map[k].cmd == XC_DATA
		 && fx->map[k].colname) {
			len = fx->map[k].sqlinlen;
			if (len <= 0)
				len = strlen((char*)fx->map[k].sdata);
			hex_dump( fx->map[k].sdata, len, hexwrk);
			DEBUG_LOG("db",("Index %d Field %d: %-20s %s\n",idx,k,fx->map[k].colname,
								fx->map[k].setnull?"NULL":hexwrk));
		}
	}
#endif
}

void
cob_dump_xfd (struct file_xfd *fx, FILE *fo)
{
	int	k,j;
	const char	*op;
	for (k=0; k < fx->nmap; k++) {
		for (j=0; j < fx->nlbl; j++) {
			if (k > 0
			 && k == fx->xlbl[j]) {
				fprintf(fo,"L,%d\n",j);
				break;
			}
		}
		switch (fx->map[k].cmd) {
		case XC_GOTO:
			fprintf(fo,"G,%d\n",fx->map[k].target);
			break;
		case XC_WHEN:
			op = " ?? ";
			switch (fx->map[k].opcode) {
			case XO_GE:		op = ">="; break;
			case XO_LE:		op = "<="; break;
			case XO_GT:		op = ">"; break;
			case XO_LT:		op = "<"; break;
			case XO_NE:		op = "!="; break;
			case XO_EQ:		op = "="; break;
			case XO_AND:	op = "&&"; break;
			case XO_OR:		op = "||"; break;
			case XO_NOT:	op = "!"; break;
			case XO_NULL:	op = " ? "; break;
			}
			if (fx->map[k].colname
			&& fx->map[k].colname[0] > ' ') {
				fprintf(fo,"C,%d,%s %s ",fx->map[k].target,fx->map[k].colname,op);
				if (fx->map[k].valnum)
					fprintf(fo,"%s",fx->map[k].value);
				else if (fx->map[k].value)
					fprintf(fo,"'%s'",fx->map[k].value);
				fprintf(fo,"\n");
			} else {
				fprintf(fo,"C,%d,%s\n",fx->map[k].target,op);
			}
			break;
		case XC_DATA:
			fprintf(fo,"D,%04d,%04d",fx->map[k].offset,fx->map[k].size);
			fprintf(fo,",%02d,%04d",fx->map[k].type,fx->map[k].sqlsize);
			fprintf(fo,",%d,%d,",fx->map[k].digits,fx->map[k].scale);
			if (fx->map[k].dtfrm)
				fprintf(fo,"%s",fx->map[k].dtfrm->format);
			fprintf(fo,",%02d,%s",fx->map[k].level,fx->map[k].colname);
			fprintf(fo,"\n");
			break;
		}
	}
}

/*
 * Free all memory used
 */
void
cob_drop_xfd (struct file_xfd *fx)
{
	int	k;
	for (k=0; k < fx->nmap; k++) {
		switch (fx->map[k].cmd) {
		case XC_GOTO:
			break;
		case XC_WHEN:
			if (fx->map[k].colname)
				cob_free (fx->map[k].colname);
			if (fx->map[k].value)
				cob_free (fx->map[k].value);
			break;
		case XC_DATA:
			if (fx->map[k].colname)
				cob_free (fx->map[k].colname);
			if (fx->map[k].value)
				cob_free (fx->map[k].value);
			break;
		}
	}
	for (k=0; k <= fx->ndate; k++) {
		if (fx->date[k] != NULL)
			cob_free (fx->date[k]);
	}
	cob_free (fx->date);
	if (fx->xlbl)
		cob_free (fx->xlbl);
	if (fx->map)
		cob_free (fx->map);
	if (fx->sqlbf)
		cob_free (fx->sqlbf);
	if (fx->insert.text)
		cob_free (fx->insert.text);
	if (fx->update.text)
		cob_free (fx->update.text);
	if (fx->delete.text)
		cob_free (fx->delete.text);
	if (fx->create_table)
		cob_free (fx->create_table);
	if (fx->create_sequence)
		cob_free (fx->create_sequence);
	for(k=0; k < fx->nkeys; k++) {
		if (fx->key[k]->create_index)
			cob_free (fx->key[k]->create_index);
		if (fx->key[k]->count_eq.text)
			cob_free (fx->key[k]->count_eq.text);
		if (fx->key[k]->where_eq.text)
			cob_free (fx->key[k]->where_eq.text);
		if (fx->key[k]->where_ge.text)
			cob_free (fx->key[k]->where_ge.text);
		if (fx->key[k]->where_gt.text)
			cob_free (fx->key[k]->where_gt.text);
		if (fx->key[k]->where_le.text)
			cob_free (fx->key[k]->where_le.text);
		if (fx->key[k]->where_lt.text)
			cob_free (fx->key[k]->where_lt.text);
		if (fx->key[k]->where_ndup.text)
			cob_free (fx->key[k]->where_ndup.text);
		if (fx->key[k]->where_pdup.text)
			cob_free (fx->key[k]->where_pdup.text);
		cob_free (fx->key[k]);
	}
	cob_free (fx);
}

static int
chrtohex ( char x )
{
	if (x >= '0' && x <= '9') return (int)(x - '0');
	if (x >= 'a' && x <= 'f') return (int)(x - 'a' + 10);
	if (x >= 'A' && x <= 'F') return (int)(x - 'A' + 10);
	return 0;
}

/*************************************************************************
	Try various combinations of schema name and suffix or ORACLE + SUFFIX
	as an environment variable. Return the correct one
**************************************************************************/
char *
getSchemaEnvName(
	struct db_state	*db,
	char		*envname,		/* Resulting Env Name last matched */
	const char	*suffix,
	char		*out)
{
	char	*env;
	int		k;
	char	sch[48];

	if (len_high_value == 0) {
		if( (env = cob_get_env ("SQL_HIGH_VALUE", NULL)) == NULL) 
			env = cob_get_env ("SQL_HIGH_VALUES", NULL);
		if( env != NULL) {
			for (k=0; *env != 0x00 && k < 6; env++) {
				if (isxdigit (*env)) {
					high_value[k] = (chrtohex ((char)env[0]) << 4)
									| chrtohex ((char)env[1]);
					env++;
					k++;
				}
			}
			len_high_value = k;
		} else {
			len_high_value = -2;
		}
	}

	if (db->dbSchema[0] <= ' ') {
		if( (env = getenv("COB_SCHEMA_NAME")) != NULL)
			snprintf(db->dbSchema,sizeof(db->dbSchema),"%s",env);
	}
	env = NULL;
	if (out)
		*out = 0;
	if (db->dbSchema[0] > ' ') {
		sprintf(envname,"%s%s",db->dbSchema,suffix);
		if( (env = getenv(envname)) != NULL) 
			goto gotit;

		for(k=0; db->dbSchema[k] != 0; k++) {
			sch[k] = db->dbSchema[k];
			if(sch[k] == '-')
				sch[k] = '_';
		}
		sch[k] = 0;

		sprintf(envname,"%s%s",sch,suffix);
		if( (env = getenv(envname)) != NULL)
			goto gotit;
	}

	sprintf(envname,"%s%s","COB_SCHEMA",suffix);
	if( (env = getenv(envname)) != NULL)
		goto gotit;

	if (db->mysql) {
		sprintf(envname,"%s%s","MYSQL",suffix);
		if( (env = getenv(envname)) != NULL) {
			if (out)
				strcpy(out,env);
			goto gotit;
		}
	}
	if (db->oracle || db->isoci) {
		sprintf(envname,"%s%s","ORACLE",suffix);
		if( (env = getenv(envname)) != NULL) 
			goto gotit;
	}
	sprintf(envname,"%s%s","SQL",suffix);
	if( (env = getenv(envname)) != NULL)
		goto gotit;

	if (db->db2) {
		sprintf(envname,"%s%s","DB2",suffix);
		if( (env = getenv(envname)) != NULL)
			goto gotit;
	}

	if (db->isodbc) {
		sprintf(envname,"%s%s","ODBC",suffix);
		env = getenv(envname);
	}

gotit:
	if( env != NULL) {
		if (out)
			strcpy(out,env);
	}
	return env;
}

/*************************************************************************
	Log the <schema>suffix env variable value
**************************************************************************/
void
logSchemaEnvName(
	struct db_state	*db,
	const char		*suffix)
{
#ifdef COB_DEBUG_LOG
	char	*env;
	char	envname[64];

	if ((env=getSchemaEnvName (db, envname, suffix, NULL)) != NULL) {
		DEBUG_LOG("db",("~%s is %s\n",envname,env));
	} else {
		DEBUG_LOG("db",("~Env: xxxx%s is %s\n",suffix,"undefined!"));
	}
#endif
	return;
}

#ifdef COB_DEBUG_LOG
static const char *condname[20] = {"0","EQ","LT","LE","GT","GE","NE","FI","LA",
				"9","10","11","12","13","14","15","COUNT","NDUP","PDUP","19"};
#endif
static const char *condstr[9] = {"?","=","<","<=",">",">=","<>",">","<"};

static int
bld_where (struct db_state *db, struct file_xfd *fx, int idx, int cond, int pos, char *sbuf)
{
	int		j,k,lparen;
	char	andstr[12], orstr[12];
	const char	*fmt, *rel;

	if (db->isodbc)
		fmt = "%s%s %s ?";
	else
		fmt = "%s%s %s :%d";
	if (cond == COB_GE
	 || cond == COB_GT)
		rel = ">";
	else
		rel = "<";
	lparen = 0;
	strcpy(andstr,"(");
	strcpy(orstr," OR (");
	for (j=0; j < fx->key[idx]->ncols-1; j++) {
		k = fx->key[idx]->col[j];
		lparen += 2;
		pos += sprintf(&sbuf[pos],fmt,andstr,fx->map[k].colname,rel,j+1);
		pos += sprintf(&sbuf[pos],fmt,orstr,fx->map[k].colname,"=",j+1);
		strcpy(andstr," AND (");
	}
	k = fx->key[idx]->col[j];
	pos += sprintf(&sbuf[pos],fmt,andstr,fx->map[k].colname,condstr[cond],j+1);
	while (lparen >= 0) {
		pos += sprintf(&sbuf[pos],")");
		lparen--;
	}
	return pos;
}

/*
 * Build SQL Statement and return as malloced string
 */
char *
cob_sql_stmt (
	struct db_state *db, 
	struct file_xfd *fx, 
	char	*stmt, 
	int		idx, 
	int 	cond,
	int		read_opts)
{
	char	*sbuf,comma[8],rowcol[48],wrk[48],*op;
	const char *fmt;
	cob_file	*f = fx->fl;
	int		bufsz,j,k,pos,ncols;
	int		lmode = FALSE;
	int		waitsecs = 0;

	if (idx >= fx->nkeys) {
		cob_runtime_error (_("SQL Index %d incorrect: %d max!"),idx,fx->nkeys);
		return NULL;
	}
	sbuf = NULL;
	if (db->isodbc)
		fmt = "%s%s %s ?";
	else
		fmt = "%s%s %s :%d";
	k = 0;
	sprintf(rowcol,"rid_%s",fx->tablename);
	if (strncasecmp(stmt,"SELECT",6) == 0) {
		if (f->retry_mode == COB_RETRY_SECONDS) {
			if (f->retry_times > 0)
				waitsecs = f->retry_seconds * f->retry_times;
			else
				waitsecs = f->retry_seconds;
		}
		if ((f->retry_mode & COB_RETRY_FOREVER)) {
			waitsecs = 0;
		} else
		if ((f->retry_mode & COB_RETRY_SECONDS)) {
			waitsecs = f->retry_seconds;
		} else
		if ((f->retry_mode & COB_RETRY_TIMES)) {
			waitsecs = f->retry_times / COB_RETRY_PER_SECOND;
			if (waitsecs <= 0)
				waitsecs = 1;
		} else
		if ((f->retry_mode & COB_RETRY_NEVER)) {
			waitsecs = -1;
		}
		if ((read_opts & COB_READ_LOCK)
		 || (read_opts & COB_READ_WAIT_LOCK)) {
			lmode = TRUE;		
		}
		if ((read_opts & COB_READ_IGNORE_LOCK)
		 || (read_opts & COB_READ_NO_LOCK) ) {
			lmode = FALSE;
		}
		db->dbStatus = 0;
		if (fx->select == NULL) {			/* Build list of Column Names */
			bufsz = 8 + fx->lncols + fx->ncols;
			sbuf = cob_malloc (bufsz + 1);
			strcpy(comma,"");
			pos = 0;
			for (k=0; k < fx->nmap; k++) {
				if (fx->map[k].cmd == XC_DATA
				 && fx->map[k].colname) {
					pos += sprintf(&sbuf[pos],"%s%s",comma,fx->map[k].colname);
					strcpy(comma,",");
				}
			}
			fx->lnselect = pos;
			fx->select = sbuf;
		}

		if (fx->fileorg == COB_ORG_RELATIVE) {
			bufsz = 128 + strlen(stmt) + fx->lnselect;
			bufsz += strlen(rowcol) + 64;
		} else {
			bufsz = 128 + strlen(stmt) + fx->lnselect + (fx->key[idx]->lncols * 3);
			bufsz += (fx->key[idx]->ncols * 20);
		}
		if (lmode)
			bufsz += 32;
		if (cond == COB_GT
		 || cond == COB_GE
		 || cond == COB_LT
		 || cond == COB_LE)
			bufsz += (fx->key[idx]->ncols * 12);
		sbuf = cob_malloc (bufsz + 1);
		if (fx->fl
		 && fx->fl->limitreads > 0
		 && db->mssql) 
			sprintf(wrk," TOP %d ",fx->fl->limitreads);
		else
			strcpy(wrk," ");
		strcpy(comma,"");
		pos = sprintf(sbuf,"%s%s%s FROM %s",stmt,wrk,fx->select,fx->tablename);
		if (db->mssql) {
			if (lmode
			 && db->mssqlnfu) 
				pos += sprintf(&sbuf[pos]," WITH (XLOCK, ROWLOCK)");
			if ((f->retry_mode & COB_ADVANCING_LOCK)) {
				pos += sprintf(&sbuf[pos]," WITH (READPAST)");
			} else if (waitsecs > 0) {
			} else if (waitsecs < 0) {
				pos += sprintf(&sbuf[pos]," WITH (NOWAIT)");
			} 
		}
		if (cond != COB_FI
		 && cond != COB_LA)
			pos += sprintf(&sbuf[pos]," WHERE ");
		strcpy(comma,"");
		if (fx->fileorg == COB_ORG_RELATIVE) {
			op = (char*)"=";
			if (cond == COB_NE)
				op = (char*)"<>";
			else if (cond == COB_GT)
				op = (char*)">";
			else if (cond == COB_GE)
				op = (char*)">=";
			else if (cond == COB_LT)
				op = (char*)"<";
			else if (cond == COB_LE)
				op = (char*)"<=";
			if (cond != COB_FI
			 && cond != COB_LA)
				pos += sprintf(&sbuf[pos],fmt,comma,rowcol,op,1);
		} else
		if (cond == 0
		 || cond == COB_EQ) {
			for (j=0; j < fx->key[idx]->ncols; j++) {
				k = fx->key[idx]->col[j];
				pos += sprintf(&sbuf[pos],fmt,comma,fx->map[k].colname,"=",j+1);
				strcpy(comma," AND ");
			}
		} else if (cond == COB_NE) {
			for (j=0; j < fx->key[idx]->ncols; j++) {
				k = fx->key[idx]->col[j];
				pos += sprintf(&sbuf[pos],fmt,comma,fx->map[k].colname,"<>", j+1);
				strcpy(comma," OR ");
			}
		} else if (cond != COB_FI
				&& cond != COB_LA) {
			k = pos;
			pos = bld_where (db, fx, idx, cond, pos, sbuf);
		}
		if (cond == COB_LT
		 || cond == COB_LE
		 || cond == COB_LA)
			fmt = " DESC";
		else 
			fmt = "";
		pos += sprintf(&sbuf[pos]," ORDER BY ");
		strcpy(comma,"");
		if (fx->fileorg == COB_ORG_RELATIVE) {
			pos += sprintf(&sbuf[pos],"%s%s",rowcol,fmt);
		} else {
			for (j=0; j < fx->key[idx]->ncols; j++) {
				k = fx->key[idx]->col[j];
				pos += sprintf(&sbuf[pos],"%s%s%s",comma,fx->map[k].colname,fmt);
				strcpy(comma,",");
			}
		}
		if (fx->fl
		 && fx->fl->limitreads > 0) {
			if (db->mysql || db->postgres || db->sqlite)
				pos += sprintf(&sbuf[pos]," LIMIT %d",fx->fl->limitreads);
		}
		if (lmode) {
			if (!db->no_for_update) 
				pos += sprintf(&sbuf[pos]," FOR UPDATE");
		}
		if (db->mysql) {
			if ((f->retry_mode & COB_ADVANCING_LOCK)) {
				if (!db->mariadb) 
					pos += sprintf(&sbuf[pos]," SKIP LOCKED");
			} else if (waitsecs > 0) {
				if (db->mariadb) 
					pos += sprintf(&sbuf[pos]," WAIT %d",waitsecs);
			} else if (waitsecs < 0) {
				if (db->mariadb) 
					pos += sprintf(&sbuf[pos]," NOWAIT");
			} 
		} else if (db->isoci) {
			if ((f->retry_mode & COB_ADVANCING_LOCK)) {
				pos += sprintf(&sbuf[pos]," SKIP LOCKED");
			} else if (waitsecs > 0) {
				pos += sprintf(&sbuf[pos]," WAIT %d",waitsecs);
			} else if (waitsecs < 0) {
				pos += sprintf(&sbuf[pos]," NOWAIT");
			} 
		}
		DEBUG_TRACE("db",("Build %s %s Index %d\n",stmt,condname[cond],idx));

	} else if (strcasecmp(stmt,"INSERT") == 0) {
		idx = 0;
		bufsz = 64 + fx->lncols + fx->ncols * 4 + strlen(fx->tablename);
		bufsz += fx->key[0]->lncols + fx->key[0]->ncols * 12;
		sbuf = cob_malloc (bufsz + 1);
		strcpy(comma,"");
		if (db->isodbc)
			fmt = "%s?";
		else
			fmt = "%s:%d";
		pos = sprintf(sbuf,"INSERT INTO %s (",fx->tablename);
		ncols = 0;
		for (k=0; k < fx->nmap; k++) {
			if (fx->map[k].cmd == XC_DATA
			 && fx->map[k].type != COB_XFDT_COMP5IDX
			 && fx->map[k].colname) {
				pos += sprintf(&sbuf[pos],"%s%s",comma,fx->map[k].colname);
				strcpy(comma,",");
				ncols++;
			}
		}
		pos += sprintf(&sbuf[pos],") VALUES (");
		strcpy(comma,"");
		for (j=0; j < ncols; j++) {
			pos += sprintf(&sbuf[pos],fmt,comma,j+1);
			strcpy(comma,",");
		}
		pos += sprintf(&sbuf[pos],")");

	} else if (strcasecmp(stmt,"UPDATE") == 0) {
		bufsz = 64 + fx->lncols + fx->ncols * 8 + strlen(fx->tablename);
		sbuf = cob_malloc (bufsz + 1);
		strcpy(comma,"");
		pos = sprintf(sbuf,"UPDATE %s SET ",fx->tablename);
		if (db->isodbc)
			fmt = "%s%s %s ?";
		else
			fmt = "%s%s %s :%d";
		j = 0;
		for (k=0; k < fx->nmap; k++) {
			if (fx->map[k].cmd == XC_DATA
			 && fx->map[k].type != COB_XFDT_COMP5IDX
			 && fx->map[k].colname) {
				pos += sprintf(&sbuf[pos],fmt,comma,fx->map[k].colname,"=",++j);
				strcpy(comma,",");
			}
		}
		pos += sprintf(&sbuf[pos]," WHERE ");
		strcpy(comma,"");
		if (fx->fileorg == COB_ORG_RELATIVE) {
			pos += sprintf(&sbuf[pos],fmt,comma,rowcol,"=",1);
		} else {
			for (j=0; j < fx->key[0]->ncols; j++) {
				k = fx->key[0]->col[j];
				pos += sprintf(&sbuf[pos],fmt,comma,fx->map[k].colname,"=",j+1);
				strcpy(comma," AND ");
			}
		}

	} else if (strcasecmp(stmt,"DELETE") == 0) {
		bufsz = 64 + fx->lncols + fx->ncols * 4 + strlen(fx->tablename);
		sbuf = cob_malloc (bufsz + 1);
		strcpy(comma,"");
		pos = sprintf(sbuf,"DELETE FROM %s ",fx->tablename);
		pos += sprintf(&sbuf[pos],"WHERE ");
		if (db->isodbc)
			fmt = "%s%s %s ?";
		else
			fmt = "%s%s %s :%d";
		strcpy(comma,"");
		if (fx->fileorg == COB_ORG_RELATIVE) {
			pos += sprintf(&sbuf[pos],fmt,comma,rowcol,"=",1);
		} else {
			for (j=0; j < fx->key[0]->ncols; j++) {
				k = fx->key[0]->col[j];
				pos += sprintf(&sbuf[pos],fmt,comma,fx->map[k].colname,"=",j+1);
				strcpy(comma," AND ");
			}
		}

	} else if (strncasecmp(stmt,"DUPAHEAD",8) == 0) {
		db->dbStatus = 0;
		bufsz = 512 + (fx->key[idx]->ncols * 12);
		sbuf = cob_malloc (bufsz + 1);
		pos = sprintf(sbuf,"SELECT");
		if (db->mssql) 
			pos += sprintf(&sbuf[pos]," TOP 1");
		pos += sprintf(&sbuf[pos]," %s FROM %s WHERE ",rowcol,fx->tablename);
		op = (char*)"=";
		strcpy(comma,"");
		for (j=0; j < fx->key[idx]->ncols-1; j++) {
			k = fx->key[idx]->col[j];
			pos += sprintf(&sbuf[pos],fmt,comma,fx->map[k].colname,"=",j+1);
			strcpy(comma," AND ");
		}
		if (cond == COB_PDUP)
			pos += sprintf(&sbuf[pos],fmt,comma,rowcol,"<",j+1);
		else
			pos += sprintf(&sbuf[pos],fmt,comma,rowcol,">",j+1);
		if (db->isoci) 
			pos += sprintf(&sbuf[pos]," AND ROWNUM <= 1");
		pos += sprintf(&sbuf[pos]," ORDER BY ");
		if (cond == COB_PDUP)
			fmt = " DESC";
		else 
			fmt = "";
		strcpy(comma,"");
		for (j=0; j < fx->key[idx]->ncols; j++) {
			k = fx->key[idx]->col[j];
			pos += sprintf(&sbuf[pos],"%s%s%s",comma,fx->map[k].colname,fmt);
			strcpy(comma,", ");
		}
		if (db->mysql || db->postgres || db->sqlite)
			pos += sprintf(&sbuf[pos]," LIMIT 1");
		DEBUG_TRACE("db",("Build %s %s Index %d\n",stmt,condname[cond],idx));

	} else if (strcasecmp(stmt,"COUNT") == 0
			&& fx->fileorg == COB_ORG_INDEXED) {
		bufsz = 64 + fx->lncols + fx->ncols * 4 + strlen(fx->tablename);
		sbuf = cob_malloc (bufsz + 1);
		strcpy(comma,"");
		pos = sprintf(sbuf,"SELECT COUNT(*) FROM %s WHERE ",fx->tablename);
		if (db->isodbc)
			fmt = "%s%s %s ?";
		else
			fmt = "%s%s %s :%d";
		strcpy(comma,"");
		for (j=0; j < fx->key[idx]->ncols; j++) {
			k = fx->key[idx]->col[j];
			if (fx->map[k].type == COB_XFDT_COMP5IDX)
				continue;
			pos += sprintf(&sbuf[pos],fmt,comma,fx->map[k].colname,"=",j+1);
			strcpy(comma," AND ");
		}

	} else {
		cob_runtime_error (_("Unknown SQL statement: %.20s!"),stmt);
		return NULL;
	}
	if (strncasecmp(stmt,"SELECT",6) == 0) {
		/* Leave space for adding ' FOR UPDATE'; 11 bytes */
		sbuf = cob_realloc (sbuf, (size_t)bufsz, (size_t)pos+16);
	} else if (bufsz > (pos+2)) {
		sbuf = cob_realloc (sbuf, (size_t)bufsz, (size_t)pos+2);
	}
	sbuf[pos] = 0;
	cob_sql_dump_stmt (db, sbuf, TRUE);
	return sbuf;
}

SQL_STMT *
cob_sql_select ( 
	struct db_state *db, 
	struct file_xfd *fx, 
	int		ky, 
	int		cond,
	int		read_opts,
	void  (*freeit)( SQL_STMT *))
{
	SQL_STMT *s;

	if (cond == COB_EQ)
		s = &fx->key[ky]->where_eq;
	else if (cond == COB_GE)
		s = &fx->key[ky]->where_ge;
	else if (cond == COB_LE)
		s = &fx->key[ky]->where_le;
	else if (cond == COB_LT)
		s = &fx->key[ky]->where_lt;
	else if (cond == COB_GT)
		s = &fx->key[ky]->where_gt;
	else if (cond == COB_FI)
		s = &fx->key[ky]->where_fi;
	else if (cond == COB_LA)
		s = &fx->key[ky]->where_la;
	else if (cond == COB_NE)
		s = &fx->key[ky]->where_ne;
	else if (cond == COB_COUNT)
		s = &fx->key[ky]->count_eq;
	else if (cond == COB_NDUP)
		s = &fx->key[ky]->where_ndup;
	else if (cond == COB_PDUP)
		s = &fx->key[ky]->where_pdup;
	else
		return NULL;

	if (s->text == NULL) {
		if (cond == COB_COUNT)
			s->text = cob_sql_stmt (db, fx, (char*)"COUNT", ky, cond, read_opts);
		else
			s->text = cob_sql_stmt (db, fx, (char*)"SELECT", ky, cond, read_opts);
		if (fx->fl
		 && (fx->fl->flag_read_chk_dups || fx->fl->limitreads > 0)
		 && fx->key[ky]->dups) {
			if (fx->key[ky]->where_ndup.text == NULL)
				fx->key[ky]->where_ndup.text = cob_sql_stmt (db, fx, 
											(char*)"DUPAHEAD", ky, COB_NDUP, 0);
			if (fx->key[ky]->where_pdup.text == NULL)
				fx->key[ky]->where_pdup.text = cob_sql_stmt (db, fx, 
											(char*)"DUPAHEAD", ky, COB_PDUP, 0);
		}
	} else if (s->readopts != read_opts) {
		DEBUG_LOG ("db",("Free %s Statement\n",condname[cond]));
		freeit (s);
		if (cond == COB_COUNT)
			s->text = cob_sql_stmt (db, fx, (char*)"COUNT", ky, cond, read_opts);
		else
			s->text = cob_sql_stmt (db, fx, (char*)"SELECT", ky, cond, read_opts);
	}
	s->readopts = read_opts;
	if (cond == COB_LT
	 || cond == COB_LE
	 || cond == COB_LA) 
		s->isdesc = TRUE;
	else
		s->isdesc = FALSE;
	return s;
}

void
cob_xfd_swap_data (char *p1, char *p2, int len)
{
	char	wk;
	while(len-- > 0) {
		wk = *p1;
		*p1 = *p2;
		*p2 = wk;
		p1++;
		p2++;
	}
}

/*
 * Copy data from File index fields to SQL data field(s)
 */
void 
cob_index_to_xfd (struct db_state *db, struct file_xfd *fx, cob_file *fl, int idx, int cond)
{
	int	i,k,nx,dl;
	char		sqlbuf[48];
	cob_field	sqlwrk;
	COB_UNUSED(db);
	if (len_high_value == -2) {
		/* Note that MySQL and PostgreSQL object to passing HIGH-VALUES for a column
		 * in a WHERE clause
		 */
		if (db->mysql) {
			high_value[0] = '~';
			len_high_value = 1;
		} else 
		if (db->postgres) {
			memcpy(high_value,"\xC3\xBF",2);
			len_high_value = 2;
		} else {
			len_high_value = -1;
		}
	}
	for (i=0; i < fx->key[idx]->ncols; i++) {
		k = fx->key[idx]->col[i];
		if (fx->map[k].cmd == XC_DATA
		 && fx->map[k].colname) {
			fx->map[k].setnull = 0;
			fx->map[k].sqlfld.size = fx->map[k].sqloutlen;
			if (fx->map[k].type == COB_XFDT_COMP5IDX) {
				if (fx->map[k].sqlfld.data[0] < ' ')
					strcpy((char*)fx->map[k].sqlfld.data,"0");
				DEBUG_LOG("db",("%3d: Index %d %s ROWID: '%s'\n",k,idx,
							fx->map[k].colname,fx->map[k].sqlfld.data));
				continue;
			}
			memset (fx->map[k].sqlfld.data, 0, fx->map[k].sqlfld.size);
			if (fx->map[k].dtfrm) {
				memcpy(&sqlwrk,&fx->map[k].sqlfld,sizeof(cob_field));
				sqlwrk.data = (unsigned char *)sqlbuf;
				sqlwrk.size = fx->map[k].dtfrm->digits;
				cob_move (&fx->map[k].recfld, &sqlwrk);
				sqlbuf[sqlwrk.size] = 0;
				dl = convert_to_date (db, fx->map[k].dtfrm, (char*)sqlbuf, (int)sqlwrk.size, 
								(char*)fx->map[k].sqlfld.data, (int)fx->map[k].sqlfld.size, &nx);
				fx->map[k].sqlfld.data[dl] = 0;
				DEBUG_LOG("db",("%3d: Index %d date '%s' %s d:%d z:%d :%s\n",k,idx,
							fx->map[k].dtfrm->format,fx->map[k].colname,
							fx->map[k].sqlDecimals, fx->map[k].sqlColSize, nx?"Ok":"Bad Date"));
				DEBUG_DUMP("db",fx->map[k].sqlfld.data,dl);
			} else if (fx->map[k].type == COB_XFDT_FLOAT) {
				memcpy (fx->map[k].sdata,fx->map[k].recfld.data,fx->map[k].size);
			} else if (fx->map[k].type == COB_XFDT_BIN) {
				memcpy (fx->map[k].sdata,fx->map[k].recfld.data,fx->map[k].size);
				fx->map[k].sdata[fx->map[k].size] = 0;

			} else if (cond == COB_LA) {	/* LAST of index */
				if (fx->map[k].type == COB_XFDT_PICX
			 	 || fx->map[k].type == COB_XFDT_PICA
			 	 || fx->map[k].type == COB_XFDT_VARX) {
					DEBUG_LOG("db",("%3d: Index %d %s PIC X(%d) LAST set HIGH\n",
									k,idx,fx->map[k].colname,(int)fx->map[k].size));
					if (len_high_value <= 0
					 || fx->map[k].size < len_high_value) {
						memset (fx->map[k].sdata, '~', fx->map[k].size);
					} else {
						memcpy (fx->map[k].sdata, high_value, len_high_value);
						memset (&fx->map[k].sdata[len_high_value], '~', 
								fx->map[k].size - len_high_value);
					}
					fx->map[k].sdata[fx->map[k].size] = 0;
					DEBUG_DUMP("db",fx->map[k].sqlfld.data,fx->map[k].sqlfld.size);
				} else {
					DEBUG_LOG("db",("%3d: Index %d %s PIC 9(%d) LAST set 9s\n",
									k,idx,fx->map[k].colname,(int)fx->map[k].size));
					memset (fx->map[k].sdata, '9', fx->map[k].digits);
					fx->map[k].sdata[fx->map[k].digits] = 0;
					DEBUG_DUMP("db",fx->map[k].sqlfld.data,fx->map[k].digits);
				}

			} else if (cond == COB_FI) {		/* FIRST of index */
				if (fx->map[k].type == COB_XFDT_PICX
			 	 || fx->map[k].type == COB_XFDT_PICA
			 	 || fx->map[k].type == COB_XFDT_VARX) {
					DEBUG_LOG("db",("%3d: Index %d %s PIC X(%d) FIRST set SPACES\n",
									k,idx,fx->map[k].colname,(int)fx->map[k].size));
					memset (fx->map[k].sdata, ' ', fx->map[k].size);
					fx->map[k].sdata[fx->map[k].size] = 0;
				} else {
					DEBUG_LOG("db",("%3d: Index %d %s PIC 9(%d) FIRST set ZERO\n",
									k,idx,fx->map[k].colname,(int)fx->map[k].size));
					memset (fx->map[k].sdata, '0', fx->map[k].digits);
					fx->map[k].sdata[fx->map[k].digits] = 0;
				}

			} else if (len_high_value > 0
				&& isAllChar (fx->map[k].recfld.data, (int)fx->map[k].recfld.size, 0xFF)) {
				/* COBOL field is all HIGH-VALUES so set to highest usable value */
				if (fx->map[k].type == COB_XFDT_PICX
			 	 || fx->map[k].type == COB_XFDT_PICA
			 	 || fx->map[k].type == COB_XFDT_VARX) {
					DEBUG_LOG("db",("%3d: Index %d %s PIC X(%d) HIGH-VALUES\n",
									k,idx,fx->map[k].colname,(int)fx->map[k].size));
					if (fx->map[k].size < len_high_value) {
						memset (fx->map[k].sdata, '~', fx->map[k].size);
					} else {
						memcpy (fx->map[k].sdata, high_value, len_high_value);
						memset (&fx->map[k].sdata[len_high_value], '~', 
								fx->map[k].size - len_high_value);
					}
					fx->map[k].sdata[fx->map[k].size] = 0;
					DEBUG_DUMP("db",fx->map[k].sqlfld.data,fx->map[k].sqlfld.size);
				} else {
					DEBUG_LOG("db",("%3d: Index %d %s PIC 9(%d) HIGH-VALUE\n",
									k,idx,fx->map[k].colname,(int)fx->map[k].size));
					memset (fx->map[k].sdata, '9', fx->map[k].digits);
					fx->map[k].sdata[fx->map[k].digits] = 0;
					DEBUG_DUMP("db",fx->map[k].sqlfld.data,fx->map[k].digits);
				}

			} else if (fx->map[k].type == COB_XFDT_PICX
			 		|| fx->map[k].type == COB_XFDT_PICA
			 		|| fx->map[k].type == COB_XFDT_VARX) {
				memcpy (fx->map[k].sdata,fx->map[k].recfld.data,fx->map[k].size);
				fx->map[k].sdata[fx->map[k].size] = 0;
				DEBUG_LOG("db",("%3d: Index %d %s type:PIC X(%d) \n",
								k,idx,fx->map[k].colname,(int)fx->map[k].size));
				DEBUG_DUMP("db",fx->map[k].sqlfld.data,fx->map[k].sqlfld.size);
			} else {
				cob_move (&fx->map[k].recfld, &fx->map[k].sqlfld);
				fx->map[k].sqlfld.data[fx->map[k].sqlfld.size] = 0;
				DEBUG_LOG("db",("%3d: Index %d %s type:%02d \n",k,idx,
							fx->map[k].colname,fx->map[k].type));
				DEBUG_DUMP("db",fx->map[k].sqlfld.data,fx->map[k].sqlfld.size);
			}
		}
	}
}

/*
 * Clear data from File index fields to SQL data field(s)
 */
void 
cob_index_clear (struct db_state *db, struct file_xfd *fx, cob_file *fl, int idx)
{
	int	i,k;
	COB_UNUSED(db);
	COB_UNUSED(fl);
	for (i=0; i < fx->key[idx]->ncols; i++) {
		k = fx->key[idx]->col[i];
		if (fx->map[k].cmd == XC_DATA
		 && fx->map[k].colname) {
			fx->map[k].setnull = 0;
			fx->map[k].sqlfld.size = fx->map[k].sqloutlen;
			if (fx->map[k].type != COB_XFDT_COMP5IDX) {
				memset (fx->map[k].sqlfld.data, 0, fx->map[k].sqlfld.size);
			}
		}
	}
}

/*
 * Copy data from File record area to SQL data field(s)
 */
void 
cob_file_to_xfd (struct db_state *db, struct file_xfd *fx, cob_file *fl)
{
	int	k,nx,dl;
	char		sqlbuf[48];
	cob_field	sqlwrk;
#ifdef COB_DEBUG_LOG
	char		hexwrk[80];
#endif
	for (k=0; k < fx->nmap; ) {
		if (fx->map[k].cmd == XC_DATA
		 && fx->map[k].colname) {
			fx->map[k].setnull = 0;
			fx->map[k].sqlfld.size = fx->map[k].sqloutlen;
			memset (fx->map[k].sqlfld.data, 0, fx->map[k].sqlfld.size);
			if (fx->map[k].dtfrm) {
				memcpy(&sqlwrk,&fx->map[k].sqlfld,sizeof(cob_field));
				sqlwrk.data = (unsigned char *)sqlbuf;
				sqlwrk.size = fx->map[k].dtfrm->digits;
				cob_move (&fx->map[k].recfld, &sqlwrk);
				sqlbuf[sqlwrk.size] = 0;
				nx = 0;
				if (!fx->map[k].notnull
				 &&	isAllChar (fx->map[k].recfld.data, (int)fx->map[k].recfld.size, 0x00)) {
					fx->map[k].setnull = 1;
					fx->map[k].sqlfld.data[0] = 0;
				} else {
					dl = convert_to_date (db, fx->map[k].dtfrm, (char*)sqlbuf, (int)sqlwrk.size, 
									(char*)fx->map[k].sqlfld.data, (int)fx->map[k].sqlfld.size, &nx);
					fx->map[k].sqlfld.data[dl] = 0;
				}
#ifdef COB_DEBUG_LOG
				if (fx->map[k].setnull) {
					hex_dump( fx->map[k].recfld.data,(int)fx->map[k].recfld.size, hexwrk);
					DEBUG_LOG("db",("%3d: Copy date '%s' had %s; set NULL\n",k,
								fx->map[k].colname,hexwrk));
				} else {
					if (fx->map[k].type == COB_XFDT_PIC9L
					 || fx->map[k].type == COB_XFDT_PIC9LS
					 || fx->map[k].type == COB_XFDT_PIC9T
					 || fx->map[k].type == COB_XFDT_PIC9TS
					 || fx->map[k].type == COB_XFDT_PIC9S
					 || fx->map[k].type == COB_XFDT_PIC9U) {
						sprintf(hexwrk,"'%.*s'",(int)fx->map[k].recfld.size,fx->map[k].recfld.data);
					} else {
						hex_dump( fx->map[k].recfld.data,(int)fx->map[k].recfld.size, hexwrk);
					}
					DEBUG_LOG("db",("%3d: Copy date '%s' %s ct:%02d ht:%d st:%d d:%d z:%d :%s\n",k,
								fx->map[k].dtfrm->format,fx->map[k].colname,
								fx->map[k].type, fx->map[k].hostType, fx->map[k].sqlType,
								fx->map[k].sqlDecimals, fx->map[k].sqlColSize, 
								nx?"Ok":"Bad Date"));
					DEBUG_LOG ("db",(" %s ->  Temp:%.*s -> SQL:%.*s\n",hexwrk,
										sqlwrk.size,sqlbuf,
										fx->map[k].sqlsize,fx->map[k].sqlfld.data));
				}
#endif
			} else if (fx->map[k].type == COB_XFDT_PICX
			 		|| fx->map[k].type == COB_XFDT_PICA
			 		|| fx->map[k].type == COB_XFDT_VARX) {
				if (!fx->map[k].notnull
				 &&	isAllChar (fx->map[k].recfld.data, (int)fx->map[k].recfld.size, 0x00)) {
					fx->map[k].setnull = 1;
					fx->map[k].sqlfld.data[0] = 0;
					DEBUG_LOG("db",("%3d: Copy '%s' LOW-VALUES; set NULL\n",k,fx->map[k].colname));
				} else {
					memcpy (fx->map[k].sdata,fx->map[k].recfld.data,fx->map[k].size);
					fx->map[k].sdata[fx->map[k].size] = 0;
					DEBUG_LOG("db",("%3d: Copy %s PIC X(%d)  ht:%d st:%d z:%d\n",k,
								fx->map[k].colname, fx->map[k].size,
								fx->map[k].hostType, fx->map[k].sqlType,
								fx->map[k].sqlColSize));
					DEBUG_LOG ("db",("   '%.*s'\n",(int)fx->map[k].recfld.size,
											fx->map[k].recfld.data));
				}
			} else if (fx->map[k].type == COB_XFDT_FLOAT) {
				memcpy (fx->map[k].sdata,fx->map[k].recfld.data,fx->map[k].size);
			} else if (fx->map[k].type == COB_XFDT_BIN) {
				memcpy (fx->map[k].sdata,fx->map[k].recfld.data,fx->map[k].size);
				fx->map[k].sdata[fx->map[k].size] = 0;
			} else 
			if (fx->map[k].type == COB_XFDT_COMP5IDX) {
				k++;
				continue;
			} else {
				if (fx->map[k].type == COB_XFDT_PIC9S
				 || fx->map[k].type == COB_XFDT_PIC9L
				 || fx->map[k].type == COB_XFDT_PIC9LS
				 || fx->map[k].type == COB_XFDT_PIC9T
				 || fx->map[k].type == COB_XFDT_PIC9TS
				 || fx->map[k].type == COB_XFDT_PIC9U) {
					if (!fx->map[k].notnull
					 &&	isAllChar (fx->map[k].recfld.data, (int)fx->map[k].recfld.size, 0x00)) {
						fx->map[k].setnull = 1;
						fx->map[k].sqlfld.data[0] = 0;
						DEBUG_LOG("db",("%3d: Copy '%s' LOW-VALUES; set NULL\n",k,fx->map[k].colname));
					} else
					if (isAllChar (fx->map[k].recfld.data, (int)fx->map[k].recfld.size, ' ')) {
						strcpy((char*)fx->map[k].sqlfld.data, "0");
						DEBUG_LOG("db",("%3d: Copy '%s' SPACES; set ZERO\n",k,fx->map[k].colname));
					} else {
						cob_move (&fx->map[k].recfld, &fx->map[k].sqlfld);
						fx->map[k].sqlfld.data[fx->map[k].sqlfld.size] = 0;
						DEBUG_LOG("db",("%3d: Copy %s type:%02d %dv%d ht:%d st:%d d:%d z:%d\n",k,
									fx->map[k].colname,fx->map[k].type,
									fx->map[k].digits,fx->map[k].scale,
									fx->map[k].hostType, fx->map[k].sqlType,
									fx->map[k].sqlDecimals, fx->map[k].sqlColSize));
						DEBUG_LOG ("db",("   '%.*s'\n",(int)fx->map[k].recfld.size,
											fx->map[k].recfld.data));
					}
				} else {
					cob_move (&fx->map[k].recfld, &fx->map[k].sqlfld);
					fx->map[k].sqlfld.data[fx->map[k].sqlfld.size] = 0;
					DEBUG_LOG("db",("%3d: Copy %s type:%02d %dv%d ht:%d st:%d d:%d z:%d\n",k,
								fx->map[k].colname,fx->map[k].type,
								fx->map[k].digits,fx->map[k].scale,
								fx->map[k].hostType, fx->map[k].sqlType,
								fx->map[k].sqlDecimals, fx->map[k].sqlColSize));
					DEBUG_DUMP("db",fx->map[k].recfld.data,fx->map[k].recfld.size);
					DEBUG_DUMP("db",fx->map[k].sqlfld.data,fx->map[k].sqlfld.size);
				}
			}
			k++;

		} else if (fx->map[k].cmd == XC_GOTO) {
			nx = fx->xlbl[fx->map[k].target];
			do {
				k++;
				if (fx->map[k].cmd == XC_DATA)	/* Field skipped are set NULL */
					fx->map[k].setnull = 1;
			} while (k < nx);
			fx->map[k].jumpto = k;
			continue;
		} else if (fx->map[k].cmd == XC_WHEN) {
			nx = eval_expr (fx, k);
			if (nx < k)
				break;
			k = nx;
		} else {
			k++;
		}
	}
}

/*
 * Copy data from SQL data field(s) to File Record area
 */
void 
cob_xfd_to_file (struct db_state *db, struct file_xfd *fx, cob_file *fl)
{
	int	k,nx,i,j,slen,flen;
	unsigned char	*sptr,*fptr;
	char		sqlbuf[48];
	cob_field	sqlwrk;
#ifdef COB_DEBUG_LOG
	char		hexwrk[80];
#endif
	for (k=0; k < fx->nmap; ) {
		if (fx->map[k].cmd == XC_DATA
		 && fx->map[k].colname) {
			if (fx->map[k].setnull) {
				memset (fx->map[k].recfld.data, 0, fx->map[k].recfld.size);
				memset (fx->map[k].sqlfld.data, 0, fx->map[k].sqlfld.size);
				DEBUG_LOG("db",("%3d: Read %s  was NULL -> LOW-VALUES\n",k,fx->map[k].colname));
				k++;
				continue;
			}
			if (fx->map[k].dtfrm) {
				memcpy(&sqlwrk,&fx->map[k].sqlfld,sizeof(cob_field));
				sqlwrk.data = (unsigned char *)sqlbuf;
				sqlwrk.size = fx->map[k].dtfrm->digits;
				cob_move (&fx->map[k].sqlfld, &sqlwrk);
				sqlbuf[sqlwrk.size] = 0;
				convert_from_date (db, fx->map[k].dtfrm, 
								(char*)fx->map[k].sqlfld.data, (int)fx->map[k].sqlfld.size, 
								(char*)sqlbuf, (int)sqlwrk.size);
				cob_move (&sqlwrk, &fx->map[k].recfld);
#ifdef COB_DEBUG_LOG
				DEBUG_LOG("db",("%3d: Read date '%s' %s type: %02d \n",k,
							fx->map[k].dtfrm->format,fx->map[k].colname,fx->map[k].type));
				hex_dump( fx->map[k].recfld.data,fx->map[k].recfld.size, hexwrk);
				DEBUG_LOG ("db",("   SQL:%.*s -> Temp:%.*s -> %s\n",
									fx->map[k].sqlsize,fx->map[k].sqlfld.data,
									sqlwrk.size,sqlbuf,hexwrk));
#endif
			} else if (fx->map[k].type == COB_XFDT_PICX
			 		|| fx->map[k].type == COB_XFDT_PICA
			 		|| fx->map[k].type == COB_XFDT_VARX) {
				slen = fx->map[k].sqlfld.size = fx->map[k].sqlinlen;
				sptr = fx->map[k].sqlfld.data;
				flen = (int)fx->map[k].recfld.size;
				fptr = fx->map[k].recfld.data;
				/* Copy to first NUL and then space fill COBOL field */
				for(i=j=0; i < flen && j < slen && *sptr != 0; i++,j++) 
					*fptr++ = *sptr++;
				while(i < flen) {
					*fptr++ = ' ';
					i++;
				}
				DEBUG_LOG("db",("%3d: Read %s PIC X(%d)\n",k,
							fx->map[k].colname,fx->map[k].sqlinlen));
				DEBUG_LOG ("db",("   '%.*s'\n",(int)fx->map[k].recfld.size,
										fx->map[k].recfld.data));
			} else if (fx->map[k].type == COB_XFDT_FLOAT) {
				memcpy (fx->map[k].recfld.data,fx->map[k].sdata,fx->map[k].size);
			} else if (fx->map[k].type == COB_XFDT_BIN) {
				memcpy (fx->map[k].recfld.data,fx->map[k].sdata,fx->map[k].size);
			} else 
			if (fx->map[k].type == COB_XFDT_COMP5IDX) {
				fx->recnum = atol ((void*)fx->map[k].sqlfld.data);
				DEBUG_LOG("db",("%3d: Read %s ROWID %ld\n",k,
									fx->map[k].colname,fx->recnum));
				k++;
				continue;
			} else if (fx->map[k].digits > 0) {
				fx->map[k].sqlfld.size = fx->map[k].sqlinlen;
				cob_move (&fx->map[k].sqlfld, &fx->map[k].recfld);
#ifdef COB_DEBUG_LOG
				if (fx->map[k].type == COB_XFDT_PIC9L
				 || fx->map[k].type == COB_XFDT_PIC9LS
				 || fx->map[k].type == COB_XFDT_PIC9T
				 || fx->map[k].type == COB_XFDT_PIC9TS
				 || fx->map[k].type == COB_XFDT_PIC9S
				 || fx->map[k].type == COB_XFDT_PIC9U) {
					sprintf(hexwrk,"'%.*s'",(int)fx->map[k].recfld.size,fx->map[k].recfld.data);
				} else {
					hex_dump( fx->map[k].recfld.data,(int)fx->map[k].recfld.size, hexwrk);
				}
				DEBUG_LOG("db",("%3d: Read %s type: %02d %dv%d inlen:%d fldsz:%d\n",k,
							fx->map[k].colname,fx->map[k].type,
							fx->map[k].digits,fx->map[k].scale,
							fx->map[k].sqlinlen,(int)fx->map[k].recfld.size));
				DEBUG_LOG ("db",("   SQL:%.*s -> %s\n",
									fx->map[k].sqlinlen,fx->map[k].sqlfld.data,
									hexwrk));
#endif
			} else {
				fx->map[k].sqlfld.size = fx->map[k].sqlinlen;
				cob_move (&fx->map[k].sqlfld, &fx->map[k].recfld);
#ifdef COB_DEBUG_LOG
				DEBUG_LOG("db",("%3d: Read %s type: %02d len:%d\n",k,
							fx->map[k].colname,fx->map[k].type,
							fx->map[k].sqlinlen));
				DEBUG_DUMP("db",fx->map[k].sqlfld.data,fx->map[k].sqlfld.size);
				DEBUG_DUMP("db",fx->map[k].recfld.data,fx->map[k].recfld.size);
#endif
			}
			k++;

		} else if (fx->map[k].cmd == XC_GOTO) {
			if (fx->map[k].jumpto > 0) {
				k = fx->map[k].jumpto;
				continue;
			}
			nx = fx->xlbl[fx->map[k].target];
			do {
				k++;
			} while (k < nx);
			continue;
		} else if (fx->map[k].cmd == XC_WHEN) {
			nx = eval_expr (fx, k);
			if (nx < k)
				break;
			k = nx;
		} else {
			k++;
		}
	}
}

/*
 * Create DDL from XFD
 */
void 
cob_xfd_to_ddl (struct db_state *db, struct file_xfd *fx, FILE *fo)
{
	int	k,nx,col;
	char		comma[8],idxname[48];
	fprintf(fo,"CREATE TABLE %s (",fx->tablename);
	strcpy(comma,"\n");
	for (k=0; k < fx->nmap; k++) {
		if (fx->map[k].cmd == XC_DATA
		 && fx->map[k].colname
		 && fx->map[k].level > 0) {
			fprintf(fo,"%s%s ",comma,fx->map[k].colname);
			strcpy(comma,",\n");
			if (fx->map[k].dtfrm) {
				if (fx->map[k].dtfrm->hasTime 
				 && fx->map[k].dtfrm->hasDate) 
					fprintf(fo,"TIMESTAMP");
				else
				if (fx->map[k].dtfrm->hasTime 
				 && db->isoci)
					fprintf(fo,"DATE");
				else
				if (fx->map[k].dtfrm->hasTime)
					fprintf(fo,"TIME");
				else
					fprintf(fo,"DATE");
			} else if (fx->map[k].type == COB_XFDT_FLOAT) {
				if (fx->map[k].size > 4)
					fprintf(fo,"FLOAT(53)");
				else
					fprintf(fo,"FLOAT(23)");
			} else if ( fx->map[k].type == COB_XFDT_COMP5IDX
					 || fx->map[k].type == COB_XFDT_COMP5REL) {
					fprintf(fo,"INTEGER");
			} else if ( fx->map[k].type == COB_XFDT_PIC9L
					 || fx->map[k].type == COB_XFDT_PIC9LS
					 || fx->map[k].type == COB_XFDT_PIC9T
					 || fx->map[k].type == COB_XFDT_PIC9TS
					 || fx->map[k].type == COB_XFDT_PIC9S
					 || fx->map[k].type == COB_XFDT_COMP5S
					 || fx->map[k].type == COB_XFDT_COMP5U
					 || fx->map[k].type == COB_XFDT_COMPS
					 || fx->map[k].type == COB_XFDT_COMPU
					 || fx->map[k].type == COB_XFDT_COMPX
					 || fx->map[k].type == COB_XFDT_PACKS
					 || fx->map[k].type == COB_XFDT_PACKU
					 || fx->map[k].type == COB_XFDT_PIC9U) {
				if (fx->map[k].scale > 0)
					fprintf(fo,"DECIMAL(%d,%d)",fx->map[k].digits,fx->map[k].scale);
				else
					fprintf(fo,"DECIMAL(%d)",fx->map[k].digits);
			} else if (fx->map[k].type == COB_XFDT_BIN) {
				if (db->isoci)
					fprintf(fo,"RAW(%d)",fx->map[k].size);
				else
					fprintf(fo,"BINARY(%d)",fx->map[k].size);
			} else if (fx->map[k].type == COB_XFDT_VARX) {
				fprintf(fo,"VARCHAR(%d)",fx->map[k].size);
			} else {
				fprintf(fo,"CHAR(%d)",fx->map[k].size);
			}
			if (fx->map[k].notnull)
				fprintf(fo," NOT NULL");
		}
	}
	if (fx->fl
	 && fx->fl->organization == COB_ORG_RELATIVE) {
		fprintf(fo,"%srid_%s %s PRIMARY KEY",comma,fx->tablename,
					db->isoci?"INT":(db->sqlite?"INTEGER":"BIGINT"));
	}
	fprintf(fo,"\n);\n");

	if (fx->fl
	 && fx->fl->organization == COB_ORG_INDEXED) {
		for (nx=0; nx < fx->nkeys && fx->key[nx]; nx++) {
			if(nx == 0)
				sprintf(idxname,"pk_%s",fx->tablename);
			else
				sprintf(idxname,"k%d_%s",nx,fx->tablename);
			fprintf(fo,"CREATE %sINDEX %s ON %s (",
						fx->key[nx]->dups || fx->key[nx]->sup ? "" : "UNIQUE ",
						idxname,fx->tablename);
			strcpy(comma,"");
			for(k=0; k < fx->key[nx]->ncols; k++) {
				col = fx->key[nx]->col[k];
				fprintf(fo,"%s%s",comma,fx->map[col].colname);
				strcpy(comma,",");
			}
			fprintf(fo,");\n");
		}
	}
}
#endif

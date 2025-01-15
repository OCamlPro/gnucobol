/*
   Copyright (C) 2006-2012, 2013, 2017-2023 Free Software Foundation, Inc.
   Written by Roger While, Ron Norman, Simon Sobisch, Edward Hart

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

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>

#include "cobc.h"
#include "tree.h"

#ifdef COB_NO_UNALIGNED_ATTRIBUTE
#define UNALIGNED_ATTRIBUTE ""
#else
#define UNALIGNED_ATTRIBUTE "__unaligned "
#endif

static void
output_storage (const char *fmt, ...)
{
	va_list		ap;

	if (cb_storage_file) {
		va_start (ap, fmt);
		vfprintf (cb_storage_file, fmt, ap);
		va_end (ap);
		fputc ('\n', cb_storage_file);
	}
}

void
cob_gen_optim (const enum cb_optim val)
{
	output_storage ("");
	switch (val) {

	case COB_SET_SCREEN:
		output_storage ("#define COB_SET_SCREEN(s,typ,att,nxt,prv,chld,p,fld,val,l,c,fg,bg,prmpt,occ) \\");
		output_storage ("do{	s.next = nxt;     \\");
		output_storage ("	s.prev = prv;     \\");
		output_storage ("	s.child = chld;   \\");
		output_storage ("	s.parent = p;     \\");
		output_storage ("	s.field = fld;    \\");
		output_storage ("	s.value = val;    \\");
		output_storage ("	s.line = l;       \\");
		output_storage ("	s.column = c;     \\");
		output_storage ("	s.foreg = fg;     \\");
		output_storage ("	s.backg = bg;     \\");
		output_storage ("	s.prompt = prmpt; \\");
		output_storage ("	s.type = typ;     \\");
		output_storage ("	s.occurs = occ;   \\");
		output_storage ("	s.attr = att;     \\");
		output_storage ("} ONCE_COB");
		return;

	case COB_SET_REPORT:
		output_storage ("static void COB_INLINE COB_A_INLINE");
		output_storage ("cob_set_report (cob_report *r, cob_file *pfile)");
		output_storage ("{");
		output_storage ("	r->report_file = pfile;");
		output_storage ("}");
		return;

	case COB_SET_ML_TREE:
		output_storage ("static void COB_NOINLINE");
		output_storage ("cob_set_ml_attr (cob_ml_attr *attr, cob_field *name,");
		output_storage ("		cob_field *value, unsigned int is_suppressed,");
		output_storage ("		cob_ml_attr *sibling)");
		output_storage ("{");
		output_storage ("	attr->name = name;");
		output_storage ("	attr->value = value;");
		output_storage ("	attr->is_suppressed = is_suppressed;");
		output_storage ("	attr->sibling = sibling;");
		output_storage ("}");
		output_storage ("static void COB_NOINLINE");
		output_storage ("cob_set_ml_tree (cob_ml_tree *tree, cob_field *name, cob_ml_attr *attrs,");
		output_storage ("		cob_field *content, unsigned int is_suppressed,");
		output_storage ("		cob_ml_tree *children, cob_ml_tree *sibling)");
		output_storage ("{");
		output_storage ("	tree->name = name;");
		output_storage ("	tree->attrs = attrs;");
		output_storage ("	tree->content = content;");
		output_storage ("	tree->is_suppressed = is_suppressed;");
		output_storage ("	tree->children = children;");
		output_storage ("	tree->sibling = sibling;");
		output_storage ("}");
		return;

	case COB_CHK_BASED:
		/* no need for an expensive function call (at least prevented if inline is honored)
		   if we know the pointer to be non-null;
		   may be changed if we do a more expensive check to validate the pointer */
		output_storage ("static void COB_INLINE COB_A_INLINE");
		output_storage ("cob_check_based_inline (const unsigned char *ptr, const char *name)");
		output_storage ("{");
		output_storage ("	if (!ptr) cob_check_based (NULL, name);");
		output_storage ("}");
		output_storage ("#define cob_check_based" "\t" "cob_check_based_inline");
		return;

	case COB_CHK_LINKAGE:
		/* no need for an expensive function call (at least prevented if inline is honored)
		   if we know the pointer to be non-null;
		   may be changed if we do a more expensive check to validate the pointer */
		output_storage ("static void COB_INLINE COB_A_INLINE");
		output_storage ("cob_check_linkage_inline (const unsigned char *ptr, const char *name)");
		output_storage ("{");
		output_storage ("	if (!ptr) cob_check_linkage (NULL, name);");
		output_storage ("}");
		output_storage ("#define cob_check_linkage" "\t" "cob_check_linkage_inline");
		return;

	case COB_CHK_SUBSCRIPT:
		/* no need for an expensive function call (at least prevented if inline is honored)
		   if we know the subscript to be valid inline function used in any case to prevent
		   "i" being resolved more than once as it may need unpacking */
		output_storage ("static void COB_INLINE COB_A_INLINE");
		output_storage ("cob_check_subscript_inline (const int i, const int max,");
		output_storage ("			const char* name, const int odo_item)");
		output_storage ("{");
		output_storage ("	if (i < 1 || i > max) {");
		output_storage ("		cob_check_subscript (i, max, name, odo_item);");
		output_storage ("	}");
		output_storage ("}");
		output_storage ("#define cob_check_subscript" "\t" "cob_check_subscript_inline");
		return;

	case COB_CHK_ODO:
		/* no need for an expensive function call (at east prevented if inline is honored)
		   if we know the subscript to be valid, inline function used in any case to prevent
		   "i" being resolved more than once as it may need unpacking */
		output_storage ("static void COB_INLINE COB_A_INLINE");
		output_storage ("cob_check_odo_inline (const int i, const int min, const int max,");
		output_storage ("			const char* name, const char* dep_name)");
		output_storage ("{");
		output_storage ("	if (i < min || i > max) {");
		output_storage ("		cob_check_odo (i, min, max, name, dep_name);");
		output_storage ("	}");
		output_storage ("}");
		output_storage ("#define cob_check_odo" "\t" "cob_check_odo_inline");
		return;

	case COB_CHK_REFMOD:
		/* no need for an expensive function call (at least prevented if inline is honored)
		   if we know the refmod to be valid, inline function used in any case to prevent
		   "offset" and "length" being resolved more than once as it may need unpacking */
		output_storage ("static void COB_INLINE COB_A_INLINE");
		output_storage ("cob_check_ref_mod_inline (const char* name, const int abend, const int zero_allowed,");
		output_storage ("	const int size, const int offset, const int length)");
		output_storage ("{");
		output_storage ("	const int minimal_length = zero_allowed ? 0 : 1;");
		output_storage ("	if (offset < 1 || length < minimal_length");
		output_storage ("	 || offset + length - 1 > size) {");
		output_storage ("		cob_check_ref_mod_detailed (name, abend, zero_allowed, size, offset, length);");
		output_storage ("	}");
		output_storage ("}");
		output_storage ("#define cob_check_ref_mod_detailed" "\t" "cob_check_ref_mod_inline");
		return;

	case COB_CHK_REFMOD_MIN:
		/* no need for an expensive function call (at least prevented if inline is honored)
		   if we know the refmod to be valid */
		output_storage ("static void COB_INLINE COB_A_INLINE");
		output_storage ("cob_check_ref_mod_minimal_inline (const char* name, const int offset, const int length)");
		output_storage ("{");
		output_storage ("	if (offset < 1 || length < 1) cob_check_ref_mod_minimal (name, offset, length);");
		output_storage ("}");
		output_storage ("#define cob_check_ref_mod_minimal" "\t" "cob_check_ref_mod_minimal_inline");
		return;

	case COB_CHK_MEMORYFENCE:
		/* no need for an expensive function call (at least prevented if inline is honored)
		   if we know the memory fence to be valid */
		output_storage ("static void COB_INLINE COB_A_INLINE");
		output_storage ("cob_check_fence_inline (const char *fence_pre, const char *fence_post,");
		output_storage ("	const enum cob_statement stmt, const char *name)");
		output_storage ("{");
		output_storage ("	if (memcmp (fence_pre, \"\\xFF\\xFE\\xFD\\xFC\\xFB\\xFA\\xFF\", 8)");
		output_storage ("	 || memcmp (fence_post, \"\\xFA\\xFB\\xFC\\xFD\\xFE\\xFF\\xFA\", 8)) {");
		output_storage ("		cob_check_fence (fence_pre, fence_post, stmt, name);");
		output_storage ("	}");
		output_storage ("}");
		output_storage ("#define cob_check_fence" "\t" "cob_check_fence_inline");
		return;

	case COB_NOP:
		/* cob_nop is only used to force something the optimizer does not remove
		   to have "something" to call; a fast check (module is normally always set)
		   costs less than a function call; no need for inline function as this is
		   always a separate generated call */
		output_storage ("#define cob_nop" "\t" "if (!module) cob_nop");
		return;

	case COB_POINTER_MANIP:
		output_storage ("static void COB_NOINLINE");
		output_storage ("cob_pointer_manip (void *f1, cob_field *f2, const unsigned int addsub)");
		output_storage ("{");
		output_storage ("	unsigned char   *tmptr;");
		output_storage ("	memcpy (&tmptr, f1, sizeof(unsigned char *));");
		output_storage ("	if (addsub) {");
		output_storage ("		tmptr -= cob_get_int (f2);");
		output_storage ("	} else {");
		output_storage ("		tmptr += cob_get_int (f2);");
		output_storage ("	}");
		output_storage ("	memcpy (f1, &tmptr, sizeof(unsigned char *));");
		output_storage ("}");
		return;

	case COB_GET_NUMDISP:
		output_storage ("static int COB_INLINE COB_A_INLINE");
		output_storage ("cob_get_numdisp (const void *data, const int size)");
		output_storage ("{");
		output_storage ("	register const unsigned char	*p = (const unsigned char *)data;");
		output_storage ("	register int	n;");
		output_storage ("	register int 	val = 0;");
		/* Improve performance by skipping leading ZEROs */
		output_storage ("	for (n = 0; n < val; ++n, ++p) {");
		output_storage ("		if (*p > '0' && *p <= '9')");
		output_storage ("			break;");
		output_storage ("	}");
			/* Improve performance by skipping leading ZEROs */
		output_storage ("	for (n = 0; n < size; ++n, ++p) {");
		output_storage ("		if (*p > '0' && *p <= '9')");
		output_storage ("	       break;");
		output_storage ("	}");
		output_storage ("	for (; n < size; ++n, ++p) {");
		output_storage ("   	val = (val * 10)");
		output_storage ("   	       + ((*p > '0' && *p <= '9') ? (*p - '0') : 0);");
		output_storage ("	}");
		output_storage ("	return val;");
		output_storage ("}");
		return;

	case COB_GET_NUMDISPS:
		output_storage ("static int COB_INLINE COB_A_INLINE");
		output_storage ("cob_get_numdisps (const void *data, const int size)");
		output_storage ("{");
		output_storage ("	register const unsigned char	*p = (const unsigned char *)data;");
		output_storage ("	register int	n;");
		output_storage ("	register int 	val = size - 1;");
		/* Improve performance by skipping leading ZEROs */
		output_storage ("	for (n = 0; n < val; ++n, ++p) {");
		output_storage ("		if (*p > '0' && *p <= '9')");
		output_storage ("			break;");
		output_storage ("	}");
		output_storage ("	val = 0;");
		output_storage ("	for (; n < size; ++n, ++p) {");
		output_storage ("		val *= 10;");
		output_storage ("		if (*p > '0' && *p <= '9') {");
		output_storage ("		    val += (*p - '0');");
		output_storage ("		} else if ((*p & 0x40) && (n + 1) == size) {");
		output_storage ("		    val += (*p & 0x0F);");
		output_storage ("		    val = -val;");
		output_storage ("	    }");
		output_storage ("	}");
		output_storage ("	return val;");
		output_storage ("}");
		return;

	case COB_GET_NUMDISP64:
		output_storage ("static cob_s64_t COB_INLINE COB_A_INLINE");
		output_storage ("cob_get_numdisp64 (const void *data, const int size)");
		output_storage ("{");
		output_storage ("	register const unsigned char	*p;");
		output_storage ("	register int	n;");
		output_storage ("	register cob_s64_t 	val = 0;");
		output_storage ("	p = (const unsigned char *)data;");
			/* Improve performance by skipping leading ZEROs */
		output_storage ("	for (n = 0; n < size; ++n, ++p) {");
		output_storage ("		if (*p > '0' && *p <= '9')");
		output_storage ("	       break;");
		output_storage ("	}");
		output_storage ("	for (; n < size; ++n, ++p) {");
		output_storage ("   	val = (val * 10)");
		output_storage ("   	       + ((*p > '0' && *p <= '9') ? (*p - '0') : 0);");
		output_storage ("	}");
		output_storage ("	return val;");
		output_storage ("}");
		return;

	case COB_GET_NUMDISPS64:
		output_storage ("static cob_s64_t COB_INLINE COB_A_INLINE");
		output_storage ("cob_get_numdisps64 (const void *data, const int size)");
		output_storage ("{");
		output_storage ("	register const unsigned char	*p;");
		output_storage ("	register cob_s64_t	n;");
		output_storage ("	register cob_s64_t	val = size - 1;");
		output_storage ("	p = (const unsigned char *)data;");
			/* Improve performance by skipping leading ZEROs */
		output_storage ("	for (n = 0; n < val; ++n, ++p) {");
		output_storage ("		if (*p > '0' && *p <= '9')");
		output_storage ("			break;");
		output_storage ("	}");
		output_storage ("	val = 0;");
		output_storage ("	for (; n < size; ++n, ++p) {");
		output_storage ("		val *= 10;");
		output_storage ("		if (*p > '0' && *p <= '9') {");
		output_storage ("		    val += (*p - '0');");
		output_storage ("		} else if ((*p & 0x40) && (n + 1) == size) {");
		output_storage ("		    val += (*p & 0x0F);");
		output_storage ("		    val = -val;");
		output_storage ("	    }");
		output_storage ("	}");
		output_storage ("	return val;");
		output_storage ("}");
		return;

#if 0	/* libcob's optimized version is not slower, so drop that */
	case COB_CMP_PACKED_INT:
		output_storage ("static int COB_NOINLINE");
		output_storage ("cob_cmp_packed_int (const cob_field *f, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	register unsigned char		*p = f->data;");
		output_storage ("	const register unsigned char	*p_end = p + f->size - 1;");
		output_storage ("	register cob_s64_t	val = 0;");

		output_storage ("	while (p < p_end) {");
		output_storage ("		val = val * 10");
		output_storage ("		    + (*p >> 4);");
		output_storage ("		val = val * 10");
		output_storage ("		    + (*p++ & 0x0f);");
		output_storage ("	}");
		output_storage ("	val = val * 10");
		output_storage ("	    + (*p >> 4);");
		output_storage ("	if ((*p & 0x0f) == 0x0d) {");
		output_storage ("		val = -val;");
		output_storage ("	}");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_GET_PACKED_INT:
		output_storage ("static int COB_NOINLINE");
		output_storage ("cob_get_packed_int (const cob_field *f)");
		output_storage ("{");
		output_storage ("	register unsigned char		*p = f->data;");
		output_storage ("	const register unsigned char	*p_end = p + f->size - 1;");
		output_storage ("	register int	val = 0;");

		output_storage ("	while (p < p_end) {");
		output_storage ("		val = val * 10");
		output_storage ("		    + (*p >> 4);");
		output_storage ("		val = val * 10");
		output_storage ("		    + (*p++ & 0x0f);");
		output_storage ("	}");
		output_storage ("	val = val * 10");
		output_storage ("	    + (*p >> 4);");
		output_storage ("	if ((*p & 0x0f) == 0x0d) {");
		output_storage ("		val = -val;");
		output_storage ("	}");
		output_storage ("	return val;");
		output_storage ("}");
		return;

	case COB_GET_PACKED_INT64:
		output_storage ("static cob_s64_t COB_NOINLINE");
		output_storage ("cob_get_packed_int64 (const cob_field *f)");
		output_storage ("{");
		output_storage ("	register unsigned char		*p = f->data;");
		output_storage ("	const register unsigned char	*p_end = p + f->size - 1;");
		output_storage ("	register cob_s64_t	val = 0;");

		output_storage ("	while (p < p_end) {");
		output_storage ("		val = val * 10");
		output_storage ("		    + (*p >> 4);");
		output_storage ("		val = val * 10");
		output_storage ("		    + (*p++ & 0x0f);");
		output_storage ("	}");
		output_storage ("	val = val * 10");
		output_storage ("	    + (*p >> 4);");
		output_storage ("	if ((*p & 0x0f) == 0x0d) {");
		output_storage ("		val = -val;");
		output_storage ("	}");
		output_storage ("	return val;");
		output_storage ("}");
		return;
#endif

	case COB_ADD_PACKED_INT:
		output_storage ("static int COB_NOINLINE");
		output_storage ("cob_add_packed_int (cob_field *f, const int val)");
		output_storage ("{");
		output_storage ("	register unsigned char	*p;");
		output_storage ("	size_t		size;");
		output_storage ("	int		carry = 0;");
		output_storage ("	int		n;");
		output_storage ("	int		inc;");

		output_storage ("	if (val == 0) {");
		output_storage ("		return 0;");
		output_storage ("	}");
		output_storage ("	p = f->data + f->size - 1;");
		output_storage ("	if ((*p & 0x0f) == 0x0d) {");
		output_storage ("		if (val > 0) {");
		output_storage ("			return cob_add_int (f, val, 0);");
		output_storage ("		}");
		output_storage ("		n = -val;");
		output_storage ("	} else {");
		output_storage ("		if (val < 0) {");
		output_storage ("			return cob_add_int (f, val, 0);");
		output_storage ("		}");
		output_storage ("		n = val;");
		output_storage ("	}");
		output_storage ("	inc = (*p >> 4) + (n %% 10);");
		output_storage ("	n /= 10;");
		output_storage ("	carry = inc / 10;");
		output_storage ("	*p = ((inc %% 10) << 4) | (*p & 0x0f);");
		output_storage ("	p--;");

		output_storage ("	for (size = 0; size < f->size - 1; ++size, --p) {");
		output_storage ("		if (!carry && !n) {");
		output_storage ("			break;");
		output_storage ("		}");
		output_storage ("		inc = ((*p >> 4) * 10) + (*p & 0x0f) + carry + (n %% 100);");
		output_storage ("		carry = inc / 100;");
		output_storage ("		n /= 100;");
		output_storage ("		inc %%= 100;");
		output_storage ("		*p = ((inc / 10) << 4) | (inc %% 10);");
		output_storage ("	}");
		output_storage ("	return 0;");
		output_storage ("}");
		return;

	case COB_ADD_PACKED_INT64:
		output_storage ("static int COB_NOINLINE");
		output_storage ("cob_add_packed_int64 (cob_field *f, const cob_s64_t val)");
		output_storage ("{");
		output_storage ("	register unsigned char	*p;");
		output_storage ("	size_t		size;");
		output_storage ("	cob_s64_t	carry = 0;");
		output_storage ("	cob_s64_t	n;");
		output_storage ("	cob_s64_t	inc;");

		output_storage ("	if (val == 0) {");
		output_storage ("		return 0;");
		output_storage ("	}");
		output_storage ("	p = f->data + f->size - 1;");
		output_storage ("	if ((*p & 0x0f) == 0x0d) {");
		output_storage ("		if (val > 0) {");
		output_storage ("			return cob_add_int (f, val, 0);");
		output_storage ("		}");
		output_storage ("		n = -val;");
		output_storage ("	} else {");
		output_storage ("		if (val < 0) {");
		output_storage ("			return cob_add_int (f, val, 0);");
		output_storage ("		}");
		output_storage ("		n = val;");
		output_storage ("	}");
		output_storage ("	inc = (*p >> 4) + (n %% 10);");
		output_storage ("	n /= 10;");
		output_storage ("	carry = inc / 10;");
		output_storage ("	*p = ((inc %% 10) << 4) | (*p & 0x0f);");
		output_storage ("	p--;");

		output_storage ("	for (size = 0; size < f->size - 1; ++size, --p) {");
		output_storage ("		if (!carry && !n) {");
		output_storage ("			break;");
		output_storage ("		}");
		output_storage ("		inc = ((*p >> 4) * 10) + (*p & 0x0f) + carry + (n %% 100);");
		output_storage ("		carry = inc / 100;");
		output_storage ("		n /= 100;");
		output_storage ("		inc %%= 100;");
		output_storage ("		*p = ((inc / 10) << 4) | (inc %% 10);");
		output_storage ("	}");
		output_storage ("	return 0;");
		output_storage ("}");
		return;

	/* Aligned variants */

	/* Aligned compares */

	case COB_CMP_ALIGN_U16:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_align_u16 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	unsigned short	val;");

		output_storage ("	if (n < 0) {");
		output_storage ("		return 1;");
		output_storage ("	}");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = *(unsigned short " UNALIGNED_ATTRIBUTE "*)p;");
#else
		output_storage ("	optim_memcpy ((void*)&val,p,sizeof(unsigned short));");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_ALIGN_S16:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_align_s16 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	short	val;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = *(short " UNALIGNED_ATTRIBUTE "*)p;");
#else
		output_storage ("	optim_memcpy ((void*)&val,p,sizeof(short));");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_ALIGN_U32:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_align_u32 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	unsigned int	val;");

		output_storage ("	if (n < 0) {");
		output_storage ("		return 1;");
		output_storage ("	}");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = *(unsigned int " UNALIGNED_ATTRIBUTE "*)p;");
#else
		output_storage ("	optim_memcpy ((void*)&val,p,sizeof(unsigned int));");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_ALIGN_S32:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_align_s32 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	int	val;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = *(int " UNALIGNED_ATTRIBUTE "*)p;");
#else
		output_storage ("	optim_memcpy ((void*)&val,p,sizeof(int));");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_ALIGN_U64:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_align_u64 (const void *p, const cob_u64_t n)");
		output_storage ("{");
		output_storage ("	cob_u64_t	val;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = *(cob_u64_t " UNALIGNED_ATTRIBUTE "*)p;");
#else
		output_storage ("	optim_memcpy ((void*)&val,p,sizeof(cob_u64_t));");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_ALIGN_S64:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_align_s64 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_s64_t	val;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = *(cob_s64_t " UNALIGNED_ATTRIBUTE "*)p;");
#else
		output_storage ("	optim_memcpy((void*)&val,p,sizeof(cob_s64_t));");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	/* Aligned adds */

	case COB_ADD_ALIGN_U16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_align_u16 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(unsigned short " UNALIGNED_ATTRIBUTE "*)p += val;");
		output_storage ("}");
		return;

	case COB_ADD_ALIGN_S16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_align_s16 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(short " UNALIGNED_ATTRIBUTE "*)p += val;");
		output_storage ("}");
		return;

	case COB_ADD_ALIGN_U32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_align_u32 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(unsigned int " UNALIGNED_ATTRIBUTE "*)p += val;");
		output_storage ("}");
		return;

	case COB_ADD_ALIGN_S32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_align_s32 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(int " UNALIGNED_ATTRIBUTE "*)p += val;");
		output_storage ("}");
		return;

	case COB_ADD_ALIGN_U64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_align_u64 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(cob_u64_t " UNALIGNED_ATTRIBUTE "*)p += val;");
		output_storage ("}");
		return;

	case COB_ADD_ALIGN_S64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_align_s64 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(cob_s64_t " UNALIGNED_ATTRIBUTE "*)p += val;");
		output_storage ("}");
		return;

	/* Aligned subtracts */

	case COB_SUB_ALIGN_U16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_align_u16 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(unsigned short " UNALIGNED_ATTRIBUTE "*)p -= val;");
		output_storage ("}");
		return;

	case COB_SUB_ALIGN_S16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_align_s16 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(short " UNALIGNED_ATTRIBUTE "*)p -= val;");
		output_storage ("}");
		return;

	case COB_SUB_ALIGN_U32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_align_u32 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(unsigned int " UNALIGNED_ATTRIBUTE "*)p -= val;");
		output_storage ("}");
		return;

	case COB_SUB_ALIGN_S32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_align_s32 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(int " UNALIGNED_ATTRIBUTE "*)p -= val;");
		output_storage ("}");
		return;

	case COB_SUB_ALIGN_U64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_align_u64 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(cob_u64_t " UNALIGNED_ATTRIBUTE "*)p -= val;");
		output_storage ("}");
		return;

	case COB_SUB_ALIGN_S64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_align_s64 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(cob_s64_t " UNALIGNED_ATTRIBUTE "*)p -= val;");
		output_storage ("}");
		return;

	case COB_CMPSWP_ALIGN_U16:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_align_u16 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	unsigned short	val;");

		output_storage ("	if (n < 0) {");
		output_storage ("		return 1;");
		output_storage ("	}");
		output_storage ("	val = COB_BSWAP_16 (*(unsigned short " UNALIGNED_ATTRIBUTE "*)p);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_ALIGN_S16:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_align_s16 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	short	val;");

		output_storage ("	val = COB_BSWAP_16 (*(short " UNALIGNED_ATTRIBUTE "*)p);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_ALIGN_U32:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_align_u32 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	unsigned int	val;");

		output_storage ("	if (n < 0) {");
		output_storage ("		return 1;");
		output_storage ("	}");
		output_storage ("	val = COB_BSWAP_32 (*(unsigned int " UNALIGNED_ATTRIBUTE "*)p);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_ALIGN_S32:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_align_s32 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	int	val;");

		output_storage ("	val = COB_BSWAP_32 (*(int " UNALIGNED_ATTRIBUTE "*)p);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_ALIGN_U64:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_align_u64 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_u64_t	val;");

		output_storage ("	if (n < 0) {");
		output_storage ("		return 1;");
		output_storage ("	}");
		output_storage ("	val = COB_BSWAP_64 (*(cob_u64_t " UNALIGNED_ATTRIBUTE "*)p);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_ALIGN_S64:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_align_s64 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_s64_t	val;");

		output_storage ("	val = COB_BSWAP_64 (*(cob_s64_t " UNALIGNED_ATTRIBUTE "*)p);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	/* Binary compare */

	case COB_CMP_U8:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_u8 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	if (n < 0) {");
		output_storage ("		return 1;");
		output_storage ("	}");
		output_storage ("	return (*(const unsigned char *)p < n) ? -1 : (*(const unsigned char *)p > n);");
		output_storage ("}");
		return;

	case COB_CMP_S8:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_s8 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	return (*(const signed char *)p < n) ? -1 : (*(const signed char *)p > n);");
		output_storage ("}");
		return;

	case COB_CMP_U16:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_u16 (const void *p, const cob_s64_t n)");
		output_storage ("{");
#ifndef COB_ALLOW_UNALIGNED
		output_storage ("	void		*x;");
#endif
		output_storage ("	unsigned short	val;");

		output_storage ("	if (n < 0) {");
		output_storage ("		return 1;");
		output_storage ("	}");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = *(const unsigned short " UNALIGNED_ATTRIBUTE "*)p;");
#else
		output_storage ("	x = &val;");
		output_storage ("	optim_memcpy (x, p, 2);");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_S16:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_s16 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	short	val;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = *(const short " UNALIGNED_ATTRIBUTE "*)p;");
#else
		output_storage ("	void	*x;");

		output_storage ("	x = &val;");
		output_storage ("	optim_memcpy (x, p, 2);");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_U24:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_u24 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned int	val = 0;");

		output_storage ("	if (n < 0) {");
		output_storage ("		return 1;");
		output_storage ("	}");
#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&val) + 1;");
#else
		output_storage ("	x = (unsigned char *)&val;");
#endif
		output_storage ("	optim_memcpy (x, p, 3);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_S24:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_s24 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	int		val = 0;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = (unsigned char *)&val;");
#else
		output_storage ("	x = ((unsigned char *)&val) + 1;");
#endif
		output_storage ("	optim_memcpy (x, p, 3);");
		output_storage ("	val >>= 8;	/* Shift with sign */");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_U32:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_u32 (const void *p, const cob_s64_t n)");
		output_storage ("{");
#ifndef COB_ALLOW_UNALIGNED
		output_storage ("	void		*x;");
#endif
		output_storage ("	unsigned int	val;");

		output_storage ("	if (n < 0) {");
		output_storage ("		return 1;");
		output_storage ("	}");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = *(const unsigned int " UNALIGNED_ATTRIBUTE "*)p;");
#else
		output_storage ("	x = &val;");
		output_storage ("	optim_memcpy (x, p, 4);");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_S32:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_s32 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	int	val;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = *(const int " UNALIGNED_ATTRIBUTE "*)p;");
#else
		output_storage ("	void	*x;");

		output_storage ("	x = &val;");
		output_storage ("	optim_memcpy (x, p, 4);");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_U40:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_u40 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_u64_t		val = 0;");
		output_storage ("	unsigned char		*x;");

		output_storage ("	if (n < 0) {");
		output_storage ("		return 1;");
		output_storage ("	}");
#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&val) + 3;");
#else
		output_storage ("	x = (unsigned char *)&val;");
#endif
		output_storage ("	optim_memcpy (x, p, 5);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_S40:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_s40 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_s64_t		val = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = (unsigned char *)&val;");
#else
		output_storage ("	x = ((unsigned char *)&val) + 3;");
#endif
		output_storage ("	optim_memcpy (x, p, 5);");
		output_storage ("	val >>= 24;	/* Shift with sign */");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_U48:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_u48 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_u64_t		val = 0;");
		output_storage ("	unsigned char		*x;");

		output_storage ("	if (n < 0) {");
		output_storage ("		return 1;");
		output_storage ("	}");
#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&val) + 2;");
#else
		output_storage ("	x = (unsigned char *)&val;");
#endif
		output_storage ("	optim_memcpy (x, p, 6);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_S48:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_s48 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_s64_t		val = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = (unsigned char *)&val;");
#else
		output_storage ("	x = ((unsigned char *)&val) + 2;");
#endif
		output_storage ("	optim_memcpy (x, p, 6);");
		output_storage ("	val >>= 16;	/* Shift with sign */");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_U56:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_u56 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_u64_t		val = 0;");
		output_storage ("	unsigned char		*x;");

		output_storage ("	if (n < 0) {");
		output_storage ("		return 1;");
		output_storage ("	}");
#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&val) + 1;");
#else
		output_storage ("	x = (unsigned char *)&val;");
#endif
		output_storage ("	optim_memcpy (x, p, 7);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_S56:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_s56 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_s64_t		val = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = (unsigned char *)&val;");
#else
		output_storage ("	x = ((unsigned char *)&val) + 1;");
#endif
		output_storage ("	optim_memcpy (x, p, 7);");
		output_storage ("	val >>= 8;	/* Shift with sign */");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_U64:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_u64 (const void *p, const cob_u64_t n)");
		output_storage ("{");
#ifndef COB_ALLOW_UNALIGNED
		output_storage ("	void		*x;");
#endif
		output_storage ("	cob_u64_t	val;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = *(const cob_u64_t " UNALIGNED_ATTRIBUTE "*)p;");
#else
		output_storage ("	x = &val;");
		output_storage ("	optim_memcpy (x, p, 8);");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_S64:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_s64 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_s64_t	val;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = *(const cob_s64_t " UNALIGNED_ATTRIBUTE "*)p;");
#else
		output_storage ("	void		*x;");

		output_storage ("	x = &val;");
		output_storage ("	optim_memcpy (x, p, 8);");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	/* Add/Subtract */

	case COB_ADD_U8:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_u8 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(unsigned char *)p += val;");
		output_storage ("}");
		return;

	case COB_ADD_S8:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_s8 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(signed char *)p += val;");
		output_storage ("}");
		return;

	case COB_ADD_U16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_u16 (void *p, const int val)");
		output_storage ("{");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	*(unsigned short " UNALIGNED_ATTRIBUTE "*)p += val;");
#else
		output_storage ("	void		*x;");
		output_storage ("	unsigned short	n;");

		output_storage ("	x = &n;");
		output_storage ("	optim_memcpy (x, p, 2);");
		output_storage ("	n += val;");
		output_storage ("	optim_memcpy (p, x, 2);");
#endif
		output_storage ("}");
		return;

	case COB_ADD_S16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_s16 (void *p, const int val)");
		output_storage ("{");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	*(short " UNALIGNED_ATTRIBUTE "*)p += val;");
#else
		output_storage ("	void	*x;");
		output_storage ("	short	n;");

		output_storage ("	x = &n;");
		output_storage ("	optim_memcpy (x, p, 2);");
		output_storage ("	n += val;");
		output_storage ("	optim_memcpy (p, x, 2);");
#endif
		output_storage ("}");
		return;

	case COB_ADD_U24:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_u24 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned int	n = 0;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 1;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (x, p, 3);");
		output_storage ("	n += val;");
		output_storage ("	optim_memcpy (p, x, 3);");
		output_storage ("}");
		return;

	case COB_ADD_S24:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_s24 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	int		n = 0;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = (unsigned char *)&n;");
#else
		output_storage ("	x = ((unsigned char *)&n) + 1;");
#endif
		output_storage ("	optim_memcpy (x, p, 3);");
		output_storage ("	n >>= 8;	/* Shift with sign */");
		output_storage ("	n += val;");
#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 1;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (p, x, 3);");
		output_storage ("}");
		return;

	case COB_ADD_U32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_u32 (void *p, const int val)");
		output_storage ("{");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	*(unsigned int " UNALIGNED_ATTRIBUTE "*)p += val;");
#else
		output_storage ("	void		*x;");
		output_storage ("	unsigned int	n;");

		output_storage ("	x = &n;");
		output_storage ("	optim_memcpy (x, p, 4);");
		output_storage ("	n += val;");
		output_storage ("	optim_memcpy (p, x, 4);");
#endif
		output_storage ("}");
		return;

	case COB_ADD_S32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_s32 (void *p, const int val)");
		output_storage ("{");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	*(int " UNALIGNED_ATTRIBUTE "*)p += val;");
#else
		output_storage ("	void	*x;");
		output_storage ("	int	n;");

		output_storage ("	x = &n;");
		output_storage ("	optim_memcpy (x, p, 4);");
		output_storage ("	n += val;");
		output_storage ("	optim_memcpy (p, x, 4);");
#endif
		output_storage ("}");
		return;

	case COB_ADD_U40:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_u40 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 3;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (x, p, 5);");
		output_storage ("	n += val;");
		output_storage ("	optim_memcpy (p, x, 5);");
		output_storage ("}");
		return;

	case COB_ADD_S40:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_s40 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = (unsigned char *)&n;");
#else
		output_storage ("	x = ((unsigned char *)&n) + 3;");
#endif
		output_storage ("	optim_memcpy (x, p, 5);");
		output_storage ("	n >>= 24;	/* Shift with sign */");
		output_storage ("	n += val;");
#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 3;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (p, x, 5);");
		output_storage ("}");
		return;

	case COB_ADD_U48:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_u48 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 2;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (x, p, 6);");
		output_storage ("	n += val;");
		output_storage ("	optim_memcpy (p, x, 6);");
		output_storage ("}");
		return;

	case COB_ADD_S48:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_s48 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = (unsigned char *)&n;");
#else
		output_storage ("	x = ((unsigned char *)&n) + 2;");
#endif
		output_storage ("	optim_memcpy (x, p, 6);");
		output_storage ("	n >>= 16;	/* Shift with sign */");
		output_storage ("	n += val;");
#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 2;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (p, x, 6);");
		output_storage ("}");
		return;

	case COB_ADD_U56:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_u56 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 1;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (x, p, 7);");
		output_storage ("	n += val;");
		output_storage ("	optim_memcpy (p, x, 7);");
		output_storage ("}");
		return;

	case COB_ADD_S56:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_s56 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = (unsigned char *)&n;");
#else
		output_storage ("	x = ((unsigned char *)&n) + 1;");
#endif
		output_storage ("	optim_memcpy (x, p, 7);");
		output_storage ("	n >>= 8;	/* Shift with sign */");
		output_storage ("	n += val;");
#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 1;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (p, x, 7);");
		output_storage ("}");
		return;

	case COB_ADD_U64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_u64 (void *p, const int val)");
		output_storage ("{");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	*(cob_u64_t " UNALIGNED_ATTRIBUTE "*)p += val;");
#else
		output_storage ("	void		*x;");
		output_storage ("	cob_u64_t	n;");

		output_storage ("	x = &n;");
		output_storage ("	optim_memcpy (x, p, 8);");
		output_storage ("	n += val;");
		output_storage ("	optim_memcpy (p, x, 8);");
#endif
		output_storage ("}");
		return;

	case COB_ADD_S64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_s64 (void *p, const int val)");
		output_storage ("{");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	*(cob_s64_t " UNALIGNED_ATTRIBUTE "*)p += val;");
#else
		output_storage ("	void		*x;");
		output_storage ("	cob_s64_t	n;");

		output_storage ("	x = &n;");
		output_storage ("	optim_memcpy (x, p, 8);");
		output_storage ("	n += val;");
		output_storage ("	optim_memcpy (p, x, 8);");
#endif
		output_storage ("}");
		return;

	case COB_SUB_U8:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_u8 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(unsigned char *)p -= val;");
		output_storage ("}");
		return;

	case COB_SUB_S8:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_s8 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(signed char *)p -= val;");
		output_storage ("}");
		return;

	case COB_SUB_U16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_u16 (void *p, const int val)");
		output_storage ("{");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	*(unsigned short " UNALIGNED_ATTRIBUTE "*)p -= val;");
#else
		output_storage ("	void		*x;");
		output_storage ("	unsigned short	n;");

		output_storage ("	x = &n;");
		output_storage ("	optim_memcpy (x, p, 2);");
		output_storage ("	n -= val;");
		output_storage ("	optim_memcpy (p, x, 2);");
#endif
		output_storage ("}");
		return;

	case COB_SUB_S16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_s16 (void *p, const int val)");
		output_storage ("{");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	*(short " UNALIGNED_ATTRIBUTE "*)p -= val;");
#else
		output_storage ("	void	*x;");
		output_storage ("	short	n;");

		output_storage ("	x = &n;");
		output_storage ("	optim_memcpy (x, p, 2);");
		output_storage ("	n -= val;");
		output_storage ("	optim_memcpy (p, x, 2);");
#endif
		output_storage ("}");
		return;

	case COB_SUB_U24:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_u24 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned int	n = 0;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 1;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (x, p, 3);");
		output_storage ("	n -= val;");
		output_storage ("	optim_memcpy (p, x, 3);");
		output_storage ("}");
		return;

	case COB_SUB_S24:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_s24 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	int		n = 0;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = (unsigned char *)&n;");
#else
		output_storage ("	x = ((unsigned char *)&n) + 1;");
#endif
		output_storage ("	optim_memcpy (x, p, 3);");
		output_storage ("	n >>= 8;	/* Shift with sign */");
		output_storage ("	n -= val;");
#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 1;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (p, x, 3);");
		output_storage ("}");
		return;

	case COB_SUB_U32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_u32 (void *p, const int val)");
		output_storage ("{");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	*(unsigned int " UNALIGNED_ATTRIBUTE "*)p -= val;");
#else
		output_storage ("	void		*x;");
		output_storage ("	unsigned int	n;");

		output_storage ("	x = &n;");
		output_storage ("	optim_memcpy (x, p, 4);");
		output_storage ("	n -= val;");
		output_storage ("	optim_memcpy (p, x, 4);");
#endif
		output_storage ("}");
		return;

	case COB_SUB_S32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_s32 (void *p, const int val)");
		output_storage ("{");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	*(int " UNALIGNED_ATTRIBUTE "*)p -= val;");
#else
		output_storage ("	void	*x;");
		output_storage ("	int	n;");

		output_storage ("	x = &n;");
		output_storage ("	optim_memcpy (x, p, 4);");
		output_storage ("	n -= val;");
		output_storage ("	optim_memcpy (p, x, 4);");
#endif
		output_storage ("}");
		return;

	case COB_SUB_U40:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_u40 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 3;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (x, p, 5);");
		output_storage ("	n -= val;");
		output_storage ("	optim_memcpy (p, x, 5);");
		output_storage ("}");
		return;

	case COB_SUB_S40:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_s40 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = (unsigned char *)&n;");
#else
		output_storage ("	x = ((unsigned char *)&n) + 3;");
#endif
		output_storage ("	optim_memcpy (x, p, 5);");
		output_storage ("	n >>= 24;	/* Shift with sign */");
		output_storage ("	n -= val;");
#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 3;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (p, x, 5);");
		output_storage ("}");
		return;

	case COB_SUB_U48:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_u48 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 2;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (x, p, 6);");
		output_storage ("	n -= val;");
		output_storage ("	optim_memcpy (p, x, 6);");
		output_storage ("}");
		return;

	case COB_SUB_S48:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_s48 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = (unsigned char *)&n;");
#else
		output_storage ("	x = ((unsigned char *)&n) + 2;");
#endif
		output_storage ("	optim_memcpy (x, p, 6);");
		output_storage ("	n >>= 16;	/* Shift with sign */");
		output_storage ("	n -= val;");
#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 2;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (p, x, 6);");
		output_storage ("}");
		return;

	case COB_SUB_U56:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_u56 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 1;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (x, p, 7);");
		output_storage ("	n -= val;");
		output_storage ("	optim_memcpy (p, x, 7);");
		output_storage ("}");
		return;

	case COB_SUB_S56:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_s56 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = (unsigned char *)&n;");
#else
		output_storage ("	x = ((unsigned char *)&n) + 1;");
#endif
		output_storage ("	optim_memcpy (x, p, 7);");
		output_storage ("	n >>= 8;	/* Shift with sign */");
		output_storage ("	n -= val;");
#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 1;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (p, x, 7);");
		output_storage ("}");
		return;

	case COB_SUB_U64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_u64 (void *p, const int val)");
		output_storage ("{");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	*(cob_u64_t " UNALIGNED_ATTRIBUTE "*)p -= val;");
#else
		output_storage ("	void		*x;");
		output_storage ("	cob_u64_t	n;");

		output_storage ("	x = &n;");
		output_storage ("	optim_memcpy (x, p, 8);");
		output_storage ("	n -= val;");
		output_storage ("	optim_memcpy (p, x, 8);");
#endif
		output_storage ("}");
		return;

	case COB_SUB_S64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_s64 (void *p, const int val)");
		output_storage ("{");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	*(cob_s64_t " UNALIGNED_ATTRIBUTE "*)p -= val;");
#else
		output_storage ("	void		*x;");
		output_storage ("	cob_s64_t	n;");

		output_storage ("	x = &n;");
		output_storage ("	optim_memcpy (x, p, 8);");
		output_storage ("	n -= val;");
		output_storage ("	optim_memcpy (p, x, 8);");
#endif
		output_storage ("}");
		return;

	/* Binary swapped compare */

	case COB_CMPSWP_U16:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_u16 (const void *p, const cob_s64_t n)");
		output_storage ("{");
#ifndef COB_ALLOW_UNALIGNED
		output_storage ("	void		*x;");
#endif
		output_storage ("	unsigned short	val;");

		output_storage ("	if (n < 0) {");
		output_storage ("		return 1;");
		output_storage ("	}");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = COB_BSWAP_16 (*(unsigned short " UNALIGNED_ATTRIBUTE "*)p);");
#else
		output_storage ("	x = &val;");
		output_storage ("	optim_memcpy (x, p, 2);");
		output_storage ("	val = COB_BSWAP_16 (val);");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_S16:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_s16 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	short	val;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = COB_BSWAP_16 (*(short " UNALIGNED_ATTRIBUTE "*)p);");
#else
		output_storage ("	void	*x;");

		output_storage ("	x = &val;");
		output_storage ("	optim_memcpy (x, p, 2);");
		output_storage ("	val = COB_BSWAP_16 (val);");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_U24:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_u24 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned int	val = 0;");

		output_storage ("	if (n < 0) {");
		output_storage ("		return 1;");
		output_storage ("	}");
		output_storage ("	x = ((unsigned char *)&val) + 1;");
		output_storage ("	optim_memcpy (x, p, 3);");
		output_storage ("	val = COB_BSWAP_32 (val);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_S24:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_s24 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	int		val = 0;");

		output_storage ("	x = (unsigned char *)&val;");
		output_storage ("	optim_memcpy (x, p, 3);");
		output_storage ("	val = COB_BSWAP_32 (val);");
		output_storage ("	val >>= 8;	/* Shift with sign */");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_U32:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_u32 (const void *p, const cob_s64_t n)");
		output_storage ("{");
#ifndef COB_ALLOW_UNALIGNED
		output_storage ("	void		*x;");
#endif
		output_storage ("	unsigned int	val;");

		output_storage ("	if (n < 0) {");
		output_storage ("		return 1;");
		output_storage ("	}");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = COB_BSWAP_32 (*(const unsigned int " UNALIGNED_ATTRIBUTE "*)p);");
#else
		output_storage ("	x = &val;");
		output_storage ("	optim_memcpy (x, p, 4);");
		output_storage ("	val = COB_BSWAP_32 (val);");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_S32:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_s32 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	int	val;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = COB_BSWAP_32 (*(const int " UNALIGNED_ATTRIBUTE "*)p);");
#else
		output_storage ("	void	*x;");

		output_storage ("	x = &val;");
		output_storage ("	optim_memcpy (x, p, 4);");
		output_storage ("	val = COB_BSWAP_32 (val);");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_U40:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_u40 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_u64_t	val = 0;");
		output_storage ("	unsigned char	*x;");

		output_storage ("	if (n < 0) {");
		output_storage ("		return 1;");
		output_storage ("	}");
		output_storage ("	x = ((unsigned char *)&val) + 3;");
		output_storage ("	optim_memcpy (x, p, 5);");
		output_storage ("	val = COB_BSWAP_64 (val);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_S40:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_s40 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_s64_t	val = 0;");
		output_storage ("	unsigned char	*x;");

		output_storage ("	x = (unsigned char *)&val;");
		output_storage ("	optim_memcpy (x, p, 5);");
		output_storage ("	val = COB_BSWAP_64 (val);");
		output_storage ("	val >>= 24;	/* Shift with sign */");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_U48:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_u48 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_u64_t	val = 0;");
		output_storage ("	unsigned char	*x;");

		output_storage ("	if (n < 0) {");
		output_storage ("		return 1;");
		output_storage ("	}");
		output_storage ("	x = ((unsigned char *)&val) + 2;");
		output_storage ("	optim_memcpy (x, p, 6);");
		output_storage ("	val = COB_BSWAP_64 (val);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_S48:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_s48 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_s64_t	val = 0;");
		output_storage ("	unsigned char	*x;");

		output_storage ("	x = (unsigned char *)&val;");
		output_storage ("	optim_memcpy (x, p, 6);");
		output_storage ("	val = COB_BSWAP_64 (val);");
		output_storage ("	val >>= 16;	/* Shift with sign */");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_U56:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_u56 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_u64_t	val = 0;");
		output_storage ("	unsigned char	*x;");

		output_storage ("	if (n < 0) {");
		output_storage ("		return 1;");
		output_storage ("	}");
		output_storage ("	x = ((unsigned char *)&val) + 1;");
		output_storage ("	optim_memcpy (x, p, 7);");
		output_storage ("	val = COB_BSWAP_64 (val);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_S56:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_s56 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_s64_t	val = 0;");
		output_storage ("	unsigned char	*x;");

		output_storage ("	x = (unsigned char *)&val;");
		output_storage ("	optim_memcpy (x, p, 7);");
		output_storage ("	val = COB_BSWAP_64 (val);");
		output_storage ("	val >>= 8;	/* Shift with sign */");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_U64:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_u64 (const void *p, const cob_s64_t n)");
		output_storage ("{");
#ifndef COB_ALLOW_UNALIGNED
		output_storage ("	void		*x;");
#endif
		output_storage ("	cob_u64_t	val;");

		output_storage ("	if (n < 0) {");
		output_storage ("		return 1;");
		output_storage ("	}");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = COB_BSWAP_64 (*(const cob_u64_t " UNALIGNED_ATTRIBUTE "*)p);");
#else
		output_storage ("	x = &val;");
		output_storage ("	optim_memcpy (x, p, 8);");
		output_storage ("	val = COB_BSWAP_64 (val);");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_S64:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_s64 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_s64_t	val;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = COB_BSWAP_64 (*(const cob_s64_t " UNALIGNED_ATTRIBUTE "*)p);");
#else
		output_storage ("	void		*x;");
		output_storage ("	x = &val;");
		output_storage ("	optim_memcpy (x, p, 8);");
		output_storage ("	val = COB_BSWAP_64 (val);");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	/* Binary swapped add */

	case COB_ADDSWP_U16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_u16 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned short	n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = COB_BSWAP_16 (*(unsigned short " UNALIGNED_ATTRIBUTE "*)p);");
		output_storage ("	n += val;");
		output_storage ("	*(unsigned short " UNALIGNED_ATTRIBUTE "*)p = COB_BSWAP_16(n);");
#else
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[1];");
		output_storage ("	x[1] = px[0];");
		output_storage ("	n += val;");
		output_storage ("	px[0] = x[1];");
		output_storage ("	px[1] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_ADDSWP_S16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_s16 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	short		n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = COB_BSWAP_16 (*(short " UNALIGNED_ATTRIBUTE "*)p);");
		output_storage ("	n += val;");
		output_storage ("	*(short " UNALIGNED_ATTRIBUTE "*)p = COB_BSWAP_16(n);");
#else
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[1];");
		output_storage ("	x[1] = px[0];");
		output_storage ("	n += val;");
		output_storage ("	px[0] = x[1];");
		output_storage ("	px[1] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_ADDSWP_U24:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_u24 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");
		output_storage ("	unsigned int	n = 0;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[2];");
		output_storage ("	x[1] = px[1];");
		output_storage ("	x[2] = px[0];");
		output_storage ("	n += val;");
		output_storage ("	px[0] = x[2];");
		output_storage ("	px[1] = x[1];");
		output_storage ("	px[2] = x[0];");
		output_storage ("}");
		return;

	case COB_ADDSWP_S24:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_s24 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");
		output_storage ("	int		n = 0;");

		output_storage ("	x = ((unsigned char *)&n) + 1;");
		output_storage ("	x[0] = px[2];");
		output_storage ("	x[1] = px[1];");
		output_storage ("	x[2] = px[0];");
		output_storage ("	n >>= 8;	/* Shift with sign */");
		output_storage ("	n += val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[2];");
		output_storage ("	px[1] = x[1];");
		output_storage ("	px[2] = x[0];");
		output_storage ("}");
		return;

	case COB_ADDSWP_U32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_u32 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned int	n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = COB_BSWAP_32 (*(unsigned int " UNALIGNED_ATTRIBUTE "*)p);");
		output_storage ("	n += val;");
		output_storage ("	*(unsigned int " UNALIGNED_ATTRIBUTE "*)p = COB_BSWAP_32(n);");
#else
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[3];");
		output_storage ("	x[1] = px[2];");
		output_storage ("	x[2] = px[1];");
		output_storage ("	x[3] = px[0];");
		output_storage ("	n += val;");
		output_storage ("	px[0] = x[3];");
		output_storage ("	px[1] = x[2];");
		output_storage ("	px[2] = x[1];");
		output_storage ("	px[3] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_ADDSWP_S32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_s32 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	int		n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = COB_BSWAP_32 (*(int " UNALIGNED_ATTRIBUTE "*)p);");
		output_storage ("	n += val;");
		output_storage ("	*(int " UNALIGNED_ATTRIBUTE "*)p = COB_BSWAP_32(n);");
#else
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[3];");
		output_storage ("	x[1] = px[2];");
		output_storage ("	x[2] = px[1];");
		output_storage ("	x[3] = px[0];");
		output_storage ("	n += val;");
		output_storage ("	px[0] = x[3];");
		output_storage ("	px[1] = x[2];");
		output_storage ("	px[2] = x[1];");
		output_storage ("	px[3] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_ADDSWP_U40:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_u40 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n = 0;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[4];");
		output_storage ("	x[1] = px[3];");
		output_storage ("	x[2] = px[2];");
		output_storage ("	x[3] = px[1];");
		output_storage ("	x[4] = px[0];");
		output_storage ("	n += val;");
		output_storage ("	px[0] = x[4];");
		output_storage ("	px[1] = x[3];");
		output_storage ("	px[2] = x[2];");
		output_storage ("	px[3] = x[1];");
		output_storage ("	px[4] = x[0];");
		output_storage ("}");
		return;

	case COB_ADDSWP_S40:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_s40 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n = 0;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = ((unsigned char *)&n) + 3;");
		output_storage ("	x[0] = px[4];");
		output_storage ("	x[1] = px[3];");
		output_storage ("	x[2] = px[2];");
		output_storage ("	x[3] = px[1];");
		output_storage ("	x[4] = px[0];");
		output_storage ("	n >>= 24;	/* Shift with sign */");
		output_storage ("	n += val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[4];");
		output_storage ("	px[1] = x[3];");
		output_storage ("	px[2] = x[2];");
		output_storage ("	px[3] = x[1];");
		output_storage ("	px[4] = x[0];");
		output_storage ("}");
		return;

	case COB_ADDSWP_U48:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_u48 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n = 0;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[5];");
		output_storage ("	x[1] = px[4];");
		output_storage ("	x[2] = px[3];");
		output_storage ("	x[3] = px[2];");
		output_storage ("	x[4] = px[1];");
		output_storage ("	x[5] = px[0];");
		output_storage ("	n += val;");
		output_storage ("	px[0] = x[5];");
		output_storage ("	px[1] = x[4];");
		output_storage ("	px[2] = x[3];");
		output_storage ("	px[3] = x[2];");
		output_storage ("	px[4] = x[1];");
		output_storage ("	px[5] = x[0];");
		output_storage ("}");
		return;

	case COB_ADDSWP_S48:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_s48 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n = 0;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = ((unsigned char *)&n) + 2;");
		output_storage ("	x[0] = px[5];");
		output_storage ("	x[1] = px[4];");
		output_storage ("	x[2] = px[3];");
		output_storage ("	x[3] = px[2];");
		output_storage ("	x[4] = px[1];");
		output_storage ("	x[5] = px[0];");
		output_storage ("	n >>= 16;	/* Shift with sign */");
		output_storage ("	n += val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[5];");
		output_storage ("	px[1] = x[4];");
		output_storage ("	px[2] = x[3];");
		output_storage ("	px[3] = x[2];");
		output_storage ("	px[4] = x[1];");
		output_storage ("	px[5] = x[0];");
		output_storage ("}");
		return;

	case COB_ADDSWP_U56:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_u56 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n = 0;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[6];");
		output_storage ("	x[1] = px[5];");
		output_storage ("	x[2] = px[4];");
		output_storage ("	x[3] = px[3];");
		output_storage ("	x[4] = px[2];");
		output_storage ("	x[5] = px[1];");
		output_storage ("	x[6] = px[0];");
		output_storage ("	n += val;");
		output_storage ("	px[0] = x[6];");
		output_storage ("	px[1] = x[5];");
		output_storage ("	px[2] = x[4];");
		output_storage ("	px[3] = x[3];");
		output_storage ("	px[4] = x[2];");
		output_storage ("	px[5] = x[1];");
		output_storage ("	px[6] = x[0];");
		output_storage ("}");
		return;

	case COB_ADDSWP_S56:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_s56 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n = 0;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = ((unsigned char *)&n) + 1;");
		output_storage ("	x[0] = px[6];");
		output_storage ("	x[1] = px[5];");
		output_storage ("	x[2] = px[4];");
		output_storage ("	x[3] = px[3];");
		output_storage ("	x[4] = px[2];");
		output_storage ("	x[5] = px[1];");
		output_storage ("	x[6] = px[0];");
		output_storage ("	n >>= 8;	/* Shift with sign */");
		output_storage ("	n += val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[6];");
		output_storage ("	px[1] = x[5];");
		output_storage ("	px[2] = x[4];");
		output_storage ("	px[3] = x[3];");
		output_storage ("	px[4] = x[2];");
		output_storage ("	px[5] = x[1];");
		output_storage ("	px[6] = x[0];");
		output_storage ("}");
		return;

	case COB_ADDSWP_U64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_u64 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = COB_BSWAP_64 (*(cob_u64_t " UNALIGNED_ATTRIBUTE "*)p);");
		output_storage ("	n += val;");
		output_storage ("	*(cob_u64_t " UNALIGNED_ATTRIBUTE "*)p = COB_BSWAP_64(n);");
#else
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[7];");
		output_storage ("	x[1] = px[6];");
		output_storage ("	x[2] = px[5];");
		output_storage ("	x[3] = px[4];");
		output_storage ("	x[4] = px[3];");
		output_storage ("	x[5] = px[2];");
		output_storage ("	x[6] = px[1];");
		output_storage ("	x[7] = px[0];");
		output_storage ("	n += val;");
		output_storage ("	px[0] = x[7];");
		output_storage ("	px[1] = x[6];");
		output_storage ("	px[2] = x[5];");
		output_storage ("	px[3] = x[4];");
		output_storage ("	px[4] = x[3];");
		output_storage ("	px[5] = x[2];");
		output_storage ("	px[6] = x[1];");
		output_storage ("	px[7] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_ADDSWP_S64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_s64 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = COB_BSWAP_64 (*(cob_s64_t " UNALIGNED_ATTRIBUTE "*)p);");
		output_storage ("	n += val;");
		output_storage ("	*(cob_s64_t " UNALIGNED_ATTRIBUTE "*)p = COB_BSWAP_64(n);");
#else
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[7];");
		output_storage ("	x[1] = px[6];");
		output_storage ("	x[2] = px[5];");
		output_storage ("	x[3] = px[4];");
		output_storage ("	x[4] = px[3];");
		output_storage ("	x[5] = px[2];");
		output_storage ("	x[6] = px[1];");
		output_storage ("	x[7] = px[0];");
		output_storage ("	n += val;");
		output_storage ("	px[0] = x[7];");
		output_storage ("	px[1] = x[6];");
		output_storage ("	px[2] = x[5];");
		output_storage ("	px[3] = x[4];");
		output_storage ("	px[4] = x[3];");
		output_storage ("	px[5] = x[2];");
		output_storage ("	px[6] = x[1];");
		output_storage ("	px[7] = x[0];");
#endif
		output_storage ("}");
		return;

	/* Binary swapped subtract */

	case COB_SUBSWP_U16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_u16 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned short	n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = COB_BSWAP_16 (*(unsigned short " UNALIGNED_ATTRIBUTE "*)p);");
		output_storage ("	n -= val;");
		output_storage ("	*(unsigned short " UNALIGNED_ATTRIBUTE "*)p = COB_BSWAP_16(n);");
#else
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[1];");
		output_storage ("	x[1] = px[0];");
		output_storage ("	n -= val;");
		output_storage ("	px[0] = x[1];");
		output_storage ("	px[1] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_SUBSWP_S16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_s16 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	short		n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = COB_BSWAP_16 (*(short " UNALIGNED_ATTRIBUTE "*)p);");
		output_storage ("	n -= val;");
		output_storage ("	*(short " UNALIGNED_ATTRIBUTE "*)p = COB_BSWAP_16(n);");
#else
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[1];");
		output_storage ("	x[1] = px[0];");
		output_storage ("	n -= val;");
		output_storage ("	px[0] = x[1];");
		output_storage ("	px[1] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_SUBSWP_U24:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_u24 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");
		output_storage ("	unsigned int	n = 0;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[2];");
		output_storage ("	x[1] = px[1];");
		output_storage ("	x[2] = px[0];");
		output_storage ("	n -= val;");
		output_storage ("	px[0] = x[2];");
		output_storage ("	px[1] = x[1];");
		output_storage ("	px[2] = x[0];");
		output_storage ("}");
		return;

	case COB_SUBSWP_S24:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_s24 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");
		output_storage ("	int		n = 0;");

		output_storage ("	x = ((unsigned char *)&n) + 1;");
		output_storage ("	x[0] = px[2];");
		output_storage ("	x[1] = px[1];");
		output_storage ("	x[2] = px[0];");
		output_storage ("	n >>= 8;	/* Shift with sign */");
		output_storage ("	n -= val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[2];");
		output_storage ("	px[1] = x[1];");
		output_storage ("	px[2] = x[0];");
		output_storage ("}");
		return;

	case COB_SUBSWP_U32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_u32 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned int	n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = COB_BSWAP_32 (*(unsigned int " UNALIGNED_ATTRIBUTE "*)p);");
		output_storage ("	n -= val;");
		output_storage ("	*(unsigned int " UNALIGNED_ATTRIBUTE "*)p = COB_BSWAP_32(n);");
#else
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[3];");
		output_storage ("	x[1] = px[2];");
		output_storage ("	x[2] = px[1];");
		output_storage ("	x[3] = px[0];");
		output_storage ("	n -= val;");
		output_storage ("	px[0] = x[3];");
		output_storage ("	px[1] = x[2];");
		output_storage ("	px[2] = x[1];");
		output_storage ("	px[3] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_SUBSWP_S32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_s32 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	int		n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = COB_BSWAP_32 (*(int " UNALIGNED_ATTRIBUTE "*)p);");
		output_storage ("	n -= val;");
		output_storage ("	*(int " UNALIGNED_ATTRIBUTE "*)p = COB_BSWAP_32(n);");
#else
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[3];");
		output_storage ("	x[1] = px[2];");
		output_storage ("	x[2] = px[1];");
		output_storage ("	x[3] = px[0];");
		output_storage ("	n -= val;");
		output_storage ("	px[0] = x[3];");
		output_storage ("	px[1] = x[2];");
		output_storage ("	px[2] = x[1];");
		output_storage ("	px[3] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_SUBSWP_U40:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_u40 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n = 0;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[4];");
		output_storage ("	x[1] = px[3];");
		output_storage ("	x[2] = px[2];");
		output_storage ("	x[3] = px[1];");
		output_storage ("	x[4] = px[0];");
		output_storage ("	n -= val;");
		output_storage ("	px[0] = x[4];");
		output_storage ("	px[1] = x[3];");
		output_storage ("	px[2] = x[2];");
		output_storage ("	px[3] = x[1];");
		output_storage ("	px[4] = x[0];");
		output_storage ("}");
		return;

	case COB_SUBSWP_S40:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_s40 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n = 0;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = ((unsigned char *)&n) + 3;");
		output_storage ("	x[0] = px[4];");
		output_storage ("	x[1] = px[3];");
		output_storage ("	x[2] = px[2];");
		output_storage ("	x[3] = px[1];");
		output_storage ("	x[4] = px[0];");
		output_storage ("	n >>= 24;	/* Shift with sign */");
		output_storage ("	n -= val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[4];");
		output_storage ("	px[1] = x[3];");
		output_storage ("	px[2] = x[2];");
		output_storage ("	px[3] = x[1];");
		output_storage ("	px[4] = x[0];");
		output_storage ("}");
		return;

	case COB_SUBSWP_U48:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_u48 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n = 0;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[5];");
		output_storage ("	x[1] = px[4];");
		output_storage ("	x[2] = px[3];");
		output_storage ("	x[3] = px[2];");
		output_storage ("	x[4] = px[1];");
		output_storage ("	x[5] = px[0];");
		output_storage ("	n -= val;");
		output_storage ("	px[0] = x[5];");
		output_storage ("	px[1] = x[4];");
		output_storage ("	px[2] = x[3];");
		output_storage ("	px[3] = x[2];");
		output_storage ("	px[4] = x[1];");
		output_storage ("	px[5] = x[0];");
		output_storage ("}");
		return;

	case COB_SUBSWP_S48:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_s48 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n = 0;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = ((unsigned char *)&n) + 2;");
		output_storage ("	x[0] = px[5];");
		output_storage ("	x[1] = px[4];");
		output_storage ("	x[2] = px[3];");
		output_storage ("	x[3] = px[2];");
		output_storage ("	x[4] = px[1];");
		output_storage ("	x[5] = px[0];");
		output_storage ("	n >>= 16;	/* Shift with sign */");
		output_storage ("	n -= val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[5];");
		output_storage ("	px[1] = x[4];");
		output_storage ("	px[2] = x[3];");
		output_storage ("	px[3] = x[2];");
		output_storage ("	px[4] = x[1];");
		output_storage ("	px[5] = x[0];");
		output_storage ("}");
		return;

	case COB_SUBSWP_U56:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_u56 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n = 0;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[6];");
		output_storage ("	x[1] = px[5];");
		output_storage ("	x[2] = px[4];");
		output_storage ("	x[3] = px[3];");
		output_storage ("	x[4] = px[2];");
		output_storage ("	x[5] = px[1];");
		output_storage ("	x[6] = px[0];");
		output_storage ("	n -= val;");
		output_storage ("	px[0] = x[6];");
		output_storage ("	px[1] = x[5];");
		output_storage ("	px[2] = x[4];");
		output_storage ("	px[3] = x[3];");
		output_storage ("	px[4] = x[2];");
		output_storage ("	px[5] = x[1];");
		output_storage ("	px[6] = x[0];");
		output_storage ("}");
		return;

	case COB_SUBSWP_S56:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_s56 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n = 0;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = ((unsigned char *)&n) + 1;");
		output_storage ("	x[0] = px[6];");
		output_storage ("	x[1] = px[5];");
		output_storage ("	x[2] = px[4];");
		output_storage ("	x[3] = px[3];");
		output_storage ("	x[4] = px[2];");
		output_storage ("	x[5] = px[1];");
		output_storage ("	x[6] = px[0];");
		output_storage ("	n >>= 8;	/* Shift with sign */");
		output_storage ("	n -= val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[6];");
		output_storage ("	px[1] = x[5];");
		output_storage ("	px[2] = x[4];");
		output_storage ("	px[3] = x[3];");
		output_storage ("	px[4] = x[2];");
		output_storage ("	px[5] = x[1];");
		output_storage ("	px[6] = x[0];");
		output_storage ("}");
		return;

	case COB_SUBSWP_U64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_u64 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t	n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = COB_BSWAP_64 (*(cob_u64_t " UNALIGNED_ATTRIBUTE "*)p);");
		output_storage ("	n -= val;");
		output_storage ("	*(cob_u64_t " UNALIGNED_ATTRIBUTE "*)p = COB_BSWAP_64(n);");
#else
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[7];");
		output_storage ("	x[1] = px[6];");
		output_storage ("	x[2] = px[5];");
		output_storage ("	x[3] = px[4];");
		output_storage ("	x[4] = px[3];");
		output_storage ("	x[5] = px[2];");
		output_storage ("	x[6] = px[1];");
		output_storage ("	x[7] = px[0];");
		output_storage ("	n -= val;");
		output_storage ("	px[0] = x[7];");
		output_storage ("	px[1] = x[6];");
		output_storage ("	px[2] = x[5];");
		output_storage ("	px[3] = x[4];");
		output_storage ("	px[4] = x[3];");
		output_storage ("	px[5] = x[2];");
		output_storage ("	px[6] = x[1];");
		output_storage ("	px[7] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_SUBSWP_S64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_s64 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = COB_BSWAP_64 (*(cob_s64_t " UNALIGNED_ATTRIBUTE "*)p);");
		output_storage ("	n -= val;");
		output_storage ("	*(cob_s64_t " UNALIGNED_ATTRIBUTE "*)p = COB_BSWAP_64(n);");
#else
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[7];");
		output_storage ("	x[1] = px[6];");
		output_storage ("	x[2] = px[5];");
		output_storage ("	x[3] = px[4];");
		output_storage ("	x[4] = px[3];");
		output_storage ("	x[5] = px[2];");
		output_storage ("	x[6] = px[1];");
		output_storage ("	x[7] = px[0];");
		output_storage ("	n -= val;");
		output_storage ("	px[0] = x[7];");
		output_storage ("	px[1] = x[6];");
		output_storage ("	px[2] = x[5];");
		output_storage ("	px[3] = x[4];");
		output_storage ("	px[4] = x[3];");
		output_storage ("	px[5] = x[2];");
		output_storage ("	px[6] = x[1];");
		output_storage ("	px[7] = x[0];");
#endif
		output_storage ("}");
		return;

	/* Binary set swapped value */
	case COB_SETSWP_U16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_u16 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned short	n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = val;");
		output_storage ("	*(unsigned short " UNALIGNED_ATTRIBUTE "*)p = COB_BSWAP_16(n);");
#else
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[1];");
		output_storage ("	px[1] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_SETSWP_S16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_s16 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	short		n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = val;");
		output_storage ("	*(short " UNALIGNED_ATTRIBUTE "*)p = COB_BSWAP_16(n);");
#else
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[1];");
		output_storage ("	px[1] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_SETSWP_U24:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_u24 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");
		output_storage ("	unsigned int	n;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[2];");
		output_storage ("	px[1] = x[1];");
		output_storage ("	px[2] = x[0];");
		output_storage ("}");
		return;

	case COB_SETSWP_S24:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_s24 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");
		output_storage ("	int		n;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[2];");
		output_storage ("	px[1] = x[1];");
		output_storage ("	px[2] = x[0];");
		output_storage ("}");
		return;

	case COB_SETSWP_U32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_u32 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned int	n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = val;");
		output_storage ("	*(unsigned int " UNALIGNED_ATTRIBUTE "*)p = COB_BSWAP_32(n);");
#else
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[3];");
		output_storage ("	px[1] = x[2];");
		output_storage ("	px[2] = x[1];");
		output_storage ("	px[3] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_SETSWP_S32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_s32 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	int		n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = val;");
		output_storage ("	*(int " UNALIGNED_ATTRIBUTE "*)p = COB_BSWAP_32(n);");
#else
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[3];");
		output_storage ("	px[1] = x[2];");
		output_storage ("	px[2] = x[1];");
		output_storage ("	px[3] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_SETSWP_U40:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_u40 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[4];");
		output_storage ("	px[1] = x[3];");
		output_storage ("	px[2] = x[2];");
		output_storage ("	px[3] = x[1];");
		output_storage ("	px[4] = x[0];");
		output_storage ("}");
		return;

	case COB_SETSWP_S40:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_s40 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[4];");
		output_storage ("	px[1] = x[3];");
		output_storage ("	px[2] = x[2];");
		output_storage ("	px[3] = x[1];");
		output_storage ("	px[4] = x[0];");
		output_storage ("}");
		return;

	case COB_SETSWP_U48:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_u48 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[5];");
		output_storage ("	px[1] = x[4];");
		output_storage ("	px[2] = x[3];");
		output_storage ("	px[3] = x[2];");
		output_storage ("	px[4] = x[1];");
		output_storage ("	px[5] = x[0];");
		output_storage ("}");
		return;

	case COB_SETSWP_S48:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_s48 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[5];");
		output_storage ("	px[1] = x[4];");
		output_storage ("	px[2] = x[3];");
		output_storage ("	px[3] = x[2];");
		output_storage ("	px[4] = x[1];");
		output_storage ("	px[5] = x[0];");
		output_storage ("}");
		return;

	case COB_SETSWP_U56:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_u56 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[6];");
		output_storage ("	px[1] = x[5];");
		output_storage ("	px[2] = x[4];");
		output_storage ("	px[3] = x[3];");
		output_storage ("	px[4] = x[2];");
		output_storage ("	px[5] = x[1];");
		output_storage ("	px[6] = x[0];");
		output_storage ("}");
		return;

	case COB_SETSWP_S56:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_s56 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[6];");
		output_storage ("	px[1] = x[5];");
		output_storage ("	px[2] = x[4];");
		output_storage ("	px[3] = x[3];");
		output_storage ("	px[4] = x[2];");
		output_storage ("	px[5] = x[1];");
		output_storage ("	px[6] = x[0];");
		output_storage ("}");
		return;

	case COB_SETSWP_U64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_u64 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = val;");
		output_storage ("	*(cob_u64_t " UNALIGNED_ATTRIBUTE "*)p = COB_BSWAP_64(n);");
#else
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[7];");
		output_storage ("	px[1] = x[6];");
		output_storage ("	px[2] = x[5];");
		output_storage ("	px[3] = x[4];");
		output_storage ("	px[4] = x[3];");
		output_storage ("	px[5] = x[2];");
		output_storage ("	px[6] = x[1];");
		output_storage ("	px[7] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_SETSWP_S64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_s64 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = val;");
		output_storage ("	*(cob_s64_t " UNALIGNED_ATTRIBUTE "*)p = COB_BSWAP_64(n);");
#else
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[7];");
		output_storage ("	px[1] = x[6];");
		output_storage ("	px[2] = x[5];");
		output_storage ("	px[3] = x[4];");
		output_storage ("	px[4] = x[3];");
		output_storage ("	px[5] = x[2];");
		output_storage ("	px[6] = x[1];");
		output_storage ("	px[7] = x[0];");
#endif
		output_storage ("}");
		return;
	/* LCOV_EXCL_START */
	default:
		cobc_err_msg (_("unexpected optimization value: %d"), val);
		COBC_ABORT ();
	/* LCOV_EXCL_STOP */
	}
}

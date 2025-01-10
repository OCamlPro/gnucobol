/*
   Copyright (C) 2001-2023 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Simon Sobisch, Ron Norman,
   Edward Hart

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
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <limits.h>
#ifdef	HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef	_WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

#ifdef	HAVE_LOCALE_H
#include <locale.h>
#endif

#include "cobc.h"
#include "tree.h"

struct system_table {
	const char		*const syst_name;
	const unsigned int	syst_params_min;
	const unsigned int	syst_params_max;
};

struct optim_table {
	const char		*const optim_name;
	const enum cb_optim	optim_val;
};

struct expr_node {
	/* The token of this node.
	 *  'x'                          - values (cb_tree)
	 *  '+', '-', '*', '/', '^'      - arithmetic operators
	 *  '=', '~', '<', '>', '[', ']' - relational operators
	 *  '!', '&', '|'                - logical operators
	 *  'n', 'a', 'o', 'e',          - bitwise operators
	 *  'l', 'r', 'c', 'd'           - bitshift operators
	 *  '(', ')'                     - parentheses
	 */
	int		token;
	/* The value itself if this node is a value */
	cb_tree		value;
};

#define START_STACK_SIZE	32
#define TOKEN(offset)		(expr_stack[expr_index + offset].token)
#define VALUE(offset)		(expr_stack[expr_index + offset].value)
/* Expressions are stored in the stack, starting at position 3,
   because we keep 3 zeroes above the top of the stack: the algorithm
   uses the 3 preceeding entries before the current expr_index to
   compare the priority of the current operator with the operator of
   the expression on the left, so these 3 initial entries must be zero
   (and zero has the lowest priority, so it forces reduction of all
   other operators on its right).
*/
#define TOPSTACK                3
#define FMT_LEN cb_pretty_display ? "%d" : "%010d"

#define dpush(x)		CB_ADD_TO_CHAIN (x, decimal_stack)

/* Global variables */

cb_tree				cb_debug_item;
cb_tree				cb_debug_line;
cb_tree				cb_debug_name;
cb_tree				cb_debug_sub_1;
cb_tree				cb_debug_sub_2;
cb_tree				cb_debug_sub_3;
cb_tree				cb_debug_contents;

size_t				suppress_warn = 0;

/* Local variables */

static cb_tree			decimal_stack = NULL;

static const char		*inspect_func;
static cb_tree			inspect_data;
struct cb_statement		*error_statement = NULL;

#ifndef WITH_XML2
static int			warn_xml_done = 0;
#endif
#if   !defined (WITH_CJSON) && !defined (WITH_JSON_C)
static int			warn_json_done = 0;
#endif
#ifndef WITH_EXTENDED_SCREENIO
static int			warn_screen_done = 0;
#endif

struct external_defined_register {
	struct external_defined_register	*next;
	const char		*name;
	const char		*definition;
};

static struct external_defined_register	*external_defined_fields_ws;
static struct external_defined_register	*external_defined_fields_global;

static int			report_id = 1;

static int			expr_op;		/* Last operator */
static cb_tree		expr_lh;		/* Last left hand */
static int			expr_dmax = -1;		/* Max scale for expression result */
#define MAX_NESTED_EXPR	64
static cb_tree		expr_x = NULL;
static cb_tree		sz_shift;
static int			expr_dec_align = -1;
static int			expr_nest = 0;
static int			expr_decp[MAX_NESTED_EXPR];
static int			cond_fixed = -1;	/* 0 means TRUE, 1 means FALSE, -1 unknown */
#define MAX_NESTED_COND	128
static int			if_nest = 0;
static int			if_cond[MAX_NESTED_COND];
static int			if_stop = 0;
static int			expr_line = 0;		/* Line holding expression for warnings */
static int			int_usage = -1;
static cb_tree		expr_rslt = NULL;	/* Expression result */

static size_t		initialized = 0;
static size_t		overlapping = 0;

static int			expr_index;		/* Stack index */
static int			expr_stack_size;	/* Stack max size */
static struct expr_node		*expr_stack;		/* Expression node stack */

#ifdef	HAVE_DESIGNATED_INITS
static const unsigned char	expr_prio[256] = {
	['x'] = 0,
	['^'] = 1,
	['*'] = 2,
	['/'] = 2,
	['+'] = 3,
	['-'] = 3,
	['a'] = 4,	/* B-AND */
	['n'] = 5,	/* B-NOT */
	['o'] = 4,	/* B-OR */
	['e'] = 4,	/* B-XOR */
	['l'] = 4,	/* B-LEFT */
	['r'] = 4,	/* B-RIGHT */
	['c'] = 4,	/* B-SHIFT-LC */
	['d'] = 4,	/* B-SHIFT-RC */
	['='] = 4,
	['~'] = 4,
	['<'] = 4,
	['>'] = 4,
	['['] = 4,
	[']'] = 4,
	['!'] = 5,
	['&'] = 6,
	['|'] = 7,
	[')'] = 8,
	['('] = 9,
	[0] = 10
};
#else
static unsigned char		expr_prio[256];
#endif

#ifdef	COB_EBCDIC_MACHINE
/* EBCDIC referring to ASCII */
static const unsigned char	cob_refer_ascii[256] = {
	0x00, 0x01, 0x02, 0x03, 0x37, 0x2D, 0x2E, 0x2F,
	0x16, 0x05, 0x25, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,
	0x10, 0x11, 0x12, 0x13, 0x3C, 0x3D, 0x32, 0x26,
	0x18, 0x19, 0x3F, 0x27, 0x1C, 0x1D, 0x1E, 0x1F,
	0x40, 0x5A, 0x7F, 0x7B, 0x5B, 0x6C, 0x50, 0x7D,
	0x4D, 0x5D, 0x5C, 0x4E, 0x6B, 0x60, 0x4B, 0x61,
	0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7,
	0xF8, 0xF9, 0x7A, 0x5E, 0x4C, 0x7E, 0x6E, 0x6F,
	0x7C, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7,
	0xC8, 0xC9, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6,
	0xD7, 0xD8, 0xD9, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6,
	0xE7, 0xE8, 0xE9, 0xAD, 0xE0, 0xBD, 0x5F, 0x6D,
	0x79, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
	0x88, 0x89, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96,
	0x97, 0x98, 0x99, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6,
	0xA7, 0xA8, 0xA9, 0xC0, 0x6A, 0xD0, 0xA1, 0x07,
	0x68, 0xDC, 0x51, 0x42, 0x43, 0x44, 0x47, 0x48,
	0x52, 0x53, 0x54, 0x57, 0x56, 0x58, 0x63, 0x67,
	0x71, 0x9C, 0x9E, 0xCB, 0xCC, 0xCD, 0xDB, 0xDD,
	0xDF, 0xEC, 0xFC, 0xB0, 0xB1, 0xB2, 0x3E, 0xB4,
	0x45, 0x55, 0xCE, 0xDE, 0x49, 0x69, 0x9A, 0x9B,
	0xAB, 0x9F, 0xBA, 0xB8, 0xB7, 0xAA, 0x8A, 0x8B,
	0xB6, 0xB5, 0x62, 0x4F, 0x64, 0x65, 0x66, 0x20,
	0x21, 0x22, 0x70, 0x23, 0x72, 0x73, 0x74, 0xBE,
	0x76, 0x77, 0x78, 0x80, 0x24, 0x15, 0x8C, 0x8D,
	0x8E, 0x41, 0x06, 0x17, 0x28, 0x29, 0x9D, 0x2A,
	0x2B, 0x2C, 0x09, 0x0A, 0xAC, 0x4A, 0xAE, 0xAF,
	0x1B, 0x30, 0x31, 0xFA, 0x1A, 0x33, 0x34, 0x35,
	0x36, 0x59, 0x08, 0x38, 0xBC, 0x39, 0xA0, 0xBF,
	0xCA, 0x3A, 0xFE, 0x3B, 0x04, 0xCF, 0xDA, 0x14,
	0xE1, 0x8F, 0x46, 0x75, 0xFD, 0xEB, 0xEE, 0xED,
	0x90, 0xEF, 0xB3, 0xFB, 0xB9, 0xEA, 0xBB, 0xFF
};
#else
/* ASCII referring to EBCDIC */
static const unsigned char	cob_refer_ebcdic[256] = {
	0x00, 0x01, 0x02, 0x03, 0xEC, 0x09, 0xCA, 0x7F,
	0xE2, 0xD2, 0xD3, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,
	0x10, 0x11, 0x12, 0x13, 0xEF, 0xC5, 0x08, 0xCB,
	0x18, 0x19, 0xDC, 0xD8, 0x1C, 0x1D, 0x1E, 0x1F,
	0xB7, 0xB8, 0xB9, 0xBB, 0xC4, 0x0A, 0x17, 0x1B,
	0xCC, 0xCD, 0xCF, 0xD0, 0xD1, 0x05, 0x06, 0x07,
	0xD9, 0xDA, 0x16, 0xDD, 0xDE, 0xDF, 0xE0, 0x04,
	0xE3, 0xE5, 0xE9, 0xEB, 0x14, 0x15, 0x9E, 0x1A,
	0x20, 0xC9, 0x83, 0x84, 0x85, 0xA0, 0xF2, 0x86,
	0x87, 0xA4, 0xD5, 0x2E, 0x3C, 0x28, 0x2B, 0xB3,
	0x26, 0x82, 0x88, 0x89, 0x8A, 0xA1, 0x8C, 0x8B,
	0x8D, 0xE1, 0x21, 0x24, 0x2A, 0x29, 0x3B, 0x5E,
	0x2D, 0x2F, 0xB2, 0x8E, 0xB4, 0xB5, 0xB6, 0x8F,
	0x80, 0xA5, 0x7C, 0x2C, 0x25, 0x5F, 0x3E, 0x3F,
	0xBA, 0x90, 0xBC, 0xBD, 0xBE, 0xF3, 0xC0, 0xC1,
	0xC2, 0x60, 0x3A, 0x23, 0x40, 0x27, 0x3D, 0x22,
	0xC3, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
	0x68, 0x69, 0xAE, 0xAF, 0xC6, 0xC7, 0xC8, 0xF1,
	0xF8, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F, 0x70,
	0x71, 0x72, 0xA6, 0xA7, 0x91, 0xCE, 0x92, 0xA9,
	0xE6, 0x7E, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78,
	0x79, 0x7A, 0xAD, 0xA8, 0xD4, 0x5B, 0xD6, 0xD7,
	0x9B, 0x9C, 0x9D, 0xFA, 0x9F, 0xB1, 0xB0, 0xAC,
	0xAB, 0xFC, 0xAA, 0xFE, 0xE4, 0x5D, 0xBF, 0xE7,
	0x7B, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
	0x48, 0x49, 0xE8, 0x93, 0x94, 0x95, 0xA2, 0xED,
	0x7D, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F, 0x50,
	0x51, 0x52, 0xEE, 0x96, 0x81, 0x97, 0xA3, 0x98,
	0x5C, 0xF0, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58,
	0x59, 0x5A, 0xFD, 0xF5, 0x99, 0xF7, 0xF6, 0xF9,
	0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
	0x38, 0x39, 0xDB, 0xFB, 0x9A, 0xF4, 0xEA, 0xFF
};
#endif

/* System routines */

#undef	COB_SYSTEM_GEN
#define	COB_SYSTEM_GEN(cob_name, pmin, pmax, c_name)	{ cob_name, pmin, pmax },

static const struct system_table	system_tab[] = {
#include "../libcob/system.def"
	{ NULL, 0, 0 }
};

#undef	COB_SYSTEM_GEN

static const struct optim_table	bin_set_funcs[] = {
	{ NULL,			COB_OPTIM_MIN },
	{ "cob_setswp_u16",	COB_SETSWP_U16 },
	{ "cob_setswp_u24",	COB_SETSWP_U24 },
	{ "cob_setswp_u32",	COB_SETSWP_U32 },
	{ "cob_setswp_u40",	COB_SETSWP_U40 },
	{ "cob_setswp_u48",	COB_SETSWP_U48 },
	{ "cob_setswp_u56",	COB_SETSWP_U56 },
	{ "cob_setswp_u64",	COB_SETSWP_U64 },
	{ NULL,			COB_OPTIM_MIN },
	{ "cob_setswp_s16",	COB_SETSWP_S16 },
	{ "cob_setswp_s24",	COB_SETSWP_S24 },
	{ "cob_setswp_s32",	COB_SETSWP_S32 },
	{ "cob_setswp_s40",	COB_SETSWP_S40 },
	{ "cob_setswp_s48",	COB_SETSWP_S48 },
	{ "cob_setswp_s56",	COB_SETSWP_S56 },
	{ "cob_setswp_s64",	COB_SETSWP_S64 }
};

static const struct optim_table	bin_compare_funcs[] = {
	{ "cob_cmp_u8",		COB_CMP_U8 },
	{ "cob_cmp_u16",	COB_CMP_U16 },
	{ "cob_cmp_u24",	COB_CMP_U24 },
	{ "cob_cmp_u32",	COB_CMP_U32 },
	{ "cob_cmp_u40",	COB_CMP_U40 },
	{ "cob_cmp_u48",	COB_CMP_U48 },
	{ "cob_cmp_u56",	COB_CMP_U56 },
	{ "cob_cmp_u64",	COB_CMP_U64 },
	{ "cob_cmp_s8",		COB_CMP_S8 },
	{ "cob_cmp_s16",	COB_CMP_S16 },
	{ "cob_cmp_s24",	COB_CMP_S24 },
	{ "cob_cmp_s32",	COB_CMP_S32 },
	{ "cob_cmp_s40",	COB_CMP_S40 },
	{ "cob_cmp_s48",	COB_CMP_S48 },
	{ "cob_cmp_s56",	COB_CMP_S56 },
	{ "cob_cmp_s64",	COB_CMP_S64 },
	{ "cob_cmp_u8",		COB_CMP_U8 },
	{ "cob_cmpswp_u16",	COB_CMPSWP_U16 },
	{ "cob_cmpswp_u24",	COB_CMPSWP_U24 },
	{ "cob_cmpswp_u32",	COB_CMPSWP_U32 },
	{ "cob_cmpswp_u40",	COB_CMPSWP_U40 },
	{ "cob_cmpswp_u48",	COB_CMPSWP_U48 },
	{ "cob_cmpswp_u56",	COB_CMPSWP_U56 },
	{ "cob_cmpswp_u64",	COB_CMPSWP_U64 },
	{ "cob_cmp_s8",		COB_CMP_S8 },
	{ "cob_cmpswp_s16",	COB_CMPSWP_S16 },
	{ "cob_cmpswp_s24",	COB_CMPSWP_S24 },
	{ "cob_cmpswp_s32",	COB_CMPSWP_S32 },
	{ "cob_cmpswp_s40",	COB_CMPSWP_S40 },
	{ "cob_cmpswp_s48",	COB_CMPSWP_S48 },
	{ "cob_cmpswp_s56",	COB_CMPSWP_S56 },
	{ "cob_cmpswp_s64",	COB_CMPSWP_S64 }
};

static const struct optim_table	bin_add_funcs[] = {
	{ "cob_add_u8",		COB_ADD_U8 },
	{ "cob_add_u16",	COB_ADD_U16 },
	{ "cob_add_u24",	COB_ADD_U24 },
	{ "cob_add_u32",	COB_ADD_U32 },
	{ "cob_add_u40",	COB_ADD_U40 },
	{ "cob_add_u48",	COB_ADD_U48 },
	{ "cob_add_u56",	COB_ADD_U56 },
	{ "cob_add_u64",	COB_ADD_U64 },
	{ "cob_add_s8",		COB_ADD_S8 },
	{ "cob_add_s16",	COB_ADD_S16 },
	{ "cob_add_s24",	COB_ADD_S24 },
	{ "cob_add_s32",	COB_ADD_S32 },
	{ "cob_add_s40",	COB_ADD_S40 },
	{ "cob_add_s48",	COB_ADD_S48 },
	{ "cob_add_s56",	COB_ADD_S56 },
	{ "cob_add_s64",	COB_ADD_S64 },
	{ "cob_add_u8",		COB_ADD_U8 },
	{ "cob_addswp_u16",	COB_ADDSWP_U16 },
	{ "cob_addswp_u24",	COB_ADDSWP_U24 },
	{ "cob_addswp_u32",	COB_ADDSWP_U32 },
	{ "cob_addswp_u40",	COB_ADDSWP_U40 },
	{ "cob_addswp_u48",	COB_ADDSWP_U48 },
	{ "cob_addswp_u56",	COB_ADDSWP_U56 },
	{ "cob_addswp_u64",	COB_ADDSWP_U64 },
	{ "cob_add_s8",		COB_ADD_S8 },
	{ "cob_addswp_s16",	COB_ADDSWP_S16 },
	{ "cob_addswp_s24",	COB_ADDSWP_S24 },
	{ "cob_addswp_s32",	COB_ADDSWP_S32 },
	{ "cob_addswp_s40",	COB_ADDSWP_S40 },
	{ "cob_addswp_s48",	COB_ADDSWP_S48 },
	{ "cob_addswp_s56",	COB_ADDSWP_S56 },
	{ "cob_addswp_s64",	COB_ADDSWP_S64 }
};

static const struct optim_table	bin_sub_funcs[] = {
	{ "cob_sub_u8",		COB_SUB_U8 },
	{ "cob_sub_u16",	COB_SUB_U16 },
	{ "cob_sub_u24",	COB_SUB_U24 },
	{ "cob_sub_u32",	COB_SUB_U32 },
	{ "cob_sub_u40",	COB_SUB_U40 },
	{ "cob_sub_u48",	COB_SUB_U48 },
	{ "cob_sub_u56",	COB_SUB_U56 },
	{ "cob_sub_u64",	COB_SUB_U64 },
	{ "cob_sub_s8",		COB_SUB_S8 },
	{ "cob_sub_s16",	COB_SUB_S16 },
	{ "cob_sub_s24",	COB_SUB_S24 },
	{ "cob_sub_s32",	COB_SUB_S32 },
	{ "cob_sub_s40",	COB_SUB_S40 },
	{ "cob_sub_s48",	COB_SUB_S48 },
	{ "cob_sub_s56",	COB_SUB_S56 },
	{ "cob_sub_s64",	COB_SUB_S64 },
	{ "cob_sub_u8",		COB_SUB_U8 },
	{ "cob_subswp_u16",	COB_SUBSWP_U16 },
	{ "cob_subswp_u24",	COB_SUBSWP_U24 },
	{ "cob_subswp_u32",	COB_SUBSWP_U32 },
	{ "cob_subswp_u40",	COB_SUBSWP_U40 },
	{ "cob_subswp_u48",	COB_SUBSWP_U48 },
	{ "cob_subswp_u56",	COB_SUBSWP_U56 },
	{ "cob_subswp_u64",	COB_SUBSWP_U64 },
	{ "cob_sub_s8",		COB_SUB_S8 },
	{ "cob_subswp_s16",	COB_SUBSWP_S16 },
	{ "cob_subswp_s24",	COB_SUBSWP_S24 },
	{ "cob_subswp_s32",	COB_SUBSWP_S32 },
	{ "cob_subswp_s40",	COB_SUBSWP_S40 },
	{ "cob_subswp_s48",	COB_SUBSWP_S48 },
	{ "cob_subswp_s56",	COB_SUBSWP_S56 },
	{ "cob_subswp_s64",	COB_SUBSWP_S64 }
};

#if	defined(COB_NON_ALIGNED) && !defined(_MSC_VER) && defined(COB_ALLOW_UNALIGNED)
static const struct optim_table	align_bin_compare_funcs[] = {
	{ "cob_cmp_u8",			COB_CMP_U8 },
	{ "cob_cmp_align_u16",		COB_CMP_ALIGN_U16 },
	{ "cob_cmp_u24",		COB_CMP_U24 },
	{ "cob_cmp_align_u32",		COB_CMP_ALIGN_U32 },
	{ "cob_cmp_u40",		COB_CMP_U40 },
	{ "cob_cmp_u48",		COB_CMP_U48 },
	{ "cob_cmp_u56",		COB_CMP_U56 },
	{ "cob_cmp_align_u64",		COB_CMP_ALIGN_U64 },
	{ "cob_cmp_s8",			COB_CMP_S8 },
	{ "cob_cmp_align_s16",		COB_CMP_ALIGN_S16 },
	{ "cob_cmp_s24",		COB_CMP_S24 },
	{ "cob_cmp_align_s32",		COB_CMP_ALIGN_S32 },
	{ "cob_cmp_s40",		COB_CMP_S40 },
	{ "cob_cmp_s48",		COB_CMP_S48 },
	{ "cob_cmp_s56",		COB_CMP_S56 },
	{ "cob_cmp_align_s64",		COB_CMP_ALIGN_S64 },
	{ "cob_cmp_u8",			COB_CMP_U8 },
	{ "cob_cmpswp_align_u16",	COB_CMPSWP_ALIGN_U16 },
	{ "cob_cmpswp_u24",		COB_CMPSWP_U24 },
	{ "cob_cmpswp_align_u32",	COB_CMPSWP_ALIGN_U32 },
	{ "cob_cmpswp_u40",		COB_CMPSWP_U40 },
	{ "cob_cmpswp_u48",		COB_CMPSWP_U48 },
	{ "cob_cmpswp_u56",		COB_CMPSWP_U56 },
	{ "cob_cmpswp_align_u64",	COB_CMPSWP_ALIGN_U64 },
	{ "cob_cmp_s8",			COB_CMP_S8 },
	{ "cob_cmpswp_align_s16",	COB_CMPSWP_ALIGN_S16 },
	{ "cob_cmpswp_s24",		COB_CMPSWP_S24 },
	{ "cob_cmpswp_align_s32",	COB_CMPSWP_ALIGN_S32 },
	{ "cob_cmpswp_s40",		COB_CMPSWP_S40 },
	{ "cob_cmpswp_s48",		COB_CMPSWP_S48 },
	{ "cob_cmpswp_s56",		COB_CMPSWP_S56 },
	{ "cob_cmpswp_align_s64",	COB_CMPSWP_ALIGN_S64 },
};

static const struct optim_table	align_bin_add_funcs[] = {
	{ "cob_add_u8",		COB_ADD_U8 },
	{ "cob_add_align_u16",	COB_ADD_ALIGN_U16 },
	{ "cob_add_u24",	COB_ADD_U24 },
	{ "cob_add_align_u32",	COB_ADD_ALIGN_U32 },
	{ "cob_add_u40",	COB_ADD_U40 },
	{ "cob_add_u48",	COB_ADD_U48 },
	{ "cob_add_u56",	COB_ADD_U56 },
	{ "cob_add_align_u64",	COB_ADD_ALIGN_U64 },
	{ "cob_add_s8",		COB_ADD_S8 },
	{ "cob_add_align_s16",	COB_ADD_ALIGN_S16 },
	{ "cob_add_s24",	COB_ADD_S24 },
	{ "cob_add_align_s32",	COB_ADD_ALIGN_S32 },
	{ "cob_add_s40",	COB_ADD_S40 },
	{ "cob_add_s48",	COB_ADD_S48 },
	{ "cob_add_s56",	COB_ADD_S56 },
	{ "cob_add_align_s64",	COB_ADD_ALIGN_S64 },
	{ "cob_add_u8",		COB_ADD_U8 },
	{ "cob_addswp_u16",	COB_ADDSWP_U16 },
	{ "cob_addswp_u24",	COB_ADDSWP_U24 },
	{ "cob_addswp_u32",	COB_ADDSWP_U32 },
	{ "cob_addswp_u40",	COB_ADDSWP_U40 },
	{ "cob_addswp_u48",	COB_ADDSWP_U48 },
	{ "cob_addswp_u56",	COB_ADDSWP_U56 },
	{ "cob_addswp_u64",	COB_ADDSWP_U64 },
	{ "cob_add_s8",		COB_ADD_S8 },
	{ "cob_addswp_s16",	COB_ADDSWP_S16 },
	{ "cob_addswp_s24",	COB_ADDSWP_S24 },
	{ "cob_addswp_s32",	COB_ADDSWP_S32 },
	{ "cob_addswp_s40",	COB_ADDSWP_S40 },
	{ "cob_addswp_s48",	COB_ADDSWP_S48 },
	{ "cob_addswp_s56",	COB_ADDSWP_S56 },
	{ "cob_addswp_s64",	COB_ADDSWP_S64 },
};

static const struct optim_table	align_bin_sub_funcs[] = {
	{ "cob_sub_u8",		COB_SUB_U8 },
	{ "cob_sub_align_u16",	COB_SUB_ALIGN_U16 },
	{ "cob_sub_u24",	COB_SUB_U24 },
	{ "cob_sub_align_u32",	COB_SUB_ALIGN_U32 },
	{ "cob_sub_u40",	COB_SUB_U40 },
	{ "cob_sub_u48",	COB_SUB_U48 },
	{ "cob_sub_u56",	COB_SUB_U56 },
	{ "cob_sub_align_u64",	COB_SUB_ALIGN_U64 },
	{ "cob_sub_s8",		COB_SUB_S8 },
	{ "cob_sub_align_s16",	COB_SUB_ALIGN_S16 },
	{ "cob_sub_s24",	COB_SUB_S24 },
	{ "cob_sub_align_s32",	COB_SUB_ALIGN_S32 },
	{ "cob_sub_s40",	COB_SUB_S40 },
	{ "cob_sub_s48",	COB_SUB_S48 },
	{ "cob_sub_s56",	COB_SUB_S56 },
	{ "cob_sub_align_s64",	COB_SUB_ALIGN_S64 },
	{ "cob_sub_u8",		COB_SUB_U8 },
	{ "cob_subswp_u16",	COB_SUBSWP_U16 },
	{ "cob_subswp_u24",	COB_SUBSWP_U24 },
	{ "cob_subswp_u32",	COB_SUBSWP_U32 },
	{ "cob_subswp_u40",	COB_SUBSWP_U40 },
	{ "cob_subswp_u48",	COB_SUBSWP_U48 },
	{ "cob_subswp_u56",	COB_SUBSWP_U56 },
	{ "cob_subswp_u64",	COB_SUBSWP_U64 },
	{ "cob_sub_s8",		COB_SUB_S8 },
	{ "cob_subswp_s16",	COB_SUBSWP_S16 },
	{ "cob_subswp_s24",	COB_SUBSWP_S24 },
	{ "cob_subswp_s32",	COB_SUBSWP_S32 },
	{ "cob_subswp_s40",	COB_SUBSWP_S40 },
	{ "cob_subswp_s48",	COB_SUBSWP_S48 },
	{ "cob_subswp_s56",	COB_SUBSWP_S56 },
	{ "cob_subswp_s64",	COB_SUBSWP_S64 },
};
#endif

/* Functions */
static cb_tree cb_build_name_reference (struct cb_field *f1, struct cb_field *f2);
static void cb_walk_cond		(cb_tree);
static int	cb_check_move		(cb_tree, cb_tree, const int);
static int	cb_check_set_to		(cb_tree, cb_tree, const int);
static int	cb_check_arithmetic	(cb_tree, cb_tree, const int);
static cb_tree cb_build_length_1 (cb_tree x);
static struct cb_field	*check_search_table = NULL;
static struct cb_field	*check_search_index = NULL;
static struct cb_field	*check_base_p = NULL;
static struct cb_field	*check_odo_p = NULL;
static struct cb_field	*check_subscript_p = NULL;
static struct cb_field	*check_sub = NULL;
static int cb_is_integer_expr (cb_tree x);
static int error_string_not_usage_display_or_national (cb_tree ref);


/*
 * Force reset of all check optimization
 */
static void
cb_check_reset ()
{
	check_subscript_p = NULL;
	check_base_p = NULL;
	check_odo_p = NULL;
	check_sub = NULL;
}

/*
 * Does a change to the given field invalidate any optimization
 */
static void
cb_check_optim (struct cb_field *p)
{
	if (!cb_flag_optimize_check
	 || p == NULL)
		return;
	if (p == check_subscript_p
	 || p == check_odo_p
	 || p == check_sub) {
		check_subscript_p = NULL;
		check_odo_p = NULL;
		check_sub = NULL;
	} else
	if (p == check_base_p) {
		check_subscript_p = NULL;
		check_base_p = NULL;
		check_odo_p = NULL;
		check_sub = NULL;
	}
	if (check_subscript_p != NULL
	 && check_subscript_p->depending
	 && cb_code_field (check_subscript_p->depending) == p) {
		check_subscript_p = NULL;
		check_base_p = NULL;
		check_odo_p = NULL;
		check_sub = NULL;
	}
}

static void
cb_check_list (cb_tree vars)
{
	cb_tree	x, y, l;
	struct cb_field *f;
	struct cb_assign *ap;

	if (!cb_flag_optimize_check)
		return;
	for (l = vars; l; l = CB_CHAIN (l)) {
		x = CB_VALUE(l);
		f = NULL;
		if (CB_FIELD_P (x)) {
			f = CB_FIELD_PTR(x);
		} else if (CB_ASSIGN_P (x)) {
			ap = CB_ASSIGN(x);
			if (CB_REF_OR_FIELD_P (ap->var)) {
				f = CB_FIELD_PTR(ap->var);
			}
		} else if (CB_CAST_P (x)) {
			f = cb_code_field (CB_CAST(x)->val);
		} else if (CB_REFERENCE_P (x)) {
			y = cb_ref (x);
			if (y == cb_error_node) 
				continue;
			if (CB_FIELD_P (y))
				f = CB_FIELD_PTR (y);
		}
		if (f)
			cb_check_optim (f);
	}
}

static void
cb_add_null_check (const char *routine, struct cb_field *p, struct cb_field *f)
{
	if (cb_flag_optimize_check
	 && p == check_base_p)
		return;
	check_base_p = p;
	current_statement->null_check = CB_BUILD_FUNCALL_2 (
		routine, 
		cb_build_address (cb_build_field_reference (p, NULL)),
		CB_BUILD_STRING0 (
			CB_REFERENCE(cb_build_name_reference (p, f))->word->name));
}

static cb_tree
cb_add_check_odo ( struct cb_field *p )
{
	if (cb_flag_optimize_check
	 && check_odo_p == p)
		return NULL;
	check_odo_p = p;
	return CB_BUILD_FUNCALL_5 ("cob_check_odo",
			 cb_build_cast_int (p->depending),
			 cb_int (p->occurs_min),
			 cb_int (p->occurs_max),
			 CB_BUILD_STRING0 (p->name),
			 CB_BUILD_STRING0 (CB_FIELD_PTR (p->depending)->name));
}

static cb_tree
cb_add_check_subscript ( struct cb_field *p, cb_tree sub, const char *name, const int opt )
{
	struct cb_field *sub_p, *pp;
	cb_tree	y;

	sub_p = NULL;
	if (CB_REFERENCE_P (sub)) {
		y = cb_ref (sub);
		if (y != cb_error_node
		 && CB_FIELD_P (y))
			sub_p = CB_FIELD_PTR (y);
	} else if (CB_FIELD_P (sub)) {
		sub_p = CB_FIELD_PTR (sub);
	}
	if (check_search_index != NULL
	 && check_search_index == sub_p) {
		/* If field is a member of the table being searched, skip check */
		for (pp = p; pp; pp = pp->parent) {
			if (pp == check_search_table)	
				return NULL;
		}
	}
	if (cb_flag_optimize_check) {
		if (check_subscript_p == p
		 && check_sub == sub_p)
			return NULL;
		check_subscript_p = p;
		check_sub = sub_p;
	}
	if (opt == 1) {
		return CB_BUILD_FUNCALL_4 ("cob_check_subscript",
			 cb_build_cast_int (sub),
			 cb_build_cast_int (p->depending),
			 CB_BUILD_STRING0 (name),
			 cb_int1);
	} else {
		return CB_BUILD_FUNCALL_4 ("cob_check_subscript",
			 cb_build_cast_int (sub),
			 cb_int (p->occurs_max),
			 CB_BUILD_STRING0 (name),
			 cb_int0);
	}
}

/*
 * Is the field 'native' binary (short/int/long)
 * and aligned on memory address suitable for direct use
 */
static int
cb_is_integer_field (const struct cb_field *f)
{
	if (!cb_flag_fast_math)
		return 0;
	if (f->flag_sign_clause
	 || f->flag_blank_zero
	 || f->flag_any_numeric
	 || f->indexes != 0
	 || !f->pic
	 || f->pic->scale != 0)
		return 0;
	if (f->usage == CB_USAGE_DISPLAY
	 && f->size < 16)
		return 1;
	if (f->usage == int_usage	/* For check to allow PACKED */
	 && f->size < 16)
		return 1;
	if (int_usage == CB_USAGE_PACKED	/* Check for COMP-6 */
	 && f->usage == CB_USAGE_COMP_6
	 && f->size < 16)
		return 1;
	if (f->usage == CB_USAGE_COMP_X
	 && f->size == 1)
		return 1;
	if (f->usage == CB_USAGE_BINARY
	 && cb_binary_truncate)
		return 0;
#ifdef WORDS_BIGENDIAN
	if (f->usage != CB_USAGE_COMP_5
	 && f->usage != CB_USAGE_DISPLAY
	 && f->usage != CB_USAGE_BINARY
	 && f->usage != CB_USAGE_COMP_X)
		return 0;
#else
	if (f->usage != CB_USAGE_COMP_5
	 && f->usage != CB_USAGE_BINARY
	 && f->usage != CB_USAGE_DISPLAY)
		return 0;
#endif
	if (f->storage == CB_STORAGE_WORKING
#ifdef	COB_SHORT_BORK
	 && (f->size == 4 || f->size == 8 || f->size == 1)
#else
	 && (f->size == 2 || f->size == 4 || f->size == 8 || f->size == 1)
#endif
#if !defined(COB_ALLOW_UNALIGNED)
	 && (f->offset % f->size) == 0
#endif
	 ) {
		return 1;
	}
	return 0;
}

/*
 * Is the field 'native' binary (short/int/long) or comp-x
 */
static int
cb_is_compx_field (struct cb_field *f)
{
	if (f->usage == CB_USAGE_COMP_5
	 || f->usage == CB_USAGE_COMP_X)
		return 1;
	cb_error_x (CB_TREE(current_statement), _("%s should be COMP-5/COMP-X for logical operator"), f->name);
	return 0;
}

/*
 * Is this an 'compx' value or expression
 */
static int
cb_is_compx_expr (cb_tree x)
{
	struct cb_binary_op	*p;
	cb_tree	y;
	if (!cb_flag_fast_math)
		return 0;
	if (current_statement
	 && (current_statement->ex_handler
	  || current_statement->not_ex_handler
	  || current_statement->handler_type != NO_HANDLER))
		return 0;
	if (CB_REFERENCE_P (x)) {
		y = cb_ref (x);
		if (y == cb_error_node) {
			return 0;
		}
		if (CB_FIELD_P (y))
			return cb_is_compx_field (CB_FIELD_PTR (y));
		return 0;
	}
	if (CB_FIELD_P (x)) {
		return cb_is_compx_field (CB_FIELD_PTR (x));
	}
	if (CB_BINARY_OP_P (x)) {
		p = CB_BINARY_OP (x);
		if (p->op == '+'
		 || p->op == '-'
		 || p->op == '*') {
			if (cb_is_integer_expr (p->x)
			 && cb_is_integer_expr (p->y))
				return 1;
		}
		if (p->op == '='
		 || p->op == '>'
		 || p->op == '<'
		 || p->op == ']'
		 || p->op == '['
		 || p->op == '~'
		 || p->op == '('
		 || p->op == ')'
		 || p->op == '@') {
			if (CB_NUMERIC_LITERAL_P (p->x)
			 && (CB_NUMERIC_LITERAL_P (p->y) || CB_BINARY_OP_P (p->y)))
				return 0;
			if (CB_NUMERIC_LITERAL_P (p->y)
			 && (CB_NUMERIC_LITERAL_P (p->x) || CB_BINARY_OP_P (p->x)))
				return 0;
			if (p->x
			 && !cb_is_integer_expr (p->x))
				return 0;
			if (p->y
			 && !cb_is_integer_expr (p->y))
				return 0;
			return 1;
		}
		if (p->op == 'a'
		 || p->op == 'o'
		 || p->op == 'e'
		 || p->op == 'c'
		 || p->op == 'd'
		 || p->op == 'l'
		 || p->op == 'r') {	/* BIT-WISE */
			if (p->x
			 && !cb_is_compx_expr (p->x))
				return 0;
			if (p->y
			 && !cb_is_compx_expr (p->y))
				return 0;
			return 1;
		}
		if (p->op == 'n') {	/* BIT-WISE */
			if (cb_is_compx_expr (p->y))
				return 1;
		}
	}
	if (CB_NUMERIC_LITERAL_P (x))
		return 1;
	return 0;
}

/*
 * Is this an 'integer' value or expression
 */
static int
cb_is_integer_expr (cb_tree x)
{
	struct cb_binary_op	*p;
	cb_tree	y;
	if (!cb_flag_fast_math)
		return 0;
	if (current_statement
	 && (current_statement->ex_handler
	  || current_statement->not_ex_handler
	  || current_statement->handler_type != NO_HANDLER))
		return 0;
	if (CB_REFERENCE_P (x)) {
		y = cb_ref (x);
		if (y == cb_error_node) {
			return 0;
		}
		if (CB_FIELD_P (y))
			return cb_is_integer_field (CB_FIELD_PTR (y));
		return 0;
	}
	if (CB_FIELD_P (x)) {
		return cb_is_integer_field (CB_FIELD_PTR (x));
	}
	if (CB_NUMERIC_LITERAL_P (x)) {
		if (CB_LITERAL (x)->scale > expr_dmax)
			expr_dmax = CB_LITERAL (x)->scale;
	 	if (CB_LITERAL (x)->scale == 0
		 && cb_fits_int (x))
			return 1;
		return 0;
	}
	if (CB_BINARY_OP_P (x)) {
		p = CB_BINARY_OP (x);
		if (p->op == '+'
		 || p->op == '-'
		 || p->op == '*') {
			if (cb_is_integer_expr (p->x)
			 && cb_is_integer_expr (p->y))
				return 1;
		}
		if (p->op == '='
		 || p->op == '>'
		 || p->op == '<'
		 || p->op == ']'
		 || p->op == '['
		 || p->op == '~'
		 || p->op == '('
		 || p->op == ')'
		 || p->op == '@') {
			if (CB_NUMERIC_LITERAL_P (p->x)
			 && (CB_NUMERIC_LITERAL_P (p->y) || CB_BINARY_OP_P (p->y)))
				return 0;
			if (CB_NUMERIC_LITERAL_P (p->y)
			 && (CB_NUMERIC_LITERAL_P (p->x) || CB_BINARY_OP_P (p->x)))
				return 0;
			if (p->x
			 && !cb_is_integer_expr (p->x))
				return 0;
			if (p->y
			 && !cb_is_integer_expr (p->y))
				return 0;
			return 1;
		}
		if (p->op == 'a'
		 || p->op == 'o'
		 || p->op == 'e'
		 || p->op == 'l'
		 || p->op == 'r') {	/* BIT-WISE */
			if (CB_NUMERIC_LITERAL_P (p->x)
			 && (CB_NUMERIC_LITERAL_P (p->y) || CB_BINARY_OP_P (p->y))) {
				return 1;
			}
			if (CB_NUMERIC_LITERAL_P (p->y)
			 && (CB_NUMERIC_LITERAL_P (p->x) || CB_BINARY_OP_P (p->x))) {
				return 1;
			}
			if (p->x
			 && !cb_is_compx_expr (p->x))
				return 0;
			if (p->y
			 && !cb_is_compx_expr (p->y))
				return 0;
			return 1;
		}
		if (p->op == 'n') {	/* BIT-WISE NOT */
			if (p->y
			 && !cb_is_compx_expr (p->y))
				return 0;
			return 1;
		}
	}
	return 0;
}

/*
 * Is field an aligned binary and 'n' is either integer
 * or another aligned binary field
 */
static int
cb_is_integer_field_and_int (struct cb_field *f, cb_tree n)
{
	if (!cb_is_integer_field (f))
		return 0;
	if (CB_NUMERIC_LITERAL_P (n)) {
	 	if (CB_LITERAL (n)->scale == 0
		 && CB_LITERAL (n)->sign
		 && cb_fits_int (n)
		 && f->pic->have_sign == 0)
			return 0;
		return 1;
	}
	return cb_is_integer_expr (n);
}

static cb_tree
cb_check_needs_break (cb_tree stmt)
{
	cb_tree		l;

	/* Check if last statement is GO TO */
	for (l = stmt; l; l = CB_CHAIN (l)) {
		if (!CB_CHAIN(l)) {
			break;
		}
	}
	if (l && CB_VALUE (l) && CB_STATEMENT_P (CB_VALUE (l))) {
		l = CB_STATEMENT(CB_VALUE(l))->body;
		if (l && CB_VALUE (l) && !CB_GOTO_P (CB_VALUE(l))) {
			/* Append a break */
			l = cb_build_direct ("break;", 0);
			return cb_list_add (stmt, l);
		}
	}
	return stmt;
}

static size_t
cb_validate_one (cb_tree x)
{

	if (x == cb_error_node) {
		return 1;
	}
	if (!x) {
		return 0;
	}
	if (CB_REFERENCE_P (x)) {
		const cb_tree	y = cb_ref (x);
		if (y == cb_error_node) {
			return 1;
		}
		if (CB_FIELD_P (y)) {
			const struct cb_field	*f = CB_FIELD (y);
			if (f->level == 88) {
				cb_error_x (x, _("condition-name not allowed here: '%s'"), f->name);
				return 1;
			}
			if (f->flag_invalid) {
				return 1;
			}
			/* validate use of handles depending on the statement */
			if (f->usage == CB_USAGE_HNDL ||
				f->usage == CB_USAGE_HNDL_WINDOW ||
				f->usage == CB_USAGE_HNDL_SUBWINDOW ||
				f->usage == CB_USAGE_HNDL_FONT ||
				f->usage == CB_USAGE_HNDL_THREAD ||
				f->usage == CB_USAGE_HNDL_MENU ||
				f->usage == CB_USAGE_HNDL_VARIANT ||
				f->usage == CB_USAGE_HNDL_LM) {
				/* valid statements: CALL, MOVE, DISPLAY + expressions
				   the only statements reaching this are MOVE and DISPLAY */
				if (current_statement->statement != STMT_MOVE
				 && current_statement->statement != STMT_DISPLAY
				 && current_statement->statement != STMT_DESTROY
				 &&	current_statement->statement != STMT_CLOSE_WINDOW) {
				 		cb_error_x (x, _("%s item not allowed here: '%s'"),
							"HANDLE", f->name);
					return 1;
				}
			}
		}
	}
	return 0;
}

static size_t
cb_validate_list (cb_tree l)
{
	for (; l; l = CB_CHAIN (l)) {
		if (cb_validate_one (CB_VALUE (l))) {
			return 1;
		}
	}
	return 0;
}

static cb_tree
cb_check_group_name (cb_tree x)
{
	cb_tree		y;

	if (x == cb_error_node) {
		return cb_error_node;
	}

	if (CB_REFERENCE_P (x)) {
		y = cb_ref (x);
		if (y == cb_error_node) {
			return cb_error_node;
		}
		if (CB_FIELD_P (y)
		 && CB_FIELD (y)->children != NULL
		 && CB_REFERENCE (x)->offset == NULL) {
			return x;
		}
	}

	cb_error_x (x, _("'%s' is not a group name"), cb_name (x));
	return cb_error_node;
}

static cb_tree
cb_check_numeric_name (cb_tree x)
{
#if 0 /* already checked before called */
	if (x == cb_error_node) {
		return cb_error_node;
	}
#endif

	if (CB_CAST_P (x)) {
		x = CB_CAST (x)->val;
	}

	if (CB_REFERENCE_P (x)
	 && CB_FIELD_P (cb_ref (x))
	 && CB_TREE_CATEGORY (x) == CB_CATEGORY_NUMERIC) {
		return x;
	}
	if(CB_REFERENCE_P (x)
	&& CB_FIELD_P (cb_ref (x))) {
		const struct cb_field *f = CB_FIELD_PTR (x);
		if(f->usage == CB_USAGE_COMP_X)
			return x;
	}

	cb_error_x (x, _("'%s' is not a numeric name"), cb_name (x));
	return cb_error_node;
}

static cb_tree
cb_check_numeric_edited_name (cb_tree x)
{
#if 0 /* already checked before called */
	if (x == cb_error_node) {
		return cb_error_node;
	}
#endif

	if (CB_REFERENCE_P (x)
	 && CB_FIELD_P (cb_ref (x))) {
		enum cb_category cat = CB_TREE_CATEGORY(x);
		if (cat == CB_CATEGORY_NUMERIC
		 || cat == CB_CATEGORY_NUMERIC_EDITED
		 || cat == CB_CATEGORY_FLOATING_EDITED) {
			return x;
		}
	}

	if(CB_REFERENCE_P (x)
	&& CB_FIELD_P (cb_ref (x))) {
		const struct cb_field *f = CB_FIELD_PTR (x);
		if(f->usage == CB_USAGE_COMP_X)
			return x;
	}
	cb_error_x (x, _("'%s' is not a numeric or numeric-edited name"), cb_name (x));
	return cb_error_node;
}

int
cb_is_field_unbounded (struct cb_field *fld)
{
	struct cb_field         *f;

	if (fld->flag_unbounded) {
		return 1;
	}
	for (f = fld->children; f; f = f->sister) {
		if (cb_is_field_unbounded (f)) {
			return 1;
		}
	}
	return 0;
}

cb_tree
cb_check_sum_field (cb_tree x)
{
	struct cb_field		*f;

	if (CB_TREE_CATEGORY (x) != CB_CATEGORY_NUMERIC_EDITED) {
		return x;
	}

	f = CB_FIELD (cb_ref(x));
	if (f->report) {		/* If part of a REPORT, check if it is a SUM */
		struct cb_field	*sc = get_sum_data_field (f->report, f);
		if (sc) {	/* Use the SUM variable instead of the print variable */
			return cb_build_field_reference (sc, NULL);
		}
	}
	return x;
}

cb_tree
cb_check_numeric_value (cb_tree x)
{
	const enum cb_category cat = CB_TREE_CATEGORY (x);

	if (cb_validate_one (x)) {
		return cb_error_node;
	}

	switch (cat) {
	case CB_CATEGORY_NUMERIC:
		return x;
	case CB_CATEGORY_NUMERIC_EDITED:
	case CB_CATEGORY_FLOATING_EDITED:
	{
		struct cb_field	*f = CB_FIELD_PTR (x);
		if (f->report) {
			struct cb_field	*sc = get_sum_data_field (f->report, f);
			if (sc) {	/* Use the SUM variable instead of the print variable */
				/* FIXME: this is not directly a check */
				return cb_build_field_reference (sc, NULL);
			}
		}
		/* Fall-through as we only allow this for RW: SUM */
	}
	default:
		cb_error_x (x, _("'%s' is not numeric"), cb_name (x));
	}
	return cb_error_node;
}

static cb_tree
cb_check_integer_value (cb_tree x)
{
	struct cb_literal	*l;
	struct cb_field		*f;
	cb_tree			y;

	if (x == cb_error_node) {
		return cb_error_node;
	}

	if (CB_TREE_CATEGORY (x) != CB_CATEGORY_NUMERIC) {
		goto invalid;
	}

	switch (CB_TREE_TAG (x)) {
	case CB_TAG_CONST:
		if (x != cb_zero) {
			goto invalid;
		}
		return x;
	case CB_TAG_LITERAL:
		l = CB_LITERAL (x);
		if (l->sign < 0 || l->scale > 0) {
			goto invliteral;
		}
		return x;
	case CB_TAG_REFERENCE:
		y = cb_ref (x);
		if (y == cb_error_node) {
			return cb_error_node;
		}
		f = CB_FIELD (y);
		if (f->pic->scale > 0) {
			goto invalid;
		}
		return x;
	case CB_TAG_BINARY_OP:
		/* TODO: need to check */
		return x;
	case CB_TAG_INTRINSIC:
		/* TODO: need to check */
		return x;
	default:
invalid:
		cb_error_x (x, _("'%s' is not an integer"), cb_name (x));
		return cb_error_node;
	}
invliteral:
	cb_error_x (x, _("positive numeric integer is required here"));
	return cb_error_node;
}

static COB_INLINE COB_A_INLINE cb_tree
cb_emit (cb_tree x)
{
	current_statement->body = cb_list_add (current_statement->body, x);
	return x;
}

static COB_INLINE COB_A_INLINE cb_tree
cb_emit_list (cb_tree l)
{
	current_statement->body = cb_list_append (current_statement->body, l);
	return l;
}

static void
cb_emit_incompat_data_checks (cb_tree x)
{
	struct cb_field		*f;

	if (!x || x == cb_error_node) {
		return;
	}
	if (!CB_REF_OR_FIELD_P (x)
	 || CB_TREE_CATEGORY (x) != CB_CATEGORY_NUMERIC) {
		return;
	}
	f = CB_FIELD_PTR (x);
	if (cb_flag_correct_numeric && f->usage == CB_USAGE_DISPLAY) {
		cb_emit (CB_BUILD_FUNCALL_1 ("cob_correct_numeric", x));
	}
	if (CB_EXCEPTION_ENABLE (COB_EC_DATA_INCOMPATIBLE)) {
		if (f->usage == CB_USAGE_DISPLAY
		 || f->usage == CB_USAGE_PACKED
		 || f->usage == CB_USAGE_COMP_6) {
			cb_emit (CB_BUILD_FUNCALL_2 ("cob_check_numeric",
					x,
					CB_BUILD_STRING0 (f->name)));
		}
	}
}

static void
cb_check_lit_subs (struct cb_reference *r, const int numsubs,
		   const int numindex)
{
	cb_tree			l;
	cb_tree			v;
	struct cb_literal	*lt;
	int			size;

	/* Check for DPC and non-standard separator usage */
	if (!cb_relaxed_syntax_checks
	 || current_program->decimal_point != ',') {
		return;
	}
	if (numsubs > numindex) {
		return;
	}

	for (l = r->subs; l; l = CB_CHAIN (l)) {
		v = CB_VALUE (l);
		if (v == cb_error_node) {
			continue;
		}
		if (!CB_LITERAL_P (v)) {
			continue;
		}
		lt = CB_LITERAL (v);
		if (!lt->scale) {
			continue;
		}
		if (lt->scale == (int)lt->size) {
			lt->scale = 0;
			continue;
		}
		size = lt->size - lt->scale;
		v = cb_build_numsize_literal (&lt->data[size],
					      (size_t)lt->scale, lt->sign);
		CB_VALUE (l) = v;
		v = cb_build_numsize_literal (lt->data, (size_t)size, 0);
		CB_CHAIN (l) = CB_BUILD_CHAIN (v, CB_CHAIN (l));
	}
	return;
}

static int
usage_is_thread_handle (cb_tree x)
{
	struct cb_field *f;
	f = CB_FIELD_PTR (x);

	if (f->usage == CB_USAGE_HNDL ||
		f->usage == CB_USAGE_HNDL_THREAD) {
		return 1;
	}
	return 0;
}

static int
usage_is_window_handle (cb_tree x)
{
	struct cb_field *f;
	f = CB_FIELD_PTR (x);

	if (f->usage == CB_USAGE_HNDL ||
		f->usage == CB_USAGE_HNDL_WINDOW ||
		f->usage == CB_USAGE_HNDL_SUBWINDOW) {
		return 1;
	}
	if (f->usage == CB_USAGE_DISPLAY &&
		f->pic->category == CB_CATEGORY_ALPHANUMERIC &&
		f->size == 10){
		return 1;
	}
	return 0;
}

/* List system routines */

void
cb_list_system_routines (void)
{
	const struct system_table	*psyst;

	putchar ('\n');

	putchar ('\n');
	printf ("%-32s%s\n", _("System routine"), _("Parameters"));
	putchar ('\n');

	for (psyst = system_tab; psyst->syst_name; psyst++) {
		if (strlen (psyst->syst_name) != 1) {
			printf ("%-32s", psyst->syst_name);
		} else {
			printf ("X\"%2X\"%-27s", (unsigned char)psyst->syst_name[0], "");
		}
		if (psyst->syst_params_min != psyst->syst_params_max) {
			printf ("%d - %d", psyst->syst_params_min, psyst->syst_params_max);
		} else {
			printf ("%d", psyst->syst_params_min);
		}
		putchar ('\n');
	}
}

/* Check if tree is an INDEX */
size_t
cb_check_index_or_handle_p (cb_tree x)
{
	struct cb_field	*f;

	if (!CB_REF_OR_FIELD_P (x)) {
		return 0;
	}
	f = CB_FIELD_PTR (x);
	if (f->children) {
		return 0;
	}
	if (f->usage == CB_USAGE_INDEX ||
		f->usage == CB_USAGE_HNDL ||
		f->usage == CB_USAGE_HNDL_WINDOW ||
		f->usage == CB_USAGE_HNDL_SUBWINDOW ||
		f->usage == CB_USAGE_HNDL_FONT ||
		f->usage == CB_USAGE_HNDL_THREAD ||
		f->usage == CB_USAGE_HNDL_MENU ||
		f->usage == CB_USAGE_HNDL_VARIANT ||
		f->usage == CB_USAGE_HNDL_LM) {
		return 1;
	}
	return 0;
}

/* Check if a field reference requires debugging */

void
cb_check_field_debug (cb_tree fld)
{
	cb_tree		l;
	cb_tree		x;
	cb_tree		z;
	size_t		size;
	size_t		found;
	char		buff[COB_MINI_BUFF]; /* at least DEBUG-NAME + 4 + COB_MAX_NAMELEN...*/

	/* Basic reference check */
	if (CB_WORD_COUNT (fld) > 0) {
		if (!CB_WORD_ITEMS (fld)) {
			return;
		}
		z = CB_VALUE(CB_WORD_ITEMS (fld));
		if (!CB_FIELD_P (z)) {
			return;
		}
		x = cb_ref (fld);
		if (x == cb_error_node) {
			return;
		}
	} else {
		return;
	}

	found = 0;
	/* Check if reference is being debugged */
	for (l = current_program->debug_list; l; l = CB_CHAIN (l)) {
		if (!CB_PURPOSE (l)) {
			continue;
		}
		if (x == CB_PURPOSE (l)) {
			if (CB_REFERENCE (fld)->flag_target
			 || CB_REFERENCE (CB_VALUE (l))->flag_all_debug) {
				found = 1;
			}
			break;
		}
	}
	if (!found) {
		return;
	}

	found = 0;
	/* Found it - check if it is already in the statement list */
	for (l = current_statement->debug_nodups; l; l = CB_CHAIN (l)) {
		if (CB_VALUE (l) == x) {
			found = 1;
			break;
		}
	}
	if (found) {
		return;
	}

	/* Set up debug info */
	size = sprintf (buff, "%s", CB_FIELD (x)->name);
	/* DEBUG-NAME's max is fixed-length 30, so no use in writing more */
	for (l = CB_REFERENCE (fld)->chain; l && size < 30; l = CB_REFERENCE (l)->chain) {
		z = cb_ref (l);
		if (z != cb_error_node) {
			const char		*name = CB_FIELD (z)->name;
			const size_t	sname = strlen (name) + 1;
			if (size + 4 + sname >= sizeof(buff)) {
				break;
			}
			size += sprintf (buff + size, " OF %s", name);
		}
	}
	current_statement->debug_nodups =
		cb_list_add (current_statement->debug_nodups, x);
	current_statement->debug_check =
		cb_list_add (current_statement->debug_check,
			     cb_build_debug (cb_debug_name, buff, NULL));
	current_statement->debug_check =
		cb_list_add (current_statement->debug_check,
			     cb_build_debug (cb_debug_contents, NULL, fld));
	found = 0;
	CB_REFERENCE (fld)->subs = cb_list_reverse (CB_REFERENCE (fld)->subs);
	l = CB_REFERENCE (fld)->subs;
	for (; l && found < 3; l = CB_CHAIN (l), ++found) {
		switch (found) {
		case 0:
			current_statement->debug_check =
				cb_list_add (current_statement->debug_check,
					     cb_build_move (CB_VALUE (l),
							    cb_debug_sub_1));
			break;
		case 1:
			current_statement->debug_check =
				cb_list_add (current_statement->debug_check,
					     cb_build_move (CB_VALUE (l),
							    cb_debug_sub_2));
			break;
		case 2:
			current_statement->debug_check =
				cb_list_add (current_statement->debug_check,
					     cb_build_move (CB_VALUE (l),
							    cb_debug_sub_3));
			break;
		default:
			break;
		}
	}
	CB_REFERENCE (fld)->subs = cb_list_reverse (CB_REFERENCE (fld)->subs);

	for (; found < 3; ++found) {
		switch (found) {
		case 0:
			current_statement->debug_check =
				cb_list_add (current_statement->debug_check,
					     CB_BUILD_FUNCALL_3 ("memset",
						CB_BUILD_CAST_ADDRESS (cb_debug_sub_1),
						cb_int (' '),
						CB_BUILD_CAST_LENGTH (cb_debug_sub_1)));
			break;
		case 1:
			current_statement->debug_check =
				cb_list_add (current_statement->debug_check,
					     CB_BUILD_FUNCALL_3 ("memset",
						CB_BUILD_CAST_ADDRESS (cb_debug_sub_2),
						cb_int (' '),
						CB_BUILD_CAST_LENGTH (cb_debug_sub_2)));
			break;
		case 2:
			current_statement->debug_check =
				cb_list_add (current_statement->debug_check,
					     CB_BUILD_FUNCALL_3 ("memset",
						CB_BUILD_CAST_ADDRESS (cb_debug_sub_3),
						cb_int (' '),
						CB_BUILD_CAST_LENGTH (cb_debug_sub_3)));
			break;
		default:
			break;
		}
	}

	current_statement->debug_check =
		cb_list_add (current_statement->debug_check,
			     cb_build_debug_call (CB_FIELD(x)->debug_section));
}

/* Program registers */


/* RETURN-CODE */
static void
cb_build_register_return_code (const char *name, const char *definition)
{
	cb_tree field;

	if (!definition) {
		definition = cb_get_register_definition (name);
		if (!definition) {
			return;
		}
	}

	/* take care of (likely) GLOBAL */
#if 0	/* more to adjust in other places */
	if (current_program->nested_level && strstr (definition, "GLOBAL")) {
#else
	if (current_program->nested_level) {
#endif
		return;
	}

	field = cb_build_index (cb_build_reference (name), cb_zero, 0, NULL);
	CB_FIELD_PTR (field)->index_type = CB_STATIC_INT_INDEX;
	CB_FIELD_PTR (field)->flag_internal_register = 1;
	CB_FIELD_PTR (field)->level = 77;
	CB_FIELD_PTR (field)->flag_real_binary = 1;
	current_program->cb_return_code = field;
}

/* SORT-RETURN */
static void
cb_build_register_sort_return (const char *name, const char *definition)
{
	cb_tree field;

	if (!definition) {
		definition = cb_get_register_definition (name);
		if (!definition) {
			return;
		}
	}

#if 0	/* more to adjust in other places */
	/* take care of (unlikely) GLOBAL */
	if (current_program->nested_level && strstr (definition, "GLOBAL")) {
		return;
	}
#endif

	field = cb_build_index (cb_build_reference (name), cb_zero, 0, NULL);
	CB_FIELD_PTR (field)->flag_no_init = 1;
	CB_FIELD_PTR (field)->flag_internal_register = 1;
	CB_FIELD_PTR (field)->level = 77;
	CB_FIELD_PTR (field)->flag_real_binary = 1;
	current_program->cb_sort_return = field;
}

/* NUMBER-OF-CALL-PARAMETERS (OpenCOBOL/GnuCOBOL extension 1.0+) */
static void
cb_build_register_number_parameters (const char *name, const char *definition)
{
	cb_tree field;

	if (!definition) {
		definition = cb_get_register_definition (name);
		if (!definition) {
			return;
		}
	}

	field = cb_build_index (cb_build_reference (name), cb_zero, 0, NULL);
	CB_FIELD_PTR (field)->flag_no_init = 1;
	CB_FIELD_PTR (field)->flag_local = 1;
	CB_FIELD_PTR (field)->flag_internal_register = 1;
	CB_FIELD_PTR (field)->level = 77;
	CB_FIELD_PTR (field)->index_type = CB_INT_INDEX;
	CB_FIELD_PTR (field)->flag_real_binary = 1;
	current_program->cb_call_params = field;
}

static void cb_build_constant_register (cb_tree name, cb_tree value)
{
	cb_tree constant = cb_build_constant (name, value);
	CB_FIELD (constant)->flag_internal_register = 1;
	CB_FIELD_PTR (constant)->level = 77;
}

/* WHEN-COMPILED */
static void
cb_build_register_when_compiled (const char *name, const char *definition)
{
	char		buff[32]; /* 32: make the compiler happy as "unsigned short" *could*
						         have more digits than we "assume" */
	size_t lit_size;

	if (!definition) {
		definition = cb_get_register_definition (name);
		if (!definition) {
			return;
		}
	}

	/* FIXME: the actual content is different for at least OSVS,
	   as this uses "hh.mm.ssMMM DD, YYYY", we should  assume this
	   if the register's definition contains X(20)! */
#if 0
	if (doesn_t_contain_X_20(definition)) {
#endif
		snprintf (buff, sizeof (buff), "%2.2d/%2.2d/%2.2d%2.2d.%2.2d.%2.2d",
			(cob_u16_t) current_compile_time.day_of_month,
			(cob_u16_t) current_compile_time.month,
			(cob_u16_t) current_compile_time.year % 100,
			(cob_u16_t) current_compile_time.hour,
			(cob_u16_t) current_compile_time.minute,
			(cob_u16_t) current_compile_time.second);
		lit_size = 16;
#if 0
	} else {
		snprintf (buff, sizeof (buff) + 1, "%2.2d\.%2.2d\.%2.2d%s %2.2d, %4.4d",
			(cob_u16_t) current_compile_time.hour,
			(cob_u16_t) current_compile_time.minute,
			(cob_u16_t) current_compile_time.second,
			(cob_u16_t) current_compile_time.month,
			(cob_u16_t) current_compile_time.day_of_month,
			(cob_u16_t) current_compile_time.year);
		lit_size = 20;
	}
#endif
	cb_build_constant_register (cb_build_reference (name),
		cb_build_alphanumeric_literal (buff, lit_size));
}

/* save external definition that is able to be parsed for later */
static void
add_to_register_list (struct external_defined_register** list, const char *name, const char *definition)
{
	struct external_defined_register *reg;
	reg = cobc_main_malloc (sizeof (struct external_defined_register));
	reg->next = *list;
	reg->name = cobc_main_strdup (name);
	reg->definition = cobc_main_strdup (definition);
	*list = reg;
}

/* moves word from sentence buffer to word bufffer,
   replacing it in sentence buffer to spaces,
   ignores leading spaces,
   returns length of word */
static size_t
extract_next_word_from_buffer (char *sentence, char *word)
{
	char *p = sentence, *r;
	while (*p == ' ') p++;

	r = p;
	while (*r != 0 && *r != ' ') r++;
	memcpy (word, p, r - p);
	word [r - p] = 0;
	memset (sentence, ' ', r - sentence);
	return r - p;
}

/* General register creation from specified or default definition
   (may not contain tabs between clauses);
   used for TALLY, LIN, COL;
   stores the resulting field's address in the optional last parameter */
/* TODO: test - and possibly complete change to generic function;
         currently missing: USAGE (either specify "DISPLAY" or get BINARY) */
int
cb_build_generic_register (const char *name, const char *external_definition,
	struct cb_field **result_field)
{
	/*
	   implementation note: this function is called in two scenarios:
	   1 - free definition outside of parsing tree (current_program != NULL);
	       in this case (config / command line) we do the complete parsing
	       here (for error messages), but can't do full field validation and
	       don't end up storing the prepared field somewhere;
	       instead the string we know can be fully parsed is cached
	   2 - building within parsing tree; in this case the result is added
	       to the current working storage
	*/
	struct cb_field	*field;
	char	definition[COB_MINI_BUFF] = { 0 };
	char	word[COB_MINI_BUFF];
	char	*p;
	size_t	def_len, word_len;
	int 	ret;

	if (!external_definition) {
		external_definition = cb_get_register_definition (name);
	}
	if (!external_definition || !external_definition[0]) {
		if (result_field) {
			*result_field = NULL;
		}
		cb_error ("missing definition for special register '%s'", name);
		return 1;
	}

	def_len = strlen (external_definition);
	if (def_len > COB_MINI_MAX) {
		cb_error ("unexpected definition for special register '%s', "
			"too long: %s", name, external_definition);
		def_len = COB_MINI_MAX;	/* still parse up to max */
	}
	memcpy (definition, external_definition, def_len);

	/* check for GLOBAL, leave if we don't need to define it again (nested program) */
	p = strstr (definition, "GLOBAL");
	if (p && (*(p + 6)  == ' ' || *(p + 6) == 0)) {
		if (current_program && current_program->nested_level) {
			/* in this case the program creation should have copied the reference */
			/* TODO: test pending */
			return 0;
		}
		memset (p, ' ', 6);	/* remove from local copy */
	}

	/* actual field generation */
	ret = 0;
	field = CB_FIELD (cb_build_field (cb_build_reference (name)));
	field->flag_is_global = (p != NULL);		/* any GLOBAL found ? */
	field->level = 77;

	/* handle USAGE */
	p = strstr (definition, "USAGE ");
	if (p) {
		enum cb_usage	usage;
		memset (p, ' ', 5);
		p += 6;
		word_len = extract_next_word_from_buffer (p, word);
		if (word_len == 7 && memcmp (word, "DISPLAY", 7) == 0) {
			usage = CB_USAGE_DISPLAY;
		} else {
			/* FIXME: parse actual USAGE from temp */
			usage = CB_USAGE_BINARY;
		}
		field->usage = usage;
#if 0
	} else {
		/* CHECKME: Should this be derived from PIC? */
		/* note: default usage from cb_build_field is CB_USAGE_DISPLAY */
#endif
	}

	/* handle PICTURE */
	field->pic = NULL;
	p = strstr (definition, "PIC ");
	if (p) {
		memset (p, ' ', 3);
		p += 4;
	} else {
		p = strstr (definition, "PICTURE ");
		if (p) {
			memset (p, ' ', 7);
			p += 8;
		}
	}
	if (p) {
		const enum cb_warn_val backup = get_warn_opt_value (cb_warn_unfinished);
		(void)extract_next_word_from_buffer (p, word);
		set_warn_opt_value (cb_warn_unfinished, COBC_WARN_DISABLED);
		field->pic = cb_build_picture (word);
		set_warn_opt_value (cb_warn_unfinished, backup);
		if (field->pic->size == 0) {
			ret = 1;
		}
	}

	/* CHECKME: Is PIC calculated from VALUE later on if empty? */

	/* handle ANY LENGTH / ANY NUMERIC (automatic: read-only) */
	p = strstr (definition, "ANY ");
	if (p) {
		field->storage = CB_STORAGE_LINKAGE;
		field->level = 01;
		word_len = extract_next_word_from_buffer (p + 4, word);
		if (word_len == 6 && memcmp (word, "LENGTH", 6) == 0) {
			memset (p, ' ', 3);
			field->flag_any_length = 1;
		} else
		if (word_len == 7 && memcmp (word, "NUMERIC", 7) == 0) {
			memset (p, ' ', 3);
			field->flag_any_length = 1;
			field->flag_any_numeric = 1;
		}
	}

	/* handle VALUE */
	p = strstr (definition, "VALUE ");
	if (p) {
		memset (p, ' ', 5);
		p += 6;
	}
	if (p) {
		cb_tree	lit = NULL;
		char *p2;

		while (*p == ' ') p++;

		/* alphanumeric / national / boolean literal, possibly in hex */
		p2 = (*p == 'N' || *p == 'B') ? p + 1 : p;
		if (*p2 == 'X') p2++;
		if (*p2 == '"' || *p2 == '\'') {
			char	*sep = strchr (p2 + 1, *p2);
			if (sep == NULL) {
				/* closing quote missing */
				cb_error ("unexpected definition for special register '%s', "
					"not parsed: VALUE %s", name, p);
				ret = 1;
				memset (p, ' ', strlen (p));
		} else if (p2 == p) {
				/* plain alphanumeric literal */
				if (current_program) {
					lit = cb_build_alphanumeric_literal (p + 1, sep - 1 - p);
				}
				memset (p, ' ', sep - p);
			} else {
				/* on the first run we don't add anything to the actual parse tree */
#if 0			/* TODO: move literal building out of scanner.l and call here */
				if (current_program) {
					lit = cb_build_some_literal (p, sep);
				}
#else
				*(sep + 1) = 0;
				cb_error ("unexpected definition for special register '%s', "
					"not parsed: VALUE %s", name, p);
				ret = 1;
#endif
				memset (p, ' ', sep - p);

			}
		} else {
			word_len = extract_next_word_from_buffer (p, word);
			/* note: without current_program cb_zero and friends will be
			         either NULL or point to invalid memory - that's no
			         problem as we only want do do the parsing in this case */
			if ((word_len == 4 && memcmp (word, "ZERO", 4) == 0)
			 || (word_len == 5 && memcmp (word, "ZEROS", 5) == 0)
			 || (word_len == 6 && memcmp (word, "ZEROES", 6) == 0)) {
				lit = cb_zero;
			} else
			if ((word_len == 5 && memcmp (word, "SPACE", 5) == 0)
			 || (word_len == 6 && memcmp (word, "SPACES", 6) == 0)) {
				lit = cb_space;
			}
			else
			if (word_len == 4 && memcmp (word, "NULL", 4) == 0) {
				lit = cb_null;
			} else
			if ((word_len == 5 && memcmp (word, "QUOTE", 5) == 0)
			 || (word_len == 6 && memcmp (word, "QUOTES", 6) == 0)) {
				lit = cb_quote;
			}
			else
			if ((word_len == 9 && memcmp (word, "LOW-VALUE", 9) == 0)
			 || (word_len == 10 && memcmp (word, "LOW-VALUES", 10) == 0)) {
				lit = cb_low;
			}
			else
			if ((word_len == 10 && memcmp (word, "HIGH-VALUE", 10) == 0)
			 || (word_len == 11 && memcmp (word, "HIGH-VALUES", 11) == 0)) {
				lit = cb_high;
			} else {
				for (p2 = word; *p2; p2++) {
					if (*p2 < '0' || *p2 > '9') {
						ret = 2;
						break;
					}
				}
				if (ret == 2) {
					cb_error ("unexpected definition for special register '%s', "
							"not parsed: VALUE %s", name, word);
				} else {
					/* on the first run we don't add anything to the actual parse tree */
					if (current_program) {
						lit = cb_build_numeric_literal (0, word, 0);
					}
				}
			}
		}
		if (lit) {
			field->values = lit;
		}
	}

	/* handle CONSTANT */
	p = strstr (definition, "CONSTANT ");
	if (p) {
		memset (p, ' ', 8);
		p += 8;
		field->flag_internal_constant = 1;
	}

	/* check that the local definition is completely parsed -> spaces */
	{
		char *d, *e;
		for (d = definition, e = definition + def_len - 1; d != e; d++) {
			if (*d != ' ') {
				while (e != d && *e == ' ') *e-- = 0; /* drop trailing spaces */
				cb_error ("unexpected definition for special register '%s', "
					"not parsed: %s", name, d);
				ret = 1;
				break;
			}
		}
	}

	if (ret) {
		field->flag_invalid = 1;
	} else
	if (current_program) {
		const enum cb_warn_val backup = get_warn_opt_value (cb_warn_unfinished);
		/* note: the necessary tree items like cb_zero won't be available
		   without a program, and therefore full validation is not possible */
		field->flag_internal_register = 1;
		field->flag_no_init = 1;
		set_warn_opt_value (cb_warn_unfinished, COBC_WARN_DISABLED);
		cb_validate_field (field);
		set_warn_opt_value (cb_warn_unfinished, backup);
	}

	if (field->flag_invalid) {
		return 1;
 	}

	if (current_program) {
		if (field->storage == CB_STORAGE_LINKAGE) {
			CB_FIELD_ADD (current_program->linkage_storage, field);
		} else
		if (field->storage == CB_STORAGE_LOCAL) {
			CB_FIELD_ADD (current_program->local_storage, field);
		} else {
			CB_FIELD_ADD (current_program->working_storage, field);
		}
	} else if (field->flag_is_global) {
		add_to_register_list (&external_defined_fields_global, name, external_definition);
	} else {
		add_to_register_list (&external_defined_fields_ws, name, external_definition);
	}
	if (result_field) {
		*result_field = field;
	}

	return 0;
}

static COB_INLINE COB_A_INLINE struct cb_field *
cb_build_generic_register_field (const char *name, const char *external_definition)
{
	struct cb_field *field = NULL;
	cb_build_generic_register (name, external_definition, &field);
	return field;
}

static struct cb_field *
cb_build_register_internal_code (const char* name, const char* definition)
{
	cb_tree tfield;
	struct cb_field *field;

	/* take care of GLOBAL */
	if (current_program->nested_level) {
		return NULL;
	}

	if (!definition) {
		definition = cb_get_register_definition (name);
		if (!definition) {
			return NULL;
		}
	}

	tfield = cb_build_field (cb_build_reference (name));
	field = CB_FIELD (tfield);
	field->usage = CB_USAGE_BINARY;
	field->pic = cb_build_picture ("S9(9)");
	cb_validate_field (field);
	field->values = cb_zero;
	field->flag_no_init = 1;
	field->flag_is_global = 1;
	field->flag_internal_register = 1;
	field->level = 77;

	return field;
}


/* build a concrete register */
static void
cb_build_single_register (const char *name, const char *definition)
{
	/* TODO: parse definition here or in sub-functions */

	/* registers that are currently created elsewhere
	   TODO: move them here */
	/* FIXME: LENGTH OF (must have different results depending on compiler configuration) */
	if (!cb_strcasecmp (name, "ADDRESS OF")
	 || !cb_strcasecmp (name, "LENGTH OF")
	 || !cb_strcasecmp (name, "COB-CRT-STATUS")
	 || !cb_strcasecmp (name, "DEBUG-ITEM")) {
		return;
	}

	/* registers that need a special handling / internal registration */
	if (!cb_strcasecmp (name, "RETURN-CODE")) {
		cb_build_register_return_code (name, definition);
		return;
	}
	if (!cb_strcasecmp (name, "SORT-RETURN")) {
		cb_build_register_sort_return (name, definition);
		return;
	}
	if (!cb_strcasecmp (name, "NUMBER-OF-CALL-PARAMETERS")) {
		cb_build_register_number_parameters (name, definition);
		return;
	}
	if (!cb_strcasecmp (name, "WHEN-COMPILED")) {
		cb_build_register_when_compiled (name, definition);
		return;
	}
	if (!cb_strcasecmp (name, "XML-CODE")) {
		current_program->xml_code = cb_build_register_internal_code (name, definition);
		return;
	}
	if (!cb_strcasecmp (name, "XML-EVENT")) {
		current_program->xml_event = cb_build_generic_register_field (name, definition);
		return;
	}
	if (!cb_strcasecmp (name, "XML-INFORMATION")) {
		current_program->xml_information = cb_build_register_internal_code (name, definition);
		return;
	}
	if (!cb_strcasecmp (name, "XML-TEXT")) {
		current_program->xml_text = cb_build_generic_register_field (name, definition);
		return;
	}
	if (!cb_strcasecmp (name, "XML-NTEXT")) {
		current_program->xml_ntext = cb_build_generic_register_field (name, definition);
		return;
	}
	if (!cb_strcasecmp (name, "XML-NAMESPACE")) {
		current_program->xml_namespace = cb_build_generic_register_field (name, definition);
		return;
	}
	if (!cb_strcasecmp (name, "XML-NNAMESPACE")) {
		current_program->xml_nnamespace = cb_build_generic_register_field (name, definition);
		return;
	}
	if (!cb_strcasecmp (name, "XML-NAMESPACE-PREFIX")) {
		current_program->xml_namespace_prefix = cb_build_generic_register_field (name, definition);
		return;
	}
	if (!cb_strcasecmp (name, "XML-NNAMESPACE-PREFIX")) {
		current_program->xml_nnamespace_prefix = cb_build_generic_register_field (name, definition);
		return;
	}
	if (!cb_strcasecmp (name, "JSON-CODE")) {
		current_program->json_code =  cb_build_register_internal_code (name, definition);
		return;
	}
	if (!cb_strcasecmp (name, "JSON-STATUS")) {
		current_program->json_status = cb_build_register_internal_code (name, definition);
		return;
	}

	/* "normal" registers */
	if (!cb_strcasecmp (name, "TALLY")
	 || !cb_strcasecmp (name, "LIN")
	 || !cb_strcasecmp (name, "COL")) {
		cb_build_generic_register (name, definition, NULL);
		return;
	}

	/* LCOV_EXCL_START */
	/* This should never happen (and therefore doesn't get a translation) */
	cb_error ("unexpected special register %s, defined as \"%s\"", name, definition);
	COBC_ABORT();
	/* LCOV_EXCL_STOP */
}

/* get all active registers and build them */
void
cb_build_registers (void)
{
	const char *name, *definition = NULL;

	name = cb_register_list_get_first (&definition);
	while (name) {
		cb_build_single_register (name, definition);
		name = cb_register_list_get_next (&definition);
	}
}

/* add registers defined externally (configuration/compiler option) */
void
cb_add_external_defined_registers (void)
{
	/* in this case we have a list of entries and need to reparse these,
	   to add the fields both to the program's working storage and word list */
	struct external_defined_register* list;

	if (external_defined_fields_ws) {
		for (list = external_defined_fields_ws; list; list = list->next) {
			cb_build_generic_register (list->name, list->definition, NULL);
		}
	}
	if (external_defined_fields_global && !current_program->nested_level) {
		for (list = external_defined_fields_global; list; list = list->next) {
			cb_build_generic_register (list->name, list->definition, NULL);
		}
	}
}

/*
  TODO: build on first reference (we have the compile time which is the reason
  that it was placed here in the first place available fixed in
  current_compile_time now).
*/
void
cb_set_intr_when_compiled (void)
{
	char	buff[36]; /* 36: make the compiler happy as "unsigned short" *could*
						     have more digits than we "assume" */
	cob_u16_t	offset_minutes;

	snprintf (buff, sizeof (buff), "%4.4d%2.2d%2.2d%2.2d%2.2d%2.2d%2.2d",
		(cob_u16_t) current_compile_time.year,
		(cob_u16_t) current_compile_time.month,
		(cob_u16_t) current_compile_time.day_of_month,
		(cob_u16_t) current_compile_time.hour,
		(cob_u16_t) current_compile_time.minute,
		(cob_u16_t) current_compile_time.second,
		(cob_u16_t) (current_compile_time.nanosecond / 10000000));
	if (current_compile_time.offset_known) {
		if (current_compile_time.utc_offset >= 0) {
			offset_minutes = current_compile_time.utc_offset % 60;
		} else {
			offset_minutes = -current_compile_time.utc_offset % 60;
		}
		snprintf (buff + 16, (size_t)11, "%+2.2d%2.2d",	/* 11: see above */
			  (cob_s16_t) current_compile_time.utc_offset / 60,
			  offset_minutes);
	} else {
		snprintf (buff + 16, (size_t)6, "00000");
	}
	cb_intr_whencomp = cb_build_alphanumeric_literal (buff, (size_t)21);
}

/* check program-id literal and trim, if necessary */
void
cb_trim_program_id (cb_tree id_literal)
{
	char	*s;
	cob_u32_t	len;

	s = (char *) (CB_LITERAL (id_literal)->data);
	if (!strchr (s, ' ')) {
		return;
	}

	len = (cob_u32_t) strlen (s);
	if (*s == ' ') {
		/* same warning as in libcob/common.c */
		cb_warning_x (COBC_WARN_FILLER, id_literal,
			_("'%s' literal includes leading spaces which are omitted"), s);
	}
	if (s[len - 1] == ' ') {
		cb_warning_x (cb_warn_additional, id_literal,
			_("'%s' literal includes trailing spaces which are omitted"), s);
	}
	while (*s == ' ') {
		memmove (s, s + 1, len--);
	}
	while (s[len - 1] == ' ' && len > 0) {
		len--;
	}
	s[len] = 0;
	CB_LITERAL (id_literal)->size = len;
}

/** encode given name
  \param name to encode
  \param strip_path specifying if name may include directory which
         should be stripped in the encoded version
  \return pointer to encoded name
 */
char *
cb_encode_program_id (const char *name, const int strip_path, const int fold_case)
{
	const unsigned char	*s = (const unsigned char *)name;
	unsigned char		buff[COB_MINI_BUFF];

	/* position after last path separator (included for CALL) */
	if (strip_path) {
		const unsigned char	*t;
		for (t = s + strlen (name); t > s; t--) {
			if (*t == (unsigned char)'/' || *t == (unsigned char)'\\') {
				s = t + 1;
				break;
			}
		}
	}

	/* Encode program name, including case folding */
	cob_encode_program_id ((unsigned char *)name, buff, COB_MINI_MAX, fold_case);

	return cobc_check_string ((char *)buff);
}

char *
cb_build_program_id (const char *name, const cob_u32_t is_func)
{
	/* always convert function names to upper case */
	const int	folding = is_func ? COB_FOLD_UPPER : cb_fold_call;

	/* checking for valid name, the error raised there is enough to stop
	   the generation, therefore we ignore the result */
	(void)cobc_check_valid_name (name, PROGRAM_ID_NAME);

	/* Set and encode the PROGRAM-ID */
	current_program->orig_program_id = (char *) name;
	return cb_encode_program_id (name, 0, folding);
}

cb_tree
cb_define_switch_name (cb_tree name, cb_tree sname, const int flag)
{
	cb_tree		switch_id;
	cb_tree		value;

	if (!name || name == cb_error_node) {
		return NULL;
	}
	if (!sname || sname == cb_error_node ||
	    CB_SYSTEM_NAME (sname)->category != CB_SWITCH_NAME) {
		cb_error_x (name, _("ON/OFF usage requires a SWITCH name"));
		return NULL;
	}
	switch_id = cb_int (CB_SYSTEM_NAME (sname)->token);
	value = CB_BUILD_FUNCALL_1 ("cob_get_switch", switch_id);
	if (flag == 0) {
		value = CB_BUILD_NEGATION (value);
	}
	cb_build_constant (name, value);
	return value;
}

void
cb_check_word_length (unsigned int length, const char *word)
{
	if (length > cb_word_length) {
		if (length > COB_MAX_WORDLEN) {
			/* Absolute limit */
			cb_error (_("word length exceeds maximum of %d characters: '%s'"),
				  COB_MAX_WORDLEN, word);
		} else {
			(void) cb_syntax_check (_("word length exceeds %d characters: '%s'"),
						cb_word_length, word);
		}
	}
}

cb_tree
cb_build_section_name (cb_tree name, const int sect_or_para)
{
	cb_tree x;
	struct cb_word	*w;
	int			nwlength;

	if (name == cb_error_node) {
		return cb_error_node;
	}

	cb_check_reset ();
	/* Check word length
	needed here for numeric-only words that bypass the checks
	in scanner.l */
	w = CB_REFERENCE (name)->word;
	for (nwlength = 0; w->name[nwlength] != 0; nwlength++) {
		if (!isdigit ((int)w->name[nwlength])) {
			nwlength = 0;
			break;
		}
	}
	if (nwlength > 0) {
		cb_check_word_length(nwlength, w->name);
	}

	if (CB_WORD_COUNT (name) > 0) {
		x = CB_VALUE (CB_WORD_ITEMS (name));
		/*
		  Used as a non-label name or used as a section name.
		  Duplicate paragraphs are allowed if not referenced;
		  Checked in typeck.c
		*/
		if (!CB_LABEL_P (x) || sect_or_para == 0 ||
		    (sect_or_para && CB_LABEL_P (x) &&
		    CB_LABEL (x)->flag_section)) {
			redefinition_error (name);
			return cb_error_node;
		}
	}

	return name;
}

static const char *
remove_labels_from_filename (const char *name_ptr)
{
	const char	*p = NULL;

	p = strrchr (name_ptr, '-');
	if (p) {
		return p + 1;
	} else {
		return name_ptr;
	}
}

/*
  Build name for ASSIGN EXTERNAL: convert the word in the ASSIGN clause into
  a literal.
 */
static cb_tree
build_external_assignment_name (cb_tree name)
{
	const char	*name_ptr;
	const char	*orig_ptr;

	name_ptr = orig_ptr = CB_NAME (name);

	/* Remove (and warn about) labels */
	name_ptr = remove_labels_from_filename (name_ptr);
	if (name_ptr != orig_ptr) {
		cb_warning (cb_warn_additional, _("ASSIGN %s interpreted as '%s'"),
			orig_ptr, name_ptr);
	}

	/* Convert the EXTERNAL name into literal */
	return cb_build_alphanumeric_literal (name_ptr, strlen (name_ptr));
}

/* build name for ASSIGN, to be resolved later as we don't have any
   field info at this point (postponed to cb_validate_program_data) */
cb_tree
cb_build_assignment_name (struct cb_file *cfile, cb_tree name)
{
	if (name == cb_error_node) {
		return cb_error_node;
	}
	/* For special assignment */
	if (name == NULL) {
		return NULL;
	}

	if (CB_LITERAL_P (name)) {
		return name;
	}

	if (!CB_REFERENCE_P (name)) {
		return cb_error_node;
	}

	if (cfile->assign_type == CB_ASSIGN_EXT_FILE_NAME_REQUIRED) {
		return build_external_assignment_name (name);
	} else {
		const char	*name_ptr = CB_NAME (name);
		/* handle name as literal if it matches the file name */
		if (strcmp (name_ptr, cfile->name) == 0) {
			return cb_build_alphanumeric_literal (name_ptr, strlen (name_ptr));
		}
		current_program->reference_list =
			cb_list_add (current_program->reference_list, name);
		return name;
	}
}

cb_tree
cb_build_index (cb_tree x, cb_tree values, const unsigned int indexed_by,
		struct cb_field *qual)
{
	enum cb_storage		storage = CB_STORAGE_WORKING;
	struct cb_field	*f = CB_FIELD (cb_build_field (x));

	/* TODO: possibly second type which is 0-based, depending on dialect option,
	   see FR #428 */
	f->usage = CB_USAGE_INDEX;
	cb_validate_field (f);
	f->values = values;
	f->index_qual = qual;
	f->flag_indexed_by = !!indexed_by;
	if (f->flag_indexed_by) {
		f->flag_real_binary = 1;
	}
	if (qual) {
		storage = qual->storage;
	}
	switch (storage) {
	case CB_STORAGE_FILE:
	case CB_STORAGE_WORKING:
		CB_FIELD_ADD (current_program->working_storage, f);
		break;
	case CB_STORAGE_LINKAGE:
		/* explicit: not passed -> program local -> WS / LO */
		if (current_program->flag_recursive) {
			CB_FIELD_ADD (current_program->local_storage, f);
		} else {
			CB_FIELD_ADD (current_program->working_storage, f);
		}
		break;
	case CB_STORAGE_SCREEN:
		CB_FIELD_ADD (current_program->screen_storage, f);
		break;
	case CB_STORAGE_REPORT:
		CB_FIELD_ADD (current_program->report_storage, f);
		break;
	case CB_STORAGE_LOCAL:
		CB_FIELD_ADD (current_program->local_storage, f);
		break;
	/* LCOV_EXCL_START */
	default:
		cobc_err_msg ("unexpected register storage: %d", storage);
		return cb_error_node;
	/* LCOV_EXCL_STOP */		
	}
	return x;
}

cb_tree
cb_build_address (cb_tree x)
{
	cb_tree			v;
	struct cb_reference	*r;
	const char		*name;
	unsigned int	numsubs, refsubs;

	if (x == cb_error_node) {
		return cb_error_node;
	}
	if (!CB_REFERENCE_P (x)) {
		return CB_BUILD_CAST_ADDRESS (x);
	}

	r = CB_REFERENCE (x);
	name = r->word->name;
	v = cb_ref (x);
	if (v == cb_error_node) {
		return cb_error_node;
	}

	refsubs = cb_list_length (r->subs);
	if (CB_FIELD_P (v)) {
		numsubs = CB_FIELD (v)->indexes;
		if (refsubs > numsubs) {
			goto subserror;
		} else if (refsubs < numsubs) {
			if (!cb_relaxed_syntax_checks) {
				goto subserror;
			} else {
				cb_warning_x (COBC_WARN_FILLER, x,
					    _("subscript missing for '%s' - defaulting to 1"),
					    name);
				for (; refsubs < numsubs; ++refsubs) {
					CB_ADD_TO_CHAIN (cb_one, r->subs);
				}
			}
		}
	} else {
		numsubs = 0;
		if (r->subs) {
			goto subserror;
		}
		if (r->offset) {
			cb_error_x (x, _("'%s' cannot be reference modified"), name);
			return cb_error_node;
		}
	}

	return CB_BUILD_CAST_ADDRESS (x);

subserror:
	switch (numsubs) {
	case 0:
		cb_error_x (x, _("'%s' cannot be subscripted"), name);
		break;
	case 1:
		/* FIXME: Change to P_, needs changes to Makevars and tests */
		cb_error_x (x, _("'%s' requires one subscript"), name);
		break;
	default:
		cb_error_x (x, _("'%s' requires %d subscripts"),
			    name, numsubs);
		break;
	}
	return cb_error_node;
}

/* return a reference for a given field combination, needed for calls to CB_FUNC_CALL
   as the string would not be allocated during codegen otherwise */
static cb_tree
cb_build_name_reference (struct cb_field *f1, struct cb_field *f2)
{
	char		full_name[COB_MAX_WORDLEN * 2 + 10];
	if (f1 == f2) {
		/* TRANSLATORS: This msgid is used when a variable name
		   or label is referenced in a compiler message. */
		sprintf(full_name, _("'%s'"), f1->name);
	} else {
		sprintf(full_name, _("'%s' (accessed by '%s')"), f1->name, f2->name);
	}

	return cb_build_reference (full_name);
}

cb_tree
cb_build_identifier (cb_tree x, const int subchk)
{
	struct cb_reference	*r;
	struct cb_field		*f;
	struct cb_field		*p;
	const char		*name;
	cb_tree			v;
	cb_tree			e1;
	cb_tree			l;
	cb_tree			sub;
	int			offset;
	int			length;
	int			n;
	int			numsubs;
	int			refsubs;
	int			pseudosize;

	if (x == cb_error_node) {
		return cb_error_node;
	}

	r = CB_REFERENCE (x);
	name = r->word->name;

	/* Resolve reference */
	v = cb_ref (x);
	if (v == cb_error_node) {
		return cb_error_node;
	}

	/* Check if it is a data name */
	if (!CB_FIELD_P (v)) {
		if (r->subs) {
			cb_error_x (x, _("'%s' cannot be subscripted"), name);
			return cb_error_node;
		}
		if (r->offset) {
			cb_error_x (x, _("'%s' cannot be reference modified"), name);
			return cb_error_node;
		}
		return x;
	}
	f = CB_FIELD (v);

	/* BASED check and check for OPTIONAL LINKAGE items */

	/* CHECKME: do we need the field founder to decide?  LINKAGE and flag_item_based
	            should be available in 'f' already ... */
	if (current_statement && !suppress_data_exceptions
	 && ( CB_EXCEPTION_ENABLE (COB_EC_DATA_PTR_NULL)
	   || CB_EXCEPTION_ENABLE (COB_EC_PROGRAM_ARG_OMITTED))) {
		p = cb_field_founder (f);
		if (p->redefines) {
			p = p->redefines;
		}
		if (CB_EXCEPTION_ENABLE (COB_EC_PROGRAM_ARG_OMITTED)
		 && p->storage == CB_STORAGE_LINKAGE
		 && p->flag_is_pdiv_parm
#if 0
		/* note: we can only ignore the check for fields with flag_is_pdiv_opt
		   when we check for COB_EC_PROGRAM_ARG_MISMATCH in all entry points
		   and this check is currently completely missing... */
		 && !(p->flag_is_pdiv_opt && CB_EXCEPTION_ENABLE (COB_EC_PROGRAM_ARG_MISMATCH))
#endif
		 ) {
			cb_add_null_check ("cob_check_linkage", p, f);
			optimize_defs[COB_CHK_LINKAGE] = 1;
		} else
		if (CB_EXCEPTION_ENABLE (COB_EC_DATA_PTR_NULL)
		 && !current_statement->flag_no_based) {
			if (p->flag_item_based
			 || (p->storage == CB_STORAGE_LINKAGE 
			  && !(p->flag_is_pdiv_parm 
			    || p->flag_is_returning))) {
				cb_add_null_check ("cob_check_based", p, f);
				optimize_defs[COB_CHK_BASED] = 1;
			}
		}
	}

	for (l = r->subs; l; l = CB_CHAIN (l)) {
		cb_tree val = CB_VALUE (l);
		if (CB_BINARY_OP_P (val)) {
			/* Set special flag for codegen */
			CB_BINARY_OP (val)->flag = BOP_RESOLVE_AS_INTEGER;
		}
	}

	/* Check the number of subscripts */
	numsubs = refsubs = cb_list_length (r->subs);
	cb_check_lit_subs (r, numsubs, f->indexes);
	if (subchk) {
		if (!f->indexes) {
			cb_error_x (x, _("'%s' has no OCCURS clause"), name);
			return cb_error_node;
		}
		numsubs = f->indexes - 1;
	} else {
		numsubs = f->indexes;
	}
	if (!r->flag_all) {
		if (refsubs != numsubs) {
			if (refsubs > numsubs) {
				goto refsubserr;
			} else if (refsubs < numsubs) {
				if (!cb_relaxed_syntax_checks) {
					goto refsubserr;
				} else {
					cb_warning_x (COBC_WARN_FILLER, x,
							_("subscript missing for '%s' - defaulting to 1"),
							name);
					for (; refsubs < numsubs; ++refsubs) {
						CB_ADD_TO_CHAIN (cb_one, r->subs);
					}
				}
			}
		}

		/* Run-time check for ODO (including all the fields' subordinate items),
		   FIXME: this should only be done "once" per ODO and statement, but
		   if the statement is a list it is done multiple times:
			 77 XX PIC 99 VALUE 5.
			 01 X         PIC X OCCURS 0 TO 10 DEPENDING ON XX.
			   MOVE ZERO TO X(2) X(4) X(6) X(8) X(10) X(1)
		   --> currently generated for each of the 6 "X" items, as we (need to)
		       call this function 6 times from parser (target_identifier)
		   --> either cache field name here (dropping after each statement)
		       or remove/skip later during codegen
		   */
		if (CB_EXCEPTION_ENABLE (COB_EC_BOUND_SUBSCRIPT) && f->odo_level != 0) {
			for (p = f; p; p = p->children) {
				if (CB_VALID_TREE (p->depending)
				    /* Do not check length for implicit access to
				       a PIC L field (i.e, via enclosing group),
					   as those disregard the DEPENDING.
				       However, assuming the filler is never explicitly
				       accessed (p == f), check is still done for explicit
				       access to PIC L field (p->parent == f). */
				 && (!p->parent || p->parent == f || !p->parent->flag_picture_l)
				 && !p->flag_unbounded) {
					e1 = cb_add_check_odo (p);
					if (e1 != NULL) {
						optimize_defs[COB_CHK_ODO] = 1;
						r->check = cb_list_add (r->check, e1);
					}
				}
			}
		}

		/* Subscript check along with setting of table offset */
		if (r->subs &&! cb_validate_list (r->subs)) {
			l = r->subs;
			for (p = f; p && l; p = p->parent) {
				if (!p->flag_occurs) {
					continue;
				}
				sub = cb_check_integer_value (CB_VALUE (l));
				l = CB_CHAIN (l);
				if (sub == cb_error_node) {
					continue;
				}

				/* Compile-time check for all literals */
				if (CB_LITERAL_P (sub)) {
					n = cb_get_int (sub);
					if (n < 1 || (!p->flag_unbounded && n > p->occurs_max)) {
						if (cb_syntax_check_x (x, _("subscript of '%s' out of bounds: %d"),
								       name, n)) {
							continue;	/* *skip runtime check, as MF does */
						}
					}
				}

				/* Run-time check for all non-literals */
				if (CB_EXCEPTION_ENABLE (COB_EC_BOUND_SUBSCRIPT)) {
					if (p->depending && p->depending != cb_error_node) {
						e1 = cb_add_check_subscript (p, sub, name, 1);
						if (e1 != NULL) {
							optimize_defs[COB_CHK_SUBSCRIPT] = 1;
							r->check = cb_list_add (r->check, e1);
						}
					} else {
						if (!CB_LITERAL_P (sub)) {
							e1 = cb_add_check_subscript (p, sub, name, 0);
							if (e1 != NULL) {
								optimize_defs[COB_CHK_SUBSCRIPT] = 1;
								r->check = cb_list_add (r->check, e1);
							}
						}
					}
				}
			}
		}
	}

	if (subchk) {
		r->subs = cb_list_reverse (r->subs);
		r->subs = cb_list_add (r->subs, cb_int1);
		r->subs = cb_list_reverse (r->subs);
	}

	/* Reference modification check */
#if 0 /* CHECKME: if active, then one test fails (field), if not another (group)...
                  it seems both are different checks, do we need a flag? */
	if (cb_reference_bounds_check == CB_WARNING
	 || cb_reference_bounds_check == CB_OK) {
		p = cb_field_founder (f);
		if (p != f) {
			pseudosize = p->size - f->offset;	/* Remaining size of group item */
		}
	}
#endif
	if (f->flag_any_length) {
		pseudosize = 0 - f->size;
	} else {
		if (f->usage == CB_USAGE_NATIONAL) {
			pseudosize = f->size / 2;
		} else if (f->pic && f->pic->orig && f->pic->orig[0] == 'U') {
			pseudosize = f->size / 4;
		} else {
			/* note: child elements under UNBOUNDED are not included! */
			pseudosize = f->size;
		}
		if (cb_field_has_unbounded (f)) {
			pseudosize *= -1;
		}
	}
	if (r->offset) {
		/* Compile-time check */
		if (CB_LITERAL_P (r->offset)
	 	 && !cb_is_field_unbounded (f)) {
			offset = cb_get_int (r->offset);
			if (pseudosize < 0) {
				if (offset < 1) {
					cb_error_x (x, _("offset must be greater than zero"));
				} else if (r->length && CB_LITERAL_P (r->length)) {
					length = cb_get_int (r->length);
					/* FIXME: needs to be supported for zero length literals */
					if (length < 1) {
						cb_error_x (x, _("length must be greater than zero"));
					}
				}
			} else {
				if (offset < 1) {
					cb_error_x (x, _("offset must be greater than zero"));
				} else if (offset > pseudosize) {
					if (cb_reference_bounds_check == CB_WARNING) {
						cb_warning_x (cb_warn_additional, x, _("offset of '%s' out of bounds: %d"), name, offset);
					} else
					if (cb_reference_bounds_check == CB_ERROR) {
						cb_error_x (x, _("offset of '%s' out of bounds: %d"), name, offset);
					}
				}
				if (r->length && CB_LITERAL_P (r->length)) {
					length = cb_get_int (r->length);
					/* FIXME: needs to be supported for zero length literals */
					if (length < 1) {
						cb_error_x (x, _("length must be greater than zero"));
					} else if ((length > pseudosize - offset + 1)
						&& (offset <= pseudosize && offset >= 1) ) {
						if (cb_reference_bounds_check == CB_WARNING) {
							cb_warning_x (cb_warn_additional, x, _("length of '%s' out of bounds: %d"),
								    name, length);
						} else
						if (cb_reference_bounds_check == CB_ERROR) {
							cb_error_x (x, _("length of '%s' out of bounds: %d"),
								    name, length);
						}
					}
				}
			}
		} else if (r->length && CB_LITERAL_P (r->length)
 	 		&& !cb_is_field_unbounded (f)) {
			length = cb_get_int (r->length);
			/* FIXME: needs to be supported for zero length literals */
			if (length < 1) {
				cb_error_x (x, _("length must be greater than zero"));
			} else if (pseudosize > 0 && pseudosize <= length) {
				if (cb_reference_bounds_check == CB_WARNING) {
					cb_warning_x (cb_warn_additional, x, _("length of '%s' out of bounds: %d"),
						    name, length);
				} else
				if (cb_reference_bounds_check == CB_ERROR) {
					cb_error_x (x, _("length of '%s' out of bounds: %d"),
						    name, length);
				}
			}
		}

		/* Run-time check */
		if (CB_EXCEPTION_ENABLE (COB_EC_BOUND_REF_MOD)) {
			if (f->flag_any_length
			 || cb_field_variable_size (f)
			 || !CB_LITERAL_P (r->offset)
			 || (r->length && !CB_LITERAL_P (r->length))) {
				cb_tree temp = NULL;
				if (cb_field_variable_size (f) ) {
					temp = cb_build_index (cb_build_filler (), NULL, 0, NULL);
					CB_FIELD (cb_ref (temp))->usage = CB_USAGE_LENGTH;
					CB_FIELD (cb_ref (temp))->count++;
					CB_FIELD (cb_ref (temp))->pic->have_sign = 0;	/* LENGTH is UNSIGNED */
					cb_emit (cb_build_assign (temp, cb_build_length_1 (cb_build_field_reference (f, NULL))));
				}
				/* allow everything but negative/zero */
				if (cb_ref_mod_zero_length == 2) {
					e1 = CB_BUILD_FUNCALL_3 ("cob_check_ref_mod_minimal",
								 CB_BUILD_STRING0 (f->name),
								 cb_build_cast_int (r->offset),
								 r->length ?
								  cb_build_cast_int (r->length) :
								  cb_int1);
					optimize_defs[COB_CHK_REFMOD_MIN] = 1;
				} else {
					/* check upper + size + lower as requested */
					e1 = CB_BUILD_FUNCALL_6 ("cob_check_ref_mod",
								 CB_BUILD_STRING0 (f->name),
								 cb_int1,	/* abend */
								 cb_int (cb_ref_mod_zero_length),
								 temp /* field is variable size */ ?
								  cb_build_cast_int (temp) :
								 f->flag_any_length ?
								  CB_BUILD_CAST_LENGTH (v) /* known via field.size */ :
								  pseudosize < 0 ?
								    CB_BUILD_CAST_LENGTH (x) /* needs to be runtime-calculated */ :
								    cb_int (pseudosize),
								 cb_build_cast_int (r->offset),
								 r->length ?
								  cb_build_cast_int (r->length) :
								  cb_int1);
					optimize_defs[COB_CHK_REFMOD] = 1;
				}
				r->check = cb_list_add (r->check, e1);
			}
		}
	}

	if (f->storage == CB_STORAGE_CONSTANT) {
		return f->values;
	}

	return x;

refsubserr:
	switch (numsubs) {
	case 0:
		cb_error_x (x, _("'%s' cannot be subscripted"), name);
		break;
	case 1:
		/* FIXME: Change to P_, needs changes to Makevars and tests */
		cb_error_x (x, _("'%s' requires one subscript"), name);
		break;
	default:
		cb_error_x (x, _("'%s' requires %d subscripts"),
			    name, f->indexes);
		break;
	}
	return cb_error_node;
}

static cb_tree
cb_build_length_1 (cb_tree x)
{
	struct cb_field *f;
	cb_tree		e;
	cb_tree		size;

	f = CB_FIELD (cb_ref (x));

	if (cb_field_variable_size (f) == NULL) {
		/* Constant size */
		return cb_int (cb_field_size (x));
	}
	/* Variable size */
	e = NULL;
	for (f = f->children; f; f = f->sister) {
		size = cb_build_length_1 (cb_build_field_reference (f, x));
		if (f->depending) {
			if (!cb_odoslide && f->flag_odo_relative) {
				size = cb_build_binary_op (size, '*', cb_int (f->occurs_max));
			} else {
				size = cb_build_binary_op (size, '*', f->depending);
			}
		} else if (f->occurs_max > 1) {
			size = cb_build_binary_op (size, '*', cb_int (f->occurs_max));
		}
		e = e ? cb_build_binary_op (e, '+', size) : size;
	}
	return e;
}

cb_tree
cb_build_const_length (cb_tree x)
{
	struct cb_field		*f;
	char			buff[32];

	if (CB_INVALID_TREE(x)) {
		return cb_error_node;
	}
	if (CB_INTEGER_P (x)) {
		sprintf (buff, FMT_LEN, CB_INTEGER(x)->val);
		return cb_build_numeric_literal (0, buff, 0);
	}
	if (CB_LITERAL_P (x)) {
		sprintf (buff, "%d", CB_LITERAL(x)->size);
		return cb_build_numsize_literal (buff, strlen(buff), 0);
	}
	if (CB_REFERENCE_P (x)) {
		if (cb_ref (x) == cb_error_node) {
			return cb_error_node;
		}
		if (CB_REFERENCE (x)->offset) {
			cb_error (_("reference modification not allowed here"));
			return cb_error_node;
		}
	} else if (!CB_FIELD_P (x)) {
		return cb_error_node;
	}

	f = CB_FIELD_PTR (x);
	cb_validate_field (f);
	if (f->flag_any_length) {
		cb_error (_("ANY LENGTH item not allowed here"));
		return cb_error_node;
	}
	if (f->level == 88) {
		cb_error (_("88 level item not allowed here"));
		return cb_error_node;
	}
	if (!cobc_in_procedure
	 && !cb_length_in_data_division) {
		cb_error_x (x,_("LENGTH OF '%s' not allowed outside of Procedure Division"),f->name);
		return cb_error_node;
	}
	if (cb_field_variable_size (f)) {
		cb_error (_("variable length item not allowed here"));
		return cb_error_node;
	}
	memset (buff, 0, sizeof (buff));
	if (f->redefines) {
		cb_validate_field (f->redefines);
		if (f->rename_thru) {
			cb_validate_field (f->rename_thru);
		}
		cb_validate_field (f);
		sprintf (buff, FMT_LEN, f->size);
	} else {
		cb_validate_field (f);
		sprintf (buff, FMT_LEN, f->memory_size);
	}
	return cb_build_numeric_literal (0, buff, 0);
}

cb_tree
cb_build_const_from (cb_tree x)
{
	struct cb_define_struct *p;

	if (x == cb_error_node) {
		return cb_error_node;
	}
	p = ppp_search_lists (CB_NAME(x));
	if (p == NULL
	 || p->deftype == PLEX_DEF_DEL) {
		cb_error (_("'%s' has not been DEFINEd"), CB_NAME(x));
		return cb_error_node;
	}

	if (p->deftype == PLEX_DEF_NUM) {
		return cb_build_numeric_literal (0, p->value, 0);
	} else {
		return cb_build_alphanumeric_literal (p->value, (size_t)strlen(p->value));
	}
}

/**
 * build numeric literal for level 78 VALUE START OF with the offset
 * of the given item
 *
 * Note: we don't return an error node even if an error occurs as this would
 * trigger a "needs a VALUE clause" error
 */
cb_tree
cb_build_const_start (struct cb_field *f, cb_tree x)
{
	struct cb_field		*target, *p;
	char			buff[32];

	if (x == cb_error_node) {
		return cb_error_node;
	}
	if (CB_REFERENCE_P (x)) {
		if (cb_ref (x) == cb_error_node) {
			return cb_error_node;
		}
		if (CB_REFERENCE (x)->offset) {
			cb_error (_("reference modification not allowed here"));
			return cb_build_numeric_literal (0, "1", 0);
		}
	} else {
		cb_error (_("only field names allowed here"));
		return cb_build_numeric_literal (0, "1", 0);
	}

	target = CB_FIELD (cb_ref (x));
	if (!target) {
		return cb_error_node;
	}
	if (!target->flag_external
	 && target->storage != CB_STORAGE_FILE
	 && target->storage != CB_STORAGE_LINKAGE) {
		cb_error (_("VALUE of '%s': %s target '%s' is invalid"),
					f->name, "START OF", target->name);
		cb_error (_("target must be in FILE SECTION or LINKAGE SECTION or have the EXTERNAL clause"));
		return cb_build_numeric_literal (0, "1", 0);
	}

	if (target->flag_any_length) {
		cb_error (_("ANY LENGTH item not allowed here"));
		return cb_build_numeric_literal (0, "1", 0);
	}
	if (target->level == 88) {
		cb_error (_("88 level item not allowed here"));
		return cb_build_numeric_literal (0, "1", 0);
	}
	if (cb_field_variable_size (target)) {
		cb_error (_("variable length item not allowed here"));
		return cb_build_numeric_literal (0, "1", 0);
	}
	for (p = target; p; p = p->parent) {
		p->flag_is_verified = 0;		/* Force redo compute_size */
		p->flag_invalid = 0;
		cb_validate_field (p);
		if (cb_field_variable_size (p)) {
			cb_error (_("variable length item not allowed here"));
			return cb_build_numeric_literal (0, "1", 0);
		}
	}
	snprintf (buff, sizeof(buff), "%d", target->offset);
	for (p = target; p; p = p->parent) {
		p->flag_is_verified = 0;		/* Force redo compute_size */
		p->flag_invalid = 0;
	}
	return cb_build_numeric_literal (0, buff, 0);
}

/**
 * build numeric literal for level 78 VALUE NEXT with the offset
 * at which the NEXT byte of storage occurs after the previous data declaration
 *
 * Important: this is NOT identical with START OF the next item as SYNC may
 * set a different offset for it and when the previous data declaration has
 * an OCCURS clause, the value returned by NEXT is the offset at which the next
 * byte of storage occurs *after the first element* of the table
 *
 * Note: we don't return an error node even if an error occurs as this would
 * trigger a "needs a VALUE clause" error
 */
cb_tree
cb_build_const_next (struct cb_field *f)
{
	struct cb_field		*p;
	char			buff[32];
	struct cb_field *previous;
	int				sav_min, sav_max;

	previous = cb_get_real_field ();

	if (!previous) {
		cb_error (_("VALUE of '%s': %s target is invalid"),
			f->name, "NEXT");
		cb_error (_("no previous data-item found"));
		return cb_build_numeric_literal (0, "1", 0);
	}

	if (previous->storage != CB_STORAGE_FILE
	 && previous->storage != CB_STORAGE_LINKAGE
	 && !cb_field_founder(previous)->flag_external) {
		cb_error (_("VALUE of '%s': %s target is invalid"), f->name, "NEXT");
		cb_error (_("target must be in FILE SECTION or LINKAGE SECTION or have the EXTERNAL clause"));
		return cb_build_numeric_literal (0, "1", 0);
	}

	/*
	 * Compute the size of the last and all its parent fields,
	 * later fields aren't parsed yet and are therefore not counted
	*/
	if (previous->level != 1) {
		sav_min = previous->occurs_min;
		sav_max = previous->occurs_max;
		previous->occurs_min = previous->occurs_max = 1;
		for (p = previous; p; p = p->parent) {
			p->flag_is_verified = 0;	/* Force compute_size */
			p->flag_invalid = 0;
			cb_validate_field (p);
			if (cb_field_variable_size (p)) {
				cb_error (_("variable length item not allowed here"));
				p->size = 0;
				break;
			}
			if (!p->parent) {
				break;
			}
		}
		previous->occurs_min = sav_min;
		previous->occurs_max = sav_max;
	} else {
		p = previous;
	}

	snprintf (buff, sizeof (buff), "%d", p->size);

	/* Force compute_size for later access */
	for (p = previous; p; p = p->parent) {
		p->flag_is_verified = 0;
		p->flag_invalid = 0;
	}

	return cb_build_numeric_literal (0, buff, 0);
}

cb_tree
cb_build_length (cb_tree x)
{
	struct cb_field		*f, *ftemp;
	struct cb_literal	*l;
	cb_tree			temp,z1,z2;
	char			buff[32];

	if (x == cb_error_node) {
		return cb_error_node;
	}
	if (CB_REFERENCE_P (x) && cb_ref (x) == cb_error_node) {
		return cb_error_node;
	}

	if (CB_LITERAL_P (x)) {
		l = CB_LITERAL (x);
		sprintf (buff, FMT_LEN, (int)l->size);
		return cb_build_numeric_literal (0, buff, 0);
	}
	if (CB_INTRINSIC_P (x)) {
		return cb_build_any_intrinsic (CB_LIST_INIT (x));
	}
	if (cb_occurs_max_length_without_subscript
	 && CB_REFERENCE_P (x)
	 && CB_REFERENCE (x)->length == NULL
	 && CB_REFERENCE (x)->offset == NULL) {
		f = CB_FIELD_PTR (x);
		if (f->flag_occurs) {
			if (!CB_REFERENCE (x)->subs) {
				sprintf (buff, FMT_LEN, cb_field_size (x) * f->occurs_max);
				return cb_build_numeric_literal (0, buff, 0);
			}
		}
		if (cb_field_variable_size (f)) {
			sprintf (buff, FMT_LEN, cb_field_size (x));
			return cb_build_numeric_literal (0, buff, 0);
		}
	}
	if (CB_REF_OR_FIELD_P (x)) {
		if (CB_REFERENCE_P (x) && CB_REFERENCE (x)->offset) {
			return cb_build_any_intrinsic (CB_LIST_INIT (x));
		}
		f = CB_FIELD_PTR (x);
		/* CHECKME: Why do we need this in the first place?
		   Should be validated already, but isn't at least for some
		   RENAMES entries! */
		if (f->size == 0) {
			cb_validate_field (f);
		}
		if (f->flag_any_length) {
			return cb_build_any_intrinsic (CB_LIST_INIT (x));
		}
		if (f->flag_picture_l || cb_field_variable_size (f) == NULL) {
			sprintf (buff, FMT_LEN, cb_field_size (x));
			return cb_build_numeric_literal (0, buff, 0);
		}
	}
	temp = cb_build_index (cb_build_filler (), NULL, 0, NULL);
	ftemp = CB_FIELD (cb_ref (temp));
	ftemp->usage = CB_USAGE_LENGTH;
	ftemp->count++;
	ftemp->pic->have_sign = 0;	/* LENGTH is UNSIGNED */
	cb_emit (cb_build_assign (temp, cb_build_length_1 (x)));

	if (cb_pretty_display
	 && cobc_cs_check == CB_CS_DISPLAY) {
		z1 = cb_build_filler ();
		z2 = cb_build_field_tree (0, z1, NULL, CB_STORAGE_WORKING, NULL, 1);
		ftemp = CB_FIELD (z2);
		ftemp->pic = cb_build_picture ("9(10)");
		ftemp->flag_filler = 1;
		ftemp->usage = CB_USAGE_DISPLAY;
		ftemp->count++;
		cb_validate_field (ftemp);
		cb_emit (CB_BUILD_FUNCALL_2 ("cob_field_int_display", temp, z2));
		return z1;
	}
	return temp;
}

cb_tree
cb_build_ppointer (cb_tree x)
{
	if (x == cb_error_node) {
		return cb_error_node;
	}

	if (CB_REFERENCE_P (x)) {
		/* we get here with either a field reference
		   (then increment use), or by prototpye;
		   CHECKME, count should be incremented by reference already */
		cb_tree xf = cb_ref (x);
		if (xf == cb_error_node) {
			return cb_error_node;
		}
		if (CB_FIELD_P (xf)) {
			CB_FIELD (xf)->count++;
		}
	}
	return CB_BUILD_CAST_PPOINTER (x);
}

/* Validate program */


static void
set_argument_defaults (cb_tree argument, cb_tree parameter, const struct cb_field *arg_field)
{
	const int argument_size = CB_SIZES_INT(argument);
	if (argument_size == CB_SIZE_UNSET) {
		if (parameter) {
			CB_SIZES(argument) = CB_SIZES(parameter);
		} else {
#ifdef COB_64_BIT_POINTER
			CB_SIZES(argument) = CB_SIZE_8;
#else
			CB_SIZES(argument) = CB_SIZE_4;
#endif
		}
	} else if (argument_size == CB_SIZE_AUTO && arg_field) {
		/* TODO: move size upon field size setting here */
	}

}


static void
validate_main_using_param (cb_tree using_list)
{
	cb_tree		x;
	struct cb_field	*f;
	struct cb_field *size_field;
	struct cb_field *string_field;

	if (current_program->num_proc_params == 0) {
		return;
	} else if (current_program->num_proc_params > 1) {
		cb_error (_("at most one USING parameter allowed in main programs"));
		return;
	}

	/* De-reference the parameter */
	x = CB_VALUE (using_list);
	if (!CB_VALID_TREE (x) || cb_ref (x) == cb_error_node) {
		return;
	}
	f = CB_FIELD (cb_ref (x));

	/* Verify is group item */
	if (!f->children) {
		cb_error_x (CB_TREE (f), _("parameter '%s' is not a group item"), f->name);
	}
	/* Containing a signed 16-bit integer */
	size_field = f->children;
	if (!(size_field->size == 2
	 && size_field->pic != NULL
	 && size_field->pic->have_sign
	 && size_field->usage == CB_USAGE_BINARY)) {
		cb_error_x (CB_TREE (size_field), _("item '%s' is not a signed 16-bit COMP integer"), size_field->name);
	}

	/* And a USAGE DISPLAY string (usually ODO). */
	string_field = f->children->sister;
	if (!string_field) {
		cb_error_x (CB_TREE (f), _("group item '%s' has no record for parameter string"), f->name);
	} else if (string_field->usage != CB_USAGE_DISPLAY) {
		cb_error_x (CB_TREE (string_field), _("item '%s' must be USAGE DISPLAY"), string_field->name);
	}

	CB_PENDING (_("parameter for main program"));
}

static void
validate_using (cb_tree using_list)
{
	cb_tree		l;
	cb_tree		x;
	cb_tree		check_list;
	struct cb_field	*f;

	check_list = NULL;
	for (l = using_list; l; l = CB_CHAIN (l)) {
		set_argument_defaults (l, NULL, NULL);	/* TODO: move for supporting that with prototypes */
		x = CB_VALUE (l);
		if (cb_try_ref (x) != cb_error_node) {
			f = CB_FIELD (cb_ref (x));
			if (!current_program->flag_chained) {
				if (f->storage != CB_STORAGE_LINKAGE) {
					cb_error_x (x, _("'%s' is not in LINKAGE SECTION"), f->name);
				}
				if (f->flag_item_based || f->flag_external) {
					cb_error_x (x, _("'%s' cannot be BASED/EXTERNAL"), f->name);
				}
			} else {
				if (f->storage != CB_STORAGE_WORKING) {
					cb_error_x (x, _("'%s' is not in WORKING-STORAGE SECTION"), f->name);
				}
			}
			if (f->level != 01 && f->level != 77) {
				cb_error_x (x, _("'%s' not level 01 or 77"), f->name);
			}
			if (f->redefines) {
				cb_error_x (x, _("'%s' REDEFINES field not allowed here"), f->name);
			}
			if (CB_PURPOSE_INT (l) == CB_CALL_BY_REFERENCE) {
				check_list = cb_list_add (check_list, x);
			}
		}
	}

	if (check_list != NULL) {
		for (l = check_list; l; l = CB_CHAIN (l)) {
			cb_tree	l2 = CB_VALUE (l);
			x = cb_ref (l2);
			if (x != cb_error_node) {
				for (l2 = check_list; l2 != l; l2 = CB_CHAIN (l2)) {
					if (cb_ref (CB_VALUE (l2)) == x) {
						cb_error_x (l,
							_("duplicate USING BY REFERENCE item '%s'"),
							cb_name (CB_VALUE (l)));
						CB_VALUE (l) = cb_error_node;
						break;
					}
				}
			}
		}
	}
}

void
cb_validate_parameters_and_returning (struct cb_program *prog, cb_tree using_list)
{
	cb_tree		l, x;
	struct cb_field	*f, *ret_f;
	int		param_num = 1;

	validate_using (using_list);

	if (current_program->flag_main
	 && !current_program->flag_chained) {
		validate_main_using_param (using_list);
	}

	/* Mark USING fields as parameters */
	param_num = 1;
	for (l = using_list; l; l = CB_CHAIN (l)) {
		x = CB_VALUE (l);
		if (cb_try_ref (x) != cb_error_node) {
			f = CB_FIELD (cb_ref (x));
			if (!current_program->flag_chained) {
				f->flag_is_pdiv_parm = 1;
			} else {
				f->flag_chained = 1;
				f->param_num = param_num;
				param_num++;
			}
			/* add a "receiving" entry for the USING parameter */
			if (cb_listing_xref) {
				cobc_xref_link (&f->xref, CB_REFERENCE (x)->common.source_line, 1);
			}
		}
	}

	/* Validate RETURNING */
	if (prog->returning &&
		cb_ref (prog->returning) != cb_error_node) {
		ret_f = CB_FIELD (cb_ref (prog->returning));
		if (ret_f->redefines) {
			cb_error_x (prog->returning,
				_("'%s' REDEFINES field not allowed here"), ret_f->name);
		}
	} else {
		ret_f = NULL;
	}

	/* Check returning item against using items when FUNCTION */
	if (prog->prog_type == COB_MODULE_TYPE_FUNCTION && ret_f) {
		for (l = using_list; l; l = CB_CHAIN (l)) {
			x = CB_VALUE (l);
			if (CB_VALID_TREE (x) && cb_ref (x) != cb_error_node) {
				f = CB_FIELD (cb_ref (x));
				if (ret_f == f) {
					cb_error_x (x, _("'%s' USING item duplicates RETURNING item"), f->name);
				}
			}
		}
	}

}

 
/* TODO: Add params differing in BY REFERENCE/VALUE and OPTIONAL to testsuite */

static struct cb_program *
try_get_program (cb_tree prog_ref)
{
	struct cb_program	*program = NULL;
	const char		*name_str;
	cb_tree			ref;

	if (CB_LITERAL_P (prog_ref)
	    /* && TODO: Check user wants checks on this kind of CALL. */) {
		name_str = (char *) CB_LITERAL (prog_ref)->data;
		program = cb_find_defined_program_by_name (name_str);
	} else if (CB_REFERENCE_P (prog_ref)) {
		ref = cb_ref (prog_ref);
		if (ref == cb_error_node) {
			return NULL;
		}

		if (CB_FIELD_P (ref) && CB_FIELD (ref)->flag_item_78
		    /* && TODO: Check user wants checks on this kind of CALL. */) {
			name_str = (char *) CB_LITERAL (CB_VALUE (CB_FIELD (ref)->values))->data;
			program = cb_find_defined_program_by_name (name_str);
		} else if (CB_PROTOTYPE_P (ref)) {
			name_str = CB_PROTOTYPE (ref)->ext_name;
			program = cb_find_defined_program_by_id (name_str);
		} else if (CB_PROGRAM_P (ref)) {
			program = CB_PROGRAM (ref);
		}
	}

	return program;
}

static int
is_alphanum_group (const struct cb_field *f)
{
	return f->children && CB_TREE_CATEGORY (f) == CB_CATEGORY_ALPHANUMERIC;
}

/* get numbered USING parameter tree */
static cb_tree
find_nth_parameter (const struct cb_program *prog, const unsigned int n)
{
	cb_tree		entry_param;
	unsigned int parmnum = 1;
	for (entry_param = CB_VALUE (CB_VALUE (prog->entry_list)); entry_param;
	     entry_param = CB_CHAIN (entry_param)) {
		if (n == parmnum++) {
			return entry_param;
		}
	}

	return NULL;
}

static void
emit_definition_prototype_error_header (const char *name)
{
	/* FIXME: move to error.c and cleanup similar to configuration_error */
	cb_warning (cb_warn_repository_checks,
		_("prototype and definition of '%s' do not match"), name);
}

static void
emit_definition_prototype_error (const char *name, const char *error,
				 int * const prototype_error_header_shown)
{
	/* FIXME: move to error.c and cleanup similar to configuration_error */
	if (!*prototype_error_header_shown) {
		emit_definition_prototype_error_header (name);
		*prototype_error_header_shown = 1;
	}

	cb_note (cb_warn_repository_checks, 0, "%s", error);
}

static void
emit_definition_prototype_clause_mismatch (const char *name, const char *clause,
				 int * const prototype_error_header_shown)
{
	/* FIXME: move to error.c and cleanup similar to configuration_error */
	if (!*prototype_error_header_shown) {
		emit_definition_prototype_error_header (name);
		*prototype_error_header_shown = 1;
	}

	cb_note (cb_warn_repository_checks, 0, "%s clauses differ", clause);
}

static int
items_have_same_data_clauses (const struct cb_field * const field_1,
			      const struct cb_field * const field_2,
			      const int check_any_length)
{
	const int	any_length_check =
		!check_any_length
		|| (field_1->flag_any_length == field_2->flag_any_length);
	int	same_pic;

	if (!any_length_check) {
		return 1;
	}

	if (field_1->pic && field_2->pic) {
		if (check_any_length
		 || field_1->flag_any_length == field_2->flag_any_length) {
			if (field_1->usage != field_2->usage) {
				same_pic = 0;
			} else {
				same_pic = strcmp (field_1->pic->orig, field_2->pic->orig) == 0;
			}
		} else {
			/* only one has any length -> ensure it is the prototype and
			   that the othr has the same numeric/nonnumeric type */
			if (!field_1->flag_any_length) {
				return 1;
			}
			if (field_1->flag_any_numeric) {
				same_pic = CB_TREE_CATEGORY (field_2) == CB_CATEGORY_NUMERIC;
			} else {
				same_pic = field_1->pic->orig[1] == field_2->pic->orig[1];
			}
		}
	} else {
		if (field_1->pic || field_2->pic) {
			same_pic = 0;
		} else {
			same_pic = field_1->usage == field_2->usage;
		}
	}

	return same_pic
		&& (field_1->flag_blank_zero == field_2->flag_blank_zero)
		&& (field_1->flag_justified == field_2->flag_justified)
		&& (field_1->flag_sign_separate == field_2->flag_sign_separate
		 && field_1->flag_sign_leading == field_2->flag_sign_leading);
}

static int
error_if_items_differ (const char *element_name,
		       const struct cb_field * const def_item,
		       const struct cb_field * const proto_item,
		       const int is_parameter,
		       const int parameter_num,
		       int * const prototype_error_header_shown)
{
	int	        error_found;
	const struct cb_field	*def_child;
	const struct cb_field	*proto_child;

	/* Perform error checks */
	error_found = !items_have_same_data_clauses (def_item, proto_item, 1);

	/* Perform checks for children, if present */
	if (!error_found) {
		for (def_child = def_item->children, proto_child = proto_item->children;
		     def_child && proto_child;
		     def_child = def_child->sister, proto_child = proto_child->sister) {
			if (error_if_items_differ (element_name, def_child,
						   proto_child, is_parameter,
						   parameter_num,
						   prototype_error_header_shown)) {
				return 1;
			}
		}

		/* Different number of children */
		if (def_child || proto_child) {
			error_found = 1;
		}
	}

	if (error_found) {
		if (!*prototype_error_header_shown) {
			emit_definition_prototype_error_header (element_name);
			*prototype_error_header_shown = 1;
		}

		/* TODO: Indicate location of the items in error. */
		if (is_parameter) {
			cb_note (cb_warn_repository_checks, 0,
				  _("parameters #%d ('%s' in the definition and '%s' in the prototype) differ"),
				  parameter_num, def_item->name, proto_item->name);
		} else { /* RETURNING item */
			cb_note (cb_warn_repository_checks, 0,
				  _("returning items ('%s' in the definition and '%s' in the prototype) differ"),
				  def_item->name, proto_item->name);
		}
	}

	return error_found;
}

static void
error_if_signatures_differ (struct cb_program *prog1, struct cb_program *prog2)
{
	struct cb_program	*definition;
	struct cb_program	*prototype;
	const char		*element_name = prog1->orig_program_id;
	cb_tree		def_item, proto_item;
	const struct cb_field	*def_field, *proto_field;
	unsigned int	parameter_num;
	int	prototype_error_header_shown = 0;

	/* We assume one of the parameters is a prototype */
	if (prog1->flag_prototype) {
		definition = prog2;
		prototype = prog1;
	} else {
		definition = prog1;
		prototype = prog2;
	}

	/* If error detected, output header:
	   Error: 1: prototype and definition of "foo" do not match:
	   Error: 1:  * first error ...
	*/

	if (definition->prog_type != prototype->prog_type) {
		if (definition->prog_type == COB_MODULE_TYPE_PROGRAM) {
			emit_definition_prototype_error (element_name,
					_("definition is a program but the prototype is a function"),
					&prototype_error_header_shown);
		} else { /* function */
			emit_definition_prototype_error (element_name,
					_("definition is a function but the prototype is a program"),
					&prototype_error_header_shown);
		}
	}

	if (definition->decimal_point != prototype->decimal_point) {
		emit_definition_prototype_clause_mismatch (
			element_name, "DECIMAL-POINT IS COMMA",
			&prototype_error_header_shown);
	}

	if (definition->currency_symbol != prototype->currency_symbol) {
		emit_definition_prototype_clause_mismatch (
			element_name, "CURRENCY",
			&prototype_error_header_shown);
	}

	/*
	   prototype is a COBOL 2002 feature, which dropped the ENTRY statement,
	   we therefore only check the number of its "main" entry point and
	   also check the call-convention using that
	 */
	if (cb_get_int (definition->entry_convention)
	 != cb_get_int (prototype->entry_convention)) {
		emit_definition_prototype_clause_mismatch (
			element_name, "ENTRY-CONVENTION",
			&prototype_error_header_shown);
	}

	/*
	  Check number of parameters is the same and if so, compare each
	  parameter.
	*/

	if (definition->num_proc_params == prototype->num_proc_params) {
		/* Compare corresponding parameters */
		for (parameter_num = 1;
		     parameter_num <= definition->num_proc_params;
		     ++parameter_num) {
			def_item = find_nth_parameter (definition, parameter_num);
			proto_item = find_nth_parameter (prototype, parameter_num);

			if (def_item && proto_item) {
				def_field = CB_FIELD_PTR(CB_VALUE(def_item));
				proto_field = CB_FIELD_PTR(CB_VALUE(proto_item));
				error_if_items_differ (element_name, def_field,
						       proto_field, 1,
						       parameter_num,
						       &prototype_error_header_shown);

				if ((CB_PURPOSE_INT (def_item) != CB_PURPOSE_INT (proto_item))
				 || (def_field->flag_is_pdiv_opt != proto_field->flag_is_pdiv_opt)) {
					cb_note (cb_warn_repository_checks, 0,
						_("parameters #%d ('%s' in the definition and '%s' in the prototype) differ"),
						(int)parameter_num, def_field->name, proto_field->name);
					emit_definition_prototype_clause_mismatch (
						element_name, "OPTIONAL", &prototype_error_header_shown);
				}
			}

		}
	} else {
		emit_definition_prototype_error (element_name,
				_("number of parameters differ"),
				&prototype_error_header_shown);
	}

	/* Compare returning items. */

	if ((definition->returning || prototype->returning)
	    && !(definition->returning && prototype->returning)) {
		if (definition->returning) {
			emit_definition_prototype_error (element_name,
					_("definition has a RETURNING item but prototype does not"),
					&prototype_error_header_shown);
		} else {
			emit_definition_prototype_error (element_name,
					_("definition does not have a RETURNING item but prototype does"),
					&prototype_error_header_shown);
		}
	} else if (definition->returning && prototype->returning) {
		error_if_items_differ (element_name,
				       CB_FIELD (cb_ref (definition->returning)),
				       CB_FIELD (cb_ref (prototype->returning)),
				       0, 0, &prototype_error_header_shown);
	}
}

void
cb_check_definition_matches_prototype (struct cb_program *prog)
{
	struct cb_program *prog2 = prog;	/* work around bad anaylzer warning */
	cb_tree	l;

	/* if check is explicit disabled: don't care */
	if (get_warn_opt_value (cb_warn_repository_checks) == COBC_WARN_DISABLED) {
		return;
	}

	/* Find the previous prototype/definition. */
	for (l = defined_prog_list; l; l = CB_CHAIN (l)) {
		prog2 = CB_PROGRAM (CB_VALUE (l));
		if (prog != prog2
		 && !strcmp (prog2->orig_program_id, prog->orig_program_id)) {
			break;
		}
	}

	if (l) {
		error_if_signatures_differ (prog2, prog);
	}
}

static int
get_size (cb_tree x)
{
	switch (CB_TREE_TAG (x)) {
	case CB_TAG_CONST:
		return strlen (CB_CONST (x)->val);
	case CB_TAG_LITERAL:
		return CB_LITERAL (x)->size;
	case CB_TAG_FIELD:
		return CB_FIELD (x)->size;
	case CB_TAG_REFERENCE:
		return get_size (cb_ref (x));
	/* LCOV_EXCL_START */
	default:
		cobc_err_msg (_("unexpected tree tag: %d"), CB_TREE_TAG (x));
		return 0;
	/* LCOV_EXCL_STOP */
	}
}

static void
check_argument_conformance (struct cb_program *program, cb_tree argument_tripple,
			    const int param_num)
{
	cb_tree		param = find_nth_parameter (program, param_num);
	cb_tree		arg_tree = CB_VALUE (argument_tripple);
	enum cb_call_mode	arg_mode = CB_PURPOSE_INT (argument_tripple);
	const enum cb_call_mode	param_mode = CB_PURPOSE_INT (param);
	const struct cb_field	*arg_field, *param_field;
	cb_tree			param_ref;
	int			error_found = 0;
	enum cb_class		param_class;

	/* Check BY REFERENCE/CONTENT/VALUE is correct. */
	if ((arg_mode == CB_CALL_BY_REFERENCE || arg_mode == CB_CALL_BY_CONTENT)
	 &&  param_mode != CB_CALL_BY_REFERENCE) {
		/* TODO: Improve name of CB_VALUE (argument_tripple) */
		cb_warning_x (cb_warn_repository_checks, arg_tree,
			_("expected argument #%d, %s, to be passed BY VALUE"),
			param_num, cb_name (arg_tree));
	} else if (arg_mode == CB_CALL_BY_VALUE
	        && param_mode != CB_CALL_BY_VALUE) {
		cb_warning_x (cb_warn_repository_checks, arg_tree,
			_("expected argument #%d, %s, to be passed BY REFERENCE/CONTENT"),
			param_num, cb_name (arg_tree));
	}

	if (CB_REF_OR_FIELD_P (arg_tree)) {
		arg_field = CB_FIELD_PTR(arg_tree);
	} else {
		arg_field = NULL;
	}
	param_field = CB_FIELD_PTR(CB_VALUE(param));

	/*
	  If BY REFERENCE, check OMITTED was specified for OPTIONAL parameter.
	*/

	if (arg_mode == CB_CALL_BY_REFERENCE
	 && arg_tree == cb_null
	 && !param_field->flag_is_pdiv_opt) {
		cb_warning_x (cb_warn_repository_checks, arg_tree,
			_("argument #%d is not optional"), param_num);
		return;
	}

	param_ref = cb_build_field_reference ((struct cb_field *)param_field, NULL);

	/*
	  Check the definition of the argument is compatible with the parameter.
	*/
	if ((arg_field && is_alphanum_group (arg_field))
	 || is_alphanum_group (param_field)) {
		if (param_mode == CB_CALL_BY_REFERENCE) {
			if (get_size (arg_tree) < param_field->size) {
				cb_warning_x (cb_warn_repository_checks, arg_tree,
						_("argument #%d must be at least %d bytes long"),
						param_num, param_field->size);
			}
			return;
		} else {
			/* BY CONTENT (BY VALUE items must be strongly typed) */
			/* Same checks as for MOVE */
			error_found = cb_check_move (arg_tree, CB_LIST_INIT (param_ref), 0);
		}
	} else {
		if (arg_mode == CB_CALL_BY_REFERENCE) {
			if (CB_TREE_CLASS (param) == CB_CLASS_POINTER) {
				if (CB_TREE_CATEGORY (arg_tree) != CB_TREE_CATEGORY (param)) {
					/* TODO: Improve error message */
					cb_warning_x (cb_warn_repository_checks, arg_tree,
						    _("argument #%d is a different type of pointer than the parameter"),
						    param_num);
				}
				return;
			} else if (arg_field) {
				if (arg_field->flag_any_length && !param_field->flag_any_length) {
					cb_warning_x (cb_warn_repository_checks, arg_tree,
						    _("argument #%d is ANY LENGTH, but expecting a fixed size item"),
						    param_num);
					return;
				}
				error_found = !items_have_same_data_clauses (arg_field, param_field, 0);
			}
		} else { /* BY CONTENT or BY VALUE */
			if (arg_mode == CB_CALL_BY_VALUE) {
				set_argument_defaults (argument_tripple, param, arg_field);
				/* TODO: check size conformance */
			}

			param_class = CB_TREE_CLASS (param);
			if (CB_TREE_CLASS (param) == CB_CLASS_POINTER
			 || CB_TREE_CLASS (arg_tree) == CB_CLASS_POINTER) {
				error_found = cb_check_set_to (CB_LIST_INIT (param_ref),
								arg_tree, 0);
			} else if (param_class == CB_CLASS_NUMERIC) {
				error_found = cb_check_arithmetic (CB_LIST_INIT (param_ref),
								arg_tree, 1);
			} else {
				error_found = cb_check_move (arg_tree, CB_LIST_INIT (param_ref), 0);
			}
		}
	}

	if (error_found) {
		cb_warning_x (cb_warn_repository_checks, arg_tree,
				_("argument #%d, %s, does not conform to the parameter definition"),
				param_num, cb_name (arg_tree));
	}
}

void
cb_check_conformance (cb_tree prog_ref, cb_tree using_list,
		   cb_tree returning)
{
	struct cb_program	*program = NULL;
	cb_tree			l;
	cb_tree			last_arg = NULL;
	cb_tree			param;
	unsigned int	param_num, num_params;
	const struct cb_field	*prog_returning_field;
	const struct cb_field	*call_returning_field;

	/* Try to get the program referred to by prog_ref. */
	program = try_get_program (prog_ref);
	if (!program) {
		/*
		 */
		for (l = using_list; l;	l = CB_CHAIN (l)) {
			set_argument_defaults (l, NULL, NULL);
		}
		return;
	}

	/*
	  Check each parameter is conformant: has right type, has right
	  REFERENCE/VALUE phrase, has right length, etc.
	*/

	for (l = using_list, param_num = 1;
	     l && param_num <= program->num_proc_params;
	     l = CB_CHAIN (l), ++param_num) {
		check_argument_conformance (program, l, param_num);
		last_arg = l;
	}

	/* If there are more params in the using list than in the prototype, error */
	if (l && param_num > program->num_proc_params) {
		for (num_params = param_num;
		     CB_CHAIN (last_arg);
		     last_arg = CB_CHAIN (last_arg), ++num_params);
		/* CHECKME: is that an actual error or should we only warn? */
		cb_warning_x (cb_warn_repository_checks, CB_VALUE (last_arg),
			    _("expecting up to %d arguments, but found %d"),
			    (int)program->num_proc_params, (int)num_params);
	}

	/*
	  If there are less arguments in the using list than in the prototype,
	  check the omitted ones are for OPTIONAL parameters.
	*/
	for (; param_num <= program->num_proc_params; ++param_num) {
		param = find_nth_parameter (program, param_num);
		if (CB_PURPOSE_INT(param) != CB_CALL_BY_REFERENCE
		  || !CB_FIELD_PTR(CB_VALUE(param))->flag_is_pdiv_opt) {
			if (last_arg) {
				cb_warning_x (cb_warn_repository_checks, CB_VALUE (last_arg),
				    _("argument #%d is not optional"),
					(int)param_num);
			} else {
				cb_warning (cb_warn_repository_checks, _("argument #%d is not optional"),
					(int)param_num);
			}
		}
	}

	/* Check RETURNING item. */

	if (returning && program->returning) {
		/* Basically same checks as for elementary item BY REFERENCE */
		prog_returning_field = CB_FIELD (cb_ref (program->returning));
		call_returning_field = CB_FIELD (cb_ref (returning));
		if (prog_returning_field->flag_any_length
		    && !call_returning_field->flag_any_length) {
			/* TODO: Check! */
			cb_warning_x (cb_warn_repository_checks, returning,
				_("the RETURNING item is of a fixed size, not ANY LENGTH"));
		}
		if (!items_have_same_data_clauses (call_returning_field,
						   prog_returning_field, 0)) {
			/* TODO: Improve message! */
			cb_warning_x (cb_warn_repository_checks, returning,
					_("RETURNING item %s is not a valid type"),
				    cb_name (CB_TREE (call_returning_field)));
		}
	} else if (returning && !program->returning) {
		/* CHECKME: do we want to cater for RETURNING internally setting RETURN-CODE? */
		cb_warning_x (cb_warn_repository_checks, returning,
			_("unexpected RETURNING item"));
	} else if (!returning && program->returning) {
		cb_warning_x (cb_warn_repository_checks, returning,
			_("expecting a RETURNING item, but none provided"));
	}
}

static int
get_value (cb_tree x)
{
	if (x == cb_space) {
		return ' ';
	} else if (x == cb_zero) {
		return '0';
	} else if (x == cb_quote) {
		return cb_flag_apostrophe ? '\'' : '"';
	} else if (x == cb_norm_low) {
		return 0;
	} else if (x == cb_norm_high) {
		return 255;
	} else if (x == cb_null) {
		return 0;
	} else {
		enum cb_class cls = CB_TREE_CLASS (x);
		if (cls == CB_CLASS_NUMERIC) {
			return cb_get_int (x) - 1;
		} else {
			struct cb_literal* lit = CB_LITERAL (x);
			if (cls == CB_CLASS_NATIONAL) {
				/* actually would need to check BE/LE here to do correct calculation */
				cob_u32_t i;
				int ret = lit->data[0];
				for (i = 1; i < lit->size; ++i) {
					ret *= 256;
					ret += lit->data[i];
				}
				return ret;
			}
			return lit->data[0];
		}
	}
}

static int
cb_validate_collating (cb_tree collating_sequence)
{
	cb_tree		x;

	if (!collating_sequence) {
		return 0;
	}

	x = cb_ref (collating_sequence);
	if (!CB_ALPHABET_NAME_P (x)) {
		cb_error_x (collating_sequence, _("'%s' is not an alphabet-name"),
			    cb_name (collating_sequence));
		return 1;
	}
	if (CB_ALPHABET_NAME (x)->alphabet_type != CB_ALPHABET_CUSTOM) {
		return 0;
	}
	if (CB_ALPHABET_NAME (x)->low_val_char) {
		cb_low = cb_build_alphanumeric_literal ("\0", (size_t)1);
		CB_LITERAL(cb_low)->data[0] = (unsigned char)CB_ALPHABET_NAME (x)->low_val_char;
		CB_LITERAL(cb_low)->all = 1;
	}
	if (CB_ALPHABET_NAME (x)->high_val_char != 255){
		cb_high = cb_build_alphanumeric_literal ("\0", (size_t)1);
		CB_LITERAL(cb_high)->data[0] = (unsigned char)CB_ALPHABET_NAME (x)->high_val_char;
		CB_LITERAL(cb_high)->all = 1;
	}
	return 0;
}

static void
validate_alphabet (cb_tree alphabet)
{
	struct cb_alphabet_name *ap = CB_ALPHABET_NAME (alphabet);
	unsigned int		n;

	/* Native */
	if (ap->alphabet_type == CB_ALPHABET_NATIVE) {
		for (n = 0; n < 256; n++) {
			ap->values[n] = n;
			ap->alphachr[n] = n;
		}
		return;
	}

	/* ASCII */
	if (ap->alphabet_type == CB_ALPHABET_ASCII) {
		for (n = 0; n < 256; n++) {
#ifdef	COB_EBCDIC_MACHINE
			ap->values[n] = (int)cob_refer_ascii[n];
			ap->alphachr[n] = (int)cob_refer_ascii[n];
#else
			ap->values[n] = n;
			ap->alphachr[n] = n;
#endif
		}
		return;
	}

	/* EBCDIC */
	if (ap->alphabet_type == CB_ALPHABET_EBCDIC) {
		for (n = 0; n < 256; n++) {
#ifdef	COB_EBCDIC_MACHINE
			ap->values[n] = n;
			ap->alphachr[n] = n;
#else
			ap->values[n] = (int)cob_refer_ebcdic[n];
			ap->alphachr[n] = (int)cob_refer_ebcdic[n];
#endif
		}
		return;
	}

	/* Custom alphabet */
	{
		cb_tree		l, x;
		size_t		count = 0;
		int			unvals = 0, dupls = 0;
		int			lastval = 0, tableval = 0;
		int			pos = 0;
		int			i;
		int			values[256];
		int			charvals[256];
		int			dupvals[256];

		for (n = 0; n < 256; n++) {
			values[n] = -1;
			charvals[n] = -1;
			dupvals[n] = -1;
			ap->values[n] = -1;
			ap->alphachr[n] = -1;
		}
		ap->low_val_char = 0;
		ap->high_val_char = 255;
		for (l = ap->custom_list; l; l = CB_CHAIN (l)) {
			x = CB_VALUE (l);
			pos++;
			if (count > 255
			 || x == NULL) {
				unvals = pos;
				break;
			}
			if (CB_PAIR_P (x)) {		/* X THRU Y */
				int lower = get_value (CB_PAIR_X (x));
				int upper = get_value (CB_PAIR_Y (x));
				lastval = upper;
				if (!count) {
					ap->low_val_char = lower;
				}
				/* regression in NATIONAL literals as
				   thpose are unfinished; would be fine
				   with national alphabet in general */
				if (lower < 0 || lower > 255) {
					unvals = pos;
					continue;
				}
				if (upper < 0 || upper > 255) {
					unvals = pos;
					continue;
				}
				if (lower <= upper) {
					for (i = lower; i <= upper; i++) {
						if (values[i] != -1) {
							dupvals[i] = i;
							dupls = 1;
						}
						values[i] = i;
						charvals[i] = i;
						ap->alphachr[tableval] = i;
						ap->values[i] = tableval++;
						count++;
					}
				} else {
					for (i = lower; i >= upper; i--) {
						if (values[i] != -1) {
							dupvals[i] = i;
							dupls = 1;
						}
						values[i] = i;
						charvals[i] = i;
						ap->alphachr[tableval] = i;
						ap->values[i] = tableval++;
						count++;
					}
				}
			} else if (CB_LIST_P (x)) {		/* X ALSO Y ... */
				cb_tree			ls;
				if (!count) {
					ap->low_val_char = get_value (CB_VALUE (x));
				}
				for (ls = x; ls; ls = CB_CHAIN (ls)) {
					int val = get_value (CB_VALUE (ls));
					if (!CB_CHAIN (ls)) {
						lastval = val;
					}
					/* regression in NATIONAL literals as
					   those are unfinished; would be fine
					   with national alphabet in general */
					if (val < 0 || val > 255) {
						unvals = pos;
						continue;
					}
					n = (unsigned char)val;
					if (values[n] != -1) {
						dupvals[n] = n;
						dupls = 1;
					}
					values[n] = n;
					ap->values[n] = tableval;
					if (ls == x) {
						ap->alphachr[tableval] = n;
						charvals[n] = n;
					}
					count++;
				}
				tableval++;
			} else if (CB_NUMERIC_LITERAL_P (x)) {	/* Numeric Literal */
				lastval = get_value (x);
				if (!count) {
					ap->low_val_char = lastval;
				}
				if (lastval < 0 || lastval > 255) {
					unvals = pos;
					continue;
				}
				n = (unsigned char)lastval;
				if (values[n] != -1) {
					dupvals[n] = n;
					dupls = 1;
				}
				values[n] = n;
				charvals[n] = n;
				ap->alphachr[tableval] = n;
				ap->values[n] = tableval++;
				count++;
			} else if (CB_LITERAL_P (x)) {		/* Non-numeric Literal */
				int size = (int)CB_LITERAL (x)->size;
				unsigned char *data = CB_LITERAL (x)->data;
				if (!count) {
					ap->low_val_char = data[0];
				}
				lastval = data[size - 1];
				if (CB_TREE_CATEGORY (x) != CB_CATEGORY_NATIONAL) {
					for (i = 0; i < size; i++) {
						n = data[i];
						if (values[n] != -1) {
							dupvals[n] = n;
							dupls = 1;
						}
						values[n] = n;
						charvals[n] = n;
						ap->alphachr[tableval] = n;
						ap->values[n] = tableval++;
						count++;
					}
				} else {
					for (i = 0; i < size; i++) {
						/* assuming we have UTF16BE here */
						if (data[i] == 0) {
						/* only checking lower entries, all others,
							which are currently only possible with
							national-hex literals are not checked
							TODO: add a list of values for those and
							iterate over the list */
							n = data[++i];
							if (values[n] != -1) {
								dupvals[n] = n;
								dupls = 1;
							}
							values[n] = n;
							charvals[n] = n;
							ap->values[n] = tableval;
						} else {
							n = data[i++];
							n = n * 255 + data[i];
						}
						ap->alphachr[tableval++] = n;
						count++;
					}
				}
			} else {	/* CHECKME and doc here */
				lastval = get_value (x);
				if (!count) {
					ap->low_val_char = lastval;
				}
				if (lastval < 0 || lastval > 255) {
					unvals = pos;
					continue;
				}
				n = (unsigned char) lastval;
				if (values[n] != -1) {
					dupls = 1;
				}
				values[n] = n;
				charvals[n] = n;
				ap->alphachr[tableval] = n;
				ap->values[n] = tableval++;
				count++;
			}
		}
		if (dupls || unvals) {
			if (dupls) {
				char		errmsg[256];
				i = 0;
				for (n = 0; n < 256; n++) {
					if (dupvals[n] != -1) {
						if (i > 240) {
							i += sprintf (&errmsg[i], ", ...");
							break;
						}
						if (i) {
							i += sprintf (&errmsg[i], ", ");
						}
						if (isprint (n)) {
							errmsg[i++] = (char)n;
						} else {
							i += sprintf (&errmsg[i], "x'%02x'", n);
						}
					};
				}
				errmsg[i] = 0;
				cb_error_x (alphabet,
					_("duplicate character values in alphabet '%s': %s"),
					ap->name, errmsg);
			}
			if (unvals) {
				cb_error_x (alphabet,
					_("invalid character values in alphabet '%s', starting at position %d"),
					ap->name, pos);
			}
			ap->low_val_char = 0;
			ap->high_val_char = 255;
			return;
		}
		/* Calculate HIGH-VALUE */
		/* If all 256 values have been specified, */
		/* HIGH-VALUE is the last one */
		/* Otherwise if HIGH-VALUE has been specified, find the highest */
		/* value that has not been used */
		if (count == 256) {
			ap->high_val_char = lastval;
		} else if (values[255] != -1) {
			ap->high_val_char = 0;
			for (n = 254; n > 0; n--) {
				if (values[n] == -1) {
					ap->high_val_char = n;
					break;
				}
			}
		}

		/* Get rest of code set */
		for (n = tableval; n < 256; ++n) {
			for (i = 0; i < 256; ++i) {
				if (charvals[i] < 0) {
					charvals[i] = 0;
					ap->alphachr[n] = i;
					break;
				}
			}
		}

		/* Fill in missing characters */
		for (n = 0; n < 256; n++) {
			if (ap->values[n] < 0) {
				ap->values[n] = tableval++;
			}
		}
	}
}

static void
check_class_duplicates (cb_tree class_name)
{
	struct cb_class_name* cp = CB_CLASS_NAME (class_name);
	size_t			dupls = 0;
	int			values[256] = { 0 };
	cb_tree			l;

#if 0 /* should not be necessary with init above */
	memset (values, 0, sizeof (values));
#endif
	for (l = cp->list; l; l = CB_CHAIN (l)) {
		cb_tree			x = CB_VALUE (l);
		if (CB_PAIR_P (x)) {
			/* X THRU Y */
			int lower = get_value (CB_PAIR_X (x));
			int upper = get_value (CB_PAIR_Y (x));
			int i;
			for (i = lower; i <= upper; i++) {
				if (values[i]) {
					dupls = 1;
				} else {
					values[i] = 1;
				}
			}
		} else {
			int			n;
			if (CB_NUMERIC_LITERAL_P (x)) {
				n = get_value (x);
				if (values[n]) {
					dupls = 1;
				} else {
					values[n] = 1;
				}
			} else if (CB_LITERAL_P (x)) {
				int	 size = (int)CB_LITERAL (x)->size;
				unsigned char* data = CB_LITERAL (x)->data;
				int i;
				for (i = 0; i < size; i++) {
					n = data[i];
					if (values[n]) {
						dupls = 1;
					} else {
						values[n] = 1;
					}
				}
			} else {
				n = get_value (x);
				if (values[n]) {
					dupls = 1;
				} else {
					values[n] = 1;
				}
			}
		}
	}
	if (dupls) {
		cb_warning_x (cb_warn_additional, class_name,
			_("duplicate character values in class '%s'"),
			cb_name (class_name));
	}
}

void
cb_validate_program_environment (struct cb_program *prog)
{
	cb_tree			l;

	/* Check ALPHABET clauses */
	/* Complicated by difference between code set and collating sequence */
	for (l = prog->alphabet_name_list; l; l = CB_CHAIN (l)) {
		validate_alphabet (CB_VALUE (l));
	}

	/* Reset HIGH/LOW-VALUES */
	cb_low = cb_norm_low;
	cb_high = cb_norm_high;

	/* Check and generate SYMBOLIC clauses */
	for (l = prog->symbolic_char_list; l; l = CB_CHAIN (l)) {
		cb_tree x;
		if (CB_VALUE (l)) {
			x = cb_ref (CB_VALUE (l));
			if (x == cb_error_node) {
				continue;
			}
			if (!CB_ALPHABET_NAME_P (x)) {
				cb_error_x (x, _("invalid ALPHABET name"));
				continue;
			}
		} else {
			x = NULL;
		}
		cb_build_symbolic_chars (CB_PURPOSE (l), x);
	}

	/* Check CLASS clauses for duplicates */
	if (get_warn_opt_value (cb_warn_additional) != COBC_WARN_DISABLED) {
		for (l = prog->class_name_list; l; l = CB_CHAIN (l)) {
			check_class_duplicates (CB_VALUE (l));
		}
	}

	/* Resolve the program collating sequences */
	if (cb_validate_collating (prog->collating_sequence)) {
		prog->collating_sequence = NULL;
	};
	if (cb_validate_collating (prog->collating_sequence_n)) {
		prog->collating_sequence_n = NULL;
	};

	/* Resolve the program classification */
	if (prog->classification && prog->classification != cb_int1) {
		cb_tree x = cb_ref (prog->classification);
		if (!CB_LOCALE_NAME_P (x)) {
			cb_error_x (prog->classification,
				    _("'%s' is not a locale name"),
				    cb_name (prog->classification));
			prog->classification = NULL;
			return;
		}
	}
}

/* default (=minimal) size of DEBUG-CONTENTS */
#ifdef DFLT_DEBUG_CONTENTS_SIZE
#if DFLT_DEBUG_CONTENTS_SIZE < 13
#undef  DFLT_DEBUG_CONTENTS_SIZE
#define DFLT_DEBUG_CONTENTS_SIZE 13	/* Lenght of fixed values */
#endif
#else
#define DFLT_DEBUG_CONTENTS_SIZE 30
#endif


void
cb_build_debug_item (void)
{
	struct cb_field	*f;
	cb_tree			l;
	cb_tree			x;
	cb_tree			lvl01_tree;

	/* check if it is actually available - for example not the case for ACU */
	if (!cb_get_register_definition ("DEBUG-ITEM")) {
		return;
	}

	/* unreserve the DEBUG-ITEM register/reserved words */

	/* FIXME: using remove_reserved_word lead to those words be still available,
	          using remove_reserved_word_now breaks the reserved word list,
			  effectively removing other words */
	remove_reserved_word_now ("DEBUG-ITEM");
	remove_reserved_word_now ("DEBUG-LINE");
	remove_reserved_word_now ("DEBUG-NAME");
	remove_reserved_word_now ("DEBUG-SUB-1");
	remove_reserved_word_now ("DEBUG-SUB-2");
	remove_reserved_word_now ("DEBUG-SUB-3");
	remove_reserved_word_now ("DEBUG-CONTENTS");

	/* Set up DEBUG-ITEM */
	l = cb_build_reference ("DEBUG-ITEM");
	lvl01_tree = cb_build_field_tree (0, l, NULL, CB_STORAGE_WORKING,
				 NULL, 1);
	f = CB_FIELD (lvl01_tree);
	f->values = CB_LIST_INIT (cb_space);
	cb_debug_item = l;

	l = cb_build_reference ("DEBUG-LINE");
	x = cb_build_field_tree (0, l, f, CB_STORAGE_WORKING, NULL, 3);
	f = CB_FIELD (x);
	f->pic = cb_build_picture ("X(6)");
	cb_validate_field (f);
	cb_debug_line = l;

	l = cb_build_filler ();
	x = cb_build_field_tree (0, l, f, CB_STORAGE_WORKING, NULL, 3);
	f = CB_FIELD (x);
	f->pic = cb_build_picture ("X");
	f->flag_filler = 1;
	cb_validate_field (f);

	l = cb_build_reference ("DEBUG-NAME");
	x = cb_build_field_tree (0, l, f, CB_STORAGE_WORKING, NULL, 3);
	f = CB_FIELD (x);
	f->pic = cb_build_picture ("X(30)");
	cb_validate_field (f);
	cb_debug_name = l;

	l = cb_build_filler ();
	x = cb_build_field_tree (0, l, f, CB_STORAGE_WORKING, NULL, 3);
	f = CB_FIELD (x);
	f->pic = cb_build_picture ("X");
	f->flag_filler = 1;
	cb_validate_field (f);

	l = cb_build_reference ("DEBUG-SUB-1");
	x = cb_build_field_tree (0, l, f, CB_STORAGE_WORKING, NULL, 3);
	f = CB_FIELD (x);
	f->pic = cb_build_picture ("S9(4)");
	f->flag_sign_leading = 1;
	f->flag_sign_separate = 1;
	cb_validate_field (f);
	cb_debug_sub_1 = l;

	l = cb_build_filler ();
	x = cb_build_field_tree (0, l, f, CB_STORAGE_WORKING, NULL, 3);
	f = CB_FIELD (x);
	f->pic = cb_build_picture ("X");
	f->flag_filler = 1;
	cb_validate_field (f);

	l = cb_build_reference ("DEBUG-SUB-2");
	x = cb_build_field_tree (0, l, f, CB_STORAGE_WORKING, NULL, 3);
	f = CB_FIELD (x);
	f->pic = cb_build_picture ("S9(4)");
	f->flag_sign_leading = 1;
	f->flag_sign_separate = 1;
	cb_validate_field (f);
	cb_debug_sub_2 = l;

	l = cb_build_filler ();
	x = cb_build_field_tree (0, l, f, CB_STORAGE_WORKING, NULL, 3);
	f = CB_FIELD (x);
	f->pic = cb_build_picture ("X");
	f->flag_filler = 1;
	cb_validate_field (f);

	l = cb_build_reference ("DEBUG-SUB-3");
	x = cb_build_field_tree (0, l, f, CB_STORAGE_WORKING, NULL, 3);
	f = CB_FIELD (x);
	f->pic = cb_build_picture ("S9(4)");
	f->flag_sign_leading = 1;
	f->flag_sign_separate = 1;
	cb_validate_field (f);
	cb_debug_sub_3 = l;

	l = cb_build_filler ();
	x = cb_build_field_tree (0, l, f, CB_STORAGE_WORKING, NULL, 3);
	f = CB_FIELD (x);
	f->pic = cb_build_picture ("X");
	f->flag_filler = 1;
	cb_validate_field (f);

	l = cb_build_reference ("DEBUG-CONTENTS");
	x = cb_build_field_tree (0, l, f, CB_STORAGE_WORKING, NULL, 3);
	f = CB_FIELD (x);
	f->pic = cb_build_picture ("X(" CB_XSTRINGIFY(DFLT_DEBUG_CONTENTS_SIZE) ")");
	cb_validate_field (f);
	cb_debug_contents = l;

	f = CB_FIELD (lvl01_tree);
	cb_validate_field (f);
	CB_FIELD_ADD (current_program->working_storage, f);
}

static void
validate_record_depending (cb_tree x)
{
	struct cb_field		*p;
	cb_tree			r;

	/* get reference (and check if it exists) */
	r = cb_ref (x);
	if (r == cb_error_node) {
		return;
	}
#if 0 /* Simon: Why should we use a reference here? */
	if (CB_REF_OR_FIELD_P(x)) {
		cb_error_x (x, _("invalid RECORD DEPENDING item"));
		return;
	}
#else
	if (!CB_FIELD_P(r)) {
		cb_error_x (x, _("RECORD DEPENDING must reference a data-item"));
		return;
	}
#endif
	p = CB_FIELD_PTR (x);
	switch (p->storage) {
	case CB_STORAGE_WORKING:
	case CB_STORAGE_LOCAL:
	case CB_STORAGE_LINKAGE:
		break;
	default:
		/* RXWRXW - This breaks old legacy programs; FIXME: use compiler configuration */
		{
			enum cb_support	missing_compiler_config;
			if (!cb_relaxed_syntax_checks
			 || get_warn_opt_value (cb_warn_additional) == COBC_WARN_AS_ERROR) {
				missing_compiler_config = CB_ERROR;
			} else if (get_warn_opt_value (cb_warn_additional) == COBC_WARN_ENABLED) {
				missing_compiler_config = CB_WARNING;
			} else {
				missing_compiler_config = CB_OK;
			}
			cb_warning_dialect_x (missing_compiler_config, x,
				_("RECORD DEPENDING item '%s' should be defined in "
				  "WORKING-STORAGE, LOCAL-STORAGE or LINKAGE SECTION"), p->name);
		}
	}
}

static void
validate_relative_key_field (struct cb_file *file)
{
	struct cb_field	*key_field = CB_FIELD_PTR (file->key);

	if (CB_TREE_CATEGORY (key_field) != CB_CATEGORY_NUMERIC) {
		cb_error_x (file->key,
			    _("file %s: RELATIVE KEY %s is not numeric"),
			    file->name, key_field->name);
	}

	/* TODO: Check if key_field is an integer based on USAGE */
	if (key_field->pic != NULL) {
		if (key_field->pic->category == CB_CATEGORY_NUMERIC
		    && key_field->pic->scale != 0) {
			cb_error_x (file->key,
				    _("file %s: RELATIVE KEY %s must be integer"),
				    file->name, key_field->name);
		}
		if (key_field->pic->have_sign) {
			cb_error_x (file->key,
				    _("file %s: RELATIVE KEY %s must be unsigned"),
				    file->name, key_field->name);
		}
	}

	if (key_field->flag_occurs) {
		cb_error_x (file->key,
			    _("file %s: RELATIVE KEY %s cannot have OCCURS"),
			    file->name, key_field->name);
	}

	if (cb_field_founder (key_field)->file == file) {
		cb_error_x (file->key,
			    _("RELATIVE KEY %s cannot be in file record belonging to %s"),
			    key_field->name, file->name);
	}

#if 0 /* Simon: deemed to be not neccessary, see related bug #421 */
	if (key_field->storage != CB_STORAGE_WORKING
	 && key_field->storage != CB_STORAGE_FILE
	 && key_field->storage != CB_STORAGE_LOCAL) {
		cb_verify_x (file->key, cb_select_working,
			    _("file %s: RELATIVE KEY %s declared outside WORKING-STORAGE"),
			    file->name, key_field->name);
	}
#endif
}

static cb_tree
cb_validate_crt_status (cb_tree ref, cb_tree field_tree) {
	struct cb_field	*field;
	/* LCOV_EXCL_START */
	if (ref == NULL || !CB_REFERENCE_P (ref)) {
		cobc_err_msg (_("call to '%s' with invalid parameter '%s'"),
			"cb_validate_crt_status", "ref");
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */
	if (field_tree == NULL) {
		field_tree = cb_ref (ref);
	}
	if (field_tree == cb_error_node) {
		return NULL;
	}
	if (!CB_FIELD_P (field_tree)) {
		cb_error_x (ref, _("'%s' is not a valid data name"), cb_name (ref));
		return NULL;
	}
	field = CB_FIELD (field_tree);
	if (field->storage != CB_STORAGE_WORKING
	 && field->storage != CB_STORAGE_LOCAL) {
		cb_error_x (ref,
			_("CRT STATUS item '%s' should be defined in "
			  "WORKING-STORAGE or LOCAL-STORAGE"), field->name);
		return NULL;
	}
	if (CB_TREE_CATEGORY (field_tree) == CB_CATEGORY_NUMERIC) {
		if (field->size < 4) {
			cb_error_x (ref, _("'%s' numeric CRT STATUS must have at least 4 digits"),
				field->name);
			return NULL;
		}
	}
	else if (field->size != 3 && field->size != 4) {
		cb_error_x (ref, _("'%s' CRT STATUS must be 3 or 4 characters long"),
				field->name);
		return NULL;
	}
	return ref;
}

static void
validate_file_status (cb_tree fs)
{
	struct cb_field	*fs_field;
	enum cb_category category;

	cb_tree x = cb_ref (fs);

	/* TODO: If not defined, implicitly define PIC XX */
	if (x == cb_error_node) {
		return;
	}

	if (!CB_FIELD_P (x)
	 || CB_FIELD (x)->flag_constant) {
		cb_error_x (fs, _("FILE STATUS '%s' is not a field"), CB_NAME (fs));
		return;
	}

	fs_field = CB_FIELD (x);
	category = cb_tree_category (x);
	if (category == CB_CATEGORY_ALPHANUMERIC) {
		/* ok */
	} else if (category == CB_CATEGORY_NUMERIC) {
		if (fs_field->pic
		    && fs_field->pic->scale != 0) {
			cb_error_x (fs,
				_("FILE STATUS '%s' may not be a decimal or have a PIC with a P"),
				fs_field->name);
			return;
		}
		cb_warning_x (cb_warn_additional, fs,
			_("FILE STATUS '%s' is a numeric field, but I-O status codes are not numeric in general"),
			fs_field->name);
	} else {
		cb_error_x (fs,
			_("FILE STATUS '%s' must be an alphanumeric or numeric field"),
			fs_field->name);
		return;
	}

	if (fs_field->usage != CB_USAGE_DISPLAY) {
		cb_error_x (fs,
			_("FILE STATUS '%s' must be USAGE DISPLAY"),
			fs_field->name);
	}

	/* Check file status is two characters long */
	if (fs_field->size != 2) {
		cb_error_x (fs,
			_("FILE STATUS '%s' must be 2 characters long"),
			fs_field->name);
	}

	if (fs_field->storage != CB_STORAGE_WORKING
	 && fs_field->storage != CB_STORAGE_LOCAL
	 && fs_field->storage != CB_STORAGE_LINKAGE) {
		cb_error_x (fs,
			_("FILE STATUS '%s' must be in WORKING-STORAGE, LOCAL-STORAGE or LINKAGE"),
			fs_field->name);
	}

	if (fs_field->flag_odo_relative) {
		cb_error_x (fs,
			_("FILE STATUS '%s' may not be located after an OCCURS DEPENDING field"),
			fs_field->name);
	}
}

static void
create_implicit_assign_dynamic_var (struct cb_program * const prog,
				    cb_tree assign)
{
	cb_tree	x;
	struct cb_field	*p;
	const char	*assign_name = CB_NAME (assign);

	cb_warning (cb_warn_implicit_define,
		    _("variable '%s' will be implicitly defined"), CB_NAME (assign));
	x = cb_build_implicit_field (assign, COB_FILE_MAX);
	p = CB_FIELD (x);
#if 0
	p->count++;
#endif
	x = CB_TREE (build_literal (CB_CATEGORY_ALPHANUMERIC, assign_name, strlen (assign_name)));
	p->values = CB_LIST_INIT (x);
	CB_FIELD_ADD (prog->working_storage, p);
}

static void
process_undefined_assign_name (struct cb_file * const f,
			       struct cb_program * const prog)
{
	cb_tree	assign = f->assign;
	cb_tree	l;
	cb_tree	ll;

	if (f->assign_type != CB_ASSIGN_VARIABLE_DEFAULT) {
		/* An error is emitted later */
		return;
	}

	/*
	  Either create a variable or treat the assign name as an external-file-
	  name.
	*/
	if (cb_implicit_assign_dynamic_var) {
		cb_verify_x (CB_TREE (f), cb_assign_variable, _("ASSIGN [TO] variable in SELECT"));
		create_implicit_assign_dynamic_var (prog, assign);
	} else {
		/* Remove reference */
		for (l = prog->reference_list;
		     CB_VALUE (l) != assign && CB_VALUE (CB_CHAIN (l)) != assign;
		     l = CB_CHAIN (l));
		if (CB_VALUE (l) == assign) {
			prog->reference_list = CB_CHAIN (l);
		} else {
			ll = CB_CHAIN (CB_CHAIN (l));
			cobc_parse_free (CB_CHAIN (l));
			CB_CHAIN (l) = ll;
		}

		/* Reinterpret word */
		f->assign = build_external_assignment_name (assign);
	}
}

/* Ensure ASSIGN name refers to a valid identifier */
static void
validate_assign_name (struct cb_file * const f,
		      struct cb_program * const prog)
{
	cb_tree	assign = f->assign;
	cb_tree	x;
	struct cb_field	*p;

	if (!assign) {
		return;
	}

	if (!CB_REFERENCE_P (assign)) {
		return;
	}

	/* Error if assign name is same as a file name */
	for (x = prog->file_list; x; x = CB_CHAIN (x)) {
		if (!strcmp (CB_FILE (CB_VALUE (x))->name,
			     CB_NAME (assign))) {
			redefinition_error (assign);
		}
	}

	/* If assign is a 78-level, change assign to the 78-level's literal. */
	p = check_level_78 (CB_NAME (assign));
	if (p) {
		char *c = (char *)CB_LITERAL (p->values)->data;
		assign = CB_TREE (build_literal (CB_CATEGORY_ALPHANUMERIC, c, strlen (c)));
		f->assign = assign;
		return;
	}

	if (CB_WORD_COUNT (assign) == 0) {
		process_undefined_assign_name (f, prog);
	} else {
		/*
		  We now know we have a variable, so can validate whether it is
		  is allowed
		*/
		if (f->flag_assign_no_keyword) {
			cb_verify_x (CB_TREE (f), cb_assign_variable, _("ASSIGN variable"));
		}

		x = cb_ref (assign);
		if (CB_FIELD_P (x) && CB_FIELD (x)->level == 88) {
			cb_error_x (assign, _("ASSIGN data item '%s' is invalid"),
				    CB_NAME (assign));
		}
	}
}

void
cb_validate_program_data (struct cb_program *prog)
{
	cb_tree			l, x;
	struct cb_field		*p;
	struct cb_field		*q;
	struct cb_field		*field;
	char			buff[COB_MINI_BUFF];

	prog->report_list = cb_list_reverse (prog->report_list);

	for (l = prog->report_list; l; l = CB_CHAIN (l)) {
		/* Set up LINE-COUNTER / PAGE-COUNTER */
		struct cb_report	*rep = CB_REPORT (CB_VALUE (l));
		if (rep->line_counter == NULL) {
			snprintf (buff, (size_t)COB_MINI_MAX,
				  "LINE-COUNTER %s", rep->cname);
			x = cb_build_field (cb_build_reference (buff));
			CB_FIELD (x)->usage = CB_USAGE_UNSIGNED_INT;
			CB_FIELD (x)->values = CB_LIST_INIT (cb_zero);
			CB_FIELD (x)->count++;
			cb_validate_field (CB_FIELD (x));
			rep->line_counter = cb_build_field_reference (CB_FIELD (x), NULL);
			CB_FIELD_ADD (prog->working_storage, CB_FIELD (x));
		}
		if (rep->page_counter == NULL) {
			snprintf (buff, (size_t)COB_MINI_MAX,
				  "PAGE-COUNTER %s", rep->cname);
			x = cb_build_field (cb_build_reference (buff));
			CB_FIELD (x)->usage = CB_USAGE_UNSIGNED_INT;
			CB_FIELD (x)->values = CB_LIST_INIT (cb_zero);
			CB_FIELD (x)->count++;
			cb_validate_field (CB_FIELD (x));
			rep->page_counter = cb_build_field_reference (CB_FIELD (x), NULL);
			CB_FIELD_ADD (prog->working_storage, CB_FIELD (x));
		}
	}

	prog->file_list = cb_list_reverse (prog->file_list);

	for (l = prog->file_list; l; l = CB_CHAIN (l)) {
		struct cb_file		*file;
		file = CB_FILE (CB_VALUE (l));
		if (!file->flag_finalized) {
			finalize_file (file, NULL);
		}
	}

	/* Build undeclared assignment names now */
	for (l = prog->file_list; l; l = CB_CHAIN (l)) {
		validate_assign_name (CB_FILE (CB_VALUE (l)), prog);
	}

	if (prog->cursor_pos) {
		x = cb_ref (prog->cursor_pos);
		if (x == cb_error_node) {
			prog->cursor_pos = NULL;
		} else if (CB_FIELD(x)->size != 6 && CB_FIELD(x)->size != 4) {
			cb_error_x (prog->cursor_pos,
					_("'%s' CURSOR must be 4 or 6 characters long"),
				    cb_name (prog->cursor_pos));
			prog->cursor_pos = NULL;
		} else {
			prog->cursor_pos = x;
		}
	}
	if (prog->crt_status) {
		prog->crt_status = cb_validate_crt_status (prog->crt_status, NULL);
	} else {
		/* TODO: Add to registers list */
		l = cb_build_reference ("COB-CRT-STATUS");
		x = cb_try_ref (l);
		if (x == cb_error_node) {
			p = CB_FIELD (cb_build_field (l));
			p->usage = CB_USAGE_DISPLAY;
			p->pic = cb_build_picture ("9(4)");
			cb_validate_field (p);
			p->flag_no_init = 1;
			/* Do not initialize/bump ref count here
			p->values = CB_LIST_INIT (cb_zero);
			p->count++;
			*/
			CB_FIELD_ADD (prog->working_storage, p);
			prog->crt_status = l;
		} else {
			prog->crt_status = cb_validate_crt_status (l, x);
		}
	}

	/* Resolve all references so far */
	for (l = cb_list_reverse (prog->reference_list); l; l = CB_CHAIN (l)) {
		cb_ref (CB_VALUE (l));
	}

	/* Check ODO items */
	for (l = cb_depend_check; l; l = CB_CHAIN (l)) {
		struct cb_field		*depfld = NULL;
		unsigned int		odo_level = 0, parent_is_pic_l;
		cb_tree	xerr = NULL;
		x = CB_VALUE (l);
		if (x == NULL || x == cb_error_node) {
			continue;
		}
		q = CB_FIELD_PTR (x);
		if (cb_validate_one (q->depending)) {
			q->depending = cb_error_node;
		} else if (cb_ref (q->depending) != cb_error_node) {
			cb_tree dep_x = q->depending;
			if (cb_tree_category (dep_x) != CB_CATEGORY_NUMERIC) {
				cb_error_x (dep_x, _ ("'%s' is not numeric"), cb_name (dep_x));
				q->depending = cb_error_node;
			} else {
				depfld = CB_FIELD_PTR (q->depending);
			}
			if (chk_field_variable_address (depfld) ) {
				if (cb_depending_on_not_fixed == CB_WARNING) {
					cb_warning_x (COBC_WARN_FILLER, CB_TREE (depfld),
						      _("%s does not have a fixed location"),depfld->name);
				} else
				if (cb_depending_on_not_fixed == CB_ERROR) {
					cb_error_x (CB_TREE (depfld),
						_("%s does not have a fixed location"),depfld->name);
				}
			}
		}
		/* Direct parent being PIC L means we are checking an implicit
		   FILLER with ODO: this permits nested ODO and further sister
		   fields. */
		parent_is_pic_l = q->parent && q->parent->flag_picture_l;
		/* The data item that contains a OCCURS DEPENDING clause must be
		   the last data item in the group */
		for (p = q; ; p = p->parent) {
			if (p->depending) {
				if (odo_level > 0
				 && !cb_odoslide
				 && !parent_is_pic_l) {
					xerr = x;
					cb_error_x (x,
						_("'%s' cannot have nested OCCURS DEPENDING"),
						cb_name (x));
				}
				odo_level++;
			}
			p->odo_level = odo_level;
			if (!p->parent) {
				break;
			}
			for (; p->sister; p = p->sister) {
				if (p->sister->level == 66) continue;
				if (p->sister == depfld && !parent_is_pic_l && x != xerr) {
					xerr = x;
					cb_error_x (x,
					    _("'%s' OCCURS DEPENDING ON field item invalid here"),
						p->sister->name);
				}
				if (!p->sister->redefines) {
					if (!cb_odoslide
					 && !cb_complex_odo
					 && !parent_is_pic_l
					 && x != xerr) {
						xerr = x;
						cb_error_x (x,
							_("'%s' cannot have OCCURS DEPENDING because of '%s'"),
							cb_name (x), p->sister->name);
						break;
					}
					p->flag_odo_relative = 1;
				}
			}
		}

		/* If the field is GLOBAL, then the ODO must also be GLOBAL */
		if (q->flag_is_global && depfld) {
			if (!depfld->flag_is_global) {
				cb_error_x (x, _("'%s' OCCURS DEPENDING ON item must have GLOBAL attribute"),
					depfld->name);
			}
		}
	}
	cb_depend_check = NULL;
	cb_needs_01 = 0;

	/* file definition checks */
	for (l = prog->file_list; l; l = CB_CHAIN (l)) {
		struct cb_file	*file = CB_FILE (CB_VALUE (l));
		if (file->flag_external) {
			if (CB_VALID_TREE (file->password)
				&& !CB_FIELD (cb_ref(file->password))->flag_external) {
				cb_error_x (file->password, _("PASSWORD '%s' for EXTERNAL file '%s' must have EXTERNAL attribute"),
					CB_NAME (file->password), file->name);
			}
		}
		if (CB_VALID_TREE (file->record_depending)) {
			validate_record_depending (file->record_depending);
		}
		if (file->organization == COB_ORG_RELATIVE && file->key
		 && cb_ref (file->key) != cb_error_node) {
			validate_relative_key_field (file);
		}
		if (file->file_status) {
			validate_file_status (file->file_status);
		}
	}

	/* check alphabets */
	for (l = current_program->alphabet_name_list; l; l = CB_CHAIN(l)) {
		struct cb_alphabet_name *alphabet = CB_ALPHABET_NAME (CB_VALUE(l));
		if (alphabet->alphabet_type == CB_ALPHABET_LOCALE) {
			x = cb_ref (alphabet->custom_list);
			if (x != cb_error_node && !CB_LOCALE_NAME_P(x)) {
				cb_error_x (alphabet->custom_list, _("'%s' is not a locale-name"),
					cb_name(x));
				alphabet->custom_list = cb_error_node;
			}
		}
	}

	/* Resolve APPLY COMMIT  */
	if (CB_VALID_TREE(prog->apply_commit)) {
		for (l = prog->apply_commit; l; l = CB_CHAIN(l)) {
			cb_tree	l2 = CB_VALUE (l);
			x = cb_ref (l2);
			if (x != cb_error_node) {
				for (l2 = prog->apply_commit; l2 != l; l2 = CB_CHAIN(l2)) {
					if (cb_ref (CB_VALUE (l2)) == x) {
						cb_error_x (l,
							_("duplicate APPLY COMMIT target: '%s'"),
							cb_name (CB_VALUE (l)));
						x = cb_error_node;
						break;
					}
				}
			}
			if (x == cb_error_node) {
				continue;
			}
			if (CB_FILE_P (x)) {
				struct cb_file	*file = CB_FILE (x);
				if (file->organization == COB_ORG_SORT) {
					cb_error_x (l,
						_("APPLY COMMIT statement invalid for SORT file"));
				} else if (file->flag_report) {
					cb_error_x (l,
						_("APPLY COMMIT statement invalid for REPORT file"));
				} else {
					file->lock_mode |= (COB_LOCK_ROLLBACK|COB_LOCK_MULTIPLE);
				}
			} else if (CB_FIELD_P (x)) {
				field = CB_FIELD (x);
				if (field->storage != CB_STORAGE_WORKING
				 && field->storage != CB_STORAGE_LOCAL) {
					cb_error_x (l,
						_("APPLY COMMIT item '%s' should be defined in "
							"WORKING-STORAGE or LOCAL-STORAGE"), field->name);
				}
				cb_warning_x (COBC_WARN_FILLER, x, 
							_("APPLY COMMIT %s; not implemented"),field->name);
				if (field->level != 01 && field->level != 77) {
					cb_error_x (l, _("'%s' not level 01 or 77"), field->name);
#if 0 /* currently not part of the rules */
				} else if (field->flag_item_based || field->flag_external) {
					cb_error_x (l, _("'%s' cannot be BASED/EXTERNAL"), field->name);
#endif
				} else if (field->redefines) {
					cb_error_x (l, _("'%s' REDEFINES field not allowed here"),
						field->name);
				}
			} else {
				cb_error_x (l, _("item not allowed here: '%s'"), cb_name (x));
			}
		}
	}
}


static int
error_if_subscript_or_refmod (cb_tree ref, const char *name)
{
	int	error = 0;

	if (CB_REFERENCE (ref)->subs) {
		cb_error_x (ref, _("%s may not be subscripted"), name);
		error = 1;
	}
	if (CB_REFERENCE (ref)->offset) {
		cb_error_x (ref, _("%s may not be reference modified"), name);
		error = 1;
	}

	return error;
}

static int
has_sub_reference (struct cb_field *fld)
{
	struct cb_field		*f;

	if (fld->count) {
		return 1;
	}
	if (fld->validation) {
		for (f = fld->validation; f; f = f->sister) {
			if (f->count) {
				return 1;
			}
		}
	} else {
		for (f = fld->children; f; f = f->sister) {
			if (has_sub_reference (f)) {
				return 1;
			}
		}
		for (f = fld->sister; f; f = f->sister) {
			if (f->redefines == fld) {
				if (has_sub_reference (f)) {
					return 1;
				}
			}
		}
	}
	return 0;
}

/* Resolve DEBUG references, return necessary size for DEBUG-CONTENTS */
static int
cb_resolve_debug_refs (struct cb_program *prog, int size)
{
	cb_tree		l;
	cb_tree		x;
	cb_tree		v;

	/* For data items, we may need to adjust the size of DEBUG-CONTENTS directly,
	   for file items from its maximum length */
	for (l = prog->debug_list; l; l = CB_CHAIN (l)) {
		x = CB_VALUE (l);
		(void)cb_set_ignore_error (CB_REFERENCE (x)->flag_ignored);
		v = cb_ref (x);
		if (v == cb_error_node) {
			continue;
		}
		current_section = CB_REFERENCE (x)->section;
		current_paragraph = CB_REFERENCE (x)->paragraph;
		switch (CB_TREE_TAG (v)) {
		case CB_TAG_LABEL:
			if (!CB_LABEL (v)->flag_real_label) {
				cb_error_x (x, _("DEBUGGING target invalid: '%s'"),
					    cb_name (x));
			} else if (CB_LABEL (v)->flag_debugging_mode) {
				cb_error_x (x, _("duplicate DEBUGGING target: '%s'"),
					    cb_name (x));
			} else if (prog->all_procedure) {
				cb_error_x (x, _("DEBUGGING target already specified with ALL PROCEDURES: '%s'"),
					    cb_name (x));
				CB_LABEL (v)->flag_debugging_mode = 1;
			} else {
				CB_LABEL (v)->debug_section =
					CB_REFERENCE (x)->debug_section;
				CB_LABEL (v)->flag_debugging_mode = 1;
			}
			break;
		case CB_TAG_FILE:
			if (CB_FILE (v)->record_max > size) {
				size = CB_FILE (v)->record_max;
			}
			break;
		case CB_TAG_CD:
			if (CB_CD (v)->record && CB_CD (v)->record->size > size) {
				size = CB_CD(v)->record->size;
			}
			break;
		case CB_TAG_FIELD:
			if (!error_if_subscript_or_refmod (x, _("DEBUGGING target"))) {
				if (CB_FIELD (v)->size > size) {
					size = CB_FIELD (v)->size;
				}
			}
			break;
		default:
			cb_error_x (x, _("'%s' is not a valid DEBUGGING target"),
				    cb_name (x));
			break;
		}
	}
	/* reset error handling */
	cb_set_ignore_error (0);

	return size;
}

/* Resolve all labels */
static void
cb_validate_labels (struct cb_program *prog)
{
	cb_tree		l;

	for (l = cb_list_reverse (prog->label_list); l; l = CB_CHAIN (l)) {
		const cb_tree x = CB_VALUE (l);
		const struct cb_reference *ref = CB_REFERENCE (x);
		cb_tree v;   /* note: can't be set here,
		                because must be done after set_ignore_error */
		(void)cb_set_ignore_error (ref->flag_ignored);
		v = cb_ref (x);
		/* cb_error_node -> reference not defined, message raised in cb_ref() */
		if (v == cb_error_node) {
			continue;
		}
		current_section = ref->section;
		current_paragraph = ref->paragraph;
		/* Check refs in to / out of DECLARATIVES */
		if (CB_LABEL_P (v)) {
			struct cb_label *label = CB_LABEL (v);

			label->flag_begin = 1;
			if (ref->length) {
				label->flag_return = 1;
			}

			if (ref->flag_in_decl
			 && !label->flag_declaratives) {
				/* verify reference-out-of-declaratives  */
				switch (cb_reference_out_of_declaratives) {
				case CB_OK:
					break;
				case CB_ERROR:
					cb_error_x (x, _("'%s' is not in DECLARATIVES"),
						    label->name);
					continue;
				case CB_WARNING:
					if (get_warn_opt_value (cb_warn_dialect) == COBC_WARN_DISABLED) {
						break;
					}
					cb_warning_x (cb_warn_dialect, x,
						    _("'%s' is not in DECLARATIVES"),
							label->name);
					continue;
				default:
					break;
				}
			}

			/* checks for GO TO */
			if (ref->statement == STMT_GO_TO) {

				/* GO TO into DECLARATIVES is not allowed */
				if (label->flag_declaratives
				 && !ref->flag_in_decl) {
					cb_error_x (x, _("invalid reference to '%s' (in DECLARATIVES)"),
						CB_LABEL (v)->name);
					continue;
				}

				/* check for warning options "house-rules" relevant for later optimizations */
				if (label->flag_section) {
					if (label != current_section) {
						cb_warning_x (cb_warn_goto_section, x,
							"GO TO SECTION '%s'", label->name);
					}
				} else if (label->section != current_section) {
					char qualified_name[COB_MAX_WORDLEN * 2 + 4 + 1];
					cb_warning_x (cb_warn_goto_different_section, x,
						_("GO TO paragraph '%s' which is defined in another SECTION"),
						label->name);
					sprintf (qualified_name, "%s IN %s", label->name, label->section->name);
					cb_note_x (cb_warn_goto_different_section, v,
						_("'%s' defined here"), qualified_name);
				}

			}

		} else {
			cb_error_x (x, _("'%s' is not a procedure name"), cb_name (x));
		}
	}
	/* reset error handling */
	cb_set_ignore_error (0);
}

/* Validate range of all PERFORM THRU */
static void
cb_validate_perform_thru_ranges (struct cb_program *prog)
{
	cb_tree		l;
	if (!cb_flag_section_exit_check
	 && get_warn_opt_value (cb_warn_suspicious_perform_thru) == COBC_WARN_DISABLED) {
		return;
	}
	for (l = prog->perform_thru_list; l; l = CB_CHAIN (l)) {
		const cb_tree v = CB_VALUE (l);
		const cb_tree x = cb_ref (CB_PAIR_X (v));
		const cb_tree y = cb_ref (CB_PAIR_Y (v));
		if (x != y
		 && x != cb_error_node
		 && y != cb_error_node) {
			const struct cb_label *lb = CB_LABEL (x);
			const struct cb_label *le = CB_LABEL (y);
			if (le->flag_section) {
				if (cb_flag_section_exit_check) {
					cb_warning_x (COBC_WARN_FILLER, v,
						_("%s and %s are mutually exclusive"),
						"PERFORM ... THROUGH SECTION", "-fsection-exit-check");
					/* this code would always raise that check, so disable */
					cb_flag_section_exit_check = 0;
				}
			} else if (le->section != lb->section && le->section != lb) {
				cb_warning_x (cb_warn_suspicious_perform_thru, v,
					_("%s and %s are not in the same SECTION"), lb->name, le->name);
			}
			if (le->common.source_file == lb->common.source_file
			 && le->common.source_line < lb->common.source_line) {
				cb_warning_x (cb_warn_suspicious_perform_thru, v,
					_("%s is defined before %s"), le->name, lb->name);
				cb_note_x (cb_warn_suspicious_perform_thru, x, _("'%s' defined here"), lb->name);
				cb_note_x (cb_warn_suspicious_perform_thru, y, _("'%s' defined here"), le->name);
			}
		}
	}
}

void
cb_validate_program_body (struct cb_program *prog)
{
	cb_tree			l;
	cb_tree			x;
	cb_tree			v;
	struct cb_label		*save_section;
	struct cb_label		*save_paragraph;
	struct cb_alter_id	*aid;
	struct cb_label		*l1;
	struct cb_label		*l2;
	struct cb_field		*f, *ret_fld;

#if 0	/* Check reference to ANY LENGTH items */
	for (f = prog->linkage_storage; f; f = f->sister) {
		/* only check fields with ANY LENGTH;
			RETURNING is already a valid reference */
		if (!f->flag_any_length
		  || f->flag_internal_register
		  || f->flag_is_returning) {
			continue;
		}

		/* ignore fields that are part of main entry USING */
		for (l = CB_VALUE (CB_VALUE (prog->entry_list)); l; l = CB_CHAIN (l)) {
			x = CB_VALUE (l);
			if (CB_VALID_TREE (x) && cb_ref (x) != cb_error_node) {
				if (f == CB_FIELD (cb_ref (x))) {
					break;
				}
			}
		}
		if (!l) {
			cb_error_x (CB_TREE (f),
				_("'%s' ANY LENGTH item must be a formal parameter"),
				f->name);
		}
	}
#endif /* TODO: recheck later */

	/* Validate entry points */

	/* Check dangling LINKAGE items */
	if (get_warn_opt_value (cb_warn_linkage) != COBC_WARN_DISABLED
	 && prog->linkage_storage) {
		if (prog->returning
		 && cb_ref (prog->returning) != cb_error_node) {
			ret_fld = CB_FIELD (cb_ref (prog->returning));
			if (ret_fld->redefines) {
				/* error, but we check this in parser.y already and just go on here */
				ret_fld = ret_fld->redefines;
			}
		} else {
			ret_fld = NULL;
		}
		for (v = prog->entry_list; v; v = CB_CHAIN (v)) {
			for (f = prog->linkage_storage; f; f = f->sister) {

				/* ignore RETURNING fields, fields that REDEFINES
				   and internal registers */
				if (f == ret_fld
				 || f->redefines
				 || f->flag_internal_register) {
					continue;
				}

				/* ignore fields that are part of current entry USING */
				for (l = CB_VALUE (CB_VALUE (v)); l; l = CB_CHAIN (l)) {
					x = CB_VALUE (l);
					if (CB_VALID_TREE (x) && cb_ref (x) != cb_error_node) {
						if (f == CB_FIELD (cb_ref (x))) {
							break;
						}
					}
				}
				if (l) {
					continue;
				}

				/* check if field or its children have any actual reference,
				   otherwise the warning is useless */
				if (has_sub_reference(f)) {
					cb_warning_x (cb_warn_linkage, CB_TREE (f),
						_("LINKAGE item '%s' is not a PROCEDURE USING parameter"), f->name);
				}
			}
		}
	}

	save_section = current_section;
	save_paragraph = current_paragraph;

	/* Resolve all labels */
	cb_validate_labels (prog);

	/* check for overlapping PERFORM ranges */
	cb_validate_perform_thru_ranges (prog);

	if (prog->flag_debugging) {
		/* Resolve DEBUGGING references and calculate DEBUG-CONTENTS size */
		/* Basic size of DEBUG-CONTENTS is DFLT_DEBUG_CONTENTS_SIZE */
		int debug_contents_size = cb_resolve_debug_refs (prog, DFLT_DEBUG_CONTENTS_SIZE);

		/* If necessary, adjust size of DEBUG-CONTENTS (and DEBUG-ITEM) */
		if (debug_contents_size != DFLT_DEBUG_CONTENTS_SIZE) {
			f = CB_FIELD_PTR (cb_debug_contents);
			f->size = debug_contents_size;
			f->memory_size = debug_contents_size;

			f = CB_FIELD_PTR (cb_debug_item);
			f->size += debug_contents_size - DFLT_DEBUG_CONTENTS_SIZE;
			f->memory_size += debug_contents_size - DFLT_DEBUG_CONTENTS_SIZE;
		}
	}

	/* Build ALTER ids - We need to remove duplicates */
	for (l = prog->alter_list; l; l = CB_CHAIN (l)) {
		if (CB_PURPOSE (l) == cb_error_node) {
			continue;
		}
		if (CB_VALUE (l) == cb_error_node) {
			continue;
		}
		x = CB_PURPOSE (l);
		v = CB_VALUE (l);
		if (CB_REFERENCE (x)->value == cb_error_node
		 || CB_REFERENCE (x)->flag_ignored) {
			continue;
		}
		if (CB_REFERENCE (v)->value == cb_error_node
		 || CB_REFERENCE (v)->flag_ignored) {
			continue;
		}
		l1 = CB_LABEL (CB_REFERENCE (x)->value);
		l2 = CB_LABEL (CB_REFERENCE (v)->value);
		current_section = CB_REFERENCE (x)->section;
		current_paragraph = CB_REFERENCE (x)->paragraph;
		/* First statement in paragraph must be a GO TO */
		if (!l1->flag_first_is_goto) {
			cb_error_x (x, _("'%s' is not an alterable paragraph"),
				    l1->name);
			continue;
		}
		for (aid = l1->alter_gotos; aid; aid = aid->next) {
			if (aid->goto_id == l2->id) {
				break;
			}
		}
		if (!aid) {
			aid = cobc_parse_malloc (sizeof(struct cb_alter_id));
			aid->next = l1->alter_gotos;
			aid->goto_id = l2->id;
			l1->alter_gotos = aid;
		}
		for (aid = prog->alter_gotos; aid; aid = aid->next) {
			if (aid->goto_id == l1->id) {
				break;
			}
		}
		if (!aid) {
			aid = cobc_parse_malloc (sizeof(struct cb_alter_id));
			aid->next = prog->alter_gotos;
			aid->goto_id = l1->id;
			prog->alter_gotos = aid;
		}
	}

	current_section = save_section;
	current_paragraph = save_paragraph;
	cobc_cs_check = 0;

	prog->exec_list = cb_list_reverse (prog->exec_list);
}

/* General */

static COB_INLINE COB_A_INLINE void
cb_copy_source_reference (cb_tree target, cb_tree x)
{
	SET_SOURCE(target, x->source_file, x->source_line);
	target->source_column = x->source_column;
}

/* Expressions */

static void
build_expr_init (void)
{
	if (initialized == 0) {
		initialized = 1;
		/* Init stack */
		expr_stack_size = START_STACK_SIZE;
		expr_stack = cobc_main_malloc (sizeof (struct expr_node) * START_STACK_SIZE);
	} else {
		memset (expr_stack, 0, expr_stack_size * sizeof (struct expr_node));
	}
	expr_op = 0;
	expr_lh = NULL;
	expr_index = TOPSTACK;
}

static int
expr_chk_cond (cb_tree expr_1, cb_tree expr_2)
{
	struct cb_field		*f1;
	struct cb_field		*f2;
	int			is_ptr_1;
	int			is_ptr_2;

	/* 88 level is invalid here */
	/* Likewise combination of pointer and non-pointer */
	is_ptr_1 = 0;
	is_ptr_2 = 0;
	if (CB_REF_OR_FIELD_P (expr_1)) {
		f1 = CB_FIELD_PTR (expr_1);
		if (f1->level == 88) {
			return 1;
		}
		if (f1->flag_is_pointer) {
			is_ptr_1 = 1;
		}
	} else if (CB_CAST_P (expr_1)) {
		switch (CB_CAST (expr_1)->cast_type) {
		case CB_CAST_ADDRESS:
		case CB_CAST_ADDR_OF_ADDR:
		case CB_CAST_PROGRAM_POINTER:
			is_ptr_1 = 1;
			break;
		default:
			break;
		}
	} else if (expr_1 == cb_null) {
		is_ptr_1 = 1;
	}
	if (CB_REF_OR_FIELD_P (expr_2)) {
		f2 = CB_FIELD_PTR (expr_2);
		if (f2->level == 88) {
			return 1;
		}
		if (f2->flag_is_pointer) {
			is_ptr_2 = 1;
		}
	} else if (CB_CAST_P (expr_2)) {
		switch (CB_CAST (expr_2)->cast_type) {
		case CB_CAST_ADDRESS:
		case CB_CAST_ADDR_OF_ADDR:
		case CB_CAST_PROGRAM_POINTER:
			is_ptr_2 = 1;
			break;
		default:
			break;
		}
	} else if (expr_2 == cb_null) {
		is_ptr_2 = 1;
	}
	return is_ptr_1 ^ is_ptr_2;
}

/* Reduce expressions of the left of 'token' that have higher priority */
static int
build_expr_reduce (int token)
{
	/* Example:
	 * index: -3  -2  -1   0      (relative position to expr_index)
	 * token: 'x' '*' 'x' '+'
	 becomes
	 * index:  -3   -2            (and then, expr_index -= 2)
	 * token: 'x*x' '+'
	 */

	while (expr_prio[TOKEN (-2)] <= expr_prio[token]) {
		enum cb_binary_op_op op;

		switch (TOKEN (-2)) {
		case 'x':
		case '(':
		case ')':
			/* no binary op, nothing more to do */
			return 0;
		default:
			op = TOKEN (-2);
			break;
		}

		/* Reduce the expression depending on the last operator */
		switch (op) {

		case 'a': case 'o': case 'e': case 'l': case 'r': /* BIT-WISE */
		case '+': case '-': case '*': case '/': case '^':
			/* Arithmetic operators: 'x' op 'x' */
			if (TOKEN (-1) != 'x' || TOKEN (-3) != 'x') {
				return -1;
			}
			TOKEN (-3) = 'x';
			VALUE (-3) = cb_build_binary_op (VALUE (-3), op, VALUE (-1));
			expr_index -= 2;
			break;

		case '!':
		case 'n':  /* BIT-WISE */
			/* Negation: '!' 'x' */
			if (TOKEN (-1) != 'x') {
				return -1;
			}
			/* 'x' '=' 'x' '|' '!' 'x' */
			if (expr_lh) {
				if (CB_TREE_CLASS (VALUE (-1)) != CB_CLASS_BOOLEAN) {
					VALUE (-1) = cb_build_binary_op (expr_lh, expr_op, VALUE (-1));
				}
			}
			TOKEN (-2) = 'x';
			VALUE (-2) = CB_BUILD_NEGATION (VALUE (-1));
			expr_index -= 1;
			break;

		case '&':
		case '|':
			/* Logical AND/OR: 'x' op 'x' */
			if (TOKEN (-1) != 'x' || TOKEN (-3) != 'x') {
				return -1;
			}
			/* 'x' '=' 'x' '|' 'x' */
			if (expr_lh) {
				if (CB_TREE_CLASS (VALUE (-1)) != CB_CLASS_BOOLEAN) {
					VALUE (-1) = cb_build_binary_op (expr_lh, expr_op, VALUE (-1));
				}
				if (CB_TREE_CLASS (VALUE (-3)) != CB_CLASS_BOOLEAN) {
					VALUE (-3) = cb_build_binary_op (expr_lh, expr_op, VALUE (-3));
				}
			}
			TOKEN (-3) = 'x';
			VALUE (-3) = cb_build_binary_op (VALUE (-3), op,
							 VALUE (-1));
			expr_index -= 2;
			break;

		default:
			{
				cb_tree lhs;
				/* Relational operators */
				if (TOKEN (-1) != 'x') {
					return -1;
				}
				lhs = VALUE (-1);

				if (TOKEN (-3) == '!') {
					enum cb_binary_op_op new_token = 0;
					/* '!' '=' --> '~', etc. */
					switch (op) {
					case '=':
						new_token = '~';
						break;
					case '~':
						new_token = '=';
						break;
					case '<':
						new_token = ']';
						break;
					case '>':
						new_token = '[';
						break;
					case '[':
						new_token = '>';
						break;
					case ']':
						new_token = '<';
						break;
					default:
						break;
					}
					if (new_token != 0) {
						op = new_token;
						cb_next_binary_op_flag = cb_next_binary_op_flag == 0 ? BOP_OPERANDS_SWAPPED : 0;
						expr_index -= 1;
					}
				}
				/* Fall-through */
				switch (TOKEN (-3)) {
				case 'x':
					/* Simple condition: 'x' op 'x' */
					if (VALUE (-3) == cb_error_node ||
						lhs == cb_error_node) {
						VALUE (-3) = cb_error_node;
					} else {
						expr_lh = VALUE (-3);
						if (expr_chk_cond (expr_lh, lhs)) {
							VALUE (-3) = cb_error_node;
							return 1;
						}
						expr_op = op;
						TOKEN (-3) = 'x';
						if (CB_TREE_CLASS (lhs) != CB_CLASS_BOOLEAN) {
							VALUE (-3) = cb_build_binary_op (expr_lh, op, lhs);
#if 0						/* Note:   We loose the source reference here if
									   the result is true/false, for example because of
									   comparing 'A' = 'B'. As we now have cb_false
									   in VALUE (-3) we should not add the reference there.
							  CHECKME: Should we store the value as PAIR with a new
									   cb_tree containing the reference and unpack it
									   everywhere or is there a better option to find?
							 See:     Test syn_misc.at - Constant Expressions (2)
							*/
							cb_copy_source_reference (VALUE (-3), expr_lh);
#endif
						} else {
							VALUE (-3) = lhs;
						}
					}
					expr_index -= 2;
					break;
				case '&':
				case '|':
					/* Complex condition: 'x' '=' 'x' '|' op 'x' */
					if (lhs == cb_error_node) {
						VALUE (-2) = cb_error_node;
					} else {
						expr_op = op;
						TOKEN (-2) = 'x';
						if (CB_TREE_CLASS (lhs) != CB_CLASS_BOOLEAN && expr_lh) {
							VALUE (-2) = cb_build_binary_op (expr_lh, op, lhs);
						} else {
							VALUE (-2) = lhs;
						}
					}
					expr_index -= 1;
					break;
				default:
					return -1;
				}
				break;
			}
		}
	}

	/* Handle special case "op OR x AND" */
	if (token == '&' && TOKEN (-2) == '|'
	 && CB_TREE_CLASS (VALUE (-1)) != CB_CLASS_BOOLEAN) {
		/* LCOV_EXCL_START */
		if (!expr_lh) {
			/* untranslated as highly unlikely to be raised */
			cobc_err_msg ("missing left-hand-expression");
			COBC_ABORT ();
		}
		/* LCOV_EXCL_STOP */
		TOKEN (-1) = 'x';
		VALUE (-1) = cb_build_binary_op (expr_lh, expr_op, VALUE (-1));
	}

	return 0;
}

static void
build_expr_shift_sign (const int op)
{
	int	have_not;

	if (TOKEN (-1) == '!') {
		have_not = 1;
		expr_index--;
	} else {
		have_not = 0;
	}
	(void)build_expr_reduce ('=');
	if (TOKEN (-1) == 'x') {
		VALUE (-1) = cb_build_binary_op (VALUE (-1), op, cb_zero);
		if (have_not) {
			VALUE (-1) = CB_BUILD_NEGATION (VALUE (-1));
		}
	}
}

static void
build_expr_shift_class (const char *name)
{
	int	have_not;

	if (TOKEN (-1) == '!') {
		have_not = 1;
		expr_index--;
	} else {
		have_not = 0;
	}
	(void)build_expr_reduce ('=');
	if (TOKEN (-1) == 'x') {
		VALUE (-1) = CB_BUILD_FUNCALL_1 (name, VALUE (-1));
		if (have_not) {
			VALUE (-1) = CB_BUILD_NEGATION (VALUE (-1));
		}
	}
}

static int
binary_op_is_relational (const struct cb_binary_op * const op)
{
	return op->op == '='
		|| op->op == '>'
		|| op->op == '<'
		|| op->op == '['
		|| op->op == ']'
		|| op->op == '~';
}

static void
build_expr_shift (int token, cb_tree value)
{
	switch (token) {
	case 'M':
		break;
	case 'x':
		/* Sign ZERO condition */
		if (value == cb_zero) {
			if (TOKEN (-1) == 'x' || TOKEN (-1) == '!') {
				build_expr_shift_sign ('=');
				return;
			}
		}

		/* Unary sign */
		if ((TOKEN (-1) == '+' || TOKEN (-1) == '-')
		 &&  TOKEN (-2) != 'x') {
			if (TOKEN (-1) == '-') {
				value = cb_build_binary_op (cb_zero, '-', value);
			}
			expr_index -= 1;
		}
		break;

	case '(':
		/* 'x' op '(' --> '(' 'x' op */
		switch (TOKEN (-1)) {
		case '=':
		case '~':
		case '<':
		case '>':
		case '[':
		case ']':
			expr_op = TOKEN (-1);
			if (TOKEN (-2) == 'x') {
				expr_lh = VALUE (-2);
			}
			break;
		default:
			break;
		}
		break;

	case ')':
		/* Enclosed by parentheses */
		(void)build_expr_reduce (token);
		if (VALUE (-1)
		 && CB_BINARY_OP_P (VALUE (-1))
		 && binary_op_is_relational (CB_BINARY_OP (VALUE (-1)))) {
			/*
			  If a relation is surrounded in parentheses, it cannot
			  be the start of an abbreviated condition.
			*/
			expr_lh = NULL;
		}
		if (TOKEN (-2) == '(') {
			if (VALUE (-1)) {
				value = CB_BUILD_PARENTHESES (VALUE (-1));
			} else {
				value = NULL;
			}
			expr_index -= 2;
			build_expr_shift ('x', value);
			return;
		}
		break;

	default:
		/* '<' '|' '=' --> '[' */
		/* '>' '|' '=' --> ']' */
		if (token == '=' && TOKEN (-1) == '|' &&
		    (TOKEN (-2) == '<' || TOKEN (-2) == '>')) {
			token = (TOKEN (-2) == '<') ? '[' : ']';
			expr_index -= 2;
		}
		break;
	}

	/* Reduce */
	/* Catch invalid condition */
	if (build_expr_reduce (token) > 0) {
		return;
	}

	/* Allocate sufficient stack memory */
	if (expr_index >= expr_stack_size) {
		while (expr_stack_size <= expr_index) {
			expr_stack_size *= 2;
		}
		expr_stack = cobc_main_realloc (expr_stack, sizeof (struct expr_node) * expr_stack_size);
	}

	/* Put on the stack */
	TOKEN (0) = token;
	VALUE (0) = value;
	expr_index++;
}

/* shortcut parentheses within the generated expression */
static void
build_expr_expand (cb_tree *x)
{
	struct cb_binary_op	*p;

start:
	/* Remove parentheses */
	if (CB_BINARY_OP_P (*x)) {
		p = CB_BINARY_OP (*x);
		if (p->op == '@') {
			*x = p->x;
			goto start;
		}
		build_expr_expand (&p->x);
		if (p->y) {
			build_expr_expand (&p->y);
		}
	}
}

static cb_tree
build_expr_finish (void)
{
	/* Reduce all (prio of token 0 is smaller than all other ones) */
	(void)build_expr_reduce (0);

	SET_SOURCE(expr_stack[TOPSTACK].value, cb_source_file, cb_exp_line);

	if (expr_index != TOPSTACK+1) {
		cb_error_x (expr_stack[TOPSTACK].value, _("invalid expression: unfinished expression"));
		return cb_error_node;
	}

	if (!expr_stack[TOPSTACK].value) {
		/* TODO: Add test case for this to syn_misc.at invalid expression */
		cb_error (_("invalid expression"));
		return cb_error_node;
	}

	build_expr_expand (&expr_stack[TOPSTACK].value);
	if (expr_stack[TOPSTACK].token != 'x') {
		/* TODO: add a test case, for now, no idea how to reach this */
		cb_error_x (expr_stack[TOPSTACK].value, _("invalid expression"));
		return cb_error_node;
	}

	return expr_stack[TOPSTACK].value;
}

cb_tree
cb_build_expr (cb_tree list)
{
	cb_tree	l, v;
	struct cb_field	*f;
	int	op, has_rel, has_con, has_var, bad_cond;

	build_expr_init ();

	/* Checkme: maybe add validate_list(l) here */

	bad_cond = has_rel = has_con = has_var = 0;
	for (l = list; l; l = CB_CHAIN (l)) {
		op = CB_PURPOSE_INT (l);
		switch (op) {
		case '9':
			/* NUMERIC */
			build_expr_shift_class ("cob_is_numeric");
			has_rel = 1;
			break;
		case 'A':
			/* ALPHABETIC */
			build_expr_shift_class ("cob_is_alpha");
			has_rel = 1;
			break;
		case 'L':
			/* ALPHABETIC_LOWER */
			build_expr_shift_class ("cob_is_lower");
			has_rel = 1;
			break;
		case 'U':
			/* ALPHABETIC_UPPER */
			build_expr_shift_class ("cob_is_upper");
			has_rel = 1;
			break;
		case 'P':
			/* POSITIVE */
			build_expr_shift_sign ('>');
			has_rel = 1;
			break;
		case 'N':
			/* NEGATIVE */
			build_expr_shift_sign ('<');
			has_rel = 1;
			break;
		case 'O':
			/* OMITTED */
			if (current_statement) {
				current_statement->null_check = NULL;
			}
			build_expr_shift_class ("cob_is_omitted");
			has_rel = 1;
			break;
		case 'C':
			/* CLASS */
			build_expr_shift_class (CB_CLASS_NAME (cb_ref (CB_VALUE (l)))->cname);
			has_rel = 1;
			break;
		default:
			if(TOKEN (-1) == '!'){
/* switch `NOT` relation, e.g. the two expression tokens `NOT` and `>`
 * are reduced to a single token `<=` */
				switch(op){
				case '=': TOKEN (-1) = '~'; continue;
				case '>': TOKEN (-1) = '['; continue;
				case '<': TOKEN (-1) = ']'; continue;
				case ']': TOKEN (-1) = '<'; continue;
				case '[': TOKEN (-1) = '>'; continue;
				}
			}
			v = CB_VALUE (l);
			if (op == 'x') {
                                if( has_var && v == cb_zero ){
                                        has_rel = 1;
                                }
				has_var = 1;
				if (CB_TREE_TAG (v) == CB_TAG_BINARY_OP) {
					has_rel = 1;
				} else
				if (CB_TREE_TAG (v) == CB_TAG_FUNCALL) {
					has_rel = 1;
				} else
                                if (CB_REF_OR_FIELD_P (v)) {
					f = CB_FIELD_PTR (v);
					if (f->level == 88) {
						has_rel = 1;
					} else
					if (f->storage == CB_STORAGE_CONSTANT) {
						has_rel = 1;
					}
				}
			 } else
			 if (op == '|'
			  || op == '&') {
				has_con = 1;
				if (has_var && !has_rel) {
					bad_cond = 1;
				}
			 } else
			 if (op == '>'
			  || op == '<'
			  || op == '='
			  || op == '~'
			  || op == '['
			  || op == ']') {
				has_rel = 1;
			 } else
			 if (op == '!') {
				has_rel = 1;
			 }
			/* Warning for complex expressions without explicit parentheses
			   (i.e., "a OR b AND c" or "a AND b OR c") */
			if (expr_index > 3
			 && (op == '|' || op == '&')) {
			 	/* hack to use exp_line instead of source_line */
				cb_error_node->source_line = cb_exp_line;
#if 0 /* old code */
				SET_SOURCE(e, cb_source_file, cb_exp_line);
#endif

				if (op == '|' && expr_stack[expr_index-2].token == '&') {
					cb_warning_x (cb_warn_parentheses, cb_error_node,
						_("suggest parentheses around %s within %s"), "AND", "OR");
				} else
				if (op == '&' && expr_stack[expr_index-2].token == '|') {
					cb_warning_x (cb_warn_parentheses, cb_error_node,
						_("suggest parentheses around %s within %s"), "OR", "AND");
				}
				cb_error_node->source_line = 0;	/* undo hack */
			}
			build_expr_shift (op, v);
			break;
		}
	}
	if (bad_cond) {
		cb_error_x (list, _("invalid conditional expression"));
		return cb_any;
	}

	return build_expr_finish ();
}

const char *
explain_operator (const enum cb_binary_op_op op)
{
	switch (op) {
	case '>':
		return "GREATER THAN";
	case '<':
		return "LESS THAN";
	case ']':
		return "GREATER OR EQUAL";
	case '[':
		return "LESS OR EQUAL";
	case '=':
		return "EQUALS";
	case '~':
		return "NOT EQUAL";
	case '!':
		return "NOT";
	case '&':
		return "AND";
	case '|':
		return "OR";
	case 'a':	return "B-AND";
	case 'n':	return "B-NOT";
	case 'o':	return "B-OR";
	case 'e':	return "B-XOR";
	case 'l':	return "B-SHIFT-L";
	case 'r':	return "B-SHIFT-R";
	case 'c':	return "B-SHIFT-LC";
	case 'd':	return "B-SHIFT-RC";
	default:
		return NULL;
	}
}

const char *
enum_explain_storage (const enum cb_storage storage)
{
	switch (storage) {
	case CB_STORAGE_CONSTANT:
		return "Constants";
	case CB_STORAGE_FILE:
		return "FILE SECTION";
	case CB_STORAGE_WORKING:
		return "WORKING-STORAGE SECTION";
	case CB_STORAGE_LOCAL:
		return "LOCAL-STORAGE SECTION";
	case CB_STORAGE_LINKAGE:
		return "LINKAGE SECTION";
	case CB_STORAGE_SCREEN:
		return "SCREEN SECTION";
	case CB_STORAGE_REPORT:
		return "REPORT SECTION";
	case CB_STORAGE_COMMUNICATION:
		return "COMMUNICATION SECTION";
	default:
		break;
	}
	return "UNKNOWN";
}

/* Numerical operation */

static cb_tree
build_store_option (cb_tree x, cb_tree round_opt)
{
	const struct cb_field	*f = CB_FIELD_PTR (x);
	const enum cb_usage	usage = f->usage;
	int		opt;

#if	0	/* RXWRXW - FP */
	if (usage == CB_USAGE_LONG_DOUBLE
	 || usage == CB_USAGE_DOUBLE
	 || usage == CB_USAGE_FLOAT) {
		/* Rounding on FP is useless */
		opt = 0;
	} else {
#endif
		opt = CB_INTEGER (round_opt)->val;
#if	0	/* RXWRXW - FP */
	}
#endif

	if (usage == CB_USAGE_COMP_5
	 || usage == CB_USAGE_COMP_X
	 || usage == CB_USAGE_COMP_N) {
		/* Do not check NOT ERROR case, so that we optimize */
		if (current_statement->ex_handler) {
			opt |= COB_STORE_KEEP_ON_OVERFLOW;
		}
	} else if (current_statement->handler_type != NO_HANDLER) {
		/* There is a [NOT] ERROR/OVERFLOW/EXCEPTION - Set in parser */
		opt |= COB_STORE_KEEP_ON_OVERFLOW;
	} else if (usage == CB_USAGE_BINARY && cb_binary_truncate) {
		/* Truncate binary field to digits in picture */
		opt |= COB_STORE_TRUNC_ON_OVERFLOW;
	}

	return cb_int (opt);
}

static cb_tree
decimal_alloc (void)
{
	cb_tree x;

	if (current_program->decimal_index < 0)
		current_program->decimal_index = 0;
	x = cb_build_decimal (current_program->decimal_index);
	current_program->decimal_index++;
	/* LCOV_EXCL_START */
	if (current_program->decimal_index >= COB_MAX_DEC_STRUCT) {
		cobc_err_msg (_("internal decimal structure size exceeded: %d"),
				COB_MAX_DEC_STRUCT);
		if (current_statement->statement != STMT_COMPUTE) {
			cobc_err_msg (_("Try to minimize the number of parentheses "
							 "or split into multiple computations."));
		}
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */
	if (current_program->decimal_index > current_program->decimal_index_max) {
		current_program->decimal_index_max = current_program->decimal_index;
	}
	return x;
}

static void
decimal_free (void)
{
	current_program->decimal_index--;
	if (current_program->decimal_index < 0)
		current_program->decimal_index = 0;
}
static void
push_expr_dec (int dec)
{
	if (expr_nest < MAX_NESTED_EXPR) {
		expr_decp[expr_nest++] = dec;
	} else {
		cb_warning (COBC_WARN_FILLER,
			_("more than %d nested expressions"), MAX_NESTED_EXPR);
	}
}

static void
decimal_align (void)
{
	cb_tree		expr_dec = NULL;	/* Int value for decimal_align */

	if (expr_dec_align >= 0
	 && expr_x != NULL) {
		switch(expr_dec_align) {
		case 0:
			expr_dec = cb_int0;
			break;
		case 1:
			expr_dec = cb_int1;
			break;
		case 2:
			expr_dec = cb_int2;
			break;
		case 3:
			expr_dec = cb_int3;
			break;
		case 4:
			expr_dec = cb_int4;
			break;
		case 5:
			expr_dec = cb_int5;
			break;
		case 6:
			expr_dec = cb_int6;
			break;
		default:
			expr_dec = cb_int (expr_dec_align);
			break;
		}
		dpush (CB_BUILD_FUNCALL_2 ("cob_decimal_align", expr_x, expr_dec));
		if (expr_line != cb_source_line) {
			expr_line = cb_source_line; /* only warn once per line */
			cb_warning_x (cb_warn_arithmetic_osvs, CB_TREE (current_statement),
				_("precision of result may change with arithmetic-osvs"));
		}
		expr_dec_align = -1;
		expr_x = NULL;
	}
}

static void
decimal_compute (const int op, cb_tree x, cb_tree y)
{
	const char	*func;
	int		decp, d;

	/* skip if the actual statement can't be generated any more
	   to prevent multiple errors here */
	if (error_statement == current_statement) {
		return;
	}

	if (!current_program->flag_decimal_comp) {
		struct cb_program* prog;
		for (prog = current_program; prog && !prog->flag_decimal_comp; prog = prog->next_program) {
			prog->flag_decimal_comp = 1;
		}
	}

	if (cb_arithmetic_osvs) {
		if (expr_dec_align >= 0
		 && expr_x != NULL
		 && expr_x != x) {
			decimal_align ();
		}
		decp = expr_dmax;
	} else {
		decp = -1;	/* fix missing initialization warning, not actually used */
	}
	switch (op) {
	case '+':
		func = "cob_decimal_add";
		break;
	case '-':
		func = "cob_decimal_sub";
		break;
	case '*':
		func = "cob_decimal_mul";
		break;
	case '/':
		func = "cob_decimal_div";
		break;
	case '^':
		func = "cob_decimal_pow";
		break;
	case 'n':
		func = "cob_logical_not";
		break;
	case 'a':
		func = "cob_logical_and";
		break;
	case 'o':
		func = "cob_logical_or";
		break;
	case 'e':
		func = "cob_logical_xor";
		break;
	case 'l':
		func = "cob_logical_left";
		break;
	case 'r':
		func = "cob_logical_right";
		break;
	case 'c':
		func = "cob_logical_left_c";
		break;
	case 'd':
		func = "cob_logical_right_c";
		break;
	default:
		func = explain_operator (op);
		/* LCOV_EXCL_START */
		if (!func) {
			cobc_err_msg (_("unexpected operation: %c (%d)"), (char)op, op);
			COBC_ABORT ();
		}
		/* LCOV_EXCL_STOP */
		error_statement = current_statement;
		cb_error_x (CB_TREE(current_statement), _("%s operator may be misplaced"), func);
		return;
	}
	if (cb_arithmetic_osvs
	 && expr_nest > 1) {
		expr_nest--;
		switch (op) {
		case '+':
			if (expr_decp [expr_nest] > expr_decp [expr_nest-1]) {
				expr_decp [expr_nest-1] = expr_decp [expr_nest];
			}
			break;
		case '-':
			if (expr_decp [expr_nest] > expr_decp [expr_nest-1]) {
				expr_decp [expr_nest-1] = expr_decp [expr_nest];
			}
			break;
		case '*':
			expr_decp [expr_nest-1] += expr_decp [expr_nest];
			break;
		case '/':
			d = expr_decp [expr_nest-1] - expr_decp [expr_nest];
			if (d > expr_dmax) {
				expr_decp [expr_nest-1] = d;
			} else {
				expr_decp [expr_nest-1] = expr_dmax;
			}
			break;
		case '^':
			if (expr_decp [expr_nest-1] - expr_decp [expr_nest]
				< expr_decp [expr_nest-1]) {
				expr_decp [expr_nest-1] = expr_decp [expr_nest-1] - expr_decp [expr_nest];
			}
			break;
		default:
			break;
		}
		decp = expr_decp [expr_nest-1];
	}

	if (op == 'c' || op == 'd') {
		dpush (CB_BUILD_FUNCALL_3 (func, x, y, sz_shift));
	} else {
		dpush (CB_BUILD_FUNCALL_2 (func, x, y));
	}

	/* Save for later decimal_align */
	if (cb_arithmetic_osvs) {
		if (decp > expr_dec_align)
			expr_dec_align = decp;
	} else {
		expr_dec_align = -1;
	}
	expr_x = x;
}

/**
 * expand tree x to the previously allocated decimal tree d.
 * Returns either d or cb_error_node in case of error.
 */
static cb_tree
decimal_expand (cb_tree d, cb_tree x)
{
	struct cb_literal	*l;
	struct cb_field		*f;
	struct cb_binary_op	*p;
	cb_tree			t;

	/* skip if the actual statement can't be generated any more
	   to prevent multiple errors here */
	if (error_statement == current_statement) {
		return d;
	}
	switch (CB_TREE_TAG (x)) {
	case CB_TAG_CONST:
		/* LCOV_EXCL_START */
		if (x != cb_zero) {
			cobc_err_msg (_("unexpected constant expansion"));
			COBC_ABORT ();
		}
		/* LCOV_EXCL_STOP */
		dpush (CB_BUILD_FUNCALL_2 ("cob_decimal_set_llint", d, cb_int0));
		break;
	case CB_TAG_LITERAL:
		/* Set d, N */
		decimal_align ();
		l = CB_LITERAL (x);
		if (l->size < 19 && l->scale == 0) {
			dpush (CB_BUILD_FUNCALL_2 ("cob_decimal_set_llint", d,
				cb_build_cast_llint (x)));
		} else {
			dpush (CB_BUILD_FUNCALL_2 ("cob_decimal_set_field", d, x));
			push_expr_dec (l->scale);	/* CHECKME: Why is this here in this branch? */
		}
		break;
	case CB_TAG_REFERENCE:
		/* Set d, X */
		f = CB_FIELD_PTR (x);
		/* Check numeric */
		if (cb_flag_correct_numeric && f->usage == CB_USAGE_DISPLAY) {
			cb_emit (CB_BUILD_FUNCALL_1 ("cob_correct_numeric", x));
		}
		if (CB_EXCEPTION_ENABLE (COB_EC_DATA_INCOMPATIBLE)) {
			if (f->usage == CB_USAGE_DISPLAY ||
			    f->usage == CB_USAGE_PACKED ||
			    f->usage == CB_USAGE_COMP_6) {
				dpush (CB_BUILD_FUNCALL_2 ("cob_check_numeric",
							   x, CB_BUILD_STRING0 (f->name)));
			}
		}
		decimal_align ();

		if (  (f->usage == CB_USAGE_BINARY
		    || f->usage == CB_USAGE_COMP_5
			|| f->usage == CB_USAGE_INDEX
			|| f->usage == CB_USAGE_HNDL
			|| f->usage == CB_USAGE_HNDL_WINDOW
			|| f->usage == CB_USAGE_HNDL_SUBWINDOW
			|| f->usage == CB_USAGE_HNDL_FONT
			|| f->usage == CB_USAGE_HNDL_THREAD
			|| f->usage == CB_USAGE_HNDL_MENU
			|| f->usage == CB_USAGE_HNDL_VARIANT
			|| f->usage == CB_USAGE_HNDL_LM
			|| f->usage == CB_USAGE_COMP_X
			|| f->usage == CB_USAGE_COMP_N)
		 && !f->pic->scale
		 && (f->size == 1 || f->size == 2 || f->size == 4 ||
		     f->size == 8)) {
			if (f->pic->have_sign) {
				dpush (CB_BUILD_FUNCALL_2 ("cob_decimal_set_llint",
					 		   d, cb_build_cast_llint (x)));
			} else {
				dpush (CB_BUILD_FUNCALL_2 ("cob_decimal_set_ullint",
							   d, cb_build_cast_llint (x)));
			}
		} else {
			dpush (CB_BUILD_FUNCALL_2 ("cob_decimal_set_field", d, x));
			push_expr_dec (f->pic->scale);
		}
		break;
	case CB_TAG_BINARY_OP:
		/* Set d, X
		 * Set t, Y
		 * OP d, t */
		p = CB_BINARY_OP (x);

		switch (p->op){
		case '=': case '~': case '<': case '>': case '[': case ']':
		case '!': case '&': case '|':
			cb_error_x (x, "condition expression found where decimal expression was expected");
			error_statement = current_statement;
			return cb_error_node;
		case 'c':
		case 'd':
			if (CB_REF_OR_FIELD_P (p->x)) {
				sz_shift = cb_int (CB_FIELD_PTR (p->x)->size);
				break;
			}
		default: sz_shift = cb_int1;
		}
		d = decimal_expand (d, p->x);

		if (CB_TREE_TAG (p->y) == CB_TAG_LITERAL
		 && CB_TREE_CATEGORY (p->y) == CB_CATEGORY_NUMERIC) {
			t = cb_build_decimal_literal (cb_lookup_literal(p->y,1));
			decimal_compute (p->op, d, t);
		} else {
			t = decimal_alloc ();
			t = decimal_expand (t, p->y);
			decimal_compute (p->op, d, t);
			decimal_free ();
		}
		break;
	case CB_TAG_INTRINSIC:
		decimal_align ();
		dpush (CB_BUILD_FUNCALL_2 ("cob_decimal_set_field", d, x));
		push_expr_dec (0);
		break;
	/* LCOV_EXCL_START */
	default:
		CB_TREE_TAG_UNEXPECTED_ABORT (x);
	/* LCOV_EXCL_STOP */
	}
	return d;
}

static void
decimal_assign (cb_tree x, cb_tree d, cb_tree round_opt)
{
	dpush (CB_BUILD_FUNCALL_3 ("cob_decimal_get_field", d, x,
				   build_store_option (x, round_opt)));
}

static cb_tree
cb_build_mul (cb_tree v, cb_tree n, cb_tree round_opt)
{
	cb_tree		opt;
	struct cb_field	*f;

	if (CB_INDEX_OR_HANDLE_P (v)) {
		return cb_build_move (cb_build_binary_op (v, '*', n), v);
	}

	if (CB_REF_OR_FIELD_P (n)) {
		f = CB_FIELD_PTR (n);
		f->count++;
	}
	if (CB_REF_OR_FIELD_P (v)) {
		f = CB_FIELD_PTR (v);
		f->count++;
		if (round_opt == cb_int0
		 && cb_fits_long_long (n)
		 && cb_is_integer_field(f)
		 && cb_is_integer_expr (n)) {
			return cb_build_assign (v, cb_build_binary_op (v, '*', n));
		}
	}
	opt = build_store_option (v, round_opt);
	return CB_BUILD_FUNCALL_3 ("cob_mul", v, n, opt);
}

static cb_tree
cb_build_div (cb_tree v, cb_tree n, cb_tree round_opt)
{
	cb_tree		opt;
	struct cb_field	*f;

	if (CB_INDEX_OR_HANDLE_P (v)) {
		return cb_build_move (cb_build_binary_op (v, '/', n), v);
	}

	if (CB_REF_OR_FIELD_P (n)) {
		f = CB_FIELD_PTR (n);
		f->count++;
	}
	opt = build_store_option (v, round_opt);
	if (CB_REF_OR_FIELD_P (v)) {
		f = CB_FIELD_PTR (v);
		f->count++;
	}
	return CB_BUILD_FUNCALL_3 ("cob_div", v, n, opt);
}

static cb_tree
build_decimal_assign (cb_tree vars, const int op, cb_tree val)
{
	struct cb_field	*f;
	cb_tree	l;
	cb_tree	t;
	cb_tree	s1;
	cb_tree	s2;
	cb_tree	d;

	/* note: vars validated by caller: cb_emit_arithmetic */
	if (cb_arithmetic_osvs) {
		/* ARITHMETIC-OSVS: Determine largest scale used in result field */
		expr_dec_align = -1;
		expr_rslt = CB_VALUE(vars);
		for (l = vars; l; l = CB_CHAIN (l)) {
			if (CB_FIELD_P (cb_ref (CB_VALUE(l)))) {
				f = CB_FIELD_PTR (CB_VALUE(l));
				if(f->pic->scale > expr_dmax) {
					expr_dmax = f->pic->scale;
				}
			}
		}
		cb_walk_cond (val);
	} else {
		expr_dmax = -1;
		expr_dec_align = -1;
	}
	expr_nest = 0;

	d = decimal_alloc ();

	/* Set d, VAL */
	d = decimal_expand (d, val);

	s1 = NULL;
	if (op == 0) {
		for (l = vars; l; l = CB_CHAIN (l)) {
			/* Set VAR, d */
			decimal_assign (CB_VALUE (l), d, CB_PURPOSE (l));
			s2 = cb_list_reverse (decimal_stack);
			if (!s1) {
				s1 = s2;
			} else {
				s1 = cb_list_append (s1, s2);
			}
			decimal_stack = NULL;
		}
	} else {
		t = decimal_alloc ();
		for (l = vars; l; l = CB_CHAIN (l)) {
			/* Set t(emporary) <- VAR
			 * OP t, d
			 * set VAR <- t, with appropriate rounding
			 */
			t = decimal_expand (t, CB_VALUE (l));
			decimal_compute (op, t, d);
			decimal_assign (CB_VALUE (l), t, CB_PURPOSE (l));
			s2 = cb_list_reverse (decimal_stack);
			if (!s1) {
				s1 = s2;
			} else {
				s1 = cb_list_append (s1, s2);
			}
			decimal_stack = NULL;
		}
		decimal_free ();
	}

	decimal_free ();
	expr_dmax = -1;
	expr_dec_align = -1;
	expr_nest = 0;

	return s1;
}

void
cb_set_dmax (int scale)
{
	if (cb_arithmetic_osvs
	 && scale > expr_dmax) {
		expr_dmax = scale;
	}
}

static int
cb_check_arithmetic (cb_tree vars, cb_tree x, const int only_numeric_allowed)
{
	if (cb_validate_one (x)
	 || cb_validate_list (vars)) {
		return 1;
	}

	if (only_numeric_allowed) {
		return cb_list_map (cb_check_numeric_name, vars);
	} else {
		return cb_list_map (cb_check_numeric_edited_name, vars);
	}
}

void
cb_emit_arithmetic (cb_tree vars, const int op, cb_tree val)
{
	cb_tree	x = cb_check_numeric_value (val);

	if (cb_check_arithmetic (vars, x, op != '\0')) {
		return;
	}

	if (!CB_BINARY_OP_P (x)
	 && (op == '+' || op == '-' || op == '*' || op == '/')) {
		cb_tree l;
		cb_emit_incompat_data_checks (x);
		for (l = vars; l; l = CB_CHAIN (l)) {
			const cb_tree	target = CB_VALUE(l);
			const cb_tree	round_and_trunc = CB_PURPOSE(l);
			cb_emit_incompat_data_checks (target);
			switch (op) {
			case '+':
				CB_VALUE (l) = cb_build_add (target, x, round_and_trunc);
				break;
			case '-':
				CB_VALUE (l) = cb_build_sub (target, x, round_and_trunc);
				break;
			case '*':
				CB_VALUE (l) = cb_build_mul (target, x, round_and_trunc);
				break;
			case '/':
				CB_VALUE (l) = cb_build_div (target, x, round_and_trunc);
				break;
			}
		}
		cb_emit_list (vars);
		cb_check_list (vars);
	} else {
		cb_check_list (vars);
		if (op == 0
		 && vars
		 && CB_CHAIN(vars) == NULL
		 && (CB_PURPOSE (vars) == NULL || CB_PURPOSE (vars) == cb_int0)
		 && cb_is_integer_expr (val)
		 && CB_VALUE (vars)
	 	 && cb_is_integer_expr (CB_VALUE(vars))) {
			cb_emit (cb_build_assign (CB_VALUE (vars), val));
			return;
		}
		cb_emit_list (build_decimal_assign (vars, op, x));
	}
}

/* Condition */

static cb_tree
build_cond_88 (cb_tree x)
{
	struct cb_field	*f;

	cb_tree		l;
	cb_tree		t;
	cb_tree		c1;
	cb_tree		c2;

	f = CB_FIELD_PTR (x);
	/* Refer to parents data storage */
	if (!f->parent) {
		/* Field is invalid */
		return cb_error_node;
	}
	x = cb_build_field_reference (f->parent, x);
	f->parent->count++;
	c1 = NULL;

	/* Build condition */
	for (l = f->values; l; l = CB_CHAIN (l)) {
		t = CB_VALUE (l);
		if (CB_PAIR_P (t)) {
			/* VALUE THRU VALUE */
			const enum cob_statement real_stmt = current_statement->statement;
			/* bad hack... */
			current_statement->statement = STMT_VALUE_THRU;
			c2 = cb_build_binary_op (cb_build_binary_op (x, ']', CB_PAIR_X (t)),
						 '&', cb_build_binary_op (x, '[', CB_PAIR_Y (t)));
			current_statement->statement = real_stmt;
		} else {
			/* VALUE */
			c2 = cb_build_binary_op (x, '=', t);
		}
		if (c1 == NULL) {
			c1 = c2;
		} else {
			c1 = cb_build_binary_op (c1, '|', c2);
		}
	}

	return c1;
}

static cb_tree
cb_build_optim_cond (struct cb_binary_op *p)
{
	const char	*s;
	size_t		n;
	const cb_tree left = p->x;
	const cb_tree right = p->y;
	struct cb_field	*f = CB_REF_OR_FIELD_P (left)
	               	   ? CB_FIELD_PTR (left) : NULL;

#if	0	/* RXWRXW - US */
	if (CB_REF_OR_FIELD_P (right)) {
		const struct cb_field	*fy = CB_FIELD_PTR (right);
		if (!fy->pic->have_sign
		 && (fy->usage == CB_USAGE_BINARY
		  || fy->usage == CB_USAGE_COMP_5
		  || fy->usage == CB_USAGE_COMP_X
		  || fy->usage == CB_USAGE_COMP_N)) {
			return CB_BUILD_FUNCALL_2 ("cob_cmp_uint", left,
						   cb_build_cast_int (right));
		}
	}
#endif

	if (f == NULL) {
		if (!cb_fits_long_long (right)) {
			return NULL;
		}
		return CB_BUILD_FUNCALL_2 ("cob_cmp_llint", left,
					    cb_build_cast_llint (right));
	}

#if 0	/* TODO: if the right side is a literal: then build an ideal
		   memcmp as if it was a field of same attributes as left-side,
		   with the value of the literal */
	if (CB_LITERAL_P (right) || right == cb_zero) {
		if (f->usage == CB_USAGE_PACKED
		 || f->usage == CB_USAGE_COMP_6) {
			return CB_BUILD_FUNCALL_3 ("memcmp",
				CB_BUILD_CAST_ADDRESS (left),
				cb_build_direct (get_hex_encoded_packed_literal (right), 0),
				cb_int (f->size));
		}
	}
#endif
	if (f->usage == CB_USAGE_PACKED
	 || f->usage == CB_USAGE_COMP_6) {
		if (CB_REF_OR_FIELD_P (right)) {
			const struct cb_field	*fy = CB_FIELD_PTR (right);
			if (fy->usage == CB_USAGE_PACKED
			 || fy->usage == CB_USAGE_COMP_6) {
				if (f->pic->scale
				 || f->pic->digits >= 19
				 || fy->pic->scale
				 || fy->pic->digits >= 19
					) {
					if (f->pic->scale >= 0 && fy->pic->scale >= 0) {
						/* for now skip negative scale, until this is added and tested */
						return CB_BUILD_FUNCALL_2 ("cob_bcd_cmp", left, right);
					}
				}
			}
		}
	}

	if (!cb_fits_long_long (right)) {
		return NULL;
	}

#if	0	/* RXWRXW - SI */
	if (f->index_type) {
		return CB_BUILD_FUNCALL_2 ("c",
			cb_build_cast_int (left),
			cb_build_cast_int (right));
	}
#endif
	if (f->pic->scale || f->flag_any_numeric) {
		return CB_BUILD_FUNCALL_2 ("cob_cmp_llint", left,
					    cb_build_cast_llint (right));
	}
#if 0	/* libcob's optimized version "cob_cmp_packed" is not slower,
     	   so drop these specific local optimization functions */
	if (f->usage == CB_USAGE_PACKED) {
		if (f->pic->digits < 19) {
			optimize_defs[COB_CMP_PACKED_INT] = 1;
			return CB_BUILD_FUNCALL_2 ("cob_cmp_packed_int",
				left,
				cb_build_cast_llint (right));
		} else {
			return CB_BUILD_FUNCALL_2 ("cob_cmp_packed",
				left,
				cb_build_cast_llint (right));
		}
	}
	if (f->usage == CB_USAGE_COMP_6) {
		return CB_BUILD_FUNCALL_2 ("cob_cmp_packed",
			left,
			cb_build_cast_llint (right));
	}
#else
	if (f->usage == CB_USAGE_PACKED
	 || f->usage == CB_USAGE_COMP_6) {
		return CB_BUILD_FUNCALL_2 ("cob_cmp_packed",
			left,
			cb_build_cast_llint (right));
	}
#endif
	if (f->usage == CB_USAGE_DISPLAY
	 && !f->flag_sign_leading
	 && !f->flag_sign_separate) {
		if (cb_fits_long_long (left)) {
			return CB_BUILD_FUNCALL_4 ("cob_cmp_numdisp",
				CB_BUILD_CAST_ADDRESS (left),
				cb_int (f->size),
				cb_build_cast_llint (right),
				cb_int (f->pic->have_sign ? 1 : 0));
		}
		return CB_BUILD_FUNCALL_2 ("cob_cmp_llint", left,
					    cb_build_cast_llint (right));
	}
	if (f->usage == CB_USAGE_BINARY
	 || f->usage == CB_USAGE_COMP_5
	 || f->usage == CB_USAGE_INDEX
	 ||	f->usage == CB_USAGE_HNDL
	 ||	f->usage == CB_USAGE_HNDL_WINDOW
	 ||	f->usage == CB_USAGE_HNDL_SUBWINDOW
	 ||	f->usage == CB_USAGE_HNDL_FONT
	 ||	f->usage == CB_USAGE_HNDL_THREAD
	 ||	f->usage == CB_USAGE_HNDL_MENU
	 ||	f->usage == CB_USAGE_HNDL_VARIANT
	 ||	f->usage == CB_USAGE_HNDL_LM
	 || f->usage == CB_USAGE_COMP_X
	 || f->usage == CB_USAGE_COMP_N) {
		n = ((size_t)f->size - 1)
		  + ((f->pic->have_sign   ? 1 : 0) * 8)
		  + ((f->flag_binary_swap ? 1 : 0) * 16);
#if	defined(COB_NON_ALIGNED) && !defined(_MSC_VER) && defined(COB_ALLOW_UNALIGNED)
		switch (f->size) {
		case 2:
#ifdef	COB_SHORT_BORK
			optimize_defs[bin_compare_funcs[n].optim_val] = 1;
			s = bin_compare_funcs[n].optim_name;
			break;
#endif
		case 4:
		case 8:
			if (f->storage != CB_STORAGE_LINKAGE
			 && f->indexes == 0
			 && (f->offset % f->size) == 0) {
				optimize_defs[align_bin_compare_funcs[n].optim_val] = 1;
				s = align_bin_compare_funcs[n].optim_name;
			} else {
				optimize_defs[bin_compare_funcs[n].optim_val] = 1;
				s = bin_compare_funcs[n].optim_name;
			}
			break;
		default:
			optimize_defs[bin_compare_funcs[n].optim_val] = 1;
			s = bin_compare_funcs[n].optim_name;
			break;
		}
#else
		optimize_defs[bin_compare_funcs[n].optim_val] = 1;
		s = bin_compare_funcs[n].optim_name;
#endif
		if (s) {
			return CB_BUILD_FUNCALL_2 (s,
				CB_BUILD_CAST_ADDRESS (left),
				cb_build_cast_llint (right));
		}
	}
	return CB_BUILD_FUNCALL_2 ("cob_cmp_llint", left,
				   cb_build_cast_llint (right));
}

static int
cb_check_num_cond (cb_tree x, cb_tree y)
{
	struct cb_field		*fx;
	struct cb_field		*fy;

	if (!CB_REF_OR_FIELD_P (x)
	 || !CB_REF_OR_FIELD_P (y)) {
		return 0;
	}
	if (CB_TREE_CATEGORY (x) != CB_CATEGORY_NUMERIC
	 || CB_TREE_CATEGORY (y) != CB_CATEGORY_NUMERIC
	 || CB_TREE_CLASS (x) != CB_CLASS_NUMERIC
	 || CB_TREE_CLASS (y) != CB_CLASS_NUMERIC) {
		return 0;
	}
	fx = CB_FIELD_PTR (x);
	fy = CB_FIELD_PTR (y);
	if (fx->usage != fy->usage) {
		return 0;
	}
	if (fx->usage == CB_USAGE_DISPLAY) {
		if (fx->pic->have_sign
		 || fy->pic->have_sign) {
			return 0;
		}
#if 0	/* possibly add this with an optimizing flag
		   which isn't active by default; previously we
		   did what MF also does: only consider a
		   sign nibble of 0x0d as negative, and the rest
		   as positive -> 0x195f == 0x195c (and even 0x1950)
		*/
	} else if (fx->usage == CB_USAGE_PACKED) {
		if (fx->pic->have_sign != fy->pic->have_sign) {
			return 0;
		}
		/* needs following attribute check to decide */;
#endif
	/* no sign nibble so directly comparable;
	   note: previous versions of GnuCOBOL did handle
	   invalid data with padding nibble different,
	   PIC 9 COMP-6 with 0x11 == 0x01, which isn't done now
	   any more (note: this is identical to at least MicroFocus) */
	} else if (fx->usage == CB_USAGE_COMP_6) {
		/* needs following attribute check to decide */;
	} else {
		return 0;
	}
	if (fx->pic->digits != fy->pic->digits /* digits instead of size to cater for packed */
	 || fx->pic->scale  != fy->pic->scale) {
		return 0;
	}
	return 1;
}

static int
cb_check_alpha_cond (cb_tree x)
{
	if (CB_LITERAL_P (x)
	 || CB_CONST_P (x)) {
		return 1;
	}
	if (CB_REF_OR_FIELD_P (x)) {
		const enum cb_category cat = CB_TREE_CATEGORY (x);
		if (cat != CB_CATEGORY_ALPHANUMERIC
		 && cat != CB_CATEGORY_ALPHABETIC) {
			/* CHECKME: Shouldn't _EDITED fields lead to
			            alphanumeric comparision, too ?
			*/
			return 0;
		}
	} else {
		return 0;
	}
	if (cb_field_variable_size (CB_FIELD_PTR (x))) {
		return 0;
	}
	return 1;
}

static void
cb_walk_cond (cb_tree x)
{
	struct cb_binary_op	*p;
	struct cb_field		*f;
	struct cb_literal	*l;

	if (x == NULL) {
		return;
	}

	switch (CB_TREE_TAG (x)) {
	case CB_TAG_LITERAL:
		if (CB_TREE_CATEGORY (x) == CB_CATEGORY_NUMERIC) {
			l = CB_LITERAL (x);
			if (l->scale > expr_dmax) {
				expr_dmax = l->scale;
			}
		}
		break;

	case CB_TAG_REFERENCE:
		if (!CB_FIELD_P (cb_ref (x))) {
			return;
		}

		f = CB_FIELD_PTR (x);

		if (f->level == 88) {
			return ;
		}
		if(f->pic
		&& f->pic->scale > expr_dmax) {
			expr_dmax = f->pic->scale;
		}

		break;

	case CB_TAG_BINARY_OP:
		p = CB_BINARY_OP (x);
		cb_walk_cond (p->x);
		if (p->op != '/') {
			cb_walk_cond (p->y);
		}
		break;

	default:
		return;
	}
}

/* Field comparison */
static cb_tree
cb_build_cond_fields (struct cb_binary_op *p,
	cb_tree left, cb_tree right, const enum cb_class l_class)
{
	const enum cb_category	x_cat = CB_TREE_CATEGORY (left);
	const int	size1 = cb_field_size (left);
	const int	size2 = cb_field_size (right);

	if ((CB_REF_OR_FIELD_P (left))
	 && (x_cat == CB_CATEGORY_ALPHANUMERIC
	  || x_cat == CB_CATEGORY_ALPHABETIC)
	 && size1 == 1
	 && (right == cb_space || right == cb_zero
	  || right == cb_high  || right == cb_low)) {
		return CB_BUILD_FUNCALL_2 ("$G", left, right);
	}
	if (size1 == 1 && size2 == 1) {
		return CB_BUILD_FUNCALL_2 ("$G", left, right);
	}
	if (size1 > 0 && size1 == size2) {
		return CB_BUILD_FUNCALL_3 ("memcmp",
			CB_BUILD_CAST_ADDRESS (left),
			CB_BUILD_CAST_ADDRESS (right),
			cb_int (size1));
	}
	if (right == cb_zero && l_class == CB_CLASS_NUMERIC) {
		return cb_build_optim_cond (p);
	}
	if (right == cb_space
	 && (l_class == CB_CLASS_ALPHANUMERIC || l_class == CB_CLASS_ALPHABETIC)
	 && (size1 > 0 && size1 <= COB_SPACES_ALPHABETIC_BYTE_LENGTH)) {
		return CB_BUILD_FUNCALL_3 ("memcmp",
			CB_BUILD_CAST_ADDRESS (left),
			cb_build_direct ("COB_SPACES_ALPHABETIC", 0),
			cb_int (size1));
	}
	if (right == cb_zero
	 && (l_class == CB_CLASS_ALPHANUMERIC || l_class == CB_CLASS_ALPHABETIC)
	 && (size1 > 0 && size1 <= COB_ZEROES_ALPHABETIC_BYTE_LENGTH)) {
		return CB_BUILD_FUNCALL_3 ("memcmp",
			CB_BUILD_CAST_ADDRESS (left),
			cb_build_direct ("COB_ZEROES_ALPHABETIC", 0),
			cb_int (size1));
	}
	
#if 0	/* TODO: if at least one is a literal and smaller:
		   possibly extend by building a new literal correctly
		   left/right padded with system SPACE allowing direct memcmp;
		   not useful for PIC X(12000) and a 2 byte literal,
		   but likely useful for PIC X(10) or X(32) or ??? */
#define COB_SPACES_ALPHABETIC_EXPAND_LENGTH 32
	if (CB_LITERAL_P (right)
	 && (l_class == CB_CLASS_ALPHANUMERIC || l_class == CB_CLASS_ALPHABETIC)
	 && size1 > 0 && size1 <= COB_SPACES_ALPHABETIC_EXPAND_LENGTH
	 && size2 <= COB_SPACES_ALPHABETIC_EXPAND_LENGTH) {
		cb_tree new_lit, lit;
		char data [COB_SPACES_ALPHABETIC_EXPAND_LENGTH + 1];
		memcpy (data, CB_LITERAL (right)->data, size2);
		if (size2 < COB_SPACES_ALPHABETIC_EXPAND_LENGTH) {
			memset (data, ' ', size1 - size2);
		}
		new_lit = cb_build_alphanumeric_literal (data, size1);
		lit = cb_lookup_literal (new_lit, 0);
		return CB_BUILD_FUNCALL_3 ("memcmp",
			CB_BUILD_CAST_ADDRESS (left),
			CB_BUILD_CAST_ADDRESS (lit),
			cb_int (size1));
	}
#endif
	return CB_BUILD_FUNCALL_2 ("cob_cmp", left, right);
}

static cb_tree
cb_build_cond_default (struct cb_binary_op *p, cb_tree left, cb_tree right)
{
	const enum cb_class l_class = CB_TREE_CLASS (left);
	const enum cb_class r_class = CB_TREE_CLASS (right);

	int has_any_len = 0;

	if (CB_TREE_TAG (left) == CB_TAG_DECIMAL) {
		cb_tree	d2;
		cb_tree	ret;
		if (CB_TREE_TAG (right) == CB_TAG_LITERAL
		 && CB_TREE_CATEGORY (right) == CB_CATEGORY_NUMERIC) {
			d2 = cb_build_decimal_literal (cb_lookup_literal(right,1));
			dpush (CB_BUILD_FUNCALL_2 ("cob_decimal_cmp", left, d2));
		} else {
			d2 = decimal_alloc ();
			decimal_expand (d2, right);
			dpush (CB_BUILD_FUNCALL_2 ("cob_decimal_cmp", left, d2));
			decimal_free ();
		}
		ret = cb_list_reverse (decimal_stack);
		decimal_stack = NULL;
		return ret;
	}

	if (CB_REF_OR_FIELD_P (left)) {
		struct cb_field	*f = CB_FIELD_PTR (left);
		if (f->flag_any_length) {
			has_any_len = 1;
		}
	}

	if (CB_BINARY_OP_P (left)
	 || CB_BINARY_OP_P (right)) {
		/* Decimal comparison */
		cb_tree	ret;
		cb_tree	d1;
		cb_tree	d2;
		if (cb_is_integer_expr (CB_TREE (p))) {
			ret = cb_build_optim_cond (p);
			if (ret) {
				return ret;
			}
		}
		d1 = decimal_alloc ();
		d2 = decimal_alloc ();
		d1 = decimal_expand (d1, left);
		d2 = decimal_expand (d2, right);
		dpush (CB_BUILD_FUNCALL_2 ("cob_decimal_cmp", d1, d2));
		decimal_free ();
		decimal_free ();
		ret = cb_list_reverse (decimal_stack);
		decimal_stack = NULL;
		return ret;
	}

#if 0	/* possibly add check of classes of the two operands, note that there
		   are a lot of defined comparisions in the standard 8.8.4.1.1 relation
		   conditions, with explicit comparision of class alphanumeric (where
		   all edited items go to) and of class numeric; so likely only do this
		   with a new warning only enabled with -Wextra. */
	if (get_warn_opt_value (cb_warn_strict_typing) != COBC_WARN_DISABLED) {
		if (cb_tree_class_are_something (left, right)) {
			cb_warning_x (cb_warn_strict_typing,
				CB_TREE (p), _("alphanumeric value is expected"));
		} else {
			cb_warning_x (cb_warn_strict_typing,
				CB_TREE(p), _("numeric value is expected"));
		}
	}
#endif

	if (CB_INDEX_OR_HANDLE_P (left)
	 || CB_INDEX_OR_HANDLE_P (right)
	 || l_class == CB_CLASS_POINTER
	 || r_class == CB_CLASS_POINTER) {
		return cb_build_binary_op (left, '-', right);
	}

	/* DEBUG Bypass optimization for PERFORM and upon request */
	if (current_program->flag_debugging
	 || !cb_flag_fast_compare) {
		return CB_BUILD_FUNCALL_2 ("cob_cmp", left, right);
	}

	if (cb_check_num_cond (left, right)) {
		const int size1 = cb_field_size (left);
		return CB_BUILD_FUNCALL_3 ("memcmp",
			CB_BUILD_CAST_ADDRESS (left),
			CB_BUILD_CAST_ADDRESS (right),
			cb_int (size1));
	}

	if (p->op == '='
	 || p->op == '~'
	 || p->op == '>'
	 || p->op == '<'
	 || p->op == ']'
	 || p->op == '[') {
		int_usage = CB_USAGE_PACKED;
		if (l_class == CB_CLASS_NUMERIC
		 && r_class == CB_CLASS_NUMERIC
		 && CB_TREE_TAG (left) == CB_TAG_LITERAL
		 && CB_TREE_TAG (right) == CB_TAG_REFERENCE) {	/* literal relop field */
			cb_tree	d1;
			d1 = p->x;
			p->x = p->y;	/* Swap operands */
			p->y = d1;
			left = p->x;
			right = p->y;
			if (p->op == '>')
				p->op = '<';
			else if (p->op == '<')
				p->op = '>';
			else if (p->op == '[')
				p->op = ']';
			else if (p->op == ']')
				p->op = '[';
		}
	} else {
		int_usage = -1;
	}

	if (l_class == CB_CLASS_NUMERIC
	 && r_class == CB_CLASS_NUMERIC) {
		cb_tree ret;
		if (CB_REF_OR_FIELD_P (left)) {
			struct cb_field	*f = CB_FIELD_PTR (left);
			if (cb_is_integer_field_and_int (f, right)
			 && cb_fits_int (right)) {
				/* 'native' (short/int/long) on SYNC boundary */
				int_usage = -1;
				ret = CB_BUILD_FUNCALL_3 ("$:", left, (cb_tree)(long)p->op, right);
				cb_copy_source_reference (ret, CB_TREE (p));
				return ret;
			}
		}
		int_usage = -1;
		ret = cb_build_optim_cond (p);
		if (ret) {
			return ret;
		}
	}
	int_usage = -1;

	/* TODO: try building cob_fields and calling cob_cmp directly from
	   here. */
	if (current_program->alphabet_name_list
	 || has_any_len
	 || (CB_TREE_CLASS (left) == CB_CLASS_NATIONAL
	     ? current_program->collating_sequence_n != NULL
	     : current_program->collating_sequence != NULL)
	 || (CB_TREE_CLASS (right) == CB_CLASS_NATIONAL
	     ? current_program->collating_sequence_n != NULL
	     : current_program->collating_sequence != NULL)
	 || !cb_check_alpha_cond (left)
	 || !cb_check_alpha_cond (right)) {
		return CB_BUILD_FUNCALL_2 ("cob_cmp", left, right);
	}
	return cb_build_cond_fields (p, left, right, l_class);
}

static void
swap_condition_operands (struct cb_binary_op *p)
{
	cb_tree y = p->x;

	p->flag = p->flag == 0 ? BOP_OPERANDS_SWAPPED : 0;

	p->x = p->y;
	p->y = y;

	if (p->op == '>') p->op = '<';
	else if (p->op == '<') p->op = '>';
	else if (p->op == '[') p->op = ']';
	else if (p->op == ']') p->op = '[';
}

cb_tree
cb_build_cond (cb_tree x)
{
	cb_tree			ret;

	if (x == cb_error_node) {
		return cb_error_node;
	}

	if (cb_arithmetic_osvs) {
		/* ARITHMETIC-OSVS: Determine largest scale used in condition */
		if (expr_dmax == -1) {
			/* FIXME: this is a hack, x should always be a list! */
			if (CB_LIST_P (x)) {
				expr_rslt = CB_VALUE (x);
			} else {
				expr_rslt = x;
			}
			cb_walk_cond (x);
		}
	} else {
		expr_dmax = -1;
		expr_dec_align = -1;
		expr_nest = 0;
	}

	switch (CB_TREE_TAG (x)) {
	case CB_TAG_CONST:
		if (x != cb_any && x != cb_true && x != cb_false) {
			/* TODO: Add test case for this to syn_misc.at invalid expression */
			cb_error_x (CB_TREE(current_statement),
				    _("invalid expression: condition expected"));
			return cb_error_node;
		}
		return x;
	case CB_TAG_FUNCALL:
		return x;
	case CB_TAG_REFERENCE:
	{
		cb_tree r = cb_ref (x);
		if (!CB_FIELD_P (r)) {
			ret = cb_build_cond (r);
			cb_copy_source_reference (ret, x);
			return ret;
		}

		/* Level 88 condition */
		if (CB_FIELD_PTR (x)->level == 88) {
			/* Build an 88 condition at every occurrence */
			/* as it may be subscripted */
			ret = cb_build_cond (build_cond_88 (x));
			cb_copy_source_reference (ret, x);
			return ret;
		}

		break;
	}
	case CB_TAG_BINARY_OP:
	{
		struct cb_binary_op	*p = CB_BINARY_OP (x);
		if (!p->x || p->x == cb_error_node) {
			return cb_error_node;
		}
		switch (p->op) {
		case '!':
			ret = CB_BUILD_NEGATION (cb_build_cond (p->x));
			break;
		case '&':
		case '|':
			if (!p->y || p->y == cb_error_node) {
				return cb_error_node;
			}
			ret = cb_build_binary_op (cb_build_cond (p->x), p->op, cb_build_cond (p->y));
			break;
		default:
			if (!p->y || p->y == cb_error_node) {
				return cb_error_node;
			}
			/* move figurative constants and literals to the right for comparision */
			if (cb_flag_fast_compare
			 &&  (CB_CONST_P (p->x) || CB_LITERAL_P (p->x))
			 && !(CB_CONST_P (p->y) || CB_LITERAL_P (p->y))) {
				swap_condition_operands (p);
			}
			ret = cb_build_cond_default (p, p->x, p->y);
			if (CB_FUNCALL_P(ret) && !strcmp(CB_FUNCALL(ret)->name, "$:")) {
				break;
			}
			cb_next_binary_op_flag = p->flag;
			ret = cb_build_binary_op (ret, p->op, p->y);
			if (CB_VALID_TREE (ret)) {
				CB_BINARY_OP (ret)->flag = p->flag;
			}
		}
		if (ret != cb_true && ret != cb_false) {
			cb_copy_source_reference (ret, x);
		}
		return ret;
	}
	default:
		break;
	}
	cb_error_x (x, _("incomplete expression"));
	return cb_error_node;
}

/* End parsing a 'condition' */
void
cb_end_cond (cb_tree rslt)
{
	expr_dmax = -1;		/* Reset 'Max scale' */
	expr_dec_align = -1;
	expr_nest = 0;
	expr_line = -1;

	if (cb_flag_remove_unreachable == 0) {
		/* Do not remove the code */
		cond_fixed = -1;
		return;
	}

	if (rslt == cb_true) {
		cond_fixed = 0;
	} else
	if (rslt == cb_false) {
		cond_fixed = 1;
	} else {
		cond_fixed = -1;
	}
}

/* Save this 'condition' result */
void
cb_save_cond (void)
{
	if (if_stop) {
		return;
	}
	if (if_nest < MAX_NESTED_COND) {
		if_cond[if_nest++] = cond_fixed;
	} else {
		/* result: errors won't be ignored in "false" condition parts */
		cb_warning (COBC_WARN_FILLER,
			_("more than %d nested conditions"), MAX_NESTED_COND);
		if_stop = 1;
		if_nest = 0;
		cb_set_ignore_error (0);
	}
}

/* TRUE side of 'condition' */
void
cb_true_side (void)
{
	if (cond_fixed == 1) {
		cb_set_ignore_error (1);
	} else {
		cb_set_ignore_error (0);
	}
}

/* FALSE side of 'condition' */
void
cb_false_side (void)
{
	if (cond_fixed == 0) {
		cb_set_ignore_error (1);
	} else {
		cb_set_ignore_error (0);
	}
}

/* END of statement that had a 'condition' */
void
cb_terminate_cond (void)
{
	cb_check_reset ();
	if (if_stop)
		return;
	if_nest--;
	if (if_nest <= 0) {
		cond_fixed = -1;
		cb_set_ignore_error (0);
		if_nest = 0;
	} else {
		cond_fixed = if_cond[if_nest];
	}
}

/* Now at PERIOD, ending statement(s) */
void
cb_end_statement (void)
{
	expr_dmax = -1;
	expr_dec_align = -1;
	expr_nest = 0;
	if_stop = 0;
	if_nest = 0;
	cb_set_ignore_error (0);
	expr_line = -1;
}

/* ADD/SUBTRACT CORRESPONDING */

static cb_tree
cb_build_optim_add (cb_tree v, cb_tree n)
{
	if (CB_REF_OR_FIELD_P (v)) {
		const struct cb_field	*f = CB_FIELD_PTR (v);
#if 0 /* CHECKME: this breaks FLD0370B in data_display.at ADD/SUB w/o SIZE ERROR */
		if (cb_is_integer_field(f)
		 && cb_is_integer_expr (n)
		 && cb_binary_truncate) {
			return cb_build_assign (v, cb_build_binary_op (v, '+', n));
		}
#endif
		if (!f->pic
		 ||  f->pic->scale ) {
			return CB_BUILD_FUNCALL_3 ("cob_add_int", v,
						   cb_build_cast_int (n),
						   cb_int0);
		}
		if (f->usage == CB_USAGE_BINARY
		 || f->usage == CB_USAGE_COMP_5
		 || f->usage == CB_USAGE_COMP_X
		 || f->usage == CB_USAGE_COMP_N) {
			const char	*s;
			const size_t	z
			  = ((size_t)f->size - 1)
			  + (8 * (f->pic->have_sign ? 1 : 0))
			  + (16 * (f->flag_binary_swap ? 1 : 0));
#if	defined(COB_NON_ALIGNED) && !defined(_MSC_VER) && defined(COB_ALLOW_UNALIGNED)
			switch (f->size) {
			case 2:
#ifdef	COB_SHORT_BORK
				optimize_defs[bin_add_funcs[z].optim_val] = 1;
				s = bin_add_funcs[z].optim_name;
				break;
#endif
			case 4:
			case 8:
				if (f->storage != CB_STORAGE_LINKAGE
				 && f->indexes == 0
				 && (f->offset % f->size) == 0) {
					optimize_defs[align_bin_add_funcs[z].optim_val] = 1;
					s = align_bin_add_funcs[z].optim_name;
				} else {
					optimize_defs[bin_add_funcs[z].optim_val] = 1;
					s = bin_add_funcs[z].optim_name;
				}
				break;
			default:
				optimize_defs[bin_add_funcs[z].optim_val] = 1;
				s = bin_add_funcs[z].optim_name;
				break;
			}
#else
#ifdef COB_ALLOW_UNALIGNED
			if (f->usage == CB_USAGE_COMP_5) {
				switch (f->size) {
				case 1:
				case 2:
				case 4:
				case 8:
					return cb_build_assign (v, cb_build_binary_op (v, '+', n));
				default:
					break;
				}
			}
#endif
			optimize_defs[bin_add_funcs[z].optim_val] = 1;
			s = bin_add_funcs[z].optim_name;
#endif
			if (s) {
				return CB_BUILD_FUNCALL_2 (s,
					CB_BUILD_CAST_ADDRESS (v),
					cb_build_cast_int (n));
			}
		} else if (f->usage == CB_USAGE_PACKED) {
		    if (f->pic->digits < 10) {
				optimize_defs[COB_ADD_PACKED_INT] = 1;
				return CB_BUILD_FUNCALL_2 ("cob_add_packed_int",
					v, cb_build_cast_int (n));
			} else if (f->pic->digits < 19) {
				optimize_defs[COB_ADD_PACKED_INT64] = 1;
				return CB_BUILD_FUNCALL_2 ("cob_add_packed_int64",
					v, cb_build_cast_llint (n));
			}
		}
		/* we may want negative/positive zero,
		   which prevents a direct integer calculation */
		if ((f->usage == CB_USAGE_PACKED || f->usage == CB_USAGE_DISPLAY)
		 && (f->pic && f->pic->have_sign)) {
			return CB_BUILD_FUNCALL_3 ("cob_add_int", v,
						   cb_build_cast_int (n), cb_int0);
		}
		if (CB_NUMERIC_LITERAL_P (n)
		 && (f->usage == CB_USAGE_PACKED || f->usage == CB_USAGE_DISPLAY)
		 && (f->pic && f->pic->scale == 0)
		 && CB_LITERAL (n)->scale == 0
		 && CB_LITERAL (n)->size == 1) {
			return CB_BUILD_FUNCALL_3 ("cob_add_int", v,
						   cb_build_cast_int (n), cb_int0);
		}
		if (cb_is_integer_field(f)
		 && cb_is_integer_expr (n)) {
			return cb_build_assign (v, cb_build_binary_op (v, '+', n));
		}
	}
	return CB_BUILD_FUNCALL_3 ("cob_add_int", v,
				   cb_build_cast_int (n), cb_int0);
}

static cb_tree
cb_build_optim_sub (cb_tree v, cb_tree n)
{
	if (CB_REF_OR_FIELD_P (v)) {
		const struct cb_field	*f = CB_FIELD_PTR (v);
#if 0 /* CHECKME: this breaks FLD0370B in data_display.at ADD/SUB w/o SIZE ERROR */
		if (cb_is_integer_field(f)
		 && cb_is_integer_expr (n)
		 && cb_binary_truncate) {
			return cb_build_assign (v, cb_build_binary_op (v, '-', n));
		}
#endif
		if (!f->pic
		 ||  f->pic->scale) {
			return CB_BUILD_FUNCALL_3 ("cob_sub_int", v,
						   cb_build_cast_int (n), cb_int0);
		}
		if (f->usage == CB_USAGE_BINARY
		 || f->usage == CB_USAGE_COMP_5
		 || f->usage == CB_USAGE_COMP_X
		 || f->usage == CB_USAGE_COMP_N) {
			const char	*s;
			const size_t z
			  = ((size_t)f->size - 1)
			  + (8 * (f->pic->have_sign ? 1 : 0))
			  +	(16 * (f->flag_binary_swap ? 1 : 0));
#if	defined(COB_NON_ALIGNED) && !defined(_MSC_VER) && defined(COB_ALLOW_UNALIGNED)
			switch (f->size) {
			case 2:
#ifdef	COB_SHORT_BORK
				optimize_defs[bin_sub_funcs[z].optim_val] = 1;
				s = bin_sub_funcs[z].optim_name;
				break;
#endif
			case 4:
			case 8:
				if (f->storage != CB_STORAGE_LINKAGE &&
				    f->indexes == 0 && (f->offset % f->size) == 0) {
					optimize_defs[align_bin_sub_funcs[z].optim_val] = 1;
					s = align_bin_sub_funcs[z].optim_name;
				} else {
					optimize_defs[bin_sub_funcs[z].optim_val] = 1;
					s = bin_sub_funcs[z].optim_name;
				}
				break;
			default:
				optimize_defs[bin_sub_funcs[z].optim_val] = 1;
				s = bin_sub_funcs[z].optim_name;
				break;
			}
#else
#ifdef COB_ALLOW_UNALIGNED
			if (f->usage == CB_USAGE_COMP_5) {
				switch (f->size) {
				case 1:
				case 2:
				case 4:
				case 8:
					return cb_build_assign (v, cb_build_binary_op (v, '-', n));
				default:
					break;
				}
			}
#endif
			optimize_defs[bin_sub_funcs[z].optim_val] = 1;
			s = bin_sub_funcs[z].optim_name;
#endif
			if (s) {
				return CB_BUILD_FUNCALL_2 (s,
					CB_BUILD_CAST_ADDRESS (v),
					cb_build_cast_int (n));
			}
		} else if (f->usage == CB_USAGE_PACKED) {
		    if (f->pic->digits < 10) {
				cb_tree n_negative = cb_build_cast_int (n);
				CB_CAST (n_negative)->cast_type = CB_CAST_NEGATIVE_INTEGER;
				optimize_defs[COB_ADD_PACKED_INT] = 1;
				return CB_BUILD_FUNCALL_2 ("cob_add_packed_int",
					v, n_negative);
			} else if (f->pic->digits < 19) {
				cb_tree n_negative = cb_build_cast_llint (n);
				CB_CAST (n_negative)->cast_type = CB_CAST_NEGATIVE_LONG_INT;
				optimize_defs[COB_ADD_PACKED_INT64] = 1;
				return CB_BUILD_FUNCALL_2 ("cob_add_packed_int64",
					v, n_negative);
			}
		}
		/* we may want negative/positive zero,
		   which prevents a direct integer calculation */
		if ((f->usage == CB_USAGE_PACKED || f->usage == CB_USAGE_DISPLAY)
		 && (f->pic && f->pic->have_sign)) {
			return CB_BUILD_FUNCALL_3 ("cob_sub_int", v,
						   cb_build_cast_int (n), cb_int0);
		}
		if (CB_NUMERIC_LITERAL_P (n)
		 && (f->usage == CB_USAGE_PACKED || f->usage == CB_USAGE_DISPLAY)
		 && (f->pic && f->pic->scale == 0)
		 && CB_LITERAL (n)->scale == 0
		 && CB_LITERAL (n)->size == 1) {
			return CB_BUILD_FUNCALL_3 ("cob_sub_int", v,
						   cb_build_cast_int (n), cb_int0);
		}
		if (cb_is_integer_field(f)
		 && cb_is_integer_expr (n)) {
			return cb_build_assign (v, cb_build_binary_op (v, '-', n));
		}
	}
	return CB_BUILD_FUNCALL_3 ("cob_sub_int", v,
				   cb_build_cast_int (n), cb_int0);
}

cb_tree
cb_build_add (cb_tree v, cb_tree n, cb_tree round_opt)
{
	cb_tree		opt;
	struct cb_field	*f;

#ifdef	COB_NON_ALIGNED
	if (CB_INDEX_OR_HANDLE_P (v)) {
		return cb_build_move (cb_build_binary_op (v, '+', n), v);
	}
	if (CB_TREE_CLASS (v) == CB_CLASS_POINTER) {
		optimize_defs[COB_POINTER_MANIP] = 1;
		return CB_BUILD_FUNCALL_3 ("cob_pointer_manip", cb_build_address (v), n, cb_int0);
	}
#else
	if (CB_INDEX_OR_HANDLE_P (v) || CB_TREE_CLASS (v) == CB_CLASS_POINTER) {
		return cb_build_move (cb_build_binary_op (v, '+', n), v);
	}
#endif

	if (CB_REF_OR_FIELD_P (v)) {
		f = CB_FIELD_PTR (v);
		f->count++;
	}
	if (CB_REF_OR_FIELD_P (n)) {
		f = CB_FIELD_PTR (n);
		f->count++;
	}
	if (round_opt == cb_high) {
		/* Short circuit from tree.c for perform */
		if (cb_fits_int (n)) {
			return cb_build_optim_add (v, n);
		} else {
			return CB_BUILD_FUNCALL_3 ("cob_add", v, n, cb_int0);
		}
	}
	opt = build_store_option (v, round_opt);
	if (opt == cb_int0
	 && cb_fits_int (n)) {
		return cb_build_optim_add (v, n);
	}
	return CB_BUILD_FUNCALL_3 ("cob_add", v, n, opt);
}

cb_tree
cb_build_sub (cb_tree v, cb_tree n, cb_tree round_opt)
{
	cb_tree		opt;
	struct cb_field	*f;

#ifdef	COB_NON_ALIGNED
	if (CB_INDEX_OR_HANDLE_P (v)) {
		return cb_build_move (cb_build_binary_op (v, '-', n), v);
	}
	if (CB_TREE_CLASS (v) == CB_CLASS_POINTER) {
		optimize_defs[COB_POINTER_MANIP] = 1;
		return CB_BUILD_FUNCALL_3 ("cob_pointer_manip", cb_build_address (v), n, cb_int1);
	}
#else
	if (CB_INDEX_OR_HANDLE_P (v) || CB_TREE_CLASS (v) == CB_CLASS_POINTER) {
		return cb_build_move (cb_build_binary_op (v, '-', n), v);
	}
#endif

	if (CB_REF_OR_FIELD_P (v)) {
		f = CB_FIELD_PTR (v);
		f->count++;
	}
	if (CB_REF_OR_FIELD_P (n)) {
		f = CB_FIELD_PTR (n);
		f->count++;
	}
	opt = build_store_option (v, round_opt);
	if (opt == cb_int0
	 && cb_fits_int (n)) {
		return cb_build_optim_sub (v, n);
	}
	return CB_BUILD_FUNCALL_3 ("cob_sub", v, n, opt);
}

static unsigned int
emit_corresponding (cb_tree (*func) (cb_tree f1, cb_tree f2, cb_tree f3),
		    cb_tree x1, cb_tree x2, cb_tree opt)
{
	struct cb_field *f1, *f2;
	cb_tree		t1;
	cb_tree		t2;
	unsigned int	found;

	found = 0;
	for (f1 = CB_FIELD_PTR (x1)->children; f1; f1 = f1->sister) {
		if (!f1->redefines && !f1->flag_occurs) {
			for (f2 = CB_FIELD_PTR (x2)->children; f2; f2 = f2->sister) {
				if (!f2->redefines && !f2->flag_occurs) {
					if (strcmp (f1->name, f2->name) == 0) {
						t1 = cb_build_field_reference (f1, x1);
						t2 = cb_build_field_reference (f2, x2);
						if (f1->children && f2->children) {
							found += emit_corresponding (func, t1, t2, opt);
						} else {
							if ((CB_TREE_CATEGORY (t1) == CB_CATEGORY_NUMERIC) &&
							    (CB_TREE_CATEGORY (t2) == CB_CATEGORY_NUMERIC)) {
								found++;
								cb_emit (func (t1, t2, opt));
							}
						}
					}
				}
			}
		}
	}
	return found;
}

void
cb_emit_corresponding (cb_tree (*func) (cb_tree f1, cb_tree f2, cb_tree f3),
		       cb_tree x1, cb_tree x2, cb_tree opt)
{
	x1 = cb_check_group_name (x1);
	x2 = cb_check_group_name (x2);

	if (cb_validate_one (x1)) {
		return;
	}
	if (cb_validate_one (x2)) {
		return;
	}

	if (!emit_corresponding (func, x1, x2, opt)) {
		cb_warning_x (cb_warn_corresponding, x2, _("no CORRESPONDING items found"));
	}
}

void
cb_emit_tab_arithmetic (cb_tree (*func) (cb_tree f1, cb_tree f2, cb_tree f3),
	cb_tree x1, cb_tree x2, cb_tree opt, cb_tree from_to_idx, cb_tree dest_idx)
{
	if (cb_validate_one (x1)) {
		return;
	}
	if (cb_tree_category (x1) != CB_CATEGORY_NUMERIC) {
		cb_error_x (x1, _("'%s' is not numeric"), cb_name (x1));
	}

	if (cb_validate_one (x2)) {
		return;
	}
	if (cb_tree_category (x2) != CB_CATEGORY_NUMERIC) {
		cb_error_x (x2, _("'%s' is not numeric"), cb_name (x2));
	}

	/* TODO pending, no actual code generation */
	COB_UNUSED (func);
	COB_UNUSED (opt);
	COB_UNUSED (from_to_idx);
	COB_UNUSED (dest_idx);
}

static unsigned int
emit_move_corresponding (cb_tree x1, cb_tree x2)
{
	struct cb_field *f1, *f2;
	unsigned int	found;

	found = 0;
	for (f1 = CB_FIELD_PTR (x1)->children; f1; f1 = f1->sister) {
		if (f1->redefines || f1->flag_occurs) continue;
		for (f2 = CB_FIELD_PTR (x2)->children; f2; f2 = f2->sister) {
			if (f2->redefines || f2->flag_occurs) continue;
			if (strcmp (f1->name, f2->name) == 0) {
				const cb_tree t1 = cb_build_field_reference (f1, x1);
				const cb_tree t2 = cb_build_field_reference (f2, x2);
				/* GCOS 7: Contrary to the documentation,
				   handling of PIC L fields in MOVE
				   CORRESPONDING ignores the DEPENDING var for
				   both sending and receiving fields. */
				if (f1->flag_picture_l) {
					CB_REFERENCE (t1)->length = cb_int (f1->size);
				}
				if (f2->flag_picture_l) {
					CB_REFERENCE (t2)->length = cb_int (f2->size);
				}
				if (f1->children && !f1->flag_picture_l
				 && f2->children && !f2->flag_picture_l) {
					found += emit_move_corresponding (t1, t2);
				} else {
					cb_emit (cb_build_move (t1, t2));
					found++;
				}
			}
		}
	}
	return found;
}

void
cb_emit_move_corresponding (cb_tree source, cb_tree target_list)
{
	cb_tree		l;

	source = cb_check_group_name (source);
	if (cb_validate_one (source)) {
		return;
	}
	for (l = target_list; l; l = CB_CHAIN(l)) {
		const cb_tree target = cb_check_group_name (CB_VALUE(l));
		if (cb_validate_one (target)) {
			return;
		}
		if (!emit_move_corresponding (source, target)) {
			cb_warning_x (cb_warn_corresponding, target, _("no CORRESPONDING items found"));
		} else if (cb_listing_xref) {
			cobc_xref_set_receiving (target);
		}
	}
}

static unsigned int
emit_accept_external_form (cb_tree x)
{
	struct cb_field *f;
	cb_tree		f_ref, f_ref_2, ext_form_id, index_lit;
	int		i;
	char		buff[32];
	unsigned int	found = 0;

	for (f = CB_FIELD_PTR (x)->children; f; f = f->sister) {
		if (f->redefines) {
			continue;
		}

		if (f->children) {
			f_ref = cb_build_field_reference (f, x);
			found += emit_accept_external_form (f_ref);
			continue;
		}

		if (f->external_form_identifier) {
			ext_form_id = f->external_form_identifier;
		} else {
			ext_form_id = cb_build_alphanumeric_literal (f->name, strlen (f->name));
		}
		if (f->flag_occurs) {
			for (i = 1; i <= f->occurs_max; i++) {
				sprintf (buff, "%d", i);
				index_lit = cb_build_numeric_literal (0, buff, 0);

				f_ref_2 = cb_build_field_reference (f, x);
				CB_REFERENCE (f_ref_2)->subs = CB_LIST_INIT (index_lit);

#if 0 /* TODO: implement CGI runtime, see Patch #27 */
				cb_emit (CB_BUILD_FUNCALL_3 ("cob_cgi_getCgiValue",
							     ext_form_id, index_lit,
							     f_ref_2));
#endif
			}
#if 0 /* TODO: implement CGI runtime, see Patch #27 */
		} else {
			index_lit = cb_build_numeric_literal (0, "1", 0);
			cb_emit (CB_BUILD_FUNCALL_3 ("cob_cgi_getCgiValue",
						     ext_form_id, index_lit,
						     f_ref));
#else
			COB_UNUSED (ext_form_id);
#endif
		}
		found++;
	}

	return found;
}

static void
cb_emit_accept_external_form (cb_tree x1)
{
	cb_tree		x2;

	x2 = cb_check_group_name (x1);
	if (cb_validate_one (x2)) {
		return;
	}
	if (!emit_accept_external_form (x2)) {
		cb_warning_x (COBC_WARN_FILLER, x1, _("no items to ACCEPT found"));
	}
}

static unsigned int
emit_display_external_form (cb_tree x)
{
	struct cb_field *f, *f_ref_field;
	cb_tree		f_ref, ext_form_id;
	unsigned int	found = 0;

	for (f = CB_FIELD_PTR (x)->children; f; f = f->sister) {
		if (f->redefines || f->flag_occurs) {
			continue;
		}

		f_ref = cb_build_field_reference (f, x);
			if (f->children) {
			found += emit_display_external_form (f_ref);
			} else {
			/* TODO: Is CB_FIELD (cb_ref (f_ref)) == f? */
			f_ref_field = CB_FIELD (cb_ref (f_ref));
			if (f_ref_field->external_form_identifier) {
				ext_form_id = f_ref_field->external_form_identifier;
				} else {
				ext_form_id = cb_build_alphanumeric_literal (f_ref_field->name,
								   strlen (f_ref_field->name));
				}
#if 0 /* TODO: implement CGI runtime, see Patch #27 */
			cb_emit (CB_BUILD_FUNCALL_2 ("cob_cgi_addTplVar", ext_form_id, f_ref));
#else
			COB_UNUSED (ext_form_id);
#endif
				found++;
			}
		}

	return found;
}

static void
cb_emit_display_external_form (cb_tree x1)
{
	cb_tree		x2;

	x2 = cb_check_group_name (x1);
	if (cb_validate_one (x2)) {
		return;
	}
	if (!emit_display_external_form (x2)) {
		cb_warning_x (COBC_WARN_FILLER, x1, _("no items to DISPLAY found"));
	}
}

static int
get_screen_type (const struct cb_field * const p)
{
	if (p->children) {
		return COB_SCREEN_TYPE_GROUP;
	} else if (p->values) {
		return COB_SCREEN_TYPE_VALUE;
	} else if (p->size > 0) {
		return COB_SCREEN_TYPE_FIELD;
	} else {
		return COB_SCREEN_TYPE_ATTRIBUTE;
	}
}

static void
output_screen_from (struct cb_field *p, const unsigned int sisters)
{
	int type;

	if (sisters && p->sister) {
		output_screen_from (p->sister, 1U);
	}
	if (p->children) {
		output_screen_from (p->children, 1U);
	}

	type = get_screen_type (p);
	if (type == COB_SCREEN_TYPE_FIELD && p->screen_from) {
		/* Bump reference count */
		p->count++;
		cb_emit (CB_BUILD_FUNCALL_2 ("cob_move", p->screen_from, CB_TREE (p)));
	}
}

static void
output_screen_to (struct cb_field *p, const unsigned int sisters)
{
	int type;

	if (sisters && p->sister) {
		output_screen_to (p->sister, 1U);
	}
	if (p->children) {
		output_screen_to (p->children, 1U);
	}

	type = get_screen_type (p);
	if (type == COB_SCREEN_TYPE_FIELD && p->screen_to) {
		/* Bump reference count */
		p->count++;
		cb_emit (CB_BUILD_FUNCALL_2 ("cob_move", CB_TREE (p), p->screen_to));
	}
}

/* ACCEPT statement */

static int
numeric_screen_pos_type (struct cb_field *pos)
{
	return pos->pic
		&& pos->pic->category == CB_CATEGORY_NUMERIC
		&& pos->pic->scale == 0;
}

static int
numeric_children_screen_pos_type (struct cb_field* child)
{
	child = child->children;
	if (!child) return 0;

	for (; child; child = child->sister) {
		if (child->redefines) continue;
		if (!numeric_screen_pos_type (child)) {
			return 0;
		}
	}

	return 1;
}

static int
valid_screen_pos (cb_tree pos)
{
	cb_tree pos_ref = pos;
	int	size = -1;

	/* Find size of pos value, if possible */
	if (CB_INVALID_TREE (pos)) {
		return 0;
	}
	if (CB_REFERENCE_P (pos)) {
		pos = cb_ref (pos);
	}
	if (CB_LITERAL_P (pos)) {
		if (CB_TREE_CATEGORY (pos) == CB_CATEGORY_NUMERIC) {
			size = CB_LITERAL (pos)->size;
		} else {
			size = -1;
		}
	} else if (CB_FIELD_P (pos)) {
		struct cb_field *field = CB_FIELD (pos);
		if (numeric_screen_pos_type (field)) {
			size = field->pic->size;
		} else if (numeric_children_screen_pos_type (field)) {
			size = field->size;
		}
	} else if (pos == cb_zero) {
		cb_error_x (pos_ref, _("cannot specify figurative constant ZERO in AT clause"));
		return 0;
	}
	if (size == -1) {
		cb_error_x (pos_ref, _("value in AT clause is not numeric"));
		return 0;
	}

	/* Check if size is valid. If it isn't, display error. */
	if (size != 4 && size != 6) {
		cb_error_x (pos_ref, _("value in AT clause must have 4 or 6 digits"));
		return 0;
	} else {
		return 1;
	}
}

static void
get_line_and_column_from_pos (const cb_tree pos, cb_tree * const line_or_pos,
	cb_tree * const column)
{
	if (!pos) {
		*line_or_pos = NULL;
		*column = NULL;
	} else if (CB_PAIR_P (pos)) {
		*line_or_pos = CB_PAIR_X (pos);
		*column = CB_PAIR_Y (pos);
		/* Note: This must not be done for column where we need the 0,
		         otherwise screenio.c (extract_line_and_col_vals) would
				 evaluate the field "line" as a combined position */
		if (*line_or_pos == cb_int0) {
			*line_or_pos = NULL;
		}
	} else if (valid_screen_pos (pos)) {
		*line_or_pos = pos;
		*column = NULL;
	}
}

static COB_INLINE COB_A_INLINE int
line_col_zero_is_supported (void)
{
	return cb_accept_display_extensions == CB_OK
		|| cb_accept_display_extensions == CB_WARNING
		|| cb_accept_display_extensions == CB_ARCHAIC
		|| cb_accept_display_extensions == CB_OBSOLETE;
}

static void
emit_field_accept_display (const enum cob_statement stmt,
	cb_tree x, cob_flags_t disp_attrs,
	cb_tree pos, cb_tree fgc, cb_tree bgc, cb_tree scroll,
	cb_tree timeout, cb_tree prompt, cb_tree size_is,
	cb_tree control, cb_tree color, cb_tree cursor)
{
	cb_tree	params[CB_BUILD_FUNCALL_MAX - 3] = { 0 };
	char	param_ids[CB_BUILD_FUNCALL_MAX - 3] = { 0 };
	unsigned char parnum = 0;
	cb_tree	parameter_ids = NULL;

	if (!pos) {
		/* no position to pass */
	} else if (CB_LIST_P (pos)) {
		/* AT LINE/COL */
		cb_tree		line = NULL;
		cb_tree		column = NULL;
		get_line_and_column_from_pos (pos, &line, &column);
		if (line) {
			param_ids[parnum] = 'l';
			params[parnum++] = line;
		}
		if (column) {
			param_ids[parnum] = 'c';
			params[parnum++] = column;
		}
	} else if (valid_screen_pos (pos)) {
		/* AT POS */
		param_ids[parnum] = 'p';
		params[parnum++] = pos;
	}
	if (fgc) {
		param_ids[parnum] = 'f';
		params[parnum++] = fgc;
	}
	if (bgc) {
		param_ids[parnum] = 'b';
		params[parnum++] = bgc;
	}
	if (scroll) {
		param_ids[parnum] = 's';
		params[parnum++] = scroll;
	}
	if (timeout) {
		param_ids[parnum] = 't';
		params[parnum++] = timeout;
	}
	if (prompt) {
		param_ids[parnum] = 'P';
		params[parnum++] = prompt;
	}
	if (size_is) {
		param_ids[parnum] = 'S';
		params[parnum++] = size_is;
	}
	if (control) {
		param_ids[parnum] = 'C';
		params[parnum++] = control;
	}
	if (color) {
		param_ids[parnum] = 'L';
		params[parnum++] = color;
	}
	if (cursor) {
		param_ids[parnum] = 'R';
		params[parnum++] = cursor;
	}
	if (parnum) {
		parameter_ids = cb_build_string (cobc_parse_strdup (param_ids), parnum);
	}
	cb_emit (cb_build_funcall (
		stmt == STMT_ACCEPT ? "cob_accept_field" : "cob_display_field",
		parnum + 3, x, cb_flags_t (disp_attrs), parameter_ids, 
		params[0], params[1], params[2], params[3], params[4], params[5],
		params[6], params[7], params[8], params[9], params[10]));
}

void
cb_emit_accept (cb_tree var, cb_tree pos, struct cb_attr_struct *attr_ptr)
{
	cb_tree		line = NULL;
	cb_tree		column = NULL;
	cb_tree		fgc = NULL;
	cb_tree		bgc = NULL;
	cb_tree		scroll = NULL;
	cb_tree		timeout = NULL;
	cb_tree		prompt = NULL;
	cb_tree		size_is = NULL;		/* WITH SIZE IS */
	cb_tree		control = NULL;		/* variable named attributes */
	cb_tree		color = NULL;		/* variable numeric added attributes */
	cb_tree		cursor = NULL;		/* CURSOR (position within the field) */
	cob_flags_t		disp_attrs = 0;

	if (current_program->flag_screen) {
#ifndef WITH_EXTENDED_SCREENIO
	if (!warn_screen_done) {
		warn_screen_done = 1;
		cb_warning (cb_warn_unsupported,
			_("runtime is not configured to support %s"), "SCREEN SECTION");
	}
#endif
	}
	if (cb_validate_one (var)) {
		return;
	}
	if (cb_listing_xref) {
		cobc_xref_set_receiving (var);
	}

	if (attr_ptr) {
		fgc = attr_ptr->fgc;
		bgc = attr_ptr->bgc;
		scroll = attr_ptr->scroll;
		timeout = attr_ptr->timeout;
		prompt = attr_ptr->prompt;
		size_is = attr_ptr->size_is;
		control = attr_ptr->control;
		cursor = attr_ptr->cursor;
		color = attr_ptr->color;
		disp_attrs = attr_ptr->dispattrs;
		if (cb_validate_one (pos)
		 || cb_validate_one (fgc)
		 || cb_validate_one (bgc)
		 || cb_validate_one (scroll)
		 || cb_validate_one (timeout)
		 || cb_validate_one (prompt)
		 || cb_validate_one (size_is)
		 || cb_validate_one (control)
		 || cb_validate_one (cursor)
		 || cb_validate_one (color)) {
			return;
		}
	}

	if (prompt) {
		/* PROMPT character - 1 character identifier or literal */
		if (CB_LITERAL_P (prompt)) {
			if (CB_LITERAL (prompt)->size != 1) {
				cb_error_x (prompt, _("invalid PROMPT literal"));
				return;
			}
		} else {
			if (CB_FIELD_PTR (prompt)->size != 1) {
				cb_error_x (prompt, _("invalid PROMPT identifier"));
				return;
			}
		}
	}

	/* CGI: ACCEPT external-form */
	/* TODO: CHECKME, see Patch #27 */
	if (CB_REF_OR_FIELD_P (var) && CB_FIELD (cb_ref (var))->flag_is_external_form) {
		cb_emit_accept_external_form (var);
		return;
	}

#if	0	/* RXWRXW - Screen */
	if (CB_REF_OR_FIELD_P (var)
	 && CB_FIELD_PTR (var)->storage == CB_STORAGE_SCREEN) {
		current_program->flag_screen = 1;
	}
#endif

	if (current_program->flag_screen) {
		/* Bump ref count to force CRT STATUS field generation
		   and include it in cross-reference */
		if (current_program->crt_status) {
			CB_FIELD_PTR (current_program->crt_status)->count++;
			if (cb_listing_xref) {
				cobc_xref_set_receiving (current_program->crt_status);
			}
		}
		if (CB_REF_OR_FIELD_P (var)
		 && CB_FIELD_PTR (var)->storage == CB_STORAGE_SCREEN) {
			output_screen_from (CB_FIELD_PTR (var), 0);
			gen_screen_ptr = 1;
			if (pos) {
				if (CB_LIST_P (pos)) {
					line = CB_PAIR_X (pos);
					column = CB_PAIR_Y (pos);
					cb_emit (CB_BUILD_FUNCALL_5 ("cob_screen_accept",
								     var, line, column, timeout,
								     cb_int (line_col_zero_is_supported ())));
				} else if (valid_screen_pos (pos)) {
					cb_emit (CB_BUILD_FUNCALL_5 ("cob_screen_accept",
								     var, pos, NULL, timeout,
								     cb_int (line_col_zero_is_supported ())));
				}
			} else {
				cb_emit (CB_BUILD_FUNCALL_5 ("cob_screen_accept",
							     var, NULL, NULL, timeout,
							     cb_int (line_col_zero_is_supported ())));
			}
			gen_screen_ptr = 0;
			output_screen_to (CB_FIELD (cb_ref (var)), 0);
			return;
		}
	}

	if (var == cb_null) {
		var = NULL;
	}
	if (disp_attrs || pos || fgc || bgc || scroll
	 || timeout || prompt || size_is
	 || control || color || cursor) {
		emit_field_accept_display (STMT_ACCEPT, var, disp_attrs,
				pos, fgc, bgc, scroll, timeout, prompt,
				size_is, control, color, cursor);

		/* Bump ref count to force CRT STATUS field generation
		   and include it in cross-reference */
		if (current_program->crt_status) {
			CB_FIELD_PTR (current_program->crt_status)->count++;
			if (cb_listing_xref) {
				cobc_xref_set_receiving (current_program->crt_status);
			}
		}
	} else {
		cb_emit (CB_BUILD_FUNCALL_1 ("cob_accept", var));
	}
}

void
cb_emit_accept_line_or_col (cb_tree var, const int l_or_c)
{
	if (cb_validate_one (var)) {
		return;
	}
	if (cb_listing_xref) {
		cobc_xref_set_receiving (var);
	}
	cb_emit (CB_BUILD_FUNCALL_2 ("cob_screen_line_col", var, cb_int (l_or_c)));
}

void
cb_emit_accept_escape_key (cb_tree var)
{
	if (cb_validate_one (var)) {
		return;
	}
	if (cb_listing_xref) {
		cobc_xref_set_receiving (var);
	}
	cb_emit (CB_BUILD_FUNCALL_1 ("cob_accept_escape_key", var));
}

void
cb_emit_accept_exception_status (cb_tree var)
{
	if (cb_validate_one (var)) {
		return;
	}
	if (cb_listing_xref) {
		cobc_xref_set_receiving (var);
	}
	cb_emit (CB_BUILD_FUNCALL_1 ("cob_accept_exception_status", var));
}

void
cb_emit_accept_user_name (cb_tree var)
{
	if (cb_validate_one (var)) {
		return;
	}
	if (cb_listing_xref) {
		cobc_xref_set_receiving (var);
	}
	cb_emit (CB_BUILD_FUNCALL_1 ("cob_accept_user_name", var));
}

void
cb_emit_accept_date (cb_tree var)
{
	if (cb_validate_one (var)) {
		return;
	}
	if (cb_listing_xref) {
		cobc_xref_set_receiving (var);
	}
	cb_emit (CB_BUILD_FUNCALL_1 ("cob_accept_date", var));
}

void
cb_emit_accept_date_yyyymmdd (cb_tree var)
{
	if (cb_validate_one (var)) {
		return;
	}
	if (cb_listing_xref) {
		cobc_xref_set_receiving (var);
	}
	cb_emit (CB_BUILD_FUNCALL_1 ("cob_accept_date_yyyymmdd", var));
}

void
cb_emit_accept_day (cb_tree var)
{
	if (cb_validate_one (var)) {
		return;
	}
	if (cb_listing_xref) {
		cobc_xref_set_receiving (var);
	}
	cb_emit (CB_BUILD_FUNCALL_1 ("cob_accept_day", var));
}

void
cb_emit_accept_day_yyyyddd (cb_tree var)
{
	if (cb_validate_one (var)) {
		return;
	}
	if (cb_listing_xref) {
		cobc_xref_set_receiving (var);
	}
	cb_emit (CB_BUILD_FUNCALL_1 ("cob_accept_day_yyyyddd", var));
}

void
cb_emit_accept_day_of_week (cb_tree var)
{
	if (cb_validate_one (var)) {
		return;
	}
	if (cb_listing_xref) {
		cobc_xref_set_receiving (var);
	}
	cb_emit (CB_BUILD_FUNCALL_1 ("cob_accept_day_of_week", var));
}

void
cb_emit_accept_time (cb_tree var, int with_microseconds)
{
	if (cb_validate_one (var)) {
		return;
	}
	if (cb_listing_xref) {
		cobc_xref_set_receiving (var);
	}

	if (!with_microseconds && cb_std_define == CB_STD_ACU) {
		/* for ACU: automatically use high-precision with big enough fields */
		const struct cb_field	*f = CB_FIELD_PTR (var);
		if (f->size >= 12) {	/* FIXME: should also work with binary -> digits/scale */
			with_microseconds = 1;
		}
	}
	if (with_microseconds) {
		cb_emit (CB_BUILD_FUNCALL_1 ("cob_accept_microsecond_time", var));
	} else {
		cb_emit (CB_BUILD_FUNCALL_1 ("cob_accept_time", var));
	}
}

void
cb_emit_accept_command_line (cb_tree var)
{
	if (cb_validate_one (var)) {
		return;
	}
	if (cb_listing_xref) {
		cobc_xref_set_receiving (var);
	}
	cb_emit (CB_BUILD_FUNCALL_1 ("cob_accept_command_line", var));
}

void
cb_emit_get_environment (cb_tree envvar, cb_tree envval)
{
	if (cb_validate_one (envvar)) {
		return;
	}
	if (cb_listing_xref) {
		cobc_xref_set_receiving (envvar);
	}
	if (cb_validate_one (envval)) {
		return;
	}
	cb_emit (CB_BUILD_FUNCALL_2 ("cob_get_environment", envvar, envval));
}

void
cb_emit_accept_environment (cb_tree var)
{
	if (cb_validate_one (var)) {
		return;
	}
	if (cb_listing_xref) {
		cobc_xref_set_receiving (var);
	}
	cb_emit (CB_BUILD_FUNCALL_1 ("cob_accept_environment", var));
}

void
cb_emit_accept_arg_number (cb_tree var)
{
	if (cb_validate_one (var)) {
		return;
	}
	if (cb_listing_xref) {
		cobc_xref_set_receiving (var);
	}
	cb_emit (CB_BUILD_FUNCALL_1 ("cob_accept_arg_number", var));
}

void
cb_emit_accept_arg_value (cb_tree var)
{
	if (cb_validate_one (var)) {
		return;
	}
	if (cb_listing_xref) {
		cobc_xref_set_receiving (var);
	}
	cb_emit (CB_BUILD_FUNCALL_1 ("cob_accept_arg_value", var));
}

void
cb_emit_accept_mnemonic (cb_tree var, cb_tree mnemonic)
{
	cb_tree		mnemonic_ref;

	if (cb_validate_one (var)) {
		return;
	}
	mnemonic_ref = cb_ref (mnemonic);
	if (mnemonic_ref == cb_error_node) {
		return;
	}
	switch (CB_SYSTEM_NAME (mnemonic_ref)->token) {
	case CB_DEVICE_CONSOLE:
	case CB_DEVICE_SYSIN:
		cb_emit (CB_BUILD_FUNCALL_1 ("cob_accept", var));
		break;
	default:
		cb_error_x (mnemonic, _("'%s' is not an input device"),
			    cb_name (mnemonic));
		break;
	}
}

void
cb_emit_accept_name (cb_tree var, cb_tree name)
{
	cb_tree		sys;

	if (cb_validate_one (var)) {
		return;
	}
	if (cb_listing_xref) {
		cobc_xref_set_receiving (var);
	}

	/* Allow direct reference to a device name (not defined as mnemonic name) */
	sys = get_system_name (CB_NAME (name));
	if (sys) {
		switch (CB_SYSTEM_NAME (sys)->token) {
		case CB_DEVICE_CONSOLE:
		case CB_DEVICE_SYSIN:
			if (!cb_device_mnemonics
			 && !cb_relaxed_syntax_checks) {
				cb_warning_x (COBC_WARN_FILLER, name,
					_("'%s' is not defined in SPECIAL-NAMES"), CB_NAME (name));
			}
			cb_emit (CB_BUILD_FUNCALL_1 ("cob_accept", var));
			return;
		default:
			cb_error_x (name, _("invalid input device '%s'"),
				    cb_name (name));
			return;
		}
	} else if (is_default_reserved_word (CB_NAME (name))) {
		cb_error_x (name, _("unknown device '%s'; it may exist in another dialect"),
				    CB_NAME (name));
	} else {
		cb_error_x (name, _("unknown device '%s'; not defined in SPECIAL-NAMES"),
		    CB_NAME (name));
	}
}

/* ALLOCATE statement */

static int
check_allocate_returning (cb_tree returning)
{
	if (!returning) {
		return 0;
	}
	if (cb_validate_one (returning)) {
		return 1;
	}
	if (! ( CB_REFERENCE_P (returning)
		 && CB_TREE_CLASS (returning) == CB_CLASS_POINTER)) {
		cb_error_x (CB_TREE(current_statement),
			_("target of RETURNING is not a data pointer"));
		return 1;
	}
	if (cb_listing_xref) {
		cobc_xref_set_receiving (returning);
	}
	return 0;
}

void
cb_emit_allocate_identifier (cb_tree allocate_identifier, cb_tree returning, const int init_flag)
{
	char		buff[32];

	if (cb_validate_one (allocate_identifier)
	 || cb_validate_one (returning)) {
		return;
	}

	/* syntax checks */
	if (!(CB_REFERENCE_P(allocate_identifier) &&
		    CB_FIELD_PTR (allocate_identifier)->flag_item_based)) {
		if (cb_relaxed_syntax_checks) {
			if (CB_FIELD_PTR (allocate_identifier)->storage != CB_STORAGE_LINKAGE) {
				/* Micro Focus still does not allow BASED items,
				   but allows a LINKAGE item to be the target for ALLOCATE instead */
				cb_error_x (CB_TREE (current_statement),
					_("cannot change address of '%s', which is not BASED or a LINKAGE item"),
					cb_name (allocate_identifier));
				return;
			}
		} else {
			cb_error_x (CB_TREE(current_statement),
				_("target of ALLOCATE must have BASED clause"));
			return;
		}
	}
	if (check_allocate_returning (returning)) {
		return;
	}

	/* code to emit for:	ALLOCATE identifier [INITIALIZED] [RETURNING x] */
	sprintf (buff, "%d", CB_FIELD_PTR (allocate_identifier)->memory_size);
	cb_emit (CB_BUILD_FUNCALL_4 ("cob_allocate",
			CB_BUILD_CAST_ADDR_OF_ADDR (allocate_identifier),
			returning, cb_build_numeric_literal (0, buff, 0), NULL));
	/* ALLOCATE identifier INITIALIZED -> implicit
	   INITIALIZE identifier WITH FILLER ALL TO VALUE THEN TO DEFAULT */
	if (init_flag) {
		current_statement->not_ex_handler =
			cb_build_initialize (allocate_identifier, cb_true, NULL, 1, STMT_ALLOCATE, 0);
	}
}

void
cb_emit_allocate_characters (cb_tree size, cb_tree initialized_to, cb_tree returning)
{
	if (cb_validate_one (size)
	 || cb_validate_one (initialized_to)
	 || cb_validate_one (returning)) {
		return;
	}

	/* syntax checks */
	if (size) {
		if (CB_TREE_CLASS (size) != CB_CLASS_NUMERIC) {
			cb_error_x (CB_TREE(current_statement),
				_("amount must be specified as a numeric expression"));
			return;
		}
	}
	if (initialized_to && !cb_category_is_alpha (initialized_to)) {
		cb_error_x (CB_TREE (current_statement),
			_("INITIALIZED TO item is not alphanumeric"));
	}
	if (check_allocate_returning (returning)) {
		return;
	}

	/* code to emit for
	   ALLOCATE size CHARACTERS [INITIALIZED [TO x]] RETURNING alloc_return */
	cb_emit (CB_BUILD_FUNCALL_4 ("cob_allocate", NULL, returning, size, initialized_to));
}


/* ALTER statement */

void
cb_emit_alter (cb_tree source, cb_tree target)
{
	if (source == cb_error_node) {
		return;
	}
	if (target == cb_error_node) {
		return;
	}
	CB_REFERENCE(source)->flag_alter_code = 1;
	cb_emit (cb_build_alter (source, target));
}

/* CALL statement */

static const char *
get_constant_call_name (cb_tree prog)
{
	/* plain literal or constant (level 78 item, 01 CONSTANT, SYMBOLIC CONSTANT) */
	if (CB_LITERAL_P (prog) && CB_TREE_CATEGORY (prog) != CB_CATEGORY_NUMERIC) {
		return (const char *)CB_LITERAL (prog)->data;
	/* reference (ideally on a prototype) */
	} else if (CB_REFERENCE_P (prog)) {
		cb_tree x = cb_ref (prog);
		if (CB_PROTOTYPE_P (x)) {
			return CB_PROTOTYPE (x)->ext_name;
		}
	}
	return NULL;
}

void
cb_emit_call (cb_tree prog, cb_tree par_using, cb_tree returning,
	      cb_tree on_exception, cb_tree not_on_exception,
	      cb_tree convention, cb_tree newthread, cb_tree handle,
	      int call_line_number)
{
	cb_tree				l;
	cb_tree				check_list;
	cb_tree				x;
	struct cb_field			*f;
	const struct system_table	*psyst;
	const char			*entry;
	const char			*constant_call_name = get_constant_call_name (prog);
	char				c;
	cob_s64_t			val;
	cob_s64_t			valmin;
	cob_s64_t			valmax;
	cob_u32_t			is_sys_call;
	cob_u32_t			is_sys_idx;
	int				error_ind;
	int				call_conv;
	unsigned int		numargs;

	if (CB_INTRINSIC_P (prog)) {
		if (CB_INTRINSIC (prog)->intr_tab->category != CB_CATEGORY_ALPHANUMERIC) {
			cb_error_x (CB_TREE (current_statement),
				    _("only alphanumeric FUNCTION types are allowed here"));
			return;
		}
	}
	if (returning && returning != cb_null) {
		if (CB_TREE_CLASS (returning) != CB_CLASS_NUMERIC &&
		    CB_TREE_CLASS (returning) != CB_CLASS_POINTER) {
			cb_error_x (CB_TREE (current_statement),
					_("invalid RETURNING field"));
				return;
			}
		}

	error_ind = 0;

	if (convention) {
		if (CB_INTEGER_P (convention)) {
			call_conv = CB_INTEGER (convention)->val;
		} else {
			call_conv = cb_get_int (convention);
		}
	} else {
		call_conv = 0;
	}
#ifndef	_WIN32
	if (call_conv & CB_CONV_STDCALL) {
		call_conv &= ~CB_CONV_STDCALL;
		cb_warning (cb_warn_additional, _("STDCALL not available on this platform"));
	}
#elif	defined(_WIN64)
	if (call_conv & CB_CONV_STDCALL) {
		cb_warning (cb_warn_additional, _("STDCALL used on 64-bit Windows platform"));
	}
#endif
	if ((call_conv & CB_CONV_STATIC_LINK) && !constant_call_name) {
		cb_error_x (CB_TREE (current_statement),
			_("STATIC CALL convention requires a literal program name"));
		error_ind = 1;
	}

	if (handle && !usage_is_thread_handle(handle)) {
		cb_error_x (handle, _("HANDLE must be either a generic or a THREAD HANDLE"));
		error_ind = 1;
	}

	numargs = 0;
	check_list = NULL;
	for (l = par_using; l; l = CB_CHAIN (l), numargs++) {
		x = CB_VALUE (l);
		if (x == cb_error_node) {
			error_ind = 1;
			continue;
		}
		if (CB_NUMERIC_LITERAL_P (x)) {
			if (CB_PURPOSE_INT (l) != CB_CALL_BY_VALUE) {
				continue;
			}
			if (CB_SIZES_INT_UNSIGNED(l) &&
			    CB_LITERAL (x)->sign < 0) {
				cb_error_x (x, _("numeric literal is negative"));
				error_ind = 1;
				continue;
			}
			val = 0;
			valmin = 0;
			valmax = 0;
			switch (CB_SIZES_INT (l)) {
			case CB_SIZE_1:
				val = cb_get_long_long (x);
				if (CB_SIZES_INT_UNSIGNED(l)) {
					valmin = 0;
					valmax = UCHAR_MAX;
				} else {
					valmin = CHAR_MIN;
					valmax = CHAR_MAX;
				}
				break;
			case CB_SIZE_2:
				val = cb_get_long_long (x);
				if (CB_SIZES_INT_UNSIGNED(l)) {
					valmin = 0;
					valmax = USHRT_MAX;
				} else {
					valmin = SHRT_MIN;
					valmax = SHRT_MAX;
				}
				break;
			case CB_SIZE_4:
				val = cb_get_long_long (x);
				if (CB_SIZES_INT_UNSIGNED(l)) {
					valmin = 0;
					valmax = UINT_MAX;
				} else {
					valmin = INT_MIN;
					valmax = INT_MAX;
				}
				break;
			case CB_SIZE_8:
			case CB_SIZE_AUTO:
				if (CB_SIZES_INT_UNSIGNED(l)) {
					if (CB_LITERAL (x)->size < 20) {
						break;
					}
					if (CB_LITERAL (x)->size > 20) {
						valmin = 1;
						break;
					}
					if (memcmp (CB_LITERAL (x)->data,
						    "18446744073709551615",
						    (size_t)20) > 0) {
						valmin = 1;
						break;
					}
				} else {
					if (CB_LITERAL (x)->size < 19) {
						break;
					}
					if (CB_LITERAL (x)->size > 19) {
						valmin = 1;
						break;
					}
					if (memcmp (CB_LITERAL (x)->data,
						    CB_LITERAL (x)->sign ?
								"9223372036854775808" :
								"9223372036854775807",
						    (size_t)19) > 0) {
						valmin = 1;
						break;
					}
				}
				break;
			default:
				break;
			}
			if (!valmin && !valmax) {
				continue;
			}
			if (val < valmin || val > valmax) {
				cb_error_x (x, _("numeric literal exceeds size limits"));
				error_ind = 1;
			}
			continue;
		}
		if (CB_CONST_P (x)
		 && x != cb_null
		 && x != cb_space
		 && x != cb_zero) {
			if (x == cb_space ||
				x == cb_norm_low ||
				x == cb_norm_high||
				x == cb_quote) {
				c = (char)get_value (x);
				x = cb_build_alphanumeric_literal (&c, 1);
			} else if (x == cb_zero) {
				x = cb_build_numsize_literal ("0", 1, 0);
			} else{
				cb_error_x (x, _("figurative constant %s invalid here"), cb_name (x));
				error_ind = 1;
				continue;
			}
		}
		if (CB_FIELD_P (x)) {	/* TODO: remove after 3.1 RC1 */
			cobc_abort ("should be not be a field", 1);
		}
		if ((CB_REFERENCE_P (x) && CB_FIELD_P(CB_REFERENCE(x)->value))) {
			f = CB_FIELD (cb_ref (x));
			if (f->level == 88) {
				cb_error_x (x, _("'%s' is not a valid data name"), CB_NAME (x));
				error_ind = 1;
				continue;
			}
			if (CB_PURPOSE_INT (l) == CB_CALL_BY_REFERENCE) {
				if (f->level != 01 && f->level != 77) {
					cb_warning_x (cb_warn_call_params, x,
						_("'%s' is not a 01 or 77 level item"), CB_NAME (x));
				}
				check_list = cb_list_add (check_list, x);
			} else if (f->flag_any_length) {
				cb_error_x (x, _("'%s' ANY LENGTH item not passed BY REFERENCE"), CB_NAME (x));
				error_ind = 1;
				continue;
			}

		}
	}

	if (check_list != NULL) {
		for (l = check_list; l; l = CB_CHAIN (l)) {
			cb_tree	l2 = CB_VALUE (l);
			x = cb_ref (l2);
			if (x != cb_error_node) {
				for (l2 = check_list; l2 != l; l2 = CB_CHAIN (l2)) {
					if (cb_ref (CB_VALUE (l2)) == x) {
						cb_warning_x (COBC_WARN_FILLER, l,
							_("duplicate USING BY REFERENCE item '%s'"),
							cb_name (CB_VALUE (l)));
						CB_VALUE (l) = cb_error_node;
						break;
					}
				}
			}
		}
	}

	is_sys_call = 0;
	if (constant_call_name) {
		const char			*p = constant_call_name;
		entry = p;
		for (; *p; ++p) {
			if (*p == '/' || *p == '\\') {
				entry = p + 1;
			}

		}

		is_sys_idx = 1;
		for (psyst = system_tab; psyst->syst_name; psyst++, is_sys_idx++) {
			if (!strcmp(entry, (const char *)psyst->syst_name)) {
				char *name;
				char xname[7];
				if (psyst->syst_name[1]) {
					name = (char *)psyst->syst_name;
				} else {
					sprintf (xname, "X\"%2X\"", (unsigned char)psyst->syst_name[0]);
					name = (char *)&xname;
				}
				if (psyst->syst_params_min > numargs) {
					cb_error_x (CB_TREE (current_statement),
						_("wrong number of CALL parameters for '%s', %d given, %d expected"),
						name, numargs, psyst->syst_params_min);
					return;
				} else if (psyst->syst_params_max < numargs) {
					cb_warning_x (COBC_WARN_FILLER, CB_TREE (current_statement),
						_("wrong number of CALL parameters for '%s', %d given, %d expected"),
						name, numargs, psyst->syst_params_max);
				}
				is_sys_call = is_sys_idx;
				break;
			}
		}
		if (cb_listing_xref) {
			cobc_xref_call (entry, call_line_number, 0, is_sys_call);
		}
	}
	else if (cb_listing_xref && CB_REFERENCE_P(prog)) {
		entry = CB_FIELD(CB_REFERENCE(prog)->value)->name;
		cobc_xref_call (entry, call_line_number, 1, 0);
	}

	if (error_ind) {
		return;
	}

	/* adjust maximum call parameters for later generation */
	if (numargs > current_program->max_call_param) {
		current_program->max_call_param = numargs;
	}

#if 0 /* TODO: implement THREADs in libcob */
	  /* remark: this won't work as the CALL has to be started in the new thread
	if (newthread) {
		cb_emit (CB_BUILD_FUNCALL_0 ("cob_threadstart"));
	}
	if (handle) {
		cb_emit (CB_BUILD_FUNCALL_1 ("cob_get_threadhandle", handle));
	} */
#else
	COB_UNUSED (newthread);
#endif
	cb_emit (cb_build_call (prog, par_using, on_exception, not_on_exception,
				returning, is_sys_call, call_conv));
}

/* CANCEL statement */

void
cb_emit_cancel (cb_tree prog)
{
	if (cb_validate_one (prog)) {
		return;
	}
	cb_emit (cb_build_cancel (prog));
}

/* CLOSE statement */

void
cb_emit_close (cb_tree file, cb_tree opt)
{
	struct cb_file	*f;

	file = cb_ref (file);
	if (file == cb_error_node) {
		return;
	}
	current_statement->file = file;
	f = CB_FILE (file);

	if (f->organization == COB_ORG_SORT) {
		cb_error_x (CB_TREE (current_statement),
				_("%s not allowed on %s files"), "CLOSE", "SORT");
	}

	if (f->extfh) {
		cb_emit (CB_BUILD_FUNCALL_5 ("cob_extfh_close", f->extfh, file,
					     f->file_status, opt, cb_int0));
	} else {
		cb_emit (CB_BUILD_FUNCALL_4 ("cob_close", file,
					     f->file_status, opt, cb_int0));
	}

	/* Check for file debugging */
	if (current_program->flag_debugging
	 && !current_statement->flag_in_debug
	 && CB_FILE(file)->flag_fl_debug) {
		cb_emit (cb_build_debug (cb_debug_name, f->name, NULL));
		cb_emit (cb_build_move (cb_space, cb_debug_contents));
		cb_emit (cb_build_debug_call (f->debug_section));
	}
}

/* COMMIT statement */

void
cb_emit_commit (void)
{
	cb_emit (CB_BUILD_FUNCALL_0 ("cob_commit"));
}

/* CONTINUE statement */

void
cb_emit_continue (cb_tree continue_after)
{
	if (continue_after) {
		/* CONTINUE AFTER exp SECONDS */
		if (!cb_verify (cb_continue_after, _("AFTER phrase in CONTINUE statement"))
		 || cb_validate_one (continue_after)) {
			return;
		}
		if (CB_TREE_CLASS (continue_after) != CB_CLASS_NUMERIC) {
			cb_error_x (CB_TREE(current_statement),
				_("amount must be specified as a numeric expression"));
			return;
		}
		cb_emit (CB_BUILD_FUNCALL_1 ("cob_continue_after",
			 continue_after));
		return;
	}
	/* "common" CONTINUE */
	cb_emit (cb_build_continue ());
}

/* DELETE statement */

void
cb_emit_delete (cb_tree file)
{
	struct cb_file	*f;

	file = cb_ref (file);
	if (file == cb_error_node) {
		return;
	}
	current_statement->file = file;
	f = CB_FILE (file);

	if (cb_listing_xref) {
		/* add a "receiving" entry for the file */
		cobc_xref_link (&f->xref, current_statement->common.source_line, 1);
	}

	if (f->organization == COB_ORG_SORT) {
		cb_error_x (CB_TREE (current_statement),
				_("%s not allowed on %s files"), "DELETE", "SORT");
		return;
	} else if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
		cb_error_x (CB_TREE (current_statement),
				_("%s not allowed on %s files"), "DELETE", "LINE SEQUENTIAL");
		return;
	} else if (f->organization == COB_ORG_SEQUENTIAL) {
		cb_error_x (CB_TREE (current_statement),
				_("%s not allowed on %s files"), "DELETE", "SEQUENTIAL");
		return;
	}

	/* Check for file debugging */
	if (current_program->flag_debugging
	 && !current_statement->flag_in_debug
	 && f->flag_fl_debug) {
		/* Gen callback after delete but before exception test */
		current_statement->flag_callback = 1;
	}

	if (f->extfh) {
		cb_emit (CB_BUILD_FUNCALL_3 ("cob_extfh_delete", f->extfh, file,
					     f->file_status));
	} else {
		cb_emit (CB_BUILD_FUNCALL_2 ("cob_delete", file,
					     f->file_status));
	}
}

void
cb_emit_delete_file (cb_tree file)
{
	file = cb_ref (file);
	if (file == cb_error_node) {
		return;
	}
	if (CB_FILE (file)->organization == COB_ORG_SORT) {
		cb_error_x (CB_TREE (current_statement),
				_("%s not allowed on %s files"), "DELETE FILE", "SORT");
		return;
	}
	if (current_statement->ex_handler == NULL
	 && current_statement->not_ex_handler == NULL)
	  	current_statement->handler_type = NO_HANDLER;

	/* Check for file debugging */
	if (current_program->flag_debugging
	 && !current_statement->flag_in_debug
	 && CB_FILE(file)->flag_fl_debug) {
		/* Gen callback after delete but before exception test */
		current_statement->flag_callback = 1;
	}

	cb_emit (CB_BUILD_FUNCALL_3 ("cob_delete_file", file,
				     CB_FILE(file)->file_status, cb_int0));
}


static int
validate_attrs (cb_tree pos, cb_tree fgc, cb_tree bgc, cb_tree scroll,
		cb_tree size_is, cb_tree control, cb_tree color)
{
	return 	cb_validate_one (pos)
		|| cb_validate_one (fgc)
		|| cb_validate_one (bgc)
		|| cb_validate_one (scroll)
		|| cb_validate_one (size_is)
		|| cb_validate_one (control)
		|| cb_validate_one (color);
}

static void
initialize_attrs (const struct cb_attr_struct * const attr_ptr,
		  cb_tree * const fgc, cb_tree * const bgc,
		  cb_tree * const scroll, cb_tree * const size_is,
		  cb_tree * const control, cb_tree * const color,
		  cob_flags_t * const dispattrs)
{
	if (attr_ptr) {
		*fgc = attr_ptr->fgc;
		*bgc = attr_ptr->bgc;
		*scroll = attr_ptr->scroll;
		*size_is = attr_ptr->size_is;
		*control = attr_ptr->control;
		*color = attr_ptr->color;
		*dispattrs = attr_ptr->dispattrs;
	} else {
		*fgc = NULL;
		*bgc = NULL;
		*scroll = NULL;
		*size_is = NULL;
		*control = NULL;
		*color = NULL;
		*dispattrs = 0;
	}
}


/* DISPLAY [FLOATING | INITIAL] WINDOW statement */

void
cb_emit_display_window (cb_tree type, cb_tree own_handle, cb_tree upon_handle,
		 cb_tree line_column, struct cb_attr_struct *attr_ptr)
{
	cb_tree		fgc;
	cb_tree		bgc;
	cb_tree		scroll;
	cb_tree		size_is;	/* WITH SIZE IS */
	cb_tree		control;	/* CONTROL VALUE numeric added window attributes */
	cb_tree		color;		/* COLOR numeric added attributes */
	cob_flags_t		disp_attrs;
	int ret = 0;

	/* type may be: NULL     --> normal WINDOW,
	                cb_int0  --> FLOATING WINDOW
	   otherwise it is an INITIAL WINDOW type:
	   cb_int1 = INITIAL, cb_int2 = STANDARD, cb_int3 = INDEPENDENT */
	if ((type == cb_int1 || type == cb_int2) && line_column != NULL) {
		cb_error_x (line_column, _("positions cannot be specified for main windows"));
	}

	/* Validate line_column and the attributes */
	initialize_attrs (attr_ptr, &fgc, &bgc, &scroll, &size_is,
			&control, &color, &disp_attrs);
	if (validate_attrs (line_column, fgc, bgc, scroll, size_is, control, color)) {
		ret++;
	}

	if (own_handle && !usage_is_window_handle (own_handle)) {
		cb_error_x (own_handle, _("HANDLE must be either a generic or a WINDOW HANDLE or X(10)"));
		ret++;
	}
	if (upon_handle && !usage_is_window_handle (upon_handle)) {
		cb_error_x (upon_handle, _("HANDLE must be either a generic or a WINDOW HANDLE or X(10)"));
		ret++;
	}
	/* all syntax checks done, so leave on error */
	if (ret) {
		return;
	}

#if 0 /* TODO, likely as multiple functions */
	/* note that CONTROL VALUE in WINDOW related statements 
	   is different from CONTROL, which is not available for DISPLAY WINDOW */
	cb_emit (CB_BUILD_FUNCALL_2 ("cob_display_window", own_handle, upon_handle));
#endif
}


/* CLOSE WINDOW statement (WITH NO DISPLAY)
   Note: CLOSE WINDOW without WITH NO DISPLAY is resolved as cb_emit_destroy
*/

void
cb_emit_close_window (cb_tree handle, cb_tree no_display)
{
	if (handle && !usage_is_window_handle (handle)) {
		cb_error_x (handle, _("HANDLE must be either a generic or a WINDOW HANDLE or X(10)"));
		return;
	}
	if (no_display) {
		cb_emit (CB_BUILD_FUNCALL_1 ("cob_close_window", handle));
	} else {
		cb_emit_destroy (CB_LIST_INIT (handle));
	}
}


/* DESTROY statement */

void
cb_emit_destroy (cb_tree controls)
{
#if 0 /* TODO */
	cb_tree		l;
	struct cb_field	*f;
	int		i;
#endif

	/* DESTROY ALL CONTROLS */
	if (!controls) {
		cb_emit (CB_BUILD_FUNCALL_1 ("cob_destroy_control", NULL));
		return;
	}

	/* DESTROY list-of-controls */
	if (cb_validate_list (controls)) {
		return;
	}
#if 0 /* TODO */
	for (l = controls, i = 1; l; l = CB_CHAIN (l), i++) {
		if (CB_REF_OR_FIELD_P (CB_VALUE (l))) {
			f = CB_FIELD_PTR (CB_VALUE (l));
			if (!f->...checks) {
				...
			}
			cb_emit (CB_BUILD_FUNCALL_1 ("cob_destroy_control", CB_VALUE (l)));
		} else {
			...
		}
	}
#endif
}

/* DISPLAY statement */

/* DISPLAY ... UPON ENVIRONMENT NAME */
void
cb_emit_env_name (cb_tree value)
{
	if (cb_validate_one (value)) {
		return;
	}
	cb_emit (CB_BUILD_FUNCALL_1 ("cob_display_environment", value));
}

/* DISPLAY ... UPON ENVIRONMENT VALUE */
void
cb_emit_env_value (cb_tree value)
{
	if (cb_validate_one (value)) {
		return;
	}
	cb_emit (CB_BUILD_FUNCALL_1 ("cob_display_env_value", value));
}

/* DISPLAY ... UPON ARGUMENT-NUMBER */
void
cb_emit_arg_number (cb_tree value)
{
	if (cb_validate_one (value)) {
		return;
	}
	cb_emit (CB_BUILD_FUNCALL_1 ("cob_display_arg_number", value));
}

/* DISPLAY ... UPON COMMAND-LINE */
void
cb_emit_command_line (cb_tree value)
{
	if (cb_validate_one (value)) {
		return;
	}
	cb_emit (CB_BUILD_FUNCALL_1 ("cob_display_command_line", value));
}

/*
  Return 1 if a value in the list values has an unexpected type (tree tag, to be
  precise) or is an error node. Otherwise, return 0.
*/
static int
validate_types_of_display_values (struct cb_list *l)
{
	cb_tree		x;

	for (; l; l = l->chain ? CB_LIST(l->chain): NULL) {
		x = l->value;
		if (x == cb_error_node) {
			return 1;
		}

		switch (CB_TREE_TAG (x)) {
		case CB_TAG_LITERAL:
		case CB_TAG_INTRINSIC:
		case CB_TAG_CONST:
		case CB_TAG_STRING:
		case CB_TAG_INTEGER:
			break;
		case CB_TAG_REFERENCE:
			if (!CB_FIELD_P(CB_REFERENCE(x)->value)) {
				cb_error_x (x, _("'%s' is an invalid type for DISPLAY operand"), cb_name (x));
				return 1;
			}
			break;
		default:
			cb_error_x (x, _("invalid type for DISPLAY operand"));
			return 1;
		}
	}

	return 0;
}

static void
emit_device_display (cb_tree values, cb_tree upon, cb_tree no_adv)
{
	cb_tree	p;

	p = CB_BUILD_FUNCALL_3 ("cob_display", upon, no_adv, values);
	CB_FUNCALL (p)->varcnt = (int)cb_list_length (values);
	CB_FUNCALL (p)->nolitcast = 1;
	cb_emit (p);
}

static void
increment_field_ref_counts (cb_tree value_list)
{
	cb_tree	x;

	for (; value_list; value_list = CB_CHAIN (value_list)) {
		x = CB_VALUE (value_list);
		if (CB_FIELD_P (x)) {
			CB_FIELD (cb_ref (x))->count++;
		}
	}
}

static void
emit_screen_display (const cb_tree x, const cb_tree pos)
{
	cb_tree	line = NULL;
	cb_tree	column = NULL;

	get_line_and_column_from_pos (pos, &line, &column);
	cb_emit (CB_BUILD_FUNCALL_4 ("cob_screen_display", x, line, column,
				     cb_int (line_col_zero_is_supported ())));
}

static void
process_special_values (cb_tree value, cb_tree * const size_is, cob_flags_t * const attrs)
{
	/*
	  The following are MF extensions. MF specifically
	  states X"01", X"02" and X"07", so the values do not
	  need to be changed for other codesets.

	  For all special values, the SIZE clause is ignored.
	*/

	/* LOW-VALUES positions cursor */
	if (value == cb_low) {
		*attrs |= COB_SCREEN_NO_DISP;
		*size_is = NULL;
		return;
	}

	if (!cb_display_special_fig_consts) {
		return;
	}

	/* SPACE clears to end of screen */
	if (value == cb_space) {
		*attrs |= COB_SCREEN_ERASE_EOS;
		*attrs |= COB_SCREEN_NO_DISP;
		*size_is = NULL;
	} else if (CB_LITERAL_P (value) && CB_LITERAL (value)->all &&
		   CB_LITERAL (value)->size == 1) {
		if (CB_LITERAL (value)->data[0] == '\1') {
			/* ASCII char \1 is SOH, start of header */
			*attrs |= COB_SCREEN_ERASE_EOL;
			*attrs |= COB_SCREEN_NO_DISP;
			*size_is = NULL;
		} else if (CB_LITERAL (value)->data[0] == '\2') {
			/* ASCII char \2 is STX, start of text */
			cb_emit (CB_BUILD_FUNCALL_0 ("cob_sys_clear_screen"));
			/* We might still need to position the cursor */
			*attrs |= COB_SCREEN_NO_DISP;
			*size_is = NULL;
		} else if (CB_LITERAL (value)->data[0] == '\7') {
			/* ASCII char \7 is BEL, bell */
			*attrs |= COB_SCREEN_BELL;
			*attrs |= COB_SCREEN_NO_DISP;
			*size_is = NULL;
		}
	}
}

static cb_tree
get_integer_literal_pair (const char *value)
{
	const cb_tree	num = cb_build_numeric_literal (1, value, 0);

	return CB_BUILD_PAIR (num, num);
}

static COB_INLINE COB_A_INLINE cb_tree
get_after_last_line_column (void)
{
	return get_integer_literal_pair ("0");
}

static COB_INLINE COB_A_INLINE cb_tree
get_origin_line_column (void)
{
	return get_integer_literal_pair ("1");
}

static void
emit_screen_displays (cb_tree screen_list, cb_tree line_col_for_last)
{
	cb_tree	l;
	cb_tree pos;
	cb_tree	screen_ref;

	/* note: screen_list validated by caller cb_emit_display */
	for (l = screen_list; l; l = CB_CHAIN (l)) {
		/*
		  LINE 1 COL 1 is assumed, not LINE 0 COL 0 as in field
		  DISPLAYs. (This is RM-COBOL behaviour, who support multiple
		  screens in one DISPLAY.)
		*/
		if (CB_CHAIN (l) || !line_col_for_last) {
			pos = get_origin_line_column ();
		} else {
			pos = line_col_for_last;
		}

		screen_ref = CB_VALUE (l);
		output_screen_from (CB_FIELD (cb_ref (screen_ref)), 0);

		gen_screen_ptr = 1;
		emit_screen_display (screen_ref, pos);
		gen_screen_ptr = 0;
	}
}

static cb_tree
get_default_field_line_column (const int is_first_display_item)
{
	/*
	  Note if LINE/COL 0 is not allowed, then this must be a
	  standard format DISPLAY (DISPLAY ... UPON CRT), which must
	  follow previous items, unlike the DISPLAY with screen clauses
	  (DISPLAY ... WITH HIGHLIGHT, etc.).
	*/
	const int	display_after_last =
		!line_col_zero_is_supported ()
		|| !is_first_display_item
		|| cb_line_col_zero_default;

	if (display_after_last) {
		return get_after_last_line_column ();
	} else {
		return get_origin_line_column ();
	}

}

static void
emit_default_field_display_for_all_but_last (cb_tree values, cb_tree size_is,
					     const int is_first_display_list)
{
	cb_tree	l;
	int	is_first_display_item = is_first_display_list;
	cb_tree	pos;
	cob_flags_t	disp_attrs;
	cb_tree	x;

	/* LCOV_EXCL_START */
	if (!values) {
		cobc_err_msg (_("call to '%s' with invalid parameter '%s'"),
			"emit_default_field_display_for_all_but_last", "values");
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */

	for (l = values; l && CB_CHAIN (l); l = CB_CHAIN (l)) {
		pos = get_default_field_line_column (is_first_display_item);
		is_first_display_item = 0;

		x = CB_VALUE (l);
		disp_attrs = 0;
		process_special_values (x, &size_is, &disp_attrs);

		emit_field_accept_display (STMT_DISPLAY, x, disp_attrs,
			pos, NULL, NULL, NULL, NULL, NULL,
			size_is, NULL, NULL,  NULL);
	}
}

static void
emit_field_display_for_last (cb_tree values, cb_tree line_column, cb_tree fgc,
			     cb_tree bgc, cb_tree scroll, cb_tree size_is,
			     cb_tree control, cb_tree color, cob_flags_t disp_attrs,
			     const int is_first_display_list)
{
	cb_tree		l;
	cb_tree		last_elt;
	int	is_first_item;

	/* DISPLAY OMITTED ? */
	if (CB_LIST(values)->value == cb_null) {
		l = last_elt = cb_null;
	} else {
		for (l = values; l && CB_CHAIN (l); l = CB_CHAIN (l));
		/* LCOV_EXCL_START */
		if (!l) {
			cobc_err_msg (_("call to '%s' with invalid parameter '%s'"),
				"emit_field_display_for_last", "values");
			COBC_ABORT ();
		}
		/* LCOV_EXCL_STOP */
		last_elt = CB_VALUE (l);
	}

	if (line_column == NULL) {
		is_first_item = is_first_display_list && l == values;
		line_column = get_default_field_line_column (is_first_item);
	}

	process_special_values (last_elt, &size_is, &disp_attrs);
	emit_field_accept_display (STMT_DISPLAY, last_elt, disp_attrs,
		line_column, fgc, bgc, scroll, NULL, NULL,
		size_is, control, color, NULL);
}

void
cb_emit_display (cb_tree values, cb_tree upon, cb_tree no_adv,
		 cb_tree line_column, struct cb_attr_struct *attr_ptr,
		 int is_first_display_list,
		 const enum cb_display_type display_type)
{
	cb_tree		fgc;
	cb_tree		bgc;
	cb_tree		scroll;
	cb_tree		size_is;	/* WITH SIZE IS */
	cb_tree		control;	/* CONTROL IS variable-named attributes */
	cb_tree		color;		/* COLOR variable numeric added attributes */
	cob_flags_t		disp_attrs;
	cb_tree		m;
	struct cb_field	*f = NULL;

	/* Validate upon and values */
	if (upon == cb_error_node
	 || cb_validate_list (values)
	 || validate_types_of_display_values (CB_LIST (values))) {
		return;
	}
	if (current_statement->ex_handler == NULL
	 && current_statement->not_ex_handler == NULL)
	  	current_statement->handler_type = NO_HANDLER;

	/* Validate line_column and the attributes */
	initialize_attrs (attr_ptr, &fgc, &bgc, &scroll, &size_is,
			&control, &color, &disp_attrs);
	if (validate_attrs (line_column, fgc, bgc, scroll, size_is, control, color)) {
		return;
	}

	/* Emit appropriate function call(s) */
	switch (display_type) {
	case DEVICE_DISPLAY:

		/* CGI: DISPLAY external-form */
		/* TODO: CHECKME, see Patch #27 */
		m = CB_LIST (values)->value;
		if (CB_REF_OR_FIELD_P (m)) {
			f = CB_FIELD_PTR (m);
		}
		if (f && (f->flag_is_external_form || f->external_form_identifier)) {
			/* static content has both attributes */
			if (f->flag_is_external_form && f->external_form_identifier) {
#if 0 /* TODO: implement CGI runtime, see Patch #27 */
				cb_emit (CB_BUILD_FUNCALL_1 ("cob_cgi_static", f->external_form_identifier));
#endif
				return;
			}
			cb_emit_display_external_form (m);
			/* TODO: CHECKME, DISPLAY without identifier (template) is a "debug display" */
			if (f->external_form_identifier) {
				m = f->external_form_identifier;
			} else {
				m = cb_build_alphanumeric_literal (f->name, strlen(f->name));
			}
#if 0 /* TODO: implement CGI runtime, see Patch #27 */
			cb_emit (CB_BUILD_FUNCALL_1 ("cob_cgi_renderTpl", m));
#endif
			return;
		}

		if (upon == NULL) {
			upon = cb_int0;
		}
		emit_device_display (values, upon, no_adv);
		increment_field_ref_counts (values);
		break;

	case SCREEN_DISPLAY:
		emit_screen_displays (values, line_column);
		break;

	case FIELD_ON_SCREEN_DISPLAY:
		/* no DISPLAY OMITTED */
		if (CB_LIST(values)->value != cb_null) {
			emit_default_field_display_for_all_but_last (values, size_is,
									 is_first_display_list);
		}
		emit_field_display_for_last (values, line_column, fgc, bgc,
					     scroll, size_is, control, color, disp_attrs,
					     is_first_display_list);

		break;

	default:
		/* Any other type will already have emitted errors */
		;
	}
}

cb_tree
cb_build_display_mnemonic (cb_tree x)
{
	if (cb_ref (x) == cb_error_node) {
		return cb_int0;
	}

	switch (CB_SYSTEM_NAME (cb_ref (x))->token) {
	case CB_DEVICE_CONSOLE:
	case CB_DEVICE_SYSOUT:
		return cb_int0;
	case CB_DEVICE_SYSERR:
		return cb_int1;
	case CB_DEVICE_PRINTER:
		return cb_int2;
	case CB_DEVICE_SYSPCH:
		return cb_int3;
	default:
		cb_error_x (x, _("'%s' is not an output device"), CB_NAME (x));
		return cb_int0;
	}
}

cb_tree
cb_build_display_name (cb_tree x)
{
	const char	*name;
	cb_tree		sys;

	if (x == cb_error_node) {
		return cb_error_node;
	}
	name = CB_NAME (x);
	/* Allow direct reference to a device name (not defined as mnemonic name) */
	sys = get_system_name (name);
	if (sys) {
		switch (CB_SYSTEM_NAME (sys)->token) {
		case CB_DEVICE_CONSOLE:
		case CB_DEVICE_SYSOUT:
			sys = cb_int0;
			break;
		case CB_DEVICE_SYSERR:
			sys = cb_int1;
			break;
		case CB_DEVICE_PRINTER:
			sys = cb_int2;
			break;
		case CB_DEVICE_SYSPCH:
			sys = cb_int3;
			break;
		default:
			cb_error_x (x, _("'%s' is not an output device"), name);
			return cb_error_node;
		}
		if (!cb_device_mnemonics
		 && !cb_relaxed_syntax_checks) {
			/* TODO: this is not allowed and therefore should raise an error */
			cb_warning_x (COBC_WARN_FILLER, x,
				_("'%s' is not defined in SPECIAL-NAMES"), name);
		}
		return sys;
	} else if (is_default_reserved_word (CB_NAME (x))) {
		cb_error_x (x, _("unknown device '%s'; it may exist in another dialect"),
				    name);
	} else {
		cb_error_x (x, _("unknown device '%s'; not defined in SPECIAL-NAMES"), name);
	}
	return cb_error_node;
}

/* DIVIDE statement - with REMAINDER
   other variants are handled in cb_emit_arithmetic -> cb_build_div */

void
cb_emit_divide (cb_tree dividend, cb_tree divisor, cb_tree quotient,
		cb_tree remainder)
{
	cb_tree quotient_field, remainder_field;

	if (cb_validate_one (dividend)
	 || cb_validate_one (divisor)
	 || cb_validate_one (CB_VALUE(quotient))
	 || cb_validate_one (CB_VALUE(remainder))) {
		return;
	}

	quotient_field = cb_check_numeric_edited_name (CB_VALUE(quotient));
	remainder_field = cb_check_numeric_edited_name (CB_VALUE(remainder));

	if (quotient_field == cb_error_node
	 || remainder_field == cb_error_node) {
		return;
	}

	cb_emit (CB_BUILD_FUNCALL_4 ("cob_div_quotient", dividend, divisor,
		quotient_field, build_store_option (quotient_field, CB_PURPOSE (quotient))));
	cb_emit (CB_BUILD_FUNCALL_2 ("cob_div_remainder",
		remainder_field, build_store_option (remainder_field, cb_int0)));
}

/* EVALUATE statement */

static cb_tree
evaluate_test (cb_tree s, cb_tree o)
{
	cb_tree		x;
	cb_tree		y;
	cb_tree		t;
	int		flag;

	/* ANY is always true */
	if (o == cb_any) {
		return cb_true;
	}

	/* Object TRUE or FALSE */
	if (o == cb_true) {
		return s;
	}
	if (o == cb_false) {
		return CB_BUILD_NEGATION (s);
	}
	if (o == cb_error_node) {
		return cb_error_node;
	}

	flag = CB_PURPOSE_INT (o);
	x = CB_PAIR_X (CB_VALUE (o));
	y = CB_PAIR_Y (CB_VALUE (o));

	/* Subject TRUE or FALSE */
	if (s == cb_true) {
		return flag ? CB_BUILD_NEGATION (x) : x;
	}
	if (s == cb_false) {
		return flag ? x : CB_BUILD_NEGATION (x);
	}

	/* x THRU y */
	if (y) {
		t = cb_build_binary_op (cb_build_binary_op (x, '[', s),
					'&',
					cb_build_binary_op (s, '[', y));

		return flag ? CB_BUILD_NEGATION (t) : t;
	}

	if (CB_REFERENCE_P(x) 
	 && CB_FIELD_P(CB_REFERENCE(x)->value) 
	 && CB_FIELD(CB_REFERENCE(x)->value)->level == 88) {
		cb_error_x (CB_TREE (current_statement),
			    _("invalid use of 88 level in WHEN expression"));
		return NULL;
	}

	/* Regular comparison */
	switch (flag) {
	case 0:
		/* Equal comparison */
		return cb_build_binary_op (s, '=', x);
	case 1:
		/* Unequal comparison */
		return cb_build_binary_op (s, '~', x);
	default:
		/* Class and relational conditions */
		return x;
	}
}

/* creating statements for EVALUATE entries;
   this may be called recursive for each set of WHEN + statements;
   parameters: subjects (may include true/false),
               cases    (1..n WHEN + statements),
			   internal exit label */
static void
build_evaluate (cb_tree subject_list, cb_tree case_list, cb_tree goto_end_label)
{
	cb_tree		whens, stmt;
	cb_tree		c1, c2, c3;

	if (case_list == NULL) {
		return;
	}

	whens = CB_VALUE (case_list);
	stmt  = CB_VALUE (whens);
	whens = CB_CHAIN (whens);
	c1 = NULL;

	/* For each WHEN sequence */
	for (; whens; whens = CB_CHAIN (whens)) {
		cb_tree		subjs, objs;
		c2 = NULL;
		/* Single WHEN test -> combine all "ALSO" with operator "&&" */
		for (subjs = subject_list, objs = CB_VALUE (whens);
		     subjs && objs;
		     subjs = CB_CHAIN (subjs), objs = CB_CHAIN (objs)) {
			c3 = evaluate_test (CB_VALUE (subjs), CB_VALUE (objs));
			if (c3 == NULL || c3 == cb_error_node) {
				return;
			}

			if (c2 == NULL) {
				c2 = c3;
			} else {
				c2 = cb_build_binary_op (c2, '&', c3);
				if (c2 == cb_error_node) {
					return;
				}
			}
		}
		if (subjs || objs) {
			cb_error_x (whens, _("wrong number of WHEN parameters"));
		}
		/* Combine multiple WHEN's that share a list of statements
		   with operator "||" */
		if (c1 == NULL) {
			c1 = c2;
		} else if (c2) {
			c1 = cb_build_binary_op (c1, '|', c2);
			if (c1 == cb_error_node) {
				return;
			}
		}
	}

	if (c1 == NULL) {
		int old_line = cb_source_line;
		const char *old_file = cb_source_file;

		cb_source_line = stmt->source_line;
		cb_source_file = stmt->source_file;

		cb_emit (cb_build_comment ("WHEN OTHER"));
		cb_emit (stmt);

		cb_source_file = old_file;
		cb_source_line = old_line;

	} else {
		c2 = stmt;
		/* Check if last statement is GO TO */
		for (c3 = stmt; c3; c3 = CB_CHAIN (c3)) {
			if (!CB_CHAIN(c3)) {
				break;
			}
		}
		if (c3 && CB_VALUE (c3) && CB_STATEMENT_P (CB_VALUE (c3))) {
			c3 = CB_STATEMENT (CB_VALUE (c3))->body;
			if (c3 && CB_VALUE (c3) && !CB_GOTO_P (CB_VALUE(c3))) {
				/* Append the jump */
				c2 = cb_list_add (stmt, goto_end_label);
			}
		}
		cb_emit (cb_build_if (cb_build_cond (c1), c2, NULL, STMT_WHEN));
		build_evaluate (subject_list, CB_CHAIN (case_list), goto_end_label);
	}
}

void
cb_emit_evaluate (cb_tree subject_list, cb_tree case_list)
{
	cb_tree	x;
	cb_tree subjs, whens, c1, d1;
	int		need_dec, used_dec;
	char	sbuf[16];

#if 0	/* TODO: check for "simple" EVALUATE and use a different codegen
	         --> if we have a _single_ selection-object and it is either
			 alphanumeric with size 1 or numeric, then generate a switch ()
			 instead (this most used case also removes the need for the
			 temporary selection-value mentioned below). */
	if (cb_list_length (subject_list) == 1) {
		const cb_tree sel_obj = CB_VALUE (subject_list);
		if (is_alphanumeric_lenth_one (sel_obj)
		 || is_numeric (sel_obj)) {
			....
		}
		return;
	}
#endif

	/* code to skip to internal label of END-EVALUATE */
	sprintf (sbuf, "goto %s%d;", CB_PREFIX_LABEL, cb_id);
	x = cb_build_direct (cobc_parse_strdup (sbuf), 0);

	/* Providing there are enough WHENs to warrant it and the EVALUATE
	 * expression is either an integer or decimal result
	 * iterate over subject_list here:
	      for each of its values that are a reference:
		      build creation of a temporary field with its value
			  and replace the reference in the subject_list with this

		This is necessary to follow the standard rule
		"At the beginning of the execution of the EVALUATE statement,
		  each selection subject is evaluated and assigned a value"
	    
		Currently we do the selection on _each_ WHEN, which is bad
		because (apart from not being correct per standard) this has
		side effects when the selection-object is an intrinsic or user
		function (as bad example: FUNCTION SECONDS-PAST-MIDNIGHT) because
		the selection changes between the WHENs and user-defined functions
		may be quite costly / adjust EXTERNAL variables used in the selection;
		Not doing that correct (one time) also leads to all runtime checks that
		are assigned to the selection-object being executed for each WHEN
		and if there _is_ a runtime error it is raised at the first WHEN, while
		it should be raised at the EVALUATE.
	*/
	used_dec = need_dec = 0;
	if (cb_list_length (case_list) > 2) {	/* Is it worth creating a temp variable? */
		for (subjs = subject_list; subjs; subjs = CB_CHAIN (subjs)) {
			c1 = CB_VALUE (subjs);
			if (CB_TREE_TAG (c1) == CB_TAG_BINARY_OP) {
				if (binary_op_is_relational (CB_BINARY_OP (c1))) {
					need_dec = 0;		/* Skip trying to optimize this one */
					break;
				}
				need_dec++;
			} else if (CB_TREE_TAG (c1) == CB_TAG_INTRINSIC) {
				need_dec++;
			}
		}
		if ((need_dec + current_program->decimal_index) > (COB_MAX_DEC_STRUCT - 4))
			need_dec = 0;		/* Too many needed so skip this */

		whens = CB_VALUE (case_list);
		whens = CB_CHAIN (whens);
		/* For each WHEN sequence */
		for (whens = CB_VALUE (whens); whens && need_dec > 0; whens = CB_CHAIN (whens)) {
			cb_tree		objs;
			for (objs = CB_VALUE (whens); objs && need_dec > 0; objs = CB_CHAIN (objs)) {
				if (objs == cb_any
				 || objs == cb_true
				 || objs == cb_false) {
					need_dec = 0;
					break;
				} 
				c1 = CB_VALUE (objs);
				if (CB_PAIR_P (c1)) {
					c1 = CB_PAIR_X (c1);
				}
				if (c1 == cb_any
				 || c1 == cb_true
				 || c1 == cb_false) {
					need_dec = 0;
					break;
				} 
				if (CB_LITERAL_P (c1)
				 && !CB_NUMERIC_LITERAL_P (c1)) {	/* Non-Numeric literal so skip */
					need_dec = 0;
				}
			}
		}
	}
	need_dec = 0;
	if (need_dec > 0) {
		for (subjs = subject_list; subjs; subjs = CB_CHAIN (subjs)) {
			c1 = CB_VALUE (subjs);
			if (CB_TREE_TAG (c1) == CB_TAG_BINARY_OP) {
				if (cb_is_integer_expr (c1)) {
					cb_tree		temp;
					struct cb_field	*f;
					temp = cb_build_index (cb_build_filler (), NULL, 0, NULL);
					temp->source_line = c1->source_line;
					f = CB_FIELD (cb_ref (temp));
					f->usage = CB_USAGE_BINARY;
					f->size = sizeof(long);
					f->count++;
					cb_emit (cb_build_comment ("Evaluate Integer Expression"));
					cb_emit (cb_build_assign (temp, c1));
					CB_VALUE (subjs) = temp;
				} else {
					used_dec++;
					d1 = decimal_alloc ();
					decimal_expand (d1, c1);
					cb_emit (cb_build_comment ("Evaluate decimal Expression"));
					cb_emit (cb_list_reverse (decimal_stack));
					decimal_stack = NULL;
					CB_VALUE (subjs) = d1;
				}
			} else if (CB_TREE_TAG (c1) == CB_TAG_INTRINSIC) {
				used_dec++;
				d1 = decimal_alloc ();
				decimal_expand (d1, c1);
				cb_emit (cb_build_comment ("Evaluate Expression"));
				cb_emit (cb_list_reverse (decimal_stack));
				decimal_stack = NULL;
				CB_VALUE (subjs) = d1;
			}
		}
	}
	build_evaluate (subject_list, case_list, x);

	/* internal label for END-EVALUATE */
	cb_emit (cb_build_comment ("End EVALUATE"));
	sprintf (sbuf, "%s%d:;", CB_PREFIX_LABEL, cb_id);
	cb_emit (cb_build_direct (cobc_parse_strdup (sbuf), 0));

	while (used_dec > 0) {
		decimal_free ();
		used_dec--;
	}

	/* TODO: drop temporary fields here */

	cb_id++;
}

/* FREE statement */

void
cb_emit_free (cb_tree vars)
{
	cb_tree		l;
	struct cb_field	*f;
	int		i;

	if (cb_validate_list (vars)) {
		return;
	}
	for (l = vars, i = 1; l; l = CB_CHAIN (l), i++) {
		if (CB_TREE_CLASS (CB_VALUE (l)) == CB_CLASS_POINTER) {
			if (CB_CAST_P (CB_VALUE (l))) {
				f = CB_FIELD_PTR (CB_CAST (CB_VALUE(l))->val);
				if (!f->flag_item_based) {
					cb_error_x (CB_TREE (current_statement),
						_("target %d of FREE is not a BASED data item"), i);
				}
				cb_emit (CB_BUILD_FUNCALL_2 ("cob_free_alloc",
					CB_BUILD_CAST_ADDRESS (CB_VALUE (l)), NULL));
			} else {
				cb_emit (CB_BUILD_FUNCALL_2 ("cob_free_alloc",
					NULL, CB_BUILD_CAST_ADDRESS (CB_VALUE (l))));
			}
		} else if (CB_REF_OR_FIELD_P (CB_VALUE (l))) {
				f = CB_FIELD_PTR (CB_VALUE (l));
				if (!f->flag_item_based) {
					cb_error_x (CB_TREE (current_statement),
						_("target %d of FREE is not a BASED data item"), i);
				}
				cb_emit (CB_BUILD_FUNCALL_2 ("cob_free_alloc",
					CB_BUILD_CAST_ADDR_OF_ADDR (CB_VALUE (l)), NULL));
		} else {
			cb_error_x (CB_TREE (current_statement),
				_("target %d of FREE must be a data pointer"), i);
		}
	}
}

/* GO TO statement */

void
cb_emit_goto (cb_tree target, cb_tree depending)
{
	if (target == cb_error_node) {
		return;
	}
	cb_check_reset ();
	if (target == NULL) {
		cb_verify (cb_goto_statement_without_name, _("GO TO without procedure-name"));
	} else if (depending) {
		/* GO TO procedure-name ...   DEPENDING ON numeric-identifier  and
		   GO TO ENTRY entry-name ... DEPENDING ON numeric-identifier */
		cb_emit_incompat_data_checks (depending);
		cb_emit (cb_build_goto (target, depending));
	} else if (CB_CHAIN (target)) {
			cb_error_x (CB_TREE (current_statement),
				    _("GO TO with multiple procedure-names"));
	} else {
		/* GO TO procedure-name   and
		   GO TO ENTRY entry-name */
		cb_emit (cb_build_goto (CB_VALUE (target), NULL));
	}
}

void
cb_emit_exit (const unsigned int goback)
{
	if (goback) {
		cb_emit (cb_build_goto (cb_int1, NULL));
	} else {
		cb_emit (cb_build_goto (NULL, NULL));
	}
}

/* IF statement */

void
cb_emit_if (cb_tree cond, cb_tree stmt1, cb_tree stmt2)
{
	cb_emit (cb_build_if (cond, stmt1, stmt2, STMT_IF));
}

/* SEARCH .. WHEN clause (internal IF statement) */

cb_tree
cb_build_if_check_break (cb_tree cond, cb_tree stmts)
{
	cb_tree		stmt_lis;

	stmt_lis = cb_check_needs_break (stmts);
	return cb_build_if (cond, stmt_lis, NULL, STMT_WHEN);
}

/* INITIALIZE statement */

void
cb_emit_initialize (cb_tree vars, cb_tree fillinit, cb_tree value,
		    cb_tree replacing, cb_tree def)
{
	cb_tree		l;
	struct cb_field		*f, *p;
	int			odo_level;
	unsigned int	no_fill_init;
	unsigned int	def_init;
	cb_tree		x;

	if (cb_validate_list (vars)) {
		return;
	}
	if (value == NULL && replacing == NULL) {
		def = cb_true;
	}
	no_fill_init = (fillinit == NULL);
	def_init = (def != NULL);
	for (l = vars; l; l = CB_CHAIN (l)) {
		x = CB_VALUE (l);
		if (CB_VALID_TREE (x)
		 && ( CB_FIELD_P (x)
		  || (CB_REFERENCE_P (x) && CB_FIELD_P (CB_REFERENCE (x)->value)))) {
			/* as expected */
		} else {
			cb_error_x (CB_TREE (current_statement), _("invalid INITIALIZE statement"));
			return;
		}

		f = CB_FIELD_PTR (x);
		odo_level = 0;
		while (f->children)
			f = f->children;
		for (p = f; p; p = p->parent) {
			if (p->depending) {
				odo_level++;
			}
			p->odo_level = odo_level;
			if (!p->parent) {
				break;
			}
		}
		if (CB_FIELD_PTR (x)->odo_level
		 && CB_REFERENCE_P (x)
		 && CB_REFERENCE (x)->subs == NULL
		 && CB_REFERENCE (x)->length == NULL) {
			/* GCOS 7: Contrary to what the documentation states,
			   PIC L fields are initialized up to length indicated
			   by DEPENDING var. */
			cb_tree		temp;
			temp = cb_build_index (cb_build_filler (), NULL, 0, NULL);
			f = CB_FIELD (cb_ref (temp));
			f->usage = CB_USAGE_LENGTH;
			f->count++;
			f->pic->have_sign = 0;	/* LENGTH is UNSIGNED */
			cb_emit (cb_build_assign (temp, cb_build_length_1 (x)));
			CB_REFERENCE (x)->length = temp;
		}
		cb_emit (cb_build_initialize (x , value, replacing,
					      def_init, STMT_INITIALIZE, no_fill_init));
	}
}

static size_t calc_reference_size (cb_tree xr)
{
	cb_tree	ref = cb_ref (xr);
	if (ref == cb_error_node) {
		return 0;
	}
	if (CB_REF_OR_FIELD_P (ref)) {
		struct cb_reference	*r = CB_REFERENCE (xr);
		if (r->offset) {
			if (r->length) {
				if (CB_LITERAL_P (r->length)) {
					return cb_get_int (r->length);
				}
			} else {
				if (CB_LITERAL_P (r->offset)) {
					return (size_t)CB_FIELD_PTR (xr)->size
						- cb_get_int (r->offset) + 1;
				}
			}
		} else {
			return CB_FIELD_PTR (xr)->size;
		}
	} else if (CB_ALPHABET_NAME_P (ref)) {
		return 256;
	}
	return 0;
}


/* INSPECT statement */

static void
validate_inspect (cb_tree x, cb_tree y, const unsigned int replacing_or_converting)
{
	size_t	size1;
	size_t	size2;

	switch (CB_TREE_TAG(x)) {
	case CB_TAG_REFERENCE:
		size1 = calc_reference_size (x);
		break;
	case CB_TAG_LITERAL:
		size1 = CB_LITERAL(x)->size;
		break;
	case CB_TAG_CONST:
		size1 = 1;
		break;
	default:
		size1 = 0;
		break;
	}
	if (size1) {
		switch (CB_TREE_TAG(y)) {
		case CB_TAG_REFERENCE:
			size2 = calc_reference_size (y);
			break;
		case CB_TAG_LITERAL:
			size2 = CB_LITERAL(y)->size;
			break;
		/* note: in case of CONST the original size is used */
		default:
			size2 = 0;
			break;
		}
		if (size2 && size1 != size2) {
			if (replacing_or_converting == 1) {
				cb_error_x (CB_TREE (current_statement),
						_("%s operands differ in size"), "REPLACING");
			} else {
				cb_error_x (CB_TREE (current_statement),
						_("%s operands differ in size"), "CONVERTING");
			}
		}
	}
}

static void
emit_invalid_target_error (const enum cb_inspect_clause clause)
{
	const char	*clause_name;

	switch (clause) {
	case TALLYING_CLAUSE:
		clause_name = "TALLYING";
		break;

	case REPLACING_CLAUSE:
		clause_name = "REPLACING";
		break;

	case CONVERTING_CLAUSE:
		clause_name = "CONVERTING";
		break;

	case TRANSFORM_STATEMENT:
		clause_name = "TRANSFORM";
		break;

	/* LCOV_EXCL_START */
	default:
		cobc_err_msg (_("unexpected clause %d"), clause);
		COBC_ABORT ();
	/* LCOV_EXCL_STOP */
	}

	cb_error_x (CB_TREE (current_statement), _("invalid target for %s"),
		    clause_name);
}

void
cb_emit_inspect (cb_tree var, cb_tree body, const enum cb_inspect_clause clause)
{
	switch (CB_TREE_TAG (var)) {
	case CB_TAG_REFERENCE:
		break;
	case CB_TAG_INTRINSIC:
		/* note: the case here and below are pre-checked by the parser already,
		         therefore we don't ever execute this */
		if (clause != TALLYING_CLAUSE) {
			goto error;
		}
		switch (CB_TREE_CATEGORY (var)) {
		case CB_CATEGORY_ALPHABETIC:
		case CB_CATEGORY_ALPHANUMERIC:
		case CB_CATEGORY_NATIONAL:
			break;
		default:
			goto error;
		}
		break;
	case CB_TAG_LITERAL:
		if (clause != TALLYING_CLAUSE) {
			goto error;
		}
		break;
	default:
		goto error;
	}
	{
		const cb_tree	replacing_flag = clause == REPLACING_CLAUSE ? cb_int1 : cb_int0;
		if (clause == CONVERTING_CLAUSE || clause == TRANSFORM_STATEMENT) {
			cb_emit (CB_BUILD_FUNCALL_1 ("cob_inspect_init_converting", var));
			cb_emit_list (body);
			/* no finish here */
		} else {
			cb_emit (CB_BUILD_FUNCALL_2 ("cob_inspect_init", var, replacing_flag));
			cb_emit_list (body);
			cb_emit (CB_BUILD_FUNCALL_0 ("cob_inspect_finish"));
		}
	}
	return;

 error:
	emit_invalid_target_error (clause);
}

void
cb_init_tallying (void)
{
	inspect_func = NULL;
	inspect_data = NULL;
}

cb_tree
cb_build_tallying_data (cb_tree x)
{
	inspect_data = x;
	return NULL;
}

cb_tree
cb_build_tallying_characters (cb_tree l)
{
	if (inspect_data == NULL) {
		cb_error_x (CB_TREE (current_statement),
			    _("data name expected before %s"), "CHARACTERS");
	}
	inspect_func = NULL;
	return cb_list_add (l, CB_BUILD_FUNCALL_1 ("cob_inspect_characters", inspect_data));
}

cb_tree
cb_build_tallying_all (void)
{
	if (inspect_data == NULL) {
		cb_error_x (CB_TREE (current_statement),
			    _("data name expected before %s"), "ALL");
	}
	inspect_func = "cob_inspect_all";
	return NULL;
}

cb_tree
cb_build_tallying_leading (void)
{
	if (inspect_data == NULL) {
		cb_error_x (CB_TREE (current_statement),
			    _("data name expected before %s"), "LEADING");
	}
	inspect_func = "cob_inspect_leading";
	return NULL;
}

cb_tree
cb_build_tallying_trailing (void)
{
	if (inspect_data == NULL) {
		cb_error_x (CB_TREE (current_statement),
			    _("data name expected before %s"), "TRAILING");
	}
	inspect_func = "cob_inspect_trailing";
	return NULL;
}

cb_tree
cb_build_tallying_value (cb_tree x, cb_tree l)
{
	if (inspect_func == NULL) {
		cb_error_x (x, _("ALL, LEADING or TRAILING expected before '%s'"), cb_name (x));
	}
	return cb_list_add (l, CB_BUILD_FUNCALL_2 (inspect_func, inspect_data, x));
}

cb_tree
cb_build_replacing_characters (cb_tree x, cb_tree l)
{
	if (CB_LITERAL_P (x) && CB_LITERAL(x)->size != 1) {
		cb_error_x (CB_TREE (current_statement),
			    _("operand has wrong size"));
	}
	return cb_list_add (l, CB_BUILD_FUNCALL_1 ("cob_inspect_characters", x));
}

cb_tree
cb_build_replacing_all (cb_tree x, cb_tree y, cb_tree l)
{
	validate_inspect (x, y, 1);
	return cb_list_add (l, CB_BUILD_FUNCALL_2 ("cob_inspect_all", y, x));
}

cb_tree
cb_build_replacing_leading (cb_tree x, cb_tree y, cb_tree l)
{
	validate_inspect (x, y, 1);
	return cb_list_add (l, CB_BUILD_FUNCALL_2 ("cob_inspect_leading", y, x));
}

cb_tree
cb_build_replacing_first (cb_tree x, cb_tree y, cb_tree l)
{
	validate_inspect (x, y, 1);
	return cb_list_add (l, CB_BUILD_FUNCALL_2 ("cob_inspect_first", y, x));
}

cb_tree
cb_build_replacing_trailing (cb_tree x, cb_tree y, cb_tree l)
{
	validate_inspect (x, y, 1);
	return cb_list_add (l, CB_BUILD_FUNCALL_2 ("cob_inspect_trailing", y, x));
}

cb_tree
cb_build_converting (cb_tree x, cb_tree y, cb_tree l)
{
	validate_inspect (x, y, 2);
	return cb_list_add (l, CB_BUILD_FUNCALL_2 ("cob_inspect_converting", x, y));
}

cb_tree
cb_build_inspect_region_start (void)
{
	return CB_LIST_INIT (CB_BUILD_FUNCALL_0 ("cob_inspect_start"));
}

/* MOVE statement */

static void
warning_destination (const enum cb_warn_opt warning_opt, cb_tree x)
{
	struct cb_field		*f;
	const char *usage;

	if (CB_REFERENCE_P(x)) {
		struct cb_reference	*r = CB_REFERENCE (x);
		if (r->offset) {
			return;
		}
		f = CB_FIELD (r->value);
		x = CB_TREE (f);
	} else if (CB_FIELD_P(x)) {
		f = CB_FIELD (x);
	} else {
		cobc_err_msg (_("call to '%s' with invalid parameter '%s'"),
			"warning_destination", "x");
		CB_TREE_TAG_UNEXPECTED_ABORT (x);
	}

#if 1  /* FIXME: this is wrong, should be removed and register building be
	      adjusted, for example ACU has RETURN-CODE as SIGNED-LONG, EXTERNAL */
	if (f->flag_internal_register) {
		usage = "BINARY-LONG";
	} else
#endif
	if (f->flag_real_binary) {
		usage = f->pic->orig;
	} else if (f->usage == CB_USAGE_FLOAT) {
		usage = "FLOAT";
	} else if (f->usage == CB_USAGE_DOUBLE) {
		usage = "DOUBLE";
	} else if (f->usage == CB_USAGE_LONG_DOUBLE) {
		usage = "FLOAT EXTENDED";
	} else if (f->usage == CB_USAGE_FP_BIN32) {
		usage = "FLOAT-BINARY-7";
	} else if (f->usage == CB_USAGE_FP_BIN64) {
		usage = "FLOAT-BINARY-16";
	} else if (f->usage == CB_USAGE_FP_BIN128) {
		usage = "FLOAT-BINARY-34";
	} else if (f->usage == CB_USAGE_FP_DEC64) {
		usage = "FLOAT-DECIMAL-16";
	} else if (f->usage == CB_USAGE_FP_DEC128) {
		usage = "FLOAT-DECIMAL-34";
	} else if (f->pic) {
		cb_note_x (warning_opt, x, _("'%s' defined here as PIC %s"),
			cb_name (x), f->pic->orig);
		return;
	} else {
		cb_note_x (warning_opt, x, _("'%s' defined here as a group of length %d"),
			cb_name (x), f->size);
		return;
	}

	if (f->flag_internal_register) {
		cb_note_x (warning_opt, x, _("internal register '%s' defined as USAGE %s"),
			f->name, usage);
	} else {
		cb_note_x (warning_opt, x, _("'%s' defined here as USAGE %s"),
			f->name, usage);
	}
}

static void
move_warning (cb_tree src, cb_tree dst, const unsigned int value_flag,
	      const enum cb_warn_opt warning_opt, const int src_flag, const char *msg)
{
	cb_tree		loc;

	if (suppress_warn) {
		return;
	}
	if (cobc_cs_check == CB_CS_SET || !src->source_line) {
		loc = dst;
	} else {
		loc = src;
	}
	if (value_flag) {
		/* VALUE clause --> always warn */
		cb_warning_x (COBC_WARN_FILLER, loc, "%s", msg);
		if (CB_LITERAL_P (src) && src_flag > 0) {
			/* looks like the message above is always "value size exceeds data size"
			   when src_flag is > 0 but that is not guaranteed, consider refactoring */
			cb_note_x (COBC_WARN_FILLER, loc, _("value size is %d"), src_flag);
		}
	} else {
		/* MOVE or SET statement */
		if (get_warn_opt_value (warning_opt) != COBC_WARN_DISABLED) {
			cb_warning_x (warning_opt, loc, "%s", msg);
			if (src_flag) {
				/* note: src_flag is -1 for numeric literals,
				   contains literal size otherwise */
				if (!CB_LITERAL_P (src)) {
					warning_destination (warning_opt, src);
				} else if (src_flag == -1) {
					if (CB_LITERAL_P (src)) {
						if (CB_LITERAL (src)->size < 40) {
							char numval[48];
							int  p, k = 0;
							if (CB_LITERAL (src)->sign == -1)
								numval[k++] = '-';
							strcpy(&numval[k],(void*)(CB_LITERAL (src)->data));
							if (CB_LITERAL (src)->scale > 0) {
								p = CB_LITERAL (src)->size - CB_LITERAL (src)->scale;
								numval[k + p] = '.';
								strcpy(&numval[k+p+1],(void*)(CB_LITERAL (src)->data+p));
							}
							cb_note_x (warning_opt, dst, 
									_("value is %s"), numval);
						} else {
							cb_note_x (warning_opt, dst, 
									_("value is %s"), CB_LITERAL (src)->data);
						}
					}
				} else {
					cb_note_x (warning_opt, dst,
						_("value size is %d"), src_flag);
				}
			}
			warning_destination (warning_opt, dst);
		}
	}

	return;
}

/* Count number of free places in an edited field;
   note that PICTURE is pre-validated so national fields
   won't include A + X, alhanumeric won't include N */
static int
count_pic_edited (struct cb_field *field)
{
	cob_pic_symbol	*s;
	int		count = 0;

	for (s = field->pic->str; s->symbol != '\0'; ++s) {
		const char sym = s->symbol;
		if (sym == '9' || sym == 'A' || sym == 'X' || sym == 'N') {
			count += s->times_repeated;
		}
	}
	return count;
}

/* check if data of two fields may overlap;
  returns:
	0 = no overlapping
	1 = possible overlapping, would need more checks for a warning
	2 = possible overlapping, warn
	3 = overlapping, warn

  src_f, dst_f
	fields to be checked
  src, dst
	references, may be NULL (no subscripts/references checked)

*/
static size_t
cb_check_overlapping (struct cb_field *src_f, struct cb_field *dst_f,
	cb_tree src, cb_tree dst)
{
	struct cb_field	*f1;
	struct cb_field	*ff1;
	struct cb_field	*ff2;
	struct cb_reference *sr;
	struct cb_reference *dr;
	int		src_size;
	int		dst_size;
	int		src_off;
	int		dst_off;

	if (CB_REFERENCE_P(src)) {
		sr = CB_REFERENCE (src);
	} else {
		sr = NULL;
	}

	if (CB_REFERENCE_P(dst)) {
		dr = CB_REFERENCE (dst);
	} else {
		dr = NULL;
	}

	/* Check for identical field */
	if (src_f == dst_f) {
		if (!sr || !dr) {
			/* same fields, no information about sub/refmod,
			   overlapping possible */
			return 1;
		}
		if (sr->subs) {
			/* same fields with subs, overlapping possible */
#if 0		/* FIXME: more checks needed:
			   1: are all subs of source and dest identical ?
			   2: are all subs of source and dest literals with the same integer value ?
			*/
			if (...) {
				return 2;
			} else {
				return 0;
			}
#else
			/* for now: at least resolve one sub and handle when both reference a literal
			   or a reference ...*/
			if (!CB_CHAIN (sr->subs)
			 && !CB_CHAIN (dr->subs)) {
				if (CB_NUMERIC_LITERAL_P(CB_VALUE (sr->subs))
				 && CB_NUMERIC_LITERAL_P(CB_VALUE (dr->subs))) {
					struct cb_literal *sl, *dl;

					sl = CB_LITERAL(CB_VALUE (sr->subs));
					dl = CB_LITERAL(CB_VALUE (dr->subs));
					if (atoll((const char*)sl->data) !=
						atoll((const char*)dl->data)) {
						return 0;
					}
				} else if (CB_REFERENCE_P(CB_VALUE (sr->subs))
				 && CB_REFERENCE_P(CB_VALUE (dr->subs))) {
					struct cb_reference *tsr, *tdr;

					tsr = CB_REFERENCE(CB_VALUE (sr->subs));
					tdr = CB_REFERENCE(CB_VALUE (dr->subs));
					if (tsr->subs || tdr->subs) {
						return 1;
					} else {
						if (tsr->value != tdr->value) {
							return 1;
						}
					}
				} else {
					return 1;
				}
			} else {
				return 1;
			}
#endif
		}

		/* same fields, at least one without ref-mod -> overlapping */
		if (!sr->offset || !dr->offset) {
			return 3;
		}

	} else {

		/* Check basic overlapping */
		for (f1 = src_f->children; f1; f1 = f1->sister) {
			if (f1 == dst_f) {
				return 3;
			}
		}
		for (f1 = dst_f->children; f1; f1 = f1->sister) {
			if (f1 == src_f) {
				return 3;
			}
		}

		/* Check for same parent field */
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 6011)  /* cb_field_founder always returns a valid pointer */
#endif
		ff1 = cb_field_founder (src_f);
		ff2 = cb_field_founder (dst_f);
		if (ff1->redefines) {
			ff1 = ff1->redefines;
		}
		if (ff2->redefines) {
			ff2 = ff2->redefines;
		}
		if (ff1 != ff2) {
			/* different field founder -> no overlapping */
			/* if at least one of the vars can have an assignment
			   of a different address we must return 1 */
			if (ff1->flag_local_storage || ff1->flag_item_based ||
				ff2->flag_local_storage || ff2->flag_item_based) {
				return 1;
			} else {
				return 0;
			}
		}
	}
#ifdef _MSC_VER
#pragma warning(pop)
#endif

	/* check if both fields are references, otherwise we can't check further */
	if (!sr || !dr) {
		/* overlapping possible as they have the same field founder */
		return 1;
	}

	src_off = src_f->offset;
	dst_off = dst_f->offset;

	/* Check for occurs */
	if (src_f != dst_f && (sr->subs || dr->subs)) {
		/* overlapping possible */
#if 0	/* FIXME: more checks needed:
		1: if all subs are integer literals: a full offset check of both fields
		2: if at least one isn't an integer literal: check that all "upper" literals
		   are either identical or numeric literals with the same integer value */
		if (...) {
			return 2;
		} else {
			return 0;
		}
#else
		return 1;
#endif
	}

	src_size = cb_field_size (src);
	dst_size = cb_field_size (dst);

	/* Adjusting offsets by reference modification */
	if (sr->offset) {
		if (src_size == FIELD_SIZE_UNKNOWN ||
			!CB_LITERAL_P (sr->offset)) {
			return 2;
		}
		src_off += cb_get_int (sr->offset) - 1;
	}
	if (dr->offset) {
		if (dst_size == FIELD_SIZE_UNKNOWN ||
			!CB_LITERAL_P (dr->offset)) {
			return 2;
		}
		dst_off += cb_get_int (dr->offset) - 1;
	}

	if (src_size == 0 || dst_size == 0
	 || cb_field_variable_size (src_f)
	 || cb_field_variable_size (dst_f)) {
		/* overlapping possible, would need more checks */
		return 1;
	}

	if (src_off >= dst_off && src_off < (dst_off + dst_size)) {
		return 3;
	}
	if (src_off < dst_off && (src_off + src_size) > dst_off) {
		return 3;
	}
	return 0;
}

static int
is_floating_point_usage (const enum cb_usage usage)
{
	return usage == CB_USAGE_DOUBLE
		|| usage == CB_USAGE_FLOAT
		|| usage == CB_USAGE_LONG_DOUBLE
		|| usage == CB_USAGE_FP_BIN32
		|| usage == CB_USAGE_FP_BIN64
		|| usage == CB_USAGE_FP_BIN128
		|| usage == CB_USAGE_FP_DEC64
		|| usage == CB_USAGE_FP_DEC128;
}

enum move_outcome {
	MOVE_OK,
	MOVE_INVALID,
	MOVE_INVALID_NO_MESSAGE,
	MOVE_SUBSTITUTING_ZERO,
	MOVE_NUMERIC_LIT_OVERFLOW,
	MOVE_NON_INTEGER_TO_ALNUM,
	MOVE_NUMERIC_EXPECTED,
	MOVE_ALNUM_EXPECTED,
	MOVE_NATIONAL_EXPECTED,
	MOVE_VALUE_NOT_FIT_PIC,
	MOVE_GENERAL_OVERFLOW,
	MOVE_GENERAL_POSSIBLE_TRUNCATION,
	MOVE_NUMERIC_POSSIBLE_TRUNCATION
};

static enum move_outcome
validate_move_from_const (cb_tree src, cb_tree dst, const unsigned int is_value)
{
	cb_tree			loc = src->source_line ? src : dst;

	if (src == cb_space || src == cb_low || src == cb_high || src == cb_quote) {
		if ( CB_TREE_CATEGORY (dst) == CB_CATEGORY_NUMERIC
		 || (CB_TREE_CATEGORY (dst) == CB_CATEGORY_NUMERIC_EDITED && !is_value)
		 || (CB_TREE_CATEGORY (dst) == CB_CATEGORY_FLOATING_EDITED && !is_value)) {
			if ((current_statement && current_statement->statement == STMT_SET)
			   || cobc_cs_check == CB_CS_SET) {
				return MOVE_INVALID;
			}
		}
	}

	if (src == cb_space) {	/* error because SPACE is category alphabetic */
		if (CB_TREE_CATEGORY (dst) == CB_CATEGORY_NUMERIC
		    || (CB_TREE_CATEGORY (dst) == CB_CATEGORY_NUMERIC_EDITED && !is_value)
		    || (CB_TREE_CATEGORY (dst) == CB_CATEGORY_FLOATING_EDITED && !is_value)) {
			/* note: ACUCOBOL and MF allow this, but not for NUMERIC + VALUE */
			if (is_value) {
				return MOVE_INVALID;
			}
			if (cb_verify_x (loc, cb_move_fig_space_to_numeric,
					 _("MOVE of figurative constant SPACE to numeric item"))) {
				if (cb_move_nonnumlit_to_numeric_is_zero) {
					return MOVE_SUBSTITUTING_ZERO;
				}
				return MOVE_OK;
			}
			return MOVE_INVALID_NO_MESSAGE; /* error message raised already*/
		}
	} else if (src == cb_zero) {
		if (CB_TREE_CATEGORY (dst) == CB_CATEGORY_ALPHABETIC) {
			return MOVE_INVALID;
		}
	} else if (src == cb_quote) {	/* remark: no error because QUOTE is category alphanumeric */
		if (CB_TREE_CATEGORY (dst) == CB_CATEGORY_NUMERIC) {
			if (!cb_verify_x (loc, cb_move_fig_quote_to_numeric,
					  _("MOVE of figurative constant QUOTE to numeric item"))) {
				return MOVE_INVALID_NO_MESSAGE;
			}
			if (cb_move_fig_quote_to_numeric != cb_move_fig_constant_to_numeric) {
				if (!cb_verify_x (loc, cb_move_fig_constant_to_numeric,
						  _("MOVE of figurative constant to numeric item"))) {
					return MOVE_INVALID_NO_MESSAGE;
				}
			}
			if (cb_move_nonnumlit_to_numeric_is_zero) {
				return MOVE_SUBSTITUTING_ZERO;
			}
		}
	} else if (src == cb_low || src == cb_high) {
		if (CB_TREE_CATEGORY (dst) == CB_CATEGORY_NUMERIC
		    || (CB_TREE_CATEGORY (dst) == CB_CATEGORY_NUMERIC_EDITED && !is_value)) {
			if (!cb_verify_x (loc, cb_move_fig_constant_to_numeric,
					  _("MOVE of figurative constant to numeric item"))) {
				return MOVE_INVALID_NO_MESSAGE;
			}
			if (cb_move_nonnumlit_to_numeric_is_zero) {
				return MOVE_SUBSTITUTING_ZERO;
			}
		}
	}

	return MOVE_OK;
}


static enum move_outcome
validate_move_from_num_lit (cb_tree src, cb_tree dst, const unsigned int is_value,
			    int * const size)
{
	struct cb_field		*fdst = CB_FIELD_PTR (dst);
	struct cb_literal	*l = CB_LITERAL (src);
	int			leftmost_significant, most_significant, least_significant;
	size_t			i;
	cob_s64_t		val;
	cb_tree			loc = src->source_line ? src : dst;

	/* Numeric literal */
	if (l->all) {
		return MOVE_INVALID;
	}
	if (is_floating_point_usage (fdst->usage)) {
		/* TODO: add check for exponent size */
		return MOVE_OK;
	}

	/* Compute the most significant figure place
	   in relatation to the decimal point (negative = decimal position) */
	for (leftmost_significant = 0; leftmost_significant < l->size; leftmost_significant++) {
		if (l->data[leftmost_significant] != '0') {
			break;
		}
	}
	if (leftmost_significant == l->size) {
		most_significant = -999;
	} else {
		most_significant = l->size - l->scale - leftmost_significant;
		if (most_significant < 1) most_significant--;
	}

	/* Compute the least significant figure place
	   in relatation to the decimal point (negative = decimal position) */
	for (i = l->size - 1; i != 0; i--) {
		if (l->data[i] != '0') {
			break;
		}
	}
	if (i == 0) {
		least_significant = 999;
	} else {
		least_significant = (l->size - l->scale) - i;
		if (least_significant < 1) least_significant--;
	}

	/* Value check */
	switch (CB_TREE_CATEGORY (dst)) {
	case CB_CATEGORY_ALPHANUMERIC:
	case CB_CATEGORY_ALPHANUMERIC_EDITED:
		if (fdst->usage == CB_USAGE_COMP_X) {
			break;
		}
		if (is_value
		 || l->scale == 0) {
			return MOVE_ALNUM_EXPECTED;
		}
		return MOVE_INVALID;

	case CB_CATEGORY_NATIONAL:
	case CB_CATEGORY_NATIONAL_EDITED:
		if (is_value
		 || l->scale == 0) {
			return MOVE_NATIONAL_EXPECTED;
		}
		return MOVE_NON_INTEGER_TO_ALNUM;

	case CB_CATEGORY_NUMERIC_EDITED:
	case CB_CATEGORY_FLOATING_EDITED:
		if (is_value) {
			cb_verify_x (loc, cb_numeric_value_for_edited_item,
				     _("numeric literal in VALUE clause of numeric-edited item"));
		}
		/* Fall-through */
	case CB_CATEGORY_NUMERIC:
	{
		const struct cb_picture *pic = fdst->pic;
		if (pic->scale < 0) {
			/* Check for PIC 9(n)P(m) */
			if (least_significant <= -pic->scale) {
				/* 12300 (3) <= 99PPP (- -3) */
				return MOVE_VALUE_NOT_FIT_PIC;
			}
		} else if (pic->scale > pic->digits) {
			/* Check for PIC P(n)9(m) */
			if (most_significant > pic->digits - pic->scale - 1) {
				/* .00123 (-3) > PPP99 (-4 [2 - 5 - 1]) */
				return MOVE_VALUE_NOT_FIT_PIC;
			}
		}
		break;
	}
	case CB_CATEGORY_ALPHABETIC:
		if (is_value) {
			return MOVE_ALNUM_EXPECTED;
		}
		/* Coming from codegen */
		if (!suppress_warn) {
			return MOVE_INVALID;
		}
		cb_warning_x (cb_warn_additional, loc,
			      _("numeric move to ALPHABETIC"));
		break;
	default:
		if (is_value) {
			return MOVE_ALNUM_EXPECTED;
		}
		return MOVE_INVALID;
	}

	/* Sign check */
	if (l->sign != 0 && !fdst->pic->have_sign) {
		if (is_value) {
			cb_error_x (loc, _("data item not signed"));
			return MOVE_INVALID_NO_MESSAGE;
		}
		cb_warning_x (cb_warn_truncate, loc, _("ignoring sign"));
	}

	/* Size check */
	if (fdst->flag_real_binary
	    || (  !cb_binary_truncate
		  && fdst->pic->scale <= 0
		  && (  fdst->usage == CB_USAGE_COMP_5
			 || fdst->usage == CB_USAGE_COMP_X
			 || fdst->usage == CB_USAGE_COMP_N
			 || fdst->usage == CB_USAGE_BINARY))) {

		i = l->size - leftmost_significant;
		if (i <= 19) {
			val = cb_get_long_long (src);
		} else if (fdst->size < 8) {
			return MOVE_NUMERIC_LIT_OVERFLOW;
		} else {
			val = 0;
		}
		/* handle negative scale aka 999PPP */
		if (fdst->pic->scale < 0) {
			int j = fdst->pic->scale;
			i += j;
			while (j != 0) {
				val /= 10;
				j++;
			}
		}

		switch (fdst->size) {
		case 1:
			if (fdst->pic->have_sign) {
				if (val < COB_S64_C (-128)
				 || val > COB_S64_C (127)) {
					return MOVE_NUMERIC_LIT_OVERFLOW;
				}
			} else {
				if (val > COB_S64_C (255)) {
					return MOVE_NUMERIC_LIT_OVERFLOW;
				}
			}
			break;
		case 2:
			if (fdst->pic->have_sign) {
				if (val < COB_S64_C (-32768) ||
				    val > COB_S64_C (32767)) {
					return MOVE_NUMERIC_LIT_OVERFLOW;
				}
			} else {
				if (val > COB_S64_C (65535)) {
					return MOVE_NUMERIC_LIT_OVERFLOW;
				}
			}
			break;
		case 3:
			if (fdst->pic->have_sign) {
				if (val < COB_S64_C (-8388608)
				 || val > COB_S64_C (8388607)) {
					return MOVE_NUMERIC_LIT_OVERFLOW;
				}
			} else {
				if (val > COB_S64_C (16777215)) {
					return MOVE_NUMERIC_LIT_OVERFLOW;
				}
			}
			break;
		case 4:
			if (fdst->pic->have_sign) {
				if (val < COB_S64_C (-2147483648)
				 || val > COB_S64_C (2147483647)) {
					return MOVE_NUMERIC_LIT_OVERFLOW;
				}
			} else {
				if (val > COB_S64_C (4294967295)) {
					return MOVE_NUMERIC_LIT_OVERFLOW;
				}
			}
			break;
		case 5:
			if (fdst->pic->have_sign) {
				if (val < COB_S64_C (-549755813888)
				 || val > COB_S64_C (549755813887)) {
					return MOVE_NUMERIC_LIT_OVERFLOW;
				}
			} else {
				if (val > COB_S64_C (1099511627775)) {
					return MOVE_NUMERIC_LIT_OVERFLOW;
				}
			}
			break;
		case 6:
			if (fdst->pic->have_sign) {
				if (val < COB_S64_C (-140737488355328)
				 || val > COB_S64_C (140737488355327)) {
					return MOVE_NUMERIC_LIT_OVERFLOW;
				}
			} else {
				if (val > COB_S64_C (281474976710655)) {
					return MOVE_NUMERIC_LIT_OVERFLOW;
				}
			}
			break;
		case 7:
			if (fdst->pic->have_sign) {
				if (val < COB_S64_C (-36028797018963968)
				 || val > COB_S64_C (36028797018963967)) {
					return MOVE_NUMERIC_LIT_OVERFLOW;
				}
			} else {
				if (val > COB_S64_C (72057594037927935)) {
					return MOVE_NUMERIC_LIT_OVERFLOW;
				}
			}
			break;
		default:
			if (fdst->pic->have_sign) {
				if (i <= cb_max_binary) {
					break;
				}
				if (i > cb_max_binary) {
					return MOVE_NUMERIC_LIT_OVERFLOW;
				}
				if (i == 19
				 && memcmp (l->data + leftmost_significant,
					    l->sign ? "9223372036854775808" :
						      "9223372036854775807",
					    19) > 0) {
					return MOVE_NUMERIC_LIT_OVERFLOW;
				}
			} else {
				if (i <= cb_max_binary) {
					break;
				}
				if (i > cb_max_binary) {
					return MOVE_NUMERIC_LIT_OVERFLOW;
				}
				if (i == 20
				 && memcmp (l->data + leftmost_significant,
					    "18446744073709551615",
					    20) > 0) {
					return MOVE_NUMERIC_LIT_OVERFLOW;
				}
			}
			break;
		}
		return MOVE_OK;
	}
	if (least_significant < -fdst->pic->scale) {
		*size = -1;
		return MOVE_GENERAL_OVERFLOW;
	}
	if (fdst->pic->scale > 0) {
		*size = fdst->pic->digits - fdst->pic->scale;
	} else {
		*size = fdst->pic->digits;
	}
	if (most_significant > *size) {
		*size = -1;
		return MOVE_GENERAL_OVERFLOW;
	}

	return MOVE_OK;
}

static enum move_outcome
validate_move_from_national_lit (cb_tree src, cb_tree dst, const unsigned int is_value,
				 int * const size)
{
	struct cb_field		*fdst = CB_FIELD_PTR (dst);
	struct cb_literal	*l = CB_LITERAL (src);
	size_t			i;

	if (l->size % COB_NATIONAL_SIZE != 0) {
		return MOVE_INVALID;
	}

	/* Value check */
	switch (CB_TREE_CATEGORY (dst)) {
	case CB_CATEGORY_NATIONAL:
		break;
	case CB_CATEGORY_NUMERIC:
		if (is_value) {
			return MOVE_NUMERIC_EXPECTED;
		} else {
			for (i = 0; i < l->size; i++) {
				if (l->data[i++] != 0x00) {
					return MOVE_NUMERIC_EXPECTED;
				}
				if (!isdigit (l->data[i])) {
					return MOVE_NUMERIC_EXPECTED;
				}
			}
		}
		break;
	case CB_CATEGORY_NUMERIC_EDITED:
		if (!is_value) {
			for (i = 0; i < l->size; i++) {
				if (l->data[i++] != 0x00) {
					return MOVE_NUMERIC_EXPECTED;
				}
				if (!isdigit (l->data[i])
				 && l->data[i] != '.'
				 && l->data[i] != ','
				 && l->data[i] != '+'
				 && l->data[i] != '-'
				 && l->data[i] != ' ') {
					return MOVE_NUMERIC_EXPECTED;
				}
			}
		} else {
			/* TODO: validate the value for VALUE - needed? */
		}
		break;
	default:
		return MOVE_INVALID;
	}

	/* Size check */
	*size = cb_field_size (dst);
	if (CB_TREE_CATEGORY (dst) == CB_CATEGORY_NATIONAL) {
		*size /= COB_NATIONAL_SIZE;
	}
	if (*size > 0
	 && l->size > 0
	 && !fdst->flag_any_length) {
		/* check the real size */
		fdst = CB_FIELD_PTR (dst);
		if (fdst->flag_justified) {
			/* right justified: trim left */
			for (i = 0; i != l->size; i += 2) {
				if (l->data[i] != 0x00
				 || l->data[i + 1] != ' ') {
					break;
				}
			}
			i = l->size - i;
		} else {
			/* normal field: trim right */
			for (i = l->size - 1; i != 0; i -= 2) {
				if (l->data[i] != ' '
				 || l->data[i - 1] != 0x00) {
					break;
				}
			}
			i++;
		}
		i /= COB_NATIONAL_SIZE;
		if ((int)i > *size) {
			*size = (signed int)i;
			return MOVE_GENERAL_OVERFLOW;
		}
		/* for VALUE: additional check without trim */
		if (is_value && (int)(l->size / COB_NATIONAL_SIZE) > *size) {
			return MOVE_VALUE_NOT_FIT_PIC;
		}
	}
	return MOVE_OK;
}

static enum move_outcome
validate_move_from_alnum_lit (cb_tree src, cb_tree dst, const unsigned int is_value,
			      int * const size)
{
	struct cb_field		*fdst = CB_FIELD_PTR (dst);
	struct cb_literal	*l = CB_LITERAL (src);
	size_t			i;

	/* Value check */
	switch (CB_TREE_CATEGORY (dst)) {
	case CB_CATEGORY_ALPHABETIC:
		for (i = 0; i < l->size; i++) {
			if (!isalpha (l->data[i]) &&
			    l->data[i] != ' ') {
				return MOVE_VALUE_NOT_FIT_PIC;
			}
		}
		break;
	case CB_CATEGORY_NUMERIC:
		/* TODO: add check (maybe a configuration)
		   for numeric data in alphanumeric literal
		   note - we did this in versions before 3.0 */
		if (is_value) {
			return MOVE_NUMERIC_EXPECTED;
		} else {
			for (i = 0; i < l->size; i++) {
				if (!isdigit (l->data[i])) {
					/* no check for +-,. as MF seems to not do this here */
					if (cb_move_nonnumlit_to_numeric_is_zero) {
						return MOVE_SUBSTITUTING_ZERO;
					}
					return MOVE_NUMERIC_EXPECTED;
				}
			}
		}
		break;

	case CB_CATEGORY_NUMERIC_EDITED:
		/* TODO: add check (maybe a configuration)
		   for numeric data in alphanumeric literal
		   note - we did this in versions before 3.0 */
		if (!is_value) {
			/* TODO check if the following is correct: */
			/* validate the value for normal MOVE as MF does*/
			for (i = 0; i < l->size; i++) {
				if (!isdigit (l->data[i])
				    && l->data[i] != '.'
				    && l->data[i] != ','
				    && l->data[i] != '+'
				    && l->data[i] != '-'
				    && l->data[i] != ' ') {
					if (cb_move_nonnumlit_to_numeric_is_zero) {
						return MOVE_SUBSTITUTING_ZERO;
					}
					return MOVE_NUMERIC_EXPECTED;
				}
			}
		} else {
			/* TODO: validate the value for VALUE - needed? */
		}
		break;
	case CB_CATEGORY_FLOATING_EDITED:
		if (!is_value) {
			/* TODO check if the following is correct: */
			/* validate the value for normal MOVE as MF does*/
			for (i = 0; i < l->size; i++) {
				if (!isdigit (l->data[i])
				    && l->data[i] != '.'
				    && l->data[i] != ','
				    && l->data[i] != '+'
				    && l->data[i] != '-'
				    && l->data[i] != 'E'
				    && l->data[i] != ' ') {
					if (cb_move_nonnumlit_to_numeric_is_zero) {
						return MOVE_SUBSTITUTING_ZERO;
					}
					return MOVE_NUMERIC_EXPECTED;
				}
			}
		} else {
			/* TODO: validate the value for VALUE - needed? */
		}
		break;
	default:
		break;
	}

	/* Size check */
	*size = cb_field_size (dst);
	if (CB_TREE_CATEGORY (dst) == CB_CATEGORY_NATIONAL) {
		*size /= COB_NATIONAL_SIZE;
	}
	if (*size > 0
	 && l->size > 0
	 && !fdst->flag_any_length) {
		/* check the real size */
		if (fdst->flag_justified) {
			/* right justified: trim left */
			for (i = 0; i != l->size; i++) {
				if (l->data[i] != ' ') {
					break;
				}
			}
			i = l->size - i;
		} else {
			/* normal field: trim right */
			for (i = l->size - 1; i != 0; i--) {
				if (l->data[i] != ' ') {
					break;
				}
			}
			i++;
		}
		if ((int)i > *size) {
			*size = (signed int)i;
			return MOVE_GENERAL_OVERFLOW;
		}
		/* for VALUE: additional check without trim */
		if (is_value && (int)l->size > *size) {
			return MOVE_VALUE_NOT_FIT_PIC;
		}
	}
	if (is_value
	 && CB_TREE_CATEGORY (dst) == CB_CATEGORY_NATIONAL) {
		return MOVE_NATIONAL_EXPECTED;
	}

	return MOVE_OK;
}

static enum move_outcome
validate_move_from_literal (cb_tree src, cb_tree dst, const unsigned int is_value,
			    int * const size)
{
	if (CB_TREE_CLASS (src) == CB_CLASS_NUMERIC) {
		return validate_move_from_num_lit (src, dst, is_value, size);
	} else if (CB_TREE_CLASS (src) == CB_CLASS_NATIONAL) {
		return validate_move_from_national_lit (src, dst, is_value, size);
	} else {
		return validate_move_from_alnum_lit (src, dst, is_value, size);
	}
}

static void
warn_overlapping_move (cb_tree loc)
{
	switch (overlapping) {
	case 0:
	case 1:
		break;
	case 2:
		if (!suppress_warn) {
			cb_warning_x(cb_warn_pos_overlap, loc,
				_("overlapping MOVE may occur and produce unpredictable results"));
		}
		break;
	case 3:
		if (!suppress_warn) {
			cb_warning_x (cb_warn_overlap, loc,
				_("overlapping MOVE may produce unpredictable results"));
		}
		break;
		/* LCOV_EXCL_START */
	default:
		cobc_err_msg ("unexpected overlap result: %d", (int)overlapping);
		COBC_ABORT();
		/* LCOV_EXCL_STOP */
	}
}

static enum move_outcome
validate_non_elem_move_from_field_or_ref (int src_size, int dst_size)
{
	if (dst_size == FIELD_SIZE_UNKNOWN) {
		return MOVE_OK;
	}
	if (src_size > dst_size) {
		return MOVE_GENERAL_POSSIBLE_TRUNCATION;
	}
	return MOVE_OK;
}

static enum move_outcome
validate_elem_move_from_field_or_ref (cb_tree src, cb_tree dst)
{
	struct cb_field		*fdst = CB_FIELD_PTR (dst);
	struct cb_field		*fsrc = CB_FIELD_PTR (src);
	int			src_size = cb_field_size (src);
	int			dst_size = cb_field_size (dst);
	int			is_numeric_edited = 0;
	cb_tree			loc = src->source_line ? src : dst;
	int			src_scale_mod;
	int			dst_scale_mod;

	switch (CB_TREE_CATEGORY (src)) {
	case CB_CATEGORY_ALPHANUMERIC:
		switch (CB_TREE_CATEGORY (dst)) {
		case CB_CATEGORY_NUMERIC:
		case CB_CATEGORY_NUMERIC_EDITED:
			if (src_size > (int)fdst->pic->digits) {
				return MOVE_NUMERIC_POSSIBLE_TRUNCATION;
			}
			break;
		case CB_CATEGORY_ALPHANUMERIC_EDITED:
		case CB_CATEGORY_NATIONAL_EDITED:
		case CB_CATEGORY_FLOATING_EDITED:
			if (dst_size == FIELD_SIZE_UNKNOWN) {
				break;
			}
			if (src_size > count_pic_edited (fdst)) {
				return MOVE_GENERAL_POSSIBLE_TRUNCATION;
			}
			break;
		case CB_CATEGORY_NATIONAL:
			if (dst_size == FIELD_SIZE_UNKNOWN) {
				break;
			}
			if (src_size > fdst->size / COB_NATIONAL_SIZE) {
				return MOVE_GENERAL_POSSIBLE_TRUNCATION;
			}
			break;
		default:
			if (dst_size == FIELD_SIZE_UNKNOWN) {
				break;
			}
			if (src_size > fdst->size) {
				return MOVE_GENERAL_POSSIBLE_TRUNCATION;
			}
			break;
		}
		break;

	case CB_CATEGORY_NATIONAL:
		switch (CB_TREE_CATEGORY (dst)) {
		case CB_CATEGORY_NUMERIC:
		case CB_CATEGORY_NUMERIC_EDITED:
			if (src_size > (int)fdst->pic->digits) {
				return MOVE_NUMERIC_POSSIBLE_TRUNCATION;
			}
			break;
		case CB_CATEGORY_NATIONAL:
			if (dst_size == FIELD_SIZE_UNKNOWN) {
				break;
			}
			if (src_size > fdst->size / COB_NATIONAL_SIZE) {
				return MOVE_GENERAL_POSSIBLE_TRUNCATION;
			}
			break;
		case CB_CATEGORY_NATIONAL_EDITED:
			if (dst_size == FIELD_SIZE_UNKNOWN) {
				break;
			}
			if (src_size > count_pic_edited (fdst)) {
				return MOVE_GENERAL_POSSIBLE_TRUNCATION;
			}
			break;
		case CB_CATEGORY_BOOLEAN:
			/* TODO: add checks */
			break;
		default:
			return MOVE_INVALID;
		}
		break;

	case CB_CATEGORY_NATIONAL_EDITED:
		switch (CB_TREE_CATEGORY (dst)) {
		case CB_CATEGORY_NATIONAL:
		case CB_CATEGORY_NATIONAL_EDITED:
			if (dst_size == FIELD_SIZE_UNKNOWN) {
				break;
			}
			if (src_size > fdst->size / COB_NATIONAL_SIZE) {
				return MOVE_GENERAL_POSSIBLE_TRUNCATION;
			}
			break;
		default:
			return MOVE_INVALID;
		}
		break;

	case CB_CATEGORY_ALPHABETIC:
	case CB_CATEGORY_ALPHANUMERIC_EDITED:
		switch (CB_TREE_CATEGORY (dst)) {
		case CB_CATEGORY_NUMERIC:
		case CB_CATEGORY_NUMERIC_EDITED:
		case CB_CATEGORY_FLOATING_EDITED:
			return MOVE_INVALID;
		case CB_CATEGORY_ALPHANUMERIC_EDITED:
		case CB_CATEGORY_NATIONAL_EDITED:
			if (dst_size == FIELD_SIZE_UNKNOWN) {
				break;
			}
			if (src_size > count_pic_edited(fdst)) {
				return MOVE_GENERAL_POSSIBLE_TRUNCATION;
			}
			break;
		case CB_CATEGORY_NATIONAL:
			if (dst_size == FIELD_SIZE_UNKNOWN) {
				break;
			}
			if (src_size > fdst->size / COB_NATIONAL_SIZE) {
				return MOVE_GENERAL_POSSIBLE_TRUNCATION;
			}
			break;
		default:
			if (dst_size == FIELD_SIZE_UNKNOWN) {
				break;
			}
			if (src_size > fdst->size) {
				return MOVE_GENERAL_POSSIBLE_TRUNCATION;
			}
			break;
		}
		break;

	case CB_CATEGORY_NUMERIC:
	case CB_CATEGORY_NUMERIC_EDITED:
	case CB_CATEGORY_FLOATING_EDITED:
		switch (CB_TREE_CATEGORY (dst)) {
		case CB_CATEGORY_ALPHABETIC:
			return MOVE_INVALID;
		case CB_CATEGORY_ALPHANUMERIC_EDITED:
		case CB_CATEGORY_NATIONAL_EDITED:
			is_numeric_edited = 1;
			/* Fall through */
		case CB_CATEGORY_ALPHANUMERIC:
		case CB_CATEGORY_NATIONAL:
			if (!fsrc->pic) {
				return MOVE_INVALID_NO_MESSAGE;
			}
			if (CB_TREE_CATEGORY (src) == CB_CATEGORY_NUMERIC
			    && fsrc->pic->scale > 0) {
				return MOVE_NON_INTEGER_TO_ALNUM;
			}
			if (dst_size == FIELD_SIZE_UNKNOWN) {
				break;
			}
			if (is_numeric_edited) {
				dst_size = count_pic_edited (fdst);
			} else {
				dst_size = fdst->size;
			}
			if (CB_TREE_CATEGORY (src) == CB_CATEGORY_NUMERIC
			    && (int)fsrc->pic->digits > dst_size) {
				return MOVE_NUMERIC_POSSIBLE_TRUNCATION;
			}
			if (CB_TREE_CATEGORY (src) == CB_CATEGORY_NUMERIC_EDITED
			    && fsrc->size > dst_size) {
				return MOVE_GENERAL_POSSIBLE_TRUNCATION;
			}
			break;
		default:
			if (!fsrc->pic) {
				return MOVE_INVALID_NO_MESSAGE;
			}
			if (!fdst->pic) {
				return MOVE_INVALID_NO_MESSAGE;
			}
			src_scale_mod = fsrc->pic->scale < 0 ?
				0 : fsrc->pic->scale;
			dst_scale_mod = fdst->pic->scale < 0 ?
				0 : fdst->pic->scale;
			if (fsrc->pic->digits - src_scale_mod >
			    fdst->pic->digits - dst_scale_mod
			    || src_scale_mod > dst_scale_mod) {
				return MOVE_NUMERIC_POSSIBLE_TRUNCATION;
			}
			break;
		}
		break;
	default:
		cb_error_x (loc, _("invalid source for MOVE"));
		return MOVE_INVALID_NO_MESSAGE;
	}

	return MOVE_OK;
}

static enum move_outcome
validate_move_from_field_or_ref (cb_tree src, cb_tree dst)
{
	struct cb_field		*fdst;
	struct cb_field		*fsrc;
	cb_tree			loc = src->source_line ? src : dst;
	int			dst_size;
	signed int		src_size;

	fdst = CB_FIELD_PTR (dst);
	/* Check dst not constant */
	if (fdst->flag_internal_constant || fdst->flag_constant) {
		return MOVE_INVALID;
	}
	if (CB_REFERENCE_P(src)) {
		cb_tree val = CB_REFERENCE(src)->value;
		if (CB_ALPHABET_NAME_P (val)) {
			return MOVE_OK;
		}
		if (!CB_FIELD_P (val)) {
			return MOVE_INVALID;
		}
		fsrc = CB_FIELD (val);
	} else {
		fsrc = CB_FIELD (src);
	}

	if (cb_move_ibm) {
		/* This MOVE result is exactly as on IBM, ignore overlapping */
		overlapping = 0;
	} else {
		/* Check basic overlapping */
		overlapping = cb_check_overlapping (fsrc, fdst, src, dst);
		warn_overlapping_move (loc);
	}

	src_size = cb_field_size (src);
	if (CB_TREE_CATEGORY (src) == CB_CATEGORY_NATIONAL) {
		src_size /= COB_NATIONAL_SIZE;
	}
	dst_size = cb_field_size (dst);
	if (CB_TREE_CATEGORY (dst) == CB_CATEGORY_NATIONAL) {
		dst_size /= COB_NATIONAL_SIZE;
	}

	if (fsrc->children || fdst->children) {
		return validate_non_elem_move_from_field_or_ref (src_size, dst_size);
	} else {
		return validate_elem_move_from_field_or_ref (src, dst);
	}
}

int
validate_move (cb_tree src, cb_tree dst, const unsigned int is_value, int *move_zero)
{
	cb_tree			loc = src->source_line ? src : dst;
	signed int		size;	/* -1 as special value */
	enum move_outcome	outcome = MOVE_OK;

	/* CHECKME: most of the "invalid" checks should possibly be handled in the parser */

	overlapping = 0;

	if (move_zero) {
		*move_zero = 0;
	}

	/* Easy stuff */
	/* Check dst not alphabet, file or constant */
	if (CB_REFERENCE_P (dst)) {
		cb_tree dstr = CB_REFERENCE (dst)->value;
		if (CB_ALPHABET_NAME_P (dstr)
		 || CB_CONST_P (dstr)
		 || CB_LITERAL_P (dstr)
		 || CB_FILE_P (dstr)) {
			outcome = MOVE_INVALID;
			goto process_outcome;
		}
	}
	/* CHECKME: why was it necessary to add this when merging 4875 from GC3 ? */
	/* Check dst not literal */
	if (CB_LITERAL_P (dst)) {
		outcome = MOVE_INVALID;
		goto process_outcome;
	}

	if (CB_TREE_CLASS (dst) == CB_CLASS_POINTER) {
		/* Check MOVE is between pointers only */
		if (CB_TREE_CLASS (src) == CB_CLASS_POINTER) {
			outcome = MOVE_OK;
		} else {
			if (cb_numeric_pointer
			 && CB_TREE_CLASS (src) == CB_CLASS_NUMERIC) {
				outcome = MOVE_OK;
			} else {
				outcome = MOVE_INVALID;
			}
		}
		goto process_outcome;
	}

	/* Check dst not Boolean */
	if (CB_TREE_CATEGORY (dst) == CB_CATEGORY_BOOLEAN) {
		outcome = MOVE_INVALID;
		goto process_outcome;
	}

	switch (CB_TREE_TAG (src)) {
	case CB_TAG_CONST:
		outcome = validate_move_from_const (src, dst, is_value);
		break;
	case CB_TAG_LITERAL:
		outcome = validate_move_from_literal (src, dst, is_value, &size);
		break;
	case CB_TAG_FIELD:
	case CB_TAG_REFERENCE:
		outcome = validate_move_from_field_or_ref (src, dst);
		break;
	case CB_TAG_CAST:
		outcome =  MOVE_INVALID;
	case CB_TAG_INTEGER:
	case CB_TAG_BINARY_OP:
	case CB_TAG_INTRINSIC:
	case CB_TAG_FUNCALL:
		/* TODO: check this */
		break;
	/* LCOV_EXCL_START */
	default:
		CB_TREE_TAG_UNEXPECTED_ABORT (src);
	/* LCOV_EXCL_STOP */
	}

 process_outcome:
	switch (outcome) {
	case MOVE_OK:
		return 0;

	case MOVE_NON_INTEGER_TO_ALNUM:
		if (cb_move_noninteger_to_alphanumeric != CB_ERROR) {
			if (!suppress_warn) {
				cb_warning_x (COBC_WARN_FILLER, loc, _("MOVE of non-integer to alphanumeric"));
			}
			return 0;
		}
		/* fall through */
	case MOVE_INVALID:
		if (is_value) {
			cb_error_x (loc, _("invalid VALUE clause"));
		} else if ((current_statement && current_statement->statement == STMT_SET)
			   || cobc_cs_check == CB_CS_SET) {
			cb_error_x (loc, _("invalid SET statement"));
		} else {
			cb_error_x (loc, _("invalid MOVE statement"));
		}
		return -1;

	case MOVE_INVALID_NO_MESSAGE:
		return -1;

	case MOVE_SUBSTITUTING_ZERO:
		cb_warning_x (COBC_WARN_FILLER, loc,
			      _("source is non-numeric - substituting zero"));
		if (move_zero) {
			*move_zero = 1;
		}
		return 0;

	case MOVE_NUMERIC_LIT_OVERFLOW:
		if (is_value) {
			cb_error_x (loc, _("invalid VALUE clause"));
			cb_error_x (loc, _("literal exceeds data size"));
			return -1;
		}
		if (!suppress_warn) {
			cb_warning_x (cb_warn_truncate, loc, _("numeric literal exceeds data size"));
		}
		return 0;

	case MOVE_NUMERIC_EXPECTED:
		move_warning (src, dst, is_value, cb_warn_strict_typing, 0,
			      _("numeric value is expected"));
		return 0;

	case MOVE_ALNUM_EXPECTED:
		move_warning (src, dst, is_value, cb_warn_strict_typing, 0,
			      _("alphanumeric value is expected"));
		return 0;

	case MOVE_NATIONAL_EXPECTED:
		move_warning (src, dst, is_value, cb_warn_strict_typing, 0,
			      _("national value is expected"));
		return 0;

	case MOVE_VALUE_NOT_FIT_PIC:
		move_warning (src, dst, is_value, cb_warn_truncate, 0,
			      _("value does not fit the picture string"));
		return 0;

	case MOVE_GENERAL_OVERFLOW:
		/* note: size is -1 for numeric literals, contains literal size otherwise */
		move_warning (src, dst, is_value, cb_warn_truncate, size,
			      _("value size exceeds data size"));
		return 0;

	case MOVE_GENERAL_POSSIBLE_TRUNCATION:
		move_warning (src, dst, is_value, cb_warn_pos_truncate, 1,
			      _("sending field larger than receiving field"));
		return 0;

	case MOVE_NUMERIC_POSSIBLE_TRUNCATION:
		move_warning (src, dst, is_value, cb_warn_pos_truncate, 1,
			      _("some digits may be truncated"));
		return 0;

	/* LCOV_EXCL_START */
	default:
		cobc_err_msg (_("unexpected enum value: %d"), outcome);
		COBC_ABORT ();
	/* LCOV_EXCL_STOP */
	}
}

static cb_tree
cb_build_memset (cb_tree x, const int c)
{
	cb_tree source = cb_int (c);
	if (cb_field_size (x) == 1) {
		return CB_BUILD_FUNCALL_2 ("$E", x, source);
	}
	return CB_BUILD_FUNCALL_3 ("memset",
				   CB_BUILD_CAST_ADDRESS (x),
				   source, CB_BUILD_CAST_LENGTH (x));
}

static cb_tree
cb_build_move_copy (cb_tree src, cb_tree dst)
{
	int	size;

	size = cb_field_size (dst);
	if (size == 1) {
		return CB_BUILD_FUNCALL_2 ("$F", dst, src);
	}
	if (cb_move_ibm) {
		overlapping = 0;
		return CB_BUILD_FUNCALL_3 ("cob_move_ibm",
					   CB_BUILD_CAST_ADDRESS (dst),
					   CB_BUILD_CAST_ADDRESS (src),
					   CB_BUILD_CAST_LENGTH (dst));
	} else if (overlapping
	|| CB_FIELD_PTR (src)->storage == CB_STORAGE_LINKAGE
	|| CB_FIELD_PTR (dst)->storage == CB_STORAGE_LINKAGE
	|| CB_FIELD_PTR (src)->flag_item_based
	|| CB_FIELD_PTR (dst)->flag_item_based) {
		overlapping = 0;
		return CB_BUILD_FUNCALL_3 ("memmove",
					   CB_BUILD_CAST_ADDRESS (dst),
					   CB_BUILD_CAST_ADDRESS (src),
					   CB_BUILD_CAST_LENGTH (dst));
	} else {
		return CB_BUILD_FUNCALL_3 ("memcpy",
					   CB_BUILD_CAST_ADDRESS (dst),
					   CB_BUILD_CAST_ADDRESS (src),
					   CB_BUILD_CAST_LENGTH (dst));
	}
}

static cb_tree
cb_build_move_num_zero (cb_tree x)
{
	struct cb_field		*f;

	f = CB_FIELD_PTR (x);
	switch (f->usage) {
	case CB_USAGE_BINARY:
	case CB_USAGE_COMP_5:
	case CB_USAGE_COMP_X:
	case CB_USAGE_COMP_N:
		if (f->flag_binary_swap) {
			return cb_build_memset (x, 0);
		}
		switch (f->size) {
#ifdef	COB_NON_ALIGNED
		case 1:
			return cb_build_assign (x, cb_int0);
		case 2:
#ifdef	COB_SHORT_BORK
			if (f->storage != CB_STORAGE_LINKAGE && f->indexes == 0 &&
			   (f->offset % 4 == 0)) {
				return cb_build_assign (x, cb_int0);
			}
			break;
#endif
		case 4:
		case 8:
			if (f->storage != CB_STORAGE_LINKAGE && f->indexes == 0 &&
			   (f->offset % f->size == 0)) {
				return cb_build_assign (x, cb_int0);
			}
			break;
#else
		case 1:
		case 2:
		case 4:
		case 8:
			return cb_build_assign (x, cb_int0);
#endif
		default:
			break;
		}
		return cb_build_memset (x, 0);
	case CB_USAGE_DISPLAY:
		if (!cb_ebcdic_sign) {
			return cb_build_memset (x, '0');
		}
		if (f->pic && !f->pic->have_sign) {
			return cb_build_memset (x, '0');
		}
		break;
	case CB_USAGE_PACKED:
		return CB_BUILD_FUNCALL_1 ("cob_set_packed_zero", x);
	case CB_USAGE_COMP_6:
		return cb_build_memset (x, 0);
	default:
		break;
	}
	return CB_BUILD_FUNCALL_2 ("cob_move", cb_zero, x);
}

static cb_tree
cb_build_move_space (cb_tree x)
{
	switch (CB_TREE_CATEGORY (x)) {
	case CB_CATEGORY_NUMERIC:
	case CB_CATEGORY_ALPHABETIC:
	case CB_CATEGORY_ALPHANUMERIC:
		if (!CB_FIELD_PTR (x)->flag_any_length) {
			return cb_build_memset (x, ' ');
		}
		/* Fall through */
	default:
		return CB_BUILD_FUNCALL_2 ("cob_move", cb_space, x);
	}
}

static cb_tree
cb_build_move_zero (cb_tree x)
{
	switch (CB_TREE_CATEGORY (x)) {
	case CB_CATEGORY_NUMERIC:
		if (CB_FIELD_PTR (x)->flag_blank_zero) {
			return cb_build_move_space (x);
		} else if (CB_FIELD_PTR (x)->flag_sign_separate) {
			return CB_BUILD_FUNCALL_2 ("cob_move", cb_zero, x);
		} else {
			return cb_build_move_num_zero (x);
		}
	case CB_CATEGORY_ALPHABETIC:
	case CB_CATEGORY_ALPHANUMERIC:
		if (!CB_FIELD_PTR (x)->flag_any_length) {
			return cb_build_memset (x, '0');
		}
		/* Fall through */
	default:
		return CB_BUILD_FUNCALL_2 ("cob_move", cb_zero, x);
	}
}

static cb_tree
cb_build_move_high (cb_tree x)
{
	switch (CB_TREE_CATEGORY (x)) {
	case CB_CATEGORY_NUMERIC:
	case CB_CATEGORY_ALPHABETIC:
	case CB_CATEGORY_ALPHANUMERIC:
		if (CB_FIELD_PTR (x)->flag_any_length) {
			return CB_BUILD_FUNCALL_2 ("cob_move", cb_high, x);
		}
		if (cb_high == cb_norm_high) {
			return cb_build_memset (x, 255);
		}
		/* Fall through */
	default:
		return CB_BUILD_FUNCALL_2 ("cob_move", cb_high, x);
	}
}

static cb_tree
cb_build_move_low (cb_tree x)
{
	switch (CB_TREE_CATEGORY (x)) {
	case CB_CATEGORY_NUMERIC:
	case CB_CATEGORY_ALPHABETIC:
	case CB_CATEGORY_ALPHANUMERIC:
		if (CB_FIELD_PTR (x)->flag_any_length) {
			return CB_BUILD_FUNCALL_2 ("cob_move", cb_low, x);
		}
		if (cb_low == cb_norm_low) {
			return cb_build_memset (x, 0);
		}
		/* Fall through */
	default:
		return CB_BUILD_FUNCALL_2 ("cob_move", cb_low, x);
	}
}

static cb_tree
cb_build_move_quote (cb_tree x)
{
	switch (CB_TREE_CATEGORY (x)) {
	case CB_CATEGORY_NUMERIC:
	case CB_CATEGORY_ALPHABETIC:
	case CB_CATEGORY_ALPHANUMERIC:
		if (!CB_FIELD_PTR (x)->flag_any_length) {
			return cb_build_memset (x, cb_flag_apostrophe ? '\'' : '"');
		}
		/* Fall through */
	default:
		return CB_BUILD_FUNCALL_2 ("cob_move", cb_quote, x);
	}
}

#ifdef	COB_EBCDIC_MACHINE
static void
cob_put_sign_ascii (unsigned char *p)
{
	switch (*p) {
	case '0':
		*p = (unsigned char)'p';
		return;
	case '1':
		*p = (unsigned char)'q';
		return;
	case '2':
		*p = (unsigned char)'r';
		return;
	case '3':
		*p = (unsigned char)'s';
		return;
	case '4':
		*p = (unsigned char)'t';
		return;
	case '5':
		*p = (unsigned char)'u';
		return;
	case '6':
		*p = (unsigned char)'v';
		return;
	case '7':
		*p = (unsigned char)'w';
		return;
	case '8':
		*p = (unsigned char)'x';
		return;
	case '9':
		*p = (unsigned char)'y';
		return;
	}
}
#endif

/* does an EBCDIC overpunch with the expected character values */
static void
cob_put_sign_ebcdic (unsigned char *p, const int sign)
{
	if (sign == -1) {
		switch (*p) {
		case '0':
			*p = (unsigned char)'}';
			return;
		case '1':
			*p = (unsigned char)'J';
			return;
		case '2':
			*p = (unsigned char)'K';
			return;
		case '3':
			*p = (unsigned char)'L';
			return;
		case '4':
			*p = (unsigned char)'M';
			return;
		case '5':
			*p = (unsigned char)'N';
			return;
		case '6':
			*p = (unsigned char)'O';
			return;
		case '7':
			*p = (unsigned char)'P';
			return;
		case '8':
			*p = (unsigned char)'Q';
			return;
		case '9':
			*p = (unsigned char)'R';
			return;
		default:
			/* What to do here? */
			*p = (unsigned char)'}';
			return;
		}
	}
	switch (*p) {
	case '0':
		*p = (unsigned char)'{';
		return;
	case '1':
		*p = (unsigned char)'A';
		return;
	case '2':
		*p = (unsigned char)'B';
		return;
	case '3':
		*p = (unsigned char)'C';
		return;
	case '4':
		*p = (unsigned char)'D';
		return;
	case '5':
		*p = (unsigned char)'E';
		return;
	case '6':
		*p = (unsigned char)'F';
		return;
	case '7':
		*p = (unsigned char)'G';
		return;
	case '8':
		*p = (unsigned char)'H';
		return;
	case '9':
		*p = (unsigned char)'I';
		return;
	default:
		/* What to do here ? */
		*p = (unsigned char)'{';
		return;
	}
}

static cb_tree
cb_build_move_literal (cb_tree src, cb_tree dst)
{
	const struct cb_literal	*l = CB_LITERAL (src);
	const struct cb_field		*f = CB_FIELD_PTR (dst);
	const enum cb_category	cat = CB_TREE_CATEGORY (dst);

	unsigned char		*buff;
	unsigned char		bbyte;
	struct cb_reference	*r;
	int			i;
	int			val;
	int			n;

	if (f->flag_any_length) {
		return CB_BUILD_FUNCALL_2 ("cob_move", src, dst);
	}

	if (CB_REFERENCE_P (dst)) {
		r = CB_REFERENCE (dst);
		if ((cb_reference_bounds_check == CB_WARNING
		  || cb_reference_bounds_check == CB_OK)
		 && (r->offset != NULL
		  || r->length != NULL)) {
			return CB_BUILD_FUNCALL_2 ("cob_move", src, dst);
		}
	}

	if (l->all) {
		if (cat == CB_CATEGORY_NUMERIC
		 || cat == CB_CATEGORY_NUMERIC_EDITED
		 || cat == CB_CATEGORY_FLOATING_EDITED) {
			return CB_BUILD_FUNCALL_2 ("cob_move", src, dst);
		}
		if (l->size == 1) {
			return CB_BUILD_FUNCALL_3 ("memset",
					   CB_BUILD_CAST_ADDRESS (dst),
					   cb_int (l->data[0]),
					   CB_BUILD_CAST_LENGTH (dst));
		}
		bbyte = l->data[0];
		for (i = 0; i < (int)l->size; i++) {
			if (bbyte != l->data[i]) {
				break;
			}
			bbyte = l->data[i];
		}
		if (i == (int)l->size) {
			return CB_BUILD_FUNCALL_3 ("memset",
					   CB_BUILD_CAST_ADDRESS (dst),
					   cb_int (l->data[0]),
					   CB_BUILD_CAST_LENGTH (dst));
		}
		if (f->size > 128) {
			return CB_BUILD_FUNCALL_2 ("cob_move", src, dst);
		}
		buff = cobc_parse_malloc ((size_t)f->size);
		for (i = 0; i < f->size; i++) {
			buff[i] = l->data[i % l->size];
		}
		return CB_BUILD_FUNCALL_3 ("memcpy",
					   CB_BUILD_CAST_ADDRESS (dst),
					   cb_build_string (buff, (size_t)f->size),
					   CB_BUILD_CAST_LENGTH (dst));
	}

	if (cat == CB_CATEGORY_NUMERIC_EDITED
	 || cat == CB_CATEGORY_FLOATING_EDITED) {
		return CB_BUILD_FUNCALL_2 ("cob_move", src, dst);
	}

	if ((  cat == CB_CATEGORY_NUMERIC
	    && f->usage == CB_USAGE_DISPLAY
	    && f->pic->scale == l->scale
	    && !f->flag_sign_separate)
	 || ( (cat == CB_CATEGORY_ALPHABETIC || cat == CB_CATEGORY_ALPHANUMERIC)
		&& f->size < (int) (l->size + 16)
		&& !cb_field_variable_size (f))) {
		const int	diff = (int) (f->size - l->size);
		buff = cobc_parse_malloc ((size_t)f->size);
		if (cat == CB_CATEGORY_NUMERIC) {
			unsigned char		*p;
			if (diff <= 0) {
				memcpy (buff, l->data - diff, (size_t)f->size);
			} else {
				memset (buff, '0', (size_t)diff);
				memcpy (buff + diff, l->data, (size_t)l->size);
			}
			/* Check all zeros */
			n = 0;
			for (p = buff; p < buff + f->size; p++) {
				if (*p != '0') {
					n = 1;
					break;
				}
			}
			if (f->pic->have_sign) {
				if (f->flag_sign_leading) {
					p = buff;
				} else {
					p = buff + f->size - 1;
				}
#if 0	/* Simon: negative zero back by disabling the following code
´                 included without documentation by Roger in 2.0 */
				if (!n) {
					/* Zeros */
					/* EBCDIC - store sign otherwise nothing */
					if (cb_ebcdic_sign) {
						cob_put_sign_ebcdic (p, 1);
					}
				} else 
#endif
				if (cb_ebcdic_sign) {
					cob_put_sign_ebcdic (p, l->sign);
				} else
				if (l->sign == -1) {
#ifdef	COB_EBCDIC_MACHINE
					cob_put_sign_ascii (p);
#else
					*p += 0x40;
#endif
				}
			}
			if (f->flag_blank_zero && !n) {
				cobc_parse_free (buff);
				return CB_BUILD_FUNCALL_3 ("memset",
						   CB_BUILD_CAST_ADDRESS (dst),
						   cb_int (' '),
						   CB_BUILD_CAST_LENGTH (dst));
			}
		} else {
			if (f->flag_justified) {
				if (diff <= 0) {
					memcpy (buff, l->data - diff, (size_t)f->size);
				} else {
					memset (buff, ' ', (size_t)diff);
					memcpy (buff + diff, l->data, (size_t)l->size);
				}
			} else {
				if (diff <= 0) {
					memcpy (buff, l->data, (size_t)f->size);
				} else {
					memcpy (buff, l->data, (size_t)l->size);
					memset (buff + l->size, ' ', (size_t)diff);
				}
			}
		}
		bbyte = *buff;
		if (f->size == 1) {
			cobc_parse_free (buff);
			return CB_BUILD_FUNCALL_2 ("$E", dst, cb_int (bbyte));
		}
		for (i = 0; i < f->size; i++) {
			if (bbyte != buff[i]) {
				break;
			}
		}
		if (i == f->size) {
			cobc_parse_free (buff);
			return CB_BUILD_FUNCALL_3 ("memset",
					   CB_BUILD_CAST_ADDRESS (dst),
					   cb_int (bbyte),
					   CB_BUILD_CAST_LENGTH (dst));
		}
		return CB_BUILD_FUNCALL_3 ("memcpy",
					   CB_BUILD_CAST_ADDRESS (dst),
					   cb_build_string (buff, (size_t)f->size),
					   CB_BUILD_CAST_LENGTH (dst));
	}

	if ((f->usage == CB_USAGE_BINARY
	  || f->usage == CB_USAGE_COMP_5
	  || f->usage == CB_USAGE_COMP_X
	  || f->usage == CB_USAGE_COMP_N)
	 && cb_fits_int (src)
	 && f->size <= 8) {
		if (cb_binary_truncate) {
			if (!CB_NUMERIC_LITERAL_P (src)
			 || CB_LITERAL (src)->scale != 0
			 || f->size != sizeof(int)
			 || !f->flag_real_binary)
				return CB_BUILD_FUNCALL_2 ("cob_move", src, dst);
		}

		val = cb_get_int (src);
		n = f->pic->scale - l->scale;
		if ((l->size + n) > 9) {
			return CB_BUILD_FUNCALL_2 ("cob_move", src, dst);
		}
		for (; n > 0; n--) {
			val *= 10;
		}
		for (; n < 0; n++) {
			val /= 10;
		}
		if (val == 0) {
			/* binary cannot store negative zero */
			return cb_build_move_num_zero (dst);
		}
		if (val < 0 && !f->pic->have_sign) {
			val = -val;
		}
		if (f->size == 1) {
			return cb_build_assign (dst, cb_int (val));
		}
		if (f->flag_binary_swap) {
			i = (f->size - 1) + (8 * (f->pic->have_sign ? 1 : 0));
			optimize_defs[bin_set_funcs[i].optim_val] = 1;
			return CB_BUILD_FUNCALL_2 (bin_set_funcs[i].optim_name,
				CB_BUILD_CAST_ADDRESS (dst),
				cb_int (val));
		}
		switch (f->size) {
		case 2:
#ifdef	COB_SHORT_BORK
			if (f->storage != CB_STORAGE_LINKAGE && f->indexes == 0 &&
			   (f->offset % 4 == 0)) {
				return cb_build_assign (dst, cb_int (val));
			}
			break;
#endif
		case 4:
		case 8:
#ifdef	COB_NON_ALIGNED
			if (f->storage != CB_STORAGE_LINKAGE && f->indexes == 0 &&
			   (f->offset % f->size == 0)) {
				return cb_build_assign (dst, cb_int (val));
			}
			break;
#else
			return cb_build_assign (dst, cb_int (val));
#endif
		default:
			break;
		}
		return CB_BUILD_FUNCALL_2 ("cob_move", src, dst);
	}

	if ((f->usage == CB_USAGE_PACKED || f->usage == CB_USAGE_COMP_6)
	 && cb_fits_int (src)) {
		if (f->pic->scale < 0) {
			return CB_BUILD_FUNCALL_2 ("cob_move", src, dst);
		}
		n = f->pic->scale - l->scale;
		if ((l->size + n) > 9) {
			return CB_BUILD_FUNCALL_2 ("cob_move", src, dst);
		}
		val = cb_get_int (src);
		for (; n > 0; n--) {
			val *= 10;
		}
		for (; n < 0; n++) {
			val /= 10;
		}
		if (val == 0) {
			return cb_build_move_num_zero (dst);
		}
		if (val < 0 && !f->pic->have_sign) {
			val = -val;
		}
#if	1	/* RXWRXW - Set packed */
		return CB_BUILD_FUNCALL_2 ("cob_set_packed_int", dst,
					   cb_int (val));
#else
		return CB_BUILD_FUNCALL_2 ("cob_set_packed_int", dst,
					   cb_build_cast_llint (src));
#endif
	}
	return CB_BUILD_FUNCALL_2 ("cob_move", src, dst);
}

static cb_tree
cb_build_move_field (cb_tree src, cb_tree dst)
{
	const struct cb_field	*src_f = CB_FIELD_PTR (src);
	const struct cb_field	*dst_f = CB_FIELD_PTR (dst);
	int		src_size;
	int		dst_size;

	if (dst_f->flag_any_length || src_f->flag_any_length) {
		return CB_BUILD_FUNCALL_2 ("cob_move", src, dst);
	}
	src_size = cb_field_size (src);
	dst_size = cb_field_size (dst);
	if (src_size == -1
	 && dst_size == -1
	 && CB_REFERENCE_P (src)
	 && CB_REFERENCE_P (dst)) {
		/* check for same length, allowing us to do an optimized copy
		   case:  MOVE VAR1 (POS1:LEN) TO VAR2 (POS2:LEN) */
		const struct cb_reference *r_src = CB_REFERENCE (src);
		const struct cb_reference *r_dst = CB_REFERENCE (dst);
		if (r_src->length && r_dst->length
		 && CB_REFERENCE_P (r_src->length)
		 && CB_REFERENCE_P (r_dst->length)
		 && CB_REFERENCE (r_src->length)->value
		 == CB_REFERENCE (r_dst->length)->value) {
			src_size = dst_size = 1;
		}
	}
	if (src_size > 0 && dst_size > 0 && src_size >= dst_size
	 && !cb_field_variable_size (src_f)
	 && !cb_field_variable_size (dst_f)) {
		switch (CB_TREE_CATEGORY (src)) {
		case CB_CATEGORY_ALPHABETIC:
			if (CB_TREE_CATEGORY (dst) == CB_CATEGORY_ALPHABETIC
			 || CB_TREE_CATEGORY (dst) == CB_CATEGORY_ALPHANUMERIC) {
				if (dst_f->flag_justified == 0) {
					return cb_build_move_copy (src, dst);
				}
			}
			break;
		case CB_CATEGORY_ALPHANUMERIC:
			if (CB_TREE_CATEGORY (dst) == CB_CATEGORY_ALPHANUMERIC) {
				if (dst_f->flag_justified == 0) {
					return cb_build_move_copy (src, dst);
				}
			}
			break;
		case CB_CATEGORY_NUMERIC:
			if (CB_TREE_CATEGORY (dst) == CB_CATEGORY_NUMERIC
			 && src_f->usage == dst_f->usage
			 && src_f->pic->size == dst_f->pic->size
			 && src_f->pic->digits == dst_f->pic->digits
			 && src_f->pic->scale == dst_f->pic->scale
			 && src_f->pic->have_sign == dst_f->pic->have_sign
			 && src_f->flag_binary_swap == dst_f->flag_binary_swap
			 && src_f->flag_sign_leading == dst_f->flag_sign_leading
			 && src_f->flag_sign_separate == dst_f->flag_sign_separate) {
				return cb_build_move_copy (src, dst);
			}
			if (CB_TREE_CATEGORY (dst) == CB_CATEGORY_ALPHANUMERIC
			 && src_f->usage == CB_USAGE_DISPLAY
			 && src_f->pic->have_sign == 0
			 && !src_f->flag_sign_leading
			 && !src_f->flag_sign_separate) {
				return cb_build_move_copy (src, dst);
			}
			break;
		default:
			break;
		}
	}

	if ( (src_f->usage == CB_USAGE_PACKED
	   || src_f->usage == CB_USAGE_COMP_6)
	  && (dst_f->usage == CB_USAGE_PACKED
	   || dst_f->usage == CB_USAGE_COMP_6)) {
		/* TODO: add handling of negative scales to cob_move_bcd */
		if (src_f->pic->scale >= 0
		 && dst_f->pic->scale >= 0) {
			return CB_BUILD_FUNCALL_2 ("cob_move_bcd", src, dst);
		}
	}

	return CB_BUILD_FUNCALL_2 ("cob_move", src, dst);
}

cb_tree
cb_build_move (cb_tree src, cb_tree dst)
{
	struct cb_reference	*src_ref, *dst_ref, *x;
	cb_tree	chks = NULL;
	cb_tree	ret;
	int	move_zero;

	if (CB_INVALID_TREE (src)
	 || CB_INVALID_TREE (dst)) {
		return cb_error_node;
	}

	if (validate_move (src, dst, 0, &move_zero) < 0) {
		return cb_error_node;
	}

#if	0	/* Flag receiving */
	if (CB_REFERENCE_P (src)) {
		CB_REFERENCE (src)->flag_receiving = 0;
	}
#endif
	if (move_zero) {
		src = cb_zero;
	} else if (CB_LITERAL_P (src)) {
		/* FIXME: don't do this for a DYNAMIC LENGTH target */
		const struct cb_literal* lit = CB_LITERAL (src);
		char *p = (char*)lit->data;
		char *end = p + lit->size - 1;
		if (*end == ' ') {
			while (p < end && *p == ' ') p++;
			if (p == end) src = cb_space;
		}
	}

	if (current_program->flag_report) {
		/* FIXME: way too much for SUM field */
		src = cb_check_sum_field (src);
		dst = cb_check_sum_field (dst);
	}

	if (CB_REFERENCE_P (src)) {
		src_ref = CB_REFERENCE (src);
	} else {
		src_ref = NULL;
	}
	if (CB_REFERENCE_P (dst)) {
		/* Clone reference */
		x = cobc_parse_malloc (sizeof(struct cb_reference));
		*x = *CB_REFERENCE (dst);
		x->flag_receiving = 1;
		dst = CB_TREE (x);
		dst_ref = x;
	} else {
		dst_ref = NULL;
	}

	if (CB_TREE_CLASS (dst) == CB_CLASS_POINTER
	 || CB_TREE_CLASS (src) == CB_CLASS_POINTER) {
		if (cb_numeric_pointer
		 && CB_TREE_CLASS (dst) != CB_TREE_CLASS (src)) {
			return CB_BUILD_FUNCALL_2 ("cob_move", src, dst);
		}
		return cb_build_assign (dst, src);
	}

	if (src_ref && CB_ALPHABET_NAME_P (src_ref->value)) {
		return CB_BUILD_FUNCALL_2 ("cob_move", src, dst);
	}
	if (CB_INDEX_OR_HANDLE_P (dst)) {
		if (src == cb_null) {
			return cb_build_assign (dst, cb_zero);
		}
		return cb_build_assign (dst, src);
	}

	if (CB_INDEX_OR_HANDLE_P (src)) {
		return CB_BUILD_FUNCALL_2 ("cob_set_int", dst,
					   cb_build_cast_int (src));
	}

	if (CB_INTRINSIC_P (src) || CB_INTRINSIC_P (dst)) {
		return CB_BUILD_FUNCALL_2 ("cob_move", src, dst);
	}

#if 1 /* OPTCHK Simon: optimal copy also with runtime checks enabled */
	/* TODO: optimize by resolving subscripts as integers once per statement,
	     77 XX PIC 99 VALUE 5.
	     01 X         PIC X OCCURS 0 TO 10 DEPENDING ON XX.
	       MOVE ZERO TO X(2) X(4) X(6) X(8)
	  current version:
		cob_check_odo (cob_get_numdisp (b_17, 2), 0, 10, "X", "XX");
		cob_check_subscript (2, cob_get_numdisp (b_17, 2), "X", 1);
		*(b_18 + 1) = 48;
		cob_check_odo (cob_get_numdisp (b_17, 2), 0, 10, "X", "XX");
		cob_check_subscript (4, cob_get_numdisp (b_17, 2), "X", 1);
		*(b_18 + 3) = 48;
		cob_check_odo (cob_get_numdisp (b_17, 2), 0, 10, "X", "XX");
		cob_check_subscript (6, cob_get_numdisp (b_17, 2), "X", 1);
		*(b_18 + 5) = 48;
		cob_check_odo (cob_get_numdisp (b_17, 2), 0, 10, "X", "XX");
		cob_check_subscript (8, cob_get_numdisp (b_17, 2), "X", 1);
		*(b_18 + 7) = 48;
	  much better version (separate issue: the odo-item should
	  only be checked once, see comment on its addition):
	  {
	    const int odo_value = cob_get_numdisp (b_17, 2);
		cob_check_odo (, 0, 10, "X", "XX");
		cob_check_subscript (2, odo_value, "X", 1);
		*(b_18 + 1) = 48;
		cob_check_odo (odo_value, 0, 10, "X", "XX");
		cob_check_subscript (4, odo_value, "X", 1);
		*(b_18 + 3) = 48;
		cob_check_odo (odo_value, 0, 10, "X", "XX");
		cob_check_subscript (6, odo_value, "X", 1);
		*(b_18 + 5) = 48;
		cob_check_odo (odo_value, 0, 10, "X", "XX");
		cob_check_subscript (8, odo_value, "X", 1);
		*(b_18 + 7) = 48;
	  }
	*/
	if (src_ref && src_ref->check) {
		chks = src_ref->check;
		src_ref->check = NULL;
		if (dst_ref && dst_ref->check) {
			chks = cb_list_add (chks, dst_ref->check);
			dst_ref->check = NULL;
		}
	} else
	if (dst_ref && dst_ref->check) {
		chks = dst_ref->check;
		dst_ref->check = NULL;
	}
#else
	if (src_ref && src_ref->check) {
		return CB_BUILD_FUNCALL_2 ("cob_move", src, dst);
	}
	if (dst_ref && dst_ref->check) {
		return CB_BUILD_FUNCALL_2 ("cob_move", src, dst);
	}
#endif

	/* Output optimal code */
	if (src == cb_zero) {
		ret = cb_build_move_zero (dst);
	} else if (src == cb_space) {
		ret = cb_build_move_space (dst);
	} else if (src == cb_high) {
		ret = cb_build_move_high (dst);
	} else if (src == cb_low) {
		ret = cb_build_move_low (dst);
	} else if (src == cb_quote) {
		ret = cb_build_move_quote (dst);
	} else if (CB_LITERAL_P (src)) {
		ret = cb_build_move_literal (src, dst);
	} else {
		ret = cb_build_move_field (src, dst);
	}
#if 1 /* OPTCHK Simon: optimal copy also with runtime checks enabled */
	if (chks) {
		return cb_list_add (chks, ret);
	}
#endif
	return ret;
}

/* TODO: Shouldn't this include validate_move()? */
static int
cb_check_move (cb_tree src, cb_tree dsts, const int emit_error)
{
	cb_tree		l;
	cb_tree		x;
	int		error_found = 0;

	if (cb_validate_one (src)) {
		return 1;
	}
	if (cb_validate_list (dsts)) {
		return 1;
	}

	for (l = dsts; l; l = CB_CHAIN (l)) {
		x = CB_VALUE (l);
		if (CB_LITERAL_P (x) || CB_CONST_P (x)) {
			if (emit_error) {
				cb_error_x (CB_TREE (current_statement),
					    _("invalid MOVE target: %s"),
					    cb_name (x));
			}
			error_found = 1;
		}
	}

	return error_found;
}

void
cb_emit_move (cb_tree src, cb_tree dsts)
{
	cb_tree		l;
	cb_tree		x;
	cb_tree		m;
	cb_tree		svoff;
	struct cb_literal	*lt;
	struct cb_field	*f, *p;
	unsigned int	tempval;
	struct cb_reference	*r;
	int		bgnpos;

	if (cb_check_move (src, dsts, 1)) {
		return;
	}

	/* Validate source, if requested. */
	cb_emit_incompat_data_checks (src);

	/* FIXME: this is way to much to cater for sum field */
	src = cb_check_sum_field (src);

	tempval = 0;
	if (cb_list_length (dsts) > 1) {
		if (CB_REFERENCE_P (src)) {
			r = CB_REFERENCE (src);
		} else {
			r = NULL;
		}
		if (CB_INTRINSIC_P (src) || (r && (r->subs || r->offset))) {
			tempval = 1;
			cb_emit (CB_BUILD_FUNCALL_1 ("cob_put_indirect_field",
						     src));
		}
	}

	cb_check_list (dsts);
	for (l = dsts; l; l = CB_CHAIN (l)) {
		x = CB_VALUE (l);
		if (CB_REFERENCE_P (x)) {
			r = CB_REFERENCE (x);
		} else {
			r = NULL;
		}
		if (CB_LITERAL_P (x) || CB_CONST_P (x) ||
			(r && (CB_LABEL_P (r->value) || CB_PROTOTYPE_P (r->value)))) {
			cb_error_x (CB_TREE (current_statement),
				    _("invalid MOVE target: %s"), cb_name (x));
			continue;
		}
		if (!tempval) {
			if (CB_REFERENCE_P (x)
			 && CB_REFERENCE (x)->length == NULL
			 && (cb_odoslide || cb_complex_odo)) {
				p = CB_FIELD_PTR(x);
				if ((f = chk_field_variable_size (p)) != NULL) {
					bgnpos = -1;
					if (CB_REFERENCE (x)->offset == NULL
					 || CB_REFERENCE (x)->offset == cb_int1) {
						bgnpos = 1;
					} else if (CB_REFERENCE (x)->offset == cb_int2) {
						bgnpos = 2;
					} else
					if (CB_REFERENCE (x)->offset != NULL
					 && CB_LITERAL_P (CB_REFERENCE (x)->offset)) {
						lt = CB_LITERAL (CB_REFERENCE (x)->offset);
						bgnpos = atoi ((const char *)lt->data);
					}
					if (bgnpos >= 1
					 && p->storage != CB_STORAGE_LINKAGE
					 && !p->flag_item_based
					 && CB_LITERAL_P (src)
					 && !cb_is_field_unbounded (p)
					 && !p->flag_picture_l) {
						CB_REFERENCE (x)->length = cb_int (p->size - bgnpos + 1);
					} else {
						if (bgnpos >= p->offset
						 && bgnpos < f->offset
						 && p->offset < f->offset) {
							/* Move for fixed size header of field */
							/* to move values of possible DEPENDING ON fields */
							svoff = CB_REFERENCE (x)->offset;
							CB_REFERENCE (x)->offset = cb_int (bgnpos);
							CB_REFERENCE (x)->length = cb_int (f->offset - p->offset - bgnpos + 1);
							m = cb_build_move (src, cb_check_sum_field(x));
							cb_emit (m);
							CB_REFERENCE (x)->offset = svoff;
							CB_REFERENCE (x)->length = NULL;
							/* Then move the full field with ODO lengths set */
						}
					}
				}
			}
#if 0 /* CHECKME: this is way to much to cater for sum field */
			m = cb_build_move (src, cb_check_sum_field(x));
#else
			m = cb_build_move (src, x);
#endif
		} else {
			m = CB_BUILD_FUNCALL_1 ("cob_get_indirect_field", x);
		}
		cb_emit (m);
	}
}

/* OPEN statement */

void
cb_emit_open (cb_tree file, cb_tree mode, cb_tree sharing)
{
	cb_tree orig_file = file;
	struct cb_file	*f;
	int open_mode;

	file = cb_ref (file);
	if (file == cb_error_node) {
		return;
	}
	current_statement->file = file;
	f = CB_FILE (file);
	open_mode = CB_INTEGER(mode)->val;

	if (cb_listing_xref
	 && open_mode != COB_OPEN_INPUT) {
		/* add a "receiving" entry for the file */
		cobc_xref_link (&f->xref, CB_REFERENCE (orig_file)->common.source_line, 1);
	}

	if (f->organization == COB_ORG_SORT) {
		cb_error_x (CB_TREE (current_statement),
				_("%s not allowed on %s files"), "OPEN", "SORT");
		return;
	}
	if (sharing == NULL) {
		if (f->sharing) {
			sharing = f->sharing;
		} else {
			sharing = cb_int0;
		}
	}

	/* TODO: replace sharing with tree containing a string constant
	         (defines in common.h / codegen like COB_OPEN_I_O) */

	if (f->extfh) {
		cb_emit (CB_BUILD_FUNCALL_5 ("cob_extfh_open", f->extfh, file,
			 cb_build_direct (cb_open_mode_to_string (open_mode), 0),
			 sharing, f->file_status));
	} else {
		cb_emit (CB_BUILD_FUNCALL_4 ("cob_open", file,
			 cb_build_direct (cb_open_mode_to_string (open_mode), 0),
			 sharing, f->file_status));
	}

	/* Check for file debugging */
	if (current_program->flag_debugging
	 && !current_statement->flag_in_debug
	 && f->flag_fl_debug) {
		cb_emit (cb_build_debug (cb_debug_name, f->name, NULL));
		cb_emit (cb_build_move (cb_space, cb_debug_contents));
		cb_emit (cb_build_debug_call (f->debug_section));
	}
}

/* PERFORM statement */

void
cb_emit_perform (cb_tree perform, cb_tree body, cb_tree newthread, cb_tree handle)
{
	if (perform == cb_error_node) {
		return;
	}
	cb_check_reset ();
	if (handle && !usage_is_thread_handle (handle)) {
		cb_error_x (handle, _("HANDLE must be either a generic or a THREAD HANDLE"));
		return;
	}
	if (current_program->flag_debugging
	 && !current_statement->flag_in_debug && body && CB_PAIR_P (body)) {
		cb_emit (cb_build_debug (cb_debug_contents, "PERFORM LOOP", NULL));
	}

#if 0 /* TODO: implement THREADs in libcob */
	  /* remark: this won't work as the CALL has to be started in the new thread
	if (newthread) {
		cb_emit (CB_BUILD_FUNCALL_0 ("cob_threadstart"));
	}
	if (handle) {
		cb_emit (CB_BUILD_FUNCALL_1 ("cob_get_threadhandle", handle));
	} */
#else
	COB_UNUSED (newthread);
#endif
	CB_PERFORM (perform)->body = body;
	cb_emit (perform);
}

cb_tree
cb_build_perform_once (cb_tree body)
{
	cb_tree x;

	if (body == cb_error_node) {
		return cb_error_node;
	}
	x = cb_build_perform (CB_PERFORM_ONCE);
	CB_PERFORM (x)->body = body;
	return x;
}

cb_tree
cb_build_perform_times (cb_tree times)
{
	cb_tree		x;

	if (cb_check_integer_value (times) == cb_error_node) {
		return cb_error_node;
	}

	x = cb_build_perform (CB_PERFORM_TIMES);
	CB_PERFORM (x)->data = times;
	return x;
}

cb_tree
cb_build_perform_until (cb_tree condition, cb_tree varying)
{
	cb_tree		x;

	x = cb_build_perform (CB_PERFORM_UNTIL);
	CB_PERFORM (x)->test = condition;
	CB_PERFORM (x)->varying = varying;
	return x;
}

cb_tree
cb_build_perform_forever (cb_tree body)
{
	cb_tree		x;

	if (body == cb_error_node) {
		return cb_error_node;
	}
	x = cb_build_perform (CB_PERFORM_FOREVER);
	CB_PERFORM (x)->body = body;
	return x;
}

cb_tree
cb_build_perform_exit (struct cb_label *label)
{
	cb_tree		x;

	x = cb_build_perform (CB_PERFORM_EXIT);
	CB_PERFORM (x)->data = CB_TREE (label);
	return x;
}

/* READ statement */

void
cb_emit_read (cb_tree ref, cb_tree next, cb_tree into,
	      cb_tree key, cb_tree lock_opts)
{
	cb_tree		file;
	cb_tree		rec;
	cb_tree		x;
	struct cb_file	*f;
	int		read_opts;

	read_opts = 0;
	if (lock_opts == cb_int1) {
		read_opts = COB_READ_LOCK;
	} else if (lock_opts == cb_int2) {
		read_opts = COB_READ_NO_LOCK;
	} else if (lock_opts == cb_int3
		|| current_statement->flag_ignore_lock) {
		read_opts = COB_READ_IGNORE_LOCK;
		current_statement->flag_ignore_lock = 0;
	} else if (lock_opts == cb_int4) {
		read_opts = COB_READ_WAIT_LOCK;
	} else if (lock_opts == cb_int5) {
		read_opts = COB_READ_LOCK | COB_READ_KEPT_LOCK;
	} else if (lock_opts == cb_int6
		|| current_statement->flag_advancing_lock) {
		read_opts = COB_READ_ADVANCING_LOCK;
		current_statement->flag_advancing_lock = 0;
	}
	file = cb_ref (ref);
	if (file == cb_error_node) {
		return;
	}
	f = CB_FILE (file);

	rec = cb_build_field_reference (f->record, ref);
	if (f->organization == COB_ORG_SORT) {
		cb_error_x (CB_TREE (current_statement),
				_("%s not allowed on %s files"), "READ", "SORT");
		return;
	}
	if (next == cb_int1 || next == cb_int2 ||
	    f->access_mode == COB_ACCESS_SEQUENTIAL) {
		/* READ NEXT/PREVIOUS */
		if (next == cb_int2) {
			switch (f->organization) {
			case COB_ORG_INDEXED:
			case COB_ORG_RELATIVE:
				break;
			default:
				cb_error_x (CB_TREE (current_statement),
				_("READ PREVIOUS not allowed for this file type"));
				return;
			}
			read_opts |= COB_READ_PREVIOUS;
		} else {
			read_opts |= COB_READ_NEXT;
		}
		if (key) {
			cb_warning (COBC_WARN_FILLER, _("KEY ignored with sequential READ"));
		}
		if (f->extfh) {
			cb_emit (CB_BUILD_FUNCALL_4 ("cob_extfh_read_next", f->extfh, file,
				 f->file_status, cb_int (read_opts)));
		} else {
			cb_emit (CB_BUILD_FUNCALL_3 ("cob_read_next", file,
				 f->file_status, cb_int (read_opts)));
		}
	} else {
		/* READ */
		/* DYNAMIC with [NOT] AT END */
		if (f->access_mode == COB_ACCESS_DYNAMIC &&
		    current_statement->handler_type == AT_END_HANDLER) {
			read_opts |= COB_READ_NEXT;
			if (f->extfh) {
				cb_emit (CB_BUILD_FUNCALL_4 ("cob_extfh_read_next", f->extfh, file,
					 f->file_status, cb_int (read_opts)));
			} else {
				cb_emit (CB_BUILD_FUNCALL_3 ("cob_read_next", file,
					 f->file_status, cb_int (read_opts)));
			}
		} else if (key || f->key) {
			if (f->extfh) {
				cb_emit (CB_BUILD_FUNCALL_5 ("cob_extfh_read", f->extfh,
					 file, key ? key : f->key,
					 f->file_status, cb_int (read_opts)));
			} else {
				cb_emit (CB_BUILD_FUNCALL_4 ("cob_read",
					 file, key ? key : f->key,
					 f->file_status, cb_int (read_opts)));
			}
		} else {
			if (f->extfh) {
				cb_emit (CB_BUILD_FUNCALL_4 ("cob_extfh_read_next", f->extfh, file,
					 f->file_status, cb_int (read_opts)));
			} else {
				cb_emit (CB_BUILD_FUNCALL_3 ("cob_read_next", file,
					 f->file_status, cb_int (read_opts)));
			}
		}
	}
	if (into) {
		current_statement->handler3 = cb_build_move (rec, into);
	}

	/* Check for file debugging */
	if (current_program->flag_debugging
	 && !current_statement->flag_in_debug
	 && f->flag_fl_debug) {
		if (into) {
			current_statement->handler3 =
				CB_LIST_INIT (current_statement->handler3);
		}
		x = cb_build_debug (cb_debug_name, f->name, NULL);
		current_statement->handler3 =
			cb_list_add (current_statement->handler3, x);
		x = cb_build_move (rec, cb_debug_contents);
		current_statement->handler3 =
			cb_list_add (current_statement->handler3, x);
		x = cb_build_debug_call (f->debug_section);
		current_statement->handler3 =
			cb_list_add (current_statement->handler3, x);
	}
	current_statement->file = file;
}

/* READY TRACE statement */

void
cb_emit_ready_trace (void)
{
	cb_emit (CB_BUILD_FUNCALL_0 ("cob_ready_trace"));
}


/* RESET TRACE statement */

void
cb_emit_reset_trace (void)
{
	cb_emit (CB_BUILD_FUNCALL_0 ("cob_reset_trace"));
}

/* REWRITE statement */

static int
error_if_invalid_file_from_clause_literal (cb_tree literal)
{
	enum cb_category	category = CB_TREE_CATEGORY (literal);

	if (cb_relaxed_syntax_checks || !(CB_CONST_P (literal) || CB_LITERAL_P (literal))) {
		return 0;
	}

	if (cb_is_figurative_constant (literal)) {
		cb_error_x (literal, _("figurative constants not allowed in FROM clause"));
		return 1;
	}

	if (!(category == CB_CATEGORY_ALPHANUMERIC
	   || category == CB_CATEGORY_NATIONAL
	   || category == CB_CATEGORY_BOOLEAN)) {
		cb_error_x (literal, _("literal in FROM clause must be alphanumeric, utf-8, national or boolean"));
		return 1;
	}

	return 0;
}

void
cb_emit_rewrite (cb_tree record, cb_tree from, cb_tree lockopt)
{
	cb_tree		file;
	cb_tree		rtree;
	struct cb_file	*f;
	int		opts;

	if (cb_validate_one (record)
	 || cb_validate_one (from)) {
		return;
	}
	rtree = cb_ref (record);
	if (CB_FILE_P (rtree)) {
		if (from == NULL) {
			cb_error_x (CB_TREE (current_statement),
				_("%s FILE requires a FROM clause"), "REWRITE");
			return;
		}
		file = rtree;		/* FILE filename: was used */
		f = CB_FILE (file);
		if (f->record->sister) {
			record = CB_TREE(f->record->sister);
		} else {
			record = CB_TREE(f->record);
		}

		if (error_if_invalid_file_from_clause_literal (from)) {
			return;
		}
	} else {
		if (!CB_REF_OR_FIELD_P (rtree)) {
			cb_error_x (CB_TREE (current_statement),
				_("%s requires a record name as subject"), "REWRITE");
			return;
		}
		if (CB_FIELD_PTR (record)->storage != CB_STORAGE_FILE) {
			cb_error_x (CB_TREE (current_statement),
				_("%s subject does not refer to a record name"), "REWRITE");
			return;
		}

		file = CB_TREE (CB_FIELD (rtree)->file);
		if (!file || file == cb_error_node) {
			return;
		}
	}
	current_statement->file = file;
	f = CB_FILE (file);
	opts = 0;

	if (cb_listing_xref) {
		/* add a "receiving" entry for the file */
		cobc_xref_link (&f->xref, current_statement->common.source_line, 1);
	}

	if (f->organization == COB_ORG_SORT) {
		cb_error_x (CB_TREE (current_statement),
				_("%s not allowed on %s files"), "REWRITE", "SORT");
		return;
	} else if (f->reports) {
		cb_error_x (CB_TREE (current_statement),
				_("%s not allowed on %s files"), "REWRITE", "REPORT");
		return;
	} else if (current_statement->handler_type == INVALID_KEY_HANDLER &&
		  (f->organization != COB_ORG_RELATIVE &&
		   f->organization != COB_ORG_INDEXED)) {
			cb_error_x (CB_TREE(current_statement),
			_("INVALID KEY clause invalid with this file type"));
		return;
	} else if ((f->lock_mode & COB_LOCK_AUTOMATIC) && lockopt) {
		cb_error_x (CB_TREE (current_statement),
		_("LOCK clause invalid with file LOCK AUTOMATIC"));
		return;
	} else if (lockopt == cb_int1) {
		opts = COB_WRITE_LOCK;
	} else if (lockopt == cb_int2) {
		opts = COB_WRITE_NO_LOCK;
	}

	if (from && (!CB_FIELD_P(from) || (CB_FIELD_PTR (from) != CB_FIELD_PTR (record)))) {
		cb_emit (cb_build_move (from, record));
	}

	/* Check debugging on record name */
	if (current_program->flag_debugging &&
	    !current_statement->flag_in_debug &&
	    CB_FIELD_PTR (record)->flag_field_debug) {
		cb_emit (cb_build_debug (cb_debug_name,
					 CB_FIELD_PTR (record)->name, NULL));
		cb_emit (cb_build_move (record, cb_debug_contents));
		cb_emit (cb_build_debug_call (CB_FIELD_PTR (record)->debug_section));
	}
	if (f->extfh) {
		cb_emit (CB_BUILD_FUNCALL_5 ("cob_extfh_rewrite", f->extfh, file, record,
				cb_int (opts), f->file_status));
	} else {
		cb_emit (CB_BUILD_FUNCALL_4 ("cob_rewrite", file, record,
				cb_int (opts), f->file_status));
	}
}

/* RELEASE statement */

void
cb_emit_release (cb_tree record, cb_tree from)
{
	struct cb_field	*f;
	cb_tree		file;

	if (cb_validate_one (record)) {
		return;
	}
	if (cb_validate_one (from)) {
		return;
	}
	if (!CB_REF_OR_FIELD_P (cb_ref (record))) {
		cb_error_x (CB_TREE (current_statement),
			_("%s requires a record name as subject"), "RELEASE");
		return;
	}
	f = CB_FIELD_PTR (record);
	if (f->storage != CB_STORAGE_FILE) {
		cb_error_x (CB_TREE (current_statement),
			_("%s subject does not refer to a record name"), "RELEASE");
		return;
	}
	file = CB_TREE (f->file);
	if (CB_FILE (file)->organization != COB_ORG_SORT) {
		cb_error_x (CB_TREE (current_statement),
			_("RELEASE not allowed on this record item"));
		return;
	}
	current_statement->file = file;
	if (from) {
		cb_emit (cb_build_move (from, record));
	}
	cb_emit (CB_BUILD_FUNCALL_1 ("cob_file_release", file));
}

/* RETURN statement */

void
cb_emit_return (cb_tree ref, cb_tree into)
{
	cb_tree		file;
	cb_tree		rec;

	if (cb_validate_one (ref)
	 || cb_validate_one (into)) {
		return;
	}
	file = cb_ref (ref);
	if (file == cb_error_node) {
		return;
	}
	rec = cb_build_field_reference (CB_FILE (file)->record, ref);
	cb_emit (CB_BUILD_FUNCALL_1 ("cob_file_return", file));
	if (into) {
		current_statement->handler3 = cb_build_move (rec, into);
	}
	current_statement->file = file;
}

/* ROLLBACK statement */

void
cb_emit_rollback (void)
{
	cb_emit (CB_BUILD_FUNCALL_0 ("cob_rollback"));
}

/* SEARCH statement */

/* SEARCH ALL with the given key */
static unsigned int
search_set_keys (struct cb_field *f, cb_tree x)
{
	struct cb_binary_op	*p;
	struct cb_field		*fldx;
	struct cb_field		*fldy;
	int			i;

	if (CB_REFERENCE_P (x)) {
		x = build_cond_88 (x);
		if (!x || x == cb_error_node) {
			return 1;
		}
	}

	p = CB_BINARY_OP (x);
	switch (p->op) {
	case '&':
		if (search_set_keys (f, p->x)) {
			return 1;
		}
		if (search_set_keys (f, p->y)) {
			return 1;
		}
		break;
	case '=':
		fldx = NULL;
		fldy = NULL;
		/* One of the operands must be a key reference */
		if (CB_REF_OR_FIELD_P (p->x)) {
			fldx = CB_FIELD_PTR (p->x);
		}
		if (CB_REF_OR_FIELD_P (p->y)) {
			fldy = CB_FIELD_PTR (p->y);
		}
		if (!fldx && !fldy) {
			cb_error_x (CB_TREE (current_statement),
				    _("invalid SEARCH ALL condition"));
			return 1;
		}

		for (i = 0; i < f->nkeys; ++i) {
			if (fldx == CB_FIELD_PTR (f->keys[i].key)) {
				/* TODO: detach bound check here, but not for  KEY (IDX(other)) */
				f->keys[i].ref = p->x;
				f->keys[i].val = p->y;
				break;
			}
		}
		if (i == f->nkeys) {
			for (i = 0; i < f->nkeys; ++i) {
				if (fldy == CB_FIELD_PTR (f->keys[i].key)) {
					f->keys[i].ref = p->y;
					f->keys[i].val = p->x;
					break;
				}
			}
			if (i == f->nkeys) {
				cb_error_x (CB_TREE (current_statement),
					    _("invalid SEARCH ALL condition"));
				return 1;
			}
		}
		break;
	default:
		cb_error_x (CB_TREE (current_statement),
			    _("invalid SEARCH ALL condition"));
		return 1;
	}
	return 0;
}

static cb_tree
cb_build_search_all (cb_tree table, cb_tree cond)
{
	cb_tree		c1;
	cb_tree		c2;
	struct cb_field	*f;
	int		i;

	f = CB_FIELD_PTR (table);
	/* Set keys */
	for (i = 0; i < f->nkeys; i++) {
		f->keys[i].ref = NULL;
	}
	if (search_set_keys (f, cond)) {
		return NULL;
	}
	c1 = NULL;

	/* Build condition */
	for (i = 0; i < f->nkeys; i++) {
		if (f->keys[i].ref) {
			if (f->keys[i].dir == COB_ASCENDING) {
				c2 = cb_build_binary_op (f->keys[i].ref, '=',
							 f->keys[i].val);
			} else {
				c2 = cb_build_binary_op (f->keys[i].val, '=',
							 f->keys[i].ref);
			}
			if (c1 == NULL) {
				c1 = c2;
			} else {
				c1 = cb_build_binary_op (c1, '&', c2);
			}
		}
	}

	if (!c1) {
		return NULL;
	}
	return cb_build_cond (c1);
}

void
cb_search_ready (const cb_tree table)
{
	struct cb_field	*f;
	if (table == NULL) {
		check_search_table = NULL;
		check_search_index = NULL;
		return;
	}
	f = cb_code_field (table);
	if(f->index_list) {
		check_search_table = f;
		check_search_index = cb_code_field (CB_VALUE (f->index_list));
	} else {
		check_search_table = NULL;
		check_search_index = NULL;
	}
}

cb_tree
cb_emit_search (cb_tree table, cb_tree varying, cb_tree at_end, cb_tree whens)
{
	cb_tree		x;

	if (cb_validate_one (table)
	 || cb_validate_one (varying)
	 || whens == cb_error_node) {
		return NULL;
	}

	whens = cb_list_reverse (whens);
	if (at_end) {
		cb_check_needs_break (CB_PAIR_Y (at_end));
	}
	x = cb_emit (cb_build_search (0, table, varying, at_end, whens));
	cb_search_ready (NULL);
	return x;
}

cb_tree
cb_emit_search_all (cb_tree table, cb_tree at_end, cb_tree when, cb_tree stmts)
{
	cb_tree		x;
	cb_tree		stmt_lis;

	if (cb_validate_one (table)
	 || when == cb_error_node) {
		return NULL;
	}
	x = cb_build_search_all (table, when);
	if (!x) {
		return NULL;
	}

	stmt_lis = cb_check_needs_break (stmts);
	if (at_end) {
		cb_check_needs_break (CB_PAIR_Y (at_end));
	}
	x = cb_build_if (x, stmt_lis, NULL, STMT_WHEN);
	x = cb_emit (cb_build_search (1, table, NULL, at_end, x));
	cb_search_ready (NULL);
	return x;
}

/* SET statement */

void
cb_emit_setenv (cb_tree x, cb_tree y)
{
	cb_emit (CB_BUILD_FUNCALL_2 ("cob_set_environment", x, y));
}

static void
cb_emit_check_index (cb_tree vars, int hasval, int setval)
{
	cb_tree		l, v;
	struct cb_field *f, *p;
	for (l = vars; l; l = CB_CHAIN (l)) {
		v = CB_VALUE (l);
		if (!CB_REF_OR_FIELD_P (v)) continue;
		f = CB_FIELD_PTR (v);
		if (!f->flag_indexed_by) continue;
		if (!f->index_qual) continue;
		p = f->index_qual;
		if (p->depending) {
			if (hasval) {
				if (setval > p->occurs_max
				 || setval < p->occurs_min) {
					cb_warning_x (COBC_WARN_FILLER, l,
							_("SET %s TO %d is out of bounds"), f->name, setval);
#if 0 /* FIXME: add back as option, because not conforming to ISO */
					cb_emit (CB_BUILD_FUNCALL_1 ("cob_set_exception", cb_int (COB_EC_RANGE_INDEX)));
#endif
				}
				if (setval >= p->occurs_min) continue;
			}
		} else if (hasval
				&& setval >= p->occurs_min
				&& setval <= p->occurs_max) {
			continue;	/* Checks OK at compile time */
		} else {
			if (hasval) {
				cb_warning_x (COBC_WARN_FILLER, l,
						_("SET %s TO %d is out of bounds"), f->name, setval);
			}
		}
	}
}

static int
cb_check_set_to (cb_tree vars, cb_tree x, const int emit_error)
{
	cb_tree		l;
	cb_tree		v;
	cb_tree		rtree;
	struct cb_cast	*p;
	enum cb_class	tree_class;
	int		error_found = 0;

	if (cb_validate_one (x)
	 || cb_validate_list (vars)) {
		return 1;
	}

	/* Check PROGRAM-POINTERs are the target for SET ... TO ENTRY. */
	if (CB_CAST_P (x)
	 && CB_CAST (x)->cast_type == CB_CAST_PROGRAM_POINTER) {
		for (l = vars; l; l = CB_CHAIN (l)) {
			v = CB_VALUE (l);
			if (!CB_REFERENCE_P (v)) {
				if (emit_error) {
					cb_error_x (CB_TREE (current_statement),
						    _("SET targets must be PROGRAM-POINTER"));
				}
				CB_VALUE (l) = cb_error_node;
				error_found = 1;
			} else if (CB_FIELD(cb_ref(v))->usage != CB_USAGE_PROGRAM_POINTER) {
				if (emit_error) {
					cb_error_x (CB_TREE (current_statement),
						    _("SET targets must be PROGRAM-POINTER"));
				}
				CB_VALUE (l) = cb_error_node;
				error_found = 1;
			}
		}
	}

	cb_check_list (vars);

	/* Check ADDRESS OF targets can be modified and for class. */
	for (l = vars; l; l = CB_CHAIN (l)) {
		v = CB_VALUE (l);
		if (!CB_CAST_P (v)
		 || CB_CAST (v)->cast_type != CB_CAST_ADDRESS) {
			continue;
		}
		tree_class = cb_tree_class (CB_VALUE (l));
		switch (tree_class) {
		case CB_CLASS_INDEX:
		case CB_CLASS_NUMERIC:
		case CB_CLASS_POINTER:
			/* all fine */
			break;
		default:
			if (CB_VALUE (l) != cb_error_node) {
				cb_error_x (CB_TREE (current_statement),
					    _("SET target '%s' is not numeric, an INDEX or a POINTER"),
					    cb_name (CB_VALUE(l)));
				error_found = 1;
			}
		}
		p = CB_CAST (v);
		rtree = cb_ref (p->val);
		/* LCOV_EXCL_START */
		if (rtree == cb_error_node) {
			cobc_err_msg (_("call to '%s' with invalid parameter '%s'"),
				"cb_emit_set_to", "vars");
			COBC_ABORT ();
		}
		/* LCOV_EXCL_STOP */
		if (CB_FIELD (rtree)->level != 1
		 && CB_FIELD (rtree)->level != 77) {
			if (emit_error) {
				cb_error_x (p->val, _("cannot change address of '%s', which is not level 1 or 77"),
					    cb_name (p->val));
				CB_VALUE (l) = cb_error_node;
			}
			error_found = 1;
		} else if (!CB_FIELD (rtree)->flag_base) {
			if (emit_error) {
				cb_error_x (p->val, _("cannot change address of '%s', which is not BASED or a LINKAGE item"),
					    cb_name (p->val));
				CB_VALUE (l) = cb_error_node;
			}
			error_found = 1;
		}
	}
	return error_found;
}

void
cb_emit_set_to (cb_tree vars, cb_tree src)
{
	cb_tree	l;
	int			hasval, setval;

	/* Emit statements only if targets have the correct class. */
	if (cb_check_set_to (vars, src, 1)) {
		return;
	}

	/* Validate source, if requested. */
	cb_emit_incompat_data_checks (src);

	/* Emit statements. */
	for (l = vars; l; l = CB_CHAIN (l)) {
		cb_emit (cb_build_move (src, CB_VALUE (l)));
	}

	hasval = setval = 0;
	if (CB_LITERAL_P (src)) {
		if (CB_NUMERIC_LITERAL_P (src)) {
			setval = cb_get_int (src);
			hasval = 1;
		}
	} else if (src == cb_zero) {
		hasval = 1;
	}
	if (CB_EXCEPTION_ENABLE (COB_EC_BOUND_SUBSCRIPT)) {
		cb_emit_check_index (vars, hasval, setval);
	}
}

/*
 * SET pointer TO ADDRESS OF FH--FCD OF filename
 */
void
cb_emit_set_to_fcd (cb_tree vars, cb_tree x)
{
	cb_tree		l;
	cb_tree		v;
	cb_tree		rtree;
	cb_tree		file;
	struct cb_cast	*p;
	enum cb_class	tree_class;

	if (cb_validate_one (x)
	 || cb_validate_list (vars)) {
		return;
	}

	/* Check ADDRESS OF targets can be modified. */
	for (l = vars; l; l = CB_CHAIN (l)) {
		v = CB_VALUE (l);
		if (!CB_CAST_P (v)) {
			continue;
		}
		p = CB_CAST (v);
		if (p->cast_type != CB_CAST_ADDRESS) {
			continue;
		}
		rtree = cb_ref (p->val);
		if (rtree == cb_error_node) {
			cobc_err_msg (_("call to '%s' with invalid parameter '%s'"),
				"cb_emit_set_to_fcd", "vars");
			COBC_ABORT ();
		}
		if (CB_FIELD (rtree)->level != 1
		 && CB_FIELD (rtree)->level != 77) {
			cb_error_x (p->val, _("cannot change address of '%s', which is not level 1 or 77"),
				    cb_name (p->val));
			CB_VALUE (l) = cb_error_node;
		} else if (!CB_FIELD (rtree)->flag_base) {
			cb_error_x (p->val, _("cannot change address of '%s', which is not BASED or a LINKAGE item"),
				    cb_name (p->val));
			CB_VALUE (l) = cb_error_node;
		}
	}

	file = cb_ref (x);
	if (file == cb_error_node) {
		return;
	}

	/* Emit statements if targets have the correct class. */
	for (l = vars; l; l = CB_CHAIN (l)) {
		tree_class = cb_tree_class (CB_VALUE (l));
		switch (tree_class) {
		case CB_CLASS_POINTER:
			cb_emit (CB_BUILD_FUNCALL_2 ("cob_file_fcd_adrs", file, cb_build_address (CB_VALUE (l))));
			break;
		default:
			if (CB_VALUE (l) != cb_error_node) {
				cb_error_x (CB_TREE (current_statement),
					    _("SET target '%s' is not a POINTER for FCD"), cb_name (CB_VALUE(l)));
			}
			break;
		}
	}
}

/*
 * SET pointer TO ADDRESS OF FH--KEYDEF OF filename
 */
void
cb_emit_set_to_fcdkey (cb_tree vars, cb_tree x)
{
	cb_tree		l;
	cb_tree		v;
	cb_tree		rtree;
	cb_tree		file;
	struct cb_cast	*p;
	enum cb_class	tree_class;

	if (cb_validate_one (x)
	 || cb_validate_list (vars)) {
		return;
	}

	/* Check ADDRESS OF targets can be modified. */
	for (l = vars; l; l = CB_CHAIN (l)) {
		v = CB_VALUE (l);
		if (!CB_CAST_P (v)) {
			continue;
		}
		p = CB_CAST (v);
		if (p->cast_type != CB_CAST_ADDRESS) {
			continue;
		}
		rtree = cb_ref (p->val);
		if (rtree == cb_error_node) {
			cobc_err_msg (_("call to '%s' with invalid parameter '%s'"),
				"cb_emit_set_to_fcd", "vars");
			COBC_ABORT ();
		}
		if (CB_FIELD (rtree)->level != 1
		 && CB_FIELD (rtree)->level != 77) {
			cb_error_x (p->val, _("cannot change address of '%s', which is not level 1 or 77"),
				    cb_name (p->val));
			CB_VALUE (l) = cb_error_node;
		} else if (!CB_FIELD (rtree)->flag_base) {
			cb_error_x (p->val, _("cannot change address of '%s', which is not BASED or a LINKAGE item"),
				    cb_name (p->val));
			CB_VALUE (l) = cb_error_node;
		}
	}

	file = cb_ref (x);
	if (file == cb_error_node) {
		return;
	}

	/* Emit statements if targets have the correct class. */
	for (l = vars; l; l = CB_CHAIN (l)) {
		cb_tree target = CB_VALUE (l);
		tree_class = cb_tree_class (target);
		switch (tree_class) {
		case CB_CLASS_POINTER:
			cb_emit (CB_BUILD_FUNCALL_2 ("cob_file_fcdkey_adrs", file, cb_build_address (target)));
			break;
		default:
			if (CB_VALUE (l) != cb_error_node) {
				cb_error_x (CB_TREE (current_statement),
					    _("SET target '%s' is not a POINTER for FCD-KEYDEF"), cb_name (target));
			}
			break;
		}
	}
}
void
cb_emit_set_up_down (cb_tree l, cb_tree flag, cb_tree x)
{
	cb_tree vars = l;
	if (cb_validate_one (x)
	 || cb_validate_list (l)) {
		return;
	}
	cb_check_list (l);
	for (; l; l = CB_CHAIN (l)) {
		cb_tree target = CB_VALUE (l);
		if (flag == cb_int0) {
			cb_emit (cb_build_add (target, x, cb_int0));
		} else {
			cb_emit (cb_build_sub (target, x, cb_int0));
		}
	}
	if (CB_EXCEPTION_ENABLE (COB_EC_RANGE_INDEX)) {
		cb_emit_check_index (vars, 0, 0);
	}
}

void
cb_emit_set_on_off (cb_tree l, cb_tree flag)
{
	struct cb_system_name *s;

	if (cb_validate_list (l)) {
		return;
	}
	for (; l; l = CB_CHAIN (l)) {
		s = CB_SYSTEM_NAME (cb_ref (CB_VALUE (l)));
		cb_emit (CB_BUILD_FUNCALL_2 ("cob_set_switch",
					     cb_int (s->token), flag));
	}
}

void
cb_emit_set_true (cb_tree l)
{
	cb_tree		x;
	struct cb_field *f;
	cb_tree		ref;
	cb_tree		val;

	for (; l; l = CB_CHAIN (l)) {
		x = CB_VALUE (l);
		if (x == cb_error_node) {
			return;
		}
		if (!(CB_REFERENCE_P (x) && CB_FIELD_P(CB_REFERENCE(x)->value)) &&
		    !CB_FIELD_P (x)) {
			cb_error_x (x, _("invalid SET statement"));
			return;
		}
		f = CB_FIELD_PTR (x);
		if (f->level != 88) {
			cb_error_x (x, _("invalid SET statement"));
			return;
		}
		ref = cb_build_field_reference (f->parent, x);
		val = CB_VALUE (f->values);
		if (CB_PAIR_P (val)) {
			val = CB_PAIR_X (val);
		}
		cb_emit (cb_build_move (val, ref));
	}
}

void
cb_emit_set_false (cb_tree l)
{
	struct cb_field *f;
	cb_tree		ref;
	cb_tree		val;
	cb_tree		x;

	for (; l; l = CB_CHAIN (l)) {
		x = CB_VALUE (l);
		if (x == cb_error_node) {
			return;
		}
		if (!(CB_REFERENCE_P (x) && CB_FIELD_P(CB_REFERENCE(x)->value)) &&
		    !CB_FIELD_P (x)) {
			cb_error_x (x, _("invalid SET statement"));
			return;
		}
		f = CB_FIELD_PTR (x);
		if (f->level != 88) {
			cb_error_x (x, _("invalid SET statement"));
			return;
		}
		if (!f->false_88) {
			cb_error_x (x, _("field does not have a FALSE clause"));
			return;
		}
		ref = cb_build_field_reference (f->parent, x);
		val = CB_VALUE (f->false_88);
		if (CB_PAIR_P (val)) {
			val = CB_PAIR_X (val);
		}
		cb_emit (cb_build_move (val, ref));
	}
}

void
cb_emit_set_thread_priority (cb_tree handle, cb_tree priority)
{
	cb_tree used_handle;

	if (handle && handle != cb_null && !usage_is_thread_handle (handle)) {
		cb_error_x (handle, _("HANDLE must be either a generic or a THREAD HANDLE"));
		return;
	}
	used_handle = handle;
	if (used_handle && used_handle == cb_null) {
		used_handle = CB_BUILD_FUNCALL_1 ("cob_get_threadhandle", NULL);
	}

	if (cb_validate_one (priority)) {
		return;
	}
	if (CB_LITERAL_P (priority)) {
		if (cb_get_int (priority) > 32767) {
			cb_error (_("THREAD-priority must be between 1 and 32767"));
		}
	}
#if 0 /* TODO: implement THREADs in libcob */
	cb_emit (CB_BUILD_FUNCALL_2 ("set_thread_priority",
			used_handle, cb_build_cast_int (priority)));
#endif
}

void
cb_emit_set_attribute (cb_tree x, const cob_flags_t val_on,
		       const cob_flags_t val_off)
{
	struct cb_field		*f;

	if (cb_validate_one (x)) {
		return;
	}
	if (!CB_REF_OR_FIELD_P (cb_ref (x))) {
		cb_error_x (CB_TREE (current_statement),
			_("SET ATTRIBUTE requires a screen item as subject"));
		return;
	}
	f = CB_FIELD_PTR (x);
	if (f->storage != CB_STORAGE_SCREEN) {
		cb_error_x (CB_TREE (current_statement),
			_("SET ATTRIBUTE subject does not refer to a screen item"));
		return;
	}
	cb_emit (cb_build_set_attribute (f, val_on, val_off));
}

void
cb_emit_set_last_exception_to_off (void)
{
	cb_emit (CB_BUILD_FUNCALL_1 ("cob_set_exception", cb_int0));
}

/* SORT + MERGE statements */

void
cb_emit_sort_init (cb_tree name, cb_tree keys, cb_tree col, cb_tree nat_col)
{
	cb_tree			l;
	cb_tree			rtree;

	if (cb_validate_list (keys)) {
		return;
	}
	rtree = cb_ref (name);
	if (rtree == cb_error_node) {
		return;
	}
	for (l = keys; l; l = CB_CHAIN (l)) {
		if (CB_VALUE (l) == NULL) {
			CB_VALUE (l) = name;
		}
	}

	/* note: the reference to the program's collation,
	   if not explicit specified in SORT/MERGE, is done within libcob */
	if (col == NULL) {
		col = cb_null;
	} else {
		col = cb_ref (col);
	}
	if (nat_col == NULL) {
		nat_col = cb_null;
	} else {
		nat_col = cb_ref (nat_col);
	}
	/* TODO: pass national collation to libcob */
	COB_UNUSED (nat_col);

	if (CB_FILE_P (rtree)) {
		cb_tree sort_return;
		const struct cb_file *sd_file = CB_FILE (rtree);
		if (current_program->cb_sort_return) {
			CB_FIELD_PTR (current_program->cb_sort_return)->count++;
			sort_return = CB_BUILD_CAST_ADDRESS (current_program->cb_sort_return);
		} else {
			sort_return = cb_null;
		}
		cb_emit (CB_BUILD_FUNCALL_5 ("cob_file_sort_init", rtree,
						    cb_int ((int)cb_list_length (keys)), col,
						    sort_return, sd_file->file_status));
		if (current_statement->statement == STMT_MERGE) {
			/* note: this  function can be used later to set more options */
			cb_emit (CB_BUILD_FUNCALL_2 ("cob_file_sort_options", rtree,
				cb_build_string (cobc_parse_strdup ("M"), 1)));
		}
		/* TODO: pass key-specific collation to libcob */
		for (l = keys; l; l = CB_CHAIN (l)) {
			cb_tree fref = CB_VALUE (l);
			cb_emit (CB_BUILD_FUNCALL_4 ("cob_file_sort_init_key",
						     rtree,
						     fref,
						     CB_PURPOSE (l),
						     cb_int (CB_FIELD_PTR (fref)->offset)));
		}
	} else {
		struct cb_field	* const fr = CB_FIELD (rtree);
		cb_emit (CB_BUILD_FUNCALL_2 ("cob_table_sort_init",
					     cb_int ((int)cb_list_length (keys)), col));
		/* TODO: pass key-specific collation to libcob */
		for (l = keys; l; l = CB_CHAIN (l)) {
			struct cb_field * const f = CB_FIELD_PTR (CB_VALUE(l));
			cb_emit (CB_BUILD_FUNCALL_3 ("cob_table_sort_init_key",
						     CB_VALUE (l),
						     CB_PURPOSE (l),
						     cb_int(f->offset -
							    (f->parent ? f->parent->offset : 0))));
		}
		cb_emit (CB_BUILD_FUNCALL_2 ("cob_table_sort", name,
					     (fr->depending
					      ? cb_build_cast_int (fr->depending)
					      : cb_int (fr->occurs_max))));
	}
}

void
cb_emit_sort_using (cb_tree file, cb_tree l)
{
	cb_tree rtree;

	if (cb_validate_list (l)) {
		return;
	}
	rtree = cb_ref (file);
	/* LCOV_EXCL_START */
	if (rtree == cb_error_node) {
		cobc_err_msg (_("call to '%s' with invalid parameter '%s'"),
			"cb_emit_sort_using", "file");
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */
	for (; l; l = CB_CHAIN (l)) {
		cb_tree use_ref = cb_ref (CB_VALUE (l));
		const struct cb_file *use_file = CB_FILE (use_ref);
		if (use_file->organization == COB_ORG_SORT) {
			cb_error_x (CB_TREE (current_statement),
				    _("invalid %s parameter"),
					current_statement->statement == STMT_MERGE ?
					"MERGE USING" : "SORT USING");
		}
		if (use_file->extfh) {
			cb_emit (CB_BUILD_FUNCALL_3 ("cob_file_sort_using_extfh",
				rtree, use_ref, use_file->extfh));
		} else {
			cb_emit (CB_BUILD_FUNCALL_2 ("cob_file_sort_using",
				rtree, use_ref));
		}
	}
}

void
cb_emit_sort_input (cb_tree proc)
{
	if (current_program->flag_debugging
	 && !current_statement->flag_in_debug) {
		cb_emit (cb_build_debug (cb_debug_contents, "SORT INPUT", NULL));
	}
	cb_emit (cb_build_perform_once (proc));
}

void
cb_emit_sort_giving (cb_tree sd_file, cb_tree l)
{
	cb_tree		p;
	cb_tree		extfh_list = NULL;
	int 	has_extfh = 0;
	const char *file_sort_giving_func;

	if (cb_validate_list (l)) {
		return;
	}
	for (p = l; p; p = CB_CHAIN (p)) {
		/* TODO: let parser create a list of files, not their references */
		const struct cb_file *giving_file = CB_FILE (cb_ref (CB_VALUE (p)));
		if (giving_file->organization == COB_ORG_SORT) {
			cb_error_x (CB_TREE (current_statement),
				    _("invalid %s parameter"),
					current_statement->statement == STMT_MERGE ?
					"MERGE GIVING" : "SORT GIVING");

		}
		extfh_list = cb_list_add (extfh_list, CB_TREE (giving_file));
		cb_list_add (extfh_list, giving_file->extfh);
		has_extfh += (giving_file->extfh != NULL);
	}
	p = cb_ref (sd_file);
	/* LCOV_EXCL_START */
	if (p == cb_error_node) {
		cobc_err_msg (_("call to '%s' with invalid parameter '%s'"),
			"cb_emit_sort_giving", "sd_file");
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */
	if (has_extfh) {
		file_sort_giving_func = "cob_file_sort_giving_extfh";
		l = extfh_list;
	} else {
		file_sort_giving_func = "cob_file_sort_giving";
	}
	p = CB_BUILD_FUNCALL_2 (file_sort_giving_func, p, l);
	CB_FUNCALL(p)->varcnt = cb_list_length (l);
	cb_emit (p);
}

void
cb_emit_sort_output (cb_tree proc)
{
	if (current_program->flag_debugging
	 && !current_statement->flag_in_debug) {
		if (current_statement->statement == STMT_MERGE) {
			cb_emit (cb_build_debug (cb_debug_contents,
						 "MERGE OUTPUT", NULL));
		} else {
			cb_emit (cb_build_debug (cb_debug_contents,
						 "SORT OUTPUT", NULL));
		}
	}
	cb_emit (cb_build_perform_once (proc));
}

void
cb_emit_sort_finish (cb_tree file)
{
	if (CB_FILE_P (cb_ref (file))) {
		cb_emit (CB_BUILD_FUNCALL_1 ("cob_file_sort_close", cb_ref (file)));
	}
}

/* START statement */

static unsigned int
check_valid_key (const struct cb_file *cbf, const struct cb_field *f)
{
	cb_tree			kfld;
	struct cb_alt_key	*cbak;
	struct cb_field		*f1;
	struct cb_field		*ff;

	if (cbf->organization != COB_ORG_INDEXED) {
		if (CB_FIELD_PTR (cbf->key) != f) {
			cb_error_x (CB_TREE (current_statement),
				    _("invalid key item"));
			return 1;
		}
		return 0;
	}

	/*
	 *  Pass if field f refs a declared key for target file.
	 *  This will pass split-keys which are virtual record fields.
	 */
	for (cbak = cbf->alt_key_list; cbak; cbak = cbak->next) {
		if (CB_FIELD_PTR (cbak->key) == f) {
			return 0;
		}
	}
	if (cbf->component_list != NULL
	 && CB_FIELD_PTR (cbf->key) == f) {
		return 0;
	}

	ff = cb_field_founder (f);
	for (f1 = cbf->record; f1; f1 = f1->sister) {
		if (f1 == ff) {
			break;
		}
	}
	if (!f1) {
		cb_error_x (CB_TREE (current_statement), _("invalid key item"));
		return 1;
	}

	kfld = cb_ref (cbf->key);
	if (kfld == cb_error_node) {
		return 1;
	}
	if (f->offset == CB_FIELD_PTR (kfld)->offset) {
		return 0;
	}
	for (cbak = cbf->alt_key_list; cbak; cbak = cbak->next) {
		kfld = cb_ref (cbak->key);
		if (kfld == cb_error_node) {
			return 1;
		}
		if (f->offset == CB_FIELD_PTR (kfld)->offset) {
			return 0;
		}
	}
	cb_error_x (CB_TREE (current_statement), _("invalid key item"));
	return 1;
}

void
cb_emit_start (cb_tree file, cb_tree op, cb_tree key, cb_tree keylen)
{
	cb_tree			kfld;
	cb_tree			fl;
	cb_tree			cbtkey;
	struct cb_file		*f;

	if (cb_validate_one (key)
	 || cb_validate_one (keylen)) {
		return;
	}
	fl = cb_ref (file);
	if (fl == cb_error_node) {
		return;
	}
	f = CB_FILE (fl);

	if (f->organization != COB_ORG_INDEXED &&
	    f->organization != COB_ORG_RELATIVE) {
		cb_error_x (CB_TREE (current_statement),
				_("%s not allowed on %s files"), "START", "SEQUENTIAL");
		return;
	}
	if (keylen && f->organization != COB_ORG_INDEXED) {
		cb_error_x (CB_TREE (current_statement),
			    _("LENGTH/SIZE clause only allowed on INDEXED files"));
		return;
	}
	if (f->access_mode == COB_ACCESS_RANDOM) {
		cb_error_x (CB_TREE (current_statement),
			    _("START not allowed with ACCESS MODE RANDOM"));
		return;
	}

	current_statement->file = fl;
	if (key) {
		kfld = cb_ref (key);
		if (kfld == cb_error_node) {
			return;
		}
		if (check_valid_key (f, CB_FIELD_PTR (kfld))) {
			return;
		}
		cbtkey = key;
	} else {
		cbtkey = f->key;
	}

	/* Check for file debugging */
	if (current_program->flag_debugging
	 && !current_statement->flag_in_debug
	 && f->flag_fl_debug) {
		/* Gen callback after start but before exception test */
		current_statement->flag_callback = 1;
	}

	if (f->extfh) {
		cb_emit (CB_BUILD_FUNCALL_6 ("cob_extfh_start", f->extfh, fl, op, cbtkey, keylen,
				     f->file_status));
	} else {
		cb_emit (CB_BUILD_FUNCALL_5 ("cob_start", fl, op, cbtkey, keylen,
				     f->file_status));
	}
}

/* STOP statement */

void
cb_emit_stop_run (cb_tree x)
{
	cb_emit (CB_BUILD_FUNCALL_1 ("cob_stop_run", cb_build_cast_int (x)));
}

void
cb_emit_stop_error (void)
{
	cb_emit (CB_BUILD_FUNCALL_0 ("cob_stop_error"));
}

void
cb_emit_stop_thread (cb_tree handle)
{
	cb_tree used_handle;

	if (handle && handle != cb_null && !usage_is_thread_handle (handle)) {
		cb_error_x (handle, _("HANDLE must be either a generic or a THREAD HANDLE"));
		return;
	}
	used_handle = handle;
	if (used_handle && used_handle == cb_null) {
		used_handle = CB_BUILD_FUNCALL_1 ("cob_get_threadhandle", NULL);
	}
#if 0 /* TODO: implement THREADs in libcob */
	cb_emit (CB_BUILD_FUNCALL_1 ("cob_stop_thread", used_handle));
#else
	cb_emit (CB_BUILD_FUNCALL_1 ("cob_stop_run", cb_int (0)));
#endif
}

/* STRING statement */

static int
field_can_contain_num (struct cb_field *f, int num)
{
	char	buff[COB_MINI_BUFF];
	cb_tree	num_lit;
	int	size;
	enum move_outcome	outcome;

	snprintf (buff, COB_MINI_BUFF, "%d", num);
	num_lit = cb_build_numeric_literal (0, buff, 0);
	outcome = validate_move_from_num_lit (num_lit, CB_TREE (f), 0, &size);

	return outcome == MOVE_OK;
}

static int
error_if_not_int_field_or_has_pic_p (const char *clause, cb_tree f)
{
	int		error = 0;
	enum cb_usage	usage;
	int		scale;

	if (!f) {
		return 0;
	}

	if (cb_validate_one (f)) {
		return 1;
	}

	usage = CB_FIELD_PTR (f)->usage;
	if (CB_TREE_CATEGORY (f) != CB_CATEGORY_NUMERIC
	 || is_floating_point_usage (usage)) {
		cb_error_x (f, _("%s item '%s' must be an integer"),
			    clause, CB_NAME (f));
		error = 1;
	} else if (CB_FIELD_PTR (f)->pic) {
		scale = CB_FIELD_PTR (f)->pic->scale;
		if (scale > 0) {
			cb_error_x (f, _("%s item '%s' must be an integer"),
				    clause, CB_NAME (f));
			error = 1;
		} else if (scale < 0) {
			cb_error_x (f, _("%s item '%s' may not have PICTURE with P in it"),
				    clause, CB_NAME (f));
			error = 1;
		}
	}

	return error;
}

/* Validate POINTER clause for STRING and UNSTRING */
static void
validate_pointer_clause (cb_tree pointer, cb_tree pointee)
{
	int	pointee_size = CB_FIELD_PTR (pointee)->size;
	struct cb_field	*pointer_field = CB_FIELD_PTR (pointer);

	if (pointer_field->children) {
		cb_error_x (pointee, _("'%s' is not an elementary item"),
			    CB_NAME (pointer));
		return;
	}
	if (error_if_not_int_field_or_has_pic_p ("POINTER", pointer)) {
		return;
	}
	if (!field_can_contain_num (pointer_field, pointee_size + 1)) {
		cb_error_x (pointer, _("'%s' is too small to contain the number %d (one plus the size of '%s')"),
			    CB_NAME (pointer), pointee_size + 1, CB_NAME (pointee));
	}
}

void
cb_emit_string (cb_tree items, cb_tree into, cb_tree pointer)
{

	cb_tree start;
	cb_tree l, cur;
	cb_tree end;
	cb_tree dlm;
	int		nat,nfld;
	struct cb_field	*f;

	if (cb_validate_one (into)
	 || cb_validate_one (pointer)) {
		return;
	}

	if (pointer) {
		validate_pointer_clause (pointer, into);
	}

	start = items;
	cb_emit (CB_BUILD_FUNCALL_2 ("cob_string_init", into, pointer));
	while (start) {
		/* Find next DELIMITED item */
		for (end = start; end; end = CB_CHAIN (end)) {
			if (CB_PAIR_P (CB_VALUE (end))) {
				break;
			}
		}

		/* generate cob_string_delimited from delimiter */
		dlm = end ? CB_PAIR_X (CB_VALUE (end)) : NULL;
		if (dlm == cb_int0) {
			dlm = NULL;
		} else {
			if (cb_validate_one (dlm)) {
				return;
			}
		}
		cb_emit (CB_BUILD_FUNCALL_1 ("cob_string_delimited", dlm));

		nat = nfld = 0;
		/* generate cob_string_append for all entries until delimiter */
		for (l = start; l != end; l = CB_CHAIN (l)) {
			cur = CB_VALUE (l);
			if (cb_validate_one (cur)) {
				return;
			}
			switch (CB_TREE_TAG (cur)) {
			case CB_TAG_REFERENCE:
				nfld++;
				f = CB_FIELD (cb_ref (cur));
	     		if (f->usage == CB_USAGE_NATIONAL)
					nat++;
				error_string_not_usage_display_or_national (cur);
				break;
			case CB_TAG_CONST:
			case CB_TAG_LITERAL:
				break;
			default:
				break;
			}
			cb_emit (CB_BUILD_FUNCALL_1 ("cob_string_append",
							 cur));
		}
		if (nat > 0 && nat != nfld)
			cb_error_x (CB_TREE (current_statement),
				_("STRING items must be all NATIONAL or none"));

		start = end ? CB_CHAIN (end) : NULL;
	}
	cb_emit (CB_BUILD_FUNCALL_0 ("cob_string_finish"));
}

/* UNLOCK statement */

void
cb_emit_unlock (cb_tree ref)
{
	cb_tree	file;

	file = cb_ref (ref);
	if (file != cb_error_node) {
		cb_emit (CB_BUILD_FUNCALL_2 ("cob_unlock_file",
			 file, CB_FILE(file)->file_status));
		current_statement->file = file;
	}
}

/* UNSTRING statement */

void
cb_emit_unstring (cb_tree name, cb_tree delimited, cb_tree into,
		  cb_tree pointer, cb_tree tallying)
{
	if (cb_validate_one (name)
	 || cb_validate_one (tallying)
	 || cb_validate_list (delimited)
	 || cb_validate_list (into)) {
		return;
	}
	if (pointer) {
		validate_pointer_clause (pointer, name);
	}

	cb_emit (CB_BUILD_FUNCALL_3 ("cob_unstring_init", name, pointer,
		cb_int ((int)cb_list_length (delimited))));
	cb_emit_list (delimited);
	cb_emit_list (into);
	if (tallying) {
		cb_emit (CB_BUILD_FUNCALL_1 ("cob_unstring_tallying", tallying));
	}
	cb_emit (CB_BUILD_FUNCALL_0 ("cob_unstring_finish"));
}

cb_tree
cb_build_unstring_delimited (cb_tree all, cb_tree value)
{
	if (cb_validate_one (value)) {
		return cb_error_node;
	}
	return CB_BUILD_FUNCALL_2 ("cob_unstring_delimited", value, all);
}

cb_tree
cb_build_unstring_into (cb_tree name, cb_tree delimiter, cb_tree count)
{
	if (cb_validate_one (name)) {
		return cb_error_node;
	}
	if (delimiter == NULL) {
		delimiter = cb_int0;
	}
	if (count == NULL
	 || error_if_not_int_field_or_has_pic_p ("COUNT", count)) {
		count = cb_int0;
	}
	return CB_BUILD_FUNCALL_3 ("cob_unstring_into", name, delimiter, count);
}

/* WRITE statement */

void
cb_emit_write (cb_tree record, cb_tree from, cb_tree opt, cb_tree lockopt)
{
	cb_tree		file;
	cb_tree		rtree;
	cb_tree		check_eop;
	struct cb_file	*f;

	if (cb_validate_one (record)
	 || cb_validate_one (from)) {
		return;
	}
	rtree = cb_ref (record);
	if (CB_FILE_P (rtree)) {
		/* FILE filename: was used */
		if (from == NULL) {
			cb_error_x (CB_TREE (current_statement),
				_("%s FILE requires a FROM clause"), "WRITE");
			return;
		}
		file = rtree;
		f = CB_FILE (file);
		if (f->record->sister) {
			record = CB_TREE(f->record->sister);
		} else {
			record = CB_TREE(f->record);
		}

		if (error_if_invalid_file_from_clause_literal (from)) {
			return;
		}
	} else {
		if (!CB_REF_OR_FIELD_P (rtree)) {
			cb_error_x (CB_TREE (current_statement),
				_("%s requires a record name as subject"), "WRITE");
			return;
		}
		if (CB_FIELD_PTR (record)->storage != CB_STORAGE_FILE) {
			cb_error_x (CB_TREE (current_statement),
				_("%s subject does not refer to a record name"), "WRITE");
			return;
		}
		file = CB_TREE (CB_FIELD (rtree)->file);
		if (!file || file == cb_error_node) {
			return;
		}
	}
	current_statement->file = file;
	f = CB_FILE (file);

	if (cb_listing_xref) {
		/* add a "receiving" entry for the file */
		cobc_xref_link (&f->xref, current_statement->common.source_line, 1);
	}

	if (f->organization == COB_ORG_SORT) {
		cb_error_x (CB_TREE (current_statement),
		_("%s not allowed on %s files"), "WRITE", "SORT");
	} else if (f->reports) {
		cb_error_x (CB_TREE (current_statement),
			    _("%s not allowed on %s files"), "WRITE", "REPORT");
		return;
	} else if (current_statement->handler_type == INVALID_KEY_HANDLER &&
		  (f->organization != COB_ORG_RELATIVE &&
		   f->organization != COB_ORG_INDEXED)) {
			cb_error_x (CB_TREE(current_statement),
			_("INVALID KEY clause invalid with this file type"));
	} else if (lockopt) {
		if (f->lock_mode & COB_LOCK_AUTOMATIC) {
			cb_error_x (CB_TREE (current_statement),
			_("LOCK clause invalid with file LOCK AUTOMATIC"));
		} else if (opt != cb_int0) {
			cb_error_x (CB_TREE (current_statement),
			_("LOCK clause invalid here"));
		} else if (lockopt == cb_int1) {
			opt = cb_int (COB_WRITE_LOCK);
		} else if (lockopt == cb_int2) {
			opt = cb_int (COB_WRITE_NO_LOCK);
		}
	}

	if (from && (!CB_FIELD_P(from) || (CB_FIELD_PTR (from) != CB_FIELD_PTR (record)))) {
		cb_emit (cb_build_move (from, record));
	}

	/* Check debugging on record name */
	if (current_program->flag_debugging &&
	    !current_statement->flag_in_debug &&
	    CB_FIELD_PTR (record)->flag_field_debug) {
		cb_emit (cb_build_debug (cb_debug_name,
					 CB_FIELD_PTR (record)->name, NULL));
		cb_emit (cb_build_move (record, cb_debug_contents));
		cb_emit (cb_build_debug_call (CB_FIELD_PTR (record)->debug_section));
	}
	if (f->organization == COB_ORG_SEQUENTIAL
	 &&  opt != cb_int0) {
		if (!f->flag_has_organization) {		/* No ORGANIZATION was used */
			if (cb_sequential_advancing == CB_IGNORE) {
				opt = cb_int0;		/* Ignore the ADVANCING clause */
			} else {
				cb_verify_x (CB_TREE (current_statement), cb_sequential_advancing,
							_("WRITE ADVANCING with default SEQUENTIAL file"));
				f->organization = COB_ORG_LINE_SEQUENTIAL;
			}
		} else {		/* Specifically used ORGANIZATION SEQUENTIAL */
			if (cb_sequential_advancing == CB_IGNORE) {
				opt = cb_int0;		/* Ignore the ADVANCING clause */
			} else {
				cb_verify_x (CB_TREE (current_statement), cb_sequential_advancing,
							_("WRITE ADVANCING with RECORD SEQUENTIAL file"));
			}
		}
	}
	if (f->organization == COB_ORG_LINE_SEQUENTIAL
	&&  opt == cb_int0) {
		/* Omission of ADVANCING default to
		 *   AFTER ADVANCING 1 LINE
		 */
		if (cb_flag_write_after			/* -fwrite-after */
		||  CB_FILE(file)->flag_line_adv) {
			opt = cb_int_hex (COB_WRITE_AFTER | COB_WRITE_LINES | 1);
		} else {
			opt = cb_int_hex (COB_WRITE_BEFORE | COB_WRITE_LINES | 1);
		}
	} else
	if (f->organization == COB_ORG_LINE_SEQUENTIAL
	 && opt != cb_int0
	 && cb_sequential_advancing == CB_OK
	 && cb_std_define == CB_STD_MF
	 && f->flag_line_adv == 0) {
		f->flag_line_adv = COB_LINE_ADVANCE;	/* Default to LINE ADVANCING */
	}
	if (current_statement->handler_type == EOP_HANDLER
	 && current_statement->ex_handler) {
		check_eop = cb_int1;
	} else {
		check_eop = cb_int0;
	}
	if (f->extfh) {
		cb_emit (CB_BUILD_FUNCALL_6 ("cob_extfh_write", f->extfh, file, record, opt,
					     f->file_status, check_eop));
	} else {
		cb_emit (CB_BUILD_FUNCALL_5 ("cob_write", file, record, opt,
					     f->file_status, check_eop));
	}
}

cb_tree
cb_build_write_advancing_lines (cb_tree pos, cb_tree lines)
{
	cb_tree	e;
	int	opt;

	opt = (pos == CB_BEFORE) ? COB_WRITE_BEFORE : COB_WRITE_AFTER;
	opt |= COB_WRITE_LINES;
	if (CB_LITERAL_P (lines)) {
		opt |= cb_get_int (lines);
		return cb_int_hex (opt);
	}
	e = cb_build_binary_op (cb_int (opt), '+', lines);
	return cb_build_cast_int (e);
}

cb_tree
cb_build_write_advancing_mnemonic (cb_tree pos, cb_tree mnemonic)
{
	int	opt;
	int	token;
	cb_tree rtree = cb_ref (mnemonic);

	if (rtree == cb_error_node) {
		return cb_int0;
	}
	token = CB_SYSTEM_NAME (rtree)->token;
	switch (token) {
	case CB_FEATURE_FORMFEED:	/* including S01-S05, CSP and TOP */
		opt = (pos == CB_BEFORE) ? COB_WRITE_BEFORE : COB_WRITE_AFTER;
		return cb_int_hex (opt | COB_WRITE_PAGE);
	case CB_FEATURE_C01:
	case CB_FEATURE_C02:
	case CB_FEATURE_C03:
	case CB_FEATURE_C04:
	case CB_FEATURE_C05:
	case CB_FEATURE_C06:
	case CB_FEATURE_C07:
	case CB_FEATURE_C08:
	case CB_FEATURE_C09:
	case CB_FEATURE_C10:
	case CB_FEATURE_C11:
	case CB_FEATURE_C12:
		opt = (pos == CB_BEFORE) ? COB_WRITE_BEFORE : COB_WRITE_AFTER;
		return cb_int_hex (opt | COB_WRITE_CHANNEL | COB_WRITE_PAGE | token);
	/* case CB_FEATURE_AFP_5A: what to do here? */
	default:
		cb_error_x (mnemonic, _("invalid mnemonic name"));
		return cb_int0;
	}
}

cb_tree
cb_build_write_advancing_page (cb_tree pos)
{
	int opt = (pos == CB_BEFORE) ? COB_WRITE_BEFORE : COB_WRITE_AFTER;

	return cb_int_hex (opt | COB_WRITE_PAGE);
}

#ifndef	HAVE_DESIGNATED_INITS
void
cobc_init_typeck (void)
{
	memset(expr_prio, 0, sizeof(expr_prio));
	expr_prio['x' & 0xFF] = 0;
	expr_prio['^' & 0xFF] = 1;
	expr_prio['*' & 0xFF] = 2;
	expr_prio['/' & 0xFF] = 2;
	expr_prio['+' & 0xFF] = 3;
	expr_prio['-' & 0xFF] = 3;
	expr_prio['a' & 0xFF] = 4;	/* B-AND */
	expr_prio['n' & 0xFF] = 5;	/* B-NOT */
	expr_prio['o' & 0xFF] = 4;	/* B-OR */
	expr_prio['e' & 0xFF] = 4;	/* B-XOR */
	expr_prio['l' & 0xFF] = 4;	/* B-LEFT */
	expr_prio['r' & 0xFF] = 4;	/* B-RIGHT */
	expr_prio['c' & 0xFF] = 4;	/* B-SHIFT-LC */
	expr_prio['d' & 0xFF] = 4;	/* B-SHIFT-RC */
	expr_prio['=' & 0xFF] = 4;
	expr_prio['~' & 0xFF] = 4;
	expr_prio['<' & 0xFF] = 4;
	expr_prio['>' & 0xFF] = 4;
	expr_prio['[' & 0xFF] = 4;
	expr_prio[']' & 0xFF] = 4;
	expr_prio['!' & 0xFF] = 5;
	expr_prio['&' & 0xFF] = 6;
	expr_prio['|' & 0xFF] = 7;
	expr_prio[')' & 0xFF] = 8;
	expr_prio['(' & 0xFF] = 9;
	expr_prio[0] = 10;
}
#endif

/*
 * Emit any MOVEs from non-simple field to temp field
 * for GENERATE to execute
 */
static int report_in_footing = 0;
static void
cb_emit_report_moves (struct cb_report *r, struct cb_field *f, int forterminate)
{
	struct cb_field		*p;
	const int report_footing_flag
		= (COB_REPORT_FOOTING | COB_REPORT_CONTROL_FOOTING | COB_REPORT_CONTROL_FOOTING_FINAL);
	for (p = f; p; p = p->sister) {
		if (p->report_flag & report_footing_flag) {
			report_in_footing = 1;
		}
		if (p->report_when) {
			if ((forterminate  &&  report_in_footing)
			 || (!forterminate && !report_in_footing)) {
				cb_emit (cb_build_if (p->report_when, NULL, CB_TREE (p), STMT_PRESENT_WHEN));
			}
		}
		if (p->children) {
			cb_emit_report_moves (r, p->children, forterminate);
			if (p->report_flag & report_footing_flag) {
				report_in_footing = 0;
			}
		}
	}
}

static void
cb_emit_report_move_id (cb_tree rep)
{
	struct cb_report *r = CB_REPORT_PTR (rep);
	if (r
	 && r->id == 0) {
		r->id = report_id++;
		cb_emit (CB_BUILD_FUNCALL_1 ("$M", rep));
		cb_emit_report_moves(r, r->records, 0);
		cb_emit (CB_BUILD_FUNCALL_1 ("$t", rep));
		cb_emit_report_moves(r, r->records, 1);
		cb_emit (CB_BUILD_FUNCALL_1 ("$m", rep));
	}
}

/* INITIATE statement */

void
cb_emit_initiate (cb_tree rep)
{
	if (rep == cb_error_node) {
		return;
	}
	cb_emit_report_move_id (rep);
	cb_emit (CB_BUILD_FUNCALL_1 ("$I", rep));

}

/* TERMINATE statement */

void
cb_emit_terminate (cb_tree rep)
{
	if (rep == cb_error_node) {
		return;
	}
	cb_emit_report_move_id (rep);
	cb_emit (CB_BUILD_FUNCALL_1 ("$T", rep));

}

/* GENERATE statement */

void
cb_emit_generate (cb_tree x)
{
	struct cb_field	*f;
	struct cb_report *r;
	cb_tree		y;
	cb_tree		z;
	if (x == cb_error_node) {
		return;
	}
	if (CB_REFERENCE_P (x)) {
		y = cb_ref (x);
		if (y == cb_error_node) {
			return;
		}
	} else {
		y = x;
	}
	if(CB_REPORT_P (y)) {
		r = CB_REPORT (y);
		z = cb_build_reference (r->name);
		CB_REFERENCE (z)->value = CB_TREE (y);
		cb_emit_report_move_id(z);
		cb_emit (CB_BUILD_FUNCALL_2 ("$R", z, NULL));
		return;
	}
	f = CB_FIELD (y);
	if(f == NULL
	|| f->report == NULL) {
		cb_error_x (x, _("data item is not part of a report"));
	} else {
		z = cb_build_reference (f->name);
		CB_REFERENCE (z)->value = CB_TREE (f->report);
		x->tag = CB_TAG_REPORT_LINE;
		cb_emit_report_move_id(z);
		cb_emit (CB_BUILD_FUNCALL_3 ("$R", z, x, y));
	}
}

/* SUPPRESS statement */

void
cb_emit_suppress (struct cb_field *f)
{
	cb_tree		z;
	/* MORE TO DO HERE */
	/* Find cob_report_control and set on suppress flag */
	if (f == NULL
	 || f->report == NULL) {
		cb_error (_("improper use of SUPPRESS PRINTING"));
		return;
	}
	z = cb_build_reference (f->name);
	CB_REFERENCE (z)->value = CB_TREE (f->report);
	cb_emit (CB_BUILD_FUNCALL_2 ("$S", z, cb_int (f->id)));
}

/* JSON/XML GENERATE statement */

static int
error_if_not_alnum_or_national (cb_tree ref, const char *name)
{
	if (!  (CB_TREE_CATEGORY (ref) == CB_CATEGORY_ALPHANUMERIC
	     || CB_TREE_CATEGORY (ref) == CB_CATEGORY_NATIONAL)) {
		/* note: at least with Enterprise COBOL utf8 is explicit forbidden here */
		cb_error_x (ref, _("%s must be alphanumeric or national"), name);
		return 1;
	}
	return 0;
}

static int
error_if_figurative_constant (cb_tree ref, const char *name)
{
	if (cb_is_figurative_constant (ref)) {
		cb_error_x (ref, _("%s may not be a figurative constant"), name);
		return 1;
	}
	return 0;
}

static int
is_subordinate_to (cb_tree ref, cb_tree parent_ref)
{
	const struct cb_field	*f = CB_FIELD_PTR (ref);
	struct cb_field 	*parent = CB_FIELD_PTR (parent_ref);

	for (f = f->parent; f; f = f->parent) {
		if (f == parent) {
			return 1;
		}
	}

	return 0;
}

static int
is_subordinate_to_fields (struct cb_field* f, struct cb_field* parent)
{
	for (f = f->parent; f; f = f->parent) {
		if (f == parent) {
			return 1;
		}
	}

	return 0;
}

static int
error_if_not_child_of_input_record (cb_tree ref, cb_tree input_record,
				    const char *name)
{
	if (!is_subordinate_to (ref, input_record)) {
		cb_error_x (ref, _("%s must be a child of the input record"), name);
		return 1;
	}
	return 0;
}

static int
is_ignored_child_in_ml_gen (cb_tree ref, cb_tree parent_ref)
{
	const struct cb_field 	*parent = CB_FIELD_PTR (parent_ref);
	struct cb_field	*f = CB_FIELD_PTR (ref);

	for (; f && f != parent; f = f->parent) {
		if (cb_field_is_ignored_in_ml_gen (f)) {
			return 1;
		}
	}

	return 0;
}

static int
error_if_ignored_in_ml_gen (cb_tree ref, cb_tree input_record, const char *name)
{
	if (is_ignored_child_in_ml_gen (ref, input_record)) {
		cb_error_x (ref, _("%s may not be an ignored item in JSON/XML GENERATE"), name);
		return 1;
	}
	return 0;
}

static int
error_if_not_elementary (cb_tree ref, const char *name)
{
	if (CB_FIELD_PTR (ref)->children) {
		cb_error_x (ref, _("%s must be elementary"), name);
		return 1;
	}
	return 0;
}

static int
error_string_not_usage_display_or_national (cb_tree ref)
{
	const struct cb_field	*f = CB_FIELD_PTR (ref);
	if (!  (CB_FIELD (cb_ref (ref))->usage == CB_USAGE_DISPLAY
	     || CB_FIELD (cb_ref (ref))->usage == CB_USAGE_NATIONAL)) {
		cb_error_x (ref, _("STRING item '%s' must be USAGE DISPLAY or NATIONAL"), f->name);
		return 1;
	}
	return 0;
}

static int
error_if_not_usage_display_or_national (cb_tree ref, const char *name)
{
	const struct cb_field	*f = CB_FIELD_PTR (ref);
	if (f->usage != CB_USAGE_DISPLAY
	 && f->usage != CB_USAGE_NATIONAL) {
		cb_error_x (ref, _("%s must be USAGE DISPLAY or NATIONAL"), name);
		return 1;
	}
	return 0;
}

static int
error_if_not_integer_ref (cb_tree ref, const char *name)
{
	const struct cb_field	*f = CB_FIELD_PTR (ref);
	if (CB_TREE_CATEGORY (f) == CB_CATEGORY_NUMERIC
	 && f->pic && f->pic->scale > 0) {
		cb_error_x (ref, _("%s must be an integer"), name);
		return 1;
	}
	return 0;
}

static int
syntax_check_ml_gen_receiving_item (cb_tree out)
{
	int	error = 0;

	if (cb_validate_one (out)) {
		return 1;
	}

	error |= error_if_not_alnum_or_national (out, _("JSON/XML GENERATE receiving item"));

	if (CB_FIELD (cb_ref (out))->flag_justified) {
		cb_error_x (out, _("JSON/XML GENERATE receiving item may not have JUSTIFIED clause"));
		error = 1;
	}
	error |= error_if_subscript_or_refmod (out, _("JSON/XML GENERATE receiving item"));

	return error;
}

static int
all_children_are_ignored (struct cb_field * const f)
{
	struct cb_field	*child;

	for (child = f->children; child; child = child->sister) {
		if (!cb_field_is_ignored_in_ml_gen (child)
		 && !(child->children
		   && all_children_are_ignored (child))) {
			return 0;
		}
	}

	return 1;
}

static int
name_is_unique_when_qualified_by (struct cb_field * const f,
				  struct cb_field * const qualifier)
{
	cb_tree	qual_ref = cb_build_field_reference (qualifier, NULL);
	cb_tree	f_ref = cb_build_reference (f->name);
	CB_REFERENCE (f_ref)->chain = qual_ref;

	return cb_try_ref (f_ref) != cb_error_node;
}

static int
all_children_ok_qualified_by_only (struct cb_field * const f,
				   struct cb_field * const qualifier)
{
	struct cb_field	*child;

	for (child = f->children; child; child = child->sister) {
		if (child->flag_filler) {
			continue;
		}

		if (!name_is_unique_when_qualified_by (child, qualifier)) {
			return 0;
		}
		if (child->children
		    && !all_children_ok_qualified_by_only (child, qualifier)) {
			return 0;
		}
	}

	return 1;
}


static int
contains_floating_point_item (const struct cb_field * const f, const int check_siblings)
{
	return is_floating_point_usage (f->usage)
		|| (f->children && contains_floating_point_item (f->children, 1))
		|| (check_siblings && f->sister
		    && contains_floating_point_item (f->sister, 1));
}

static int
contains_occurs_item (const struct cb_field * const f, const int check_siblings)
{
	return f->flag_occurs
		|| (f->children && contains_occurs_item (f->children, 1))
		|| (check_siblings && f->sister
		    && contains_occurs_item (f->sister, 1));
}

static int
syntax_check_ml_gen_input_rec (cb_tree from)
{
	int 	error = 0;
	struct cb_field	*from_field;

	if (cb_validate_one (from)) {
		return 1;
	}

	if (CB_REFERENCE (from)->offset) {
		cb_error_x (from, _("JSON/XML GENERATE input record may not be reference modified"));
		error = 1;
	}

	from_field = CB_FIELD (cb_ref (from));
	if (from_field->rename_thru) {
		cb_error_x (from, _("JSON/XML GENERATE input record may not have RENAMES clause"));
		error = 1;
	}

	if (from_field->children && all_children_are_ignored (from_field)) {
		cb_error_x (from, _("all the children of '%s' are ignored in JSON/XML GENERATE"),
			    cb_name (from));
		error = 1;
	}

	if (!all_children_ok_qualified_by_only (from_field, from_field)) {
		/* TODO: Output the name of the child with the nonunique name */
		cb_error_x (from, _("JSON/XML GENERATE input record has subrecords with non-unique names"));
		error = 1;
	}

	if (contains_floating_point_item (from_field, 0)) {
		CB_PENDING (_("floating-point items in JSON/XML GENERATE"));
	}

	if (contains_occurs_item (from_field, 0)) {
		CB_PENDING (_("OCCURS items in JSON/XML GENERATE"));
	}

	return error;
}

static int
syntax_check_ml_gen_count_in (cb_tree count)
{
	return error_if_not_int_field_or_has_pic_p ("COUNT IN", count);
}

static int
is_valid_uri (const struct cb_literal * const namespace)
{
	size_t size = (size_t)namespace->size;
	char	*copy = cob_malloc (size + 1);
	int	is_valid;

	memcpy (copy, namespace->data, size);
	copy[size] = '\0';
	is_valid = cob_is_valid_uri (copy);
	cob_free (copy);

	return is_valid;
}


static int
syntax_check_xml_gen_namespace (cb_tree namespace)
{
	int	error = 0;

	if (!namespace) {
		return 0;
	}

	if (cb_validate_one (namespace)) {
		return 1;
	}

	error |= error_if_not_alnum_or_national (namespace, "NAMESPACE");

	if (error_if_figurative_constant (namespace, "NAMESPACE")) {
		error = 1;
	} else {
		if (CB_LITERAL_P (namespace) && !is_valid_uri (CB_LITERAL (namespace))) {
			cb_error_x (namespace, _("NAMESPACE must be a valid URI"));
			error = 1;
		}
	}

	return error;
}

static int
is_valid_xml_name (const struct cb_literal * const name)
{
	unsigned int	i;

	if (!cob_is_xml_namestartchar (name->data[0])) {
		return 0;
	}

	for (i = 1; i < name->size; ++i) {
		if (!cob_is_xml_namechar (name->data[i])) {
			return 0;
		}
	}

	return 1;
}

static int
syntax_check_xml_gen_prefix (cb_tree prefix)
{
	int	error = 0;

	if (prefix == cb_null) {
		return 0;
	}

	if (cb_validate_one (prefix)) {
		return 1;
	}

	error |= error_if_not_alnum_or_national (prefix, "NAMESPACE-PREFIX");

	if (error_if_figurative_constant (prefix, "NAMESPACE-PREFIX")) {
		error = 1;
	} else if (CB_LITERAL_P (prefix) && !is_valid_xml_name (CB_LITERAL (prefix))) {
		cb_error_x (prefix, _("NAMESPACE-PREFIX must be a valid XML name"));
		error = 1;
	}

	return error;
}

static int
syntax_check_ml_gen_name_list (cb_tree name_list, cb_tree input, const int is_xml)
{
	cb_tree	name_pair;
	cb_tree	ref;
	cb_tree	name;
	cb_tree	l;
	struct cb_field	*reference_field;
	struct cb_field	*input_field = CB_FIELD (cb_ref (input));
	int	error = 0;

	for (l = name_list; l; l = CB_CHAIN (l)) {
		name_pair = CB_VALUE (l);
		ref = CB_PAIR_X (name_pair);
		name = CB_PAIR_Y (name_pair);
		if (cb_validate_one (ref)
		 || cb_validate_one (name)) {
			error = 1;
			continue;
		}
		reference_field = CB_FIELD (cb_ref (ref));

		error |= error_if_subscript_or_refmod (ref, _("NAME OF item"));

		if (reference_field != input_field
		 && !is_subordinate_to_fields (reference_field, input_field)) {
			cb_error_x (ref,
				_("NAME OF item must be the input record or a child of it"));
			error = 1;
		} else {
			error |= error_if_ignored_in_ml_gen (ref, input, _("NAME OF item"));
		}

		if (name == cb_null) {
			/* note: only allowed for JSON in the parser */
			if (reference_field != input_field) {
				cb_error_x (ref,
					_("NAME OF ... OMITTED only valid for source identifier"));
				error = 1;
			}
			continue;
		}

		if (!is_valid_xml_name (CB_LITERAL (name))) {
			cb_error_x (ref,
				_("NAME OF literal must be a valid %s identifier"),
				is_xml ? "XML" : "JSON");
			error = 1;
		}
	}

	return error;
}

static int
syntax_check_ml_gen_type_list (cb_tree type_list, cb_tree input)
{
	cb_tree	l;
	int 	error = 0;

	for (l = type_list; l; l = CB_CHAIN (l)) {
		cb_tree type_pair = CB_VALUE (l);
		cb_tree ref = CB_PAIR_X (type_pair);
		cb_tree type = CB_PAIR_Y (type_pair);
		if (cb_validate_one (ref)
		 || cb_validate_one (type)) {
			error = 1;
			continue;
		}

		error |= error_if_subscript_or_refmod (ref, _("TYPE OF item"));
		error |= error_if_not_elementary (ref, _("TYPE OF item"));

		if (error_if_not_child_of_input_record (ref, input,
							_("TYPE OF item"))) {
			error = 1;
		} else {
			error |= error_if_ignored_in_ml_gen (ref, input,
							_("TYPE OF item"));
		}
	}

	return error;
}

static int
syntax_check_when_list (const struct cb_ml_suppress_clause *suppress)
{
	cb_tree		l;
	int		error = 0;
	const char	*name;

	for (l = suppress->when_list; l; l = CB_CHAIN (l)) {
		/* TODO: Handle DISPLAY-1 if/when it is supported. */
		if (CB_VALUE (l) == cb_space) {
			error |= error_if_not_usage_display_or_national (suppress->identifier,
									 _("SUPPRESS WHEN SPACE item"));
		} else if (CB_VALUE (l) == cb_low || CB_VALUE (l) == cb_high) {
			if (CB_VALUE (l) == cb_low) {
				name = _("SUPPRESS WHEN LOW-VALUE item");
			} else {
				name = _("SUPPRESS WHEN HIGH-VALUE item");
			}
			error |= error_if_not_usage_display_or_national (suppress->identifier,
									 name);
			error |= error_if_not_integer_ref (suppress->identifier, name);
		}
	}

	return error;
}

static int
syntax_check_ml_gen_suppress_list (cb_tree suppress_list, cb_tree input)
{
	int	error = 0;
	cb_tree	l;

	for (l = suppress_list; l; l = CB_CHAIN (l)) {
		const struct cb_ml_suppress_clause	*suppress
				= CB_ML_SUPPRESS (CB_VALUE (l));
		if (!suppress->identifier) {
			continue;
		}

		if (cb_validate_one (suppress->identifier)) {
			return 1;
		}

		error |= error_if_subscript_or_refmod (suppress->identifier,
							_("SUPPRESS item"));

		if (suppress->when_list) {
			error |= error_if_not_elementary (suppress->identifier,
							_("SUPPRESS item with WHEN clause"));
		}

		if (error_if_not_child_of_input_record (suppress->identifier, input,
							_("SUPPRESS item"))) {
			error = 1;
		} else {
			error |= error_if_ignored_in_ml_gen (suppress->identifier,
							input, _("SUPPRESS item"));
		}

		error |= syntax_check_when_list (suppress);
	}

	return error;
}

static int
syntax_check_ml_generate (cb_tree out, cb_tree from, cb_tree count,
			  cb_tree encoding,
			  cb_tree namespace_and_prefix,
			  cb_tree name_list, cb_tree type_list,
			  cb_tree suppress_list, const int is_xml)
{
	int	error = 0;

	error |= syntax_check_ml_gen_receiving_item (out);
	error |= syntax_check_ml_gen_input_rec (from);
	error |= syntax_check_ml_gen_count_in (count);
	COB_UNUSED (encoding);	/* TODO: check encoding */
	if (namespace_and_prefix) {
		error |= syntax_check_xml_gen_namespace (CB_PAIR_X (namespace_and_prefix));
		error |= syntax_check_xml_gen_prefix (CB_PAIR_Y (namespace_and_prefix));
	}
	error |= syntax_check_ml_gen_name_list (name_list, from, is_xml);
	error |= syntax_check_ml_gen_type_list (type_list, from);
	error |= syntax_check_ml_gen_suppress_list (suppress_list, from);

	/* TODO: Warn if out is probably too short */
	/* TODO: Warn if count_in may overflow */

	return error;
}

void
cb_emit_xml_generate (cb_tree out, cb_tree from, cb_tree count,
		      cb_tree encoding,
		      const int with_xml_dec,
		      const int with_attrs,
		      cb_tree namespace_and_prefix,
		      cb_tree name_list, cb_tree type_list,
		      cb_tree suppress_list)
{
	struct cb_ml_generate_tree	*tree;
	unsigned char decimal_point;

	if (current_statement->ex_handler == NULL
	 && current_statement->not_ex_handler == NULL)
	  	current_statement->handler_type = NO_HANDLER;
#ifndef WITH_XML2
	if (!warn_xml_done) {
		warn_xml_done = 1;
		cb_warning (cb_warn_unsupported,
			_("runtime is not configured to support %s"), "XML");
	}
#endif
	if (syntax_check_ml_generate (out, from, count, encoding,
						namespace_and_prefix, name_list,
						type_list, suppress_list, 1)) {
		return;
	}

	tree = CB_ML_TREE (cb_build_ml_tree (CB_FIELD (cb_ref (from)),
						with_attrs, 0, name_list,
						type_list, suppress_list));

	tree->sibling = current_program->ml_trees;
	current_program->ml_trees = tree;

	if (with_attrs && !tree->attrs) {
		cb_warning (cb_warn_additional,
			_("WITH ATTRIBUTES specified, but no attributes can be generated"));
	}

	cb_emit (cb_build_ml_suppress_checks (tree));
	if (cb_dpc_in_data == CB_DPC_IN_XML
	    || cb_dpc_in_data == CB_DPC_IN_ALL) {
		decimal_point = current_program->decimal_point;
	} else {
		decimal_point = '.';
	}
	if (namespace_and_prefix) {
		cb_emit (CB_BUILD_FUNCALL_7 ("cob_xml_generate", out, CB_TREE (tree),
					     count, cb_int (with_xml_dec),
					     CB_PAIR_X (namespace_and_prefix),
					     CB_PAIR_Y (namespace_and_prefix),
					     cb_int (decimal_point)));
	} else {
		cb_emit (CB_BUILD_FUNCALL_7 ("cob_xml_generate", out, CB_TREE (tree),
					     count, cb_int (with_xml_dec),
					     NULL, NULL, cb_int (decimal_point)));
	}
}

void
cb_emit_xml_parse (cb_tree data, cb_tree proc,
		      const int returning_national,
		      cb_tree encoding, cb_tree validation)
{
	cb_tree ref;

#ifndef WITH_XML2
	if (!warn_xml_done) {
		warn_xml_done = 1;
		cb_warning (cb_warn_unsupported,
			_("runtime is not configured to support %s"), "XML");
	}
#endif
#if 0	/* TODO: more syntax checks */
	if (syntax_check_ml_generate (out, from, count, encoding,
						namespace_and_prefix, name_list,
						type_list, suppress_list, 1)) {
		return;
	}
#endif
	ref = cb_ref (data);
	if (CB_FIELD_P (ref)) {
		struct cb_field * field = CB_FIELD (ref);
		/* TODO: type checks here */
		cb_emit (cb_build_xml_parse (data, proc,
			returning_national | (field->usage == CB_USAGE_NATIONAL),
			encoding, validation));
	} else {
	}
}

void
cb_emit_json_generate (cb_tree out, cb_tree from, cb_tree count,
		       cb_tree name_list, cb_tree suppress_list)
{
	struct cb_ml_generate_tree	*tree;
	unsigned char decimal_point;

	if (current_statement->ex_handler == NULL
	 && current_statement->not_ex_handler == NULL)
	  	current_statement->handler_type = NO_HANDLER;
#if   !defined (WITH_CJSON) && !defined (WITH_JSON_C)
	if (!warn_json_done) {
		warn_json_done = 1;
		cb_warning (cb_warn_unsupported,
			_("runtime is not configured to support %s"), "JSON");
	}
#endif
	if (syntax_check_ml_generate (out, from, count, NULL,
						NULL, name_list,
						NULL, suppress_list, 0)) {
		return;
	}

        tree = CB_ML_TREE (cb_build_ml_tree (CB_FIELD (cb_ref (from)),
					     0, 0, name_list,
					     NULL, suppress_list));

	tree->sibling = current_program->ml_trees;
	current_program->ml_trees = tree;

	cb_emit (cb_build_ml_suppress_checks (tree));

	if (cb_dpc_in_data == CB_DPC_IN_JSON
	    || cb_dpc_in_data == CB_DPC_IN_ALL) {
		decimal_point = current_program->decimal_point;
	} else {
		decimal_point = '.';
	}
	cb_emit (CB_BUILD_FUNCALL_4 ("cob_json_generate", out, CB_TREE (tree),
				     count, cb_int (decimal_point)));
}

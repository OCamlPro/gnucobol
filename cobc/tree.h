/*
   Copyright (C) 2001-2012, 2014-2022 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Simon Sobisch, Ron Norman

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


#ifndef CB_TREE_H
#define CB_TREE_H

#define CB_BEFORE		cb_int0
#define CB_AFTER		cb_int1

#define CB_PREFIX_ATTR		"a_"	/* Field attribute (cob_field_attr) */
#define CB_PREFIX_BASE		"b_"	/* Base address (unsigned char *) */
#define CB_PREFIX_CONST		"c_"	/* Constant or literal (cob_field) */
#define CB_PREFIX_DECIMAL	"d_"	/* Decimal number (cob_decimal) */
#define CB_PREFIX_DEC_FIELD	"kc_"	/* Decimal Constant for literal (cob_field) */
#define CB_PREFIX_DEC_CONST	"dc_"	/* Decimal Constant (cob_decimal) */
#define CB_PREFIX_FIELD		"f_"	/* Field (cob_field) */
#define CB_PREFIX_SCR_FIELD		"fs_"	/* Screen field (cob_field) */
#define CB_PREFIX_FILE		"h_"	/* File (cob_file) */
#define CB_PREFIX_KEYS		"k_"	/* File keys (cob_file_key []) */
#define CB_PREFIX_LABEL		"l_"	/* Label */
#define CB_PREFIX_ML_ATTR	"ma_"	/* JSON/XML GENERATE attribute */
#define CB_PREFIX_ML_TREE	"mt_"	/* JSON/XML GENERATE tree */
#define CB_PREFIX_PIC		"p_"	/* PICTURE string */
#define CB_PREFIX_SEQUENCE	"s_"	/* Collating sequence */
#define CB_PREFIX_STRING	"st_"	/* String */
#define CB_PREFIX_REPORT	"r_"	/* Report (cob_report) */
#define CB_PREFIX_REPORT_LINE	"rl_"	/* Report line (cob_report_line) */
#define CB_PREFIX_REPORT_FIELD	"rf_"	/* Report field (cob_report_field) */
#define CB_PREFIX_REPORT_SUM	"rs_"	/* Report SUM (cob_report_sum) */
#define CB_PREFIX_REPORT_CONTROL "rc_"	/* Report CONTROL (cob_report_control) */
#define CB_PREFIX_REPORT_REF	"rr_"	/* Report CONTROL reference (cob_report_control_ref) */
#define CB_PREFIX_REPORT_SUM_CTR "rsc_"	/* Report SUM COUNTER */

/* call mode for passing/receiving a parameter */
enum cb_call_mode {
	CB_CALL_BY_REFERENCE = 1,
	CB_CALL_BY_CONTENT,
	CB_CALL_BY_VALUE,
	CB_CALL_DEFAULT_MODE
};

/* call size for passing/receiving a parameter, must always
   be accessed via macros CB_SIZES_INT | CB_SIZES_INT_UNSIGNED */
enum cb_param_size {
	CB_SIZE_UNSET = 0,
	CB_SIZE_1 = 1,			/*  8bit item -> char */
	CB_SIZE_2 = 2,			/* 16bit item -> short */
	CB_SIZE_4 = 4,			/* 32bit item -> int */
	CB_SIZE_8 = 8,			/* 64bit item -> long long int */
	CB_SIZE_16 = 16,		/* 128bit reserved */
	CB_SIZE_32 = 32,		/* 256bit reserved */
	CB_SIZE_AUTO = 64,		/* "as needed" 1-8, CHECKME: in some cases only 4/8 */
	CB_SIZE_UNSIGNED = 128	/* not directly used, contained to ensure correct position for bit-operation */
};

/* Hash values */
/* Power of 2 - see hash function in tree.c */
#define CB_WORD_HASH_SIZE	(1U << 11)
#define CB_WORD_HASH_MASK	(CB_WORD_HASH_SIZE - 1U)

/* Basic tree tag */
enum cb_tag {
	/* Primitives */
	CB_TAG_CONST = 0,	/* Constant value */
	CB_TAG_INTEGER,		/* Integer constant */
	CB_TAG_STRING,		/* String constant */
	CB_TAG_ALPHABET_NAME,	/* Alphabet-name */
	CB_TAG_CLASS_NAME,	/* Class-name */
	CB_TAG_LOCALE_NAME,	/* Locale-name */
	CB_TAG_SYSTEM_NAME,	/* System-name */
	CB_TAG_SCHEMA_NAME,	/* xml-schema-name */
	CB_TAG_LITERAL,		/* Numeric/alphanumeric literal */
	CB_TAG_DECIMAL,		/* Decimal number */
	CB_TAG_FIELD,		/* User-defined variable */
	CB_TAG_FILE,		/* File description */
	CB_TAG_REPORT,		/* Report description */
	CB_TAG_CD,		/* Communication description */
	/* Expressions */
	CB_TAG_REFERENCE,	/* Reference to a field, file, or label */
	CB_TAG_BINARY_OP,	/* Binary operation */
	CB_TAG_FUNCALL,		/* Run-time function call */
	CB_TAG_CAST,		/* Type cast */
	CB_TAG_INTRINSIC,	/* Intrinsic function */
	/* Statements */
	CB_TAG_LABEL,		/* Label statement */
	CB_TAG_ASSIGN,		/* Assignment statement */
	CB_TAG_INITIALIZE,	/* INITIALIZE statement */
	CB_TAG_SEARCH,		/* SEARCH statement */
	CB_TAG_CALL,		/* CALL statement */
	CB_TAG_GOTO,		/* GO TO statement */
	CB_TAG_IF,		/* IF statement / WHEN clause / PRESENT WHEN clause */
	CB_TAG_PERFORM,		/* PERFORM statement */
	CB_TAG_STATEMENT,	/* General statement */
	CB_TAG_CONTINUE,	/* CONTINUE statement */
	CB_TAG_CANCEL,		/* CANCEL statement */
	CB_TAG_ALTER,		/* ALTER statement */
	CB_TAG_SET_ATTR,	/* SET ATTRIBUTE statement */
	CB_TAG_XML_PARSE,	/* XML PARSE statement */
	/* Miscellaneous */
	CB_TAG_PERFORM_VARYING,	/* PERFORM VARYING parameter */
	CB_TAG_PICTURE,		/* PICTURE clause */
	CB_TAG_LIST,		/* List */
	CB_TAG_DIRECT,		/* Code output or comment */
	CB_TAG_DEBUG,		/* Debug item set */
	CB_TAG_DEBUG_CALL,	/* Debug callback */
	CB_TAG_PROGRAM,		/* Program */
	CB_TAG_PROTOTYPE,	/* Prototype */
	CB_TAG_DECIMAL_LITERAL,	/* Decimal Literal */
	CB_TAG_REPORT_LINE,	/* Report line description */
	CB_TAG_ML_SUPPRESS,	/* JSON/XML GENERATE SUPPRESS clause */
	CB_TAG_ML_TREE,	/* JSON/XML GENERATE output tree */
	CB_TAG_ML_SUPPRESS_CHECKS	/* JSON/XML GENERATE SUPPRESS checks */
	/* When adding a new entry, please remember to add it to
	   cb_enum_explain in tree.c as well. */
};

/* Alphabet target */
#define CB_ALPHABET_ALPHANUMERIC	0
#define CB_ALPHABET_NATIONAL	1

/* Alphabet type */
#define CB_ALPHABET_NATIVE	0
#define CB_ALPHABET_ASCII	1
#define CB_ALPHABET_EBCDIC	2
#define CB_ALPHABET_CUSTOM	3
#define CB_ALPHABET_LOCALE	4
#define CB_ALPHABET_UTF_8	5
#define CB_ALPHABET_UTF_16	6
#define CB_ALPHABET_UCS_4	7

/* Call convention bits */
/* Bit number	Meaning			Value */
/*	0	currently ignored by GC			*/
/*		Parameter order     0 - Right to left		*/
/*		                    1 - Left to right		*/
/*	1	currently ignored by GC			*/
/*		Stack manipulation  0 - Caller removes params	*/
/*		                    1 - Callee removes params	*/
/*	2	RETURN-CODE update  0 - Updated			*/
/*		                    1 - Not updated			*/
/*	3	Linking behaviour	0 - Normal linking		*/
/*		                    1 - Static CALL linking		*/
/*	4	currently ignored by GC + MF		*/
/*		OS/2 Optlink        0 - ??				*/
/*		                    1 - ??				*/
/*	5	currently ignored by GC + MF		*/
/*		Thunked to 16 bit   0 - No thunk			*/
/*		                    1 - Thunk			*/
/*	6	GC: works both with static/dynamic calls */
/*		MF: this has his has no effect on dynamic calls	*/
/*		STDCALL convention  0 - CDECL			*/
/*		                    1 - STDCALL			*/
/*	7	currently ignored by GC + MF		*/
/*	8	currently ignored by GC			*/
/*		parameter-count for individual entry points 0 - checked	*/
/*		                                            1 - not checked	*/
/*	9	currently ignored by GC			*/
/*		case of call + program names	0 - disregarded (depending on compile time flags) */
/*		                                1 - regarded			*/
/*	10	currently ignored by GC			*/
/*		RETURN-CODE storage 0 - passed as return value		*/
/*		                    1 - passed in the first parameter			*/
/*	11-14	currently ignored by GC+MF			*/
/*	15	GC: enabling COBOL parameter handling for external callers, likely dropped with GC4	*/
/*		currently ignored by MF			*/
/*					0 - external callers don't set cob_call_params	*/
/*					1 - external callers set cob_call_params	- standard (!) */

#define CB_CONV_L_TO_R		(1 << 0)
#define CB_CONV_CALLEE_STACK	(1 << 1)
#define CB_CONV_NO_RET_UPD	(1 << 2)
#define CB_CONV_STATIC_LINK	(1 << 3)
#define CB_CONV_OPT_LINK	(1 << 4)
#define CB_CONV_THUNK_16	(1 << 5)
#define CB_CONV_STDCALL		(1 << 6)
#define CB_CONV_COBOL	(1 << 15)
#define CB_CONV_C	(0)
#define CB_CONV_PASCAL	(CB_CONV_L_TO_R | CB_CONV_CALLEE_STACK)

/* System category */
enum cb_system_name_category {
	CB_DEVICE_NAME = 0,
	CB_SWITCH_NAME,
	CB_FEATURE_NAME,
	CB_CALL_CONVENTION_NAME,
	CB_CODE_NAME,
	CB_COMPUTER_NAME,
	CB_EXTERNAL_LOCALE_NAME,
	CB_LIBRARY_NAME,
	CB_TEXT_NAME
};

/* Mnemonic defines */
/* Devices */
#define CB_DEVICE_SYSIN		0
#define CB_DEVICE_SYSOUT	1
#define CB_DEVICE_SYSERR	2
#define CB_DEVICE_CONSOLE	3
#define CB_DEVICE_PRINTER	4
#define CB_DEVICE_SYSPCH	5
/* Switches (max. must match COB_SWITCH_MAX) */
#define CB_SWITCH_0		0
#define CB_SWITCH_1		1
#define CB_SWITCH_2		2
#define CB_SWITCH_3		3
#define CB_SWITCH_4		4
#define CB_SWITCH_5		5
#define CB_SWITCH_6		6
#define CB_SWITCH_7		7
#define CB_SWITCH_8		8
#define CB_SWITCH_9		9
#define CB_SWITCH_10		10
#define CB_SWITCH_11		11
#define CB_SWITCH_12		12
#define CB_SWITCH_13		13
#define CB_SWITCH_14		14
#define CB_SWITCH_15		15
#define CB_SWITCH_16		16
#define CB_SWITCH_17		17
#define CB_SWITCH_18		18
#define CB_SWITCH_19		19
#define CB_SWITCH_20		20
#define CB_SWITCH_21		21
#define CB_SWITCH_22		22
#define CB_SWITCH_23		23
#define CB_SWITCH_24		24
#define CB_SWITCH_25		25
#define CB_SWITCH_26		26
#define CB_SWITCH_27		27
#define CB_SWITCH_28		28
#define CB_SWITCH_29		29
#define CB_SWITCH_30		30
#define CB_SWITCH_31		31
#define CB_SWITCH_32		32
#define CB_SWITCH_33		33
#define CB_SWITCH_34		34
#define CB_SWITCH_35		35
#define CB_SWITCH_36		36
/* Features */
#define CB_FEATURE_FORMFEED	0
#define CB_FEATURE_CONVENTION	1
#define CB_FEATURE_C01		2
#define CB_FEATURE_C02		3
#define CB_FEATURE_C03		4
#define CB_FEATURE_C04		5
#define CB_FEATURE_C05		6
#define CB_FEATURE_C06		7
#define CB_FEATURE_C07		8
#define CB_FEATURE_C08		9
#define CB_FEATURE_C09		10
#define CB_FEATURE_C10		11
#define CB_FEATURE_C11		12
#define CB_FEATURE_C12		13


/* Class category */
enum cb_class {
	CB_CLASS_UNKNOWN = 0,		/* 0 */
	CB_CLASS_ALPHABETIC,		/* 1 */
	CB_CLASS_ALPHANUMERIC,		/* 2 */
	CB_CLASS_BOOLEAN,		/* 3 */
	CB_CLASS_INDEX,			/* 4 */
	CB_CLASS_NATIONAL,		/* 5 */
	CB_CLASS_NUMERIC,		/* 6 */
	CB_CLASS_OBJECT,		/* 7 */
	CB_CLASS_POINTER		/* 8 */
};

/* Category */
enum cb_category {
	CB_CATEGORY_UNKNOWN = 0,		/* 0 */
	CB_CATEGORY_ALPHABETIC,			/* 1 */
	CB_CATEGORY_ALPHANUMERIC,		/* 2 */
	CB_CATEGORY_ALPHANUMERIC_EDITED,	/* 3 */
	CB_CATEGORY_BOOLEAN,			/* 4 */
	CB_CATEGORY_INDEX,			/* 5 */
	CB_CATEGORY_NATIONAL,			/* 6 */
	CB_CATEGORY_NATIONAL_EDITED,		/* 7 */
	CB_CATEGORY_NUMERIC,			/* 8 */
	CB_CATEGORY_NUMERIC_EDITED,		/* 9 */
	CB_CATEGORY_OBJECT_REFERENCE,		/* 10 */
	CB_CATEGORY_DATA_POINTER,		/* 11 */
	CB_CATEGORY_PROGRAM_POINTER,		/* 12 */
	CB_CATEGORY_FLOATING_EDITED,	/* 13 */
	CB_CATEGORY_ERROR		/* 14, always last */
};

/* Storage sections */
enum cb_storage {
	CB_STORAGE_CONSTANT = 0,	/* Constants */
	CB_STORAGE_FILE,		/* FILE SECTION */
	CB_STORAGE_WORKING,		/* WORKING-STORAGE SECTION */
	CB_STORAGE_LOCAL,		/* LOCAL-STORAGE SECTION */
	CB_STORAGE_LINKAGE,		/* LINKAGE SECTION */
	CB_STORAGE_SCREEN,		/* SCREEN SECTION */
	CB_STORAGE_REPORT,		/* REPORT SECTION */
	CB_STORAGE_COMMUNICATION	/* COMMUNICATION SECTION */
};

/* Field types */
enum cb_usage {
	CB_USAGE_BINARY = 0,		/* 0 */
	CB_USAGE_BIT,			/* 1 */
	CB_USAGE_COMP_5,		/* 2 */
	CB_USAGE_COMP_X,		/* 3 */
	CB_USAGE_DISPLAY,		/* 4 */
	CB_USAGE_FLOAT,			/* 5 */
	CB_USAGE_DOUBLE,		/* 6 */
	CB_USAGE_INDEX,			/* 7 */
	CB_USAGE_NATIONAL,		/* 8 */
	CB_USAGE_OBJECT,		/* 9 */
	CB_USAGE_PACKED,		/* 10 */
	CB_USAGE_POINTER,		/* 11 */
	CB_USAGE_LENGTH,		/* 12 */
	CB_USAGE_PROGRAM_POINTER,	/* 13 */
	CB_USAGE_UNSIGNED_CHAR,		/* 14 */
	CB_USAGE_SIGNED_CHAR,		/* 15 */
	CB_USAGE_UNSIGNED_SHORT,	/* 16 */
	CB_USAGE_SIGNED_SHORT,		/* 17 */
	CB_USAGE_UNSIGNED_INT,		/* 18 */
	CB_USAGE_SIGNED_INT,		/* 19 */
	CB_USAGE_UNSIGNED_LONG,		/* 20 */
	CB_USAGE_SIGNED_LONG,		/* 21 */
	CB_USAGE_COMP_6,		/* 22 */
	CB_USAGE_FP_DEC64,		/* 23 */
	CB_USAGE_FP_DEC128,		/* 24 */
	CB_USAGE_FP_BIN32,		/* 25 */
	CB_USAGE_FP_BIN64,		/* 26 */
	CB_USAGE_FP_BIN128,		/* 27 */
	CB_USAGE_LONG_DOUBLE,		/* 28 */
	CB_USAGE_HNDL,			/* 29 */
	CB_USAGE_HNDL_WINDOW,		/* 30 */
	CB_USAGE_HNDL_SUBWINDOW,	/* 31 */
	CB_USAGE_HNDL_FONT,		/* 32 */
	CB_USAGE_HNDL_THREAD,		/* 33 */
	CB_USAGE_HNDL_MENU,		/* 34 */
	CB_USAGE_HNDL_VARIANT,		/* 35 */
	CB_USAGE_HNDL_LM,		/* 36 */
	CB_USAGE_COMP_N,		/* 37 */
	CB_USAGE_ERROR			/* 38, always last */
};


/* Cast type */
enum cb_cast_type {
	CB_CAST_INTEGER = 0,		/* 0 */
	CB_CAST_LONG_INT,		/* 1 */
	CB_CAST_ADDRESS,		/* 2 */
	CB_CAST_ADDR_OF_ADDR,		/* 3 */
	CB_CAST_LENGTH,			/* 4 */
	CB_CAST_PROGRAM_POINTER		/* 5 */
};

/* Intrinsic functions */
enum cb_intr_enum {
	CB_INTR_ABS = 1,
	CB_INTR_ACOS,
	CB_INTR_ANNUITY,
	CB_INTR_ASIN,
	CB_INTR_ATAN,
	CB_INTR_BASECONVERT,
	CB_INTR_BIT_OF,
	CB_INTR_BIT_TO_CHAR,
	CB_INTR_BOOLEAN_OF_INTEGER,
	CB_INTR_BYTE_LENGTH,
	CB_INTR_CHAR,
	CB_INTR_CHAR_NATIONAL,
	CB_INTR_COMBINED_DATETIME,
	CB_INTR_CONCATENATE,
	CB_INTR_CONTENT_LENGTH,
	CB_INTR_CONTENT_OF,
	CB_INTR_CONVERT,
	CB_INTR_COS,
	CB_INTR_CURRENCY_SYMBOL,
	CB_INTR_CURRENT_DATE,
	CB_INTR_DATE_OF_INTEGER,
	CB_INTR_DATE_TO_YYYYMMDD,
	CB_INTR_DAY_OF_INTEGER,
	CB_INTR_DAY_TO_YYYYDDD,
	CB_INTR_DISPLAY_OF,
	CB_INTR_E,
	CB_INTR_EXCEPTION_FILE,
	CB_INTR_EXCEPTION_FILE_N,
	CB_INTR_EXCEPTION_LOCATION,
	CB_INTR_EXCEPTION_LOCATION_N,
	CB_INTR_EXCEPTION_STATEMENT,
	CB_INTR_EXCEPTION_STATUS,
	CB_INTR_EXP,
	CB_INTR_EXP10,
	CB_INTR_FACTORIAL,
	CB_INTR_FIND_STRING,
	CB_INTR_FORMATTED_CURRENT_DATE,
	CB_INTR_FORMATTED_DATE,
	CB_INTR_FORMATTED_DATETIME,
	CB_INTR_FORMATTED_TIME,
	CB_INTR_FRACTION_PART,
	CB_INTR_HEX_OF,
	CB_INTR_HEX_TO_CHAR,
	CB_INTR_HIGHEST_ALGEBRAIC,
	CB_INTR_INTEGER,
	CB_INTR_INTEGER_OF_BOOLEAN,
	CB_INTR_INTEGER_OF_DATE,
	CB_INTR_INTEGER_OF_DAY,
	CB_INTR_INTEGER_OF_FORMATTED_DATE,
	CB_INTR_INTEGER_PART,
	CB_INTR_LENGTH,
	CB_INTR_LOCALE_COMPARE,
	CB_INTR_LOCALE_DATE,
	CB_INTR_LOCALE_TIME,
	CB_INTR_LOCALE_TIME_FROM_SECS,
	CB_INTR_LOG,
	CB_INTR_LOG10,
	CB_INTR_LOWER_CASE,
	CB_INTR_LOWEST_ALGEBRAIC,
	CB_INTR_MAX,
	CB_INTR_MEAN,
	CB_INTR_MEDIAN,
	CB_INTR_MIDRANGE,
	CB_INTR_MIN,
	CB_INTR_MOD,
	CB_INTR_MODULE_CALLER_ID,
	CB_INTR_MODULE_DATE,
	CB_INTR_MODULE_FORMATTED_DATE,
	CB_INTR_MODULE_ID,
	CB_INTR_MODULE_NAME,
	CB_INTR_MODULE_PATH,
	CB_INTR_MODULE_SOURCE,
	CB_INTR_MODULE_TIME,
	CB_INTR_MON_DECIMAL_POINT,
	CB_INTR_MON_THOUSANDS_SEP,
	CB_INTR_NATIONAL_OF,
	CB_INTR_NUM_DECIMAL_POINT,
	CB_INTR_NUM_THOUSANDS_SEP,
	CB_INTR_NUMVAL,
	CB_INTR_NUMVAL_C,
	CB_INTR_NUMVAL_F,
	CB_INTR_ORD,
	CB_INTR_ORD_MAX,
	CB_INTR_ORD_MIN,
	CB_INTR_PI,
	CB_INTR_PRESENT_VALUE,
	CB_INTR_RANDOM,
	CB_INTR_RANGE,
	CB_INTR_REM,
	CB_INTR_REVERSE,
	CB_INTR_SECONDS_FROM_FORMATTED_TIME,
	CB_INTR_SECONDS_PAST_MIDNIGHT,
	CB_INTR_SIGN,
	CB_INTR_SIN,
	CB_INTR_SQRT,
	CB_INTR_STANDARD_COMPARE,
	CB_INTR_STANDARD_DEVIATION,
	CB_INTR_STORED_CHAR_LENGTH,
	CB_INTR_SUBSTITUTE,
	CB_INTR_SUBSTITUTE_CASE,
	CB_INTR_SUM,
	CB_INTR_TAN,
	CB_INTR_TEST_DATE_YYYYMMDD,
	CB_INTR_TEST_DAY_YYYYDDD,
	CB_INTR_TEST_FORMATTED_DATETIME,
	CB_INTR_TEST_NUMVAL,
	CB_INTR_TEST_NUMVAL_C,
	CB_INTR_TEST_NUMVAL_F,
	CB_INTR_TRIM,
	CB_INTR_UPPER_CASE,
	CB_INTR_USER_FUNCTION,
	CB_INTR_VARIANCE,
	CB_INTR_WHEN_COMPILED,
	CB_INTR_YEAR_TO_YYYY
};

/* Perform type */
enum cb_perform_type {
	CB_PERFORM_EXIT = 0,
	CB_PERFORM_ONCE,
	CB_PERFORM_TIMES,
	CB_PERFORM_UNTIL,
	CB_PERFORM_FOREVER
};

/* Index type */
enum cb_index_type {
	CB_NORMAL_INDEX = 0,
	CB_INT_INDEX,
	CB_STATIC_INT_INDEX,
	CB_STATIC_INT_VARYING
};

/* Reserved word list structure */
struct cobc_reserved {
	const char	*name;		/* Word */
	unsigned short	nodegen;	/* Statement with END-xxx */
	unsigned short	context_sens;	/* Context sensitive */
	int		token;		/* Token */
	unsigned int	context_set;	/* Set context sensitive */
	unsigned int	context_test;	/* Test context sensitive */
};

/* Basic common tree structure */

struct cb_tree_common {
	enum cb_tag		tag;		/* TAG - see below */
	enum cb_category	category;	/* Category */
	const char		*source_file;	/* Source file */
	int			source_line;	/* Line */
	int			source_column;	/* Column */
};

/* Define common cb_tree/CB_TREE for following defines */

typedef struct cb_tree_common	*cb_tree;

#define CB_TREE(x)		((struct cb_tree_common *) (x))
#define CB_TREE_TAG(x)		(CB_TREE (x)->tag)
#define CB_TREE_CLASS(x)	cb_tree_class (CB_TREE (x))
#define CB_TREE_CATEGORY(x)	cb_tree_category (CB_TREE (x))

#define CB_TREE_TAG_UNEXPECTED_ABORT(x)	\
	do { /* not translated as unexpected dev-only message */ \
		cobc_err_msg ("unexpected tree tag: %s",	\
			cb_enum_explain (CB_TREE_TAG (x))); 	\
		COBC_ABORT ();		\
	} ONCE_COB

#define	CB_VALID_TREE(x)	(x && CB_TREE (x) != cb_error_node)
#define	CB_INVALID_TREE(x)	(!(x) || CB_TREE (x) == cb_error_node)

#ifdef	COB_TREE_DEBUG
#define CB_TREE_CAST(tg,ty,x)	\
	((ty *)cobc_tree_cast_check (x, __FILE__, __LINE__, tg))
#else
#define CB_TREE_CAST(tg,ty,x)	((ty *) (x))
#endif

/* any next */
struct cb_next_elem {
	struct cb_next_elem	*next;
};

/* FIXME: HAVE_FUNC should be checked via configure and the others be a fallback (note: currently only used in trunk [debug.c]) */
#if defined(NO_HAVE_FUNC)
  #define CURRENT_FUNCTION "unknown"
#elif defined(_MSC_VER)
  #define CURRENT_FUNCTION __FUNCTION__
#elif defined(__GNUC__) || (defined(__MWERKS__) && (__MWERKS__ >= 0x3000)) || (defined(__ICC) && (__ICC >= 600)) || defined(__ghs__)
  #define CURRENT_FUNCTION __PRETTY_FUNCTION__
#elif defined(__DMC__) && (__DMC__ >= 0x810)
  #define CURRENT_FUNCTION __PRETTY_FUNCTION__
#elif defined(__FUNCSIG__)
  #define CURRENT_FUNCTION __FUNCSIG__
#elif (defined(__INTEL_COMPILER) && (__INTEL_COMPILER >= 600)) || (defined(__IBMCPP__) && (__IBMCPP__ >= 500))
  #define CURRENT_FUNCTION __FUNCTION__
#elif defined(__BORLANDC__) && (__BORLANDC__ >= 0x550)
  #define CURRENT_FUNCTION __FUNC__
#elif defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901)
  #define CURRENT_FUNCTION __func__
#elif defined(__cplusplus) && (__cplusplus >= 201103)
  #define CURRENT_FUNCTION __func__
#else
  #define CURRENT_FUNCTION __FILE__
#endif

/* xref entries */
struct cb_xref_elem {
	struct cb_xref_elem	*next;
	struct cb_xref_elem	*prev;
	int			line;
	int			receive;
};

struct cb_xref {
	struct cb_xref_elem	*head;
	struct cb_xref_elem	*tail;
	int		amount;
	int		skip;
};

struct cb_call_elem {
	struct cb_call_elem	*next;
	char			*name;
	struct cb_xref		xref;
	int			is_identifier;
	int			is_system;
};

struct cb_call_xref {
	struct cb_call_elem	*head;
	struct cb_call_elem	*tail;
};

/* Constant */

struct cb_const {
	struct cb_tree_common	common;		/* Common values */
	const char		*val;		/* Constant value */
};

#define CB_CONST(x)	(CB_TREE_CAST (CB_TAG_CONST, struct cb_const, x))
#define CB_CONST_P(x)	(CB_TREE_TAG (x) == CB_TAG_CONST)

/* Code output or comment */

struct cb_direct {
	struct cb_tree_common	common;		/* Common values */
	const char		*line;		/* Line redirect */
	cob_u32_t		flag_is_direct;	/* Is directed */
	cob_u32_t		flag_new_line;	/* Need new line */
};

#define CB_DIRECT(x)	(CB_TREE_CAST (CB_TAG_DIRECT, struct cb_direct, x))
#define CB_DIRECT_P(x)	(CB_TREE_TAG (x) == CB_TAG_DIRECT)

/* DEBUG */

struct cb_debug {
	struct cb_tree_common	common;		/* Common values */
	cb_tree			target;		/* Target for debug */
	const char		*value;		/* Value for debug */
	cb_tree			fld;		/* Reference */
	size_t			size;		/* Size if relevant */
};

#define CB_DEBUG(x)	(CB_TREE_CAST (CB_TAG_DEBUG, struct cb_debug, x))
#define CB_DEBUG_P(x)	(CB_TREE_TAG (x) == CB_TAG_DEBUG)

/* DEBUG Callback */

struct cb_debug_call {
	struct cb_tree_common	common;		/* Common values */
	struct cb_label		*target;	/* Target label */
};

#define CB_DEBUG_CALL(x)	(CB_TREE_CAST (CB_TAG_DEBUG_CALL, struct cb_debug_call, x))
#define CB_DEBUG_CALL_P(x)	(CB_TREE_TAG (x) == CB_TAG_DEBUG_CALL)

/* Integer */

struct cb_integer {
	struct cb_tree_common	common;		/* Common values */
	int			val;		/* Integer value */
#ifdef USE_INT_HEX /* Simon: using this increases the struct and we
         *should* pass the flags as constants in any case... */
	unsigned int		hexval;		/* Output hex value */
#endif
};

#define CB_INTEGER(x)	(CB_TREE_CAST (CB_TAG_INTEGER, struct cb_integer, x))
#define CB_INTEGER_P(x)	(CB_TREE_TAG (x) == CB_TAG_INTEGER)

/* String */

struct cb_string {
	struct cb_tree_common	common;		/* Common values */
	const unsigned char	*data;		/* Data */
	size_t			size;		/* Data size */
};

#define CB_STRING(x)	(CB_TREE_CAST (CB_TAG_STRING, struct cb_string, x))
#define CB_STRING_P(x)	(CB_TREE_TAG (x) == CB_TAG_STRING)

/* Alphabet-name */

struct cb_alphabet_name {
	struct cb_tree_common	common;		/* Common values */
	const char		*name;		/* Original name */
	char			*cname;		/* Name used in C */
	cb_tree			custom_list;	/* Custom ALPHABET / LOCALE reference */
	unsigned int		alphabet_target;	/* ALPHANUMERIC or NATIONAL */
	unsigned int		alphabet_type;	/* ALPHABET type */
	int			low_val_char;	/* LOW-VALUE */
	int			high_val_char;	/* HIGH-VALUE */
	int			values[256];	/* Collating values */
	int			alphachr[256];	/* Actual values */
};

#define CB_ALPHABET_NAME(x)	(CB_TREE_CAST (CB_TAG_ALPHABET_NAME, struct cb_alphabet_name, x))
#define CB_ALPHABET_NAME_P(x)	(CB_TREE_TAG (x) == CB_TAG_ALPHABET_NAME)

/* Class-name */

struct cb_class_name {
	struct cb_tree_common	common;		/* Common values */
	const char		*name;		/* Original name */
	char			*cname;		/* Name used in C */
	cb_tree			list;		/* List of CLASS definitions */
};

#define CB_CLASS_NAME(x)	(CB_TREE_CAST (CB_TAG_CLASS_NAME, struct cb_class_name, x))
#define CB_CLASS_NAME_P(x)	(CB_TREE_TAG (x) == CB_TAG_CLASS_NAME)

/* Locale name */

struct cb_locale_name {
	struct cb_tree_common	common;		/* Common values */
	const char		*name;		/* Original name */
	char			*cname;		/* Name used in C */
	cb_tree			list;		/* List of locale definitions */
};

#define CB_LOCALE_NAME(x)	(CB_TREE_CAST (CB_TAG_LOCALE_NAME, struct cb_locale_name, x))
#define CB_LOCALE_NAME_P(x)	(CB_TREE_TAG (x) == CB_TAG_LOCALE_NAME)

/* System-name */

struct cb_system_name {
	struct cb_tree_common		common;		/* Common values */
	cb_tree				value;		/* System value */
	enum cb_system_name_category	category;	/* System category */
	int				token;		/* Device attributes */
};

#define CB_SYSTEM_NAME(x)	(CB_TREE_CAST (CB_TAG_SYSTEM_NAME, struct cb_system_name, x))
#define CB_SYSTEM_NAME_P(x)	(CB_TREE_TAG (x) == CB_TAG_SYSTEM_NAME)

/* XML-schema-name */

struct cb_schema_name {
	struct cb_tree_common	common;		/* Common values */
	const char		*name;		/* Original name */
	const char		*data;		/* file name */
};

#define CB_SCHEMA_NAME(x)	(CB_TREE_CAST (CB_TAG_SCHEMA_NAME, struct cb_schema_name, x))
#define CB_SCHEMA_NAME_P(x)	(CB_TREE_TAG (x) == CB_TAG_SCHEMA_NAME)

/* Literal */

struct cb_literal {
	struct cb_tree_common	common;	/* Common values */
	unsigned char		*data;	/* Literal data */
	cob_u32_t		size;	/* Literal size */
	int			scale;	/* Numeric scale */
	cob_u32_t		llit;	/* 'L' literal */
	short			sign;	/* unsigned: 0 negative: -1 positive: 1 */
	short			all;	/* ALL */
};

#define CB_LITERAL(x)	(CB_TREE_CAST (CB_TAG_LITERAL, struct cb_literal, x))
#define CB_LITERAL_P(x)	(CB_TREE_TAG (x) == CB_TAG_LITERAL)
#define CB_NUMERIC_LITERAL_P(x) \
  (CB_LITERAL_P (x) && CB_TREE_CATEGORY (x) == CB_CATEGORY_NUMERIC)

#define CB_ERR_LITMAX 38
extern char		*literal_for_diagnostic (char *buff, const char *literal_data);

/* Decimal */

struct cb_decimal {
	struct cb_tree_common	common;		/* Common values */
	unsigned int		id;		/* ID for this decimal */
};

#define CB_DECIMAL(x)	(CB_TREE_CAST (CB_TAG_DECIMAL, struct cb_decimal, x))
#define CB_DECIMAL_P(x)	(CB_TREE_TAG (x) == CB_TAG_DECIMAL)

#define CB_DECIMAL_LITERAL(x)	(CB_TREE_CAST (CB_TAG_DECIMAL_LITERAL, struct cb_decimal, x))
#define CB_DECIMAL_LITERAL_P(x)	(CB_TREE_TAG (x) == CB_TAG_DECIMAL_LITERAL)

/* Picture */

struct cb_picture {
	struct cb_tree_common	common;		/* Common values */
	char			*orig;		/* Original picture string */
	cob_pic_symbol		*str;		/* Picture string */
	int			size;		/* Byte size */
	int			lenstr;		/* Length of picture string */
	enum cb_category	category;	/* Field category */
	cob_u32_t		digits;		/* Number of digit places */
	int			scale;		/* 1/10^scale */
#if 0 /* currently unused */
	cob_u32_t		real_digits;	/* Real number of digits */
#endif
	cob_u32_t		have_sign;	/* Have 'S' */
	unsigned int flag_is_calculated	: 1;	/* is calculated */
	unsigned int variable_length	: 1;	/* Starts with 'L' */
};

#define CB_PICTURE(x)	(CB_TREE_CAST (CB_TAG_PICTURE, struct cb_picture, x))
#define CB_PICTURE_P(x)	(CB_TREE_TAG (x) == CB_TAG_PICTURE)

/* Key */

struct cb_key {
	cb_tree	key;			/* KEY */
	cb_tree	ref;			/* Reference used in SEARCH ALL */
	cb_tree	val;			/* Value to be compared in SEARCH ALL */
	int	dir;			/* ASCENDING or DESCENDING */
};

/* Field */

struct cb_field {
	struct cb_tree_common	common;		/* Common values */
	const char		*name;		/* Original name */
	const char		*ename;		/* Externalized name */
	cb_tree			depending;	/* OCCURS ... DEPENDING ON */
	cb_tree			values;		/* VALUE */
	cb_tree			false_88;	/* 88 FALSE clause */
	cb_tree			index_list;	/* INDEXED BY */
	cb_tree			external_form_identifier;	/* target of IDENTIFIED BY
								   (CGI template) */

	struct cb_field		*parent;	/* Upper level field (if any) */
	struct cb_field		*children;	/* Top of lower level fields */
	struct cb_field		*validation;	/* First level 88 field (if any) */
	struct cb_field		*sister;	/* Fields at the same level */
	struct cb_field		*redefines;	/* REDEFINES or RENAMES */
	struct cb_field		*rename_thru;	/* RENAMES THRU */
	struct cb_field		*index_qual;	/* INDEXED BY qualifier */
	struct cb_file		*file;		/* FD section file name */
	struct cb_cd		*cd;		/* CD name */
	struct cb_key		*keys;		/* SEARCH key */
	struct cb_picture	*pic;		/* PICTURE */
	struct cb_field		*vsize;		/* Variable size cache */
	struct cb_label		*debug_section;	/* DEBUG section */
	struct cb_report	*report;	/* RD section report name */

	struct cb_xref		xref;		/* xref elements */

	cb_tree			screen_line;	/* LINE */
	cb_tree			screen_column;	/* COLUMN */
	cb_tree			screen_from;	/* TO and USING */
	cb_tree			screen_to;	/* FROM and USING */
	cb_tree			screen_foreg;	/* FOREGROUND */
	cb_tree			screen_backg;	/* BACKGROUND */
	cb_tree			screen_prompt;	/* PROMPT */
	cb_tree			report_source;	/* SOURCE field */
	cb_tree			report_from;	/* SOURCE field subscripted; so MOVE to report_source */
	cb_tree			report_sum_counter;/* SUM counter */
	cb_tree			report_sum_list;/* SUM field(s) */
	cb_tree			report_sum_upon;/* SUM ... UPON detailname */
	cb_tree			report_reset;	/* RESET ON field */
	cb_tree			report_control;	/* CONTROL identifier */
	cb_tree			report_when;	/* PRESENT WHEN condition */
	cb_tree			report_column_list;/* List of Column Numbers */
	/* note: the following rw-specific fields are only set for parsing, no codegen in 3.x yet */
	cb_tree			report_vary_var;/* VARYING identifier */
	cb_tree			report_vary_from;/* VARYING FROM arith */
	cb_tree			report_vary_by;	/* VARYING BY arith */
#if 0 /* items from 4.x */
	const char		*report_source_txt;	/* SOURCE as text string */
	const char		*report_field_name;	/* Name used for this REPORT field */
	struct cb_field	*report_field_from;	/* 'field' used as SOURCE */
	int				report_field_offset;
	int				report_field_size;
#endif
	cb_tree			same_as;	/* SAME AS data-name (points to field) */
	cb_tree			external_definition;	/* by SAME AS / LIKE data-name or
											 by type-name (points to field) */
	cb_tree			like_modifier;	/* set for LIKE, may contain a length modifier */

	int			id;		/* Field id */
	int			size;		/* Field size */
	int			level;		/* Level number */
	int			memory_size;	/* Memory size */
	int			offset;		/* Byte offset from 01 level */
	int			occurs_min;	/* OCCURS <min> */
	int			occurs_max;	/* OCCURS [... TO] <max> */
	int			indexes;	/* Indices count (OCCURS) */

	int			count;		/* Reference count */
	int			mem_offset;	/* Memory offset */
	int			nkeys;		/* Number of keys */
	int			param_num;	/* CHAINING param number */
	cob_flags_t		screen_flag;	/* Flags used in SCREEN SECTION */
	int			report_flag;	/* Flags used in REPORT SECTION */
	int			report_line;	/* LINE */
	int			report_column;	/* COLUMN (first value) */
	int			report_num_col;	/* Number of COLUMNs defined */
	int			report_decl_id;	/* Label id of USE FOR REPORTING */
#if 0 /* items from 4.x */
	int			report_source_id;	/* Label id of MOVE SOURCE values */
#endif
	int			step_count;	/* STEP in REPORT */
	int			next_group_line;	/* NEXT GROUP [PLUS] line */
	unsigned int		vaddr;		/* Variable address cache */
	unsigned int		odo_level;	/* ODO level (0 = no ODO item)
						   could be direct ODO (check via depending)
						   or via subordinate) */
	enum cb_index_type	index_type;	/* Type of generated index */

	enum cb_storage		storage;	/* Storage section */
	enum cb_usage		usage;		/* USAGE */

	/* Flags */
	unsigned char flag_base;		/* Has memory allocation */
	unsigned char flag_external;		/* EXTERNAL */
	unsigned char flag_local_storage;	/* LOCAL storage */
	unsigned char flag_is_global;		/* Is GLOBAL */

	unsigned int flag_local		: 1;	/* Has local scope */
	unsigned int flag_occurs	: 1;	/* OCCURS */
	unsigned int flag_sign_clause	: 1;	/* Any SIGN clause */
	unsigned int flag_sign_separate	: 1;	/* SIGN IS SEPARATE */
	unsigned int flag_sign_leading	: 1;	/* SIGN IS LEADING */
	unsigned int flag_blank_zero	: 1;	/* BLANK WHEN ZERO */
	unsigned int flag_justified	: 1;	/* JUSTIFIED RIGHT */
	unsigned int flag_binary_swap	: 1;	/* Binary byteswap */

	unsigned int flag_real_binary	: 1;	/* BINARY-CHAR/SHORT/LONG/DOUBLE */
	unsigned int flag_is_pointer	: 1;	/* Is POINTER */
	unsigned int flag_item_78 	: 1;	/* Is a constant by 78 level,
										   01 CONSTANT or SYMBOLIC CONSTANT */
	unsigned int flag_any_length	: 1;	/* Is ANY LENGTH */
	unsigned int flag_item_based	: 1;	/* Is BASED */
	unsigned int flag_is_external_form : 1;		/* Is EXTERNAL-FORM */
	unsigned int flag_filler	: 1;	/* Implicit/explicit filler */
	unsigned int flag_synchronized	: 1;	/* SYNCHRONIZED */

	unsigned int flag_invalid	: 1;	/* Is broken */
	unsigned int flag_field		: 1;	/* Has been internally cached */
	unsigned int flag_chained	: 1;	/* CHAINING item */
	unsigned int flag_anylen_done	: 1;	/* ANY LENGTH is set up */
	unsigned int flag_is_verified	: 1;	/* Has been verified */
	unsigned int flag_is_c_long	: 1;	/* Is BINARY-C-LONG */
	unsigned int flag_is_pdiv_parm	: 1;	/* Is PROC DIV USING */
	unsigned int flag_is_pdiv_opt	: 1;	/* Is PROC DIV USING OPTIONAL */

	unsigned int flag_indexed_by	: 1;	/* INDEXED BY item */
	unsigned int flag_local_alloced	: 1;	/* LOCAL storage is allocated */
	unsigned int flag_no_init	: 1;	/* No initialize unless used */
	unsigned int flag_vsize_done	: 1;	/* Variable size cached */
	unsigned int flag_vaddr_done	: 1;	/* Variable address cached */
	unsigned int flag_odo_relative	: 1;	/* complex-odo: item address depends
							on size of a different (ODO) item */
	unsigned int flag_field_debug	: 1;	/* DEBUGGING */
	unsigned int flag_all_debug	: 1;	/* DEBUGGING */

	unsigned int flag_no_field	: 1;	/* SCREEN/REPORT dummy field */
	unsigned int flag_any_numeric	: 1;	/* Is ANY NUMERIC */
	unsigned int flag_is_returning	: 1;	/* Is RETURNING item */
	unsigned int flag_unbounded	: 1;	/* OCCURS UNBOUNDED */
	unsigned int flag_comp_1	: 1;	/* Is USAGE COMP-1 */
	unsigned int flag_volatile	: 1;	/* VOLATILE */
	unsigned int flag_constant	: 1;	/* Is 01 AS CONSTANT */
	unsigned int flag_internal_constant	: 1;	/* Is an internally generated CONSTANT */

	unsigned int flag_sync_left : 1;	/* SYNCHRONIZED LEFT */
	unsigned int flag_sync_right : 1;	/* SYNCHRONIZED RIGHT */
	unsigned int flag_internal_register	: 1;	/* Is an internally generated register */
	unsigned int flag_is_typedef : 1;	/* TYPEDEF  */
	unsigned int flag_occurs_values: 1;	/* OCCURS and multi VALUEs done */
	unsigned int flag_picture_l : 1;	/* Is USAGE PICTURE L */
};

#define CB_FIELD(x)		(CB_TREE_CAST (CB_TAG_FIELD, struct cb_field, x))
#define CB_FIELD_P(x)		(CB_TREE_TAG (x) == CB_TAG_FIELD)

#define CB_REF_OR_FIELD_P(x)	(CB_REFERENCE_P (x) || CB_FIELD_P (x))

#define CB_FIELD_PTR(x)		\
	(CB_REFERENCE_P (x) ? CB_FIELD (cb_ref (x)) : CB_FIELD (x))

/* Index */

#define CB_INDEX_OR_HANDLE_P(x)		cb_check_index_or_handle_p (x)

/* Label */

struct cb_para_label {
	struct cb_para_label	*next;
	struct cb_label		*para;
};

struct cb_alter_id {
	struct cb_alter_id	*next;
	int			goto_id;
};

struct cb_label {
	struct cb_tree_common	common;			/* Common values */
	const char		*name;			/* Name */
	const char		*orig_name;		/* Original name */
	struct cb_label		*section;		/* Parent SECTION */
	struct cb_label		*debug_section;		/* DEBUG SECTION */
	struct cb_para_label	*para_label;		/* SECTION Paragraphs */
	struct cb_xref		xref;			/* xref elements */
	cb_tree			exit_label;		/* EXIT label */
	struct cb_alter_id	*alter_gotos;		/* ALTER ids */
	int			id;			/* Unique id */
	int			section_id;		/* SECTION id */
	int			segment;		/* Segment number */

	unsigned int		flag_section		: 1;	/* Section */
	unsigned int		flag_entry		: 1;	/* Entry */
	unsigned int		flag_begin		: 1;	/* Begin label */
	unsigned int		flag_return		: 1;	/* End label */
	unsigned int		flag_real_label		: 1;	/* Is real label */
	unsigned int		flag_global		: 1;	/* GLOBAL */
	unsigned int		flag_declarative_exit	: 1;	/* Final EXIT */
	unsigned int		flag_declaratives	: 1;	/* DECLARATIVES */

	unsigned int		flag_fatal_check	: 1;	/* Fatal check */
	unsigned int		flag_dummy_section	: 1;	/* Dummy MAIN */
	unsigned int		flag_dummy_paragraph	: 1;	/* Dummy MAIN */
	unsigned int		flag_dummy_exit		: 1;	/* Dummy EXIT */
	unsigned int		flag_next_sentence	: 1;	/* NEXT SENTENCE */
	unsigned int		flag_default_handler	: 1;	/* Error handler */
	unsigned int		flag_statement		: 1;	/* Has statement */
	unsigned int		flag_first_is_goto	: 1;	/* 1st is GO TO */

	unsigned int		flag_alter		: 1;	/* ALTER code */
	unsigned int		flag_debugging_mode	: 1;	/* DEBUGGING MODE */
	unsigned int		flag_is_debug_sect	: 1;	/* DEBUGGING sect */
	unsigned int		flag_skip_label		: 1;	/* Skip label gen */
	unsigned int		flag_entry_for_goto	: 1;	/* is ENTRY FOR GO TO */
};

#define CB_LABEL(x)		(CB_TREE_CAST (CB_TAG_LABEL, struct cb_label, x))
#define CB_LABEL_P(x)		(CB_TREE_TAG (x) == CB_TAG_LABEL)

struct handler_struct {
	struct cb_label		*handler_label;		/* Handler label */
	struct cb_program	*handler_prog;		/* Handler program */
};

/* File */

struct cb_key_component {
	struct cb_key_component *next;
	cb_tree			component;		/* Field which is part of index */
};

struct cb_alt_key {
	struct cb_alt_key	*next;			/* Pointer to next */
	cb_tree			key;			/* Key item */
	cb_tree			password;			/* Password item */
	cb_tree			collating_sequence_key;	/* COLLATING */
	int			duplicates;		/* DUPLICATES */
	int			offset;			/* Offset from start */
	int			tf_suppress;		/* !0 for SUPPRESS */
	int			char_suppress;		/* character to test for suppress */
	struct cb_key_component	*component_list;	/* List of fields making up key */
};

/* How to interpret identifiers in a file's ASSIGN clause */
enum cb_assign_type {
	CB_ASSIGN_VARIABLE_DEFAULT,		/* default to ASSIGN variable, where allowed by implicit-assign-dynamic-var */
	CB_ASSIGN_VARIABLE_REQUIRED,		/* require ASSIGN variable */
	CB_ASSIGN_EXT_FILE_NAME_REQUIRED	/* require ASSIGN external-file-name */
};

struct cb_file {
	struct cb_tree_common	common;			/* Common values */
	const char		*name;			/* Original name */
	char			*cname;			/* Name used in C */
	/* SELECT */
	cb_tree			assign;			/* ASSIGN */
	cb_tree			file_status;		/* FILE STATUS */
	cb_tree			sharing;		/* SHARING */
	cb_tree			key;			/* Primary RECORD KEY */
	cb_tree			password;			/* Password item for file or primary key */
	struct cb_key_component	*component_list;	/* List of fields making up primary key */
	struct cb_alt_key	*alt_key_list;		/* ALTERNATE RECORD KEY */
	cb_tree			collating_sequence_key;	/* COLLATING */
	cb_tree			collating_sequence;	/* COLLATING */
	cb_tree			collating_sequence_n;	/* COLLATING FOR NATIONAL*/
	cb_tree			collating_sequence_keys;	/* list of postponed COLLATING OF */
	/* FD/SD */
	cb_tree			description_entry;	/* FD / SD entry rerference for warnings + errors */
	struct cb_field		*record;		/* Record descriptions */
	cb_tree			record_depending;	/* RECORD DEPENDING */
	cb_tree			reports;		/* REPORTS */
	cb_tree			linage;			/* LINAGE */
	cb_tree			linage_ctr;		/* LINAGE COUNTER */
	cb_tree			latfoot;		/* LINAGE FOOTING */
	cb_tree			lattop;			/* LINAGE TOP */
	cb_tree			latbot;			/* LINAGE BOTTOM */
	cb_tree			extfh;			/* EXTFH module to call for I/O */
	struct cb_label		*handler;		/* Error handler */
	struct cb_program	*handler_prog;		/* Prog where defined */
	struct cb_label		*debug_section;		/* DEBUG SECTION */
	struct cb_alphabet_name	*code_set;		/* CODE-SET */
	struct cb_list		*code_set_items;	/* CODE-SET FOR items */
	struct cb_xref		xref;			/* xref elements */
	int			record_min;		/* RECORD CONTAINS */
	int			record_max;		/* RECORD CONTAINS */
	int			optional;		/* OPTIONAL */
	int			organization;		/* ORGANIZATION - FIXME: use enum */
	int			access_mode;		/* ACCESS MODE - FIXME: use enum */
	int			lock_mode;		/* LOCK MODE */
	int			special;		/* Special file */
	int			same_clause;		/* SAME clause */
	enum cb_assign_type	assign_type;		/* How to interpret ASSIGN clause */
	unsigned int		flag_finalized	: 1;	/* Is finalized */
	unsigned int		flag_external	: 1;	/* Is EXTERNAL */
	unsigned int		flag_ext_assign	: 1;	/* ASSIGN EXTERNAL */
	unsigned int		flag_fileid	: 1;	/* ASSIGN DISK */
	unsigned int		flag_global	: 1;	/* Is GLOBAL */
	unsigned int		flag_fl_debug	: 1;	/* DEBUGGING */
	unsigned int		flag_line_adv	: 1;	/* LINE ADVANCING */
	unsigned int		flag_delimiter	: 1;	/* RECORD DELIMITER */
	unsigned int		flag_report	: 1;	/* Used by REPORT */
	/* Implied RECORD VARYING limits need checking */
	unsigned int		flag_check_record_varying_limits	: 1;
	/* Whether the file's ASSIGN is like "ASSIGN word", not "ASSIGN
           EXTERNAL/DYNAMIC/USING/... word" */
	unsigned int		flag_assign_no_keyword : 1;
	/* Exceptions enabled for file */
	struct cb_exception	*exception_table;
};

#define CB_FILE(x)	(CB_TREE_CAST (CB_TAG_FILE, struct cb_file, x))
#define CB_FILE_P(x)	(CB_TREE_TAG (x) == CB_TAG_FILE)

/* Communication description */

struct cb_cd {
	struct cb_tree_common	common;			/* Common values */
	const char		*name;			/* Name */
	struct cb_field		*record;		/* Record descriptions */
	struct cb_label		*debug_section;		/* DEBUG section */
	int			flag_field_debug;	/* DEBUGGING */
};

#define CB_CD(x)	(CB_TREE_CAST (CB_TAG_CD, struct cb_cd, x))
#define CB_CD_P(x)	(CB_TREE_TAG (x) == CB_TAG_CD)

/* Reference */

struct cb_word {
	struct cb_word	*next;		/* Next word with the same hash value */
	const char	*name;		/* Word name */
	cb_tree		items;		/* Objects associated with this word */
	int		count;		/* Number of words with the same name */
	int		error;		/* Set to -1 if warning raised for that, -1 if error detected */
};

#define CB_WORD_TABLE_SIZE	(CB_WORD_HASH_SIZE * sizeof (struct cb_word))

struct cb_reference {
	struct cb_tree_common	common;		/* Common values */
	cb_tree			chain;		/* Next qualified name */
	cb_tree			value;		/* Item referred to */
	cb_tree			subs;		/* List of subscripts */
	cb_tree			offset;		/* Reference mod offset */
	cb_tree			length;		/* Reference mod length */
	cb_tree			check;		/* Runtime checks */
	struct cb_word		*word;		/* Pointer to word list */
	struct cb_label		*section;	/* Current section */
	struct cb_label		*paragraph;	/* Current paragraph */
	struct cb_label		*debug_section;	/* Debug section */
	size_t			hashval;	/* Hash value of name */

	unsigned int		flag_receiving	: 1;	/* Reference target */
	unsigned int		flag_all	: 1;	/* ALL */
	unsigned int		flag_in_decl	: 1;	/* In DECLARATIVE */
	unsigned int		flag_decl_ok	: 1;	/* DECLARATIVE ref OK  */
	unsigned int		flag_alter_code	: 1;	/* Needs ALTER code */
	unsigned int		flag_debug_code	: 1;	/* Needs DEBUG code */
	unsigned int		flag_all_debug	: 1;	/* Needs ALL DEBUG code */
	unsigned int		flag_target	: 1;	/* DEBUG item is target */

	unsigned int		flag_optional	: 1;	/* Definition optional */
	unsigned int		flag_ignored	: 1;	/* Part of ignored code */
	unsigned int		flag_filler_ref	: 1;	/* Ref to FILLER */
	unsigned int		flag_duped	: 1;	/* Duplicate name */
};

#define CB_REFERENCE(x)		(CB_TREE_CAST (CB_TAG_REFERENCE, struct cb_reference, x))
#define CB_REFERENCE_P(x)	(CB_TREE_TAG (x) == CB_TAG_REFERENCE)

#define CB_WORD(x)		(CB_REFERENCE (x)->word)
#define CB_NAME(x)		(CB_REFERENCE (x)->word->name)
#define CB_WORD_COUNT(x)	(CB_REFERENCE (x)->word->count)
#define CB_WORD_ITEMS(x)	(CB_REFERENCE (x)->word->items)

/* Binary operation */

/*
  '+'	x + y
  '-'	x - y
  '*'	x * y
  '/'	x / y
  '^'	x ** y
  '='	x = y
  '>'	x > y
  '<'	x < y
  '['	x <= y
  ']'	x >= y
  '~'	x != y
  '!'	not x
  '&'	x and y
  '|'	x or y
  '@'	( x )
*/

struct cb_binary_op {
	struct cb_tree_common	common;		/* Common values */
	cb_tree			x;		/* LHS */
	cb_tree			y;		/* RHS */
	int			op;		/* Operation */
	unsigned int		flag;		/* Special usage */
};

#define CB_BINARY_OP(x)		(CB_TREE_CAST (CB_TAG_BINARY_OP, struct cb_binary_op, x))
#define CB_BINARY_OP_P(x)	(CB_TREE_TAG (x) == CB_TAG_BINARY_OP)

/* Function call */

struct cb_funcall {
	struct cb_tree_common	common;		/* Common values */
	const char		*name;		/* Function name */
	cb_tree			argv[11];	/* Function arguments */
	int			argc;		/* Number of arguments */
	int			varcnt;		/* Variable argument count */
	unsigned int		screenptr;	/* SCREEN usage */
	unsigned int		nolitcast;	/* No cast for literals */
};

#define CB_FUNCALL(x)		(CB_TREE_CAST (CB_TAG_FUNCALL, struct cb_funcall, x))
#define CB_FUNCALL_P(x)		(CB_TREE_TAG (x) == CB_TAG_FUNCALL)

/* Type cast */

struct cb_cast {
	struct cb_tree_common	common;		/* Common values */
	cb_tree			val;
	enum cb_cast_type	cast_type;
};

#define CB_CAST(x)	(CB_TREE_CAST (CB_TAG_CAST, struct cb_cast, x))
#define CB_CAST_P(x)	(CB_TREE_TAG (x) == CB_TAG_CAST)

/* Assign */

struct cb_assign {
	struct cb_tree_common	common;		/* Common values */
	cb_tree			var;
	cb_tree			val;
};

#define CB_ASSIGN(x)		(CB_TREE_CAST (CB_TAG_ASSIGN, struct cb_assign, x))
#define CB_ASSIGN_P(x)		(CB_TREE_TAG (x) == CB_TAG_ASSIGN)

/* Compiler features like directives, functions, mnemonics and registers */

enum cb_feature_mode {
	CB_FEATURE_ACTIVE = 0,	/* 0 Feature is implemented and not disabled */
	CB_FEATURE_DISABLED,		/* 1 Feature disabled */
	CB_FEATURE_MUST_BE_ENABLED,		/* 2 Feature disabled, if not explicit enabled */
	CB_FEATURE_NOT_IMPLEMENTED	/* 3 Feature known but not yet implemented */
};

/* Intrinsic FUNCTION */

struct cb_intrinsic_table {
	const char		*name;		/* FUNCTION NAME */
	const char		*intr_routine;	/* Routine name */
	const enum cb_intr_enum	intr_enum;	/* Enum intrinsic */
	const int		token;		/* Token value */
	enum cb_feature_mode	active;	/* Have we implemented it? Is it active? */
	const int		args;		/* Maximum number of arguments, -1 = unlimited */
	const int		min_args;	/* Minimum number of arguments */
	const enum cb_category	category;	/* Category */
	const unsigned int	refmod;		/* Can be refmodded */
};

struct cb_intrinsic {
	struct cb_tree_common		common;		/* Common values */
	cb_tree				name;		/* INTRINSIC name */
	cb_tree				args;		/* Arguments */
	cb_tree				intr_field;	/* Field to use */
	const struct cb_intrinsic_table	*intr_tab;	/* Table pointer */
	cb_tree				offset;		/* Reference mod */
	cb_tree				length;		/* Reference mod */
	int				isuser;		/* User function */
};

#define CB_INTRINSIC(x)		(CB_TREE_CAST (CB_TAG_INTRINSIC, struct cb_intrinsic, x))
#define CB_INTRINSIC_P(x)	(CB_TREE_TAG (x) == CB_TAG_INTRINSIC)

/* INITIALIZE */

struct cb_initialize {
	struct cb_tree_common	common;			/* Common values */
	cb_tree			var;			/* Field */
	cb_tree			val;			/* Value */
	cb_tree			rep;			/* Replacing */
	unsigned char		flag_default;		/* Default */
	unsigned char		flag_init_statement;	/* INITIALIZE statement */
	unsigned char		flag_no_filler_init;	/* No FILLER initialize */
	unsigned char		padding;		/* Padding */
};

#define CB_INITIALIZE(x)	(CB_TREE_CAST (CB_TAG_INITIALIZE, struct cb_initialize, x))
#define CB_INITIALIZE_P(x)	(CB_TREE_TAG (x) == CB_TAG_INITIALIZE)

/* SEARCH */

struct cb_search {
	struct cb_tree_common	common;		/* Common values */
	cb_tree			table;		/* Reference to table name */
	cb_tree			var;		/* VARYING field */
	cb_tree			at_end;		/* AT END (pair of position and statements) */
	cb_tree			whens;		/* WHEN (conditions and statements)
	       			      		   [for not SEARCH ALL: list of those] */
	int			flag_all;	/* SEARCH ALL */
};

#define CB_SEARCH(x)		(CB_TREE_CAST (CB_TAG_SEARCH, struct cb_search, x))
#define CB_SEARCH_P(x)		(CB_TREE_TAG (x) == CB_TAG_SEARCH)

/* XML PARSE */

struct cb_xml_parse {
	struct cb_tree_common	common;		/* Common values */
	cb_tree			data;		/* XML data (field identifier) */
	cb_tree			proc;		/* PROCESSING PROCEDURE (internally as PERFORM ...) */
	cb_tree			encoding;		/* ENCODING codepage (optional) */
	cb_tree			validating;		/* VALIDATING source (optional) */
	int			returning_national;	/* RETURNING NATIONAL */
};

#define CB_XML_PARSE(x)		(CB_TREE_CAST (CB_TAG_XML_PARSE, struct cb_xml_parse, x))
#define CB_XML_PARSE_P(x)		(CB_TREE_TAG (x) == CB_TAG_XML_PARSE)

/* CALL */

struct cb_call {
	struct cb_tree_common	common;		/* Common values */
	cb_tree			name;		/* CALL name */
	cb_tree			args;		/* Arguments */
	cb_tree			stmt1;		/* ON EXCEPTION */
	cb_tree			stmt2;		/* NOT ON EXCEPTION */
	cb_tree			call_returning;	/* RETURNING */
	cob_u32_t		is_system;	/* System call */
	int			convention;	/* CALL convention */
};

#define CB_CALL(x)		(CB_TREE_CAST (CB_TAG_CALL, struct cb_call, x))
#define CB_CALL_P(x)		(CB_TREE_TAG (x) == CB_TAG_CALL)

/* CANCEL */

struct cb_cancel {
	struct cb_tree_common	common;		/* Common values */
	cb_tree			target;		/* CANCEL target(s) */
};

#define CB_CANCEL(x)		(CB_TREE_CAST (CB_TAG_CANCEL, struct cb_cancel, x))
#define CB_CANCEL_P(x)		(CB_TREE_TAG (x) == CB_TAG_CANCEL)

/* ALTER */

struct cb_alter {
	struct cb_tree_common	common;		/* Common values */
	cb_tree			source;		/* ALTER source paragraph */
	cb_tree			target;		/* ALTER target GO TO paragraph */
};

#define CB_ALTER(x)		(CB_TREE_CAST (CB_TAG_ALTER, struct cb_alter, x))
#define CB_ALTER_P(x)		(CB_TREE_TAG (x) == CB_TAG_ALTER)

/* GO TO */

struct cb_goto {
	struct cb_tree_common	common;		/* Common values */
	cb_tree			target;		/* Procedure name(s) */
	cb_tree			depending;	/* DEPENDING */
};

#define CB_GOTO(x)		(CB_TREE_CAST (CB_TAG_GOTO, struct cb_goto, x))
#define CB_GOTO_P(x)		(CB_TREE_TAG (x) == CB_TAG_GOTO)

/* IF and WHEN and PRESENT WHEN */

struct cb_if {
	struct cb_tree_common	common;		/* Common values */
	cb_tree			test;		/* Condition */
	cb_tree			stmt1;		/* Statement list */
	cb_tree			stmt2;		/* ELSE/WHEN statement list */
	unsigned int		is_if;		/* From IF (1), WHEN (0), PRESENT WHEN (3+4) */
};

#define CB_IF(x)		(CB_TREE_CAST (CB_TAG_IF, struct cb_if, x))
#define CB_IF_P(x)		(CB_TREE_TAG (x) == CB_TAG_IF)

/* PERFORM */

struct cb_perform_varying {
	struct cb_tree_common	common;		/* Common values */
	cb_tree			name;		/* VARYING item */
	cb_tree			from;		/* FROM */
	cb_tree			step;		/* Increment */
	cb_tree			until;		/* UNTIL */
};

struct cb_perform {
	struct cb_tree_common	common;		/* Common values */
	cb_tree			test;		/* Condition */
	cb_tree			body;		/* Statements */
	cb_tree			data;		/* TIMES or procedure */
	cb_tree			varying;	/* VARYING */
	cb_tree			exit_label;	/* Implicit exit label */
	cb_tree			cycle_label;	/* EXIT PERFORM CYCLE */
	enum cb_perform_type	perform_type;	/* Perform type */
};

#define CB_PERFORM_VARYING(x)	(CB_TREE_CAST (CB_TAG_PERFORM_VARYING, struct cb_perform_varying, x))

#define CB_PERFORM(x)		(CB_TREE_CAST (CB_TAG_PERFORM, struct cb_perform, x))
#define CB_PERFORM_P(x)		(CB_TREE_TAG (x) == CB_TAG_PERFORM)

/* Struct for extended ACCEPT / DISPLAY */

struct cb_attr_struct {
	cb_tree			fgc;		/* FOREGROUND COLOR */
	cb_tree			bgc;		/* BACKGROUND COLOR */
	cb_tree			scroll;		/* SCROLL */
	cb_tree			timeout;	/* TIMEOUT */
	cb_tree			prompt;		/* PROMPT */
	cb_tree			size_is;	/* [PROTECTED] SIZE [IS] */
	cob_flags_t		dispattrs;	/* Attributes */
};

/* Exception handler type */

enum cb_handler_type {
	NO_HANDLER = 0,
	DISPLAY_HANDLER,
	ACCEPT_HANDLER,
	SIZE_ERROR_HANDLER,
	OVERFLOW_HANDLER,
	AT_END_HANDLER,
	EOP_HANDLER,
	INVALID_KEY_HANDLER,
	XML_HANDLER,
	JSON_HANDLER,
	MCS_HANDLER
};

/* Statement */

struct cb_statement {
	struct cb_tree_common	common;			/* Common values */
	const char		*name;			/* Statement name */
	cb_tree			body;			/* Statement body */
	cb_tree			file;			/* File reference */
	cb_tree			ex_handler;		/* Exception handler */
	cb_tree			not_ex_handler;		/* Exception handler */
	cb_tree			handler3;		/* INTO clause */
	cb_tree			null_check;		/* NULL check */
	cb_tree			debug_check;		/* Field DEBUG */
	cb_tree			debug_nodups;		/* Field DEBUG dups */
	struct cb_attr_struct	*attr_ptr;		/* Attributes */
	enum cb_handler_type	handler_type;		/* Handler type */
	unsigned int		flag_no_based	: 1;	/* Check BASED */
	unsigned int		flag_in_debug	: 1;	/* In DEBUGGING */
	unsigned int		flag_merge	: 1;	/* Is MERGE */
	unsigned int		flag_callback	: 1;	/* DEBUG Callback */
	unsigned int		flag_implicit	: 1;	/* Is an implicit statement */
};

#define CB_STATEMENT(x)		(CB_TREE_CAST (CB_TAG_STATEMENT, struct cb_statement, x))
#define CB_STATEMENT_P(x)	(CB_TREE_TAG (x) == CB_TAG_STATEMENT)

/* CONTINUE (*not* CONTINUE AFTER exp SECONDS) */

struct cb_continue {
	struct cb_tree_common	common;		/* Common values */
};

#define CB_CONTINUE(x)		(CB_TREE_CAST (CB_TAG_CONTINUE, struct cb_continue, x))
#define CB_CONTINUE_P(x)	(CB_TREE_TAG (x) == CB_TAG_CONTINUE)

/* SET ATTRIBUTE */

struct cb_set_attr {
	struct cb_tree_common	common;		/* Common values */
	struct cb_field		*fld;
	cob_flags_t		val_on;
	cob_flags_t		val_off;
};

#define CB_SET_ATTR(x)		(CB_TREE_CAST (CB_TAG_SET_ATTR, struct cb_set_attr, x))
#define CB_SET_ATTR_P(x)	(CB_TREE_TAG (x) == CB_TAG_SET_ATTR)

/* List */

struct cb_list {
	struct cb_tree_common	common;		/* Common values */
	cb_tree			chain;		/* Next in list */
	cb_tree			value;		/* Reference to item(s) */
	cb_tree			purpose;	/* Purpose */
	int			sizes;		/* BY VALUE SIZE */
};

#define CB_LIST(x)	(CB_TREE_CAST (CB_TAG_LIST, struct cb_list, x))
#define CB_LIST_P(x)	(CB_TREE_TAG (x) == CB_TAG_LIST)

#define CB_PURPOSE(x)			(CB_LIST (x)->purpose)
#define CB_VALUE(x)			(CB_LIST (x)->value)
#define CB_CHAIN(x)			(CB_LIST (x)->chain)
#define CB_SIZES(x)			(CB_LIST (x)->sizes)

#define CB_PURPOSE_INT(x)		(CB_INTEGER (CB_PURPOSE (x))->val)

#define CB_SIZES_INT(x)			((CB_LIST (x)->sizes) & 0x7F)
#define CB_SIZES_INT_UNSIGNED(x)	((CB_LIST (x)->sizes) & CB_SIZE_UNSIGNED)

/* Pair */

#define CB_PAIR_P(x)		(CB_LIST_P (x) && CB_PAIR_X (x))
#define CB_PAIR_X(x)		CB_PURPOSE (x)
#define CB_PAIR_Y(x)		CB_VALUE (x)

/* preprocessor definitions */

/* Flex directive actions */
enum cb_directive_action {
	PLEX_ACT_IF =	0,
	PLEX_ACT_ELSE =	1U,
	PLEX_ACT_END =	2U,
	PLEX_ACT_ELIF =	3U
};

/* Flex value types */
enum cb_definition_type {
	PLEX_DEF_NONE	=	0,
	PLEX_DEF_LIT	=	1U,
	PLEX_DEF_NUM	=	2U,
	PLEX_DEF_DEL	=	3U
};

/* Generic define list structure */
struct cb_define_struct {
	struct cb_define_struct	*next;			/* next pointer */
	struct cb_define_struct	*last;
	char			*name;
	char			*value;
	enum cb_definition_type		deftype;
	int			sign;
	int			int_part;
	int			dec_part;
};

extern void		ppp_clear_lists (void);
extern void		plex_clear_vars (void);
extern void		plex_clear_all (void);
extern void		plex_call_destroy (void);

extern void		ppparse_clear_vars (const struct cb_define_struct *);
extern struct cb_define_struct *ppp_search_lists (const char *name);

extern void		plex_action_directive (const enum cb_directive_action,
	const unsigned int);

/* Report */

struct cb_report {
	struct cb_tree_common	common;		/* Common values */
	const char		*name;		/* Original name */
	char			*cname;		/* Name used in C */
	struct cb_file		*file;		/* File */
	cb_tree			line_counter;	/* LINE-COUNTER */
	cb_tree			page_counter;	/* PAGE-COUNTER */
	cb_tree			code_clause;	/* CODE */
	cb_tree			controls;	/* CONTROLS */
	cb_tree			t_lines;	/* PAGE LIMIT LINES */
	cb_tree			t_columns;	/* PAGE LIMIT COLUMNS */
	cb_tree			t_heading;	/* HEADING */
	cb_tree			t_first_detail;	/* FIRST DE */
	cb_tree			t_last_control;	/* LAST CH */
	cb_tree			t_last_detail;	/* LAST DE */
	cb_tree			t_footing;	/* FOOTING */
	int			lines;		/* PAGE LIMIT LINES */
	int			columns;	/* PAGE LIMIT COLUMNS */
	int			heading;	/* HEADING */
	int			first_detail;	/* FIRST DE */
	int			last_control;	/* LAST CH */
	int			last_detail;	/* LAST DE */
	int			footing;	/* FOOTING */
	struct cb_field		*records;	/* First record definition of report */
	int			num_lines;	/* Number of Lines defined */
	struct cb_field		**line_ids;	/* array of LINE definitions */
	int			num_sums;	/* Number of SUM counters defined */
	struct cb_field		**sums;		/* Array of SUM fields */
	int			rcsz;		/* Longest record */
	int			id;		/* unique id for this report */
	unsigned int		control_final:1;/* CONTROL FINAL declared */
	unsigned int		global:1;	/* IS GLOBAL declared */
	unsigned int		has_declarative:1;/* Has Declaratives Code to be executed */
	unsigned int		has_detail:1;	/* Has DETAIL line */
	unsigned int		has_source_move:1;/* Has Code to MOVE SOURCE values */
	unsigned int		was_checked:1;
};

#define CB_REPORT(x)	(CB_TREE_CAST (CB_TAG_REPORT, struct cb_report, x))
#define CB_REPORT_P(x)	(CB_TREE_TAG (x) == CB_TAG_REPORT)

#define CB_REF_OR_REPORT_P(x)	\
	(CB_REFERENCE_P (x) ? CB_REPORT_P (cb_ref (x)) : CB_REPORT_P (x))

#define CB_REPORT_PTR(x)		\
	(CB_REFERENCE_P (x) ? CB_REPORT	  (cb_ref (x)) : CB_REPORT (x))

/* Mark-up Language output (JSON/XML GENERATE) tree */

enum cb_ml_type {
	CB_ML_ATTRIBUTE,
	CB_ML_ELEMENT,
	CB_ML_CONTENT,
	CB_ML_ANY_TYPE
};

struct cb_ml_generate_tree {
	struct cb_tree_common		common;
	/* Name of the ML element to generate */
	cb_tree				name;
	/* The type of the ML element to generate */
	enum cb_ml_type			type;
	/* The content of the ML element to generate */
	cb_tree			        value;
	/* The condition under which generation of the element is suppressed */
	cb_tree				suppress_cond;
	/* ID for this struct when output */
	int				id;
	/* Attributes for this element */
	struct cb_ml_generate_tree	*attrs;
	/* Parent ML element */
	struct cb_ml_generate_tree	*parent;
	/* Children ML elements */
	struct cb_ml_generate_tree	*children;
	/* Preceding ML elements */
	struct cb_ml_generate_tree	*prev_sibling;
	/* Following ML elements */
	struct cb_ml_generate_tree	*sibling;
};

#define CB_ML_TREE(x)		(CB_TREE_CAST (CB_TAG_ML_TREE, struct cb_ml_generate_tree, x))
#define CB_ML_TREE_P(x)	(CB_TREE_TAG (x) == CB_TAG_ML_TREE)

/* Program */

struct nested_list {
	struct nested_list	*next;
	struct cb_program	*nested_prog;
};

struct cb_program {
	struct cb_tree_common	common;		/* Common values */

	/* Program variables */
	struct cb_program	*next_program;		/* Nested/contained */
	struct cb_program	*next_program_ordered;	/* Nested/contained
							   when cb_correct_program_order is set */
	const char		*program_name;		/* Internal program-name */
	const char		*program_id;		/* Demangled external PROGRAM-ID */
	char			*source_name;		/* Source name */
	char			*orig_program_id;	/* Original external PROGRAM-ID */
	struct cb_word		**word_table;		/* Name hash table */
	struct local_filename	*local_include;		/* Local include info */
	struct nested_list	*nested_prog_list;	/* Callable contained */
	struct nested_list	*common_prog_list;	/* COMMON contained */
	cb_tree			entry_list;		/* Entry point list */
	cb_tree			entry_list_goto;	/* Special Entry point list */
	cb_tree			file_list;		/* File list */
	cb_tree			cd_list;		/* CD list */
	cb_tree			exec_list;		/* Executable statements */
	cb_tree			label_list;		/* Label list */
	cb_tree			reference_list;		/* Reference list */
	cb_tree			alphabet_name_list;	/* ALPHABET list */
	cb_tree			symbolic_char_list;	/* SYMBOLIC list */
	cb_tree			class_name_list;	/* CLASS list */
	cb_tree			schema_name_list;	/* XML-SCHEMA list */
	cb_tree			parameter_list;		/* USING parameters */
	cb_tree			locale_list;		/* LOCALE list */
	cb_tree			global_list;		/* GLOBAL list */
	cb_tree			report_list;		/* REPORT list */
	cb_tree			alter_list;		/* ALTER list */
	cb_tree			debug_list;		/* DEBUG ref list */
	cb_tree			cb_return_code;		/* RETURN-CODE */
	cb_tree			cb_sort_return;		/* SORT-RETURN */
	cb_tree			cb_call_params;		/* Number of CALL params */
	cb_tree			mnemonic_spec_list;	/* MNEMONIC spec */
	cb_tree			class_spec_list;	/* CLASS spec */
	cb_tree			interface_spec_list;	/* INTERFACE spec */
	cb_tree			function_spec_list;	/* FUNCTION spec */
	cb_tree			user_spec_list;		/* User FUNCTION spec */
	cb_tree			program_spec_list;	/* PROGRAM spec */
	cb_tree			property_spec_list;	/* PROPERTY spec */
	struct cb_alter_id	*alter_gotos;		/* ALTER ids */
	struct cb_field		*working_storage;	/* WORKING-STORAGE */
	struct cb_field		*local_storage;		/* LOCAL-STORAGE */
	struct cb_field		*linkage_storage;	/* LINKAGE */
	struct cb_field		*screen_storage;	/* SCREEN */
	struct cb_field		*report_storage;	/* REPORT */
	cb_tree			local_file_list;	/* Local files */
	cb_tree			global_file_list;	/* Global files */
	struct handler_struct	global_handler[5];	/* Global handlers */
	cb_tree			collating_sequence;	/* COLLATING */
	cb_tree			collating_sequence_n;	/* COLLATING FOR NATIONAL*/
	cb_tree			classification;		/* CLASSIFICATION */
	cb_tree			apply_commit;		/* APPLY COMMIT file- and data-items */
	cb_tree			cursor_pos;		/* CURSOR */
	cb_tree			crt_status;		/* CRT STATUS */
	cb_tree			xml_code;		/* XML-CODE */
	cb_tree			xml_event;		/* XML-EVENT */
	cb_tree			xml_information;	/* XML-INFORMATION */
	cb_tree			xml_namespace;		/* XML-NAMESPACE */
	cb_tree			xml_nnamespace;		/* XML-NNAMESPACE */
	cb_tree			xml_namespace_prefix;	/* XML-NAMESPACE-PREFIX */
	cb_tree			xml_nnamespace_prefix;	/* XML-NNAMESPACE-PREFIX */
	cb_tree			xml_ntext;		/* XML-NTEXT */
	cb_tree			xml_text;		/* XML-TEXT */
	cb_tree			json_code;		/* JSON-CODE */
	cb_tree			json_status;		/* JSON-STATUS */
	cb_tree			returning;		/* RETURNING */
	struct cb_label		*all_procedure;		/* DEBUGGING */
	struct cb_call_xref	call_xref;		/* CALL Xref list */
	struct cb_ml_generate_tree	*ml_trees;	/* XML GENERATE trees */
	const char		*extfh;		/* CALLFH for this program */

	int			last_source_line;	/* Line of (implicit) END PROGRAM/FUNCTION */

	/* Internal variables */
	int		loop_counter;			/* Loop counters */
	unsigned int	decimal_index;			/* cob_decimal count of this program */
	unsigned int	decimal_index_max;		/* program group's max cob_decimal */
	int		nested_level;			/* Nested program level */
	unsigned int	num_proc_params;		/* PROC DIV params */
	int		toplev_count;			/* Top level source count */
	unsigned int	max_call_param;			/* Max params */

	unsigned char	decimal_point;			/* '.' or ',' */
	unsigned char	currency_symbol;		/* '$' or user-specified */
	unsigned char	numeric_separator;		/* ',' or '.' */
	unsigned char	prog_type;			/* Program type (program = 0, function = 1) */
	cb_tree			entry_convention;	/* ENTRY convention / PROCEDURE convention */

	unsigned int	flag_main		: 1;	/* Gen main function */
	unsigned int	flag_common		: 1;	/* COMMON PROGRAM */
	unsigned int	flag_initial		: 1;	/* INITIAL PROGRAM */
	unsigned int	flag_recursive		: 1;	/* RECURSIVE PROGRAM */
	unsigned int	flag_screen		: 1;	/* Have SCREEN SECTION */
	unsigned int	flag_validated		: 1;	/* End program validate */
	unsigned int	flag_chained		: 1;	/* PROCEDURE CHAINING */
	unsigned int	flag_global_use		: 1;	/* USE GLOBAL */

	unsigned int	flag_gen_error		: 1;	/* Gen error routine */
	unsigned int	flag_file_global	: 1;	/* Global FD */
	unsigned int	flag_has_external	: 1;	/* Has EXTERNAL */
	unsigned int	flag_segments		: 1;	/* Has segments */
	unsigned int	flag_trailing_separate	: 1;	/* TRAILING SEPARATE */
	unsigned int	flag_console_is_crt	: 1;	/* CONSOLE IS CRT */
	unsigned int	flag_debugging		: 1;	/* DEBUGGING MODE */
	unsigned int	flag_gen_debug		: 1;	/* DEBUGGING MODE */

	unsigned int	flag_save_exception	: 1;	/* Save exception code */
	unsigned int	flag_report		: 1;	/* Have REPORT SECTION */
	unsigned int	flag_void		: 1;	/* void return for subprogram */
	unsigned int	flag_decimal_comp	: 1;	/* program group has decimal computations */
	unsigned int	flag_prototype		: 1;	/* Is a prototype */
};

#define CB_PROGRAM(x)	(CB_TREE_CAST (CB_TAG_PROGRAM, struct cb_program, x))
#define CB_PROGRAM_P(x)	(CB_TREE_TAG (x) == CB_TAG_PROGRAM)

/* Function prototype */

struct cb_prototype {
	struct cb_tree_common	common;
	/* Name of prototype in the REPOSITORY */
	const char		*name;
	/* External name of the prototype/definition */
	const char		*ext_name;
	int			type;
};

#define CB_PROTOTYPE(x)		(CB_TREE_CAST (CB_TAG_PROTOTYPE, struct cb_prototype, x))
#define CB_PROTOTYPE_P(x)	(CB_TREE_TAG (x) == CB_TAG_PROTOTYPE)

/* JSON/XML GENERATE SUPPRESS clause */

enum cb_ml_suppress_target {
	CB_ML_SUPPRESS_IDENTIFIER,
	CB_ML_SUPPRESS_ALL,
	CB_ML_SUPPRESS_TYPE
};

enum cb_ml_suppress_category {
	CB_ML_SUPPRESS_CAT_NUMERIC,
	CB_ML_SUPPRESS_CAT_NONNUMERIC,
	CB_ML_SUPPRESS_CAT_ANY
};

struct cb_ml_suppress_clause {
	struct cb_tree_common		common;
	/* What thing(s) the SUPPRESS clause applies to */
	enum cb_ml_suppress_target	target;
	/* If the target is IDENTIFIER, then the item targetted */
	cb_tree				identifier;
	/* What values the thing(s) should have to be SUPPRESSed */
	cb_tree				when_list;
	/* If the target is TYPE, then the type of ML elements to apply to */
	enum cb_ml_type		ml_type;
	/* If the target is TYPE, then the categories of items (of ML type
	   ml_type) to apply to */
	enum cb_ml_suppress_category	category;
};

#define CB_ML_SUPPRESS(x)	(CB_TREE_CAST (CB_TAG_ML_SUPPRESS, struct cb_ml_suppress_clause, x))
#define CB_ML_SUPPRESS_P(x)	(CB_TREE_TAG (x) == CB_TAG_ML_SUPPRESS)

struct cb_ml_suppress_checks {
	struct cb_tree_common		common;
	struct cb_ml_generate_tree	*tree;
};

#define CB_ML_SUPPRESS_CHECKS(x)	(CB_TREE_CAST (CB_TAG_ML_SUPPRESS_CHECKS, struct cb_ml_suppress_checks, x))
#define CB_ML_SUPPRESS_CHECKS_P(x)	(CB_TREE_TAG (x) == CB_TAG_ML_SUPPRESS_CHECKS)

/* DISPLAY type */

enum cb_display_type {
	UNKNOWN_DISPLAY,
	SCREEN_DISPLAY,
	FIELD_ON_SCREEN_DISPLAY,
	DEVICE_DISPLAY,
	MIXED_DISPLAY
};

/* INSPECT clauses */

enum cb_inspect_clause {
	TALLYING_CLAUSE,
	REPLACING_CLAUSE,
	CONVERTING_CLAUSE,
	/* This is what happens when you support OS/VS COBOL. */
	TRANSFORM_STATEMENT
};

/* Functions/variables */

/* tree.c */

extern cb_tree			cb_any;
extern cb_tree			cb_true;
extern cb_tree			cb_false;
extern cb_tree			cb_null;
extern cb_tree			cb_zero;
extern cb_tree			cb_one;
extern cb_tree			cb_space;
extern cb_tree			cb_low;
extern cb_tree			cb_high;
extern cb_tree			cb_norm_low;
extern cb_tree			cb_norm_high;
extern cb_tree			cb_quote;
extern cb_tree			cb_int0;
extern cb_tree			cb_int1;
extern cb_tree			cb_int2;
extern cb_tree			cb_int3;
extern cb_tree			cb_int4;
extern cb_tree			cb_int5;
extern cb_tree			cb_int6;
extern cb_tree			cb_int7;
extern cb_tree			cb_int8;
extern cb_tree			cb_int16;
extern cb_tree			cb_i[COB_MAX_SUBSCRIPTS];
extern cb_tree			cb_error_node;

extern cb_tree			cb_intr_whencomp;

extern cb_tree			cb_standard_error_handler;
extern cb_tree			cb_depend_check;

extern unsigned int		gen_screen_ptr;

extern char			*cb_name (cb_tree);
extern char			*cb_name_errmsg (cb_tree);
extern cb_tree			cb_exhbit_literal (cb_tree);
extern enum cb_class		cb_tree_class (cb_tree);
extern enum cb_category		cb_tree_category (cb_tree);
extern int			cb_tree_type (const cb_tree,
					      const struct cb_field *);
extern int			cb_category_is_alpha (cb_tree);
extern int			cb_category_is_national (cb_tree);
extern int			cb_fits_int (const cb_tree);
extern int			cb_fits_long_long (const cb_tree);
extern int			cb_get_int (const cb_tree);
extern cob_s64_t		cb_get_long_long (const cb_tree);
extern cob_u64_t		cb_get_u_long_long (const cb_tree);

extern void			cb_init_parse_constants (void);

extern cb_tree			cb_int (const int);
extern cb_tree			cb_int_hex (const int);

extern cb_tree			cb_build_string (const void *, const size_t);

extern cb_tree			cb_flags_t (const cob_flags_t);

extern cb_tree			cb_build_class_name (cb_tree, cb_tree);

extern cb_tree			cb_build_locale_name (cb_tree, cb_tree);

extern cb_tree			cb_build_numeric_literal (int,
							  const void *,
							  const int);
extern cb_tree			cb_build_alphanumeric_literal (const void *,
							       const size_t);
extern cb_tree			cb_build_national_literal (const void *,
							       const size_t);
extern cb_tree			cb_build_numsize_literal (const void *,
							  const size_t,
							  const int);
extern cb_tree			cb_concat_literals (const cb_tree,
						    const cb_tree);

extern cb_tree			cb_build_decimal (const unsigned int);
extern cb_tree			cb_build_decimal_literal (const int);
extern int			cb_lookup_literal (cb_tree x, int make_decimal);

extern cb_tree			cb_build_comment (const char *);
extern cb_tree			cb_build_direct (const char *,
						 const unsigned int);
extern cb_tree			cb_build_debug (const cb_tree, const char *,
						const cb_tree);
extern cb_tree			cb_build_debug_call (struct cb_label *);

extern struct cb_picture	*cb_build_picture (const char *);
extern struct cb_picture	*cb_build_binary_picture (const char *,
							  const cob_u32_t,
							  const cob_u32_t);

extern cb_tree			cb_build_field (cb_tree);
extern cb_tree			cb_build_implicit_field (cb_tree, const int);
extern cb_tree			cb_build_constant (cb_tree, cb_tree);
extern int			cb_build_generic_register (const char *, const char *, struct cb_field **);

extern void			cb_build_symbolic_chars (const cb_tree,
							 const cb_tree);

extern struct cb_field		*cb_field_add (struct cb_field *,
					       struct cb_field *);
extern int				cb_field_size (const cb_tree x);
#define FIELD_SIZE_UNKNOWN -1
extern struct cb_field		*cb_field_founder (const struct cb_field * const);
extern struct cb_field		*cb_field_variable_size (const struct cb_field *);
extern unsigned int		cb_field_variable_address (const struct cb_field *);
extern int			cb_field_subordinate (const struct cb_field *,
						      const struct cb_field *);

extern cb_tree			cb_build_label (cb_tree, struct cb_label *);

extern struct cb_file		*build_file (cb_tree);
extern void			validate_file (struct cb_file *, cb_tree);
extern void			finalize_file (struct cb_file *,
					       struct cb_field *);

extern struct cb_cd *		cb_build_cd (cb_tree name);
extern void			cb_finalize_cd (struct cb_cd *,
						struct cb_field *);

extern cb_tree			cb_build_filler (void);
extern cb_tree			cb_build_reference (const char *);
extern cb_tree			cb_build_field_reference (struct cb_field *,
							  cb_tree);
extern const char		*cb_define (cb_tree, cb_tree);
extern char			*cb_to_cname (const char *);
extern void			cb_set_system_names (void);
extern cb_tree			cb_ref (cb_tree);
extern cb_tree			cb_try_ref (cb_tree);

extern cb_tree			cb_build_binary_op (cb_tree, const int,
						    cb_tree);
extern cb_tree			cb_build_binary_list (cb_tree, const int);

extern cb_tree			cb_build_funcall (const char *, const int,
						  const cb_tree, const cb_tree,
						  const cb_tree, const cb_tree,
						  const cb_tree, const cb_tree,
						  const cb_tree, const cb_tree,
						  const cb_tree, const cb_tree,
						  const cb_tree);

extern cb_tree			cb_build_cast (const enum cb_cast_type,
					       const cb_tree);
extern cb_tree			cb_build_cast_int (const cb_tree);
extern cb_tree			cb_build_cast_llint (const cb_tree);

extern cb_tree			cb_build_assign (const cb_tree, const cb_tree);

extern cb_tree			cb_build_intrinsic (cb_tree, cb_tree,
						    cb_tree, const int);
extern cb_tree			cb_build_prototype (const cb_tree,
						    const cb_tree, const int);
extern cb_tree			cb_build_any_intrinsic (cb_tree);

extern cb_tree			cb_build_search (const int,
						 const cb_tree, const cb_tree,
						 const cb_tree, const cb_tree);

extern cb_tree			cb_build_call (const cb_tree, const cb_tree,
					       const cb_tree, const cb_tree,
					       const cb_tree, const cob_u32_t,
					       const int);
extern cb_tree			cb_build_call_parameter (cb_tree, int, const int);

extern cb_tree			cb_build_alter (const cb_tree, const cb_tree);

extern cb_tree			cb_build_cancel (const cb_tree);

extern cb_tree			cb_build_goto (const cb_tree, const cb_tree);

extern cb_tree			cb_build_if (const cb_tree, const cb_tree,
					     const cb_tree, const unsigned int);

extern cb_tree			cb_build_perform (const enum cb_perform_type);
extern cb_tree			cb_build_perform_varying (cb_tree, cb_tree,
							  cb_tree, cb_tree);

extern struct cb_statement	*cb_build_statement (const char *);

extern cb_tree			cb_build_continue (void);

extern cb_tree			cb_build_list (cb_tree, cb_tree, cb_tree);
extern cb_tree			cb_list_add (cb_tree, cb_tree);
extern cb_tree			cb_pair_add (cb_tree, cb_tree, cb_tree);
extern cb_tree			cb_list_append (cb_tree, cb_tree);
extern cb_tree			cb_list_reverse (cb_tree);
extern unsigned int		cb_list_length (cb_tree);
extern unsigned int		cb_next_length (struct cb_next_elem *);

extern struct cb_report		*build_report (cb_tree);
extern void			finalize_report (struct cb_report *, struct cb_field *);
extern void 			build_sum_counter(struct cb_report *r, struct cb_field *f);
extern struct cb_field		*get_sum_data_field(struct cb_report *r, struct cb_field *f);

extern void			cb_add_common_prog (struct cb_program *);
extern void			cb_insert_common_prog (struct cb_program *,
						   struct cb_program *);


extern struct cb_intrinsic_table	*lookup_intrinsic (const char *,
							   const int);

extern cb_tree		cb_build_alphabet_name (cb_tree);
extern cb_tree		cb_build_schema_name (cb_tree);

extern cb_tree		cb_build_initialize (const cb_tree, const cb_tree,
					   const cb_tree,
					   const unsigned int,
					   const unsigned int,
					   const unsigned int);

struct cb_literal	*build_literal (enum cb_category,
					   const void *, const size_t);

extern cb_tree		cb_build_system_name (const enum cb_system_name_category,
					   const int);

extern const char	*cb_enum_explain (const enum cb_tag);

extern const char	*cb_get_usage_string (const enum cb_usage);

extern cb_tree	cb_field_dup (struct cb_field *f, struct cb_reference *ref);
extern struct cb_field *cb_field_direct (struct cb_field *f, struct cb_reference *ref,
								 char *report, int *pos, int *len);

extern cb_tree		cb_build_ml_suppress_clause (void);
extern cb_tree		cb_build_ml_tree (struct cb_field *, const int,
					   const int, cb_tree, cb_tree,
					   cb_tree);
extern cb_tree		cb_build_ml_suppress_checks (struct cb_ml_generate_tree *);


/* parser.y */
extern cb_tree		cobc_printer_node;
extern int		non_const_word;
extern int		suppress_data_exceptions;
extern unsigned int	cobc_repeat_last_token;
extern unsigned int	cobc_in_id;
extern unsigned int	cobc_in_procedure;
extern unsigned int	cobc_in_repository;
extern unsigned int	cobc_force_literal;
extern unsigned int	cobc_cs_check;
extern unsigned int	cobc_allow_program_name;
extern unsigned int	cobc_in_xml_generate_body;
extern unsigned int	cobc_in_json_generate_body;

/* reserved.c */
extern int			is_reserved_word (const char *);
extern int			is_default_reserved_word (const char *);
extern void			remove_context_sensitivity (const char *,
							    const int);
extern struct cobc_reserved	*lookup_reserved_word (const char *);
extern cb_tree			get_system_name (const char *);
extern cb_tree			get_system_name_translated (cb_tree);
extern const char		*cb_get_register_definition (const char *);
extern void			cb_list_reserved (void);
extern void			cb_list_intrinsics (void);
extern void			cb_list_system_names (void);
extern void			cb_list_exceptions (void);
extern void			cb_list_registers (void);
extern void			cb_list_system_routines (void);
extern int			cb_list_map (cb_tree (*) (cb_tree), cb_tree);
extern void			cb_memcpy_upper (char *, const char * const, size_t);

/* error.c */
extern cb_tree			get_cb_error_node (void);
extern enum cb_warn_val		cb_warning_x (const enum cb_warn_opt, cb_tree, const char *, ...) COB_A_FORMAT34;
extern enum cb_warn_val		cb_warning_dialect_x (const enum cb_support, cb_tree, const char *, ...) COB_A_FORMAT34;
extern void		cb_note_x (const enum cb_warn_opt, cb_tree, const char *, ...) COB_A_FORMAT34;
extern void		cb_note (const enum cb_warn_opt, const int, const char *, ...) COB_A_FORMAT34;
extern void		cb_inclusion_note (const char *, int);
extern enum cb_warn_val		cb_error_x (cb_tree, const char *, ...) COB_A_FORMAT23;
extern unsigned int	cb_verify (const enum cb_support, const char *);
extern unsigned int	cb_verify_x (const cb_tree, const enum cb_support,
				     const char *);
#if 0 /* CHECKME: Is there any place other than "note" where we want to do listing suppression? */
extern void		listprint_suppress (void);
extern void		listprint_restore (void);
#endif

extern enum cb_warn_val		redefinition_error (cb_tree);
extern enum cb_warn_val		redefinition_warning (cb_tree, cb_tree);
extern enum cb_warn_val		undefined_error (cb_tree);
extern enum cb_warn_val		ambiguous_error (cb_tree);
extern enum cb_warn_val		group_error (cb_tree, const char *);
extern enum cb_warn_val		level_require_error (cb_tree, const char *);
extern enum cb_warn_val		level_except_error (cb_tree, const char *);
extern int		cb_set_ignore_error (int state);

/* field.c */
extern size_t		cb_needs_01;
extern int		cb_get_level (cb_tree);
extern cb_tree		cb_build_field_tree (cb_tree, cb_tree, struct cb_field *,
					     enum cb_storage, struct cb_file *,
					     const int);
extern cb_tree		cb_build_full_field_reference (struct cb_field *);
extern struct cb_field	*cb_resolve_redefines (struct cb_field *, cb_tree);
extern void		copy_into_field (struct cb_field *, struct cb_field *);
extern void		cb_validate_field (struct cb_field *);
extern void		cb_validate_88_item (struct cb_field *);
extern struct cb_field	*cb_validate_78_item (struct cb_field *, const cob_u32_t);
extern int		cb_validate_renames_item (struct cb_field *, cb_tree, cb_tree);
extern struct cb_field	*cb_get_real_field (void);
extern void		cb_clear_real_field (void);
extern int		cb_is_figurative_constant (const cb_tree);
extern int		cb_field_is_ignored_in_ml_gen (struct cb_field * const);

/* typeck.c */
extern cb_tree		cb_debug_item;
extern cb_tree		cb_debug_line;
extern cb_tree		cb_debug_name;
extern cb_tree		cb_debug_sub_1;
extern cb_tree		cb_debug_sub_2;
extern cb_tree		cb_debug_sub_3;
extern cb_tree		cb_debug_contents;

extern struct cb_program	*cb_build_program (struct cb_program *,
						   const int);

extern cb_tree		cb_check_numeric_value (cb_tree);
extern size_t		cb_check_index_or_handle_p (cb_tree x);
extern void		cb_set_dmax (int scale);

extern void		cb_set_intr_when_compiled (void);
extern void		cb_build_registers (void);
extern void		cb_add_external_defined_registers (void);
extern const char		*cb_register_list_get_first (const char **);
extern const char		*cb_register_list_get_next (const char **);
extern void		cb_build_debug_item (void);
extern void		cb_check_field_debug (cb_tree);
extern void		cb_trim_program_id (cb_tree);
extern char		*cb_encode_program_id (const char *, const int, const int);
extern char		*cb_build_program_id (const char *, const cob_u32_t);
extern cb_tree		cb_define_switch_name (cb_tree, cb_tree, const int);

extern void		cb_check_word_length (unsigned int, const char *);
extern cb_tree		cb_build_section_name (cb_tree, const int);
extern cb_tree		cb_build_assignment_name (struct cb_file *, cb_tree);
extern cb_tree		cb_build_index (cb_tree, cb_tree,
					const unsigned int, struct cb_field *);
extern cb_tree		cb_build_identifier (cb_tree, const int);
extern cb_tree		cb_build_length (cb_tree);
extern cb_tree		cb_build_const_length (cb_tree);
extern cb_tree		cb_build_const_from (cb_tree);
extern cb_tree		cb_build_const_start (struct cb_field *, cb_tree);
extern cb_tree		cb_build_const_next (struct cb_field *);
extern cb_tree		cb_build_address (cb_tree);
extern cb_tree		cb_build_ppointer (cb_tree);

extern void		cb_validate_program_environment (struct cb_program *);
extern void		cb_validate_program_data (struct cb_program *);
extern void		cb_validate_program_body (struct cb_program *);

extern cb_tree		cb_build_expr (cb_tree);
extern cb_tree		cb_build_cond (cb_tree);

extern void		cb_end_cond (cb_tree);
extern void		cb_save_cond (void);
extern void		cb_terminate_cond (void);
extern void		cb_true_side (void);
extern void		cb_false_side (void);
extern void		cb_end_statement (void);
extern const char		*explain_operator (const int);
extern const char		*enum_explain_storage (const enum cb_storage storage);

extern void		cb_emit_arithmetic (cb_tree, const int, cb_tree);
extern cb_tree		cb_build_add (cb_tree, cb_tree, cb_tree);
extern cb_tree		cb_build_sub (cb_tree, cb_tree, cb_tree);
extern void		cb_emit_corresponding (
				cb_tree (*) (cb_tree, cb_tree, cb_tree),
				cb_tree, cb_tree, cb_tree);
extern void		cb_emit_tab_arithmetic (
	cb_tree (*) (cb_tree, cb_tree, cb_tree),
	cb_tree, cb_tree, cb_tree, cb_tree, cb_tree);
extern void		cb_emit_move_corresponding (cb_tree, cb_tree);

extern void		cb_emit_accept (cb_tree, cb_tree,
					struct cb_attr_struct *);
extern void		cb_emit_accept_line_or_col (cb_tree, const int);
extern void		cb_emit_accept_escape_key (cb_tree);
extern void		cb_emit_accept_exception_status (cb_tree);
extern void		cb_emit_accept_user_name (cb_tree);
extern void		cb_emit_accept_date (cb_tree);
extern void		cb_emit_accept_date_yyyymmdd (cb_tree);
extern void		cb_emit_accept_day (cb_tree);
extern void		cb_emit_accept_day_yyyyddd (cb_tree);
extern void		cb_emit_accept_day_of_week (cb_tree);
extern void		cb_emit_accept_time (cb_tree);
extern void		cb_emit_accept_command_line (cb_tree);
extern void		cb_emit_accept_environment (cb_tree);
extern void		cb_emit_accept_mnemonic (cb_tree, cb_tree);
extern void		cb_emit_accept_name (cb_tree, cb_tree);
extern void		cb_emit_accept_arg_number (cb_tree);
extern void		cb_emit_accept_arg_value (cb_tree);
extern void		cb_emit_get_environment (cb_tree, cb_tree);

extern void		cb_emit_allocate_identifier (cb_tree, cb_tree, const int);
extern void		cb_emit_allocate_characters (cb_tree, cb_tree, cb_tree);

extern void		cb_emit_alter (cb_tree, cb_tree);
extern void		cb_emit_free (cb_tree);

extern void		cb_emit_call (cb_tree, cb_tree, cb_tree, cb_tree,
				      cb_tree, cb_tree, cb_tree, cb_tree, int);

extern void		cb_emit_cancel (cb_tree);
extern void		cb_emit_close (cb_tree, cb_tree);
extern void		cb_emit_commit (void);
extern void		cb_emit_continue (cb_tree);
extern void		cb_emit_delete (cb_tree);
extern void		cb_emit_delete_file (cb_tree);


extern void		cb_emit_display_window (cb_tree, cb_tree, cb_tree,
					 cb_tree, struct cb_attr_struct *);
extern void		cb_emit_close_window (cb_tree, cb_tree);
extern void		cb_emit_destroy (cb_tree);

extern void		cb_emit_display (cb_tree, cb_tree,
					 cb_tree, cb_tree,
					 struct cb_attr_struct *,
					 int, enum cb_display_type);
extern cb_tree		cb_build_display_mnemonic (cb_tree);
extern cb_tree		cb_build_display_name (cb_tree);

extern void		cb_emit_env_name (cb_tree);
extern void		cb_emit_env_value (cb_tree);
extern void		cb_emit_arg_number (cb_tree);
extern void		cb_emit_command_line (cb_tree);

extern void		cb_emit_divide (cb_tree, cb_tree,
					cb_tree, cb_tree);

extern void		cb_emit_evaluate (cb_tree, cb_tree);

extern void		cb_emit_goto (cb_tree, cb_tree);
extern void		cb_emit_goto_entry (cb_tree, cb_tree);
extern void		cb_emit_exit (const unsigned int);

extern void		cb_emit_if (cb_tree, cb_tree, cb_tree);
extern cb_tree		cb_build_if_check_break (cb_tree, cb_tree);

extern void		cb_emit_initialize (cb_tree, cb_tree,
					    cb_tree, cb_tree,
					    cb_tree);

extern void		cb_emit_inspect (cb_tree, cb_tree,
					 const enum cb_inspect_clause);
extern void		cb_init_tallying (void);
extern cb_tree		cb_build_tallying_data (cb_tree);
extern cb_tree		cb_build_tallying_characters (cb_tree);
extern cb_tree		cb_build_tallying_all (void);
extern cb_tree		cb_build_tallying_leading (void);
extern cb_tree		cb_build_tallying_trailing (void);
extern cb_tree		cb_build_tallying_value (cb_tree, cb_tree);
extern cb_tree		cb_build_replacing_characters (cb_tree, cb_tree);
extern cb_tree		cb_build_replacing_all (cb_tree, cb_tree, cb_tree);
extern cb_tree		cb_build_replacing_leading (cb_tree, cb_tree, cb_tree);
extern cb_tree		cb_build_replacing_first (cb_tree, cb_tree, cb_tree);
extern cb_tree		cb_build_replacing_trailing (cb_tree, cb_tree, cb_tree);
extern cb_tree		cb_build_converting (cb_tree, cb_tree, cb_tree);
extern cb_tree		cb_build_inspect_region_start (void);

extern int		validate_move (cb_tree, cb_tree, const unsigned int, int *);
extern cb_tree		cb_build_move (cb_tree, cb_tree);
extern void		cb_emit_move (cb_tree, cb_tree);

extern void		cb_emit_open (cb_tree, cb_tree, cb_tree);

extern void		cb_emit_perform (cb_tree, cb_tree, cb_tree, cb_tree);
extern cb_tree		cb_build_perform_once (cb_tree);
extern cb_tree		cb_build_perform_times (cb_tree);
extern cb_tree		cb_build_perform_until (cb_tree, cb_tree);
extern cb_tree		cb_build_perform_forever (cb_tree);
extern cb_tree		cb_build_perform_exit (struct cb_label *);
extern void		cb_build_perform_after_until(void);

extern void		cb_emit_read (cb_tree, cb_tree, cb_tree,
				      cb_tree, cb_tree);

extern void		cb_emit_ready_trace (void);
extern void		cb_emit_rewrite (cb_tree, cb_tree, cb_tree);

extern void		cb_emit_release (cb_tree, cb_tree);
extern void		cb_emit_reset_trace (void);
extern void		cb_emit_return (cb_tree, cb_tree);

extern void		cb_emit_rollback (void);

extern void		cb_emit_search (cb_tree, cb_tree,
					cb_tree, cb_tree);
extern void		cb_emit_search_all (cb_tree, cb_tree,
					    cb_tree, cb_tree);

extern void		cb_emit_setenv (cb_tree, cb_tree);
extern void		cb_emit_set_to (cb_tree, cb_tree);
extern void		cb_emit_set_to_fcd (cb_tree, cb_tree);
extern void		cb_emit_set_to_fcdkey (cb_tree, cb_tree);
extern void		cb_emit_set_up_down (cb_tree, cb_tree, cb_tree);
extern void		cb_emit_set_on_off (cb_tree, cb_tree);
extern void		cb_emit_set_true (cb_tree);
extern void		cb_emit_set_false (cb_tree);
extern void		cb_emit_set_thread_priority (cb_tree, cb_tree);
extern void		cb_emit_set_attribute (cb_tree,
					       const cob_flags_t,
					       const cob_flags_t);
extern cb_tree		cb_build_set_attribute (const struct cb_field *,
						const cob_flags_t,
						const cob_flags_t);
extern void		cb_emit_set_last_exception_to_off (void);

extern void		cb_emit_sort_init (cb_tree, cb_tree, cb_tree, cb_tree);
extern void		cb_emit_sort_using (cb_tree, cb_tree);
extern void		cb_emit_sort_input (cb_tree);
extern void		cb_emit_sort_giving (cb_tree, cb_tree);
extern void		cb_emit_sort_output (cb_tree);
extern void		cb_emit_sort_finish (cb_tree);

extern void		cb_emit_start (cb_tree, cb_tree, cb_tree, cb_tree);

extern void		cb_emit_stop_run (cb_tree);

extern void		cb_emit_stop_error (void);

extern void		cb_emit_stop_thread (cb_tree);

extern void		cb_emit_string (cb_tree, cb_tree, cb_tree);

extern void		cb_emit_unlock (cb_tree);

extern void		cb_emit_unstring (cb_tree, cb_tree, cb_tree, cb_tree,
					  cb_tree);
extern cb_tree		cb_build_unstring_delimited (cb_tree, cb_tree);
extern cb_tree		cb_build_unstring_into (cb_tree, cb_tree, cb_tree);

extern void		cb_emit_write (cb_tree, cb_tree, cb_tree, cb_tree);
extern cb_tree		cb_build_write_advancing_lines (cb_tree, cb_tree);
extern cb_tree		cb_build_write_advancing_mnemonic (cb_tree, cb_tree);
extern cb_tree		cb_build_write_advancing_page (cb_tree);
extern cb_tree		cb_check_sum_field (cb_tree x);
extern void		cb_check_conformance (cb_tree, cb_tree, cb_tree);
extern void		cb_emit_initiate (cb_tree rep);
extern void		cb_emit_terminate (cb_tree rep);
extern void		cb_emit_generate (cb_tree rep);
extern void		cb_emit_suppress (struct cb_field *f);
extern void		cb_emit_xml_generate (cb_tree, cb_tree, cb_tree,
					      cb_tree, const int, const int,
					      cb_tree, cb_tree, cb_tree,
					      cb_tree);
extern void		cb_emit_xml_parse (cb_tree, cb_tree, const int,
					      cb_tree, cb_tree);
extern cb_tree		cb_build_xml_parse (cb_tree, cb_tree,
						const int, cb_tree, cb_tree);
extern void		cb_emit_json_generate (cb_tree, cb_tree, cb_tree,
					       cb_tree, cb_tree);

#ifdef	COB_TREE_DEBUG
extern cb_tree		cobc_tree_cast_check (const cb_tree, const char *,
					      const int, const enum cb_tag);
#endif


/* codegen.c */
extern void		codegen (struct cb_program *, const char *);
extern int		cb_wants_dump_comments;	/* likely to be removed later */

/* scanner.l */
extern void		cb_unput_dot (void);
extern void		cb_add_78 (struct cb_field *);
extern void		cb_reset_78 (void);
extern void		cb_reset_global_78 (void);
extern struct cb_field	*check_level_78 (const char *);

extern struct cb_program	*cb_find_defined_program_by_name (const char *);
extern struct cb_program	*cb_find_defined_program_by_id (const char *);

extern void		cb_validate_parameters_and_returning (struct cb_program *, cb_tree);
extern void		cb_check_definition_matches_prototype (struct cb_program *);

/* cobc.c */
#ifndef COB_EXTERNAL_XREF
#define COB_INTERNAL_XREF
#endif

#ifdef COB_INTERNAL_XREF
extern void			cobc_xref_link (struct cb_xref *, const int, const int);
extern void			cobc_xref_link_parent (const struct cb_field *);
extern void			cobc_xref_call (const char *, const int, const int, const int);
#else
#define cobc_xref_link(x,l,r)
#define cobc_xref_link_parent(f)
#define cobc_xref_call(n,l,i,s)
#endif
extern void			cobc_xref_set_receiving (const cb_tree);
extern unsigned int		cb_correct_program_order;

/* Function defines */

#define CB_BUILD_FUNCALL_0(f)					\
	cb_build_funcall (f, 0, NULL, NULL, NULL, NULL, NULL,	\
			  NULL, NULL, NULL, NULL, NULL, NULL)

#define CB_BUILD_FUNCALL_1(f,a1)				\
	cb_build_funcall (f, 1, a1, NULL, NULL, NULL, NULL,	\
			  NULL, NULL, NULL, NULL, NULL, NULL)

#define CB_BUILD_FUNCALL_2(f,a1,a2)				\
	cb_build_funcall (f, 2, a1, a2, NULL, NULL, NULL,	\
			  NULL, NULL, NULL, NULL, NULL, NULL)

#define CB_BUILD_FUNCALL_3(f,a1,a2,a3)				\
	cb_build_funcall (f, 3, a1, a2, a3, NULL, NULL, NULL,	\
			  NULL, NULL, NULL, NULL, NULL)

#define CB_BUILD_FUNCALL_4(f,a1,a2,a3,a4)			\
	cb_build_funcall (f, 4, a1, a2, a3, a4, NULL,		\
			  NULL, NULL, NULL, NULL, NULL, NULL)

#define CB_BUILD_FUNCALL_5(f,a1,a2,a3,a4,a5)			\
	cb_build_funcall (f, 5, a1, a2, a3, a4, a5,		\
			  NULL, NULL, NULL, NULL, NULL, NULL)

#define CB_BUILD_FUNCALL_6(f,a1,a2,a3,a4,a5,a6)			\
	cb_build_funcall (f, 6, a1, a2, a3, a4, a5, a6,		\
			  NULL, NULL, NULL, NULL, NULL)

#define CB_BUILD_FUNCALL_7(f,a1,a2,a3,a4,a5,a6,a7)		\
	cb_build_funcall (f, 7, a1, a2, a3, a4, a5, a6, a7,	\
			  NULL, NULL, NULL, NULL)

#define CB_BUILD_FUNCALL_8(f,a1,a2,a3,a4,a5,a6,a7,a8)		\
	cb_build_funcall (f, 8, a1, a2, a3, a4, a5, a6, a7, a8,	\
			  NULL, NULL, NULL)

#define CB_BUILD_FUNCALL_9(f,a1,a2,a3,a4,a5,a6,a7,a8,a9)	\
	cb_build_funcall (f, 9, a1, a2, a3, a4, a5, a6, a7, a8,	\
			  a9, NULL, NULL)

#define CB_BUILD_FUNCALL_10(f,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)		\
	cb_build_funcall (f, 10, a1, a2, a3, a4, a5, a6, a7, a8,	\
			  a9, a10, NULL)

#define CB_BUILD_FUNCALL_11(f,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11)	\
	cb_build_funcall (f, 11, a1, a2, a3, a4, a5, a6, a7, a8,	\
			  a9, a10, a11)

/* Miscellaneous defines */

#define CB_BUILD_CAST_ADDRESS(x)	cb_build_cast (CB_CAST_ADDRESS, x)
#define CB_BUILD_CAST_ADDR_OF_ADDR(x)	cb_build_cast (CB_CAST_ADDR_OF_ADDR, x)
#define CB_BUILD_CAST_LENGTH(x)		cb_build_cast (CB_CAST_LENGTH, x)
#define CB_BUILD_CAST_PPOINTER(x)	cb_build_cast (CB_CAST_PROGRAM_POINTER, x)

#define CB_BUILD_PARENTHESES(x)		cb_build_binary_op (x, '@', NULL)
#define CB_BUILD_NEGATION(x)		cb_build_binary_op (x, '!', NULL)

#define CB_BUILD_STRING0(str)		cb_build_string (str, strlen ((char *)(str)))

#define CB_LIST_INIT(x)			cb_build_list (NULL, x, NULL)
#define CB_BUILD_CHAIN(x,y)		cb_build_list (NULL, x, y)
#define CB_BUILD_PAIR(x,y)		cb_build_list (x, y, NULL)
#define CB_ADD_TO_CHAIN(x,y)		y = CB_BUILD_CHAIN (x, y)
#define CB_CHAIN_PAIR(x,y,z)		x = cb_pair_add (x, y, z)
#define CB_FIELD_ADD(x,y)		x = cb_field_add (x, y)


#endif /* CB_TREE_H */

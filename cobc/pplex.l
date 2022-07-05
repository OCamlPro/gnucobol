/*
   Copyright (C) 2001-2012, 2014-2022 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Simon Sobisch, Dave Pitts

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


%option 8bit
%option case-insensitive
%option never-interactive
%option prefix="pp"

%option stack

%option noyy_top_state
%option noyy_scan_buffer
%option noyy_scan_bytes
%option noyy_scan_string

%option noyyget_extra
%option noyyset_extra
%option noyyget_leng
%option noyyget_text
%option noyyget_lineno
%option noyyset_lineno
%option noyyget_in
%option noyyset_in
%option noyyget_out
%option noyyset_out
%option noyyget_lval
%option noyyset_lval
%option noyyget_lloc
%option noyyset_lloc
%option noyyget_debug
%option noyyset_debug

%{
#undef	YY_READ_BUF_SIZE
#define	YY_READ_BUF_SIZE	32768
#undef	YY_BUF_SIZE
#define	YY_BUF_SIZE		32768

#define	YY_SKIP_YYWRAP
static int ppwrap (void) {
	return 1;
}

#define	PPLEX_BUFF_LEN		512
#define YY_INPUT(buf,result,max_size)	result = ppinput (buf, max_size);
#define	ECHO				fputs (yytext, yyout)

#define	YY_USER_INIT							\
	if (!plexbuff1) {						\
		plexbuff1 = cobc_malloc ((size_t)COB_SMALL_BUFF);	\
	}								\
	if (!plexbuff2) {						\
		plexbuff2 = cobc_malloc ((size_t)COB_SMALL_BUFF);	\
	}								\
	requires_listing_line = 1;					\
	comment_allowed = 1;

#include "config.h"

#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#else
#define	YY_NO_UNISTD_H	1
#endif
#include <ctype.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <string.h>
#ifdef	HAVE_STRINGS_H
#include <strings.h>
#endif

#define	COB_IN_PPLEX	1
#include "cobc.h"
#include "tree.h"
#include "ppparse.h"

#ifdef	_WIN32
#include <io.h>	/* for access */
#endif

/* ignore unused functions here as flex generates unused ones */
#ifdef	__GNUC__
#if	defined (__clang__) || __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 4)
#pragma GCC diagnostic ignored "-Wunused-function"
#endif
#endif

#define PLEX_COND_DEPTH		16

struct copy_info {
	struct copy_info	*next;
	struct copy_info	*prev;
	struct list_files	*containing_files;
	const char		*file;
	char			*dname;
	struct cb_replace_list	*replacing;
	YY_BUFFER_STATE		buffer;
	int			line;
	int			quotation_mark;
	int			source_format;
};

struct plex_stack {
	unsigned int		cmd;
	unsigned int		skip;
	unsigned int		cond;
	int			line;
};

/* Global variables */

/* Format-specific columns; should only be assigned in cobc_set_source_format
   below (exported for listing code in `cobc.c`) */
int			cb_indicator_column = 7;
int			cb_text_column = 72;

/* Local variables */
static char			*plexbuff1 = NULL;
static char			*plexbuff2 = NULL;
static struct list_files	*old_list_file = NULL;
static size_t			newline_count = 0;
static size_t			within_comment = 0;
static size_t			inside_bracket = 0;
static size_t			consecutive_quotation = 0;
static size_t			need_continuation = 0;
static size_t			buffer_overflow = 0;
static size_t			comment_allowed;
static unsigned int		plex_skip_input = 0;
static unsigned int		plex_nest_depth = 0;
static int			quotation_mark = 0;
static int			listing_line = 0;
static int			requires_listing_line;
static int			requires_new_line = 0;
static enum cb_format		source_format;
static int	floating_area_b = 0;	   /* whether indicator is optional */
static int	fill_continued_alnums = 1; /* whether continued alphanumeric
					      literals should be filled with
					      spaces up to text column */

static char	display_msg[PPLEX_BUFF_LEN];

static struct cb_replace_list	*current_replace_list = NULL;
static struct cb_replace_list	*save_current_replace = NULL;
static struct cb_replace_list	*base_replace_list = NULL;

static struct cb_text_list	*text_queue = NULL;

static struct copy_info		*copy_stack = NULL;

static struct plex_stack	plex_cond_stack[PLEX_COND_DEPTH];

/* Function declarations */
static int	ppinput			(char *, const size_t);
static void	ppecho			(const char *, const cob_u32_t,
					 const int);
static void	ppecho_direct			(const char *);
static void	switch_to_buffer	(const int, const char *,
					 const YY_BUFFER_STATE);
static void	check_listing		(const char *, const unsigned int);
static void	skip_to_eol		(void);
static void	display_finish		(void);
static void	set_print_replace_list	(struct cb_replace_list *list);
static void	get_new_listing_file	(void);

%}

WORD		[_0-9A-Z\x80-\xFF-]+
NUMRIC_LITERAL	[+-]?[0-9,.]*[0-9]
ALNUM_LITERAL	"\""[^""\n]*"\""|"\'"[^''\n]*"\'"
SET_PAREN_LIT	\([^()\n]*\)
DEFNUM_LITERAL	[+-]?[0-9]*[\.]*[0-9]+

%x CALL_DIRECTIVE_STATE
%x COBOL_WORDS_DIRECTIVE_STATE
%x COPY_STATE
%x PSEUDO_STATE
%x SOURCE_DIRECTIVE_STATE
%x DEFINE_DIRECTIVE_STATE
%x ON_OFF_DIRECTIVE_STATE
%x SET_DIRECTIVE_STATE
%x TURN_DIRECTIVE_STATE
%x IF_DIRECTIVE_STATE
%x ELSE_DIRECTIVE_STATE
%x ENDIF_DIRECTIVE_STATE
%x ALNUM_LITERAL_STATE
%x CONTROL_STATEMENT_STATE
%x DISPLAY_DIRECTIVE_STATE

%%

%{
%}

<*>"*>".*		{
	/* 2002+: inline comment */
	#if	0	/* RXWRXW - Directive state */
	if (YY_START != DIRECTIVE_STATE && YY_START != SET_DIRECTIVE_STATE) {
		ppecho (" ", 0, 1);
	}
	#endif
}

^[ ]*">>"[ ]?"COBOL-WORDS"	{
	/* 202x+: directive for setting source format */
	BEGIN COBOL_WORDS_DIRECTIVE_STATE;
	return COBOL_WORDS_DIRECTIVE;
}

^[ ]*">>"[ ]?"DEFINE"	{
	/* 2002+: definition of compiler constants display message during compilation */
	/* Define here to preempt next debug rule below */
	BEGIN DEFINE_DIRECTIVE_STATE;
	return DEFINE_DIRECTIVE;
}

^[ ]*">>"[ ]?"DISPLAY"[ ]+ {
	/* previous OpenCOBOL/GnuCOBOL 2.x extension, added in COBOL 202x with slightly different syntax:
	   display message during compilation --> needs a dialect option to switch to the appropriate state */
	display_msg[0] = 0;
	BEGIN DISPLAY_DIRECTIVE_STATE;
}

^[ ]*">>"[ ]?"REF-MOD-ZERO-LENGTH"	{
	/* 202x: directive to allow zero ref-mod */
	BEGIN ON_OFF_DIRECTIVE_STATE;
	return REFMOD_DIRECTIVE;
}

^[ ]*">>D"		{
	/* 2002 (only) floating debug line */
	/* Remove line if debugging lines not activated */
	/* Otherwise ignore the directive part of the line */
	(void) cb_verify (cb_debugging_mode, _("debugging indicator"));
	if (!cb_flag_debugging_line) {
		skip_to_eol ();
	}
}

^[ ]*">>"[ ]?"PAGE"	{
	/* 2002+: listing directive for page eject with optional comment
	   Note: processed in cobc.c */
	skip_to_eol ();
}

^[ ]*">>"[ ]?"LISTING"	{
	/* 2002+: listing directive for (de-)activating the listing,
	   ON implied for empty value
	   Note: further checks in ppparse.y, processed in cobc.c */
	BEGIN ON_OFF_DIRECTIVE_STATE;
	return LISTING_DIRECTIVE;
}

^[ ]*">>"[ ]?"SOURCE"	{
	/* 2002+: directive for setting source format */
	BEGIN SOURCE_DIRECTIVE_STATE;
	return SOURCE_DIRECTIVE;
}

^[ ]*">>"[ ]?"SET"	{
	/* OpenCOBOL/GnuCOBOL 2.0 extension: MF SET directive in 2002+ style format */
	BEGIN SET_DIRECTIVE_STATE;
	return SET_DIRECTIVE;
}

^[ ]*">>"[ ]?"TURN"	{
	/* 2002+: directive for (de-)activating exception checks */
	BEGIN TURN_DIRECTIVE_STATE;
	return TURN_DIRECTIVE;
}

^[ ]*">>"[ ]?"IF"	{
	/* 2002+: conditional compilation */
	BEGIN IF_DIRECTIVE_STATE;
	return IF_DIRECTIVE;
}
^[ ]*">>"[ ]?"ELIF" |
^[ ]*">>"[ ]?"ELSE-IF"	{
	/* OpenCOBOL extension: conditional compilation combined ELSE IF,
	   2002+ style format */
	BEGIN IF_DIRECTIVE_STATE;
	return ELIF_DIRECTIVE;
}
^[ ]*">>"[ ]?"ELSE"	{
	/* 2002+: conditional compilation */
	BEGIN ELSE_DIRECTIVE_STATE;
	return ELSE_DIRECTIVE;
}
^[ ]*">>"[ ]?"END-IF"	{
	/* 2002+: conditional compilation */
	BEGIN ENDIF_DIRECTIVE_STATE;
	return ENDIF_DIRECTIVE;
}

^[ ]*">>"[ ]?"LEAP-SECOND"	{
	/* 2002+: more then 60 seconds per minute (currently always set to off),
	          OFF implied for empty value */
	BEGIN ON_OFF_DIRECTIVE_STATE;
	return LEAP_SECOND_DIRECTIVE;
}

^[ ]*">>"[ ]?"CALL-CONVENTION"	{
	/* 2002+: convention for CALL/CANCEL */
	BEGIN CALL_DIRECTIVE_STATE;
	return CALL_DIRECTIVE;
}

^[ ]*">>"[ ]*\n		{
	/* empty 2002+ style directive */
	cb_plex_warning (COBC_WARN_FILLER, newline_count,
			_("ignoring empty directive"));
	unput ('\n');
}

^[ ]*">>"[ ]*[_0-9A-Z-]+	{
	/* unknown 2002+ style directive */
	char	*s;

	s = strchr (yytext, '>');
	cb_plex_warning (COBC_WARN_FILLER, newline_count,
			_("ignoring invalid directive: '%s'"), s);
	skip_to_eol ();
}

^[ ]*">>"		{
	/* unknown 2002+ style directive */
	cb_plex_warning (COBC_WARN_FILLER, newline_count,
			_("ignoring invalid directive"));
	skip_to_eol ();
}

^[ ]*"$DISPLAY"[ ]+"VCS"[ ]+"="[ ]+	{
	/* MF extension: include @(#)text\0 in the object file */
	/* we just add a warning for now, maybe implement it later */
	CB_PENDING (_("VCS directive"));
	skip_to_eol ();
}

^[ ]*"$DISPLAY"[ ]+		{
	/* MF extension: display message during compilation */
	display_msg[0] = 0;
	BEGIN DISPLAY_DIRECTIVE_STATE;
}

^[ ]*"$SET"		{
	/* MF extension: SET directive */
	/* TODO: check position of the $SET directive */
	BEGIN SET_DIRECTIVE_STATE;
	return SET_DIRECTIVE;
}

^[ ]*"$IF"		{
	/* MF extension: conditional compilation */
	BEGIN IF_DIRECTIVE_STATE;
	return IF_DIRECTIVE;
}
^[ ]*"$ELIF" |
^[ ]*"$ELSE-IF"		{
	/* OpenCOBOL/GnuCOBOL 2.0 extension: conditional compilation combined ELSE IF,
	   MF style format */
	BEGIN IF_DIRECTIVE_STATE;
	return ELIF_DIRECTIVE;
}
^[ ]*"$ELSE"		{
	/* MF extension: conditional compilation */
	BEGIN ELSE_DIRECTIVE_STATE;
	return ELSE_DIRECTIVE;
}
^[ ]*"$END"		|
^[ ]*"$END-IF"	{
	/* MF extension: conditional compilation, second undocumented */
	BEGIN ENDIF_DIRECTIVE_STATE;
	return ENDIF_DIRECTIVE;
}

^[ ]*"$"[_0-9A-Z-]+	{
	/* unknown MF style directive */
	char	*s;

	s = strchr (yytext, '$');
	cb_plex_warning (COBC_WARN_FILLER, newline_count,
			_("ignoring invalid directive: '%s'"), s);
	skip_to_eol ();
}

^......."@OPTIONS"	{
	/* Fujitsu COBOL extension for specifying command line options */
	/* TODO: check position of the @OPTIONS directive */
	char *s = strchr (yytext, '@');
	cb_plex_warning (COBC_WARN_FILLER, newline_count - 1,
		_("ignoring unknown directive: '%s'"), s);
	skip_to_eol ();
}

^[ ]*("PROCESS"|"CBL")/[ ,;\n] {
	/* IBM COBOL extension for specifying compiler options */
	/* TODO: The CBL (PROCESS) statement must be placed before any comment lines or other compiler-directing statements. */
	cb_plex_warning (COBC_WARN_FILLER, newline_count - 1,
		_("PROCESS statement ignored"));
	skip_to_eol ();
}

^[ ]*"$"		{
	cb_plex_warning (COBC_WARN_FILLER, newline_count,
		_("spurious '$' detected - ignored"));
	skip_to_eol ();
}

"COPY"/[ ,;\n]			{
	yy_push_state (COPY_STATE);
	if (cb_src_list_file) {
		get_new_listing_file ();
	}
	return COPY;
}

"INCLUDE"/[ ,;\n]		{
	/* Note: ++INCLUDE/-INC (include only the data records,
	   must be specified in column 8/1) and are not implemented yet */
	yy_push_state (COPY_STATE);
	if (cb_src_list_file) {
		get_new_listing_file ();
	}
	return COPY;
}

"REPLACE"/[ ,;\n]		{
	yy_push_state (COPY_STATE);
	return REPLACE;
}

^[ ]*"*CONTROL"		|
^[ ]*"*CBL"		{
	BEGIN CONTROL_STATEMENT_STATE;
	return CONTROL_STATEMENT;
}

("ID"|"IDENTIFICATION")[ ,;\n]+"DIVISION" {
	/* Allow comment sentences/paragraphs */
	comment_allowed = 1;
	ppecho (yytext, 0, (int)yyleng);
}

"PROGRAM-ID"/[ .,;\n]	{
	/* Allow comment sentences/paragraphs */
	comment_allowed = 1;
	ppecho (yytext, 0, (int)yyleng);
}

"DIVISION"/[ .,;\n]	{
	/* Disallow comment sentences/paragraphs */
	comment_allowed = 0;
	ppecho (yytext, 0, (int)yyleng);
}

"SECTION"/[ .,;\n]	{
	/* Disallow comment sentences/paragraphs */
	comment_allowed = 0;
	ppecho (yytext, 0, (int)yyleng);
}

^[ ]*"EJECT"([ ]*\.)? |
^[ ]*"SKIP1"([ ]*\.)? |
^[ ]*"SKIP2"([ ]*\.)? |
^[ ]*"SKIP3"([ ]*\.)?	{
	/* These words can either be a listing-directive statement,
	   a reserved word, or a user-defined word...
	   some implementations (dis-)allow the (optional) "."
	   some start column 8+ some column 12+
	   We ignore the detailed rules and just do the parsing. */
	if (cb_verify (cb_listing_statements, yytext)) {
		/* handle as listing-directive statement */
		skip_to_eol();
		return LISTING_STATEMENT;
	} else if (cb_listing_statements == CB_SKIP) {
		/* handle later (normal reserved / user defined word) */
		ECHO;
		check_listing (yytext, 0);
	} else {
		/* Ignore */
	}
}

^[ ]*"TITLE"[ ,;\n]	{
	/* This word can either be a listing-directive statement,
	   a reserved word, or a user-defined word...
	   some implementations (dis-)allow the (optional) "."
	   some start column 8+ some column 12+,
	   most limit the literal length (we cut in cobc.c)
	   We ignore the detailed rules and just do the parsing. */
	if (cb_verify (cb_title_statement, yytext)) {
		/* handle as listing-directive statement */
		BEGIN ALNUM_LITERAL_STATE;
		return TITLE_STATEMENT;
	} else if (cb_title_statement == CB_SKIP) {
		/* handle later (normal reserved / user defined word) */
		ECHO;
		check_listing (yytext, 0);
	} else {
		/* Ignore */
	}
}

("WITH"[ ,;\n]+)?"DEBUGGING"[ ,;\n]+"MODE"	{
	/* Pick up early - Also activates debugging lines */
	cb_verify (cb_debugging_mode, "DEBUGGING MODE");
	cb_flag_debugging_line = 1;
	ppecho (yytext, 0, (int)yyleng);
}

[,;]?\n		{
	ppecho ("\n", 0, 1);
	cb_source_line++;
}

[;]?[ ]+	{
	ppecho (" ", 1U, 1);
}

[,]?[ ]+	{
	if (inside_bracket) {
		ppecho (", ", 0, 2);
	} else {
		ppecho (" ", 1U, 1);
	}
}

"("		{
	inside_bracket++;
	ppecho ("(", 0, 1);
}

")"		{
	if (inside_bracket) {
		inside_bracket--;
	}
	ppecho (")", 0, 1);
}

{WORD} |
{NUMRIC_LITERAL} |
{ALNUM_LITERAL} |
.		{
	ppecho (yytext, 0, (int)yyleng);
}

<CALL_DIRECTIVE_STATE,
SOURCE_DIRECTIVE_STATE,
DEFINE_DIRECTIVE_STATE,
ON_OFF_DIRECTIVE_STATE,
SET_DIRECTIVE_STATE,
TURN_DIRECTIVE_STATE,
IF_DIRECTIVE_STATE,
ELSE_DIRECTIVE_STATE,
ENDIF_DIRECTIVE_STATE,
ALNUM_LITERAL_STATE,
CONTROL_STATEMENT_STATE,
COBOL_WORDS_DIRECTIVE_STATE>{
  \n			{
	BEGIN INITIAL;
	unput ('\n');
	return TERMINATOR;
  }
  [ ,;]+		{ /* ignore */ }
  "." {
	return DOT;
  }
}

<DISPLAY_DIRECTIVE_STATE>{
  \n			{
	BEGIN INITIAL;
	display_finish();
  }

  {ALNUM_LITERAL} {
	yytext[yyleng - 1] = 0;
	strncat (display_msg, yytext + 1, (size_t)(PPLEX_BUFF_LEN - 1));
  }

  [ A-Z0-9\x80-\xFF.,;#/\\_+-<>]+ {
	strncat (display_msg, yytext, (size_t)(PPLEX_BUFF_LEN - 1));
  }
}

<ON_OFF_DIRECTIVE_STATE>{
  "ON"			{ return ON; }
  "OFF"			{ return OFF; }
}

<COBOL_WORDS_DIRECTIVE_STATE>{
  "EQUATE"		{ return EQUATE; }
  "WITH"		{ return WITH; }
  "UNDEFINE"	{ return UNDEFINE; }
  "SUBSTITUTE"	{ return SUBSTITUTE; }
  "BY"			{ return BY; }
  "RESERVE"		{ return RESERVE; }
  {NUMRIC_LITERAL} |
  {ALNUM_LITERAL}	{
	pplval.s = cobc_plex_strdup (yytext);
	return LITERAL;
  }
}

<SOURCE_DIRECTIVE_STATE>{
  "FORMAT"		{ return FORMAT; }
  "IS"			{ return IS; }
  {WORD}		{
	pplval.s = cobc_plex_strdup (yytext);
	return VARIABLE_NAME;
  }
}

<CALL_DIRECTIVE_STATE>{
  "COBOL"		{ return COBOL; }
  "EXTERN"		{ return TOK_EXTERN; }
  "STDCALL"		{ return STDCALL; }
  "STATIC"		{ return STATIC; }
}

<CONTROL_STATEMENT_STATE>{
  "SOURCE"		{ return SOURCE; }
  "NOSOURCE"		{ return NOSOURCE; }
  "LIST"		{ return LIST; }
  "NOLIST"		{ return NOLIST; }
  "MAP"			{ return MAP; }
  "NOMAP"		{ return NOMAP; }
}

<DEFINE_DIRECTIVE_STATE>{
  /* OpenCOBOL/GnuCOBOL 2.0 extension: MF $SET CONSTANT in 2002+ style as
     >> DEFINE CONSTANT var [AS] literal  archaic extension:
     use plain  >> DEFINE var [AS] literal  for conditional compilation and
     use        01 CONSTANT with/without FROM clause  for constant definitions */
  "CONSTANT"		{
	return CONSTANT;
  }
  "AS"			{
	return AS;
  }
  "OFF"			{
	return OFF;
  }
  "OVERRIDE"		{
	return OVERRIDE;
  }
  "PARAMETER"		{
	return PARAMETER;
  }
  {NUMRIC_LITERAL} |
  {ALNUM_LITERAL}	{
	pplval.s = cobc_plex_strdup (yytext);
	return LITERAL;
  }
  {WORD}		{
	pplval.s = cobc_plex_strdup (yytext);
	return VARIABLE_NAME;
  }
}

<SET_DIRECTIVE_STATE>{
  "ADDRSV"	|
  "ADD-RSV"	{
	return ADDRSV;
  }
  "ADDSYN"	|
  "ADD-SYN"	{
	return ADDSYN;
  }
  "ASSIGN"	{
	return ASSIGN;
  }
  "BOUND"	{
	 return BOUND;
  }
  "CALLFH"	{
	return CALLFH;
  }
  "CHECKNUM"	|
  "CHECK-NUM"	{
	return CHECKNUM;
  }
  "COMP1"	|
  "COMP-1"	{
	return COMP1;
  }
  "CONSTANT"	{
	return CONSTANT;
  }
  "DPCINDATA"	|
  "DPC-IN-DATA"	{
	return DPC_IN_DATA;
  }
  "FOLDCOPYNAME"	|
  "FOLD-COPY-NAME"	{
	return FOLDCOPYNAME;
  }
  "MAKESYN" 	|
  "MAKE-SYN"	{
	return MAKESYN;
  }
  "NOBOUND" |
  "NO-BOUND"		{
	return NOBOUND;
  }
  "NOCHECKNUM"  	|
  "NO-CHECKNUM" 	|
  "NO-CHECK-NUM"	{
	return NOCHECKNUM;
  }
  "NODPCINDATA" 	|
  "NO-DPCINDATA"	|
  "NODPC-IN-DATA"	|
  "NO-DPC-IN-DATA"	{
	return NODPC_IN_DATA;
  }
  "NOFOLDCOPYNAME"  	|
  "NOFOLD-COPY-NAME"	|
  "NO-FOLD-COPY-NAME"	{
	return NOFOLDCOPYNAME;
  }
  "NOODOSLIDE"	|
  "NO-ODOSLIDE"	{
	return NOODOSLIDE;
  }
  "NOSSRANGE"	|
  "NO-SSRANGE"	{
	return NOSSRANGE;
  }
  "NOSPZERO"	|
  "NO-SPZERO"	{
	return NOSPZERO;
  }
  "ODOSLIDE"	{
	return ODOSLIDE;
  }
  "OVERRIDE"	{
	return OVERRIDE;
  }
  "REMOVE"		{
	return REMOVE;
  }
  "SSRANGE"	{
	return SSRANGE;
  }
  "SPZERO"	{
	return SPZERO;
  }
  "SOURCEFORMAT" |
  "SOURCE-FORMAT"	{
	return SOURCEFORMAT;
  }
  /*"AS"			{ - not available with MF compilers -
	return AS;
  }*/
  {DEFNUM_LITERAL}	|
  {ALNUM_LITERAL}	|
  {SET_PAREN_LIT}	{
	pplval.s = cobc_plex_strdup (yytext);
	return LITERAL;
  }
  {WORD}		{
	pplval.s = cobc_plex_strdup (yytext);
	return VARIABLE_NAME;
  }
  " = "			{
	/* MF rule: "The equals signs must be surrounded by spaces." */
	return EQ;
  }
}

<TURN_DIRECTIVE_STATE>{
  "ON"			{
	return ON;
  }
  "OFF"			{
	return OFF;
  }
  "WITH"		{
	return WITH;
  }
  "LOCATION"		{
	return LOCATION;
  }
  "CHECKING"		{
	return CHECKING;
  }
  {DEFNUM_LITERAL} |
  {ALNUM_LITERAL}	{
	pplval.s = cobc_plex_strdup (yytext);
	return LITERAL;
  }
  {SET_PAREN_LIT}	{
	yytext[yyleng - 1] = 0;
	pplval.s = cobc_plex_strdup (yytext + 1);
	return LITERAL;
  }
  {WORD}		{
	pplval.s = cobc_plex_strdup (yytext);
	return VARIABLE_NAME;
  }
}

<CALL_DIRECTIVE_STATE,
SOURCE_DIRECTIVE_STATE,
ON_OFF_DIRECTIVE_STATE,
ELSE_DIRECTIVE_STATE,
ENDIF_DIRECTIVE_STATE>{
  {NUMRIC_LITERAL} |
  {ALNUM_LITERAL}	{
	return LITERAL;
  }
  {WORD}		{
	return GARBAGE;
  }
}

<IF_DIRECTIVE_STATE>{
  "IS"			{ return IS; }
  "NOT"			{ return NOT; }
  "EQUAL"		{ return EQUAL; }
  "TO"			{ return TO; }
  "OR"			{ return OR; }
  "GREATER"		{ return GREATER; }
  "LESS"		{ return LESS; }
  "THAN"		{ return THAN; }
  "DEFINED"		{ return DEFINED; }
  "SET"			{ return SET; }
  ">="			{ return GE; }
  ">"			{ return GT; }
  "<="			{ return LE; }
  "<>"			{ return NE; }
  "<"			{ return LT; }
  "="			{ return EQ; }
  {NUMRIC_LITERAL} |
  {ALNUM_LITERAL}	{
	pplval.s = cobc_plex_strdup (yytext);
	return LITERAL;
  }
  {WORD}		{
	pplval.s = cobc_plex_strdup (yytext);
	return VARIABLE_NAME;
  }
}

<ALNUM_LITERAL_STATE>{
  {ALNUM_LITERAL}	{
	return LITERAL;
  }
}

<COPY_STATE>{
  [,;]?\n		{
	ECHO;
	check_listing (yytext, 0);
	cb_source_line++;
  }
  [,;]?[ ]+		{ /* ignore */ }
  [_0-9A-Z\x80-\xFF-]+(\.[_0-9A-Z\x80-\xFF-]+)+		{
	/* special case to allow copybook names with periods
	   without a literal
	   given the rule: not starting, not ending, not doubled
	   *unlikely* to need a configuration option */
	pplval.s = cobc_plex_strdup (yytext);
	return TEXT_NAME;
  }
  \. 			{
	yy_pop_state ();
	return DOT;
  }
  "=="			{ yy_push_state (PSEUDO_STATE); return EQEQ; }
  "("			{ return '('; }
  ")"			{ return ')'; }
  "BY"			{ return BY; }
  "IN"			{ return IN; }
  "OF"			{ return OF; }
  "OFF"			{ return OFF; }
  "SUPPRESS"		{ return SUPPRESS; }
  "PRINTING"		{ return PRINTING; }
  "REPLACING"		{ return REPLACING; }
  "LEADING"		{ return LEADING; }
  "TRAILING"		{ return TRAILING; }
  "ALSO"		{ return ALSO; }
  "LAST"		{ return LAST; }
  {WORD} |
  {NUMRIC_LITERAL} |
  {ALNUM_LITERAL} |
  .				{
	pplval.s = cobc_plex_strdup (yytext);
	return TOKEN;
  }
}

<PSEUDO_STATE>{
  [,;]?\n		{
	ECHO;
	check_listing (yytext, 0);
	cb_source_line++;
  }

  [,;]?[ ]+		{
	pplval.s = cobc_plex_strdup (" ");
	return TOKEN;
  }

  "=="			{
	yy_pop_state ();
	return EQEQ;
  }

  {WORD} |
  {NUMRIC_LITERAL} |
  {ALNUM_LITERAL} |
  .			{
	pplval.s = cobc_plex_strdup (yytext);
	return TOKEN;
  }
}

<<EOF>> {
	struct copy_info *current_copy_info = copy_stack;

	yy_delete_buffer (YY_CURRENT_BUFFER);

	/* Terminate at the end of all input */
	if (current_copy_info->next == NULL) {
		/* Check dangling IF/ELSE */
		for (; plex_nest_depth > 0; --plex_nest_depth) {
			cb_source_line = plex_cond_stack[plex_nest_depth].line;
			cb_error (_("IF/ELIF/ELSE directive without matching END-IF"));
		}
		plex_nest_depth = 0;
		cobc_free (current_copy_info->dname);
		cobc_free (current_copy_info);
		listing_line = 0;
		requires_listing_line = 1;
		requires_new_line = 0;
		need_continuation = 0;
		buffer_overflow = 0;
		within_comment = 0;
		newline_count = 0;
		inside_bracket = 0;
		comment_allowed = 1;
		current_replace_list = NULL;
		base_replace_list = NULL;
		save_current_replace = NULL;
		text_queue = NULL;
		copy_stack = NULL;
		quotation_mark = 0;
		consecutive_quotation = 0;
		yyterminate ();
	}

	/* Close the current file (can be NULL if open failed) */
	if (ppin) {
		fclose (ppin);
		ppin = NULL;
	}

	if (current_copy_info->containing_files) {
		cb_current_file = current_copy_info->containing_files;
	}

	/* Switch to previous buffer */
	switch_to_buffer (current_copy_info->line, current_copy_info->file,
			  current_copy_info->buffer);

	/* Restore variables */
	current_replace_list = current_copy_info->replacing;
	quotation_mark = current_copy_info->quotation_mark;
	cobc_set_source_format (current_copy_info->source_format);

	copy_stack = current_copy_info->next;
	cobc_free (current_copy_info->dname);
	cobc_free (current_copy_info);
}

%%

/* Global functions */

void
pp_set_replace_list (struct cb_replace_list *list, const cob_u32_t is_pushpop)
{
	/* Handle REPLACE verb */
	if (!list) {
		/* REPLACE [LAST] OFF */
		if (!is_pushpop) {
			base_replace_list = NULL;
			return;
		}
		if (!base_replace_list) {
			return;
		}
		base_replace_list = base_replace_list->prev;
		return;
	}
	/* REPLACE [ALSO] ... */
	if (base_replace_list && is_pushpop) {
		list->last->next = base_replace_list;
		list->prev = base_replace_list;
	} else {
		list->prev = NULL;
	}
	base_replace_list = list;
	if (cb_src_list_file) {
		set_print_replace_list (list);
	}
}

/* open file (source or coypbook) for further processing */
int
ppopen (const char *name, struct cb_replace_list *replacing_list)
{
	struct copy_info	*current_copy_info;
#if 0
	char			*s;
#endif
	char			*dname;
	cb_tree			x = NULL;

	if (ppin) {
		for (; newline_count > 0; newline_count--) {
			ungetc ('\n', ppin);
		}
	}

	/* Open copy/source file, or use stdin */
	if (strcmp (name, COB_DASH) == 0) {
		ppin = stdin;
	} else {
		for (current_copy_info = copy_stack; current_copy_info; current_copy_info = current_copy_info->next) {
			/* FIXME: for WIN32 compare with cleaning / and \ (COPY "lib/file" vs COPY "lib\file"),
			*         ideally open first, then check if we have thy physical same file
			*         (could also fix symlinked files) */
			if (!strcmp (name, current_copy_info->dname)) {
				for (current_copy_info = current_copy_info->next; current_copy_info; current_copy_info = current_copy_info->prev) {
					int		line;
					if (current_copy_info->prev) {
						line = current_copy_info->prev->line;
					} else {
						line = cb_source_line;
					}
					cb_inclusion_note (current_copy_info->dname, line);
				}
				x = cobc_malloc (sizeof (struct cb_tree_common));
				x->source_file = name;
				x->source_line = -1;
				cb_error_x (x, _("recursive inclusion"));
				cobc_free (x);
				break;
			}

		}
		if (!x) {
#ifdef	__OS400__
			ppin = fopen (name, "r");
#else
			ppin = fopen (name, "rb");
#endif
		} else {
			ppin = 0;
		}
	}

	if (!ppin && !x) {
		cb_error ("%s: %s", name, cb_get_strerror ());
		/* Note: postpone error exit as we need the saved buffers later on */
	}

	/* Check for BOM - *not* for input from stdin as rewind() clears the input
	  buffer if used on stdin and output in console has normally no BOM at all */
	if (ppin && strcmp (name, COB_DASH) != 0) {
		unsigned char		bom[4];
		if (fread (bom, 3, 1, ppin) == 1) {
			if (bom[0] != 0xEF || bom[1] != 0xBB || bom[2] != 0xBF) {
				rewind (ppin);
			}
		} else {
			rewind (ppin);
		}
	}

	/* Save name for listing */
	if (cb_current_file && !cb_current_file->name) {
		cb_current_file->name = cobc_strdup (name);
	}

	/* Preserve the current buffer */
	current_copy_info = cobc_malloc (sizeof (struct copy_info));
	current_copy_info->file = cb_source_file;
	current_copy_info->buffer = YY_CURRENT_BUFFER;

	/* Save variables */
	current_copy_info->replacing = current_replace_list;
	current_copy_info->line = cb_source_line;
	current_copy_info->quotation_mark = quotation_mark;
	current_copy_info->source_format = cobc_get_source_format ();

	current_copy_info->next = copy_stack;
	current_copy_info->containing_files = old_list_file;
	if (copy_stack) {
		copy_stack->prev = current_copy_info;
	}
	copy_stack = current_copy_info;

	if (cb_current_file) {
		cb_current_file->copy_line = cb_source_line;
	}

	/* Set replacing list */
	if (replacing_list) {
		if (current_replace_list) {
			replacing_list->last->next = current_replace_list;
			replacing_list->last = current_replace_list->last;
		}
		current_replace_list = replacing_list;
		if (cb_src_list_file) {
			set_print_replace_list (replacing_list);
		}
	}

	dname = cobc_strdup (name);
	current_copy_info->dname = dname;
#if 0	/* Simon: better adjust the output where needed */
	for (s = dname; *s; ++s) {
		if (*s == '\\') {
			*s = '/';
		}
	}
#endif

	/* Switch to new buffer */
	switch_to_buffer (1, dname, yy_create_buffer (ppin, YY_BUF_SIZE));

	/* postponed errror handling */
	if (!ppin) return -1;
	return 0;
}

static const char *
ppcopy_try_open (const char *dir, const char *name, int has_ext)
{
	struct cb_text_list	*el = cb_extension_list;
	const char		*extension = "";
	struct stat st;

	for (;;) {
		if (dir) {
			snprintf (plexbuff2, (size_t)COB_SMALL_MAX, "%s%c%s%s",
				dir, SLASH_CHAR, name, extension);
		} else {
			snprintf (plexbuff2, (size_t)COB_SMALL_MAX, "%s%s",
				name, extension);
		}
		plexbuff2[COB_SMALL_MAX] = 0;
#if 0	/* TODO: output in separate COB_DEBUG_LOG */
		printf ("checking COPY file: %s... ", plexbuff2);
			printf ("Found!\n");
			printf ("%s\n", cb_get_strerror ());
#endif
		/* Must be an accessible, regular file */
		if (access (plexbuff2, R_OK) == 0
		 && stat (plexbuff2, &st) == 0
		 && S_ISREG (st.st_mode)) {
			return plexbuff2;
		}
#ifdef COB_MULTI_EXTENSION	/* compat to older GC versions */
		COB_UNUSED (has_ext):
#else
		if (has_ext) {
			break;
		}
#endif
		if (!el) {
			break;
		}
		extension = el->text;
		el = el->next;
	}
	return NULL;
}

/* try to locate the file by searching,
   each with all known copybook extensions:
   1 - as is
   2 - all known copybook directories */
static const char *
ppcopy_find_file (const char *name, int has_ext)
{
	const char* filename;

	/* try name as is + extensions ... */
	filename = ppcopy_try_open (NULL, name, has_ext);
	if (filename) {
		return filename;
	}

	/* ... if not found, try in copybook directories  */
	if (*name != SLASH_CHAR
#if defined (_WIN32) || defined (__DJGPP__)
	 && *name != '/' && *(name + 1) != ':'
#endif
		) {
		struct cb_text_list* il;
		for (il = cb_include_list; il; il = il->next) {
			filename = ppcopy_try_open (il->text, name, has_ext);
			if (filename) {
				return filename;
			}
		}
	}

	/* no candidate found */
	return NULL;
}

int
ppcopy (const char *name, const char *lib, struct cb_replace_list *replace_list)
{
	const char		*s;
	const char		*filename = NULL;
	const int 		has_ext = (strchr (name, '.') != NULL);

	if (cb_current_file) {
		cb_current_file->copy_line = cb_source_line;
	}

	/* TODO: open with path relative to the current file's path,
	         if any (applies both to with and without "lib") */

	/* Locate and open COPY file */
	if (lib) {
		const char *lib_env = NULL;
		s = plexbuff1;

		/* check for library-name with directory-name via environment
		   (as emulation of specifying a library-name via JCL) */
		if (*lib >= 'A' && *lib <= 'Z') {
			char envname[COB_MINI_BUFF];

			snprintf (envname, (size_t)COB_MINI_MAX, "COB_COPY_LIB_%s", lib);
			envname[COB_MINI_MAX] = 0;
			lib_env = getenv (envname);

			if (lib_env) {
				if (*lib_env) {
					/* TODO: if lib_env is not a full path:
					   prefix by current active file */
					snprintf (plexbuff1, (size_t)COB_SMALL_MAX, "%s%c%s",
						lib_env, SLASH_CHAR, name);
					plexbuff1[COB_SMALL_MAX] = 0;
					filename = ppcopy_find_file (s, has_ext);
				} else {
					filename = ppcopy_find_file (name, has_ext);
				}
			}
		}
		/* check for library-name as directory-name */
		if (!filename && !lib_env) {
			/* TODO: if lib is not a full path:
			   prefix by current active file */
			snprintf (plexbuff1, (size_t)COB_SMALL_MAX, "%s%c%s",
				lib, SLASH_CHAR, name);
			plexbuff1[COB_SMALL_MAX] = 0;
			filename = ppcopy_find_file (s, has_ext);
		}

		/* try without library name, if not resolved by env */
		if (!filename && !lib_env) {
			filename = ppcopy_find_file (name, has_ext);
			if (filename) {
				cb_plex_warning (COBC_WARN_FILLER, 0,
					_("copybook not found in library '%s', library-name ignored"),
					lib);
			}
		}

		/* restore original name for error handling */
		if (!filename) {
			snprintf (plexbuff1, (size_t)COB_SMALL_MAX, "%s%c%s",
				lib, SLASH_CHAR, name);
			plexbuff1[COB_SMALL_MAX] = 0;
		}
	} else {
		s = name;
		filename = ppcopy_find_file (s, has_ext);
	}
	
	/* expected case: filename found */
	if (likely (filename)) {
		int ret;
		ret = ppopen (filename, replace_list);
		/* expected case: copybook could be processed */
		if (likely (ret == 0)) {
			return 0;
		}
		/* otherwise fall-trough to error handling */
	} else {
		/* ensure to have errno from name as specified, not from another file */
		(void)access (s, R_OK);
		/* pass file error as we have no more places to check */
		cb_error ("%s: %s", s, cb_get_strerror ());
	}

	/* On COPY, open error restore old file */
	cb_current_file = old_list_file;
	fprintf (yyout, "#line %d \"%s\"\n", cb_source_line, cb_source_file);
	return -1;
}

void
ppparse_error (const char *err_msg)
{
	cb_plex_error (newline_count, "%s", err_msg);
}

/* Sets `source_format`, `cb_indicator_column`, `cb_text_column`,
   `floating_area_b`, and `fill_continued_alnums`, based on the given source
   format. */
void
cobc_set_source_format (const enum cb_format sf) {
	source_format = sf;

	switch (source_format) {
	case CB_FORMAT_ACUTERM:
	case CB_FORMAT_ICOBOL_CRT:
	case CB_FORMAT_COBOLX:
		cb_indicator_column = 1;
		floating_area_b = source_format != CB_FORMAT_COBOLX;
		fill_continued_alnums = 0;
		break;
	default:
		cb_indicator_column = 7;
		floating_area_b = 0;
		fill_continued_alnums = 1;
		break;
	}

	switch (source_format) {
	case CB_FORMAT_FIXED:
		cb_text_column = cb_config_text_column; /* 72 by default */
		break;
	case CB_FORMAT_VARIABLE:
		/* This value matches most MF Visual COBOL 4.0 version. */
		/* CHECKME: 500 seen in format_type entry of `ppparse.y`. */
		cb_text_column = 250;
		break;
	case CB_FORMAT_ACUTERM:
	case CB_FORMAT_ICOBOL_CRT:
		/* CHECKME:
		   https://sf.net/p/gnucobol/feature-requests/29/#c2d0/8d2f */
		cb_text_column = 320;
		break;
	case CB_FORMAT_COBOLX:
	case CB_FORMAT_ICOBOL_XCARD:
		cb_text_column = 255;
		break;
	case CB_FORMAT_FREE:
		/* text-column should be ignored: put an invalid value to catch
		   some bugs? */
		cb_text_column = -1;
		break;
	}
}

enum cb_format
cobc_get_source_format (void) {
	return source_format;
}

static int
cobc_parse_source_format (enum cb_format *const out, const char *const sfname) {
	enum cb_format format;
	if (!cb_strcasecmp (sfname, "FIXED")) {
		format = CB_FORMAT_FIXED;
	} else if (!cb_strcasecmp (sfname, "FREE")) {
		format = CB_FORMAT_FREE;
	} else if (!cb_strcasecmp (sfname, "VARIABLE")) {
		format = CB_FORMAT_VARIABLE;
	} else if (!cb_strcasecmp (sfname, "XCARD")) { /* CHECKME: name? */
		format = CB_FORMAT_ICOBOL_XCARD;
	} else if (!cb_strcasecmp (sfname, "CRT")) { /* CHECKME: name? */
		format = CB_FORMAT_ICOBOL_CRT;
	} else if (!cb_strcasecmp (sfname, "TERMINAL")) {
		format = CB_FORMAT_ACUTERM;
	} else if (!cb_strcasecmp (sfname, "COBOLX")) {
		format = CB_FORMAT_COBOLX;
	} else {
		return 0;	/* invalid argument */
	}
	*out = format;
	return 1;
}

int cobc_deciph_source_format (const char* const sfname) {
	enum cb_format format;
	if (cobc_parse_source_format (&format, sfname)) {
		cobc_set_source_format (format);
		return 1;
	}
	return 0;
}

void
plex_clear_vars (void)
{
	/* Reset variables */
	plex_skip_input = 0;
	plex_nest_depth = 0;
	memset (plex_cond_stack, 0, sizeof(plex_cond_stack));
	requires_listing_line = 1;
	comment_allowed = 1;
}

void
plex_clear_all (void)
{
	if (plexbuff1) {
		cobc_free (plexbuff1);
		plexbuff1 = NULL;
	}
	if (plexbuff2) {
		cobc_free (plexbuff2);
		plexbuff2 = NULL;
	}
}

void
plex_call_destroy (void)
{
	struct copy_info* ci;

	while (copy_stack) {
		ci = copy_stack;
		copy_stack = ci->next;
		cobc_free (ci);
	}

	(void)pplex_destroy ();
}

void
plex_action_directive (const enum cb_directive_action cmdtype, const unsigned int is_true)
{
	unsigned int	n;

	/* Action IF/ELSE/END-IF/ELIF */
	switch (cmdtype) {
	case PLEX_ACT_IF:
		/* Push stack - First occurrence is dummy */
		if (++plex_nest_depth >= PLEX_COND_DEPTH) {
			/* LCOV_EXCL_START */
			cobc_err_msg (_("directive nest depth exceeded: %d"),
					PLEX_COND_DEPTH);
			COBC_ABORT ();
			/* LCOV_EXCL_STOP */
		}
		plex_cond_stack[plex_nest_depth].cmd = 1U;
		/* Intersection with previous - first is always 0 */
		n = plex_cond_stack[plex_nest_depth - 1].skip | !is_true;
		plex_cond_stack[plex_nest_depth].skip = n;
		plex_cond_stack[plex_nest_depth].cond = is_true;
		plex_cond_stack[plex_nest_depth].line = cb_source_line;
		plex_skip_input = n;
		return;
	case PLEX_ACT_ELSE:
		/* Must have an associated IF/ELIF */
		if (!plex_nest_depth ||
		    plex_cond_stack[plex_nest_depth].cmd != 1) {
			cb_plex_error (newline_count,
				_("ELSE directive without matching IF/ELIF"));
			return;
		}
		plex_cond_stack[plex_nest_depth].cmd = 2U;
		/* Reverse any IF/ELIF condition */
		n = plex_cond_stack[plex_nest_depth].cond;
		plex_cond_stack[plex_nest_depth].skip = n;
		plex_cond_stack[plex_nest_depth].line = cb_source_line;
		/* Intersection with previous */
		plex_skip_input = plex_cond_stack[plex_nest_depth - 1].skip | n;
		return;
	case PLEX_ACT_END:
		/* Must have an associated IF/ELIF/ELSE */
		if (!plex_nest_depth ||
		    !plex_cond_stack[plex_nest_depth].cmd) {
			cb_plex_error (newline_count,
				_("END-IF directive without matching IF/ELIF/ELSE"));
			return;
		}
		plex_cond_stack[plex_nest_depth].cmd = 0;
		plex_cond_stack[plex_nest_depth].skip = 0;
		plex_cond_stack[plex_nest_depth].cond = 0;
		plex_cond_stack[plex_nest_depth].line = 0;
		/* Pop stack - set skip to previous */
		plex_nest_depth--;
		plex_skip_input = plex_cond_stack[plex_nest_depth].skip;
		return;
	case PLEX_ACT_ELIF:
		/* Must have an associated IF/ELIF */
		if (!plex_nest_depth ||
		    plex_cond_stack[plex_nest_depth].cmd != 1) {
			cb_plex_error (newline_count,
				_("ELIF directive without matching IF/ELIF"));
			return;
		}
		plex_cond_stack[plex_nest_depth].line = cb_source_line;
		if (plex_cond_stack[plex_nest_depth].cond) {
			/* Previous IF or one of previous ELIF was true */
			/* Set to skip */
			n = 1U;
		} else if (is_true) {
			/* Condition is true */
			plex_cond_stack[plex_nest_depth].cond = 1U;
			n = 0;
		} else {
			/* Set to skip */
			n = 1U;
		}
		plex_cond_stack[plex_nest_depth].skip = n;
		/* Intersection with previous */
		plex_skip_input = plex_cond_stack[plex_nest_depth - 1].skip | n;
		return;
	default:
		/* LCOV_EXCL_START */
		cobc_err_msg (_("invalid internal case: %u"),
				cmdtype);
		COBC_ABORT ();
		/* LCOV_EXCL_STOP */
	}
}

/* Local functions */

static void
get_new_listing_file (void)
{
	struct list_files	*newfile = cobc_malloc (sizeof (struct list_files));

	if (!cb_current_file->copy_head) {
		cb_current_file->copy_head = newfile;
	}
	if (cb_current_file->copy_tail) {
		cb_current_file->copy_tail->next = newfile;
	}
	cb_current_file->copy_tail = newfile;

	newfile->copy_line = cb_source_line;
	newfile->source_format = cobc_get_source_format ();
	old_list_file = cb_current_file;
	cb_current_file = newfile;
}

static void
set_print_replace_list (struct cb_replace_list *list)
{
	struct cb_replace_list		*r;
	const struct cb_text_list	*l;
	struct list_replace		*repl;
	size_t				length;

	for (r = list; r; r = r->next) {
		repl = cobc_malloc (sizeof (struct list_replace));
		repl->firstline = r->line_num;
		repl->lead_trail = r->lead_trail;
		repl->lastline = cb_source_line;

		for (l = r->old_text, length = 0; l; l = l->next) {
			length += strlen (l->text);
		}
		repl->from = cobc_malloc (length + 2);
		for (l = r->old_text; l; l = l->next) {
			strcat (repl->from, l->text);
		}

		for (l = r->new_text, length = 0; l; l = l->next) {
			length += strlen (l->text);
		}
		repl->to = cobc_malloc (length + 2);
		for (l = r->new_text; l; l = l->next) {
			strcat (repl->to, l->text);
		}

		if (cb_current_file->replace_tail) {
			cb_current_file->replace_tail->next = repl;
		}
		if (!cb_current_file->replace_head) {
			cb_current_file->replace_head = repl;
		}
		cb_current_file->replace_tail = repl;
	}
}

static void
switch_to_buffer (const int line, const char *file, const YY_BUFFER_STATE buffer)
{
	/* Reset file/line */
	cb_source_line = line;
	cb_source_file = cobc_plex_strdup (file);
	fprintf (yyout, "#line %d \"%s\"\n", line, file);
	/* Switch buffer */
	yy_switch_to_buffer (buffer);
}

static COB_INLINE COB_A_INLINE int
is_cobol_word_char (const char c)
{
	return c == '-' || c == '_' || isalnum (c);
}

static char *
get_word (const char *str, char *word_buff)
{
	char *ret = NULL;
	if (str) {
		int length = COB_MAX_WORDLEN + 1;
		while (*str == ' ' || *str == ',' || *str == ';') {
			++str;
		}
		ret = word_buff;
		while (is_cobol_word_char(*str)
		    && length--) {
			*word_buff++ = toupper((unsigned char)*str++);
		}
		if (length != 0) {
			*word_buff = 0;
		} else {
			ret = NULL;
		}		
	}
	return ret;
}

static int
is_condition_directive_clause (const char *buff)
{
	char word_buff[COB_MAX_WORDLEN + 1];
	char *p;
	if (buff[0] == '$') {
		buff++;
	} else if (buff[0] == '>' && buff[1] == '>') {
		buff += 2;
	}
	p = get_word (buff, word_buff);

	if (p == NULL) {
		return 0;
	}

	return (strcmp (p, "IF") == 0
		|| strcmp (p, "ELSE") == 0
		|| strcmp (p, "END-IF") == 0
		|| strcmp (p, "ELIF") == 0
		|| strcmp (p, "END") == 0
		|| strcmp (p, "EVALUATE") == 0
		|| strcmp (p, "WHEN") == 0)
		|| strcmp (p, "END-EVALUATE") == 0;
}

static int
next_word_is_comment_paragraph_name (const char *buff)
{
	char word_buff[COB_MAX_WORDLEN + 1];
	const char	*p = get_word (buff, word_buff);

	if (!p) {
		return 0;
	}
	if (strcmp (p, "AUTHOR") == 0
	 || strcmp (p, "DATE-WRITTEN") == 0
	 || strcmp (p, "DATE-MODIFIED") == 0
	 || strcmp (p, "DATE-COMPILED") == 0
	 || strcmp (p, "INSTALLATION") == 0
	 || strcmp (p, "REMARKS") == 0
	 || strcmp (p, "SECURITY") == 0) {
		if (cb_comment_paragraphs != CB_IGNORE) {
			/* check so we get warnings / errors, but leave result unchanged */
			cb_plex_verify (newline_count, cb_comment_paragraphs, p);
			return 1;
		}
	}
	return 0;
}


/* FIXME: try to optimize as this function used 25-10% (according to callgrind)
   of the complete time spent in a sample run with
   -fsyntax-only on 880 production code files (2,500,000 LOC)
   --> too much  [Note: 10% are spent in getc and therefore could only
   be optimized by using a buffered read instead]
*/
static int
ppinput (char *buff, const size_t max_size)
{
	char	*bp;
	size_t	gotcr;
	size_t	line_overflow;
	size_t	continuation;
	size_t	indicator_width;
	int	ipchar;
	int	i;
	int	n;
	int	coln;
	struct list_skip *skip;

	/* check that we actually have input to process
	   (isn't the case if something went wrong beforehand) */
	/* LCOV_EXCL_START */
	if (unlikely (ppin == NULL)) {
		return YY_NULL; /* fake eof (no further processing of the not available file) */
	}
	/* LCOV_EXCL_STOP */

	/* Read line(s) */

	continuation = 0;
start:
	if (unlikely (buffer_overflow ||
		     (newline_count + PPLEX_BUFF_LEN) >= max_size)) {
		if (need_continuation || continuation) {
			cb_plex_error (newline_count,
					_("buffer overrun - too many continuation lines"));
#if 0		/* CHECKME: does anything breaks if we don't fake EOF here? */
			return YY_NULL; /* fake eof (no further processing) */
#endif
		}
		if (newline_count < max_size) {
			memset (buff, '\n', newline_count);
			buff[newline_count] = 0;
			ipchar = (int)newline_count;
			newline_count = 0;
			buffer_overflow = 0;
			return ipchar;
		}
		buffer_overflow = 1;
		ipchar = max_size - 1;
		memset (buff, '\n', (size_t)ipchar);
		buff[ipchar] = 0;
		newline_count -= ipchar;
		return ipchar;
	}
	gotcr = 0;
	line_overflow = 0;
	ipchar = 0;
	for (n = 0; ipchar != '\n';) {
		if (unlikely (n == PPLEX_BUFF_LEN)) {
			if (line_overflow != 2) {
				line_overflow = 1;
			}
		}
		ipchar = getc (ppin);
		if (unlikely (ipchar == EOF)) {
			if (n > 0) {
				/* No end of line at end of file
				   ignore if all space,
				   otherwise raise an error */
				do {
					if (buff[--n] != ' ') break;
				} while (n != 0);
				if (n != 0) break;
			}
			if (newline_count == 0) {
				return YY_NULL;
			}
			memset (buff, '\n', newline_count);
			buff[newline_count] = 0;
			ipchar = (int)newline_count;
			newline_count = 0;
			return ipchar;
		}
#ifndef	COB_EBCDIC_MACHINE
		if (unlikely (ipchar == 0x1A && !n)) {
			continue;
		}
#endif
		if (unlikely (gotcr)) {
			gotcr = 0;
			if (ipchar != '\n') {
				if (likely (line_overflow == 0)) {
					buff[n++] = '\r';
				} else {
					line_overflow = 2;
				}
			}
		}
		if (unlikely (ipchar == '\r')) {
			gotcr = 1;
			continue;
		}
		if (unlikely (ipchar == '\t')) {
			if (likely (line_overflow == 0)) {
				buff[n++] = ' ';
				while (n % cb_tab_width != 0) {
					buff[n++] = ' ';
				}
				if (unlikely (n > PPLEX_BUFF_LEN)) {
					n = PPLEX_BUFF_LEN;
				}
			}
			continue;
		}
		if (likely (line_overflow == 0)) {
			buff[n++] = (char)ipchar;
		} else if ((char)ipchar != ' ' && (char)ipchar != '\n') {
			line_overflow = 2;
		}
	}

	if (buff[n - 1] != '\n') {
		/* FIXME: cb_source_line is one too low when CB_FORMAT_FREE is used
		   [but only during ppinput() in pplex.l ?]
		   Workaround for now: Temporary newline_count + 1
		*/
		if (source_format == CB_FORMAT_FREE) {
			if (line_overflow == 0) {
				cb_plex_warning (cb_missing_newline, newline_count + 1,
						_("line not terminated by a newline"));
				n++;
			} else if (line_overflow == 2) {
				cb_plex_warning (COBC_WARN_FILLER, newline_count + 1,
						_("source text exceeds %d bytes, will be truncated"),
						  PPLEX_BUFF_LEN);
			}
		} else {
			if (line_overflow == 0) {
				cb_plex_warning (cb_missing_newline, newline_count,
						_("line not terminated by a newline"));
				n++;
			} else if (line_overflow == 2) {
				cb_plex_warning (COBC_WARN_FILLER, newline_count,
						_("source text exceeds %d bytes, will be truncated"),
						  PPLEX_BUFF_LEN);
			}
		}
		buff[n++] = '\n';
	}
	buff[n] = 0;

	/* check for vcs conflict marker */
	if (n == 8 || (n > 8 && isspace((unsigned char) buff[7]))) {
		if (memcmp("<<<<<<<", buff, 7) == 0
		 || memcmp("=======", buff, 7) == 0
		 || memcmp(">>>>>>>", buff, 7) == 0) {
			/* FIXME: the different line numbers (see test "conflict markers"
			   are definitely a bug to solve in short time */
			if (source_format == CB_FORMAT_FREE) {
				++newline_count;
			}
			cb_plex_error (newline_count,
				_("version control conflict marker in file"));
			if (source_format != CB_FORMAT_FREE) {
				++newline_count;
			}
			goto start;
		}
	}

	if (source_format == CB_FORMAT_FREE) {
		bp = buff;
	} else {
		if (n <= cb_indicator_column) {
			/* Line too short */
			newline_count++;
			goto start;
		}

		if (cb_flag_mfcomment) {
			if (buff[0] == '*' || buff[0] == '/') {
				newline_count++;
				goto start;
			}
		}

		/* Check if text is longer than cb_text_column */
		if (n > cb_text_column + 1) {
			/* Show warning if it is not whitespace
			   (postponed after checking for comments by setting
			    line_overflow to first column that leads to
			    "source text too long")
			*/
			if (cb_warn_opt_val[cb_warn_column_overflow] && line_overflow == 0) {
				for (coln = cb_text_column; coln < n; ++coln) {
					if (buff[coln] != ' ' && buff[coln] != '\n') {
						line_overflow = coln;
						break;
					}
				}
			} else {
				line_overflow = 0;
			}
			/* Remove it */
			buff[cb_text_column] = '\n';
			buff[cb_text_column + 1] = 0;
			n = cb_text_column + 1;
		} else {
			line_overflow = 0;
		}

		memset (buff, ' ', (size_t)(cb_indicator_column - 1));
		/* Note we allow directive lines to start in the indicator column */
		bp = &buff[cb_indicator_column - 1];

		/* Special case: acucomment must be checked here as we'd pass comments
		   as directives otherwise */
		if (cb_flag_acucomment && buff[cb_indicator_column - 1] == '$') {
			buff[cb_indicator_column - 1] = '*';
		}
	}

	/* Check for directives/floating comment at first non-space of line */
	ipchar = 0;
	for (; *bp; bp++) {
		if (*bp != ' ') {
			if ((*bp == '$' && bp[1] != ' ') || (*bp == '>' && bp[1] == '>')) {
				/* Directive */
				ipchar = 1;
			} else if (*bp == '*' && bp[1] == '>') {
				/* Float comment */
				newline_count++;
				goto start;
			} else if (cb_flag_acucomment && *bp == '|') {
				/* ACU Float comment */
				newline_count++;
				goto start;
			}
			break;
		}
	}
	if (ipchar && (!plex_skip_input
	            || is_condition_directive_clause (bp))) {
		/* Directive - pass complete line with NL to ppparse */
		if (newline_count) {
			/* Move including NL and NULL byte */
			memmove (buff + newline_count, buff, (size_t)n + 1);
			memset (buff, '\n', newline_count);
			n += newline_count;
			newline_count = 0;
		}
		return n;
	}

	if (plex_skip_input) {
		/* Skipping input */
		newline_count++;
		if (cb_src_list_file) {
			skip = cobc_malloc (sizeof (struct list_skip));
			skip->skipline = cb_source_line + (int)newline_count;

			if (cb_current_file->skip_tail) {
				cb_current_file->skip_tail->next = skip;
			}
			cb_current_file->skip_tail = skip;

			if (!cb_current_file->skip_head) {
				cb_current_file->skip_head = skip;
			}
		}
		goto start;
	}

	/*
	  Check that line isn't start of ID DIVISION comment paragraph.
	*/
	if (comment_allowed && next_word_is_comment_paragraph_name (bp)) {
		/* Skip comments until the end of line. */
		within_comment = 1;
		++newline_count;
		goto start;
	}

	/* Return when free format (no floating comments removed!) */
	if (source_format == CB_FORMAT_FREE) {
		within_comment = 0;
		if (newline_count) {
			memmove (buff + newline_count, buff, (size_t)n + 1);
			memset (buff, '\n', newline_count);
			n += newline_count;
			newline_count = 0;
		}
		return n;
	}

	/* Fixed format */

	/* Check the indicator */
	indicator_width = 1;
	switch (buff[cb_indicator_column - 1]) {
	case ' ':
		break;
	case '-':
		if (unlikely (within_comment)) {
			cb_plex_error (newline_count,
					_("invalid continuation in comment entry"));
			newline_count++;
			goto start;
		} else if (!need_continuation) {
			cb_plex_verify (newline_count, cb_word_continuation,
					_("continuation of COBOL words"));
		}
		continuation = 1;
		break;
	case 'd':
	case 'D':
		if (source_format == CB_FORMAT_ACUTERM) {
			indicator_width = 0;
			break; /* ACU terminal: this 'D' denotes a nominal text line */
		}
		/* Debugging line */
		(void) cb_verify (cb_debugging_mode, _("debugging indicator"));
		if (cb_flag_debugging_line) {
			break;
		}
		newline_count++;
		goto start;
	case '*':	/* Comment line */
	case '/':	/* Comment line requested page-break in listing */
		newline_count++;
		goto start;
	case '\\':
		if (buff[cb_indicator_column] == 'D' &&
		    source_format == CB_FORMAT_ACUTERM) {
			/* ACUTERM debugging line */
			(void) cb_verify (cb_debugging_mode, _("debugging indicator"));
			indicator_width = 2;
			if (cb_flag_debugging_line) {
				break;
			}
		} else {
			/* Invalid ACU terminal indicator? */
			cb_plex_error (newline_count,
				       _("invalid indicator '\\' at column %d"),
				       cb_indicator_column);
		}
		newline_count++;
		goto start;
	default:
		if (floating_area_b) { /* indicator is optional */
			indicator_width = 0;
			break;
		}
		/* Invalid indicator */
		cb_plex_error (newline_count,
				_("invalid indicator '%c' at column %d"),
				buff[cb_indicator_column - 1], cb_indicator_column);
		/* Note: Treat as comment line to allow further parsing
		         instead of aborting compilation */
		newline_count++;
		goto start;
	}

	/* Skip comments that follow after AUTHORS, etc. */
	if (unlikely (within_comment)) {
		/* Check all of "Area A" */
		/* CHECKME: MF's doc on TERMINAL format reads "Area B starts in
		   column 5 and extends to the end of the line."  Does this hold
		   for debug lines "\D ..." as well? */
		const int margin_b = cb_indicator_column + indicator_width - 1 + 4;
		const int ipchar_max = (n - 1 < margin_b) ? n - 1 : margin_b;
		for (ipchar = cb_indicator_column; ipchar < ipchar_max; ++ipchar) {
			if (buff[ipchar] != ' ') {
				ipchar = 0;
				break;
			}
		}
		if (ipchar) {
			newline_count++;
			goto start;
		}
		within_comment = 0;
	}

	/* Skip blank lines */
	for (i = cb_indicator_column; buff[i] == ' '; ++i);
	if (buff[i] == '\n') {
		newline_count++;
		goto start;
	}

	/* Substitute spaces for indicator */
	const int margin_a = cb_indicator_column + indicator_width - 1;
	for (i = cb_indicator_column - 1; i < margin_a; i++) {
		buff[i] = ' ';
	}
	bp = buff + margin_a;

	if (unlikely (continuation)) {
		/* Line continuation */
		need_continuation = 0;
		for (; *bp == ' '; ++bp) {
			;
		}
		/* Validate concatenation */
		if (consecutive_quotation) {
			if (bp[0] == quotation_mark && bp[1] == quotation_mark) {
				bp++;
			} else {
				cb_plex_error (newline_count,
						_("invalid line continuation"));
				return YY_NULL;
			}
			quotation_mark = 0;
			consecutive_quotation = 0;
		} else if (quotation_mark) {
			/* Literal concatenation */
			if (*bp == quotation_mark) {
				bp++;
			} else {
				cb_plex_error (newline_count,
						_("invalid line continuation"));
				return YY_NULL;
			}
		}
	} else {
		/* Normal line */
		if (need_continuation) {
			cb_plex_error (newline_count,
					_("continuation character expected"));
			need_continuation = 0;
		}
		quotation_mark = 0;
		consecutive_quotation = 0;
	}

	/* Check if string literal is to be continued */
	for (i = bp - buff; buff[i] != '\n'; ++i) {
		/* Pick up floating comment and force loop exit */
		if (!quotation_mark && ((buff[i] == '*' && buff[i + 1] == '>') ||
			                    (cb_flag_acucomment && buff[i] == '|'))) {
			/* remove indicator "source text too long" if the column
			   leading to the indicator comes after the floating comment
			*/
			if (i < cb_text_column) {
				line_overflow = 0;
			}
			/* Set to null, 'i' is predecremented further below */
			buff[i] = 0;
			break;
		} else if (buff[i] == '\'' || buff[i] == '"') {
			if (quotation_mark == 0) {
				/* Literal start */
				quotation_mark = buff[i];
			} else if (quotation_mark == buff[i]) {
				if (i == cb_text_column - 1) {
					/* Consecutive quotation */
					consecutive_quotation = 1;
				} else {
					/* Literal end */
					quotation_mark = 0;
				}
			}
		}
	}

	if (unlikely (quotation_mark)) {
		/* Expecting continuation */
		if (!consecutive_quotation) {
			need_continuation = 1;
		}
		if (fill_continued_alnums) {
			for (; i < cb_text_column;) {
				buff[i++] = ' ';
			}
		}
		buff[i] = 0;
	} else {
		/* Truncate trailing spaces, including the newline */
		for (i--; i >= 0 && buff[i] == ' '; i--) {
			;
		}
		if (i < 0) {
			/* Empty line after removing floating comment */
			newline_count++;
			goto start;
		}
		if (buff[i] == '\'' || buff[i] == '\"') {
			buff[++i] = ' ';
		}
		buff[i + 1] = 0;
	}

	/* Show warning if text is longer than cb_text_column
	   and not whitespace (postponed here) */
	if (line_overflow != 0) {
		cb_plex_warning (cb_warn_source_after_code, newline_count,
				 _("source text after program-text area (column %d)"),
				cb_text_column);
	}

	if (unlikely (continuation)) {
		gotcr = strlen (bp);
		memmove (buff, bp, gotcr + 1);
		newline_count++;
	} else {
		/* Insert newlines at the start of the buffer */
		gotcr = strlen (buff);
		if (newline_count != 0) {
			memmove (buff + newline_count, buff, gotcr + 1);
			memset (buff, '\n', newline_count);
			gotcr += newline_count;
		}
		newline_count = 1;
	}
	return (int)gotcr;
}

static struct cb_text_list *
pp_text_list_add (struct cb_text_list *list, const char *text,
		  const size_t size)
{
	struct cb_text_list	*p;
	void			*tp;

	p = cobc_plex_malloc (sizeof (struct cb_text_list));
	tp = cobc_plex_malloc (size + 1);
	memcpy (tp, text, size);
	p->text = tp;
	if (!list) {
		p->last = p;
		return p;
	}
	list->last->next = p;
	list->last = p;
	return list;
}

static void
ppecho (const char *text, const cob_u32_t alt_space, const int textlen)
{
	/* performance note: while this function used 5% (according to callgrind)
	   of the complete time spent in a sample run with
	   -fsyntax-only on 880 production code files (2,500,000 LOC)
	   3.8% of this time is spent in fwrite, therefore not much potential
	   for optimization
	*/
	struct cb_replace_list		*r;
	struct cb_replace_list		*save_ptr;
	const struct cb_text_list	*lno;
	struct cb_text_list		*queue;
	struct cb_text_list		*save_queue;
	const char			*s;
	char				*temp_ptr;
	size_t				size;
	size_t				size2;

	/* ensure nothing is in the stream buffer */
	fflush (ppout);

	/* Check for replacement text before outputting */
	if (alt_space) {
		s = yytext;
	} else {
		s = text;
	}

	if (text_queue == NULL && (text[0] == ' ' || text[0] == '\n')) {
		/* No replacement */
		fwrite (text, (size_t)textlen, (size_t)1, ppout);
		/* TODO: instead of \n (empty line: set "needs source-loc" flag and
		   before first non-empty line output a #line directive, saving
		   quite some file io [keep 1 empty line]) */
		if (cb_listing_file) {
			check_listing (s, 0);
		}
		return;
	}
	if (!current_replace_list && !base_replace_list) {
		/* Output queue */
		for (; text_queue; text_queue = text_queue->next) {
			fputs (text_queue->text, ppout);
		}
		fwrite (text, (size_t)textlen, (size_t)1, ppout);
		if (cb_listing_file) {
			check_listing (s, 0);
		}
		return;
	}
	if (!current_replace_list) {
		current_replace_list = base_replace_list;
		save_ptr = NULL;
	} else {
		current_replace_list->last->next = base_replace_list;
		save_ptr = current_replace_list->last;
	}

	/* Do replacement */
	text_queue = pp_text_list_add (text_queue, text, (size_t)textlen);

	save_queue = NULL;
	size = 0;
	size2 = 0;
	for (r = current_replace_list; r; r = r->next) {
		queue = text_queue;
		/* The LEADING/TRAILING code looks peculiar as we use */
		/* variables after breaking out of the loop BUT */
		/* ppparse.y guarantees that we have only one token */
		/* and therefore only one iteration of this loop */
		for (lno = r->old_text; lno; lno = lno->next) {
			if (lno->text[0] == ' ' || lno->text[0] == '\n') {
				continue;
			}
			while (queue && (queue->text[0] == ' ' ||
			       queue->text[0] == '\n')) {
				queue = queue->next;
			}
			if (queue == NULL) {
				/* Partial match */
				if (!save_ptr) {
					current_replace_list = NULL;
				} else {
					save_ptr->next = NULL;
				}
				return;
			}
			if (r->lead_trail == CB_REPLACE_LEADING) {
				/* Check leading text */
				size = strlen (lno->text);
				if (strncasecmp (lno->text, queue->text, size)) {
					/* No match */
					break;
				}
				save_queue = queue;
			} else if (r->lead_trail == CB_REPLACE_TRAILING) {
				/* Check trailing text */
				size = strlen (lno->text);
				size2 = strlen (queue->text);
				if (size2 < size) {
					/* No match */
					break;
				}
				size2 -= size;
				if (strncasecmp (lno->text, queue->text + size2, size)) {
					/* No match */
					break;
				}
				save_queue = queue;
			} else if (strcasecmp (lno->text, queue->text)) {
				/* No match */
				break;
			}
			queue = queue->next;
		}
		if (lno == NULL) {
			/* Match */
			if (r->lead_trail == CB_REPLACE_TRAILING
				&& save_queue /* <- silence warnings */) {
				/* Non-matched part of original text */
				fprintf (ppout, "%*.*s", (int)size2, (int)size2,
					 save_queue->text);
				if (cb_listing_file) {
					temp_ptr = cobc_strdup (save_queue->text);
					*(temp_ptr + size2) = 0;
					check_listing (temp_ptr, 0);
					cobc_free (temp_ptr);
				}
			}
			for (lno = r->new_text; lno; lno = lno->next) {
				ppecho_direct (lno->text);
			}
			if (r->lead_trail == CB_REPLACE_LEADING
				&& save_queue /* <- silence warnings */) {
				/* Non-matched part of original text */
				ppecho_direct (save_queue->text + size);
			}
			text_queue = queue;
			continue;
		}
	}

	/* No match */
	for (; text_queue; text_queue = text_queue->next) {
		ppecho_direct (text_queue->text);
	}
	if (!save_ptr) {
		current_replace_list = NULL;
	} else {
		save_ptr->next = NULL;
	}
}

static void
skip_to_eol (void)
{
	int	c;

	/* Skip bytes to end of line */
	while ((c = input ()) != EOF) {
		if (c == '\n') {
			break;
		}
	}
	if (c != EOF) {
		unput (c);
	}
}

static void
display_finish (void)
{
	if (!plex_skip_input) {
		int msg_len = strlen (display_msg) - 1;
		while (msg_len != 0 && display_msg[msg_len] == ' ') {
			display_msg[msg_len--] = 0;
		}
		puts (display_msg);
		display_msg[0] = 0;
	}
	unput ('\n');
}

static void
ppecho_direct (const char *text)
{
	fputs (text, ppout);
	if (cb_listing_file) {
		check_listing (text, 0);
	}
}

static void
check_listing (const char *text, const unsigned int comment)
{
	const char	*s;
	char		c;

	/* Check for listing */
	if (!cb_listing_file) {
		/* Nothing to do */
		return;
	}
	if (!text) {
		return;
	}
#ifndef COB_INTERNAL_XREF
	if (cobc_gen_listing == 2) {
		/* Passed to cobxref */
		fputs (text, cb_listing_file);
		return;
	}
#endif
	if (comment) {
		c = '*';
	} else {
		c = ' ';
	}

	if (requires_listing_line) {
		if (requires_new_line) {
			requires_new_line = 0;
			putc ('\n', cb_listing_file);
		}
		fprintf (cb_listing_file, "%6d%c", ++listing_line, c);
	}

	/* FIXME: what's the condition here? */
	if (requires_listing_line && source_format != CB_FORMAT_FREE &&
	    strlen (text) > 6) {
		s = &text[6];
	} else {
		s = text;
	}
	fputs (s, cb_listing_file);
	if (strchr (text, '\n')) {
		requires_listing_line = 1;
	} else {
		requires_listing_line = 0;
	}
}

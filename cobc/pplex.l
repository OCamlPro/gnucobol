/*
   Copyright (C) 2001-2012, 2014-2023 Free Software Foundation, Inc.
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
%option noyywrap

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

static void insert_copy_arg (void);

#define	PPLEX_BUFF_LEN		512
#define YY_INPUT(buf,result,max_size)	result = ppinput (buf, max_size);
#define	ECHO				fputs (yytext, yyout)

/* The first --copy COPYBOOK is inserted using this macro. The next
   ones will be inserted in <<EOF>>, when we come back to the toplevel
   source file. */
#define	YY_USER_INIT							\
	requires_listing_line = 1;					\
	comment_allowed = 1;						\
	copy_list_pointer = cb_copy_list;				\
	insert_copy_arg ();


#include "config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#else
#define	YY_NO_UNISTD_H	1
#endif
#include <ctype.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <stdio.h>
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
	int			indicator_error;
};

struct plex_stack {
	unsigned int		cmd;
	unsigned int		skip;
	unsigned int		cond;
	int			line;
};

/* Global variables */

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
static int			echo_newline = 0;
static int			listing_line = 0;
static int			requires_listing_line;
static enum cb_format		source_format = CB_FORMAT_AUTO;
static int	indicator_column = 7;
static int	indicator_error = 0;
static int	text_column = 72;	   /* end of area B (in single-byte
					      characters) */
static int	floating_area_b = 0;	   /* whether indicator is optional */
static int	fill_continued_alnums = 1; /* whether continued alphanumeric
					      literals should be filled with
					      spaces up to text column */
static int	emit_area_a_tokens = 0;

static char	display_msg[PPLEX_BUFF_LEN];

static struct copy_info		*copy_stack = NULL;

static struct plex_stack	plex_cond_stack[PLEX_COND_DEPTH];

/* Function declarations */
static int	ppinput			(char *, const size_t);
static void	ppecho                  (const char *text, const char *token );

static void	switch_to_buffer	(const int, const char *,
					 const YY_BUFFER_STATE);
static void	check_listing		(const char *, const unsigned int);
static void	skip_to_eol		(void);
static void	count_newlines		(const char *);
static void	display_finish		(void);
static void	get_new_listing_file	(void);
static void	output_pending_newlines	(FILE *);

static struct cb_text_list	*pp_text_list_add (struct cb_text_list *,
					 const char *, const size_t);

static struct cb_text_list *copy_list_pointer = NULL;

static void insert_copy_arg (void)
{
	if (copy_list_pointer != NULL) {
		int ret = ppcopy (copy_list_pointer->text, NULL, NULL);
		if (ret < 0) { /* This should never happen, as we already test it before */
			cobc_err_msg (_("fatal error: %s"), "could not find --copy argument");
			cobc_abort_terminate (0);
		}
		copy_list_pointer = copy_list_pointer->next;
	}
}

%}

WORD		[_0-9A-Z\x80-\xFF-]+
NUMRIC_LITERAL	[+-]?[0-9,.]*[0-9]
ALNUM_LITERAL_Q	"\""([^""\n]|("\""[0-9][0-9, ]*"\""))*"\""
ALNUM_LITERAL_A	"\'"([^''\n]|("\'"[0-9][0-9, ]+"\'"))*"\'"
ALNUM_LITERAL	{ALNUM_LITERAL_Q}|{ALNUM_LITERAL_A}
SET_PAREN_LIT	\([^()\n]*\)
DEFNUM_LITERAL	[+-]?[0-9]*[\.]*[0-9]+

AREA_A		[ ]?#
MAYBE_AREA_A	[ ]?#?

%x CALL_DIRECTIVE_STATE
%x COBOL_WORDS_DIRECTIVE_STATE
%x COPY_STATE
%x REPLACING_STATE
%x PSEUDO_STATE
%x REPLACE_STATE
%x CONTROL_DIVISION_STATE
%x SUBSTITUTION_SECTION_STATE
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
		ppecho (" ", NULL);
	}
	#endif
}

^{MAYBE_AREA_A}[ ]*">>"[ ]?"COBOL-WORDS"	{
	/* 202x+: directive for setting source format */
	BEGIN COBOL_WORDS_DIRECTIVE_STATE;
	output_pending_newlines (ppout);
	return COBOL_WORDS_DIRECTIVE;
}

^{MAYBE_AREA_A}[ ]*">>"[ ]?"DEFINE"	{
	/* 2002+: definition of compiler constants display message during compilation */
	/* Define here to preempt next debug rule below */
	BEGIN DEFINE_DIRECTIVE_STATE;
	output_pending_newlines (ppout);
	return DEFINE_DIRECTIVE;
}

^{MAYBE_AREA_A}[ ]*">>"[ ]?"DISPLAY"[ ]+ {
	/* previous OpenCOBOL/GnuCOBOL 2.x extension, added in COBOL 202x with slightly different syntax:
	   display message during compilation --> needs a dialect option to switch to the appropriate state */
	display_msg[0] = 0;
	output_pending_newlines (ppout);
	BEGIN DISPLAY_DIRECTIVE_STATE;
}

^{MAYBE_AREA_A}[ ]*">>"[ ]?"REF-MOD-ZERO-LENGTH"	{
	/* 202x: directive to allow zero ref-mod */
	BEGIN ON_OFF_DIRECTIVE_STATE;
	output_pending_newlines (ppout);
	return REFMOD_DIRECTIVE;
}

^{MAYBE_AREA_A}[ ]*">>D"		{
	/* 2002 (only) floating debug line */
	/* Remove line if debugging lines not activated */
	/* Otherwise ignore the directive part of the line */
	(void) cb_verify (cb_debugging_mode, _("debugging indicator"));
	if (!cb_flag_debugging_line) {
		skip_to_eol ();
	}
}

^{MAYBE_AREA_A}[ ]*">>"[ ]?"PAGE"	{
	/* 2002+: listing directive for page eject with optional comment
	   Note: processed in cobc.c */
	skip_to_eol ();
}

^{MAYBE_AREA_A}[ ]*">>"[ ]?"LISTING"	{
	/* 2002+: listing directive for (de-)activating the listing,
	   ON implied for empty value
	   Note: further checks in ppparse.y, processed in cobc.c */
	BEGIN ON_OFF_DIRECTIVE_STATE;
	output_pending_newlines (ppout);
	return LISTING_DIRECTIVE;
}

^{MAYBE_AREA_A}[ ]*">>"[ ]?"SOURCE"	{
	/* 2002+: directive for setting source format */
	BEGIN SOURCE_DIRECTIVE_STATE;
	output_pending_newlines (ppout);
	return SOURCE_DIRECTIVE;
}

^{MAYBE_AREA_A}[ ]*">>"[ ]?"SET"	{
	/* OpenCOBOL/GnuCOBOL 2.0 extension: MF SET directive in 2002+ style format */
	BEGIN SET_DIRECTIVE_STATE;
	output_pending_newlines (ppout);
	return SET_DIRECTIVE;
}

^{MAYBE_AREA_A}[ ]*">>"[ ]?"TURN"	{
	/* 2002+: directive for (de-)activating exception checks */
	BEGIN TURN_DIRECTIVE_STATE;
	output_pending_newlines (ppout);
	return TURN_DIRECTIVE;
}

^{MAYBE_AREA_A}[ ]*">>"[ ]?"IF"	{
	/* 2002+: conditional compilation */
	BEGIN IF_DIRECTIVE_STATE;
	output_pending_newlines (ppout);
	return IF_DIRECTIVE;
}
^{MAYBE_AREA_A}[ ]*">>"[ ]?"ELIF" |
^{MAYBE_AREA_A}[ ]*">>"[ ]?"ELSE-IF"	{
	/* OpenCOBOL extension: conditional compilation combined ELSE IF,
	   2002+ style format */
	BEGIN IF_DIRECTIVE_STATE;
	output_pending_newlines (ppout);
	return ELIF_DIRECTIVE;
}
^{MAYBE_AREA_A}[ ]*">>"[ ]?"ELSE"	{
	/* 2002+: conditional compilation */
	BEGIN ELSE_DIRECTIVE_STATE;
	output_pending_newlines (ppout);
	return ELSE_DIRECTIVE;
}
^{MAYBE_AREA_A}[ ]*">>"[ ]?"END-IF"	{
	/* 2002+: conditional compilation */
	BEGIN ENDIF_DIRECTIVE_STATE;
	output_pending_newlines (ppout);
	return ENDIF_DIRECTIVE;
}

^{MAYBE_AREA_A}[ ]*">>"[ ]?"LEAP-SECOND"	{
	/* 2002+: more then 60 seconds per minute (currently always set to off),
	          OFF implied for empty value */
	BEGIN ON_OFF_DIRECTIVE_STATE;
	output_pending_newlines (ppout);
	return LEAP_SECOND_DIRECTIVE;
}

^{MAYBE_AREA_A}[ ]*">>"[ ]?"CALL-CONVENTION"	{
	/* 2002+: convention for CALL/CANCEL */
	BEGIN CALL_DIRECTIVE_STATE;
	output_pending_newlines (ppout);
	return CALL_DIRECTIVE;
}

^{MAYBE_AREA_A}[ ]*">>"[ ]*\n		{
	/* empty 2002+ style directive */
	cb_plex_warning (COBC_WARN_FILLER, newline_count,
			_("ignoring empty directive"));
	unput ('\n');
}

^{MAYBE_AREA_A}[ ]*">>"[ ]*[_0-9A-Z-]+	{
	/* unknown 2002+ style directive */
	char	*s;

	s = strchr (yytext, '>');
	cb_plex_warning (COBC_WARN_FILLER, newline_count,
			_("ignoring invalid directive: '%s'"), s);
	skip_to_eol ();
}

^{MAYBE_AREA_A}[ ]*">>"		{
	/* unknown 2002+ style directive */
	cb_plex_warning (COBC_WARN_FILLER, newline_count,
			_("ignoring invalid directive"));
	skip_to_eol ();
}

^{MAYBE_AREA_A}[ ]*"$DISPLAY"[ ]+"VCS"[ ]+"="[ ]+	{
	/* MF extension: include @(#)text\0 in the object file */
	/* we just add a warning for now, maybe implement it later */
	CB_PENDING (_("VCS directive"));
	skip_to_eol ();
}

^{MAYBE_AREA_A}[ ]*"$DISPLAY"[ ]+		{
	/* MF extension: display message during compilation */
	display_msg[0] = 0;
	BEGIN DISPLAY_DIRECTIVE_STATE;
	output_pending_newlines (ppout);
}

^{MAYBE_AREA_A}[ ]*$[ \t]*"SET"		{
	/* MF extension: SET directive */
	/* TODO: check position of the $SET directive */
	BEGIN SET_DIRECTIVE_STATE;
	output_pending_newlines (ppout);
	return SET_DIRECTIVE;
}

^{MAYBE_AREA_A}[ ]*$[ \t]*"IF"		{
	/* MF extension: conditional compilation */
	BEGIN IF_DIRECTIVE_STATE;
	output_pending_newlines (ppout);
	return IF_DIRECTIVE;
}
^{MAYBE_AREA_A}[ ]*$[ \t]*"ELIF" |
^{MAYBE_AREA_A}[ ]*$[ \t]*"ELSE-IF"		{
	/* OpenCOBOL/GnuCOBOL 2.0 extension: conditional compilation combined ELSE IF,
	   MF style format */
	BEGIN IF_DIRECTIVE_STATE;
	output_pending_newlines (ppout);
	return ELIF_DIRECTIVE;
}
^{MAYBE_AREA_A}[ ]*$[ \t]*"ELSE"		{
	/* MF extension: conditional compilation */
	BEGIN ELSE_DIRECTIVE_STATE;
	output_pending_newlines (ppout);
	return ELSE_DIRECTIVE;
}
^{MAYBE_AREA_A}[ ]*$[ \t]*"END"		|
^{MAYBE_AREA_A}[ ]*$[ \t]*"END-IF"	{
	/* MF extension: conditional compilation, second undocumented */
	BEGIN ENDIF_DIRECTIVE_STATE;
	output_pending_newlines (ppout);
	return ENDIF_DIRECTIVE;
}
^{MAYBE_AREA_A}[ ]*$[ \t]*"REGION"		|
^{MAYBE_AREA_A}[ ]*$[ \t]*"END-$REGION"	{
	/* MF extension for logical marking of a block in the editor */
	/* _possibly_ check: MF rule "$REGION and $IF statements must not overlap." */
	skip_to_eol ();
}

^{MAYBE_AREA_A}[ ]*$[ \t]*[_0-9A-Z-]+	{
	/* unknown MF style directive */
	char	*s;

	s = strchr (yytext, '$');
	cb_plex_warning (COBC_WARN_FILLER, newline_count,
			_("ignoring invalid directive: '%s'"), s);
	skip_to_eol ();
}

^{MAYBE_AREA_A}.{7}"@OPTIONS"[ ][A-Z0-9() ,;'"=]* {
	/* Fujitsu COBOL extension for specifying command line options */
	/* TODO: check position of the @OPTIONS directive */
	char *s = strchr (yytext, '@');
	cb_plex_warning (COBC_WARN_FILLER, newline_count - 1,
		_("ignoring unknown directive: '%s'"), s);
	skip_to_eol ();
}

^{MAYBE_AREA_A}[ ]*("PROCESS"|"CBL")[ ,;]*[\n] {
	/* IBM COBOL extension for specifying compiler options */
	/* TODO: The CBL (PROCESS) statement must be placed before any
	   comment lines, IDENTIFICATION DIVISION, or other
	   compiler-directing statements. */
	/* empty - so ignored */
	skip_to_eol ();
}

^{MAYBE_AREA_A}[ ]*("PROCESS"|"CBL")[ ][A-Z0-9() ,;'"=]* {
	/* IBM COBOL extension for specifying compiler options */
	/* TODO: The CBL (PROCESS) statement must be placed before any
	   comment lines, IDENTIFICATION DIVISION, or other
	   compiler-directing statements. */
	char *s = yytext;
	while (*s == ' ') s++;
	cb_plex_warning (COBC_WARN_FILLER, newline_count - 1,
		_("ignoring unknown directive: '%s'"), s);
	skip_to_eol ();
/*
}
   This test was deemed to produce more issues than it is useful,
   see bug #839 and others

^{MAYBE_AREA_A}[ ]*"$"		{
	cb_plex_warning (COBC_WARN_FILLER, newline_count,
		_("spurious '$' detected - ignored"));
	skip_to_eol ();
*/
}

%{/* Strip any Area A marker leading to COPY, INCLUDE, and REPLACE right now as
     at this stage they may not be seen on the line that directly follows the
     marker.  However, we still need to count the lines we are skipping this
     way. */%}
^{AREA_A}[ \n]*/"COPY"[ ,;\n]    { count_newlines (yytext); }
^{AREA_A}[ \n]*/"INCLUDE"[ ,;\n] { count_newlines (yytext); }
^{AREA_A}[ \n]*/"REPLACE"[ ,;\n] { count_newlines (yytext); }

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
	yy_push_state (REPLACE_STATE);
	return REPLACE;
}

^{MAYBE_AREA_A}.{6}[ ]*"*CONTROL" |
^{MAYBE_AREA_A}.{6}[ ]*"*CBL"	{
	BEGIN CONTROL_STATEMENT_STATE;
	output_pending_newlines (ppout);
	return CONTROL_STATEMENT;
}

<*>^{AREA_A}		{
	/* Output a special marker that precedes lines starting in area A.  This
	   is required to detect some missing periods.

	   See "(*area-a*)" in `ppinput' function below for the code that emits
	   the `#'.

	   The optional space before '#' in the definition of AREA_A is only
	   needed to properly handle the first line in each buffer, that for
	   some reason always starts with a space.
	*/
	fprintf (ppout, "#area_a\n");
}

"CONTROL"[ ,;\n]+"DIVISION" {
	/* Syntax extension for GCOS: such a division may include a SUBSTITUTION
	   SECTION that records source text replacement statements, along with a
	   DEFAULT SECTION where compile-time defaults are specified. */
	/* cf `ppparse.y`, grammar entry `program_with_control_division`, along
	   with `parser.y`, entry `_control_division`. */
	ppecho (yytext, NULL);
	yy_push_state (CONTROL_DIVISION_STATE);
	return CONTROL_DIVISION;
}

<CONTROL_DIVISION_STATE>{
  "SUBSTITUTION"[ ,;\n]+"SECTION" {
	  yy_push_state (SUBSTITUTION_SECTION_STATE);
	  return SUBSTITUTION_SECTION;
  }
  \. 			{
	  /* Pass dots to the parser to handle DEFAULT SECTION. */
	  ppecho (yytext, NULL);
	  return DOT;
  }
}

<SUBSTITUTION_SECTION_STATE,
CONTROL_DIVISION_STATE>{
  "REPLACE"	       {
	  yy_push_state (REPLACE_STATE);
	  return REPLACE;
  }
}
<SUBSTITUTION_SECTION_STATE>{
  \. 			{
	  /* Intercept dots within the SUBSTITUTION SECTION */
	  return DOT;
  }
}

<CONTROL_DIVISION_STATE,
SUBSTITUTION_SECTION_STATE>{
  "DEFAULT"[ ,;\n]+"SECTION" {
	  /* Pop any control division-related start condition state. */
	  while (YY_START == CONTROL_DIVISION_STATE ||
		 YY_START == SUBSTITUTION_SECTION_STATE)
		  yy_pop_state ();
	  ppecho (yytext, NULL);
  }
  [,;]?\n		{
	  ECHO;
	  check_listing (yytext, 0);
	  cb_source_line++;
  }
  [,;]?[ ]+		{ /* ignore */ }
}

<INITIAL,
CONTROL_DIVISION_STATE,
SUBSTITUTION_SECTION_STATE>
("ID"|"IDENTIFICATION")[ ,;\n]+"DIVISION" {
	/* Pop any control division-related start condition state. */
	while (YY_START == CONTROL_DIVISION_STATE ||
	       YY_START == SUBSTITUTION_SECTION_STATE)
		yy_pop_state ();
	/* Allow comment sentences/paragraphs */
	comment_allowed = 1;
	ppecho (yytext, NULL);
}

"PROGRAM-ID"/[ .,;\n]	{
	/* Allow comment sentences/paragraphs */
	comment_allowed = 1;
	ppecho (yytext, NULL);
}

"DIVISION"/[ .,;\n]	{
	/* Disallow comment sentences/paragraphs */
	comment_allowed = 0;
	ppecho (yytext, NULL);
}

"SECTION"/[ .,;\n]	{
	/* Disallow comment sentences/paragraphs */
	comment_allowed = 0;
	ppecho (yytext, NULL);
}

^{MAYBE_AREA_A}[ ]*"EJECT"([ ]*\.)? |
^{MAYBE_AREA_A}[ ]*"SKIP1"([ ]*\.)? |
^{MAYBE_AREA_A}[ ]*"SKIP2"([ ]*\.)? |
^{MAYBE_AREA_A}[ ]*"SKIP3"([ ]*\.)?	{
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

^{MAYBE_AREA_A}[ ]*"TITLE"[ ,;\n]	{
	/* This word can either be a listing-directive statement,
	   a reserved word, or a user-defined word...
	   some implementations (dis-)allow the (optional) "."
	   some start column 8+ some column 12+,
	   most limit the literal length (we cut in cobc.c)
	   We ignore the detailed rules and just do the parsing. */
	if (cb_verify (cb_title_statement, yytext)) {
		/* handle as listing-directive statement */
		BEGIN ALNUM_LITERAL_STATE;
		output_pending_newlines (ppout);
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
	ppecho (yytext, NULL);
}

[,;]?\n		{
	ppecho ("\n", NULL);
	cb_source_line++;
}

[;]?[ ]+	{
	ppecho (" ", yytext);
}

[,]?[ ]+	{
	if (inside_bracket) {
		ppecho (", ", NULL);
	} else {
		ppecho (" ", yytext);
	}
}

"("		{
	inside_bracket++;
	ppecho (yytext, NULL);
}

")"		{
	if (inside_bracket) {
		inside_bracket--;
	}
	ppecho (yytext, NULL);
}

{WORD} |
{NUMRIC_LITERAL} |
{ALNUM_LITERAL} |
.		{
	ppecho (yytext, NULL);
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
  "AREACHECK"	|
  "AREA-CHECK"	{
	return AREACHECK;
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
  "KEYCOMPRESS"	{
	return KEYCOMPRESS;
  }
  "NOKEYCOMPRESS"	{
	return NOKEYCOMPRESS;
  }
  "MAKESYN" 	|
  "MAKE-SYN"	{
	return MAKESYN;
  }
  "NOBOUND" |
  "NO-BOUND"		{
	return NOBOUND;
  }
  "NOAREACHECK"		|
  "NO-AREACHECK"	|
  "NO-AREA-CHECK"	{
	return NOAREACHECK;
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
  "NOSSRANGE"	|
  "NO-SSRANGE"	{
	return NOSSRANGE;
  }
  "NOSPZERO"	|
  "NO-SPZERO"	{
	return NOSPZERO;
  }
  "NOODOSLIDE"	|
  "NO-ODOSLIDE"	{
	return NOODOSLIDE;
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
  "XFD"		{
	return XFD;
  }
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
	/* note: if we get into the REPLACING state, then
	   that one handles the dot and pops this state */
	yy_pop_state ();
	return DOT;
  }
  /* for COPY IN/OF lib */
  "IN"			{ return IN; }
  "OF"			{ return OF; }

  "SUPPRESS"		{ return SUPPRESS; }
  "PRINTING"		{ return PRINTING; }
  "REPLACING"		{ yy_push_state (REPLACING_STATE); return REPLACING; }
  "ALSO"		{ return ALSO; }
  "LAST"		{ return LAST; }

  /* for error handling only, only used in REPLACING state */
  "LEADING"		{ return LEADING; }
  "TRAILING"		{ return TRAILING; }
  "=="			{ return EQEQ; }
  "BY"			{ return BY; }

  {WORD} |
  {NUMRIC_LITERAL} |
  {ALNUM_LITERAL} |
  .				{
	pplval.s = cobc_plex_strdup (yytext);
	return TOKEN;
  }
}

<REPLACING_STATE>{
  [,;]?\n		{
	ECHO;
	check_listing (yytext, 0);
	cb_source_line++;
  }
  [,;]?[ ]+		{ /* ignore */ }

  "LEADING"		{ return LEADING; }
  "TRAILING"		{ return TRAILING; }
  "=="			{ yy_push_state (PSEUDO_STATE); return EQEQ; }
  "BY"			{ return BY; }
  \. 			{
	  yy_pop_state ();
	  yy_pop_state ();
	  return DOT;
  }
  /* for qualification and subscripting */
  "IN"			{ return IN; }
  "OF"			{ return OF; }
  "("			{ return '('; }
  ")"			{ return ')'; }

  /* for error handling only, only used in COPY state */
  "REPLACING"		{ return REPLACING; }

  {WORD} |
  {NUMRIC_LITERAL} |
  {ALNUM_LITERAL} |
  .			{
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

<REPLACE_STATE>{
  [,;]?\n		{
	ECHO;
	check_listing (yytext, 0);
	cb_source_line++;
  }
  [,;]?[ ]+		{ /* ignore */ }
  \. 			{
	yy_pop_state ();
	return DOT;
  }
  /* for qualification and subscripting */
  "IN"			{ return IN; }
  "OF"			{ return OF; }
  "("			{ return '('; }
  ")"			{ return ')'; }

  "LEADING"		{ return LEADING; }
  "TRAILING"		{ return TRAILING; }
  "=="			{ yy_push_state (PSEUDO_STATE); return EQEQ; }
  "BY"			{ return BY; }
  "ALSO"		{ return ALSO; }
  "LAST"		{ return LAST; }
  "OFF"			{ return OFF; }

  {WORD} |
  {NUMRIC_LITERAL} |
  {ALNUM_LITERAL} |
  .				{
	pplval.s = cobc_plex_strdup (yytext);
	return TOKEN;
  }
}

<<EOF>> {
	struct copy_info *current_copy_info = copy_stack;

	yy_delete_buffer (YY_CURRENT_BUFFER);

	/* Terminate at the end of all input */
	if (current_copy_info->next == NULL) {
		output_pending_newlines (ppout);	/* CHECKME: do we want to drop those? */
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
		need_continuation = 0;
		buffer_overflow = 0;
		within_comment = 0;
		newline_count = 0;
		inside_bracket = 0;
		comment_allowed = 1;
		cb_free_replace ();
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
	cb_set_copy_replacing_list (current_copy_info->replacing);
	quotation_mark = current_copy_info->quotation_mark;
	cobc_set_source_format (current_copy_info->source_format);

	copy_stack = current_copy_info->next;
	cobc_free (current_copy_info->dname);
	cobc_free (current_copy_info);

	/* Check whether we are back at the toplevel source file. In this case,
	   check if there is a pending copy argument (--copy COPYBOOK) waiting
	   to be inserted. */
	if (copy_stack->next == NULL) {
		insert_copy_arg();
	}
}

%%

/* Global functions */

static int
is_fixed_indicator (char c){
	switch (c){ /* same indicators as in ppinput() */
	case ' ':
	case '-':
	case 'd':
	case 'D':
	case '*':
	case '/':
	case '\\':
	case '$':
		return 1;
	default:
		return 0;
	}
}

/* open file with the specified 'name', then check for BOM (utf-8 skipped, utf16/32 errored)
   and if in reference-format "auto" also for "likely free-format" */
static FILE *
ppopen_get_file (const char *name)
{
	struct copy_info	*current_copy_info;

	/* special case stdin;
	   note that we cannot handle BOM or deduce source format in this case
	   as rewind() clears the input buffer if used on stdin (and output in
	   console has normally no BOM at all), therefore compile from stdin
	   _has to_ specify free format if needed (or, more reasonable, use a CDF
	   directive to specify that) */
	if (strcmp (name, COB_DASH) == 0) {
		return stdin;
	}

	/* check for recursive inclusion */
	for (current_copy_info = copy_stack; current_copy_info; current_copy_info = current_copy_info->next) {
		/* FIXME: for WIN32 compare with cleaning / and \ (COPY "lib/file" vs COPY "lib\file"),
		          ideally open first, then check if we have the same physical file
		          (would also fix recursion check for symlinked files) */
		if (!strcmp (name, current_copy_info->dname)) {
		struct cb_tree_common	loc;
		for (current_copy_info = current_copy_info->next; current_copy_info; current_copy_info = current_copy_info->prev) {
			int		line;
			if (current_copy_info->prev) {
				line = current_copy_info->prev->line;
			} else {
				line = cb_source_line;
			}
			cb_inclusion_note (current_copy_info->dname, line);
		}
		loc.source_file = name;
		loc.source_line = -1;
		cb_error_x (&loc, _("recursive inclusion"));
		return 0;		
		}
	}

	/* try to open the file with the given name */
#ifdef	__OS400__
	ppin = fopen (name, "r");
#else
	ppin = fopen (name, "rb");
#endif
	if (!ppin) {
		cb_error ("%s: %s", name, cb_get_strerror ());
		/* Note: postpone error exit as we need the saved buffers later on */
		return 0;
	}

	/* Check for BOM and, if source-format was not specified, also for free-form */
	{
		unsigned char fseek_to = 0 ;
#define COBC_LOOKAHEAD 20
		unsigned char buffer[COBC_LOOKAHEAD];
		const int nread = fread (buffer, 1, COBC_LOOKAHEAD, ppin);

		/* check for and skip UTF-8 BOM */
		if (nread >= 3 && buffer[0] == 0xEF && buffer[1] == 0xBB && buffer[2] == 0xBF) {
			fseek_to = 3;
		} else
		/* check for binary */
		if (nread > 5) {
			const char *enc = NULL;
			if (buffer[0] == 0xFF && buffer[1] == 0xFE) {
				if (buffer[2] == 0x00 && buffer[3] == 0x00) {
					enc = "UTF-32LE";
				} else {
					enc = "UTF-16LE";
				}
			} else if (buffer[0] == 0xFE && buffer[1] == 0xFF) {
				enc = "UTF-16BE";
			} else if (buffer[0] == 0x00 && buffer[1] == 0x00
			        && buffer[2] == 0xFE && buffer[3] == 0xFF) {
				enc = "UTF-32BE";
			}
			if (enc) {
				/* unsupported encoding */
				fclose (ppin);
				ppin = NULL;
				cb_note (COB_WARNOPT_NONE, 0, _("try \"iconv -f %s -t UTF-8 '%s' -o 'out%c%s'"),
					enc, name, SLASH_CHAR, name);
				if (cb_source_line == 0) {
					/* if this is a "main" source, terminate with an error */
					cobc_terminate_exit (name, _("source file has unsupported encoding"));
				}
				cb_error ("%s: %s", name, _("source file has unsupported encoding"));
				return 0;
			}
#if 0
			if ((buffer[0] == 0x7F && buffer[1] == 0x45 && buffer[2] == 0x4C
			  && buffer[3] == 0x46 && (buffer[4] & 0xF0) == 0x00	/* ELF */)
			 || (buffer[0] == 0x4D && buffer[1] == 0x5A && buffer[3] == 0x00
				 && (buffer[4] & 0xF0) == 0x00	/* PE */)) {
				cb_error ("%s: %s", name, _("source file is binary"));
				close (ppin);
				return 0;
			}
#endif
			{
				int chck_pos = nread - 2;
				while (chck_pos > 0) {
					unsigned char test = buffer[chck_pos--];
					if (test != 0x00 && test != 0x80 && test != 0x90) {
						continue;
					}
					test = buffer[chck_pos--];
					if (test == 0x00 || test == 0x80 || test == 0x90) {
						/* two consecutive NULL / invalid ASCII/EBCDIC -> must be binary */
						fclose (ppin);
						ppin = NULL;
						if (cb_source_line == 0) {
							/* if this is a "main" source, terminate with an error */
							cobc_terminate_exit (name, _("source file is binary"));
						}
						cb_error ("%s: %s", name, _("source file is binary"));
						return 0;
					}
				}
			}
		}

		/* try to deduce source format */
		if (source_format == CB_FORMAT_AUTO) {
			unsigned char pos = fseek_to;
			/* if indicator is wrong on first line with source, switch to free format */
			/* skip empty lines */
			char last_pos_7 = ' ';
			unsigned char amount_of_0a_seen = 0;
			unsigned char line_pos = 0;
			while (nread - pos > 7) {
				switch (buffer[pos]) {
				case '\r':
					break;
				case '\n':
					amount_of_0a_seen++;
					line_pos = 0;
					break;
				case '\t':
					buffer[pos] = ' ';
					line_pos++;				/* move test from run_extensions.at 1809 to -fformat */
					while (line_pos % cb_tab_width != 0) {
						line_pos++;
					}
					break;
				default:
					line_pos++;
					break;
				}
				if (line_pos >= 7) {
					last_pos_7 = buffer[pos];
					break;
				}
				pos++;
			}
			/* check tab or indicator */
			if (!is_fixed_indicator (last_pos_7)) {
				struct cb_tree_common	loc;
				loc.source_file = name;
				loc.source_line = 1 + amount_of_0a_seen;
				loc.source_column = 7;
				cb_note_x (COB_WARNOPT_NONE, &loc, _("free format detected"));
				(void)cobc_deciph_source_format ("FREE");
			}
		}
		fseek (ppin, fseek_to, SEEK_SET);
	}

	return ppin;
}

/* open file (source or coypbook) for further processing */
int
ppopen (const char *name, struct cb_replace_list *replacing_list)
{
	struct copy_info	*current_copy_info;
	char			*dname;
	struct cb_replace_list *current_replace_list;

	if (ppin) {
		for (; newline_count > 0; newline_count--) {
			ungetc ('\n', ppin);
		}
	}

	/* open copy/source file, or use stdin */
	ppin = ppopen_get_file (name);

	/* note: detection of free format in ppopen_get_file above (not for stdin) */
	if (source_format == CB_FORMAT_AUTO) {
		cobc_set_source_format (CB_FORMAT_FIXED);
	}

	/* store listing information if requested */
	if (cb_current_file) {
		if (cb_current_file->source_format == CB_FORMAT_AUTO) {
			cb_current_file->source_format = cobc_get_source_format ();
		}
		/* this must be delayed until after format detection */
		cobc_set_listing_header_code ();
		/* Save name for listing */
		if (!cb_current_file->name) {
			cb_current_file->name = cobc_strdup (name);
		}
		cb_current_file->copy_line = cb_source_line;
	}

	/* add opened file to dependency list */
	if (cb_depend_file) {
		cb_depend_list = pp_text_list_add (cb_depend_list, name, strlen (name));
	}

	/* preserve the current buffer */
	current_replace_list = cb_get_copy_replacing_list();
	current_copy_info = cobc_malloc (sizeof (struct copy_info));
	current_copy_info->file = cb_source_file;
	current_copy_info->buffer = YY_CURRENT_BUFFER;

	/* save variables */
	current_copy_info->replacing = current_replace_list;
	current_copy_info->line = cb_source_line;
	current_copy_info->quotation_mark = quotation_mark;
	current_copy_info->source_format = cobc_get_source_format ();
	current_copy_info->indicator_error = indicator_error;

	current_copy_info->next = copy_stack;
	current_copy_info->containing_files = old_list_file;
	if (copy_stack) {
		copy_stack->prev = current_copy_info;
	}
	copy_stack = current_copy_info;

	/* set replacing list */
	if (replacing_list) {
		if (current_replace_list) {
			replacing_list->last->next = current_replace_list;
			replacing_list->last = current_replace_list->last;
		}
		cb_set_copy_replacing_list (replacing_list);
		if (cb_src_list_file) {
			cb_set_print_replace_list (replacing_list);
		}
	}

	dname = cobc_strdup (name);
	current_copy_info->dname = dname;
#if 0	/* Simon: better adjust the output where needed */
	{
		char *s = dname;
		while (*s) {
			if (*s == '\\') {
				*s = '/';
			}
			s++;
		}
	}
#endif

	/* switch to new buffer */
	switch_to_buffer (1, current_copy_info->dname, yy_create_buffer (ppin, YY_BUF_SIZE));
	indicator_error = 0;

	/* postponed errror handling */
	if (!ppin) {
		return -1;
	}
	return 0;
}

static const char *
ppcopy_try_open (const char *dir, const char *name, int has_ext)
{
	struct cb_text_list	*el = cb_extension_list;
	const char		*extension = "";
	struct stat st;

	if (!plexbuff2) {
		plexbuff2 = cobc_malloc ((size_t)COB_SMALL_BUFF);
	}

	for (;;) {
		if (dir) {
			snprintf (plexbuff2, (size_t)COB_SMALL_MAX, "%s%c%s%s",
				dir, SLASH_CHAR, name, extension);
		} else {
			snprintf (plexbuff2, (size_t)COB_SMALL_MAX, "%s%s",
				name, extension);
		}
		plexbuff2[COB_SMALL_MAX] = 0;
		if (access (plexbuff2, R_OK) == 0
		 && stat (plexbuff2, &st) == 0) {
			/* Must be a regular file */
			if (S_ISREG(st.st_mode)) 	/* Use POSIX macro */	
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
const char *
cb_copy_find_file (char *name, int has_ext)
{
	const char *filename;
	{
		const size_t len = strlen (name);
		size_t i;
#if defined (_WIN32) || defined (__DJGPP__)
		const char from = '/';
		const char to   = '\\';
#else
		const char from = '\\';
		const char to   = '/';
#endif
		for (i = 0; i < len; i++) {
			if (name[i] == from) {
				name[i] = to;
			}
		}
	}

	/* try name as is + extensions ... */
	filename = ppcopy_try_open (NULL, name, has_ext);
	if (filename) {
		return filename;
	}

	/* ... if not found, try in copybook directories  */
	if (*name != SLASH_CHAR
#if defined (_WIN32) || defined (__DJGPP__)
	 && *(name + 1) != ':'	/* note: the name is at least 2 bytes... */
#endif
		) {
		struct cb_text_list *il;
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

static COB_INLINE COB_A_INLINE void
output_pending_newlines (FILE *stream)
{
	if (echo_newline > 9) {
		/* too much newlines (likely becaue of conditional compilation or
		   long comment blocks, for example from EXEC SQL preparsers),
		   so generate source directive from the already adjusted static vars
		   instead of spitting out possibly hundreds of empty lines */
		fprintf (stream, "\n#line %d \"%s\"\n", cb_source_line, cb_source_file);
		echo_newline = 0;
	} else {
		while (echo_newline > 1) {
			fputc ('\n', stream);
			echo_newline--;
		}
		echo_newline = 0;
	}
}

int
ppcopy (const char *name, const char *lib, struct cb_replace_list *replace_list)
{
	const char		*filename = NULL;
	const int 		has_ext = (strchr (name, '.') != NULL);

	output_pending_newlines (yyout);

	if (cb_current_file) {
		cb_current_file->copy_line = cb_source_line;
	}

	if (!plexbuff1) {
		plexbuff1 = cobc_malloc ((size_t)COB_SMALL_BUFF);
	}

	/* TODO: open with path relative to the current file's path,
	         if any (applies both to with and without "lib") */

	/* Locate and open COPY file */
	if (lib) {
		const char *lib_env = NULL;

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
					filename = cb_copy_find_file (plexbuff1, has_ext);
				} else {
					strcpy (plexbuff1, name);
					filename = cb_copy_find_file (plexbuff1, has_ext);
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
			filename = cb_copy_find_file (plexbuff1, has_ext);
		}

		/* try without library name, if not resolved by env */
		if (!filename && !lib_env) {
			strcpy (plexbuff1, name);
			filename = cb_copy_find_file (plexbuff1, has_ext);
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
		strcpy (plexbuff1, name);
		filename = cb_copy_find_file (plexbuff1, has_ext);
	}

	/* expected case: filename found */
	if (filename) {
		if (ppopen (filename, replace_list) == 0) {
			/* expected case: copybook could be processed */
			return 0;
		}
		/* otherwise fall-trough to error handling */
	} else {
		/* ensure to have errno from name as specified, not from another file */
		(void)access (plexbuff1, R_OK);
		/* pass file error as we have no more places to check */
		cb_error ("%s: %s", plexbuff1, cb_get_strerror ());
	}

	/* On COPY, open error restore old file */
	cb_current_file = old_list_file;
	fprintf (yyout, "#line %d \"%s\"\n", cb_source_line, cb_source_file);
	return -1;
}

/* `newline_count` is one line ahead when the two functions below are called
   (from `ppparse.y`). */

unsigned int
ppparse_verify (const enum cb_support tag, const char *feature)
{
	return cb_plex_verify (newline_count-1, tag, feature);
}

void
ppparse_error (const char *err_msg)
{
	cb_plex_error (newline_count-1, "%s", err_msg);
}

int
cobc_has_areacheck_directive (const char * directive) {
	if (source_format != CB_FORMAT_FIXED &&
	    source_format != CB_FORMAT_FREE &&
	    source_format != CB_FORMAT_AUTO ) {
		return 1;
	} else {
		cb_plex_warning (COBC_WARN_FILLER, newline_count,
				 _("ignoring %s directive because of %s"),
				 directive, "-fformat=fixed/free");
		return 0;
	}
}

/* Sets `source_format`, `indicator_column`, `text_column`, `floating_area_b`,
   `fill_continued_alnums`, and `cobc_areacheck`, based on the given source
   format. */
void
cobc_set_source_format (const enum cb_format sf) {
	source_format = sf;

	switch (source_format) {
	case CB_FORMAT_XOPEN_FFF:
	case CB_FORMAT_ICOBOL_CRT:
	case CB_FORMAT_ACUTERM:
	case CB_FORMAT_COBOLX:
		indicator_column = 1;
		floating_area_b = source_format != CB_FORMAT_COBOLX;
		fill_continued_alnums = 0;
		break;
	default:
		indicator_column = 7;
		floating_area_b = 0;
		fill_continued_alnums = 1;
		break;
	}

	switch (source_format) {
	case CB_FORMAT_FIXED:
	case CB_FORMAT_AUTO:
	case CB_FORMAT_COBOL85:
		text_column = cb_config_text_column; /* 72 by default */
		break;
	case CB_FORMAT_VARIABLE:
		/* This value matches most MF Visual COBOL 4.0 version. */
		text_column = 250;
		break;
	case CB_FORMAT_ACUTERM:
	case CB_FORMAT_ICOBOL_CRT:
		/* CHECKME:
		   https://sf.net/p/gnucobol/feature-requests/29/#c2d0/8d2f */
		text_column = 320;
		break;
	case CB_FORMAT_XOPEN_FFF:
	case CB_FORMAT_COBOLX:
	case CB_FORMAT_ICOBOL_XCARD:
		/* For X/Open, this is the number of "bytes" allowed on single
		   source line as per the X/Open CAE Specification (1991),
		   Section 7.4 "Limits". */
		text_column = 255;
		break;
	case CB_FORMAT_FREE:
		/* text-column should be ignored: put an invalid value to catch
		   some bugs. */
		text_column = -1;
		break;
	}

	switch (source_format) {
	case CB_FORMAT_FIXED:
	case CB_FORMAT_FREE:
	case CB_FORMAT_AUTO:
	case CB_FORMAT_XOPEN_FFF:
		emit_area_a_tokens = 0;
		cobc_areacheck = 0;
		break;
	default:
		emit_area_a_tokens = 1;
		cobc_areacheck = cb_areacheck;
	}
}

enum cb_format cobc_get_source_format (void) { return source_format; }
int cobc_get_indicator_column (void) { return indicator_column; }
int cobc_get_text_column (void) { return text_column; }

/* The three functions below return indexes on the lines (starting from 0), not
   character positions (counted from 1). */
int cobc_get_indicator (void) {
	return indicator_column - 1;
}
int cobc_get_margin_a (const int indicator_width) {
	return indicator_column + indicator_width - 1;
}
int cobc_get_margin_b (const int indicator_width) {
	/* careful, for COBOL 2002 there is no margin B */
	return indicator_column + indicator_width + 3;
}

static int
cobc_parse_source_format (enum cb_format *const out, const char *const sfname) {
	enum cb_format format;
	if (!cb_strcasecmp (sfname, "FIXED")) {
		format = CB_FORMAT_FIXED;
	} else if (!cb_strcasecmp (sfname, "FREE")) {
		/* Note: SOURCEFORMAT"FREE" in MicroFocus dialect actually
		   selects an X/Open-derived free-form format. */
		format = (cb_std_define == CB_STD_MF)
			? CB_FORMAT_XOPEN_FFF
			: CB_FORMAT_FREE;
	} else if (!cb_strcasecmp (sfname, "COBOL85")) {
		format = CB_FORMAT_COBOL85;
	} else if (!cb_strcasecmp (sfname, "VARIABLE")) {
		format = CB_FORMAT_VARIABLE;
	} else if (!cb_strcasecmp (sfname, "XOPEN")) {
		format = CB_FORMAT_XOPEN_FFF;
	} else if (!cb_strcasecmp (sfname, "XCARD")) {
		format = CB_FORMAT_ICOBOL_XCARD;
	} else if (!cb_strcasecmp (sfname, "CRT")) {
		format = CB_FORMAT_ICOBOL_CRT;
	} else if (!cb_strcasecmp (sfname, "TERMINAL")) {
		format = CB_FORMAT_ACUTERM;
	} else if (!cb_strcasecmp (sfname, "COBOLX")) {
		format = CB_FORMAT_COBOLX;
	} else if (!cb_strcasecmp (sfname, "AUTO")) {
		format = CB_FORMAT_AUTO;
	} else {
		return 1;	/* invalid argument */
	}
	*out = format;
	return 0;
}

int cobc_deciph_source_format (const char *sfname) {
	enum cb_format format;
	if (cobc_parse_source_format (&format, sfname) == 0) {
		cobc_set_source_format (format);
		return 0;
	}
	return 1;
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
	echo_newline = 0;
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
		if (ci->dname) {
			cobc_free (ci->dname);
		}
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

void
cb_set_print_replace_list (struct cb_replace_list *list)
{
	struct cb_replace_list		*r;
	const struct cb_text_list	*l;
	struct list_replace		*repl;
	size_t				length;

	for (r = list; r; r = r->next) {
		const struct cb_replace_src *src = r->src;
		repl = cobc_malloc (sizeof (struct list_replace));
		repl->firstline = r->line_num;
		repl->lead_trail = src->lead_trail;
		repl->strict_partial = src->strict;
		repl->lastline = cb_source_line;

		for (l = src->text_list, length = 0; l; l = l->next) {
			length += strlen (l->text);
		}
		repl->from = cobc_malloc (length + 2);
		for (l = src->text_list; l; l = l->next) {
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
	output_pending_newlines (yyout);

	/* Reset file/line */
	cb_source_line = line;
	cb_source_file = file;
	fprintf (yyout, "#line %d \"%s\"\n", line, file);
	/* Switch buffer */
	yy_switch_to_buffer (buffer);
}

static COB_INLINE COB_A_INLINE int
is_cobol_word_char (const char c)
{
	return c == '-' || c == '_' || isalnum (c);
}

/* copies the next word in str to word_buff of size buff_len, and
returns the length of the copy. If the word is longer than buff_len,
returns 0. */
static int
get_word (const char *str, char *word_buff, size_t buff_len)
{
	int length = 0;
	if (str) {
		while (*str == ' ' || *str == ',' || *str == ';') {
			++str;
		}
		while (is_cobol_word_char(*str)){
			if (length++ == buff_len) return 0;
			*word_buff++ = (unsigned char)toupper((unsigned char)*str++);
		}
	}
	return length;
}

static int
is_condition_directive_clause (const char *buff)
{
	char p[12]; /* size of maximal comparison, here END-EVALUATE */
	int len;

	if (buff[0] == '$') {
		buff++;
	} else if (buff[0] == '>' && buff[1] == '>') {
		buff += 2;
	}

	len = get_word (buff, p, sizeof(p));
	switch (len){
	case 2:	return !memcmp (p, "IF", len);
	case 3: return !memcmp (p, "END", len);
	case 4: return     !memcmp (p, "ELSE", len)
			|| !memcmp (p, "ELIF", len)
			|| !memcmp (p, "WHEN", len);
	case 6: return  !memcmp (p, "END-IF", len);
	case 8: return  !memcmp (p, "EVALUATE", len);
	case 12: return !memcmp (p, "END-EVALUATE", len);
	}
	return 0;
}

static int
next_word_is_comment_paragraph_name (const char *buff)
{
	char p[13+1]; /* size of maximal comparison + 1, here
			 DATE-MODIFIED.  We need +1 because we may
			 have to add a terminating 0 to pass the
			 string to cb_plex_verify(). */
	int len;

	if (cb_comment_paragraphs == CB_IGNORE) return 0;

	len = get_word (buff, p, sizeof(p)-1);

	switch (len){
	case 6: if (memcmp (p, "AUTHOR", len)) return 0;
		break;
	case 7: if (memcmp (p, "REMARKS", len)) return 0;
		break;
	case 8: if (memcmp (p, "SECURITY", len)) return 0;
		break;
	case 12: if (memcmp (p, "DATE-WRITTEN", len)
		     && memcmp (p, "INSTALLATION", len)) return 0;
		break;
	case 13: if (memcmp (p, "DATE-MODIFIED", len)
		     && memcmp (p, "DATE-COMPILED", len)) return 0;
		break;
	default: return 0;
	}
	/* this part is reached only if a comparison above was successful */

	/* check so we get warnings / errors, but leave result unchanged */
	p[len] = 0;
	cb_plex_verify (newline_count, cb_comment_paragraphs, p);
	return 1;
}

static COB_INLINE COB_A_INLINE int
is_space_or_nl (const char c)
{
	return c == ' ' || c == '\n';
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
	char	*bp, qt, wrk[256];
	size_t	gotcr;
	size_t	line_overflow;
	size_t	continuation;
	size_t	indicator_width;
	int	ipchar;
	int	i,k;
	int	n;
	int	coln;
	int	in_area_a;
	struct list_skip *skip;

	/* check that we actually have input to process
	   (isn't the case if something went wrong beforehand) */
	/* LCOV_EXCL_START */
	if (ppin == NULL) {
		return YY_NULL; /* fake eof (no further processing of the not available file) */
	}
	/* LCOV_EXCL_STOP */

	/* Read line(s) */

	continuation = 0;
start:
	if (buffer_overflow ||
		     (newline_count + PPLEX_BUFF_LEN) >= max_size) {
		if (need_continuation || continuation) {
			cb_plex_error (newline_count,
					_("buffer overrun - too many continuation lines"));
#if 0		/* CHECKME: does anything breaks if we don't fake EOF here? */
			return YY_NULL; /* fake eof (no further processing) */
#endif
		}
		if (newline_count < max_size) {
			/* FIXME: this doesn't check the buffer size ! */
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
		if (n == PPLEX_BUFF_LEN) {
			if (line_overflow != 2) {
				line_overflow = 1;
			}
		}
		ipchar = getc (ppin);
		if (ipchar == EOF) {
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
			/* FIXME: this doesn't check the buffer size ! */
			memset (buff, '\n', newline_count);
			buff[newline_count] = 0;
			ipchar = (int)newline_count;
			newline_count = 0;
			return ipchar;
		}
#ifndef	COB_EBCDIC_MACHINE
		if (ipchar == 0x1A && !n) {
			continue;
		}
#endif
		if (gotcr) {
			gotcr = 0;
			if (ipchar != '\n') {
				if (line_overflow == 0) {
					buff[n++] = '\r';
				} else {
					line_overflow = 2;
				}
			}
		}
		if (ipchar == '\r') {
			gotcr = 1;
			continue;
		}
		if (ipchar == '\t') {
			if (line_overflow == 0) {
				buff[n++] = ' ';
				while (n % cb_tab_width != 0) {
					buff[n++] = ' ';
				}
				if (n > PPLEX_BUFF_LEN) {
					n = PPLEX_BUFF_LEN;
				}
			}
			continue;
		}
		if (line_overflow == 0) {
			buff[n++] = (char)ipchar;
		} else if (!is_space_or_nl ((char)ipchar)) {
			line_overflow = 2;
		}
	}

	if (buff[n - 1] != '\n') {
		/* FIXME: cb_source_line is one too low when CB_FORMAT_FREE is used
		   [but only during ppinput() in pplex.l ?]
		   Workaround for now: temporary newline_count + 1
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
		if (n <= indicator_column) {
			/* Line too short */
			newline_count++;
			goto start;
		}

		if (buff[0] == '*'
		 && cb_flag_mfcomment
		 && CB_MFCOMMENT_APPLIES (source_format)) {
			newline_count++;
			goto start;
		}

		/* Check if text is longer than text_column */
		if (n > text_column + 1) {
			/* Show warning if it is not whitespace
			   (postponed after checking for comments by setting
			    line_overflow to first column that leads to
			    "source text too long")
			*/
			if (get_warn_opt_value (cb_warn_column_overflow) && line_overflow == 0) {
				for (coln = text_column; coln < n; ++coln) {
					if (!is_space_or_nl (buff[coln])) {
						line_overflow = coln;
						break;
					}
				}
			} else {
				line_overflow = 0;
			}
			/* Remove it */
			buff[text_column] = '\n';
			buff[text_column + 1] = 0;
			n = text_column + 1;
		} else {
			line_overflow = 0;
		}

		memset (buff, ' ', (size_t)(indicator_column - 1));
		/* Note we allow directive lines to start in the indicator column */
		bp = &buff[indicator_column - 1];

		/* Special case: acucomment must be checked here as we'd pass comments
		   as directives otherwise */
		if (cb_flag_acucomment && buff[indicator_column - 1] == '$') {
			buff[indicator_column - 1] = '*';
		}
	}
	/*
	 * Convert the two forms of XFD into $SET XFD
	 * for internal processing  
	 */
	if (buff[6] == '$') {
		for(k=7; buff[k] == ' '; k++);
		if (strncasecmp(&buff[k],"XFD ",4) == 0) {
			for(k=k+3; buff[k] == ' '; k++);
			k = sprintf(wrk,"%s",&buff[k]);
			while(wrk[k-1] == '\n' || wrk[k-1] == '\r')
				wrk[--k] = 0;
			qt = strchr(wrk,'"') ? '\'' : '"';
			for (k=0; wrk[k] != 0; k++) {	/* Remove any 'QT' chars from string */
				if (wrk[k] == qt)
					wrk[k] = (qt == '"' ? '\'' : '"');
			}
			n = sprintf(buff,"      $SET XFD %c%s%c\n",qt,wrk,qt);
		}
	} else
	if (buff[6] == '*') {
		for(k=7; isspace(buff[k]); k++);
		if(memcmp(&buff[k],"((",2) == 0) {
			k = k + 2;
			while(isspace(buff[k])) k++;
			if (strncasecmp(&buff[k],"XFD ",4) == 0) {
				for(k=k+4; isspace(buff[k]); k++);
				k = sprintf(wrk,"%s",&buff[k]);
				while (wrk[k-1] == '\r'
					|| wrk[k-1] == '\n'
					|| isspace(wrk[k-1]))
					wrk[--k] = 0;
				if(memcmp(&wrk[k-2],"))",2) == 0) {
					k = k - 2;
					while (isspace(wrk[k-1])) wrk[--k] = 0;
					wrk[k] = 0;
					qt = strchr(wrk,'"') ? '\'' : '"';
					for (k=0; wrk[k] != 0; k++) {	/* Remove any 'QT' chars from string */
						if (wrk[k] == qt)
							wrk[k] = (qt == '"' ? '\'' : '"');
					}
					n = sprintf(buff,"      $SET XFD %c%s%c\n",qt,wrk,qt);
				}
			}
		}
	}

	/* Check for directives/floating comment at first non-space of line */
	ipchar = 0;
	for (; *bp; bp++) {
		if (*bp != ' ') {
			if ((*bp == '$' && bp[1] != ' ')
			 || (*bp == '>' && bp[1] == '>')) {
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
			/* FIXME: this doesn't check the buffer size ! */
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
	switch (buff[indicator_column - 1]) {
	case ' ':
		if (floating_area_b) { /* indicator is optional */
			indicator_width = 0;
		}
		break;
	case '-':
		if (source_format == CB_FORMAT_XOPEN_FFF) {
			/* X/Open free-form does not interpret '-' as
			   continuation indicator */
			indicator_width = 0;
			break;
		}
		if (within_comment) {
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
		} else if (source_format == CB_FORMAT_XOPEN_FFF) {
			if (buff[indicator_column - 1] == 'D' &&
			    buff[indicator_column] == ' ') {
				indicator_width = 2;
			} else {
				/* X/Open free-form: not a debugging line */
				indicator_width = 0;
				break;
			}
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
		if (buff[indicator_column] == 'D' &&
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
				       indicator_column);
		}
		newline_count++;
		goto start;
	default:
		if (floating_area_b) { /* indicator is optional */
			indicator_width = 0;
			break;
		}
		/* Invalid indicator */
		if (indicator_error++ < 5) {
			cb_plex_error (newline_count,
					_("invalid indicator '%c' at column %d"),
					buff[indicator_column - 1], indicator_column);
		}
		if (indicator_error == 5) {
			int sav_line = cb_source_line;
			cb_source_line = 0;
			cb_error (_("too many format errors in file, skip output of further errors"));
			cb_source_line = sav_line;
		}

		/* Note: Treat as comment line to allow further parsing
		         instead of aborting compilation */
		newline_count++;
		goto start;
	}

	/* Skip comments that follow after AUTHORS, etc. */
	if (within_comment) {
		/* Check all of "Area A" */
		const int margin_a = cobc_get_margin_a (indicator_width);
		const int margin_b = cobc_get_margin_b (indicator_width);
		const int ipchar_max = (n - 1 < margin_b) ? n - 1 : margin_b;
		for (ipchar = margin_a;
		     ipchar < ipchar_max && buff[ipchar] == ' ';
		     ++ipchar);
		if (ipchar == ipchar_max) {
			newline_count++;
			goto start;
		}
		within_comment = 0;
	}

	/* Skip blank lines */
	for (i = indicator_column; buff[i] == ' '; ++i);
	if (buff[i] == '\n') {
		newline_count++;
		goto start;
	}

	/* Substitute spaces for indicator, and check whether area A is blank */
	{
		const int margin_a = cobc_get_margin_a (indicator_width);
		const int margin_b = cobc_get_margin_b (indicator_width);
		for (i = indicator_column - 1; i < margin_a; i++) {
			buff[i] = ' ';
		}
		bp = buff + margin_a;

		in_area_a = 0;
		if (emit_area_a_tokens) {
			for (i = margin_a; i < margin_b; i++)
				in_area_a |= buff[i] != ' ';
		}
	}

	if (continuation) {
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
				if (indicator_error++ < 5) {
					cb_plex_error (newline_count,
						_("invalid line continuation"));
				}
				if (indicator_error == 5) {
					int sav_line = cb_source_line;
					cb_source_line = 0;
					cb_error (_("too many format errors in file, skip output of further errors"));
					cb_source_line = sav_line;
				}
				return YY_NULL;
			}
			quotation_mark = 0;
			consecutive_quotation = 0;
		} else if (quotation_mark) {
			/* Literal concatenation */
			if (*bp == quotation_mark) {
				bp++;
			} else {
				if (indicator_error++ < 5) {
					cb_plex_error (newline_count,
						_("invalid line continuation"));
				}
				if (indicator_error == 5) {
					int sav_line = cb_source_line;
					cb_source_line = 0;
					cb_error (_("too many format errors in file, skip output of further errors"));
					cb_source_line = sav_line;
				}
				return YY_NULL;
			}
		}
	} else {
		/* Normal line */
		if (need_continuation) {
			/* CHECKME: close the last literal */
			if (indicator_error++ < 5) {
				cb_plex_error (newline_count,
					_("continuation character expected"));
			}
			if (indicator_error == 5) {
				int sav_line = cb_source_line;
				cb_source_line = 0;
				cb_error (_("too many format errors in file, skip output of further errors"));
				cb_source_line = sav_line;
			}
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
			if (i < text_column) {
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
				if (i == text_column - 1) {
					/* Consecutive quotation */
					consecutive_quotation = 1;
				} else {
					/* Literal end */
					quotation_mark = 0;
				}
			}
		}
	}

	if (quotation_mark) {
		/* Expecting continuation */
		if (!consecutive_quotation) {
			need_continuation = 1;
		}
		if (fill_continued_alnums) {
			for (; i < text_column;) {
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

	/* Show warning if text is longer than text_column
	   and not whitespace (postponed here) */
	if (line_overflow != 0) {
		cb_plex_warning (cb_warn_source_after_code, newline_count,
				 _("source text after program-text area (column %d)"),
				text_column);
	}

	if (continuation) {
		gotcr = strlen (bp);
		memmove (buff, bp, gotcr + 1);
		newline_count++;
	} else {
		/* Insert newlines at the start of the buffer */
		gotcr = strlen (buff);
		if (in_area_a) {
			/* (*area-a*): prepend special marker `#', indicating
			   the current non-continuation line starts in area A
			   (see "AREA_A" and "MAYBE_AREA_A"... in lexer rules
			   above).

			   Assumes `buff' is large enough for one extra char. */
			memmove (buff + 1, buff, gotcr + 1);
			buff[0] = '#';
			gotcr += 1;
		}
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
ppecho (const char *text, const char *token)
{
	cb_ppecho_copy_replace (text, token);
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
count_newlines (const char *p)
{
	for (; *p; p++)
		if (*p == '\n')
			newline_count++;
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

void cb_ppecho_direct (const char *text, const char *token )
{
	if (text[0] == '\n' && text[1] == 0) {
		if (echo_newline == 0) {
			/* always keep one trailing \n */
			fputc ('\n', ppout);
		}
		echo_newline++;
	} else {
		output_pending_newlines (ppout);
		fputs (text, ppout);
	}
	if (cb_listing_file) {
		check_listing (token != NULL ? token : text, 0);
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
		fprintf (cb_listing_file, "%6d%c", ++listing_line, c);
	}

	if (requires_listing_line
	 && source_format != CB_FORMAT_FREE) {
		if (strlen (text) >= (size_t)indicator_column) {
			s = text + indicator_column - 1;
		} else if (strchr (text, '\n')) {
			s = "\n";	/* no sequence area, but line break */
		} else {
			s = "";	/* do not output sequence area */
		}
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

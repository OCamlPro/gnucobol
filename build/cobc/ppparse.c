/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1


/* Substitute the variable and function names.  */
#define yyparse         ppparse
#define yylex           pplex
#define yyerror         pperror
#define yydebug         ppdebug
#define yynerrs         ppnerrs
#define yylval          pplval
#define yychar          ppchar

/* First part of user prologue.  */
#line 34 "../../cobc/ppparse.y"

#include "config.h"

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#ifdef	HAVE_STRINGS_H
#include <strings.h>
#endif
#include <ctype.h>

#define	COB_IN_PPPARSE	1
#include "cobc.h"
#include "tree.h"

#ifndef	_STDLIB_H
#define	_STDLIB_H 1
#endif

#define pperror(x)	cb_error_always ("%s", x)

#define COND_EQ		0
#define COND_LT		1U
#define COND_GT		2U
#define COND_LE		3U
#define COND_GE		4U
#define COND_NE		5U

/* Global variables */

int				current_call_convention;

/* Local variables */

static struct cb_define_struct	*ppp_setvar_list = NULL;
static enum cb_directive_action		current_cmd = PLEX_ACT_IF;

/* Local functions */

/* Strips the given string from its quotation characters, if any.  Returns its
   argument as is otherwise. */
static char *
unquote (char *name)
{
	size_t size;
	if ((name[0] == '\'' || name[0] == '"') && (size = strlen (name)) > 1 &&
	    (name[0] == name[size - 1])) {
		name[size - 1] = '\0';
		name++;
	}
	return name;
}
#define fix_filename(filename) unquote (filename)

static int
literal_is_space_keyword (char *lit)
{
	return (strcmp ("SPACE",  lit) == 0
		 || strcmp ("SPACES", lit) == 0);
}

static char *
literal_token (char *t, int allow_spaces)
{
	if (t[0] == '\'' || t[0] == '"') {
		if (cb_partial_replace_when_literal_src != CB_SKIP)
			(void) ppparse_verify (cb_partial_replace_when_literal_src,
					       _("partial replacing with literal"));
	} else if (allow_spaces && literal_is_space_keyword (t)) {
		if (cb_partial_replace_when_literal_src != CB_SKIP)
			(void) ppparse_verify (cb_partial_replace_when_literal_src,
					       _("partial replacing with literal"));
		t[0] = '\0';
	} else {
		ppparse_error (_("unexpected COBOL word in partial replacement "
				 "phrase"));
	}
	return unquote (t);
}

static char *
fold_lower (char *name)
{
	unsigned char	*p;

	for (p = (unsigned char *)name; *p; p++) {
		*p = (cob_u8_t)tolower (*p);
	}
	return name;
}

static char *
fold_upper (char *name)
{
	unsigned char	*p;

	for (p = (unsigned char *)name; *p; p++) {
		*p = (cob_u8_t)toupper (*p);
	}
	return name;
}

static struct cb_replace_src *
ppp_replace_src (const struct cb_text_list * const text_list,
		 const unsigned int literal_src)
{
	const unsigned int allow_empty_replacement =
		!literal_src || cb_partial_replace_when_literal_src != CB_SKIP;
	struct cb_replace_src *s = cobc_plex_malloc (sizeof (struct cb_replace_src));
	/* Note the two next fields are re-assessed in ppp_replace_list_add below */
	s->lead_trail = CB_REPLACE_ALL;
	s->strict = allow_empty_replacement ? 0 : 1;
	s->text_list = text_list;
	return s;
}

static struct cb_replace_list *
ppp_replace_list_add (struct cb_replace_list *list,
		      struct cb_replace_src *src,
		      const struct cb_text_list *new_text,
		      const unsigned int lead_or_trail)
{
	struct cb_replace_list *p;

	p = cobc_plex_malloc (sizeof (struct cb_replace_list));
	p->line_num = cb_source_line;
	src->lead_trail = lead_or_trail;
	if (!lead_or_trail) {
		/* Strictness flag is irrelevant for non-LEADING nor TRAILING
		   replacements */
		src->strict = 0;
	} else {
		/* Use replacement text to decide strictness of partial match */
		const unsigned char *c;
		int has_space = new_text->next != NULL;
		for (c = (unsigned char *) new_text->text; !has_space && *c; c++) {
			has_space = isspace(*c);
		}
		if (has_space) {
			/* Note: as it appears, multi-word or spaces in
			   replacing is forbidden on GCOS. */
			ppparse_error (_("invalid partial replacing operand"));
			return NULL;
		}
		src->strict = src->strict && *new_text->text == '\0';
	}
	p->src = src;
	p->new_text = new_text;
	if (!list) {
		p->last = p;
		return p;
	}
	list->last->next = p;
	list->last = p;
	return list;
}

static unsigned int
ppp_set_value (struct cb_define_struct *p, const char *value)
{
	const char	*s;
	size_t		size;
	unsigned int	dotseen;

	p->value = NULL;
	p->sign = 0;
	p->int_part = 0;
	p->dec_part = 0;

	if (!value) {
		p->deftype = PLEX_DEF_NONE;
		return 0;
	}

	if (*value == '"' || *value == '\'') {
		s = value + 1;
		size = strlen (s) - 1;
		if (s[size] != *value) {
			p->deftype = PLEX_DEF_NONE;
			return 1;
		}
		p->deftype = PLEX_DEF_LIT;
		p->value = cobc_plex_strdup (s);
		p->value[size] = 0;
		return 0;
	}

	if (*value == '(') {
		/* actual MicroFocus Format for numeric values: (numlit) */
		s = value + 1;
		size = strlen (s) - 1;
		if (s[size] != ')') {
			p->deftype = PLEX_DEF_NONE;
			return 1;
		}
		p->deftype = PLEX_DEF_NUM;
		p->value = cobc_plex_strdup (s);
		p->value[size] = 0;
	} else {
		/* compatibility because this was supported since OpenCOBOL 2.0 */
		p->deftype = PLEX_DEF_NUM;
		p->value = cobc_plex_strdup (value);
	}

	p->sign = 0;
	s = p->value;
	if (*s == '+') {
		s++;
	} else if (*s == '-') {
		s++;
		p->sign = 1;
	}
	dotseen = 0;
	for ( ; *s; ++s) {
		if (*s == '.') {
			if (dotseen) {
				p->deftype = PLEX_DEF_NONE;
				return 1;
			}
			dotseen = 1;
			continue;
		}
		if (*s > '9' || *s < '0') {
			p->deftype = PLEX_DEF_NONE;
			return 1;
		}
		if (!dotseen) {
			p->int_part = (p->int_part * 10) + (*s - '0');
		} else {
			p->dec_part = (p->dec_part * 10) + (*s - '0');
		}
	}

	if (!p->int_part && !p->dec_part) {
		p->sign = 0;	/* zero is unsigned */
	}
	return 0;
}

static unsigned int
ppp_compare_vals (const struct cb_define_struct *p1,
		 const struct cb_define_struct *p2,
		 const unsigned int cond)
{
	int	result;

	if (!p1 || !p2) {
		return 0;
	}
	if (p1->deftype != PLEX_DEF_LIT && p1->deftype != PLEX_DEF_NUM) {
		return 0;
	}
	if (p2->deftype != PLEX_DEF_LIT && p2->deftype != PLEX_DEF_NUM) {
		return 0;
	}
	if (p1->deftype != p2->deftype) {
		cb_warning (COBC_WARN_FILLER, _("directive comparison on different types"));
		return 0;
	}
	if (p1->deftype == PLEX_DEF_LIT) {
		result = strcmp (p1->value, p2->value);
	} else {
		if (p1->sign && !p2->sign) {
			result = -1;
		} else if (!p1->sign && p2->sign) {
			result = 1;
		} else if (p1->int_part < p2->int_part) {
			if (p1->sign) {
				result = 1;
			} else {
				result = -1;
			}
		} else if (p1->int_part > p2->int_part) {
			if (p1->sign) {
				result = -1;
			} else {
				result = 1;
			}
		} else if (p1->dec_part < p2->dec_part) {
			if (p1->sign) {
				result = 1;
			} else {
				result = -1;
			}
		} else if (p1->dec_part > p2->dec_part) {
			if (p1->sign) {
				result = -1;
			} else {
				result = 1;
			}
		} else {
			result = 0;
		}
	}
	switch (cond) {
	case COND_EQ:
		return (result == 0);
	case COND_LT:
		return (result < 0);
	case COND_GT:
		return (result > 0);
	case COND_LE:
		return (result <= 0);
	case COND_GE:
		return (result >= 0);
	case COND_NE:
		return (result != 0);
	default:
		break;
	}
	return 0;
}

static struct cb_define_struct *
ppp_define_add (struct cb_define_struct *list, const char *name,
		const char *text, const unsigned int override)
{
	struct cb_define_struct	*p;
	struct cb_define_struct	*l;

	/* Check duplicate */
	for (l = list; l; l = l->next) {
		if (!strcasecmp (name, l->name)) {
			if (!override && l->deftype != PLEX_DEF_DEL) {
				cb_error (_("duplicate DEFINE directive '%s'"), name);
				return NULL;
			}
			if (l->value) {
				l->value = NULL;
			}
			if (ppp_set_value (l, text)) {
				cb_error (_("invalid constant %s in DEFINE directive"), text);
				return NULL;
			}
			return list;
		}
	}

	p = cobc_plex_malloc (sizeof (struct cb_define_struct));
	p->name = cobc_plex_strdup (name);
	if (ppp_set_value (p, text)) {
		cb_error (_ ("invalid constant %s in DEFINE directive"), text);
		return NULL;
	}

	if (!list) {
		p->last = p;
		return p;
	}
	list->last->next = p;
	list->last = p;
	return list;
}

static void
ppp_define_del (const char *name)
{
	struct cb_define_struct	*l;

	for (l = ppp_setvar_list; l; l = l->next) {
		if (!strcmp (name, l->name)) {
			l->deftype = PLEX_DEF_DEL;
			if (l->value) {
				l->value = NULL;
			}
			l->sign = 0;
			l->int_part = 0;
			l->dec_part = 0;
			break;
		}
	}
}

void
ppp_clear_lists (void)
{
	ppp_setvar_list = NULL;
}

struct cb_define_struct *
ppp_search_lists (const char *name)
{
	struct cb_define_struct	*p;

	for (p = ppp_setvar_list; p; p = p->next) {
		if (p->name == NULL) {
			continue;
		}
		if (!strcasecmp (name, p->name)) {
			if (p->deftype != PLEX_DEF_DEL) {
				return p;
			}
			break;
		}
	}
	return NULL;
}

static struct cb_text_list *
ppp_list_add (struct cb_text_list *list, const char *text)
{
	struct cb_text_list	*p;

	p = cobc_plex_malloc (sizeof (struct cb_text_list));
	p->text = cobc_plex_strdup (text);
	if (!list) {
		p->last = p;
		return p;
	}
	list->last->next = p;
	list->last = p;
	return list;
}

static struct cb_text_list *
ppp_list_append (struct cb_text_list *list_1, struct cb_text_list *list_2)
{
	struct cb_text_list	*list_1_end;

	if (!list_1) {
		return list_2;
	}

	for (list_1_end = list_1;
	     list_1_end->next;
	     list_1_end = list_1_end->next);
	list_1_end->next = list_2;
	list_2->last = list_1_end;

	return list_1;
}

static unsigned int
ppp_search_comp_vars (const char *name)
{
#undef	CB_PARSE_DEF
#define	CB_PARSE_DEF(x,z)	if (!cb_strcasecmp (name, x)) return (z);
#include "ppparse.def"
#undef	CB_PARSE_DEF
	cb_warning (COBC_WARN_FILLER, _("compiler flag '%s' unknown"), name);
	return 0;
}

static unsigned int
ppp_check_needs_quote (const char *envval)
{
	const char	*s;
	size_t		size;
	unsigned int	dot_seen;
	unsigned int	sign_seen;

	/* Non-quoted value - Check if possible numeric */
	dot_seen = 0;
	sign_seen = 0;
	size = 0;
	s = envval;
	if (*s == '+' || *s == '-') {
		sign_seen = 1;
		size++;
		s++;
	}
	for (; *s; ++s) {
		if (*s == '.') {
			if (dot_seen) {
				break;
			}
			dot_seen = 1;
			size++;
			continue;
		}
		if (*s > '9' || *s < '0') {
			break;
		}
		size++;
	}

	if (*s || size <= ((size_t)dot_seen + sign_seen)) {
		return 1;
	}
	return 0;
}

static void
ppp_error_invalid_option (const char *directive, const char *option)
{
	if (option) {
		cb_error (_("invalid %s directive option '%s'"), directive, option);
	} else {
		cb_error (_("invalid %s directive option"), directive);
	}
}

static void
append_to_turn_list (struct cb_text_list *ec_names, int enable, int with_location)
{
	struct cb_turn_list	*l;
	struct cb_turn_list	*turn_list_end;

	/* Add turn directive data to end of cb_turn_list */
	l = cobc_plex_malloc (sizeof (struct cb_turn_list));
	l->ec_names = ec_names;
	l->enable = enable;
	l->with_location = with_location;
	l->next = NULL;
	/* The line number is set properly in the scanner */
	l->line = -1;

	if (cb_turn_list) {
		for (turn_list_end = cb_turn_list;
		     turn_list_end->next;
		     turn_list_end = turn_list_end->next);
		turn_list_end->next = l;
	} else {
		cb_turn_list = l;
	}

	/*
	  Output #TURN so we can assign a line number to this data later in the
	  scanner.
	*/
	fprintf (ppout, "#TURN\n");
}

/* Global functions */

void
ppparse_clear_vars (const struct cb_define_struct *p)
{
	const struct cb_define_struct	*q;

	ppp_setvar_list = NULL;
	/* Set standard DEFINE's */
	if (cb_perform_osvs) {
		ppp_setvar_list = ppp_define_add (ppp_setvar_list,
						  "PERFORM-TYPE",
						  "'OSVS'", 0);
	} else {
		ppp_setvar_list = ppp_define_add (ppp_setvar_list,
						  "PERFORM-TYPE",
						  "'MF'", 0);
	}
	if (cb_ebcdic_sign) {
		ppp_setvar_list = ppp_define_add (ppp_setvar_list,
						  "SIGN",
						  "'EBCDIC'", 0);
	} else {
		ppp_setvar_list = ppp_define_add (ppp_setvar_list,
						  "SIGN",
						  "'ASCII'", 0);
	}
#ifdef	WORDS_BIGENDIAN
	ppp_setvar_list = ppp_define_add (ppp_setvar_list,
					  "ENDIAN",
					  "'BIG'", 0);
#else
	ppp_setvar_list = ppp_define_add (ppp_setvar_list,
					  "ENDIAN",
					  "'LITTLE'", 0);
#endif
#if	' ' == 0x20
	ppp_setvar_list = ppp_define_add (ppp_setvar_list,
					  "CHARSET",
					  "'ASCII'", 0);
#elif	' ' == 0x40
	ppp_setvar_list = ppp_define_add (ppp_setvar_list,
					  "CHARSET",
					  "'EBCDIC'", 0);
#else
	ppp_setvar_list = ppp_define_add (ppp_setvar_list,
					  "CHARSET",
					  "'UNKNOWN'", 0);
#endif
	/* Set DEFINE's from '-D' option(s) */
	for (q = p; q; q = q->next) {
		ppp_setvar_list = ppp_define_add (ppp_setvar_list,
						  q->name,
						  q->value, 0);
	}
	/* reset CALL CONVENTION */
	current_call_convention = CB_CONV_COBOL;
}


#line 663 "ppparse.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

/* Use api.header.include to #include this header
   instead of duplicating it here.  */
#ifndef YY_PP_PPPARSE_H_INCLUDED
# define YY_PP_PPPARSE_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int ppdebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    TOKEN_EOF = 0,                 /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    ALSO = 258,                    /* ALSO  */
    BY = 259,                      /* BY  */
    COPY = 260,                    /* COPY  */
    EQEQ = 261,                    /* "=="  */
    IN = 262,                      /* IN  */
    LAST = 263,                    /* LAST  */
    LEADING = 264,                 /* LEADING  */
    OF = 265,                      /* OF  */
    OFF = 266,                     /* OFF  */
    PRINTING = 267,                /* PRINTING  */
    REPLACE = 268,                 /* REPLACE  */
    REPLACING = 269,               /* REPLACING  */
    SUPPRESS = 270,                /* SUPPRESS  */
    TRAILING = 271,                /* TRAILING  */
    DOT = 272,                     /* "."  */
    GARBAGE = 273,                 /* "word"  */
    LISTING_DIRECTIVE = 274,       /* LISTING_DIRECTIVE  */
    LISTING_STATEMENT = 275,       /* LISTING_STATEMENT  */
    TITLE_STATEMENT = 276,         /* TITLE_STATEMENT  */
    COBOL_WORDS_DIRECTIVE = 277,   /* COBOL_WORDS_DIRECTIVE  */
    EQUATE = 278,                  /* EQUATE  */
    UNDEFINE = 279,                /* UNDEFINE  */
    SUBSTITUTE = 280,              /* SUBSTITUTE  */
    RESERVE = 281,                 /* RESERVE  */
    CONTROL_STATEMENT = 282,       /* CONTROL_STATEMENT  */
    SOURCE = 283,                  /* SOURCE  */
    NOSOURCE = 284,                /* NOSOURCE  */
    LIST = 285,                    /* LIST  */
    NOLIST = 286,                  /* NOLIST  */
    MAP = 287,                     /* MAP  */
    NOMAP = 288,                   /* NOMAP  */
    LEAP_SECOND_DIRECTIVE = 289,   /* LEAP_SECOND_DIRECTIVE  */
    CONTROL_DIVISION = 290,        /* "CONTROL DIVISION"  */
    SUBSTITUTION_SECTION = 291,    /* "SUBSTITUTION SECTION"  */
    SOURCE_DIRECTIVE = 292,        /* SOURCE_DIRECTIVE  */
    FORMAT = 293,                  /* FORMAT  */
    IS = 294,                      /* IS  */
    CALL_DIRECTIVE = 295,          /* CALL_DIRECTIVE  */
    COBOL = 296,                   /* COBOL  */
    TOK_EXTERN = 297,              /* "EXTERN"  */
    STDCALL = 298,                 /* STDCALL  */
    STATIC = 299,                  /* STATIC  */
    DEFINE_DIRECTIVE = 300,        /* DEFINE_DIRECTIVE  */
    AS = 301,                      /* AS  */
    PARAMETER = 302,               /* PARAMETER  */
    OVERRIDE = 303,                /* OVERRIDE  */
    REFMOD_DIRECTIVE = 304,        /* REFMOD_DIRECTIVE  */
    SET_DIRECTIVE = 305,           /* SET_DIRECTIVE  */
    ADDRSV = 306,                  /* ADDRSV  */
    ADDSYN = 307,                  /* ADDSYN  */
    AREACHECK = 308,               /* AREACHECK  */
    NOAREACHECK = 309,             /* NOAREACHECK  */
    ASSIGN = 310,                  /* ASSIGN  */
    BOUND = 311,                   /* BOUND  */
    CALLFH = 312,                  /* CALLFH  */
    CHECKNUM = 313,                /* CHECKNUM  */
    COMP1 = 314,                   /* COMP1  */
    CONSTANT = 315,                /* CONSTANT  */
    DPC_IN_DATA = 316,             /* "DPC-IN-DATA"  */
    FOLDCOPYNAME = 317,            /* FOLDCOPYNAME  */
    MAKESYN = 318,                 /* MAKESYN  */
    NOBOUND = 319,                 /* NOBOUND  */
    NOCHECKNUM = 320,              /* NOCHECKNUM  */
    NODPC_IN_DATA = 321,           /* "NODPC-IN-DATA"  */
    NOFOLDCOPYNAME = 322,          /* NOFOLDCOPYNAME  */
    NOODOSLIDE = 323,              /* NOODOSLIDE  */
    NOSPZERO = 324,                /* NOSPZERO  */
    NOSSRANGE = 325,               /* NOSSRANGE  */
    ODOSLIDE = 326,                /* ODOSLIDE  */
    REMOVE = 327,                  /* REMOVE  */
    SOURCEFORMAT = 328,            /* SOURCEFORMAT  */
    SPZERO = 329,                  /* SPZERO  */
    SSRANGE = 330,                 /* SSRANGE  */
    IF_DIRECTIVE = 331,            /* IF_DIRECTIVE  */
    ELSE_DIRECTIVE = 332,          /* ELSE_DIRECTIVE  */
    ENDIF_DIRECTIVE = 333,         /* ENDIF_DIRECTIVE  */
    ELIF_DIRECTIVE = 334,          /* ELIF_DIRECTIVE  */
    GE = 335,                      /* ">="  */
    LE = 336,                      /* "<="  */
    LT = 337,                      /* "<"  */
    GT = 338,                      /* ">"  */
    EQ = 339,                      /* "="  */
    NE = 340,                      /* "<>"  */
    NOT = 341,                     /* NOT  */
    THAN = 342,                    /* THAN  */
    TO = 343,                      /* TO  */
    OR = 344,                      /* OR  */
    EQUAL = 345,                   /* EQUAL  */
    GREATER = 346,                 /* GREATER  */
    LESS = 347,                    /* LESS  */
    SET = 348,                     /* SET  */
    DEFINED = 349,                 /* DEFINED  */
    TURN_DIRECTIVE = 350,          /* TURN_DIRECTIVE  */
    ON = 351,                      /* ON  */
    CHECKING = 352,                /* CHECKING  */
    WITH = 353,                    /* WITH  */
    LOCATION = 354,                /* LOCATION  */
    TERMINATOR = 355,              /* "end of line"  */
    TOKEN = 356,                   /* "Word or Literal"  */
    TEXT_NAME = 357,               /* "Text-Name"  */
    VARIABLE_NAME = 358,           /* "Variable"  */
    LITERAL = 359                  /* "Literal"  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 619 "../../cobc/ppparse.y"

	char			*s;
	struct cb_text_list	*l;
	struct cb_replace_src	*p;
	struct cb_replace_list	*r;
	struct cb_define_struct	*ds;
	unsigned int		ui;
	int			si;

#line 827 "ppparse.c"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE pplval;


int ppparse (void);


#endif /* !YY_PP_PPPARSE_H_INCLUDED  */
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_ALSO = 3,                       /* ALSO  */
  YYSYMBOL_BY = 4,                         /* BY  */
  YYSYMBOL_COPY = 5,                       /* COPY  */
  YYSYMBOL_EQEQ = 6,                       /* "=="  */
  YYSYMBOL_IN = 7,                         /* IN  */
  YYSYMBOL_LAST = 8,                       /* LAST  */
  YYSYMBOL_LEADING = 9,                    /* LEADING  */
  YYSYMBOL_OF = 10,                        /* OF  */
  YYSYMBOL_OFF = 11,                       /* OFF  */
  YYSYMBOL_PRINTING = 12,                  /* PRINTING  */
  YYSYMBOL_REPLACE = 13,                   /* REPLACE  */
  YYSYMBOL_REPLACING = 14,                 /* REPLACING  */
  YYSYMBOL_SUPPRESS = 15,                  /* SUPPRESS  */
  YYSYMBOL_TRAILING = 16,                  /* TRAILING  */
  YYSYMBOL_DOT = 17,                       /* "."  */
  YYSYMBOL_GARBAGE = 18,                   /* "word"  */
  YYSYMBOL_LISTING_DIRECTIVE = 19,         /* LISTING_DIRECTIVE  */
  YYSYMBOL_LISTING_STATEMENT = 20,         /* LISTING_STATEMENT  */
  YYSYMBOL_TITLE_STATEMENT = 21,           /* TITLE_STATEMENT  */
  YYSYMBOL_COBOL_WORDS_DIRECTIVE = 22,     /* COBOL_WORDS_DIRECTIVE  */
  YYSYMBOL_EQUATE = 23,                    /* EQUATE  */
  YYSYMBOL_UNDEFINE = 24,                  /* UNDEFINE  */
  YYSYMBOL_SUBSTITUTE = 25,                /* SUBSTITUTE  */
  YYSYMBOL_RESERVE = 26,                   /* RESERVE  */
  YYSYMBOL_CONTROL_STATEMENT = 27,         /* CONTROL_STATEMENT  */
  YYSYMBOL_SOURCE = 28,                    /* SOURCE  */
  YYSYMBOL_NOSOURCE = 29,                  /* NOSOURCE  */
  YYSYMBOL_LIST = 30,                      /* LIST  */
  YYSYMBOL_NOLIST = 31,                    /* NOLIST  */
  YYSYMBOL_MAP = 32,                       /* MAP  */
  YYSYMBOL_NOMAP = 33,                     /* NOMAP  */
  YYSYMBOL_LEAP_SECOND_DIRECTIVE = 34,     /* LEAP_SECOND_DIRECTIVE  */
  YYSYMBOL_CONTROL_DIVISION = 35,          /* "CONTROL DIVISION"  */
  YYSYMBOL_SUBSTITUTION_SECTION = 36,      /* "SUBSTITUTION SECTION"  */
  YYSYMBOL_SOURCE_DIRECTIVE = 37,          /* SOURCE_DIRECTIVE  */
  YYSYMBOL_FORMAT = 38,                    /* FORMAT  */
  YYSYMBOL_IS = 39,                        /* IS  */
  YYSYMBOL_CALL_DIRECTIVE = 40,            /* CALL_DIRECTIVE  */
  YYSYMBOL_COBOL = 41,                     /* COBOL  */
  YYSYMBOL_TOK_EXTERN = 42,                /* "EXTERN"  */
  YYSYMBOL_STDCALL = 43,                   /* STDCALL  */
  YYSYMBOL_STATIC = 44,                    /* STATIC  */
  YYSYMBOL_DEFINE_DIRECTIVE = 45,          /* DEFINE_DIRECTIVE  */
  YYSYMBOL_AS = 46,                        /* AS  */
  YYSYMBOL_PARAMETER = 47,                 /* PARAMETER  */
  YYSYMBOL_OVERRIDE = 48,                  /* OVERRIDE  */
  YYSYMBOL_REFMOD_DIRECTIVE = 49,          /* REFMOD_DIRECTIVE  */
  YYSYMBOL_SET_DIRECTIVE = 50,             /* SET_DIRECTIVE  */
  YYSYMBOL_ADDRSV = 51,                    /* ADDRSV  */
  YYSYMBOL_ADDSYN = 52,                    /* ADDSYN  */
  YYSYMBOL_AREACHECK = 53,                 /* AREACHECK  */
  YYSYMBOL_NOAREACHECK = 54,               /* NOAREACHECK  */
  YYSYMBOL_ASSIGN = 55,                    /* ASSIGN  */
  YYSYMBOL_BOUND = 56,                     /* BOUND  */
  YYSYMBOL_CALLFH = 57,                    /* CALLFH  */
  YYSYMBOL_CHECKNUM = 58,                  /* CHECKNUM  */
  YYSYMBOL_COMP1 = 59,                     /* COMP1  */
  YYSYMBOL_CONSTANT = 60,                  /* CONSTANT  */
  YYSYMBOL_DPC_IN_DATA = 61,               /* "DPC-IN-DATA"  */
  YYSYMBOL_FOLDCOPYNAME = 62,              /* FOLDCOPYNAME  */
  YYSYMBOL_MAKESYN = 63,                   /* MAKESYN  */
  YYSYMBOL_NOBOUND = 64,                   /* NOBOUND  */
  YYSYMBOL_NOCHECKNUM = 65,                /* NOCHECKNUM  */
  YYSYMBOL_NODPC_IN_DATA = 66,             /* "NODPC-IN-DATA"  */
  YYSYMBOL_NOFOLDCOPYNAME = 67,            /* NOFOLDCOPYNAME  */
  YYSYMBOL_NOODOSLIDE = 68,                /* NOODOSLIDE  */
  YYSYMBOL_NOSPZERO = 69,                  /* NOSPZERO  */
  YYSYMBOL_NOSSRANGE = 70,                 /* NOSSRANGE  */
  YYSYMBOL_ODOSLIDE = 71,                  /* ODOSLIDE  */
  YYSYMBOL_REMOVE = 72,                    /* REMOVE  */
  YYSYMBOL_SOURCEFORMAT = 73,              /* SOURCEFORMAT  */
  YYSYMBOL_SPZERO = 74,                    /* SPZERO  */
  YYSYMBOL_SSRANGE = 75,                   /* SSRANGE  */
  YYSYMBOL_IF_DIRECTIVE = 76,              /* IF_DIRECTIVE  */
  YYSYMBOL_ELSE_DIRECTIVE = 77,            /* ELSE_DIRECTIVE  */
  YYSYMBOL_ENDIF_DIRECTIVE = 78,           /* ENDIF_DIRECTIVE  */
  YYSYMBOL_ELIF_DIRECTIVE = 79,            /* ELIF_DIRECTIVE  */
  YYSYMBOL_GE = 80,                        /* ">="  */
  YYSYMBOL_LE = 81,                        /* "<="  */
  YYSYMBOL_LT = 82,                        /* "<"  */
  YYSYMBOL_GT = 83,                        /* ">"  */
  YYSYMBOL_EQ = 84,                        /* "="  */
  YYSYMBOL_NE = 85,                        /* "<>"  */
  YYSYMBOL_NOT = 86,                       /* NOT  */
  YYSYMBOL_THAN = 87,                      /* THAN  */
  YYSYMBOL_TO = 88,                        /* TO  */
  YYSYMBOL_OR = 89,                        /* OR  */
  YYSYMBOL_EQUAL = 90,                     /* EQUAL  */
  YYSYMBOL_GREATER = 91,                   /* GREATER  */
  YYSYMBOL_LESS = 92,                      /* LESS  */
  YYSYMBOL_SET = 93,                       /* SET  */
  YYSYMBOL_DEFINED = 94,                   /* DEFINED  */
  YYSYMBOL_TURN_DIRECTIVE = 95,            /* TURN_DIRECTIVE  */
  YYSYMBOL_ON = 96,                        /* ON  */
  YYSYMBOL_CHECKING = 97,                  /* CHECKING  */
  YYSYMBOL_WITH = 98,                      /* WITH  */
  YYSYMBOL_LOCATION = 99,                  /* LOCATION  */
  YYSYMBOL_TERMINATOR = 100,               /* "end of line"  */
  YYSYMBOL_TOKEN = 101,                    /* "Word or Literal"  */
  YYSYMBOL_TEXT_NAME = 102,                /* "Text-Name"  */
  YYSYMBOL_VARIABLE_NAME = 103,            /* "Variable"  */
  YYSYMBOL_LITERAL = 104,                  /* "Literal"  */
  YYSYMBOL_105_ = 105,                     /* '('  */
  YYSYMBOL_106_ = 106,                     /* ')'  */
  YYSYMBOL_YYACCEPT = 107,                 /* $accept  */
  YYSYMBOL_program_structure = 108,        /* program_structure  */
  YYSYMBOL_program_with_control_division = 109, /* program_with_control_division  */
  YYSYMBOL_control_division_no_replace = 110, /* control_division_no_replace  */
  YYSYMBOL_control_division_with_replace = 111, /* control_division_with_replace  */
  YYSYMBOL_statement_list = 112,           /* statement_list  */
  YYSYMBOL_statement_no_replace_list = 113, /* statement_no_replace_list  */
  YYSYMBOL_statement = 114,                /* statement  */
  YYSYMBOL_statement_no_replace = 115,     /* statement_no_replace  */
  YYSYMBOL_directive = 116,                /* directive  */
  YYSYMBOL_117_1 = 117,                    /* $@1  */
  YYSYMBOL_118_2 = 118,                    /* $@2  */
  YYSYMBOL_119_3 = 119,                    /* $@3  */
  YYSYMBOL_if_directive_if = 120,          /* if_directive_if  */
  YYSYMBOL_if_directive_elif = 121,        /* if_directive_elif  */
  YYSYMBOL_set_directive = 122,            /* set_directive  */
  YYSYMBOL_set_choice = 123,               /* set_choice  */
  YYSYMBOL_alnum_list = 124,               /* alnum_list  */
  YYSYMBOL_alnum_equality_list = 125,      /* alnum_equality_list  */
  YYSYMBOL_alnum_equality = 126,           /* alnum_equality  */
  YYSYMBOL_alnum_with_list = 127,          /* alnum_with_list  */
  YYSYMBOL_alnum_with = 128,               /* alnum_with  */
  YYSYMBOL_alnum_by_list = 129,            /* alnum_by_list  */
  YYSYMBOL_alnum_by = 130,                 /* alnum_by  */
  YYSYMBOL_set_options = 131,              /* set_options  */
  YYSYMBOL_refmod_directive = 132,         /* refmod_directive  */
  YYSYMBOL_source_directive = 133,         /* source_directive  */
  YYSYMBOL__literal = 134,                 /* _literal  */
  YYSYMBOL_define_directive = 135,         /* define_directive  */
  YYSYMBOL_cobol_words_directive = 136,    /* cobol_words_directive  */
  YYSYMBOL_listing_directive = 137,        /* listing_directive  */
  YYSYMBOL_listing_statement = 138,        /* listing_statement  */
  YYSYMBOL_control_options = 139,          /* control_options  */
  YYSYMBOL_control_option = 140,           /* control_option  */
  YYSYMBOL__dot = 141,                     /* _dot  */
  YYSYMBOL_leap_second_directive = 142,    /* leap_second_directive  */
  YYSYMBOL_turn_directive = 143,           /* turn_directive  */
  YYSYMBOL_ec_list = 144,                  /* ec_list  */
  YYSYMBOL_on_or_off = 145,                /* on_or_off  */
  YYSYMBOL_on_with_loc = 146,              /* on_with_loc  */
  YYSYMBOL_with_loc = 147,                 /* with_loc  */
  YYSYMBOL_call_directive = 148,           /* call_directive  */
  YYSYMBOL_call_choice = 149,              /* call_choice  */
  YYSYMBOL_if_directive = 150,             /* if_directive  */
  YYSYMBOL_garbage = 151,                  /* garbage  */
  YYSYMBOL_variable_or_literal = 152,      /* variable_or_literal  */
  YYSYMBOL_object_id = 153,                /* object_id  */
  YYSYMBOL_condition_clause = 154,         /* condition_clause  */
  YYSYMBOL_copy_statement = 155,           /* copy_statement  */
  YYSYMBOL_copy_source = 156,              /* copy_source  */
  YYSYMBOL__copy_in = 157,                 /* _copy_in  */
  YYSYMBOL_in_or_of = 158,                 /* in_or_of  */
  YYSYMBOL__copy_suppress = 159,           /* _copy_suppress  */
  YYSYMBOL__copy_replacing = 160,          /* _copy_replacing  */
  YYSYMBOL_replace_statement_with_dot = 161, /* replace_statement_with_dot  */
  YYSYMBOL_replace_statement = 162,        /* replace_statement  */
  YYSYMBOL_replacing_list = 163,           /* replacing_list  */
  YYSYMBOL_text_src = 164,                 /* text_src  */
  YYSYMBOL_text_dst = 165,                 /* text_dst  */
  YYSYMBOL_text_partial_src = 166,         /* text_partial_src  */
  YYSYMBOL_text_partial_dst = 167,         /* text_partial_dst  */
  YYSYMBOL_token_list = 168,               /* token_list  */
  YYSYMBOL_identifier = 169,               /* identifier  */
  YYSYMBOL_subscripts = 170,               /* subscripts  */
  YYSYMBOL_lead_trail = 171,               /* lead_trail  */
  YYSYMBOL_unquoted_literal = 172,         /* unquoted_literal  */
  YYSYMBOL__override = 173,                /* _override  */
  YYSYMBOL__not = 174,                     /* _not  */
  YYSYMBOL__also = 175,                    /* _also  */
  YYSYMBOL__last = 176,                    /* _last  */
  YYSYMBOL__as = 177,                      /* _as  */
  YYSYMBOL__format = 178,                  /* _format  */
  YYSYMBOL__is = 179,                      /* _is  */
  YYSYMBOL__printing = 180,                /* _printing  */
  YYSYMBOL__on = 181,                      /* _on  */
  YYSYMBOL__than = 182,                    /* _than  */
  YYSYMBOL__to = 183                       /* _to  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if 1

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* 1 */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  5
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   318

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  107
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  77
/* YYNRULES -- Number of rules.  */
#define YYNRULES  220
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  316

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   359


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     105,   106,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   788,   788,   789,   797,   798,   799,   800,   804,   809,
     812,   813,   816,   817,   821,   822,   826,   827,   828,   829,
     833,   834,   835,   836,   837,   838,   839,   840,   842,   841,
     847,   846,   851,   855,   860,   859,   872,   873,   881,   882,
     890,   891,   895,   911,   912,   919,   926,   932,   944,   949,
     953,   957,   962,   974,   988,  1000,  1004,  1010,  1015,  1020,
    1024,  1028,  1032,  1037,  1045,  1049,  1056,  1063,  1074,  1079,
    1084,  1125,  1129,  1136,  1137,  1144,  1152,  1153,  1160,  1168,
    1169,  1176,  1185,  1188,  1195,  1200,  1208,  1217,  1225,  1226,
    1230,  1239,  1272,  1276,  1292,  1299,  1307,  1314,  1322,  1332,
    1335,  1336,  1340,  1341,  1345,  1346,  1350,  1351,  1352,  1353,
    1354,  1355,  1358,  1359,  1362,  1364,  1368,  1372,  1379,  1383,
    1390,  1394,  1398,  1405,  1406,  1410,  1411,  1415,  1416,  1420,
    1425,  1430,  1435,  1442,  1449,  1456,  1466,  1481,  1489,  1490,
    1491,  1495,  1496,  1500,  1513,  1527,  1531,  1535,  1539,  1543,
    1547,  1551,  1555,  1559,  1563,  1567,  1574,  1579,  1586,  1595,
    1608,  1611,  1618,  1619,  1622,  1624,  1629,  1632,  1639,  1640,
    1647,  1651,  1658,  1662,  1666,  1670,  1677,  1681,  1706,  1710,
    1714,  1718,  1725,  1735,  1739,  1747,  1751,  1755,  1762,  1766,
    1773,  1777,  1784,  1791,  1806,  1810,  1818,  1822,  1829,  1849,
    1852,  1860,  1863,  1871,  1874,  1882,  1885,  1891,  1891,  1892,
    1892,  1893,  1893,  1894,  1894,  1895,  1895,  1896,  1896,  1897,
    1897
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if 1
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "ALSO", "BY", "COPY",
  "\"==\"", "IN", "LAST", "LEADING", "OF", "OFF", "PRINTING", "REPLACE",
  "REPLACING", "SUPPRESS", "TRAILING", "\".\"", "\"word\"",
  "LISTING_DIRECTIVE", "LISTING_STATEMENT", "TITLE_STATEMENT",
  "COBOL_WORDS_DIRECTIVE", "EQUATE", "UNDEFINE", "SUBSTITUTE", "RESERVE",
  "CONTROL_STATEMENT", "SOURCE", "NOSOURCE", "LIST", "NOLIST", "MAP",
  "NOMAP", "LEAP_SECOND_DIRECTIVE", "\"CONTROL DIVISION\"",
  "\"SUBSTITUTION SECTION\"", "SOURCE_DIRECTIVE", "FORMAT", "IS",
  "CALL_DIRECTIVE", "COBOL", "\"EXTERN\"", "STDCALL", "STATIC",
  "DEFINE_DIRECTIVE", "AS", "PARAMETER", "OVERRIDE", "REFMOD_DIRECTIVE",
  "SET_DIRECTIVE", "ADDRSV", "ADDSYN", "AREACHECK", "NOAREACHECK",
  "ASSIGN", "BOUND", "CALLFH", "CHECKNUM", "COMP1", "CONSTANT",
  "\"DPC-IN-DATA\"", "FOLDCOPYNAME", "MAKESYN", "NOBOUND", "NOCHECKNUM",
  "\"NODPC-IN-DATA\"", "NOFOLDCOPYNAME", "NOODOSLIDE", "NOSPZERO",
  "NOSSRANGE", "ODOSLIDE", "REMOVE", "SOURCEFORMAT", "SPZERO", "SSRANGE",
  "IF_DIRECTIVE", "ELSE_DIRECTIVE", "ENDIF_DIRECTIVE", "ELIF_DIRECTIVE",
  "\">=\"", "\"<=\"", "\"<\"", "\">\"", "\"=\"", "\"<>\"", "NOT", "THAN",
  "TO", "OR", "EQUAL", "GREATER", "LESS", "SET", "DEFINED",
  "TURN_DIRECTIVE", "ON", "CHECKING", "WITH", "LOCATION",
  "\"end of line\"", "\"Word or Literal\"", "\"Text-Name\"",
  "\"Variable\"", "\"Literal\"", "'('", "')'", "$accept",
  "program_structure", "program_with_control_division",
  "control_division_no_replace", "control_division_with_replace",
  "statement_list", "statement_no_replace_list", "statement",
  "statement_no_replace", "directive", "$@1", "$@2", "$@3",
  "if_directive_if", "if_directive_elif", "set_directive", "set_choice",
  "alnum_list", "alnum_equality_list", "alnum_equality", "alnum_with_list",
  "alnum_with", "alnum_by_list", "alnum_by", "set_options",
  "refmod_directive", "source_directive", "_literal", "define_directive",
  "cobol_words_directive", "listing_directive", "listing_statement",
  "control_options", "control_option", "_dot", "leap_second_directive",
  "turn_directive", "ec_list", "on_or_off", "on_with_loc", "with_loc",
  "call_directive", "call_choice", "if_directive", "garbage",
  "variable_or_literal", "object_id", "condition_clause", "copy_statement",
  "copy_source", "_copy_in", "in_or_of", "_copy_suppress",
  "_copy_replacing", "replace_statement_with_dot", "replace_statement",
  "replacing_list", "text_src", "text_dst", "text_partial_src",
  "text_partial_dst", "token_list", "identifier", "subscripts",
  "lead_trail", "unquoted_literal", "_override", "_not", "_also", "_last",
  "_as", "_format", "_is", "_printing", "_on", "_than", "_to", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-198)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-208)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     -13,     9,    47,   123,    32,  -198,    19,    43,    37,  -198,
     -29,    61,   208,    38,    56,  -198,   -45,    41,   215,  -198,
    -198,  -198,  -198,   -15,  -198,  -198,   -30,  -198,  -198,  -198,
      24,    79,  -198,   170,    83,   123,    88,  -198,  -198,    67,
    -198,  -198,    22,   115,  -198,  -198,  -198,   124,    51,    52,
      55,    52,  -198,  -198,  -198,  -198,  -198,  -198,  -198,    50,
    -198,  -198,  -198,  -198,  -198,  -198,   114,    49,    58,   -28,
    -198,  -198,  -198,  -198,  -198,  -198,  -198,    62,    52,    62,
    -198,  -198,    63,  -198,    63,  -198,    63,    59,    63,   108,
      62,  -198,  -198,  -198,  -198,  -198,  -198,  -198,  -198,    52,
     108,  -198,    65,   -33,   215,  -198,     8,    15,  -198,  -198,
     -47,  -198,   148,  -198,   157,  -198,  -198,  -198,  -198,  -198,
     156,   -62,    73,  -198,  -198,  -198,    22,   178,    17,     2,
    -198,  -198,    84,    85,    51,  -198,  -198,    81,   189,    55,
    -198,    81,  -198,    94,  -198,   -43,  -198,  -198,  -198,  -198,
      49,  -198,   108,  -198,    -5,   111,    62,  -198,    81,  -198,
    -198,  -198,  -198,  -198,    92,  -198,    63,  -198,    81,    10,
    -198,  -198,  -198,    99,  -198,  -198,     4,     6,  -198,  -198,
      13,  -198,  -198,  -198,  -198,    -1,  -198,  -198,  -198,   123,
     170,   186,   191,  -198,  -198,    26,   202,     2,    23,   107,
     110,   112,   113,  -198,   205,  -198,   125,  -198,  -198,   126,
    -198,  -198,  -198,  -198,  -198,   138,  -198,   164,   164,   139,
    -198,  -198,  -198,  -198,  -198,  -198,   130,   130,  -198,  -198,
    -198,  -198,     3,   118,  -198,  -198,  -198,  -198,  -198,  -198,
    -198,    22,   210,  -198,  -198,    23,   224,    28,  -198,  -198,
    -198,    17,  -198,  -198,  -198,   -37,   238,    29,  -198,  -198,
     164,  -198,  -198,  -198,  -198,  -198,   141,    96,  -198,  -198,
      22,  -198,  -198,    29,  -198,    30,  -198,  -198,  -198,    31,
    -198,  -198,  -198,  -198,  -198,  -198,  -198,  -198,  -198,   162,
     158,   158,  -198,  -198,    36,    36,  -198,  -198,  -198,   245,
    -198,  -198,  -198,   163,   165,  -198,  -198,  -198,  -198,  -198,
     166,   167,   162,   162,  -198,  -198
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
      10,     0,     0,     3,    10,     1,     0,   203,    99,   102,
       0,     0,     0,   114,   209,    34,     0,   215,     0,    28,
      32,    33,    30,     0,    11,    14,     0,    18,    16,    15,
       0,     0,     2,     6,     0,     4,     0,   158,   159,   160,
     204,   206,     0,     0,   101,   100,    26,   112,     0,     0,
       0,     0,    22,   106,   107,   108,   109,   110,   111,   112,
     104,   116,   115,    27,   210,    20,   211,     0,     0,   207,
     142,    21,    94,    85,   216,    24,    84,     0,     0,     0,
      46,    56,     0,    48,    50,    51,     0,     0,     0,   207,
       0,    57,    58,    59,    60,    61,    62,    63,    64,     0,
     207,    69,    88,    82,    23,    40,     0,     0,   118,    25,
       0,    17,     0,   168,     8,    10,    12,   157,   162,   163,
     164,     0,     0,   196,   197,   190,   170,     0,   177,     0,
     171,   113,     0,     0,    95,    76,    71,    96,     0,    97,
      79,    98,   105,     0,   212,     0,   129,   130,   131,   132,
      35,   127,   207,   208,     0,     0,    65,    73,    44,    45,
     198,    47,    49,    52,     0,    53,     0,    55,    66,     0,
      89,    70,    43,     0,    41,    37,   211,   211,    29,    36,
       0,   138,    39,    31,    38,     0,   119,   169,     9,     5,
       7,   213,   166,   161,   188,     0,     0,     0,     0,     0,
       0,     0,     0,   184,     0,   103,     0,    77,    72,     0,
      80,    19,    86,    87,   128,     0,    92,   199,   199,     0,
      74,    42,    54,    68,    67,    83,   201,   201,   140,   141,
     139,   122,   121,     0,   126,   117,   120,   124,    13,   214,
     165,     0,     0,   176,   189,     0,     0,     0,   181,   182,
     172,   180,   191,   192,   194,     0,     0,     0,    78,    81,
     199,   200,    91,    90,    75,   202,     0,     0,   123,   125,
     167,   156,   174,     0,   178,     0,   195,   193,   183,     0,
     187,   173,    93,   150,   152,   153,   151,   154,   155,   219,
     217,   217,   134,   133,     0,     0,   175,   179,   185,     0,
     220,   149,   218,   146,   148,   144,   143,   135,   136,   186,
       0,     0,   219,   219,   145,   147
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -198,  -198,  -198,  -198,  -198,     0,  -198,  -198,   -32,  -198,
    -198,  -198,  -198,  -198,  -198,  -198,   149,   -34,  -198,   -67,
    -198,   121,  -198,   119,  -198,  -198,  -198,  -198,  -198,  -198,
    -198,  -198,  -198,   200,   201,  -198,  -198,  -198,  -198,  -198,
      60,  -198,   143,   154,  -198,   -16,    -4,    -3,  -198,   173,
    -198,  -198,  -198,  -198,  -198,   181,    21,  -124,    53,   100,
      27,    54,  -179,  -198,  -123,   -31,  -197,    69,  -198,  -198,
     -27,  -198,   -25,  -198,  -198,    11,  -164
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     2,    32,    33,    34,     3,   190,    24,    25,    26,
     106,   107,    67,   178,   183,   104,   105,   137,   156,   157,
     134,   135,   139,   140,   172,    75,    65,   171,    71,    52,
      46,    27,    59,    60,   132,    63,   109,   110,   235,   236,
     237,   150,   151,   179,   180,   181,   307,   294,    28,    39,
     120,   121,   192,   242,    29,    30,   126,   127,   250,   204,
     281,   195,   128,   255,   129,   161,   262,   266,    42,    43,
     154,    66,   145,   240,    76,   303,   301
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      72,   115,   196,   197,    35,  -141,   216,  -142,   202,   175,
     231,   223,   159,   153,   228,    68,   182,   141,   153,   251,
      36,   263,     1,   167,   199,   112,     4,   200,   122,   247,
     248,   123,   243,   249,   274,   279,   297,   298,   124,    37,
      38,   113,   217,   144,   158,   144,    40,     5,    44,    61,
     185,    41,    73,   162,  -205,   163,   186,   165,    69,    70,
     212,   213,   166,   282,   276,   168,   251,   131,    31,   277,
     111,  -207,  -141,   169,   118,    47,   173,   119,    53,    54,
      55,    56,    57,    58,    48,    49,    50,    51,   108,   220,
     146,   147,   148,   149,    64,   232,   114,   233,   234,   218,
     116,   233,   234,   203,  -141,   117,  -142,  -141,  -141,  -142,
    -142,   176,   177,  -137,   160,   189,   229,    70,   176,   177,
      37,    38,   201,   125,   125,   215,   130,   244,     6,   194,
     280,   244,   299,    45,    62,   222,     7,    74,   224,   305,
     306,   131,     8,     9,    10,    11,   196,   197,   314,   315,
      12,   226,   227,   144,   153,   133,   136,    13,   238,   138,
      14,   152,   164,    15,   230,   187,   155,   160,    16,   170,
       7,   191,    17,    18,   194,     6,   283,   284,   285,   286,
     287,   288,   198,   206,   205,   208,   289,   290,   291,     8,
       9,    10,    11,   209,   211,   219,   221,    12,   239,    19,
      20,    21,    22,   225,    13,   241,   245,    14,   252,   257,
      15,   253,   261,   254,   256,    16,   265,   269,    23,    17,
      18,   283,   284,   285,   286,   287,   288,   271,   273,   258,
     259,   289,   290,   291,   292,   293,    53,    54,    55,    56,
      57,    58,   260,   264,   278,   302,    19,    20,    21,    22,
     300,   309,   310,   174,   311,   207,   312,   313,   210,   142,
     143,   184,   270,    77,   295,    23,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,   308,   268,   214,   193,   188,   267,   246,   272,     0,
     296,   275,   304,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   103
};

static const yytype_int16 yycheck[] =
{
      16,    33,   126,   126,     4,     1,    11,     1,     6,     1,
      11,     1,    79,    46,     1,    60,     1,    51,    46,   198,
       1,   218,    35,    90,     7,     1,    17,    10,     6,     6,
       7,     9,     6,    10,     6,     6,     6,     6,    16,   101,
     102,    17,    47,    39,    78,    39,     3,     0,    11,    11,
      97,     8,    11,    84,    11,    86,   103,    88,   103,   104,
     103,   104,    89,   260,   101,    99,   245,    17,    36,   106,
     100,   104,   100,   100,     7,   104,   103,    10,    28,    29,
      30,    31,    32,    33,    23,    24,    25,    26,   103,   156,
      41,    42,    43,    44,    38,    96,    17,    98,    99,   104,
      17,    98,    99,   101,   100,    17,   100,   103,   104,   103,
     104,   103,   104,   100,   104,   115,   103,   104,   103,   104,
     101,   102,   105,   101,   101,   152,    11,   101,     5,   101,
     101,   101,   101,    96,    96,   166,    13,    96,   169,   103,
     104,    17,    19,    20,    21,    22,   270,   270,   312,   313,
      27,   176,   177,    39,    46,   104,   104,    34,   190,   104,
      37,   103,   103,    40,   180,    17,   104,   104,    45,   104,
      13,    15,    49,    50,   101,     5,    80,    81,    82,    83,
      84,    85,     4,    98,   100,   104,    90,    91,    92,    19,
      20,    21,    22,     4,   100,    84,   104,    27,    12,    76,
      77,    78,    79,   104,    34,    14,     4,    37,   101,     4,
      40,   101,    48,   101,   101,    45,    86,    99,    95,    49,
      50,    80,    81,    82,    83,    84,    85,    17,     4,   104,
     104,    90,    91,    92,    93,    94,    28,    29,    30,    31,
      32,    33,   104,   104,     6,    87,    76,    77,    78,    79,
      88,     6,    89,   104,    89,   134,    90,    90,   139,    59,
      59,   107,   241,    48,   267,    95,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,   295,   232,   150,   121,   114,   227,   197,   245,    -1,
     273,   247,   291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    35,   108,   112,    17,     0,     5,    13,    19,    20,
      21,    22,    27,    34,    37,    40,    45,    49,    50,    76,
      77,    78,    79,    95,   114,   115,   116,   138,   155,   161,
     162,    36,   109,   110,   111,   112,     1,   101,   102,   156,
       3,     8,   175,   176,    11,    96,   137,   104,    23,    24,
      25,    26,   136,    28,    29,    30,    31,    32,    33,   139,
     140,    11,    96,   142,    38,   133,   178,   119,    60,   103,
     104,   135,   152,    11,    96,   132,   181,    48,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,   103,   122,   123,   117,   118,   103,   143,
     144,   100,     1,    17,    17,   115,    17,    17,     7,    10,
     157,   158,     6,     9,    16,   101,   163,   164,   169,   171,
      11,    17,   141,   104,   127,   128,   104,   124,   104,   129,
     130,   124,   140,   141,    39,   179,    41,    42,    43,    44,
     148,   149,   103,    46,   177,   104,   125,   126,   124,   126,
     104,   172,   172,   172,   103,   172,   177,   126,   124,   177,
     104,   134,   131,   177,   123,     1,   103,   104,   120,   150,
     151,   152,     1,   121,   150,    97,   103,    17,   162,   112,
     113,    15,   159,   156,   101,   168,   164,   171,     4,     7,
      10,   105,     6,   101,   166,   100,    98,   128,   104,     4,
     130,   100,   103,   104,   149,   177,    11,    47,   104,    84,
     126,   104,   172,     1,   172,   104,   179,   179,     1,   103,
     152,    11,    96,    98,    99,   145,   146,   147,   115,    12,
     180,    14,   160,     6,   101,     4,   166,     6,     7,    10,
     165,   169,   101,   101,   101,   170,   101,     4,   104,   104,
     104,    48,   173,   173,   104,    86,   174,   174,   147,    99,
     163,    17,   165,     4,     6,   168,   101,   106,     6,     6,
     101,   167,   173,    80,    81,    82,    83,    84,    85,    90,
      91,    92,    93,    94,   154,   154,   167,     6,     6,   101,
      88,   183,    87,   182,   182,   103,   104,   153,   153,     6,
      89,    89,    90,    90,   183,   183
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_uint8 yyr1[] =
{
       0,   107,   108,   108,   109,   109,   109,   109,   110,   111,
     112,   112,   113,   113,   114,   114,   115,   115,   115,   115,
     116,   116,   116,   116,   116,   116,   116,   116,   117,   116,
     118,   116,   116,   116,   119,   116,   120,   120,   121,   121,
     122,   122,   123,   123,   123,   123,   123,   123,   123,   123,
     123,   123,   123,   123,   123,   123,   123,   123,   123,   123,
     123,   123,   123,   123,   123,   123,   123,   123,   123,   123,
     123,   124,   124,   125,   125,   126,   127,   127,   128,   129,
     129,   130,   131,   131,   132,   132,   133,   133,   134,   134,
     135,   135,   135,   135,   135,   136,   136,   136,   136,   137,
     137,   137,   138,   138,   139,   139,   140,   140,   140,   140,
     140,   140,   141,   141,   142,   142,   142,   143,   144,   144,
     145,   145,   145,   146,   146,   147,   147,   148,   148,   149,
     149,   149,   149,   150,   150,   150,   150,   150,   151,   151,
     151,   152,   152,   153,   153,   154,   154,   154,   154,   154,
     154,   154,   154,   154,   154,   154,   155,   155,   156,   156,
     157,   157,   158,   158,   159,   159,   160,   160,   161,   161,
     162,   162,   163,   163,   163,   163,   164,   164,   165,   165,
     165,   165,   165,   166,   166,   167,   167,   167,   168,   168,
     169,   169,   169,   169,   170,   170,   171,   171,   172,   173,
     173,   174,   174,   175,   175,   176,   176,   177,   177,   178,
     178,   179,   179,   180,   180,   181,   181,   182,   182,   183,
     183
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     3,     1,     1,     3,     1,     3,     2,     3,
       0,     2,     0,     2,     1,     1,     1,     2,     1,     4,
       2,     2,     2,     2,     2,     2,     2,     2,     0,     3,
       0,     3,     1,     1,     0,     3,     1,     1,     1,     1,
       1,     2,     3,     2,     2,     2,     1,     2,     1,     2,
       1,     1,     2,     2,     3,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     3,     3,     1,
       2,     1,     2,     1,     2,     3,     1,     2,     3,     1,
       2,     3,     0,     2,     1,     1,     3,     3,     0,     1,
       4,     4,     3,     5,     1,     2,     2,     2,     2,     0,
       1,     1,     1,     4,     1,     2,     1,     1,     1,     1,
       1,     1,     0,     1,     0,     1,     1,     3,     1,     2,
       1,     1,     1,     2,     1,     2,     1,     1,     2,     1,
       1,     1,     1,     4,     4,     5,     5,     1,     1,     2,
       2,     1,     1,     1,     1,     5,     2,     5,     2,     2,
       1,     1,     1,     1,     1,     1,     6,     3,     1,     1,
       0,     2,     1,     1,     0,     2,     0,     2,     2,     3,
       3,     3,     3,     4,     4,     5,     3,     1,     2,     3,
       1,     1,     1,     3,     1,     2,     3,     1,     1,     2,
       1,     3,     3,     4,     1,     2,     1,     1,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


/* Context of a parse error.  */
typedef struct
{
  yy_state_t *yyssp;
  yysymbol_kind_t yytoken;
} yypcontext_t;

/* Put in YYARG at most YYARGN of the expected tokens given the
   current YYCTX, and return the number of tokens stored in YYARG.  If
   YYARG is null, return the number of expected tokens (guaranteed to
   be less than YYNTOKENS).  Return YYENOMEM on memory exhaustion.
   Return 0 if there are more than YYARGN expected tokens, yet fill
   YYARG up to YYARGN. */
static int
yypcontext_expected_tokens (const yypcontext_t *yyctx,
                            yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  int yyn = yypact[+*yyctx->yyssp];
  if (!yypact_value_is_default (yyn))
    {
      /* Start YYX at -YYN if negative to avoid negative indexes in
         YYCHECK.  In other words, skip the first -YYN actions for
         this state because they are default actions.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;
      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yyx;
      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
        if (yycheck[yyx + yyn] == yyx && yyx != YYSYMBOL_YYerror
            && !yytable_value_is_error (yytable[yyx + yyn]))
          {
            if (!yyarg)
              ++yycount;
            else if (yycount == yyargn)
              return 0;
            else
              yyarg[yycount++] = YY_CAST (yysymbol_kind_t, yyx);
          }
    }
  if (yyarg && yycount == 0 && 0 < yyargn)
    yyarg[0] = YYSYMBOL_YYEMPTY;
  return yycount;
}




#ifndef yystrlen
# if defined __GLIBC__ && defined _STRING_H
#  define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
# else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
# endif
#endif

#ifndef yystpcpy
# if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#  define yystpcpy stpcpy
# else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
# endif
#endif

#ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
      char const *yyp = yystr;
      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            else
              goto append;

          append:
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
}
#endif


static int
yy_syntax_error_arguments (const yypcontext_t *yyctx,
                           yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yyctx->yytoken != YYSYMBOL_YYEMPTY)
    {
      int yyn;
      if (yyarg)
        yyarg[yycount] = yyctx->yytoken;
      ++yycount;
      yyn = yypcontext_expected_tokens (yyctx,
                                        yyarg ? yyarg + 1 : yyarg, yyargn - 1);
      if (yyn == YYENOMEM)
        return YYENOMEM;
      else
        yycount += yyn;
    }
  return yycount;
}

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return -1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return YYENOMEM if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                const yypcontext_t *yyctx)
{
  enum { YYARGS_MAX = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  yysymbol_kind_t yyarg[YYARGS_MAX];
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* Actual size of YYARG. */
  int yycount = yy_syntax_error_arguments (yyctx, yyarg, YYARGS_MAX);
  if (yycount == YYENOMEM)
    return YYENOMEM;

  switch (yycount)
    {
#define YYCASE_(N, S)                       \
      case N:                               \
        yyformat = S;                       \
        break
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
    }

  /* Compute error message size.  Don't count the "%s"s, but reserve
     room for the terminator.  */
  yysize = yystrlen (yyformat) - 2 * yycount + 1;
  {
    int yyi;
    for (yyi = 0; yyi < yycount; ++yyi)
      {
        YYPTRDIFF_T yysize1
          = yysize + yytnamerr (YY_NULLPTR, yytname[yyarg[yyi]]);
        if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
          yysize = yysize1;
        else
          return YYENOMEM;
      }
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return -1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yytname[yyarg[yyi++]]);
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= TOKEN_EOF)
    {
      yychar = TOKEN_EOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 28: /* $@1: %empty  */
#line 842 "../../cobc/ppparse.y"
  {
	current_cmd = PLEX_ACT_IF;
  }
#line 2530 "ppparse.c"
    break;

  case 30: /* $@2: %empty  */
#line 847 "../../cobc/ppparse.y"
  {
	current_cmd = PLEX_ACT_ELIF;
  }
#line 2538 "ppparse.c"
    break;

  case 32: /* directive: ELSE_DIRECTIVE  */
#line 852 "../../cobc/ppparse.y"
  {
	plex_action_directive (PLEX_ACT_ELSE, 0);
  }
#line 2546 "ppparse.c"
    break;

  case 33: /* directive: ENDIF_DIRECTIVE  */
#line 856 "../../cobc/ppparse.y"
  {
	plex_action_directive (PLEX_ACT_END, 0);
  }
#line 2554 "ppparse.c"
    break;

  case 34: /* $@3: %empty  */
#line 860 "../../cobc/ppparse.y"
  {
	current_call_convention = 0;
  }
#line 2562 "ppparse.c"
    break;

  case 35: /* directive: CALL_DIRECTIVE $@3 call_directive  */
#line 864 "../../cobc/ppparse.y"
  {
	if (current_call_convention == CB_CONV_STATIC_LINK) {
		current_call_convention |= CB_CONV_COBOL;
	};
  }
#line 2572 "ppparse.c"
    break;

  case 37: /* if_directive_if: error  */
#line 874 "../../cobc/ppparse.y"
  {
	cb_error (_("invalid %s directive"), "IF");
	yyerrok;
  }
#line 2581 "ppparse.c"
    break;

  case 39: /* if_directive_elif: error  */
#line 883 "../../cobc/ppparse.y"
  {
	cb_error (_("invalid %s directive"), "ELIF");
	yyerrok;
  }
#line 2590 "ppparse.c"
    break;

  case 42: /* set_choice: CONSTANT "Variable" "Literal"  */
#line 896 "../../cobc/ppparse.y"
  {
	/* note: the old version was _as LITERAL but MF doesn't support this */
	struct cb_define_struct	*p;

	p = ppp_define_add (ppp_setvar_list, (yyvsp[-1].s), (yyvsp[0].s), 1);
	if (p) {
		ppp_setvar_list = p;
		p = p->last;
		if (p->deftype == PLEX_DEF_NUM) {
			fprintf (ppout, "#DEFLIT %s %s\n", (yyvsp[-1].s), p->value);
		} else {
			fprintf (ppout, "#DEFLIT %s \"%s\"\n", (yyvsp[-1].s), p->value);
		}
	}
  }
#line 2610 "ppparse.c"
    break;

  case 44: /* set_choice: ADDRSV alnum_list  */
#line 913 "../../cobc/ppparse.y"
  {
	struct cb_text_list	*l;
	for (l = (yyvsp[0].l); l; l = l->next) {
		fprintf (ppout, "#ADDRSV %s\n", l->text);
	}
  }
#line 2621 "ppparse.c"
    break;

  case 45: /* set_choice: ADDSYN alnum_equality  */
#line 920 "../../cobc/ppparse.y"
  {
	struct cb_text_list	*l;
	for (l = (yyvsp[0].l); l; l = l->next->next) {
		fprintf (ppout, "#ADDSYN %s %s\n", l->text, l->next->text);
	}
  }
#line 2632 "ppparse.c"
    break;

  case 46: /* set_choice: AREACHECK  */
#line 927 "../../cobc/ppparse.y"
  {
	if (cobc_has_areacheck_directive ("AREACHECK")) {
		fprintf (ppout, "#AREACHECK\n");
	}
  }
#line 2642 "ppparse.c"
    break;

  case 47: /* set_choice: ASSIGN unquoted_literal  */
#line 933 "../../cobc/ppparse.y"
  {
	char	*p = (yyvsp[0].s);

	if (!cb_strcasecmp (p, "EXTERNAL")) {
		fprintf (ppout, "#ASSIGN %d\n", (int)CB_ASSIGN_EXT_FILE_NAME_REQUIRED);
	} else if (!cb_strcasecmp (p, "DYNAMIC")) {
		fprintf (ppout, "#ASSIGN %d\n", (int)CB_ASSIGN_VARIABLE_DEFAULT);
	} else {
		ppp_error_invalid_option ("ASSIGN", p);
	}
  }
#line 2658 "ppparse.c"
    break;

  case 48: /* set_choice: BOUND  */
#line 945 "../../cobc/ppparse.y"
  {
	/* Enable EC-BOUND-SUBSCRIPT checking */
	append_to_turn_list (ppp_list_add (NULL, "EC-BOUND-SUBSCRIPT"), 1, 0);
  }
#line 2667 "ppparse.c"
    break;

  case 49: /* set_choice: CALLFH unquoted_literal  */
#line 950 "../../cobc/ppparse.y"
  {
	fprintf (ppout, "#CALLFH \"%s\"\n", (yyvsp[0].s));
  }
#line 2675 "ppparse.c"
    break;

  case 50: /* set_choice: CALLFH  */
#line 954 "../../cobc/ppparse.y"
  {
	fprintf (ppout, "#CALLFH \"EXTFH\"\n");
  }
#line 2683 "ppparse.c"
    break;

  case 51: /* set_choice: CHECKNUM  */
#line 958 "../../cobc/ppparse.y"
  {
	/* Enable EC-DATA-INCOMPATIBLE checking */
	append_to_turn_list (ppp_list_add (NULL, "EC-DATA-INCOMPATIBLE"), 1, 0);
  }
#line 2692 "ppparse.c"
    break;

  case 52: /* set_choice: COMP1 unquoted_literal  */
#line 963 "../../cobc/ppparse.y"
  {
	char	*p = (yyvsp[0].s);

	if (!cb_strcasecmp (p, "BINARY")) {
		cb_binary_comp_1 = 1;
	} else if (!cb_strcasecmp (p, "FLOAT")) {
		cb_binary_comp_1 = 0;
	} else {
		ppp_error_invalid_option ("COMP1", p);
	}
  }
#line 2708 "ppparse.c"
    break;

  case 53: /* set_choice: "DPC-IN-DATA" unquoted_literal  */
#line 975 "../../cobc/ppparse.y"
  {
	char	*p = (yyvsp[0].s);

	if (!cb_strcasecmp (p, "XML")) {
		cb_dpc_in_data = CB_DPC_IN_XML;
	} else if (!cb_strcasecmp (p, "JSON")) {
		cb_dpc_in_data = CB_DPC_IN_JSON;
	} else if (!cb_strcasecmp (p, "ALL")) {
		cb_dpc_in_data = CB_DPC_IN_ALL;
	} else {
		ppp_error_invalid_option ("DPC-IN-DATA", p);
	}
  }
#line 2726 "ppparse.c"
    break;

  case 54: /* set_choice: FOLDCOPYNAME _as unquoted_literal  */
#line 989 "../../cobc/ppparse.y"
  {
	char	*p = (yyvsp[0].s);

	if (!cb_strcasecmp (p, "UPPER")) {
		cb_fold_copy = COB_FOLD_UPPER;
	} else if (!cb_strcasecmp (p, "LOWER")) {
		cb_fold_copy = COB_FOLD_LOWER;
	} else {
		ppp_error_invalid_option ("FOLD-COPY-NAME", p);
	}
  }
#line 2742 "ppparse.c"
    break;

  case 55: /* set_choice: MAKESYN alnum_equality  */
#line 1001 "../../cobc/ppparse.y"
  {
	fprintf (ppout, "#MAKESYN %s %s\n", (yyvsp[0].l)->text, (yyvsp[0].l)->next->text);
  }
#line 2750 "ppparse.c"
    break;

  case 56: /* set_choice: NOAREACHECK  */
#line 1005 "../../cobc/ppparse.y"
  {
	if (cobc_has_areacheck_directive ("NOAREACHECK")) {
		fprintf (ppout, "#NOAREACHECK\n");
	}
  }
#line 2760 "ppparse.c"
    break;

  case 57: /* set_choice: NOBOUND  */
#line 1011 "../../cobc/ppparse.y"
  {
	/* Disable EC-BOUND-SUBSCRIPT checking */
	append_to_turn_list (ppp_list_add (NULL, "EC-BOUND-SUBSCRIPT"), 0, 0);
  }
#line 2769 "ppparse.c"
    break;

  case 58: /* set_choice: NOCHECKNUM  */
#line 1016 "../../cobc/ppparse.y"
  {
	/* Disable EC-DATA-INCOMPATIBLE checking */
	append_to_turn_list (ppp_list_add (NULL, "EC-DATA-INCOMPATIBLE"), 0, 0);
  }
#line 2778 "ppparse.c"
    break;

  case 59: /* set_choice: "NODPC-IN-DATA"  */
#line 1021 "../../cobc/ppparse.y"
  {
	cb_dpc_in_data = CB_DPC_IN_NONE;
  }
#line 2786 "ppparse.c"
    break;

  case 60: /* set_choice: NOFOLDCOPYNAME  */
#line 1025 "../../cobc/ppparse.y"
  {
	cb_fold_copy = 0;
  }
#line 2794 "ppparse.c"
    break;

  case 61: /* set_choice: NOODOSLIDE  */
#line 1029 "../../cobc/ppparse.y"
  {
	fprintf (ppout, "#ODOSLIDE 0\n");
  }
#line 2802 "ppparse.c"
    break;

  case 62: /* set_choice: NOSPZERO  */
#line 1033 "../../cobc/ppparse.y"
  {
	CB_PENDING ("SPZERO");
	/* TODO: cb_space_is_zero = 0; */
  }
#line 2811 "ppparse.c"
    break;

  case 63: /* set_choice: NOSSRANGE  */
#line 1038 "../../cobc/ppparse.y"
  {
	/* Disable EC-BOUND-SUBSCRIPT and -REF-MOD checking */
	struct cb_text_list	*txt = ppp_list_add (NULL, "EC-BOUND-SUBSCRIPT");
	txt = ppp_list_add (txt, "EC-BOUND-REF-MOD");

	append_to_turn_list (txt, 0, 0);
  }
#line 2823 "ppparse.c"
    break;

  case 64: /* set_choice: ODOSLIDE  */
#line 1046 "../../cobc/ppparse.y"
  {
	fprintf (ppout, "#ODOSLIDE 1\n");
  }
#line 2831 "ppparse.c"
    break;

  case 65: /* set_choice: OVERRIDE alnum_equality_list  */
#line 1050 "../../cobc/ppparse.y"
  {
	struct cb_text_list	*l;
	for (l = (yyvsp[0].l); l; l = l->next->next) {
		fprintf (ppout, "#OVERRIDE %s %s\n", l->text, l->next->text);
	}
  }
#line 2842 "ppparse.c"
    break;

  case 66: /* set_choice: REMOVE alnum_list  */
#line 1057 "../../cobc/ppparse.y"
  {
	struct cb_text_list	*l;
	for (l = (yyvsp[0].l); l; l = l->next) {
		fprintf (ppout, "#REMOVE %s\n", l->text);
	}
  }
#line 2853 "ppparse.c"
    break;

  case 67: /* set_choice: SOURCEFORMAT _as unquoted_literal  */
#line 1064 "../../cobc/ppparse.y"
  {
	char	*p = (yyvsp[0].s);

	if (cobc_deciph_source_format (p) != 0) {
		ppp_error_invalid_option ("SOURCEFORMAT", p);
	}
	if (cb_src_list_file) {
		cb_current_file->source_format = cobc_get_source_format ();
	}
  }
#line 2868 "ppparse.c"
    break;

  case 68: /* set_choice: SOURCEFORMAT _as error  */
#line 1075 "../../cobc/ppparse.y"
  {
	/* FIXME: we should consume until end of line here! */
	ppp_error_invalid_option ("SOURCEFORMAT", NULL);
  }
#line 2877 "ppparse.c"
    break;

  case 69: /* set_choice: SPZERO  */
#line 1080 "../../cobc/ppparse.y"
  {
	CB_PENDING ("SPZERO");
	/* TODO: cb_space_is_zero = 1; */
  }
#line 2886 "ppparse.c"
    break;

  case 70: /* set_choice: SSRANGE _literal  */
#line 1085 "../../cobc/ppparse.y"
  {
	char	*p = (yyvsp[0].s);
	char	ep = 0;

	/* Remove surrounding quotes/brackets */
	if (p) {
		size_t	size;
		++p;
		size = strlen (p) - 1;
		p[size] = '\0';
		if (size == 1 && *p >= '1' && *p <= '3') {
			ep = *p;
		}
	} else {
		ep = '2';
	}

	/* Enable EC-BOUND-SUBSCRIPT and -REF-MOD checking */
	if (ep) {
		struct cb_text_list	*txt;
		if (ep == '3') {
			/* SSRANGE"3": REF-MOD, with zero length allowed (at runtime) */
			fprintf (ppout, "#REFMOD_ZERO 1\n");
		} else if (ep == '2') {
			/* SSRANGE"2": REF-MOD, zero length not allowed */
			fprintf (ppout, "#REFMOD_ZERO 0\n");
		} else /* if (ep == '1') */ {
			/* SSRANGE"1": REF-MOD minimal - check only for zero/negative */
			fprintf (ppout, "#REFMOD_ZERO 2\n");
		}
		txt = ppp_list_add (NULL, "EC-BOUND-SUBSCRIPT");
		txt = ppp_list_add (txt, "EC-BOUND-REF-MOD");
		append_to_turn_list (txt, 1, 0);
	} else {
		ppp_error_invalid_option ("SSRANGE", p);
	}
  }
#line 2928 "ppparse.c"
    break;

  case 71: /* alnum_list: "Literal"  */
#line 1126 "../../cobc/ppparse.y"
  {
	(yyval.l) = ppp_list_add (NULL, (yyvsp[0].s));
  }
#line 2936 "ppparse.c"
    break;

  case 72: /* alnum_list: alnum_list "Literal"  */
#line 1130 "../../cobc/ppparse.y"
  {
	(yyval.l) = ppp_list_add ((yyvsp[-1].l), (yyvsp[0].s));
  }
#line 2944 "ppparse.c"
    break;

  case 74: /* alnum_equality_list: alnum_equality_list alnum_equality  */
#line 1138 "../../cobc/ppparse.y"
  {
	(yyval.l) = ppp_list_append ((yyvsp[-1].l), (yyvsp[0].l));
  }
#line 2952 "ppparse.c"
    break;

  case 75: /* alnum_equality: "Literal" "=" "Literal"  */
#line 1145 "../../cobc/ppparse.y"
  {
	(yyval.l) = ppp_list_add (NULL, (yyvsp[-2].s));
	(yyval.l) = ppp_list_add ((yyval.l), (yyvsp[0].s));
  }
#line 2961 "ppparse.c"
    break;

  case 77: /* alnum_with_list: alnum_with_list alnum_with  */
#line 1154 "../../cobc/ppparse.y"
  {
	(yyval.l) = ppp_list_append ((yyvsp[-1].l), (yyvsp[0].l));
  }
#line 2969 "ppparse.c"
    break;

  case 78: /* alnum_with: "Literal" WITH "Literal"  */
#line 1161 "../../cobc/ppparse.y"
  {
	(yyval.l) = ppp_list_add (NULL, (yyvsp[-2].s));
	(yyval.l) = ppp_list_add ((yyval.l), (yyvsp[0].s));
  }
#line 2978 "ppparse.c"
    break;

  case 80: /* alnum_by_list: alnum_by_list alnum_by  */
#line 1170 "../../cobc/ppparse.y"
  {
	(yyval.l) = ppp_list_append ((yyvsp[-1].l), (yyvsp[0].l));
  }
#line 2986 "ppparse.c"
    break;

  case 81: /* alnum_by: "Literal" BY "Literal"  */
#line 1177 "../../cobc/ppparse.y"
  {
	(yyval.l) = ppp_list_add (NULL, (yyvsp[-2].s));
	(yyval.l) = ppp_list_add ((yyval.l), (yyvsp[0].s));
  }
#line 2995 "ppparse.c"
    break;

  case 82: /* set_options: %empty  */
#line 1185 "../../cobc/ppparse.y"
  {
	fprintf (ppout, "#OPTION %s\n", (yyvsp[0].s));
  }
#line 3003 "ppparse.c"
    break;

  case 83: /* set_options: _as "Literal"  */
#line 1189 "../../cobc/ppparse.y"
  {
	fprintf (ppout, "#OPTION %s %s\n", (yyvsp[-2].s), (yyvsp[0].s));
  }
#line 3011 "ppparse.c"
    break;

  case 84: /* refmod_directive: _on  */
#line 1196 "../../cobc/ppparse.y"
  {
	cb_ref_mod_zero_length = 1;
	fprintf (ppout, "#OPTION REFMOD_ZERO 1\n");
  }
#line 3020 "ppparse.c"
    break;

  case 85: /* refmod_directive: OFF  */
#line 1201 "../../cobc/ppparse.y"
  {
	cb_ref_mod_zero_length = 0;
	fprintf (ppout, "#OPTION REFMOD_ZERO 0\n");
  }
#line 3029 "ppparse.c"
    break;

  case 86: /* source_directive: _format _is "Variable"  */
#line 1209 "../../cobc/ppparse.y"
  {
	  if (cobc_deciph_source_format ((yyvsp[0].s)) != 0) {
		  ppp_error_invalid_option ("SOURCE", (yyvsp[0].s));
	  }
	  if (cb_src_list_file) {
		  cb_current_file->source_format = cobc_get_source_format ();
	  }
  }
#line 3042 "ppparse.c"
    break;

  case 87: /* source_directive: _format _is "Literal"  */
#line 1218 "../../cobc/ppparse.y"
  {
	ppp_error_invalid_option ("SOURCE", (yyvsp[0].s));
	YYERROR;
  }
#line 3051 "ppparse.c"
    break;

  case 88: /* _literal: %empty  */
#line 1225 "../../cobc/ppparse.y"
                { (yyval.s) = NULL; }
#line 3057 "ppparse.c"
    break;

  case 90: /* define_directive: "Variable" _as "Literal" _override  */
#line 1231 "../../cobc/ppparse.y"
  {
	struct cb_define_struct	*p;

	p = ppp_define_add (ppp_setvar_list, (yyvsp[-3].s), (yyvsp[-1].s), (yyvsp[0].ui));
	if (p) {
		ppp_setvar_list = p;
	}
  }
#line 3070 "ppparse.c"
    break;

  case 91: /* define_directive: "Variable" _as PARAMETER _override  */
#line 1240 "../../cobc/ppparse.y"
  {
	char			*s;
	char			*q;
	struct cb_define_struct	*p;

	s = getenv ((yyvsp[-3].s));
	q = NULL;
	if (s && *s && *s != ' ') {
		if (*s == '"' || *s == '\'') {
			const size_t	size = strlen (s) - 1U;
			/* Ignore if improperly quoted */
			if (s[0] == s[size]) {
				q = s;
			}
		} else {
			if (ppp_check_needs_quote (s)) {
				/* Alphanumeric literal */
				q = cobc_plex_malloc (strlen (s) + 4U);
				sprintf (q, "'%s'", s);
			} else {
				/* Numeric literal */
				q = s;
			}
		}
	}
	if (q) {
		p = ppp_define_add (ppp_setvar_list, (yyvsp[-3].s), q, (yyvsp[0].ui));
		if (p) {
			ppp_setvar_list = p;
		}
	}
  }
#line 3107 "ppparse.c"
    break;

  case 92: /* define_directive: "Variable" _as OFF  */
#line 1273 "../../cobc/ppparse.y"
  {
	ppp_define_del ((yyvsp[-2].s));
  }
#line 3115 "ppparse.c"
    break;

  case 93: /* define_directive: CONSTANT "Variable" _as "Literal" _override  */
#line 1277 "../../cobc/ppparse.y"
  {
  /* OpenCOBOL/GnuCOBOL 2.0 extension: MF $SET CONSTANT in 2002+ style as
     >> DEFINE CONSTANT var [AS] literal  archaic extension:
     use plain  >> DEFINE var [AS] literal  for conditional compilation and
     use        01 CONSTANT with/without FROM clause  for constant definitions */
	struct cb_define_struct	*p;

	if (cb_verify (cb_define_constant_directive, ">> DEFINE CONSTANT var")) {
		p = ppp_define_add (ppp_setvar_list, (yyvsp[-3].s), (yyvsp[-1].s), (yyvsp[0].ui));
		if (p) {
			ppp_setvar_list = p;
			fprintf (ppout, "#DEFLIT %s %s%s\n", (yyvsp[-3].s), (yyvsp[-1].s), (yyvsp[0].ui) ? " OVERRIDE" : "");
		}
	}
  }
#line 3135 "ppparse.c"
    break;

  case 94: /* define_directive: variable_or_literal  */
#line 1293 "../../cobc/ppparse.y"
  {
	cb_error (_("invalid %s directive"), "DEFINE/SET");
  }
#line 3143 "ppparse.c"
    break;

  case 95: /* cobol_words_directive: EQUATE alnum_with_list  */
#line 1300 "../../cobc/ppparse.y"
  {
	struct cb_text_list* l;
	/* GC-Extension: standard has only one literal combination here */
	for (l = (yyvsp[0].l); l; l = l->next->next) {
		fprintf (ppout, "#ADDSYN-STD %s %s\n", l->text, l->next->text);
	}
  }
#line 3155 "ppparse.c"
    break;

  case 96: /* cobol_words_directive: UNDEFINE alnum_list  */
#line 1308 "../../cobc/ppparse.y"
  {
	struct cb_text_list	*l;
	for (l = (yyvsp[0].l); l; l = l->next) {
		fprintf (ppout, "#REMOVE-STD %s\n", l->text);
	}
  }
#line 3166 "ppparse.c"
    break;

  case 97: /* cobol_words_directive: SUBSTITUTE alnum_by_list  */
#line 1315 "../../cobc/ppparse.y"
  {
	struct cb_text_list* l;
	/* GC-Extension: standard has only one literal combination here */
	for (l = (yyvsp[0].l); l; l = l->next->next) {
		fprintf (ppout, "#OVERRIDE-STD %s %s\n", l->text, l->next->text);
	}
  }
#line 3178 "ppparse.c"
    break;

  case 98: /* cobol_words_directive: RESERVE alnum_list  */
#line 1323 "../../cobc/ppparse.y"
  {
	struct cb_text_list	*l;
	for (l = (yyvsp[0].l); l; l = l->next) {
		fprintf (ppout, "#ADDRSV %s\n", l->text);
	}
  }
#line 3189 "ppparse.c"
    break;

  case 115: /* leap_second_directive: ON  */
#line 1365 "../../cobc/ppparse.y"
  {
	CB_PENDING (_("LEAP-SECOND ON directive"));
  }
#line 3197 "ppparse.c"
    break;

  case 117: /* turn_directive: ec_list CHECKING on_or_off  */
#line 1373 "../../cobc/ppparse.y"
  {
	append_to_turn_list ((yyvsp[-2].l), !!(yyvsp[0].ui), (yyvsp[0].ui) == 2U);
  }
#line 3205 "ppparse.c"
    break;

  case 118: /* ec_list: "Variable"  */
#line 1380 "../../cobc/ppparse.y"
  {
	(yyval.l) = ppp_list_add (NULL, (yyvsp[0].s));
  }
#line 3213 "ppparse.c"
    break;

  case 119: /* ec_list: ec_list "Variable"  */
#line 1384 "../../cobc/ppparse.y"
  {
	(yyval.l) = ppp_list_add ((yyvsp[-1].l), (yyvsp[0].s));
  }
#line 3221 "ppparse.c"
    break;

  case 120: /* on_or_off: on_with_loc  */
#line 1391 "../../cobc/ppparse.y"
  {
	(yyval.ui) = 2U;
  }
#line 3229 "ppparse.c"
    break;

  case 121: /* on_or_off: ON  */
#line 1395 "../../cobc/ppparse.y"
  {
	(yyval.ui) = 1U;
  }
#line 3237 "ppparse.c"
    break;

  case 122: /* on_or_off: OFF  */
#line 1399 "../../cobc/ppparse.y"
  {
	(yyval.ui) = 0;
  }
#line 3245 "ppparse.c"
    break;

  case 129: /* call_choice: COBOL  */
#line 1421 "../../cobc/ppparse.y"
  {
	current_call_convention |= CB_CONV_COBOL;
	current_call_convention &= ~CB_CONV_STDCALL;
  }
#line 3254 "ppparse.c"
    break;

  case 130: /* call_choice: "EXTERN"  */
#line 1426 "../../cobc/ppparse.y"
  {
	current_call_convention &= ~CB_CONV_STDCALL;
	current_call_convention &= ~CB_CONV_COBOL;
  }
#line 3263 "ppparse.c"
    break;

  case 131: /* call_choice: STDCALL  */
#line 1431 "../../cobc/ppparse.y"
  {
	current_call_convention |= CB_CONV_STDCALL;
	current_call_convention &= ~CB_CONV_COBOL;
  }
#line 3272 "ppparse.c"
    break;

  case 132: /* call_choice: STATIC  */
#line 1436 "../../cobc/ppparse.y"
  {
	current_call_convention |= CB_CONV_STATIC_LINK;
  }
#line 3280 "ppparse.c"
    break;

  case 133: /* if_directive: "Variable" _is _not DEFINED  */
#line 1443 "../../cobc/ppparse.y"
  {
	unsigned int		found;

	found = (ppp_search_lists ((yyvsp[-3].s)) != NULL);
	plex_action_directive (current_cmd, found ^ (yyvsp[-1].ui));
  }
#line 3291 "ppparse.c"
    break;

  case 134: /* if_directive: "Variable" _is _not SET  */
#line 1450 "../../cobc/ppparse.y"
  {
	unsigned int		found;

	found = ppp_search_comp_vars ((yyvsp[-3].s));
	plex_action_directive (current_cmd, found ^ (yyvsp[-1].ui));
  }
#line 3302 "ppparse.c"
    break;

  case 135: /* if_directive: "Variable" _is _not condition_clause object_id  */
#line 1457 "../../cobc/ppparse.y"
  {
	struct cb_define_struct	*p;
	unsigned int		found;

	found = 0;
	p = ppp_search_lists ((yyvsp[-4].s));
	found = ppp_compare_vals (p, (yyvsp[0].ds), (yyvsp[-1].ui));
	plex_action_directive (current_cmd, found ^ (yyvsp[-2].ui));
  }
#line 3316 "ppparse.c"
    break;

  case 136: /* if_directive: "Literal" _is _not condition_clause object_id  */
#line 1467 "../../cobc/ppparse.y"
  {
	struct cb_define_struct	*p;
	unsigned int		found;

	found = 0;
	p = cobc_plex_malloc (sizeof (struct cb_define_struct));
	p->next = NULL;
	if (ppp_set_value (p, (yyvsp[-4].s))) {
		cb_error (_("invalid constant"));
	} else {
		found = ppp_compare_vals (p, (yyvsp[0].ds), (yyvsp[-1].ui));
	}
	plex_action_directive (current_cmd, found ^ (yyvsp[-2].ui));
  }
#line 3335 "ppparse.c"
    break;

  case 137: /* if_directive: garbage  */
#line 1482 "../../cobc/ppparse.y"
  {
	plex_action_directive (current_cmd, 0);
	YYERROR;
  }
#line 3344 "ppparse.c"
    break;

  case 143: /* object_id: "Literal"  */
#line 1501 "../../cobc/ppparse.y"
  {
	struct cb_define_struct	*p;

	p = cobc_plex_malloc (sizeof (struct cb_define_struct));
	p->next = NULL;
	if (ppp_set_value (p, (yyvsp[0].s))) {
		cb_error (_("invalid constant"));
		(yyval.ds) = NULL;
	} else {
		(yyval.ds) = p;
	}
  }
#line 3361 "ppparse.c"
    break;

  case 144: /* object_id: "Variable"  */
#line 1514 "../../cobc/ppparse.y"
  {
	struct cb_define_struct	*p;

	p = ppp_search_lists ((yyvsp[0].s));
	if (p != NULL && p->deftype != PLEX_DEF_NONE) {
		(yyval.ds) = p;
	} else {
		(yyval.ds) = NULL;
	}
  }
#line 3376 "ppparse.c"
    break;

  case 145: /* condition_clause: GREATER _than OR EQUAL _to  */
#line 1528 "../../cobc/ppparse.y"
  {
	(yyval.ui) = COND_GE;
  }
#line 3384 "ppparse.c"
    break;

  case 146: /* condition_clause: GREATER _than  */
#line 1532 "../../cobc/ppparse.y"
  {
	(yyval.ui) = COND_GT;
  }
#line 3392 "ppparse.c"
    break;

  case 147: /* condition_clause: LESS _than OR EQUAL _to  */
#line 1536 "../../cobc/ppparse.y"
  {
	(yyval.ui) = COND_LE;
  }
#line 3400 "ppparse.c"
    break;

  case 148: /* condition_clause: LESS _than  */
#line 1540 "../../cobc/ppparse.y"
  {
	(yyval.ui) = COND_LT;
  }
#line 3408 "ppparse.c"
    break;

  case 149: /* condition_clause: EQUAL _to  */
#line 1544 "../../cobc/ppparse.y"
  {
	(yyval.ui) = COND_EQ;
  }
#line 3416 "ppparse.c"
    break;

  case 150: /* condition_clause: ">="  */
#line 1548 "../../cobc/ppparse.y"
  {
	(yyval.ui) = COND_GE;
  }
#line 3424 "ppparse.c"
    break;

  case 151: /* condition_clause: ">"  */
#line 1552 "../../cobc/ppparse.y"
  {
	(yyval.ui) = COND_GT;
  }
#line 3432 "ppparse.c"
    break;

  case 152: /* condition_clause: "<="  */
#line 1556 "../../cobc/ppparse.y"
  {
	(yyval.ui) = COND_LE;
  }
#line 3440 "ppparse.c"
    break;

  case 153: /* condition_clause: "<"  */
#line 1560 "../../cobc/ppparse.y"
  {
	(yyval.ui) = COND_LT;
  }
#line 3448 "ppparse.c"
    break;

  case 154: /* condition_clause: "="  */
#line 1564 "../../cobc/ppparse.y"
  {
	(yyval.ui) = COND_EQ;
  }
#line 3456 "ppparse.c"
    break;

  case 155: /* condition_clause: "<>"  */
#line 1568 "../../cobc/ppparse.y"
  {
	(yyval.ui) = COND_NE;
  }
#line 3464 "ppparse.c"
    break;

  case 156: /* copy_statement: COPY copy_source _copy_in _copy_suppress _copy_replacing "."  */
#line 1575 "../../cobc/ppparse.y"
  {
	fputc ('\n', ppout);
	ppcopy ((yyvsp[-4].s), (yyvsp[-3].s), (yyvsp[-1].r));
  }
#line 3473 "ppparse.c"
    break;

  case 157: /* copy_statement: COPY error "."  */
#line 1580 "../../cobc/ppparse.y"
  {
	yyerrok;
  }
#line 3481 "ppparse.c"
    break;

  case 158: /* copy_source: "Word or Literal"  */
#line 1587 "../../cobc/ppparse.y"
  {
	(yyval.s) = fix_filename ((yyvsp[0].s));
	if (cb_fold_copy == COB_FOLD_LOWER) {
		(yyval.s) = fold_lower ((yyval.s));
	} else if (cb_fold_copy == COB_FOLD_UPPER) {
		(yyval.s) = fold_upper ((yyval.s));
	}
  }
#line 3494 "ppparse.c"
    break;

  case 159: /* copy_source: "Text-Name"  */
#line 1596 "../../cobc/ppparse.y"
  {
	(yyval.s) = (yyvsp[0].s);
	if (cb_fold_copy == COB_FOLD_LOWER) {
		(yyval.s) = fold_lower ((yyval.s));
	} else {
		(yyval.s) = fold_upper ((yyval.s));
	}
  }
#line 3507 "ppparse.c"
    break;

  case 160: /* _copy_in: %empty  */
#line 1608 "../../cobc/ppparse.y"
  {
	(yyval.s) = NULL;
  }
#line 3515 "ppparse.c"
    break;

  case 161: /* _copy_in: in_or_of copy_source  */
#line 1612 "../../cobc/ppparse.y"
  {
	(yyval.s) = (yyvsp[0].s);
  }
#line 3523 "ppparse.c"
    break;

  case 166: /* _copy_replacing: %empty  */
#line 1629 "../../cobc/ppparse.y"
  {
	(yyval.r) = NULL;
  }
#line 3531 "ppparse.c"
    break;

  case 167: /* _copy_replacing: REPLACING replacing_list  */
#line 1633 "../../cobc/ppparse.y"
  {
	(yyval.r) = (yyvsp[0].r);
  }
#line 3539 "ppparse.c"
    break;

  case 169: /* replace_statement_with_dot: replace_statement error "."  */
#line 1641 "../../cobc/ppparse.y"
  {
	yyerrok;
  }
#line 3547 "ppparse.c"
    break;

  case 170: /* replace_statement: REPLACE _also replacing_list  */
#line 1648 "../../cobc/ppparse.y"
  {
	cb_set_replace_list ((yyvsp[0].r), (yyvsp[-1].ui));
  }
#line 3555 "ppparse.c"
    break;

  case 171: /* replace_statement: REPLACE _last OFF  */
#line 1652 "../../cobc/ppparse.y"
  {
	cb_set_replace_list (NULL, (yyvsp[-1].ui));
  }
#line 3563 "ppparse.c"
    break;

  case 172: /* replacing_list: text_src BY text_dst  */
#line 1659 "../../cobc/ppparse.y"
  {
	(yyval.r) = ppp_replace_list_add (NULL, (yyvsp[-2].p), (yyvsp[0].l), 0);
  }
#line 3571 "ppparse.c"
    break;

  case 173: /* replacing_list: lead_trail text_partial_src BY text_partial_dst  */
#line 1663 "../../cobc/ppparse.y"
  {
	(yyval.r) = ppp_replace_list_add (NULL, (yyvsp[-2].p), (yyvsp[0].l), (yyvsp[-3].ui));
  }
#line 3579 "ppparse.c"
    break;

  case 174: /* replacing_list: replacing_list text_src BY text_dst  */
#line 1667 "../../cobc/ppparse.y"
  {
	(yyval.r) = ppp_replace_list_add ((yyvsp[-3].r), (yyvsp[-2].p), (yyvsp[0].l), 0);
  }
#line 3587 "ppparse.c"
    break;

  case 175: /* replacing_list: replacing_list lead_trail text_partial_src BY text_partial_dst  */
#line 1671 "../../cobc/ppparse.y"
  {
	(yyval.r) = ppp_replace_list_add ((yyvsp[-4].r), (yyvsp[-2].p), (yyvsp[0].l), (yyvsp[-3].ui));
  }
#line 3595 "ppparse.c"
    break;

  case 176: /* text_src: "==" token_list "=="  */
#line 1678 "../../cobc/ppparse.y"
  {
	(yyval.p) = ppp_replace_src ((yyvsp[-1].l), 0);
  }
#line 3603 "ppparse.c"
    break;

  case 177: /* text_src: identifier  */
#line 1682 "../../cobc/ppparse.y"
  {
	(yyval.p) = ppp_replace_src ((yyvsp[0].l), 0);
/* CHECKME later (parser conflict)
  }
| IN
  {
	/ * as we need this word, which is valid as replacement,
	   also for qualification, we need to explicit make it
	   a word if given alone * /
	$$ = ppp_list_add (NULL, "IN");
	$$ = ppp_replace_src ($$, 0);
  }
| OF
  {
	/ * as we need this word, which is valid as replacement,
	   also for qualification, we need to explicit make it
	   a word if given alone * /
	$$ = ppp_list_add (NULL, "OF");
	$$ = ppp_replace_src ($$, 0);
*/
  }
#line 3629 "ppparse.c"
    break;

  case 178: /* text_dst: "==" "=="  */
#line 1707 "../../cobc/ppparse.y"
  {
	(yyval.l) = NULL;
  }
#line 3637 "ppparse.c"
    break;

  case 179: /* text_dst: "==" token_list "=="  */
#line 1711 "../../cobc/ppparse.y"
  {
	(yyval.l) = (yyvsp[-1].l);
  }
#line 3645 "ppparse.c"
    break;

  case 180: /* text_dst: identifier  */
#line 1715 "../../cobc/ppparse.y"
  {
	(yyval.l) = (yyvsp[0].l);
  }
#line 3653 "ppparse.c"
    break;

  case 181: /* text_dst: IN  */
#line 1719 "../../cobc/ppparse.y"
  {
	/* as we need this word, which is valid as replacement,
	   also for qualification, we need to explicit make it
	   a word if given alone */
	(yyval.l) = ppp_list_add (NULL, "IN");
  }
#line 3664 "ppparse.c"
    break;

  case 182: /* text_dst: OF  */
#line 1726 "../../cobc/ppparse.y"
  {
	/* as we need this word, which is valid as replacement,
	   also for qualification, we need to explicit make it
	   a word if given alone */
	(yyval.l) = ppp_list_add (NULL, "OF");
  }
#line 3675 "ppparse.c"
    break;

  case 183: /* text_partial_src: "==" "Word or Literal" "=="  */
#line 1736 "../../cobc/ppparse.y"
  {
	(yyval.p) = ppp_replace_src (ppp_list_add (NULL, (yyvsp[-1].s)), 0);
  }
#line 3683 "ppparse.c"
    break;

  case 184: /* text_partial_src: "Word or Literal"  */
#line 1740 "../../cobc/ppparse.y"
  {
	(yyval.p) = ppp_replace_src (ppp_list_add (NULL, literal_token ((yyvsp[0].s), 0)),
			      ((yyvsp[0].s)[0] == '\'' || (yyvsp[0].s)[0] == '"'));
  }
#line 3692 "ppparse.c"
    break;

  case 185: /* text_partial_dst: "==" "=="  */
#line 1748 "../../cobc/ppparse.y"
  {
	(yyval.l) = NULL;
  }
#line 3700 "ppparse.c"
    break;

  case 186: /* text_partial_dst: "==" "Word or Literal" "=="  */
#line 1752 "../../cobc/ppparse.y"
  {
	(yyval.l) = ppp_list_add (NULL, (yyvsp[-1].s));
  }
#line 3708 "ppparse.c"
    break;

  case 187: /* text_partial_dst: "Word or Literal"  */
#line 1756 "../../cobc/ppparse.y"
  {
	(yyval.l) = ppp_list_add (NULL, literal_token ((yyvsp[0].s), 1));
  }
#line 3716 "ppparse.c"
    break;

  case 188: /* token_list: "Word or Literal"  */
#line 1763 "../../cobc/ppparse.y"
  {
	(yyval.l) = ppp_list_add (NULL, (yyvsp[0].s));
  }
#line 3724 "ppparse.c"
    break;

  case 189: /* token_list: token_list "Word or Literal"  */
#line 1767 "../../cobc/ppparse.y"
  {
	(yyval.l) = ppp_list_add ((yyvsp[-1].l), (yyvsp[0].s));
  }
#line 3732 "ppparse.c"
    break;

  case 190: /* identifier: "Word or Literal"  */
#line 1774 "../../cobc/ppparse.y"
  {
	(yyval.l) = ppp_list_add (NULL, (yyvsp[0].s));
  }
#line 3740 "ppparse.c"
    break;

  case 191: /* identifier: identifier IN "Word or Literal"  */
#line 1778 "../../cobc/ppparse.y"
  {
	(yyval.l) = ppp_list_add ((yyvsp[-2].l), " ");
	(yyval.l) = ppp_list_add ((yyval.l), "IN");
	(yyval.l) = ppp_list_add ((yyval.l), " ");
	(yyval.l) = ppp_list_add ((yyval.l), (yyvsp[0].s));
  }
#line 3751 "ppparse.c"
    break;

  case 192: /* identifier: identifier OF "Word or Literal"  */
#line 1785 "../../cobc/ppparse.y"
  {
	(yyval.l) = ppp_list_add ((yyvsp[-2].l), " ");
	(yyval.l) = ppp_list_add ((yyval.l), "OF");
	(yyval.l) = ppp_list_add ((yyval.l), " ");
	(yyval.l) = ppp_list_add ((yyval.l), (yyvsp[0].s));
  }
#line 3762 "ppparse.c"
    break;

  case 193: /* identifier: identifier '(' subscripts ')'  */
#line 1792 "../../cobc/ppparse.y"
  {
	struct cb_text_list *l;

	(yyval.l) = ppp_list_add ((yyvsp[-3].l), " ");
	(yyval.l) = ppp_list_add ((yyval.l), "(");
	(yyvsp[-1].l) = ppp_list_add ((yyvsp[-1].l), ")");
	for (l = (yyval.l); l->next; l = l->next) {
		;
	}
	l->next = (yyvsp[-1].l);
  }
#line 3778 "ppparse.c"
    break;

  case 194: /* subscripts: "Word or Literal"  */
#line 1807 "../../cobc/ppparse.y"
  {
	(yyval.l) = ppp_list_add (NULL, (yyvsp[0].s));
  }
#line 3786 "ppparse.c"
    break;

  case 195: /* subscripts: subscripts "Word or Literal"  */
#line 1811 "../../cobc/ppparse.y"
  {
	(yyval.l) = ppp_list_add ((yyvsp[-1].l), " ");
	(yyval.l) = ppp_list_add ((yyval.l), (yyvsp[0].s));
  }
#line 3795 "ppparse.c"
    break;

  case 196: /* lead_trail: LEADING  */
#line 1819 "../../cobc/ppparse.y"
  {
	(yyval.ui) = CB_REPLACE_LEADING;
  }
#line 3803 "ppparse.c"
    break;

  case 197: /* lead_trail: TRAILING  */
#line 1823 "../../cobc/ppparse.y"
  {
	(yyval.ui) = CB_REPLACE_TRAILING;
  }
#line 3811 "ppparse.c"
    break;

  case 198: /* unquoted_literal: "Literal"  */
#line 1830 "../../cobc/ppparse.y"
  {
	/* Do not reuse unquote as some literals here may be delimited with
	   parentheses */
	char	*p = (yyvsp[0].s);
	size_t	size;

	/* Remove surrounding quotes/brackets */
	++p;
	size = strlen (p) - 1;
	p[size] = '\0';

	(yyval.s) = p;
  }
#line 3829 "ppparse.c"
    break;

  case 199: /* _override: %empty  */
#line 1849 "../../cobc/ppparse.y"
  {
	(yyval.ui) = 0;
  }
#line 3837 "ppparse.c"
    break;

  case 200: /* _override: OVERRIDE  */
#line 1853 "../../cobc/ppparse.y"
  {
	(yyval.ui) = 1U;
  }
#line 3845 "ppparse.c"
    break;

  case 201: /* _not: %empty  */
#line 1860 "../../cobc/ppparse.y"
  {
	(yyval.ui) = 0;
  }
#line 3853 "ppparse.c"
    break;

  case 202: /* _not: NOT  */
#line 1864 "../../cobc/ppparse.y"
  {
	(yyval.ui) = 1U;
  }
#line 3861 "ppparse.c"
    break;

  case 203: /* _also: %empty  */
#line 1871 "../../cobc/ppparse.y"
  {
	(yyval.ui) = 0;
  }
#line 3869 "ppparse.c"
    break;

  case 204: /* _also: ALSO  */
#line 1875 "../../cobc/ppparse.y"
  {
	(yyval.ui) = 1U;
  }
#line 3877 "ppparse.c"
    break;

  case 205: /* _last: %empty  */
#line 1882 "../../cobc/ppparse.y"
  {
	(yyval.ui) = 0;
  }
#line 3885 "ppparse.c"
    break;

  case 206: /* _last: LAST  */
#line 1886 "../../cobc/ppparse.y"
  {
	(yyval.ui) = 1U;
  }
#line 3893 "ppparse.c"
    break;


#line 3897 "ppparse.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      {
        yypcontext_t yyctx
          = {yyssp, yytoken};
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == -1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *,
                             YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (yymsg)
              {
                yysyntax_error_status
                  = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
                yymsgp = yymsg;
              }
            else
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = YYENOMEM;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == YYENOMEM)
          YYNOMEM;
      }
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= TOKEN_EOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == TOKEN_EOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
  return yyresult;
}

#line 1899 "../../cobc/ppparse.y"


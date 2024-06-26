/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

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

#line 178 "ppparse.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE pplval;


int ppparse (void);


#endif /* !YY_PP_PPPARSE_H_INCLUDED  */

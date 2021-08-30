/*
   Copyright (C) 2010-2012, 2014-2016, 2018-2020 Free Software Foundation, Inc.
   Modified for use in GnuCOBOL by Roger While, Simon Sobisch
*/

/* Getopt for GNU.
   Copyright (C) 1987-2020 Free Software Foundation, Inc.
   This file was originally part of the GNU C Library and part of gnulib.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <https://www.gnu.org/licenses/>.  */

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <ctype.h>

#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#endif

/* This implementation of 'getopt' has three modes for handling
   options interspersed with non-option arguments.  It can stop
   scanning for options at the first non-option argument encountered,
   as POSIX specifies.  It can continue scanning for options after the
   first non-option argument, but permute 'argv' as it goes so that,
   after 'getopt' is done, all the options precede all the non-option
   arguments and 'optind' points to the first non-option argument.
   Or, it can report non-option arguments as if they were arguments to
   the option character '\x01'.

   The default behavior of 'cobgetopt_long' is to permute the argument list.
   The default behavior of 'getopt' is to stop at the first non-option argument.
   Setting the environment variable POSIXLY_CORRECT to any value
   disables permutation.

   If the first character of the OPTSTRING argument to 'getopt' or
   'getopt_long' is '+', both functions will stop at the first
   non-option argument.  If it is '-', both functions will report
   non-option arguments as arguments to the option character '\x01'.  */

#ifdef	ENABLE_NLS
#include "lib/gettext.h"
#define _(msgid)		gettext(msgid)
#define N_(msgid)		gettext_noop(msgid)
#else
#define _(msgid)		msgid
#define N_(msgid)		msgid
#endif

#ifndef HAVE_FLOCKFILE
#define flockfile(x)
#define funlockfile(x)
#endif

   /* Force symbol exports */
#define	COB_LIB_EXPIMP
#include "libcob.h"

#include "cobgetopt.h"

/* internal definition start */

/* Formerly, initialization of getopt depended on optind==0, which
   causes problems with re-calling getopt as programs generally don't
   know that. */

static int getopt_initialized = 0;

/* The next char to be scanned in the option-element
   in which the last option character we returned was found.
   This allows us to pick up the scan where we left off.

   If this is zero, or a null string, it means resume the scan
   by advancing to the next ARGV-element.  */

static char *nextchar = NULL;

/* Describe how to deal with options that follow non-option ARGV-elements.

   If the caller did not specify anything,
   the default is REQUIRE_ORDER if the environment variable
   POSIXLY_CORRECT is defined, PERMUTE otherwise.

   REQUIRE_ORDER means don't recognize them as options;
   stop option processing when the first non-option is seen.
   This is what Unix does.
   This mode of operation is selected by either setting the environment
   variable POSIXLY_CORRECT, or using '+' as the first character
   of the list of option characters.

   PERMUTE is the default.  We permute the contents of ARGV as we scan,
   so that eventually all the non-options are at the end.  This allows options
   to be given in any order, even with programs that were not written to
   expect this.

   RETURN_IN_ORDER is an option available to programs that were written
   to expect options and other ARGV-elements in any order and that care about
   the ordering of the two.  We describe each non-option ARGV-element
   as if it were the argument of an option with character code 1.
   Using '-' as the first character of the list of option characters
   selects this mode of operation.

   The special argument '--' forces an end of option-scanning regardless
   of the value of 'ordering'.  In the case of RETURN_IN_ORDER, only
   '--' can cause 'getopt' to return -1 with 'optind' != ARGC.  */

static enum {
	REQUIRE_ORDER, PERMUTE, RETURN_IN_ORDER
} ordering;

/* Handle permutation of arguments.  */

/* Describe the part of ARGV that contains non-options that have
   been skipped.  'first_nonopt' is the index in ARGV of the first of them;
   'last_nonopt' is the index after the last of them.  */

static int first_nonopt;
static int last_nonopt;

/* internal definition end */

/* For communication from 'getopt' to the caller.
   When 'getopt' finds an option that takes an argument,
   the argument value is returned here.
   Also, when 'ordering' is RETURN_IN_ORDER,
   each non-option ARGV-element is returned here.  */

#define optarg cob_optarg
char *optarg;

/* Index in ARGV of the next element to be scanned.
   This is used for communication to and from the caller
   and for communication between successive calls to 'getopt'.

   On entry to 'getopt', zero means this is the first call; initialize.

   When 'getopt' returns -1, this is the index of the first of the
   non-option elements that the caller should itself scan.

   Otherwise, 'optind' communicates from one call to the next
   how much of ARGV has been scanned so far.  */

/* 1003.2 says this must be 1 before any call.  */
#define optind cob_optind
int optind = 1;

/* Callers store zero here to inhibit the error message
   for unrecognized options.  */

#define opterr cob_opterr
int opterr = 1;

/* Set to an option character which was unrecognized.
   This must be initialized on some systems to avoid linking in the
   system's own getopt implementation.  */

#define optopt cob_optopt
int optopt = '?';

/* Exchange two adjacent subsequences of ARGV.
   One subsequence is elements [first_nonopt,last_nonopt)
   which contains all the non-options that have been skipped so far.
   The other is elements [last_nonopt,optind), which contains all
   the options processed since those non-options were skipped.

   'first_nonopt' and 'last_nonopt' are relocated so that they describe
   the new indices of the non-options in ARGV after they are moved.  */

static void
exchange (char **argv)
{
  int bottom = first_nonopt;
  int middle = last_nonopt;
  int top = optind;
  char *tem;

  /* Exchange the shorter segment with the far end of the longer segment.
     That puts the shorter segment into the right place.
     It leaves the longer segment in the right place overall,
     but it consists of two parts that need to be swapped next.  */

  while (top > middle && middle > bottom)
    {
      if (top - middle > middle - bottom)
	{
	  /* Bottom segment is the short one.  */
	  int len = middle - bottom;
	  int i;

	  /* Swap it with the top part of the top segment.  */
	  for (i = 0; i < len; i++)
	    {
	      tem = argv[bottom + i];
	      argv[bottom + i] = argv[top - (middle - bottom) + i];
	      argv[top - (middle - bottom) + i] = tem;
	    }
	  /* Exclude the moved bottom segment from further swapping.  */
	  top -= len;
	}
      else
	{
	  /* Top segment is the short one.  */
	  int len = top - middle;
	  int i;

	  /* Swap it with the bottom part of the bottom segment.  */
	  for (i = 0; i < len; i++)
	    {
	      tem = argv[bottom + i];
	      argv[bottom + i] = argv[middle + i];
	      argv[middle + i] = tem;
	    }
	  /* Exclude the moved top segment from further swapping.  */
	  bottom += len;
	}
    }

  /* Update records for the slots the non-options now occupy.  */

  first_nonopt += (optind - last_nonopt);
  last_nonopt = optind;
}

/* Process the argument starting with nextchar as a long option.
   optind should *not* have been advanced over this argument.

   If the value returned is -1, it was not actually a long option, the
   state is unchanged, and the argument should be processed as a set
   of short options (this can only happen when long_only is true).
   Otherwise, the option (and its argument, if any) have been consumed
   and the return value is the value to return from _getopt_internal_r.  */
static int
process_long_option (const int argc, char * const *argv, const char *optstring,
		     const struct option *longopts, int *longind,
		     const int long_only,
		     int print_errors, const char *prefix)
{
  char *nameend;
  size_t namelen;
  const struct option *p;
  const struct option *pfound = NULL;
  int n_options;
  int option_index = 0;

  for (nameend = nextchar; *nameend && *nameend != '='; nameend++)
   /* Do nothing.  */ ;
  namelen = nameend - nextchar;

  /* First look for an exact match, counting the options as a side
     effect.  */
  for (p = longopts, n_options = 0; p->name; p++, n_options++)
    if (!strncmp (p->name, nextchar, namelen)
	&& namelen == strlen (p->name))
      {
	/* Exact match found.  */
	pfound = p;
	option_index = n_options;
	break;
      }

  if (pfound == NULL)
    {
      /* Didn't find an exact match, so look for abbreviations.  */
      unsigned char *ambig_set = NULL;
      int ambig_malloced = 0;
      int ambig_fallback = 0;
      int indfound = -1;

      for (p = longopts, option_index = 0; p->name; p++, option_index++)
	if (!strncmp (p->name, nextchar, namelen))
	  {
	    if (pfound == NULL)
	      {
		/* First nonexact match found.  */
		pfound = p;
		indfound = option_index;
	      }
	    else if (long_only
		     || pfound->has_arg != p->has_arg
		     || pfound->flag != p->flag
		     || pfound->val != p->val)
	      {
		/* Second or later nonexact match found.  */
		if (!ambig_fallback)
		  {
		    if (!print_errors)
		      /* Don't waste effort tracking the ambig set if
			 we're not going to print it anyway.  */
		      ambig_fallback = 1;
		    else if (!ambig_set)
		      {
			/* note: we explicit don't want to use cob_malloc here as
			         that halts if the memory is not available */
			if ((ambig_set = calloc ((size_t)1, n_options)) == NULL)
			  /* Fall back to simpler error message.  */
			  ambig_fallback = 1;
			else
			  ambig_malloced = 1;

			if (ambig_set)
			    ambig_set[indfound] = 1;
		      }
		    if (ambig_set)
		      ambig_set[option_index] = 1;
		  }
	      }
	  }

      if (ambig_set || ambig_fallback)
	{
	  if (print_errors)
	    {
	      if (ambig_fallback)
		  {
		  fprintf (stderr, _("%s: option '%s%s' is ambiguous"),
			 argv[0], prefix, nextchar);
		  fputc ('\n', stderr);
		}
	      else
		{
		  flockfile (stderr);
		  fprintf (stderr,
			   _("%s: option '%s%s' is ambiguous; possibilities:"),
			   argv[0], prefix, nextchar);

		  for (option_index = 0; option_index < n_options; option_index++)
		    if (ambig_set[option_index])
		      fprintf (stderr, " '%s%s'",
			       prefix, longopts[option_index].name);

		  fputc ('\n', stderr);
		  funlockfile (stderr);
		}
	    }
	  if (ambig_malloced)
	    free (ambig_set);	/* no cob_malloc, no cob_free */
	  nextchar += strlen (nextchar);
	  optind++;
	  optopt = 0;
	  return '?';
	}

      option_index = indfound;
    }

  if (pfound == NULL)
    {
      /* Can't find it as a long option.  If this is not getopt_long_only,
	 or the option starts with '--' or is not a valid short option,
	 then it's an error.  */
      if (!long_only || argv[optind][1] == '-'
	  || strchr (optstring, *nextchar) == NULL)
	{
	  if (print_errors)
	  {
		  fprintf (stderr, _("%s: unrecognized option '%s%s'"),
		     argv[0], prefix, nextchar);
		  fputc ('\n', stderr);
	  }

	  nextchar = NULL;
	  optind++;
	  optopt = 0;
	  return '?';
	}

      /* Otherwise interpret it as a short option.  */
      return -1;
    }

  /* We have found a matching long option.  Consume it.  */
  optind++;
  nextchar = NULL;
  if (*nameend)
    {
      /* Don't test has_arg with >, because some C compilers don't
	 allow it to be used on enums.  */
      if (pfound->has_arg)
	optarg = nameend + 1;
      else
	{
	  if (print_errors)
		  {
		  fprintf (stderr,
		     _("%s: option '%s%s' doesn't allow an argument"),
		     argv[0], prefix, pfound->name);
		  fputc ('\n', stderr);
		  }

	  optopt = pfound->val;
	  return '?';
	}
    }
  else if (pfound->has_arg == 1)
    {
      if (optind < argc)
	optarg = argv[optind++];
      else
	{
	  if (print_errors)
		  {
		    fprintf (stderr,
		     _("%s: option '%s%s' requires an argument"),
		     argv[0], prefix, pfound->name);
		  fputc ('\n', stderr);
		  }

	  optopt = pfound->val;
	  return optstring[0] == ':' ? ':' : '?';
	}
    }

  if (longind != NULL)
    *longind = option_index;
  if (pfound->flag)
    {
      *(pfound->flag) = pfound->val;
      return 0;
    }
  return pfound->val;
}

/* Initialize internal data upon the first call to getopt.  */

static const char *
_getopt_initialize (const char *optstring)
{
#if 1 /* CHECKME: only reasonable when static libraries are built */
  /* minimal initialization of the environment like binding textdomain,
     allowing test to be run under WIN32 (implied in cob_init(),
     no need to call outside of GnuCOBOL); added here as static libcob
     possibly doesn't have it set otherwise */
  cob_common_init (NULL);
#endif

  /* Start processing options with ARGV-element 1 (since ARGV-element 0
     is the program name); the sequence of previously skipped
     non-option ARGV-elements is empty.  */
  if (optind == 0)
    optind = 1;

  first_nonopt = last_nonopt = optind;
  nextchar = NULL;

  /* Determine how to handle the ordering of options and nonoptions.  */
  if (optstring[0] == '-')
    {
      ordering = RETURN_IN_ORDER;
      ++optstring;
    }
  else if (optstring[0] == '+')
    {
      ordering = REQUIRE_ORDER;
      ++optstring;
    }
  else if (!!getenv ("POSIXLY_CORRECT"))
    ordering = REQUIRE_ORDER;
  else
    ordering = PERMUTE;

  getopt_initialized = 1;
  return optstring;
}

/* Scan elements of ARGV (whose length is ARGC) for option characters
   given in OPTSTRING.

   If an element of ARGV starts with '-', and is not exactly "-" or "--",
   then it is an option element.  The characters of this element
   (aside from the initial '-') are option characters.  If 'getopt'
   is called repeatedly, it returns successively each of the option characters
   from each of the option elements.

   If 'getopt' finds another option character, it returns that character,
   updating 'optind' and 'nextchar' so that the next call to 'getopt' can
   resume the scan with the following option character or ARGV-element.

   If there are no more option characters, 'getopt' returns -1.
   Then 'optind' is the index in ARGV of the first ARGV-element
   that is not an option.  (The ARGV-elements have been permuted
   so that those that are not options now come last.)

   OPTSTRING is a string containing the legitimate option characters.
   If an option character is seen that is not listed in OPTSTRING,
   return '?' after printing an error message.  If you set 'opterr' to
   zero, the error message is suppressed but we still return '?'.

   If a char in OPTSTRING is followed by a colon, that means it wants an arg,
   so the following text in the same ARGV-element, or the text of the following
   ARGV-element, is returned in 'optarg'.  Two colons mean an option that
   wants an optional arg; if there is text in the current ARGV-element,
   it is returned in 'optarg', otherwise 'optarg' is set to zero.

   If OPTSTRING starts with '-' or '+', it requests different methods of
   handling the non-option ARGV-elements.
   See the comments about RETURN_IN_ORDER and REQUIRE_ORDER, above.

   Long-named options begin with '--' instead of '-'.
   Their names may be abbreviated as long as the abbreviation is unique
   or is an exact match for some defined option.  If they have an
   argument, it follows the option name in the same ARGV-element, separated
   from the option name by a '=', or else the in next ARGV-element.
   When 'getopt' finds a long-named option, it returns 0 if that option's
   'flag' field is nonzero, the value of the option's 'val' field
   if the 'flag' field is zero.

   The elements of ARGV aren't really const, because we permute them.
   But we pretend they're const in the prototype to be compatible
   with other systems.

   LONGOPTS is a vector of 'struct option' terminated by an
   element containing a name which is zero.

   LONGIND returns the index in LONGOPT of the long-named option found.
   It is only valid when a long-named option has been found by the most
   recent call.

   If LONG_ONLY is nonzero, '-' as well as '--' can introduce
   long-named options.  */

int
cob_getopt_long_long (const int argc, char *const *argv, const char *optstring,
		      const struct option *longopts, int *longind,
		      const int long_only)
{
  int print_errors = opterr;

  if (argc < 1)
    return -1;

  optarg = NULL;


  if (optind == 0 || !getopt_initialized)
    optstring = _getopt_initialize (optstring);
  else if (optstring[0] == '-' || optstring[0] == '+')
    optstring++;

  if (optstring[0] == ':')
    print_errors = 0;  

  /* Test whether ARGV[optind] points to a non-option argument.  */
#define NONOPTION_P (argv[optind][0] != '-' || argv[optind][1] == '\0')

  if (nextchar == NULL || *nextchar == '\0')
    {
      /* Advance to the next ARGV-element.  */

      /* Give FIRST_NONOPT & LAST_NONOPT rational values if optind has been
	 moved back by the user (who may also have changed the arguments).  */
      if (last_nonopt > optind)
	last_nonopt = optind;
      if (first_nonopt > optind)
	first_nonopt = optind;

      if (ordering == PERMUTE)
	{
	  /* If we have just processed some options following some non-options,
	     exchange them so that the options come first.  */

	  if (first_nonopt != last_nonopt
	      && last_nonopt != optind)
	    exchange ((char **) argv);
	  else if (last_nonopt != optind)
	    first_nonopt = optind;

	  /* Skip any additional non-options
	     and extend the range of non-options previously skipped.  */

	  while (optind < argc && NONOPTION_P)
	    optind++;
	  last_nonopt = optind;
	}

      /* The special ARGV-element '--' means premature end of options.
	 Skip it like a null option,
	 then exchange with previous non-options as if it were an option,
	 then skip everything else like a non-option.  */

      if (optind != argc && !strcmp (argv[optind], "--"))
	{
	  optind++;

	  if (first_nonopt != last_nonopt
	      && last_nonopt != optind)
	    exchange ((char **) argv);
	  else if (first_nonopt == last_nonopt)
	    first_nonopt = optind;
	  last_nonopt = argc;

	  optind = argc;
	}

      /* If we have done all the ARGV-elements, stop the scan
	 and back over any non-options that we skipped and permuted.  */

      if (optind == argc)
	{
	  /* Set the next-arg-index to point at the non-options
	     that we previously skipped, so the caller will digest them.  */
	  if (first_nonopt != last_nonopt)
	    optind = first_nonopt;
	  return -1;
	}

      /* If we have come to a non-option and did not permute it,
	 either stop the scan or describe it to the caller and pass it by.  */

      if (NONOPTION_P)
	{
	  if (ordering == REQUIRE_ORDER)
	    return -1;
	  optarg = argv[optind++];
	  return 1;
	}

      /* We have found another option-ARGV-element.
	 Check whether it might be a long option.  */
      if (longopts)
	{
	  if (argv[optind][1] == '-')
	    {
	      /* "--foo" is always a long option.  The special option
		 "--" was handled above.  */
	      nextchar = argv[optind] + 2;
	      return process_long_option (argc, argv, optstring, longopts,
					  longind, long_only,
					  print_errors, "--");
	    }

	  /* If long_only and the ARGV-element has the form "-f",
	     where f is a valid short option, don't consider it an
	     abbreviated form of a long option that starts with f.
	     Otherwise there would be no way to give the -f short
	     option.

	     On the other hand, if there's a long option "fubar" and
	     the ARGV-element is "-fu", do consider that an
	     abbreviation of the long option, just like "--fu", and
	     not "-f" with arg "u".

	     This distinction seems to be the most useful approach.  */
	  if (long_only && (argv[optind][2]
			    || !strchr (optstring, argv[optind][1])))
	    {
	      int code;
	      nextchar = argv[optind] + 1;
	      code = process_long_option (argc, argv, optstring, longopts,
					  longind, long_only,
					  print_errors, "-");
	      if (code != -1)
		return code;
	    }
	}

      /* It is not a long option.  Skip the initial punctuation.  */
     nextchar = argv[optind] + 1;
    }

  /* Look at and handle the next short option-character.  */

  {
    char c = *nextchar++;
    char *temp = strchr (optstring, c);

    /* Increment 'optind' when we start to process its last character.  */
    if (*nextchar == '\0')
      ++optind;

    if (temp == NULL || c == ':' || c == ';')
      {
	if (print_errors)
	  {
		fprintf (stderr, _("%s: invalid option -- '%c'"), argv[0], c);
		fputc ('\n', stderr);
	  }
	optopt = c;
	return '?';
      }

    /* Convenience. Treat POSIX -W foo same as long option --foo */
    if (temp[0] == 'W' && temp[1] == ';' && longopts != NULL)
      {
	/* This is an option that requires an argument.  */
	if (*nextchar != '\0')
	    optarg = nextchar;
	else if (optind == argc)
	  {
	    if (print_errors)
	      {
	      fprintf (stderr,
		       _("%s: option requires an argument -- '%c'"),
		       argv[0], c);
	      fputc ('\n', stderr);
	      }
	    optopt = c;
	    if (optstring[0] == ':')
	      c = ':';
	    else
	      c = '?';
	    return c;
	  }
	else
	  optarg = argv[optind];

	nextchar = optarg;
	optarg = NULL;
	return process_long_option (argc, argv, optstring, longopts,
					longind, 0 /* long_only */, print_errors, "-W ");
      }
    if (temp[1] == ':')
      {
	if (temp[2] == ':')
	  {
	    /* This is an option that accepts an argument optionally.  */
	    if (*nextchar != '\0')
	      {
		optarg = nextchar;
		optind++;
	      }
	    else
	      optarg = NULL;
	    nextchar = NULL;
	  }
	else
	  {
	    /* This is an option that requires an argument.  */
	    if (*nextchar != '\0')
	      {
		optarg = nextchar;
		/* If we end this ARGV-element by taking the rest as an arg,
		   we must advance to the next element now.  */
		optind++;
	      }
	    else if (optind == argc)
	      {
		if (print_errors)
		  {
		    fprintf (stderr, _("%s: option requires an argument -- '%c'"),
			     argv[0], c);
		     fputc ('\n', stderr);
		  }
		optopt = c;
		if (optstring[0] == ':')
		  c = ':';
		else
		  c = '?';
	      }
	    else
	      /* We already incremented 'optind' once;
		 increment it again when taking next ARGV-elt as argument.  */
	      optarg = argv[optind++];
	    nextchar = NULL;
	  }
      }
    return c;
  }
}


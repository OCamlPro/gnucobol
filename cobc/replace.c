/*
   Copyright (C) 2001-2024 Free Software Foundation, Inc.

   Authors:
   Keisuke Nishida, Roger While, Ron Norman, Simon Sobisch, Brian Tiffin,
   Edward Hart, Dave Pitts, Fabrice Le Fessant

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

#include "tarstamp.h"
#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <string.h>
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#include <time.h>
#include <limits.h>

#include "cobc.h"
#include "tree.h"

/* This is an implementation of the *two* phases of COPY-REPLACING and
   REPLACE on a stream of tokens: the stream of tokens generated by the
   pplex.l/parser.y goes first through COPY-REPLACING replacements,
   and then through REPLACE replacements, as expected by the COBOL
   standard.

   However, it does not fully conform to the standard, as REPLACE are
   parsed on the input stream *before* any COPY-REPLACING could have
   been applied.

   The general entry point is `add_text_to_replace (stream, prequeue,
   token)`, it adds `token` to `stream`, `prequeue` is 1 if the
   token should not be treated immediately (because it may be merged
   with other following tokens if they are of the same kind), 0
   otherwise.

   Initially, `pp_echo()` in `pplex.l` will use
   `cb_ppecho_copy_replace()` to add tokens to the first stream
   `copy_repls` (using `add_text_to_replace`), i.e. the stream of
   `COPY ... REPLACING`.

   Once copy-replacing operations have been performed in this stream,
   `ppecho_replace()` is used to add tokens to the second stream
   `replace_repls` (using again `add_text_to_replace`), i.e. the
   stream of `REPLACE`.

   Once replace operations have been performed on this second stream,
   `cb_ppecho_direct()` (in pplex.l) is used to output the final
   tokens.

   The states of both streams are stored in a struct
   `cb_replacement_state`, and `add_text_to_replace` calls the
   function `do_replace()` to perform the replacement on a given
   stream.
 */

/* Uncomment the following lines to have a trace of replacements.
   It uses macros WITH_DEPTH that adds an additional argument to every
   function to keep the depth of the recursion. */

/* #define DEBUG_REPLACE_TRACE */
/* #define DEBUG_REPLACE */

#ifdef DEBUG_REPLACE_TRACE
#define DEBUG_REPLACE
#endif

/* BEGIN implementation of a queue of text and token pairs. The
   implementation could easily be translated to any other type of
   data.

   Note that there are two synchronized circular buffers for texts and
   tokens. As an optimization, both are allocated in one block, so they
   need to be free also together.
*/

struct cb_token_queue {
	int maxsize;
	int pos;
	int length;
	const char **texts;
	const char **tokens;
};

static struct cb_token_queue *token_queue_new(int initial_size)
{
	struct cb_token_queue *q;

	/* assert (initial_size > 0); */
	q = cobc_malloc (sizeof (struct cb_token_queue));
	q->maxsize = initial_size;
	q->pos = 0;
	q->length = 0;
	q->texts = cobc_malloc (sizeof(char*) * initial_size * 2);
	q->tokens = q->texts + initial_size;
	return q;
}

static void token_queue_put(struct cb_token_queue *q, int strdup,
			    const char *text,
			    const char *token)
{
	int pos;
	if (q->length == q->maxsize) {
		int maxsize = q->maxsize * 2;
		int n = q->maxsize - q->pos;
		const char **p = cobc_malloc (sizeof(char *) * maxsize * 2);
		const char **old_text = q->texts;

		memcpy (p, q->texts + q->pos, sizeof(char *) * n);
		if (q->pos > 0) {
			memcpy (p + n, q->texts,
				sizeof(char *) * ( q->maxsize - n ) );
		}
		q->texts = p;

		p += maxsize;

		memcpy (p, q->tokens + q->pos, sizeof(char *) * n);
		if (q->pos > 0) {
			memcpy (p + n, q->tokens,
				sizeof(char *) * ( q->maxsize - n ) );
		}
		q->tokens = p;

		cobc_free (old_text);
		q->pos = 0;
		q->maxsize = maxsize;
	}
	pos = (q->pos+q->length) % q->maxsize ;
	q->texts[pos] = strdup ? cobc_plex_strdup(text) : text;
	if (token != NULL && strdup){
		token = cobc_plex_strdup (token);
	}
	q->tokens[pos] = token;
	q->length++;
}

static COB_INLINE COB_A_INLINE
int token_queue_is_empty (struct cb_token_queue *q)
{
	return (q->length == 0);
}

static COB_INLINE COB_A_INLINE
int token_queue_length (struct cb_token_queue *q)
{
	return q->length;
}

static
void token_queue_peek (struct cb_token_queue *q, const char **text,
			      const char **token) {
	/* assert (q->length > 0); */
	if (text)
		*text = q->texts[q->pos];
	if (token)
		*token = q->tokens[q->pos];
}

static void token_queue_get(struct cb_token_queue *q, int index,
			     const char **text,
			     const char **token) {
	/* assert (q->length - index > 0); */
	int pos = ( q->pos + index ) % q->maxsize;
	if (text)
		*text = q->texts[pos];
	if (token)
		*token = q->tokens[pos];
}

static
void token_queue_take (struct cb_token_queue *q, const char **text,
			    const char **token) {

	/* assert (q->length > 0); */

	if (text)
		*text = q->texts[q->pos];
	if (token)
		*token = q->tokens[q->pos];
	q->length--;
	q->pos = (q->pos+1) % q->maxsize;
}

static
void token_queue_remove (struct cb_token_queue *q, int n)
{
	/* assert (q->length >= n); */
	q->length -= n;
	q->pos = ( q->pos + n ) % q->maxsize;
}

static
void token_queue_empty (struct cb_token_queue *q)
{
	/* assert (q->length >= n); */
	q->length = 0;
	q->pos = 0;
}

static void token_queue_free(struct cb_token_queue *q) {
	if (q){
		cobc_free (q->texts);
		cobc_free (q);
	}
}

/* END implementation of queues */


struct cb_token_list {
	struct cb_token_list	*next;			/* next pointer */
	struct cb_token_list	*last;

	 /* The text in the source to be matched. Most of the time, it
	  *  directly what appears in the source file, but it may also
	  *  be a simplified version, typically for spaces, in which
	  *  case the exact text is stored in the `token` field (to be
	  *  used if no replacement is performed) */
	const char		*text;

	/* NULL most of the time, non-NULL only if the `text` was
	 * replaced by a simplified version, i.e. space to easy
	 * testing. */
	const char		*token;
};

/* types */
enum cb_ppecho {
	CB_PPECHO_DIRECT  = 0, /* direct output */
	CB_PPECHO_REPLACE = 1, /* output to REPLACE */
};

struct cb_replacement_state {

	/* The queue of tokens that are currently being checked for
	 * replacements. Empty, unless a partial match occurred. */
	struct cb_token_queue *token_queue ;

	/* We don't queue WORD tokens immediately, because
	 * preprocessing could create larger words. Instead, we buffer
	 * WORD tokens (and merge them) until another kind of token
	 * (SPACE,DELIM,etc.) is received. */
	const char	      *text_prequeue ;

	/* Current list of replacements specified in COPY-REPLACING or
	 * REPLACE */
	struct cb_replace_list  *replace_list ;

	/* List of replacements after a partial match that still need
	 * to be tested. */
	const struct cb_replace_list  *current_list ;

	/* The next pass to which generated tokens should be passed
	 * (either REPLACE pass or direct output */
	enum cb_ppecho	   ppecho ;

#ifdef DEBUG_REPLACE
	const char* name ;
#endif
};


#ifdef DEBUG_REPLACE_TRACE

#define WITH_DEPTH int depth,
#define INIT_DEPTH 1,
#define MORE_DEPTH depth+1,

#define MAX_DEPTH 100
char depth_buffer[MAX_DEPTH+1];
#define DEPTH depth_buffer + ( MAX_DEPTH-depth )

#else /* DEBUG_REPLACE_TRACE */

#define WITH_DEPTH
#define DEPTH
#define INIT_DEPTH
#define MORE_DEPTH

#endif /* DEBUG_REPLACE_TRACE */


#ifdef DEBUG_REPLACE

#define MAX_TEXT_LIST_STRING 10000
char text_list_string[MAX_TEXT_LIST_STRING];

/* In debugging mode only, stores a list of text/tokens into a
   preallocated string for easy display */
#define STRING_OF_LIST(kind)						\
static									\
char * string_of_##kind##_list(const struct cb_##kind##_list *list)	\
{									\
	int pos = 1;							\
	text_list_string[0] = '[';					\
									\
	for(; list != NULL; list = list->next){				\
		size_t len = strlen (list->text);			\
		text_list_string[pos++] = '"';				\
		memcpy (text_list_string + pos, list->text, len);	\
		pos += len;						\
		text_list_string[pos++] = '"';				\
		text_list_string[pos++] = ',';				\
		text_list_string[pos++] = ' ';				\
	}								\
									\
	text_list_string[pos] = ']';					\
	text_list_string[pos+1]=0;					\
	return text_list_string;					\
}

/* string_of_text_list (...) */
STRING_OF_LIST(text);

static
char * string_of_token_queue_after(struct cb_token_queue *q, int index)
{
	int pos = 1;
	text_list_string[0] = '[';

	for(; index < token_queue_length (q); index++){
		size_t len;
		const char *text;

		token_queue_get (q,index, &text, NULL);
		len = strlen (text);
		text_list_string[pos++] = '"';
		memcpy (text_list_string + pos, text, len);
		pos += len;
		text_list_string[pos++] = '"';
		text_list_string[pos++] = ',';
		text_list_string[pos++] = ' ';
	}

	text_list_string[pos] = ']';
	text_list_string[pos+1]=0;
	return text_list_string;
}

static void dump_replacement(struct cb_replacement_state* repls)
{
	fprintf(stderr, "dump_replacement('%s'):n", repls->name);
	struct cb_replace_list  *list = repls->replace_list ;
	for (;list;list = list->next){
		fprintf(stderr, "   replace: %s\n", string_of_text_list (list->src->text_list));
		fprintf(stderr, "        by: %s\n", string_of_text_list (list->new_text));
	}
	fprintf(stderr, "=================================================================\n");
}

#endif /* DEBUG_REPLACE */

/* global state */
static struct cb_replacement_state * replace_repls;
static struct cb_replacement_state * copy_repls;

/* forward definitions */
static COB_INLINE COB_A_INLINE void ppecho_replace (WITH_DEPTH const char *text, const char* token);
static void do_replace (WITH_DEPTH struct cb_replacement_state* repls);
static void check_replace_after_match (WITH_DEPTH struct cb_replacement_state *repls);
static void check_replace_all (WITH_DEPTH struct cb_replacement_state *repls,
			       const struct cb_text_list *new_text,
			       const struct cb_text_list *src,
			       const struct cb_replace_list *replace_list);

static void
ppecho_switch (WITH_DEPTH struct cb_replacement_state *repls,
		    const char* text, const char* token)
{
#ifdef DEBUG_REPLACE_TRACE
	fprintf (stderr, "%sppecho_switch(%s, '%s')\n",
		DEPTH, repls->name, text);
#endif
	switch (repls->ppecho) {
	case CB_PPECHO_DIRECT:
#ifdef DEBUG_REPLACE
		fprintf (stderr, "%s ppecho_direct('%s')\n", DEPTH, text);
#endif
		cb_ppecho_direct (text, token);
		return;
	case CB_PPECHO_REPLACE:
		ppecho_replace (MORE_DEPTH text, token);
		return;
	}
}

static void
ppecho_switch_text_list (WITH_DEPTH struct cb_replacement_state *repls,
			 const struct cb_text_list *p)
{
#ifdef DEBUG_REPLACE_TRACE
	fprintf (stderr, "%sppecho_switch_text_list(%s, %s)\n",
		DEPTH, repls->name, string_of_text_list(p));
#endif

	for (;p;p=p->next){
		ppecho_switch (MORE_DEPTH repls, p->text, NULL);
	}
}


static void
ppecho_switch_token_queue (WITH_DEPTH struct cb_replacement_state *repls,
			   struct cb_token_queue *q)
{
#ifdef DEBUG_REPLACE_TRACE
	fprintf (stderr, "%sppecho_switch_token_list(%s, %s)\n",
		 DEPTH, repls->name, string_of_token_queue_after(q,0));
#endif
	int n;
	const char *text;
	const char *token;
	for ( n = token_queue_length (q); n>0 ; --n){
		token_queue_take (q, &text, &token);
		ppecho_switch (MORE_DEPTH repls, text, token);
	}
}

static int
is_leading_or_trailing (WITH_DEPTH int leading,
			    const char* src_text,
			    const char* text,
			    int strict)
{

	const size_t src_len = strlen (src_text);
	const size_t text_len = strlen(text);
	int result ;
	if (text_len > src_len || ( !strict && text_len == src_len )) {
		int pos = leading ? 0 : text_len - src_len ;
		if (strncasecmp (src_text, text+pos, src_len)) {
			result = 0;
		} else {
			result = 1;
		}
	} else {
		result = 0;
	}
#ifdef DEBUG_REPLACE_TRACE
	fprintf (stderr,
		"%sis_leading_or_trailing(%d, '%s', input='%s', %d) -> %d\n",
		DEPTH, leading, src_text, text, strict, result);
#endif
	return result;
}

/* after a LEADING or TRAILING match, perform the replacement within
   the text, and pass the resulting new text to the next stream */
static void
ppecho_leading_or_trailing (WITH_DEPTH struct cb_replacement_state *repls,
				 int leading,
				 const char *src_text,
				 const char *text,
				 const struct cb_text_list *   new_text)
{
#ifdef DEBUG_REPLACE_TRACE
	fprintf (stderr,
		"%sppecho_leading_or_trailing(%s, %d, '%s', input='%s', ...)\n",
		DEPTH, repls->name, leading, src_text, text);
#endif

	size_t src_len = strlen (src_text);
	size_t text_len = strlen (text);

	if (!leading && text_len > src_len) {
		/* For TRAILING, we have to keep only the non-matched
		   prefix part of the matching text */
		const char* remaining_text =
			cobc_plex_strsub (text,
					  text_len - src_len);
		ppecho_switch (MORE_DEPTH repls, remaining_text, NULL);
	}

	ppecho_switch_text_list (MORE_DEPTH repls, new_text);

	if (leading && text_len > src_len) {
		const char* remaining_text =
			cobc_plex_strsub (text+src_len,
					  text_len - src_len);
		ppecho_switch (MORE_DEPTH repls, remaining_text, NULL);
	}
}

/* `check_replace( repls, replace_list )`: check if one of the
 * replacements in the list `replace_list` applies on the stream
 * `repls`.
 * * `repls`: the current stream
 * * `replace_list`: the current list of possible replacements on check
 */

static void
check_replace (WITH_DEPTH struct cb_replacement_state* repls,
		    const struct cb_replace_list *replace_list)
{
#ifdef DEBUG_REPLACE_TRACE
	fprintf (stderr, "%scheck_replace(%s, ...)\n", DEPTH,
		repls->name);
#endif
	repls->current_list = replace_list;

	if (replace_list == NULL){

		/* NO MATCH: no possible replacement on this text */

		/* remove the text from the current stream */
		const char* text;
		const char* token;
		token_queue_take (repls->token_queue, &text, &token);

		/* pass it to the next stream */
		ppecho_switch (MORE_DEPTH repls, text, token);

		/* restart replacements on this stream */
		check_replace_after_match (MORE_DEPTH repls);

	} else {
		const struct cb_replace_src *src = replace_list->src;
		const struct cb_text_list *new_text = replace_list->new_text;
		replace_list = replace_list->next;

		if (src->lead_trail == CB_REPLACE_LEADING
		 || src->lead_trail == CB_REPLACE_TRAILING){
			/* LEADING and TRAILING replacements are
			 * different: they match only on one text, so
			 * we just need one test to decide if it is a
			 * match or a failure */
			int leading = (src->lead_trail == CB_REPLACE_LEADING);
			unsigned int strict = src->strict;
			const char *src_text = src->text_list->text;
			const char *text;

			token_queue_peek (repls->token_queue, &text, NULL);

			if (is_leading_or_trailing (MORE_DEPTH leading,
						  src_text,text,strict)){

				/* MATCH */
				/* remove the text from the current stream */
				token_queue_remove (repls->token_queue, 1);

				/* perform a partial replacement on the text,
				   and pass it to the next stream */
				ppecho_leading_or_trailing (MORE_DEPTH repls,
						  leading,
						  src_text,text,
						  new_text) ;

				/* restart replacements on this stream */
				check_replace_after_match (MORE_DEPTH repls);
			} else {
				check_replace (MORE_DEPTH repls,replace_list);
			}
		} else {
			/* we need to compare a list of texts from
			 * this stream with a list of texts from the
			 * replacement */
			check_replace_all (MORE_DEPTH repls,
					  new_text,
					  src->text_list,
					  replace_list);
		}
	}
}

static COB_INLINE COB_A_INLINE int
is_space_or_nl (const char c)
{
	return c == ' ' || c == '\n';
}

/* `check_replace_all( repls, new_text, src, replace_list )`:
 * checks whether a particular replacement is possible on the current
 * list of texts.
 * * `repls` is the current stream state
 * * `new_text` is the text by which the texts should be replace in case of match
 * * `src` is the list of texts from the replacement to be matched
 * * `replace_list` is the next replacements to try in case of failure
 */
static void
check_replace_all (WITH_DEPTH
			struct cb_replacement_state *repls,
			const struct cb_text_list *new_text,
			const struct cb_text_list *src,
			const struct cb_replace_list *replace_list)
{
	int matched = 0;

	for (;;) {
		const char* src_text;
		const char *text;

#ifdef DEBUG_REPLACE_TRACE
		fprintf (stderr, "%scheck_replace_all(%s,",
			 DEPTH, repls->name);
		fprintf (stderr, "%s    new_text = %s,\n", DEPTH,
			 string_of_text_list(new_text));
		fprintf (stderr, "%s    texts = %s,\n", DEPTH,
			 string_of_token_queue_after (repls->token_queue, matched));
		fprintf (stderr, "%s    src = %s,\n", DEPTH,
			 string_of_text_list(src));
		fprintf (stderr, "%s)\n", DEPTH);
#endif

		if (!src){
			/* MATCH */
			/* pass the new text to the next stream */
			ppecho_switch_text_list (MORE_DEPTH repls, new_text) ;
			/* keep only in this stream the remaining texts that have not
			 * been matched */
			token_queue_remove (repls->token_queue, matched);
			/* restart replacements on the stream */
			check_replace_after_match (MORE_DEPTH repls);
			break;
		}

		src_text = src->text;

		if (is_space_or_nl(src_text[0])) {
			/* skip spaces in replacement */
			src = src->next;
			continue;
		}

		if ( token_queue_length (repls->token_queue) == matched){
			/* PARTIAL MATCH, we have emptied the
			 * list of texts, but there are still
			 * texts in the replacement, so wait
			 * for more texts to be added on the
			 * stream */
#ifdef DEBUG_REPLACE_TRACE
			fprintf (stderr, "%s  check_replace_all --> PARTIAL MATCH\n", DEPTH);
#endif
			return;
		}

		token_queue_get(repls->token_queue, matched, &text, NULL);
		matched++;

		if (is_space_or_nl(*text)) {
			/* skip spaces in texts */
			continue;
		}

		if (!strcasecmp (src_text, text)){
			/* We could match one text from the stream
			 * with a text from the replacement, so move
			 * on to the next text */
			src = src->next;
			continue;
		}

		/* match failed, move on to the next potential
		 * replacement */
		check_replace (
			MORE_DEPTH repls,
			replace_list);
		return;
	}
}

static void
check_replace_after_match (WITH_DEPTH struct cb_replacement_state *repls)
{
#ifdef DEBUG_REPLACE_TRACE
	fprintf (stderr, "%scheck_replace_after_match(%s)\n",
		DEPTH, repls->name);
#endif
	repls->current_list = NULL;
	while (!token_queue_is_empty(repls->token_queue)) {
		const char *text;
		const char *token;
		token_queue_peek (repls->token_queue, &text, &token);
		if (is_space_or_nl (text[0])) {
			ppecho_switch(MORE_DEPTH repls, text, token);
			token_queue_remove (repls->token_queue, 1);
		} else {
			do_replace(MORE_DEPTH repls);
			return;
		}
	}
}

static void
do_replace (WITH_DEPTH struct cb_replacement_state* repls)
{
#ifdef DEBUG_REPLACE_TRACE
	fprintf (stderr, "%sdo_replace(%s)\n",DEPTH, repls->name);
#endif
	if (repls->current_list == NULL){
		if (repls->replace_list == NULL){

			/* Beware: this is incorrect if a REPLACE is
			 * withing the queue, as it has already been
			 * parsed before any COPY-REPLACING
			 * substitution. */
			ppecho_switch_token_queue (MORE_DEPTH repls,
						  repls->token_queue);
			token_queue_empty (repls->token_queue);
		} else {
			check_replace (MORE_DEPTH repls, repls->replace_list);
		}
	} else {
		check_replace (MORE_DEPTH repls, repls->current_list);
	}
}

/* Whether a word matches the definition of WORD in pplex.l */
static int
is_word (WITH_DEPTH const char *s) {
	for (;;) {
		unsigned char c = (unsigned char) *s++;

		/* word character, just go on */
		if (c == '_'
		 || c == '-'
		 || ( c >= '0' && c <= '9' )
		 || ( c >= 'A' && c <= 'Z' )
		 || ( c >= 'a' && c <= 'z' )
		 || ( c >= 128) ) {
			continue;
		}

		/* end of string, no previous bad character -> is a word */
		if (c == 0) {
#ifdef DEBUG_REPLACE_TRACE
			fprintf (stderr, "%sis_word('%s') -> 1\n", DEPTH, s);
#endif
			return 1;
		}

		/* string 's' contains non-word characters -> isn't a word */
#ifdef DEBUG_REPLACE_TRACE
		fprintf (stderr, "%sis_word('%s') -> 0\n", DEPTH, s);
#endif
		return 0;
	}
}

static void
add_text_to_replace (WITH_DEPTH struct cb_replacement_state *repls,
		     int prequeue, const char* text, const char* token
	)
{
	/* CHECKME: this function takes >35% of the parsing cpu instructions,
	   with > 18% for memory allocation - can we reduce especially the
	   later in cases where REPLACE / REPLACING is not active? */
#ifdef DEBUG_REPLACE_TRACE
	fprintf (stderr, "%sadd_text_to_replace (%s%s, '%s')\n", DEPTH,
		repls->name, prequeue ? ", PREQUEUE" : "", text);
#endif
	if (prequeue) {

		if (is_word (MORE_DEPTH text) ) {

			if (repls->text_prequeue == NULL) {
				/* a word should be kept in the prequeue */
				repls->text_prequeue =
					cobc_plex_strdup (text);
			} else {
				/* two following words should be merged,
				   and keep waiting in the prequeue */
				repls->text_prequeue =
					cobc_plex_stradd (repls->text_prequeue,
							  text);
			}
		} else if (repls->text_prequeue == NULL) {
			/* not a word, and empty prequeue,
			   just perform replacements */
			add_text_to_replace (MORE_DEPTH repls, 0, text, token);
		} else {
			/* not a word, one word in the prequeue,
			   flush the word from the prequeue and pass the
			   current text to the replacements */
			const char *pretext = repls->text_prequeue;
			repls->text_prequeue = NULL;
			add_text_to_replace (MORE_DEPTH repls, 0, pretext, NULL);
			add_text_to_replace (MORE_DEPTH repls, 0, text, token);
		}

	} else {

		if ( token_queue_is_empty (repls->token_queue)
		 && is_space_or_nl (text[0]) ) {
			ppecho_switch (MORE_DEPTH repls, text, token);
		} else {
			/* use strdup if we are in the COPY phase */
			int strdup = repls->ppecho == CB_PPECHO_REPLACE ? 1 : 0;
#ifdef DEBUG_REPLACE_TRACE
			fprintf (stderr,
				"%s add_text_to_replace () -> push_text()\n",
				DEPTH);
#endif
			token_queue_put (repls->token_queue,
					 strdup, text, token);

			do_replace (MORE_DEPTH repls);
		}
	}
}

/* pass a text to the replace stream (called from the copy-replacing
  stream). Use prequeue = 1 so that texts of the same kind are
  merged into a single text.
 */
static COB_INLINE COB_A_INLINE void
ppecho_replace (WITH_DEPTH const char *text, const char *token)
{
#ifdef DEBUG_REPLACE
	fprintf (stderr, "%sppecho_replace('%s')\n", DEPTH, text);
#endif
	add_text_to_replace (MORE_DEPTH replace_repls, 1, text, token);
}

static struct cb_replacement_state *
create_replacements (enum cb_ppecho ppecho)
{
	struct cb_replacement_state *s
		= cobc_malloc (sizeof(struct cb_replacement_state));

	s->text_prequeue = NULL;
	s->token_queue = token_queue_new (8);
	s->replace_list = NULL ;
	s->current_list = NULL ;
	s->ppecho = ppecho;

#ifdef DEBUG_REPLACE
	if (ppecho == CB_PPECHO_REPLACE) {
		s->name = "COPY-REPLACING";
	} else {
		s->name = "REPLACE";
	}
#endif

	return s;
}

/* pass a text to the copy-replacing stream (called from ppecho() in
   pplex.l). Use prequeue = 0 as texts of the same kind from the
   source file should not be merged. There are two fast paths,
   i.e. cases in which we completely skip the replacement machinery.
 */
void
cb_ppecho_copy_replace (const char *text, const char *token)
{
#ifdef DEBUG_REPLACE
	fprintf (stderr, "cb_ppecho_copy_replace('%s')\n", text);
#endif

	/* two fast path to avoid the streams if no replacements are
	 * active */

	if (is_space_or_nl(text[0])
	 && token_queue_is_empty(copy_repls->token_queue)
	 && replace_repls->text_prequeue == NULL
	 && token_queue_is_empty(replace_repls->token_queue)) {
		cb_ppecho_direct (text, token);
		return;
	}

	if (copy_repls->replace_list == NULL
	 && copy_repls->current_list == NULL
	 && replace_repls->replace_list == NULL
	 && replace_repls->current_list == NULL) {
		cb_ppecho_direct (text, token);
		return;
	}

	add_text_to_replace (INIT_DEPTH copy_repls, 0, text, token);
}

/* Called by pplex.l at EOF of top file */
void
cb_free_replace (void)
{
	token_queue_free (copy_repls->token_queue);
	cobc_free (copy_repls);
	copy_repls = NULL;

	token_queue_free (replace_repls->token_queue);
	cobc_free (replace_repls);
	replace_repls = NULL;
}

/* Called by pplex.l when a new file is opened to save the previous
   stack of active copy-replacing */
struct cb_replace_list *
cb_get_copy_replacing_list (void)
{
#ifdef DEBUG_REPLACE_TRACE
	fprintf (stderr, "cb_get_copy_replacing_list()\n");
#endif

	if (copy_repls == NULL) {
#ifdef DEBUG_REPLACE_TRACE
		int i;
		for (i = 0; i < MAX_DEPTH; i++) depth_buffer[i] = ' ';
		depth_buffer[MAX_DEPTH] = 0;
#endif
		copy_repls = create_replacements (CB_PPECHO_REPLACE);
		replace_repls = create_replacements (CB_PPECHO_DIRECT);
	}
	return copy_repls->replace_list ;
}

/* Called by pplex.l, either at the end of a file to restore the
previous stack of active copy-replacing, or when a new file is open to
set additional copy replacing */
void
cb_set_copy_replacing_list (struct cb_replace_list *list)
{
	copy_repls->current_list = NULL;
	copy_repls->replace_list = list;
#ifdef DEBUG_REPLACE
	fprintf (stderr, "set_copy_replacing_list(\n");
	for (;list != NULL; list=list->next){
		fprintf (stderr, "    repl = {\n");
		fprintf (stderr, "       src = %s\n",
			string_of_text_list(list->src->text_list));
		fprintf (stderr, "       leading = %d\n",
			list->src->lead_trail);
		fprintf (stderr, "       new_text = %s\n",
			string_of_text_list(list->new_text));
		fprintf (stderr, "    };\n");
	}
	fprintf (stderr, "   )\n");
#endif
}

/* Called by pplex.l from pp_set_replace_list() after a REPLACE statement:

                          list       is_pushpop
 REPLACE <repls>.         <> NULL    false
 REPLACE ALSO <repls>.    <> NULL    true
 REPLACE LAST OFF.        NULL       true
 REPLACE OFF.             NULL       false
 */
void
cb_set_replace_list (struct cb_replace_list *list, const int is_pushpop)
{
#ifdef DEBUG_REPLACE_TRACE
	fprintf (stderr, "set_replace_list(...)\n");
#endif
	if (!list) {
		/* REPLACE [LAST] OFF */
		if (!is_pushpop) {
			replace_repls->replace_list = NULL;
			return;
		}
		if (!replace_repls->replace_list) {
			return;
		}
		replace_repls->replace_list = replace_repls->replace_list->prev;
		return;
	}
	/* REPLACE [ALSO] ... */
	if (replace_repls->replace_list && is_pushpop) {
		list->last->next = replace_repls->replace_list;
		list->prev = replace_repls->replace_list;
	} else {
		list->prev = NULL;
	}
	replace_repls->replace_list = list;
	if (cb_src_list_file) {
		cb_set_print_replace_list (list);
	}
#ifdef DEBUG_REPLACE_TRACE
	dump_replacement(replace_repls);
#endif
}

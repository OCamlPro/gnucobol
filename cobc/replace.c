/*
   Copyright (C) 2003-2022 Free Software Foundation, Inc.

   Authors:
   Keisuke Nishida, Roger While, Ron Norman, Simon Sobisch, Brian Tiffin,
   Edward Hart, Dave Pitts

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
#ifdef	HAVE_STRINGS_H
#include <strings.h>
#endif
#include <ctype.h>
#include <time.h>
#include <limits.h>

#include "cobc.h"
#include "tree.h"
#include "replace.h"

static struct cb_replace_list	*current_replace_list = NULL;
static struct cb_replace_list	*save_current_replace = NULL;
static struct cb_replace_list	*base_replace_list = NULL;

struct cb_token_list {
	struct cb_token_list	*next;			/* next pointer */
	struct cb_token_list	*last;
	const char		*text;
	const char		*token;
};

static struct cb_token_list	*text_queue = NULL;
static size_t			check_partial_match = 0;

static int      ppecho_replace          (struct cb_replace_list *);

static struct cb_token_list *
pp_token_list_add (struct cb_token_list *list,
		      const char *text,
		      const char *token)
{
	struct cb_token_list	*p;
	void			*tp;
	int text_size = strlen (text);

	p = cobc_plex_malloc (sizeof (struct cb_token_list));
	if (token == NULL) {
		tp = cobc_plex_malloc (text_size + 1);
		p->token = NULL;
	} else {
		int token_size = strlen (token);
		tp = cobc_plex_malloc( text_size + token_size + 2);
		memcpy (tp+text_size+1, token, token_size);
		p->token = tp+text_size+1;
	}
	memcpy (tp, text, text_size);
	p->text = tp;
	if (!list) {
		p->last = p;
		return p;
	}
	list->last->next = p;
	list->last = p;
	return list;
}

void cb_free_replace( void )
{
	current_replace_list = NULL;
	base_replace_list = NULL;
	save_current_replace = NULL;
	text_queue = NULL;
}

struct cb_replace_list *cb_get_copy_replacing_list (void)
{
        return current_replace_list;
}

void cb_set_copy_replacing_list (struct cb_replace_list *list)
{
        current_replace_list = list;
}

void
cb_ppecho_copy_replace (const char *text, const char *token)
{
	/* performance note (last verified with GnuCOBOL 2.2):
	   while this function used 5% (according to callgrind)
	   of the complete time spent in a sample run with
	   -fsyntax-only on 880 production code files (2,500,000 LOC),
	   3.8% of this time is spent in fwrite, therefore not much potential
	   for optimization */

	struct cb_replace_list		*save_ptr;
	struct cb_token_list		*save_ptr_text_queue;
	int				status, save_status;

#if 0	/* Simon: disabled until found necessary, as this takes together with frwite
		   a big part of the parsing phase of cobc, increasing the IO cost by numbers */
	/* ensure nothing is in the stream buffer */
	fflush (ppout);
#endif

	if (text_queue == NULL && (text[0] == ' ' || text[0] == '\n')) {
		cb_ppecho_direct (text, token);
		return;
	}
	if (!current_replace_list && !base_replace_list) {
		/* Output queue */
		for (; text_queue; text_queue = text_queue->next) {
			cb_ppecho_direct(text_queue->text, text_queue->token);
		}
		cb_ppecho_direct(text, token);
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
	text_queue = pp_token_list_add (text_queue, text, token);

	save_ptr_text_queue = text_queue;
	status = ppecho_replace (save_ptr);
	/* Search another replacement when have a Partial Match	in the last ppecho call */
	if (check_partial_match && status != -1) {
		save_status = status;
		text_queue = save_ptr_text_queue;
		while (text_queue && check_partial_match) {
			if (is_space_or_nl (text_queue->text[0])) {
				cb_ppecho_direct (text_queue->text,
						  text_queue->token);
				text_queue = text_queue->next;
				continue;
			}
			status = ppecho_replace (save_ptr);
			if (status > save_status) {
				save_status = status;
			}
			if (text_queue) {
				/* Write text_queue if is not replaced */
				if (status != -1 && check_partial_match) {
					cb_ppecho_direct (text_queue->text,
							  text_queue->token);
				}
				text_queue = text_queue->next;
			}
		}
		status = save_status;
	}
	/* Manage Partial Match */
	if (status == -1) {
		check_partial_match = save_ptr_text_queue != NULL;
		return;
	}
	if (!status) {
		current_replace_list = NULL;
	} else {
		save_ptr->next = NULL;
	}

	/* No match */
	for (; text_queue; text_queue = text_queue->next) {
		cb_ppecho_direct (text_queue->text, text_queue->token);
	}
}

/* handle all kinds of COPY REPLACING and REPLACE */
static int
ppecho_replace (struct cb_replace_list *save_ptr)
{
	char				*temp_ptr;
	size_t				size;
	size_t				size2;
	struct cb_token_list		*queue;
	struct cb_token_list		*save_queue;
	const struct cb_text_list	*lno;
	struct cb_replace_list		*r;

	save_queue = NULL;
	size = 0;
	size2 = 0;
	for (r = current_replace_list; r; r = r->next) {
		queue = text_queue;
		/* The LEADING/TRAILING code looks peculiar as we use */
		/* variables after breaking out of the loop BUT */
		/* ppparse.y guarantees that we have only one token */
		/* and therefore only one iteration of this loop */
		for (lno = r->src->text_list; lno; lno = lno->next) {
			if (is_space_or_nl (lno->text[0])) {
				continue;
			}
			while (queue && is_space_or_nl (queue->text[0])) {
				queue = queue->next;
			}
			if (queue == NULL) {
				/* Partial match */
				if (!save_ptr) {
					current_replace_list = NULL;
				} else {
					save_ptr->next = NULL;
				}
				return -1;
			}
			if (r->src->lead_trail == CB_REPLACE_LEADING) {
				/* Check leading text */
				size = strlen (lno->text);
				if ((r->src->strict && strlen (queue->text) == size)
				 || strncasecmp (lno->text, queue->text, size)) {
					/* No match */
					break;
				}
				save_queue = queue;
			} else if (r->src->lead_trail == CB_REPLACE_TRAILING) {
				/* Check trailing text */
				size = strlen (lno->text);
				size2 = strlen (queue->text);
				if (size2 < size
				 || (r->src->strict && size2 == size)) {
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
			if (r->src->lead_trail == CB_REPLACE_TRAILING
			 && save_queue /* <- silence warnings */) {
				/* Non-matched part of original text */
				temp_ptr = cobc_strdup (save_queue->text);
				*(temp_ptr + size2) = 0;
				cb_ppecho_direct (temp_ptr, NULL);
				cobc_free (temp_ptr);
			}
			for (lno = r->new_text; lno; lno = lno->next) {
				cb_ppecho_direct (lno->text, NULL);
			}
			if (r->src->lead_trail == CB_REPLACE_LEADING
			 && save_queue /* <- silence warnings */) {
				/* Non-matched part of original text */
				cb_ppecho_direct (save_queue->text + size, NULL);
			}
			check_partial_match = 0;
			text_queue = queue;
			continue;
		}
	}
	return (save_ptr ? 1 : 0);
}


void
cb_set_replace_list (struct cb_replace_list *list, const int is_pushpop)
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
		cb_set_print_replace_list (list);
	}
}

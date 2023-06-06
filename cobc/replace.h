/*
   Copyright (C) 2023-2023 Free Software Foundation, Inc.
   Written by Fabrice Le Fessant

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

#ifndef CB_REPLACE_H
#define CB_REPLACE_H

// defined in pplex.l
extern void	cb_ppecho_direct (const char *text, const char *token );
extern struct cb_text_list	*cb_pp_text_list_add (struct cb_text_list *,
					 const char *, const size_t);

extern void	cb_ppecho_copy_replace (const char *text, const char *token );
extern void     cb_free_replace (void);

/* For COPY-REPLACING */
extern void cb_set_copy_replacing_list (struct cb_replace_list *list);
extern struct cb_replace_list * cb_get_copy_replacing_list (void);

extern void
cb_set_print_replace_list	(struct cb_replace_list *);

static COB_INLINE COB_A_INLINE int
is_space_or_nl (const char c)
{
	return c == ' ' || c == '\n';
}

#endif // CB_REPLACE_H

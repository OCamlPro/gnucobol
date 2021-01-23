/*
   Copyright (C) 2005-2012, 2014-2017 Free Software Foundation, Inc.
   Written by Sergey Kashyrin

   This file is part of GnuCOBOL C++.

   The GnuCOBOL C++ runtime library is free software: you can redistribute it
   and/or modify it under the terms of the GNU Lesser General Public License
   as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   GnuCOBOL C++ is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with GnuCOBOL C++.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include "cobc.h"
#include "tree.h"
#include "vcache.h"

// NAMES REORGANIZATION
static int dupnum = 1;
static vcache<cb_field *> Names;

static void reorg_fields(cb_field * f)
{
	for(; f; f = f->sister) {
		if(f->sister) {
			f->sister->upsister = f;
		}
		if(f->children) {
			reorg_fields(f->children);
		}
	}
}

static void reorg_redef(cb_field * f, int level, cb_field ** base = 0)
{
	int unum = 1;
	while(f) {
		if(f->sister && f->sister->redefines == f) {
			cb_field * p = (cb_field *) cobc_parse_malloc(sizeof(cb_field));
			*p = *f;
			p->id = cb_field_id++;
			char * nm = new char[20];
			sprintf(nm, "_un%de%dn%d", f->storage, level, unum++);
			p->name = nm;
			p->usage = CB_USAGE_UNION;
			p->occurs_max = 1;
			p->flag_occurs = 0;
			p->values = 0;
			p->children = f;
			cb_field * fnext = f->sister;
			cb_field * flast = fnext;
			while(fnext && fnext->redefines == f) {
				flast = fnext;
				fnext->parent = p;
				fnext = fnext->sister;
			}
			p->sister = fnext;
			flast->sister = 0;
			if(fnext) {
				fnext->upsister = p;
			}
			if(f->upsister) {
				f->upsister->sister = p;
			} else if(f->parent) {
				f->parent->children = p;
			} else {
				*base = p;
			}
			f->parent = p;
			f->upsister = 0;
			while(f) {
				if(f->children) {
					reorg_redef(f->children, level + 1);
				}
				f = f->sister;
			}
			f = fnext;
			continue;
		}
		if(f->children) {
			reorg_redef(f->children, level + 1);
		}
		f = f->sister;
	}
}

static char * reserved[] = {
	"LC_NUMERIC",
	"LC_NAME",
	"PAGE_SIZE",
	"ADJ_STATUS",
	"EOF"
};

static bool isreserved(const char * name)
{
	for(int i = 0; i < sizeof(reserved) / sizeof(reserved[0]); ++i) {
		if(0 == strcmp(reserved[i], name)) {
			return true;
		}
	}
	return false;
}

static void reorg_fnames(cb_field * f, const char * basename, const char * add = NULL)
{
	cb_field * fbase = f;
	int dupnum = 1;
	while(f) {
		if(f->flag_base && *basename == 0 && !f->flag_occurs) {
			f->bPointer = true;
		}
		// Resolving unused duplicate names
		char adddup[20];
		adddup[0] = 0;
		for(cb_field * prev = fbase; prev != f; prev = prev->sister) {
			if(0 == strcmp(prev->name, f->name)) {
				// Duplicate
				sprintf(adddup, "_%d_", dupnum);
				++dupnum;
				break;
			}
		}
		if(f == fbase && add != NULL) {
			// Duplicate of 01 level
			strcpy(adddup, add);
		}
		int nm0len = (int)(strlen(f->name) + strlen(adddup) + 1);
		char * nm0 = new char[nm0len + 1];
		if(!isalpha(f->name[0]) && f->name[0] != '_') {
			nm0[0] = '_';
			strcpy(nm0 + 1, f->name);
			if(adddup[0] != 0) {
				strcat(nm0, adddup);
			}
			++nm0len;
		} else {
			strcpy(nm0, f->name);
			if(adddup[0] != 0) {
				strcat(nm0, adddup);
			}
		}
		for(char * p = nm0; *p; ++p) {
			if(*p == ' ' || *p == '-') {
				*p = '_';
			} else {
				*p = toupper(*p);
			}
		}
		if(isreserved(nm0)) {
			char * nn = new char[nm0len + 1];
			memcpy(nn, nm0, nm0len);
			nn[nm0len - 1] = '_';
			nn[nm0len] = 0;
			delete [] nm0;
			nm0 = nn;
			++nm0len;
		}
		int ct = (int)(nm0len + strlen(basename) + 1);
		char * nm = new char[ct];
		if(*basename == 0) {
			strcpy(nm, nm0);
		} else {
			strcpy(nm, basename);
			strcat(nm, ".");
			strcat(nm, nm0);
		}
		f->sName = nm0;
		f->sRefName = nm;
		f->bUseName = true;
		if(f->children) {
			reorg_fnames(f->children, nm);
		}
		if(*basename == 0) {
			break;
		}
		f = f->sister;
	}
}

static bool chkren(cb_field * f, bool bSister = true)
{
	if(f->rename_thru != 0) {
		return true;
	}
	if(f->children) {
		if(chkren(f->children)) {
			return true;
		}
	}
	if(bSister && f->sister) {
		if(chkren(f->sister)) {
			return true;
		}
	}
	return false;
}

void externalize_tree(cb_program * pgm, cb_field ** base, bool bWS)
{
	if(*base == 0) {
		return;
	}
	reorg_fields(*base);
	reorg_redef(*base, 1, base);
	for(cb_field * f = *base; f; f = f->sister) {
		// Check if it does not have RENAME - it is screwing
		if(chkren(f, false)) {
			skadbg("NOT REORG - chkren %s\n", f->name);
			continue;
		}
		// Check if it is not Complex ODO
		if(f->children) {
			// Find the last field
			cb_field * p = f->children;
			while(p->children || p->sister) {
				if(p->sister) {
					p = p->sister;
					continue;
				}
				p = p->children;
			}
			if(chk_field_variable_address(p)) {
				skadbg("NOT REORG - ODO %s\n", f->name);
				continue;	// Can't use names of C++ structure !!!
			}
		}
		if(f->flag_is_global && f->id != 1 /* RETURN-CODE */) {
			skadbg("NOT REORG - global %s\n", f->name);
			continue;	// Due to possible name collisions
		}
		if(f->flag_external) {
			skadbg("NOT REORG - external %s\n", f->name);
			continue;	// names issues
		}
		skadbg("YES REORG - %s\n", f->name);
		// Resolving duplicate names of 01 level
		char adddup[20];
		adddup[0] = 0;
		cb_field * fd = NULL;
		if(0 == strcmp(pgm->program_id, f->name)) {
			sprintf(adddup, "_%d_", dupnum);
			++dupnum;
		} else {
			if(bWS) {
				fd = Names[f->name];
				if(fd != NULL && f->id != fd->id) {
					// 01 level duplicate in the same or different program
					sprintf(adddup, "_%d_", dupnum);
					++dupnum;
				}
			} else {
				for(cb_field * prev = *base; prev != f; prev = prev->sister) {
					if(0 == strcmp(prev->name, f->name)) {
						// Duplicate
						sprintf(adddup, "_%d_", dupnum);
						++dupnum;
						break;
					}
				}
			}
		}
		reorg_fnames(f, "", adddup);
		if(bWS && fd == NULL) {
			Names.put(f->name, f);
		}
	}
}

void naming_init()
{
	dupnum = 1;
	Names.clear();
}

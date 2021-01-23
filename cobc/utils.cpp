/*
   Copyright (C) 2001-2012, 2014-2017 Free Software Foundation, Inc.
   Written by Sergey Kashyrin

   This file is part of GnuCOBOL C++.

   The GnuCOBOL C++ compiler is free software: you can redistribute it
   and/or modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   GnuCOBOL C++ is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GnuCOBOL C++.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <stdlib.h>
#include <stdarg.h>
#include <time.h>
#include <ctype.h>
#include "cobc.h"
#include "tree.h"

/////////////////////////// SKA DEBUG //////////////////////
//
static FILE * sk = 0;
static bool inited = false;

static void skopen()
{
	inited = true;
	char * e = getenv("SKADBG");
	if(!e) {
		return;
	}
	sk = fopen(e, "w+");
	if(sk != 0) {
		time_t ti = time(NULL);
		struct tm * tm = localtime(&ti);
		char bt[128];
		strftime(bt, sizeof bt, "STARTED %Y.%m.%d %H:%M:%S\n", tm);
		fputs(bt, sk);
	}
}

void skadbg(const char * fmt, ...)
{
	va_list	ap;
	if(!inited) {
		skopen();
	}
	if(sk) {
		va_start(ap, fmt);
		vfprintf(sk, fmt, ap);
		fflush(sk);
		va_end(ap);
	}
}

void prtrone(cb_field * f)
{
	if(!inited) {
		skopen();
	}
	if(sk == 0) {
		return;
	}
	fprintf(sk, "FLD 0%d %s    id %d sz %d\n", f->level, f->name, f->id, f->size);
	if(f->sName) {
		fprintf(sk, "\tC++ NAME %s\n", f->sName);
	}
	if(f->sType) {
		fprintf(sk, "\tC++ struct type %s\n", f->sType);
	}
	if(f->sRefName) {
		fprintf(sk, "\tFull NAME %s\n", f->sRefName);
	}
	if(f->ename) {
		fprintf(sk, "\tExtername NAME %s\n", f->ename);
	}
	if(f->depending) {
		cb_field * f1 = (cb_field *) f->depending;
		fprintf(sk, "\tOCCURS ... DEPENDING ON %s\n", f1->name);
	}
	if(f->values) {
		fprintf(sk, "\thas VALUE\n");
	}
	if(f->false_88) {
		fprintf(sk, "\t88 FALSE clause\n");
	}
	if(f->index_list) {
		fprintf(sk, "\tINDEXED BY\n");
	}
	if(f->parent) {
		fprintf(sk, "\tPARENT %s\n", f->parent->name);
	}
	if(f->children) {
		fprintf(sk, "\tCHILD %s\n", f->children->name);
	}
	if(f->sister) {
		fprintf(sk, "\tSISTER %s\n", f->sister->name);
	}
	if(f->redefines) {
		fprintf(sk, "\tREDEF %s\n", f->redefines->name);
	}
	if(f->rename_thru) {
		fprintf(sk, "\tREN %s\n", f->rename_thru->name);
	}
	if(f->index_qual) {
		fprintf(sk, "\tINDEXED BY QUAL %s\n", f->index_qual->name);
	}

	fprintf(sk, "\tsz %d memsz %d off %d memoff %d\n", f->size, f->memory_size, f->offset, f->mem_offset);
	fprintf(sk, "\tocc_min %d occ_max %d indexes %d count %d\n", f->occurs_min, f->occurs_max, f->indexes, f->count);
	fprintf(sk, "\tstorage %d usage %d\n", f->storage, f->usage);
	fprintf(sk, "\tf_base %d f_ext %d f_loc %d f_glob %d\n", f->flag_base, f->flag_external, f->flag_local_storage, f->flag_is_global);
	fprintf(sk, "\tflag_binary_swap %d flag_real_binary %d\n", f->flag_binary_swap, f->flag_real_binary);
	fprintf(sk, "\n");
}

void prtree(cb_field * f)
{
	if(!inited) {
		skopen();
	}
	if(sk == 0) {
		return;
	}
	if(f == 0) {
		fprintf(sk, "\t*** empty ***\n");
		return;
	}
	prtrone(f);
	if(f->children) {
		prtree(f->children);
	}
	if(f->sister) {
		prtree(f->sister);
	}
}
//
/////////////////////////// SKA DEBUG //////////////////////

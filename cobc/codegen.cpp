/*
   Copyright (C) 2003-2012, 2014-2017 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Ron Norman, Simon Sobisch,
   Edward Hart, Sergey Kashyrin

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


#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <limits.h>
#include <string>

#include "tarstamp.h"

#include "cobc.h"
#include "tree.h"
#include "vcache.h"

using namespace std;

#ifdef	HAVE_ATTRIBUTE_ALIGNED
	#define COB_ALIGN " __attribute__((aligned))"
#else
	#define COB_ALIGN ""
#endif

static char buf[1024 * 1024];

#define COB_MAX_SUBSCRIPTS	16

#define COB_MALLOC_ALIGN	15

#define COB_INSIDE_SIZE		64

#define INITIALIZE_NONE		0
#define INITIALIZE_ONE		1
#define INITIALIZE_DEFAULT	2
#define INITIALIZE_COMPOUND	3

#define CB_NEED_HIGH		(1U << 0)
#define CB_NEED_LOW		(1U << 1)
#define CB_NEED_QUOTE		(1U << 2)
#define CB_NEED_SPACE		(1U << 3)
#define CB_NEED_ZERO		(1U << 4)

struct sort_list {
	sort_list 	*	next;
};

struct system_table {
	const char *	syst_name;
	const char *	syst_call;
	const unsigned int	syst_max_params;
};

struct label_list {
	label_list *	next;
	int				id;
	int				call_num;
};

struct string_list {
	string_list *	next;
	char 	*		text;
	int				id;
};

struct pic_list {
	pic_list 		*		next;
	const cob_pic_symbol *	str;
	int						length;
	int						id;
};

struct attr_list {
	attr_list 	*	next;
	int				pic_id;
	int				id;
	int				type;
	cob_u32_t		digits;
	int				scale;
	cob_u32_t		flags;
};

struct literal_list {
	literal_list *	next;
	cb_literal *	literal;
	cb_tree			x;
	int				id;
	int				make_decimal;
};

struct field_list {
	field_list *	next;
	cb_field 	*	f;
	cb_tree			x;
	const char *	curr_prog;
};

struct call_list {
	call_list 	*	next;
	const char *	callname;
};

#define COB_RETURN_INT		0
#define COB_RETURN_ADDRESS_OF	1
#define COB_RETURN_NULL		2

struct base_list {
	base_list 	*	next;
	cb_field 	*	f;
	const char *	curr_prog;
};


/* Local variables */

static pic_list 	*	pic_cache = NULL;
static attr_list 	*	attr_cache = NULL;
static literal_list *	literal_cache = NULL;
static field_list 	*	field_cache = NULL;
static field_list 	*	local_field_cache = NULL;
static call_list 	*	call_cache = NULL;
static call_list 	*	func_call_cache = NULL;
static base_list 	*	base_cache = NULL;
static base_list 	*	globext_cache = NULL;
static base_list 	*	local_base_cache = NULL;
static string_list *	string_cache = NULL;
static char 	*		string_buffer = NULL;
static label_list 	*	label_cache = NULL;


static FILE 	*		output_target = NULL;
static FILE 	*		cb_local_file = NULL;
static const char	*	excp_current_program_id = NULL;
static const char	*	excp_current_section = NULL;
static const char	*	excp_current_paragraph = NULL;
static cb_program	*	current_prog = NULL;

static cb_label 	*	last_section = NULL;
static unsigned char *	litbuff = NULL;
static int				litsize = 0;

static unsigned int		needs_exit_prog = 0;
static unsigned int		needs_unifunc = 0;
static unsigned int		need_save_exception = 0;
static unsigned int		gen_nested_tab = 0;
static unsigned int		gen_alt_ebcdic = 0;
static unsigned int		gen_ebcdic_ascii = 0;
static unsigned int		gen_full_ebcdic = 0;
static unsigned int		gen_native = 0;
static unsigned int		gen_custom = 0;
static unsigned int		gen_figurative = 0;
static unsigned int		gen_dynamic = 0;

static int				struct_id = 0;
static int				param_id = 0;
static int				stack_id = 0;
static int				string_id = 1;
static int				num_cob_fields = 0;
static int				non_nested_count = 0;
static int				loop_counter = 0;
static int				progid = 0;
static int				last_line = 0;
static cob_u32_t		field_iteration = 0;
static int				screenptr = 0;
static int				local_mem = 0;
static int				working_mem = 0;
static int				local_working_mem = 0;
static int				output_indent_level = 0;
static int				last_segment = 0;
static int				gen_if_level = 0;
static unsigned int		nolitcast = 0;

static unsigned int		inside_check = 0;
static unsigned int		inside_stack[COB_INSIDE_SIZE];

static unsigned int		i_counters[COB_MAX_SUBSCRIPTS];

#undef	COB_SYSTEM_GEN
#define	COB_SYSTEM_GEN(cob_name, pmin, pmax, c_name)	{ cob_name, #c_name, pmax },

static const system_table	system_tab[] = {
#include "libcob/system.def"
	{ NULL, NULL, 0 }
};

#undef	COB_SYSTEM_GEN

static vcache<const char *> declpgms;

/* Declarations */
static string output(const char *, ...)		COB_A_FORMAT12;
static string output_line(const char *, ...)	COB_A_FORMAT12;
static void output_storage(const char *, ...)	COB_A_FORMAT12;
static void output_local(const char *, ...)	COB_A_FORMAT12;

static string output_stmt(cb_tree);
static string output_integer(cb_tree);
static string output_index(cb_tree);
static string output_func_1(const char *, cb_tree);
static string output_param(cb_tree, int);
static string output_funcall(cb_tree);

/* Local functions */

static cb_field *
cb_code_field(cb_tree x)
{
	if(likely(CB_REFERENCE_P(x))) {
		if(unlikely(!CB_REFERENCE(x)->value)) {
			return CB_FIELD(cb_ref(x));
		}
		return CB_FIELD(CB_REFERENCE(x)->value);
	}
	return CB_FIELD(x);
}

static int
lookup_string(const char * p)
{
	for(string_list * stp = string_cache; stp; stp = stp->next) {
		if(strcmp(p, stp->text) == 0) {
			return stp->id;
		}
	}
	string_list * stp = (string_list *) cobc_parse_malloc(sizeof(string_list));
	stp->text = cobc_parse_strdup(p);
	stp->id = string_id;
	stp->next = string_cache;
	string_cache = stp;
	return string_id++;
}

static void
lookup_call(const char * p)
{
	for(call_list * clp = call_cache; clp; clp = clp->next) {
		if(strcmp(p, clp->callname) == 0) {
			return;
		}
	}
	call_list * clp = (call_list *) cobc_parse_malloc(sizeof(call_list));
	clp->callname = p;
	clp->next = call_cache;
	call_cache = clp;
}

static void
lookup_func_call(const char * p)
{
	for(call_list * clp = func_call_cache; clp; clp = clp->next) {
		if(strcmp(p, clp->callname) == 0) {
			return;
		}
	}
	call_list * clp = (call_list *) cobc_parse_malloc(sizeof(call_list));
	clp->callname = p;
	clp->next = func_call_cache;
	func_call_cache = clp;
}

template <class T>
static T * list_reverse(T * p)
{
	T * last = NULL;
	while(p != NULL) {
		T * next = p->next;
		p->next = last;
		last = p;
		p = next;
	}
	return last;
};

static int field_cache_cmp(const void * mp1, const void * mp2)
{
	const field_list * fl1 = (const field_list *)mp1;
	const field_list * fl2 = (const field_list *)mp2;
	int ret = strcasecmp(fl1->curr_prog, fl2->curr_prog);
	if(ret) {
		return ret;
	}
	return fl1->f->id - fl2->f->id;
}

static int base_cache_cmp(const void * mp1, const void * mp2)
{
	const base_list * fl1 = (const base_list *)mp1;
	const base_list * fl2 = (const base_list *)mp2;
	return fl1->f->id - fl2->f->id;
}

/* Sort a structure linked list in place */
/* Assumed that pointer "next" is first item in structure */

static void *
list_cache_sort(void * inlist, int(*cmpfunc)(const void * mp1, const void * mp2))
{
	if(!inlist) {
		return NULL;
	}
	sort_list * list = (sort_list *)inlist;
	size_t insize = 1;
	for(;;) {
		sort_list * p = list;
		list = NULL;
		sort_list * tail = NULL;
		size_t nmerges = 0;
		while(p) {
			nmerges++;
			sort_list * q = p;
			size_t psize = 0;
			for(size_t i = 0; i < insize; i++) {
				psize++;
				q = q->next;
				if(!q) {
					break;
				}
			}
			size_t qsize = insize;
			while(psize > 0 || (qsize > 0 && q)) {
				sort_list * e;
				if(psize == 0) {
					e = q;
					q = q->next;
					if(qsize) {
						qsize--;
					}
				} else if(qsize == 0 || !q) {
					e = p;
					p = p->next;
					if(psize) {
						psize--;
					}
				} else if((*cmpfunc)(p, q) <= 0) {
					e = p;
					p = p->next;
					if(psize) {
						psize--;
					}
				} else {
					e = q;
					q = q->next;
					if(qsize) {
						qsize--;
					}
				}
				if(tail) {
					tail->next = e;
				} else {
					list = e;
				}
				tail = e;
			}
			p = q;
		}
		tail->next = NULL;
		if(nmerges <= 1) {
			return(void *)list;
		}
		insize *= 2;
	}
}

/* Output routines */

static void
real_output(const char * s)
{
	if(output_target) {
		fputs(s, output_target);
	}
}

static string
output_str(const char * fmt, ...)
{
	va_list		ap;
	va_start(ap, fmt);
	vsprintf(buf, fmt, ap);
	va_end(ap);
	return buf;
}

static string
output(const char * fmt, ...)
{
	va_list		ap;
	if(output_target) {
		va_start(ap, fmt);
		vsprintf(buf, fmt, ap);
		va_end(ap);
		return buf;
	}
	return "";
}

static string
output_newline(void)
{
	string str("");
	if(output_target) {
		str = "\n";
	}
	return str;
}

static string
output_prefix(void)
{
	string str("");
	if(output_target) {
		for(int i = 0; i < output_indent_level; i++) {
			str += " ";
		}
	}
	return str;
}

static string
output_line(const char * fmt, ...)
{
	va_list		ap;

	if(output_target) {
		string str = output_prefix();
		va_start(ap, fmt);
		vsprintf(buf, fmt, ap);
		va_end(ap);
		str += buf;
		str += "\n";
		return str;
	}
	return "";
}

static string
output_indent(const char * str)
{
	const char * p;
	int level = 2;
	for(p = str; *p == ' '; p++) {
		level++;
	}

	if(*p == '}' && strcmp(str, "})") != 0) {
		output_indent_level -= level;
	}

	string s = output_line(str);

	if(*p == '{' && strcmp(str, ")}") != 0) {
		output_indent_level += level;
	}
	return s;
}

static string
output_string(const unsigned char * s, const int size, const cob_u32_t llit)
{
	if(!s) {
		return output("NULL");
	}
	string str = output("\"");
	for(int i = 0; i < size; i++) {
		int c = s[i];
		if(!isprint(c)) {
			str += output("\\%03o", c);
		} else if(c == '\"') {
			str += output("\\%c", c);
		} else if((c == '\\' || c == '?') && !llit) {
			str += output("\\%c", c);
		} else {
			str += output("%c", c);
		}
	}
	str += output("\"");
	return str;
}

static void
output_storage(const char * fmt, ...)
{
	va_list		ap;

	if(cb_storage_file) {
		va_start(ap, fmt);
		vfprintf(cb_storage_file, fmt, ap);
		va_end(ap);
	}
}

static void
output_local(const char * fmt, ...)
{
	va_list		ap;

	if(cb_local_file) {
		va_start(ap, fmt);
		vfprintf(cb_local_file, fmt, ap);
		va_end(ap);
	}
}

/* Field */

static cb_field *
real_field_founder(const cb_field * f)
{
	/////////////////////////// SKA //////////////////////
	cb_field * ff = (cb_field *) f;
	while(ff->parent) {
		ff = ff->parent;
	}
	if(ff->redefines) {
		ff = ff->redefines;
	}
	if(ff->usage == CB_USAGE_UNION) {
		ff = ff->children;
	}
	return ff;
}

static cb_field *
chk_field_variable_size(cb_field * f)
{
	if(f->flag_vsize_done) {
		return f->vsize;
	}
	for(cb_field * fc = f->children; fc; fc = fc->sister) {
		if(fc->depending) {
			f->vsize = fc;
			f->flag_vsize_done = 1;
			return fc;
		} else {
			cb_field * p = chk_field_variable_size(fc);
			if(p != NULL) {
				f->vsize = p;
				f->flag_vsize_done = 1;
				return p;
			}
		}
	}
	f->vsize = NULL;
	f->flag_vsize_done = 1;
	return NULL;
}

unsigned int
chk_field_variable_address(cb_field * fld)
{
	if(fld->flag_vaddr_done) {
		return fld->vaddr;
	}
	cb_field * f = fld;
	for(cb_field * p = f->parent; p; f = f->parent, p = f->parent) {
		for(p = p->children; p != f; p = p->sister) {
			if(p->depending || chk_field_variable_size(p)) {
				fld->vaddr = 1;
				fld->flag_vaddr_done = 1;
				return 1;
			}
		}
	}
	fld->vaddr = 0;
	fld->flag_vaddr_done = 1;
	return 0;
}

static string
output_base(cb_field * f, const cob_u32_t no_output)
{
	string str("");

	if(unlikely(f->flag_item_78)) {
		/* LCOV_EXCL_START */
		cobc_err_msg(_("unexpected CONSTANT item"));
		COBC_ABORT();
		/* LCOV_EXCL_STOP */
	}

	cb_field * f01 = real_field_founder(f);

/////////////////////////// SKA DEBUG //////////////////////
	skadbg("output_base noout %d base %d f01 %d %s f %d %s\n", no_output, f01->flag_base, f01->id, f01->name, f->id, f->name);
	if(!f01->flag_base) {
		void prtrone(cb_field *);
		prtrone(f01);
	}
/////////////////////////// SKA DEBUG //////////////////////

	/* Base storage */

	if(!f01->flag_base) {
		if(f01->special_index == 2U) {
			base_list * bl = (base_list *) cobc_parse_malloc(sizeof(base_list));
			bl->f = f01;
			bl->curr_prog = excp_current_program_id;
			bl->next = local_base_cache;
			local_base_cache = bl;
		} else if(!f01->flag_external && !f01->flag_local_storage) {
			if(!f01->flag_local || f01->flag_is_global) {
				base_list * bl = (base_list *) cobc_parse_malloc(sizeof(base_list));
				bl->f = f01;
				bl->curr_prog = excp_current_program_id;
				if(f01->flag_is_global ||
						current_prog->flag_file_global) {
					bl->next = base_cache;
					base_cache = bl;
				} else {
					bl->next = local_base_cache;
					local_base_cache = bl;
				}
			} else {
				if(current_prog->flag_global_use) {
					output_local("unsigned char\t\t*%s%d = NULL;",
								 CB_PREFIX_BASE, f01->id);
					output_local("\t/* %s */\n", f01->name);
					output_local("static unsigned char\t*save_%s%d;\n",
								 CB_PREFIX_BASE, f01->id);
				} else {
					output_local("unsigned char\t*%s%d = NULL;",
								 CB_PREFIX_BASE, f01->id);
					output_local("\t/* %s */\n", f01->name);
				}
			}
		}
		f01->flag_base = 1;
	}
	if(no_output) {
		return str;
	}

	if(f01->special_index) {
		str += output("(cob_u8_t *)&%s%d", CB_PREFIX_BASE, f01->id);
		return str;
	} else if(f01->flag_local_storage) {
		if(f01->mem_offset) {
			str += output("cob_local_ptr + %d", f01->mem_offset);
		} else {
			str += output("cob_local_ptr");
		}
		if(f01->sRefName) {
			str += output(" /* %s */ ", f01->sRefName);
		}
	} else {
		str += output("%s%d", CB_PREFIX_BASE, f01->id);
	}

	if(chk_field_variable_address(f)) {
		for(cb_field * p = f->parent; p; f = f->parent, p = f->parent) {
			for(p = p->children; p != f; p = p->sister) {
				cb_field * v = chk_field_variable_size(p);
				if(v) {
					str += output(" + %d + ", v->offset - p->offset);
					if(v->size != 1) {
						str += output("%d * ", v->size);
					}
					str += output_integer(v->depending);
				} else if(p->depending && cb_flag_odoslide) {
					str += output(" + ");
					if(p->size != 1) {
						str += output("%d * ", p->size);
					}
					str += output_integer(p->depending);
				} else {
					str += output(" + %d", p->size * p->occurs_max);
				}
			}
		}
	} else if(f->offset > 0) {
		str += output(" + %d", f->offset);
	}
	return str;
}

static string
output_data(cb_tree x)
{
	string str("");

	switch(CB_TREE_TAG(x)) {
	case CB_TAG_LITERAL: {
		cb_literal * l = CB_LITERAL(x);
		if(CB_TREE_CLASS(x) == CB_CLASS_NUMERIC) {
			str += output("(cob_u8_ptr)\"%s%s\"",
						  (l->sign < 0) ? "-" : (l->sign > 0) ? "+" : "",
						  (char *)l->data);
		} else {
			str += output("(cob_u8_ptr)");
			str += output_string(l->data, (int) l->size, l->llit);
		}
	}
	break;
	case CB_TAG_FIELD:
		str += output_base(CB_FIELD(x), 0);
		break;
	case CB_TAG_REFERENCE: {
		cb_reference * r = CB_REFERENCE(x);
		cb_field * f = CB_FIELD(r->value);

		if(f->bUseName) {
			// We can use name !!!
			output_base(f, 1);	// Don't output anything
			string name = "";
			bool bIndex = f->special_index != 0 || f->children;
			bool bPtr = f->bPointer && !f->flag_occurs;
			bool bNoAdrOp = false;
			// possible subscripts
			cb_tree lsub = r->subs;
			for(; f; f = f->parent) {
				if(name.length() != 0) {
					if(f->bPointer) {
						name = "->" + name;
					} else {
						name = "." + name;
					}
				}
				if(f->flag_occurs) {
					if(lsub) {
						name = "]" + name;
						name = output_index(CB_VALUE(lsub)) + name;
						name = "[" + name;
						lsub = CB_CHAIN(lsub);
					} else {
						name = "[0]" + name;
					}
				}
				if(f->flag_local_storage && !f->parent) {
					bNoAdrOp = true;
					string st = "(";
					st += "(";
					st += f->sType;
					st += " *)";
					st += "(cob_local_ptr";
					if(f->mem_offset) {
						sprintf(buf, " + %d", f->mem_offset);
						st += buf;
					}
					st += ")";
					st += ")";
					name = st + name;
				} else {
					name = f->sName + name;
				}
			}
			if(bIndex) {
				if(bPtr) {
					if(bNoAdrOp) {
						name = "(cob_u8_t *)" + name;
					} else {
						name = "*(cob_u8_t **)&" + name;
					}
				} else {
					name = "(cob_u8_t *)&" + name;
				}
			}
			str = output("%s", name.c_str());
			/* Offset */
			if(r->offset) {
				str += output(" + ");
				str += output_index(r->offset);
			}
			return str;
		}
		/* Base address */
		str += output_base(f, 0);

		/* Subscripts */
		if(r->subs) {
			cb_tree lsub = r->subs;
			cb_field * o_slide = NULL;
			for(; f && lsub; f = f->parent) {
				/* add current field size for OCCURS */
				if(f->flag_occurs) {
					/* recalculate size for nested ODO ... */
					if(unlikely(o_slide)) {
						for(cb_field * o = o_slide; o; o = o->children) {
							if(o->depending) {
								str += output(" + (%d * ", o->size);
								str += output_integer(o->depending);
								str += output(")");
							}
						}
						str += output(" * ");
					} else {
						/* ... use field size otherwise */
						str += output(" + ");
						if(f->size != 1) {
							str += output("%d * ", f->size);
						}
					}
					if(cb_flag_odoslide && f->depending) {
						o_slide = f;
					}

					str += output_index(CB_VALUE(lsub));
					lsub = CB_CHAIN(lsub);
				}
			}
		}

		/* Offset */
		if(r->offset) {
			str += output(" + ");
			str += output_index(r->offset);
		}
	}
	break;
	case CB_TAG_CAST: {
		string nm = output_param(x, 0);
		char c = (nm.length() == 0) ? 0 : nm.at(0);
		if(c != '(') {
			str += output("&");
			str += nm;
		} else {
			size_t ix = nm.find(')');
			str += nm.substr(0, ix);
			str += "*)&";
			str += nm.substr(ix + 1);
		}
	}
	break;
	case CB_TAG_INTRINSIC:
		str += output("cob_procedure_params[%u]->data",
					  field_iteration);
		break;
	case CB_TAG_CONST:
		if(x == cb_null) {
			str += output("NULL");
			return str;
		}
	/* Fall through */
	default:
		/* LCOV_EXCL_START */
		cobc_err_msg(_("unexpected tree tag: %d"), (int)CB_TREE_TAG(x));
		COBC_ABORT();
		/* LCOV_EXCL_STOP */
	}
	return str;
}

static string
output_size(const cb_tree x)
{
	string str("");

	switch(CB_TREE_TAG(x)) {
	case CB_TAG_CONST:
		str += output("1");
		break;
	case CB_TAG_LITERAL: {
		cb_literal * l = CB_LITERAL(x);
		str += output("%d", (int)(l->size + ((l->sign != 0) ? 1 : 0)));
	}
	break;
	case CB_TAG_REFERENCE: {
		cb_reference * r = CB_REFERENCE(x);
		cb_field * f = CB_FIELD(r->value);
		if(f->flag_no_field) {
			str += output("0");
			break;
		}
		if(r->length) {
			str += output_integer(r->length);
		} else if(r->offset && f->flag_any_length) {
			str += output("%s%d.size - ", CB_PREFIX_FIELD, f->id);
			str += output_index(r->offset);
		} else {
			cb_field * p = chk_field_variable_size(f);
			cb_field * q = f;
again:
			if(!cb_flag_odoslide && p && p->flag_odo_relative) {
				q = p;
				str += output("%d", p->size * p->occurs_max);
			} else if(p && (!r->flag_receiving ||
							!cb_field_subordinate(cb_code_field(p->depending),
												  q))) {
				if(p->offset - q->offset > 0) {
					str += output("%d + ", p->offset - q->offset);
				}
				if(p->size != 1) {
					str += output("%d * ", p->size);
				}
				str += output_integer(p->depending);
				q = p;
			} else {
				str += output("%d", q->size);
			}

			for(; q != f; q = q->parent) {
				if(q->sister && !q->sister->redefines) {
					q = q->sister;
					p = q->depending ? q : chk_field_variable_size(q);
					str += output(" + ");
					goto again;
				}
			}
			if(r->offset) {
				str += output(" - ");
				str += output_index(r->offset);
			}
		}
	}
	break;
	case CB_TAG_FIELD:
		str += output("(int)%s%d.size", CB_PREFIX_FIELD, CB_FIELD(x)->id);
		break;
	default:
		/* LCOV_EXCL_START */
		cobc_err_msg(_("unexpected tree tag: %d"), (int)CB_TREE_TAG(x));
		COBC_ABORT();
		/* LCOV_EXCL_STOP */
	}
	return str;
}

/* Picture strings */

static int
lookup_pic(const cob_pic_symbol * pic, const int length)
{
	/* Search picture string cache */
	for(pic_list * l = pic_cache; l; l = l->next) {
		if(length != l->length) {
			continue;
		}

		bool different_pic_str = false;
		for(int i = 0; i < l->length; ++i) {
			if(pic[i].symbol != l->str[i].symbol || pic[i].times_repeated != l->str[i].times_repeated) {
				different_pic_str = true;
				break;
			}
		}

		if(different_pic_str) {
			continue;
		}

		return l->id;
	}

	/* Cache new picture string */

	pic_list * l = (pic_list *) cobc_parse_malloc(sizeof(pic_list));
	l->id = cb_pic_id;
	l->length = length;
	l->str = pic;
	l->next = pic_cache;
	pic_cache = l;

	return cb_pic_id++;
}

static void
output_pic_cache(void)
{
	if(pic_cache == NULL) {
		return;
	}

	output_storage("\n/* Picture strings */\n\n");
	pic_cache = list_reverse(pic_cache);

	for(pic_list * pic = pic_cache; pic; pic = pic->next) {
		output_storage("static const cob_pic_symbol %s%d[] = {\n",
					   CB_PREFIX_PIC, pic->id);

		for(int pos = 0; pos < pic->length && pic->str[pos].symbol != '\0'; ++pos) {
			output_storage("\tcob_pic_symbol('%c', %u)",
						   pic->str[pos].symbol,
						   pic->str[pos].times_repeated);
			output_storage(",\n");
		}
		output_storage("\tcob_pic_symbol(0, 1)");
		output_storage("\n};\n");
	}
	output_storage("\n");
}

/* Attributes */

static int
lookup_attr(const int type, const cob_u32_t digits, const int scale,
			const cob_u32_t flags, cob_pic_symbol * pic, const int lenstr)
{
	const int pic_id = pic ? lookup_pic(pic, lenstr) : -1;

	/* Search attribute cache */
	for(attr_list * l = attr_cache; l; l = l->next) {
		if(type == l->type &&
				digits == l->digits &&
				scale == l->scale &&
				flags == l->flags &&
				pic_id == l->pic_id) {
			return l->id;
		}
	}

	/* Cache new attribute */

	attr_list * l = (attr_list *) cobc_parse_malloc(sizeof(attr_list));
	l->id = cb_attr_id;
	l->type = type;
	l->digits = digits;
	l->scale = scale;
	l->flags = flags;
	l->pic_id = pic_id;
	l->next = attr_cache;
	attr_cache = l;

	return cb_attr_id++;
}

static string
output_attr(const cb_tree x)
{
	string str("");

	int id = 0;
	switch(CB_TREE_TAG(x)) {
	case CB_TAG_LITERAL: {
		cb_literal * l = CB_LITERAL(x);
		if(CB_TREE_CLASS(x) == CB_CLASS_NUMERIC) {
			cob_u32_t flags = COB_FLAG_CONSTANT;
			if(l->sign != 0) {
				flags = COB_FLAG_HAVE_SIGN | COB_FLAG_SIGN_SEPARATE | COB_FLAG_SIGN_LEADING;
			}
			id = lookup_attr(COB_TYPE_NUMERIC_DISPLAY,
							 l->size, l->scale, flags, NULL, 0);
		} else {
			if(l->all) {
				id = lookup_attr(COB_TYPE_ALPHANUMERIC_ALL, 0, 0, COB_FLAG_CONSTANT, NULL, 0);
			} else {
				id = lookup_attr(COB_TYPE_ALPHANUMERIC, 0, 0, COB_FLAG_CONSTANT, NULL, 0);
			}
		}
	}
	break;
	case CB_TAG_REFERENCE: {
		cb_reference * r = CB_REFERENCE(x);
		cb_field * f = CB_FIELD(r->value);
		cob_u32_t flags = 0;
		if(r->offset) {
			id = lookup_attr(COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL, 0);
		} else {
			int type = cb_tree_type(x, f);
			switch(type) {
			case COB_TYPE_GROUP:
			case COB_TYPE_ALPHANUMERIC:
				if(f->flag_justified) {
					id = lookup_attr(type, 0, 0, COB_FLAG_JUSTIFIED, NULL, 0);
				} else {
					id = lookup_attr(type, 0, 0, 0, NULL, 0);
				}
				break;
			default:
				if(f->pic->have_sign) {
					flags |= COB_FLAG_HAVE_SIGN;
					if(f->flag_sign_separate) {
						flags |= COB_FLAG_SIGN_SEPARATE;
					}
					if(f->flag_sign_leading) {
						flags |= COB_FLAG_SIGN_LEADING;
					}
				}
				if(f->flag_blank_zero) {
					flags |= COB_FLAG_BLANK_ZERO;
				}
				if(f->flag_justified) {
					flags |= COB_FLAG_JUSTIFIED;
				}
				if(f->flag_binary_swap) {
					flags |= COB_FLAG_BINARY_SWAP;
				}
				if(f->flag_real_binary) {
					flags |= COB_FLAG_REAL_BINARY;
				}
				if(f->flag_is_pointer) {
					flags |= COB_FLAG_IS_POINTER;
				}
				if(cb_binary_truncate &&
						f->usage == CB_USAGE_BINARY &&
						!f->flag_real_binary) {
					flags |= COB_FLAG_BINARY_TRUNC;
				}

				switch(f->usage) {
				case CB_USAGE_COMP_6:
					flags |= COB_FLAG_NO_SIGN_NIBBLE;
					break;
				case CB_USAGE_DOUBLE:
				case CB_USAGE_FLOAT:
				case CB_USAGE_LONG_DOUBLE:
#if	0	/* RXWRXW - Floating ind */
				case CB_USAGE_FP_BIN32:
				case CB_USAGE_FP_BIN64:
				case CB_USAGE_FP_BIN128:
				case CB_USAGE_FP_DEC64:
				case CB_USAGE_FP_DEC128:
#endif
					flags |= COB_FLAG_IS_FP;
					break;
				default:
					break;
				}

				id = lookup_attr(type, f->pic->digits,
								 f->pic->scale, flags,
								 f->pic->str,
								 f->pic->lenstr);
				break;
			}
		}
	}
	break;
	case CB_TAG_ALPHABET_NAME:
		id = lookup_attr(COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL, 0);
		break;
	default:
		/* LCOV_EXCL_START */
		cobc_err_msg(_("unexpected tree tag: %d"), (int)CB_TREE_TAG(x));
		COBC_ABORT();
		/* LCOV_EXCL_STOP */
	}

	str += output("&%s%d", CB_PREFIX_ATTR, id);
	return str;
}

static string
output_struct(cb_field * f, int indent, string & head, bool bMain = false, bool bFake = false)
{
	string str("");
	if(bMain) {
		str = "static ";
	}
	while(f) {
		for(int i = 0; i < indent; ++i) {
			str += "\t";
		}
		if(!f->children) {
			if(f->flag_occurs) {
				str += output_str("cob_u8_t  %s[%d][%d];\t// %s\n",
								  f->sName, f->occurs_max, f->size, f->name);
			} else {
				str += output_str("cob_u8_t  %s[%d];\t// %s\n",
								  f->sName, f->size, f->name);
			}
			f = f->sister;
			continue;
		}
		if(f->sType == 0) {
			sprintf(buf, "_s%d_%s", struct_id, f->sName);
			++struct_id;
			char * nm = new char[strlen(buf) + 1];
			strcpy(nm, buf);
			f->sType = nm;
		}
		if(indent == 14) {	// We need to externalize that
			string str2 = output_struct(f, 0, head, false, true);
			head += str2;
			if(f->usage == CB_USAGE_UNION) {
				str += output_str("union %s ", f->sType);
			} else {
				str += output_str("struct %s ", f->sType);
			}
			if(f->flag_occurs) {
				str += output_str("%s[%d];\t// %s\n", f->sName, f->occurs_max, f->name);
			} else {
				str += output_str("%s;\t// %s\n", f->sName, f->name);
			}
		} else {
			if(f->usage == CB_USAGE_UNION) {
				str += output_str("union %s {\n", f->sType);
			} else {
				str += output_str("struct %s {\n", f->sType);
			}
			str += output_struct(f->children, indent + 1, head);
			for(int i = 0; i < indent; ++i) {
				str += "\t";
			}
			if(bFake) {
				str += output_str("};\t// struct %s only\n", f->sType);
			} else {
				if(f->flag_occurs) {
					str += output_str("} %s[%d];\t// %s\n", f->sName, f->occurs_max, f->name);
				} else {
					str += output_str("} %s;\t// %s\n", f->sName, f->name);
				}
			}
		}
		if(indent == 0) {
			break;
		}
		f = f->sister;
	}
	return str;
}

static string
output_field_dcl(cb_field * f)
{
	if(!f->bUseName) {
		return output_str("static cob_u8_t  %s%d[%d]%s;\t// %s\n",
						  CB_PREFIX_BASE, f->id,
						  f->memory_size, COB_ALIGN, f->name);
	}
	if(!f->children) {
		if(f->flag_occurs) {
			return output_str("static cob_u8_t  %s[%d][%d]%s;\t// %s\n",
							  f->sName, f->occurs_max, f->size, COB_ALIGN, f->name);
		}
		return output_str("static cob_u8_t  %s[%d]%s;\t// %s\n",
						  f->sName, f->memory_size, COB_ALIGN, f->name);
	}
#if defined(__SUNPRO_CC) || defined(__xlC__)
	string str("#pragma pack(1)\n");
#elif defined (__GNUC__) && __GNUC__ < 3
	string str("");
#else
	string str("#pragma pack(push, 1)\n");
#endif
	string head("");
	string str2 = output_struct(f, 0, head, true);
	str += head;
	str += str2;
#ifdef __SUNPRO_CC
	str += "#pragma pack(0)\n";
#elif defined (__GNUC__) && __GNUC__ < 3
	str += "\n";
#else
	str += "#pragma pack(pop)\n";
#endif
	return str;
}

static string
output_fake_dcl(cb_field * f)
{
#if defined(__SUNPRO_CC) || defined(__xlC__)
	string str("#pragma pack(1)\n");
#elif defined (__GNUC__) && __GNUC__ < 3
	string str("");
#else
	string str("#pragma pack(push, 1)\n");
#endif
	string head("");
	string str2 = output_struct(f, 0, head, false, true);
	str += head;
	str += str2;
#ifdef __SUNPRO_CC
	str += "#pragma pack(0)\n";
#elif defined (__GNUC__) && __GNUC__ < 3
	str += "\n";
#else
	str += "#pragma pack(pop)\n";
#endif
	return str;
}

static string
get_parm_name(cb_field * f)
{
	if(!f->bUseName) {
		return output_str("%s%d", CB_PREFIX_BASE, f->id);
	}
	if(f->parent) {
		f = f->parent;
	}
	if(!f->children) {
		if(f->flag_occurs) {
			f->bUseName = false;
			return output_str("%s%d", CB_PREFIX_BASE, f->id);
		}
		return f->sName;
	}
	if(!f->bOutDone) {
		string st = output_fake_dcl(f);
		output_storage("%s", st.c_str());
		f->bOutDone = true;
	}
	return f->sName;
}

static string
output_field(cb_tree x)
{
	string str = output("(");
	str += output_size(x);
	str += output(", ");
	str += output_data(x);
	str += output(", ");
	str += output_attr(x);
	str += output(")");
	return str;
}

static char *
user_func_upper(const char * func)
{
	char * rets = cb_encode_program_id(func);
	for(unsigned char * s = (unsigned char *)rets; *s; s++) {
		if(islower((int)*s)) {
			*s = (cob_u8_t)toupper((int) * s);
		}
	}
	return rets;
}

/* Literal */

int
cb_lookup_literal(cb_tree x, int make_decimal)
{
	cb_literal * literal = CB_LITERAL(x);
	/* Search literal cache */
	for(literal_list * l = literal_cache; l; l = l->next) {
		if(CB_TREE_CLASS(literal) == CB_TREE_CLASS(l->literal) &&
				literal->size == l->literal->size &&
				literal->all == l->literal->all &&
				literal->sign == l->literal->sign &&
				literal->scale == l->literal->scale &&
				memcmp(literal->data, l->literal->data,
					   (size_t)literal->size) == 0) {
			if(make_decimal) {
				l->make_decimal = 1;
			}
			return l->id;
		}
	}

	/* Output new literal */
	FILE * savetarget = output_target;
	output_target = NULL;
	output_field(x);
	output_target = savetarget;

	/* Cache it */
	literal_list * l = (literal_list *) cobc_parse_malloc(sizeof(literal_list));
	l->id = cb_literal_id;
	l->literal = literal;
	l->make_decimal = make_decimal;
	l->x = x;
	l->next = literal_cache;
	literal_cache = l;

	return cb_literal_id++;
}

/*
 * Should numeric literal for truncated into a PIC S9(9) BINARY field, ignoring scale?
 *  (This is the way that Micro Focus COBOL works; RJN Nov 2016)
 * - I don't want to have different behaviour for big and little endian
 */
static int
cb_fit_to_int(const cb_tree x)
{
	/*
	#ifndef WORDS_BIGENDIAN
	if (cb_binary_byteorder == CB_BYTEORDER_BIG_ENDIAN) {
		gen_num_lit_big_end = 1;
	} else {
		gen_num_lit_big_end = 0;
	}
	#else
	gen_num_lit_big_end = 1;
	#endif
	*/

	if(CB_NUMERIC_LITERAL_P(x)) {
		//if(gen_num_lit_big_end) return 1;
		cb_literal * l = CB_LITERAL(x);
		if(l->scale > 0) {
			int scale = l->scale;
			l->scale = 0;
			int sts = cb_fits_int(x);
			l->scale = scale;
			return sts;
		}
	}

	return cb_fits_int(x);
}

/* Integer */

static string
output_integer(cb_tree x)
{
	cb_field * f;
	string str("");

	switch(CB_TREE_TAG(x)) {
	case CB_TAG_CONST:
		if(x == cb_zero) {
			str = output("0");
		} else if(x == cb_null) {
			str = output("(cob_u8_ptr)NULL");
		} else {
			str = output("%s", CB_CONST(x)->val);
		}
		break;
	case CB_TAG_INTEGER:
		if(CB_INTEGER(x)->hexval) {
			str = output("0x%X", CB_INTEGER(x)->val);
		} else {
			str = output("%d", CB_INTEGER(x)->val);
		}
		break;
	case CB_TAG_LITERAL:
		str = output("%d", cb_get_int(x));
		break;
	case CB_TAG_BINARY_OP: {
		cb_binary_op * p = CB_BINARY_OP(x);
		if(p->flag) {
			if(!cb_fits_int(p->x) || !cb_fits_int(p->y)) {
				str = output("cob_get_int(");
				str += output_param(x, -1);
				str += output(")");
				break;
			}
		}
		if(p->op == '^') {
			str += output("(int) pow((double)");
			str += output_integer(p->x);
			str += output(", (int)");
			str += output_integer(p->y);
			str += output(")");
		} else {
			str += output("(");
			str += output_integer(p->x);
			str += output(" %c ", p->op);
			str += output_integer(p->y);
			str += output(")");
		}
	}
	break;
	case CB_TAG_CAST: {
		cb_cast * cp = CB_CAST(x);
		switch(cp->cast_type) {
		case CB_CAST_ADDRESS:
			str += output("(");
			str += output_data(cp->val);
			str += output(")");
			break;
		case CB_CAST_PROGRAM_POINTER:
			str += output("(unsigned char *)cob_call_field(");
			str += output_param(x, -1);
			if(current_prog->nested_prog_list) {
				gen_nested_tab = 1;
				str += output(", cob_nest_tab, 0, %d)", cb_fold_call);
			} else {
				str += output(", NULL, 0, %d)", cb_fold_call);
			}
			break;
		default:
			/* LCOV_EXCL_START */
			cobc_err_msg(_("unexpected cast type: %d"),
						 (int)cp->cast_type);
			COBC_ABORT();
			/* LCOV_EXCL_STOP */
		}
	}
	break;
	case CB_TAG_REFERENCE:
		f = cb_code_field(x);
		switch(f->usage) {
		case CB_USAGE_INDEX:
			if(f->special_index) {
				str += output_base(f, 1U);
				if(f->bUseName) {
					str += f->sName;
				} else {
					str += output("%s%d", CB_PREFIX_BASE, f->id);
				}
				return str;
			}
		/* Fall through */
		case CB_USAGE_HNDL:
		case CB_USAGE_HNDL_WINDOW:
		case CB_USAGE_HNDL_SUBWINDOW:
		case CB_USAGE_HNDL_FONT:
		case CB_USAGE_HNDL_THREAD:
		case CB_USAGE_HNDL_MENU:
		case CB_USAGE_HNDL_VARIANT:
		case CB_USAGE_HNDL_LM:
		case CB_USAGE_LENGTH:
			str += output("(*(int *)(");
			str += output_data(x);
			str += output("))");
			return str;

		case CB_USAGE_POINTER:
		case CB_USAGE_PROGRAM_POINTER:
#ifdef	COB_NON_ALIGNED
			str += output("(cob_get_pointer(");
			str += output_data(x);
			str += output("))");
#else
			str += output("(*(unsigned char **)(");
			str += output_data(x);
			str += output("))");
#endif
			return str;

		case CB_USAGE_DISPLAY:
			if(f->pic && f->pic->scale >= 0 &&
					f->size - f->pic->scale > 0 &&
					f->size - f->pic->scale <= 9 &&
					f->pic->have_sign == 0 &&
					!cb_ebcdic_sign) {
				optimize_defs[COB_GET_NUMDISP] = 1;
				str += output("cob_get_numdisp(");
				str += output_data(x);
				str += output(", %d)", f->size - f->pic->scale);
				return str;
			}
			break;

		case CB_USAGE_PACKED:
			if(f->pic->scale == 0 && f->pic->digits < 10) {
				optimize_defs[COB_GET_PACKED_INT] = 1;
				str += output_func_1("cob_get_packed_int", x);
				return str;
			}
			break;

		case CB_USAGE_BINARY:
		case CB_USAGE_COMP_5:
		case CB_USAGE_COMP_X:
			if(f->size == 1) {
				str += output("(*(");
				if(!f->pic->have_sign) {
					str += output("cob_u8_ptr)(");
				} else {
					str += output("cob_s8_ptr)(");
				}
				str += output_data(x);
				str += output("))");
				return str;
			}
#ifdef	COB_NON_ALIGNED
			if(f->storage != CB_STORAGE_LINKAGE && f->indexes == 0 && (
#ifdef	COB_SHORT_BORK
					(f->size == 2 && (f->offset % 4 == 0)) ||
#else
					(f->size == 2 && (f->offset % 2 == 0)) ||
#endif
					(f->size == 4 && (f->offset % 4 == 0)) ||
					(f->size == 8 && (f->offset % 8 == 0)))) {
#else
			if(f->size == 2 || f->size == 4 || f->size == 8) {
#endif
				if(f->flag_binary_swap) {
					str += output("((");
					switch(f->size) {
					case 2:
						if(!f->pic->have_sign) {
							str += output("unsigned short)COB_BSWAP_16(");
						} else {
							str += output("short)COB_BSWAP_16(");
						}
						break;
					case 4:
						if(!f->pic->have_sign) {
							str += output("unsigned int)COB_BSWAP_32(");
						} else {
							str += output("int)COB_BSWAP_32(");
						}
						break;
					case 8:
						if(!f->pic->have_sign) {
							str += output("cob_u64_t)COB_BSWAP_64(");
						} else {
							str += output("cob_s64_t)COB_BSWAP_64(");
						}
						break;
					default:
						break;
					}
					str += output("*(");
					switch(f->size) {
					case 2:
						str += output("short *)(");
						break;
					case 4:
						str += output("int *)(");
						break;
					case 8:
						str += output("cob_s64_t *)(");
						break;
					default:
						break;
					}
					str += output_data(x);
					str += output(")))");
					return str;
				} else {
					str += output("(*(");
					switch(f->size) {
					case 2:
						if(!f->pic->have_sign) {
							str += output("unsigned short *)(");
						} else {
							str += output("short *)(");
						}
						break;
					case 4:
						if(!f->pic->have_sign) {
							str += output("unsigned int *)(");
						} else {
							str += output("int *)(");
						}
						break;
					case 8:
						if(!f->pic->have_sign) {
							str += output("cob_u64_ptr)(");
						} else {
							str += output("cob_s64_ptr)(");
						}
						break;
					default:
						break;
					}
					str += output_data(x);
					str += output("))");
					return str;
				}
			}
			if(f->pic->have_sign == 0) {
				str += output("(unsigned int)");
			}
			break;

		default:
			break;
		}

		str += output_func_1("cob_get_int", x);
		break;
	case CB_TAG_INTRINSIC:
		str += output("cob_get_int(");
		str += output_param(x, -1);
		str += output(")");
		break;
	default:
		/* LCOV_EXCL_START */
		cobc_err_msg(_("unexpected tree tag: %d"), (int)CB_TREE_TAG(x));
		COBC_ABORT();
		/* LCOV_EXCL_STOP */
	}
	return str;
}

static string
output_long_integer(cb_tree x)
{
	cb_field * f;
	string str("");

	switch(CB_TREE_TAG(x)) {
	case CB_TAG_CONST:
		if(x == cb_zero) {
			str += output("0");
		} else if(x == cb_null) {
			str += output("(cob_u8_ptr)NULL");
		} else {
			str += output("%s", CB_CONST(x)->val);
		}
		break;
	case CB_TAG_INTEGER:
		if(CB_INTEGER(x)->hexval) {
			str += output("0x%X", CB_INTEGER(x)->val);
		} else {
			str += output("%d", CB_INTEGER(x)->val);
		}
		break;
	case CB_TAG_LITERAL:
		str += output(CB_FMT_LLD_F, cb_get_long_long(x));
		break;
	case CB_TAG_BINARY_OP: {
		cb_binary_op * p = CB_BINARY_OP(x);
		if(p->flag) {
			if(!cb_fits_long_long(p->x) ||
					!cb_fits_long_long(p->y)) {
				str += output("cob_get_llint(");
				str += output_param(x, -1);
				str += output(")");
				break;
			}
		}
		if(p->op == '^') {
			str += output("(cob_s64_t) pow((double)");
			str += output_long_integer(p->x);
			str += output(", (double)");
			str += output_long_integer(p->y);
			str += output(")");
		} else {
			str += output("(");
			str += output_long_integer(p->x);
			str += output(" %c ", p->op);
			str += output_long_integer(p->y);
			str += output(")");
		}
	}
	break;
	case CB_TAG_CAST: {
		cb_cast * cp = CB_CAST(x);
		switch(cp->cast_type) {
		case CB_CAST_ADDRESS:
			str += output("(");
			str += output_data(cp->val);
			str += output(")");
			break;
		case CB_CAST_PROGRAM_POINTER:
			str += output("(unsigned char *)cob_call_field(");
			str += output_param(x, -1);
			if(current_prog->nested_prog_list) {
				gen_nested_tab = 1;
				str += output(", cob_nest_tab, 0, %d)", cb_fold_call);
			} else {
				str += output(", NULL, 0, %d)", cb_fold_call);
			}
			break;
		default:
			/* LCOV_EXCL_START */
			cobc_err_msg(_("unexpected cast type: %d"),
						 (int)cp->cast_type);
			COBC_ABORT();
			/* LCOV_EXCL_STOP */
		}
	}
	break;
	case CB_TAG_REFERENCE:
		f = cb_code_field(x);
		switch(f->usage) {
		case CB_USAGE_INDEX:
			if(f->special_index) {
				str += output_base(f, 1U);
				if(f->bUseName) {
					str += output("(cob_s64_t)");
					str += f->sName;
				} else {
					str += output("(cob_s64_t)%s%d", CB_PREFIX_BASE, f->id);
				}
				return str;
			}
		/* Fall through */
		case CB_USAGE_HNDL:
		case CB_USAGE_HNDL_WINDOW:
		case CB_USAGE_HNDL_SUBWINDOW:
		case CB_USAGE_HNDL_FONT:
		case CB_USAGE_HNDL_THREAD:
		case CB_USAGE_HNDL_MENU:
		case CB_USAGE_HNDL_VARIANT:
		case CB_USAGE_HNDL_LM:
		case CB_USAGE_LENGTH:
			str += output("(cob_s64_t)(*(int *)(");
			str += output_data(x);
			str += output("))");
			return str;

		case CB_USAGE_POINTER:
		case CB_USAGE_PROGRAM_POINTER:
#ifdef	COB_NON_ALIGNED
			str += output("(cob_get_pointer(");
			str += output_data(x);
			str += output("))");
#else
			str += output("(*(unsigned char **)(");
			str += output_data(x);
			str += output("))");
#endif
			return str;

		case CB_USAGE_BINARY:
		case CB_USAGE_COMP_5:
		case CB_USAGE_COMP_X:
			if(f->size == 1) {
				str += output("(*(");
				if(!f->pic->have_sign) {
					str += output("cob_u8_ptr)(");
				} else {
					str += output("cob_s8_ptr)(");
				}
				str += output_data(x);
				str += output("))");
				return str;
			}
#ifdef	COB_NON_ALIGNED
			if(f->storage != CB_STORAGE_LINKAGE && f->indexes == 0 && (
#ifdef	COB_SHORT_BORK
					(f->size == 2 && (f->offset % 4 == 0)) ||
#else
					(f->size == 2 && (f->offset % 2 == 0)) ||
#endif
					(f->size == 4 && (f->offset % 4 == 0)) ||
					(f->size == 8 && (f->offset % 8 == 0)))) {
#else
			if(f->size == 2 || f->size == 4 || f->size == 8) {
#endif
				if(f->flag_binary_swap) {
					str += output("((");
					switch(f->size) {
					case 2:
						if(!f->pic->have_sign) {
							str += output("unsigned short)COB_BSWAP_16(");
						} else {
							str += output("short)COB_BSWAP_16(");
						}
						break;
					case 4:
						if(!f->pic->have_sign) {
							str += output("unsigned int)COB_BSWAP_32(");
						} else {
							str += output("int)COB_BSWAP_32(");
						}
						break;
					case 8:
						if(!f->pic->have_sign) {
							str += output("cob_u64_t)COB_BSWAP_64(");
						} else {
							str += output("cob_s64_t)COB_BSWAP_64(");
						}
						break;
					default:
						break;
					}
					str += output("*(");
					switch(f->size) {
					case 2:
						str += output("short *)(");
						break;
					case 4:
						str += output("int *)(");
						break;
					case 8:
						str += output("cob_s64_t *)(");
						break;
					default:
						break;
					}
					str += output_data(x);
					str += output(")))");
					return str;
				} else {
					str += output("(*(");
					switch(f->size) {
					case 2:
						if(!f->pic->have_sign) {
							str += output("unsigned short *)(");
						} else {
							str += output("short *)(");
						}
						break;
					case 4:
						if(!f->pic->have_sign) {
							str += output("unsigned int *)(");
						} else {
							str += output("int *)(");
						}
						break;
					case 8:
						if(!f->pic->have_sign) {
							str += output("cob_u64_ptr)(");
						} else {
							str += output("cob_s64_ptr)(");
						}
						break;
					default:
						break;
					}
					str += output_data(x);
					str += output("))");
					return str;
				}
			}
#if	0	/* RXWRXW - unsigned */
			if(f->pic->have_sign == 0) {
				str += output("(unsigned int)");
			}
#endif
			break;

		default:
			break;
		}

		str += output_func_1("cob_get_llint", x);
		break;
	case CB_TAG_INTRINSIC:
		str += output("cob_get_llint(");
		str += output_param(x, -1);
		str += output(")");
		break;
	default:
		/* LCOV_EXCL_START */
		cobc_err_msg(_("unexpected tree tag: %d"), (int)CB_TREE_TAG(x));
		COBC_ABORT();
		/* LCOV_EXCL_STOP */
	}
	return str;
}

static string
output_index(cb_tree x)
{
	string str("");
	switch(CB_TREE_TAG(x)) {
	case CB_TAG_INTEGER:
		str += output("%d", CB_INTEGER(x)->val - 1);
		break;
	case CB_TAG_LITERAL:
		str += output("%d", cb_get_int(x) - 1);
		break;
	default:
		str += output("(");
		if(CB_TREE_TAG(x) == CB_TAG_REFERENCE) {
			cb_field * f = cb_code_field(x);
			if(f->pic && f->pic->have_sign == 0) {	/* Avoid ((unsigned int)(0 - 1)) */
				f->pic->have_sign = 1;	/* Handle subscript as signed */
				str += output_integer(x);
				f->pic->have_sign = 0;	/* Restore to unsigned */
			} else {
				str += output_integer(x);
			}
		} else {
			str += output_integer(x);
		}
		str += output(" - 1)");
		break;
	}
	return str;
}

/* Parameter */

static string
output_param(cb_tree x, int id)
{
	string str("");
	cb_reference 	*	r;
	cb_field 	*		f;
	cb_field 	*		ff;
	cb_cast 	*		cp;
	cb_binary_op 	*	bp;
	cb_intrinsic 	*	ip;
	cb_alphabet_name *	abp;

	param_id = id;

	if(x == NULL) {
		str += output("NULL");
		return str;
	}

	switch(CB_TREE_TAG(x)) {
	case CB_TAG_CONST:
		if(x == cb_quote) {
			gen_figurative |= CB_NEED_QUOTE;
		} else if(x == cb_norm_low) {
			gen_figurative |= CB_NEED_LOW;
		} else if(x == cb_norm_high) {
			gen_figurative |= CB_NEED_HIGH;
		} else if(x == cb_space) {
			gen_figurative |= CB_NEED_SPACE;
		} else if(x == cb_zero) {
			gen_figurative |= CB_NEED_ZERO;
		}
		str += output("%s", CB_CONST(x)->val);
		break;
	case CB_TAG_INTEGER:
		str += output_integer(x);
		break;
	case CB_TAG_STRING:
		str += output_string(CB_STRING(x)->data, (int) CB_STRING(x)->size, 0);
		break;
	case CB_TAG_LOCALE_NAME:
		str += output_param(CB_LOCALE_NAME(x)->list, id);
		break;
	case CB_TAG_ALPHABET_NAME:
		abp = CB_ALPHABET_NAME(x);
		switch(abp->alphabet_type) {
		case CB_ALPHABET_ASCII:
#ifdef	COB_EBCDIC_MACHINE
			gen_ebcdic_ascii = 1;
			str += output("cob_ebcdic_ascii");
			break;
#endif
		/* Fall through for ASCII */
		case CB_ALPHABET_NATIVE:
			if(current_prog->collating_sequence) {
				gen_native = 1;
				str += output("cob_native");
			} else {
				str += output("NULL");
			}
			break;
		case CB_ALPHABET_EBCDIC:
#ifdef	COB_EBCDIC_MACHINE
			if(current_prog->collating_sequence) {
				gen_native = 1;
				str += output("cob_native");
			} else {
				str += output("NULL");
			}
#else
			if(cb_flag_alt_ebcdic) {
				gen_alt_ebcdic = 1;
				str += output("cob_a2e");
			} else {
				gen_full_ebcdic = 1;
				str += output("cob_ascii_ebcdic");
			}
#endif
			break;
		case CB_ALPHABET_CUSTOM:
			gen_custom = 1;
			str += output("%s%s", CB_PREFIX_SEQUENCE, abp->cname);
			break;
		default:
			break;
		}
		break;
	case CB_TAG_CAST:
		cp = CB_CAST(x);
		switch(cp->cast_type) {
		case CB_CAST_INTEGER:
			str += output_integer(cp->val);
			break;
		case CB_CAST_LONG_INT:
			str += output_long_integer(cp->val);
			break;
		case CB_CAST_ADDRESS:
			str += output_data(cp->val);
			break;
		case CB_CAST_ADDR_OF_ADDR: {
			string nm = output_data(cp->val);
			char c = (nm.length() == 0) ? 0 : nm.at(0);
			if(c != '(') {
				str += output("&");
				str += nm;
			} else {
				size_t ix = nm.find(')');
				str += nm.substr(0, ix);
				str += "*)&";
				str += nm.substr(ix + 1);
			}
		}
		break;
		case CB_CAST_LENGTH:
			str += output_size(cp->val);
			break;
		case CB_CAST_PROGRAM_POINTER:
			str += output_param(cp->val, id);
			break;
		default:
			break;
		}
		break;
	case CB_TAG_DECIMAL:
		str += output("_d%d", CB_DECIMAL(x)->id);
		break;
	case CB_TAG_DECIMAL_LITERAL:
		str += output("%s%d", CB_PREFIX_DEC_CONST, CB_DECIMAL_LITERAL(x)->id);
		break;
	case CB_TAG_FILE:
		str += output("%s%s", CB_PREFIX_FILE, CB_FILE(x)->cname);
		break;
	case CB_TAG_LITERAL:
		if(nolitcast) {
			str += output("&%s%d", CB_PREFIX_CONST, cb_lookup_literal(x, 0));
		} else {
			str += output("(cob_field *)&%s%d", CB_PREFIX_CONST,
						  cb_lookup_literal(x, 0));
		}
		break;
	case CB_TAG_FIELD:
		/* TODO: remove me */
		str += output_param(cb_build_field_reference(CB_FIELD(x), NULL), id);
		break;
	case CB_TAG_REFERENCE:
		r = CB_REFERENCE(x);
		if(CB_LOCALE_NAME_P(r->value)) {
			str += output_param(CB_LOCALE_NAME(r->value)->list, id);
			break;
		}
		if(r->check) {
			inside_stack[inside_check++] = 0;
			if(inside_check >= COB_INSIDE_SIZE) {
				/* LCOV_EXCL_START */
				cobc_err_msg(_("internal statement stack depth exceeded: %d"),
							 COB_INSIDE_SIZE);
				COBC_ABORT();
				/* LCOV_EXCL_STOP */
			}
			str += output("\n");
			str += output_prefix();
			str += output("(");
			int n = output_indent_level;
			output_indent_level = 0;
			for(cb_tree l = r->check; l; l = CB_CHAIN(l)) {
				int sav_stack_id = stack_id;
				str += output_stmt(CB_VALUE(l));
				stack_id = sav_stack_id;
				if(l == r->check) {
					output_indent_level = n;
				}
			}
		}

		if(CB_FILE_P(r->value)) {
			str += output("%s%s", CB_PREFIX_FILE, CB_FILE(r->value)->cname);
			if(r->check) {
				if(inside_check) {
					--inside_check;
				}
				str += output(" )");
			}
			break;
		}
		if(CB_ALPHABET_NAME_P(r->value)) {
			cb_alphabet_name * rbp = CB_ALPHABET_NAME(r->value);
			switch(rbp->alphabet_type) {
			case CB_ALPHABET_ASCII:
#ifdef	COB_EBCDIC_MACHINE
				gen_ebcdic_ascii = 2;
				str += output("&f_ebcdic_ascii");
				break;
#endif
			/* Fall through for ASCII */
			case CB_ALPHABET_NATIVE:
				gen_native = 2;
				str += output("&f_native");
				break;
			case CB_ALPHABET_EBCDIC:
#ifdef	COB_EBCDIC_MACHINE
				gen_native = 2;
				str += output("&f_native");
#else
				gen_full_ebcdic = 2;
				str += output("&f_ascii_ebcdic");
#endif
				break;
			case CB_ALPHABET_CUSTOM:
				gen_custom = 1;
				str += output("&%s%s", CB_PREFIX_FIELD, rbp->cname);
				break;
			default:
				break;
			}
			if(r->check) {
				if(inside_check) {
					--inside_check;
				}
				str += output(" )");
			}
			break;
		}

		if(!CB_FIELD_P(r->value)) {
			/* LCOV_EXCL_START */
			cobc_err_msg(_("call to '%s' with invalid parameter '%s'"),
						 "output_param", "x");
			cobc_err_msg(_("%s is not a field"), r->word->name);
			cobc_err_msg(_("Please report this!"));
			COBC_ABORT();
			/* LCOV_EXCL_STOP */
		}

		f = CB_FIELD(r->value);

		ff = real_field_founder(f);

		if(ff->flag_external) {
			f->flag_external = 1;
			f->flag_local = 1;
		} else if(ff->flag_item_based) {
			f->flag_local = 1;
		}

		if(!r->subs && !r->offset && f->count > 0 &&
				!chk_field_variable_size(f) &&
				!chk_field_variable_address(f)) {
			if(!f->flag_field) {
				FILE * savetarget = output_target;
				output_target = NULL;
				str += output_field(x);

				field_list * fl = (field_list *) cobc_parse_malloc(sizeof(field_list));
				fl->x = x;
				fl->f = f;
				fl->curr_prog = excp_current_program_id;
				if(f->special_index != 2U && (f->flag_is_global ||
											  current_prog->flag_file_global)) {
					fl->next = field_cache;
					field_cache = fl;
				} else {
					fl->next = local_field_cache;
					local_field_cache = fl;
				}

				f->flag_field = 1;
				output_target = savetarget;
			}
			if(f->flag_local) {
#if	0	/* RXWRXW - Any data pointer */
				if(f->flag_any_length && f->flag_anylen_done) {
					str += output("&%s%d",
								  CB_PREFIX_FIELD, f->id);
				} else {
#endif
					str += output("COB_SET_DATA(%s%d, ",
								  CB_PREFIX_FIELD, f->id);
					str += output_data(x);
					str += output(")");
#if	0	/* RXWRXW - Any data pointer */
					f->flag_anylen_done = 1;
				}
#endif
			} else {
				if(screenptr && f->storage == CB_STORAGE_SCREEN) {
					str += output("&s_%d", f->id);
				} else {
					str += output("&%s%d", CB_PREFIX_FIELD, f->id);
				}
			}
		} else {
			if(stack_id >= num_cob_fields) {
				num_cob_fields = stack_id + 1;
			}
			char fname[12];
			sprintf(fname, "_f%d", stack_id++);
			if(inside_check != 0) {
				if(inside_stack[inside_check - 1] != 0) {
					inside_stack[inside_check - 1] = 0;
					str += output(",\n");
					str += output_prefix();
				}
			}
			str += output("COB_SET_FLD(%s, ", fname);
			str += output_size(x);
			str += output(", ");
			str += output_data(x);
			str += output(", ");
			str += output_attr(x);
			str += output(")");
		}

		if(r->check) {
			if(inside_check) {
				--inside_check;
			}
			str += output(" )");
		}
		break;
	case CB_TAG_BINARY_OP:
		bp = CB_BINARY_OP(x);
		str += output("cob_intr_binop(");
		str += output_param(bp->x, id);
		str += output(", ");
		str += output("%d", bp->op);
		str += output(", ");
		str += output_param(bp->y, id);
		str += output(")");
		break;
	case CB_TAG_INTRINSIC:
		ip = CB_INTRINSIC(x);
		if(ip->isuser) {
			char * func = user_func_upper(CB_PROTOTYPE(cb_ref(ip->name))->ext_name);
			lookup_func_call(func);
#if	0	/* RXWRXW Func */
			str += output("cob_user_function(func_%s, &cob_dyn_%u, ",
						  func, gen_dynamic);
#else
			str += output("func_%s.funcfld(&cob_dyn_%u",
						  func, gen_dynamic);
#endif
			gen_dynamic++;
			if(ip->intr_field || ip->args) {
				str += output(", ");
			}
#if	0	/* RXWRXW Func */
			if(ip->intr_tab->refmod) {
				if(ip->offset) {
					str += output_integer(ip->offset);
					str += output(", ");
				} else {
					str += output("0, ");
				}
				if(ip->length) {
					str += output_integer(ip->length);
				} else {
					str += output("0");
				}
				if(ip->intr_field || ip->args) {
					str += output(", ");
				}
			}
#endif
		} else {
			str += output("%s(", ip->intr_tab->intr_routine);
			if(ip->intr_tab->refmod) {
				if(ip->offset) {
					str += output_integer(ip->offset);
					str += output(", ");
				} else {
					str += output("0, ");
				}
				if(ip->length) {
					str += output_integer(ip->length);
				} else {
					str += output("0");
				}
				if(ip->intr_field || ip->args) {
					str += output(", ");
				}
			}
		}
		if(ip->intr_field) {
			if(ip->intr_field == cb_int0) {
				str += output("NULL");
			} else if(ip->intr_field == cb_int1) {
				str += output("%u", cb_list_length(ip->args));
			} else {
				str += output_param(ip->intr_field, id);
			}
			if(ip->args) {
				str += output(", ");
			}
		}
		for(cb_tree l = ip->args; l; l = CB_CHAIN(l)) {
			str += output_param(CB_VALUE(l), id);
			id++;
			param_id++;
			if(CB_CHAIN(l)) {
				str += output(", ");
			}
		}
		str += output(")");
		break;
	case CB_TAG_FUNCALL:
		str += output_funcall(x);
		break;
	default:
		/* LCOV_EXCL_START */
		cobc_err_msg(_("unexpected tree tag: %d"), (int)CB_TREE_TAG(x));
		COBC_ABORT();
		/* LCOV_EXCL_STOP */
	}
	return str;
}

/* Function call */

static string
output_funcall(cb_tree x)
{
	string str("");

	cb_funcall * p = CB_FUNCALL(x);
	if(p->name[0] == '$') {
		switch(p->name[1]) {
		case 'E':
			/* Set of one character */
			str += output("*(");
			str += output_data(p->argv[0]);
			str += output(") = ");
			str += output_param(p->argv[1], 1);
			break;
		case 'F':
			/* Move of one character */
			str += output("*(");
			str += output_data(p->argv[0]);
			str += output(") = *(");
			str += output_data(p->argv[1]);
			str += output(")");
			break;
		case 'G':
			/* Test of one character */
			str += output("(int)(*(");
			str += output_data(p->argv[0]);
			if(p->argv[1] == cb_space) {
				str += output(") - ' ')");
			} else if(p->argv[1] == cb_zero) {
				str += output(") - '0')");
			} else if(p->argv[1] == cb_low) {
				str += output("))");
			} else if(p->argv[1] == cb_high) {
				str += output(") - 255)");
			} else if(CB_LITERAL_P(p->argv[1])) {
				str += output(") - %d)", *(CB_LITERAL(p->argv[1])->data));
			} else {
				str += output(") - *(");
				str += output_data(p->argv[1]);
				str += output("))");
			}
			break;
		default:
			/* LCOV_EXCL_START */
			cobc_err_msg(_("unexpected function: %s"), p->name);
			COBC_ABORT();
			/* LCOV_EXCL_STOP */
		}
		return str;
	}
	screenptr = p->screenptr;
	str += output("%s(", p->name);
	for(int i = 0; i < p->argc; i++) {
		if(p->varcnt && i + 1 == p->argc) {
			str += output("%d, ", p->varcnt);
			for(cb_tree l = p->argv[i]; l; l = CB_CHAIN(l)) {
				if(CB_VALUE(l) && CB_LITERAL_P(CB_VALUE(l))) {
					nolitcast = p->nolitcast;
				}
				str += output_param(CB_VALUE(l), i);
				nolitcast = 0;
				i++;
				if(CB_CHAIN(l)) {
					str += output(", ");
				}
			}
		} else {
			if(p->argv[i] && CB_LITERAL_P(p->argv[i])) {
				nolitcast = p->nolitcast;
			}
			str += output_param(p->argv[i], i);
			nolitcast = 0;
			if(i + 1 < p->argc) {
				str += output(", ");
			}
		}
	}
	str += output(")");
	nolitcast = 0;
	screenptr = 0;
	return str;
}

static string
output_func_1(const char * name, cb_tree x)
{
	string str = output("%s(", name);
	str += output_param(x, param_id);
	str += output(")");
	return str;
}

/* Condition */

static string
output_cond(cb_tree x, const int save_flag)
{
	cb_binary_op * p;
	string str("");

	switch(CB_TREE_TAG(x)) {
	case CB_TAG_CONST:
		if(x == cb_true) {
			str += output("1");
		} else if(x == cb_false) {
			str += output("0");
		} else {
			/* LCOV_EXCL_START */
			cobc_err_msg("invalid constant");
			COBC_ABORT();
			/* LCOV_EXCL_STOP */
		}
		break;
	case CB_TAG_BINARY_OP:
		p = CB_BINARY_OP(x);
		switch(p->op) {
		case '!':
			str += output("!");
			str += output_cond(p->x, save_flag);
			break;

		case '&':
		case '|':
			str += output("(");
			str += output_cond(p->x, save_flag);
			str += output(p->op == '&' ? " && " : " || ");
			str += output_newline();
			str += output_prefix();
			str += output("    ");
			str += output_cond(p->y, save_flag);
			str += output(")");
			break;

		case '=':
		case '<':
		case '[':
		case '>':
		case ']':
		case '~':
			str += output("((int)");
			str += output_cond(p->x, save_flag);
			switch(p->op) {
			case '=':
				str += output(" == 0");
				break;
			case '<':
				str += output(" < 0");
				break;
			case '[':
				str += output(" <= 0");
				break;
			case '>':
				str += output(" > 0");
				break;
			case ']':
				str += output(" >= 0");
				break;
			case '~':
				str += output(" != 0");
				break;
			default:
				/* FIXME - Check */
				break;
			}
			str += output(")");
			break;

		default:
			str += output_integer(x);
			break;
		}
		break;
	case CB_TAG_FUNCALL:
		if(save_flag) {
			str += output("(ret = ");
		}
		str += output_funcall(x);
		if(save_flag) {
			str += output(")");
		}
		break;
	case CB_TAG_LIST:
		if(save_flag) {
			str += output("(ret = ");
		}
		inside_stack[inside_check++] = 0;
		if(inside_check >= COB_INSIDE_SIZE) {
			/* LCOV_EXCL_START */
			cobc_err_msg(_("internal statement stack depth exceeded: %d"),
						 COB_INSIDE_SIZE);
			COBC_ABORT();
			/* LCOV_EXCL_STOP */
		}
		str += output("(\n");
		for(; x; x = CB_CHAIN(x)) {
			str += output_stmt(CB_VALUE(x));
		}
		if(inside_check) {
			--inside_check;
		}
		str += output(")");
		if(save_flag) {
			str += output(")");
		}
		break;
	default:
		/* LCOV_EXCL_START */
		cobc_err_msg(_("unexpected tree tag: %d"), (int)CB_TREE_TAG(x));
		COBC_ABORT();
		/* LCOV_EXCL_STOP */
	}
	return str;
}

/* MOVE */

static string
output_move(cb_tree src, cb_tree dst)
{
	string str("");

	/* Suppress warnings */
	suppress_warn = 1;
	cb_tree x = cb_build_move(src, dst);
	if(x != cb_error_node) {
		str += output_stmt(x);
	}
	suppress_warn = 0;
	return str;
}

/* INITIALIZE */

static int
initialize_type(cb_initialize * p, cb_field * f, const int topfield)
{
	if(f->flag_item_78) {
		/* LCOV_EXCL_START */
		cobc_err_msg(_("unexpected CONSTANT item"));
		COBC_ABORT();
		/* LCOV_EXCL_STOP */
	}

	if(f->flag_external && !p->flag_init_statement) {
		return INITIALIZE_NONE;
	}

	if(f->redefines && (!topfield || !p->flag_init_statement)) {
		return INITIALIZE_NONE;
	}

	if(f->flag_filler && p->flag_no_filler_init && !f->children) {
		return INITIALIZE_NONE;
	}

	if(p->val && f->values) {
		return INITIALIZE_ONE;
	}

	if(p->var && CB_REFERENCE_P(p->var)
			&& CB_REFERENCE(p->var)->offset) {
		/* Reference modified item */
		return INITIALIZE_ONE;
	}

	if(f->children) {
		int type = initialize_type(p, f->children, 0);
		if(type == INITIALIZE_ONE) {
			return INITIALIZE_COMPOUND;
		}
		for(f = f->children->sister; f; f = f->sister) {
			if(type != initialize_type(p, f, 0)) {
				return INITIALIZE_COMPOUND;
			}
		}
		return type;
	} else {
		for(cb_tree l = p->rep; l; l = CB_CHAIN(l)) {
			if((int)CB_PURPOSE_INT(l) == (int)CB_TREE_CATEGORY(f)) {
				return INITIALIZE_ONE;
			}
		}
	}

	if(p->flag_default) {
		if(cb_default_byte >= 0 && !p->flag_init_statement) {
			return INITIALIZE_DEFAULT;
		}
		switch(f->usage) {
		case CB_USAGE_FLOAT:
		case CB_USAGE_DOUBLE:
		case CB_USAGE_LONG_DOUBLE:
		case CB_USAGE_FP_BIN32:
		case CB_USAGE_FP_BIN64:
		case CB_USAGE_FP_BIN128:
		case CB_USAGE_FP_DEC64:
		case CB_USAGE_FP_DEC128:
			return INITIALIZE_ONE;
		default:
			break;
		}
		switch(CB_TREE_CATEGORY(f)) {
		case CB_CATEGORY_NUMERIC_EDITED:
		case CB_CATEGORY_ALPHANUMERIC_EDITED:
		case CB_CATEGORY_NATIONAL_EDITED:
			return INITIALIZE_ONE;
		default:
			if(cb_tree_type(f, f) == COB_TYPE_NUMERIC_PACKED) {
				return INITIALIZE_ONE;
			} else {
				return INITIALIZE_DEFAULT;
			}
		}
	}

	return INITIALIZE_NONE;
}

static int
initialize_uniform_char(cb_field * f, const cb_initialize * p)
{
	if(f->children) {
		int c = initialize_uniform_char(f->children, p);
		for(f = f->children->sister; f; f = f->sister) {
			if(!f->redefines) {
				if(c != initialize_uniform_char(f, p)) {
					return -1;
				}
			}
		}
		return c;
	} else {
		if(cb_default_byte >= 0 && !p->flag_init_statement) {
			return cb_default_byte;
		}
		switch(cb_tree_type(f, f)) {
		case COB_TYPE_NUMERIC_BINARY:
			return 0;
		case COB_TYPE_NUMERIC_DISPLAY:
			return '0';
		case COB_TYPE_ALPHANUMERIC:
			return ' ';
		default:
			return -1;
		}
	}
}

static string
output_figurative(cb_tree x, const cb_field * f, const int value,
				  const int init_occurs)
{
	string str = output_prefix();
	/* Check for non-standard 01 OCCURS */
	if(init_occurs) {
		str += output("memset(");
		str += output_data(x);
		str += output(", %d, %d);\n", value, f->occurs_max);
	} else if(f->size == 1) {
		str += output("*(cob_u8_ptr)(");
		str += output_data(x);
		str += output(") = %d;\n", value);
	} else {
		str += output("memset(");
		str += output_data(x);
		if(CB_REFERENCE_P(x) && CB_REFERENCE(x)->length) {
			str += output(", %d, ", value);
			str += output_size(x);
			str += output(");\n");
		} else {
			str += output(", %d, %d);\n", value, f->size);
		}
	}
	return str;
}

static string
output_initialize_literal(cb_tree x, cb_field * f,
						  cb_literal * l, const int init_occurs)
{
	string str("");
	int size;
	int lsize;

	/* Check for non-standard 01 OCCURS */
	if(init_occurs) {
		size = f->occurs_max;
		lsize = (int)l->size;
		/* Check truncated literal */
		if(lsize > f->size) {
			lsize = f->size;
		}
	} else {
		size = f->size;
		lsize = (int)l->size;
	}
	if(lsize == 1) {
		str += output_prefix();
		str += output("memset(");
		str += output_data(x);
		if(CB_REFERENCE_P(x) && CB_REFERENCE(x)->length) {
			str += output(", %d, ", l->data[0]);
			str += output_size(x);
			str += output(");\n");
		} else {
			str += output(", %d, %d);\n", l->data[0], size);
		}
		return str;
	}
	if(lsize >= size) {
		str += output_prefix();
		str += output("memcpy(");
		str += output_data(x);
		str += output(", ");
		str += output_string(l->data, size, l->llit);
		str += output(", %d);\n", size);
		return str;
	}
	int i = size / lsize;
	i_counters[0] = 1;
#ifdef _MSC_VER
	str += output_indent("{");
#endif
	str += output_line("for(_i0 = 0; _i0 < %d; _i0++)", i);
	str += output_indent("{");
	str += output_prefix();
	str += output("memcpy(");
	str += output_data(x);
	str += output(" +(_i0 * %d), ", lsize);
	str += output_string(l->data, lsize, l->llit);
	str += output(", %d);\n", lsize);
	str += output_indent("}");
#ifdef _MSC_VER
	str += output_indent("}");
#endif
	int n = size % lsize;
	if(n) {
		str += output_prefix();
		str += output("memcpy(");
		str += output_data(x);
		str += output(" +(_i0 * %d), ", lsize);
		str += output_string(l->data, n, l->llit);
		str += output(", %d);\n", n);
	}
	return str;
}

static string
output_initialize_fp_bindec(cb_tree x, cb_field * f)
{
	string str = output_prefix();
	str += output("memset(");
	str += output_data(x);
	str += output(", 0, %d);\n", (int)f->size);
	return str;
}

static string
output_initialize_fp(cb_tree x, cb_field * f)
{
	string str = output_prefix();
	if(f->usage == CB_USAGE_FLOAT) {
		str += output("{float temp = 0.0;");
	} else {
		str += output("{double temp = 0.0;");
	}
	str += output(" memcpy(");
	str += output_data(x);
	str += output(", (void *)&temp, sizeof(temp));}\n");
	return str;
}

static string
output_initialize_uniform(cb_tree x, const int c, const int size)
{
	string str = output_prefix();
	if(size == 1) {
		str += output("*(cob_u8_ptr)(");
		str += output_data(x);
		str += output(") = %d;\n", c);
	} else {
		str += output("memset(");
		str += output_data(x);
		if(CB_REFERENCE_P(x) && CB_REFERENCE(x)->length) {
			str += output(", %d, ", c);
			str += output_size(x);
			str += output(");\n");
		} else {
			str += output(", %d, %d);\n", c, size);
		}
	}
	return str;
}

static string
output_initialize_chaining(cb_field * f, cb_initialize * p)
{
	/* only handle CHAINING for program initialization*/
	if(p->flag_init_statement) {
		return "";
	}
	/* Note: CHAINING must be an extra initialization step as parameters not passed
	         must have standard initialization */
	if(f->flag_chained) {
		string str = output_prefix();
		str += output("cob_chain_setup(");
		str += output_data(p->var);
		str += output(", %d, %d);\n", f->param_num, f->size);
		return str;
	}
	return "";
}

static string
output_initialize_one(cb_initialize * p, cb_tree x)
{
	cb_field * f = cb_code_field(x);

	/* Initialize by value */
	if(p->val && f->values) {
		cb_tree value = CB_VALUE(f->values);
		/* Check for non-standard OCCURS */
		int init_occurs;
		if((f->level == 1 || f->level == 77) &&
				f->flag_occurs && !p->flag_init_statement) {
			init_occurs = 1;
		} else {
			init_occurs = 0;
		}
		if(value == cb_space) {
			return output_figurative(x, f, ' ', init_occurs);
		} else if(value == cb_low) {
			return output_figurative(x, f, 0, init_occurs);
		} else if(value == cb_high) {
			return output_figurative(x, f, 255, init_occurs);
		} else if(value == cb_quote) {
			if(cb_flag_apostrophe) {
				return output_figurative(x, f, '\'', init_occurs);
			}
			return output_figurative(x, f, '"', init_occurs);
		} else if(value == cb_zero && f->usage == CB_USAGE_DISPLAY) {
			if(!f->flag_sign_separate && !f->flag_blank_zero) {
				return output_figurative(x, f, '0', init_occurs);
			}
			return output_move(cb_zero, x);
		} else if(value == cb_null && f->usage == CB_USAGE_DISPLAY) {
			return output_figurative(x, f, 0, init_occurs);
		} else if(CB_LITERAL_P(value) && CB_LITERAL(value)->all) {
			/* ALL literal */
			return output_initialize_literal(x, f,
											 CB_LITERAL(value), init_occurs);
		} else if(CB_CONST_P(value) ||
				  CB_TREE_CLASS(value) == CB_CLASS_NUMERIC) {
			string str("");
			/* Figurative literal, numeric literal */
			/* Check for non-standard 01 OCCURS */
			if(init_occurs) {
				i_counters[0] = 1;
#ifdef _MSC_VER
				str += output_indent("{");
#endif
				str += output_line("for(_i0 = 1; _i0 <= %d; _i0++)",
								   f->occurs_max);
				str += output_indent("{");
				CB_REFERENCE(x)->subs =
				CB_BUILD_CHAIN(cb_i[0], CB_REFERENCE(x)->subs);
				str += output_move(value, x);
				CB_REFERENCE(x)->subs =
				CB_CHAIN(CB_REFERENCE(x)->subs);
				str += output_indent("}");
#ifdef _MSC_VER
				str += output_indent("}");
#endif
			} else {
				str += output_move(value, x);
			}
			return str;
		}
		/* Alphanumeric literal */
		/* We do not use output_move here because
		   we do not want to have the value be edited. */

		cb_literal * l = CB_LITERAL(value);

		/* Check for non-standard 01 OCCURS */
		if(init_occurs) {
			return output_initialize_literal(x, f, l, 1);
		}

		int size = f->size;

		if(size == 1) {
			string str = output_prefix();
			str += output("*(cob_u8_ptr)(");
			str += output_data(x);
			str += output(") = %u;\n", l->data[0]);
			return str;
		}

		unsigned char buffchar = l->data[0];
		size_t lsize;
		for(lsize = 0; lsize < l->size; lsize++) {
			if(l->data[lsize] != buffchar) {
				break;
			}
		}
		if(lsize == l->size) {
			string str = output_prefix();
			str += output("memset(");
			str += output_data(x);
			str += output(", %u, %d);\n", (unsigned int)buffchar,
						  (int)lsize);
			if((int)l->size < (int)size) {
				str += output_prefix();
				str += output("memset(");
				str += output_data(x);
				str += output(" + %d, ' ', %d);\n",
							  (int)lsize, (int)(size - lsize));
			}
			return str;
		}

		if(size > litsize) {
			litsize = size + 128;
			if(litbuff) {
				litbuff = (unsigned char *) cobc_main_realloc(litbuff, (size_t)litsize);
			} else {
				litbuff = (unsigned char *) cobc_main_malloc((size_t)litsize);
			}
		}

		if((int)l->size >= (int)size) {
			memcpy(litbuff, l->data, (size_t)size);
		} else {
			memcpy(litbuff, l->data, (size_t)l->size);
			memset(litbuff + l->size, ' ', (size_t)(size - l->size));
		}

		buffchar = *(litbuff + size - 1);
		int n = 0;
		int i;
		for(i = size - 1; i >= 0; i--, n++) {
			if(*(litbuff + i) != buffchar) {
				break;
			}
		}
		if(i < 0) {
			string str = output_prefix();
			str += output("memset(");
			str += output_data(x);
			str += output(", %u, %d);\n", (unsigned int)buffchar, size);
			return str;
		}
		int offset;
		if(n > 2) {
			offset = size - n;
			size -= n;
		} else {
			offset = 0;
		}
		string str("");
		cob_u32_t inci = 0;
		for(; size > 509; size -= 509, inci += 509) {
			str += output_prefix();
			str += output("memcpy(");
			str += output_data(x);
			if(!inci) {
				str += output(", ");
			} else {
				str += output(" + %u, ", inci);
			}
			str += output_string(litbuff + inci, 509, l->llit);
			str += output(", 509);\n");
		}

		str += output_prefix();
		str += output("memcpy(");
		str += output_data(x);
		if(!inci) {
			str += output(", ");
		} else {
			str += output(" + %u, ", inci);
		}
		str += output_string(litbuff + inci, size, l->llit);
		str += output(", %d);\n", size);

		if(offset) {
			str += output_prefix();
			str += output("memset(");
			str += output_data(x);
			str += output(" + %d, %u, %d);\n",
						  offset, (unsigned int)buffchar, n);
		}
		return str;
	}

	/* Initialize replacing */
	if(!f->children) {
		for(cb_tree lrp = p->rep; lrp; lrp = CB_CHAIN(lrp)) {
			if((int)CB_PURPOSE_INT(lrp) == (int)CB_TREE_CATEGORY(x)) {
				return output_move(CB_VALUE(lrp), x);
			}
		}
	}

	/* Initialize by default */
	if(p->flag_default) {
		switch(f->usage) {
		case CB_USAGE_FLOAT:
		case CB_USAGE_DOUBLE:
		case CB_USAGE_LONG_DOUBLE:
			return output_initialize_fp(x, f);
		case CB_USAGE_FP_BIN32:
		case CB_USAGE_FP_BIN64:
		case CB_USAGE_FP_BIN128:
		case CB_USAGE_FP_DEC64:
		case CB_USAGE_FP_DEC128:
			return output_initialize_fp_bindec(x, f);
		default:
			break;
		}
		switch(CB_TREE_CATEGORY(x)) {
		case CB_CATEGORY_NUMERIC:
		case CB_CATEGORY_NUMERIC_EDITED:
			return output_move(cb_zero, x);
		case CB_CATEGORY_ALPHANUMERIC:
		case CB_CATEGORY_ALPHANUMERIC_EDITED:
		case CB_CATEGORY_NATIONAL:
		case CB_CATEGORY_NATIONAL_EDITED:
			return output_move(cb_space, x);
		default:
			/* LCOV_EXCL_START */
			cobc_err_msg(_("unexpected tree category: %d"),
						 (int)CB_TREE_CATEGORY(x));
			COBC_ABORT();
			/* LCOV_EXCL_STOP */
		}
	}
	return "";
}

static string
output_initialize_compound(cb_initialize * p, cb_tree x)
{
	string str("");

	cb_field * ff = cb_code_field(x);
	for(cb_field * f = ff->children; f; f = f->sister) {
		int type = initialize_type(p, f, 0);
		cb_tree c = cb_build_field_reference(f, x);
		cb_field * last_field;
		int last_char;
		switch(type) {
		case INITIALIZE_NONE:
			break;
		case INITIALIZE_DEFAULT:
			last_field = f;
			last_char = initialize_uniform_char(f, p);

			if(last_char != -1) {
				if(f->flag_occurs) {
					CB_REFERENCE(c)->subs =
					CB_BUILD_CHAIN(cb_int1,
								   CB_REFERENCE(c)->subs);
				}

				for(; f->sister; f = f->sister) {
					if(!f->sister->redefines) {
						if(initialize_type(p, f->sister, 0) != INITIALIZE_DEFAULT ||
								initialize_uniform_char(f->sister, p) != last_char) {
							break;
						}
					}
				}
				size_t size;
				if(f->sister) {
					size = f->sister->offset - last_field->offset;
				} else {
					size = ff->offset + ff->size - last_field->offset;
				}

				str += output_initialize_uniform(c, last_char, (int) size);
				break;
			}
		/* Fall through */
		default:
			if(f->flag_occurs) {
				/* Begin occurs loop */
				int i = f->indexes;
				i_counters[i] = 1;
#ifdef _MSC_VER
				str += output_indent("{");
#endif
				str += output_line("for(_i%d = 1; _i%d <= %d; _i%d++)",
								   i, i, f->occurs_max, i);
				str += output_indent("{");
				CB_REFERENCE(c)->subs =
				CB_BUILD_CHAIN(cb_i[i], CB_REFERENCE(c)->subs);
			}

			if(type == INITIALIZE_ONE) {
				str += output_initialize_one(p, c);
			} else {
				str += output_initialize_compound(p, c);
			}

			if(f->flag_occurs) {
				/* Close loop */
				CB_REFERENCE(c)->subs = CB_CHAIN(CB_REFERENCE(c)->subs);
				str += output_indent("}");
#ifdef _MSC_VER
				str += output_indent("}");
#endif
			}
		}
	}
	return str;
}

static string
output_initialize(cb_initialize * p)
{
	string str("");
	cb_tree x;
	int c;
	cb_field * f = cb_code_field(p->var);
	int type = initialize_type(p, f, 1);
	/* Check for non-standard OCCURS */
	if((f->level == 1 || f->level == 77) &&
			f->flag_occurs && !p->flag_init_statement) {
		switch(type) {
		case INITIALIZE_NONE:
			return "";
		case INITIALIZE_ONE:
			str = output_initialize_one(p, p->var);
			str += output_initialize_chaining(f, p);
			return str;
		case INITIALIZE_DEFAULT:
			c = initialize_uniform_char(f, p);
			if(c != -1) {
				str = output_initialize_uniform(p->var, c, f->occurs_max);
				str += output_initialize_chaining(f, p);
				return str;
			}
		/* Fall through */
		case INITIALIZE_COMPOUND:
			i_counters[0] = 1;
#ifdef _MSC_VER
			str += output_indent("{");
#endif
			str += output_line("for(_i0 = 1; _i0 <= %d; _i0++)", f->occurs_max);
			str += output_indent("{");
			x = cb_build_field_reference(f, NULL);
			CB_REFERENCE(x)->subs =
			CB_BUILD_CHAIN(cb_i[0], CB_REFERENCE(x)->subs);
			str += output_initialize_compound(p, x);
			CB_REFERENCE(x)->subs =
			CB_CHAIN(CB_REFERENCE(x)->subs);
			str += output_indent("}");
			str += output_initialize_chaining(f, p);
#ifdef _MSC_VER
			str += output_indent("}");
#endif
			return str;
		default:
			break;
		}
	}
	switch(type) {
	case INITIALIZE_NONE:
		return str;
	case INITIALIZE_ONE:
		str += output_initialize_one(p, p->var);
		str += output_initialize_chaining(f, p);
		return str;
	case INITIALIZE_DEFAULT:
		c = initialize_uniform_char(f, p);
		if(c != -1) {
			str += output_initialize_uniform(p->var, c, f->size);
			str += output_initialize_chaining(f, p);
			return str;
		}
	/* Fall through */
	case INITIALIZE_COMPOUND:
		str += output_initialize_compound(p, p->var);
		str += output_initialize_chaining(f, p);
		return str;
	default:
		break;
	}
	return str;
}

/* SEARCH */

static string
output_occurs(cb_field * p)
{
	if(p->depending) {
		return output_integer(p->depending);
	}
	return output("%d", p->occurs_max);
}

static string
output_search_whens(cb_tree table, cb_field * p, cb_tree stmt, cb_tree var, cb_tree whens)
{
	string str("");
	cb_tree idx = NULL;

	if(!p->index_list) {
		/* LCOV_EXCL_START */
		cobc_err_msg(_("call to '%s' with invalid parameter '%s'"),
					 "output_search", "table");
		COBC_ABORT();
		/* LCOV_EXCL_STOP */
	}

	/* Determine the index to use */
	if(var) {
		for(cb_tree l = p->index_list; l; l = CB_CHAIN(l)) {
			if(cb_ref(CB_VALUE(l)) == cb_ref(var)) {
				idx = var;
			}
		}
	}
	if(!idx) {
		idx = CB_VALUE(p->index_list);
	}

	/* Start loop */
#ifdef _MSC_VER
	str += output_line("{");
#endif
	str += output_line("for(;;) {");
	output_indent_level += 2;

	/* End test */
	str += output_prefix();
	str += output("if(");
	str += output_integer(idx);
	str += output(" > ");
	str += output_occurs(p);
	str += output(")\n");
	str += output_indent("{");
	str += output_line("/* Table end */");
	if(stmt) {
		str += output_stmt(stmt);
	} else {
		str += output_line("break;");
	}
	str += output_indent("}");

	/* WHEN test */
	str += output_stmt(whens);

	/* Iteration */
	str += output_newline();
	str += output_prefix();
	str += output_integer(idx);
	str += output("++;\n");
	if(var && var != idx) {
		str += output_move(idx, var);
	}
	str += output_line("/* Iterate */");
	/* End loop */
	output_indent_level -= 2;
	str += output_line("}");
#ifdef _MSC_VER
	str += output_line("}");
#endif
	return str;
}

static string
output_search_all(cb_tree table, cb_field * p, cb_tree stmt, cb_tree cond, cb_tree when)
{
	cb_tree idx = CB_VALUE(p->index_list);
	/* Header */
	string str = output_indent("{");
	str += output_line("int ret, head, tail;");
	str += output_line("head = 0;");
	str += output_prefix();
	str += output("tail = ");
	str += output_occurs(p);
	str += output(" + 1;\n");

	/* Check for at least one entry */
	str += output_prefix();
	str += output("if (");
	str += output_occurs(p);
	str += output(" == 0) head = tail;\n");

	/* Start loop */
#ifdef _MSC_VER
	str += output_indent("{");
#endif
	str += output_line("for(;;)");
	str += output_indent("{");

	/* End test */
	str += output_line("if(head >= tail - 1)");
	str += output_indent("{");
	str += output_line("/* Table end */");
	if(stmt) {
		str += output_stmt(stmt);
	} else {
		str += output_line("break;");
	}
	str += output_indent("}");

	/* Next index */
	str += output_prefix();
	str += output_integer(idx);
	str += output(" = (head + tail) / 2;\n");
	str += output_newline();

	/* WHEN test */
	str += output_line("/* WHEN */");
	str += output_prefix();
	str += output("if(");
	str += output_cond(cond, 1);
	str += output(")\n");
	str += output_indent("{");
	str += output_stmt(when);
	str += output_indent("}");

	str += output_line("if(ret < 0)");
	str += output_prefix();
	str += output("  head = ");
	str += output_integer(idx);
	str += output(";\n");
	str += output_line("else");
	str += output_prefix();
	str += output("  tail = ");
	str += output_integer(idx);
	str += output(";\n");
	str += output_indent("}");
	str += output_indent("}");
#ifdef _MSC_VER
	str += output_indent("}");
#endif
	return str;
}

static string
output_search(cb_search * p)
{
	struct cb_field * fp = cb_code_field(p->table);

	/* TODO: Add run-time checks for the table, including ODO */

	if(p->flag_all) {
		return output_search_all(p->table, fp, p->end_stmt,
								 CB_IF(p->whens)->test, CB_IF(p->whens)->stmt1);
	}
	return output_search_whens(p->table, fp, p->end_stmt, p->var, p->whens);
}

/* CALL */

/**
 * cast function pointer call frame to avoid default argument promotion
 */
static string
output_call_protocast(cb_tree x, cb_tree l)
{
	switch(CB_TREE_TAG(x)) {
	case CB_TAG_CAST:
		return output("int");
	case CB_TAG_INTRINSIC:
		if(CB_INTRINSIC(x)->intr_tab->category == CB_CATEGORY_NUMERIC) {
			return output("int");
		}
		return output("void *");
	case CB_TAG_LITERAL: {
		if(CB_TREE_CLASS(x) != CB_CLASS_NUMERIC) {
			return output("int");
		}
		if(CB_SIZES_INT_UNSIGNED(l)) {
			cob_u64_t uval = cb_get_u_long_long(x);
			switch(CB_SIZES_INT(l)) {
			case CB_SIZE_AUTO:
				if(uval > UINT_MAX) {
					return output("cob_u64_t");
				}
			/* Fall through to case 4 */
			case CB_SIZE_4:
				return output("cob_u32_t");
			case CB_SIZE_1:
				return output("cob_u8_t");
			case CB_SIZE_2:
				return output("cob_u16_t");
			case CB_SIZE_8:
				return output("cob_u64_t");
			default:
				/* LCOV_EXCL_START */
				cobc_err_msg(_("unexpected size: %d"), CB_SIZES_INT(l));
				COBC_ABORT();
				/* LCOV_EXCL_STOP */
			}
		}
		cob_s64_t val = cb_get_long_long(x);
		switch(CB_SIZES_INT(l)) {
		case CB_SIZE_AUTO:
			if(val > INT_MAX) {
				return output("cob_s64_t");
			}
		/* Fall through to case 4 */
		case CB_SIZE_4:
			return output("cob_s32_t");
		case CB_SIZE_1:
			return output("cob_s8_t");
		case CB_SIZE_2:
			return output("cob_s16_t");
		case CB_SIZE_8:
			return output("cob_s64_t");
		default:
			/* LCOV_EXCL_START */
			cobc_err_msg(_("unexpected size: %d"), CB_SIZES_INT(l));
			COBC_ABORT();
			/* LCOV_EXCL_STOP */
		}
		return "";
	}
	default:
		cb_field * f = cb_code_field(x);
		switch(f->usage) {
		case CB_USAGE_BINARY:
		case CB_USAGE_COMP_5:
		case CB_USAGE_COMP_X:
		case CB_USAGE_PACKED:
		case CB_USAGE_DISPLAY:
		case CB_USAGE_COMP_6: {
			int sizes = CB_SIZES_INT(l);
			int sign = 0;
			if(sizes == CB_SIZE_AUTO) {
				if(f->pic->have_sign) {
					sign = 1;
				}
				if(f->usage == CB_USAGE_PACKED ||
						f->usage == CB_USAGE_DISPLAY ||
						f->usage == CB_USAGE_COMP_6) {
					sizes = f->pic->digits - f->pic->scale;
				} else {
					sizes = f->size;
				}
				switch(sizes) {
				case 0:
					sizes = CB_SIZE_4;
					break;
				case 1:
					sizes = CB_SIZE_1;
					break;
				case 2:
					sizes = CB_SIZE_2;
					break;
				case 3:
					sizes = CB_SIZE_4;
					break;
				case 4:
					sizes = CB_SIZE_4;
					break;
				case 5:
					sizes = CB_SIZE_8;
					break;
				case 6:
					sizes = CB_SIZE_8;
					break;
				case 7:
					sizes = CB_SIZE_8;
					break;
				default:
					sizes = CB_SIZE_8;
					break;
				}
			} else {
				if(!CB_SIZES_INT_UNSIGNED(l)) {
					sign = 1;
				}
			}
			const char * s;
			switch(sizes) {
			case CB_SIZE_1:
				if(sign) {
					s = "cob_c8_t";
				} else {
					s = "cob_u8_t";
				}
				break;
			case CB_SIZE_2:
				if(sign) {
					s = "cob_s16_t";
				} else {
					s = "cob_u16_t";
				}
				break;
			case CB_SIZE_4:
				if(sign) {
					s = "cob_s32_t";
				} else {
					s = "cob_u32_t";
				}
				break;
			case CB_SIZE_8:
				if(sign) {
					s = "cob_s64_t";
				} else {
					s = "cob_u64_t";
				}
				break;
			default:
				if(sign) {
					s = "cob_s32_t";
				} else {
					s = "cob_u32_t";
				}
				break;
			}
			return s;
		}
		case CB_USAGE_INDEX:
			return output("cob_s32_t");
		case CB_USAGE_LENGTH:
			return output("cob_u32_t");
		case CB_USAGE_POINTER:
		case CB_USAGE_PROGRAM_POINTER:
			return output("void *");
		case CB_USAGE_FLOAT:
			return output("float");
		case CB_USAGE_DOUBLE:
			return output("double");
		case CB_USAGE_LONG_DOUBLE:
			return output("long double");
		case CB_USAGE_FP_BIN32:
			return output("cob_u32_t");
		case CB_USAGE_FP_BIN64:
		case CB_USAGE_FP_DEC64:
			return output("cob_u64_t");
		case CB_USAGE_FP_BIN128:
		case CB_USAGE_FP_DEC128:
			return output("cob_fp_128");
		default:
			return output("void *");
		}
	}
}

/**
 * dereference BY VALUE arguments, sync with call_output_protocast
 */
static string
output_call_by_value_args(cb_tree x, cb_tree l)
{
	string str("");
	cb_field * f;
	cob_s64_t val;
	cob_u64_t uval;

	switch(CB_TREE_TAG(x)) {
	case CB_TAG_CAST:
		return output_integer(x);
	case CB_TAG_INTRINSIC:
		if(CB_INTRINSIC(x)->intr_tab->category == CB_CATEGORY_NUMERIC) {
			str += output("cob_get_int(");
			str += output_param(x, -1);
			str += output(")");
		} else {
			str += output_data(x);
		}
		return str;
	case CB_TAG_LITERAL:
		if(CB_TREE_CLASS(x) != CB_CLASS_NUMERIC) {
			str += output("%d", CB_LITERAL(x)->data[0]);
			return str;
		}
		if(CB_SIZES_INT_UNSIGNED(l)) {
			uval = cb_get_u_long_long(x);
			switch(CB_SIZES_INT(l)) {
			case CB_SIZE_AUTO:
				if(uval > UINT_MAX) {
					str += output("(cob_u64_t)");
					str += output(CB_FMT_LLU_F, uval);
					return str;
				}
			/* Fall through to case 4 */
			case CB_SIZE_4:
				str += output("(cob_u32_t)");
				str += output(CB_FMT_LLU_F, uval);
				return str;
			case CB_SIZE_1:
				str += output("(cob_u8_t)");
				str += output(CB_FMT_LLU_F, uval);
				return str;
			case CB_SIZE_2:
				str += output("(cob_u16_t)");
				str += output(CB_FMT_LLU_F, uval);
				return str;
			case CB_SIZE_8:
				str += output("(cob_u64_t)");
				str += output(CB_FMT_LLU_F, uval);
				return str;
			default:
				/* LCOV_EXCL_START */
				cobc_err_msg(_("unexpected size: %d"), CB_SIZES_INT(l));
				COBC_ABORT();
				/* LCOV_EXCL_STOP */
			}
		}
		val = cb_get_long_long(x);
		switch(CB_SIZES_INT(l)) {
		case CB_SIZE_AUTO:
			if(val > INT_MAX) {
				str += output("(cob_s64_t)");
				str += output(CB_FMT_LLD_F, val);
				return str;
			}
		/* Fall through to case 4 */
		case CB_SIZE_4:
			str += output("(cob_s32_t)");
			str += output(CB_FMT_LLD_F, val);
			return str;
		case CB_SIZE_1:
			str += output("(cob_s8_t)");
			str += output(CB_FMT_LLD_F, val);
			return str;
		case CB_SIZE_2:
			str += output("(cob_s16_t)");
			str += output(CB_FMT_LLD_F, val);
			return str;
		case CB_SIZE_8:
			str += output("(cob_s64_t)");
			str += output(CB_FMT_LLD_F, val);
			return str;
		default:
			/* LCOV_EXCL_START */
			cobc_err_msg(_("unexpected size: %d"), CB_SIZES_INT(l));
			COBC_ABORT();
			/* LCOV_EXCL_STOP */
		}
		return str;
	default:
		int sign;
		int sizes;
		f = cb_code_field(x);
		switch(f->usage) {
		case CB_USAGE_PACKED:
		case CB_USAGE_DISPLAY:
		case CB_USAGE_COMP_6:
			str += output("*(cob_u64_t *)(");
			str += output_data(x);
			str += output(")");
			return str;
		case CB_USAGE_BINARY:
		case CB_USAGE_COMP_5:
		case CB_USAGE_COMP_X:
			sign = 0;
			sizes = CB_SIZES_INT(l);
			if(sizes == CB_SIZE_AUTO) {
				if(f->pic->have_sign) {
					sign = 1;
				}
				if(f->size > 4) {
					sizes = CB_SIZE_8;
				} else if(f->size > 2) {
					sizes = CB_SIZE_4;
				} else if(f->size > 1) {
					sizes = CB_SIZE_2;
				} else {
					sizes = CB_SIZE_1;
				}
			} else {
				if(!CB_SIZES_INT_UNSIGNED(l)) {
					sign = 1;
				}
			}
			const char * s;
			switch(sizes) {
			case CB_SIZE_1:
				if(sign) {
					s = "cob_c8_t";
				} else {
					s = "cob_u8_t";
				}
				break;
			case CB_SIZE_2:
				if(sign) {
					s = "cob_s16_t";
				} else {
					s = "cob_u16_t";
				}
				break;
			case CB_SIZE_4:
				if(sign) {
					s = "cob_s32_t";
				} else {
					s = "cob_u32_t";
				}
				break;
			case CB_SIZE_8:
				if(sign) {
					s = "cob_s64_t";
				} else {
					s = "cob_u64_t";
				}
				break;
			default:
				if(sign) {
					s = "cob_s32_t";
				} else {
					s = "cob_u32_t";
				}
				break;
			}
			str += output("(%s)(", s);
			str += output_integer(x);
			str += output(")");
			return str;
		case CB_USAGE_INDEX:
		case CB_USAGE_HNDL:
		case CB_USAGE_HNDL_WINDOW:
		case CB_USAGE_HNDL_SUBWINDOW:
		case CB_USAGE_HNDL_FONT:
		case CB_USAGE_HNDL_THREAD:
		case CB_USAGE_HNDL_MENU:
		case CB_USAGE_HNDL_VARIANT:
		case CB_USAGE_HNDL_LM:
		case CB_USAGE_LENGTH:
		case CB_USAGE_POINTER:
		case CB_USAGE_PROGRAM_POINTER:
			str += output_integer(x);
			return str;
		case CB_USAGE_FLOAT:
			str += output("(double) *(float *)(");
			str += output_data(x);
			str += output(")");
			return str;
		case CB_USAGE_DOUBLE:
			str += output("*(double *)(");
			str += output_data(x);
			str += output(")");
			return str;
		case CB_USAGE_LONG_DOUBLE:
			str += output("*(long double *)(");
			str += output_data(x);
			str += output(")");
			return str;
		case CB_USAGE_FP_BIN32:
			str += output("*(cob_u32_t *)(");
			str += output_data(x);
			str += output(")");
			return str;
		case CB_USAGE_FP_BIN64:
		case CB_USAGE_FP_DEC64:
			str += output("*(cob_u64_t *)(");
			str += output_data(x);
			str += output(")");
			return str;
		case CB_USAGE_FP_BIN128:
		case CB_USAGE_FP_DEC128:
			str += output("*(cob_fp_128 *)(");
			str += output_data(x);
			str += output(")");
			return str;
		default:
			str += output("*(");
			str += output_data(x);
			str += output(")");
			break;
		}
	}
	return str;
}

static string
output_bin_field(const cb_tree x, const cob_u32_t id)
{
	if(!CB_NUMERIC_LITERAL_P(x)) {
		return "";
	}
	cob_u32_t aflags = 0;
	cob_u32_t size;
	cob_u32_t digits;
	if(cb_fit_to_int(x)) {
		size = 4;
		digits = 9;
		aflags = COB_FLAG_HAVE_SIGN;
#ifndef WORDS_BIGENDIAN
		if(cb_binary_byteorder == CB_BYTEORDER_BIG_ENDIAN) {
			aflags |= COB_FLAG_BINARY_SWAP;
		}
#endif
	} else {
		size = 8;
		digits = 18;
		if(CB_LITERAL(x)->sign < 0) {
			aflags |= COB_FLAG_HAVE_SIGN;
		}
	}
	aflags |= COB_FLAG_CONSTANT | COB_FLAG_REAL_BINARY;
	int i = lookup_attr(COB_TYPE_NUMERIC_BINARY, digits, 0, aflags, NULL, 0);
	return output_line("cob_field\tcontent_fb_%u(%u, content_%u.data, &%s%d);",
					   id, size, id, CB_PREFIX_ATTR, i);
}

static COB_INLINE COB_A_INLINE int
is_literal_or_prototype_ref(cb_tree x)
{
	return CB_LITERAL_P(x)
		   || (CB_REFERENCE_P(x) && CB_PROTOTYPE_P(cb_ref(x)));
}

static char *
get_program_id_str(cb_tree id_item)
{
	if(CB_LITERAL_P(id_item)) {
		return (char *) CB_LITERAL(id_item)->data;
	}
	/* prototype */
	return (char *) CB_PROTOTYPE(cb_ref(id_item))->ext_name;
}

static struct nested_list *
find_nested_prog_with_id(const char * encoded_id)
{
	struct nested_list	* nlp;

	for(nlp = current_prog->nested_prog_list; nlp; nlp = nlp->next) {
		if(!strcmp(encoded_id, nlp->nested_prog->program_id)) {
			break;
		}
	}

	return nlp;
}

static string
output_call(cb_call * p)
{
	string str("");
	const system_table * psyst = NULL;
	const int name_is_literal_or_prototype
		= is_literal_or_prototype_ref(p->name);

	size_t retptr = 0;
	size_t gen_exit_program = 0;
	size_t dynamic_link = 1;
	int except_id = 0;
	if(p->call_returning && p->call_returning != cb_null &&
			CB_TREE_CLASS(p->call_returning) == CB_CLASS_POINTER) {
		retptr = 1;
	}

#ifdef	_WIN32
	const char * convention;
	if(p->convention & CB_CONV_STDCALL) {
		convention = "_std";
	} else {
		convention = "";
	}
#else
	const char * convention = "";
#endif

	/* System routine entry points */
	if(p->is_system) {
		psyst = &system_tab[p->is_system - 1];
		dynamic_link = 0;
	}

	if(dynamic_link && name_is_literal_or_prototype) {
		if(cb_flag_static_call || (p->convention & CB_CONV_STATIC_LINK)) {
			dynamic_link = 0;
		}

		const char * name_str;
		if(CB_LITERAL_P(p->name)) {
			name_str = (const char *) CB_LITERAL(p->name)->data;
		} else { /* prototype */
			name_str = CB_PROTOTYPE(cb_ref(p->name))->ext_name;
		}

		for(cb_text_list * ctl = cb_static_call_list; ctl; ctl = ctl->next) {
			if(!strcmp(name_str, ctl->text)) {
				dynamic_link = 0;
				break;
			}
		}
		for(cb_text_list * ctl = cb_early_exit_list; ctl; ctl = ctl->next) {
			if(!strcmp(name_str, ctl->text)) {
				gen_exit_program = 1;
				break;
			}
		}
	}
	size_t need_brace = 0;

#ifdef	COB_NON_ALIGNED
	if(dynamic_link && retptr) {
		if(!need_brace) {
			need_brace = 1;
			output_indent("{");
		}
		str += output_line("void *temptr;");
	}
#endif

	if(CB_REFERENCE_P(p->name) &&
			CB_FIELD_P(CB_REFERENCE(p->name)->value) &&
			CB_FIELD(CB_REFERENCE(p->name)->value)->usage == CB_USAGE_PROGRAM_POINTER) {
		dynamic_link = 0;
	}

	/*
	 * Check for LENGTH OF as BY REFERENCE and change to BY CONTENT
	 * as LENGTH OF is effectively a numeric literal
	 */
	cob_u32_t n = 1;
	for(cb_tree l = p->args; l; l = CB_CHAIN(l), n++) {
		cb_tree x = CB_VALUE(l);
		if(CB_PURPOSE_INT(l) == CB_CALL_BY_REFERENCE
				&& CB_REF_OR_FIELD_P(x)
				&& CB_TREE_CATEGORY(x) == CB_CATEGORY_NUMERIC
				&& cb_code_field(x)->usage == CB_USAGE_LENGTH) {
			CB_INTEGER(CB_PURPOSE(l))->val = CB_CALL_BY_CONTENT;
		}
	}

	/* Set up arguments */
	n = 1;
	for(cb_tree l = p->args; l; l = CB_CHAIN(l), n++) {
		cb_tree x = CB_VALUE(l);
		switch(CB_PURPOSE_INT(l)) {
		case CB_CALL_BY_REFERENCE:
			if(CB_NUMERIC_LITERAL_P(x) || CB_BINARY_OP_P(x)) {
				if(!need_brace) {
					need_brace = 1;
					str += output_indent("{");
				}
				str += output_line("cob_content\tcontent_%u;", n);
				str += output_bin_field(x, n);
			} else if(CB_CAST_P(x)) {
				if(!need_brace) {
					need_brace = 1;
					str += output_indent("{");
				}
				str += output_line("void *ptr_%u;", n);
			}
			break;
		case CB_CALL_BY_CONTENT:
			if(CB_CAST_P(x)) {
				if(!need_brace) {
					need_brace = 1;
					str += output_indent("{");
				}
				str += output_line("void *ptr_%u;", n);
			} else if(CB_TREE_TAG(x) != CB_TAG_INTRINSIC &&
					  x != cb_null && !(CB_CAST_P(x))) {
				if(!need_brace) {
					need_brace = 1;
					str += output_indent("{");
				}
				str += output_line("union {");
				str += output_prefix();
				str += output("\tunsigned char data[");
				if(CB_NUMERIC_LITERAL_P(x) ||
						CB_BINARY_OP_P(x) || CB_CAST_P(x)) {
					str += output("8");
				} else {
					if(CB_REF_OR_FIELD_P(x)) {
						str += output("%u", (cob_u32_t)cb_code_field(x)->size);
					} else {
						str += output_size(x);
					}
				}
				str += output("];\n");
				str += output_line("\tcob_s64_t     datall;");
				str += output_line("\tcob_u64_t     dataull;");
				str += output_line("\tint           dataint;");
				str += output_line("} content_%u;", n);
#if defined(__xlC__) || defined(__SUNPRO_CC)
				str += output_line("char content_%s%u[sizeof(cob_field)];", CB_PREFIX_FIELD, n);
				str += output_line("char content_%s%u[sizeof(cob_field_attr)];", CB_PREFIX_ATTR, n);
#else
				str += output_line("cob_field content_%s%u(0,0,0);", CB_PREFIX_FIELD, n);
				str += output_line("cob_field_attr content_%s%u(0,0,0,0,0);", CB_PREFIX_ATTR, n);
#endif
				str += output_bin_field(x, n);
			}
			break;
		case CB_CALL_BY_VALUE:
			if(CB_TREE_TAG(x) == CB_TAG_REFERENCE
					&& CB_TREE_TAG(CB_REFERENCE(x)->value) == CB_TAG_FIELD
					&& CB_TREE_CATEGORY(x) == CB_CATEGORY_NUMERIC
					&& cb_code_field(x)->usage == CB_USAGE_LENGTH) {
				if(!need_brace) {
					need_brace = 1;
					str += output_indent("{");
				}
				str += output_line("union {");
				str += output_prefix();
				str += output("\tunsigned char data[");
				if(CB_NUMERIC_LITERAL_P(x)
						|| CB_BINARY_OP_P(x)
						|| CB_CAST_P(x)) {
					str += output("8");
				} else {
					if(CB_REF_OR_FIELD_P(x)) {
						str += output("%u", (cob_u32_t)cb_code_field(x)->size);
					} else {
						str += output_size(x);
					}
				}
				str += output("];\n");
				str += output_line("\tcob_s64_t     datall;");
				str += output_line("\tcob_u64_t     dataull;");
				str += output_line("\tint           dataint;");
				str += output_line("} content_%u;", n);
#if defined(__xlC__) || defined(__SUNPRO_CC)
				str += output_line("char content_%s%u[sizeof(cob_field)];", CB_PREFIX_FIELD, n);
				str += output_line("char content_%s%u[sizeof(cob_field_attr)];", CB_PREFIX_ATTR, n);
#else
				str += output_line("cob_field content_%s%u(0,0,0);", CB_PREFIX_FIELD, n);
				str += output_line("cob_field_attr content_%s%u(0,0,0,0,0);", CB_PREFIX_ATTR, n);
#endif
				str += output_bin_field(x, n);
			}
			break;
		default:
			break;
		}
	}

	if(need_brace) {
		str += output_newline();
	}
	n = 1;
	for(cb_tree l = p->args; l; l = CB_CHAIN(l), n++) {
		cb_tree x = CB_VALUE(l);
		switch(CB_PURPOSE_INT(l)) {
		case CB_CALL_BY_REFERENCE:
			if(CB_NUMERIC_LITERAL_P(x)) {
				str += output_prefix();
				if(cb_fit_to_int(x)) {
					//str += output("content_%u.dataint = ", n);
					//str += output("%d", cb_get_int(x));
					unsigned int pval = (unsigned int) cb_get_int(x);
					str += output("cob_set_int(&content_fb_%d, %d)", n, pval);
				} else {
					if(CB_LITERAL(x)->sign >= 0) {
						str += output("content_%u.dataull = ", n);
						str += output(CB_FMT_LLU_F,
									  cb_get_u_long_long(x));
					} else {
						str += output("content_%u.datall = ", n);
						str += output(CB_FMT_LLD_F,
									  cb_get_long_long(x));
					}
				}
				str += output(";\n");
			} else if(CB_BINARY_OP_P(x)) {
				str += output_prefix();
				str += output("content_%u.dataint = ", n);
				str += output_integer(x);
				str += output(";\n");
			} else if(CB_CAST_P(x)) {
				str += output_prefix();
				str += output("ptr_%u = ", n);
				str += output_integer(x);
				str += output(";\n");
			}
			break;
		case CB_CALL_BY_CONTENT:
			if(CB_CAST_P(x)) {
				str += output_prefix();
				str += output("ptr_%u = ", n);
				str += output_integer(x);
				str += output(";\n");
			} else if(CB_TREE_TAG(x) != CB_TAG_INTRINSIC) {
				if(CB_NUMERIC_LITERAL_P(x)) {
					str += output_prefix();
					if(cb_fit_to_int(x)) {
						//str += output("content_%u.dataint = ", n);
						//str += output("%d", cb_get_int(x));
						unsigned int pval = (unsigned int) cb_get_int(x);
						str += output("cob_set_int(&content_fb_%d, %d)", n, pval);
					} else if(CB_LITERAL(x)->sign >= 0) {
						str += output("content_%u.dataull = ", n);
						str += output(CB_FMT_LLU_F, cb_get_u_long_long(x));
					} else {
						str += output("content_%u.datall = ", n);
						str += output(CB_FMT_LLD_F, cb_get_long_long(x));
					}
					str += output(";\n");
				} else if(CB_REF_OR_FIELD_P(x) &&
						  CB_TREE_CATEGORY(x) == CB_CATEGORY_NUMERIC &&
						  cb_code_field(x)->usage == CB_USAGE_LENGTH) {
					str += output_prefix();
					//str += output("content_%u.dataint = ", n);
					//str += output_integer(x);
					//str += output(";\n");
					str += output("cob_field_constant (");
					str += output_param(x, -1);
#if defined(__xlC__) || defined(__SUNPRO_CC)
					str += output(", (cob_field *)content_%s%u ", CB_PREFIX_FIELD, n);
					str += output(", (cob_field_attr *)content_%s%u ", CB_PREFIX_ATTR, n);
#else
					str += output(", &content_%s%u ", CB_PREFIX_FIELD, n);
					str += output(", &content_%s%u ", CB_PREFIX_ATTR, n);
#endif
					str += output(", &content_%u);\n", n);
#ifndef WORDS_BIGENDIAN
					if(cb_binary_byteorder == CB_BYTEORDER_BIG_ENDIAN) {
						str += output_prefix();
#if defined(__xlC__) || defined(__SUNPRO_CC)
						str += output("((cob_field_attr *)content_%s%u)->flags |= COB_FLAG_BINARY_SWAP;\n",
									  CB_PREFIX_ATTR, n);
#else
						str += output("content_%s%u.flags |= COB_FLAG_BINARY_SWAP;\n",
									  CB_PREFIX_ATTR, n);
#endif
						str += output_prefix();
						str += output("content_%u.dataint = ", n);
						str += output("(unsigned int)COB_BSWAP_32(");
						str += output_integer(x);
						str += output(");\n");
					}
#endif
				} else if(x != cb_null && !(CB_CAST_P(x))) {
					/*
					 * Create copy of cob_field&attr pointing to local copy of data
					 * and setting flag COB_FLAG_CONSTANT
					 */
					str += output_prefix();
					str += output("cob_field_constant(");
					str += output_param(x, -1);
#if defined(__xlC__) || defined(__SUNPRO_CC)
					str += output(", (cob_field *)content_%s%u ", CB_PREFIX_FIELD, n);
					str += output(", (cob_field_attr *)content_%s%u ", CB_PREFIX_ATTR, n);
#else
					str += output(", &content_%s%u ", CB_PREFIX_FIELD, n);
					str += output(", &content_%s%u ", CB_PREFIX_ATTR, n);
#endif
					str += output(", &content_%u);\n", n);
				}
			}
			break;
		case CB_CALL_BY_VALUE:
			if(CB_TREE_TAG(x) == CB_TAG_REFERENCE
					&& CB_TREE_TAG(CB_REFERENCE(x)->value) == CB_TAG_FIELD
					&& CB_TREE_CATEGORY(x) == CB_CATEGORY_NUMERIC
					&& cb_code_field(x)->usage == CB_USAGE_LENGTH) {
				str += output_prefix();
				str += output("cob_field_constant (");
				str += output_param(x, -1);
#if defined(__xlC__) || defined(__SUNPRO_CC)
				str += output(", (cob_field *)content_%s%u ", CB_PREFIX_FIELD, n);
				str += output(", (cob_field_attr *)content_%s%u ", CB_PREFIX_ATTR, n);
#else
				str += output(", &content_%s%u ", CB_PREFIX_FIELD, n);
				str += output(", &content_%s%u ", CB_PREFIX_ATTR, n);
#endif
				str += output(", &content_%u);\n", n);
			}
			break;
		default:
			break;
		}
	}

	/* Set up parameter types */
	n = 0;
	for(cb_tree l = p->args; l; l = CB_CHAIN(l), n++) {
		cb_tree x = CB_VALUE(l);
		field_iteration = n;
		str += output_prefix();
		str += output("cob_procedure_params[%u] = ", n);
		switch(CB_TREE_TAG(x)) {
		case CB_TAG_LITERAL:
			if(CB_NUMERIC_LITERAL_P(x) &&
					CB_PURPOSE_INT(l) != CB_CALL_BY_VALUE) {
				str += output("&content_fb_%u", n + 1);
				break;
			}
		/* Fall through */
		case CB_TAG_FIELD:
			if(CB_PURPOSE_INT(l) == CB_CALL_BY_CONTENT) {
#if defined(__xlC__) || defined(__SUNPRO_CC)
				str += output("(cob_field *)content_%s%u", CB_PREFIX_FIELD, n + 1);
#else
				str += output("&content_%s%u", CB_PREFIX_FIELD, n + 1);
#endif
				break;
			}
		/* Fall through */
		case CB_TAG_INTRINSIC:
			str += output_param(x, -1);
			break;
		case CB_TAG_REFERENCE:
			switch(CB_TREE_TAG(CB_REFERENCE(x)->value)) {
			case CB_TAG_LITERAL:
			case CB_TAG_INTRINSIC:
				if(CB_PURPOSE_INT(l) == CB_CALL_BY_CONTENT) {
#if defined(__xlC__) || defined(__SUNPRO_CC)
					str += output("(cob_field *)content_%s%u", CB_PREFIX_FIELD, n + 1);
#else
					str += output("&content_%s%u", CB_PREFIX_FIELD, n + 1);
#endif
					break;
				}
				str += output_param(x, -1);
				break;
			case CB_TAG_FIELD:
				if(CB_PURPOSE_INT(l) == CB_CALL_BY_CONTENT
						|| (CB_TREE_CATEGORY(x) == CB_CATEGORY_NUMERIC
							&& cb_code_field(x)->usage == CB_USAGE_LENGTH)) {
#if defined(__xlC__) || defined(__SUNPRO_CC)
					str += output("(cob_field *)content_%s%u", CB_PREFIX_FIELD, n + 1);
#else
					str += output("&content_%s%u", CB_PREFIX_FIELD, n + 1);
#endif
					break;
				}
				str += output_param(x, -1);
				break;
			default:
				str += output("NULL");
				break;
			}
			break;
		default:
			str += output("NULL");
			break;
		}
		str += output(";\n");
	}

#if	0	/* RXWRXW - Clear params */
	/* Clear extra parameters */
	if(n > MAX_CALL_FIELD_PARAMS - 4) {
		parmnum = MAX_CALL_FIELD_PARAMS - n;
	} else {
		parmnum = 4;
	}
	parmnum *= sizeof(cob_field *);
	str += output_line("memset(&(cob_procedure_params[%u]), 0, %u);",
					   n, parmnum);
#endif

	/* Set number of parameters */
	str += output_prefix();
	str += output("cob_glob_ptr->cob_call_params = %u;\n", n);

	str += output_prefix();
	if(p->stmt1) {
		str += output("cob_glob_ptr->cob_stmt_exception = 1;\n");
	} else {
		str += output("cob_glob_ptr->cob_stmt_exception = 0;\n");
	}

	/* Function name */
	str += output_prefix();
	/* Special for program pointers */
	if(CB_REFERENCE_P(p->name) &&
			CB_FIELD_P(CB_REFERENCE(p->name)->value) &&
			CB_FIELD(CB_REFERENCE(p->name)->value)->usage ==
			CB_USAGE_PROGRAM_POINTER) {
		needs_unifunc = 1;
		str += output("cob_unifunc.funcvoid = ");
		str += output_integer(p->name);
		str += output(";\n");
		str += output_prefix();
		if(p->call_returning == cb_null) {
			str += output("cob_unifunc.funcnull");
		} else if(retptr) {
#ifdef	COB_NON_ALIGNED
			str += output("temptr");
#else
			str += output_integer(p->call_returning);
#endif
			str += output(" = (unsigned char *) cob_unifunc.funcptr");
		} else {
			if(p->convention & CB_CONV_NO_RET_UPD) {
				str += output("(void)cob_unifunc.funcint");
			} else {
				str += output_integer(current_prog->cb_return_code);
				str += output(" = cob_unifunc.funcint");
			}
		}
	} else if(!dynamic_link) {
		/* Static link */
		if(p->call_returning != cb_null) {
			if(retptr) {
#ifdef	COB_NON_ALIGNED
				str += output("temptr");
#else
				str += output_integer(p->call_returning);
#endif
				str += output(" = (unsigned char *)");
			} else if(!(p->convention & CB_CONV_NO_RET_UPD)
					  &&  current_prog->cb_return_code) {
				str += output_integer(current_prog->cb_return_code);
				str += output(" = ");
			} else {
				str += output("(void)");
			}
		}
		if(psyst) {
			str += output("%s", psyst->syst_call);
		} else {
			char * s = get_program_id_str(p->name);
			char * name_str = cb_encode_program_id(s);

			/* Check contained programs */
			nested_list * nlp = find_nested_prog_with_id(name_str);
			if(nlp) {
				str += output("%s_%d__", name_str,
							  nlp->nested_prog->toplev_count);
			} else {
				if(!(p->convention & CB_CONV_OPT_LINK)) {
					const char * declared = declpgms.get(name_str);
					if(declared == 0) {
						if(retptr) {
							output_storage("extern \"C\" void * %s(...);\n", name_str);
						} else if(!(p->convention & CB_CONV_NO_RET_UPD)) {
							output_storage("extern \"C\" int %s(...);\n", name_str);
						} else {
							output_storage("extern \"C\" void %s(...);\n", name_str);
						}
						declpgms.put(name_str, "");
					}
				}
				str += output("%s", name_str);
			}
		}
	} else {
		/* Dynamic link */
		char * callp;
		if(name_is_literal_or_prototype) {
			char * s = get_program_id_str(p->name);
			callp = cb_encode_program_id(s);
			lookup_call(callp);

			str += output("if(unlikely(call_%s.funcvoid == NULL || cob_glob_ptr->cob_physical_cancel)) {\n", callp);
			str += output_prefix();

			nested_list * nlp = find_nested_prog_with_id(callp);
			if(nlp) {
				str += output("  call_%s.funcint = (fintptr) %s_%d__;\n",
							  callp, callp,
							  nlp->nested_prog->toplev_count);
			} else {
				str += output("  call_%s.funcvoid = ", callp);
				str += output("cob_resolve_cobol(");
				str += output_string((const unsigned char *)s,
									 (int)strlen(s), 0);
				str += output(", %d, %d);\n", cb_fold_call, !p->stmt1);
			}
			str += output_prefix();
			str += output("}\n");
		} else {
			callp = NULL;
			needs_unifunc = 1;
			str += output("cob_unifunc.funcvoid = cob_call_field(");
			str += output_param(p->name, -1);
			if(current_prog->nested_prog_list) {
				gen_nested_tab = 1;
				str += output(", cob_nest_tab, %d, %d);\n",
							  !p->stmt1, cb_fold_call);
			} else {
				str += output(", NULL, %d, %d);\n",
							  !p->stmt1, cb_fold_call);
			}
		}
		if(p->stmt1) {
			if(callp) {
				str += output_line("if(unlikely(call_%s.funcvoid == NULL))", callp);
			} else {
				str += output_line("if(unlikely(cob_unifunc.funcvoid == NULL))");
			}
			str += output_line("{");
			output_indent_level += 2;
			except_id = cb_id++;
			str += output_line("%s%d:", CB_PREFIX_LABEL, except_id);
			str += output_line("cob_set_exception(0);");
			str += output_stmt(p->stmt1);
			output_indent_level -= 2;
			str += output_line("}");
			str += output_line("else");
			str += output_indent("{");
		}
		str += output_prefix();
		if(p->call_returning == cb_null) {
			if(callp) {
				str += output("call_%s.funcnull%s", callp, convention);
			} else {
				str += output("cob_unifunc.funcnull%s", convention);
			}
		} else if(retptr) {
#ifdef	COB_NON_ALIGNED
			str += output("temptr");
#else
			str += output_integer(p->call_returning);
#endif
			if(callp) {
				str += output(" = (unsigned char *) call_%s.funcptr%s", callp, convention);
			} else {
				str += output(" = (unsigned char *) cob_unifunc.funcptr%s", convention);
			}
		} else {
			if(p->convention & CB_CONV_NO_RET_UPD || !current_prog->cb_return_code) {
				str += output("(void)");
			} else {
				str += output_integer(current_prog->cb_return_code);
				str += output(" = ");
			}
			if(callp) {
				str += output("call_%s.funcint%s", callp, convention);
			} else {
				str += output("cob_unifunc.funcint%s", convention);
			}
		}
	}

	/* Arguments */
	str += output("(");
	n = 1;
	for(cb_tree l = p->args; l; l = CB_CHAIN(l), n++) {
		cb_tree x = CB_VALUE(l);
		field_iteration = n - 1U;
		switch(CB_PURPOSE_INT(l)) {
		case CB_CALL_BY_REFERENCE:
			if(CB_NUMERIC_LITERAL_P(x) || CB_BINARY_OP_P(x)) {
				str += output("content_%u.data", n);
			} else if(CB_REFERENCE_P(x) && CB_FILE_P(cb_ref(x))) {
				str += output_param(cb_ref(x), -1);
			} else if(CB_CAST_P(x)) {
				str += output("&ptr_%u", n);
			} else {
				str += output_data(x);
			}
			break;
		case CB_CALL_BY_CONTENT:
			if(CB_TREE_TAG(x) != CB_TAG_INTRINSIC && x != cb_null) {
				if(CB_CAST_P(x)) {
					str += output("&ptr_%u", n);
				} else {
					str += output("content_%u.data", n);
				}
			} else {
				str += output_data(x);
			}
			break;
		case CB_CALL_BY_VALUE:
			str += output_call_by_value_args(x, l);
			break;
		default:
			break;
		}
		/* early exit if call parameters don't match, this is actually
		needed for all static calls, but only possible with an
		external program repository, so just do so for system calls
		*/
		if(psyst && psyst->syst_max_params == n) {
			break;
		}
		if(CB_CHAIN(l)) {
			str += output(", ");
		}
	}

	str += output(");\n");

	if(except_id > 0) {
		str += output_line("if (unlikely((cob_glob_ptr->cob_exception_code & 0x%04x) == 0x%04x))",
			CB_EXCEPTION_CODE(COB_EC_PROGRAM), CB_EXCEPTION_CODE(COB_EC_PROGRAM));
		str += output_line("\tgoto %s%d;", CB_PREFIX_LABEL, except_id);
	}

	if(p->call_returning && (!(p->convention & CB_CONV_NO_RET_UPD)) && current_prog->cb_return_code) {
		if(p->call_returning == cb_null) {
			str += output_prefix();
			str += output_integer(current_prog->cb_return_code);
			str += output(" = 0;\n");
		} else if(!retptr) {
			str += output_move(current_prog->cb_return_code,
							   p->call_returning);
#ifdef	COB_NON_ALIGNED
		} else {
			str += output_prefix();
			str += output("memcpy(");
			str += output_data(p->call_returning);
			str += output(", &temptr, %u);\n", (cob_u32_t)sizeof(void *));
#endif
		}
	}
	if(gen_exit_program) {
		needs_exit_prog = 1;
		str += output_line("if(unlikely(module->flag_exit_program)) {");
		str += output_line("\tmodule->flag_exit_program = 0;");
		str += output_line("\tgoto exit_program;");
		str += output_line("}");
	}
	if(p->stmt2) {
		str += output_stmt(p->stmt2);
	}

	if(dynamic_link && p->stmt1) {
		str += output_indent("}");
	}

	if(need_brace) {
		str += output_indent("}");
	}
	return str;
}

/* SET ATTRIBUTE */

static string
output_set_attribute(const struct cb_field * f, cob_flags_t val_on,
					 cob_flags_t val_off)
{
	/* Extension */
	/* Prevent specifying HIGHLIGHT and LOWLIGHT simultaneously. */
	if(val_on & COB_SCREEN_HIGHLIGHT) {
		val_off |= COB_SCREEN_LOWLIGHT;
	} else if(val_on & COB_SCREEN_LOWLIGHT) {
		val_off |= COB_SCREEN_HIGHLIGHT;
	}

	string str("");
	if(val_on) {
		str += output_line("s_%d.attr |= 0x" CB_FMT_LLX ";", f->id, val_on);
	}
	if(val_off) {
		str += output_line("s_%d.attr &= ~0x" CB_FMT_LLX ";", f->id, val_off);
	}
	return str;
}

/* CANCEL */

static string
output_cancel(cb_cancel * p)
{
	string str("");

	if(is_literal_or_prototype_ref(p->target)) {
		char * s = get_program_id_str(p->target);
		char * callp = cb_encode_program_id(s);
		nested_list * nlp = find_nested_prog_with_id(callp);
		if(nlp) {
			str += output_prefix();
			str += output("(void)%s_%d_(-1", callp,
						  nlp->nested_prog->toplev_count);
			for(unsigned int i = 0; i < nlp->nested_prog->num_proc_params; ++i) {
				str += output(", NULL");
			}
			str += output(");\n");
		} else {
			str += output("cob_cancel(");
			str += output_string((const unsigned char *)s,
								 (int)strlen(s), 0);
			str += output(");\n");
		}
		return str;
	}
	str += output_prefix();
	str += output("cob_cancel_field(");
	str += output_param(p->target, -1);
	if(current_prog->nested_prog_list) {
		gen_nested_tab = 1;
		str += output(", cob_nest_tab");
	} else {
		str += output(", NULL");
	}
	str += output(");\n");
	return str;
}

/* PERFORM */

static string
output_perform_call(cb_label * lb, cb_label * le)
{
	string str("");

	if(lb == current_prog->all_procedure || lb->flag_is_debug_sect) {
		str += output_line("/* DEBUGGING Callback PERFORM %s */",
						   (const char *)lb->name);
	} else if(lb == le) {
		str += output_line("/* PERFORM %s */", (const char *)lb->name);
	} else {
		str += output_line("/* PERFORM %s THRU %s */", (const char *)lb->name,
						   (const char *)le->name);
	}

	/* Save current independent segment pointers */
	if(current_prog->flag_segments && last_section &&
			last_section->section_id != lb->section_id) {
		cb_para_label * p = last_section->para_label;
		for(; p; p = p->next) {
			if(p->para->segment > 49 &&
					p->para->flag_alter) {
				str += output_line("save_label_%s%d = label_%s%d;",
								   CB_PREFIX_LABEL, p->para->id,
								   CB_PREFIX_LABEL, p->para->id);
			}
		}
	}
	/* Zap target independent labels */
	if(current_prog->flag_segments && last_segment != lb->segment) {
		cb_para_label * p;
		if(lb->flag_section) {
			p = lb->para_label;
		} else if(lb->section) {
			p = lb->section->para_label;
		} else {
			p = NULL;
		}
		for(; p; p = p->next) {
			if(p->para->segment > 49 &&
					p->para->flag_alter) {
				str += output_line("label_%s%d = 0;",
								   CB_PREFIX_LABEL, p->para->id);
			}
		}
	}

	/* Update debugging name */
	if(current_prog->flag_gen_debug && lb->flag_real_label &&
			(current_prog->all_procedure || lb->flag_debugging_mode)) {
		str += output_stmt(cb_build_debug(cb_debug_name,
										  (const char *)lb->name, NULL));
	}

	str += output_line("_frame_ptr++;");
	if(cb_flag_stack_check) {
		str += output_line("if(unlikely(_frame_ptr == _frame_overflow))");
		str += output_line("    cob_fatal_error(COB_FERROR_STACK);");
	}
	str += output_line("_frame_ptr->perform_through = %d;", le->id);
	if(!cb_flag_computed_goto) {
		label_list * l = (label_list *) cobc_parse_malloc(sizeof(label_list));
		l->next = label_cache;
		l->id = cb_id;
		if(label_cache == NULL) {
			l->call_num = 0;
		} else {
			l->call_num = label_cache->call_num + 1;
		}
		label_cache = l;
		str += output_line("_frame_ptr->return_address_num = %d;", l->call_num);
		str += output_line("goto %s%d;", CB_PREFIX_LABEL, lb->id);
		str += output_line("%s%d:", CB_PREFIX_LABEL, cb_id);
	} else {
		str += output_line("_frame_ptr->return_address_ptr = &&%s%d;",
						   CB_PREFIX_LABEL, cb_id);
		str += output_line("goto %s%d;", CB_PREFIX_LABEL, lb->id);
		str += output_line("%s%d:", CB_PREFIX_LABEL, cb_id);
	}
	str += output_line("_frame_ptr--;");
	cb_id++;

	if(current_prog->flag_segments && last_section &&
			last_section->section_id != lb->section_id) {
		/* Restore current independent segment pointers */
		cb_para_label * p = last_section->para_label;
		for(; p; p = p->next) {
			if(p->para->segment > 49 &&
					p->para->flag_alter) {
				str += output_line("label_%s%d = save_label_%s%d;",
								   CB_PREFIX_LABEL, p->para->id,
								   CB_PREFIX_LABEL, p->para->id);
			}
		}
		/* Zap target independent labels */
		if(lb->flag_section) {
			p = lb->para_label;
		} else if(lb->section) {
			p = lb->section->para_label;
		} else {
			p = NULL;
		}
		for(; p; p = p->next) {
			if(p->para->segment > 49 &&
					p->para->flag_alter) {
				str += output_line("label_%s%d = 0;",
								   CB_PREFIX_LABEL, p->para->id);
			}
		}
	}
	return str;
}

static string
output_perform_exit(cb_label * l)
{
	string str("");
	if(l->flag_global) {
		str += output_newline();
		str += output_line("/* Implicit GLOBAL DECLARATIVE return */");
		str += output_line("if(entry == %d) {", l->id);
		str += output_line("  cob_module_leave(module);");
		if(cb_flag_stack_on_heap || current_prog->flag_recursive) {
			str += output_line("  cob_free(_frame_stack);");
			str += output_line("  cob_free(cob_procedure_params);");
			str += output_line("  cob_module_free(&module);");
		}
		str += output_line("  return 0;");
		str += output_line("}");
	}
	str += output_newline();

	if(l->flag_declarative_exit) {
		str += output_line("/* Implicit DECLARATIVE return */");
	} else if(l->flag_default_handler) {
		str += output_line("/* Implicit Default Error Handler return */");
	} else {
		str += output_line("/* Implicit PERFORM return */");
	}

	if(cb_perform_osvs && current_prog->prog_type == CB_PROGRAM_TYPE) {
		str += output_line("{");
		str += output_line
			   ("  for(cob_frame * _temp_index = _frame_ptr; _temp_index->perform_through; _temp_index--) {");
		str += output_line("    if(_temp_index->perform_through == %d) {", l->id);
		str += output_line("      _frame_ptr = _temp_index;");
		if(!cb_flag_computed_goto) {
			str += output_line("      goto P_switch;");
		} else {
			str += output_line("      goto *_frame_ptr->return_address_ptr;");
		}
		str += output_line("    }");
		str += output_line("  }");
		str += output_line("}");
	} else {
		str += output_line("if(_frame_ptr->perform_through == %d)", l->id);
		if(!cb_flag_computed_goto) {
			str += output_line("  goto P_switch;");
		} else {
			str += output_line("  goto *_frame_ptr->return_address_ptr;");
		}
	}

	if(l->flag_fatal_check) {
		str += output_newline();
		str += output_line("/* Fatal error if reached */");
		str += output_line("cob_fatal_error(COB_FERROR_GLOBAL);");
	}
	return str;
}

static string
output_funcall_debug(cb_tree x)
{
	string str("");

	cb_funcall * p = CB_FUNCALL(x);
	if(p->name[0] == '$') {
		cb_tree z = p->argv[0];
		if(CB_REF_OR_FIELD_P(z) &&
				cb_code_field(z)->flag_field_debug) {
			/* DEBUG */
			str += output_stmt(cb_build_debug(cb_debug_name,
											  (const char *)cb_code_field(z)->name, NULL));
			str += output_stmt(cb_build_debug(cb_debug_contents,
											  NULL, z));
			str += output_perform_call(cb_code_field(z)->debug_section,
									   cb_code_field(z)->debug_section);
		}
		z = p->argv[1];
		if(CB_REF_OR_FIELD_P(z) &&
				cb_code_field(z)->flag_field_debug) {
			/* DEBUG */
			str += output_stmt(cb_build_debug(cb_debug_name,
											  (const char *)cb_code_field(z)->name, NULL));
			str += output_stmt(cb_build_debug(cb_debug_contents,
											  NULL, z));
			str += output_perform_call(cb_code_field(z)->debug_section,
									   cb_code_field(z)->debug_section);
		}
		return str;
	}
	for(int i = 0; i < p->argc; i++) {
		if(p->varcnt && i + 1 == p->argc) {
			for(cb_tree l = p->argv[i]; l; l = CB_CHAIN(l)) {
				str += output_param(CB_VALUE(l), i);
				cb_tree z = CB_VALUE(l);
				if(CB_REF_OR_FIELD_P(z) &&
						cb_code_field(z)->flag_field_debug) {
					/* DEBUG */
					str += output_stmt(cb_build_debug(cb_debug_name,
													  (const char *)cb_code_field(z)->name, NULL));
					str += output_stmt(cb_build_debug(cb_debug_contents,
													  NULL, z));
					str += output_perform_call(cb_code_field(z)->debug_section,
											   cb_code_field(z)->debug_section);
				}
				i++;
			}
		} else {
			cb_tree z = p->argv[i];
			if(CB_REF_OR_FIELD_P(z) &&
					cb_code_field(z)->flag_field_debug) {
				/* DEBUG */
				str += output_stmt(cb_build_debug(cb_debug_name,
												  (const char *)cb_code_field(z)->name, NULL));
				str += output_stmt(cb_build_debug(cb_debug_contents,
												  NULL, z));
				str += output_perform_call(cb_code_field(z)->debug_section,
										   cb_code_field(z)->debug_section);
			}
		}
	}
	return str;
}

static string
output_cond_debug(cb_tree x)
{
	string str("");
	cb_binary_op * p;

	switch(CB_TREE_TAG(x)) {
	case CB_TAG_FUNCALL:
		str += output_funcall_debug(x);
		break;
	case CB_TAG_LIST:
		break;
	case CB_TAG_BINARY_OP:
		p = CB_BINARY_OP(x);
		switch(p->op) {
		case '!':
			str += output_cond_debug(p->x);
			break;

		case '&':
		case '|':
			str += output_cond_debug(p->x);
			str += output_cond_debug(p->y);
			break;

		case '=':
		case '<':
		case '[':
		case '>':
		case ']':
		case '~':
			str += output_cond_debug(p->x);
			break;

		default:
			if(CB_REF_OR_FIELD_P(x) &&
					cb_code_field(x)->flag_field_debug) {
				str += output_stmt(cb_build_debug(cb_debug_name,
												  (const char *)cb_code_field(x)->name, NULL));
				str += output_stmt(cb_build_debug(cb_debug_contents,
												  NULL, x));
				str += output_perform_call(cb_code_field(x)->debug_section,
										   cb_code_field(x)->debug_section);
			}
			break;
		}
		break;
	default:
		break;
	}
	return str;
}

static string
output_perform_once(cb_perform * p)
{
	string str("");
	if(p->body && CB_PAIR_P(p->body)) {
		str += output_perform_call(CB_LABEL(cb_ref(CB_PAIR_X(p->body))),
								   CB_LABEL(cb_ref(CB_PAIR_Y(p->body))));
	} else {
		str += output_stmt(p->body);
	}
	if(p->cycle_label) {
		str += output_stmt(cb_ref(p->cycle_label));
	}
	return str;
}

static string
output_perform_until(cb_perform * p, cb_tree l)
{
	string str("");

	if(l == NULL) {
		/* Perform body at the end */
		str += output_perform_once(p);
		return str;
	}

	cb_perform_varying * v = CB_PERFORM_VARYING(CB_VALUE(l));
	cb_tree next = CB_CHAIN(l);

#ifdef _MSC_VER
	str += output_indent("{");
#endif
	str += output_line("for(;;)");
	str += output_indent("{");

	if(next && CB_PERFORM_VARYING(CB_VALUE(next))->name) {
		str += output_move(CB_PERFORM_VARYING(CB_VALUE(next))->from,
						   CB_PERFORM_VARYING(CB_VALUE(next))->name);
		/* DEBUG */
		if(current_prog->flag_gen_debug) {
			cb_field * f = CB_FIELD(cb_ref(CB_PERFORM_VARYING(CB_VALUE(next))->name));
			if(f->flag_field_debug) {
				str += output_stmt(cb_build_debug(cb_debug_name,
												  (const char *)f->name, NULL));
				str += output_stmt(cb_build_debug(cb_debug_contents,
												  NULL, CB_PERFORM_VARYING(CB_VALUE(next))->name));
				str += output_perform_call(f->debug_section,
										   f->debug_section);
			}
		}

	}

	if(p->test == CB_AFTER) {
		str += output_perform_until(p, next);
	}

	/* DEBUG */
	if(current_prog->flag_gen_debug) {
		str += output_cond_debug(v->until);
	}

	str += output_prefix();
	str += output("if(");
	str += output_cond(v->until, 0);
	str += output(")\n");
	str += output_line("  break;");

	if(p->test == CB_BEFORE) {
		str += output_perform_until(p, next);
	}

	if(v->step) {
		str += output_stmt(v->step);
	}

	str += output_indent("}");
#ifdef _MSC_VER
	str += output_indent("}");
#endif
	return str;
}

static string
output_perform(cb_perform * p)
{
	string str("");
	cb_perform_varying * v;

	switch(p->perform_type) {
	case CB_PERFORM_EXIT:
		if(CB_LABEL(p->data)->flag_return) {
			str += output_perform_exit(CB_LABEL(p->data));
		}
		break;
	case CB_PERFORM_ONCE:
		str += output_perform_once(p);
		break;
	case CB_PERFORM_TIMES:
#ifdef _MSC_VER
		str += output_indent("{");
#endif
		str += output_prefix();
		str += output("for(_n%d = ", loop_counter);
		str += output_param(cb_build_cast_llint(p->data), 0);
		str += output("; _n%d > 0; _n%d--)\n", loop_counter, loop_counter);
		loop_counter++;
		str += output_indent("{");
		str += output_perform_once(p);
		str += output_indent("}");
#ifdef _MSC_VER
		str += output_indent("}");
#endif
		break;
	case CB_PERFORM_UNTIL:
		v = CB_PERFORM_VARYING(CB_VALUE(p->varying));
		if(v->name) {
			str += output_move(v->from, v->name);
			/* DEBUG */
			if(current_prog->flag_gen_debug) {
				cb_field * f = CB_FIELD(cb_ref(v->name));
				if(f->flag_field_debug) {
					str += output_stmt(cb_build_debug(cb_debug_name,
													  (const char *)f->name, NULL));
					str += output_stmt(cb_build_debug(cb_debug_contents,
													  NULL, v->name));
					str += output_perform_call(f->debug_section,
											   f->debug_section);
				}
			}

		}
		str += output_perform_until(p, p->varying);
		break;
	case CB_PERFORM_FOREVER:
#ifdef _MSC_VER
		str += output_indent("{");
#endif
		str += output_prefix();
		str += output("for(;;)\n");
		str += output_indent("{");
		str += output_perform_once(p);
		str += output_indent("}");
#ifdef _MSC_VER
		str += output_indent("}");
#endif
		break;
	default:
		break;
	}
	if(p->exit_label) {
		str += output_stmt(cb_ref(p->exit_label));
	}
	return str;
}

static string
output_file_error(cb_file * pfile)
{
	string str("");

	if(current_prog->flag_gen_debug) {
		str += output_stmt(cb_build_debug(cb_debug_contents,
										  "USE PROCEDURE", NULL));
	}
	for(cb_tree l =  current_prog->local_file_list; l; l = CB_CHAIN(l)) {
		cb_file * fl = CB_FILE(CB_VALUE(l));
		if(!strcmp(pfile->name, fl->name)) {
			str += output_perform_call(fl->handler,
									   fl->handler);
			return str;
		}
	}
	for(cb_tree l =  current_prog->global_file_list; l; l = CB_CHAIN(l)) {
		cb_file * fl = CB_FILE(CB_VALUE(l));
		if(!strcmp(pfile->name, fl->name)) {
			if(fl->handler_prog == current_prog) {
				str += output_perform_call(fl->handler,
										   fl->handler);
			} else {
				if(fl->handler_prog->nested_level) {
					str += output_line("%s_%d_(%d);",
									   fl->handler_prog->program_id,
									   fl->handler_prog->toplev_count,
									   fl->handler->id);
				} else {
					str += output_line("%s_(%d);",
									   fl->handler_prog->program_id,
									   fl->handler->id);
				}
			}
			return str;
		}
	}
	str += output_perform_call(pfile->handler, pfile->handler);
	return str;
}

/* GO TO */

static string
output_goto_1(cb_tree x)
{
	string str("");

	cb_label * lb = CB_LABEL(cb_ref(x));
	if(current_prog->flag_segments && last_segment != lb->segment) {
		/* Zap independent labels */
		cb_para_label * p;
		if(lb->flag_section) {
			p = lb->para_label;
		} else if(lb->section) {
			p = lb->section->para_label;
		} else {
			p = NULL;
		}
		for(; p; p = p->next) {
			if(p->para->segment > 49 &&
					p->para->flag_alter) {
				str += output_line("label_%s%d = 0;",
								   CB_PREFIX_LABEL, p->para->id);
			}
		}
	}

	/* Check for debugging on procedure */
	if(current_prog->flag_gen_debug && lb->flag_real_label &&
			(current_prog->all_procedure || lb->flag_debugging_mode)) {
		str += output_stmt(cb_build_debug(cb_debug_name,
										  (const char *)lb->name, NULL));
		str += output_move(cb_space, cb_debug_contents);
	}

	str += output_line("goto %s%d;", CB_PREFIX_LABEL, lb->id);
	return str;
}

static string
output_goto(cb_goto * p)
{
	string str("");

	int i = 1;
	if(p->depending) {
		/* Check for debugging on the DEPENDING item */
		if(current_prog->flag_gen_debug) {
			cb_field * f = CB_FIELD(cb_ref(p->depending));
			if(f->flag_all_debug) {
				str += output_stmt(cb_build_debug(cb_debug_name,
												  (const char *)f->name, NULL));
				str += output_stmt(cb_build_debug(cb_debug_contents,
												  NULL, p->depending));
				str += output_perform_call(f->debug_section,
										   f->debug_section);
			}
		}
		str += output_prefix();
		str += output("switch(");
		str += output_param(cb_build_cast_int(p->depending), 0);
		str += output(")\n");
		str += output_indent("{");
		for(cb_tree l = p->target; l; l = CB_CHAIN(l)) {
			output_indent_level -= 2;
			str += output_line("case %d:", i++);
			output_indent_level += 2;
			str += output_goto_1(CB_VALUE(l));
		}
		str += output_indent("}");
	} else if(p->target == NULL) {
		/* EXIT PROGRAM/FUNCTION */
		needs_exit_prog = 1;
		if(cb_flag_implicit_init || current_prog->nested_level ||
				current_prog->prog_type == CB_FUNCTION_TYPE) {
			str += output_line("goto exit_program;");
		} else {
			/* Ignore if not a callee */
			str += output_line("if(module->next)");
			str += output_line("  goto exit_program;");
		}
	} else if(p->target == cb_int1) {
		needs_exit_prog = 1;
		str += output_line("goto exit_program;");
	} else {
		str += output_goto_1(p->target);
	}
	return str;
}

/* ALTER */

static string
output_alter(cb_alter * p)
{
	string str("");

	cb_label * l1 = CB_LABEL(CB_REFERENCE(p->source)->value);
	cb_label * l2 = CB_LABEL(CB_REFERENCE(p->target)->value);
	str += output_line("label_%s%d = %d;", CB_PREFIX_LABEL, l1->id, l2->id);

	/* Check for debugging on procedure name */
	if(current_prog->flag_gen_debug && l1->flag_real_label &&
			(current_prog->all_procedure || l1->flag_debugging_mode)) {
		str += output_stmt(cb_build_debug(cb_debug_name,
										  (const char *)l1->name, NULL));
		str += output_stmt(cb_build_debug(cb_debug_contents,
										  (const char *)l2->name, NULL));
		if(current_prog->all_procedure) {
			str += output_perform_call(current_prog->all_procedure,
									   current_prog->all_procedure);
		} else if(l1->flag_debugging_mode) {
			str += output_perform_call(l1->debug_section,
									   l1->debug_section);
		}
	}
	return str;
}

/* Output statement */

static int
get_ec_code_for_handler(const enum cb_handler_type handler_type)
{
	switch(handler_type) {
	case AT_END_HANDLER:
		return CB_EXCEPTION_CODE(COB_EC_I_O_AT_END);
	case EOP_HANDLER:
		return CB_EXCEPTION_CODE(COB_EC_I_O_EOP);
	case INVALID_KEY_HANDLER:
		return CB_EXCEPTION_CODE(COB_EC_I_O_INVALID_KEY);
	default:
		/* LCOV_EXCL_START */
		cobc_err_msg(_("unexpected handler type: %d"), (int) handler_type);
		COBC_ABORT();
		/* LCOV_EXCL_STOP */
	}
}

static string
output_ferror_stmt(cb_statement * p)
{
	string str = output_line("if(unlikely(cob_glob_ptr->cob_exception_code != 0))");
	str += output_indent("{");
	if(p->ex_handler) {
		str += output_line("if(cob_glob_ptr->cob_exception_code == 0x%04x)",
						   get_ec_code_for_handler(p->handler_type));
		str += output_indent("{");
		str += output_stmt(p->ex_handler);
		str += output_indent("}");
		str += output_line("else");
		str += output_indent("{");
	}
	str += output_file_error(CB_FILE(p->file));
	str += output_indent("}");
	if(p->ex_handler) {
		str += output_indent("}");
	}
	if(p->not_ex_handler || p->handler3) {
		str += output_line("else");
		str += output_indent("{");
		if(p->handler3) {
			str += output_stmt(p->handler3);
		}
		if(p->not_ex_handler) {
			str += output_stmt(p->not_ex_handler);
		}
		str += output_indent("}");
	}
	return str;
}

static string
output_section_info(cb_label * lp)
{
	if(lp == cb_standard_error_handler) {
		return "";
	}
	if(lp->flag_dummy_exit) {
		return "";
	}
	if(lp->flag_section) {
		if(!lp->flag_dummy_section) {
			sprintf(string_buffer, "Section:   %s", lp->orig_name);
		} else {
			sprintf(string_buffer, "Section:   (None)");
		}
	} else if(lp->flag_entry) {
		sprintf(string_buffer, "Entry:     %s", lp->orig_name);
	} else {
		if(!lp->flag_dummy_paragraph) {
			sprintf(string_buffer, "Paragraph: %s", lp->orig_name);
		} else {
			sprintf(string_buffer, "Paragraph: (None)");
		}
	}
	if(lp->source_file) {
		return output_line("cob_trace_section(%s%d, %s%d, %d);",
						   CB_PREFIX_STRING,
						   lookup_string(string_buffer),
						   CB_PREFIX_STRING,
						   lookup_string(lp->source_file),
						   lp->source_line);
	}
	return output_line("cob_trace_section(%s%d, NULL, %d);",
					   CB_PREFIX_STRING,
					   lookup_string(string_buffer),
					   lp->source_line);
}

static string
output_trace_info(cb_tree x, const char * name)
{
	string str = output_prefix();
	str += output("cob_set_location(%s%d, %d, ",
				  CB_PREFIX_STRING,
				  lookup_string(x->source_file),
				  x->source_line);
	if(excp_current_section) {
		str += output("%s%d, ",
					  CB_PREFIX_STRING, lookup_string(excp_current_section));
	} else {
		str += output("NULL, ");
	}
	if(excp_current_paragraph) {
		str += output("%s%d, ",
					  CB_PREFIX_STRING, lookup_string(excp_current_paragraph));
	} else {
		str += output("NULL, ");
	}
	if(name) {
		str += output("%s%d);\n",
					  CB_PREFIX_STRING, lookup_string(name));
	} else {
		str += output("NULL);\n");
	}
	return str;
}

static string
output_label_info(cb_tree x, cb_label * lp)
{
	if(lp->flag_dummy_section || lp->flag_dummy_paragraph) {
		return "";
	}

	string str = output_newline();

	if(lp->flag_dummy_exit) {
		str += output_line("/* Implicit EXIT label */");
		return str;
	} else if(lp->flag_next_sentence) {
		str += output_line("/* Implicit NEXT SENTENCE label */");
		return str;
	}

	str += output_prefix();
	if(x->source_file) {
		str += output("/* Line: %-10d: ", x->source_line);
	} else {
		str += output("/* ");
	}
	if(lp->flag_section) {
		str += output("Section   %-24s", (const char *)lp->name);
		excp_current_section = (const char *)lp->name;
		excp_current_paragraph = NULL;
	} else {
		if(lp->flag_entry) {
			str += output("Entry     %-24s", lp->orig_name);
			excp_current_section = NULL;
			excp_current_paragraph = NULL;
		} else {
			str += output("Paragraph %-24s", (const char *)lp->name);
			excp_current_paragraph = (const char *)lp->name;
		}
	}
	if(x->source_file) {
		str += output(": %s */\n", x->source_file);
	} else {
		str += output("*/\n");
	}
	return str;
}

static string
output_alter_check(cb_label * lp)
{
	output_local("static int\tlabel_%s%d = 0;\n",
				 CB_PREFIX_LABEL, lp->id);
	if(current_prog->flag_segments) {
		output_local("static int\tsave_label_%s%d = 0;\n",
					 CB_PREFIX_LABEL, lp->id);
	}
	string str = output_newline();
	str += output_line("/* ALTER processing */");
	str += output_line("switch(label_%s%d)",
					   CB_PREFIX_LABEL, lp->id);
	str += output_indent("{");
	for(cb_alter_id * aid = lp->alter_gotos; aid; aid = aid->next) {
		str += output_line("case %d:", aid->goto_id);
		str += output_line("goto %s%d;", CB_PREFIX_LABEL, aid->goto_id);
	}
	str += output_indent("}");
	str += output_newline();
	return str;
}

static string
output_level_2_ex_condition(const int level_2_ec)
{
	return output_line("if(unlikely((cob_glob_ptr->cob_exception_code & 0xff00) == 0x%04x))",
					   CB_EXCEPTION_CODE(level_2_ec));
}

static string
output_display_accept_ex_condition(const cb_handler_type handler_type)
{
	string str = output_line("if(unlikely((cob_glob_ptr->cob_exception_code & 0xff00) == 0x%04x",
							 CB_EXCEPTION_CODE(COB_EC_SCREEN));

	int imp_ec = (handler_type == DISPLAY_HANDLER) ? COB_EC_IMP_DISPLAY : /* ACCEPT_HANDLER */ COB_EC_IMP_ACCEPT;
	str += output_line("               || cob_glob_ptr->cob_exception_code == 0x%04x))", CB_EXCEPTION_CODE(imp_ec));
	return str;
}

static string
output_ec_condition_for_handler(const cb_handler_type handler_type)
{
	switch(handler_type) {
	case DISPLAY_HANDLER:
		return output_display_accept_ex_condition(DISPLAY_HANDLER);

	case ACCEPT_HANDLER:
		return output_display_accept_ex_condition(ACCEPT_HANDLER);

	case SIZE_ERROR_HANDLER:
		return output_level_2_ex_condition(COB_EC_SIZE);

	case OVERFLOW_HANDLER:
		return output_level_2_ex_condition(COB_EC_OVERFLOW);

	default:
		/* LCOV_EXCL_START */
		cobc_err_msg(_("unexpected handler type: %d"), (int) handler_type);
		COBC_ABORT();
		/* LCOV_EXCL_STOP */
	}
	return "";
}

static string
output_handler(cb_statement * stmt)
{
	if(stmt->file) {
		return output_ferror_stmt(stmt);
	}

	string str("");
	if(stmt->ex_handler) {
		str += output_ec_condition_for_handler(stmt->handler_type);
		str += output_indent("{");
		str += output_stmt(stmt->ex_handler);
		str += output_indent("}");
		if(stmt->not_ex_handler) {
			str += output_line("else");
		}
	}
	if(stmt->not_ex_handler) {
		if(stmt->ex_handler == NULL) {
			str += output_line("if(!cob_glob_ptr->cob_exception_code)");
		}
		str += output_indent("{");
		str += output_stmt(stmt->not_ex_handler);
		str += output_indent("}");
	}
	return str;
}

static string
output_stmt(cb_tree x)
{
	string str("");
	cb_statement * p;
	cb_label * lp;
	cb_assign * ap;
	cb_if * ip;
	cb_para_label * pal;
	cb_set_attr	* sap;
#ifdef	COB_NON_ALIGNED
	cb_cast * cp;
#endif
	size_t size;
	int code;

	stack_id = 0;
	if(x == NULL) {
		str += output_line(";");
		return str;
	}
	if(unlikely(x == cb_error_node)) {
		/* LCOV_EXCL_START */
		cobc_err_msg(_("unexpected error_node parameter"));
		COBC_ABORT();
		/* LCOV_EXCL_STOP */
	}

	if(inside_check != 0) {
		if(inside_stack[inside_check - 1] != 0) {
			inside_stack[inside_check - 1] = 0;
			str += output(",\n");
		}
	}

	switch(CB_TREE_TAG(x)) {
	case CB_TAG_STATEMENT:
		p = CB_STATEMENT(x);
		/* Output source location as a comment */
		if(p->name) {
			str += output_newline();
			str += output_line("/* Line: %-10d: %-19.19s: %s */",
							   x->source_line, p->name, x->source_file);
		}
		if(x->source_file) {
			if(cb_flag_source_location) {
				/* Output source location as code */
				str += output_trace_info(x, p->name);
			}
			if(current_prog->flag_gen_debug &&
					!p->flag_in_debug) {
				str += output_prefix();
				str += output("memcpy(");
				str += output_data(cb_debug_line);
				str += output(", \"%6d\", 6);\n", x->source_line);
			}
			last_line = x->source_line;
		}

		if(!p->file && (p->ex_handler || p->not_ex_handler)) {
			str += output_line("cob_glob_ptr->cob_exception_code = 0;");
		}

		if(p->null_check) {
			str += output_stmt(p->null_check);
		}

		if(p->body) {
			str += output_stmt(p->body);
		}

		/* Output field debugging statements */
		if(current_prog->flag_gen_debug && p->debug_check) {
			str += output_stmt(p->debug_check);
		}

		/* Special debugging callback for START / DELETE */
		/* Must be done immediately after I/O and before */
		/* status check */
		if(current_prog->flag_gen_debug && p->file && p->flag_callback) {
			str += output_line("save_exception_code = cob_glob_ptr->cob_exception_code;");
			str += output_stmt(cb_build_debug(cb_debug_name,
											  CB_FILE(p->file)->name, NULL));
			str += output_move(cb_space, cb_debug_contents);
			str += output_perform_call(CB_FILE(p->file)->debug_section,
									   CB_FILE(p->file)->debug_section);
			str += output_line("cob_glob_ptr->cob_exception_code = save_exception_code;");
			need_save_exception = 1;
		}

		if(p->ex_handler || p->not_ex_handler ||
				(p->file && CB_EXCEPTION_ENABLE(COB_EC_I_O))) {
			str += output_handler(p);
		}
		break;
	case CB_TAG_LABEL:
		lp = CB_LABEL(x);
		if(lp->flag_skip_label) {
			break;
		}
		str += output_label_info(x, lp);
		if(lp->flag_section) {
			for(pal = lp->para_label; pal; pal = pal->next) {
				if(pal->para->segment > 49 &&
						pal->para->flag_alter) {
					str += output_line("label_%s%d = 0;",
									   CB_PREFIX_LABEL, pal->para->id);
				}
			}
			last_segment = lp->segment;
			last_section = lp;
		}
		if(lp->flag_begin) {
			str += output_line("%s%d:;", CB_PREFIX_LABEL, lp->id);
		}

		/* Check for runtime debug flag */
		if(current_prog->flag_debugging && lp->flag_is_debug_sect) {
			str += output_line("if(!cob_debugging_mode)");
			str += output_line("\tgoto %s%d;",
							   CB_PREFIX_LABEL, CB_LABEL(lp->exit_label)->id);
		}

		if(cb_flag_trace) {
			str += output_section_info(lp);
		}

		/* Check procedure debugging */
		if(current_prog->flag_gen_debug && lp->flag_real_label) {
			str += output_stmt(cb_build_debug(cb_debug_name,
											  (const char *)lp->name, NULL));
			if(current_prog->all_procedure) {
				str += output_perform_call(current_prog->all_procedure,
										   current_prog->all_procedure);
			} else if(lp->flag_debugging_mode) {
				str += output_perform_call(lp->debug_section,
										   lp->debug_section);
			}
		}

		/* Check ALTER processing */
		if(lp->flag_alter) {
			str += output_alter_check(lp);
		}

		break;
	case CB_TAG_FUNCALL:
		str += output_prefix();
		str += output_funcall(x);
		if(inside_check == 0) {
			str += output(";\n");
		} else {
			inside_stack[inside_check - 1] = 1;
		}
		break;
	case CB_TAG_ASSIGN:
		ap = CB_ASSIGN(x);
#ifdef	COB_NON_ALIGNED
		/* Nonaligned */
		if(CB_TREE_TAG(ap->val) != CB_TAG_BINARY_OP &&
				(CB_TREE_CLASS(ap->var) == CB_CLASS_POINTER ||
				 CB_TREE_CLASS(ap->val) == CB_CLASS_POINTER)) {
			/* Pointer assignment */
			str += output_indent("{");
			str += output_line("void *temp_ptr;");

			/* temp_ptr = source address; */
			str += output_prefix();
			if(ap->val == cb_null || ap->val == cb_zero) {
				/* MOVE NULL ... */
				str += output("temp_ptr = 0;\n");
			} else if(CB_TREE_TAG(ap->val) == CB_TAG_CAST) {
				/* MOVE ADDRESS OF val ... */
				cp = CB_CAST(ap->val);
				str += output("temp_ptr = ");
				switch(cp->cast_type) {
				case CB_CAST_ADDRESS:
					str += output_data(cp->val);
					break;
				case CB_CAST_PROGRAM_POINTER:
					str += output("(unsigned char *)cob_call_field(");
					str += output_param(ap->val, -1);
					if(current_prog->nested_prog_list) {
						gen_nested_tab = 1;
						str += output(", cob_nest_tab, 0, %d)",
									  cb_fold_call);
					} else {
						str += output(", NULL, 0, %d)",
									  cb_fold_call);
					}
					break;
				default:
					/* LCOV_EXCL_START */
					cobc_err_msg(_("unexpected cast type: %d"),
								 cp->cast_type);
					COBC_ABORT();
					/* LCOV_EXCL_STOP */
				}
				str += output(";\n");
			} else {
				/* MOVE val ... */
				str += output("memcpy(&temp_ptr, ");
				str += output_data(ap->val);
				str += output(", sizeof(temp_ptr));\n");
			}

			/* Destination address = temp_ptr; */
			str += output_prefix();
			if(CB_TREE_TAG(ap->var) == CB_TAG_CAST) {
				/* SET ADDRESS OF var ... */
				cp = CB_CAST(ap->var);
				if(cp->cast_type != CB_CAST_ADDRESS) {
					/* LCOV_EXCL_START */
					cobc_err_msg(_("unexpected tree type: %d"),
								 cp->cast_type);
					COBC_ABORT();
					/* LCOV_EXCL_STOP */
				}
				str += output("*((void **)&");
				str += output_data(cp->val);
				str += output(") = temp_ptr;\n");
			} else {
				/* MOVE ... TO var */
				str += output("memcpy(");
				str += output_data(ap->var);
				str += output(", &temp_ptr, sizeof(temp_ptr));\n");
			}

			str += output_indent("}");
		} else {
			/* Numeric assignment */
			str += output_prefix();
			str += output_integer(ap->var);
			str += output(" = ");
			str += output_integer(ap->val);
			if(inside_check == 0) {
				str += output(";\n");
			} else {
				inside_stack[inside_check - 1] = 1;
			}
		}
#else	/* Nonaligned */
		str += output_prefix();
		str += output_integer(ap->var);
		str += output(" = ");
		str += output_integer(ap->val);
		if(inside_check == 0) {
			str += output(";\n");
		} else {
			inside_stack[inside_check - 1] = 1;
		}
#endif	/* Nonaligned */
		break;
	case CB_TAG_INITIALIZE:
		str += output_initialize(CB_INITIALIZE(x));
		break;
	case CB_TAG_SEARCH:
		str += output_search(CB_SEARCH(x));
		break;
	case CB_TAG_CALL:
		str += output_call(CB_CALL(x));
		break;
	case CB_TAG_GOTO:
		str += output_goto(CB_GOTO(x));
		break;
	case CB_TAG_CANCEL:
		str += output_cancel(CB_CANCEL(x));
		break;
	case CB_TAG_SET_ATTR:
		sap = CB_SET_ATTR(x);
		str += output_set_attribute(sap->fld, sap->val_on, sap->val_off);
		break;
	case CB_TAG_ALTER:
		str += output_alter(CB_ALTER(x));
		break;
	case CB_TAG_IF:
		ip = CB_IF(x);
		if(ip->stmt1 == NULL
				&& ip->stmt2 == NULL) {
			if(!ip->is_if) {
				str += output_line("/* WHEN has code omitted */");
			} else {
				str += output_line("/* IF has code omitted */");
			}
			break;
		}
		if(!ip->is_if) {
			str += output_newline();
			if(ip->test == cb_true) {
				str += output_line("/* WHEN is always TRUE */");
			} else if(ip->test == cb_false) {
				str += output_line("/* WHEN is always FALSE */");
			} else if(ip->test
					  && CB_TREE_TAG(ip->test) == CB_TAG_BINARY_OP) {
				cb_binary_op * bop = CB_BINARY_OP(ip->test);
				cb_tree w = NULL;
				if(bop->op == '!') {
					w = bop->x;
				} else if(bop->y) {
					w = bop->y;
				} else if(bop->x) {
					w = bop->x;
				}
				if(w == cb_true) {
					str += output_line("/* WHEN is always %s */", bop->op == '!' ? "FALSE" : "TRUE");
				} else if(w == cb_false) {
					str += output_line("/* WHEN is always %s */", bop->op != '!' ? "FALSE" : "TRUE");
				} else if(w && w->source_line) {
					str += output_prefix();
					str += output("/* Line: %-10d: %-19s", w->source_line, "WHEN");
					if(w->source_file) {
						str += output(": %s ", w->source_file);
					}
					str += output("*/\n");
					if(cb_flag_source_location
							&& last_line != w->source_line) {
						/* Output source location as code */
						str += output_trace_info(w, "WHEN");
					}
				} else {
					str += output_line("/* WHEN */");
				}
			} else if(ip->test->source_line) {
				str += output_line("/* Line: %-10d: WHEN */", ip->test->source_line);
				if(cb_flag_source_location
						&& last_line != ip->test->source_line) {
					/* Output source location as code */
					str += output_trace_info(ip->test, "WHEN");
				}
			} else {
				str += output_line("/* WHEN */");
			}
			str += output_newline();
		}
		gen_if_level++;
		code = 0;
		str += output_prefix();
		int skip_else;
		if(ip->test == cb_false
				&& ip->stmt1 == NULL) {
			str += output_line(" /* FALSE condition and code omitted */");
			skip_else = 1;
		} else {
			skip_else = 0;
			str += output("if(");
			str += output_cond(ip->test, 0);
			str += output(")\n");
			str += output_line("{");
			output_indent_level += 2;
			if(ip->stmt1) {
				str += output_stmt(ip->stmt1);
			} else {
				str += output_line("; /* Nothing */");
			}
			if(gen_if_level > cb_if_cutoff) {
				if(ip->stmt2) {
					code = cb_id++;
					str += output_line("goto l_%d;", code);
				}
			}
			output_indent_level -= 2;
			str += output_line("}");
		}
		if(ip->stmt2) {
			if(gen_if_level <= cb_if_cutoff) {
				if(!skip_else) {
					str += output_line("else");
				}
				str += output_line("{");
				output_indent_level += 2;
			}
			if(ip->is_if) {
				str += output_line("/* ELSE */");
			} else {
				str += output_line("/* WHEN */");
			}
			str += output_stmt(ip->stmt2);
			if(gen_if_level <= cb_if_cutoff) {
				output_indent_level -= 2;
				str += output_line("}");
			} else {
				str += output_line("l_%d:;", code);
			}
		}
		gen_if_level--;
		break;
	case CB_TAG_PERFORM:
		str += output_perform(CB_PERFORM(x));
		break;
	case CB_TAG_CONTINUE:
		str += output_prefix();
		str += output(";\n");
		break;
	case CB_TAG_LIST:
		if(cb_flag_extra_brace) {
			str += output_indent("{");
		}
		for(; x; x = CB_CHAIN(x)) {
			str += output_stmt(CB_VALUE(x));
		}
		if(cb_flag_extra_brace) {
			str += output_indent("}");
		}
		break;
	case CB_TAG_REFERENCE:
		str += output_stmt(CB_REFERENCE(x)->value);
		break;
	case CB_TAG_DIRECT:
		if(CB_DIRECT(x)->flag_is_direct) {
			if(CB_DIRECT(x)->flag_new_line) {
				str += output_newline();
			}
			str += output_line("%s", (const char *)(CB_DIRECT(x)->line));
		} else {
			str += output_newline();
			str += output_line("/* %s */", (const char *)(CB_DIRECT(x)->line));
		}
		break;
	case CB_TAG_DEBUG:
		if(!current_prog->flag_gen_debug) {
			break;
		}
		str += output_prefix();
		size = cb_code_field(CB_DEBUG(x)->target)->size;
		if(CB_DEBUG(x)->value) {
			if(size <= CB_DEBUG(x)->size) {
				str += output("memcpy(");
				str += output_data(CB_DEBUG(x)->target);
				str += output(", %s%d, %d);\n", CB_PREFIX_STRING,
							  lookup_string(CB_DEBUG(x)->value),
							  (int)size);
			} else {
				str += output("memcpy(");
				str += output_data(CB_DEBUG(x)->target);
				str += output(", %s%d, %d);\n", CB_PREFIX_STRING,
							  lookup_string(CB_DEBUG(x)->value),
							  (int)CB_DEBUG(x)->size);
				str += output_prefix();
				str += output("memset(");
				str += output_data(CB_DEBUG(x)->target);
				code = (int)(size - CB_DEBUG(x)->size);
				str += output(" + %d, ' ', %d);\n",
							  (int)CB_DEBUG(x)->size, code);

			}
		} else {
			if(size <= CB_DEBUG(x)->size) {
				str += output("memcpy(");
				str += output_data(CB_DEBUG(x)->target);
				str += output(", ");
				str += output_data(CB_DEBUG(x)->fld);
				str += output(", %d);\n", (int)size);
			} else {
				str += output("memcpy(");
				str += output_data(CB_DEBUG(x)->target);
				str += output(", ");
				str += output_data(CB_DEBUG(x)->fld);
				str += output(", %d);\n", (int)CB_DEBUG(x)->size);
				str += output_prefix();
				str += output("memset(");
				str += output_data(CB_DEBUG(x)->target);
				code = (int)(size - CB_DEBUG(x)->size);
				str += output(" + %d, ' ', %d);\n",
							  (int)CB_DEBUG(x)->size, code);
			}
		}
		break;
	case CB_TAG_DEBUG_CALL:
		str += output_perform_call(CB_DEBUG_CALL(x)->target,
								   CB_DEBUG_CALL(x)->target);
		break;
	default:
		/* LCOV_EXCL_START */
		cobc_err_msg(_("unexpected tree tag: %d"), (int)CB_TREE_TAG(x));
		COBC_ABORT();
		/* LCOV_EXCL_STOP */
	}
	return str;
}

/* File definition */

static int
output_file_allocation(cb_file * f)
{
	if(f->flag_global) {
#if	0	/* RXWRXW - Global status */
		if(f->file_status) {
			/* Force status into main storage file */
			CB_FIELD_PTR(f->file_status)->flag_is_global = 1;
		}
#endif
		output_storage("\n/* Global file %s */\n", f->name);
	} else {
		output_local("\n/* File %s */\n", f->name);
	}
	/* Output RELATIVE/RECORD KEY's */
	if(f->organization == COB_ORG_RELATIVE ||
			f->organization == COB_ORG_INDEXED) {
		if(f->flag_global) {
			output_storage("static cob_file_key\t*%s%s = NULL;\n",
						   CB_PREFIX_KEYS, f->cname);
		} else {
			output_local("static cob_file_key\t*%s%s = NULL;\n",
						 CB_PREFIX_KEYS, f->cname);
		}
	}
	if(f->flag_global) {
		output_storage("static cob_file\t\t*%s%s = NULL;\n",
					   CB_PREFIX_FILE, f->cname);
		output_storage("static unsigned char\t%s%s_status[4];\n",
					   CB_PREFIX_FILE, f->cname);
	} else {
		output_local("static cob_file\t\t*%s%s = NULL;\n",
					 CB_PREFIX_FILE, f->cname);
		output_local("static unsigned char\t%s%s_status[4];\n",
					 CB_PREFIX_FILE, f->cname);
	}

	if(f->code_set) {
		gen_native = 1;
		switch(f->code_set->alphabet_type) {
		case CB_ALPHABET_ASCII:
			gen_ebcdic_ascii = 1;
			break;
		case CB_ALPHABET_EBCDIC:
			gen_full_ebcdic = 1;
			break;
		case CB_ALPHABET_CUSTOM:
			gen_custom = 1;
			break;
		default:
			break;
		}
	}

	if(f->linage) {
		return 1;
	}
	return 0;
}

static string
output_file_initialization(cb_file * f)
{
	string str("");
	int nkeys;
	char key_ptr[64];

	str += output_line("/* File initialization for %s */", f->name);
	if(f->organization == COB_ORG_RELATIVE
			|| f->organization == COB_ORG_INDEXED) {
		nkeys = 1;
		for(cb_alt_key * l = f->alt_key_list; l; l = l->next) {
			nkeys++;
		}
		sprintf(key_ptr, "&%s%s", CB_PREFIX_KEYS, f->cname);
	} else {
		nkeys = 0;
		strcpy(key_ptr, "NULL");
	}
	if(f->flag_external) {
		str += output_line("cob_file_external_addr(\"%s\", &%s%s, %s, %d, %d);",
						   f->cname,
						   CB_PREFIX_FILE, f->cname,
						   key_ptr, nkeys, f->linage ? 1 : 0);
		str += output_line("if(cob_glob_ptr->cob_initial_external)");
		str += output_indent("{");
	} else {
		str += output_line("cob_file_malloc (&%s%s, %s, %d, %d);",
						   CB_PREFIX_FILE, f->cname,
						   key_ptr, nkeys, f->linage ? 1 : 0);
	}
	nkeys = 1;
	/* Output RELATIVE/RECORD KEY's */
	if(f->organization == COB_ORG_RELATIVE
			|| f->organization == COB_ORG_INDEXED) {
		str += output_prefix();
		str += output("%s%s->field = ", CB_PREFIX_KEYS, f->cname);
		str += output_param(f->key, -1);
		str += output(";\n");
		str += output_prefix();
		str += output("%s%s->flag = 0;\n", CB_PREFIX_KEYS, f->cname);
		str += output_prefix();
		if(f->key) {
			str += output("%s%s->offset = %d;\n", CB_PREFIX_KEYS, f->cname,
						  cb_code_field(f->key)->offset);
		} else {
			str += output("%s%s->offset = 0;\n", CB_PREFIX_KEYS, f->cname);
		}
		for(cb_alt_key * l = f->alt_key_list; l; l = l->next) {
			str += output_prefix();
			str += output("(%s%s + %d)->field = ", CB_PREFIX_KEYS, f->cname,
						  nkeys);
			str += output_param(l->key, -1);
			str += output(";\n");
			str += output_prefix();
			str += output("(%s%s + %d)->flag = %d;\n", CB_PREFIX_KEYS,
						  f->cname, nkeys, l->duplicates);
			str += output_prefix();
			str += output("(%s%s + %d)->offset = %d;\n", CB_PREFIX_KEYS,
						  f->cname, nkeys, cb_code_field(l->key)->offset);
			nkeys++;
		}
	}

	str += output_line("%s%s->select_name = (const char *)\"%s\";", CB_PREFIX_FILE,
					   f->cname, f->name);
	if(f->flag_external && !f->file_status) {
		str += output_line("%s%s->file_status = (unsigned char *) cob_external_addr(\"%s%s_status\", 4);",
						   CB_PREFIX_FILE, f->cname, CB_PREFIX_FILE, f->cname);
	} else {
		str += output_line("%s%s->file_status = %s%s_status;", CB_PREFIX_FILE,
						   f->cname, CB_PREFIX_FILE, f->cname);
		str += output_line("memset(%s%s_status, '0', 2);", CB_PREFIX_FILE,
						   f->cname);
	}
	str += output_prefix();
	str += output("%s%s->assign = ", CB_PREFIX_FILE, f->cname);
	str += output_param(f->assign, -1);
	str += output(";\n");
	str += output_prefix();
	str += output("%s%s->record = ", CB_PREFIX_FILE, f->cname);
	str += output_param(f->record, -1);
	str += output(";\n");
	str += output_prefix();
	str += output("%s%s->variable_record = ", CB_PREFIX_FILE, f->cname);
	if(f->record_depending) {
		str += output_param(f->record_depending, -1);
	} else {
		str += output("NULL");
	}
	str += output(";\n");
	str += output_line("%s%s->record_min = %d;", CB_PREFIX_FILE,
					   f->cname, f->record_min);
	str += output_line("%s%s->record_max = %d;", CB_PREFIX_FILE,
					   f->cname, f->record_max);
	if(f->organization == COB_ORG_RELATIVE
			|| f->organization == COB_ORG_INDEXED) {
		str += output_line("%s%s->nkeys = %d;", CB_PREFIX_FILE,
						   f->cname, nkeys);
		str += output_line("%s%s->keys = %s%s;", CB_PREFIX_FILE,
						   f->cname, CB_PREFIX_KEYS, f->cname);
	} else {
		str += output_line("%s%s->nkeys = 0;", CB_PREFIX_FILE, f->cname);
		str += output_line("%s%s->keys = NULL;", CB_PREFIX_FILE, f->cname);
	}
	str += output_line("%s%s->file = NULL;", CB_PREFIX_FILE, f->cname);

	if(f->linage) {
		str += output_line("lingptr = (cob_linage *) %s%s->linorkeyptr;",
						   CB_PREFIX_FILE, f->cname);
		str += output_prefix();
		str += output("lingptr->linage = ");
		str += output_param(f->linage, -1);
		str += output(";\n");
		str += output_prefix();
		str += output("lingptr->linage_ctr = ");
		str += output_param(f->linage_ctr, -1);
		str += output(";\n");
		if(f->latfoot) {
			str += output_prefix();
			str += output("lingptr->latfoot = ");
			str += output_param(f->latfoot, -1);
			str += output(";\n");
		} else {
			str += output_line("lingptr->latfoot = NULL;");
		}
		if(f->lattop) {
			str += output_prefix();
			str += output("lingptr->lattop = ");
			str += output_param(f->lattop, -1);
			str += output(";\n");
		} else {
			str += output_line("lingptr->lattop = NULL;");
		}
		if(f->latbot) {
			str += output_prefix();
			str += output("lingptr->latbot = ");
			str += output_param(f->latbot, -1);
			str += output(";\n");
		} else {
			str += output_line("lingptr->latbot = NULL;");
		}
		str += output_line("lingptr->lin_lines = 0;");
		str += output_line("lingptr->lin_foot = 0;");
		str += output_line("lingptr->lin_top = 0;");
		str += output_line("lingptr->lin_bot = 0;");
	}

	str += output_line("%s%s->fd = -1;", CB_PREFIX_FILE, f->cname);
	str += output_line("%s%s->organization = %d;", CB_PREFIX_FILE, f->cname,
					   f->organization);
	str += output_line("%s%s->access_mode = %d;", CB_PREFIX_FILE, f->cname,
					   f->access_mode);
	str += output_line("%s%s->lock_mode = " CB_FMT_LLD ";", CB_PREFIX_FILE, f->cname,
					   f->lock_mode);
	str += output_line("%s%s->open_mode = 0;", CB_PREFIX_FILE, f->cname);
	str += output_line("%s%s->flag_optional = %d;", CB_PREFIX_FILE, f->cname,
					   f->optional);
	int features = 0;
	if(f->file_status) {
		features |= COB_SELECT_FILE_STATUS;
	}
	if(f->linage) {
		features |= COB_SELECT_LINAGE;
	}
	if(f->flag_ext_assign) {
		features |= COB_SELECT_EXTERNAL;
	}
	if(f->special) {
		/* Special assignment */
		features |= f->special;
	}
	str += output_line("%s%s->flag_select_features = %d;", CB_PREFIX_FILE, f->cname,
					   features);
	str += output_line("%s%s->file_version = %d;", CB_PREFIX_FILE, f->cname,
					   COB_FILE_VERSION);
	if(f->flag_external) {
		str += output_indent("}");
	}
	str += output_newline();
	return str;
}

/* Screen definition */

static void
output_screen_definition(cb_field * p)
{
	if(p->sister) {
		output_screen_definition(p->sister);
	}
	if(p->children) {
		output_screen_definition(p->children);
	}

	int type = (p->children ? COB_SCREEN_TYPE_GROUP :
				p->values ? COB_SCREEN_TYPE_VALUE :
				(p->size > 0) ? COB_SCREEN_TYPE_FIELD : COB_SCREEN_TYPE_ATTRIBUTE);
	if(type == COB_SCREEN_TYPE_FIELD || type == COB_SCREEN_TYPE_VALUE) {
		p->count++;
	}

	output_local("static cob_screen\ts_%d;\n", p->id);
}

static string
output_screen_init(cb_field * p, cb_field * previous)
{
	int type = (p->children ? COB_SCREEN_TYPE_GROUP :
				p->values ? COB_SCREEN_TYPE_VALUE :
				(p->size > 0) ? COB_SCREEN_TYPE_FIELD : COB_SCREEN_TYPE_ATTRIBUTE);
	string str = output_prefix();
	str += output("cob_set_screen(&s_%d, ", p->id);

	if(p->sister && p->sister->level != 1) {
		str += output("&s_%d, ", p->sister->id);
	} else {
		str += output("NULL, ");
	}

	if(previous && previous->level != 1) {
		str += output("&s_%d, ", previous->id);
	} else {
		str += output("NULL, ");
	}

	str += output_newline();
	str += output_prefix();
	str += output("\t\t  ");

	if(type == COB_SCREEN_TYPE_GROUP) {
		str += output("&s_%d, ", p->children->id);
	} else {
		str += output("NULL, ");
	}

	if(p->parent) {
		str += output("&s_%d, ", p->parent->id);
	} else {
		str += output("NULL, ");
	}

	if(type == COB_SCREEN_TYPE_FIELD) {
		str += output_param(cb_build_field_reference(p, NULL), -1);
		str += output(", ");
	} else {
		str += output("NULL, ");
	}

	str += output_newline();
	str += output_prefix();
	str += output("\t\t  ");

	if(type == COB_SCREEN_TYPE_VALUE) {
		/* Need a field reference here */
		str += output_param(cb_build_field_reference(p, NULL), -1);
		str += output(", ");
	} else {
		str += output("NULL, ");
	}

	if(p->screen_line) {
		str += output_param(p->screen_line, 0);
		str += output(", ");
	} else {
		str += output("NULL, ");
	}

	if(p->screen_column) {
		str += output_param(p->screen_column, 0);
		str += output(", ");
	} else {
		str += output("NULL, ");
	}

	str += output_newline();
	str += output_prefix();
	str += output("\t\t  ");

	if(p->screen_foreg) {
		str += output_param(p->screen_foreg, 0);
		str += output(", ");
	} else {
		str += output("NULL, ");
	}

	if(p->screen_backg) {
		str += output_param(p->screen_backg, 0);
		str += output(", ");
	} else {
		str += output("NULL, ");
	}

	if(p->screen_prompt) {
		str += output_param(p->screen_prompt, 0);
		str += output(", ");
	} else {
		str += output("NULL, ");
	}

	str += output_newline();
	str += output_prefix();
	str += output("\t\t  ");

	str += output("%d, %d, 0x" CB_FMT_LLX ");\n", type, p->occurs_min, p->screen_flag);

	if(p->children) {
		str += output_screen_init(p->children, NULL);
	}
	if(p->sister) {
		str += output_screen_init(p->sister, p);
	}
	return str;
}

/* Alphabet-name */

static int
literal_value(cb_tree x)
{
	if(x == cb_space) {
		return ' ';
	} else if(x == cb_zero) {
		return '0';
	} else if(x == cb_quote) {
		return cb_flag_apostrophe ? '\'' : '"';
	} else if(x == cb_norm_low) {
		return 0;
	} else if(x == cb_norm_high) {
		return 255;
	} else if(x == cb_null) {
		return 0;
	} else if(CB_TREE_CLASS(x) == CB_CLASS_NUMERIC) {
		return cb_get_int(x) - 1;
	} else {
		return CB_LITERAL(x)->data[0];
	}
}

static void
output_alphabet_name_definition(cb_alphabet_name * p)
{
	if(p->alphabet_type != CB_ALPHABET_CUSTOM) {
		return;
	}

	/* Output the table */
	output_local("static const unsigned char %s%s[256] = {\n",
				 CB_PREFIX_SEQUENCE, p->cname);
	for(int i = 0; i < 256; i++) {
		if(i == 255) {
			output_local(" %d", p->values[i]);
		} else {
			output_local(" %d,", p->values[i]);
		}
		if(i % 16 == 15) {
			output_local("\n");
		}
	}
	output_local("};\n");
	int i = lookup_attr(COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL, 0);
	output_local("static cob_field %s%s(256, (cob_u8_ptr)%s%s, &%s%d);\n",
				 CB_PREFIX_FIELD, p->cname,
				 CB_PREFIX_SEQUENCE, p->cname,
				 CB_PREFIX_ATTR, i);
	output_local("\n");
}

/* Class definition */

static string
output_class_name_definition(cb_class_name * p)
{
	string str = output_line("static int");
	str += output_line("%s(cob_field *f)", p->cname);
	str += output_indent("{");
	str += output_line("size_t\ti;\n");
	str += output_line("for(i = 0; i < f->size; i++)");
	str += output_indent("{");
	str += output_line("switch(f->data[i]) {");
	int vals[256];
	memset(vals, 0, sizeof(vals));
	for(cb_tree l = p->list; l; l = CB_CHAIN(l)) {
		cb_tree x = CB_VALUE(l);
		if(CB_PAIR_P(x)) {
			int lower = literal_value(CB_PAIR_X(x));
			int upper = literal_value(CB_PAIR_Y(x));
			for(int n = lower; n <= upper; ++n) {
				vals[n] = 1;
			}
		} else {
			if(CB_NUMERIC_LITERAL_P(x)) {
				vals[literal_value(x)] = 1;
			} else if(x == cb_space) {
				vals[' '] = 1;
			} else if(x == cb_zero) {
				vals['0'] = 1;
			} else if(x == cb_quote) {
				if(cb_flag_apostrophe) {
					vals['\''] = 1;
				} else {
					vals['"'] = 1;
				}
			} else if(x == cb_null) {
				vals[0] = 1;
			} else if(x == cb_low) {
				vals[0] = 1;
			} else if(x == cb_high) {
				vals[255] = 1;
			} else {
				size_t size = CB_LITERAL(x)->size;
				unsigned char * data = CB_LITERAL(x)->data;
				for(size_t i = 0; i < size; i++) {
					vals[data[i]] = 1;
				}
			}
		}
	}
	for(size_t i = 0; i < 256; ++i) {
		if(vals[i]) {
			str += output_line("case %d:", (int)i);
		}
	}
	str += output_line("    break;");
	str += output_line("default:");
	str += output_line("    return 0;");
	str += output_line("}");
	str += output_indent("}");
	str += output_line("return 1;");
	str += output_indent("}");
	str += output_newline();
	return str;
}

static string
output_class_names(cb_program * prog)
{
	string str("");
	if(!prog->nested_level && prog->class_name_list) {
		str += output("/* Class names */\n");
		for(cb_tree l = prog->class_name_list; l; l = CB_CHAIN(l)) {
			str += output_class_name_definition(CB_CLASS_NAME(CB_VALUE(l)));
		}
	}
	return str;
}

static string
output_initial_values(cb_field * f)
{
	string str("");
	for(cb_field * p0 = f; p0; p0 = p0->sister) {
		cb_field * p = p0;
		if(p->usage == CB_USAGE_UNION) {
			p = p->children;
		}
		cb_tree x = cb_build_field_reference(p, NULL);
		if(p->flag_item_based) {
			continue;
		}
		/* For special registers */
		if(p->flag_no_init && !p->count) {
			continue;
		}
		str += output_stmt(cb_build_initialize(x, cb_true, NULL, 1, 0, 0));
	}
	return str;
}

static string
output_error_handler(cb_program * prog)
{
	string str = output_newline();
	bool seen = false;
	for(int i = COB_OPEN_INPUT; i <= COB_OPEN_EXTEND; i++) {
		if(prog->global_handler[i].handler_label) {
			seen = true;
			break;
		}
	}
	str += output_stmt(cb_standard_error_handler);
	str += output_newline();
	if(seen) {
		str += output_line("switch(cob_glob_ptr->cob_error_file->last_open_mode)");
		str += output_indent("{");
		for(int i = COB_OPEN_INPUT; i <= COB_OPEN_EXTEND; i++) {
			handler_struct * hstr = &prog->global_handler[i];
			if(hstr->handler_label) {
				str += output_line("case %d:", i);
				str += output_indent("{");
				if(prog == hstr->handler_prog) {
					str += output_perform_call(hstr->handler_label,
											   hstr->handler_label);
				} else {
					str += output_prefix();
					if(hstr->handler_prog->nested_level) {
						str += output("%s_%d_(%d",
									  hstr->handler_prog->program_id,
									  hstr->handler_prog->toplev_count,
									  hstr->handler_label->id);
					} else {
						str += output("%s_(%d",
									  hstr->handler_prog->program_id,
									  hstr->handler_label->id);
					}
					int parmnum = cb_list_length(hstr->handler_prog->parameter_list);
					for(int n = 0; n < parmnum; n++) {
						str += output(", NULL");
					}
					str += output(");\n");
				}
				str += output_line("break;");
				str += output_indent("}");
			}
		}
		str += output_line("default:");
		str += output_indent("{");
	}
	str += output_line("if(!(cob_glob_ptr->cob_error_file->flag_select_features & COB_SELECT_FILE_STATUS)) {");
	str += output_line("\tcob_fatal_error(COB_FERROR_FILE);");
	str += output_line("}");
	if(seen) {
		str += output_line("break;");
		str += output_indent("}");
		str += output_indent("}");
	}
	str += output_perform_exit(CB_LABEL(cb_standard_error_handler));
	str += output_newline();
	str += output_line("/* Fatal error if reached */");
	str += output_line("cob_fatal_error(COB_FERROR_CODEGEN);");
	str += output_newline();
	return str;
}

static string
output_module_init(cb_program * prog)
{
	/* Do not initialize next pointer, parameter list pointer + count */
	string str = output_line("/* Initialize module structure */");
	str += output_line("module->module_name = \"%s\";", prog->orig_program_id);
	str += output_line("module->module_formatted_date = COB_MODULE_FORMATTED_DATE;");
	str += output_line("module->module_source = COB_SOURCE_FILE;");
	if(!prog->nested_level) {
		str += output_line("module->module_entry.funcptr = (fvoidpptr)%s;",
						   prog->program_id);
		if(prog->prog_type == CB_FUNCTION_TYPE) {
			str += output_line("module->module_cancel.funcptr = NULL;");
		} else {
			str += output_line("module->module_cancel.funcptr = (fvoidpptr)%s_;",
							   prog->program_id);
		}
	} else {
		str += output_line("module->module_entry.funcvoid = NULL;");
		str += output_line("module->module_cancel.funcvoid = NULL;");
	}

	if(prog->collating_sequence) {
		str += output_prefix();
		str += output("module->collating_sequence = ");
		str += output_param(cb_ref(prog->collating_sequence), -1);
		str += output(";\n");
	} else {
		str += output_line("module->collating_sequence = NULL;");
	}
	if(prog->crt_status && cb_code_field(prog->crt_status)->count) {
		str += output_prefix();
		str += output("module->crt_status = ");
		str += output_param(cb_ref(prog->crt_status), -1);
		str += output(";\n");
	} else {
		str += output_line("module->crt_status = NULL;");
	}
	if(prog->cursor_pos) {
		str += output_prefix();
		str += output("module->cursor_pos = ");
		str += output_param(cb_ref(prog->cursor_pos), -1);
		str += output(";\n");
	} else {
		str += output_line("module->cursor_pos = NULL;");
	}
	if(!cobc_flag_main && non_nested_count > 1) {
		str += output_line("module->module_ref_count = &cob_reference_count;");
	} else {
		str += output_line("module->module_ref_count = NULL;");
	}
	str += output_line("module->module_path = &cob_module_path;");
	str += output_line("module->module_active = 0;");
	str += output_line("module->module_date = COB_MODULE_DATE;");
	str += output_line("module->module_time = COB_MODULE_TIME;");
	str += output_line("module->module_type = %d;", (int)prog->prog_type);
	str += output_line("module->module_param_cnt = %u;", prog->num_proc_params);
	str += output_line("module->module_returning = 0;");
	str += output_line("module->ebcdic_sign = %d;", cb_ebcdic_sign);
	str += output_line("module->decimal_point = '%c';", prog->decimal_point);
	str += output_line("module->currency_symbol = '%c';", prog->currency_symbol);
	str += output_line("module->numeric_separator = '%c';", prog->numeric_separator);
	str += output_line("module->flag_filename_mapping = %d;", cb_filename_mapping);
	str += output_line("module->flag_binary_truncate = %d;", cb_binary_truncate);
	str += output_line("module->flag_pretty_display = %d;", cb_pretty_display);
	str += output_line("module->flag_host_sign = %d;", cb_host_sign);
	str += output_line("module->flag_no_phys_canc = %d;", no_physical_cancel);
	str += output_line("module->flag_main = %d;", cobc_flag_main);
	str += output_line("module->flag_fold_call = %d;", cb_fold_call);
	str += output_line("module->flag_exit_program = 0;");
	str += output_newline();
	return str;
}

static string
output_internal_function(cb_program * prog, cb_tree parameter_list)
{
	string str("");

	/* Program function */
#if	0	/* RXWRXW USERFUNC */
	if(prog->prog_type == CB_FUNCTION_TYPE) {
		str += output("static cob_field *\n%s_(const int entry, cob_field **cob_parms",
					  prog->program_id);
#else
	if(prog->prog_type == CB_FUNCTION_TYPE) {
		str += output("static cob_field *\n%s_(const int entry",
					  prog->program_id);
#endif
	} else if(!prog->nested_level) {
		str += output("static int\n%s_(const int entry",
					  prog->program_id);
	} else {
		str += output("static int\n%s_%d_(const int entry",
					  prog->program_id, prog->toplev_count);
	}
	int parmnum = 0;
#if	0	/* RXWRXW USERFUNC */
	if(!prog->flag_chained && prog->prog_type != CB_FUNCTION_TYPE) {
#else
	if(!prog->flag_chained) {
#endif
		for(cb_tree l = parameter_list; l; l = CB_CHAIN(l)) {
			str += output(", ");
			if(parmnum && !(parmnum % 2)) {
				str += output("\n\t");
			}
			cb_field * f = cb_code_field(CB_VALUE(l));
			string fname = get_parm_name(f);
			if(f->parent) {
				f = f->parent;
			}
			if(f->children && f->bUseName) {
				str += output("%s *%s", f->sType, fname.c_str());
			} else {
				str += output("cob_u8_t *%s", fname.c_str());
			}
			parmnum++;
		}
	}
	str += output(")\n");
	str += output_indent("{");

	/* Program local variables */
	str += output_line("/* Program local variables */");
	str += output_line("#include \"%s\"", prog->local_include->local_name);
	str += output_newline();

	/* Alphabet-names */
	if(prog->alphabet_name_list) {
		for(cb_tree l = prog->alphabet_name_list; l; l = CB_CHAIN(l)) {
			output_alphabet_name_definition(CB_ALPHABET_NAME(CB_VALUE(l)));
		}
	}

	/* Module initialization indicator */
	output_local("/* Module initialization indicator */\n");
	output_local("static unsigned int\tinitialized = 0;\n\n");

	if(prog->flag_recursive) {
		output_local("/* Module structure pointer for recursive */\n");
		output_local("cob_module\t\t*module = NULL;\n\n");
	} else {
		output_local("/* Module structure pointer */\n");
		output_local("static cob_module\t*module = NULL;\n\n");
	}

	output_local("/* Global variable pointer */\n");
	output_local("cob_global\t\t*cob_glob_ptr;\n\n");

	/* Decimal structures */
	if(prog->decimal_index_max) {
		output_local("/* Decimal structures */\n");
		for(unsigned int i = 0; i < prog->decimal_index_max; i++) {
			output_local("cob_decimal * _d%d = NULL;\n", i);
		}
		output_local("\n");
	}

	/* External items */
	bool seen = false;
	for(cb_field * f0 = prog->working_storage; f0; f0 = f0->sister) {
		cb_field * f = f0;
		if(f->usage == CB_USAGE_UNION) {
			f = f->children;
		}
		if(f->flag_external) {
			if(f->flag_is_global) {
				base_list * bl = (base_list *) cobc_parse_malloc(sizeof(base_list));
				bl->f = f;
				bl->curr_prog = excp_current_program_id;
				bl->next = globext_cache;
				globext_cache = bl;
				continue;
			}
			if(!seen) {
				seen = true;
				output_local("/* EXTERNAL items */\n");
			}
			output_local("static unsigned char\t*%s%d = NULL;",
						 CB_PREFIX_BASE, f->id);
			output_local("  /* %s */\n", f->name);
		}
	}
	if(seen) {
		output_local("\n");
	}
	for(cb_tree l = prog->file_list; l; l = CB_CHAIN(l)) {
		cb_field * f = CB_FILE(CB_VALUE(l))->record;
		if(f->usage == CB_USAGE_UNION) {
			f = f->children;
		}
		if(f->flag_external) {
			if(f->flag_is_global) {
				base_list * bl = (base_list *) cobc_parse_malloc(sizeof(base_list));
				bl->f = f;
				bl->curr_prog = excp_current_program_id;
				bl->next = globext_cache;
				globext_cache = bl;
				continue;
			}
			output_local("static unsigned char\t*%s%d = NULL;",
						 CB_PREFIX_BASE, f->id);
			output_local("  /* %s */\n", f->name);
		}
	}

	/* Allocate files */
	if(prog->file_list) {
		int i = 0;
		for(cb_tree l = prog->file_list; l; l = CB_CHAIN(l)) {
			i += output_file_allocation(CB_FILE(CB_VALUE(l)));
		}
		if(i) {
			output_local("\n/* LINAGE pointer */\n");
			output_local("static cob_linage\t\t*lingptr;\n");
		}
	}

	/* BASED working-storage */
	seen = false;
	for(cb_field * f0 = prog->working_storage; f0; f0 = f0->sister) {
		cb_field * f = f0;
		if(f->usage == CB_USAGE_UNION) {
			f = f->children;
		}
		if(f->redefines) {
			continue;
		}
		if(f->flag_item_based) {
			if(!seen) {
				seen = true;
				output_local("\n/* BASED WORKING-STORAGE SECTION */\n");
			}
			if(!f->bUseName) {
				output_local("static cob_u8_t\t*%s%d = NULL;\t// %s\n",
							 CB_PREFIX_BASE, f->id, f->name);
			} else {
				if(f->parent) {
					f = f->parent;
				}
				if(!f->children) {
					if(f->flag_occurs) {
						f->bUseName = false;
						output_local("static cob_u8_t\t*%s%d = NULL;\t// %s\n",
									 CB_PREFIX_BASE, f->id, f->name);
					} else {
						output_local("static cob_u8_t\t*%s = NULL;\t// %s\n",
									 f->sName, f->name);
					}
				} else {
					if(!f->bOutDone) {
						string st = output_fake_dcl(f);
						output_storage("%s", st.c_str());
						f->bOutDone = true;
					}
					output_local("static %s\t*%s = NULL;\t// %s\n",
								 f->sType, f->sName, f->name);
				}
			}
		}
	}
	if(seen) {
		output_local("\n");
	}

	/* BASED local-storage */
	seen = false;
	for(cb_field * f0 = prog->local_storage; f0; f0 = f0->sister) {
		cb_field * f = f0;
		if(f->usage == CB_USAGE_UNION) {
			f = f->children;
		}
		if(f->redefines) {
			continue;
		}
		if(f->flag_item_based) {
			if(!seen) {
				seen = true;
				output_local("\n/* BASED LOCAL-STORAGE */\n");
			}
			// it should not be static as it is local-storage.
			if(!f->bUseName) {
				output_local("cob_u8_t\t*%s%d = NULL; // %s\n",
							 CB_PREFIX_BASE, f->id, f->name);
			} else {
				if(f->parent) {
					f = f->parent;
				}
				if(!f->children) {
					if(f->flag_occurs) {
						f->bUseName = false;
						output_local("cob_u8_t\t*%s%d = NULL; // %s\n",
									 CB_PREFIX_BASE, f->id, f->name);
					} else {
						output_local("cob_u8_t\t*%s = NULL; // %s\n",
									 f->sName, f->name);
					}
				} else {
					if(!f->bOutDone) {
						string st = output_fake_dcl(f);
						output_storage("%s", st.c_str());
						f->bOutDone = true;
					}
					output_local("%s\t*%s = NULL;\t// %s\n",
								 f->sType, f->sName, f->name);
					f->bPointer = true;
				}
			}
		}
	}
	if(seen) {
		output_local("\n");
	}

#if	0	/* RXWRXW USERFUNC */
	if(prog->prog_type == CB_FUNCTION_TYPE) {
		/* USING parameters for user FUNCTION */
		seen = false;
		for(cb_tree l = parameter_list; l; l = CB_CHAIN(l)) {
			cb_field * f = cb_code_field(CB_VALUE(l));
			if(!seen) {
				seen = true;
				output_local("\n/* USING parameters */\n");
			}
			output_local("unsigned char\t*%s%d = NULL;  /* %s */\n",
						 CB_PREFIX_BASE, f->id, f->name);
		}
		if(seen) {
			output_local("\n");
		}
	}
#endif

	/* Dangling linkage section items */
	seen = false;
	for(cb_field * f0 = prog->linkage_storage; f0; f0 = f0->sister) {
		cb_field * f = f0;
		if(f->usage == CB_USAGE_UNION) {
			f = f->children;
		}
		if(f->redefines) {
			continue;
		}
		cb_tree  l;
		for(l = parameter_list; l; l = CB_CHAIN(l)) {
			if(f == cb_code_field(CB_VALUE(l))) {
				break;
			}
		}
		if(l == NULL) {
			if(!seen) {
				seen = true;
				output_local("\n/* LINKAGE SECTION(Items not referenced by USING clause) */\n");
			}
			if(!f->bUseName) {
				if(f->flag_is_returning) {
					output_local("cob_s64_t\tret_%s%d = 0;\t// %s\n", CB_PREFIX_BASE, f->id, f->name);
					output_local("cob_u8_t\t*%s%d = (unsigned char *)&ret_%s%d;\t// %s\n", CB_PREFIX_BASE, f->id, CB_PREFIX_BASE, f->id, f->name);
				} else {
					output_local("static cob_u8_t\t*%s%d = NULL;\t// %s\n", CB_PREFIX_BASE, f->id, f->name);
				}
			} else {
				if(f->parent) {
					f = f->parent;
				}
				if(!f->children) {
					if(f->flag_is_returning) {
						output_local("cob_s64_t\tret_%s = 0;\t// %s\n", f->sName, f->name);
						output_local("cob_u8_t\t*%s = (unsigned char *)&ret_%s;\t// %s\n", f->sName, f->sName, f->name);
					} else {
						if(f->flag_occurs) {
							f->bUseName = false;
							output_local("static cob_u8_t\t*%s%d = NULL;\t// %s\n", CB_PREFIX_BASE, f->id, f->name);
						} else {
							output_local("static cob_u8_t\t*%s = NULL;\t// %s\n", f->sName, f->name);
						}
					}
				} else {
					string st = output_fake_dcl(f);
					output_storage("%s", st.c_str());
					output_local("static %s\t*%s = NULL;\t// %s\n", f->sType, f->sName, f->name);
				}
			}
		}
	}
	if(seen) {
		output_local("\n");
	}

	/* Screens */
	if(prog->screen_storage) {
		optimize_defs[COB_SET_SCREEN] = 1;
		output_local("\n/* Screens */\n\n");
		output_screen_definition(prog->screen_storage);
		output_local("\n");
	}

	/* ANY LENGTH items */
	bool anyseen = false;

	for(cb_tree l = parameter_list; l; l = CB_CHAIN(l)) {
		cb_field * f = cb_code_field(CB_VALUE(l));
		if(f->flag_any_length) {
			anyseen = true;
#if	0	/* RXWRXW - Any */
			output_local("/* ANY LENGTH variable */\n");
			output_local("cob_field\t\t*cob_anylen;\n\n");
#endif
			break;
		}
	}

	/* Save variables for global callback */
	if(prog->flag_global_use && parameter_list) {
		output_local("/* Parameter save */\n");
		for(cb_tree l = parameter_list; l; l = CB_CHAIN(l)) {
			cb_field * f = cb_code_field(CB_VALUE(l));
			output_local("static unsigned char\t*save_%s%d;\n",
						 CB_PREFIX_BASE, f->id);
		}
		output_local("\n");
	}

	/* Runtime DEBUGGING MODE variable */
	if(prog->flag_debugging) {
		str += output_line("char\t\t*s;");
		str += output_newline();
	}

	/* Start of function proper */
	str += output_line("/* Start of function code */");
	str += output_newline();

	/* CANCEL callback */
	if(prog->prog_type == CB_PROGRAM_TYPE) {
		str += output_line("/* CANCEL callback */");
		str += output_line("if(unlikely(entry < 0)) {");
		str += output_line("\tif(entry == -20)");
		str += output_line("\t\tgoto P_clear_decimal;");
		str += output_line("\tgoto P_cancel;");
		str += output_line("}");
		str += output_newline();
	}

	str += output_line("/* Check initialized, check module allocated, */");
	str += output_line("/* set global pointer, */");
	str += output_line("/* push module stack, save call parameter count */");
#if 0 /* cob_call_name_hash and cob_call_from_c are rw-branch only features
         for now - TODO: activate on merge of r1547 */
	str += output_line("if (cob_module_global_enter (&module, &cob_glob_ptr, %d, entry, %sname_hash))",
					   cb_flag_implicit_init, CB_PREFIX_STRING);
#else
	str += output_line("if (cob_module_global_enter (&module, &cob_glob_ptr, %d, entry, 0))",
					   cb_flag_implicit_init);
#endif
	if(prog->prog_type == CB_FUNCTION_TYPE) {
		str += output_line("\treturn NULL;");
	} else {
		str += output_line("\treturn -1;");
	}
	str += output_newline();

	if(prog->flag_chained) {
		str += output_line("/* Check program with CHAINING being main program */");
		str += output_line("if(cob_glob_ptr->cob_current_module->next) {");
		str += output_line("\tcob_fatal_error(COB_FERROR_CHAINING);");
		str += output_line("}");
		str += output_newline();
	}

	/* Recursive module initialization */
	if(prog->flag_recursive) {
		str += output_module_init(prog);
	}

	/* Module Parameters */
	str += output_line("/* Set address of module parameter list */");
	if(cb_flag_stack_on_heap || prog->flag_recursive) {
		int i = prog->max_call_param ? prog->max_call_param : 1;
		str += output_line("cob_procedure_params = (cob_field **) cob_malloc(%dU * sizeof(void *));", i);
	}
	str += output_line("module->cob_procedure_params = cob_procedure_params;");
	str += output_newline();

#if	0	/* RXWRXW USERFUNC */
	if(prog->prog_type == CB_FUNCTION_TYPE) {
		parmnum = 0;
		for(cb_tree l = parameter_list; l; l = CB_CHAIN(l), parmnum++) {
			cb_field * f = cb_code_field(CB_VALUE(l));
			str += output_line("if(cob_parms[%d])", parmnum);
			str += output_line(" %s%d = cob_parms[%d]->data;",
							   CB_PREFIX_BASE, f->id, parmnum);
			str += output_line("else");
			str += output_line(" %s%d = NULL;",
							   CB_PREFIX_BASE, f->id);
		}
		str += output_newline();
	}
#endif

	str += output_line("/* Set frame stack pointer */");
	if(cb_flag_stack_on_heap || prog->flag_recursive) {
		int i = (prog->flag_recursive && cb_stack_size == 255) ? 63 : cb_stack_size;
		str += output_line("_frame_stack = (cob_frame *) cob_malloc(%dU * sizeof(struct cob_frame));", i);
		str += output_line("_frame_ptr = _frame_stack;");
		if(cb_flag_stack_check) {
			str += output_line("_frame_overflow = _frame_ptr + %d - 1;", i);
		}
	} else {
		str += output_line("_frame_ptr = _frame_stack;");
		str += output_line("_frame_ptr->perform_through = 0;");
		if(cb_flag_stack_check) {
			str += output_line("_frame_overflow = _frame_ptr + %d - 1;",
							   cb_stack_size);
		}
	}
	str += output_newline();

	/* Set up LOCAL-STORAGE size */
	if(prog->local_storage) {
		for(cb_field * f0 = prog->local_storage; f0; f0 = f0->sister) {
			cb_field * f = f0;
			if(f->usage == CB_USAGE_UNION) {
				f = f->children;
			}
			if(f->flag_item_based || f->flag_local_alloced) {
				continue;
			}
			if(f->redefines) {
				continue;
			}
			if(f->flag_item_78) {
				/* LCOV_EXCL_START */
				cobc_err_msg(_("unexpected CONSTANT item"));
				COBC_ABORT();
				/* LCOV_EXCL_STOP */
			}
			f->flag_local_storage = 1;
			f->flag_local_alloced = 1;
			f->mem_offset = local_mem;
			// Correct possible union
			if(f0 != f) {	// it was a union
				f0->flag_local_storage = 1;
				f0->flag_local_alloced = 1;
				f0->mem_offset = local_mem;
				f0->memory_size = f->memory_size;
			}
			// Output structs
			if(f0->children && f0->bUseName) {
				string st = output_fake_dcl(f0);
				output_storage("%s", st.c_str());
				f0->bPointer = true;
			} else {
				f0->bUseName = false; // we can't use names of the simple vars in local.
			}
			// Round up to COB_MALLOC_ALIGN + 1 bytes
			// Caters for current types
			local_mem += ((f->memory_size + COB_MALLOC_ALIGN) &
						  ~COB_MALLOC_ALIGN);
		}
		/* Allocate / initialize LOCAL storage */
		if(local_mem) {
			str += output_line("/* Allocate LOCAL storage */");
			str += output_line("cob_local_ptr = (unsigned char *) cob_malloc(%dU);",
							   local_mem);
			if(prog->flag_global_use) {
				str += output_line("cob_local_save = cob_local_ptr;");
			}
		}
		str += output_newline();
		str += output_line("/* Initialize LOCAL storage */");
		str += output_initial_values(prog->local_storage);
		str += output_newline();
	}

	/* Initialization */
	str += output_line("/* Initialize program */");
	str += output_line("if(unlikely(initialized == 0)) {");
	str += output_line("\tgoto P_initialize;");
	str += output_line("}");
	str += output_line("P_ret_initialize:");
	str += output_newline();

	if(prog->decimal_index_max) {
		str += output_line("/* Allocate decimal numbers */");
		str += output_prefix();
		if(prog->flag_recursive) {
			str += output("cob_decimal_push(%u", prog->decimal_index_max);
		} else {
			str += output("cob_decimal_alloc(%u", prog->decimal_index_max);
		}
		for(unsigned int i = 0; i < prog->decimal_index_max; i++) {
			str += output(", &_d%u", i);
		}
		str += output(");\n");
		str += output_newline();
	}

	/* Global entry dispatch */
	if(prog->global_list) {
		str += output_line("/* Global entry dispatch */");
		str += output_newline();
		for(cb_tree l = prog->global_list; l; l = CB_CHAIN(l)) {
			str += output_line("if(unlikely(entry == %d)) {",
							   CB_LABEL(CB_VALUE(l))->id);
			if(local_mem) {
				str += output_line("\tcob_local_ptr = cob_local_save;");
			}
			for(cb_tree l2 = parameter_list; l2; l2 = CB_CHAIN(l2)) {
				cb_field * f = cb_code_field(CB_VALUE(l2));
				str += output_line("\t%s%d = save_%s%d;",
								   CB_PREFIX_BASE, f->id,
								   CB_PREFIX_BASE, f->id);
			}
			str += output_line("\tgoto %s%d;",
							   CB_PREFIX_LABEL,
							   CB_LABEL(CB_VALUE(l))->id);
			str += output_line("}");
		}
		str += output_newline();
	}

	if(!prog->flag_recursive) {
		str += output_line("/* Increment module active */");
		str += output_line("module->module_active++;");
		str += output_newline();
	}

	if(!cobc_flag_main && non_nested_count > 1) {
		str += output_line("/* Increment module reference count */");
		str += output_line("cob_reference_count++;");
		str += output_newline();
	}

	/* Initialize W/S and files unconditionally when INITIAL program */
	if(prog->flag_initial) {
		if(prog->working_storage) {
			str += output_line("/* Initalize INITIAL program WORKING-STORAGE */");
			str += output_initial_values(prog->working_storage);
			str += output_newline();
		}
		for(cb_tree l = prog->file_list; l; l = CB_CHAIN(l)) {
			str += output_file_initialization(CB_FILE(CB_VALUE(l)));
		}
		str += output_newline();
	}

	/* Call parameters */
	if(prog->cb_call_params && cb_code_field(prog->cb_call_params)->count) {
		str += output_line("/* Set NUMBER-OF-CALL-PARAMETERS (independent from LINKAGE) */");
		str += output_prefix();
		str += output_integer(prog->cb_call_params);
		str += output(" = cob_glob_ptr->cob_call_params;\n");
		str += output_newline();
	}

#if	0	/* RXWRXW - Save call params */
	str += output_line("/* Save number of call params */");
	str += output_line("module->module_num_params = cob_glob_ptr->cob_call_params;");
	str += output_newline();
#endif
	if(!cb_sticky_linkage && !prog->flag_chained
#if	0	/* RXWRXW USERFUNC */
			&& prog->prog_type != CB_FUNCTION_TYPE
#endif
			&& prog->num_proc_params) {
		str += output_line("/* Set not passed parameter pointers to NULL */");
		str += output_line("switch(cob_call_params) {");
		int i = 0;
		for(cb_tree l = parameter_list; l; l = CB_CHAIN(l)) {
			cb_field * f = cb_code_field(CB_VALUE(l));
			string fname = get_parm_name(f);
			str += output_line("case %d:", i++);
			str += output_line("\t%s = NULL;", fname.c_str());
		}
		str += output_line("default:\n\tbreak;");
		str += output_line("}");
		str += output_newline();
	}

	/* Set up ANY length items */
	if(anyseen) {
		str += output_line("/* Initialize ANY LENGTH parameters */");
		int i = 0;
		for(cb_tree l = parameter_list; l; l = CB_CHAIN(l), i++) {
			cb_field * f = cb_code_field(CB_VALUE(l));
			if(f->flag_any_length) {
				/* Force field cache */
				FILE * savetarget = output_target;
				output_target = NULL;
				str += output_param(CB_VALUE(l), i);
				output_target = savetarget;

				str += output_line("if(cob_call_params > %d && %s%d%s)",
								   i, "module->next->cob_procedure_params[",
								   i, "]");
				if(f->flag_any_numeric) {
					/* Copy complete structure */
					str += output_line("  %s%d = *(%s%d%s);",
									   CB_PREFIX_FIELD, f->id,
									   "module->next->cob_procedure_params[",
									   i, "]");
				} else {
					/* Copy size */
					str += output_line("  %s%d.size = %s%d%s;",
									   CB_PREFIX_FIELD, f->id,
									   "module->next->cob_procedure_params[",
									   i, "]->size");
				}
				str += output_prefix();
				str += output("%s%d.data = ", CB_PREFIX_FIELD, f->id);
				str += output_data(CB_VALUE(l));
				str += output(";\n");
#if	0	/* RXWRXW - Num check */
				if(CB_EXCEPTION_ENABLE(COB_EC_DATA_INCOMPATIBLE) &&
						f->flag_any_numeric &&
						(f->usage == CB_USAGE_DISPLAY ||
						 f->usage == CB_USAGE_PACKED ||
						 f->usage == CB_USAGE_COMP_6)) {
					str += output_line("cob_check_numeric(&%s%d, %s%d);",
									   CB_PREFIX_FIELD
									   f->id,
									   CB_PREFIX_STRING,
									   lookup_string(f->name));
				}
#endif
			}
		}
		str += output_newline();
	}

	if(prog->prog_type == CB_FUNCTION_TYPE &&
			CB_FIELD_PTR(prog->returning)->storage == CB_STORAGE_LINKAGE) {
		str += output_line("/* Storage for returning item */");
		str += output_prefix();
		str += output("*((void**)& ");
		str += output_data(prog->returning);
		str += output(") = cob_malloc(");
		str += output_size(prog->returning);
		str += output("U);\n\n");
	}

	if(prog->flag_global_use && parameter_list) {
		str += output_line("/* Parameter save */");
		for(cb_tree l = parameter_list; l; l = CB_CHAIN(l)) {
			cb_field * f = cb_code_field(CB_VALUE(l));
			string fname = get_parm_name(f);
			str += output_line("save_%s = %s;", fname.c_str(), fname.c_str());
		}
		str += output_newline();
	}

	/* Classification */
	if(prog->classification) {
		if(prog->classification == cb_int1) {
			str += output_line("cob_set_locale(NULL, COB_LC_CLASS);");
		} else {
			str += output_prefix();
			str += output("cob_set_locale(");
			str += output_param(prog->classification, -1);
			str += output(", COB_LC_CTYPE);");
		}
		str += output_newline();
	}

	/* Entry dispatch */
	str += output_line("/* Entry dispatch */");
	if(cb_list_length(prog->entry_list) > 1) {
		str += output_newline();
		str += output_line("switch(entry)");
		str += output_line("  {");
		int i = 0;
		for(cb_tree l = prog->entry_list; l; l = CB_CHAIN(l)) {
			str += output_line("  case %d:", i++);
			str += output_line("    goto %s%d;", CB_PREFIX_LABEL,
							   CB_LABEL(CB_PURPOSE(l))->id);
		}
		str += output_line("  }");
		str += output_line("/* This should never be reached */");
		str += output_line("cob_fatal_error(COB_FERROR_MODULE);");
		str += output_newline();
	} else {
		cb_tree l = prog->entry_list;
		str += output_line("goto %s%d;", CB_PREFIX_LABEL,
						   CB_LABEL(CB_PURPOSE(l))->id);
		str += output_newline();
	}

	/* PROCEDURE DIVISION */
	str += output_line("/* PROCEDURE DIVISION */");
	for(cb_tree l = prog->exec_list; l; l = CB_CHAIN(l)) {
		str += output_stmt(CB_VALUE(l));
	}
	str += output_newline();

	/* End of program */
	str += output_line("/* Program exit */");
	str += output_newline();

	if(needs_exit_prog) {
		str += output_line("exit_program:");
		str += output_newline();
	}

	if(!prog->flag_recursive) {
		str += output_line("/* Decrement module active count */");
		str += output_line("if(module->module_active) {");
		str += output_line("\tmodule->module_active--;");
		str += output_line("}");
		str += output_newline();
	}

	if(!cobc_flag_main && non_nested_count > 1) {
		str += output_line("/* Decrement module reference count */");
		str += output_line("if(cob_reference_count) {");
		str += output_line("\tcob_reference_count--;");
		str += output_line("}");
		str += output_newline();
	}

	if(gen_dynamic) {
		str += output_line("/* Deallocate dynamic FUNCTION-ID fields */");
		for(cob_u32_t inc = 0; inc < gen_dynamic; inc++) {
			str += output_line("if(cob_dyn_%u) {", inc);
			str += output_line("  if(cob_dyn_%u->data) {", inc);
			str += output_line("    cob_free(cob_dyn_%u->data);", inc);
			str += output_line("  }");
			str += output_line("  cob_free(cob_dyn_%u);", inc);
			str += output_line("  cob_dyn_%u = NULL;", inc);
			str += output_line("}");
		}
		str += output_newline();
	}

	if(prog->local_storage) {
		str += output_line("/* Deallocate LOCAL storage */");
		if(local_mem) {
			str += output_line("if(cob_local_ptr) {");
			str += output_line("\tcob_free(cob_local_ptr);");
			str += output_line("\tcob_local_ptr = NULL;");
			if(current_prog->flag_global_use) {
				str += output_line("\tcob_local_save = NULL;");
			}
			str += output_line("}");
		}
		for(cb_field * f = prog->local_storage; f; f = f->sister) {
			if(f->flag_item_based) {
				string fname = get_parm_name(f);
				str += output_line("if(%s) {", fname.c_str());
				str += output_line("\tcob_free_alloc(&*(cob_u8_t **)&%s, NULL);", fname.c_str());
				str += output_line("\t%s = NULL;", fname.c_str());
				str += output_line("}");
			}
		}
		str += output_newline();
	}

	if(prog->decimal_index_max && prog->flag_recursive) {
		str += output_line("/* Free decimal structures */");
		str += output_prefix();
		str += output("cob_decimal_pop(%u", prog->decimal_index_max);
		for(unsigned int i = 0; i < prog->decimal_index_max; i++) {
			str += output(", _d%u", i);
		}
		str += output(");\n");
		str += output_newline();
	}

	if(cb_flag_stack_on_heap || prog->flag_recursive) {
		str += output_line("/* Free frame stack / call parameters */");
		str += output_line("cob_free(_frame_stack);");
		str += output_line("cob_free(cob_procedure_params);");
		str += output_newline();
	}

	if(cb_flag_trace) {
		str += output_line("/* Trace program exit */");
		sprintf(string_buffer, "Exit:      %s", excp_current_program_id);
		str += output_line("cob_trace_section(%s%d, NULL, 0);",
						   CB_PREFIX_STRING,
						   lookup_string(string_buffer));
		str += output_newline();
	}

	str += output_line("/* Pop module stack */");
	str += output_line("cob_module_leave(module);");
	str += output_newline();

	if(prog->flag_recursive) {
		str += output_line("/* Free for recursive module */");
		str += output_line("cob_module_free(&module);");
		str += output_newline();
	}

	/* Implicit CANCEL for INITIAL program */
	if(prog->flag_initial) {
		str += output_line("/* CANCEL for INITIAL program */");
		str += output_prefix();
		if(!prog->nested_level) {
			str += output("%s_(-1", prog->program_id);
		} else {
			str += output("%s_%d_(-1", prog->program_id,
						  prog->toplev_count);
		}
		if(!prog->flag_chained) {
			for(cb_tree l = parameter_list; l; l = CB_CHAIN(l)) {
				str += output(", NULL");
			}
		}
		str += output(");\n");
		str += output_newline();
	}

	str += output_line("/* Program return */");
	if(prog->returning && prog->cb_return_code) {
		str += output_move(prog->returning, prog->cb_return_code);
	}
	str += output_prefix();
	str += output("return ");
	if(prog->prog_type == CB_FUNCTION_TYPE) {
		str += output_param(prog->returning, -1);
	} else if(prog->cb_return_code) {
		str += output_integer(prog->cb_return_code);
	} else {
		str += output("0");
	}
	str += output(";\n");

	/* Error handlers */
	if(prog->file_list || prog->flag_gen_error) {
		str += output_error_handler(prog);
	}

	/* Frame stack jump table for compiler without computed goto */
	if(!cb_flag_computed_goto) {
		str += output_newline();
		str += output_line("/* Frame stack jump table */");
		str += output_line("P_switch:");
		if(label_cache) {
			str += output_line(" switch(_frame_ptr->return_address_num) {");
			for(label_list * pl = label_cache; pl; pl = pl->next) {
				str += output_line(" case %d:", pl->call_num);
				str += output_line("   goto %s%d;", CB_PREFIX_LABEL, pl->id);
			}
			str += output_line(" }");
		}
		str += output_line(" cob_fatal_error(COB_FERROR_CODEGEN);");
		str += output_newline();
	}

	/* Program initialization */

#if	0	/* RXWRXW WS */
	if(prog->working_storage) {
		for(f = prog->working_storage; f; f = f->sister) {
			if(f->flag_item_based || f->flag_local_alloced) {
				continue;
			}
			if(f->redefines || f->flag_external) {
				continue;
			}
#if	0	/* RXWRXW - Check global */
			if(f->flag_is_global) {
				continue;
			}
#endif
			if(f->flag_no_init && !f->count) {
				continue;
			}
			if(f->flag_item_78) {
				/* LCOV_EXCL_START */
				cobc_err_msg(_("unexpected CONSTANT item"));
				COBC_ABORT();
				/* LCOV_EXCL_STOP */
			}
			if(f->flag_is_global) {
				f->mem_offset = working_mem;
				working_mem += ((f->memory_size + COB_MALLOC_ALIGN) &
								~COB_MALLOC_ALIGN);
			} else {
				f->mem_offset = local_working_mem;
				local_working_mem += ((f->memory_size + COB_MALLOC_ALIGN) &
									  ~COB_MALLOC_ALIGN);
			}
		}
	}
#endif

	str += output_newline();
	str += output_line("/* Program initialization */");
	str += output_line("P_initialize:");
	str += output_newline();

	/* Check matching version */
	if(!prog->nested_level) {
		str += output_line("cob_check_version(COB_SOURCE_FILE, COB_PACKAGE_VERSION, COB_PATCH_LEVEL);");
		str += output_newline();
	}

	/* Resolve user functions */
	for(call_list * clp = func_call_cache; clp; clp = clp->next) {
		str += output_line("func_%s.funcvoid = cob_resolve_func(\"%s\");",
						   clp->callname, clp->callname);
	}

	if(cobc_flag_main && !prog->nested_level) {
		str += output_line("cob_module_path = cob_glob_ptr->cob_main_argv0;");
		str += output_newline();
	}

	/* Module initialization */
	if(!prog->flag_recursive) {
		str += output_module_init(prog);
	}


	/* Check runtime DEBUGGING MODE variable */
	if(prog->flag_debugging) {
		str += output_line("if((s = getenv(\"COB_SET_DEBUG\")) &&(*s == 'Y' || *s == 'y' || *s == '1'))");
		str += output_line("\tcob_debugging_mode = 1;");
		str += output_newline();
	}

	/* Setup up CANCEL callback */
	if(!prog->nested_level && prog->prog_type == CB_PROGRAM_TYPE) {
		str += output_line("/* Initialize cancel callback */");
#if	0	/* RXWRXW CA */
		if(!cb_flag_implicit_init) {
			str += output_line("if(module->next)");
		}
#endif
		str += output_line("cob_set_cancel(module);");
		str += output_newline();
	}

	/* Initialize EXTERNAL files */
	for(cb_tree l = prog->file_list; l; l = CB_CHAIN(l)) {
		cb_field * f = CB_FILE(CB_VALUE(l))->record;
		if(f->flag_external) {
			strcpy(string_buffer, f->name);
			for(char * p = string_buffer; *p; p++) {
				if(*p == '-' || *p == ' ') {
					*p = '_';
				}
			}
			str += output_line("%s%d = (unsigned char *) cob_external_addr(\"%s\", %d);",
							   CB_PREFIX_BASE, f->id, string_buffer,
							   CB_FILE(CB_VALUE(l))->record_max);
		}
	}

	/* Initialize WORKING-STORAGE EXTERNAL items */
	for(cb_field * f0 = prog->working_storage; f0; f0 = f0->sister) {
		cb_field * f = f0;
		if(f->usage == CB_USAGE_UNION) {
			f = f->children;
		}
		if(f->redefines) {
			continue;
		}
		if(!f->flag_external) {
			continue;
		}
		str += output_prefix();
		str += output_base(f, 0);
		str += output(" = (unsigned char *) cob_external_addr(\"%s\", %d);\n",
					  f->ename, f->size);
	}

	/* Initialize WORKING-STORAGE/files if not INITIAL program */
	if(!prog->flag_initial) {
		if(prog->working_storage) {
			str += output_line("/* Initialize WORKING-STORAGE */");
			str += output_initial_values(prog->working_storage);
			str += output_newline();
		}
		if(prog->file_list) {
			str += output_newline();
			for(cb_tree l = prog->file_list; l; l = CB_CHAIN(l)) {
				str += output_file_initialization(CB_FILE(CB_VALUE(l)));
			}
			str += output_newline();
		}
	}
	bool bFirst = true;
	for(literal_list * m = literal_cache; m; m = m->next) {
		if(CB_TREE_CLASS(m->x) == CB_CLASS_NUMERIC
				&& m->make_decimal) {
			if(bFirst) {
				bFirst = false;
				str += output_line("/* Set Decimal Constant values */");
			}
			str += output_line("%s%d = &%s%d;", 	CB_PREFIX_DEC_CONST, m->id,
								CB_PREFIX_DEC_FIELD, m->id);
			str += output_line("cob_decimal_init(%s%d);", CB_PREFIX_DEC_CONST, m->id);
			str += output_line("cob_decimal_set_field (%s%d, (cob_field *)&%s%d);",
								CB_PREFIX_DEC_CONST, m->id,
								CB_PREFIX_CONST, m->id);
			str += output_newline();
		}
	}

	if(prog->screen_storage) {
		str += output_line("/* Initialize SCREEN items */");
		/* Initialize items with VALUE */
		str += output_initial_values(prog->screen_storage);
		str += output_screen_init(prog->screen_storage, NULL);
		str += output_newline();
	}

	str += output_line("initialized = 1;");
	str += output_line("goto P_ret_initialize;");

	/* Set up CANCEL callback code */

	if(prog->prog_type != CB_PROGRAM_TYPE) {
		goto prog_cancel_end;
	}

	str += output_newline();
	str += output_line("/* CANCEL callback handling */");
	str += output_line("P_cancel:");
	str += output_newline();
	str += output_line("if(!initialized) {");
	str += output_line("\treturn 0;");
	str += output_line("}");
	str += output_line("if(module->module_active) {");
	str += output_line("\tcob_fatal_error(COB_FERROR_CANCEL);");
	str += output_line("}");
	str += output_newline();

	if(prog->flag_main) {
		goto cancel_end;
	}

	/* Check for implicit cancel of contained programs */
	for(cb_program * next_prog = prog->next_program; next_prog; next_prog = next_prog->next_program) {
		if(next_prog->nested_level == prog->nested_level + 1) {
			str += output_prefix();
			str += output("(void)%s_%d_(-1", next_prog->program_id,
						  next_prog->toplev_count);
			for(unsigned int i = 0; i < next_prog->num_proc_params; ++i) {
				str += output(", NULL");
			}
			str += output(");\n");
		}
	}

	/* Close files on cancel */
	for(cb_tree l = prog->file_list; l; l = CB_CHAIN(l)) {
		cb_file * fl = CB_FILE(CB_VALUE(l));
		if(fl->organization != COB_ORG_SORT) {
			str += output_line("cob_close(%s%s, NULL, COB_CLOSE_NORMAL, 1);",
							   CB_PREFIX_FILE, fl->cname);
			if(!fl->flag_external) {
				char key_ptr[64];
				if(fl->organization == COB_ORG_RELATIVE
						|| fl->organization == COB_ORG_INDEXED) {
					sprintf(key_ptr, "&%s%s", CB_PREFIX_KEYS, fl->cname);
				} else {
					strcpy(key_ptr, "NULL");
				}
				output_line("cob_file_free(&%s%s, %s);", CB_PREFIX_FILE, fl->cname, key_ptr);

			}
		} else {
			str += output_line("cob_cache_free(%s%s);",
							   CB_PREFIX_FILE, fl->cname);
			str += output_line("%s%s = NULL;",
							   CB_PREFIX_FILE, fl->cname);
		}
	}

	/* Clear alter indicators */
	for(cb_alter_id * cpl = prog->alter_gotos; cpl; cpl = cpl->next) {
		str += output_line("label_%s%d = 0;",
						   CB_PREFIX_LABEL, cpl->goto_id);
		if(prog->flag_segments) {
			str += output_line("save_label_%s%d = 0;",
							   CB_PREFIX_LABEL, cpl->goto_id);
		}
	}

	/* Release based storage */
	for(cb_field * f = prog->working_storage; f; f = f->sister) {
		if(f->flag_item_based) {
			string fname = get_parm_name(f);
			str += output_line("if(%s) {", fname.c_str());
			str += output_line("\tcob_free_alloc(&*(cob_u8_t **)&%s, NULL);", fname.c_str());
			str += output_line("}");
		}
	}

	/* Reset DEBUGGING mode */
	if(prog->flag_debugging) {
		str += output_line("cob_debugging_mode = 0;");
	}

	/* Clear CALL pointers */
	for(call_list * clp = call_cache; clp; clp = clp->next) {
		str += output_line("call_%s.funcvoid = NULL;", clp->callname);
	}
	for(call_list * clp = func_call_cache; clp; clp = clp->next) {
		str += output_line("func_%s.funcvoid = NULL;", clp->callname);
	}

	/* Clear sticky-linkage pointers */
	if(cb_sticky_linkage) {
		for(cb_tree l = prog->parameter_list; l; l = CB_CHAIN(l)) {
			cb_field * f = cb_code_field(CB_VALUE(l));
			string fname = get_parm_name(f);
			str += output_line("cob_parm_%s = NULL;", fname.c_str());
		}
	}

	/* Clear RETURN-CODE */
	if(!prog->nested_level && prog->cb_return_code) {
		str += output_prefix();
		str += output_integer(current_prog->cb_return_code);
		str += output(" = 0;\n");
	}

	str += output_line("cob_module_free(&module);");
	str += output_newline();

cancel_end:
	str += output_line("initialized = 0;");
	str += output_newline();
	str += output_line("P_clear_decimal:");
	{
		bool bFirst = true;
		for(literal_list * m = literal_cache; m; m = m->next) {
			if(CB_TREE_CLASS(m->x) == CB_CLASS_NUMERIC
					&& m->make_decimal) {
				if(bFirst) {
					bFirst = false;
					str += output_line("/* Clear Decimal Constant values */");
				}
				str += output_line("cob_decimal_clear(%s%d);", CB_PREFIX_DEC_CONST, m->id);
				str += output_line("%s%d = NULL;", CB_PREFIX_DEC_CONST, m->id);
			}
		}
	}
	str += output_newline();
	str += output_line("return 0;");
	str += output_newline();
	/* End of CANCEL callback code */

prog_cancel_end:
	str += output_indent("}");
	str += output_newline();
	const char * s = (prog->prog_type == CB_FUNCTION_TYPE) ? "FUNCTION-ID" : "PROGRAM-ID";
	str += output_line("/* End %s '%s' */", s, prog->orig_program_id);
	str += output_newline();
	return str;
}

/* Output the entry function for a COBOL function. */
static string
output_function_entry_function(struct cb_program * prog, const int gencode,
							   const char * entry_name, cb_tree using_list)
{
	string str("");
	if(gencode) {
		str += output("cob_field *\n");
	} else {
		str += output("cob_field\t\t*");
	}
	str += output("%s(", entry_name);
	if(!gencode) {
		str += output("cob_field **, const int");
	} else {
		str += output("cob_field ** cob_fret, const int cob_pam");
	}
	cob_u32_t parmnum = 0;
	if(using_list) {
		str += output(", ");
		for(cb_tree l = using_list; l; l = CB_CHAIN(l), ++parmnum) {
			if(!gencode) {
				str += output("cob_field * ");
			} else {
				str += output("cob_field * f%u", parmnum);
			}
			if(CB_CHAIN(l)) {
				str += output(", ");
			}
		}
	}
	if(gencode) {
		str += output(")\n");
	} else {
		/* Finish prototype and return */
		str += output(");\n");
		return str;
	}
	str += output_indent("{");
	str += output_line("struct cob_func_loc\t*floc;");
	str += output_newline();
	str += output_line("/* Save environment */");
	str += output_prefix();
	str += output("floc = cob_save_func (cob_fret, cob_pam, %u",
				  parmnum);

	for(cob_u32_t n = 0; n < parmnum; ++n) {
		str += output(", f%u", n);
	}

	str += output(");\n");
	str += output_line("cob_call_params = cob_get_global_ptr ()->cob_call_params;");

	str += output_prefix();
	str += output("floc->ret_fld = %s_ (0", prog->program_id);

	if(parmnum != 0) {
		str += output(", ");
		for(cob_u32_t n = 0; n < parmnum; ++n) {
			str += output("floc->data[%u]", n);
			if(n != parmnum - 1) {
				str += output(", ");
			}
		}
	}
	str += output(");\n");
	str += output_line("**cob_fret = *floc->ret_fld;");
	str += output_line("/* Restore environment */");
	str += output_line("cob_restore_func (floc);");
	str += output_line("return *cob_fret;");
	str += output_indent("}");
	str += output_newline();
	return str;
}

/* Returns NULL if it could not deduce the parameter type. */
static const char *
try_get_by_value_parameter_type(const cb_field * f, cb_tree l)
{
	if(f->usage == CB_USAGE_FLOAT || f->usage == CB_USAGE_DOUBLE) {
		return "double";
	}
	if(f->usage == CB_USAGE_LONG_DOUBLE) {
		return "long double";
	}
	if(f->usage == CB_USAGE_FP_BIN32) {
		return "cob_u32_t";
	}
	if(f->usage == CB_USAGE_FP_BIN64 || f->usage == CB_USAGE_FP_DEC64 ||
			f->usage == CB_USAGE_PACKED ||
			(f->usage == CB_USAGE_DISPLAY && CB_TREE_CLASS(CB_VALUE(l)) == CB_CLASS_NUMERIC) ||
			f->usage == CB_USAGE_COMP_6) {
		return "cob_u64_t";
	}
	if(f->usage == CB_USAGE_FP_BIN128 ||
			f->usage == CB_USAGE_FP_DEC128) {
		return "cob_fp_128";
	}
	if(CB_TREE_CLASS(CB_VALUE(l)) == CB_CLASS_NUMERIC) {
		int sizes = CB_SIZES_INT(l);
		if(sizes == CB_SIZE_AUTO) {
			if(f->size > 4) {
				sizes = CB_SIZE_8;
			} else if(f->size > 2) {
				sizes = CB_SIZE_4;
			} else if(f->size > 1) {
				sizes = CB_SIZE_2;
			} else {
				sizes = CB_SIZE_1;
			}
		}
		bool is_unsigned = (CB_SIZES(l) & CB_SIZE_UNSIGNED) != 0;
		switch(sizes) {
		case CB_SIZE_1:
			if(is_unsigned) {
				return "cob_u8_t";
			}
			return "cob_c8_t";
		case CB_SIZE_2:
			if(is_unsigned) {
				return "cob_u16_t";
			}
			return "cob_s16_t";
		case CB_SIZE_4:
			if(is_unsigned) {
				return "cob_u32_t";
			}
			return "cob_s32_t";
		case CB_SIZE_8:
			if(is_unsigned) {
				return "cob_u64_t";
			}
			return "cob_s64_t";
		default:
			break;
		}
	}
	return NULL;
}

static string
output_program_entry_function_parameters(cb_tree using_list, const int gencode,
		string * s_type)
{
	string str("");
	cob_u32_t n = 0;
	for(cb_tree l = using_list; l; l = CB_CHAIN(l), ++n) {
		cb_field * f = cb_code_field(CB_VALUE(l));
		string fname = get_parm_name(f);
		switch(CB_PURPOSE_INT(l)) {
		case CB_CALL_BY_VALUE: {
			const char * type = try_get_by_value_parameter_type(f, l);
			if(type) {
				if(gencode) {
					str += output("%s %s", type, fname.c_str());
				} else {
					str += output("%s", type);
				}

				if(cb_sticky_linkage) {
					s_type[n] = "";
				} else {
					s_type[n] = "(cob_u8_ptr)&";
				}
				break;
			}
		}
		/* Fall through */
		case CB_CALL_BY_REFERENCE:
		case CB_CALL_BY_CONTENT:
			if(!f->bUseName || (!f->parent && !f->children)) {
				if(gencode) {
					str += output("cob_u8_t *%s", fname.c_str());
				} else {
					str += output("cob_u8_t *");
				}
			} else {
				if(f->parent) {
					f = f->parent;
				}
				if(gencode) {
					str += output("%s *%s", f->sType, fname.c_str());
				} else {
					str += output("%s *", f->sType);
				}
			}
			s_type[n] = "";
			break;
		default:
			break;
		}
		if(CB_CHAIN(l)) {
			str += output(", ");
		}
	}
	return str;
}

static string
output_entry_function(cb_program * prog, cb_tree entry,
					  cb_tree parameter_list, bool gencode)
{
	string	str("");

	const char * entry_name = CB_LABEL(CB_PURPOSE(entry))->name;
	cb_tree using_list = CB_VALUE(CB_VALUE(entry));

	if(gencode) {
		str += output("/* ENTRY '%s' */\n\n", entry_name);
	}

	if(!gencode && !prog->nested_level) {
		str += output("extern \"C\" ");
#if	defined(_WIN32) || defined(__CYGWIN__)
		str += output("__declspec(dllexport) ");
#endif
	}

	if(unlikely(prog->prog_type == CB_FUNCTION_TYPE)) {
		str += output_function_entry_function(prog, gencode, entry_name, using_list);
		return str;
	}

	/* entry convention */
	int entry_convention = 0;
	cb_tree l = CB_PURPOSE(CB_VALUE(entry));
	if(!l || !(CB_INTEGER_P(l) || CB_NUMERIC_LITERAL_P(l))) {
		/* not translated as it is an unlikely internal abort, remove the check later */
		/* LCOV_EXCL_START */
		cobc_err_msg("Missing /wrong internal entry convention!");
		cobc_err_msg(_("Please report this!"));
		COBC_ABORT();
		/* LCOV_EXCL_STOP */
	} else if(CB_INTEGER_P(l)) {
		entry_convention = CB_INTEGER(l)->val;
	} else if(CB_NUMERIC_LITERAL_P(l)) {
		entry_convention = cb_get_int(l);
	}

	/* Output return type. */
	if((prog->nested_level && !prog->flag_void)
			|| (prog->flag_main && !prog->flag_recursive
				&& !strcmp(prog->program_id, entry_name))) {
		str += output("static ");
	}
	if(prog->flag_void) {
		str += output("void");
	} else {
		str += output("int");
	}
	if(entry_convention & CB_CONV_STDCALL) {
		str += output(" __stdcall");
	}
	if(gencode) {
		str += output("\n");
	} else {
		str += output("\t\t");
	}

	/* Output function name */
	if(prog->nested_level) {
		str += output("%s_%d__(", entry_name, prog->toplev_count);
	} else {
		str += output("%s(", entry_name);
	}

	/* Output parameter list */
	if(prog->flag_chained) {
		using_list = NULL;
		parameter_list = NULL;
	}

	if(!gencode && !using_list) {
		str += output("void);\n");
		return str;
	}

	string	s_type[MAX_CALL_FIELD_PARAMS];
	string	sticky_ids[MAX_CALL_FIELD_PARAMS];
	char	sticky_nonp[MAX_CALL_FIELD_PARAMS];
	memset(sticky_nonp, 0, sizeof(sticky_nonp));

	str += output_program_entry_function_parameters(using_list, gencode, s_type);

	if(gencode) {
		str += output(")\n");
	} else {
		/* Finish prototype and return */
		str += output(");\n");
		return str;
	}

	str += output_indent("{");

	/* By value fields */
	for(cb_tree l = using_list; l; l = CB_CHAIN(l)) {
		cb_field * f = cb_code_field(CB_VALUE(l));
		if(CB_PURPOSE_INT(l) == CB_CALL_BY_VALUE) {
			if(f->usage == CB_USAGE_POINTER || f->usage == CB_USAGE_PROGRAM_POINTER) {
				string fname = get_parm_name(f);
				str += output_line("  unsigned char\t\t*_ptr_%s;", fname.c_str());
			} else if(f->usage == CB_USAGE_FLOAT && !cb_sticky_linkage) {
				string fname = get_parm_name(f);
				str += output_line("  float\t\t_flt_%s;", fname.c_str());
			}
		}
	}

	/* For calling into a module, cob_call_params may not be known */
	if(using_list) {
		int parmnum = 0;
		for(cb_tree l = using_list; l; l = CB_CHAIN(l)) {
			parmnum++;
		}
		if(entry_convention & CB_CONV_COBOL) {
			str += output_line("/* Get current number of call parameters,");
			str += output_line("   if the parameter count is unknown, set it to all */");
			if(cb_flag_implicit_init) {
				str += output_line("if(cob_is_initialized() && cob_get_global_ptr()->cob_current_module) {");
			} else {
				str += output_line("if(cob_get_global_ptr()->cob_current_module) {");
			}
			str += output_line("\tcob_call_params = cob_get_global_ptr()->cob_call_params;");
			str += output_line("} else {");
			str += output_line("\tcob_call_params = %d;", parmnum);
			str += output_line("};\n");
		} else {
			str += output_line("/* Set current number of call parameters to max */\n");
			str += output_line("cob_call_params = %d;\n", parmnum);
		}
		str += output_newline();
	}

	/* We have to cater for sticky-linkage here at the entry point site */
	/* Doing it in the internal function is too late as we */
	/* then do not have the information as to possible ENTRY clauses */

	cob_u32_t parmnum = 0;
	/* Sticky linkage parameters */
	if(cb_sticky_linkage && using_list) {
		for(cb_tree l = using_list; l; l = CB_CHAIN(l), parmnum++) {
			cb_field * f = cb_code_field(CB_VALUE(l));
			sticky_ids[parmnum] = get_parm_name(f);
			if(CB_PURPOSE_INT(l) == CB_CALL_BY_VALUE) {
				const char * s = NULL;
				const char * s2 = "0";
				if(f->usage == CB_USAGE_FLOAT) {
					s = "float";
				} else if(f->usage == CB_USAGE_DOUBLE) {
					s = "double";
				} else if(f->usage == CB_USAGE_LONG_DOUBLE) {
					s = "long double";
				} else if(f->usage == CB_USAGE_FP_BIN32) {
					s = "cob_u32_t";
				} else if(f->usage == CB_USAGE_FP_BIN64 ||
						  f->usage == CB_USAGE_FP_DEC64) {
					s = "cob_u64_t";
				} else if(f->usage == CB_USAGE_FP_BIN128 ||
						  f->usage == CB_USAGE_FP_DEC128) {
					s = "cob_fp_128";
					s2 = "{{0, 0}}";
				} else if(CB_TREE_CLASS(CB_VALUE(l)) == CB_CLASS_NUMERIC) {
					int sizes = CB_SIZES_INT(l);
					if(sizes == CB_SIZE_AUTO) {
						if(f->size > 4) {
							sizes = CB_SIZE_8;
						} else if(f->size > 2) {
							sizes = CB_SIZE_4;
						} else if(f->size > 1) {
							sizes = CB_SIZE_2;
						} else {
							sizes = CB_SIZE_1;
						}
					}
					switch(sizes) {
					case CB_SIZE_1:
						if(CB_SIZES(l) & CB_SIZE_UNSIGNED) {
							s = "cob_u8_t";
						} else {
							s = "cob_c8_t";
						}
						break;
					case CB_SIZE_2:
						if(CB_SIZES(l) & CB_SIZE_UNSIGNED) {
							s = "cob_u16_t";
						} else {
							s = "cob_s16_t";
						}
						if(f->flag_binary_swap) {
							sticky_nonp[parmnum] = 2;
						}
						break;
					case CB_SIZE_4:
						if(CB_SIZES(l) & CB_SIZE_UNSIGNED) {
							s = "cob_u32_t";
						} else {
							s = "cob_s32_t";
						}
						if(f->flag_binary_swap) {
							sticky_nonp[parmnum] = 4;
						}
						break;
					case CB_SIZE_8:
						if(CB_SIZES(l) & CB_SIZE_UNSIGNED) {
							s = "cob_u64_t";
						} else {
							s = "cob_s64_t";
						}
						if(f->flag_binary_swap) {
							sticky_nonp[parmnum] = 8;
						}
						break;
					default:
						break;
					}
				}
				if(s) {
					str += output_line("  static %s\tcob_parm_l_%s = %s;",
									   s, sticky_ids[parmnum].c_str(), s2);
					if(sticky_nonp[parmnum] == 0) {
						sticky_nonp[parmnum] = 1;
					}
				}
			}
		}
	}

	/* Sticky linkage set up */
	if(cb_sticky_linkage && using_list) {
		str += output_line("/* Set the parameter list */");
		parmnum = 1;
		str += output_line("switch(cob_call_params) {");
		str += output_line("case 0:");
		str += output_line("\tbreak;");
		for(cb_tree l = using_list; l; l = CB_CHAIN(l), parmnum++) {
			if(CB_CHAIN(l)) {
				str += output_line("case %u:", parmnum);
			} else {
				str += output_line("default:");
			}
			for(cob_u32_t n = 0; n < parmnum; ++n) {
				if(sticky_nonp[n]) {
					switch(sticky_nonp[n]) {
					case 8:
						str += output_line("\tcob_parm_l_%s = COB_BSWAP_64(%s);",
										   sticky_ids[n].c_str(), sticky_ids[n].c_str());
						break;
					case 4:
						str += output_line("\tcob_parm_l_%s = COB_BSWAP_32((cob_u32_t)%s);",
										   sticky_ids[n].c_str(), sticky_ids[n].c_str());
						break;
					case 2:
						str += output_line("\tcob_parm_l_%s = COB_BSWAP_16((cob_u16_t)%s);",
										   sticky_ids[n].c_str(), sticky_ids[n].c_str());
						break;
					default:
						str += output_line("\tcob_parm_l_%s = %s;",
										   sticky_ids[n].c_str(), sticky_ids[n].c_str());
						break;
					}
					str += output_line("\tcob_parm_%s = (cob_u8_ptr)&cob_parm_l_%s;",
									   sticky_ids[n].c_str(), sticky_ids[n].c_str());
				} else {
					str += output_line("\tcob_parm_%s = %s;",
									   sticky_ids[n].c_str(), sticky_ids[n].c_str());
				}
			}
			str += output_line("\tbreak;");
		}
		str += output_line("}");
	}

	const char * s_prefix = (cb_sticky_linkage) ? "cob_parm_" : "";

	for(cb_tree l = using_list; l; l = CB_CHAIN(l)) {
		cb_field * f = cb_code_field(CB_VALUE(l));
		if(CB_PURPOSE_INT(l) == CB_CALL_BY_VALUE) {
			if(f->usage == CB_USAGE_POINTER || f->usage == CB_USAGE_PROGRAM_POINTER) {
				string fname = get_parm_name(f);
				str += output_line("  _ptr_%s = %s%s;",
								   fname.c_str(), s_prefix, fname.c_str());
			} else if(!cb_sticky_linkage && f->usage == CB_USAGE_FLOAT) {
				string fname = get_parm_name(f);
				str += output_line("  _flt_%s = %s;", fname.c_str(), fname.c_str());
			} else if(!cb_sticky_linkage && f->flag_binary_swap) {
				string fname = get_parm_name(f);
				switch(f->size) {
				case 2:
					str += output_line("  %s = COB_BSWAP_16((cob_u16_t)%s);", fname.c_str(), fname.c_str());
					break;
				case 4:
					str += output_line("  %s = COB_BSWAP_32((cob_u32_t)%s);", fname.c_str(), fname.c_str());
					break;
				case 8:
					str += output_line("  %s = COB_BSWAP_64(%s);", fname.c_str(), fname.c_str());
					break;
				}
			}
		}
	}

	str += output_prefix();
	if(prog->flag_void) {
		str += output("  (void)");
	} else {
		str += output("  return ");
	}
	if(!prog->nested_level) {
		str += output("%s_(%d", prog->program_id, progid++);
	} else {
		str += output("%s_%d_(%d", prog->program_id,
					  prog->toplev_count, progid++);
	}

	if(using_list || parameter_list) {
		/* Output parameter list for final function call. */
		for(cb_tree l1 = parameter_list; l1; l1 = CB_CHAIN(l1)) {
			cb_field * f1 = cb_code_field(CB_VALUE(l1));
			cob_u32_t n = 0;
			cb_tree l2;
			for(l2 = using_list; l2; l2 = CB_CHAIN(l2), ++n) {
				cb_field * f2 = cb_code_field(CB_VALUE(l2));
				string fname = get_parm_name(f2);
				if(strcasecmp(f1->name, f2->name) == 0) {
					switch(CB_PURPOSE_INT(l2)) {
					case CB_CALL_BY_VALUE:
						if(f2->usage == CB_USAGE_POINTER ||
								f2->usage == CB_USAGE_PROGRAM_POINTER) {
							str += output(", (cob_u8_ptr)&_ptr_%s", fname.c_str());
							break;
						}
						if(f2->usage == CB_USAGE_FLOAT && !cb_sticky_linkage) {
							str += output(", (cob_u8_ptr)&_flt_%s", fname.c_str());
							break;
						}
					/* Fall through */
					case CB_CALL_BY_REFERENCE:
					case CB_CALL_BY_CONTENT:
						str += output(", %s%s%s",
									  s_type[n].c_str(), s_prefix, fname.c_str());
						break;
					default:
						break;
					}
					break;
				}
			}
			if(l2 == NULL) {
				string fname = get_parm_name(f1);
				if(cb_sticky_linkage) {
					str += output(", %s%s", s_prefix, fname.c_str());
				} else {
					str += output(", NULL");
				}
			}
		}
	}
	str += output(");\n");
	str += output_indent("}");
	str += output_newline();
	return str;
}

static string
output_main_function(cb_program * prog)
{
	string str = output_line("/* Main function */");
	if(!cb_flag_winmain) {
		str += output_line("int");
		str += output_line("main(int argc, char **argv)");
		str += output_indent("{");
		str += output_line("cob_init(argc, argv);");
	} else {
		str += output_line("int WINAPI");
		str += output_line("WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, PSTR pCmdLine, int nCmdShow)");
		str += output_indent("{");
		str += output_line("cob_init(__argc, __argv);");
	}
	str += output_line("cob_stop_run(%s());", prog->program_id);
	str += output_indent("}\n");
	return str;
}

static void
output_header(FILE * fp, const char * locbuff, const cb_program * cp)
{
	if(fp) {
		fprintf(fp, "/* Generated by            cobc %s.%d */\n",
				PACKAGE_VERSION, PATCH_LEVEL);
		fprintf(fp, "/* Generated from              %s */\n", cb_source_file);
		fprintf(fp, "/* Generated at                %s */\n", locbuff);
		fprintf(fp, "/* GnuCOBOL C++ build date     %s */\n", cb_cobc_build_stamp);
		fprintf(fp, "/* GnuCOBOL C++ package date   %s */\n", COB_TAR_DATE);
		fprintf(fp, "/* Compile command         ");
		for(int i = 0; i < cb_saveargc; i++) {
			fprintf(fp, "%s ", cb_saveargv[i]);
		}
		fprintf(fp, "*/\n\n");
		if(cp) {
			fprintf(fp, "/* Program local variables for '%s' */\n\n",
					cp->orig_program_id);
		}
	}
}

void
codegen(cb_program * prog, const int nested)
{
	/* Clear local program stuff */
	declpgms.clear();
	current_prog = prog;
	param_id = 0;
	stack_id = 0;
	num_cob_fields = 0;
	progid = 0;
	loop_counter = 0;
	output_indent_level = 0;
	last_line = 0;
	needs_exit_prog = 0;
	gen_custom = 0;
	gen_nested_tab = 0;
	gen_dynamic = 0;
	gen_if_level = 0;
	local_mem = 0;
	local_working_mem = 0;
	need_save_exception = 0;
	last_segment = 0;
	last_section = NULL;
	call_cache = NULL;
	func_call_cache = NULL;
	label_cache = NULL;
	local_base_cache = NULL;
	local_field_cache = NULL;
	inside_check = 0;
	for(int i = 0; i < COB_INSIDE_SIZE; ++i) {
		inside_stack[i] = 0;
	}
	excp_current_program_id = prog->orig_program_id;
	excp_current_section = NULL;
	excp_current_paragraph = NULL;
	memset((void *)i_counters, 0, sizeof(i_counters));

	output_target = yyout;
	cb_local_file = current_prog->local_include->local_fp;

	if(!nested) {
		/* First iteration */
		gen_alt_ebcdic = 0;
		gen_ebcdic_ascii = 0;
		gen_full_ebcdic = 0;
		gen_native = 0;
		gen_figurative = 0;
		non_nested_count = 0;
		working_mem = 0;
		pic_cache = NULL;
		base_cache = NULL;
		globext_cache = NULL;
		field_cache = NULL;
		if(!string_buffer) {
			string_buffer = (char *) cobc_main_malloc((size_t)COB_MINI_BUFF);
		}

		time_t sectime = time(NULL);
		struct tm * loctime = localtime(&sectime);
		/* Leap seconds ? */
		if(loctime->tm_sec >= 60) {
			loctime->tm_sec = 59;
		}
		if(loctime) {
			strftime(string_buffer, (size_t)COB_MINI_MAX,
					 "%b %d %Y %H:%M:%S", loctime);
		} else {
			strcpy(string_buffer, "Unknown");
		}
		output_header(output_target, string_buffer, NULL);
		output_header(cb_storage_file, string_buffer, NULL);
		for(cb_program * cp = prog; cp; cp = cp->next_program) {
			output_header(cp->local_include->local_fp,
						  string_buffer, cp);
		}

#ifndef	_GNU_SOURCE
#ifdef	_XOPEN_SOURCE_EXTENDED
		real_output("#ifndef\t_XOPEN_SOURCE_EXTENDED\n");
		real_output("#define\t_XOPEN_SOURCE_EXTENDED 1\n");
		real_output("#endif\n");
#endif
#endif
		real_output("#include <stdio.h>\n");
		real_output("#include <stdlib.h>\n");
		real_output("#include <stddef.h>\n");
		real_output("#include <string.h>\n");
		real_output("#include <math.h>\n");
#ifdef	WORDS_BIGENDIAN
		real_output("#define  WORDS_BIGENDIAN 1\n");
#endif
#ifdef	COB_KEYWORD_INLINE
		real_output(output("#define  COB_KEYWORD_INLINE %s\n",
						   CB_XSTRINGIFY(COB_KEYWORD_INLINE)).c_str());
#endif
		if(cb_flag_winmain) {
			real_output("#include <windows.h>\n");
		}
		real_output("#include <libcob.h>\n\n");

		real_output(output("#define  COB_SOURCE_FILE\t\t\"%s\"\n",
						   cb_source_file).c_str());
		real_output(output("#define  COB_PACKAGE_VERSION\t\t\"%s\"\n",
						   PACKAGE_VERSION).c_str());
		real_output(output("#define  COB_PATCH_LEVEL\t\t%d\n",
						   PATCH_LEVEL).c_str());
		/* string_buffer has formatted date from above */
		real_output(output("#define  COB_MODULE_FORMATTED_DATE\t\"%s\"\n",
						   string_buffer).c_str());
		if(loctime) {
			int i = ((loctime->tm_year + 1900) * 10000) +
					((loctime->tm_mon + 1) * 100) +
					loctime->tm_mday;
			real_output(output("#define  COB_MODULE_DATE\t\t%d\n", i).c_str());
			i = (loctime->tm_hour * 10000) +
				(loctime->tm_min * 100) +
				loctime->tm_sec;
			real_output(output("#define  COB_MODULE_TIME\t\t%d\n", i).c_str());
		} else {
			real_output(output("#define  COB_MODULE_DATE\t\t0\n").c_str());
			real_output(output("#define  COB_MODULE_TIME\t\t0\n").c_str());
		}

		real_output(output_newline().c_str());
		real_output("/* Global variables */\n");
		real_output(output("#include \"%s\"\n\n", cb_storage_file_name).c_str());

		real_output("/* Function prototypes */\n\n");
		for(cb_program * cp = prog; cp; cp = cp->next_program) {
			/* Build parameter list */
			for(cb_tree entry = cp->entry_list; entry; entry = CB_CHAIN(entry)) {
				for(cb_tree entry_param = CB_VALUE(CB_VALUE(entry)); entry_param; entry_param = CB_CHAIN(entry_param)) {
					cb_tree prog_param;
					for(prog_param = cp->parameter_list; prog_param; prog_param = CB_CHAIN(prog_param)) {
						if(strcasecmp(cb_code_field(CB_VALUE(entry_param))->name,
									  cb_code_field(CB_VALUE(prog_param))->name) == 0) {
							break;
						}
					}
					if(prog_param == NULL) {
						cp->parameter_list = cb_list_add(cp->parameter_list, CB_VALUE(entry_param));
					}
				}
			}
			if(cp->flag_main) {
				if(!cp->flag_recursive) {
					real_output(output("static int\t\t%s();\n",
									   cp->program_id).c_str());
				} else {
					real_output(output("int\t\t\t%s();\n",
									   cp->program_id).c_str());
				}
				for(cb_tree l = cp->entry_list; l; l = CB_CHAIN(l)) {
					const char * entry_name = CB_LABEL(CB_PURPOSE(l))->name;
					if(0 == strcmp(entry_name, cp->program_id)) {
						continue;
					}
					real_output(output_entry_function(cp, l, cp->parameter_list, false).c_str());
				}
			} else {
				for(cb_tree l = cp->entry_list; l; l = CB_CHAIN(l)) {
					real_output(output_entry_function(cp, l, cp->parameter_list, false).c_str());
				}
			}
			if(cp->prog_type == CB_FUNCTION_TYPE) {
				non_nested_count++;
#if	0	/* RXWRXW USERFUNC */
				real_output(output("static cob_field\t*%s_(const int, cob_field **",
#else
				real_output(output("static cob_field\t*%s_(const int",
#endif
								   cp->program_id).c_str());
			} else if(!cp->nested_level) {
				non_nested_count++;
				real_output(output("static int\t\t%s_(const int",
								   cp->program_id).c_str());
			} else {
				real_output(output("static int\t\t%s_%d_(const int",
								   cp->program_id, cp->toplev_count).c_str());
			}
#if	0	/* RXWRXW USERFUNC */
			if(!cp->flag_chained && cp->prog_type != CB_FUNCTION_TYPE) {
#else
			if(!cp->flag_chained) {
#endif
				for(cb_tree l = cp->parameter_list; l; l = CB_CHAIN(l)) {
					cb_field * f = cb_code_field(CB_VALUE(l));
					string fname = get_parm_name(f);
					if(f->parent) {
						f = f->parent;
					}
					if(f->children && f->bUseName) {
						real_output(output(", %s *", f->sType).c_str());
					} else {
						real_output(", cob_u8_t *");
					}
					if(cb_sticky_linkage) {
						if(f->children && f->bUseName) {
							output_storage("static %s\t\t*cob_parm_%s = NULL;\n", f->sType, fname.c_str());
						} else {
							output_storage("static cob_u8_t\t\t\t*cob_parm_%s = NULL;\n", fname.c_str());
						}
					}
				}
			}
#if	0	/* RXWRXW - NOINLINE */
			if(cb_flag_stack_check) {
				real_output(") COB_NOINLINE;\n");
			} else {
#endif
				real_output(");\n");
#if	0	/* RXWRXW - NOINLINE */
			}
#endif
		}
		real_output("\n");
	}

	/* Class-names */
	if(!prog->nested_level && prog->class_name_list) {
		real_output("/* Class names */\n");
		for(cb_tree l = prog->class_name_list; l; l = CB_CHAIN(l)) {
			real_output(output_class_name_definition(CB_CLASS_NAME(CB_VALUE(l))).c_str());
		}
	}

	/* Main function */
	if(prog->flag_main) {
		real_output(output_main_function(prog).c_str());
	}

	/* Functions */
	if(!nested) {
		real_output("/* Functions */\n\n");
	}

	if(prog->prog_type == CB_FUNCTION_TYPE) {
		real_output(output("/* FUNCTION-ID '%s' */\n\n", prog->orig_program_id).c_str());
	} else {
		real_output(output("/* PROGRAM-ID '%s' */\n\n", prog->orig_program_id).c_str());
	}

	for(cb_tree l = prog->entry_list; l; l = CB_CHAIN(l)) {
		real_output(output_entry_function(prog, l, prog->parameter_list, true).c_str());
	}

	real_output(output_internal_function(prog, prog->parameter_list).c_str());

	if(!prog->next_program) {
		real_output("/* End functions */\n\n");
	}

	if(gen_native || gen_full_ebcdic ||
			gen_ebcdic_ascii || prog->alphabet_name_list) {
		(void)lookup_attr(COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL, 0);
	}

	output_target = cb_storage_file;

	/* Program local stuff */

	/* CALL cache */
	if(needs_unifunc || call_cache || func_call_cache) {
		output_local("\n/* Call pointers */\n");
	}
	if(needs_unifunc) {
		output_local("cob_call_union\t\tcob_unifunc;\n");
	}
	for(call_list * clp = call_cache; clp; clp = clp->next) {
		output_local("static cob_call_union\tcall_%s;\n",
					 clp->callname);
	}
	for(call_list * clp = func_call_cache; clp; clp = clp->next) {
		output_local("static cob_call_union\tfunc_%s;\n",
					 clp->callname);
	}
	needs_unifunc = 0;

	/* Nested / contained list */
	if(prog->nested_prog_list && gen_nested_tab) {
		/* Generate contained prog list */
		output_local("\n/* Nested call table */\n");
		output_local("static struct cob_call_struct\tcob_nest_tab[] = {\n");
		for(nested_list * nlp = prog->nested_prog_list; nlp; nlp = nlp->next) {
			if(nlp->nested_prog == prog) {
#if	0	/* RXWRXW Fix recursive */
				if(!prog->flag_recursive) {
					continue;
				}
#endif
				output_local("\t{ \"%s\", {(fvoidpptr)%s_%d__ }, { NULL } },\n",
							 nlp->nested_prog->orig_program_id,
							 nlp->nested_prog->program_id,
							 nlp->nested_prog->toplev_count);
			} else {
				output_local("\t{ \"%s\", {(fvoidpptr)%s_%d__ }, {(fvoidpptr)%s_%d_ } },\n",
							 nlp->nested_prog->orig_program_id,
							 nlp->nested_prog->program_id,
							 nlp->nested_prog->toplev_count,
							 nlp->nested_prog->program_id,
							 nlp->nested_prog->toplev_count);
			}
		}
		output_local("\t{ NULL, { NULL }, { NULL } }\n");
		output_local("};\n");
	}

	/* Local indexes */
	bool found = false;
	for(int i = 0; i < COB_MAX_SUBSCRIPTS; i++) {
		if(i_counters[i]) {
			if(!found) {
				found = true;
				output_local("\n/* Subscripts */\n");
			}
			output_local("int\t\t_i%d;\n", i);
		}
	}

	/* PERFORM TIMES counters */
	if(loop_counter) {
		output_local("\n/* Loop counters */\n");
		for(int i = 0; i < loop_counter; i++) {
			output_local("cob_s64_t\t_n%d = 0;\n", i);
		}
		output_local("\n");
	}

	/* Local implicit fields */
	if(num_cob_fields) {
		output_local("\n/* Local cob_field items */\n");
		for(int i = 0; i < num_cob_fields; i++) {
			output_local("cob_field\t\t_f%d(0, 0, 0);\n", i);
		}
		output_local("\n");
	}

	/* Debugging fields */
	if(prog->flag_debugging) {
		output_local("\n/* DEBUG runtime switch */\n");
		output_local("static int\tcob_debugging_mode = 0;\n");
	}
	if(need_save_exception) {
		output_local("\n/* DEBUG exception code save */\n");
		output_local("int\t\tsave_exception_code = 0;\n");
	}

	/* LOCAL storage pointer */
	if(prog->local_storage && local_mem) {
		output_local("\n/* LOCAL storage pointer */\n");
		output_local("unsigned char\t\t*cob_local_ptr = NULL;\n");
		if(current_prog->flag_global_use) {
			output_local("static unsigned char\t*cob_local_save = NULL;\n");
		}
	}

	/* Call parameter stack */
	output_local("\n/* Call parameters */\n");
	if(cb_flag_stack_on_heap || prog->flag_recursive) {
		output_local("cob_field\t\t**cob_procedure_params;\n");
	} else {
		int i = prog->max_call_param ? prog->max_call_param : 1;
		output_local("cob_field\t\t*cob_procedure_params[%d];\n", i);
	}

	/* Frame stack */
	output_local("\n/* Perform frame stack */\n");
	if(cb_flag_stack_check) {
		output_local("struct cob_frame\t* _frame_overflow;\n");
	}
	output_local("struct cob_frame\t* _frame_ptr;\n");
	if(cb_flag_stack_on_heap || prog->flag_recursive) {
		output_local("struct cob_frame\t* _frame_stack;\n\n");
	} else {
		output_local("struct cob_frame\t_frame_stack[%d];\n\n", cb_stack_size);
	}

	if(gen_dynamic) {
		output_local("\n/* Dynamic field FUNCTION-ID pointers */\n");
		for(cob_u32_t inc = 0; inc < gen_dynamic; inc++) {
			output_local("cob_field\t*cob_dyn_%u = NULL;\n", inc);
		}
	}

	if(local_base_cache) {
		output_local("\n/* Data storage */\n");
		local_base_cache = (base_list *) list_cache_sort(local_base_cache,
						   &base_cache_cmp);
		for(base_list * blp = local_base_cache; blp; blp = blp->next) {
			cb_field * f = blp->f;
			if(f->special_index == 2U) {
				if(!f->bUseName) {
					output_local("int              %s%d;\t// %s\n",
								 CB_PREFIX_BASE, f->id, f->name);
				} else {
					output_local("int              %s;\t// %s\n", f->sName, f->name);
				}
			} else if(f->special_index) {
				if(!f->bUseName) {
					output_local("static int       %s%d;\t// %s\n",
								 CB_PREFIX_BASE, f->id, f->name);
				} else {
					output_local("static int       %s;\t// %s\n", f->sName, f->name);
				}
			} else {
				if(f->bUseName && f->parent) {
					f = f->parent;
				}
				string st = output_field_dcl(f);
				output_local("%s", st.c_str());
			}
		}
		output_local("\n/* End of data storage */\n\n");
	}

	if(local_field_cache) {
		/* Switch to local storage file */
		output_target = current_prog->local_include->local_fp;
		output_local("\n/* Fields */\n");
		local_field_cache = (field_list *) list_cache_sort(local_field_cache,
							&field_cache_cmp);
		for(field_list * k = local_field_cache; k; k = k->next) {
			real_output(output("static cob_field %s%d", CB_PREFIX_FIELD,
							   k->f->id).c_str());
			if(!k->f->flag_local) {
				real_output(output_field(k->x).c_str());
			} else {
				real_output(output("(").c_str());
				real_output(output_size(k->x).c_str());
				real_output(output(", NULL, ").c_str());
				real_output(output_attr(k->x).c_str());
				real_output(output(")").c_str());
			}
			if(k->f->flag_filler) {
				real_output(output(";\t/* Implicit FILLER */\n").c_str());
			} else {
				real_output(output(";\t/* %s */\n", k->f->name).c_str());
			}
		}
		output_local("\n/* End of fields */\n\n");
		/* Switch to main storage file */
		output_target = cb_storage_file;
	}

	/* Skip to next nested program */

	if(prog->next_program) {
		codegen(prog->next_program, 1);
		return;
	}

	/* Finalize the main include file */

	if(!cobc_flag_main && non_nested_count > 1) {
		output_storage("\n/* Module reference count */\n");
		output_storage("static unsigned int\t\tcob_reference_count = 0;\n");
	}

	output_storage("\n/* Module path */\n");
	output_storage("static const char\t\t*cob_module_path = NULL;\n");

	output_storage("\n/* Number of call parameters */\n");
	output_storage("static int\t\tcob_call_params = 0;\n");

	if(globext_cache) {
		output_storage("\n/* GLOBAL EXTERNAL pointers */\n");
		globext_cache = (base_list *) list_cache_sort(globext_cache, &base_cache_cmp);
		for(base_list * blp = globext_cache; blp; blp = blp->next) {
			output_storage("static unsigned char\t\t*%s%d = NULL;",
						   CB_PREFIX_BASE, blp->f->id);
			output_storage("\t/* %s */\n", blp->f->name);
		}
	}

	if(base_cache) {
		output_storage("\n/* Data storage */\n");
		base_cache = (base_list *) list_cache_sort(base_cache, &base_cache_cmp);
		const char * prevprog = NULL;
		for(base_list * blp = base_cache; blp; blp = blp->next) {
			if(blp->curr_prog != prevprog) {
				prevprog = blp->curr_prog;
				output_storage("\n/* PROGRAM-ID : %s */\n",
							   prevprog);
			}
			cb_field * f = blp->f;
			if(f->special_index) {
				if(!f->sName) {
					output_storage("static int       %s%d;\t// %s\n",
								   CB_PREFIX_BASE, f->id, f->name);
				} else {
					output_storage("static int       %s;\t// %s\n", f->sName, f->name);
				}
			} else {
				if(f->bUseName && f->parent) {
					f = f->parent;
				}
				string st = output_field_dcl(f);
				output_storage("%s", st.c_str());
			}
		}
		output_storage("\n/* End of data storage */\n\n");
	}

	output_pic_cache();

	/* Attributes */
	if(attr_cache || gen_figurative) {
		output_storage("\n/* Attributes */\n\n");
		attr_cache = list_reverse(attr_cache);
		for(attr_list * attr = attr_cache; attr; attr = attr->next) {
			output_storage("static const cob_field_attr %s%d(", CB_PREFIX_ATTR, attr->id);
			output_storage("0x%02x, %3u, %3d, 0x%04x, ",
						   attr->type, attr->digits,
						   attr->scale, attr->flags);
			if(attr->pic_id != -1) {
				output_storage("%s%d", CB_PREFIX_PIC, attr->pic_id);
			} else {
				output_storage("NULL");
			}
			output_storage(");\n");
		}
		if(gen_figurative) {
			output_storage("\nstatic const cob_field_attr cob_all_attr");
			output_storage("(0x%02x, 0, 0, 0, NULL);\n", COB_TYPE_ALPHANUMERIC_ALL);
		}
		output_storage("\n");
	}

	if(field_cache) {
		output_storage("\n/* Fields */\n");
		field_cache = (field_list *) list_cache_sort(field_cache, &field_cache_cmp);
		const char * prevprog = NULL;
		for(field_list * k = field_cache; k; k = k->next) {
			if(k->curr_prog != prevprog) {
				prevprog = k->curr_prog;
				output_storage("\n/* PROGRAM-ID : %s */\n",
							   prevprog);
			}
			real_output(output("static cob_field %s%d", CB_PREFIX_FIELD,
							   k->f->id).c_str());
			if(!k->f->flag_local) {
				real_output(output_field(k->x).c_str());
			} else {
				real_output(output("(").c_str());
				real_output(output_size(k->x).c_str());
				real_output(output(", NULL, ").c_str());
				real_output(output_attr(k->x).c_str());
				real_output(output(")").c_str());
			}
			if(k->f->flag_filler) {
				real_output(output(";\t/* Implicit FILLER */\n").c_str());
			} else {
				real_output(output(";\t/* %s */\n", k->f->name).c_str());
			}
		}
		output_storage("\n/* End of fields */\n\n");
	}

	/* Literals, figuratives, constants */
	if(literal_cache || gen_figurative) {
		output_storage("\n/* Constants */\n");
		string str("");
		literal_cache = list_reverse(literal_cache);
		for(literal_list * m = literal_cache; m; m = m->next) {
#if	0	/* RXWRXW - Const */
			str += output("static const cob_fld_union %s%d\t= ",
						  CB_PREFIX_CONST, m->id);
			str += output("{");
			str += output_size(m->x);
			str += output(", ");
			cb_literal * lp = CB_LITERAL(m->x);
			if(CB_TREE_CLASS(m->x) == CB_CLASS_NUMERIC) {
				str += output("\"%s%s\"", (char *)lp->data,
							  (lp->sign < 0) ? "-" : (lp->sign > 0) ? "+" : "");
			} else {
				str += output_string(lp->data, (int) lp->size, lp->llit);
			}
			str += output(", ");
			str += output_attr(m->x);
			str += output("}");
#else
			str += output("static const cob_field %s%d",
						  CB_PREFIX_CONST, m->id);
			str += output_field(m->x);
#endif
			str += output(";\n");
		}
		if(gen_figurative) {
			str += output("\n");
			if(gen_figurative & CB_NEED_LOW) {
				str += output("static cob_field cob_all_low");
				str += output("(1, ");
				str += output("(cob_u8_ptr)\"\\0\", ");
				str += output("&cob_all_attr);\n");
			}
			if(gen_figurative & CB_NEED_HIGH) {
				str += output("static cob_field cob_all_high");
				str += output("(1, ");
				str += output("(cob_u8_ptr)\"\\xff\", ");
				str += output("&cob_all_attr);\n");
			}
			if(gen_figurative & CB_NEED_QUOTE) {
				str += output("static cob_field cob_all_quote");
				str += output("(1, ");
				if(cb_flag_apostrophe) {
					str += output("(cob_u8_ptr)\"'\", ");
				} else {
					str += output("(cob_u8_ptr)\"\\\"\", ");
				}
				str += output("&cob_all_attr);\n");
			}
			if(gen_figurative & CB_NEED_SPACE) {
				str += output("static cob_field cob_all_space");
				str += output("(1, ");
				str += output("(cob_u8_ptr)\" \", ");
				str += output("&cob_all_attr);\n");
			}
			if(gen_figurative & CB_NEED_ZERO) {
				str += output("static cob_field cob_all_zero");
				str += output("(1, ");
				str += output("(cob_u8_ptr)\"0\", ");
				str += output("&cob_all_attr);\n");
			}
		}
		str += output("\n");
		real_output(str.c_str());
	}

	/* Collating tables */
	if(gen_alt_ebcdic) {
		output_storage("\n/* ASCII to EBCDIC translate table(restricted) */\n");
		string str = output("static const unsigned char\tcob_a2e[256] = {\n");
		/* Restricted table */
		str += output("\t0x00, 0x01, 0x02, 0x03, 0x1D, 0x19, 0x1A, 0x1B,\n");
		str += output("\t0x0F, 0x04, 0x16, 0x06, 0x07, 0x08, 0x09, 0x0A,\n");
		str += output("\t0x0B, 0x0C, 0x0D, 0x0E, 0x1E, 0x1F, 0x1C, 0x17,\n");
		str += output("\t0x10, 0x11, 0x20, 0x18, 0x12, 0x13, 0x14, 0x15,\n");
		str += output("\t0x21, 0x27, 0x3A, 0x36, 0x28, 0x30, 0x26, 0x38,\n");
		str += output("\t0x24, 0x2A, 0x29, 0x25, 0x2F, 0x2C, 0x22, 0x2D,\n");
		str += output("\t0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7A,\n");
		str += output("\t0x7B, 0x7C, 0x35, 0x2B, 0x23, 0x39, 0x32, 0x33,\n");
		str += output("\t0x37, 0x57, 0x58, 0x59, 0x5A, 0x5B, 0x5C, 0x5D,\n");
		str += output("\t0x5E, 0x5F, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66,\n");
		str += output("\t0x67, 0x68, 0x69, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F,\n");
		str += output("\t0x70, 0x71, 0x72, 0x7D, 0x6A, 0x7E, 0x7F, 0x31,\n");
		str += output("\t0x34, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F, 0x40, 0x41,\n");
		str += output("\t0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49,\n");
		str += output("\t0x4A, 0x4B, 0x4C, 0x4E, 0x4F, 0x50, 0x51, 0x52,\n");
		str += output("\t0x53, 0x54, 0x55, 0x56, 0x2E, 0x60, 0x4D, 0x05,\n");
		str += output("\t0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,\n");
		str += output("\t0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D, 0x8E, 0x8F,\n");
		str += output("\t0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,\n");
		str += output("\t0x98, 0x99, 0x9A, 0x9B, 0x9C, 0x9D, 0x9E, 0x9F,\n");
		str += output("\t0xA0, 0xA1, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7,\n");
		str += output("\t0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF,\n");
		str += output("\t0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7,\n");
		str += output("\t0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF,\n");
		str += output("\t0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7,\n");
		str += output("\t0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF,\n");
		str += output("\t0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7,\n");
		str += output("\t0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF,\n");
		str += output("\t0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7,\n");
		str += output("\t0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF,\n");
		str += output("\t0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7,\n");
		str += output("\t0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF\n");
		str += output("};\n");
		real_output(str.c_str());
		output_storage("\n");
	}
	if(gen_full_ebcdic) {
		output_storage("\n/* ASCII to EBCDIC table */\n");
		string str = output("static const unsigned char\tcob_ascii_ebcdic[256] = {\n");
		str += output("\t0x00, 0x01, 0x02, 0x03, 0x37, 0x2D, 0x2E, 0x2F,\n");
		str += output("\t0x16, 0x05, 0x25, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,\n");
		str += output("\t0x10, 0x11, 0x12, 0x13, 0x3C, 0x3D, 0x32, 0x26,\n");
		str += output("\t0x18, 0x19, 0x3F, 0x27, 0x1C, 0x1D, 0x1E, 0x1F,\n");
		str += output("\t0x40, 0x5A, 0x7F, 0x7B, 0x5B, 0x6C, 0x50, 0x7D,\n");
		str += output("\t0x4D, 0x5D, 0x5C, 0x4E, 0x6B, 0x60, 0x4B, 0x61,\n");
		str += output("\t0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7,\n");
		str += output("\t0xF8, 0xF9, 0x7A, 0x5E, 0x4C, 0x7E, 0x6E, 0x6F,\n");
		str += output("\t0x7C, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7,\n");
		str += output("\t0xC8, 0xC9, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6,\n");
		str += output("\t0xD7, 0xD8, 0xD9, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6,\n");
		str += output("\t0xE7, 0xE8, 0xE9, 0xAD, 0xE0, 0xBD, 0x5F, 0x6D,\n");
		str += output("\t0x79, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,\n");
		str += output("\t0x88, 0x89, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96,\n");
		str += output("\t0x97, 0x98, 0x99, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6,\n");
		str += output("\t0xA7, 0xA8, 0xA9, 0xC0, 0x6A, 0xD0, 0xA1, 0x07,\n");
		str += output("\t0x68, 0xDC, 0x51, 0x42, 0x43, 0x44, 0x47, 0x48,\n");
		str += output("\t0x52, 0x53, 0x54, 0x57, 0x56, 0x58, 0x63, 0x67,\n");
		str += output("\t0x71, 0x9C, 0x9E, 0xCB, 0xCC, 0xCD, 0xDB, 0xDD,\n");
		str += output("\t0xDF, 0xEC, 0xFC, 0xB0, 0xB1, 0xB2, 0x3E, 0xB4,\n");
		str += output("\t0x45, 0x55, 0xCE, 0xDE, 0x49, 0x69, 0x9A, 0x9B,\n");
		str += output("\t0xAB, 0x9F, 0xBA, 0xB8, 0xB7, 0xAA, 0x8A, 0x8B,\n");
		str += output("\t0xB6, 0xB5, 0x62, 0x4F, 0x64, 0x65, 0x66, 0x20,\n");
		str += output("\t0x21, 0x22, 0x70, 0x23, 0x72, 0x73, 0x74, 0xBE,\n");
		str += output("\t0x76, 0x77, 0x78, 0x80, 0x24, 0x15, 0x8C, 0x8D,\n");
		str += output("\t0x8E, 0x41, 0x06, 0x17, 0x28, 0x29, 0x9D, 0x2A,\n");
		str += output("\t0x2B, 0x2C, 0x09, 0x0A, 0xAC, 0x4A, 0xAE, 0xAF,\n");
		str += output("\t0x1B, 0x30, 0x31, 0xFA, 0x1A, 0x33, 0x34, 0x35,\n");
		str += output("\t0x36, 0x59, 0x08, 0x38, 0xBC, 0x39, 0xA0, 0xBF,\n");
		str += output("\t0xCA, 0x3A, 0xFE, 0x3B, 0x04, 0xCF, 0xDA, 0x14,\n");
		str += output("\t0xE1, 0x8F, 0x46, 0x75, 0xFD, 0xEB, 0xEE, 0xED,\n");
		str += output("\t0x90, 0xEF, 0xB3, 0xFB, 0xB9, 0xEA, 0xBB, 0xFF\n");
		str += output("};\n");
		if(gen_full_ebcdic > 1) {
			int i = lookup_attr(COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL, 0);
			str += output
				   ("static cob_field f_ascii_ebcdic(256, (cob_u8_ptr)cob_ascii_ebcdic, &%s%d);\n",
					CB_PREFIX_ATTR, i);
		}
		real_output(str.c_str());
		output_storage("\n");
	}
	if(gen_ebcdic_ascii) {
		output_storage("\n/* EBCDIC to ASCII table */\n");
		string str = output("static const unsigned char\tcob_ebcdic_ascii[256] = {\n");
		str += output("\t0x00, 0x01, 0x02, 0x03, 0xEC, 0x09, 0xCA, 0x7F,\n");
		str += output("\t0xE2, 0xD2, 0xD3, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,\n");
		str += output("\t0x10, 0x11, 0x12, 0x13, 0xEF, 0xC5, 0x08, 0xCB,\n");
		str += output("\t0x18, 0x19, 0xDC, 0xD8, 0x1C, 0x1D, 0x1E, 0x1F,\n");
		str += output("\t0xB7, 0xB8, 0xB9, 0xBB, 0xC4, 0x0A, 0x17, 0x1B,\n");
		str += output("\t0xCC, 0xCD, 0xCF, 0xD0, 0xD1, 0x05, 0x06, 0x07,\n");
		str += output("\t0xD9, 0xDA, 0x16, 0xDD, 0xDE, 0xDF, 0xE0, 0x04,\n");
		str += output("\t0xE3, 0xE5, 0xE9, 0xEB, 0x14, 0x15, 0x9E, 0x1A,\n");
		str += output("\t0x20, 0xC9, 0x83, 0x84, 0x85, 0xA0, 0xF2, 0x86,\n");
		str += output("\t0x87, 0xA4, 0xD5, 0x2E, 0x3C, 0x28, 0x2B, 0xB3,\n");
		str += output("\t0x26, 0x82, 0x88, 0x89, 0x8A, 0xA1, 0x8C, 0x8B,\n");
		str += output("\t0x8D, 0xE1, 0x21, 0x24, 0x2A, 0x29, 0x3B, 0x5E,\n");
		str += output("\t0x2D, 0x2F, 0xB2, 0x8E, 0xB4, 0xB5, 0xB6, 0x8F,\n");
		str += output("\t0x80, 0xA5, 0x7C, 0x2C, 0x25, 0x5F, 0x3E, 0x3F,\n");
		str += output("\t0xBA, 0x90, 0xBC, 0xBD, 0xBE, 0xF3, 0xC0, 0xC1,\n");
		str += output("\t0xC2, 0x60, 0x3A, 0x23, 0x40, 0x27, 0x3D, 0x22,\n");
		str += output("\t0xC3, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,\n");
		str += output("\t0x68, 0x69, 0xAE, 0xAF, 0xC6, 0xC7, 0xC8, 0xF1,\n");
		str += output("\t0xF8, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F, 0x70,\n");
		str += output("\t0x71, 0x72, 0xA6, 0xA7, 0x91, 0xCE, 0x92, 0xA9,\n");
		str += output("\t0xE6, 0x7E, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78,\n");
		str += output("\t0x79, 0x7A, 0xAD, 0xA8, 0xD4, 0x5B, 0xD6, 0xD7,\n");
		str += output("\t0x9B, 0x9C, 0x9D, 0xFA, 0x9F, 0xB1, 0xB0, 0xAC,\n");
		str += output("\t0xAB, 0xFC, 0xAA, 0xFE, 0xE4, 0x5D, 0xBF, 0xE7,\n");
		str += output("\t0x7B, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,\n");
		str += output("\t0x48, 0x49, 0xE8, 0x93, 0x94, 0x95, 0xA2, 0xED,\n");
		str += output("\t0x7D, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F, 0x50,\n");
		str += output("\t0x51, 0x52, 0xEE, 0x96, 0x81, 0x97, 0xA3, 0x98,\n");
		str += output("\t0x5C, 0xF0, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58,\n");
		str += output("\t0x59, 0x5A, 0xFD, 0xF5, 0x99, 0xF7, 0xF6, 0xF9,\n");
		str += output("\t0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,\n");
		str += output("\t0x38, 0x39, 0xDB, 0xFB, 0x9A, 0xF4, 0xEA, 0xFF\n");
		str += output("};\n");
		if(gen_ebcdic_ascii > 1) {
			int i = lookup_attr(COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL, 0);
			str += output
				   ("static cob_field f_ebcdic_ascii(256, (cob_u8_ptr)cob_ebcdic_ascii, &%s%d);\n",
					CB_PREFIX_ATTR, i);
		}
		real_output(str.c_str());
		output_storage("\n");
	}
	if(gen_native) {
		output_storage("\n/* NATIVE table */\n");
		string str = output("static const unsigned char\tcob_native[256] = {\n");
		str += output("\t0, 1, 2, 3, 4, 5, 6, 7,\n");
		str += output("\t8, 9, 10, 11, 12, 13, 14, 15,\n");
		str += output("\t16, 17, 18, 19, 20, 21, 22, 23,\n");
		str += output("\t24, 25, 26, 27, 28, 29, 30, 31,\n");
		str += output("\t32, 33, 34, 35, 36, 37, 38, 39,\n");
		str += output("\t40, 41, 42, 43, 44, 45, 46, 47,\n");
		str += output("\t48, 49, 50, 51, 52, 53, 54, 55,\n");
		str += output("\t56, 57, 58, 59, 60, 61, 62, 63,\n");
		str += output("\t64, 65, 66, 67, 68, 69, 70, 71,\n");
		str += output("\t72, 73, 74, 75, 76, 77, 78, 79,\n");
		str += output("\t80, 81, 82, 83, 84, 85, 86, 87,\n");
		str += output("\t88, 89, 90, 91, 92, 93, 94, 95,\n");
		str += output("\t96, 97, 98, 99, 100, 101, 102, 103,\n");
		str += output("\t104, 105, 106, 107, 108, 109, 110, 111,\n");
		str += output("\t112, 113, 114, 115, 116, 117, 118, 119,\n");
		str += output("\t120, 121, 122, 123, 124, 125, 126, 127,\n");
		str += output("\t128, 129, 130, 131, 132, 133, 134, 135,\n");
		str += output("\t136, 137, 138, 139, 140, 141, 142, 143,\n");
		str += output("\t144, 145, 146, 147, 148, 149, 150, 151,\n");
		str += output("\t152, 153, 154, 155, 156, 157, 158, 159,\n");
		str += output("\t160, 161, 162, 163, 164, 165, 166, 167,\n");
		str += output("\t168, 169, 170, 171, 172, 173, 174, 175,\n");
		str += output("\t176, 177, 178, 179, 180, 181, 182, 183,\n");
		str += output("\t184, 185, 186, 187, 188, 189, 190, 191,\n");
		str += output("\t192, 193, 194, 195, 196, 197, 198, 199,\n");
		str += output("\t200, 201, 202, 203, 204, 205, 206, 207,\n");
		str += output("\t208, 209, 210, 211, 212, 213, 214, 215,\n");
		str += output("\t216, 217, 218, 219, 220, 221, 222, 223,\n");
		str += output("\t224, 225, 226, 227, 228, 229, 230, 231,\n");
		str += output("\t232, 233, 234, 235, 236, 237, 238, 239,\n");
		str += output("\t240, 241, 242, 243, 244, 245, 246, 247,\n");
		str += output("\t248, 249, 250, 251, 252, 253, 254, 255\n");
		str += output("};\n");
		if(gen_native > 1) {
			int i = lookup_attr(COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL, 0);
			str += output
				   ("static cob_field f_native(256, (cob_u8_ptr)cob_native, &%s%d);\n",
					CB_PREFIX_ATTR, i);
		}
		real_output(str.c_str());
		output_storage("\n");
	}

	/* Strings */
	if(string_cache) {
		output_storage("\n/* Strings */\n");
		string_cache = list_reverse(string_cache);
		string str("");
		for(string_list * stp = string_cache; stp; stp = stp->next) {
			str += output("static const char %s%d[]\t= \"%s\";\n",
						  CB_PREFIX_STRING, stp->id, stp->text);
		}
		real_output(str.c_str());
		output_storage("\n");
	}

	/* Optimizer output */
	for(cb_optim optidx = COB_OPTIM_MIN; optidx < COB_OPTIM_MAX; optidx = (cb_optim)((int)optidx + 1)) {
		if(optimize_defs[optidx]) {
			cob_gen_optim(optidx);
			output_storage("\n");
		}
	}

	if(literal_cache) {
		output_storage("\t/* Decimal constants */\n");
		for(literal_list * m = literal_cache; m; m = m->next) {
			if(CB_TREE_CLASS(m->x) == CB_CLASS_NUMERIC
					&& m->make_decimal) {
				output_storage("static\tcob_decimal\t%s%d;\n", CB_PREFIX_DEC_FIELD, m->id);
				output_storage("static\tcob_decimal\t*%s%d = NULL;\n", CB_PREFIX_DEC_CONST, m->id);
			}
		}
	}
	/* Clean up by clearing these */
	attr_cache = NULL;
	literal_cache = NULL;
	string_cache = NULL;
	string_id = 1;
}

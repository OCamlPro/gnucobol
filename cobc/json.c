/*
   Copyright (C) 2001-2019 Free Software Foundation, Inc.

   Authors:
   Fabrice Le Fessant

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
   along with GnuCOBOL.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "cobc.h"
#include "tree.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define MAX_INDENT 1000
static char*indents[MAX_INDENT+1];
static FILE* oc = NULL;

struct json_record {
  int indent;
  int full;
  int has_field;
};

static int output_locations = 0;

#define RECORD_BEGIN(r, set_indent)                     \
  if( indent > MAX_INDENT ){                            \
    fprintf(oc, "null");                                \
    return;                                             \
    } else {                                            \
    struct json_record r;                               \
    r.indent = set_indent+2;                            \
    r.has_field = 0;                                    \
    r.full = full;                                      \
    fprintf(oc, "{\n");

#define RECORD_FIELD(r, name, printer, value)                   \
    if( r.has_field ) fprintf(oc, ",\n");                       \
    else { r.has_field = 1; }                                   \
    fprintf(oc, "%s\"%s\": ", indents[r.indent], name);         \
    print_##printer(value, r.indent, 1);

// This one is used to pass full=0 as argument, usually to tell
// the printer not to go deep inside the record
#define RECORD_FIELD0(r, name, printer, value)                   \
    if( r.has_field ) fprintf(oc, ",\n");                       \
    else { r.has_field = 1; }                                   \
    fprintf(oc, "%s\"%s\": ", indents[r.indent], name);         \
    print_##printer(value, r.indent, 0);

#define RECORD_FIELDB(r, name, value)                           \
 if( value ){                                                   \
    if( r.has_field ) fprintf(oc, ",\n");                       \
    else { r.has_field = 1; }                                   \
    fprintf(oc, "%s\"%s\": true", indents[r.indent], name);     \
 }

#define RECORD_FIELDP(r, name, printer, value)                  \
  if( value != NULL ){                                          \
    RECORD_FIELD(r, name, printer, value);                      \
  }

// This one is used to pass full=0 as argument, usually to tell
// the printer not to go deep inside the record
#define RECORD_FIELDP0(r, name, printer, value)                  \
  if( value != NULL ){                                          \
    RECORD_FIELD0(r, name, printer, value);                     \
  }

#define RECORD_FIELDNZ(r, name, printer, value)                  \
    if( value != 0 ){                                            \
      RECORD_FIELD(r, name, printer, value);                     \
    }

#define RECORD_TREE( r, name, t)                                        \
    RECORD_FIELD(r, "node", string, name);                           \
    r.has_field = 0;                                                 \
    RECORD_FIELD(r, "uid", pointer, (cb_tree) t);                    \
    if(output_locations){                                               \
    RECORD_FIELDP(r, "source_file", string, ((cb_tree) t)->source_file); \
    RECORD_FIELDNZ(r, "source_line", int, ((cb_tree) t)->source_line); \
    RECORD_FIELDNZ(r, "source_column", int, ((cb_tree) t)->source_column); \
  }

#define RECORD_END(r)                           \
  if( r.has_field ) fprintf( oc, "\n" );        \
  fprintf(oc, "%s}", indents[r.indent-2]);      \
  }

static void print_cb_program ( const struct cb_program *p, int indent, int full );
static void print_cb_tree ( const cb_tree t, int indent, int full );
static void print_cb_field ( const struct cb_field * t, int indent, int full);
static void print_cb_label ( struct cb_label * f, int indent, int full );

static void print_pointer ( void* p, int indent, int full )
{
  fprintf(oc, "\"%p\"", p);
}

static void print_int ( int p, int indent, int full )
{
  fprintf(oc, "%d", p);
}

static void print_uint ( unsigned int p, int indent, int full )
{
  fprintf(oc, "%d", p);
}

static void print_json_char ( unsigned char c )
{
  switch( c ){
  case '"':
  case '\\':
    fprintf(oc, "\\%c", c);
  default:
    if( c > 31 && c < 127 ){
    fprintf(oc, "%c", c);
    } else {
      fprintf(oc, "\\u%04x", c);
    }
  }
}

static void print_uchar ( unsigned char c, int indent, int full )
{
  fprintf(oc, "\"");
  print_json_char( c );
  fprintf(oc, "\"");
}

static void print_intchar ( int c, int indent, int full )
{
  if( c>=0 && c<256 ){
    fprintf(oc, "\"");
    print_json_char( c );
    fprintf(oc, "\"");
  } else {
    fprintf(oc, "%d", c);
  }
}

static void print_string ( const char *p, int indent, int full )
{
  int len = strlen( p );
  int i;
  fprintf(oc, "\"");
  for( i = 0; i<len; i++){
    print_json_char( p[i] );
  }
  fprintf(oc, "\"");
}

static void print_ustring ( const unsigned char *p, int indent, int full )
{
  fprintf(oc, "\"%s\"", p); // TODO: json escape string
}

static void print_cb_tag ( enum cb_tag tag, int indent, int full )
{
  print_string(cb_tag_str(tag), indent, full);
}

static void print_cb_category ( enum cb_category tag, int indent, int full )
{
  print_string(cb_category_str(tag), indent, full);
}

static void print_cb_nested_list ( struct nested_list* t, int indent, int full )
{
  RECORD_BEGIN(r, indent);
  RECORD_FIELD(r, "next", cb_nested_list, t->next);
  RECORD_FIELD(r, "nested_prog", cb_program, t->nested_prog);
  RECORD_END(r);
}

static void print_cb_list( struct cb_list *l, int indent, int full )
{
  RECORD_BEGIN( r, indent);
  RECORD_TREE( r, "list", l );
  RECORD_FIELDP( r, "value", cb_tree, l->value );
  if( l->purpose != NULL ){
    RECORD_FIELDNZ( r, "purpose", intchar, CB_PURPOSE_INT(l) );
  }
  RECORD_FIELDP( r, "chain", cb_tree, l->chain );
  RECORD_FIELDNZ( r, "sizes", int, l->sizes );
  RECORD_END( r);
}

static void print_cb_local_filename( struct local_filename *l, int indent, int full )
{
  RECORD_BEGIN( r, indent);
  RECORD_FIELDP( r, "local_name", string, l->local_name );
  RECORD_FIELDP( r, "local_include_name", string, l->local_include_name );
  // RECORD_FIELD( r, "local_fp", cb_FILE, l->local_fp );
  RECORD_FIELDP( r, "next", cb_local_filename, l->next );
  RECORD_END( r);
}

static void print_cb_key ( struct cb_key * l, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_FIELDP( r, "key", cb_tree, l->key );
  RECORD_FIELDP( r, "ref", cb_tree, l->ref );
  RECORD_FIELDP( r, "val", cb_tree, l->val );
  RECORD_FIELD( r, "dir", int, l->dir );
  RECORD_END( r );
}

static void print_cb_cd ( struct cb_cd * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "cd", f);
  RECORD_FIELDP( r, "name", string, f->name );
  RECORD_FIELDP( r, "record", cb_field, f->record );
  RECORD_FIELDP( r, "debug_section", cb_label, f->debug_section );
  RECORD_FIELD( r, "flag_field_debug", int, f->flag_field_debug );
  RECORD_END( r );
}

static void print_cb_const ( struct cb_const * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "const", f);
  RECORD_FIELDP( r, "val", string, f->val );
  RECORD_END( r );
}

static void print_cb_integer ( struct cb_integer * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "integer", f);
  RECORD_FIELD( r, "val", int, f->val );
  RECORD_END( r );
}

static void print_cb_binary_op ( struct cb_binary_op * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "binary_op", f);
  RECORD_FIELDP( r, "x", cb_tree, f->x );
  RECORD_FIELDP( r, "y", cb_tree, f->y );
  RECORD_FIELD( r, "op", intchar, f->op );
  RECORD_FIELD( r, "flag", uint, f->flag );
  RECORD_END( r );
}

static void print_cb_cast ( struct cb_cast * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "cast", f);
  RECORD_FIELDP( r, "val", cb_tree, f->val );
  RECORD_FIELD( r, "cast_type", int, f->cast_type ); // TODO:	enum cb_cast_type	cast_type;
  RECORD_END( r );
}

static void print_cb_intrinsic ( struct cb_intrinsic * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "intrinsic", f);
  RECORD_FIELDP( r, "name", cb_tree, f->name );
  RECORD_FIELDP( r, "args", cb_tree, f->args );
  RECORD_FIELDP( r, "intr_field", cb_tree, f->intr_field );
#if 0
  // TODO
	const struct cb_intrinsic_table	*intr_tab;	
#endif
        RECORD_FIELDP( r, "offset", cb_tree, f->offset );
        RECORD_FIELDP( r, "length", cb_tree, f->length );
        RECORD_FIELD( r, "isuser", int, f->isuser );
  RECORD_END( r );
}

static void print_cb_xrefs( struct cb_xref_elem *x, int indent, int full )
{
  int first = 1;
  fprintf(oc, "[\n");
  while( x != NULL ){
    if( !first ){
      fprintf(oc,",\n");
      first = 0;
    }
    RECORD_BEGIN ( r, indent+2 );
    RECORD_FIELD ( r, "line", int, x->line );
    RECORD_FIELD ( r, "receive", int, x->receive );
    RECORD_END ( r );
    x = x->next;
  }
  fprintf(oc, "]");
}

static void print_cb_xref( const struct cb_xref *x, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_FIELD ( r, "refs", cb_xrefs, x->head );
  RECORD_FIELD ( r, "skip", uint, x->skip );
  RECORD_END ( r );
}

static void print_cb_funcall_args ( struct cb_funcall * f, int indent, int full )
{
  int i;
  fprintf(oc, "[\n");
  for( i = 0; i < f->argc; i++ ){
    if( i>0 ){
      fprintf(oc,",\n");
    }
    fprintf(oc, "%s",indents[indent+2]);
    if( i == 1 && f->name[0] == '$' && f->name[1] == ':' ){
      // TODO: this is wrong but necessary, see comment in typeck.c:cb_build_cond
      RECORD_BEGIN(r, indent);
      RECORD_FIELD(r, "node", string, "call_bin_op");
      RECORD_FIELD(r, "uid", pointer, f->argv[i]);
      if(output_locations){
        RECORD_FIELDP(r, "source_file", string, ((cb_tree) f)->source_file);
        RECORD_FIELDNZ(r, "source_line", int, ((cb_tree) f)->source_line);
        RECORD_FIELDNZ(r, "source_column", int, ((cb_tree) f)->source_column);
      }
      RECORD_FIELD(r, "op", uint, (unsigned long)(f->argv[i]));
      RECORD_END( r);
    } else {
      print_cb_tree(f->argv[i], indent + 2, 1 );
    }
  }
  fprintf(oc, "]");
}

static void print_cb_funcall ( struct cb_funcall * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "funcall", f);
  RECORD_FIELDP( r, "name", string, f->name );
  RECORD_FIELDP( r, "argv", cb_funcall_args, f ); //	cb_tree	 argv[11];	is varcnt used too ?
  RECORD_FIELD( r, "argc", int, f->argc );
  RECORD_FIELD( r, "varcnt", int, f->varcnt );
  RECORD_FIELD( r, "screenptr", uint, f->screenptr );
  RECORD_FIELD( r, "nolitcast", uint, f->nolitcast );
  RECORD_END( r );
}

static void print_cb_literal ( struct cb_literal * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "literal", f);
  RECORD_FIELD( r, "data", ustring, f->data );
  RECORD_FIELD( r, "size", uint, f->size );
  RECORD_FIELDNZ( r, "scale", int, f->scale );
  RECORD_FIELDNZ( r, "llit", uint, f->llit );
  RECORD_FIELDNZ( r, "sign", int, f->sign );
  RECORD_FIELDNZ( r, "all", int, f->all );
  RECORD_END( r );
}

static void print_cb_decimal ( struct cb_decimal * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "decimal", f);
  RECORD_FIELD( r, "id", uint, f->id );
  RECORD_END( r );
}

static void print_cb_report ( struct cb_report * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "report", f);
  RECORD_FIELDP( r, "name", string, f->name );
  RECORD_FIELDP( r, "cname", string, f->cname );
#if 0
  // TODO
	struct cb_file		*file;		
	cb_tree			line_counter;	
	cb_tree			page_counter;	
	cb_tree			code_clause;	
	cb_tree			controls;		
	cb_tree			t_lines;		
	cb_tree			t_columns;		
	cb_tree			t_heading;		
	cb_tree			t_first_detail;	
	cb_tree			t_last_control;	
	cb_tree			t_last_detail;	
	cb_tree			t_footing;		
	struct cb_field	*t_heading_final;
	struct cb_field	*t_footing_final;
	int			lines;				
	int			columns;			
	int			heading;			
	int			first_detail;		
	int			last_control;		
	int			last_detail;		
	int			footing;			
	struct cb_field		*records;	
	int			num_lines;			
	struct cb_field		**line_ids;	
	int			num_sums;			
	struct cb_field		**sums;		
	int			rcsz;				
	int			id;					
	int			sum_exec;			
	unsigned int		control_final:1;
	unsigned int		global:1;	
	unsigned int		has_declarative:1;
	unsigned int		has_detail:1;	
	unsigned int		has_source_move:1;
	unsigned int		was_checked:1;
#endif
  RECORD_END( r );
}

static void print_cb_alphabet_name ( struct cb_alphabet_name * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "alphabet_name", f);
  RECORD_FIELDP( r, "name", string, f->name );
  RECORD_FIELDP( r, "cname", string, f->cname );
#if 0
  // TODO
	cb_tree			custom_list;	
	unsigned int		alphabet_target;	
	unsigned int		alphabet_type;	
	int			low_val_char;	
	int			high_val_char;	
	int			values[256];	
	int			alphachr[256];	

#endif
  RECORD_END( r );
}

static void print_cb_locale_name ( struct cb_locale_name * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "locale_name", f);
  RECORD_FIELDP( r, "name", string, f->name );
  RECORD_FIELDP( r, "cname", string, f->cname );
  RECORD_FIELDP( r, "list", cb_tree, f->list );
  RECORD_END( r );
}

static void print_cb_class_name ( struct cb_class_name * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "class_name", f);
  RECORD_FIELDP( r, "name", string, f->name );
  RECORD_FIELDP( r, "cname", string, f->cname );
  RECORD_FIELDP( r, "list", cb_tree, f->list );
  RECORD_END( r );
}

static void print_cb_system_name ( struct cb_system_name * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "system_name", f);
  RECORD_FIELDP( r, "value", cb_tree, f->value );
  RECORD_FIELD( r, "category", int, f->category ); // TODO 	enum cb_system_name_category	category;	
  RECORD_FIELD( r, "token", int, f->token );
  RECORD_END( r );
}

static void print_cb_string ( struct cb_string * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "string", f);
  RECORD_FIELDP( r, "data", ustring, f->data );
  RECORD_FIELD( r, "size", int, f->size ); // size_t
  RECORD_END( r );
}

static void print_cb_assign ( struct cb_assign * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "assign", f);
  RECORD_FIELDP( r, "var", cb_tree, f->var );
  RECORD_FIELDP( r, "val", cb_tree, f->val );
  RECORD_END( r );
}

static void print_cb_initialize ( struct cb_initialize * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "initialize", f);
  RECORD_FIELDP( r, "var", cb_tree, f->var );
  RECORD_FIELDP( r, "val", cb_tree, f->val );
  RECORD_FIELDP( r, "rep", cb_tree, f->rep );
  RECORD_FIELD( r, "flag_default", uint, f->flag_default );
  RECORD_FIELD( r, "flag_init_statement", uint, f->flag_init_statement );
  RECORD_FIELD( r, "flag_no_filler_init", uint, f->flag_no_filler_init );
  RECORD_FIELD( r, "padding", uint, f->padding );
  RECORD_END( r );
}

static void print_cb_search ( struct cb_search * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "search", f);
  RECORD_FIELDP( r, "table", cb_tree, f->table );
  RECORD_FIELDP( r, "var", cb_tree, f->var );
#ifdef COBC_VERSION4
  RECORD_FIELDP( r, "end_stmt", cb_tree, f->end_stmt );
#else
  RECORD_FIELDP( r, "end_stmt", cb_tree, f->at_end );
#endif
  RECORD_FIELDP( r, "whens", cb_tree, f->whens );
  RECORD_FIELD( r, "flag_all", int, f->flag_all );
  RECORD_END( r );
}

static void print_cb_call ( struct cb_call * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "call", f);
  RECORD_FIELDP( r, "name", cb_tree, f->name );
  RECORD_FIELDP( r, "args", cb_tree, f->args );
  RECORD_FIELDP( r, "stmt1", cb_tree, f->stmt1 );
  RECORD_FIELDP( r, "stmt2", cb_tree, f->stmt2 );
  RECORD_FIELDP( r, "call_returning", cb_tree, f->call_returning );
  RECORD_FIELD( r, "is_system", uint, f->is_system );
  RECORD_FIELD( r, "convention", int, f->convention );
  RECORD_END( r );
}

static void print_cb_goto ( struct cb_goto * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "goto", f);
  RECORD_FIELDP( r, "target", cb_tree, f->target );
  RECORD_FIELDP( r, "depending", cb_tree, f->depending );
  RECORD_END( r );
}

static void print_cb_if ( struct cb_if * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "if", f);
  RECORD_FIELDP( r, "test", cb_tree, f->test );
  RECORD_FIELDP( r, "stmt1", cb_tree, f->stmt1 );
  RECORD_FIELDP( r, "stmt2", cb_tree, f->stmt2 );
  RECORD_FIELD( r, "is_if", uint, f->is_if );
  RECORD_END( r );
}

static void print_cb_label_simple ( struct cb_label * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "label_simple", f);
  RECORD_FIELDP( r, "name", string, f->name );
  RECORD_END( r );
}

static void print_cb_label ( struct cb_label * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "label", f);
  RECORD_FIELDP( r, "name", string, f->name );
  RECORD_FIELDP( r, "orig_name", string, f->orig_name );
  RECORD_FIELDP( r, "section", cb_label_simple, f->section );
  RECORD_FIELDP( r, "debug_section", cb_label_simple, f->debug_section );
#if 0
  // TODO
	struct cb_para_label	*para_label;		
	struct cb_xref		xref;			
	cb_tree			exit_label;		
	struct cb_alter_id	*alter_gotos;		
	int			id;			
	int			section_id;		
	int			segment;		

	unsigned int		flag_section		: 1;	
	unsigned int		flag_entry		: 1;	
	unsigned int		flag_begin		: 1;	
	unsigned int		flag_return		: 1;	
	unsigned int		flag_real_label		: 1;	
	unsigned int		flag_global		: 1;	
	unsigned int		flag_declarative_exit	: 1;	
	unsigned int		flag_declaratives	: 1;	

	unsigned int		flag_fatal_check	: 1;	
	unsigned int		flag_dummy_section	: 1;	
	unsigned int		flag_dummy_paragraph	: 1;	
	unsigned int		flag_dummy_exit		: 1;	
	unsigned int		flag_next_sentence	: 1;	
	unsigned int		flag_default_handler	: 1;	
	unsigned int		flag_statement		: 1;	
	unsigned int		flag_first_is_goto	: 1;	

	unsigned int		flag_alter		: 1;	
	unsigned int		flag_debugging_mode	: 1;	
	unsigned int		flag_is_debug_sect	: 1;	
	unsigned int		flag_skip_label		: 1;	
	unsigned int		flag_entry_for_goto	: 1;	

#endif
  RECORD_END( r );
}

static void print_cb_perform ( struct cb_perform * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "perform", f);
  RECORD_FIELDP( r, "test", cb_tree, f->test );
  RECORD_FIELDP( r, "body", cb_tree, f->body );
  RECORD_FIELDP( r, "data", cb_tree, f->data );
  RECORD_FIELDP( r, "varying", cb_tree, f->varying );
  RECORD_FIELDP( r, "exit_label", cb_tree, f->exit_label );
  RECORD_FIELDP( r, "cycle_label", cb_tree, f->cycle_label );
  RECORD_FIELD( r, "perform_type", int, f->perform_type ); // TODO enum cb_perform_type	perform_type;
  RECORD_END( r );
}

static void print_cb_reference ( struct cb_reference * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "reference", f);
  RECORD_FIELDP( r, "chain", cb_tree, f->chain );
  RECORD_FIELDP0( r, "value", cb_tree, f->value );
  RECORD_FIELDP( r, "subs", cb_tree, f->subs );
  RECORD_FIELDP( r, "offset", cb_tree, f->offset );
  RECORD_FIELDP( r, "length", cb_tree, f->length );
  RECORD_FIELDP( r, "check", cb_tree, f->check );
#if 0
  // TODO
	struct cb_word		*word;		
	struct cb_label		*section;	
	struct cb_label		*paragraph;	
	struct cb_label		*debug_section;	
	size_t			hashval;	

	unsigned int		flag_receiving	: 1;	
	unsigned int		flag_all	: 1;	
	unsigned int		flag_in_decl	: 1;	
	unsigned int		flag_decl_ok	: 1;	
	unsigned int		flag_alter_code	: 1;	
	unsigned int		flag_debug_code	: 1;	
	unsigned int		flag_all_debug	: 1;	
	unsigned int		flag_target	: 1;	

	unsigned int		flag_optional	: 1;	
	unsigned int		flag_ignored	: 1;	
	unsigned int		flag_filler_ref	: 1;	
	unsigned int		flag_duped	: 1;	
#endif
  RECORD_END( r );
}

static void print_cb_statement ( struct cb_statement * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "statement", f);
  RECORD_FIELDP( r, "name", string, f->name );
  RECORD_FIELDP( r, "body", cb_tree, f->body );
  RECORD_FIELDP( r, "file", cb_tree, f->file );
  RECORD_FIELDP( r, "ex_handler", cb_tree, f->ex_handler );
  RECORD_FIELDP( r, "not_ex_handler", cb_tree, f->not_ex_handler );
  RECORD_FIELDP( r, "handler3", cb_tree, f->handler3 );
  RECORD_FIELDP( r, "null_check", cb_tree, f->null_check );
  RECORD_FIELDP( r, "debug_check", cb_tree, f->debug_check );
  RECORD_FIELDP( r, "debug_nodups", cb_tree, f->debug_nodups );
#ifdef COBC_VERSION4
  RECORD_FIELDP( r, "retry", cb_tree, f->retry );
#endif
#if 0
  // TODO
	struct cb_attr_struct	*attr_ptr;		
	enum cb_handler_type	handler_type;		
	unsigned int		flag_no_based	: 1;	
	unsigned int		flag_in_debug	: 1;	
	unsigned int		flag_merge	: 1;	
	unsigned int		flag_callback	: 1;	
	unsigned int		flag_implicit	: 1;	
	unsigned int		flag_retry_times: 1;	
	unsigned int		flag_retry_seconds: 1;	
	unsigned int		flag_retry_forever: 1;	
	unsigned int		flag_advancing_lock: 1;	
	unsigned int		flag_ignore_lock: 1;	
#endif
  RECORD_END( r );
}

static void print_cb_continue ( struct cb_continue * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "continue", f);
  RECORD_END( r );
}

static void print_cb_cancel ( struct cb_cancel * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "cancel", f);
  RECORD_FIELDP( r, "target", cb_tree, f->target );
  RECORD_END( r );
}

static void print_cb_alter ( struct cb_alter * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "alter", f);
  RECORD_FIELDP( r, "source", cb_tree, f->source );
  RECORD_FIELDP( r, "target", cb_tree, f->target );
  RECORD_END( r );
}

static void print_cb_set_attr ( struct cb_set_attr * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "set_attr", f);
  RECORD_FIELDP( r, "fld", cb_field, f->fld );
  RECORD_FIELD( r, "val_on", uint, f->val_on );
  RECORD_FIELD( r, "val_off", uint, f->val_off );
  RECORD_END( r );
}

static void print_cb_perform_varying ( struct cb_perform_varying * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "perform_varying", f);
  RECORD_FIELDP( r, "name", cb_tree, f->name );
  RECORD_FIELDP( r, "from", cb_tree, f->from );
  RECORD_FIELDP( r, "step", cb_tree, f->step );
  RECORD_FIELDP( r, "until", cb_tree, f->until );
  RECORD_END( r );
}

static void print_cb_picture ( struct cb_picture * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "picture", f);
  RECORD_FIELD( r, "orig", string, f->orig );
  RECORD_FIELD( r, "size", int, f->size );
  RECORD_FIELD( r, "lenstr", int, f->lenstr );
  RECORD_FIELD( r, "category", cb_category, f->category );
  RECORD_FIELD( r, "digits", uint, f->digits );
  RECORD_FIELD( r, "scale", int, f->scale );
  RECORD_FIELD( r, "have_sign", uint, f->have_sign );
  RECORD_FIELD( r, "flag_is_calculated", uint, f->flag_is_calculated );
#if 0
        // TODO
	cob_pic_symbol		*str;		
#endif
  RECORD_END( r );
}

#ifdef COBC_VERSION4
static void print_cb_vary ( struct cb_vary * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "vary", f);
  RECORD_FIELDP( r, "var", cb_tree, f->var );
  RECORD_FIELDP( r, "from", cb_tree, f->from );
  RECORD_FIELDP( r, "by", cb_tree, f->by );
  RECORD_END( r );
}
#endif

static void print_cb_direct ( struct cb_direct * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "direct", f);
  RECORD_FIELD( r, "line", string, f->line );
  RECORD_FIELD( r, "flag_is_direct", uint, f->flag_is_direct );
  RECORD_FIELD( r, "flag_new_line", uint, f->flag_new_line );
  RECORD_END( r );
}

static void print_cb_debug ( struct cb_debug * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "debug", f);
  RECORD_FIELDP( r, "target", cb_tree, f->target );
  RECORD_FIELDP( r, "value", string, f->value );
  RECORD_FIELDP( r, "fld", cb_tree, f->fld );
  RECORD_FIELD( r, "size", int, f->size );
  RECORD_END( r );
}

static void print_cb_debug_call ( struct cb_debug_call * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "debug_call", f);
  RECORD_FIELDP( r, "target", cb_label, f->target );
  RECORD_END( r );
}

static void print_cb_prototype ( struct cb_prototype * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "prototype", f);
  RECORD_FIELDP( r, "name", string, f->name );
  RECORD_FIELDP( r, "ext_name", string, f->ext_name );
  RECORD_FIELD( r, "type", int, f->type );
  RECORD_END( r );
}

static void print_cb_decimal_literal ( struct cb_decimal * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "decimal_literal", f);
  RECORD_FIELD( r, "id", uint, f->id );
  RECORD_END( r );
}

static void print_cb_ml_suppress ( struct cb_ml_suppress_clause * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "ml_suppress", f);
#if 0
  // TODO
	enum cb_ml_suppress_target	target;
	cb_tree				identifier;
	cb_tree				when_list;
	enum cb_ml_type		ml_type;
	/* If the target is TYPE, then the categories of items (of ML type
	   ml_type) to apply to */
	enum cb_ml_suppress_category	category;
#endif
  RECORD_END( r );
}

static void print_cb_ml_tree ( struct cb_ml_generate_tree * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "ml_tree", f);
#if 0
  // TODO
	cb_tree				name;
	enum cb_ml_type			type;
	cb_tree			        value;
	cb_tree				suppress_cond;
	int				id;
	struct cb_ml_generate_tree	*attrs;
	struct cb_ml_generate_tree	*parent;
	struct cb_ml_generate_tree	*children;
	struct cb_ml_generate_tree	*prev_sibling;
	struct cb_ml_generate_tree	*sibling;
#endif
  RECORD_END( r );
}

static void print_cb_ml_suppress_checks ( struct cb_ml_suppress_checks * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "ml_suppress_checks", f);
  RECORD_FIELDP( r, "tree", cb_ml_tree, f->tree );
  RECORD_END( r );
}

static void print_cb_file ( struct cb_file * f, int indent, int full )
{
  RECORD_BEGIN ( r, indent );
  RECORD_TREE( r, "file", f );
  RECORD_FIELD( r, "name", string, f->name );
  RECORD_FIELD( r, "cname", string, f->cname );

#if 0
        // TODO
	cb_tree			assign;			
	cb_tree			file_status;		
	cb_tree			sharing;		
	cb_tree			key;			
	cb_tree			password;			
	struct cb_key_component	*component_list;	
	struct cb_alt_key	*alt_key_list;		
	cb_tree			collating_sequence_key;	
	cb_tree			collating_sequence;	
	cb_tree			collating_sequence_n;	
	cb_tree			collating_sequence_keys;	
	
	cb_tree			description_entry;	
	struct cb_field		*record;		
	cb_tree			record_depending;	
	cb_tree			reports;		
	cb_tree			linage;			
	cb_tree			linage_ctr;		
	cb_tree			latfoot;		
	cb_tree			lattop;			
	cb_tree			latbot;			
	cb_tree			extfh;			
	struct cb_label		*handler;		
	struct cb_program	*handler_prog;		
	struct cb_label		*debug_section;		
	struct cb_alphabet_name	*code_set;		
	struct cb_list		*code_set_items;	
	struct cb_xref		xref;			
	char			*sql_name;		
	int			record_min;		
	int			record_max;		
	int			optional;		
	int			organization;		
	int			access_mode;		
	int			lock_mode;		
	int			fd_share_mode;		
	int			special;		
	int			same_clause;		
	int			max_sql_name_len;		
	int			sql_filler_id;		
	enum cb_assign_type	assign_type;		
	unsigned int		flag_finalized	: 1;	
	unsigned int		flag_external	: 1;	
	unsigned int		flag_ext_assign	: 1;	
	unsigned int		flag_fileid	: 1;	
	unsigned int		flag_global	: 1;	
	unsigned int		flag_fl_debug	: 1;	
	unsigned int		flag_line_adv	: 4;	
	unsigned int		flag_delimiter	: 1;	

	unsigned int		flag_report	: 1;	
	
	unsigned int		flag_check_record_varying_limits	: 1;
	unsigned int		flag_sql_xfd : 1;		
	unsigned int		flag_sql_trim_prefix : 1;	
	unsigned int		flag_sql_trim_dash : 1;		
	unsigned int		flag_sql_keep_filler : 1;	
	/* Whether the file's ASSIGN is like "ASSIGN word", not "ASSIGN
           EXTERNAL/DYNAMIC/USING/... word" */
	unsigned int		flag_assign_no_keyword : 1;
	unsigned int		flag_has_organization : 1;	
	unsigned int		flag_primary_dups : 1;	
#endif
        RECORD_END(r);
};

static void print_cb_tree_common ( const struct cb_tree_common *t, int indent, int full )
{
  RECORD_BEGIN(r, indent);
  RECORD_TREE( r, "cb_tree_common", t );
  RECORD_FIELD(r, "tag", cb_tag, ((cb_tree) t)->tag);
  RECORD_FIELD(r, "category", cb_category, ((cb_tree) t)->category);
  RECORD_END(r);
}

static
void print_cb_field ( const struct cb_field * t, int indent, int full)
{
  RECORD_BEGIN( r, indent);
  RECORD_TREE( r, "field", t );
  RECORD_FIELDP ( r, "name", string, t->name );
  if( full ){
  RECORD_FIELDP ( r, "ename", string, t->ename );
  RECORD_FIELDP ( r, "depending", cb_tree, t->depending );
  RECORD_FIELDP ( r, "values", cb_tree, t->values );
  RECORD_FIELDP ( r, "false_88", cb_tree, t->false_88 );
  RECORD_FIELDP ( r, "index_list", cb_tree, t->index_list );
  RECORD_FIELDP ( r, "external_form_identifier", cb_tree, t->external_form_identifier );
  RECORD_FIELDP0 ( r, "parent", cb_field, t->parent );
  RECORD_FIELDP ( r, "children", cb_field, t->children );
  RECORD_FIELDP ( r, "validation", cb_field, t->validation );
  RECORD_FIELDP ( r, "sister", cb_field, t->sister );
  RECORD_FIELDP0 ( r, "redefines", cb_field, t->redefines );
  RECORD_FIELDP ( r, "rename_thru", cb_field, t->rename_thru );
  RECORD_FIELDP0 ( r, "index_qual", cb_field, t->index_qual );
  RECORD_FIELDP ( r, "file", cb_file, t->file );
  RECORD_FIELDP ( r, "cd", cb_cd, t->cd );
  RECORD_FIELDP ( r, "keys", cb_key, t->keys );
  RECORD_FIELDP ( r, "pic", cb_picture, t->pic );
  RECORD_FIELDP ( r, "vsize", cb_field, t->vsize );
  RECORD_FIELDP ( r, "debug_section", cb_label, t->debug_section );
  RECORD_FIELDP ( r, "report", cb_report, t->report );
  RECORD_FIELDP ( r, "xref", cb_xref, &t->xref );

  RECORD_FIELDP ( r, "screen_line", cb_tree, t->screen_line );
  RECORD_FIELDP ( r, "screen_column", cb_tree, t->screen_column );
  RECORD_FIELDP ( r, "screen_from", cb_tree, t->screen_from );
  RECORD_FIELDP ( r, "screen_to", cb_tree, t->screen_to );
  RECORD_FIELDP ( r, "screen_foreg", cb_tree, t->screen_foreg );
  RECORD_FIELDP ( r, "screen_backg", cb_tree, t->screen_backg );
  RECORD_FIELDP ( r, "screen_prompt", cb_tree, t->screen_prompt );

  RECORD_FIELDP ( r, "report_source", cb_tree, t->report_source );
  RECORD_FIELDP ( r, "report_from", cb_tree, t->report_from );
  RECORD_FIELDP ( r, "report_sum_counter", cb_tree, t->report_sum_counter );
  RECORD_FIELDP ( r, "report_sum_list", cb_tree, t->report_sum_list );
  RECORD_FIELDP ( r, "report_sum_upon", cb_tree, t->report_sum_upon );
  RECORD_FIELDP ( r, "report_reset", cb_tree, t->report_reset );
  RECORD_FIELDP ( r, "report_control", cb_tree, t->report_control );
  RECORD_FIELDP ( r, "report_when", cb_tree_common, t->report_when );
  RECORD_FIELDP ( r, "report_column_list", cb_tree_common, t->report_column_list );
#ifdef COBC_VERSION4
  RECORD_FIELDP ( r, "report_vary_list", cb_tree_common, t->report_vary_list );
  RECORD_FIELDP ( r, "report_source_txt", string, t->report_source_txt );
  RECORD_FIELDP ( r, "report_field_name", string, t->report_field_name );
  if( t-> report_field_from != NULL) {
    RECORD_FIELDP ( r, "report_field_from", cb_field, t->report_field_from );
    RECORD_FIELD ( r, "report_field_offset", int, t->report_field_offset );
    RECORD_FIELD ( r, "report_field_size", int, t->report_field_size );
  }
#endif
  RECORD_FIELDP ( r, "external_definition", cb_tree, t->external_definition );
  RECORD_FIELDP ( r, "like_modifier", cb_tree, t->like_modifier );

  RECORD_FIELD ( r, "id", int, t->id );
  RECORD_FIELD ( r, "size", int, t->size );
  RECORD_FIELD ( r, "level", int, t->level );
  RECORD_FIELD ( r, "memory_size", int, t->memory_size );
#ifdef COBC_VERSION4
  RECORD_FIELD ( r, "compx_size", int, t->compx_size );
#endif
  RECORD_FIELD ( r, "offset", int, t->offset );
  RECORD_FIELD ( r, "occurs_min", int, t->occurs_min );
  RECORD_FIELD ( r, "occurs_max", int, t->occurs_max );
  RECORD_FIELD ( r, "indexes", int, t->indexes );
  RECORD_FIELD ( r, "count", int, t->count );
  RECORD_FIELD ( r, "mem_offset", int, t->mem_offset );
  RECORD_FIELD ( r, "nkeys", int, t->nkeys );
  RECORD_FIELD ( r, "param_num", int, t->param_num );
  RECORD_FIELD ( r, "screen_flag", uint, t->screen_flag ); // cob_flags_t
  RECORD_FIELD ( r, "report_flag", int, t->report_flag );
  RECORD_FIELD ( r, "report_line", int, t->report_line );
  RECORD_FIELD ( r, "report_column", int, t->report_column );
  RECORD_FIELD ( r, "report_num_col", int, t->report_num_col );
  RECORD_FIELD ( r, "report_decl_id", int, t->report_decl_id );
  RECORD_FIELD ( r, "report_source_id", int, t->report_source_id );
  RECORD_FIELD ( r, "step_count", int, t->step_count );
  RECORD_FIELD ( r, "next_group_line", int, t->next_group_line );
  RECORD_FIELD ( r, "vaddr", uint, t->vaddr );
  RECORD_FIELD ( r, "odo_level", uint, t->odo_level );

  RECORD_FIELD ( r, "index_type", uint, t->index_type ); // enum cb_index_type
  RECORD_FIELD ( r, "storage", uint, t->storage ); // enum cb_storage
  RECORD_FIELD ( r, "usage", uint, t->usage ); // enum cb_usage

#ifdef COBC_VERSION4
  RECORD_FIELDP ( r, "sql_name", string, t->sql_name );
  RECORD_FIELDP ( r, "sql_date_format", string, t->sql_date_format );
  RECORD_FIELDP ( r, "sql_when", string, t->sql_when );
  RECORD_FIELD ( r, "sql_filler_id", int, t->sql_filler_id );
  RECORD_FIELD ( r, "symtab", uint, t->symtab );
#endif

#if 0
  // TODO

	unsigned char flag_base;		
	unsigned char flag_external;		
	unsigned char flag_local_storage;	
	unsigned char flag_is_global;		

	unsigned int flag_local		: 1;	
	unsigned int flag_occurs	: 1;	
	unsigned int flag_sign_clause	: 1;	
	unsigned int flag_sign_separate	: 1;	
	unsigned int flag_sign_leading	: 1;	
	unsigned int flag_blank_zero	: 1;	
	unsigned int flag_justified	: 1;	
	unsigned int flag_binary_swap	: 1;	

	unsigned int flag_real_binary	: 1;	
	unsigned int flag_is_pointer	: 1;	
	unsigned int flag_item_78 	: 1;	/* Is a constant by 78 level,
										   01 CONSTANT or SYMBOLIC CONSTANT */
	unsigned int flag_any_length	: 1;	
	unsigned int flag_item_based	: 1;	
	unsigned int flag_is_external_form : 1;		
	unsigned int flag_filler	: 1;	
	unsigned int flag_synchronized	: 1;	

	unsigned int flag_invalid	: 1;	
	unsigned int flag_field		: 1;	
	unsigned int flag_chained	: 1;	
	unsigned int flag_data_set	: 1;	
	unsigned int flag_is_verified	: 1;	
	unsigned int flag_is_c_long	: 1;	
	unsigned int flag_is_pdiv_parm	: 1;	
	unsigned int flag_is_pdiv_opt	: 1;	

	unsigned int flag_indexed_by	: 1;	
	unsigned int flag_local_alloced	: 1;	
	unsigned int flag_no_init	: 1;	
	unsigned int flag_vsize_done	: 1;	
	unsigned int flag_vaddr_done	: 1;	
	unsigned int flag_odo_relative	: 1;	/* complex-odo: item address depends
							on size of a different (ODO) item */
	unsigned int flag_field_debug	: 1;	
	unsigned int flag_all_debug	: 1;	

	unsigned int flag_no_field	: 1;	
	unsigned int flag_any_numeric	: 1;	
	unsigned int flag_is_returning	: 1;	
	unsigned int flag_unbounded	: 1;	
	unsigned int flag_comp_1	: 1;	
	unsigned int flag_volatile	: 1;	
	unsigned int flag_constant	: 1;	
	unsigned int flag_internal_constant	: 1;	

	unsigned int flag_internal_register	: 1;	

	unsigned int flag_sql_binary	: 1;	
	unsigned int flag_sql_char	: 1;		
	unsigned int flag_sql_varchar : 1;		
	unsigned int flag_sql_numeric : 1;		
	unsigned int flag_sql_date : 1;			
	unsigned int flag_sql_time : 1;			
	unsigned int flag_sql_group : 1;		
	unsigned int flag_validated : 1;	
	unsigned int flag_usage_defined : 1;	

	unsigned int flag_sync_left : 1;	
	unsigned int flag_sync_right : 1;	
	unsigned int flag_sql_filler : 1;	
	unsigned int flag_sym_emitted: 1;	
	unsigned int flag_cob_field	: 1;	
	unsigned int flag_binary_assign: 1;	
	unsigned int flag_occurs_multi_col: 1;	
	unsigned int flag_set_col_offset: 1;	

	unsigned int flag_is_typedef : 1;	
	unsigned int flag_occurs_values: 1;	
#endif
  }
  RECORD_END(r);
}

static void print_cb_program ( const struct cb_program *p, int indent, int full )
{
  RECORD_BEGIN(r, indent);
  RECORD_TREE( r, "program", p );
        RECORD_FIELDP( r, "next_program", cb_program, p->next_program );
        RECORD_FIELDP( r, "next_program_ordered", cb_program, p->next_program_ordered );
        RECORD_FIELDP( r, "program_name", string, p->program_name );
        RECORD_FIELDP( r, "program_id", string, p->program_id );
        RECORD_FIELDP( r, "source_name", string, p->source_name );
        RECORD_FIELDP( r, "orig_program_id", string, p->orig_program_id );
	// struct cb_word   **word_table;  // Name hash table */

        RECORD_FIELDP( r, "local_include", cb_local_filename, p->local_include);
	RECORD_FIELDP( r, "nested_prog_list", cb_nested_list, p->nested_prog_list);
        RECORD_FIELDP( r, "common_prog_list", cb_nested_list, p->common_prog_list);

	RECORD_FIELDP( r, "entry_list", cb_tree, p->entry_list );
	RECORD_FIELDP( r, "entry_list_goto", cb_tree, p->entry_list_goto );
	RECORD_FIELDP( r, "file_list", cb_tree, p->file_list );
	RECORD_FIELDP( r, "cd_list", cb_tree, p->cd_list );
	RECORD_FIELDP( r, "exec_list", cb_tree, p->exec_list );
	RECORD_FIELDP( r, "label_list", cb_tree, p->label_list );
	RECORD_FIELDP( r, "reference_list", cb_tree, p->reference_list );
	RECORD_FIELDP( r, "alphabet_name_list", cb_tree, p->alphabet_name_list );
	RECORD_FIELDP( r, "symbolic_char_list", cb_tree, p->symbolic_char_list );
	RECORD_FIELDP( r, "class_name_list", cb_tree, p->class_name_list );
	RECORD_FIELDP( r, "parameter_list", cb_tree, p->parameter_list );
	RECORD_FIELDP( r, "locale_list", cb_tree, p->locale_list );
	RECORD_FIELDP( r, "global_list", cb_tree, p->global_list );
	RECORD_FIELDP( r, "report_list", cb_tree, p->report_list );
	RECORD_FIELDP( r, "alter_list", cb_tree, p->alter_list );
	RECORD_FIELDP( r, "debug_list", cb_tree, p->debug_list );
	RECORD_FIELDP( r, "cb_return_code", cb_tree, p->cb_return_code );
	RECORD_FIELDP( r, "cb_sort_return", cb_tree, p->cb_sort_return );
	RECORD_FIELDP( r, "cb_call_params", cb_tree, p->cb_call_params );
	RECORD_FIELDP( r, "mnemonic_spec_list", cb_tree, p->mnemonic_spec_list );
	RECORD_FIELDP( r, "class_spec_list", cb_tree, p->class_spec_list );
	RECORD_FIELDP( r, "interface_spec_list", cb_tree, p->interface_spec_list );
	RECORD_FIELDP( r, "function_spec_list", cb_tree, p->function_spec_list );
	RECORD_FIELDP( r, "user_spec_list", cb_tree, p->user_spec_list );
	RECORD_FIELDP( r, "program_spec_list", cb_tree, p->program_spec_list );
	RECORD_FIELDP( r, "property_spec_list", cb_tree, p->property_spec_list );
#if 0
        // TODO
	struct cb_alter_id	*alter_gotos;		// ALTER ids */
#endif
        RECORD_FIELDP( r, "working_storage", cb_field, p->working_storage );
        RECORD_FIELDP( r, "local_storage", cb_field, p->local_storage );
        RECORD_FIELDP( r, "linkage_storage", cb_field, p->linkage_storage );
        RECORD_FIELDP( r, "screen_storage", cb_field, p->screen_storage );
        RECORD_FIELDP( r, "report_storage", cb_field, p->report_storage );
	RECORD_FIELDP( r, "local_file_list", cb_tree, p->local_file_list );
	RECORD_FIELDP( r, "global_file_list", cb_tree, p->global_file_list );
#if 0
        // TODO
	struct handler_struct	global_handler[5];	// Global handlers */
#endif
	RECORD_FIELDP( r, "collating_sequence", cb_tree, p->collating_sequence );
	RECORD_FIELDP( r, "collating_sequence_n", cb_tree, p->collating_sequence_n );
	RECORD_FIELDP( r, "classification", cb_tree, p->classification );
	RECORD_FIELDP( r, "apply_commit", cb_tree, p->apply_commit );
	RECORD_FIELDP( r, "cursor_pos", cb_tree, p->cursor_pos );
	RECORD_FIELDP( r, "crt_status", cb_tree, p->crt_status );
	RECORD_FIELDP( r, "xml_code", cb_tree, p->xml_code );
	RECORD_FIELDP( r, "xml_event", cb_tree, p->xml_event );
	RECORD_FIELDP( r, "xml_information", cb_tree, p->xml_information );
	RECORD_FIELDP( r, "xml_namespace", cb_tree, p->xml_namespace );
	RECORD_FIELDP( r, "xml_nnamespace", cb_tree, p->xml_nnamespace );
	RECORD_FIELDP( r, "xml_namespace_prefix", cb_tree, p->xml_namespace_prefix );
	RECORD_FIELDP( r, "xml_nnamespace_prefix", cb_tree, p->xml_nnamespace_prefix );
	RECORD_FIELDP( r, "xml_ntext", cb_tree, p->xml_ntext );
	RECORD_FIELDP( r, "xml_text", cb_tree, p->xml_text );
	RECORD_FIELDP( r, "json_code", cb_tree, p->json_code );
	RECORD_FIELDP( r, "json_status", cb_tree, p->json_status );
	RECORD_FIELDP( r, "returning", cb_tree, p->returning );
#if 0
        // TODO
	struct cb_label		*all_procedure;		// DEBUGGING */
	struct cb_call_xref	call_xref;		// CALL Xref list */
	struct cb_ml_generate_tree	*ml_trees;	// XML GENERATE trees */
#endif
	RECORD_FIELDP( r, "extfh", string, p->extfh );

	RECORD_FIELD( r, "last_source_line", int, p-> last_source_line );

        RECORD_FIELD( r, "loop_counter", int, p->loop_counter );
        RECORD_FIELD( r, "decimal_index", uint, p->decimal_index );
        RECORD_FIELD( r, "decimal_index_max", uint, p->decimal_index_max );
	RECORD_FIELD( r, "nested_level", int, p->nested_level );
	RECORD_FIELD( r, "num_proc_params", uint, p->num_proc_params );
	RECORD_FIELD( r, "toplev_count", int, p->toplev_count );
	RECORD_FIELD( r, "max_call_param", uint, p->max_call_param );

	RECORD_FIELD( r, "decimal_point", uchar, p->decimal_point );
	RECORD_FIELD( r, "currency_symbol", uchar, p->currency_symbol );
	RECORD_FIELD( r, "numeric_separator", uchar, p->numeric_separator );
	RECORD_FIELD( r, "prog_type", uchar, p->prog_type );

	RECORD_FIELDP( r, "entry_convention", cb_tree, p->entry_convention );
        RECORD_FIELDB( r, "flag_main", p->flag_main );
	RECORD_FIELDB( r, "flag_common", p->flag_common );
	RECORD_FIELDB( r, "flag_initial", p->flag_initial );
	RECORD_FIELDB( r, "flag_recursive", p->flag_recursive );
#ifdef COBC_VERSION4
	RECORD_FIELDB( r, "flag_resident", p->flag_resident );
#endif
	RECORD_FIELDB( r, "flag_validated", p->flag_validated );
	RECORD_FIELDB( r, "flag_chained", p->flag_chained );
	RECORD_FIELDB( r, "flag_global_use", p->flag_global_use );

	RECORD_FIELDB( r, "flag_gen_error", p->flag_gen_error );
	RECORD_FIELDB( r, "flag_file_global", p->flag_file_global );
	RECORD_FIELDB( r, "flag_has_external", p->flag_has_external );
	RECORD_FIELDB( r, "flag_segments", p->flag_segments );
	RECORD_FIELDB( r, "flag_trailing_separate", p->flag_trailing_separate );
	RECORD_FIELDB( r, "flag_console_is_crt", p->flag_console_is_crt );
	RECORD_FIELDB( r, "flag_debugging", p->flag_debugging );
        RECORD_FIELDB( r, "flag_gen_debug", p->flag_gen_debug );

	RECORD_FIELDB( r, "flag_save_exception", p->flag_save_exception );
	RECORD_FIELDB( r, "flag_report", p->flag_report );
	RECORD_FIELDB( r, "flag_screen", p->flag_screen );
	RECORD_FIELDB( r, "flag_void", p->flag_void );
	RECORD_FIELDB( r, "flag_decimal_comp", p->flag_decimal_comp );

        RECORD_END(r);
};

static void print_cb_tree ( const cb_tree t, int indent, int full )
{
  if( t == NULL ) {
    fprintf(oc, "null");
    return ;
  }
  switch( t->tag ){
  case CB_TAG_FIELD :
    return print_cb_field( (struct cb_field*)t, indent, full );
  case CB_TAG_PROGRAM :
    return print_cb_program( (struct cb_program*)t, indent, full );
  case CB_TAG_LIST :
    return print_cb_list( (struct cb_list*)t, indent, full );
  case CB_TAG_FILE :
    return print_cb_file( (struct cb_file*)t, indent, full );
  case CB_TAG_CD :
    return print_cb_cd( (struct cb_cd*)t, indent, full );
  case CB_TAG_CONST :
    return print_cb_const( (struct cb_const*)t, indent, full );
  case CB_TAG_INTEGER :
    return print_cb_integer( (struct cb_integer*)t, indent, full );
  case CB_TAG_STRING :
    return print_cb_string( (struct cb_string*)t, indent, full );
  case CB_TAG_ALPHABET_NAME :
    return print_cb_alphabet_name( (struct cb_alphabet_name*)t, indent, full );
  case CB_TAG_CLASS_NAME :
    return print_cb_class_name( (struct cb_class_name*)t, indent, full );
  case CB_TAG_LOCALE_NAME :
    return print_cb_locale_name( (struct cb_locale_name*)t, indent, full );
  case CB_TAG_SYSTEM_NAME :
    return print_cb_system_name( (struct cb_system_name*)t, indent, full );
  case CB_TAG_LITERAL :
    return print_cb_literal( (struct cb_literal*)t, indent, full );
  case CB_TAG_DECIMAL :
    return print_cb_decimal( (struct cb_decimal*)t, indent, full );
  case CB_TAG_REPORT :
    return print_cb_report( (struct cb_report*)t, indent, full );
  case CB_TAG_REFERENCE :
    return print_cb_reference( (struct cb_reference*)t, indent, full );
  case CB_TAG_BINARY_OP :
    return print_cb_binary_op( (struct cb_binary_op*)t, indent, full );
  case CB_TAG_FUNCALL :
    return print_cb_funcall( (struct cb_funcall*)t, indent, full );
  case CB_TAG_CAST :
    return print_cb_cast( (struct cb_cast*)t, indent, full );
  case CB_TAG_INTRINSIC :
    return print_cb_intrinsic( (struct cb_intrinsic*)t, indent, full );

  case CB_TAG_LABEL :
    return print_cb_label( (struct cb_label*)t, indent, full );
  case CB_TAG_ASSIGN :
    return print_cb_assign( (struct cb_assign*)t, indent, full );
  case CB_TAG_INITIALIZE :
    return print_cb_initialize( (struct cb_initialize*)t, indent, full );
  case CB_TAG_SEARCH :
    return print_cb_search( (struct cb_search*)t, indent, full );
  case CB_TAG_CALL :
    return print_cb_call( (struct cb_call*)t, indent, full );
  case CB_TAG_GOTO :
    return print_cb_goto( (struct cb_goto*)t, indent, full );
  case CB_TAG_IF :
    return print_cb_if( (struct cb_if*)t, indent, full );
  case CB_TAG_PERFORM :
    return print_cb_perform( (struct cb_perform*)t, indent, full );
  case CB_TAG_STATEMENT :
    return print_cb_statement( (struct cb_statement*)t, indent, full );
  case CB_TAG_CONTINUE :
    return print_cb_continue( (struct cb_continue*)t, indent, full );
  case CB_TAG_CANCEL :
    return print_cb_cancel( (struct cb_cancel*)t, indent, full );
  case CB_TAG_ALTER :
    return print_cb_alter( (struct cb_alter*)t, indent, full );
  case CB_TAG_SET_ATTR :
    return print_cb_set_attr( (struct cb_set_attr*)t, indent, full );
  case CB_TAG_PERFORM_VARYING :
     return print_cb_perform_varying( (struct cb_perform_varying*)t, indent, full );
  case CB_TAG_PICTURE :
    return print_cb_picture( (struct cb_picture*)t, indent, full );
  case CB_TAG_DIRECT :
    return print_cb_direct( (struct cb_direct*)t, indent, full );
  case CB_TAG_DEBUG :
    return print_cb_debug( (struct cb_debug*)t, indent, full );
  case CB_TAG_DEBUG_CALL :
    return print_cb_debug_call( (struct cb_debug_call*)t, indent, full );
  case CB_TAG_PROTOTYPE :
    return print_cb_prototype( (struct cb_prototype*)t, indent, full );
  case CB_TAG_DECIMAL_LITERAL :
    return print_cb_decimal_literal( (struct cb_decimal*)t, indent, full );
  case CB_TAG_ML_SUPPRESS :
    return print_cb_ml_suppress( (struct cb_ml_suppress_clause*)t, indent, full );
  case CB_TAG_ML_TREE :
    return print_cb_ml_tree( (struct cb_ml_generate_tree*)t, indent, full );
  case CB_TAG_ML_SUPPRESS_CHECKS :
    return print_cb_ml_suppress_checks( (struct cb_ml_suppress_checks*)t, indent, full );
#ifdef COBC_VERSION4
  case CB_TAG_VARY :
    return print_cb_vary( (struct cb_vary*)t, indent, full );
  case CB_TAG_REPORT_LINE :
#endif
  default:  {
    return print_cb_tree_common ( t, indent, full );
  }
  }
}

static char* indentation = NULL;

static void json_init_indents(void)
{
  if( indentation == NULL){
    int i;
    indentation = malloc(MAX_INDENT+2);
    for( i=0; i<=MAX_INDENT; i++ ){
      indentation[i]=' ';
      indents[i] = (indentation+MAX_INDENT)-(i % 80);
    }
    indentation[MAX_INDENT+1] = 0;
  }
}

static void json_free_indents(void)
{
  if( indentation != NULL ){
    free(indentation);
    indentation = NULL;
  }
}


void json_print_program( const struct cb_program *p )
{
  int namelen = strlen(p->program_name);
  char filename [namelen+10];
  strcpy(filename, p->program_name);
  strcpy(filename+namelen, ".json");
  oc = fopen(filename, "w");

  if( getenv("COBC_JSON_LOCS") != NULL ) output_locations = 1;
  json_init_indents();
  print_cb_program( p, 0, 1 );
  json_free_indents();
  fclose(oc);
}

void json_print_tree ( const char * msg, const cb_tree t )
{
  oc = stderr ;
  json_init_indents();
  fprintf(oc, "%s>>>>>>\n", msg);
  print_cb_tree( t, 0, 1 );
  fprintf(oc, "\n%s<<<<<<\n", msg);
  json_free_indents();
}

/*
   Copyright (C) 2018-2020, 2022-2025 Free Software Foundation, Inc.
   Written by Edward Hart, Simon Sobisch

   This file is part of GnuCOBOL.

   The GnuCOBOL runtime library is free software: you can redistribute it
   and/or modify it under the terms of the GNU Lesser General Public License
   as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   GnuCOBOL is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with GnuCOBOL.  If not, see <https://www.gnu.org/licenses/>.
*/

#include "config.h"

#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <ctype.h>
#include <stdio.h>

#ifdef  _WIN32
#include "localcharset.h"
#endif

/* include internal and external libcob definitions, forcing exports */
#define	COB_LIB_EXPIMP
#include "coblocal.h"

#if defined (WITH_XML2)
#include <libxml/uri.h>
#include <libxml/parser.h>
#include <libxml/xmlerror.h>
#include <libxml/xmlschemas.h>
#include "libxml/xmlstring.h"
#include <libxml/xmlversion.h>
#include <libxml/xmlwriter.h>
#include <libxml/tree.h>
#include <libxml/SAX2.h>

#ifndef LIBXML_CONST_ERROR_PTR
#if LIBXML_VERSION >= 21200
#define LIBXML_CONST_ERROR_PTR	const xmlError *
#else
#define LIBXML_CONST_ERROR_PTR	xmlErrorPtr		/* use old ABI */
#endif
#endif

#else
/* libxml2 compatible definition to reduce the need for
   even more conditional compile */
typedef unsigned char xmlChar;
#define xmlStrlen(s)	strlen((const char *)s)
#endif

#if defined (WITH_CJSON)
#if defined (HAVE_CJSON_CJSON_H)
#include <cjson/cJSON.h>
#elif defined (HAVE_CJSON_H)
#include <cJSON.h>
#else
#error CJSON without necessary header
#endif
#elif defined (WITH_JSON_C)
#if defined (HAVE_JSON_C_JSON_H)
#include <json-c/json_object.h>
/* we use the current  (0.16+ name), for older versions defining
   it with the now deprecated name */
#ifndef JSON_C_OBJECT_ADD_CONSTANT_KEY
/* deprecated name and object function with flags exists since 0.13 */
#ifdef JSON_C_OBJECT_KEY_IS_CONSTANT
#define JSON_C_OBJECT_ADD_CONSTANT_KEY JSON_C_OBJECT_KEY_IS_CONSTANT
#else
/* For version 0.12 (minimal version supported),
   redefine json_object_object_add_ex to ignore flags */
#define json_object_object_add_ex(obj, key, val, opts) \
	json_object_object_add (obj, key, val)

static inline const char *
json_object_to_json_string_length (struct json_object *obj,
	int flags, size_t *length)
{
	const char *str = json_object_to_json_string_ext (obj, flags);
	if (length) {
		*length = str ? strlen(str) : 0;
	}
	return str;
}
#endif

#endif /* JSON_C_OBJECT_ADD_CONSTANT_KEY */

#include <json-c/linkhash.h>
#elif defined (HAVE_JSON_H)
#include <json.h>
#else
#error JSON-C without necessary header
#endif
#endif

/* Local variables */

/* XMLSS return-code halfword */
#define XRC_SUCCESS        	0x0000	/* XMLPARSE processing successfull */
#define XRC_NOT_WELL_FORMED	0x000C	/* not well-formed doc */
#define XRC_FATAL          	0x0010	/* fatal error with potential bad / invalid output */
#define XRC_NOT_VALID      	0x0018	/* non-fatal: doc doesn't match specified schema */

/* XMLSS reason-code halfword */
#define XRSN_SUCCESS                	0x0000	/* XMLPARSE processing successfull */
#define XRSN_UNKNOWN_ERROR          	0x1154	/* unknown error */
#define XRSN_PARM_UNSUPPORT_ENCODING	0x1203	/* encoding not supported */

/* standard error codes */
enum xml_code_status {
	XML_STMT_EXIT = -1,
	XML_STMT_SUCCESSFULL = 0,
	XML_PARSE_ERROR_FATAL = XRC_FATAL & (XRSN_UNKNOWN_ERROR << 1),
	XML_PARSE_ERROR_MISC_COMPAT = 201, /* various errors, only in XMLPARSE COMPAT */
	XML_OUT_FIELD_TOO_SMALL = 400,
	XML_INVALID_NAMESPACE = 416,
	XML_INVALID_CHAR_REPLACED = 417,
	XML_INVALID_NAMESPACE_PREFIX = 419,
	XML_INTERNAL_ERROR = 600
};

/* TODO: check for necessary cleanup */

enum xml_parser_state {
	XML_PARSER_NOT_STARTED = 0,
	XML_PARSER_VALIDATION_SETUP,
	XML_PARSER_VALIDATION_SETUP_MEM,
	XML_PARSER_JUST_STARTED,
	XML_PARSER_DOCUMENT_START,
	XML_PARSER_HAD_END_OF_DOCUMENT,
	XML_PARSER_HAD_END_OF_INPUT,
	XML_PARSER_FINE,
	XML_PARSER_HAD_NONFATAL_ERROR,
	XML_PARSER_HAD_FATAL_ERROR,
	XML_PARSER_STARTING_NEXT_CHUNK,
	XML_PARSER_FINISHED,
	XML_PARSER_IGNORE_ERROR /* special value for suppressing errors */
};

enum cob_xml_registers {
	SREG_XML_EVENT,
	SREG_XML_INFORMATION,
	SREG_XML_TEXT,
	SREG_XML_NTEXT,
	SREG_XML_NAMESPACE,
	SREG_XML_NNAMESPACE,
	SREG_XML_NS_PREFIX,
	SREG_XML_NNS_PREFIX
};

#define COB_XML_EVENT(name,str)	name,
enum cob_xml_event {
	EVENT_UNKNOWN = 0,
#include "xmlevent.def"
	EVENT_MAX_ENTRY /* always the last entry */
};
#undef COB_XML_EVENT

/* content found in special register XML-EVENT */
#ifdef	HAVE_DESIGNATED_INITS
const char	*xml_event_name[EVENT_MAX_ENTRY] = {
	[EVENT_UNKNOWN] = "UNKNOWN"
#define COB_XML_EVENT(ename,str)	, [ename] = str
#include "xmlevent.def"
#undef COB_XML_EVENT
};
const size_t	xml_event_name_len[EVENT_MAX_ENTRY] = {
	[EVENT_UNKNOWN] = 0
#define COB_XML_EVENT(ename,str)	, [ename] = sizeof (str) - 1
#include "xmlevent.def"
#undef COB_XML_EVENT
};
#else
const char	*xml_event_name[EVENT_MAX_ENTRY];
size_t	xml_event_name_len[EVENT_MAX_ENTRY];
static void init_xml_event_list (void);
#endif

struct xml_event {
	enum cob_xml_event		event;
	struct xml_event		*next;				/* pointer to next element */
	const char				*text_ptr;			/* text pointer in buff */
	size_t					text_len;			/* length of this text */
	const char				*namespace_ptr;		/* namespace pointer in buff */
	size_t					namespace_len;		/* length of this namespace */
	const char				*prefix_ptr;		/* prefix pointer in buff */
	size_t					prefix_len;			/* length of this prefix */
};

struct xml_state {
	enum xml_parser_state state;
	enum xml_code_status last_xml_code;
	int		flags;
#if WITH_XML2
	xmlSAXHandler sax;
	xmlParserCtxtPtr ctx;
	xmlSchemaPtr schema;
	xmlSchemaValidCtxtPtr val_ctx;
	xmlSchemaSAXPlugPtr xsd_plug;
	xmlParserErrors err;
#endif
	struct xml_event *first_event;	/* pointer to first processed event */
	struct xml_event *event;	/* pointer to last processed event */
	const char	*input_data_ptr;
	const char	*input_data_end;
	void	*buff; 	/* buffer for event data */
	size_t	buff_len;		/* size of current buffer for "text"
	                 		  (increasing until end of XML processing) */
	size_t	buff_off;		/* offset in buffer, reset before each iteration */
	int		eof;
};

enum json_code_status {
	JSON_ALL_OK 		 = 0,
	JSON_OUT_FIELD_TOO_SMALL = 1,
	JSON_INTERNAL_ERROR = 500
};


static cob_global		*cobglobptr;

/* Local functions prototypes */

static void xml_endDocument (void *ctx);
void * buffer_xml_event_data (struct xml_state *state,
							const void *data,
							size_t size);


/* set special register XML-CODE */
static COB_INLINE COB_A_INLINE void
set_xml_code (const enum xml_code_status code)
{
	/* LCOV_EXCL_START */
	if (!COB_MODULE_PTR->xml_code) {
		/* compat only - always available with GC 3.2 */
		return;
	}
	/* LCOV_EXCL_STOP */
	cob_set_int (COB_MODULE_PTR->xml_code, (int)code);
}

/* set internal XML exception and special register XML-CODE */
static void
set_xml_exception (const enum xml_code_status code)
{
	cob_set_exception (COB_EC_XML_IMP);
	set_xml_code (code);
}

/* get special register XML-CODE */
static COB_INLINE COB_A_INLINE int
get_xml_code (void)
{
	return cob_get_int (COB_MODULE_PTR->xml_code);
}

/* set special register XML-EVENT */
static void
set_xml_event (enum cob_xml_event event)
{
	const size_t size1 = xml_event_name_len[event];
	const unsigned char *data1 = (unsigned char *)xml_event_name[event];

	/* note: it is up to the compiler to ensure that the register is setup
	   and we only get here for GC 3.2+ modules which have it - verified
	   in the initial run of cob_xml_parse */

	const size_t size2 = 30;
	unsigned char *data2 = COB_MODULE_PTR->xml_event->data;

	memcpy (data2, data1, size1);
	memset (data2 + size1, ' ', size2 - size1);
}

/* provide event structure and does the setup in the state,
   note: re-uses events if possible, allocates a new event if needed */
static struct xml_event *
xml_event_initialized (struct xml_event *event) {
			event->text_ptr			= NULL;
			event->text_len			= 0;
			event->namespace_ptr	= NULL;
			event->namespace_len	= 0;
			event->prefix_ptr		= NULL;
			event->prefix_len		= 0;
	return event;
}

/* provide event structure and does the setup in the state,
   note: re-uses events if possible, allocates a new event if needed */
static struct xml_event *
new_xml_event (struct xml_state *state,  enum cob_xml_event xml_event) {
	struct xml_event *event = state->event;

	/* re-use event structure from previous run */
	if (event) {
		if (event->event == EVENT_UNKNOWN) {
			/* very first element, and unsused: */
			event->event = xml_event;
			return xml_event_initialized (event);
		}
		if (event->next) {
			/* another unused element */
			event = event->next;
			event->event = xml_event;
			state->event = event;
			return xml_event_initialized (event);
		}
	}

	/* no empty events from previous parsing, create a new one */
	event = cob_malloc (sizeof (struct xml_event));
	/* add logic to check for malloc failure */
	memset (event, '\0', sizeof (struct xml_event));
	event->event = xml_event;
	if (state->event) {
		state->event->next = event;
	} else {
		state->first_event = event;
	}
	state->event = event;
	return event;
}

/* the following functions may be used with partially manual parsing
   of XML (which could be useful for the testsuite), but are currently
   only used for complete parsing via libxml2 */
#if defined (WITH_XML2)


/* add data to event buffer with given size;
   returns -1 if buffer allocation is not possible */
void *
buffer_xml_event_data (struct xml_state *state,
		       const void *data,
		       size_t size)
{
	size_t buff_free_size = state->buff_len - state->buff_off;
	void *next_buffer_pos = ((unsigned char *)state->buff) + state->buff_off;

	/* most common: enough size in the buffer, so copy and finish */
	if (size <= buff_free_size) {
		memcpy (next_buffer_pos, data, size);
		state->buff_off += size;
		return next_buffer_pos;
	}

	/* otherwise: allocate new buffer with additional space, preserving existing data */
	{
		const size_t malloc_size = state->buff_len + COB_LARGE_BUFF;
		void	*mptr = cob_fast_malloc (malloc_size);
		/* CHECKME: we possibly want to handle out of memory to pass it to COBOL
		   as XML error - but cob_fast_malloc / cob_malloc already abort the runtime
		   in case of missing memory */
		if (mptr) {
			if (state->buff_off) {
				memcpy (mptr, state->buff, state->buff_off);
			}
			cob_free (state->buff);
			state->buff = mptr;
			state->buff_len = malloc_size;
			buff_free_size = state->buff_len - state->buff_off;
			next_buffer_pos = ((unsigned char *)state->buff) + state->buff_off;

	/* most common: enough size in the buffer, so copy and finish */
			if (size <= buff_free_size) {
				memcpy (next_buffer_pos, data, size);
				state->buff_off += size;
				return next_buffer_pos;
			}
		}
	}

	return NULL;
}

/* add data to event buffer with given size, ignores size = zero;
   returns -1 if buffer allocation is not possible */
static void
add_xml_event_data (struct xml_state *state,
					enum cob_xml_registers sreg,
					const void *data,
					size_t size)
{
	void	*buff_data;
	buff_data = buffer_xml_event_data (state, data, size);

	/* add to the current event's data*/
	if (size == 0) {
		return;
	}
	switch (sreg) {
		case SREG_XML_TEXT :
		case SREG_XML_NTEXT :
			state->event->text_ptr		= (char *) buff_data;
			state->event->text_len		= size;
			break;
		case SREG_XML_NAMESPACE :
		case SREG_XML_NNAMESPACE :
			state->event->namespace_ptr	= (char *) buff_data;
			state->event->namespace_len	= size;
			break;
		case SREG_XML_NS_PREFIX :
		case SREG_XML_NNS_PREFIX :
			state->event->prefix_ptr	= (char *) buff_data;
			state->event->prefix_len	= size;
			break;
	}
}
#endif /* defined (WITH_XML2) */

/* set special registers XML-TEXT / XML-NTEXT
   the size is calculated if not explicit specified (size -> -1)
   if the state is given then the text is copied to its buffer */
static void
set_xml_text (const int ntext, const void *data, const size_t size)
{
	if (ntext) {
		/* TODO (later): convert input data (libxml2 uses UTF8) to UTF-16
		   (or the specified national character set) */
		COB_MODULE_PTR->xml_ntext->data = (unsigned char *) data;
		COB_MODULE_PTR->xml_ntext->size = size;
		COB_MODULE_PTR->xml_text->data = (unsigned char *) "";
		COB_MODULE_PTR->xml_text->size = 0;
	} else {
		/* XML-NTEXT and other XML-N... special registers are not available with ACUCOBOL */
		if (COB_MODULE_PTR->xml_ntext) {
			COB_MODULE_PTR->xml_ntext->data = (unsigned char *) "";
			COB_MODULE_PTR->xml_ntext->size = 0;
		}
		COB_MODULE_PTR->xml_text->data = (unsigned char *) data;
		COB_MODULE_PTR->xml_text->size = size;
	}
}

/* set special registers XML-NAMESPACE / XML-NNAMESPACE as well
   as optional XML-NAMESPACE-PREFIX / XML-NNAMESPACE-PREFIX
   the size is auto-calculated  */
static void
set_xml_namespace (const int ntext, const void *nsdata, const size_t ns_size,
		const void *prefix, const size_t prefix_size)
{
	if (ntext) {
		/* TODO (later): convert input data (libxml2 uses UTF8) to UTF-16
		   (or the specified national character set) */
		COB_MODULE_PTR->xml_nnamespace->data = (unsigned char *) nsdata;
		COB_MODULE_PTR->xml_nnamespace->size = ns_size;
		if (prefix) {
			COB_MODULE_PTR->xml_nnamespace_prefix->data = (unsigned char *) prefix;
			COB_MODULE_PTR->xml_nnamespace_prefix->size = prefix_size;
		} else {
			COB_MODULE_PTR->xml_nnamespace_prefix->data = (unsigned char *) "";
			COB_MODULE_PTR->xml_nnamespace_prefix->size = 0;
		}
		COB_MODULE_PTR->xml_namespace->data = (unsigned char *) "";
		COB_MODULE_PTR->xml_namespace->size = 0;
		COB_MODULE_PTR->xml_namespace_prefix->data = (unsigned char *) "";
		COB_MODULE_PTR->xml_namespace_prefix->size = 0;
	} else {
		/* XML-NTEXT and other XML-N... special registers are not available with ACUCOBOL */
		if (COB_MODULE_PTR->xml_namespace) {
			COB_MODULE_PTR->xml_nnamespace->data = (unsigned char *) "";
			COB_MODULE_PTR->xml_nnamespace->size = 0;
		}
		if (COB_MODULE_PTR->xml_nnamespace_prefix) {
			COB_MODULE_PTR->xml_nnamespace_prefix->data = (unsigned char *) "";
			COB_MODULE_PTR->xml_nnamespace_prefix->size = 0;
		}
		COB_MODULE_PTR->xml_namespace->data = (unsigned char *) nsdata;
		COB_MODULE_PTR->xml_namespace->size = ns_size;
		if (prefix) {
			COB_MODULE_PTR->xml_namespace_prefix->data = (unsigned char *) prefix;
			COB_MODULE_PTR->xml_namespace_prefix->size = prefix_size;
		} else {
			COB_MODULE_PTR->xml_namespace_prefix->data = (unsigned char *) "";
			COB_MODULE_PTR->xml_namespace_prefix->size = 0;
		}
	}
}

/* set register special JSON-CODE */
static COB_INLINE COB_A_INLINE void
set_json_code (const enum json_code_status code)
{
	/* LCOV_EXCL_START */
	if (!COB_MODULE_PTR->json_code) {
		/* compat only - always available with GC 3.2 */
		return;
	}
	/* LCOV_EXCL_STOP */
	cob_set_int (COB_MODULE_PTR->json_code, (int)code);
}

/* set internal JSON exception and special register JSON-CODE */
static void
set_json_exception (const enum json_code_status code)
{
	cob_set_exception (COB_EC_JSON_IMP);
	set_json_code (code);
}

/* check if given cob_field has zero-length or is all spaces */
static int
is_empty (const cob_field * const f)
{
	size_t	i;

	for (i = 0; i < f->size; ++i) {
		if (f->data[i] != ' ') {
			return 0;
		}
	}

	return 1;
}

/* strdup-like wrapper for get_trimmed_data, returns a pointer to
   fresh allocated memory pointing to a copy of the specified
   data with specified size as string (+ trailing NULL) */
static void *
copy_data_as_string (const char* data, const size_t size)
{
	char *ptr = cob_fast_malloc (size + 1);
#if 0	/* CHECKME: we possibly want to handle out of memory to pass it to COBOL
		   as XML error - but cob_fast_malloc / cob_malloc already abort the runtime
		   in case of missing memory */
	if (!ptr) {
		return NULL;
	}
#endif
	memcpy (ptr, data, size);
	ptr[size] = 0;
	return (void *)ptr;
}

/* returns a pointer + lenght to given cob_field's data,
   right trimmed with no JUSTIFIED RIGHT, left-trimmed otherwise,
   returns pointer to single space if empty (or variable lenght zero) */
static void
get_trimmed_data_int (const cob_field * const f,
	const char **data, int *out_len)
{
	char	*str = (char *)f->data;
	int 	len = f->size;

	if (len == 0) {
		*out_len = 1;
		*data = " ";
		return;
	}

	if (COB_FIELD_JUSTIFIED (f)) {
		for (; *str == ' ' && len > 1; ++str, --len);
	} else {
		for (; (str[len - 1] == ' ' || str[len - 1] == 0) && len > 1; --len);
	}

	*out_len = len;
	*data = str;
}

/* returns a duplicate of the given cob_field's data,
   right trimmed with no JUSTIFIED RIGHT, left-trimmed otherwise,
   returns pointer to single space if empty (or variable lenght zero) */
static void *
get_trimmed_data (const cob_field * const f,
	void * (*strndup_func)(const char *, size_t))
{
	int 	len;
	const char	*str;

	get_trimmed_data_int (f, &str, &len);

	return (*strndup_func)(str, len);
}

/* Returns 1 if str contains invalid XML 1.0 chars, 0 otherwise. */
static int
has_invalid_xml_char (const cob_field * const f)
{
	size_t	i;

	/*  Char       ::=      #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF] */
	/* TO-DO: This assumes the data is already in UTF-8! */
	for (i = 0; i < f->size; ++i) {
		if (iscntrl (f->data[i])
		 && f->data[i] != 0x09
		 && f->data[i] != 0x0a
		 && f->data[i] != 0x0d) {
			return 1;
		}
	}

	/* TO-DO: 2/3/4-byte characters. Will this need libicu? */

	return 0;
}

/* check for valid XML name */
static int
is_valid_xml_name (const cob_field * const f)
{
	char	*str, *c;
	int	ret;

	if (!cob_is_xml_namestartchar (f->data[0])) {
		return 0;
	}

	str = get_trimmed_data (f, &copy_data_as_string);
	if (!str) {
		/* likely should raise an exception */
		return 0;
	}

	ret = 1;
	for (c = str + 1; *c; ++c) {
		if (!cob_is_xml_namechar (*c)) {
			ret = 0;
			break;
		}
	}

	cob_free (str);
	return ret;
}

#if defined (WITH_XML2) || defined (WITH_CJSON) || defined (WITH_JSON_C)

static cob_pic_symbol *
get_pic_for_num_field (const unsigned short num_int_digits,
			const unsigned short num_dec_digits)
{
	static cob_pic_symbol pic[2 + 2 + 1];
	cob_pic_symbol	*symbol = pic;

	/* note: we want a floating sign even for non-signed values
	   as this allows to left-trim spaces afterwards */
	symbol->symbol = '-';
	symbol->times_repeated = num_int_digits ? num_int_digits : 1;
	++symbol;

	symbol->symbol = '9';
	symbol->times_repeated = 1;
	++symbol;

	if (num_dec_digits) {
		symbol->symbol = COB_MODULE_PTR->decimal_point;
		symbol->times_repeated = 1;
		++symbol;

		symbol->symbol = '9';
		symbol->times_repeated = num_dec_digits;
		++symbol;
	}

	symbol->symbol = '\0';

	return pic;
}

static void *
get_num_int (cob_field * const f, const char decimal_point,
	unsigned char *buffer, int *out_len)
{
	unsigned char  	*p = buffer;
	const unsigned short	num_integer_digits
		= cob_max_int (0, COB_FIELD_DIGITS (f) - COB_FIELD_SCALE (f));
	const unsigned short	num_decimal_digits
		= cob_max_int (0, COB_FIELD_SCALE (f));
	int 	len = (num_integer_digits ? (1 + num_integer_digits) : 2)
			    + (num_decimal_digits ? (1 + num_decimal_digits) : 0);
	const unsigned char mod_orig_decimal_point = COB_MODULE_PTR->decimal_point != decimal_point
	                                           ? COB_MODULE_PTR->decimal_point : 0;
	cob_field_attr	attr;
	cob_field   	edited_field = { .size = len, .data = p, .attr = &attr };

	/* temporarily swap decimal point, if needed */
	if (mod_orig_decimal_point) {
		COB_MODULE_PTR->decimal_point = decimal_point;
	}

	/* Initialize attribute for nicely edited version of f */
	attr.type = COB_TYPE_NUMERIC_EDITED;
	attr.flags = (COB_FLAG_JUSTIFIED | COB_FIELD_HAVE_SIGN (f));
	attr.scale = (COB_FIELD_SCALE (f) < 0) ? 0 : COB_FIELD_SCALE (f);
	attr.digits = (num_integer_digits ? num_integer_digits : 1) + num_decimal_digits;

	attr.pic = get_pic_for_num_field (num_integer_digits,
					  num_decimal_digits);

	/* Set field (with expected decimal point as-needed) */
	cob_move (f, &edited_field);
	if (mod_orig_decimal_point) {
		COB_MODULE_PTR->decimal_point = mod_orig_decimal_point;
	}

	/* Trim output (we know the edited field will have at least one digit)
	   and provide a duplicate as we need it later */
	for (; *p == ' '; ++p, --len);
	p[len] = 0;

	*out_len = len;
	return p;
}

#endif

#if defined (WITH_XML2)


/* XML strdup wrapper for get_trimmed_xml_data */
static void *
xmlCharStrndup_void (const char *str, const size_t size)
{
	return (void *)xmlCharStrndup (str, size);
}

/* returns a duplicate of the given cob_field's data,
   allocated with XML strdup,
   right trimmed with no JUSTIFIED RIGHT, left-trimmed otherwise,
   returns pointer to single space if empty (or variable lenght zero) */
static xmlChar *
get_trimmed_xml_data (const cob_field * const f)
{
	return (xmlChar *) get_trimmed_data (f, &xmlCharStrndup_void);
}

static xmlChar *
get_xml_name (const cob_field * const f)
{
	xmlChar	*name;

	name = get_trimmed_xml_data (f);

	if (name && !cob_is_xml_namestartchar (name[0])) {
		xmlChar	*underscore;
		xmlChar	*name_with_underscore;
		underscore = xmlCharStrdup ("_");
		if (underscore) {
			name_with_underscore = xmlStrcat (underscore, name);
		} else {
			name_with_underscore = NULL;
		}

		xmlFree (name);
		return name_with_underscore;
	} else {
		return name;
	}
}

static int
generate_xml_from_tree (xmlTextWriterPtr, cob_ml_tree *, xmlChar *, xmlChar *,
			const char, unsigned int *);

static xmlChar *
get_name_with_hex_prefix (const cob_field * const name)
{
	xmlChar	*hex_str;
	xmlChar	*x_name;
	xmlChar	*hex_name;

	/*
	  NB: hex_str must be allocated every time because xmlStrcat will
	  realloc hex_str.
	*/
	hex_str = xmlCharStrdup ("hex.");

	x_name = get_xml_name (name);
	hex_name = xmlStrcat (hex_str, x_name);
	xmlFree (x_name);

	return hex_name;
}

static char
int_to_hex (int n)
{
	if (n < 10) {
		n = n + '0';
	} else {
		n = n - 10 + 'a';
	}
	return (char)n;
}

static xmlChar *
get_hex_xml_data (const cob_field * const f)
{
	xmlBufferPtr	buff;
	size_t		i;
	char		hex_num[3] = { '\0' };
	xmlChar		*hex_data;

	buff = xmlBufferCreate ();
	if (!buff) {
		return NULL;
	}

	for (i = 0; i < f->size; ++i) {
		hex_num[0] = int_to_hex (f->data[i] / 16);
		hex_num[1] = int_to_hex (f->data[i] % 16);
		xmlBufferWriteChar (buff, hex_num);
	}

	hex_data = xmlStrdup (xmlBufferContent (buff));
	xmlBufferFree (buff);

	return hex_data;
}

static int
generate_hex_attribute (xmlTextWriterPtr writer, cob_ml_attr *attr, unsigned int *count)
{
	xmlChar	*hex_name;
	xmlChar	*value;
	int 	ret;

	hex_name = get_name_with_hex_prefix (attr->name);
	value = get_hex_xml_data (attr->value);
	ret = xmlTextWriterWriteAttribute (writer, hex_name, value);
	xmlFree (hex_name);
	xmlFree (value);

	if (ret < 0) {
		return ret;
	}
	return 0;
}

static int
generate_normal_attribute (xmlTextWriterPtr writer, cob_ml_attr *attr, unsigned int *count)
{
	xmlChar	*name;
	xmlChar	*value;
	int 	ret;

	name = get_xml_name (attr->name);
	value = get_trimmed_xml_data (attr->value);
	ret = xmlTextWriterWriteAttribute (writer, name, value);
	xmlFree (name);
	xmlFree (value);

	if (ret < 0) {
		return ret;
	}
	return 0;
}

static int
generate_attributes (xmlTextWriterPtr writer, cob_ml_attr *attr, unsigned int *count)
{
	int	status;

	for (; attr; attr = attr->sibling) {
		if (attr->is_suppressed) {
			continue;
		}

		if (has_invalid_xml_char (attr->value)) {
			set_xml_code (XML_INVALID_CHAR_REPLACED);
			status = generate_hex_attribute (writer, attr, count);
		} else {
			status = generate_normal_attribute (writer, attr, count);
		}

		if (status < 0) {
			return status;
		}
	}

	return 0;
}

static int
generate_hex_element (xmlTextWriterPtr writer, cob_ml_tree *tree,
		      xmlChar *x_ns, xmlChar *x_ns_prefix, unsigned int *count)
{
	xmlChar		*hex_name;
	int		status;
	xmlChar		*hex_value;
	int 	ret;

	hex_name = get_name_with_hex_prefix (tree->name);
	ret = xmlTextWriterStartElementNS (writer, x_ns_prefix, hex_name, x_ns);
	xmlFree (hex_name);
	if (ret < 0) {
		return ret;
	}

	status = generate_attributes (writer, tree->attrs, count);
	if (status < 0) {
		return status;
	}

	hex_value = get_hex_xml_data (tree->content);
	ret = xmlTextWriterWriteString (writer, hex_value);
	xmlFree (hex_value);
	if (ret < 0) {
		return ret;
	}

	ret = xmlTextWriterEndElement (writer);
	if (ret < 0) {
		return ret;
	}

	return 0;
}

static int
generate_content (xmlTextWriterPtr writer, cob_ml_tree *tree,
		  const char decimal_point, unsigned int *count)
{
	cob_field	*content = tree->content;
	int 	ret = 0;

	if (COB_FIELD_IS_FP (content)) {
		/* TO-DO: Implement! */
		/* TO-DO: Stop compilation if float in field */
		cob_set_exception (COB_EC_IMP_FEATURE_MISSING);
		cob_fatal_error (COB_FERROR_XML);
	} else if (COB_FIELD_IS_NUMERIC (content)) {
		unsigned char	edited_data[COB_MAX_BINARY + 2];
		int 	len = 0;
		xmlChar		*x_content = get_num_int (tree->content, decimal_point, edited_data, &len);
		ret = xmlTextWriterWriteRawLen (writer, x_content, len);
	} else {
		int 	len;
		const char	*p;
		get_trimmed_data_int (tree->content, &p, &len);
		/* for XML escape we need to pass a null-terminated string,
		   so create a temporary buffer with that */
		if (len < COB_SMALL_BUFF) {
			xmlChar	x_content[COB_SMALL_BUFF];
			memcpy (x_content, p, len);
			x_content[len] = 0;
			ret = xmlTextWriterWriteString (writer, x_content);
		} else {
			xmlChar	*x_content = copy_data_as_string (p, len);
			memcpy (x_content, p, len);
			ret = xmlTextWriterWriteString (writer, x_content);
			cob_free (x_content);
		}
	}

	if (ret < 0) {
		return ret;
	}

	return 0;
}


static int
generate_normal_element (xmlTextWriterPtr writer, cob_ml_tree *tree,
			 xmlChar *x_ns, xmlChar *x_ns_prefix,
			 const char decimal_point, unsigned int *count)
{
	int		status;
	xmlChar		*x_name;
	cob_ml_tree	*child;
	int  	ret;

	/* Start element */
	x_name = get_xml_name (tree->name);
	ret = xmlTextWriterStartElementNS (writer, x_ns_prefix, x_name, x_ns);
	xmlFree (x_name);
	if (ret < 0) {
		return ret;
	}

	status = generate_attributes (writer, tree->attrs, count);
	if (status < 0) {
		return status;
	}

	/* Output child elements or content. */
	if (tree->children) {
		for (child = tree->children; child; child = child->sibling) {
			/*
			  Note we only have a namespace attribute on the
			  outermost element.
			*/
			status = generate_xml_from_tree (writer, child, NULL,
							 x_ns_prefix,
							 decimal_point, count);
			if (status < 0) {
				return status;
			}
		}
	} else if (tree->content) {
		status = generate_content (writer, tree, decimal_point, count);
		if (status < 0) {
			return status;
		}
	}

	/* Complete element */
	ret = xmlTextWriterEndElement (writer);
	if (ret < 0) {
		return ret;
	}

	return 0;
}

static int
generate_element (xmlTextWriterPtr writer, cob_ml_tree *tree,
		  xmlChar *x_ns, xmlChar *x_ns_prefix, const char decimal_point,
		  unsigned int *count)
{
	/* Check for invalid characters. */
	if (tree->content
	 && !COB_FIELD_IS_NUMERIC (tree->content)
	 && has_invalid_xml_char (tree->content)) {
		set_xml_code (XML_INVALID_CHAR_REPLACED);
		return generate_hex_element (writer, tree, x_ns, x_ns_prefix,
					     count);
	} else {
		return generate_normal_element (writer, tree, x_ns,
						x_ns_prefix, decimal_point,
						count);
	}
}

static int
generate_xml_from_tree (xmlTextWriterPtr writer, cob_ml_tree *tree,
			xmlChar *ns, xmlChar *ns_prefix,
			const char decimal_point, unsigned int *count)
{
	if (tree->is_suppressed) {
		return 0;
	}

	if (tree->name) {
		return generate_element (writer, tree, ns, ns_prefix,
					 decimal_point, count);
	} else {
		return generate_content (writer, tree, decimal_point, count);
	}
}

#endif

#if defined (WITH_CJSON) || defined (WITH_JSON_C)


#if defined (WITH_CJSON)
static int
generate_json_from_tree (cob_ml_tree *tree, const char decimal_point, cJSON *out)
{
	cob_ml_tree	*child;
	char		*name = NULL;
	char		*content = NULL;
	int		status = 0;
	cJSON		*children_json = NULL;

	if (tree->is_suppressed) {
		return 0;
	}

	/* NAME OF ... OMITTED to generate an anonymous JSON object */
	if (tree->name != NULL) {
		name = get_trimmed_data (tree->name, &copy_data_as_string);
	}
	if (tree->children) {
		if (name != NULL) {
			children_json = cJSON_CreateObject ();
		} else {
			children_json = out;
		}
		for (child = tree->children; child; child = child->sibling) {
			status = generate_json_from_tree (child, decimal_point,
							  children_json);
			if (status < 0) {
				cJSON_Delete (children_json);
				goto end;
			}
		}
		if (name != NULL) {
			cJSON_AddItemToObject (out, name, children_json);
		}
	} else if (tree->content) {
		if (name == NULL) {
			/* TO-DO: Handle correctly, that's possibly an internal error! */
			cob_set_exception (COB_EC_IMP_FEATURE_MISSING);
			cob_fatal_error (COB_FERROR_JSON);
		}
		if (COB_FIELD_IS_FP (tree->content)) {
			/* TO-DO: Implement! */
			/* TO-DO: Stop compilation if float in field */
			cob_set_exception (COB_EC_IMP_FEATURE_MISSING);
			cob_fatal_error (COB_FERROR_JSON);
		} else if (COB_FIELD_IS_NUMERIC (tree->content)) {
			unsigned char	edited_data[COB_MAX_BINARY + 3];	/* minus, comma, null */
			const char *p;
			int 	len;

			p = get_num_int (tree->content, decimal_point, edited_data, &len);

			content = copy_data_as_string (p, len);
			/*
			  We use AddRaw instead of AddNumber because a PIC 9(32)
			  may not be representable using the double AddNumber
			  uses internally.
			*/
			if (!cJSON_AddRawToObject (out, name, content)) {
				status = -1;
				goto end;
			}
		} else {
			content = (char *) get_trimmed_data (tree->content, &copy_data_as_string);
			if (!cJSON_AddStringToObject (out, name, content)) {
				status = -1;
				goto end;
			}
		}
	}

 end:
	if (content) {
		cob_free (content);
	}
	if (name) {
		cob_free (name);
	}
	return status;
}
#elif defined (WITH_JSON_C)
static int
generate_json_from_tree (cob_ml_tree *tree, const char decimal_point, json_object *out)
{
	cob_ml_tree	*child;
	const char 	*name = NULL;
	int 	status = 0;
	json_object	*children_json = NULL;

	if (tree->is_suppressed) {
		return 0;
	}

	/* NAME OF ... OMITTED to generate an anonymous JSON object */
	if (tree->name != NULL) {
		/* note: those "fields" are actual constants either from the variable name
		   or from NAME OF ... IS literal; in the first case there are no spaces
		   to trim and in the second case: JSON keys (per spec) may contain spaces anywhere,
		   including at its start/end; if a user added them, it was likely on purpose,
		   so no trimming here; also: in any case this is data that includes a trailing
		   nul, so we can directly use it as a constant(!) C literal */
		name = (const char 	*)tree->name->data;
	}
	if (tree->children) {
		if (name != NULL) {
			children_json = json_object_new_object ();
		} else {
			children_json = out;
		}
		for (child = tree->children; child; child = child->sibling) {
			status = generate_json_from_tree (child, decimal_point, children_json);
			if (status < 0) {
				json_object_put (children_json);
				goto end;
			}
		}
		if (name != NULL) {
			json_object_object_add_ex (out, name, children_json,
					JSON_C_OBJECT_ADD_CONSTANT_KEY);
		}
	} else if (tree->content) {
		if (name == NULL) {
			/* TO-DO: Handle correctly, that's possibly an internal error! */
			cob_set_exception (COB_EC_IMP_FEATURE_MISSING);
			cob_fatal_error (COB_FERROR_JSON);
		}
		if (COB_FIELD_IS_FP (tree->content)) {
			/* TO-DO: Implement! */
			/* TO-DO: Stop compilation if float in field */
			cob_set_exception (COB_EC_IMP_FEATURE_MISSING);
			cob_fatal_error (COB_FERROR_JSON);
		} else if (COB_FIELD_IS_NUMERIC (tree->content)) {
			unsigned char	edited_data[COB_MAX_BINARY + 2];
			int len = 0;
			char *str = get_num_int (tree->content, decimal_point, edited_data, &len);
			/*
			  Since we're only going to serialise the JSON, we don't
			  care how JSON-C represents it internally. So, we tell
			  C-JSON the number is 0.0f.
			*/
			json_object_object_add_ex (out, name,
					json_object_new_double_s (0.0, str),
					JSON_C_OBJECT_ADD_CONSTANT_KEY);
		} else {
			int 	len;
			const char	*str;
			get_trimmed_data_int (tree->content, &str, &len);
			json_object_object_add_ex (out, name,
					json_object_new_string_len (str, len),
					JSON_C_OBJECT_ADD_CONSTANT_KEY);
		}
	}

 end:
	return status;
}
#endif

#endif

/* Global functions */

int
cob_is_xml_namestartchar (const int c)
{
	/*
	  From XML 1.0 spec (https://www.w3.org/TR/xml/):
	  [4] NameStartChar ::= ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6]
				    | [#xD8-#xF6] | [#xF8-#x2FF]
				    | [#x370-#x37D] | [#x37F-#x1FFF]
				    | [#x200C-#x200D] | [#x2070-#x218F]
				    | [#x2C00-#x2FEF] | [#x3001-#xD7FF]
				    | [#xF900-#xFDCF] | [#xFDF0-#xFFFD]
				    | [#x10000-#xEFFFF]
	  [4a] NameChar ::= NameStartChar | "-" | "." | [0-9] | #xB7
					| [#x0300-#x036F] | [#x203F-#x2040]
	*/
	/* TO-DO: Deal with 2/3/4-byte chars. */
	return isalpha(c) || c == '_'
		|| (c >= 0xc0 && c <= 0xd6)
		|| (c >= 0xd8 && c <= 0xf6)
		|| (c >= 0xf8);
}

int
cob_is_xml_namechar (const int c)
{
	/* TO-DO: Deal with 2/3/4-byte chars. */
	return cob_is_xml_namestartchar (c) || c == '-' || c == '.' || isdigit (c)
		|| c == 0xb7;
}

/*
   check if string is a valid URI - may not contain trailing spaces
   URI = scheme:[//authority]path[?query][#fragment]
*/
int
cob_is_valid_uri (const char *str)
{
#if defined (WITH_XML2)
	int		is_valid;
	xmlURIPtr	p;

	p = xmlParseURI (str);
	is_valid = !!p;
	if (p) {
		xmlFreeURI (p);
	}

	return is_valid;
#else
	/* scheme must start with lower-strase */
	if (!str || *str <= 'a' || *str >= 'z') return 0;

	/* scheme completes with ":" */
	str++;
	while (*str && *str != ':') str++;

	/* check for "any scheme" with any path */
	if (*str == ':' && str[1]) return 1;

	return 0;
#endif
}

/* entry function for XML GENERATE (compat) */
void
cob_xml_generate (cob_field *out, cob_ml_tree *tree, cob_field *count,
	const int with_xml_dec, cob_field *ns, cob_field *ns_prefix)
{
	const char dp = COB_MODULE_PTR->decimal_point;
	cob_xml_generate_new (out, tree, count, with_xml_dec, ns, ns_prefix, dp);
}

static void xml_generate (cob_field *out, cob_ml_tree *tree, cob_field *count,
	const int with_xml_dec, const char *ns_data, cob_field *ns_prefix,
	const char decimal_point);

/* entry function for XML GENERATE */
void
cob_xml_generate_new (cob_field *out, cob_ml_tree *tree, cob_field *count,
	const int with_xml_dec, cob_field *ns, cob_field *ns_prefix,
	const char decimal_point)
{
	const char *ns_data;

	/* no field */
	if (!out || !tree) {
		set_xml_exception (XML_INTERNAL_ERROR);
		cob_fatal_error (COB_FERROR_CODEGEN);
	}
	/* LINKAGE or BASED item without data */
	if (!out->data) {
		set_xml_exception (XML_INTERNAL_ERROR);
		return;
	}
	/* likely a separate error case: emtpy variable length item */
	if (out->size == 0) {
		set_xml_exception (XML_INTERNAL_ERROR);
		return;
	}

	if (ns) {
		if (is_empty (ns)) {
			ns_data = NULL;
		} else if (has_invalid_xml_char (ns)) {
			set_xml_exception (XML_INVALID_NAMESPACE);
			return;
		} else {
			ns_data = get_trimmed_data (ns, &copy_data_as_string);
			if (!cob_is_valid_uri (ns_data)) {
				set_xml_exception (XML_INVALID_NAMESPACE);
				cob_free ((void *)ns_data);
				return;
			}
		}
	} else {
		ns_data = NULL;
	}

	if (ns_prefix) {
		if (is_empty (ns_prefix)) {
			ns_prefix = NULL;
		} else if (!is_valid_xml_name (ns_prefix)) {
			if (ns_data) {
				cob_free ((void *)ns_data);
			}
			set_xml_exception (XML_INVALID_NAMESPACE_PREFIX);
			return;
		}
	}
	xml_generate (out, tree, count, with_xml_dec, ns_data, ns_prefix, decimal_point);
	if (ns_data) {
		cob_free ((void *)ns_data);
	}
}

static void xml_parse (cob_field *in, cob_field *encoding, cob_field *validation,
	const int flags, struct xml_state *state);
static void xml_endDocument (void *);
static void xml_free_parse_memory (struct xml_state *state);

static void xml_process_next_event (struct xml_state *state);

/* entry function for XML PARSE */
int cob_xml_parse (cob_field *in, cob_field *encoding, cob_field *validation,
		const int flags, void **saved_state)
{
	struct xml_state *state;
	int xml_code;

	/* no state yet ? first call */
	if (*saved_state == NULL) {
		/* LCOV_EXCL_START */
		/* codegen error: register not available / setup correctly */
		if (!COB_MODULE_PTR->xml_code
		 || !COB_MODULE_PTR->xml_event
		 || COB_MODULE_PTR->xml_code->data == NULL
		 || COB_MODULE_PTR->xml_event->data == NULL
		/* XML event not of expected size - currently possible
		   with manual register defintion; prefer speed (fixed-length) over
		   adjustability in cobc for now as all implementations have that
		   as X(30) and we only fill up to 29 chars */
		 || COB_MODULE_PTR->xml_event->size != 30 ) {
			cob_fatal_error (COB_FERROR_CODEGEN);
		}
		/* no field */
		if (!in) {
#if 0	/* seems like a codegen error, which should not happen */
			set_xml_exception (XML_INTERNAL_ERROR);
			set_xml_event (EVENT_EXCEPTION);
			return -1;
#else
			cob_fatal_error (COB_FERROR_CODEGEN);
#endif
		}
		/* LCOV_EXCL_STOP */
		*saved_state = cob_malloc (sizeof (struct xml_state));
		memset (*saved_state, '\0', sizeof(struct xml_state));
		((struct xml_state *)*saved_state)->flags = flags;
		xml_code = 0;
	}

	state = (struct xml_state *)*saved_state;

	/* postponed loading of XML code to have codegen check (register setup)
	   up front */
	xml_code = get_xml_code ();

	/* initial setup of registers, ensuring they are available
	   in the processing procedure */
#if 0
	set_xml_text (0, "", 0);
	set_xml_namespace (0, "", 0, NULL, 0);
#endif

	/* LINKAGE or BASED item without data */
	if (!in->data) {
		state->last_xml_code = XML_INTERNAL_ERROR;
		set_xml_exception (XML_INTERNAL_ERROR);
		set_xml_event (EVENT_EXCEPTION);
		return 0;
	}

	if (encoding && is_empty (encoding)) {
		encoding = NULL;
	}

	/* parser function had fatal error */
	if (state->state == XML_PARSER_HAD_FATAL_ERROR) {
		set_xml_code (state->last_xml_code);
		xml_free_parse_memory (state);
		*saved_state = NULL;
		return 1;
	}

	/* parser had non-fatal error but the user did not reset it */
	if (state->state == XML_PARSER_HAD_NONFATAL_ERROR) {
		if (xml_code != 0) {
			/* TODO: recheck !COB_XML_XMLNSS has one
			  "Parses using the difference as the encoding value" */
			set_xml_code (state->last_xml_code);
			xml_free_parse_memory (state);
			*saved_state = NULL;
			return 1;
		} else {
			if (COB_MODULE_PTR->xml_mode == COB_XML_XMLNSS) {
				/* note: Next event is ATTRIBUTE-NAME or START-OF-ELEMENT */
				/* TODO: likely set appropriate instead of parsing more data */
			} else {
				/* TODO: runs with adjusted encoding */
			}
		}
	}

	/* user user-initiated exception condition (-1) */
	if (xml_code == -1) {
		/* xml code stays with one */
		xml_free_parse_memory (state);
		*saved_state = NULL;
		return 1;
	}

	/* we reached "end of input" (xmlss only?) and were not told to go on */
	if (state->state == XML_PARSER_HAD_END_OF_INPUT) {
		switch (xml_code) {
		case 0:
			xml_endDocument (state);
			if (state->eof == 0) {
				state->eof = 1;
			} else {
//				xml_code = 1;
				state->state == XML_PARSER_FINISHED;
			}
			break;
		case 1:
			/* goes on with parsing
			   note that since we are processing a new chunk
			   of the xml data, we need to set both data pointers */
			xml_code = 0;
			state->input_data_ptr = (const char*)in->data;
			state->input_data_end = state->input_data_ptr + in->size;
			state->state = XML_PARSER_STARTING_NEXT_CHUNK;
			break;
		default:
			/* fatal runtime error,
			   TODO: at least a runtime warning, likely runtime exit */
			cob_set_exception (COB_EC_XML);
			xml_free_parse_memory (state);
			*saved_state = NULL;
			return 1;
		}
	}

	/* empty item = no error, just "no data any more" */
	if (state->state != XML_PARSER_HAD_END_OF_DOCUMENT
	 && state->state != XML_PARSER_FINISHED
	 && is_empty (in)) {
		set_xml_event (EVENT_END_OF_INPUT);
		set_xml_code (XML_STMT_SUCCESSFULL);
		state->state = XML_PARSER_HAD_END_OF_INPUT;
		return 0;
	}

	if (xml_code != 0) {
		/* note: -1 is handled above, also 1 where possible */
		if (COB_MODULE_PTR->xml_mode == COB_XML_XMLNSS) {
			/* fatal runtime error,
			   TODO: at least a runtime warning, likely runtime exit */
			cob_set_exception (COB_EC_XML);
		} else {
			set_xml_code (-1);
		}
		xml_free_parse_memory (state);
		*saved_state = NULL;
		return 1;
	}

	/* we're done, and came back from the PROCESSING FUNCTION */
	if (state->state == XML_PARSER_FINISHED) {
		xml_free_parse_memory (state);
		*saved_state = NULL;
		return 1;
	}

	if (state->event
	 && state->event->event != EVENT_UNKNOWN) {
		/* if there are still events in the queue -> get next one */
		xml_process_next_event (state);
	} else {
		/* do actual parsing */
		xml_parse (in, encoding, validation, flags, state);
	}

	return 0;
}


static void
set_xml_code_parsing_error (const int libxml2_err) {
	int xml_err = 0x00000018 /* 24 COMP in split field per IBM doc */
				+ (libxml2_err << 8); /* second part with error number */
#ifdef WORDS_BIGENDIAN	/* CHECKME: is that correct? */
	xml_err = COB_BSWAP_32 (xml_err);
#endif
	memcpy (COB_MODULE_PTR->xml_code->data, &xml_err, sizeof (int));
}


#if defined (WITH_XML2)

/* actual handling of XML GENERATE */
void
xml_generate (cob_field *out, cob_ml_tree *tree, cob_field *count,
		  const int with_xml_dec, const char *ns_data, cob_field *ns_prefix,
		  const char decimal_point)
{
	xmlBufferPtr		buff;
	xmlTextWriterPtr	writer = NULL;
	int			status;
	unsigned int		chars_written = 0;
	xmlChar			*x_ns = NULL;
	xmlChar			*x_ns_prefix = NULL;
	int			buff_len;
	int			copy_len;
	int			num_newlines = 0;

	set_xml_code (XML_STMT_SUCCESSFULL);

	buff = xmlBufferCreate ();
	if (buff == NULL) {
		set_xml_exception (XML_INTERNAL_ERROR);
		goto end;
	}

	writer = xmlNewTextWriterMemory (buff, 0);
	if (writer == NULL) {
		goto end;
	}

	if (with_xml_dec) {
		/* TO-DO: Support encoding */
		status = xmlTextWriterStartDocument (writer, NULL, NULL, NULL);
		if (status < 0) {
			set_xml_exception (XML_INTERNAL_ERROR);
			goto end;
		} else {
			chars_written += status;
		}
	}

	if (ns_data) {
		x_ns = xmlCharStrdup (ns_data);
	}

	if (ns_prefix) {
		x_ns_prefix = get_trimmed_xml_data (ns_prefix);
	}

	status = generate_xml_from_tree (writer, tree, x_ns, x_ns_prefix,
				decimal_point, &chars_written);
	if (status < 0) {
		set_xml_exception (XML_INTERNAL_ERROR);
		goto end;
	}

	status = xmlTextWriterEndDocument (writer);
	if (status < 0) {
		set_xml_exception (XML_INTERNAL_ERROR);
		goto end;
	} else {
		chars_written += status;
	}

	/* Copy generated tree to output field */
	buff_len = xmlBufferLength (buff);
	copy_len = cob_min_int (buff_len, (int) out->size);
	memcpy (out->data, xmlBufferContent (buff), copy_len);
	memset (out->data + copy_len, ' ', out->size - copy_len);
	/* Remove trailing newlines */
	for (; copy_len > 0 && out->data[copy_len - 1] == '\n'; --copy_len) {
		out->data[copy_len - 1] = ' ';
		--chars_written;
		++num_newlines;
	}
	/* Raise exception if output field is too small */

	/* FIXME: the order is wrong!
	   in general _only_ the must be overwritten that has a valid generation;
	   and the *count should only be set this far; currently *count is set to
	   the full size and the complete data is inserted; instead only data up
	   to a tag end (opening or ending) should be copied */
	if (buff_len - num_newlines > copy_len) {
		set_xml_exception (XML_OUT_FIELD_TOO_SMALL);
		goto end;
	}

 end:
	if (x_ns) {
		xmlFree (x_ns);
	}
	if (x_ns_prefix) {
		xmlFree (x_ns_prefix);
	}
	if (writer) {
		xmlFreeTextWriter (writer);
	}
	if (buff) {
		xmlBufferFree (buff);
	}
	if (count) {
		/* FIXME: COUNT IN may never be bigger than the field size! See above. */
		/* TODO: for NATIONAL data (UTF-16): bytes / 2;
		         otherwise - including UTF-8 amount of bytes */
		cob_set_int (count, chars_written);
	}
}

static void
xml_error_handling (struct xml_state *state, const xmlError *err) {
	char err_code[5];
	new_xml_event (state, EVENT_EXCEPTION);
	add_xml_event_data (state,
						SREG_XML_TEXT,
						err->message,
						strlen (err->message) + 1);
	new_xml_event (state, EVENT_EXCEPTION);
	sprintf (err_code, "%4d", err->code);
	add_xml_event_data (state,
						SREG_XML_TEXT,
						err_code,
						5);

	/* CHECKME: Which other elements of the xmlError do we want to pass? */
#if 0 /* CHECKME: Do we want that? */
	state->state = XML_PARSER_HAD_NONFATAL_ERROR;
#endif
}

static void
xml_error_handler (void *ctx, LIBXML_CONST_ERROR_PTR err) {
	struct xml_state *parse_state = ctx;
	enum xml_parser_state state = parse_state->state;
	static int last_error_code = 0;

	/* suppress duplicate message */
	if (err->code == XML_SCHEMAP_FAILED_LOAD
	 && last_error_code == XML_IO_LOAD_ERROR) {
		last_error_code = err->code;
		return;
	}

	if (state == XML_PARSER_VALIDATION_SETUP
	 || state == XML_PARSER_VALIDATION_SETUP_MEM) {
		/* skip schema detail issues we are not interested in */
		if (err->code < XML_IO_UNKNOWN) {
			return;
	 	}
	 }

	switch (state) {
	case XML_PARSER_VALIDATION_SETUP:
		if (err->file) {
			cob_runtime_warning (_("XML PARSE setup for VALIDATE FILE %s:%d (%d): %s"),
				err->file, err->line, err->code, err->message);
		} else {
			cob_runtime_warning (_("XML PARSE setup for VALIDATE FILE (%d): %s"),
				err->code, err->message);
		}
		set_xml_event (EVENT_EXCEPTION);
		parse_state->last_xml_code = XML_PARSE_ERROR_FATAL;
		parse_state->state = XML_PARSER_HAD_FATAL_ERROR;
		set_xml_code_parsing_error (err->code);
		break;
	case XML_PARSER_VALIDATION_SETUP_MEM:
		cob_runtime_warning (_("XML PARSE setup for VALIDATE (%d): %s"),
			err->code, err->message);
		set_xml_event (EVENT_EXCEPTION);
		parse_state->last_xml_code = XML_PARSE_ERROR_FATAL;
		parse_state->state = XML_PARSER_HAD_FATAL_ERROR;
		set_xml_code_parsing_error (err->code);
		break;
	case XML_PARSER_JUST_STARTED:
	case XML_PARSER_DOCUMENT_START:
	case XML_PARSER_FINE:
	case XML_PARSER_HAD_NONFATAL_ERROR:
		xml_error_handling (parse_state, err);
		break;
	default:
		/* not translated as unplanned */
		cob_runtime_warning ("XML PARSE state %d on %s:%d (%d): %s",
			state, err->file, err->line, err->code, err->message);
	}

	last_error_code = err->code;
}

#endif	/* WITH_XML2 */

static void
xml_endDocument (void *ctx) {
	struct xml_state *state = ctx;
	new_xml_event (state, EVENT_END_OF_DOCUMENT);
	state->state = XML_PARSER_HAD_END_OF_DOCUMENT;
}

/* the following functions may be partially used with
   partially manual parsing (which could be useful for the testsuite),
   but currently, they aren't */
#if defined (WITH_XML2)

static void
xml_startDocument (void *ctx) {
	struct xml_state *state = ctx;
	xmlParserCtxtPtr ctxt = state->ctx;

#if LIBXML_VERSION >= 21200
	const xmlChar *encoding		= xmlCtxtGetDeclaredEncoding(ctxt);
	int standalone				= xmlCtxtGetStandalone(ctxt);
	const xmlChar *version		= xmlCtxtGetVersion(ctxt);

#else
	const xmlChar *encoding		= ctxt->encoding;
	int standalone				= ctxt->standalone;
	const xmlChar *version		= ctxt->version;
#endif

	new_xml_event (state, EVENT_START_OF_DOCUMENT);
	new_xml_event (state, EVENT_VERSION_INFORMATION);
	add_xml_event_data (state,
						SREG_XML_TEXT,
						version,
						xmlStrlen (version));
	new_xml_event (state, EVENT_ENCODING_DECLARATION);
	add_xml_event_data (state,
						SREG_XML_TEXT,
						encoding,
						xmlStrlen (encoding));
	state->state = XML_PARSER_DOCUMENT_START;

	switch (ctxt->standalone) {
		case 1 :
			new_xml_event (state, EVENT_STANDALONE_DECLARATION);
			add_xml_event_data (state,
								SREG_XML_TEXT,
								"YES",
								3);
			break;
		case 0 :
			new_xml_event (state, EVENT_STANDALONE_DECLARATION);
			add_xml_event_data (state,
								SREG_XML_TEXT,
								"no",
								2);
			break;
	}

}

static void
xml_endofInput (struct xml_state *state) {
	new_xml_event (state, EVENT_END_OF_INPUT);
}

static void
xml_comment (void *ctx, const xmlChar *content) {
	struct xml_state *state = ctx;
	new_xml_event (state, EVENT_COMMENT);
	add_xml_event_data (state,
						SREG_XML_TEXT,
						content,
						xmlStrlen (content));
}

static void
xml_processingInstruction (void *ctx,
							const xmlChar *target,
							const xmlChar *data) {
	struct xml_state *state = ctx;
	new_xml_event (state, EVENT_PROCESSING_INSTRUCTION_TARGET);
	add_xml_event_data (state,
						SREG_XML_TEXT,
						target,
						xmlStrlen (target));
	new_xml_event (state, EVENT_PROCESSING_INSTRUCTION_DATA);
	add_xml_event_data (state,
						SREG_XML_TEXT,
						data,
						xmlStrlen (data));
}

static void
xml_startElementNs (void *ctx,
		const xmlChar *localname, const xmlChar *prefix, const xmlChar *URI,
		int nb_namespaces, const xmlChar **namespaces,
		int nb_attributes, int nb_defaulted, const xmlChar **attributes) {
	int	cntr,	attr_value_len;
	struct xml_state *state = ctx;

	new_xml_event (state, EVENT_START_OF_ELEMENT);
	add_xml_event_data (state,
						SREG_XML_TEXT,
						localname,
						xmlStrlen (localname));
	/* TODO: cleanup and code namespace stuff and check what to do on endElement */
	if (prefix) {
		add_xml_event_data (state,
							SREG_XML_NS_PREFIX,
							prefix,
							xmlStrlen (prefix));
	}
	if (URI) {
		add_xml_event_data (state,
							SREG_XML_NAMESPACE,
							URI,
							xmlStrlen (URI));
	}

	/*	Now we start to process the NAMESPACE-DECLARATION's		*/
	if (namespaces != NULL) {
		for (cntr = 0; cntr < nb_namespaces * 2; cntr++) {
			new_xml_event (state, EVENT_NAMESPACE_DECLARATION);
			const xmlChar *nprefix = namespaces[cntr++];  // Get nprefix (even index)
			const xmlChar *nuri = namespaces[cntr];	   // Get URI (odd index)
		/*	Handle default namespace (nprefix is NULL)		*/
			if (nuri) {
				add_xml_event_data (state,
									SREG_XML_NAMESPACE,
									nuri,
									xmlStrlen (nuri));
			}
			if (nprefix) {
				add_xml_event_data (state,
									SREG_XML_NS_PREFIX,
									nprefix,
									xmlStrlen (nprefix));
			}
		}
	}

	/*	Process each attribute									*/
	for (cntr = 0; cntr < nb_attributes * 5; cntr += 5) {
		const xmlChar *attr_name = attributes[cntr];		   // ATTRIBUTE-NAME
		const xmlChar *attr_prefix = attributes[cntr + 1];	 // ATTRIBUTE-NAMESPACE
		const xmlChar *attr_value_start = attributes[cntr + 3]; // ATTRIBUTE-CHARACTERS start
		const xmlChar *attr_value_end = attributes[cntr + 4];   // ATTRIBUTE-CHARACTERS end
		
		// Calculate attribute value length
		attr_value_len = attr_value_end - attr_value_start;
		
		// Use the extracted information
		new_xml_event (state, EVENT_ATTRIBUTE_NAME);
		add_xml_event_data (state,
							SREG_XML_TEXT,
							attr_name,
							xmlStrlen (attr_name));
		new_xml_event (state, EVENT_ATTRIBUTE_CHARACTERS);
		add_xml_event_data (state,
							SREG_XML_TEXT,
							attr_value_start,
							attr_value_len);
	}
}

static void
xml_endElementNs (void *ctx,
		const xmlChar *localname, const xmlChar *prefix, const xmlChar *URI) {
	struct xml_state *state = ctx;
	new_xml_event (state, EVENT_END_OF_ELEMENT);
	add_xml_event_data (state, 
						SREG_XML_TEXT,
						localname,
						xmlStrlen (localname));
	/* TODO: cleanup and code namespace stuff and check what to do on endElement */
	if (prefix) {
		add_xml_event_data (state,
							SREG_XML_NS_PREFIX,
							prefix,
							xmlStrlen (prefix));
	}
	if (URI) {
		add_xml_event_data (state,
							SREG_XML_NAMESPACE,
							URI,
							xmlStrlen (URI));
	}
}

static void
xml_startElement (void *ctx, const xmlChar *name, const xmlChar **atts) {
	struct xml_state *state = ctx;
	new_xml_event (state, EVENT_START_OF_ELEMENT);
	add_xml_event_data (state,
						SREG_XML_TEXT,
						name,
						xmlStrlen (name));
}

static void
xml_endElement (void *ctx, const xmlChar *name) {
	struct xml_state *state = ctx;
	new_xml_event (state, EVENT_END_OF_ELEMENT);
	add_xml_event_data (state,
						SREG_XML_TEXT,
						name,
						xmlStrlen (name));
}

static void
xml_characters (void *ctx, const xmlChar *content, int len) {
	struct xml_state *state = ctx;
	new_xml_event (state, EVENT_CONTENT_CHARACTERS);
	add_xml_event_data (state,
						SREG_XML_TEXT,
						content,
						len);
}

static void
myStructuredErrorHandler(void *ctx, const xmlError *error) {

	struct xml_state *state = ctx;
	static int errorCount = 0;
	int		i, len;
	if (error->level == XML_ERR_ERROR || error->level == XML_ERR_FATAL) {
		errorCount++;
	}

#if LIBXML_VERSION >= 21400  // 2.14.0  
	if (error->code == XML_WAR_ENCODING_MISMATCH) {
#else
	if (error->code == 113) {
#endif
		fprintf(stderr, "WARNING: Encoding mismatch detected!\n");
		fprintf(stderr, "Message: %s\n", error->message);
		if (error->str1) {
			fprintf(stderr, "Declared encoding: %s\n", error->str1);
		}
		if (error->str2) {
			fprintf(stderr, "Auto-detected encoding: %s\n", error->str2);
		}
		return;
	}  

	if 	((errorCount == 1) &&
		(error->code == XML_ERR_XMLDECL_NOT_FINISHED ||
		error->code == XML_ERR_SPACE_REQUIRED)) {
		len = state->input_data_end - state->input_data_ptr;
#ifdef	_WIN32
		fprintf(stderr, 
				"Encoding declaration '%s' appears incompatible with input data\n"
				"The current runtime character encoding is %s \n"
				"This caused XML declaration parsing to fail with: %s",
				state->ctx->encoding, locale_charset(), error->message);
#else
		fprintf(stderr, 
				"Encoding declaration '%s' appears incompatible with input data\n"
				"The current runtime character encoding does not match  \n"
				"This caused XML declaration parsing to fail with: %s",
				state->ctx->encoding, error->message);
#endif
		fprintf(stderr, 
				"The first 5 characters in HEX are X'");
		if (len > 10) {
			len = 10;
		}
		for (i = 0; i < 5; i++) {
			printf("%02x", (unsigned char)state->input_data_ptr[i]);
		}
		printf("'\n");
	}

	if (error->code == XML_ERR_INVALID_ENCODING) {
		fprintf(stderr, "Encoding Error: %s\n", error->message);
		if (error->file) {
			fprintf(stderr, "File: %s\n", error->file);
		}
		if (error->line > 0) {
			fprintf(stderr, "Line: %d", error->line);
			if (error->int2 > 0) {
				fprintf(stderr, ", Column: %d", error->int2);
			}
			fprintf(stderr, "\n");
		}
		
		// Display problematic bytes if available
		if (error->str1) {
			fprintf(stderr, "Context: %s\n", error->str1);
		}
	} else {
		fprintf(stderr, 
				"XML Error ==> %d %s \n",
					error->code,
					error->message);
	}

	if (error->level == XML_ERR_FATAL) {
		state->last_xml_code = error->code;
		new_xml_event (state, EVENT_EXCEPTION);
		len = state->input_data_end - state->input_data_ptr;
		if (len > 100) {
			len = 100;
		}
		add_xml_event_data (state,
							SREG_XML_TEXT,
							state->input_data_ptr,
							len);
	}
}

static void
xml_internalSubset(void *ctx,
					const xmlChar *name,
					const xmlChar *ExternalID,
					const xmlChar *SystemID) {
	struct xml_state *state = ctx;

	if (state == NULL || name == NULL) {
		return;
	}
	new_xml_event (state, EVENT_DOCUMENT_TYPE_DECLARATION);
	add_xml_event_data (state,
						SREG_XML_TEXT,
						name,
						xmlStrlen (name));
}

static void
xml_cdata (void *ctx, const xmlChar *content, int len) {
	struct xml_state *state = ctx;
	new_xml_event (state, EVENT_START_OF_CDATA_SECTION);
	if (COB_MODULE_PTR->xml_mode == COB_XML_COMPAT) {
		add_xml_event_data (state,
							SREG_XML_TEXT,
							"<![CDATA[",
							9);
	}
	new_xml_event (state, EVENT_CONTENT_CHARACTERS);
	add_xml_event_data (state,
						SREG_XML_TEXT,
						content,
						len);

	new_xml_event (state, EVENT_END_OF_CDATA_SECTION);
	if (COB_MODULE_PTR->xml_mode == COB_XML_COMPAT) {
		add_xml_event_data (state,
							SREG_XML_TEXT,
							"]]>",
							3);
	}
}
#endif /* defined (WITH_XML2) */

#if defined (WITH_XML2)
/* actual handling of XML PARSE (not implemented yet) */
void xml_parse (cob_field *in, cob_field *encoding, cob_field *validation,
		const int flags, struct xml_state *state)
{
	static int first_xml = 1;
	const xmlError *error;

	if (state->ctx == NULL) {
		char	*enc = NULL;
		if (encoding) {
			/* CHECKME: is there a reasonable array size to use instead? */
			enc = cob_get_picx (encoding->data, encoding->size, NULL, 0);
		}

		/* setup sax-parser callbacks */
		memset(&state->sax, 0, sizeof(xmlSAXHandler));

		/* do NOT use xmlSAXVersion(&state->sax, 2);
			only set the callbacks that we need to use
			All other callbacks remain NULL from memset		*/

		state->sax.startDocument = xml_startDocument;
		state->sax.endDocument = xml_endDocument;
		state->sax.comment = xml_comment;

		if (COB_MODULE_PTR->xml_mode == COB_XML_XMLNSS) {
			state->sax.initialized = XML_SAX2_MAGIC;
			state->sax.startElementNs = xml_startElementNs;
			state->sax.endElementNs = xml_endElementNs;
		} else {
			state->sax.startElement = xml_startElement;
			state->sax.endElement = xml_endElement;
		}
		state->sax.internalSubset = xml_internalSubset;
		state->sax.cdataBlock = xml_cdata;
		state->sax.endElement = xml_endElement;
		state->sax.processingInstruction = xml_processingInstruction;

		state->sax.characters = xml_characters;
		state->sax.serror = myStructuredErrorHandler;

		/*
		 * The document being in memory, it have no base per RFC 2396,
		 * and the "noname.xml" argument will serve as its base.
		*/
		state->ctx = xmlCreatePushParserCtxt (&state->sax, state,
			NULL, 0, "noname.xml");

		// Add this immediately after creating the context:  
		if (state->ctx != NULL) {
#if LIBXML_VERSION >= 21200
			int options = xmlCtxtGetOptions(state->ctx);
#else
			int options = state->ctx->options;
#endif
			options &= ~XML_PARSE_NOWARNING;		/* Clear the NOWARNING flag */
			options &= ~XML_PARSE_NOERROR;			/* Also clear NOERROR flag */
#if LIBXML_VERSION >= 21200
			xmlCtxtSetOptions(state->ctx, options);  
#else
			state->ctx->options = options;
#endif
		}

		state->input_data_ptr = (const char*)in->data;
		state->input_data_end = state->input_data_ptr + in->size;

		if (enc) {
			/* TODO (later): handle encoding */
			cob_free (enc);
		}
		if (state->ctx == NULL) {
			state->last_xml_code = XML_PARSE_ERROR_FATAL;
			state->state = XML_PARSER_HAD_FATAL_ERROR;
			if (COB_MODULE_PTR->xml_mode == COB_XML_XMLNSS) {
				set_xml_exception (XML_PARSE_ERROR_FATAL);
			} else {
				set_xml_exception (XML_PARSE_ERROR_MISC_COMPAT);
			}
			set_xml_event (EVENT_EXCEPTION);
			return;
		}

		/* setup global error handler for every domain that hasn't its own */
		xmlSetStructuredErrorFunc (state, xml_error_handler);

		if (validation) {
			xmlSchemaParserCtxtPtr schema_ctx;

			/* use of empty data or,
			   what should be catched with -fec=all up front,
			   LINKAGE / BASED item without data */
			if (validation->data == NULL
			 || is_empty (validation)) {
				state->last_xml_code = XML_INVALID_NAMESPACE;
				set_xml_exception (XML_INVALID_NAMESPACE);
				state->state = XML_PARSER_HAD_FATAL_ERROR;
				return;
			}

			/* create parser context from file or memory */
			if (flags & COB_XML_PARSE_VALIDATE_FILE) {
				const char	*file_name = cob_setup_filename (validation);
				state->state = XML_PARSER_VALIDATION_SETUP;
				schema_ctx = xmlSchemaNewParserCtxt (file_name);
			} else {
				state->state = XML_PARSER_VALIDATION_SETUP_MEM;
				schema_ctx = xmlSchemaNewMemParserCtxt ((const char *)validation->data,validation->size);
			}
			/* parse and compile the schema */
			if (schema_ctx) {
				xmlSchemaSetParserStructuredErrors (schema_ctx, xml_error_handler, state);
				state->schema = xmlSchemaParse (schema_ctx);
				/* free context used to compile the schema */
				xmlSchemaFreeParserCtxt (schema_ctx);
			}

			if (state->schema == NULL) {
				/* don't override catched and handled errors */
				if (state->state != XML_PARSER_HAD_FATAL_ERROR) {
					state->last_xml_code = XML_INVALID_NAMESPACE;
					state->state = XML_PARSER_HAD_FATAL_ERROR;
					set_xml_exception (XML_INVALID_NAMESPACE);
				}
				return;
			}

			/* get validation context and plug it into the parser */
			state->val_ctx = xmlSchemaNewValidCtxt (state->schema);
			if (state->val_ctx == NULL) {
				xmlSchemaFree (state->schema);
				state->schema = NULL;
				/* don't override catched and handled errors */
				if (state->state != XML_PARSER_HAD_FATAL_ERROR) {
					state->last_xml_code = XML_INVALID_NAMESPACE;
					state->state = XML_PARSER_HAD_FATAL_ERROR;
					set_xml_exception (XML_INVALID_NAMESPACE);
				}
				return;
			}
			state->xsd_plug = xmlSchemaSAXPlug (state->val_ctx,
				&(state->ctx->sax), &(state->ctx->userData));

			/* Note: the call above goes wrong if anything is not setup correctly
			   for example if there's a mix of XMLCOMPAT and validition */
			if (state->xsd_plug == NULL) {
				xmlSchemaFreeValidCtxt (state->val_ctx);
				state->val_ctx = NULL;
				xmlSchemaFree (state->schema);
				state->schema = NULL;
				state->last_xml_code = XML_INTERNAL_ERROR;
				state->state = XML_PARSER_HAD_FATAL_ERROR;
				set_xml_exception (XML_INTERNAL_ERROR);
				return;
			}
		}

		state->buff = cob_malloc (COB_LARGE_BUFF);
		state->buff_len = COB_LARGE_BUFF;

		state->state = XML_PARSER_JUST_STARTED;
	}

#if 0
	if (first_xml) {
		first_xml = 0;
		cob_runtime_warning (_("%s is unfinished"),
			"XML PARSE");
	}
#endif

	/* unset existing events, allowing re-use*/
	{
		struct xml_event *event;
		for (event = state->first_event; event; event = event->next) {
			event->event = EVENT_UNKNOWN;
		}
	}
	state->event = state->first_event;
	state->buff_off = 0;

	while (state->event == NULL
	    || state->event->event == EVENT_UNKNOWN) {
		const int end_of_parsing = state->input_data_ptr >= state->input_data_end;
		int size = state->input_data_end - state->input_data_ptr;
		if (size > 100) {
			size = 100;
		}
		if (state->eof) {
			state->err = xmlParseChunk (state->ctx, state->input_data_ptr, 0, 1);
			break;
		} else if (!end_of_parsing){
			state->err = xmlParseChunk (state->ctx, state->input_data_ptr, size, end_of_parsing);
#if 0
			if (state->err) {
				error = xmlGetLastError ();
				fprintf(stderr, "xmlParseChunk returned error %d ==> %s \n",
					state->err,
					error->message);
			}
#endif
			state->input_data_ptr += size;
		} else {
			break;
		}
	}

	if (state->input_data_ptr >= state->input_data_end) {
		if (state->eof) {
			state->state = XML_PARSER_FINISHED;
		} else {
			xml_endofInput (state);
		}
	}

#if 0
	{
		struct xml_event *event = state->first_event;
		for (;event && event->event != EVENT_UNKNOWN; event = event->next) {
			printf("Event ==> %30.*s \n",
				xml_event_name_len[event->event],
				(unsigned char *)xml_event_name[event->event]);
		}
	}
#endif

	state->event = state->first_event;
	xml_process_next_event (state);
}

#endif /* defined (WITH_XML2) */

/* processing of parsed XML events from the queue */
void
xml_process_next_event (struct xml_state *state)
{
	struct xml_event *event = state->event;
	const int ntext = state->flags & COB_XML_PARSE_NATIONAL;

	/*	First set all XML registers to zero length		*/

	cob_set_int (COB_MODULE_PTR->xml_information, (int) 1);
	COB_MODULE_PTR->xml_namespace->size = 0;
	COB_MODULE_PTR->xml_namespace_prefix->size = 0;
	COB_MODULE_PTR->xml_nnamespace->size = 0;
	COB_MODULE_PTR->xml_nnamespace_prefix->size = 0;
	COB_MODULE_PTR->xml_text->size = 0;
	COB_MODULE_PTR->xml_ntext->size = 0;

	COB_MODULE_PTR->xml_namespace->data = NULL;
	COB_MODULE_PTR->xml_namespace_prefix->data = NULL;
	COB_MODULE_PTR->xml_nnamespace->data = NULL;
	COB_MODULE_PTR->xml_nnamespace_prefix->data = NULL;
	COB_MODULE_PTR->xml_text->data = NULL;
	COB_MODULE_PTR->xml_ntext->data = NULL;

	if (event->event == EVENT_END_OF_INPUT && !state->eof) {
		state->state = XML_PARSER_HAD_END_OF_INPUT;
	}

	set_xml_event (event->event);
	if (state->last_xml_code) {
		set_xml_code (state->last_xml_code);
	} else {
	set_xml_code (0);
	}

	if (ntext) {
		COB_MODULE_PTR->xml_ntext->size = event->text_len;
		COB_MODULE_PTR->xml_ntext->data = event->text_ptr;
		COB_MODULE_PTR->xml_nnamespace->size = event->namespace_len;
		COB_MODULE_PTR->xml_nnamespace->data = event->namespace_ptr;
		COB_MODULE_PTR->xml_nnamespace_prefix->size = event->prefix_len;
		COB_MODULE_PTR->xml_nnamespace_prefix->data = event->prefix_ptr;
	} else {
		COB_MODULE_PTR->xml_text->size = event->text_len;
		COB_MODULE_PTR->xml_text->data = event->text_ptr;
		COB_MODULE_PTR->xml_namespace->size = event->namespace_len;
		COB_MODULE_PTR->xml_namespace->data = event->namespace_ptr;
		COB_MODULE_PTR->xml_namespace_prefix->size = event->prefix_len;
		COB_MODULE_PTR->xml_namespace_prefix->data = event->prefix_ptr;
	}

	state->event = event->next;
}

#if defined (WITH_XML2)

void xml_free_parse_memory (struct xml_state* state)
{
	if (state->ctx) {
		if (state->xsd_plug) {
			xmlSchemaSAXUnplug (state->xsd_plug);
			xmlSchemaFreeValidCtxt (state->val_ctx);
			xmlSchemaFree (state->schema);
		}
		if (state->ctx->myDoc) {
			xmlFreeDoc (state->ctx->myDoc);
		}
		xmlFreeParserCtxt (state->ctx);
	}
	if (state->buff) {
		cob_free (state->buff);
	}
	{
		struct xml_event *event = state->first_event;
		while (event) {
			struct xml_event *next = event->next;
			cob_free (event);
			event = next;
		}
	}
	cob_free (state);
}

#else /* !defined (WITH_XML2) */

/* actual (non) handling of XML GENERATE */
void
xml_generate (cob_field *out, cob_ml_tree *tree, cob_field *count,
		  const int with_xml_dec, const char *ns_data, cob_field *ns_prefix,
		  const char decimal_point)
{
	static int first_xml = 1;

	COB_UNUSED (out);
	COB_UNUSED (tree);
	COB_UNUSED (count);
	COB_UNUSED (with_xml_dec);
	COB_UNUSED (ns_data);
	COB_UNUSED (ns_prefix);
	COB_UNUSED (decimal_point);
	if (first_xml) {
		first_xml = 0;
		cob_runtime_warning (_("runtime is not configured to support %s"),
			"XML");
	}
	set_xml_exception (XML_INTERNAL_ERROR);
	cob_add_exception (COB_EC_IMP_FEATURE_DISABLED);
}

/* actual (non) handling of XML PARSE */
void xml_parse (cob_field *in, cob_field *encoding, cob_field *validation,
		const int flags, struct xml_state *state)
{
	static int first_xml = 1;

	COB_UNUSED (in);
	COB_UNUSED (encoding);
	COB_UNUSED (validation);
	COB_UNUSED (flags);

	if (first_xml) {
		first_xml = 0;
		cob_runtime_warning (_("runtime is not configured to support %s"),
			"XML");
	}

	state->last_xml_code = XML_INTERNAL_ERROR;
	set_xml_exception (XML_INTERNAL_ERROR);
	cob_add_exception (COB_EC_IMP_FEATURE_DISABLED);
	set_xml_event (EVENT_EXCEPTION);
	state->state = XML_PARSER_HAD_FATAL_ERROR;
}

void xml_free_parse_memory (struct xml_state* state)
{
	cob_free (state);
}

#endif

/* entry function for JSON GENERATE (compat) */
void
cob_json_generate (cob_field *out, cob_ml_tree *tree, cob_field *count)
{
	const char dp = COB_MODULE_PTR->decimal_point;
	cob_json_generate_new (out, tree, count, dp);
}

#if defined (WITH_CJSON) || defined (WITH_JSON_C)

#if defined (WITH_JSON_C) && !defined(NO_JSON)
	COB_TLS json_object	*json = NULL;
#endif
/* entry function for JSON GENERATE */
void
cob_json_generate_new (cob_field *out, cob_ml_tree *tree, cob_field *count,
		   const char decimal_point)
{
	const char	*printed_json = NULL;
	size_t	print_len = 0;
	size_t	copy_len;
	int	num_newlines = 0;
	int	status = 0;

#if defined (WITH_CJSON)
	cJSON	*json = cJSON_CreateObject ();

	if (!json) {
		set_json_exception (JSON_INTERNAL_ERROR);
		goto end;
	}

	status = generate_json_from_tree (tree, decimal_point, json);
	if (status < 0) {
		set_json_exception (JSON_INTERNAL_ERROR);
		goto end;
	}

	/* TO-DO: Set cJSON to use cob_free in InitHook? */
	printed_json = (const char *) cJSON_PrintUnformatted (json);

	if (!printed_json) {
		set_json_exception (JSON_INTERNAL_ERROR);
		goto end;
	}
	print_len = strlen (printed_json);

#elif defined (WITH_JSON_C)
#if defined (JSON_CACHING_OFF)
	json = json_object_new_object ();
#else
	if (json == NULL) {
		json = json_object_new_object ();
	} else {
		/* before reusing (second+ iterations), remove all sub-elements,
		   note: those used the main's print buffer, which stays allocated */
		json_object_object_foreach (json, key, val) {
			COB_UNUSED (val);
			json_object_object_del (json, key);
		}
	}
#endif

	status = generate_json_from_tree (tree, decimal_point, json);
	if (status < 0) {
		set_json_exception (JSON_INTERNAL_ERROR);
		goto end;
	}

	printed_json = json_object_to_json_string_length (json,
			JSON_C_TO_STRING_PLAIN, &print_len);

	if (!printed_json) {
		set_json_exception (JSON_INTERNAL_ERROR);
		goto end;
	}
#endif

	/* TO-DO: Duplication! */
	copy_len = cob_min_int ((int) print_len, (int) out->size);
	memcpy (out->data, printed_json, copy_len);
	memset (out->data + copy_len, ' ', out->size - copy_len);
	/* Remove trailing newlines */
	for (; copy_len > 0 && out->data[copy_len - 1] == '\n'; --copy_len) {
		out->data[copy_len - 1] = ' ';
		--print_len;
		++num_newlines;
	}
	/* Raise exception if output field is too small */
	if (print_len - num_newlines > copy_len) {
		set_json_exception (JSON_OUT_FIELD_TOO_SMALL);
		goto end;
	}

	set_json_code (0);

 end:
#if defined (WITH_CJSON)
	if (printed_json) {
		cJSON_free ((void *)printed_json);
	}
	if (json) {
		cJSON_Delete (json);
	}
#elif defined (WITH_JSON_C)
#if defined (JSON_CACHING_OFF)
	if (json) {
		json_object_put (json);
		json = NULL;
	}
#endif
#endif
	if (count) {
		/* FIXME: COUNT IN may never be bigger than the field size! See above. */

		/* TODO: for NATIONAL data (UTF-16): bytes / 2;
		         otherwise - including UTF-8 amount of bytes */
		cob_set_int (count, print_len);
	}
}

#else /* no JSON */

/* entry function for JSON GENERATE (not handled) */
void
cob_json_generate_new (cob_field *out, cob_ml_tree *tree, cob_field *count,
		   const char decimal_point)
{
	static int first_json = 1;

	COB_UNUSED (out);
	COB_UNUSED (tree);
	COB_UNUSED (count);
	COB_UNUSED (decimal_point);

	if (first_json) {
		first_json = 0;
		cob_runtime_warning (_("runtime is not configured to support %s"),
			"JSON");
	}
	set_json_exception (JSON_INTERNAL_ERROR);
	cob_add_exception (COB_EC_IMP_FEATURE_DISABLED);
}

#endif

void
cob_init_mlio (cob_global * const g)
{
#if defined (WITH_XML2)
	LIBXML_TEST_VERSION
#endif
#ifndef	HAVE_DESIGNATED_INITS
	init_xml_event_list ();
#endif
	cobglobptr = g;
}


#ifndef	HAVE_DESIGNATED_INITS
void
init_xml_event_list (void)
{
	xml_event_name[EVENT_UNKNOWN] = "UNKNOWN";
#define COB_XML_EVENT(ename,str) \
	xml_event_name[ename] = str;
#include "xmlevent.def"
#undef COB_XML_EVENT

	xml_event_name_len[EVENT_UNKNOWN] = sizeof ("UNKNOWN") - 1;
#define COB_XML_EVENT(ename,str) \
	xml_event_name_len[ename] = sizeof (str) - 1;
#include "xmlevent.def"
#undef COB_XML_EVENT
}
#endif

void
cob_exit_mlio (void)
{
#if defined (WITH_XML2)
	xmlCleanupParser ();
#endif
#if defined (WITH_JSON_C) && !defined (JSON_CACHING_OFF)
	if (json) {
	 	json_object_put (json);
		json = NULL;
	}
#endif
}

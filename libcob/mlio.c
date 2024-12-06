/*
   Copyright (C) 2018-2023 Free Software Foundation, Inc.
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

#include <string.h>
#include <stddef.h>
#include <ctype.h>
#include <stdio.h>

/* include internal and external libcob definitions, forcing exports */
#define	COB_LIB_EXPIMP
#include "coblocal.h"

#if defined (WITH_XML2)
#include <libxml/xmlversion.h>
#include <libxml/xmlwriter.h>
#include <libxml/uri.h>
#include <libxml/parser.h>
#include <libxml/tree.h>
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
#include <json-c/json.h>
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

enum xml_parser_state {
	XML_PARSER_NOT_STARTED = 0,
	XML_PARSER_JUST_STARTED,
	XML_PARSER_HAD_END_OF_INPUT,
	XML_PARSER_FINE,
	XML_PARSER_HAD_NONFATAL_ERROR,
	XML_PARSER_HAD_FATAL_ERROR,
	XML_PARSER_FINISHED
};

enum json_code_status {
	JSON_OUT_FIELD_TOO_SMALL = 1,
	JSON_INTERNAL_ERROR = 500
};

/* content found in special register XML-EVENT */
#define EVENT_ATTRIBUTE_CHARACTER				"ATTRIBUTE-CHARACTER"
#define EVENT_ATTRIBUTE_CHARACTERS				"ATTRIBUTE-CHARACTERS"
#define EVENT_ATTRIBUTE_NAME					"ATTRIBUTE-NAME"
#define EVENT_ATTRIBUTE_NATIONAL_CHARACTER		"ATTRIBUTE-NATIONAL-CHARACTER"
#define EVENT_COMMENT							"COMMENT"
#define EVENT_CONTENT_CHARACTER					"CONTENT-CHARACTER"
#define EVENT_CONTENT_CHARACTERS				"CONTENT-CHARACTERS"
#define EVENT_CONTENT_NATIONAL_CHARACTER		"CONTENT-NATIONAL-CHARACTER"
#define EVENT_DOCUMENT_TYPE_DECLARATION			"DOCUMENT-TYPE-DECLARATION"
#define EVENT_ENCODING_DECLARATION				"ENCODING-DECLARATION"
#define EVENT_END_OF_CDATA_SECTION				"END-OF-CDATA-SECTION"
#define EVENT_END_OF_DOCUMENT					"END-OF-DOCUMENT"
#define EVENT_END_OF_ELEMENT					"END-OF-ELEMENT"
#define EVENT_END_OF_INPUT						"END-OF-INPUT"
#define EVENT_EXCEPTION							"EXCEPTION"
#define EVENT_NAMESPACE_DECLARATION				"NAMESPACE-DECLARATION"
#define EVENT_PROCESSING_INSTRUCTION_DATA		"PROCESSING-INSTRUCTION-DATA"
#define EVENT_PROCESSING_INSTRUCTION_TARGET		"PROCESSING-INSTRUCTION-TARGET"
#define EVENT_STANDALONE_DECLARATION			"STANDALONE-DECLARATION"
#define EVENT_START_OF_CDATA_SECTION			"START-OF-CDATA-SECTION"
#define EVENT_START_OF_DOCUMENT					"START-OF-DOCUMENT"
#define EVENT_START_OF_ELEMENT					"START-OF-ELEMENT"
#define EVENT_UNKNOWN_REFERENCE_IN_ATTRIBUTE	"UNKNOWN-REFERENCE-IN-ATTRIBUTE"
#define EVENT_UNKNOWN_REFERENCE_IN_CONTENT		"UNKNOWN-REFERENCE-IN-CONTENT"
#define EVENT_UNRESOLVED_REFERENCE				"UNRESOLVED-REFERENCE"
#define EVENT_VERSION_INFORMATION				"VERSION-INFORMATION"

static cob_global		*cobglobptr;

/* Local functions */

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
set_xml_event (const char *data)
{
	/* note: it is up to the compiler to ensure that this constant
	   is read-only (and therefore no overwriting of const data happens) */
	COB_MODULE_PTR->xml_event->data = (unsigned char *) data;
	COB_MODULE_PTR->xml_event->size = strlen (data);
}

/* set special registers XML-TEXT / XML-NTEXT
   the size is calculated if not explicit specified (size -> -1) */
static void
set_xml_text (const int ntext, const void *data, size_t size)
{
	/* note: it is up to the compiler to ensure that these constants
	   are read-only (and therefore no overwriting of const data happens) */
	if (ntext) {
		/* FIXME (later): ensure in the caller that data is UTF-16
			(or the specified national character set) and swap call from strlen */
		COB_MODULE_PTR->xml_ntext->data = (unsigned char *) data;
		COB_MODULE_PTR->xml_ntext->size = size != -1 ? size : strlen (data);
		COB_MODULE_PTR->xml_text->data = (unsigned char *) "";
		COB_MODULE_PTR->xml_text->size = 0;
	} else {
		/* XML-NTEXT and other XML-N... special registers are not available with ACUCOBOL */
		if (COB_MODULE_PTR->xml_ntext) {
			COB_MODULE_PTR->xml_ntext->data = (unsigned char *) "";
			COB_MODULE_PTR->xml_ntext->size = 0;
		}
		COB_MODULE_PTR->xml_text->data = (unsigned char *) data;
		COB_MODULE_PTR->xml_text->size = size != -1 ? size : strlen (data);
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
	char *ptr = cob_malloc (size + 1);
	if (!ptr) {
		return NULL;
	}
	memcpy (ptr, data, size);
	ptr[size] = 0;
	return (void *)ptr;
}

/* returns a duplicate of the given cob_field's data,
   right trimmed with no JUSTIFIED RIGHT, left-trimmed otherwise,
   returns pointer to single space if empty (or variable lenght zero) */
static void *
get_trimmed_data (const cob_field * const f,
	void * (*strndup_func)(const char *, size_t))
{
	size_t	len = f->size;
	char	*str;

	if (len == 0) {
		return (*strndup_func)(" ", 1);
	}
	
	str = (char *)f->data;
	if (COB_FIELD_JUSTIFIED (f)) {
		for (; *str == ' ' && len > 1; ++str, --len);
	} else {
		for (; (str[len - 1] == ' ' || str[len - 1] == 0) && len > 1; --len);
	}

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
get_pic_for_num_field (const size_t num_int_digits, const size_t num_dec_digits)
{
	size_t	num_pic_symbols = (size_t)2 + (2 * !!num_dec_digits) + 1;
	cob_pic_symbol	*pic = cob_malloc (num_pic_symbols * sizeof (cob_pic_symbol));
	cob_pic_symbol	*symbol = pic;

	symbol->symbol = '-';
	symbol->times_repeated = cob_max_int ((int) num_int_digits, 1);
	++symbol;

	symbol->symbol = '9';
	symbol->times_repeated = 1;
	++symbol;

	if (num_dec_digits) {
		symbol->symbol = COB_MODULE_PTR->decimal_point;
		symbol->times_repeated = 1;
		++symbol;

		symbol->symbol = '9';
		symbol->times_repeated = (int) num_dec_digits;
		++symbol;
	}

	symbol->symbol = '\0';

	return pic;
}

static void *
get_num (cob_field * const f, void * (*strndup_func)(const char *, size_t),
	 const char decimal_point)
{
	size_t		num_integer_digits
		= cob_max_int (0, COB_FIELD_DIGITS (f) - COB_FIELD_SCALE (f));
	size_t		num_decimal_digits
		= cob_max_int (0, COB_FIELD_SCALE (f));
	cob_field_attr	attr;
	cob_field       edited_field;
	char		*dp_pos;
	void		*num;

	/* TODO: add test cases with PPP99 and 9PPP to verify it works "as expected" */

	/* Initialize attribute for nicely edited version of f */
	attr.type = COB_TYPE_NUMERIC_EDITED;
	attr.flags = COB_FLAG_JUSTIFIED;
	attr.scale = COB_FIELD_SCALE (f);
	attr.digits = COB_FIELD_DIGITS (f);
	attr.pic = get_pic_for_num_field (num_integer_digits,
					  num_decimal_digits);

	/* Initialize field for nicely edited version */
	edited_field.attr = &attr;
	edited_field.size = cob_max_int (2, (int) num_integer_digits + 1);
	if (num_decimal_digits) {
		edited_field.size += 1 + num_decimal_digits;
	}
	edited_field.data = cob_malloc (edited_field.size);

	/* Set field */
	cob_move (f, &edited_field);

	/* Replace decimal point in num with given decimal_point */
	dp_pos = memchr (edited_field.data, COB_MODULE_PTR->decimal_point,
			 edited_field.size);
	if (dp_pos) {
		*dp_pos = decimal_point;
	}

	/* Trim output and clean up */
	num = get_trimmed_data (&edited_field, strndup_func);

	cob_free (edited_field.data);
	cob_free ((void *) edited_field.attr->pic);

	return num;

}
#endif

#if WITH_XML2


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

#define IF_NEG_RETURN_ELSE_COUNT(func)			\
	do {						\
		int	macro_status = (func);		\
		if (macro_status < 0) {			\
			return macro_status;			\
		} else {				\
			*count += macro_status;		\
		}					\
	} ONCE_COB

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

	hex_name = get_name_with_hex_prefix (attr->name);
	value = get_hex_xml_data (attr->value);
	IF_NEG_RETURN_ELSE_COUNT (xmlTextWriterWriteAttribute (writer, hex_name, value));
	xmlFree (hex_name);
	xmlFree (value);

	return 0;
}

static int
generate_normal_attribute (xmlTextWriterPtr writer, cob_ml_attr *attr, unsigned int *count)
{
	xmlChar	*name;
	xmlChar	*value;

	name = get_xml_name (attr->name);
	value = get_trimmed_xml_data (attr->value);
	IF_NEG_RETURN_ELSE_COUNT (xmlTextWriterWriteAttribute (writer, name, value));
	xmlFree (name);
	xmlFree (value);

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

	hex_name = get_name_with_hex_prefix (tree->name);
	IF_NEG_RETURN_ELSE_COUNT (xmlTextWriterStartElementNS (writer, x_ns_prefix,
							       hex_name, x_ns));
	xmlFree (hex_name);

	status = generate_attributes (writer, tree->attrs, count);
	if (status < 0) {
		return status;
	}

	hex_value = get_hex_xml_data (tree->content);
	IF_NEG_RETURN_ELSE_COUNT (xmlTextWriterWriteString (writer, hex_value));
	xmlFree (hex_value);

	IF_NEG_RETURN_ELSE_COUNT (xmlTextWriterEndElement (writer));

	return 0;
}


static xmlChar *
get_xml_num (cob_field * const f, const char decimal_point)
{
	return get_num (f, &xmlCharStrndup_void, decimal_point);
}

static int
generate_content (xmlTextWriterPtr writer, cob_ml_tree *tree,
		  const char decimal_point, unsigned int *count)
{
	cob_field	*content = tree->content;
	xmlChar		*x_content;

	if (COB_FIELD_IS_FP (content)) {
		/* TO-DO: Implement! */
		/* TO-DO: Stop compilation if float in field */
		cob_set_exception (COB_EC_IMP_FEATURE_MISSING);
		cob_fatal_error (COB_FERROR_XML);
	} else if (COB_FIELD_IS_NUMERIC (content)) {
		x_content = get_xml_num (content, decimal_point);
	} else {
		x_content = get_trimmed_xml_data (content);
	}

	IF_NEG_RETURN_ELSE_COUNT (xmlTextWriterWriteString (writer, x_content));
	xmlFree (x_content);

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

	/* Start element */
	x_name = get_xml_name (tree->name);
	IF_NEG_RETURN_ELSE_COUNT (xmlTextWriterStartElementNS (writer, x_ns_prefix,
							       x_name, x_ns));
	xmlFree (x_name);

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
	IF_NEG_RETURN_ELSE_COUNT (xmlTextWriterEndElement (writer));

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

#undef IF_NEG_RETURN_ELSE_COUNT

#endif

#if defined (WITH_CJSON) || defined (WITH_JSON_C)

static void *
json_strndup (const char *str, const size_t size)
{
	char	*dup = cob_malloc (size + 1);
	memcpy (dup, str, size);
	return dup;
}

static char *
get_trimmed_json_data (const cob_field * const f)
{
	return (char *) get_trimmed_data (f, &json_strndup);
}

static char *
get_json_num (cob_field * const f, const char decimal_point)
{
	return (char *) get_num (f, &json_strndup, decimal_point);
}

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
		name = get_trimmed_json_data (tree->name);
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
			content = get_json_num (tree->content, decimal_point);
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
			content = (char *) get_trimmed_json_data (tree->content);
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
	char		*name = NULL;
	char		*content = NULL;
	int		status = 0;
	json_object	*children_json = NULL;

	if (tree->is_suppressed) {
		return 0;
	}

	/* NAME OF ... OMITTED to generate an anonymous JSON object */
	if (tree->name != NULL) {
		name = get_trimmed_json_data (tree->name);
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
			json_object_object_add (out, name, children_json);
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
			content = get_json_num (tree->content, decimal_point);
			/*
			  Since we're only going to serialise the JSON, we don't
			  care how JSON-C represents it internally. So, we tell
			  C-JSON the number is 0.0f.
			*/
			json_object_object_add (out, name,
						json_object_new_double_s (0.0, content));
		} else {
			content = get_trimmed_json_data (tree->content);
			json_object_object_add (out, name,
						json_object_new_string (content));
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
#if WITH_XML2
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

static void xml_generate (cob_field *out, cob_ml_tree *tree, cob_field *count,
	const int with_xml_dec, const char *ns_data, cob_field *ns_prefix,
	const char decimal_point);

/* entry function for XML GENERATE */
void
cob_xml_generate (cob_field *out, cob_ml_tree *tree, cob_field *count,
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

struct xml_state {
	enum xml_parser_state state;
	enum xml_code_status last_xml_code;
	const char* dummy;
#if WITH_XML2
	xmlParserCtxtPtr *ctx;
	xmlParserErrors err;
#endif

};

static void xml_parse (cob_field *in, cob_field *encoding, cob_field *validation,
	const int flags, struct xml_state *state);
static void xml_free_parse_memory (struct xml_state *state);

/* entry function for XML PARSE */
int cob_xml_parse (cob_field *in, cob_field *encoding, cob_field *validation,
		const int flags, void **saved_state)
{
	struct xml_state *state;
	int xml_code = get_xml_code ();

	/* no state yet ? first call */
	if (*saved_state == NULL) {
		/* no field */
		if (!in) {
#if 0	/* seems like a codegen error, which should not happen */
			set_xml_exception (XML_INTERNAL_ERROR);
			set_xml_event (EVENT_EXCEPTION);
			set_xml_text (0, "", 0);
			return -1;
#else
			cob_fatal_error (COB_FERROR_CODEGEN);
#endif
		}
		*saved_state = cob_malloc (sizeof (struct xml_state));
	}

	state = (struct xml_state *)*saved_state;

	/* LINKAGE or BASED item without data */
	if (!in->data) {
		state->last_xml_code = XML_INTERNAL_ERROR;
		set_xml_exception (XML_INTERNAL_ERROR);
		set_xml_event (EVENT_EXCEPTION);
		set_xml_text (0, "", 0);
		return 0;
	}
	/* likely a separate error case: emtpy item */
	if (is_empty (in)) {
		state->last_xml_code = XML_INTERNAL_ERROR;
		set_xml_exception (XML_INTERNAL_ERROR);
		set_xml_event (EVENT_EXCEPTION);
		set_xml_text (0, "", 0);
		return 0;
	}

	if (encoding && is_empty (encoding)) {
		encoding = NULL;
	}
	if (validation) {
		if (is_empty (validation)) {
			validation = NULL;
		} else if (has_invalid_xml_char (validation)) {
			state->last_xml_code = XML_INVALID_NAMESPACE;
			set_xml_exception (XML_INVALID_NAMESPACE);
			return 0;
		}
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

	/* user user-initiated exception condition (-1) /*/
	if (xml_code == -1) {
		/* xml code stays with one */
		xml_free_parse_memory (state);
		*saved_state = NULL;
		return 1;
	}

	/* we reached "end of input" (xmlss only?) and were not told to go on */
	if (state->state == XML_PARSER_HAD_END_OF_INPUT) {
		if (xml_code == 0) {
			set_xml_event (EVENT_END_OF_DOCUMENT);
			set_xml_code (0);
			state->state = XML_PARSER_FINISHED;
			return 1;
		}
		if (xml_code == 1) {
			/* goes on with parsing */
			xml_code = 0;
		} else {
			/* fatal runtime error,
			   TODO: at least a runtime warning, likely runtime exit */
			cob_set_exception (COB_EC_XML);
			xml_free_parse_memory (state);
			*saved_state = NULL;
			return 1;
		}
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

	/* do actual parsing */
	xml_parse (in, encoding, validation, flags, state);
	return 0;
}

#if WITH_XML2

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

/* actual handling of XML PARSE (not implemented yet) */
void xml_parse (cob_field *in, cob_field *encoding, cob_field *validation,
		const int flags, struct xml_state *state)
{
	static int first_xml = 1;

	COB_UNUSED (in);
	if (validation) {
		if (validation->size > 5 && memcmp (validation->data, "FILE ", 5)) {
			/* TODO: read file with name validation->data + 5 into local buffer
			   if needed by libxml, otherwise use the file directly;
			   the target name should be resolved with fileio.c (cob_chk_file_mapping) */
		} else {
			/* otherwise get data via get_trimmed_data (validation)
			   and expect it to contain a full valid schema definition */
		}
	}

	if (state->ctx == NULL) {
		char	*enc = NULL;
		state->ctx = cob_malloc (sizeof (xmlParserCtxtPtr));
		/*
		 * just copied without knowledge from the sample, possibly totally dumb...
		 * The document being in memory, it have no base per RFC 2396,
		 * and the "noname.xml" argument will serve as its base.
		*/
		if (encoding) {
			/* CHECKME: is there a reasonable array size to use instead? */
			enc = cob_get_picx (encoding->data, encoding->size, NULL, 0);
		}
		*state->ctx = xmlCreatePushParserCtxt (NULL, NULL,
			(const char*)in->data, in->size, "noname.xml");
		if (enc) {
			/* TODO (later): handle encoding */
			cob_free (enc);
		}
		if (*state->ctx == NULL) {
			state->last_xml_code = XML_PARSER_HAD_FATAL_ERROR;
			if (COB_MODULE_PTR->xml_mode == COB_XML_XMLNSS) {
				set_xml_exception (XML_PARSE_ERROR_FATAL);
			} else {
				set_xml_exception (XML_PARSE_ERROR_MISC_COMPAT);
			}
			set_xml_event (EVENT_EXCEPTION);
			set_xml_text (0, "", 0);
			return;
		}
		set_xml_event (EVENT_START_OF_DOCUMENT);
		if (COB_MODULE_PTR->xml_mode == COB_XML_XMLNSS) {
			set_xml_text (0, "", 0);
		} else {
			set_xml_text (flags & COB_XML_PARSE_NATIONAL, in->data, in->size);
		}
		state->state = XML_PARSER_JUST_STARTED;
		return;
	}
	if (state->state != XML_PARSER_JUST_STARTED) {
		int end_of_parsing = 0;	/* CHECKME: How to know this? */
		state->err = xmlParseChunk (*state->ctx,
			(const char*)in->data, in->size, end_of_parsing);
	} else {
		state->state = XML_PARSER_HAD_END_OF_INPUT;
		/* that's just an assumption and expected for the IBM sample */
		set_xml_event (EVENT_END_OF_INPUT);
		set_xml_code (0);	/* is that correct (seems what MF sets)? */
		return;

	}

	if (first_xml) {
		first_xml = 0;
		cob_runtime_warning (_("%s is not implemented"),
			"XML PARSE");
	}
	state->last_xml_code = XML_INTERNAL_ERROR;
	set_xml_exception (XML_INTERNAL_ERROR);
	cob_add_exception (COB_EC_IMP_FEATURE_MISSING);
	set_xml_event (EVENT_EXCEPTION);
	/* in case of EXCEPTIONs - should have a pointer to the text already parsed */
	set_xml_text (flags & COB_XML_PARSE_NATIONAL, "" , 0);
	state->state = XML_PARSER_HAD_FATAL_ERROR;
}

void xml_free_parse_memory (struct xml_state* state)
{
	if (state->ctx) {
		xmlDocPtr doc = (*state->ctx)->myDoc;
		xmlFreeDoc (doc);
		xmlFreeParserCtxt (*state->ctx);
		cob_free (state->ctx);
	}
	cob_free (state);
}

#else /* !WITH_XML2 */

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
	set_xml_text (0, "", 0); /* nothing parsed -> always empty */
	state->state = XML_PARSER_HAD_FATAL_ERROR;
}

void xml_free_parse_memory (struct xml_state* state)
{
	cob_free (state);
}

#endif

#if defined (WITH_CJSON) || defined (WITH_JSON_C)
/* entry function for JSON GENERATE */
void
cob_json_generate (cob_field *out, cob_ml_tree *tree, cob_field *count,
		   const char decimal_point)
{
	const char	*printed_json = NULL;
	unsigned int	print_len = 0;
	unsigned int	copy_len;
	int	num_newlines = 0;
	int	status = 0;
#if defined (WITH_CJSON)
	cJSON	*json;
#elif defined (WITH_JSON_C)
	json_object	*json = NULL;
#endif

	set_json_code (0);

#if defined (WITH_CJSON)
	json = cJSON_CreateObject ();
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

#elif defined (WITH_JSON_C)

	json = json_object_new_object ();
	status = generate_json_from_tree (tree, decimal_point, json);
	if (status < 0) {
		set_json_exception (JSON_INTERNAL_ERROR);
		goto end;
	}

	printed_json = json_object_to_json_string_ext (json, JSON_C_TO_STRING_PLAIN);
#endif

	if (!printed_json) {
		set_json_exception (JSON_INTERNAL_ERROR);
		goto end;
	}

	/* TO-DO: Duplication! */
	print_len = strlen (printed_json);
	copy_len = cob_min_int (print_len, (int) out->size);
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

 end:
#if defined (WITH_CJSON)
	if (printed_json) {
		cJSON_free ((void *)printed_json);
	}
	if (json) {
		cJSON_Delete (json);
	}
#elif defined (WITH_JSON_C)
	if (json) {
		json_object_put (json);
	}
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
cob_json_generate (cob_field *out, cob_ml_tree *tree, cob_field *count,
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
#if WITH_XML2
	LIBXML_TEST_VERSION
#endif
	cobglobptr = g;
}

void
cob_exit_mlio (void)
{
#if WITH_XML2
	xmlCleanupParser ();
#endif
}

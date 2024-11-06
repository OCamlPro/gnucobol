/*
  Copyright (C) 2024 Free Software Foundation, Inc.
  Written by Vedant Tewari, Nicolas Berthier,

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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <jni.h>

/* Force symbol exports */
#define	COB_LIB_EXPIMP
#include "config.h"
#include "common.h"
#include "coblocal.h"

/* Declarations */
static JavaVM *jvm = NULL;
static JNIEnv *env = NULL;

typedef struct __cob_java_static_method {
	jclass		cls;
	jmethodID	mid;
} cob_java_handle;

/* Only exported symbol: */
int		cob_jni_init (cob_java_api *api);

static int			/* non-zero means there's an error */
jvm_load (void) {
	/* JDK/JRE 6 VM initialization arguments */
	JavaVMInitArgs	args;
	JavaVMOption	options[1];
	const char	*classpath;
	char		cp_buffer[COB_MEDIUM_BUFF];

	args.version = JNI_VERSION_1_6;
	args.options = options;
	args.nOptions = 0;
	args.ignoreUnrecognized = JNI_FALSE;

	if ((classpath = getenv ("CLASSPATH")) != NULL) {
		snprintf (cp_buffer, COB_MEDIUM_MAX,
			  "-Djava.class.path=%s", classpath);
		options[args.nOptions++].optionString = cp_buffer;
	}

	return JNI_CreateJavaVM (&jvm, (void**)&env, &args);
}

static
cob_java_handle*
resolve_java (const char		*class_name,
	      const char		*method_name,
	      const char		*method_signature) {
	jclass cls;
	jmethodID mid;
	cob_java_handle *handle;

	char *jni_class_name = strdup(class_name);
	cls = (*env)->FindClass(env, jni_class_name);
	cob_free(jni_class_name);
	if (!cls) {
		cob_runtime_error (_("Java class '%s' not found"), class_name);
		cob_set_exception (COB_EC_FUNCTION_NOT_FOUND);
		return NULL;
	}

	mid = (*env)->GetStaticMethodID(env, cls, method_name, method_signature);
	if (!mid) {
		cob_runtime_error (_("Java method '%s' with signature '%s' not found in class '%s'"),
				   method_name, method_signature, class_name);
		(*env)->DeleteLocalRef(env, cls);
		cob_set_exception (COB_EC_OO_METHOD);
		return NULL;
	}

	handle = (cob_java_handle*)cob_malloc(sizeof(cob_java_handle));
	if (!handle) {
		cob_runtime_error (_("Memory allocation failed for Java method handle"));
		(*env)->DeleteLocalRef (env, cls);
		cob_set_exception (COB_EC_STORAGE_NOT_AVAIL);
		return NULL;
	}

	handle->cls = (*env)->NewGlobalRef(env, cls);
	handle->mid = mid;
	(*env)->DeleteLocalRef(env, cls);

	return handle;
}

static void
call_java (const cob_java_handle *method_handle)
{
	if (method_handle == NULL) {
		return;
	}
	(*env)->CallStaticVoidMethod(env,
				     method_handle->cls,
				     method_handle->mid, NULL);
	jthrowable exception = (*env)->ExceptionOccurred(env);
	if(exception) {
		jclass throwable = (*env)->FindClass(env, "java/lang/Throwable");
		jmethodID getMessage = (*env)->GetMethodID(env,
							   throwable, "getMessage", "()Ljava/lang/String;");
		if(getMessage != NULL) {
			jstring message = (jstring)(*env)->CallObjectMethod(env, exception, getMessage);
			const char *messageChars = (*env)->GetStringUTFChars(env, message, NULL);
			cob_runtime_error(_("Java exception: %s"), messageChars);
			(*env)->ReleaseStringUTFChars(env, message, messageChars);
			(*env)->DeleteLocalRef(env, message);
		}
		jclass stringWriter = (*env)->FindClass(env, "java/io/StringWriter");
		jclass printWriter = (*env)->FindClass(env, "java/io/PrintWriter");
		jobject stringWriterObj = (*env)->NewObject(env,
							    stringWriter,
							    (*env)->GetMethodID(env, stringWriter, "<init>", "()V"));
		jobject printWriterObj = (*env)->NewObject(env,
							   printWriter,
							   (*env)->GetMethodID(env, printWriter, "<init>", "(Ljava/io/Writer;)V"),
							   stringWriterObj);
		jmethodID printStackTrace = (*env)->GetMethodID(env, throwable, "printStackTrace", "(Ljava/io/PrintWriter;)V");
		(*env)->CallVoidMethod(env, exception, printStackTrace, printWriter);
		jmethodID toString = (*env)->GetMethodID(env, stringWriter, "toString", "()Ljava/lang/String;");
		jstring stackTrace = (jstring)(*env)->CallObjectMethod(env, stringWriterObj, toString);
		const char *stackTraceChars = (*env)->GetStringUTFChars(env, stackTrace, NULL);
		cob_runtime_error(_("Java stack trace: %s"), stackTraceChars);

		(*env)->ReleaseStringUTFChars(env, stackTrace, stackTraceChars);
		(*env)->DeleteLocalRef(env, stackTrace);
		(*env)->DeleteLocalRef(env, stringWriterObj);
		(*env)->DeleteLocalRef(env, printWriterObj);
		(*env)->DeleteLocalRef(env, exception);
	}
}

/* Entry-point for the module: initializes a Java API structure. */
int
cob_jni_init (cob_java_api *api) {
	if (jvm_load ()) {
		return 1;	/* Unable to initialize/load the JVM */
	}
	api->cob_resolve = resolve_java;
	api->cob_call = call_java;
	return 0;
}

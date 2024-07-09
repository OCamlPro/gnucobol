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
#include "libcob.h"
#include "coblocal.h"

/* Declarations */
static JavaVM *jvm = NULL;
static JNIEnv *env = NULL;

typedef struct __cob_java_static_method {
	jclass		cls;
	jmethodID	mid;
} cob_java_handle;

static int			/* non-zero means there's an error */
jvm_load (void) {
    /* JDK/JRE 6 VM initialization arguments */
    JavaVMInitArgs args;
    JavaVMOption* options;
    args.version = JNI_VERSION_1_6;
    const char *classpath = getenv("CLASSPATH");
    if (classpath == NULL) {
        classpath = "";
    }
    /* inline */
    args.nOptions = 1;
    size_t option_len = strlen("-Djava.class.path=") + strlen(classpath) + 1;
    options = (JavaVMOption*)cob_malloc(sizeof(JavaVMOption) * 1);
    options[0].optionString = (char*)cob_malloc(option_len);
    strcpy(options[0].optionString, "-Djava.class.path=");
    strcat(options[0].optionString, classpath);
    args.options = options;
    args.ignoreUnrecognized = 1;
    /* loading and initializing a Java VM, returning as JNI interface */
    return JNI_CreateJavaVM(&jvm, (void**)&env, &args);
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
	for (char *p = jni_class_name; *p; ++p) {
		if (*p == '.') {
			*p = '_';
		}
	}

	cls = (*env)->FindClass(env, jni_class_name);
	free(jni_class_name);
	if (!cls) {
		return NULL;
	}

	mid = (*env)->GetStaticMethodID(env, cls, method_name, method_signature);
	if (!mid) {
		(*env)->DeleteLocalRef(env, cls);
		return NULL;
	}

	handle = (cob_java_handle*)cob_malloc(sizeof(cob_java_handle));
	if (!handle) {
		(*env)->DeleteLocalRef(env, cls);
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
	if ((*env)->ExceptionCheck(env)) {
		(*env)->ExceptionDescribe(env);
		(*env)->ExceptionClear(env);
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

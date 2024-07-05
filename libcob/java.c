/*
   Copyright (C) 2024 Free Software Foundation, Inc.
   Written by Vedant Tewari

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

#include <jni.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <common.h>

#define HAVE_JNI

/* For caching which is an optimization but can we internally implement it as well? */

static JavaVM *jvm = NULL;
/* pointer to native method interface */  
static JNIEnv *env = NULL;

static void
cob_java_initialize() {
    /* JDK/JRE 6 VM initialization arguments */
    JavaVMInitArgs args;
    JavaVMOption* options = (JavaVMOption*)cob_malloc(sizeof(JavaVMOption) * 1);
    args.version = JNI_VERSION_1_6;
    /* inline */
    args.nOptions = 1;
    options[0].optionString = "-Djava.class.path=/usr/lib/java";
    args.options = &options;
    args.ignoreUnrecognized = 1;
    int rv;
    /* loading and initializing a Java VM, returning as JNI interface */
    rv = JNI_CreateJavaVM(jvm, (void**)&env, &args);
}

static void cob_handle_error(const char* method_sig) {
    if (method_sig != NULL) {
        free(method_sig);
    }
    if ((*env)->ExceptionCheck(env)) {
        (*env)->ExceptionDescribe(env);
        (*env)->ExceptionClear(env);
    }
    cob_cleanup();
}

void cob_delete_java_object(jobject obj) {
    if (obj != NULL) {
        (*env)->DeleteGlobalRef(env, obj);
    }
}

cob_java_handle*
cob_resolve_java(const char *class_name, const char* method_name, const char *return_type, const char *type_signature) {
    if(jvm == NULL) {
        cob_java_initialize();
    }
    jclass cls = (*env)->FindClass(env, class_name);
    if (cls == NULL) {
        return NULL;
    }

    jmethodID mid = get_from_cache(cls, method_name, type_signature);
    if (mid == NULL) {
        mid = (*env)->GetStaticMethodID(env, cls, method_name, type_signature);
        if (mid == NULL) {
            return NULL;
        }
    }
    // run autoconf
    // allocate structure that represents the method to be called
    cob_java_handle* h = (cob_java_handle*)cob_malloc(sizeof(cob_java_handle));
    return h;
}

static void 
cob_static_method(jclass cls, jmethodID mid) {
    (*env)->CallStaticVoidMethod(env, cls, mid, NULL);
}

static void 
JNICALL cob_call_java_static_method(jclass cls, char *class_name, const char* method_name, const char *return_type, jobject obj, jstring input) {
    char* paramTypes = cob_get_method_parameter_types(env, cls, method_name);
    char* methodSig = cob_gen_method_sig(paramTypes, 1, return_type);

    jclass cls = (*env)->FindClass(env, class_name);
    if (cls == NULL) {
        cob_handle_error(methodSig);
        return;
    }

    jmethodID mid = (*env)->GetStaticMethodID(env, cls, method_name, methodSig);
    if (mid == NULL) {
        cob_handle_error(methodSig);
        return;
    }

    const char *nativeInput = (*env)->GetStringUTFChars(env, input, NULL);
    if (nativeInput == NULL) {
        cob_handle_error(methodSig);
        return;
    }
    jstring result = (jstring)(*env)->CallStaticObjectMethod(env, cls, mid, (*env)->NewStringUTF(env, nativeInput));

    const char *nativeResult = (*env)->GetStringUTFChars(env, result, 0);
    if (nativeResult == NULL) {
        cob_handle_error(NULL);
        return;
    }

    (*env)->ReleaseStringUTFChars(env, input, nativeInput);
    (*env)->ReleaseStringUTFChars(env, result, nativeResult);

    if ((*env)->ExceptionCheck(env)) {
        cob_handle_error(NULL);
        return;
    }

    free(methodSig);
}

jobject 
cob_create_java_object(const char *class_name, const char *constructor_sig, jvalue *args) {
    jclass cls = (*env)->FindClass(env, class_name);
    if (cls == NULL) {
        cob_handle_error(NULL);
        return NULL; 
    }

    jmethodID constructor = (*env)->GetMethodID(env, cls, "<init>", constructor_sig);
    if (constructor == NULL) {
        cob_handle_error(NULL);
        return NULL;
    }

    jobject obj = (*env)->NewObjectA(env, cls, constructor, args);
    if (obj == NULL) {
        cob_handle_error(NULL);
        return NULL;
    }
    jobject global_obj = (*env)->NewGlobalRef(env, obj);
    (*env)->DeleteLocalRef(env, obj);
    return global_obj;
}

int
cob_call_java(const cob_java_handle *method_handle) {
    JNIEnv *env;
    va_list args;
    jobject result;

    if (method_handle == NULL) {
        return -1;
    }

    env = cob_get_jni_env();
    if (env == NULL) {
        cob_fatal_error("Failed to get JNI environment");
        return -1;
    }

    va_start(args, method_handle);
    result = (*env)->CallStaticObjectMethodV(env, method_handle->cls, method_handle->mid, args);
    va_end(args);

    if ((*env)->ExceptionCheck(env)) {
        (*env)->ExceptionDescribe(env);
        (*env)->ExceptionClear(env);;
        return -1;
    }

    return (result != NULL) ? 0 : -1;
}

cob_java_handle* cob_resolve_java(const char *class_name, const char* method_name, const char *type_signature) {
    if (jvm == NULL) {
        cob_java_initialize();
    }

    jclass cls = (*env)->FindClass(env, class_name);
    if (!cls) {
        cob_handle_error("Class not found");
        return NULL;
    }

    jmethodID mid = (*env)->GetStaticMethodID(env, cls, method_name, type_signature);
    if (!mid) {
        (*env)->DeleteLocalRef(env, cls);
        cob_handle_error("Method not found");
        return NULL;
    }

    cob_java_handle *handle = (cob_java_handle*)malloc(sizeof(cob_java_handle));
    if (!handle) {
        (*env)->DeleteLocalRef(env, cls);
        cob_handle_error("Memory allocation failed");
        return NULL;
    }

    handle->cls = (*env)->NewGlobalRef(env, cls);
    handle->mid = mid;
    (*env)->DeleteLocalRef(env, cls);

    return handle;
}
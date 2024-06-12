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

/* For caching which is an optimization but can we internally implement it as well? */
typedef struct {
    jclass cls;
    jmethodID mid;
} MethodCache;

MethodCache* cache = NULL;
int cacheSize = 0;

static JavaVM *jvm = NULL;
/* pointer to native method interface */  
static JNIEnv* env = NULL;

static void 
add_to_cache(jclass cls, jmethodID mid) {
    cacheSize++;
    cache = (MethodCache*)realloc(cache, sizeof(MethodCache) * cacheSize);
    cache[cacheSize - 1].cls = cls;
    cache[cacheSize - 1].mid = mid;
}

jmethodID 
get_from_cache(jclass cls, const char* methodName, const char* methodSig) {
    for (int i = 0; i < cacheSize; i++) {
        if (cache[i].cls == cls) {
            return cache[i].mid;
        }
    }
    return;
}

static JNIEnv* 
cob_create_vm() {
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
    if (rv < 0 || !env)
        return;
    else
        return rv;
    return env;
}

static void 
cob_handle_error(JavaVM* jvm, char* methodSig) {
    if (methodSig != NULL) {
        free(methodSig);
    }
    if (jvm != NULL) {
        (*jvm)->DestroyJavaVM(jvm);
    }
}

static char* 
cob_gen_method_sig(const char** paramType, int paramCount, const char** returnType) {
    /* (param_list) + return_type */
    int length = 2 + strlen(returnType);
    for(int i = 0; i < paramCount; i++) {
        length += strlen(paramType[i]);
    }
    
    /* internal malloc */
    char* sig = (char*)cob_malloc(length + 1);
    sig[0] = '(';
    int pos = 1;
    for(int i = 0; i < paramCount; i++) {
        strcpy(sig + pos, paramType[i]);
        pos += strlen(paramType[i]);
    }
    sig[pos++] = ')';
    strcpy(sig + pos, returnType);
    return sig;
}

static void
cob_lookup_static_method(JNIEnv* env, JavaVM* jvm, const char *className, const char *methodName, 
const char *methodSig, const char *returnType, const char** paramTypes, int paramCount) {
    jclass cls = (*env)->FindClass(env, className);
    if (cls == NULL) {
        (*jvm)->DestroyJavaVM(jvm);
        return;
    }
    
    jmethodID mid = get_from_cache(cls, methodName, methodSig);
    if (mid == NULL) {
        char* signature = generate_method_signature(paramTypes, paramCount, returnType);
        mid = (*env)->GetStaticMethodID(env, cls, methodName, signature);
        if (mid == NULL) {
            free(signature);
            (*jvm)->DestroyJavaVM(jvm);
            return;
        }
        add_to_cache(cls, mid);
        free(signature);
    }

    cob_static_method(env, jvm, cls, mid);
}

static void 
cob_static_method(JNIEnv* env, JavaVM* jvm, jclass cls, jmethodID mid) {
    (*env)->CallStaticVoidMethod(env, cls, mid, NULL);
}

static void 
JNICALL cob_call_java_static_method(JNIEnv *env, JavaVM *jvm, const char *className, const char *methodName, jobject obj, jstring input) {
    const char* paramTypes[] = {"Ljava/lang/String;"};
    char* methodSig = generate_method_signature(paramTypes, 1, "Ljava/lang/String;");

    jclass cls = (*env)->FindClass(env, className);
    if (cls == NULL) {
        cob_handle_error(jvm, methodSig);
        return;
    }

    jmethodID mid = (*env)->GetStaticMethodID(env, cls, methodName, methodSig);
    if (mid == NULL) {
        cob_handle_error(jvm, methodSig);
        return;
    }

    const char *nativeInput = (*env)->GetStringUTFChars(env, input, NULL);
    jstring result = (jstring)(*env)->CallStaticObjectMethod(env, cls, mid, (*env)->NewStringUTF(env, nativeInput));

    const char *nativeResult = (*env)->GetStringUTFChars(env, result, 0);

    (*env)->ReleaseStringUTFChars(env, input, nativeInput);
    (*env)->ReleaseStringUTFChars(env, result, nativeResult);

    free(methodSig);
}

/*
extern "C"
JNIEXPORT jstring JNICALL Java_callJavaMemberFunction(JNIEnv *env, jobject obj, jobject javaObject, jstring input) {
    jclass cls = (*env)->GetObjectClass(env, javaObject);
    if (cls == NULL) {
        return NULL;
    }

    const char* paramTypes[] = {"Ljava/lang/String;"};
    char* methodSig = generate_method_signature(paramTypes, 1, "Ljava/lang/String;");

    jmethodID mid = (*env)->GetMethodID(env, cls, "greet", methodSig);
    if (mid == NULL) {
        free(methodSig);
        return NULL;
    }

    jstring result = (jstring)(*env)->CallObjectMethod(env, javaObject, mid, input);

    free(methodSig);
    return result;
}
*/
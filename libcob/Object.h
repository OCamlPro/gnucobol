#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "hash_table.h"
#ifndef OOMODULE
#define OOMODULE

typedef struct _cob_field {
   char* data;
   /*A completer avec les éléments nécessaires pour caractériser une variable COBOL*/
} cob_field;

typedef int (*cobol_method)(
    struct cobol_object *self,
    char *args[]);

typedef struct _interface{
    const char name;
    char **method_list;
    int size;
}cob_interface;


typedef struct _meth_table
{
   struct _meth_table *next;
   const char *name;
   cobol_method call_pointer;
} meth_table;

typedef struct _field_table
{
   struct _field_table *next;
   const char *name;
   int index;
} field_table;

typedef struct  cobol_object
{
   //struct cobol_class *my_class;
   struct cobol_class *my_class;
   cob_field (*data)[]; 
   /* la taille est 1 dans la définition de type, mais
   on fera un malloc avec la vraie taille à runtime */
}cob_object;





typedef struct cobol_class
{
   ht_hash_table *tmv;   
   struct cobol_classv **super_class;
   int ft_size;
}cob_class;

struct cobol_class *cb_class_new(const char *name);


struct cobol_object *cb_object_new(struct cobol_class *myclass,
                                   cob_field *args[]);


cobol_method cb_object_method(struct cobol_object *self,
                              const char *method_name);

void cb_class_inherit(struct cobol_class *myclass,
                      struct cobol_class *ancester);
                      
void cb_class_new_method(struct cobol_class *myclass,
                         const char *method_name,
                         cobol_method *method_code);

void cb_class_new_field(struct cobol_class *myclass,
                        const char *field_name,
                        int size);

#endif







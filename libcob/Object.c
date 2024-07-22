
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "Object.h"
#include "hash_table.h"

/* Un type pour toutes les fonctions qui définissent des méthodes d'un objet.
   Ici, c'est un pointeur directement vers le code de la fonction, mais
   on pourrait imaginer passer par un "struct" qui contiendrait plus
   d'informations */

/* Field structure */

/*Un type pour définir les variables cobol, éventuellement à étendre pour fournir plus d'informations*/

/* Interface de création de la classe */

struct cobol_class *cb_class_new(const char *name)
{
   struct cobol_class *new_class;
   new_class = malloc(sizeof *new_class * 1);
   new_class->tmv = ht_new();
   /* alloue et initialise une classe vide */
}

static ht_item HT_DELETED_ITEM = {NULL, NULL};

void cb_class_inherit(struct cobol_class *myclass,
                      struct cobol_class *ancester)
{
   for (int i = 0; i < ancester->tmv->size; i++)
   {
      ht_item *item = ancester->tmv->items[i];
      if (item != NULL && item != &HT_DELETED_ITEM)
      {
         ht_insert(myclass->tmv, item->key, item->value);
      }
   }
   /* */ /* ajoute la class [ancester] comme un ancêtre de la
            classe [myclass] */
}

void cb_class_new_method(struct cobol_class *myclass,
                         const char *method_name,
                         cobol_method *method_code)
{
   ht_insert(myclass->tmv, method_name, method_code);
   /* */ /* ajoute la méthode [method_name] à la classe [myclass]
            pointant sur le code [method_code]. On peut convenir
            que si [method_name] est NULL, c'est le constructeur. Ici,
            je suppose qu'il n'y a pas d'overloading en fonction
            des arguments ? S'il est possible de faire des méthodes
            virtuelles, alors [method_code] peut être NULL dans ce cas. */
}

void cb_class_new_field(struct cobol_class *myclass,
                        const char *field_name,
                        int size)
{
   myclass->ft_size++;
   /* */ /* ajoute une variable d'instance [field_name]
            dans la class [myclass] avec une taille [size].
            A noter qu'on peut remplacer [int size] par une information
            de type plus détaillée, comme celle utilisée par GnuCOBOL.
            Dans ce cas, la fonction [cb_object_field] plus loin
            pourrait retourner une structure plutôt que juste un
            pointeur vers le champ.
            */
}

int cb_interface_nconformew(cob_interface *my_interface, cob_class *my_class)
{
   for (int i = 0; i < my_interface->size; i++){
      if(ht_search(my_class->tmv,my_interface->method_list[i]) != NULL){
         return 1;
      }
   }
   return 0;
}

/* Interface de création des objets et accès */

struct cobol_object *cb_object_new(struct cobol_class *myclass,
                                   cob_field *args[])

{
   struct cobol_object *new_object = (struct cobol_object *)
       malloc(sizeof(struct cobol_object) + myclass->ft_size * sizeof(struct _cob_field));
   new_object->my_class = myclass;
   /* allocation d'un objet de la class [myclass] et
           appel du constructeur */
}

cobol_method cb_object_method(struct cobol_object *self,
                              const char *method_name)
{
   meth_table *tmv = self->my_class->tmv;
   return ht_search(tmv, method_name);

   /* */ /* retourne le pointeur sur la méthode [method_name] dans
            l'objet [self]. Si le pointeur retourné n'est pas NULL,
            il peut être utilisé pour appeler la méthode. */
}

char *cb_object_field(struct cobol_object *self,
                      const char *field_name)

{
   /* */ /* retourne le pointeur sur la zone de mémoire de [self]
            contenant la variable d'instance [field_name] */
}

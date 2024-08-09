#ifndef HTABLE
#define HTABLEht_item

#define  HT_PRIME_1  151
#define  HT_PRIME_2 193
#define  HT_INITIAL_BASE_SIZE  47



typedef struct {
    char* key;
    char* value;
} ht_item;



typedef struct {
    int size;
    int count;
    int base_size;
    ht_item** items;
} ht_hash_table;


ht_hash_table* ht_new();

void ht_insert(ht_hash_table* ht, const char* key, cobol_method *value);
cobol_method* ht_search(ht_hash_table* ht, const char* key);
void ht_delete(ht_hash_table* h, const char* key);

#endif



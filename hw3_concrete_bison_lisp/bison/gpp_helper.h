#ifndef GPP_HELPER_H
#define GPP_HELPER_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct fraction {
   int num;
   int den;
} fraction;

typedef struct lli_t {
   struct lli_t* next;
   int val;
} int_list_elem;

typedef struct lfrac_t {
   struct lfrac_t* next;
   fraction val;
} frac_list_elem;

typedef struct lid_t {
   struct lid_t* next;
   char* str;
} id_list_elem;

typedef struct int_llist_t {
   int_list_elem* head;
   int_list_elem* tail;
} int_llist;

typedef struct frac_llist_t {
   frac_list_elem* head;
   frac_list_elem* tail;
} frac_llist;

typedef struct id_llist_t {
   id_list_elem* head;
   id_list_elem* tail;
} id_llist;

extern id_llist* id_table;
extern int_llist* val_table;
extern frac_llist* frac_table;

typedef enum { constEnum,
               idEnum,
               opEnum,
               listEnum } typeEnum;
typedef enum { binaryEnum,
               valueEnum } constTypeEnum;

int_llist* ll_int_create(int);
id_llist* ll_id_create(char*);
frac_llist* ll_frac_create(fraction);

void ll_int_append(int_llist*, int);
void ll_id_append(id_llist*, char*);
void ll_frac_append(frac_llist*, fraction);

int id_lookup(char*);
int id_bind(char*, int);

fraction parse_fraction(char* str);

//fraction arithmetic functions

fraction create_fraction(int, int);
char* fraction_to_string(fraction);

fraction add_fractions(fraction, fraction);
fraction subtract_fractions(fraction, fraction);
fraction multiply_fractions(fraction, fraction);
fraction divide_fractions(fraction, fraction);
int compare_fractions(fraction, fraction);

#endif
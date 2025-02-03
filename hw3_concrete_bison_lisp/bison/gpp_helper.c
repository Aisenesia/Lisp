#include "gpp_helper.h"

id_llist* id_table = NULL;
int_llist* val_table = NULL;


int_llist* ll_int_create(int val) {
    int_llist* l = malloc(sizeof(int_llist));
    l->head = l->tail = malloc(sizeof(int_list_elem));
    l->head->val = val;
    l->head->next = NULL;
    return l;
}

void ll_int_append(int_llist* l, int val) {
    l->tail = l->tail->next = malloc(sizeof(int_list_elem));
    l->tail->val = val;
    l->tail->next = NULL;
}

id_llist* ll_id_create(char* str) {
    id_llist* l = malloc(sizeof(id_llist));
    l->head = l->tail = malloc(sizeof(id_list_elem));
    l->head->str = strdup(str);
    l->head->next = NULL;
    return l;
}

void ll_id_append(id_llist* l, char* str) {
    l->tail = l->tail->next = malloc(sizeof(id_list_elem));
    l->tail->str = strdup(str);
    l->tail->next = NULL;
}

int id_lookup(char* str) {
    id_list_elem* id = id_table->head;
    int_list_elem* vals = val_table->head;
    
    while(id && vals) {
        if(!strcmp(id->str, str)) return vals->val;
        id = id->next;
        vals = vals->next;
    }
    printf("Identifier Not Found\n");
    exit(-1);
}

int id_bind(char* str, int val) {
    if(!id_table) {
        id_table = ll_id_create(str);
        val_table = ll_int_create(val);
        return val;
    }
    
    id_list_elem* id = id_table->head;
    int_list_elem* vals = val_table->head;
    
    while(id) {
        if(!strcmp(id->str, str)) {
            vals->val = val;
            return val;
        }
        id = id->next;
        vals = vals->next;
    }
    
    ll_id_append(id_table, str);
    ll_int_append(val_table, val);
    return val;
}

fraction parse_fraction(char* str){
    // Parse a fraction from a string in form of "numeratorfdenominator"
    // where numerator and denominator are integers
    fraction f;
    char *p, *start;
    p = start = str;
    while(*p && *p != 'f') p++;
    *p = '\0';
    f.num = atoi(start);
    f.den = atoi(p+1);
    return f;
}

frac_llist* ll_frac_create(fraction f){
    frac_llist* l = malloc(sizeof(frac_llist));
    l->head = l->tail = malloc(sizeof(frac_list_elem));
    l->head->val = f;
    l->head->next = NULL;
    return l;
}

void ll_frac_append(frac_llist* l, fraction f){
    l->tail = l->tail->next = malloc(sizeof(frac_list_elem));
    l->tail->val = f;
    l->tail->next = NULL;
}

fraction add_fractions(fraction f3, fraction f4){
    fraction f;
    f.num = f3.num * f4.den + f4.num * f3.den;
    f.den = f3.den * f4.den;
    return f;
}

fraction subtract_fractions(fraction f3, fraction f4){
    fraction f;
    f.num = f3.num * f4.den - f4.num * f3.den;
    f.den = f3.den * f4.den;
    return f;   
}

fraction multiply_fractions(fraction f3, fraction f4){
    fraction f;
    f.num = f3.num * f4.num;
    f.den = f3.den * f4.den;
    return f;
}

fraction divide_fractions(fraction f3, fraction f4){
    fraction f;
    f.num = f3.num * f4.den;
    f.den = f3.den * f4.num;
    return f;
}

int compare_fractions(fraction f3, fraction f4){
    if(f3.num * f4.den == f4.num * f3.den) return 0;
    if(f3.num * f4.den > f4.num * f3.den) return 1;
    return -1;
}

fraction create_fraction(int num, int den){
    fraction f;
    f.num = num;
    f.den = den;
    return f;
}

char* fraction_to_string(fraction f){
    char* str = malloc(100);
    sprintf(str, "%df%d", f.num, f.den);
    return str;
}

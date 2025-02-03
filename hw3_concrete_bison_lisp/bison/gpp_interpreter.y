%{
#include <stdio.h> 
#include <string.h>
#include <stdlib.h>
#include "gpp_helper.h"

void yyerror(char *);
int yylex(void);

%}

%union {
    int num;
    char* str;
    fraction fract;
    int_llist* int_list;
    id_llist* id_list;
    frac_llist* fract_list;
}

%token COMMENT KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_GREATER KW_NIL KW_LIST 
%token KW_APPEND KW_CONCAT KW_SET KW_DEFFUN KW_FOR KW_IF KW_EXIT 
%token KW_LOAD KW_DISP KW_TRUE KW_FALSE OP_PLUS OP_MINUS OP_DIV 
%token OP_MULT OP_OP OP_CP OP_Q KW_WHILE 
%token KW_DEFVAR STRING
%token <str> IDENTIFIER
%token <num> VALUEI
%token <fract> VALUEF

%type <int_list> list explisti values
%type <num> expi expb

%type <fract> expf
%type <fract_list> explistf 

%nonassoc UMINUS

%%

start: start input | input;

input: expi | explisti | expb | expf | explistf | COMMENT;

list: OP_Q OP_OP values OP_CP {$$ = $3;}
       | OP_OP KW_LIST values OP_CP {$$ = $3;}
       | OP_Q OP_OP OP_CP {$$ = NULL;}
       | KW_NIL {$$ = NULL;};

values: values VALUEI {ll_int_append($1, $2); $$ = $1;}
      | VALUEI {$$ = ll_int_create($1);};

expi: VALUEI {$$ = $1;}
    | IDENTIFIER {$$ = id_lookup($1);}
    | OP_MINUS expi %prec UMINUS {$$ = -$2;}
    | OP_OP KW_SET IDENTIFIER expi OP_CP {$$ = id_bind($3, $4);}
    | OP_OP OP_PLUS expi expi OP_CP {$$ = $3 + $4;}
    | OP_OP OP_MINUS expi expi OP_CP {$$ = $3 - $4;}
    | OP_OP OP_DIV expi expi OP_CP {$$ = $4 ? $3/$4 : 0 ;}
    | OP_OP OP_MULT expi expi OP_CP {$$ = $3 * $4;}
    | OP_OP KW_IF expb explisti OP_CP {$$ = $3 ? ($4 ? $4->tail->val : 0) : 0;}
    | OP_OP KW_IF expb expi OP_CP {$$ = $3 ? $4 : 0;}
    | OP_OP KW_IF expb explisti explisti OP_CP {$$ = $3 ? ($4 ? $4->tail->val : 0) : ($5 ? $5->tail->val : 0);}
    | OP_OP KW_IF expb expi expi OP_CP {$$ = $3 ? $4 : $5;}
    | OP_OP KW_WHILE OP_OP expb OP_CP explisti OP_CP {while($4) $$ = $6 ? $6->tail->val : 0;}
    | OP_OP KW_WHILE OP_OP expb OP_CP expi OP_CP {while($4) $$ = $6;}
    | OP_OP KW_FOR OP_OP IDENTIFIER expi expi OP_CP explisti OP_CP 
        {for(id_bind($4, 0); id_lookup($4) < $5; id_bind($4, id_lookup($4) + $6)) $$ = $8 ? $8->tail->val : 0;}
    | OP_OP KW_FOR OP_OP IDENTIFIER expi expi OP_CP expi OP_CP 
        {for(id_bind($4, 0); id_lookup($4) < $5; id_bind($4, id_lookup($4) + $6)) $$ = $8;}
    | OP_OP KW_DISP expi OP_CP {printf("%d\n", $3); $$ = 1;}
    | OP_OP KW_DISP explisti OP_CP 
        {
            if($3) for(int_list_elem* p = $3->head; p; p = p->next) printf("%d ", p->val);
            else printf("null");
            printf("\n");
            $$ = 1;
        }
    | OP_OP KW_LOAD STRING OP_CP {printf("true"); $$ = 1;}
    | OP_OP KW_EXIT OP_CP {exit(0);};

expb: OP_OP KW_AND expb expb OP_CP {$$ = $3 && $4;}
    | OP_OP KW_OR expb expb OP_CP {$$ = $3 || $4;}
    | OP_OP KW_NOT expb OP_CP {$$ = !$3;}
    | OP_OP KW_EQUAL expb expb OP_CP {$$ = $3 == $4;}
    | OP_OP KW_EQUAL expi expi OP_CP {$$ = $3 == $4;}
    | OP_OP KW_LESS expi expi OP_CP {$$ = $3 < $4;}
    | OP_OP KW_EQUAL expf expf OP_CP {$$ = compare_fractions($3, $4) == 0;}
    | OP_OP KW_LESS expf expf OP_CP {$$ = compare_fractions($3, $4) < 0;}
    | OP_OP KW_GREATER expf expf OP_CP {$$ = compare_fractions($3, $4) > 0;};
    | KW_TRUE {$$ = 1;}
    | KW_FALSE {$$ = 0;};

explisti: OP_OP KW_CONCAT explisti explisti OP_CP 
        {
            $$ = ($3 || $4) ? ($3 ? ($3->tail->next = $4 ? $4->head : NULL, $3) : $4) : NULL;
        }
    | OP_OP KW_APPEND expi explisti OP_CP 
        {$$ = $4 ? (ll_int_append($4, $3), $4) : ll_int_create($3);}
    | list {$$ = $1;};


expf: VALUEF {$$ = $1;}
    | OP_OP KW_DISP expf OP_CP {printf("%s\n", fraction_to_string($3)); $$ = $3;}
    | OP_OP OP_PLUS expf expf OP_CP {$$ = add_fractions($3, $4);}
    | OP_OP OP_MINUS expf expf OP_CP {$$ = subtract_fractions($3, $4);}
    | OP_OP OP_MULT expf expf OP_CP {$$ = multiply_fractions($3, $4);}
    | OP_OP OP_DIV expf expf OP_CP {$$ = divide_fractions($3, $4);};

explistf: OP_OP KW_CONCAT explistf explistf OP_CP 
        {
            $$ = ($3 || $4) ? ($3 ? ($3->tail->next = $4 ? $4->head : NULL, $3) : $4) : NULL;
        }
    | OP_OP KW_APPEND expf explistf OP_CP 
        {$$ = $4 ? (ll_frac_append($4, $3), $4) : ll_frac_create($3);}
    | OP_OP KW_LIST explistf OP_CP {$$ = $3;}
    | OP_OP KW_LIST OP_CP {$$ = NULL;};


%%

void yyerror(char *s) {fprintf(stderr, "%s\n", s);}

int main(void) {
    id_table = malloc(sizeof(id_llist));
    val_table = malloc(sizeof(int_llist));
    id_table->head = id_table->tail = NULL;
    val_table->head = val_table->tail = NULL;
    return yyparse();
}
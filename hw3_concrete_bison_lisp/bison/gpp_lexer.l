%{
#include <stdlib.h>
#include "gpp_helper.h"
#include "gpp_interpreter.tab.h"
%}

%option caseless

%%
;;.*                    { return COMMENT; }
[ \t\n\r]              { /* Skip whitespace */ }

and                    { return KW_AND; }
or                     { return KW_OR; }
not                    { return KW_NOT; }
equal                  { return KW_EQUAL; }
less                   { return KW_LESS; }
nil                    { return KW_NIL; }
list                   { return KW_LIST; }
append                 { return KW_APPEND; }
concat                 { return KW_CONCAT; }
set                    { return KW_SET; }
deffun                 { return KW_DEFFUN; }
for                    { return KW_FOR; }
if                     { return KW_IF; }
exit                   { return KW_EXIT; }
load                   { return KW_LOAD; }
print                  { return KW_DISP; }
true                   { return KW_TRUE; }
false                  { return KW_FALSE; }
while                  { return KW_WHILE; }
defvar                 { return KW_DEFVAR; }

[+]                    { return OP_PLUS; }
[-]                    { return OP_MINUS; }
[/]                    { return OP_DIV; }
[*]                    { return OP_MULT; }
[(]                    { return OP_OP; }
[)]                    { return OP_CP; }
\"[^\"]*\"            { return STRING; }
[']                    { return OP_Q; }

[0][0-9]+             { printf("%s Could Not Be Tokenized\n", yytext); }
[0-9]+[a-zA-Z]+.*     { printf("%s Could Not Be Tokenized\n", yytext); }

[a-zA-Z][a-zA-Z0-9]*  { yylval.str = strdup(yytext); return IDENTIFIER; }
0                     { yylval.num = 0; return VALUEI; }
[1-9][0-9]*"f"[1-9][0-9]* { yylval.fract = parse_fraction(yytext); return VALUEF; }
[1-9][0-9]*          { yylval.num = atoi(yytext); return VALUEI; }


.                     { printf("%s Could Not Be Tokenized\n", yytext); }

%%

int yywrap(void) { return 1; }
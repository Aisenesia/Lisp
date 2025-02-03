%{
#include <stdio.h>
#include <string.h>
%}

/* def */
WHITESPACE      [ \t\n\r]
DIGIT           [0-9]
LETTER          [a-zA-Z]
ALPHANUMERIC    [a-zA-Z0-9]
COMMENT         ;;.*

%%

"and"           { printf("KW_AND\n"); }
"or"            { printf("KW_OR\n"); }
"not"           { printf("KW_NOT\n"); }
"equal"         { printf("KW_EQUAL\n"); }
"less"          { printf("KW_LESS\n"); }
"nil"           { printf("KW_NIL\n"); }
"list"          { printf("KW_LIST\n"); }
"append"        { printf("KW_APPEND\n"); }
"concat"        { printf("KW_CONCAT\n"); }
"set"           { printf("KW_SET\n"); }
"deffun"        { printf("KW_DEFFUN\n"); }
"for"           { printf("KW_FOR\n"); }
"if"            { printf("KW_IF\n"); }
"exit"          { printf("KW_EXIT\n"); }
"load"          { printf("KW_LOAD\n"); }
"disp"          { printf("KW_DISP\n"); }
"true"          { printf("KW_TRUE\n"); }
"false"         { printf("KW_FALSE\n"); }

"+"             { printf("OP_PLUS\n"); }
"-"             { printf("OP_MINUS\n"); }
"/"             { printf("OP_DIV\n"); }
"*"             { printf("OP_MULT\n"); }
"("             { printf("OP_OP\n"); }
")"             { printf("OP_CP\n"); }
","             { printf("OP_COMMA\n"); }

{DIGIT}+{LETTER}{ALPHANUMERIC}*    { printf("SYNTAX_ERROR\n"); }
{DIGIT}+                           { printf("VALUEI\n"); }
{DIGIT}+f{DIGIT}+                  { printf("VALUEF\n"); }
{LETTER}[a-zA-Z0-9_]*             { printf("IDENTIFIER\n"); }
{COMMENT}                          { printf("COMMENT\n"); }
{WHITESPACE}                       { /* ignore */ }

.               { printf("SYNTAX_ERROR\n"); }

%%

int yywrap(void) {
    return 1;
}

int main() {
    yylex();
    return 0;
}
all: clean compile run

clean:
	rm -f gpp_interpreter.tab.c gpp_interpreter.tab.h lex.yy.c gpp_interpreter

flex: gpp_lexer.l
	flex gpp_lexer.l

bison: gpp_interpreter.y
	bison -d gpp_interpreter.y

compile: flex bison
	gcc -o gpp_interpreter gpp_interpreter.tab.c lex.yy.c gpp_helper.c -lfl

gdb: compile
	gdb ./gpp_interpreter

run:
	./gpp_interpreter
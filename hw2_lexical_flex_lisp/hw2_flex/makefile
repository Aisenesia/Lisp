all: clean compile run

compile: flex
	@gcc -o lexer gpp_lexer.c -lfl

flex: gpp_lexer.l
	@flex -o gpp_lexer.c gpp_lexer.l

clean:
	@rm -f lexer gpp_lexer.c

run: lexer
	@./lexer
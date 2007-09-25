CXXFLAGS=-I. -Ilib 
CC=g++

tutorial: tutorial.o AST.o AST_factory.o AST_transform.o AST_visitor.o lib/String.o lib/AttrMap.o lib/Boolean.o lib/Integer.o lexer.o scanner.o

AST.cpp AST_factory.cpp AST_transform.cpp AST_visitor.cpp: tutorial.tea
	../maketea tutorial.tea

# make sure to compile the scanner before the lexer
lexer.l: scanner.o

clean:
	rm -f *.o tutorial AST* lib/*.o y.tab.h

CFLAGS=-I.
CXXFLAGS=${CFLAGS}
CC=g++ -ggdb3 ${CFLAGS} 
YACC=bison -y -d

tutorial: tutorial.o AST.o AST_factory.o AST_transform.o AST_visitor.o lib/String.o lib/AttrMap.o lib/Boolean.o lib/Integer.o lexer.o scanner.o Unparser.h lib/demangle.o 

tutorial.o: AST.h Unparser.h XML_unparser.h Eval.h Add_prints.h

AST.cpp AST.h AST_factory.cpp AST_factory.h AST_transform.cpp AST_transform.h AST_visitor.cpp AST_visitor.h: tutorial.tea
	../maketea tutorial.tea

# make sure to compile the scanner before the lexer
lexer.l: scanner.o

clean:
	rm -f *.o tutorial AST* lib/*.o y.tab.h

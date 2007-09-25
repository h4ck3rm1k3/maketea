#include <iostream>
#include "AST.h"
#include "Unparser.h"

using namespace std;

int yyparse();
extern AST_program* program; 

int main()
{
	if(yyparse()) return -1;
	
	Unparser unparser;
	program->visit(&unparser);

	return 0;
}

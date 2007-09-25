#include <iostream>
#include "AST.h"
#include "Unparser.h"
#include "XML_unparser.h"

using namespace std;

int yyparse();
extern AST_program* program; 

int main()
{
	if(yyparse()) return -1;
	
	Unparser unparser;
//	program->visit(&unparser);

	XML_unparser xml_unparser;	
	program->visit(&xml_unparser);

	return 0;
}

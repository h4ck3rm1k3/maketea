#include <iostream>
#include "AST.h"

using namespace std;

int yyparse();
extern AST_program* program; 

int main()
{
	if(yyparse()) return -1;
	cout << "yeah" << endl;

	return 0;
}

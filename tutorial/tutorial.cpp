#include <iostream>
#include "AST.h"
#include "Unparser.h"
#include "XML_unparser.h"
#include "Eval.h"
#include "Add_prints.h"

using namespace std;

int yyparse();
extern AST_program* program; 

int usage(char** argv)
{
	cerr << "usage: " << argv[0] << " (--pp|--xml|--eval|--debug)" << endl;
	return -1;
}

int main(int argc, char** argv)
{
	if(argc != 2) return usage(argv);
	if(yyparse()) return -1;

	if(!strcmp(argv[1], "--pp"))
	{
		Unparser unparser;
		program->visit(&unparser);
		return 0;
	}

	if(!strcmp(argv[1], "--xml"))
	{
		XML_unparser xml_unparser;	
		program->visit(&xml_unparser);
		return 0;
	}

	if(!strcmp(argv[1], "--eval"))
	{
		Eval eval;
		program->visit(&eval);
		return 0;
	}

	if(!strcmp(argv[1], "--debug"))
	{
		Add_prints add_prints;
		program->transform_children(&add_prints);
		Unparser unparser;
		program->visit(&unparser);
		return 0;
	}
	
	return usage(argv);
}

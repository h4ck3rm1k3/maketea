#include "AST_visitor.h"
#include <iostream>

using namespace std;

class Unparser : public AST_visitor 
{
protected:
	string indent;

public:
	void post_literal(AST_literal* literal)
	{
		cout 
			<< indent
			<< *literal->lhs->value << " := " 
			<< literal->integer->value << ";" << endl;
	}

	void post_add(AST_add* add)
	{
		cout 
			<< indent
			<< *add->lhs->value << " := "
			<< *add->left->value << " + "
			<< *add->right->value << ";" << endl;
	}

	void pre_while(AST_while* wh)
	{
		cout << indent << "while " << *wh->var->value << " {" << endl;
		indent.push_back('\t');
	}

	void post_while(AST_while* wh)
	{
		indent = indent.substr(1);
		cout << indent << "}" << endl;
	}
};

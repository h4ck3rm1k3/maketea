#include "AST_visitor.h"
#include "lib/demangle.h"

class XML_unparser : public AST_visitor
{
public:
	void pre_node(AST_node* node)
	{
		cout << "<" << demangle(node) << ">" << endl;
	}

	void post_node(AST_node* node)
	{
		cout << "</" << demangle(node) << ">" << endl; 
	}

	void pre_var(Token_var* var)
	{
		cout << *var->value << endl;
	}

	void pre_integer(Token_integer* integer)
	{
		cout << integer->value << endl;
	}
};

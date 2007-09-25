#include "AST_visitor.h"

class Eval : public AST_visitor
{
protected:
	map<string, long> env;	

public:
	void pre_literal(AST_literal* literal)
	{
		env[*literal->lhs->value] = literal->integer->value;
	}

	void pre_print(AST_print* print)
	{
		cout << *print->var->value << ": " << env[*print->var->value] << endl;
	}

	void pre_add(AST_add* add)
	{
		env[*add->lhs->value] = env[*add->left->value] + env[*add->right->value];	
	}

	void children_while(AST_while* wh)
	{
		while(env[*wh->var->value])
			AST_visitor::children_while(wh);
	}
};

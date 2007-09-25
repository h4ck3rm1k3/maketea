#include "AST_transform.h"

class Add_prints : public AST_transform
{
public:
	void pre_add(AST_add* in, List<AST_statement*>* out)
	{
		out->push_back(in);
		out->push_back(new AST_print(in->lhs->clone()));
	}
};

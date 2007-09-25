/*
 * Syntax analyser 
 */

%{
	#include <iostream>
	#include "AST.h"
	using namespace std;

	int yylex();
	void yyerror(char const* s);
	AST_program* program;
%}

%error-verbose
%defines

%union {
	Token_var* token_var;
	Token_integer* token_integer;
	List<AST_statement*>* list_ast_statement;
	AST_statement* ast_statement;
	AST_assignment* ast_assignment;
	AST_while* ast_while;
	AST_literal* ast_literal;
	AST_add* ast_add;
}

%token ASSIGN
%token WHILE
%token <token_integer> INT
%token <token_var> VAR

%type <list_ast_statement> statement_list
%type <ast_statement> statement
%type <ast_assignment> assignment
%type <ast_while> while
%type <ast_literal> literal
%type <ast_add> add

%%

program: statement_list 
		{ program = new AST_program($1); }
	;

statement_list:
	  statement_list statement 
	  	{ $1->push_back($2); $$ = $1; } 
	| /* empty */
		{ $$ = new List<AST_statement*>; }
	;

statement: 
	  assignment
	  	{ $$ = $1; }
	| while
		{ $$ = $1; }
	;

assignment:
	  literal
	  	{ $$ = $1; }
	| add
		{ $$ = $1; }
	;

while: WHILE VAR '{' statement_list '}'
		{ $$ = new AST_while($2, $4); } 
	;

literal: VAR ASSIGN INT ';' 
		{ $$ = new AST_literal($1, $3); }
	;

add: VAR ASSIGN VAR '+' VAR ';' 
		{ $$ = new AST_add($1, $3, $5); }
	;

%%

void yyerror(char const* s)
{
	cerr << s << endl;
}

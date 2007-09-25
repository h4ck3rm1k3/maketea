/*
 * Syntax analyser 
 */

%{
	#include <iostream>
	using namespace std;

	int yylex();
	void yyerror(char const* s);
%}

%error-verbose
%defines

%token VAR
%token ASSIGN
%token WHILE
%token PLUS
%token INT

%%

program: INT ; 

%%

void yyerror(char const* s)
{
	cerr << s << endl;
}

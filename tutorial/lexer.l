/*
 * Lexical analyser
 */

	#include "AST.h"
	#include "y.tab.h"
	#include <stdlib.h>

%option noyywrap

%%

":="		return ASSIGN;
"while"		return WHILE;
[0-9]+		yylval.token_integer = new Token_integer(atol(yytext), new String(yytext)); return INT;
[a-z]+		yylval.token_var = new Token_var(new String(yytext)); return VAR;	
[{};+]		return *yytext;
.			// ignore

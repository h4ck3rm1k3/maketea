/*
	Lexical analyser
*/

%option noyywrap

%%

[a-z]+	printf("[%s]\n", yytext); 
.		// ignore

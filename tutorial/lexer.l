/*
 * Lexical analyser
 */
	
	#include "y.tab.h"

%option noyywrap

%%

[a-z]+	return VAR;	
.		// ignore

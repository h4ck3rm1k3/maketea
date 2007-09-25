#include <iostream>

using namespace std;

int yylex();

int main()
{
	while(yylex()) ;
}

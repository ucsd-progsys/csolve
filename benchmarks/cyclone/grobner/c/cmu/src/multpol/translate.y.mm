%{
#include <string.h>
%}

%token NUMBER,SYMBOL,POWER,PLUS,MINUS,STOP,TIMES,SEP,UNKNOWN,FULBUF

%%

read	:	pol STOP	{return;}
	|	STOP  		{status=1;return;}
	|	error		{status=2;return;}
	|	FULBUF		{status=4;return;}
	|	SEP read
	;

pol	:	pol sgnterm	{mpoladd(&temp,&tempmon,&temp);}
	|	first		{mpoladd(&temp,&tempmon,&temp);}
	;


first	:	sgnterm			
	|	unsterm			
	;

sgnterm	:	PLUS unsterm
	|	MINUS unsterm		{mnegate(&(tempmon.coefs[0]));}
	;
	
unsterm	:	powprod			{MFREE(&coef);MSET(1,&coef);
					 mpolmonmove(&coef,expo,&tempmon);
					 clean1();}
	|	NUMBER TIMES powprod    {MFREE(&coef);
					 mstrtoul(&coef,numbers[$1],NULL,10);
					 free(numbers[$1]);numbers[$1]=NULL;
					 mpolmonmove(&coef,expo,&tempmon);
					 clean1();}
	|	NUMBER SEP powprod      {MFREE(&coef);
					 mstrtoul(&coef,numbers[$1],NULL,10);
					 free(numbers[$1]);numbers[$1]=NULL;
					 mpolmonmove(&coef,expo,&tempmon);
					 clean1();}
	|	NUMBER			{MFREE(&coef);
					 mstrtoul(&coef,numbers[$1],NULL,10);
					 free(numbers[$1]);numbers[$1]=NULL;
					 mpolmonmove(&coef,expo,&tempmon);
					 clean1();}
	;

powprod	:	powprod TIMES power
	|	powprod SEP power
	|	power
	;

power	:	SYMBOL POWER NUMBER    {expo[0]+=(expo[$1+1]+=
					   (short)strtol(numbers[$3],NULL,10));
					free(numbers[$3]);numbers[$3]=NULL;}
	|	SYMBOL 		       {expo[0]+=(expo[$1+1]+=1);}
	|	UNKNOWN POWER NUMBER   
	|	UNKNOWN		       
	;

%%
#include "lex.yy.c"

void yyerror(s) char *s; {printf("%s\n",s);}

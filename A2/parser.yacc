
%%

%name BooleanAlgebra

%term ID of string | EOF | TERM | CONST | NOT | AND | OR | XOR | EQUALS | IMPLIES | 
	  IF | THEN | ELSE | LPAREN | RPAREN 

%nonterm program | statements | statement | formula 

%pos int

%eop EOF
%noshift EOF

%right IF THEN ELSE
%right IMPLIES
%left EQUALS XOR OR AND
%right NOT

%start program

%verbose

%%
program: statements (print("program : statements, "))
statements: statement statements (print("statements: statement, ")) | statement (print("statement: formula, "))
statement: formula TERM (print("statement : formula, "))
formula: LPAREN formula RPAREN (print("LPAREN formula RPAREN, ")) |
NOT formula (print("NOT formula, ")) | 
formula AND formula (print("AND formula, ")) | 
formula OR formula  (print("OR formula, ")) | 
formula XOR formula (print("XOR formula, ")) |
formula EQUALS formula (print("EQUALS formula, ")) |  
formula IMPLIES formula (print("IMPLIES formula, ")) | 
IF formula THEN formula ELSE formula (print("IF formula THEN formula ELSE formula, ")) | 
CONST (print("CONST formula, ")) | 
ID (print("ID formula, "))


%%

%name BooleanAlgebra

%term ID of string | EOF | TERM | CONST of string| NOT | AND | OR | XOR | EQUALS | IMPLIES | 
	  IF | THEN | ELSE | FI | LPAREN | RPAREN | LET | IN | END | EQ | NUM of int | TIMES | PLUS | 
	  MINUS | NEGATE | LESSTHAN | GREATERTHAN | FUN | FN | COLON | ARROW | TO | INT | BOOL

%nonterm program of AST.exp | statements of AST.exp | statement of AST.exp | formula of AST.exp | decleration of AST.decl

%pos int

%eop EOF
%noshift EOF

%right IF THEN ELSE
%right IMPLIES
%left EQUALS XOR OR AND
%right NOT

%left LESSTHAN GREATERTHAN
%left PLUS MINUS
%left TIMES
%right NEGATE
%nonassoc EQ

%start program

%verbose

%%
program: statements (statements) 
statements: statement statements (statement) | statement (statement) 
statement: formula TERM (formula) 

decleration: ID EQ formula (AST.ValDecl(ID,formula))

formula: LPAREN formula RPAREN (formula) |
NOT formula (AST.UnExp(AST.Not,formula)) | 
formula AND formula (AST.BinExp(AST.And,formula1,formula2)) | 
formula OR formula  (AST.BinExp(AST.Or,formula1,formula2)) | 
formula XOR formula (AST.BinExp(AST.Xor,formula1,formula2)) |
formula EQUALS formula (AST.BinExp(AST.Equals,formula1,formula2)) |  
formula IMPLIES formula (AST.BinExp(AST.Implies,formula1,formula2)) | 
NEGATE formula (AST.UnExp(AST.Negate,formula)) |
formula PLUS formula (AST.BinExp(AST.Plus,formula1,formula2)) |
formula MINUS formula (AST.BinExp(AST.Minus,formula1,formula2)) |
formula TIMES formula (AST.BinExp(AST.Times,formula1,formula2)) |
formula EQ formula (AST.BinExp(AST.Eq,formula1,formula2)) |
formula GREATERTHAN formula (AST.BinExp(AST.Greaterthan,formula1,formula2)) |
formula LESSTHAN formula (AST.BinExp(AST.Lessthan,formula1,formula2)) |
NUM (AST.NumExp(NUM)) |
IF formula THEN formula ELSE formula FI (AST.TriExp(AST.IfThenElse,formula1,formula2,formula3)) |
LET decleration IN formula END (AST.LetExp(decleration,formula)) | 
CONST (AST.ConstExp(CONST)) | 
ID (AST.VarExp(ID))

structure Tokens = Tokens

type pos = int
type colNumber = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token 
type lexresult = (svalue,pos) token

val pos = ref 1
val lineNumber = ref 1
val colNumber = ref 1

val errorAtLine = ref 1
val errorAtCol = ref 1

val err = ref false
val res = ref "["

fun final_res () = res := substring(!res,0,String.size (!res) - 2)

val invalid_token = ref ""
fun prnt_error () = print("Unknown Token:"^(Int.toString (!errorAtLine))^":"^(Int.toString(!errorAtCol))^":"^(!invalid_token)^"\n")
fun prnt_res () =  print(!res ^ "]\n")
val eof = fn () => (if !err then prnt_error () else (final_res(); prnt_res ());Tokens.EOF(!pos, !colNumber))
fun resetCol x = (colNumber := x)
fun incrColby x = colNumber := !colNumber + x
fun incrLineby x = lineNumber:= !lineNumber + x
fun concat x = res := !res ^ x
fun errorLineAndCol (x,y) = (errorAtLine := x; errorAtCol := y)

%%

%header (functor BooleanAlgebraLexFun(structure Tokens: BooleanAlgebra_TOKENS));
alpha = [A-Za-z];
ws = [\ ];
%%

\n => (incrLineby 1; resetCol(1); pos:= !pos + 1;lex()); 
{ws}+ => (incrColby 1; lex());
";" => (incrColby 1;concat("TERM \";\", ");Tokens.TERM(!pos,!pos));
"TRUE" => (incrColby 4; concat("CONST \"TRUE\", ");Tokens.CONST(!pos,!pos));
"FALSE" => (incrColby 5; concat("CONST \"FALSE\", ");Tokens.CONST(!pos,!pos));
"NOT" => (incrColby 3; concat("NOT \"NOT\", ");Tokens.NOT(!pos,!pos));
"AND" => (incrColby 3;concat("AND \"AND\", ");Tokens.AND(!pos,!pos));
"OR" => (incrColby 2;concat("OR \"OR\", "); Tokens.OR(!pos,!pos));
"XOR" => (incrColby 3;concat("XOR \"XOR\", "); Tokens.XOR(!pos,!pos));
"EQUALS" => (incrColby 6;concat("EQUALS \"EQUALS\", "); Tokens.EQUALS(!pos,!pos));
"IMPLIES" => (incrColby 7;concat("IMPLIES \"IMPLIES\", "); Tokens.IMPLIES(!pos,!pos));
"IF" => (incrColby 2;concat("IF \"IF\", "); Tokens.IF(!pos,!pos));
"THEN" => (incrColby 4;concat("THEN \"THEN\", "); Tokens.THEN(!pos,!pos));
"ELSE" => (incrColby 4;concat("ELSE \"ELSE\", "); Tokens.ELSE(!pos,!pos));
"(" => (incrColby 1;concat("LPAREN \"(\", "); Tokens.LPAREN(!pos,!pos));
")" => (incrColby 1;concat("RPAREN \")\", "); Tokens.RPAREN(!pos,!pos));
{alpha}+ => (incrColby (size yytext) ;concat("ID \""^yytext^"\", ");Tokens.ID (yytext,!pos,!pos));
. => (err := true;errorLineAndCol(!lineNumber,!colNumber);invalid_token := yytext;incrColby(size yytext);lex());
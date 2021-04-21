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

val keywords =
  [
   ("end",  Tokens.END),
   ("in",  Tokens.IN),
   ("let",  Tokens.LET)
   ]


fun findKeywords (str:string, pos1:pos, pos2:pos) =
	case List.find (fn (s, _) => s = str )  keywords of 
		SOME (_, tk) => (concat("Keyword "^"\""^str^"\""^", "); tk(pos1, pos2))|
		NONE => (concat("ID "^"\""^str^"\""^", "); Tokens.ID (str, pos1, pos2))

fun concatdigits(str:string) = concat("NUM "^"\""^str^"\""^", ");

%%

%header (functor BooleanAlgebraLexFun(structure Tokens: BooleanAlgebra_TOKENS));
alpha = [A-Za-z];
ws = [\ ];
digit = [0-9];
%%

\n => (incrLineby 1; resetCol(1); pos:= !pos + 1;lex()); 
{ws}+ => (incrColby 1; lex());
{digit}+ => (incrColby 1;concatdigits(yytext);Tokens.NUM
	     (List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),
	      !pos, !pos));
";" => (incrColby 1;concat("TERM \";\", ");Tokens.TERM(!pos,!pos));
"TRUE" => (incrColby 4; concat("CONST \"TRUE\", ");Tokens.CONST(yytext,!pos,!pos));
"FALSE" => (incrColby 5; concat("CONST \"FALSE\", ");Tokens.CONST(yytext,!pos,!pos));
"NOT" => (incrColby 3; concat("NOT \"NOT\", ");Tokens.NOT(!pos,!pos));
"AND" => (incrColby 3;concat("AND \"AND\", ");Tokens.AND(!pos,!pos));
"OR" => (incrColby 2;concat("OR \"OR\", "); Tokens.OR(!pos,!pos));
"XOR" => (incrColby 3;concat("XOR \"XOR\", "); Tokens.XOR(!pos,!pos));
"EQUALS" => (incrColby 6;concat("EQUALS \"EQUALS\", "); Tokens.EQUALS(!pos,!pos));
"IMPLIES" => (incrColby 7;concat("IMPLIES \"IMPLIES\", "); Tokens.IMPLIES(!pos,!pos));
"if" => (incrColby 2;concat("IF \"if\", "); Tokens.IF(!pos,!pos));
"then" => (incrColby 4;concat("THEN \"then\", "); Tokens.THEN(!pos,!pos));
"else" => (incrColby 4;concat("ELSE \"else\", "); Tokens.ELSE(!pos,!pos));
"fi" => (incrColby 2; concat("FI \"fi\", "); Tokens.FI(!pos,!pos));
"(" => (incrColby 1;concat("LPAREN \"(\", "); Tokens.LPAREN(!pos,!pos));
")" => (incrColby 1;concat("RPAREN \")\", "); Tokens.RPAREN(!pos,!pos));
"=" => (incrColby 1;concat("EQ \"=\", "); Tokens.EQ(!pos,!pos));
"PLUS" => (incrColby 1;concat("PLUS \"+\", "); Tokens.PLUS(!pos,!pos));
"MINUS" => (incrColby 1;concat("MINUS \"-\", "); Tokens.MINUS(!pos,!pos));
"NEGATE" => (incrColby 1;concat("NEGATE\"~\", "); Tokens.NEGATE(!pos,!pos));
"LESSTHAN" => (incrColby 2;concat("LESSTHAN\"<=\", "); Tokens.LESSTHAN(!pos,!pos));
"GREATERTHAN" => (incrColby 2;concat("GREATERTHAN\">=\", "); Tokens.GREATERTHAN(!pos,!pos));
"TIMES" => (incrColby 1; concat("TIMES\"*\", "); Tokens.TIMES(!pos,!pos));
{alpha}+ => (incrColby (size yytext) ;findKeywords(yytext,!pos,!pos));
. => (err := true;errorLineAndCol(!lineNumber,!colNumber);invalid_token := yytext;incrColby(size yytext);lex());
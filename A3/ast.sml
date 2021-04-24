structure AST = 
struct 

type id = string

datatype unop = Not | Negate 
datatype binop = And | Or | Xor | Equals | Implies |
				Plus | Minus | Times | Eq | Lessthan | Greaterthan 
datatype conditional = IfThenElse 

datatype decl = ValDecl of id * exp 
and exp =  		ConstExp of string
			|	NumExp of int
			|	StringExp of string
			|	VarExp of id
			|	UnExp of unop * exp
			|	BinExp of binop * exp * exp
			|	TriExp of conditional * exp * exp * exp
			|	LetExp of decl * exp

datatype value = IntVal of int
              |  StringVal of string
              |  BoolVal of bool


(*AST expressions list*)
type statements = exp list

val astExps = ref [] : statements ref (* List containing all the statements*)

fun addASTexp (statement) = (astExps := (statement::[]) @ !astExps)

(*Evaluation list*)
type evaluations = value list

val evals = ref [] : evaluations ref (* List containing all the evaluations *)

(*Env*)
type environment = (id * value) list

fun envAdd (var:id, v:value, env:environment) =
    (var,v)::env

fun envLookup (var:id, env:environment) =
    case List.find(fn (x, _) => x = var) env of
				        SOME (x, v)   => v
				    |   NONE => raise Fail "Environment lookup error"							   

end
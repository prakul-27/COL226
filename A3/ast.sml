structure AST = 
struct 

type id = string

datatype unop = Not | Negate 
datatype binop = And | Or | Xor | Equals | Implies |
				Plus | Minus | Times | Lessthan | Greaterthan | Eq
datatype conditional = IfThenElse 

datatype typ = IntTy 
			|  BoolTy
			|  StringTy
			|  FnTy of typ * typ

datatype decl = ValDecl of id * exp 
and exp =  		ConstExp of string 
			|	NumExp of int 
			|	StringExp of string 
			|	VarExp of id 
			|	UnExp of unop * exp 
			|	BinExp of binop * exp * exp 
			|	TriExp of conditional * exp * exp * exp 
			|	LetExp of decl * exp 
			|	AppExp of exp * exp 
			|	FnExp of id * typ * typ * exp 
			|	FunExp of id * id * typ * typ * exp 

datatype value = IntVal of int
              |  StringVal of string
              |  BoolVal of bool
              |	 FnVal of id * exp * ((id * value) list ref) (*Pointer to env in which FnExp was defined*)

(*Scope for all the functions*)

(*type scope = (id * funcdecl * exp) list

val funcScope = ref [] : scope ref

fun addFunToScope (x) = (funcScope := (x::[]) @ !funcScope)

fun scopeLookup (var:id,scpe:scope) = 
	case List.find(fn (x,_,e) => x = var) scpe of
		SOME (x,_,e) => e
	|	NONE => raise Fail ("Function not defined yet!")
*)

(*AST expressions list*)

type statements = exp list
 
val astExps = ref [] : statements ref  (*List containing all the statements*)

fun addASTexp (statement) = (astExps := (statement::[]) @ !astExps)


(*Evaluation list*)
type evaluations = value list

val evals = ref [] : evaluations ref 

(*Env*)
type environment = (id * value) list  

fun envAdd (var:id, v:value, env:environment) =
    (var,v)::env

fun envLookup (var:id, env:environment) =
    case List.find(fn (x, _) => x = var) env of
				        SOME (x, v)   => v
				    |   NONE => raise Fail "Environment lookup error"							   

fun reverseList (x,z) = 
	if null (x) then z else reverseList (tl x,hd (x)::z)

end
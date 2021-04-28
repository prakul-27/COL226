structure Typing =
struct 
		 (*Typing information is a synthesised attribute !!*)
open AST (*Traverse the AST to compute value of synthesised attributes*)

type typEnv = (id * typ) list

fun typEnvLookup (var:id, env:typEnv):typ = 
	case List.find(fn (x,_) => x = var) env of
		SOME (x,v) => v
	|	NONE => raise Fail ("Variable " ^ var ^ " is without a type")

fun typEnvAdd (var:id, t:typ, env:typEnv):typ = 
	(var,t)::env

fun getType (e:exp, env:typEnv):typ = 
	case e of 
		NumExp _ 		=> 	IntTy
	|	BoolExp _ 		=> 	BoolTy
	|	VarExp x 		=> 	typEnvLookup (x,env)
	|	AppExp (e1,e2) 	=>	
		(case (getType(e1,env),getType(e2,env)) of 
			|	FnTy((t1,t2),t3) => 
				if t1 = t3
				then t2
				else raise Fail "Application arguments type mismatch"

				(_,_) => raise Fail "Function was expected"
		)
	| 	FnExp (x,t1,t2,e) =>
		(
			FnTy (t, getType(e, typEnvAdd(x,t,env))) 
		)
	|	TriExp (cond,e1,e2,e3) => 
		(
			let
				val t1 = getType (e1,env)
				val t2 = getType (e2,env) 
				val t3 = getType (e3,env)
			in
				if t1 <> BoolTy
				then raise Fail "If condition does not have a boolean"
				else
					if t2 <> t3
					then raise Fail "Type Error: Then and Else branch do not have the same types!"
					else t2
			end
		)
	|	FunExp (fname,x2,t1,t2,e) =>
		(
			let
				val eType = getType (e, typEnvAdd(fname,t2,env))
			in
				
			end
		)
end
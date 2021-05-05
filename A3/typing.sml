structure Typing =
struct 
		 
open AST

fun checkValidBoolType t =
	case t of 
		BoolTy => true
	|	FnTy (t1,t2) => (t2 = BoolTy)
	|	_ => false

fun checkValidIntType t = 
	case t of
		IntTy => true
	|	FnTy (t1,t2) => (t2 = IntTy)
	|	_ => false

fun bothBoolTy (e1Type,e2Type) = ((checkValidBoolType e1Type) andalso (checkValidBoolType e2Type))
	
fun bothIntTy (e1Type,e2Type) = ((checkValidIntType e1Type) andalso (checkValidIntType e2Type))

type typEnv = (id * typ) list

fun typEnvLookup (var:id, env:typEnv):typ = 
	case List.find(fn (x,_) => x = var) env of
		SOME (x,v) => v
	|	NONE => raise Fail ("Variable " ^ var ^ " is without a type")

fun typEnvAdd (var:id, t:typ, env:typEnv):typEnv = 
	(var,t)::env

fun getType (e:exp, env:typEnv):typ = 
	case e of 
		NumExp _ 		=> 	IntTy
	|	StringExp _ 	=>  StringTy
	|	ConstExp _ 		=> 	BoolTy
	|	VarExp x 		=> 	typEnvLookup (x,env)
	|	AppExp (e1,e2) 	=>	
		(
			case (getType(e1,env),getType(e2,env)) of
				(FnTy (t1,t2),t3) =>
				if t1 = t3
				then t2
				else raise Fail "Application arguments type mismatch"

			|	_ => raise Fail "Function was expected"
		)
	| 	FnExp (x,t1,t2,e) =>
		(
			FnTy (getType (e,typEnvAdd(x,t1,env)), t2)
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
	|	FunExp (fname,x,t1,t2,e) =>
		(
			let
				val eType = getType (e, typEnvAdd(fname,FnTy(t1,t2),typEnvAdd(x,t1,env)))
			in
				if eType <> t2
				then raise Fail "Mismatch in declared and actual type"
				else FnTy (t1,t2)
			end
		)
	|	UnExp (operator,e) =>
		(
			let
				val eType = getType (e,env)
			in
				(case operator of
						Not => (if (checkValidBoolType eType)
								then BoolTy 
								else raise Fail "Type Error")
					|	Negate => (	if (checkValidIntType eType)
									then IntTy
									else raise Fail "Type Error")
				)
			end
		)
	|	BinExp (operator,e1,e2) =>
		(
			let
				val e1Type = getType (e1,env)
				val e2Type = getType (e2,env)
			in
				(case operator of
					And 	=> (if (bothBoolTy (e1Type,e2Type))
							then e1Type
							else raise Fail "Type Error")
				|	Or 		=> (if (bothBoolTy(e1Type,e2Type))
							then e1Type
							else raise Fail "Type Error")
				|	Xor 	=> (if (bothBoolTy (e1Type,e2Type))
							then e1Type
							else raise Fail "Type Error")
				|	Equals 	=> (if (bothBoolTy (e1Type,e2Type) orelse bothIntTy(e1Type,e2Type))
							then e1Type 
							else raise Fail "Equals type is broken")
				|	Implies => (if (bothBoolTy (e1Type,e2Type))
							then e1Type
							else raise Fail "Type Error")
				|	Plus 	=> (if (bothIntTy (e1Type,e2Type))
							then e1Type
							else raise Fail "Plus Type is broken")
				|	Minus 	=> (if (bothIntTy (e1Type,e2Type))
							then e1Type
							else raise Fail "Type Error")
				|	Times 	=> (if (bothIntTy (e1Type,e2Type))
							then e1Type
							else raise Fail "Type Error")
				|	Lessthan => (if (bothIntTy (e1Type,e2Type))
							then e1Type
							else raise Fail "Type Error")							
				|	Greaterthan => (if (bothIntTy (e1Type,e2Type))
							then e1Type
							else raise Fail "Type Error")
				|	Eq 		=>	e2Type							
				)
			end
		)
	|	LetExp (dec,e) => 
		(
			let
				val letType = 
				(
					case dec of 
						ValDecl (name,e1) =>
						(
							let
								val declType = getType (e1,env)
								val eType = getType (e,typEnvAdd (name,declType,env))
							in
								eType								
							end
						)
				)
			in
				letType
			end
		)

type ts = typ list

val types = ref [] : ts ref

fun getTypeofStatements (statementList) = 
	case statementList of
			[] => ()
		| 	x::xs => (types := getType(x,[])::[] @ !types; getTypeofStatements(xs))
end
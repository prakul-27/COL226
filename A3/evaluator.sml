structure Evaluator = 
struct 
open AST

val brokenTypes = Fail "Type is Broken!"

fun getBoolValue (str:string) =
    if(str = "TRUE")
    then true
    else false

fun evalExp(e:exp, env:environment):value =
    case e of
	      NumExp i            => IntVal i
      | ConstExp b          => BoolVal (getBoolValue(b))
      | UnExp (u,e1)        => evalUnExp (u,e1,env)
      | StringExp s         => StringVal s
      | VarExp x            => envLookup (x, env) 				  
      | BinExp (b, e1, e2)  => evalBinExp(b, e1, e2, env)
      | LetExp(ValDecl(x, e1), e2)  =>
  	let
	    val v1 = evalExp (e1, env)
	  in
	    evalExp(e2, envAdd (x, v1, env))
    end		   
and
evalUnExp (u:unop, e:exp, env:environment):value = 
  case (u,evalExp(e,env)) of
        (Not, BoolVal b) => BoolVal (not b)
    |   (Negate, IntVal i) => IntVal (~1 * i)
    |   _ => raise brokenTypes
and  
evalBinExp (b:binop, e1:exp, e2:exp, env:environment):value =
  case (b, evalExp(e1, env), evalExp(e2, env))  of
        (Plus, IntVal i1, IntVal i2) => IntVal (i1+i2)
    |   (Minus, IntVal i1, IntVal i2) => IntVal (i1-i2)
    |   (Times, IntVal i1, IntVal i2) => IntVal (i1*i2)
    |   (Eq, IntVal i1, IntVal i2)  => BoolVal (i1 = i2)
    |   (Eq, StringVal s1, StringVal s2) => BoolVal (s1 = s2)
    |   (Greaterthan, IntVal i1, IntVal i2) => BoolVal (i1 > i2)
    |   (Lessthan, IntVal i1, IntVal i2) => BoolVal (i1 < i2)
    |   (And, BoolVal b1, BoolVal b2) => BoolVal (b1 andalso b2)
    |   (Or, BoolVal b1, BoolVal b2) => BoolVal (b1 orelse b2)
    |   (Equals, BoolVal b1, BoolVal b2) => BoolVal (b1 = b2)
    |   (Xor, BoolVal b1, BoolVal b2) => BoolVal ((b1 orelse b2) andalso not(b1 andalso b2))
    |   (Implies, BoolVal b1, BoolVal b2) => BoolVal ((not b1) orelse b2) 
    |   _  => raise brokenTypes
end

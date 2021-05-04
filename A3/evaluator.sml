structure Evaluator = 
struct 
open AST

(*Exceptions*)
val brokenTypes = Fail "Type is Broken!"

val brokenIfThenElse = Fail "if-then-else-fi then and else branch are different or/and if branch is not bool!"

(*Helper functions for evaluating expression*)
fun getBoolValue (str:string) =
    if(str = "TRUE")
    then true
    else false

fun evalIfThenElseBool (x:bool, b1:bool, b2:bool) =
    if x = true
    then b1
    else b2

fun evalIfThenElseInt (x:bool, i1:int, i2:int) = 
    if x = true
    then i1
    else i2

(*Main function to evaluate expresssion*)
fun evalExp(e:exp, env:environment):value =
    case e of
	      NumExp i            => IntVal i
      | ConstExp b          => BoolVal (getBoolValue(b))
      | UnExp (u,e1)        => evalUnExp (u,e1,env)
      | StringExp s         => StringVal s
      | VarExp x            => envLookup (x, env) 				  
      | BinExp (b, e1, e2)  => evalBinExp(b, e1, e2, env)
      | TriExp (c,e1,e2,e3) => evalTriExp(c, e1, e2, e3, env)
      | FnExp (x1,_,_,e1)   => FnVal (x1,e1,ref env)
      | AppExp (e1,e2)      => 
        (case evalExp (e1,env) of
            FnVal (x1,body,enclosingEnv) => 
            (
              let
                  val v1 = evalExp (e2,env)
              in
                  evalExp (body,envAdd (x1,v1,!enclosingEnv))
              end
            )
          | _ => raise brokenTypes

        )
      | FunExp (fname,x1,_,_,body) =>
        (
          let
            val intialEnv = ref []
            val enclosingEnv = envAdd (fname,FnVal(x1,body,intialEnv),env)
            val () = (intialEnv := enclosingEnv)
          in
            evalExp (appexp, enclosingEnv)                      
          end
        )
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
    |   (Greaterthan, IntVal i1, IntVal i2) => BoolVal (i1 > i2)
    |   (Lessthan, IntVal i1, IntVal i2) => BoolVal (i1 < i2)
    |   (And, BoolVal b1, BoolVal b2) => BoolVal (b1 andalso b2)
    |   (Or, BoolVal b1, BoolVal b2) => BoolVal (b1 orelse b2)
    |   (Equals, BoolVal b1, BoolVal b2) => BoolVal (b1 = b2)
    |   (Equals, IntVal i1, IntVal i2) => BoolVal (i1 = i2)
    |   (Xor, BoolVal b1, BoolVal b2) => BoolVal ((b1 orelse b2) andalso not(b1 andalso b2))
    |   (Implies, BoolVal b1, BoolVal b2) => BoolVal ((not b1) orelse b2) 
    |   _  => raise brokenTypes
and
evalTriExp (c:conditional, e1:exp, e2:exp, e3:exp, env:environment):value =
  case (c, evalExp(e1,env), evalExp(e2, env), evalExp(e3, env)) of 
        (IfThenElse, BoolVal b1, BoolVal b2, BoolVal b3) => BoolVal (evalIfThenElseBool(b1,b2,b3))
    |   (IfThenElse, BoolVal b1, IntVal i1, IntVal i2) => IntVal (evalIfThenElseInt(b1,i1,i2))
    |   _ => raise brokenIfThenElse
fun evalStatements (statementList) = 
    case statementList of
      [] => StringVal "DONE"
    | x::xs => (evals := evalExp(x,[])::[] @ !evals; evalStatements(xs))

end
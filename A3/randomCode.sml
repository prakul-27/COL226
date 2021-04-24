fun getBoolValue (str:string) =
    if (str = "TRUE")
    then true
    else false

fun iterateList (xs,result:int list) =
	case xs of
		  [] => result
		| x::xs' => iterateList(xs', result@[x])

fun negate (v:int) = ~1 * v

val a = getBoolValue ("TRUE");
val b = getBoolValue ("FALSE");
val c = getBoolValue ("RANDOMWORD");
val d = negate 1
val e = negate (~1)

val f = iterateList ([1,2,3],[])

(* Function Grammar *)
fnDecl: FN LPAREN arguments RPAREN COLON typ TO fnDecl () | 
		FN LPAREN arguments RPAREN COLON typ TO formula()
funDecl: FUN LPAREN arguments RPAREN COLON typ TO fnDecl () |
		 FUN LPAREN arguments RPAREN COLON typ TO formula () 
arguments: ID COLON typ ()
typ: INT () | BOOL () | typ ARROW typ ()

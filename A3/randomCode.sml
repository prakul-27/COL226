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
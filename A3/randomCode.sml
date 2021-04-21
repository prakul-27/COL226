fun getBoolValue (str:string) =
    if (str = "TRUE")
    then true
    else false

fun negate (v:int) = ~1 * v

val a = getBoolValue ("TRUE");
val b = getBoolValue ("FALSE");
val c = getBoolValue ("RANDOMWORD");
val d = negate 1
val e = negate (~1)
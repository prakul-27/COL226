functor BooleanAlgebraLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : BooleanAlgebra_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\013\000\004\000\012\000\005\000\011\000\011\000\010\000\
\\015\000\009\000\017\000\008\000\021\000\007\000\025\000\006\000\000\000\
\\001\000\001\000\029\000\000\000\
\\001\000\002\000\000\000\000\000\
\\001\000\003\000\072\000\006\000\072\000\007\000\072\000\008\000\072\000\
\\009\000\072\000\010\000\072\000\012\000\072\000\013\000\072\000\
\\014\000\072\000\016\000\072\000\018\000\072\000\019\000\072\000\
\\022\000\072\000\023\000\072\000\024\000\072\000\026\000\072\000\
\\027\000\072\000\000\000\
\\001\000\003\000\025\000\006\000\024\000\007\000\023\000\008\000\022\000\
\\009\000\021\000\010\000\020\000\020\000\019\000\022\000\018\000\
\\023\000\017\000\024\000\016\000\026\000\015\000\027\000\014\000\000\000\
\\001\000\006\000\024\000\007\000\023\000\008\000\022\000\009\000\021\000\
\\010\000\020\000\012\000\047\000\020\000\019\000\022\000\018\000\
\\023\000\017\000\024\000\016\000\026\000\015\000\027\000\014\000\000\000\
\\001\000\006\000\024\000\007\000\023\000\008\000\022\000\009\000\021\000\
\\010\000\020\000\013\000\052\000\020\000\019\000\022\000\018\000\
\\023\000\017\000\024\000\016\000\026\000\015\000\027\000\014\000\000\000\
\\001\000\006\000\024\000\007\000\023\000\008\000\022\000\009\000\021\000\
\\010\000\020\000\014\000\054\000\020\000\019\000\022\000\018\000\
\\023\000\017\000\024\000\016\000\026\000\015\000\027\000\014\000\000\000\
\\001\000\006\000\024\000\007\000\023\000\008\000\022\000\009\000\021\000\
\\010\000\020\000\016\000\046\000\020\000\019\000\022\000\018\000\
\\023\000\017\000\024\000\016\000\026\000\015\000\027\000\014\000\000\000\
\\001\000\006\000\024\000\007\000\023\000\008\000\022\000\009\000\021\000\
\\010\000\020\000\019\000\051\000\020\000\019\000\022\000\018\000\
\\023\000\017\000\024\000\016\000\026\000\015\000\027\000\014\000\000\000\
\\001\000\018\000\044\000\000\000\
\\001\000\020\000\045\000\000\000\
\\056\000\000\000\
\\057\000\000\000\
\\058\000\001\000\013\000\004\000\012\000\005\000\011\000\011\000\010\000\
\\015\000\009\000\017\000\008\000\021\000\007\000\025\000\006\000\000\000\
\\059\000\000\000\
\\060\000\006\000\024\000\007\000\023\000\008\000\022\000\009\000\021\000\
\\010\000\020\000\020\000\019\000\022\000\018\000\023\000\017\000\
\\024\000\016\000\026\000\015\000\027\000\014\000\000\000\
\\061\000\000\000\
\\062\000\020\000\019\000\022\000\018\000\023\000\017\000\024\000\016\000\
\\026\000\015\000\027\000\014\000\000\000\
\\063\000\020\000\019\000\022\000\018\000\023\000\017\000\024\000\016\000\
\\026\000\015\000\027\000\014\000\000\000\
\\064\000\020\000\019\000\022\000\018\000\023\000\017\000\024\000\016\000\
\\026\000\015\000\027\000\014\000\000\000\
\\065\000\020\000\019\000\022\000\018\000\023\000\017\000\024\000\016\000\
\\026\000\015\000\027\000\014\000\000\000\
\\066\000\020\000\019\000\022\000\018\000\023\000\017\000\024\000\016\000\
\\026\000\015\000\027\000\014\000\000\000\
\\067\000\006\000\024\000\007\000\023\000\008\000\022\000\009\000\021\000\
\\010\000\020\000\020\000\019\000\022\000\018\000\023\000\017\000\
\\024\000\016\000\026\000\015\000\027\000\014\000\000\000\
\\068\000\020\000\019\000\000\000\
\\069\000\020\000\019\000\022\000\018\000\000\000\
\\070\000\020\000\019\000\022\000\018\000\000\000\
\\071\000\020\000\019\000\000\000\
\\073\000\020\000\019\000\022\000\018\000\023\000\017\000\024\000\016\000\000\000\
\\074\000\020\000\019\000\022\000\018\000\023\000\017\000\024\000\016\000\000\000\
\\075\000\000\000\
\\076\000\000\000\
\\077\000\000\000\
\\078\000\000\000\
\\079\000\000\000\
\"
val actionRowNumbers =
"\000\000\004\000\014\000\012\000\
\\000\000\030\000\001\000\000\000\
\\000\000\000\000\033\000\034\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\015\000\
\\013\000\024\000\010\000\011\000\
\\008\000\005\000\018\000\028\000\
\\029\000\026\000\025\000\027\000\
\\003\000\023\000\022\000\021\000\
\\020\000\019\000\000\000\000\000\
\\017\000\000\000\009\000\016\000\
\\006\000\032\000\000\000\007\000\
\\031\000\002\000"
val gotoT =
"\
\\001\000\053\000\002\000\003\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\002\000\024\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\004\000\025\000\000\000\
\\000\000\
\\005\000\026\000\000\000\
\\004\000\028\000\000\000\
\\004\000\029\000\000\000\
\\004\000\030\000\000\000\
\\000\000\
\\000\000\
\\004\000\031\000\000\000\
\\004\000\032\000\000\000\
\\004\000\033\000\000\000\
\\004\000\034\000\000\000\
\\004\000\035\000\000\000\
\\004\000\036\000\000\000\
\\004\000\037\000\000\000\
\\004\000\038\000\000\000\
\\004\000\039\000\000\000\
\\004\000\040\000\000\000\
\\004\000\041\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\046\000\000\000\
\\004\000\047\000\000\000\
\\000\000\
\\004\000\048\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\051\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 54
val numrules = 24
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | NUM of unit ->  (int) | CONST of unit ->  (string)
 | ID of unit ->  (string) | decleration of unit ->  (AST.decl)
 | formula of unit ->  (AST.exp) | statement of unit ->  (AST.exp)
 | statements of unit ->  (AST.exp) | program of unit ->  (AST.exp)
end
type svalue = MlyValue.svalue
type result = AST.exp
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 1) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "EOF"
  | (T 2) => "TERM"
  | (T 3) => "CONST"
  | (T 4) => "NOT"
  | (T 5) => "AND"
  | (T 6) => "OR"
  | (T 7) => "XOR"
  | (T 8) => "EQUALS"
  | (T 9) => "IMPLIES"
  | (T 10) => "IF"
  | (T 11) => "THEN"
  | (T 12) => "ELSE"
  | (T 13) => "FI"
  | (T 14) => "LPAREN"
  | (T 15) => "RPAREN"
  | (T 16) => "LET"
  | (T 17) => "IN"
  | (T 18) => "END"
  | (T 19) => "EQ"
  | (T 20) => "NUM"
  | (T 21) => "TIMES"
  | (T 22) => "PLUS"
  | (T 23) => "MINUS"
  | (T 24) => "NEGATE"
  | (T 25) => "LESSTHAN"
  | (T 26) => "GREATERTHAN"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 19)
 $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12)
 $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ 
(T 4) $$ (T 2) $$ (T 1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.statements statements1, statements1left, 
statements1right)) :: rest671)) => let val  result = MlyValue.program
 (fn _ => let val  (statements as statements1) = statements1 ()
 in (statements)
end)
 in ( LrTable.NT 0, ( result, statements1left, statements1right), 
rest671)
end
|  ( 1, ( ( _, ( MlyValue.statements statements1, _, statements1right)
) :: ( _, ( MlyValue.statement statement1, statement1left, _)) :: 
rest671)) => let val  result = MlyValue.statements (fn _ => let val  
statement1 = statement1 ()
 val  (statements as statements1) = statements1 ()
 in (statements)
end)
 in ( LrTable.NT 1, ( result, statement1left, statements1right), 
rest671)
end
|  ( 2, ( ( _, ( MlyValue.statement statement1, statement1left, 
statement1right)) :: rest671)) => let val  result = 
MlyValue.statements (fn _ => let val  (statement as statement1) = 
statement1 ()
 in (statement)
end)
 in ( LrTable.NT 1, ( result, statement1left, statement1right), 
rest671)
end
|  ( 3, ( ( _, ( _, _, TERM1right)) :: ( _, ( MlyValue.formula 
formula1, formula1left, _)) :: rest671)) => let val  result = 
MlyValue.statement (fn _ => let val  (formula as formula1) = formula1
 ()
 in (formula)
end)
 in ( LrTable.NT 2, ( result, formula1left, TERM1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: _ ::
 ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result
 = MlyValue.decleration (fn _ => let val  (ID as ID1) = ID1 ()
 val  (formula as formula1) = formula1 ()
 in (AST.ValDecl(ID,formula))
end)
 in ( LrTable.NT 4, ( result, ID1left, formula1right), rest671)
end
|  ( 5, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.formula 
formula1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.formula (fn _ => let val  (formula as formula1
) = formula1 ()
 in (formula)
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: ( _,
 ( _, NOT1left, _)) :: rest671)) => let val  result = MlyValue.formula
 (fn _ => let val  (formula as formula1) = formula1 ()
 in (AST.UnExp(AST.Not,formula))
end)
 in ( LrTable.NT 3, ( result, NOT1left, formula1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _ ::
 ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671)) =>
 let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.And,formula1,formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 8, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _ ::
 ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671)) =>
 let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.Or,formula1,formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 9, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _ ::
 ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671)) =>
 let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.Xor,formula1,formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 10, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.Equals,formula1,formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 11, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.Implies,formula1,formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 12, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: ( _
, ( _, NEGATE1left, _)) :: rest671)) => let val  result = 
MlyValue.formula (fn _ => let val  (formula as formula1) = formula1 ()
 in (AST.UnExp(AST.Negate,formula))
end)
 in ( LrTable.NT 3, ( result, NEGATE1left, formula1right), rest671)

end
|  ( 13, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.Plus,formula1,formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 14, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.Minus,formula1,formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 15, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.Times,formula1,formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 16, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.Eq,formula1,formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 17, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.Greaterthan,formula1,formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 18, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.Lessthan,formula1,formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 19, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.formula (fn _ => let val  (NUM as NUM1
) = NUM1 ()
 in (AST.NumExp(NUM))
end)
 in ( LrTable.NT 3, ( result, NUM1left, NUM1right), rest671)
end
|  ( 20, ( ( _, ( _, _, FI1right)) :: ( _, ( MlyValue.formula formula3
, _, _)) :: _ :: ( _, ( MlyValue.formula formula2, _, _)) :: _ :: ( _,
 ( MlyValue.formula formula1, _, _)) :: ( _, ( _, IF1left, _)) :: 
rest671)) => let val  result = MlyValue.formula (fn _ => let val  
formula1 = formula1 ()
 val  formula2 = formula2 ()
 val  formula3 = formula3 ()
 in (AST.TriExp(AST.IfThenElse,formula1,formula2,formula3))
end)
 in ( LrTable.NT 3, ( result, IF1left, FI1right), rest671)
end
|  ( 21, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.formula 
formula1, _, _)) :: _ :: ( _, ( MlyValue.decleration decleration1, _,
 _)) :: ( _, ( _, LET1left, _)) :: rest671)) => let val  result = 
MlyValue.formula (fn _ => let val  (decleration as decleration1) = 
decleration1 ()
 val  (formula as formula1) = formula1 ()
 in (AST.LetExp(decleration,formula))
end)
 in ( LrTable.NT 3, ( result, LET1left, END1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.CONST CONST1, CONST1left, CONST1right)) :: 
rest671)) => let val  result = MlyValue.formula (fn _ => let val  (
CONST as CONST1) = CONST1 ()
 in (AST.ConstExp(CONST))
end)
 in ( LrTable.NT 3, ( result, CONST1left, CONST1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.formula (fn _ => let val  (ID as ID1) = 
ID1 ()
 in (AST.VarExp(ID))
end)
 in ( LrTable.NT 3, ( result, ID1left, ID1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : BooleanAlgebra_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.CONST (fn () => i),p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun XOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun IMPLIES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun NEGATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATERTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
end
end

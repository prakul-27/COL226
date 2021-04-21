structure BooleanAlgebraLrVals = BooleanAlgebraLrValsFun(structure Token = LrParser.Token)
structure BooleanAlgebraLex = BooleanAlgebraLexFun(structure Tokens = BooleanAlgebraLrVals.Tokens);
structure BooleanAlgebraParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = BooleanAlgebraLrVals.ParserData
     	       structure Lex = BooleanAlgebraLex)
     
fun invoke lexstream =
    	let fun print_error (s,pos:int,_) = ()
		    	(*TextIO.output(TextIO.stdOut, "Syntax Error:" ^ (Int.toString pos) ^ ":" ^ "\n")*)
		in
		    BooleanAlgebraParser.parse(0,lexstream,print_error,())
		end
		
fun stringToLexer str =
    let val done = ref false
    	val lexer=  BooleanAlgebraParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
		lexer
    end	

fun parse (lexer) =
    let val dummyEOF = BooleanAlgebraLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
		val (nextToken, lexer) = BooleanAlgebraParser.Stream.get lexer
    in
        if BooleanAlgebraParser.sameToken(nextToken, dummyEOF) then result
 	else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end

val parseString = parse o stringToLexer

fun read_file x =
	let
		local
			val instream = TextIO.openIn x
		in
			val str = TextIO.inputAll instream
			val _ = TextIO.closeIn instream
		end
	in
		str
	end

fun parseFile x = 
	let
		val str = read_file x
	in
		parseString str
	end
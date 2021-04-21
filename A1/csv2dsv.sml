exception UnevenFieldException
exception emptyInputFile 

fun raiseException (expected, present, lineNumber) = 
	let 
		val exp = Int.toString (expected + 1)
		val pres = Int.toString (present + 1)
		val ln = Int.toString (lineNumber + 1)
	in
		print("Expected: " ^ exp ^  " fields, Present: " ^ pres ^ " fields on Line " ^ ln);
		raise UnevenFieldException
	end
fun convertDelimiters (infilename, delim1, outfilename, delim2) = 
	let
		val LF = "\n"
		val delim1s = String.str delim1
		val delim2s = String.str delim2

		val instream = TextIO.openIn infilename
		val outstream = TextIO.openOut outfilename

		(*function to read file*)
		fun readfile instream = 
			let
				val content = TextIO.inputAll instream handle Io => (TextIO.closeIn; raise emptyInputFile)
				val _ = TextIO.closeIn instream
			in
				explode content
			end
		
		(*function to convert char list to string list*)
		fun charToStringList x = 
			if null x 
			then []
			else String.str (hd x) :: charToStringList (tl x)

		val content = readfile instream
		val text = charToStringList content
		val len = List.length text

		val inside_ = ref false
		val count  = ref 0
		val index = ref 0
		
		
		fun calculateExpectedFields (i) = 
			if (List.nth (text,i) = LF andalso not(!inside_))
			then index := (i+1)
			else
				let
					val x = List.nth (text,i)
					fun check (x) = 
						if (x = "\"")
						then inside_ := not (!inside_)
						else
							if (x = delim1s andalso not(!inside_))
							then count := !count + 1
							else ()
					val _ = check (x)
				in
					calculateExpectedFields (i+1)
				end
		
		val _ = calculateExpectedFields (0)

		val expected = !count
		val insideQ = ref false (*to keep track of the fields inside quotes*)
		val lineNumber = ref 1 (*lineNumber of the ith record*)
		val ith = ref 0 (*count of the number of fields in the ith record*)

		fun sanityOfFeilds (text,i) = 
			if i >= len
			then ()
			else
				let
					val x = List.nth (text,i)
					fun check (x) = 
						if (x = "\"")
						then insideQ := not (!insideQ)
						else
							if (x = delim1s andalso not(!insideQ))
							then ith := !ith + 1 
							else
								if (x = LF andalso not(!insideQ))
								then
									let
										fun isithEqualtoExpected (x) = 
											if (!ith = expected)
											then ()
											else raiseException (expected,!ith,!lineNumber)												
									in
										isithEqualtoExpected (x);
										ith := 0;
										lineNumber := !lineNumber + 1
									end
								else ()
					val _ = check (x);
				in
					sanityOfFeilds (text,i+1)
				end
		
		val _ = sanityOfFeilds (text,!index + 1)

		val inside = ref false
		fun solve (i) = 
			if i >= len
			then ()
			else
				let
					val x = List.nth (text, i)
					
					fun replace (x) = 
						if (x = "\"")
						then
							let
								val ans = x
							in
								inside := not (!inside);
								ans
							end
						else
							if (x = delim1s andalso not (!inside))
							then delim2s
							else
								if (x = delim1s andalso !inside)
								then delim1s
								else x

					val ans = replace x
				in
					TextIO.output (outstream,ans);
					solve (i+1)
				end
	in
		solve (0);
		TextIO.closeOut outstream
	end

fun csv2tsv (infilename, outfilename) = 
	convertDelimiters (infilename, #",", outfilename, #"\t")

fun tsv2csv (infilename, outfilename) = 
	convertDelimiters (infilename, #"\t", outfilename, #",")
structure A3LrVals = A3LrValsFun(structure Token = LrParser.Token)
structure A3Lex = A3LexFun(structure Tokens = A3LrVals.Tokens);
structure A3Parser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = A3LrVals.ParserData
     	       structure Lex = A3Lex)
     
fun invoke lexstream =
    	     	let fun print_error (s,pos:int,_) =
		    	TextIO.output(TextIO.stdOut, "Error, line " ^ (Int.toString pos) ^ "," ^ s ^ "\n")
		in
		    A3Parser.parse(0,lexstream,print_error,())
		end

fun fileToLexer file =
    let 
		val instream = TextIO.openIn(file)
    in
		A3Parser.makeLexer(fn n => TextIO.inputAll(instream))
    end	
		
fun parse (lexer) =
    let 
		val dummyEOF = A3LrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
		val (nextToken, lexer) = A3Parser.Stream.get lexer
    in
        if A3Parser.sameToken(nextToken, dummyEOF) then result
		else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end

val parsefile = parse o fileToLexer




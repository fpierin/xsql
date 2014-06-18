structure Xsql : sig 
    val parse : string -> unit    
    val main : string * (string list) -> int  
  end 
= struct


structure XsqlLrVals =
  XsqlLrValsFun(structure Token = LrParser.Token)

structure XsqlLex =
  XsqlLexFun(structure Tokens = XsqlLrVals.Tokens)

structure XsqlParser =
  Join(structure LrParser = LrParser
    structure ParserData = XsqlLrVals.ParserData
    structure Lex = XsqlLex)



fun readlist (infile : string) =
  let 
    val ins = TextIO.openIn infile 
    fun loop ins = 
      case TextIO.inputLine ins of  
        SOME line => line :: loop ins 
      | NONE      => []  
  in 
    loop ins before TextIO.closeIn ins 
end;


fun invoke lexstream =
  let fun print_error (s,i:int,_) =
      TextIO.output(TextIO.stdOut,
      "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
  in XsqlParser.parse(0,lexstream,print_error,())
end  

fun parse (arquivo: string) =
  let
    val file = TextIO.openIn arquivo
    val lexer = XsqlParser.makeLexer (fn _ => (case TextIO.inputLine file
        of SOME s => s
          | _ => ""))
    val dummyEOF = XsqlLrVals.Tokens.EOF(0,0)
    val dummySEMI = XsqlLrVals.Tokens.PONT_VIRG(0,0)
    fun loop lexer =
      let val (result,lexer) = invoke lexer
          val (nextToken,lexer) = XsqlParser.Stream.get lexer
          val _ = ()
      in if XsqlParser.sameToken(nextToken,dummyEOF) then ()
        else loop lexer
    end
  in loop lexer
end

fun main (progname, args) =
	let
		val srcFile = (hd(tl(args)))
		val _ = parse(srcFile); 
	in
		TextIO.print progname;
		TextIO.print srcFile;
		1
end

end

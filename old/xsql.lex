structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
fun teste() = print "oi";
fun eof () = Tokens.EOF(!pos,!pos)
fun error (e,l : int,_) = TextIO.output (TextIO.stdOut,
	String.concat["line ", (Int.toString l), ": ", e, "\n"])

fun stringToInt s =
let
	val SOME x = Int.fromString(s)
in
	x
end

fun stringToReal s =
let
	val SOME x = Real.fromString(s)
in
	x
end



%%
%header (functor XsqlLexFun(structure Tokens: Xsql_TOKENS));
letra=[A-Za-z];
digito=[0-9];
branco=[\ \t];
alls=[^\"];
%%

"\n"		=> (pos := (!pos) + 1; lex());
{branco}+	=> (lex());
"+"		=> (Tokens.MAIS(!pos,!pos));
"-"		=> (Tokens.MENOS(!pos,!pos)); 
"*"		=> (Tokens.MULT(!pos,!pos));
"/"		=> (Tokens.DIV(!pos,!pos));
"mod"		=> (Tokens.MOD(!pos,!pos));

"and"		=> (Tokens.AND(!pos,!pos));
"or"		=> (Tokens.OR(!pos,!pos));
"not"		=> (Tokens.NOT(!pos,!pos));

"="		=> (Tokens.IGUAL(!pos,!pos));
"<>"		=> (Tokens.DIF(!pos,!pos));
">"		=> (Tokens.MENOR(!pos,!pos));
"<"		=> (Tokens.MAIOR(!pos,!pos));
">="		=> (Tokens.MAIGUAL(!pos,!pos));
"<="		=> (Tokens.MENIGUAL(!pos,!pos));

";"		=> (Tokens.PONT_VIRG(!pos,!pos));
":"		=> (Tokens.DOIS_PONT(!pos,!pos));
","		=> (Tokens.VIRG(!pos,!pos));
"("		=> (Tokens.PAREN_E(!pos,!pos));
")"		=> (Tokens.PAREN_D(!pos,!pos));
"{"		=> (Tokens.CHAVE_E(!pos,!pos));
"}"		=> (Tokens.CHAVE_D(!pos,!pos));

"var"		=> (Tokens.VAR(!pos,!pos));
"function"	=> (Tokens.FUNC(!pos,!pos));
"struct"	=> (Tokens.STRUCT(!pos,!pos));
"int"		=> (Tokens.T_INT(!pos,!pos));
"float"		=> (Tokens.T_REAL(!pos,!pos));
"string"	=> (Tokens.T_STRING(!pos,!pos));
"bool"		=> (Tokens.T_BOOL(!pos,!pos));

"if"		=> (Tokens.IF(!pos,!pos));
"else"		=> (Tokens.ELSE(!pos,!pos));
"while"		=> (Tokens.WHILE(!pos,!pos));

"select"	=> (Tokens.SELECT(!pos,!pos));
"delete"	=> (Tokens.DELETE(!pos,!pos));
"update"	=> (Tokens.UPDATE(!pos,!pos));
"create"	=> (Tokens.CREATE(!pos,!pos));
"from"		=> (Tokens.FROM(!pos,!pos));
"where"		=> (Tokens.WHERE(!pos,!pos));
"as"		=> (Tokens.AS(!pos,!pos));

"true"		=> (Tokens.BOOL(true,!pos,!pos));
"false"		=> (Tokens.BOOL(false,!pos,!pos));

\"{alls}*\"	=> (Tokens.STRING(yytext,!pos,!pos));
{digito}+	=> (Tokens.INT(stringToInt(yytext),!pos,!pos));

{digito}+\.{digito}+ => (Tokens.REAL(stringToReal(yytext),!pos,!pos));
{letra}[{letra}{digito}_]* => (print (yytext^"\n"); Tokens.IDENT(yytext,!pos,!pos));

.		=> (error ("ignoring bad character "^yytext,!pos,!pos) ;lex());

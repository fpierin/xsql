structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
fun eof () = Tokens.EOF(!pos,!pos)
fun error (e,l : int,_) = TextIO.output (TextIO.stdOut,
	String.concat["line ", (Int.toString l), ": ", e, "\n"])

fun achouToken tk = print (tk^" ")

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

fun tiraAspas s =  String.substring (s, 1, (size s) - 2)


%%
%header (functor XsqlLexFun(structure Tokens: Xsql_TOKENS));
letra=[A-Za-z];
digito=[0-9];
branco=[\ \t];
alls=[^\"];
%%

\n		=> (pos := (!pos) + 1; print (Int.toString(!pos)^"\n "); lex());
{branco}+	=> (lex());
"+"		=> (achouToken (yytext); Tokens.MAIS(!pos,!pos));
"-"		=> (achouToken (yytext); Tokens.MENOS(!pos,!pos)); 
"*"		=> (achouToken (yytext); Tokens.MULT(!pos,!pos));
"/"		=> (achouToken (yytext); Tokens.DIV(!pos,!pos));
"mod"		=> (achouToken (yytext); Tokens.MOD(!pos,!pos));

"and"		=> (achouToken (yytext); Tokens.AND(!pos,!pos));
"or"		=> (achouToken (yytext); Tokens.OR(!pos,!pos));
"not"		=> (achouToken (yytext); Tokens.NOT(!pos,!pos));

"="		=> (achouToken (yytext); Tokens.IGUAL(!pos,!pos));
"<>"		=> (achouToken (yytext); Tokens.DIF(!pos,!pos));
"<"		=> (achouToken (yytext); Tokens.MENOR(!pos,!pos));
">"		=> (achouToken (yytext); Tokens.MAIOR(!pos,!pos));
">="		=> (achouToken (yytext); Tokens.MAIGUAL(!pos,!pos));
"<="		=> (achouToken (yytext); Tokens.MENIGUAL(!pos,!pos));

";"		=> (achouToken (yytext); Tokens.PONT_VIRG(!pos,!pos));
":"		=> (achouToken (yytext); Tokens.DOIS_PONT(!pos,!pos));
","		=> (achouToken (yytext); Tokens.VIRG(!pos,!pos));
"("		=> (achouToken (yytext); Tokens.PAREN_E(!pos,!pos));
")"		=> (achouToken (yytext); Tokens.PAREN_D(!pos,!pos));
"{"		=> (achouToken (yytext); Tokens.CHAVE_E(!pos,!pos));
"}"		=> (achouToken (yytext); Tokens.CHAVE_D(!pos,!pos));

"var"		=> (achouToken (yytext); Tokens.VAR(!pos,!pos));
"function"	=> (achouToken (yytext); Tokens.FUNC(!pos,!pos));
"struct"	=> (achouToken (yytext); Tokens.STRUCT(!pos,!pos));
"int"		=> (achouToken (yytext); Tokens.T_INT(!pos,!pos));
"float"		=> (achouToken (yytext); Tokens.T_REAL(!pos,!pos));
"string"	=> (achouToken (yytext); Tokens.T_STRING(!pos,!pos));
"bool"		=> (achouToken (yytext); Tokens.T_BOOL(!pos,!pos));

"if"		=> (achouToken (yytext); Tokens.IF(!pos,!pos));
"else"		=> (achouToken (yytext); Tokens.ELSE(!pos,!pos));
"while"		=> (achouToken (yytext); Tokens.WHILE(!pos,!pos));

"select"	=> (achouToken (yytext); Tokens.SELECT(!pos,!pos));
"delete"	=> (achouToken (yytext); Tokens.DELETE(!pos,!pos));
"update"	=> (achouToken (yytext); Tokens.UPDATE(!pos,!pos));
"create"	=> (achouToken (yytext); Tokens.CREATE(!pos,!pos));
"table"		=> (achouToken (yytext); Tokens.TABLE(!pos,!pos));
"from"		=> (achouToken (yytext); Tokens.FROM(!pos,!pos));
"where"		=> (achouToken (yytext); Tokens.WHERE(!pos,!pos));
"as"		=> (achouToken (yytext); Tokens.AS(!pos,!pos));
"set"		=> (achouToken (yytext); Tokens.SET(!pos,!pos));

"true"		=> (achouToken (yytext); Tokens.BOOL(true,!pos,!pos));
"false"		=> (achouToken (yytext); Tokens.BOOL(false,!pos,!pos));

\"{alls}*\"	=> (achouToken (yytext); Tokens.STRING(tiraAspas(yytext),!pos,!pos));
{digito}+	=> (achouToken (yytext); Tokens.INT(stringToInt(yytext),!pos,!pos));

{digito}+\.{digito}+ => (achouToken (yytext); Tokens.REAL(stringToReal(yytext),!pos,!pos));
{letra}({letra}|{digito}|"_")* => (achouToken (yytext); Tokens.IDENT(yytext,!pos,!pos));

.		=> (error ("ignoring bad character "^yytext,!pos,!pos) ;lex());

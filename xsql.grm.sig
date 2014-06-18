signature Xsql_TOKENS =
sig
type ('a,'b) token
type svalue
val EOF:  'a * 'a -> (svalue,'a) token
val IDENT: (string) *  'a * 'a -> (svalue,'a) token
val STRUCT:  'a * 'a -> (svalue,'a) token
val FUNC:  'a * 'a -> (svalue,'a) token
val VAR:  'a * 'a -> (svalue,'a) token
val SET:  'a * 'a -> (svalue,'a) token
val AS:  'a * 'a -> (svalue,'a) token
val WHERE:  'a * 'a -> (svalue,'a) token
val FROM:  'a * 'a -> (svalue,'a) token
val TABLE:  'a * 'a -> (svalue,'a) token
val CREATE:  'a * 'a -> (svalue,'a) token
val UPDATE:  'a * 'a -> (svalue,'a) token
val DELETE:  'a * 'a -> (svalue,'a) token
val SELECT:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val BOOL: (bool) *  'a * 'a -> (svalue,'a) token
val STRING: (string) *  'a * 'a -> (svalue,'a) token
val REAL: (real) *  'a * 'a -> (svalue,'a) token
val INT: (int) *  'a * 'a -> (svalue,'a) token
val T_BOOL:  'a * 'a -> (svalue,'a) token
val T_STRING:  'a * 'a -> (svalue,'a) token
val T_REAL:  'a * 'a -> (svalue,'a) token
val T_INT:  'a * 'a -> (svalue,'a) token
val VIRG:  'a * 'a -> (svalue,'a) token
val PONT_VIRG:  'a * 'a -> (svalue,'a) token
val DOIS_PONT:  'a * 'a -> (svalue,'a) token
val CHAVE_D:  'a * 'a -> (svalue,'a) token
val CHAVE_E:  'a * 'a -> (svalue,'a) token
val PAREN_D:  'a * 'a -> (svalue,'a) token
val PAREN_E:  'a * 'a -> (svalue,'a) token
val MENIGUAL:  'a * 'a -> (svalue,'a) token
val MAIGUAL:  'a * 'a -> (svalue,'a) token
val MAIOR:  'a * 'a -> (svalue,'a) token
val MENOR:  'a * 'a -> (svalue,'a) token
val DIF:  'a * 'a -> (svalue,'a) token
val IGUAL:  'a * 'a -> (svalue,'a) token
val NOT:  'a * 'a -> (svalue,'a) token
val OR:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val MOD:  'a * 'a -> (svalue,'a) token
val DIV:  'a * 'a -> (svalue,'a) token
val MULT:  'a * 'a -> (svalue,'a) token
val MENOS:  'a * 'a -> (svalue,'a) token
val MAIS:  'a * 'a -> (svalue,'a) token
end
signature Xsql_LRVALS=
sig
structure Tokens : Xsql_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end

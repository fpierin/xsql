functor XsqlLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Xsql_TOKENS
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
\\001\000\001\000\107\000\002\000\106\000\003\000\105\000\004\000\104\000\
\\005\000\103\000\006\000\102\000\007\000\101\000\009\000\100\000\
\\010\000\099\000\011\000\098\000\012\000\097\000\013\000\096\000\
\\014\000\095\000\000\000\
\\001\000\003\000\042\000\008\000\041\000\015\000\040\000\026\000\039\000\
\\027\000\038\000\028\000\037\000\029\000\036\000\045\000\035\000\000\000\
\\001\000\008\000\041\000\015\000\040\000\026\000\039\000\027\000\038\000\
\\028\000\037\000\029\000\036\000\033\000\021\000\045\000\044\000\000\000\
\\001\000\008\000\041\000\015\000\040\000\026\000\039\000\027\000\038\000\
\\028\000\037\000\029\000\036\000\045\000\035\000\000\000\
\\001\000\008\000\041\000\015\000\040\000\026\000\039\000\027\000\038\000\
\\028\000\037\000\029\000\036\000\045\000\044\000\000\000\
\\001\000\009\000\028\000\000\000\
\\001\000\009\000\090\000\000\000\
\\001\000\015\000\060\000\000\000\
\\001\000\015\000\115\000\000\000\
\\001\000\016\000\081\000\000\000\
\\001\000\016\000\108\000\000\000\
\\001\000\016\000\123\000\000\000\
\\001\000\017\000\026\000\000\000\
\\001\000\017\000\062\000\000\000\
\\001\000\017\000\063\000\000\000\
\\001\000\017\000\121\000\000\000\
\\001\000\018\000\071\000\000\000\
\\001\000\018\000\110\000\000\000\
\\001\000\018\000\111\000\000\000\
\\001\000\018\000\125\000\000\000\
\\001\000\019\000\047\000\000\000\
\\001\000\019\000\072\000\000\000\
\\001\000\020\000\027\000\000\000\
\\001\000\020\000\046\000\000\000\
\\001\000\020\000\112\000\000\000\
\\001\000\021\000\178\000\038\000\178\000\040\000\154\000\000\000\
\\001\000\022\000\070\000\023\000\069\000\024\000\068\000\025\000\067\000\000\000\
\\001\000\022\000\070\000\023\000\069\000\024\000\068\000\025\000\067\000\
\\045\000\066\000\000\000\
\\001\000\028\000\053\000\000\000\
\\001\000\030\000\023\000\031\000\022\000\033\000\021\000\034\000\020\000\
\\035\000\019\000\036\000\018\000\045\000\017\000\000\000\
\\001\000\037\000\029\000\000\000\
\\001\000\038\000\031\000\000\000\
\\001\000\038\000\058\000\000\000\
\\001\000\038\000\061\000\000\000\
\\001\000\040\000\056\000\000\000\
\\001\000\040\000\073\000\000\000\
\\001\000\041\000\054\000\000\000\
\\001\000\042\000\006\000\000\000\
\\001\000\044\000\004\000\000\000\
\\001\000\045\000\007\000\000\000\
\\001\000\045\000\025\000\000\000\
\\001\000\045\000\030\000\000\000\
\\001\000\045\000\049\000\000\000\
\\001\000\045\000\055\000\000\000\
\\001\000\045\000\075\000\000\000\
\\001\000\045\000\078\000\000\000\
\\001\000\045\000\080\000\000\000\
\\001\000\045\000\083\000\000\000\
\\001\000\045\000\088\000\000\000\
\\001\000\046\000\000\000\000\000\
\\127\000\000\000\
\\128\000\044\000\004\000\000\000\
\\129\000\000\000\
\\130\000\045\000\049\000\000\000\
\\131\000\000\000\
\\132\000\000\000\
\\133\000\000\000\
\\134\000\000\000\
\\135\000\000\000\
\\136\000\042\000\006\000\000\000\
\\137\000\000\000\
\\138\000\000\000\
\\139\000\000\000\
\\140\000\030\000\023\000\031\000\022\000\033\000\021\000\034\000\020\000\
\\035\000\019\000\036\000\018\000\045\000\017\000\000\000\
\\141\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\000\000\
\\145\000\000\000\
\\146\000\000\000\
\\147\000\000\000\
\\148\000\000\000\
\\149\000\000\000\
\\150\000\000\000\
\\151\000\000\000\
\\152\000\032\000\116\000\000\000\
\\153\000\000\000\
\\154\000\000\000\
\\155\000\000\000\
\\156\000\000\000\
\\157\000\000\000\
\\158\000\000\000\
\\159\000\000\000\
\\160\000\000\000\
\\161\000\000\000\
\\162\000\000\000\
\\163\000\000\000\
\\164\000\000\000\
\\165\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\000\000\
\\172\000\000\000\
\\173\000\000\000\
\\174\000\000\000\
\\175\000\000\000\
\\176\000\021\000\057\000\000\000\
\\177\000\000\000\
\\179\000\000\000\
\\180\000\000\000\
\\181\000\000\000\
\\182\000\021\000\118\000\000\000\
\\183\000\000\000\
\\184\000\000\000\
\\185\000\000\000\
\\186\000\039\000\077\000\000\000\
\\187\000\021\000\114\000\000\000\
\\188\000\000\000\
\"
val actionRowNumbers =
"\038\000\037\000\039\000\029\000\
\\040\000\012\000\068\000\069\000\
\\071\000\070\000\067\000\066\000\
\\065\000\022\000\050\000\005\000\
\\030\000\041\000\031\000\001\000\
\\004\000\004\000\023\000\020\000\
\\042\000\063\000\002\000\028\000\
\\036\000\043\000\034\000\099\000\
\\032\000\025\000\083\000\082\000\
\\081\000\080\000\004\000\007\000\
\\033\000\013\000\077\000\014\000\
\\059\000\027\000\016\000\021\000\
\\064\000\072\000\073\000\035\000\
\\044\000\108\000\045\000\003\000\
\\046\000\009\000\004\000\047\000\
\\029\000\029\000\060\000\061\000\
\\062\000\058\000\057\000\056\000\
\\055\000\051\000\026\000\048\000\
\\108\000\006\000\106\000\004\000\
\\101\000\100\000\108\000\000\000\
\\010\000\108\000\017\000\018\000\
\\052\000\024\000\102\000\103\000\
\\004\000\109\000\107\000\097\000\
\\008\000\096\000\095\000\093\000\
\\094\000\092\000\091\000\090\000\
\\089\000\088\000\087\000\086\000\
\\085\000\084\000\079\000\098\000\
\\075\000\074\000\053\000\104\000\
\\004\000\004\000\015\000\054\000\
\\044\000\110\000\011\000\029\000\
\\105\000\078\000\019\000\076\000\
\\049\000"
val gotoT =
"\
\\001\000\124\000\002\000\001\000\000\000\
\\003\000\003\000\000\000\
\\000\000\
\\008\000\014\000\009\000\013\000\010\000\012\000\011\000\011\000\
\\012\000\010\000\014\000\009\000\016\000\008\000\017\000\007\000\
\\018\000\006\000\000\000\
\\005\000\022\000\000\000\
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
\\019\000\032\000\020\000\031\000\023\000\030\000\000\000\
\\023\000\041\000\000\000\
\\023\000\043\000\000\000\
\\000\000\
\\000\000\
\\006\000\046\000\000\000\
\\008\000\048\000\009\000\013\000\010\000\012\000\011\000\011\000\
\\012\000\010\000\014\000\009\000\016\000\008\000\017\000\007\000\
\\018\000\006\000\000\000\
\\018\000\050\000\023\000\049\000\000\000\
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
\\023\000\057\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\062\000\000\000\
\\004\000\063\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\015\000\072\000\000\000\
\\021\000\074\000\000\000\
\\000\000\
\\019\000\077\000\020\000\031\000\023\000\030\000\000\000\
\\000\000\
\\000\000\
\\023\000\080\000\000\000\
\\000\000\
\\008\000\082\000\009\000\013\000\010\000\012\000\011\000\011\000\
\\012\000\010\000\014\000\009\000\016\000\008\000\017\000\007\000\
\\018\000\006\000\000\000\
\\008\000\083\000\009\000\013\000\010\000\012\000\011\000\011\000\
\\012\000\010\000\014\000\009\000\016\000\008\000\017\000\007\000\
\\018\000\006\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\084\000\000\000\
\\004\000\085\000\000\000\
\\000\000\
\\021\000\087\000\000\000\
\\000\000\
\\000\000\
\\022\000\090\000\023\000\089\000\000\000\
\\000\000\
\\000\000\
\\021\000\091\000\000\000\
\\024\000\092\000\000\000\
\\000\000\
\\021\000\107\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\023\000\111\000\000\000\
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
\\000\000\
\\000\000\
\\006\000\115\000\000\000\
\\000\000\
\\022\000\117\000\023\000\089\000\000\000\
\\023\000\118\000\000\000\
\\000\000\
\\000\000\
\\015\000\120\000\000\000\
\\000\000\
\\000\000\
\\008\000\122\000\009\000\013\000\010\000\012\000\011\000\011\000\
\\012\000\010\000\014\000\009\000\016\000\008\000\017\000\007\000\
\\018\000\006\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 125
val numrules = 62
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
 | IDENT of unit ->  (string) | BOOL of unit ->  (bool)
 | STRING of unit ->  (string) | REAL of unit ->  (real)
 | INT of unit ->  (int)
 | op_bin of unit ->  ( ( Valor * Valor -> Valor ) )
 | exp of unit ->  ( ( Estado -> Valor ) )
 | condicoes of unit ->  ( ( Estado -> Valor ) )
 | where_exp of unit ->  ( ( string * Estado -> ConteudoTabela * ConteudoTabela ) )
 | atributo of unit ->  ( ( string * (Estado -> Valor) ) )
 | atributos of unit ->  ( ( (string * (Estado -> Valor)) list ) )
 | select of unit ->  ( ( Estado -> ConteudoTabela ) )
 | create of unit ->  ( ( Estado -> Estado ) )
 | delete of unit ->  ( ( Estado -> Estado ) )
 | atribuicoes of unit ->  ( ( (string * (Estado -> Valor)) list ) )
 | update of unit ->  ( ( Estado -> Estado ) )
 | select_com of unit ->  ( ( Estado -> Estado ) )
 | ifelse of unit ->  ( ( Estado -> Estado ) )
 | while_com of unit ->  ( ( Estado -> Estado ) )
 | atrib of unit ->  ( ( Estado -> Estado ) )
 | comando of unit ->  ( ( Estado -> Estado ) )
 | bloco of unit ->  ( ( Estado -> Estado ) )
 | def_func of unit ->  ( ( Estado -> Estado ) )
 | campos of unit ->  ( ( (string * TipoBasico) list ) )
 | declaracao of unit ->  ( ( Estado -> Estado ) )
 | tipoS of unit ->  ( ( TipoBasico ) )
 | def_var of unit ->  ( ( Estado -> Estado ) )
 | def_tipo of unit ->  ( ( Estado -> Estado ) )
 | start of unit ->  ( ( Estado ) )
end
type svalue = MlyValue.svalue
type result =  ( Estado ) 
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
fn (T 45) => true | _ => false
val showTerminal =
fn (T 0) => "MAIS"
  | (T 1) => "MENOS"
  | (T 2) => "MULT"
  | (T 3) => "DIV"
  | (T 4) => "MOD"
  | (T 5) => "AND"
  | (T 6) => "OR"
  | (T 7) => "NOT"
  | (T 8) => "IGUAL"
  | (T 9) => "DIF"
  | (T 10) => "MENOR"
  | (T 11) => "MAIOR"
  | (T 12) => "MAIGUAL"
  | (T 13) => "MENIGUAL"
  | (T 14) => "PAREN_E"
  | (T 15) => "PAREN_D"
  | (T 16) => "CHAVE_E"
  | (T 17) => "CHAVE_D"
  | (T 18) => "DOIS_PONT"
  | (T 19) => "PONT_VIRG"
  | (T 20) => "VIRG"
  | (T 21) => "T_INT"
  | (T 22) => "T_REAL"
  | (T 23) => "T_STRING"
  | (T 24) => "T_BOOL"
  | (T 25) => "INT"
  | (T 26) => "REAL"
  | (T 27) => "STRING"
  | (T 28) => "BOOL"
  | (T 29) => "WHILE"
  | (T 30) => "IF"
  | (T 31) => "ELSE"
  | (T 32) => "SELECT"
  | (T 33) => "DELETE"
  | (T 34) => "UPDATE"
  | (T 35) => "CREATE"
  | (T 36) => "TABLE"
  | (T 37) => "FROM"
  | (T 38) => "WHERE"
  | (T 39) => "AS"
  | (T 40) => "SET"
  | (T 41) => "VAR"
  | (T 42) => "FUNC"
  | (T 43) => "STRUCT"
  | (T 44) => "IDENT"
  | (T 45) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 44) => MlyValue.IDENT(fn () => ("bogus")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 45) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38)
 $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31)
 $$ (T 30) $$ (T 29) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20)
 $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13)
 $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ 
(T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.bloco bloco1, _, bloco1right)) :: ( _, ( 
MlyValue.def_var def_var1, _, _)) :: ( _, ( MlyValue.def_tipo 
def_tipo1, def_tipo1left, _)) :: rest671)) => let val  result = 
MlyValue.start (fn _ => let val  (def_tipo as def_tipo1) = def_tipo1
 ()
 val  (def_var as def_var1) = def_var1 ()
 val  (bloco as bloco1) = bloco1 ()
 in (bloco (def_var (def_tipo ([]))))
end)
 in ( LrTable.NT 0, ( result, def_tipo1left, bloco1right), rest671)

end
|  ( 1, ( ( _, ( _, _, CHAVE_D1right)) :: ( _, ( MlyValue.campos 
campos1, _, _)) :: _ :: ( _, ( MlyValue.IDENT IDENT1, _, _)) :: ( _, (
 _, STRUCT1left, _)) :: rest671)) => let val  result = 
MlyValue.def_tipo (fn _ => let val  (IDENT as IDENT1) = IDENT1 ()
 val  (campos as campos1) = campos1 ()
 in (fn x => XSQLLib.addTipo(IDENT, campos, x))
end)
 in ( LrTable.NT 1, ( result, STRUCT1left, CHAVE_D1right), rest671)

end
|  ( 2, ( ( _, ( MlyValue.def_tipo def_tipo1, _, def_tipo1right)) :: _
 :: ( _, ( MlyValue.campos campos1, _, _)) :: _ :: ( _, ( 
MlyValue.IDENT IDENT1, _, _)) :: ( _, ( _, STRUCT1left, _)) :: rest671
)) => let val  result = MlyValue.def_tipo (fn _ => let val  (IDENT as 
IDENT1) = IDENT1 ()
 val  (campos as campos1) = campos1 ()
 val  (def_tipo as def_tipo1) = def_tipo1 ()
 in (fn x => def_tipo (XSQLLib.addTipo(IDENT, campos, x)))
end)
 in ( LrTable.NT 1, ( result, STRUCT1left, def_tipo1right), rest671)

end
|  ( 3, ( ( _, ( _, _, PONT_VIRG1right)) :: ( _, ( MlyValue.tipoS 
tipoS1, _, _)) :: _ :: ( _, ( MlyValue.IDENT IDENT1, IDENT1left, _))
 :: rest671)) => let val  result = MlyValue.campos (fn _ => let val  (
IDENT as IDENT1) = IDENT1 ()
 val  (tipoS as tipoS1) = tipoS1 ()
 in ([(IDENT,tipoS)])
end)
 in ( LrTable.NT 5, ( result, IDENT1left, PONT_VIRG1right), rest671)

end
|  ( 4, ( ( _, ( MlyValue.campos campos1, _, campos1right)) :: _ :: (
 _, ( MlyValue.tipoS tipoS1, _, _)) :: _ :: ( _, ( MlyValue.IDENT 
IDENT1, IDENT1left, _)) :: rest671)) => let val  result = 
MlyValue.campos (fn _ => let val  (IDENT as IDENT1) = IDENT1 ()
 val  (tipoS as tipoS1) = tipoS1 ()
 val  (campos as campos1) = campos1 ()
 in ((IDENT,tipoS) :: campos)
end)
 in ( LrTable.NT 5, ( result, IDENT1left, campos1right), rest671)
end
|  ( 5, ( ( _, ( _, T_INT1left, T_INT1right)) :: rest671)) => let val 
 result = MlyValue.tipoS (fn _ => (TInt))
 in ( LrTable.NT 3, ( result, T_INT1left, T_INT1right), rest671)
end
|  ( 6, ( ( _, ( _, T_REAL1left, T_REAL1right)) :: rest671)) => let
 val  result = MlyValue.tipoS (fn _ => (TReal))
 in ( LrTable.NT 3, ( result, T_REAL1left, T_REAL1right), rest671)
end
|  ( 7, ( ( _, ( _, T_STRING1left, T_STRING1right)) :: rest671)) =>
 let val  result = MlyValue.tipoS (fn _ => (TString))
 in ( LrTable.NT 3, ( result, T_STRING1left, T_STRING1right), rest671)

end
|  ( 8, ( ( _, ( _, T_BOOL1left, T_BOOL1right)) :: rest671)) => let
 val  result = MlyValue.tipoS (fn _ => (TBool))
 in ( LrTable.NT 3, ( result, T_BOOL1left, T_BOOL1right), rest671)
end
|  ( 9, ( ( _, ( _, _, PONT_VIRG1right)) :: ( _, ( MlyValue.declaracao
 declaracao1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let
 val  result = MlyValue.def_var (fn _ => let val  (declaracao as 
declaracao1) = declaracao1 ()
 in (declaracao)
end)
 in ( LrTable.NT 2, ( result, VAR1left, PONT_VIRG1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.def_var def_var1, _, def_var1right)) :: _
 :: ( _, ( MlyValue.declaracao declaracao1, _, _)) :: ( _, ( _, 
VAR1left, _)) :: rest671)) => let val  result = MlyValue.def_var (fn _
 => let val  (declaracao as declaracao1) = declaracao1 ()
 val  (def_var as def_var1) = def_var1 ()
 in (fn x => def_var(declaracao x))
end)
 in ( LrTable.NT 2, ( result, VAR1left, def_var1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.tipoS tipoS1, _, tipoS1right)) :: _ :: ( _,
 ( MlyValue.IDENT IDENT1, IDENT1left, _)) :: rest671)) => let val  
result = MlyValue.declaracao (fn _ => let val  (IDENT as IDENT1) = 
IDENT1 ()
 val  (tipoS as tipoS1) = tipoS1 ()
 in (fn x => XSQLLib.addVarS(IDENT, tipoS, x))
end)
 in ( LrTable.NT 4, ( result, IDENT1left, tipoS1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.IDENT IDENT2, _, IDENT2right)) :: _ :: ( _,
 ( MlyValue.IDENT IDENT1, IDENT1left, _)) :: rest671)) => let val  
result = MlyValue.declaracao (fn _ => let val  IDENT1 = IDENT1 ()
 val  IDENT2 = IDENT2 ()
 in (fn x => XSQLLib.addVarT(IDENT1, IDENT2, x))
end)
 in ( LrTable.NT 4, ( result, IDENT1left, IDENT2right), rest671)
end
|  ( 13, ( ( _, ( _, _, PONT_VIRG1right)) :: ( _, ( MlyValue.comando 
comando1, comando1left, _)) :: rest671)) => let val  result = 
MlyValue.bloco (fn _ => let val  (comando as comando1) = comando1 ()
 in (comando)
end)
 in ( LrTable.NT 7, ( result, comando1left, PONT_VIRG1right), rest671)

end
|  ( 14, ( ( _, ( MlyValue.bloco bloco1, _, bloco1right)) :: _ :: ( _,
 ( MlyValue.comando comando1, comando1left, _)) :: rest671)) => let
 val  result = MlyValue.bloco (fn _ => let val  (comando as comando1)
 = comando1 ()
 val  (bloco as bloco1) = bloco1 ()
 in (fn x => bloco (comando x))
end)
 in ( LrTable.NT 7, ( result, comando1left, bloco1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.atrib atrib1, atrib1left, atrib1right)) :: 
rest671)) => let val  result = MlyValue.comando (fn _ => let val  (
atrib as atrib1) = atrib1 ()
 in (atrib)
end)
 in ( LrTable.NT 8, ( result, atrib1left, atrib1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.while_com while_com1, while_com1left, 
while_com1right)) :: rest671)) => let val  result = MlyValue.comando
 (fn _ => let val  (while_com as while_com1) = while_com1 ()
 in (while_com)
end)
 in ( LrTable.NT 8, ( result, while_com1left, while_com1right), 
rest671)
end
|  ( 17, ( ( _, ( MlyValue.ifelse ifelse1, ifelse1left, ifelse1right))
 :: rest671)) => let val  result = MlyValue.comando (fn _ => let val 
 (ifelse as ifelse1) = ifelse1 ()
 in (ifelse)
end)
 in ( LrTable.NT 8, ( result, ifelse1left, ifelse1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.select select1, select1left, select1right))
 :: rest671)) => let val  result = MlyValue.comando (fn _ => let val 
 (select as select1) = select1 ()
 in (fn x => #1 (x, XSQLLib.printTable(select x)))
end)
 in ( LrTable.NT 8, ( result, select1left, select1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.create create1, create1left, create1right))
 :: rest671)) => let val  result = MlyValue.comando (fn _ => let val 
 (create as create1) = create1 ()
 in (create)
end)
 in ( LrTable.NT 8, ( result, create1left, create1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.update update1, update1left, update1right))
 :: rest671)) => let val  result = MlyValue.comando (fn _ => let val 
 (update as update1) = update1 ()
 in (update)
end)
 in ( LrTable.NT 8, ( result, update1left, update1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.delete delete1, delete1left, delete1right))
 :: rest671)) => let val  result = MlyValue.comando (fn _ => let val 
 (delete as delete1) = delete1 ()
 in (delete)
end)
 in ( LrTable.NT 8, ( result, delete1left, delete1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.IDENT IDENT1, IDENT1left, _)) :: rest671)) => let val  result
 = MlyValue.atrib (fn _ => let val  (IDENT as IDENT1) = IDENT1 ()
 val  (exp as exp1) = exp1 ()
 in (fn x => XSQLLib.atribuicao(IDENT, exp x, x))
end)
 in ( LrTable.NT 9, ( result, IDENT1left, exp1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.select select1, _, select1right)) :: _ :: (
 _, ( MlyValue.IDENT IDENT1, IDENT1left, _)) :: rest671)) => let val  
result = MlyValue.atrib (fn _ => let val  (IDENT as IDENT1) = IDENT1
 ()
 val  (select as select1) = select1 ()
 in (fn x => #1 (x,
					XSQLLib.atribSelect(IDENT, select x, x)))
end
)
 in ( LrTable.NT 9, ( result, IDENT1left, select1right), rest671)
end
|  ( 24, ( ( _, ( _, _, CHAVE_D1right)) :: ( _, ( MlyValue.bloco 
bloco1, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _, 
WHILE1left, _)) :: rest671)) => let val  result = MlyValue.while_com
 (fn _ => let val  (exp as exp1) = exp1 ()
 val  (bloco as bloco1) = bloco1 ()
 in (fn x => XSQLLib.enquanto(exp,bloco,x))
end)
 in ( LrTable.NT 10, ( result, WHILE1left, CHAVE_D1right), rest671)

end
|  ( 25, ( ( _, ( _, _, CHAVE_D1right)) :: ( _, ( MlyValue.bloco 
bloco1, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _, 
IF1left, _)) :: rest671)) => let val  result = MlyValue.ifelse (fn _
 => let val  (exp as exp1) = exp1 ()
 val  (bloco as bloco1) = bloco1 ()
 in (fn x => XSQLLib.condicao(exp x, bloco, (fn x => x), x))
end)
 in ( LrTable.NT 11, ( result, IF1left, CHAVE_D1right), rest671)
end
|  ( 26, ( ( _, ( _, _, CHAVE_D2right)) :: ( _, ( MlyValue.bloco 
bloco2, _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.bloco bloco1, _, _))
 :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _, IF1left, _)) ::
 rest671)) => let val  result = MlyValue.ifelse (fn _ => let val  (exp
 as exp1) = exp1 ()
 val  bloco1 = bloco1 ()
 val  bloco2 = bloco2 ()
 in (fn x => XSQLLib.condicao(exp x,bloco1,bloco2,x))
end)
 in ( LrTable.NT 11, ( result, IF1left, CHAVE_D2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.IDENT IDENT1, IDENT1left, IDENT1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (IDENT
 as IDENT1) = IDENT1 ()
 in (fn x => XSQLLib.valorar(IDENT, x))
end)
 in ( LrTable.NT 22, ( result, IDENT1left, IDENT1right), rest671)
end
|  ( 28, ( ( _, ( _, _, PAREN_D2right)) :: ( _, ( MlyValue.exp exp2, _
, _)) :: _ :: ( _, ( MlyValue.op_bin op_bin1, _, _)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, PAREN_E1left, _)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  (op_bin as op_bin1) = op_bin1 ()
 val  exp2 = exp2 ()
 in (fn x => op_bin(exp1 x, exp2 x))
end)
 in ( LrTable.NT 22, ( result, PAREN_E1left, PAREN_D2right), rest671)

end
|  ( 29, ( ( _, ( _, _, PAREN_D1right)) :: ( _, ( MlyValue.exp exp1, _
, _)) :: _ :: ( _, ( _, NOT1left, _)) :: rest671)) => let val  result
 = MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (fn x => XSQLLib.negar(exp x))
end)
 in ( LrTable.NT 22, ( result, NOT1left, PAREN_D1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)
) => let val  result = MlyValue.exp (fn _ => let val  (INT as INT1) = 
INT1 ()
 in (fn _ => VInt INT)
end)
 in ( LrTable.NT 22, ( result, INT1left, INT1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.REAL REAL1, REAL1left, REAL1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (REAL
 as REAL1) = REAL1 ()
 in (fn _ => VReal REAL)
end)
 in ( LrTable.NT 22, ( result, REAL1left, REAL1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.STRING STRING1, STRING1left, STRING1right))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (
STRING as STRING1) = STRING1 ()
 in (fn _ => VString STRING)
end)
 in ( LrTable.NT 22, ( result, STRING1left, STRING1right), rest671)

end
|  ( 33, ( ( _, ( MlyValue.BOOL BOOL1, BOOL1left, BOOL1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (BOOL
 as BOOL1) = BOOL1 ()
 in (fn _ => VBool BOOL)
end)
 in ( LrTable.NT 22, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 34, ( ( _, ( _, MAIS1left, MAIS1right)) :: rest671)) => let val  
result = MlyValue.op_bin (fn _ => (XSQLLib.somar))
 in ( LrTable.NT 23, ( result, MAIS1left, MAIS1right), rest671)
end
|  ( 35, ( ( _, ( _, MENOS1left, MENOS1right)) :: rest671)) => let
 val  result = MlyValue.op_bin (fn _ => (XSQLLib.subtrair))
 in ( LrTable.NT 23, ( result, MENOS1left, MENOS1right), rest671)
end
|  ( 36, ( ( _, ( _, MULT1left, MULT1right)) :: rest671)) => let val  
result = MlyValue.op_bin (fn _ => (XSQLLib.multiplicar))
 in ( LrTable.NT 23, ( result, MULT1left, MULT1right), rest671)
end
|  ( 37, ( ( _, ( _, DIV1left, DIV1right)) :: rest671)) => let val  
result = MlyValue.op_bin (fn _ => (XSQLLib.dividir))
 in ( LrTable.NT 23, ( result, DIV1left, DIV1right), rest671)
end
|  ( 38, ( ( _, ( _, MOD1left, MOD1right)) :: rest671)) => let val  
result = MlyValue.op_bin (fn _ => (XSQLLib.modulo))
 in ( LrTable.NT 23, ( result, MOD1left, MOD1right), rest671)
end
|  ( 39, ( ( _, ( _, AND1left, AND1right)) :: rest671)) => let val  
result = MlyValue.op_bin (fn _ => (XSQLLib.eLogico))
 in ( LrTable.NT 23, ( result, AND1left, AND1right), rest671)
end
|  ( 40, ( ( _, ( _, OR1left, OR1right)) :: rest671)) => let val  
result = MlyValue.op_bin (fn _ => (XSQLLib.ouLogico))
 in ( LrTable.NT 23, ( result, OR1left, OR1right), rest671)
end
|  ( 41, ( ( _, ( _, IGUAL1left, IGUAL1right)) :: rest671)) => let
 val  result = MlyValue.op_bin (fn _ => (XSQLLib.igual))
 in ( LrTable.NT 23, ( result, IGUAL1left, IGUAL1right), rest671)
end
|  ( 42, ( ( _, ( _, DIF1left, DIF1right)) :: rest671)) => let val  
result = MlyValue.op_bin (fn _ => (
fn (x,y) => XSQLLib.negar (XSQLLib.igual (x,y))))
 in ( LrTable.NT 23, ( result, DIF1left, DIF1right), rest671)
end
|  ( 43, ( ( _, ( _, MAIOR1left, MAIOR1right)) :: rest671)) => let
 val  result = MlyValue.op_bin (fn _ => (XSQLLib.maior))
 in ( LrTable.NT 23, ( result, MAIOR1left, MAIOR1right), rest671)
end
|  ( 44, ( ( _, ( _, MENOR1left, MENOR1right)) :: rest671)) => let
 val  result = MlyValue.op_bin (fn _ => (XSQLLib.menor))
 in ( LrTable.NT 23, ( result, MENOR1left, MENOR1right), rest671)
end
|  ( 45, ( ( _, ( _, MAIGUAL1left, MAIGUAL1right)) :: rest671)) => let
 val  result = MlyValue.op_bin (fn _ => (
fn (x,y) => XSQLLib.ouLogico (XSQLLib.maior (x,y), XSQLLib.igual (x,y))
))
 in ( LrTable.NT 23, ( result, MAIGUAL1left, MAIGUAL1right), rest671)

end
|  ( 46, ( ( _, ( _, MENIGUAL1left, MENIGUAL1right)) :: rest671)) =>
 let val  result = MlyValue.op_bin (fn _ => (
fn (x,y) => XSQLLib.ouLogico (XSQLLib.menor (x,y), XSQLLib.igual (x,y))
))
 in ( LrTable.NT 23, ( result, MENIGUAL1left, MENIGUAL1right), rest671
)
end
|  ( 47, ( ( _, ( MlyValue.where_exp where_exp1, _, where_exp1right))
 :: ( _, ( MlyValue.IDENT IDENT1, _, _)) :: _ :: ( _, ( 
MlyValue.atributos atributos1, _, _)) :: ( _, ( _, SELECT1left, _)) ::
 rest671)) => let val  result = MlyValue.select (fn _ => let val  (
atributos as atributos1) = atributos1 ()
 val  (IDENT as IDENT1) = IDENT1 ()
 val  (where_exp as where_exp1) = where_exp1 ()
 in (
fn x =>
					let
						val (tabFiltrada,_) = where_exp (IDENT, x)
					in
						XSQLLib.select (tabFiltrada, atributos, x)
					end
)
end)
 in ( LrTable.NT 17, ( result, SELECT1left, where_exp1right), rest671)

end
|  ( 48, ( ( _, ( MlyValue.where_exp where_exp1, _, where_exp1right))
 :: ( _, ( MlyValue.IDENT IDENT1, _, _)) :: _ :: _ :: ( _, ( _, 
SELECT1left, _)) :: rest671)) => let val  result = MlyValue.select (fn
 _ => let val  (IDENT as IDENT1) = IDENT1 ()
 val  (where_exp as where_exp1) = where_exp1 ()
 in (
fn x =>
					let
						val (tabFiltrada,_) = where_exp (IDENT, x)
					in
						tabFiltrada
					end
)
end)
 in ( LrTable.NT 17, ( result, SELECT1left, where_exp1right), rest671)

end
|  ( 49, ( ( _, ( MlyValue.atributo atributo1, atributo1left, 
atributo1right)) :: rest671)) => let val  result = MlyValue.atributos
 (fn _ => let val  (atributo as atributo1) = atributo1 ()
 in ([atributo])
end)
 in ( LrTable.NT 18, ( result, atributo1left, atributo1right), rest671
)
end
|  ( 50, ( ( _, ( MlyValue.atributos atributos1, _, atributos1right))
 :: _ :: ( _, ( MlyValue.atributo atributo1, atributo1left, _)) :: 
rest671)) => let val  result = MlyValue.atributos (fn _ => let val  (
atributo as atributo1) = atributo1 ()
 val  (atributos as atributos1) = atributos1 ()
 in (atributo::atributos)
end)
 in ( LrTable.NT 18, ( result, atributo1left, atributos1right), 
rest671)
end
|  ( 51, ( ( _, ( MlyValue.IDENT IDENT1, IDENT1left, IDENT1right)) :: 
rest671)) => let val  result = MlyValue.atributo (fn _ => let val  (
IDENT as IDENT1) = IDENT1 ()
 in ((IDENT, fn x => XSQLLib.valorar(IDENT, x)))
end)
 in ( LrTable.NT 19, ( result, IDENT1left, IDENT1right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.IDENT IDENT1, _, IDENT1right)) :: _ :: ( _,
 ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.atributo (fn _ => let val  (exp as exp1) = exp1 ()
 val  (IDENT as IDENT1) = IDENT1 ()
 in ((IDENT, exp))
end)
 in ( LrTable.NT 19, ( result, exp1left, IDENT1right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.IDENT IDENT1, _, IDENT1right)) :: _ :: ( _,
 ( MlyValue.STRING STRING1, _, _)) :: _ :: ( _, ( _, CREATE1left, _))
 :: rest671)) => let val  result = MlyValue.create (fn _ => let val  (
STRING as STRING1) = STRING1 ()
 val  (IDENT as IDENT1) = IDENT1 ()
 in (fn x => #1 (x,
						XSQLLib.create (STRING, IDENT, x)))
end)
 in ( LrTable.NT 16, ( result, CREATE1left, IDENT1right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.where_exp where_exp1, _, where_exp1right))
 :: ( _, ( MlyValue.atribuicoes atribuicoes1, _, _)) :: _ :: ( _, ( 
MlyValue.IDENT IDENT1, _, _)) :: ( _, ( _, UPDATE1left, _)) :: rest671
)) => let val  result = MlyValue.update (fn _ => let val  (IDENT as 
IDENT1) = IDENT1 ()
 val  (atribuicoes as atribuicoes1) = atribuicoes1 ()
 val  (where_exp as where_exp1) = where_exp1 ()
 in (
fn x => #1 (x,
						XSQLLib.update (IDENT, where_exp (IDENT, x), atribuicoes, x))
)
end)
 in ( LrTable.NT 13, ( result, UPDATE1left, where_exp1right), rest671)

end
|  ( 55, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.IDENT IDENT1, IDENT1left, _)) :: rest671)) => let val  result
 = MlyValue.atribuicoes (fn _ => let val  (IDENT as IDENT1) = IDENT1
 ()
 val  (exp as exp1) = exp1 ()
 in ([(IDENT,exp)])
end)
 in ( LrTable.NT 14, ( result, IDENT1left, exp1right), rest671)
end
|  ( 56, ( ( _, ( MlyValue.atribuicoes atribuicoes1, _, 
atribuicoes1right)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: (
 _, ( MlyValue.IDENT IDENT1, IDENT1left, _)) :: rest671)) => let val  
result = MlyValue.atribuicoes (fn _ => let val  (IDENT as IDENT1) = 
IDENT1 ()
 val  (exp as exp1) = exp1 ()
 val  (atribuicoes as atribuicoes1) = atribuicoes1 ()
 in ((IDENT,exp)::atribuicoes)
end)
 in ( LrTable.NT 14, ( result, IDENT1left, atribuicoes1right), rest671
)
end
|  ( 57, ( ( _, ( MlyValue.where_exp where_exp1, _, where_exp1right))
 :: ( _, ( MlyValue.IDENT IDENT1, _, _)) :: _ :: ( _, ( _, DELETE1left
, _)) :: rest671)) => let val  result = MlyValue.delete (fn _ => let
 val  (IDENT as IDENT1) = IDENT1 ()
 val  (where_exp as where_exp1) = where_exp1 ()
 in (
fn x =>
					let
						val (_,tabFiltrada) = where_exp (IDENT, x)
						val _ = XSQLLib.delete (IDENT, tabFiltrada, x)
					in
						x
					end
)
end)
 in ( LrTable.NT 15, ( result, DELETE1left, where_exp1right), rest671)

end
|  ( 58, ( ( _, ( MlyValue.condicoes condicoes1, _, condicoes1right))
 :: ( _, ( _, WHERE1left, _)) :: rest671)) => let val  result = 
MlyValue.where_exp (fn _ => let val  (condicoes as condicoes1) = 
condicoes1 ()
 in (
fn (nomeTab,x) => XSQLLib.filtrar (XSQLLib.leTabela(nomeTab, x), condicoes, x)
)
end)
 in ( LrTable.NT 20, ( result, WHERE1left, condicoes1right), rest671)

end
|  ( 59, ( rest671)) => let val  result = MlyValue.where_exp (fn _ =>
 (fn (nomeTab,x) => (XSQLLib.leTabela(nomeTab, x), [])))
 in ( LrTable.NT 20, ( result, defaultPos, defaultPos), rest671)
end
|  ( 60, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)
) => let val  result = MlyValue.condicoes (fn _ => let val  (exp as 
exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 21, ( result, exp1left, exp1right), rest671)
end
|  ( 61, ( ( _, ( MlyValue.condicoes condicoes1, _, condicoes1right))
 :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let
 val  result = MlyValue.condicoes (fn _ => let val  (exp as exp1) = 
exp1 ()
 val  (condicoes as condicoes1) = condicoes1 ()
 in (fn x => XSQLLib.eLogico (exp x, condicoes x))
end)
 in ( LrTable.NT 21, ( result, exp1left, condicoes1right), rest671)

end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Xsql_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun MAIS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun MENOS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun MULT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun IGUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun DIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun MENOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun MAIOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun MAIGUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun MENIGUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun PAREN_E (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun PAREN_D (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun CHAVE_E (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun CHAVE_D (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun DOIS_PONT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun PONT_VIRG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun VIRG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun T_INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun T_REAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun T_STRING (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun T_BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun REAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.REAL (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun BOOL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.BOOL (fn () => i),p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun SELECT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun DELETE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun UPDATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun CREATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun TABLE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun FROM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun WHERE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun AS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun SET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun STRUCT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun IDENT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.IDENT (fn () => i),p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
end
end

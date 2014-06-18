datatype TipoBasico = TInt | TReal | TString | TBool

type TipoTabela = (string * TipoBasico) list
type Tabela     = string * string  (*primeira string e' nome do tipo, segunda o arquivo*)

datatype Valor	= VInt of int
	| VReal of real
	| VString of string
	| VBool of bool
	| VTabela of Tabela
	| VTipo of TipoTabela


type Estado	= (string * Valor) list



fun erro msg = #2 (print msg, OS.Process.exit (OS.Process.failure));


fun paraString v = case v of
	  VInt i	=> Int.toString(i)
	| VReal r	=> Real.toString(r)
	| VBool b	=> Bool.toString(b)
	| VString s	=> s;


fun buscar (nome:string, sigma:Estado) =
let
	fun buscaRec s = case s of
		  (n,v)::xs	=> if String.compare(nome, n) = EQUAL
				then	SOME v
				else	buscaRec xs
		| []	=> NONE
in
	buscaRec sigma
end;




structure XSQLLib : sig
	val addTipo: string * TipoTabela * Estado -> Estado
	val addVarS: string * TipoBasico * Estado -> Estado
	val addVarT: string * string * Estado -> Estado
	val atribuicao: string * Valor * Estado -> Estado
	val condicao: Valor * (Estado->Estado) * (Estado->Estado) * Estado -> Estado
	val enquanto: (Estado->Valor) * (Estado->Estado) * Estado -> Estado
	
	val valorar:	string * Estado -> Valor
	val negar:	Valor -> Valor
	val somar:	Valor * Valor -> Valor
	val subtrair:	Valor * Valor -> Valor
	val multiplicar:Valor * Valor -> Valor
	val dividir:	Valor * Valor -> Valor
	val modulo:	Valor * Valor -> Valor
	val eLogico:	Valor * Valor -> Valor
	val ouLogico:	Valor * Valor -> Valor
	val igual:	Valor * Valor -> Valor
	val menor:	Valor * Valor -> Valor
	val maior:	Valor * Valor -> Valor

	val leTabela: string * Estado -> (string * Valor) list list
	val filtrar: ((string * Valor) list list) * (Estado->Valor) * Estado -> (string * Valor) list list
	val select_com: ((string * Valor) list list) * (string list) -> unit list
end = struct


fun addTipo (nome:string, campos:TipoTabela, sigma:Estado) =
	(nome, VTipo campos) :: sigma;



fun addVarS (nome:string, tipo:TipoBasico, sigma:Estado) = let
	val valor = case tipo of
		  TInt		=> VInt 0
		| TReal 	=> VReal 0.0
		| TString	=> VString ""
		| TBool		=> VBool false
in
	(nome, valor) :: sigma
end;



fun addVarT (nome:string, tipo:string, sigma:Estado) =
	case buscar (tipo, sigma) of
		  SOME (VTipo _)	=> (nome, VTabela (tipo,nome^".tmp")) :: sigma
		| NONE	  		=> erro("tipo nao existe");



fun atribuicao (nome:string, valor:Valor, sigma:Estado) =
let
	fun atribRec l = case l of
		(n,v)::xs	=> if String.compare(n,nome) = EQUAL
			then case (v, valor) of
				  (VInt _, VInt _)	=> (nome, valor) :: xs
				| (VReal _, VReal _)	=> (nome, valor) :: xs
				| (VString _, VString _) => (nome, valor) :: xs
				| (VBool _, VBool _)	=> (nome, valor) :: xs
				| (VTabela (t1,_), VTabela (t2,_)) => if String.compare(t1,t2) = EQUAL
					then (nome, valor) :: xs
					else erro ("Tipo invalido")
				| (VTabela (t,_), VString s) => (nome, VTabela(t, s)) :: xs
				| _			=> erro ("Tipo invalido")
			else (n,v)::(atribRec xs)
		| []	=> erro("Variavel nao declarada")
in
	atribRec sigma
end;



fun condicao (cond:Valor, branch1:Estado->Estado, branch2:Estado->Estado, sigma:Estado) =
	case cond of
		  VBool true	=> branch1 sigma
		| VBool false	=> branch2 sigma
		| _		=> erro("Tipo Inválido");



fun enquanto (cond:Estado->Valor, corpo:Estado->Estado, sigma:Estado) =
let
	fun enqRec s = case cond s of
		  VBool true	=> enqRec (corpo s)
		| VBool false	=> s
		| _		=> erro("Tipo Inválido")
in
	enqRec sigma
end;




fun valorar (nome:string, sigma:Estado) =
	case buscar (nome, sigma) of
		  SOME v	=> v
		| NONE		=> erro ("Variavel nao declarada");



fun negar valor	= case valor of
	  VBool b	=> VBool (not b)
	| _		=> erro ("Tipo invalido");


fun somar v	= case v of
	  (VInt a1,  VInt a2)	=> VInt (a1 + a2)
	| (VReal a1, VReal a2)	=> VReal (a1 + a2)
	| (VString a1, VString a2) => VString (a1 ^ a2)
	| _			=> erro ("Tipo invalido");


fun subtrair v	= case v of
	  (VInt a1,  VInt a2)	=> VInt (a1 - a2)
	| (VReal a1, VReal a2)	=> VReal (a1 - a2)
	| _			=> erro ("Tipo invalido");


fun multiplicar v = case v of
	  (VInt a1,  VInt a2)	=> VInt (a1 * a2)
	| (VReal a1, VReal a2)	=> VReal (a1 * a2)
	| _			=> erro ("Tipo invalido");


fun dividir v	= case v of
	  (VInt a1,  VInt a2)	=> VInt (a1 div a2)
	| (VReal a1, VReal a2)	=> VReal (a1 / a2)
	| _			=> erro ("Tipo invalido");


fun modulo v	= case v of
	  (VInt a1,  VInt a2)	=> VInt (a1 mod a2)
	| _			=> erro ("Tipo invalido");


fun eLogico v	= case v of
	  (VBool a1, VBool a2)	=> VBool (a1 andalso a2)
	| _			=> erro ("Tipo invalido");


fun ouLogico v	= case v of
	  (VBool a1, VBool a2)	=> VBool (a1 orelse a2)
	| _			=> erro ("Tipo invalido");


fun igual v	= case v of
	  (VInt a1,  VInt a2)	=> VBool (a1 = a2)
(*	| (VReal a1, VReal a2)	=> VBool (a1 = a2) *)
	| (VBool a1, VBool a2)	=> VBool (a1 = a2)
	| (VString a1, VString a2) => VBool (String.compare(a1,a2) = EQUAL)
	| _			=> erro ("Tipo invalido");


fun maior v	= case v of
	  (VInt a1,  VInt a2)	=> VBool (a1 > a2)
	| (VReal a1, VReal a2)	=> VBool (a1 > a2)
	| (VString a1, VString a2) => VBool (String.compare(a1,a2) = GREATER)
	| _			=> erro ("Tipo invalido");


fun menor v	= case v of
	  (VInt a1,  VInt a2)	=> VBool (a1 < a2)
	| (VReal a1, VReal a2)	=> VBool (a1 < a2)
	| (VString a1, VString a2) => VBool (String.compare(a1,a2) = LESS)
	| _			=> erro ("Tipo invalido");




fun filtrar (t:(string * Valor) list list, cond:Estado->Valor, sigma:Estado) =
let
	fun respeitaCond item = case cond (item @ sigma) of
		  VBool v	=> v
		| _		=> erro("Expressao de tipo invalido")
in
	List.filter respeitaCond t
end;



fun leTabela (nomeTab:string, sigma:Estado) =
let
	val (nomeTipo, f) = case buscar (nomeTab,sigma) of
		  SOME (VTabela v)	=> v
		| NONE			=> erro ("Tabela nao declarada")
	
	val tipo = case buscar (nomeTipo, sigma) of
		  SOME (VTipo t)	=> t

	val tags = map (fn (x,_) => x) tipo

	val tab = XMLUtils.mapFile (tags, nomeTipo, f)

	fun traduzir (nome, valor) = 
	let
		fun encontrar ((n,v)::xs) = if String.compare(nome, n) = EQUAL
			then v
			else encontrar xs
	in
		case encontrar tipo of
			  TInt	=> (case Int.fromString valor of
				  SOME v	=> (nome, VInt v)
				| NONE		=> erro ("Arquivo nao corresponde ao tipo"))
			| TReal	=> (case Real.fromString valor of
				  SOME v	=> (nome, VReal v)
				| NONE		=> erro ("Arquivo nao corresponde ao tipo"))
			| TBool => (case Bool.fromString valor of
				  SOME v	=> (nome, VBool v)
				| NONE		=> erro ("Arquivo nao corresponde ao tipo"))
			| TString => (nome, VString valor)
	end
in
	map (map traduzir) tab
end;



fun select_com (t:(string * Valor) list list, atribs:string list) =
let
	fun selRec _ [] = TextIO.print ""
	 |  selRec atrib ((nome,valor)::xs) = if String.compare(atrib,nome) = EQUAL
	 	then TextIO.print (nome^": "^paraString(valor))
		else selRec atrib xs
	
	fun recAtribs [] _= TextIO.print ""
	 |  recAtribs (x::xs) ln = selRec x ln
in
	map (recAtribs atribs) t
end;


(*
fun select_exp (nomeVar:string, t:(string * Valor) list list, atribs:string list, sigma:Estado) =
let
  	val var = valorar (nomeVar, sigma)
*)

end

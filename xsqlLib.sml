datatype TipoBasico = TInt | TReal | TString | TBool

type TipoTabela = (string * TipoBasico) list
type Tabela     = string * string  (*primeira string e' nome do tipo, segunda o arquivo*)

datatype Valor	= VInt of int
		| VReal of real
		| VString of string
		| VBool of bool
		| VTabela of Tabela
		| VTipo of TipoTabela

type ConteudoTabela = (string * Valor) list list

type Estado	= (string * Valor) list




fun erro msg = #2 (
	print (msg^"\n"),
	OS.Process.exit (OS.Process.failure)
);


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


fun escreverArq (nomeArq:string, separador:string, tab:ConteudoTabela) = 
let
	val arq = TextIO.openOut nomeArq

	fun printLnRec [] = ()
	 |  printLnRec ((nome,valor)::ln) = #2(
	 	TextIO.output (arq, "  <"^nome^">"^(paraString valor)^"</"^nome^">\n"),
		printLnRec ln
	 )

	fun printLn ln = #2(
		TextIO.output (arq, "<"^separador^">\n"),
		printLnRec ln,
		TextIO.output (arq, "</"^separador^">\n")
	)
in
	map (printLn) tab before TextIO.closeOut arq
end;




structure XSQLLib : sig
	val addTipo: string * TipoTabela * Estado -> Estado
	val addVarS: string * TipoBasico * Estado -> Estado
	val addVarT: string * string * Estado -> Estado
	val atribuicao: string * Valor * Estado -> Estado
	val atribSelect: string * ConteudoTabela * Estado -> unit list
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

	val leTabela: string * Estado -> ConteudoTabela
	val filtrar: ConteudoTabela * (Estado->Valor) * Estado -> (ConteudoTabela * ConteudoTabela)
	val select: ConteudoTabela * ((string * (Estado -> Valor)) list) * Estado -> ConteudoTabela
	val create: string * string * Estado -> OS.Process.status
	val update: string * (ConteudoTabela * ConteudoTabela) *
				((string * (Estado -> Valor)) list) * Estado -> unit list
	val delete: string * ConteudoTabela * Estado -> unit list
	val printTable: ConteudoTabela -> unit list
end = struct


fun addTipo (nome:string, campos:TipoTabela, sigma:Estado) = (nome, VTipo campos) :: sigma;



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



fun atribSelect (nome:string, tab:ConteudoTabela, sigma:Estado) =
let
	val (nomeTipo, arq) = case buscar (nome, sigma) of
		  SOME (VTabela v)	=> v
		| NONE			=> erro ("Tabela nao declarada")

	val tipoCompativel =
	let
		val tipo = case buscar (nomeTipo, sigma) of
			  SOME (VTipo t) => t

		val ln = hd tab

		fun mesmoTipo (tb, v) = case (tb, v) of
			  (TInt, VInt _)	=> true
			| (TReal, VReal _)	=> true
			| (TString, VString _)	=> true
			| (TBool, VBool _)	=> true
			| _			=> false

		fun compativel [] _ = false
		 | compativel ((tag,tBasico)::xs) (tag',valor) = if String.compare(tag,tag') = EQUAL
		 	then if mesmoTipo (tBasico, valor)
				then true
				else false
			else compativel xs (tag',valor)
	in
		foldl (fn (x,ac) => ac andalso (compativel tipo x)) true ln
	end
in
	if tipoCompativel
		then escreverArq (arq, nomeTipo, tab)
		else erro ("Tabelas incompatíveis")
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
		| NONE		=> erro ("Identificador nao declarado");



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




(*Divide a lista em duas: a primeira respeita a condição, a segunda não.*)
fun filtrar (t:ConteudoTabela, cond:Estado->Valor, sigma:Estado) =
let
	fun respeitaCond item = case cond (item @ sigma) of
		  VBool v	=> v
		| _		=> erro("Expressao de tipo invalido")
	
	fun filtro [] = ([],[])
	 |  filtro (x::xs) =
	 let
	 	val (l1,l2) = filtro xs
	 in
		case respeitaCond x of
	 		  true	=> (x::l1, l2)
			| false => (l1, x::l2)
	end
in
	filtro t
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



fun select (t:ConteudoTabela, atribs:(string * (Estado->Valor)) list, sigma:Estado) =
let
	fun recAtribs [] _= []
	 |  recAtribs ((tag,exp)::xs) ln =
	 let
	 	val valor = exp (ln @ sigma)
	 in
	 	(tag, valor)::(recAtribs xs ln)
	 end
in
	map (recAtribs atribs) t
end;



fun create (nomeNovo:string, nomeTab:string, sigma:Estado) =
let
	val nomeAntigo = case buscar (nomeTab, sigma) of
		  SOME (VTabela (_,nomeArq))	=> nomeArq
		| NONE				=> erro ("Tabela nao declarada.")
in
	OS.Process.system ("cp " ^ nomeAntigo ^ " " ^ nomeNovo)
end;
	


fun update (nomeTab:string, (t1, t2), atribs:(string * (Estado->Valor)) list, sigma:Estado) =
let
	val (tipo, arq) = case buscar (nomeTab, sigma) of
		  SOME (VTabela (t,a))	=> (t,a)

	fun vals ln = map (fn (a,f) => (a,f (ln @ sigma))) atribs

	fun atribui [] s = s
	 |  atribui ((a,v)::xs) s = atribuicao (a, v, atribui xs s)

	val novaTab = map (fn x => atribui (vals x) x) t1
in
	escreverArq (arq, tipo, (novaTab @ t2))
end;

	

fun delete (nomeTab:string, tab:ConteudoTabela, sigma:Estado) =
let
	val (tipo, arq) = case buscar (nomeTab, sigma) of
		  SOME (VTabela (t,a))	=> (t,a)
in
	escreverArq (arq, tipo, tab)
end;



fun printTable (tab:ConteudoTabela) =
let
	fun printLine [] = print "\n"
	 |  printLine ((tag,valor)::xs) = #2(
	 	print ("\t"^tag^": "^paraString(valor)),
		printLine xs
	)
in
	map printLine tab
end;

end

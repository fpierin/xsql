datatype TipoTag = SEPARADOR
		 | TAG of string * string
		 | NADA

(*
	Esta funcao recupera o indice de um substring dentro de uma linha

	Exemplo de uso: 
		index("el","felipe");

	@param substr	- O substring
	@param str 		- A string inicial
	@return int 	- A posicao encontrada
*)
fun index(substr:string, str:string) = let
  val (pref, suff) = Substring.position substr (Substring.full str)
  val (s, i, n) = Substring.base suff
in
  if i = size str then
    ~1
  else
    i
end;

(*
	Esta funcao avalia se um determinado substring existe dentro de uma string

	Exemplo de uso: 
		contains("el","felipe");

	@param substr	- O substring
	@param str 		- A string inicial
	@return boolean	- O resultado da avaliacao
*)
fun contains(substr:string, str:string) = index(substr^">", str) > ~1;

(*
	Esta funcao realiza a leitura linha a linha de um arquivo qualquer

	Exemplo de uso: 
		readFile("/home/felipe/workspace/xsql/alunos.xml");

	@param file 	- O Arquivo que sera lido
	@return list 	- Lista contendo as linhas do arquivo
*)
fun readFile(file: string) = let
	val myfile = TextIO.openIn file
	fun read myfile =
		case TextIO.inputLine myfile of
			SOME line => line :: (read myfile)
		|	NONE => []
in
	read myfile before TextIO.closeIn myfile
end;

(*
	Esta funcao auxiliar remove a tag sufixo de um XML.

	Exemplo de uso: 
		xmlRmSu("nome", "<nome>Felipe</nome>");

	@param prop 	- O nome da tag
	@param str 		- A linha que sera avaliada
	@return string 	- O resultado da remocao da tag de sufixo
*)
fun xmlRmSu(prop, str) = let
	val tag = "</" ^ prop ^ ">"
	val tagS = size(tag)
	val posI = index(tag, str)
in
	case String.compare(tag, str) of
		EQUAL => ""
	| 	_ => if (posI >= 0) then substring(str, 0, posI) else str
end;

(*
	Esta funcao auxiliar remove a tag prefixo de um XML.

	Exemplo de uso: 
		xmlRmPr("nome", "<nome>Felipe</nome>");

	@param prop 	- O nome da tag
	@param str 		- A linha que sera avaliada
	@return string 	- O resultado da remocao da tag de sufixo
*)
fun xmlRmPr(prop, str) = let
	val tag = "<" ^ prop ^ ">"
	val tagS = size(tag)
	val posI = index(tag, str) + tagS
in
	case String.compare(tag, str) of
		EQUAL => ""
	| 	_ => if (posI > ~1) then substring(str, posI, size(str) - posI) else ""
end;

(*
	Esta funcao auxiliar recupera o valor dentro de uma tag XML.

	Exemplo de uso: 
		xmlVal("nome", "<nome>Felipe</nome>");

	@param prop 	- O nome da tag
	@param str 		- A linha que sera avaliada
	@return string 	- O resultado da remocao da tag de sufixo
*)
fun xmlVal(prop, str) = xmlRmSu(prop, xmlRmPr(prop, str));

(*
	Esta funcao avalia a existencia de uma determinada Tag em uma linha.

	Exemplo de uso: 
		readIfLn(["nome", "idade"], "<nome>Felipe</nome>");

	@param f 		- A funcao booleana que indica a forma de selecao
	@param [] 		- A lista contendo os valores a serem avaliados
	@param s 		- A linha que sera avaliada
	@return list 	- Lista contendo Os valores encontrados
*)
fun readIfLn(f,tags,separador,l) =
let
	fun readIfLnRec [] = NADA
	 |  readIfLnRec (x::xs) = if f(x, l)
	 	then TAG (x, xmlVal(x, l))
		else readIfLnRec xs
in
	if f(separador,l)
		then SEPARADOR
		else readIfLnRec tags
end;

(*
	Esta funcao recupera uma lista de atributos de uma lista de linhas.

	Exemplo de uso: 
		readIf(["nome", "idade"], ["<nome>Felipe</nome>", "<idade>27</idade>"]);

	@param f 		- A funcao booleana que indica a forma de selecao
	@param [] 		- A lista contendo os valores a serem avaliados
	@param [] 		- A lista contendo as linhas a serem avaliados
	@return string 	- Os valores encontrados
*)
fun readIf(f,tags,separador,file) =
let
	fun readIfInterno [] = ([],[])
	 |  readIfInterno (x::xs) = case readIfLn(f,tags,separador,x) of
		  NADA		=> readIfInterno xs
		| SEPARADOR	=> ([],xs)
		| TAG valor	=>
			let
				val (vals,xs') = readIfInterno xs
			in
				(valor::vals, xs')
			end

	fun readIfExterno [] = []
	 |  readIfExterno (x::xs) = case readIfLn(f,tags,separador,x) of
		  NADA		=> readIfExterno xs
		| TAG _		=> readIfExterno xs
		| SEPARADOR	=>
			let
				val (vals,xs') = readIfInterno xs
			in
				vals::readIfExterno xs'
			end
in
	readIfExterno file
end;

(*
	Esta mapeia as tags de um determinado XML

	Exemplo de uso: 
		mapFile(["nome","idade"],"alunos.xml"); 
		mapFile(["nome"],"alunos.xml");

	Exemplo de saida:
		- mapFile(["nome","idade"],"alunos.xml");
		val it =
		  [[["nome","Felipe"],["idade","26"]],[["nome","Rafael"]],[["idade","23"]],
		   [["nome","Vazio"]],[["idade",""]]] : string list list list

		- mapFile(["nome"],"alunos.xml");        
			val it = [[["nome","Felipe"]],[["nome","Rafael"]],[["nome","Vazio"]]]
		  : string list list list

	@param []		- A lista de tags de interesse
	@param s 		- O caminho do arquivo XML
	@return list	- Uma lista contendo o mapa do resultado
*)

structure XMLUtils : sig
	val mapFile : (string list) * string * string -> (string * string) list list
end = struct

fun mapFile([],_,_) = nil
|	mapFile(tags,separador,file) = readIf(contains,tags,separador,readFile(file));

end

(*
    Esta função copia um arquivo por meio de uma chamada externa

    Exemplo de uso:
        doCreate("alunos.xml","alunos2.xml");

    @param iFile    - O arquivo de entrada
    @param oFile     - O arquivo de saída
    @return int    - O resultado da execução do comando externo
*)
fun doCreate(iFile, oFile) = OS.Process.system ("cp " ^ iFile ^ " " ^ oFile);

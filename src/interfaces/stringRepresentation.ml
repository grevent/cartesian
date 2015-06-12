
open CartesianTree
open BasicTools

let fromString str = 
	let buffer = Lexing.from_string str in
	Syntax.phrase Lex.lexer buffer
;;

let rec generateString x = match x with
	  ACTIONWRAPPER x -> (generateString x)
	| SEQUENCEACTION x -> "{"^(listIterator2String generateString ";" x)^"}"
	| WHILEACTION (x,y) -> "while "^(generateString x)^" do "^(generateString y)
	| ACTORSENDSACTION (blocking,rules) -> (listIterator2String (fun (id,pattern,action) -> id^" ==>"^(generateString pattern)^" -> "^(generateString action)) " | " rules)^(if blocking then "" else "| ...")
	| REGISTERSTARTACTION (id,action) -> id^" !=> "^(generateString action)
	| ACTORRECEIVESACTION (id,expr) -> id^" <== "^(generateString expr)
	| THREADACTION action -> "// "^(generateString action)
	| TRYACTION (actions,matches) -> 
		"try "^(listIterator2String generateString "; " actions)^" with "^(listIterator2String (fun (patterns,action) -> (listIterator2String generateString " " patterns)^" -> "^(generateString action)) " | " matches)
	| ACTIONEXPRESSION expr -> (generateString expr)
	| FUNCTION matches -> (listIterator2String (fun (patterns,expr) -> "lambda "^(listIterator2String generateString " " patterns)^" -> "^(generateString expr)) " | " matches)
	| WILDCARDPATTERN -> "_"
	| STRINGPROTOTYPE (uc,value) -> "\""^value^"\""
	| STRINGPATTERN value -> "\""^value^"\""
	| RENAMINGPATTERN (pattern,newId) -> (generateString pattern)^" as "^newId
	| RAISEACTION expr -> "raise "^(generateString expr)
	| QUOTEDIDEXPRESSION id -> "'"^id 
	| OBJECTWRAPPEREXPRESSION obj -> (generateString obj)
	| OBJECTPROTOTYPE (uc,attributes) -> (listIterator2String (fun (attribute,expr) -> attribute^"= "^(generateString expr)) "; " attributes)
	| OBJECTPATTERN attributes -> (listIterator2String (fun (attribute,expr) -> attribute^"= "^(generateString expr)) "; " attributes)
	| PROTOTYPESEXPRESSION (value,prototypes) -> "("^(generateString value)^(listIterator2String generateString "" prototypes)^")"
	| OBJECT attributes -> "{"^(listIterator2String (fun (attribute,expr) -> attribute^"= "^(generateString expr)) "; " attributes)^"}"
	| OBJECTEXPRESSION attributes -> "{"^(listIterator2String (fun (attribute,params,expr) -> attribute^(listIterator2String (fun param -> " "^(generateString param)) "" params)^(generateString expr)) "; " attributes)^"}"
	| NATIVEFUNCTION -> raise NativeFunction
	| BOOLEXPRESSION bl -> if bl then "true" else "false"
	| NUMEXPRESSION (re,im) -> (Printf.sprintf "(%f,%f)" re im)
	| MATRIXEXPRESSION mat -> 
		let (x,y) = MatrixTools.dim mat in
		let result = ref "[|" in
  
		for i = 0 to x - 1 do
			for j = 0 to y - 1 do
				result := !result ^ (generateString mat.(i).(j));
				if j != (y-1) then
					result := !result ^ "; ";
			done;
			
			if i != (x-1) then
				result := !result ^ " || "
			else
				result := !result ^ " |]";
		done;
		!result
	| MATRIXPATTERN mat -> 
		let (x,y) = MatrixTools.dim mat in
		let result = ref "[|" in
  
		for i = 0 to x - 1 do
			for j = 0 to y - 1 do
				result := !result ^ (generateString mat.(i).(j));
				if j != (y-1) then
					result := !result ^ "; ";
			done;
			
			if i != (x-1) then
				result := !result ^ " || "
			else
				result := !result ^ " |]";
		done;
		!result
	| MATRIXWRAPPER ar -> 
		MatrixTools.toString ar
	| LISTEXPRESSION lst -> "["^(listIterator2String generateString "; " lst)^"]"
	| NODEXPRESSION -> "nod"
	| STRINGEXPRESSION str -> "\""^str^"\""
	| IDEXPRESSION id -> id
	| NATIVEACTION _ -> raise NativeAction
	| ASSIGNACTION (id,expr) -> id^" <- "^(generateString expr)
	| CONTEXTACTION (context,expr) -> "context "^(generateString context)^" "^(generateString expr)
	| ATTRIBUTEACCESSEXPRESSION (expr,attr) -> "("^(generateString expr)^")."^attr
	| BOOLPATTERN bl -> if bl then "true" else "false"
	| BOOLPROTOTYPE (uc,bl) -> if bl then "true" else "false"
	| CONSPATTERN (car,cdr) -> "("^(generateString car)^")::("^(generateString cdr)^")"
	| FUNCTIONEXPRESSION lambdas -> "lambda "^(listIterator2String (fun (params,expr) -> (listIterator2String generateString " " params)^" -> "^(generateString expr)) " | " lambdas)
	| DEFINEACTION (id,params,expr) -> id^(listIterator2String (fun param -> " "^(generateString param)) "" params)^": "^(generateString expr)
	| DOACTION (action,expr) -> "do "^(generateString action)^" while "^(generateString expr)
	| EXPRACTION expr -> (generateString expr)
	| NUMPATTERN (re,im) -> (Printf.sprintf "(%f,%f)" re im)
 	| NUMPROTOTYPE (uc,(re,im)) -> (Printf.sprintf "(%f,%f)" re im)
	| MATRIXPROTOTYPE (uc,mat) -> MatrixTools.toString mat
	| FORACTION (id,expr,action) -> "for "^id^" in "^(generateString expr)^" do "^(generateString action)
	| IDPATTERN id -> id
	| COMMENT (comment,node) -> (generateString node)^" "^comment
	| WHEREPATTERN (pattern,expr) -> (generateString pattern)^" where "^(generateString expr)
	| FUNCTIONCALLEXPRESSION (expr,params) -> "("^(generateString expr)^(listIterator2String (fun param -> " "^(generateString param)) "" params)^")"
	| LISTPATTERN lst -> "["^(listIterator2String generateString "; " lst)^"]"
	| INSTANCESEXPRESSION prototypes -> "("^(listIterator2String generateString " " prototypes)^")"
	| LETEXPRESSION (defs,expr) -> "let "^(listIterator2String (fun (pattern,params,expr) -> (generateString pattern)^(listIterator2String (fun param -> " "^(generateString param)) "" params)^" = "^(generateString expr)) "and" defs)^" in"
	| LISTPROTOTYPE (uc,lst) -> "["^(listIterator2String generateString "; " lst)^"]"
	| MATCHEXPRESSION (expr,matches) -> "match "^(generateString expr)^" with "^(listIterator2String (fun (params,expr) -> (listIterator2String generateString " " params)^" -> "^(generateString expr)) " | " matches)
;;

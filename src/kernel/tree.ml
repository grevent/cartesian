
open Type
open StringTools

type exprNode = 
	FUNCTIONCALLEXPR of cType*exprNode*(exprNode list) |
	LAMBDAEXPR of cType*(((patternNode list)*exprNode) list) |
	LETEXPR of cType*((patternNode*exprNode) list)*exprNode |	
	MATCHEXPR of cType*exprNode*((patternNode*exprNode) list) |
	TYPEACCESSEXPR of cType*exprNode*typeNode |
	TYPEVERIFICATIONEXPR of cType*exprNode*typeNode |
	TOSUBTYPEEXPR of cType*exprNode*typeNode |
	CONVERSIONEXPR of cType*exprNode*typeNode |
	INTEXPR of int |
	FLOATEXPR of float |
	STRINGEXPR of string |
	BOOLEXPR of bool |
	IDEXPR of cType*string | 
	ACTIONEXPR of actionNode |
	LISTEXPR of cType*(exprNode list) |
	INTERVALEXPR of cType*exprNode*exprNode | 
	INTERVALSTEPEXPR of cType*exprNode*exprNode*exprNode |
	NODEXPR |
	PAIREXPR of cType*(exprNode list) |
	LISTCOMPREHENSIONEXPR of cType*exprNode*patternNode*exprNode |
	ARRAYEXPR of cType*(exprNode list)
and patternNode = 
	CONSPATTERN of cType*patternNode*patternNode |
	INTPATTERN of int |
	FLOATPATTERN of float |
	STRINGPATTERN of string |
	BOOLPATTERN of bool | 
	LISTPATTERN of cType*(patternNode list) | 
	RENAMINGPATTERN of cType*patternNode*string |
	WILDCARDPATTERN  of cType |
	PAIRPATTERN of cType*(patternNode list) | 
	IDPATTERN of cType*string |
	WHEREPATTERN of cType*patternNode*exprNode |
	ARRAYPATTERN of cType*(patternNode list) |
	TYPEDPATTERN of cType*patternNode*typeNode 
and typeNode = 
	NODTYPE |
	INTTYPE |
	FLOATTYPE |
	STRINGTYPE |
	ARRAYTYPE of typeNode |
	LISTTYPE of typeNode |
	GENTYPE of string |
	PAIRTYPE of (typeNode list) |
	NAMEDTYPE of (typeNode list)*string |
	ALTERNATIVESTYPE of (typeNode list) 
and actionNode =
	ASSIGNACTION of string*exprNode |
	DOACTION of exprNode |
	COPYACTION of string
and objectNode =
	OBJECT of syncMode*((string*exprNode) list)
and syncMode = 
	LOCAL | 
	INTERFACE of string
;; 

let rec exprToString expr = 
	match expr with
		FUNCTIONCALLEXPR (_,fn,params) -> "("^(concatAndInsert " " (List.map exprToString (fn::params)))^")" |
		LAMBDAEXPR (_,lambdas) -> "lambda "^(concatAndInsert " | " (List.map (fun (params,expr) -> (concatAndInsert " " (List.map patternToString params))^" -> "^(exprToString expr)) lambdas)) |
		LETEXPR (_,assigns,expr) -> "let "^(concatAndInsert " and " (List.map (fun (param,expr) -> (patternToString param)^" = "^(exprToString expr)) assigns))^" in "^(exprToString expr) |
		MATCHEXPR (_,expr,alternatives) -> "match "^(exprToString expr)^" with "^(concatAndInsert " | " (List.map (fun (param,expr) -> (patternToString param)^" -> "^(exprToString expr)) alternatives)) |
		TYPEACCESSEXPR (_,expr,tp) -> (exprToString expr)^".:"^(typeToString tp) |
		TYPEVERIFICATIONEXPR (_,expr,tp) -> "("^(exprToString expr)^": "^(typeToString tp)^")" |
		TOSUBTYPEEXPR (_,expr,tp) -> "("^(exprToString expr)^":> "^(typeToString tp)^")" |
		CONVERSIONEXPR (_,expr,tp) -> "("^(exprToString expr)^":< "^(typeToString tp)^")" |
		INTEXPR i -> (Printf.sprintf "%d" i) |
		FLOATEXPR f -> (Printf.sprintf "%f" f) |
		STRINGEXPR s -> (Printf.sprintf "\"%s\"" s) |
		BOOLEXPR b -> if b then "true" else "false" |
		IDEXPR (_,id) -> id |
		ACTIONEXPR a -> (actionToString a) |
		LISTEXPR (_,lst) -> "["^(concatAndInsert "; " (List.map exprToString lst))^"]" |
		INTERVALEXPR (_,start,ending) -> "["^(exprToString start)^" .. "^(exprToString ending)^"]" |
		INTERVALSTEPEXPR (_,value1,value2,lastValue) -> "["^(exprToString value1)^"; "^(exprToString value2)^" .. "^(exprToString lastValue)^"]" |
		NODEXPR -> "nod" |
		PAIREXPR (_,exprs) -> "("^(concatAndInsert ", " (List.map exprToString exprs))^")" |
		ARRAYEXPR (_,exprs) -> "[|"^(concatAndInsert "; " (List.map exprToString exprs))^"|]" |
		LISTCOMPREHENSIONEXPR (_,expr,pattern,set) -> "["^(exprToString expr)^" | "^(patternToString pattern)^" in "^(exprToString set)
and patternToString pattern = 
	match pattern with
		INTPATTERN i -> (Printf.sprintf "%d" i) |
		CONSPATTERN (_,car,cdr) -> (patternToString car)^"::"^(patternToString cdr) |
		FLOATPATTERN f -> (Printf.sprintf "%f" f) |
		STRINGPATTERN s -> (Printf.sprintf "\"%s\"" s) |
		BOOLPATTERN b -> if b then "true" else "false" |
		LISTPATTERN (_,lst) -> "["^(concatAndInsert "; " (List.map patternToString lst))^"]" |
		RENAMINGPATTERN (_,pt,id) -> (patternToString pt)^" as "^id |
		WILDCARDPATTERN _ -> "_" |
		PAIRPATTERN (_,lst) -> "("^(concatAndInsert ", " (List.map patternToString lst))^")" |
		IDPATTERN (_,id) -> id |
		WHEREPATTERN (_,pattern,expr) -> (patternToString pattern)^" where "^(exprToString expr) |
		ARRAYPATTERN (_,lst) -> "[|"^(concatAndInsert "; " (List.map patternToString lst))^"|]" |
		TYPEDPATTERN (_,pattern,tp) -> "("^(patternToString pattern)^": "^(typeToString tp)^")"
and typeToString tp = 
	match tp with
		NODTYPE -> "nod" |
		INTTYPE -> "int" |
		FLOATTYPE -> "float" |
		STRINGTYPE -> "string" |
		ARRAYTYPE t -> (typeToString t)^" array" |
		LISTTYPE t -> (typeToString t)^" list" |
		GENTYPE st -> "'"^st |
		PAIRTYPE lst -> "("^(concatAndInsert " " (List.map typeToString lst))^")" |
		NAMEDTYPE (lst,st) -> "("^(concatAndInsert " " (List.map typeToString lst))^" "^st^")" |
		ALTERNATIVESTYPE lst -> (concatAndInsert " | " (List.map typeToString lst)) 
and actionToString act = 
	match act with
		ASSIGNACTION (id,expr) -> id^"<- "^(exprToString expr) |
		DOACTION expr -> "do "^(exprToString expr) |
		COPYACTION id -> "copy "^id
and objectToString obj = 
	match obj with
		OBJECT (syncMode,attributes) -> "{"^(syncModeToString  syncMode)^(concatAndInsert "; " (List.map (fun (att,expr) -> att^"= "^(exprToString expr)) attributes))
and syncModeToString mode = 
	match mode with
		LOCAL -> "" |
		INTERFACE st -> "|" ^st^"|"
;; 
	 

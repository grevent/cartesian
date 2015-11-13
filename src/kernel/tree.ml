
open StringTools

type exprNode = 
	FUNCTIONCALLEXPR of int*exprNode*(exprNode list) |
	LAMBDAEXPR of int*(((patternNode list)*exprNode) list) |
	LETEXPR of int*((patternNode*(patternNode list)*exprNode) list)*exprNode |	
	MATCHEXPR of int*exprNode*(((patternNode list)*exprNode) list) |
	MATCHPOSSIBLEEXPR of int*exprNode*(((patternNode list)*exprNode) list) |
	TYPEACCESSEXPR of int*exprNode*typeNode |
	TYPEVERIFICATIONEXPR of int*exprNode*typeNode |
	TOSUBTYPEEXPR of int*exprNode*typeNode |
	CONVERSIONEXPR of int*exprNode*typeNode |
	INTEXPR of int |
	FLOATEXPR of float |
	STRINGEXPR of string |
	BOOLEXPR of bool |
	IDEXPR of int*string | 
	ACTIONEXPR of int*(actionNode list) |
	LISTEXPR of int*(exprNode list) |
	INTERVALEXPR of int*exprNode*exprNode | 
	INTERVALSTEPEXPR of int*exprNode*exprNode*exprNode |
	NODEXPR |
	PAIREXPR of int*(exprNode list) |
	LISTCOMPREHENSIONEXPR of int*exprNode*patternNode*exprNode |
	ARRAYEXPR of int*(exprNode list) |
	OBJEXPR of int*objectNode |
	TRANSITIONEXPR of int*transitionNode
and patternNode = 
	CONSPATTERN of int*patternNode*patternNode |
	INTPATTERN of int |
	FLOATPATTERN of float |
	STRINGPATTERN of string |
	BOOLPATTERN of bool | 
	LISTPATTERN of int*(patternNode list) | 
	RENAMINGPATTERN of int*patternNode*string |
	WILDCARDPATTERN  of int |
	PAIRPATTERN of int*(patternNode list) | 
	IDPATTERN of int*string |
	WHEREPATTERN of int*patternNode*exprNode |
	ARRAYPATTERN of int*(patternNode list) |
	TYPEDPATTERN of int*patternNode*typeNode 
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
	ALTERNATIVESTYPE of (typeNode list) |
	OBJECTTYPE |
	TRANSITIONTYPE 
and actionNode =
	ASSIGNACTION of string*exprNode |
	DOACTION of exprNode |
	COPYACTION of string |
	EXPRACTION of exprNode |
	NEWACTION of string*exprNode |
	DELETEACTION of string  |
	REPLACEACTION of string*exprNode |
	DEFINETYPEACTION of string*(string list)*typeNode |
	DEFINEACTION of string*(patternNode list)*exprNode |
	EXTERNACTION of string*typeNode
and objectNode =
	OBJECT of syncMode*((string*exprNode) list)
and syncMode = 
	LOCAL | 
	INTERFACE of string
and transitionNode =
	EXPRTRANS of ((objectPatternNode list list)*exprNode) |
	ACTIONTRANS of ((objectPatternNode list list)*exprNode) 
and objectPatternNode = 
	OPENOBJPATTERN |
	OBJPATTERN of string*patternNode
;; 



let rec exprToString expr = 
	match expr with
		FUNCTIONCALLEXPR (_,fn,params) -> "("^(concatAndInsert " " (List.map exprToString (fn::params)))^")" |
		LAMBDAEXPR (_,lambdas) -> "lambda "^(concatAndInsert " | " (List.map (fun (params,expr) -> (concatAndInsert " " (List.map patternToString params))^" -> "^(exprToString expr)) lambdas)) |
		LETEXPR (_,assigns,expr) -> "let "^(concatAndInsert " and " 
			(List.map (fun (param,patterns,expr) -> (patternToString param)^(concatAndInsert " " (List.map patternToString patterns))^" = "^(exprToString expr)) assigns))^" in "^(exprToString expr) |
		MATCHEXPR (_,expr,alternatives) -> "match "^(exprToString expr)^" with "^(concatAndInsert " | " (List.map (fun (params,expr) -> (concatAndInsert " " (List.map patternToString params))^" -> "^(exprToString expr)) alternatives)) |
		MATCHPOSSIBLEEXPR (_,expr,alternatives) -> "match possible "^(exprToString expr)^" with "^(concatAndInsert " | " (List.map (fun (params,expr) -> (concatAndInsert " " (List.map patternToString params))^" -> "^(exprToString expr)) alternatives)) |
		TYPEACCESSEXPR (_,expr,tp) -> (exprToString expr)^".:"^(typeToString tp) |
		TYPEVERIFICATIONEXPR (_,expr,tp) -> "("^(exprToString expr)^": "^(typeToString tp)^")" |
		TOSUBTYPEEXPR (_,expr,tp) -> "("^(exprToString expr)^":> "^(typeToString tp)^")" |
		CONVERSIONEXPR (_,expr,tp) -> "("^(exprToString expr)^":< "^(typeToString tp)^")" |
		INTEXPR i -> (Printf.sprintf "%d" i) |
		FLOATEXPR f -> (Printf.sprintf "%f" f) |
		STRINGEXPR s -> (Printf.sprintf "\"%s\"" s) |
		BOOLEXPR b -> if b then "true" else "false" |
		IDEXPR (_,id) -> id |
		ACTIONEXPR (_,a) -> (concatAndInsert "; " (List.map actionToString a)) |
		LISTEXPR (_,lst) -> "["^(concatAndInsert "; " (List.map exprToString lst))^"]" |
		INTERVALEXPR (_,start,ending) -> "["^(exprToString start)^" .. "^(exprToString ending)^"]" |
		INTERVALSTEPEXPR (_,value1,value2,lastValue) -> "["^(exprToString value1)^"; "^(exprToString value2)^" .. "^(exprToString lastValue)^"]" |
		NODEXPR -> "nod" |
		PAIREXPR (_,exprs) -> "("^(concatAndInsert ", " (List.map exprToString exprs))^")" |
		ARRAYEXPR (_,exprs) -> "[|"^(concatAndInsert "; " (List.map exprToString exprs))^"|]" |
		LISTCOMPREHENSIONEXPR (_,expr,pattern,set) -> "["^(exprToString expr)^" | "^(patternToString pattern)^" in "^(exprToString set) |
		OBJEXPR (_,obj) -> objectToString obj |
		TRANSITIONEXPR (_,transition) -> transitionToString transition
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
		ALTERNATIVESTYPE lst -> (concatAndInsert " | " (List.map typeToString lst)) |
		OBJECTTYPE -> "object" |
		TRANSITIONTYPE -> "transition"  
and actionToString act = 
	match act with
		ASSIGNACTION (id,expr) -> id^"<- "^(exprToString expr) |
		DOACTION expr -> "do "^(exprToString expr) |
		COPYACTION id -> "copy "^id |
		EXPRACTION expr -> (exprToString expr) |
		NEWACTION (id,expr) -> "new "^id^" = "^(exprToString expr) |
		DELETEACTION id -> "delete "^id |
		REPLACEACTION (id,expr) -> "replace "^id^" with "^(exprToString expr) |
		DEFINETYPEACTION (id,params,typeDef) -> "type "^id^(concatAndInsert " " params)^" = "^(typeToString typeDef) |
		DEFINEACTION (id,params,expr) -> "define "^id^(concatAndInsert " " (List.map patternToString params))^" = "^(exprToString expr) |
		EXTERNACTION (id,typeDef) -> "extern "^id^": "^(typeToString typeDef)
and objectToString obj = 
	match obj with
		OBJECT (syncMode,attributes) -> "{"^(syncModeToString  syncMode)^(concatAndInsert "; " (List.map (fun (att,expr) -> att^"= "^(exprToString expr)) attributes))
and syncModeToString mode = 
	match mode with
		LOCAL -> "" |
		INTERFACE st -> "|" ^st^"|"
and transitionToString transition = 
	match transition with
		EXPRTRANS (patterns,expr) -> (concatAndInsert " " (List.map (fun objPattern -> "{"^(concatAndInsert "; " (List.map objectPatternToString objPattern))^"}") patterns))^" => "^(exprToString expr) |
		ACTIONTRANS (patterns,expr) -> (concatAndInsert " " (List.map (fun objPattern -> "{"^(concatAndInsert "; " (List.map objectPatternToString objPattern))^"}") patterns))^" !-> "^(exprToString expr)
and objectPatternToString pattern =
	match pattern with
		OPENOBJPATTERN -> "..." |
		OBJPATTERN (id,pattern) -> id^"= "^(patternToString pattern)
;; 
	 
exception ExpressionIsNotActionList of string;;
let exprToActions expr = 
	match expr with
		ACTIONEXPR (_,actions) -> actions |
		_ -> raise (ExpressionIsNotActionList (exprToString expr))
;;

exception ExpressionIsNotList of string;;
let exprToList expr = 
	match expr with 
		LISTEXPR (_,lst) -> lst |
		_ -> raise (ExpressionIsNotList (exprToString expr))
;;

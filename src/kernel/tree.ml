
open StringTools
open CartesianDataModel

let rec exprEqual expr1 expr2 = 
	match (expr1,expr2) with
		(NODEXPR _,NODEXPR _) -> true |
		(INTEXPR (_,i1),INTEXPR (_,i2)) -> i1 == i2 |
		(FLOATEXPR (_,f1),FLOATEXPR (_,f2)) -> f1 == f2 |
		(STRINGEXPR (_,s1),STRINGEXPR (_,s2)) -> (String.compare s1 s2) == 0 |
		(BOOLEXPR (_,b1),BOOLEXPR (_,b2)) -> b1 == b2 |
		(LISTEXPR (_,l1),LISTEXPR (_,l2)) -> not (List.exists2 (fun x y -> not (exprEqual x y)) l1 l2) |
		(ARRAYEXPR (_,l1),ARRAYEXPR (_,l2)) -> not (List.exists2 (fun x y -> not (exprEqual x y)) l1 l2) |
		(PAIREXPR (_,l1),PAIREXPR (_,l2)) -> not (List.exists2 (fun x y -> not (exprEqual x y)) l1 l2) |
		_ -> false
;;

let rec exprToString expr = 
	match expr with
		VARIANTEXPR (_,id,expr) -> id^" "^(exprToString expr) |
		FUNCTIONCALLEXPR (_,fn,param) -> "("^(exprToString fn)^" "^(exprToString param)^")" |
		LAMBDAEXPR (_,pattern,expr) -> "lambda "^(patternToString pattern)^" -> "^(exprToString expr) |
		LETEXPR (_,assigns,expr) -> "let "^(concatAndInsert " and " 
			(List.map (fun (param,expr) -> (patternToString param)^" = "^(exprToString expr)) assigns))^" in "^(exprToString expr) |
		TYPEACCESSEXPR (_,expr,tp) -> (exprToString expr)^".:"^(typeToString tp) |
		TYPEVERIFICATIONEXPR (_,expr,tp) -> "("^(exprToString expr)^": "^(typeToString tp)^")" |
		INTEXPR (_,i) -> (Printf.sprintf "%d" i) |
		FLOATEXPR (_,f) -> (Printf.sprintf "%f" f) |
		STRINGEXPR (_,s) -> (Printf.sprintf "\"%s\"" s) |
		BOOLEXPR (_,b) -> if b then "true" else "false" |
		IDEXPR (_,id) -> id |
		ACTIONEXPR (_,a) -> (concatAndInsert "; " (List.map actionToString a)) |
		LISTEXPR (_,lst) -> "["^(concatAndInsert "; " (List.map exprToString lst))^"]" |
		NODEXPR _ -> "nod" |
		PAIREXPR (_,exprs) -> "("^(concatAndInsert ", " (List.map exprToString exprs))^")" |
		ARRAYEXPR (_,exprs) -> "[|"^(concatAndInsert "; " (List.map exprToString exprs))^"|]" |
		NARROWTYPEEXPR (_,expr,typeExpr) -> "("^(exprToString expr)^" :> "^(typeToString typeExpr)^")" | 
		GENERALISETYPEEXPR (_,expr,typeExpr) -> "("^(exprToString expr)^" :< "^(typeToString typeExpr)^")" |
		ERROREXPR _ -> "error" |
		FUNCTIONEXPR (_,lambdas) -> (concatAndInsert " | " (List.map exprToString lambdas))
and patternToString pattern = 
	match pattern with
		VARIANTPATTERN (_,id,pattern) -> id^" "^(patternToString pattern) |
		INTPATTERN (_,i) -> (Printf.sprintf "%d" i) |
		CONSPATTERN (_,car,cdr) -> (patternToString car)^"::"^(patternToString cdr) |
		FLOATPATTERN (_,f) -> (Printf.sprintf "%f" f) |
		STRINGPATTERN (_,s) -> (Printf.sprintf "\"%s\"" s) |
		BOOLPATTERN (_,b) -> if b then "true" else "false" |
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
		NODTYPE _ -> "nod" |
		INTTYPE _ -> "int" |
		FLOATTYPE _ -> "float" |
		STRINGTYPE _ -> "string" |
		ARRAYTYPE (_,t) -> (typeToString t)^" array" |
		LISTTYPE (_,t) -> (typeToString t)^" list" |
		GENTYPE (_,st) -> "'"^st |
		PAIRTYPE (_,lst) -> "("^(concatAndInsert " " (List.map typeToString lst))^")" |
		NAMEDTYPE (_,st,[]) -> st |
		NAMEDTYPE (_,st,types) -> "("^st^" "^(List.fold_left (fun currentState tp -> currentState^" "^(typeToString tp)) "" types)^")" |
		VARIANTTYPE (_,lst) -> (concatAndInsert " | " (List.map (fun (variant,tp) -> variant^" "^(typeToString tp)) lst)) |
		FUNCTIONTYPE (_,param,result) -> (typeToString param)^" -> "^(typeToString result) 
and actionToString act = 
	match act with
		ASSIGNACTION (id,expr) -> id^"<- "^(exprToString expr) |
		EXPRACTION expr -> (exprToString expr) |
		DEFINETYPEACTION (id,typeParams,tp) -> "define type"^(List.fold_left (fun current id -> current^" "^id) "" typeParams)^" "^id^" = "^(typeToString tp) | 
		DEFINEACTION (_,id,expr) -> "define "^id^" = "^(exprToString expr) |
		DEFINEEXTERNALACTION (_,id,tp) -> "define external "^id^" : "^(typeToString tp) |
		USEACTION (_,str) -> "use "^str
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

exception ExpressionIsNotArray of string;;
let exprToArrayAsList expr = 
	match expr with 
		ARRAYEXPR (_,ar) -> ar |
		_ -> raise (ExpressionIsNotArray (exprToString expr))
;;

exception ExpressionIsNotLambda of string;;
let exprToLambda expr =
	match expr with
		LAMBDAEXPR (_,lambdaPattern,lambdaExpr) -> (lambdaPattern,lambdaExpr) |
		_ -> raise (ExpressionIsNotLambda (exprToString expr))
;; 

exception ExpressionIsNotInt of string;;
let exprToInt expr =
	match expr with
		INTEXPR (_,i) -> i |
		_ -> raise (ExpressionIsNotInt (exprToString expr))
;;

exception ExpressionIsNotPair of string;;
let exprToPairsAsList expr = 
	match expr with
		PAIREXPR (_,lst) -> lst | 
		_ -> raise (ExpressionIsNotPair (exprToString expr))
;;

exception ExpressionIsNotFloat of string;;
let exprToFloat expr = 
	match expr with
		FLOATEXPR (_,f) -> f |
		_ -> raise (ExpressionIsNotFloat (exprToString expr))
;;

exception ExpressionIsNotBool of string;;
let exprToBool expr = 
	match expr with
		BOOLEXPR (_,b) -> b |
		_ -> raise (ExpressionIsNotBool (exprToString expr))
;;

exception ExpressionIsNotCorrespondingVariant of string;;
let variantToExpr id0 expr = 
	match expr with
		VARIANTEXPR (_,id,expr) when String.compare id0 id == 0 -> expr |
		_ -> raise (ExpressionIsNotCorrespondingVariant (exprToString expr))
;; 

let exprToId expr = 
	match expr with
		FUNCTIONCALLEXPR (nd,_,_) -> nd |
		LAMBDAEXPR (nd,_,_) -> nd |
		FUNCTIONEXPR (nd,_) -> nd |
		LETEXPR (nd,_,_) -> nd |
		NARROWTYPEEXPR (nd,_,_) -> nd |
		GENERALISETYPEEXPR (nd,_,_) -> nd |
		TYPEACCESSEXPR (nd,_,_) -> nd |
		TYPEVERIFICATIONEXPR (nd,_,_) -> nd |
		INTEXPR (nd,_) -> nd |
		FLOATEXPR (nd,_) -> nd |
		STRINGEXPR (nd,_) -> nd |
		BOOLEXPR (nd,_) -> nd |
		IDEXPR (nd,_) -> nd |
		ACTIONEXPR (nd,_) -> nd |
		LISTEXPR (nd,_) -> nd |
		NODEXPR nd -> nd | 
		ERROREXPR nd -> nd |
		PAIREXPR (nd,_) -> nd |
		ARRAYEXPR (nd,_) -> nd |
		VARIANTEXPR (nd,_,_) -> nd
;;

let typeToId tp = 
	match tp with
		NODTYPE nd -> nd |
		INTTYPE nd -> nd |
		FLOATTYPE nd -> nd |
		STRINGTYPE nd -> nd |
		ARRAYTYPE (nd,_) -> nd |
		LISTTYPE (nd,_) -> nd |
		GENTYPE (nd,_) -> nd |
		PAIRTYPE (nd,_) -> nd |
		NAMEDTYPE (nd,_,_) -> nd |
		VARIANTTYPE (nd,_) -> nd |
		FUNCTIONTYPE (nd,_,_) -> nd
;;

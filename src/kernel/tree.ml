
open StringTools
open CartesianDataModel

let rec exprEqual expr1 expr2 = 
	match (expr1,expr2) with
		(NODEXPR,NODEXPR) -> true |
		(INTEXPR i1,INTEXPR i2) -> i1 == i2 |
		(FLOATEXPR f1,FLOATEXPR f2) -> f1 == f2 |
		(STRINGEXPR s1,STRINGEXPR s2) -> (String.compare s1 s2) == 0 |
		(BOOLEXPR b1,BOOLEXPR b2) -> b1 == b2 |
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
		MATCHEXPR (_,expr,alternatives) -> "match "^(exprToString expr)^" with "^(concatAndInsert " | " (List.map exprToString alternatives)) |
		MATCHINLISTEXPR (_,expr,alternatives) -> "match in list "^(exprToString expr)^" with "^(concatAndInsert " | " (List.map exprToString alternatives)) |
		MATCHINARRAYEXPR (_,expr,alternatives) -> "match in array "^(exprToString expr)^" with "^(concatAndInsert " | " (List.map exprToString alternatives)) |
		TYPEACCESSEXPR (_,expr,tp) -> (exprToString expr)^".:"^(typeToString tp) |
		TYPEVERIFICATIONEXPR (_,expr,tp) -> "("^(exprToString expr)^": "^(typeToString tp)^")" |
		INTEXPR i -> (Printf.sprintf "%d" i) |
		FLOATEXPR f -> (Printf.sprintf "%f" f) |
		STRINGEXPR s -> (Printf.sprintf "\"%s\"" s) |
		BOOLEXPR b -> if b then "true" else "false" |
		IDEXPR (_,id) -> id |
		ACTIONEXPR (_,a) -> (concatAndInsert "; " (List.map actionToString a)) |
		LISTEXPR (_,lst) -> "["^(concatAndInsert "; " (List.map exprToString lst))^"]" |
		INTERVALEXPR (start,ending) -> "["^(exprToString start)^" .. "^(exprToString ending)^"]" |
		INTERVALSTEPEXPR (value1,value2,lastValue) -> "["^(exprToString value1)^"; "^(exprToString value2)^" .. "^(exprToString lastValue)^"]" |
		NODEXPR -> "nod" |
		PAIREXPR (_,exprs) -> "("^(concatAndInsert ", " (List.map exprToString exprs))^")" |
		ARRAYEXPR (_,exprs) -> "[|"^(concatAndInsert "; " (List.map exprToString exprs))^"|]" |
		LISTCOMPREHENSIONEXPR (_,expr,pattern,set) -> "["^(exprToString expr)^" | "^(patternToString pattern)^" in "^(exprToString set) |
		OBJEXPR (obj) -> objectToString obj |
		TRANSITIONEXPR (transition) -> transitionToString transition |
		NARROWTYPEEXPR (_,expr,typeExpr) -> "("^(exprToString expr)^" :> "^(typeToString typeExpr)^")" | 
		GENERALISETYPEEXPR (_,expr,typeExpr) -> "("^(exprToString expr)^" :< "^(typeToString typeExpr)^")" |
		PROMISEEXPR (expr,_) -> exprToString !expr |
		NATIVEEXPR expr -> "native" |
		INSTANCIATEDLAMBDAEXPR (_,_,pattern,expr) -> "internalLambda "^(patternToString pattern)^" -> "^(exprToString expr) |
		TBDEXPR -> "not defined"
and patternToString pattern = 
	match pattern with
		VARIANTPATTERN (_,id,pattern) -> id^" "^(patternToString pattern) |
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
		NAMEDTYPE (st,[]) -> st |
		NAMEDTYPE (st,types) -> "("^st^" "^(List.fold_left (fun currentState tp -> currentState^" "^(typeToString tp)) "" types)^")" |
		OBJECTTYPE -> "object" |
		TRANSITIONTYPE -> "transition" |
		VARIANTTYPE lst -> (concatAndInsert " | " (List.map (fun (variant,tp) -> variant^" "^(typeToString tp)) lst)) |
		FUNCTIONTYPE (param,result) -> (typeToString param)^" -> "^(typeToString result) 
and actionToString act = 
	match act with
		IMMEDIATEACTION expr -> "NOW "^(exprToString expr) |
		ASSIGNACTION (id,expr) -> id^"<- "^(exprToString expr) |
		DOACTION expr -> "do "^(exprToString expr) |
		EXPRACTION expr -> (exprToString expr) |
		ASSIGNRULEACTION (id,expr) -> "rule "^id^" <- "^(exprToString expr) |
		ASSIGNOBJECTACTION (id,expr) -> "object "^id^" <- "^(exprToString expr) | 
		DELETERULEACTION id -> "delete rule "^id |
		DELETEOBJECTACTION id -> "delete object "^id |
		DEFINETYPEACTION (id,typeParams,tp) -> "define type"^(List.fold_left (fun current id -> current^" "^id) "" typeParams)^" "^id^" = "^(typeToString tp) | 
		DEFINEACTION (_,id,expr) -> "define "^id^" = "^(exprToString expr) | 
		DEFINEEXTERNALACTION (_,id,tp) -> "define external "^id^" : "^(typeToString tp) | 
		DEFINEOBJECTACTION (id,expr) -> "define object "^id^" = "^(exprToString expr) | 
		DEFINERULEACTION (id,expr) -> "define rule "^id^" = "^(exprToString expr) | 
		DEFINEINTERFACE (id,driver,ins,outs) -> "define interface "^id^" = {< "^driver^" | "^(List.fold_left (fun str inElement -> str^" << "^inElement) "" ins)^" "^(List.fold_left (fun str outElement -> str^" >> "^outElement) "" outs) 
and objectToString obj = 
	match obj with
		SIMPLEOBJECT attributes -> 
			"{|"^(concatAndInsert "; " (List.map (fun (att,expr) -> att^"= "^(exprToString expr)) attributes))^"|}" |
		TRANSIENTSIMPLEOBJECT attributes -> 
			"{~"^(concatAndInsert "; " (List.map (fun (att,expr) -> att^"= "^(exprToString expr)) attributes))^"~}" |
		OBJECT (driverId,attributes) -> 
			"{|"^driverId^" <> "^(concatAndInsert "; " (List.map (fun (att,expr,interface) -> att^"= "^(exprToString expr)^" "^(attributeInterfacingToString interface)) attributes))^"|}" | 
		TRANSIENTOBJECT (driverId,attributes) -> 
			"{~"^driverId^" <> "^(concatAndInsert "; " (List.map (fun (att,expr,interface) -> att^"= "^(exprToString expr)^" "^(attributeInterfacingToString interface)) attributes))^"~}"
and attributeInterfacingToString description = 
	match description with
		SEND id -> ">> "^id |
		RECEIVE id -> "<< "^id |
		NOINTERFACE -> "" 
and transitionToString transition = 
	match transition with
		EXPRTRANS (objPatterns,expr) -> (concatAndInsert " " (List.map (fun x -> "{"^(concatAndInsert "; " (List.map attributePatternToString x))^"}") objPatterns))^" => "^(exprToString expr) |
		ACTIONTRANS (objPatterns,expr) -> (concatAndInsert " " (List.map (fun x -> "{"^(concatAndInsert "; " (List.map attributePatternToString x))^"}") objPatterns))^" !-> "^(exprToString expr)
and attributePatternToString att = 
	match att with
		VALUEATTRIBUTEPATTERN (id,pattern) -> id^"= "^(patternToString pattern) |
		PRESENTATTRIBUTEPATTERN id -> id |
		TYPEATTRIBUTEPATTERN (id,tp) -> id^": "^(typeToString tp)
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
		INTEXPR i -> i |
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
		FLOATEXPR f -> f |
		_ -> raise (ExpressionIsNotFloat (exprToString expr))
;;

exception ExpressionIsNotBool of string;;
let exprToBool expr = 
	match expr with
		BOOLEXPR b -> b |
		_ -> raise (ExpressionIsNotBool (exprToString expr))
;;

exception ExpressionIsNotCorrespondingVariant of string;;
let variantToExpr id0 expr = 
	match expr with
		VARIANTEXPR (_,id,expr) when String.compare id0 id == 0 -> expr |
		_ -> raise (ExpressionIsNotCorrespondingVariant (exprToString expr))
;; 

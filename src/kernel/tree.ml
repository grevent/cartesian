
open StringTools
open CartesianDataModel

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
		GENERALISETYPEEXPR (_,expr,typeExpr) -> "("^(exprToString expr)^" :< "^(typeToString typeExpr)^")"  
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
		NAMEDTYPE st -> st |
		OBJECTTYPE -> "object" |
		TRANSITIONTYPE -> "transition" |
		VARIANTTYPE lst -> (concatAndInsert " | " (List.map (fun (variant,tp) -> variant^" "^(typeToString tp)) lst)) |
		FUNCTIONTYPE (param,result) -> (typeToString param)^" -> "^(typeToString result) |
		INCHANNELTYPE -> "inchannel" |
		OUTCHANNELTYPE -> "outchannel" 		
and actionToString act = 
	match act with
		ASSIGNACTION (id,expr) -> id^"<- "^(exprToString expr) |
		DOACTION expr -> "do "^(exprToString expr) |
		EXPRACTION expr -> (exprToString expr) |
		ASSIGNRULEACTION (id,expr) -> "rule "^id^" <- "^(exprToString expr) |
		ASSIGNOBJECTACTION (id,expr) -> "object "^id^" <- "^(exprToString expr) | 
		DELETERULEACTION id -> "delete rule "^id |
		DELETEOBJECTACTION id -> "delete object "^id |
		DEFINETYPEACTION (id,tp) -> "define type "^id^" = "^(typeToString tp) | 
		DEFINEACTION (id,params,expr) -> "define "^id^(List.fold_left (fun acc param -> acc^" "^(patternToString param)) "" params)^" = "^(exprToString expr) | 
		DEFINEEXTERNALACTION (id,tp) -> "define external "^id^" : "^(typeToString tp) | 
		DEFINEOBJECTACTION (id,expr) -> "define object "^id^" = "^(exprToString expr) | 
		DEFINERULEACTION (id,expr) -> "define rule "^id^" = "^(exprToString expr) | 
		OUTACTION (channel,tp) -> (exprToString channel)^" >> "^(typeToString tp) | 
		INACTION (channel,tp) -> (exprToString channel)^" << "^(typeToString tp)
and objectToString obj = 
	match obj with
		OBJECT (syncMode,attributes) -> "{"^(syncModeToString  syncMode)^(concatAndInsert "; " (List.map (fun (att,expr) -> att^"= "^(exprToString expr)) attributes))
and syncModeToString mode = 
	match mode with
		LOCAL -> "" |
		INTERFACE st -> "|" ^st^"|"
and transitionToString transition = 
	match transition with
		EXPRTRANS (objPatterns,expr) -> (concatAndInsert " " (List.map objectPatternToString objPatterns))^" => "^(exprToString expr) |
		ACTIONTRANS (objPatterns,expr) -> (concatAndInsert " " (List.map objectPatternToString objPatterns))^" !-> "^(exprToString expr)
and objectPatternToString obj =
	match obj with
		OBJPATTERN attributes -> "{"^(concatAndInsert "; " (List.map attributePatternToString attributes))^"}" 
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


open CartesianTree
open BasicTools

let fromString str = 
	let buffer = Lexing.from_string str in
	Syntax.phrase Lex.lexer buffer
;;

let rec generateString x = match x with
	  ACTIONWRAPPER x -> (generateString x)
	| SEQUENCEACTION x -> "{ "^(listIterator2String generateString "; " x)^"} "
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
	| RENAMINGPATTERN (pattern,newId) -> (generateString pattern)^" as "^"newId"
	| RAISEACTION expr -> "raise "^(generateString expr)
	| QUOTEDIDEXPRESSION id -> "'"^id 
	| OBJECTWRAPPEREXPRESSION obj -> (generateString obj)
	| OBJECTPROTOTYPE (uc,attributes) -> (listIterator2String (fun (attribute,expr) -> attribute^"= "^(generateString expr)) "; " attributes)
	| OBJECTPATTERN attributes -> (listIterator2String (fun (attribute,expr) -> attribute^"= "^(generateString expr)) "; " attributes)
	| PROTOTYPESEXPRESSION (value,prototypes) -> 
	| 
;;

(* let rec generateString = function


  | PROTOTYPESEXPRESSION of (cartesianTree*(cartesianTree list))
  | OBJECT of ((string*cartesianTree) list)
  | OBJECTEXPRESSION of ((string*(cartesianTree list)*cartesianTree) list)
  | NATIVEFUNCTION 
  | BOOLEXPRESSION of bool
  | NUMEXPRESSION of (float*float)
  | MATRIXEXPRESSION of (cartesianTree array array)
  | MATRIXWRAPPER of ((float*float) array array)
  | LISTEXPRESSION of (cartesianTree list)
  | NODEXPRESSION
  | STRINGEXPRESSION of string
  | IDEXPRESSION of string
  | NATIVEACTION of string
  | ASSIGNACTION of (string*cartesianTree)
  | CONTEXTACTION of (cartesianTree*cartesianTree)
  | ATTRIBUTEACCESSEXPRESSION of (cartesianTree*string)
  | BOOLPATTERN of bool
  | BOOLPROTOTYPE of (string*bool)
  | CONSPATTERN of (cartesianTree*cartesianTree)
  | FUNCTIONEXPRESSION of (((cartesianTree list)*cartesianTree) list)
  | DEFINEACTION of (string*(cartesianTree list)*cartesianTree)
  | DOACTION of cartesianTree*cartesianTree
  | EXPRACTION of cartesianTree
  | NUMPATTERN of (float*float)
  | MATRIXPATTERN of (cartesianTree array array)
  | NUMPROTOTYPE of (string*(float*float))
  | MATRIXPROTOTYPE of (string*((float*float) array array))
  | FORACTION of (string*cartesianTree*cartesianTree)
  | FUNCTIONCALLEXPRESSION of (cartesianTree*(cartesianTree list))
  | LISTPATTERN of (cartesianTree list)
  | IDPATTERN of string
  | INSTANCESEXPRESSION of (cartesianTree list)
  | LETEXPRESSION of ((cartesianTree*cartesianTree) list)*cartesianTree
  | LISTPROTOTYPE of (string*(cartesianTree list))
  | MATCHEXPRESSION of (cartesianTree*(((cartesianTree list)*cartesianTree) list))
  | COMMENT of (string*cartesianTree)
  | WHEREPATTERN of (cartesianTree*cartesianTree)
;;
	


;;
*)

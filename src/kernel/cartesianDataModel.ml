
type exprNode = 
	FUNCTIONCALLEXPR of int*exprNode*(exprNode list) |
	LAMBDAEXPR of int*(((patternNode list)*exprNode) list) |
	LETEXPR of int*((patternNode*(patternNode list)*exprNode) list)*exprNode |	
	MATCHEXPR of int*exprNode*(((patternNode list)*exprNode) list) |
	MATCHPOSSIBLEEXPR of int*exprNode*(((patternNode list)*exprNode) list) |
	NARROWTYPEEXPR of int*exprNode*typeNode |
	GENERALISETYPEEXPR of int*exprNode*typeNode | 
	TYPEACCESSEXPR of int*exprNode*typeNode |
	TYPEVERIFICATIONEXPR of int*exprNode*typeNode |
	TOSUBTYPEEXPR of int*exprNode*typeNode |
	INTEXPR of int |
	FLOATEXPR of float |
	STRINGEXPR of string |
	BOOLEXPR of bool |
	IDEXPR of int*string | 
	ACTIONEXPR of int*(actionNode list) |
	LISTEXPR of int*(exprNode list) |
	INTERVALEXPR of exprNode*exprNode | 
	INTERVALSTEPEXPR of exprNode*exprNode*exprNode |
	NODEXPR |
	PAIREXPR of int*(exprNode list) |
	LISTCOMPREHENSIONEXPR of int*exprNode*patternNode*exprNode |
	ARRAYEXPR of int*(exprNode list) |
	OBJEXPR of objectNode |
	TRANSITIONEXPR of transitionNode
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
	NAMEDTYPE of string |
	VARIANTTYPE of ((string*typeNode) list) |
	FUNCTIONTYPE of (typeNode*typeNode) |
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

type cType = 
	UNKNOWN |
	INT |
	FLOAT |
	STRING |
	BOOL |
	ACTION |
	GENTYPE of int |
	NOD |
	LIST of cType |
	ARRAY of cType |
	PAIR of cType list |
	NAMED of string | 
	VARIANT of (string*cType) list |
	FUNCTION of cType*cType |
	OBJECT |
	TRANSITION
;;

type decoration = { node: int; typeDes: cType };;
type rule = { rule: objectPatternNode; exec: exprNode };;
type objectEntry = { name: string; mutable value: exprNode; locked: Mutex.t}
type cObject = { id: int; attributes: objectEntry list; mutable changed: bool };;
type value = { id: string; nodeId: int; cType: cType; value: exprNode };;
type parentScope = 
	ROOT |
	PARENT of env
and
	env = { scope: parentScope; values: value list }
;;

type runtime = { 
	mutable objects: cObject list; 
	mutable rules: rule list; 
	mutable env0: env; 
	mutable decorations: decoration list;
	mutable genericTypes: (int*cType) list;
	mutable namedTypes: (string*cType) list
};;

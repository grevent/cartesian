
open ListReference

type cType = 
	T_INT |
	T_FLOAT |
	T_STRING |
	T_BOOL |
	T_ACTION |
	T_GENERIC of int |
	T_NOD |
	T_LIST of cType |
	T_ARRAY of cType |
	T_PAIR of cType list |
	T_NAMED of (string*(cType list)) | 
	T_VARIANT of (string*cType) list |
	T_FUNCTION of cType*cType |
	T_OBJECT |
	T_TRANSITION
;;

type intermediateValue = 
	IV_NOD |
	IV_ERROR |
	IV_INT of int |
	IV_FLOAT of float |
	IV_STRING of string |
	IV_REF of int |
	IV_ARRAY of (intermediateValue array) |
	IV_LIST of (intermediateValue list) |
	IV_VARIANT of string*intermediateValue |
	IV_PAIR of (intermediateValue list) |
	IV_OBJECT of (string*intermediateValue) list | 
	IV_LAMBDA of (int*intermediateLang) |
	IV_EXTERNAL of (intermediateValue -> intermediateValue) |
	IV_INTERNAL of internalFunction |
	IV_ACTIONS of (intermediateAction list)
and internalFunction = 
	IF_ALTERNATIVEONERROR
and intermediateAction =
	IA_DEFINE of (int*intermediateLang) |
	IA_SET of (int*intermediateLang)
and intermediateLang =
	IL_VALUE of intermediateValue |
	IL_CALL of (intermediateLang*intermediateLang) | 
	IL_LET of (int*intermediateValue*intermediateValue)
;;

type exprNode = 
	FUNCTIONCALLEXPR of int*exprNode*exprNode |
	LAMBDAEXPR of int*patternNode*exprNode |
	FUNCTIONEXPR of int*(exprNode list) |
	LETEXPR of int*((patternNode*exprNode) list)*exprNode |	
	NARROWTYPEEXPR of int*exprNode*typeNode |
	GENERALISETYPEEXPR of int*exprNode*typeNode | 
	TYPEACCESSEXPR of int*exprNode*typeNode |
	TYPEVERIFICATIONEXPR of int*exprNode*typeNode |
	INTEXPR of int*int |
	FLOATEXPR of int*float |
	STRINGEXPR of int*string |
	BOOLEXPR of int*bool |
	IDEXPR of int*string | 
	ACTIONEXPR of int*(actionNode list) |
	LISTEXPR of int*(exprNode list) |
	NODEXPR of int |
	ERROREXPR of int |
	PAIREXPR of int*(exprNode list) |
	ARRAYEXPR of int*(exprNode list) |
	OBJEXPR of int*objectNode |
	TRANSITIONEXPR of int*transitionNode |
	NATIVEEXPR of (int*cType*intermediateLang) |
	VARIANTEXPR of (int*string*exprNode)
and patternNode = 
	CONSPATTERN of int*patternNode*patternNode |
	INTPATTERN of int*int |
	FLOATPATTERN of int*float |
	STRINGPATTERN of int*string |
	BOOLPATTERN of int*bool | 
	LISTPATTERN of int*(patternNode list) | 
	RENAMINGPATTERN of int*patternNode*string |
	WILDCARDPATTERN  of int |
	PAIRPATTERN of int*(patternNode list) | 
	IDPATTERN of int*string |
	WHEREPATTERN of int*patternNode*exprNode |
	ARRAYPATTERN of int*(patternNode list) |
	TYPEDPATTERN of int*patternNode*typeNode |
	VARIANTPATTERN of int*string*patternNode
and typeNode = 
	NODTYPE |
	INTTYPE |
	FLOATTYPE |
	STRINGTYPE |
	ARRAYTYPE of typeNode |
	LISTTYPE of typeNode |
	GENTYPE of string |
	PAIRTYPE of (typeNode list) |
	NAMEDTYPE of string*(typeNode list) |
	VARIANTTYPE of ((string*typeNode) list) |
	FUNCTIONTYPE of (typeNode*typeNode) |
	OBJECTTYPE |
	TRANSITIONTYPE 
and actionNode =
	ASSIGNACTION of string*exprNode |
	ASSIGNRULEACTION of string*exprNode |
	ASSIGNOBJECTACTION of string*exprNode |
	EXPRACTION of exprNode |
	DELETERULEACTION of string  |
	DELETEOBJECTACTION of string |
	DEFINETYPEACTION of string*(string list)*typeNode |
	DEFINEACTION of int*string*exprNode |
	DEFINEEXTERNALACTION of int*string*typeNode |
	DEFINEOBJECTACTION of string*exprNode |
	DEFINERULEACTION of string*exprNode |
	IMMEDIATEACTION of exprNode
and objectNode = 
	OBJECT of ((string*exprNode) list) | 
	TRANSIENTOBJECT of ((string*exprNode) list)
and transitionNode =
	EXPRTRANS of (attributePatternNode list list)*(attributePatternNode list) |
	ACTIONTRANS of ((attributePatternNode list list)*exprNode) 
and attributePatternNode = 
	VALUEATTRIBUTEPATTERN of string*patternNode |
	PRESENTATTRIBUTEPATTERN of string |
	TYPEATTRIBUTEPATTERN of string*typeNode
;;

type objectEntry = 
	{ name: string; mutable objValue: exprNode; locked: Mutex.t }
and value = 
	{ id: string; nodeId: int; mutable tp: cType }
and parentScope = 
	ROOT |
	PARENT of scopeDescription
and scopeDescription = 
	{ scope: parentScope; mutable values: value list }
and env = { 
	scopes: scopeDescription; 
	decorations: (int*cType) listReference;
	genericTypes: (int*cType) listReference;
	namedTypes: (string*cType*(cType list)) listReference;
};;

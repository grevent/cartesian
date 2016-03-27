
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
	T_FUNCTION of cType*cType
;;

type intermediateValue = 
	IV_NOD |
	IV_ERROR |
	IV_INT of int |
	IV_FLOAT of float |
	IV_STRING of string |
	IV_BOOL of bool |
	IV_ARRAY of (intermediateValue array) |
	IV_LIST of (intermediateValue list) |
	IV_TAGGED of string*intermediateValue |
	IV_PAIR of (intermediateValue list) |
	IV_LAMBDA of (int*intermediateExpr) |
	IV_INTERNAL of internalFunction |
	IV_ACTIONS of (intermediateAction list) |
	IV_REF of int
and internalFunction = 
	IF_ALTERNATIVEONERROR |
	IF_IF |
	IF_COMPAREINT |
	IF_COMPAREBOOL |
	IF_COMPAREFLOAT |
	IF_COMPARESTRING |
	IF_CAR |
	IF_CDR |
	IF_COMPARE |
	IF_STOPONERROR |
	IF_PAIRACCESS |
	IF_ISEMPTYLIST |
	IF_GETVARIANTVAL |
	IF_GETVARIANTTAG |
	IF_GETPAIRELEMENT |
	IF_GETARRAYELEMENT
and intermediateAction =
	IA_DEFINE of string |
	IA_SET of (string*intermediateActionAdapter) |
	IA_COMPUTED of (intermediateExpr) |
	IA_DEFINEEXTERNAL of string 
and intermediateActionAdapter =
	IAD_INJECT of (int*intermediateActionAdapter) |
	IAD_EXPR of intermediateExpr
and intermediateExpr =
	IL_VALUE of intermediateValue |
	IL_CALL of (intermediateExpr*intermediateExpr) | 
	IL_LET of (int*intermediateExpr*intermediateExpr)
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
	NODTYPE of int |
	INTTYPE of int |
	FLOATTYPE of int |
	STRINGTYPE of int |
	ARRAYTYPE of int*typeNode |
	LISTTYPE of int*typeNode |
	GENTYPE of int*string |
	PAIRTYPE of int*(typeNode list) |
	NAMEDTYPE of int*string*(typeNode list) |
	VARIANTTYPE of int*((string*typeNode) list) |
	FUNCTIONTYPE of int*typeNode*typeNode
and actionNode =
	ASSIGNACTION of string*exprNode |
	EXPRACTION of exprNode |
	DEFINETYPEACTION of string*(string list)*typeNode |
	DEFINEACTION of int*string*exprNode |
	DEFINEEXTERNALACTION of int*string*typeNode |
	USEACTION of int*string
;;

(* type objectEntry = 
	{ name: string; mutable objValue: exprNode; locked: Mutex.t }
and value = 
	{ id: string; nodeId: int; mutable tp: cType  }
*)
	
type value = 
	{ id: string; nodeId: int; mutable tp: cType  }
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
	idDefs: (string*(int*bool)) listReference; (* the int represents the id of the definition... *)
};;

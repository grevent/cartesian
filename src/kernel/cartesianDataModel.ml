
open ListReference

type cType = 
	INT |
	FLOAT |
	STRING |
	BOOL |
	ACTION |
	GENERIC of int |
	NOD |
	LIST of cType |
	ARRAY of cType |
	PAIR of cType list |
	NAMED of (string*(cType list)) | 
	VARIANT of (string*cType) list |
	FUNCTION of cType*cType |
	OBJECT |
	TRANSITION
;;

type exprNode = 
	FUNCTIONCALLEXPR of int*exprNode*exprNode |
	LAMBDAEXPR of int*patternNode*exprNode |
	LETEXPR of int*((patternNode*exprNode) list)*exprNode |	
	MATCHEXPR of int*exprNode*(exprNode list) |
	MATCHINLISTEXPR of int*exprNode*(exprNode list) |
	MATCHINARRAYEXPR of int*exprNode*(exprNode list) |
	NARROWTYPEEXPR of int*exprNode*typeNode |
	GENERALISETYPEEXPR of int*exprNode*typeNode | 
	TYPEACCESSEXPR of int*exprNode*typeNode |
	TYPEVERIFICATIONEXPR of int*exprNode*typeNode |
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
	TRANSITIONEXPR of transitionNode | 
	PROMISEEXPR of ((exprNode ref)*runtime) |
	NATIVEEXPR of (cType*(runtime -> exprNode -> exprNode)) |
	INSTANCIATEDLAMBDAEXPR of (int*runtime*patternNode*exprNode) | 
	TBDEXPR |
	VARIANTEXPR of (int*string*exprNode)
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
	DOACTION of exprNode |
	EXPRACTION of exprNode |
	DELETERULEACTION of string  |
	DELETEOBJECTACTION of string |
	DEFINETYPEACTION of string*(string list)*typeNode |
	DEFINEACTION of int*string*exprNode |
	DEFINEEXTERNALACTION of int*string*typeNode |
	DEFINEOBJECTACTION of string*exprNode |
	DEFINERULEACTION of string*exprNode |
	DEFINEINTERFACE of string*string*(string list)*(string list) |
	IMMEDIATEACTION of exprNode
and objectNode =
	SIMPLEOBJECT of ((string*exprNode) list) | 
	OBJECT of (string*((string*exprNode*attributeInterfacing) list)) |
	TRANSIENTSIMPLEOBJECT of ((string*exprNode) list) |
	TRANSIENTOBJECT of (string*((string*exprNode*attributeInterfacing) list)) 
and attributeInterfacing = 
	SEND of string |
	RECEIVE of string |
	NOINTERFACE
and transitionNode =
	EXPRTRANS of (attributePatternNode list list)*exprNode |
	ACTIONTRANS of ((attributePatternNode list list)*exprNode) 
and attributePatternNode = 
	VALUEATTRIBUTEPATTERN of string*patternNode |
	PRESENTATTRIBUTEPATTERN of string |
	TYPEATTRIBUTEPATTERN of string*typeNode
and rule = 
	{ rule: attributePatternNode list list; exec: exprNode }
and objectEntry = 
	{ name: string; mutable objValue: exprNode; locked: Mutex.t}
and cObject = 
	{ objId: int; attributes: objectEntry list; mutable changed: bool }
and value = 
	{ id: string; nodeId: int; mutable tp: cType; mutable value: exprNode }
and parentScope = 
	ROOT |
	PARENT of env
and env = 
	{ scope: parentScope; mutable values: value list }
and runtime = { 
	objects: cObject listReference; 
	rules: rule listReference; 
	env0: env; 
	decorations: (int*cType) listReference;
	genericTypes: (int*cType) listReference;
	namedTypes: (string*cType*(cType list)) listReference;
};;


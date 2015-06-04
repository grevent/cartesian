
type cartesianTree =
    ACTIONWRAPPER of cartesianTree
  | SEQUENCEACTION of (cartesianTree list)
  | WHILEACTION of cartesianTree*cartesianTree
  | ACTORSENDSACTION of (bool*((string*cartesianTree*cartesianTree) list))
  | REGISTERSTARTACTION of (string*cartesianTree)
  | ACTORRECEIVESACTION of string*cartesianTree
  | THREADACTION of cartesianTree
  | TRYACTION of (cartesianTree list)*(((cartesianTree list)*cartesianTree) list)
  | ACTIONEXPRESSION of cartesianTree
  | FUNCTION of (((cartesianTree list)*cartesianTree) list)
  | WILDCARDPATTERN 
  | STRINGPROTOTYPE of string*string
  | STRINGPATTERN of string
  | RENAMINGPATTERN of cartesianTree*string
  | RAISEACTION of cartesianTree
  | QUOTEDIDEXPRESSION of string
  | OBJECTWRAPPEREXPRESSION of cartesianTree
  | OBJECTPROTOTYPE of string*((string*cartesianTree) list)
  | OBJECTPATTERN of ((string*cartesianTree) list)
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

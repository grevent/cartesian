
open Types

type cartesianAction = 
    DEFINETYPEACTION of typeDescription*(string*(string list)*cartesianType)
  | SENDACTION of typeDescription*(string*(((cartesianPattern list)*cartesianExpression) list))
  | NEWOBJECTACTION of typeDescription*(string*cartesianExpression)
  | EXTENDEDOBJECTACTION of typeDescription*(string*cartesianExpression*cartesianExpression)
  | SEQUENCEACTION of typeDescription*(cartesianAction list)
  | ACTORRECEIVESACTION of typeDescription*(string*cartesianExpression)
  | USEEXTERNALDEFSACTION of typeDescription*string
  | ASSIGNACTION of typeDescription*(string*cartesianExpression)
  | DOACTION of typeDescription*(cartesianExpression)
  | DEFINEACTORACTION of typeDescription*(string*string*cartesianExpression) 
  | DEFINEACTION of typeDescription*(string*(cartesianPattern list)*cartesianExpression)
  | EXPRACTION of typeDescription*cartesianExpression
  | EXTERNALACTION of typeDescription*(string*cartesianType)
and cartesianPattern = 
  | WILDCARDPATTERN of typeDescription
  | STRINGPATTERN of typeDescription*string
  | RENAMINGPATTERN of typeDescription*(cartesianPattern*string)
  | OPENOBJECTPATTERN of typeDescription*((string*cartesianPattern) list)
  | CLOSEDOBJECTPATTERN of typeDescription*((string*cartesianPattern) list)
  | BOOLPATTERN of typeDescription*bool
  | CONSPATTERN of typeDescription*(cartesianPattern*cartesianPattern)
  | LISTPATTERN of typeDescription*(cartesianPattern list)
  | IDPATTERN of typeDescription*string
  | WHEREPATTERN of typeDescription*(cartesianPattern*cartesianExpression)
  | PAIRPATTERN of typeDescription*(cartesianPattern list)
  | ARRAYPATTERN of typeDescription*(cartesianPattern list)
  | TYPEDPATTERN of typeDescription*(cartesianPattern*cartesianType)
  | INTPATTERN of typeDescription*int
  | FLOATPATTERN of typeDescription*float
and cartesianExpression =
  | ACTIONEXPRESSION of typeDescription*cartesianAction
  | OBJECTEXPRESSION of typeDescription*((string*(cartesianPattern list)*cartesianExpression) list)
  | BOOLEXPRESSION of typeDescription*bool
  | FLOATEXPRESSION of typeDescription*float
  | INTEXPRESSION of typeDescription*int
  | LISTEXPRESSION of typeDescription*(cartesianExpression list)
  | NODEXPRESSION of typeDescription
  | STRINGEXPRESSION of typeDescription*string
  | IDEXPRESSION of typeDescription*string
  | ATTRIBUTEACCESSEXPRESSION of (typeDescription*(cartesianExpression*string))
  | FUNCTIONEXPRESSION of typeDescription*(((cartesianPattern list)*cartesianExpression) list)
  | FUNCTIONCALLEXPRESSION of typeDescription*(cartesianExpression*(cartesianExpression list))
  | LETEXPRESSION of typeDescription*(((cartesianPattern*(cartesianPattern list)*cartesianExpression) list)*cartesianExpression)
  | MATCHEXPRESSION of typeDescription*(cartesianHelper*cartesianExpression*(((cartesianPattern list)*cartesianExpression) list))  
  | TYPEACCESSEXPRESSION of typeDescription*(cartesianExpression*cartesianType)
  | FROMSUBTYPEEXPRESSION of typeDescription*(cartesianExpression*cartesianType)
  | TOSUBTYPEEXPRESSION of typeDescription*(cartesianExpression*cartesianType)
  | TYPEVERIFICATIONEXPRESSION of typeDescription*(cartesianExpression*cartesianType)
  | INTERVALEXPRESSION of typeDescription*(cartesianExpression*cartesianExpression)
  | INTERVALSTEPEXPRESSION of typeDescription*(cartesianExpression*cartesianExpression*cartesianExpression)
  | LISTCOMPREHENSIONEXPRESSION of typeDescription*(cartesianExpression*cartesianPattern*cartesianExpression)
  | ARRAYEXPRESSION of typeDescription*(cartesianExpression list)
  | THISEXPRESSION of typeDescription
  | PAIREXPRESSION of typeDescription*(cartesianExpression list)
and cartesianHelper = 
  | ONEELEMENT
  | ALLELEMENTS
  | EACHELEMENT
  | ALLPOSSIBLEELEMENTS
  | EACHPOSSIBLEELEMENTS
and cartesianType =
  | NODTYPE of typeDescription
  | INTTYPE of typeDescription
  | FLOATTYPE of typeDescription
  | LISTTYPE of typeDescription*cartesianType
  | ARRAYTYPE of typeDescription*cartesianType
  | STRINGTYPE of typeDescription
  | OPENOBJECTTYPE of typeDescription*((string*cartesianType) list)
  | CLOSEDOBJECTTYPE of typeDescription*((string*cartesianType) list)
  | GENTYPE of typeDescription*string
  | PAIRTYPE of typeDescription*(cartesianType list)
  | ALTERNATIVESTYPE of typeDescription*(cartesianType list)
  | NAMEDTYPE of typeDescription*(string*((cartesianType list)))
;;

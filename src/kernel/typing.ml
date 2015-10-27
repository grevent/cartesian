
open Env
open Types

exception NotSameLengthOfLists;;

let analyseList analyseFunction env list = 
	let analysedList = List.fold_left (fun acc element -> 
		let (oldEnv,previousAnalysedElements) = acc in
		let (nextEnv,analysedElement) = analyseFunction oldEnv element in
		(nextEnv,previousAnalysedElements@[ analysedElement ]) ) (env,[]) list
;;

let rec analyse2List analyseFunction env list1 list2 =
	match (list1,list2) with
		[],[] -> (env,[]) |
		(car1::cdr1,car2::cdr2) -> 
			let (newEnv,result) = analyseFunction env car1 car2 in
			let (resultEnv,lst) = analyse2List analyseFunction newEnv cdr1 cdr2 in
			(resultEnv,result::lst)
		_ -> 
			raise NotSameLengthOfLists
;;	

let analyseAction env tree =
	match tree with
		DEFINETYPEACTION (currentType,(typeId,typeParams,otherType)) ->
			let subTypes = List.map (fun x -> newGeneric()) typeParams in
			let typeExpression = newGeneric() in
			let mainType = PARAMETRIZEDTYPE (subTypes,typeExpression) in
			let env1 = addId env typeId mainType in
			let env2 = addScope env1 in
			let (env3,_) = analyseList2 (fun env id tp -> (addId env id tp),[]) typeParams subTypes in
			let (env4,evaluatedOtherType) = getTypeTree (analyseAction env3 otherType) in
			let (env5,tp) = unification env4 typeExpression evaluatedOtherType in
			let env6 = deleteScope env5 in
			let env7 = finalizeIdDef env6 typeId in
			(env7,DEFINETYPEACTIONACTION) |  
		SENDACTION (currentType,(actorId,matches) ->
			
			
			
			
			
			

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


exception NoCorrespondingPatternForExpr

let rec exprToPattern expr = 
  if (expr#isBool()) then
    (new BoolPatternObject.boolPatternObject (expr#returnBool()))
  else if expr#isFloat() then
    (new FloatPatternObject.floatPatternObject (expr#returnFloat()))
  else if expr#isInt() then
    (new IntPatternObject.intPatternObject (expr#returnInt()))
  else if expr#isChar() then
    (new CharPatternObject.charPatternObject (expr#returnChar()))
  else if expr#isList() then
    (new ListPatternObject.listPatternObject (List.map exprToPattern (expr#returnList())))
  else if expr#isArray() then
    (new ArrayPatternObject.arrayPatternObject (Array.to_list (Array.map exprToPattern (expr#returnArray()))))
  else if expr#isString() then
    (new StringPatternObject.stringPatternObject (expr#returnString()))
  else
    raise NoCorrespondingPatternForExpr
;;

class idPatternObject id = 
object(self)
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractPatternObject.abstractPatternObject 
    
  method matchToExpression env expr = 
    Debug.stdDebug (self#toXml 3) "matchToExpression" "<-" "";
    let exprEval = 
      try 
	expr#eval env 
      with
	exc -> 
	  raise exc
    in
    
    try 
      let idExpr = (env#getOnLevel id) in
      let pattern = exprToPattern idExpr in 
      let result = pattern#matchToExpression env exprEval in
      
      Debug.stdDebug (self#toXml 3) "matchToExpression" "->" (if result then "true" else "false");
      result
    with Env.IdNotDefined _ -> 
      env#add id exprEval;
      Debug.stdDebug (self#toXml 3) "matchToExpression" "->" "true";
      true

  method getIds () = 
    [ id ]

  method toString() = 
    id

  method toXml x = 
    match x with
      0 -> "..."
    | n -> "<idPatternObject>"^id^"</idPatternObject>"
	
end;;


exception NoCorrespondingPatternForExpr

let rec exprToPattern expr = 
  if (expr#isBool()) then
    (new BoolPatternObject.boolPatternObject (expr#returnBool()))
  else if expr#isNum() then
    (new NumPatternObject.numPatternObject (expr#returnNum()))
  else if expr#isObject() then
    begin
      let tmp = expr#returnObject() in
      let attr = tmp#getAttributes() in
      (new ObjectPatternObject.objectPatternObject (List.map (fun (attr,expr) -> (attr,(exprToPattern expr))) attr))
    end
  else if expr#isString() then
    (new StringPatternObject.stringPatternObject (expr#returnString()))
  else if expr#isMatrix() then
    begin
      let mat = (expr#returnMatrix()) in
      (new MatrixPatternObject.matrixPatternObject
	   (Array.map (fun x -> (Array.map (fun y -> exprToPattern (new NumExpressionObject.numExpressionObject y)) x)) mat) )
    end
  else
    raise NoCorrespondingPatternForExpr
;;

class idPatternObject id = 
object(self)
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractPatternObject.abstractPatternObject 
    
  method matchToExpression env expr =
    Debug.debugStartMethod self "matchToExpression";
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

      Debug.debugEndMethod self "matchToExpression" self;
      result
    with Env.IdNotDefined _ -> 
      env#add id exprEval;
      Debug.debugEndMethod self "matchToExpression" self;
      true

  method getIds () = 
    [ id ]

  method toTree() =
    CartesianTree.IDPATTERN id
      
end;;

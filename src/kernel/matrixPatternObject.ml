
class matrixPatternObject (mat: AbstractExpressionObject.abstractExpressionObject AbstractPatternObject.abstractPatternObject array array)  =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractPatternObject.abstractPatternObject 
							      
  method matchToExpression env expr = 
    let exprEval = expr#eval env in

    if exprEval#isMatrix() then
      begin
		let exprMat = exprEval#returnMatrix() in
		BasicTools.array2foldIJ (fun acc i j y -> if acc then (mat.(i).(j))#matchToExpression env (new  NumExpressionObject.numExpressionObject y) else false) exprMat true;
      end
    else
      false

  method getIds() = 
    []

  method toTree() = 
    CartesianTree.MATRIXPATTERN
      (Array.map (fun x -> (Array.map (fun y -> y#toTree()) x)) mat)
	
end;;


class matrixExpressionObject (exprMat : AbstractExpressionObject.abstractExpressionObject array array)=
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
	    
  method eval env =
    let mat =
      Array.map (fun x ->
		 Array.map (fun y ->
			    let vl = y#eval env in
			    vl#returnNum() ) x) exprMat
    in
    ((new MatrixWrapperObject.matrixWrapperObject mat) :> AbstractExpressionObject.abstractExpressionObject)

  method preEval env idList =
    let mat =
      Array.map (fun x ->
		 Array.map (fun y ->
			    let (_,vl) = y#preEval env idList in
			    vl) x) exprMat
    in
    (idList,((new matrixExpressionObject mat) :> AbstractExpressionObject.abstractExpressionObject))
      
  method toTree() = 
    CartesianTree.MATRIXEXPRESSION (Array.map (fun x -> (Array.map (fun y -> y#toTree()) x)) exprMat)

end;;

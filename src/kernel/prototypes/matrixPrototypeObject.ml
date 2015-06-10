
class matrixPrototypeObject uc (mat: (float*float) array array) = 
object(self)
  inherit AbstractPrototypeObject.abstractPrototypeObject uc
    
  method returnValue() = 
	new MatrixExpressionObject.matrixExpressionObject 
		(Array.map (fun x -> (Array.map (fun y -> new NumExpressionObject.numExpressionObject y) x)) mat)
    
  method verifyValue vl =
	if vl#isNum() then
		begin
			let mat0 = vl#returnMatrix() in
			MatrixTools.matrixCompare mat0 mat
		end
	else
		false

  method toTree() =
    CartesianTree.MATRIXPROTOTYPE (uc,mat)
			    
end;;
      

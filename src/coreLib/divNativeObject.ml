
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class divNumHelper (re,im) =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj =
    let (re0,im0) = obj#returnNum() in
    (new NumExpressionObject.numExpressionObject (ComplexTools.div (re,im) (re0,im0)))
	
  method evalNOD obj = obj
			 
end;;

class divNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;

class divMatrixHelper mat =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
							      
  method evalNOD obj = obj
  method evalNum obj =
    let den = (obj#returnNum()) in
    new MatrixWrapperObject.matrixWrapperObject (BasicTools.array2mapIJ (fun i j x -> ComplexTools.div x den) mat)
end;;
  
class divHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new divNodHelper defaultValue)
    
  method evalNum obj = (new divNumHelper (obj#returnNum()))
  method evalNOD obj = (new divNodHelper obj)
  method evalMatrix obj = (new divMatrixHelper (obj#returnMatrix()))
end;;

class divNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_/" (new divHelper)
end;;

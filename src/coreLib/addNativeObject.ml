
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class addNumHelper (re,im) =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj =
    let (re0,im0) = obj#returnNum() in
    new NumExpressionObject.numExpressionObject (re +. re0,im +. im0)
  method evalNOD obj = obj 
end;;

class addStringHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalString obj = new StringExpressionObject.stringExpressionObject (x ^ (obj#returnString()))
  method evalNOD obj = obj 
end;;

class addBoolHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalBool obj = new BoolExpressionObject.boolExpressionObject (if x == (obj#returnBool()) then false else true)
  method evalNOD obj = obj 
end;;

class addActionHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
  
  method evalAction obj = 
    new ActionExpressionObject.actionExpressionObject [
      x; 
      obj
    ];

  method evalNOD obj = obj 
end;;

class addNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;

class addMatrixHelper mat =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalMatrix obj =
    let mat0 = obj#returnMatrix() in
    let newMat = BasicTools.array2mapIJ (fun i j (re,im) ->
					 let (re0,im0) = mat0.(i).(j) in (re +. re0,im +. im0)) mat
    in
    (new MatrixExpressionObject.matrixExpressionObject newMat)
end;;
      
class addHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new addNodHelper defaultValue)
    
  method evalNum obj = (new addNumHelper (obj#returnNum()))
  method evalString obj = (new addStringHelper (obj#returnString()))
  method evalBool obj = (new addBoolHelper (obj#returnBool()))
  method evalMatrix obj = (new addMatrixHelper (obj#returnMatrix()))
  method evalAction obj = (new addActionHelper obj)
  method evalNOD obj = (new addNodHelper obj)
end;;

class addNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_+" (new addHelper)

  method toTree() = CartesianTree.NATIVEFUNCTION

end;;

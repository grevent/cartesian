
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class addFloatHelper fl =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject (fl +. (obj#returnFloat()))
  method evalInt obj = new FloatExpressionObject.floatExpressionObject (fl +. (float_of_int (obj#returnInt())))
  method evalNOD obj = obj 
end;;

class addIntHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject ((float_of_int x) +. (obj#returnFloat()))
  method evalInt obj = new IntExpressionObject.intExpressionObject (x + (obj#returnInt()))
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

class addListHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalList obj = new ListExpressionObject.listExpressionObject (x@(obj#returnList()))
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
    
class addHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new addNodHelper defaultValue)
    
  method evalFloat obj = (new addFloatHelper (obj#returnFloat()))
  method evalInt obj = (new addIntHelper (obj#returnInt()))
  method evalString obj = (new addStringHelper (obj#returnString()))
  method evalBool obj = (new addBoolHelper (obj#returnBool()))
  method evalList obj = (new addListHelper (obj#returnList()))
  method evalAction obj = (new addActionHelper obj)
  method evalNOD obj = (new addNodHelper obj)
end;;

class addNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_+" (new addHelper)
    
  method toXml x = 
    match x with
      0 -> "..."
    | _ -> "<addNativeObject/>"
end;;

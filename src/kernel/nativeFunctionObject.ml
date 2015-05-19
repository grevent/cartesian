
class nativeFunctionObject implementation = 
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method isFunction() = true
  method returnFunction env = implementation
  method eval env = 
    Debug.debugStartMethod self "eval";
    Debug.debugEndMethod self "eval" self;
    (self :> AbstractExpressionObject.abstractExpressionObject)

  method preEval env idList = 
    Debug.debugStartMethod self "preEval";
    let (newIdList,newImplementation) = implementation#preEval env idList in
    let result = ((new nativeFunctionObject newImplementation) :> AbstractExpressionObject.abstractExpressionObject) in
    Debug.debugEndMethod self "preEval" result;
    (newIdList,result)

  method toTree() = 
    CartesianTree.NATIVEFUNCTION 

end;;

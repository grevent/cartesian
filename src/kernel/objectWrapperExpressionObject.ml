
class objectWrapperExpressionObject (obj: AbstractExpressionObject.abstractExpressionObject ObjectObject.objectObject) =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject 
    
  method eval env = 
    (self :> AbstractExpressionObject.abstractExpressionObject)
      
  method returnObject() = obj
    
  method isObject() = true

  method copy() = 
    ((new objectWrapperExpressionObject (obj#copyObj())) :> AbstractExpressionObject.abstractExpressionObject)

  method toString() = 
    (obj#toString())

  method preEval env idList = 
    (idList,(self :> (AbstractExpressionObject.abstractExpressionObject)))

  method toXml x = 
    match x with
      0 -> "..."
    | x -> 
      "<objectWrapperExpressionObject>"^(obj#toXml(x-1))^"</objectWrapperExpressionObject>"

end;;

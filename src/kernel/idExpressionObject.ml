
class idExpressionObject (id: string) =
object
  inherit AbstractExpressionObject.abstractExpressionObject 
    
  method eval env = 
    env#get id

  method toString() = 
    id
end

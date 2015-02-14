

class virtual abstractPrototypeObject (useCase: string) =
object
  inherit RuntimeObject.runtimeObject
    
  method returnUC() = useCase
  method isUC uc = if (compare useCase uc) == 0 then true else false
    
  method virtual returnValue: unit -> AbstractExpressionObject.abstractExpressionObject
  method virtual verifyValue: AbstractExpressionObject.abstractExpressionObject -> bool
end;;


class startActionObject (dispatcher: Dispatcher.dispatcher) blocking signals =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject
    
  method exec parents = 
    dispatcher#registerStart message actorId action parents
      
  method preExec env idList = 
    let (_,preExecedAction) = action#preExec env idList in 
    
    (idList,((new startActionObject listener id preExecedAction)
	     :> (AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject)))
      
  method toRepresentation() =
    CartesianRepresentation.STARTACTION (blocking,(List.map (fun x -> x#toRepresentation()) signals))
      
end;;

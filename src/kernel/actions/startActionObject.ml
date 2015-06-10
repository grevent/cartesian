
exception AlreadyInSession

class startActionObject (dispatcher: Dispatcher.dispatcher) id action =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject
							      
  method exec session parents =
    dispatcher#register id (fun newSession -> (action#exec newSession parents))
      
  method preExec env idList = 
    let (_,preExecedAction) = action#preExec env idList in 
    
    (idList,((new startActionObject dispatcher id preExecedAction)
	     :> (AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject)))
      
  method toTree() =
    CartesianTree.REGISTERSTARTACTION (id,(action#toTree()))
      
end;;

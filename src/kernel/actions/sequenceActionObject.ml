
class sequenceActionObject actions =
  object
    inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject
								
    method exec session parents =
      List.iter (fun action -> action#exec session parents) actions;
	     
    method preExec env idList =
      let (newIds,newActions) =
	List.fold_left (fun (ids,actions) action ->
			let (nextIds,nextAction) = action#preExec env ids in
			(nextIds,actions@[nextAction]) )
		       (idList,[])
		       actions
      in
      (newIds,((new sequenceActionObject newActions) :>
	 (AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject)))
	
  method toTree() = 
    CartesianTree.SEQUENCEACTION (List.map (fun x -> x#toTree()) actions)
      
end;;

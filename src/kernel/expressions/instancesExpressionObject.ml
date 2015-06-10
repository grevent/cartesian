
exception UseCaseNotPrototyped
  
class instancesExpressionObject useCases =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method eval env = 
    match env#getMode() with
      Env.Standard -> 
	let useCase = (List.nth useCases (Random.int (List.length useCases))) in
	env#setUC (useCase#returnUC());
	useCase#returnValue();
    | Env.UseCase uc -> 
      let lst = List.filter (fun x -> x#isUC uc) useCases in
      (match lst with
	[] -> raise UseCaseNotPrototyped
      | _ -> let useCase = (List.nth useCases (Random.int (List.length useCases))) in
	     useCase#returnValue();)

  method preEval env idList = 
    (idList,self#eval env)

  method toTree() =
    CartesianTree.INSTANCESEXPRESSION (List.map (fun x -> x#toTree()) useCases)

end;;


class objectExpressionObject (defs: (string*((AbstractExpressionObject.abstractExpressionObject AbstractPatternObject.abstractPatternObject) list)*(AbstractExpressionObject.abstractExpressionObject)) list) =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method eval env =
    let obj = new ObjectObject.objectObject [] in
    
    List.iter (fun (id,patterns,expr) -> 
	       obj#addAttribute id 
				(if List.length patterns > 0 then
				   (new FunctionExpressionObject.functionExpressionObject [(patterns,expr)])
				 else
	    (expr#eval env) ) ) defs;
    
    new ObjectWrapperExpressionObject.objectWrapperExpressionObject obj

  method toTree() = 
    CartesianTree.OBJECTEXPRESSION 
      (List.map (fun (id,patterns,expr) -> (id,(List.map (fun x -> x#toTree()) patterns),expr#toTree())) defs)

  method preEval env idList = 
    let idsWithAtt = List.fold_left (fun acc (id,_,_) -> acc@[id]) idList defs in
    let pDefs = List.map (fun (id,params,expr) -> 
      let ids = List.fold_left (fun acc param -> acc@(param#getIds())) idsWithAtt params in
      let (_,pExpr) = expr#preEval env ids in
      (id,params,pExpr) ) defs
    in
    (idList,((new objectExpressionObject pDefs) :> AbstractExpressionObject.abstractExpressionObject))

end;;

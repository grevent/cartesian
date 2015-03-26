
class objectExpressionObject (defs: (string*((AbstractExpressionObject.abstractExpressionObject AbstractPatternObject.abstractPatternObject) list)*(AbstractExpressionObject.abstractExpressionObject)) list) =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method eval env =
    let obj = new ObjectObject.objectObject in
    
    List.iter (fun (id,patterns,expr) -> 
      obj#addAttribute id 
	(if List.length patterns > 0 then
	    (new FunctionExpressionObject.functionExpressionObject [(patterns,expr)])
	 else
	    (expr#eval env) ) ) defs;
    
    new ObjectWrapperExpressionObject.objectWrapperExpressionObject obj

  method toString() = 
    "{ "^(List.fold_left (fun acc (attribute,patterns,expr) -> 
      acc^attribute^
	(List.fold_left (fun acc pattern -> acc^(pattern#toString())^" ") "" patterns)^"= "^(expr#toString())) "" defs)^" }"

  method preEval env idList = 
    let idsWithAtt = List.fold_left (fun acc (id,_,_) -> acc@[id]) idList defs in
    let pDefs = List.map (fun (id,params,expr) -> 
      let ids = List.fold_left (fun acc param -> acc@(param#getIds())) idsWithAtt params in
      let (_,pExpr) = expr#preEval env ids in
      (id,params,pExpr) ) defs
    in
    (idList,((new objectExpressionObject pDefs) :> AbstractExpressionObject.abstractExpressionObject))

  method toXml x = 
    match x with
      0 -> "..."
    | x -> 
      "<objectExpressionObject>"^
	(List.fold_left (fun acc (id,patterns,expr) -> acc^"<id>"^id^"</id>"^
	  (List.fold_left (fun acc pattern -> acc^(pattern#toXml(x-1))) "" patterns)^
	  (expr#toXml(x-1))) "" defs)^
	"</objectExpressionObject>"

end;;

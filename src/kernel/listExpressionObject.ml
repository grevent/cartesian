
class listExpressionObject lst = 
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method eval env =
    let obj = new ObjectObject.objectObject [] in

    List.iter (fun x -> (obj#add (x#eval env))) lst;
    new ObjectWrapperExpressionObject.objectWrapperExpressionObject obj

  method preEval env idList = 
    AbstractExpressionObject.listPreEval env idList lst (fun x -> new listExpressionObject x)

  method toTree() = 
    CartesianTree.LISTEXPRESSION (List.map (fun x -> x#toTree()) lst)

end;;

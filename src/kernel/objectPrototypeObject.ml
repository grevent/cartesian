
class objectPrototypeObject uc lst =
object
  inherit AbstractPrototypeObject.abstractPrototypeObject uc 
    
  method returnValue() = 
    let obj = new ObjectObject.objectObject in
    
    List.iter (fun (attr,proto) -> obj#addAttribute attr (proto#returnValue())) lst;

    new ObjectWrapperExpressionObject.objectWrapperExpressionObject obj;

  method verifyValue vl = 
    let obj = vl#returnObject() in
    (List.fold_left (fun acc (attr,proto) -> if (proto#verifyValue (obj#getAttribute attr)) then acc else false) true lst)

  method toTree() = 
    CartesianTree.OBJECTPROTOTYPE (uc,
				   (List.map (fun (id,expr) -> (id,expr#toTree())) lst))
      
end;;

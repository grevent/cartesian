
class listPrototypeObject uc lst =
object
  inherit AbstractPrototypeObject.abstractPrototypeObject uc
    
  method returnValue() =
    let obj = new ObjectObject.objectObject [] in
    List.iter (fun x -> (obj#add (x#returnValue()))) lst;
    new ObjectWrapperExpressionObject.objectWrapperExpressionObject obj;
    
  method verifyValue vl = 
    if vl#isObject() then
      (List.fold_left2 (fun acc x y -> if (x#verifyValue y) then acc else false) true lst (vl#returnObjectAsList()))
    else
      false

  method toTree() =
    CartesianTree.LISTPROTOTYPE (uc,(List.map (fun x -> x#toTree()) lst))

end;;

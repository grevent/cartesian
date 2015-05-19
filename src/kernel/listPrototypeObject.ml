
class listPrototypeObject uc lst =
object
  inherit AbstractPrototypeObject.abstractPrototypeObject uc
    
  method returnValue() = 
    new ListWrapperExpressionObject.listWrapperExpressionObject (List.map (fun x -> x#returnValue()) lst)
      
  method verifyValue vl = 
    if vl#isList() then
      (List.fold_left2 (fun acc x y -> if (x#verifyValue y) then acc else false) true lst (vl#returnList()))
    else
      false

  method toTree() =
    CartesianTree.LISTPROTOTYPE (uc,(List.map (fun x -> x#toTree()) lst))

end;;

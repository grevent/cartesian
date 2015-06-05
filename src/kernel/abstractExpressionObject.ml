
exception CanNotConvertToBool of string
exception CanNotConvertToString of string
exception CanNotConvertToNum of string
exception CanNotConvertToInt of string
exception CanNotConvertToFloat of string
exception CanNotConvertToBoolean of string
exception CanNotConvertToObject of string
exception CanNotConvertToAction of string
exception CanNotConvertToFunction of string
exception CanNotConvertToMatrix of string
exception CanNotConvertToVector of string
				       
exception ExpressionNotAnObject
exception ExpressionNotAFunction    

class virtual abstractExpressionObject =
object(self)
  inherit RuntimeObject.runtimeObject
  
  method virtual eval: abstractExpressionObject Env.env -> abstractExpressionObject
  method virtual preEval: abstractExpressionObject Env.env -> string list -> ((string list)*abstractExpressionObject)

  method isString() = false
  method isNum() = false
  method isMatrix() = false
  method isObject() = false
  method isFunction() = false
  method isAction() = false
  method isNOD() = false
  method isBool() = false
  method isId() = false

  method returnMatrix(): (float*float) array array = raise (CanNotConvertToMatrix (self#toString()))
  method returnId() : string = raise (CanNotConvertToBool (self#toString()))
  method returnBool() : bool = raise (CanNotConvertToBool (self#toString()))
  method returnString() : string = raise (CanNotConvertToString (self#toString()))
  method returnNum() : (float*float) = raise (CanNotConvertToNum (self#toString()))
  method returnBoolean(): bool = raise (CanNotConvertToBoolean (self#toString()))
  method returnObject(): abstractExpressionObject ObjectObject.objectObject = raise (CanNotConvertToObject (self#toString()))
  method returnAction(): abstractExpressionObject AbstractActionObject.abstractActionObject = raise (CanNotConvertToAction (self#toString()))
  method returnFunction (env: abstractExpressionObject Env.env): abstractExpressionObject FunctionObject.functionObject = raise (CanNotConvertToFunction (self#toString()))


  method returnNumAsFloat() =
    let (re,im) = self#returnNum() in
    if (mod_float im 1.0) > 0.0 then
      raise (CanNotConvertToFloat (Printf.sprintf "(%f;%f)" re im))
    else
      re
	
  method returnNumAsInt() =
    let vl = self#returnNumAsFloat() in
    let vl_i = mod_float vl 1.0 in
    if vl_i > 0.0 then
      raise (CanNotConvertToInt (Printf.sprintf "%f" vl))
    else
      (int_of_float vl)

  method returnMatrixAsVector() =
    let mat = self#returnMatrix() in
    if (Array.length mat) == 1 then
      mat.(0)
    else
      raise (CanNotConvertToVector (self#toString()));
	
  method returnObjectAsList() =
    let obj = self#returnObject() in
    List.map (fun (_,el) -> el) (obj#getAttributes());
	
  method copy() = 
    (self :> abstractExpressionObject)

end;;

let listPreEval env idStart lst newFn = 
  let (nextIdList,nextLst) = (List.fold_left 
				(fun (currentIdList,currentLst) x -> 
				  let (tmpIdList,tmpX) = x#preEval env currentIdList in
				  (tmpIdList,(currentLst@[tmpX])) )
				(idStart,[])
				lst ) in
  (nextIdList, 
   ((newFn nextLst) :> abstractExpressionObject) )
    
let arrayPreEval env idStart ar newFn = 
  let (nextIdList,nextLst) = (List.fold_left 
				(fun (currentIdList,currentLst) x -> 
				  let (tmpIdList,tmpX) = x#preEval env currentIdList in
				  (tmpIdList,(currentLst@[tmpX])) )
				(idStart,[])
				(Array.to_list ar) ) in
  (nextIdList, 
   ((newFn (Array.of_list nextLst)) :> abstractExpressionObject) )
  

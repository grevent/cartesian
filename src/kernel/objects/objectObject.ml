
exception AttributeDoesNotExist of string
exception AttributeExistsMoreThanOneTime of string
exception AttributeAlreadyExisting of string

let createAttrName obj =
  let rec helper vl =
    let str = Printf.sprintf "_%d" vl in
    if obj#isAttributeExisting str then
      helper (vl+1)
    else
      str
  in
  helper (obj#amountAttributes())
;;
  
class ['expression] objectObject startAttributes = 
object(self)
  inherit RuntimeObject.runtimeObject
    
  val mutable attributes = (startAttributes : (string*'expression) list )

  method toTree() = 
    CartesianTree.OBJECT (List.map (fun (id,expr) -> id,(expr#toTree())) attributes)
      
  method getAttributes() = 
    attributes

  method isAttributeExisting attr = 
    let result = List.filter (fun (x,_) -> (compare x attr == 0)) attributes in    
    match result with 
      [] -> false
    | _ -> true
      
  method copyObj() = 
    let newObj = new objectObject [] in

    (List.iter 
       (fun (att,expr) -> 
	 let tmp = expr#copy() in
	 (newObj#addAttribute att tmp) ) 
       attributes);
    
    newObj
      
  method addAttribute attr obj = 
    if self#isAttributeExisting attr then
      raise (AttributeAlreadyExisting attr)
    else
      attributes <- attributes@[(attr,obj)];
    
  method setAttribute attr0 obj = 
    let rec helper = function
	[] -> 
	raise (AttributeDoesNotExist attr0)
      | (attr,vl)::suite -> 
	 if (compare attr attr0) == 0 then 
	   (attr,obj)::suite 
	 else 
	   (attr,vl)::(helper suite)
    in
    attributes <- helper attributes 
			 
  method renameAttribute attr0 attr1 =
    let rec helper = function
	[] -> 
	raise (AttributeDoesNotExist attr0)
      | (attr,vl)::suite -> 
	 if (compare attr attr0) == 0 then 
	   (attr1,vl)::suite 
	 else 
	   (attr,vl)::(helper suite)
    in
    attributes <- helper attributes 
			 
  method amountAttributes () = 
    List.length attributes
      
  method getAttribute att =
    let result = List.filter (fun (x,_) -> (compare x att == 0)) attributes in
    match result with
      [] -> raise (AttributeDoesNotExist att)
    | [(_,obj)] -> obj
    | _ -> raise (AttributeExistsMoreThanOneTime att)

  method getAt i =
    let (_,vl) = List.nth attributes i in
    vl

  method cons vl =
    let result = new objectObject (self#getAttributes()) in
    result#addFirst vl;
    result
		    
  method add vl =
    attributes <- attributes@[((createAttrName self),vl)];

  method addFirst vl =
    attributes <- (createAttrName self,vl)::attributes;

end;;

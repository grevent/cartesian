
exception AttributeDoesNotExist of string
exception AttributeExistsMoreThanOneTime of string
exception AttributeAlreadyExisting of string

class ['expression] objectObject = 
object(self)
  val mutable attributes = ([] : (string*'expression) list )
   
  method toString() = 
    "{ "^
      (List.fold_left (fun acc (id,expr) -> acc^id^"= "^(expr#toString())^"; ") "" attributes)^
      "}"

  method toXml x =
    match x with
      0 -> "..."
    | x -> 
      "<objectObject>"^
	(List.fold_left (fun acc (id,expr) -> acc^"<id>"^id^"</id>"^(expr#toXml(x-1))) "" attributes)^
	"</objectObject>"

  method getAttributes() = 
    attributes
    
  method isAttributeExisting attr = 
    let result = List.filter (fun (x,_) -> (compare x attr == 0)) attributes in    
    match result with 
      [] -> false
    | _ -> true
      
  method copyObj() = 
    let newObj = 
      new objectObject in

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

  method amountAttributes () = 
    List.length attributes
      
  method getAttribute att =
    let result = List.filter (fun (x,_) -> (compare x att == 0)) attributes in
    match result with
      [] -> raise (AttributeDoesNotExist att)
    | [(_,obj)] -> obj
    | _ -> raise (AttributeExistsMoreThanOneTime att)

end;;

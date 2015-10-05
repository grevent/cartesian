(* Comment *) 

exception ParamDoesNotExist
exception DerivingAdapterWithoutNewName
exception DerivingAdapterWithSameName

let rec paramsReplacement originParams newParams = 
	match originParams with
		[] -> newParams
	| (key,value)::suite -> 
		if (List.exists (fun (x,_) -> if (String.compare x key) == 0 then true else false) originParams) then
			paramsReplacement suite newParams
		else
			paramsReplacement suite ((key,value)::newParams)
;;

class virtual abstractAdapter (startConfig: (string*string) list) =
object(self)
  val mutable currentConfig = startConfig
  
  method updateParams newConfig = 
	currentConfig <- paramsReplacement currentConfig newConfig
  
  method virtual listen: (int -> string -> CartesianTree.cartesianTree -> unit) ->
			 (string -> int) ->
			 unit
			  
  method virtual send: int -> CartesianTree.cartesianTree -> unit

  method deriveAdapter newParams = 
	let oldName = self#getId() in
	(try
		let (_,newName) = List.find (fun (x,_) -> if (String.compare x "name") == 0 then true else false) currentConfig in
		if (String.compare newName oldName) == 0 then
			raise DerivingAdapterWithSameName
	with
		_ -> raise DerivingAdapterWithoutNewName);
  
	let newObject = Oo.copy self in
	newObject#updateParams newParams;
	newObject
   
  method getId() = 
	self#getParam "name"

  method getParam key = 
	try
		let (_,result) = List.find (fun (x,_) -> if (String.compare x key) == 0 then true else false) currentConfig in
		result
	with _ -> raise ParamDoesNotExist

end;;


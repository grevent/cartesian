
exception NoLevels
exception IdNotDefined of string

type evalMode = 
  Standard
| UseCase of string
;;

class ['a] env = 
object
  val mutable currentLevel = []
  val mutable levels = []
  val mutable mode = Standard
    
  method getMode () = 
    mode
      
  method setMode md =
    mode <- md;
  
  method setUC uc = 
    mode <- UseCase uc

  method addLevel() = 
    levels <- currentLevel::levels;
    currentLevel <- [];
    
  method removeLevel() = 
    match levels with
      [] -> raise NoLevels
    | el::suite -> currentLevel <- el; levels <- suite
      
  method add (id: string) (value: 'a) =
    currentLevel <- (id,value)::currentLevel

  method getOnLevel (id0: string) = 
    let filterResult = List.filter (fun (id,vl) -> if (compare id id0) == 0 then true else false) currentLevel in
    match filterResult with
      [] -> raise (IdNotDefined  id0)
    | (_,vl)::_ -> vl

  method get (id0: string) =
    let rec helper lst =
      match lst with
	[] -> raise (IdNotDefined id0)
      | []::suite -> helper suite
      | el::suite -> 
	let filterResult = List.filter (fun (id,vl) -> if (compare id id0) == 0 then true else false) el in
	match filterResult with
	  [] -> helper suite 
	| (_,vl)::_ -> vl
    in
    helper (currentLevel::levels)
    
  method toString() = 
    let modeStr = match mode with
	Standard -> "Standard mode" 
    | UseCase st -> "Use Case "^st
    in
    let levelsStr = List.fold_left (fun acc lst -> 
      let lstStr = List.fold_left (fun acc (id,vl) -> acc^id^": "^(vl#toString())^"; ") "" lst in
      acc^"["^lstStr^"]\n") "" (currentLevel::levels) 
    in
    "ENV "^modeStr^" [\n"^levelsStr^"\n]\n"

end;;

let newEnv objs =
  let env = new env in
  let rec helper objs = 
    match objs with
      obj::others -> 
	let attributes = obj#getAttributes() in
	List.iter (fun (id,expr) -> (env#add id (expr#copy()))) attributes;
	helper others
    | [] -> 
      env
  in
  (helper (List.rev objs))
;;

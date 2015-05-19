
exception NoLevels
exception IdNotDefined of string

type evalMode = 
  Standard
| UseCase of string
;;

class ['a] env = 
object(self)
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
    Debug.debugStartMethod self "addLevel";
    levels <- currentLevel::levels;
    currentLevel <- [];
    Debug.debugEndMethod self "addLevel" self;
    
  method removeLevel() = 
    Debug.debugStartMethod self "removeLevel";
    (match levels with
      [] -> raise NoLevels
    | el::suite -> currentLevel <- el; levels <- suite);
    Debug.debugEndMethod self "removeLevel" self;
      
  method add (id: string) (value: 'a) =
    Debug.debugStartMethod self "add";
    currentLevel <- (id,value)::currentLevel;
    Debug.debugEndMethod self "add" self;

  method getOnLevel (id0: string) = 
    let filterResult = List.filter (fun (id,vl) -> if (compare id id0) == 0 then true else false) currentLevel in
    match filterResult with
      [] -> raise (IdNotDefined  id0)
    | (_,vl)::_ -> vl

  method get (id0: string) =
    Debug.debugStartMethod self "get";
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
    let result = helper (currentLevel::levels) in
    Debug.debugEndMethod self "get" self;
    result
    
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

  method signature() = 
    let modeStr = match mode with
	Standard -> "STD" 
      | UseCase st -> "UC("^st^")"
    in
    let tmp = List.fold_left (fun acc lst -> (Printf.sprintf "%s %d" acc (List.length lst))) "" (currentLevel::levels) in
    modeStr^"("^tmp^")"

  method toXml() = 
    "<env/>"

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
  let result = (helper (List.rev objs)) in
  result
;;

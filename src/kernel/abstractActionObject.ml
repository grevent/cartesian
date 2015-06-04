
exception NoParents 

class virtual ['expression] abstractActionObject = 
object
  inherit RuntimeObject.runtimeObject
    
  method virtual exec: int -> ('expression ObjectObject.objectObject list) -> unit
  method virtual preExec: 'expression Env.env -> string list -> ((string list)*('expression abstractActionObject))
end;;

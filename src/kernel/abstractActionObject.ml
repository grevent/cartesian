
exception NoParents 

class virtual ['expression] abstractActionObject = 
object
  inherit RuntimeObject.runtimeObject
    
  method virtual exec: ('expression ObjectObject.objectObject list) -> unit
end;;

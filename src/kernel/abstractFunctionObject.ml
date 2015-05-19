
exception ApplyNotPossible
exception ParametersNotAccepted of (string*(string list))

class virtual ['expression] abstractFunctionObject =
object
  inherit RuntimeObject.runtimeObject
    
  method virtual apply: 'expression Env.env -> 'expression list -> 'expression
  method virtual preEval: 'expression Env.env -> string list -> (string list * 'expression abstractFunctionObject)
end;;

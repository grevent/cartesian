
exception ApplyNotPossible
exception ParametersNotAccepted of (string*(string list))

class virtual ['expression] abstractFunctionObject =
object
  inherit RuntimeObject.runtimeObject
    
  method virtual toString: unit -> string
  method virtual apply: 'expression Env.env -> 'expression list -> 'expression
end;;

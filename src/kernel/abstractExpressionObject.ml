
exception CanNotConvertToBool of string
exception CanNotConvertToString of string
exception CanNotConvertToFloat of string
exception CanNotConvertToInt of string
exception CanNotConvertToChar of string
exception CanNotConvertToList of string
exception CanNotConvertToArray of string
exception CanNotConvertToBoolean of string
exception CanNotConvertToObject of string
exception CanNotConvertToAction of string
exception CanNotConvertToFunction of string
exception ExpressionNotAnObject
exception ExpressionNotAFunction    

class virtual abstractExpressionObject =
object(self)
  inherit RuntimeObject.runtimeObject
    
  method virtual eval: abstractExpressionObject Env.env -> abstractExpressionObject
  method virtual preEval: abstractExpressionObject Env.env -> string list -> abstractExpressionObject

  method isString() = false
  method isFloat() = false
  method isInt() = false
  method isChar() = false
  method isObject() = false
  method isFunction() = false
  method isList() = false
  method isArray() = false
  method isAction() = false
  method isNOD() = false
  method isBool() = false

  method returnBool() : bool = raise (CanNotConvertToBool (self#toString()))
  method returnString() : string = raise (CanNotConvertToString (self#toString()))
  method returnFloat() : float = raise (CanNotConvertToFloat (self#toString()))
  method returnInt() : int = raise (CanNotConvertToInt (self#toString()))
  method returnChar() : char = raise (CanNotConvertToChar (self#toString()))
  method returnList() : abstractExpressionObject list = raise (CanNotConvertToList (self#toString()))
  method returnArray() : abstractExpressionObject array = raise (CanNotConvertToArray (self#toString()))
  method returnBoolean(): bool = raise (CanNotConvertToBoolean (self#toString()))
  method returnObject(): abstractExpressionObject ObjectObject.objectObject = raise (CanNotConvertToObject (self#toString()))
  method returnAction(): abstractExpressionObject AbstractActionObject.abstractActionObject = raise (CanNotConvertToAction (self#toString()))
  method returnFunction (env: abstractExpressionObject Env.env): abstractExpressionObject FunctionObject.functionObject = raise (CanNotConvertToFunction (self#toString()))
  
  method copy() = 
    (self :> abstractExpressionObject)

end;;

    

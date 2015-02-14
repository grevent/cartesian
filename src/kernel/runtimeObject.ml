
exception InternalError
exception ObjectCanNotBeEvaluated
exception NotAFunction
exception NotABoolean
exception NotAListNorAVector

class virtual runtimeObject = 
object(self)
  val mutable comment = ""

  method virtual toString: unit -> string

  method attachComment st = comment <- st

end


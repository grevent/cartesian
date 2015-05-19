
exception InternalError
exception ObjectCanNotBeEvaluated
exception NotAFunction
exception NotABoolean
exception NotAListNorAVector

class virtual runtimeObject = 
object(self)
  val mutable comment = ""

  method virtual toTree: unit -> CartesianTree.cartesianTree
  method toXml() = XmlRepresentation.generateXml (self#toTree())
  method toString() = StringRepresentation.generateString (self#toTree())

  method attachComment st = comment <- st

end


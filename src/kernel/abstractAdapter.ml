
class virtual abstractAdapter =
object(self)
  method virtual listen: (int -> string -> CartesianTree.cartesianTree -> unit)  -> unit 
  method virtual send: int -> CartesianTree.cartesianTree -> unit 
  method virtual getId: unit -> string
			  
end;;

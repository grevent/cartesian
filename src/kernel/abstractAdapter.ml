(* Comment *) 

class virtual abstractAdapter (actorName: string) =
object(self)

  method virtual listen: (int -> string -> CartesianTree.cartesianTree -> unit) ->
			 (string -> int) ->
			 Thread.t
			  
  method virtual send: int -> CartesianTree.cartesianTree -> unit
   
  method getId() = actorName 

end;;

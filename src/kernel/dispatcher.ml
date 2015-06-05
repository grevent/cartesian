
let currentSessionId = ref 1;;

class dispatcher =
object(self)
  val adapters = Hashtbl.create 3
  val receiveQueues = Hashtbl.create 3
  val startHandlers = Hashtbl.create 3
  
	(* 
		Method for the creation of a new session:
		- Creates the associated queue
		- Creates a session Id, and returns it 
	*)
    
  method newSession() = 
    Hashtbl.add receiveQueues !currentSessionId (Queue.create());
    currentSessionId := !currentSessionId + 1;
    (!currentSessionId - 1)

	(* 
		Method for the addition or replacement of a new adapter. Associates it to the actor name
		At the end, starts the listeners of the given adapter
		Returns the thread of the associated thread
	*)

  method adapter (adapter: AbstractAdapter.abstractAdapter) = 
    Hashtbl.replace adapters (adapter#getId()) adapter;
    adapter#listen
	      (fun sessionId actorId tree -> self#actorSendsMessage sessionId actorId tree)
	      (fun actorId -> self#actorStartsSession actorId)
    
    (*
		Sends for a given session to an actor a given object represented as a tree 
    *)
    
  method send sessionId (actorId: string) (tree: CartesianTree.cartesianTree) = 
    let adapter = Hashtbl.find adapters actorId in
    adapter#send sessionId tree
      
      (*
		Gets for a given session the last received object represented as a tree in association to the actor Name
      *)
      
  method receive sessionId = 
    let queue = Hashtbl.find receiveQueues sessionId in
    let (actorId,tree) = Queue.pop queue in
    (actorId,tree)

	(*
		Registration by the program of a given handler for the starting of a session
	*)

  method register actorId (handler: int -> unit) =
    ignore (Hashtbl.replace startHandlers actorId handler);
      
      (* Concrete start of a given session... The listener of an actor should call this at start *)
      
  method actorStartsSession (actorId: string) =
    let session = self#newSession() in
    let handler = Hashtbl.find startHandlers actorId in
    (handler session);
    session
    
    (* Concrete reception, or storing in a queue of the given message send by an actor in a given session *)
    
  method actorSendsMessage sessionId (actorId: string) (tree: CartesianTree.cartesianTree) =
    let queue = Hashtbl.find receiveQueues sessionId in
    Queue.add (actorId,tree) queue;
      
end;;

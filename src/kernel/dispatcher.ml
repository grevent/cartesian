
let currentSessionId = ref 1;;

class dispatcher =
object(self)
  val adapters = Hashtbl.create 3
  val receiveQueues = Hashtbl.create 3
  val startHandlers = Hashtbl.create 3
    
  method newSession() = 
    Hashtbl.add receiveQueues !currentSessionId (Queue.create());
    currentSessionId := !currentSessionId + 1;
    (!currentSessionId - 1)

  method adapter (adapter: AbstractAdapter.abstractAdapter) = 
    Hashtbl.replace adapters (adapter#getId()) adapter;
    adapter#listen
	      (fun sessionId actorId tree -> self#actorSendsMessage sessionId actorId tree)
	      (fun actorId tree -> self#actorStartsSession actorId)
    
  method send sessionId (actorId: string) (tree: CartesianTree.cartesianTree) = 
    let adapter = Hashtbl.find adapters actorId in
    adapter#send sessionId tree
      
  method receive sessionId = 
    let queue = Hashtbl.find receiveQueues sessionId in
    let (actorId,tree) = Queue.pop queue in
    (actorId,tree)

  method register actorId (handler: int -> unit) =
    ignore (Hashtbl.replace startHandlers actorId handler);
      
  method actorStartsSession (actorId: string) =
    let session = self#newSession() in
    let handler = Hashtbl.find startHandlers actorId in
    (handler session);
    session
    
  method actorSendsMessage sessionId (actorId: string) (tree: CartesianTree.cartesianTree) =
    let queue = Hashtbl.find receiveQueues sessionId in
    Queue.add (actorId,tree) queue;
      
end;;

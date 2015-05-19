
let currentSessionId = ref 1;;

class dispatcher =
object(self)
  val adapters = Hashtbl.create 3
  val receiveQueues = Hashtbl.create 3 
    
  method newSession() = 
    Hashtbl.add receiveQueues !currentSessionId (Queue.create());
    currentSessionId := !currentSessionId + 1;
    (!currentSessionId - 1)

  method addAdapter (adapter: AbstractAdapter.abstractAdapter) = 
    Hashtbl.add adapters (adapter#getId()) adapter;
    adapter#listen (fun sessionId actorId tree -> self#actorSendsMessage sessionId actorId tree);
    
  method send sessionId (actorId: string) (tree: CartesianTree.cartesianTree) = 
    let adapter = Hashtbl.find adapters actorId in
    adapter#send sessionId tree
      
  method receive sessionId (actorId: string) = 
    let queue = Hashtbl.find receiveQueues sessionId in
    let (actorId,tree) = Queue.pop queue in
    (actorId,tree)

  method actorSendsMessage sessionId (actorId: string) (tree: CartesianTree.cartesianTree) =
    let queue = Hashtbl.find receiveQueues sessionId in
    Queue.add (actorId,tree) queue;
      
end;;

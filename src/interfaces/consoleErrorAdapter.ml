
exception ConsoleErrorAdapterCanTBeDerived

class consoleErrorAdapter =
object(self)
	inherit AbstractAdapter.abstractAdapter [("name","error")]
	
	val mutable consoleErrorId = (-1) 

	(* No listener for this, thus, no start or restart in classical sense *)
	
	method listen receiveHandler startHandler =			
		consoleErrorId <- startHandler (self#getId());
		
	method send sessionId tree = 
		if sessionId == consoleErrorId then
			begin
				prerr_string (StringRepresentation.generateString tree);
				prerr_newline();
			end
		else
			begin
				prerr_string (Printf.sprintf "Session %d: %s" sessionId (StringRepresentation.generateString tree));
				prerr_newline();
			end;
		
	method deriveAdapter lst = 
		raise ConsoleErrorAdapterCanTBeDerived;
end;;

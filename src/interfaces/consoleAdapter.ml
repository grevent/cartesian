
exception ConsoleAdapterCanTBeDerived

class consoleAdapter =
object(self)
	inherit AbstractAdapter.abstractAdapter [("name","console")]
	
	val mutable consoleSessionId = (-1)
	val mutable currentThread = Thread.self()
	val mutable firstStart = true
	
	(* Starts or restarts listener *)
	
	method listen receiveHandler startHandler =
		let listener sessionId = 
			while true do 
				let inputString = read_line() in
				let tree = StringRepresentation.fromString inputString in
				receiveHandler sessionId (self#getId()) tree
			done
		in
		
		if firstStart then
			begin
				firstStart <- false;
			end
		else
			begin
				Thread.kill currentThread;
			end;		

		consoleSessionId <- startHandler (self#getId());
		currentThread <- Thread.create listener consoleSessionId;
	
	method send sessionId tree = 
		if sessionId == consoleSessionId then
			begin
				print_string (StringRepresentation.generateString tree);
			end
		else
			begin
				prerr_string (Printf.sprintf "Session %d: %s" sessionId (StringRepresentation.generateString tree));
				prerr_newline();
			end;

	method deriveAdapter lst = 
		raise ConsoleAdapterCanTBeDerived

end;;

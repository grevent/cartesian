
class consoleAdapter =
object(self)
	inherit AbstractAdapter.abstractAdapter "console"
	
	val mutable consoleSessionId = (-1)
	
	method listen receiveHandler startHandler =
		let listener sessionId = 
			while true do 
				let inputString = read_line() in
				let tree = StringRepresentation.fromString inputString in
				receiveHandler sessionId (self#getId()) tree
			done
		in
		
		consoleSessionId <- startHandler (self#getId());
		Thread.create listener consoleSessionId;
	
	method send sessionId tree = 
		if sessionId == consoleSessionId then
			begin
				print_string (StringRepresentation.generateString tree);
			end
		else
			begin
				print_string (Printf.sprintf "Session %d: %s" sessionId (StringRepresentation.generateString tree));
			end;

end;;

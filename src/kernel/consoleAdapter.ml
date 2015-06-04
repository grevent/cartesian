
class consoleAdapter =
object
	inherit AbstractAdapter.abstractAdapter
	
	method getId() = "console"
	
	method send sessionId tree = 
		

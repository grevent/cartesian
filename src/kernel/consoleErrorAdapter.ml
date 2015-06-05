
class consoleErrorAdapter =
object
	inherit AbstractAdapter.abstractAdapter "error"
	
	method send sessionId tree = 
		

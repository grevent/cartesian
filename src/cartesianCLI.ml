
open Debug

let _ = 
    let rootObject = new ObjectObject.objectObject [] in

    InitCoreLib.init rootObject;
    InitStandardLib.init rootObject;
	
	let dispatcher = new Dispatcher.dispatcher in
	
	let initChannel = open_in "init.car" in
	let initBuffer = Lexing.from_channel initChannel in
	let tree = Syntax.phrase Lex.lexer initBuffer in

	close_in initChannel;

	let obj = CartesianTreeInterface.tree2Expr dispatcher tree in

	ignore (dispatcher#adapter (new ConsoleAdapter.consoleAdapter));
	ignore (dispatcher#adapter (new ConsoleErrorAdapter.consoleErrorAdapter));
	ignore (dispatcher#adapter (new SystemAdapter.systemAdapter));
(*	ignore (dispatcher#adapter (new GuiAdapter.guiAdapter));
	ignore (dispatcher#adapter (new RestClientAdapter.restClientAdapter));
	ignore (disptacher#adapter (new RestServerAdapter.restServerAdapter));
	ignore (dispatcher#adatper (new FileAdapter.fileAdapter));
*)

	let runtimeExpr = obj#eval  (Env.newEnv [rootObject]) in
	let runtimeAction = runtimeExpr#returnAction() in
	
	runtimeAction#exec (-1) [rootObject];
;;

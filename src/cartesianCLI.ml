
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
	let runtimeExpr = obj#eval  (Env.newEnv [rootObject]) in
	let runtimeAction = runtimeExpr#returnAction() in
	
	runtimeAction#exec (-1) [rootObject];
;;

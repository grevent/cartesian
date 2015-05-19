
open Debug

let _ = 
  try
    prerr_string (Printf.sprintf "\tCartesian V0.01\n"); 
    prerr_string (Printf.sprintf "\t(c) 2015 actioplan GmbH under GNU GENERAL PUBLIC LICENSE V3\n"); 
    prerr_newline();
    
    let rootObject = new ObjectObject.objectObject in

    InitCoreLib.init rootObject;
    InitStandardLib.init rootObject;
    
    let buffer = Lexing.from_channel stdin in 
    
    while true do
      (try
	  let tree = Syntax.phrase Lex.lexer buffer in
	  let obj = CartesianTreeInterface.tree2Action tree in
	  
	 obj#exec [rootObject];
	 prerr_string "OK"; prerr_newline();
	with 
	  Env.IdNotDefined id -> prerr_string (Printf.sprintf "Error: Identifier '%s' not defined" id); prerr_newline();
	| AbstractExpressionObject.CanNotConvertToAction st -> prerr_string (Printf.sprintf "Error: '%s' is not an action" st); prerr_newline();
	| Parsing.Parse_error -> prerr_string (Printf.sprintf "Error: Incorrect syntax"); prerr_newline();
	| FunctionObject.NoPatternForFunctionEval (fn,params) -> 
	   prerr_string (Printf.sprintf "Error: Function '%s' does have no rule for the parameters %s" 
					fn
					(List.fold_left (fun acc param -> acc^param^"; ") "" params));
	   prerr_newline();
      );
    done;
  with Lex.Eof ->
    prerr_string "Bye";
    prerr_newline();
    exit 0
;;


open Debug
open Ledit
open CartesianDataModel
open Runtime
open Object
open EvalTree

let rec read_line() = 
	let tmp = input_char stdin in

	if (Char.code tmp.[0]) == 10 then
		""
	else 
		tmp^(read_line())
;;

let cli prompt parser lexer evaluer =
	let addLine = ref false in
	let line = ref "" in
	let start = ref true in
    
	while !start || !addLine do  	
		try
			start := false;
			if !addLine then
				begin
					set_prompt (". ");
					line := (!line)^"\n"^(read_line())
				end
			else
				begin
					set_prompt (prompt^" ");
					line := (read_line());
				end;
				
			if String.compare !line "" != 0 then
				begin
					let lexbuf = Lexing.from_string !line in				
					let tree = parser lexer lexbuf in
					genericDebug "evaluating...";
					evaluer tree;
					addLine := false;					
				end;
		with 
			End_of_file -> 
				prerr_string (Printf.sprintf "\n\rPrompt reset");
				prerr_newline(); 
				addLine := false; |
			Parsing.Parse_error -> 
				prerr_string (Printf.sprintf "Syntax Error: '%s'" !line);
				prerr_newline();
				addLine := false |
			Match_failure e -> 
				prerr_string (Printexc.to_string (Match_failure e));
				prerr_newline();
				addLine := false; |
			e -> 
				prerr_string ("Continuing: "^(Printexc.to_string (e)));
				prerr_newline();
				addLine := true;
	done;
;;

let exec runtime tree = 
	let tp = evalExprType runtime tree in
	ignore (unification runtime.namedType runtime.genericTypes ACTION tp);
	let expr = evalExpr runtime tree in
	let actions = exprToActions expr in
	List.iter (evalAction runtime) actions;
;;  

let cartesianCLI () =
	let runtime = newRuntime() in
	
	print_string "\tcartesian system v0.1";
	print_newline();
	print_newline();

	while true do
		cli ">" CartesianSyntax.command CartesianLex.lexer exec;
	done;
;;


open Debug
open Ledit
       
let cartesianCLI () =
	let reader = (fun s sz -> let tmp = input_char stdin in s.[0] <- tmp.[0]; 1) in
	let lexbuf = Lexing.from_function reader in

	print_string "\tcartesian system v0.1";
	print_newline();
	print_newline();
  
	set_prompt "> ";
  
	while true do  	
		let tree = CliSyntax.phrase CliLex.lexem lexbuf in
    
		print_string (CliTree.tree2string tree);
		print_newline();
	done;
;;


open Debug

let _ = 
	while true do 
		let inputString = read_line() in
		let buffer = Lexing.from_string inputString in
		let tree = Syntax.phrase Lex.lexer buffer in
		ignore 0
    done
;;

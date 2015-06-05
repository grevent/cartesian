
open CartesianTree
open BasicTools

let rec generateString = function
    (FUNCTION x) -> "lambda"^" "^(listIterator (fun (patterns,expr) -> (listIterator generateString " " patterns)^" -> "^(generateString expr)) " | " x)
  | ACTIONEXPRESSION x -> (generateString x)
;;

let fromString str = 
	let buffer = Lexing.from_string str in
	Syntax.phrase Lex.lexer buffer
;;
	

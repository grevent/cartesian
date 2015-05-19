
open CartesianTree
open BasicTools

let rec generateString = function
(ACTIONS x) -> (listIterator generateString "; " x)
  | (FUNCTION x) -> "lambda"^" "^(listIterator (fun (patterns,expr) -> (listIterator generateString " " patterns)^" -> "^(generateString expr)) " | " x)
  | ACTIONEXPRESSION x -> "{ "^(listIterator generateString "; " x)^"} "
;;

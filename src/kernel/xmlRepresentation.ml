
open CartesianTree
open BasicTools
  
let rec generateXml = function
    (FUNCTION x) -> "<functionObject>"^(listIterator (fun (patterns,expr) -> (listIterator generateXml "" patterns)^(generateXml expr)) "" x)^"</functionObject>"
  | ACTIONEXPRESSION x -> "<actionExpressionObject>"^(generateXml x)^"</actionExpressionObject>"
;;


open CartesianTree
open BasicTools
  
let rec generateXml = function
(ACTIONS x) -> "<actionsObject>"^(listIterator generateXml "" x)^"</actionsObject>"
  | (FUNCTION x) -> "<functionObject>"^(listIterator (fun (patterns,expr) -> (listIterator generateXml "" patterns)^(generateXml expr)) "" x)^"</functionObject>"
  | ACTIONEXPRESSION x -> "<actionExpressionObject>"^(listIterator generateXml "" x)^"</actionExpressionObject>"
;;

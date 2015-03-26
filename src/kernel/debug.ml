
let debugOn = true;;
let lexDebugOn = false;;
let synDebugOn = false;;
let stdDebugOn = true;;
let debugMethods = [];;
  (* ["eval"; "preEval"; "exec"; "preExec"; "removeLevel"; "addLevel"; "apply"; "matchToExpression"; "add"];; *)

let genericDebug str = 
  if debugOn then
    begin
      Printf.eprintf "DBG: %s" str;
      prerr_newline();
    end;
;;

let lexDebug str = 
  if lexDebugOn then
    begin
      Printf.eprintf "LEX: %s" str;
      prerr_newline();
    end;
;;

let synDebug str = 
  if synDebugOn then
    begin
      Printf.eprintf "SYN: %s" str;
      prerr_newline();
    end;
;;

let stdDebug objStr mthd inout objXml = 
  if stdDebugOn then
    if (List.exists (fun x -> (String.compare x mthd) == 0) debugMethods) || ((List.length debugMethods) == 0) then
      begin
	Printf.eprintf "%s: %s %s %s" mthd objStr inout objXml;
	prerr_newline();
      end;
;;

  

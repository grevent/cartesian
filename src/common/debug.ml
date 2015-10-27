
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

let debugStartMethod obj mthd =
  if stdDebugOn then
    if (List.exists (fun x -> (String.compare x mthd) == 0) debugMethods) || ((List.length debugMethods) == 0) then
      begin
	Printf.eprintf ">%s: %s" mthd (obj#toString());
	prerr_newline();
      end;
;;

let debugEndMethod obj mthd objR =
  if stdDebugOn then
    if (List.exists (fun x -> (String.compare x mthd) == 0) debugMethods) || ((List.length debugMethods) == 0) then
      begin
	Printf.eprintf "<%s: %s = %s" mthd (obj#toString()) (objR#toString());
	prerr_newline();
      end;
;;

let debugEnd0Method obj mthd =
  if stdDebugOn then
    if (List.exists (fun x -> (String.compare x mthd) == 0) debugMethods) || ((List.length debugMethods) == 0) then
      begin
	Printf.eprintf "<%s: %s" mthd (obj#toString());
	prerr_newline();
      end;
;;

  
let debugStd objName mthd = 
  if stdDebugOn then
    if (List.exists (fun x -> (String.compare x mthd) == 0) debugMethods) || ((List.length debugMethods) == 0) then
      begin
	Printf.eprintf "!%s: %s" mthd objName;
	prerr_newline();
      end;
;;
  

  

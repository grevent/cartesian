
let debugOn = false;;
let lexDebugOn = false;;
let synDebugOn = false;;
let stdDebugOn = false;;
let debugFun = [ "cartesianCLI_local" ];;
let typDebugOn = true;;

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

let typDebug str = 
	if typDebugOn then
		begin
			Printf.eprintf "TYP: %s" str;
			prerr_newline();
		end;
;;

let debugStartFun file mthd =
  if stdDebugOn then
    if (List.exists (fun x -> (String.compare x mthd) == 0) debugFun) || ((List.length debugFun) == 0) then
      begin
		(Printf.eprintf ">: %s/%s" file mthd);
		prerr_newline();
      end;
;;

let debugEndFun file mthd result =
  if stdDebugOn then
    if (List.exists (fun x -> (String.compare x mthd) == 0) debugFun) || ((List.length debugFun) == 0) then
      begin
		(Printf.eprintf "<: %s/%s = %s" file mthd result);
		prerr_newline();
      end;
;;

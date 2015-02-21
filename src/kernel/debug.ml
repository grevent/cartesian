
let debugOn = true;;
let lexDebugOn = false;;
let synDebugOn = false;;
let funDebugOn = false;;
let patternDebugOn = true;;

let genericDebug str = 
  if debugOn then
    begin
      Printf.printf "DBG: %s" str;
      print_newline();
    end;
;;

let lexDebug str = 
  if lexDebugOn then
    begin
      Printf.printf "LEX: %s" str;
      print_newline();
    end;
;;

let synDebug str = 
  if synDebugOn then
    begin
      Printf.printf "SYN: %s" str;
      print_newline();
    end;
;;

let funEvalDebug str =   
  if funDebugOn then
    begin
      Printf.printf "FUN: %s" str;
      print_newline();
    end;
;;

let patternDebug str =   
  if patternDebugOn then
    begin
      Printf.printf "FUN: %s" str;
      print_newline();
    end;
;;


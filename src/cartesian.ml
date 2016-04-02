
(* Cartesian System: Orchestration of all components... *)

open Arg
open Env

let debugLevel = ref 0;;
let files = ref [];;
let defaultGenerator = ref GeneratorXml.generator;;

let argFunctions = [
  ("-debug", Set_int debugLevel, "Debug Mode on/off switch");
];;


let _ = 
	parse argFunctions (fun x -> files:= x::!files) "Usage: cartesian [options] file1 file2 ...";

	let env0 = newEnv() in
	let env = InitLib.standardEnv env0 in
	
	Orchestrator.orchestrator env !defaultGenerator "file.cts" "file.c" 
;;

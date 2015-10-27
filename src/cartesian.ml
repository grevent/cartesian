
(* Cartesian System: Orchestration of all components... *)

let _ = 
	let runtime = Thread.create Runtime.runtime () in
	let cli = Thread.create CartesianCLI.cartesianCLI () in
	Thread.join cli;
	Thread.join runtime;
;;

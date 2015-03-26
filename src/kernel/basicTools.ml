
let array_fold_left2 fn acc0 ar1 ar2 = 
  if (Array.length ar1) == (Array.length ar2) then
    begin
      let acc = ref acc0 in
      
      for i = 0 to (Array.length ar1) - 1 do
	acc := fn !acc ar1.(i) ar2.(i);
      done;
      !acc
    end
  else
    raise (Invalid_argument "BasicTools.array_fold_left2: Arrays not same size");
;;

let compose f g x = (f (g x));;
  
let string_reverse str = 
  let lst = ref [] in
  String.iter (fun c -> lst := (String.make 1 c)::(!lst)) str;
  String.concat "" !lst
;;

let rec functional_for i0 iend istep fn startVl = 
  if (i0 - iend) * istep <= 0 then
    functional_for (i0+istep) iend istep fn (fn i0 startVl)
  else
    startVl
;;

let rec statefullListMap f state0 lst = 
  match lst with
    [] -> (state0,[])
  | el::suite -> 
    let (state1,y) = (f state0 el) in
    let (staten,yList) = (statefullListMap f state1 suite) in
    (staten,(y::yList))
;;

let preEval lambdas env idList = 
  (List.map (fun (patterns,expr) -> 
    let patternIds = List.fold_left (fun acc pattern -> acc@(pattern#getIds())) idList patterns in
    let (newIds,newExpr) = expr#preEval env patternIds in
    (patterns,newExpr) ) lambdas)
;;

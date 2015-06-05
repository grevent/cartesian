
exception DimensionForMatrixMultiplicationNOK
exception DimensionForMatrixAdditionNOK
exception DimensionForMatrixAbsNOK
  
open BasicTools
  
let mul mat1 mat2 =
  Array.init
    (Array.length mat1)
    (fun i ->
     (Array.init
	(Array.length mat2.(0))
	(fun j ->
	 let result = ref (0.0,0.0) in
	 if (Array.length (mat1.(i))) == (Array.length mat2) then
	   for k = 0 to (Array.length mat2) - 1 do
	     result := ComplexTools.add !result (ComplexTools.mul mat1.(i).(k) mat2.(k).(j));
	   done
	 else
	   raise DimensionForMatrixMultiplicationNOK;
	 !result ) ) )
;;

let add mat1 mat2 =
  if (Array.length mat1) == (Array.length mat2) then
    Array.init
      (Array.length mat1)
      (fun i ->
       if ((Array.length mat1.(i)) == (Array.length mat2.(i))) then
	 (Array.init
	    (Array.length mat1.(i))
	    (fun j ->
	     ComplexTools.add mat1.(i).(j) mat2.(i).(j) ) )
       else
	 raise DimensionForMatrixAdditionNOK;)
  else
    raise DimensionForMatrixAdditionNOK;
;;

let sub mat1 mat2 =
  if (Array.length mat1) == (Array.length mat2) then
    Array.init
      (Array.length mat1)
      (fun i ->
       if ((Array.length mat1.(i)) == (Array.length mat2.(i))) then
	 (Array.init
	    (Array.length mat1.(i))
	    (fun j ->
	     ComplexTools.sub mat1.(i).(j) mat2.(i).(j) ) )
       else
	 raise DimensionForMatrixAdditionNOK;)
  else
    raise DimensionForMatrixAdditionNOK;
;;

exception DimensionNotUnique
  
let dim mat =
  let x = Array.length mat in
  let y = Array.length mat.(0) in

  for i = 0 to (Array.length mat) - 1 do
    if (Array.length mat.(0) != y) then
      raise DimensionNotUnique
  done;
  (x,y)
;;
  
let trans mat =
  let (x,y) = dim mat in
  let result =
    Array.init
      y
      (fun i ->
       (Array.init
	  x
	  (fun j ->
	   mat.(j).(i)) ) )
  in
  result
;;

exception AbsOnlyForVectors;;
exception AbsOnlyForRealVectors;;
  
let _abs mat =
  let (x,y) = dim mat in
  if y != 1 then
    raise AbsOnlyForVectors
  else
    begin
      let result = ref 0.0 in
      for i = 0 to x - 1 do
	let (x,y) = mat.(i).(0) in
	if (abs_float y) > 0.0 then
	  raise AbsOnlyForRealVectors
	else
	  result := !result +. x *. x;
      done;
      ((sqrt !result),0.0)
    end;
;;
  
let toString mat =
  let (x,y) = dim mat in
  let result = ref "[|" in
  
  for i = 0 to x - 1 do
    for j = 0 to y - 1 do
      result := !result ^ (ComplexTools.toString mat.(i).(j));
      if j != (y-1) then
	result := !result ^ "; ";
    done;
    if i != (x-1) then
      result := !result ^ " || "
    else
      result := !result ^ " |]";
  done;
  !result
;;

let matrixCompare mat0 mat1 = 
	BasicTools.array2Compare (fun x1 x2 -> (BasicTools.array2Compare (fun (re1,im1) (re2,im2) -> if (re1 == re2) && (im1 == im2) then true else false) x1 x2)) mat0 mat1  
;;

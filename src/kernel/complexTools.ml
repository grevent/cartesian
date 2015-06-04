
let pi = 3.14159265358979323846264338327950288
;;

let add (re1,im1) (re2,im2) =
  (re1 +. re2, im1 +. im2)
;;
    
let sub (re1,im1) (re2,im2) =
  (re1 -. re2, im1 -. im2)
;;

let mul (re1,im1) (re2,im2) =
  (re1 *. re2 -. im1 *. im2 ,
   re1 *. im2 +. im1 *. re2)

let div (re1,im1) (re2,im2) =
  let norm2 = (re2 *. re2 +. im2 *. im2 ) in
  (((re1 *. re2 +. im1 *. im2) /. norm2),
   ((im1 *. re2 -. re1 *. im2) /. norm2) )
;;
    
let norm (re,im) =
  (re *. re +. im *. im,0.0)
;;

let neg (re,im) =
  (0.0 -. re,im)
  
exception ArgNotPossible
	    
let arg (re,im) =
  if re > 0.0 then
    (atan (im /. re))
  else if (re < 0.0) && (im > 0.0) then
    ((atan (im /. re)) +. pi)
  else if (re < 0.0) && (im < 0.0) then
    ((atan (im /. re)) -. pi)
  else
    raise ArgNotPossible
	  
let _abs (re,im) =
  let (x,_) = (norm (re,im)) in
  (sqrt x,0.0)
;;
    
let _ln (re,im) =
  let (absolute,_) = _abs (re,im) in
  
  ((log absolute),
   (arg (re,im)) )
;;

let _log10 x =
  div (_ln x) ((log 10.0),0.0)
;;
  
let _exp (re,im) =
  let e = (exp re) in
  ((e *. cos (im)),(e *. sin (im)))
;;
    
let _expm1 (re,im) =
  let e = (expm1 re) in
  ((e *. cos (im)),(e *. sin (im)))
;;

let _log1p (re,im) =
  _ln (re +. 1.0,im)
;;
    
let _sinh (re,im) =
  div (sub (_exp (re,im))
	   (_exp ((0.0 -. re),(0.0 -. im))) )
      (2.0,0.0) 
;;

let _cosh (re,im) =
  div (add (_exp (re,im))
	   (_exp ((0.0 -. re),(0.0 -. im))) )
      (2.0,0.0) 
;;

let _tanh (re,im) =
  div (_sinh (re,im))
      (_cosh (re,im))
;;
  
let puiss (re1,im1) (re2,im2) =
  _exp (mul (_ln (re1,im1)) (re2,im2))
;;

let _sqrt (re,im) =
  let (absZ,_) = _abs (re,im) in
  let sgn = if im < 0.0 then (-1.0) else 1.0 in
  ((sqrt ((absZ +. re) /. 2.0)),(sgn *. (sqrt ((absZ -. re) /. 2.0))))
;;

  
let _asin (re,im) =
  (mul (0.0,-1.0)
	(_ln (add ((0.0 -. im),re)
		   (_sqrt (sub (1.0,0.0)
				(mul (re,im) (re,im)) ) ) ) ) )
  
let _acos (re,im) =
  (sub (pi /. 2.0, 0.0) (_asin (re,im)))
;;

let _atan(re,im) =
  mul (0.0,-0.5)
      (sub (_ln (sub (1.0,0.0) (0.0 -. im,re)))
	   (_ln (add (1.0,0.0) (0.0 -. im,re))) )
;;

let _sin (re,im) =
  ((sin re) *. (cosh im),(cos re) *. (sinh im))
;;

let _cos (re,im) =
  ((cos re) *. (cosh im),0.0 -. (sin re) *. (sinh im))
;;

let _tan x =
  (div (_sin x) (_cos x))
;;

let parse st =
  try
    Scanf.sscanf st "( %f , %f )" (fun re im -> (re,im))
  with
    _ -> Scanf.sscanf st "%f" (fun x -> (x,0.0))
;;

let toString (re,im) =
  if (abs_float im) > 0.0 then
    (Printf.sprintf "(%f,%f)" re im)
  else
    (Printf.sprintf "%f" re);

  
    

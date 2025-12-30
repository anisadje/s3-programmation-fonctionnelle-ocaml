let rec pgcd a b = 
  if b = 0 then a
  else pgcd b (a mod b)

(*oui, récursif terminal car la récursion à la dernière ligne*)

let () = Printf.printf "%d\n" (pgcd 10 5)

type frac = {
  num : int 
  denom : int
}

let sign i = 
  if i > 0 then 1 
  else if i = 0 then 0 
  else -1

let simp f = 
  let d = pgcd (abs f.num) (abs f.denom) in 
  let n = f.num / d in 
  let m = f.denom / d in 

  if m < 0 then {num = -n; denom = -m}
  else {num = n; denom = m}

let () = 
  let f1 = {num = 8; denom = 12} in 
  let f2 = simp f1 in 
  Printf.printf "%d/%d\n" f2.num f2.denom 

let add_frac f1 f2 = 
  (*formule générale : a/b + c/d = (a*d + c*b)/(b*d)*)
  let num = f1.num * f2.denom + f2.num * f1.denom in 
  let denom = f1.denom * f2.denom in 

  simp {num; denom}

let () = 
  let f1 = { num = 1; denom = 2 } in 
  let f2 = { num = 1; denom = 3 } in 
  let f3 = add_frac f1 f2 in 

  Printf.printf "%d/%d\n" f3.num f3.denom

let neg_frac f = 
  { num = -f.num ;denom = f.denom }

let sub_frac f1 f2 = 
  let num = f1.num * f2.denom - f2.num * f1.denom in 
  let denom = f1.denom * f2.denom in 

  simp {num; denom}

let () = 
  let f1 = { num = 1; denom = 2 } in 
  let f2 = { num = 1; denom = 3 } in 
  let f3 = sub_frac f1 f2 in 

  Printf.printf "%d/%d\n" f3.num f3.denom

let mul_frac f1 f2 = 
  let num = f1.num * f2.num in 
  let denom = f1.denom * f2.denom in 

  simp {num; denom}

let inv_frac f =
  if f.num = 0 then failwith "Impossible d'inverser une fraction nulle"
  { num = f.denom; denom = f.num }

let div_frac f1 f2 = 
  let num = f1.num * f2.denom in 
  let denom = f1.denom * f2.num in 

  simp {num; denom}

let string_of_frac f = 
  string_of_int f.num ^ "/" ^ string_of_int f.denom 

let float_of_frac f = 
  (float_of_int f.num) /. (float_of_int f.denom)

(*ici, num peut avoir 3 types : int, float ou frac*)
type num = 
  Int of int 
  | Float of float 
  | Frac of frac 

(*Donc, on doit convertir en fonction du type de num*)

let string_of_num num = 
  match n with 
  | Int i -> string_of_int i 
  | Float f -> string_of_float f 
  | Frac fr -> string_of_frac fr 

(*On aurait pu faire avec Printf.sprintf 

let string_of_num n = 
  match n with 
  | Int i -> Printf.sprintf "%d" i 
  | Float f -> Printf.sprintf "%.3f" f (= 3 décimales)
  | Frac fr -> string_of_frac fr 

*)

let exec_op n1 n2 op_i op_fr op_fl = 
  match n1, n2 with 
  | Float fl1, Float fl2 -> Float (op_fl fl1 fl2)
  | Float fl1, Frac fr2 -> Float (op_fl fl1 (float_of_frac fr2))
  | Float fl1, Int i2 -> Float (op_fl fl1 (float_of_int i2))
  | Int i1, Int i2 -> Int (op_i i1 i2)
  | Int i1, Frac fr2 -> Frac (op_fr (frac_of_int i1) fr2)
  | Int i1, Float fl2 -> Float (op_fl (float_of_int i1) fl2)
  | Frac fr1, Int i2 -> Frac (op_fr fr1 (frac_of_int i2))
  | Frac fr1, Float fl2 -> Float (op_fl (float_of_frac fr1) fl2)
  | Frac fr1, Frac fr2 -> Frac (op_fr fr1 fr2)
  
let add_num n1 n2 = exec_op n1 n2 ( + ) add_frac ( +. )
let sub_num n1 n2 = exec_op n1 n2 ( - ) sub_frac ( -. )
let mul_num n1 n2 = exec_op n1 n2 ( * ) mul_frac ( *. )
let div_num n1 n2 = exec_op n1 n2 ( / ) div_frac ( /. )

let rec pow n k = 
  if k = 0 then 1 
  else 
    n* pow n (k-1)


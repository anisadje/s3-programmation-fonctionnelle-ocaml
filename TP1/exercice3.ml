let rec somme_entiers n = 
  if n <= 0 then 0 else n + somme_entiers (n-1)

let () = Printf.printf "La somme des entiers de 5 est %d \n" (somme_entiers 5)
let () = Printf.printf "La somme des entiers de 10 est %d \n" (somme_entiers 10)

let rec somme_carres n = 
  if n <= 0 then 0 else n*n + somme_carres (n-1)

let () = Printf.printf "La sommes des carrés de 5 est %d \n" (somme_carres 5)

let rec leibniz n = 
  if n = 0 then 1.0
  else 
    let v = if n mod 2 == 0 then 1.0 else -1.0 in 
    v *. (1.0/. float(2*n+1)) +. leibniz(n-1)

(* cette fonction calcul pi/4 avec la série de leibniz, 
il faut donc pour avoir pi *. 4*)

let () = Printf.printf "La formule de Leibniz 10 donne %f \n" (4. *. leibniz 10)
let rec longueur lst =
  match lst with 
  | [] -> 0 
  | x :: xs -> 1 + longueur xs (*pour que ce soit récursif, on doit séparer l'en-tête*)

(*en réalité cette fonction revient à List.Length*)
let () = 
  let res = longueur [2; 3; 4; 5] in 
  Printf.printf "La longueur de la liste est %d \n" res;;

let compter_element_tail lst = 
  let rec auxiliaire acc lst = 
    match lst with 
    | [] -> acc 
    | x :: xs -> auxiliaire (acc + 1) xs 
  in 
  auxiliaire 0 lst


let () = 
  let res = compter_element_tail [2;3;4;5] in 
  Printf.printf "La liste contient %d éléments\n " res;;
let rec min_max lst = 
  match lst with 
  | [x] -> (x, x)
  | x :: xs ->
    let (min_rest, max_rest) = min_max xs in 
    let min_final = if x < min_rest then x else min_rest in 
    let max_final = if x > max_rest then x else max_rest in 
    (min_final, max_final)

let () =
  let (min, max) = min_max [3; 1; 4; 2] in 
  Printf.printf "min = %d, max = %d\n" min max;;

let min_max_tail lst = 
  let rec auxiliaire accumulateur lst = 
    match lst with 
    | [] -> accumulateur 
    | x :: xs -> 
      let (min, max) = accumulateur in
      let min_final = if x < min then x else min 
      let max_final = if x > max then x else max 
      auxiliaire (min_final, max_final) xs
    in 
  let first = List.hd lst (*premier élément -> min et max initiaux*)
  let rest = Lst.tl lst (*reste de la liste à parcourir*)
  auxiliaire (first, first) rest
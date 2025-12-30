let rec hanoi_aux dep aux dest n = 
  if n > 0 then begin 
    hanoi_aux dep dest aux (n-1);
    Printf.printf "%s -> %s \n" dep dest ;
    hanoi_aux aux dep dest (n-1)
  end 


let hanoi n = 
  hanoi_aux "A" "B" "C" n 

let () = hanoi 3

(*Sys.time retourne un float, le nb de secondes écoulées depuis le début*)

let () = 
  let n = 40 in 
  let start = Sys.time () in 
  hanoi n;
  let stop = Sys.time () in 
  Printf.printf "Temps pour n=%d : %f secondes\n" n (stop -. start) 
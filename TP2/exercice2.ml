let rec guess n i = 
  let v = read_int() in
  if v = n then Printf.printf "Trouvé ! Avec %d essais \n" i
  else begin
    if v < n then Printf.printf "Trop petit ! \n" 
    else Printf.printf "Trop grand ! \n";
    guess n (i+1)
  end

let () =
  Random.self_init ();
  let n = 1 + Random.int(100) in
  let i = 0 in 
  guess n i






let bissextile a = 
  (a mod 4 = 0 && a mod 100 <> 0) || (a mod 400 = 0)

let () = Printf.printf "2024 est-elle bissextile ? %b\n" (bissextile 2024)

let jour_mois m a = 
  if m = 2 then if bissextile a then 29 else 28
  else if m <= 7 then if m mod 2 = 1 then 31 else 30
  else if m mod 2 = 0 then 31 else 30

let () = 
  Printf.printf "Le nombre de jour le 02/2024 = %d jours\n" (jour_mois 2 2024);
  Printf.printf "Le nombre de jour le 03/2025 = %d jours\n" (jour_mois 3 2025)

let rec jour_date j m a = 
  if m = 1 then j
  else j + jour_date (jour_mois (m-1) a) (m-1) a

let () =
  Printf.printf "Le nombre de jour à la date 27/03/1980 = %d\n" (jour_date 27 3 1980);
  Printf.printf "Le nombre de jour à la date 19/01/2001 = %d\n" (jour_date 19 1 2001)

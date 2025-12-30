let hanoi n = 
  let rec hanoi_aux dep mil arr n = 
    if n>0 then begin 
      hanoi_aux dep arr mil (n-1);
      Printf.printf "%s -> %s\n" dep arr;
      hanoi_aux mil dep arr (n-1)
    end
  in
  hanoi_aux "dep" "mil" "arr" n 

let () =  hanoi 4

type piquet = string * int list 
type jeu = piquet list 

let affiche_piquet (nom, disques : piquet) = 
  Printf.printf "%s|" nom;
  List.iter (fun x -> Printf.printf "%d-" x) (List.rev disques );
  print_newline()

let p1 = ("A", [3;2;1]);;
affiche_piquet p1;;

let choix_piquet (piquets : jeu) m = 
  List.find (fun (nom, disques) -> nom = m) piquets

let piquets = [ ("dep", [2;3;4]);
                ("mid", []);
                ("arr", [1])]
let m = choix_piquet piquets "mid"

let affiche_jeu jeu = 
  Printf.printf "\x1b[2J\x1b[H";
  affiche_piquet (choix_piquet jeu "dep");
  affiche_piquet (choix_piquet jeu "mid");
  affiche_piquet (choix_piquet jeu "arr");

  Printf.printf "%!";

let deplace_sommet ((nom1, d1) : piquet) ((nom2, d2) : piquet) = 
  match d1, d2 with 
  | [], _ -> 
    failwith "Le premier piquet est vide"
  | x::_, y::_ when x > y -> 
    failwith "Deplacement interdit"
  | x::xs, _ -> 
    ((nom1, xs), (nom2, x::d2))

let joue piquets src dst autre = 
  let p_src = choix_piquet piquets src in 
  let p_dst = choix_piquet piquets dst in 

  let (p_src', p_dst') = deplace_sommet p_src p_dst in 

  List.map (fun (nom, d) -> (*List.map sert à parcourir une liste élément par 
  élément et à construire une nouvelle liste en transformant chaque élément*)
    if nom = src then p_src' (*si le nom du piquet est celui de la source, alors remplace le par un nv piquet*)
    else if nom = dst then p_dst' (*si le nom du piquet est celui de la destination, alors remplace-le par le nouveau piquet destination*)
    else (nom, d) (*sinon, c'est un autre piquet, on ne change rien*)
    ) piquets 

(* let gen_list n = List.init n (fun i -> i + 1)*)

(*ou sinon à la main*)

let gen_list n = 
  if n = 0 then []
  else gen_list (n-1) @ [n] (*@ est la concaténationd de liste*)

let hanoi_list n = 
  let rec hanoi_aux piquets dep mil arr n = 
    if n>0 then begin 
      let piquets = hanoi_aux piquets dep arr mil (n-1) in 
      let piquets = joue piquets dep arr mil in 

      affiche_jeu piquets; 
      Unix.spleepf 0.5; 

      hanoi_aux piquets mil dep arr (n-1)

    end else 
      piquets




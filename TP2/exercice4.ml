let rec affiche_tab i a = 
  if Array.length a > i then begin Printf.printf "    %s\n" a.(i);
  affiche_tab (i+1) a
  end 

let affiche_tab_aux a = 
  affiche_tab 0 a 

let rec affiche_dir d = 
  if Sys.is_directory d then begin (*vérifie si d est un répertoire*)
    Printf.printf "%s : \n" d; 
  let read = Sys.readdir d in (*lit le contenu du répertoire d*)
  affiche_tab_aux read; 
  Sys.chdir d; (*change le dépetoire courant pour d*)
  for i = 0 to Array.length read - 1 do 
    affiche_dir read.(i) (*affiche chaque élément du tableau read*)
  done; 
  Sys.chdir ".." (*remonte d'un niveau dans l'arborescence après avoir fini le traitement du répertoire*)
end 


let () = 
  affiche_dir "." (*le répertoir courant*)


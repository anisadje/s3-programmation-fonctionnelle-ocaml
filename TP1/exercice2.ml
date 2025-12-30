let () = 
  if Array.length Sys.argv < 2 then Printf.printf "Pas assez d'arguments\n"
  else
    if not (Sys.file_exists Sys.argv.(1)) then Printf.printf "Le fichier n'existe pas \n"
    else 
      if Sys.is_directory Sys.argv.(1) then Printf.printf "Le répertoire existe \n"
      else Printf.printf "Le fichier existe \n"

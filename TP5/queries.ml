type movie = { 
    id : int;
    title : string;
    year : int;
    runtime : int;
    rank : int
}

type res = Movie of movie | Invalid | Eof

let input_movie in_c =
  try
    let s = input_line in_c in
    match String.split_on_char ';' s with
    [ s_id; title; s_year; s_runtime ; s_rank ] ->
       Movie ({
        id = int_of_string s_id;
        title = title;
        year = int_of_string s_year;
        runtime = int_of_string s_runtime;
        rank = int_of_string s_rank;
      })
    | _ -> Invalid

  with
   End_of_file -> Eof
  | _ -> Invalid
;;

let load_movies f =
  let in_c = open_in f in
  let rec loop in_c acc =
    match input_movie in_c with
    | Eof -> acc
    | Invalid -> loop in_c acc
    | Movie m -> loop in_c (m :: acc)
  in
    let res = loop in_c [] in
    close_in in_c;
    res
;;

let movies = load_movies "movies.csv"
;;

let pr_movie m = 
  Printf.printf "{ id= %d; title= %s; year= %d; runtime= %d; rank= %d}\n" m.id m.title m.year m.runtime m.rank;;

let pr_movies ms = List.iter pr_movie ms;;

let moviesTop10 ml = 
  List.filter ( fun m -> m.rank <= 10 ) ml 

let () = pr_movies (moviesTop10 movies);;

let movies1980 ml = 
  List.filter (fun m -> m.year >= 1980 && m.year <= 1989)

let movie_titles ml = 
  List.map (fun m -> m.title) ml;;

let pr_titles titles =
  List.iter (fun t -> Printf.printf "%s\n" t) titles;;

let () = pr_titles (movie_titles movies);;

let max_id ml = 
  List.fold_left (fun acc m -> if m.id > acc then m.id else acc) 0 ml;;

let () = Printf.printf "L'id max est %d\n" (max_id movies)

let average_runtime ml = 
  let (total, count) = 
    List.fold_left (fun (sum, n) m -> (sum + m.runtime, n+1)) (0, 0) ml
  in 
  float_of_int total /. float_of_int count

let () = Printf.printf "Durée moyenne : %.2f minutes \n" (average_runtime movies);;

let average_by_year ml = 
  let sorted = List.sort (fun m1 m2 -> compare m1.year m2.year) ml in 

  let rec loop films acc = 
    match films with 
    | [] -> acc (*liste vide -> renvoyer l'accumulateur *)
    | f :: rest -> 
      match acc with 
      | [] -> (*accumulateur vide -> créer une nouvelle paire*)
        loop rest [(f.year), [f]]
      | (y, flist) :: t -> 
        if f.year = y then 
          (*même année -> ajouter le film à la liste*)
          loop rest ((y, f :: flist) :: t)
        else 
          loop rest ((f.year, [f]) :: acc)
  in
   (* appeler loop sur la liste triée, résultat inversé pour l’ordre croissant *)
  let grouped = List.rev (loop sorted []) in

  (* calculer la moyenne pour chaque année *)
  List.map (fun (y, flist) -> (y, average_runtime flist)) grouped
;;

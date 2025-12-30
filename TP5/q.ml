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

let pr_movie movie = 
  Printf.printf "{ id=%d; title=%s; year=%d; runtime=%d; rank=%d }\n" movie.id movie.title movie.year movie.runtime movie.rank;;

let pr_movies ms = 
  List.iter (fun m -> pr_movie m) ms 

let movieTop10 ms = 
  pr_movies (List.filter(fun m -> m.rank <= 10) ms)

(*let () = pr_movies movies*)

(*let () = movieTop10 movies*)

let movies1980 ms = 
  pr_movies (List.filter(fun m -> m.year >= 1980 && m.year <= 1989) ms)

(*let () = movies1980 movies*)

let movie_titles ms = 
  List.iter(fun m -> Printf.printf "Titre = %s\n" m.title) ms 

(*let () = movie_titles movies*)

let max_id ms = 
  Printf.printf "Id max :%d\n" (List.fold_left (fun acc m -> if m.id > acc then m.id else acc) 0 ms)

let () = max_id movies 

let average_runtime ms = 
  let (total, count) = 
    List.fold_left (fun (somme, n) m -> (somme + m.runtime, n+1))
    (0, 0)
    ms 
  in
  if count = 0 then 0.0 
  else float_of_int total /. float_of_int count 
  
let () = Printf.printf "Temps moyen= %2.f minutes \n" (average_runtime movies)


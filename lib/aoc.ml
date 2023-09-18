let read_lines () =
  let day =
    Sys.argv.(0)
    |> Str.split (Str.regexp Filename.dir_sep)
    |> List.rev |> List.hd
  in
  let is_test = Array.exists (fun x -> x = "test") Sys.argv in
  let chan =
    open_in
      (Printf.sprintf "data/%s/%s.txt" day (if is_test then "test" else "in"))
  in
  let rec read acc =
    try read (input_line chan :: acc) with End_of_file -> List.rev acc
  in
  read []
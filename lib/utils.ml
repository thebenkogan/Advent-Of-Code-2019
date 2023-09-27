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
  let lines = read [] in
  close_in chan;
  lines

let string_to_chars s = List.init (String.length s) (String.get s)

let list_to_string printer lst =
  "[" ^ String.concat ", " (List.map printer lst) ^ "]"

let chunk chunk_size elements =
  let _, first, chunked =
    List.fold_right
      (fun elt (size, curr, acc) ->
        if size = chunk_size then (1, [ elt ], curr :: acc)
        else (size + 1, elt :: curr, acc))
      elements (0, [], [])
  in
  first :: chunked

let permutations elements =
  let len = List.length elements in
  let gen_opts acc =
    elements
    |> List.filter (fun opt -> not (List.mem opt acc))
    |> List.map (fun opt -> opt :: acc)
  in
  let rec build size accs =
    if size = len then accs
    else
      accs
      |> List.map (fun acc -> gen_opts acc)
      |> List.flatten
      |> build (size + 1)
  in
  build 0 [ [] ]

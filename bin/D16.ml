open AOC.Utils

let lines = read_lines ()

let nums =
  lines |> List.hd |> string_to_chars |> List.map (fun c -> int_of_char c - 48)

let num_digits = List.length nums
let base = [ 0; 1; 0; -1 ]

let rec repeat times lst =
  let rec repeat_elt n elt =
    if n = times then [] else elt :: repeat_elt (n + 1) elt
  in
  lst |> List.map (repeat_elt 0) |> List.flatten

let loop_lst len lst =
  let start_len = List.length lst in
  if start_len > len then List.filteri (fun i _ -> i < len) lst
  else
    let times = (len - start_len) / start_len in
    let rec build acc n =
      if n = times then acc else build (lst @ acc) (n + 1)
    in
    let repeated = build lst 0 in
    repeated @ List.filteri (fun i _ -> i < (len - start_len) mod start_len) lst

let loops =
  nums
  |> List.mapi (fun i _ ->
         base |> repeat (i + 1) |> loop_lst (num_digits + 1) |> List.tl)

let phase nums =
  List.combine nums loops
  |> List.map (fun (_, loop) ->
         List.combine nums loop
         |> List.map (fun (a, b) -> a * b)
         |> List.fold_left ( + ) 0)
  |> List.map (fun n -> abs (n mod 10))

let arr_to_num arr =
  arr |> List.map string_of_int |> List.fold_left ( ^ ) "" |> int_of_string

let p1 =
  let rec loop acc n = if n = 100 then acc else loop (phase acc) (n + 1) in
  loop nums 0 |> List.filteri (fun i _ -> i < 8) |> arr_to_num

let nums_cut =
  let skip = nums |> List.filteri (fun i _ -> i < 7) |> arr_to_num in
  let rec repeat_lst n = if n = 10000 then [] else nums @ repeat_lst (n + 1) in
  repeat_lst 0 |> List.filteri (fun i _ -> i >= skip)

let phase_hack nums =
  let rec build acc last = function
    | [] -> acc
    | h :: t ->
        let next = (h + last) mod 10 in
        build (next :: acc) next t
  in
  build [] 0 (List.rev nums)

let p2 =
  let rec loop acc n = if n = 100 then acc else loop (phase_hack acc) (n + 1) in
  loop nums_cut 0 |> List.filteri (fun i _ -> i < 8) |> arr_to_num

let () = Printf.printf "Part 1: %d\nPart 2: %d" p1 p2

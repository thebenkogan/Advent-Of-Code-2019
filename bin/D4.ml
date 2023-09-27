open AOC.Utils

let low, high =
  let split =
    read_lines () |> List.hd |> String.split_on_char '-'
    |> List.map int_of_string
  in
  (List.nth split 0, List.nth split 1)

let range =
  let rec loop n acc = if n < low then acc else loop (n - 1) (n :: acc) in
  loop high []

let rec has_adjacent = function
  | [] | _ :: [] -> false
  | a :: b :: t -> if a = b then true else has_adjacent (b :: t)

let rec is_non_decreasing = function
  | [] | _ :: [] -> true
  | a :: b :: t -> if a > b then false else is_non_decreasing (b :: t)

let p1 =
  List.fold_left
    (fun acc n ->
      let cs = n |> string_of_int |> string_to_chars in
      if has_adjacent cs && is_non_decreasing cs then acc + 1 else acc)
    0 range

let has_adjacent_only_2 cs =
  let rec has_only_2_repeat prev = function
    | [] | _ :: [] -> false
    | [ a; b ] -> a = b && a <> prev
    | a :: b :: c :: t ->
        if prev <> a && a = b && b <> c then true
        else has_only_2_repeat a (b :: c :: t)
  in
  has_only_2_repeat 'a' cs

let p2 =
  List.fold_left
    (fun acc n ->
      let cs = n |> string_of_int |> string_to_chars in
      if has_adjacent_only_2 cs && is_non_decreasing cs then acc + 1 else acc)
    0 range

let () = Printf.printf "Part 1: %d\nPart 2: %d" p1 p2
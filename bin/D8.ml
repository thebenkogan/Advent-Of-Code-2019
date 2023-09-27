open AOC.Utils

let lines = read_lines ()

let layers =
  lines |> List.hd |> string_to_chars
  |> List.map (fun c -> int_of_char c - 48)
  |> chunk (25 * 6)

let rec count_digit num = function
  | [] -> 0
  | h :: t -> if h = num then 1 + count_digit num t else count_digit num t

let p1 =
  layers
  |> List.fold_left
       (fun (best_layer, num_zeros) layer ->
         let count = count_digit 0 layer in
         if count < num_zeros then (layer, count) else (best_layer, num_zeros))
       ([], max_int)
  |> fst
  |> fun layer -> count_digit 1 layer * count_digit 2 layer

let () = Printf.printf "Part 1: %d\n" p1

let start_output =
  let rec loop size acc =
    if size = 25 * 6 then acc else loop (size + 1) (2 :: acc)
  in
  loop 0 []

let () =
  layers
  |> List.fold_left
       (fun output layer ->
         List.combine output layer
         |> List.map (fun (existing_pixel, next_pixel) ->
                if existing_pixel = 2 then next_pixel else existing_pixel))
       start_output
  |> chunk 25
  |> List.map (fun row -> row |> List.map string_of_int |> String.concat "")
  |> List.iter print_endline

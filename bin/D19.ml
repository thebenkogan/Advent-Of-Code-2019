open AOC
open Utils

let lines = read_lines ()
let state () = lines |> Intcode.read_mem |> Intcode.initial_state
let is_pulled x y = Intcode.run (ref [ x; y ]) (state ()) |> fst |> ( = ) 1

let rec scan_grid x y acc =
  if x = 49 && y = 49 then acc
  else
    let new_acc = if is_pulled x y then acc + 1 else acc in
    if y = 49 then scan_grid (x + 1) 0 new_acc else scan_grid x (y + 1) new_acc

let p1 = scan_grid 0 0 0

let square_fits sx sy =
  is_pulled sx sy && is_pulled (sx + 99) sy && is_pulled sx (sy + 99)

let square_on_row y =
  let rec scan x prev =
    let pulled = is_pulled x y in
    if (not pulled) && prev then None
    else if pulled && square_fits x y then Some (x, y)
    else scan (x + 1) pulled
  in
  scan 0 false

let rec bin_search_to_row lo hi =
  if hi - lo = 1 then hi
  else
    let mid = (lo + hi) / 2 in
    match square_on_row mid with
    | Some _ -> bin_search_to_row lo mid
    | None -> bin_search_to_row mid hi

let p2 =
  (bin_search_to_row 5 5000 |> square_on_row |> function
   | Some p -> p
   | None -> failwith "bad")
  |> fun (x, y) -> (x * 10_000) + y

let () = Printf.printf "Part 1: %d\nPart 2: %d" p1 p2

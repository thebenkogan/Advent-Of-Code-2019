open AOC.Utils

let lines = read_lines ()

module Coord = struct
  type t = int * int

  let compare = compare
end

module CoordSet = Set.Make (Coord)
module CoordMap = Map.Make (Coord)
module StringSet = Set.Make (String)

let initial_bugs =
  let char_lines = List.map string_to_chars lines in
  char_lines
  |> List.mapi (fun y line ->
         List.mapi (fun x c -> if c = '#' then Some (x, y) else None) line)
  |> List.flatten |> List.filter_map Fun.id |> CoordSet.of_list

let dirs = [ (0, 1); (0, -1); (1, 0); (-1, 0) ]
let in_bounds (x, y) = x >= 0 && x < 5 && y >= 0 && y < 5

let step bugs =
  let empty_counts = ref CoordMap.empty in
  let surviving_bugs =
    CoordSet.filter_map
      (fun (x, y) ->
        let bugs, empties =
          dirs
          |> List.map (fun (dx, dy) -> (x + dx, y + dy))
          |> List.filter in_bounds
          |> List.partition (fun c -> CoordSet.mem c bugs)
        in
        List.iter
          (fun c ->
            empty_counts :=
              CoordMap.update c
                (function Some n -> Some (n + 1) | None -> Some 1)
                !empty_counts)
          empties;
        if List.length bugs = 1 then Some (x, y) else None)
      bugs
  in
  CoordMap.fold
    (fun c n acc -> if n = 1 || n = 2 then CoordSet.add c acc else acc)
    !empty_counts surviving_bugs

let hash bugs =
  bugs |> CoordSet.to_seq |> List.of_seq
  |> list_to_string (pair_to_string string_of_int)

let rec step_til_repeat seen bugs =
  let next_bugs = step bugs in
  let bugs_hash = hash next_bugs in
  if StringSet.mem bugs_hash seen then next_bugs
  else step_til_repeat (StringSet.add bugs_hash seen) next_bugs

let biodiversity bugs =
  CoordSet.fold
    (fun (x, y) acc ->
      let rating = 2. ** ((5 * y) + x |> float_of_int) |> int_of_float in
      acc + rating)
    bugs 0

let p1 = initial_bugs |> step_til_repeat StringSet.empty |> biodiversity

module IntMap = Map.Make (Int)

let initial_levels =
  IntMap.singleton 0 initial_bugs
  |> IntMap.add 1 CoordSet.empty
  |> IntMap.add (-1) CoordSet.empty

let inner_neighbors = function
  | 1, 0 -> [ (4, 0); (4, 1); (4, 2); (4, 3); (4, 4) ]
  | -1, 0 -> [ (0, 0); (0, 1); (0, 2); (0, 3); (0, 4) ]
  | 0, 1 -> [ (0, 4); (1, 4); (2, 4); (3, 4); (4, 4) ]
  | _ -> [ (0, 0); (1, 0); (2, 0); (3, 0); (4, 0) ]

let neighbors depth (x, y) =
  dirs
  |> List.map (fun (dx, dy) -> (x + dx, y + dy))
  |> List.map (fun (nx, ny) ->
         if (nx, ny) = (2, 2) then
           (x - nx, y - ny)
           |> inner_neighbors
           |> List.map (fun c -> (c, depth + 1))
         else if in_bounds (nx, ny) then [ ((nx, ny), depth) ]
         else if nx < 0 then [ ((1, 2), depth - 1) ]
         else if nx >= 5 then [ ((3, 2), depth - 1) ]
         else if ny < 0 then [ ((2, 1), depth - 1) ]
         else [ ((2, 3), depth - 1) ])
  |> List.flatten

let all_coords =
  let rec loop (x, y) acc =
    let new_acc = (x, y) :: acc in
    if (x, y) = (4, 4) then new_acc
    else if x = 4 then loop (0, y + 1) new_acc
    else loop (x + 1, y) new_acc
  in
  loop (0, 0) [] |> List.filter (( <> ) (2, 2))

let step2 levels depth =
  let bugs = IntMap.find depth levels in
  all_coords
  |> List.filter_map (fun c ->
         let ns =
           neighbors depth c
           |> List.filter (fun (c, d) ->
                  let nbugs = IntMap.find d levels in
                  CoordSet.mem c nbugs)
           |> List.length
         in
         if CoordSet.mem c bugs then if ns = 1 then Some c else None
         else if ns = 1 || ns = 2 then Some c
         else None)
  |> CoordSet.of_list

let step_levels levels =
  let min_depth = levels |> IntMap.min_binding |> fst in
  let max_depth = levels |> IntMap.max_binding |> fst in
  let levels =
    levels
    |> IntMap.add (min_depth - 1) CoordSet.empty
    |> IntMap.add (max_depth + 1) CoordSet.empty
  in
  IntMap.mapi
    (fun d bugs ->
      if d > min_depth - 1 && d < max_depth + 1 then step2 levels d else bugs)
    levels

let count_bugs levels =
  IntMap.fold (fun _ cs acc -> acc + CoordSet.cardinal cs) levels 0

let p2 =
  let rec loop levels i =
    if i = 200 then levels else loop (step_levels levels) (i + 1)
  in
  let result = loop initial_levels 0 in
  count_bugs result

let () = Printf.printf "Part 1: %d\nPart 2: %d" p1 p2

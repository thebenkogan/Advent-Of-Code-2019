open AOC.Utils

let lines = read_lines ()

type dir = Up of int | Right of int | Left of int | Down of int

let dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let parse_dirs input =
  input
  |> Str.split (Str.regexp ",")
  |> List.map (fun s ->
         let amount = Str.string_after s 1 |> int_of_string in
         match s.[0] with
         | 'R' -> Right amount
         | 'L' -> Left amount
         | 'D' -> Down amount
         | 'U' -> Up amount
         | _ -> failwith "unknown direction")

module Coord = struct
  type t = int * int

  let compare = compare
end

module CoordSet = Set.Make (Coord)

let rec walk_line (x, y) (dx, dy) amount acc =
  if amount = 0 then acc
  else
    let next_coord = (x + dx, y + dy) in
    walk_line next_coord (dx, dy) (amount - 1) (next_coord :: acc)

let rec walk_dirs (x, y) acc = function
  | [] -> List.rev acc
  | h :: t ->
      let dir, amount, next_coord =
        match h with
        | Right amount -> ((1, 0), amount, (x + amount, y))
        | Up amount -> ((0, 1), amount, (x, y + amount))
        | Left amount -> ((-1, 0), amount, (x - amount, y))
        | Down amount -> ((0, -1), amount, (x, y - amount))
      in
      let next_acc = walk_line (x, y) dir amount acc in
      walk_dirs next_coord next_acc t

let coords1, coords2 =
  let dirs1 = List.nth lines 0 |> parse_dirs in
  let dirs2 = List.nth lines 1 |> parse_dirs in
  (walk_dirs (0, 0) [] dirs1, walk_dirs (0, 0) [] dirs2)

let intersections =
  CoordSet.inter (CoordSet.of_list coords1) (CoordSet.of_list coords2)
  |> CoordSet.elements

let p1 = List.map (dist (0, 0)) intersections |> List.fold_left min max_int

let rec get_steps target steps = function
  | [] -> steps
  | h :: t -> if h = target then steps else get_steps target (steps + 1) t

let p2 =
  List.map
    (fun i -> get_steps i 1 coords1 + get_steps i 1 coords2)
    intersections
  |> List.fold_left min max_int

let () = Printf.printf "Part 1: %d\nPart 2: %d" p1 p2
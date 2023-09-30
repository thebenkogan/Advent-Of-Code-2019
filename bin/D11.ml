open AOC
open Utils

let lines = read_lines ()

module Coord = struct
  type t = int * int

  let compare = compare
end

module CoordMap = Map.Make (Coord)

type color = White | Black
type dir = Up | Down | Left | Right

let turn facing dir =
  match (facing, dir) with
  | Up, Left | Down, Right -> Left
  | Up, Right | Down, Left -> Right
  | Left, Left | Right, Right -> Down
  | Left, Right | Right, Left -> Up
  | _ -> failwith "invalid turn"

let painted_hull starting_color =
  let rec loop (x, y) dir cmap state =
    try
      let input =
        match CoordMap.find_opt (x, y) cmap with Some White -> 1 | _ -> 0
      in
      let color_val, state1 = Intcode.run (ref [ input; input ]) state in
      let turn_val, state2 = Intcode.run (ref [ input; input ]) state1 in
      let paint_color = if color_val = 0 then Black else White in
      let next_cmap = CoordMap.add (x, y) paint_color cmap in
      let turn_dir = if turn_val = 0 then Left else Right in
      let next_dir = turn dir turn_dir in
      let next_pos =
        match next_dir with
        | Left -> (x - 1, y)
        | Right -> (x + 1, y)
        | Up -> (x, y + 1)
        | Down -> (x, y - 1)
      in
      loop next_pos next_dir next_cmap state2
    with Intcode.Halt -> cmap
  in
  let cmap = CoordMap.singleton (0, 0) starting_color in
  loop (0, 0) Up cmap (Intcode.read_mem lines |> Intcode.initial_state)

let p1 = CoordMap.cardinal (painted_hull Black)
let () = Printf.printf "Part 1: %d\n" p1

(* turns out all x values are non-negative and all y values are non-positive
   so I will just multiply the y coords by -1 to have valid indices *)
let () =
  let hull = painted_hull White in
  let max_x, max_y =
    CoordMap.fold
      (fun (x, y) _ (best_x, best_y) -> (max x best_x, max (-1 * y) best_y))
      hull (min_int, min_int)
  in
  let grid = Array.make_matrix (max_y + 1) (max_x + 1) "." in
  CoordMap.iter
    (fun (x, y) color -> if color = White then grid.(-1 * y).(x) <- "#")
    hull;
  grid
  |> Array.map (fun row -> row |> Array.to_list |> String.concat "")
  |> Array.iter print_endline

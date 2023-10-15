open AOC
open Utils

let lines = read_lines ()

module Coord = struct
  type t = int * int

  let compare = compare
end

module CoordMap = Map.Make (Coord)

let insert k v map =
  CoordMap.update k
    (function Some elts -> Some (v :: elts) | None -> None)
    map

let neighbors (x, y) = [ (x - 1, y); (x + 1, y); (x, y + 1); (x, y - 1) ]

let adj_map =
  let rec loop graph (x, y) state =
    try
      let output, next_state = Intcode.run (ref []) state in
      match char_of_int output with
      | '#' | '>' | 'v' | '<' | '^' ->
          let next_graph =
            List.fold_left
              (fun acc n -> acc |> insert n (x, y) |> insert (x, y) n)
              (CoordMap.add (x, y) [] graph)
              (neighbors (x, y) |> List.filter (fun c -> CoordMap.mem c graph))
          in
          loop next_graph (x + 1, y) next_state
      | '\n' -> loop graph (0, y + 1) next_state
      | _ -> loop graph (x + 1, y) next_state
    with Intcode.Halt -> graph
  in
  lines |> Intcode.read_mem |> Intcode.initial_state
  |> loop CoordMap.empty (0, 0)

let p1 =
  adj_map |> CoordMap.bindings
  |> List.filter_map (fun ((x, y), ns) ->
         if List.length ns = 4 then Some (x * y) else None)
  |> List.fold_left ( + ) 0

(* figured these out by hand *)
let a = "R,6,L,8,R,8\n"
let b = "R,4,R,6,R,6,R,4,R,4\n"
let c = "L,8,R,6,L,10,L,10\n"
let main = "A,A,B,C,B,C,B,C,A,C\n"

let input =
  let translate s = s |> string_to_chars |> List.map int_of_char in
  translate main @ translate a @ translate b @ translate c @ [ 110; 10 ]

let p2 =
  let mem = Intcode.read_mem lines in
  Intcode.set_addr mem 0 2;
  let output = Intcode.run_until_halt (ref input) mem in
  output

let () = Printf.printf "Part 1: %d\nPart 2: %d" p1 p2

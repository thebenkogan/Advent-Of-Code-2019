open AOC
open Utils

let lines = read_lines ()

module Coord = struct
  type t = int * int

  let compare = compare
end

module CoordSet = Set.Make (Coord)
module CoordMap = Map.Make (Coord)

let pos_with_dirs p = [ (1, p); (2, p); (3, p); (4, p) ]

let next_pos (x, y) = function
  | 1 -> (x, y + 1)
  | 2 -> (x, y - 1)
  | 3 -> (x - 1, y)
  | 4 -> (x + 1, y)
  | _ -> failwith "invalid dir"

let dir_to_move (cx, cy) (tx, ty) =
  match (cx - tx, cy - ty) with
  | 0, -1 -> 1
  | 0, 1 -> 2
  | 1, 0 -> 3
  | -1, 0 -> 4
  | _ -> failwith "invalid backtrack"

let rec backtrack_until_pos curr_pos target_pos state backtrack =
  if curr_pos = target_pos then (state, backtrack)
  else
    match backtrack with
    | [] -> failwith "empty backtrack"
    | backtrack_pos :: t ->
        let _, next_state =
          Intcode.run (ref [ dir_to_move curr_pos backtrack_pos ]) state
        in
        backtrack_until_pos backtrack_pos target_pos next_state t

let insert k v map =
  CoordMap.update k
    (function Some elts -> Some (v :: elts) | None -> Some [ v ])
    map

let target = ref (0, 0)

let rec explore curr_pos state backtrack stack graph =
  match stack with
  | [] -> graph
  | (dir, pos) :: t -> (
      let state, backtrack =
        if pos <> curr_pos then backtrack_until_pos curr_pos pos state backtrack
        else (state, backtrack)
      in
      let result, next_state = Intcode.run (ref [ dir ]) state in
      match result with
      | 0 -> explore pos next_state backtrack t graph
      | v ->
          let next = next_pos pos dir in
          if v = 2 then target := next;
          let next_pos_dirs =
            pos_with_dirs next
            |> List.filter (fun (dir, pos) ->
                   not (CoordMap.mem (next_pos pos dir) graph))
          in
          let next_graph = graph |> insert pos next |> insert next pos in
          explore next next_state (pos :: backtrack) (next_pos_dirs @ t)
            next_graph)

let graph =
  explore (0, 0)
    (lines |> Intcode.read_mem |> Intcode.initial_state)
    []
    (pos_with_dirs (0, 0))
    (CoordMap.singleton (0, 0) [])

let rec bfs_to_target queue seen =
  let pos, steps = Queue.pop queue in
  if pos = !target then steps
  else
    let next_seen = ref seen in
    CoordMap.find pos graph
    |> List.filter (fun pos -> not (CoordSet.mem pos seen))
    |> List.iter (fun pos ->
           next_seen := CoordSet.add pos !next_seen;
           Queue.add (pos, steps + 1) queue);
    bfs_to_target queue !next_seen

let p1 =
  let queue = Queue.create () in
  Queue.add ((0, 0), 0) queue;
  bfs_to_target queue (CoordSet.singleton (0, 0))

let rec bfs_max_steps queue seen max_steps =
  try
    let pos, steps = Queue.pop queue in
    let next_max_steps = max max_steps steps in
    let next_seen = ref seen in
    CoordMap.find pos graph
    |> List.filter (fun pos -> not (CoordSet.mem pos seen))
    |> List.iter (fun pos ->
           next_seen := CoordSet.add pos !next_seen;
           Queue.add (pos, steps + 1) queue);
    bfs_max_steps queue !next_seen next_max_steps
  with Queue.Empty -> max_steps

let p2 =
  let queue = Queue.create () in
  Queue.add (!target, 0) queue;
  bfs_max_steps queue (CoordSet.singleton !target) min_int

let () = Printf.printf "Part 1: %d\nPart 2: %d" p1 p2

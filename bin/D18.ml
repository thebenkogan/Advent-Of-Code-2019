open AOC.Utils

let lines = read_lines ()

module Coord = struct
  type t = int * int

  let compare = compare
end

module CoordSet = Set.Make (Coord)
module CoordMap = Map.Make (Coord)

let dirs = [ (0, 1); (0, -1); (1, 0); (-1, 0) ]

let insert k v map =
  CoordMap.update k
    (function Some elts -> Some (v :: elts) | None -> None)
    map

let adj_map, key_map, door_map, starts =
  let rec build (x, y) line rest acc keys doors starts =
    match line with
    | [] ->
        if rest = [] then (acc, keys, doors, starts)
        else
          build (0, y + 1) (List.hd rest) (List.tl rest) acc keys doors starts
    | h :: t ->
        let new_acc =
          if h <> '#' then
            List.fold_left
              (fun nacc (dx, dy) ->
                if CoordMap.mem (x + dx, y + dy) nacc then
                  nacc
                  |> insert (x + dx, y + dy) (x, y)
                  |> insert (x, y) (x + dx, y + dy)
                else nacc)
              (CoordMap.add (x, y) [] acc)
              dirs
          else acc
        in
        let new_keys =
          match h with 'a' .. 'z' -> CoordMap.add (x, y) h keys | _ -> keys
        in
        let new_doors =
          match h with 'A' .. 'Z' -> CoordMap.add (x, y) h doors | _ -> doors
        in
        let new_starts = if h = '@' then (x, y) :: starts else starts in
        build (x + 1, y) t rest new_acc new_keys new_doors new_starts
  in
  let char_lines = List.map string_to_chars lines in
  build (0, -1) [] char_lines CoordMap.empty CoordMap.empty CoordMap.empty []

module CharSet = Set.Make (Char)

let rec bfs_all_paths seen_keys queue seen acc :
    (int * (int * int) * char list) list =
  match Queue.take_opt queue with
  | None -> acc
  | Some (pos, steps, deps) ->
      let next_keys, next_acc =
        match CoordMap.find_opt pos key_map with
        | Some k when not (CharSet.mem k seen_keys) ->
            (CharSet.add k seen_keys, (steps, pos, deps) :: acc)
        | _ -> (seen_keys, acc)
      in
      let next_deps =
        match CoordMap.find_opt pos door_map with
        | Some d -> Char.lowercase_ascii d :: deps
        | None -> deps
      in
      let next_seen = ref seen in
      CoordMap.find pos adj_map
      |> List.filter (fun n -> not (CoordSet.mem n seen))
      |> List.iter (fun n ->
             next_seen := CoordSet.add n !next_seen;
             Queue.push (n, steps + 1, next_deps) queue);
      bfs_all_paths next_keys queue !next_seen next_acc

(*
   map of key or start position to a list of reachable keys with their distances and dependencies
   i.e. pos -> (dist, key, deps) list
*)
let key_paths =
  List.map (fun s -> (s, '@')) starts @ CoordMap.bindings key_map
  |> List.fold_left
       (fun acc (pos, k) ->
         let queue = Queue.create () in
         Queue.add (pos, 0, []) queue;
         let paths =
           bfs_all_paths (CharSet.singleton k) queue CoordSet.empty []
         in
         CoordMap.add pos paths acc)
       CoordMap.empty

let satisfies_deps keys deps = List.for_all (fun k -> List.mem k keys) deps
let num_keys = CoordMap.cardinal key_map

module Node = struct
  type t = int * (int * int) * char list
  (** steps to node (i.e. weight), position, and collected keys *)

  let compare = compare
end

module PQueue = Set.Make (Node)
module StringSet = Set.Make (String)

let hash pos keys =
  pair_to_string string_of_int pos
  ^ (keys |> List.sort_uniq compare |> list_to_string Char.escaped)

let rec dijkstra_best_keys queue seen target =
  let steps, pos, keys = PQueue.min_elt queue in
  if satisfies_deps keys target then steps
  else
    let new_queue = ref (PQueue.remove (steps, pos, keys) queue) in
    let node_hash = hash pos keys in
    if StringSet.mem node_hash seen then
      dijkstra_best_keys !new_queue seen target
    else
      (CoordMap.find pos key_paths
       |> List.iter (fun (dist, key_pos, deps) ->
              let key = CoordMap.find key_pos key_map in
              if (not (List.mem key keys)) && satisfies_deps keys deps then
                new_queue :=
                  PQueue.add (steps + dist, key_pos, key :: keys) !new_queue);
       dijkstra_best_keys !new_queue (StringSet.add node_hash seen))
        target

let p1 =
  try
    let queue = PQueue.singleton (0, List.hd starts, []) in
    let all_keys = CoordMap.bindings key_map |> List.map (fun (_, k) -> k) in
    dijkstra_best_keys queue StringSet.empty all_keys
  with Not_found -> 0

let rec reachable_keys acc = function
  | [] -> acc
  | p :: t ->
      let next_keys =
        CoordMap.find p key_paths
        |> List.map (fun (_, key_pos, _) -> key_pos)
        |> List.filter (fun p ->
               not (CharSet.mem (CoordMap.find p key_map) acc))
      in
      let new_acc =
        next_keys
        |> List.map (fun key_pos -> CoordMap.find key_pos key_map)
        |> CharSet.of_list |> CharSet.union acc
      in
      reachable_keys new_acc (next_keys @ t)

let starts_with_keys =
  List.map (fun s -> (s, reachable_keys CharSet.empty [ s ])) starts

let quadrant_costs =
  List.map
    (fun (s, keys) ->
      let starting_keys =
        starts_with_keys
        |> List.filter (fun (p, _) -> p <> s)
        |> List.fold_left (fun acc (_, keys) -> CharSet.elements keys @ acc) []
      in
      let queue = PQueue.singleton (0, s, starting_keys) in
      dijkstra_best_keys queue StringSet.empty (CharSet.elements keys))
    starts_with_keys

let p2 = quadrant_costs |> List.fold_left ( + ) 0
let () = Printf.printf "Part 1: %d\nPart 2: %d" p1 p2

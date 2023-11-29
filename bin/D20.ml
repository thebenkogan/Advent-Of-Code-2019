open AOC.Utils

let lines = read_lines ()

module Coord = struct
  type t = int * int

  let compare = compare
end

module CoordSet = Set.Make (Coord)
module CoordMap = Map.Make (Coord)
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

let grid =
  lines |> List.map string_to_chars |> List.map Array.of_list |> Array.of_list

let rows = Array.length grid
let cols = Array.length grid.(0)
let dirs = [ (0, 1); (0, -1); (1, 0); (-1, 0) ]

let insert k v map =
  CoordMap.update k
    (function Some elts -> Some (v :: elts) | None -> None)
    map

let is_letter = function 'A' .. 'Z' -> true | _ -> false
let in_bounds (x, y) = x >= 0 && x < cols && y >= 0 && y < rows

let read_portal (x, y) =
  let c1 = grid.(y).(x) in
  let nx, ny =
    dirs
    |> List.map (fun (dx, dy) -> (x + dx, y + dy))
    |> List.filter in_bounds
    |> List.find (fun (nx, ny) -> is_letter grid.(ny).(nx))
  in
  let c2 = grid.(ny).(nx) in
  if x - nx = 1 || y - ny = 1 then Char.escaped c2 ^ Char.escaped c1
  else Char.escaped c1 ^ Char.escaped c2

let adj_map, portal_map =
  let rec build (x, y) adj portals =
    if x = cols - 1 && y = rows - 1 then (adj, portals)
    else
      let curr = grid.(y).(x) in
      let neighbors =
        dirs
        |> List.map (fun (dx, dy) -> (x + dx, y + dy))
        |> List.filter in_bounds
      in
      let new_pos = if x = cols - 1 then (0, y + 1) else (x + 1, y) in
      if curr = '.' then
        let new_adj =
          List.fold_left
            (fun nadj (nx, ny) ->
              if CoordMap.mem (nx, ny) nadj then
                nadj |> insert (nx, ny) (x, y) |> insert (x, y) (nx, ny)
              else nadj)
            (CoordMap.add (x, y) [] adj)
            neighbors
        in
        let new_portals =
          match
            List.find_opt (fun (nx, ny) -> is_letter grid.(ny).(nx)) neighbors
          with
          | Some p ->
              let portal = read_portal p in
              StringMap.update portal
                (function
                  | Some elts -> Some ((x, y) :: elts) | None -> Some [ (x, y) ])
                portals
          | None -> portals
        in
        build new_pos new_adj new_portals
      else build new_pos adj portals
  in
  build (0, 0) CoordMap.empty StringMap.empty

let start = StringMap.find "AA" portal_map |> List.hd
let stop = StringMap.find "ZZ" portal_map |> List.hd

let adj_map =
  StringMap.fold
    (fun _ cs acc ->
      if List.length cs > 1 then
        acc
        |> insert (List.nth cs 0) (List.nth cs 1)
        |> insert (List.nth cs 1) (List.nth cs 0)
      else acc)
    portal_map adj_map

let rec bfs_to_target queue seen =
  let pos, steps = Queue.pop queue in
  if pos = stop then steps
  else
    let next_seen = ref seen in
    CoordMap.find pos adj_map
    |> List.filter (fun pos -> not (CoordSet.mem pos seen))
    |> List.iter (fun pos ->
           next_seen := CoordSet.add pos !next_seen;
           Queue.add (pos, steps + 1) queue);
    bfs_to_target queue !next_seen

let p1 =
  let queue = Queue.create () in
  Queue.add (start, 0) queue;
  bfs_to_target queue (CoordSet.singleton (0, 0))

let hash pos level = pair_to_string string_of_int pos ^ string_of_int level

let portals =
  StringMap.fold (fun _ cs acc -> cs @ acc) portal_map [] |> CoordSet.of_list

let is_outer (x, y) = x = 2 || x = cols - 3 || y = 2 || y = rows - 3

let rec bfs_to_target_leveled queue seen =
  let pos, level, steps = Queue.pop queue in
  if pos = stop && level = 0 then steps
  else
    let next_seen = ref seen in
    CoordMap.find pos adj_map
    |> List.map (fun neighbor ->
           let next_level =
             if CoordSet.mem pos portals && CoordSet.mem neighbor portals then
               if is_outer pos then level - 1 else level + 1
             else level
           in
           (neighbor, next_level))
    |> List.filter (fun (neighbor, new_level) ->
           not (StringSet.mem (hash neighbor new_level) seen))
    |> List.iter (fun (neighbor, new_level) ->
           if new_level >= 0 then (
             next_seen := StringSet.add (hash neighbor new_level) !next_seen;
             Queue.add (neighbor, new_level, steps + 1) queue));
    bfs_to_target_leveled queue !next_seen

let p2 =
  let queue = Queue.create () in
  Queue.add (start, 0, 0) queue;
  bfs_to_target_leveled queue (StringSet.singleton (hash (0, 0) 0))

let () = Printf.printf "Part 1: %d\nPart 2: %d" p1 p2

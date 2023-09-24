open AOC.Aoc

let lines = read_lines ()

module StringMap = Map.Make (String)

let adj_map =
  lines
  |> List.map (fun l ->
         match String.split_on_char ')' l with
         | [ a; b ] -> (a, b)
         | _ -> failwith "invalid line")
  |> List.fold_left (fun smap (a, b) -> StringMap.add b a smap) StringMap.empty

let rec num_orbits start stop =
  match StringMap.find_opt start adj_map with
  | Some other -> if other = stop then 1 else 1 + num_orbits other stop
  | None -> 0

let p1 = StringMap.fold (fun k _ acc -> acc + num_orbits k "COM") adj_map 0

let rec list_orbits start =
  match StringMap.find_opt start adj_map with
  | Some other -> other :: list_orbits other
  | None -> []

let rec find_first_match l1 = function
  | [] -> None
  | h :: t -> if List.mem h l1 then Some h else find_first_match l1 t

let p2 =
  let you_orbit = StringMap.find "YOU" adj_map in
  let san_orbit = StringMap.find "SAN" adj_map in
  match find_first_match (list_orbits you_orbit) (list_orbits san_orbit) with
  | Some matching_orbit ->
      num_orbits you_orbit matching_orbit + num_orbits san_orbit matching_orbit
  | None -> failwith "no shared planet found"

let () = Printf.printf "Part 1: %d\nPart 2: %d" p1 p2

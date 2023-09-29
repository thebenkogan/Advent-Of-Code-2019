open AOC.Utils

let lines = read_lines ()

type quadrant = Q1 | Q2 | Q3 | Q4

let slope (x1, y1) (x2, y2) =
  (y2 - y1 |> float_of_int) /. (x2 - x1 |> float_of_int)

let dist (x1, y1) (x2, y2) =
  hypot (x2 - x1 |> float_of_int) (y2 - y1 |> float_of_int)

let sign n = if n > 0 then 1 else -1

let quad_with_slope (x1, y1) (x2, y2) =
  let m = slope (x1, y1) (x2, y2) in
  match (sign (x2 - x1), sign (y2 - y1)) with
  | 1, -1 -> (Q1, m)
  | -1, -1 -> (Q2, m)
  | -1, 1 -> (Q3, m)
  | _ -> (Q4, m)

let points_in_sight (row, col) points =
  points
  |> List.filter (( <> ) (row, col))
  |> List.sort (fun p1 p2 -> compare (dist (row, col) p1) (dist (row, col) p2))
  |> List.map (fun p -> (p, quad_with_slope (row, col) p))
  |> List.sort_uniq (fun (_, qs1) (_, qs2) -> compare qs1 qs2)
  |> List.map fst

let count_in_sight (row, col) points =
  points |> points_in_sight (row, col) |> List.length

let points =
  lines
  |> List.mapi (fun row line ->
         line
         |> Str.split (Str.regexp "")
         |> List.mapi (fun col point ->
                if point = "#" then Some (row, col) else None))
  |> List.flatten |> List.filter_map Fun.id

let laser_point, in_sight =
  points
  |> List.fold_left
       (fun (best_point, best_in_sight) point ->
         let count = count_in_sight point points in
         if count > best_in_sight then (point, count)
         else (best_point, best_in_sight))
       ((0, 0), min_int)

let p1 = in_sight

let clockwise_ang (y, x) =
  let ly, lx = laser_point in
  Float.pi -. atan2 (x - lx |> float_of_int) (y - ly |> float_of_int)

let p2 =
  let rec loop points acc =
    let in_sight = points_in_sight laser_point points in
    if in_sight = [] then acc
    else
      let rest = List.filter (fun p -> not (List.mem p in_sight)) points in
      in_sight
      |> List.sort (fun p1 p2 -> compare (clockwise_ang p1) (clockwise_ang p2))
      |> ( @ ) acc |> loop rest
  in
  let y, x = List.nth (loop points []) 199 in
  (x * 100) + y

let () = Printf.printf "Part 1: %d\nPart 2: %d" p1 p2

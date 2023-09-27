open AOC.Utils

let fuel mass = (mass / 3) - 2

let p1 =
  read_lines ()
  |> List.map (fun s -> s |> int_of_string |> fuel)
  |> List.fold_left ( + ) 0

let rec fuel_nested mass =
  let fuel_val = fuel mass in
  if fuel_val <= 0 then 0 else fuel_val + fuel_nested fuel_val

let p2 =
  read_lines ()
  |> List.map (fun s -> s |> int_of_string |> fuel_nested)
  |> List.fold_left ( + ) 0

let () = Printf.printf "Part 1: %d\nPart 2: %d" p1 p2

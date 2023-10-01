open AOC.Utils

let lines = read_lines ()

type moon = { x : int; y : int; z : int; x_vel : int; y_vel : int; z_vel : int }

let r = Str.regexp "<x=\\(-?[0-9]+\\), y=\\(-?[0-9]+\\), z=\\(-?[0-9]+\\)>"

let moons =
  lines
  |> List.map (fun l ->
         Str.string_match r l 0 |> ignore;
         {
           x = Str.matched_group 1 l |> int_of_string;
           y = Str.matched_group 2 l |> int_of_string;
           z = Str.matched_group 3 l |> int_of_string;
           x_vel = 0;
           y_vel = 0;
           z_vel = 0;
         })

let apply_gravity m1 m2 =
  {
    m1 with
    x_vel = m1.x_vel + compare m2.x m1.x;
    y_vel = m1.y_vel + compare m2.y m1.y;
    z_vel = m1.z_vel + compare m2.z m1.z;
  }

let apply_gravities ms =
  ms
  |> List.map (fun m ->
         ms |> List.filter (( <> ) m) |> List.fold_left apply_gravity m)

let apply_velocity moon =
  {
    moon with
    x = moon.x + moon.x_vel;
    y = moon.y + moon.y_vel;
    z = moon.z + moon.z_vel;
  }

let get_energy { x; y; z; x_vel; y_vel; z_vel } =
  (abs x + abs y + abs z) * (abs x_vel + abs y_vel + abs z_vel)

let p1 =
  let rec loop n moons =
    if n = 1000 then moons
    else moons |> apply_gravities |> List.map apply_velocity |> loop (n + 1)
  in
  loop 0 moons |> List.map get_energy |> List.fold_left ( + ) 0

let rec loop_until_periods curr_moons n x_period y_period z_period =
  match (x_period, y_period, z_period) with
  | Some xp, Some yp, Some zp -> (xp, yp, zp)
  | x_period, y_period, z_period ->
      let next_moons =
        curr_moons |> apply_gravities |> List.map apply_velocity
      in
      let combined = List.combine moons next_moons in
      let next_x_period =
        if x_period <> None then x_period
        else if
          combined
          |> List.for_all (fun (m, nm) -> m.x = nm.x && m.x_vel = nm.x_vel)
        then Some n
        else None
      in
      let next_y_period =
        if y_period <> None then y_period
        else if
          combined
          |> List.for_all (fun (m, nm) -> m.y = nm.y && m.y_vel = nm.y_vel)
        then Some n
        else None
      in
      let next_z_period =
        if z_period <> None then z_period
        else if
          combined
          |> List.for_all (fun (m, nm) -> m.z = nm.z && m.z_vel = nm.z_vel)
        then Some n
        else None
      in
      loop_until_periods next_moons (n + 1) next_x_period next_y_period
        next_z_period

let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let lcm a b c =
  let lcm_2 a b = a * b / gcd a b in
  lcm_2 a (lcm_2 b c)

let p2 =
  let xp, yp, zp = loop_until_periods moons 1 None None None in
  lcm xp yp zp

let () = Printf.printf "Part 1: %d\nPart 2: %d" p1 p2

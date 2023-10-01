open AOC
open Utils

let lines = read_lines ()

type id = Empty | Wall | Block | Paddle | Ball

let num_to_id = function
  | 0 -> Empty
  | 1 -> Wall
  | 2 -> Block
  | 3 -> Paddle
  | 4 -> Ball
  | _ -> failwith "invalid id"

let run_3_times state direction =
  let input_buffer =
    match direction with Some d -> ref [ d ] | None -> ref []
  in
  let a, state1 = Intcode.run input_buffer state in
  let b, state2 = Intcode.run input_buffer state1 in
  let c, state3 = Intcode.run input_buffer state2 in
  (a, b, c, state3)

let tile_ids mem =
  let rec loop state acc =
    try
      let _, _, id, next_state = run_3_times state None in
      loop next_state (num_to_id id :: acc)
    with Intcode.Halt -> acc
  in
  loop (Intcode.initial_state mem) []

let p1 =
  lines |> Intcode.read_mem |> tile_ids
  |> List.filter (( = ) Block)
  |> List.length

let run_game mem =
  let rec setup state =
    let x, y, _, next_state = run_3_times state None in
    if x = -1 && y = 0 then next_state else setup next_state
  in
  let start_state = mem |> Intcode.initial_state |> setup in
  let last_score = ref 0 in
  let rec play state bx px =
    let direction = compare bx px in
    let x, y, id, next_state = run_3_times state (Some direction) in
    if x = -1 && y = 0 then (
      last_score := id;
      play next_state bx px)
    else
      let next_bx = if num_to_id id = Ball then x else bx in
      let next_px = if num_to_id id = Paddle then x else px in
      play next_state next_bx next_px
  in
  try play start_state 0 0 with Intcode.Halt -> !last_score

let p2 =
  let mem = Intcode.read_mem lines in
  Intcode.set_addr mem 0 2;
  run_game mem

let () = Printf.printf "Part 1: %d\nPart 2: %d" p1 p2

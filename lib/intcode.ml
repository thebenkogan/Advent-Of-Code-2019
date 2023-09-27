let read_mem lines =
  lines |> List.hd |> String.split_on_char ',' |> List.map int_of_string
  |> Array.of_list

exception Halt

let rec read_modes mode_int =
  if mode_int = 0 then [] else (mode_int mod 10) :: read_modes (mode_int / 10)

let next_mode = function [] -> (0, []) | h :: t -> (h, t)
let resolve_val mem mode input = if mode = 1 then input else mem.(input)

let next_2 mem modes pos =
  let mode1, modes1 = next_mode modes in
  let mode2, _ = next_mode modes1 in
  let v1 = resolve_val mem mode1 mem.(pos + 1) in
  let v2 = resolve_val mem mode2 mem.(pos + 2) in
  (v1, v2)

let run input_buffer mem start_pos =
  let read_input () =
    let input = List.hd !input_buffer in
    input_buffer := List.tl !input_buffer;
    input
  in
  let rec compute pos =
    let modes, op = (mem.(pos) / 100 |> read_modes, mem.(pos) mod 100) in
    if op = 99 then raise Halt
    else if op = 3 then (
      mem.(mem.(pos + 1)) <- read_input ();
      compute (pos + 2))
    else if op = 4 then
      let mode, _ = next_mode modes in
      let output = resolve_val mem mode mem.(pos + 1) in
      (output, mem, pos + 2)
    else if op = 5 || op = 6 then
      let v1, v2 = next_2 mem modes pos in
      if (op = 5 && v1 <> 0) || (op = 6 && v1 = 0) then compute v2
      else compute (pos + 3)
    else if op = 7 || op = 8 then (
      let v1, v2 = next_2 mem modes pos in
      let dest = mem.(pos + 3) in
      mem.(dest) <-
        (if (op = 7 && v1 < v2) || (op = 8 && v1 = v2) then 1 else 0);
      compute (pos + 4))
    else
      let v1, v2 = next_2 mem modes pos in
      let dest = mem.(pos + 3) in
      mem.(dest) <- (if op = 1 then v1 + v2 else v1 * v2);
      compute (pos + 4)
  in
  compute start_pos

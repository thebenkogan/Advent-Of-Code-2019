type mem = (int, int) Hashtbl.t

let read_mem lines =
  lines |> List.hd |> String.split_on_char ','
  |> List.mapi (fun i s -> (i, int_of_string s))
  |> List.to_seq |> Hashtbl.of_seq

let get_addr mem addr =
  match Hashtbl.find_opt mem addr with Some v -> v | None -> 0

let set_addr = Hashtbl.add

exception Halt

let rec read_modes mode_int =
  if mode_int = 0 then [] else (mode_int mod 10) :: read_modes (mode_int / 10)

let next_mode = function [] -> (0, []) | h :: t -> (h, t)

let resolve_val mem mode rel_base input =
  if mode = 2 then get_addr mem (input + rel_base)
  else if mode = 1 then input
  else get_addr mem input

let next_2 mem modes rel_base pos =
  let mode1, modes1 = next_mode modes in
  let mode2, _ = next_mode modes1 in
  let v1 = resolve_val mem mode1 rel_base (get_addr mem (pos + 1)) in
  let v2 = resolve_val mem mode2 rel_base (get_addr mem (pos + 2)) in
  (v1, v2)

let run input_buffer mem start_pos =
  let read_input () =
    let input = List.hd !input_buffer in
    input_buffer := List.tl !input_buffer;
    input
  in
  let rel_base = ref 0 in
  let rec compute pos =
    let instr = get_addr mem pos in
    let modes, op = (instr / 100 |> read_modes, instr mod 100) in
    if op = 99 then raise Halt
    else if op = 3 then (
      let mode, _ = next_mode modes in
      let dest = get_addr mem (pos + 1) + if mode = 2 then !rel_base else 0 in
      set_addr mem dest (read_input ());
      compute (pos + 2))
    else if op = 4 then
      let mode, _ = next_mode modes in
      let output = resolve_val mem mode !rel_base (get_addr mem (pos + 1)) in
      (output, mem, pos + 2)
    else if op = 5 || op = 6 then
      let v1, v2 = next_2 mem modes !rel_base pos in
      if (op = 5 && v1 <> 0) || (op = 6 && v1 = 0) then compute v2
      else compute (pos + 3)
    else if op = 7 || op = 8 then (
      let v1, v2 = next_2 mem modes !rel_base pos in
      let mode = try List.nth modes 2 with _ -> 0 in
      let dest = get_addr mem (pos + 3) + if mode = 2 then !rel_base else 0 in
      set_addr mem dest
        (if (op = 7 && v1 < v2) || (op = 8 && v1 = v2) then 1 else 0);
      compute (pos + 4))
    else if op = 9 then (
      let mode, _ = next_mode modes in
      let offset = resolve_val mem mode !rel_base (get_addr mem (pos + 1)) in
      rel_base := !rel_base + offset;
      compute (pos + 2))
    else
      let v1, v2 = next_2 mem modes !rel_base pos in
      let mode = try List.nth modes 2 with _ -> 0 in
      let dest = get_addr mem (pos + 3) + if mode = 2 then !rel_base else 0 in
      set_addr mem dest (if op = 1 then v1 + v2 else v1 * v2);
      compute (pos + 4)
  in
  compute start_pos

let run_until_halt input_buffer mem =
  let last_output = ref 0 in
  let rec loop mem pos =
    let output, finish_mem, finish_pos = run input_buffer mem pos in
    last_output := output;
    loop finish_mem finish_pos
  in
  try loop mem 0 with Halt -> !last_output

open AOC
open Utils

let lines = read_lines ()

let run_amps =
  List.fold_left
    (fun input phase ->
      let output, _, _ =
        Intcode.run (ref [ phase; input ]) (Intcode.read_mem lines) 0
      in
      output)
    0

let p1 =
  permutations [ 0; 1; 2; 3; 4 ]
  |> List.map run_amps |> List.fold_left max min_int

let run_feedback_loop settings =
  let states =
    settings |> List.map (fun _ -> (Intcode.read_mem lines, 0)) |> Array.of_list
  in
  let input_buffers =
    settings |> List.map (fun setting -> ref [ setting ]) |> Array.of_list
  in
  input_buffers.(0) := !(input_buffers.(0)) @ [ 0 ];
  let last_output = ref 0 in
  let rec loop i =
    let start_mem, start_pos = states.(i) in
    let output, finish_mem, finish_pos =
      Intcode.run input_buffers.(i) start_mem start_pos
    in
    last_output := output;
    let next_i = (i + 1) mod 5 in
    input_buffers.(next_i) := !(input_buffers.(next_i)) @ [ output ];
    states.(i) <- (finish_mem, finish_pos);
    loop next_i
  in
  try loop 0 with Intcode.Halt -> !last_output

let p2 =
  permutations [ 5; 6; 7; 8; 9 ]
  |> List.map run_feedback_loop |> List.fold_left max min_int

let () = Printf.printf "Part 1: %d\nPart 2: %d" p1 p2

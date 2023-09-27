open AOC
open Utils

let lines = read_lines ()

let run_until_halt input =
  let last_output = ref 0 in
  let rec loop mem pos =
    let output, finish_mem, finish_pos = Intcode.run (ref [ input ]) mem pos in
    last_output := output;
    loop finish_mem finish_pos
  in
  try loop (Intcode.read_mem lines) 0 with Intcode.Halt -> !last_output

let p1 = run_until_halt 1
let p2 = run_until_halt 5
let () = Printf.printf "Part 1: %d\nPart 2: %d" p1 p2

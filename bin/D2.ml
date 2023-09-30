open AOC
open Utils

let lines = read_lines ()

let run_with noun verb =
  let mem = Intcode.read_mem lines in
  Intcode.set_addr mem 1 noun;
  Intcode.set_addr mem 2 verb;
  try
    let _ = Intcode.run (ref []) mem 0 0 in
    failwith "didn't halt"
  with Intcode.Halt -> Intcode.get_addr mem 0

let p1 = run_with 12 2

let p2 =
  let rec find noun verb =
    if run_with noun verb = 19690720 then (noun, verb)
    else if verb + 1 = 100 then find (noun + 1) 0
    else find noun (verb + 1)
  in
  let noun, verb = find 0 0 in
  (100 * noun) + verb

let () = Printf.printf "Part 1: %d\nPart 2: %d" p1 p2
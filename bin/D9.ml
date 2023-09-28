open AOC
open Utils

let lines = read_lines ()
let p1 = Intcode.run_until_halt (ref [ 1 ]) (Intcode.read_mem lines)
let p2 = Intcode.run_until_halt (ref [ 2 ]) (Intcode.read_mem lines)
let () = Printf.printf "Part 1: %d\nPart 2: %d" p1 p2

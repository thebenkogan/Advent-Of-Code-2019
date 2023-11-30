open AOC
open Utils

let lines = read_lines ()

(* program jumps if there is ground 4 tiles away and any of the 3 tiles before are holes *)

let instructions =
  [ "NOT A J"; "NOT B T"; "OR T J"; "NOT C T"; "OR T J"; "AND D J" ]

let run command instr =
  let translate s = s |> string_to_chars |> List.map int_of_char in
  let input =
    instr @ [ command ]
    |> List.map (fun i -> translate (i ^ "\n"))
    |> List.flatten
  in
  let mem = Intcode.read_mem lines in
  let output = Intcode.run_until_halt (ref input) mem in
  output

let p1 = run "WALK" instructions

(*
   program jumps if there is ground 4 tiles away and any of the 3 tiles before are holes
   and there is at least one tile afterwards to walk forward or another jump 4 tiles away
*)

let instructions2 =
  [
    "NOT A J";
    "NOT B T";
    "OR T J";
    "NOT C T";
    "OR T J";
    "AND D J";
    "NOT J T";
    "OR E T";
    "OR H T";
    "AND T J";
  ]

let p2 = run "RUN" instructions2
let () = Printf.printf "Part 1: %d\nPart 2: %d" p1 p2

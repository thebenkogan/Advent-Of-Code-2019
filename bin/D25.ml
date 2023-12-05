open AOC
open Utils

let lines = read_lines ()
let translate s = s ^ "\n" |> string_to_chars |> List.map int_of_char

(* print each char in run_until_halt and explore! *)
let () =
  let input =
    [
      "south";
      "take whirled peas";
      "south";
      "south";
      "south";
      "take festive hat";
      "north";
      "north";
      "north";
      "north";
      "west";
      "take pointer";
      "east";
      "north";
      "take coin";
      "north";
      "take astronaut ice cream";
      "north";
      "west";
      "take dark matter";
      "south";
      "take klein bottle";
      "west";
      "take mutex";
      "west";
      "south";
      "inv";
      "drop dark matter";
      "drop astronaut ice cream";
      "drop klein bottle";
      "drop pointer";
      "east";
    ]
    |> List.map translate |> List.flatten
  in
  Intcode.run_until_halt (ref input) (Intcode.read_mem lines) |> ignore

let p1 = 16410
let () = Printf.printf "Part 1: %d" p1

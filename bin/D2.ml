open AOC.Aoc

exception Halt

let read_mem () =
  read_lines () |> List.hd |> String.split_on_char ',' |> List.map int_of_string
  |> Array.of_list

let execute op v1 v2 =
  if op = 1 then v1 + v2
  else if op = 2 then v1 * v2
  else if op = 99 then raise Halt
  else failwith "unrecognized op"

let run noun verb =
  let arr = read_mem () in
  arr.(1) <- noun;
  arr.(2) <- verb;
  let rec compute pos =
    let op = arr.(pos) in
    let v1 = arr.(arr.(pos + 1)) in
    let v2 = arr.(arr.(pos + 2)) in
    let dest = arr.(pos + 3) in
    arr.(dest) <- execute op v1 v2;
    compute (pos + 4)
  in
  try compute 0 with Halt -> arr.(0)

let p1 = run 12 2
let output = 19690720

let p2 =
  let rec find noun verb =
    if run noun verb = output then (noun, verb)
    else if verb + 1 = 100 then find (noun + 1) 0
    else find noun (verb + 1)
  in
  let noun, verb = find 0 0 in
  (100 * noun) + verb

let () = Printf.printf "Part 1: %d\nPart 2: %d" p1 p2
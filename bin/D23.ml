open AOC
open Utils

let lines = read_lines ()

type computer = { state : Intcode.state; queue : int list ref }

let computers =
  Array.init 50 (fun addr ->
      let state = lines |> Intcode.read_mem |> Intcode.initial_state in
      let queue = ref [ addr; -1 ] in
      { state; queue })

let run_til_io { state; queue } =
  try
    let dest, s2 = Intcode.run queue state in
    let x, s3 = Intcode.run queue s2 in
    let y, s4 = Intcode.run queue s3 in
    (s4, Some (dest, x, y))
  with Intcode.NeedInput s -> (s, None)

let nat_packet = ref (-1, -1, -1)
let p1_ans = ref (-1)

let run_all () =
  let num_idle = ref 0 in
  Array.iteri
    (fun i c ->
      let next_state, output = run_til_io c in
      computers.(i) <- { (computers.(i)) with state = next_state };
      match output with
      | Some (dest, x, y) ->
          if dest = 255 then (
            if !p1_ans = -1 then p1_ans := y;
            nat_packet := (dest, x, y))
          else
            computers.(dest).queue := !(computers.(dest).queue) @ [ x; y; -1 ]
      | None -> incr num_idle)
    computers;
  !num_idle

let last_nat_y = ref (-1)

exception Found of int

let rec simulate () =
  let num_idle = run_all () in
  if num_idle = 50 then (
    let _, nat_x, nat_y = !nat_packet in
    if nat_y = !last_nat_y then raise (Found nat_y) else last_nat_y := nat_y;
    computers.(0).queue := !(computers.(0).queue) @ [ nat_x; nat_y; -1 ]);
  simulate ()

let p2 = try simulate () with Found y -> y
let p1 = !p1_ans
let () = Printf.printf "Part 1: %d\nPart 2: %d" p1 p2

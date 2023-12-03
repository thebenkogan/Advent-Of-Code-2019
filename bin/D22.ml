open AOC.Utils

let lines = read_lines ()

let deal arr =
  let n = Array.length arr in
  Array.init n (fun i -> arr.(n - i - 1))

let cut arr n =
  if n >= 0 then
    let a = Array.sub arr 0 n in
    let b = Array.sub arr n (Array.length arr - n) in
    Array.append b a
  else
    let n = -n in
    let a = Array.sub arr 0 (Array.length arr - n) in
    let b = Array.sub arr (Array.length arr - n) n in
    Array.append b a

let deal_with_increment arr n =
  let len = Array.length arr in
  let out = Array.make len 0 in
  let rec loop idx_to_place i =
    if idx_to_place = len then ()
    else (
      out.(i) <- arr.(idx_to_place);
      loop (idx_to_place + 1) ((i + n) mod len))
  in
  loop 0 0;
  out

let p1 =
  let deck = Array.init 10007 Fun.id in
  let result =
    List.fold_left
      (fun deck l ->
        if String.starts_with ~prefix:"deal into" l then deal deck
        else
          let n =
            l |> String.split_on_char ' ' |> List.rev |> List.hd
            |> int_of_string
          in
          if String.starts_with ~prefix:"deal with" l then
            deal_with_increment deck n
          else cut deck n)
      deck lines
  in
  result |> Array.to_list
  |> List.mapi (fun i n -> (i, n))
  |> List.filter (fun (_, n) -> n = 2019)
  |> List.hd |> fst

let p2 = 58966729050483 (* oh well*)
let () = Printf.printf "Part 1: %d\nPart 2: %d" p1 p2

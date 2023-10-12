open AOC.Utils

let lines = read_lines ()
let r = Str.regexp "\\([0-9]+ [A-Z]+\\)"

let rec get_all_matches s i =
  match Str.search_forward r s i with
  | i ->
      let group = Str.matched_string s in
      group :: get_all_matches s (i + String.length group)
  | exception Not_found -> []

module StringMap = Map.Make (String)

type reactant = { name : string; quantity : int }

let adj_map =
  lines
  |> List.map (fun l ->
         Str.string_match r l 0 |> ignore;
         let matches = get_all_matches l 0 in
         let product_name =
           matches |> List.rev |> List.hd |> String.split_on_char ' '
           |> List.rev |> List.hd
         in
         let reactants =
           matches
           |> List.map (fun s ->
                  match String.split_on_char ' ' s with
                  | [ q; n ] -> { name = n; quantity = int_of_string q }
                  | _ -> failwith "bad input")
         in
         let p, rs =
           List.partition
             (fun { name; quantity = _ } -> name = product_name)
             reactants
         in
         (List.hd p, rs))
  |> List.fold_left
       (fun acc (p, rs) -> StringMap.add p.name (p.quantity, rs) acc)
       StringMap.empty

let supply : (string, int) Hashtbl.t = Hashtbl.create 20
let ore_produced = ref 0

let rec produce { name; quantity } =
  if name = "ORE" then ore_produced := !ore_produced + quantity
  else
    let supply_amnt =
      match Hashtbl.find_opt supply name with Some v -> v | None -> 0
    in
    if supply_amnt >= quantity then
      Hashtbl.replace supply name (supply_amnt - quantity)
    else
      let prod_amnt, rs = StringMap.find name adj_map in
      let to_produce = quantity - supply_amnt in
      let times =
        (to_produce / prod_amnt) + if to_produce mod prod_amnt = 0 then 0 else 1
      in
      List.iter (fun r -> produce { r with quantity = r.quantity * times }) rs;
      Hashtbl.replace supply name ((times * prod_amnt) + supply_amnt - quantity)

let p1 =
  produce { name = "FUEL"; quantity = 1 };
  !ore_produced

let ore_supply = 1000000000000

let p2 =
  let low = ore_supply / !ore_produced in
  let high = ore_supply in
  let rec bin_search l h =
    if h - l <= 1 then l
    else
      let mid = l + ((h - l) / 2) in
      ore_produced := 0;
      produce { name = "FUEL"; quantity = mid };
      if !ore_produced > ore_supply then bin_search l mid else bin_search mid h
  in
  let amnt = bin_search low high in
  if !ore_produced > ore_supply then amnt - 1 else amnt

let () = Printf.printf "Part 1: %d\nPart 2: %d" p1 p2

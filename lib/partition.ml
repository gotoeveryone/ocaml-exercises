(* リストを奇数と偶数に分ける *)
type result = { odds : int list; evens : int list }

let rec partition lst =
  match lst with
  | [] -> ([], [])
  | x :: xs ->
      let odds, evens = partition xs in
      if x mod 2 = 0 then (odds, x :: evens) else (x :: odds, evens)

let to_string r =
  match r with
  | { odds; evens } ->
      Printf.sprintf "Odds: [%s]\nEvens: [%s]"
        (String.concat "; " (List.map string_of_int odds))
        (String.concat "; " (List.map string_of_int evens))

(* 使用例 *)
(* let () =
  partition [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] |> fun (odds, evens) ->
  Printf.printf "%s\n" (to_string { odds; evens }) *)

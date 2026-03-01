(* クイックソート *)
(* 計算量を減らすため、pivot は先頭要素ではなくランダムに取得する *)
let rec split_at i lst =
  match (i, lst) with
  | _, [] -> failwith "Index out of bounds"
  | 0, x :: xs -> (x, xs)
  | i, x :: xs ->
      let pivot, rest = split_at (i - 1) xs in
      (pivot, x :: rest)

let rec quicksort lst =
  match lst with
  | [] -> []
  | _ ->
      let len = List.length lst in
      let i = Random.int len in
      let pivot, rest = split_at i lst in
      let lt, gte = List.partition (fun x -> x < pivot) rest in
      quicksort lt @ (pivot :: quicksort gte)

(* 使用例 *)
(* let () =
  Random.self_init ();
  let sorted = quicksort [ 3; 6; 8; 10; 1; 5; 2; 1; 4 ] in
  List.iter (Printf.printf "%d ") sorted;
  Printf.printf "\n" *)

(* map 版 *)
let rec map f = function [] -> [] | x :: xs -> f x :: map f xs

(* 使用例 *)
(* let () =
  map (fun x -> x * 2) [ 1; 2; 3; 4; 5 ] |> List.iter (Printf.printf "%d\n") *)

(* fold_right 版 *)
let rec fold_right f acc = function
  | [] -> acc
  | x :: xs -> f x (fold_right f acc xs)

(* 使用例 *)
(* let () =
  fold_right (fun x acc -> (x * 2) :: acc) [] [ 1; 2; 3; 4; 5 ]
  |> List.iter (Printf.printf "%d\n") *)

(* fold_left 版 *)
let rec fold_left f acc = function
  | [] -> acc
  | x :: xs -> fold_left f (f acc x) xs

(* 使用例 *)
(* let () =
  fold_left (fun acc x -> (x * 2) :: acc) [] [ 1; 2; 3; 4; 5 ]
  |> List.rev
  |> List.iter (Printf.printf "%d\n") *)

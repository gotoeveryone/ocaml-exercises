type result = Words of string list | Number of int

(* 利用するルール *)
let rules = [ (3, "Fizz"); (5, "Buzz"); (7, "Bazz"); (9, "AAAA") ]

(* 数値を分類する *)
let classify r n =
  let words =
    List.fold_right
      (fun (divisor, word) acc ->
        if n mod divisor = 0 then word :: acc else acc)
      r []
  in
  match words with [] -> Number n | ws -> Words ws

(* 数値を文字列に変換する *)
let to_string = function
  | Words ws -> String.concat "" ws
  | Number x -> string_of_int x

(* FizzBuzzの文字列を取得する *)
let fizzbuzz r n = n |> classify r |> to_string

(* ユーザー入力を取得し、上限値とする *)
let limit = read_line () |> int_of_string

(* ループ処理 *)
let rec loop n =
  if n > limit then ()
  else (
    print_endline (fizzbuzz rules n);
    loop (n + 1))

let () = loop 1

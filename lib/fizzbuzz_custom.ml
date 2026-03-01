type result = Words of string list | Number of int

(* 数値を分類する *)
let classify n =
  let words =
    []
    |> (fun acc -> if n mod 3 = 0 then "Fizz" :: acc else acc)
    |> (fun acc -> if n mod 5 = 0 then "Buzz" :: acc else acc)
    |> (fun acc -> if n mod 7 = 0 then "Bazz" :: acc else acc)
    |> List.rev
  in
  match words with [] -> Number n | ws -> Words ws

(* 数値を文字列に変換する *)
let to_string = function
  | Words ws -> String.concat "" ws
  | Number x -> string_of_int x

(* FizzBuzzの文字列を取得する *)
let fizzbuzz n = n |> classify |> to_string

(* 結果のリストを返す *)
let generate limit = List.init limit (fun i -> fizzbuzz (i + 1))

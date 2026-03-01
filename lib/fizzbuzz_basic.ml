type result = Fizz | Buzz | FizzBuzz | Number of int

(* 数値を分類する *)
let classify n =
  match n with
  | _ when n mod 3 = 0 && n mod 5 = 0 -> FizzBuzz
  | _ when n mod 3 = 0 -> Fizz
  | _ when n mod 5 = 0 -> Buzz
  | _ -> Number n

(* 数値を文字列に変換する *)
let to_string t =
  match t with
  | FizzBuzz -> "FizzBuzz"
  | Fizz -> "Fizz"
  | Buzz -> "Buzz"
  | Number x -> string_of_int x

(* FizzBuzzの文字列を取得する *)
let fizzbuzz n = to_string (classify n)

(* 結果のリストを返す *)
let generate limit = List.init limit (fun i -> fizzbuzz (i + 1))

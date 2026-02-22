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

(* ユーザー入力を取得し、上限値とする *)
let limit = read_line () |> int_of_string

(* ループ処理 *)
let rec loop n =
  if n > limit then ()
  else (
    print_endline (fizzbuzz n);
    loop (n + 1))

let () = loop 1

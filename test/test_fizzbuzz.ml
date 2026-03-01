let assert_eq ~msg a b = if a <> b then failwith (msg ())

let () =
  for i = 1 to 100 do
    let a = Fizzbuzz_basic.fizzbuzz i in
    let b = Fizzbuzz_rules.fizzbuzz [ (3, "Fizz"); (5, "Buzz") ] i in
    assert_eq
      ~msg:(fun () -> Printf.sprintf "basic mismatch at %d: %s vs %s" i a b)
      a b;

    let c = Fizzbuzz_custom.fizzbuzz i in
    let d =
      Fizzbuzz_rules.fizzbuzz [ (3, "Fizz"); (5, "Buzz"); (7, "Bazz") ] i
    in
    assert_eq
      ~msg:(fun () -> Printf.sprintf "custom mismatch at %d: %s vs %s" i c d)
      c d
  done;

  let e = Fizzbuzz_basic.generate 10 in
  assert_eq
    ~msg:(fun () -> Printf.sprintf "basic generate mismatch")
    e
    [ "1"; "2"; "Fizz"; "4"; "Buzz"; "Fizz"; "7"; "8"; "Fizz"; "Buzz" ];

  let f = Fizzbuzz_custom.generate 10 in
  assert_eq
    ~msg:(fun () -> Printf.sprintf "custom generate mismatch")
    f
    [ "1"; "2"; "Fizz"; "4"; "Buzz"; "Fizz"; "Bazz"; "8"; "Fizz"; "Buzz" ];

  print_endline "Fizzbuzz tests passed!"

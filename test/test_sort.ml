let assert_true msg_thunk b = if not b then failwith (msg_thunk ())

let () =
  Random.init 0;

  assert_true
    (fun () -> "case: unsorted list")
    (Sort.quicksort [ 3; 6; 8; 10; 1; 5; 2; 1; 4 ]
    = [ 1; 1; 2; 3; 4; 5; 6; 8; 10 ]);

  (* 空 *)
  assert_true (fun () -> "case: empty list") (Sort.quicksort [] = []);

  (* ソート済み *)
  assert_true
    (fun () -> "case: already sorted")
    (Sort.quicksort [ 1; 2; 3 ] = [ 1; 2; 3 ]);

  (* 逆順 *)
  assert_true
    (fun () -> "case: reverse order")
    (Sort.quicksort [ 3; 2; 1 ] = [ 1; 2; 3 ]);

  (* 負数 *)
  assert_true
    (fun () -> "case: negative numbers")
    (Sort.quicksort [ -3; -1; -2 ] = [ -3; -2; -1 ]);

  print_endline "Sort tests passed!"

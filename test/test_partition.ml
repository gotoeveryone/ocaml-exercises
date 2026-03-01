let assert_true msg_thunk b = if not b then failwith (msg_thunk ())

let () =
  let odds, evens = Partition.partition [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] in
  assert_true (fun () -> "case: basic odds mismatch") (odds = [ 1; 3; 5; 7; 9 ]);
  assert_true
    (fun () -> "case: basic evens mismatch")
    (evens = [ 2; 4; 6; 8; 10 ]);

  (* 空 *)
  let odds, evens = Partition.partition [] in
  assert_true (fun () -> "case: empty odds mismatch") (odds = []);
  assert_true (fun () -> "case: empty evens mismatch") (evens = []);

  (* 全部片側 *)
  let odds, evens = Partition.partition [ 2; 4; 6 ] in
  assert_true (fun () -> "case: all evens odds mismatch") (odds = []);
  assert_true (fun () -> "case: all evens evens mismatch") (evens = [ 2; 4; 6 ]);

  (* 負数 *)
  let odds, evens = Partition.partition [ -1; -2; -3; -4; -5 ] in
  assert_true (fun () -> "case: negative odds mismatch") (odds = [ -1; -3; -5 ]);
  assert_true (fun () -> "case: negative evens mismatch") (evens = [ -2; -4 ]);

  print_endline "Partition tests passed!"

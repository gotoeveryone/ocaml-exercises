let () =
  assert (Sort.quicksort [] = []);

  assert (Sort.quicksort [ 1; 2; 3 ] = [ 1; 2; 3 ]);

  assert (
    Sort.quicksort [ 3; 6; 8; 10; 1; 5; 2; 1; 4 ]
    = [ 1; 1; 2; 3; 4; 5; 6; 8; 10 ]);

  print_endline "All tests passed!"

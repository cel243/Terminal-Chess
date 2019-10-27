open OUnit2
open Command 


let command_tests = [
  "'quit' is parsed as Quit" >:: (fun _ -> assert_equal Quit (parse "quit"));
  "'   Quit  ' is parsed as Quit" >:: 
  (fun _ -> assert_equal Quit (parse "    QuiT  ")); 
  "'draw' is parsed as Draw" >:: 
  (fun _ -> assert_equal Draw (parse "draw")); 
  "'   DrAw  ' is parsed as Draw" >:: 
  (fun _ -> assert_equal Draw (parse "    DrAw  ")); 
  "'A6 to B4' is parsed as Move ('A',6,'B',4)" >:: 
  (fun _ -> assert_equal Move ('A',6,'B',4) (parse "A6 to B4")); 
  "'  b3  TO  c8' is parsed as Move ('B',3,'C',8)" >:: 
  (fun _ -> assert_equal Move ('B',3,'C',8) (parse "  b3  TO  c8")); 
] 

let suite =
  "test suite"  >::: List.flatten [
    command_tests;
  ]

let _ = run_test_tt_main suite
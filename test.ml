open OUnit2
open Board
open Command 


let board_tests = [
  "The starting player is White" >:: 
  (fun _ -> assert_equal White 
      (get_current_player init_state));
  "The next player after White is Black" >:: 
  (fun _ -> assert_equal Black 
      (get_current_player (next_player init_state))); 
  "The next player after Black is White" >:: 
  (fun _ -> assert_equal White 
      (get_current_player (next_player (next_player init_state)))); 
  "The piece at A1 is a White Rook that hasn't moved" >:: 
  (fun _ -> assert_equal {p_type = Rook; col = White; has_moved = false} 
      (get_piece_at init_state 'A' 1)); 
  "'   DrAw  ' is parsed as Draw" >:: 
  (fun _ -> assert_equal 16 ((get_white_pieces init_state).length)); 
  "'A6 to B4' is parsed as Move ('A',6,'B',4)" >:: 
  (fun _ -> assert_equal Move ('A',6,'B',4) (parse "A6 to B4")); 
  "'  b3  TO  c8' is parsed as Move ('B',3,'C',8)" >:: 
  (fun _ -> assert_equal Move ('B',3,'C',8) (parse "  b3  TO  c8")); 
] 



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
    board_tests;
    command_tests;
  ]

let _ = run_test_tt_main suite
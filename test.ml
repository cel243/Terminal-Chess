open OUnit2
open Board
open Command 
open Logic 

let game_state = init_state () 

let board_tests = [
  "The starting player is White" >:: 
  (fun _ -> assert_equal White 
      (get_current_player game_state));
  "The next player works" >:: 
  (fun _ -> assert_equal ()
      (next_player game_state)); 
  "The next player after White is Black" >:: 
  (fun _ -> assert_equal Black 
      (get_current_player game_state)); 
  "The piece at A1 is a White Rook that hasn't moved" >:: 
  (fun _ -> assert_equal (Some {p_type = Rook; col = White; has_moved = false}) 
      (get_piece_at (init_state ()) 'A' 1)); 
  "White has 16 pieces" >:: 
  (fun _ -> assert_equal 16 (List.length (get_white_pieces (init_state ())))); 
  "Black has 16 pieces" >:: 
  (fun _ -> assert_equal 16 (List.length (get_black_pieces (init_state ())))); 
  "The move_piece works" >:: 
  (fun _ -> assert_equal ()
      (move_piece game_state 'A' 1 'H' 8));
  "The White rook has taken the Black Rook" >:: 
  (fun _ -> assert_equal (Some {p_type = Rook; col = White; has_moved = true})
      (get_piece_at game_state 'H' 8 )); 
  "Black has 15 pieces" >:: 
  (fun _ -> assert_equal 15 (List.length (get_black_pieces game_state)));  
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
  (fun _ -> assert_equal (Move ('A',6,'B',4)) (parse "A6 to B4")); 
  "'  b3  TO  c8' is parsed as Move ('B',3,'C',8)" >:: 
  (fun _ -> assert_equal (Move ('B',3,'C',8)) (parse "  b3  TO  c8")); 
] 

(* boards for testing: *)
let logic_GS = Board.init_state ()
let logic_GS_Black = Board.init_state () 
let () = Board.next_player logic_GS_Black

let logic_king = Board.init_state ()   
let () = Board.move_piece logic_king 'D' 8 'A' 5

let piece_move = Board.init_state () 

let logic_tests = [

  (* Tests of is_blocked *)
  "logic says move C2 to C3 is legal" >:: (fun _ -> 
      assert_equal Legal (process logic_GS (Move ('C',2,'C',3))));
  "logic moved c2 to c3 in the previous test" >:: (fun _ -> 
      assert_equal (Some {p_type=Pawn;col=White;has_moved=true})
        (get_piece_at logic_GS 'C' 3));
  "logic refuses to move A1 to A3 because the rook is blocked" >:: 
  (fun _ -> assert_equal (Illegal "This piece is blocked!") 
      (process logic_GS (Move ('A',1,'A',3))));
  "logic did not move the rook" >:: 
  (fun _ -> assert_equal None (get_piece_at logic_GS 'A' 4)); 
  "logic refuses to move C8 to F5 because the bishop is blocked" >:: 
  (fun _ -> assert_equal (Illegal "This piece is blocked!") 
      (process logic_GS_Black (Move ('C',8,'F',5))));
  "logic did not move the rook" >:: 
  (fun _ -> assert_equal None (get_piece_at logic_GS_Black 'F' 5)); 
  "logic says move D7 to D6 is legal" >:: (fun _ -> 
      assert_equal Legal (process logic_GS_Black (Move ('D',7,'D',6))));
  "logic moved d7 to d6 in the previous test" >:: (fun _ -> 
      assert_equal (Some {p_type=Pawn;col=Black;has_moved=true})
        (get_piece_at logic_GS_Black 'D' 6));
  "logic now allows bishop to move" >:: (fun _ -> 
      assert_equal Legal (process logic_GS_Black (Move ('C',8,'F',5))));
  "logic moved bishop" >:: (fun _ -> 
      assert_equal (Some {p_type=Bishop;col=Black;has_moved=true})
        (get_piece_at logic_GS_Black 'F' 5));
  (* Tests of detecting check *)
  "logic will not let pawn move because leaves king in check" >:: 
  (fun _ -> assert_equal (Illegal "You can't leave your king in check!") 
      (process logic_king (Move ('D',2,'D',3))));
  (let () = Board.move_piece logic_king 'D' 2 'D' 3 in 
   "if pawn moves, no white piece can move that doesn't stop check" >:: 
   (fun _ -> assert_equal (Illegal "You can't leave your king in check!") 
       (process logic_king (Move ('A',2,'A',3)))));
  "can move piece that prevents check" >:: 
  (fun _ -> assert_equal Legal (process logic_king (Move ('A',2,'A',3))));

  (* tests of piece movement *)
  (* PAWN *)
  "can move pawn forward 1" >:: 
  (fun _ -> assert_equal Legal (process piece_move (Move ('A',2,'A',3))));
  (let () = Board.move_piece piece_move 'A' 3 'D' 4 in 
   "cannot move pawn backwards" >:: 
   (fun _ -> assert_equal (Illegal  "This piece can't move like that!")
       (process piece_move (Move ('D',4,'D',3)))));
  "cannot move pawn sideways" >:: 
  (fun _ -> assert_equal (Illegal  "This piece can't move like that!")
      (process piece_move (Move ('D',4,'E',3))));
  "cannot move pawn diagonally" >:: 
  (fun _ -> assert_equal (Illegal  "This piece can't move like that!")
      (process piece_move (Move ('D',4,'E',5))));
  (let () = Board.move_piece piece_move 'E' 7 'E' 5 in 
   "pawn can move diagonally when taking other piece" >:: 
   (fun _ -> assert_equal Legal (process piece_move (Move ('D',4,'E',5)))));
  (* ROOK *)
  (let piece_move = Board.init_state () in 
   let () = Board.move_piece piece_move 'A' 1 'D' 4 in 
   "can move rook forward 2" >:: 
   (fun _ -> assert_equal Legal (process piece_move (Move ('D',4,'D',6)))));
  "can move rook to the right" >:: 
  (fun _ -> assert_equal Legal (process piece_move (Move ('D',4,'H',4))));
  "can move rook to the left" >:: 
  (fun _ -> assert_equal Legal (process piece_move (Move ('D',4,'A',4))));
  "can move rook backwards" >:: 
  (fun _ -> assert_equal Legal (process piece_move (Move ('D',4,'D',3))));
]

let suite =
  "test suite"  >::: List.flatten [
    board_tests;
    command_tests;
    logic_tests;
  ]

let _ = run_test_tt_main suite
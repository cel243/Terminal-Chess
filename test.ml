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
  (fun _ -> assert_equal 
      (Some {p_type = Rook; col = White; has_moved = false; points=5}) 
      (get_piece_at (init_state ()) 'A' 1)); 
  "White has 16 pieces" >:: 
  (fun _ -> assert_equal 16 (List.length (get_white_pieces (init_state ())))); 
  "Black has 16 pieces" >:: 
  (fun _ -> assert_equal 16 (List.length (get_black_pieces (init_state ())))); 
  "The move_piece works" >:: 
  (fun _ -> assert_equal ()
      (move_piece game_state 'A' 1 'H' 8));
  "The White rook has taken the Black Rook" >:: 
  (fun _ -> assert_equal 
      (Some {p_type = Rook; col = White; has_moved = true; points=5})
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

let logic_king_pawn = Board.init_state ()   
let () = Board.move_piece logic_king_pawn 'D' 8 'A' 5
let () = Board.move_piece logic_king_pawn 'D' 2 'D' 3

let piece_move_pawn = Board.init_state ()   
let () = Board.move_piece piece_move_pawn 'D' 2 'D' 3
let () = Board.move_piece piece_move_pawn 'C' 7 'C' 5

let piece_move_rook = Board.init_state ()   
let () = Board.move_piece piece_move_rook 'A' 1 'D' 4

let piece_move_knight = Board.init_state () 
let () = Board.move_piece piece_move_knight 'G' 1 'E' 5

let piece_move_bishop = Board.init_state () 
let () = Board.move_piece piece_move_bishop 'F' 1 'E' 5

let piece_move_queen = Board.init_state () 
let () = Board.move_piece piece_move_queen 'D' 1 'E' 5

let piece_move_king = Board.init_state () 
let () = Board.move_piece piece_move_king 'E' 1 'E' 4
let () = Board.move_piece piece_move_king 'F' 7 'E' 3

let piece_move = Board.init_state () 

let print_logic_res = function 
  | Legal -> "legal"
  | Illegal str -> "illegal "^str 
  | Terminate -> "terminate" 

let print_col = function 
  | White -> "white"
  | Black -> "black"

let logic_tests = [

  (* Tests of is_blocked *)
  "logic says move C2 to C3 is legal" >:: (fun _ -> 
      assert_equal Legal (process logic_GS (Move ('C',2,'C',3))));
  "logic moved c2 to c3 in the previous test" >:: (fun _ -> 
      assert_equal (Some {p_type=Pawn;col=White;has_moved=true; points=1})
        (get_piece_at logic_GS 'C' 3));
  "logic refuses to move A1 to A3 because the rook is blocked" >:: 
  (fun _ -> assert_equal (Illegal "This piece is blocked!") 
      (process logic_GS (Move ('A',1,'A',3))));
  "logic did not move the rook" >:: 
  (fun _ -> assert_equal None (get_piece_at logic_GS 'A' 4)); 
  "logic did not move the rook" >:: 
  (fun _ -> assert_equal None (get_piece_at logic_GS 'F' 5)); 
  "logic refuses to move C8 to F5 because the bishop is blocked" >:: 
  (fun _ -> assert_equal (Illegal "This piece is blocked!") 
      (process logic_GS_Black (Move ('C',8,'F',5))) 
      ~printer: print_logic_res);
  "logic says move D7 to D6 is legal" >:: (fun _ -> 
      assert_equal Legal (process logic_GS_Black (Move ('D',7,'D',6)))
        ~printer: print_logic_res);
  "logic now allows bishop to move" >:: (fun _ -> 
      assert_equal Legal (process logic_GS_Black (Move ('C',8,'F',5))));

  (** out of bounds tests *)
  "logic says move to out-of-bounds (file) is illegal" >:: (fun _ -> 
      assert_equal 
        (Illegal "You're attempting to access an out of bounds location!")
        (process logic_GS (Move ('A',2,'J',1))));
  "logic says move to out-of-bounds (rank) is illegal" >:: (fun _ -> 
      assert_equal 
        (Illegal "You're attempting to access an out of bounds location!") 
        (process logic_GS (Move ('A',2,'A',9))));
  "logic says move to is friendly-fire" >:: (fun _ -> 
      assert_equal 
        (Illegal "This is friendly fire!") 
        (process logic_GS (Move ('A',1,'A',2))));
  "logic says move from (opponent) isn't valid" >:: (fun _ -> 
      assert_equal 
        (Illegal "You don't have a piece in this square!") 
        (process logic_GS (Move ('A',7,'A',6))));
  "logic says move from (empty) isn't valid" >:: (fun _ -> 
      assert_equal 
        (Illegal "You don't have a piece in this square!") 
        (process logic_GS (Move ('A',4,'A',5))));


  (* Tests of detecting check *)
  "logic will not let pawn move because leaves king in check" >:: 
  (fun _ -> assert_equal (Illegal "You can't leave your king in check!") 
      (process logic_king (Move ('D',2,'D',3)))
      ~printer: print_logic_res) ;

  "if pawn moves, no white piece can move that doesn't stop check" >:: 
  (fun _ -> assert_equal (Illegal "You can't leave your king in check!") 
      (process logic_king_pawn (Move ('A',2,'A',3))));

  "can move piece that prevents check" >:: 
  (fun _ -> assert_equal Legal (process logic_king_pawn (Move ('C',2,'C',3)))
      ~printer: print_logic_res);


  (* tests of piece movement *)
  (* PAWN *)
  "can move pawn forward 1" >:: 
  (fun _ -> assert_equal Legal (process piece_move_pawn (Move ('D',3,'D',4)))
      ~printer: print_logic_res);
  "cannot move pawn backwards" >:: 
  (fun _ -> assert_equal (Illegal  "This piece can't move like that!")
      (process piece_move_pawn (Move ('D',4,'D',3)))
      ~printer: print_logic_res);
  "cannot move pawn sideways" >:: 
  (fun _ -> assert_equal (Illegal  "This piece can't move like that!")
      (process piece_move_pawn (Move ('D',4,'E',3)))
      ~printer: print_logic_res);
  "cannot move pawn diagonally" >:: 
  (fun _ -> assert_equal (Illegal  "This piece can't move like that!")
      (process piece_move_pawn (Move ('D',4,'E',5)))
      ~printer: print_logic_res); 
  "pawn can move diagonally when taking other piece" >:: 
  (fun _ -> assert_equal Legal (process piece_move_pawn (Move ('D',4,'C',5)))
      ~printer: print_logic_res) ;


  (* ROOK *)
  "can move rook forward 2" >:: 
  (fun _ -> assert_equal Legal 
      (process (copy_board piece_move_rook) (Move ('D',4,'D',6))));
  "can move rook to the right" >:: 
  (fun _ -> assert_equal Legal 
      (process (copy_board piece_move_rook)  (Move ('D',4,'H',4))));
  "can move rook to the left" >:: 
  (fun _ -> assert_equal Legal 
      (process (copy_board piece_move_rook) (Move ('D',4,'A',4))));
  "can move rook backwards" >:: 
  (fun _ -> assert_equal Legal 
      (process (copy_board piece_move_rook) (Move ('D',4,'D',3))));

  (* KNIGHT *)
  "can move knight forward 1 left 2" >:: 
  (fun _ -> assert_equal Legal 
      (process (copy_board piece_move_knight) (Move ('E',5,'C',6))));
  "can move knight forward 1 right 2" >:: 
  (fun _ -> assert_equal Legal 
      (process (copy_board piece_move_knight) (Move ('E',5,'G',6))));
  "can move knight forward 2 left 1" >:: 
  (fun _ -> assert_equal Legal 
      (process (copy_board piece_move_knight) (Move ('E',5,'D',7))));
  "can move knight forward 2 right 1" >:: 
  (fun _ -> assert_equal Legal 
      (process (copy_board piece_move_knight) (Move ('E',5,'F',7))));
  "can move knight back 2 right 1" >:: 
  (fun _ -> assert_equal Legal 
      (process (copy_board piece_move_knight) (Move ('E',5,'F',3))));
  "cannot move knight like a bishop" >:: 
  (fun _ -> assert_equal (Illegal  "This piece can't move like that!")
      (process (copy_board piece_move_knight) (Move ('E',5,'C',3))));
  "can move knight back 1 left 2" >:: 
  (fun _ -> assert_equal Legal 
      (process (copy_board piece_move_knight) (Move ('E',5,'C',4))));
  "can move knight back 1 right 2" >:: 
  (fun _ -> assert_equal Legal 
      (process (copy_board piece_move_knight) (Move ('E',5,'G',4))));

  (* BISHOP *)
  "can move bishop northeast 1" >:: 
  (fun _ -> assert_equal Legal 
      (process (copy_board piece_move_bishop) (Move ('E',5,'F',6))));
  "can move bishop northeast 2 and take a piece" >:: 
  (fun _ -> assert_equal Legal 
      (process (copy_board piece_move_bishop) (Move ('E',5,'G',7))));
  "can move bishop northwest 2 and take a piece" >:: 
  (fun _ -> assert_equal Legal 
      (process (copy_board piece_move_bishop) (Move ('E',5,'C',7))));
  "can move bishop southeast 2" >:: 
  (fun _ -> assert_equal Legal 
      (process (copy_board piece_move_bishop) (Move ('E',5,'G',3))));
  "can move bishop southwest 2" >:: 
  (fun _ -> assert_equal Legal 
      (process (copy_board piece_move_bishop) (Move ('E',5,'C',3))));
  "cannot move bishop to a random spot" >:: 
  (fun _ -> assert_equal (Illegal  "This piece can't move like that!")
      (process (copy_board piece_move_bishop) (Move ('E',5,'A',6))));

  (* QUEEN *)
  "can move queen forward 2 and take a piece" >:: 
  (fun _ -> assert_equal Legal
      (process (copy_board piece_move_queen) (Move ('E',5,'E',7))));
  "can move queen northeast 2 and take a piece" >:: 
  (fun _ -> assert_equal Legal
      (process (copy_board piece_move_queen) (Move ('E',5,'G',7))));
  "can move queen northwest 2 and take a piece" >:: 
  (fun _ -> assert_equal Legal
      (process (copy_board piece_move_queen) (Move ('E',5,'C',7))));
  "can move queen right 2" >:: 
  (fun _ -> assert_equal Legal
      (process (copy_board piece_move_queen) (Move ('E',5,'H',5))));
  "can move queen left 1" >:: 
  (fun _ -> assert_equal Legal
      (process (copy_board piece_move_queen) (Move ('E',5,'D',5))));
  "can move queen southeast 1" >:: 
  (fun _ -> assert_equal Legal
      (process (copy_board piece_move_queen) (Move ('E',5,'F',4))));
  "can move queen back 2" >:: 
  (fun _ -> assert_equal Legal
      (process (copy_board piece_move_queen) (Move ('E',5,'E',3))));
  "can move queen southwest 2" >:: 
  (fun _ -> assert_equal Legal
      (process (copy_board piece_move_queen) (Move ('E',5,'C',3))));
  "can move queen left 4" >:: 
  (fun _ -> assert_equal Legal
      (process (copy_board piece_move_queen) (Move ('E',5,'A',5))));
  "cannot move queen like a knight" >:: 
  (fun _ -> assert_equal (Illegal  "This piece can't move like that!")
      (process (copy_board piece_move_queen) (Move ('E',5,'D',3))));

  (* KING *)
  "can move king right 1" >:: 
  (fun _ -> assert_equal Legal
      (process (copy_board piece_move_king) (Move ('E',4,'F',4))));
  "can move king northeast 1" >:: 
  (fun _ -> assert_equal Legal
      (process (copy_board piece_move_king) (Move ('E',4,'F',5))));
  "can move king southeast 1" >:: 
  (fun _ -> assert_equal Legal
      (process (copy_board piece_move_king) (Move ('E',4,'F',3))));
  "can move king south 1 and take a piece" >:: 
  (fun _ -> assert_equal Legal
      (process (copy_board piece_move_king) (Move ('E',4,'E',3))));
  "can move king wouthwest 1" >:: 
  (fun _ -> assert_equal Legal
      (process (copy_board piece_move_king) (Move ('E',4,'D',3))));
  "can move king west 1" >:: 
  (fun _ -> assert_equal Legal
      (process (copy_board piece_move_king) (Move ('E',4,'D',4))));
  "can move king northwest 1" >:: 
  (fun _ -> assert_equal Legal
      (process (copy_board piece_move_king) (Move ('E',4,'D',5))));
  "can move king forward 1" >:: 
  (fun _ -> assert_equal Legal
      (process (copy_board piece_move_king) (Move ('E',4,'E',5))));
  "cannot move king randomly" >:: 
  (fun _ -> assert_equal (Illegal  "This piece can't move like that!")
      (process (copy_board piece_move_king) (Move ('E',4,'G',5))));
  "cannot move king left 4" >:: 
  (fun _ -> assert_equal (Illegal  "This piece can't move like that!")
      (process (copy_board piece_move_king) (Move ('E',4,'A',4)))); 

]

let suite =
  "test suite"  >::: List.flatten [
    board_tests;
    command_tests;
    logic_tests;
  ]

let _ = run_test_tt_main suite
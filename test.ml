open OUnit2
open Board
open Command 
open Logic 

(* HELPERS *)

let print_logic_res = function 
  | Legal -> "legal"
  | Illegal str -> "illegal "^str 
  | Checkmate -> "checkmate" 
  | Stalemate -> "stalemate"
  | Draw -> "draw"

let print_col = function 
  | White -> "white"
  | Black -> "black"

let make_cmmd_test name cmmd input = 
  name >:: 
  (fun _ -> assert_equal cmmd (parse input))

let logic_move_test name board c1 i1 c2 i2 outcome = 
  name >:: (fun _ -> 
      assert_equal outcome 
        (process board (Command.Move (c1,i1,c2,i2)))
        ~printer: print_logic_res)

(* BOARDS USED FOR TESTING *)

let capture_board = FileHandler.load_game "CAPTURE_TEST.json"
let checkmate_brd = FileHandler.load_game "CHECKMATE_TEST.json"
let stalemate_brd = FileHandler.load_game "STALEMATE_TEST.json"

let game_state = init_state () 

let logic_GS = Board.init_state ()

let logic_GS_Black = Board.init_state () 
let _ = Board.next_player logic_GS_Black

let logic_king = Board.init_state ()   
let _ = Board.move_piece logic_king 'D' 8 'A' 5

let logic_king_pawn = Board.init_state ()   
let _ = 
  Board.move_piece logic_king_pawn 'D' 8 'A' 5;
  Board.move_piece logic_king_pawn 'D' 2 'D' 3

let piece_move_pawn = Board.init_state ()   
let _ = 
  Board.move_piece piece_move_pawn 'D' 2 'D' 3;
  Board.move_piece piece_move_pawn 'C' 7 'C' 5

let piece_move_rook = Board.init_state ()   
let _ = Board.move_piece piece_move_rook 'A' 1 'D' 4

let piece_move_knight = Board.init_state () 
let _ = Board.move_piece piece_move_knight 'G' 1 'E' 5

let piece_move_bishop = Board.init_state () 
let _ = Board.move_piece piece_move_bishop 'F' 1 'E' 5

let piece_move_queen = Board.init_state () 
let _ = Board.move_piece piece_move_queen 'D' 1 'E' 5

let piece_move_king = Board.init_state () 
let _ = Board.move_piece piece_move_king 'E' 1 'E' 4
let _ = Board.move_piece piece_move_king 'F' 7 'E' 3

let piece_move = Board.init_state () 


let support_tests = [

]

let board_tests = [
  (* capture list tests *)
  "The starting captured pieces for white is empty" >:: 
  (fun _ -> assert_equal [] 
      (get_captured_pieces game_state White));
  "The starting captured pieces for black is empty" >:: 
  (fun _ -> assert_equal [] 
      (get_captured_pieces game_state Black));
  "The captured pieces for capture_board white is correct" >:: 
  (fun _ -> assert_equal [(Pawn, 2); (Bishop, 1)] 
      (List.sort_uniq Stdlib.compare 
         (get_captured_pieces capture_board White)));
  "The captured pieces for capture_board black is correct" >:: 
  (fun _ -> assert_equal [(Queen, 1)] 
      (get_captured_pieces capture_board Black));


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
  make_cmmd_test {|"resign" is parsed as Resign|}  Resign "resign";
  make_cmmd_test {|'   Resign  ' is parsed as Resign|} Resign "    ReSiGn  "; 
  make_cmmd_test {|'draw' is parsed as Draw|} Command.Draw "draw"; 
  make_cmmd_test {|'   DrAw  ' is parsed as Draw|} Command.Draw  "    DrAw  "; 
  make_cmmd_test {|'A6 to B4' is parsed as Move ('A',6,'B',4)|} 
    (Move ('A',6,'B',4)) "A6 to B4"; 
  make_cmmd_test {|'  b3  TO  c8' is parsed as Move ('B',3,'C',8)|} 
    (Move ('B',3,'C',8)) "  b3  TO  c8"; 
  make_cmmd_test {|"help" parsed as Help|}  Help "help";
  make_cmmd_test {|"captured" parsed as Captured|}  Captured "captured";
  make_cmmd_test {|"log" parsed as Log|}  Log "log";
  make_cmmd_test {|"save as test" parsed as Save "TEST"|}  
    (Save "TEST") "save as test";
  make_cmmd_test {|"a5" parsed as PSupport LegalMoves ('A',5)|}  
    (PSupport (LegalMoves ('A',5))) "a5";
  make_cmmd_test 
    ({|"a5 if c1 to c2" parsed as|} 
     ^{|PSupport LegalMovesIF ('A',5, 'C', 1, 'C', 2)|})  
    (PSupport (LegalMovesIF ('A',5, 'C', 1, 'C', 2))) 
    "a5 if c1 to c2" ;
  make_cmmd_test {|"under attack" parsed as PSupport UnderAttack|}  
    (PSupport UnderAttack) "under attack";
  make_cmmd_test 
    ({|"under attack if c1 to c2" parsed as|} 
     ^{|PSupport UnderAttackIF ('C', 1, 'C', 2)|})  
    (PSupport (UnderAttackIF ('C', 1, 'C', 2))) 
    "under attack if c1 to c2" ;
  make_cmmd_test {|"can attack" parsed as PSupport CanAttack|}  
    (PSupport CanAttack) "can attack";
  make_cmmd_test 
    ({|"can attack if b1 to c2" parsed as|} 
     ^{|PSupport UnderAttackIF ('B', 1, 'C', 2)|})  
    (PSupport (CanAttackIF ('B', 1, 'C', 2))) 
    "can attack if b1 to c2" ;
  make_cmmd_test {|"attackers f6" parsed as PSupport Attackers ('F',6)|}  
    (PSupport (Attackers ('F',6))) "attackers f6";
  make_cmmd_test 
    ({|"attackers a5 if c1 to c2" parsed as|} 
     ^{|PSupport AttackersIF ('A',5, 'C', 1, 'C', 2)|})  
    (PSupport (AttackersIF ('A',5, 'C', 1, 'C', 2))) 
    "attackers a5 if c1 to c2" ;
  "gibberish is invalid" >:: (fun _ -> 
      assert_raises Invalid 
        (fun () -> (parse "awsdfcvgbhnjkml"))); 
  "blank is invalid" >:: (fun _ -> 
      assert_raises Invalid 
        (fun () -> (parse "      "))); 
] 

let logic_tests = [

  (* test of checkmate/stalemate *)
  logic_move_test "logic detects checkmate" checkmate_brd 'D' 8 'H' 4 Checkmate;
  logic_move_test "logic detects stalemate" stalemate_brd 'C' 8 'E' 6 Stalemate;

  (* Tests of is_blocked *)
  logic_move_test 
    "logic says move C2 to C3 is legal" logic_GS 'C' 2 'C' 3 Legal;

  "logic moved c2 to c3 in the previous test" >:: (fun _ -> 
      assert_equal (Some {p_type=Pawn;col=White;has_moved=true; points=1})
        (get_piece_at logic_GS 'C' 3));

  logic_move_test 
    "logic refuses to move A1 to A3 because the rook is blocked" 
    logic_GS 'A' 1 'A' 3 (Illegal "This piece is blocked!"); 

  "logic did not move the rook" >:: 
  (fun _ -> assert_equal None (get_piece_at logic_GS 'A' 4)); 

  logic_move_test
    "logic refuses to move C8 to F5 because the bishop is blocked" logic_GS_Black
    'C' 8 'F' 5 (Illegal "This piece is blocked!");
  "logic did not move the bishop" >:: 
  (fun _ -> assert_equal None (get_piece_at logic_GS 'F' 5)); 

  logic_move_test
    "logic says move D7 to D6 is legal" logic_GS_Black
    'D' 7 'D' 6 Legal;
  logic_move_test
    "logic now allows bishop to move" logic_GS_Black 'C' 8 'F' 5 Legal;

  (** out of bounds tests *)
  logic_move_test
    "logic says move to out-of-bounds (file) is illegal" logic_GS 
    'A' 2 'J' 1 
    (Illegal "You're attempting to access an out of bounds location!"); 
  logic_move_test
    "logic says move to out-of-bounds (rank) is illegal" logic_GS 
    'A' 2 'A' 9
    (Illegal "You're attempting to access an out of bounds location!"); 
  logic_move_test 
    "logic says move to is friendly-fire" logic_GS 
    'A' 1 'A' 2 (Illegal "This is friendly fire!"); 
  logic_move_test
    "logic says move from (opponent) isn't valid" logic_GS 
    'A' 7 'A' 6 (Illegal "You don't have a piece in this square!"); 
  logic_move_test
    "logic says move from (empty) isn't valid" logic_GS 
    'A' 4 'A' 5 (Illegal "You don't have a piece in this square!"); 


  (* Tests of detecting check *)
  logic_move_test
    "logic will not let pawn move because leaves king in check" logic_king 
    'D' 2 'D' 3 (Illegal "You can't leave your king in check!"); 
  logic_move_test
    "if pawn moves, no white piece can move that doesn't stop check" 
    logic_king_pawn 'A' 2 'A' 3
    (Illegal "You can't leave your king in check!"); 
  logic_move_test
    "can move piece that prevents check" logic_king_pawn 'C' 2 'C' 3 Legal ;


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
    support_tests; 
  ]

let _ = run_test_tt_main suite
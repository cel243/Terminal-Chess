open OUnit2
open Board
open Command 
open Logic 
open Support 


(* BOARDS USED FOR TESTING *)

let capture_board = FileHandler.load_game "CAPTURE_TEST.json"
let checkmate_brd = FileHandler.load_game "CHECKMATE_TEST.json"
let stalemate_brd = FileHandler.load_game "STALEMATE_TEST.json"
let support_brd = FileHandler.load_game "SUPPORT_TEST.json"
let file_brd = FileHandler.load_game "FILE_TEST.json"

let file_brd_manual = Board.init_state () 
let  _ = ignore (Logic.process file_brd_manual (Move ('E' ,2 ,'E', 4)));
  Board.next_player file_brd_manual;
  ignore (Logic.process file_brd_manual (Move ('A' ,7 ,'A', 6)));
  Board.next_player file_brd_manual;
  ignore (Logic.process file_brd_manual (Move ('F' ,1 ,'C', 4)));
  Board.next_player file_brd_manual

let game_state = Board.init_state () 

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

(** [list_eq l1 l2] is true if [l1] and [l2] contain the same 
    elements.  *)
let list_eq l1 l2 = 
  (List.sort_uniq Stdlib.compare l1) = (List.sort_uniq Stdlib.compare l2)

let rec print_list_aux ls = 
  match ls with 
  | [] -> ""
  | (c,i)::t -> 
    "('"^(Char.escaped c)^"', "^(string_of_int i)^"); "^(print_list_aux t)

(** [print_list ls] turns a list of char/int tuples into 
    a string  *)
let print_list ls = "["^(print_list_aux ls)^"]"

let make_cmmd_test name cmmd input = 
  name >:: 
  (fun _ -> assert_equal cmmd (parse input))

let make_support_test name cmmd locs = 
  let output,_,_,_ = handle_player_support support_brd cmmd in 
  name >:: 
  (fun _ -> assert_equal locs output ~cmp: list_eq ~printer: print_list) 

let logic_move_test name board c1 i1 c2 i2 outcome = 
  name >:: (fun _ -> 
      assert_equal outcome 
        (process board (Command.Move (c1,i1,c2,i2)))
        ~printer: print_logic_res)

let logic_move_test_copybrd name board c1 i1 c2 i2 outcome = 
  name >:: (fun _ -> 
      assert_equal outcome 
        (process (Board.copy_board board) (Command.Move (c1,i1,c2,i2)))
        ~printer: print_logic_res)

let file_tests = [
  "Teh board loaded from the file is equal to the one you get manually" >:: 
  (fun _ -> assert_equal file_brd file_brd_manual) ;
]

let support_tests_clean = [
  "legal moves of black bishop correct", LegalMoves ('D',7), 
  [('D',7);('C',6);('B',5);('A',4)];
  "legal moves of white knight correct", LegalMoves ('G',1), 
  [('G',1);('F',3);('H',3)];
  "legal moves of blocked piece correct", LegalMoves ('A',8), [('A',8)];
  "under attack correct", UnderAttack, [('A',4);('C',5)];
  "can attack correct", CanAttack, [('A',7);('B',6);('D',6);('D',7)];
  "attacker of queen is bishop", Attackers ('A',4), [('D',7)];
  "two pawns attacking white pawn", Attackers ('C',5), [('D',6);('B',6)];

  "if queen takes bishop, she is under attack, but the pawn is no longer"
  ^" under attack becuase the king is in check.", 
  UnderAttackIF ('A',4,'D',7), [('D',7)];
  "legal moves of black queen if white queen puts king in check is only"
  ^" to take the white queen", LegalMovesIF ('D',8,'A',4,'D',7), 
  [('D',8);('D',7)];
  "if white queen moves to h4, then a7 no longer under attack, h7 is. ",
  CanAttackIF ('A',4,'H',4), [('H',7);('E',7);('B',6);('D',6)];
  "if the white queen moves to a5, then the attackers of b6 are the queen"
  ^" and the white pawn.", AttackersIF ('B',6,'A',4,'A',5), [('A',5);('C',5)]; 
]

let support_tests = 
  List.map (fun (n,c,l) -> make_support_test n c l) support_tests_clean


let command_tests_clean = [
  {|"resign" is parsed as Resign|},  Resign, "resign";
  {|'   Resign  ' is parsed as Resign|}, Resign, "    ReSiGn  "; 
  {|'draw' is parsed as Draw|}, Command.Draw, "draw"; 
  {|'   DrAw  ' is parsed as Draw|}, Command.Draw,  "    DrAw  "; 
  {|'A6 to B4' is parsed as Move ('A',6,'B',4)|}, (Move ('A',6,'B',4)), 
  "A6 to B4"; 
  {|'  b3  TO  c8' is parsed as Move ('B',3,'C',8)|}, (Move ('B',3,'C',8)), 
  "  b3  TO  c8"; 
  {|"help" parsed as Help|},  Help, "help";
  {|"captured" parsed as Captured|},  Captured, "captured";
  {|"log" parsed as Log|},  Log, "log";
  {|"save as test" parsed as Save "TEST"|},  (Save "TEST"), "save as test";
  {|"a5" parsed as PSupport LegalMoves ('A',5)|}, 
  (PSupport (LegalMoves ('A',5))), "a5";

  {|"a5 if c1 to c2" parsed as PSupport LegalMovesIF ('A',5, 'C', 1, 'C', 2)|},  
  (PSupport (LegalMovesIF ('A',5, 'C', 1, 'C', 2))), "a5 if c1 to c2" ;
  {|"under attack" parsed as PSupport UnderAttack|}, (PSupport UnderAttack), 
  "under attack";
  {|"under attack if c1 to c2" parsed as|}
  ^{|PSupport UnderAttackIF ('C', 1, 'C', 2)|},  
  (PSupport (UnderAttackIF ('C', 1, 'C', 2))), "under attack if c1 to c2" ;
  {|"can attack" parsed as PSupport CanAttack|}, (PSupport CanAttack), 
  "can attack"; 
  {|"can attack if b1 to c2" parsed as|} 
  ^{|PSupport UnderAttackIF ('B', 1, 'C', 2)|},  
  (PSupport (CanAttackIF ('B', 1, 'C', 2))), "can attack if b1 to c2" ;
  {|"attackers f6" parsed as PSupport Attackers ('F',6)|},  
  (PSupport (Attackers ('F',6))), "attackers f6"; 
  {|"attackers a5 if c1 to c2" parsed as|} 
  ^{|PSupport AttackersIF ('A',5, 'C', 1, 'C', 2)|},  
  (PSupport (AttackersIF ('A',5, 'C', 1, 'C', 2))), "attackers a5 if c1 to c2" 
] 

let command_tests = 
  (List.map (fun (n,c,s) -> make_cmmd_test n c s) command_tests_clean)
  @ ["gibberish is invalid" >:: (fun _ -> 
      assert_raises Invalid 
        (fun () -> (parse "awsdfcvgbhnjkml"))); 
     "blank is invalid" >:: (fun _ -> 
         assert_raises Invalid 
           (fun () -> (parse "      ")))]

let logic_tests_clean_WITH_effects = [

  (* test of checkmate/stalemate *)
  "logic detects checkmate", checkmate_brd, 'D', 8, 'H', 4, Checkmate;
  "logic detects stalemate", stalemate_brd, 'C', 8, 'E', 6, Stalemate;

  (* Tests of is_blocked *) 
  "logic says move C2 to C3 is legal", logic_GS, 'C', 2, 'C', 3, Legal; 
  "logic refuses to move A1 to A3 because the rook is blocked", 
  logic_GS ,'A', 1 ,'A', 3 ,(Illegal "This piece is blocked!"); 

  "logic refuses to move C8 to F5 because the bishop is blocked", 
  logic_GS_Black,'C', 8 ,'F' ,5 ,(Illegal "This piece is blocked!");
  "logic says move D7 to D6 is legal" ,logic_GS_Black,
  'D', 7 ,'D', 6 ,Legal;
  "logic now allows bishop to move", logic_GS_Black, 'C' ,8 ,'F', 5 ,Legal;

  (** out of bounds tests *)
  "logic says move to out-of-bounds (file) is illegal" ,logic_GS ,
  'A' ,2 ,'J' ,1 ,
  (Illegal "You're attempting to access an out of bounds location!"); 
  "logic says move to out-of-bounds (rank) is illegal" ,logic_GS ,
  'A' ,2, 'A', 9,
  (Illegal "You're attempting to access an out of bounds location!");  
  "logic says move to is friendly-fire" ,logic_GS ,
  'A' ,1 ,'A', 2 ,(Illegal "This is friendly fire!"); 
  "logic says move from (opponent) isn't valid", logic_GS ,
  'A', 7 ,'A' ,6 ,(Illegal "You don't have a piece in this square!"); 
  "logic says move from (empty) isn't valid", logic_GS ,
  'A' ,4, 'A' ,5 ,(Illegal "You don't have a piece in this square!"); 


  (* Tests of detecting check *)
  "logic will not let pawn move because leaves king in check", logic_king ,
  'D', 2 ,'D', 3 ,(Illegal "You can't leave your king in check!"); 
  "if pawn moves, no white piece can move that doesn't stop check" ,
  logic_king_pawn ,'A' ,2, 'A' ,3,
  (Illegal "You can't leave your king in check!"); 
  "can move piece that prevents check", logic_king_pawn, 'C' ,2, 'C' ,3 ,Legal;


  (* tests of piece movement *)
  (* PAWN *)
  "can move pawn forward 1", piece_move_pawn, 'D',3,'D',4, Legal;
  "can move pawn backwards", piece_move_pawn, 'D',4,'D',3, 
  (Illegal  "This piece can't move like that!");
  "cannot move pawn sideways", piece_move_pawn, 'D',4,'E',3, 
  (Illegal  "This piece can't move like that!");
  "cannot move pawn diagonally", piece_move_pawn, 'D',4,'E',5, 
  (Illegal  "This piece can't move like that!");
  "pawn can move diagonally when taking other piece", piece_move_pawn, 
  'D',4,'C',5, Legal; 
]

let logic_tests_clean_NO_effects = [
  (* ROOK *)
  "can move rook forward 2", piece_move_rook,'D',4,'D',6, Legal; 
  "can move rook to the right", piece_move_rook,'D',4,'H',4, Legal; 
  "can move rook to the left", piece_move_rook, 'D',4,'A',4, Legal; 
  "can move rook backwards",piece_move_rook,'D',4,'D',3, Legal; 

  (* KNIGHT *)
  "can move knight forward 1 left 2", piece_move_knight,'E',5,'C',6, Legal; 
  "can move knight forward 1 right 2",piece_move_knight,'E',5,'G',6, Legal; 
  "can move knight forward 2 left 1", piece_move_knight, 'E',5,'D',7, Legal;
  "can move knight forward 2 right 1", piece_move_knight,'E',5,'F',7, Legal;
  "can move knight back 2 right 1",piece_move_knight,'E',5,'F',3, Legal;
  "cannot move knight like a bishop",piece_move_knight,'E',5,'C',3,
  (Illegal  "This piece can't move like that!");
  "can move knight back 1 left 2",piece_move_knight,'E',5,'C',4, Legal;
  "can move knight back 1 right 2",piece_move_knight,'E',5,'G',4, Legal;

  (* BISHOP *)
  "can move bishop northeast 1",piece_move_bishop,'E',5,'F',6, Legal;
  "can move bishop northeast 2 and take a piece",piece_move_bishop,
  'E',5,'G',7, Legal;
  "can move bishop northwest 2 and take a piece",piece_move_bishop, 
  'E',5,'C',7, Legal;
  "can move bishop southeast 2",piece_move_bishop,'E',5,'G',3, Legal;
  "can move bishop southwest 2",piece_move_bishop,'E',5,'C',3, Legal;
  "cannot move bishop to a random spot",piece_move_bishop,'E',5,'A',6,
  (Illegal  "This piece can't move like that!");

  (* QUEEN *)
  "can move queen forward 2 and take a piece",piece_move_queen, 
  'E',5,'E',7, Legal;
  "can move queen northeast 2 and take a piece",piece_move_queen,
  'E',5,'G',7, Legal;
  "can move queen northwest 2 and take a piece",piece_move_queen, 
  'E',5,'C',7, Legal;
  "can move queen right 2",piece_move_queen,'E',5,'H',5, Legal;
  "can move queen left 1",piece_move_queen,'E',5,'D',5, Legal;
  "can move queen southeast 1",piece_move_queen, 'E',5,'F',4, Legal;
  "can move queen back 2",piece_move_queen, 'E',5,'E',3, Legal;
  "can move queen southwest 2",piece_move_queen,'E',5,'C',3, Legal;
  "can move queen left 4", piece_move_queen,'E',5,'A',5,Legal;
  "cannot move queen like a knight", piece_move_queen,'E',5,'D',3,
  (Illegal  "This piece can't move like that!");

  (* KING *)
  "can move king right 1", piece_move_king,'E',4,'F',4, Legal;
  "can move king northeast 1", piece_move_king,'E',4,'F',5, Legal;
  "can move king southeast 1", piece_move_king, 'E',4,'F',3, Legal ;
  "can move king south 1 and take a piece", piece_move_king,'E',4,'E',3,Legal;
  "can move king wouthwest 1", piece_move_king,'E',4,'D',3,Legal;
  "can move king west 1", piece_move_king,'E',4,'D',4,Legal;
  "can move king northwest 1", piece_move_king,'E',4,'D',5,Legal;
  "can move king forward 1", piece_move_king,'E',4,'E',5,Legal;
  "cannot move king randomly", piece_move_king,'E',4,'G',5,
  (Illegal  "This piece can't move like that!");
  "cannot move king left 4", piece_move_king,'E',4,'A',4,
  (Illegal  "This piece can't move like that!");
]

let logic_tests_unclean = [

]

let logic_tests = 
  ( List.map 
      (fun (name,brd,c1,i1,c2,i2,res) -> 
         logic_move_test name brd c1 i1 c2 i2 res)
      logic_tests_clean_WITH_effects ) 
  @ 
  ( List.map 
      (fun (name,brd,c1,i1,c2,i2,res) -> 
         logic_move_test_copybrd name brd c1 i1 c2 i2 res)
      logic_tests_clean_NO_effects ) 

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

let suite =
  "test suite"  >::: List.flatten [
    board_tests;
    command_tests;
    logic_tests;
    support_tests; 
    file_tests;
  ]

let _ = run_test_tt_main suite
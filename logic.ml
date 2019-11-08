
open ANSITerminal

(** [is_valid_location c i] is true if [c] is a member of the set {'A',...,'Z'} 
    and [i] is within the range [1, 8]; otherwise, it is false. *)
let is_valid_location c i =
  (let n = int_of_char c in (n >= 65 && n <= 72)) && (i >= 1 && i <= 8) 

(** [is_curr_players b c i] is true if there is a piece at the location (c,i) 
    and if that piece's color is equal to the color of the current player. If
    there is no piece at that location, it is false. 
    Requires: (c,i) is a valid location. *)
let is_curr_players brd c i = 
  match (Board.get_piece_at brd c i) with
  | None -> false
  | Some {col} -> col = (Board.get_current_player brd)

(** [step curr dest] is the next value of [curr] in the stepwise
    sweep. *)
let step curr dest = 
  if curr < dest then curr + 1
  else if curr > dest then curr - 1
  else curr 

(** [stepwise brd curr_c curr_i dest_c dest_i] is [true] if 
    there is a chess piece in the chess piece's path from its current
    location to its target destination, not including the target 
    destination. *)
let rec stepwise brd curr_c curr_i dest_c dest_i = 
  if curr_c = dest_c && curr_i = dest_i then false 
  else 
    match Board.get_piece_at brd (char_of_int curr_c) curr_i with
    | None -> stepwise brd (step curr_c dest_c) (step curr_i dest_i) 
                dest_c dest_i 
    | _ -> true 

(** [is_blocked brd c1 i1 c2 i2] is [true] if there is a chess piece in
    between where this piece currently is (c1, i1) and where it is
    trying to move (c2, i2), not including the start and end locations. 
    Otherwise it is [false]. If the piece at (c1, i1) is a knight, then 
    [is_blocked] is always [false]. 
    Requires: there is a piece of the appropriate color at (c1, i1), and 
    neither (c1, i1) nor (c2, i2) is out of bounds.  *)
let is_blocked brd st_c st_i dest_c dest_i = 
  match Board.get_piece_at brd st_c st_i with 
  | None -> failwith "precondition violated in is_blocked" 
  | Some {p_type=Knight} -> false 
  | _ -> stepwise brd 
           (step (int_of_char st_c) (int_of_char dest_c)) (step st_i dest_i) 
           (int_of_char dest_c) dest_i

(** [legal_for_pawn piece char_m c1 i1 c2 i2 brd] is [true] if this Pawn, 
    [piece], can legally move from [c1,i1] to [c2,i2] given the rules of 
    pawns. *)
let legal_for_pawn piece char_m c1 i1 c2 i2 brd = 
  let pawn_w = i1-i2=(-1) && piece.Board.col = White in
  let pawn_b = i1-i2=1 && piece.Board.col = Black in
  (pawn_b && char_m=0) || 
  (pawn_w && char_m=0) ||
  (((pawn_w||pawn_b) && char_m=1) && 
   match (Board.get_piece_at brd c2 i2) with 
   | None -> false
   | Some p -> p.Board.col != piece.Board.col)

(** [legal_for_piece c1 i1 c2 i2 brd] is [true] if game_piece at [c1] [i1] can
    legally move from [c1,i1] to [c2,i2] given the rules of the type 
    of said piece 
    Requires: there is a piece at [c1,i1] *)
let legal_for_piece c1 i1 c2 i2 brd = 
  match Board.get_piece_at brd c1 i1 with
  | None -> failwith "precondition violated in legal_for_piece"
  | Some piece ->
    let char_m = abs((int_of_char c1)-(int_of_char c2)) in
    let int_m = abs (i1-i2) in
    not(char_m + int_m = 0) &&
    match piece.Board.p_type with
    | Pawn -> legal_for_pawn piece char_m c1 i1 c2 i2 brd
    | Knight -> (char_m + int_m = 3) && (abs(int_m-char_m)=1) 
    | Rook -> ((char_m = 0) && (int_m > 0)) || ((char_m > 0) && (int_m = 0))
    | Bishop -> (char_m = int_m)
    | King -> (char_m < 2) && (int_m < 2)
    | Queen -> 
      ((char_m = 0) && (int_m > 0)) || 
      ((char_m > 0) && (int_m = 0)) ||
      (char_m = int_m)

(** [check_opp_attacks brd op_ls king_c king_i] is [true] if any of 
    the opposing player's pieces have the ability to take the current
    player's king in this current board state, and [false] 
    otherwise  *)
let rec check_opp_attacks brd op_ls king_c king_i = 
  match op_ls with 
  | [] -> false 
  | (p,c,i)::t -> 
    if legal_for_piece c i king_c king_i brd
    &&  not (is_blocked brd c i king_c king_i)
    then true 
    else check_opp_attacks brd t king_c king_i

(** [king_loc brd] is the location [(c,i)] of the current player's
    king  *)
let king_loc brd = 
  let k_ls = (
    (match Board.get_current_player brd with 
     | White -> Board.get_white_pieces brd
     | Black -> Board.get_black_pieces brd ) 
    |> List.filter (fun (Board.{p_type},_,_) -> p_type = King) 
  ) in 
  match k_ls with 
  | [(k, c, i)] -> (c,i) 
  | _ -> failwith "impossible 2" 

(** [king_in_check brd c1 i1 c2 i2] is [true] if the current 
    player's king is in check, and [false] otherwise.  *)
let king_in_check brd = 
  let op_piece_ls = (
    match Board.get_current_player brd with 
    | White -> Board.get_black_pieces brd 
    | Black -> Board.get_white_pieces brd) in 
  let king_c, king_i = king_loc brd in 
  check_opp_attacks brd op_piece_ls king_c king_i 

(** [leaves_king_in_check brd c1 i1 c2 i2] is [true] if the attempted
    move from [c1, i1] to [c2, i2] leaves the king in check, and 
    [false] otherwise. 
    Requires: everything else about the move from [c1, i1] to [c2, i2]
    is valid and legal.  *)
let leaves_king_in_check brd c1 i1 c2 i2 = 
  let temp = Board.copy_board brd in 
  Board.move_piece temp c1 i1 c2 i2;
  king_in_check temp 

(** [check_rows c ints piece brd] is true if there is no row in 
    column [c] such that [piece] could move to that square and 
    the current player's king would not be left in check.  *)
let rec check_rows c ints piece brd = 
  match ints with 
  | [] -> true 
  | i::t ->  
    let (p,cp,ip) = piece in 
    if legal_for_piece cp ip c i brd 
    && not (is_blocked brd cp ip c i)
    && not (leaves_king_in_check brd cp ip c i)
    then false  
    else check_rows c t piece brd 

(** [check_cols chars ints piece brd] is true if there is no column
    in which there is a square that [piece] can move that would 
    not leave the king in check. False otherwise.   *)
let rec check_cols chars ints piece brd = 
  match chars with 
  | [] -> true 
  | c::t -> 
    check_rows c ints piece brd && check_cols t ints piece brd 

(** [this_piece_cant_prevent_check piece brd] is true if this piece
    cannot move in any way that does not leave the current player's
    king in check. False otherwise. *)
let this_piece_cant_prevent_check piece brd = 
  let chars = ['A';'B';'C';'D';'E';'F';'G';'H'] in 
  let ints =  [1;2;3;4;5;6;7;8] in 
  check_cols chars ints piece brd

(** [pieces_cant_prevent_check pieces brd] is true if the current
    player's pieces cannot move in any way that does not leave the king
    in check. False otherwise.  *)
let rec pieces_cant_prevent_check pieces brd = 
  match pieces with 
  | [] -> true 
  | p::t -> 
    this_piece_cant_prevent_check p brd && pieces_cant_prevent_check t brd

(** [cant_escape_check brd] is true if the current player's king
    cannot escape check. False otherwise. *)
let cant_escape_check brd = 
  let pieces = (
    match Board.get_current_player brd with 
    | White -> Board.get_white_pieces brd
    | Black -> Board.get_black_pieces brd ) in 
  pieces_cant_prevent_check pieces brd 

(** [checkmate brd] is true if the current state of the board leaves
    the non-current player's king in check. *)
let checkmate brd = 
  let temp = Board.copy_board brd in 
  let () = Board.next_player temp in 
  if king_in_check temp 
  then cant_escape_check temp  
  else false   

(** [is_legal brd c1 c2 c2 i2] is [true] if the current player 
    moving the piece at [c1, i1] to [c2, i2] is a legal move 
    given the current state of the game.  *)
let is_legal brd c1 i1 c2 i2 =  
  (* all legality tests go here! *)
  if (not (is_valid_location c1 i1)) || (not (is_valid_location c2 i2)) then 
    (false, "You're attempting to access an out of bounds location!")
  else if not (is_curr_players brd c1 i1) then 
    (false, "You don't have a piece in this square!")
  else if (is_curr_players brd c2 i2) then 
    (false, "This is friendly fire!")
  else if (is_blocked brd c1 i1 c2 i2) then (false, "This piece is blocked!")
  else if not (legal_for_piece c1 i1 c2 i2 brd) 
  then (false, "This piece can't move like that!") 
  else if 
    leaves_king_in_check brd c1 i1 c2 i2 
  then (false, "You can't leave your king in check!")
  else (true, "") 

type res = Legal | Illegal of string  | Terminate 

(** [capture brd col c2 i2] tells Board to 'capture' the piece at the 
    target destination of this move, if such a piece exists *)
let capture brd col c2 i2 = 
  match Board.get_piece_at brd c2 i2 with 
  | None -> () 
  | Some {p_type=p2} -> 
    Board.capture_piece brd col p2

let process brd cmmd = 
  match cmmd with 
  | Command.Move (c1,i1,c2,i2) -> begin 
      match is_legal brd c1 i1 c2 i2 with 
      | true, _ -> 
        capture brd (Board.get_current_player brd) c2 i2;
        Board.move_piece brd c1 i1 c2 i2; 
        if checkmate brd then Terminate 
        else Legal 
      | false, str -> 
        Illegal str end
  | _ -> failwith "impossible 1"
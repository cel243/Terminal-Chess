

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
  | None -> failwith "precondition violated" 
  | Some {p_type=Knight} -> false 
  | _ -> stepwise brd 
           (step (int_of_char st_c) (int_of_char dest_c)) (step st_i dest_i) 
           (int_of_char dest_c) dest_i

(** [legal_for_piece piece c1 i1 c2 i2] is [true] if game_piece [piece] can
    legally move from [c1,i1] to [c2,i2] given the rules of the type 
    of [piece] *)
let legal_for_piece piece c1 i1 c2 i2 = true

(** [check_opp_attacks brd op_ls king_c king_i] is [true] if any of 
    the opposing player's pieces have the ability to take the current
    player's king in this current board state, and [false] 
    otherwise  *)
let rec check_opp_attacks brd op_ls king_c king_i = 
  match op_ls with 
  | [] -> false 
  | (p,c,i)::t -> 
    if legal_for_piece p c i king_c king_i 
    &&  not (is_blocked brd c i king_c king_i) then true 
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
  | _ -> failwith "impossible" 

(** [leaves_king_in_check brd c1 i1 c2 i2] is [true] if the attempted
    move from [c1, i1] to [c2, i2] leaves the king in check, and 
    [false] otherwise. 
    Requires: everything else about the move from [c1, i1] to [c2, i2]
    is valid and legal.  *)
let leaves_king_in_check brd c1 i1 c2 i2 = 
  let temp = Board.copy_board brd in 
  Board.move_piece temp c1 i1 c2 i2;
  let op_piece_ls = (
    match Board.get_current_player brd with 
    | Black -> Board.get_black_pieces temp 
    | White -> Board.get_white_pieces temp) in 
  let king_c, king_i = king_loc temp in 
  check_opp_attacks temp op_piece_ls king_c king_i 


(** [is_legal brd c1 c2 c2 i2] is [true] if the current player 
    moving the piece at [c1, i1] to [c2, i2] is a legal move 
    given the current state of the game.  *)
let is_legal brd c1 i1 c2 i2 =  
  (* all legality tests go here! *)
  not (is_blocked brd c1 i1 c2 i2) 
(* && ...... *)
(* && not (leaves_king_in_check brd c1 i1 c2 i2) *)

type res = Legal | Illegal | Terminate 

let process brd cmmd = 
  match cmmd with 
  | Command.Move (c1,i1,c2,i2) -> 
    if is_legal brd c1 i1 c2 i2  
    then (Board.move_piece brd c1 i1 c2 i2; Legal )
    else Illegal 
  | _ -> failwith "impossible"
open ANSITerminal 


(* 
FUNCTIONS TO SUPPORT? 
   show player all pieces it can take (and how to take them) 
   show current player all pieces under attack 
   show player which pieces are attacking this piece 
   show player top 3 suggested moves 
   show player which pieces will be under attack IF they make a given move

*)

let check_valid brd c i = 
  if not (Logic.is_valid_location c i ) then 
    (print_string [red] "\nYou have not enetered a valid location.\n";
     false )
  else (
    match Board.get_piece_at brd c i with 
    | None -> print_string [red] "\nThere is no piece on this square.\n";
      false 
    | Some {col} -> 
      if Board.get_current_player brd <> col then (
        print_string [red] "\nThat is not your piece.\n";
        false )
      else true )

let rec all_opp_attacks brd op_ls c i sofar = 
  match op_ls with 
  | [] -> sofar 
  | (_,c',i')::t -> 
    let (b,_) = Logic.is_legal brd c' i' c i in 
    if b 
    then all_opp_attacks brd t c i ((c',i')::sofar) 
    else all_opp_attacks brd t c i sofar

let attackers brd c i = 
  if not (check_valid brd c i) then () 
  else 
    let op_ls = (
      match Board.get_current_player brd with 
      | White -> Board.get_black_pieces brd 
      | Black -> Board.get_white_pieces brd) in 
    let temp = Board.copy_board brd in 
    Board.next_player temp; 
    let ops = all_opp_attacks temp op_ls c i [] in 
    if List.length ops = 0 then print_string [red] "\nNo attackers!\n"
    else 
      Display.print_highlighted_brd brd ops

let rec exist_opp_attacks brd op_ls c i = 
  match op_ls with 
  | [] -> false 
  | (_,c',i')::t -> 
    let (b,_) = Logic.is_legal brd c' i' c i in 
    if b 
    then true 
    else exist_opp_attacks brd t c i

let rec check_each_piece brd sofar op_pieces = function 
  | [] -> sofar 
  | (_,c,i)::t -> 
    if (exist_opp_attacks brd op_pieces c i) 
    then  check_each_piece brd ((c,i)::sofar) op_pieces t
    else check_each_piece brd sofar op_pieces t

let under_attack brd = 
  let pieces, op_pieces = (
    match Board.get_current_player brd with 
    | White -> Board.get_white_pieces brd, Board.get_black_pieces brd 
    | Black -> Board.get_black_pieces brd, Board.get_white_pieces brd) in 
  let temp = Board.copy_board brd in 
  Board.next_player temp; 
  Display.print_highlighted_brd brd (check_each_piece temp [] op_pieces pieces) 

(** [check_rows c' ints brd c i] is a list of locations in column [c']
    that the piece at [c,i] can legally move to.
    Requires: there is a piece at [c,i] and it belongs to the current player.  *)
let rec check_rows c' ints brd c i = 
  match ints with 
  | [] -> [] 
  | i'::t ->  
    let (b, _) = Logic.is_legal brd c i c' i' in
    if b
    then (c',i')::(check_rows c' t brd c i)  
    else check_rows c' t brd c i

(** [chars ints brd c i sofar] is a list of locations on the 
    board to which it is legal for the piece at [c,i] to move. 
    Requires: there is a piece at [c,i] and it belongs to the current player. *)
let rec check_cols chars ints brd c i sofar = 
  match chars with 
  | [] -> sofar 
  | c'::t -> check_cols t ints brd c i ((check_rows c' ints brd c i)@sofar)

(** [get_legal_squares brd c i] is a list of locations on the 
    board to which it is legal for the piece at [c,i] to move, 
    including the location [(c,i)]. 
    Requires: there is a piece at [c,i] and it belongs to the current player. 
*)
let get_legal_squares brd c i  = 
  let chars = ['A';'B';'C';'D';'E';'F';'G';'H'] in 
  let ints =  [1;2;3;4;5;6;7;8] in 
  check_cols chars ints brd c i  [] 

(** [legal_moves brd c i] prints a visual depiction of the legal moves
    of the piece at [c,i], if such a piece exists and belongs to
    the current player.   *)
let legal_moves brd c i = 
  if not (check_valid brd c i) then () 
  else 
    Display.print_highlighted_brd brd ((c,i)::(get_legal_squares brd c i))  

let handle_player_support brd = function 
  | Command.CanCapture (c,i) -> failwith "unimplemented"
  | Command.LegalMoves (c,i) -> legal_moves brd c i
  | Command.UnderAttack  -> under_attack brd 
  | Command.Attackers (c,i) -> attackers brd c i 
  | Command.UnderAttackIF (c1,i1,c2,i2) -> failwith "unimplemented"
  | Command.Log -> Display.print_log brd

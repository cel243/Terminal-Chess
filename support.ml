open ANSITerminal 


(** [check_valid brd c i] is [false, brd] if [c,i] is not a valid 
    location on [brd] or does not contain a piece, and prints an appropriate 
    error message. Otherwise it is [true, new_brd], where [new_brd] is a
    temporary board where the current player is the opposite player than the
    one that owns the piece in that square. *)
let check_valid brd c i = 
  if not (Logic.is_valid_location c i ) then 
    (print_string [red] "\nYou have not enetered a valid location.\n";
     Display.print_board brd;
     (false,brd) )
  else (
    let temp = Board.copy_board brd in 
    match Board.get_piece_at brd c i with 
    | None -> 
      print_string [red] "\nThere is no piece on this square.\n";
      Display.print_board brd;
      (false,brd) 
    | Some {col} when Board.get_current_player brd = col -> 
      Board.next_player temp; 
      (true, temp)
    | Some {col} -> (true, temp) )

(** [all_opp_attacks brd op_ls c i sofar] is a list of opponent
    pieces that are capable of capturing the piece at [c,i].  *)
let rec all_opp_attacks brd op_ls c i sofar = 
  match op_ls with 
  | [] -> sofar 
  | (_,c',i')::t -> 
    let (b,_) = Logic.is_legal brd c' i' c i in 
    if b 
    then all_opp_attacks brd t c i ((c',i')::sofar) 
    else all_opp_attacks brd t c i sofar

(** [attackers brd c i] prints [brd], with squares highlighted
    corresponding to the opponent pieces that are capable of capturing
    the piece at [c,i]. If [c,i] does not represent a valid location
    or is not the location of one of the current player's pieces, an
    appropriate error message will print instead.   *)
let attackers brd c i = 
  match check_valid brd c i with 
  | (false, _) -> ()
  | (true, temp) -> 
    let op_ls = (
      match Board.get_current_player temp with 
      | White -> Board.get_white_pieces temp 
      | Black -> Board.get_black_pieces temp) in 
    let ops = all_opp_attacks temp op_ls c i [] in 
    if List.length ops = 0 
    then ( 
      print_string [red] ("\nNo attackers!\n");
      Display.print_board brd )
    else 
      Display.print_highlighted_brd brd ops 


(** [exist_opp_attacks brd op_ls c i] is true if an opponent
    piece has the capacity to capture the piece at [c,i].  *)
let rec exist_opp_attacks brd op_ls c i = 
  match op_ls with 
  | [] -> false 
  | (_,c',i')::t -> 
    let (b,_) = Logic.is_legal brd c' i' c i in 
    if b 
    then true 
    else exist_opp_attacks brd t c i

(** [check_each_piece brd sofar op_pieces] is a list of locations
    on [brd] corresponding to the pieces of the current player
    that are in immediate danger of being captured. *)
let rec check_each_piece brd sofar op_pieces = function 
  | [] -> sofar 
  | (_,c,i)::t -> 
    if (exist_opp_attacks brd op_pieces c i) 
    then  check_each_piece brd ((c,i)::sofar) op_pieces t
    else check_each_piece brd sofar op_pieces t

(** [under_attack brd] displays [brd] with the squares highlighted
    corresponding to the pieces of the current player that are in 
    immediate danger of being captured.  *)
let under_attack brd = 
  let pieces, op_pieces = (
    match Board.get_current_player brd with 
    | White -> Board.get_white_pieces brd, Board.get_black_pieces brd 
    | Black -> Board.get_black_pieces brd, Board.get_white_pieces brd) in 
  let temp = Board.copy_board brd in 
  Board.next_player temp; 
  Display.print_highlighted_brd brd (check_each_piece temp [] op_pieces pieces) 


(** [can_attack brd] displays [brd] with the squares highlighted
    corresponding to the pieces of the opposing player that
    the current player can capture given the current state of [brd].  *)
let can_attack brd = 
  let temp = Board.copy_board brd in
  Board.next_player temp; 
  under_attack temp


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
    board to which it is legal for the piece at [c,i] to move. 
    Requires: there is a piece at [c,i] and it belongs to the current player. 
*)
let get_legal_squares brd c i  = 
  let chars = ['A';'B';'C';'D';'E';'F';'G';'H'] in 
  let ints =  [1;2;3;4;5;6;7;8] in 
  check_cols chars ints brd c i  [] 

(** [legal_moves brd c i] prints a visual depiction of the legal moves
    of the piece at [c,i], if the location is on the board
    and there is a piece at that location. Otherwise, an
    appropriate error message is printed. *)
let legal_moves brd c i = 
  match check_valid brd c i with 
  | false, _ -> () 
  | true, temp -> 
    Board.next_player temp; 
    Display.print_highlighted_brd temp ((c,i)::(get_legal_squares temp c i))  

(** [hypothetical cmmd brd c1 i1 c2 i2] displays two boards: first,
    a hypothetical board, highlighting the squares that [cmmd] would
    normally highlight if the piece at [c1,i1] were moved to [c2,i2];
    and second, the current board. If the move from [c1,i1] to [c2,i2]
    is illegal, an appropriate error message is displayed. 
    Requires: [cmmd] is one of the [IF PSupport] types.  *)
let hypothetical cmmd brd c1 i1 c2 i2 = 
  match Logic.is_legal brd c1 i1 c2 i2 with 
  | false, s -> 
    print_string [red] ("\nCannot check this move: "^s^"\n");
    Display.print_board brd 
  | true, _ -> (
      let temp = Board.copy_board brd in 
      Board.move_piece temp c1 i1 c2 i2; 
      print_string [red] "\nHypothetical board: \n";
      (match cmmd with 
       | Command.UnderAttackIF _ -> under_attack temp
       | Command.CanAttackIF _ -> can_attack temp 
       | Command.AttackersIF (c,i,_,_,_,_) -> attackers temp c i  
       | Command.LegalMovesIF (c,i,_,_,_,_) -> legal_moves temp c i 
       | _ -> failwith "precondition violated" );
      print_string [red] "\n Current board: \n";
      Display.print_board brd 
    )


let handle_player_support brd = function 
  | Command.LegalMoves (c,i) -> legal_moves brd c i
  | Command.UnderAttack  -> under_attack brd 
  | Command.CanAttack -> can_attack brd 
  | Command.Attackers (c,i) -> attackers brd c i 
  | Command.CanAttackIF (c1,i1,c2,i2) 
  | Command.AttackersIF (_,_,c1,i1,c2,i2) 
  | Command.LegalMovesIF (_,_,c1,i1,c2,i2)
  | Command.UnderAttackIF (c1,i1,c2,i2) as cmmd -> 
    hypothetical cmmd brd c1 i1 c2 i2 

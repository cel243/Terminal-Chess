(** [depth_cpu] is how many moves ahead the cpu looks *)
let depth_cpu = 3

(** [get_pieces brd counter] makes a list of all the pieces on the board 
    belonging to the current player of the form (Piece,rank,file,score). *)
let rec get_pieces brd counter = 
  if 
    counter > -1 
  then
    let c = char_of_int((counter/8)+65) in
    let i = (counter mod 8)+1 in
    match Board.get_piece_at brd c i with
    | None -> get_pieces brd (counter-1)
    | Some {p_type = p; col = col; has_moved; points = pnt} -> 
      if 
        col = Board.get_current_player brd
      then
        (p, c, i, pnt) :: get_pieces brd (counter-1)
      else 
        get_pieces brd (counter-1)
  else
    []

(** [get_moves_piece brd counter c1 i1] is the list of all possible moves 
    of the piece at [c1, i1]. *)
let rec get_moves_piece brd counter c i =
  if 
    counter > -1 
  then
    let c1 = char_of_int((counter/8)+65) in
    let i1 = (counter mod 8)+1 in
    let legal, _ = Logic.is_legal brd c i c1 i1 in
    if legal 
    then (c, i, c1, i1) :: get_moves_piece brd (counter-1) c i
    else get_moves_piece brd (counter-1) c i
  else
    [] 

(** [get_moves brd lst] is the list of all possible moves for every piece in 
    [lst]. *)
let rec get_moves brd = function
  | [] -> []
  | (p, c, i, pnt) :: t -> 
    (get_moves_piece brd (63) c i) @ (get_moves brd t)

(** [create_board brd c1 i1 c2 i2] is the fake board where the hypothetical 
    move [c1,i1] to [c2,i2] was made, unless the move results
    in the capture of the king, in which case the king is 
    "captured" but is not removed from the board.  *)
let create_board brd c1 i1 c2 i2 =
  let fake_board = Board.copy_board brd in
  let captured_p = (Board.get_piece_at fake_board c2 i2) in
  match captured_p with
  | None -> Board.move_piece fake_board c1 i1 c2 i2;
    fake_board
  | Some piece -> 
    if piece.p_type <> King then (
      Board.capture_piece fake_board 
        (Board.get_current_player fake_board) piece.p_type;
      Board.move_piece fake_board c1 i1 c2 i2;
      fake_board )
    else (
      Board.capture_piece fake_board 
        (Board.get_current_player fake_board) piece.p_type;
      fake_board
    )

(** [get_boards brd lst] is the list of [((c1,i1,c2,i2),brd)]
    for each move in [lst], 
    where [brd] is the fake board associated with the hypothetical 
    move [c1,i1] to [c2,i2] *)
let rec get_boards brd = function
  | [] -> []
  | (c1,i1,c2,i2) :: t -> 
    ((c1,i1,c2,i2),create_board brd c1 i1 c2 i2)::(get_boards brd t)

(** [largest_score score move lst] 
    is the move in [lst] with the largest score *)
let rec largest_score score move = function
  | [] -> move
  | (sc,mv)::t ->
    if sc >= score
    then 
      largest_score sc mv t
    else 
      largest_score score move t

(** [highest_piece brd ls score loc p_type] is the piece  
    at a location in [ls] with the highest point value.
    Requires: every location in [ls] corresponds to 
    a square on the board with a piece.  *)
let rec highest_piece brd ls score loc p_type =
  match ls with 
  | [] -> loc, p_type 
  | (c, i)::t -> begin 
      match Board.get_piece_at brd c i with 
      | None -> failwith "violates precondition"
      | Some piece -> 
        if piece.points > score then 
          highest_piece brd t piece.points (c,i) piece.p_type
        else 
          highest_piece brd t score loc p_type
    end 

(** [get_opp_move brd c2 i2] is the next predicted move the cpu 
    thinks the opposing player will make, assuming that if the 
    opposing player can take a piece, it will, and if the 
    opposing player can take multiple pieces, it will 
    take the piece of the highest value. If the piece
    the opponent takes is the king, then the king will
    not be removed from the board. *)
let get_opp_move brd c2 i2 = 
  Board.next_player brd; 
  let locs,_,_,_ = Support.handle_player_support brd (Command.CanAttack) in 
  if locs = [] then (Board.next_player brd; brd )
  else 
    let (c,i), p = highest_piece brd locs 0 ('A',1) Pawn in 
    if p <> King then (
      let attackers,_,_,_ = 
        Support.handle_player_support brd (Command.Attackers (c,i)) in 
      let att_c,att_i = List.nth attackers 0 in 
      Board.capture_piece brd (Board.get_current_player brd) p; 
      Board.move_piece brd  att_c att_i c i; 
      Board.next_player brd;
      brd  )
    else (
      Board.capture_piece brd (Board.get_current_player brd) p; 
      Board.next_player brd;
      brd )

(** [next_move_helper brd dpth move1 tsc csc] 
    is a list of [(score, (c1,i1,c2,i2))] for each leaf
    of the move tree of depth [dpth], 
    where [score] is the score of the current 
    state of that board's leaf.
    It calls on [for_next_move] as a recursive dependency *)
let rec next_move_helper brd dpth move1 =
  if dpth > 0
  then
    let pieces = get_pieces brd 63 in
    let moves = get_moves brd pieces in
    let boards = get_boards brd moves in
    for_next_move dpth move1 boards
  else
    ((Board.get_score_cpu brd (Board.get_current_player brd)), move1)::[]

(** [for_next_move dpth move1 tsc csc] 
    is a list of [(score, (c1,i1,c2,i2))] for each leaf
    of the move tree, where [score] is the score of the current 
    state of that board's leaf. 
    It calls on [next_move_helper] as a recursive dependency *)
and for_next_move dpth move1 = function
  | [] -> []
  | ((c1,i1,c2,i2),brd)::t -> 
    if dpth = depth_cpu
    then
      (next_move_helper (get_opp_move brd c2 i2) (dpth-1) (c1,i1,c2,i2) )
      @(for_next_move dpth move1  t)
    else
      (next_move_helper (get_opp_move brd c2 i2) (dpth-1) move1)
      @(for_next_move dpth move1  t)

let next_move brd =
  let pieces = get_pieces brd 63 in
  let moves = get_moves brd pieces in
  let boards = get_boards brd moves in
  let movelist = for_next_move depth_cpu ('A', 1, 'A', 1) boards in
  (largest_score (-1000) ('A',1,'A',1) (movelist))




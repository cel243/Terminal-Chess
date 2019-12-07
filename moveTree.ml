

(** How far ahead the cpu looks *)
let depth_cpu = 3

(** [get_index_at int lst] is the element [e] at index [int] of [lst] *)
let rec get_index_at int = function
  | [] -> failwith "index error"
  | h :: t -> if int = 0 then h else get_index_at (int-1) t

(** [create_board brd c1 i1 c2 i2] is the [fake_board] where the hypothetical 
    move c2 i2 was made *)
let create_board brd c1 i1 c2 i2 =
  let fake_board = Board.copy_board brd in
  let captured_p = (Board.get_piece_at fake_board c2 i2) in
  match captured_p with
  | None -> Board.move_piece fake_board c1 i1 c2 i2;
    fake_board
  | Some piece -> 
    Board.capture_piece fake_board 
      (Board.get_current_player fake_board) piece.p_type;
    Board.move_piece fake_board c1 i1 c2 i2;
    fake_board

(** [get_boards brd lst] is the ((c1,i1,c2,i2),brd) where brd is the fake board
    associated with the hypothetical move c2 i1 *)
let rec get_boards brd = function
  | [] -> []
  | (c1,i1,c2,i2) :: t -> 
    ((c1,i1,c2,i2),create_board brd c1 i1 c2 i2)::(get_boards brd t)

(** [next_move_helper brd dpth move1 tsc csc] 
    is a list of (score, (c1,i1,c2,i2)) 
    It calls on [for_next_move] as a recursive dependency *)
let rec next_move_helper brd dpth move1 tsc csc =
  if dpth > 0
  then
    let pieces = Machine.get_pieces brd 63 in
    let moves = Machine.get_moves brd pieces in
    let boards = get_boards brd moves in
    for_next_move dpth move1 tsc csc boards
  else
    ((Board.get_score brd (Board.get_current_player brd))+csc, move1)::[]

(** [for_next_move dpth move1 tsc csc lst] is a list of (score, (c1,i1,c2,i2)) 
    It calls on [next_move_helper] as a recursive dependency *)
and for_next_move dpth move1 tsc csc = function
  | [] -> []
  | ((c1,i1,c2,i2),brd)::t -> 
    let score = 
      (((Board.get_score brd (Board.get_current_player brd))
        *dpth)-csc+tsc) in
    if dpth = depth_cpu
    then
      (next_move_helper brd (dpth-1) (c1,i1,c2,i2) score csc)
      @(for_next_move dpth move1 tsc csc t)
    else
      (next_move_helper brd (dpth-1) move1 score csc)
      @(for_next_move dpth move1 tsc csc t)

(** [largest_score score move lst] 
    is the [move] with the largest [score] from [lst] *)
let rec largest_score score move = function
  | [] -> move
  | (sc,mv)::t ->
    if sc > score
    then 
      largest_score sc mv t
    else 
      largest_score score move t


(** [next_move brd] is the next move (c1,i1,c2,i2) the cpu recommends *)
let next_move brd =
  let pieces = Machine.get_pieces brd 63 in
  let moves = Machine.get_moves brd pieces in
  let boards = get_boards brd moves in
  let movelist = for_next_move depth_cpu ('A', 1, 'A', 1) 0 
      (Board.get_score brd (Board.get_current_player brd)) boards in
  (largest_score (-1) ('A',1,'A',1) (movelist))




type cc1 = char
type cc2 = char
type ii1 = int
type ii2 = int
type score = float
type color = Board.color
type depth = int

let depth_cpu = 2

type t = 
  | Node of score*depth*cc1*ii1*cc2*ii2*(t list)
  | Leaf 

let init brd () =
  let col = Board.get_current_player brd in
  let sc = Board.get_score brd col in
  (col, sc, 0, 'A', 1, 'A', 1, [])

let rec get_index_at int = function
  | [] -> failwith "index error"
  | h :: t -> if int = 0 then h else get_index_at (int-1) t

let create_board brd c1 i1 c2 i2 =
  let fake_board = Board.copy_board brd in
  Board.move_piece fake_board c1 i1 c2 i2;
  fake_board

let rec get_boards brd = function
  | [] -> []
  | (c1,i1,c2,i2) :: t -> 
    ((c1,i1,c2,i2),create_board brd c1 i1 c2 i2)::(get_boards brd t)

let rec next_move_helper brd dpth move1 =
  if dpth > 0
  then
    let pieces = Machine.get_pieces brd 63 in
    let moves = Machine.get_moves brd pieces in
    let boards = get_boards brd moves in
    for_next_move dpth move1 boards
  else
    ((Board.get_score brd (Board.get_current_player brd)), move1)::[]

and for_next_move dpth move1 = function
  | [] -> []
  | ((c1,i1,c2,i2),brd)::t -> 
    if dpth = depth_cpu
    then
      (next_move_helper brd (dpth-1) (c1,i1,c2,i2))@(for_next_move dpth move1 t)
    else
      (next_move_helper brd (dpth-1) move1)@(for_next_move dpth move1 t)

let rec largest_score score move = function
  | [] -> move
  | (sc,mv)::t ->
    if sc > score
    then 
      largest_score sc mv t
    else 
      largest_score score move t

let next_move brd =
  let pieces = Machine.get_pieces brd 63 in
  let moves = Machine.get_moves brd pieces in
  let boards = get_boards brd moves in
  let movelist = for_next_move depth_cpu ('A', 1, 'A', 1) boards in
  (largest_score (-1) ('A',1,'A',1) (movelist))




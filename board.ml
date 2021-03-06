type piece = Pawn | Rook | Bishop | Knight | Queen | King
type color = Black | White
type game_piece = {p_type : piece; col : color; has_moved : bool; points : int }

(**
   AF: [{p_turn=color; white_captured=w_ls; black_captured=b_ls; 
   board=brd_arr; moves=move_ls}] is a game where [color] is the current
   player, [w_ls] is the pieces that the White player has captured mapped
   to the number of that type of piece it has captured, [b_ls] is the pieces 
   that the Black player has captured mapped to the number of that piece it 
   has captured, [brd_arr] is the game board, where [brd_arr.(0)] corresponds 
   to the A column of the chess board, and [brd_arr.(0).(0)] corresponds 
   to the square at A1 (EX: [brd_arr.(5).(2)] represents F3), and 
   [move_ls] is the list of moves performed during the game. 

   RI: [brd_arr] is an 8*8 arr, where the pieces are arranged in 
   such a way that they could have been moved there during a legitimate
   game of chess from the standard starting position. For example, 
   there will always be two kings on the board. 
   [w_ls], [b_ls], and [move_ls] contain only positive integers,
   and [move_ls] further contains only characters in A...H. 
*)
type t = { 
  mutable p_turn : color;
  mutable white_captured : (piece*int) list; 
  mutable black_captured : (piece*int) list; 
  board : ((game_piece option) array) array;
  mutable moves : (
    (piece * color * char * int) * 
    (char * int) *
    ((piece * char * int) option)
  ) list 
}

(** [get_piece_value piece] is how many points [piece] is worth *)
let get_piece_value = function
  | Pawn -> 1
  | Rook -> 5
  | Bishop -> 3
  | Knight -> 3
  | Queen -> 9
  | King -> 100

let get_opp_color = function
  | Black -> White
  | White -> Black

let init_state init_state_loader = init_state_loader "INIT_STATE.json" 

let set_game p_turn board log w_cap b_cap= 
  {
    p_turn=p_turn;
    white_captured= w_cap;
    black_captured= b_cap;
    board=board;
    moves = log
  }

let get_current_player state = state.p_turn

let board_to_array state = state.board

let log_to_list state = state.moves 

let white_cap_to_list state = state.white_captured

let black_cap_to_list state = state.black_captured 

let next_player state =
  if 
    state.p_turn = White 
  then
    state.p_turn <- Black
  else 
    state.p_turn <- White

let get_piece_at state c i =
  state.board.((int_of_char c)-65).(i-1)

let get_moves state = state.moves

let get_last_move t = match (get_moves t) with 
  | [] -> None
  | h :: _ -> Some h

(** [loop_array c f i r] i the [(game_piece * char * int)] list representation of 
    board pieces of color c in a given rank r from array f. *)
let rec loop_array color file i r =
  if i = 8 then []
  else 
    match file.(i) with
    | None -> (loop_array color file (i+1) r)
    | Some g_piece -> 
      if g_piece.col = color
      then (g_piece, char_of_int(r+65), i+1)::(loop_array color file (i+1) r)
      else (loop_array color file (i+1) r)

(** [loop_board c t i] is the [(game_piece * char * int)] list representation of 
    board pieces of color c, passes each file array to [loop_array c f i r] *)
let rec loop_board color state i =
  if i = 8 then []
  else (loop_array color state.board.(i) 0 i)@(loop_board color state (i+1))

let get_white_pieces state =
  loop_board White state 0

let get_black_pieces state =
  loop_board Black state 0

let copy_piece state c1 i1 c2 i2 =
  match state.board.((int_of_char c1)-65).(i1-1) with
  | None -> raise (Failure "piece not there")
  | Some {p_type=s; col=c; has_moved=h; points=p} -> 
    if ((i2 = 8 && c = White && s = Pawn) ||
        (i2 = 1 && c = Black && s = Pawn))
    then state.board.((int_of_char c2)-65).(i2-1) <- 
        Some {p_type=Queen; col=c; has_moved=true; points=p}
    else
      state.board.((int_of_char c2)-65).(i2-1) <- 
        Some {p_type=s; col=c; has_moved=true; points=p}

(** [log_move state c1 i1 c2 i2 c3 i3] prepends a move of either form:
    1) ((p1,c1,i1), (c2, i2), Some (p3, c3, i3)) if there was 
    a piece at the destination
    2) ((p1,c1,i1), (c2, i2), None)) if there was no piece at the destination,
    or raises a Failure if the starting space is empty. *)
let log_move state c1 i1 c2 i2 c3 i3 = 
  let moves' = 
    match state.board.((int_of_char c1)-65).(i1-1), 
          state.board.((int_of_char c3)-65).(i3-1) with
    (* This should never happen due to Logic's vetting *)
    | None, _ -> raise (Failure "source piece not there: log")
    (* Friendly piece to empty square *)
    | Some {p_type=p1; _}, None -> begin
        let curr = (get_current_player state) in
        let move = ((p1, curr, c1, i1), (c2, i2), None) in
        move::state.moves
      end
    (* Friendly capturing enemy piece *)
    | Some {p_type=p1; _}, Some {p_type=p2; _} -> begin
        let curr = (get_current_player state) in
        let move = ((p1, curr, c1, i1), (c2, i2), Some (p2, c3, i3)) in
        move::state.moves
      end in
  state.moves <- moves'

let move_piece_en_passant state c1 i1 c2 i2 c3 i3 =
  (log_move state c1 i1 c2 i2 c3 i3);
  if 
    (copy_piece state c1 i1 c2 i2) = () 
  then
    begin
      state.board.((int_of_char c1)-65).(i1-1) <- None;
      state.board.((int_of_char c3)-65).(i3-1) <- None
    end
  else 
    raise (Failure "piece not moved")

let move_piece state c1 i1 c2 i2 =
  (log_move state c1 i1 c2 i2 c2 i2);
  if 
    (copy_piece state c1 i1 c2 i2) = () 
  then
    state.board.((int_of_char c1)-65).(i1-1) <- None
  else 
    raise (Failure "piece not moved")

let copy_board state = 
  {p_turn = 
     (match state.p_turn with
      | Black -> Black
      | White -> White ); 
   white_captured = state.white_captured;
   black_captured = state.black_captured; 
   moves = state.moves;
   board = 
     let board_copy = Array.make 8 (Array.make 8 None) in
     (for x=0 to 7 do
        board_copy.(x) <- Array.copy state.board.(x)
      done);
     board_copy
  }

let capture_piece state col piece = 
  match col with 
  | White -> 
    if (List.mem_assoc piece state.white_captured) then 
      state.white_captured <- (
        (piece, (List.assoc piece state.white_captured) + 1)
        ::(List.remove_assoc piece state.white_captured )  )
    else 
      state.white_captured <- (
        (piece, 1)::state.white_captured  )
  | Black -> 
    if (List.mem_assoc piece state.black_captured) then 
      state.black_captured <- (
        (piece, (List.assoc piece state.black_captured) + 1)
        ::(List.remove_assoc piece state.black_captured )  )
    else 
      state.black_captured <- (
        (piece, 1)::state.black_captured  )

let get_captured_pieces state = function 
  | White -> state.white_captured
  | Black -> state.black_captured
(** [get_score_tr sum pieces] is the sum of all the values of the pieces
    in [pieces] multiplied by the number each is mapped to.  *)
let rec get_score_tr sum = function
  | [] -> sum
  | (p, q) :: a -> begin
      let addition = (get_piece_value p) * q in
      get_score_tr (sum + addition) a
    end

let get_score st col = 
  let pieces = (get_captured_pieces st col) in
  get_score_tr 0 pieces

let get_score_cpu st col = 
  let pieces, op_pieces = (
    match col with 
    | White -> get_captured_pieces st White, get_captured_pieces st Black 
    | Black -> get_captured_pieces st Black, get_captured_pieces st White )
  in (get_score_tr 0 pieces) - (get_score_tr 0 op_pieces)

let get_move_cnt st = List.length (get_moves st)


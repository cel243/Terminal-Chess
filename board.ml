type piece = Pawn | Rook | Bishop | Knight | Queen | King
type color = Black | White
type game_piece = {p_type : piece; col : color; has_moved : bool; points : int }

(**
 * AF: t represents a game with t.p_turn as the turn of the current player, 
 * t.board is the game board, where t.board.(0) is the 1st array corresponding 
 * the A file on a chess board (i.e. t.board.(7) is the H file)
 * t.board.(0).(0) represents the position A1 on the board (i.e. t.board.(5).(2) 
 * represents F3).
 * t.white_captured is the pieces the White player has captured mapped to the 
 * number of that particular piece white has captured 
 * t.black_captured is the pieces the Black player has captured mapped to the 
 * number of that particular piece white has captured
 * t.moves is the list of moves performed during the game
 * RI: t.board is a 8*8 array. 
 * t.white_captured, t.black_captured, and t.moves contain nonzero, positive 
 * integers only.
*)
type t = { 
  mutable p_turn : color;
  mutable white_captured : (piece*int) list; 
  mutable black_captured : (piece*int) list; 
  board : ((game_piece option) array) array;
  mutable moves : ((piece * color * char * int) * 
                   (char * int) *
                   ((piece * char * int) option)) list 
}

let get_opp_color = function
  | Black -> White
  | White -> Black

let init_state () = 
  { p_turn = White;
    white_captured = [];
    black_captured = [];
    moves = [];
    board = [|
      [|
        Some {p_type = Rook; col = White; has_moved = false; points=5};
        Some {p_type = Pawn; col = White; has_moved = false; points=1};
        None;
        None;
        None;
        None;
        Some {p_type = Pawn; col = Black; has_moved = false; points=1};
        Some {p_type = Rook; col = Black; has_moved = false; points=5}
      |];
      [|
        Some {p_type = Knight; col = White; has_moved = false; points=3};
        Some {p_type = Pawn; col = White; has_moved = false; points=1};
        None;
        None;
        None;
        None;
        Some {p_type = Pawn; col = Black; has_moved = false; points=1};
        Some {p_type = Knight; col = Black; has_moved = false; points=3}
      |];
      [|
        Some {p_type = Bishop; col = White; has_moved = false; points=3};
        Some {p_type = Pawn; col = White; has_moved = false; points=1};
        None;
        None;
        None;
        None;
        Some {p_type = Pawn; col = Black; has_moved = false; points=1};
        Some {p_type = Bishop; col = Black; has_moved = false; points=3}
      |];
      [|
        Some {p_type = Queen; col = White; has_moved = false; points=9};
        Some {p_type = Pawn; col = White; has_moved = false; points=1};
        None;
        None;
        None;
        None;
        Some {p_type = Pawn; col = Black; has_moved = false; points=1};
        Some {p_type = Queen; col = Black; has_moved = false; points=9}
      |];
      [|
        Some {p_type = King; col = White; has_moved = false; points=100};
        Some {p_type = Pawn; col = White; has_moved = false; points=1};
        None;
        None;
        None;
        None;
        Some {p_type = Pawn; col = Black; has_moved = false; points=1};
        Some {p_type = King; col = Black; has_moved = false; points=100}
      |];
      [|
        Some {p_type = Bishop; col = White; has_moved = false; points=3};
        Some {p_type = Pawn; col = White; has_moved = false; points=1};
        None;
        None;
        None;
        None;
        Some {p_type = Pawn; col = Black; has_moved = false; points=1};
        Some {p_type = Bishop; col = Black; has_moved = false; points=3}
      |];
      [|
        Some {p_type = Knight; col = White; has_moved = false; points=3};
        Some {p_type = Pawn; col = White; has_moved = false; points=1};
        None;
        None;
        None;
        None;
        Some {p_type = Pawn; col = Black; has_moved = false; points=1};
        Some {p_type = Knight; col = Black; has_moved = false; points=3}
      |];
      [|
        Some {p_type = Rook; col = White; has_moved = false; points=5};
        Some {p_type = Pawn; col = White; has_moved = false; points=1};
        None;
        None;
        None;
        None;
        Some {p_type = Pawn; col = Black; has_moved = false; points=1};
        Some {p_type = Rook; col = Black; has_moved = false; points=5}
      |];
    |];
  }

let get_current_player state =
  state.p_turn

let next_player state =
  if 
    state.p_turn = White 
  then
    state.p_turn <- Black
  else 
    state.p_turn <- White

let get_piece_at state c i =
  state.board.((int_of_char c)-65).(i-1)

let get_moves state = 
  state.moves

(* [loop_array c f i r] i the (game_piece * char * int) list representation of 
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

(* [loop_board c t i] is the (game_piece * char * int) list representation of 
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
    if (((i2 = 8) && (c = White) && (s = Pawn)) ||
        ((i2 = 1) && (c = Black) && (s = Pawn)))
    then state.board.((int_of_char c2)-65).(i2-1) <- 
        Some {p_type=Queen; col=c; has_moved=true; points=p}
    else
      state.board.((int_of_char c2)-65).(i2-1) <- 
        Some {p_type=s; col=c; has_moved=true; points=p}

(** [log_move state c1 i1 c2 i2 c3 i3] prepends a move of either form:
    1) ((p1,c1,i1), (c2, i2), Some (p3, c3, i3)) if there was a piece at the destination
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

(*
p3 --> c3 i3
(log_move state c1 i1 c2 i2 c3 i3)
*)

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



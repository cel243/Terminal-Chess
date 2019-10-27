type piece = Pawn | Rook | Bishop | Knight | Queen | King
type color = Black | White
type game_piece = {p_type : piece; col : color; has_moved : bool }
type t = { mutable p_turn : color;
           board : ((game_piece option) array) array }
(**
 * AF: t represents a game with t.p_turn as the turn of the current player
 * t.board is the game board
 * RI: t.board is a 8*8 array where t.board.(0) is the 1st array corresponding 
 * the A file on a chess board (i.e. t.board.(7) is the H file)
 * t.board.(0).(0) represents the position A1 on the board (i.e. t.board.(5).(2) 
 * represents F3) 
*)

let init_state = 
  { p_turn = White;
    board = [|
      [|
        Some {p_type = Rook; col = White; has_moved = false};
        Some {p_type = Pawn; col = White; has_moved = false};
        None;
        None;
        None;
        None;
        Some {p_type = Pawn; col = Black; has_moved = false};
        Some {p_type = Rook; col = Black; has_moved = false}
      |];
      [|
        Some {p_type = Knight; col = White; has_moved = false};
        Some {p_type = Pawn; col = White; has_moved = false};
        None;
        None;
        None;
        None;
        Some {p_type = Pawn; col = Black; has_moved = false};
        Some {p_type = Knight; col = Black; has_moved = false}
      |];
      [|
        Some {p_type = Bishop; col = White; has_moved = false};
        Some {p_type = Pawn; col = White; has_moved = false};
        None;
        None;
        None;
        None;
        Some {p_type = Pawn; col = Black; has_moved = false};
        Some {p_type = Bishop; col = Black; has_moved = false}
      |];
      [|
        Some {p_type = Queen; col = White; has_moved = false};
        Some {p_type = Pawn; col = White; has_moved = false};
        None;
        None;
        None;
        None;
        Some {p_type = Pawn; col = Black; has_moved = false};
        Some {p_type = Queen; col = Black; has_moved = false}
      |];
      [|
        Some {p_type = King; col = White; has_moved = false};
        Some {p_type = Pawn; col = White; has_moved = false};
        None;
        None;
        None;
        None;
        Some {p_type = Pawn; col = Black; has_moved = false};
        Some {p_type = King; col = Black; has_moved = false}
      |];
      [|
        Some {p_type = Bishop; col = White; has_moved = false};
        Some {p_type = Pawn; col = White; has_moved = false};
        None;
        None;
        None;
        None;
        Some {p_type = Pawn; col = Black; has_moved = false};
        Some {p_type = Bishop; col = Black; has_moved = false}
      |];
      [|
        Some {p_type = Knight; col = White; has_moved = false};
        Some {p_type = Pawn; col = White; has_moved = false};
        None;
        None;
        None;
        None;
        Some {p_type = Pawn; col = Black; has_moved = false};
        Some {p_type = Knight; col = Black; has_moved = false}
      |];
      [|
        Some {p_type = Rook; col = White; has_moved = false};
        Some {p_type = Pawn; col = White; has_moved = false};
        None;
        None;
        None;
        None;
        Some {p_type = Pawn; col = Black; has_moved = false};
        Some {p_type = Rook; col = Black; has_moved = false}
      |];
    |] }

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

let rec loop_array color file i r =
  if i = 8 then []
  else 
    match file.(i) with
    | None -> (loop_array color file (i+1) r)
    | Some g_piece -> 
      if g_piece.col = color
      then (g_piece, char_of_int(r+65), i+1)::(loop_array color file (i+1) r)
      else (loop_array color file (i+1) r)

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
  | Some {p_type=s; col=c; has_moved=h} -> 
    state.board.((int_of_char c2)-65).(i2-1) <- 
      Some {p_type=s; col=c; has_moved=true}

let move_piece state c1 i1 c2 i2 =
  if 
    (copy_piece state c1 i1 c2 i2) = () 
  then
    state.board.((int_of_char c1)-65).(i1-1) <- None
  else 
    raise (Failure "piece not moved")


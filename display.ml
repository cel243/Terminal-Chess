open ANSITerminal

(** [get_rep col p] is the Unicode glyph for a piece [p] in the color [col]. *)
let get_rep col p = match col, p with
  | Board.White, Board.Pawn -> "\u{2659}"
  | Board.White, Board.Rook -> "\u{2656}"
  | Board.White, Board.Bishop -> "\u{2657}"
  | Board.White, Board.Knight -> "\u{2658}"
  | Board.White, Board.Queen -> "\u{2655}"
  | Board.White, Board.King -> "\u{2654}"
  | Board.Black, Board.Pawn -> "\u{265F}"
  | Board.Black, Board.Rook -> "\u{265C}"
  | Board.Black, Board.Bishop -> "\u{265D}"
  | Board.Black, Board.Knight -> "\u{265E}"
  | Board.Black, Board.Queen -> "\u{265B}"
  | Board.Black, Board.King -> "\u{265A}"

let get_rep_long = function
  | Board.Pawn -> "Pawn"
  | Board.Rook -> "Rook"
  | Board.Bishop -> "Bishop"
  | Board.Knight -> "Knight"
  | Board.Queen -> "Queen"
  | Board.King -> "King"

let get_input () = 
  match (read_line ()) with
  | exception End_of_file -> begin
      ANSITerminal.print_string [ANSITerminal.green] "Goodbye.\n"; exit 0
    end
  | str -> str

(** [get_background r f] is [ASNITerminal.on_blue] if either the file [f]
    or rank [r] is even, but not both; otherwise, is [ANSITerminal.on_cyan]. *)
let get_background r f = 
  if (f mod 2 = 0 && not (r mod 2 = 0)) || (not (f mod 2 = 0) && (r mod 2 = 0)) 
  then
    ANSITerminal.on_blue
  else
    ANSITerminal.on_cyan

(** [get_highlighted_background r f] is [ASNITerminal.on_yellow] if either the
    file [f] or rank [r] is even, but not both; otherwise, is 
    [ANSITerminal.on_red]. *)
let get_highlighted_background r f = 
  if (f mod 2 = 0 && not (r mod 2 = 0)) || (not (f mod 2 = 0) && (r mod 2 = 0)) 
  then
    ANSITerminal.on_red
  else
    ANSITerminal.on_yellow

(** [get_foreground c] is [ANSITerminal.black] if [c] is [Black], and
    is [ANSITerminal.white] if [c] is [White]. This function serves
    as a translator between the UI colors and internal logic designators. *)
let get_foreground (c : Board.color) = 
  match c with
  | Black -> ANSITerminal.black
  | White -> ANSITerminal.white

(** [print_rank r b] prints the rank (row) [r] given the pieces in board [b].
    The printed row is preceded by its number on a black background and is
    fololowed by a new line. Pieces are displayed via [get_rep] and empty
    squares are shown as spaces. *)
let print_rank r b =
  ANSITerminal.print_string [white; on_black] (" "^(string_of_int r)^" ");
  for f = 1 to 8 do
    let bg = get_background r f in
    match (Board.get_piece_at b (char_of_int (64 + f)) r) with
    | None -> ANSITerminal.print_string [bg] "   "
    | Some p -> begin
        let s = get_rep p.col p.p_type in
        let chck_cnd = (Logic.king_in_check b) && 
                       (p.col = Board.get_current_player b) in
        let s' = (" "^s^" ") in
        match p.p_type with 
        | Board.King when chck_cnd -> begin
            ANSITerminal.print_string [(get_foreground p.col); on_red] s'
          end
        | _ -> begin
            ANSITerminal.print_string [(get_foreground p.col); bg] s'
          end
      end
  done;
  ANSITerminal.print_string [default] "\n"; ()

let print_board b =
  ANSITerminal.print_string [default] "\n";
  for r = 8 downto 1 do
    print_rank r b
  done;
  ANSITerminal.print_string [white; on_black] "    A  B  C  D  E  F  G  H ";
  ANSITerminal.print_string [default] "\n"

let get_color_str = function
  | Board.Black -> "Black"
  | Board.White -> "White"

let get_opp_color_str = function
  | Board.White -> "Black"
  | Board.Black -> "White"

(** [construct_start_str (p,col,c,i) k] is "(k) [col'] p' at (c,i)", where
    col' is the string representation of the color [col] and p' is the long
    string representation of the piece type [p]. 

    Requires:
    [p] is a value of Board.piece
    [col] is a value of Board.color 
    [c] is a character
    [i] and [k] are integers 
*)
let construct_start_str (p,col,c,i) k = 
  let cstr = get_color_str col in
  let rep = get_rep_long p in
  let is = string_of_int i in
  let cs = Char.escaped c in
  let ks = string_of_int k in 
  "("^ks^") ["^cstr^"] "^rep^" at ("^cs^","^is^")"

(** [construct_move_str i m] creates a string representative of the move [m].
    The move's string always starts with: (i) [<player color>] at (<origin>)
    The latter part depends on the move type and is determined as follows:
    =======================|====================================================
            Move           |                 Predicate
    =======================|====================================================
     Piece to empty square | TO (<destination>)
    =======================|====================================================
     Piece captures Piece  | CAPTURES [<opponent color>] at (<destination>)
    =======================|====================================================
     En Passant capture    | CAPTURES [<opponent color>] at (<destination>) 
                           | BY EN PASSANT
    =======================|====================================================
*)
let construct_move_str i = function
  | (s, (c2, i2), None) ->
    let start = construct_start_str s i in
    let i2s = string_of_int i2 in
    let c2s = Char.escaped c2 in 
    start^" TO ("^c2s^","^i2s^")"
  | (((_,col,_,_) as s, (c2, i2), Some (p2, c3, i3))) ->
    let start = construct_start_str s i in
    let cstr2 = get_opp_color_str col in
    let rep2 = get_rep_long p2 in
    let i2s = string_of_int i2 in
    let i3s = string_of_int i3 in
    let c2s = Char.escaped c2 in
    let c3s = Char.escaped c3 in
    if i2 = i3 then (start^" CAPTURES "^
                     "["^cstr2^"] "^rep2^" at ("^c2s^","^i2s^")")
    else (start^" TO ("
          ^c2s^","^i2s^")"^" AND CAPTURES "^
          "["^cstr2^"] "^rep2^" at ("^c3s^","^i3s^") BY EN PASSANT")

let print_move i m b = 
  let s = construct_move_str i m in
  let s' = if b then "Last Move: "^s else s in
  ANSITerminal.print_string [default] (s'^"\n")

let print_log b = 
  ANSITerminal.print_string [default] "Move Log:\n";
  let rec print_all_moves i = function
    | [] -> ANSITerminal.print_string [default] "\n"
    | h :: t -> 
      print_move i h false;
      print_all_moves (i+1) t
  in 
  print_all_moves 0 (List.rev (Board.get_moves b));
  print_board b

(** [print_rank_highlighted r b locs col] 
    prints the rank (row) [r] given the pieces in board [b].
    The printed row is preceded by its number on a black background and is
    fololowed by a new line. Pieces are displayed via [get_rep] and empty
    squares are shown as spaces. The backgrounds of locations in 
    [locs] are highlighted with [col]. *)
let print_rank_highlighted r b locs =
  ANSITerminal.print_string [white; on_black] (" "^(string_of_int r)^" ");
  for f = 1 to 8 do
    let ch = char_of_int (f+64) in 
    let bg = (if List.mem (ch, r) locs 
              then get_highlighted_background r f 
              else get_background r f ) in
    match (Board.get_piece_at b (char_of_int (64 + f)) r) with
    | None -> ANSITerminal.print_string [bg] "   "
    | Some p -> begin
        let s = get_rep p.col p.p_type in
        ANSITerminal.print_string [(get_foreground p.col); bg] (" "^s^" ")
      end
  done;
  ANSITerminal.print_string [default] "\n";
  ()

(** [print_highlighted_brd b locs] prints the same representation of
    the chess board [b] that [print_board] does, but with all of the 
    locations indicated in [loc] highlighted. *)
let print_highlighted_brd b locs = 
  ANSITerminal.print_string [default] "\n";
  for r = 8 downto 1 do
    print_rank_highlighted r b locs
  done;
  ANSITerminal.print_string [white; on_black] "    A  B  C  D  E  F  G  H ";
  ANSITerminal.print_string [default] "\n"

let p_support_display (locs, b, hyp, b') = 
  if hyp then (
    ANSITerminal.print_string [red] "\nHypothetical board: \n";
    print_highlighted_brd b locs;
    print_string [red] "\n Current board: \n";
    print_board b' )
  else (
    print_highlighted_brd b locs 
  )

let help_menu () = 
  ANSITerminal.print_string [green] (
    "\n-----------HELP MENU----------\n"
    ^"CN to C'N' --> if C and C' are characters in A..H and N and N' are"
    ^" integers in 1..8, \nthen move the piece at CN to C'N', if legal.\n"
    ^"Resign --> forfeit the game\n"
    ^"Draw --> declare a draw between the players \n"
    ^"Help --> display the help menu \n"
    ^"Captured --> display the current player's captured pieces \n"
    ^"CN --> highlights legal moves of the piece at CN.\n"
    ^"Attackers CN --> highlights pieces that can capture piece at CN.\n"
    ^ "Under attack --> highlights pieces in danger of being captured.\n"
    ^ "Can attack --> highlights pieces the current player can capture.\n"
    ^"Log --> prints all of the moves performed this game.\n"
    ^"Save as <file_name> --> saves the current game at <file_name>.json\n"
    ^" in the current directory, to be loaded later.\n"
    ^"Suggest --> query the AI for a good next move.\n"
    ^"_____ IF CN to C'N' --> the same command, but executed in "
    ^"a world where the move CN to C'N' has occured.\n")

(** [print_piece_list lst] prints each piece's long representation string
    separated by a colon and followed by the quantity (integer) associated with 
    that entry.

    For example, the list [(Pawn, 2); (Bishop, 1)] is printed as:
    Pawn: 2
    Bishop: 1

    Requires: [lst] is a list of pairs of Board.piece and int. 
*)
let rec print_piece_list = function 
  | [] -> () 
  | (p, n)::t -> 
    ANSITerminal.print_string [red] 
      ((get_rep_long p)^": "^(string_of_int n)^"\n"); 
    print_piece_list t

(** [print_captured_piece brd col] prints all the types and quantity of each
    of pieces taken by the player with color [col] given the game represented
    by [brd]. 
    Requires: [brd] is a list of Board.piece * int entries
              [col] is a value of Board.color
*)
let print_captured_pieces brd = function 
  | Board.White -> 
    ANSITerminal.print_string 
      [red] "\nWHITE'S CAPTURED PIECES: \n"; 
    print_piece_list (Board.get_captured_pieces brd White)  
  | Board.Black -> 
    ANSITerminal.print_string 
      [red] "\nBLACK'S CAPTURED PIECES: \n"; 
    print_piece_list (Board.get_captured_pieces brd Black) 

(** [capture_message brd c1 i1 c2 i2] prints either:
    "Pawn takes Pawn by En Passant!" if the piece at [c1, i1] on [brd] is
    capable of taking a Pawn at [c2, i2] on the same [brd]; or,
    "<attacker piece type> takes <victim piece type>!" if there are pieces
    at [c1, i1] and [c2, i2] on the [brd].
*)
let capture_message brd c1 i1 c2 i2 = 
  match Board.get_piece_at brd c1 i1, Board.get_piece_at brd c2 i2 with 
  | _, None -> 
    if Logic.en_passant i1 c2 brd then  
      ANSITerminal.print_string 
        [red] 
        ("\nPawn takes Pawn by En Passant!\n")
    else ()
  | Some {p_type=p1;col}, Some {p_type=p2} -> 
    ANSITerminal.print_string 
      [red] 
      ("\n"^(get_rep_long p1)^" takes "^(get_rep_long p2)^"!\n")
  | _ , _ -> failwith "precond violated"
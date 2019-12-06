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

(** [get_rep_long p] is the full string representation of the piece type [p]. *)
let get_rep_long = function
  | Board.Pawn -> "Pawn"
  | Board.Rook -> "Rook"
  | Board.Bishop -> "Bishop"
  | Board.Knight -> "Knight"
  | Board.Queen -> "Queen"
  | Board.King -> "King"

(** [get_input ()] is the next line inputted by the user to stdin, or 
    exits the application if it encounters an EOF (i.e., CTRL-D). *)
let get_input () = 
  match (read_line ()) with
  | exception End_of_file -> begin
      ANSITerminal.print_string [ANSITerminal.green] "Goodbye.\n"; exit 0
    end
  | str -> str

(** [get_background r f] is [ASNITerminal.on_blue] if either the file [f]
    or rank [r] is even, but not both; otherwise, is [ANSITerminal.on_cyan]. *)
let get_background r f = 
  if (f mod 2 = 0 && not (r mod 2 = 0)) || (not (f mod 2 = 0) && (r mod 2 = 0)) then
    ANSITerminal.on_blue
  else
    ANSITerminal.on_cyan

(** [get_highlighted_background r f] is [ASNITerminal.on_yellow] if either the file [f]
    or rank [r] is even, but not both; otherwise, is [ANSITerminal.on_red]. *)
let get_highlighted_background r f = 
  if (f mod 2 = 0 && not (r mod 2 = 0)) || (not (f mod 2 = 0) && (r mod 2 = 0)) then
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
  ANSITerminal.print_string [default] "\n";
  ()

(** [print_board b] displays to the console all of the pieces on board [b]
    with a checkered background. Rows are preceded by corresponding numbers
    and columns have the representative letter (one of A though H) displayed
    beneath them. *)
let print_board b =
  ANSITerminal.print_string [default] "\n";
  for r = 8 downto 1 do
    print_rank r b
  done;
  ANSITerminal.print_string [white; on_black] "    A  B  C  D  E  F  G  H ";
  ANSITerminal.print_string [default] "\n"

(** [get_color_str c] is "Black" if [c] is [Board.Black], and is "White"
    if [c] is [Board.White] *)
let get_color_str = function
  | Board.Black -> "Black"
  | Board.White -> "White"

(** [get_opp_color_str c] is "Black" if [c] is [Board.White], and is "White"
    if [c] is [Board.Black] *)
let get_opp_color_str = function
  | Board.White -> "Black"
  | Board.Black -> "White"

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
  | ((p1,col,c1,i1), (c2, i2), None) -> begin
      let cstr = get_color_str col in
      let rep = get_rep_long p1 in
      let i1s = string_of_int i1 in
      let i2s = string_of_int i2 in
      let c1s = Char.escaped c1 in
      let c2s = Char.escaped c2 in
      let is = string_of_int i in 
      "("^is^") ["^cstr^"] "^rep^" at ("^c1s^","^i1s
      ^") TO ("^c2s^","^i2s^")"
    end
  | ((p1,col,c1,i1), (c2, i2), Some (p2, c3, i3)) -> begin
      let cstr = get_color_str col in
      let cstr2 = get_opp_color_str col in
      let rep1 = get_rep_long p1 in
      let rep2 = get_rep_long p2 in
      let i1s = string_of_int i1 in
      let i2s = string_of_int i2 in
      let i3s = string_of_int i3 in
      let c1s = Char.escaped c1 in
      let c2s = Char.escaped c2 in
      let c3s = Char.escaped c3 in
      let is = string_of_int i in
      if i2 = i3
      then ("("^is^") ["^cstr^"] "^rep1^" at ("^c1s^","^i1s^") CAPTURES "^
            "["^cstr2^"] "^rep2^" at ("^c2s^","^i2s^")")
      else ("("^is^") ["^cstr^"] "^rep1^" at ("^c1s^","^i1s^") TO ("
            ^c2s^","^i2s^")"^" AND CAPTURES "^
            "["^cstr2^"] "^rep2^" at ("^c3s^","^i3s^") BY EN PASSANT")
    end

let print_move i m = 
  let s = construct_move_str i m in
  ANSITerminal.print_string [default] ("Last Move: "^s^"\n")

let print_log b = 
  ANSITerminal.print_string [default] "Move Log:\n";
  let rec print_all_moves i = function
    | [] -> ANSITerminal.print_string [default] "\n"
    | h :: t -> 
      print_move i h;
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
    ^"CN if C'N' to C''N'' --> highlights legal moves of the piece at CN "
    ^"IF player moves CN to C'N'.\n"
    ^"Attackers CN --> highlights pieces that can capture piece at CN.\n"
    ^ "Attackers CN if C'N' to C''N'' --> highlights pieces that can capture "
    ^"piece at CN IF player moves C'N' to C''N''.\n"
    ^ "Under attack --> highlights pieces in danger of being captured.\n"
    ^ "Under attack if CN to C'N' --> highlights pieces in danger of being "
    ^"captured IF player moves CN to C'N'.\n"
    ^ "Can attack --> highlights pieces the current player can capture.\n"
    ^"Can attack if CN to C'N' --> highlights pieces current player can "
    ^"capture IF player moves CN to C'N'.\n"
    ^"LOG --> prints all of the moves performed this game.\n"
    ^"Save as <file_name> --> saves the current game at <file_name>.json\n"
    ^" in the current directory, to be loaded later.\n")

let rec print_piece_list = function 
  | [] -> () 
  | (p, n)::t -> 
    ANSITerminal.print_string [red] 
      ((get_rep_long p)^": "^(string_of_int n)^"\n"); 
    print_piece_list t

let print_captured_pieces brd = function 
  | Board.White -> 
    ANSITerminal.print_string 
      [red] "\nWHITE'S CAPTURED PIECES: \n"; 
    print_piece_list (Board.get_captured_pieces brd White)  
  | Board.Black -> 
    ANSITerminal.print_string 
      [red] "\nBLACK'S CAPTURED PIECES: \n"; 
    print_piece_list (Board.get_captured_pieces brd Black) 

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

type outcome = 
  | Win of Board.color
  | Draw

type opponent =
  | Human
  | CPU

(** [handle_resign color b] prints "[color'] has resigned [color''] wins!",
    redisplays the game board, and returns [Some Win [color'']].
    where [color'] is the string representation of [color] and [color''] is
    the opposite color of [color]. *)
let handle_resign color b =
  let c = Display.get_color_str color in
  let copp = Display.get_opp_color_str color in
  let str = c ^ " has resigned. " ^ copp ^ " wins!\n" in
  ANSITerminal.print_string [ANSITerminal.green] str;
  Display.print_board b;
  Some (Win (Board.get_opp_color color))

(** [print_draw_prompt b] prints the prompt that requests the opposing color
    to type "Agree" in order to accept; the prompt is indicated by a '>'. *)
let print_draw_prompt b = 
  let curr = Board.get_current_player b in
  let c = Display.get_color_str curr in
  let c' = Display.get_opp_color_str curr in
  let s = c^" has requested a draw. If "^c'^" agrees, type \"Agree\".\n" in
  ANSITerminal.print_string [ANSITerminal.red] s;
  Display.print_board b;
  print_string (c' ^ "'s input:\n>")

(** [handle_cpu_draw b] is [true] if the CPU's score is less than or equal
    to the player's score, and is [false] otherwise. *)
let handle_cpu_draw b =
  let player_color = Board.get_current_player b in
  let cpu_color = Board.get_opp_color player_color in
  let player_score = Board.get_score b player_color in
  let cpu_score = Board.get_score b cpu_color in
  cpu_score <= player_score

(** [handle_human_draw b] is [true] if, after prompting the user to type "agree"
    the user does so and accepts the draw proposal; [false] otherwise. *)
let handle_human_draw b = 
  print_draw_prompt b;
  ((Display.get_input ()) |> String.lowercase_ascii) = "agree"

(** [handle_draw b] handles the result of one player requesting a draw. 
    If the other player agrees to the draw, the program terminates, otherwise
    the game continues with the current player. *)
let handle_draw b oppt = 
  if (match oppt with
      | Human -> handle_human_draw b
      | CPU -> handle_cpu_draw b) then
    begin
      ANSITerminal.erase ANSITerminal.Screen;
      ANSITerminal.print_string [ANSITerminal.green]  "It's a draw!\n"; 
      Display.print_board b; 
      Some Draw
    end
  else
    begin
      ANSITerminal.erase ANSITerminal.Screen;
      ANSITerminal.print_string [ANSITerminal.red] 
        ("\nThe other player did not agree to the draw."
         ^ " Please continue the game or resign.\n");
      Display.print_board b; 
      None
    end

(** [handle_checkmate b] prints the game-winning string for the appropriate
    player color, displays the game-winning board, and returns [Some Win col]
    where [col] is the color that won.*)
let handle_checkmate b =
  ANSITerminal.erase ANSITerminal.Screen;
  let col = Board.get_current_player b in
  let c = Display.get_color_str col in
  ANSITerminal.print_string [ANSITerminal.green] ("CHECKMATE! "^c^" wins!\n");
  Display.print_board b;
  Some (Win col)

(** [handle_result b b_prev c1 i1 c2 i2] handles the result of 
    the legality of a player's move command. 
    Requires: 
    [b] is the Board post-move
    [b_prev] is is the Board pre-move
    [c1, i1] and [c2, i2] are valid locations on the Board
*)
let handle_result b b_prev c1 i1 c2 i2 = function
  | Logic.Legal ->
    Display.capture_message b_prev c1 i1 c2 i2; 
    Board.next_player b;
    None
  | Logic.Illegal str ->
    print_string ("That move is illegal. "^str^" Please retry.\n");
    None
  | Logic.Checkmate -> handle_checkmate b
  | Logic.Stalemate -> begin
      ANSITerminal.erase ANSITerminal.Screen;
      ANSITerminal.print_string [ANSITerminal.red] "STALEMATE!\n";
      Display.print_board b;
      Some Draw
    end
  | Logic.Draw -> begin
      ANSITerminal.erase ANSITerminal.Screen;
      ANSITerminal.print_string [ANSITerminal.green] "It's an 80-move draw!\n";
      Display.print_board b;
      Some Draw
    end

(** [handle_save s b] is [None] but firsts saves the board [b] to a file
    with filename [s] and notifies the user of this. *)
let handle_save s b =
  FileHandler.save_game s b; 
  ANSITerminal.print_string [ANSITerminal.green] ("Game Saved as "^s^"\n");
  Display.print_board b; 
  None 

(** [handle_help b] is [None] but prints the Help menu and current Board. *)
let handle_help b =
  Display.help_menu ();
  Display.print_board b; 
  None

(** [handle_help b] is [None] but prints the list of captured pieces for the
    current player and then redisplays the current board. *)
let handle_captured b = 
  Display.print_captured_pieces b (Board.get_current_player b);
  Display.print_board b;
  None

(** [handle_support b req] is [None] but prints the support information
    encoded by the request [req] given the board [b]. *)
let handle_support b req = 
  Display.p_support_display (Support.handle_player_support b req); 
  None

(** [handle_move (c1,i1,c2,i2) c b] is the [outcome] of the move from 
    [c1,i1] to [c2,i2].  *)
let handle_move (c1,i1,c2,i2) c b =       
  let b_prev = Board.copy_board b in
  match ((Logic.process b c) |> handle_result b b_prev c1 i1 c2 i2) with
  | None -> Display.print_board b; None
  | outcome -> outcome

(** [parse_input b str] interprets the player's input as a command
    and responds to the command appropriately. 
    Requires: 
    [b] is the current Board
    [str] is the input of the current player *)
let parse_input b str opp =
  let c = (Command.parse Cpu.next_move str) in
  match c with
  | Resign -> handle_resign (Board.get_current_player b) b
  | Draw -> handle_draw b opp
  | Save s -> handle_save s b
  | Help -> handle_help b
  | Captured -> handle_captured b
  | PSupport req -> handle_support b req
  | Move (c1,i1,c2,i2) as c -> handle_move (c1,i1,c2,i2) c b
  | Log -> Display.print_log b; None
  | exception Command.Invalid ->
    print_string "Invalid command.\n";
    Display.help_menu ();
    Display.print_board b;
    None

(** [print_move col] prints the appropriate prompt for a player of color 
    [col] to input a command.  *)
let print_move = function
  | Board.Black -> print_string "Black's move.\n"; ()
  | Board.White -> print_string "White's move.\n"; ()

(** [display_last_move b] prints the last move performed on Board [b], or
    nothing if there have not been any moves yet.*)
let display_last_move b = 
  match (Board.get_last_move b) with
  | None -> ()
  | Some m -> Display.print_move ((Board.get_move_cnt b) - 1) m true

let is_not_cpu = function
  | Human -> true
  | CPU -> false

let get_human_move () = 
  print_string "> ";
  (Display.get_input ())

let get_cpu_move b =
  let (c1, i1, c2, i2) = (Cpu.next_move b) in
  (Char.escaped c1)^(string_of_int i1)^" to "^
  (Char.escaped c2)^(string_of_int i2)

(** [get_next_move b] is the next move to perform for the game on Board [b].
    If it is the human's turn, the user is prompted via the console to input
    a move; otherwise, the CPU is tasked with determining a move and that is
    returned instead.*)
let get_next_move b oppt person =
  if (person || (is_not_cpu oppt)) then get_human_move ()
  else get_cpu_move b

let is_persons_turn flag oppt = flag || (is_not_cpu oppt)

(** [play_board b oopt person] prints the current board, 
    prompts a player or CPU to  input a command, accepts player input, 
    and delegates the handling of that input appropriately.  *)
let rec play_board b oppt person =
  display_last_move b;
  let curr = (Board.get_current_player b) in curr |> print_move;
  let curr' = (Board.get_current_player b) in
  let move = (get_next_move b oppt person) in
  match (parse_input b move oppt) with
  | None -> begin
      if (Board.get_current_player b) = curr' then (* Illegal Move *)
        play_board b oppt person
      else
        play_board b oppt (not person)
    end
  | outcome -> outcome

let play b oppt human_start = 
  Display.print_board b; 
  play_board b oppt human_start
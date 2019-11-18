
type outcome = 
  | Win of Board.color
  | Draw

let handle_resign color b =
  let c = Display.get_color_str color in
  let copp = Display.get_opp_color_str color in
  let str = c ^ " has resigned. " ^ copp ^ " wins!\n" in
  ANSITerminal.print_string [ANSITerminal.green] str;
  Display.print_board b;
  Some (Win (Board.get_opp_color color))

let print_draw_prompt b = 
  let curr = Board.get_current_player b in
  let c = Display.get_color_str curr in
  let c' = Display.get_opp_color_str curr in
  let s = c^" has requested a draw. If "^c'^" agrees, type \"Agree\".\n" in
  ANSITerminal.print_string [ANSITerminal.red] s;
  Display.print_board b;
  print_string (c' ^ "'s input:\n>")

(** [handle_draw b] handles the result of one player requesting a draw. 
    If the other player agrees to the draw, the program terminates, otherwise
    the game continues with the current player.  *)
let handle_draw b = 
  print_draw_prompt b;
  if ((Display.get_input ()) |> String.lowercase_ascii) = "agree" then 
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

let handle_checkmate b =
  ANSITerminal.erase ANSITerminal.Screen;
  let col = Board.get_current_player b in
  let c = Display.get_color_str col in
  ANSITerminal.print_string [ANSITerminal.green] ("CHECKMATE! "^c^" wins!\n");
  Display.print_board b;
  Some (Win col)

(** [handle_result b b_prev c1 i1 c2 i2] handles the result of 
    the legality of a player's move command.  *)
let handle_result b b_prev c1 i1 c2 i2 = function
  | Logic.Legal -> begin
      Display.capture_message b_prev c1 i1 c2 i2; 
      Board.next_player b;
      None
    end
  | Logic.Illegal str -> begin
      print_string ("That move is illegal. "^str^" Please retry.\n");
      None
    end
  | Logic.Checkmate -> handle_checkmate b
  | Logic.Stalemate -> begin
      ANSITerminal.erase ANSITerminal.Screen;
      ANSITerminal.print_string [ANSITerminal.red] "STALEMATE!\n";
      Display.print_board b;
      Some Draw
    end

(** [parse_input b str] interprets the player's input as a command
    and responds to the command appropriately.  *)
let parse_input b str = 
  ANSITerminal.erase ANSITerminal.Screen;
  match (Command.parse str) with
  | Resign -> handle_resign (Board.get_current_player b) b
  | Draw -> handle_draw b
  | Help -> begin
      Display.help_menu ();
      Display.print_board b; 
      None
    end
  | Captured -> begin
      Display.print_captured_pieces b (Board.get_current_player b);
      Display.print_board b;
      None
    end
  | PSupport req -> begin
      Support.handle_player_support b req; 
      None
    end
  | Move (c1,i1,c2,i2) as c -> begin
      let b_prev = Board.copy_board b in
      match ((Logic.process b c) |> handle_result b b_prev c1 i1 c2 i2) with
      | None -> Display.print_board b; None
      | outcome -> outcome
    end
  | exception Command.Invalid -> begin
      print_string "Invalid command.\n";
      Display.help_menu ();
      Display.print_board b;
      None
    end

(** [print_move col] prints the appropriate prompt for a player of color 
    [col] to input a command.  *)
let print_move = function
  | Board.Black -> print_string "Black's move.\n"; ()
  | Board.White -> print_string "White's move.\n"; ()

(** [play_chess b] prints the current board, prompts a player to 
    input a command, accepts player input, and delegates the 
    hdanling of that input appropriately.  *)
let rec play_board b = 
  (Board.get_current_player b) |> print_move;
  print_string "> ";
  match (parse_input b (Display.get_input ())) with
  | None -> play_board b
  | outcome -> outcome

let play () = 
  let b = Board.init_state () in  
  Display.print_board b; 
  play_board b
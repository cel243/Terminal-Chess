
(** [handle_result b b_prev c1 i1 c2 i2] handles the result of 
    the legality of a player's move command.  *)
let handle_result b b_prev c1 i1 c2 i2 = function
  | Logic.Legal -> 
    Display.capture_message b_prev c1 i1 c2 i2; 
    Board.next_player b
  | Logic.Illegal str -> 
    print_string ("That move is illegal. "^str^" Please retry.\n")
  | Logic.Checkmate -> begin
      match (Board.get_current_player b) with
      | Black -> 
        ANSITerminal.erase ANSITerminal.Screen;
        ANSITerminal.print_string [ANSITerminal.green] "CHECKMATE! Black wins!\n"; 
        Display.print_board b; exit 0
      | White -> 
        ANSITerminal.erase ANSITerminal.Screen;
        ANSITerminal.print_string [ANSITerminal.green] "CHECKMATE! White wins!\n";
        Display.print_board b; exit 0
    end
  | Logic.Stalemate ->
    ANSITerminal.erase ANSITerminal.Screen;
    ANSITerminal.print_string [ANSITerminal.red] "STALEMATE!\n";
    Display.print_board b; exit 0

(** [handle_draw b] handles the result of one player requesting a draw. 
    If the other player agrees to the draw, the program terminates, otherwise
    the game continues with the current player.  *)
let handle_draw b = 
  let () = (
    match Board.get_current_player b with 
    | White -> 
      ANSITerminal.print_string [ANSITerminal.red] 
        ("White has requested a draw." 
         ^" If Black agrees, input 'AGREE'.\n");
      Display.print_board b;
      print_string "Black's Input.\n>";
    | Black -> 
      ANSITerminal.print_string [ANSITerminal.red]
        ("Black has requested a draw."
         ^" If White agrees, input 'AGREE'.\n");
      Display.print_board b;
      print_string "White's Input.\n>"; ) 
  in 
  match read_line () with
  | exception End_of_file -> print_string "Goodbye.\n"; ()
  | str -> if String.uppercase_ascii str = "AGREE" 
    then (
      ANSITerminal.erase ANSITerminal.Screen;
      ANSITerminal.print_string [ANSITerminal.green]  
        "It's a draw!\n"; 
      Display.print_board b; exit 0 )
    else (
      ANSITerminal.erase ANSITerminal.Screen;
      ANSITerminal.print_string [ANSITerminal.red] 
        ("\nThe other player did not agree to the draw."
         ^ " Please continue the game or resign.\n");
      Display.print_board b;)

(** [parse_input b str] interprets the player's input as a command
    and responds to the command appropriately.  *)
let parse_input b str = 
  match (Command.parse str) with
  | Resign -> begin
      match Board.get_current_player b with 
      | White -> 
        ( ANSITerminal.erase ANSITerminal.Screen;
          ANSITerminal.print_string [ANSITerminal.green] 
            "White has resigned. Black wins!\n";
          Display.print_board b; exit 0) 
      | Black -> 
        ( ANSITerminal.erase ANSITerminal.Screen;
          ANSITerminal.print_string [ANSITerminal.green] 
            "Black has resigned. White wins!\n";
          Display.print_board b; exit 0) 
    end
  | Draw ->  ANSITerminal.erase ANSITerminal.Screen; handle_draw b
  | Help -> ANSITerminal.erase ANSITerminal.Screen; Display.help_menu (); 
    Display.print_board b;
  | Captured -> ANSITerminal.erase ANSITerminal.Screen; 
    Display.print_captured_pieces b (Board.get_current_player b);
    Display.print_board b
  | PSupport req -> ANSITerminal.erase ANSITerminal.Screen; 
    Support.handle_player_support b req 
  | Move (c1,i1,c2,i2) as c -> begin
      ANSITerminal.erase ANSITerminal.Screen;
      let b_prev = Board.copy_board b in 
      (Logic.process b c) |> handle_result b b_prev c1 i1 c2 i2 ;
      Display.print_board b;
    end
  | exception Command.Invalid -> 
    ANSITerminal.erase ANSITerminal.Screen;
    print_string "Invalid command.\n";
    Display.help_menu ();
    Display.print_board b

(** [print_move col] prints the appropriate prompt for a player of color 
    [col] to input a command.  *)
let print_move = function
  | Board.Black -> print_string "Black's move.\n"; ()
  | Board.White -> print_string "White's move.\n"; ()

(** [play_chess b] prints the current board, prompts a player to 
    input a command, accepts player input, and delegates the 
    hdanling of that input appropriately.  *)
let rec play_chess b = 
  (Board.get_current_player b) |> print_move;
  print_string "> ";
  match read_line () with
  | exception End_of_file -> print_string "Goodbye.\n"; ()
  | str -> parse_input b str; play_chess b

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.erase ANSITerminal.Screen;
  print_string "Welcome to chess.\n";
  let b = Board.init_state () in  
  Display.print_board b; play_chess b 

(* Execute the game engine. *)
let () = main ()
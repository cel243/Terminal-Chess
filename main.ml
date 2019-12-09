open Unix

(** Represents the context in which Chess games for this application instance
    are to be played. *)
type gametype = 
  | Exhibition of Board.t 
  | Series

(** [handle_load ()] prompts the user for a filename and returns 
    [Exhibition b] where [b] is the Board derived from the savefile with
    that filename. *)
let rec handle_load () = 
  print_string "Please enter the name of the saved game file.\n";
  print_string "> ";
  let f_name =  (Display.get_input ()) |> String.uppercase_ascii in 
  try let brd = FileHandler.load_game (f_name^".json")  in 
    Exhibition brd 
  with _ -> 
    print_string "Invalid file name. Please try again.\n";
    print_string "> ";
    handle_load ()

(** [prompt_gametype ()] is prompts the user for the desired game mode to play
    and returns one of: [Exhibition b] where [b] is either a new Board and 
    a loaded one from a saved file; [Series] for a new tournament; or exits
    if the user types "exit". *)
let rec prompt_gametype () = 
  print_string "Which mode would you like to play?\n";
  print_string "For an exhibiton match, type \"quick\".\n";
  print_string "For a tournament, type \"tournament\".\n";
  print_string "To load a previous game, type \"load\".\n";
  print_string "> ";
  match (Display.get_input ()) |> String.lowercase_ascii with
  | "quick" -> Exhibition (Board.init_state FileHandler.load_game)
  | "tournament" -> Series
  | "load" -> handle_load () 
  | "exit" -> print_string "Goodbye!"; exit 0
  | _ -> print_string "Invalid mode. Please retry.\n"; prompt_gametype ()

(** [prompt_opponent_type ()] prompts the user for the desired opponent
    to face, and is either [Human] or [CPU]. *)
let rec prompt_opponent_type () = 
  print_string "Who would you like to play against?\n";
  print_string "For a human player, type \"local\"\n";
  print_string "For a computer player, type \"cpu\"\n";
  print_string "> ";
  match (Display.get_input ()) |> String.lowercase_ascii with
  | "local" -> Game.Human
  | "cpu" -> Game.CPU
  | _ -> begin
      print_string "Invalid player type. Please retry.\n"; 
      prompt_opponent_type ()
    end

(** [main ()] prompts for the game to play, then starts the game. *)
let main () =
  ANSITerminal.erase ANSITerminal.Screen;
  print_string "Welcome to chess.\n";
  let opp = prompt_opponent_type () in
  match (prompt_gametype ()) with 
  | Exhibition brd -> begin
      let _ = Game.play brd opp true in ()
    end
  | Series -> begin
      (Tournament.play (Tournament.create opp) false); ()
    end

(* Execute the game engine. *)
let () = main ()
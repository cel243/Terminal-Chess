
(** *)

(**
 * AF: t represents a tournament with:
 *   - The first element being the number of games to be played.
 *   - The second element being the number of completed games up until this point.
 *   - The third element being the color that Player 1 played as previously. 
 *   - The fourth element being Player 1's score.
 *   - The fifth element being Player 2's score.
 *   -  The sixth element indicating the type of Player 2 (Human or CPU)
 * 
 * RI: 
 *    -  The first element is a positive, non-zero integer.
 *    -  The second element is a non-negative integer.
 *    - The third element is a type of Board.color and is [Black] when initialized
 *    - The fourth element is a non-negative float.
 *    - The fifth element is a non-negative float.
*)
type t = 
  (
    int   *       (* Best of ___ *)
    int   *       (* Games Played *)
    Board.color * (* Player 1's previous color *)
    float *       (* Player 1 Score *)
    float *       (* Player 2 Score *)
    Game.opponent (* Player 2 Type *)
  )

type player =
  | PlayerOne
  | PlayerTwo

type score = float

let rec create opp = 
  print_string "How many games are to be played?\n";
  print_string "> ";
  try
    ((int_of_string (Display.get_input ())), 0, Board.Black, 0.0, 0.0, opp)
  with _ -> begin
      print_string "That is not a valid number. Please retry.\n";
      create opp
    end

let get_games_played (_, n, _, _, _, _) = n

let get_best_of_cnt (p, _, _, _, _, _) = p

let get_score t color = match color, t with
  | PlayerOne, (_, _, _, one, _, _) -> one
  | PlayerTwo, (_, _, _, _, two, _) -> two

let is_tied (_, _, _, one, two, _) = one = two

let get_winner = function
  | (_, _, _, one, two, _) -> begin
      if one = two then None
      else if one < two then Some PlayerTwo
      else Some PlayerOne
    end

(** [get_opp_type t] returns [Human] if Player 2 is controlled by a real person,
    and is [CPU] if Player 2 is controlled by the computer. *)
let get_opp_type (_, _, _, _, _, opp) = opp

let to_score = function
  | Game.Win _ -> 1.0
  | Game.Draw -> 0.5

(** [opp_col c] is [Black] if [c] is [White]; is [White] if [c] is [Black] *)
let opp_col = function
  | Board.White -> Board.Black
  | Board.Black -> Board.White

let get_player_with_color (_, _, c, _, _, _) col = 
  if c = col then PlayerTwo
  else PlayerOne

(** [update_result t res] adjusts the players' score(s) according to the
    result [res] and advances the tournamnet forward one game by
    switching player colors and incrementing the completed game count. *)
let update_result ((p, n, c, one, two, opp) as t) res =
  let addition = to_score res in 
  match res with
  | Game.Draw -> (p, n+1, opp_col c, one +. addition, two +. addition, opp)
  | Game.Win v -> begin
      match (get_player_with_color t v) with
      | PlayerOne ->  (p, n+1, opp_col c, one +. addition, two, opp)
      | PlayerTwo -> (p, n+1, opp_col c, one, two +. addition, opp)
    end

let update ((p, n, c, one, two, opp) as t) outcome = 
  match outcome with
  | None -> t
  | Some res -> update_result t res

(** [col_to_string c] is "White" is [c] is [White] and is "Black" if [c] is 
    [Black]. *)
let col_to_string = function
  | Board.White -> "White"
  | Board.Black -> "Black"

let get_new_player_color (_, _, c, _, _, _)  = function
  | PlayerOne -> opp_col c
  | PlayerTwo -> c

let display_tourny (p, n, c, one, two, opp) = 
  print_string "\n---TOURNAMENT RESULTS THUS FAR---";
  print_string ("\nGames played: " ^ (string_of_int n) 
                ^ " of " ^ (string_of_int p));
  print_string ("\nPlayer One's Score: " ^ (string_of_float one));
  (match opp with
   | Game.Human -> begin
       print_string ("\nPlayer Two's Score: " ^ (string_of_float two));
     end
   | Game.CPU -> begin
       print_string ("\nCPU's Score: " ^ (string_of_float two));
       print_string ("\n---------------------------------");
     end);
  ()

let display_game ((_, n, _, _, _, opp) as t) = 
  print_string ("\nNow starting: Game " ^ (string_of_int (n + 1)));
  print_string ("\nPlayer One's Color: " ^ 
                (col_to_string (get_new_player_color t PlayerOne)));
  match opp with
  | Game.Human -> begin
      print_string 
        ("\nPlayer Two's Color: " ^ 
         (col_to_string (get_new_player_color t PlayerTwo)));
    end
  | Game.CPU -> begin 
      print_string 
        ("\nCPU's Color: " ^ 
         (col_to_string (get_new_player_color t PlayerTwo)));
      print_string "\n---------------------------------\n";
      ()
    end

let display_final t = 
  display_tourny t;
  print_string "\nThe results are in...\n";
  (match get_winner t with
   | None -> "It's a draw!\n"
   | Some PlayerOne -> "Player One wins!\n"
   | Some PlayerTwo -> begin
       match get_opp_type t with
       | Game.Human -> "Player Two Wins!\n"
       | Game.CPU -> "CPU Wins!\n"
     end 
  )
  |>
  ANSITerminal.print_string [ANSITerminal.green]

let rec play t prev = 
  if (get_games_played t) = (get_best_of_cnt t) then
    begin
      display_final t
    end
  else 
    begin
      display_tourny t;
      display_game t;
      play (update t 
              (Game.play 
                 (Board.init_state FileHandler.load_game) 
                 (get_opp_type t) (not prev)))
        (not prev)
    end
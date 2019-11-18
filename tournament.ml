
type t = 
  (
    int   *       (* Best of ___ *)
    int   *       (* Games Played *)
    Board.color * (* Player 1's previous color *)
    float *       (* Player 1 Score *)
    float         (* Player 2 Score *)
  )

type player =
  | PlayerOne
  | PlayerTwo

type score = float

let rec create () = 
  print_string "How many games are to be played?\n";
  print_string "> ";
  try
    ((int_of_string (Display.get_input ())), 0, Board.Black, 0.0, 0.0)
  with _ -> begin
      print_string "That is not a valid number. Please retry.\n";
      create ()
    end

let get_games_played = function
  | (_, n, _, _, _) -> n

let get_best_of_cnt = function
  | (p, _, _, _, _) -> p

let get_score t color = match color, t with
  | PlayerOne, (_, _, _, one, _) -> one
  | PlayerTwo, (_, _, _, _, two) -> two

let is_tied = function
  | (_, _, _, one, two) -> one = two

let get_winner = function
  | (_, _, _, one, two) -> begin
      if one = two then None
      else if one < two then Some PlayerTwo
      else Some PlayerOne
    end

let to_score = function
  | Game.Win _ -> 1.0
  | Game.Draw -> 0.5

let opp_col = function
  | Board.White -> Board.Black
  | Board.Black -> Board.White

let get_player_with_color (_, _, c, _, _) col = 
  if c = col then PlayerTwo
  else PlayerOne

let update ((p, n, c, one, two) as t) outcome = 
  match outcome with
  | None -> t
  | Some res ->
    let addition = to_score res in 
    match res with
    | Game.Draw -> (p, n+1, opp_col c, one +. addition, two +. addition)
    | Game.Win v -> begin
        match (get_player_with_color t v) with
        | PlayerOne ->  (p, n+1, opp_col c, one +. addition, two)
        | PlayerTwo -> (p, n+1, opp_col c, one, two +. addition)
      end

let col_to_string = function
  | Board.White -> "White"
  | Board.Black -> "Black"

let get_new_player_color (_, _, c, _, _)  = function
  | PlayerOne -> opp_col c
  | PlayerTwo -> c

let display_tourny (p, n, c, one, two) = 
  print_string "\n---TOURNAMENT RESULTS THUS FAR---";
  print_string ("\nGames played: " ^ (string_of_int n) ^ " of " ^ (string_of_int p));
  print_string ("\nPlayer One's Score: " ^ (string_of_float one));
  print_string ("\nPlayer Two's Score: " ^ (string_of_float two));
  print_string "\n---------------------------------";
  ()

let display_game ((_, n, _, _, _) as t) = 
  print_string ("\nNow starting: Game " ^ (string_of_int (n + 1)));
  print_string ("\nPlayer One's Color: " ^ (col_to_string (get_new_player_color t PlayerOne)));
  print_string ("\nPlayer Two's Color: " ^ (col_to_string (get_new_player_color t PlayerTwo)));
  print_string "\n---------------------------------\n";
  ()

let display_final t = 
  display_tourny t;
  print_string "\nThe results are in...\n";
  (match get_winner t with
   | None -> "It's a draw!\n"
   | Some PlayerOne -> "Player One wins!\n"
   | Some PlayerTwo -> "Player Two wins!\n") 
  |>
  ANSITerminal.print_string [ANSITerminal.green]

let rec play t = 
  if (get_games_played t) = (get_best_of_cnt t) then
    begin
      display_final t
    end
  else 
    begin
      display_tourny t;
      display_game t;
      play (update t (Game.play ()))
    end

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

let get_opp_type (_, _, _, _, _, opp) = opp

let to_score = function
  | Game.Win _ -> 1.0
  | Game.Draw -> 0.5

let opp_col = function
  | Board.White -> Board.Black
  | Board.Black -> Board.White

let get_player_with_color (_, _, c, _, _, _) col = 
  if c = col then PlayerTwo
  else PlayerOne

let update ((p, n, c, one, two, opp) as t) outcome = 
  match outcome with
  | None -> t
  | Some res ->
    let addition = to_score res in 
    match res with
    | Game.Draw -> (p, n+1, opp_col c, one +. addition, two +. addition, opp)
    | Game.Win v -> begin
        match (get_player_with_color t v) with
        | PlayerOne ->  (p, n+1, opp_col c, one +. addition, two, opp)
        | PlayerTwo -> (p, n+1, opp_col c, one, two +. addition, opp)
      end

let col_to_string = function
  | Board.White -> "White"
  | Board.Black -> "Black"

let get_new_player_color (_, _, c, _, _, _)  = function
  | PlayerOne -> opp_col c
  | PlayerTwo -> c

let display_tourny (p, n, c, one, two, opp) = 
  print_string "\n---TOURNAMENT RESULTS THUS FAR---";
  print_string ("\nGames played: " ^ (string_of_int n) ^ " of " ^ (string_of_int p));
  print_string ("\nPlayer One's Score: " ^ (string_of_float one));
  (
    match opp with
    | Game.Human -> 
      print_string ("\nPlayer Two's Score: " ^ (string_of_float two))
    | Game.CPU -> print_string ("\nCPU's Score: " ^ (string_of_float two))
  );
  print_string "\n---------------------------------";
  ()

let display_game ((_, n, _, _, _, opp) as t) = 
  print_string ("\nNow starting: Game " ^ (string_of_int (n + 1)));
  print_string ("\nPlayer One's Color: " ^ (col_to_string (get_new_player_color t PlayerOne)));
  (
    match opp with
    | Game.Human -> 
      print_string 
        ("\nPlayer Two's Color: " ^ 
         (col_to_string (get_new_player_color t PlayerTwo)));
    | Game.CPU -> 
      print_string 
        ("\nCPU's Color: " ^ 
         (col_to_string (get_new_player_color t PlayerTwo)));
  );
  print_string "\n---------------------------------\n";
  ()

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

let rec play t = 
  if (get_games_played t) = (get_best_of_cnt t) then
    begin
      display_final t
    end
  else 
    begin
      display_tourny t;
      display_game t;
      play (update t (Game.play (Board.init_state ()) (get_opp_type t)))
    end
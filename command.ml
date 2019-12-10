
type locations = char * int * char * int 
type request = 
  | LegalMoves of (char * int) 
  | LegalMovesIF of (char * int * char * int * char * int ) 
  | UnderAttack 
  | UnderAttackIF of locations
  | CanAttack
  | CanAttackIF of locations
  | Attackers of (char * int) 
  | AttackersIF of (char * int * char * int * char * int ) 
  | Suggest of (Board.t -> locations)
  | SuggestIF of (Board.t -> locations)*locations
type t = 
  | Resign 
  | Draw 
  | Help
  | Captured
  | Move of locations
  | PSupport of request 
  | Log 
  | Save of string 
exception Invalid 

(** [get_locations_3 loc l1 l2] is [(c,i,c1,i1,c2,i2)], where [c,i]
    is the character and integer in [loc], [c1,i1] is the character and 
    integer in [l1], and [c2,i2] is the character and integer in [l2]. 
    Example: get_locations_3 "C3" "A2" "B6" = ('C',3,'A',2,'B',6)
    Requires: [loc, l1, l2] all have length [2] and consist 
    of an uppercase character in A...H followed by an integer
    in 1...8.  *)
let get_locations_3 loc l1 l2 = 
  (String.get loc 0, int_of_char (String.get loc 1) - 48,
   String.get l1 0, int_of_char (String.get l1 1) - 48,
   String.get l2 0, int_of_char (String.get l2 1) - 48 )

(** [get_locations_2 l1 l2] is [(i1,c2,i2)], where [c1,i1] is the character 
    and integer in [l1] and [c2,i2] is the character and integer in [l2]. 
    Example: get_locations_2 "A2" "B6" = ('A',2,'B',6)
    Requires: [loc, l1, l2] all have length [2] and consist 
    of an uppercase character in A...H followed by an integer
    in 1...8.  *)
let get_locations_2 l1 l2 = 
  (String.get l1 0, int_of_char (String.get l1 1) - 48,
   String.get l2 0, int_of_char (String.get l2 1) - 48 )

(** [get_hypothetical_cmmd wrd_ls] is the command that [wrd_ls] represents,
    where [wrd_ls] represents one of the [IF PSupport] commands.
    Raises: Invalid if [wrd_ls] is not a valid command  *)
let get_hypothetical_cmmd f wrd_ls = 
  match wrd_ls with 
  | [loc;"IF"; l1; "TO"; l2] 
    when String.length l1 = 2 
      && String.length l2 = 2 
      && String.length loc = 2 -> 
    PSupport (LegalMovesIF (get_locations_3 loc l1 l2) )
  | ["ATTACKERS";loc;"IF"; l1; "TO"; l2] 
    when String.length l1 = 2 
      && String.length l2 = 2 
      && String.length loc = 2 -> 
    PSupport (AttackersIF (get_locations_3 loc l1 l2))
  | ["UNDER";"ATTACK";"IF"; l1; "TO"; l2] 
    when String.length l1 = 2 && String.length l2 = 2 -> 
    PSupport (UnderAttackIF (get_locations_2 l1 l2))
  | ["CAN";"ATTACK";"IF"; l1; "TO"; l2] 
    when String.length l1 = 2 && String.length l2 = 2 -> 
    PSupport (CanAttackIF (get_locations_2 l1 l2))
  | ["SUGGEST"; "IF"; l1; "TO"; l2] 
    when String.length l1 = 2 && String.length l2 = 2 -> 
    PSupport (SuggestIF (f,(get_locations_2 l1 l2)))
  | _ -> raise Invalid  

(** [get_psupport_command wrd_ls] is the command that [wrd_ls] represents,
    where [wrd_ls] wither represents a Player Support command type. 
    Raises: Invalid if [wrd_ls] is not a valid command  *)
let get_psupport_cmmd f wrd_ls = 
  match wrd_ls with 
  | [loc] when String.length loc = 2 -> 
    PSupport (
      LegalMoves (String.get loc 0, int_of_char (String.get loc 1) - 48))
  | ["ATTACKERS"; loc] when String.length loc = 2 -> 
    PSupport (
      Attackers (String.get loc 0, int_of_char (String.get loc 1) - 48))
  | ["UNDER";"ATTACK"] -> PSupport UnderAttack 
  | ["CAN";"ATTACK"] -> PSupport CanAttack 
  | ["SUGGEST"] -> PSupport (Suggest f)
  | _ -> get_hypothetical_cmmd f wrd_ls

(** [get_command wrd_ls] is the command that [wrd_ls] represents. 
    Raises: Invalid if [wrd_ls] is not a valid command  *)
let get_command f wrd_ls = 
  match wrd_ls with 
  | ["RESIGN"] -> Resign 
  | ["DRAW"] -> Draw 
  | ["HELP"] -> Help 
  | ["CAPTURED"] -> Captured
  | ["SAVE";"AS";s] -> Save s 
  | ["LOG"] -> Log
  | [l1; "TO"; l2] when String.length l1 = 2 && String.length l2 = 2 ->
    Move (get_locations_2 l1 l2)
  | _ -> get_psupport_cmmd f wrd_ls 

let parse f p_in = 
  String.split_on_char ' ' p_in 
  |> List.filter (fun x -> x <> "") 
  |> List.map (fun s -> String.uppercase_ascii s)
  |> get_command f  





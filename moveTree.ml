type c1 = char
type c2 = char
type i1 = int
type i2 = int
type score = float



type t = 
  | Node of score*c1*i1*c2*i2*t*t*t*t*t*t*t*t*t*t*t*t*t*t*t*t
  | Leaf 

let create () =
  (0, Board.get_current_player, [])
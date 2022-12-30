exception InvalidColor of char
exception InvalidID of char
exception InvalidName of string

type t = {
  name : string;
  id : char;
  color : char;
  points : int;
}

type piece_name =
  | Bishop
  | Knight
  | Rook
  | Pawn
  | Queen
  | King

let piece_names = [ "Bishop"; "Knight"; "Rook"; "Pawn"; "Queen"; "King" ]
let ids = [ 'B'; 'N'; 'R'; 'P'; 'Q'; 'K' ]

let check_color (color : char) : char =
  if color = 'B' || color = 'W' then color else raise (InvalidColor color)

let check_id (id : char) : char =
  if List.mem id ids then id else raise (InvalidID id)

let check_name (name : string) : string =
  if List.mem name piece_names then name else raise (InvalidName name)

let check_points (worth : int) : int = assert false

let create_piece (name1 : string) (id1 : char) (color1 : char) (worth : int) : t
    =
  { name = name1; id = id1; color = color1; points = worth }

let make_piece (name : string) (id : char) (color : char) (worth : int) : t =
  create_piece (check_name name) (check_id id) (check_color color) worth

let make_pawn (color : char) : t = make_piece "Pawn" 'P' color 1
let make_rook (color : char) : t = make_piece "Rook" 'R' color 3
let make_bishop (color : char) : t = make_piece "Bishop" 'B' color 3
let make_knight (color : char) : t = make_piece "Knight" 'N' color 5
let make_king (color : char) : t = make_piece "King" 'K' color 100
let make_queen (color : char) : t = make_piece "Queen" 'Q' color 9
let make_empty : t = create_piece "Empty" ' ' ' ' 0

let gen_piece (name : piece_name) (color : char) =
  match name with
  | Rook -> make_rook color
  | Bishop -> make_bishop color
  | Pawn -> make_pawn color
  | King -> make_king color
  | Knight -> make_knight color
  | Queen -> make_queen color

let upgrade (piece : t) (new_piece : piece_name) : t =
  gen_piece new_piece piece.color

let points (piece : t) : int = piece.points
let name (piece : t) : string = piece.name
let id (piece : t) : char = piece.id
let color (piece : t) : char = piece.color
let empty : t = make_empty

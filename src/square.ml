exception InvalidCoords of (int * char)

type t = {
  location : int * char;
  piece : Piece.t;
}

let check_location (coords : int * char) : int * char =
  let x, y = coords in
  let num = Char.code y in
  if (x >= 1 && x <= 8) && num >= 65 && num <= 72 then coords
  else raise (InvalidCoords coords)

let get_position (tile : t) = tile.location
let get_piece (tile : t) = tile.piece

let create_space (coords : int * char) (piece : Piece.piece_name) (color : char)
    : t =
  { location = coords; piece = Piece.gen_piece piece color }

let gen_space (coords : int * char) (piece : Piece.piece_name) (color : char) :
    t =
  create_space (check_location coords) piece color

let empty_space (coords : int * char) : t =
  { location = check_location coords; piece = Piece.empty }

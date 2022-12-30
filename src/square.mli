(** Representation of square data.

    This module represents the data stored squares/tiles on a board, it contains
    coordinates of the square and the piece on the square. It handles creation
    of squares and information fetching from squares.

    Contributors to this file: Tristan Golden *)

exception InvalidCoords of (int * char)
(** Raised when a coords that are not on the chess board are entered*)

type t
(** The abstract type of values representing a tile/space. *)

val get_position : t -> int * char
(** [get_position t] is the position of square that [t] represents ((2, E) is
    the pawn right in front of the king). Requires: [t] is a valid square. *)

val get_piece : t -> Piece.t
(** [get_piece t] is the piece of square that [t] represents. Requires: [t] is a
    valid square. *)

val gen_space : int * char -> Piece.piece_name -> char -> t
(** [get_space coords t] creates a square with position [coords] and piece from
    using the gen_piece constructor with piece name [piece_name] and results in
    a valid square [t] . Requires: [coords] are a valid chess board position and
    [piece_name is a valid \[Piece.piece_name\]]*)

val empty_space : int * char -> t
(** [empty_space coords ] creates a square with position [coords] and an empty
    piece and results in a valid square [t]. Requires: [coords] are a valid
    chess board position and [piece_name is a valid \[Piece.piece_name\]]*)

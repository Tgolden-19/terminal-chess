(** Representation of piece data.

    This module represents the data stored pieces in squares on a board, it
    contains piece name, point value, letter id, and color (piece name: string)
    (point value: int) (letter id: char) (color: char). It handles creation of
    pieces and information fetching from pieces.

    Contributors to this file: Tristan Golden *)

exception InvalidColor of char
(** Raised when an invalid color value is passed. It carries the color of the
    piece. *)

exception InvalidID of char
(** Raised when an invalid cid value is passed. It carries the id of the piece. *)

exception InvalidName of string
(** Raised when an invalid name is passed. It carries the name of the piece. *)

type t
(** The abstract type of values representing a piece. *)

type piece_name =
  | Bishop
  | Knight
  | Rook
  | Pawn
  | Queen
  | King
      (** The algebraic type corresponding to the names of pieces representing a
          piece. *)

val gen_piece : piece_name -> char -> t
(** [name p] is the name of the piece that [p] represents. Requires: [p] is a
    valid piece. *)

val name : t -> string
(** [name p] is the name of the piece that [p] represents. Requires: [p] is a
    valid piece. *)

val id : t -> char
(** [id p] is the id character of the piece that [p] represents
    ([ 'P'; 'N'; 'R'; 'B'; 'Q'; 'K' ]). Requires: [p] is a valid piece. *)

val color : t -> char
(** [color p] is the color character of the piece that [p] represents ('B' or
    'W'). Requires: [p] is a valid piece. *)

val empty : t
(** [empty] is the piece representative of a on significant type with no
    affiliation (not black nor white) with no name or identifiers *)

val points : t -> int
(** [point p] is the point value of the piece that [p] represents. Requires: [p]
    is a valid piece. *)

val upgrade : t -> piece_name -> t
(** [point t piece_name] is the function that takes in a piece and upgrades it
    to piece of name [piece_name]. Requires: [piece_name] is a valid piece name. *)

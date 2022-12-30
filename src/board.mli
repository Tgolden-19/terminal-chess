(** This is the representation of the chess board. It handles all the moving for
    each piece along with the all the individual rules to chess that have been
    implemented. The board is represented by a 2D array.

    Contributors to this file: Tristan Golden, Jonas Funk, Luke Griffiths

    Project Contributors: Tristan Golden, Jonas Funk, Luke Griffiths *)

type t
(** The abstract type of values representing a piece. *)

val player : int ref
(**player is either [1] for player one or [2] for player 2*)

exception NoPiece
(** Raised when a player tries to move from a spot that has no piece *)

exception WrongPiece
(** Raised when a player tries to move a piece that isn't theirs*)

exception NoMove
(** Raised when a player tries to move a piece to the same place they are moving
    from*)

exception SameColor
(** Raised when a player tries to move a piece to a place that already has one
    of their pieces*)

exception BadPawn
(** Raised when a player tries to move a pawn to a place that isn't allowed*)

exception BadKnight
(** Raised when a player tries to move a knight to a place that isn't allowed*)

exception BadCastle
(** Raised when a player tries to do a castle that isn't allowed*)

exception BadRook
(** Raised when a player tries to move a rook to a place that isn't allowed*)

exception BadBishop
(** Raised when a player tries to move a bishop to a place that isn't allowed*)

exception BadQueen
(** Raised when a player tries to move a queen to a place that isn't allowed*)

exception BadKing
(** Raised when a player tries to do a king move that isn't allowed*)

exception WTFNoKing
(** Raised when there is no king to be found on the board *)

exception IntoCheck
(** Raised when the player is going to move their king into check which is to be
    disallowed*)

exception Win of string
(** Raised when a players king is taken and thus the other player has won *)

val init_board : t
(** [init_board] is the original board that the game starts with*)

val init_test_board : t
(** [init_test_board] is a board with pieces in play to be used for testing*)

val make_test_board : Square.t array array -> t
(** [make_test_board] changes array of arrays to the board type with pieces in
    play to be used for testing*)

val get_piece_id_from_square : int -> int -> t -> char
(**[get_piece_id_from_square row col chess_board] gets the id/character for a
   piece that is in the square row col. Requires [row] and [col] are valid
   integers in the range 0-7*)

val get_piece_color : int -> int -> t -> char
(**[get_piece_color row col chess_board] gets the color of a piece that is in
   the square row col. Requires [row] and [col] are valid integers in the range
   0-7*)

val move_piece : int -> int -> int -> int -> t -> unit
(**[move_piece col1 row1 col2 row2] moves the piece from [col1], [row1] to
   [col2], [row2]. This function also checks whether or not the move specified
   by the user is valid.*)

val try_move_rook : int -> int -> int -> int -> t -> exn -> unit
(**[try_move_rook] checks whether or not a move is valid from [col1], [row1] to
   [col2], [row2]. If it is a valid move, then it calls move_piece. On an
   unallowed move, it raises [BadRook] *)

val try_move_bishop : int -> int -> int -> int -> t -> exn -> unit
(**[try_move_bishop] checks whether or not a move is valid from [col1], [row1]
   to [col2], [row2]. If it is a valid move, then it calls move_piece. On an
   unallowed move, it raises [BadBishop] *)

val try_move_queen : int -> int -> int -> int -> t -> exn -> unit
(**[try_move_queen] checks whether or not a move is valid from [col1], [row1] to
   [col2], [row2]. If it is a valid move, then it calls move_piece. On an
   unallowed move, it raises [BadQueen] *)

val try_move_pawn : int -> int -> int -> int -> t -> unit
(** [try_move_pawn c1 r1 c2 r2 cb] attempts to move a pawn from location [c1],
    [r1] to [c2], [r2] on chess board [cb]. If this move is valid (i.e. it
    follows the basic chess rules), then the pieces are moved, otherwise
    [BadPawn] is raised.*)

val try_move_knight : int -> int -> int -> int -> t -> unit
(** [try_move_knight c1 r1 c2 r2 cb] attempts to move a knight from location
    [c1], [r1] to [c2], [r2] on chess board [cb]. If this move is valid (i.e. it
    follows the basic chess rules), then the pieces are moved, otherwise
    [BadKnight] is raised.*)

val try_move_king : int -> int -> int -> int -> t -> unit
(** [try_move_king c1 r1 c2 r2 cb] attempts to move the king from location [c1],
    [r1] to [c2], [r2] on chess board [cb]. If this move is valid (i.e. it
    follows the basic chess rules), then the pieces are moved, otherwise
    [BadKing] is raised.*)

val try_move : int -> int -> int -> int -> t -> unit
(**[try_move] checks whether or not a move is valid from [col1], [row1] to
   [col2], [row2]. If it is a valid move, then it calls move_piece. On an
   unallowed move, it raises one of the exceptions above*)

val check_warn : t -> unit
(**[check warn board] checks whether there is a check for either king on the
   board and warns the player of which king is in check and both if both are in
   check*)

val try_castle : int -> int -> int -> int -> t -> unit
(**[try_castle] checks whether or not a castle is valid from [col1], [row1] to
   [col2], [row2]. If it is a valid castle, the king and rook will be swapped.
   On an disallowed castle, it raises one [BadCastle]*)

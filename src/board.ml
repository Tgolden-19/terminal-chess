(* Contributors to this file: Tristan Golden, Jonas Funk, Luke Griffiths

   Project Contributors: Tristan Golden Jonas Funk Luke Griffiths*)
open Square
open Piece

type t = { board : Square.t array array }

let player = ref 1
let debug = false

(* These are the exceptions for all the unallowed moves *)
exception NoPiece
exception WrongPiece
exception NoMove
exception SameColor
exception BadPawn
exception BadKnight
exception BadKing
exception BadRook
exception BadBishop
exception BadQueen
exception BlockedCheck
exception WTFNoKing
exception BadCastle
exception SillyPiece of char
exception IntoCheck
exception InCheck
exception Win of string

let init_board : t =
  {
    board =
      [|
        [|
          gen_space (1, 'A') Rook 'W';
          gen_space (1, 'A') Knight 'W';
          gen_space (1, 'A') Bishop 'W';
          gen_space (1, 'A') Queen 'W';
          gen_space (1, 'A') King 'W';
          gen_space (1, 'A') Bishop 'W';
          gen_space (1, 'A') Knight 'W';
          gen_space (1, 'A') Rook 'W';
        |];
        Array.make 8 (gen_space (1, 'A') Pawn 'W');
        Array.make 8 (empty_space (1, 'A'));
        Array.make 8 (empty_space (1, 'A'));
        Array.make 8 (empty_space (1, 'A'));
        Array.make 8 (empty_space (1, 'A'));
        Array.make 8 (gen_space (1, 'A') Pawn 'B');
        [|
          gen_space (1, 'A') Rook 'B';
          gen_space (1, 'A') Knight 'B';
          gen_space (1, 'A') Bishop 'B';
          gen_space (1, 'A') Queen 'B';
          gen_space (1, 'A') King 'B';
          gen_space (1, 'A') Bishop 'B';
          gen_space (1, 'A') Knight 'B';
          gen_space (1, 'A') Rook 'B';
        |];
      |];
  }

let init_test_board : t =
  {
    board =
      [|
        [|
          gen_space (1, 'A') Rook 'W';
          gen_space (1, 'A') Knight 'W';
          gen_space (1, 'A') Bishop 'W';
          empty_space (1, 'A');
          gen_space (1, 'A') King 'W';
          empty_space (1, 'A');
          gen_space (1, 'A') Knight 'W';
          gen_space (1, 'A') Rook 'W';
        |];
        [|
          gen_space (1, 'A') Pawn 'W';
          gen_space (1, 'A') Pawn 'W';
          empty_space (1, 'A');
          gen_space (1, 'A') Pawn 'W';
          empty_space (1, 'A');
          gen_space (1, 'A') Pawn 'W';
          gen_space (1, 'A') Pawn 'W';
          gen_space (1, 'A') Pawn 'W';
        |];
        [|
          empty_space (1, 'A');
          empty_space (1, 'A');
          empty_space (1, 'A');
          gen_space (1, 'A') Bishop 'W';
          empty_space (1, 'A');
          empty_space (1, 'A');
          empty_space (1, 'A');
          empty_space (1, 'A');
        |];
        [|
          gen_space (1, 'A') Queen 'W';
          empty_space (1, 'A');
          gen_space (1, 'A') Pawn 'W';
          empty_space (1, 'A');
          gen_space (1, 'A') Pawn 'W';
          empty_space (1, 'A');
          empty_space (1, 'A');
          empty_space (1, 'A');
        |];
        [|
          gen_space (1, 'A') Pawn 'B';
          empty_space (1, 'A');
          empty_space (1, 'A');
          empty_space (1, 'A');
          empty_space (1, 'A');
          empty_space (1, 'A');
          empty_space (1, 'A');
          gen_space (1, 'A') Pawn 'B';
        |];
        [|
          empty_space (1, 'A');
          empty_space (1, 'A');
          empty_space (1, 'A');
          empty_space (1, 'A');
          empty_space (1, 'A');
          gen_space (1, 'A') Knight 'B';
          empty_space (1, 'A');
          gen_space (1, 'A') Rook 'B';
        |];
        [|
          empty_space (1, 'A');
          gen_space (1, 'A') Pawn 'B';
          gen_space (1, 'A') Pawn 'B';
          gen_space (1, 'A') Pawn 'B';
          gen_space (1, 'A') Pawn 'B';
          gen_space (1, 'A') Pawn 'B';
          gen_space (1, 'A') Pawn 'B';
          empty_space (1, 'A');
        |];
        [|
          gen_space (1, 'A') Rook 'B';
          gen_space (1, 'A') Knight 'B';
          gen_space (1, 'A') Bishop 'B';
          gen_space (1, 'A') Queen 'B';
          gen_space (1, 'A') King 'B';
          gen_space (1, 'A') Bishop 'B';
          empty_space (1, 'A');
          empty_space (1, 'A');
        |];
      |];
  }

let make_test_board (test_board : Square.t array array) : t =
  { board = test_board }

let get_piece_id_from_square (col : int) (row : int) (chess_board : t) : char =
  id (get_piece chess_board.board.(row).(col))

let get_piece_color (col : int) (row : int) (chess_board : t) : char =
  color (get_piece chess_board.board.(row).(col))

let move_piece (col1 : int) (row1 : int) (col2 : int) (row2 : int)
    (chess_board : t) : unit =
  chess_board.board.(row2).(col2) <- chess_board.board.(row1).(col1);
  chess_board.board.(row1).(col1) <- empty_space (1, 'A')

let rec upgrade_pawn (col : int) (row : int) (color : char) (chess_board : t) :
    unit =
  print_endline
    "Choose a piece to upgrade your pawn to: \n\
     (1) Knight\n\
     (2) Rook\n\
     (3) Bishop \n\
     (4) Queen";
  match read_line () with
  | exception End_of_file -> ()
  | "1" -> chess_board.board.(row).(col) <- gen_space (1, 'A') Knight color
  | "2" -> chess_board.board.(row).(col) <- gen_space (1, 'A') Rook color
  | "3" -> chess_board.board.(row).(col) <- gen_space (1, 'A') Bishop color
  | "4" -> chess_board.board.(row).(col) <- gen_space (1, 'A') Queen color
  | _ ->
      print_endline "That is a bad input. Input one of either 1-4";
      upgrade_pawn col row color chess_board

let dist_calc (c1 : int) (r1 : int) (c2 : int) (r2 : int) : int =
  let x_dist = r2 - r1 in
  let y_dist = c2 - c1 in
  let temp = (x_dist * x_dist) + (y_dist * y_dist) in
  int_of_float (sqrt (float_of_int temp))

let get_king (cb : t) (color : char) : int * int =
  let coords = ref (-1, -1) in
  for x = 0 to Array.length cb.board - 1 do
    for y = 0 to Array.length cb.board.(x) - 1 do
      let curr_piece_id = get_piece_id_from_square x y cb in
      let curr_piece_c = get_piece_color x y cb in
      if curr_piece_c = color && curr_piece_id = 'K' then coords := (x, y)
    done
  done;
  let y_temp, x_temp = !coords in
  if x_temp = -1 || y_temp = -1 then raise WTFNoKing else !coords

let check_UDLR_aux (piece_id : char) (piece_c : char) (king_color : char) =
  if piece_id <> 'K' then
    if piece_c = king_color then raise BlockedCheck (* break the loop*)
    else if piece_id = 'P' || piece_id = 'B' || piece_id = 'N' then
      raise (SillyPiece piece_id)
    else if piece_id = 'R' || piece_id = 'Q' then raise InCheck

let check_diag_aux (piece_id : char) (piece_c : char) (king_color : char) =
  if piece_id <> 'K' then
    if piece_c = king_color then raise BlockedCheck (* break the loop*)
    else if piece_id = 'P' || piece_id = 'R' || piece_id = 'N' then
      raise (SillyPiece piece_id)
    else if piece_id = 'B' || piece_id = 'Q' then raise InCheck

let check_up (c : int) (r : int) (cb : t) (color : char) : bool =
  let king_color = color in

  (*checking northern exposure*)
  try
    for x = c to Array.length cb.board.(r) - 1 do
      let piece_id = get_piece_id_from_square x r cb in
      let piece_c = get_piece_color x r cb in
      check_UDLR_aux piece_id piece_c king_color
    done;

    false
  with
  | BlockedCheck -> false
  | SillyPiece id -> false
  | InCheck -> true
  | _ -> false

let check_down (c : int) (r : int) (cb : t) (color : char) : bool =
  let king_color = color in

  (*checking southern exposure*)
  try
    for x = c downto 0 do
      let piece_id = get_piece_id_from_square x r cb in
      let piece_c = get_piece_color x r cb in
      check_UDLR_aux piece_id piece_c king_color
    done;

    false
  with
  | BlockedCheck -> false
  | SillyPiece id -> false
  | InCheck -> true
  | _ -> false

let check_right (c : int) (r : int) (cb : t) (color : char) : bool =
  let king_color = color in

  (*checking eastern exposure*)
  try
    for x = r to Array.length cb.board - 1 do
      let piece_id = get_piece_id_from_square c x cb in
      let piece_c = get_piece_color c x cb in
      check_UDLR_aux piece_id piece_c king_color
    done;
    false
  with
  | BlockedCheck -> false
  | SillyPiece id -> false
  | InCheck -> true
  | _ -> false

let check_left (c : int) (r : int) (cb : t) (color : char) : bool =
  let king_color = color in

  (*checking western exposure*)
  try
    for x = r downto 0 do
      let piece_id = get_piece_id_from_square c x cb in
      let piece_c = get_piece_color c x cb in
      check_UDLR_aux piece_id piece_c king_color
    done;

    false
  with
  | BlockedCheck -> false
  | SillyPiece id -> false
  | InCheck -> true
  | _ -> false

let check_SW (c : int) (r : int) (cb : t) (color : char) : bool =
  let king_color = color in

  let minim = min c r in

  (*checking southwest diagonal exposure (excluding pawns because they are a
    special case)*)
  try
    for x = 0 to minim do
      let piece_id = get_piece_id_from_square (c - x) (r - x) cb in
      let piece_c = get_piece_color (c - x) (r - x) cb in
      if piece_c = king_color then raise BlockedCheck (* break the loop*)
      else if piece_id = 'P' || piece_id = 'R' || piece_id = 'N' then
        raise (SillyPiece piece_id)
      else if piece_id = 'B' || piece_id = 'Q' then raise InCheck
    done;

    false
  with
  | BlockedCheck -> false
  | SillyPiece id -> false
  | InCheck -> true
  | _ -> false

let check_NE (c : int) (r : int) (cb : t) (color : char) : bool =
  let king_color = color in

  let maxim = max c r in
  (*checking northeast diagonal exposure (excluding pawns because they are a
    special case)*)
  try
    for x = 0 to Array.length cb.board - 1 - maxim do
      let piece_id = get_piece_id_from_square (c + x) (r + x) cb in
      let piece_c = get_piece_color (c + x) (r + x) cb in
      check_diag_aux piece_id piece_c king_color
    done;

    false
  with
  | BlockedCheck -> false
  | SillyPiece id -> false
  | InCheck -> true
  | _ -> false

let check_NW (c : int) (r : int) (cb : t) (color : char) : bool =
  let king_color = color in
  let closest = min (abs (c - 8)) r in
  (*checking northwest diagonal exposure (excluding pawns because they are a
    special case)*)
  try
    for x = 0 to closest do
      let piece_id = get_piece_id_from_square (c + x) (r - x) cb in
      let piece_c = get_piece_color (c + x) (r - x) cb in
      check_diag_aux piece_id piece_c king_color
    done;

    false
  with
  | BlockedCheck -> false
  | SillyPiece id -> false
  | InCheck -> true
  | _ -> false

let check_SE (c : int) (r : int) (cb : t) (color : char) : bool =
  let king_color = color in
  let closest = min c (abs (r - 8)) in
  (*checking northwest diagonal exposure (excluding pawns because they are a
    special case)*)
  try
    for x = 0 to closest do
      let piece_id = get_piece_id_from_square (c - x) (r + x) cb in
      let piece_c = get_piece_color (c - x) (r + x) cb in
      check_diag_aux piece_id piece_c king_color
    done;

    false
  with
  | BlockedCheck -> false
  | SillyPiece id -> false
  | InCheck -> true
  | _ -> false

let knight_N (c : int) (r : int) (cb : t) (color : char) : bool =
  let king_color = color in
  try
    let piece_id1 = get_piece_id_from_square (c + 2) (r + 1) cb in
    if piece_id1 = 'N' then
      let piece_color1 = get_piece_color (c + 2) (r + 1) cb in
      if piece_color1 <> king_color then true else false
    else
      false
      ||
      let piece_id2 = get_piece_id_from_square (c + 2) (r - 1) cb in
      if piece_id2 = 'N' then
        let piece_color2 = get_piece_color (c + 2) (r - 1) cb in
        if piece_color2 <> king_color then true else false
      else false
  with _ -> false

let knight_E (c : int) (r : int) (cb : t) (color : char) : bool =
  let king_color = color in
  try
    let piece_id1 = get_piece_id_from_square (c + 1) (r + 2) cb in
    if piece_id1 = 'N' then
      let piece_color1 = get_piece_color (c + 1) (r + 2) cb in
      if piece_color1 <> king_color then true else false
    else
      false
      ||
      let piece_id2 = get_piece_id_from_square (c - 1) (r + 2) cb in
      if piece_id2 = 'N' then
        let piece_color2 = get_piece_color (c - 1) (r + 2) cb in
        if piece_color2 <> king_color then true else false
      else false
  with _ -> false

let knight_W (c : int) (r : int) (cb : t) (color : char) : bool =
  let king_color = color in
  try
    let piece_id1 = get_piece_id_from_square (c + 1) (r - 2) cb in
    if piece_id1 = 'N' then
      let piece_color1 = get_piece_color (c + 1) (r - 2) cb in
      if piece_color1 <> king_color then true else false
    else
      false
      ||
      let piece_id2 = get_piece_id_from_square (c - 1) (r - 2) cb in
      if piece_id2 = 'N' then
        let piece_color2 = get_piece_color (c - 1) (r - 2) cb in
        if piece_color2 <> king_color then true else false
      else false
  with _ -> false

let knight_S (c : int) (r : int) (cb : t) (color : char) : bool =
  let king_color = color in
  try
    let piece_id1 = get_piece_id_from_square (c - 2) (r + 1) cb in
    if piece_id1 = 'N' then
      let piece_color1 = get_piece_color (c - 2) (r + 1) cb in
      if piece_color1 <> king_color then true else false
    else
      false
      ||
      let piece_id2 = get_piece_id_from_square (c - 2) (r - 1) cb in
      if piece_id2 = 'N' then
        let piece_color2 = get_piece_color (c - 2) (r - 1) cb in
        if piece_color2 <> king_color then true else false
      else false
  with _ -> false

let check_knight (c : int) (r : int) (cb : t) (color : char) : bool =
  knight_N c r cb color || knight_S c r cb color || knight_E c r cb color
  || knight_W c r cb color

let check_pawn (c : int) (r : int) (cb : t) (color : char) : bool =
  let king_color = color in
  if king_color = 'W' then
    let piece_color1 = get_piece_color (c + 1) (r + 1) cb in
    if piece_color1 <> king_color then
      let piece_id1 = get_piece_id_from_square (c + 1) (r + 1) cb in
      if piece_id1 = 'P' then true else false
    else false
  else
    false
    ||
    if king_color = 'W' then
      let piece_color1 = get_piece_color (c - 1) (r + 1) cb in
      if piece_color1 <> king_color then
        let piece_id1 = get_piece_id_from_square (c - 1) (r + 1) cb in
        if piece_id1 = 'P' then true else false
      else false
    else
      false
      ||
      if king_color = 'B' then
        let piece_color1 = get_piece_color (c - 1) (r - 1) cb in
        if piece_color1 <> king_color then
          let piece_id1 = get_piece_id_from_square (c - 1) (r - 1) cb in
          if piece_id1 = 'P' then true else false
        else false
      else
        false
        ||
        if king_color = 'B' then
          let piece_color1 = get_piece_color (c + 1) (r - 1) cb in
          if piece_color1 <> king_color then
            let piece_id1 = get_piece_id_from_square (c + 1) (r - 1) cb in
            if piece_id1 = 'P' then true else false
          else false
        else false

let check_full (c : int) (r : int) (cb : t) (color : char) : bool =
  let cardinals =
    check_NE c r cb color || check_NW c r cb color || check_up c r cb color
    || check_SE c r cb color || check_SW c r cb color || check_down c r cb color
    || check_left c r cb color || check_right c r cb color
  in
  let specials = check_knight c r cb color || check_pawn c r cb color in
  cardinals || specials

let check_warn (chess_board : t) =
  let y_w_king, x_w_king = get_king chess_board 'W' in
  if check_full y_w_king x_w_king chess_board 'W' then
    print_endline "Blue King is in check. Beware...";
  let y_b_king, x_b_king = get_king chess_board 'B' in
  if check_full y_b_king x_b_king chess_board 'B' then
    print_endline "Red King is in check. Beware..."

let rec try_move (col1 : int) (row1 : int) (col2 : int) (row2 : int)
    (chess_board : t) : unit =
  (* Makes sure player moves their piece*)
  let c1 = get_piece_color col1 row1 chess_board in
  let c2 = get_piece_color col2 row2 chess_board in
  let was_king = get_piece_id_from_square col2 row2 chess_board = 'K' in
  (if not ((!player = 1 && c1 = 'W') || (!player = 2 && c1 = 'B') || c1 = ' ')
  then raise WrongPiece
  else if row1 = row2 && col1 = col2 then raise NoMove
  else if c1 = c2 && c1 != ' ' then raise SameColor
  else
    match get_piece_id_from_square col1 row1 chess_board with
    | ' ' -> raise NoPiece (* Player tries to move piece that isn't there*)
    | 'P' -> try_move_pawn col1 row1 col2 row2 chess_board
    | 'N' -> try_move_knight col1 row1 col2 row2 chess_board
    | 'R' -> try_move_rook col1 row1 col2 row2 chess_board BadRook
    | 'B' -> try_move_bishop col1 row1 col2 row2 chess_board BadBishop
    | 'Q' -> try_move_queen col1 row1 col2 row2 chess_board BadQueen
    | 'K' -> try_move_king col1 row1 col2 row2 chess_board
    | _ -> move_piece col1 row1 col2 row2 chess_board);
  if was_king then raise (Win (if !player = 1 then "Blue" else "Red"))

and try_move_pawn (c1 : int) (r1 : int) (c2 : int) (r2 : int) (cb : t) : unit =
  match (abs (c2 - c1), abs (r1 - r2)) with
  | 0, 2 ->
      (* First make sure that the pawn can only move forward vertically twice
         with no pieces in the way if is in its starting position*)
      if
        (!player = 1 && r1 = 1
         && r2 - r1 > 0
         && get_piece_id_from_square c1 (r1 + 1) cb
            = ' ' (* Player 1 only upward, and can't skip over pieces*)
        || !player = 2 && r1 = 6
           && r2 - r1 < 0
           && get_piece_id_from_square c1 (r1 - 1) cb
              = ' ' (* Player 2 only downward*))
        && get_piece_id_from_square c2 r2 cb = ' '
      then move_piece c1 r1 c2 r2 cb
      else raise BadPawn
  | 0, 1 ->
      (* Check to see if the pawn can move foward by one. Make sure that there
         are no pieces directly in front of the pawn*)
      if
        (!player = 1 && r2 - r1 > 0 && get_piece_id_from_square c2 r2 cb = ' ')
        || !player = 2
           && r2 - r1 < 0
           && get_piece_id_from_square c2 r2 cb = ' '
        (* Last line ensures that there is no piece*)
      then move_piece c1 r1 c2 r2 cb
      else raise BadPawn;
      (* Check to see if the pawn has reached the end*)
      if (!player = 1 && r2 == 7) || (!player = 2 && r2 == 0) then
        upgrade_pawn c2 r2 (get_piece_color c2 r2 cb) cb
      else ()
  | 1, 1 ->
      (* Check to see if the pawn can go diagonally. This can only occur when
         there is a piece that is in the proper diagonal position*)
      if
        (!player = 1 && r2 - r1 > 0 && get_piece_id_from_square c2 r2 cb != ' ')
        || !player = 2
           && r2 - r1 < 0
           && get_piece_id_from_square c2 r2 cb != ' '
      then move_piece c1 r1 c2 r2 cb
      else raise BadPawn;
      if (!player = 1 && r2 == 7) || (!player = 2 && r2 == 0) then
        upgrade_pawn c2 r2 (get_piece_color c2 r2 cb) cb
      else ()
  (* Check to see if the pawn has reached the end*)
  | _ -> raise BadPawn

and try_move_knight (c1 : int) (r1 : int) (c2 : int) (r2 : int) (cb : t) : unit
    =
  match (abs (c2 - c1), abs (r1 - r2)) with
  | 1, 2 | 2, 1 -> move_piece c1 r1 c2 r2 cb
  | _ -> raise BadKnight

(* this can't start from exactly the row, it must be one off otherwise it will
   detect the piece at the starting position and immediately declare false*)
and row_traverse_valid (c : int) (rs : int) (rf : int) (piece_color : char)
    (cb : t) : bool =
  match rs = rf with
  | true -> if get_piece_color c rf cb = piece_color then false else true
  | false ->
      if get_piece_color c rs cb <> ' ' then false
      else if rf - rs > 0 then row_traverse_valid c (1 + rs) rf piece_color cb
      else row_traverse_valid c (rs - 1) rf piece_color cb

and diag_traverse_valid (cs : int) (rs : int) (cf : int) (rf : int) (cb : t) :
    bool =
  if abs (rf - rs) <> abs (cf - cs) then false
  else if cs <> cf && get_piece_color cs rs cb <> ' ' then false
  else
    match cf > cs && get_piece_color cs rs cb = ' ' with
    | true ->
        if rf > rs then diag_traverse_valid (cs + 1) (rs + 1) cf rf cb
        else diag_traverse_valid (cs + 1) (rs - 1) cf rf cb
    | false ->
        if cf = cs then true
        else if rf > rs then diag_traverse_valid (cs - 1) (rs + 1) cf rf cb
        else diag_traverse_valid (cs - 1) (rs - 1) cf rf cb

and col_traverse_valid (r : int) (cs : int) (cf : int) (piece_color : char)
    (cb : t) : bool =
  match cs = cf with
  | true -> if get_piece_color cf r cb = piece_color then false else true
  | false ->
      if get_piece_color cs r cb <> ' ' then false
      else if cf - cs > 0 then col_traverse_valid r (cs + 1) cf piece_color cb
      else col_traverse_valid r (cs - 1) cf piece_color cb

and try_move_rook (c1 : int) (r1 : int) (c2 : int) (r2 : int) (cb : t) (e : exn)
    : unit =
  match c1 = c2 with
  | true -> (
      let pcolor = get_piece_color c1 r1 cb in
      match r2 - r1 > 0 with
      | true ->
          if row_traverse_valid c1 (r1 + 1) r2 pcolor cb then
            move_piece c1 r1 c2 r2 cb
          else raise e
      | false ->
          if row_traverse_valid c1 (r1 - 1) r2 pcolor cb then
            move_piece c1 r1 c2 r2 cb
          else raise e)
  | false -> (
      match r1 = r2 with
      | false -> raise e
      | true -> (
          let pcolor = get_piece_color c1 r1 cb in
          match c2 - c1 > 0 with
          | true ->
              if col_traverse_valid r1 (c1 + 1) c2 pcolor cb then
                move_piece c1 r1 c2 r2 cb
              else raise e
          | false ->
              if col_traverse_valid r1 (c1 - 1) c2 pcolor cb then
                move_piece c1 r1 c2 r2 cb
              else raise e))

and try_move_bishop (c1 : int) (r1 : int) (c2 : int) (r2 : int) (cb : t)
    (e : exn) : unit =
  if c2 - c1 > 0 then
    if r2 - r1 > 0 then
      if diag_traverse_valid (c1 + 1) (r1 + 1) c2 r2 cb then
        move_piece c1 r1 c2 r2 cb
      else raise e
    else if diag_traverse_valid (c1 + 1) (r1 - 1) c2 r2 cb then
      move_piece c1 r1 c2 r2 cb
    else raise e
  else if r2 - r1 > 0 then
    if diag_traverse_valid (c1 - 1) (r1 + 1) c2 r2 cb then
      move_piece c1 r1 c2 r2 cb
    else raise e
  else if diag_traverse_valid (c1 - 1) (r1 - 1) c2 r2 cb then
    move_piece c1 r1 c2 r2 cb
  else raise e

and try_move_queen (c1 : int) (r1 : int) (c2 : int) (r2 : int) (cb : t)
    (e : exn) : unit =
  if c1 = c2 || r1 = r2 then try_move_rook c1 r1 c2 r2 cb e
  else try_move_bishop c1 r1 c2 r2 cb e

and try_move_king (c1 : int) (r1 : int) (c2 : int) (r2 : int) (cb : t) : unit =
  let in_range = dist_calc c1 r1 c2 r2 <= 1 in
  (* checking that the intended move is within a distance of 1 (king can only
     move 1 space any direction)*)
  let piece_id = get_piece_id_from_square c2 r2 cb in
  let not_piece = piece_id = ' ' in
  let king_c = get_piece_color c1 r1 cb in
  if check_full c2 r2 cb king_c then raise IntoCheck
  else if in_range && not_piece then move_piece c1 r1 c2 r2 cb
  else
    let piece_c = get_piece_color c2 r2 cb in
    let king_c = get_piece_color c1 r1 cb in
    if piece_c <> king_c && in_range then move_piece c1 r1 c2 r2 cb
    else raise BadKing

let try_castle (c1 : int) (r1 : int) (c2 : int) (r2 : int) (cb : t) : unit =
  let color = get_piece_color c1 r1 cb in
  if
    not (* Makes sure player is moving their piece*)
      ((!player = 1 && color = 'W')
      || (!player = 2 && color = 'B')
      || color = ' ')
  then raise WrongPiece
    (* Castling can only occur in row 1 or row 8 and when pieces are in the
       corner*)
  else if r1 != 0 && r1 != 7 then raise BadCastle
    (* Castling can only occur when the king and rook are in their correct start
       positions*)
  else if c1 != 4 || (c2 != 0 && c2 != 7) then raise BadCastle
  else
    match
      (get_piece_id_from_square c1 r1 cb, get_piece_id_from_square c2 r2 cb)
    with
    | 'K', 'R' ->
        (* This looks at all the positions between the king and rook and makes
           sure that there are no pieces*)
        let x1, x2 = if c1 - c2 > 0 then (1, 3) else (5, 6) in
        for c = x1 to x2 do
          if get_piece_id_from_square c r1 cb != ' ' then raise BadCastle
        done;
        (* ck is where the king is to move and cr is where the rook has to move
           after the castle*)
        let ck, cr = if c1 - c2 > 0 then (2, 3) else (6, 5) in
        move_piece c1 r1 ck r2 cb (* King to ck *);
        move_piece c2 r2 cr r2 cb (* Rook to cr *)
    | _ -> raise BadCastle

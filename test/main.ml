(** Test plan: We used a combination of OUnit testing and manual testing to
    ensure our program is correct. We made OUnit tests to ensure we could
    correctly get the color of all pieces, empty squares, and promoted pieces.
    We also tested the functions to get a piece's id, name, and points value. We
    wrote many test cases to test our method of passing in game instructions.
    These ensure that we parse out invalid (mistyped) commands. Finally, we have
    many test cases to check if a move is illegal (breaks the rules of chess).
    Although we used OUnit to test each component of our program individually,
    we manually tested the collective by playing many games of chess and trying
    invalid moves/commands.

    All of our OUnit tests were designed as black box tests. This was a very
    natural way to test our game, since we know the rules of chess and can
    generate dozens of tests based on what the outcome of a particular move
    should be.

    We are confident that our code is correct. Obviously, we couldn't create
    every possible sequence of moves and test that every possible permutation of
    a chess game was valid; however, since we tested the moves of each piece, we
    can be certain that any collection of these valid moves is also valid. *)

open OUnit2
open Game
open Piece
open Instruction
open Board
open Square

let pawn_white = gen_piece Pawn 'W'
let pawn_black = gen_piece Pawn 'B'
let queen_white = gen_piece Queen 'W'
let queen_black = gen_piece Queen 'B'
let king_white = gen_piece King 'W'
let king_black = gen_piece King 'B'
let rook_white = gen_piece Rook 'W'
let rook_black = gen_piece Rook 'B'
let bishop_white = gen_piece Bishop 'W'
let bishop_black = gen_piece Bishop 'B'
let knight_white = gen_piece Knight 'W'
let knight_black = gen_piece Knight 'B'
let upgrade_pawn_black = upgrade pawn_black Queen
let upgrade_pawn_white = upgrade pawn_white Queen

(*end of piece creation*)
let king_stress_board =
  [|
    [|
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      gen_space (1, 'A') Knight 'W';
      gen_space (1, 'A') Rook 'W';
    |];
    [|
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      gen_space (1, 'A') Pawn 'W';
      empty_space (1, 'A');
      gen_space (1, 'A') Pawn 'W';
      gen_space (1, 'A') Pawn 'W';
    |];
    [|
      empty_space (1, 'A');
      gen_space (1, 'A') Pawn 'W';
      gen_space (1, 'A') Pawn 'W';
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      gen_space (1, 'A') Rook 'B';
      empty_space (1, 'A');
    |];
    [|
      empty_space (1, 'A');
      empty_space (1, 'A');
      gen_space (1, 'A') Knight 'B';
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
    |];
    [|
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      gen_space (1, 'A') King 'W';
      empty_space (1, 'A');
      empty_space (1, 'A');
    |];
    [|
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      gen_space (1, 'A') Bishop 'B';
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
      gen_space (1, 'A') Rook 'B';
      gen_space (1, 'A') Knight 'B';
      gen_space (1, 'A') Bishop 'B';
      gen_space (1, 'A') Queen 'B';
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
    |];
  |]

let king_stress_board2 =
  [|
    [|
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      gen_space (1, 'A') Knight 'W';
      gen_space (1, 'A') Rook 'W';
    |];
    [|
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      gen_space (1, 'A') Pawn 'W';
      empty_space (1, 'A');
    |];
    [|
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      gen_space (1, 'A') Rook 'W';
      empty_space (1, 'A');
    |];
    [|
      empty_space (1, 'A');
      empty_space (1, 'A');
      gen_space (1, 'A') Knight 'W';
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
    |];
    [|
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      gen_space (1, 'A') King 'B';
      empty_space (1, 'A');
      empty_space (1, 'A');
    |];
    [|
      empty_space (1, 'A');
      gen_space (1, 'A') Queen 'W';
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
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
      gen_space (1, 'A') Pawn 'B';
      gen_space (1, 'A') Bishop 'W';
    |];
    [|
      gen_space (1, 'A') Rook 'B';
      gen_space (1, 'A') Knight 'B';
      gen_space (1, 'A') Bishop 'B';
      gen_space (1, 'A') Queen 'B';
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
      empty_space (1, 'A');
    |];
  |]

(** [piece_color_test name piece expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [Piece.color piece].*)
let piece_color_test (name : string) (piece : Piece.t) (expected_output : char)
    : test =
  name >:: fun _ ->
  assert_equal expected_output (Piece.color piece) ~printer:Char.escaped

(** [piece_id_test name piece expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [Piece.id piece piece].*)
let piece_id_test (name : string) (piece : Piece.t) (expected_output : char) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (Piece.id piece) ~printer:Char.escaped

(** [piece_name_test name piece expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [Piece.name piece].*)
let piece_name_test (name : string) (piece : Piece.t) (expected_output : string)
    : test =
  name >:: fun _ ->
  assert_equal expected_output (Piece.name piece) ~printer:String.escaped

(** [piece_points_test name piece expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [Piece.points piece].*)
let piece_points_test (name : string) (piece : Piece.t) (expected_output : int)
    : test =
  name >:: fun _ ->
  assert_equal expected_output (Piece.points piece) ~printer:string_of_int

(** [to_instruction_test name input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [to_instruction input].*)
let to_instruction_test (name : string) (input : string)
    (expected_output : instruction) : test =
  name >:: fun _ -> assert_equal expected_output (to_instruction input)

(**[illegal_piece_move_test] constructs an OUnit test named [name] that asserts
   the quality of [expected_output] with [try_move c1 r1 c2 r2 cb]*)
let illegal_piece_move_test (name : string) c1 r1 c2 r2 cb
    (expected_output : exn) : test =
  name >:: fun _ ->
  assert_raises expected_output (fun () -> try_move c1 r1 c2 r2 cb)

let piece_tests =
  [
    piece_color_test "testing get color on a white pawn" pawn_white 'W';
    piece_color_test "testing get color on a black pawn" pawn_black 'B';
    piece_color_test "testing get color on a white queen" queen_white 'W';
    piece_color_test "testing get color on a black queen" queen_black 'B';
    piece_color_test "testing get color on a white king" king_white 'W';
    piece_color_test "testing get color on a black king" king_black 'B';
    piece_color_test "testing get color on a white rook" rook_white 'W';
    piece_color_test "testing get color on a black rook" rook_black 'B';
    piece_color_test "testing get color on a white bishop" bishop_white 'W';
    piece_color_test "testing get color on a black bishop" bishop_black 'B';
    piece_color_test "testing get color on a white knight" knight_white 'W';
    piece_color_test "testing get color on a black knight" knight_black 'B';
    piece_id_test "testing get id on a white pawn" pawn_white 'P';
    piece_id_test "testing get id on a black pawn" pawn_black 'P';
    piece_id_test "testing get id on a white queen" queen_white 'Q';
    piece_id_test "testing get id on a black queen" queen_black 'Q';
    piece_id_test "testing get id on a white king" king_white 'K';
    piece_id_test "testing get id on a black king" king_black 'K';
    piece_id_test "testing get id on a white rook" rook_white 'R';
    piece_id_test "testing get id on a black rook" rook_black 'R';
    piece_id_test "testing get id on a white bishop" bishop_white 'B';
    piece_id_test "testing get id on a black bishop" bishop_black 'B';
    piece_id_test "testing get id on a white knight" knight_white 'N';
    piece_id_test "testing get id on a black knight" knight_black 'N';
    piece_name_test "testing get name on a white pawn" pawn_white "Pawn";
    piece_name_test "testing get name on a black pawn" pawn_black "Pawn";
    piece_name_test "testing get name on a white queen" queen_white "Queen";
    piece_name_test "testing get name on a black queen" queen_black "Queen";
    piece_name_test "testing get name on a white king" king_white "King";
    piece_name_test "testing get name on a black king" king_black "King";
    piece_name_test "testing get name on a white rook" rook_white "Rook";
    piece_name_test "testing get name on a black rook" rook_black "Rook";
    piece_name_test "testing get name on a white bishop" bishop_white "Bishop";
    piece_name_test "testing get name on a black bishop" bishop_black "Bishop";
    piece_name_test "testing get name on a white knight" knight_white "Knight";
    piece_name_test "testing get name on a black knight" knight_black "Knight";
    piece_points_test "testing get points on a white pawn" pawn_white 1;
    piece_points_test "testing get points on a black pawn" pawn_black 1;
    piece_points_test "testing get points on a white queen" queen_white 9;
    piece_points_test "testing get points on a black queen" queen_black 9;
    piece_points_test "testing get points on a white king" king_white 100;
    piece_points_test "testing get points on a black king" king_black 100;
    piece_points_test "testing get points on a white rook" rook_white 3;
    piece_points_test "testing get points on a black rook" rook_black 3;
    piece_points_test "testing get points on a white bishop" bishop_white 3;
    piece_points_test "testing get points on a black bishop" bishop_black 3;
    piece_points_test "testing get points on a white knight" knight_white 5;
    piece_points_test "testing get points on a black knight" knight_black 5;
    piece_color_test "testing get color on a  upgraded white pawn into queen"
      upgrade_pawn_white 'W';
    piece_color_test "testing get color on a  upgraded black pawn into queen"
      upgrade_pawn_black 'B';
    piece_id_test "testing get id on a upgraded pawn to queen"
      upgrade_pawn_black 'Q';
    piece_name_test "testing get name on a upgraded pawn to queen"
      upgrade_pawn_white "Queen";
    piece_points_test "testing get points on a upgraded pawn to queen"
      queen_white 9;
  ]

let instruction_tests =
  [
    (* Instructions*)
    to_instruction_test "lc instruction" "instructions" InstructionList;
    to_instruction_test "uc Instruction" "Instructions" InstructionList;
    to_instruction_test "spaced instruction" "  instructions " InstructionList;
    (* move instructions. *)
    to_instruction_test "move a1 a2" "move a1 a2" (Move (0, 0, 0, 1));
    to_instruction_test "move a2 b3" "move a2 b3" (Move (0, 1, 1, 2));
    to_instruction_test "move b2 c3" "move b2 c3" (Move (1, 1, 2, 2));
    to_instruction_test "move c3 d4" "move c3 d4" (Move (2, 2, 3, 3));
    to_instruction_test "move d4 e5" "move d4 e5" (Move (3, 3, 4, 4));
    to_instruction_test "move e5 f6" "move e5 f6" (Move (4, 4, 5, 5));
    to_instruction_test "move f6 g7" "move f6 g7" (Move (5, 5, 6, 6));
    to_instruction_test "move g7 h8" "move g7 h8" (Move (6, 6, 7, 7));
    to_instruction_test "spaced move" " move  f1  h3 " (Move (5, 0, 7, 2));
    (* Try Corners *)
    to_instruction_test "move a1 a8" "move a1 a8" (Move (0, 0, 0, 7));
    to_instruction_test "move a8 h8" "move a8 h8" (Move (0, 7, 7, 7));
    to_instruction_test "move h8 h1" "move h8 h1" (Move (7, 7, 7, 0));
    to_instruction_test "move h1 a1" "move h1 a1" (Move (7, 0, 0, 0));
    (* Castle *)
    to_instruction_test "castle e1 h1" "castle e1 h1" (Castle (4, 0, 7, 0));
    to_instruction_test "castle e1 a1" "castle e1 a1" (Castle (4, 0, 0, 0));
    to_instruction_test "castle e8 h8" "castle e8 h8" (Castle (4, 7, 7, 7));
    to_instruction_test "castle e8 a8" "castle e8 a8" (Castle (4, 7, 0, 7));
    to_instruction_test "spaced castle" " castle d3 g4 " (Castle (3, 2, 6, 3));
    (* Quit *)
    to_instruction_test "regular quit" "quit" Quit;
    to_instruction_test "spaced quit" "  quit  " Quit;
    (* Exceptions *)
    ( "instruction: Empty exception" >:: fun _ ->
      assert_raises BadInstruction (fun () -> to_instruction "") );
    ( "instruction: uppercase Move" >:: fun _ ->
      assert_raises BadInstruction (fun () -> to_instruction "Move a3 b4") );
    ( "instruction: uppercase Castle" >:: fun _ ->
      assert_raises BadInstruction (fun () -> to_instruction "Castle a3 b4") );
    ( "instruction: misspelled Move" >:: fun _ ->
      assert_raises BadInstruction (fun () -> to_instruction "mvoe a3 b4") );
    ( "instruction: move with no space 1" >:: fun _ ->
      assert_raises BadInstruction (fun () -> to_instruction "move a3b4") );
    ( "instruction: move with no space 1" >:: fun _ ->
      assert_raises BadInstruction (fun () -> to_instruction "movea3 b4") );
    ( "instruction: move off board 1" >:: fun _ ->
      assert_raises BadInstruction (fun () -> to_instruction "move a0 b3") );
    ( "instruction: move off board 2" >:: fun _ ->
      assert_raises BadInstruction (fun () -> to_instruction "move a1 b9") );
    ( "instruction: move off board 2" >:: fun _ ->
      assert_raises BadInstruction (fun () -> to_instruction "move a1 i3") );
    ( "instruction: move with uppercase" >:: fun _ ->
      assert_raises BadInstruction (fun () -> to_instruction "move A1 B3") );
    ( "instruction: castle with uppercase" >:: fun _ ->
      assert_raises BadInstruction (fun () -> to_instruction "castle A1 B3") );
  ]

let chess_board = init_board

(* chess_board2 is a board in mid game, so the other pieces are exposed *)
(* -------------------------------------------------*)
(*8 | R | N | B | Q | K | B | - | - |*)
(* -------------------------------------------------*)
(*7 | - | P | P | P | P | P | P | - |*)
(* -------------------------------------------------*)
(*6 | - | - | - | - | - | N | - | R |*)
(* -------------------------------------------------*)
(*5 | P | - | - | - | - | - | - | P |*)
(* -------------------------------------------------*)
(*4 | Q | - | P | - | P | - | - | - |*)
(*-------------------------------------------------*)
(*3 | - | - | - | B | - | - | - | - |*)
(* -------------------------------------------------*)
(*2 | P | P | - | P | - | P | P | P |*)
(*-------------------------------------------------*)
(*1 | R | N | B | - | K | - | N | R |*)
(*-------------------------------------------------*)
(* A B C D E F G H*)

let chess_board2 = init_test_board

let illegal_moves_tests =
  (* col1 row1 col2 row2 *)
  [
    illegal_piece_move_test "Test: move pawn forward too far" 1 1 1 4
      chess_board BadPawn;
    illegal_piece_move_test "Test: move pawn to right" 1 1 2 1 chess_board
      SameColor;
    illegal_piece_move_test "Test: move pawn to left" 1 1 0 1 chess_board
      SameColor;
    illegal_piece_move_test "Test: move pawn backward" 1 1 1 0 chess_board
      SameColor;
    illegal_piece_move_test "Test: move rook diagonal right" 0 0 2 2 chess_board
      BadRook;
    illegal_piece_move_test "move rook diagonal left" 7 0 5 2 chess_board
      BadRook;
    illegal_piece_move_test "move rook diagonal left far" 7 0 5 5 chess_board
      BadRook;
    illegal_piece_move_test "Test: move rook diagonal right" 0 0 2 2 chess_board
      BadRook;
    illegal_piece_move_test "Test: move rook diagonal right far" 0 0 3 3
      chess_board BadRook;
    illegal_piece_move_test "Test: move bishop straight" 2 0 2 2 chess_board
      BadBishop;
    illegal_piece_move_test "Test: move bishop off center" 2 0 3 2 chess_board
      BadBishop;
    illegal_piece_move_test "Test: move queen off center right" 3 0 4 2
      chess_board BadQueen;
    illegal_piece_move_test "Test: move queen off center left" 3 0 2 2
      chess_board BadQueen;
    illegal_piece_move_test "Test: move queen through piece" 3 0 3 7 chess_board
      BadQueen;
    illegal_piece_move_test "Test: move king 2 forward" 4 0 4 2 chess_board
      BadKing;
    illegal_piece_move_test "Test: move king 2 diagonal left" 4 0 2 2
      chess_board BadKing;
    illegal_piece_move_test "Test: move king 2 diagonal right" 4 0 6 2
      chess_board BadKing;
    illegal_piece_move_test "Test: move knight 2 forward" 1 0 1 2 chess_board
      BadKnight;
    illegal_piece_move_test "Test: move knight 3 forward" 1 0 1 3 chess_board
      BadKnight;
    illegal_piece_move_test "Test: move knight diagonal left" 1 0 0 1
      chess_board SameColor;
    illegal_piece_move_test "Test: move knight diagonal right" 1 0 3 2
      chess_board BadKnight;
    illegal_piece_move_test "Test: move Queen through pawn" 0 3 0 5 chess_board2
      BadQueen;
    illegal_piece_move_test "Test: move Queen through own piece" 0 3 3 3
      chess_board2 BadQueen;
    illegal_piece_move_test "Test: move Queen through pawn" 0 3 0 5 chess_board2
      BadQueen;
    illegal_piece_move_test "Test: move Queen through piece diagonal" 0 3 4 7
      chess_board2 BadQueen;
    illegal_piece_move_test "Test: move Queen in illegal L" 0 3 1 5 chess_board2
      BadQueen;
    illegal_piece_move_test "Test: move pawn too far forward" 2 3 2 5
      chess_board2 BadPawn;
    illegal_piece_move_test "Test: move pawn diagonal no capture right" 2 3 3 4
      chess_board2 BadPawn;
    illegal_piece_move_test "Test: move pawn diagonal no capture left" 2 3 1 4
      chess_board2 BadPawn;
    illegal_piece_move_test "Test: illegal move pawn right" 2 3 3 3 chess_board2
      BadPawn;
    illegal_piece_move_test "Test: illegal move pawn left" 2 3 1 3 chess_board2
      BadPawn;
    illegal_piece_move_test "Test: trying to move a piece of the wrong color" 2
      3 4 4
      (make_test_board king_stress_board)
      WrongPiece;
    illegal_piece_move_test "Test: trying to move a piece of the wrong color" 6
      2 6 5
      (make_test_board king_stress_board)
      WrongPiece;
    illegal_piece_move_test "Test: move into check" 5 4 4 4
      (make_test_board king_stress_board)
      IntoCheck;
    illegal_piece_move_test "Test: move into check" 5 4 6 4
      (make_test_board king_stress_board)
      IntoCheck;
    illegal_piece_move_test "Test: trying to move a piece to the same location"
      1 1 1 1 chess_board NoMove;
    illegal_piece_move_test "Test: trying to move empty space" 4 4 3 3
      chess_board NoPiece;
    illegal_piece_move_test "Test: move to win the game" 7 6 5 4
      (make_test_board king_stress_board2)
      (Win "Blue");
  ]

let suite =
  "test suite for Chess"
  >::: List.flatten [ piece_tests; instruction_tests; illegal_moves_tests ]

let _ = run_test_tt_main suite

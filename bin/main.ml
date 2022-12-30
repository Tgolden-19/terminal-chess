open Game
open Board
open Piece
open Instruction

let red = "\027[31m"
let blue = "\027[34m"
let none = "\027[0m"

let print_instructions unit =
  print_endline "Here is a list of valid commands:";
  print_endline
    "(1) Move. Ex. Type \"move a2 a3\" to move pawn at position a2 to a3";
  print_endline
    "(2) Castle. Ex. Type \"castle e1 h1\" to castle the king in spot e1 with \
     the rook in spot h1"

let switch player = if !player = 1 then player := 2 else player := 1

let draw_board board =
  for x = 8 downto 1 do
    print_endline "   -------------------------------------------------";
    print_string (" " ^ string_of_int x ^ " ");
    for y = 8 downto 1 do
      if get_piece_color (8 - y) (x - 1) board = 'W' then
        print_string
          ("|  " ^ blue
          ^ String.make 1 (get_piece_id_from_square (8 - y) (x - 1) board)
          ^ none ^ "  ")
      else
        print_string
          ("|  " ^ red
          ^ String.make 1 (get_piece_id_from_square (8 - y) (x - 1) board)
          ^ none ^ "  ")
    done;
    print_endline "|"
  done;
  print_endline "   -------------------------------------------------";
  print_endline "      A     B     C     D     E     F     G     H"

let rec play_game board =
  draw_board board;
  check_warn board;
  print_endline "\nType \"Instructions\" for list of valid commands";
  print_string ("\nMove Player " ^ string_of_int !player ^ " > ");
  match read_line () with
  | exception End_of_file -> ()
  | instruction -> (
      try
        match to_instruction instruction with
        | Move (c1, r1, c2, r2) ->
            try_move c1 r1 c2 r2 board;
            switch player;
            play_game board
        | Castle (c1, r1, c2, r2) ->
            try_castle c1 r1 c2 r2 board;
            switch player;
            play_game board
        | InstructionList ->
            print_instructions ();
            play_game board
        | Quit ->
            print_endline "Thanks for playing";
            exit 0
      with
      | BadInstruction ->
          print_endline "Bad instruction! Try again...";
          play_game board
      | WrongPiece ->
          print_endline "That is not your piece to move! Try again...";
          play_game board
      | NoMove ->
          print_endline "You didn't even move the piece! Try again...";
          play_game board
      | SameColor ->
          print_endline "That is your own piece! Try again...";
          play_game board
      | NoPiece ->
          print_endline "There is no piece there! Try again...";
          play_game board
      | BadPawn ->
          print_endline "That is not a valid Pawn move! Try again...";
          play_game board
      | BadRook ->
          print_endline "That is not a valid Rook move! Try again...";
          play_game board
      | BadBishop ->
          print_endline "That is not a valid Bishop move! Try again...";
          play_game board
      | BadQueen ->
          print_endline "That is not a valid Queen move! Try again...";
          play_game board
      | BadKnight ->
          print_endline "That is not a valid Knight move! Try again...";
          play_game board
      | BadCastle ->
          print_endline "That is a bad castle! Try again...";
          play_game board
      | BadKing ->
          print_endline "That is not a valid King move! Try again...";
          play_game board
      | IntoCheck ->
          print_endline "You can't move your King into check! Try again...";
          play_game board
      | Win s ->
          print_endline (s ^ " has won the game!");
          print_endline "Thanks for playing";
          exit 0)

(* [main ()] is what starts the game. Credit should go to Michael Clarkson here
   since A2 uses a main function which inspires the main function below*)
let main () =
  print_endline
    "\n\
     Welcome to chess. Two players are required. Instructions: type move \n\
     followed by a row then column of a piece you want followed by the row and \n\
     column of the space you want to move to. For example, 'move c2 c4' will \n\
     move the pawn in c2 to c4. type 'quit' to quit. Player 1 = Blue, Player 2 \n\
     = Red.For list of all instructions, type \"Instructions\" during the \
     game.\n\n\
     If you want to win the game you must capture the enemy king. No need to \
     checkmate.";
  print_endline "Are you ready to play???  (1) Yes (2) Yes.\n";
  match read_line () with
  | exception End_of_file -> ()
  | "Yes" ->
      print_endline "\nYay! Have a good time!\n";
      play_game init_board
  | other_string ->
      print_endline "\nToo bad, you are playing anyways\n";
      play_game init_board

(* runs main () to start the game*)
let () = main ()

(** This is the file that parses the instructions specified by the user

    Contributors to this file: Jonas Funk

    Project Contributors: Tristan Golden, Jonas Funk, Luke Griffiths*)

(**[instruction] represents the user specified instruction during Chess. This is
   a self-explanatory action that is followed by four ints that specify the
   coordinates of the first chess piece and the second chess piece, respectively*)
type instruction =
  | Move of (int * int * int * int)
  | Castle of (int * int * int * int)
  | InstructionList
  | Quit

exception BadInstruction
(**Raised when there is an unknown/invalid instruction*)

val to_instruction : string -> instruction
(** [to_instruction str] takes a string [str] and returns a valid instruction of
    type [instruction].

    Raises: [BadInstruction] if the instruction is invalid *)

(* Contributors to this file: Jonas Funk

   Project Contributors: Tristan Golden, Jonas Funk, Luke Griffiths*)

type instruction =
  | Move of (int * int * int * int)
  | Castle of (int * int * int * int)
  | InstructionList
  | Quit

exception BadInstruction

let rec assign_accumulator num (a, b, c, d) idx =
  if idx = 0 then (num, b, c, d)
  else if idx = 1 then (a, num, c, d)
  else if idx = 2 then (a, b, num, d)
  else (a, b, c, num)

let rec coordinates_helper str (a, b, c, d) idx =
  match str.[idx] with
  | '1' ->
      if idx = 3 then assign_accumulator 0 (a, b, c, d) idx
      else
        coordinates_helper str (assign_accumulator 0 (a, b, c, d) idx) (idx + 1)
  | '2' ->
      if idx = 3 then assign_accumulator 1 (a, b, c, d) idx
      else
        coordinates_helper str (assign_accumulator 1 (a, b, c, d) idx) (idx + 1)
  | '3' ->
      if idx = 3 then assign_accumulator 2 (a, b, c, d) idx
      else
        coordinates_helper str (assign_accumulator 2 (a, b, c, d) idx) (idx + 1)
  | '4' ->
      if idx = 3 then assign_accumulator 3 (a, b, c, d) idx
      else
        coordinates_helper str (assign_accumulator 3 (a, b, c, d) idx) (idx + 1)
  | '5' ->
      if idx = 3 then assign_accumulator 4 (a, b, c, d) idx
      else
        coordinates_helper str (assign_accumulator 4 (a, b, c, d) idx) (idx + 1)
  | '6' ->
      if idx = 3 then assign_accumulator 5 (a, b, c, d) idx
      else
        coordinates_helper str (assign_accumulator 5 (a, b, c, d) idx) (idx + 1)
  | '7' ->
      if idx = 3 then assign_accumulator 6 (a, b, c, d) idx
      else
        coordinates_helper str (assign_accumulator 6 (a, b, c, d) idx) (idx + 1)
  | '8' ->
      if idx = 3 then assign_accumulator 7 (a, b, c, d) idx
      else
        coordinates_helper str (assign_accumulator 7 (a, b, c, d) idx) (idx + 1)
  | 'a' ->
      if idx = 3 then assign_accumulator 0 (a, b, c, d) idx
      else
        coordinates_helper str (assign_accumulator 0 (a, b, c, d) idx) (idx + 1)
  | 'b' ->
      if idx = 3 then assign_accumulator 1 (a, b, c, d) idx
      else
        coordinates_helper str (assign_accumulator 1 (a, b, c, d) idx) (idx + 1)
  | 'c' ->
      if idx = 3 then assign_accumulator 2 (a, b, c, d) idx
      else
        coordinates_helper str (assign_accumulator 2 (a, b, c, d) idx) (idx + 1)
  | 'd' ->
      if idx = 3 then assign_accumulator 3 (a, b, c, d) idx
      else
        coordinates_helper str (assign_accumulator 3 (a, b, c, d) idx) (idx + 1)
  | 'e' ->
      if idx = 3 then assign_accumulator 4 (a, b, c, d) idx
      else
        coordinates_helper str (assign_accumulator 4 (a, b, c, d) idx) (idx + 1)
  | 'f' ->
      if idx = 3 then assign_accumulator 5 (a, b, c, d) idx
      else
        coordinates_helper str (assign_accumulator 5 (a, b, c, d) idx) (idx + 1)
  | 'g' ->
      if idx = 3 then assign_accumulator 6 (a, b, c, d) idx
      else
        coordinates_helper str (assign_accumulator 6 (a, b, c, d) idx) (idx + 1)
  | 'h' ->
      if idx = 3 then assign_accumulator 7 (a, b, c, d) idx
      else
        coordinates_helper str (assign_accumulator 7 (a, b, c, d) idx) (idx + 1)
  | _ -> raise BadInstruction

let convert_to_coordinates lst =
  if String.length (String.concat "" lst) = 4 then
    coordinates_helper (String.concat "" lst) (0, 0, 0, 0) 0
  else raise BadInstruction

(* Note that this implementation of to_instruction uses some of Jonas' code from
   Michael Clarkson's A2 assignment*)
let rec find_instruction_list acc = function
  (* We want two locations. One for the original place for the piece and one for
     where we want the piece to go to*)
  | [] ->
      if List.length acc = 2 then convert_to_coordinates (List.rev acc)
      else raise BadInstruction
  | h :: t ->
      if h <> "" then find_instruction_list (h :: acc) t
      else find_instruction_list acc t

let rec find_instruction = function
  | [] -> raise BadInstruction
  | h :: t ->
      if h = "" then find_instruction t
      else if h = "move" then
        if find_instruction_list [] t = (0, 0, 0, 0) then raise BadInstruction
        else Move (find_instruction_list [] t)
      else if h = "castle" then
        if find_instruction_list [] t = (0, 0, 0, 0) then raise BadInstruction
        else Castle (find_instruction_list [] t)
      else if h = "Instructions" || h = "instructions" then InstructionList
      else if h = "quit" then Quit
      else raise BadInstruction

let to_instruction str = find_instruction (String.split_on_char ' ' str)
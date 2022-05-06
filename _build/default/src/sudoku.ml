open Ast

type t = int list

type env = int list
let size = 9
let root = int_of_float (sqrt (float_of_int size))

type 'a printer = Format.formatter -> 'a -> unit

let pp : 'a printer = fun fmt sudoku ->
  for i = 0 to (size -1) do
    if i mod root = 0 && i <> 0 then
      Format.fprintf fmt "@.";
    for j = 0 to (size -1) do
      if j mod root = 0 && j <> 0 then
        Format.fprintf fmt " ";
      Format.fprintf fmt "%d " (List.nth sudoku (i*size + j))
    done;
    Format.fprintf fmt "@.";
  done

let clause_ligne i k = 
  let clause = ref Clause.empty in
  for j=0 to 8 do 
    clause := Clause.add (i*81 + j*9 + k) !clause 
  done;
  !clause

let clause_colonne j k =
  let clause = ref Clause.empty in
  for i=0 to 8 do 
    clause := Clause.add (i*81 + j*9 + k) !clause 
  done;
  !clause

let clause_carre i j k =
  let clause = ref Clause.empty in
  for ind_i = 0 to 2 do
    for ind_j = 0 to 2 do
      clause := Clause.add ((i+ind_i)*81 + (j+ind_j)*9 + k) !clause
    done
  done;
  !clause

let to_cnf : t -> env * Ast.t = fun sudoku -> failwith "todo: to_cnf"

let solution_of : env -> Ast.model -> t = fun env model -> failwith "todo: solution_of"

let grid_of_str : string -> t = fun str ->
  let l = ref [] in
  String.iter (fun c -> l := (int_of_char c - (int_of_char '0'))::!l) str;
  List.rev !l

let read : string -> t * t = fun str ->
  match String.split_on_char ',' str with
  | [left;right] -> grid_of_str left, grid_of_str right
  | _ -> assert false

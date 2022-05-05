type t = int list

type env (* TO FILL *)
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

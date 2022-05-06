open Ast
open Format

type t = int list

type env = unit
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
  let l = ref [] in
  for j=0 to 8 do 
    l := (i*81 + j*9 + k)::!l 
  done;
  Clause.of_list !l

let clause_colonne j k =
  let l = ref [] in
  for i=0 to 8 do 
    l := (i*81 + j*9 + k)::!l 
  done;
  Clause.of_list !l

let clause_carre i j k =
  let l = ref [] in
  for ind_i = 0 to 2 do
    for ind_j = 0 to 2 do
      l := ((i+ind_i)*81 + (j+ind_j)*9 + k)::!l
    done
  done;
  Clause.of_list !l

let unit_var =
  let l = ref [] in
  for i = 0 to 8 do
    for j = 0 to 8 do
      for i_k = 1 to 9 do
        for j_k = i_k+1 to 9 do
          let clause = Clause.of_list ([-(81*i+9*j+i_k);-(81*i+9*j+j_k)])
          in l := clause::!l
        done
      done;
    done;
  done;
  !l

let cnf_sudoku = 
  let l = ref [] in
  for k = 1 to 9 do
    for i = 0 to 8 do
      l := (clause_ligne i k)::!l; l:= (clause_colonne i k)::!l;
      for j = 0 to 8 do
        l := (clause_carre i j k)::!l
      done;
    done;
  done;
  let cnf = Cnf.of_list (unit_var@(!l)) in
  {nb_var = 729; nb_clause = Cnf.cardinal cnf; cnf}
  
(* let _ = Ast.pp_cnf Format.std_formatter cnf_sudoku.cnf *)

let rec remove_lvar_clause l c = match l with
  | [] -> c
  | h::q -> let c1 = (Cnf.map (Clause.remove (-h)) c) in
            remove_lvar_clause q (Cnf.filter (fun elt -> not(Clause.mem h elt)) c1)

let to_cnf : t -> env * Ast.t = fun sudoku -> 
  let rec aux ind l1 l2 = match l1 with
    | [] -> l2
    | h::q -> let l = ref l2 in 
              for k=1 to 9 do
                if k = h then l := (ind+k)::!l else l := -(ind+k)::!l
              done;
              aux (ind+1) q !l
  in let l = aux 0 sudoku [] in
  let cnf = remove_lvar_clause l cnf_sudoku.cnf in
  ((),{nb_var = 729 - List.length l; nb_clause = Cnf.cardinal cnf; cnf})

let solution_of : env -> Ast.model -> t = fun env model -> 
  let tab = Array.make 81 0 in
  let rec aux = function
    | [] -> ()
    | h::q when h > 0 -> let chiffre = h mod 9 and case = h / 9 in 
                          if chiffre = 0 then tab.(case-1) <- 9
                          else tab.(case) <- chiffre;
                          aux q
    | _::q -> aux q
  in aux model; Array.to_list tab

let grid_of_str : string -> t = fun str ->
  let l = ref [] in
  String.iter (fun c -> l := (int_of_char c - (int_of_char '0'))::!l) str;
  List.rev !l

let read : string -> t * t = fun str ->
  match String.split_on_char ',' str with
  | [left;right] -> grid_of_str left, grid_of_str right
  | _ -> assert false

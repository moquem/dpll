open Ast
open Format

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
    done;
    for i = 0 to 2 do
      for j = 0 to 2 do
        l := (clause_carre (i*3) (j*3) k)::!l
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

let rec print_list = function
  | [] -> ()
  | h::q -> print_int h; print_string "|"; print_list q

let recup_pos x = 
  let i = x/81 in
  let j = (x-i*81)/9 in
  let k = x mod 9 in
  if k = 0 then 
    if j = 0 then (i-1,8,9)
    else (i,j-1,9)
  else (i,j,k)

let indice (i,j,k) =
  i*81+j*9+k

let display_grid t = 
  for i=0 to 8 do
    for j=0 to 8 do
      print_int t.(i*9+j); print_string " "
    done;
  print_string "\n";
  done


let to_cnf : t -> env * Ast.t = fun sudoku -> 
  let rec aux ind l1 s1 = match l1 with
    | [] -> s1
    | h::q -> let s = ref s1 in 
              let i_ind,j_ind,_ = recup_pos (ind*9+1) in
              if h > 0 then
                ( for k=1 to 9 do
                    if k = h then s := Clause.add (ind*9+k) !s
                    else s := Clause.add (-(ind*9+k)) !s
                  done;
                  for i=0 to 8 do
                    if i <> i_ind then s := Clause.add (-(indice (i,j_ind,h))) !s;              
                    if i <> j_ind then s := Clause.add (-(indice (i_ind,i,h))) !s;
                  done;
                  let deb_i = (i_ind/3)*3 and deb_j = (j_ind/3)*3 in
                    for i=0 to 2 do
                      for j=0 to 2 do
                        if deb_i+i <> i_ind && deb_j+j <> j_ind then s:= Clause.add (-(indice (deb_i+i,deb_j+j,h))) !s
                      done;
                    done;
                );
              aux (ind+1) q !s
  in 
  let l = Clause.elements (aux 0 sudoku Clause.empty) in
  let cnf = remove_lvar_clause l cnf_sudoku.cnf in
  print_list l;
  let total_cnf = {nb_var = 729 - List.length l; nb_clause = Cnf.cardinal cnf; cnf} in
  Ast.pp Format.std_formatter total_cnf; (l,total_cnf)

let solution_of : env -> Ast.model -> t = fun env model -> 
  let tab = Array.make 81 0 in
  let rec aux = function
    | [] -> ()
    | h::q when h > 0 -> let chiffre = h mod 9 and case = h / 9 in 
                          if chiffre = 0 then tab.(case-1) <- 9
                          else tab.(case) <- chiffre;
                          aux q
    | _::q -> aux q
  in 
  let l = env@model in
  aux l;
  display_grid tab;
  Array.to_list tab

let grid_of_str : string -> t = fun str ->
  let l = ref [] in
  String.iter (fun c -> l := (int_of_char c - (int_of_char '0'))::!l) str;
  List.rev !l

let read : string -> t * t = fun str ->
  match String.split_on_char ',' str with
  | [left;right] -> grid_of_str left, grid_of_str right
  | _ -> assert false

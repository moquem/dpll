(* J'ai laissé en commentaire toutes les lignes d'affichage qui permettaient de débugguer. *)

open Ast
open Format

type t = int list

type env = int list
let size = 4
let root = int_of_float (sqrt (float_of_int size))

type 'a printer = Format.formatter -> 'a -> unit

let pp : 'a printer = fun fmt unequal ->
  for i = 0 to (size -1) do
    if i mod root = 0 && i <> 0 then
      Format.fprintf fmt "@.";
    for j = 0 to (size -1) do
      if j mod root = 0 && j <> 0 then
        Format.fprintf fmt " ";
      Format.fprintf fmt "%d " (List.nth unequal (i*size + j))
    done;
    Format.fprintf fmt "@.";
  done

(* Fonctions pour créer les clauses correspondant à l'apparition d'au moins un chiffre de chaque pour chaque ligne, colonne*)

let clause_ligne i k = 
  let l = ref [] in
  for j=0 to 3 do 
    l := (i*16 + j*4 + k)::!l 
  done;
  Clause.of_list !l

let clause_colonne j k =
  let l = ref [] in
  for i=0 to 3 do 
    l := (i*16 + j*4 + k)::!l 
  done;
  Clause.of_list !l

(* Clauses correspondantes à l'unicité du chiffre pour chaque case *)

let unit_var =
  let l = ref [] in
  for i = 0 to 3 do
    for j = 0 to 3 do
      for i_k = 1 to 4 do
        for j_k = i_k+1 to 4 do
          let clause = Clause.of_list ([-(16*i+4*j+i_k);-(16*i+4*j+j_k)])
          in l := clause::!l
        done
      done;
    done;
  done;
  !l

(* Modèle CNF d'une grille de unequal *)

let cnf_unequal = 
  let l = ref [] in
  for k = 1 to 4 do
    for i = 0 to 3 do
      l := (clause_ligne i k)::!l; l:= (clause_colonne i k)::!l;
    done;
  done;
  let cnf = Cnf.of_list (unit_var@(!l)) in
  {nb_var = 64; nb_clause = Cnf.cardinal cnf; cnf}


(* Permet de nettoyer la CNF quand on connaît l'assignation d'une variable *)
let rec remove_lvar_clause l c = match l with
  | [] -> c
  | h::q -> let c1 = (Cnf.map (Clause.remove (-h)) c) in
            remove_lvar_clause q (Cnf.filter (fun elt -> not(Clause.mem h elt)) c1)

(* Pour l'affichage des assignations *)
let rec print_list = function
  | [] -> print_string "\n";
  | h::q -> print_int h; print_string "|"; print_list q
  


(* Bijection pour identifier les variables (i,j,k) (ligne,colonne,chiffre) par un entier naturel *)
let recup_pos x = 
  let i = x/16 in
  let j = (x-i*16)/4 in
  let k = x mod 4 in
  if k = 0 then 
    if j = 0 then (i-1,3,4)
    else (i,j-1,4)
  else (i,j,k)

let indice (i,j,k) =
  i*16+j*4+k

let inegal_ligne x s =
  let i = x / 3 and j = x mod 3 and l = ref [] in
  if s = 1 then 
    for k_j = 2 to 4 do
      for k_i = 1 to k_j do
        l := (Clause.of_list [-indice(i,j,k_j);-indice(i,j+1,k_i)])::!l
      done;
    done
  else (
    if s = 2 then
    for k_j = 2 to 4 do
      for k_i = 1 to k_j do
        l := (Clause.of_list [-indice(i,j,k_i);-indice(i,j+1,k_j)])::!l
      done;
    done;
  ); Cnf.of_list !l

let inegal_colonne x s =
  let i = x / 4 and j = x mod 4 and l = ref [] in
  if s = 1 then 
    for k_j = 2 to 4 do
      for k_i = 1 to k_j do
        l := (Clause.of_list [-indice(i,j,k_j);-indice(i+1,j,k_i)])::!l
      done;
    done
  else (
    if s = 2 then
    for k_j = 2 to 4 do
      for k_i = 1 to k_j do
        l := (Clause.of_list [-indice(i,j,k_i);-indice(i+1,j,k_j)])::!l
      done;
    done;
  ); Cnf.of_list !l


(* Affichage du problème *)
let display_grid t = 
  print_string "\n";
  for i=0 to 3 do
    for j=0 to 3 do
      print_int t.(i*4+j); print_string " "
    done;
  print_string "\n";
  done

(* Découper une liste *)
let rec split_list l ind = match l with
  | [] -> [],[]
  | _ when ind = 0 -> [],l
  | h::q -> let l1,l2 = split_list q (ind-1) in h::l1,l2


let to_cnf : t -> env * Ast.t = fun unequal -> 
  print_list unequal; 
  let rec assign ind l1 s1 = match l1 with (* On renvoie un set comme ça on élimine directement les doublons *)
    | [] -> s1
    | h::q -> let s = ref s1 in 
              let i_ind,j_ind,_ = recup_pos (ind*4+1) in
              if h > 0 then
                ( for k=1 to 4 do  (* On sait que cette case ne pourra contenir aucun autre chiffre, donc on value les variables correspondantes négativement *)
                    if k = h then s := Clause.add (ind*4+k) !s
                    else s := Clause.add (-(ind*4+k)) !s
                  done;
                  for i=0 to 3 do (* On sait qu'aucune autre case de la ligne ou colonne pourra contenir ce même chiffre, on value les variables correspondantes *)
                    if i <> i_ind then s := Clause.add (-(indice (i,j_ind,h))) !s;              
                    if i <> j_ind then s := Clause.add (-(indice (i_ind,i,h))) !s;
                  done
                );
              assign (ind+1) q !s

  and new_clause_l ind l s = match l with
    | [] -> s
    | h::q -> if h <> 0 then new_clause_l (ind+1) q (Cnf.union s (inegal_ligne ind h)) else new_clause_l (ind+1) q s
  
  and new_clause_c ind l s = match l with
    | [] -> s
    | h::q -> if h <> 0 then new_clause_c (ind+1) q (Cnf.union s (inegal_colonne ind h)) else new_clause_c (ind+1) q s
 
  in 

  let l1,l2 = split_list unequal 16 in
  let var = Clause.elements (assign 0 l1 Clause.empty) in
  let ineq_ligne,ineq_colonne = split_list l2 12 in
  let cnf = Cnf.union (new_clause_l 0 ineq_ligne Cnf.empty) (new_clause_c 0 ineq_colonne Cnf.empty) in
  let cnf = Cnf.union cnf cnf_unequal.cnf in
  (* Nettoyage de la CNF *)
  let cnf = remove_lvar_clause var cnf in
  let total_cnf = {nb_var = 64 - List.length var; nb_clause = Cnf.cardinal cnf; cnf} in
  (* Ast.pp Format.std_formatter total_cnf; *) (var,total_cnf)

let solution_of : env -> Ast.model -> t = fun env model -> 
  (* print_list model; *)

  (* Représente les 16 cases du unequal, dans chaque case on indique le numéro de remplissage *)
  let tab = Array.make 16 0 in
  let rec aux = function
    | [] -> ()
    | h::q when h > 0 -> let chiffre = h mod 4 and case = h / 4 in 
                          if chiffre = 0 then tab.(case-1) <- 4
                          else tab.(case) <- chiffre;
                          aux q
    | _::q -> aux q
  in 
  let l = env@model in
  aux l;
  (* display_grid tab; *)
  let l = Array.to_list tab in 
  (l@[-35])

let grid_of_str : string -> t = fun str ->
  let l = ref [] in
  String.iter (fun c -> l := (int_of_char c - (int_of_char '0'))::!l) str;
  List.rev !l

let read : string -> t * t = fun str ->
  match String.split_on_char ',' str with
  | [left;right] -> grid_of_str left, grid_of_str right
  | _ -> assert false

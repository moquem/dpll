open Ast

module type CHOICE = sig
  val choice : Ast.Cnf.t -> Ast.var
end

module DefaultChoice =
struct
  let choice : Ast.Cnf.t -> Ast.var = fun cnf -> failwith "todo: choice"
end

module type SOLVER = sig
  val solve : Ast.t -> Ast.model option
end

module DPLL(C:CHOICE) : SOLVER =
struct
let rec solve : Ast.t -> Ast.model option = fun p -> 
    (* print_string "new_cnf \n"; 
    Ast.pp Format.std_formatter p; *)
    if Memois.mem p.cnf !memoisation then None
    else 
    if Cnf.is_empty p.cnf then Some []
    else 
    ( let l_sgl = ref [] and cnf = ref p.cnf
      in

      (* Balaie la CNF pour enlever tous les singletons *)
      while Cnf.exists (fun x -> (Clause.cardinal x) = 1) !cnf do
      let l = List.map (fun elt -> Clause.choose elt) (Cnf.elements (Cnf.filter (fun elt -> (Clause.cardinal elt) = 1) !cnf)) in 
      l_sgl := l@(!l_sgl); cnf := remove_lvar_clause l !cnf
      done;

      let cnf3 = Cnf.filter (fun elt -> not(Clause.is_empty elt)) !cnf in

      (* Vérifie si lors de l'assignation des singletons, il n'y a pas eu de conflits, à savoir assignation impossible.
      Si c'est le cas, alors on en profite pour stocker cette CNF comme insatisfiable. *)
      if Cnf.cardinal !cnf <> Cnf.cardinal cnf3 then (memoisation := Memois.add p.cnf !memoisation; None)

      else 
      match Cnf.choose_opt cnf3 with
        | None -> Some !l_sgl (* La CNF est vide, donc satisfiable *)
        | Some elt -> let new_var = Clause.choose elt in 
                      begin
                        let cnf4 = remove_var_clause cnf3 new_var in
                        let new_cnf = {nb_var = (p.nb_var - (List.length !l_sgl)) - 1; nb_clause = Cnf.cardinal cnf4; cnf = cnf4} in
                        match solve new_cnf with
                          | None -> 
                          ( memoisation := Memois.add cnf4 !memoisation;
                            let second_var = -new_var in 
                            let cnf6 = remove_var_clause cnf3 second_var in
                            let second_cnf = solve {nb_var = new_cnf.nb_var; nb_clause = Cnf.cardinal cnf6; cnf = cnf6} in
                            match second_cnf with
                              | None -> memoisation := Memois.add p.cnf !memoisation; None
                              | Some l -> Some (second_var::((!l_sgl)@l))
                          )
                          | Some l -> Some (new_var::(!l_sgl@l))
                      end
    )



  and remove_var_clause c v = 
      let c1 = Cnf.map (Clause.remove (-v)) c in
      Cnf.filter (fun elt -> not(Clause.mem v elt)) c1

  and remove_var_clause_unit c v =
      Cnf.filter (fun elt -> not(Clause.mem v elt)) c
  
  and remove_lvar_clause l c = match l with
      | [] -> c
      | h::q -> let c1 = (Cnf.map (Clause.remove (-h)) c) in
                remove_lvar_clause q (Cnf.filter (fun elt -> not(Clause.mem h elt)) c1)
  
  and recup_var c = 
      Cnf.fold (fun x i -> Clause.union x i) c Clause.empty
  
  and recup_unit_var c1 c2 =
      Clause.elements (Clause.filter (fun elt -> Cnf.for_all (fun x -> Clause.mem elt x || not(Clause.mem (-elt) x)) c2) c1)
  
  and memoisation = ref Memois.empty 
end

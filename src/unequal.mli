type t
type env

(** [read str] reads a string encoding a unequal problem. Returns a unequal problem and its solution. *)
val read : string -> t * t

(** [to_cnf t] encodes a unequal problem and returns an environement as long as a cnf problem *)
val to_cnf : t -> env * Ast.t

(** [solution_of env m] takes an environement [env] and a model [m] and returns the solution of a unequal problem *)
val solution_of : env -> Ast.model -> t

val print_list : t -> unit

type 'a printer = Format.formatter -> 'a -> unit


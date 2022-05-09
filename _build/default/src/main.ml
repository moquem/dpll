type execution_mode =
  | Cnf (** Solve a formula *)
  | Sudoku of string (** Solve a sudoku given as a string *)
  | Unequal of string (** Solve a unequal problem given as a string *)


(** By default we solve a formula given in the dimacs format *)
let mode = ref Cnf

(** Default solver. By doing the third part of the project, you might come with other solvers *)
module S = Dpll.DPLL(Dpll.DefaultChoice)

(** Handle files given on the command line *)
let handle_file : string -> unit = fun fname ->
  let p =  Dimacs.parse_file fname in
  begin
    match S.solve p with
    | None -> Format.printf "false@."
    | Some _ -> Format.printf "true@."
  end

let handle_sudoku : string -> unit = fun str ->
  let sudoku, solution = Sudoku.read str in
  let env, ast = Sudoku.to_cnf sudoku in
  match S.solve ast with
  | None ->
    raise @@ failwith "No answer. Probably your encoding is incorrect"
  | Some model ->
    let candidate = Sudoku.solution_of env model in
    if candidate <> solution then
      raise @@ failwith "You found an incorrect answer."

let handle_unequal : string -> unit = fun str ->
  let unequal, solution = Unequal.read str in
  let env, ast = Unequal.to_cnf unequal in
  match S.solve ast with
  | None ->
    (* Unequal.print_list solution; *)
    raise @@ failwith "No answer. Probably your encoding is incorrect"
  | Some model ->
    let candidate = Unequal.solution_of env model in
    Unequal.print_list candidate;
    Unequal.print_list solution; 
    if candidate <> solution then
      raise @@ failwith "You found an incorrect answer."

(** Specification of the options handle by the program. You are only allowed to ADD new options *)
let spec =
  let debug_flags =
    let fn acc l = acc ^ "\n        " ^ l in
    List.fold_left fn "\n      Available flags:" (Console.log_summary ())
  in
  let spec = Arg.align
      [ ( "--debug"
        , Arg.String (Console.set_debug true)
        , "<flags> Sets the given debugging flags" ^ debug_flags )
      ; ( "--sudoku"
        , Arg.String (fun s -> mode := Sudoku s)
        , " Solve a sudoku given on the command line as a string" )
        
      ; ( "--unequal"
        , Arg.String (fun s -> mode := Unequal s)
        , " Solve a unequal problem given on the command line as a string" )]
  in
  spec

(** Entry point of the program *)
let _ =
  let open Console in
  let usage = Format.sprintf "Usage: %s [CMD] [FILE]" Sys.argv.(0) in
  let files = ref [] in
  Arg.parse spec (fun s -> files := s :: !files) usage;
  try
    begin
      match !mode with
      | Cnf -> List.iter handle_file !files
      | Sudoku str ->  handle_sudoku str
      | Unequal str -> handle_unequal str
    end
  with
  | Fatal(None,    msg) -> exit_with "%s" msg
  | Fatal(Some(p), msg) -> exit_with "[%a] %s" Location.print p msg

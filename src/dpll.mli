open Cnf

(* the return model, either unsat or an valid assignment. *)
type model =
  | Solution of (string * bool) list
  | Unsat

type disjunction = (string * Cnf.sign) list
type conjuction = disjunction list

(* SAT solver using DPLL algorithm. The input should be cnf. *)
val dpll : conjuction -> model

val print_model : model -> unit

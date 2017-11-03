type symbol
type cnf

type sign = Id | Not

exception SymbolAlreadyAssigned

val symbol_to_string : symbol -> string
val symbol_from_string : string -> symbol

val make_cnf : (string * sign) list list -> cnf
val unpack_cnf : cnf -> (string * sign) list list

(* chooses an unassigned symbol from this cnf. *)
val choose_symbol : cnf -> symbol

(* assigns an unassigned symbol. *)
val assign_and_simplify : symbol -> bool -> cnf -> cnf

(* finds a unit clause and propagate it to this cnf. *)
val unit_clause_propagate : cnf -> (symbol * bool * cnf) option

(* finds an occurred and pure literal, and assigns it to this cnf. *)
val pure_literal_assign : cnf -> (symbol * bool * cnf) option

(* returns true if all symbols are assigned and this cnf is consistent.
 * returning false does not imply this cnf is unsatisfiable. *)
val solved : cnf -> bool

(* returns true if this cnf cannot be satisfied.
 * returning false does not imply this cnf is satisfiable. *)
val unsat : cnf -> bool

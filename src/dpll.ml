open Cnf

type model =
  | Solution of (string * bool) list
  | Unsat

type disjunction = (string * sign) list
type conjuction = disjunction list

type assignment = (string * bool) list

let rec dpll_rec (cnf : cnf) (model : assignment) : assignment option =
  if solved cnf then
    Some model
  else if unsat cnf then
    None
  else
    match unit_clause_propagate cnf with
    | Some (sym, value, new_cnf) ->
        dpll_rec new_cnf ((symbol_to_string sym, value) :: model)
    | None ->
        match pure_literal_assign cnf with
        | Some (sym, value, new_cnf) ->
            dpll_rec new_cnf ((symbol_to_string sym, value) :: model)
        | None ->
            let sym = choose_symbol cnf in
            match dpll_rec (assign_and_simplify sym true cnf)
                           ((symbol_to_string sym, true) :: model) with
            | Some result -> Some result
            | None -> dpll_rec (assign_and_simplify sym false cnf)
                               ((symbol_to_string sym, false) :: model)

let dpll (cnf : conjuction) : model =
  match dpll_rec (make_cnf cnf) [] with
  | Some sol -> Solution sol
  | None -> Unsat

let print_model (model : model) : unit =
  match model with
  | Unsat -> Format.printf "unsat\n"
  | Solution model ->
      List.iter (fun (symbol, value) ->
        Format.printf "%s: %s\n" symbol (if value then "true" else "false"))
        model

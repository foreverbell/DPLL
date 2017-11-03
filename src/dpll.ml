include Cnf

type model = (string * bool) list

let rec dpll_rec (cnf : Cnf.cnf) (model : model) : model option =
  if Cnf.solved cnf then
    Some model
  else if Cnf.unsat cnf then
    None
  else
    match Cnf.unit_clause_propagate cnf with
    | Some (sym, value, new_cnf) ->
        dpll_rec new_cnf ((Cnf.symbol_to_string sym, value) :: model)
    | None ->
        match Cnf.pure_literal_assign cnf with
        | Some (sym, value, new_cnf) ->
            dpll_rec new_cnf ((Cnf.symbol_to_string sym, value) :: model)
        | None ->
            let sym = Cnf.choose_symbol cnf in
            match dpll_rec (Cnf.assign_and_simplify sym true cnf)
                           ((Cnf.symbol_to_string sym, true) :: model) with
            | Some result -> Some result
            | None -> dpll_rec (Cnf.assign_and_simplify sym false cnf)
                               ((Cnf.symbol_to_string sym, false) :: model)

let dpll (cnf : (string * Cnf.sign) list list) : model option =
  dpll_rec (Cnf.make_cnf cnf) []

let print_model (model : model) : unit =
  List.iter
    (fun (symbol, value) ->
      Format.printf
        "%s %s\n"
        symbol
        (if value then "true" else "false"))
    model

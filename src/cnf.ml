type symbol = string
type sign = Id | Not

module Symbol = struct
  type t = symbol
  let compare = Pervasives.compare
end
module SymbolMap = Map.Make(Symbol)
module SymbolSet = Set.Make(Symbol)

type clause = sign SymbolMap.t

type cnf = {
  symbols : SymbolSet.t;
  clauses : clause list
}

exception SymbolAlreadyAssigned

let evaluate_symbol (sign : sign) (value : bool) =
  match sign with
  | Id -> value
  | Not -> not value

let symbol_to_string (sym : symbol) : string = sym

let symbol_from_string (sym : string) : symbol = sym

let make_cnf (conj : (string * sign) list list) : cnf =
  let sym_strings =
    List.map (fun literal -> fst literal) (List.concat conj) in
  let build_clause disj =
    List.fold_left
      (fun map (sym, sign) -> SymbolMap.add sym sign map)
      SymbolMap.empty
      disj in
  { symbols = SymbolSet.of_list sym_strings;
    clauses = List.map build_clause conj }

let unpack_cnf (cnf : cnf) : (string * sign) list list =
  List.map SymbolMap.bindings cnf.clauses

let assign_and_simplify (sym : symbol) (value : bool) (cnf : cnf) : cnf =
  if not (SymbolSet.mem sym cnf.symbols) then
    raise SymbolAlreadyAssigned
  else
    (* returns None if this clause is evaluated to true. *)
    let simplify_clause clause =
      try
        if evaluate_symbol (SymbolMap.find sym clause) value
          then None
          else Some (SymbolMap.remove sym clause)
      with Not_found -> Some clause in
    { symbols = SymbolSet.remove sym cnf.symbols;
      clauses = List.fold_right
                  ( fun clause new_clauses ->
                      match simplify_clause clause with
                      | None -> new_clauses
                      | Some clause -> clause :: new_clauses )
                  cnf.clauses
                  [] }

let choose_symbol (cnf : cnf) : symbol = SymbolSet.choose cnf.symbols

let unit_clause_propagate (cnf : cnf) : (symbol * bool * cnf) option =
  try
    let unit_clause =
      List.find (fun clause -> SymbolMap.cardinal clause == 1) cnf.clauses in
    let (sym, symsign) = SymbolMap.min_binding unit_clause in
    let symval = match symsign with Id -> true | Not -> false in
    Some (sym, symval, assign_and_simplify sym symval cnf)
  with Not_found -> None

let is_uniform xs =
  let rec is_uniform_rec x xs =
    match xs with
    | [] -> true
    | y :: ys -> x == y && is_uniform_rec y ys in
  match xs with
  | [] -> false
  | x :: xs -> is_uniform_rec x xs

let pure_literal_assign (cnf : cnf) : (symbol * bool * cnf) option =
  let unassigned_syms = SymbolSet.elements cnf.symbols in
  let is_pure (sym : symbol) : sign option =
    let polarity =
      List.map
        (fun clause -> SymbolMap.find sym clause)
        (List.filter (fun clause -> SymbolMap.mem sym clause) cnf.clauses) in
    if is_uniform polarity then
      Some (List.hd polarity)
    else
      None in
  let rec find_pure_symbol (syms : symbol list) : (symbol * sign) option =
    match syms with
    | [] -> None
    | sym :: syms ->
        match is_pure sym with
        | Some sign -> Some (sym, sign)
        | None -> find_pure_symbol syms in
  match find_pure_symbol unassigned_syms with
  | None -> None
  | Some (sym, Id) -> Some (sym, true, assign_and_simplify sym true cnf)
  | Some (sym, Not) -> Some (sym, false, assign_and_simplify sym false cnf)

let solved (cnf : cnf) : bool =
  let all_symbols_assigned = SymbolSet.is_empty cnf.symbols in
  let all_clauses_true = List.length cnf.clauses == 0 in
  all_symbols_assigned && all_clauses_true

let unsat (cnf : cnf) : bool =
  List.exists (fun clause -> SymbolMap.is_empty clause) cnf.clauses

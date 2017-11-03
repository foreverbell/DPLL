type symbol = string
type sign = Id | Not

module Symbol = struct
  type t = symbol
  let compare = Pervasives.compare
end

module SymbolMap = Map.Make(Symbol)

type clause = sign SymbolMap.t 

type cnf = {
  symbols : bool SymbolMap.t;
  clauses : clause list
}

exception SymbolAlreadyAssigned

let evaluate_symbol (sign : sign) (value : bool) =
  match sign with
  | Id -> value
  | Not -> not value
    
let symbol_to_string (sym : symbol) : string = sym

let make_cnf (conj : (string * sign) list list) : cnf =
  let sym_strings =
    List.map (fun literal -> fst literal) (List.concat conj) in
  let build_clause disj =
    List.fold_left
      (fun map (sym, b) -> SymbolMap.add sym b map)
      SymbolMap.empty
      disj in
  {
    symbols = List.fold_left
                (fun map sym -> SymbolMap.add sym false map)
                SymbolMap.empty
                sym_strings;
    clauses = List.map build_clause conj }

let assign (sym : symbol) (value : bool) (cnf : cnf) : cnf =
  if SymbolMap.find sym cnf.symbols == true then
    raise SymbolAlreadyAssigned
  else
    let simplify_clause (clause : clause) : clause option =
      try
        if evaluate_symbol (SymbolMap.find sym clause) value
          then None
          else Some (SymbolMap.remove sym clause)
      with Not_found -> Some clause in
    { symbols = SymbolMap.add sym true cnf.symbols;
      clauses = List.fold_left
                  ( fun new_clauses clause ->
                      match simplify_clause clause with
                      | None -> new_clauses
                      | Some clause -> clause :: new_clauses )
                  cnf.clauses
                  [] }

let unit_clause_propagate (cnf : cnf) : (symbol * bool * cnf) option =
  try
    let unit_clause =
      List.find (fun clause -> SymbolMap.cardinal clause == 1) cnf.clauses in
    let (sym, symsign) = SymbolMap.min_binding unit_clause in
    let symval = match symsign with Id -> true | Not -> false in
    Some (sym, symval, assign sym symval cnf)
  with Not_found -> None

(* finds an occurred and pure literal, and assigns it to this cnf. *)
let pure_literal_assign (cnf : cnf) : (symbol * bool * cnf) option = None

let solved (cnf : cnf) : bool =
  let all_literals_assigned =
    SymbolMap.for_all (fun k sym -> sym) cnf.symbols in
  let all_clauses_true = List.length cnf.clauses == 0 in
  all_literals_assigned && all_clauses_true

let unsat (cnf : cnf) : bool =
  List.exists (fun clause -> SymbolMap.is_empty clause) cnf.clauses

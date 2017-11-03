(** Caveat: the tests are weak. *)

open Cnf
open Dpll
open OUnit2

let optmap (f : 'a -> 'b) (x : 'a option) : 'b option =
  match x with
  | None -> None
  | Some y -> Some (f y)

let cnf_1 = make_cnf [
  [ ("x_1", Id) ];
  [ ("x_1", Not) ];
  [ ("x_1", Id); ("x_2", Not) ];
  [ ("x_1", Not); ("x_2", Id) ];
  [ ("x_3", Id) ];
]

let cnf_2 = make_cnf [
  [ ("x_1", Id); ("x_2", Not) ];
  [ ("x_1", Not); ("x_2", Id) ];
  [ ("x_1", Id); ("x_3", Id) ];
  [ ("x_3", Id) ];
]

let tests = "test suite"  >::: [

  "assign_and_simplify_1" >:: (fun _ ->
    assert_equal
      (unpack_cnf (assign_and_simplify (symbol_from_string "x_1") true cnf_1))
      [ []; [ ("x_2", Id) ]; [ ("x_3", Id) ] ]
  );

  "assign_and_simplify_2" >:: (fun _ ->
    assert_equal
      (unpack_cnf (assign_and_simplify (symbol_from_string "x_1") false cnf_1))
      [ []; [ ("x_2", Not) ]; [ ("x_3", Id) ] ]
  );

  "unit_clause_propagate" >:: (fun _ ->
    assert_equal
      (optmap (fun (a, b, c) -> unpack_cnf c) (unit_clause_propagate cnf_2))
      (Some [ [ ("x_1", Id); ("x_2", Not) ]; [ ("x_1", Not); ("x_2", Id) ] ])
  );

  "pure_literal_assign" >:: (fun _ ->
    assert_equal
      (optmap (fun (a, b, c) -> unpack_cnf c) (pure_literal_assign cnf_2))
      (Some [ [ ("x_1", Id); ("x_2", Not) ]; [ ("x_1", Not); ("x_2", Id) ] ])
  );

  "dpll_1" >:: (fun _ ->
    assert_equal (dpll (unpack_cnf cnf_1)) Unsat
  );

  "dpll_2" >:: (fun _ ->
    assert_equal
      (dpll (unpack_cnf cnf_2))
      (Solution [ ("x_2", true); ("x_1", true); ("x_3", true) ])
  );

]

let _ = run_test_tt_main tests

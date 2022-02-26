open Core_kernel

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

open KB.Let

module Make(CT : T.Core) = struct

  let m = Bitvec.modulus 32
  let width = T.Bitv.define 32
  let nop = T.Effect.Sort.data "NOP"
  let no_data = KB.return (T.Effect.empty nop)
  let no_ctrl = KB.return (T.Effect.empty T.Effect.Sort.fall)
  let empty_blk = CT.blk T.Label.null no_data no_ctrl

  let rec compile_expr (expr : Ast.expr) : 'b T.Bitv.t T.value KB.t =
    match expr with
    | Ast.Var reg -> CT.var (T.Var.define width reg)
    | Ast.Num n -> CT.int width Bitvec.(int n mod m)
    | Ast.Add (e1, e2) -> CT.add (compile_expr e1) (compile_expr e2)

  let compile_assignment (assign : Ast.assignment) : 'a T.effect KB.t =
    let reg = Ast.lhs_of_assignment assign in
    let var = T.Var.define width reg in
    let expr = Ast.rhs_of_assignment assign in
    CT.set var (compile_expr expr)

  let compile_data (data : Ast.data) : 'a T.effect KB.t =
    List.fold data ~init:no_data ~f:(fun acc assignment ->
      CT.seq (compile_assignment assignment) acc)

  let compile_ctrl (ctrl : Ast.control) : 'a T.effect KB.t =
    match ctrl with
    | Goto dest ->
      let* label = T.Label.for_name dest in
      CT.goto label
    | Fallthrough -> no_ctrl

  let compile_blk (blk : Ast.block) : 'a T.effect KB.t =
    let blk_name = Ast.label_of_block blk in
    let* label = T.Label.for_name blk_name in
    let assignments = Ast.data_of_block blk in
    let* data = compile_data assignments in
    let control = Ast.control_of_block blk in
    let* ctrl = compile_ctrl control in
    CT.blk label (KB.return data) (KB.return ctrl)

  let semantics_of (ast : Ast.t) : 'a T.effect KB.t =
    List.fold ast ~init:empty_blk ~f:(fun acc blk ->
      CT.seq (compile_blk blk) acc)

end

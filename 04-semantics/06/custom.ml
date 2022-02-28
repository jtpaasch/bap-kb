module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

open KB.Let

let name = "my-theory"
let package = "my.org"

let expr_slot : (T.Value.cls, string) KB.slot =
  KB.Class.property T.Value.cls "expr-slot" KB.Domain.string
    ~package

let stmnt_slot : (T.Semantics.cls, string) KB.slot =
  KB.Class.property T.Semantics.cls "stmnt-slot" KB.Domain.string
    ~package

module Theory : T.Core = struct

  include T.Empty

  let empty = T.Effect.empty T.Effect.Sort.bot
  let null_of s = T.Value.empty s

  let int (sort : 's T.Bitv.t T.Value.sort) (bv : Bitvec.t)
      : 's T.Bitv.t T.Value.t KB.t =
    let semantics = Format.asprintf "%a" Bitvec.pp bv in
    let snapshot = KB.Value.put expr_slot (null_of sort) semantics in
    KB.return snapshot

  let var (var : 'a T.Var.t) : 'a T.Value.t KB.t =
    let name = T.Var.name var in
    let sort = T.Var.sort var in
    let snapshot = KB.Value.put expr_slot (null_of sort) name in
    KB.return snapshot

  let set (var : 'a T.Var.t) (expr : 'a T.Value.t KB.t)
      : T.data T.Effect.t KB.t =
    let* e = expr in
    let lhs = T.Var.name var in
    let rhs = KB.Value.get expr_slot e in
    let semantics = Format.sprintf "set %s (%s)" lhs rhs in
    let snapshot = KB.Value.put stmnt_slot empty semantics in
    KB.return snapshot

  let blk (label : T.Label.t) (data : T.data T.Effect.t KB.t)
      (ctrl : T.ctrl T.Effect.t KB.t) : unit T.Effect.t KB.t =
    let* d = data in
    let* c = ctrl in
    let sem1 = KB.Value.get stmnt_slot d in
    let sem2 = KB.Value.get stmnt_slot c in
    let semantics = Format.sprintf "(%s) (%s)" sem1 sem2 in
    let snapshot = KB.Value.put stmnt_slot empty semantics in
    KB.return snapshot

  let seq (prog1 : 'a T.Effect.t KB.t) (prog2 : 'a T.Effect.t KB.t)
      : 'a T.Effect.t KB.t =
    let* p1 = prog1 in
    let* p2 = prog2 in
    let sem1 = KB.Value.get stmnt_slot p1 in
    let sem2 = KB.Value.get stmnt_slot p2 in
    let semantics = Format.sprintf "(%s %s)" sem1 sem2 in
    let snapshot = KB.Value.put stmnt_slot empty semantics in
    KB.return snapshot

end

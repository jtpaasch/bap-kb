open Bap.Std

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

open KB.Let

let () = match Bap_main.init () with
  | Ok () -> ()
  | Error _ -> failwith "Error initializing BAP"

let create_semantics (label : T.Label.t) : 'a T.effect KB.t =
  let* (module CT) = T.current in
  let nop = T.Effect.Sort.data "NOP" in
  let data = KB.return (T.Effect.empty nop) in
  let ctrl = KB.return (T.Effect.empty T.Effect.Sort.fall) in
  CT.blk label data ctrl

let create_compilation_unit (target : T.Target.t) : T.Unit.t KB.t =
  let* comp_unit = KB.Object.create T.Unit.cls in
  let* () = KB.provide T.Unit.target comp_unit target in
  KB.return comp_unit

let create_program (addr : Bitvec.t) (target : T.Target.t) : T.Label.t KB.t =
  let* label = T.Label.for_addr addr in
  let* comp_unit = create_compilation_unit target in
  let* () = KB.provide T.Label.unit label (Some comp_unit) in
  let* semantics = create_semantics label in
  let* () = KB.provide T.Semantics.slot label semantics in
  KB.return label

let inspect_program (label : T.Label.t KB.t) (state : KB.state) : unit =
  let result = KB.run T.Program.cls label state in
  match result with
  | Ok (snapshot, _) -> Format.printf "%a\n%!" KB.Value.pp snapshot
  | Error e -> Format.printf "%a\n%!" KB.Conflict.pp e

let () =
  let state = Toplevel.current () in
  let target = T.Target.read "bap:armv7+le" in
  let addr = Bitvec.(int 0x10400 mod m32) in
  let label = create_program addr target in
  inspect_program label state

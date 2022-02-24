open Core_kernel
open Bap.Std

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

open KB.Let

let () = match Bap_main.init () with
  | Ok () -> ()
  | Error _ -> failwith "Error initializing BAP"

let create_label (addr : Bitvec.t) : T.Label.t KB.t =
  let* label = KB.Object.create T.Program.cls in
  let* () = KB.provide T.Label.addr label (Some addr) in
  KB.return label

let create_compilation_unit (target : T.Target.t) : T.Unit.t KB.t =
  let* comp_unit = KB.Object.create T.Unit.cls in
  let* () = KB.provide T.Unit.target comp_unit target in
  KB.return comp_unit

let create_program (addr : Bitvec.t) (target : T.Target.t) : T.Label.t KB.t =
  let* label = create_label addr in
  let* () = KB.provide T.Label.addr label (Some addr) in

  let* comp_unit = create_compilation_unit target in
  let* () = KB.provide T.Label.unit label (Some comp_unit) in

  KB.return label

let inspect_comp_unit (comp_unit : T.Unit.t option) (state : KB.state) : unit =
  match comp_unit with
  | Some comp_unit' ->
    begin
      let result = KB.run T.Unit.cls (KB.return comp_unit') state in
      match result with
      | Ok (snapshot, _) ->
        let target = KB.Value.get T.Unit.target snapshot in
        Format.printf "- Target: %a\n%!" T.Target.pp target
      | Error e -> Format.printf "Error: %a\n%!" KB.Conflict.pp e
    end
  | None -> Format.printf "No compilation unit\n%!"

let inspect_program (program : T.Label.t KB.t) (state : KB.state) : unit =
  let result = KB.run T.Program.cls program state in
  match result with
  | Ok (snapshot, state') ->
    begin
      let comp_unit = KB.Value.get T.Label.unit snapshot in
      inspect_comp_unit comp_unit state'
    end
  | Error e -> Format.printf "Error: %a\n%!" KB.Conflict.pp e

let () =
  let target = T.Target.read "bap:armv7+le" in
  let addr = Bitvec.(int 0x10400 mod m32) in
  let program = create_program addr target in

  let state = Toplevel.current () in
  inspect_program program state

open Core_kernel
open Bap.Std

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

open KB.Let

let () = match Bap_main.init () with
  | Ok () -> ()
  | Error _ -> failwith "Error initializing BAP"

let create_program : T.Label.t KB.t =
  let* label = KB.Object.create T.Program.cls in
  KB.return label

let () =
  let state = Toplevel.current () in
  let result = KB.run T.Program.cls create_program state in
  match result with
  | Ok (snapshot, _) -> Format.printf "Program: %a\n%!" KB.Value.pp snapshot
  | Error e -> Format.eprintf "Error: %a\n%!" KB.Conflict.pp e

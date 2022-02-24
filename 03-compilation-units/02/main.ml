open Core_kernel
open Bap.Std

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

open KB.Let

let () = match Bap_main.init () with
  | Ok () -> ()
  | Error _ -> failwith "Error initializing BAP"

let create_program (addr : Bitvec.t) : T.Label.t KB.t =
  let* label = KB.Object.create T.Program.cls in
  let* () = KB.provide T.Label.addr label (Some addr) in
  KB.return label

let inspect_address (addr : Bitvec.t option) : unit =
  match addr with
  | Some addr -> Format.printf "Address: %a\n%!" Bitvec.pp addr
  | None -> Format.printf "Address: None\n%!" 

let () =
  let addr = Bitvec.(int 0x10400 mod m32) in
  let program = create_program addr in

  let state = Toplevel.current () in
  let result = KB.run T.Program.cls program state in
  match result with
  | Ok (snapshot, _) ->
    begin
      let addr = KB.Value.get T.Label.addr snapshot in
      inspect_address addr
    end
  | Error e -> Format.eprintf "Error: %a\n%!" KB.Conflict.pp e

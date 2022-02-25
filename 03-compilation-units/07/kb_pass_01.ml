open Core_kernel
open Bap.Std

module Analysis = struct

  module T = Bap_core_theory.Theory
  module KB = Bap_core_theory.KB
  open KB.Let

  let get_label (addr : Bitvec.t) : T.Label.t KB.t =
    let* label = T.Label.for_addr addr in
    KB.return label

  let explore (addr : Bitvec.t) : unit =
    let label = get_label addr in
    let state = Toplevel.current () in
    let result = KB.run T.Program.cls label state in
    match result with
    | Ok (snapshot, _) ->
      begin
        Format.printf "Program: %a\n%!" KB.Value.pp snapshot
      end
    | Error e -> Format.printf "KB error: %a\n%!" KB.Conflict.pp e

end

module Setup = struct

  module Conf = Bap_main.Extension.Configuration
  module Param_type = Bap_main.Extension.Type

  let addr = Conf.parameter Param_type.string "addr"

  let pass (ctxt : Bap_main.ctxt) (proj : Project.t) : unit =
    let addr = Conf.get ctxt addr in
    let () = 
      if String.is_empty addr
        then failwith "No address specified"
        else ()
    in
    let word = Bitvec.of_string addr in
    Analysis.explore word

  let run (ctxt : Bap_main.ctxt) : (unit, Bap_main.error) Stdlib.result =
    Project.register_pass' (pass ctxt);
    Ok ()

end

let () = Bap_main.Extension.declare Setup.run

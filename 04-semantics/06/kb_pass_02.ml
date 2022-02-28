open Core_kernel
open Bap.Std

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

module Setup = struct

  module Conf = Bap_main.Extension.Configuration
  module Param_type = Bap_main.Extension.Type

  let addr = Conf.parameter Param_type.string "addr"

  let explore (addr : Bitvec.t) : unit =
    let label = T.Label.for_addr addr in
    let semantics = Toplevel.eval T.Semantics.slot label in
    Format.printf "%a\n%!" KB.Value.pp semantics

  let pass (ctxt : Bap_main.ctxt) (proj : Project.t) : unit =
    let addr = Conf.get ctxt addr in
    let () = 
      if String.is_empty addr
        then failwith "No address specified"
        else ()
    in
    let word = Bitvec.of_string addr in
    explore word

  let run (ctxt : Bap_main.ctxt) : (unit, Bap_main.error) Stdlib.result =
    let theory = KB.return (module Custom.Theory : T.Core) in
    T.declare theory ~package:Custom.package ~name:Custom.name;
    Project.register_pass' (pass ctxt);
    Ok ()

end

let () = Bap_main.Extension.declare Setup.run

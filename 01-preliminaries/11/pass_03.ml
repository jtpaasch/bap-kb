open Core_kernel
open Bap.Std

module L = Bap_main_event.Log.Create()

let pass (proj : Project.t) : unit =
  L.info "My pass 03 is: %s" "running";
  L.debug "Debug: %s" "a debug message"

let run (ctxt : Bap_main.ctxt) : (unit, Bap_main.error) Stdlib.result =
  Project.register_pass' pass;
  Ok ()

let () =
  Bap_main.Extension.declare run

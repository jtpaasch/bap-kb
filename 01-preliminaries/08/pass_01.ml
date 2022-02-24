open Core_kernel
open Bap.Std

let pass (proj : Project.t) : Project.t =
  print_endline "Running my pass, and returning the project";
  proj

let run (_ctxt : Bap_main.ctxt) : (unit, Bap_main.error) Stdlib.result =
  Project.register_pass pass;
  Ok ()

let () =
  Bap_main.Extension.declare run

open Bap.Std

let pass (proj : Project.t) : unit =
  print_endline "Running my pass: hello, world!"

let run (_ctxt : Bap_main.ctxt) : (unit, Bap_main.error) Stdlib.result =
  Project.register_pass' pass;
  Ok ()

let () = Bap_main.Extension.declare run

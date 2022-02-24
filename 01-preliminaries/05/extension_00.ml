let run (_ctxt : Bap_main.ctxt) : (unit, Bap_main.error) Stdlib.result =
  print_endline "My extension runs";
  Ok ()

let () = Bap_main.Extension.declare run

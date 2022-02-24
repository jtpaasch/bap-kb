open Core_kernel
open Bap.Std

module Conf = Bap_main.Extension.Configuration
module Param_type = Bap_main.Extension.Type

let user = Conf.parameter Param_type.string "user"

let run (ctxt : Bap_main.ctxt) : (unit, Bap_main.error) Stdlib.result =
  let user = Conf.get ctxt user in
  printf "Hello, '%s'\n%!" user;
  Ok ()

let () =
  Bap_main.Extension.declare run

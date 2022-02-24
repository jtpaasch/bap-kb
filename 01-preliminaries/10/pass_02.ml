open Core_kernel
open Bap.Std

module Conf = Bap_main.Extension.Configuration
module Param_type = Bap_main.Extension.Type

let user = Conf.parameter Param_type.string "user"
let fav_num = Conf.parameter Param_type.int "favorite-num"

let pass (ctxt : Bap_main.ctxt) (proj : Project.t) : unit =
  let user = Conf.get ctxt user in
  let fav_num = Conf.get ctxt fav_num in
  printf "Hello, '%s', your favorite number is '%d'\n%!" user fav_num

let run (ctxt : Bap_main.ctxt) : (unit, Bap_main.error) Stdlib.result =
  printf "Conf: '%s'\n%!" Conf.confdir;
  Project.register_pass' (pass ctxt);
  Ok ()

let () =
  Bap_main.Extension.declare run

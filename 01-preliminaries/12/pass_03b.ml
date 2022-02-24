open Core_kernel
open Bap.Std

module E = Bap_main_event

type E.event += Msg of string

let handler (e : E.event) : unit =
  match e with
  | Msg s -> eprintf "-- msg: '%s'\n%!" s
  | _ -> ()

let pass (proj : Project.t) : unit =
  E.send (Msg "hello there")

let run (ctxt : Bap_main.ctxt) : (unit, Bap_main.error) Stdlib.result =
  Project.register_pass' pass;
  Ok ()

let () =
  Bap_future.Std.Stream.observe E.stream handler;
  Bap_main.Extension.declare run

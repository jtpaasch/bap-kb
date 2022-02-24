type Bap_main.Extension.Error.t += Fail of string

let print_error (e : Bap_main.Extension.Error.t) : string option =
  match e with
  | Fail s -> Some (Format.sprintf "We encountered an error: %s" s)
  | _ -> None

let run (_ctxt : Bap_main.ctxt) : (unit, Bap_main.error) Stdlib.result =
  Error (Fail "could not initialize extension")

let () =
  Bap_main.Extension.Error.register_printer print_error;
  Bap_main.Extension.declare run

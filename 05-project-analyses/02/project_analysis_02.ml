open Core_kernel
open Bap.Std

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

open KB.Let

module A = Project.Analysis

let name = "encoding"
let package = "my.org"
let desc = "prints the encoding of a given address"
let grammar = A.(args @@ program $ flag "show-name")

let show_encoding (label : T.Label.t) (show_name : bool) : unit KB.t =
  let* encoding = KB.collect T.Label.encoding label in
  Format.printf "Encoding: %s\n%!" (T.Language.to_string encoding);
  if show_name then
    let* name = KB.collect T.Label.name label in
    let repr = Option.value name ~default:"none" in
    Format.printf "Name: %s\n%!" repr;
    KB.return ()
  else
    KB.return ()

let () = 
  A.register name grammar show_encoding
    ~desc
    ~package

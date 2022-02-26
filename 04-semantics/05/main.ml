open Core_kernel
open Bap.Std

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

open KB.Let

let () = match Bap_main.init () with
  | Ok () -> ()
  | Error _ -> failwith "Error initializing BAP"

let provide_semantics (ast : Ast.t) (_ : T.Label.t) : 'a T.effect KB.t =
  let* (module CT) = T.current in
  let module Cmplr = Compiler.Make(CT) in
  Cmplr.semantics_of ast

let () =
  let ast = Ast.from_file "program.lisp" in
  KB.promise T.Semantics.slot (provide_semantics ast)

let () =
  let label = KB.Object.create T.Program.cls in
  let program = Toplevel.eval T.Semantics.slot label in
  Format.printf "%a\n%!" KB.Value.pp program

open Core_kernel
open Bap.Std

module T = Bap_core_theory.Theory

let () = match Bap_main.init () with
  | Ok () -> ()
  | Error e ->
    failwith (Format.asprintf "%a" Bap_main.Extension.Error.pp e)

let load_exe (filename : string) : project =
  let input = Project.Input.file ~loader:"llvm" ~filename in
  match Project.create input ~package:filename with
  | Ok proj -> proj
  | Error e -> failwith (Error.to_string_hum e)

let () =
  let filepath =
    if Array.length Sys.argv <= 1 then
      failwith "Argument missing: specify a /path/to/exe"
    else
      Sys.argv.(1)
  in
  Format.printf "Loading: %s\n%!" filepath;
  let project = load_exe filepath in
  let target = Project.target project in
  Format.printf "Target architecture: %a\n%!" T.Target.pp target

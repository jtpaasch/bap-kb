open Core_kernel

module Param_type = Bap_main.Extension.Type
module Cmd = Bap_main.Extension.Command
module Err = Bap_main.Extension.Error

type Err.t += Fail of string

let error_printer (e : Err.t) : string option =
  match e with
  | Fail s -> Some (sprintf "My custom command error: %s" s)
  | _ -> None

module Cli = struct

  let name = "my-command-01"
  let doc = "Another demo BAP command"

  let job_title = Cmd.parameter Param_type.string "job-title"
    ~doc:"Your job title"

  let first_name = Cmd.argument Param_type.string
    ~doc:"Your first name"

  let grammar = Cmd.(args $ job_title $ first_name)

  let callback (job_title : string) (first_name : string)
    (ctxt : Bap_main.ctxt) : (unit, Bap_main.error) result =
    match first_name with
    | "Jo" ->
      Error (Fail "Jo, you can't be here")
    | _ ->
      printf "First name: %s\n%!" first_name;
      printf "Job title: %s\n%!" job_title;
      Ok ()

end

let () =
  Err.register_printer error_printer;
  Cmd.declare Cli.name Cli.grammar Cli.callback ~doc:Cli.doc

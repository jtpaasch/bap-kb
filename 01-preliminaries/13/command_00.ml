open Core_kernel

module Param_type = Bap_main.Extension.Type
module Cmd = Bap_main.Extension.Command

module Cli = struct

  let name = "my-command-00"
  let doc = "A demo BAP command"

  let job_title = Cmd.parameter Param_type.string "job-title"
    ~doc:"Your job title"

  let first_name = Cmd.argument Param_type.string
    ~doc:"Your first name"

  let grammar = Cmd.(args $ job_title $ first_name)

  let callback (job_title : string) (first_name : string)
    (ctxt : Bap_main.ctxt) : (unit, Bap_main.error) result =
    printf "First name: %s\n%!" first_name;
    printf "Job title: %s\n%!" job_title;
    Ok ()

end

let () =
  Cmd.declare Cli.name Cli.grammar Cli.callback ~doc:Cli.doc

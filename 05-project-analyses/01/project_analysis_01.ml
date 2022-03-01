open Bap.Std

module KB = Bap_core_theory.KB
module A = Project.Analysis

let name = "hello-world"
let package = "my.org"
let desc = "prints 'hello world'"
let grammar = A.(args empty)

let do_hello_world () : unit KB.t = 
  print_endline "Hello world";
  KB.return ()

let () = 
  A.register name grammar do_hello_world
    ~desc
    ~package

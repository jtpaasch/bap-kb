open Core_kernel

module KB = Bap_knowledge.Knowledge

let () = match Bap_main.init () with
  | Ok () -> ()
  | Error _ -> failwith "Error initializing BAP"

let string_domain : string KB.Domain.t =
  KB.Domain.flat "string-domain"
    ~inspect:(fun s -> Sexp.Atom s)
    ~equal:String.(=)
    ~empty:""

let () =
  let name = KB.Domain.name string_domain in
  Format.printf "Domain name: %s\n%!" name

let optional_bool_domain : bool option KB.Domain.t =
  KB.Domain.optional "optional-bool-domain"
    ~inspect:(fun b -> Sexp.Atom (Bool.to_string b))
    ~equal:Bool.(=)

let () =
  let name = KB.Domain.name optional_bool_domain in
  Format.printf "Domain name: %s\n%!" name

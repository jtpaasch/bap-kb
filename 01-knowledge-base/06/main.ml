open Core_kernel
open Bap.Std

module KB = Bap_knowledge.Knowledge
open KB.Let

let () = match Bap_main.init () with
  | Ok () -> ()
  | Error _ -> failwith "Error initializing BAP"

module Car = struct

  let package = "my.org"
  type tag = Car
  type sort = unit

  let name = "car"
  let desc = "A class representing cars"
  let index = ()
  let cls : (tag, sort) KB.cls =
    KB.Class.declare name index ~package ~desc

  let string_domain : string KB.Domain.t =
    KB.Domain.flat "string-domain"
      ~inspect:(fun s -> Sexp.Atom s)
      ~equal:String.(=)
      ~empty:""

  let color : (tag, string) KB.slot =
    KB.Class.property cls "color" string_domain ~package 

end

let provide_color (_ : Car.tag KB.obj) : string KB.t =
  KB.return "red"

let () =
  KB.promise Car.color provide_color;
  Toplevel.exec
    begin
      let* car = KB.Object.create Car.cls in
      let* color = KB.collect Car.color car in
      Format.printf "- Color: %s\n%!" color;
      KB.return ()
    end

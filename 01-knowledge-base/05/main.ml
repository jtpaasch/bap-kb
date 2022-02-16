open Core_kernel
open Bap.Std

module KB = Bap_knowledge.Knowledge
open KB.Let

let () = match Bap_main.init () with
  | Ok () -> ()
  | Error _ -> failwith "Error initializing BAP"

module Car = struct

  let package = "my.org"
  type tag
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

let () =
  Toplevel.exec
    begin
      let* car = KB.Object.create Car.cls in
      let* repr = KB.Object.repr Car.cls car in
      Format.printf "- Car: %s\n%!" repr;

      let* () = KB.provide Car.color car "red" in
      let* color = KB.collect Car.color car in
      Format.printf "- Color: %s\n%!" color;

      KB.return ()
    end

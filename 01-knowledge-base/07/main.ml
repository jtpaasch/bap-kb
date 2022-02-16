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

let build_car : Car.tag KB.obj KB.t =
  let* car = KB.Object.create Car.cls in
  let* () = KB.provide Car.color car "red" in
  KB.return car

let () =
  let state = Toplevel.current () in
  let result = KB.run Car.cls build_car state in
  match result with
  | Ok (snapshot, _) ->
    begin
      Format.printf "- Snapshot: %a\n%!" KB.Value.pp snapshot;
      let color = KB.Value.get Car.color snapshot in
      Format.printf "- Color: %s\n%!" color
    end
  | Error e -> Format.eprintf "KB problem: %a\n%!" KB.Conflict.pp e

module KB = Bap_knowledge.Knowledge

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

end

let get_name (cls : ('k, 's) KB.cls) : string =
  let kb_name = KB.Class.name cls in
  KB.Name.show kb_name

let () =
  let car_class_name = get_name Car.cls in
  Format.printf "Car class name: %s\n%!" car_class_name

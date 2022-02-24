module KB = Bap_knowledge.Knowledge

let () = match Bap_main.init () with
  | Ok () -> ()
  | Error _ -> failwith "Error initializing BAP"

module Employee = struct

  let package = "my.org"
  type tag = Employee
  type sort = Sales | Marketing | Executive

  let name = "sales-employee"
  let desc = "A class representing employees on the sales team"
  let index = Sales
  let sales_cls : (tag, sort) KB.cls =
    KB.Class.declare name index ~package ~desc

  let name = "executive-employee"
  let desc = "A class representing employees on the executive team"
  let index = Executive
  let executive_cls : (tag, sort) KB.cls =
    KB.Class.declare name index ~package ~desc

end

let get_name (cls : ('k, 's) KB.cls) : string =
  let kb_name = KB.Class.name cls in
  KB.Name.show kb_name

let () =
  let sales_class_name = get_name Employee.sales_cls in
  let executive_class_name = get_name Employee.executive_cls in
  Format.printf "Sales class name: %s\n%!" sales_class_name;
  Format.printf "Executive class name: %s\n%!" executive_class_name

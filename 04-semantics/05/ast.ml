open Core_kernel

type label = string
type reg = string
type num = int

type expr = Var of reg | Num of num | Add of expr * expr
type assignment = Assign of reg * expr

type data = assignment list
type control = Goto of label | Fallthrough

type block = Block of label * data * control
type t = block list

let lhs_of_assignment a = match a with Assign (reg, _) -> reg
let rhs_of_assignment a = match a with Assign (_, expr) -> expr 

let label_of_block b = match b with Block (label, _, _) -> label
let data_of_block b = match b with Block (_, data, _) -> data
let control_of_block b = match b with Block (_, _, control) -> control

let err (msg : string) (sexp : Sexp.t) : string =
  let sexp_str = Sexp.to_string sexp in
  Format.sprintf "Parse error: %s: %s" msg sexp_str

let parse_number (num : string) : num =
  try int_of_string num
  with _ -> failwith (err "Invalid number" (Sexp.Atom num))

let rec parse_expr (sexp : Sexp.t) : expr =
  match sexp with
  | Sexp.List [ Atom "reg"; Atom reg ] -> Var reg
  | Sexp.List [ Atom "int"; Atom n ] -> Num (parse_number n) 
  | Sexp.List [ Atom "add"; e1 ; e2 ] ->
    Add (parse_expr e1, parse_expr e2)
  | _ -> failwith (err "Invalid expr" sexp)

let parse_data (sexps : Sexp.t list) : data =
  List.fold sexps ~init:[] ~f:(fun assigns sexp -> match sexp with
    | Sexp.List [ Atom "set"; Atom reg; e ] ->
      List.append assigns [Assign (reg, parse_expr e)]
    | _ -> failwith (err "Invalid data" sexp))

let parse_ctrl (sexp : Sexp.t) : control =
  match sexp with
  | Sexp.List [ Atom "goto"; Atom label ] -> Goto label
  | Sexp.List [ Atom "fallthrough" ] -> Fallthrough
  | _ -> failwith (err "Invalid control" sexp)

let parse (sexps : Sexp.t list) : t =
  List.fold sexps ~init:[] ~f:(fun blks sexp -> match sexp with
    | Sexp.List [ Atom "block"; Atom label;
        List [ Atom "data"; List data ];
        List [ Atom "control"; ctrl ]; ] ->
      List.append blks [Block (label, parse_data data, parse_ctrl ctrl)]
    | _ -> failwith (err "Parse error" sexp))

let from_file (filepath : string) : t =
  let sexps = Sexp.load_sexps filepath in
  parse sexps

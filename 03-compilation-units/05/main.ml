open Core_kernel
open Bap.Std

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

open KB.Let

let () = match Bap_main.init () with
  | Ok () -> ()
  | Error _ -> failwith "Error initializing BAP"

let create_label (addr : Bitvec.t) : T.Label.t KB.t =
  let* label = KB.Object.create T.Program.cls in
  let* () = KB.provide T.Label.addr label (Some addr) in
  KB.return label

let create_compilation_unit (target : T.Target.t) : T.Unit.t KB.t =
  let* comp_unit = KB.Object.create T.Unit.cls in
  let* () = KB.provide T.Unit.target comp_unit target in
  KB.return comp_unit

let create_program (addr : Bitvec.t) (target : T.Target.t) : T.Label.t KB.t =
  let* label = create_label addr in
  let* comp_unit = create_compilation_unit target in
  KB.return label

let inspect_target (target : T.Target.t) : unit =
  let endianness = T.Target.endianness target in
  let wordsize = T.Target.bits target in
  let memory_address_size = T.Target.data_addr_size target in
  let pc_size = T.Target.code_addr_size target in
  let insn_alignment_size = T.Target.code_alignment target in
  let mem_var = T.Target.data target in
  let sp = T.Target.reg target T.Role.Register.stack_pointer in
  let sp_to_string sp =
    match sp with
    | Some reg -> T.Var.name reg
    | None -> "unknown"
  in
  let regs = T.Target.regs target in
  let regs_to_string regs =
    let regs_list = List.map (Set.to_list regs) ~f:T.Var.name in
    String.concat regs_list ~sep:", "
  in
  Format.printf "- Endianness: %a\n%!" T.Endianness.pp endianness;
  Format.printf "- Wordsize: %d\n%!" wordsize;
  Format.printf "- Memory address size: %d\n%!" memory_address_size;
  Format.printf "- Instruction size (program counter size): %d\n%!" pc_size;
  Format.printf "- Instruction alignment: %d\n%!" insn_alignment_size;
  Format.printf "- Memory variable (its name): %s\n%!" (T.Var.name mem_var);
  Format.printf "- SP: %s\n%!" (sp_to_string sp);
  Format.printf "- Registers: %s\n%!" (regs_to_string regs)

let inspect_comp_unit (comp_unit : T.Unit.t option) (state : KB.state) : unit =
  match comp_unit with
  | Some comp_unit' ->
    begin
      let result = KB.run T.Unit.cls (KB.return comp_unit') state in
      match result with
      | Ok (snapshot, _) ->
        let target = KB.Value.get T.Unit.target snapshot in
        Format.printf "- Target: %a\n%!" T.Target.pp target;
        inspect_target target
      | Error e -> Format.printf "Error: %a\n%!" KB.Conflict.pp e
    end
  | None -> Format.printf "No compilation unit\n%!"

let inspect_program (program : T.Label.t KB.t) (state : KB.state) : unit =
  let result = KB.run T.Program.cls program state in
  match result with
  | Ok (snapshot, state') ->
    begin
      let comp_unit = KB.Value.get T.Label.unit snapshot in
      inspect_comp_unit comp_unit state'
    end
  | Error e -> Format.printf "Error: %a\n%!" KB.Conflict.pp e

let () =
  let target = T.Target.read "bap:armv7+le" in
  let addr = Bitvec.(int 0x10400 mod m32) in
  let program = create_program addr target in

  let state = Toplevel.current () in
  inspect_program program state

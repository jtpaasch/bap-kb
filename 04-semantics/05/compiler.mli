module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

module Make(CT : T.Core) : sig
  val semantics_of : Ast.t -> unit T.effect KB.t
end

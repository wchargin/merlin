module Make
    (G : Utils.Grammar) (A : Recover_attrib.S)
    (S : Synthesis.S) (R : Recovery.S) :
sig
  val emit_prelude : name:string -> Format.formatter -> unit
  val emit_recovery : Format.formatter -> unit
end

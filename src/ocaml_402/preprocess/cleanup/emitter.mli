module Make
    (G : Utils.G)
    (A : Recover_attrib.S with module G = G)
    (S : Synthesis.S with module G = G)
    (R : Recovery.S with module G = G) :
sig
  val emit_prelude : name:string -> Format.formatter -> unit
  val emit_recovery : Format.formatter -> unit
end

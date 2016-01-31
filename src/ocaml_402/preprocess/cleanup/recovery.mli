open MenhirSdk.Cmly_format

type item = lr1_state * production * int

type recovery = lr1_state -> int * (lr1_state option * item list) list

module type S = sig
  val recover : recovery
  val report : Format.formatter -> unit
end

module Make (G : Utils.Grammar) (S : Synthesis.S) : S

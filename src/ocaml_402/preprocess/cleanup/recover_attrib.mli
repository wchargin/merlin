open MenhirSdk.Cmly_format

module type S = sig
  val cost_of_prod    : production -> float
  val penalty_of_item : production * int -> float
  val cost_of_symbol  : symbol -> float

  val default_prelude : Format.formatter -> unit
  val default_value   : symbol -> string option
end

module Make (G : Utils.Grammar) : S

open MenhirSdk.Cmly_format

type variable =
  | Head of lr1_state * nonterminal
  | Tail of lr1_state * production * int

type 'a paction =
  | Abort
  | Reduce of production
  | Shift  of symbol
  | Var    of 'a

type action = variable paction

module type S = sig
  val cost_of  : variable -> float
  val cost_of_action  : action -> float
  val cost_of_actions : action list -> float
  val solution : variable -> action list
  val report   : Format.formatter -> unit
end

module Make (G : Utils.Grammar) (A : Recover_attrib.S) : S

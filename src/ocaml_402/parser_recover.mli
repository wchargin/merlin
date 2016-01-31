open Parser_raw

val default_value : 'a MenhirInterpreter.symbol -> 'a

type t =
  | Abort
  | Reduce of int
  | Shift : 'a MenhirInterpreter.symbol -> t
  | Sub of t list

val recover : int -> int * (int -> t list)

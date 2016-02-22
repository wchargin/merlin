open Parser_raw

val default_value : 'a MenhirInterpreter.symbol -> 'a

type action =
  | Abort
  | Pop
  | R of int
  | S : 'a MenhirInterpreter.symbol -> action
  | Sub of action list

type decision =
  | Nothing
  | One of action list
  | Select of (int -> action list)

val depth : int array

val recover : int -> decision

(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2014  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

(* Lexing is split in two steps.

   First the list of tokens is represented by a [item History.t].
   It's a pure value, independent of the context.

   Second the process of lexing is represented by values of type [t].  You
   resume the process from an arbitrary list of tokens, feeding it with one
   or more string, and you can extract the current list of tokens and cursor
   position at any time.
   Beware, the cursor may be in the middle of a not yet determined token.

   The process ultimately ends when fed with the empty string, representing
   EOF.
*)

(* Lexing step *)
type 'state item =
  | Valid of Lexing.position * Raw_parser.token * Lexing.position * 'state option
  | Error of Raw_lexer.error * Location.t

module type S = sig
  type state
  val equal: state -> state -> bool

  type t
  val start: Lexing.position -> state -> t
  val seek: t -> Lexing.position -> unit
  val feed: t -> string -> (state item -> unit) -> unit
  val position: t -> Lexing.position
  val eof: t -> bool
end
type 'state lexer = (module S with type state = 'state)

(** Create an empty list new lexer *)
val empty: filename:string -> 'state -> (exn list * 'state item) History.t

(** Prepare for lexing.
    Returns the start position (end position of last valid token), and a
    lexing function that will append at most one token to the history at each
    call. *)
type 'state t
val history: 'state t -> (exn list * 'state item) History.t
val start: 'state lexer -> (exn list * 'state item) History.t -> 'state t
val position: 'state t -> Lexing.position
val feed: 'state t -> string -> unit
val eof: 'state t -> bool

(* Some lexers *)

module Caml_lexer : sig
  include S
  val from_keywords : Raw_lexer.keywords -> state
end
val caml_lexer : Caml_lexer.state lexer

(* Miscellaneous functions *)

val item_equal: 'state lexer -> 'state item -> 'state item -> bool
val item_start: _ item -> Lexing.position
val item_end: _ item -> Lexing.position

val reconstruct_identifier: ?for_locate:bool -> (exn list * _ item) History.t -> string Location.loc list
val identifier_suffix: string Location.loc list -> string Location.loc list

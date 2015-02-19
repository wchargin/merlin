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

type lexer_state = exn

(* Lexing step *)
type item =
  | Valid of Lexing.position * Raw_parser.token * Lexing.position * lexer_state
  | Error of Raw_lexer.error * Location.t

module type S = sig
  type state
  exception State of state

  type t
  val start: Lexing.position -> state -> t
  val seek: t -> Lexing.position -> unit
  val feed: t -> string ->
    token:(Lexing.position -> Raw_parser.token -> Lexing.position -> state option -> unit) ->
    error:(Raw_lexer.error -> Location.t -> unit) ->
    unit
  val position: t -> Lexing.position
  val eof: t -> bool
end
type 'state lexer = (module S with type state = 'state)

(** Create an empty list new lexer *)
val empty: filename:string -> 'state lexer -> 'state -> (exn list * item) History.t

(** Prepare for lexing.
    Returns the start position (end position of last valid token), and a
    lexing function that will append at most one token to the history at each
    call. *)
type t
val history: t -> (exn list * item) History.t
val start: _ lexer -> (exn list * item) History.t -> t
val position: t -> Lexing.position
val feed: t -> string -> unit
val eof: t -> bool

(* Some lexers *)

type caml_lex
val from_keywords : Raw_lexer.keywords -> caml_lex
val caml_lexer : caml_lex lexer

(* Miscellaneous functions *)

val same_token: item -> item -> bool
val item_start: item -> Lexing.position
val item_end: item -> Lexing.position

val reconstruct_identifier: ?for_locate:bool -> (exn list * item) History.t -> string Location.loc list
val identifier_suffix: string Location.loc list -> string Location.loc list

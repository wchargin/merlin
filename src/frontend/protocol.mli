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

open Std
open Merlin_lib

module Completion : sig
  type entry = {
    name: string;
    kind: [`Value|`Constructor|`Variant|`Label|
           `Module|`Modtype|`Type|`MethodCall];
    desc: string;
    info: string;
  }

  type application_context = {
    argument_type: string;
    labels : (string * string) list;
  }

  type t = {
    entries: entry list;
    context: [ `Unknown
             | `Application of application_context
             ]
  }
end

module Outline : sig
  type t = item list

  and item = {
    name : string ;
    kind : [`Value|`Constructor|`Label|`Module|`Modtype|`Type|`Exn] ;
    location : Location.t ;
    children : t ;
  }
end

module Shape : sig
  type t = {
    location : Location.t;
    children : t list;
  }
end

module Type_enclosing : sig
  type is_tail_position = [`No | `Tail_position | `Tail_call]

  type t = {
    location : Location.t;
    text : string;
    is_tail : is_tail_position;
  }
end

type context = {
  path: string;
  kind: [`ML | `MLI | `Auto];
  name: string option;
  config: string list option;
  stdlib: string option;
}

type cursor = Lexing.position

type synchronization =
  | Sync_none
  | Sync_set of string

type _ command =
  | Clear
    :  unit command
  | Noop
    :  unit command
  | Type_expr
    :  string
    -> string command
  | Type_enclosing
    :  (string * int) option
    -> Type_enclosing.t list command
  | Enclosing
    :  Location.t list command
  | Complete_prefix
    :  string * bool
    -> Completion.t command
  | Expand_prefix
    :  string
    -> Completion.t command
  | Document
    : string option
    -> [ `Found of string
       | `Invalid_context
       | `Not_in_env of string
       | `File_not_found of string
       | `Not_found of string * string option
       | `No_documentation
       ] command
  | Locate
    : string option * [ `ML | `MLI ]
    -> [ `Found of string option * Lexing.position
       | `Invalid_context
       | `Not_in_env of string
       | `File_not_found of string
       | `Not_found of string * string option
       | `At_origin
       ] command
  | Case_analysis
    :  Location.t
    -> (Location.t * string) command
  | Outline
    :  Outline.t command
  | Shape
    :  Shape.t list command
  | Errors
    :  Error_report.t list command
  | Dump
    :  [`Env of [`Normal|`Full] | `Flags | `Warnings
       |`Sig|`Parser|`Exn|`Browse|`Recover|`Typer of [`Input|`Output] | `Tokens]
    -> Json.json command
  | Which_path
    :  string list
    -> string command
  | Which_with_ext
    :  string list
    -> string list command
  | Flags_set
    :  string list
    -> [ `Ok | `Failures of (string * exn) list ] command
  | Flags_get
    :  string list list command
  | Findlib_use
    :  string list
    -> [`Ok | `Failures of (string * exn) list] command
  | Findlib_list
    :  string list command
  | Extension_list
    :  [`All|`Enabled|`Disabled]
    -> string list command
  | Extension_set
    :  [`Enabled|`Disabled] * string list
    -> [`Ok | `Failures of (string * exn) list] command
  | Path
    :  [`Build|`Source]
     * [`Add|`Rem]
     * string list
    -> unit command
  | Path_reset
    :  unit command
  | Path_list
    :  [`Build|`Source]
    -> string list command
  | Project_get
    :  (string list * [`Ok | `Failures of (string * exn) list]) command
  | Occurrences
    : [`Ident]
    -> Location.t list command
  | Idle_job
    : bool command
  | Version
    : string command

type request = Request : context * cursor * synchronization * 'a command -> request

type result =
  | Return    : 'a command * 'a -> result
  | Failure   : string -> result
  | Error     : Json.json -> result
  | Exception : exn -> result

type response = {
  messages: string list;
  result: result;
}

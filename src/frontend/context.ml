open Std
open Merlin_lib

let table : (Protocol.context, Buffer.t) Hashtbl.t = Hashtbl.create 7

let instantiate ctx =
  let open Protocol in
  let kind = match ctx.kind with
    | `ML -> Merlin_parser.implementation
    | `MLI -> Merlin_parser.interface
    | `Auto when Filename.check_suffix ctx.path ".ml" ->
      Merlin_parser.implementation
    | `Auto when Filename.check_suffix ctx.path ".mli" ->
      Merlin_parser.interface
    | `Auto -> Merlin_parser.implementation
  in
  Buffer.create ?dot_merlins:ctx.config ~path:ctx.path kind

let get ctx =
  try Hashtbl.find table ctx
  with Not_found ->
    let result = instantiate ctx in
    Hashtbl.add table ctx result;
    result

let clear ctx =
  Hashtbl.remove table ctx

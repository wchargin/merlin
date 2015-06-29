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

type namespace = [
  | `Vals
  | `Type
  | `Constr
  | `Mod
  | `Modtype
  | `Functor
  | `Labels
  | `Unknown
]
type path = (string * namespace) list

type trie = (Location.t * namespace * node) list String.Map.t
 and node =
   | Leaf
   | Internal of trie
   | Included of path
   | Alias    of path


type cmt_item = {
  cmt_infos : Cmt_format.cmt_infos ;
  cmi_infos : Cmi_format.cmi_infos option;
  mutable location_trie : trie ;
}

include File_cache.Make (struct
  type t = cmt_item

  let read file =
    match Cmt_format.read file with
    | cmi_infos, Some cmt_infos ->
      {
        cmt_infos ; cmi_infos ;
        location_trie = String.Map.empty ;
      }
    | _, None -> raise Cmt_format.(Error (Not_a_typedtree file))
end)

let read_cmi name =
  let cmti = Filename.chop_extension name ^ ".cmti" in
  try
    if not (Sys.file_exists cmti) then
      raise Not_found;
    let ic = open_in_bin name in
    try
      let magic_len = String.length Config.cmi_magic_number in
      let buffer = String.create magic_len in
      really_input ic buffer 0 magic_len;
      if buffer <> Config.cmi_magic_number then
        raise Not_found;
      Misc.skip_value ic; (* string * Types.signature *);
      let crcs, crc = Cmi_format.read_crcs ic in
      let crc = match crc with
        | Some crc -> crc
        | None -> raise Not_found
      in
      let cached = read cmti in
      match cached with
      | {cmt_infos = {Cmt_format. cmt_interface_digest = Some crc';
                      cmt_annots = Cmt_format.Interface sg};
         cmi_infos = Some cmi_infos}
        when crc = crc' -> {cmi_infos with Cmi_format.
                                        cmi_crcs = crcs;
                                        cmi_sign = sg.Typedtree.sig_type}
      | _ ->
        raise Not_found
    with exn ->
      close_in_noerr ic;
      raise exn
  with Not_found ->
    Cmi_format.read_cmi name

let () = Cmi_cache.read_cmi := read_cmi

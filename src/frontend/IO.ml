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
open Logger

type io = Protocol.request Stream.t * (Protocol.response -> unit)
type low_io = Json.json Stream.t * (Json.json -> unit)
type io_maker =
  on_read:(Unix.file_descr -> unit) -> input:Unix.file_descr ->
  output:Unix.file_descr ->
  low_io

let section = Logger.section "protocol"

let invalid_arguments () = failwith "invalid arguments"

let last_time = ref (Sys.time ())
let log_time fields =
  let old_time = !last_time in
  let new_time = Sys.time () in
  last_time := new_time;
  ("delta", `Int (int_of_float ((new_time -. old_time) *. 1000.))) ::
  fields

let json_log (input,output) =
  let wrap json = `Assoc (log_time ["body", json]) in
  let log_input json =
    Logger.infojf section ~title:"input" wrap json; json
  in
  let log_output json =
    Logger.infojf section ~title:"output" wrap json; json
  in
  let input' = Stream.map ~f:log_input input in
  let output' json = output (log_output json) in
  input', output'

let json_make ~on_read ~input ~output =
  let rec read buf len =
    on_read input;
    try Unix.read input buf 0 len
    with Unix.Unix_error (Unix.EINTR,_,_) ->
      read buf len
  in
  let lexbuf = Lexing.from_function read in
  let input = Json.stream_from_lexbuf (Json.init_lexer ()) lexbuf in
  let output = Unix.out_channel_of_descr output in
  let output' = Json.to_channel output in
  let output json =
    output' json;
    output_char output '\n';
    flush output
  in
  input, output

let makers = ref ["json", ("(default) simple JSON-based protocol", json_make)]

let register_protocol ~name ~desc inst =
  makers := (name, (desc,inst)) :: !makers

let make' = ref json_make
let make ~on_read ~input ~output =
  let io = !make' ~on_read ~input ~output in
  if Logger.is_monitored section then json_log io else io

let select_frontend name =
  try make' := snd (List.assoc name !makers)
  with Not_found ->
    if name <> "help" then
      prerr_endline
        ("Unknown protocol '" ^ name ^ "' (maybe check build configuration)\n");
    prerr_endline "Choose protocol to use for communication. Known protocols:";
    List.iter (fun (name, (desc, _)) ->
        prerr_endline (name ^ "\t" ^ desc))
      !makers;
    exit 1

module Protocol_io = struct
  exception Failure' = Failure
  open Protocol

  let json_of_error {Error_report. valid; text; where; sub; loc} =
    let of_sub (msg,loc) =
      Json.with_location ~skip_none:true loc ["message", `String msg] in
    let content = [
      "type"    , `String where;
      "sub"     , `List (List.map ~f:of_sub sub);
      "valid"   , `Bool valid;
      "message" , `String text;
    ] in
    Json.with_location ~skip_none:true loc content

  let error_catcher exn =
    match Error_report.error_catcher exn with
    | None -> None
    | Some (loc,t) -> Some (loc, json_of_error t)

  let make_pos (pos_lnum, pos_cnum) =
    Lexing.({ pos_fname = "" ; pos_lnum ; pos_cnum ; pos_bol = 0 })

  let pos_of_json = function
    | `Assoc props ->
      begin try match List.assoc "line" props, List.assoc "col" props with
        | `Int line, `Int col -> make_pos (line,col)
        | _ -> failwith "Incorrect position"
      with Not_found -> failwith "Incorrect position"
      end
    | _ -> failwith "Incorrect position"

  let optional_position = function
    | [`String "at"; jpos] -> Some (pos_of_json jpos)
    | [] -> None
    | _ -> invalid_arguments ()

  let mandatory_position = function
    | [`String "at"; jpos] -> pos_of_json jpos
    | _ -> invalid_arguments ()

  let optional_string = function
    | [`String name] -> Some name
    | [] -> None
    | _ -> invalid_arguments ()

  let string_list l =
    List.map (function `String s -> s | _ -> invalid_arguments ()) l

  let json_of_string_list l =
    `List (List.map (fun s -> `String s) l)

  let json_of_type_loc {Type_enclosing. location; text; is_tail} =
    Json.with_location location [
      "type", `String text;
      "tail", `String (match is_tail with
          | `No -> "no"
          | `Tail_position -> "position"
          | `Tail_call -> "call")
    ]

  let string_of_kind = function
    | `Value       -> "Value"
    | `Variant     -> "Variant"
    | `Constructor -> "Constructor"
    | `Label       -> "Label"
    | `Module      -> "Module"
    | `Modtype     -> "Signature"
    | `Type        -> "Type"
    | `MethodCall  -> "#"
    | `Exn         -> "Exn"

  let json_of_completion {Completion. name; kind; desc; info} =
    `Assoc ["name", `String name;
            "kind", `String (string_of_kind kind);
            "desc", `String desc;
            "info", `String info]

  let json_of_completions {Completion. entries; context } =
    `Assoc [
      "entries", `List (List.map json_of_completion entries);
      "context", (match context with
          | `Unknown -> `Null
          | `Application {Completion. argument_type; labels} ->
            let label (name,ty) = `Assoc ["name", `String name;
                                          "type", `String ty] in
            let a = `Assoc ["argument_type", `String argument_type;
                            "labels", `List (List.map label labels)] in
            `List [`String "application"; a])
    ]

  let rec json_of_outline outline =
    let json_of_item {Outline. name ; kind ; location ; children } =
      Json.with_location location [
        "name", `String name;
        "kind", `String (string_of_kind kind);
        "children", `List (json_of_outline children);
      ]
    in
    List.map json_of_item outline

  let rec json_of_shape {Shape. location; children } =
    Json.with_location location [
      "children", `List (List.map ~f:json_of_shape children);
    ]

  let source_or_build = function
    | "source" -> `Source
    | "build"  -> `Build
    | _ -> invalid_arguments ()

  let ml_or_mli = function
    | "ml" -> `ML
    | "mli"  -> `MLI
    | _ -> invalid_arguments ()

  let auto_ml_or_mli = function
    | "auto" -> `Auto
    | x -> ml_or_mli x

  let add_or_remove = function
    | "add"    -> `Add
    | "remove" -> `Rem
    | _ -> invalid_arguments ()

  let load_or_find = function
    | "load" -> `File
    | "find" -> `Find
    | _ -> invalid_arguments ()

  let with_failures assoc = function
    | `Ok -> assoc
    | `Failures failures ->
      let packages, flags, extensions =
        List.fold_left failures ~init:([],[],[]) ~f:(
          fun (pkgs, flgs, exts) (str,exn) ->
            let str = "\"" ^ str ^ "\"" in
            match exn with
            | Fl_package_base.No_such_package _ -> str :: pkgs, flgs, exts
            | Arg.Bad _ -> pkgs, str :: flgs, exts
            | Extension.Unknown -> pkgs, flgs, str :: exts
            | e -> (str ^ " (" ^ Printexc.to_string e ^ ")") :: pkgs, flgs, exts
        )
      in
      let packages =
        match packages with
        | [] -> []
        | failures ->
          let str = String.concat ~sep:", " failures in
          [ `String ("Failed to load some packages " ^ str) ]
      in
      let flags =
        match flags with
        | [] -> []
        | failures ->
          let str = String.concat ~sep:", " failures in
          [ `String ("Unknown flags " ^ str) ]
      in
      let extensions =
        match extensions with
        | [] -> []
        | failures ->
          let str = String.concat ~sep:", " failures in
          [ `String ("Unknown extensions " ^ str) ]
      in
      ("failures", `List (packages @ flags @ extensions)) :: assoc

  open Json.Util

  let context_of_json json =
    let path = match member "path" json |> to_string_option with
      | Some path -> path
      | None -> invalid_arguments ()

    and kind = match member "kind" json |> to_string_option with
      | Some "ml" -> `ML
      | Some "mli" -> `MLI
      | Some "auto" | None -> `Auto
      | _ -> invalid_arguments ()

    and config = match member "config" json with
      | `Null -> None
      | `List l -> Some (List.map to_string l)
      | _ -> invalid_arguments ()

    and stdlib = match member "stdlib" json with
      | `Null -> None
      | `String s -> Some s
      | _ -> invalid_arguments ()

    and name = match member "name" json with
      | `Null -> None
      | `String s -> Some s
      | _ -> invalid_arguments ()

    in
    {path; kind; name; config; stdlib}

  let cursor_of_json json =
    Lexing.make_pos (
      member "line" json |> to_int,
      member "col"  json |> to_int
    )

  let synchronization_of_json json =
    match member "kind" json |> to_string_option with
    | Some "set" -> Sync_set (member "body" json |> to_string)
    | Some "none" -> Sync_none
    | None | Some _ -> invalid_arguments ()

  let command_of_json context cursor sync =
    let request x = Request (context, cursor, sync, x) in
    function
    | [`String "type"; `String "expression"; `String expr] ->
      request (Type_expr expr)
    | [`String "type"; `String "enclosing";
       ( `Assoc [ "expr", `String expr  ; "offset", `Int offset ]
       | `Assoc [ "offset", `Int offset ; "expr", `String expr  ] )
      ] ->
      request (Type_enclosing (Some (expr, offset)))
    | [`String "type"; `String "enclosing"] ->
      request (Type_enclosing None)
    | [ `String "case"; `String "analysis"; `String "from"; x; `String "to"; y ] ->
      let loc_start = pos_of_json x in
      let loc_end = pos_of_json y in
      let loc_ghost = true in
      request (Case_analysis ({ Location. loc_start ; loc_end ; loc_ghost }))
    | [`String "enclosing"] ->
      request Enclosing
    | [`String "complete"; `String "prefix"; `String prefix] ->
      request (Complete_prefix (prefix, false))
    | [`String "complete"; `String "prefix"; `String prefix;
       `String "with"; `String "doc"] ->
      request (Complete_prefix (prefix, true))
    | [`String "expand"; `String "prefix"; `String prefix] ->
      request (Expand_prefix prefix)
    | [`String "document"; (`String "" | `Null)] ->
      request (Document None)
    | [`String "document"; `String path] ->
      request (Document (Some path))
    | [`String "locate"; (`String "" | `Null); `String choice] ->
      request (Locate (None, ml_or_mli choice))
    | [`String "locate"; `String path; `String choice] ->
      request (Locate (Some path, ml_or_mli choice))
    | [`String "outline"] ->
      request Outline
    | [`String "shape"] ->
      request Shape
    | [`String "errors"] ->
      request Errors
    | [`String "dump"; `String "env"] ->
      request (Dump (`Env `Normal))
    | [`String "dump"; `String "full_env"] ->
      request (Dump (`Env `Full))
    | [`String "dump"; `String "sig"] ->
      request (Dump `Sig)
    | [`String "dump"; `String "parser"] ->
      request (Dump `Parser)
    | [`String "dump"; `String "recover"] ->
      request (Dump `Recover)
    | [`String "dump"; `String "exn"] ->
      request (Dump `Exn)
    | [`String "dump"; `String "browse"] ->
      request (Dump `Browse)
    | [`String "dump"; `String "typer"; `String "input"] ->
      request (Dump (`Typer `Input))
    | [`String "dump"; `String "typer"; `String "output"] ->
      request (Dump (`Typer `Output))
    | [`String "dump"; `String "tokens"] ->
      request (Dump `Tokens)
    | [`String "dump"; `String "flags"] ->
      request (Dump `Flags)
    | [`String "dump"; `String "warnings"] ->
      request (Dump `Warnings)
    | [`String "which"; `String "path"; `String name] ->
      request (Which_path [name])
    | [`String "which"; `String "path"; `List names] ->
      request (Which_path (string_list names))
    | [`String "which"; `String "with_ext"; `String ext] ->
      request (Which_with_ext [ext])
    | [`String "which"; `String "with_ext"; `List exts] ->
      request (Which_with_ext (string_list exts))
    | [`String "flags" ; `String "set" ; `List flags ] ->
      request (Flags_set (string_list flags))
    | [`String "flags" ; `String "get" ] ->
      request (Flags_get)
    | [`String "find"; `String "use"; `List packages] ->
      request (Findlib_use (string_list packages))
    | [`String "find"; `String "list"] ->
      request Findlib_list
    | [`String "extension"; `String "enable"; `List extensions] ->
      request (Extension_set (`Enabled,string_list extensions))
    | [`String "extension"; `String "disable"; `List extensions] ->
      request (Extension_set (`Disabled,string_list extensions))
    | [`String "extension"; `String "list"] ->
      request (Extension_list `All)
    | [`String "extension"; `String "list"; `String "enabled"] ->
      request (Extension_list `Enabled)
    | [`String "extension"; `String "list"; `String "disabled"] ->
      request (Extension_list `Disabled)
    | [`String "path"; `String "list";
                       `String ("source"|"build" as var)] ->
      request (Path_list (source_or_build var))
    | [`String "path"; `String "reset"] ->
      request Path_reset
    | [`String "path"; `String ("add"|"remove" as action);
       `String ("source"|"build" as var); `List pathes] ->
      request (Path (source_or_build var, add_or_remove action, string_list pathes))
    | [`String "project"; `String "get"] ->
      request (Project_get)
    | [`String "version"] ->
      request (Version)
    | [`String "clear"] ->
      request (Clear)
    | [`String "noop"] ->
      request (Noop)
    | _ -> invalid_arguments ()

  let json_of_result = function
    | Failure s | Exception (Failure' s) ->
      "failure", `String s
    | Error error -> "error", error
    | Exception exn ->
      begin match error_catcher exn with
      | Some (_,error) -> "error", error
      | None -> "exception", `String (Printexc.to_string exn)
      end
    | Return (request, response) ->
      "success",
      begin match request, response with
        | Type_expr _, str -> `String str
        | Type_enclosing _, results ->
          `List (List.map ~f:json_of_type_loc results)
        | Enclosing, results ->
          `List (List.map ~f:Json.of_location results)
        | Complete_prefix _, compl ->
          json_of_completions compl
        | Expand_prefix _, compl ->
          json_of_completions compl
        | Document _, resp ->
          begin match resp with
          | `No_documentation -> `String "No documentation available"
          | `Invalid_context -> `String "Not a valid identifier"
          | `Not_found (id, None) -> `String ("didn't manage to find " ^ id)
          | `Not_found (i, Some f) ->
            `String
              (sprintf "%s was supposed to be in %s but could not be found" i f)
          | `Not_in_env str ->
            `String (Printf.sprintf "Not in environment '%s'" str)
          | `File_not_found msg ->
            `String msg
          | `Found doc ->
            `String doc
          end
        | Locate _, resp ->
          begin match resp with
          | `At_origin -> `String "Already at definition point"
          | `Invalid_context -> `String "Not a valid identifier"
          | `Not_found (id, None) -> `String ("didn't manage to find " ^ id)
          | `Not_found (i, Some f) ->
            `String
              (sprintf "%s was supposed to be in %s but could not be found" i f)
          | `Not_in_env str ->
            `String (Printf.sprintf "Not in environment '%s'" str)
          | `File_not_found msg ->
            `String msg
          | `Found (None,pos) ->
            `Assoc ["pos", Json.of_position pos]
          | `Found (Some file,pos) ->
            `Assoc ["file",`String file; "pos", Json.of_position pos]
          end
        | Case_analysis _, ({ Location. loc_start ; loc_end }, str) ->
          let assoc =
            `Assoc [
              "start", Json.of_position loc_start  ;
              "end", Json.of_position loc_end ;
            ]
          in
          `List [ assoc ; `String str ]
        | Outline, outlines ->
          `List (json_of_outline outlines)
        | Shape, shapes ->
          `List (List.map ~f:json_of_shape shapes)
        | Errors, errors ->
          `List (List.map ~f:json_of_error errors)
        | Dump _, json -> json
        | Which_path _, str -> `String str
        | Which_with_ext _, strs -> json_of_string_list strs
        | Flags_set _, failures ->
          `Assoc (with_failures ["result", `Bool true] failures)
        | Flags_get, flags ->
          `List (List.map json_of_string_list flags)
        | Findlib_use _, failures ->
          `Assoc (with_failures ["result", `Bool true] failures)
        | Findlib_list, strs -> json_of_string_list strs
        | Extension_list _, strs -> json_of_string_list strs
        | Extension_set _, failures ->
          `Assoc (with_failures ["result", `Bool true] failures)
        | Path _, () -> `Bool true
        | Path_list _, strs -> json_of_string_list strs
        | Path_reset, () -> `Bool true
        | Project_get, (strs, failures) ->
          `Assoc (with_failures ["result", json_of_string_list strs] failures)
        | Occurrences _, locations ->
          `List (List.map locations ~f:Json.of_location)
        | Idle_job, b -> `Bool b
        | Version, version ->
          `String version
        | Clear, () -> `Bool true
        | Noop, () -> `Bool true
      end

  let json_of_response {messages; result} =
    let kind, result = json_of_result result in
    `Assoc [
      "messages" , `List (List.map ~f:(fun x -> `String x) messages);
      "kind"     , `String kind;
      "result"   , result;
    ]

  let request_of_json json =
    command_of_json
      (context_of_json (member "context" json))
      (cursor_of_json (member "cursor" json))
      (synchronization_of_json (member "sync" json))
      (member "command" json |> to_list)
end

let lift (i,o : low_io) : io =
  (Stream.map ~f:Protocol_io.request_of_json i,
   (fun x -> o (Protocol_io.json_of_response x)))

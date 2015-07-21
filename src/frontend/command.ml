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
open Misc

open Merlin_lib

module Printtyp = Type_utils.Printtyp

let with_typer buffer f =
  let typer = Buffer.typer buffer in
  Typer.with_typer typer (fun () -> f typer)

let sync_kind (type a) (command : a Protocol.command) : [`None | `Cursor | `Full | `Any]
  = match command with
  | (Protocol.Type_expr _       : a Protocol.command) -> `Cursor
  | (Protocol.Type_enclosing _  : a Protocol.command) -> `Cursor
  | (Protocol.Enclosing         : a Protocol.command) -> `Cursor
  | (Protocol.Complete_prefix _ : a Protocol.command) -> `Cursor
  | (Protocol.Expand_prefix _   : a Protocol.command) -> `Cursor
  | (Protocol.Document _        : a Protocol.command) -> `Cursor
  | (Protocol.Locate _          : a Protocol.command) -> `Cursor
  | (Protocol.Noop              : a Protocol.command) -> `Cursor
  | (Protocol.Idle_job          : a Protocol.command) -> `Cursor
  | (Protocol.Case_analysis _   : a Protocol.command) -> `Full
  | (Protocol.Occurrences _     : a Protocol.command) -> `Full
  | (Protocol.Outline           : a Protocol.command) -> `Full
  | (Protocol.Shape             : a Protocol.command) -> `Full
  | (Protocol.Errors            : a Protocol.command) -> `Full
  | (Protocol.Dump _            : a Protocol.command) -> `Full
  | (Protocol.Which_path _      : a Protocol.command) -> `None
  | (Protocol.Which_with_ext _  : a Protocol.command) -> `None
  | (Protocol.Flags_set _       : a Protocol.command) -> `None
  | (Protocol.Flags_get         : a Protocol.command) -> `None
  | (Protocol.Project_get       : a Protocol.command) -> `None
  | (Protocol.Findlib_list      : a Protocol.command) -> `None
  | (Protocol.Findlib_use _     : a Protocol.command) -> `None
  | (Protocol.Extension_list _  : a Protocol.command) -> `None
  | (Protocol.Extension_set _   : a Protocol.command) -> `None
  | (Protocol.Path _            : a Protocol.command) -> `None
  | (Protocol.Path_list _       : a Protocol.command) -> `None
  | (Protocol.Path_reset        : a Protocol.command) -> `None
  | (Protocol.Version           : a Protocol.command) -> `None
  | (Protocol.Clear             : a Protocol.command) -> `None

let dispatch buffer ~verbosity ~cursor =
  fun (type a) (command : a Protocol.command) ->
  (match command with
  | (Protocol.Type_expr source : a Protocol.command) ->
    with_typer buffer @@ fun typer ->
    let env =
      let node, _ancestors = Browse.node_at typer cursor in
      node.BrowseT.t_env
    in
    let ppf, to_string = Format.to_string () in
    ignore (Type_utils.type_in_env ~verbosity env ppf source : bool);
    to_string ()

  | (Protocol.Type_enclosing expro : a Protocol.command) ->
    let open BrowseT in
    let open Typedtree in
    let open Override in
    with_typer buffer @@ fun typer ->
    let structures = Typer.contents typer in
    let structures = Browse.of_typer_contents structures in
    let path = Browse.enclosing cursor structures in
    let path = Browse.annotate_tail_calls_from_leaf path in
    let aux (t,tail) =
      let { t_loc ; t_env ; t_node ; _ } = t in
      match t_node with
      | Expression {exp_type = t}
      | Pattern {pat_type = t}
      | Core_type {ctyp_type = t}
      | Value_description { val_desc = { ctyp_type = t } } ->
        let ppf, to_string = Format.to_string () in
        Printtyp.wrap_printing_env t_env ~verbosity
          (fun () -> Type_utils.print_type_with_decl ~verbosity t_env ppf t);
        Some (t_loc, to_string (), tail)

      | Type_declaration { typ_id = id; typ_type = t} ->
        let ppf, to_string = Format.to_string () in
        Printtyp.wrap_printing_env t_env ~verbosity
          (fun () -> Printtyp.type_declaration t_env id ppf t);
        Some (t_loc, to_string (), tail)

      | Module_expr {mod_type = m}
      | Module_type {mty_type = m}
      | Module_binding {mb_expr = {mod_type = m}}
      | Module_declaration {md_type = {mty_type = m}}
      | Module_type_declaration {mtd_type = Some {mty_type = m}}
      | Module_binding_name {mb_expr = {mod_type = m}}
      | Module_declaration_name {md_type = {mty_type = m}}
      | Module_type_declaration_name {mtd_type = Some {mty_type = m}} ->
        let ppf, to_string = Format.to_string () in
        Printtyp.wrap_printing_env t_env ~verbosity
          (fun () -> Printtyp.modtype t_env ppf m);
        Some (t_loc, to_string (), tail)

      | _ -> None
    in
    let result = List.filter_map ~f:aux path in
    (* enclosings of cursor in given expression *)
    let exprs =
      match expro with
      | None ->
        let lexer = Buffer.lexer buffer in
        let lexer =
          History.seek_backward
            (fun (_,item) -> Lexing.compare_pos cursor (Lexer.item_start item) < 0)
            lexer
        in
        let path = Lexer.reconstruct_identifier lexer in
        let path = Lexer.identifier_suffix path in
        begin match path with
        | [] -> []
        | base :: tail ->
          let f {Location. txt=base; loc=bl} {Location. txt=dot; loc=dl} =
            let loc = Parsing_aux.location_union bl dl in
            let txt = base ^ "." ^ dot in
            Location.mkloc txt loc
          in
          [ List.fold_left tail ~init:base ~f ]
        end
      | Some (expr, offset) ->
        let loc_start =
          let l, c = Lexing.split_pos cursor in
          Lexing.make_pos (l, c - offset)
        in
        let shift loc int =
          let l, c = Lexing.split_pos loc in
          Lexing.make_pos (l, c + int)
        in
        let add_loc source =
          let loc =
            { Location.
              loc_start ;
              loc_end = shift loc_start (String.length source) ;
              loc_ghost = false ;
            } in
          Location.mkloc source loc
        in
        let len = String.length expr in
        let rec aux acc i =
          if i >= len then
            List.rev_map ~f:add_loc (expr :: acc)
          else if expr.[i] = '.' then
            aux (String.sub expr ~pos:0 ~len:i :: acc) (succ i)
          else
            aux acc (succ i) in
        aux [] offset
    in
    let small_enclosings =
      let node, _ = Browse.node_at typer cursor in
      let env = node.BrowseT.t_env in
      let include_lident = match node.BrowseT.t_node with
        | BrowseT.Pattern _ -> false
        | _ -> true
      in
      let include_uident = match node.BrowseT.t_node with
        | BrowseT.Module_binding _
        | BrowseT.Module_binding_name _
        | BrowseT.Module_declaration _
        | BrowseT.Module_declaration_name _
        | BrowseT.Module_type_declaration _
        | BrowseT.Module_type_declaration_name _
          -> false
        | _ -> true
      in
      List.filter_map exprs ~f:(fun {Location. txt = source; loc} ->
          match source with
          | "" -> None
          | source when not include_lident && Char.is_lowercase source.[0] ->
            None
          | source when not include_uident && Char.is_uppercase source.[0] ->
            None
          | source ->
            try
              let ppf, to_string = Format.to_string () in
              if Type_utils.type_in_env ~verbosity env ppf source then
                Some (loc, to_string (), `No)
              else
                None
            with _ ->
              None
        )
    in
    let normalize ({Location. loc_start; loc_end}, text, _tail) =
        Lexing.split_pos loc_start, Lexing.split_pos loc_end, text in
    let result = List.merge_cons (small_enclosings @ result)
        ~f:(fun a b ->
            (* Tail position is computed only on result, and result comes last
               As an approximation, when two items are similar, we returns the
               rightmost one *)
            if normalize a = normalize b then Some b else None)
    in
    List.map result ~f:(fun (location, text, is_tail) ->
        {Protocol.Type_enclosing. location; text; is_tail})

  | (Protocol.Enclosing : a Protocol.command) ->
    with_typer buffer @@ fun typer ->
    let structures = Typer.contents typer in
    let structures = Browse.of_typer_contents structures in
    let path = Browse.enclosing cursor structures in
    List.map (fun t -> t.BrowseT.t_loc) path

  | (Protocol.Complete_prefix (prefix, with_doc) : a Protocol.command) ->
    let complete ~no_labels typer =
      let node, ancestors = Browse.node_at ~skip_recovered:true typer cursor in
      let target_type, context =
        Completion.application_context ~verbosity ~prefix node ancestors
      in
      let get_doc =
        if not with_doc then None else
        let config     = Buffer.config buffer in
        let comments   = Buffer.comments buffer in
        let source     = Buffer.unit_name buffer in
        let local_defs = Typer.contents typer in
        Some (
          Track_definition.get_doc ~config ~env:node.BrowseT.t_env ~local_defs
            ~comments ~pos:cursor source
        )
      in
      let entries =
        Completion.node_complete ?get_doc ?target_type buffer node prefix
      and context = match context with
        | `Application context when no_labels ->
          `Application {context with Protocol.Completion.labels = []}
        | context -> context
      in
      {Protocol.Completion. entries = List.rev entries; context }
    in
    let lexer0 = Buffer.lexer buffer in
    let lexer =
      History.seek_backward
        (fun (_,item) -> Lexing.compare_pos cursor (Lexer.item_start item) <= 0)
        lexer0
    in
    let lexer =
      History.seek_forward ~last:true
        (fun (_,item) -> Lexing.compare_pos (Lexer.item_end item) cursor <= 0)
        lexer
    in
    let need_token, no_labels =
      let open Raw_parser in
      let exns, item = History.focused lexer in
      let loc = Lexer.item_location item in
      let need_token =
        if Parsing_aux.compare_pos cursor loc = 0 &&
           (match item with
            | Lexer.Valid (_, (LIDENT _ | UIDENT _), _) -> true
            | _ -> false)
        then
          None
        else
          Some (exns, Lexer.Valid (cursor, LIDENT "", cursor))
      and no_labels =
        (* Cursor is already over a label, don't suggest another one *)
        match item with
        | Lexer.Valid (_, (LABEL _ | OPTLABEL _), _) -> true
        | _ -> false
      in
      need_token, no_labels
    in
    begin match need_token with
    | None -> with_typer buffer (complete ~no_labels)
    | Some token ->
      (* Setup fake AST *)
      let lexer' = History.fake_insert token lexer in
      let lexer' = History.seek (History.position lexer0 + 1) lexer' in
      ignore (Buffer.update buffer lexer' : [> ]);
      try_finally
        (* Complete on adjusted buffer *)
        (fun () -> with_typer buffer (complete ~no_labels))
        (* Restore original buffer *)
        (fun () -> ignore (Buffer.update buffer lexer0 : [> ]))
    end

  | (Protocol.Expand_prefix prefix : a Protocol.command) ->
    with_typer buffer @@ fun typer ->
    let env =
      let node, _ = Browse.node_at typer cursor in
      node.BrowseT.t_env
    in
    let global_modules = Buffer.global_modules buffer in
    let entries = Completion.expand_prefix env ~global_modules prefix in
    { Protocol.Completion. entries ; context = `Unknown }

  | (Protocol.Document patho : a Protocol.command) ->
    with_typer buffer @@ fun typer ->
    let comments = Buffer.comments buffer in
    let env, local_defs =
      let node, _ = Browse.node_at typer cursor in
      node.BrowseT.t_env, Typer.contents typer
    in
    let path =
      match patho with
      | Some p -> p
      | None ->
        let lexer = Buffer.lexer buffer in
        let lexer =
          History.seek_backward (fun (_,item) ->
            Lexing.compare_pos cursor (Lexer.item_start item) < 0) lexer
        in
        let path = Lexer.reconstruct_identifier ~for_locate:true lexer in
        let path = Lexer.identifier_suffix path in
        let path = List.map ~f:(fun {Location. txt} -> txt) path in
        String.concat ~sep:"." path
    in
    if path = "" then `Invalid_context else
    let source  = Buffer.unit_name buffer in
    let config = Buffer.config buffer in
    Track_definition.get_doc ~config ~env ~local_defs ~comments ~pos:cursor source
      (`User_input path)

  | (Protocol.Locate (patho, ml_or_mli) : a Protocol.command) ->
    with_typer buffer @@ fun typer ->
    let env, local_defs =
      let node, _ = Browse.node_at typer cursor in
      node.BrowseT.t_env, Typer.contents typer
    in
    let path =
      match patho with
      | Some p -> p
      | None ->
        let lexer = Buffer.lexer buffer in
        let lexer =
          History.seek_backward (fun (_,item) ->
            Lexing.compare_pos cursor (Lexer.item_start item) < 0) lexer
        in
        let path = Lexer.reconstruct_identifier ~for_locate:true lexer in
        let path = Lexer.identifier_suffix path in
        let path = List.map ~f:(fun {Location. txt} -> txt) path in
        String.concat ~sep:"." path
    in
    if path = "" then `Invalid_context else
    let config = Buffer.config buffer in
    begin match
      Track_definition.from_string ~config ~env ~local_defs ~pos:cursor ml_or_mli path
    with
    | `Found (file, pos) ->
      Logger.info (Track_definition.section)
        (Option.value ~default:"<local buffer>" file);
      `Found (file, pos)
    | otherwise -> otherwise
    end

  | (Protocol.Case_analysis ({ Location. loc_start ; loc_end } as loc) : a Protocol.command) ->
    with_typer buffer @@ fun typer ->
    let env = Typer.env typer in
    Printtyp.wrap_printing_env env ~verbosity @@ fun () ->
    let structures = Typer.contents typer in
    let structures = Browse.of_typer_contents structures in
    let enclosings = Browse.enclosing loc_start structures in
    begin match
        List.drop_while enclosings ~f:(fun t ->
            Lexing.compare_pos t.BrowseT.t_loc.Location.loc_end loc_end < 0
          )
      with
      | [] -> failwith "No node at given range"
      | node :: parents -> Destruct.node ~loc parents node
    end

  | (Protocol.Outline : a Protocol.command) ->
    with_typer buffer @@ fun typer ->
    let typed_tree = Typer.contents typer in
    Outline.get (Browse.of_typer_contents typed_tree)

  | (Protocol.Shape : a Protocol.command) ->
    with_typer buffer @@ fun typer ->
    let typed_tree = Typer.contents typer in
    Outline.shape cursor (Browse.of_typer_contents typed_tree)

  | (Protocol.Errors : a Protocol.command) ->
    begin
      with_typer buffer @@ fun typer ->
      Printtyp.wrap_printing_env (Typer.env typer) ~verbosity @@ fun () ->
      try
        let typer = Buffer.typer buffer in
        let cmp (l1,_) (l2,_) =
          Lexing.compare_pos l1.Location.loc_start l2.Location.loc_start in
        let err exns =
          List.filter ~f:(fun (l,err) ->
            not l.Location.loc_ghost || err.Error_report.where <> "warning"
          ) @@
          List.sort_uniq ~cmp (List.map ~f:Error_report.of_exn exns)
        in
        let err_lexer  = err (Buffer.lexer_errors buffer) in
        let err_parser = err (Buffer.parser_errors buffer) in
        let err_typer  =
          (* When there is a cmi error, we will have a lot of meaningless errors,
           * there is no need to report them. *)
          let exns = Typer.exns typer @ Typer.delayed_checks typer in
          let exns =
            let cmi_error = function Cmi_format.Error _ -> true | _ -> false in
            try [ List.find exns ~f:cmi_error ]
            with Not_found -> exns
          in
          err exns
        in
        (* Return parsing warnings & first parsing error,
           or type errors if no parsing errors *)
        let rec extract_warnings acc = function
          | (_,{Error_report. where = "warning"; _ }) as err :: errs ->
            extract_warnings (err :: acc) errs
          | err :: _ ->
            List.rev (err :: acc),
            List.take_while ~f:(fun err' -> cmp err' err < 0) err_typer
          | [] ->
            List.rev acc, err_typer in
        (* Filter duplicate error messages *)
        let err_parser, err_typer = extract_warnings [] err_parser in
        let errors =
          List.map ~f:snd @@
          List.merge ~cmp err_lexer @@
          List.merge ~cmp err_parser err_typer
        in
        Error_report.flood_barrier errors
      with exn -> match Error_report.strict_of_exn exn with
        | None -> raise exn
        | Some (_loc, err) -> [err]
    end

  | (Protocol.Dump `Parser : a Protocol.command) ->
    Merlin_recover.dump (Buffer.recover buffer);

  | (Protocol.Dump (`Typer `Input) : a Protocol.command) ->
    with_typer buffer @@ fun typer ->
    let ppf, to_string = Format.to_string () in
    Typer.dump ppf typer;
    `String (to_string ())

  | (Protocol.Dump (`Typer `Output) : a Protocol.command) ->
    with_typer buffer @@ fun typer ->
    let ppf, to_string = Format.to_string () in
    List.iter (fun (content,_) -> match content with
        | `Sg sg -> Printtyped.interface ppf sg
        | `Str str -> Printtyped.implementation ppf str
        | `Fail (_,loc) ->
          Format.fprintf ppf "<failed to type at %a>\n"
            Location.print loc
      ) (Typer.contents typer);
    `String (to_string ())

  | (Protocol.Dump `Recover : a Protocol.command) ->
    Merlin_recover.dump_recoverable (Buffer.recover buffer);

  | (Protocol.Dump (`Env kind) : a Protocol.command) ->
    with_typer buffer @@ fun typer ->
    let env =
      let node, _ = Browse.node_at typer cursor in
      node.BrowseT.t_env
    in
    let sg = Browse_misc.signature_of_env ~ignore_extensions:(kind = `Normal) env in
    let aux item =
      let ppf, to_string = Format.to_string () in
      Printtyp.signature ppf [item];
      let content = to_string () in
      let ppf, to_string = Format.to_string () in
      match Raw_compat.signature_loc item with
      | Some loc ->
        Location.print_loc ppf loc;
        let loc = to_string () in
        `List [`String loc ; `String content]
      | None -> `String content
    in
    `List (List.map ~f:aux sg)

  | (Protocol.Dump `Browse : a Protocol.command) ->
    with_typer buffer @@ fun typer ->
    let structures = Typer.contents typer in
    let structures = Browse.of_typer_contents structures in
    Browse_misc.dump_ts structures

  | (Protocol.Dump `Tokens : a Protocol.command) ->
    let tokens = Buffer.lexer buffer in
    let tokens = History.seek_backward (fun _ -> true) tokens in
    let tokens = History.tail tokens in
    `List (List.filter_map tokens
             ~f:(fun (_exns,item) -> match item with
             | Lexer.Error _ -> None
             | Lexer.Valid (s,t,e) ->
               let t = Raw_parser_values.symbol_of_token t in
               let t = Raw_parser_values.class_of_symbol t in
               let t = Raw_parser_values.string_of_class t in
               Some (`Assoc [
                   "start", Json.of_position s;
                   "end", Json.of_position e;
                   "token", `String t;
                 ])
             )
          )

  (*| (Protocol.Dump `Flags : a Protocol.command) ->
    let flags = Config.get_flags (Buffer.config buffer) in
    let assoc =
      List.map flags ~f:(fun (src, flag_lists) ->
        let l = List.concat_map flag_lists ~f:(List.map ~f:(fun s -> `String s)) in
        src, `List l
      )
    in
    `Assoc assoc*)

  | (Protocol.Dump `Warnings : a Protocol.command) ->
    with_typer buffer @@ fun _typer ->
    Warnings.dump ()

  | (Protocol.Dump `Exn : a Protocol.command) ->
    with_typer buffer @@ fun typer ->
    let exns =
      Typer.exns typer
      @ Buffer.lexer_errors buffer
      @ Buffer.parser_errors buffer
    in
    `List (List.map ~f:(fun x -> `String (Printexc.to_string x)) exns)


  | (Protocol.Dump _ : a Protocol.command) ->
    failwith "TODO"

  | (Protocol.Which_path xs : a Protocol.command) ->
    begin
      let config = Buffer.config buffer in
      let rec aux = function
        | [] -> raise Not_found
        | x :: xs ->
          try
            find_in_path_uncap (Config.source_path config) x
          with Not_found -> try
            find_in_path_uncap (Config.build_path config) x
          with Not_found ->
            aux xs
      in
      aux xs
    end

  | (Protocol.Which_with_ext exts : a Protocol.command) ->
    let config = Buffer.config buffer in
    let path = Path_list.to_strict_list (Config.source_path config) in
    let with_ext ext = modules_in_path ~ext path in
    List.concat_map ~f:with_ext exts

  | (Protocol.Flags_set flags : a Protocol.command) ->
    failwith "TODO"
    (*let config = Buffer.config buffer in
    ignore (Merlin_lib.Config.User.clear_flags config);
    Merlin_lib.Config.User.add_flags config flags*)

  | (Protocol.Flags_get : a Protocol.command) ->
    failwith "TODO"
    (*let config = Buffer.config buffer in
    Merlin_lib.Config.User.get_flags config*)

  | (Protocol.Project_get : a Protocol.command) ->
    let config = Buffer.config buffer in
    (Config.dot_merlins config,
     match Config.dot_failures config with
     | [] -> `Ok
     | failures -> `Failures failures)

  | (Protocol.Findlib_list : a Protocol.command) ->
    Fl_package_base.list_packages ()

  | (Protocol.Findlib_use packages : a Protocol.command) ->
    failwith "TODO"
    (*let config = Buffer.config buffer in
    Config.User.load_packages config packages*)

  | (Protocol.Extension_list kind : a Protocol.command) ->
    let config = Buffer.config buffer in
    let enabled = Config.extensions config in
    let set = match kind with
      | `All -> Extension.all
      | `Enabled -> enabled
      | `Disabled -> String.Set.diff Extension.all enabled
    in
    String.Set.to_list set

  | (Protocol.Extension_set (action,exts) : a Protocol.command) ->
    failwith "TODO"
    (*let enabled = match action with
      | `Enabled  -> true
      | `Disabled -> false
    in
    let config = Buffer.config buffer in
    begin match
      List.filter_map exts ~f:(Config.User.set_extension config ~enabled)
    with
    | [] -> `Ok
    | lst -> `Failures lst
    end*)

  | (Protocol.Path (var,action,paths) : a Protocol.command) ->
    failwith "TODO"
    (*let config = Buffer.config buffer in
    List.iter paths ~f:(Config.User.path config ~action ~var ?cwd:None)*)

  | (Protocol.Path_list `Build : a Protocol.command) ->
    let config = Buffer.config buffer in
    Path_list.to_strict_list (Config.build_path config)

  | (Protocol.Path_list `Source : a Protocol.command) ->
    let config = Buffer.config buffer in
    Path_list.to_strict_list (Config.source_path config)

  | (Protocol.Path_reset : a Protocol.command) ->
    failwith "TODO"
    (*let config = Buffer.config buffer in
    Config.User.reset config*)

  | (Protocol.Occurrences `Ident : a Protocol.command) ->
    with_typer buffer @@ fun typer ->
    let str = Typer.contents typer in
    let str = Browse.of_typer_contents str in
    let node = match Browse.enclosing cursor str with
      | node :: _ -> node
      | [] -> BrowseT.dummy
    in
    let get_loc {Location.txt = _; loc} = loc in
    let ident_occurrence () =
      let paths = BrowseT.node_paths node.BrowseT.t_node in
      let under_cursor p = Parsing_aux.compare_pos cursor (get_loc p) = 0 in
      Logger.infojf (Logger.section "occurences") ~title:"Occurrences paths"
        (fun paths ->
          let dump_path ({Location.txt; loc} as p) =
            let ppf, to_string = Format.to_string () in
            Printtyp.path ppf txt;
            Json.with_location loc [
              "under_cursor", `Bool (under_cursor p);
              "path", `String (to_string ())
            ]
          in
          `List (List.map ~f:dump_path paths)
        ) paths;
      match List.filter paths ~f:under_cursor with
      | [] -> []
      | (path :: _) ->
        let path = path.Location.txt in
        let ts = List.concat_map ~f:(Browse.all_occurrences path) str in
        let loc (_t,paths) = List.map ~f:get_loc paths in
        List.concat_map ~f:loc ts

    and constructor_occurrence d =
      let ts = List.concat_map str
          ~f:(Browse.all_constructor_occurrences (node,d)) in
      List.map ~f:get_loc ts

    in
    let locs = match BrowseT.is_constructor node with
      | Some d -> constructor_occurrence d.Location.txt
      | None -> ident_occurrence ()
    in
    let loc_start l = l.Location.loc_start in
    let cmp l1 l2 = Lexing.compare_pos (loc_start l1) (loc_start l2) in
    List.sort ~cmp locs

  | (Protocol.Version : a Protocol.command) ->
    Main_args.version_spec

  | (Protocol.Idle_job : a Protocol.command) ->
    Buffer.idle_job buffer

  | (Protocol.Clear : a Protocol.command) ->
    assert false

  | (Protocol.Noop : a Protocol.command) ->
    ()

  : a)

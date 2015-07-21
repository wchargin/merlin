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

(* Stateful parts:
   - lexer keywords, done Lexer
   - typer snapshot & env, done Typer
   - compiler path, done Project
   - compiler flags, done Project
*)
module Lexer  = Merlin_lexer
module Parser = Merlin_parser
module Typer  = Merlin_typer
module Recover = Merlin_recover

(* Compute and cache configuration (paths, extensions, ...) *)
module Config : sig
  type t

  val create : dot_merlins:string list -> local_path:string list -> t

  val dot_merlins : t -> string list
  val set_user_config : t -> Dot_merlin.config -> unit

  val dot_failures : t -> (string * exn) list
  val user_failures : t -> (string * exn) list

  (* Path configuration *)
  val source_path : t -> Path_list.t
  val build_path  : t -> Path_list.t
  val cmt_path    : t -> Path_list.t

  (* List all top modules  *)
  val global_modules : t -> string list

  (* Enabled extensions *)
  val extensions: t -> Extension.set

  (* Lexer keywords for current config *)
  val keywords: t -> Lexer.keywords

  (* Make global state point to current config *)
  val setup : t -> unit

  val check_disk : t -> unit
  val stamp : t -> bool ref
end = struct

  type t = {
    mutable dot_merlin : Dot_merlin.t * Dot_merlin.config;
    mutable dot_failures : (string * exn) list;

    mutable user_config : Dot_merlin.config;
    mutable user_failures : (string * exn) list;

    flags : Clflags.set;
    warnings : Warnings.state;

    local_path  : string list;

    source_path : string list ref;
    build_path  : string list ref;
    cmt_path    : string list ref;

    mutable extensions : Extension.set;
    mutable keywords : Lexer.keywords;
    mutable stamp : bool ref;
  }

  let create ~dot_merlins ~local_path =
    let dot = Dot_merlin.load dot_merlins in
    {
      dot_merlin = (dot, Dot_merlin.config dot);
      dot_failures = [];

      user_config = Dot_merlin.empty_config;
      user_failures = [];

      flags = Clflags.copy Clflags.initial;
      warnings = Warnings.copy Warnings.initial;

      local_path;

      source_path = ref [];
      build_path  = ref [];
      cmt_path    = ref [];

      extensions = Extension.empty;
      keywords = Lexer.default_keywords;

      stamp = ref false;
    }

  let compute_flags t ~dot ~user =
    Clflags.reset t.flags ~from:Clflags.initial;
    Warnings.reset t.warnings ~from:Warnings.initial;
    let spec =
      Clflags.arg_spec t.flags @
      Warnings.arg_spec t.warnings
    in
    let process_flags spec flags =
      let failures = ref [] in
      let rec loop ?(current=(ref 0)) flags =
        try Arg.parse_argv ~current flags spec (fun flg -> raise (Arg.Bad flg)) "" with
        | Arg.Bad _ ->
          Logger.info Logger.Section.project_load ~title:"flags"
            (sprintf "unknown flag: %s" flags.(!current));
          failures := (flags.(!current), Arg.Bad flags.(!current)) :: !failures ;
          loop ~current flags
        | Arg.Help _ -> (* ignore *)
          loop ~current flags
      in
      loop flags;
      !failures
    in
    let process_flags_list lst ~init =
      List.fold_left lst ~init ~f:(fun acc lst ->
        let flags = Array.of_list ("merlin" :: lst) in
        List.rev_append (process_flags spec flags) acc
      )
    in
    let dfailures = process_flags_list dot.Dot_merlin.flags ~init:[] in
    let dfailures = List.rev_append (process_flags (Main_args.flags @ spec) Sys.argv) dfailures in
    let ufailures = process_flags_list user.Dot_merlin.flags ~init:[] in
    dfailures, ufailures

  let refresh t =
    let dm = fst t.dot_merlin in
    let d = Dot_merlin.config dm and u = t.user_config in
    t.dot_merlin <- (dm, d);
    let `Failures derrors, dpkgs, dppxs =
      Dot_merlin.path_of_packages d.Dot_merlin.packages
    and `Failures uerrors, upkgs, uppxs =
      Dot_merlin.path_of_packages u.Dot_merlin.packages
    in
    let derrors', uerrors' = compute_flags t ~dot:d ~user:u in
    t.dot_failures <- derrors' @ derrors;
    t.user_failures <- uerrors' @ uerrors;
    t.source_path := List.flatten [
        u.Dot_merlin.source_path;
        t.local_path;
        d.Dot_merlin.source_path;
        upkgs;
        dpkgs;
      ];
    t.build_path := List.flatten [
        u.Dot_merlin.cmi_path;
        u.Dot_merlin.build_path;
        t.local_path;
        d.Dot_merlin.cmi_path;
        d.Dot_merlin.build_path;
        upkgs;
        dpkgs;
        !(t.flags.Clflags.include_dirs);
        !(t.flags.Clflags.std_include);
      ];
    t.cmt_path := List.flatten [
        u.Dot_merlin.source_path;
        t.local_path;
        d.Dot_merlin.source_path;
        upkgs;
        dpkgs;
        !(t.flags.Clflags.include_dirs);
        !(t.flags.Clflags.std_include);
      ];
    t.flags.Clflags.ppx <-
      Ppxsetup.(union t.flags.Clflags.ppx (union dppxs uppxs));
    t.extensions <-
      String.Set.union
        (String.Set.of_list (u.Dot_merlin.extensions @ d.Dot_merlin.extensions))
        (Extension.from_packages (u.Dot_merlin.packages @ d.Dot_merlin.packages));
    t.keywords <- Extension.keywords t.extensions

  let refresh t =
    if not !(t.stamp) then
      (refresh t; t.stamp <- ref true)

  let check_disk t =
    let dm, c = t.dot_merlin in
    Dot_merlin.update dm;
    if not (Dot_merlin.same (Dot_merlin.config dm) c) then
      t.stamp := false

  let stamp t =
    refresh t;
    assert !(t.stamp);
    t.stamp

  let global_modules t =
    refresh t;
    Misc.modules_in_path ~ext:".cmi" !(t.build_path)

  let extensions t =
    refresh t;
    t.extensions

  let keywords t =
    refresh t;
    t.keywords

  let source_path t =
    refresh t;
    Path_list.of_string_list_ref t.source_path

  let build_path t =
    refresh t;
    Path_list.of_string_list_ref t.build_path

  let cmt_path t =
    refresh t;
    Path_list.of_string_list_ref t.cmt_path

  (* Make global state point to current config *)
  let setup t =
    refresh t;
    Clflags.set := t.flags;
    Warnings.current := t.warnings;
    Config.load_path := Path_list.of_string_list_ref t.build_path

  let dot_merlins t =
    refresh t; (snd t.dot_merlin).Dot_merlin.dot_merlins
  let dot_failures t =
    refresh t; t.dot_failures

  let user_failures t =
    refresh t; t.user_failures

  let set_user_config t config =
    t.user_config <- config;
    t.stamp := false
end

module Buffer : sig
  type t
  val create: ?dot_merlins:string list -> ?path:string -> Parser.state -> t

  val unit_name: t -> string
  val config: t -> Config.t

  val lexer: t -> (exn list * Lexer.item) History.t
  val update: t -> (exn list * Lexer.item) History.t -> unit
  val start_lexing: t -> Lexer.t
  val lexer_errors: t -> exn list

  val comments: t -> (string * Location.t) list

  val parser: t -> Parser.t
  val parser_errors: t -> exn list

  val recover: t -> Recover.t
  val recover_history : t -> (Lexer.item * Recover.t) History.t

  val typer: t -> Typer.t

  val get_mark: t -> Parser.frame option
  val has_mark: t -> Parser.frame option -> bool

  val is_implementation : t -> bool

  (* All top modules, with current module removed *)
  val global_modules: t -> string list

  (* Try to do a background job, return false if nothing has to be done *)
  val idle_job : t -> bool
end = struct
  type t = {
    kind: Parser.state;
    path: string option;
    dot_merlins: string list;
    unit_name : string;
    mutable config : Config.t;
    mutable stamp : bool ref;
    mutable lexer: (exn list * Lexer.item) History.t;
    mutable recover: (Lexer.item * Recover.t) History.t;
    mutable typer: Typer.t;
  }

  let is_implementation { kind ; _ } = kind = Parser.implementation

  let initial_step kind (_,token) =
    let input = match token with
      | Lexer.Valid (s,t,e) -> s,t,e
      | _ -> assert false
    in
    (token, Recover.fresh (Parser.from kind input))

  let setup config =
    Config.check_disk config;
    Config.setup config

  let create ?(dot_merlins=[]) ?path kind =
    let path, filename = match path with
      | None -> None, "*buffer*"
      | Some path -> Some (Filename.dirname path), Filename.basename path
    in
    let dot_merlins = match dot_merlins, path with
      | [], Some path -> [path]
      | [], None -> []
      | xs, cwd -> List.map ~f:(Misc.canonicalize_filename ?cwd) xs
    in
    let unit_name =
      try String.sub filename ~pos:0 ~len:(String.index filename '.')
      with Not_found -> filename
    in
    let unit_name = String.capitalize unit_name in
    let lexer = Lexer.empty ~filename in
    let config = Config.create ~dot_merlins ~local_path:(Option.to_list path) in
    let stamp = ref true in
    Config.setup config;
    {
      dot_merlins; path; config; lexer; kind; unit_name; stamp;
      keywords = Config.keywords config;
      recover = History.initial (initial_step kind (History.focused lexer));
      typer = Typer.fresh
          ~unit_name ~stamp:[Config.stamp config; stamp]
          (Config.extensions config);
    }

  let config t = t.config
  let setup t = setup (config t)
  let unit_name t = t.unit_name
  let lexer b = b.lexer
  let lexer_errors b = fst (History.focused b.lexer)
  let recover_history b = b.recover
  let recover b = snd (History.focused b.recover)
  let comments b = Recover.comments (recover b)
  let parser b = Recover.parser (recover b)
  let parser_errors b = Recover.exns (recover b)

  let typer b =
    setup b;
    let valid = Typer.is_valid b.typer &&
                String.Set.equal
                  (Typer.extensions b.typer)
                  (Config.extensions b.config) in
    if not valid then
      b.typer <- Typer.fresh
          ~unit_name:b.unit_name
          ~stamp:[Config.stamp b.config; b.stamp]
          (Config.extensions b.config);
    b.typer <- Typer.update (parser b) b.typer;
    b.typer

  let update t l =
    t.lexer <- l;
    let strong_check (_,token) (token',_) = token == token' in
    let weak_check (_,token) (token',_) = Lexer.equal token token' in
    let init token = initial_step t.kind token in
    let strong_fold (_,token) (_,recover) = token, Recover.fold token recover in
    let weak_update (_,token) (_,recover) = (token,recover) in
    let recover', _updated = History.sync t.lexer (Some t.recover)
        ~init ~strong_check ~strong_fold ~weak_check ~weak_update in
    t.recover <- recover';

  let start_lexing b = let kw = Config.keywords b.config in
    Lexer.start kw (History.seek_backward (fun _ -> true) b.lexer)

  let get_mark t = Parser.find_marker (parser t)

  let has_mark t = function
    | None -> false
    | Some frame -> Parser.has_marker (parser t) frame

  let global_modules t =
    setup t;
    List.remove t.unit_name (Config.global_modules t.config)

  exception Break
  let idle_job t =
    Typer.with_typer (typer t) @@ fun () ->
    Clflags.real_paths () <> `Real &&
    let concr = Env.used_persistent () in
    Types.Concr.exists Printtyp.compute_map_for_pers concr
end

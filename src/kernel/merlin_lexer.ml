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
let empty ~filename state =
  let pos =
    { Lexing.
      pos_fname = filename;
      pos_lnum  = 1;
      pos_bol   = 0;
      pos_cnum  = 0;
    }
  in
  History.initial ([], Valid (pos, Raw_parser.ENTRYPOINT, pos, Some state))

type ('state, 'lexer) lexer' =
  (module S with type state = 'state and type t = 'lexer)

type ('state, 'lexer) t' = {
  (* Result *)
  mutable history: (exn list * 'state item) History.t;
  (* Lexer *)
  klass: ('state, 'lexer) lexer';
  lexer: 'lexer;
}

type 'state t = T : ('state, 'lexer) t' -> 'state t

let history (T t) = t.history

(** Prepare for lexing.
    Returns the start position (end position of last valid token), and a
    lexing function that will append at most one token to the history at each
    call. *)
let make_lexbuf empty refill position =
  Lexing.from_strings ~position ~empty ""
    (fun () ->
       match !refill with
       | Some s -> refill := None; s
       | None -> "")

let start (type a) (type t) (klass : (t,a) lexer') history =
  let module Lexer = (val klass) in
  let seek_start = function
    | _, Valid (_,_,_,Some _) -> false
    | _ -> true
  in
  let history = History.seek_backward seek_start history in
  let position, state = match History.focused history with
    | _, Valid (_,_,p,Some state) -> p, state
    | _ -> assert false
  in
  {
    history; klass;
    lexer = Lexer.start position state;
  }
let start (type t) (type s) (klass : s lexer) history =
  let module Lexer = (val klass) in
  T (start (module Lexer : S with type state = s and type t = Lexer.t) history)

let position (type s) (type k) (t : (s,k) t') =
  let module Lexer = (val t.klass) in
  Lexer.position t.lexer
let position (T t) = position t

let eof (type s) (type k) (t : (s,k) t') =
  let module Lexer = (val t.klass) in
  Lexer.eof t.lexer
let eof (T t) = eof t

let feed (type s) (type t) (l : (s,t) lexer') (t : t) str f =
  let module Lexer = (val l) in
  Lexer.feed t str f

let feed (T t) str =
  let warnings = ref (fst (History.focused t.history)) in
  Parsing_aux.catch_warnings warnings @@ fun () ->
  let append item =
    begin match item with
      | Error (e,l) -> warnings := Raw_lexer.Error (e,l) :: !warnings
      | _ -> ()
    end;
    t.history <- History.insert (!warnings, item) t.history
  in
  feed t.klass t.lexer str append

module Caml_lexer = struct
  type at_beginning = bool
  type state = at_beginning * Raw_lexer.keywords
  let from_keywords s = true, s

  let equal (b1,kw1) (b2,kw2) = b1 = b2 && kw1 == kw2

  type t =
    {
      mutable state: state;
      (* Input buffer *)
      refill: string option ref; (* Input not yet sent to lexer *)
      refill_empty: bool ref;    (* Lexer internal buffer status *)
      mutable pushed_eof: bool;
      (* Lexer data *)
      lex_state: Raw_lexer.state;
      lexbuf: Lexing.lexbuf;
      mutable resume: (unit -> Raw_parser.token Raw_lexer.result) option;
    }

  let seek t pos = t.lexbuf.Lexing.lex_curr_p <- pos
  let position t = Lexing.immediate_pos t.lexbuf
  let eof t = t.lexbuf.Lexing.lex_eof_reached

  let start position (_, keywords as state) =
    let refill = ref None in
    let refill_empty = ref true in
    let lexbuf = make_lexbuf refill_empty refill position in
    {
      state;
      lex_state = Raw_lexer.make keywords;
      pushed_eof = false;
      resume = None; refill; refill_empty; lexbuf;
    }

  let on_append t =
    match t.state with
    | true, kw -> t.state <- false, kw
    | _ -> ()

  let feed t str append =
    t.refill := Some str;
    let rec aux = function
      (* Lexer interrupted, there is data to refill or eof reached: continue. *)
      | Raw_lexer.Refill f
        when !(t.refill) <> None || not !(t.refill_empty) || str = "" ->
        aux (f ())
      (* Lexer interrupted, nothing to refill, return to caller. *)
      | Raw_lexer.Refill r ->
        t.resume <- Some r
      (* EOF Reached: notify EOF to parser, stop now *)
      | Raw_lexer.Return Raw_parser.EOF when t.pushed_eof ->
        ()
      | Raw_lexer.Return Raw_parser.EOF ->
        on_append t;
        append (Valid (t.lexbuf.Lexing.lex_start_p,
                       Raw_parser.EOF,
                       t.lexbuf.Lexing.lex_curr_p,
                       None));
        t.pushed_eof <- true
      | Raw_lexer.Return token ->
        on_append t;
        append (Valid (t.lexbuf.Lexing.lex_start_p,
                       token,
                       t.lexbuf.Lexing.lex_curr_p,
                       Some t.state));
        continue ()
      | Raw_lexer.Fail (e,l) ->
        append (Error (e,l));
        continue ()
    and continue () =
      aux (Raw_lexer.token t.lex_state t.lexbuf)
    in
    match t.resume with
    (* At the beginning *)
    | None when fst t.state ->
      aux (Raw_lexer.skip_sharp_bang t.lex_state t.lexbuf)
    (* Next token *)
    | None -> continue ()
    (* Resume *)
    | Some f ->
      t.resume <- None;
      aux (f ())

end

let caml_lexer : _ lexer = (module Caml_lexer)

(* Miscellaneous functions *)

let item_equal (type a) (lexer : a lexer) (it1 : a item) (it2 : a item) =
  let module Lexer = (val lexer) in
  match it1, it2 with
  | Valid (s1,t1,e1,_), Valid (s2,t2,e2,_) ->
    Lexing.compare_pos s1 s2 = 0 &&
    Lexing.compare_pos e1 e2 = 0 &&
    t1 = t2
  | Error (v1,l1), Error (v2,l2) ->
    Lexing.compare_pos l1.Location.loc_start l2.Location.loc_start = 0 &&
    Lexing.compare_pos l1.Location.loc_end l2.Location.loc_end = 0 &&
    v1 = v2
  | _ -> false

let item_start (Valid (p,_,_,_) | Error (_,{Location. loc_start = p})) = p
let item_end (Valid (_,_,p,_) | Error (_,{Location. loc_end = p})) = p

let token is = function
  | Valid (_,op,_,_) -> (is op <> None)
  | _ -> false

let extract_op for_locate = function
  | Error _ -> assert false
  | Valid (s,t,e,_) ->
    let t = Option.get (Raw_parser_values.is_operator t) in
    let t = if for_locate then t else "(" ^ t ^ ")" in
    Location.mkloc t {Location. loc_start = s; loc_end = e; loc_ghost = false}

let extract_ident = function
  | Error _ -> assert false
  | Valid (s,t,e,_) ->
    let t =
      match Raw_parser_values.is_ident t with
      | Some t -> t
      | None ->
        match Raw_parser_values.is_operator t with
        | Some t -> "( " ^ t ^ " )"
        | None -> assert false
    in
    Location.mkloc t {Location. loc_start = s; loc_end = e; loc_ghost = false}

let reconstruct_identifier ?(for_locate=false) h =
  (*List.iter (fun (_,item) ->
      match item with
      | Valid (_,tok,_) ->
        let sym = Raw_parser_values.symbol_of_token tok in
        let cls = Raw_parser_values.class_of_symbol sym in
        prerr_endline (Raw_parser_values.string_of_class cls)
      | _ -> () ) (History.tail h);*)
  let h = match History.focused h with
    | _, Valid (_,Raw_parser.DOT,_,_) -> History.move 1 h
    | _ -> h
  in
  match History.head h with
  | List.One (_, op) when token Raw_parser_values.is_operator op ->
    [ extract_op for_locate op ]
  | List.More ((_, op), (List.More ((_, rest), _) | List.One (_, rest)))
    when token Raw_parser_values.is_operator op
    && not (token Raw_parser_values.is_lparen rest) ->
    [ extract_op for_locate op ]
  | _ ->
    let acc, h = match History.head h, History.tail h with
      | (List.More((_, ident), _) | List.One (_, ident)), _
        when token Raw_parser_values.is_ident ident -> [ident], h
      | ( List.More ((_, Valid (_,Raw_parser.LPAREN,_,_)), _)
        | List.One (_, Valid (_,Raw_parser.LPAREN,_,_))),
        (_, op) :: (_, Valid (_,Raw_parser.RPAREN,_,_)) :: _
        when token Raw_parser_values.is_operator op -> [op], h
      | List.More ((_, op),
                  ( List.More ((_, Valid (_,Raw_parser.LPAREN,_,_)), _)
                  | List.One (_, Valid (_,Raw_parser.LPAREN,_,_)))),
        (_, Valid (_,Raw_parser.RPAREN,_,_)) :: _
        when token Raw_parser_values.is_operator op -> [op], History.move (-1) h
      | List.More ((_, Valid (_,Raw_parser.RPAREN,_,_)),
                  List.More ((_, op),
                              ( List.More ((_, Valid (_,Raw_parser.LPAREN,_,_)), _)
                              | List.One (_, Valid (_,Raw_parser.LPAREN,_,_))))),
        _
        when token Raw_parser_values.is_operator op -> [op], History.move (-2) h
      | _ -> [], h
    in
    let h = History.move (-1) h in
    let rec head acc = function
      | List.More ((_, Valid (_,Raw_parser.DOT,_,_)),
                  List.More ((_, ident), tl))
        when token Raw_parser_values.is_ident ident -> head (ident :: acc) tl
      | List.More ((_, Valid (_,Raw_parser.DOT,_,_)),
                  List.One (_, ident))
        when token Raw_parser_values.is_ident ident -> (ident :: acc)
      | _ -> acc
    in
    List.map ~f:extract_ident (head acc (History.head h))

let is_uppercase {Location. txt = x} =
  x <> "" && Char.is_uppercase x.[0]

let rec drop_lowercase acc = function
  | [x] -> List.rev (x :: acc)
  | x :: xs when not (is_uppercase x) -> drop_lowercase [] xs
  | x :: xs -> drop_lowercase (x :: acc) xs
  | [] -> List.rev acc

let identifier_suffix ident =
  match List.last ident with
  | Some x when is_uppercase x -> drop_lowercase [] ident
  | _ -> ident

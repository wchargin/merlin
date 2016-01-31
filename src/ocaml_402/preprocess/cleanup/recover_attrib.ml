open Utils
open MenhirSdk.Cmly_format

module type S = sig
  val cost_of_prod    : production -> float
  val penalty_of_item : production * int -> float
  val cost_of_symbol  : symbol -> float

  val default_prelude : Format.formatter -> unit
  val default_value   : symbol -> string option
end

module Make (G : Utils.Grammar) : S = struct
  open G

  let table_terminal f =
    let table = Array.map f grammar.g_terminals in
    fun t -> table.(t.t_index)

  let table_nonterminal f =
    let table = Array.map f grammar.g_nonterminals in
    fun n -> table.(n.n_index)

  let table_symbol f =
    let ft = table_terminal (fun t -> f (T t)) in
    let fn = table_nonterminal (fun n -> f (N n)) in
    function
    | T t -> ft t
    | N n -> fn n

  let table_production f =
    let table = Array.map f grammar.g_productions in
    fun p -> table.(p.p_index)

  let cost_of_attributes attrs =
    List.fold_left
      (fun total attr ->
         if is_attribute "cost" attr then
           total +. float_of_string (string_of_stretch (snd attr))
         else total)
      0. attrs

  let cost_of_symbol =
    let measure ~default attrs =
      if List.exists (is_attribute "recovery") attrs then
        cost_of_attributes attrs
      else default
    in
    table_symbol (function
        | T t when t.t_type = None ->
            measure ~default:0.0 t.t_attributes
        | T t ->
            measure ~default:infinity t.t_attributes
        | N n ->
            measure ~default:infinity n.n_attributes
      )

  let cost_of_prod =
    table_production (fun p -> cost_of_attributes p.p_attributes)

  let penalty_of_item =
    let f = table_production @@ fun p ->
      Array.map (fun (_,_,a) -> cost_of_attributes a) p.p_rhs
    in
    fun (p,i) ->
      let costs = f p in
      if i < Array.length costs then costs.(i) else cost_of_prod p

  let default_prelude ppf =
    List.iter (fun a ->
        if is_attribute "header" a || is_attribute "recovery.header" a then
          Format.fprintf ppf "%s\n" (string_of_stretch (snd a))
      ) grammar.g_attributes

  let default_printer ?(fallback="raise Not_found") attrs =
    match List.find (is_attribute "recovery") attrs with
    | exception Not_found -> fallback
    | (_, stretch) -> string_of_stretch stretch

  let default_value = function
    | T t ->
        begin match t.t_kind with
        | `REGULAR | `ERROR | `EOF ->
            let fallback = match t.t_type with
              | None -> Some "()"
              | Some _ -> None
            in
            Some (default_printer ?fallback t.t_attributes)
        | `PSEUDO -> None
        end
    | N n ->
        begin match n.n_kind with
        | `REGULAR -> Some (default_printer n.n_attributes)
        | `START -> None
        end
end

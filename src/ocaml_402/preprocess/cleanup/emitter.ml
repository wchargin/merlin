open MenhirSdk.Cmly_format
open Utils

let menhir = "MenhirInterpreter"

module Codeconsing (S : Synthesis.S) (R : Recovery.S with module G = S.G) : sig

  (* Step 1: record all definitions *)
  val record_items : R.item list -> unit

  type instr =
    | Nil
    | Cons of instr S.paction * instr
    | Ref of int ref * instr

  (* Step 2: get prelude maximizing & serialization function *)
  val normalize : unit -> instr list * (R.item list -> instr)

end = struct

  open S
  open G

  type fixed = A of fixed paction list

  let normalized_actions = Hashtbl.create 113

  let rec normalize_actions ~flatten = function
    | [] -> []
    | [Var v] when flatten -> normalize_actions ~flatten:true (S.solution v)
    | (x :: xs) as xxs ->
        try !(Hashtbl.find normalized_actions xxs)
        with Not_found ->
          let x' = normalize_action ~flatten x in
          let xs' = normalize_actions ~flatten xs in
          let xxs' = x' :: xs' in
          Hashtbl.add normalized_actions xxs (ref xxs');
          xxs'

  and normalize_action ~flatten = function
    | Abort | Reduce _ | Shift _ | Pop as a -> a
    | Var v ->
        match normalize_actions ~flatten:true (S.solution v) with
        | [x] when flatten -> x
        | xs -> Var (A xs)

  let items_to_actions items =
    let prepare (st, prod, pos) =
      Var (Tail (st, prod, pos)) in
    normalize_actions ~flatten:false (List.map prepare items)

  let roots : R.item list list ref = ref []

  let record_items root =
    roots := root :: !roots

  let share () =
    let table = Hashtbl.create 113 in
    let rec get = function
      | [] -> []
      | (x :: xs) ->
          let xxs = (get_one x :: get xs) in
          try
            let r, v = Hashtbl.find table xxs in
            incr r; v
          with Not_found ->
            Hashtbl.add table xxs (ref 1, xxs);
            xxs
    and get_one = function
      | Var (A v) -> Var (A (get v))
      | x -> x
    in
    Hashtbl.iter (fun k v -> v := get !v) normalized_actions;
    (* Return counter *)
    (fun v -> !(fst (Hashtbl.find table v)))

  type instr =
    | Nil
    | Cons of instr paction * instr
    | Ref of int ref * instr

  let emitter () =
    let counter = share () in
    let table = Hashtbl.create 113 in
    let frozen = ref false in
    let values = ref [] in
    let rec emit = function
      | [] -> Nil
      | (x :: xs) as xxs ->
          try Hashtbl.find table xxs
          with Not_found ->
            let x = match x with
              | Var (A ys) -> Var (emit ys)
              | Pop | Abort | Reduce _ | Shift _ as a -> a
            in
            let value = Cons (x, emit xs) in
            if counter xxs = 1 then value else (
              assert (not !frozen);
              let value = Ref (ref (-1), value) in
              values := value :: !values;
              Hashtbl.add table xxs value;
              value
            )
    in
    frozen, values, emit

  let normalize () =
    let roots = List.map items_to_actions !roots in
    let frozen, values, emit = emitter () in
    let pass_2 items = ignore (emit items) in
    List.iter pass_2 roots;
    frozen := true;
    !values, (fun items -> emit (items_to_actions items))
end

module Make
    (G : Utils.G)
    (A : Recover_attrib.S with module G = G)
    (S : Synthesis.S with module G = G)
    (R : Recovery.S with module G = G) :
sig
  val emit : Format.formatter -> unit
end = struct

  open G
  open Format

  let emit_default_value ppf =
    fprintf ppf "open %s\n\n" (String.capitalize Grammar.basename);
    fprintf ppf "module Default = struct\n";
    A.default_prelude ppf;

    fprintf ppf "  let value (type a) : a %s.symbol -> a = function\n" menhir;
    Terminal.iter (fun t ->
        match A.default_terminal t with
        | None -> ()
        | Some str ->
            fprintf ppf "    | %s.T %s.T_%s -> %s\n" menhir menhir (Terminal.name t) str
      );
    Nonterminal.iter (fun n ->
        match A.default_nonterminal n with
        | None -> ()
        | Some str ->
            fprintf ppf "    | %s.N %s.N_%s -> %s\n" menhir menhir (Nonterminal.mangled_name n) str
      );
    fprintf ppf "    | _ -> raise Not_found\n";
    fprintf ppf "end\n\n";
    fprintf ppf "let default_value = Default.value\n\n"

  let emit_defs ppf =
    fprintf ppf "open %s\n\n" menhir;
    fprintf ppf "type t =\n\
                \  | Abort\n\
                \  | Pop\n\
                \  | Reduce of int\n\
                \  | Shift : 'a symbol -> t\n\
                \  | Get of int\n\
                \  | Sub of t list\n\n"

  module C = Codeconsing(S)(R)

  let emit_depth ppf =
    let open Format in
    fprintf ppf "let depth =\n  [|";
    Lr1.iter (fun st ->
        let depth, _ = R.recover st in
        fprintf ppf "%d;" depth
      );
    fprintf ppf "|]\n"

  let _code, get_instr, iter_entries =
    Lr1.iter (fun st ->
        let _depth, cases = R.recover st in
        List.iter (fun (_case, items) -> C.record_items (List.rev items))
          cases
      );
    let code, get_instr = C.normalize () in
    let get_instr items = get_instr (List.rev items) in
    let all_instrs =
      Lr1.tabulate (fun st ->
          let _depth, cases = R.recover st in
          List.map (fun (_case, items) -> get_instr items)
            cases
        )
    in
    code, get_instr,
    (fun f -> Lr1.iter (fun st -> List.iter f (all_instrs st)))

  let emit_recoveries ppf =
    let k = ref 0 in
    let instrs = ref [] in
    let rec alloc_entry = function
      | C.Nil -> ()
      | C.Cons (act, instr) -> alloc_entry_action act; alloc_entry instr
      | C.Ref (r, instr) ->
        if (!r = -1) then (
          alloc_entry instr;
          r := !k;
          instrs := (!k, instr) :: !instrs;
          incr k;
          )
    and alloc_entry_action = function
      | S.Abort | S.Reduce _ | S.Shift _ | S.Pop -> ()
      | S.Var instr -> alloc_entry instr
    in
    iter_entries alloc_entry;
    let open Format in

    let rec emit_action ppf = function
      | S.Abort -> fprintf ppf "Abort"
      | S.Pop   -> fprintf ppf "Pop"
      | S.Reduce prod -> fprintf ppf "Reduce %d" (Production.to_int prod)
      | S.Shift (T t) -> fprintf ppf "Shift (T T_%s)" (Terminal.name t)
      | S.Shift (N n) -> fprintf ppf "Shift (N N_%s)" (Nonterminal.mangled_name n)
      | S.Var instr -> fprintf ppf "Sub (%a)" emit_instr instr
    and emit_instr ppf = function
      | C.Nil -> fprintf ppf "[]"
      | C.Cons (act, instr) ->
        fprintf ppf "%a :: %a" emit_action act emit_instr instr
      | C.Ref (r, _) -> fprintf ppf "[Get %d]" !r
    in

    fprintf ppf "let shared = [|\n";
    let emit_shared k' (k, instr) =
      assert (k = k');
      fprintf ppf "  %a;\n" emit_instr instr
    in
    List.iteri emit_shared (List.rev !instrs);
    fprintf ppf "|]\n\n";

    fprintf ppf "let recover = [|\n";
    Lr1.iter (fun st ->
        fprintf ppf "    [|";
        let _, cases = R.recover st in
        List.iter (fun (st', items) ->
            fprintf ppf "(%d, %a);"
              (match st' with None -> -1 | Some st' -> Lr1.to_int st')
              emit_instr (get_instr items)
          ) cases;
        fprintf ppf "|];\n";
      );
    fprintf ppf "  |]\n"


  let emit ppf =
    emit_default_value ppf;
    emit_defs ppf;
    emit_depth ppf;
    emit_recoveries ppf

end

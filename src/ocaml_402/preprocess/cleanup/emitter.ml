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

  type instr =
    | KEnter
    | KReturn
    | KJump of int
    | KConstant of int

  let emit_defs ppf =
    fprintf ppf "open %s\n\n" menhir;
    fprintf ppf "type instr =\n\
                \  | Enter\n\
                \  | Return\n\
                \  | Jump of int\n\
                \  | Abort\n\
                \  | Pop\n\
                \  | Reduce of int\n\
                \  | Shift : 'a symbol -> instr\n\n"

  module C = Codeconsing(S)(R)

  let code, get_instr, iter_entries =
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

  let emit_constants ppf =
    let table = Hashtbl.create 113 in
    let k = ref 0 in
    let constants = ref [] in
    let rec emit_constant = function
      | C.Nil -> ()
      | C.Cons (action, instr) ->
        emit_action action;
        emit_constant instr
      | C.Ref (r, instr) ->
        if !r = -1
        then (r := -2; emit_constant instr)
        else assert (!r = -2)
    and emit_action = function
      | S.Var v -> emit_constant v
      | a ->
        if Hashtbl.mem table a then () else (
          constants := a :: !constants;
          Hashtbl.add table a !k;
          incr k
        )
    in
    iter_entries emit_constant;
    let open Format in
    let emit_const = function
      | S.Abort -> fprintf ppf "Abort; "
      | S.Pop   -> fprintf ppf "Pop; "
      | S.Reduce prod -> fprintf ppf "Reduce %d; " (Production.to_int prod)
      | S.Shift (T t) -> fprintf ppf "Shift (T T_%s); " (Terminal.name t)
      | S.Shift (N n) -> fprintf ppf "Shift (N N_%s); " (Nonterminal.mangled_name n)
      | _ -> assert false
    in
    fprintf ppf "let constants =\n  [|";
    List.iter emit_const (List.rev !constants);
    fprintf ppf "|]\n";
    (fun v -> try Hashtbl.find table v with Not_found -> assert false),
    !k

  let link_code csts ppf =
    let ptr = ref 0 in
    let code = ref [] in
    let emit_one x =
      code := x :: !code;
      incr ptr;
    in
    let rec emit_instr = function
      | C.Nil -> emit_one KReturn
      | C.Ref (r, _) when !r >= 0 ->
        emit_one (KJump !r)
      | C.Ref (r, instr) ->
        r := !ptr;
        emit_instr instr
      | C.Cons (S.Var v, C.Nil) ->
        emit_instr v
      | C.Cons (S.Var v, instr) ->
        emit_one KEnter;
        emit_instr v;
        emit_instr instr
      | C.Cons (a, instr) ->
        emit_one (KConstant (csts a));
        emit_instr instr
    in
    let emit_entry instr =
      let ptr = !ptr in
      emit_instr instr;
      ptr
    in
    let table = Hashtbl.create 113 in
    iter_entries (fun entry -> Hashtbl.add table entry (emit_entry entry));
    let code = Array.of_list (List.rev !code) in
    code, Hashtbl.find table

  let emit_packed ppf (k,str) =
    Format.fprintf ppf "(%d, %S)" k str

  let emit_code cst_count code ppf =
    let encode = function
      | KEnter -> 0
      | KReturn -> 1
      | KConstant cst -> 2 + cst
      | KJump offset -> 2 + cst_count + offset
    in
    let packed =
      code
      |> Array.map encode
      |> MenhirLib.PackedIntArray.pack
    in
    let open Format in
    fprintf ppf
      "let decode =\n\
      \  let aux = function
      \    | 0 -> Enter\n\
      \    | 1 -> Return\n\
      \    | n ->\n\
      \      let n = n - 2 and cst_count = Array.length constants in\n\
      \      if n < cst_count then constants.(n)\n\
      \      else (KJump (n - cst_count))\n\
      \  in\n\
      \  let bytecode = %a in\n\
      \  fun i -> aux (MenhirLib.PackedIntArray.get bytecode i)\n\n"
      emit_packed packed

  let emit_depth ppf =
    let packed =
      Array.init Lr1.count (fun st -> fst (R.recover (Lr1.of_int st)))
      |> MenhirLib.PackedIntArray.pack
    in
    let open Format in
    fprintf ppf
      "let depth =\n\
      \  let depthmap = %a in\n\
      \  fun i -> MenhirLib.PackedIntArray.get depthmap i\n\n"
      emit_packed packed

  let emit_dispatch_table ppf entries =
    let table =
      Array.init Lr1.count @@ fun st ->
      let st = Lr1.of_int st in
      let row = Array.make (Lr1.count + 1) 0 in
      let _, cases = R.recover st in
      List.iter (fun (st', items) ->
          let idx = match st' with
            | None -> Lr1.count
            | Some st' -> Lr1.to_int st'
          in
          row.(idx) <- entries (get_instr items)
        ) cases;
      row
    in
    let disp, data =
      MenhirLib.RowDisplacement.compress ((=) : int -> int -> bool) ((=) 0) 0
        Lr1.count (Lr1.count + 1)
        table
    in
    let (_, s) = MenhirLib.PackedIntArray.pack data in
    let c = open_out "data.out" in
    output_string c s;
    close_out c;
    fprintf ppf
      "let entrypoint =\n\
      \  let disp = %a in\n\
      \  let data = %a in\n\
      \  fun st1 st2 -> \n\
      \    MenhirLib.PackedIntArray.unmarshal2 disp data st1 st2\n\n"
      emit_packed (MenhirLib.PackedIntArray.pack disp)
      emit_packed (MenhirLib.PackedIntArray.pack data)

  let emit ppf =
    emit_default_value ppf;
    emit_defs ppf;
    let csts, cst_count = emit_constants ppf in
    let code, entries = link_code csts ppf in
    emit_code cst_count code ppf;
    emit_depth ppf;
    emit_dispatch_table ppf entries

end

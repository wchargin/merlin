open MenhirSdk
open Cmly_format

let name = ref ""
let verbose = ref false

let usage () =
  Printf.eprintf "Usage: %s [-v] file.cmly\n"
    Sys.argv.(0);
  exit 1

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    if Sys.argv.(i) = "-v" then
      verbose := true
    else if !name = "" then
      name := Sys.argv.(i)
    else
      usage ()
  done;
  if !name = "" then
    usage ()

module G = struct
  let grammar = Cmly_io.read_file !name
end

module A = Recover_attrib.Make(G)

let () =
  let open Format in
  let ppf = Format.err_formatter in
  if !verbose then begin
    Array.iter (fun st ->
        fprintf ppf "\n# LR(1) state #%d\n\n" st.lr1_index;
        fprintf ppf "Items:\n";
        Utils.print_table ppf
          (Utils.items_table (Array.to_list st.lr1_lr0.lr0_items));
        fprintf ppf "Transitions:\n";
        Array.iter (fun (sym,st') ->
            fprintf ppf " - on %s, goto #%d\n"
              (Utils.name_of_symbol sym) st'.lr1_index
          ) st.lr1_transitions;
        fprintf ppf "Reductions:\n";
        Array.iter (fun (t,ps) ->
            fprintf ppf " - on %s, reduce p%d\n" t.t_name (List.hd ps).p_index
          ) st.lr1_reductions;
      ) G.grammar.g_lr1_states;
    Array.iter (fun p ->
        fprintf ppf "\n# Producion p%d\n" p.p_index;
        Utils.print_table ppf (Utils.items_table [p,-1]);
      ) G.grammar.g_productions
  end

module S = Synthesis.Make(G)(A)

let () = if !verbose then S.report Format.err_formatter

module R = Recovery.Make(G)(S)

let () = if !verbose then R.report Format.err_formatter

module E = Emitter.Make(G)(A)(S)(R)

let () =
  let name = Filename.chop_extension (Filename.basename !name) in
  E.emit_prelude ~name Format.std_formatter;
  E.emit_recovery Format.std_formatter

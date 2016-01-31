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

module S = Synthesis.Make(G)(A)

let () = if !verbose then S.report Format.err_formatter

module R = Recovery.Make(G)(S)

let () = if !verbose then R.report Format.err_formatter

module E = Emitter.Make(G)(A)(S)(R)

let () =
  let name = Filename.chop_extension (Filename.basename !name) in
  E.emit_prelude ~name Format.std_formatter;
  E.emit_recovery Format.std_formatter

open Std

include Yojson.Basic

let string x = `String x
let int x = `Int x

let of_position pos =
  let line, col = Lexing.split_pos pos in
  `Assoc ["line", `Int line; "col", `Int col]

let with_location ?(skip_none=false) loc assoc =
  if skip_none && loc = Location.none then
    `Assoc assoc
  else
    `Assoc (("start", of_position loc.Location.loc_start) ::
            ("end",   of_position loc.Location.loc_end) ::
            assoc)

let of_location loc =
  with_location loc []

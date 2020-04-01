open Syntax

let error pos msg =
    raise (Error (pos, msg))


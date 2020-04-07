open Syntax

let add_symbol tab name ty v =
    let tenv = Env.extend name (ref (Type.new_type_schema ty)) tab.tenv in
    let env = Env.extend name (ref v) tab.env in
    make_table env tenv


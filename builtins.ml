
open Syntax

let type_error pos s = error pos ("type '" ^ s ^ "' required")

let fn_nl _ _ =
    print_newline ();
    flush stdout;
    VUnit

let fn_putn pos = function
    | VInt n -> print_int n; VUnit
    | _ -> type_error pos "int"

let fn_puts pos = function
    | VString s -> print_string s; VUnit
    | _ -> type_error pos "string"

let fn_head pos = function
    | VCons (hd, _) -> hd
    | _ -> type_error pos "list"

let fn_tail pos = function
    | VCons (_, tl) -> tl
    | _ -> type_error pos "list"

let builtins_list =
    let head_type = Type.new_tvar () in
    let tail_type = Type.new_tvar () in
    [
        ("nl", TFun (TUnit, TUnit), fn_nl);
        ("putn", TFun (TInt, TUnit), fn_putn);
        ("puts", TFun (TString, TUnit), fn_puts);
        ("hd", TFun (TList head_type, head_type), fn_head);
        ("tl", TFun (TList tail_type, TList tail_type), fn_tail);
    ]

let init () =
    let add_func (env, tenv) (name, ty, fn) =
        let tenv = Env.extend name (ref (Type.new_type_schema ty)) tenv in
        let env = Env.extend name (ref (VBuiltin fn)) env in
        (env, tenv)
    in
    let tenv = Env.extend "true" (ref (Type.new_type_schema TBool)) [] in
    let env = Env.extend "true" (ref (VBool true)) [] in
    let tenv = Env.extend "false" (ref (Type.new_type_schema TBool)) tenv in
    let env = Env.extend "false" (ref (VBool false)) env in
    List.fold_left (fun env bs -> add_func env bs) (env, tenv) builtins_list

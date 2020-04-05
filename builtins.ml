
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

let fn_putv pos v = 
    print_string @@ s_value v;
    VUnit

let fn_head pos = function
    | VCons (hd, _) -> hd
    | _ -> type_error pos "list"

let fn_tail pos = function
    | VCons (_, tl) -> tl
    | _ -> type_error pos "list"

let fn_fst pos = function
    | VTuple (x::_) -> x
    | _ -> type_error pos "tuple"

let fn_snd pos = function
    | VTuple (_::x::_) -> x
    | _ -> type_error pos "tuple"

let builtins_list =
    let putv_type = Type.new_tvar () in
    let head_type = Type.new_tvar () in
    let tail_type = Type.new_tvar () in
    let fst_type = Type.new_tvar () in
    let fst_any_type = Type.new_tvar () in
    let snd_type = Type.new_tvar () in
    let snd_any_type = Type.new_tvar () in
    [
        ("true", TBool, VBool true);
        ("false", TBool, VBool false);
        ("nl", TFun (TUnit, TUnit), VBuiltin fn_nl);
        ("putn", TFun (TInt, TUnit), VBuiltin fn_putn);
        ("puts", TFun (TString, TUnit), VBuiltin fn_puts);
        ("putv", TFun (putv_type, TUnit), VBuiltin fn_putv);
        ("hd", TFun (TList head_type, head_type), VBuiltin fn_head);
        ("tl", TFun (TList tail_type, TList tail_type), VBuiltin fn_tail);
        ("fst", TFun (TTuple (fst_type::[fst_any_type]), fst_type), VBuiltin fn_fst);
        ("snd", TFun (TTuple (snd_any_type::[snd_type]), snd_type), VBuiltin fn_snd);
    ]

let init () =
    let add_symbol (env, tenv) (name, ty, v) =
        let tenv = Env.extend name (ref (Type.new_type_schema ty)) tenv in
        let env = Env.extend name (ref v) env in
        (env, tenv)
    in
    List.fold_left (fun env bs -> add_symbol env bs) ([], []) builtins_list


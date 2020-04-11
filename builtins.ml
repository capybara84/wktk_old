
open Syntax

let type_error pos s = error pos ("type '" ^ s ^ "' required")

let fn_nl _ env _ =
    print_newline ();
    flush stdout;
    (env, VUnit)

let fn_put pos env = function
    | VString s -> print_string s; (env, VUnit)
    | _ -> type_error pos "string"

let fn_putn pos env = function
    | VInt n -> print_int n; (env, VUnit)
    | _ -> type_error pos "int"

let fn_putv _ env v = 
    print_string @@ s_value v;
    (env, VUnit)

let fn_puts pos env = function
    | VString s -> print_endline s; (env, VUnit)
    | _ -> type_error pos "string"

let fn_head pos env = function
    | VCons (hd, _) -> (env, hd)
    | VString x ->
        if x = "" then (env, VNull)
        else (env, VChar (String.get x 0))
    | _ -> type_error pos "list"

let fn_tail pos env = function
    | VCons (_, tl) -> (env, tl)
    | VString x ->
        if x = "" then (env, VNull)
        else (env, VString (String.sub x 1 (String.length x - 1)))
    | _ -> type_error pos "list"

let fn_fst pos env = function
    | VTuple (x::_) -> (env, x)
    | _ -> type_error pos "tuple"

let fn_snd pos env = function
    | VTuple (_::x::_) -> (env, x)
    | _ -> type_error pos "tuple"

let fn_env _ env _ =
    List.iter
        (fun e ->
            print_endline @@ fst e ^ " = " ^ s_value !(snd e)
        ) env;
    (env, VUnit)

let fn_modules _ env _ =
    Symbol.print_modules ();
    (env, VUnit)

let rec fn_builtins _ env _ =
    List.iter (fun (name, ty, _) -> print_endline @@ name ^ " :: " ^ s_typ ty) builtins_list;
    (env, VUnit)

and builtins_list =
    let putv_type = Type.new_tvar () in
    let head_type = Type.new_tvar () in
    let tail_type = Type.new_tvar () in
    let fst_any_type = Type.new_tvar () in
    let fst_type = Type.new_tvar () in
    let snd_any_type = Type.new_tvar () in
    let snd_type = Type.new_tvar () in
    [
        ("true", TBool, VBool true);
        ("false", TBool, VBool false);
        ("nl", TFun (TUnit, TUnit), VBuiltin fn_nl);
        ("put", TFun (TString, TUnit), VBuiltin fn_put);
        ("putn", TFun (TInt, TUnit), VBuiltin fn_putn);
        ("putv", TFun (putv_type, TUnit), VBuiltin fn_putv);
        ("puts", TFun (TString, TUnit), VBuiltin fn_puts);
        ("hd", TFun (TList head_type, head_type), VBuiltin fn_head);
        ("tl", TFun (TList tail_type, TList tail_type), VBuiltin fn_tail);
        ("fst", TFun (TTuple (fst_type::[fst_any_type]), fst_type), VBuiltin fn_fst);
        ("snd", TFun (TTuple (snd_any_type::[snd_type]), snd_type), VBuiltin fn_snd);
        ("env", TFun (TUnit, TUnit), VBuiltin fn_env);
        ("builtins", TFun (TUnit, TUnit), VBuiltin fn_builtins);
        ("modules", TFun (TUnit, TUnit), VBuiltin fn_modules);
    ]

let init () =
    List.iter
        (fun (name, ty, value) -> Symbol.insert_default name (Type.create_poly_type ty) value)
        builtins_list


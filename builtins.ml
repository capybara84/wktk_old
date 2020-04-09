
open Syntax

let type_error pos s = error pos ("type '" ^ s ^ "' required")

let fn_nl _ env _ =
    print_newline ();
    flush stdout;
    (env, VUnit)

let fn_putn pos env = function
    | VInt n -> print_int n; (env, VUnit)
    | _ -> type_error pos "int"

let fn_puts pos env = function
    | VString s -> print_string s; (env, VUnit)
    | _ -> type_error pos "string"

let fn_putv _ env v = 
    print_string @@ s_value v;
    (env, VUnit)

let fn_head pos env = function
    | VCons (hd, _) -> (env, hd)
    | _ -> type_error pos "list"

let fn_tail pos env = function
    | VCons (_, tl) -> (env, tl)
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
    let show_sym (id, ts) = 
        print_endline @@ " " ^ id ^ " :: " ^ s_typ !ts.body
    in
    let show (id, symtab) =
        print_endline ("module " ^ id);
        List.iter show_sym symtab.tenv
    in
    List.iter show !Symbol.all_modules;
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
        ("putn", TFun (TInt, TUnit), VBuiltin fn_putn);
        ("puts", TFun (TString, TUnit), VBuiltin fn_puts);
        ("putv", TFun (putv_type, TUnit), VBuiltin fn_putv);
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
        (fun (name, ty, value) -> Symbol.insert_default name (Type.new_type_schema ty) value)
        builtins_list


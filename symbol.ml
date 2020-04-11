open Syntax

let default_module_name = "Main"

let all_modules = ref Env.empty

let insert_module name =
    let modu = { module_name = name; env = Env.empty; tenv = Env.empty } in
    all_modules := Env.extend name modu !all_modules;
    modu

let lookup_module id =
    Env.lookup id !all_modules

let exist_module id =
    try ignore (Env.lookup id !all_modules);  true
    with Not_found -> false

let default_module = insert_module default_module_name
let current_module = ref default_module

let get_current_module () = !current_module
let set_current_module modu = current_module := modu

let set_module id =
    (try
        current_module := lookup_module id
    with Not_found ->
        current_module := insert_module id);
    !current_module

let rename_module old_name new_name =
    let modu = lookup_module old_name in
    let module_list = List.remove_assoc old_name !all_modules in
    all_modules := Env.extend new_name modu module_list

let get_current_tenv () = !current_module.tenv
let set_current_tenv tenv = !current_module.tenv <- tenv
let get_current_env () = !current_module.env
let set_current_env env = !current_module.env <- env

let lookup_default_type s = Env.lookup s default_module.tenv
let lookup_default s = Env.lookup s default_module.env

let insert_default name ts value =
    default_module.tenv <- Env.extend name (ref ts) default_module.tenv;
    default_module.env <- Env.extend name (ref value) default_module.env

let print_modules () =
    let show_sym (id, ts) = 
        print_endline @@ " " ^ id ^ " :: " ^ s_typ !ts.body
    in
    let show (id, symtab) =
        print_endline ("module " ^ id);
        List.iter show_sym symtab.tenv
    in
    List.iter show !all_modules;


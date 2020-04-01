open Syntax

let debug_scope_flag = ref false
let debug_indent = ref 0

let rec debug_show_space = function
    | 0 -> ()
    | n -> print_char ' '; debug_show_space (n-1)

let debug_type s =
    if !debug_scope_flag then
        (debug_show_space !debug_indent; print_endline s)

let debug_type_in s =
    if !debug_scope_flag then
        (debug_type @@ "IN " ^ s; incr debug_indent)

let debug_type_out s =
    if !debug_scope_flag then
        (decr debug_indent; debug_type @@ "OUT " ^ s)

let error pos msg =
    raise (Error ("runtime", pos, msg))

let seed = ref 0
let new_tvar () =
    let ty = TVar (!seed, ref None) in
    incr seed;
    ty

let get_tnum = function
    | TVar (n, _) -> n
    | _ -> failwith "tnum"

let t_constr name typlist = TConstr (name, typlist)
let t_unit = t_constr "unit" []
let t_bool = t_constr "bool" []
let t_int = t_constr "int" []
let t_char = t_constr "char" []
let t_float = t_constr "float" []
let t_list t = t_constr "list" [t]
let t_string = t_list t_char

let new_list () =
    t_list (new_tvar ())

let new_type_schema ty =
    { vars = []; body = ty }

let rec equal t1 t2 =
    match (t1, t2) with
    | (TConstr (s1, tl1), TConstr (s2, tl2)) ->
        s1 = s2 && list_equal (tl1, tl2)
    | (TFun (t11, t12), TFun (t21, t22)) ->
        equal t11 t21 && equal t21 t22
    | (TVar (n, {contents = None}), TVar (m, {contents = None})) ->
        n = m
    | (TVar (_, {contents = None}), _)
    | (_, TVar (_, {contents = None})) ->
        true
    | (TVar (_, {contents = Some t1'}), _) ->
        equal t1' t2
    | (_, TVar (_, {contents = Some t2'})) ->
        equal t1 t2'
    | _ -> false
and list_equal = function
    | ([], []) -> true
    | (_, []) | ([], _) -> false
    | (x::xs, y::ys) ->
        if equal x y then
            list_equal (xs, ys)
        else false

let rec unwrap_var free_vars = function
    | TConstr (s, tl) ->
        let (free_vars, new_tl) = unwrap_tl_var free_vars [] tl in
        (free_vars, TConstr (s, new_tl))
    | TFun (t1, t2) ->
        let (free_vars, t1') = unwrap_var free_vars t1 in
        let (free_vars, t2') = unwrap_var free_vars t2 in
        (free_vars, TFun (t1', t2'))
    | TVar (n, ({contents = None})) as typ ->
        if List.mem n free_vars then
            (free_vars, typ)
        else
            (n::free_vars, typ)
    | TVar (_, ({contents = Some t})) ->
        let (free_vars, new_t) = unwrap_var free_vars t in
        (free_vars, new_t)
and unwrap_tl_var free_vars new_tl = function
    | [] -> (free_vars, List.rev new_tl)
    | x::xs ->
        let (free_vars, t) = unwrap_var free_vars x in
        unwrap_tl_var free_vars (t::new_tl) xs

let create_poly_type ty =
    let (free_vars, unwrapped_type) = unwrap_var [] ty in
    { vars = free_vars; body = unwrapped_type }

let create_alpha_equivalent ts =
    let rec fresh_vars res_vars res_map = function
        | [] -> (List.rev res_vars, List.rev res_map)
        | x::xs ->
            let ty = new_tvar () in
            let n = get_tnum ty in
            fresh_vars (n::res_vars) ((x, ty)::res_map) xs
    in
    let rec subst map = function
        | TConstr (s, tl) -> TConstr (s, List.map (subst map) tl)
        | TFun (t1, t2) -> TFun (subst map t1, subst map t2)
        | TVar (_, {contents = Some t}) -> subst map t
        | TVar (n, {contents = None}) as t ->
            (try List.assoc n map with Not_found -> t)
    in
    let (new_vars, var_map) = fresh_vars [] [] ts.vars in
    { vars = new_vars; body = subst var_map ts.body }


let rec prune = function
    | TVar (_, ({contents = Some t'} as instance)) ->
        let inst = prune t' in
        instance := Some inst;
        inst
    | t -> t

let rec type_var_equal t1 t2 =
    match (t1, t2) with
    | (TVar (n, {contents = None}), TVar (m, {contents = None})) when n = m
        -> true
    | (TVar (_, {contents = Some t1'}), _)
        -> type_var_equal t1' t2
    | (_, TVar (_, {contents = Some t2'}))
        -> type_var_equal t1 t2'
    | _ -> false

let rec occurs_in_type t t2 =
    let t2 = prune t2 in
    if type_var_equal t t2 then true
    else
        match t2 with
        | TConstr (_, tl) -> occurs_in t tl
        | TFun (tf1, tf2) -> occurs_in t [tf1;tf2]
        | _ -> false

and occurs_in t types =
    List.exists (fun t2 -> occurs_in_type t t2) types

let rec unify t1 t2 pos =
    if equal t1 t_unit && equal t2 t_unit then ();
    if equal t1 t_bool && equal t2 t_bool then ();
    if equal t1 t_int && equal t2 t_int then ();
    if equal t1 t_char && equal t2 t_char then ();
    if equal t1 t_float && equal t2 t_float then ();
    if equal t1 t_string && equal t2 t_string then ();
    match (t1, t2) with
    | (TConstr (s1, tl1), TConstr (s2, tl2)) when s1 = s2 ->
        if List.length tl1 <> List.length tl2 then
            error pos @@ "type mismatch between " ^ s_typ t2 ^ " and " ^ s_typ t1;
        List.iter2 (fun a b -> unify a b pos) tl1 tl2
    | (TFun (t11, t12), TFun (t21, t22)) ->
        unify t11 t21 pos;
        unify t12 t22 pos
    | (TVar (n1, {contents=None}), TVar (n2, {contents=None})) when n1 = n2 -> ()
    | (TVar (_, {contents = Some t1'}), _) -> unify t1' t2 pos
    | (_, TVar (_, {contents = Some t2'})) -> unify t1 t2' pos
    | (TVar (_, ({contents = None} as r1)), _) ->
        if occurs_in_type t1 t2 then
            error pos @@ "type circularity between " ^ s_typ t1 ^ " and " ^ s_typ t2
        else begin
            r1 := Some t2;
        end
    | (_, TVar (_, ({contents = None} as r2))) ->
        if occurs_in_type t2 t1 then
            error pos @@ "type circularity between " ^ s_typ t2 ^ " and " ^ s_typ t1
        else begin
            r2 := Some t1;
        end
    | (_, _) -> error pos @@ "type mismatch between " ^ s_typ t2 ^ " and " ^ s_typ t1

let rec infer tenv e =
    debug_type_in @@ "infer: " ^ s_expr e;
    let infer_unary op t pos =
        debug_type_in @@ "infer_unary: '" ^ s_unop op ^ "' " ^ s_typ t;
        let res =
            match op with
            | UMinus ->
                if equal t t_int then t_int
                else if equal t t_float then t_float
                else error pos @@ "The unary minus expression has type " ^ s_typ t ^ " but an expression was expected of type int/float"
            | UNot ->
                if equal t t_bool then t_bool
                else error pos @@ "The unary not expression has type " ^ s_typ t ^ " but an expression was expected of type int"
        in
        debug_type_out @@ "infer_unary: " ^ s_typ res;
        res
    in
    let infer_binary op tl tr pos =
        debug_type_in @@ "infer_binary: '" ^ s_binop op ^ "' " ^ s_typ tl ^ " " ^ s_typ tr;
        let res =
            match op with
            | BinAdd | BinSub | BinMul | BinDiv | BinMod ->
                unify tl tr pos;
                if equal tl t_int || equal tl t_float then tl
                else error pos @@ "The binary expression has type " ^ s_typ tl ^ " but an expression was expected of type int/float"
            | BinLT | BinLE | BinGT | BinGE ->
                unify tl tr pos;
                if equal tl t_char || equal tl t_int || equal tl t_float || equal tl t_string then t_bool
                else error pos @@ "The relational expression has type " ^ s_typ tl ^ " but an expression was expected of type char/int/float/string"
            | BinEql | BinNeq ->
                unify tl tr pos;
                t_bool
            | BinLOr | BinLAnd -> 
                unify t_bool tl pos;
                unify t_bool tr pos;
                t_bool
            | BinCons ->
                unify (t_list tl) tr pos;
                tr
            | _ -> failwith "infer_binary"
        in
        debug_type_out @@ "infer_binary: " ^ s_typ res;
        res
    in
    let res =
        match e with
        | (ENull, _) -> (tenv, new_list ())
        | (EUnit, _) -> (tenv, t_unit)
        | (ELit (Int _), _) -> (tenv, t_int)
        | (ELit (Char _), _) -> (tenv, t_char)
        | (ELit (Float _), _) -> (tenv, t_float)
        | (ELit (String _), _) -> (tenv, t_string)
        | (EId s, pos) ->
            let ts = (try !(Env.lookup s tenv) with Not_found -> error pos @@ "'" ^ s ^ "' not found") in
            let new_ts = create_alpha_equivalent ts in
            (tenv, new_ts.body)
        | (EUnary (op, e), pos) ->
            let (_, t) = infer tenv e in
            (tenv, infer_unary op t pos)
        | (EBinary (BinOp op, l, r), pos) ->
            (*TODO *)
            (tenv, t_unit)
        | (EBinary (op, l, r), pos) ->
            let (_, tl) = infer tenv l in
            let (_, tr) = infer tenv r in
            (tenv, infer_binary op tl tr pos)
        | (ECond (cond_e, then_e, else_e), pos) ->
            let (_, t_cond) = infer tenv cond_e in
            unify t_bool t_cond pos;
            let (_, t_then) = infer tenv then_e in
            let (_, t_else) = infer tenv else_e in
            unify t_then t_else pos;
            (tenv, t_then)
        | (ELambda ("()", body), _) ->
            let (_, t_body) = infer tenv body in
            (tenv, TFun (t_unit, t_body))
        | (ELambda ("_", body), pos) ->
            let t_arg = new_tvar () in
            let (_, t_body) = infer tenv body in
            (tenv, TFun (t_arg, t_body))
        | (ELambda (arg, body), pos) ->
            let t_arg = new_tvar () in
            let ts = new_type_schema t_arg in
            let tenv = Env.extend arg (ref ts) tenv in
            let (_, t_body) = infer tenv body in
            (tenv, TFun (t_arg, t_body))
        | (EApply (fn, arg), pos) ->
            let (_, t_fn) = infer tenv fn in
            let (_, t_arg) = infer tenv arg in
            let t = new_tvar () in
            unify t_fn (TFun (t_arg, t)) pos;
            (tenv, t)
        | (ELet (id, e), _) ->
            let (_, t) = infer tenv e in
            let ts = create_poly_type t in
            let tenv = Env.extend id (ref ts) tenv in
            (tenv, t_unit)
        | (ELetRec (id, e), _) ->
            let r = ref (new_type_schema (new_tvar ())) in
            let tenv = Env.extend id r tenv in
            let (_, t) = infer tenv e in
            r := create_poly_type t;
            (tenv, t_unit)
        | (ESeq el, _) ->
            let rec loop tenv = function
                | [] -> (tenv, t_unit)
                | x::[] ->
                    infer tenv x
                | x::xs ->
                    let (tenv, t) = infer tenv x in
                    if t <> t_unit then error (snd x) "expression should have type unit";
                    loop tenv xs
            in loop tenv el
    in
    debug_type_out @@ "infer: " ^ s_typ (snd res);
    res


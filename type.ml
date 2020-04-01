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
    raise (Error (pos, msg))

let seed = ref 0
let new_tvar () =
    let ty = TVar (!seed, ref None) in
    incr seed;
    ty

let get_tnum = function
    | TVar (n, _) -> n
    | _ -> failwith "tnum"

let new_list () =
    TList (new_tvar ())

let new_type_schema ty =
    { vars = []; body = ty }

let rec equal t1 t2 =
    match (t1, t2) with
    | (TUnit, TUnit) | (TBool, TBool) | (TInt, TInt) | (TChar, TChar)
    | (TFloat, TFloat) | (TString, TString)
    | (TList TChar, TString) | (TString, TList TChar) -> true
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
(*
and list_equal = function
    | ([], []) -> true
    | (_, []) | ([], _) -> false
    | (x::xs, y::ys) ->
        if equal x y then
            list_equal (xs, ys)
        else false
*)

let rec unwrap_var free_vars = function
    | TList ty ->
        let (free_vars, unwrapped_t) = unwrap_var free_vars ty in
        (free_vars, TList unwrapped_t)
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
    | t -> (free_vars, t)
(*
and unwrap_tl_var free_vars new_tl = function
    | [] -> (free_vars, List.rev new_tl)
    | x::xs ->
        let (free_vars, t) = unwrap_var free_vars x in
        unwrap_tl_var free_vars (t::new_tl) xs
*)

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
        | TList t -> TList (subst map t)
        | TFun (t1, t2) -> TFun (subst map t1, subst map t2)
        | TVar (_, {contents = Some t}) -> subst map t
        | TVar (n, {contents = None}) as t ->
            (try List.assoc n map with Not_found -> t)
        | t -> t
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
        | TList t' -> occurs_in_type t t'
        | TFun (tf1, tf2) -> occurs_in t [tf1;tf2]
        | _ -> false

and occurs_in t types =
    List.exists (fun t2 -> occurs_in_type t t2) types


let rec is_type t1 t2 =
    let t1 = prune t1 in
    let t2 = prune t2 in
    match t1, t2 with
    | (TVar (_, {contents=Some t1'}), _) -> is_type t1' t2
    | (_, TVar (_, {contents=Some t2'})) -> is_type t1 t2'
    | (TList TChar, TString)
    | (TString, TList TChar) -> true
    | _ when t1 = t2 -> true
    | _ -> false

let is_type_in t tl =
    List.exists (fun t2 -> is_type t t2) tl

let is_tvar t =
    let t = prune t in
    match t with
    | TVar _ -> true
    | _ -> false


let rec unify t1 t2 pos =
    debug_type_in @@ "unify: " ^ s_typ_raw t1 ^ ", " ^ s_typ_raw t2;
    let t1 = prune t1 in
    let t2 = prune t2 in
    (match (t1, t2) with
    | (TUnit, TUnit) | (TBool, TBool) | (TInt, TInt) | (TChar, TChar) | (TFloat, TFloat)
    | (TString, TString) | (TList TChar, TString) | (TString, TList TChar) -> ()
    | (TList tl, TList tr) ->
        unify tl tr pos
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
            debug_type @@ "unify result " ^ s_typ_raw t1 ^ " ... ";
            r1 := Some t2;
            debug_type @@ "... " ^ s_typ_raw t1
        end
    | (_, TVar (_, ({contents = None} as r2))) ->
        if occurs_in_type t2 t1 then
            error pos @@ "type circularity between " ^ s_typ t2 ^ " and " ^ s_typ t1
        else begin
            debug_type @@ "unify result " ^ s_typ_raw t2 ^ " ... ";
            r2 := Some t1;
            debug_type @@ "... " ^ s_typ_raw t2
        end
    | (_, _) -> error pos @@ "type mismatch between " ^ s_typ t2 ^ " and " ^ s_typ t1);
    debug_type_out @@ "unify"

let rec infer tenv e =
    debug_type_in @@ "infer: " ^ s_expr e;
    let infer_unary op t pos =
        debug_type_in @@ "infer_unary: '" ^ s_unop op ^ "', " ^ s_typ_raw t;
        let res =
            match op with
            | UMinus ->
                if is_type t TInt then TInt
                else if is_type t TFloat then TFloat
                else error pos @@ "The unary minus expression has type " ^ s_typ t ^ " but an expression was expected of type int/float"
            | UNot ->
                if is_type t TBool then TBool
                else error pos @@ "The unary not expression has type " ^ s_typ t ^ " but an expression was expected of type int"
        in
        debug_type_out @@ "infer_unary = " ^ s_typ_raw res;
        res
    in
    let infer_binary op tl tr pos =
        debug_type_in @@ "infer_binary: '" ^ s_binop op ^ "', " ^ s_typ_raw tl ^ ", " ^ s_typ_raw tr;
        let res =
            match op with
            | BinAdd | BinSub | BinMul | BinDiv | BinMod ->
                unify tl tr pos;
                if is_type_in tl [TInt;TFloat] || is_tvar tl then tl
                else
                    error pos @@ "The binary expression has type " ^ s_typ tl ^
                                    " but an expression was expected of type int/float"
            | BinLT | BinLE | BinGT | BinGE ->
                unify tl tr pos;
                if is_type_in tl [TChar;TInt;TFloat;TString;TList TChar] || is_tvar tl then TBool
                else error pos @@ "The relational expression has type " ^ s_typ tl ^
                                    " but an expression was expected of type char/int/float/string"
            | BinEql | BinNeq ->
                unify tl tr pos;
                TBool
            | BinLOr | BinLAnd -> 
                unify TBool tl pos;
                unify TBool tr pos;
                TBool
            | BinCons ->
                unify (TList tl) tr pos;
                tr
            | _ -> failwith "infer_binary"
        in
        debug_type_out @@ "infer_binary = " ^ s_typ_raw res;
        res
    in
    let res =
        match e with
        | (ENull, _) ->
            debug_type "infer Null";
            (tenv, new_list ())
        | (EUnit, _) ->
            debug_type "infer Unit";
            (tenv, TUnit)
        | (ELit (Int _), _) ->
            debug_type "infer Int";
            (tenv, TInt)
        | (ELit (Char _), _) ->
            debug_type "infer Char";
            (tenv, TChar)
        | (ELit (Float _), _) ->
            debug_type "infer Float";
            (tenv, TFloat)
        | (ELit (String _), _) ->
            debug_type "infer String";
            (tenv, TString)
        | (EId s, pos) ->
            debug_type @@ "infer Id " ^ s;
            let ts = (try !(Env.lookup s tenv) with Not_found -> error pos @@ "'" ^ s ^ "' not found") in
            let new_ts = create_alpha_equivalent ts in
            (tenv, new_ts.body)
        | (EUnary (op, e), pos) ->
            debug_type @@ "infer unary '" ^ s_unop op ^ "', " ^ s_expr e;
            let (_, t) = infer tenv e in
            (tenv, infer_unary op t pos)
        | (EBinary (BinOp op, l, r), pos) ->
            (*TODO *)
            (tenv, TUnit)
        | (EBinary (op, l, r), pos) ->
            debug_type @@ "infer binary '" ^ s_binop op ^ "', " ^ s_expr l ^ ", " ^ s_expr r;
            let (_, tl) = infer tenv l in
            let (_, tr) = infer tenv r in
            (tenv, infer_binary op tl tr pos)
        | (ECond (cond_e, then_e, else_e), pos) ->
            debug_type @@ "infer cond " ^ s_expr cond_e ^ ", " ^ s_expr then_e ^ ", " ^ s_expr else_e;
            let (_, t_cond) = infer tenv cond_e in
            unify TBool t_cond pos;
            let (_, t_then) = infer tenv then_e in
            let (_, t_else) = infer tenv else_e in
            unify t_then t_else pos;
            (tenv, t_then)
        | (ELambda ("()", body), _) ->
            debug_type @@ "infer lambda () -> " ^ s_expr body;
            let (_, t_body) = infer tenv body in
            (tenv, TFun (TUnit, t_body))
        | (ELambda ("_", body), pos) ->
            debug_type @@ "infer lambda _ -> " ^ s_expr body;
            let t_arg = new_tvar () in
            let (_, t_body) = infer tenv body in
            (tenv, TFun (t_arg, t_body))
        | (ELambda (arg, body), pos) ->
            debug_type @@ "infer lambda " ^ arg  ^ " -> " ^ s_expr body;
            let t_arg = new_tvar () in
            let ts = new_type_schema t_arg in
            let tenv = Env.extend arg (ref ts) tenv in
            let (_, t_body) = infer tenv body in
            (tenv, TFun (t_arg, t_body))
        | (EApply (fn, arg), pos) ->
            debug_type @@ "infer apply " ^ s_expr fn ^ ", " ^ s_expr arg;
            let (_, t_fn) = infer tenv fn in
            let (_, t_arg) = infer tenv arg in
            let t = new_tvar () in
            unify t_fn (TFun (t_arg, t)) pos;
            (tenv, t)
        | (ELet (id, e), _) ->
            debug_type @@ "infer let " ^ id ^ " = " ^ s_expr e;
            let (_, t) = infer tenv e in
            let ts = create_poly_type t in
            let tenv = Env.extend id (ref ts) tenv in
            (tenv, TUnit)
        | (ELetRec (id, e), _) ->
            debug_type @@ "infer letrec " ^ id ^ " = " ^ s_expr e;
            let r = ref (new_type_schema (new_tvar ())) in
            let tenv = Env.extend id r tenv in
            let (_, t) = infer tenv e in
            r := create_poly_type t;
            (tenv, TUnit)
        | (ESeq el, _) ->
            debug_type @@ "infer seq " ^ s_list s_expr "; " el;
            let rec loop tenv = function
                | [] -> (tenv, TUnit)
                | x::[] ->
                    infer tenv x
                | x::xs ->
                    let (tenv, t) = infer tenv x in
                    if t <> TUnit then error (snd x) "expression should have type unit";
                    loop tenv xs
            in loop tenv el
    in
    debug_type_out @@ "infer = " ^ s_typ_raw (snd res);
    res


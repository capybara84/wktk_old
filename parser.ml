open Syntax

let debug_scope_flag = ref false
let debug_indent = ref 0

let rec debug_show_space = function
    | 0 -> ()
    | n -> print_char ' '; debug_show_space (n-1)

let debug_parse s =
    if !debug_scope_flag then
        (debug_show_space !debug_indent; print_endline s)

let debug_parse_in s =
    if !debug_scope_flag then
        (debug_parse @@ "IN " ^ s; incr debug_indent)

let debug_parse_out s =
    if !debug_scope_flag then
        (decr debug_indent; debug_parse @@ "OUT " ^ s)

type parser = {
    mutable toks : token list;
}

let create_parser toks = { toks = toks }

let is_eof pars = pars.toks = []

let get_pos pars =
    if is_eof pars then
        { filename = ""; line = 0; col = 0 }
    else
        snd @@ List.hd pars.toks

let make_expr edecl pos = (edecl, pos)

let next_token pars =
    pars.toks <- List.tl pars.toks

let peek_token pars =
    if is_eof pars then Eof
    else fst @@ List.hd pars.toks

let rec skip_newline pars =
    if peek_token pars = Newline then
       (next_token pars; skip_newline pars)

let rec skip_semi pars =
    if peek_token pars = Semi then
        next_token pars

let parse_error pars msg =
    let at_token =
        match peek_token pars with
        | Eof -> ""
        | x -> " at token '" ^ s_token x ^ "'"
    in
    error (get_pos pars) (msg ^ at_token)

let expect pars tok =
    if peek_token pars <> tok then
        parse_error pars ("expect '" ^ s_token tok ^ "'")
    else
        next_token pars

let is_apply e pars =
    match e with
    | (EParen _, _)
    | (ELambda _, _)
    | (EApply _, _)
    | (EId _, _) ->
        (match peek_token pars with
            | Null
            | Id _
            | Lit _
            | LParen
            | LBracket
            | Unit
                -> true
            | _ -> false)
    | _ -> false

let token_to_op = function
    | DColon -> BinCons | LOr -> BinLOr | LAnd -> BinLAnd | Eql -> BinEql | Neq -> BinNeq
    | LT -> BinLT | LE -> BinLE | GT -> BinGT | GE -> BinGE | Plus -> BinAdd | Minus -> BinSub
    | Star -> BinMul | Slash -> BinDiv | Percent -> BinMod | Op op -> BinOp op
    | _ -> failwith "bug"

let priority = function
    | BinLAnd | BinLOr -> -2
    | BinEql | BinNeq | BinLT | BinLE | BinGT | BinGE -> -1
    | BinAdd | BinSub -> 0
    | BinMul | BinDiv | BinMod -> 1
    | _ -> 4

let can_swap op1 op2 =
    let p1 = priority op1 in
    let p2 = priority op2 in
    if p1 < p2 then
        true
    else if p1 = p2 then
        op1 <> BinCons
    else
        false

let rec make_binop op e1 e2 =
    let pos = snd e1 in
    match e2 with
    | (EBinary (op2, _e1, _e2), _) when can_swap op2 op ->
        let e = make_binop op e1 _e1 in
        (EBinary (op2, e, _e2), pos)
    | _ -> (EBinary (op, e1, e2), pos)

(*
list_expr
    = '[' [expr {',' expr}] ']'
*)
let rec parse_list_expr pars =
    debug_parse_in @@ "parse_list_expr: " ^ s_token_src_list pars.toks;
    let rec aux acc pars =
        match peek_token pars with
        | RBracket ->
            next_token pars;
            List.rev acc
        | _ ->
            let e = parse_expr pars in
            if peek_token pars = Comma then begin
                next_token pars;
                skip_newline pars;
                aux (e :: acc) pars
            end else begin
                expect pars RBracket;
                List.rev (e :: acc)
            end
    in
    let pos = get_pos pars in
    next_token pars;
    skip_newline pars;
    let el = aux [] pars in
    let res = List.fold_right (fun a b -> make_expr (EBinary (BinCons, a, b)) pos) el (ENull, pos)
    in
    debug_parse_out @@ "parse_list_expr: " ^ s_expr_src res;
    res

(*
id_expr
    = ID
*)
and parse_id_expr id pos pars =
    debug_parse_in @@ "parse_id_expr: " ^ id ^ " " ^ s_token_src_list pars.toks;
    let res = make_expr (EId id) pos
    in
    debug_parse_out @@ "parse_id_expr: " ^ s_expr_src res;
    res

(*
simple_expr
    = id_expr
    | BOOL_LIT | INT_LIT | CHAR_LIT | FLOAT_LIT | STRING_LIT 
    | list_expr | '[]' | '(' expr {',' expr} ')'
*)
and parse_simple_expr pars =
    debug_parse_in @@ "parse_simple_expr: " ^ s_token_src_list pars.toks;
    let pos = get_pos pars in
    let res =
        match peek_token pars with
        | Id id ->          next_token pars; parse_id_expr id pos pars
        | LBracket ->       parse_list_expr pars
        | Null ->
            debug_parse "parse_simple_expr Null";
            next_token pars; make_expr ENull pos
        | Unit ->
            debug_parse "parse_simple_expr Unit";
            next_token pars; make_expr EUnit pos
        | Lit lit ->
            debug_parse "parse_simple_expr literal";
            next_token pars;
            make_expr (ELit lit) pos
        | LParen ->
            debug_parse "parse_simple_expr ( )";
            next_token pars;
            skip_newline pars;
            let expr = parse_expr pars in
            if peek_token pars = Comma then begin
                let rec loop lst =
                    let e = parse_expr pars in
                    if peek_token pars = Comma then begin
                        next_token pars;
                        skip_newline pars;
                        loop (e :: lst)
                    end else
                        List.rev (e :: lst)
                in
                let pos = get_pos pars in
                next_token pars;
                skip_newline pars;
                let e2 = loop [] in
                expect pars RParen;
                make_expr (ETuple (expr::e2)) pos
            end else begin
                expect pars RParen;
                make_expr (EParen expr) pos
            end
        | Eof ->
            debug_parse "EOF";
            raise End_of_file
        | _ -> parse_error pars "syntax error"
    in
    debug_parse_out @@ "parse_simple_expr: " ^ s_expr_src res;
    res

(*
unary_expr
    = [unary_op] simple_expr
*)
and parse_unary_expr pars =
    debug_parse_in @@ "parse_unary_expr: " ^ s_token_src_list pars.toks;
    let pos = get_pos pars in
    let op =
        match peek_token pars with
        | Not -> next_token pars; Some UNot
        | Minus -> next_token pars; Some UMinus
        | _ -> None
    in
    let expr = parse_simple_expr pars in
    let res =
        match op with
        | None -> expr
        | Some uop -> make_expr (EUnary (uop, expr)) pos
    in
    debug_parse_out @@ "parse_unary_expr: " ^ s_expr_src res;
    res

(*
apply_expr
    = unary_expr {simple_expr}
*)
and parse_apply_expr pars =
    debug_parse_in @@ "parse_apply_expr: " ^ s_token_src_list pars.toks;
    let rec parse_apply_rhs lhs pars =
        let pos = get_pos pars in
        let expr = parse_simple_expr pars in
        let e = make_expr (EApply (lhs, expr)) pos in
        if is_apply e pars then parse_apply_rhs e pars
        else e
    in
    let e = parse_unary_expr pars in
    let res =
        if is_apply e pars then parse_apply_rhs e pars
        else e
    in
    debug_parse_out @@ "parse_apply_expr: " ^ s_expr_src res;
    res

(*
bin_expr
    = apply_expr {bin_op apply_expr}
*)
and parse_bin_expr pars =
    debug_parse_in @@ "parse_bin_expr: " ^ s_token_src_list pars.toks;
    let rec parse_rhs lhs pars =
        match peek_token pars with
        | DColon | LOr | LAnd | Eql | Neq | LT | LE | GT | GE | Plus
        | Minus | Star | Slash | Percent | Op _ as t ->
            let op = token_to_op t in
            next_token pars;
            skip_newline pars;
            let rhs = parse_expr pars in
            parse_rhs (make_binop op lhs rhs) pars
        | _ -> lhs
    in
    let exp = parse_apply_expr pars in
    let res = parse_rhs exp pars in
    debug_parse_out @@ "parse_bin_expr: " ^ s_expr_src res;
    res

(*
cond_expr
    = bin_expr ['?' expr ':' expr]
*)
and parse_cond_expr pars =
    debug_parse_in @@ "parse_cond_expr: " ^ s_token_src_list pars.toks;
    let pos = get_pos pars in
    let expr = parse_bin_expr pars in
    let res =
        if peek_token pars = Ques then begin
            next_token pars;
            skip_newline pars;
            let e2 = parse_expr pars in
            skip_newline pars;
            expect pars Colon;
            skip_newline pars;
            let e3 = parse_expr pars in
            make_expr (ECond (expr, e2, e3)) pos
        end else expr
    in
    debug_parse_out @@ "parse_cond_expr: " ^ s_expr_src res;
    res

(*
comp_expr
    = '{' {expr} '}'
*)
and parse_comp_expr pars =
    debug_parse_in @@ "parse_comp_expr: " ^ s_token_src_list pars.toks;
    let rec aux acc pars =
        skip_semi pars;
        skip_newline pars;
        if peek_token pars = RBrace then begin
            next_token pars;
            List.rev acc
        end else begin
            let expr = parse_expr pars in
            match peek_token pars with
            | RBrace ->
                next_token pars;
                List.rev (expr :: acc)
            | _ ->
                aux (expr :: acc) pars
        end
    in
    let pos = get_pos pars in
    next_token pars;
    skip_newline pars;
    let el = aux [] pars in
    let res = make_expr (ESeq el) pos in
    debug_parse_out @@ "parse_comp_expr: " ^ s_expr_src res;
    res

(*
fn_expr
    | FN params '->' expr
*)
and parse_fn_expr pars =
    debug_parse_in @@ "parse_fn_expr: " ^ s_token_src_list pars.toks;
    let pos = get_pos pars in
    next_token pars;
    let ids = parse_params [] pars in
    expect pars RArrow;
    skip_newline pars;
    let body = parse_expr pars in
    let res = List.fold_right (fun a b -> make_expr (ELambda (a, b)) pos) ids body in
    debug_parse_out @@ "parse_fn_expr: " ^ s_expr_src res;
    res

(*
if_expr
    = IF expr THEN expr [ELSE expr]
*)
and parse_if_expr pars =
    debug_parse_in @@ "parse_if_expr: " ^ s_token_src_list pars.toks;
    let pos = get_pos pars in
    next_token pars;
    let e_cond = parse_expr pars in
    skip_newline pars;
    expect pars Then;
    skip_newline pars;
    let e_then = parse_expr pars in
    skip_newline pars;
    let e_else =
        if peek_token pars = Else then begin
            skip_newline pars;
            next_token pars;
            parse_expr pars
        end else make_expr EUnit pos
    in
    skip_newline pars;
    let res = (ECond (e_cond, e_then, e_else), pos)
    in
    debug_parse_out @@ "parse_if_expr: " ^ s_expr_src res;
    res

(*
let_expr
    = LET id_def
*)
and parse_let_expr pars =
    debug_parse_in @@ "parse_let_expr: " ^ s_token_src_list pars.toks;
    next_token pars;
    let g =  
        if peek_token pars = Rec then
            (next_token pars; true)
        else false
    in
    let res = parse_id_def g pars in
    debug_parse_out @@ "parse_let_expr: " ^ s_expr_src res;
    res

(*
expr
    = let_expr
    | if_expr
    | fn_expr
    | comp_expr
    | cond_expr
    | ';'
    | NEWLINE
*)
and parse_expr pars =
    debug_parse_in @@ "parse_expr: " ^ s_token_src_list pars.toks;
    let res =
        match peek_token pars with
        | Let -> parse_let_expr pars
        | If -> parse_if_expr pars
        | Fn -> parse_fn_expr pars
        | LBrace -> parse_comp_expr pars
        | Newline | Semi -> next_token pars; parse_expr pars
        | _ -> parse_cond_expr pars
    in
    debug_parse_out @@ "parse_expr: " ^ s_expr_src res;
    res

(*
params
    = {ID | '_' | '()'}
*)
and parse_params acc pars =
    debug_parse_in @@ "parse_params: " ^ s_token_src_list pars.toks;
    let res =
        let pos = get_pos pars in
        match peek_token pars with
        | Unit ->
            next_token pars;
            parse_params ((EUnit, pos) :: acc) pars
        | Id id ->
            next_token pars;
            parse_params ((EId id, pos) :: acc) pars
        | _ -> List.rev acc
    in
    debug_parse_out @@ "parse_params: [" ^ s_list s_expr ";" res ^ "]";
    res

(*
id_def
    = ID {params} '=' expr
*)
and parse_id_def global pars =
    debug_parse_in @@ "parse_id_def: " ^ s_token_src_list pars.toks;
    let pos = get_pos pars in
    let res =
        match peek_token pars with
        | Id id ->
            next_token pars;
            let params = parse_params [] pars in
            expect pars Eq;
            skip_newline pars;
            let body = parse_expr pars in
            begin
                match params with
                | [] when not global -> make_expr (ELet (id, body)) pos
                | [] when global -> make_expr (ELetRec (id, body)) pos
                | _ -> 
                    make_expr (ELetRec (id, List.fold_right (fun a b -> make_expr (ELambda (a, b)) pos) params body)) pos
            end
        | _ -> parse_error pars "expect identifier"
    in
    debug_parse_out @@ "parse_id_def: " ^ s_expr_src res;
    res

(*
program
    = {id_def}
*)
let parse_program pars =
    debug_parse_in @@ "parse_program: " ^ s_token_src_list pars.toks;
    let rec aux acc pars =
        debug_parse "parse_program aux";
        if is_eof pars then List.rev acc
        else if peek_token pars = Semi || peek_token pars = Newline then
            (next_token pars; aux acc pars)
        else
            let e = parse_id_def true pars in
            aux (e :: acc) pars
    in
    let res = aux [] pars in
    debug_parse_out "parse_program";
    res

let parse toks =
    let pars = create_parser toks in
    parse_program pars



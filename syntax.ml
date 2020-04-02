
type source_pos = {
    filename : string;
    line : int;
    col : int;
}

exception Error of source_pos * string

type lit = Bool of bool | Int of int | Char of char | Float of float | String of string

type token_decl =
    | Eof | Newline | Id of string | Lit of lit | Op of string | Let | If | Then | Else | Fn
    | Semi | Colon | DColon | Comma | Dot | Null | Unit | Vertical | Ques | Eq | LOr | LAnd
    | Eql | Neq | LT | LE | GT | GE | Plus | Minus | Star | Slash | Percent | Not
    | LBrace | RBrace | LParen | RParen | LBracket | RBracket | RArrow

type token = token_decl * source_pos

type typ =
    | TUnit | TBool | TInt | TChar | TFloat | TString
    | TList of typ
    | TFun of typ * typ
    | TVar of int * typ option ref
and type_schema = {
    vars : int list;
    body : typ;
}

type binop = BinAdd | BinSub | BinMul | BinDiv | BinMod | BinLT | BinLE | BinGT | BinGE
    | BinEql | BinNeq | BinLOr | BinLAnd | BinCons | BinOp of string
type unop = UNot | UMinus

type expr_decl =
    | ENull | EUnit
    | ELit of lit
    | EId of string
    | EUnary of unop * expr
    | EBinary of binop * expr * expr
    | ECond of expr * expr * expr
    | ELambda of string * expr
    | EApply of expr * expr
    | ELet of string * expr
    | ELetRec of string * expr
    | ESeq of expr list

and expr = expr_decl * source_pos

type value =
    | VUnit | VNull | VBool of bool | VInt of int | VChar of char
    | VFloat of float | VString of string
    | VCons of value * value
    | VClosure of string * expr * (value ref) Env.t
    | VBuiltin of (value -> value)

let error pos msg = raise (Error (pos, msg))

let s_pos pos = Printf.sprintf "%s, line=%d, col=%d: " pos.filename pos.line pos.col

let id x = x

let quote x = "\"" ^ x ^ "\""

let escape_char = function
    | '\n' -> "\\n"
    | '\t' -> "\\t"
    | '\r' -> "\\r"
    | '\\' -> "\\\\"
    | c ->
        if c >= '\032' && c <= '\126' then
            String.make 1 c
        else
            Printf.sprintf "\\%.3d" (int_of_char c)

let escape_str s =
    let buffer = Buffer.create (String.length s) in
    for i = 0 to (String.length s) - 1 do
        Buffer.add_string buffer @@ escape_char s.[i]
    done;
    Buffer.contents buffer


let s_lit = function
    | Bool b -> string_of_bool b
    | Int i -> string_of_int i
    | Char c -> "'" ^ escape_char c ^ "'"
    | Float f -> string_of_float f
    | String s -> quote @@ escape_str s

let rec s_list to_s sep = function
    | [] -> ""
    | [x] -> to_s x
    | x::xs -> to_s x ^ sep ^ s_list to_s sep xs


let s_token = function
    | Eof -> "<EOF>" | Newline -> "<NEWLINE>" | Id id -> id | Lit l -> s_lit l | Op s -> s
    | Let -> "let" | If -> "if" | Then -> "then" | Else -> "else" | Fn -> "fn"
    | Semi -> ";" | Colon -> ":" | DColon -> "::" | Comma -> "," | Dot -> "." | Null -> "[]"
    | Unit -> "()" | Vertical -> "|" | Ques -> "?" | Eq -> "=" | LOr -> "||" | LAnd -> "&&"
    | Eql -> "==" | Neq -> "!=" | LT -> "<" | LE -> "<=" | GT -> ">" | GE -> ">="
    | Plus -> "+" | Minus -> "-" | Star -> "*" | Slash -> "/" | Percent -> "%" | Not -> "!"
    | LBrace -> "{" | RBrace -> "}" | LParen -> "(" | RParen -> ")" | LBracket -> "["
    | RBracket -> "]" | RArrow -> "->" 

let int_to_alpha x =
    if x <= Char.code 'z' - Char.code 'a' then
        String.make 1 (Char.chr ((Char.code 'a') + x))
    else
        string_of_int x

let s_typ ty =
    let counter = ref 0 in
    let dic = ref [] in
    let rec to_s n ty =
(*
        let s_typ_list tl =
            let rec aux = function
                | [] -> ""
                | x::[] -> to_s 0 x
                | x::xs ->
                    let s = to_s 0 x in
                    s ^ ", " ^ aux xs
            in
            match tl with
            | [] -> ""
            | x::[] -> to_s 0 x ^ " "
            | _::_ -> "(" ^ aux tl ^ ") " 
        in
*)
        let (m, str) =
            match ty with
            | TUnit -> (3, "unit") | TBool -> (3, "bool") | TInt -> (3, "int") | TChar -> (3, "char")
            | TFloat -> (3, "float") | TString -> (3, "string")
            | TList t -> (3, to_s 0 t ^ " list")
            | TFun (t1, t2) ->
                let s1 = to_s 1 t1 in
                let s2 = to_s 0 t2 in
                (1, s1 ^ " -> " ^ s2)
            | TVar (x, {contents = None}) ->
                let y = try List.assoc x !dic
                    with Not_found ->
                        dic := (x, !counter) :: !dic;
                        let n = !counter in
                        incr counter;
                        n
                in
                (3, "'" ^ int_to_alpha y)
            | TVar (_, {contents = Some t}) ->
                (3, to_s n t)
        in
        if m > n then str
        else "(" ^ str ^ ")"
    in to_s (-1) ty

let s_typ_raw ty =
    let rec to_s n ty =
(*
        let s_typ_list tl =
            let rec aux = function
                | [] -> ""
                | x::[] -> to_s 0 x
                | x::xs ->
                    let s = to_s 0 x in
                    s ^ ", " ^ aux xs
            in
            match tl with
            | [] -> ""
            | x::[] -> to_s 0 x ^ " "
            | _::_ -> "(" ^ aux tl ^ ") " 
        in
*)
        let (m, str) =
            match ty with
            | TUnit -> (3, "unit") | TBool -> (3, "bool") | TInt -> (3, "int") | TChar -> (3, "char")
            | TFloat -> (3, "float") | TString -> (3, "string")
            | TList t -> (3, to_s 0 t ^ " list")
            | TFun (t1, t2) ->
                let s1 = to_s 1 t1 in
                let s2 = to_s 0 t2 in
                (1, s1 ^ " -> " ^ s2)
            | TVar (x, {contents = None}) ->
                (3, "'" ^ string_of_int x)
            | TVar (_, {contents = Some t}) ->
                (3, to_s n t ^ "!")
        in
        if m > n then str
        else "(" ^ str ^ ")"
    in to_s (-1) ty

let s_type_schema ts =
    let rec s_list = function
        | [] -> ""
        | x::[] -> string_of_int x
        | x::xs -> string_of_int x ^ "," ^ s_list xs
    in
    "{ vars:[" ^ s_list ts.vars ^ "], body:" ^ s_typ_raw ts.body ^ " }"

let s_binop = function
    | BinAdd -> "+" | BinSub -> "-" | BinMul -> "*" | BinDiv -> "/" | BinMod -> "%"
    | BinLT -> "<" | BinLE -> "<=" | BinGT -> ">" | BinGE -> ">=" | BinEql -> "=="
    | BinNeq -> "!=" | BinLOr -> "||" | BinLAnd -> "&&" | BinCons -> "::"
    | BinOp op -> op
let s_unop = function UNot -> "!" | UMinus -> "-"

let rec s_expr = function
    | (ENull, _) -> "[]" | (EUnit, _) -> "()"
    | (ELit l, _) -> s_lit l | (EId s, _) -> s
    | (EUnary (op, e), _) -> "(unary '" ^ s_unop op ^ "' " ^ s_expr e ^ ")"
    | (EBinary (op, l, r), _) -> "(binary '" ^ s_binop op ^ "' " ^ s_expr l ^ " " ^ s_expr r ^ ")"
    | (ECond (c, t, e), _) -> "(cond " ^ s_expr c ^ " then " ^ s_expr t ^ " else " ^ s_expr e ^ ")"
    | (ELambda (a, b), _) -> "(lambda " ^ a ^ " -> " ^ s_expr b ^ ")"
    | (EApply (f, a), _) -> "(apply " ^ s_expr f ^ " " ^ s_expr a ^ ")"
    | (ELet (s, e), _) -> "(let " ^ s ^ " = " ^ s_expr e ^ ")"
    | (ELetRec (s, e), _) -> "(letrec " ^ s ^ " = " ^ s_expr e ^ ")"
    | (ESeq el, _) -> "{ " ^ s_exprlist "; " el ^ " }"
and s_exprlist sep = s_list s_expr sep

let s_vlit = function
    | Bool b -> string_of_bool b
    | Int i -> string_of_int i
    | Char c -> String.make 1 c
    | Float f -> string_of_float f
    | String s -> s

let rec s_value = function
    | VUnit -> "()"
    | VNull -> "[]"
    | VBool b -> string_of_bool b
    | VInt n -> string_of_int n
    | VChar c -> String.make 1 c
    | VFloat f -> string_of_float f
    | VString s -> s
    | VCons (car, cdr) -> s_value car ^ "::" ^ s_value cdr
    | VClosure _ -> "<closure>"
    | VBuiltin _ -> "<builtin>"


let s_lit_src = function
    | Bool b -> "Bool " ^ string_of_bool b
    | Int i -> "Int " ^ string_of_int i
    | Char c -> "Char '" ^ escape_char c ^ "'"
    | Float f -> "Float " ^ string_of_float f
    | String s -> "String " ^ quote @@ escape_str s

let s_token_src = function
    | Eof -> "Eof" | Newline -> "Newline" | Id id -> "Id " ^ quote id
    | Lit l -> "Lit (" ^ s_lit_src l ^ ")" | Op s -> "Op " ^ quote s
    | Let -> "Let" | If -> "If" | Then -> "Then" | Else -> "Else" | Fn -> "Fn"
    | Semi -> "Semi" | Colon -> "Colon" | DColon -> "DColon" | Comma -> "Comma"
    | Dot -> "Dot" | Null -> "Null" | Unit -> "Unit" | Vertical -> "Vertical"
    | Ques -> "Ques" | Eq -> "Eq" | LOr -> "LOr" | LAnd -> "LAnd"
    | Eql -> "Eql" | Neq -> "Neq" | LT -> "LT" | LE -> "LE" | GT -> "GT" | GE -> "GE"
    | Plus -> "Plus" | Minus -> "Minus" | Star -> "Star" | Slash -> "Slash"
    | Percent -> "Percent" | Not -> "Not"
    | LBrace -> "LBrace" | RBrace -> "RBrace" | LParen -> "LParen"
    | RParen -> "RParen" | LBracket -> "LBracket" | RBracket -> "RBracket" | RArrow -> "RArrow" 

let s_token_src_list toks = "[" ^ s_list (fun (x, _) -> s_token_src x) "; " toks ^ "]"

let s_binop_src = function
    | BinAdd -> "BinAdd" | BinSub -> "BinSub" | BinMul -> "BinMul" | BinDiv -> "BinDiv" | BinMod -> "BinMod"
    | BinLT -> "BinLT" | BinLE -> "BinLE" | BinGT -> "BinGT" | BinGE -> "BinGE" | BinEql -> "BinEql"
    | BinNeq -> "BinNeq" | BinLOr -> "BinLOr" | BinLAnd -> "BinLAnd" | BinCons -> "BinCons"
    | BinOp op -> "BinOp " ^ quote op
let s_unop_src = function UNot -> "UNot" | UMinus -> "UMinus"

let rec s_expr_src = function
    | (ENull, _) -> "ENull" | (EUnit, _) -> "EUnit"
    | (ELit l, _) -> "(ELit (" ^ s_lit_src l ^ "))" | (EId s, _) -> "(EId " ^ quote s ^ ")"
    | (EUnary (op, e), _) -> "(EUnary (" ^ s_unop_src op ^ ", " ^ s_expr_src e ^ "))"
    | (EBinary (op, l, r), _) -> "(EBinary (" ^ s_binop_src op ^ ", " ^ s_expr_src l ^ ", " ^ s_expr_src r ^ "))"
    | (ECond (c, t, e), _) -> "(ECond (" ^ s_expr_src c ^ ", " ^ s_expr_src t ^ ", " ^ s_expr_src e ^ "))"
    | (ELambda (a, b), _) -> "(ELambda (" ^ quote a ^ ", " ^ s_expr_src b ^ "))"
    | (EApply (f, a), _) -> "(EApply (" ^ s_expr_src f ^ ", " ^ s_expr_src a ^ "))"
    | (ELet (s, e), _) -> "(ELet (" ^ quote s ^ ", " ^ s_expr_src e ^ "))"
    | (ELetRec (s, e), _) -> "(ELetRec (" ^ quote s ^ ", " ^ s_expr_src e ^ "))"
    | (ESeq el, _) -> "(ESeq " ^ s_exprlist_src el ^ ")"
and s_exprlist_src el = "[" ^ s_list s_expr_src "; " el ^ "]"


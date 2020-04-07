
type source_pos = {
    filename : string;
    line : int;
    col : int;
}

exception Error of source_pos * string

type lit = Bool of bool | Int of int | Char of char | Float of float | String of string

type token_decl =
    | Eof | Newline | Id of string | CId of string | Lit of lit | Op of string
    | Module | Import | As | Let | Rec | If | Then | Else | Fn
    | Semi | Colon | DColon | Comma | Dot | Null | Unit | Vertical | Ques | Eq | LOr | LAnd
    | Eql | Neq | LT | LE | GT | GE | Plus | Minus | Star | Slash | Percent | Not
    | LBrace | RBrace | LParen | RParen | LBracket | RBracket | RArrow

type token = token_decl * source_pos

type typ =
    | TUnit | TBool | TInt | TChar | TFloat | TString
    | TTuple of typ list | TList of typ
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
    | EModId of string list * string
    | ETuple of expr list
    | EParen of expr
    | EUnary of unop * expr
    | EBinary of binop * expr * expr
    | ECond of expr * expr * expr
    | ELambda of expr * expr
    | EApply of expr * expr
    | ELet of string * expr
    | ELetRec of string * expr
    | ESeq of expr list
    | EModule of string
    | EImport of string * string option

and expr = expr_decl * source_pos

type tenv = (type_schema ref) Env.t

type value =
    | VUnit | VNull | VBool of bool | VInt of int | VChar of char
    | VFloat of float | VString of string
    | VTuple of value list | VCons of value * value
    | VClosure of expr * expr * env
    | VBuiltin of (source_pos -> env -> value -> (env * value))
and env = (value ref) Env.t

type symtab = {
    mutable env : env;
    mutable tenv : tenv;
}

let make_table env tenv = { env = env; tenv = tenv }


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
    | Eof -> "<EOF>" | Newline -> "<NEWLINE>" | Id s -> s | CId s -> s | Lit l -> s_lit l | Op s -> s
    | Module -> "module" | Import -> "import" | As -> "as"
    | Let -> "let" | Rec -> "rec" | If -> "if" | Then -> "then" | Else -> "else" | Fn -> "fn"
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
        let (m, str) =
            match ty with
            | TUnit -> (5, "unit") | TBool -> (5, "bool") | TInt -> (5, "int") | TChar -> (5, "char")
            | TFloat -> (5, "float") | TString -> (5, "string")
            | TTuple tl -> (3, s_list (to_s 4) " * " tl)
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
                (5, "'" ^ int_to_alpha y)
            | TVar (_, {contents = Some t}) ->
                (3, to_s n t)
        in
        if m > n then str
        else "(" ^ str ^ ")"
    in to_s (-1) ty

let s_typ_raw ty =
    let rec to_s n ty =
        let (m, str) =
            match ty with
            | TUnit -> (5, "unit") | TBool -> (5, "bool") | TInt -> (5, "int") | TChar -> (5, "char")
            | TFloat -> (5, "float") | TString -> (5, "string")
            | TTuple tl -> (3, s_list (to_s 4) " * " tl)
            | TList t -> (3, to_s 0 t ^ " list")
            | TFun (t1, t2) ->
                let s1 = to_s 1 t1 in
                let s2 = to_s 0 t2 in
                (1, s1 ^ " -> " ^ s2)
            | TVar (x, {contents = None}) ->
                (5, "'" ^ string_of_int x)
            | TVar (_, {contents = Some t}) ->
                (3, to_s n t ^ "!")
        in
        if m > n then str
        else "(" ^ str ^ ")"
    in to_s (-1) ty

let s_type_schema ts =
    "{ vars:[" ^ s_list string_of_int "," ts.vars ^ "], body:" ^ s_typ_raw ts.body ^ " }"

let s_binop = function
    | BinAdd -> "+" | BinSub -> "-" | BinMul -> "*" | BinDiv -> "/" | BinMod -> "%"
    | BinLT -> "<" | BinLE -> "<=" | BinGT -> ">" | BinGE -> ">=" | BinEql -> "=="
    | BinNeq -> "!=" | BinLOr -> "||" | BinLAnd -> "&&" | BinCons -> "::"
    | BinOp op -> op
let s_unop = function UNot -> "!" | UMinus -> "-"

let rec s_expr = function
    | (ENull, _) -> "[]" | (EUnit, _) -> "()"
    | (ELit l, _) -> s_lit l | (EId s, _) -> s | (EModId (ml, s), _) -> s_list id "." ml ^ "." ^ s
    | (ETuple el, _) -> "(" ^ s_list s_expr ", " el ^ ")"
    | (EParen e, _) -> "(" ^ s_expr e ^ ")"
    | (EUnary (op, e), _) -> "(unary '" ^ s_unop op ^ "' " ^ s_expr e ^ ")"
    | (EBinary (op, l, r), _) -> "(binary '" ^ s_binop op ^ "' " ^ s_expr l ^ " " ^ s_expr r ^ ")"
    | (ECond (c, t, e), _) -> "(cond " ^ s_expr c ^ " then " ^ s_expr t ^ " else " ^ s_expr e ^ ")"
    | (ELambda (a, b), _) -> "(lambda " ^ s_expr a ^ " -> " ^ s_expr b ^ ")"
    | (EApply (f, a), _) -> "(apply " ^ s_expr f ^ " " ^ s_expr a ^ ")"
    | (ELet (s, e), _) -> "(let " ^ s ^ " = " ^ s_expr e ^ ")"
    | (ELetRec (s, e), _) -> "(letrec " ^ s ^ " = " ^ s_expr e ^ ")"
    | (ESeq el, _) -> "{ " ^ s_exprlist "; " el ^ " }"
    | (EModule mid, _) -> "(module " ^ mid ^ ")"
    | (EImport (mid, None), _) -> "(import " ^ mid ^ ")"
    | (EImport (mid, Some aid), _) -> "(import " ^ mid ^ " as " ^ aid ^ ")"
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
    | VTuple vl -> "(" ^ s_list s_value ", " vl ^ ")"
    | VCons (car, cdr) -> "(" ^ s_value car ^ "::" ^ s_value cdr ^ ")"
    | VClosure _ -> "<closure>"
    | VBuiltin _ -> "<builtin>"

let s_lit_src = function
    | Bool b -> "Bool " ^ string_of_bool b
    | Int i -> "Int " ^ string_of_int i
    | Char c -> "Char '" ^ escape_char c ^ "'"
    | Float f -> "Float " ^ string_of_float f
    | String s -> "String " ^ quote @@ escape_str s

let s_token_src = function
    | Eof -> "Eof" | Newline -> "Newline" | Id s -> "Id " ^ quote s | CId s -> "CId " ^ quote s
    | Lit l -> "Lit (" ^ s_lit_src l ^ ")" | Op s -> "Op " ^ quote s
    | Module -> "Module" | Import -> "Import" | As -> "As"
    | Let -> "Let" | Rec -> "Rec" | If -> "If" | Then -> "Then" | Else -> "Else" | Fn -> "Fn"
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
    | (EModId (ml, s), _) -> "(EModId ([" ^ s_list quote ";" ml ^ "]," ^ quote s ^ "))"
    | (ETuple el, _) -> "(ETuple [" ^ s_list s_expr_src "; " el ^ "])"
    | (EParen e, _) -> "(EParen " ^ s_expr_src e ^ ")"
    | (EUnary (op, e), _) -> "(EUnary (" ^ s_unop_src op ^ ", " ^ s_expr_src e ^ "))"
    | (EBinary (op, l, r), _) -> "(EBinary (" ^ s_binop_src op ^ ", " ^ s_expr_src l ^ ", " ^ s_expr_src r ^ "))"
    | (ECond (c, t, e), _) -> "(ECond (" ^ s_expr_src c ^ ", " ^ s_expr_src t ^ ", " ^ s_expr_src e ^ "))"
    | (ELambda (a, b), _) -> "(ELambda (" ^ s_expr_src a ^ ", " ^ s_expr_src b ^ "))"
    | (EApply (f, a), _) -> "(EApply (" ^ s_expr_src f ^ ", " ^ s_expr_src a ^ "))"
    | (ELet (s, e), _) -> "(ELet (" ^ quote s ^ ", " ^ s_expr_src e ^ "))"
    | (ELetRec (s, e), _) -> "(ELetRec (" ^ quote s ^ ", " ^ s_expr_src e ^ "))"
    | (ESeq el, _) -> "(ESeq " ^ s_exprlist_src el ^ ")"
    | (EModule mid, _) -> "(EModule " ^ quote mid ^ ")"
    | (EImport (mid, None), _) -> "(EImport (" ^ quote mid ^ ", None))"
    | (EImport (mid, Some aid), _) -> "(EImport (" ^ quote mid ^ ", Some " ^ quote aid ^ "))"
and s_exprlist_src el = "[" ^ s_list s_expr_src "; " el ^ "]"


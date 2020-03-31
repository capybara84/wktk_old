
type source_pos = {
    line : int;
    col : int;
}

exception Error of string * source_pos * string

type lit = Int of int | Char of char | Float of float | String of string

type token_decl =
    | Eof | Newline | Id of string | Lit of lit | Op of string | Let | If | Then | Else | Fn
    | Semi | Colon | DColon | Comma | Dot | Null | Unit | Vertical | Ques | Eq | LOr | LAnd
    | Eql | Neq | LT | LE | GT | GE | Plus | Minus | Star | Slash | Percent | Not
    | LBrace | RBrace | LParen | RParen | LBracket | RBracket | RArrow

type token = token_decl * source_pos

type binop = BinAdd | BinSub | BinMul | BinDiv | BinMod | BinLT | BinLE | BinGT | BinGE
    | BinEql | BinNeq | BinLOr | BinLAnd | BinCons | BinOp of string
type unop = UNot | UMinus

type expr_decl =
    | ENull | EUnit
    | ESeq of expr list
    | EFunc of string * string * expr
    | ELet of string * expr
    | ECond of expr * expr * expr
    | ELambda of string * expr
    | EApply of expr * expr
    | EUnary of unop * expr
    | EBinary of binop * expr * expr
    | EId of string
    | ELit of lit

and expr = expr_decl * source_pos


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

let s_binop = function
    | BinAdd -> "+" | BinSub -> "-" | BinMul -> "*" | BinDiv -> "/" | BinMod -> "%"
    | BinLT -> "<" | BinLE -> "<=" | BinGT -> ">" | BinGE -> ">=" | BinEql -> "=="
    | BinNeq -> "!=" | BinLOr -> "||" | BinLAnd -> "&&" | BinCons -> "::"
    | BinOp op -> op
let s_unop = function UNot -> "!" | UMinus -> "-"

let rec s_expr = function
    | (ENull, _) -> "[]"
    | (EUnit, _) -> "()"
    | (ESeq el, _) -> "{ " ^ s_exprlist "; " el ^ " }"
    | (EFunc (s, a, e), _) -> "(func " ^ s ^ " " ^ a ^ " = " ^ s_expr e ^ ")"
    | (ELet (s, e), _) -> "(let " ^ s ^ " = " ^ s_expr e ^ ")"
    | (ECond (c, t, e), _) -> "(cond " ^ s_expr c ^ " then " ^ s_expr t ^ " else " ^ s_expr e ^ ")"
    | (ELambda (a, b), _) -> "(lambda " ^ a ^ " -> " ^ s_expr b ^ ")"
    | (EApply (f, a), _) -> "(apply " ^ s_expr f ^ " " ^ s_expr a ^ ")"
    | (EUnary (op, e), _) -> "(unary '" ^ s_unop op ^ "' " ^ s_expr e ^ ")"
    | (EBinary (op, l, r), _) -> "(binary '" ^ s_binop op ^ "' " ^ s_expr l ^ " " ^ s_expr r ^ ")"
    | (EId s, _) -> s
    | (ELit l, _) -> s_lit l
and s_exprlist sep = s_list s_expr sep

let s_lit_src = function
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
    | (ENull, _) -> "ENull"
    | (EUnit, _) -> "EUnit"
    | (ESeq el, _) -> "(ESeq " ^ s_exprlist_src el ^ ")"
    | (EFunc (s, a, e), _) -> "(EFunc (" ^ quote s ^ ", " ^ quote a ^ ", " ^ s_expr_src e ^ "))"
    | (ELet (s, e), _) -> "(ELet (" ^ quote s ^ ", " ^ s_expr_src e ^ "))"
    | (ECond (c, t, e), _) -> "(ECond (" ^ s_expr_src c ^ ", " ^ s_expr_src t ^ ", " ^ s_expr_src e ^ "))"
    | (ELambda (a, b), _) -> "(ELambda (" ^ quote a ^ ", " ^ s_expr_src b ^ "))"
    | (EApply (f, a), _) -> "(EApply (" ^ s_expr_src f ^ ", " ^ s_expr_src a ^ "))"
    | (EUnary (op, e), _) -> "(EUnary (" ^ s_unop_src op ^ ", " ^ s_expr_src e ^ "))"
    | (EBinary (op, l, r), _) -> "(EBinary (" ^ s_binop_src op ^ ", " ^ s_expr_src l ^ ", " ^ s_expr_src r ^ "))"
    | (EId s, _) -> "(EId " ^ quote s ^ ")"
    | (ELit l, _) -> "(ELit (" ^ s_lit_src l ^ "))"
and s_exprlist_src el = "[" ^ s_list s_expr_src "; " el ^ "]"


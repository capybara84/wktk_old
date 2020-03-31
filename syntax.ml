
type source_pos = {
    line : int;
    col : int;
}

exception Error of string * source_pos * string

type lit = Int of int | Char of char | Float of float | String of string

type token =
    | Eof | Newline | Id of string | Lit of lit | Op of string | Let | If | Then | Else | Fn
    | Semi | Colon | DColon | Comma | Dot | Null | Unit | Vertical | Ques | Eq | LOr | LAnd
    | Eql | Neq | LT | LE | GT | GE | Plus | Minus | Star | Slash | Percent | Not
    | LBrace | RBrace | LParen | RParen | LBracket | RBracket | RArrow

type expr =
    | ENull | EUnit
    | ESeq of expr list
    | EFunc of string * string * expr
    | ELet of string * expr
    | ECond of expr * expr * expr
    | ELambda of string * expr
    | EApply of expr * expr
    | EUnary of string * expr
    | EBinary of string * expr * expr
    | EId of string
    | EList of expr list


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


let src_lit = function
    | Int i -> "Int " ^ string_of_int i
    | Char c -> "Char '" ^ escape_char c ^ "'"
    | Float f -> "Float " ^ string_of_float f
    | String s -> "String " ^ quote @@ escape_str s

let s_token_src = function
    | Eof -> "Eof" | Newline -> "Newline" | Id id -> "Id " ^ quote id
    | Lit l -> "Lit (" ^ src_lit l ^ ")" | Op s -> "Op " ^ quote s
    | Let -> "Let" | If -> "If" | Then -> "Then" | Else -> "Else" | Fn -> "Fn"
    | Semi -> "Semi" | Colon -> "Colon" | DColon -> "DColon" | Comma -> "Comma"
    | Dot -> "Dot" | Null -> "Null" | Unit -> "Unit" | Vertical -> "Vertical"
    | Ques -> "Ques" | Eq -> "Eq" | LOr -> "LOr" | LAnd -> "LAnd"
    | Eql -> "Eql" | Neq -> "Neq" | LT -> "LT" | LE -> "LE" | GT -> "GT" | GE -> "GE"
    | Plus -> "Plus" | Minus -> "Minus" | Star -> "Star" | Slash -> "Slash"
    | Percent -> "Percent" | Not -> "Not"
    | LBrace -> "LBrace" | RBrace -> "RBrace" | LParen -> "LParen"
    | RParen -> "RParen" | LBracket -> "LBracket" | RBracket -> "RBracket" | RArrow -> "RArrow" 


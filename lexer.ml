open Syntax

type t = {
    text : string;
    len : int;
    pos : source_pos;
    current : int;
}
    
let init_lex filename text = {
    text = text;
    len = String.length text;
    pos = { filename = filename; line = 1; col = 1 };
    current = 0;
}


let lexer filename text =
    let is_op c = String.contains "~`!@$%^&*-+=|:<>?,./" c in
    let is_end lex = lex.current = lex.len in
    let peek lex = lex.text.[lex.current] in
    let next lex =
        if is_end lex then
            lex
        else
            let npos = { lex.pos with col = lex.pos.col + 1 } in
            { lex with pos = npos; current = lex.current + 1 }
    in
    let next_line lex =
        let npos = { lex.pos with line = lex.pos.line + 1; col = 1 } in
        { lex with pos = npos; current = lex.current + 1 }
    in
    let rec skip_newline lex =
        let lex = next_line lex in
        if not (is_end lex) && peek lex = '\n' then
            skip_newline lex
        else
            lex
    in
    let cut_token pred lex =
        let buffer = Buffer.create 5 in
        let rec aux lex =
            if is_end lex then
                lex
            else
                match peek lex with
                | ch when pred ch -> (
                    Buffer.add_char buffer ch;
                    aux (next lex))
                | _ -> lex
        in
        let lex = aux lex in
        (Buffer.contents buffer, lex)
    in
    let lex_ident lex =
        let is_ident = function 'a'..'z' | 'A'..'Z' | '0'..'9' | '_'  -> true | _ -> false in
        cut_token is_ident lex
    in
    let lex_number lex =
        (*TODO float *)
        let is_digit = function '0'..'9' -> true | _ -> false in
        let (s, lex) = cut_token is_digit lex in
        (Lit (Int (int_of_string s)), lex)
    in
    let get_char lex =
        match peek lex with
        | '\\' ->
            let lex = next lex in
                ((match peek lex with
                    | 'n' -> '\n'
                    | 'r' -> '\r'
                    | 't' -> '\t'
                    | ch -> ch),
                    (* TODO support '\123' style char *)
                next lex)
        | ch -> (ch, next lex)
    in
    let lex_char lex =
        let (c, lex) = get_char lex in
        if peek lex <> '\'' then
            error lex.pos "missing single-quote";
        (Lit (Char c), next lex)
    in
    let lex_string lex =
        let buffer = Buffer.create 10 in
        let rec aux lex =
            if is_end lex then
                error lex.pos "unterminated string";
            let (c, lex) = get_char lex in
            match c with
            | '"' ->
                lex
            | _ ->
                Buffer.add_char buffer c;
                aux lex
        in
        let lex = aux lex in
        (Lit (String (Buffer.contents buffer)), lex)
    in
    let lex_op lex = cut_token is_op lex in
    let rec get_tokens lex acc =
        let pos = lex.pos in
        let make_tokens pos t = ((t, pos) :: acc) in
        let rec skip_comment lex =
            if is_end lex then
                List.rev acc
            else if peek lex = '\n' then
                get_tokens (skip_newline lex) @@ make_tokens pos Newline
            else
                skip_comment (next lex)
        in
        let rec skip_nested_comment lex =
            let rec aux lex =
                if is_end lex then
                    error lex.pos "unexpected eof"
                else
                    match peek lex with
                    | '*' ->
                        let lex = next lex in
                        if peek lex = '/' then
                            next lex
                        else
                            aux lex
                    | '/' ->
                        let lex = next lex in
                        if peek lex = '*' then
                            aux (skip_nested_comment (next lex))
                        else
                            aux lex
                    | '\n' ->
                        aux (next_line lex);
                    | _ -> aux (next lex)
            in aux lex
        in
        let get_2char lex ch one two =
            let lex = next lex in
            if peek lex = ch then
                get_tokens (next lex) @@ make_tokens pos two
            else
                get_tokens lex @@ make_tokens pos one
        in
        if is_end lex then
            List.rev acc
        else
            match peek lex with
            | ' ' | '\t' | '\r' -> get_tokens (next lex) acc
            | '\n' -> get_tokens (skip_newline lex) @@ make_tokens pos Newline
            | '\'' ->
                let (t, lex) = lex_char (next lex) in
                get_tokens lex @@ make_tokens pos t
            | '"' ->
                let (t, lex) = lex_string (next lex) in
                get_tokens lex @@ make_tokens pos t
            | '0'..'9' ->
                let (t, lex) = lex_number lex in
                get_tokens lex @@ make_tokens pos t
            | 'A'..'Z' | 'a'..'z' | '_' ->
                let (id, lex) = lex_ident lex in
                let t = match id with
                    | "let" -> Let | "if" -> If | "then" -> Then | "else" -> Else
                    | "fn" -> Fn | _ -> Id id
                in
                get_tokens lex @@ make_tokens pos t
            | ';' -> get_tokens (next lex) @@ make_tokens pos Semi
            | '{' -> get_tokens (next lex) @@ make_tokens pos LBrace
            | '}' -> get_tokens (next lex) @@ make_tokens pos RBrace
            | ')' -> get_tokens (next lex) @@ make_tokens pos RParen
            | ']' -> get_tokens (next lex) @@ make_tokens pos RBracket
            | '(' -> get_2char lex ')' LParen Unit
            | '[' -> get_2char lex ']' LBracket Null
            | '/' ->
                let lex = next lex in
                if peek lex = '/' then
                    skip_comment (next lex)
                else if peek lex = '*' then
                    get_tokens (skip_nested_comment (next lex)) acc
                else
                    get_tokens lex @@ make_tokens pos Slash
            | c when is_op c ->
                let (op, lex) = lex_op lex in
                let tk = match op with
                    | ":" -> Colon | "::" -> DColon | "," -> Comma | "." -> Dot
                    | "|" -> Vertical | "?" -> Ques | "=" -> Eq | "||" -> LOr
                    | "&&" -> LAnd | "==" -> Eql | "!=" -> Neq | "<" -> LT
                    | "<=" -> LE | ">" -> GT | ">=" -> GE | "+" -> Plus
                    | "-" -> Minus | "*" -> Star | "/" -> Slash
                    | "%" -> Percent | "!" -> Not
                    | "->" -> RArrow | _ -> Op op
                in
                get_tokens lex @@ make_tokens pos tk
            | _ -> error lex.pos @@ "Unknown character '" ^ escape_char (peek lex) ^ "'"
    in
    get_tokens (init_lex filename text) []



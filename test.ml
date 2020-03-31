open Syntax
module L = Lexer
module P = Parser

let color c s = "\x1b[3" ^ (string_of_int c) ^ "m" ^ s ^ "\x1b[0m"
let red = color 1 and green = color 2 and yellow = color 3 and blue = color 4
and magenta = color 5 and cyan = color 6 and white = color 7

let n_ok = ref 0
let n_fail = ref 0

let test_ok () = incr n_ok; print_string @@ green "."
let test_fail s = incr n_fail; print_endline @@ red @@ "! " ^ s
let test_eq a b m = if a = b then test_ok () else test_fail m

let test_report () =
    let n_all = !n_ok + !n_fail in
    print_endline @@ "All   : " ^ string_of_int n_all;
    print_endline @@ "OK    : " ^ green @@ string_of_int !n_ok;
    print_endline @@ "Failed: " ^ if !n_fail = 0 then "0" else (red @@ string_of_int !n_fail)

let lexer_test_text = "
/* test
    /* nest */
*/

/*  6 */ identifier Ident 12345
/*  7 */ 'a' '\\t' \"abc\\n\"

// comment

/* 11 */ let if then else fn
/* 12 */ ; : :: , . [] | ? = || && == != < <= > >= + - * / % ! ()
/* 13 */ { } ( ) [ ] -> ,= ++ =/=
"
let lexer_test_tokens = [
    (Newline, {line=1;col=1});
    (Newline, {line=4;col=3});
    (Id "identifier", {line=6;col=10});
    (Id "Ident", {line=6;col=21});
    (Lit (Int 12345), {line=6;col=27});
    (Newline, {line=6;col=32});
    (Lit (Char 'a'), {line=7;col=10});
    (Lit (Char '\t'), {line=7;col=14});
    (Lit (String "abc\n"), {line=7;col=19});
    (Newline, {line=7;col=26});
    (Newline, {line=9;col=1});
    (Let, {line=11;col=10});
    (If, {line=11;col=14});
    (Then, {line=11;col=17});
    (Else, {line=11;col=22});
    (Fn, {line=11;col=27});
    (Newline, {line=11;col=29});
    (Semi, {line=12;col=10});
    (Colon, {line=12;col=12});
    (DColon, {line=12;col=14});
    (Comma, {line=12;col=17});
    (Dot, {line=12;col=19});
    (Null, {line=12;col=21});
    (Vertical, {line=12;col=24});
    (Ques, {line=12;col=26});
    (Eq, {line=12;col=28});
    (LOr, {line=12;col=30});
    (LAnd, {line=12;col=33});
    (Eql, {line=12;col=36});
    (Neq, {line=12;col=39});
    (LT, {line=12;col=42});
    (LE, {line=12;col=44});
    (GT, {line=12;col=47});
    (GE, {line=12;col=49});
    (Plus, {line=12;col=52});
    (Minus, {line=12;col=54});
    (Star, {line=12;col=56});
    (Slash, {line=12;col=58});
    (Percent, {line=12;col=60});
    (Not, {line=12;col=62});
    (Unit, {line=12;col=64});
    (Newline, {line=12;col=66});
    (LBrace, {line=13;col=10});
    (RBrace, {line=13;col=12});
    (LParen, {line=13;col=14});
    (RParen, {line=13;col=16});
    (LBracket, {line=13;col=18});
    (RBracket, {line=13;col=20});
    (RArrow, {line=13;col=22});
    (Op ",=", {line=13;col=25});
    (Op "++", {line=13;col=28});
    (Op "=/=", {line=13;col=31});
    (Newline, {line=13;col=34});
]

let src_token (tok, pos)  =
    Printf.sprintf "    (%s, {line=%d;col=%d});" (s_token_src tok) pos.line pos.col

let lexer_test verbose =
    print_string "Lexer Test: ";
    (try
        let tokens = L.lexer "test" lexer_test_text in
        let expected = List.length lexer_test_tokens in
        let actual = List.length tokens in
        test_eq expected actual @@ "Length " ^ string_of_int expected ^ " != " ^ string_of_int actual;
        (*
        print_newline ();
        List.iter (fun x -> print_endline @@ src_token x) @@ L.lexer "test" lexer_test_text;
        *)
        List.iter2
            (fun t1 t2 ->
                if verbose then
                    (print_endline @@ "required " ^ src_token t1;
                     print_endline @@ " parsed   " ^ src_token t2);
                test_eq t1 t2 @@ src_token t1 ^ " != " ^ src_token t2
            ) lexer_test_tokens tokens;
        print_newline ()
    with Invalid_argument s -> test_fail @@ "Invalid_argument: " ^ s
        | Error (filename, pos, msg) -> test_fail @@ Printf.sprintf "%s, line=%d, col=%d: Error: %s" filename pos.line pos.col msg)


let parser_test_texts = [
    (";",                       "(ESeq [])");
    ("x = 0",                   "(ESeq [(ELet (\"x\", (ELit (Int 0))))])");
    ("x = 0; y = 1",            "(ESeq [(ELet (\"x\", (ELit (Int 0)))); (ELet (\"y\", (ELit (Int 1))))])");
    ("_ = let x = 0",           "(ESeq [(ELet (\"_\", (ELet (\"x\", (ELit (Int 0))))))])");
    ("_ = { let a = 0 }",       "(ESeq [(ELet (\"_\", (ESeq [(ELet (\"a\", (ELit (Int 0))))])))])");
    ("_ = { 1 }",               "(ESeq [(ELet (\"_\", (ESeq [(ELit (Int 1))])))])");
    ("_ = { 1; }",              "(ESeq [(ELet (\"_\", (ESeq [(ELit (Int 1))])))])");
    ("_ = { 1; 2 }",            "(ESeq [(ELet (\"_\", (ESeq [(ELit (Int 1)); (ELit (Int 2))])))])");
    ("_ = { 1; 2; }",           "(ESeq [(ELet (\"_\", (ESeq [(ELit (Int 1)); (ELit (Int 2))])))])");
    ("_ = { 1\n 2; }",          "(ESeq [(ELet (\"_\", (ESeq [(ELit (Int 1)); (ELit (Int 2))])))])");
    ("_ = { 1\n 2\n }",         "(ESeq [(ELet (\"_\", (ESeq [(ELit (Int 1)); (ELit (Int 2))])))])");
    ("_ = if 1 then 2 else 3",  "(ESeq [(ELet (\"_\", (ECond ((ELit (Int 1)), (ELit (Int 2)), (ELit (Int 3))))))])");
    ("_ = if 1 then 2",         "(ESeq [(ELet (\"_\", (ECond ((ELit (Int 1)), (ELit (Int 2)), EUnit))))])");
    ("_ = if \n 1 \n then \n 2 \n else \n 3",
                                "(ESeq [(ELet (\"_\", (ECond ((ELit (Int 1)), (ELit (Int 2)), (ELit (Int 3))))))])");
    ("_ = fn x -> x",           "(ESeq [(ELet (\"_\", (ELambda (\"x\", (EId \"x\")))))])");
    ("_ = fn x ->\n x",         "(ESeq [(ELet (\"_\", (ELambda (\"x\", (EId \"x\")))))])");
    ("_ = fn () -> 0",          "(ESeq [(ELet (\"_\", (ELambda (\"()\", (ELit (Int 0))))))])");
    ("_ = 1 ? 2 : 3",           "(ESeq [(ELet (\"_\", (ECond ((ELit (Int 1)), (ELit (Int 2)), (ELit (Int 3))))))])");
    ("_ = 1 ?\n 2\n :\n 3",     "(ESeq [(ELet (\"_\", (ECond ((ELit (Int 1)), (ELit (Int 2)), (ELit (Int 3))))))])");
    ("_ = 1 + 2",               "(ESeq [(ELet (\"_\", (EBinary (\"+\", (ELit (Int 1)), (ELit (Int 2))))))])");
    ("_ = 1 + 2 * 3",           "(ESeq [(ELet (\"_\", (EBinary (\"+\", (ELit (Int 1)), (EBinary (\"*\", (ELit (Int 2)), (ELit (Int 3))))))))])");
    ("_ = 1 - 2 * 3 + 4",       "(ESeq [(ELet (\"_\", (EBinary (\"+\", (EBinary (\"-\", (ELit (Int 1)), (EBinary (\"*\", (ELit (Int 2)), (ELit (Int 3)))))), (ELit (Int 4))))))])");
    ("_ = 1 - 2 < 3 - 4",       "(ESeq [(ELet (\"_\", (EBinary (\"<\", (EBinary (\"-\", (ELit (Int 1)), (ELit (Int 2)))), (EBinary (\"-\", (ELit (Int 3)), (ELit (Int 4))))))))])");
    ("_ = 1 :: 2 :: 3", "(ESeq [(ELet (\"_\", (EBinary (\"::\", (ELit (Int 1)), (EBinary (\"::\", (ELit (Int 2)), (ELit (Int 3))))))))])");
    ("_ = 1 :: 2 :: [3]",
        "(ESeq [(ELet (\"_\", (EBinary (\"::\", (ELit (Int 1)), (EBinary (\"::\", (ELit (Int 2)), (EList [(ELit (Int 3))])))))))])");
    ("_ = 1 :: 2 :: []",
        "(ESeq [(ELet (\"_\", (EBinary (\"::\", (ELit (Int 1)), (EBinary (\"::\", (ELit (Int 2)), ENull))))))])");
    ("_ = foo ()",              "(ESeq [(ELet (\"_\", (EApply ((EId \"foo\"), EUnit))))])");
    ("_ = foo 1",               "(ESeq [(ELet (\"_\", (EApply ((EId \"foo\"), (ELit (Int 1))))))])");
    ("_ = foo 1 2",             "(ESeq [(ELet (\"_\", (EApply ((EApply ((EId \"foo\"), (ELit (Int 1)))), (ELit (Int 2))))))])");
    ("_ = foo (1)",             "(ESeq [(ELet (\"_\", (EApply ((EId \"foo\"), (ELit (Int 1))))))])");
    ("_ = foo [1,2]",           "(ESeq [(ELet (\"_\", (EApply ((EId \"foo\"), (EList [(ELit (Int 1)); (ELit (Int 2))])))))])");
    ("_ = foo 1::2",            "(ESeq [(ELet (\"_\", (EBinary (\"::\", (EApply ((EId \"foo\"), (ELit (Int 1)))), (ELit (Int 2))))))])");
    ("_ = foo (1::2)",          "(ESeq [(ELet (\"_\", (EApply ((EId \"foo\"), (EBinary (\"::\", (ELit (Int 1)), (ELit (Int 2))))))))])");
    ("_ = -11",                 "(ESeq [(ELet (\"_\", (EUnary (\"-\", (ELit (Int 11))))))])");
    ("_ = !2",                  "(ESeq [(ELet (\"_\", (EUnary (\"!\", (ELit (Int 2))))))])");
    ("_ = a",                   "(ESeq [(ELet (\"_\", (EId \"a\")))])");
    ("_ = [1,2,3]",             "(ESeq [(ELet (\"_\", (EList [(ELit (Int 1)); (ELit (Int 2)); (ELit (Int 3))])))])");
    ("_ = [ ]",                 "(ESeq [(ELet (\"_\", ENull))])");
    ("_ = []",                  "(ESeq [(ELet (\"_\", ENull))])");
    ("_ = 'a'",                 "(ESeq [(ELet (\"_\", (ELit (Char 'a'))))])");
    ("_ = \"abc\"",             "(ESeq [(ELet (\"_\", (ELit (String \"abc\"))))])");
    ("_ = (23)",                "(ESeq [(ELet (\"_\", (ELit (Int 23))))])");
]

let parser_test verbose =
    print_string "Parser Test: ";
    let do_parse (text, expected) =
        try
            if verbose then
                print_endline @@ "\ntext     > " ^ text;
            let toks = L.lexer "test" text in
            if verbose then
                print_endline @@ "tokens   > " ^ s_token_src_list toks;
            let s = s_expr_src @@ P.parse "" toks in
            if verbose then begin
                print_endline @@ "expected > " ^ expected;
                print_endline @@ "parsed   > " ^ s
            end;
            test_eq s expected (s ^ " != " ^ expected)
        with Error (_, pos, msg) -> test_fail @@ Printf.sprintf "line=%d, col=%d: Error: %s" pos.line pos.col msg
    in
    List.iter do_parse parser_test_texts;
    print_newline ()

let test_print verbose =
    List.iter ( fun (text, _) ->
            try
                if verbose then print_string @@ "text  > " ^ text ^ "\nresult> ";
                let e = P.parse "" @@ L.lexer "" text in
                print_endline @@ s_expr_src e;
                if verbose then print_endline @@ "      > " ^ s_expr e
            with Error (_, pos, msg) -> Printf.printf "line=%d, col=%d: Error: %s\n" pos.line pos.col msg
        ) parser_test_texts;
    print_newline ()

let test verbose =
    lexer_test verbose;
    parser_test verbose;
    test_report ()


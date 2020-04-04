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
    (Newline, {filename="test";line=1;col=1});
    (Newline, {filename="test";line=4;col=3});
    (Id "identifier", {filename="test";line=6;col=10});
    (Id "Ident", {filename="test";line=6;col=21});
    (Lit (Int 12345), {filename="test";line=6;col=27});
    (Newline, {filename="test";line=6;col=32});
    (Lit (Char 'a'), {filename="test";line=7;col=10});
    (Lit (Char '\t'), {filename="test";line=7;col=14});
    (Lit (String "abc\n"), {filename="test";line=7;col=19});
    (Newline, {filename="test";line=7;col=26});
    (Newline, {filename="test";line=9;col=1});
    (Let, {filename="test";line=11;col=10});
    (If, {filename="test";line=11;col=14});
    (Then, {filename="test";line=11;col=17});
    (Else, {filename="test";line=11;col=22});
    (Fn, {filename="test";line=11;col=27});
    (Newline, {filename="test";line=11;col=29});
    (Semi, {filename="test";line=12;col=10});
    (Colon, {filename="test";line=12;col=12});
    (DColon, {filename="test";line=12;col=14});
    (Comma, {filename="test";line=12;col=17});
    (Dot, {filename="test";line=12;col=19});
    (Null, {filename="test";line=12;col=21});
    (Vertical, {filename="test";line=12;col=24});
    (Ques, {filename="test";line=12;col=26});
    (Eq, {filename="test";line=12;col=28});
    (LOr, {filename="test";line=12;col=30});
    (LAnd, {filename="test";line=12;col=33});
    (Eql, {filename="test";line=12;col=36});
    (Neq, {filename="test";line=12;col=39});
    (LT, {filename="test";line=12;col=42});
    (LE, {filename="test";line=12;col=44});
    (GT, {filename="test";line=12;col=47});
    (GE, {filename="test";line=12;col=49});
    (Plus, {filename="test";line=12;col=52});
    (Minus, {filename="test";line=12;col=54});
    (Star, {filename="test";line=12;col=56});
    (Slash, {filename="test";line=12;col=58});
    (Percent, {filename="test";line=12;col=60});
    (Not, {filename="test";line=12;col=62});
    (Unit, {filename="test";line=12;col=64});
    (Newline, {filename="test";line=12;col=66});
    (LBrace, {filename="test";line=13;col=10});
    (RBrace, {filename="test";line=13;col=12});
    (LParen, {filename="test";line=13;col=14});
    (RParen, {filename="test";line=13;col=16});
    (LBracket, {filename="test";line=13;col=18});
    (RBracket, {filename="test";line=13;col=20});
    (RArrow, {filename="test";line=13;col=22});
    (Op ",=", {filename="test";line=13;col=25});
    (Op "++", {filename="test";line=13;col=28});
    (Op "=/=", {filename="test";line=13;col=31});
    (Newline, {filename="test";line=13;col=34});
]

let src_token (tok, pos)  =
    Printf.sprintf "    (%s, {filename=\"%s\";line=%d;col=%d});" (s_token_src tok) pos.filename pos.line pos.col

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
        | Error (pos, msg) -> test_fail @@ s_pos pos ^ "Error: " ^ msg)


let parser_test_texts = [
    (";",                       "[]");
    ("x = 0",                   "[(ELetRec (\"x\", (ELit (Int 0))))]");
    ("x = 0; y = 1",            "[(ELetRec (\"x\", (ELit (Int 0)))); (ELetRec (\"y\", (ELit (Int 1))))]");
    ("_ = let x = 0",           "[(ELetRec (\"_\", (ELet (\"x\", (ELit (Int 0))))))]");
    ("_ = { let a = 0 }",       "[(ELetRec (\"_\", (ESeq [(ELet (\"a\", (ELit (Int 0))))])))]");
    ("_ = { 1 }",               "[(ELetRec (\"_\", (ESeq [(ELit (Int 1))])))]");
    ("_ = { 1; }",              "[(ELetRec (\"_\", (ESeq [(ELit (Int 1))])))]");
    ("_ = { 1; 2 }",            "[(ELetRec (\"_\", (ESeq [(ELit (Int 1)); (ELit (Int 2))])))]");
    ("_ = { 1; 2; }",           "[(ELetRec (\"_\", (ESeq [(ELit (Int 1)); (ELit (Int 2))])))]");
    ("_ = { 1\n 2; }",          "[(ELetRec (\"_\", (ESeq [(ELit (Int 1)); (ELit (Int 2))])))]");
    ("_ = { 1\n 2\n }",         "[(ELetRec (\"_\", (ESeq [(ELit (Int 1)); (ELit (Int 2))])))]");
    ("_ = if 1 then 2 else 3",  "[(ELetRec (\"_\", (ECond ((ELit (Int 1)), (ELit (Int 2)), (ELit (Int 3))))))]");
    ("_ = if 1 then 2",         "[(ELetRec (\"_\", (ECond ((ELit (Int 1)), (ELit (Int 2)), EUnit))))]");
    ("_ = if \n 1 \n then \n 2 \n else \n 3",
                                "[(ELetRec (\"_\", (ECond ((ELit (Int 1)), (ELit (Int 2)), (ELit (Int 3))))))]");
    ("_ = fn x -> x",           "[(ELetRec (\"_\", (ELambda (\"x\", (EId \"x\")))))]");
    ("_ = fn x ->\n x",         "[(ELetRec (\"_\", (ELambda (\"x\", (EId \"x\")))))]");
    ("_ = fn () -> 0",          "[(ELetRec (\"_\", (ELambda (\"()\", (ELit (Int 0))))))]");
    ("_ = 1 ? 2 : 3",           "[(ELetRec (\"_\", (ECond ((ELit (Int 1)), (ELit (Int 2)), (ELit (Int 3))))))]");
    ("_ = 1 ?\n 2\n :\n 3",     "[(ELetRec (\"_\", (ECond ((ELit (Int 1)), (ELit (Int 2)), (ELit (Int 3))))))]");
    ("_ = 1 + 2",               "[(ELetRec (\"_\", (EBinary (BinAdd, (ELit (Int 1)), (ELit (Int 2))))))]");
    ("_ = 1 + 2 * 3",           "[(ELetRec (\"_\", (EBinary (BinAdd, (ELit (Int 1)), (EBinary (BinMul, (ELit (Int 2)), (ELit (Int 3))))))))]");
    ("_ = 1 - 2 * 3 + 4",       "[(ELetRec (\"_\", (EBinary (BinAdd, (EBinary (BinSub, (ELit (Int 1)), (EBinary (BinMul, (ELit (Int 2)), (ELit (Int 3)))))), (ELit (Int 4))))))]");
    ("_ = 1 - 2 < 3 - 4",       "[(ELetRec (\"_\", (EBinary (BinLT, (EBinary (BinSub, (ELit (Int 1)), (ELit (Int 2)))), (EBinary (BinSub, (ELit (Int 3)), (ELit (Int 4))))))))]");
    ("_ = 1 :: 2 :: 3", "[(ELetRec (\"_\", (EBinary (BinCons, (ELit (Int 1)), (EBinary (BinCons, (ELit (Int 2)), (ELit (Int 3))))))))]");
    ("_ = 1 :: 2 :: [3]",       "[(ELetRec (\"_\", (EBinary (BinCons, (ELit (Int 1)), (EBinary (BinCons, (ELit (Int 2)), (EBinary (BinCons, (ELit (Int 3)), ENull))))))))]");
    ("_ = 1 :: 2 :: []",
        "[(ELetRec (\"_\", (EBinary (BinCons, (ELit (Int 1)), (EBinary (BinCons, (ELit (Int 2)), ENull))))))]");
    ("_ = foo ()",              "[(ELetRec (\"_\", (EApply ((EId \"foo\"), EUnit))))]");
    ("_ = foo 1",               "[(ELetRec (\"_\", (EApply ((EId \"foo\"), (ELit (Int 1))))))]");
    ("_ = foo 1 2",             "[(ELetRec (\"_\", (EApply ((EApply ((EId \"foo\"), (ELit (Int 1)))), (ELit (Int 2))))))]");
    ("_ = foo (1)",             "[(ELetRec (\"_\", (EApply ((EId \"foo\"), (EParen (ELit (Int 1)))))))]");
    ("_ = foo [1,2]",
        "[(ELetRec (\"_\", (EApply ((EId \"foo\"), (EBinary (BinCons, (ELit (Int 1)), (EBinary (BinCons, (ELit (Int 2)), ENull))))))))]");
    ("_ = foo 1::2",            "[(ELetRec (\"_\", (EBinary (BinCons, (EApply ((EId \"foo\"), (ELit (Int 1)))), (ELit (Int 2))))))]");
    ("_ = foo (1::2)",          "[(ELetRec (\"_\", (EApply ((EId \"foo\"), (EParen (EBinary (BinCons, (ELit (Int 1)), (ELit (Int 2)))))))))]");
    ("_ = -11",                 "[(ELetRec (\"_\", (EUnary (UMinus, (ELit (Int 11))))))]");
    ("_ = !2",                  "[(ELetRec (\"_\", (EUnary (UNot, (ELit (Int 2))))))]");
    ("_ = a",                   "[(ELetRec (\"_\", (EId \"a\")))]");
    ("_ = [1,2,3]",             "[(ELetRec (\"_\", (EBinary (BinCons, (ELit (Int 1)), (EBinary (BinCons, (ELit (Int 2)), (EBinary (BinCons, (ELit (Int 3)), ENull))))))))]");
    ("_ = [ ]",                 "[(ELetRec (\"_\", ENull))]");
    ("_ = []",                  "[(ELetRec (\"_\", ENull))]");
    ("_ = 'a'",                 "[(ELetRec (\"_\", (ELit (Char 'a'))))]");
    ("_ = \"abc\"",             "[(ELetRec (\"_\", (ELit (String \"abc\"))))]");
    ("_ = (23)",                "[(ELetRec (\"_\", (EParen (ELit (Int 23)))))]");
    ("id x = x",                "[(ELetRec (\"id\", (ELambda (\"x\", (EId \"x\")))))]");
    ("add x y = x + y",         "[(ELetRec (\"add\", (ELambda (\"x\", (ELambda (\"y\", (EBinary (BinAdd, (EId \"x\"), (EId \"y\")))))))))]");
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
            let s = s_exprlist_src @@ P.parse toks in
            if verbose then begin
                print_endline @@ "expected > " ^ expected;
                print_endline @@ "parsed   > " ^ s
            end;
            test_eq s expected (s ^ " != " ^ expected)
        with Error (pos, msg) -> test_fail @@ (s_pos pos) ^ "Error: " ^ msg
    in
    List.iter do_parse parser_test_texts;
    print_newline ()

let test_print verbose =
    List.iter ( fun (text, _) ->
            try
                if verbose then print_string @@ "text  > " ^ text ^ "\nresult> ";
                let el = P.parse @@ L.lexer "" text in
                print_endline @@ s_exprlist_src el;
                if verbose then print_endline @@ "      > " ^ s_exprlist ";" el
            with Error (pos, msg) -> print_endline @@ s_pos pos ^ "Error: " ^ msg
        ) parser_test_texts;
    print_newline ()

let type_test_texts = [
    ("123", "int");
    ("'a'", "char");
    ("\"abc\"", "string");
    ("300+12", "int");
    ("300*12+3", "int");
    ("300+12*3", "int");
    ("fn x -> x + 1", "int -> int");
    ("fn _ -> ()", "'a -> unit");
    ("(fn x -> x + 1) (300 * (12 + 3))", "int");
    ("1+2 < 3*4", "bool");
    ("2 * -(1+2)", "int");
    ("fn x y -> x + y", "'a -> 'a -> 'a");
    ("(fn x -> x) 1", "int");
    ("(fn x -> x) 1==1", "bool");
    ("(fn _ -> 1) 'a'", "int");
    ("(fn _ -> 2) 3", "int");
    ("[]", "'a list");
    ("[1,2,3]", "int list");
    ("1::2::[]", "int list");
    ("['a','b']", "char list");
    ("\"abc\"", "string");
    ("(fn x -> x)", "'a -> 'a");
    ("fn x -> fn y -> x", "'a -> 'b -> 'a");
    ("fn x -> fn y -> y", "'a -> 'b -> 'b");
    ("(fn x -> x + 1) 2 + (fn x -> x + -1) 3", "int");
    ("fn f -> fn g -> fn x -> g (f x)", "('a -> 'b) -> ('b -> 'c) -> 'a -> 'c");
    ("fn x -> fn y -> fn z -> x z (y z)", "('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c");
    ("fn x -> {let y = x + 1; x}", "int -> int");
    ("fn x -> {let y = x + 1; y}", "int -> int");
    ("fn b -> fn x -> if x b then x else (fn x -> b)", "bool -> (bool -> bool) -> bool -> bool");
    ("fn x -> if 1==1 then x else (if x then 1==1 else 1==2)", "bool -> bool");
    ("fn x -> fn y -> if x then x else y", "bool -> bool -> bool");
    ("fn n -> (fn x -> x (fn y -> y)) (fn f -> f n)", "'a -> 'a");
    ("fn x -> fn y -> x y", "('a -> 'b) -> 'a -> 'b");
    ("fn x -> fn y -> x (y x)", "('a -> 'b) -> (('a -> 'b) -> 'a) -> 'b");
    ("fn x -> fn y -> x (y x) (y x)", "('a -> 'a -> 'b) -> (('a -> 'a -> 'b) -> 'a) -> 'b");
    ("fn x -> fn y -> fn z -> x (z x) (y (z x y))", "((('a -> 'b) -> 'a) -> 'b -> 'c) -> ('a -> 'b) -> (((('a -> 'b) -> 'a) -> 'b -> 'c) -> ('a -> 'b) -> 'a) -> 'c");
    ("{ let id = fn x -> x; let f = fn y -> id (y id); f}", "(('a -> 'a) -> 'b) -> 'b"); 
    ("{ let k = fn x -> fn y -> x; let k1 = fn x -> fn y -> k (x k); k1 }",
        "(('a -> 'b -> 'a) -> 'c) -> 'd -> 'e -> 'c");
    ("{ let s = fn x -> fn y -> fn z -> x z (y z); let s1 = fn x -> fn y -> fn z -> x s (z s) (y s (z s)); s1 }",
        "((('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c) -> 'd -> 'e -> 'f) -> ((('g -> 'h -> 'i) -> ('g -> 'h) -> 'g -> 'i) -> 'd -> 'e) -> ((('j -> 'k -> 'l) -> ('j -> 'k) -> 'j -> 'l) -> 'd) -> 'f");
    ("{ let g = fn h -> fn t -> fn f -> fn x -> f h (t f x); g }",
        "'a -> (('a -> 'b -> 'c) -> 'd -> 'b) -> ('a -> 'b -> 'c) -> 'd -> 'c");
    ("{ let s = fn x -> fn y -> fn z -> x z (y z); let k = fn x -> fn y -> x; let kk = fn x -> fn y -> x; s k kk }", "'a -> 'a");
    ("{ let s = fn x -> fn y -> fn z -> x z (y z); let k = fn x -> fn y -> x; s k k }",
        "'a -> 'a");
    ("{ let s = fn x -> fn y -> fn z -> x z (y z); let kk = fn x -> fn y -> y; s kk kk }",
        "'a -> 'b -> 'b");
(*
    ("fn x -> fn y -> fn z -> { let b = x y z ; if b then z y else y }", "('a -> ('a -> 'a) -> bool) -> 'a -> ('a -> 'a) -> 'a");
*)
    ("{let pair = fn x1 -> fn x2 -> fn y -> y x1 x2; let proj1 = fn p -> p (fn x1 -> fn x2 -> x1); let proj2 = fn p -> p (fn x1 -> fn x2 -> x2); proj1 (pair 1 100)}", "int");
    ("{let pair = fn x1 -> fn x2 -> fn y -> y x1 x2; let proj1 = fn p -> p (fn x1 -> fn x2 -> x1); let proj2 = fn p -> p (fn x1 -> fn x2 -> x2); proj1 (proj2 (pair 10 (pair 20 30)))}", "int");
    ("{let f = fn x -> x; if f 1==1 then f 1 else f 2}", "int");
    ("{let f = fn x -> 3; f (1==1) + f 4}", "int");
    ("fn b -> {let f = fn x -> x; let g = fn y -> y; if b then f g else g f}", "bool -> 'a -> 'a");
(*
    ("fn b -> fn f -> { let g1 = fn x -> x f; let g2 = fn x -> x f; fn z -> if b then g1 z g2 else g2 z g1}", "bool -> 'a -> ('a -> (('a -> 'b) -> 'b) -> 'c) -> 'c");
*)

]

let type_test verbose =
    print_string "Type Test: ";
    let do_test (text, expected) =
        try
            if verbose then
                print_endline @@ "\ntext    > " ^ text;
            let toks = L.lexer "test" text in
            if verbose then
                print_endline @@ "tokens  > " ^ s_token_src_list toks;
            let pars = P.create_parser toks in
            let e = P.parse_expr pars in
            if verbose then
                print_endline @@ "parsed  > " ^ s_expr_src e;
            let (tenv, t) = Type.infer [] e in
            let s = s_typ t in
            if verbose then
                print_endline @@ "infer   > " ^ s;
            test_eq s expected (s ^ " != " ^ expected)
        with Error (pos, msg) -> test_fail @@ s_pos pos ^ "Error: " ^ msg
    in
    List.iter do_test type_test_texts;
    print_newline ()

let eval_test_texts = [
    ("'a'", VChar 'a');
    ("\"abc\"", VString "abc");
    ("12", VInt 12);
    ("300 + 12", VInt 312);
    ("300 * 12 + 3", VInt 3603);
    ("300 * (12 + 3)", VInt 4500);
    ("300 / (12 - 3)", VInt 33);
    ("300 % (12 - 3)", VInt 3);
    ("1 < 2", VBool true);
    ("1 <= 1", VBool true);
    ("1 > 2", VBool false);
    ("2 >= 2", VBool true);
    ("2 == 2", VBool true);
    ("2 == 1", VBool false);
    ("2 != 1", VBool true);
    ("2 != 2", VBool false);
    ("'a' == 'a'", VBool true);
    ("'a' == 'b'", VBool false);
    ("'a' != 'a'", VBool false);
    ("'a' != 'b'", VBool true);
    ("'a' < 'b'", VBool true);
    ("'b' < 'a'", VBool false);
    ("'a' <= 'a'", VBool true);
    ("'b' <= 'a'", VBool false);
    ("'a' > 'b'", VBool false);
    ("'b' > 'a'", VBool true);
    ("'a' >= 'b'", VBool false);
    ("'a' >= 'a'", VBool true);
    ("\"abc\" == \"abc\"", VBool true);
    ("\"abc\" == \"def\"", VBool false);
    ("\"abc\" != \"abc\"", VBool false);
    ("\"abc\" != \"def\"", VBool true);
    ("\"abc\" < \"def\"", VBool true);
    ("\"abc\" > \"def\"", VBool false);
    ("1 > 2 || 2 > 1", VBool true);
    ("1 < 2 && 2 < 1", VBool false);
    ("-5", VInt (-5));
    ("1::[2,3]", VCons (VInt 1, VCons (VInt 2, VCons (VInt 3, VNull))));
    ("1::2::[3]", VCons (VInt 1, VCons (VInt 2, VCons (VInt 3, VNull))));
    ("[1,2,3]", VCons (VInt 1, VCons (VInt 2, VCons (VInt 3, VNull))));
    ("[1,2,3] == 1::[2,3]", VBool true);
    ("[1,2,3] == 1::[2,3,4]", VBool false);
    ("{ let x = 1; x }", VInt 1);
    ("let x = 2", VUnit);
    ("x", VInt 2);
    ("let f = fn () -> 5", VUnit);
    ("f ()", VInt 5);
    ("let g = fn _ -> 8", VUnit);
    ("g 3", VInt 8);
    ("let a = fn x -> x + 1", VUnit);
    ("a 4", VInt 5);
    ("let add = fn x -> fn y -> x + y", VUnit);
    ("add 1 2", VInt 3);
    ("let add5 = add 5", VUnit);
    ("add5 3", VInt 8);
    ("let foo = fn x -> x + 2", VUnit);
    ("foo 4", VInt 6);
    ("let rec fact = fn n -> if n < 1 then 1 else n * fact (n-1)", VUnit);
    ("fact 5", VInt 120);
]

let env_print env =
    print_string "Env [";
    let rec aux = function
        | [] -> ()
        | x::xs ->
            Printf.printf "(%s, %s)" (fst x) (s_value !(snd x));
            aux xs
    in aux env;
    print_endline "]"

let eval_test verbose =
    print_string "Eval Test: ";
    let do_test (env, tenv) (text, expected) =
        try
            (*
            print_endline "do_test";
            env_print env;
            *)
            if verbose then
                print_endline @@ "text    > " ^ text;
            let toks = L.lexer "test" text in
            if verbose then
                print_endline @@ "tokens  > " ^ s_token_src_list toks;
            let e = P.parse_expr @@ P.create_parser toks in
            if verbose then
                print_endline @@ "parsed  > " ^ s_expr_src e;
            let (new_tenv, t) = Type.infer tenv e in
            if verbose then
                print_endline @@ "infer   > " ^ s_typ t;
            let (new_env, v) = Eval.eval env e in
            if verbose then
                print_endline @@ "eval    > " ^ s_value v;
            test_eq v expected (s_value v ^ " != " ^ s_value expected);
            (*
            env_print new_env;
            *)
            (new_env, new_tenv)
        with Error (pos, msg) -> test_fail @@ (s_pos pos) ^ "Error: " ^ msg; (env, tenv)
    in
    ignore @@ List.fold_left (fun env test -> do_test env test) ([],[]) eval_test_texts;
    print_newline ()


let test verbose =
    lexer_test verbose;
    parser_test verbose;
    type_test verbose;
    eval_test verbose;
    test_report ()


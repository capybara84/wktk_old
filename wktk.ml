open Syntax

let rec read_print_eval_loop verbose env tenv =
    try
        print_string "> ";
        flush stdout;
        let toks = Lexer.lexer "" @@ input_line stdin in
        if verbose then print_endline @@ s_token_src_list toks;
        let e = Parser.parse_expr @@ Parser.create_parser toks in
        if verbose then begin
            print_endline @@ "-> " ^ s_expr_src e;
            print_endline @@ "-> " ^ s_expr e
        end;
        let (tenv, t) = Type.infer tenv e in
        let (env, v) = Eval.eval env e in
        print_endline @@ s_value v ^ " : " ^ s_typ t;
        read_print_eval_loop verbose env tenv
    with
        | Error (pos, msg) ->
            print_endline @@ s_pos pos ^ "Error: " ^ msg;
            read_print_eval_loop verbose env tenv
        | Sys_error s -> print_endline s
        | End_of_file -> ()

let () =
    let filenames = ref []
    and verbose = ref false
    and do_test = ref false
    and do_test_print = ref false
    in
    Arg.parse
        [
            ("-v", Arg.Unit (fun () -> verbose := true), " verbose");
            ("-t", Arg.Unit (fun () -> do_test := true), " unit test");
            ("-tp", Arg.Unit (fun () -> do_test_print := true), " test print");
            ("-dp", Arg.Unit (fun () -> Parser.debug_scope_flag := true), " debug parser");
            ("-dt", Arg.Unit (fun () -> Type.debug_scope_flag := true), " debug type");
            ("-de", Arg.Unit (fun () -> Eval.debug_scope_flag := true), " debug eval");
        ]
        (fun name -> filenames := name :: !filenames)
        "usage: wktk [-v]][-t][-tp][-dp][-dt] filename...";
    let (env, tenv) = Builtins.init () in
    List.iter (fun x -> print_endline x) !filenames;
    if !do_test then
        Test.test !verbose
    else if !do_test_print then
        Test.test_print !verbose
    else
        read_print_eval_loop !verbose env tenv


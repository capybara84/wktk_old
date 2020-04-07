open Syntax

let load_file filename =
    let ic = open_in filename in
    let n = in_channel_length ic in
    let text = really_input_string ic n in
    close_in ic;
    text

let load tab filename =
    try
        let text = load_file filename in
        let el = Parser.parse @@ Lexer.lexer filename text in
        List.fold_left (fun tab e ->
            let (tenv, t) = Type.infer tab.tenv e in
            let (env, v) = Eval.eval tab.env e in
            make_table env tenv) tab el
    with
        | Error (pos, msg) -> print_endline @@ s_pos pos ^ "Error: " ^ msg; tab
        | Sys_error s -> print_endline s; tab
        | End_of_file -> tab


let rec read_eval_print_loop verbose tab =
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
        let (tenv, t) = Type.infer tab.tenv e in
        let (env, v) = Eval.eval tab.env e in
        print_endline @@ s_value v ^ " : " ^ s_typ t;
        read_eval_print_loop verbose (make_table env tenv)
    with
        | Error (pos, msg) ->
            print_endline @@ s_pos pos ^ "Error: " ^ msg;
            read_eval_print_loop verbose tab
        | Sys_error s -> print_endline s
        | End_of_file -> ()

let () =
    let filenames = ref []
    and interactive = ref false
    and verbose = ref false
    and do_test = ref false
    and do_test_print = ref false
    in
    Arg.parse
        [
            ("-v", Arg.Unit (fun () -> verbose := true), " verbose");
            ("-i", Arg.Unit (fun () -> interactive := true), " interactive");
            ("-t", Arg.Unit (fun () -> do_test := true), " unit test");
            ("-tp", Arg.Unit (fun () -> do_test_print := true), " test print");
            ("-dp", Arg.Unit (fun () -> Parser.debug_scope_flag := true), " debug parser");
            ("-dt", Arg.Unit (fun () -> Type.debug_scope_flag := true), " debug type");
            ("-de", Arg.Unit (fun () -> Eval.debug_scope_flag := true), " debug eval");
        ]
        (fun name -> filenames := name :: !filenames)
        "usage: wktk [-v]][-t][-tp][-dp][-dt] filename...";
    let tab = Builtins.init () in
    if !do_test then
        Test.test !verbose
    else if !do_test_print then
        Test.test_print !verbose
    else if !filenames <> [] then begin
        let tab = List.fold_left (fun e filename -> load e filename) tab !filenames in
        if !interactive then
            read_eval_print_loop !verbose tab
    end else
        read_eval_print_loop !verbose tab


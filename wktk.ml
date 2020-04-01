open Syntax

let rec read_print_loop verbose tenv =
    try
        print_string "> ";
        flush stdout;
        let toks = Lexer.lexer "" @@ input_line stdin in
        if verbose then print_endline @@ s_token_src_list toks;
        let e = Parser.parse_expr @@ Parser.create_parser "" toks in
        print_endline @@ "-> " ^ s_expr_src e;
        print_endline @@ "-> " ^ s_expr e;
        let (tenv, t) = Type.infer tenv e in
        print_endline @@ " : " ^ s_typ t;
        read_print_loop verbose tenv
    with
        | Error (_, pos, msg) ->
            Printf.printf "line=%d, col=%d: Error: %s\n" pos.line pos.col msg;
            read_print_loop verbose tenv
        | Sys_error s -> print_endline s
        | End_of_file -> ()

let () =
    let filenames = ref []
    and verbose = ref false
    and do_test = ref false
    and do_test_print = ref false
    and do_print = ref false
    in
    Arg.parse
        [
            ("-v", Arg.Unit (fun () -> verbose := true), " verbose");
            ("-t", Arg.Unit (fun () -> do_test := true), " unit test");
            ("-tp", Arg.Unit (fun () -> do_test_print := true), " test print");
            ("-p", Arg.Unit (fun () ->  do_print := true), " print expression");
        ]
        (fun name -> filenames := name :: !filenames)
        "usage: wktk [-v]][-t][-tp][-p] filename...";
    List.iter (fun x -> print_endline x) !filenames;
    if !do_test then
        Test.test !verbose
    else if !do_test_print then
        Test.test_print !verbose
    else if !do_print then
        read_print_loop !verbose []
    else
        ()


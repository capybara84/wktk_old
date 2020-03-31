
let () =
    let filenames = ref [] in
    let verbose = ref false in
    let quit = ref false in
    Arg.parse
        [
            ("-v", Arg.Unit (fun () -> verbose := true), " verbose");
            ("-t", Arg.Unit (fun () -> quit := true; Test.test !verbose), " unit test");
            ("-tp", Arg.Unit (fun () -> quit := true; Test.test_print !verbose), " test print");
        ]
        (fun name -> filenames := name :: !filenames)
        "usage: wktk [-v]][-p][-t][-tp] filename...";
    if not !quit then
        List.iter (fun x -> print_endline x) !filenames;
    ()


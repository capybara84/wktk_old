
let () =
    let filenames = ref [] in
    let verbose = ref false in
    let do_test = ref false in
    Arg.parse
        [
            ("-v", Arg.Unit (fun () -> verbose := true), " verbose");
            ("-t", Arg.Unit (fun () -> do_test := true), " unit test");
        ]
        (fun name -> filenames := name :: !filenames)
        "usage: wktk [-v]][-p][-t][-tp] filename...";
    List.iter (fun x -> print_endline x) !filenames;
    if !do_test then
        Test.test !verbose;
    ()


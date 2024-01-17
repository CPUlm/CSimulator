open LibCNetlist

type act = PrintProg | PrintBlocks | PrintColorTable | PrintUseless | Compile

let action = ref Compile

let quiet = ref false

let cleanup = ref true

let number_steps = ref None

let usage = "usage: csimulator [options] file.net"

let spec =
  Arg.align
    [ (* Disable '-help' because its ugly without the -- *)
      ( "-help"
      , Arg.Unit (fun () -> raise (Arg.Bad "unknown option '-help'"))
      , "" )
    ; ( "--print-prog"
      , Arg.Unit (fun () -> action := PrintProg)
      , " Output the program as a Graphviz File." )
    ; ( "--no-clean"
      , Arg.Unit (fun () -> cleanup := false)
      , " Disable the Netlist Cleanup." )
    ; ( "--print-blocks"
      , Arg.Unit (fun () -> action := PrintBlocks)
      , " Output the blocks of the program as Graphviz File with colored node."
      )
    ; ( "--color-table"
      , Arg.Unit (fun () -> action := PrintColorTable)
      , " Output a table with the block of each node." )
    ; ( "--useless-stats"
      , Arg.Unit (fun () -> action := PrintUseless)
      , " Output the number of useless variables." )
    ; ("--quiet", Arg.Set quiet, " Produce a quiet program.")
    ; ( "--steps"
      , Arg.Int (fun i -> number_steps := Some i)
      , " Number of steps to simulate." ) ]

let filename =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".net") then
      raise (Arg.Bad "no .net extension")
    else if Option.is_some !file then raise (Arg.Bad "multiple input files.")
    else file := Some s
  in
  Arg.parse spec set_file usage ;
  match !file with Some f -> f | None -> Arg.usage spec usage ; exit 1

let program, nb_useless =
  try
    let c = open_in filename in
    let lexbuf = Lexing.from_channel c in
    try
      let mut_prog = Parser.program Lexer.token lexbuf in
      ProgramToGraph.to_graph ~clean:!cleanup mut_prog
    with e ->
      let exn_txt = Printexc.to_string_default e in
      Format.eprintf "Error at %s:%d%d:@.%s@." filename
        lexbuf.lex_curr_p.pos_lnum
        (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)
        exn_txt ;
      (let backtrace = Printexc.get_backtrace () in
       if backtrace <> "" then (
         Format.eprintf "@.Backtrace:@." ;
         List.iter
           (fun line -> if line <> "" then Format.eprintf " - %s@." line)
           (String.split_on_char '\n' backtrace) ;
         Format.print_newline () ) ) ;
      exit 1
  with e ->
    let exn_txt = Printexc.to_string_default e in
    Format.eprintf "Error in file %s:@.%s@." filename exn_txt ;
    (let backtrace = Printexc.get_backtrace () in
     if backtrace <> "" then (
       Format.eprintf "@.Backtrace:@." ;
       List.iter
         (fun line -> if line <> "" then Format.eprintf " - %s@." line)
         (String.split_on_char '\n' backtrace) ;
       Format.print_newline () ) ) ;
    exit 1

let () =
  match !action with
  | PrintProg ->
      ToDot.pp_graph Format.std_formatter (program, None)
  | PrintBlocks ->
      let colors, _ = BlockSplitter.split program in
      ToDot.pp_graph Format.std_formatter (program, Some colors)
  | PrintColorTable ->
      let colors, blocks = BlockSplitter.split program in
      Format.printf "Nb Colors: %i@.@.%a"
        (Variable.Map.cardinal blocks)
        PrettyPrinter.pp_color (colors, program)
  | PrintUseless ->
      Format.printf "Number of:@. - Variables: %i@. - Useless variables: %i@."
        (Variable.Set.cardinal program.vars + nb_useless)
        nb_useless
  | Compile ->
      let _, blocks = BlockSplitter.split program in
      ToC.pp_prog Format.std_formatter (program, blocks)

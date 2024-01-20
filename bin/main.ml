open LibCNetlist

type act = PrintProg | PrintBlocks | PrintColorTable | PrintStats | Compile

let action = ref Compile

let quiet = ref false

let screen = ref true

let cleanup = ref true

let pause = ref false

let usage =
  "usage: csimulator [options] file.net [out_dir]\n\n\
   This program compiles a NetList to a C program.\n\n\
   Options:"

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
    ; ( "--disable-screen"
      , Arg.Unit (fun () -> screen := false)
      , " Disable the screen in the output." )
    ; ( "--stats"
      , Arg.Unit (fun () -> action := PrintStats)
      , " Print stats of the NetList." )
    ; ( "--pause"
      , Arg.Unit (fun () -> pause := true)
      , " Pause the program at each cycle." )
    ; ("--quiet", Arg.Set quiet, " Produce a quiet program.") ]

let filename, out_dir =
  let file = ref None in
  let out_dir = ref None in
  let set_files s =
    if Filename.check_suffix s ".net" then
      if Option.is_some !file then raise (Arg.Bad "multiple input files.")
      else file := Some s
    else
      match !action with
      | Compile ->
          if Option.is_some !out_dir then
            raise (Arg.Bad "multiple output directories.")
          else out_dir := Some s
      | _ ->
          raise (Arg.Bad "no .net extension")
  in
  Arg.parse spec set_files usage ;
  match (!file, !action, !out_dir) with
  | None, _, _ ->
      Format.eprintf "Missing input file.@." ;
      Arg.usage spec usage ;
      exit 1
  | Some _, Compile, None ->
      Format.eprintf "Missing output directory.@." ;
      Arg.usage spec usage ;
      exit 1
  | Some f, Compile, Some out_dir ->
      (f, Some out_dir)
  | Some f, _, _ ->
      (f, None)

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
  | PrintStats ->
      let _, b = BlockSplitter.split program in
      let rep = Hashtbl.create 17 in
      let () =
        Variable.Map.iter
          (fun _ block ->
            let nb_var = Variable.Set.cardinal BlockSplitter.(block.members) in
            match Hashtbl.find_opt rep nb_var with
            | Some nb ->
                Hashtbl.replace rep nb_var (nb + 1)
            | None ->
                Hashtbl.add rep nb_var 1 )
          b
      in
      Format.printf
        "Number of:@. - Variables: %i@. - Useless variables: %i@. - Number of \
         Blocks: %i@.@.Repartition:@.@[<v>%a@]@."
        (Variable.Set.cardinal program.vars + nb_useless)
        nb_useless (Variable.Map.cardinal b)
        (Format.pp_print_list (fun ppf (nb_var, nb_block) ->
             Format.fprintf ppf "Block of %i vars: %i" nb_var nb_block ) )
        ( Hashtbl.to_seq rep |> List.of_seq
        |> List.sort (fun a b -> Int.compare (fst a) (fst b)) )
  | Compile -> (
    match out_dir with
    | None ->
        assert false
    | Some out_dir ->
        let _, blocks = BlockSplitter.split program in
        let genv = WriteLogic.create_env program blocks !screen !pause in
        ToC.export_into out_dir genv )

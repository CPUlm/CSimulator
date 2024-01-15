open LibCNetlist

type act =
  | PrintProg
  | PrintBlocks
  | PrintColorTable
  | PrintPositionTable
  | Compile

let action = ref Compile

let quiet = ref false

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
    ; ( "--print-blocks"
      , Arg.Unit (fun () -> action := PrintBlocks)
      , " Output the blocks of the program as Graphviz File with colored node."
      )
    ; ( "--color-table"
      , Arg.Unit (fun () -> action := PrintColorTable)
      , " Output a table with the block of each node." )
    ; ( "--position-table"
      , Arg.Unit (fun () -> action := PrintPositionTable)
      , " Output a table with the position of each variable." )
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

let program =
  try
    let c = open_in filename in
    let lexbuf = Lexing.from_channel c in
    try Parser.program Lexer.token lexbuf
    with e ->
      Format.eprintf "Error at %s:%d%d:@.%s@." filename
        lexbuf.lex_curr_p.pos_lnum
        (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)
        (Printexc.to_string e) ;
      exit 1
  with e ->
    Format.eprintf "Error in file %s:@.%s@." filename (Printexc.to_string e) ;
    exit 1

let () =
  match !action with
  | PrintProg ->
      ToDot.pp_graph Format.std_formatter (program, None)
  | PrintBlocks ->
      let _, colors, _ = BlockSplitter.split program in
      ToDot.pp_graph Format.std_formatter (program, Some colors)
  | PrintColorTable ->
      let top, _ = BlockSplitter.variable_ordering program in
      let _, colors, blocks = BlockSplitter.split program in
      Format.printf "Nb Colors: %i@.@.%a"
        (Variable.Map.cardinal blocks)
        BlockSplitter.pp_color (colors, top, program)
  | PrintPositionTable ->
      let top, _ = BlockSplitter.variable_ordering program in
      let pos, _ = Position.compute_pos program top in
      let useless, always = Position.split_nodes pos in
      Format.printf "%a@.Always: %i@.Useless: %i@." Position.pp_pos
        (pos, top, program)
        (Variable.Set.cardinal always)
        (Variable.Set.cardinal useless) ;
      Format.printf "List: %a@.@."
        (Format.pp_print_seq
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
           Variable.pp )
        (Variable.Set.to_seq useless) ;
      Format.printf "List: %a@."
        (Format.pp_print_seq
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
           Variable.pp )
        (Variable.Set.to_seq always)
  | Compile ->
      assert false

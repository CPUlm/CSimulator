open LibCNetlistSimulator

let print_only = ref false

let number_steps = ref (-1)

let quiet = ref false

exception Parse_error of string

let find_file filename =
  try open_in filename
  with _ -> raise (Parse_error ("No such file '" ^ filename ^ "%s'"))

(** [read_file filename] reads the [filename] file and outputs the corresponding
    Netlist_ast.program.*)
let read_file filename =
  let ic = find_file filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= filename} ;
  try Parser.program Lexer.token lexbuf
  with _ ->
    let loc =
      Format.sprintf "line %d, column %d" lexbuf.lex_curr_p.pos_lnum
        (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)
    in
    raise (Parse_error ("Syntax error at " ^ loc))

let compile filename =
  try
    let p = Utils.read_file filename in
    try
      let p = Scheduler.schedule p in
      if !print_only then PrettyPrinter.print_program stdout p
      else
        let c_prog = NetlistToC.to_c p !number_steps in
        (*let c_file = open_out "prog.cpp" in
          Printf.fprintf c_file "%s\n" c_prog;
          close_out c_file*)
        Printf.printf "%s" c_prog
    with Scheduler.Combinational_cycle ->
      Format.eprintf "The netlist has a combinatory cycle.@."
  with Utils.Parse_error s ->
    Format.eprintf "An error accurred: %s@." s ;
    exit 2

let () =
  Arg.parse
    [ ("-n", Arg.Set_int number_steps, "Number of steps to simulate")
    ; ( "--print"
      , Arg.Set print_only
      , "print the sorted net-list, without simulating it" )
    ; ( "-q"
      , Arg.Set quiet
      , "De not show the questions (eg a=... is not printed)" ) ]
    compile ""

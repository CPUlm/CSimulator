open LibCNetlist

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
    let p = read_file filename in
    if !print_only then PrettyPrinter.print_program Format.std_formatter p
    else
      try
        let s, _ = Scheduler.schedule p in
        let l =
          Hashtbl.to_seq s |> List.of_seq
          |> List.sort (fun (_, i) (_, j) -> i - j)
        in
        List.iter (fun (v, i) -> Format.printf "%5i <-> %a@." i Variable.pp v) l
      with Graph.Cycle ->
        Format.eprintf "The netlist has a combinatory cycle.@."
  with Parse_error s ->
    Format.eprintf "An error accurred: %s@." s ;
    exit 2

let () =
  Arg.parse
    [ ("--steps", Arg.Set_int number_steps, "Number of steps to simulate")
    ; ( "--print"
      , Arg.Set print_only
      , "print the sorted net-list, without simulating it" )
    ; ( "--quiet"
      , Arg.Set quiet
      , "De not show the questions (eg a=... is not printed)" ) ]
    compile ""

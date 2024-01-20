{
open Parser

let tok_of_string =
  let kw = Hashtbl.create 17 in
  List.iter
    (fun (lex, tok) -> Hashtbl.add kw lex tok)
    [ ("AND", AND)
    ; ("CONCAT", CONCAT)
    ; ("IN", IN)
    ; ("INPUT", INPUT)
    ; ("MUX", MUX)
    ; ("NAND", NAND)
    ; ("NOT", NOT)
    ; ("OR", OR)
    ; ("OUTPUT", OUTPUT)
    ; ("RAM", RAM)
    ; ("REG", REG)
    ; ("ROM", ROM)
    ; ("SELECT", SELECT)
    ; ("SLICE", SLICE)
    ; ("VAR", VAR)
    ; ("XOR", XOR) ] ;
  fun s -> match Hashtbl.find_opt kw s with Some t -> t | None -> NAME s
}

let comment = '#'[^'\n']*
let int = ['0'-'9']+
let ident = '_'? ['A'-'Z' 'a'-'z']('_'?['A'-'Z' 'a'-'z' ''' '0'-'9'])*

rule token = parse
  | '\n'            { Lexing.new_line lexbuf; token lexbuf }
  | ' ' | '\t'      { token lexbuf }
  | comment         { token lexbuf }
  | "="             { EQUAL }
  | ":"             { COLON }
  | ","             { COMMA }
  | int as lxm      { CONST lxm }
  | ident as id     {  tok_of_string id }
  | eof             { EOF }

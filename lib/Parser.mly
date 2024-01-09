%{
 open ParserUtils
%}

%token <string> CONST
%token <string> NAME
%token AND MUX NAND OR RAM ROM XOR REG NOT
%token CONCAT SELECT SLICE
%token COLON EQUAL COMMA VAR IN INPUT OUTPUT
%token EOF

%start program             /* the entry point */
%type <Ast.program> program

%%
program:
  INPUT inp=separated_list(COMMA, NAME)
  OUTPUT out=separated_list(COMMA, NAME)
  VAR vars=separated_list(COMMA, var)
  IN eqs=list(equ)
  EOF { mk_prog inp out vars eqs }

equ: x=NAME EQUAL e=exp   { mk_expr x e }

exp:
  | a=arg                                       { mk_arg a }
  | NOT x=arg                                   { mk_not x }
  | REG x=NAME                                  { mk_reg x }
  | AND x=arg y=arg                             { mk_and x y }
  | OR x=arg y=arg                              { mk_or x y }
  | NAND x=arg y=arg                            { mk_nand x y }
  | XOR x=arg y=arg                             { mk_xor x y }
  | MUX x=arg y=arg z=arg                       { mk_mux x y z }
  | ROM a=int w=int r=arg                       { mk_rom a w r }
  | RAM a=int w=int r=arg we=arg wa=arg wd=arg  { mk_ram a w r we wa wd }
  | CONCAT x=arg y=arg                          { mk_concat x y }
  | SELECT idx=int x=arg                        { mk_select idx x }
  | SLICE min=int max=int x=arg                 { mk_slice min max x }

arg:
  | n=CONST   { mk_const n }
  | v=NAME    { mk_var v }

var: x=NAME ty=ty_exp { Variable.fresh x ty }
ty_exp:
  | /*empty*/ { 1 }
  | COLON n=int { n }

int:
  | c=CONST { int_of_string c }

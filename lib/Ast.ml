type binop = Or | Xor | And | Nand

(* argument of operators (variable or constant) *)
type arg = Avar of Variable.t | Aconst of {value: int; size: int}

(* Expressions *)
type exp =
  | Earg of arg (* a : argument *)
  | Ereg of Variable.t (* REG x : register *)
  | Enot of arg (* NOT a *)
  | Ebinop of binop * arg * arg (* OP a1 a2 : boolean operator *)
  | Emux of arg * arg * arg (* MUX a1 a2 : multiplexer *)
  | Erom of {addr_size: int; word_size: int; read_addr: arg}
  (* ROM addr_size word_size read_addr *)
  | Eram of
      { addr_size: int
      ; word_size: int
      ; read_addr: arg
      ; write_enable: arg
      ; write_addr: arg
      ; write_data: arg }
  (* RAM addr_size word_size read_addr write_enable write_addr data *)
  | Econcat of arg * arg (* CONCAT a1 a2 : concatenation of arrays *)
  | Eslice of {min: int; max: int; arg: arg}
    (* SLICE i1 i2 a : extract the slice of a between indices i1 and i2 *)
  | Eselect of int * arg
(* SELECT i a : ith element of a *)

type program =
  { p_eqs: exp Variable.map
  ; p_inputs: Variable.set (* inputs *)
  ; p_outputs: Variable.set (* outputs *)
  ; p_vars: Variable.set (* all variables *) }

type binop = Or | Xor | And | Nand

(* argument of operators (variable or constant) *)
type arg = Variable of Variable.t | Constant of {value: int; size: int}

(* Expressions *)
type exp =
  | Arg of arg (* a : argument *)
  | Reg of Variable.t (* REG x : register *)
  | Not of arg (* NOT a *)
  | Binop of binop * arg * arg (* OP a1 a2 : boolean operator *)
  | Mux of {cond: arg; true_b: arg; false_b: arg} (* MUX a1 a2 : multiplexer *)
  | Rom of {addr_size: int; word_size: int; read_addr: arg}
  (* ROM addr_size word_size read_addr *)
  | Ram of
      { addr_size: int
      ; word_size: int
      ; read_addr: arg
      ; write_enable: arg
      ; write_addr: arg
      ; write_data: arg }
  (* RAM addr_size word_size read_addr write_enable write_addr data *)
  | Concat of arg * arg (* CONCAT a1 a2 : concatenation of arrays *)
  | Slice of {min: int; max: int; arg: arg}
    (* SLICE i1 i2 a : extract the slice of a between indices i1 and i2 *)
  | Select of int * arg
(* SELECT i a : ith element of a *)

type mut_program =
  { p_eqs: (Variable.t, exp) Hashtbl.t
  ; p_inputs: (Variable.t, unit) Hashtbl.t (* inputs *)
  ; p_outputs: (Variable.t, unit) Hashtbl.t (* outputs *)
  ; p_vars: (Variable.t, unit) Hashtbl.t (* all variables *) }

module VarGraph = Graph.Make (Variable)

type axiom =
  {reg_vars: Variable.set; out_vars: Variable.set; we_vars: Variable.set}

type program =
  { input_vars: Variable.set
  ; output_vars: Variable.set
  ; eqs: (Variable.t, exp) Hashtbl.t
  ; vars: Variable.set
  ; axioms: axiom
  ; deps_graph: VarGraph.t
  ; order: (Variable.t, int) Hashtbl.t }

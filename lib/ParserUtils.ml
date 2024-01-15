open Ast

let get_var n =
  match Variable.exists n with
  | Some v ->
      v
  | None ->
      failwith ("Unknown variable: " ^ n)

let set_from_list s =
  let set = Hashtbl.create 17 in
  let _ =
    List.iter
      (fun v ->
        let v = get_var v in
        if Hashtbl.mem set v then failwith "Multiple time the same var in decl"
        else Hashtbl.add set v () )
      s
  in
  set

let mk_prog inputs outputs vars eqs_list =
  let p_vars = Hashtbl.create 17 in
  let _ = List.iter (fun v -> Hashtbl.add p_vars v ()) vars in
  let p_eqs = Hashtbl.create 17 in
  let _ =
    List.iter
      (fun (v, eq) ->
        match Hashtbl.find_opt p_eqs v with
        | Some _ ->
            failwith "Already defined variable"
        | None ->
            Hashtbl.add p_eqs v eq )
      eqs_list
  in
  let p_inputs = set_from_list inputs in
  let p_outputs = set_from_list outputs in
  let _ =
    Hashtbl.iter
      (fun v _ ->
        if Hashtbl.mem p_eqs v then (* Variable defined with an equation *)
          ()
        else if Hashtbl.mem p_inputs v then (* Variable defined as a input *)
          ()
        else failwith "Undefined Variable Value" )
      p_vars
  in
  {p_eqs; p_inputs; p_outputs; p_vars}

let sizeof_arg = function Variable v -> Variable.size v | Constant c -> c.size

let mk_const s =
  let n = String.length s in
  if n = 0 then raise Parsing.Parse_error
  else Ast.Constant {value= Int64.(of_string ("0b" ^ s) |> to_int); size= n}

let mk_not x = (Not x, sizeof_arg x)

let mk_arg x = (Arg x, sizeof_arg x)

let mk_reg x =
  let v = get_var x in
  (Reg v, Variable.size v)

let mk_and x y =
  let s = sizeof_arg x in
  if s <> sizeof_arg y then failwith "Size Missmatch" else (Binop (And, x, y), s)

let mk_or x y =
  let s = sizeof_arg x in
  if s <> sizeof_arg y then failwith "Size Missmatch" else (Binop (Or, x, y), s)

let mk_nand x y =
  let s = sizeof_arg x in
  if s <> sizeof_arg y then failwith "Size Missmatch"
  else (Binop (Nand, x, y), s)

let mk_xor x y =
  let s = sizeof_arg x in
  if s <> sizeof_arg y then failwith "Size Missmatch" else (Binop (Xor, x, y), s)

let mk_mux x y z =
  let s = sizeof_arg y in
  if s <> sizeof_arg z then failwith "Size Missmatch"
  else if sizeof_arg x <> 1 then failwith "Mux with a variable not of size 1"
  else (Mux {cond= x; true_b= y; false_b= z}, s)

let mk_rom addr_size word_size read_addr =
  if sizeof_arg read_addr <> addr_size then failwith "Read Address missmatch"
  else (Rom {addr_size; word_size; read_addr}, word_size)

let mk_ram addr_size word_size read_addr write_enable write_addr write_data =
  if sizeof_arg read_addr <> addr_size then failwith "Read Address missmatch"
  else if sizeof_arg write_addr <> addr_size then
    failwith "Write Address missmatch"
  else if sizeof_arg write_enable <> 1 then failwith "Write Enable not a bit"
  else if sizeof_arg write_data <> word_size then
    failwith "Write data size missmatch"
  else
    ( Ram {addr_size; word_size; read_addr; write_enable; write_addr; write_data}
    , word_size )

let mk_concat x y =
  let s = sizeof_arg x + sizeof_arg y in
  (Concat (x, y), s)

let mk_select idx x =
  if idx < 0 then failwith "Select with negative index"
  else if sizeof_arg x <= idx then failwith "Select with index too large"
  else (Select (idx, x), 1)

let mk_slice min max x =
  if min > max then failwith "Slice with min > max"
  else if sizeof_arg x <= max then failwith "Slice with max too large"
  else (Slice {min; max; arg= x}, max - min + 1)

let mk_expr v (exp, exp_size) =
  let v = get_var v in
  if Variable.size v <> exp_size then failwith "Equation size Missmatch"
  else (v, exp)

let mk_var v = Variable (get_var v)

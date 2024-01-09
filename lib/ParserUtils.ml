open Ast

let get_var n =
  match Variable.exists n with
  | Some v ->
      v
  | None ->
      failwith ("Unknown variable: " ^ n)

let set_from_list s =
  List.fold_left
    (fun in_set v ->
      let v = get_var v in
      if Variable.Set.mem v in_set then failwith "Error"
      else Variable.Set.add v in_set )
    Variable.Set.empty s

let mk_prog inputs outputs vars eqs_list =
  let p_vars = Variable.Set.of_list vars in
  let p_eqs =
    List.fold_left
      (fun eqs (v, eq) ->
        match Variable.Map.find_opt v eqs with
        | Some _ ->
            failwith "Already defined variable"
        | None ->
            Variable.Map.add v eq eqs )
      Variable.Map.empty eqs_list
  in
  let p_inputs = set_from_list inputs in
  let p_outputs = set_from_list outputs in
  let _ =
    Variable.Set.iter
      (fun v ->
        if Variable.Map.mem v p_eqs then
          (* Variable defined with an equation *)
          ()
        else if Variable.Set.mem v p_inputs then
          (* Variable defined as a input *)
          ()
        else failwith "Undefined Variable Value" )
      p_vars
  in
  {p_eqs; p_inputs; p_outputs; p_vars}

let sizeof_arg = function Avar v -> Variable.size v | Aconst c -> c.size

let mk_const s =
  let n = String.length s in
  if n = 0 then raise Parsing.Parse_error
  else Ast.Aconst {value= Int64.(of_string ("0b" ^ s) |> to_int); size= n}

let mk_not x = (Enot x, sizeof_arg x)

let mk_arg x = (Earg x, sizeof_arg x)

let mk_reg x =
  let v = get_var x in
  (Ereg v, Variable.size v)

let mk_and x y =
  let s = sizeof_arg x in
  if s <> sizeof_arg y then failwith "Size Missmatch"
  else (Ebinop (And, x, y), s)

let mk_or x y =
  let s = sizeof_arg x in
  if s <> sizeof_arg y then failwith "Size Missmatch" else (Ebinop (Or, x, y), s)

let mk_nand x y =
  let s = sizeof_arg x in
  if s <> sizeof_arg y then failwith "Size Missmatch"
  else (Ebinop (Nand, x, y), s)

let mk_xor x y =
  let s = sizeof_arg x in
  if s <> sizeof_arg y then failwith "Size Missmatch"
  else (Ebinop (Xor, x, y), s)

let mk_mux x y z =
  let s = sizeof_arg y in
  if s <> sizeof_arg z then failwith "Size Missmatch"
  else if sizeof_arg x <> 1 then failwith "Mux with a variable not of size 1"
  else (Emux (x, y, z), s)

let mk_rom addr_size word_size read_addr =
  if sizeof_arg read_addr <> addr_size then failwith "Read Address missmatch"
  else (Erom {addr_size; word_size; read_addr}, word_size)

let mk_ram addr_size word_size read_addr write_enable write_addr write_data =
  if sizeof_arg read_addr <> addr_size then failwith "Read Address missmatch"
  else if sizeof_arg write_addr <> addr_size then
    failwith "Write Address missmatch"
  else if sizeof_arg write_enable <> 1 then failwith "Write Enable not a bit"
  else if sizeof_arg write_data <> word_size then
    failwith "Write data size missmatch"
  else
    ( Eram
        {addr_size; word_size; read_addr; write_enable; write_addr; write_data}
    , word_size )

let mk_concat x y =
  let s = sizeof_arg x + sizeof_arg y in
  (Econcat (x, y), s)

let mk_select idx x =
  if idx < 0 then failwith "Select with negative index"
  else if sizeof_arg x <= idx then failwith "Select with index too large"
  else (Eselect (idx, x), 1)

let mk_slice min max x =
  if min > max then failwith "Slice with min > max"
  else if sizeof_arg x <= max then failwith "Slice with max too large"
  else (Eslice {min; max; arg= x}, max - min + 1)

let mk_expr v (exp, exp_size) =
  let v = get_var v in
  if Variable.size v <> exp_size then failwith "Equation size Missmatch"
  else (v, exp)

let mk_var v = Avar (get_var v)

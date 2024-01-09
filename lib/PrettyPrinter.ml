open Ast
open Format

let print_bool ff b = if b then fprintf ff "1" else fprintf ff "0"

let print_value ff v = Format.fprintf ff "%#x" v

let print_arg ff arg =
  match arg with
  | Aconst c ->
      print_value ff c.value
  | Avar id ->
      Variable.pp ff id

let print_op ff op =
  match op with
  | And ->
      fprintf ff "AND"
  | Nand ->
      fprintf ff "NAND"
  | Or ->
      fprintf ff "OR"
  | Xor ->
      fprintf ff "XOR"

let print_exp ff e =
  match e with
  | Earg a ->
      print_arg ff a
  | Ereg x ->
      fprintf ff "REG %a" Variable.pp x
  | Enot x ->
      fprintf ff "NOT %a" print_arg x
  | Ebinop (op, x, y) ->
      fprintf ff "%a %a %a" print_op op print_arg x print_arg y
  | Emux (c, x, y) ->
      fprintf ff "MUX %a %a %a " print_arg c print_arg x print_arg y
  | Erom rom ->
      fprintf ff "ROM %d %d %a" rom.addr_size rom.word_size print_arg
        rom.read_addr
  | Eram ram ->
      fprintf ff "RAM %d %d %a %a %a %a" ram.addr_size ram.word_size print_arg
        ram.read_addr print_arg ram.write_enable print_arg ram.write_addr
        print_arg ram.write_data
  | Eselect (idx, x) ->
      fprintf ff "SELECT %d %a" idx print_arg x
  | Econcat (x, y) ->
      fprintf ff "CONCAT %a %a" print_arg x print_arg y
  | Eslice s ->
      fprintf ff "SLICE %d %d %a" s.min s.max print_arg s.arg

let rec print_idents ~with_size ff var_set =
  let card = Variable.Set.cardinal var_set in
  if card = 0 then ()
  else if card = 1 then Variable.pp ff (Variable.Set.choose var_set)
  else
    let v = Variable.Set.choose var_set in
    let v_size = Variable.size v in
    let var_set = Variable.Set.remove v var_set in
    if with_size && v_size <> 1 then
      fprintf ff "@[%a : %i@],@ %a" Variable.pp v v_size
        (print_idents ~with_size) var_set
    else fprintf ff "%a,@ %a" Variable.pp v (print_idents ~with_size) var_set

let print_program oc p =
  let ff = formatter_of_out_channel oc in
  fprintf ff "INPUT @[%a@]@." (print_idents ~with_size:false) p.p_inputs ;
  fprintf ff "OUTPUT @[%a@]@." (print_idents ~with_size:false) p.p_outputs ;
  fprintf ff "VARS @[%a@]@.IN@." (print_idents ~with_size:true) p.p_vars ;
  Variable.Map.iter
    (fun v eq -> fprintf ff "%a = %a@." Variable.pp v print_exp eq)
    p.p_eqs ;
  fprintf ff "@."

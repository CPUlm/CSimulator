open Ast
open Format

let print_bool ff b = if b then fprintf ff "1" else fprintf ff "0"

let print_value ff v = Format.fprintf ff "%#x" v

let print_arg ff arg =
  match arg with
  | Constant c ->
      print_value ff c.value
  | Variable id ->
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
  | Arg a ->
      print_arg ff a
  | Reg x ->
      fprintf ff "REG %a" Variable.pp x
  | Not x ->
      fprintf ff "NOT %a" print_arg x
  | Binop (op, x, y) ->
      fprintf ff "%a %a %a" print_op op print_arg x print_arg y
  | Mux md ->
      fprintf ff "MUX %a %a %a " print_arg md.cond print_arg md.true_b print_arg
        md.false_b
  | Rom rom ->
      fprintf ff "ROM %d %d %a" rom.addr_size rom.word_size print_arg
        rom.read_addr
  | Ram ram ->
      fprintf ff "RAM %d %d %a %a %a %a" ram.addr_size ram.word_size print_arg
        ram.read_addr print_arg ram.write_enable print_arg ram.write_addr
        print_arg ram.write_data
  | Select (idx, x) ->
      fprintf ff "SELECT %d %a" idx print_arg x
  | Concat (x, y) ->
      fprintf ff "CONCAT %a %a" print_arg x print_arg y
  | Slice s ->
      fprintf ff "SLICE %d %d %a" s.min s.max print_arg s.arg

let print_idents ~with_size ff l =
  let pp_var ff v =
    let v_size = Variable.size v in
    if with_size && v_size <> 1 then
      fprintf ff "@[%a : %i@]" Variable.pp v v_size
    else Variable.pp ff v
  in
  pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ",@ ") pp_var ff l

let print_program ff p =
  fprintf ff "INPUT @[%a@]@."
    (print_idents ~with_size:false)
    (Hashtbl.to_seq_keys p.p_inputs |> List.of_seq) ;
  fprintf ff "OUTPUT @[%a@]@."
    (print_idents ~with_size:false)
    (Hashtbl.to_seq_keys p.p_outputs |> List.of_seq) ;
  fprintf ff "VARS @[%a@]@.IN@."
    (print_idents ~with_size:true)
    (Hashtbl.to_seq_keys p.p_vars |> List.of_seq) ;
  Hashtbl.iter
    (fun v eq -> fprintf ff "%a = %a@." Variable.pp v print_exp eq)
    p.p_eqs ;
  fprintf ff "@."

let center ?(filler = ' ') size pp i =
  let text = Format.asprintf "%a" pp i in
  let left_padding, right_padding =
    let text_length = String.length text in
    let left_size = (size - text_length) / 2 in
    ( String.make left_size filler
    , String.make (size - left_size - text_length) filler )
  in
  left_padding ^ text ^ right_padding

let pp_color ppf (colors, program) =
  let l =
    Hashtbl.to_seq program.order
    |> List.of_seq
    |> List.sort (fun (_, i) (_, j) -> Int.compare i j)
  in
  let col_width = 20 in
  let sep = String.make (5 * col_width) '-' in
  Format.fprintf ppf "%s|%s|%s|%s|%s@.%s@."
    (center col_width Format.pp_print_string "Variable")
    (center col_width Format.pp_print_string "Ordering")
    (center col_width Format.pp_print_string "Color")
    (center col_width Format.pp_print_string "Input")
    (center col_width Format.pp_print_string "Output")
    sep ;
  List.iter
    BlockSplitter.(
      fun (v, i) ->
        let c =
          match Hashtbl.find_opt colors v with
          | Some (Color i) ->
              string_of_int i
          | None ->
              "None"
        in
        Format.fprintf ppf "%s|%s|%s|%s|%s@."
          (center col_width Variable.pp v)
          (center col_width Format.pp_print_int i)
          (center col_width Format.pp_print_string c)
          (center col_width Format.pp_print_string
             (if Variable.Set.mem v program.input_vars then "x" else "") )
          (center col_width Format.pp_print_string
             (if Variable.Set.mem v program.output_vars then "x" else "") ) )
    l ;
  Format.fprintf ppf "%s@." sep

let pp_graph pp ppf g =
  VarGraph.iter
    (fun id par chi ->
      Format.(
        fprintf ppf "Node %a:@.@[<hv 2>Childrens: %a@;Parents: %a@]@." pp id
          (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ",@ ") Variable.pp)
          (VarGraph.Set.elements chi)
          (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ",@ ") Variable.pp)
          (VarGraph.Set.elements par) ) )
    g

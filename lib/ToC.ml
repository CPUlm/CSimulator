open Ast
open BlockSplitter
open Format

let value_array, regs_values, value_update, cycle_id, do_cycle =
  ("var_values", "regs_values", "var_last_update", "cycle_id", "do_cycle")

type loc = Global | Local

type local_env =
  { var_pos: (Variable.t, loc) Hashtbl.t
  ; reg_index: (Variable.t, int) Hashtbl.t
  ; var_defined: (Variable.t, unit) Hashtbl.t
  ; var_eq: (Variable.t, exp) Hashtbl.t }

type global_env =
  { var_pos: (Variable.t, loc) Hashtbl.t
  ; reg_index: (Variable.t, int) Hashtbl.t
  ; var_table_index: (Variable.t, int) Hashtbl.t
  ; var_eq: (Variable.t, exp) Hashtbl.t }

let arg_size = function Variable v -> Variable.size v | Constant c -> c.size

let var_fun ppf = fprintf ppf "fun_%a ()" Variable.pp

let var_out ppf = fprintf ppf "out_%a" Variable.pp

let var_mask ppf var =
  let size = Variable.size var in
  let () = assert (size < Sys.int_size) in
  let mask = Int.shift_left 1 size in
  fprintf ppf "%#x" (mask - 1)

let reg_last_value reg_map ppf reg =
  let reg_index = Hashtbl.find reg_map reg in
  fprintf ppf "%s[2*%i + ((%s+1)%%2)]" regs_values reg_index cycle_id

let reg_current_value reg_map ppf reg =
  let reg_index = Hashtbl.find reg_map reg in
  fprintf ppf "%s[2*%i + ((%s)%%2)]" regs_values reg_index cycle_id

let mk_tmp_ppf () =
  let sob = make_symbolic_output_buffer () in
  let fmt = formatter_of_symbolic_output_buffer sob in
  let () = fprintf fmt "@[<v>" in
  let pp_buf ppf () =
    fprintf fmt "@]" ;
    let data = flush_symbolic_output_buffer sob in
    let () =
      List.iter
        (function
          | Output_flush ->
              pp_print_flush ppf ()
          | Output_newline ->
              pp_print_cut ppf ()
          | Output_spaces i ->
              pp_print_string ppf (String.make i ' ')
          | Output_indent i ->
              pp_print_string ppf (String.make i ' ')
          | Output_string s ->
              pp_print_string ppf s )
        data
    in
    if data <> [] then pp_print_cut ppf ()
  in
  (fmt, pp_buf)

let rec process_arg env ppf arg =
  match arg with
  | Constant c ->
      fun ppf () -> pp_print_int ppf c.value
  | Variable var -> (
      if Hashtbl.mem env.var_defined var then fun ppf () -> Variable.pp ppf var
      else
        match Hashtbl.find env.var_pos var with
        | Local ->
            let eq = Hashtbl.find env.var_eq var in
            c_of_expr env ppf (var, eq) ;
            fun ppf () -> Variable.pp ppf var
        | Global ->
            fun ppf () -> var_fun ppf var )

and c_of_expr env ppf (var, eq) =
  let () = assert (not (Hashtbl.mem env.var_defined var)) in
  let () =
    match eq with
    | Arg arg ->
        let pp_arg = process_arg env ppf arg in
        fprintf ppf "@[<h>value_t %a = %a;@]@," Variable.pp var pp_arg ()
    | Reg reg ->
        fprintf ppf "@[<h>value_t %a = %a;@]@," Variable.pp var
          (reg_last_value env.reg_index)
          reg
    | Not arg ->
        let pp_arg = process_arg env ppf arg in
        fprintf ppf "@[<h>value_t %a = (~(%a)) & (%a);@]@," Variable.pp var
          pp_arg () var_mask var
    | Binop (binop, arg1, arg2) ->
        let pp_arg1 = process_arg env ppf arg1 in
        let pp_arg2 = process_arg env ppf arg2 in
        let () =
          match binop with
          | And ->
              fprintf ppf "@[<h>value_t %a = (%a) & (%a);@]@," Variable.pp var
                pp_arg1 () pp_arg2 ()
          | Or ->
              fprintf ppf "@[<h>value_t %a = (%a) | (%a);@]@," Variable.pp var
                pp_arg1 () pp_arg2 ()
          | Xor ->
              fprintf ppf "@[<h>value_t %a = (%a) ^ (%a);@]@," Variable.pp var
                pp_arg1 () pp_arg2 ()
          | Nand ->
              fprintf ppf "@[<h>value_t %a = (~(%a) & (%a)) & (%a);@]@,"
                Variable.pp var pp_arg1 () pp_arg2 () var_mask var
        in
        ()
    | Mux md ->
        let pp_cond = process_arg env ppf md.cond in
        let true_fmt, pp_true_fmt = mk_tmp_ppf () in
        let pp_true_arg = process_arg env true_fmt md.true_b in
        let false_fmt, pp_false_fmt = mk_tmp_ppf () in
        let pp_false_arg = process_arg env false_fmt md.false_b in
        fprintf ppf
          "@[<v>value_t %a;@,\
           if (%a) {@;\
           <0 4>@[<v>%a%a = %a;@]@,\
           } else {@;\
           <0 4>@[<v>%a%a = %a;@]@,\
           }@]@,"
          Variable.pp var pp_cond () pp_true_fmt () Variable.pp var pp_true_arg
          () pp_false_fmt () Variable.pp var pp_false_arg ()
    | Rom _ ->
        assert false
    | Ram _ ->
        assert false
    | Concat (arg1, arg2) ->
        let pp_arg1 = process_arg env ppf arg1 in
        let pp_arg2 = process_arg env ppf arg2 in
        fprintf ppf "@[<h>value_t %a = ((%a) << (%i) | (%a));@]@," Variable.pp
          var pp_arg2 () (arg_size arg1) pp_arg1 ()
    | Slice sd ->
        let pp_arg = process_arg env ppf sd.arg in
        fprintf ppf "@[<h>value_t %a = ((%a) >> (%i)) & (%a);@]@," Variable.pp
          var pp_arg () sd.min var_mask var
    | Select (index, arg) ->
        let pp_arg = process_arg env ppf arg in
        fprintf ppf "@[<h>value_t %a = ((%a) >> (%i)) & (%a);@]@," Variable.pp
          var pp_arg () index var_mask var
  in
  Hashtbl.add env.var_defined var ()

let block_fun env ppf (_, block) =
  let block_id = Hashtbl.find env.var_table_index block.repr in
  let lenv =
    { var_pos= env.var_pos
    ; var_defined= Hashtbl.create 17
    ; var_eq= env.var_eq
    ; reg_index= env.reg_index }
  in
  let repr_eq = Hashtbl.find env.var_eq block.repr in
  let () =
    fprintf ppf
      "@[<v>value_t %a {@;\
       <0 4>@[<v>if (%s[%i] == %s) {@;\
       <0 4>@[<h>return %s[%i];@]@,\
       } else {@;\
       <0 4>@[<v>%a@,\
       %s[%i] = %a;@,\
       %s[%i] = %s;@,\
       return %a;@]@,\
       }@]@,\
       }@]"
      var_fun block.repr value_update block_id cycle_id value_array block_id
      (c_of_expr lenv) (block.repr, repr_eq) value_array block_id Variable.pp
      block.repr value_update block_id cycle_id Variable.pp block.repr
  in
  let computed_vars =
    Hashtbl.to_seq_keys lenv.var_defined |> Variable.Set.of_seq
  in
  assert (
    Variable.Set.(
      is_empty (diff computed_vars block.members)
      && is_empty (diff block.members computed_vars) ) )

let block_def ppf (_, block) = fprintf ppf "value_t %a;" var_fun block.repr

(* out << "=> " << out_var->get_name() << " : ";
   out << "binary: 0b"
       << Utilities::value_to_str(var_v, out_var->get_bus_size());
   out << ", hex: 0x" << std::hex
       << std::setw((out_var->get_bus_size() + 3) / 4) << std::setfill('0')
       << var_v << ", unsigned decimal: " << std::dec << var_v
       << ", signed decimal: " << (long)sign_extended << "\n";
*)
let pp_prog ppf (program, blocks) =
  let var_pos = Hashtbl.create 17 in
  let () =
    Variable.Set.iter
      (fun v ->
        if Variable.Map.mem v blocks then Hashtbl.add var_pos v Global
        else Hashtbl.add var_pos v Local )
      program.vars
  in
  let var_table_index = Hashtbl.create 17 in
  let array_size =
    Variable.Map.fold
      (fun v _ index ->
        Hashtbl.add var_table_index v index ;
        index + 1 )
      blocks 0
  in
  let reg_index = Hashtbl.create 17 in
  let nb_regs =
    Variable.Set.fold
      (fun v index ->
        Hashtbl.add reg_index v index ;
        index + 1 )
      program.axioms.reg_vars 0
  in
  let genv = {var_pos; var_table_index; var_eq= program.eqs; reg_index} in
  let () =
    fprintf ppf
      "@[<v>/* Globals Vars */@,\
       value_t %s[%i];@,\
       value_t %s[2*%i];@,\
       cycle_t %s[%i];@,\
       cycle_t %s;@,\
       @,"
      value_array array_size regs_values nb_regs value_update array_size
      cycle_id
  in
  let () =
    fprintf ppf "/* Blocks Declarations */@,%a@,@," (pp_print_list block_def)
      (Variable.Map.to_list blocks)
  in
  let () =
    fprintf ppf "/* Blocks Implementations */@,%a@,@,"
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf "@,@,")
         (block_fun genv) )
      (Variable.Map.to_list blocks)
  in
  let () =
    fprintf ppf
      "/* Cycle Function */@,\
       @[<v>void %s () {@;\
       <0 4>@[<v>/* Compute Registers */@,\
       %a@,\
       @,\
       /* Compute Outputs */@,\
       %a@,\
       @,\
       /* Performs Writes */@,\
       %a@]@,\
       }@]"
      do_cycle
      (pp_print_list (fun ppf v ->
           assert (Variable.Map.mem v blocks) ;
           fprintf ppf "@[<h>%a = %a;@]"
             (reg_current_value reg_index)
             v var_fun v ) )
      (Variable.Set.to_list program.axioms.reg_vars)
      (pp_print_list (fun ppf v ->
           assert (Variable.Map.mem v blocks) ;
           fprintf ppf "@[<h>%a;@]" var_fun v ) )
      (Variable.Set.to_list program.axioms.out_vars)
      pp_print_string ""
  in
  fprintf ppf "@]@."

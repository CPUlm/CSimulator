open Ast
open BlockSplitter
open Format

let find_rams_roms program =
  Hashtbl.fold
    (fun v eq (roms, rams) ->
      match eq with
      | Ram _ ->
          (roms, Variable.Set.add v rams)
      | Rom _ ->
          (Variable.Set.add v roms, rams)
      | _ ->
          (roms, rams) )
    program.eqs
    Variable.Set.(empty, empty)

let vars_values, regs_values, vars_last_update, cycle_id, inputs_values =
  ("vars_values", "regs_values", "vars_last_update", "cycle_id", "inputs_values")

type loc = Global | Local | Input

type global_env =
  { var_pos: (Variable.t, loc) Hashtbl.t
  ; reg_index: (Variable.t, int) Hashtbl.t
  ; var_table_index: (Variable.t, int) Hashtbl.t
  ; var_eq: (Variable.t, exp) Hashtbl.t
  ; rom_var: Variable.t option
  ; ram_var: Variable.t option
  ; axioms: axiom
  ; vars: Variable.set
  ; inputs: int Variable.map
  ; blocks: block list
  ; with_screen: bool
  ; with_pause: bool }

let arg_size = function Variable v -> Variable.size v | Constant c -> c.size

let var_fun ppf = fprintf ppf "fun_%a()" Variable.pp

let var_out ppf = fprintf ppf "out_%a" Variable.pp

let var_rom ppf = fprintf ppf "rom_%a" Variable.pp

let var_ram ppf = fprintf ppf "ram_%a" Variable.pp

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
  let buf = Buffer.create 17 in
  let fmt = formatter_of_buffer buf in
  let () = fprintf fmt "@[<v>" in
  let pp_buf ppf () =
    fprintf fmt "@]" ;
    pp_print_flush fmt () ;
    let data = String.split_on_char '\n' (Buffer.contents buf) in
    let () =
      List.iter (fun s -> if s == "" then () else fprintf ppf "%s@," s) data
    in
    ()
  in
  (fmt, pp_buf)

let rec process_arg (env, v_def) ppf arg =
  match arg with
  | Constant c ->
      (v_def, fun ppf () -> pp_print_int ppf c.value)
  | Variable var -> (
    match Hashtbl.find env.var_pos var with
    | Local ->
        if Variable.Set.mem var v_def then
          (v_def, fun ppf () -> Variable.pp ppf var)
        else
          let eq = Hashtbl.find env.var_eq var in
          let v_def = c_of_expr (env, v_def) ppf (var, eq) in
          (v_def, fun ppf () -> Variable.pp ppf var)
    | Global ->
        (v_def, fun ppf () -> var_fun ppf var)
    | Input ->
        ( v_def
        , fun ppf () ->
            fprintf ppf "%s[%i]" inputs_values
              (Variable.Map.find var env.inputs) ) )

and c_of_expr (env, v_def) ppf (var, eq) =
  let () = assert (not (Variable.Set.mem var v_def)) in
  let v_def =
    match eq with
    | Arg arg ->
        let v_def, pp_arg = process_arg (env, v_def) ppf arg in
        let () =
          fprintf ppf "@[<h>value_t %a = %a;@]@," Variable.pp var pp_arg ()
        in
        v_def
    | Reg reg ->
        let () =
          fprintf ppf "@[<h>value_t %a = %a;@]@," Variable.pp var
            (reg_last_value env.reg_index)
            reg
        in
        v_def
    | Not arg ->
        let v_def, pp_arg = process_arg (env, v_def) ppf arg in
        let () =
          fprintf ppf "@[<h>value_t %a = (~(%a)) & (%a);@]@," Variable.pp var
            pp_arg () var_mask var
        in
        v_def
    | Binop (binop, arg1, arg2) ->
        let v_def, pp_arg1 = process_arg (env, v_def) ppf arg1 in
        let v_def, pp_arg2 = process_arg (env, v_def) ppf arg2 in
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
        v_def
    | Mux md ->
        let v_def, pp_cond = process_arg (env, v_def) ppf md.cond in
        let true_fmt, pp_true_fmt = mk_tmp_ppf () in
        let _, pp_true_arg = process_arg (env, v_def) true_fmt md.true_b in
        let false_fmt, pp_false_fmt = mk_tmp_ppf () in
        let _, pp_false_arg = process_arg (env, v_def) false_fmt md.false_b in
        let () =
          fprintf ppf
            "@[<v>value_t %a;@,\
             if (!%a) {@;\
             <0 4>@[<v>%a%a = %a;@]@,\
             } else {@;\
             <0 4>@[<v>%a%a = %a;@]@,\
             }@]@,"
            Variable.pp var pp_cond () pp_true_fmt () Variable.pp var
            pp_true_arg () pp_false_fmt () Variable.pp var pp_false_arg ()
        in
        v_def
    | Rom romd -> (
      match env.rom_var with
      | Some rom_var ->
          assert (var = rom_var) ;
          let v_def, pp_arg = process_arg (env, v_def) ppf romd.read_addr in
          fprintf ppf "@[<h>value_t %a = rom_get(%a, %a);@]@," Variable.pp var
            var_rom rom_var pp_arg () ;
          v_def
      | None ->
          (* We can only have one ROM Block in this simulator *)
          assert false )
    | Ram ramd -> (
      match env.ram_var with
      | Some ram_var ->
          assert (var = ram_var) ;
          let v_def, pp_arg = process_arg (env, v_def) ppf ramd.read_addr in
          fprintf ppf "@[<h>value_t %a = ram_get(%a, %a);@]@," Variable.pp var
            var_ram ram_var pp_arg () ;
          v_def
      | None ->
          (* We can only have one RAM Block in this simulator *)
          assert false )
    | Concat (arg1, arg2) ->
        let v_def, pp_arg1 = process_arg (env, v_def) ppf arg1 in
        let v_def, pp_arg2 = process_arg (env, v_def) ppf arg2 in
        let () =
          fprintf ppf "@[<h>value_t %a = ((%a) << (%i) | (%a));@]@," Variable.pp
            var pp_arg2 () (arg_size arg1) pp_arg1 ()
        in
        v_def
    | Slice sd ->
        let v_def, pp_arg = process_arg (env, v_def) ppf sd.arg in
        let () =
          fprintf ppf "@[<h>value_t %a = ((%a) >> (%i)) & (%a);@]@," Variable.pp
            var pp_arg () sd.min var_mask var
        in
        v_def
    | Select (index, arg) ->
        let v_def, pp_arg = process_arg (env, v_def) ppf arg in
        let () =
          fprintf ppf "@[<h>value_t %a = ((%a) >> (%i)) & (%a);@]@," Variable.pp
            var pp_arg () index var_mask var
        in
        v_def
  in
  Variable.Set.add var v_def

let block_fun env ppf block =
  let block_id = Hashtbl.find env.var_table_index block.repr in
  let repr_eq = Hashtbl.find env.var_eq block.repr in
  let () =
    fprintf ppf
      "@[<v>value_t %a {@;\
       <0 4>@[<v>if (%s[%i] == %s) {@;\
       <0 4>@[<h>return %s[%i];@]@,\
       } else {@;\
       <0 4>@[<v>" var_fun block.repr vars_last_update block_id cycle_id
      vars_values block_id
  in
  let _ = c_of_expr (env, Variable.Set.empty) ppf (block.repr, repr_eq) in
  let () =
    fprintf ppf "@,%s[%i] = %a;@,%s[%i] = %s;@,return %a;@]@,}@]@,}@]"
      vars_values block_id Variable.pp block.repr vars_last_update block_id
      cycle_id Variable.pp block.repr
  in
  ()

let block_def ppf block = fprintf ppf "value_t %a;" var_fun block.repr

let do_cycle_fun ppf genv =
  let get_var_value ppf v =
    match Hashtbl.find genv.var_pos v with
    | Local ->
        failwith "Not possible !"
    | Global ->
        var_fun ppf v
    | Input ->
        fprintf ppf "%s[%i]" inputs_values (Variable.Map.find v genv.inputs)
  in
  let () =
    fprintf ppf
      "/* Cycle Function */@,\
       @[<v>bool do_cycle(cycle_t *cid) {@;\
       <0 4>@[<v>/* New cycle */@,\
       %s++;@,\
       *cid = %s;@,\
       @,"
      cycle_id cycle_id
  in
  let () =
    if
      Variable.Map.is_empty genv.inputs
      && Variable.Set.is_empty genv.axioms.out_vars
    then ()
    else fprintf ppf "@,fprintf(stdout, \"\\nStep: %%lu\\n\", %s);@,@," cycle_id
  in
  let () =
    if Variable.Map.is_empty genv.inputs then ()
    else
      fprintf ppf "/* Retrieve Inputs */@,%a@,@,"
        (pp_print_list (fun ppf (var, index) ->
             fprintf ppf "%s[%i] = get_input(\"%a\", %i);" inputs_values index
               Variable.pp var (Variable.size var) ) )
        (Variable.Map.bindings genv.inputs)
  in
  let () =
    if Variable.Set.is_empty genv.axioms.reg_vars then ()
    else
      let reg_vars = Variable.Set.elements genv.axioms.reg_vars in
      fprintf ppf "/* Compute Registers */@,%a@,@,"
        (pp_print_list (fun ppf v ->
             fprintf ppf "@[<h>%a = %a;@]"
               (reg_current_value genv.reg_index)
               v var_fun v ) )
        reg_vars
  in
  let () =
    if Variable.Set.is_empty genv.axioms.out_vars then ()
    else
      let outvars = Variable.Set.elements genv.axioms.out_vars in
      fprintf ppf "/* Compute Outputs */@,%a@,@,print_header(stdout);@,%a@,@,"
        (pp_print_list (fun ppf v ->
             fprintf ppf "@[<h>value_t %a = %a;@]" var_out v get_var_value v )
        )
        outvars
        (pp_print_list (fun ppf v ->
             fprintf ppf "print_variable(stdout, \"%a\", %a, %i);" Variable.pp v
               var_out v (Variable.size v) ) )
        outvars
  in
  let pp_arg ppf = function
    | Variable v ->
        get_var_value ppf v
    | Constant c ->
        pp_print_int ppf c.value
  in
  let () =
    match genv.ram_var with
    | None ->
        ()
    | Some ram_var ->
        fprintf ppf "/* Performs Write */@,%a@,"
          (fun ppf ram_var ->
            match Hashtbl.find genv.var_eq ram_var with
            | Ram ramd ->
                fprintf ppf
                  "@[<v>if (%a) {@;<0 4>@[<h>ram_set(%a, %a, %a);@]@,}@]@,"
                  pp_arg ramd.write_enable var_ram ram_var pp_arg
                  ramd.write_addr pp_arg ramd.write_data
            | _ ->
                assert false )
          ram_var
  in
  let () = if genv.with_pause then fprintf ppf "getchar();@," in
  let () = fprintf ppf "return false;@]@,}@]@,@," in
  ()

let init_rom_fun ppf genv =
  let () =
    fprintf ppf "@[<v>void init_rom(const char *rom_file) {@;<0 4>@[<v>"
  in
  let () =
    match genv.rom_var with
    | None ->
        ()
    | Some rom_var ->
        fprintf ppf
          "if (rom_file == NULL) {@;\
           <0 4>@[<v>fprintf(stdout, \"Error: Expected a ROM File.\\n\");@,\
           exit(1);@]@,\
           } else {@;\
           <0 4>@[<h>%a = rom_from_file(rom_file);@]@,\
           }@]"
          var_rom rom_var
  in
  let () = fprintf ppf "@]@,}@,@," in
  ()

let init_ram_fun ppf genv =
  let () =
    fprintf ppf "@[<v>void init_ram(const char *ram_file) {@;<0 4>@[<v>"
  in
  let () =
    match genv.ram_var with
    | None ->
        ()
    | Some ram_var ->
        let () =
          fprintf ppf
            "if (ram_file == NULL) {@;\
             <0 4>@[<v>fprintf(stdout, \"Error: Expected a RAM File.\\n\");@,\
             exit(1);@]@,\
             } else {@;\
             <0 4>@[<v>%a = ram_from_file(ram_file);" var_ram ram_var
        in
        let () =
          if genv.with_screen then
            fprintf ppf "@,screen_init_with_ram_mapping(%a);" var_ram ram_var
        in
        let () = fprintf ppf "@]@,}@]" in
        ()
  in
  let () = fprintf ppf "@]@,}@,@," in
  ()

let end_simul_fun ppf env =
  let () =
    fprintf ppf
      "/* End Simulation Function */@,@[<v>void end_simulation() {@;<0 4>@[<v>"
  in
  let () =
    match env.rom_var with
    | None ->
        ()
    | Some rom_var ->
        fprintf ppf "@[<v>/* Free Rom */@,rom_destroy(%a);@]@,@," var_rom
          rom_var
  in
  let () =
    match env.ram_var with
    | None ->
        ()
    | Some ram_var ->
        fprintf ppf "@[<v>/* Free Ram */@,ram_destroy(%a);@]@," var_ram ram_var
  in
  let () = fprintf ppf "@]@,}@]@," in
  ()

let pp_prog ppf genv =
  let array_size = Hashtbl.length genv.var_table_index in
  let nb_regs = Hashtbl.length genv.reg_index in
  let nb_inputs = Variable.Map.cardinal genv.inputs in
  let () =
    fprintf ppf
      "@[<v>/* Includes */@,\
       #include \"commons.h\"@,\
       #include \"memory.h\"@,\
       #include \"screen.h\"@,\
       @,"
  in
  let () = fprintf ppf "/* Globals Vars */@,cycle_t %s = 0;@," cycle_id in
  let () =
    if array_size <> 0 then
      fprintf ppf "value_t %s[%i] = {0};@,cycle_t %s[%i] = {0};@," vars_values
        array_size vars_last_update array_size
  in
  let () =
    if nb_regs <> 0 then
      fprintf ppf "value_t %s[2*%i] = {0};@," regs_values nb_regs
  in
  let () =
    if nb_inputs <> 0 then
      fprintf ppf "value_t %s[%i] = {0};@,@," inputs_values nb_inputs
  in
  let () =
    match genv.rom_var with
    | None ->
        ()
    | Some rom_var ->
        fprintf ppf "@[<v>/* ROM Declaration */@,rom_t %a;@,@]@," var_rom
          rom_var
  in
  let () =
    match genv.ram_var with
    | None ->
        ()
    | Some ram_var ->
        fprintf ppf "@[<v>/* RAM Declaration */@,ram_t* %a;@,@]@," var_ram
          ram_var
  in
  let () =
    fprintf ppf "/* Blocks Declarations */@,%a@,@," (pp_print_list block_def)
      genv.blocks
  in
  let () =
    fprintf ppf "/* Blocks Implementations */@,%a@,@,"
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf "@,@,")
         (block_fun genv) )
      genv.blocks
  in
  let () = do_cycle_fun ppf genv in
  let () = init_rom_fun ppf genv in
  let () = init_ram_fun ppf genv in
  let () = end_simul_fun ppf genv in
  fprintf ppf "@]@."

let create_env (program : program) blocks with_screen with_pause =
  let var_pos = Hashtbl.create 17 in
  let () =
    Variable.Set.iter
      (fun v ->
        if Variable.Set.mem v program.input_vars then
          Hashtbl.add var_pos v Input
        else if Variable.Map.mem v blocks then Hashtbl.add var_pos v Global
        else Hashtbl.add var_pos v Local )
      program.vars
  in
  let var_table_index = Hashtbl.create 17 in
  let blocks = Variable.Map.bindings blocks in
  let blocks =
    List.mapi
      (fun index (v, b) ->
        Hashtbl.add var_table_index v index ;
        b )
      blocks
  in
  let reg_index = Hashtbl.create 17 in
  let _ =
    Variable.Set.fold
      (fun v index ->
        Hashtbl.add reg_index v index ;
        index + 1 )
      program.axioms.reg_vars 0
  in
  let inputs, _ =
    Variable.Set.fold
      (fun v (acc, index) -> (Variable.Map.add v index acc, index + 1))
      program.input_vars (Variable.Map.empty, 0)
  in
  { var_pos
  ; var_table_index
  ; var_eq= program.eqs
  ; reg_index
  ; rom_var= program.rom_var
  ; ram_var= program.ram_var
  ; axioms= program.axioms
  ; vars= program.vars
  ; blocks
  ; inputs
  ; with_screen
  ; with_pause }

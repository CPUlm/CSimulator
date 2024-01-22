open Ast
open BlockSplitter
open Format

let ( vars_values
    , regs_values
    , vars_last_update
    , cycle_id
    , inputs_values
    , need_stop ) =
  ( "vars_values"
  , "regs_values"
  , "vars_last_update"
  , "cycle_id"
  , "inputs_values"
  , "need_stop" )

type loc =
  | Global of (formatter -> unit -> unit)
  | Local
  | Input of (formatter -> unit -> unit)

type global_env =
  { var_compare: Variable.t -> Variable.t -> int
  ; var_pos: (Variable.t, loc) Hashtbl.t
  ; var_table_index: (Variable.t, int) Hashtbl.t
  ; var_eq: (Variable.t, exp) Hashtbl.t
  ; rom_var: (Variable.t * (formatter -> unit -> unit)) option
  ; ram_var: (Variable.t * (formatter -> unit -> unit)) option
  ; reg_vars:
      ( Variable.t
      , (formatter -> unit -> unit) * (formatter -> unit -> unit) )
      Hashtbl.t
  ; inputs: Variable.set
  ; outputs: Variable.set
  ; with_pause: bool
  ; with_screen: bool
  ; with_debug: bool
  ; with_tick: bool
  ; blocks: block list }

type local_env =
  { var_pos: (Variable.t, loc) Hashtbl.t
  ; rom_var: (Variable.t * (formatter -> unit -> unit)) option
  ; ram_var: (Variable.t * (formatter -> unit -> unit)) option
  ; reg_vars:
      ( Variable.t
      , (formatter -> unit -> unit) * (formatter -> unit -> unit) )
      Hashtbl.t
  ; var_compare: Variable.t -> Variable.t -> int
  ; defined_vars: (formatter -> unit -> unit) Variable.map
  ; block_vars: exp Variable.map }

let arg_size = function Variable v -> Variable.size v | Constant c -> c.size

let var_mask ppf var =
  let size = Variable.size var in
  let () = assert (size < Sys.int_size) in
  let mask = Int.shift_left 1 size in
  fprintf ppf "%#x" (mask - 1)

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

let needed_vars lenv arg =
  let visited = Hashtbl.create 17 in
  let rec loop acc = function
    | Constant _ ->
        acc
    | Variable v -> (
        if Hashtbl.mem visited v then acc
        else
          let () = Hashtbl.add visited v () in
          match Hashtbl.find lenv.var_pos v with
          | Local -> (
            match Variable.Map.find_opt v lenv.defined_vars with
            | Some _ ->
                acc
            | None -> (
              match Variable.Map.find v lenv.block_vars with
              | Arg arg | Not arg | Select (_, arg) ->
                  let loc = loop acc arg in
                  Variable.Set.(add v loc)
              | Reg _ ->
                  Variable.Set.empty
              | Binop (_, lhs, rhs) | Concat (lhs, rhs) ->
                  let lhs_loc = loop acc lhs in
                  let rhs_loc = loop acc rhs in
                  let loc = Variable.Set.(union lhs_loc rhs_loc) in
                  Variable.Set.add v loc
              | Rom rd ->
                  let loc = loop acc rd.read_addr in
                  Variable.Set.add v loc
              | Ram rd ->
                  let loc = loop acc rd.read_addr in
                  Variable.Set.add v loc
              | Slice sd ->
                  let loc = loop acc sd.arg in
                  Variable.Set.add v loc
              | Mux md ->
                  let cond_loc = loop acc md.cond in
                  let true_loc = loop acc md.true_b in
                  let false_loc = loop acc md.false_b in
                  let loc =
                    Variable.Set.(union cond_loc (inter true_loc false_loc))
                  in
                  Variable.Set.add v loc ) )
          | Global _ ->
              acc
          | Input _ ->
              acc )
  in
  loop Variable.Set.empty arg

let var_pp_of_var v ppf () = fprintf ppf "var_%a" Variable.pp v

let nop ppf () = ignore ppf

let rec process_arg lenv arg =
  match arg with
  | Constant c ->
      (lenv, nop, fun ppf () -> pp_print_int ppf c.value)
  | Variable var -> (
    match Hashtbl.find lenv.var_pos var with
    | Local -> (
      match Variable.Map.find_opt var lenv.defined_vars with
      | Some name ->
          (lenv, nop, name)
      | None ->
          let tmp_ppf, pp_tmp_ppf = mk_tmp_ppf () in
          let lenv = c_of_expr lenv tmp_ppf var in
          let pp_var = Variable.Map.find var lenv.defined_vars in
          (lenv, pp_tmp_ppf, pp_var) )
    | Global fun_name ->
        (lenv, nop, fun_name)
    | Input index_pos ->
        (lenv, nop, index_pos) )

and c_of_expr lenv ppf var =
  let () = assert (Variable.Map.mem var lenv.block_vars) in
  let () = assert (not (Variable.Map.mem var lenv.defined_vars)) in
  let eq = Variable.Map.find var lenv.block_vars in
  let var_pp = var_pp_of_var var in
  let lenv =
    match eq with
    | Arg arg ->
        let lenv, pp_before, pp_arg = process_arg lenv arg in
        let () = pp_before ppf () in
        let () = fprintf ppf "@[<h>value_t %a = %a;@]@," var_pp () pp_arg () in
        lenv
    | Reg reg ->
        let last, _ = Hashtbl.find lenv.reg_vars reg in
        let () = fprintf ppf "@[<h>value_t %a = %a;@]@," var_pp () last () in
        lenv
    | Not arg ->
        let lenv, pp_before, pp_arg = process_arg lenv arg in
        let () = pp_before ppf () in
        let () =
          fprintf ppf "@[<h>value_t %a = (~(%a)) & (%a);@]@," var_pp () pp_arg
            () var_mask var
        in
        lenv
    | Binop (binop, arg1, arg2) ->
        let lenv, pp_before1, pp_arg1 = process_arg lenv arg1 in
        let lenv, pp_before2, pp_arg2 = process_arg lenv arg2 in
        let () = pp_before1 ppf () in
        let () = pp_before2 ppf () in
        let () =
          match binop with
          | And ->
              fprintf ppf "@[<h>value_t %a = (%a) & (%a);@]@," var_pp () pp_arg1
                () pp_arg2 ()
          | Or ->
              fprintf ppf "@[<h>value_t %a = (%a) | (%a);@]@," var_pp () pp_arg1
                () pp_arg2 ()
          | Xor ->
              fprintf ppf "@[<h>value_t %a = (%a) ^ (%a);@]@," var_pp () pp_arg1
                () pp_arg2 ()
          | Nand ->
              fprintf ppf "@[<h>value_t %a = (~(%a) & (%a)) & (%a);@]@," var_pp
                () pp_arg1 () pp_arg2 () var_mask var
        in
        lenv
    | Mux md ->
        let lenv, pp_before_cond, pp_cond = process_arg lenv md.cond in
        let () = pp_before_cond ppf () in
        let true_loc_needed = needed_vars lenv md.true_b in
        let false_loc_needed = needed_vars lenv md.false_b in
        let local_vars_needed =
          Variable.Set.(inter true_loc_needed false_loc_needed |> elements)
          |> List.sort lenv.var_compare
        in
        let lenv =
          List.fold_left
            (fun lenv v ->
              if Variable.Map.mem v lenv.defined_vars then lenv
              else
                let lenv = c_of_expr lenv ppf v in
                lenv )
            lenv local_vars_needed
        in
        let _, pp_bef_true, pp_true = process_arg lenv md.true_b in
        let _, pp_bef_false, pp_false = process_arg lenv md.false_b in
        let () =
          fprintf ppf
            "@[<v>value_t %a;@,\
             if (!%a) {@;\
             <0 4>@[<v>%a%a = %a;@]@,\
             } else {@;\
             <0 4>@[<v>%a%a = %a;@]@,\
             }@]@,"
            var_pp () pp_cond () pp_bef_true () var_pp () pp_true ()
            pp_bef_false () var_pp () pp_false ()
        in
        lenv
    | Rom romd ->
        let rom_var =
          match lenv.rom_var with
          | Some (_, rom_pp) ->
              rom_pp
          | None ->
              assert false
        in
        let max_addr = Int64.(sub (shift_left one romd.addr_size) one) in
        let lenv, pp_before, pp_arg = process_arg lenv romd.read_addr in
        let () = pp_before ppf () in
        let () =
          fprintf ppf
            "@[<v>value_t %a = 0;@,\
             value_t read_addr = %a;@,\
             if (read_addr == %#Lx) {@;\
             <0 4>@[<h>%s = true;@]@,\
             } else {@;\
             <0 4>@[<h>%a = rom_get(%a, read_addr);@]@,\
             }@]@,"
            var_pp () pp_arg () max_addr need_stop var_pp () rom_var ()
        in
        lenv
    | Ram ramd ->
        let ram_var =
          match lenv.ram_var with
          | Some (_, ram_pp) ->
              ram_pp
          | None ->
              assert false
        in
        let lenv, pp_before, pp_arg = process_arg lenv ramd.read_addr in
        let () = pp_before ppf () in
        let () =
          fprintf ppf "@[<h>value_t %a = ram_get(%a, %a);@]@," var_pp () ram_var
            () pp_arg ()
        in
        lenv
    | Concat (arg1, arg2) ->
        let lenv, pp_before1, pp_arg1 = process_arg lenv arg1 in
        let lenv, pp_before2, pp_arg2 = process_arg lenv arg2 in
        let () = pp_before1 ppf () in
        let () = pp_before2 ppf () in
        let () =
          fprintf ppf "@[<h>value_t %a = ((%a) << (%i) | (%a));@]@," var_pp ()
            pp_arg2 () (arg_size arg1) pp_arg1 ()
        in
        lenv
    | Slice sd ->
        let lenv, pp_before, pp_arg = process_arg lenv sd.arg in
        let () = pp_before ppf () in
        let () =
          fprintf ppf "@[<h>value_t %a = ((%a) >> (%i)) & (%a);@]@," var_pp ()
            pp_arg () sd.min var_mask var
        in
        lenv
    | Select (index, arg) ->
        let lenv, pp_before, pp_arg = process_arg lenv arg in
        let () = pp_before ppf () in
        let () =
          fprintf ppf "@[<h>value_t %a = ((%a) >> (%i)) & (%a);@]@," var_pp ()
            pp_arg () index var_mask var
        in
        lenv
  in
  {lenv with defined_vars= Variable.Map.add var var_pp lenv.defined_vars}

let block_fun genv ppf block =
  let block_id = Hashtbl.find genv.var_table_index block.repr in
  let block_vars =
    Variable.Set.fold
      (fun v map -> Variable.Map.add v (Hashtbl.find genv.var_eq v) map)
      block.members Variable.Map.empty
  in
  let lenv =
    { defined_vars= Variable.Map.empty
    ; block_vars
    ; rom_var= genv.rom_var
    ; ram_var= genv.ram_var
    ; var_pos= genv.var_pos
    ; var_compare= genv.var_compare
    ; reg_vars= genv.reg_vars }
  in
  let pp_fun =
    match Hashtbl.find genv.var_pos block.repr with
    | Global f ->
        f
    | _ ->
        assert false
  in
  let () =
    fprintf ppf
      "@[<v>value_t %a {@;\
       <0 4>@[<v>if (%s[%i] == %s) {@;\
       <0 4>@[<h>return %s[%i];@]@,\
       } else {@;\
       <0 4>@[<v>" pp_fun () vars_last_update block_id cycle_id vars_values
      block_id
  in
  let lenv = c_of_expr lenv ppf block.repr in
  let pp_var = Variable.Map.find block.repr lenv.defined_vars in
  let () =
    fprintf ppf "@,%s[%i] = %a;@,%s[%i] = %s;@,return %a;@]@,}@]@,}@]"
      vars_values block_id pp_var () vars_last_update block_id cycle_id pp_var
      ()
  in
  ()

let do_cycle_fun ppf (genv : global_env) =
  let get_var_value ppf v =
    match Hashtbl.find genv.var_pos v with
    | Local ->
        assert false
    | Global g ->
        g ppf ()
    | Input i ->
        i ppf ()
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
    if Variable.Set.is_empty genv.inputs && Variable.Set.is_empty genv.outputs
    then ()
    else fprintf ppf "@,fprintf(stdout, \"\\nStep: %%lu\\n\", %s);@,@," cycle_id
  in
  let () =
    if Variable.Set.is_empty genv.inputs then ()
    else
      fprintf ppf "/* Retrieve Inputs */@,%a@,@,"
        (pp_print_seq (fun ppf var ->
             let pp =
               match Hashtbl.find genv.var_pos var with
               | Input f ->
                   f
               | _ ->
                   assert false
             in
             fprintf ppf "%a = get_input(\"%a\", %i);" pp () Variable.pp var
               (Variable.size var) ) )
        (Variable.Set.to_seq genv.inputs)
  in
  let () =
    if Hashtbl.length genv.reg_vars <> 0 then
      fprintf ppf "/* Compute Registers */@,%a@,@,"
        (pp_print_seq (fun ppf (reg, (_, current_reg)) ->
             fprintf ppf "@[<h>%a = %a;@]" current_reg () get_var_value reg ) )
        (Hashtbl.to_seq genv.reg_vars)
  in
  let () =
    if Variable.Set.is_empty genv.outputs then ()
    else
      let var_out ppf v = fprintf ppf "out_%a" Variable.pp v in
      let outvars = Variable.Set.to_seq genv.outputs in
      fprintf ppf "/* Compute Outputs */@,%a@,@,print_header(stdout);@,%a@,@,"
        (pp_print_seq (fun ppf v ->
             fprintf ppf "@[<h>value_t %a = %a;@]" var_out v get_var_value v )
        )
        outvars
        (pp_print_seq (fun ppf v ->
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
    | Some (ram_var, ram_pp) -> (
      match Hashtbl.find genv.var_eq ram_var with
      | Ram ramd ->
          let () =
            fprintf ppf
              "/* Performs Writes */@,\
               @[<v>if (%a) {@;\
               <0 4>@[<h>ram_set(%a, %a, %a);@]@,\
               }@]@,"
              pp_arg ramd.write_enable ram_pp () pp_arg ramd.write_addr pp_arg
              ramd.write_data
          in
          let () =
            if genv.with_tick then fprintf ppf "clock_tick(%a);@," ram_pp ()
          in
          ()
      | _ ->
          assert false )
  in
  let () =
    if genv.with_pause then
      fprintf ppf "printf(\"\\x1b[%d;%dH\");@,getchar();@," 1 17
  in
  let () = fprintf ppf "return %s;@]@,}@]@,@," need_stop in
  ()

let init_rom_fun ppf (genv : global_env) =
  let () =
    fprintf ppf "@[<v>void init_rom(const char *rom_file) {@;<0 4>@[<v>"
  in
  let () =
    match genv.rom_var with
    | None ->
        ()
    | Some (_, rom_pp) ->
        fprintf ppf
          "if (rom_file == NULL) {@;\
           <0 4>@[<v>fprintf(stdout, \"Error: Expected a ROM File.\\n\");@,\
           exit(1);@]@,\
           } else {@;\
           <0 4>@[<h>%a = rom_from_file(rom_file);@]@,\
           }@]"
          rom_pp ()
  in
  let () = fprintf ppf "@]@,}@,@," in
  ()

let init_ram_fun ppf (genv : global_env) =
  let () =
    fprintf ppf "@[<v>void init_ram(const char *ram_file) {@;<0 4>@[<v>"
  in
  match genv.ram_var with
  | None ->
      ()
  | Some (_, ram_pp) ->
      let () =
        fprintf ppf
          "if (ram_file == NULL) {@;\
           <0 4>@[<v>fprintf(stdout, \"Error: Expected a RAM File.\\n\");@,\
           exit(1);@]@,\
           } else {@;\
           <0 4>@[<v>%a = ram_from_file(ram_file);" ram_pp ()
      in
      let () =
        if genv.with_screen then
          fprintf ppf "@,screen_init_with_ram_mapping(%a);" ram_pp ()
      in
      let () =
        if genv.with_debug then
          fprintf ppf
            "@,\
             ram_install_read_debugger(%a, %b);@,\
             ram_install_write_debugger(%a, %b);" ram_pp () genv.with_screen
            ram_pp () genv.with_screen
      in
      let () = fprintf ppf "@]@,}@]" in
      let () = fprintf ppf "@]@,}@,@," in
      ()

let end_simul_fun ppf (genv : global_env) =
  let () =
    fprintf ppf
      "/* End Simulation Function */@,@[<v>void end_simulation() {@;<0 4>@[<v>"
  in
  let () =
    match genv.rom_var with
    | None ->
        ()
    | Some (_, rom_pp) ->
        fprintf ppf "@[<v>/* Free Rom */@,rom_destroy(%a);@]@,@," rom_pp ()
  in
  let () =
    match genv.ram_var with
    | None ->
        ()
    | Some (_, ram_pp) ->
        fprintf ppf "@[<v>/* Free Ram */@,ram_destroy(%a);@]@,@," ram_pp ()
  in
  let () =
    if genv.with_screen then
      fprintf ppf
        "@[<v>/* Restore Screen */@,\
         screen_terminate();@,\
         fprintf(stdout,\"\\n\");@]@,"
  in
  let () =
    if genv.with_debug then
      fprintf ppf "@[<v>fprintf(stdout,\"Number of cycle: %%i\\n\", %s);@]@,"
        cycle_id
  in
  let () = fprintf ppf "@]@,}@]@," in
  ()

let pp_prog ppf (genv : global_env) =
  let array_size = Hashtbl.length genv.var_table_index in
  let nb_regs = Hashtbl.length genv.reg_vars in
  let nb_inputs = Variable.Set.cardinal genv.inputs in
  let () =
    fprintf ppf
      "@[<v>/* Includes */@,#include \"commons.h\"@,#include \"screen.h\"@,@,"
  in
  let () =
    fprintf ppf "/* Globals Vars */@,cycle_t %s = 0;@,bool %s = false;@,"
      cycle_id need_stop
  in
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
    | Some (_, pp_rom) ->
        fprintf ppf "@[<v>/* ROM Declaration */@,rom_t %a;@,@]@," pp_rom ()
    | None ->
        ()
  in
  let () =
    match genv.ram_var with
    | Some (_, pp_ram) ->
        fprintf ppf "@[<v>/* RAM Declaration */@,ram_t* %a;@,@]@," pp_ram ()
    | None ->
        ()
  in
  let () =
    fprintf ppf "/* Blocks Declarations */@,%a@,@,"
      (pp_print_seq
         ~pp_sep:(fun ppf () -> ignore ppf)
         (fun ppf -> function
           | Input _ | Local ->
               ()
           | Global pp ->
               fprintf ppf "value_t %a;@," pp () ) )
      (Hashtbl.to_seq_values genv.var_pos)
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

let create_env (program : program) blocks with_screen with_pause with_debug
    with_tick =
  let var_pos = Hashtbl.create 17 in
  let () =
    Variable.Set.fold
      (fun v index ->
        Hashtbl.add var_pos v
          (Input (fun ppf () -> fprintf ppf "%s[%i]" inputs_values index)) ;
        index + 1 )
      program.input_vars 0
    |> ignore
  in
  let () =
    Variable.Set.iter
      (fun v ->
        if Variable.Set.mem v program.input_vars then ()
        else if Variable.Map.mem v blocks then
          Hashtbl.add var_pos v
            (Global (fun ppf () -> fprintf ppf "fun%a()" Variable.pp v))
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
  let rom_var =
    Option.map
      (fun v -> (v, fun ppf () -> fprintf ppf "rom%a" Variable.pp v))
      program.rom_var
  in
  let ram_var =
    Option.map
      (fun v -> (v, fun ppf () -> fprintf ppf "ram%a" Variable.pp v))
      program.ram_var
  in
  let reg_vars = Hashtbl.create 17 in
  let () =
    Variable.Set.fold
      (fun v index ->
        Hashtbl.add reg_vars v
          ( (fun ppf () ->
              fprintf ppf "%s[2*%i + ((%s+1)%%2)]" regs_values index cycle_id )
          , fun ppf () ->
              fprintf ppf "%s[2*%i + ((%s)%%2)]" regs_values index cycle_id ) ;
        index + 1 )
      program.axioms.reg_vars 0
    |> ignore
  in
  ( { var_table_index
    ; var_pos
    ; var_eq= program.eqs
    ; rom_var
    ; ram_var
    ; var_compare=
        (fun a b ->
          Int.compare
            (Hashtbl.find program.order a)
            (Hashtbl.find program.order b) )
    ; blocks
    ; with_debug
    ; inputs= program.input_vars
    ; outputs= program.output_vars
    ; reg_vars
    ; with_screen
    ; with_pause
    ; with_tick }
    : global_env )

open Ast
module SSet = Set.Make (String)

let pp_fct ppf ram = Format.fprintf ppf "fct_%s" ram

let pp_ram ppf ram = Format.fprintf ppf "ram_%s" ram

let pp_rom ppf rom = Format.fprintf ppf "rom_%s" rom

let pp_var ppf var = Format.fprintf ppf "var_%s" var

let pp_arg ppf = function
  | Avar ident ->
      Format.fprintf ppf "gate_calc(&%a)" pp_var ident
  | Aconst (VBit b) ->
      Format.fprintf ppf "%#xu" (Bool.to_int b)
  | Aconst (VBitArray a) ->
      let value =
        Array.fold_left (fun result bit -> (result lsl 1) + Bool.to_int bit) 0 a
      in
      Format.fprintf ppf "%#xu" value

let gateSize p ident =
  let t = Env.find ident p.p_vars in
  match t with TBit -> 1 | TBitArray n -> n

let pp_inst p ident ppf = function
  | Earg a1 ->
      pp_arg ppf a1
  | Ebinop (And, a1, a2) ->
      Format.fprintf ppf "%a & %a" pp_arg a1 pp_arg a2
  | Ebinop (Or, a1, a2) ->
      Format.fprintf ppf "%a | %a" pp_arg a1 pp_arg a2
  | Ebinop (Xor, a1, a2) ->
      Format.fprintf ppf "%a ^ %a" pp_arg a1 pp_arg a2
  | Ebinop (Nand, a1, a2) ->
      Format.fprintf ppf "~(%a & %a)" pp_arg a1 pp_arg a2
  | Enot a1 ->
      Format.fprintf ppf "~%a" pp_arg a1
  | Eselect (i, a1) ->
      Format.fprintf ppf "(%a >> %d) & 0x1" pp_arg a1 i
  | Ereg id ->
      Format.fprintf ppf "%a.old_value" pp_var id
  | Eram (_, _, readAddr, _, _, _) ->
      Format.fprintf ppf "ram_get(%a, %a)" pp_ram ident pp_arg readAddr
  | Erom (_, _, readAddr) ->
      Format.fprintf ppf "rom_get(%a, %a)" pp_rom ident pp_arg readAddr
  | Emux (a1, a2, a3) ->
      Format.fprintf ppf "(%a) ? (%a) : (%a)" pp_arg a1 pp_arg a3 pp_arg a2
  | Eslice (i1, i2, a1) ->
      Format.fprintf ppf "(%a >> %d) & %#x" pp_arg a1 i1 ((1 lsl i2) - 1)
  | Econcat (a1, a2) ->
      let s1 =
        match a1 with
        | Avar ident ->
            gateSize p ident
        | Aconst (VBit _) ->
            1
        | Aconst (VBitArray ar) ->
            Array.length ar
      in
      Format.fprintf ppf "(%a << %d) | (%a & %#x)" pp_arg a2 s1 pp_arg a1
        ((1 lsl s1) - 1)

let pp_fct_def p ppf (ident, exp) =
  Format.fprintf ppf "static word_t %a() { return %a; }\n" pp_fct ident
    (pp_inst p ident) exp

let pp_gate_def regs ppf (ident, _) =
  if SSet.mem ident regs then Format.fprintf ppf "reg_t %a;\n" pp_var ident
  else Format.fprintf ppf "static gate_t %a;\n" pp_var ident

let pp_mem_def ppf (ident, exp) =
  match exp with
  | Eram _ ->
      Format.fprintf ppf "static ram_t* %a = NULL;\n" pp_ram ident
  | Erom _ ->
      Format.fprintf ppf "static rom_t %a = { NULL };\n" pp_rom ident
  | _ ->
      ()

let pp_read_rom ppf (ident, exp) =
  match exp with
  | Eram _ ->
      Format.fprintf ppf
        "if (cur_ram_file_idx < ram_file_count)@.{ %a = \
         ram_from_file(ram_files[cur_ram_file_idx++]); }@.else@.{ %a = \
         ram_create(); }@."
        pp_ram ident pp_ram ident
  | Erom _ ->
      Format.fprintf ppf
        "if (cur_rom_file_idx < rom_file_count)@.{ %a = \
         rom_from_file(rom_files[cur_rom_file_idx++]); }@.else@.{ \
         fprintf(stderr, \"error: missing a rom file\\n\"); abort(); }@."
        pp_rom ident
  | _ ->
      ()

let pp_write_ram ppf (ident, exp) =
  match exp with
  | Eram (_, _, _, we, wa, data) ->
      Format.fprintf ppf "ram_write(%a, %a, %a, %a);@." pp_ram ident pp_arg we
        pp_arg wa pp_arg data
  | _ ->
      ()

let pp_input ppf ident =
  Format.fprintf ppf "/* TODO: reading %a */@." pp_var ident

let pp_output ppf ident =
  Format.fprintf ppf "/* TODO: outputting %a */@." pp_var ident

let pp_fct_set ppf (ident, _) =
  Format.fprintf ppf "%a.fct = &%a;@." pp_var ident pp_fct ident

let pp_calc ppf ident = Format.fprintf ppf "reg_calc(&%a);@." pp_var ident

let pp_init regs ppf (ident, _) =
  if SSet.mem ident regs then Format.fprintf ppf "reg_init(&%a);@." pp_var ident
  else Format.fprintf ppf "gate_init(&%a);@." pp_var ident

let pp_list pp ppf l = List.iter (fun x -> pp ppf x) l

let template =
  let ch = open_in_bin "lib/template.c" in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ; s

let search_replace text pattern repl =
  let result = ref text in
  let patternLen = String.length pattern in
  for beg = 0 to String.length text - patternLen do
    if String.equal pattern (String.sub text beg patternLen) then
      result :=
        String.sub text 0 beg ^ repl
        ^ String.sub text (beg + patternLen)
            (String.length text - patternLen - beg)
  done ;
  !result

let generate_definitions p regs =
  Format.asprintf
    "/* Memory blocks (RAM and ROM): */@.%a/* Gates: */@.%a/* Functions: */@.%a"
    (pp_list pp_mem_def) p.p_eqs
    (pp_list (pp_gate_def regs))
    (Env.bindings p.p_vars)
    (pp_list (pp_fct_def p))
    p.p_eqs

let generate_init p =
  Format.asprintf "/* Set functions. */@.%a/* Initialize RAM and ROM: */@.%a"
    (pp_list pp_fct_set) p.p_eqs (pp_list pp_read_rom) p.p_eqs

(** Generates the C code that simulates a single cycle of the Netlist [p]. *)
let generate_cycle_body p regs =
  Format.asprintf
    "%a/* Inputs: */@.%a/* Simulating the Netlist: */@.%a/* Flushing writes to \
     RAM: */@.%a/* Outputs: */@.%a"
    (pp_list (pp_init regs))
    p.p_eqs (pp_list pp_input) p.p_inputs (pp_list pp_calc) (SSet.elements regs)
    (pp_list pp_write_ram) p.p_eqs (pp_list pp_output) p.p_outputs

let to_c p _ =
  let regs =
    List.fold_left
      (fun regs (_, exp) ->
        match exp with Ereg a -> SSet.add a regs | _ -> regs )
      SSet.empty p.p_eqs
  in
  let output = search_replace template "$DEFS$" (generate_definitions p regs) in
  let output = search_replace output "$INIT$" (generate_init p) in
  let output = search_replace output "$CYCLE$" (generate_cycle_body p regs) in
  output

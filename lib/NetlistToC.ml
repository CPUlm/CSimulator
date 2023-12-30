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
      Format.fprintf ppf "0b%du" (Bool.to_int b)
  | Aconst (VBitArray a) ->
      Format.fprintf ppf "0b" ;
      Array.iter (fun b -> Format.fprintf ppf "%d" (Bool.to_int b)) a ;
      Format.fprintf ppf "u"

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

let pp_mem_def ppf (ident, exp) =
  match exp with
  | Eram _ ->
      Format.fprintf ppf "ram_t* %a = ram_create();\n" pp_ram ident
  | Erom _ ->
      Format.fprintf ppf "const rom_t* %a = NULL;\n" pp_rom ident
  | _ ->
      ()

let pp_read_rom ppf (ident, exp) =
  match exp with
  | Erom (_, _, _) ->
      Format.fprintf ppf "%a = rom_create(NULL, 0);@." pp_rom ident
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

let pp_calc ppf ident = Format.fprintf ppf "reg_calc(&%a);@." pp_var ident

let pp_init regs ppf (ident, _) =
  if SSet.mem ident regs then Format.fprintf ppf "reg_init(&%a);@." pp_var ident
  else Format.fprintf ppf "gate_init(&%a);@." pp_var ident

let pp_list pp ppf l = List.iter (fun x -> pp ppf x) l

let template =
  let ch = open_in_bin "lib/template.cpp" in
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

(** Generates the C code that simulates a single cycle of the Netlist [p]. *)
let generate_cycle_body p regs =
  Format.asprintf
    "%a/* Inputs: */@.\
     %a/* Simulating the Netlist: */@.\
     %a/* Flushing writes to RAM: */@.\
     %a/* Outputs: */@.\
     %a"
    (pp_list (pp_init regs))
    p.p_eqs (pp_list pp_input) p.p_inputs (pp_list pp_calc) (SSet.elements regs)
    (pp_list pp_write_ram) p.p_eqs (pp_list pp_output) p.p_outputs

let to_c p nbSteps =
  let regs =
    List.fold_left
      (fun regs (_, exp) ->
        match exp with Ereg a -> SSet.add a regs | _ -> regs )
      SSet.empty p.p_eqs
  in
  let v1 = search_replace template "$NB_STEPS$" (string_of_int nbSteps) in
  let memDef =
    Format.asprintf "/* Registered memory blocks (RAM and ROM): */\n.%a"
      (pp_list pp_mem_def) p.p_eqs
  in
  let v1Bis = search_replace v1 "$MEM_DEF$" memDef in
  let gateDef = ref "" in
  Env.iter
    (fun ident _ ->
      let code =
        if SSet.mem ident regs then Format.asprintf "reg_t %a;\n" pp_var ident
        else Format.asprintf "gate_t %a;\n" pp_var ident
      in
      gateDef := !gateDef ^ code )
    p.p_vars ;
  let v2 = search_replace v1Bis "$GATE_DEF$" !gateDef in
  let fctDef =
    Format.asprintf "/* Instructions: */\n%a" (pp_list (pp_fct_def p)) p.p_eqs
  in
  let v3 = search_replace v2 "$FCT_DEF$" fctDef in
  let fctSet = ref "" in
  List.iter
    (fun (ident, _) ->
      fctSet := Format.asprintf "%a.fct = &%a;\n" pp_var ident pp_fct ident )
    p.p_eqs ;
  let v4 = search_replace v3 "$FCT_SET$" !fctSet in
  let readRom =
    Format.asprintf "/* Initializing ROM: */\n%a" (pp_list pp_read_rom) p.p_eqs
  in
  let v4Bis = search_replace v4 "$READ_ROM$" readRom in
  let v8 = search_replace v4Bis "$CYCLE$" (generate_cycle_body p regs) in
  v8

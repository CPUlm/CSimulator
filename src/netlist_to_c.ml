open Netlist_ast
let varPrefix = "var_" 
let fctPrefix = "fct_"
let memPrefix = "mem_"
let template =
	let ch = open_in_bin "src/template.cpp" in
	let s = really_input_string ch (in_channel_length ch) in
	close_in ch; s


let search_replace text pattern repl =
	let result = ref text in
	let patternLen = String.length pattern in
	for beg = 0 to (String.length text - patternLen) do
		if String.equal pattern (String.sub text beg patternLen) then
			result := (String.sub text 0 beg)^repl^(String.sub text (beg+patternLen) (String.length text - patternLen - beg))
	done;
	!result



let to_c p nbSteps =
	let regLst = ref [] in
	List.iter (fun (ident,exp) -> begin match exp with
		| Ereg a -> regLst := a::!regLst
		| _ -> ()
	end ) p.p_eqs;
	regLst := List.sort_uniq compare !regLst;
	let isReg ident = List.mem ident !regLst in


	let v1 = search_replace template "$NB_STEPS$" (string_of_int nbSteps) in

	let memDef = ref "" in
	List.iter (fun (ident,exp) -> begin match exp with
		| Eram _ | Erom _ -> memDef := !memDef ^ "Memory " ^ memPrefix ^ ident ^ ";\n"
		| _ -> ()
	end) p.p_eqs;
	let v1Bis = search_replace v1 "$MEM_DEF$" !memDef in

	let gateDef = ref "" in
	Env.iter (fun ident _ ->
		if isReg ident then gateDef := !gateDef ^ "GateReg " ^ varPrefix ^ ident ^ " (true);\n"
							else gateDef := !gateDef ^ "Gate " ^ varPrefix ^ ident ^ " (true);\n"
	) p.p_vars;
	let v2 = search_replace v1Bis "$GATE_DEF$" !gateDef in


	let from_arg = function
			| Avar ident -> varPrefix ^ ident ^ ".calcGate()"
			| Aconst (VBit b) -> if b then "1" else "0"
			| Aconst (VBitArray a) -> "0b"^(Array.fold_left (fun s b -> if b then "1"^s else "0"^s) "" a)^"u"
	in
	let gateSize ident =
		let t = Env.find ident p.p_vars in
		match t with
			| TBit -> 1
			| TBitArray n -> n
	in

	let fctDef = ref "" in
	List.iter (fun (ident,exp) ->
			let mask1 size = "0b"^(String.make size '1')^"ull" in
			let exp_to_string ident = function
			| Earg a1 -> "return " ^ (from_arg a1) ^ ";"
			| Ebinop (And, a1, a2) -> "return (" ^ (from_arg a1) ^ " & " ^ (from_arg a2) ^ ");"
			| Ebinop (Or, a1, a2) -> "return (" ^ (from_arg a1) ^ " | " ^ (from_arg a2) ^ ");"
			| Ebinop (Xor, a1, a2) -> "return (" ^ (from_arg a1) ^ " ^ " ^ (from_arg a2) ^ ");"
			| Ebinop (Nand, a1, a2) -> "return ~(" ^ (from_arg a1) ^ " & " ^ (from_arg a2) ^ ");"
			| Enot a1 -> "return ~"^(from_arg a1)^";"
			| Eselect (i, a1) -> "return (" ^ (from_arg a1) ^ " >> " ^ (string_of_int i) ^ ");"
			| Ereg (id) -> "return " ^ varPrefix ^ id ^ ".getOldValue();"
			| Eram (_,_,readAddr,_,_,_) | Erom (_,_,readAddr) -> "return " ^ memPrefix^ident ^ ".get(" ^ (from_arg readAddr) ^ ");"
			| Emux (a1, a2, a3) -> "return ( ("^(from_arg a1) ^ ")? " ^ (from_arg a3) ^ " : " ^ (from_arg a2) ^ ");"
			| Eslice (i1, i2, a1) -> "return (" ^ (from_arg a1) ^ " >> " ^ (string_of_int i1) ^ ");"
			| Econcat (a1, a2) ->
				let s1 = (match a1 with
					| Avar ident -> gateSize ident
					| Aconst (VBit _) -> 1
					| Aconst (VBitArray ar)	-> Array.length ar
				) in
				"return ((" ^(from_arg a2) ^ " << " ^ (string_of_int s1) ^ ") | (" ^ (from_arg a1) ^ " & " ^ (mask1 s1) ^ "));"
		in
		fctDef := !fctDef ^ "uint32_t " ^ fctPrefix ^ ident ^ " (){" ^ (exp_to_string ident exp) ^ "}\n"
	) p.p_eqs;
	let v3 = search_replace v2 "$FCT_DEF$" !fctDef in

	let fctSet = ref "" in
	List.iter (fun (ident,exp) ->
		fctSet := !fctSet ^ "	" ^ varPrefix^ident ^ ".setFct(&" ^ fctPrefix ^ ident ^ ");\n"
	) p.p_eqs;
	let v4 = search_replace v3 "$FCT_SET$" !fctSet in

	let readRom = ref "" in
	List.iter (fun (ident,exp) -> begin match exp with
		| Erom (addrSize, wordSize,_) -> readRom := !readRom ^ "	" ^ memPrefix ^ ident ^ ".initRom(" ^ (string_of_int addrSize) ^","^ (string_of_int wordSize) ^ ");\n"
		| _ -> ()
	end) p.p_eqs;
	let v4Bis = search_replace v4 "$READ_ROM$" !readRom in

	let init = ref "" in
	List.iter (fun (ident,exp) ->
		init := !init ^ "		" ^ varPrefix^ident ^ ".init();\n"
	) p.p_eqs;
	let v5 = search_replace v4Bis "$INIT$" !init in

	let input = ref "" in
	List.iter (fun ident ->
		input := !input ^ "		" ^ varPrefix^ident ^ ".readInput("^ (string_of_int (gateSize ident)) ^ ");\n"
	) p.p_inputs;
	let v5Bis = search_replace v5 "$INPUT$" !input in



	let calc = ref "" in
	List.iter (fun (ident,ext) ->
		calc := !calc ^ "		if(" ^ varPrefix^ident ^ ".mustBeCalculated) " ^ varPrefix ^ ident ^ ".calcGate();\n"
	) p.p_eqs;
	let v6 = search_replace v5Bis "$CALC$" !calc in

	let writeRam = ref "" in
	List.iter (fun (ident,exp) -> begin match exp with
		| Eram (_,_,_,we,wa,data)  -> writeRam := !writeRam ^ "if ("^(from_arg we) ^") " ^ memPrefix ^ ident ^ ".set(" ^ (from_arg wa) ^ ", " ^ (from_arg data) ^ ");\n"
		| _ -> ()
	end) p.p_eqs;
	let v7 = search_replace v6 "$WRITE_RAM$" !writeRam in

	

	let output = ref "" in
	List.iter (fun ident ->
		output := !output ^ "		printf(\"=> " ^ ident ^ " = \"); " ^ varPrefix ^ ident ^ ".show(" ^ (string_of_int (gateSize ident)) ^ "); printf(\"\\n\");\n"
	) p.p_outputs;
	let v8 = search_replace v7 "$OUTPUT$" !output in
	v8
